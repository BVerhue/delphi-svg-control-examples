unit UnitViewerFMX;

//------------------------------------------------------------------------------
//
//                             SVG Control 2.0
//                     Copyright (c) 2015 Bruno Verhue
//
//------------------------------------------------------------------------------

// [The "BSD licence"]
//
//  Copyright (c) 2013 Bruno Verhue
//  All rights reserved.
//
//  Redistribution and use in source and binary forms, with or without
//  modification, are permitted provided that the following conditions
//  are met:
//  1. Redistributions of source code must retain the above copyright
//     notice, this list of conditions and the following disclaimer.
//  2. Redistributions in binary form must reproduce the above copyright
//     notice, this list of conditions and the following disclaimer in the
//     documentation and/or other materials provided with the distribution.
//  3. The name of the author may not be used to endorse or promote products
//     derived from this software without specific prior written permission.
//
//  THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
//  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
//  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
//  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
//  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
//  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
//  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
//  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
//  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
//  THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

// -----------------------------------------------------------------------------
//
//  Demo application "SVG viewer"
//
//  Demonstrates:
//
//  - Loading and parsing SVG documents
//  - Copying SVG control
//  - Finding rendering elements and changing properties

// When loading the "animatied clock", it will have some interactive
// functionality: it will be set and updated to the current time, and you
// can move the clock hands by mouse.
//
// -----------------------------------------------------------------------------

{$LEGACYIFEND ON}

{$Include '..\..\..\Common\CompilerSettings.inc'}

interface
uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
{$IFDEF Ver240Up}
  System.UIConsts,
{$ENDIF}
  System.Generics.Collections,
{$IFDEF VER270Up}
  System.Math.Vectors,
{$ENDIF}
  System.Actions,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.Objects,
  FMX.Layouts,
  FMX.Memo,
  FMX.Edit,
  FMX.Menus,
  FMX.StdCtrls,
{$IFDEF VER260Up}
  FMX.Graphics,
{$ENDIF}
  FMX.ActnList,
  BVE.SVG2Types,
  BVE.SVG2Intf,
  BVE.SVG2Elements,
  BVE.SVG2Elements.FMX,
  BVE.SVG2GeomUtility,
  BVE.SVG2Doc,
  BVE.Selection.FMX,
  BVE.ViewerCore.FMX;

type
  TfrmViewer = class(TForm)
    OpenDialog1: TOpenDialog;
    tbZoom: TTrackBar;
    sb: TScrollBox;
    lSize: TLayout;
    lZoom: TLayout;
    ActionList1: TActionList;
    aLoad: TAction;
    aProperties: TAction;
    aCopyDirect: TAction;
    aExit: TAction;
    Panel1: TPanel;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Label2: TLabel;
    Button6: TButton;
    aZoomReset: TAction;
    Action1: TAction;
    aCopySVG: TAction;
    aPasteSVG: TAction;
    Button1: TButton;
    Button7: TButton;
    procedure FormCreate(Sender: TObject);
    procedure tbZoomChange(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure FormDestroy(Sender: TObject);
    procedure aLoadExecute(Sender: TObject);
    procedure aCopyDirectExecute(Sender: TObject);
    procedure aZoomResetExecute(Sender: TObject);
    procedure aPropertiesExecute(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure aExitExecute(Sender: TObject);
    procedure sbDragOver(Sender: TObject; const Data: TDragObject;
      const Point: TPointF; var Accept: Boolean);
    procedure sbDragDrop(Sender: TObject; const Data: TDragObject;
      const Point: TPointF);
    procedure aCopySVGExecute(Sender: TObject);
    procedure aPasteSVGExecute(Sender: TObject);
  private
    FZoom: single;
    FSelectionList: TSelectionList;
    procedure SetCurrSelectControl(const Value: TSVGSelectControl);
    function GetCurrSelectControl: TSVGSelectControl;
    function LoadFile(const aFilename: string): boolean;
  public
    procedure CalcLayoutDimensions;
    procedure SVGSelectMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    property CurrSelectControl: TSVGSelectControl read GetCurrSelectControl write SetCurrSelectControl;
  end;

var
  frmViewer: TfrmViewer;

implementation

{$R *.fmx}

uses
  System.Rtti,
  FMX.Platform,
  FMX.Surfaces,
  UnitPropertiesFMX;

procedure TfrmViewer.FormCreate(Sender: TObject);
begin
  FormatSettings.DecimalSeparator := '.';
  FZoom := tbZoom.Value;

  tbZoomChange(self);

  FSelectionList := TSelectionList.Create(False);
end;

procedure TfrmViewer.FormDestroy(Sender: TObject);
begin
  FSelectionList.Free;
end;

function ChildrenRect(aControl: TFmxObject): TRectF;
var
  i: integer;
  c: TControl;
begin
  // Calc max size of a parent control
  Result.Left := 100000;
  Result.Right := 0;
  Result.Top := 100000;
  Result.Bottom := 0;
  for i := 0 to aControl.ChildrenCount - 1 do
  begin
    if aControl.Children[i] is TControl then
    begin
      c := (aControl.Children[i] as TControl);

      if c.Position.X < Result.Left then
        Result.Left := c.Position.X;

      if c.Position.Y < Result.Top then
        Result.Top := c.Position.Y;

      if c.Position.X + c.Width > Result.Right then
        Result.Right := c.Position.X + c.Width;

      if c.Position.Y + c.Height > Result.Bottom then
        Result.Bottom := c.Position.Y + c.Height;
    end;
  end;
end;

procedure TfrmViewer.CalcLayoutDimensions;
var
  CR, WR, R: TRectF;
begin
  // Calc max size of lZoom layout

  CR := ChildrenRect( lZoom);

  if tbZoom.Value > 0 then
    WR := RectF(
      0, 0,
      ClientRect.Width / tbZoom.Value, ClientRect.Height / tbZoom.Value)
  else
    WR := ClientRect;

  R := MaxRect(CR, WR);

  lZoom.Width := R.Right;
  lZoom.Height := R.Bottom;
  lSize.Width := lZoom.Width * tbZoom.Value;
  lSize.Height := lZoom.Height * tbZoom.Value;
end;

procedure TfrmViewer.tbZoomChange(Sender: TObject);
var
  mx, my: single;
begin
  // Calc middle of currently visible rectangle of scrollbox

  mx := (sb.ViewportPosition.X + sb.Width/2) / FZoom;
  my := (sb.ViewportPosition.Y + sb.Height/2) / FZoom;

  FZoom := tbZoom.Value;

  // Set zoom of lZoom layout
  lZoom.Scale.X := FZoom;
  lZoom.Scale.Y := FZoom;

  // Calc new size of lSize layout
  lSize.Width := lZoom.Width * FZoom;
  lSize.Height := lZoom.Height * FZoom;

  // Adjust scrollbox to keep centered on original mid point
  sb.ViewportPosition := PointF( mx * FZoom - (sb.Width/2), my * FZoom - (sb.Height/2));
end;

procedure TfrmViewer.aCopyDirectExecute(Sender: TObject);
var SelectControl : TSVGSelectControl;
begin
  // Copies the rendering tree directly

  if not assigned(CurrSelectControl) then
    exit;

  SelectControl := CurrSelectControl.CreateCopy(Self, FSelectionList);
  SelectControl.Parent := lZoom;
  SelectControl.OnMouseUp := SVGSelectMouseUp;
  CurrSelectControl := SelectControl;
end;

procedure TfrmViewer.aCopySVGExecute(Sender: TObject);
var
  Svc: IFMXClipboardService;
  sl: TStringList;
begin
  // Makes a copy of the SVG text and puts it on the clipboard

  if not assigned(CurrSelectControl) then
    exit;

  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, IInterface(Svc)) then
  begin
    sl := TStringList.Create;
    try
      if CurrSelectControl.SVGControl.SVG.Count <> 0 then
        sl.Assign(CurrSelectControl.SVGControl.SVG)
      else
       sl.LoadFromFile(CurrSelectControl.SVGControl.Filename);
      Svc.SetClipboard(sl.Text)
    finally
      sl.Free;
    end;
  end;
end;

procedure TfrmViewer.aExitExecute(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmViewer.aLoadExecute(Sender: TObject);
begin
  if OpenDialog1.Execute then begin
    LoadFile(openDialog1.Filename);
  end;
end;

procedure TfrmViewer.aPasteSVGExecute(Sender: TObject);
var
  Svc: IFMXClipboardService;
  Value: TValue;
  sl: TStringList;
  SelectControl: TSVGSelectControl;
begin
  // Paste the contents of the clipboard as an svg image

  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, IInterface(Svc)) then
  begin

    Value := Svc.GetClipboard;
    if not Value.IsEmpty then
    begin
      if Value.IsType<string> then
      begin
        sl := TStringList.Create;
        try
          sl.Text := Value.ToString;

          SelectControl := TSVGSelectControl.Create(Self, FSelectionList);
          SelectControl.Parent := lZoom;
          SelectControl.OnMouseUp := SVGSelectMouseUp;
          SelectControl.SVGControl.SVG.Assign(sl);
          CurrSelectControl := SelectControl;
          SelectControl.ParseSVG;
        finally
          sl.Free;
        end;
      end
    end;
  end;
end;

procedure TfrmViewer.aPropertiesExecute(Sender: TObject);
begin
  frmProperties.Show;
end;

procedure TfrmViewer.aZoomResetExecute(Sender: TObject);
begin
  if tbZoom.Value <> 1 then
    tbZoom.Value := 1
  else
    tbZoom.Value := 2;
end;

procedure TfrmViewer.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
var SelectControl : TSVGSelectControl;
begin
  if Key = vkDelete then begin
    if assigned(CurrSelectControl) then begin

      SelectControl := CurrSelectControl;

      if frmProperties.SVGSelectControl = CurrSelectControl then
        frmProperties.SVGSelectControl := nil;

      SelectControl.Parent := nil;
      SelectControl.Free;

      FSelectionList.UnselectAll;
    end;
  end;
end;

procedure TfrmViewer.FormResize(Sender: TObject);
begin
  CalcLayoutDimensions;
end;

function TfrmViewer.GetCurrSelectControl: TSVGSelectControl;
begin
  Result := frmProperties.SVGSelectControl;
end;

function TfrmViewer.LoadFile(const aFilename: string): boolean;
var
  SelectControl: TSVGSelectControl;
  Ext: string;
begin
  Result := False;
  Ext := Lowercase(ExtractFileExt(aFileName));
  if Ext = '.svg' then
  begin
    SelectControl := TSVGSelectControl.Create(Self, FSelectionList);
    SelectControl.Parent := lZoom;
    SelectControl.OnMouseUp := SVGSelectMouseUp;
    SelectControl.LoadSVG(aFileName);
    CurrSelectControl := SelectControl;
    Result := True;
    SelectControl.ParseSVG;
  end;
end;

procedure TfrmViewer.sbDragOver(Sender: TObject; const Data: TDragObject;
  const Point: TPointF; var Accept: Boolean);
begin
  Accept := Length(Data.Files) > 0;
end;

procedure TfrmViewer.sbDragDrop(Sender: TObject; const Data: TDragObject;
  const Point: TPointF);
var
  i: integer;
begin
  for i := 0 to Length(Data.Files) - 1 do
  begin
    if LoadFile(Data.Files[i]) then
      CurrSelectControl.Position.Point := Point;
  end;
end;

procedure TfrmViewer.SetCurrSelectControl(const Value: TSVGSelectControl);
begin
  frmProperties.SVGSelectControl := Value;
  aProperties.Enabled := assigned(CurrSelectControl);
  aCopyDirect.Enabled := assigned(CurrSelectControl);
  aCopySVG.Enabled := assigned(CurrSelectControl);
end;

procedure TfrmViewer.SVGSelectMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  CurrSelectControl := Sender as TSVGSelectControl;
  CalcLayoutDimensions;
end;

end.
