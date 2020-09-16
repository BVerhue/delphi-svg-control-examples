unit UnitViewerFMX;

// ------------------------------------------------------------------------------
//
// SVG Control 2.0
// Copyright (c) 2015 Bruno Verhue
//
// ------------------------------------------------------------------------------

// [The "BSD licence"]
//
// Copyright (c) 2013 Bruno Verhue
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
// 1. Redistributions of source code must retain the above copyright
// notice, this list of conditions and the following disclaimer.
// 2. Redistributions in binary form must reproduce the above copyright
// notice, this list of conditions and the following disclaimer in the
// documentation and/or other materials provided with the distribution.
// 3. The name of the author may not be used to endorse or promote products
// derived from this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
// OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
// IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
// INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
// NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
// THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

// -----------------------------------------------------------------------------
//
// Demo application "SVG viewer"
//
// Demonstrates:
//
// - Loading and parsing SVG documents
// - Copying SVG control
// - Finding rendering elements and changing properties

// When loading the "animatied clock", it will have some interactive
// functionality: it will be set and updated to the current time, and you
// can move the clock hands by mouse.
//
// -----------------------------------------------------------------------------

{$Include '..\..\Demo common\DemoAppCompilerSettings.inc'}

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
{$IFDEF Ver240Up}
  System.Actions,
  FMX.ActnList,
{$ENDIF}
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.Objects,
  FMX.Layouts,
  FMX.Memo,
  FMX.Edit,
  FMX.Menus,
{$IFDEF Ver240Up}
  FMX.StdCtrls,
{$ENDIF}
{$IFDEF VER260Up}
  FMX.Graphics,
{$ENDIF}
  Xml.xmldom,
  Xml.XMLIntf,
  Xml.XMLDoc,
  Xml.adomxmldom,
{$IFDEF MSWINDOWS}
  Xml.Win.msxmldom,
{$ENDIF}
  BVE.SVG2Types,
  BVE.SVG2Intf,
  BVE.SVG2Elements,
  BVE.SVG2Elements.FMX,
  BVE.SVG2GeomUtility,
  BVE.SVG2Doc,
  BVE.SVG2SaxParser,
  BVE.Selection.FMX,
  BVE.ViewerCore.FMX;

type
  TfrmViewer = class(TForm)
    OpenDialog1: TOpenDialog;
    tbZoom: TTrackBar;
    sb: TScrollBox;
    lSize: TLayout;
    lZoom: TLayout;
    Panel1: TPanel;
    bLoad: TButton;
    bProperties: TButton;
    bCopy: TButton;
    bExit: TButton;
    Label2: TLabel;
    bReset: TButton;
    procedure FormCreate(Sender: TObject);
    procedure tbZoomChange(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure bResetClick(Sender: TObject);
    procedure bPropertiesClick(Sender: TObject);
    procedure bCopyClick(Sender: TObject);
    procedure bExitClick(Sender: TObject);
    procedure bLoadClick(Sender: TObject);
  private
    FZoom: single;
    FSelectionList: TSelectionList;
    procedure SetCurrSelectControl(const Value: TSVGSelectControl);
    function GetCurrSelectControl: TSVGSelectControl;
  public
    procedure CalcLayoutDimensions;
    procedure SVGSelectMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: single);
    property CurrSelectControl: TSVGSelectControl read GetCurrSelectControl
      write SetCurrSelectControl;
  end;

var
  frmViewer: TfrmViewer;

implementation

{$R *.fmx}

uses
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
  CR := ChildrenRect(lZoom);

  if tbZoom.Value > 0 then
    WR := RectF(0, 0, ClientRect.Width / tbZoom.Value,
      ClientRect.Height / tbZoom.Value)
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

{$IFDEF Ver230Down}
  if sb.HScrollBar.Visible then
    mx := (sb.Width / 2 + sb.HScrollBar.Value) / FZoom
  else
    mx := (sb.Width / 2) / FZoom;

  if sb.VScrollBar.Visible then
    my := (sb.Height / 2 + sb.VScrollBar.Value) / FZoom
  else
    my := (sb.Height / 2) / FZoom;
{$ELSE}
  mx := (sb.ViewportPosition.X + sb.Width / 2) / FZoom;
  my := (sb.ViewportPosition.Y + sb.Height / 2) / FZoom;
{$ENDIF}
  FZoom := tbZoom.Value;

  // Set zoom of lZoom layout
  lZoom.Scale.X := FZoom;
  lZoom.Scale.Y := FZoom;

  // Calc new size of lSize layout
  lSize.Width := lZoom.Width * FZoom;
  lSize.Height := lZoom.Height * FZoom;

  // Adjust scrollbox to keep centered on original mid point
{$IFDEF Ver230Down}
  sb.HScrollBar.Value := mx * FZoom - (sb.Width / 2);
  sb.VScrollBar.Value := my * FZoom - (sb.Height / 2);
{$ELSE}
  sb.ViewportPosition := PointF(mx * FZoom - (sb.Width / 2),
    my * FZoom - (sb.Height / 2));
{$ENDIF}
end;

procedure TfrmViewer.bCopyClick(Sender: TObject);
var
  SelectControl: TSVGSelectControl;
begin
  if not assigned(CurrSelectControl) then
    exit;

  SelectControl := CurrSelectControl.CreateCopy(self, FSelectionList);
  SelectControl.Parent := lZoom;
  SelectControl.OnMouseUp := SVGSelectMouseUp;
  CurrSelectControl := SelectControl;
end;

procedure TfrmViewer.bExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmViewer.bLoadClick(Sender: TObject);
var
  SelectControl: TSVGSelectControl;
begin
  if OpenDialog1.Execute then
  begin

    SelectControl := TSVGSelectControl.Create(self, FSelectionList);
    SelectControl.Parent := lZoom;
    SelectControl.OnMouseUp := SVGSelectMouseUp;
    SelectControl.LoadSVG(OpenDialog1.Filename);
    CurrSelectControl := SelectControl;
    SelectControl.ParseSVG;
  end;
end;

procedure TfrmViewer.bPropertiesClick(Sender: TObject);
begin
  frmProperties.Show;
end;

procedure TfrmViewer.bResetClick(Sender: TObject);
begin
  if tbZoom.Value <> 1 then
    tbZoom.Value := 1
  else
    tbZoom.Value := 2;
end;

procedure TfrmViewer.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
var
  SelectControl: TSVGSelectControl;
begin
  if Key = vkDelete then
  begin
    if assigned(CurrSelectControl) then
    begin

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

procedure TfrmViewer.SetCurrSelectControl(const Value: TSVGSelectControl);
begin
  frmProperties.SVGSelectControl := Value;
  bProperties.Enabled := assigned(CurrSelectControl);
  bCopy.Enabled := assigned(CurrSelectControl);
end;

procedure TfrmViewer.SVGSelectMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: single);
begin
  CurrSelectControl := Sender as TSVGSelectControl;
  CalcLayoutDimensions;
end;

// Prevent the 'DTD is prohibited' error.
{$IFDEF MSWINDOWS}

initialization

{$IFDEF VER230}
  MSXML6_ProhibitDTD := False;
{$ELSE}
  MSXMLDOMDocumentFactory.AddDOMProperty('ProhibitDTD', False);
{$ENDIF}
{$ENDIF}

end.
