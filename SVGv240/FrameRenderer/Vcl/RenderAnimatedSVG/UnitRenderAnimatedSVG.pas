unit UnitRenderAnimatedSVG;

// ------------------------------------------------------------------------------
//
//                          SVG Control Package 2.0
//                       Copyright (c) 2015 Bruno Verhue
//
// ------------------------------------------------------------------------------

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

///  <summary>
///    User interface for converting animated SVG
///  </summary>

// For converting to animated png, we need the imaginglib by Marek Mauder
// https://github.com/galfar/imaginglib

{$DEFINE SVG_TO_APNG}

// For converting to avi, we need CreateAviFromBitmap by Francois Piette
// http://francois-piette.blogspot.com/2013/07/creating-avi-file-from-bitmaps-using.html

{$DEFINE SVG_TO_AVI}

// Gif library by Anders Melander is build into Delphi

{$DEFINE SVG_TO_GIF}

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.MMSystem,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Actions,
  System.Generics.Collections,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ActnList,
  Vcl.PlatformDefaultStyleActnCtrls,
  Vcl.ActnMan,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ToolWin,
  Vcl.ActnCtrls,
  Vcl.ComCtrls,
  Vcl.Grids,
  Vcl.ValEdit,
  {$IFDEF SVG_TO_GIF}
  Vcl.Imaging.GIFImg,
  {$ENDIF}
  {$IFDEF SVG_TO_AVI}
  AviFromBitmaps,
  Vfw,
  {$ENDIF}
  {$IFDEF SVG_TO_APNG}
  Imaging,
  ImagingComponents,
  ImagingNetworkGraphics,
  {$ENDIF}
  BVE.SVG2Types,
  BVE.SVG2Intf,
  BVE.SVG2Graphic.VCL,
  BVE.SVGFrameRendererVCL;

type
  TSVGConvTarget = class(TCustomPanel)
  public const
    k_filename = 'Filename';
  private
    FFilename: string;
    FValueListEditor: TValueListEditor;
  protected
    function GetFileExt: string; virtual; abstract;
    function GetFilename: string;
    procedure SetFilename(const Value: string);

    procedure DoPropsInit; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Convert(aFrameRenderer: TSVGFrameRenderer); virtual; abstract;

    procedure PropEditButtonClick(aVle: TValueListEditor); virtual; abstract;
    procedure PropValidate(aVle: TValueListEditor; ACol, ARow: Integer;
      const KeyName, KeyValue: string); virtual;

    property Filename: string read GetFilename write SetFilename;
    property FileExt: string read GetFileExt;
    property ValueListEditor: TValueListEditor read FValueListEditor
      write FValueListEditor;
  end;

  {$IFDEF SVG_TO_APNG}
  TSVGConvTargetApng = class(TSVGConvTarget)
  public const
    k_file_extension = '.png';
    k_animation_loops = 'AnimationLoops';
  private
    FAnimatedLoops: Integer;
  protected
    function GetAnimatedLoops: Integer;
    function GetFileExt: string; override;
    procedure SetAnimatedLoops(const Value: Integer);

    procedure DoPropsInit; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure PropEditButtonClick(aVle: TValueListEditor); override;
    procedure PropValidate(aVle: TValueListEditor; ACol, ARow: Integer;
      const KeyName, KeyValue: string); override;

    procedure Convert(aFrameRenderer: TSVGFrameRenderer); override;

    property AnimatedLoops: Integer read GetAnimatedLoops
      write SetAnimatedLoops;
  end;
  {$ENDIF}

  {$IFDEF SVG_TO_AVI}
  TAviCompression = (acNone, acXVID);

  TSVGConvTargetAvi = class(TSVGConvTarget)
  public const
    k_file_extension = '.avi';
    k_compression = 'Compression';
  private
    FCompression: TAviCompression;
  protected
    function GetCompression: TAviCompression;
    function GetFileExt: string; override;
    procedure SetCompression(const Value: TAviCompression);

    procedure DoPropsInit; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure PropEditButtonClick(aVle: TValueListEditor); override;
    procedure PropValidate(aVle: TValueListEditor; ACol, ARow: Integer;
      const KeyName, KeyValue: string); override;

    procedure Convert(aFrameRenderer: TSVGFrameRenderer); override;

    property Compression: TAviCompression read GetCompression write SetCompression;
  end;
  {$ENDIF}

  {$IFDEF SVG_TO_GIF}
  TSVGConvTargetGif = class(TSVGConvTarget)
  public const
    k_file_extension = '.gif';
    k_animation_loops = 'AnimationLoops';
    k_transparent = 'Transparent';
    k_optimize_color_map = 'OptimizeColorMap';
    k_merge = 'Merge';
    k_crop = 'Crop';
  private
    FAnimatedLoops: Integer;
    FOptimizeColorMap: Boolean;
    FOptimizeOptions: TGIFOptimizeOptions;
    FTransparent: Boolean;
  protected
    function GetAnimatedLoops: Integer;
    function GetFileExt: string; override;
    function GetOptimizeColorMap: Boolean;
    function GetOptimizeOptions: TGIFOptimizeOptions;
    function GetTransparent: Boolean;
    procedure SetAnimatedLoops(const Value: Integer);
    procedure SetOptimizeColorMap(const Value: Boolean);
    procedure SetOptimizeOptions(const Value: TGIFOptimizeOptions);
    procedure SetTransparent(const Value: Boolean);

    procedure DoPropsInit; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure PropValidate(aVle: TValueListEditor; ACol, ARow: Integer;
      const KeyName, KeyValue: string); override;
    procedure PropEditButtonClick(aVle: TValueListEditor); override;

    procedure Convert(aFrameRenderer: TSVGFrameRenderer); override;

    property AnimatedLoops: Integer read GetAnimatedLoops
      write SetAnimatedLoops;
    property Transparent: Boolean read GetTransparent write SetTransparent;
    property OptimizeColorMap: Boolean read GetOptimizeColorMap
      write SetOptimizeColorMap;
    property OptimizeOptions: TGIFOptimizeOptions read GetOptimizeOptions
      write SetOptimizeOptions;
  end;
  {$ENDIF}

  TfrmRenderAnimatedSVG = class(TForm)
    ActionManager1: TActionManager;
    aRecord: TAction;
    Panel1: TPanel;
    eTime: TEdit;
    aOpenSVG: TAction;
    OpenDialog1: TOpenDialog;
    Panel2: TPanel;
    Label3: TLabel;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Button2: TButton;
    Label2: TLabel;
    eDuration: TEdit;
    eFPS: TEdit;
    Panel3: TPanel;
    Splitter1: TSplitter;
    TabControl1: TTabControl;
    vleTarget: TValueListEditor;
    vleSVG: TValueListEditor;
    ColorDialog1: TColorDialog;
    PaintBox1: TPaintBox;
    procedure aRecordExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
    procedure vleSVGEditButtonClick(Sender: TObject);
    procedure vleSVGValidate(Sender: TObject; ACol, ARow: Integer;
      const KeyName, KeyValue: string);
    procedure vleTargetEditButtonClick(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure vleTargetValidate(Sender: TObject; ACol, ARow: Integer;
      const KeyName, KeyValue: string);
  const
    k_filename = 'Filename';
    k_file_extension = '.svg';
    k_width = 'Width';
    k_height = 'Height';
    k_background_color = 'BackgroundColor';
    k_auto_viewbox = 'AutoViewbox';
    k_aspect_ratio_align = 'Align';
    k_aspect_ratio_meet_or_slice = 'MeetOrSlice';
    k_render_options_clippaths = 'Clippaths';
    k_render_options_filters = 'Filters';
  private
    FFrameRenderer: TSVGFrameRenderer;

    FFilename: string;
    FImageWidth: Integer;
    FImageHeight: Integer;
    FBackgroundColor: TColor;
    FAutoViewbox: Boolean;
    FAspectRatioAlign: TSVGAspectRatioAlign;
    FAspectRatioMeetOrSlice: TSVGAspectRatioMeetOrSlice;
    FRenderOptions: TSVGRenderOptions;

    FUpdateCounter: Integer;

    FSVG: TSVG2Graphic;
    FGraphic: TGraphic;

    FTargetList: TList<TSVGConvTarget>;

    procedure UpdatePreview;
    procedure DrawGraphic;
  protected
    function GetAspectRatioAlign: TSVGAspectRatioAlign;
    function GetAspectRatioMeetOrSlice: TSVGAspectRatioMeetOrSlice;
    function GetAutoViewbox: Boolean;
    function GetBackgroundColor: TColor;
    function GetFilename: string;
    function GetImageHeight: Integer;
    function GetImageWidth: Integer;
    function GetRenderOptions: TSVGRenderOptions;
    function GetTarget: TSVGConvTarget;

    procedure SetAspectRatioAlign(const Value: TSVGAspectRatioAlign);
    procedure SetAspectRatioMeetOrSlice(
      const Value: TSVGAspectRatioMeetOrSlice);
    procedure SetAutoViewbox(const Value: Boolean);
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetImageHeight(const Value: Integer);
    procedure SetImageWidth(const Value: Integer);
    procedure SetFilename(const Value: string);
    procedure SetRenderOptions(const Value: TSVGRenderOptions);

    procedure PropWriteBoolean(aVle: TValueListEditor; const aKey: string;
      const aValue: Boolean);
    procedure PropWritePickList(aVle: TValueListEditor; const aKey: string;
      const aItemIndex: Integer; aPickList: string);
    procedure PropWriteInteger(aVle: TValueListEditor; const aKey: string;
      const aValue: Integer);
    procedure PropWriteColor(aVle: TValueListEditor; const aKey: string;
      const aValue: TColor);
    procedure PropWriteFilename(aVle: TValueListEditor; const aKey,
      aValue: string);

    function PropReadBoolean(aVle: TValueListEditor;
      const aKey: string): Boolean;
    function PropReadPickList(aVle: TValueListEditor;
      const aKey: string): Integer;
    function PropReadInteger(aVle: TValueListEditor;
      const aKey: string): Integer;
    function PropReadColor(aVle: TValueListEditor;
      const aKey: string): TColor;
    function PropReadFilename(aVle: TValueListEditor;
      const aKey: string): string;

    procedure PropEditButtonClick(aVle: TValueListEditor);

    procedure UpdateBegin;
    procedure UpdateEnd;

    procedure FrameRendererFrame(Sender: TObject; const aAnimationTime: Integer;
      const aBitmap: TBitmap);
  public
    procedure OpenSVG(const aFilename: string);

    procedure PropsInit;
    procedure PropsInitTarget;

    procedure TargetAdd(aTarget: TSVGConvTarget; const aCaption: string);

    property AspectRatioAlign: TSVGAspectRatioAlign read GetAspectRatioAlign
      write SetAspectRatioAlign;
    property AspectRatioMeetOrSlice: TSVGAspectRatioMeetOrSlice
      read GetAspectRatioMeetOrSlice write SetAspectRatioMeetOrSlice;
    property AutoViewBox: Boolean read GetAutoViewbox write SetAutoViewbox;
    property BackgroundColor: TColor read GetBackgroundColor
      write SetBackgroundColor;
    property Filename: string read GetFilename write SetFilename;
    property ImageHeight: Integer read GetImageHeight write SetImageHeight;
    property ImageWidth: Integer read GetImageWidth write SetImageWidth;
    property RenderOptions: TSVGRenderOptions read GetRenderOptions
      write SetRenderOptions;
    property Target: TSVGConvTarget read GetTarget;
  end;

var
  frmRenderAnimatedSVG: TfrmRenderAnimatedSVG;

implementation

{$R *.dfm}

function BoolToStr(const aValue: Boolean): string;
begin
  if aValue then
    Result := 'True'
  else
    Result := 'False';
end;

procedure TfrmRenderAnimatedSVG.aRecordExecute(Sender: TObject);
var
  FPS: Integer;
  Duration: Integer;
begin
  if not assigned(Target) then
    Exit;

  FPS := StrToInt(eFPS.Text);
  Duration := StrToInt(eDuration.Text);

  // Prepare frame renderer

  FFrameRenderer.Width := ImageWidth;
  FFrameRenderer.Height := ImageHeight;

  FFrameRenderer.SVGRoot := FSVG.SVGRoot;
  FFrameRenderer.BackgroundColor := BackgroundColor;

  FFrameRenderer.RenderOptions := RenderOptions;
  FFrameRenderer.AutoViewbox := AutoViewbox;
  FFrameRenderer.AspectRatioAlign := AspectRatioAlign;
  FFrameRenderer.AspectRatioMeetOrSlice := AspectRatioMeetOrSlice;

  FFrameRenderer.FPS := FPS;
  FFrameRenderer.Duration := Duration;

  // Start recording

  aRecord.Enabled := False;
  try
    Target.Convert(FFrameRenderer);
  finally
    aRecord.Enabled := True;
  end;
end;

procedure TfrmRenderAnimatedSVG.DrawGraphic;
begin
  // Clear the background

  PaintBox1.Canvas.Brush.Color := clBtnFace;
  PaintBox1.Canvas.Brush.Style := bsSolid;
  PaintBox1.Canvas.FillRect(
    Rect(0, 0, PaintBox1.ClientWidth, PaintBox1.ClientHeight));

  // Draw graphic centered

  if assigned(FGraphic) then
  begin
    PaintBox1.Canvas.Draw(
      (PaintBox1.ClientWidth - FGraphic.Width) div 2,
      (PaintBox1.ClientHeight - FGraphic.Height) div 2,
      FGraphic);
  end;
end;

procedure TfrmRenderAnimatedSVG.FormCreate(Sender: TObject);
begin
  FUpdateCounter := 0;

  FFrameRenderer := TSVGFrameRenderer.Create(Self);
  FFrameRenderer.OnFrame := FrameRendererFrame;

  FSVG := TSVG2Graphic.Create;

  FBackgroundColor := clNone;
  FAutoViewbox := False;
  FAspectRatioAlign := TSVGAspectRatioALign.arXMidYMid;
  FAspectRatioMeetOrSlice := TSVGAspectRatioMeetOrSlice.arMeet;
  FRenderOptions := [sroClippath, sroFilters];

  ImageWidth := PaintBox1.Width;
  ImageHeight := PaintBox1.Height;

  FTargetList := TList<TSVGConvTarget>.Create;

  {$IFDEF SVG_TO_GIF}
  TargetAdd(TSVGConvTargetGif.Create(Self), 'GIF');
  {$ENDIF}
  {$IFDEF SVG_TO_AVI}
  TargetAdd(TSVGConvTargetAvi.Create(Self), 'AVI');
  {$ENDIF}
  {$IFDEF SVG_TO_APNG}
  TargetAdd(TSVGConvTargetAPng.Create(Self), 'APNG');
  {$ENDIF}

  PropsInit;
  PropsInitTarget;
  UpdatePreview;
end;

procedure TfrmRenderAnimatedSVG.FormDestroy(Sender: TObject);
begin
  FTargetList.Free;
  FSVG.Free;
end;

procedure TfrmRenderAnimatedSVG.FrameRendererFrame(Sender: TObject;
  const aAnimationTime: Integer; const aBitmap: TBitmap);
begin
  // Show progress

  eTime.Text := Format('%6d', [FFrameRenderer.AnimationTime]);

  FGraphic := FFrameRenderer.Bitmap;
  PaintBox1.Repaint;
end;

function TfrmRenderAnimatedSVG.GetAspectRatioAlign: TSVGAspectRatioAlign;
begin
  Result := FAspectRatioAlign;
end;

function TfrmRenderAnimatedSVG.GetAspectRatioMeetOrSlice: TSVGAspectRatioMeetOrSlice;
begin
  Result := FAspectRatioMeetOrSlice;
end;

function TfrmRenderAnimatedSVG.GetAutoViewbox: Boolean;
begin
  Result := FAutoViewbox;
end;

function TfrmRenderAnimatedSVG.GetBackgroundColor: TColor;
begin
  Result := FBackgroundColor;
end;

function TfrmRenderAnimatedSVG.GetFilename: string;
begin
  Result := FFilename;
end;

function TfrmRenderAnimatedSVG.GetImageHeight: Integer;
begin
  Result := FImageHeight;
end;

function TfrmRenderAnimatedSVG.GetImageWidth: Integer;
begin
  Result := FImageWidth;
end;

function TfrmRenderAnimatedSVG.GetRenderOptions: TSVGRenderOptions;
begin
  Result := FRenderOptions;
end;

function TfrmRenderAnimatedSVG.GetTarget: TSVGConvTarget;
begin
  Result := nil;

  if TabControl1.Tabs.Count = 0 then
    Exit;

  Result := FTargetList[TabControl1.TabIndex];
end;

procedure TfrmRenderAnimatedSVG.OpenSVG(const aFilename: string);
var
  R: TSVGRect;
begin
  UpdateBegin;
  try
    Filename := aFilename;

    FSVG.Filename := FFilename;
    FSVG.ParseSVG;

    R := FSVG.SVGRoot.CalcIntrinsicSize(
      SVGRect(0, 0, PaintBox1.Width, PaintBox1.Height));

    ImageWidth := Round(R.Width);
    ImageHeight := Round(R.Height);
  finally
    UpdateEnd;
  end;

  UpdatePreview;
end;

procedure TfrmRenderAnimatedSVG.PaintBox1Paint(Sender: TObject);
begin
  DrawGraphic;
end;

procedure TfrmRenderAnimatedSVG.PropEditButtonClick(aVle: TValueListEditor);
begin
  if aVle.Keys[aVle.Row] = k_filename then
  begin

    OpenDialog1.Filter := 'SVG files (*.svg)|*.SVG';

    if Opendialog1.Execute then
      OpenSVG(OpenDialog1.FileName);

  end else

  if aVle.Keys[aVle.Row] = k_background_color then
  begin
    if ColorDialog1.Execute then
      BackgroundColor := ColorDialog1.Color;
  end;
end;

procedure TfrmRenderAnimatedSVG.PropsInit;
begin
  if FUpdateCounter > 0 then
    Exit;

  vleSVG.Strings.Clear;

  // Initialize the SVG settings in the value list edit

  PropWriteFilename(vleSVG, k_filename, FFilename);

  PropWriteInteger(vleSVG, k_width, ImageWidth);
  PropWriteInteger(vleSVG, k_height, ImageHeight);

  PropWriteColor(vleSVG, k_background_color, BackgroundColor);

  PropWriteBoolean(vleSVG, k_auto_viewbox, AutoViewbox);

  PropWritePickList(vleSVG, k_aspect_ratio_align, Ord(AspectRatioAlign),
      'None;X-Min Y-Min;X-Mid Y-Min;X-Max Y-Min;X-Min Y-Mid;X-Mid Y-Mid;'
    + 'X-Max Y-Mid;X-Min Y-Max;X-Mid Y-Max;X-Max Y-Max;');
  PropWritePickList(vleSVG, k_aspect_ratio_meet_or_slice,
    Ord(AspectRatioMeetOrSlice), 'Meet;Slice;');

  PropWriteBoolean(vleSVG, k_render_options_clippaths,
    sroClippath in RenderOptions);
  PropWriteBoolean(vleSVG, k_render_options_filters,
    sroFilters in RenderOptions);
end;

procedure TfrmRenderAnimatedSVG.PropsInitTarget;
begin
  if assigned(Target) then
    Target.DoPropsInit;
end;

function TfrmRenderAnimatedSVG.PropReadBoolean(aVle: TValueListEditor;
  const aKey: string): Boolean;
begin
  Result := aVle.Values[aKey] = BoolToStr(True);
end;

function TfrmRenderAnimatedSVG.PropReadColor(aVle: TValueListEditor;
  const aKey: string): TColor;
begin
  Result := StringToColor(aVle.Values[aKey]);
end;

function TfrmRenderAnimatedSVG.PropReadFilename(aVle: TValueListEditor;
  const aKey: string): string;
begin
  Result := aVle.Values[aKey];
end;

function TfrmRenderAnimatedSVG.PropReadInteger(aVle: TValueListEditor;
  const aKey: string): Integer;
begin
  Result := StrToInt(Trim(aVle.Values[aKey]));
end;

function TfrmRenderAnimatedSVG.PropReadPickList(aVle: TValueListEditor;
  const aKey: string): Integer;
var
  Value: string;
begin
  Value := aVle.Values[aKey];
  Result := aVle.ItemProps[aKey].PickList.IndexOf(Value);
end;

procedure TfrmRenderAnimatedSVG.PropWriteBoolean(aVle: TValueListEditor;
  const aKey: string; const aValue: Boolean);
begin
  PropWritePickList(aVle, aKey, Ord(aValue),
    BoolToStr(False) + ';' + BoolToStr(True) + ';');
end;

procedure TfrmRenderAnimatedSVG.PropWriteColor(aVle: TValueListEditor;
  const aKey: string; const aValue: TColor);
begin
  if aVle.Strings.IndexOfName(aKey) = -1 then
  begin
    aVle.InsertRow(aKey, ColorToString(aValue), True);
    aVle.ItemProps[aKey].EditStyle := TEditStyle.esEllipsis;
  end else
    aVle.Strings.Values[aKey] := ColorToString(aValue);
end;

procedure TfrmRenderAnimatedSVG.PropWriteFilename(aVle: TValueListEditor;
  const aKey, aValue: string);
begin
  if aVle.Strings.IndexOfName(aKey) = -1 then
  begin
    aVle.InsertRow(aKey, aValue, True);
    aVle.ItemProps[aKey].EditStyle := TEditStyle.esEllipsis;
  end else
    aVle.Strings.Values[aKey] := aValue;
end;

procedure TfrmRenderAnimatedSVG.PropWriteInteger(aVle: TValueListEditor;
  const aKey: string; const aValue: Integer);
begin
  if aVle.Strings.IndexOfName(aKey) = -1 then
  begin
    aVle.InsertRow(aKey, IntToStr(aValue), True);
    aVle.ItemProps[aKey].EditMask := '9990';
  end else
    aVle.Strings.Values[aKey] := IntToStr(aValue);
end;

procedure TfrmRenderAnimatedSVG.PropWritePickList(aVle: TValueListEditor;
  const aKey: string; const aItemIndex: Integer; aPickList: string);
var
  sl: TStringList;
begin
  if aVle.Strings.IndexOfName(aKey) = -1 then
  begin
    sl := TStringList.Create;
    try
      sl.Delimiter := ';';
      sl.StrictDelimiter := True;
      sl.DelimitedText := aPickList;

      aVle.InsertRow(aKey, sl[aItemIndex], True);

      aVle.ItemProps[aKey].EditStyle := esPickList;
      aVle.ItemProps[aKey].ReadOnly := True;
      aVle.ItemProps[aKey].PickList := sl;
    finally
      sl.Free;
    end;
  end else
    aVle.Strings.Values[aKey] := aVle.ItemProps[aKey].PickList[aItemIndex];
end;

procedure TfrmRenderAnimatedSVG.SetAspectRatioAlign(
  const Value: TSVGAspectRatioAlign);
begin
  if FAspectRatioAlign <> Value then
  begin
    FAspectRatioAlign := Value;

    PropsInit;
  end;
end;

procedure TfrmRenderAnimatedSVG.SetAspectRatioMeetOrSlice(
  const Value: TSVGAspectRatioMeetOrSlice);
begin
  if FAspectRatioMeetOrSlice <> Value then
  begin
    FAspectRatioMeetOrSlice := Value;

    PropsInit;
  end;
end;

procedure TfrmRenderAnimatedSVG.SetAutoViewbox(const Value: Boolean);
begin
  if FAutoViewbox <> Value then
  begin
    FAutoViewbox := Value;

    PropsInit;
  end;
end;

procedure TfrmRenderAnimatedSVG.SetBackgroundColor(const Value: TColor);
begin
  if FBackgroundColor <> Value then
  begin
    FBackgroundColor := Value;

    PropsInit;
  end;
end;

procedure TfrmRenderAnimatedSVG.SetFilename(const Value: string);
var
  Target: TSVGConvTarget;
begin
  FFilename := Value;

  for Target in FTargetList do
    Target.Filename := FFilename;

  PropsInit;
end;

procedure TfrmRenderAnimatedSVG.SetImageHeight(const Value: Integer);
begin
  if FImageHeight <> Value then
  begin
    FImageHeight := Value;

    PropsInit;
  end;
end;

procedure TfrmRenderAnimatedSVG.SetImageWidth(const Value: Integer);
begin
  if FImageWidth <> Value then
  begin
    FImageWidth := Value;

    PropsInit;
  end;
end;

procedure TfrmRenderAnimatedSVG.SetRenderOptions(
  const Value: TSVGRenderOptions);
begin
  if FRenderOptions <> Value then
  begin
    FRenderOptions := Value;

    PropsInit;
  end;
end;

procedure TfrmRenderAnimatedSVG.TabControl1Change(Sender: TObject);
begin
  PropsInitTarget;
end;

procedure TfrmRenderAnimatedSVG.TargetAdd(aTarget: TSVGConvTarget;
  const aCaption: string);
begin
  FTargetList.Add(aTarget);
  aTarget.ValueListEditor := vleTarget;

  TabControl1.Tabs.Append(aCaption);
  PropsInit;
end;

procedure TfrmRenderAnimatedSVG.UpdateBegin;
begin
  Inc(FUpdateCounter);
end;

procedure TfrmRenderAnimatedSVG.UpdateEnd;
begin
  Dec(FUpdateCounter);
  if FUpdateCounter = 0 then
    PropsInit;
end;

procedure TfrmRenderAnimatedSVG.UpdatePreview;
begin
  FSVG.AutoViewbox := AutoViewBox;
  FSVG.AspectRatioAlign := AspectRatioAlign;
  FSVG.AspectRatioMeetOrSlice := AspectRatioMeetOrSlice;
  FSVG.RenderOptions := RenderOptions;
  FSVG.Height := ImageHeight;
  FSVG.Width := ImageWidth;

  FGraphic := FSVG;
  PaintBox1.Invalidate;

  aRecord.Enabled := assigned(Target) and assigned(FSVG.SVGRoot);
end;

procedure TfrmRenderAnimatedSVG.vleSVGEditButtonClick(Sender: TObject);
begin
  if Sender is TValueListEditor then
    PropEditButtonClick(Sender as TValueListEditor);
end;

procedure TfrmRenderAnimatedSVG.vleSVGValidate(Sender: TObject; ACol,
  ARow: Integer; const KeyName, KeyValue: string);
begin
  FAspectRatioAlign := TSVGAspectRatioAlign(
    PropReadPickList(vleSVG, k_aspect_ratio_align));
  FAspectRatioMeetOrSlice := TSVGAspectRatioMeetOrSlice(
    PropReadPickList(vleSVG, k_aspect_ratio_meet_or_slice));
  FAutoViewbox := PropReadBoolean(vleSVG, k_auto_viewbox);
  FBackgroundColor := PropReadColor(vleSVG, k_background_color);
  FImageHeight := PropReadInteger(vleSVG, k_height);
  FImageWidth := PropReadInteger(vleSVG, k_width);

  FRenderOptions := [];

  if PropReadBoolean(vleSVG, k_render_options_clippaths) then
    FRenderOptions := FRenderOptions + [sroClippath];

  if PropReadBoolean(vleSVG, k_render_options_filters) then
    FRenderOptions := FRenderOptions + [sroFilters];

  UpdatePreview;
end;

procedure TfrmRenderAnimatedSVG.vleTargetEditButtonClick(Sender: TObject);
begin
  if not assigned(Target) then
    Exit;

  if Sender is TValueListEditor then
  begin
    if (Sender as TValueListEditor).Strings.Count = 0 then
      Exit;

    Target.PropEditButtonClick(Sender as TValueListEditor);
  end;
end;

procedure TfrmRenderAnimatedSVG.vleTargetValidate(Sender: TObject; ACol, ARow: Integer;
  const KeyName, KeyValue: string);
begin
  if not assigned(Target) then
    Exit;

  if Sender is TValueListEditor then
  begin
    if (Sender as TValueListEditor).Strings.Count = 0 then
      Exit;

    Target.PropValidate(Sender as TValueListEditor,
      ACol, ARow, KeyName, KeyValue);
  end;
end;

// -----------------------------------------------------------------------------
//
// TSVGConvTarget
//
// -----------------------------------------------------------------------------

constructor TSVGConvTarget.Create(AOwner: TComponent);
begin
  inherited;

  FFilename := '';
end;

destructor TSVGConvTarget.Destroy;
begin
  inherited;
end;

procedure TSVGConvTarget.DoPropsInit;
begin
  FValueListEditor.Strings.Clear;
  frmRenderAnimatedSVG.PropWriteFilename(FValueListEditor, k_filename,
    FFilename);
end;

function TSVGConvTarget.GetFilename: string;
begin
  Result := FFilename;
end;

procedure TSVGConvTarget.PropValidate(aVle: TValueListEditor;
  ACol, ARow: Integer; const KeyName, KeyValue: string);
begin
  FFilename := frmRenderAnimatedSVG.PropReadFilename(aVle, k_filename);
end;

procedure TSVGConvTarget.SetFilename(const Value: string);
begin
  FFilename := ChangeFileExt(Value, FileExt);

  frmRenderAnimatedSVG.PropsInitTarget;
end;

{$IFDEF SVG_TO_AVI}
// -----------------------------------------------------------------------------
//
// TSVGConvTargetAvi
//
// -----------------------------------------------------------------------------

procedure TSVGConvTargetAvi.Convert(aFrameRenderer: TSVGFrameRenderer);
var
  Avi: TAviFromBitmaps;
  CompressionTag: FOURCC;
begin
  // http://francois-piette.blogspot.com/2013/07/creating-avi-file-from-bitmaps-using.html

  // Compression will only work if you have installed the necessary codecs
  // XVID compressor, download the setup from http://www.xvid.org

  CompressionTag := MKFOURCC('D', 'I', 'B', ' ');

  case Compression of
    acXVID: CompressionTag := MKFOURCC('x', 'v', 'i', 'd');
  end;


  Avi := TAviFromBitmaps.CreateAviFile(
    nil,
    Filename,
    CompressionTag,
    Cardinal(aFrameRenderer.FPS),
    1);
  try

    if aFrameRenderer.RenderFirst then
    begin
      try
        repeat
          // Add the bitmap to the avi

          Avi.AppendNewFrame(aFrameRenderer.Bitmap.Handle);

        until not aFrameRenderer.RenderNext;

      finally
        aFrameRenderer.RenderClose;
      end;
    end;

  finally
    Avi.Free;
  end;
end;

constructor TSVGConvTargetAvi.Create(AOwner: TComponent);
begin
  inherited;

  FCompression := acNone;
end;

destructor TSVGConvTargetAvi.Destroy;
begin
  inherited;
end;

procedure TSVGConvTargetAvi.DoPropsInit;
begin
  inherited;

  frmRenderAnimatedSVG.PropWritePickList(FValueListEditor, k_compression,
    Ord(FCompression), 'None;XVID;');
end;

function TSVGConvTargetAvi.GetCompression: TAviCompression;
begin
  Result := FCompression;
end;

function TSVGConvTargetAvi.GetFileExt: string;
begin
  Result := k_file_extension;
end;

procedure TSVGConvTargetAvi.PropEditButtonClick(aVle: TValueListEditor);
begin
  if aVle.Keys[aVle.Row] = k_filename then
  begin

    frmRenderAnimatedSVG.OpenDialog1.Filter := 'AVI files (*.avi)|*.AVI';

    if frmRenderAnimatedSVG.Opendialog1.Execute then
    begin
      Filename := frmRenderAnimatedSVG.Opendialog1.FileName;
    end;
  end
end;

procedure TSVGConvTargetAvi.PropValidate(aVle: TValueListEditor; ACol,
  ARow: Integer; const KeyName, KeyValue: string);
begin
  if aVle.Strings.Count = 0 then
    Exit;

  inherited;

  FCompression := TAViCompression(
    frmRenderAnimatedSVG.PropReadPickList(FValueListEditor, k_compression));
end;

procedure TSVGConvTargetAvi.SetCompression(const Value: TAviCompression);
begin
  if FCompression <> Value then
  begin
    FCompression := Value;

    frmRenderAnimatedSVG.PropsInitTarget;
  end;
end;

{$ENDIF}

{$IFDEF SVG_TO_GIF}
// -----------------------------------------------------------------------------
//
// TSVGConvTargetGif
//
// -----------------------------------------------------------------------------

procedure TSVGConvTargetGif.Convert(aFrameRenderer: TSVGFrameRenderer);
var
  GIFImage: TGifImage;
  GifFrame: TGIFFrame;
  GCE: TGIFGraphicControlExtension;
  LoopExt: TGIFAppExtNSLoop;
begin
  GIFImage := TGifImage.Create;
  try
    if aFrameRenderer.RenderFirst then
    begin
      try
        repeat
          // Add the bitmap to the gif image list

          GifFrame := GifImage.Add(aFrameRenderer.Bitmap);

          // Loop extension must be the first extension in the first frame

          if GifImage.Images.Count = 1 then
          begin
            LoopExt := TGIFAppExtNSLoop.Create(GifFrame);

            // Number of loops (0 = forever)

            LoopExt.Loops := AnimatedLoops;
          end;

          // Add Graphic Control Extension

          GCE := TGIFGraphicControlExtension.Create(GifFrame);

          // Delay is in hundreds of a second

          GCE.Delay := aFrameRenderer.Delay div 10;
          if Transparent then
          begin
            GCE.Transparent := True;
            GCE.TransparentColorIndex :=  GifFrame.Pixels[0, aFrameRenderer.Height - 1];
            GCE.Disposal := dmBackground;
          end;

        until not aFrameRenderer.RenderNext;

      finally
        aFrameRenderer.RenderClose;
      end;
    end;

    // Optimize Color map...

    if OptimizeColorMap then
      GifImage.OptimizeColorMap;

    // Optimize aGifImage frames...

    if OptimizeOptions <> [] then
      GifImage.Optimize(OptimizeOptions, rmNone, dmNearest, 0);

    if not GifImage.Empty then
      GifImage.SaveToFile(Filename);

  finally
    GifImage.Free;
  end;
end;

constructor TSVGConvTargetGif.Create(AOwner: TComponent);
begin
  inherited;

  FAnimatedLoops := 0;
  FOptimizeColorMap := True;
  FOptimizeOptions := [ooCrop, ooMerge];
  FTransparent := False;
end;

destructor TSVGConvTargetGif.Destroy;
begin
  inherited;
end;

procedure TSVGConvTargetGif.DoPropsInit;
begin
  inherited;

  frmRenderAnimatedSVG.PropWriteBoolean(FValueListEditor, k_transparent, False);
  frmRenderAnimatedSVG.PropWriteInteger(ValueListEditor, k_animation_loops, 0);
  frmRenderAnimatedSVG.PropWriteBoolean(FValueListEditor, k_optimize_color_map, True);
  frmRenderAnimatedSVG.PropWriteBoolean(FValueListEditor, k_merge, True);
  frmRenderAnimatedSVG.PropWriteBoolean(FValueListEditor, k_crop, True);
end;

function TSVGConvTargetGif.GetAnimatedLoops: Integer;
begin
  Result := FAnimatedLoops;
end;

function TSVGConvTargetGif.GetFileExt: string;
begin
  Result := k_file_extension;
end;

function TSVGConvTargetGif.GetOptimizeColorMap: Boolean;
begin
  Result := FOptimizeColorMap;
end;

function TSVGConvTargetGif.GetOptimizeOptions: TGIFOptimizeOptions;
begin
  Result := FOptimizeOptions;
end;

function TSVGConvTargetGif.GetTransparent: Boolean;
begin
  Result := FTransparent;
end;

procedure TSVGConvTargetGif.PropEditButtonClick(aVle: TValueListEditor);
begin
  if aVle.Keys[aVle.Row] = k_filename then
  begin

    frmRenderAnimatedSVG.OpenDialog1.Filter := 'GIF files (*.gif)|*.GIF';

    if frmRenderAnimatedSVG.Opendialog1.Execute then
    begin
      Filename := frmRenderAnimatedSVG.Opendialog1.FileName;
    end;
  end
end;

procedure TSVGConvTargetGif.PropValidate(aVle: TValueListEditor; ACol,
  ARow: Integer; const KeyName, KeyValue: string);
begin
  inherited;

  FAnimatedLoops := frmRenderAnimatedSVG.PropReadInteger(ValueListEditor, k_animation_loops);
  FOptimizeColorMap := frmRenderAnimatedSVG.PropReadBoolean(FValueListEditor, k_optimize_color_map);

  FOptimizeOptions := [];
  if frmRenderAnimatedSVG.PropReadBoolean(FValueListEditor, k_merge) then
    FOptimizeOptions := FOptimizeOptions + [ooMerge];
  if frmRenderAnimatedSVG.PropReadBoolean(FValueListEditor, k_crop) then
    FOptimizeOptions := FOptimizeOptions + [ooCrop];

  FTransparent := frmRenderAnimatedSVG.PropReadBoolean(FValueListEditor, k_transparent)
end;

procedure TSVGConvTargetGif.SetAnimatedLoops(const Value: Integer);
begin
  if FAnimatedLoops <> Value then
  begin
    FAnimatedLoops := Value;

    frmRenderAnimatedSVG.PropsInitTarget;
  end;
end;

procedure TSVGConvTargetGif.SetOptimizeColorMap(const Value: Boolean);
begin
  if FOptimizeColorMap <> Value then
  begin
    FOptimizeColorMap := Value;

    frmRenderAnimatedSVG.PropsInitTarget;
  end;
end;

procedure TSVGConvTargetGif.SetOptimizeOptions(
  const Value: TGIFOptimizeOptions);
begin
  if FOptimizeOptions <> Value then
  begin
    FOptimizeOptions := Value;

    frmRenderAnimatedSVG.PropsInitTarget;
  end;
end;

procedure TSVGConvTargetGif.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;

    frmRenderAnimatedSVG.PropsInitTarget;
  end;
end;
{$ENDIF}

{$IFDEF SVG_TO_APNG}
// -----------------------------------------------------------------------------
//
// TSVGConvTargetApng
//
// -----------------------------------------------------------------------------

procedure TSVGConvTargetApng.Convert(aFrameRenderer: TSVGFrameRenderer);
var
  DataArray: TDynImageDataArray;
  Meta: TMetadata;
  Format: TPNGFileFormat;
  Index: Integer;
begin
  // https://github.com/galfar/imaginglib

  SetLength(DataArray, 0);
  try
    if aFrameRenderer.RenderFirst then
    begin
      SetLength(DataArray, aFrameRenderer.StepCount);
      try
        repeat

          ConvertBitmapToData(aFrameRenderer.Bitmap, DataArray[aFrameRenderer.Step]);

        until not aFrameRenderer.RenderNext;

      finally
        aFrameRenderer.RenderClose;
      end;
    end;

    Meta := TMetadata.Create;
    try
      Meta.SetMetaItemForSaving(SMetaAnimationLoops, AnimatedLoops);
      for Index := 0 to Length(DataArray) - 1 do
        Meta.SetMetaItemForSaving(SMetaFrameDelay, 1000 / aFrameRenderer.FPS, Index);

      Format := TPNGFileFormat.Create(Meta);
      try
        Format.SaveToFile(FileName, DataArray);
      finally
        Format.Free;
      end;

    finally
      Meta.Free;
    end;

  finally
    FreeImagesInArray(DataArray);
  end;
end;

constructor TSVGConvTargetApng.Create(AOwner: TComponent);
begin
  inherited;

  FAnimatedLoops := 0;
end;

destructor TSVGConvTargetApng.Destroy;
begin
  inherited;
end;

procedure TSVGConvTargetApng.DoPropsInit;
begin
  inherited;

  frmRenderAnimatedSVG.PropWriteInteger(ValueListEditor, k_animation_loops, FAnimatedLoops)
end;

function TSVGConvTargetApng.GetAnimatedLoops: Integer;
begin
  Result := FAnimatedLoops;
end;

function TSVGConvTargetApng.GetFileExt: string;
begin
  Result := k_file_extension;
end;

procedure TSVGConvTargetApng.PropEditButtonClick(aVle: TValueListEditor);
begin
  if aVle.Keys[aVle.Row] = k_filename then
  begin

    frmRenderAnimatedSVG.OpenDialog1.Filter := 'PNG files (*.png)|*.PNG';

    if frmRenderAnimatedSVG.Opendialog1.Execute then
    begin
      Filename := frmRenderAnimatedSVG.Opendialog1.FileName;
    end;
  end
end;

procedure TSVGConvTargetApng.PropValidate(aVle: TValueListEditor; ACol,
  ARow: Integer; const KeyName, KeyValue: string);
begin
  inherited;

  FAnimatedLoops := frmRenderAnimatedSVG.PropReadInteger(ValueListEditor, k_animation_loops)
end;

procedure TSVGConvTargetApng.SetAnimatedLoops(const Value: Integer);
begin
  if FAnimatedLoops <> Value then
  begin
    FAnimatedLoops := Value;

    frmRenderAnimatedSVG.PropsInitTarget;
  end;
end;
{$ENDIF}

end.
