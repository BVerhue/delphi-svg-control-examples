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

{$DEFINE SVG_TO_APNG}
{$DEFINE SVG_TO_AVI}
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
  {$IFDEF SVG_TO_GIF}
  Vcl.Imaging.GIFImg,
  {$ENDIF}
  {$IFDEF SVG_TO_AVI}
  AviFromBitmaps,
  Vfw,
  {$ENDIF}
  {$IFDEF SVG_TO_APNG}
  ImagingTypes,
  Imaging,
  ImagingClasses,
  ImagingComponents,
  ImagingCanvases,
  ImagingBinary,
  ImagingUtility,
  {$ENDIF}
  BVE.SVG2Types,
  BVE.SVG2Intf,
  BVE.SVG2Graphic.VCL,
  BVE.SVGFrameRendererVCL;

type
  TSVGConvTarget = class(TCustomPanel)
  private
    FLabelFilename: TLabel;
    FFilename: string;
  protected
    procedure AddCheckBox(aCheckbox: TCheckBox);

    function GetFileExt: string; virtual; abstract;
    function GetFilename: string;
    procedure SetFilename(const Value: string);
    procedure SetParent(AParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Convert(aFrameRenderer: TSVGFrameRenderer); virtual; abstract;

    property Filename: string read GetFilename write SetFilename;
    property FileExt: string read GetFileExt;
  end;

  {$IFDEF SVG_TO_APNG}
  TSVGConvTargetApng = class(TSVGConvTarget)
  protected
    function GetFileExt: string; override;
    procedure SetParent(AParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Convert(aFrameRenderer: TSVGFrameRenderer); override;
  end;
  {$ENDIF}

  {$IFDEF SVG_TO_AVI}
  TSVGConvTargetAvi = class(TSVGConvTarget)
  private
    FCompression: TRadiogroup;
  protected
    function GetFileExt: string; override;
    function GetCompression: FOURCC;
    procedure SetParent(AParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Convert(aFrameRenderer: TSVGFrameRenderer); override;

    property Compression: FOURCC read GetCompression;
  end;
  {$ENDIF}

  {$IFDEF SVG_TO_GIF}
  TSVGConvTargetGif = class(TSVGConvTarget)
  private
    FTransparent: TCheckbox;
    FOptimizeColorMap: TCheckBox;
    FOptimizeOptionMerge: TCheckBox;
    FOptimizeOptionCrop: TCheckBox;
  protected
    function GetFileExt: string; override;
    function GetOptimizeColorMap: Boolean;
    function GetOptimizeOptions: TGIFOptimizeOptions;
    function GetTransparent: Boolean;
    procedure SetOptimizeColorMap(const Value: Boolean);
    procedure SetOptimizeOptions(const Value: TGIFOptimizeOptions);
    procedure SetParent(AParent: TWinControl); override;
    procedure SetTransparent(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Convert(aFrameRenderer: TSVGFrameRenderer); override;

    property Transparent: Boolean read GetTransparent write SetTransparent;
    property OptimizeColorMap: Boolean read GetOptimizeColorMap write SetOptimizeColorMap;
    property OptimizeOptions: TGIFOptimizeOptions read GetOptimizeOptions write SetOptimizeOptions;
  end;
  {$ENDIF}

  TForm1 = class(TForm)
    ActionManager1: TActionManager;
    aRecord: TAction;
    Panel1: TPanel;
    eTime: TEdit;
    aOpenSVG: TAction;
    OpenDialog1: TOpenDialog;
    Panel2: TPanel;
    Button1: TButton;
    Label3: TLabel;
    GroupBox1: TGroupBox;
    lblFilenameIn: TLabel;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Button2: TButton;
    Label2: TLabel;
    eDuration: TEdit;
    eFPS: TEdit;
    GroupBox4: TGroupBox;
    cbClippaths: TCheckBox;
    cbFilters: TCheckBox;
    GroupBox5: TGroupBox;
    Label12: TLabel;
    Label13: TLabel;
    cbAutoViewbox: TCheckBox;
    cbAspectRatioAlign: TComboBox;
    cbAspectRatioMeetOrSlice: TComboBox;
    GroupBox6: TGroupBox;
    cbBackgroundColor: TColorBox;
    Panel3: TPanel;
    Image1: TImage;
    GroupBox7: TGroupBox;
    eWidth: TEdit;
    eHeight: TEdit;
    Label5: TLabel;
    Label7: TLabel;
    PageControl1: TPageControl;
    Splitter1: TSplitter;
    GroupBox2: TGroupBox;
    procedure aRecordExecute(Sender: TObject);
    procedure aOpenSVGExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbAutoViewboxClick(Sender: TObject);
    procedure cbAspectRatioAlignChange(Sender: TObject);
    procedure cbAspectRatioMeetOrSliceChange(Sender: TObject);
    procedure cbClippathsClick(Sender: TObject);
    procedure cbFiltersClick(Sender: TObject);
    procedure cbBackgroundColorClick(Sender: TObject);
    procedure eWidthExit(Sender: TObject);
    procedure eHeightExit(Sender: TObject);
  private
    FFrameRenderer: TSVGFrameRenderer;
    FFilename: string;
    FSVG: TSVG2Graphic;

    FTargetList: TList<TSVGConvTarget>;

    procedure UpdatePreview;
  protected

    function GetAspectRatioAlign: TSVGAspectRatioAlign;
    function GetAspectRatioMeetOrSlice: TSVGAspectRatioMeetOrSlice;
    function GetAutoViewbox: Boolean;
    function GetBackgroundColor: TColor;
    function GetImageHeight: Integer;
    function GetImageWidth: Integer;
    function GetRenderOptions: TSVGRenderOptions;

    procedure SetAspectRatioAlign(const Value: TSVGAspectRatioAlign);
    procedure SetAspectRatioMeetOrSlice(
      const Value: TSVGAspectRatioMeetOrSlice);
    procedure SetAutoViewBox(const Value: Boolean);
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetImageHeight(const Value: Integer);
    procedure SetImageWidth(const Value: Integer);
    procedure SetRenderOptions(const Value: TSVGRenderOptions);

    procedure FrameRendererFrame(Sender: TObject; const aAnimationTime: Integer;
      const aBitmap: TBitmap);
  public
    procedure TargetAdd(aTarget: TSVGConvTarget; const aCaption: string);

    property AspectRatioAlign: TSVGAspectRatioAlign read GetAspectRatioAlign
      write SetAspectRatioAlign;
    property AspectRatioMeetOrSlice: TSVGAspectRatioMeetOrSlice
      read GetAspectRatioMeetOrSlice write SetAspectRatioMeetOrSlice;
    property AutoViewBox: Boolean read GetAutoViewbox write SetAutoViewBox;
    property BackgroundColor: TColor read GetBackgroundColor
      write SetBackgroundColor;
    property ImageHeight: Integer read GetImageHeight write SetImageHeight;
    property ImageWidth: Integer read GetImageWidth write SetImageWidth;
    property RenderOptions: TSVGRenderOptions read GetRenderOptions
      write SetRenderOptions;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.aOpenSVGExecute(Sender: TObject);
var
  R: TSVGRect;
  Target: TSVGConvTarget;
begin
  if OpenDialog1.Execute then
  begin
    FFilename := OpenDialog1.FileName;

    for Target in FTargetList do
      Target.Filename := FFilename;

    lblFilenameIn.Caption := ExtractFilename(FFileName);

    FSVG.Filename := FFilename;
    FSVG.ParseSVG;

    R := FSVG.SVGRoot.CalcIntrinsicSize(SVGRect(0, 0, Image1.Width, Image1.Height));
    ImageWidth := Round(R.Width);
    ImageHeight := Round(R.Height);

    UpdatePreview;
  end;
end;

procedure TForm1.aRecordExecute(Sender: TObject);
var
  FPS: Integer;
  Duration: Integer;
  Target: TSVGConvTarget;
begin
  FPS := StrToInt(eFPS.Text);
  Duration := StrToInt(eDuration.Text);

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

  Target := FTargetList[PageControl1.ActivePageIndex];

  Target.Filename := FFilename;
  Target.Convert(FFrameRenderer);
end;

procedure TForm1.cbAspectRatioAlignChange(Sender: TObject);
begin
  UpdatePreview;
end;

procedure TForm1.cbAspectRatioMeetOrSliceChange(Sender: TObject);
begin
  UpdatePreview;
end;

procedure TForm1.cbAutoViewboxClick(Sender: TObject);
begin
  UpdatePreview;
end;

procedure TForm1.cbBackgroundColorClick(Sender: TObject);
begin
  UpdatePreview;
end;

procedure TForm1.cbClippathsClick(Sender: TObject);
begin
  UpdatePreview;
end;

procedure TForm1.cbFiltersClick(Sender: TObject);
begin
  UpdatePreview;
end;

procedure TForm1.eHeightExit(Sender: TObject);
begin
  UpdatePreview;
end;

procedure TForm1.eWidthExit(Sender: TObject);
begin
  UpdatePreview;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FFrameRenderer := TSVGFrameRenderer.Create(Self);
  FFrameRenderer.OnFrame := FrameRendererFrame;

  FSVG := TSVG2Graphic.Create;

  ImageHeight := Image1.Height;
  ImageWidth := Image1.Width;

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
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FTargetList.Free;
  FSVG.Free;
end;

procedure TForm1.FrameRendererFrame(Sender: TObject;
  const aAnimationTime: Integer; const aBitmap: TBitmap);
begin
  // Show progress

  eTime.Text := Format('%6d', [FFrameRenderer.AnimationTime]);

  if assigned(Image1.Picture.Graphic)
  and (Image1.Picture.Graphic is TBitmap)
  and (BackgroundColor = clNone) then
  begin
    // Clear the background, otherwise we see traces of the previous frame
    // if the SVG has a transparent background

    Image1.Canvas.Brush.Style := bsSolid;
    Image1.Canvas.Brush.Color := clWindow;
    Image1.Canvas.FillRect(Rect(0, 0, Image1.ClientWidth, Image1.ClientHeight));
    Image1.Repaint;
  end;

  Image1.Picture.Assign(FFrameRenderer.Bitmap);
  Image1.Repaint;
end;

function TForm1.GetAspectRatioAlign: TSVGAspectRatioAlign;
begin
  Result := TSVGAspectRatioAlign(cbAspectRatioAlign.ItemIndex);
end;

function TForm1.GetAspectRatioMeetOrSlice: TSVGAspectRatioMeetOrSlice;
begin
  Result := TSVGAspectRatioMeetOrSlice(cbAspectRatioMeetOrSlice.ItemIndex);
end;

function TForm1.GetAutoViewbox: Boolean;
begin
  Result := cbAutoViewbox.Checked;
end;

function TForm1.GetBackgroundColor: TColor;
begin
  Result := cbBackgroundColor.Selected;
end;

function TForm1.GetImageHeight: Integer;
begin
  Result := StrToInt(eHeight.Text);
end;

function TForm1.GetImageWidth: Integer;
begin
  Result := StrToInt(eWidth.Text);
end;

function TForm1.GetRenderOptions: TSVGRenderOptions;
begin
  Result := [];

  if cbClippaths.Checked then
    Result := Result + [sroClippath];

  if cbFilters.Checked then
    Result := Result + [sroFilters];
end;

procedure TForm1.SetAspectRatioAlign(const Value: TSVGAspectRatioAlign);
begin
  cbAspectRatioAlign.ItemIndex := Ord(Value);
end;

procedure TForm1.SetAspectRatioMeetOrSlice(
  const Value: TSVGAspectRatioMeetOrSlice);
begin
  cbAspectRatioMeetOrSlice.ItemIndex := Ord(Value);
end;

procedure TForm1.SetAutoViewBox(const Value: Boolean);
begin
  cbAutoViewbox.Checked := Value;
end;

procedure TForm1.SetBackgroundColor(const Value: TColor);
begin
  cbBackgroundColor.Selected := Value;
end;

procedure TForm1.SetImageHeight(const Value: Integer);
begin
  eHeight.Text := IntToStr(Value);
end;

procedure TForm1.SetImageWidth(const Value: Integer);
begin
  eWidth.Text := IntToStr(Value);
end;

procedure TForm1.SetRenderOptions(const Value: TSVGRenderOptions);
begin
  cbClippaths.Checked := sroClippath in Value;
  cbFilters.Checked := sroFilters in Value;
end;

procedure TForm1.TargetAdd(aTarget: TSVGConvTarget; const aCaption: string);
var
  TabSheet: TTabSheet;
begin
  FTargetList.Add(aTarget);

  TabSheet := TTabSheet.Create(PageControl1);
  TabSheet.Caption := aCaption;
  TabSheet.PageControl := PageControl1;

  aTarget.Parent := TabSheet;
  aTarget.Align := alClient;
end;

procedure TForm1.UpdatePreview;
begin
  FSVG.AutoViewbox := AutoViewBox;
  FSVG.AspectRatioAlign := AspectRatioAlign;
  FSVG.AspectRatioMeetOrSlice := AspectRatioMeetOrSlice;
  FSVG.RenderOptions := RenderOptions;
  FSVG.Height := ImageHeight;
  FSVG.Width := ImageWidth;

  Image1.Picture.Assign(FSVG);
end;

{ TSVGConvTarget }

procedure TSVGConvTarget.AddCheckBox(aCheckbox: TCheckBox);
begin
  aCheckbox.Parent := Self;
  aCheckbox.Align := TAlign.alTop;
  aCheckbox.AlignWithMargins := True;
end;

constructor TSVGConvTarget.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TSVGConvTarget.Destroy;
begin
  inherited;
end;

function TSVGConvTarget.GetFilename: string;
begin
  Result := FFilename;
end;

procedure TSVGConvTarget.SetFilename(const Value: string);
begin
  FFilename := ChangeFileExt(Value, FileExt);

  FLabelFilename.Caption := ExtractFilename(FFileName);
end;

procedure TSVGConvTarget.SetParent(AParent: TWinControl);
begin
  inherited;

  if assigned(AParent) then
  begin
    FLabelFilename := TLabel.Create(Self);
    FLabelFilename.Parent := Self;
    FLabelFilename.Align := alTop;
    FLabelFilename.AlignWithMargins := True;
  end;
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
begin
  Avi := TAviFromBitmaps.CreateAviFile(
    nil,
    Filename,
    Compression,
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
end;

destructor TSVGConvTargetAvi.Destroy;
begin
  inherited;
end;

function TSVGConvTargetAvi.GetCompression: FOURCC;
begin
  if FCompression.ItemIndex = 1 then
    Result := MKFOURCC('x', 'v', 'i', 'd') // XVID (MPEG-4) compression
  else
    Result := MKFOURCC('D', 'I', 'B', ' '); // No compression
end;

function TSVGConvTargetAvi.GetFileExt: string;
begin
  Result := '.avi';
end;

procedure TSVGConvTargetAvi.SetParent(AParent: TWinControl);
begin
  inherited;

  if assigned(AParent) then
  begin
    FCompression := TRadiogroup.Create(Self);
    FCompression.Caption := 'Compression';
    FCompression.Parent := Self;
    FCompression.Align := alTop;
    FCompression.AlignWithMargins := True;
    FCompression.Items.Add('None');
    FCompression.Items.Add('XVid');
    FCompression.ItemIndex := 0;
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

            LoopExt.Loops := 0;
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

    {Image1.Picture.Assign(GifImage);
    Image1.Width := GifImage.Width;
    Image1.Height := GifImage.Height;

    (Image1.Picture.Graphic as TGIFImage).Animate := True;}

    if not GifImage.Empty then
      GifImage.SaveToFile(Filename);
  finally
    GifImage.Free;
  end;
end;

constructor TSVGConvTargetGif.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TSVGConvTargetGif.Destroy;
begin
  inherited;
end;

function TSVGConvTargetGif.GetFileExt: string;
begin
  Result := '.gif';
end;

function TSVGConvTargetGif.GetOptimizeColorMap: Boolean;
begin
  Result := FOptimizeColorMap.Checked;
end;

function TSVGConvTargetGif.GetOptimizeOptions: TGIFOptimizeOptions;
begin
  Result := [];
  if FOptimizeOptionMerge.Checked then
    Result := Result + [ooMerge];
  if FOptimizeOptionCrop.Checked then
    Result := Result + [ooCrop];
end;

function TSVGConvTargetGif.GetTransparent: Boolean;
begin
  Result := FTransparent.Checked;
end;

procedure TSVGConvTargetGif.SetOptimizeColorMap(const Value: Boolean);
begin
  FOptimizeColorMap.Checked := Value;
end;

procedure TSVGConvTargetGif.SetOptimizeOptions(
  const Value: TGIFOptimizeOptions);
begin
  FOptimizeOptionMerge.Checked := ooMerge in Value;
  FOptimizeOptionCrop.Checked := ooCrop in Value;
end;

procedure TSVGConvTargetGif.SetParent(AParent: TWinControl);
begin
  inherited;

  if assigned(AParent) then
  begin
    FTransparent := TCheckBox.Create(Self);
    FTransparent.Caption := 'Transparent';
    AddCheckbox(FTransparent);
    FTransparent.Checked := False;

    FOptimizeColorMap := TCheckBox.Create(Self);
    FOptimizeColorMap.Caption := 'Optimize color map';
    AddCheckbox(FOptimizeColorMap);
    FOptimizeColorMap.Checked := True;

    FOptimizeOptionMerge := TCheckBox.Create(Self);
    FOptimizeOptionMerge.Caption := 'Merge';
    AddCheckbox(FOptimizeOptionMerge);
    FOptimizeOptionMerge.Checked := True;

    FOptimizeOptionCrop := TCheckBox.Create(Self);
    FOptimizeOptionCrop.Caption := 'Crop';
    AddCheckbox(FOptimizeOptionCrop);
    FOptimizeOptionCrop.Checked := True;
  end;
end;

procedure TSVGConvTargetGif.SetTransparent(const Value: Boolean);
begin
  FTransparent.Checked := Value;
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
  MultiImage: TMultiImage;
  ImageData: TImageData;
  DataArray: TDynImageDataArray;
begin
  MultiImage := TMultiImage.CreateFromParams(
    aFrameRenderer.Width,
    aFrameRenderer.Height,
    ifA8R8G8B8,
    aFrameRenderer.Duration * aFrameRenderer.FPS div 1000);
  try
    if aFrameRenderer.RenderFirst then
    begin
      SetLength(DataArray, aFrameRenderer.StepCount +1);
      try
        repeat
          // Add the bitmap to the avi

          ConvertBitmapToData(aFrameRenderer.Bitmap, ImageData);
          try
            DataArray[aFrameRenderer.Step] := ImageData;
            //MultiImage.AddImage(ImageData);
          finally
            //ImageData.Free;
          end;

        until not aFrameRenderer.RenderNext;

      finally
        aFrameRenderer.RenderClose;
      end;
    end;

    //MultiImage.ActiveImage := 0;
    MultiImage.AssignFromArray(DataArray);

    MultiImage.SaveToFile(FFilename);

  finally
    MultiImage.Free;
  end;
end;

constructor TSVGConvTargetApng.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TSVGConvTargetApng.Destroy;
begin


  inherited;
end;

function TSVGConvTargetApng.GetFileExt: string;
begin
  Result := '.png';
end;

procedure TSVGConvTargetApng.SetParent(AParent: TWinControl);
begin
  inherited;

end;
{$ENDIF}

end.
