unit BVE.SVGPrintPreviewVCL;

// ------------------------------------------------------------------------------
//
//                          SVG Control Package 2.0
//                       Copyright (c) 2015 Bruno Verhue
//
// ------------------------------------------------------------------------------

// The SVG Editor need at least version v2.40 update 9 of the SVG library

{$Include ContextSettingsVCL.inc}

{$IFDEF SVGDirect2d3D11}
  {$DEFINE SVGSupportsCmdList}
{$ENDIF}

{$IFDEF SVGGDIP}
  {$DEFINE SVGSupportsCmdList}
{$ENDIF}


interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Types,
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.ExtCtrls,
  Vcl.Printers,
  BVE.SVG2Types,
  BVE.SVG2Intf,
  BVE.SVG2Elements,
  BVE.SVGToolVCL;

type
  TSVGPrintUnits = (puMm, puCm, puInch, puPixel);

  TSVGPrintPreview = class(TCustomControl)
  private
    // Only one SVG can be printed, however, the SVG can be printer
    // over multiple pages

    FRoot: ISVGRoot;

    FViewportScale: TSVGFloat;

    FDPIX, FDPIY: Integer;
    FPrinterPageWidth: TSVGFLoat;
    FPrinterPageHeight: TSVGFLoat;
    FPxToPt: TSVGPoint;

    FPagesVertical: Integer;
    FPagesHorizontal: Integer;

    FPreviewPageMargin: Integer;
    FPreviewPageWidth: Integer;
    FPreviewPageHeight: Integer;

    FNeedRecreatePreview: Boolean;

    {$IFDEF SVGSupportsCmdList}
    FCmdList: ISVGRenderContextCmdList;
    {$ENDIF}
    FCmdListWidth: Integer;
    FCmdListHeight: Integer;

    FPrintUnits: TSVGPrintUnits;

    FMarginLeft: TSVGFloat;
    FMarginTop: TSVGFloat;
    FMarginRight: TSVGFloat;
    FMarginBottom: TSVGFloat;

    FAutoViewbox: Boolean;
    FAspectRatioAlign: TSVGAspectRatioAlign;
    FAspectRatioMeetOrSlice: TSVGAspectRatioMeetOrSlice;

    FGlueEdge: TSVGFloat;

    FPagePreviewList: TList<TBitmap>;

    function CalcRCUnits(const aValue, aDPI: TSVGFloat): TSVGFloat;
    procedure CalcPrinterDimensions;
    function CalcViewport: TSVGRect;
    function CalcViewportMatrix: TSVGMatrix;
  protected
    function GetPageCount: Integer;
    function GetPageViewBox(const aIndex: Integer): TSVGRect;

    procedure SetAutoViewbox(const Value: Boolean);
    procedure SetAspectRatioAlign(const Value: TSVGAspectRatioAlign);
    procedure SetAspectRatioMeetOrSlice(
      const Value: TSVGAspectRatioMeetOrSlice);
    procedure SetGlueEdge(const Value: TSVGFloat);
    procedure SetMarginBottom(const Value: TSVGFloat);
    procedure SetMarginLeft(const Value: TSVGFloat);
    procedure SetMarginRight(const Value: TSVGFloat);
    procedure SetMarginTop(const Value: TSVGFloat);
    procedure SetRoot(const Value: ISVGRoot);
    procedure SetPagesHorizontal(const Value: Integer);
    procedure SetPagesVertical(const Value: Integer);
    procedure SetPrintUnits(const Value: TSVGPrintUnits);

    procedure MarginsChange(aSender: TObject);

    procedure PagePreviewCalcSize;
    procedure PagePreviewListClear;
    procedure PagePreviewListCreate;

    {$IFDEF SVGSupportsCmdList}
    procedure RenderToCmdList;
    {$ENDIF}

    property PageViewbox[const aIndex: Integer]: TSVGRect read GetPageViewBox;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Print(const aPrintJobName: string);

    procedure Paint; override;
    procedure Repaint; override;
    procedure Resize; override;

    property AutoViewbox: Boolean read FAutoViewbox write SetAutoViewbox;
    property AspectRatioAlign: TSVGAspectRatioAlign read FAspectRatioAlign write SetAspectRatioAlign;
    property AspectRatioMeetOrSlice: TSVGAspectRatioMeetOrSlice read FAspectRatioMeetOrSlice write SetAspectRatioMeetOrSlice;
    property GlueEdge: TSVGFloat read FGlueEdge write SetGlueEdge;
    property MarginLeft: TSVGFloat read FMarginLeft write SetMarginLeft;
    property MarginTop: TSVGFloat read FMarginTop write SetMarginTop;
    property MarginRight: TSVGFloat read FMarginRight write SetMarginRight;
    property MarginBottom: TSVGFloat read FMarginBottom write SetMarginBottom;
    property PageCount: Integer read GetPageCount;
    property PagesHorizontal: Integer read FPagesHorizontal write SetPagesHorizontal;
    property PagesVertical: Integer read FPagesVertical write SetPagesVertical;
    property Root: ISVGRoot read FRoot write SetRoot;
    property Units: TSVGPrintUnits read FPrintUnits write SetPrintUnits;
  end;

implementation
uses
  BVE.SVG2GeomUtility,
  BVE.SVG2Context,
  BVE.SVG2Elements.VCL;

{ TSVGPrintPreview }

function TSVGPrintPreview.CalcRCUnits(const aValue, aDPI: TSVGFloat): TSVGFloat;
begin
  if TSVGRenderContextManager.RenderContextType = rcDirect2D then
  begin
    // Direct2D coords are in device independent pixels

    case FPrintUnits of
      puPixel:
        begin
          Result := 96 / aDPI * aValue;
        end;
      puMm:
        begin
          Result := 96 / 25.4 * aValue;
        end;
      puCm:
        begin
          Result := 96 / 2.54 * aValue;
        end;
      puInch:
        begin
           Result := 96 * aValue;
        end;
      else
        Result := aValue;
    end;

  end else begin
    // GDI+ coords are in pixels

    case FPrintUnits of
      puPixel:
        begin
          Result := aDPI / aDPI * aValue;
        end;
      puMm:
        begin
          Result := aDPI / 25.4 * aValue;
        end;
      puCm:
        begin
          Result := aDPI / 2.54 * aValue;
        end;
      puInch:
        begin
           Result := aValue * aDPI;
        end;
      else
        Result := aValue;
    end;
  end;
end;

procedure TSVGPrintPreview.CalcPrinterDimensions;
begin
  FPrinterPageWidth := Printer.PageWidth;
  FPrinterPageHeight := Printer.PageHeight;

  FDPIX := GetDeviceCaps(Printer.Handle, LOGPIXELSX);
  FDPIY := GetDeviceCaps(Printer.Handle, LOGPIXELSY);

  if FDPIX = 0 then
    FDPIX := 96;

  if FDPIY = 0 then
    FDPIY := 96;

  FPxToPt := SVGPoint(FDPIX / 96, FDPIY / 96);

  if TSVGRenderContextManager.RenderContextType = rcDirect2D then
  begin
    // Printer.PageWidth and Printer.PageHeight are in pixels, we need to
    // convert them to points

    FPrinterPageWidth := FPrinterPageWidth / FPxToPt.X;
    FPrinterPageHeight := FPrinterPageHeight / FPxToPt.Y;

    FPxToPt := SVGPoint(1.0, 1.0);
  end;
end;

function TSVGPrintPreview.CalcViewport: TSVGRect;
begin
  Result := SVGRect(
    CalcRCUnits(FMarginLeft, FDPIX),
    CalcRCUnits(FMarginTop, FDPIY),
    PagesHorizontal * FPrinterPageWidth - CalcRCUnits(FMarginRight, FDPIX) - CalcRCUnits(FGlueEdge, FDPIX) * (PagesHorizontal - 1),
    PagesVertical * FPrinterPageHeight - CalcRCUnits(FMarginBottom, FDPIX) - CalcRCUnits(FGlueEdge, FDPIY) * (PagesVertical - 1));
end;

function TSVGPrintPreview.CalcViewportMatrix: TSVGMatrix;
var
  Viewport, Viewbox: TSVGRect;
begin
  if FAutoViewbox then
  begin

    // The size of the SVG

    ViewBox := SVGRect(0, 0, FCmdListWidth, FCmdListHeight);

    // The available area for the SVG

    ViewPort := CalcViewport;

    Result := CreateViewBoxMatrix(
      Viewport,
      ViewBox,
      FAspectRatioAlign,
      FAspectRatioMeetOrSlice);

  end else begin

    Result := TSVGMatrix.CreateTranslation(
      CalcRCUnits(FMarginLeft, FDPIX),
      CalcRCUnits(FMarginTop, FDPIY));

    Result := TSVGMatrix.Multiply(
      Result,
      TSVGMatrix.CreateScaling(FPxToPt.X, FPxToPt.Y));
  end;

end;

constructor TSVGPrintPreview.Create(AOwner: TComponent);
begin
  inherited;

  FRoot := nil;
  FPagePreviewList := TList<TBitmap>.Create;
  FPreviewPageMargin := 10;

  FViewportScale := 1.0;

  FPagesHorizontal := 1;
  FPagesVertical := 1;

  FPrintUnits := puMM;

  FMarginLeft := 0;
  FMarginTop := 0;
  FMarginRight := 0;
  FMarginBottom := 0;

  FGlueEdge := 0;

  FAutoViewbox := False;
  FAspectRatioAlign := TSVGAspectRatioAlign.arXMidYMid;
  FAspectRatioMeetOrSlice := TSVGAspectRatioMeetOrSlice.arMeet;

  FNeedRecreatePreview := False;
end;

destructor TSVGPrintPreview.Destroy;
begin
  PagePreviewListClear;
  FPagePreviewList.Free;

  inherited;
end;

function TSVGPrintPreview.GetPageCount: Integer;
begin
  Result := PagesHorizontal * PagesVertical;
end;

function TSVGPrintPreview.GetPageViewBox(const aIndex: Integer): TSVGRect;
var
  L, T, W, H: TSVGFloat;
  R, C: Integer;
begin
  Result := TSVGRect.CreateUndefined;

  if aIndex < PageCount then
  begin
    R := aIndex div PagesHorizontal;
    C := aIndex mod PagesHorizontal;

    W := FPrinterPageWidth;
    H := FPrinterPageHeight;

    L := C * (W - CalcRCUnits(FGlueEdge, FDPIX));
    T := R * (H - CalcRCUnits(FGlueEdge, FDPIY));

    if C < PagesHorizontal - 1 then
      W := W - CalcRCUnits(FGlueEdge, FDPIX);

    if R < PagesVertical - 1 then
      H := H - CalcRCUnits(FGlueEdge, FDPIY);

    Result := SVGRect(L, T, L + W, T + H);
  end;
end;

procedure TSVGPrintPreview.MarginsChange(aSender: TObject);
begin
  FNeedRecreatePreview := True;
  Invalidate;
end;

procedure TSVGPrintPreview.PagePreviewCalcSize;
var
  M: TSVGMatrix;
  Viewport, Viewbox, R: TSVGRect;
begin
  if (PagesHorizontal <= 0) and (PagesVertical <= 0) then
  begin
    FPreviewPageWidth := 0;
    FPreviewPageHeight := 0;
  end;

  // Available space in client area for one page

  Viewport := SVGRect(
    0, 0,
    Round((ClientWidth - (PagesHorizontal + 1) * FPreviewPageMargin) / PagesHorizontal),
    Round((ClientHeight - (PagesVertical + 1) * FPreviewPageMargin) / PagesVertical));

  // Spcae needed for one page

  Viewbox := SVGRect(
    0, 0,
    FPrinterPageWidth,
    FPrinterPageHeight);

  // Calculate the scale factor

  M := CreateViewBoxMatrix(
    Viewport,
    ViewBox,
    TSVGAspectRatioAlign.arXMidYMid,
    TSVGAspectRatioMeetOrSlice.arMeet);

  FViewportScale := M.m11;

  R := TransformRect(ViewBox, M);

  FPreviewPageWidth := Round(R.Width);
  FPreviewPageHeight := Round(R.Height);
end;

procedure TSVGPrintPreview.PagePreviewListClear;
begin
  while FPagePreviewList.Count > 0 do
  begin
    FPagePreviewList[0].Free;
    FPagePreviewList.Delete(0);
  end;
end;

procedure TSVGPrintPreview.PagePreviewListCreate;
var
  i, Count: Integer;
  R: TSVGRect;
  Viewport: TSVGRect;
  Bitmap: TBitmap;
  RC: ISVGRenderContext;
  M, MV: TSVGMatrix;
begin
  FNeedRecreatePreview := False;

  CalcPrinterDimensions;

  PagePreviewListClear;

  {$IFDEF SVGSupportsCmdList}
  if not assigned(FCmdList) then
    Exit;
  {$ENDIF}

  PagePreviewCalcSize;

  Count := PageCount;

  M := CalcViewportMatrix;

  ViewPort := CalcViewport;

  for i := 0 to Count - 1 do
  begin
    R := PageViewbox[i];

    MV := TSVGMatrix.Multiply(
      TSVGMatrix.CreateTranslation(-R.Left, -R.Top),
      TSVGMatrix.CreateScaling(FViewportScale, FViewportScale));

    Bitmap := TSVGRenderContextManager.CreateCompatibleBitmap(FPreviewPageWidth, FPreviewPageHeight);
    FPagePreviewList.Add(Bitmap);

    RC := TSVGRenderContextManager.CreateRenderContextBitmap(Bitmap);

    RC.BeginScene;
    try
      RC.Clear(SVGColorWhite);

      RC.PushClipRect(
        TSVGRect.Intersect(
          TransformRect(Viewport, MV),
          TransformRect(R, MV)
          )
        );
      try
        // Draw margins

        RC.ApplyStroke(TSVGSolidBrush.Create(SVGColorGray), 1);
        RC.DrawRect(TransformRect(ViewPort, MV));

        RC.Matrix := TSVGMatrix.Multiply(RC.Matrix, M);
        RC.Matrix := TSVGMatrix.Multiply(RC.Matrix, MV);

        // Draw SVG

        {$IFDEF SVGSupportsCmdList}
        RC.DrawCmdList(FCmdList, SVGRect(0, 0, FCmdListWidth, FCmdListHeight));
        {$ELSE}
        SVGRenderToRenderContext(
          FRoot,
          RC,
          FCmdListWidth,
          FCmdListHeight,
          [sroFilters, sroClippath],
          False);
        {$ENDIF}

      finally
        RC.PopClipRect;
      end;

    finally
      RC.EndScene;
    end;
  end;
end;

procedure TSVGPrintPreview.Paint;
var
  i: Integer;
  X, Y: Integer;
begin
  inherited;

  if FNeedRecreatePreview then
    PagePreviewListCreate;

  Canvas.Brush.Color := clGray;
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(ClientRect);

  for i := 0 to FPagePreviewList.Count - 1 do
  begin
    X := FPreviewPageMargin + (i mod PagesHorizontal) * (FPreviewPageWidth + FPreviewPageMargin);
    Y := FPreviewPageMargin + (i div PagesHorizontal) * (FPreviewPageHeight + FPreviewPageMargin);

    Canvas.Draw(X, Y, FPagePreviewList[i]);
  end;
end;

procedure TSVGPrintPreview.Print(const aPrintJobName: string);
var
  i, Count: Integer;
  R: TSVGRect;
  Viewport: TSVGRect;
  PrintJob: ISVGPrintJob;
  RC: ISVGRenderContext;
  M, MV: TSVGMatrix;
begin
  CalcPrinterDimensions;

  {$IFDEF SVGSupportsCmdList}
  if not assigned(FCmdList) then
    Exit;
  {$ENDIF}

  Count := PageCount;

  M := CalcViewportMatrix;

  ViewPort := CalcViewport;

  PrintJob := TSVGRenderContextManager.CreatePrintJob(aPrintJobName);

  for i := 0 to Count - 1 do
  begin
    R := PageViewbox[i];

    MV := TSVGMatrix.CreateTranslation(-R.Left, -R.Top);

    RC := PrintJob.BeginPage(FPrinterPageWidth, FPrinterPageHeight);
    try
      RC.BeginScene;
      try
        RC.Clear(SVGColorNone);

        RC.PushClipRect(
          TSVGRect.Intersect(
            TransformRect(Viewport, MV),
            TransformRect(R, MV)
            )
          );
        try
          RC.Matrix := TSVGMatrix.Multiply(RC.Matrix, M);
          RC.Matrix := TSVGMatrix.Multiply(RC.Matrix, MV);

          {$IFDEF SVGSupportsCmdList}
          RC.DrawCmdList(FCmdList, SVGRect(0, 0, FCmdListWidth, FCmdListHeight));
          {$ELSE}
          SVGRenderToRenderContext(
            FRoot,
            RC,
            FCmdListWidth,
            FCmdListHeight,
            [sroFilters, sroClippath],
            False);
          {$ENDIF}
        finally
          RC.PopClipRect;
        end;

      finally
        RC.EndScene;
      end;
    finally
      PrintJob.EndPage;
    end;
  end;
end;

{$IFDEF SVGSupportsCmdList}
procedure TSVGPrintPreview.RenderToCmdList;
var
  RC: ISVGRenderContext;
begin
  if not assigned(FRoot) then
    Exit;

  CalcPrinterDimensions;

  FCmdList := TSVGRenderContextManager.CreateCmdList;

  RC := TSVGRenderContextManager.CreateRenderContextCmdList(
    FCmdList, FCmdListWidth, FCmdListHeight);

  RC.BeginScene;
  try
    SVGRenderToRenderContext(
      FRoot,
      RC,
      FCmdListWidth,
      FCmdListHeight,
      [sroFilters, sroClippath],
      False);

  finally
    RC.EndScene;
  end;
end;
{$ENDIF}

procedure TSVGPrintPreview.Repaint;
begin
  FNeedRecreatePreview := True;

  inherited;
end;

procedure TSVGPrintPreview.Resize;
begin
  inherited;

  FNeedRecreatePreview := True;
end;

procedure TSVGPrintPreview.SetAspectRatioAlign(
  const Value: TSVGAspectRatioAlign);
begin
  if FAspectRatioAlign <> Value then
  begin
    FAspectRatioAlign := Value;
    FNeedRecreatePreview := True;
    Invalidate;
  end;
end;

procedure TSVGPrintPreview.SetAspectRatioMeetOrSlice(
  const Value: TSVGAspectRatioMeetOrSlice);
begin
  if FAspectRatioMeetOrSlice <> Value then
  begin
    FAspectRatioMeetOrSlice := Value;
    FNeedRecreatePreview := True;
    Invalidate;
  end;
end;

procedure TSVGPrintPreview.SetAutoViewbox(const Value: Boolean);
begin
  if FAutoViewbox <> Value then
  begin
    FAutoViewbox := Value;
    FNeedRecreatePreview := True;
    Invalidate;
  end;
end;

procedure TSVGPrintPreview.SetGlueEdge(const Value: TSVGFloat);
begin
  if FGlueEdge <> Value then
  begin
    FGlueEdge := Value;
    FNeedRecreatePreview := True;
    Invalidate;
  end;
end;

procedure TSVGPrintPreview.SetMarginBottom(const Value: TSVGFloat);
begin
  if FMarginBottom <> Value then
  begin
    FMarginBottom := Value;
    FNeedRecreatePreview := True;
    Invalidate;
  end;
end;

procedure TSVGPrintPreview.SetMarginLeft(const Value: TSVGFloat);
begin
  if FMarginLeft <> Value then
  begin
    FMarginLeft := Value;
    FNeedRecreatePreview := True;
    Invalidate;
  end;
end;

procedure TSVGPrintPreview.SetMarginRight(const Value: TSVGFloat);
begin
  if FMarginRight <> Value then
  begin
    FMarginRight := Value;
    FNeedRecreatePreview := True;
    Invalidate;
  end;
end;

procedure TSVGPrintPreview.SetMarginTop(const Value: TSVGFloat);
begin
  if FMarginTop <> Value then
  begin
    FMarginTop := Value;
    FNeedRecreatePreview := True;
    Invalidate;
  end;
end;

procedure TSVGPrintPreview.SetPagesHorizontal(const Value: Integer);
begin
  if FPagesHorizontal <> Value then
  begin
    if Value <= 0 then
      FPagesHorizontal := 1
    else
      FPagesHorizontal := Value;

    FNeedRecreatePreview := True;
    Invalidate;
  end;
end;

procedure TSVGPrintPreview.SetPagesVertical(const Value: Integer);
begin
  if FPagesVertical <> Value then
  begin
    if Value <= 0 then
      FPagesVertical := 1
    else
      FPagesVertical := Value;

    FNeedRecreatePreview := True;
    Invalidate;
  end;
end;

procedure TSVGPrintPreview.SetPrintUnits(const Value: TSVGPrintUnits);
begin
  if FPrintUnits <> Value then
  begin
    FPrintUnits := Value;

    FNeedRecreatePreview := True;
    Invalidate;
  end;
end;

procedure TSVGPrintPreview.SetRoot(const Value: ISVGRoot);
var
  R: TSVGRect;
begin
  FRoot := Value;

  if assigned(FRoot) then
  begin
    R := FRoot.CalcIntrinsicSize(SVGRect(0, 0, FPrinterPageWidth, FPrinterPageHeight));

    FCmdListWidth := Round(R.Width);
    FCmdListHeight := Round(R.Height);

    {$IFDEF SVGSupportsCmdList}

    // We render the SVG to a commandlist with the intrinsic size of the SVG
    // and will later scale the commandlist over the page(s)

    RenderToCmdList;
    {$ENDIF}
  end;

  FNeedRecreatePreview := True;
  Invalidate;
end;

end.
