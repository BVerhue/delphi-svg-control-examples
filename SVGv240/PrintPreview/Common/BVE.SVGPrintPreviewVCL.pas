unit BVE.SVGPrintPreviewVCL;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

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
///    Print preview control
///  </summary>
///  <remarks>
///    The SVG Print Preview need at least version v2.40 update 9 of the SVG library
///  </remarks>


interface
uses
  {$IFnDEF FPC}
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
  {$ELSE}
  {$IFDEF Windows}
  Windows,
  WinUtilPrn,
  {$ENDIF}
  SysUtils,
  Classes,
  Generics.Collections,
  Graphics,
  Controls,
  ExtCtrls,
  Printers,
  {$ENDIF}
  BVE.SVG2Types,
  BVE.SVG2Intf,
  BVE.SVG2Elements,
  {$IFnDEF FPC}
  BVE.SVG2Elements.VCL
  {$ELSE}
  BVE.SVG2Elements.FPC
  {$ENDIF}
  ;

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
    FPrinterScale: TSVGPoint;

    FPagesVertical: Integer;
    FPagesHorizontal: Integer;

    FPreviewPageMargin: Integer;
    FPreviewPageWidth: Integer;
    FPreviewPageHeight: Integer;

    FNeedRecreatePreview: Boolean;

    FPrintUnits: TSVGPrintUnits;

    FMarginLeft: TSVGFloat;
    FMarginTop: TSVGFloat;
    FMarginRight: TSVGFloat;
    FMarginBottom: TSVGFloat;

    FAutoViewbox: Boolean;
    FAspectRatioAlign: TSVGAspectRatioAlign;
    FAspectRatioMeetOrSlice: TSVGAspectRatioMeetOrSlice;

    FGlueEdge: TSVGFloat;

    {$IFnDEF FPC}
    FPagePreviewList: TList<TBitmap>;
    {$ELSE}
    FPagePreviewList: TList<ISVGIntfBitmap>;
    {$ENDIF}

    procedure CalcPrinterDimensions;
    function CalcRCUnits(const aValue, aDPI: TSVGFloat): TSVGFloat;
    function CalcViewport: TSVGRect;
    function CalcViewportMatrix(const aSVGWidth, aSVGHeight: TSVGFloat): TSVGMatrix;
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
  BVE.SVG2Context;

const
{$IFnDEF FPC}
  SVGPrinterPointsPerInch = 96;
{$ELSE}
  {$IFDEF Darwin}
  SVGPrinterPointsPerInch = 72;
  {$ELSE}
  SVGPrinterPointsPerInch = 96;
  {$ENDIF}
{$ENDIF}

{ TSVGPrintPreview }

function TSVGPrintPreview.CalcRCUnits(const aValue, aDPI: TSVGFloat): TSVGFloat;
begin
  case FPrintUnits of
    puPixel:
      begin
        Result := SVG_StdDPI / aDPI * aValue;
      end;
    puMm:
      begin
        Result := SVG_StdDPI / 25.4 * aValue;
      end;
    puCm:
      begin
        Result := SVG_StdDPI / 2.54 * aValue;
      end;
    puInch:
      begin
         Result := SVG_StdDPI * aValue;
      end;
    else
      Result := aValue;
  end;
end;

procedure TSVGPrintPreview.CalcPrinterDimensions;
begin
  FPrinterPageWidth := Printer.PageWidth;
  FPrinterPageHeight := Printer.PageHeight;

  {$IFnDEF FPC}
  FDPIX := GetDeviceCaps(Printer.Handle, LOGPIXELSX);
  FDPIY := GetDeviceCaps(Printer.Handle, LOGPIXELSY);
  {$ELSE}
  FDPIX := Printer.XDPI;
  FDPIY := Printer.YDPI;
  {$ENDIF}

  if FDPIX = 0 then
    FDPIX := SVG_StdDPI;

  if FDPIY = 0 then
    FDPIY := SVG_StdDPI;

  // Convert to screen points (96)
  FPrinterPageWidth := FPrinterPageWidth * SVG_StdDPI / FDPIX;
  FPrinterPageHeight := FPrinterPageHeight * SVG_StdDPI / FDPIY;

  {$IFnDEF FPC}
  FPrinterScale := SVGPoint(SVGPrinterPointsPerInch / SVG_StdDPI, SVGPrinterPointsPerInch / SVG_StdDPI);
  {$ELSE}
  FPrinterScale := SVGPoint(SVGPrinterPointsPerInch / SVG_StdDPI, SVGPrinterPointsPerInch / SVG_StdDPI);
  {$ENDIF}
end;

function TSVGPrintPreview.CalcViewport: TSVGRect;
begin
  // Calculate the effective target area (all pages)

  Result := SVGRect(
    CalcRCUnits(FMarginLeft, FDPIX),
    CalcRCUnits(FMarginTop, FDPIY),
    PagesHorizontal * FPrinterPageWidth - CalcRCUnits(FMarginRight, FDPIX) - CalcRCUnits(FGlueEdge, FDPIX) * (PagesHorizontal - 1),
    PagesVertical * FPrinterPageHeight - CalcRCUnits(FMarginBottom, FDPIX) - CalcRCUnits(FGlueEdge, FDPIY) * (PagesVertical - 1));
end;

function TSVGPrintPreview.CalcViewportMatrix(const aSVGWidth,
   aSVGHeight: TSVGFloat): TSVGMatrix;
var
  Viewport, Viewbox: TSVGRect;
begin
  // Calculate the viewport matrix to fit the SVG in the effective target area

  if FAutoViewbox then
  begin

    // The size of the SVG

    ViewBox := SVGRect(0, 0, aSVGWidth, aSVGHeight);

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

  end;

end;

constructor TSVGPrintPreview.Create(AOwner: TComponent);
begin
  inherited;

  FRoot := nil;
  {$IFnDEF FPC}
  FPagePreviewList := TList<TBitmap>.Create;
  {$ELSE}
  FPagePreviewList := TList<ISVGIntfBitmap>.Create;
  {$ENDIF}
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
    {$IFnDEF FPC}
    FPagePreviewList[0].Free;
    {$ENDIF}
    FPagePreviewList.Delete(0);
  end;
end;

procedure TSVGPrintPreview.PagePreviewListCreate;
var
  i, Count: Integer;
  R: TSVGRect;
  Viewport: TSVGRect;
  {$IFnDEF FPC}
  Bitmap: TBitmap;
  {$ELSE}
  Bitmap: ISVGIntfBitmap;
  {$ENDIF}
  RC: ISVGRenderContext;
  M, MV: TSVGMatrix;
  SVGWidth, SVGHeight: TSVGFloat;
begin
  FNeedRecreatePreview := False;

  CalcPrinterDimensions;

  PagePreviewListClear;

  if not assigned(FRoot) then
    Exit;

  ViewPort := CalcViewport;

  R := FRoot.CalcIntrinsicSize(ViewPort);

  SVGWidth := R.Width;
  SVGHeight := R.Height;

  PagePreviewCalcSize;

  Count := PageCount;

  M := CalcViewportMatrix(SVGWidth, SVGHeight);

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

        SVGRenderToRenderContext(
          FRoot,
          RC,
          SVGWidth,
          SVGHeight,
          [sroFilters, sroClippath],
          False);

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

    {$IFnDEF FPC}
    Canvas.Draw(X, Y, FPagePreviewList[i]);
    {$ELSE}
    FPagePreviewList[i].Draw(X, Y, Canvas);
    {$ENDIF}
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
  SVGWidth, SVGHeight: TSVGFloat;
  {$IFDEF FPC}
  {$IFDEF Windows}
  PDev: TPrinterDevice;
  DevMode: PDeviceMOdeW;
  {$ENDIF}
  {$ENDIF}
begin
  CalcPrinterDimensions;

  if not assigned(FRoot) then
    Exit;

  ViewPort := CalcViewport;
  R := FRoot.CalcIntrinsicSize(ViewPort);

  SVGWidth := R.Width;
  SVGHeight := R.Height;

  Count := PageCount;

  M := CalcViewportMatrix(SVGWidth, SVGHeight);

  {$IFnDEF FPC}

  PrintJob := TSVGRenderContextManager.CreatePrintJob(
    aPrintJobName,
    TSVGBufferQuality.bqHighQuality,
    []);

  {$ELSE}{$IFDEF Windows}

  // We must supply the DevMode
  PDev := TPrinterDevice(Printer.Printers.Objects[Printer.PrinterIndex]);

  PrintJob := TSVGRenderContextManager.CreatePrintJob(
    aPrintJobName,
    PDev.DevModeW,
    TSVGBufferQuality.bqHighQuality,
    []);

  {$ELSE}

  PrintJob := TSVGRenderContextManager.CreatePrintJob(
    aPrintJobName,
    TSVGBufferQuality.bqHighQuality,
    []);

  {$ENDIF}
  {$ENDIF}

  for i := 0 to Count - 1 do
  begin
    R := PageViewbox[i];

    MV := TSVGMatrix.CreateIdentity;
    MV := TSVGMatrix.Multiply(MV, TSVGMatrix.CreateTranslation(-R.Left, -R.Top));
    MV := TSVGMatrix.Multiply(MV, TSVGMatrix.CreateScaling(FPrinterScale.X, FPrinterScale.Y));

    RC := PrintJob.BeginPage(
      FPrinterPageWidth * FPrinterScale.X,
      FPrinterPageHeight * FPrinterScale.Y);
    try
      RC.BeginScene;
      try
        //RC.Clear(SVGColorNone);

        RC.PushClipRect(
          TSVGRect.Intersect(
            TransformRect(Viewport, MV),
            TransformRect(R, MV)
            )
          );
        try
          RC.Matrix := TSVGMatrix.Multiply(RC.Matrix, M);
          RC.Matrix := TSVGMatrix.Multiply(RC.Matrix, MV);

          SVGRenderToRenderContext(
            FRoot,
            RC,
            SVGWidth,
            SVGHeight,
            [sroFilters, sroClippath],
            False);
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
begin
  FRoot := Value;

  FNeedRecreatePreview := True;
  Invalidate;
end;

end.
