unit BVE.SVGFrameRendererVCL;

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
///    Component for rendering animated SVG frame by frame
///  </summary>

interface
uses
  WinApi.Windows,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  BVE.SVG2Types,
  BVE.SVG2Intf;

type
  TSVGOnFrame = procedure(Sender: TObject; const aAnimationTime: Integer;
    const aBitmap: TBitmap) of object;

  TSVGFrameRenderer = class(TComponent)
  private
    FRoot: ISVGRoot;
    FTarget: ISVGAnimationTimerTarget;
    FBitmap: TBitmap;
    FDuration: Integer;
    FFPS: Integer;
    FDelay: Integer;
    FAnimationTime: Integer;
    FStep: Integer;
    FStepCount: Integer;
    FWidth, FHeight: Integer;
    FColor: TColor;
    FRenderOptions: TSVGRenderOptions;
    FAspectRatioMeetOrSlice: TSVGAspectRatioMeetOrSlice;
    FAutoViewbox: Boolean;
    FAspectRatioAlign: TSVGAspectRatioAlign;
    FViewPortSettings: TSVGViewPortSettings;
    FParentBounds: TSVGRect;
    FOnFrame: TSVGOnFrame;
  protected
    procedure CalcInitialViewport(aRC: ISVGRenderContext);
    procedure DoFrame; virtual;
    procedure RenderFrame(const aFirst: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function RenderFirst: Boolean;
    function RenderNext: Boolean;
    procedure RenderClose;

    property AutoViewbox: Boolean read FAutoViewbox write FAutoViewBox;
    property AspectRatioAlign: TSVGAspectRatioAlign read FAspectRatioAlign
      write FAspectRatioAlign;
    property AspectRatioMeetOrSlice: TSVGAspectRatioMeetOrSlice
      read FAspectRatioMeetOrSlice write FAspectRatioMeetOrSlice;
    property AnimationTime: Integer read FAnimationTime;
    property BackgroundColor: TColor read FColor write FColor;
    property Bitmap: TBitmap read FBitmap;
    property Delay: Integer read FDelay;
    property Duration: Integer read FDuration write FDuration;
    property FPS: Integer read FFPS write FFPS;
    property Height: Integer read FHeight write FHeight;
    property RenderOptions: TSVGRenderOptions read FRenderOptions
      write FRenderOptions;
    property SVGRoot: ISVGRoot read FRoot write FRoot;
    property Width: Integer read FWidth write FWidth;

    property OnFrame: TSVGOnFrame read FOnFrame write FOnFrame;
  end;

implementation
uses
  BVE.SVG2Elements,
  BVE.SVG2Elements.VCL;

{ TSVGFrameRenderer }

procedure TSVGFrameRenderer.CalcInitialViewport(aRC: ISVGRenderContext);
var
  SVG: ISVG;
  ViewBox: TSVGRect;
begin
  if not assigned(FRoot) then
    raise Exception.Create('Root not assigned');

  if not assigned(FRoot.SVG) then
    raise Exception.Create('Cannot find root SVG');

  SVG := FRoot.SVG;

  FParentBounds := SVGRect(0, 0, FWidth, FHeight);

  FRoot.RenderOptions := FRenderOptions;

  if FAutoViewBox then
  begin
    if SVG.ViewBox.IsUndefined then
    begin
      // Calculate the "intrinsic size" of the svg and set the viewbox
      // to 100%

      ViewBox := FRoot.CalcSizeScreenChildren(
        aRC,
        FParentBounds);

    end else
      ViewBox := SVG.ViewBox;

    FViewPortSettings := TSVGViewPortSettings.Init(
      TSVGDimension.Zero,
      TSVGDimension.Zero,
      TSVGDimension.Init(100, dimPerc),
      TSVGDimension.Init(100, dimPerc),
      ViewBox,
      TSVGPreserveAspectRatio.Init(
        False,
        FAspectRatioAlign,
        FAspectRatioMeetOrSlice));

  end else begin

    FViewPortSettings := TSVGViewPortSettings.Init(
      TSVGDimension.Zero,
      TSVGDimension.Zero,
      SVG.Width,
      SVG.Height,
      SVG.ViewBox,
      SVG.PreserveAspectRatio);
  end;

end;

constructor TSVGFrameRenderer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDuration := 0;
  FFPS := 40;
  FDelay := 0;
  FAnimationTime := 0;
  FStep := 0;
  FStepCount := 0;
  FRenderOptions := [sroClippath, sroFilters];
  FAspectRatioMeetOrSlice := TSVGAspectRatioMeetOrSlice.arMeet;
  FAutoViewbox := False;
  FAspectRatioAlign := TSVGAspectRatioAlign.arXMidYMid;
end;

destructor TSVGFrameRenderer.Destroy;
begin
  if assigned(FBitmap) then
    FBitmap.Free;

  inherited;
end;

procedure TSVGFrameRenderer.DoFrame;
begin
  if assigned(FOnFrame) then
    FOnFrame(Self, FAnimationTime, FBitmap);
end;

procedure TSVGFrameRenderer.RenderClose;
begin
  if assigned(FBitmap) then
    FreeAndNil(FBitmap);

  if not assigned(FTarget) then
    raise Exception.Create('RenderFirst was not called.');

  FTarget.DoAnimationTimerStop;
  FTarget := nil;
end;

function TSVGFrameRenderer.RenderFirst: Boolean;
begin
  if not assigned(FRoot) then
    raise Exception.Create('Root not assigned');

  if not Supports(FRoot, ISVGAnimationTimerTarget, FTarget) then
    raise Exception.Create('Root does not support animation');

  FDelay :=  MulDiv(1, 1000, FFPS);
  FAnimationTime := 0;
  FStepCount := FDuration * FFPS div 1000;
  FSTep := 0;

  // Create a bitmap

  if assigned(FBitmap) then
    FBitmap.Free;

  FBitmap := TSVGRenderContextManager.CreateCompatibleBitmap(FWidth, FHeight, False);

  // Render first frame

  FTarget.DoAnimationTimerStart;

  RenderFrame(True);

  Result := True;
end;

procedure TSVGFrameRenderer.RenderFrame(const aFirst: Boolean);
var
  RC: ISVGRenderContext;
begin
  RC := TSVGRenderContextManager.CreateRenderContextBitmap(FBitmap);

  if aFirst then
    CalcInitialViewport(RC);

  RC.BeginScene;
  try
    RC.Clear(ColorToSvgColor(FColor));

    FRoot.SVGPaint(RC, FViewPortSettings, FParentBounds);
  finally
    RC.EndScene;
  end;

  DoFrame;
end;

function TSVGFrameRenderer.RenderNext: Boolean;
begin
  if not assigned(FTarget) then
    raise Exception.Create('RenderFirst was not called.');

  if FStep < FSTepCount then
  begin
    FTarget.DoAnimationAdvanceFrame(FDelay);

    RenderFrame(False);

    Inc(FStep);
    Inc(FAnimationTime, FDelay);

    Result := True;
  end else
    Result := False;
end;

end.
