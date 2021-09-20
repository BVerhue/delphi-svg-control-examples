unit UnitViewerCommon.FMX;

// ------------------------------------------------------------------------------
//
//                              SVG Control 2.0
//                       Copyright (c) 2015 Bruno Verhue
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
//    notice, this list of conditions and the following disclaimer.
// 2. Redistributions in binary form must reproduce the above copyright
//    notice, this list of conditions and the following disclaimer in the
//    documentation and/or other materials provided with the distribution.
// 3. The name of the author may not be used to endorse or promote products
//    derived from this software without specific prior written permission.
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
//
// -----------------------------------------------------------------------------

///  <summary>
///
///    This unit contains the core functionality of the "SVG Viewer demo".
///
///    Add the following paths to the project search path or library path:
///
///    1. In case of demo:
///        <Root folder>\<Delphi version>\Dcu
///
///    2. In case of full version with source code:
///        <Root folder>\Common
///        <Root folder>\Common\Fmx
///        <Root folder>\Common\Platform
///
///  </summary>

{$IFDEF ANDROID}
{$DEFINE SVGIMAGE}
{$ENDIF}
{-$DEFINE SVGLINKEDIMAGE}

{$Include 'DemoAppCompilerSettings.inc'}

interface
uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
{$IFDEF ANDROID}
  FMX.Platform.Android,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.App,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Net,
  AndroidApi.Helpers,
{$ENDIF}
  FMX.Platform,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Objects,
  FMX.Layouts,
  FMX.StdCtrls,
  FMX.ListBox,
  FMX.EditBox,
  FMX.SpinBox,
  FMX.Dialogs,
  FMX.ActnList,
  BVE.SVG2Intf,
  BVE.SVG2Types,
  BVE.SVG2Doc,
  BVE.SVG2Control.FMX,
  BVE.SVG2Image.FMX,
  BVE.SVG2ImageList.FMX;

type
  TSVGSelection = class;

  ISVGViewer = interface
    ['{50CEF706-3E81-45D1-AD79-3D8F0F0D3DFC}']
    function GetForm: TForm;
    function GetScrollbox: TScrollbox;
    function GetSelection: TSVGSelection;

    procedure SetSelection(const Value: TSVGSelection);

    procedure AlignControls;

    procedure ShowInfo(const aValue: string);

    property Form: TForm read GetForm;
    property Scrollbox: TScrollbox read GetScrollbox;
    property Selection: TSVGSelection read GetSelection write SetSelection;
  end;

  TSVGZoomBox = class(TComponent)
  private
    FForm: TForm;
    FLayoutSize: TLayout;
    FLayoutZoom: TLayout;
    FScrollbox: TScrollbox;
    FZoom: Single;
    procedure SetZoom(const Value: Single);
  public
    constructor Create(aForm: TForm; aLayoutSize, aLayoutZoom: TLayout;
      aScrollbox: TScrollbox); reintroduce;
    destructor Destroy; override;

    procedure CalcLayoutDimensions;

    property Zoom: Single read FZoom write SetZoom;
  end;

  TSVGSelection = class(TSelection)
  private
    FViewer: ISVGViewer;
    FSelected: Boolean;
    FHasEvents: Boolean;

    FAnimationTimer: TSVG2AnimationTimer;

    FLastLocation: TPointF;
    FLastDistance: Single;

    FTimerUpdate: TTimer;

{$IFDEF SVGLINKEDIMAGE}
    FSVGLinkedImage: TSVG2LinkedImage;
{$ELSE}
{$IFDEF SVGIMAGE}
    FSVGImage: TSVG2Image;
{$ELSE}
    FSVGControl: TSVG2Control;
{$ENDIF}
{$ENDIF}
    function GetAnimationFPS: TSVGFloat;
    function GetAnimationIsPaused: Boolean;
    function GetAnimationIsStarted: Boolean;
    function GetAnimationTime: Cardinal;
    function GetAspectRatioAlign: TSVGAspectRatioAlign;
    function GetAspectRatioMeetOrSlice: TSVGAspectRatioMeetOrSlice;
    function GetAutoViewbox: boolean;
    function GetFilename: string;
    function GetImageIndex: integer;
    function GetImages: TSVG2ImageList;
    function GetOnAnimationSample: TNotifyEvent;
    function GetOnSVGEvent: TSVGEvent;
    function GetOpacity: Single;
    function GetRenderOptions: TSVGRenderOptions;

    procedure SetAnimationIsPaused(const Value: Boolean);
    procedure SetAnimationIsStarted(const Value: Boolean);
    procedure SetAnimationTime(const Value: Cardinal);
    procedure SetAspectRatioAlign(const Value: TSVGAspectRatioAlign);
    procedure SetAspectRatioMeetOrSlice(
      const Value: TSVGAspectRatioMeetOrSlice);
    procedure SetAutoViewbox(const Value: boolean);
    procedure SetImageIndex(const Value: integer);
    procedure SetImages(const Value: TSVG2ImageList);
    procedure SetFileName(const Value: string);
    procedure SetOnAnimationSample(const Value: TNotifyEvent);
    procedure SetOnSVGEvent(const Value: TSVGEvent);
    procedure SetOpacity(const Value: Single);
    procedure SetSelected(const Value: boolean);
    procedure SetRenderOptions(const Value: TSVGRenderOptions);
    function GetHasAnimations: Boolean;
  protected
    function GetSVG: string;
    procedure SetSVG(const Value: string);
{$IFDEF Ver270Down}
    function GetAbsoluteRect: TRectF; override;
{$ELSE}
    function DoGetUpdateRect: TRectF; override;
{$ENDIF}
    procedure DoGesture(const EventInfo: TGestureEventInfo; var Handled: Boolean); override;
    procedure UpdateContent(Sender: TObject);
  public
    constructor Create(aViewer: ISVGViewer); reintroduce;
    destructor Destroy; override;

    function CreateCopy: TSVGSelection;
    procedure Assign(aObject: TPersistent); override;

    function GetSVGContent: string;

    procedure ContentMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure ContentMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure ContentMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure ContentGesture(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;

    property AnimationFPS: TSVGFloat read GetAnimationFPS;
    property AnimationIsStarted: Boolean read GetAnimationIsStarted write SetAnimationIsStarted;
    property AnimationIsPaused: Boolean read GetAnimationIsPaused write SetAnimationIsPaused;
    property AnimationTime: Cardinal read GetAnimationTime write SetAnimationTime;
    property AspectRatioAlign: TSVGAspectRatioAlign read GetAspectRatioAlign write SetAspectRatioAlign;
    property AspectRatioMeetOrSlice: TSVGAspectRatioMeetOrSlice read GetAspectRatioMeetOrSlice write SetAspectRatioMeetOrSlice;
    property AutoViewbox: boolean read GetAutoViewbox write SetAutoViewbox;
    property Filename: string read GetFilename write SetFileName;
    property Images: TSVG2ImageList read GetImages write SetImages;
    property ImageIndex: Integer read GetImageIndex write SetImageIndex;
    property HasAnimations: Boolean read GetHasAnimations;
    property Opacity: Single read GetOpacity write SetOpacity;
    property RenderOptions: TSVGRenderOptions read GetRenderOptions write SetRenderOptions;
    property Selected: boolean read FSelected write SetSelected;
    property SVG: string read GetSVG write SetSVG;

    property OnAnimationSample: TNotifyEvent read GetOnAnimationSample write SetOnAnimationSample;
    property OnSVGEvent: TSVGEvent read GetOnSVGEvent write SetOnSVGEvent;
  end;

  TSVGViewerInfoForm = class;

  TSVGViewerForm = class(TForm, ISVGViewer)
  private
{$IFDEF ANDROID}
    const
      FileRequestCode = 0;
    var
      FMessageSubscriptionID: Integer;
      FFileName: string;
{$ENDIF}

    FTimerZoom: TTimer;
    FTimerAndroidLoad: TTimer;
    FSelection: TSVGSelection;

    FActionAddOk: TAction;
    FActionAddCancel: TAction;
    FActionAdd: TAction;
    FActionCopy: TAction;
    FActionCopyDirect: TAction;
    FActionInfo: TAction;
    FActionLoad: TAction;
    FActionPaste: TAction;
    FActionRemove: TAction;
    FActionZoomReset: TAction;
    FActionZoomDec: TAction;
    FActionZoomInc: TAction;

    FActionAnimationPause: TButton;
    FActionAnimationStart: TButton;

    FAnimationTrackBar: TTrackBar;
    FLabelTime: TLabel;

    FCheckboxAutoViewbox: TCheckBox;
    FCheckboxClippath: TCheckBox;
    FCheckboxFilters: TCheckBox;
    FCheckboxMouseEvents: TCheckBox;

    FComboboxAspectRatio: TComboBox;
    FComboboxMeetOrSlice: TComboBox;

    FSpinboxRotate: TSpinBox;
    FSpinboxOpacity: TSpinBox;

    FScrollbox: TScrollbox;
    FLayoutSide: TLayout;
    FLayoutSize: TLayout;
    FLayoutZoom: TLayout;

    FListbox: TListbox;

    FImageList1: TSVG2ImageList;
    FImageList2: TSVG2ImageList;
    FLabelInfo: TLabel;
    FOpenDialog: TOpenDialog;
    FZoomTrackBar: TTrackBar;

    FZoomBox: TSVGZoomBox;

    FFormInfo: TSVGViewerInfoForm;
  protected
    function GetForm: TForm;
    function GetScrollbox: TScrollbox;
    function GetSelection: TSVGSelection;
    procedure SetSelection(const Value: TSVGSelection);
    procedure SetFormInfo(const Value: TSVGViewerInfoForm);

    procedure ActionAddOkExecute(Sender: TObject);
    procedure ActionAddCancelExecute(Sender: TObject);
    procedure ActionAddExecute(Sender: TObject);
    procedure ActionCopyExecute(Sender: TObject);
    procedure ActionCopyDirectExecute(Sender: TObject);
    procedure ActionInfoExecute(Sender: TObject);
    procedure ActionLoadExecute(Sender: TObject);
    procedure ActionPasteExecute(Sender: TObject);
    procedure ActionRemoveExecute(Sender: TObject);
    procedure ActionZoomResetExecute(Sender: TObject);
    procedure ActionZoomDecExecute(Sender: TObject);
    procedure ActionZoomIncExecute(Sender: TObject);
    procedure ActionAnimationStartExecute(Sender: TObject);
    procedure ActionAnimationPauseExecute(Sender: TObject);

    procedure ScrollBoxDragOver(Sender: TObject; const Data: TDragObject;
      const Point: TPointF; var Operation: TDragOperation);
    procedure ScrollBoxDragDrop(Sender: TObject; const Data: TDragObject;
      const Point: TPointF);

    procedure CheckboxAutoViewboxChange(Sender: TObject);
    procedure CheckboxClippathChange(Sender: TObject);
    procedure CheckboxFiltersChange(Sender: TObject);
    procedure CheckboxMouseEventsChange(Sender: TObject);
    procedure ComboboxAspectRatioChange(Sender: TObject);
    procedure ComboboxMeetOrSliceChange(Sender: TObject);
    procedure SpinboxRotateChange(Sender: TObject);
    procedure SpinboxOpacityChange(Sender: TObject);
    procedure TimerAndroidLoadTimer(Sender: TObject);
    procedure TimerZoomTimer(Sender: TObject);
    procedure TrackBarChange(Sender: TObject);

    procedure AnimationTrackbarChange(aSender: TObject);
    procedure DoTimer(aSender: TObject);
    procedure DoShow; override;
    procedure SVGEvent(Sender: TObject; aSVGRoot: ISVGRoot; aEvent: ISVGEvent;
      const aValue: string);


    procedure AlignControls;

    procedure Resize; override;

    procedure ShowInfo(const aValue: string);

    {$IFDEF ANDROID}
    function LaunchActivityForResult(const Intent: JIntent;
      RequestCode: Integer): Boolean;
    procedure LaunchLoadFile;
    procedure HandleActivityMessage(const Sender: TObject; const M: TMessage);
    function OnActivityResult(RequestCode, ResultCode: Integer; Data: JIntent): Boolean;
    {$ENDIF}
  public
    constructor Create(AOwnder: TComponent); override;
    destructor Destroy; override;

    procedure ConnectControls(
      aActionAddOk: TAction;
      aActionAddCancel: TAction;
      aActionAdd: TAction;
      aActionCopy: TAction;
      aActionCopyDirect: TAction;
      aActionInfo: TAction;
      aActionLoad: TAction;
      aActionPaste: TAction;
      aActionRemove: TAction;
      aActionZoomReset: TAction;
      aActionZoomDec: TAction;
      aActionZoomInc: TAction;
      aActionAnimationStart: TButton;
      aActionAnimationPause: TButton;
      aCheckboxAutoViewbox: TCheckBox;
      aCheckboxClippath: TCheckBox;
      aCheckboxFilters: TCheckBox;
      aCheckboxMouseEvents: TCheckBox;
      aComboboxAspectRatio: TComboBox;
      aComboboxMeetOrSlice: TComboBox;
      aSpinboxRotate: TSpinBox;
      aSpinboxOpacity: TSpinBox;
      aScrollbox: TScrollbox;
      aLayoutSide: TLayout;
      aLayoutSize: TLayout;
      aLayoutZoom: TLayout;
      aListbox: TListbox;
      aImageList1: TSVG2ImageList;
      aImageList2: TSVG2ImageList;
      aOpenDialog: TOpenDialog;
      aZoomTrackBar: TTrackBar;
      aLabelInfo: TLabel;
      aAnimationTrackbar: TTrackbar;
      aLabelTime: TLabel);

    procedure UpdateControls;

    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;

    function AddSVG(const aFilename: string): Boolean;
    procedure AddSVGSelection;

    property Form: TForm read GetForm;
    property FormInfo: TSVGViewerInfoForm read FFormInfo write SetFormInfo;
    property Scrollbox: TScrollbox read GetScrollbox;
    property Selection: TSVGSelection read GetSelection write SetSelection;
  end;

  TSVGViewerInfoForm = class(TForm)
  private
    FButton: TButton;
    FSVGImage: TSVG2Image;
  protected
    procedure ButtonClick(Sender: TObject);
    procedure SVGEvent(Sender: TObject; aSVGRoot: ISVGRoot; aEvent: ISVGEvent; const aValue: string);
  public
    constructor Create(AOwnder: TComponent); override;
    destructor Destroy; override;

    procedure ConnectControls(aButton: TButton; aSVGImage: TSVG2Image);
    procedure Open(const FilePath: string);
  end;

  function MaxZoom(aControl: TFmxObject): Single;

implementation
uses
  System.Math,
  System.Rtti,
  System.IOUtils,
{$IFDEF MSWINDOWS}
  Winapi.ShellAPI,
  Winapi.Windows,
{$ENDIF MSWINDOWS}
{$IFDEF MACOS}
  Posix.Stdlib,
{$ENDIF MACOS}
{$IFDEF ANDROID}
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Net,
  Androidapi.JNI.App,
  Androidapi.helpers,
{$ENDIF}
{$IFDEF IOS}
  macapi.helpers,
  iOSapi.Foundation,
  FMX.helpers.iOS,
{$ENDIF}
  BVE.SVG2GeomUtility;

function ChildrenRect(aControl: TFmxObject): TRectF;
var
  i: integer;
  Control: TControl;
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
      Control := (aControl.Children[i] as TControl);
      if Control.Position.X < Result.Left then
        Result.Left := Control.Position.X;
      if Control.Position.Y < Result.Top then
        Result.Top := Control.Position.Y;
      if Control.Position.X + Control.Width > Result.Right then
        Result.Right := Control.Position.X + Control.Width;
      if Control.Position.Y + Control.Height > Result.Bottom then
        Result.Bottom := Control.Position.Y + Control.Height;
    end;
  end;
end;


function ChildrenMaxSize(aControl: TFmxObject): TRectF;

var

  i: integer;
  Control: TControl;
begin
  // Calc max size of a parent control
  Result.Left := 0;
  Result.Right := 0;
  Result.Top := 0;
  Result.Bottom := 0;
  for i := 0 to aControl.ChildrenCount - 1 do
  begin
    if aControl.Children[i] is TControl then
    begin
      Control := (aControl.Children[i] as TControl);
      if (Control.Width * Control.Height) > (Result.Width * Result.Height) then
        Result := Control.BoundsRect;
    end;
  end;
end;


function MaxZoom(aControl: TFmxObject): Single;

var
  R: TRectF;
  Size, MaxSize: integer;
begin
  R := ChildrenMaxSize(aControl);

  if (R.Width <> 0) and (R.Height <> 0) then
  begin
    Size := Max(Round(R.Width), Round(R.Height));

    MaxSize := TCanvasManager.DefaultCanvas.GetAttribute(
      TCanvasAttribute.MaxBitmapSize);

    Result := MaxSize / Size;
  end else
    Result := 10;

  if Result > 10 then
    Result := 10;
end;

// -----------------------------------------------------------------------------

//
//                            TSVGSelection
//
// -----------------------------------------------------------------------------

procedure TSVGSelection.Assign(aObject: TPersistent);
var
  Srce: TSVGSelection;
begin
  if aObject is TSVGSelection then
  begin
    Srce := (aObject as TSVGSelection);

    BeginUpdate;
    try
      Size := Srce.Size;
      Scale := Srce.Scale;
      RotationAngle := Srce.RotationAngle;
      Position := Srce.Position;

      Position.X := Position.X + 10;
      Position.Y := Position.Y + 10;

{$IFDEF SVGLINKEDIMAGE}
      FSVGLinkedImage.Assign(Srce.FSVGLinkedImage);
{$ELSE}
{$IFDEF SVGIMAGE}
      FSVGImage.Assign(Srce.FSVGImage);
{$ELSE}
      FSVGControl.Assign(Srce.FSVGControl);
{$ENDIF}
{$ENDIF}
    finally
      Endupdate;
    end;
  end else
    inherited;
end;

procedure TSVGSelection.ContentGesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  DoGesture(EventInfo, Handled);
end;

procedure TSVGSelection.ContentMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  MouseDown(Button, Shift, X, Y);
end;

procedure TSVGSelection.ContentMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
  MouseMove(Shift, X, Y);
end;

procedure TSVGSelection.ContentMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  MouseUp(Button, Shift, X, Y);
end;

constructor TSVGSelection.Create(aViewer: ISVGViewer);
begin
  inherited Create(aViewer.Form);

  FViewer := aViewer;

  FAnimationTimer := TSVG2AnimationTimer.Create(nil);

{$IFDEF SVGLINKEDIMAGE}
  FSVGLinkedImage := TSVG2LinkedImage.Create(Self);
  FSVGLinkedImage.Parent := Self;
  FSVGLinkedImage.HitTest := False;
  FSVGLinkedImage.Position.X := 0.0;
  FSVGLinkedImage.Position.Y := 0.0;
  FSVGLinkedImage.Scale.X := 1.0;
  FSVGLinkedImage.Scale.Y := 1.0;
  FSVGLinkedImage.AnimationTimer := FAnimationTimer;
  FSVGLinkedImage.OnMouseDown := ContentMouseDown;
  FSVGLinkedImage.OnMouseMove := ContentMouseMove;
  FSVGLinkedImage.OnMouseUp := ContentMouseUp;
  FSVGLinkedImage.OnGesture := ContentGesture;
{$ELSE}
{$IFDEF SVGIMAGE}
  FSVGImage := TSVG2Image.Create(Self);
  FSVGImage.Parent := Self;
  FSVGImage.HitTest := False;
  FSVGImage.Position.X := 0.0;
  FSVGImage.Position.Y := 0.0;
  FSVGImage.AutoViewbox := True;
  FSVGImage.AnimationTimer := FAnimationTimer;
  FSVGImage.OnMouseDown := ContentMouseDown;
  FSVGImage.OnMouseMove := ContentMouseMove;
  FSVGImage.OnMouseUp := ContentMouseUp;
  FSVGImage.OnGesture := ContentGesture;
{$ELSE}
  FSVGControl := TSVG2Control.Create(Self);
  FSVGControl.Parent := Self;
  FSVGControl.HitTest := True;
  FSVGControl.Position.X := 0.0;
  FSVGControl.Position.Y := 0.0;
  FSVGControl.AutoViewbox := True;
  FSVGControl.RenderOptions := [sroFilters, sroClippath, sroEvents];
  FSVGControl.Opacity := 1.0;
  FSVGControl.AnimationTimer := FAnimationTimer;
  FSVGControl.OnMouseDown := ContentMouseDown;
  FSVGControl.OnMouseMove := ContentMouseMove;
  FSVGControl.OnMouseUp := ContentMouseUp;
  FSVGControl.OnGesture := ContentGesture;
{$ENDIF}
{$ENDIF}

  Touch.InteractiveGestures := [TInteractiveGesture.Zoom, TInteractiveGesture.Pan];

  Width := 105;
  Height := 105;

  FLastLocation := PointF(0, 0);
  FLastDistance := 0;

  FTimerUpdate := TTimer.Create(Self);
  FTimerUpdate.Enabled := False;
  FTimerUpdate.Interval := 200;
  FTimerUpdate.OnTimer := UpdateContent;

  FViewer.AlignControls;
end;

function TSVGSelection.CreateCopy: TSVGSelection;
begin
  Result := TSVGSelection.Create(FViewer);
  Result.Assign(Self);
end;

destructor TSVGSelection.Destroy;
begin
  FAnimationTimer.Free;
  inherited;
end;

procedure TSVGSelection.DoGesture(const EventInfo: TGestureEventInfo;
  var Handled: Boolean);
var
  D, L, Vpx, Vpy: Single;
  R: TRectF;
begin
  if EventInfo.GestureID = igiZoom then
  begin

    if (TInteractiveGestureFlag.gfBegin in EventInfo.Flags) then
    begin
      if assigned(FViewer.Scrollbox) then
        FViewer.Scrollbox.AniCalculations.TouchTracking := [];
    end;

    if (not(TInteractiveGestureFlag.gfBegin in EventInfo.Flags))
    and (not(TInteractiveGestureFlag.gfEnd in EventInfo.Flags)) then
    begin
      BeginUpdate;
      try
        D := (EventInfo.Distance - FLastDistance);
        Vpx := EventInfo.Location.X - FLastLocation.X;
        Vpy := EventInfo.Location.Y - FLastLocation.Y;
        //Text1.Text := Format('%0:3.1f %1:3.1f %2:3d', [ Vpx, Vpy, EventInfo.Distance]);
        R.Left := Position.X;
        R.Top := Position.Y;
        R.Width := Width;
        R.Height := Height;
        L := 2 * Sqrt(R.Width * R.Width + R.Height * R.Height);
        R.Left := R.Left - (R.Right - Vpx) / L * D;
        R.Top := R.Top - (R.Bottom - Vpy) / L * D;
        R.Right := R.Right + (R.Right - Vpx) / L * D;
        R.Bottom := R.Bottom + (R.Bottom - Vpy) / L * D;
        if (R.Width < 10) or (R.Height < 10) then
          Exit;
        Position.X := R.Left;
        Position.Y := R.Top;
        Width := R.Width;
        Height := R.Height;
        FTimerUpdate.Enabled := False;
      finally
        EndUpdate;
      end;
      Repaint;
    end;
    FLastLocation := EventInfo.Location;
    FLastDistance := EventInfo.Distance;
    if (TInteractiveGestureFlag.gfEnd in EventInfo.Flags) then
    begin
      if assigned(FViewer.Scrollbox) then
        FViewer.Scrollbox.AniCalculations.TouchTracking := [ttVertical, ttHorizontal];

      FTimerUpdate.Enabled := True;
    end;
  end;
  if EventInfo.GestureID = igiPan then
  begin
    if (not(TInteractiveGestureFlag.gfBegin in EventInfo.Flags))
    and (not(TInteractiveGestureFlag.gfEnd in EventInfo.Flags)) then
    begin
      BeginUpdate;
      try
        Position.X := Position.X + (EventInfo.Location.X - FLastLocation.X);
        Position.Y := Position.Y + (EventInfo.Location.Y - FLastLocation.Y);
      finally
        EndUpdate;
      end;
      Repaint;
    end;
    FLastLocation := EventInfo.Location;
  end;
end;


{$IFDEF Ver270Down}
function TSVGSelection.GetAbsoluteRect: TRectF;
{$ELSE}
function TSVGSelection.DoGetUpdateRect: TRectF;
{$ENDIF}
begin
  // Bug in Delphi Scale should be AbsoluteScale

  Result := inherited;
  Result.Inflate((GripSize + 1) * AbsoluteScale.X, (GripSize + 1) * AbsoluteScale.Y);
end;

function TSVGSelection.GetAnimationFPS: TSVGFloat;
begin
  Result := FAnimationTimer.FPS;
end;

function TSVGSelection.GetAnimationIsPaused: Boolean;
begin
  Result := FAnimationTimer.IsPaused;
end;

function TSVGSelection.GetAnimationIsStarted: Boolean;
begin
  Result := FAnimationTimer.IsStarted;
end;

function TSVGSelection.GetAnimationTime: Cardinal;
begin
{$IFDEF SVGLINKEDIMAGE}
  Result := FSVGLinkedImage.AnimationTime;
{$ELSE}
{$IFDEF SVGIMAGE}
  Result := FSVGImage.AnimationTime;
{$ELSE}
  Result := FSVGControl.AnimationTime;
{$ENDIF}
{$ENDIF}
end;

function TSVGSelection.GetAspectRatioAlign: TSVGAspectRatioAlign;
begin
{$IFDEF SVGLINKEDIMAGE}
  Result := FSVGLinkedImage.AspectRatioAlign;
{$ELSE}
{$IFDEF SVGIMAGE}
  Result := FSVGImage.AspectRatioAlign;
{$ELSE}
  Result := FSVGControl.AspectRatioAlign;
{$ENDIF}
{$ENDIF}
end;

function TSVGSelection.GetAspectRatioMeetOrSlice: TSVGAspectRatioMeetOrSlice;
begin
{$IFDEF SVGLINKEDIMAGE}
  Result := FSVGLinkedImage.AspectRatioMeetOrSlice;
{$ELSE}
{$IFDEF SVGIMAGE}
  Result := FSVGImage.AspectRatioMeetOrSlice;
{$ELSE}
  Result := FSVGControl.AspectRatioMeetOrSlice;
{$ENDIF}
{$ENDIF}
end;

function TSVGSelection.GetAutoViewbox: boolean;
begin
{$IFDEF SVGLINKEDIMAGE}
  Result := FSVGLinkedImage.AutoViewbox;
{$ELSE}
{$IFDEF SVGIMAGE}
  Result := FSVGImage.AutoViewbox;
{$ELSE}
  Result := FSVGControl.AutoViewbox;
{$ENDIF}
{$ENDIF}
end;

function TSVGSelection.GetFilename: string;
begin
{$IFDEF SVGLINKEDIMAGE}
  Result := '';
{$ELSE}
{$IFDEF SVGIMAGE}
  Result := FSVGImage.Filename;
{$ELSE}
  Result := FSVGControl.Filename;
{$ENDIF}
{$ENDIF}
end;

function TSVGSelection.GetHasAnimations: Boolean;
begin
{$IFDEF SVGLINKEDIMAGE}
  Result := FSVGLinkedImage.HasAnimations;
{$ELSE}
{$IFDEF SVGIMAGE}
  Result := FSVGImage.HasAnimations;
{$ELSE}
  Result := FSVGControl.HasAnimations;
{$ENDIF}
{$ENDIF}
end;

function TSVGSelection.GetImageIndex: integer;
begin
{$IFDEF SVGLINKEDIMAGE}
  Result := FSVGLinkedImage.ImageIndex;
{$ELSE}
{$IFDEF SVGIMAGE}
  Result := 0;
{$ELSE}
  Result := 0;
{$ENDIF}
{$ENDIF}
end;

function TSVGSelection.GetImages: TSVG2ImageList;
begin
{$IFDEF SVGLINKEDIMAGE}
  Result := FSVGLinkedImage.Images as TSVG2ImageList;
{$ELSE}
{$IFDEF SVGIMAGE}
  Result := nil;
{$ELSE}
  Result := nil;
{$ENDIF}
{$ENDIF}
end;

function TSVGSelection.GetOnAnimationSample: TNotifyEvent;
begin
  Result := FAnimationTimer.OnSample;
end;

function TSVGSelection.GetOnSVGEvent: TSVGEvent;
begin
{$IFDEF SVGLINKEDIMAGE}
  Result := FSVGLinkedImage.OnSVGEvent;
{$ELSE}
{$IFDEF SVGIMAGE}
  Result := FSVGImage.OnSVGEvent;
{$ELSE}
  Result := FSVGControl.OnSVGEvent;
{$ENDIF}
{$ENDIF}
end;

function TSVGSelection.GetOpacity: Single;
begin
{$IFDEF SVGLINKEDIMAGE}
  Result := FSVGLinkedImage.Opacity;
{$ELSE}
{$IFDEF SVGIMAGE}
  Result := FSVGImage.Opacity;
{$ELSE}
  Result := FSVGControl.Opacity;
{$ENDIF}
{$ENDIF}
end;

function TSVGSelection.GetRenderOptions: TSVGRenderOptions;
begin
{$IFDEF SVGLINKEDIMAGE}
  Result := FSVGLinkedImage.RenderOptions;
{$ELSE}
{$IFDEF SVGIMAGE}
  Result := FSVGImage.RenderOptions;
{$ELSE}
  Result := FSVGControl.RenderOptions;
{$ENDIF}
{$ENDIF}
end;

function TSVGSelection.GetSVG: string;
begin
{$IFDEF SVGLINKEDIMAGE}
  Result := '';
{$ELSE}
{$IFDEF SVGIMAGE}
  Result := FSVGImage.SVG.Text;
{$ELSE}
  Result := FSVGControl.SVG.Text;
{$ENDIF}
{$ENDIF}
end;

function TSVGSelection.GetSVGContent: string;
var
  HasSVG: boolean;
  sl: TStringList;
  StrStream: TStringStream;
begin
{$IFDEF SVGLINKEDIMAGE}
  HasSVG := False;
{$ELSE}
{$IFDEF SVGIMAGE}
  HasSVG := FSVGImage.SVG.Count > 0;
{$ELSE}
  HasSVG := FSVGControl.SVG.Count > 0;
{$ENDIF}
{$ENDIF}
  if HasSVG then
  begin
    Result := SVG;
  end else
    if Filename <> '' then
    begin
      sl := TStringList.Create;
      try
        sl.LoadFromFile(FileName);
        Result := sl.Text;
      finally
        sl.Free;
      end;
    end else begin
      if assigned(Images) and (ImageIndex >= 0) and (ImageIndex < Images.Count) then
      begin
        StrStream := TStringStream.Create;
        try
          Images.SaveSVGToStream(ImageIndex, StrStream);
          Result := StrStream.DataString;
        finally
          StrStream.Free;
        end;
      end;
    end;
end;

procedure TSVGSelection.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  if assigned(FViewer.Scrollbox) then
    FViewer.Scrollbox.AniCalculations.TouchTracking := [];

  FViewer.Selection := Self;

  inherited;
end;

procedure TSVGSelection.MouseMove(Shift: TShiftState; X, Y: Single);
var
  Obj: ISVGObject;
begin
  if ssTouch in Shift then
    Exit;

  inherited;

  if (not FHasEvents) and (sroEvents in RenderOptions) then
  begin
{$IFDEF SVGLINKEDIMAGE}
    Obj := FSVGLinkedImage.ObjectAtPt(PointF(X, Y), False);
{$ELSE}
{$IFDEF SVGIMAGE}
    Obj := FSVGImage.ObjectAtPt(PointF(X, Y), False);
{$ELSE}
    Obj := FSVGControl.ObjectAtPt(PointF(X, Y), False);
    FSVGControl.SVGRoot.CheckEventTypes;
{$ENDIF}
{$ENDIF}
    if assigned(Obj) then
      FViewer.ShowInfo(Obj.ID)
    else
      FViewer.ShowInfo('No object');
  end;
end;

procedure TSVGSelection.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

  if assigned(FViewer.Scrollbox) then
    FViewer.Scrollbox.AniCalculations.TouchTracking := [ttVertical, ttHorizontal];

  FTimerUpdate.Enabled := True;
end;

procedure TSVGSelection.SetAnimationIsPaused(const Value: Boolean);
begin
  FAnimationTimer.IsPaused := Value;
end;

procedure TSVGSelection.SetAnimationIsStarted(const Value: Boolean);
begin
  FAnimationTimer.IsStarted := Value;
end;

procedure TSVGSelection.SetAnimationTime(const Value: Cardinal);
begin
{$IFDEF SVGLINKEDIMAGE}
  FSVGLinkedImage.AnimationTime := Value;
{$ELSE}
{$IFDEF SVGIMAGE}
  FSVGImage.AnimationTime := Value;
{$ELSE}
  FSVGControl.AnimationTime := Value;
{$ENDIF}
{$ENDIF}
end;

procedure TSVGSelection.SetAspectRatioAlign(const Value: TSVGAspectRatioAlign);
begin
{$IFDEF SVGLINKEDIMAGE}
  FSVGLinkedImage.AspectRatioAlign := Value;
{$ELSE}
{$IFDEF SVGIMAGE}
  FSVGImage.AspectRatioAlign := Value;
{$ELSE}
  FSVGControl.AspectRatioAlign := Value;
{$ENDIF}
{$ENDIF}
end;

procedure TSVGSelection.SetAspectRatioMeetOrSlice(
  const Value: TSVGAspectRatioMeetOrSlice);
begin
{$IFDEF SVGLINKEDIMAGE}
  FSVGLinkedImage.AspectRatioMeetOrSlice := Value;
{$ELSE}
{$IFDEF SVGIMAGE}
  FSVGImage.AspectRatioMeetOrSlice := Value;
{$ELSE}
  FSVGControl.AspectRatioMeetOrSlice := Value;
{$ENDIF}
{$ENDIF}
end;

procedure TSVGSelection.SetAutoViewbox(const Value: boolean);
begin
{$IFDEF SVGLINKEDIMAGE}
  FSVGLinkedImage.AutoViewbox := Value;
{$ELSE}
{$IFDEF SVGIMAGE}
  FSVGImage.AutoViewbox := Value;
{$ELSE}
  FSVGControl.AutoViewbox := Value;
{$ENDIF}
{$ENDIF}
end;

procedure TSVGSelection.SetFileName(const Value: string);
begin
{$IFDEF SVGLINKEDIMAGE}
{$ELSE}
{$IFDEF SVGIMAGE}
  FSVGImage.Filename := Value;

  FSVGImage.ParseSVG;
  FSVGImage.CalcSize;

  Width := FSVGImage.Width;
  Height := FSVGImage.Height;

  FViewer.AlignControls;

  FHasEvents := (FSVGImage.SVGRoot.EventTypeSet <> []);
{$ELSE}
  FSVGControl.Filename := Value;

  FSVGControl.ParseSVG;
  FSVGControl.CalcSize;

  Width := FSVGControl.Width;
  Height := FSVGControl.Height;

  FViewer.AlignControls;

  FHasEvents := (FSVGControl.SVGRoot.EventTypeSet <> []);
{$ENDIF}
{$ENDIF}
end;

procedure TSVGSelection.SetImageIndex(const Value: integer);
begin
{$IFDEF SVGLINKEDIMAGE}
  FSVGLinkedImage.ImageIndex := Value;
{$ELSE}
{$IFDEF SVGIMAGE}
{$ELSE}
{$ENDIF}
{$ENDIF}
end;

procedure TSVGSelection.SetImages(const Value: TSVG2ImageList);
begin
{$IFDEF SVGLINKEDIMAGE}
  FSVGLinkedImage.Images := Value;
{$ELSE}
{$IFDEF SVGIMAGE}
{$ELSE}
{$ENDIF}
{$ENDIF}
end;

procedure TSVGSelection.SetOnAnimationSample(const Value: TNotifyEvent);
begin
  FAnimationTimer.OnSample := Value;
end;

procedure TSVGSelection.SetOnSVGEvent(const Value: TSVGEvent);
begin
{$IFDEF SVGLINKEDIMAGE}
  FSVGLinkedImage.OnSVGEvent := Value;;
{$ELSE}
{$IFDEF SVGIMAGE}
  FSVGImage.OnSVGEvent := Value;;
{$ELSE}
  FSVGControl.OnSVGEvent := Value;
{$ENDIF}
{$ENDIF}
end;

procedure TSVGSelection.SetOpacity(const Value: Single);
begin
{$IFDEF SVGLINKEDIMAGE}
  FSVGLinkedImage.Opacity := Value;
{$ELSE}
{$IFDEF SVGIMAGE}
  FSVGImage.Opacity := Value;
{$ELSE}
  FSVGControl.Opacity := Value;
{$ENDIF}
{$ENDIF}
end;

procedure TSVGSelection.SetRenderOptions(const Value: TSVGRenderOptions);
begin
{$IFDEF SVGLINKEDIMAGE}
  FSVGLinkedImage.RenderOptions := Value;
{$ELSE}
{$IFDEF SVGIMAGE}
  FSVGImage.RenderOptions := Value;
{$ELSE}
  FSVGControl.RenderOptions := Value;
{$ENDIF}
{$ENDIF}
end;

procedure TSVGSelection.SetSelected(const Value: boolean);
begin
  FSelected := Value;

  HideSelection := not FSelected;
end;

procedure TSVGSelection.SetSVG(const Value: string);
begin
{$IFDEF SVGLINKEDIMAGE}
{$ELSE}
{$IFDEF SVGIMAGE}
  FSVGImage.SVG.Text := Value;

  FSVGImage.ParseSVG;
  FSVGImage.CalcSize;

  Width := FSVGImage.Width;
  Height := FSVGImage.Height;

  FViewer.AlignControls;

  FHasEvents := (FSVGImage.SVGRoot.EventTypeSet <> []);
  FSVGImage.Repaint;
{$ELSE}
  FSVGControl.SVG.Text := Value;

  FSVGControl.ParseSVG;
  FSVGControl.CalcSize;

  Width := FSVGControl.Width;
  Height := FSVGControl.Height;

  FViewer.AlignControls;

  FHasEvents := (FSVGControl.SVGRoot.EventTypeSet <> []);
  FSVGControl.Repaint;
{$ENDIF}
{$ENDIF}
end;

procedure TSVGSelection.UpdateContent(Sender: TObject);
begin
 FTimerUpdate.Enabled := False;

{$IFDEF SVGLINKEDIMAGE}
  FSVGLinkedImage.SetBounds(0, 0, Width, Height);
{$ELSE}
{$IFDEF SVGIMAGE}
  FSVGImage.SetBounds(0, 0, Width, Height);
{$ELSE}
  FSVGControl.SetBounds(0, 0, Width, Height);
{$ENDIF}
{$ENDIF}
end;

// -----------------------------------------------------------------------------
//
//                             TSVGZoomBox
//
// -----------------------------------------------------------------------------

constructor TSVGZoomBox.Create(aForm: TForm; aLayoutSize, aLayoutZoom: TLayout;
  aScrollbox: TScrollbox);
begin
  inherited Create(aForm);

  FForm := aForm;
  FLayoutSize := aLayoutSize;
  FLayoutZoom := aLayoutZoom;
  FScrollbox := aScrollbox;
  FZoom := 1.0;
end;

destructor TSVGZoomBox.Destroy;
begin
  inherited;
end;

procedure TSVGZoomBox.CalcLayoutDimensions;
var
  CR, WR, R: TRectF;
  Z: Single;
begin
  // Calc max size of lZoom layout
  CR := ChildrenRect(FLayoutZoom);
  CR.Right := CR.Right * 2;
  CR.Bottom := CR.Bottom * 2;
  if FZoom < 0 then
    Z := 1.2 / FZoom
  else
    Z := 1.2;
  WR := RectF(
    0.0, 0.0,
    FScrollbox.Width * Z,
    FScrollbox.Height * Z);
  R := MaxRect(CR, WR);
  FLayoutZoom.Position.X := 0.0;
  FLayoutZoom.Position.Y := 0.0;
  FLayoutZoom.Width := R.Right;
  FLayoutZoom.Height := R.Bottom;
  FLayoutSize.Position.X := 0.0;
  FLayoutSize.Position.Y := 0.0;
  FLayoutSize.Width := FLayoutZoom.Width * FZoom;
  FLayoutSize.Height := FLayoutZoom.Height * FZoom;
end;

procedure TSVGZoomBox.SetZoom(const Value: Single);
var
  Mx, My: Single;
begin
  if FZoom <> Value then
  begin
    // Calc middle of currently visible rectangle of scrollbox

    Mx := (FScrollbox.ViewportPosition.X + FScrollbox.Width / 2) / FLayoutSize.Width;
    My := (FScrollbox.ViewportPosition.Y + FScrollbox.Height / 2) / FLayoutSize.Height;
    FZoom := Value;
    // Set zoom of lZoom layout
    FScrollbox.BeginUpdate;
    try
      FLayoutZoom.Scale.X := FZoom;
      FLayoutZoom.Scale.Y := FZoom;
      CalcLayoutDimensions;
      FScrollbox.ViewportPosition := PointF(
        Mx * FLayoutSize.Width - (FScrollbox.Width / 2),
        My * FLayoutSize.Height - (FScrollbox.Height / 2));

    finally
      FScrollbox.EndUpdate;
      FScrollbox.Repaint;
    end;
  end;
end;

// -----------------------------------------------------------------------------
//
//                            TSVGViewerForm
//
// -----------------------------------------------------------------------------

procedure TSVGViewerForm.ActionAddCancelExecute(Sender: TObject);
begin
  FLayoutSide.Visible := False;
end;

procedure TSVGViewerForm.ActionAddExecute(Sender: TObject);
begin
  FLayoutSide.Visible := False;
  AddSVGSelection;
end;

procedure TSVGViewerForm.ActionAddOkExecute(Sender: TObject);
begin
  //
end;

procedure TSVGViewerForm.ActionAnimationPauseExecute(Sender: TObject);
begin
  if assigned(FSelection) then
    FSelection.AnimationIsPaused := FActionAnimationPause.IsPressed;

  UpdateControls;
end;

procedure TSVGViewerForm.ActionAnimationStartExecute(Sender: TObject);
begin
  if assigned(FSelection) then
    FSelection.AnimationIsStarted := FActionAnimationStart.IsPressed;

  UpdateControls;
end;

procedure TSVGViewerForm.ActionCopyDirectExecute(Sender: TObject);
begin
  if assigned(Selection) then
  begin
    Selection := Selection.CreateCopy;
    Selection.Parent := FlayoutZoom;
    Selection.Images := FImageList2;
    Selection.ImageIndex := FListBox.ItemIndex;
    Selection.OnSVGEvent := SVGEvent;
  end;
end;

procedure TSVGViewerForm.ActionCopyExecute(Sender: TObject);
var
  Svc: IFMXClipboardService;
begin
  if assigned(Selection) then
  begin
    if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, Svc) then
      Svc.SetClipboard(Selection.GetSVGContent);
  end;
end;

procedure TSVGViewerForm.ActionInfoExecute(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  FFormInfo.ShowModal;
{$ENDIF}

{$IFDEF MACOS}
  FFormInfo.ShowModal;
{$ENDIF}

{$IFDEF IOS}
  FFormInfo.Show;
{$ENDIF}

{$IFDEF ANDROID}
  FFormInfo.Show;
{$ENDIF}
end;

procedure TSVGViewerForm.ActionLoadExecute(Sender: TObject);
{$IFDEF IOS}
var
  sr: TSearchRec;
{$ENDIF}
begin
{$IFDEF ANDROID}
  LaunchLoadFile
{$ELSE}
{$IFDEF IOS}

  // Load all files in the documents folder, these need to be "deployed" first
  if FindFirst(TPath.GetDocumentsPath + PathDelim + '*.svg', faArchive, sr) = 0 then
  begin
    repeat
      AddSVG(TPath.GetDocumentsPath + PathDelim + sr.Name);
    until FindNext(sr) <> 0;

    FindClose(sr);
  end;

{$ELSE}
  if FOpenDialog.Execute then
  begin
    AddSVG(FOpenDialog.FileName);
  end;
{$ENDIF}
{$ENDIF}
end;

procedure TSVGViewerForm.ActionPasteExecute(Sender: TObject);
var
  Svc: IFMXClipboardService;
  Value: TValue;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, Svc) then
  begin
    Value := Svc.GetClipboard;
    if not Value.IsEmpty then
    begin
      if Value.IsType<string> then
      begin
        Selection := TSVGSelection.Create(Self);
        Selection.Parent := FLayoutZoom;
        Selection.Images := FImageList2;
        Selection.ImageIndex := FListBox.ItemIndex;
        Selection.SVG := Value.ToString;
        Selection.OnSVGEvent := SVGEvent;

        FZoomTrackBar.Max := MaxZoom(FLayoutZoom);
      end
    end;
  end;
end;

procedure TSVGViewerForm.ActionRemoveExecute(Sender: TObject);
begin
  if assigned(Selection) then
  begin
    Selection.Parent := nil;
    Selection.Free;
    Selection := nil;

    FZoomTrackBar.Max := MaxZoom(FLayoutZoom);
  end;
end;

procedure TSVGViewerForm.ActionZoomDecExecute(Sender: TObject);
var
  NewValue: Single;
begin
  NewValue := FZoomTrackBar.Value / 1.2;

  if NewValue < FZoomTrackBar.Min then
    FZoomTrackBar.Value := FZoomTrackBar.Min
  else
    FZoomTrackBar.Value := NewValue;
end;

procedure TSVGViewerForm.ActionZoomIncExecute(Sender: TObject);
var
  NewValue: Single;
begin
  NewValue := FZoomTrackBar.Value * 1.2;

  if NewValue > FZoomTrackBar.Max then
    FZoomTrackBar.Value := FZoomTrackBar.Max
  else
    FZoomTrackBar.Value := NewValue;
end;

procedure TSVGViewerForm.ActionZoomResetExecute(Sender: TObject);
begin
  FZoomTrackBar.Value := 1.0;
end;

function TSVGViewerForm.AddSVG(const aFilename: string): boolean;
var
  Ext: string;
  Item: TListBoxItem;
begin
  Result := False;
  Ext := Lowercase(ExtractFileExt(aFileName));
  if Ext = '.svg' then
  begin
    Result := True;

    FImageList2.AddSVG(aFilename);

    Item := TListBoxItem.Create(FListBox);
    Item.Text := ExtractFilename(aFileName);
    Item.ImageIndex := FImageList2.Count - 1;

    FListBox.AddObject(Item);
    FListBox.ItemIndex := FListBox.Items.Count - 1;

    Selection := TSVGSelection.Create(Self);
    Selection.Parent := FLayoutZoom;
    Selection.Images := FImageList2;
    Selection.Filename := aFilename;
    Selection.OnSVGEvent := SVGEvent;

    FZoomTrackBar.Max := MaxZoom(FLayoutZoom);
  end;
end;

procedure TSVGViewerForm.AddSVGSelection;
var
  StrStream: TStringStream;
begin
  Selection := TSVGSelection.Create(Self);
  Selection.Parent := FLayoutZoom;
  Selection.Images := FImageList2;
  Selection.ImageIndex := FListBox.ItemIndex;

  StrStream := TStringStream.Create;
  try
    FImageList2.SaveSVGToStream(FListBox.ItemIndex, StrStream);
    Selection.SVG := StrStream.DataString;
  finally
    StrStream.Free;
  end;
  Selection.OnSVGEvent := SVGEvent;

  FZoomTrackBar.Max := MaxZoom(FLayoutZoom);
end;

procedure TSVGViewerForm.AlignControls;
begin
  FZoomBox.CalcLayoutDimensions;
end;

procedure TSVGViewerForm.AnimationTrackbarChange(aSender: TObject);
begin
  if assigned(FSelection) then
  begin
    if FSelection.AnimationIsPaused then
    begin
      FSelection.AnimationTime := Round(FAnimationTrackbar.Value);
      DoTimer(Self);
    end;
  end;
end;

procedure TSVGViewerForm.CheckboxAutoViewboxChange(Sender: TObject);
begin
  if assigned(FSelection) then
    FSelection.AutoViewbox := FCheckboxAutoViewbox.IsChecked;
end;

procedure TSVGViewerForm.CheckboxClippathChange(Sender: TObject);
begin
  if assigned(FSelection) then
  begin
    if FCheckboxClippath.IsChecked then
      FSelection.RenderOptions := FSelection.RenderOptions + [sroClippath]
    else
      FSelection.RenderOptions := FSelection.RenderOptions - [sroClippath];
  end;
end;

procedure TSVGViewerForm.CheckboxFiltersChange(Sender: TObject);
begin
  if assigned(FSelection) then
  begin
    if FCheckboxFilters.IsChecked then
      FSelection.RenderOptions := FSelection.RenderOptions + [sroFilters]
    else
      FSelection.RenderOptions := FSelection.RenderOptions - [sroFilters];
  end;
end;

procedure TSVGViewerForm.CheckboxMouseEventsChange(Sender: TObject);
begin
  if assigned(FSelection) then
  begin
    if FCheckboxMouseEvents.IsChecked then
      FSelection.RenderOptions := FSelection.RenderOptions + [sroEvents]
    else
      FSelection.RenderOptions := FSelection.RenderOptions - [sroEvents];
  end;
end;

procedure TSVGViewerForm.ComboboxAspectRatioChange(Sender: TObject);
begin
  if assigned(FSelection) then
    FSelection.AspectRatioAlign := TSVGAspectRatioAlign(FComboboxAspectRatio.ItemIndex);
end;

procedure TSVGViewerForm.ComboboxMeetOrSliceChange(Sender: TObject);
begin
  if assigned(FSelection) then
    FSelection.AspectRatioMeetOrSlice := TSVGAspectRatioMeetOrSlice(FComboboxMeetOrSlice.ItemIndex);
end;

procedure TSVGViewerForm.ConnectControls(
  aActionAddOk,
  aActionAddCancel,
  aActionAdd,
  aActionCopy,
  aActionCopyDirect,
  aActionInfo,
  aActionLoad,
  aActionPaste,
  aActionRemove,
  aActionZoomReset,
  aActionZoomDec,
  aActionZoomInc: TAction;
  aActionAnimationStart,
  aActionAnimationPause: TButton;
  aCheckboxAutoViewbox,
  aCheckboxClippath,
  aCheckboxFilters,
  aCheckboxMouseEvents: TCheckBox;
  aComboboxAspectRatio,
  aComboboxMeetOrSlice: TComboBox;
  aSpinboxRotate,
  aSpinboxOpacity: TSpinBox;
  aScrollbox: TScrollbox;
  aLayoutSide,
  aLayoutSize, aLayoutZoom: TLayout;
  aListbox: TListbox;
  aImageList1,
  aImageList2: TSVG2ImageList;
  aOpenDialog: TOpenDialog;
  aZoomTrackBar: TTrackBar;
  aLabelInfo: TLabel;
  aAnimationTrackbar: TTrackbar;
  aLabelTime: TLabel);
begin
  FActionAddOk := aActionAddOk;
  FActionAddCancel := aActionAddCancel;
  FActionAdd := aActionAdd;
  FActionCopy := aActionCopy;
  FActionCopyDirect := aActionCopyDirect;
  FActionInfo := aActionInfo;
  FActionLoad := aActionLoad;
  FActionPaste := aActionPaste;
  FActionRemove := aActionRemove;
  FActionZoomReset := aActionZoomReset;
  FActionZoomDec := aActionZoomDec;
  FActionZoomInc := aActionZoomInc;
  FActionAnimationPause := aActionAnimationPause;
  FActionAnimationStart := aActionAnimationStart;
  FCheckboxAutoViewbox := aCheckboxAutoViewbox;
  FCheckboxClippath := aCheckboxClippath;
  FCheckboxFilters := aCheckboxFilters;
  FCheckboxMouseEvents := aCheckboxMouseEvents;
  FComboboxAspectRatio := aComboboxAspectRatio;
  FComboboxMeetOrSlice := aComboboxMeetOrSlice;
  FSpinboxRotate := aSpinboxRotate;
  FSpinboxOpacity := aSpinboxOpacity;
  FScrollbox := aScrollbox;
  FLayoutSide := aLayoutSide;
  FLayoutSize := aLayoutSize;
  FLayoutZoom := aLayoutZoom;
  FListbox := aListbox;
  FImageList1 := aImageList1;
  FImageList2 := aImageList2;
  FOpenDialog := aOpenDialog;
  FZoomTrackBar := aZoomTrackBar;
  FLabelInfo := aLabelInfo;
  FAnimationTrackBar := aAnimationTrackbar;
  FLabelTime := aLabelTime;

  // If the cachesize is too small, images in the list might have to be
  // rerendered wich can make the application slow down.

  FImageList1.CacheSize := 20;
  FImageList2.CacheSize := 20;

  FComboboxMeetOrSlice.Items.Add('Meet');
  FComboboxMeetOrSlice.Items.Add('Slice');

  FComboboxAspectRatio.Items.Add('None');
  FComboboxAspectRatio.Items.Add('XMin YMin');
  FComboboxAspectRatio.Items.Add('XMid YMin');
  FComboboxAspectRatio.Items.Add('XMax YMin');
  FComboboxAspectRatio.Items.Add('XMin YMid');
  FComboboxAspectRatio.Items.Add('XMid YMid');
  FComboboxAspectRatio.Items.Add('XMax YMid');
  FComboboxAspectRatio.Items.Add('XMin YMax');
  FComboboxAspectRatio.Items.Add('XMid YMax');
  FComboboxAspectRatio.Items.Add('XMax YMax');

  FActionAddOk.OnExecute := ActionAddOkExecute;
  FActionAddCancel.OnExecute := ActionAddCancelExecute;
  FActionAdd.OnExecute := ActionAddExecute;
  FActionCopy.OnExecute := ActionCopyExecute;
  FActionCopyDirect.OnExecute := ActionCopyDirectExecute;
  FActionInfo.OnExecute := ActionInfoExecute;
  FActionLoad.OnExecute := ActionLoadExecute;
  FActionPaste.OnExecute := ActionPasteExecute;
  FActionRemove.OnExecute := ActionRemoveExecute;
  FActionZoomReset.OnExecute := ActionZoomResetExecute;
  FActionZoomDec.OnExecute := ActionZoomDecExecute;
  FActionZoomInc.OnExecute := ActionZoomIncExecute;
  FActionAnimationPause.OnClick := ActionAnimationPauseExecute;
  FActionAnimationStart.OnClick := ActionAnimationStartExecute;

  FCheckboxAutoViewbox.OnChange := CheckboxAutoViewboxChange;
  FCheckboxClippath.OnChange := CheckboxClippathChange;
  FCheckboxFilters.OnChange := CheckboxFiltersChange;
  FCheckboxMouseEvents.OnChange := CheckboxMouseEventsChange;
  FComboboxAspectRatio.OnChange := ComboboxAspectRatioChange;
  FComboboxMeetOrSlice.OnChange := ComboboxMeetOrSliceChange;
  FSpinboxRotate.OnChange := SpinboxRotateChange;
  FSpinboxOpacity.OnChange := SpinboxOpacityChange;
  FZoomTrackBar.OnChange := TrackBarChange;
  FScrollbox.OnDragDrop := ScrollBoxDragDrop;
  FScrollbox.OnDragOver := ScrollBoxDragOver;
  FAnimationTrackBar.OnChange := AnimationTrackbarChange;

  FZoomBox := TSVGZoomBox.Create(Self, FLayoutSize, FLayoutZoom, FScrollbox);
end;

constructor TSVGViewerForm.Create(AOwnder: TComponent);
begin
  inherited;

  FTimerAndroidLoad := TTimer.Create(Self);
  FTimerAndroidLoad.Enabled := False;
  FTimerAndroidLoad.Interval := 100;
  FTimerAndroidLoad.OnTimer := TimerAndroidLoadTimer;

  FTimerZoom := TTimer.Create(Self);
  FTimerZoom.Enabled := False;
  FTimerZoom.OnTimer := TimerZoomTimer;

{$IFDEF MSWINDOWS}
  FTimerZoom.Interval := 0;
{$ENDIF}

{$IFDEF MACOS}
  FTimerZoom.Interval := 0;
{$ENDIF}

{$IFDEF IOS}
  FTimerZoom.Interval := 250;
{$ENDIF}

{$IFDEF ANDROID}
  FTimerZoom.Interval := 250;
{$ENDIF}
end;

destructor TSVGViewerForm.Destroy;
begin
  inherited;
end;

procedure TSVGViewerForm.DoShow;
begin
  inherited;

  UpdateControls;
end;

procedure TSVGViewerForm.DoTimer(aSender: TObject);
begin
  if not assigned(FSelection) then
    Exit;

  FLabelTime.Text := Format(' Time: %5.1f   FPS: %3.0f ',
    [FSelection.AnimationTime / 1000, FSelection.AnimationFPS]);

  if FSelection.AnimationTime > FAnimationTrackbar.Max then
    FAnimationTrackbar.Max := FAnimationTrackbar.Max + 5000;

  if not FSelection.AnimationIsPaused then
    FAnimationTrackbar.Value := FSelection.AnimationTime;
end;

function TSVGViewerForm.GetForm: TForm;
begin
  Result := Self;
end;

function TSVGViewerForm.GetScrollbox: TScrollbox;
begin
  Result := FScrollbox;
end;

function TSVGViewerForm.GetSelection: TSVGSelection;
begin
  Result := FSelection;
end;

procedure TSVGViewerForm.KeyDown(var Key: Word; var KeyChar: System.WideChar;
  Shift: TShiftState);
begin
  inherited;

  if Key = vkDelete then
  begin
    ActionRemoveExecute(Self)
  end;
end;

procedure TSVGViewerForm.Resize;
begin
  FZoomBox.CalcLayoutDimensions;
end;

procedure TSVGViewerForm.ScrollBoxDragDrop(Sender: TObject;
  const Data: TDragObject; const Point: TPointF);
var
  i: integer;
begin
  for i := 0 to Length(Data.Files) - 1 do
  begin
    if AddSVG(Data.Files[i]) then
      FSelection.Position.Point := Point;
  end;
end;

procedure TSVGViewerForm.ScrollBoxDragOver(Sender: TObject;
  const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
begin
  if Length(Data.Files) > 0 then
    Operation := TDragOperation.Copy
  else
    Operation := TDragOperation.None;
end;

procedure TSVGViewerForm.SetFormInfo(const Value: TSVGViewerInfoForm);
begin
  FFormInfo := Value;
end;

procedure TSVGViewerForm.SetSelection(const Value: TSVGSelection);
begin
  if assigned(FSelection) then
  begin
    FSelection.Selected := False;
    FSelection.OnAnimationSample := nil;
  end;

  FSelection := Value;

  if assigned(FSelection) then
  begin
    FSelection.Selected := True;
    FSelection.OnAnimationSample := DoTimer;

    FCheckboxAutoViewbox.IsChecked := FSelection.AutoViewbox;
    FCheckboxClippath.IsChecked := sroClippath in FSelection.RenderOptions;
    FCheckboxFilters.IsChecked := sroFilters in FSelection.RenderOptions;
    FCheckboxMouseEvents.IsChecked := sroEvents in FSelection.RenderOptions;
    FSpinboxOpacity.Value := FSelection.Opacity;
    FSpinboxRotate.Value := FSelection.RotationAngle;
    FComboboxAspectRatio.ItemIndex := Ord(FSelection.AspectRatioAlign);
    FComboboxMeetOrSlice.ItemIndex := Ord(FSelection.AspectRatioMeetOrSlice);
  end;

  UpdateControls;
end;

procedure TSVGViewerForm.ShowInfo(const aValue: string);
begin
  FLabelInfo.Text := aValue;
end;

procedure TSVGViewerForm.SpinboxOpacityChange(Sender: TObject);
begin
  if assigned(FSelection) then
    FSelection.Opacity := FSpinboxOpacity.Value;
end;

procedure TSVGViewerForm.SpinboxRotateChange(Sender: TObject);
begin
  if assigned(FSelection) then
    FSelection.RotationAngle := FSpinboxRotate.Value;
end;

procedure TSVGViewerForm.SVGEvent(Sender: TObject; aSVGRoot: ISVGRoot;
  aEvent: ISVGEvent; const aValue: string);
begin
  FLabelInfo.Text := aValue;
end;

{$IFDEF ANDROID}
// http://blong.com/articles/DelphiXE6AndroidActivityResult/ActivityResult.htm

function TSVGViewerForm.LaunchActivityForResult(const Intent: JIntent;
  RequestCode: Integer): Boolean;
var
  ResolveInfo: JResolveInfo;
begin
  ResolveInfo := SharedActivity.getPackageManager.resolveActivity(Intent, 0);
  Result := ResolveInfo <> nil;
  if Result then
    SharedActivity.startActivityForResult(Intent, RequestCode);
end;

procedure TSVGViewerForm.LaunchLoadFile;
var
  Intent: JIntent;
begin
  FFileName := '';

  FMessageSubscriptionID := TMessageManager.DefaultManager.SubscribeToMessage(
    TMessageResultNotification, HandleActivityMessage);

  Intent := TJIntent.JavaClass.init(TJIntent.JavaClass.ACTION_PICK);
  Intent.setType(StringToJString('image/svg+xml'));
  Intent.setAction(TjIntent.JavaClass.ACTION_GET_CONTENT);
  //Intent.putExtra(TJIntent.JavaClass.EXTRA_ALLOW_MULTIPLE,true);

  if not LaunchActivityForResult(Intent, FileRequestCode) then
    ShowMessage('Cannot display file picker');
end;

procedure TSVGViewerForm.HandleActivityMessage(const Sender: TObject;
  const M: TMessage);
begin
  if M is TMessageResultNotification then
    OnActivityResult(TMessageResultNotification(M).RequestCode,
      TMessageResultNotification(M).ResultCode,
      TMessageResultNotification(M).Value);
end;

function TSVGViewerForm.OnActivityResult(RequestCode, ResultCode: Integer;
  Data: JIntent): Boolean;
var
  Uri: Jnet_Uri;
begin
  Result := False;

  TMessageManager.DefaultManager.Unsubscribe(TMessageResultNotification, FMessageSubscriptionID);
  FMessageSubscriptionID := 0;

  if RequestCode = FileRequestCode then
  begin
    if ResultCode = TJActivity.JavaClass.RESULT_OK then
    begin
      if assigned(Data) then
      begin
        Uri := Data.getData;
        if assigned(Uri) then
        begin
          FFileName := JStringToString(Uri.getPath);
          FTimerAndroidLoad.Enabled := True;
        end;
      end;
    end;

    Result := True;
  end;
end;
{$ENDIF}

procedure TSVGViewerForm.TimerAndroidLoadTimer(Sender: TObject);
begin
  FTimerAndroidLoad.Enabled := False;
{$IFDEF ANDROID}
  // Work around for the Bitmap size too big error, bitmap must be created in
  // the main thread

  AddSVG(FFilename);
{$ENDIF}
end;

procedure TSVGViewerForm.TimerZoomTimer(Sender: TObject);
begin
  FTimerZoom.Enabled := False;
  FZoomBox.Zoom := FZoomTrackbar.Value;
end;

procedure TSVGViewerForm.TrackBarChange(Sender: TObject);
begin
  if FTimerZoom.Interval <> 0 then
  begin
    FTimerZoom.Enabled := False;
    FTimerZoom.Enabled := True;
  end else
    FZoomBox.Zoom := FZoomTrackbar.Value;
end;

procedure TSVGViewerForm.UpdateControls;
var
  HasAnimations: Boolean;
begin
  HasAnimations := False;

  FActionCopy.Enabled := True;
  FActionCopyDirect.Enabled := True;
  FCheckboxFilters.Enabled := True;
  FCheckboxClippath.Enabled := True;
  FCheckboxMouseEvents.Enabled := True;
  FCheckboxAutoViewbox.Enabled := True;
  FActionRemove.Enabled := True;
  FActionAnimationStart.Enabled := True;
  FActionAnimationPause.Enabled := True;
  FAnimationTrackbar.Enabled := True;

  if assigned(FSelection) then
  begin
    FCheckboxFilters.IsChecked := sroFilters in FSelection.RenderOptions;
    FCheckboxClippath.IsChecked := sroClippath in FSelection.RenderOptions;
    FCheckboxMouseEvents.IsChecked := sroEvents in FSelection.RenderOptions;
    FCheckboxAutoViewbox.IsChecked := FSelection.AutoViewbox;

    HasAnimations := FSelection.HasAnimations;

    if HasAnimations then
    begin
      FActionAnimationStart.IsPressed := FSelection.AnimationIsStarted;
      FActionAnimationPause.IsPressed := FSelection.AnimationIsPaused;
    end else begin
      FActionAnimationStart.IsPressed := False;
      FActionAnimationPause.IsPressed := False;
    end;

  end else begin
    FCheckboxFilters.IsChecked := False;
    FCheckboxClippath.IsChecked := False;
    FCheckboxMouseEvents.IsChecked := False;
    FCheckboxAutoViewbox.IsChecked := False;
    FActionAnimationStart.IsPressed := False;
    FActionAnimationPause.IsPressed := False;

    FActionCopy.Enabled := False;
    FActionCopyDirect.Enabled := False;
    FCheckboxFilters.Enabled := False;
    FCheckboxClippath.Enabled := False;
    FCheckboxMouseEvents.Enabled := False;
    FCheckboxAutoViewbox.Enabled := False;
    FActionRemove.Enabled := False;
  end;

  FActionAnimationStart.Enabled := HasAnimations;
  FActionAnimationPause.Enabled := HasAnimations;
  FAnimationTrackbar.Enabled := HasAnimations;
end;

// -----------------------------------------------------------------------------
//
//                           TSVGViewerInfoForm
//
// -----------------------------------------------------------------------------

procedure TSVGViewerInfoForm.ButtonClick(Sender: TObject);
begin
  ModalResult := mrOk;
  Close;
end;

procedure TSVGViewerInfoForm.ConnectControls(aButton: TButton;
  aSVGImage: TSVG2Image);
begin
  FButton := aButton;
  FSVGImage := aSVGImage;

  FButton.OnClick := ButtonClick;
  FSVGImage.OnSVGEvent := SVGEvent;
end;

constructor TSVGViewerInfoForm.Create(AOwnder: TComponent);
begin
  inherited;
end;

destructor TSVGViewerInfoForm.Destroy;
begin
  inherited;
end;

procedure TSVGViewerInfoForm.Open(const FilePath: string);
{$IFDEF ANDROID}
var
  Intent: JIntent;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  ShellExecute(0, nil, pchar(FilePath), nil, nil, sw_restore);
{$ENDIF MSWINDOWS}

{$IFDEF MACOS}
{$IFNDEF IOS}
  _system(PAnsiChar('open '+'"'+AnsiString(FilePath)+'"'));
{$ELSE}
  SharedApplication.OpenURL(StrToNSUrl(FilePath));
{$ENDIF}
{$ENDIF MACOS}

{$IFDEF ANDROID}
  Intent := TJIntent.Create;
  Intent.setAction(TJIntent.JavaClass.ACTION_VIEW);
  Intent.setData(StrToJURI(FilePath));
  SharedActivity.startActivity(Intent);
{$ENDIF}
end;

procedure TSVGViewerInfoForm.SVGEvent(Sender: TObject; aSVGRoot: ISVGRoot;
  aEvent: ISVGEvent; const aValue: string);
var
  ImageElement: ISVGImage;
begin
  case aEvent.EventType of
    etClick:
      begin
        if aEvent.Target.ID = 'link' then
          Open('http://www.bverhue.nl/delphisvg/');
      end;
    etMouseOut:
      begin
        if assigned(aEvent.Target) and (aEvent.Target.ID = 'link') then
        begin
          aEvent.Target.Attributes['fill'] := 'black';
          aEvent.Target.Attributes['font-size'] := '16';
          FSVGImage.Repaint;
        end;
      end;
    etMouseOver:
      begin
        if assigned(aEvent.Target) and (aEvent.Target.ID = 'link') then
        begin
          aEvent.Target.Attributes['fill'] := 'blue';
          aEvent.Target.Attributes['font-size'] := '18';
          FSVGImage.Repaint;
        end;
      end;
    etMouseDown:
      begin
        if aEvent.CurrentTarget.ID = 'img_svg_logo' then
        begin
          if Supports(aEvent.CurrentTarget, ISVGImage, ImageElement) then
          begin
            ImageElement.X := TSVGDimension.Init(284);
            ImageElement.Y := TSVGDimension.Init(14);
            FSVGImage.Repaint;
          end;
        end;
      end;
    etMouseUp:
      begin
        if aEvent.CurrentTarget.ID = 'img_svg_logo' then
        begin
          if Supports(aEvent.CurrentTarget, ISVGImage, ImageElement) then
          begin
            ImageElement.X := TSVGDimension.Init(280);
            ImageElement.Y := TSVGDimension.Init(10);
            FSVGImage.Repaint;
            Open('https://www.w3.org/TR/SVG11/');
          end;
        end;
      end;

  end;
end;

end.
