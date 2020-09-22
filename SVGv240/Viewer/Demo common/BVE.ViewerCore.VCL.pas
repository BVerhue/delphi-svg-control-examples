unit BVE.ViewerCore.VCL;

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

///  <summary>
///    This unit contains the core functionality of the "SVG Viewer demo".
///  </summary>

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$Include DemoAppCompilerSettings.inc}

interface
uses
{$IFnDEF FPC}
  Winapi.Windows,
  Winapi.Messages,
  Winapi.ShellApi,
  System.Types,
  System.Classes,
  System.UITypes,
  System.SysUtils,
  System.Generics.Collections,
  System.Math,
  Vcl.ActnList,
  Vcl.Clipbrd,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.Graphics,
  Vcl.ImgList,
  Vcl.ExtDlgs,
  Vcl.Themes,
  Vcl.Dialogs,
{$ELSE}
  Windows,
  Types,
  Classes,
  SysUtils,
  Forms,
  Controls,
  StdCtrls,
  Graphics,
  Clipbrd,
  Generics.Collections,
  ExtCtrls,
  ComCtrls,
  ActnList,
  Dialogs,
  ExtDlgs,
  LclType,
{$ENDIF}
  BVE.SVG2Types,
  BVE.SVG2Intf,
  BVE.SVG2Doc,
{$IFnDEF FPC}
  BVE.SVG2Elements.VCL,
  BVE.SVG2Control.VCl,
  BVE.SVG2Image.VCl,
  BVE.SVG2ImageList.VCL;
{$ELSE}
  BVE.SVG2Elements.FPC,
  BVE.SVG2Control.FPC,
  BVE.SVG2Image.FPC,
  BVE.SVG2ImageList.FPC;
{$ENDIF}


{$IFDEF Ver290Down}
const
  WM_DPICHANGED = 736; // 0x02E0
{$ENDIF}

type
  TParseSVGEvent = procedure(Sender: TObject) of object;

  TSVGSelectControl = class;

  TMarkerType = (mtPoint, mtCross, mtRect, mtCircle);

  TMarker = class
  private
    FID: integer;
    FBounds: TRect;
    FMarkerType: TMarkerType;
    FColor: TSVGColor;
    FDimension: integer;
    FMouseDown: TPoint;

    procedure SetColor(const Value: TSVGColor);
    procedure SetMarkerType(const Value: TMarkerType);
    procedure SetDimension(const Value: integer);
  protected
    function GetPoint: TPoint; virtual; abstract;
    procedure SetPoint(const Value: TPoint); virtual; abstract;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure MouseMove(Shift: TShiftState; X, Y: integer);
  public
    constructor Create(const aDimension: integer;
      const aMarkerType: TMarkerType; const aColor: TSVGColor);
    destructor Destroy; override;

    procedure Draw(aCanvas: TCanvas);

    procedure UpdateBounds;

    property Point: TPoint read GetPoint write SetPoint;
    property ID: integer read FID write FID;
    property Bounds: TRect read FBounds;
    property Dimension: integer read FDimension write SetDimension;
    property MarkerType: TMarkerType read FMarkerType write SetMarkerType;
    property Color: TSVGColor read FColor write SetColor;
  end;

  TBoundsMarker = class(TMarker)
  private
    FIndex: integer;
    FSelectControl: TSVGSelectControl;
  protected
    function GetPoint: TPoint; override;
    procedure SetPoint(const Value: TPoint); override;
  public
    constructor Create(const aDimension: integer);
    destructor Destroy; override;

    procedure SetPointIndex(const aIndex: integer);
  end;

  TSVGSelectionList = class(TList<TSVGSelectControl>)
  private
    FActive: boolean;
    FPosX, FPosY: integer;
    function GetSelectedCount: integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure MoveDelta(const dx, dy: integer);
    procedure UnselectAll;

    property Active: boolean read FActive write FActive;
    property PosX: integer read FPosX write FPosX;
    property Posy: integer read FPosY write FPosY;
    property SelectedCount: integer read GetSelectedCount;
  end;

  TSVGSelectControl = class(TSVG2Control)
  private
    FAnimationTimer: TSVG2AnimationTimer;
    FMargin: integer;
    FMarkerMoving: boolean;
    FBoundsMarkerList: TList<TBoundsMarker>;
    FBoundsMarkersVisible: boolean;
    FContentBounds: TRect;
    FMouseDown: TPoint;
    FSelectionList: TSVGSelectionList;
    FSelected: boolean;
    FMoveable: boolean;
    FMarker: TMarker;

    procedure CalcBoundsContent;
    procedure CreateBoundsMarkers;
    procedure ClearBoundsMarkers;
    procedure ResizeClientRect(const dw, dh: integer);
    procedure UpdateBoundsMarkers;
  protected
    function GetAnimationFPS: TSVGFloat;
    function GetAnimationIsPaused: Boolean;
    function GetAnimationIsStarted: Boolean;
    function GetAbsoluteClientRect: TRect;
    function GetBoundsMarkerPoint(const aIndex: integer): TPoint;
    function GetBoundsMarkersVisible: boolean;
    function GetOnAnimationSample: TNotifyEvent;

    procedure SetAnimationIsPaused(const Value: Boolean);
    procedure SetAnimationIsStarted(const Value: Boolean);
    procedure SetBoundsMarkerPoint(const aIndex: integer; const Value: TPoint);
    procedure SetBoundsMarkersVisible(const Value: boolean);
    procedure SetMargin(const Value: integer);
    procedure SetMoveable(const Value: boolean);
    procedure SetOnAnimationSample(const Value: TNotifyEvent);

    procedure DoAfterParse; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure Resize; override;
    {$IFnDEF FPC}
    {$IFDEF Ver310Up}
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
    {$ELSE}
    procedure ChangeScale(M, D: Integer); override;
    {$ENDIF}
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent; aSelectionList: TSVGSelectionList); reintroduce; virtual;
    destructor Destroy; override;

    {$IFDEF FPC}
    procedure AutoAdjustLayout(AMode: TLayoutAdjustmentPolicy;
      const AFromPPI, AToPPI, AOldFormWidth, ANewFormWidth: Integer); override;
    {$ENDIF}

    function CreateCopy(aSelectionList : TSVGSelectionList): TSVGSelectControl;
    procedure Assign(Source: TPersistent); override;

    procedure Paint; override;

    procedure Select;
    procedure Deselect;

    procedure MoveDelta(const dx, dy: integer);
    procedure SetPosition(const aValue: TPoint);
    procedure SetSize(const aValue: TRect);

    procedure LoadSVG(const aFileName : string);

    property BoundsMarkerPoint[const aIndex: integer]: TPoint
      read GetBoundsMarkerPoint write SetBoundsMarkerPoint;
    property AbsoluteClientRect: TRect read GetAbsoluteClientRect;
    property ContentBounds: TRect read FContentBounds;
  published
    property AnimationFPS: TSVGFloat read GetAnimationFPS;
    property AnimationIsStarted: Boolean read GetAnimationIsStarted
      write SetAnimationIsStarted;
    property AnimationIsPaused: Boolean read GetAnimationIsPaused
      write SetAnimationIsPaused;
    property BoundsMarkersVisible: boolean read GetBoundsMarkersVisible
      write SetBoundsMarkersVisible;
    property Margin: integer read FMargin write SetMargin;
    property Selected: boolean read FSelected;
    property Moveable: boolean read FMoveable write SetMoveable;

    property OnAnimationSample: TNotifyEvent read GetOnAnimationSample
      write SetOnAnimationSample;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  // Example of interactive SVG

  TSVGAnimatedClock = class(TSVGSelectControl)
  private
    FGearSeconds: ISVGObject;
    FGear7_5: ISVGObject;
    FGearMinutes: ISVGObject;
    FGear180: ISVGObject;
    FGearHours: ISVGObject;

    FHandSecond: ISVGObject;
    FPathHandSecond: ISVGObject;

    FHandMinute: ISVGObject;
    FPathHandMinute: ISVGObject;

    FHandHour: ISVGObject;
    FPathhandHour: ISVGObject;

    FHour, FMin, FSec: TSVGFloat;
    FTimer: TTimer;

    FMouseDownPoint: TPoint;
    FSelectedObject: ISVGObject;

    procedure SetLayoutAngle(aLayout: ISVGObject; aAngle: TSVGFloat);
    procedure AddDeltaSec(const aD: TSVGFloat);
    procedure AddDeltaMin(const aD: TSVGFloat);
    procedure AddDeltaHour(const aD: TSVGFloat);
    procedure UpdateClock;
    procedure SetTime(Sender: TObject);
    function GetValid: boolean;
  protected
    procedure DoAfterParse; override;
  public
    constructor Create(AOwner: TComponent; aSelectionList: TSVGSelectionList); override;
    destructor Destroy; override;

    procedure Init;

    procedure Assign(Source: TPersistent); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    procedure ProcessMouseMove( aSVGObject: ISVGObject; Shift: TShiftState; dX, dY: Integer);
  end;

  TSVGViewerPropertiesForm = class;
  TSVGViewerAboutForm = class;

  TSVGViewerForm = class(TForm)
  private
    FSelectionList: TSVGSelectionList;

    FActionOpen: TAction;
    FActionEdit: TAction;
    FActionCopy: TAction;
    FActionNew: TAction;
    FActionPaste: TAction;
    FActionCopyDirect: TAction;
    FActionAbout: TAction;
    FActionRemove: TAction;
    FActionAutoViewBox: TAction;
    FActionExport: TAction;

    FActionEnableFilters: TAction;
    FActionEnableClippaths: TAction;
    FActionEnableEvents: TAction;
    FActionEnablePersistentBuffers: TAction;
    FActionEnableTextToPath: TAction;

    FActionAnimationPause: TAction;
    FActionAnimationStart: TAction;

    FScrollBox: TScrollBox;
    FOpenDialog: TOpenDialog;
    FStatusBar: TStatusBar;
    FLabelTime: TLabel;
    FTrackBar: TTrackBar;

    FImageListList: TList<TSVG2BaseImageList>;

    FFormProperties: TSVGViewerPropertiesForm;
    FFormAbout: TSVGViewerAboutForm;

    procedure AddSVGControl(aControl: TSVGSelectControl);
    function OpenSVGSelectControl(const aFilename: string): TSVGSelectControl;
    function CreateSVGSelectControl(const aSvg: string): TSVGSelectControl;
    function NewSVGSelectControl: TSVGSelectControl;
    procedure CheckValidSVG(const aSvg: string);
    function RenderToBitmap: TBitmap;
    procedure ImageListScale(aImageList: TSVG2BaseImageList; M, D: Integer);
    procedure ImageListTheme(aImageList: TSVG2BaseImageList);

    procedure AfterParse(Sender: TObject);
    {$IFDEF Ver290Down}
    // Although you can have application build with XE8 and lower to
    // respond to monitor dpi changes, see here:
    // https://www.helpandmanual.com/downloads_delphi.html
    // It doesn't seem to work on application startup.
    procedure WMDpiChanged(var Message: TMessage); message WM_DPICHANGED;
    {$ENDIF}
    procedure BeforeMonitorDpiChanged(Sender: TObject; OldDPI,
      NewDPI: Integer);
    procedure SVGEvent(Sender: TObject; aSVGRoot: ISVGRoot; aEvent: ISVGEvent;
      const aValue: string);
    procedure ScrollBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SVGSelectMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TrackBarChange(Sender: TObject);
    {$IFnDEF FPC}
    procedure WMDROPFILES(var Msg: TWMDropFiles); message WM_DROPFILES;
    procedure WMClipboardUpdate(var Msg: TMessage); message WM_CLIPBOARDUPDATE;
    procedure SetActionEnableClippaths(const Value: TAction);
    procedure SetActionEnableEvents(const Value: TAction);
    procedure SetActionEnableFilters(const Value: TAction);
    procedure SetActionEnablePersistentBuffers(const Value: TAction);
    procedure SetActionEnableTextToPath(const Value: TAction);
    {$ELSE}
    procedure OnDropFiles(Sender: TObject; const FileNames: array of String);
    {$ENDIF}
  public
    function GetAutoViewBox: boolean;
    function GetFilters: boolean;

    procedure SetAutoViewBox(const Value: boolean);
    procedure SetActionAnimationPause(const Value: TAction);
    procedure SetActionAnimationStart(const Value: TAction);
    procedure SeActionEnableFilters(const Value: TAction);
    procedure SetActionAbout(const Value: TAction);
    procedure SetActionAutoViewBox(const Value: TAction);
    procedure SetActionCopy(const Value: TAction);
    procedure SetActionCopyDirect(const Value: TAction);
    procedure SetActionEdit(const Value: TAction);
    procedure SetActionExport(const Value: TAction);
    procedure SetActionNew(const Value: TAction);
    procedure SetActionOpen(const Value: TAction);
    procedure SetActionPaste(const Value: TAction);
    procedure SetActionRemove(const Value: TAction);
    procedure SetFilters(const Value: boolean);
    procedure SetFormAbout(const Value: TSVGViewerAboutForm);
    procedure SetFormProperties(const Value: TSVGViewerPropertiesForm);
    procedure SetLabelTime(const Value: TLabel);
    procedure SetStatusBar(const Value: TStatusBar);
    procedure SetOpenDialog(const Value: TOpenDialog);
    procedure SetTrackBar(const Value: TTrackbar);
    procedure SetScrollBox(const Value: TScrollBox);
  protected
    procedure DoCreate; override;
    procedure DoDestroy; override;
    procedure DoShow; override;
    procedure DoSVGEvent(aEvent: ISVGEvent; const aValue: string); virtual;
    procedure DoTimer(aSender: TObject);

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Hint(Sender: TObject);

    procedure ActionOpenExecute(Sender: TObject);
    procedure ActionEditExecute(Sender: TObject);
    procedure ActionCopyExecute(Sender: TObject);
    procedure ActionExitExecute(Sender: TObject);
    procedure ActionRemoveExecute(Sender: TObject);
    procedure ActionCopyDirectExecute(Sender: TObject);
    procedure ActionPasteExecute(Sender: TObject);
    procedure ActionAutoViewBoxExecute(Sender: TObject);
    procedure ActionEnableClippathsExecute(Sender: TObject);
    procedure ActionEnableEventsExecute(Sender: TObject);
    procedure ActionEnableFiltersExecute(Sender: TObject);
    procedure ActionEnablePersistentBuffersExecute(Sender: TObject);
    procedure ActionEnableTextToPathExecute(Sender: TObject);
    procedure ActionExportExecute(Sender: TObject);
    procedure ActionAboutExecute(Sender: TObject);
    procedure ActionNewExecute(Sender: TObject);
    procedure ActionAnimationStartExecute(Sender: TObject);
    procedure ActionAnimationPauseExecute(Sender: TObject);

    procedure SetCurrSelectControl(const Value: TSVGSelectControl); virtual;
    function GetCurrSelectControl: TSVGSelectControl; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ConnectControls(
      aActionOpen: TAction;
      aActionEdit: TAction;
      aActionCopy: TAction;
      aActionNew: TAction;
      aActionPaste: TAction;
      aActionCopyDirect: TAction;
      aActionAbout: TAction;
      aActionRemove: TAction;
      aActionAutoViewBox: TAction;
      aActionExport: TAction;
      aActionEnableFilters: TAction;
      aActionEnableClippaths: TAction;
      aActionEnableEvents: TAction;
      aActionEnablePersistentBuffers: TAction;
      aActionEnableTextToPath: TAction;
      aActionAnimationStart: TAction;
      aActionAnimationPause: TAction;
      aOpenDialog: TOpenDialog;
      aScrollBox: TScrollBox;
      aStatusBar: TStatusBar;
      aTrackbar: TTrackbar;
      aLabelTime: TLabel);

    function CheckClipboard: boolean;

    procedure RegisterImageList(aImageList: TSVG2BaseImageList);

    procedure Copy;
    procedure CopyDirect;
    procedure ExportSVG;
    procedure LoadSVG(const aFileName: string);
    procedure NewControl;
    procedure Paste;
    procedure RemoveSVGControl;

    procedure UpdateControls;

    property ActionOpen: TAction read FActionOpen write SetActionOpen;
    property ActionEdit: TAction read FActionEdit write SetActionEdit;
    property ActionCopy: TAction read FActionCopy write SetActionCopy;
    property ActionNew: TAction read FActionNew write SetActionNew;
    property ActionPaste: TAction read FActionPaste write SetActionPaste;
    property ActionCopyDirect: TAction read FActionCopyDirect write SetActionCopyDirect;
    property ActionAbout: TAction read FActionAbout write SetActionAbout;
    property ActionRemove: TAction read FActionRemove write SetActionRemove;
    property ActionAutoViewBox: TAction read FActionAutoViewBox write SetActionAutoViewBox;
    property ActionExport: TAction read FActionExport write SetActionExport;

    property ActionEnableFilters: TAction read FActionEnableFilters write SetActionEnableFilters;
    property ActionEnableClippaths: TAction read FActionEnableClippaths write SetActionEnableClippaths;
    property ActionEnableEvents: TAction read FActionEnableEvents write SetActionEnableEvents;
    property ActionEnablePersistentBuffers: TAction read FActionEnablePersistentBuffers write SetActionEnablePersistentBuffers;
    property ActionEnableTextToPath: TAction read FActionEnableTextToPath write SetActionEnableTextToPath;

    property ActionAnimationStart: TAction read FActionAnimationStart write SetActionAnimationStart;
    property ActionAnimationPause: TAction read FActionAnimationPause write SetActionAnimationPause;

    property OpenDialog: TOpenDialog read FOpenDialog write SetOpenDialog;
    property StatusBar: TStatusBar read FStatusBar write SetStatusBar;

    property AutoViewBox: boolean read GetAutoViewBox write SetAutoViewBox;
    property FormAbout: TSVGViewerAboutForm read FFormAbout write SetFormAbout;
    property FormProperties: TSVGViewerPropertiesForm read FFormProperties write SetFormProperties;
    property CurrSelectControl: TSVGSelectControl read GetCurrSelectControl write SetCurrSelectControl;
    property SelectionList: TSVGSelectionList read FSelectionList;
    property ScrollBox: TScrollBox read FScrollBox write SetScrollBox;
    property TrackBar: TTrackbar read FTrackBar write SetTrackBar;
    property LabelTime: TLabel read FLabelTime write SetLabelTime;
  end;

  TSVGViewerPropertiesForm = class(TForm)
  private
    FTextChanged: Boolean;
    FSelectControl: TSVGSelectControl;
    FActionParse: TAction;
    FMemoInfo: TMemo;
    FMemoSVG: TMemo;
    FPanelInfo: TPanel;
    FCheckboxFilters: TCheckbox;
    FCheckboxAutoViewbox: TCheckBox;
    FCheckboxEvents: TCheckbox;
    FCheckboxClippaths: TCheckbox;
    {$IFnDEF FPC}
    FActionAttrChange: TAction;
    FDoc: ISVGDoc;
    FListView: TListView;
    FTreeView: TTreeView;
    FMemoAttrValue: TMemo;
    {$ELSE}
    FEditWidth: TEdit;
    FEditHeight: TEdit;
    {$ENDIF}
  protected
    procedure SetActionParse(const Value: TAction);
    procedure SetMemoInfo(const Value: TMemo);
    procedure SetMemoSVG(const Value: TMemo);
    procedure SetPanelInfo(const Value: TPanel);
    procedure SetCheckboxAutoViewbox(const Value: TCheckBox);
    procedure SetCheckboxClippaths(const Value: TCheckbox);
    procedure SetCheckboxEvents(const Value: TCheckbox);
    procedure SetCheckboxFilters(const Value: TCheckbox);
    procedure SetSelectControl(const Value: TSVGSelectControl);
    {$IFnDEF FPC}
    procedure SetActionAttrChange(const Value: TAction);
    procedure SetMemoAttrValue(const Value: TMemo);
    procedure SetListView(const Value: TListView);
    procedure SetTreeView(const Value: TTreeView);
    {$ELSE}
    procedure SetEditWidth(const Value: TEdit);
    procedure SetEditHeight(const Value: TEdit);
    {$ENDIF}

    procedure DoShow; override;

    procedure ActionParseExecute(Sender: TObject);
    procedure CheckboxAutoViewboxClick(Sender: TObject);
    procedure CheckboxClippathsClick(Sender: TObject);
    procedure CheckboxFiltersClick(Sender: TObject);
    procedure CheckboxEventsClick(Sender: TObject);
    {$IFnDEF FPC}
    procedure ActionAttrChangeExecute(Sender: TObject);
    procedure ListViewClick(Sender: TObject);
    procedure TreeViewClick(Sender: TObject);
    procedure TreeViewExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
    {$ELSE}
    procedure EditWidthExit(Sender: TObject);
    procedure EditHeightExit(Sender: TObject);
    {$ENDIF}
    procedure MemoSVGChange(Sender: TObject);
    procedure MemoSVGExit(Sender: TObject);

    procedure UpdateAll;
    procedure UpdateCaption;
    procedure UpdateText;
    {$IFnDEF FPC}
    procedure UpdateTreeView;
    {$ENDIF}
    procedure UpdateOptions;
    procedure ApplyText;
  public
    constructor Create(AOwnder: TComponent); override;
    destructor Destroy; override;

    procedure ConnectControls(
      {$IFnDEF FPC}
      aActionAttrChange: TAction;
      aMemoAttrValue: TMemo;
      aListView: TListView;
      aTreeView: TTreeView;
      {$ELSE}
      aEditWidth: TEdit;
      aEditHeight: TEdit;
      {$ENDIF}
      aActionParse: TAction;
      aMemoInfo: TMemo;
      aMemoSVG: TMemo;
      aPanelInfo: TPanel;
      aCheckboxFilters: TCheckbox;
      aCheckboxAutoViewbox: TCheckBox;
      aCheckboxEvents: TCheckbox;
      aCheckboxClippaths: TCheckbox);

    procedure Parse;
    {$IFnDEF FPC}
    procedure AttrChange;
    {$ENDIF}

    property ActionParse: TAction read FActionParse write SetActionParse;
    property CheckboxAutoViewbox: TCheckBox read FCheckboxAutoViewbox write SetCheckboxAutoViewbox;
    property CheckboxFilters: TCheckbox read FCheckboxFilters write SetCheckboxFilters;
    property CheckboxClippaths: TCheckbox read FCheckboxClippaths write SetCheckboxClippaths;
    property CheckboxEvents: TCheckbox read FCheckboxEvents write SetCheckboxEvents;
    property MemoInfo: TMemo read FMemoInfo write SetMemoInfo;
    property MemoSVG: TMemo read FMemoSVG write SetMemoSVG;
    property PanelInfo: TPanel read FPanelInfo write SetPanelInfo;
    property SelectControl: TSVGSelectControl read FSelectControl write SetSelectControl;
    {$IFnDEF FPC}
    property ActionAttrChange: TAction read FActionAttrChange write SetActionAttrChange;
    property MemoAttrValue: TMemo read FMemoAttrValue write SetMemoAttrValue;
    property TreeView: TTreeView read FTreeView write SetTreeView;
    property ListView: TListView read FListView write SetListView;
    {$ELSE}
    property EditWidth: TEdit read FEditWidth write SetEditWidth;
    property EditHeight: TEdit read FEditHeight write SetEditHeight;
    {$ENDIF}
  end;

  TSVGViewerAboutForm = class(TForm)
  private
    FSVGImage: TSVG2Image;
  protected
    procedure SVGEvent(Sender: TObject; aSVGRoot: ISVGRoot; aEvent: ISVGEvent;
      const aValue: string);
  public
    constructor Create(AOwnder: TComponent); override;
    destructor Destroy; override;

    procedure ConnectControls(aSVGImage: TSVG2Image);
  end;

implementation
uses
  {$IFnDEF FPC}
  BVE.XMLTreeView.VCL,
  {$ENDIF}
  BVE.SVG2FilterUtility,
  BVE.SVG2SaxParser,
  BVE.SVG2Elements;

//------------------------------------------------------------------------------
//
//                            TSVGSelectionList
//
//------------------------------------------------------------------------------

constructor TSVGSelectionList.Create;
begin
  inherited;
end;

destructor TSVGSelectionList.Destroy;
begin
  while Count > 0 do
    Items[0].Free;

  inherited;
end;

function TSVGSelectionList.GetSelectedCount: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    if Items[i].Selected then
      Inc(Result);
end;

procedure TSVGSelectionList.MoveDelta(const dx, dy: integer);
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].Selected then
      Items[i].MoveDelta(dx, dy);
end;

procedure TSVGSelectionList.UnselectAll;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Deselect;
end;

//------------------------------------------------------------------------------
//
//                                TMarker
//
//------------------------------------------------------------------------------

constructor TMarker.Create(const aDimension: integer;
  const aMarkerType: TMarkerType; const aColor: TSVGColor);
begin
  inherited Create;

  FDimension := aDimension;

  FBounds := Rect(
    -FDimension,
    -FDimension,
     FDimension,
     FDimension);

  FMarkerType := aMarkerType;
  FColor := aColor;
end;

destructor TMarker.Destroy;
begin
  inherited;
end;

procedure TMarker.Draw(aCanvas: TCanvas);
var
  OldBrush: TBrush;
  OldPen: TPen;
begin
  OldBrush := TBrush.Create;
  try
    OldBrush.Assign(aCanvas.Brush);
    OldPen := TPen.Create;
    try
      OldPen.Assign(aCanvas.Pen);
      case FMarkerType of
        mtPoint:
          begin
            aCanvas.Brush.Color := SvgColorToColor(FColor);
            aCanvas.Brush.Style := TBrushStyle.bsSolid;
            aCanvas.Pen.Style := TPenStyle.psClear;

            aCanvas.Ellipse(
              FBounds.Left,
              FBounds.Top,
              FBounds.Right,
              FBounds.Bottom);
          end;
        mtCross:
          begin
            aCanvas.Brush.Style := TBrushStyle.bsClear;
            aCanvas.Pen.Color := SvgColorToColor(FColor);
            aCanvas.Pen.Style := TPenStyle.psSolid;

            aCanvas.MoveTo(
              FBounds.Left,
              FBounds.Top);
            aCanvas.LineTo(
              FBounds.Right,
              FBounds.Bottom);
            aCanvas.MoveTo(
              FBounds.Left,
              FBounds.Bottom);
            aCanvas.LineTo(
              FBounds.Right,
              FBounds.Top);
          end;
        mtRect:
          begin
            aCanvas.Brush.Color := SvgColorToColor(FColor);
            aCanvas.Brush.Style := TBrushStyle.bsSolid;
            aCanvas.Pen.Style := TPenStyle.psSolid;

            aCanvas.FillRect(
              Rect(
                FBounds.Left,
                FBounds.Top,
                FBounds.Right,
                FBounds.Bottom));
          end;
        mtCircle:
          begin
            aCanvas.Brush.Style := TBrushStyle.bsClear;
            aCanvas.Pen.Color := SvgColorToColor(FColor);
            aCanvas.Pen.Style := TPenStyle.psSolid;

            aCanvas.Ellipse(
              FBounds.Left,
              FBounds.Top,
              FBounds.Right,
              FBounds.Bottom);
          end;
      end;
    finally
      aCanvas.Pen.Assign(OldPen);
      OldPen.Free;
    end;
  finally
    aCanvas.Brush.Assign(OldBrush);
    OldBrush.Free;
  end;
end;

procedure TMarker.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  if ssLeft in Shift then
  begin
    FMouseDown.X := X;
    FMouseDown.Y := Y;
  end;

  inherited;
end;

procedure TMarker.MouseMove(Shift: TShiftState; X, Y: integer);
var
  dx, dy: integer;
  p: TPoint;
begin
  if ssLeft in Shift then
  begin
    dx := X - FMouseDown.X;
    dy := Y - FMouseDown.Y;

    p := Point;
    p.X := p.X + dx;
    p.Y := p.Y + dy;
    Point := p;
  end;
  inherited;
end;

procedure TMarker.SetColor(const Value: TSVGColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
  end;
end;

procedure TMarker.SetDimension(const Value: integer);
begin
  if FDimension <> Value then
  begin
    FDimension := Value;
    UpdateBounds;
  end;
end;

procedure TMarker.SetMarkerType(const Value: TMarkerType);
begin
  if FMarkerType <> Value then
  begin
    FMarkerType := Value;
  end;
end;

procedure TMarker.UpdateBounds;
var
  p: TPoint;
begin
  p := GetPoint;

  FBounds := Rect(
    p.X - FDimension,
    p.Y - FDimension,
    p.X + FDimension,
    p.Y + FDimension);
end;

//------------------------------------------------------------------------------
//
//                              TBoundsMarker
//
//------------------------------------------------------------------------------

constructor TBoundsMarker.Create(const aDimension: integer);
begin
  inherited Create(aDimension, mtCircle, SVGColorBlack);

  FIndex := 0;
end;

destructor TBoundsMarker.Destroy;
begin
  inherited;
end;

function TBoundsMarker.GetPoint: TPoint;
begin
  if assigned(FSelectControl) then
  begin
    Result := FSelectControl.BoundsMarkerPoint[FIndex];
  end
  else
  begin
    Result.X := 0;
    Result.Y := 0;
  end;
end;

procedure TBoundsMarker.SetPointIndex(const aIndex: integer);
begin
  FIndex := aIndex;
end;

procedure TBoundsMarker.SetPoint(const Value: TPoint);
begin
  if assigned(FSelectControl) then
  begin
    FSelectControl.BoundsMarkerPoint[FIndex] := Value;
  end;
end;

//------------------------------------------------------------------------------
//
//                            TSVGSelectControl
//
//------------------------------------------------------------------------------

constructor TSVGSelectControl.Create(AOwner: TComponent;
  aSelectionList: TSVGSelectionList);
begin
  FAnimationTimer := TSVG2AnimationTimer.Create(AOwner);
  AnimationTimer := FAnimationTimer;

  FBoundsMarkerList := TList<TBoundsMarker>.Create;

  FBoundsMarkersVisible := True;

  FMarkerMoving := False;

  FMargin := 10;

  FSelectionList := aSelectionList;
  FSelectionList.Add(self);
  FSelected := False;

  FMoveable := True;

  inherited Create(AOwner);

  Padding.Left := FMargin;
  Padding.Top := FMargin;
  Padding.Right := FMargin;
  Padding.Bottom := FMargin;

  AutoViewBox := False;
  RenderOptions := [sroClippath, sroPersistentBuffers, sroEvents];
end;

destructor TSVGSelectControl.Destroy;
begin
  if assigned(FSelectionList) then
    FSelectionList.Remove(Self);

  ClearBoundsMarkers;

  FBoundsMarkerList.Free;

  inherited;
end;

procedure TSVGSelectControl.DoAfterParse;
begin
  inherited;

  CalcSize;

  if HasAnimations then
    AnimationTimer := FAnimationTimer;
end;

procedure TSVGSelectControl.Assign(Source: TPersistent);
begin
  if Source is TSVGSelectControl then
  begin
    inherited;

    Width := (Source as TSVGSelectControl).Width;
    Height := (Source as TSVGSelectControl).Height;

  end else
    inherited;
end;

procedure TSVGSelectControl.CalcBoundsContent;
begin
  FContentBounds := Rect(
    Padding.Left,
    Padding.Top,
    Width - Padding.Right,
    Height - Padding.Bottom);
end;

{$IFDEF FPC}
procedure TSVGSelectControl.AutoAdjustLayout(AMode: TLayoutAdjustmentPolicy;
  const AFromPPI, AToPPI, AOldFormWidth, ANewFormWidth: Integer);
var
  i: integer;
begin
  inherited;

  for i := 0 to FBoundsMarkerList.Count - 1 do
    FBoundsMarkerList[i].Dimension := Round(4 * Scale);
end;
{$ELSE}
{$IFDEF Ver310Up}
procedure TSVGSelectControl.ChangeScale(M, D: Integer; isDpiChange: Boolean);
{$ELSE}
procedure TSVGSelectControl.ChangeScale(M, D: Integer);
{$ENDIF}
var
  i: integer;
begin
  inherited;

  for i := 0 to FBoundsMarkerList.Count - 1 do
    FBoundsMarkerList[i].Dimension := Round(4 * Scale);
end;
{$ENDIF}

procedure TSVGSelectControl.ClearBoundsMarkers;
var
  i: integer;
begin
  for i := 0 to FBoundsMarkerList.Count - 1 do
    FBoundsMarkerList[i].Free;
  FBoundsMarkerList.Clear;
end;

procedure TSVGSelectControl.CreateBoundsMarkers;
var
  BoundsMarker: TBoundsMarker;
  i: integer;
begin
  ClearBoundsMarkers;

  for i := 0 to 3 do
  begin
    BoundsMarker := TBoundsMarker.Create(Round(4 * Scale));
    BoundsMarker.ID := i;
    BoundsMarker.MarkerType := mtCircle;
    BoundsMarker.Color := SVGColorBlue;
    BoundsMarker.SetPointIndex(i);
    BoundsMarker.FSelectControl := self;
    FBoundsMarkerList.Add(BoundsMarker);
    UpdateBoundsMarkers;
  end;
end;

function TSVGSelectControl.CreateCopy(
  aSelectionList: TSVGSelectionList): TSVGSelectControl;
begin
  Result := TSVGSelectControl.Create(Owner, aSelectionList);
  Result.Assign(Self);
end;

procedure TSVGSelectControl.Deselect;
begin
  FSelected := False;
  BoundsMarkersVisible := FSelected;
end;

function TSVGSelectControl.GetAbsoluteClientRect: TRect;
begin
  Result.Left := Padding.Left - 1;
  Result.Top := Padding.Top - 1;
  Result.Right := Width - Padding.Right + 1;
  Result.Bottom := Height - Padding.Bottom + 1;
end;

function TSVGSelectControl.GetAnimationFPS: TSVGFloat;
begin
  Result := FAnimationTimer.FPS;
end;

function TSVGSelectControl.GetAnimationIsPaused: Boolean;
begin
  Result := FAnimationTimer.IsPaused;
end;

function TSVGSelectControl.GetAnimationIsStarted: Boolean;
begin
  Result := FAnimationTimer.IsStarted;
end;

function TSVGSelectControl.GetBoundsMarkerPoint(const aIndex: integer): TPoint;
var
  R: TRect;
begin
  R := AbsoluteClientRect;

  case aIndex of
    0:
      begin
        Result.X := R.Left;
        Result.Y := R.Top
      end;
    1:
      begin
        Result.X := R.Right;
        Result.Y := R.Top
      end;
    2:
      begin
        Result.X := R.Right;
        Result.Y := R.Bottom
      end;
    3:
      begin
        Result.X := R.Left;
        Result.Y := R.Bottom
      end;
    else
      begin
        Result.X := 0;
        Result.Y := 0
      end;
  end;
end;

function TSVGSelectControl.GetBoundsMarkersVisible: boolean;
begin
  Result := FBoundsMarkersVisible;
end;

function TSVGSelectControl.GetOnAnimationSample: TNotifyEvent;
begin
  Result := FAnimationTimer.OnSample;
end;

procedure TSVGSelectControl.LoadSVG(const aFileName: string);
begin
  Filename := aFilename;
  Repaint;
end;

procedure TSVGSelectControl.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
var
  i: integer;
begin
  FMarker := nil;
  for i := 0 to FBoundsMarkerList.Count - 1 do
  begin
    if PtInRect(FBoundsMarkerList[i].Bounds, Point(X, Y)) then
    begin
      FMarker := FBoundsMarkerList[i];
      Break;
    end;
  end;

  if assigned(FMarker) then
  begin
    FMarker.MouseDown(Button, Shift, X - FMarker.Bounds.Left, Y - FMarker.Bounds.Top);
  end else begin

    if ssLeft in Shift then
    begin
      FMouseDown.X := X;
      FMouseDown.Y := Y;
    end;

    if not(ssShift in Shift) then
      if assigned(FSelectionList) then
        FSelectionList.UnselectAll;

    Select;

    inherited;
  end;
end;

procedure TSVGSelectControl.MouseMove(Shift: TShiftState; X, Y: integer);
var
  dx, dy: integer;
begin
  if assigned(FMarker) then
  begin
    FMarker.MouseMove(Shift, X - FMarker.Bounds.Left, Y - FMarker.Bounds.Top);
  end else begin

    if ssLeft in Shift then
    begin
      dx := X - FMouseDown.X;
      dy := Y - FMouseDown.Y;
      if FMoveable and Selected then
        if assigned(FSelectionList) then
          FSelectionList.MoveDelta(dx, dy)
        else
          MoveDelta(dx, dy);
    end;

    inherited;
  end;
end;

procedure TSVGSelectControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: integer);
begin
  inherited;
end;

procedure TSVGSelectControl.MoveDelta(const dx, dy: integer);
begin
  Left := Left + dx;
  Top := Top + dy;
  UpdateBoundsMarkers;
end;

procedure TSVGSelectControl.Paint;
var
  i: integer;
begin
  inherited;

  if FBoundsMarkersVisible then
  begin
    Canvas.DrawFocusRect(AbsoluteClientRect);

    for i := 0 to FBoundsMarkerList.Count - 1 do
      FBoundsMarkerList[i].Draw(Canvas);
  end;
end;

procedure TSVGSelectControl.Resize;
begin
  inherited;

  CalcBoundsContent;

  if not FMarkerMoving then
  begin
    if FBoundsMarkerList.Count = 0 then
      CreateBoundsMarkers
    else
      UpdateBoundsMarkers;
  end;
end;

procedure TSVGSelectControl.ResizeClientRect(const dw, dh: integer);
begin
  if (dw = 0) and (dh = 0) then
    Exit;

  Width := Width + dw;
  Height := Height + dh;
  UpdateBoundsMarkers;
end;

procedure TSVGSelectControl.Select;
begin
  FSelected := True;
  BoundsMarkersVisible := FSelected;
end;

procedure TSVGSelectControl.SetAnimationIsPaused(const Value: Boolean);
begin
  FAnimationTimer.IsPaused := Value;
end;

procedure TSVGSelectControl.SetAnimationIsStarted(const Value: Boolean);
begin
  FAnimationTimer.IsStarted := Value;
end;

procedure TSVGSelectControl.SetBoundsMarkerPoint(const aIndex: integer;
  const Value: TPoint);
var
  R: TRect;
begin
  R := AbsoluteClientRect;

  case aIndex of
    0: SetSize(Rect(Value.X, Value.Y, R.Right, R.Bottom));
    1: SetSize(Rect(R.Left, Value.Y, Value.X, R.Bottom));
    2: SetSize(Rect(R.Left, R.Top, Value.X, Value.Y));
    3: SetSize(Rect(Value.X, R.Top, R.Right, Value.Y));
  end;
end;

procedure TSVGSelectControl.SetBoundsMarkersVisible(const Value: boolean);
begin
  if Value <> FBoundsMarkersVisible then
  begin
    FBoundsMarkersVisible := Value;
    Invalidate;
  end;
end;

procedure TSVGSelectControl.SetMargin(const Value: integer);
begin
  FMargin := Value;
  CalcBoundsContent;
  Repaint;
end;

procedure TSVGSelectControl.SetMoveable(const Value: boolean);
begin
  FMoveable := Value;
end;

procedure TSVGSelectControl.SetOnAnimationSample(const Value: TNotifyEvent);
begin
  FAnimationTimer.OnSample := Value;
end;

procedure TSVGSelectControl.SetPosition(const aValue: TPoint);
var
  R: TRect;
begin
  FMarkerMoving := True;
  try
    R := AbsoluteClientRect;
    MoveDelta(aValue.X - R.Left, aValue.Y - R.Top);
  finally
    FMarkerMoving := False;
  end;
end;

procedure TSVGSelectControl.SetSize(const aValue: TRect);
var
  R: TRect;
  dx, dy, dw, dh: integer;
begin
  FMarkerMoving := True;
  try
    R := AbsoluteClientRect;

    dx := aValue.Left - R.Left;
    dy := aValue.Top - R.Top;
    dw := aValue.Right - R.Right - dx;
    dh := aValue.Bottom - R.Bottom - dy;

    MoveDelta(dx, dy);
    ResizeClientRect(dw, dh);
  finally
    FMarkerMoving := False;
  end;
end;

procedure TSVGSelectControl.UpdateBoundsMarkers;
var
  BoundsMarker: TBoundsMarker;
begin
  for BoundsMarker in FBoundsMarkerList do
    BoundsMarker.UpdateBounds;
end;

//------------------------------------------------------------------------------
//
//                               TSVGAnimatedClock
//
//------------------------------------------------------------------------------

constructor TSVGAnimatedClock.Create(AOwner: TComponent;
  aSelectionList : TSVGSelectionList);
begin
  inherited;

  RenderOptions := [sroClippath, sroEvents];

  FTimer := TTimer.Create(self);
  FTimer.Interval := 1000;
  FTimer.Enabled := False;
  FTimer.OnTimer := SetTime;

  FSelectedObject := nil;
end;

destructor TSVGAnimatedClock.Destroy;
begin
  FSelectedObject := nil;
  inherited;
end;

procedure TSVGAnimatedClock.DoAfterParse;
begin
  Init;

  inherited;
end;

procedure TSVGAnimatedClock.Assign(Source: TPersistent);
begin
  inherited;

  if Source is TSVGAnimatedClock then
    Init;
end;

function TSVGAnimatedClock.GetValid: boolean;
begin
  Result := assigned(FGearSeconds)
        and assigned(FGear7_5)
        and assigned(FGearMinutes)
        and assigned(FGear180)
        and assigned(FGearHours)
        and assigned(FHandSecond)
        and assigned(FPathHandSecond)
        and assigned(FHandMinute)
        and assigned(FPathHandMinute)
        and assigned(FHandHour)
        and assigned(FPathHandHour);
end;

procedure TSVGAnimatedClock.Init;
begin
  // Set pointers to interactive/animated SVG parts

  FGearSeconds := SVGRoot.SVGObject['RotSeconds'];
  FGear7_5 := SVGRoot.SVGObject['RotGear7_5'];
  FGearMinutes := SVGRoot.SVGObject['RotMinutes'];
  FGear180 := SVGRoot.SVGObject['RotGear180'];
  FGearHours := SVGRoot.SVGObject['RotHours'];

  // The interactive SVG objects should have Hittest = TRUE otherwise they
  // are ignored by TSVGGroup.ObjectAt() or mouseevents

  FHandSecond := SVGRoot.SVGObject['RotSecondHand'];
  FPathHandSecond := SVGRoot.SVGObject['SecondHandPath'];

  FHandMinute := SVGRoot.SVGObject['RotMinuteHand'];
  FPathHandMinute := SVGRoot.SVGObject['MinuteHandPath'];

  FHandHour := SVGRoot.SVGObject['RotHourHand'];
  FPathHandHour := SVGRoot.SVGObject['HourHandPath'];

  if GetValid then
  begin
    FPathHandSecond.HitTest := True;
    FPathHandMinute.HitTest := True;
    FPathHandHour.HitTest := True;
    FTimer.Enabled := True;
  end;

  FSelectedObject := nil;
end;

procedure TSVGAnimatedClock.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  P: TPointF;
begin
  if ssLeft in Shift then
  begin
    P.X := X;
    P.Y := Y;

    FSelectedObject := ObjectAtPt(P);
    FMouseDownPoint := Point(X, Y);
  end;

  inherited;
end;

procedure TSVGAnimatedClock.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Dx, Dy: integer;
begin
  if ssLeft in Shift then
  begin

    if assigned(FSelectedObject) then
    begin
      Dx := X - FMouseDownPoint.X;
      Dy := Y - FMouseDownPoint.Y;
      FMouseDownPoint := Point(X, Y);

      ProcessMouseMove(FSelectedObject, Shift, Dx, Dy);
    end else
      inherited;

  end else
    inherited;
end;

procedure TSVGAnimatedClock.ProcessMouseMove(aSVGObject: ISVGObject;
  Shift: TShiftState; dX, dY: Integer);
begin
  if aSVGObject.ID = 'SecondHandPath' then
  begin
    AddDeltaSec(-dy / 23);
  end;

  if aSVGObject.ID = 'MinuteHandPath' then
  begin
    AddDeltaMin(-dy / 23);
  end;

  if aSVGObject.ID = 'HourHandPath' then
  begin
    AddDeltaHour(-dy / 23);
  end;
end;

procedure TSVGAnimatedClock.AddDeltaSec(const aD: TSVGFloat);
begin
  FTimer.Enabled := False;

  if FSec + aD > 60 then
  begin
    AddDeltaMin(1);
    FSec := Round(FSec + aD - 60);
  end else
    if FSec + aD < 0 then
    begin
      AddDeltaMin(-1);
      FSec := FSec + aD + 60;
    end else
      FSec := FSec + aD;

  UpdateClock;
end;

procedure TSVGAnimatedClock.AddDeltaMin(const aD: TSVGFloat);
begin
  FTimer.Enabled := False;

  if FMin + aD > 60 then
  begin
    AddDeltaHour(1);
    FMin := Round(Fmin + aD - 60);
  end else
    if FMin + aD < 0 then
    begin
      AddDeltaHour(-1);
      FMin := FMin + aD + 60;
    end else
      FMin := FMin + aD;

  UpdateClock;
end;

procedure TSVGAnimatedClock.AddDeltaHour(const aD: TSVGFloat);
begin
  FTimer.Enabled := False;

  if FHour + aD > 24 then
  begin
    FHour := Round(FHour + aD - 24);
  end else
    if FHour + aD < 0 then
    begin
      FHour := FHour + aD + 24;
    end else
      FHour := FHour + aD;

  UpdateClock;
end;

procedure TSVGAnimatedClock.UpdateClock;
var
  HandSec, Gear7_5, HandMin, Gear180, HandHour: TSVGFloat;
  TotalSec: TSVGFloat;
begin
  if not GetValid then
    Exit;

  TotalSec := FMin * 60 + FHour * 3600 + FSec;

  HandSec := (TotalSec * 6) - 360 * Round(TotalSec * 6 / 360);
  Gear7_5 := -((TotalSec * 6 / 7.5) - 360 * Round(TotalSec * 6 / 7.5 / 360));
  HandMin := (TotalSec * 6 /  60) - 360 * Round(TotalSec * 6 /  60 / 360);
  Gear180 := -((TotalSec * 6 / 180) - 360 * Round(TotalSec * 6 / 180 / 360));
  HandHour := (TotalSec * 6 / 720) - 360 * Round(TotalSec * 6 / 720 / 360);

  SetLayoutAngle(FGearSeconds, HandSec);
  SetLayoutAngle(FGear7_5, Gear7_5);
  SetLayoutAngle(FGearMinutes, HandMin);
  SetLayoutAngle(FGear180, Gear180);
  SetLayoutAngle(FGearHours, HandHour);
  SetLayoutAngle(FHandSecond, HandSec);
  SetLayoutAngle(FHandMinute, HandMin);
  SetLayoutAngle(FHandHour, HandHour);

  Repaint;
end;

procedure TSVGAnimatedClock.SetLayoutAngle(aLayout: ISVGObject; aAngle: TSVGFloat);
begin
  aLayout.Attributes['transform'] := Format('rotate(%f)', [aAngle], USFormatSettings);
end;

procedure TSVGAnimatedClock.SetTime(Sender: TObject);
var
  H, M, S, Ms: Word;
begin
  DecodeTime(Now, H, M, S, Ms);

  FHour := H;
  FMin := M;
  FSec := S;

  UpdateClock;
end;

// -----------------------------------------------------------------------------
//
//                             TSVGViewerForm
//
// -----------------------------------------------------------------------------

procedure TSVGViewerForm.ActionAboutExecute(Sender: TObject);
begin
  FormAbout.ShowModal;
end;

procedure TSVGViewerForm.ActionAnimationPauseExecute(Sender: TObject);
begin
  ActionAnimationPause.Checked := not ActionAnimationPause.Checked;

  if assigned(CurrSelectControl) then
    CurrSelectControl.AnimationIsPaused := ActionAnimationPause.Checked;

  UpdateControls;
end;

procedure TSVGViewerForm.ActionAnimationStartExecute(Sender: TObject);
begin
  ActionAnimationStart.Checked := not ActionAnimationStart.Checked;

  if assigned(CurrSelectControl) then
    CurrSelectControl.AnimationIsStarted := ActionAnimationStart.Checked;

  UpdateControls;
end;

procedure TSVGViewerForm.ActionAutoViewBoxExecute(Sender: TObject);
begin
  AutoViewbox := not AutoViewBox;
  UpdateControls;
end;

procedure TSVGViewerForm.ActionCopyDirectExecute(Sender: TObject);
begin
  CopyDirect;
end;

procedure TSVGViewerForm.ActionCopyExecute(Sender: TObject);
begin
  Copy;
end;

procedure TSVGViewerForm.ActionEditExecute(Sender: TObject);
begin
  FormProperties.Show;
end;

procedure TSVGViewerForm.ActionEnableClippathsExecute(Sender: TObject);
begin
  ActionEnableClippaths.Checked := not ActionEnableClippaths.Checked;

  if assigned(CurrSelectControl) then
  begin
    if ActionEnableClippaths.Checked then
      CurrSelectControl.RenderOptions := CurrSelectControl.RenderOptions + [sroClippath]
    else
      CurrSelectControl.RenderOptions := CurrSelectControl.RenderOptions - [sroClippath];
  end;

  UpdateControls;
end;

procedure TSVGViewerForm.ActionEnableEventsExecute(Sender: TObject);
begin
  ActionEnableEvents.Checked := not ActionEnableEvents.Checked;

  if assigned(CurrSelectControl) then
  begin
    if ActionEnableEvents.Checked then
      CurrSelectControl.RenderOptions := CurrSelectControl.RenderOptions + [sroEvents]
    else
      CurrSelectControl.RenderOptions := CurrSelectControl.RenderOptions - [sroEvents];
  end;

  UpdateControls;
end;

procedure TSVGViewerForm.ActionEnableFiltersExecute(Sender: TObject);
begin
  ActionEnableFilters.Checked := not ActionEnableFilters.Checked;

  if assigned(CurrSelectControl) then
  begin
    if ActionEnableFilters.Checked then
      CurrSelectControl.RenderOptions := CurrSelectControl.RenderOptions + [sroFilters]
    else
      CurrSelectControl.RenderOptions := CurrSelectControl.RenderOptions - [sroFilters];
  end;

  UpdateControls;
end;

procedure TSVGViewerForm.ActionEnablePersistentBuffersExecute(Sender: TObject);
begin
  ActionEnablePersistentBuffers.Checked := not ActionEnablePersistentBuffers.Checked;

  if assigned(CurrSelectControl) then
  begin
    if ActionEnablePersistentBuffers.Checked then
      CurrSelectControl.RenderOptions := CurrSelectControl.RenderOptions + [sroPersistentBuffers]
    else
      CurrSelectControl.RenderOptions := CurrSelectControl.RenderOptions - [sroPersistentBuffers];
  end;

  UpdateControls;
end;

procedure TSVGViewerForm.ActionEnableTextToPathExecute(Sender: TObject);
begin
  ActionEnableTextToPath.Checked := not ActionEnableTextToPath.Checked;

  if assigned(CurrSelectControl) then
  begin
    if ActionEnableTextToPath.Checked then
      CurrSelectControl.RenderOptions := CurrSelectControl.RenderOptions + [sroTextToPath]
    else
      CurrSelectControl.RenderOptions := CurrSelectControl.RenderOptions - [sroTextToPath];
  end;

  UpdateControls;
end;

procedure TSVGViewerForm.ActionExitExecute(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TSVGViewerForm.ActionExportExecute(Sender: TObject);
begin
  ExportSVG;
end;

procedure TSVGViewerForm.ActionNewExecute(Sender: TObject);
begin
  NewControl;
end;

procedure TSVGViewerForm.ActionOpenExecute(Sender: TObject);
begin
  if OpenDialog.Execute then
    LoadSVG(OpenDialog.FileName);
end;

procedure TSVGViewerForm.ActionPasteExecute(Sender: TObject);
begin
  Paste;
end;

procedure TSVGViewerForm.ActionRemoveExecute(Sender: TObject);
begin
  RemoveSVGControl;
end;

procedure TSVGViewerForm.AddSVGControl(aControl: TSVGSelectControl);
begin
  aControl.Parent := FScrollBox;
  aControl.OnMouseUp := SVGSelectMouseUp;
  aControl.OnSVGEvent := SVGEvent;
  aControl.OnAfterParse := AfterParse;

{$IFDEF Ver300Down}
  aControl.Scale := PixelsPerInch / 96;
{$ENDIF}

  CurrSelectControl := aControl;
end;

{$IFDEF Ver290Down}
procedure TSVGViewerForm.WMDpiChanged(var Message: TMessage);
begin
  BeforeMonitorDpiChanged(Self, PixelsPerInch, LOWORD(Message.wParam));
  ChangeScale(LOWORD(Message.wParam), PixelsPerInch);
  PixelsPerInch :=  LOWORD(Message.wParam);
end;
{$ENDIF}

procedure TSVGViewerForm.BeforeMonitorDpiChanged(Sender: TObject; OldDPI,
  NewDPI: Integer);
var
  ImageList: TSVG2BaseImageList;
begin
  for ImageList in FImageListList do
    ImageListScale(ImageList, NewDPI, OldDPI);
end;

procedure TSVGViewerForm.AfterParse(Sender: TObject);
var
  Control: TSVG2CustomControl;
begin
  if Sender is TSVG2CustomControl then
  begin
    Control := Sender as TSVG2CustomControl;

    Control.SVGRoot.CheckEventTypes;

    if Control.SVGRoot.EventTypeSet <> [] then
    begin
      Control.RenderOptions := Control.RenderOptions + [sroEvents];
    end;

    UpdateControls;
  end;
end;

function TSVGViewerForm.CheckClipboard: boolean;
begin
  Result := False;
  if Clipboard.HasFormat(CF_TEXT)
{$IFnDEF FPC}
  or Clipboard.HasFormat(CF_UNICODETEXT)
{$ENDIF}
  then
  begin
    try
      CheckValidSVG(Clipboard.AsText);
      Result := True;

      except on E:Exception do
        Result := False;
    end;
  end;
end;

procedure TSVGViewerForm.CheckValidSVG(const aSvg: string);
var
  SaxParser: TSVGSaxParser;
  SVGRoot: ISVGRoot;
  sl: TStringList;
begin
  SaxParser := TSVGSaxParser.Create(nil);
  sl := TStringList.Create;
  try
    sl.Text := aSVG;
{$IFnDEF FPC}
    SVGRoot := TSVGRootVcl.Create;
{$ELSE}
    SVGRoot := TSVGRootFpc.Create;
{$ENDIF}
    SaxParser.Parse(sl, SVGRoot);
  finally
    sl.Free;
    SaxParser.Free;
  end;
end;

procedure TSVGViewerForm.ConnectControls(
  aActionOpen: TAction;
  aActionEdit: TAction;
  aActionCopy: TAction;
  aActionNew: TAction;
  aActionPaste: TAction;
  aActionCopyDirect: TAction;
  aActionAbout: TAction;
  aActionRemove: TAction;
  aActionAutoViewBox: TAction;
  aActionExport: TAction;
  aActionEnableFilters: TAction;
  aActionEnableClippaths: TAction;
  aActionEnableEvents: TAction;
  aActionEnablePersistentBuffers: TAction;
  aActionEnableTextToPath: TAction;
  aActionAnimationStart: TAction;
  aActionAnimationPause: TAction;
  aOpenDialog: TOpenDialog;
  aScrollBox: TScrollBox;
  aStatusBar: TStatusBar;
  aTrackBar: TTrackBar;
  aLabelTime: TLabel);
begin
  ActionOpen := aActionOpen;
  ActionEdit := aActionEdit;
  ActionCopy := aActionCopy;
  ActionNew := aActionNew;
  ActionPaste := aActionPaste;
  ActionCopyDirect := aActionCopyDirect;
  ActionAbout := aActionAbout;
  ActionRemove := aActionRemove;
  ActionAutoViewBox := aActionAutoViewBox;
  ActionExport := aActionExport;
  ActionEnableFilters := aActionEnableFilters;
  ActionEnableClippaths := aActionEnableClippaths;
  ActionEnableEvents := aActionEnableEvents;
  ActionEnablePersistentBuffers := aActionEnablePersistentBuffers;
  ActionEnableTextToPath := aActionEnableTextToPath;
  ActionAnimationStart := aActionAnimationStart;
  ActionAnimationPause := aActionAnimationPause;
  TrackBar := aTrackBar;
  OpenDialog := aOpenDialog;
  ScrollBox := aScrollbox;
  StatusBar := aStatusBar;
  LabelTime := aLabelTime;
end;

procedure TSVGViewerForm.Copy;
var
  sl: TStringList;
begin
  // Makes a copy of the SVG text and puts it on the clipboard

  if not assigned(CurrSelectControl) then
    Exit;

  if CurrSelectControl.SVG.Count = 0 then
  begin
    sl := TStringList.Create;
    try
      sl.LoadFromFile(CurrSelectControl.FileName);
      Clipboard.AsText := sl.Text;
    finally
      sl.Free;
    end;
  end else
    Clipboard.AsText := CurrSelectControl.SVG.Text;
end;

procedure TSVGViewerForm.CopyDirect;
begin
  // Copies the rendering tree directly

  if not assigned(CurrSelectControl) then
    Exit;

  FSelectionList.UnselectAll;

  AddSVGControl(CurrSelectControl.CreateCopy(FSelectionList));
end;

constructor TSVGViewerForm.Create(AOwner: TComponent);
begin
  inherited;

  FImageListList := TList<TSVG2BaseImageList>.Create;
  FSelectionList := TSVGSelectionList.Create;
end;

function TSVGViewerForm.CreateSVGSelectControl(
  const aSvg: string): TSVGSelectControl;
begin
  Result := TSVGSelectControl.Create(Self, FSelectionList);
  Result.SVG.Text := aSvg;
end;

destructor TSVGViewerForm.Destroy;
begin
  FSelectionList.Free;
  FImageListList.Free;

  inherited;
end;

procedure TSVGViewerForm.DoCreate;
begin
  Application.OnHint := Hint;
  {$IFnDef FPC}
  AddClipboardFormatListener(Handle);
  {$ENDIF}

  inherited;

  {$IFDEF Ver300Up}
  OnBeforeMonitorDPIChanged := BeforeMonitorDpiChanged;
  {$ENDIF}

  {$IFnDEF FPC}
  DragAcceptFiles(Handle, True);
  {$ELSE}
  Application.AddOnDropFilesHandler(OnDropFiles);
  {$ENDIF}
end;

procedure TSVGViewerForm.DoDestroy;
begin
  {$IFDEF FPC}
  Application.RemoveOnDropFilesHandler(OnDropFiles);
  {$ENDIF}

  inherited;

  {$IFnDEF FPC}
  RemoveClipboardFormatListener(Handle);
  {$ENDIF}
end;

procedure TSVGViewerForm.DoShow;
begin
  inherited;

  UpdateControls;
  ActionPaste.Enabled := CheckClipboard;
end;

procedure TSVGViewerForm.DoSVGEvent(aEvent: ISVGEvent; const aValue: string);
begin
  StatusBar.SimpleText := aValue;
end;

procedure TSVGViewerForm.DoTimer(aSender: TObject);
begin
  if not assigned(CurrSelectControl) then
    Exit;

  LabelTime.Caption := Format(' Time: %5.1f   FPS: %3.0f ',
    [CurrSelectControl.AnimationTime / 1000, CurrSelectControl.AnimationFPS]);

  if CurrSelectControl.AnimationTime > Trackbar.Max then
    Trackbar.Max := Trackbar.Max + 5000;

  if not CurrSelectControl.AnimationIsPaused then
    Trackbar.Position := CurrSelectControl.AnimationTime;
end;

procedure TSVGViewerForm.ExportSVG;
var
  Bitmap: TBitmap;
  SaveDialog: TSavePictureDialog;
begin
  if FSelectionList.Count = 0 then
    Exit;

  SaveDialog := TSavePictureDialog.Create(Self);
  try
    SaveDialog.FileName := ChangeFileExt(
      ExtractFilename(CurrSelectControl.FileName),
      SaveDialog.DefaultExt);

    if SaveDialog.Execute then
    begin
      if Lowercase(ExtractFileExt(SaveDialog.FileName)) = '.svg' then
      begin
        CurrSelectControl.SVGRoot.Doc.SaveToFile(SaveDialog.FileName);
      end else begin
        Bitmap := RenderToBitmap;
        if assigned(bitmap) then
          try
            if Lowercase(ExtractFileExt(SaveDialog.FileName)) = '.bmp' then
              Bitmap.SaveToFile(SaveDialog.FileName);

            if Lowercase(ExtractFileExt(SaveDialog.FileName)) = '.jpg' then
              SaveBitmapAsJpg(Bitmap, SaveDialog.FileName);

            if Lowercase(ExtractFileExt(SaveDialog.FileName)) = '.png' then
              SaveBitmapAsPng(Bitmap, SaveDialog.FileName);
          finally
            Bitmap.Free;
          end;
      end;
    end;
  finally
    SaveDialog.Free;
  end;
end;

function TSVGViewerForm.GetAutoViewBox: boolean;
begin
  if not assigned(CurrSelectControl) then
    Result := False
  else
    Result := CurrSelectControl.AutoViewbox;
end;

function TSVGViewerForm.GetCurrSelectControl: TSVGSelectControl;
begin
  Result := FormProperties.SelectControl;
end;

function TSVGViewerForm.GetFilters: boolean;
begin
  if not assigned(CurrSelectControl) then
    Result := False
  else
    Result := sroFilters in CurrSelectControl.RenderOptions;
end;

procedure TSVGViewerForm.Hint(Sender: TObject);
begin
  Statusbar.SimpleText := Application.Hint;
end;

procedure TSVGViewerForm.ImageListScale(aImageList: TSVG2BaseImageList; M,
  D: Integer);
begin
  {$IFnDEF FPC}
  aImageList.ChangeScale(M,D);
  {$ENDIF}
end;

procedure TSVGViewerForm.ImageListTheme(aImageList: TSVG2BaseImageList);
begin
  {$IFnDEF FPC}
  if not StyleServices.Enabled then
  begin
    aImageList.BeginUpdate;
    try
      aImageList.ClearColor := clBtnFace;
      aImageList.ColorDepth := cd24Bit;
    finally
      aImageList.EndUpdate
    end;
  end else begin
    aImageList.BeginUpdate;
    try
      aImageList.ClearColor := clNone;
      aImageList.ColorDepth := cd32Bit;
    finally
      aImageList.EndUpdate
    end;
  end;
  {$ELSE}
  aImageList.BeginUpdate;
  try
    aImageList.ClearColor := clNone;
  finally
    aImageList.EndUpdate
  end;
  {$ENDIF}
end;

procedure TSVGViewerForm.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;

  if Key = VK_DELETE then
    RemoveSVGControl;
end;

procedure TSVGViewerForm.LoadSVG(const aFileName: string);
begin
  FSelectionList.UnselectAll;

  AddSVGControl(OpenSVGSelectControl(aFileName));
end;

procedure TSVGViewerForm.NewControl;
begin
  AddSVGControl(NewSVGSelectControl);
end;

function TSVGViewerForm.NewSVGSelectControl: TSVGSelectControl;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Add('<?xml version="1.0" encoding="UTF-8" standalone="no"?>');
    sl.Add('<svg version="1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink= "http://www.w3.org/1999/xlink">');
    sl.Add('</svg>');
    Result := TSVGSelectControl.Create(Self, FSelectionList);
    Result.SVG := sl;
  finally
    sl.Free;
  end;
end;

function TSVGViewerForm.OpenSVGSelectControl(
  const aFilename: string): TSVGSelectControl;
begin
  if Lowercase(ExtractFileName(aFileName)) = 'animated-clock.svg' then
    Result := TSVGAnimatedClock.Create(Self, FSelectionList)
  else
    Result := TSVGSelectControl.Create(Self, FSelectionList);

  Result.Filename := aFileName;
end;

procedure TSVGViewerForm.Paste;
begin
  FSelectionList.UnselectAll;

  AddSVGControl(CreateSVGSelectControl(Clipboard.AsText));
end;

procedure TSVGViewerForm.RegisterImageList(aImageList: TSVG2BaseImageList);
begin
  if assigned(aImageList) then
  begin
    FImageListList.Add(aImageList);

    {$IFDEF Ver310Up}
    // Berlin
    ImageListScale(aImageList, Monitor.PixelsPerInch, GetDesignDPI);
    {$ELSE}
    {$IFDEF Ver300Down}
    // Seattle
    ImageListScale(aImageList, PixelsPerInch, 96);
    {$ENDIF}
    {$ENDIF}
    ImageListTheme(aImageList);
  end;
end;

procedure TSVGViewerForm.RemoveSVGControl;
var
  SelectControl: TSVGSelectControl;
begin
  if assigned(CurrSelectControl) then
  begin
    SelectControl := CurrSelectControl;
    CurrSelectControl := nil;

    SelectControl.Parent := nil;
    SelectControl.Free;
    FSelectionList.UnselectAll;
  end;
end;

function TSVGViewerForm.RenderToBitmap: TBitmap;
var
  SelectControl: TSVGSelectControl;
  RenderContext: ISVGRenderContext;
  R: TRect;
  SaveMatrix: TSVGMatrix;
  {$IFDEF FPC}
  IntfBitmap: ISVGIntfBitmap;
  {$ENDIF}
begin
  Result := nil;

  R := Rect(0, 0, 0, 0);
  for SelectControl in FSelectionList do
    if SelectControl.Selected then
    begin
      R := TRect.Union(
        Rect(
          SelectControl.Left + SelectControl.Padding.Left,
          SelectControl.Top + SelectControl.Padding.Top,
          SelectControl.Left + SelectControl.Width - SelectControl.Padding.Right,
          SelectControl.Top + SelectControl.Height - SelectControl.Padding.Bottom),
        R);
    end;

  if (R.Width = 0) or (R.Height = 0) then
    Exit;

  {$IFDEF FPC}
  IntfBitmap := SVGCreateIntfBitmap(R.Width, R.Height);
  RenderContext := TSVGRenderContextManager.CreateRenderContextBitmap(IntfBitmap);
  {$ELSE}
  Result := TSVGRenderContextManager.CreateCompatibleBitmap(R.Width, R.Height, False);
  RenderContext := TSVGRenderContextManager.CreateRenderContextBitmap(Result);
  {$ENDIF}

  RenderContext.BeginScene;
  try
    RenderContext.Clear(0);

    for SelectControl in FSelectionList do
      if SelectControl.Selected then
      begin
        SaveMatrix := RenderContext.Matrix;
        try
          RenderContext.MultiplyMatrix(
            TSVGMatrix.CreateTranslation(
             SelectControl.Left + SelectControl.Padding.Left - R.Left,
             SelectControl.Top + SelectControl.Padding.Top - R.Top
            ));

          SVGRenderToRenderContext(
            SelectControl.SVGRoot,
            RenderContext,
            SelectControl.Width - SelectControl.Padding.Left - SelectControl.Padding.Right,
            SelectControl.Height - SelectControl.Padding.Top - SelectControl.Padding.Bottom,
            SelectControl.RenderOptions,
            SelectControl.AutoViewbox);

        finally
          RenderContext.Matrix := SaveMatrix;
        end;
      end;

   finally
     RenderContext.EndScene;
   end;

  {$IFDEF FPC}
  Result := IntfBitmap.CreateBitmap;
  {$ENDIF}
end;

procedure TSVGViewerForm.ScrollBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FSelectionList.UnselectAll;
  CurrSelectControl := nil;
end;

procedure TSVGViewerForm.SeActionEnableFilters(const Value: TAction);
begin
  FActionEnableFilters := Value;
  if assigned(FActionEnableFilters) then
    FActionEnableFilters.OnExecute := ActionEnableFiltersExecute;
end;

procedure TSVGViewerForm.SetActionAbout(const Value: TAction);
begin
  FActionAbout := Value;
  if assigned(FActionAbout) then
    FActionAbout.OnExecute := ActionAboutExecute;
end;

procedure TSVGViewerForm.SetActionAnimationPause(const Value: TAction);
begin
  FActionAnimationPause := Value;
  if assigned(FActionAnimationPause) then
    FActionAnimationPause.OnExecute := ActionAnimationPauseExecute;
end;

procedure TSVGViewerForm.SetActionAnimationStart(const Value: TAction);
begin
  FActionAnimationStart := Value;
  if assigned(FActionAnimationStart) then
    FActionAnimationStart.OnExecute := ActionAnimationStartExecute;
end;

procedure TSVGViewerForm.SetActionAutoViewBox(const Value: TAction);
begin
  FActionAutoViewBox := Value;
  if assigned(FActionAutoViewBox) then
    FActionAutoViewBox.OnExecute := ActionAutoViewBoxExecute;
end;

procedure TSVGViewerForm.SetActionCopy(const Value: TAction);
begin
  FActionCopy := Value;
  if assigned(FActionCopy) then
    FActionCopy.OnExecute := ActionCopyExecute;
end;

procedure TSVGViewerForm.SetActionCopyDirect(const Value: TAction);
begin
  FActionCopyDirect := Value;
  if assigned(FActionCopyDirect) then
    FActionCopyDirect.OnExecute := ActionCopyDirectExecute;
end;

procedure TSVGViewerForm.SetActionEdit(const Value: TAction);
begin
  FActionEdit := Value;
  if assigned(FActionEdit) then
    FActionEdit.OnExecute := ActionEditExecute;
end;

procedure TSVGViewerForm.SetActionEnableClippaths(const Value: TAction);
begin
  FActionEnableClippaths := Value;
  if assigned(FActionEnableClippaths) then
    FActionEnableClippaths.OnExecute := ActionEnableClippathsExecute;
end;

procedure TSVGViewerForm.SetActionEnableEvents(const Value: TAction);
begin
  FActionEnableEvents := Value;
  if assigned(FActionEnableEvents) then
    FActionEnableEvents.OnExecute := ActionEnableEventsExecute;
end;

procedure TSVGViewerForm.SetActionEnableFilters(const Value: TAction);
begin
  FActionEnableFilters := Value;
  if assigned(FActionEnableFilters) then
    FActionEnableFilters.OnExecute := ActionEnableFiltersExecute;
end;

procedure TSVGViewerForm.SetActionEnablePersistentBuffers(const Value: TAction);
begin
  FActionEnablePersistentBuffers := Value;
  if assigned(FActionEnablePersistentBuffers) then
    FActionEnablePersistentBuffers.OnExecute := ActionEnablePersistentBuffersExecute;
end;

procedure TSVGViewerForm.SetActionEnableTextToPath(const Value: TAction);
begin
  FActionEnableTextToPath := Value;
  if assigned(FActionEnableTextToPath) then
    FActionEnableTextToPath.OnExecute := ActionEnableTextToPathExecute;
end;

procedure TSVGViewerForm.SetActionExport(const Value: TAction);
begin
  FActionExport := Value;
  if assigned(FActionExport) then
    FActionExport.OnExecute := ActionExportExecute;
end;

procedure TSVGViewerForm.SetActionNew(const Value: TAction);
begin
  FActionNew := Value;
  if assigned(FActionNew) then
    FActionNew.OnExecute := ActionNewExecute;
end;

procedure TSVGViewerForm.SetActionOpen(const Value: TAction);
begin
  FActionOpen := Value;
  if assigned(FActionOpen) then
    FActionOpen.OnExecute := ActionOpenExecute;
end;

procedure TSVGViewerForm.SetActionPaste(const Value: TAction);
begin
  FActionPaste := Value;
  if assigned(FActionPaste) then
    FActionPaste.OnExecute := ActionPasteExecute;
end;

procedure TSVGViewerForm.SetActionRemove(const Value: TAction);
begin
  FActionRemove := Value;
  if assigned(FActionRemove) then
    FActionRemove.OnExecute := ActionRemoveExecute;
end;

procedure TSVGViewerForm.SetAutoViewBox(const Value: boolean);
begin
  if not assigned(CurrSelectControl) then
    Exit;

  CurrSelectControl.AutoViewbox := Value;
end;

procedure TSVGViewerForm.SetCurrSelectControl(const Value: TSVGSelectControl);
begin
  if FormProperties.SelectControl <> Value then
  begin
    if assigned(FormProperties.SelectControl) then
      FormProperties.SelectControl.OnAnimationSample := nil;

    FormProperties.SelectControl := Value;

    if assigned(FormProperties.SelectControl) then
      FormProperties.SelectControl.OnAnimationSample := DoTimer;

    UpdateControls;
  end;
end;

procedure TSVGViewerForm.SetFilters(const Value: boolean);
begin
  if not assigned(CurrSelectControl) then
    Exit;

  if Value then
    CurrSelectControl.RenderOptions :=
      CurrSelectControl.RenderOptions + [sroFilters]
  else
    CurrSelectControl.RenderOptions :=
      CurrSelectControl.RenderOptions - [sroFilters];
end;

procedure TSVGViewerForm.SetFormAbout(const Value: TSVGViewerAboutForm);
begin
  FFormAbout := Value;
end;

procedure TSVGViewerForm.SetFormProperties(
  const Value: TSVGViewerPropertiesForm);
begin
  FFormProperties := Value;
end;

procedure TSVGViewerForm.SetLabelTime(const Value: TLabel);
begin
  FLabelTime := Value;
end;

procedure TSVGViewerForm.SetOpenDialog(const Value: TOpenDialog);
begin
  FOpenDialog := Value;
end;

procedure TSVGViewerForm.SetScrollBox(const Value: TScrollBox);
begin
  FScrollBox := Value;

  if assigned(FScrollBox) then
    FScrollBox.OnMouseUp := ScrollBoxMouseUp;
end;

procedure TSVGViewerForm.SetStatusBar(const Value: TStatusBar);
begin
  FStatusBar := Value;
end;

procedure TSVGViewerForm.SetTrackBar(const Value: TTrackbar);
begin
  FTrackBar := Value;

  if assigned(FTrackBar) then
    FTrackBar.OnChange := TrackBarChange;
end;

procedure TSVGViewerForm.SVGEvent(Sender: TObject; aSVGRoot: ISVGRoot;
  aEvent: ISVGEvent; const aValue: string);
begin
  DoSVGEvent(aEvent, aValue);
end;

procedure TSVGViewerForm.SVGSelectMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  CurrSelectControl := Sender as TSVGSelectControl;
end;

procedure TSVGViewerForm.TrackBarChange(Sender: TObject);
begin
  if assigned(CurrSelectControl) then
  begin
    if CurrSelectControl.AnimationIsPaused then
    begin
      CurrSelectControl.AnimationTime := FTrackbar.Position;
      DoTimer(Self);
    end;
  end;
end;

procedure TSVGViewerForm.UpdateControls;
var
  HasAnimations: Boolean;
begin
  HasAnimations := False;

  ActionEdit.Enabled := True;
  ActionCopy.Enabled := True;
  ActionCopyDirect.Enabled := True;
  ActionEnableFilters.Enabled := True;
  ActionEnableClippaths.Enabled := True;
  ActionEnableEvents.Enabled := True;
  ActionEnablePersistentBuffers.Enabled := True;
  ActionEnableTextToPath.Enabled := True;
  ActionAutoViewbox.Enabled := True;
  ActionRemove.Enabled := True;
  ActionAnimationStart.Enabled := True;
  ActionAnimationPause.Enabled := True;
  Trackbar.Enabled := True;

  if assigned(CurrSelectControl) then
  begin
    ActionEnableFilters.Checked := sroFilters in CurrSelectControl.RenderOptions;
    ActionEnableClippaths.Checked := sroClippath in CurrSelectControl.RenderOptions;
    ActionEnableEvents.Checked := sroEvents in CurrSelectControl.RenderOptions;
    ActionEnablePersistentBuffers.Checked := sroPersistentBuffers in CurrSelectControl.RenderOptions;
    ActionEnableTextToPath.Checked := sroTextToPath in CurrSelectControl.RenderOptions;
    ActionAutoViewbox.Checked := CurrSelectControl.AutoViewbox;

    HasAnimations := CurrSelectControl.HasAnimations;

    if HasAnimations then
    begin
      ActionAnimationStart.Checked := CurrSelectControl.AnimationIsStarted;
      ActionAnimationPause.Checked := CurrSelectControl.AnimationIsPaused;
    end else begin
      ActionAnimationStart.Checked := False;
      ActionAnimationPause.Checked := False;
    end;

  end else begin
    ActionEnableFilters.Checked := False;
    ActionEnableClippaths.Checked := False;
    ActionEnableEvents.Checked := False;
    ActionEnablePersistentBuffers.Checked := False;
    ActionEnableTextToPath.Checked := False;
    ActionAutoViewbox.Checked := False;
    ActionAnimationStart.Checked := False;
    ActionAnimationPause.Checked := False;

    ActionEdit.Enabled := False;
    ActionCopy.Enabled := False;
    ActionCopyDirect.Enabled := False;
    ActionEnableFilters.Enabled := False;
    ActionEnableClippaths.Enabled := False;
    ActionEnableEvents.Enabled := False;
    ActionEnablePersistentBuffers.Enabled := False;
    ActionEnableTextToPath.Enabled := False;
    ActionAutoViewbox.Enabled := False;
    ActionRemove.Enabled := False;
  end;

  ActionAnimationStart.Enabled := HasAnimations;
  ActionAnimationPause.Enabled := HasAnimations;
  Trackbar.Enabled := HasAnimations;

  ActionExport.Enabled := SelectionList.SelectedCount > 0
end;

{$IFnDEF FPC}
procedure TSVGViewerForm.WMClipboardUpdate(var Msg: TMessage);
begin
  ActionPaste.Enabled := CheckClipboard;
end;

procedure TSVGViewerForm.WMDROPFILES(var Msg: TWMDropFiles);
var
  i, FileCount: integer;
  l: integer;
  FileName: string;
begin
  FileCount := DragQueryFile(Msg.Drop, $FFFFFFFF, nil, 0);

  for i := 0 to FileCount - 1 do
  begin
    l := DragQueryFile(Msg.Drop, i, nil, 0);
    SetLength(Filename, l);
    DragQueryFile(Msg.Drop, i, PChar(FileName), l + 1);

    LoadSVG(FileName);
  end;

  DragFinish(Msg.Drop);
end;
{$ELSE}
procedure TSVGViewerForm.OnDropFiles(Sender: TObject; const FileNames: array of String);
var
  i, FileCount: integer;
begin
  FileCount := Length(FileNames);

  for i := 0 to FileCount - 1 do
    LoadSVG(FileNames[i]);
end;
{$ENDIF}

// -----------------------------------------------------------------------------
//
//                        TSVGViewerPropertiesForm
//
// -----------------------------------------------------------------------------

{$IFnDEF FPC}
procedure TSVGViewerPropertiesForm.ActionAttrChangeExecute(Sender: TObject);
begin
  AttrChange;
end;
{$ENDIF}

procedure TSVGViewerPropertiesForm.ActionParseExecute(Sender: TObject);
begin
  Parse;
end;

procedure TSVGViewerPropertiesForm.ApplyText;
begin
  if assigned(FSelectControl) then
  begin
    {$IFnDEF FPC}
    FDoc.XML := MemoSVG.Lines;
    {$ENDIF}
    FSelectControl.SVG := MemoSVG.Lines;
    UpdateAll;
  end;
end;

{$IFnDEF FPC}
procedure TSVGViewerPropertiesForm.AttrChange;
begin
  AttributeValueApply(TreeView, ListView, MemoAttrValue.Lines);
  MemoSVG.Lines.Assign(FDoc.XML);
  FSelectControl.SVG := FDoc.XML;
end;
{$ENDIF}

procedure TSVGViewerPropertiesForm.CheckboxAutoViewboxClick(Sender: TObject);
begin
  if assigned(FSelectControl) then
  begin
    FSelectControl.AutoViewbox := CheckboxAutoViewbox.Checked;
    FSelectControl.Repaint;
  end;
end;

procedure TSVGViewerPropertiesForm.CheckboxClippathsClick(Sender: TObject);
begin
  if assigned(FSelectControl) then
  begin
    if CheckboxClippaths.Checked then
      FSelectControl.RenderOptions := FSelectControl.RenderOptions + [sroClippath]
    else
      FSelectControl.RenderOptions := FSelectControl.RenderOptions - [sroClippath];
    FSelectControl.Repaint;
  end;
end;

procedure TSVGViewerPropertiesForm.CheckboxEventsClick(Sender: TObject);
begin
  if assigned(FSelectControl) then
  begin
    if CheckboxEvents.Checked then
      FSelectControl.RenderOptions := FSelectControl.RenderOptions + [sroEvents]
    else
      FSelectControl.RenderOptions := FSelectControl.RenderOptions - [sroEvents];
    FSelectControl.Repaint;
  end;
end;

procedure TSVGViewerPropertiesForm.CheckboxFiltersClick(Sender: TObject);
begin
  if assigned(FSelectControl) then
  begin
    if CheckboxFilters.Checked then
      FSelectControl.RenderOptions := FSelectControl.RenderOptions + [sroFilters]
    else
      FSelectControl.RenderOptions := FSelectControl.RenderOptions - [sroFilters];
    FSelectControl.Repaint;
  end;
end;

{$IFDEF FPC}
procedure TSVGViewerPropertiesForm.EditWidthExit(Sender: TObject);
var
  W: integer;
begin
  if not assigned(FSelectControl) then
    Exit;

  if TryStrToInt(FEditWidth.Text, W) then
  begin
    W := W + FSelectControl.Padding.Top + FSelectControl.Padding.Bottom;
    FSelectControl.Width := W;
  end;
end;
{$ENDIF}

{$IFDEF FPC}
procedure TSVGViewerPropertiesForm.EditHeightExit(Sender: TObject);
var
  H: integer;
begin
  if not assigned(FSelectControl) then
    Exit;

  if TryStrToInt(FEditHeight.Text, H) then
  begin
    H := H + FSelectControl.Padding.Left + FSelectControl.Padding.Right;
    FSelectControl.Height := H;
  end;
end;
{$ENDIF}

procedure TSVGViewerPropertiesForm.ConnectControls(
  {$IFnDEF FPC}
  aActionAttrChange: TAction;
  aMemoAttrValue: TMemo;
  aListView: TListView;
  aTreeView: TTreeView;
  {$ELSE}
  aEditWidth: TEdit;
  aEditHeight: TEdit;
  {$ENDIF}
  aActionParse: TAction;
  aMemoInfo,
  aMemoSVG: TMemo;
  aPanelInfo: TPanel;
  aCheckboxFilters,
  aCheckboxAutoViewbox,
  aCheckboxEvents,
  aCheckboxClippaths: TCheckbox);
begin
  {$IFnDEF FPC}
  ActionAttrChange := aActionAttrChange;
  MemoAttrValue := aMemoAttrValue;
  ListView := aListView;
  TreeView := aTreeView;
  {$ELSE}
  EditWidth := aEditWidth;
  EditHeight := aEditHeight;
  {$ENDIF}
  ActionParse := aActionParse;
  MemoInfo := aMemoInfo;
  MemoSVG := aMemoSVG;
  PanelInfo := aPanelInfo;
  CheckboxFilters := aCheckboxFilters;
  CheckboxAutoViewbox := aCheckboxAutoViewbox;
  CheckboxEvents := aCheckboxEvents;
  CheckboxClippaths := aCheckboxClippaths;
end;

constructor TSVGViewerPropertiesForm.Create(AOwnder: TComponent);
begin
  inherited;

  {$IFnDEF FPC}
  FDoc := TSVG2Doc.Create(self);
  {$ENDIF}
end;

destructor TSVGViewerPropertiesForm.Destroy;
begin
  inherited;
end;

procedure TSVGViewerPropertiesForm.DoShow;
begin
  inherited;

  if assigned(FSelectControl) then
    UpdateAll;
end;

{$IFnDEF FPC}
procedure TSVGViewerPropertiesForm.ListViewClick(Sender: TObject);
begin
  AttributeValueUpdate(ListView, MemoAttrValue.Lines);
end;
{$ENDIF}

procedure TSVGViewerPropertiesForm.MemoSVGChange(Sender: TObject);
begin
  FTextChanged := True;
end;

procedure TSVGViewerPropertiesForm.MemoSVGExit(Sender: TObject);
begin
  if FTextChanged then
    ApplyText;
end;

procedure TSVGViewerPropertiesForm.Parse;
var
  SVGParser: TSVGSaxParser;
begin
  if assigned(FSelectControl) then
  begin

    if FTextChanged then
      ApplyText;

    MemoInfo.Lines.Add('');
    MemoInfo.Lines.Add('--------------------------------------------');
    MemoInfo.Lines.Add('Parsing: ' + FSelectControl.FileName);
    MemoInfo.Lines.Add('');

    SVGParser := FSelectControl.SVGParser;

    FSelectControl.ParseSVG;

    SVGParser.ReportStatistics(MemoInfo.Lines);


    MemoInfo.Lines.Add('--------------------------------------------');
  end;
end;

{$IFnDEF FPC}
procedure TSVGViewerPropertiesForm.SetActionAttrChange(const Value: TAction);
begin
  FActionAttrChange := Value;
  if assigned(FActionAttrChange) then
    FActionAttrChange.OnExecute := ActionAttrChangeExecute;
end;
{$ENDIF}

procedure TSVGViewerPropertiesForm.SetActionParse(const Value: TAction);
begin
  FActionParse := Value;
  if assigned(FActionParse) then
    FActionParse.OnExecute := ActionParseExecute;
end;

procedure TSVGViewerPropertiesForm.SetCheckboxAutoViewbox(
  const Value: TCheckBox);
begin
  FCheckboxAutoViewbox := Value;
  if assigned(FCheckboxAutoViewbox) then
    FCheckboxAutoViewbox.OnClick := CheckboxAutoViewboxClick;
end;

procedure TSVGViewerPropertiesForm.SetCheckboxClippaths(const Value: TCheckbox);
begin
  FCheckboxClippaths := Value;
  if assigned(FCheckboxClippaths) then
    FCheckboxClippaths.OnClick := CheckboxClippathsClick;
end;

procedure TSVGViewerPropertiesForm.SetCheckboxEvents(const Value: TCheckbox);
begin
  FCheckboxEvents := Value;
  if assigned(FCheckboxEvents) then
    FCheckboxEvents.OnClick := CheckboxEventsClick;
end;

procedure TSVGViewerPropertiesForm.SetCheckboxFilters(const Value: TCheckbox);
begin
  FCheckboxFilters := Value;
  if assigned(FCheckboxFilters) then
    FCheckboxFilters.OnClick := CheckboxFiltersClick;
end;

{$IFDEF FPC}
procedure TSVGViewerPropertiesForm.SetEditWidth(const Value: TEdit);
begin
  FEditWidth := Value;
  if assigned(FEditWidth) then
    FEditWidth.OnExit := EditWidthExit;
end;
{$ENDIF}

{$IFDEF FPC}
procedure TSVGViewerPropertiesForm.SetEditHeight(const Value: TEdit);
begin
  FEditHeight := Value;
  if assigned(FEditHeight) then
    FEditHeight.OnExit := EditHeightExit;
end;
{$ENDIF}

{$IFnDEF FPC}
procedure TSVGViewerPropertiesForm.SetListView(const Value: TListView);
begin
  FListView := Value;
  if assigned(FListView) then
    FListView.OnClick := ListViewClick;
end;
{$ENDIF}

{$IFnDEF FPC}
procedure TSVGViewerPropertiesForm.SetMemoAttrValue(const Value: TMemo);
begin
  FMemoAttrValue := Value;
end;
{$ENDIF}

procedure TSVGViewerPropertiesForm.SetMemoInfo(const Value: TMemo);
begin
  FMemoInfo := Value;
end;

procedure TSVGViewerPropertiesForm.SetMemoSVG(const Value: TMemo);
begin
  FMemoSVG := Value;

  if assigned(FMemoSVG) then
  begin
    FMemoSVG.OnChange := MemoSVGChange;
    FMemoSVG.OnExit := MemoSVGExit;
  end;
end;

procedure TSVGViewerPropertiesForm.SetPanelInfo(const Value: TPanel);
begin
  FPanelInfo := Value;
end;

procedure TSVGViewerPropertiesForm.SetSelectControl(
  const Value: TSVGSelectControl);
begin
  if FSelectControl <> Value then
  begin
    FSelectControl := Value;
    if Showing then
      UpdateAll;
  end;
end;

{$IFnDEF FPC}
procedure TSVGViewerPropertiesForm.SetTreeView(const Value: TTreeView);
begin
  FTreeView := Value;
  if assigned(FTreeView) then
  begin
    FTreeView.OnClick := TreeViewClick;
    FTreeView.OnExpanding := TreeViewExpanding;
  end;
end;
{$ENDIF}

{$IFnDEF FPC}
procedure TSVGViewerPropertiesForm.TreeViewClick(Sender: TObject);
begin
  AttributeListUpdate(TreeView, ListView, MemoAttrValue.Lines);
end;
{$ENDIF}

{$IFnDEF FPC}
procedure TSVGViewerPropertiesForm.TreeViewExpanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
begin
  TreeViewExpand(TreeView, Node);
end;
{$ENDIF}

procedure TSVGViewerPropertiesForm.UpdateAll;
begin
  UpdateCaption;
  UpdateText;
  UpdateOptions;
  {$IFnDEF FPC}
  UpdateTreeView;
  {$ENDIF}
end;

procedure TSVGViewerPropertiesForm.UpdateCaption;
begin
  if assigned(FSelectControl) then
    PanelInfo.Caption := FSelectControl.FileName
  else
    PanelInfo.Caption := 'No svg selected';
end;

procedure TSVGViewerPropertiesForm.UpdateOptions;
begin
  if assigned(FSelectControl) then
  begin
    CheckBoxAutoViewBox.Checked := FSelectControl.AutoViewbox;
    CheckBoxFilters.Checked := sroFilters in FSelectControl.RenderOptions;
    CheckBoxClippaths.Checked := sroClippath in FSelectControl.RenderOptions;
    CheckBoxEvents.Checked := sroEvents in FSelectControl.RenderOptions;
  end;
end;

procedure TSVGViewerPropertiesForm.UpdateText;
begin
  if assigned(FSelectControl) then
  begin
    if FSelectControl.SVG.Count <> 0 then
      MemoSVG.Lines.Assign(FSelectControl.SVG)
    else
      MemoSVG.Lines.LoadFromFile(FSelectControl.Filename);
  end else
    MemoSVG.Lines.Clear;

  FTextChanged := False;
end;

{$IFnDEF FPC}
procedure TSVGViewerPropertiesForm.UpdateTreeView;
begin
  if assigned(FSelectControl) then
  begin
     if FSelectControl.SVG.Count <> 0 then
     begin
       FDoc.FileName := '';
       FDoc.XML := FSelectControl.SVG
     end else begin
       FDoc.XML.Clear;
       FDoc.FileName := FSelectControl.Filename;
     end;

    FDoc.Active := True;
    TreeViewUpdate(TreeView, FDoc.DocumentElement)
  end else
    TreeViewUpdate(TreeView, nil);
end;
{$ENDIF}

// -----------------------------------------------------------------------------
//
//                         TSVGViewerAboutForm
//
// -----------------------------------------------------------------------------

procedure TSVGViewerAboutForm.ConnectControls(aSVGImage: TSVG2Image);
begin
  FSVGImage := aSVGImage;
  FSVGImage.OnSVGEvent := SVGEvent;
end;

constructor TSVGViewerAboutForm.Create(AOwnder: TComponent);
begin
  inherited;
end;

destructor TSVGViewerAboutForm.Destroy;
begin
  inherited;
end;

procedure TSVGViewerAboutForm.SVGEvent(Sender: TObject; aSVGRoot: ISVGRoot;
  aEvent: ISVGEvent; const aValue: string);
var
  ImageElement: ISVGImage;
begin
  case aEvent.EventType of
    etClick:
      begin
        if aEvent.Target.ID = 'link' then
          ShellExecute(0, nil, pchar('www.bverhue.nl/delphisvg/'), nil, nil, sw_restore);
      end;
    etMouseOut:
      begin
        if aEvent.Target.ID = 'link' then
        begin
          aEvent.Target.Attributes['fill'] := 'black';
          aEvent.Target.Attributes['font-size'] := '16';
          FSVGImage.Repaint;
        end;
      end;
    etMouseOver:
      begin
        if aEvent.Target.ID = 'link' then
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
            ShellExecute(0, nil, pchar('https://www.w3.org/TR/SVG11/'), nil, nil, sw_restore);
          end;
        end;
      end;

  end;
end;

end.

