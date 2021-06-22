unit BVE.SVGEditorVCL;

// ------------------------------------------------------------------------------
//
//                          SVG Control Package 2.0
//                       Copyright (c) 2015 Bruno Verhue
//
// ------------------------------------------------------------------------------

// The SVG Editor needs at least version v2.40 update 9 of the SVG library

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Types,
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.ExtCtrls,
  Xml.XmlIntf,
  Xml.XmlDom,
  Xml.XmlDoc,
  BVE.SVG2Types,
  BVE.SVG2Intf,
  BVE.SVG2Elements,
  BVE.SVG2PathData,
  BVE.SVG2PathGeometry,
  BVE.SVGToolVCL;

type
  TSVGEditor = class;
  TSVGToolClass = class of TSVGEditorTool;

  TSVGEditorCmdType = (ctGroup, ctSelectElement, ctAddElement, ctRemoveElement,
    ctSetAttribute, ctSelectTool);

  TSVGEditorCmd = class
  private
    FCmdType: TSVGEditorCmdType;
  public
    constructor Create(aCmdType: TSVGEditorCmdType);

    property CmdType: TSVGEditorCmdType read FCmdType;
  end;

  TSVGEditorCmdGroup = class(TSVGEditorCmd)
  private
    FCmdList: TObjectList<TSVGEditorCmd>;
  public
    constructor Create;
    destructor Destroy; override;

    property CmdList: TObjectList<TSVGEditorCmd> read FCmdList;
  end;

  TSVGEditorCmdElementID = class(TSVGEditorCmd)
  private
    FID: Integer;
  public
    constructor Create(aCmdType: TSVGEditorCmdType; const aID: Integer);

    property ID: Integer read FID;
  end;

  TSVGEditorCmdElementsSelect = class(TSVGEditorCmd)
  private
    FSelect, FUnselect: TList<Integer>;
  public
    constructor Create;
    destructor Destroy; override;

    property Select: TList<Integer> read FSelect;
    property Unselect: TList<Integer> read FUnSelect;
  end;

  TSVGEditorCmdDOM = class(TSVGEditorCmd)
  private
    FDOMDocument: IDOMDocument;
    FParentID: Integer;
    FNodeIndex: Integer;
  public
    constructor Create(aCmdType: TSVGEditorCmdType; const aParentID,
      aNodeIndex: Integer; const aDOMDocument: IDOMDocument);

    property DOMDocument: IDOMDocument read FDOMDocument;
    property ParentID: Integer read FParentID;
    property NodeIndex: Integer read FNodeIndex;
  end;

  TSVGEditorCmdElementAdd = class(TSVGEditorCmdDOM)
  public
    constructor Create(const aParentID, aNodeIndex: Integer;
      const aDOMDocument: IDOMDocument);
  end;

  TSVGEditorCmdElementRemove = class(TSVGEditorCmdDOM)
  public
    constructor Create(const aParentID, aNodeIndex: Integer;
      const aDOMDocument: IDOMDocument);
  end;

  TSVGEditorCmdSetAttribute = class(TSVGEditorCmdElementID)
  private
    FName: TSVGUnicodeString;
    FValueOld: TSVGUnicodeString;
    FValueNew: TSVGUnicodeString;
  public
    constructor Create(const aID: Integer; const aName, aValueOld,
      aValueNew: TSVGUnicodeString);

    property Name: TSVGUnicodeString read FName;
    property ValueOld: TSVGUnicodeString read FValueOld;
    property ValueNew: TSVGUnicodeString read FValueNew write FValueNew;
  end;

  TSVGEditorCmdToolSelect = class(TSVGEditorCmd)
  private
    FToolOld: TSVGToolClass;
    FToolNew: TSVGToolClass;
  public
    constructor Create(const aToolOld, aToolNew: TSVGToolClass);

    property ToolOld: TSVGToolClass read FToolOld;
    property ToolNew: TSVGToolClass read FToolNew;
  end;

  TSVGToolAttributeChangeEvent = procedure(Sender: TObject; const aAttrName,
    aAttrValue: TSVGUnicodeString) of object;

  /// <summary>Bass class for Editor tools.</summary>
  TSVGEditorTool = class(TSVGTool, ISVGRefDimensions)
  private
    FEditor: TSVGEditor;
    FRoot: ISVGRoot;
    FSVGObject: ISVGObject;
    FObjectID: Integer;
    FCache: ISVGObjectCache;
    FParentCache: ISVGObjectCache;
    FBitmap: TBitmap;
    FOrigBounds: TRect;
    FOrigCTM: TSVGMatrix;
    FMatrix: TSVGMatrix;

    FRefFontSize: TSVGFloat;
    FRefWidth: TSVGFloat;
    FRefHeight: TSVGFloat;
    FRefLeft: TSVGFLoat;
    FRefTop: TSVGFloat;
    FRefLength: TSVGFloat;

    FCmdList: TDictionary<TSVGUnicodeString, TSVGEditorCmdSetAttribute>;

    FOnAttributeChange: TSVGToolAttributeChangeEvent;
  protected
    function GetAlignMatrix: TSVGMatrix;
    function GetViewportMatrix: TSVGMatrix;
    function GetScreenBBox: TSVGRect; virtual;

    function GetRefFontSize: TSVGFloat;
    function GetRefWidth: TSVGFloat;
    function GetRefHeight: TSVGFloat;
    function GetRefLeft: TSVGFloat;
    function GetRefTop: TSVGFloat;
    function GetRefLength: TSVGFloat;

    function SVGToTool(const aPoint: TSVGPoint): TSVGPoint; virtual;
    function ToolToSVG(const aPoint: TSVGPoint): TSVGPoint; virtual;

    procedure CalcDimReferences;
    procedure CalcBounds;

    function CalcX(aValue: TSVGDimension): TSVGFloat; overload;
    function CalcY(aValue: TSVGDimension): TSVGFloat; overload;

    function CalcX(const aValue: TSVGFloat; aDimType: TSVGDimensionType): TSVGDimension; overload;
    function CalcY(const aValue: TSVGFloat; aDimType: TSVGDimensionType): TSVGDimension; overload;

    procedure CmdListAddAttribute(const aAttrName: TSVGUnicodeString);
    procedure CmdListSetAttribute(const aAttrName, aAttrValue: TSVGUnicodeString); overload;
    procedure CmdListSetAttribute(const aAttrName: TSVGUnicodeString; aDimValue: TSVGDimension); overload;

    procedure PaintRenderContext(aRC: ISVGRenderContext); virtual;

    function CheckDisplay(aCache: ISVGObjectCache): Boolean;

    procedure UpdateBitmap;
  public
    constructor Create(aEditor: TSVGEditor; aRoot: ISVGRoot;
      aObject: ISVGObject; aParentCache: ISVGObjectCache); reintroduce; virtual;
    destructor Destroy; override;

    procedure Init; virtual;

    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    procedure Apply; virtual;

    procedure CalcTransform;

    procedure Paint; override;

    procedure KeyDown(var Key: Word; Shift: TShiftState); virtual;

    procedure SetFocus; override;

    property SVGObject: ISVGObject read FSVGObject;
    property AlignMatrix: TSVGMatrix read GetAlignMatrix;
    property ViewportMatrix: TSVGMatrix read GetViewportMatrix;
    property ScreenBBox: TSVGRect read GetScreenBBox;

    property OnAttributeChange: TSVGToolAttributeChangeEvent
      read FOnAttributeChange write FOnAttributeChange;
  end;

  /// <summary>Transform tool, modifies the transform attribute of an element.</summary>
  TSVGEditorToolTransform = class(TSVGEditorTool)
  protected
    procedure DoCreateToolParts; override;

    procedure UpdateTransform;
  public
    constructor Create(aEditor: TSVGEditor; aRoot: ISVGRoot;
      aObject: ISVGObject; aParentCache: ISVGObjectCache); override;
    destructor Destroy; override;

    procedure Init; override;

    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  end;

  TSVGToolShapeMode = (tsmEdit, tsmAddLines);

  /// <summary>Shape tool, modifies the shape of an element.</summary>
  TSVGEditorToolShape = class(TSVGEditorTool)
  private
    FPathGeometry: ISVGPathGeometry;
    FFigureIndex: Integer;
    FSegmentIndex: Integer;

    FMode: TSVGToolShapeMode;

    // Arc edtitting
    FCP: TSVGPoint;
    FR: TSVGPoint;
    FAngle: TSVGFloat;
    FStartAngle, FDeltaAngle: TSVGFloat;

    function GetObjectBounds: TSVGRect;
    procedure SetObjectBounds(aRect: TSVGRect);

    procedure SetAttrPathData(const aValue: TSVGUnicodeString);
    procedure SetMode(const Value: TSVGToolShapeMode);
  protected
    function GetHandlePoint(const aIndex: Integer): TPoint; override;
    function GetPathFigure: ISVGPathFigure;
    function GetPathSegment: ISVGPathSegment;

    procedure DoCreateToolParts; override;
    procedure DoSetHandlePoint(const aIndex: Integer; const Value: TPoint); override;
    procedure DoMovePosition(const aDx, aDy: Integer); override;

    procedure DoHandleMouseDown(const aIndex: integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    // Path editing
    function PathSegmentPrev: ISVGPathSegment;
    function PathSegmentNext: ISVGPathSegment;
    function PathSegmentGetStartPoint: TSVGPoint;
    procedure PathSegmentSetStartPoint(const aPoint: TSVGPoint);

    // Path editing
    property PathGeometry: ISVGPathGeometry read FPathGeometry;
    property PathFigure: ISVGPathFigure read GetPathFigure;
    property PathSegment: ISVGPathSegment read GetPathSegment;
  public
    constructor Create(aEditor: TSVGEditor; aRoot: ISVGRoot;
      aObject: ISVGObject; aParentCache: ISVGObjectCache); override;
    destructor Destroy; override;

    procedure Init; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    property Mode: TSVGToolShapeMode read FMode write SetMode;
  end;

  /// <summary>Draw tool, for drawing a polygon.</summary>
  TSVGEditorToolDraw = class(TSVGEditorTool)
  private
    FPointList: TList<TSVGPoint>;
    FNewPoint: TSVGPoint;
    FBoundsRect: TSVGRect;

    FPolygon: ISVGPolygon;

    procedure CreateToolPartsInternal;
  protected
    function GetHandlePoint(const aIndex: Integer): TPoint; override;
    function GetPoints: TSVGUnicodeString;
    function GetScreenBBox: TSVGRect; override;

    procedure DoHandleMouseDown(const aIndex: integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DoHandleMouseMove(const aIndex: integer; Shift: TShiftState; X, Y: Integer); override;

    procedure DoSetHandlePoint(const aIndex: Integer; const Value: TPoint); override;
    procedure DoCreateToolParts; override;

    procedure PaintRenderContext(aRC: ISVGRenderContext); override;

    function SVGToTool(const aPoint: TSVGPoint): TSVGPoint; override;
    function ToolToSVG(const aPoint: TSVGPoint): TSVGPoint; override;

    property Points: TSVGUnicodeString read GetPoints;
  public
    constructor Create(aEditor: TSVGEditor; aRoot: ISVGRoot;
      aObject: ISVGObject; aParentCache: ISVGObjectCache); override;
    destructor Destroy; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    procedure Init; override;
  end;

  TElementSelectEvent = procedure(Sender: TObject) of object;
  TElementAddEvent = procedure(Sender: TObject; const aParent: ISVGElement;
    const aIndex: Integer; const aaElement: ISVGElement) of object;
  TElementRemoveEvent = procedure(Sender: TObject; const aElement: ISVGElement) of object;
  TSetAttributeEvent = procedure(Sender: TObject; const aElement: ISVGElement;
    const aName, aValue: TSVGUnicodeString) of object;
  TToolSelectEvent = procedure(Sender: TObject; const aTool: TSVGToolClass) of object;

  TRenderingSelection = (rsFull, rsBackground, rsSelection);

  TSVGEditor = class(TCustomControl)
  const
    // For undo/redo commands we cannot work with pointers because objects
    // can be recreated, in stead we must use an "id". The following namespace
    // and attribute is used to add an id to elements.
    ns_svg_editor = 'https://www.bverhue.nl/svg_editor';
    at_local_id = 'editor_id';
    prefix_local_id = 'svge';
  private
    FRenderingSelection: TRenderingSelection;

    FRoot: ISVGRoot;

    FTimerUpdatePage: TTimer;

    FBitmap: TBitmap;
    FPageRC: ISVGRenderContext;

    FMatrix: TSVGMatrix;
    FInvMatrix: TSVGMatrix;

    FPadding: TPadding;
    FCanvasRect: TRect;
    FTopLeft: TPoint;
    FScale: TSVGFloat;
    FContentScale: TSVGPoint;

    FBitmapSelection: TBitmap;

    FFilename: string;
    FBackgroundColor: TSVGColor;

    FSelectedElementList: TList<Integer>;
    FUpdateRectList: TList<TSVGRect>;

    FToolList: TList<TSVGEditorTool>;
    FFocusedTool: TSVGEditorTool;
    FCurTool: TSVGToolClass;

    FChildListBefore: TList<ISVGElement>;

    FCmdUndoStack: TStack<TSVGEditorCmd>;
    FCmdRedoStack: TStack<TSVGEditorCmd>;

    FElementList: TDictionary<Integer, ISVGElement>;
    FMaxID: Integer;

    FOnElementAdd: TElementAddEvent;
    FOnElementRemove: TElementRemoveEvent;
    FOnElementSelect: TElementSelectEvent;
    FOnSetAttribute: TSetAttributeEvent;
    FOnToolSelect: TToolSelectEvent;

    procedure WMVScroll(var msg: TWMSCROLL); message WM_VSCROLL;
    procedure WMHScroll(var msg: TWMSCROLL); message WM_HSCROLL;
    procedure WMGetDlgCode(var msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure HandleScrollbar(var aMsg: TWMSCROLL; aBar: Integer);

    procedure ChildListBeforeCreate(aParent: IXMLNode);

    function ControlSize(const aScrollbarKind: ShortInt;
      const aControlSB, aAssumeSB: Boolean): Integer;

    procedure ToolsCreate;
    procedure ToolsClear;
    procedure ToolsHide;
    procedure ToolsShow;

    procedure ElementListInit(aNode: IXMLNode; var aID: Integer);

    procedure CmdUndoStackClear;
    procedure CmdRedoStackClear;

    function PointToSVG(const aPoint: TPoint): TSVGPoint;
    procedure SetFocusedTool(const Value: TSVGEditorTool);
  protected
    function GetCmdRedoCount: Integer;
    function GetCmdUndoCount: Integer;
    function GetElement(const aID: Integer): ISVGElement;
    function GetSelectedElement(const aIndex: Integer): Integer;
    function GetSelectedElementCount: Integer;

    procedure SetCanvasRect(const Value: TRect);
    procedure SetFilename(const Value: string);
    procedure SetPadding(const Value: TPadding);
    procedure SetScale(const Value: TSVGFloat);
    procedure SetSelectedElementList(const Value: TList<Integer>);
    procedure SetTopLeft(const Value: TPoint);

    procedure RenderSVGObject(aSVGObject: ISVGObject;
      aParentCache: ISVGObjectCache; const aStage: TSVGRenderStage;
      var IsRendered: Boolean);

    procedure CreateParams(var params: TCreateParams); override;

    procedure DoElementAdd(const aParent: ISVGElement; const aIndex: Integer;
      const aElement: ISVGElement); virtual;
    procedure DoElementRemove(const aElement: ISVGElement); virtual;
    procedure DoElementSelect(const aElement: ISVGElement); virtual;
    procedure DoSetAttribute(const aElement: ISVGElement; const aName,
      aValue: TSVGUnicodeString); virtual;
    procedure DoToolSelect(const aTool: TSVGToolClass);

    procedure CmdExecInternal(aCmd: TSVGEditorCmd; const aUndo: Boolean);
    procedure CmdExec(aCmd: TSVGEditorCmd; const aUndo: Boolean);
    procedure CmdExecElementSelect(aCmd: TSVGEditorCmdElementsSelect; const aUndo: Boolean);
    procedure CmdExecElementAdd(aCmd: TSVGEditorCmdDOM);
    procedure CmdExecElementRemove(aCmd: TSVGEditorCmdDOM);
    procedure CmdExecSetAttribute(aCmd: TSVGEditorCmdSetAttribute; const aUndo: Boolean);
    procedure CmdExecToolSelect(aCmd: TSVGEditorCmdToolSelect; const aUndo: Boolean);

    function CmdElementAddCreate(const aParent: ISVGELement;
      const aNodeIndex: Integer; const aFragment: string): TSVGEditorCmdElementAdd;
    function CmdElementRemoveCreate(
      const aElement: ISVGElement): TSVGEditorCmdElementRemove;
    function CmdSetAttributeCreate(const aElement: ISVGElement; const aName,
      aValue: TSVGUnicodeString): TSVGEditorCmdSetAttribute;
    function CmdToolSelectCreate(const aTool: TSVGToolClass): TSVGEditorCmdToolSelect;

    procedure DoPaddingChange(Sender: TObject);
    procedure DoUpdatePage(Sender: TObject);

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    function ToolCreate(aSVGObject: ISVGObject): TSVGEditorTool;
    procedure ToolDestroy(var aTool: TSVGEditorTool);
    procedure ToolAttributeChangeEvent(Sender: TObject; const aAttrName,
      aAttrValue: TSVGUnicodeString);

    procedure CalcCanvasRect;

    procedure UpdateScrollbars;

    procedure Init;
    procedure Clear;

    procedure Paint; override;

    property SelectedElementList: TList<Integer> read FSelectedElementList write SetSelectedElementList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetElementID(aElement: ISVGElement): Integer; overload;
    function GetElementID(aElement: IDOMElement): Integer; overload;

    procedure Resize; override;

    procedure ElementsCopy;
    procedure ElementsCut;
    procedure ElementsPaste;
    procedure ElementsDelete;
    procedure ElementAdd(const aFragment: string; const aSelectAdded: Boolean);
    procedure ElementsAdd(const aFragments: TStringList; const aSelectAdded: Boolean);

    procedure ElementsUnselectAll;
    procedure ElementsSelect(aList: TList<Integer>);
    function ElementIsSelected(const aID: Integer): Boolean;

    procedure CmdUndo;
    procedure CmdRedo;

    procedure ToolSelect(const aTool: TSVGToolClass);

    procedure LoadFromFile(const aFilename: string);
    procedure LoadFromStream(aStream: TStream);
    procedure SaveToFile(const aFilename: string);

    procedure UpdatePage(const aRenderMode: TSVGRenderMode);

    procedure SetAttribute(const aName, aValue: TSVGUnicodeString);

    property CanvasRect: TRect read FCanvasRect write SetCanvasRect;
    property CmdUndoCount: Integer read GetCmdUndoCount;
    property CmdRedoCount: Integer read GetCmdRedoCount;
    property ContentScale: TSVGPoint read FContentScale;
    property CurTool: TSVGToolClass read FCurTool;
    property Element[const aID: Integer]: ISVGElement read GetElement;
    property Filename: string read FFilename write SetFilename;
    property FocusedTool: TSVGEditorTool read FFocusedTool write SetFocusedTool;
    property Matrix: TSVGMatrix read FMatrix;
    property Padding: TPadding read FPadding write SetPadding;
    property Root: ISVGRoot read FRoot;
    property Scale: TSVGFloat read FScale write SetScale;
    property SelectedElementCount: Integer read GetSelectedElementCount;
    property SelectedElement[const aIndex: Integer]: Integer read GetSelectedElement;
    property TopLeft: TPoint read FTopLeft write SetTopLeft;

    property OnElementAdd: TElementAddEvent read FOnElementAdd write FOnElementAdd;
    property OnElementRemove: TElementRemoveEvent read FOnElementRemove write FOnElementRemove;
    property OnElementSelect: TElementSelectEvent read FOnElementSelect write FOnElementSelect;
    property OnSetAttribute: TSetAttributeEvent read FOnSetAttribute write FOnSetAttribute;
    property OnToolSelect: TToolSelectEvent read FOnToolSelect write FOnToolSelect;
  end;

implementation
uses
  System.Math,
  Vcl.Clipbrd,
  BVE.SVG2Dom,
  BVE.SVG2Context,
  BVE.SVG2GeomUtility,
  BVE.SVG2ParseUtility,
  BVE.SVG2SaxParser,
  BVE.SVG2Elements.VCL;

const
  svg_handle_transform_style = 'fill: blue; stroke: none; stroke-width: 16;';

  svg_handle_transform_0 =
    '<?xml version="1.0" standalone="no"?>'
    + '<svg viewBox="0 0 100 100" xmlns="' + ns_uri_svg + '" version="1.1">'
    + '<path d="M0,0 L100,0 L0,100 Z" style="' + svg_handle_transform_style + '" />'
    + '</svg>';

  svg_handle_transform_1 =
    '<?xml version="1.0" standalone="no"?>'
    + '<svg viewBox="0 0 100 100" xmlns="' + ns_uri_svg + '" version="1.1">'
    + '<path d="M100,0 L100,100 L0,0 Z" style="' + svg_handle_transform_style + '" />'
    + '</svg>';

  svg_handle_transform_2 =
    '<?xml version="1.0" standalone="no"?>'
    + '<svg viewBox="0 0 100 100" xmlns="' + ns_uri_svg + '" version="1.1">'
    + '<path d="M100,100 L0,100 L100,0 Z" style="' + svg_handle_transform_style + '" />'
    + '</svg>';

  svg_handle_transform_3 =
    '<?xml version="1.0" standalone="no"?>'
    + '<svg viewBox="0 0 100 100" xmlns="' + ns_uri_svg + '" version="1.1">'
    + '<path d="M0,100 L0,0 L100,100 Z" style="' + svg_handle_transform_style + '" />'
    + '</svg>';

  svg_handle_shape_1_style = 'fill: blue; stroke: none; stroke-width: 16;';

  svg_handle_shape_1 =
    '<?xml version="1.0" standalone="no"?>'
    + '<svg viewBox="0 0 100 100" xmlns="' + ns_uri_svg + '" version="1.1">'
    + '<rect x="0" y="0" width="100" height="100" style="' + svg_handle_shape_1_style + '" />'
    + '</svg>';

  svg_handle_shape_2_style = 'fill: red; stroke: none; stroke-width: 16;';

  svg_handle_shape_2 =
    '<?xml version="1.0" standalone="no"?>'
    + '<svg viewBox="0 0 100 100" xmlns="' + ns_uri_svg + '" version="1.1">'
    + '<circle cx="50" cy="50" r="50" style="' + svg_handle_shape_2_style + '" />'
    + '</svg>';

  svg_handle_shape_3_style = 'fill: none; stroke: blue; stroke-width: 16;';

  svg_handle_shape_3 =
    '<?xml version="1.0" standalone="no"?>'
    + '<svg viewBox="0 0 100 100" xmlns="' + ns_uri_svg + '" version="1.1">'
    + '<circle cx="50" cy="50" r="50" style="' + svg_handle_shape_3_style + '" />'
    + '<circle cx="50" cy="50" r="1" style="' + svg_handle_shape_3_style + '" />'
    + '</svg>';

// -----------------------------------------------------------------------------
//
//                               TSVGEditorTool
//
// -----------------------------------------------------------------------------

procedure TSVGEditorTool.Apply;
var
  CmdGroup: TSVGEditorCmdGroup;
  Pair: TPair<TSVGUnicodeString, TSVGEditorCmdSetAttribute>;
begin
  if FCmdList.Count = 0 then
    Exit;

  CmdGroup := TSVGEditorCmdGroup.Create;
  try
    for Pair in FCmdList do
      CmdGroup.CmdList.Add(Pair.Value);
    FCmdList.Clear;

    FEditor.CmdExec(CmdGroup, False);
    FEditor.FCmdUndoStack.Push(CmdGroup);

    except on E:Exception do
      CmdGroup.Free;
  end;
end;

procedure TSVGEditorTool.CalcBounds;
var
  R: TSVGRect;
  RR: TRect;
begin
  R := ScreenBBox;

  R := TransformRect(R, FEditor.Matrix);
  RR := R.Round;

  AbsoluteContentRect :=
    Rect(
      RR.Left - FEditor.TopLeft.X,
      RR.Top - FEditor.TopLeft.Y,
      RR.Right - FEditor.TopLeft.X,
      RR.Bottom - FEditor.TopLeft.Y);
end;

procedure TSVGEditorTool.CalcDimReferences;
var
  SaveMatrix: TSVGMatrix;
  SaveStyle: TSVGStyleAttributeRec;
  SaveViewport: TSVGRect;
  Ref: ISVGRefDimensions;
begin
  FRefFontSize := 0.0;
  FRefWidth := 0.0;
  FRefHeight := 0.0;
  FRefLeft := 0.0;
  FRefTop := 0.0;
  FRefLength := 0.0;

  if assigned(FCache) then
  begin
    SaveMatrix := FRoot.CTMSave;
    SaveStyle := FRoot.CSASave;
    SaveViewport := FRoot.CVP;
    try
      FRoot.CTMRestore(FCache.CTM);
      FRoot.CSARestore(FCache.CSA);
      FRoot.CVPSet(FCache.CVP);

      Ref := FRoot as ISVGRefDimensions;

      FRefFontSize := Ref.RefFontSize;
      FRefWidth := Ref.RefWidth;
      FRefHeight := Ref.RefHeight;
      FRefLeft := Ref.RefLeft;
      FRefTop := Ref.RefTop;
      FRefLength := Ref.RefLength;

    finally
      FRoot.CVPSet(SaveViewport);
      FRoot.CSARestore(SaveStyle);
      FRoot.CTMRestore(SaveMatrix);
    end;
  end;
end;

procedure TSVGEditorTool.CalcTransform;
var
  CTM: TSVGMatrix;
  R1, R2: TSVGRect;
  Sx, Sy: TSVGFloat;
begin
  if not assigned(FSVGObject) then
    Exit;

  CTM := FOrigCTM;

  R1 := TSVGRect.Create(FOrigBounds);

  R2 := TSVGRect.Create(AbsoluteContentRect);
  R2.Offset(
    -FEditor.Padding.Left + FEditor.TopLeft.X,
    -FEditor.Padding.Top + FEditor.TopLeft.Y);

  R1 := TSVGRect.Scale(R1, 1/FEditor.ContentScale.X);
  R2 := TSVGRect.Scale(R2, 1/FEditor.ContentScale.Y);

  if R1 <> R2 then
  begin

    R1 := TransformRect(R1, CTM.Inverse);
    R2 := TransformRect(R2, CTM.Inverse);

    if R1.Width <> 0 then
      Sx := R2.Width / R1.Width
    else
      Sx := 1.0;

    if R1.Height <> 0 then
      Sy := R2.Height / R1.Height
    else
      Sy := 1.0;

    // https://stackoverflow.com/questions/14015456/create-transform-to-map-from-one-rectangle-to-another

    FMatrix := TSVGMatrix.CreateTranslation(R2.Left, R2.Top);

    FMatrix := TSVGMatrix.Multiply(
      TSVGMatrix.CreateScaling(Sx, Sy),
      FMatrix
      );

    FMatrix := TSVGMatrix.Multiply(
      TSVGMatrix.CreateTranslation(-R1.Left, -R1.Top),
      FMatrix
      );
  end;
end;

function TSVGEditorTool.CalcX(aValue: TSVGDimension): TSVGFloat;
begin
  Result := aValue.CalcAsX(Self);
end;

function TSVGEditorTool.CalcX(const aValue: TSVGFloat;
  aDimType: TSVGDimensionType): TSVGDimension;
begin
  Result := TSVGDimension.CalcAsX(aValue, aDimType, Self);
end;

function TSVGEditorTool.CalcY(aValue: TSVGDimension): TSVGFloat;
begin
  Result := aValue.CalcAsY(Self);
end;

function TSVGEditorTool.CalcY(const aValue: TSVGFloat;
  aDimType: TSVGDimensionType): TSVGDimension;
begin
  Result := TSVGDimension.CalcAsY(aValue, aDimType, Self);
end;

function TSVGEditorTool.CheckDisplay(aCache: ISVGObjectCache): Boolean;
var
  Attr: ISVGAttribute;
begin
  Result := True;

  Attr := aCache.CSA.AttrArray[atDisplay];
  if assigned(Attr) and (Attr.AsDisplay = TSVGDisplay.dpNone) then
  begin
    Result := False;
    Exit;
  end;

  if Result and assigned(aCache.ParentCache) then
    Result := CheckDisplay(aCache.ParentCache);
end;

procedure TSVGEditorTool.CmdListAddAttribute(
  const aAttrName: TSVGUnicodeString);
var
  Cmd: TSVGEditorCmdSetAttribute;
begin
  Cmd := TSVGEditorCmdSetAttribute.Create(
    FObjectID,
    aAttrName,
    FSVGObject.Attributes[aAttrName],
    FSVGObject.Attributes[aAttrName]);

  FCmdList.Add(aAttrName, Cmd);
end;

procedure TSVGEditorTool.CmdListSetAttribute(const aAttrName: TSVGUnicodeString;
  aDimValue: TSVGDimension);
begin
  CmdListSetAttribute(aAttrName, aDimValue.AsString);
end;

procedure TSVGEditorTool.CmdListSetAttribute(const aAttrName,
  aAttrValue: TSVGUnicodeString);
var
  Cmd: TSVGEditorCmdSetAttribute;
begin
  if FCmdList.TryGetValue(aAttrName, Cmd) then
  begin
    Cmd.ValueNew := aAttrValue;

    FSVGObject.Attributes[aAttrName] := aAttrValue;

    if assigned(FOnAttributeChange) then
      FOnAttributeChange(Self, aAttrName, aAttrValue);
  end;
end;

constructor TSVGEditorTool.Create(aEditor: TSVGEditor;
  aRoot: ISVGRoot; aObject: ISVGObject; aParentCache: ISVGObjectCache);
begin
  FCmdList := TDictionary<TSVGUnicodeString, TSVGEditorCmdSetAttribute>.Create;

  FEditor := aEditor;
  FMatrix := TSVGMatrix.CreateIdentity;

  FRoot := aRoot;
  FSVGObject := aObject;
  FParentCache := aParentCache;

  inherited Create(aEditor);

  Init;
end;

destructor TSVGEditorTool.Destroy;
var
  Pair: TPair<TSVGUnicodeString, TSVGEditorCmdSetAttribute>;
begin
  if assigned(FBitmap) then
    FBitmap.Free;

  for Pair in FCmdList do
    Pair.Value.Free;

  FCmdList.Free;

  inherited;
end;

function TSVGEditorTool.GetAlignMatrix: TSVGMatrix;
begin
  // Alignment of the SVG in the tool client rectangle

  Result := TSVGMatrix.Multiply(
    FEditor.Matrix,
    TSVGMatrix.CreateTranslation(
      -AbsoluteContentRect.Left - FEditor.TopLeft.X,
      -AbsoluteContentRect.Top - FEditor.TopLeft.Y)
    );
end;

function TSVGEditorTool.GetRefFontSize: TSVGFloat;
begin
  Result := FRefFontSize;
end;

function TSVGEditorTool.GetRefHeight: TSVGFloat;
begin
  Result := FRefHeight;
end;

function TSVGEditorTool.GetRefLeft: TSVGFloat;
begin
  Result := FRefLeft;
end;

function TSVGEditorTool.GetRefLength: TSVGFloat;
begin
  Result := FRefLength;
end;

function TSVGEditorTool.GetRefTop: TSVGFloat;
begin
  Result := FRefTop;
end;

function TSVGEditorTool.GetRefWidth: TSVGFloat;
begin
  Result := FRefWidth;
end;

function TSVGEditorTool.GetScreenBBox: TSVGRect;
begin
  Result := TSVGRect.CreateUndefined;
  if assigned(FCache) then
    Result := FCache.ScreenBBox;

  if Result.IsUndefined then
    Result := SVGRect(0.0, 0.0, 0.0, 0.0);
end;

function TSVGEditorTool.GetViewportMatrix: TSVGMatrix;
var
  T: TSVGMatrix;
begin
  if assigned(FCache) then
    T := FCache.CTM
  else
    T := TSVGMatrix.CreateIdentity;

  Result := T;

  {if assigned(FParentCache) then
    Result := TSVGMatrix.Multiply(Result, FParentCache.ViewportMatrix);}

  Result := TSVGMatrix.Multiply(Result, AlignMatrix);
  Result := TSVGMatrix.Multiply(Result, TSVGMatrix.CreateTranslation(Margin, Margin));
end;

procedure TSVGEditorTool.Init;
var
  i: Integer;
  Pair: TPair<TSVGUnicodeString, TSVGEditorCmdSetAttribute>;
begin
  for Pair in FCmdList do
    Pair.Value.Free;
  FCmdList.Clear;

  FOrigCTM := TSVGMatrix.CreateIdentity;

  FObjectID := FEditor.GetElementID(FSVGObject);

  if FSVGObject.CacheList.Count > 0 then
  begin
    i := 0;
    while (i < FSVGObject.CacheList.Count)
    and (FSVGObject.CacheList[i].ParentCache as IInterface <> FParentCache as IInterface) do
      Inc(i);

    if i < FSVGObject.CacheList.Count then
      FCache := FSVGObject.CacheList[i]
    else
      FCache := FSVGObject.CacheList[0];

    FOrigCTM := FCache.CTM;

  end else
    FCache := nil;

  CalcDimReferences;
  CalcBounds;

  if (AbsoluteContentRect.Right < AbsoluteContentRect.Left)
  or (AbsoluteContentRect.Bottom < AbsoluteContentRect.Top) then
    FOrigBounds := AbsoluteContentRect
  else
    FOrigBounds := Rect(
      AbsoluteContentRect.Left + FEditor.TopLeft.X - FEditor.Padding.Left,
      AbsoluteContentRect.Top + FEditor.TopLeft.Y - FEditor.Padding.Top,
      AbsoluteContentRect.Right + FEditor.TopLeft.X - FEditor.Padding.Left,
      AbsoluteContentRect.Bottom + FEditor.TopLeft.Y - FEditor.Padding.Top);

  UpdateBitmap;
end;

procedure TSVGEditorTool.KeyDown(var Key: Word; Shift: TShiftState);
begin
//
end;

procedure TSVGEditorTool.Paint;
begin
  inherited;

  Canvas.Draw(ContentRect.Left, ContentRect.Top, FBitmap);
end;

procedure TSVGEditorTool.PaintRenderContext(aRC: ISVGRenderContext);
begin
  FSVGObject.PaintIndirect(FRoot, False, FParentCache);
end;

procedure TSVGEditorTool.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
end;

procedure TSVGEditorTool.SetFocus;
begin
  inherited;

  FEditor.SetFocus;
  FEditor.FocusedTool := Self;
end;

function TSVGEditorTool.SVGToTool(const aPoint: TSVGPoint): TSVGPoint;
var
  M: TSVGMatrix;
begin
  M := ViewportMatrix;

  Result := TransformPoint(aPoint, M);
end;

function TSVGEditorTool.ToolToSVG(const aPoint: TSVGPoint): TSVGPoint;
var
  M: TSVGMatrix;
begin
  M := ViewportMatrix;

  Result := TransformPoint(aPoint, M.Inverse);
end;

procedure TSVGEditorTool.UpdateBitmap;
var
  RC: ISVGRenderContext;
  SaveMatrix: TSVGMatrix;
  SaveStyle: TSVGStyleAttributeRec;
  SaveViewport: TSVGRect;
  W, H: Integer;
  Test: Boolean;
begin
  W := ContentRect.Width;
  H := ContentRect.Height;

  if (ContentRect.Right <= ContentRect.Left)
  or (ContentRect.Bottom <= ContentRect.Top)
  then
    Exit;

  if assigned(FBitmap) then
  begin
    if (FBitmap.Width <> W) or (FBitmap.Height <> H) then
      FBitmap.SetSize(W, H);
  end else begin
    FBitmap := TSVGRenderContextManager.CreateCompatibleBitmap(W, H);
  end;

  RC := TSVGRenderContextManager.CreateRenderContextBitmap(FBitmap);

  FRoot.RenderMode := [];
  FRoot.UpdateRectList.Clear;

  FEditor.FRenderingSelection := rsSelection;

  SaveMatrix := FRoot.CTMSave;
  SaveStyle := FRoot.CSASave;
  SaveViewport := FRoot.CVP;
  try
    if assigned(FParentCache) then
    begin
      FRoot.CTMRestore(FParentCache.CTM);
      FRoot.CSARestore(FParentCache.CSA);
      FRoot.CVPSet(FParentCache.CVP);

      FRoot.CTMMultiply(FParentCache.ViewportMatrix);
    end;

    FRoot.PushBuffer(TSVGRenderBuffer.Create(RC));
    try
      RC.BeginScene;
      try
        RC.Clear(SVGColorNone);

        RC.Matrix := AlignMatrix;

        PaintRenderContext(RC);
      finally
        RC.EndScene;
      end;
    finally
      FRoot.PopBuffer;
    end;

  finally
    FRoot.CVPSet(SaveViewport);
    FRoot.CSARestore(SaveStyle);
    FRoot.CTMRestore(SaveMatrix);
  end;
end;

// -----------------------------------------------------------------------------
//
//                       TSVGEditorToolTransform
//
// -----------------------------------------------------------------------------

constructor TSVGEditorToolTransform.Create(aEditor: TSVGEditor; aRoot: ISVGRoot;
  aObject: ISVGObject; aParentCache: ISVGObjectCache);
begin
  inherited Create(aEditor, aRoot, aObject, aParentCache);
end;

destructor TSVGEditorToolTransform.Destroy;
begin
  inherited;
end;

procedure TSVGEditorToolTransform.DoCreateToolParts;
var
  i: integer;
  Handle: TSVGToolHandle;
begin
  for i := 0 to 3 do
  begin
    case i of
      0: Handle := TSVGToolHandle.Create(Self, i, 6, svg_handle_transform_0);
      1: Handle := TSVGToolHandle.Create(Self, i, 6, svg_handle_transform_1);
      2: Handle := TSVGToolHandle.Create(Self, i, 6, svg_handle_transform_2);
      else
        Handle := TSVGToolHandle.Create(Self, i, 6, svg_handle_transform_3);
    end;

    Handle.Parent := Parent;
    HandleList.Add(Handle);
  end;
end;

procedure TSVGEditorToolTransform.Init;
begin
  inherited;

  CmdListAddAttribute('transform');
end;

procedure TSVGEditorToolTransform.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  inherited;

  if IsChanged then
  begin
    UpdateTransform;
    UpdateBitmap;
    Repaint;
  end;
end;

procedure TSVGEditorToolTransform.UpdateTransform;
var
  Transform: TSVGUnicodeString;
  Parser: TSVGCssParser;
  TransformList: TSVGTransformList;
  M: TSVGMatrix;
  Count: Integer;
  Cmd: TSVGEditorCmdSetAttribute;
begin
  CalcTransform;

  if FMatrix.IsIdentity then
    Exit;

  if FCmdList.TryGetValue('transform', Cmd) then
    Transform := Cmd.ValueOld
  else
    Transform := '';

  Parser := TSVGCssParser.Create(Transform);
  try
    Parser.ReadTransformList(TransformList);

    // If the last transform is a matrix, we multiply else we add

    Count := Length(TransformList);
    if (Count > 0) and (TransformList[Count-1].TransformType = ttMatrix) then
    begin
      M := TransformList[Count-1].CalcMatrix(FRoot as ISVGRefDimensions);
      M := TSVGMatrix.Multiply(FMatrix, M);
      TransformList[Count-1].SetMatrix(M);
    end else begin
      SetLength(TransformList, Count + 1);
      TransformList[Count].SetMatrix(FMatrix);
    end;

    Transform := ConvertTransform(TransformList);

    CmdListSetAttribute('transform', Transform);
  finally
    Parser.Free;
  end;
end;

// -----------------------------------------------------------------------------
//
//                         TSVGEditorToolShape
//
// -----------------------------------------------------------------------------

constructor TSVGEditorToolShape.Create(aEditor: TSVGEditor; aRoot: ISVGRoot;
  aObject: ISVGObject; aParentCache: ISVGObjectCache);
begin
  FPathGeometry := TSVGPathGeometry.Create;
  FFigureIndex := 0;
  FSegmentIndex := 0;
  FMode := tsmEdit;

  case aObject.ElementType of
    elRect:
      begin
      end;

    elCircle:
      begin
      end;

    elEllipse:
      begin
      end;

    elPolygon:
      begin
        FPathGeometry.AsPolygonString := aObject.Attributes['points'];
      end;

    elPolyline:
      begin
        FPathGeometry.AsPolylineString := aObject.Attributes['points'];
      end;

    elPath:
      begin
        FPathGeometry.AsPathString := aObject.Attributes['d'];
        if FPathGeometry.Figures.Count = 0 then
          FMode := tsmAddLines;
      end;
  end;

  inherited Create(aEditor, aRoot, aObject, aParentCache);
end;

destructor TSVGEditorToolShape.Destroy;
begin
  inherited;
end;

procedure TSVGEditorToolShape.DoCreateToolParts;
var
  i: integer;
  ToolHandle: TSVGToolHandle;
  ToolLine: TSVGToolLine;
  ToolEllipse: TSVGToolEllipse;
  Segment: ISVGPathSegment;
  LineSegment: ISVGLineSegment;
  BezierSegment: ISVGBezierSegment;
  QuadSegment: ISVGQuadSegment;
  ArcSegment: ISVGArcSegment;
begin
  case FSVGObject.ElementType of
    elRect:
      begin
        for i := 0 to 3 do
        begin
          ToolHandle := TSVGToolHandle.Create(Self, i, 4, svg_handle_shape_1);
          ToolHandle.Parent := Parent;
          HandleList.Add(ToolHandle);
        end;
      end;

    elCircle:
      begin
        for i := 0 to 0 do
        begin
          ToolHandle := TSVGToolHandle.Create(Self, i, 4, svg_handle_shape_1);
          ToolHandle.Parent := Parent;
          HandleList.Add(ToolHandle);
        end;
      end;

    elEllipse:
      begin
        for i := 0 to 1 do
        begin
          ToolHandle := TSVGToolHandle.Create(Self, i, 4, svg_handle_shape_1);
          ToolHandle.Parent := Parent;
          HandleList.Add(ToolHandle);
        end;
      end;

    elPolygon,
    elPolyLine,
    elPath:
      begin
        Segment := PathSegment;
        if assigned(Segment) then
        begin
          if Supports(Segment, ISVGLineSegment, LineSegment) then
          begin
            ToolHandle := TSVGToolHandle.Create(Self, 0, 4, svg_handle_shape_1);
            ToolHandle.Parent := Parent;
            HandleList.Add(ToolHandle);

            ToolHandle := TSVGToolHandle.Create(Self, 1, 4, svg_handle_shape_1);
            ToolHandle.Parent := Parent;
            HandleList.Add(ToolHandle);
          end else

          if Supports(Segment, ISVGBezierSegment, BezierSegment) then
          begin
            ToolLine := TSVGToolLine.Create(Self, 0, 1, svg_tool_line);
            ToolLine.Parent := Parent;
            GuideList.Add(ToolLine);

            ToolLine := TSVGToolLine.Create(Self, 3, 2, svg_tool_line);
            ToolLine.Parent := Parent;
            GuideList.Add(ToolLine);

            ToolHandle := TSVGToolHandle.Create(Self, 0, 4, svg_handle_shape_1);
            ToolHandle.Parent := Parent;
            HandleList.Add(ToolHandle);

            ToolHandle := TSVGToolHandle.Create(Self, 1, 4, svg_handle_shape_2);
            ToolHandle.Parent := Parent;
            HandleList.Add(ToolHandle);

            ToolHandle := TSVGToolHandle.Create(Self, 2, 4, svg_handle_shape_2);
            ToolHandle.Parent := Parent;
            HandleList.Add(ToolHandle);

            ToolHandle := TSVGToolHandle.Create(Self, 3, 4, svg_handle_shape_1);
            ToolHandle.Parent := Parent;
            HandleList.Add(ToolHandle);

          end else

          if Supports(Segment, ISVGQuadSegment, QuadSegment) then
          begin
            ToolLine := TSVGToolLine.Create(Self, 0, 1, svg_tool_line);
            ToolLine.Parent := Parent;
            GuideList.Add(ToolLine);

            ToolLine := TSVGToolLine.Create(Self, 2, 1, svg_tool_line);
            ToolLine.Parent := Parent;
            GuideList.Add(ToolLine);

            ToolHandle := TSVGToolHandle.Create(Self, 0, 4, svg_handle_shape_1);
            ToolHandle.Parent := Parent;
            HandleList.Add(ToolHandle);

            ToolHandle := TSVGToolHandle.Create(Self, 1, 4, svg_handle_shape_2);
            ToolHandle.Parent := Parent;
            HandleList.Add(ToolHandle);

            ToolHandle := TSVGToolHandle.Create(Self, 2, 4, svg_handle_shape_1);
            ToolHandle.Parent := Parent;
            HandleList.Add(ToolHandle);
          end else

          if Supports(Segment, ISVGArcSegment, ArcSegment) then
          begin

            FR := ArcSegment.Radius;
            FAngle := ArcSegment.Angle * PI / 180;
            ArcSegment.GetCenterParameters(PathSegmentGetStartPoint, FCP, FStartAngle, FDeltaAngle);

            ToolEllipse := TSVGToolEllipse.Create(Self, 0, 1, 2, svg_tool_ellipse);
            ToolEllipse.Parent := Parent;
            GuideList.Add(ToolEllipse);

            ToolHandle := TSVGToolHandle.Create(Self, 0, 4, svg_handle_shape_1);
            ToolHandle.Parent := Parent;
            HandleList.Add(ToolHandle);

            ToolHandle := TSVGToolHandle.Create(Self, 1, 4, svg_handle_shape_2);
            ToolHandle.Parent := Parent;
            HandleList.Add(ToolHandle);

            ToolHandle := TSVGToolHandle.Create(Self, 2, 4, svg_handle_shape_2);
            ToolHandle.Parent := Parent;
            HandleList.Add(ToolHandle);

            ToolHandle := TSVGToolHandle.Create(Self, 3, 4, svg_handle_shape_1);
            ToolHandle.Parent := Parent;
            HandleList.Add(ToolHandle);

            ToolHandle := TSVGToolHandle.Create(Self, 4, 4, svg_handle_shape_1);
            ToolHandle.Parent := Parent;
            HandleList.Add(ToolHandle);
          end;

        end;
      end;
  end;
end;

procedure TSVGEditorToolShape.DoHandleMouseDown(const aIndex: integer;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  P: TSVGPoint;
  Figure: ISVGPathFigure;
begin
  inherited;

  case Mode of
    tsmAddLines:
      begin
        if FSVGObject.ElementType in [elPath, elPolygon, elPolyline] then
        begin
          ClearToolParts;
          try

            P := SVGPoint(X, Y);
            P := ToolToSVG(P);

            if FPathGeometry.Figures.Count = 0 then
            begin
              Figure := FPathGeometry.AddFigure(P);
              FFigureIndex := FPathGeometry.Figures.Count - 1;
            end else
              Figure := PathFigure;

            Figure.AddLine(P);
            FSegmentIndex := Figure.Segments.Count - 1;
          finally
            CreateToolParts;
            UpdateBitmap;
          end;
        end;
      end;
  end;
end;

procedure TSVGEditorToolShape.DoMovePosition(const aDx, aDy: Integer);
var
  R: TSVGRect;
  SaveCTM: TSVGMatrix;
  T, S, A: TSVGMatrix;
  P: TSVGPoint;
  ArcSegment: ISVGArcSegment;
begin
  inherited;

  T := FCache.CTM;
  A := AlignMatrix;
  S := TSVGMatrix.CreateScaling(T.m11 * A.m11, T.m22 * A.m22);
  P := TransformPoint(SVGPoint(aDx, aDy), S.Inverse);

  case FSVGObject.ElementType of
    elRect,
    elCircle,
    elEllipse:
      begin
        R := GetObjectBounds;
        if R.IsUndefined then
          Exit;

        R.Offset(P.X, P.Y);

        SetObjectBounds(R);
      end;

    elPolygon:
      begin
        PathGeometry.ApplyMatrix(
          TSVGMatrix.CreateTranslation(P.X, P.Y));

        SetAttrPathData(PathGeometry.AsPolygonString);
      end;

    elPolyLine:
      begin
        PathGeometry.ApplyMatrix(
          TSVGMatrix.CreateTranslation(P.X, P.Y));

        SetAttrPathData(PathGeometry.AsPolylineString);
      end;

    elPath:
      begin
        PathGeometry.ApplyMatrix(
          TSVGMatrix.CreateTranslation(P.X, P.Y));

        SetAttrPathData(PathGeometry.AsPathString);

        if Supports(PathSegment, ISVGArcSegment, ArcSegment) then
        begin
          FR := ArcSegment.Radius;
          FAngle := ArcSegment.Angle * PI / 180;
          ArcSegment.GetCenterParameters(PathSegmentGetStartPoint, FCP, FStartAngle, FDeltaAngle);
        end;
      end;
  end;

  if assigned(FCache) then
  begin
    SaveCTM := FRoot.CTM;
    try
      FRoot.CTMRestore(FCache.CTM);
      FCache.ScreenBBox := FSVGObject.CalcBBoxCTM(FRoot, [biFill, biStroke]);
    finally
      FRoot.CTMRestore(SaveCTM);
    end;
  end;

  UpdateBitmap;
  Invalidate;
end;

procedure TSVGEditorToolShape.DoSetHandlePoint(const aIndex: Integer;
  const Value: TPoint);
var
  P: TSVGPoint;
  R: TSVGRect;
  SaveCTM: TSVGMatrix;
  Segment: ISVGPathSegment;
  LineSegment: ISVGLineSegment;
  BezierSegment: ISVGBezierSegment;
  QuadSegment: ISVGQuadSegment;
  ArcSegment: ISVGArcSegment;
  NewAngle: TSVGFloat;
begin
  P := SVGPoint(Value.X, Value.Y);
  P := ToolToSVG(P);

  case FSVGObject.ElementType of
    elRect:
      begin
        R := GetObjectBounds;
        if R.IsUndefined then
          Exit;

        case aIndex of
          0: R := SVGRect(P.X, P.Y, R.Right, R.Bottom);
          1: R := SVGRect(R.Left, P.Y, P.X, R.Bottom);
          2: R := SVGRect(R.Left, R.Top, P.X, P.Y);
          else
             R := SVGRect(P.X, R.Top, R.Right, P.Y);
        end;

        SetObjectBounds(R);
      end;

    elCircle:
      begin
        R := GetObjectBounds;
        if R.IsUndefined then
          Exit;

        R := SVGRect(R.Left - P.X + R.Right, R.Top, P.X, R.Bottom);

        SetObjectBounds(R);
      end;

    elEllipse:
      begin
        R := GetObjectBounds;
        if R.IsUndefined then
          Exit;

        case aIndex of
          0: R := SVGRect(R.Left - P.X + R.Right, R.Top, P.X, R.Bottom);
          else
             R := SVGRect(R.Left, R.Top - P.Y + R.Bottom, R.Right, P.Y);
        end;

        SetObjectBounds(R);
      end;

    elPolyLine,
    elPolygon,
    elPath:
      begin
        Segment := PathSegment;
        if assigned(Segment) then
        begin
          if Supports(Segment, ISVGLineSegment, LineSegment) then
          begin
            case aIndex of
              0: PathSegmentSetStartPoint(P);
              1: LineSegment.Point := P;
            end;
          end else

          if Supports(Segment, ISVGBezierSegment, BezierSegment) then
          begin
            case aIndex of
              0: PathSegmentSetStartPoint(P);
              1: BezierSegment.Point1 := P;
              2: BezierSegment.Point2 := P;
              3: BezierSegment.Point3 := P;
            end;
          end else

          if Supports(Segment, ISVGQuadSegment, QuadSegment) then
          begin
            case aIndex of
              0: PathSegmentSetStartPoint(P);
              1: QuadSegment.Point1 := P;
              2: QuadSegment.Point2 := P;
            end;
          end else

          if Supports(Segment, ISVGArcSegment, ArcSegment) then
          begin
            case aIndex of
              0: begin
                   FCP := P;
                 end;
              1: begin
                   FR := SVGPoint(DistanceBetween2Pts(FCP, P), FR.Y);
                   FAngle := CalcAngle(FCP, P);
                 end;
              2: begin
                   FR := SVGPoint(FR.X, DistanceBetween2Pts(FCP, P));
                   FAngle := CalcAngle(FCP, P) - PI / 2;
                 end;
              3: begin
                   NewAngle := CalcAngle(FCP, P) - FAngle;

                   FDeltaAngle := FStartAngle + FDeltaAngle - NewAngle;
                   FStartAngle := NewAngle;
                 end;
              4: begin
                   NewAngle := CalcAngle(FCP, P) - FAngle;

                   FDeltaAngle := NewAngle - FStartAngle;
                 end;
            end;

            PathSegmentSetStartPoint(ArcSegment.SetCenterParameters(FCP, FR, FAngle, FStartAngle, FDeltaAngle));
          end;

          case FSVGObject.ElementType of
            elPolyLine:
              SetAttrPathData(PathGeometry.AsPolylineString);
            elPolygon:
              SetAttrPathData(PathGeometry.AsPolygonString);
            elPath:
              SetAttrPathData(PathGeometry.AsPathString);
          end;
        end;
      end;
  end;

  if assigned(FCache) then
  begin
    SaveCTM := FRoot.CTM;
    try
      FRoot.CTMRestore(FCache.CTM);
      FCache.ScreenBBox := FSVGObject.CalcBBoxCTM(FRoot, [biFill, biStroke]);
    finally
      FRoot.CTMRestore(SaveCTM);
    end;
  end;

  CalcBounds;
  UpdateBitmap;
  UpdateToolParts;
  Invalidate;
end;

function TSVGEditorToolShape.GetHandlePoint(const aIndex: Integer): TPoint;
var
  P, PrevPoint: TSVGPoint;
  Rect: ISVGRect;
  Circle: ISVGCircle;
  Ellipse: ISVGEllipse;
  Segment: ISVGPathSegment;
  LineSegment: ISVGLineSegment;
  BezierSegment: ISVGBezierSegment;
  QuadSegment: ISVGQuadSegment;
  ArcSegment: ISVGArcSegment;
begin
  P := SVGPoint(0.0, 0.0);

  if Supports(FSVGObject, ISVGRect, Rect) then
  begin
    case aIndex of
      0: P := SVGPoint(CalcX(Rect.X), CalcY(Rect.Y));
      1: P := SVGPoint(CalcX(Rect.X) + CalcX(Rect.Width), CalcY(Rect.Y));
      2: P := SVGPoint(CalcX(Rect.X) + CalcX(Rect.Width), CalcY(Rect.Y) + CalcY(Rect.Height));
      else
         P := SVGPoint(CalcX(Rect.X), CalcY(Rect.Y) + CalcY(Rect.Height));
    end;
  end else

  if Supports(FSVGObject, ISVGCircle, Circle) then
  begin
    P := SVGPoint(CalcX(Circle.CX) + CalcX(Circle.R), CalcY(Circle.CY));
  end else

  if Supports(FSVGObject, ISVGEllipse, Ellipse) then
  begin
    case aIndex of
      0: P := SVGPoint(CalcX(Ellipse.CX) + CalcX(Ellipse.RX), CalcY(Ellipse.CY) );
      else
         P := SVGPoint(CalcX(Ellipse.CX), CalcY(Ellipse.CY) + CalcY(Ellipse.RY));
    end;
  end else

  case FSVGObject.ElementType of
    elPolyLine,
    elPolygon,
    elPath:
      begin
        Segment := PathSegment;

        if assigned(Segment) then
        begin
          PrevPoint := PathSegmentGetStartPoint;

          if Supports(Segment, ISVGLineSegment, LineSegment) then
          begin
            case aIndex of
              0: P := PrevPoint;
              1: P := LineSegment.Point;
            end;

          end else

          if Supports(Segment, ISVGBezierSegment, BezierSegment) then
          begin
            case aIndex of
              0: P := PrevPoint;
              1: P := BezierSegment.Point1;
              2: P := BezierSegment.Point2;
              3: P := BezierSegment.Point3;
            end;
          end else

          if Supports(Segment, ISVGQuadSegment, QuadSegment) then
          begin
            case aIndex of
              0: P := PrevPoint;
              1: P := QuadSegment.Point1;
              2: P := QuadSegment.Point2;
            end;
          end else

          if Supports(Segment, ISVGArcSegment, ArcSegment) then
          begin
            case aIndex of
              0: P := FCP;
              1: begin
                   P := SVGPoint(
                     FCP.X + FR.X * Cos(FAngle),
                     FCP.Y + FR.X * Sin(FAngle));
                 end;
              2: begin
                   P := SVGPoint(
                     FCP.X - FR.Y * Sin(FAngle),
                     FCP.Y + FR.Y * Cos(FAngle));
                 end;
              3: begin
                   P := ArcSegment.GetEllipsePointForAngle(FCP, FR, FAngle, FStartAngle);
                 end;
              4: begin
                   P := ArcSegment.GetEllipsePointForAngle(FCP, FR, FAngle, FStartAngle + FDeltaAngle);
                 end;
            end;
          end;

        end;
      end;
  end;

  Result := SVGToTool(P).Round;
end;

function TSVGEditorToolShape.GetObjectBounds: TSVGRect;
var
  Rect: ISVGRect;
  Circle: ISVGCircle;
  Ellipse: ISVGEllipse;
begin
  if Supports(FSVGObject, ISVGRect, Rect) then
  begin
    Result := SVGRect(
      CalcX(Rect.X),
      CalcY(Rect.Y),
      CalcX(Rect.X) + CalcX(Rect.Width),
      CalcY(Rect.Y) + CalcY(Rect.Height));
  end else

  if Supports(FSVGObject, ISVGCircle, Circle) then
  begin
    Result := SVGRect(
      CalcX(Circle.CX) - CalcX(Circle.R),
      CalcY(Circle.CY) - CalcY(Circle.R),
      CalcX(Circle.CX) + CalcX(Circle.R),
      CalcY(Circle.CY) + CalcY(Circle.R));
  end else

  if Supports(FSVGObject, ISVGEllipse, Ellipse) then
  begin
    Result := SVGRect(
      CalcX(Ellipse.CX) - CalcX(Ellipse.RX),
      CalcY(Ellipse.CY) - CalcY(Ellipse.RY),
      CalcX(Ellipse.CX) + CalcX(Ellipse.RX),
      CalcY(Ellipse.CY) + CalcY(Ellipse.RY));
  end else

    Result := TSVGRect.CreateUndefined;
end;

function TSVGEditorToolShape.GetPathFigure: ISVGPathFigure;
begin
  Result := nil;

  if not assigned(FPathGeometry) then
    Exit;

  if FFigureIndex < FPathGeometry.Figures.Count then
    Result := FPathGeometry.Figures[FFigureIndex];
end;

function TSVGEditorToolShape.GetPathSegment: ISVGPathSegment;
var
  Figure: ISVGPathFigure;
begin
  Result := nil;

  if not assigned(FPathGeometry) then
    Exit;

  Figure := PathFigure;
  if assigned(Figure) and (FSegmentIndex < Figure.Segments.Count) then
    Result := Figure.Segments[FSegmentIndex];
end;

procedure TSVGEditorToolShape.Init;
begin
  inherited;

  case FSVGObject.ElementType of
    elRect:
      begin
        CmdListAddAttribute('x');
        CmdListAddAttribute('y');
        CmdListAddAttribute('width');
        CmdListAddAttribute('height');
      end;

    elCircle:
      begin
        CmdListAddAttribute('cx');
        CmdListAddAttribute('cy');
        CmdListAddAttribute('r');
      end;

    elEllipse:
      begin
        CmdListAddAttribute('cx');
        CmdListAddAttribute('cy');
        CmdListAddAttribute('rx');
        CmdListAddAttribute('ry');
      end;

    elPolygon,
    elPolyline:
      begin
        CmdListAddAttribute('points');
      end;

    elPath:
      begin
        CmdListAddAttribute('d');
      end;
  end;
end;

procedure TSVGEditorToolShape.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if FSVGObject.ElementType in [elPath, elPolygon, elPolyline] then
  begin
      if (ssAlt in Shift) then
      begin
        if Key = 78 then // n
        begin
          ClearToolParts;
          try
            PathSegmentNext;
          finally
            CreateToolParts;
            UpdateBitmap;
          end;
        end else

        if Key = 80 then // p
        begin
          ClearToolParts;
          try
            PathSegmentPrev;
          finally
            CreateToolParts;
            UpdateBitmap;
          end;
        end;

      end;
  end;

  inherited;
end;

procedure TSVGEditorToolShape.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P: TSVGPoint;
  Figure: ISVGPathFigure;
begin
  case Mode of
    tsmAddLines:
      begin
        if FSVGObject.ElementType in [elPath, elPolygon, elPolyline] then
        begin
          ClearToolParts;
          try

            P := SVGPoint(X, Y);
            P := ToolToSVG(P);

            if FPathGeometry.Figures.Count = 0 then
            begin
              Figure := FPathGeometry.AddFigure(P);
              FFigureIndex := FPathGeometry.Figures.Count - 1;
            end else
              //Figure := PathFigure;
              Exit;

            Figure.AddLine(P);
            FSegmentIndex := Figure.Segments.Count - 1;
          finally
            CreateToolParts;
            UpdateBitmap;
          end;
        end;
      end;
  end;

  inherited;
end;

procedure TSVGEditorToolShape.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TSVGPoint;
  i,  j: Integer;
  Figure: ISVGPathFigure;
begin
  case Mode of
    tsmAddLines:
      begin
        if FSVGObject.ElementType in [elPath, elPolygon, elPolyline] then
        begin
          Figure := PathFigure;

          if assigned(Figure) then
            DoSetHandlePoint(Figure.Segments.Count - 1, Point(X, Y));
        end;
      end;
    tsmEdit:
      begin
        if FSVGObject.ElementType in [elPath, elPolygon, elPolyline] then
        begin
          P := SVGPoint(X, Y);
          P := ToolToSVG(P);

          if not(ssLeft in Shift) and not(ssRight in Shift) then
          begin
            if FPathGeometry.SegmentAtPt(P, i, j)
            and ((i <> FFigureIndex) or (j <> FSegmentIndex)) then
            begin
              ClearToolParts;
              try
                FFigureIndex := i;
                FSegmentIndex := j;
              finally
                CreateToolParts;
                UpdateBitmap;
              end;
            end;
          end;
        end;
      end;
  end;

  inherited;
end;

function TSVGEditorToolShape.PathSegmentGetStartPoint: TSVGPoint;
var
  Figure: ISVGPathFigure;
begin
  Figure := PathFigure;
  if assigned(Figure) then
  begin
    if FSegmentIndex > 0 then
      Result := Figure.Segments[FSegmentIndex - 1].EndPoint
    else
      Result := Figure.StartPoint;
  end else
    Result := SVGPoint(0.0, 0.0);
end;

function TSVGEditorToolShape.PathSegmentNext: ISVGPathSegment;
var
  Figure: ISVGPathFigure;
begin
  Figure := PathFigure;
  if assigned(Figure) then
  begin
    Inc(FSegmentIndex);
    if FSegmentIndex >= Figure.Segments.Count then
    begin
      FSegmentIndex := 0;
      Inc(FFigureIndex);
      if FFigureIndex >= FPathGeometry.Figures.Count then
        FFigureIndex := 0;
    end;
  end;
end;

function TSVGEditorToolShape.PathSegmentPrev: ISVGPathSegment;
var
  Figure: ISVGPathFigure;
begin
  Dec(FSegmentIndex);
  if FSegmentIndex < 0 then
  begin
    Dec(FFigureIndex);
    if FFigureIndex < 0 then
      FFigureIndex := FPathGeometry.Figures.Count - 1;

    Figure := FPathGeometry.Figures[FFigureIndex];
    FSegmentIndex := Figure.Segments.Count - 1;
  end;
end;

procedure TSVGEditorToolShape.PathSegmentSetStartPoint(const aPoint: TSVGPoint);
var
  Figure: ISVGPathFigure;
begin
  Figure := PathFigure;
  if assigned(Figure) then
  begin
    if FSegmentIndex > 0 then
      Figure.Segments[FSegmentIndex - 1].EndPoint := aPoint
    else
      Figure.StartPoint := aPoint;
  end;
end;

procedure TSVGEditorToolShape.SetAttrPathData(const aValue: TSVGUnicodeString);
var
  Path: ISVGPath;
  Polygon: ISVGPolygon;
  Polyline: ISVGPolyline;
  Attr: ISVGAttribute;
  //PathShape: ISVGPathShape;
  //PathDataList: ISVGPathDataList;
  //PathData: ISVGPathData;
begin
  { Update 10
  CmdListSetAttribute('d', aValue);

  if Supports(FSVGObject, ISVGPathShape, PathShape) then
  begin
    PathDataList := TSVGRenderContextManager.CreatePathDataList;
    PathData := PathDataList.CreatePathData;
    PathDataList.Add(PathData);

    FPathGeometry.ConvertToPathData(PathData as ISVGPathDataSink);

    PathShape.SetPathDataList(FRoot, PathDataList);
    Exit;
  end;}

  // Work around because of a bug
  if Supports(FSVGObject, ISVGPath, Path) then
  begin
    CmdListSetAttribute('d', aValue);

    if Path.TryGetAttribute('d', Attr) then
      Attr.Value.Parse(FRoot, aValue);
  end else

  if Supports(FSVGObject, ISVGPolygon, Polygon) then
  begin
    CmdListSetAttribute('points', aValue);

    if Polygon.TryGetAttribute('points', Attr) then
      Attr.Value.Parse(FRoot, aValue);
  end else

  if Supports(FSVGObject, ISVGPolyline, Polyline) then
  begin
    CmdListSetAttribute('points', aValue);

    if Polyline.TryGetAttribute('points', Attr) then
      Attr.Value.Parse(FRoot, aValue);
  end;
end;

procedure TSVGEditorToolShape.SetMode(const Value: TSVGToolShapeMode);
begin
  FMode := Value;
end;

procedure TSVGEditorToolShape.SetObjectBounds(aRect: TSVGRect);
var
  Rect: ISVGRect;
  Circle: ISVGCircle;
  Ellipse: ISVGEllipse;
begin
  if Supports(FSVGObject, ISVGRect, Rect) then
  begin
    CmdListSetAttribute('x', CalcX(aRect.Left, Rect.X.DimType));
    CmdListSetAttribute('y', CalcY(aRect.Top, Rect.X.DimType));
    CmdListSetAttribute('width', CalcX(aRect.Right - aRect.Left, Rect.Width.DimType));
    CmdListSetAttribute('height', CalcX(aRect.Bottom - aRect.Top, Rect.Height.DimType));
  end else

  if Supports(FSVGObject, ISVGCircle, Circle) then
  begin
    CmdListSetAttribute('cx', CalcX((aRect.Left + aRect.Right) / 2, Circle.CX.DimType));
    CmdListSetAttribute('cy', CalcY((aRect.Top + aRect.Bottom) / 2, Circle.CY.DimType));
    CmdListSetAttribute('r', CalcX(aRect.Width / 2, Circle.R.DimType));
  end else

  if Supports(FSVGObject, ISVGEllipse, Ellipse) then
  begin
    CmdListSetAttribute('cx', CalcX((aRect.Left + aRect.Right) / 2, Ellipse.CX.DimType));
    CmdListSetAttribute('cy', CalcY((aRect.Top + aRect.Bottom) / 2, Ellipse.CY.DimType));
    CmdListSetAttribute('rx', CalcX(aRect.Width / 2, Ellipse.RX.DimType));
    CmdListSetAttribute('ry', CalcY(aRect.Height / 2, Ellipse.RX.DimType));
  end else

  if FSVGObject.ElementType = elPath then
  begin
    // See SetAttrPathData
  end;
end;

// -----------------------------------------------------------------------------
//
//                           TSVGEditorToolDraw
//
// -----------------------------------------------------------------------------

constructor TSVGEditorToolDraw.Create(aEditor: TSVGEditor; aRoot: ISVGRoot;
  aObject: ISVGObject; aParentCache: ISVGObjectCache);
begin
  FPointList := TList<TSVGPoint>.Create;
  FBoundsRect := TSVGRect.CreateUndefined;
  FPolygon := nil;

  inherited Create(aEditor, aRoot, aObject, aParentCache);
end;

procedure TSVGEditorToolDraw.CreateToolPartsInternal;
var
  ToolLine: TSVGToolLine;
  ToolHandle: TSVGToolHandle;
begin
  ClearToolParts;

  ToolLine := TSVGToolLine.Create(Self, 1, 2);
  ToolLine.Parent := Parent;
  GuideList.Add(ToolLine);

  ToolHandle := TSVGToolHandle.Create(Self, 0, 4, svg_handle_shape_1);
  ToolHandle.Parent := Parent;
  HandleList.Add(ToolHandle);

  ToolHandle := TSVGToolHandle.Create(Self, 1, 4, svg_handle_shape_1);
  ToolHandle.Parent := Parent;
  HandleList.Add(ToolHandle);

  ToolHandle := TSVGToolHandle.Create(Self, 2, 4, svg_handle_shape_3);
  ToolHandle.Parent := Parent;
  HandleList.Add(ToolHandle);
end;

destructor TSVGEditorToolDraw.Destroy;
begin
  FPointList.Free;

  inherited;
end;

procedure TSVGEditorToolDraw.DoCreateToolParts;
begin
  inherited;
end;

procedure TSVGEditorToolDraw.DoHandleMouseDown(const aIndex: integer;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
  Handle: TSVGToolHandle;
begin
  inherited;

  if aIndex = 2 then
  begin
    P := Point(HandleList[aIndex].Left + X, HandleList[aIndex].Top + Y);

    if ssDouble in Shift then
    begin
      Handle := HandleList[0];

      if PtInRect(Handle.BoundsRect, P) then
        FEditor.ElementAdd('<polygon points="' + GetPoints + '" />', False)
      else
        FEditor.ElementAdd('<polyline points="' + GetPoints + '" />', False)
    end else
      MouseDown(Button, Shift, P.X - Left, P.Y - Top);
  end;
end;

procedure TSVGEditorToolDraw.DoHandleMouseMove(const aIndex: integer;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
  Handle: TSVGToolHandle;
begin
  inherited;

  if aIndex = 2 then
  begin
    P := Point(HandleList[aIndex].Left + X, HandleList[aIndex].Top + Y);

    MouseMove(Shift, P.X - Left, P.Y - Top);

    Handle := HandleList[0];

    if PtInRect(Handle.BoundsRect, P) then
      Handle.SVG.Text := svg_handle_shape_2
    else
      Handle.SVG.Text := svg_handle_shape_1;
  end;
end;

procedure TSVGEditorToolDraw.DoSetHandlePoint(const aIndex: Integer;
  const Value: TPoint);
begin
  //inherited;

  if aIndex = 2 then
  begin
    FNewPoint := SVGPoint(Value.X, Value.Y);
    FNewPoint := ToolToSVG(FNewPoint);
  end;

  UpdateToolParts;
end;

function TSVGEditorToolDraw.GetHandlePoint(const aIndex: Integer): TPoint;
begin
  if (aIndex = 0) and (FPointList.Count > 0) then
    Result := SVGToTool(FPointList[0]).Round
  else

  if (aIndex = 1) and (FPointList.Count > 0) then
    Result := SVGToTool(FPointList[FPointList.Count - 1]).Round;

  if aIndex = 2 then
    Result := SVGToTool(FNewPoint).Round;
end;

function TSVGEditorToolDraw.GetPoints: TSVGUnicodeString;
var
  i: Integer;
  M: TSVGMatrix;
  P: TSVGPoint;
begin
  M := FCache.CTM;
  M := TSVGMatrix.Multiply(M, FCache.ViewportMatrix);
  M := M.Inverse;

  Result := '';
  for i := 0 to FPointList.Count - 1 do
  begin
    P := TransformPoint(FPointList[i], M);

    if Result <> '' then
      Result := Result + ' ';
    Result := Result + Format('%g,%g', [P.X, P.Y], USFormatSettings);
  end;
end;

function TSVGEditorToolDraw.GetScreenBBox: TSVGRect;
var
  M: TSVGMatrix;
begin
  M := AlignMatrix;
  M := TSVGMatrix.Multiply(M, TSVGMatrix.CreateTranslation(Margin, Margin));

  //Result := TSVGRect.Union(inherited, TransformRect(FBoundsRect, M.Inverse));
  Result := TSVGRect.Union(FCache.ScreenBBox, FBoundsRect);
end;

procedure TSVGEditorToolDraw.Init;
begin
  inherited;
end;

procedure TSVGEditorToolDraw.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  P: TSVGPoint;
begin
  if (ssLeft in Shift) and not(ssDouble in Shift) then
  begin
    P := SVGPoint(X, Y);
    P := ToolToSVG(P);

    FPointList.Add(P);

    if FPointList.Count = 1 then
    begin
      CreateToolPartsInternal;
      FBoundsRect := TSVGRect.Create(P.X, P.Y, P.X, P.Y);

      //FPolygon := FRoot.CreateElement(EL_POLYGON) as ISVGPolygon;
      //FSVGObject.ChildNodes.Add(FPolygon);

    end else begin
      if P.X < FBoundsRect.Left then
        FBoundsRect.Left := P.X;

      if P.Y < FBoundsRect.Top then
        FBoundsRect.Top := P.Y;

      if P.X > FBoundsRect.Right then
        FBoundsRect.Right := P.X;

      if P.Y > FBoundsRect.Bottom then
        FBoundsRect.Bottom := P.Y;
    end;

    if assigned(FPolygon) then
      FPolygon.Points := GetPoints;

    CalcBounds;
    UpdateBitmap;
  end;

  inherited;
end;

procedure TSVGEditorToolDraw.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  DoSetHandlePoint(2, Point(X, Y));

  inherited;
end;

procedure TSVGEditorToolDraw.PaintRenderContext(aRC: ISVGRenderContext);
var
  i: Integer;
begin
  inherited;

  aRC.ApplyStroke(TSVGSolidBrush.Create(SVGColorGray), 1);

  for i := 1 to FPointList.Count - 1 do
    aRC.DrawLine(
      FPointList[i - 1].X,
      FPointList[i - 1].Y,
      FPointList[i].X,
      FPointList[i].Y
      );
end;

function TSVGEditorToolDraw.SVGToTool(const aPoint: TSVGPoint): TSVGPoint;
var
  M: TSVGMatrix;
begin
  M := AlignMatrix;
  M := TSVGMatrix.Multiply(M, TSVGMatrix.CreateTranslation(Margin, Margin));

  Result := TransformPoint(aPoint, M);
end;

function TSVGEditorToolDraw.ToolToSVG(const aPoint: TSVGPoint): TSVGPoint;
var
  M: TSVGMatrix;
begin
  M := AlignMatrix;
  M := TSVGMatrix.Multiply(M, TSVGMatrix.CreateTranslation(Margin, Margin));

  Result := TransformPoint(aPoint, M.Inverse);
end;

// -----------------------------------------------------------------------------
//
//                             TSVGEditorCmd
//
// -----------------------------------------------------------------------------

constructor TSVGEditorCmd.Create(aCmdType: TSVGEditorCmdType);
begin
  inherited Create;

  FCmdType := aCmdType;
end;

// -----------------------------------------------------------------------------
//
//                           TSVGEditorCmdElementID
//
// -----------------------------------------------------------------------------

constructor TSVGEditorCmdElementID.Create(aCmdType: TSVGEditorCmdType;
  const aID: Integer);
begin
  inherited Create(aCmdType);

  FID := aID;
end;

// -----------------------------------------------------------------------------
//
//                          TSVGEditorCmdElementsSelect
//
// -----------------------------------------------------------------------------

constructor TSVGEditorCmdElementsSelect.Create;
begin
  inherited Create(ctSelectElement);

  FSelect := TList<Integer>.Create;
  FUnselect := TList<Integer>.Create;
end;


destructor TSVGEditorCmdElementsSelect.Destroy;
begin
  FUnselect.Free;
  FSelect.Free;

  inherited;
end;

// -----------------------------------------------------------------------------
//
//                         TSVGEditorCmdElementAdd
//
// -----------------------------------------------------------------------------

constructor TSVGEditorCmdElementAdd.Create(const aParentID,
  aNodeIndex: Integer; const aDOMDocument: IDOMDocument);
begin
  inherited Create(ctAddElement, aParentID, aNodeIndex, aDOMDocument);
end;

// -----------------------------------------------------------------------------
//
//                        TSVGEditorCmdElementRemove
//
// -----------------------------------------------------------------------------

constructor TSVGEditorCmdElementRemove.Create(const aParentID,
  aNodeIndex: Integer; const aDOMDocument: IDOMDocument);
begin
  inherited Create(ctRemoveElement, aParentID, aNodeIndex, aDOMDocument);
end;

// -----------------------------------------------------------------------------
//
//                        TSVGEditorCmdSetAttribute
//
// -----------------------------------------------------------------------------

constructor TSVGEditorCmdSetAttribute.Create(const aID: Integer;
  const aName, aValueOld, aValueNew: TSVGUnicodeString);
begin
  inherited Create(ctSetAttribute, aID);

  FName := aName;
  FValueOld := aValueOld;
  FValueNew := aValueNew;
end;

// -----------------------------------------------------------------------------
//
//                          TSVGEditorCmdDOM
//
// -----------------------------------------------------------------------------

constructor TSVGEditorCmdDOM.Create(aCmdType: TSVGEditorCmdType;
  const aParentID, aNodeIndex: Integer; const aDOMDocument: IDOMDocument);
begin
  inherited Create(aCmdType);

  FDOMDocument := aDOMDocument;
  FParentID := aParentID;
  FNodeIndex := aNodeIndex;
end;

// -----------------------------------------------------------------------------
//
//                          TSVGEditorCmdGroup
//
// -----------------------------------------------------------------------------

constructor TSVGEditorCmdGroup.Create;
begin
  inherited Create(ctGroup);

  FCmdList := TObjectList<TSVGEditorCmd>.Create(TRUE);
end;

destructor TSVGEditorCmdGroup.Destroy;
begin
  FCmdList.Free;

  inherited;
end;

// -----------------------------------------------------------------------------
//
//                         TSVGEditorCmdToolSelect
//
// -----------------------------------------------------------------------------

constructor TSVGEditorCmdToolSelect.Create(const aToolOld, aToolNew: TSVGToolClass);
begin
  inherited Create(ctSelectTool);

  FToolOld := aToolOld;
  FToolNew := aToolNew;
end;

// -----------------------------------------------------------------------------
//
//                               TSVGEditor
//
// -----------------------------------------------------------------------------

procedure TSVGEditor.CalcCanvasRect;
var
  SVGWidth, SVGHeight: TSVGDimension;
  W, H: Integer;
  R: TSVGRect;
  CWidth, CHeight: Integer;
begin
  // Calculate the size of the canvas

  FPadding.OnChange := nil;
  try
    FPadding.Left := 100;
    FPadding.Top := 100;
    FPadding.Right := 100;
    FPadding.Bottom := 100;

    CWidth := ControlSize(SB_HORZ, False, False);
    CHeight := ControlSize(SB_VERT, False, False);

    W := CWidth - FPadding.Right - FPadding.Left - 1;
    H := CHeight - FPadding.Bottom - FPadding.Top - 1;

    R := SVGRect(0, 0, W, H);

    if assigned(FRoot) then
    begin
      if FRoot.SVG.HasAttribute('width') then
        SVGWidth := FRoot.SVG.Width
      else
        SVGWidth := SVGDim(100, dimPerc);

      if FRoot.SVG.HasAttribute('height') then
        SVGHeight := FRoot.SVG.Height
      else
        SVGHeight := SVGDim(100, dimPerc);

      if SVGWidth.DimType = dimPerc then
        FContentScale.X := SVGWidth.DimValue / 100
      else
        FContentScale.X := FScale;

      if SVGHeight.DimType = dimPerc then
        FContentScale.Y := SVGHeight.DimValue / 100
      else
        FContentScale.Y := FScale;

      R := FRoot.CalcIntrinsicSize(R);

      R.Width := R.Width * FScale;
      R.Height := R.Height * FScale;

      // Increase padding to fill the available area

      if R.Width < W then
      begin
        FPadding.Left := (CWidth - Ceil(R.Width)) div 2;
        FPadding.Right := FPadding.Left - 1;
      end;

      if R.Height < H then
      begin
        FPadding.Top := (CHeight - Ceil(R.Height)) div 2;
        FPadding.Bottom := FPadding.Top - 1;
      end;
    end;

    CanvasRect := Rect(0, 0,
      Ceil(R.Width) + FPadding.Left + FPadding.Right,
      Ceil(R.Height) + FPadding.Top + FPadding.Bottom);

    FTopLeft := Point(0, 0);

    FMatrix := TSVGMatrix.Multiply(
      TSVGMatrix.CreateScaling(FContentScale.X, FContentScale.Y),
      TSVGMatrix.CreateTranslation(
        Round(FPadding.Left), Round(FPadding.Top))
      );

    FInvMatrix := FMatrix.Inverse;
  finally
    FPadding.OnChange := DoPaddingChange;
  end;
end;

procedure TSVGEditor.ChildListBeforeCreate(aParent: IXMLNode);
var
  i: Integer;
  Element: ISVGElement;
begin
  if assigned(FChildListBefore) then
    FChildListBefore.Free;

  FChildListBefore := TList<ISVGElement>.Create;

  for i := 0 to aParent.ChildNodes.Count - 1 do
    if Supports(aParent.ChildNodes[i], ISVGElement, Element) then
      FChildListBefore.Add(Element);
end;

procedure TSVGEditor.Clear;
begin
  ToolsClear;

  CmdUndoStackClear;
  CmdRedoStackClear;

  FSelectedElementList.Clear;

  FElementList.Clear;
  FMaxID := -1;
end;

procedure TSVGEditor.CmdExec(aCmd: TSVGEditorCmd; const aUndo: Boolean);
begin
  ToolsClear;
  try
    CmdExecInternal(aCmd, aUndo);

    DoElementSelect(nil);
  finally
    ToolsCreate;
  end;
end;

procedure TSVGEditor.CmdExecInternal(aCmd: TSVGEditorCmd; const aUndo: Boolean);
var
  i: Integer;
  CmdGroup: TSVGEditorCmdGroup;
begin
  case aCmd.CmdType of
    ctSelectElement:
      CmdExecElementSelect(aCmd as TSVGEditorCmdElementsSelect, aUndo);

    ctAddElement:
      if aUndo then
        CmdExecElementRemove(aCmd as TSVGEditorCmdDOM)
      else
        CmdExecElementAdd(aCmd as TSVGEditorCmdDOM);

    ctRemoveElement:
      if aUndo then
        CmdExecElementAdd(aCmd as TSVGEditorCmdDOM)
      else
        CmdExecElementRemove(aCmd as TSVGEditorCmdDOM);

    ctSetAttribute:
      CmdExecSetAttribute(aCmd as TSVGEditorCmdSetAttribute, aUndo);

    ctSelectTool:
      CmdExecToolSelect(aCmd as TSVGEditorCmdToolSelect, aUndo);

    ctGroup:
      begin
        CmdGroup := aCmd as TSVGEditorCmdGroup;
        if aUndo then
        begin
          for i := CmdGroup.CmdList.Count - 1 downto 0 do
            CmdExecInternal(CmdGroup.CmdList[i], aUndo);

        end else begin
          for i := 0 to CmdGroup.CmdList.Count - 1 do
            CmdExecInternal(CmdGroup.CmdList[i], aUndo);
        end;
      end;
  end;
end;

procedure TSVGEditor.CmdExecElementAdd(aCmd: TSVGEditorCmdDOM);
var
  i: Integer;
  Parent, Element: ISVGElement;
  DOMNode: IDOMNode;

  procedure AddNodeID(const aNode: IXMLNode);
  var
    i: Integer;
    Element: ISVGElement;
  begin
    if Supports(aNode, ISVGElement, Element) then
      FElementList.Add(GetElementID(Element), Element);

    for i := 0 to aNOde.ChildNodes.Count - 1 do
      AddNodeID(aNode.ChildNodes[i]);
  end;

begin
  if FSelectedElementList.Count <> 1 then
    raise Exception.Create('Selected node list <> 1');

  Parent := FElementList[aCmd.ParentID];

  ChildListBeforeCreate(Parent);
  try
    DOMNode := aCmd.DomDocument.documentElement.firstChild;
    while assigned(DOMNode) do
    begin
      XMLImportNode(DOMNode, Parent, True, aCmd.NodeIndex);

      DOMNode := DOMNode.nextSibling;
    end;

    for i := 0 to Parent.ChildNodes.Count - 1 do
    begin
      if Supports(Parent.ChildNodes[i], ISVGElement, Element) then
        if FChildListBefore.IndexOf(Element) = -1 then
        begin
          AddNodeID(Element);
          DoElementAdd(Parent, aCmd.NodeIndex, Element);
        end;
    end;

  finally
    FreeAndNil(FChildListBefore);
  end;

  UpdatePage([rsCalcCache]);
end;

procedure TSVGEditor.CmdExecElementRemove(aCmd: TSVGEditorCmdDOM);
var
  i: Integer;
  DOMElement: IDOMElement;
  DOMNode: IDOMNode;
  Parent, Element: ISVGElement;
  IDS: TSVGUnicodeString;
  ID: Integer;

  procedure RemoveNodeID(aNode: IXMLNode);
  var
    i: Integer;
    Element: ISVGElement;
  begin
    if Supports(aNode, ISVGElement, Element) then
      FElementList.Remove(GetElementID(Element));

    for i := 0 to aNode.ChildNodes.Count - 1 do
      RemoveNodeID(aNode.ChildNodes[i]);
  end;

begin
  DOMNode := aCmd.DomDocument.documentElement.firstChild;
  while assigned(DOMNode) do
  begin
    if Supports(DOMNode, IDOMElement, DOMElement) then
    begin
      IDS := DOMElement.getAttributeNS(ns_svg_editor, at_local_id);
      ID := StrToInt(IDS);

      Element := FElementList[ID];

      RemoveNodeID(Element);

      if Supports(Element.ParentNode, ISVGElement, Parent) then
      begin
        ChildListBeforeCreate(Parent);
        try
          Parent.ChildNodes.Remove(Element);

          for i := 0 to FChildListBefore.Count - 1 do
          begin
            if Parent.ChildNodes.IndexOf(FChildListBefore[i]) = -1 then
              DoElementRemove(FChildListBefore[i]);
          end;

        finally
          FreeAndNil(FChildListBefore);
        end;
      end;
    end;

    DOMNode := DOMNode.nextSibling;
  end;

  UpdatePage([rsCalcCache]);
end;

procedure TSVGEditor.CmdExecElementSelect(aCmd: TSVGEditorCmdElementsSelect;
  const aUndo: Boolean);
var
  ID: Integer;
begin
  if aUndo then
  begin

    for ID in aCmd.Select do
      FSelectedElementList.Remove(ID);

    for ID in aCmd.Unselect do
      FSelectedElementList.Add(ID);

  end else begin

    for ID in aCmd.Unselect do
      FSelectedElementList.Remove(ID);

    for ID in aCmd.Select do
      FSelectedElementList.Add(ID);
  end;
end;

procedure TSVGEditor.CmdExecSetAttribute(aCmd: TSVGEditorCmdSetAttribute;
  const aUndo: Boolean);
var
  Element: ISVGElement;
  Value: TSVGUnicodeString;
begin
  Element := FElementList[aCmd.ID];

  if aUndo then
    Value := aCmd.ValueOld
  else
    Value := aCmd.ValueNew;

  Element.Attributes[aCmd.Name] := Value;

  UpdatePage([rsCalcCache]);

  DoSetAttribute(Element, aCmd.Name, Value);
end;

procedure TSVGEditor.CmdExecToolSelect(aCmd: TSVGEditorCmdToolSelect;
  const aUndo: Boolean);
begin
  if aUndo then
    FCurTool := aCmd.ToolOld
  else
    FCurTool := aCmd.ToolNew;

  DoToolSelect(FCurTool);
end;

function TSVGEditor.CmdElementAddCreate(const aParent: ISVGElement;
  const aNodeIndex: Integer; const aFragment: string): TSVGEditorCmdElementAdd;
const
  Harnas =
    '<?xml version="1.0" encoding="UTF-8" standalone="no"?>'
  + '<svg'
    + ' xmlns="http://www.w3.org/2000/svg"'
    + ' xmlns:svg="http://www.w3.org/2000/svg"'
    + ' xmlns:svge="' + ns_svg_editor + '"'
    + '>%s</svg>';
var
  StrStream: TStringStream;
  ParentDoc, Doc: IDOMDocument;
  DOMNode: IDOMNode;
  Persist: IDOMPersist;
  ParentID: Integer;

  procedure SetNodeID(aDOMNode: IDOMNode);
  var
    DOMNode: IDOMNode;
    DOMElement: IDOMElement;
  begin
    if Supports(aDOMNode, IDOMElement, DOMElement) then
    begin
      Inc(FMaxID);

      DOMElement.setAttributeNS(ns_svg_editor, prefix_local_id + ':' +  at_local_id, IntToStr(FMaxID));
    end;

    DOMNode := aDOMNode.firstChild;
    while assigned(DOMNode) do
    begin
      SetNodeID(DOMNode);

      DOMNode := DOMNode.nextSibling;
    end;
  end;

begin
  {$IFnDEF FPC}
  StrStream := TStringStream.Create(Format(Harnas, [aFragment]), TEncoding.UTF8);
  {$ELSE}
  StrStream := TStringStream.Create(Format(Harnas, [aFragment]));
  {$ENDIF}
  try
    if StrStream.Size = 0 then
      raise Exception.Create('Invalid SVG fragment.');

    ParentDoc := FRoot.SVG.DOMNode.ownerDocument;
    Doc := ParentDoc.domImplementation.createDocument('', '', nil);
    if not Supports(Doc, IDOMPersist, Persist) then
      raise Exception.Create('Dom implementation does not allow parsing.');

    Persist.loadFromStream(StrStream);
  finally
    StrStream.Free;
  end;

  DOMNode := Doc.documentElement.firstChild;
  while assigned(DOMNode) do
  begin
    SetNodeID(DOMNode);

    DOMNode := DOMNode.nextSibling;
  end;

  ParentID := GetElementID(aParent);

  Result := TSVGEditorCmdElementAdd.Create(ParentID, aNodeIndex, Doc);
end;

function TSVGEditor.CmdElementRemoveCreate(
  const aElement: ISVGElement): TSVGEditorCmdElementRemove;
var
  DOMDoc: IDOMDocument;
  DOMNode: IDOMNode;
  DOMAttr: IDOMAttr;
  Parent: ISVGElement;
  ParentID: Integer;
  NodeIndex: Integer;
begin
  DOMNode := aElement.DOMNode;

  DOMDoc := DOMNode.ownerDocument.domImplementation.createDocument('', '', nil);
  DOMDoc.documentElement := DOMDoc.createElement(EL_SVG);

  DOMAttr := DOMDoc.createAttributeNS('http://www.w3.org/2000/svg', 'xmlns');
  DOMDoc.documentElement.setAttributeNode(DOMAttr);
  DOMAttr := DOMDoc.createAttributeNS('http://www.w3.org/2000/svg', 'xmlns:svg');
  DOMDoc.documentElement.setAttributeNode(DOMAttr);
  DOMAttr := DOMDoc.createAttributeNS(ns_svg_editor, 'xmlns:svge');
  DOMDoc.documentElement.setAttributeNode(DOMAttr);

  DOMDoc.documentElement.appendChild(DOMNode.cloneNode(TRUE));

  if not Supports(aElement.ParentNode, ISVGElement, Parent) then
    raise Exception.Create('Parent is not an element');

  ParentID := GetElementID(Parent);

  NodeIndex := Parent.ChildNodes.IndexOf(aElement);

  Result := TSVGEditorCmdElementRemove.Create(ParentID, NodeIndex, DOMDoc);
end;

procedure TSVGEditor.CmdRedo;
var
  Cmd: TSVGEditorCmd;
begin
  ToolsClear;

  if FCmdRedoStack.Count > 0 then
  begin
    Cmd := FCmdRedoStack.Pop;
    try
      CmdExec(Cmd, False);

      FCmdUndoStack.Push(Cmd);

      except on E:Exception do
      begin
        // Stacks are invalid
        CmdUndoStackClear;
        CmdRedoStackClear;
      end;
    end;
  end;
end;

procedure TSVGEditor.CmdRedoStackClear;
var
  Cmd: TSVGEditorCmd;
begin
  while FCmdRedoStack.Count > 0 do
  begin
    Cmd := FCmdRedoStack.Pop;
    Cmd.Free;
  end;
end;

function TSVGEditor.CmdSetAttributeCreate(const aElement: ISVGElement;
  const aName, aValue: TSVGUnicodeString): TSVGEditorCmdSetAttribute;
var
  ID: Integer;
  CurValue: TSVGUnicodeString;
begin
  CurValue := '';
  if aElement.HasAttribute(aName) then
    CurValue := aElement.Attributes[aName];

  ID := GetElementID(aElement);

  Result := TSVGEditorCmdSetAttribute.Create(ID, aName, CurValue, aValue);
end;

function TSVGEditor.CmdToolSelectCreate(
  const aTool: TSVGToolClass): TSVGEditorCmdToolSelect;
begin
  Result := TSVGEditorCmdToolSelect.Create(FCurTool, aTool);
end;

procedure TSVGEditor.CmdUndo;
var
  Cmd: TSVGEditorCmd;
begin
  ToolsClear;

  if FCmdUndoStack.Count > 0 then
  begin
    Cmd := FCmdUndoStack.Pop;
    try
      CmdExec(Cmd, True);

      FCmdRedoStack.Push(Cmd);

      except on E:Exception do
      begin
        // Stacks are invalid
        CmdUndoStackClear;
        CmdRedoStackClear;
      end;
    end;
  end;
end;

procedure TSVGEditor.CmdUndoStackClear;
var
  Cmd: TSVGEditorCmd;
begin
  while FCmdUndoStack.Count > 0 do
  begin
    Cmd := FCmdUndoStack.Pop;
    Cmd.Free;
  end;
end;

function TSVGEditor.ControlSize(const aScrollbarKind: ShortInt;
  const aControlSB, aAssumeSB: Boolean): Integer;
var
  BorderAdjust: Integer;

  // From TControlScrollBar

  function ScrollBarVisible(Code: Word): Boolean;
  var
    Style: Longint;
  begin
    Style := WS_HSCROLL;
    if Code = SB_VERT then Style := WS_VSCROLL;
    Result := GetWindowLong(Handle, GWL_STYLE) and Style <> 0;
  end;

  function Adjustment(Code, Metric: Word): Integer;
  begin
    Result := 0;
    if not aControlSB then
    begin
      if aAssumeSB and not ScrollBarVisible(Code) then
        Result := -(GetSystemMetrics(Metric) - BorderAdjust)
      else
        if not aAssumeSB and ScrollBarVisible(Code) then
          Result := GetSystemMetrics(Metric) - BorderAdjust;
    end;
  end;

begin
  BorderAdjust := Integer(GetWindowLong(Handle, GWL_STYLE) and
    (WS_BORDER or WS_THICKFRAME) <> 0);
  if aScrollbarKind = SB_VERT then
    Result := ClientHeight + Adjustment(SB_HORZ, SM_CXHSCROLL)
  else
    Result := ClientWidth + Adjustment(SB_VERT, SM_CYVSCROLL);
end;

constructor TSVGEditor.Create(AOwner: TComponent);
begin
  inherited;

  FElementList := TDictionary<Integer, ISVGElement>.Create;
  FMaxID := -1;

  FSelectedElementList := TList<Integer>.Create;

  FTimerUpdatePage := TTimer.Create(Self);
  FTimerUpdatePage.Enabled := False;
  FTimerUpdatePage.Interval := 200;
  FTimerUpdatePage.OnTimer := DoUpdatePage;

  FPadding := TPadding.Create(Self);
  FPadding.Left := 100;
  FPadding.Top := 100;
  FPadding.Right := 100;
  FPadding.Bottom := 100;
  FPadding.OnChange := DoPaddingChange;

  FScale := 1.0;

  FToolList := TList<TSVGEditorTool>.Create;
  FFocusedTool := nil;
  FUpdateRectList := TList<TSVGRect>.Create;

  FChildListBefore := nil;

  FCmdUndoStack := TStack<TSVGEditorCmd>.Create;
  FCmdRedoStack := TStack<TSVGEditorCmd>.Create;

  FBackgroundColor := SVGColorWhite;

  DoubleBuffered := True;

  FCurTool := TSVGEditorToolShape;
end;

procedure TSVGEditor.CreateParams(var params: TCreateParams);
begin
  inherited;

  params.Style := params.Style or WS_VSCROLL or WS_HSCROLL;
end;

destructor TSVGEditor.Destroy;
begin
  ToolsClear;
  FToolList.Free;

  CmdUndoStackClear;
  FCmdUndoStack.Free;

  CmdRedoStackClear;
  FCmdRedoStack.Free;

  FUpdateRectList.Free;

  FSelectedElementList.Free;

  if assigned(FBitmap) then
    FBitmap.Free;

  if assigned(FBitmapSelection) then
    FBitmapSelection.Free;

  if assigned(FChildListBefore) then
    FChildListBefore.Free;

  FElementList.Free;

  FPadding.Free;

  inherited;
end;

procedure TSVGEditor.DoElementAdd(const aParent: ISVGElement;
  const aIndex: Integer; const aElement: ISVGElement);
begin
  if assigned(FOnElementAdd) then
    FOnElementAdd(Self, aParent, aIndex, aElement);
end;

procedure TSVGEditor.DoElementRemove(const aElement: ISVGElement);
begin
  if assigned(FOnElementRemove) then
    FOnElementRemove(Self, aElement);
end;

procedure TSVGEditor.DoElementSelect(const aElement: ISVGElement);
begin
  if assigned(FOnElementSelect) then
    FOnElementSelect(Self);
end;

procedure TSVGEditor.DoPaddingChange(Sender: TObject);
begin
  UpdateScrollbars;

  Repaint;
end;

procedure TSVGEditor.DoSetAttribute(const aElement: ISVGElement; const aName,
  aValue: TSVGUnicodeString);
begin
  if assigned(FOnSetAttribute) then
    FOnSetAttribute(Self, aElement, aName, aValue);
end;

procedure TSVGEditor.DoToolSelect(const aTool: TSVGToolClass);
begin
  if assigned(FOnToolSelect) then
    FOnToolSelect(Self, aTool);
end;

procedure TSVGEditor.DoUpdatePage(Sender: TObject);
begin
  FTimerUpdatePage.Enabled := False;

  ToolsHide;
  try
    CalcCanvasRect;
    UpdatePage([]);
  finally
    ToolsShow;
  end;
end;

procedure TSVGEditor.ElementsUnselectAll;
var
  TempList: TList<Integer>;
begin
  TempList := TList<Integer>.Create;
  try
    SelectedElementList := TempList;
  finally
    TempList.Free;
  end;
end;

procedure TSVGEditor.ElementAdd(const aFragment: string;
  const aSelectAdded: Boolean);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Add(aFragment);

    ElementsAdd(sl, aSelectAdded);
  finally
    sl.Free;
  end;
end;

function TSVGEditor.ElementIsSelected(const aID: Integer): Boolean;
begin
  Result := SelectedElementList.IndexOf(aID) <> -1;
end;

procedure TSVGEditor.ElementListInit(aNode: IXMLNode; var aID: Integer);
var
  i: Integer;
  Element: ISVGElement;
begin
  if Supports(aNode, ISVGElement, Element) then
  begin
    Inc(aID);
    if FElementList.ContainsKey(aID) then
      FElementList[aID] := TSVGElement(Element)
    else
      FElementList.Add(aID, TSVGElement(Element));

    Element.SetAttributeNS(prefix_local_id + ':' + at_local_id, ns_svg_editor, IntToStr(aID));
  end;

  for i := 0 to aNode.ChildNodes.Count - 1 do
    ElementListInit(aNode.ChildNodes[i], aID);
end;

procedure TSVGEditor.ElementsAdd(const aFragments: TStringList;
  const aSelectAdded: Boolean);
var
  i: Integer;
  ID: Integer;
  Parent: ISVGElement;
  CmdGroup: TSVGEditorCmdGroup;
  CmdElementSelect: TSVGEditorCmdElementsSelect;
  CmdElementAdd: TSVGEditorCmdElementAdd;
  DOMNode: IDOMNode;
  DOMElement: IDOMElement;
begin
  if FSelectedElementList.Count <> 1 then
    raise Exception.Create('Select one parent element.');

  ID := FSelectedElementList[0];

  if not Supports(FElementList[ID], ISVGElement, Parent) then
    Exit;

  CmdGroup := TSVGEditorCmdGroup.Create;
  try
    CmdElementSelect := nil;

    if aSelectAdded then
    begin
      CmdElementSelect := TSVGEditorCmdElementsSelect.Create;

      // Unselect current selection
      for ID in FSelectedElementList do
      begin
        CmdElementSelect.Unselect.Add(ID);
      end;
    end;

    for i := 0 to aFragments.Count - 1 do
    begin
      CmdElementAdd := CmdElementAddCreate(Parent, -1, aFragments[i]);

      if assigned(CmdElementSelect) then
      begin
        DOMNode := CmdElementAdd.DOMDocument.documentElement.firstChild;
        while assigned(DOMNOde) do
        begin
          if Supports(DOMNode, IDOMElement, DOMElement) then
            CmdElementSelect.Select.Add(GetElementID(DOMElement));

          DOMNode := DomNode.nextSibling;
        end;
      end;

      CmdGroup.CmdList.Add(CmdElementAdd);
    end;

    if assigned(CmdElementSelect) then
      CmdGroup.CmdList.Add(CmdElementSelect);

    CmdExec(CmdGroup, False);

    FCmdUndoStack.Push(CmdGroup);

    except on E:Exception do
      CmdGroup.Free;
  end;
end;

procedure TSVGEditor.ElementsCopy;
var
  ID: Integer;
  Element: ISVGElement;
  sl: TStringList;
  Formatter: TSVGXmlFormatter;
begin
  if SelectedElementList.Count = 0 then
    Exit;

  sl := TStringList.Create;
  try
    for ID in SelectedElementList do
    begin
      Element := FElementList[ID];

      Formatter := TSVGXmlFormatter.Create([]);
      try
        Formatter.WriteNode(Element.DOMNode);
        sl.Add(Formatter.ToUnicodeString);
      finally
        Formatter.Free;
      end;
    end;

    Clipboard.AsText := sl.Text;
  finally
    sl.Free;
  end;
end;

procedure TSVGEditor.ElementsCut;
var
  ID: Integer;
  Parent, Element: ISVGElement;
  CmdGroup: TSVGEditorCmdGroup;
  CmdElementSelect: TSVGEditorCmdElementsSelect;
  sl: TStringList;
  Formatter: TSVGXmlFormatter;
begin
  if SelectedElementList.Count = 0 then
    Exit;

  // Select the parent of first node that is deleted

  Element := FElementList[SelectedElementList[0]];

  if not Supports(Element.ParentNode, ISVGElement, Parent) then
    Exit;

  CmdGroup := TSVGEditorCmdGroup.Create;
  try
    CmdElementSelect := TSVGEditorCmdElementsSelect.Create;
    CmdGroup.CmdList.Add(CmdElementSelect);

    CmdElementSelect.Select.Add(GetElementID(Parent));

    sl := TStringList.Create;
    try
      for ID in SelectedElementList do
      begin
        Element := FElementList[ID];

        CmdElementSelect.Unselect.Add(GetElementID(Element));
        CmdGroup.CmdList.Add(CmdElementRemoveCreate(Element));

        Formatter := TSVGXmlFormatter.Create([]);
        try
          Formatter.WriteNode(Element.DOMNode);
          sl.Add(Formatter.ToUnicodeString);
        finally
          Formatter.Free;
        end;
      end;

      Clipboard.AsText := sl.Text;
    finally
      sl.Free;
    end;

    CmdExec(CmdGroup, False);

    FCmdUndoStack.Push(CmdGroup);

    except on E:Exception do
      CmdGroup.Free;
  end;
end;

procedure TSVGEditor.ElementsDelete;
var
  ID: Integer;
  Parent, Element: ISVGElement;
  CmdGroup: TSVGEditorCmdGroup;
  CmdElementSelect: TSVGEditorCmdElementsSelect;
begin
  if SelectedElementList.Count = 0 then
    Exit;

  // Select the parent of first node that is deleted

  Element := FElementList[SelectedElementList[0]];

  if not Supports(Element.ParentNode, ISVGElement, Parent) then
    Exit;

  CmdGroup := TSVGEditorCmdGroup.Create;
  try
    CmdElementSelect := TSVGEditorCmdElementsSelect.Create;
    CmdGroup.CmdList.Add(CmdElementSelect);

    CmdElementSelect.Select.Add(GetElementID(Parent));

    for ID in SelectedElementList do
    begin
      Element := FElementList[ID];

      CmdElementSelect.Unselect.Add(ID);
      CmdGroup.CmdList.Add(CmdElementRemoveCreate(Element));
    end;

    CmdExec(CmdGroup, False);

    FCmdUndoStack.Push(CmdGroup);

    except on E:Exception do
      CmdGroup.Free;
  end;
end;

procedure TSVGEditor.ElementsPaste;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Text := Clipboard.AsText;

    if sl.Count = 0 then
      Exit;

    ElementsAdd(sl, True);

  finally
    sl.Free;
  end;
end;

procedure TSVGEditor.ElementsSelect(aList: TList<Integer>);
begin
  SelectedElementList := aList;
end;

function TSVGEditor.GetCmdRedoCount: Integer;
begin
  Result := FCmdRedoStack.Count;
end;

function TSVGEditor.GetCmdUndoCount: Integer;
begin
  Result := FCmdUndoStack.Count;
end;

function TSVGEditor.GetElement(const aID: Integer): ISVGElement;
begin
  Result := FElementList[aID];
end;

function TSVGEditor.GetElementID(aElement: IDOMElement): Integer;
var
  ID: string;
begin
  ID := aElement.getAttributeNS(ns_svg_editor, at_local_id);
  Result := StrToInt(ID);
end;

function TSVGEditor.GetSelectedElement(const aIndex: Integer): Integer;
begin
  if aIndex < SelectedElementList.Count then
    Result := SelectedElementList[aIndex]
  else
    Result := -1;
end;

function TSVGEditor.GetSelectedElementCount: Integer;
begin
  Result := SelectedElementList.Count;
end;

function TSVGEditor.GetElementID(aElement: ISVGElement): Integer;
var
  DOMElement: IDOMElement;
begin
  if Supports(aElement.DOMNode, IDOMElement, DOMElement) then
    Result := GetElementID(DOMElement)
  else
    raise Exception.Create('ISVGElement does not wrap a DOMElement');
end;

procedure TSVGEditor.HandleScrollbar(var aMsg: TWMSCROLL; aBar: Integer);
var
  ScrollInfo: TScrollInfo;
begin
  aMsg.result := 0;
  ScrollInfo.cbSize := Sizeof(TScrollInfo);
  ScrollInfo.fMask := SIF_ALL;
  GetScrollInfo(Handle, aBar, ScrollInfo);
  ScrollInfo.fMask := SIF_POS;

  // For simplicities sake we use 1/10 of the page size as small scroll
  // increment and the page size as large scroll increment
  case aMsg.ScrollCode of
    SB_TOP:
      ScrollInfo.nPos := ScrollInfo.nMin;
    SB_BOTTOM:
      ScrollInfo.nPos := ScrollInfo.nMax;
    SB_LINEUP:
      Dec(ScrollInfo.nPos, ScrollInfo.nPage div 10);
    SB_LINEDOWN:
      Inc(ScrollInfo.nPos, ScrollInfo.nPage div 10);
    SB_PAGEUP:
      Dec(ScrollInfo.nPos, ScrollInfo.nPage);
    SB_PAGEDOWN:
      Inc(ScrollInfo.nPos, ScrollInfo.nPage);
    SB_THUMBTRACK, SB_THUMBPOSITION:
      ScrollInfo.nPos := aMsg.Pos;
    SB_ENDSCROLL:
      Exit;
  end;

  ScrollInfo.fMask := SIF_POS;
  if ScrollInfo.nPos < ScrollInfo.nMin then
    ScrollInfo.nPos := ScrollInfo.nMin;
  if ScrollInfo.nPos > ScrollInfo.nMax then
    ScrollInfo.nPos := ScrollInfo.nMax;

  SetScrollInfo(Handle, aBar, ScrollInfo, true);

  if aBar = SB_HORZ then
    TopLeft := Point(ScrollInfo.nPos, TopLeft.Y)
  else
    TopLeft := Point(TopLeft.X, ScrollInfo.nPos);
end;

procedure TSVGEditor.Init;
begin
  if assigned(FRoot) then
  begin
    FRoot.SVG.SetAttributeNS('xmlns:svge', ns_uri_xmlns, ns_svg_editor);

    ElementListInit(FRoot.SVG, FMaxID);

    CalcCanvasRect;

    DoElementAdd(nil, -1, FRoot.SVG);
  end;

  UpdatePage([]);

  Invalidate;
end;

procedure TSVGEditor.KeyDown(var Key: Word; Shift: TShiftState);
const
  ScrollKind: array[Boolean] of Cardinal = (WM_VSCROLL, WM_HSCROLL);
var
  Tool: TSVGEditorTool;

  procedure Scroll(aScrollCode, message: Cardinal);
  begin
    Perform(message, aScrollCode, 0);
  end;

begin
  inherited;

  for Tool in FToolList do
  begin
    Tool.KeyDown(Key, Shift);
  end;

  // Ignoring shift state for arrow keys here for simplicities sake
  case Key of
    VK_UP:
      Scroll(SB_LINEUP, WM_VSCROLL);
    VK_LEFT:
      Scroll(SB_LINEUP, WM_HSCROLL);
    VK_DOWN:
      Scroll(SB_LINEDOWN, WM_VSCROLL);
    VK_RIGHT:
      Scroll(SB_LINEDOWN, WM_HSCROLL);
    VK_NEXT:
      Scroll(SB_PAGEDOWN, ScrollKind[ssCtrl in Shift]);
    VK_PRIOR:
      Scroll(SB_PAGEUP, ScrollKind[ssCtrl in Shift]);
    VK_HOME:
      Scroll(SB_TOP, ScrollKind[ssCtrl in Shift]);
    VK_END:
      Scroll(SB_BOTTOM, ScrollKind[ssCtrl in Shift]);
  end;
  Key := 0;
end;

procedure TSVGEditor.LoadFromFile(const aFilename: string);
var
  Parser: TSVGSaxParser;
begin
  Clear;

  FRoot := TSVGRootVCL.Create;
  FRoot.OnRenderSVGObject := RenderSVGObject;

  Parser := TSVGSaxParser.Create(nil);
  try
    Parser.Parse(FFileName, FRoot);
  finally
    Parser.Free;
  end;

  Init;
end;

procedure TSVGEditor.LoadFromStream(aStream: TStream);
var
  Parser: TSVGSaxParser;
begin
  Clear;

  FRoot := TSVGRootVCL.Create;
  FRoot.OnRenderSVGObject := RenderSVGObject;

  Parser := TSVGSaxParser.Create(nil);
  try
     Parser.Parse(aStream, FRoot);
  finally
    Parser.Free;
  end;

  Init;
end;

procedure TSVGEditor.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  i: Integer;
  SVGObject: ISVGObject;
  TempList: TList<Integer>;
  ToolDraw: TSVGEditorToolDraw;
begin
  inherited;

  if (Button = mbLeft) and CanFocus and not Focused then
    SetFocus;

  if (FToolList.Count > 0) and (FToolList[0] is TSVGEditorToolDraw) then
  begin
    ToolDraw := FToolList[0] as TSVGEditorToolDraw;
    ToolDraw.MouseDown(Button, Shift, X - ToolDraw.Left, Y - ToolDraw.Top);
  end else

  if assigned(FRoot) then
  begin
    SVGObject := SVGObjectAt(FRoot, PointToSVG(Point(X, Y)), False);

    i := 0;
    while (i < FToolList.Count) do
    begin
      if FToolList[i].SVGObject as IInterface = SVGObject as IInterface then
        Break;

      Inc(i);
    end;

    if i = FToolList.Count then
    begin
      TempList := TList<Integer>.Create;
      try
        if assigned(SVGObject) then
          TempList.Add(GetElementID(SVGObject));

        SelectedElementList := TempList;
      finally
        TempList.Free;
      end;
    end;
  end;
end;

procedure TSVGEditor.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  ToolDraw: TSVGEditorToolDraw;
begin
  inherited;

  if (FToolList.Count > 0) and (FToolList[0] is TSVGEditorToolDraw) then
  begin
    ToolDraw := FToolList[0] as TSVGEditorToolDraw;
    ToolDraw.MouseMove(Shift, X - ToolDraw.Left, Y - ToolDraw.Top);
  end;
end;

procedure TSVGEditor.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
end;

procedure TSVGEditor.Paint;
begin
  inherited;

  if assigned(FBitmap) then
    Canvas.Draw(-FTopLeft.X, -FTopLeft.Y, FBitmap);
end;

procedure TSVGEditor.RenderSVGObject(aSVGObject: ISVGObject;
  aParentCache: ISVGObjectCache; const aStage: TSVGRenderStage;
  var IsRendered: Boolean);
var
  Tool: TSVGEditorTool;
begin
  if FRenderingSelection = rsBackground then
  begin
    IsRendered := True;

    for Tool in FToolList do
    begin
      if Tool.SVGObject as IInterface = aSVGObject as IInterface then
      begin
        IsRendered := False;
        Break;
      end;
    end;
  end;
end;

procedure TSVGEditor.Resize;
begin
  inherited;

  UpdateScrollbars;

  FTimerUpdatePage.Enabled := False;
  FTimerUpdatePage.Enabled := True;
end;

procedure TSVGEditor.SaveToFile(const aFilename: string);
begin
  if not assigned(FRoot) then
    Exit;

  FRoot.Doc.SaveToFile(aFileName);
end;

procedure TSVGEditor.SetAttribute(const aName, aValue: TSVGUnicodeString);
var
  ID: Integer;
  Element: ISVGElement;
  CmdGroup: TSVGEditorCmdGroup;
begin
  if FSelectedElementList.Count = 0 then
    Exit;

  CmdGroup := TSVGEditorCmdGroup.Create;
  try
    for ID in SelectedElementList do
    begin
      Element := FElementList[ID];

      CmdGroup.CmdList.Add(CmdSetAttributeCreate(Element, aName, aValue));
    end;

    CmdExec(CmdGroup, False);

    FCmdUndoStack.Push(CmdGroup);

    except on E:Exception do
      CmdGroup.Free;
  end;
end;

procedure TSVGEditor.SetCanvasRect(const Value: TRect);
begin
  if FCanvasRect <> Value then
  begin
    FCanvasRect := Value;

    UpdateScrollbars;
  end;
end;

procedure TSVGEditor.SetFilename(const Value: string);
begin
  FFilename := Value;

  LoadFromFile(FFilename);
end;

procedure TSVGEditor.SetFocusedTool(const Value: TSVGEditorTool);
begin
  if FFocusedTool <> Value then
  begin
    FFocusedTool := Value;
  end;
end;

procedure TSVGEditor.SetPadding(const Value: TPadding);
begin
  FPadding.Assign(Value);
end;

procedure TSVGEditor.SetScale(const Value: TSVGFloat);
begin
  ToolsHide;
  try
    if FScale <> Value then
    begin
      FScale := Value;

      CalcCanvasRect;
      UpdatePage([]);
    end;
  finally
    ToolsShow;
  end;
end;

procedure TSVGEditor.SetSelectedElementList(const Value: TList<Integer>);
var
  ID: Integer;
  CmdElementSelect: TSVGEditorCmdElementsSelect;
begin
  CmdElementSelect := TSVGEditorCmdElementsSelect.Create;
  try
    // Deselect nodes that are not in value

    for ID in FSelectedElementList do
    begin
      if Value.IndexOf(ID) = -1 then
        CmdElementSelect.Unselect.Add(ID);
    end;

    // Select nodes that are not in FSelectedElementList

    for ID in Value do
    begin
      if FSelectedElementList.IndexOf(ID) = -1 then
        CmdElementSelect.Select.Add(ID);
    end;

    CmdExec(CmdElementSelect, False);

    FCmdUndoStack.Push(CmdElementSelect);

    except on E:Exception do
      CmdElementSelect.Free;
  end;
end;

procedure TSVGEditor.SetTopLeft(const Value: TPoint);
var
  Dx, Dy: Integer;
  Tool: TSVGEditorTool;
begin
  if FTopLeft <> Value then
  begin
    for Tool in FToolList do
    begin
      Dx := Value.X - FTopLeft.X;
      Dy := Value.Y - FTopLeft.Y;

      Tool.Left := Tool.Left - Dx;
      Tool.Top := Tool.Top - Dy;

      Tool.UpdateToolParts;
    end;

    FTopLeft := Value;
    Repaint;
  end;
end;

procedure TSVGEditor.ToolAttributeChangeEvent(Sender: TObject; const aAttrName,
  aAttrValue: TSVGUnicodeString);
var
  Tool: TSVGEditorTool;
begin
  if Sender is TSVGEditorTool then
  begin
    Tool := Sender as TSVGEditorTool;

    DoSetAttribute(Tool.SVGObject, aAttrName, aAttrValue);
  end;
end;

function TSVGEditor.ToolCreate(aSVGObject: ISVGObject): TSVGEditorTool;
var
  ParentObject: ISVGObject;
  ParentCache: ISVGObjectCache;
begin
  if Supports(aSVGObject.ParentNode, ISVGObject, ParentObject)
  and (ParentObject.CacheList.Count > 0) then
    ParentCache := ParentObject.CacheList[0]
  else
    ParentCache := nil;

  Result := FCurTool.Create(Self, FRoot, aSVGObject, ParentCache);
  Result.Parent := Self;
  Result.OnAttributeChange := ToolAttributeChangeEvent;
end;

procedure TSVGEditor.ToolDestroy(var aTool: TSVGEditorTool);
begin
  if assigned(aTool) then
  begin
    if aTool.IsChanged then
      aTool.Apply;

    UpdatePage([rsCalcCache]);

    if FFocusedTool = aTool then
      FFocusedTool := nil;

    FreeAndNil(aTool);
  end;
end;

procedure TSVGEditor.ToolsClear;
var
  R: TSVGRect;
  Tool: TSVGEditorTool;
begin
  FUpdateRectList.Clear;

  while FToolList.Count > 0 do
  begin
    Tool := FToolList[0];

    // Collect the areas behind the old tools, so we can update the
    // image behind the tools later on

    R := TSVGRect.Create(Tool.BoundsRect);
    R.Offset(TopLeft.X, TopLeft.Y);
    R := TransformRect(R, FInvMatrix);

    FUpdateRectList.Add(R);

    FToolList.Delete(0);
    ToolDestroy(Tool);
  end;
end;

procedure TSVGEditor.ToolsCreate;
var
  ID: Integer;
  Element: ISVGElement;
  R: TSVGRect;
  Tool: TSVGEditorTool;
  SVGObject: ISVGObject;
begin
  for ID in FSelectedElementList do
  begin
    Element := FElementList[ID];

    if Supports(Element, ISVGObject, SVGObject) then
    begin
      Tool := ToolCreate(SVGObject);

      FToolList.Add(Tool);

      // Add the areas behind the new tools, so we can update the
      // image behind the tools

      R := TSVGRect.Create(Tool.BoundsRect);
      R.Offset(TopLeft.X, TopLeft.Y);
      R := TransformRect(R, FInvMatrix);

      FUpdateRectList.Add(R);
    end;
  end;

  // Render the image behind the tools

  for R in FUpdateRectList do
    FRoot.InvalidateRect(R);

  UpdatePage([]);

  FUpdateRectList.Clear;
end;

procedure TSVGEditor.ToolSelect(const aTool: TSVGToolClass);
var
  CmdToolSelect: TSVGEditorCmdToolSelect;
begin
  CmdToolSelect := CmdToolSelectCreate(aTool);
  try
    CmdExec(CmdToolSelect, False);

    FCmdUndoStack.Push(CmdToolSelect);

    except on E:Exception do
      CmdToolSelect.Free;
  end;
end;

procedure TSVGEditor.ToolsHide;
var
  Tool: TSVGEditorTool;
  R: TSVGRect;
begin
  FUpdateRectList.Clear;

  for Tool in FToolList do
  begin
    if Tool.IsChanged then
      Tool.Apply;

    // Collect the areas behind the tools, so we can update the
    // image behind the tools later on

    R := TSVGRect.Create(Tool.BoundsRect);
    R.Offset(TopLeft.X, TopLeft.Y);
    R := TransformRect(R, FInvMatrix);

    FUpdateRectList.Add(R);

    Tool.Visible := False;
  end;
end;

procedure TSVGEditor.ToolsShow;
var
  Tool: TSVGEditorTool;
  R: TSVGRect;
begin
  for Tool in FToolList do
  begin
    Tool.Init;
    Tool.Visible := True;

    // Add the areas behind the tools

    R := TSVGRect.Create(Tool.BoundsRect);
    R.Offset(TopLeft.X, TopLeft.Y);
    R := TransformRect(R, FInvMatrix);

    FUpdateRectList.Add(R);
  end;

  // Render the image behind the tools

  for R in FUpdateRectList do
    FRoot.InvalidateRect(R);

  UpdatePage([]);

  FUpdateRectList.Clear;
end;

function TSVGEditor.PointToSVG(const aPoint: TPoint): TSVGPoint;
begin
  Result := TransformPoint(
    aPoint.X + FTopLeft.X,
    aPoint.Y + FTopLeft.Y,
    FInvMatrix);
end;

procedure TSVGEditor.UpdatePage(const aRenderMode: TSVGRenderMode);
var
  W, H: Integer;
  R: TSVGRect;
  SaveMatrix: TSVGMatrix;
begin
  W := FCanvasRect.Width;
  H := FCanvasRect.Height;

  if assigned(FBitmap) then
  begin
    if (FBitmap.Width <> W) or (FBitmap.Height <> H) then
    begin
      FBitmap.SetSize(W, H);
      FPageRC := TSVGRenderContextManager.CreateRenderContextBitmap(FBitmap);
    end;
  end else begin
    FBitmap := TSVGRenderContextManager.CreateCompatibleBitmap(W, H);
    FPageRC := TSVGRenderContextManager.CreateRenderContextBitmap(FBitmap);
  end;

  FPageRC.BeginScene;
  try
    if not(rsCalcCache in aRenderMode) then
    begin
      if (not assigned(FRoot)) or (FRoot.UpdateRectList.Count = 0) then
        FPageRC.Clear(FBackgroundColor);

      FPageRC.ApplyStroke(TSVGSolidBrush.Create(SVGColorGray), 1);
      FPageRC.DrawRect(
        SVGRect(
          FPadding.Left,
          FPadding.Top,
          FCanvasRect.Right - FPadding.Right,
          FCanvasRect.Bottom - FPadding.Bottom));
    end;

    if assigned(FRoot) then
    begin
      SaveMatrix := FPageRC.Matrix;
      try
        FPageRC.Matrix := FMatrix;

        FRenderingSelection := rsFull;

        if not(rsCalcCache in aRenderMode) then
        begin
          if FRoot.UpdateRectList.Count > 0 then
          begin
            FRenderingSelection := rsBackground;

            for R in FRoot.UpdateRectList do
            begin
              FPageRC.ApplyFill(TSVGSolidBrush.Create(FBackgroundColor));
              FPageRC.FillRect(R);
            end;
          end;
        end;

        FRoot.RenderMode := aRenderMode;

        SVGRenderToRenderContext(
          FRoot,
          FPageRC,
          FCanvasRect.Width - FPadding.Left - FPadding.Right,
          FCanvasRect.Height - FPadding.Top - FPadding.Bottom,
          //[sroClippath, sroFilters],
          [],
          False,
          arXMidYMid,
          arMeet);

      finally
        FPageRC.Matrix := SaveMatrix;
      end;
    end;

  finally
    FPageRC.EndScene;
  end;

  if not(rsCalcCache in aRenderMode) then
    Repaint;
end;

procedure TSVGEditor.UpdateScrollbars;
var
  ScrollInfo: TScrollInfo;
begin
  ScrollInfo.cbSize := Sizeof(TScrollInfo);
  ScrollInfo.fMask := SIF_PAGE or SIF_POS or SIF_RANGE;
  ScrollInfo.nMin := 0;
  ScrollInfo.nMax := FCanvasRect.Height;

  ScrollInfo.nPage := ControlSize(SB_VERT, False, False);
  ScrollInfo.nPos := 0;
  SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);

  ScrollInfo.nMax := FCanvasRect.Width;
  ScrollInfo.nPage := ControlSize(SB_HORZ, False, False);

  SetScrollInfo(Handle, SB_HORZ, ScrollInfo, True);
end;

procedure TSVGEditor.WMGetDlgCode(var msg: TWMGetDlgCode);
begin
  msg.result := DLGC_WANTARROWS;
end;

procedure TSVGEditor.WMHScroll(var msg: TWMSCROLL);
begin
  HandleScrollbar(msg, SB_HORZ);
end;

procedure TSVGEditor.WMVScroll(var msg: TWMSCROLL);
begin
  HandleScrollbar(msg, SB_VERT);
end;

end.
