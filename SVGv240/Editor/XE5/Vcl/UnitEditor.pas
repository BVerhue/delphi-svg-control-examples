unit UnitEditor;

// ------------------------------------------------------------------------------
//
//                          SVG Control Package 2.0
//                       Copyright (c) 2015 Bruno Verhue
//
// ------------------------------------------------------------------------------

// This editor is not meant to be a finished product, although it might some day
// be one. It's use is primarily to show some practical solutions for
// programming with the SVG library

// The SVG Editor needs at least version v2.40 update 9 of the SVG library

interface
uses
  Winapi.Windows,
  Winapi.Messages,
  System.Types,
  System.Classes,
  System.Actions,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ToolWin,
  Vcl.ActnMan,
  Vcl.ActnCtrls,
  Vcl.ActnList,
  Vcl.ExtCtrls,
  Vcl.PlatformDefaultStyleActnCtrls,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ActnMenus,
  Vcl.Grids,
  Vcl.ValEdit,
  Vcl.ImgList,
  BVE.SVG2Intf,
  BVE.SVG2Types,
  BVE.SVGEditorVCL,
  BVE.SVG2ImageList.VCL,
  BVE.SVGEditorFormVCL;

type
  TfrmEditor = class(TSVGEditorForm)
    ActionManager1: TActionManager;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Splitter1: TSplitter;
    TreeView1: TTreeView;
    SaveDialog1: TSaveDialog;
    ValueListEditor1: TValueListEditor;
    Splitter2: TSplitter;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ActionMainMenuBar1: TActionMainMenuBar;
    aOpen: TAction;
    aZoom1to1: TAction;
    aZoom1to2: TAction;
    aSaveAs: TAction;
    aAddRect: TAction;
    aAddSVG: TAction;
    aUndo: TAction;
    aRedo: TAction;
    aCut: TAction;
    aCopy: TAction;
    aPaste: TAction;
    aDelete: TAction;
    aToolShape: TAction;
    aToolTransform: TAction;
    aNew: TAction;
    aPrint: TAction;
    aExit: TAction;
    aAddCircle: TAction;
    aAddEllipse: TAction;
    aAddLine: TAction;
    aAddPolyline: TAction;
    aAddPolygon: TAction;
    aAddPath: TAction;
    aAddText: TAction;
    aAddImage: TAction;
    aAddGroup: TAction;
    SVG2ImageList1: TSVG2ImageList;
    ilNormal: TSVG2LinkedImageList;
    ilDisabled: TSVG2LinkedImageList;
    procedure FormCreate(Sender: TObject);
  end;

var
  frmEditor: TfrmEditor;

implementation

{$R *.dfm}

uses UnitPrintPreview;

procedure TfrmEditor.FormCreate(Sender: TObject);
begin
  ActionOpen := aOpen;
  ActionSaveAs := aSaveAs;
  ActionUndo := aUndo;
  ActionRedo := aRedo;
  ActionCut := aCut;
  ActionCopy := aCopy;
  ActionPaste := aPaste;
  ActionDelete := aDelete;
  ActionZoom1to1 := aZoom1to1;
  ActionZoom1to2 := aZoom1to2;
  ActionToolShape := aToolShape;
  ActionToolTransform := aToolTransform;
  ActionNew := aNew;
  ActionPrint := aPrint;
  ActionExit := aExit;
  ActionAddRect := aAddRect;
  ActionAddSVG := aAddSVG;
  ActionAddCircle := aAddCircle;
  ActionAddEllipse := aAddEllipse;
  ActionAddLine := aAddLine;
  ActionAddPolyline := aAddPolyline;
  ActionAddPolygon := aAddPolygon;
  ActionAddPath := aAddPath;
  ActionAddText := aAddText;
  ActionAddImage := aAddImage;
  ActionAddGroup := aAddGroup;

  TreeviewXML  := Treeview1;
  ValueListEditorAttribute := ValueListEditor1;
end;

end.
