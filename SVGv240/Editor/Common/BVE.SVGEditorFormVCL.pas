unit BVE.SVGEditorFormVCL;

// ------------------------------------------------------------------------------
//
//                          SVG Control Package 2.0
//                       Copyright (c) 2015 Bruno Verhue
//
// ------------------------------------------------------------------------------

// The SVG Editor need at least version v2.40 update 9 of the SVG library

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Winapi.Windows,
  Winapi.Messages,
  Winapi.ShellApi,
  System.SysUtils,
  System.Variants,
  System.Types,
  System.Classes,
  System.Generics.Collections,
  System.Actions,
  Vcl.Graphics,
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
  XML.XMLIntf,
{$ELSE}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
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
  BVE.SVG2Intf,
  BVE.SVG2Types,
  BVE.SVGEditorVCL,
  BVE.SVG2ImageList.VCL;

type
  TSVGEditorForm = class(TForm)
  private
    FActionAddPolygon: TAction;
    FActionNew: TAction;
    FActionSaveAs: TAction;
    FActionAddRect: TAction;
    FActionOpen: TAction;
    FActionToolTransform: TAction;
    FActionZoom1to2: TAction;
    FActionAddImage: TAction;
    FActionAddPolyline: TAction;
    FActionPrint: TAction;
    FActionZoom1to1: TAction;
    FActionUndo: TAction;
    FActionToolShape: TAction;
    FActionRedo: TAction;
    FActionCopy: TAction;
    FActionDelete: TAction;
    FActionAddLine: TAction;
    FActionAddText: TAction;
    FActionAddEllipse: TAction;
    FActionExit: TAction;
    FActionAddCircle: TAction;
    FActionAddGroup: TAction;
    FActionAddSVG: TAction;
    FActionCut: TAction;
    FActionAddPath: TAction;
    FActionPaste: TAction;

    FEditor: TSVGEditor;

    FTimerUpdatePage: TTimer;

    FOpenDialog: TOpenDialog;
    FSaveDialog: TSaveDialog;
    FTreeviewXML: TTreeView;
    FValueListEditorAttribute: TValueListEditor;

    {$IFnDEF FPC}
    procedure WMDROPFILES(var Msg: TWMDropFiles); message WM_DROPFILES;
    {$ELSE}
    procedure OnDropFiles(Sender: TObject; const FileNames: array of String);
    {$ENDIF}
  protected
    procedure SetActionAddCircle(const Value: TAction);
    procedure SetActionAddEllipse(const Value: TAction);
    procedure SetActionAddGroup(const Value: TAction);
    procedure SetActionAddImage(const Value: TAction);
    procedure SetActionAddLine(const Value: TAction);
    procedure SetActionAddPath(const Value: TAction);
    procedure SetActionAddPolygon(const Value: TAction);
    procedure SetActionAddPolyline(const Value: TAction);
    procedure SetActionAddRect(const Value: TAction);
    procedure SetActionAddSVG(const Value: TAction);
    procedure SetActionAddText(const Value: TAction);
    procedure SetActionCopy(const Value: TAction);
    procedure SetActionCut(const Value: TAction);
    procedure SetActionDelete(const Value: TAction);
    procedure SetActionExit(const Value: TAction);
    procedure SetActionNew(const Value: TAction);
    procedure SetActionOpen(const Value: TAction);
    procedure SetActionPaste(const Value: TAction);
    procedure SetActionPrint(const Value: TAction);
    procedure SetActionRedo(const Value: TAction);
    procedure SetActionSaveAs(const Value: TAction);
    procedure SetActionToolShape(const Value: TAction);
    procedure SetActionToolTransform(const Value: TAction);
    procedure SetActionUndo(const Value: TAction);
    procedure SetActionZoom1to1(const Value: TAction);
    procedure SetActionZoom1to2(const Value: TAction);
    procedure SetOpenDialog(const Value: TOpenDialog);
    procedure SetTreeviewXML(const Value: TTreeView);
    procedure SetValueListEditorAttribute(const Value: TValueListEditor);
    procedure SetSaveDialog(const Value: TSaveDialog);

    procedure DoCreate; override;
    procedure DoDestroy; override;
    procedure DoShow; override;

    procedure TreeViewXMLChange(Sender: TObject; Node: TTreeNode);

    procedure TimerUpdatePageTimer(Sender: TObject);

    procedure ValueListEditorAttributeEditButtonClick(Sender: TObject);
    procedure ValueListEditorAttributeValidate(Sender: TObject; ACol, ARow: Integer;
      const KeyName, KeyValue: string);
    procedure ValueListEditorAttributeUpdate;

    procedure ActionOpenExecute(Sender: TObject);
    procedure ActionZoom1to1Execute(Sender: TObject);
    procedure ActionZoom1to2Execute(Sender: TObject);
    procedure ActionSaveAsExecute(Sender: TObject);
    procedure ActionAddRectExecute(Sender: TObject);
    procedure ActionAddSVGExecute(Sender: TObject);
    procedure ActionCopyExecute(Sender: TObject);
    procedure ActionCutExecute(Sender: TObject);
    procedure ActionDeleteExecute(Sender: TObject);
    procedure ActionPasteExecute(Sender: TObject);
    procedure ActionUndoExecute(Sender: TObject);
    procedure ActionRedoExecute(Sender: TObject);
    procedure ActionToolTransformExecute(Sender: TObject);
    procedure ActionToolShapeExecute(Sender: TObject);
    procedure ActionNewExecute(Sender: TObject);
    procedure ActionPrintExecute(Sender: TObject);
    procedure ActionExitExecute(Sender: TObject);
    procedure ActionAddCircleExecute(Sender: TObject);
    procedure ActionAddEllipseExecute(Sender: TObject);
    procedure ActionAddLineExecute(Sender: TObject);
    procedure ActionAddPolylineExecute(Sender: TObject);
    procedure ActionAddPolygonExecute(Sender: TObject);
    procedure ActionAddPathExecute(Sender: TObject);
    procedure ActionAddTextExecute(Sender: TObject);
    procedure ActionAddImageExecute(Sender: TObject);
    procedure ActionAddGroupExecute(Sender: TObject);

    function CheckSelectionIsGroup(const aExceptions: Boolean): Boolean;

    procedure ElementAdd(Sender: TObject; const aParent: ISVGElement;
      const aIndex: Integer; const aElement: ISVGElement);
    procedure ElementRemove(Sender: TObject; const aElement: ISVGElement);
    procedure ElementSelect(Sender: TObject);
    procedure SetAttribute(Sender: TObject; const aElement: ISVGElement;
      const aName, aValue: TSVGUnicodeString);
    procedure ToolSelect(Sender: TObject; const aTool: TSVGToolClass);

    procedure TreeViewXMLNodeAdd(const aParent: IXMLNode; const aIndex: Integer;
      const aElement: ISVGElement);
    function TreeViewXMLNodeSelect: Boolean;
    procedure TreeViewXMLNodeRemove(const aElement: ISVGElement);

    procedure EnableActions;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DocumentNew;
    procedure DocumentOpen(const aFilename: string);

    property ActionOpen: TAction read FActionOpen write SetActionOpen;
    property ActionSaveAs: TAction read FActionSaveAs write SetActionSaveAs;
    property ActionUndo: TAction read FActionUndo write SetActionUndo;
    property ActionRedo: TAction read FActionRedo write SetActionRedo;
    property ActionCut: TAction read FActionCut write SetActionCut;
    property ActionCopy: TAction read FActionCopy write SetActionCopy;
    property ActionPaste: TAction read FActionPaste write SetActionPaste;
    property ActionDelete: TAction read FActionDelete write SetActionDelete;
    property ActionZoom1to1: TAction read FActionZoom1to1 write SetActionZoom1to1;
    property ActionZoom1to2: TAction read FActionZoom1to2 write SetActionZoom1to2;
    property ActionToolShape: TAction read FActionToolShape write SetActionToolShape;
    property ActionToolTransform: TAction read FActionToolTransform write SetActionToolTransform;
    property ActionNew: TAction read FActionNew write SetActionNew;
    property ActionPrint: TAction read FActionPrint write SetActionPrint;
    property ActionExit: TAction read FActionExit write SetActionExit;
    property ActionAddRect: TAction read FActionAddRect write SetActionAddRect;
    property ActionAddSVG: TAction read FActionAddSVG write SetActionAddSVG;
    property ActionAddCircle: TAction read FActionAddCircle write SetActionAddCircle;
    property ActionAddEllipse: TAction read FActionAddEllipse write SetActionAddEllipse;
    property ActionAddLine: TAction read FActionAddLine write SetActionAddLine;
    property ActionAddPolyline: TAction read FActionAddPolyline write SetActionAddPolyline;
    property ActionAddPolygon: TAction read FActionAddPolygon write SetActionAddPolygon;
    property ActionAddPath: TAction read FActionAddPath write SetActionAddPath;
    property ActionAddText: TAction read FActionAddText write SetActionAddText;
    property ActionAddImage: TAction read FActionAddImage write SetActionAddImage;
    property ActionAddGroup: TAction read FActionAddGroup write SetActionAddGroup;

    property TreeviewXML: TTreeView read FTreeviewXML write SetTreeviewXML;
    property ValueListEditorAttribute: TValueListEditor read FValueListEditorAttribute
      write SetValueListEditorAttribute;
  end;


implementation
uses
  XML.XMLDom,
  BVE.SVG2Elements;

{ TSVGEditorForm }

procedure TSVGEditorForm.ActionAddCircleExecute(Sender: TObject);
begin
  CheckSelectionIsGroup(True);

  FEditor.ElementAdd('<circle cx="100" cy="100" r="50" />');
end;

procedure TSVGEditorForm.ActionAddEllipseExecute(Sender: TObject);
begin
  CheckSelectionIsGroup(True);

  FEditor.ElementAdd('<ellipse cx="100" cy="80" rx="80" ry="50" />');
end;

procedure TSVGEditorForm.ActionAddGroupExecute(Sender: TObject);
begin
  CheckSelectionIsGroup(True);

  FEditor.ElementAdd('<g />');
end;

procedure TSVGEditorForm.ActionAddImageExecute(Sender: TObject);
begin
  // TODO
end;

procedure TSVGEditorForm.ActionAddLineExecute(Sender: TObject);
begin
  // TODO
end;

procedure TSVGEditorForm.ActionAddPathExecute(Sender: TObject);
begin
  // TODO
end;

procedure TSVGEditorForm.ActionAddPolygonExecute(Sender: TObject);
begin
  // TODO
end;

procedure TSVGEditorForm.ActionAddPolylineExecute(Sender: TObject);
begin
  // TODO
end;

procedure TSVGEditorForm.ActionAddRectExecute(Sender: TObject);
begin
  CheckSelectionIsGroup(True);

  FEditor.ElementAdd('<rect x="100" y="100" width="100" height="100" />');
end;

procedure TSVGEditorForm.ActionAddSVGExecute(Sender: TObject);
var
  sl: TStringList;
begin
  if FEditor.SelectedElementList.Count <> 1 then
    Exit;

  if FOpenDialog.Execute then
  begin
    sl := TStringList.Create;
    try
      sl.LoadFromFile(FOpenDialog.FileName);

      FEditor.ElementAdd(sl.Text);
    finally
      sl.Free;
    end;
  end;
end;

procedure TSVGEditorForm.ActionAddTextExecute(Sender: TObject);
begin
  // TODO
end;

procedure TSVGEditorForm.ActionCopyExecute(Sender: TObject);
begin
  TreeViewXML.OnChange := nil;
  try
    FEditor.ElementsCopy;
  finally
    TreeViewXMl.OnChange := TreeViewXMLChange;
  end;
end;

procedure TSVGEditorForm.ActionCutExecute(Sender: TObject);
begin
  TreeViewXML.OnChange := nil;
  try
    FEditor.ElementsCut;
  finally
    TreeViewXML.OnChange := TreeViewXMLChange;
  end;
end;

procedure TSVGEditorForm.ActionDeleteExecute(Sender: TObject);
begin
  TreeViewXML.OnChange := nil;
  try
    FEditor.ElementsDelete;
  finally
    TreeViewXML.OnChange := TreeViewXMLChange;
  end;
end;

procedure TSVGEditorForm.ActionExitExecute(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TSVGEditorForm.ActionNewExecute(Sender: TObject);
begin
  DocumentNew;
end;

procedure TSVGEditorForm.ActionOpenExecute(Sender: TObject);
begin
  if FOpenDialog.Execute then
  begin
    DocumentOpen(FOpenDialog.FileName);
  end;
end;

procedure TSVGEditorForm.ActionPasteExecute(Sender: TObject);
begin
  TreeViewXML.OnChange := nil;
  try
    FEditor.ElementsPaste;
  finally
    TreeViewXML.OnChange := TreeViewXMLChange;
  end;
end;

procedure TSVGEditorForm.ActionPrintExecute(Sender: TObject);
begin
  // TODO
end;

procedure TSVGEditorForm.ActionRedoExecute(Sender: TObject);
begin
  TreeViewXML.OnChange := nil;
  try
    FEditor.CmdRedo;
  finally
    TreeViewXML.OnChange := TreeViewXMLChange;
  end;
end;

procedure TSVGEditorForm.ActionSaveAsExecute(Sender: TObject);
begin
  if not assigned(FEditor.Root) then
    Exit;

  if FSaveDialog.Execute then
  begin
    FEditor.SaveToFile(FSaveDialog.Filename);
  end;
end;

procedure TSVGEditorForm.ActionToolShapeExecute(Sender: TObject);
begin
  FEditor.ToolSelect(TSVGEditorToolShape);
end;

procedure TSVGEditorForm.ActionToolTransformExecute(Sender: TObject);
begin
  FEditor.ToolSelect(TSVGEditorToolTransform);
end;

procedure TSVGEditorForm.ActionUndoExecute(Sender: TObject);
begin
  TreeViewXML.OnChange := nil;
  try
    FEditor.CmdUndo;
  finally
    TreeViewXML.OnChange := TreeViewXMLChange;
  end;
end;

procedure TSVGEditorForm.ActionZoom1to1Execute(Sender: TObject);
begin
  FEditor.Scale := 1;
end;

procedure TSVGEditorForm.ActionZoom1to2Execute(Sender: TObject);
begin
  FEditor.Scale := 2;
end;

function TSVGEditorForm.CheckSelectionIsGroup(
  const aExceptions: Boolean): Boolean;
var
  Element: ISVGElement;
begin
  Result := True;

  if FEditor.SelectedElementList.Count <> 1 then
  begin
    Result := False;
    if aExceptions then
      raise Exception.Create('Must select one parent element.')
    else
      Exit;
  end;

  Element := FEditor.Element[FEditor.SelectedElementList[0]];

  if not Supports(Element, ISVGGroup) then
  begin
    Result := False;
    if aExceptions then
      raise Exception.Create('Select object is not a container.')
    else
      Exit;
  end;
end;

constructor TSVGEditorForm.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TSVGEditorForm.Destroy;
begin
  inherited;
end;

procedure TSVGEditorForm.DoCreate;
begin
  inherited;

  FEditor := TSVGEditor.Create(Self);
  FEditor.Parent := Self;
  FEditor.Align := alClient;
  FEditor.OnElementAdd := ElementAdd;
  FEditor.OnElementRemove := ElementRemove;
  FEditor.OnElementSelect := ElementSelect;
  FEditor.OnSetAttribute := SetAttribute;
  FEditor.OnToolSelect := ToolSelect;

  FTimerUpdatePage := TTimer.Create(Self);
  FTimerUpdatePage.Enabled := False;
  FTimerUpdatePage.Interval := 250;
  FTimerUpdatePage.OnTimer := TimerUpdatePageTimer;

  FOpenDialog := TOpenDialog.Create(Self);
  FSaveDialog := TSaveDialog.Create(Self);

  DocumentNew;

  {$IFnDEF FPC}
  DragAcceptFiles(Handle, True);
  {$ELSE}
  Application.AddOnDropFilesHandler(OnDropFiles);
  {$ENDIF}
end;

procedure TSVGEditorForm.DocumentNew;
var
  StringStream: TStringStream;
begin
  StringStream := TStringStream.Create;
  try
    StringStream.WriteString(
      '<?xml version="1.0" standalone="no"?>'
      + '<svg width="480" height="360" xmlns="' + ns_uri_svg + '" xmlns:xlink="' + ns_uri_xlink + '" version="1.1" />');

    StringStream.Position := 0;
    FEditor.LoadFromStream(StringStream);
  finally
    StringStream.Free;
  end;
end;

procedure TSVGEditorForm.DocumentOpen(const aFilename: string);
begin
  FEditor.Filename := aFilename;
end;

procedure TSVGEditorForm.DoDestroy;
begin
  {$IFDEF FPC}
  Application.RemoveOnDropFilesHandler(OnDropFiles);
  {$ENDIF}

  inherited;
end;

procedure TSVGEditorForm.DoShow;
begin
  inherited;
end;

procedure TSVGEditorForm.ElementAdd(Sender: TObject; const aParent: ISVGElement;
  const aIndex: Integer; const aElement: ISVGElement);
begin
  TreeViewXMLNodeAdd(aParent, aIndex, aElement);
  EnableActions;
end;

procedure TSVGEditorForm.ElementRemove(Sender: TObject;
  const aElement: ISVGElement);
begin
  TreeViewXMLNodeRemove(aElement);
  EnableActions;
end;

procedure TSVGEditorForm.ElementSelect(Sender: TObject);
begin
  if TreeViewXMLNodeSelect then
    ValueListEditorAttributeUpdate;

  EnableActions;
end;

procedure TSVGEditorForm.EnableActions;
var
  CanAddElement: Boolean;
  CanDeleteElement: Boolean;
  OuterSVGSelected: Boolean;
  SVGDocLoaded: Boolean;
  ElementsSelected: Boolean;
begin
  ElementsSelected := FEditor.SelectedElementList.Count > 0;
  OuterSVGSelected := FEditor.SelectedElementList.IndexOf(0) <> -1;

  CanAddElement := CheckSelectionIsGroup(False);
  CanDeleteElement := ElementsSelected and (not OuterSVGSelected);

  SVGDocLoaded := assigned(FEditor.Root);

  ActionOpen.Enabled := True;
  ActionSaveAs.Enabled := SVGDocLoaded;
  ActionNew.Enabled := True;
  ActionPrint.Enabled := SVGDocLoaded;
  ActionExit.Enabled := True;

  ActionCopy.Enabled := CanDeleteElement;
  ActionCut.Enabled := CanDeleteElement;
  ActionDelete.Enabled := CanDeleteElement;
  ActionPaste.Enabled := CanAddElement;
  ActionUndo.Enabled := FEditor.CmdUndoCount > 0;
  ActionRedo.Enabled := FEditor.CmdRedoCount > 0;

  ActionAddRect.Enabled := CanAddElement;
  ActionAddSVG.Enabled := CanAddElement;
  ActionAddCircle.Enabled := CanAddElement;
  ActionAddEllipse.Enabled := CanAddElement;
  ActionAddLine.Enabled := CanAddElement;
  ActionAddPolyline.Enabled := CanAddElement;
  ActionAddPolygon.Enabled := CanAddElement;
  ActionAddPath.Enabled := CanAddElement;
  ActionAddText.Enabled := CanAddElement;
  ActionAddImage.Enabled := CanAddElement;
  ActionAddGroup.Enabled := CanAddElement;

  ActionZoom1to1.Enabled := SVGDocLoaded;
  ActionZoom1to2.Enabled := SVGDocLoaded;

  ActionToolTransform.Enabled := ElementsSelected;
  ActionToolShape.Enabled := ElementsSelected;
end;

procedure TSVGEditorForm.SetActionAddCircle(const Value: TAction);
begin
  FActionAddCircle := Value;
  if assigned(FActionAddCircle) then
    FActionAddCircle.OnExecute := ActionAddCircleExecute;
end;

procedure TSVGEditorForm.SetActionAddEllipse(const Value: TAction);
begin
  FActionAddEllipse := Value;
  if assigned(FActionAddEllipse) then
    FActionAddEllipse.OnExecute := ActionAddEllipseExecute;

end;

procedure TSVGEditorForm.SetActionAddGroup(const Value: TAction);
begin
  FActionAddGroup := Value;
  if assigned(FActionAddGroup) then
    FActionAddGroup.OnExecute := ActionAddGroupExecute;
end;

procedure TSVGEditorForm.SetActionAddImage(const Value: TAction);
begin
  FActionAddImage := Value;
  if assigned(FActionAddImage) then
    FActionAddImage.OnExecute := ActionAddImageExecute;
end;

procedure TSVGEditorForm.SetActionAddLine(const Value: TAction);
begin
  FActionAddLine := Value;
  if assigned(FActionAddLine) then
    FActionAddLine.OnExecute := ActionAddLineExecute;
end;

procedure TSVGEditorForm.SetActionAddPath(const Value: TAction);
begin
  FActionAddPath := Value;
  if assigned(FActionAddPath) then
    FActionAddPath.OnExecute := ActionAddPathExecute;

end;

procedure TSVGEditorForm.SetActionAddPolygon(const Value: TAction);
begin
  FActionAddPolygon := Value;
  if assigned(FActionAddPolygon) then
    FActionAddPolygon.OnExecute := ActionAddPolygonExecute;
end;

procedure TSVGEditorForm.SetActionAddPolyline(const Value: TAction);
begin
  FActionAddPolyline := Value;
  if assigned(FActionAddPolyline) then
    FActionAddPolyline.OnExecute := ActionAddPolylineExecute;
end;

procedure TSVGEditorForm.SetActionAddRect(const Value: TAction);
begin
  FActionAddRect := Value;
  if assigned(FActionAddRect) then
    FActionAddRect.OnExecute := ActionAddRectExecute;
end;

procedure TSVGEditorForm.SetActionAddSVG(const Value: TAction);
begin
  FActionAddSVG := Value;
  if assigned(FActionAddSVG) then
    FActionAddSVG.OnExecute := ActionAddSVGExecute;
end;

procedure TSVGEditorForm.SetActionAddText(const Value: TAction);
begin
  FActionAddText := Value;
  if assigned(FActionAddText) then
    FActionAddText.OnExecute := ActionAddTextExecute;
end;

procedure TSVGEditorForm.SetActionCopy(const Value: TAction);
begin
  FActionCopy := Value;
  if assigned(FActionCopy) then
    FActionCopy.OnExecute := ActionCopyExecute;
end;

procedure TSVGEditorForm.SetActionCut(const Value: TAction);
begin
  FActionCut := Value;
  if assigned(FActionCut) then
    FActionCut.OnExecute := ActionCutExecute;
end;

procedure TSVGEditorForm.SetActionDelete(const Value: TAction);
begin
  FActionDelete := Value;
  if assigned(FActionDelete) then
    FActionDelete.OnExecute := ActionDeleteExecute;
end;

procedure TSVGEditorForm.SetActionExit(const Value: TAction);
begin
  FActionExit := Value;
  if assigned(FActionExit) then
    FActionExit.OnExecute := ActionExitExecute;
end;

procedure TSVGEditorForm.SetActionNew(const Value: TAction);
begin
  FActionNew := Value;
  if assigned(FActionNew) then
    FActionNew.OnExecute := ActionNewExecute;
end;

procedure TSVGEditorForm.SetActionOpen(const Value: TAction);
begin
  FActionOpen := Value;
  if assigned(FActionOpen) then
    FActionOpen.OnExecute := ActionOpenExecute;
end;

procedure TSVGEditorForm.SetActionPaste(const Value: TAction);
begin
  FActionPaste := Value;
  if assigned(FActionPaste) then
    FActionPaste.OnExecute := ActionPasteExecute;
end;

procedure TSVGEditorForm.SetActionPrint(const Value: TAction);
begin
  FActionPrint := Value;
  if assigned(FActionPrint) then
    FActionPrint.OnExecute := ActionPrintExecute;
end;

procedure TSVGEditorForm.SetActionRedo(const Value: TAction);
begin
  FActionRedo := Value;
  if assigned(FActionRedo) then
    FActionRedo.OnExecute := ActionRedoExecute;
end;

procedure TSVGEditorForm.SetActionSaveAs(const Value: TAction);
begin
  FActionSaveAs := Value;
  if assigned(FActionSaveAs) then
    FActionSaveAs.OnExecute := ActionSaveAsExecute;
end;

procedure TSVGEditorForm.SetActionToolShape(const Value: TAction);
begin
  FActionToolShape := Value;
  if assigned(FActionToolShape) then
    FActionToolShape.OnExecute := ActionToolShapeExecute;
end;

procedure TSVGEditorForm.SetActionToolTransform(const Value: TAction);
begin
  FActionToolTransform := Value;
  if assigned(FActionToolTransform) then
    FActionToolTransform.OnExecute := ActionToolTransformExecute;
end;

procedure TSVGEditorForm.SetActionUndo(const Value: TAction);
begin
  FActionUndo := Value;
  if assigned(FActionUndo) then
    FActionUndo.OnExecute := ActionUndoExecute;
end;

procedure TSVGEditorForm.SetActionZoom1to1(const Value: TAction);
begin
  FActionZoom1to1 := Value;
  if assigned(FActionZoom1to1) then
    FActionZoom1to1.OnExecute := ActionZoom1to1Execute;
end;

procedure TSVGEditorForm.SetActionZoom1to2(const Value: TAction);
begin
  FActionZoom1to2 := Value;
  if assigned(FActionZoom1to2) then
    FActionZoom1to2.OnExecute := ActionZoom1to2Execute;
end;

procedure TSVGEditorForm.SetAttribute(Sender: TObject;
  const aElement: ISVGElement; const aName, aValue: TSVGUnicodeString);
begin
  ValueListEditorAttribute.OnValidate := nil;
  try
    ValueListEditorAttribute.Values[aName] := aValue;
  finally
    ValueListEditorAttribute.OnValidate := ValueListEditorAttributeValidate;
  end;

  EnableActions;
end;

procedure TSVGEditorForm.SetOpenDialog(const Value: TOpenDialog);
begin
  FOpenDialog := Value;
end;

procedure TSVGEditorForm.SetSaveDialog(const Value: TSaveDialog);
begin
  FSaveDialog := Value;
end;

procedure TSVGEditorForm.SetTreeviewXML(const Value: TTreeView);
begin
  FTreeviewXML := Value;
  if assigned(FTreeviewXML) then
    FTreeviewXML.OnChange := TreeviewXMLChange;
end;

procedure TSVGEditorForm.SetValueListEditorAttribute(
  const Value: TValueListEditor);
begin
  FValueListEditorAttribute := Value;
  if assigned(FValueListEditorAttribute) then
  begin
    FValueListEditorAttribute.OnValidate := ValueListEditorAttributeValidate;
    FValueListEditorAttribute.OnEditButtonClick := ValueListEditorAttributeEditButtonClick;
  end;
end;

procedure TSVGEditorForm.TimerUpdatePageTimer(Sender: TObject);
begin
  FEditor.UpdatePage([]);
end;

procedure TSVGEditorForm.ToolSelect(Sender: TObject;
  const aTool: TSVGToolClass);
begin
  if aTool = TSVGEditorToolTransform then
    ActionToolTransform.Checked := True
  else

  if aTool = TSVGEditorToolShape then
    ActionToolShape.Checked := True;

  EnableActions;
end;

procedure TSVGEditorForm.TreeViewXMLChange(Sender: TObject; Node: TTreeNode);
var
  i: Integer;
  ID: Integer;
  TempList: TList<Integer>;
  Handler: TTVChangedEvent;
begin
  // If the selection of the nodes in the Treeview changes, than select
  // the elements in the editor also

  TempList := TList<Integer>.Create;
  try
    i := 0;
    while i < Integer(TreeViewXML.SelectionCount) do
    begin
      ID := Integer(TreeViewXML.Selections[i].Data);
      TempList.Add(ID);

      Inc(i);
    end;

    Handler := TreeViewXML.OnChange;
    try
      TreeViewXML.OnChange := nil;

      FEditor.SelectedElementList := TempList;
    finally
      TreeViewXML.OnChange := Handler;
    end;

    ValueListEditorAttributeUpdate;

  finally
    TempList.Free;
  end;
end;

procedure TSVGEditorForm.TreeViewXMLNodeAdd(const aParent: IXMLNode;
  const aIndex: Integer; const aElement: ISVGElement);
var
  Handler: TTVChangedEvent;
  Element: ISVGElement;

  procedure DoNode(aParentTreeNode: TTreeNode; const aIndex: Integer;
    aNode: IXMLNode);
  var
    i: Integer;
    TreeNode: TTreeNode;
    Child: TTreeNode;
    Name: string;
    Sibling: IXMLNode;
    SiblingID: Integer;
  begin
    if Supports(aNode, ISVGElement, Element) then
    begin
      if aNode.HasAttribute('id') then
        Name := aNode.Attributes['id']
      else
        Name := aNode.LocalName;

      if assigned(aParentTreeNode) then
      begin
        if aIndex = -1 then
          TreeNode := TreeViewXML.Items.AddChild(aParentTreeNode, Name)
        else begin
          // Find the first sibling that is an element after aIndex

          Sibling := aNode.NextSibling;
          while assigned(Sibling) and (not Supports(Sibling, ISVGElement)) do
            Sibling := Sibling.NextSibling;

          if not assigned(Sibling) then
            TreeNode := TreeViewXML.Items.AddChild(aParentTreeNode, Name)
          else begin
            // Find treenode of sibling and insert before

            SiblingID := FEditor.GetElementID(Sibling as ISVGElement);

            Child := aParentTreeNode.getFirstChild;
            while assigned(Child) and (Integer(Child.Data) <> SiblingID) do
              Child := Child.getNextSibling;

            if assigned(Child) then
              TreeNode := TreeViewXML.Items.Insert(Child, Name)
            else
              TreeNode := TreeViewXML.Items.AddChild(aParentTreeNode, Name);
          end;
        end;
      end else
        TreeNode := TreeViewXML.Items.Add(nil, Name);

      TreeNode.Data := Pointer(FEditor.GetElementID(Element));
      Element.Data := TreeNode;

      for i := 0 to aNode.ChildNodes.Count - 1 do
        DoNode(TreeNode, -1, aNode.ChildNodes[i]);
    end;
  end;

begin
  // Insert a TreeNode subtree in the TreeView and associate it with the
  // element

  Handler := TreeViewXML.OnChange;
  try
    TreeViewXML.OnChange := nil;

    if Supports(aParent, ISVGElement, Element) then
    begin
      DoNode(TTreeNode(Element.Data), aIndex, aElement);
    end else begin
      TreeViewXML.Items.Clear;
      DoNode(nil, -1, aElement);
    end;

  finally
    TreeViewXML.OnChange := Handler;
  end;
end;

procedure TSVGEditorForm.TreeViewXMLNodeRemove(const aElement: ISVGElement);
var
  Handler: TTVChangedEvent;
begin
  // Remove the TreeNode associated with the element from the TreeView

  Handler := TreeViewXML.OnChange;
  try
    TreeViewXML.OnChange := nil;

    TreeViewXML.Items.Delete(TTreeNode(aElement.Data));
  finally
    TreeViewXML.OnChange := Handler;
  end;
end;

function TSVGEditorForm.TreeViewXMLNodeSelect: Boolean;
var
  i, j: Integer;
  ID: Integer;
  Element: ISVGELement;
  Handler: TTVChangedEvent;
begin
  // Synchronize the selected TreeView nodes with the selection in the editor
  // Return TRUE if the selecation changed

  Result := false;

  Handler := TreeViewXML.OnChange;
  try
    TreeViewXML.OnChange := nil;

    i := 0;
    while i < Integer(TreeViewXML.SelectionCount) do
    begin
      ID := Integer(TreeViewXML.Selections[i].Data);

      if FEditor.SelectedElementList.IndexOf(ID) = -1 then
      begin
        TreeViewXML.Selections[i].Selected := False;
        Result := True;
      end else
        Inc(i);
    end;

    i := 0;
    while i < FEditor.SelectedElementList.Count do
    begin
      ID := FEditor.SelectedElementList[i];

      j := 0;
      while j < Integer(TreeViewXML.SelectionCount) do
      begin
        if Integer(TreeViewXML.Selections[j].Data) = ID then
          Break;

        Inc(j);
      end;

      if j = Integer(TreeViewXML.SelectionCount) then
      begin
        Element := FEditor.Element[FEditor.SelectedElementList[i]];
        TTreeNode(Element.Data).Selected := True;
        Result := True;
      end;

      Inc(i);
    end;

  finally
    TreeViewXML.OnChange := Handler;
  end;
end;

procedure TSVGEditorForm.ValueListEditorAttributeEditButtonClick(
  Sender: TObject);
var
  AttrName, AttrValue: string;
begin
  if ValueListEditorAttribute.Row < 0 then
    Exit;

  AttrName :=  ValueListEditorAttribute.Keys[ValueListEditorAttribute.Row];
  AttrValue := InputBox('Attribute value', AttrName, ValueListEditorAttribute.Values[AttrName]);

  FEditor.SetAttribute(AttrName, AttrValue);
end;

procedure TSVGEditorForm.ValueListEditorAttributeUpdate;
var
  i: Integer;
  ID: Integer;
  Element: ISVGElement;
  DOMElement: IDOMElement;
  DOMAttr: IDOMNode;
begin
  // Update the value list editor with the attributes of the selected element

  ValueListEditorAttribute.Strings.Clear;

  if FEditor.SelectedElementList.Count <> 1 then
    Exit;

  ID := FEditor.SelectedElementList[0];

  Element := FEditor.Element[ID];

  if assigned(Element) then
  begin
    if Supports(Element.DOMNode, IDOMElement, DOMElement) then
    begin
      for i := 0 to DOMElement.attributes.length - 1 do
      begin
        DOMAttr := DOMElement.attributes[i];

        ValueListEditorAttribute.Strings.Add(DOMAttr.nodeName + '=' +  DOMAttr.nodeValue);
        ValueListEditorAttribute.ItemProps[DOMAttr.nodeName].EditStyle := TEditStyle.esEllipsis;
      end;
    end;
  end;
end;

procedure TSVGEditorForm.ValueListEditorAttributeValidate(Sender: TObject; ACol,
  ARow: Integer; const KeyName, KeyValue: string);
begin
  FEditor.SetAttribute(KeyName, KeyValue);
end;

procedure TSVGEditorForm.WMDROPFILES(var Msg: TWMDropFiles);
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

    DocumentOpen(FileName);
  end;

  DragFinish(Msg.Drop);

  EnableActions;
end;

end.
