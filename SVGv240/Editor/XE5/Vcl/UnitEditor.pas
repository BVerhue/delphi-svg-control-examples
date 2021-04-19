unit UnitEditor;

// ------------------------------------------------------------------------------
//
//                          SVG Control Package 2.0
//                       Copyright (c) 2015 Bruno Verhue
//
// ------------------------------------------------------------------------------

interface
uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.ShellApi,
  System.SysUtils,
  System.Variants,
  System.Types,
  System.Classes,
  System.Generics.Collections,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ToolWin,
  Vcl.ActnMan,
  Vcl.ActnCtrls,
  System.Actions,
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
  BVE.SVG2Intf,
  BVE.SVG2Types,
  BVE.SVGEditorVCL,
  BVE.SVG2ImageList.VCL;

type
  TForm1 = class(TForm)
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
    TimerUpdatePage: TTimer;
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
    procedure FormDestroy(Sender: TObject);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure FormCreate(Sender: TObject);
    procedure TimerUpdatePageTimer(Sender: TObject);
    procedure ValueListEditor1EditButtonClick(Sender: TObject);
    procedure ValueListEditor1Validate(Sender: TObject; ACol, ARow: Integer;
      const KeyName, KeyValue: string);
    procedure aOpenExecute(Sender: TObject);
    procedure aZoom1to1Execute(Sender: TObject);
    procedure aZoom1to2Execute(Sender: TObject);
    procedure aSaveAsExecute(Sender: TObject);
    procedure aAddRectExecute(Sender: TObject);
    procedure aAddSVGExecute(Sender: TObject);
    procedure aCopyExecute(Sender: TObject);
    procedure aCutExecute(Sender: TObject);
    procedure aDeleteExecute(Sender: TObject);
    procedure aPasteExecute(Sender: TObject);
    procedure aUndoExecute(Sender: TObject);
    procedure aRedoExecute(Sender: TObject);
    procedure aToolTransformExecute(Sender: TObject);
    procedure aToolShapeExecute(Sender: TObject);
    procedure aNewExecute(Sender: TObject);
    procedure aPrintExecute(Sender: TObject);
    procedure aExitExecute(Sender: TObject);
    procedure aAddCircleExecute(Sender: TObject);
    procedure aAddEllipseExecute(Sender: TObject);
    procedure aAddLineExecute(Sender: TObject);
    procedure aAddPolylineExecute(Sender: TObject);
    procedure aAddPolygonExecute(Sender: TObject);
    procedure aAddPathExecute(Sender: TObject);
    procedure aAddTextExecute(Sender: TObject);
    procedure aAddImageExecute(Sender: TObject);
    procedure aAddGroupExecute(Sender: TObject);
  private
    FEditor: TSVGEditor;

    procedure UpdateValueListEditor;

    {$IFnDEF FPC}
    procedure WMDROPFILES(var Msg: TWMDropFiles); message WM_DROPFILES;
    {$ELSE}
    procedure OnDropFiles(Sender: TObject; const FileNames: array of String);
    {$ENDIF}

    procedure ElementAdd(Sender: TObject; const aParent: ISVGElement;
      const aIndex: Integer; const aElement: ISVGElement);
    procedure ElementRemove(Sender: TObject; const aElement: ISVGElement);
    procedure ElementSelect(Sender: TObject);
    procedure SetAttribute(Sender: TObject; const aElement: ISVGElement;
      const aName, aValue: TSVGUnicodeString);
    procedure ToolSelect(Sender: TObject; const aTool: TSVGToolClass);

    procedure TreeViewNodeAdd(const aParent: IXMLNode; const aIndex: Integer;
      const aElement: ISVGElement);
    function TreeViewNodeSelect: Boolean;
    procedure TreeViewNodeRemove(const aElement: ISVGElement);

    function CheckSelectionIsGroup(const aExceptions: Boolean): Boolean;

    procedure EnableActions;

    procedure NewSVGDoc;
  public
    procedure SVGOpen(const aFilename: string);
  end;

var
  Form1: TForm1;

implementation
uses
  XML.XMLDom,
  BVE.SVG2Elements;

{$R *.dfm}

procedure TForm1.aAddCircleExecute(Sender: TObject);
begin
  CheckSelectionIsGroup(True);

  FEditor.ElementAdd('<circle cx="100" cy="100" r="50" />');
end;

procedure TForm1.aAddEllipseExecute(Sender: TObject);
begin
  CheckSelectionIsGroup(True);

  FEditor.ElementAdd('<ellipse cx="100" cy="80" rx="80" ry="50" />');
end;

procedure TForm1.aAddGroupExecute(Sender: TObject);
begin
  CheckSelectionIsGroup(True);

  FEditor.ElementAdd('<g />');
end;

procedure TForm1.aAddImageExecute(Sender: TObject);
begin
  //
end;

procedure TForm1.aAddLineExecute(Sender: TObject);
begin
  //
end;

procedure TForm1.aAddPathExecute(Sender: TObject);
begin
  //
end;

procedure TForm1.aAddPolygonExecute(Sender: TObject);
begin
  //
end;

procedure TForm1.aAddPolylineExecute(Sender: TObject);
begin
  //
end;

procedure TForm1.aAddRectExecute(Sender: TObject);
begin
  CheckSelectionIsGroup(True);

  FEditor.ElementAdd('<rect x="100" y="100" width="100" height="100" />');
end;

procedure TForm1.aAddSVGExecute(Sender: TObject);
var
  sl: TStringList;
begin
  if FEditor.SelectedElementList.Count <> 1 then
    Exit;

  if OpenDialog1.Execute then
  begin
    sl := TStringList.Create;
    try
      sl.LoadFromFile(OpenDialog1.FileName);

      FEditor.ElementAdd(sl.Text);
    finally
      sl.Free;
    end;
  end;
end;

procedure TForm1.aAddTextExecute(Sender: TObject);
begin
  //
end;

procedure TForm1.aCopyExecute(Sender: TObject);
begin
  TreeView1.OnChange := nil;
  try
    FEditor.ElementsCopy;
  finally
    TreeView1.OnChange := TreeView1Change;
  end;
end;

procedure TForm1.aCutExecute(Sender: TObject);
begin
  TreeView1.OnChange := nil;
  try
    FEditor.ElementsCut;
  finally
    TreeView1.OnChange := TreeView1Change;
  end;
end;

procedure TForm1.aDeleteExecute(Sender: TObject);
begin
  TreeView1.OnChange := nil;
  try
    FEditor.ElementsDelete;
  finally
    TreeView1.OnChange := TreeView1Change;
  end;
end;

procedure TForm1.aExitExecute(Sender: TObject);
begin
  // TODO
end;

procedure TForm1.aNewExecute(Sender: TObject);
begin
  NewSVGDoc;
end;

procedure TForm1.aOpenExecute(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    SVGOpen(OpenDialog1.FileName);
  end;
end;

procedure TForm1.aPasteExecute(Sender: TObject);
begin
  TreeView1.OnChange := nil;
  try
    FEditor.ElementsPaste;
  finally
    TreeView1.OnChange := TreeView1Change;
  end;
end;

procedure TForm1.aPrintExecute(Sender: TObject);
begin
  // TODO
end;

procedure TForm1.aRedoExecute(Sender: TObject);
begin
  TreeView1.OnChange := nil;
  try
    FEditor.CmdRedo;
  finally
    TreeView1.OnChange := TreeView1Change;
  end;
end;

procedure TForm1.aSaveAsExecute(Sender: TObject);
begin
  if not assigned(FEditor.Root) then
    Exit;

  if SaveDialog1.Execute then
  begin
    FEditor.SaveToFile(SaveDialog1.Filename);
  end;
end;

procedure TForm1.aToolShapeExecute(Sender: TObject);
begin
  FEditor.ToolSelect(TSVGEditorToolShape);
end;

procedure TForm1.aToolTransformExecute(Sender: TObject);
begin
  FEditor.ToolSelect(TSVGEditorToolTransform);
end;

procedure TForm1.aUndoExecute(Sender: TObject);
begin
  TreeView1.OnChange := nil;
  try
    FEditor.CmdUndo;
  finally
    TreeView1.OnChange := TreeView1Change;
  end;
end;

procedure TForm1.aZoom1to1Execute(Sender: TObject);
begin
  FEditor.Scale := 1;
end;

procedure TForm1.aZoom1to2Execute(Sender: TObject);
begin
  FEditor.Scale := 2;
end;

function TForm1.CheckSelectionIsGroup(const aExceptions: Boolean): Boolean;
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

procedure TForm1.ElementAdd(Sender: TObject; const aParent: ISVGElement;
  const aIndex: Integer; const aElement: ISVGElement);
begin
  TreeViewNodeAdd(aParent, aIndex, aElement);
  EnableActions;
end;

procedure TForm1.ElementRemove(Sender: TObject; const aElement: ISVGElement);
begin
  TreeViewNodeRemove(aElement);
  EnableActions;
end;

procedure TForm1.ElementSelect(Sender: TObject);
begin
  if TreeViewNodeSelect then
    UpdateValueListEditor;

  EnableActions;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  {$IFnDEF FPC}
  DragAcceptFiles(Handle, True);
  {$ELSE}
  Application.AddOnDropFilesHandler(OnDropFiles);
  {$ENDIF}

  FEditor := TSVGEditor.Create(Self);
  FEditor.Parent := Self;
  FEditor.Align := alClient;
  FEditor.OnElementAdd := ElementAdd;
  FEditor.OnElementRemove := ElementRemove;
  FEditor.OnElementSelect := ElementSelect;
  FEditor.OnSetAttribute := SetAttribute;
  FEditor.OnToolSelect := ToolSelect;

  NewSVGDoc;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
{$IFDEF FPC}
  Application.RemoveOnDropFilesHandler(OnDropFiles);
{$ENDIF}
end;

procedure TForm1.NewSVGDoc;
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

procedure TForm1.SetAttribute(Sender: TObject; const aElement: ISVGElement;
  const aName, aValue: TSVGUnicodeString);
begin
  ValueListEditor1.OnValidate := nil;
  try
    ValueListEditor1.Values[aName] := aValue;
  finally
    ValueListEditor1.OnValidate := ValueListEditor1Validate;
  end;

  EnableActions;
end;

procedure TForm1.SVGOpen(const aFilename: string);
begin
  FEditor.Filename := aFilename;
end;

procedure TForm1.TimerUpdatePageTimer(Sender: TObject);
begin
  FEditor.UpdatePage([]);
end;

procedure TForm1.ToolSelect(Sender: TObject; const aTool: TSVGToolClass);
begin
  if aTool = TSVGEditorToolTransform then
    aToolTransform.Checked := True
  else

  if aTool = TSVGEditorToolShape then
    aToolShape.Checked := True;

  EnableActions;
end;

procedure TForm1.TreeView1Change(Sender: TObject; Node: TTreeNode);
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
    while i < Integer(TreeView1.SelectionCount) do
    begin
      ID := Integer(TreeView1.Selections[i].Data);
      TempList.Add(ID);

      Inc(i);
    end;

    Handler := TreeView1.OnChange;
    try
      TreeView1.OnChange := nil;

      FEditor.SelectedElementList := TempList;
    finally
      TreeView1.OnChange := Handler;
    end;

    UpdateValueListEditor;

  finally
    TempList.Free;
  end;
end;

procedure TForm1.TreeViewNodeAdd(const aParent: IXMLNode; const aIndex: Integer;
  const aElement: ISVGElement);
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
          TreeNode := TreeView1.Items.AddChild(aParentTreeNode, Name)
        else begin
          // Find the first sibling that is an element after aIndex

          Sibling := aNode.NextSibling;
          while assigned(Sibling) and (not Supports(Sibling, ISVGElement)) do
            Sibling := Sibling.NextSibling;

          if not assigned(Sibling) then
            TreeNode := TreeView1.Items.AddChild(aParentTreeNode, Name)
          else begin
            // Find treenode of sibling and insert before

            SiblingID := FEditor.GetElementID(Sibling as ISVGElement);

            Child := aParentTreeNode.getFirstChild;
            while assigned(Child) and (Integer(Child.Data) <> SiblingID) do
              Child := Child.getNextSibling;

            if assigned(Child) then
              TreeNode := TreeView1.Items.Insert(Child, Name)
            else
              TreeNode := TreeView1.Items.AddChild(aParentTreeNode, Name);
          end;
        end;
      end else
        TreeNode := TreeView1.Items.Add(nil, Name);

      TreeNode.Data := Pointer(FEditor.GetElementID(Element));
      Element.Data := TreeNode;

      for i := 0 to aNode.ChildNodes.Count - 1 do
        DoNode(TreeNode, -1, aNode.ChildNodes[i]);
    end;
  end;

begin
  // Insert a TreeNode subtree in the TreeView and associate it with the
  // element

  Handler := TreeView1.OnChange;
  try
    TreeView1.OnChange := nil;

    if Supports(aParent, ISVGElement, Element) then
    begin
      DoNode(TTreeNode(Element.Data), aIndex, aElement);
    end else begin
      TreeView1.Items.Clear;
      DoNode(nil, -1, aElement);
    end;

  finally
    TreeView1.OnChange := Handler;
  end;
end;

procedure TForm1.TreeViewNodeRemove(const aElement: ISVGElement);
var
  Handler: TTVChangedEvent;
begin
  // Remove the TreeNode associated with the element from the TreeView

  Handler := TreeView1.OnChange;
  try
    TreeView1.OnChange := nil;

    TreeView1.Items.Delete(TTreeNode(aElement.Data));
  finally
    TreeView1.OnChange := Handler;
  end;
end;

function TForm1.TreeViewNodeSelect: Boolean;
var
  i, j: Integer;
  ID: Integer;
  Element: ISVGELement;
  Handler: TTVChangedEvent;
begin
  // Synchronize the selected TreeView nodes with the selection in the editor
  // Return TRUE if the selecation changed

  Result := false;

  Handler := TreeView1.OnChange;
  try
    TreeView1.OnChange := nil;

    i := 0;
    while i < Integer(TreeView1.SelectionCount) do
    begin
      ID := Integer(TreeView1.Selections[i].Data);

      if FEditor.SelectedElementList.IndexOf(ID) = -1 then
      begin
        TreeView1.Selections[i].Selected := False;
        Result := True;
      end else
        Inc(i);
    end;

    i := 0;
    while i < FEditor.SelectedElementList.Count do
    begin
      ID := FEditor.SelectedElementList[i];

      j := 0;
      while j < Integer(TreeView1.SelectionCount) do
      begin
        if Integer(TreeView1.Selections[j].Data) = ID then
          Break;

        Inc(j);
      end;

      if j = Integer(TreeView1.SelectionCount) then
      begin
        Element := FEditor.Element[FEditor.SelectedElementList[i]];
        TTreeNode(Element.Data).Selected := True;
        Result := True;
      end;

      Inc(i);
    end;

  finally
    TreeView1.OnChange := Handler;
  end;
end;

procedure TForm1.EnableActions;
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

  aOpen.Enabled := True;
  aSaveAs.Enabled := SVGDocLoaded;
  aNew.Enabled := True;
  aPrint.Enabled := SVGDocLoaded;
  aExit.Enabled := True;

  aCopy.Enabled := CanDeleteElement;
  aCut.Enabled := CanDeleteElement;
  aDelete.Enabled := CanDeleteElement;
  aPaste.Enabled := CanAddElement;
  aUndo.Enabled := FEditor.CmdUndoCount > 0;
  aRedo.Enabled := FEditor.CmdRedoCount > 0;

  aAddRect.Enabled := CanAddElement;
  aAddSVG.Enabled := CanAddElement;
  aAddCircle.Enabled := CanAddElement;
  aAddEllipse.Enabled := CanAddElement;
  aAddLine.Enabled := CanAddElement;
  aAddPolyline.Enabled := CanAddElement;
  aAddPolygon.Enabled := CanAddElement;
  aAddPath.Enabled := CanAddElement;
  aAddText.Enabled := CanAddElement;
  aAddImage.Enabled := CanAddElement;

  aZoom1to1.Enabled := SVGDocLoaded;
  aZoom1to2.Enabled := SVGDocLoaded;

  aToolTransform.Enabled := ElementsSelected;
  aToolShape.Enabled := ElementsSelected;
end;

procedure TForm1.UpdateValueListEditor;
var
  i: Integer;
  ID: Integer;
  Element: ISVGElement;
  DOMElement: IDOMElement;
  DOMAttr: IDOMNode;
begin
  // Update the value list editor with the attributes of the selected element

  ValueListEditor1.Strings.Clear;

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

        ValueListEditor1.Strings.Add(DOMAttr.nodeName + '=' +  DOMAttr.nodeValue);
        ValueListEditor1.ItemProps[DOMAttr.nodeName].EditStyle := TEditStyle.esEllipsis;
      end;
    end;
  end;
end;

procedure TForm1.ValueListEditor1EditButtonClick(Sender: TObject);
var
  AttrName, AttrValue: string;
begin
  if ValueListEditor1.Row < 0 then
    Exit;

  AttrName :=  ValueListEditor1.Keys[ValueListEditor1.Row];
  AttrValue := InputBox('Attribute value', AttrName, ValueListEditor1.Values[AttrName]);

  FEditor.SetAttribute(AttrName, AttrValue);
end;

procedure TForm1.ValueListEditor1Validate(Sender: TObject; ACol, ARow: Integer;
  const KeyName, KeyValue: string);
begin
  FEditor.SetAttribute(KeyName, KeyValue);
end;

procedure TForm1.WMDROPFILES(var Msg: TWMDropFiles);
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

    SVGOpen(FileName);
  end;

  DragFinish(Msg.Drop);

  EnableActions;
end;

end.
