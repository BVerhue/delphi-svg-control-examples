unit BVE.XMLTreeView.FMX;

// ------------------------------------------------------------------------------
//
// SVG Control 2.0
// Copyright (c) 2015 Bruno Verhue
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
// notice, this list of conditions and the following disclaimer.
// 2. Redistributions in binary form must reproduce the above copyright
// notice, this list of conditions and the following disclaimer in the
// documentation and/or other materials provided with the distribution.
// 3. The name of the author may not be used to endorse or promote products
// derived from this software without specific prior written permission.
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

{$I '..\Common\CompilerSettings.inc'}

interface

uses
  System.Classes,
  Xml.XMLIntf,
  FMX.Types,
  FMX.TreeView,
  FMX.Grid;

type
  TTreeNode = class(TTreeViewItem)
  private
    FXMLNode: IXMLNode;

    FOnChangeExpanded: TNotifyEvent;
  protected
    procedure ApplyStyle; override;
    procedure DoChangeExpanded(Sender: TObject);
  published
    property XMLNode: IXMLNode read FXMLNode write FXMLNode;
    property OnChangeExpanded: TNotifyEvent read FOnChangeExpanded
      write FOnChangeExpanded;
  end;

procedure TreeViewUpdate(aTreeView: TTreeView; aGrid: TStringGrid;
  aRootXMLNode: IXMLNode; aTreeNodeExpandProc: TNotifyEvent);
procedure TreeViewExpandNode(aTreeView: TTreeView; Node: TTreeNode;
  aTreeNodeExpandProc: TNotifyEvent);

procedure TreeNodeAdd(aTreeView: TTreeView; aParentNode: TFMXObject;
  aXMLNode: IXMLNode; aLevel: integer; aTreeNodeExpandProc: TNotifyEvent);
procedure TreeNodeUpdate(aTreeView: TTreeView; aParentNode: TTreeNode;
  aXMLNode: IXMLNode; aLevel: integer; aTreeNodeExpandProc: TNotifyEvent);

procedure AttributeListUpdate(aTreeView: TTreeView; aGrid: TStringGrid;
  aLines: TStrings);

procedure AttributeValueUpdate(aTreeView: TTreeView; aGrid: TStringGrid;
  aLines: TStrings);
procedure AttributeValueApply(aTreeView: TTreeView; aGrid: TStringGrid;
  aLines: TStrings);

implementation

uses
  FMX.Ani;

procedure TreeViewUpdate(aTreeView: TTreeView; aGrid: TStringGrid;
  aRootXMLNode: IXMLNode; aTreeNodeExpandProc: TNotifyEvent);
begin
  aTreeView.BeginUpdate;
  try
    aGrid.RowCount := 1;
    aTreeView.Clear;
    if assigned(aRootXMLNode) then
    begin
      TreeNodeAdd(aTreeView, aTreeView, aRootXMLNode, 2, aTreeNodeExpandProc);
    end;
  finally
    aTreeView.EndUpdate;
    aTreeView.Repaint;
  end;
end;

procedure TreeViewExpandNode(aTreeView: TTreeView; Node: TTreeNode;
  aTreeNodeExpandProc: TNotifyEvent);
var
  i: integer;
begin
{$IFDEF Ver250Up}
  if aTreeView.IsUpdating then
    exit;
{$ENDIF}
  if not assigned(Node) then
    exit;

  if not Node.IsExpanded then
    exit;

  aTreeView.BeginUpdate;
  try
    for i := 0 to Node.XMLNode.ChildNodes.Count - 1 do
    begin
      TreeNodeUpdate(aTreeView, Node, Node.XMLNode.ChildNodes[i], 2,
        aTreeNodeExpandProc);
    end;

  finally
    aTreeView.EndUpdate;
  end;
  aTreeView.Repaint;
end;

procedure TreeNodeAdd(aTreeView: TTreeView; aParentNode: TFMXObject;
  aXMLNode: IXMLNode; aLevel: integer; aTreeNodeExpandProc: TNotifyEvent);
var
  n: TTreeNode;
  i: integer;
  NodeName, id: string;
begin
  if aLevel = 0 then
    exit;

  if aXMLNode.IsTextElement then
    exit;

  NodeName := aXMLNode.LocalName;
  if NodeName = '#text' then
    exit;

  if aXMLNode.HasAttribute('id') then
    id := aXMLNode.Attributes['id']
  else
    id := ' no id';

  n := TTreeNode.Create(aTreeView);
  n.XMLNode := aXMLNode;
  n.OnChangeExpanded := aTreeNodeExpandProc;
  n.Parent := aParentNode;
  n.Text := '<' + aXMLNode.LocalName + '>: ' + id;
  for i := 0 to aXMLNode.ChildNodes.Count - 1 do
  begin
    TreeNodeAdd(aTreeView, n, aXMLNode.ChildNodes[i], aLevel - 1,
      aTreeNodeExpandProc);
  end;
end;

procedure TreeNodeUpdate(aTreeView: TTreeView; aParentNode: TTreeNode;
  aXMLNode: IXMLNode; aLevel: integer; aTreeNodeExpandProc: TNotifyEvent);
var
  i: integer;
  n: TTreeNode;
begin
  if aLevel = 0 then
    exit;

  i := 0;
  while (i < aParentNode.Count) and ((aParentNode.Items[i] as TTreeNode).XMLNode
    <> aXMLNode) do
    inc(i);

  if not(i < aParentNode.Count) then
  begin
    TreeNodeAdd(aTreeView, aParentNode, aXMLNode, aLevel, aTreeNodeExpandProc);
  end
  else
  begin
    n := aParentNode.Items[i] as TTreeNode;
    for i := 0 to n.XMLNode.ChildNodes.Count - 1 do
    begin
      TreeNodeUpdate(aTreeView, n, n.XMLNode.ChildNodes[i], aLevel - 1,
        aTreeNodeExpandProc);
    end;
  end;
end;

procedure AttributeListUpdate(aTreeView: TTreeView; aGrid: TStringGrid;
  aLines: TStrings);
var
  NodeList: IXMLNodeList;
  XMLNode: IXMLNode;
  i: integer;
begin
  if not assigned(aTreeView.Selected) then
    exit;

  XMLNode := TTreeNode(aTreeView.Selected).FXMLNode;

  NodeList := XMLNode.AttributeNodes;
  aGrid.RowCount := NodeList.Count;
  if NodeList.Count > 0 then
  begin
    for i := 0 to NodeList.Count - 1 do
    begin
      aGrid.Cells[0, i] := NodeList[i].LocalName;
      aGrid.Cells[1, i] := NodeList[i].NodeValue;
    end;
    if aGrid.Selected = -1 then
      aGrid.Selected := 0;
    if aGrid.Selected >= NodeList.Count then
      aGrid.Selected := NodeList.Count - 1;
  end;

  AttributeValueUpdate(aTreeView, aGrid, aLines);
end;

procedure AttributeValueUpdate(aTreeView: TTreeView; aGrid: TStringGrid;
  aLines: TStrings);
var
  NodeList: IXMLNodeList;
  XMLNode: IXMLNode;
begin
  if not assigned(aTreeView.Selected) then
    exit;

  XMLNode := TTreeNode(aTreeView.Selected).FXMLNode;

  NodeList := XMLNode.AttributeNodes;

  if (aGrid.Selected >= 0) and (aGrid.Selected < NodeList.Count) then
    aLines.Text := NodeList[aGrid.Selected].NodeValue
  else
    aLines.Text := '';
end;

procedure AttributeValueApply(aTreeView: TTreeView; aGrid: TStringGrid;
  aLines: TStrings);
var
  NodeList: IXMLNodeList;
  XMLNode: IXMLNode;
begin
  if not assigned(aTreeView.Selected) then
    exit;

  XMLNode := TTreeNode(aTreeView.Selected).FXMLNode;

  NodeList := XMLNode.AttributeNodes;

  if aGrid.Selected < NodeList.Count then
  begin
    NodeList[aGrid.Selected].NodeValue := aLines.Text;
    AttributeListUpdate(aTreeView, aGrid, aLines);
  end;

end;

// -----------------------------------------------------------------------------
//
// TTreeNode
// Trick for missing onexpand event: http://monkeystyler.com/blog
//
// -----------------------------------------------------------------------------

procedure TTreeNode.ApplyStyle;
var
  Ani: TFloatAnimation;
  Obj: TFMXObject;
begin
  inherited;

  Obj := FindStyleResource('button');
  if assigned(Obj) then
  begin
    Ani := TFloatAnimation.Create(Obj);
    Ani.Parent := Obj;
    Ani.Stored := False;
    Ani.StartValue := 0.999999999999;
    Ani.StopValue := 1;
    Ani.PropertyName := 'Opacity';
    Ani.Trigger := 'IsExpanded=true';
    Ani.TriggerInverse := 'IsExpanded=false';
    Ani.Duration := 0;
    Ani.OnFinish := DoChangeExpanded;
  end;
end;

procedure TTreeNode.DoChangeExpanded(Sender: TObject);
begin
  if assigned(OnChangeExpanded) then
    OnChangeExpanded(Self);
end;

end.
