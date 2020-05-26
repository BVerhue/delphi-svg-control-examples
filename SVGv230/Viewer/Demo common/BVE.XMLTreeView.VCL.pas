unit BVE.XMLTreeView.VCL;

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

interface
uses
  System.Classes,
  Xml.XMLIntf,
  Vcl.ComCtrls;

procedure TreeViewUpdate(aTreeView: TTreeView; aRootXMLNode: IXMLNode);
procedure TreeViewExpand(aTreeView: TTreeView; Node: TTreeNode);

procedure TreeNodeAdd(aTreeView: TTreeView; aParentNode: TTreeNode;
  aXMLNode: IXMLNode; aLevel: integer);
procedure TreeNodeUpdate(aTreeView: TTreeView; aParentNode: TTreeNode;
  aXMLNode: IXMLNode; aLevel: integer);

procedure AttributeListUpdate(aTreeView: TTreeView; aListView: TListView;
  aLines: TStrings);

procedure AttributeValueUpdate(aListView: TListView; aLines: TStrings);
procedure AttributeValueApply(aTreeView: TTreeView; aListView: TListView;
  aLines: TStrings);

implementation

procedure TreeViewUpdate(aTreeView: TTreeView; aRootXMLNode: IXMLNode);
var
  Node: TTreeNode;
begin
  aTreeView.Items.BeginUpdate;
  try
    aTreeView.Items.Clear;

    Node := aTreeView.Items.Add( nil, 'root');
    if assigned(aRootXMLNode) then
      TreeNodeAdd( aTreeView, Node, aRootXMLNode, 2);
  finally
    aTreeView.Items.EndUpdate;
  end;
end;

procedure TreeViewExpand(aTreeView: TTreeView; Node: TTreeNode);
var
  XMLNode: IXMLNode;
  i: integer;
begin
  if Node.Data = nil then
    exit;

  XMLNode := IXMLNode(Node.Data);

  aTreeView.Items.BeginUpdate;
  try
    for i := 0 to XMLNode.ChildNodes.Count - 1 do
      TreeNodeUpdate( aTreeView, Node, XMLNode.ChildNodes[i], 2);
  finally
    aTreeView.Items.EndUpdate;
  end;
end;

procedure TreeNodeAdd(aTreeView: TTreeView; aParentNode: TTreeNode;
  aXMLNode: IXMLNode; aLevel: integer);
var
  n: TTreeNode;
  i: integer;
  NodeName, id: string;
begin
  if aLevel = 0 then
    exit;

  if aXMLNode.NodeType in [TNodeType.ntText, TNodeType.ntComment] then
    exit;

  NodeName := aXMLNode.LocalName;
  if NodeName = '#text' then
    exit;

  if aXMLNode.HasAttribute('id') then
    id := aXMLNode.Attributes[ 'id']
  else
    id := ' no id';

  n := aTreeView.Items.AddChild( aParentNode, '<' + NodeName + '>: ' + id);
  n.Data := pointer(aXMLNode);

  for i := 0 to aXMLNode.ChildNodes.Count - 1 do
  begin
    TreeNodeAdd( aTreeView, n, aXMLNode.ChildNodes[i], aLevel - 1);
  end;
end;

procedure TreeNodeUpdate(aTreeView: TTreeView; aParentNode: TTreeNode;
  aXMLNode: IXMLNode; aLevel: integer);
var
  i: integer;
  n, c: TTreeNode;
  XMLNode: IXMLNode;
begin
  if aLevel = 0 then
    exit;

  c := aParentNode.getFirstChild;
  while assigned(c) and (c.Data <> pointer(aXMLNode)) do
    c := aParentNode.GetNextChild( c);

  if not assigned(c) then
  begin
    TreeNodeAdd(aTreeView, aParentNode, aXMLNode, aLevel);
  end else begin
    n := c;
    XMLNode := IXMLNode(n.Data);
    for i := 0 to XMLNode.ChildNodes.Count - 1 do
    begin
      TreeNodeUpdate( aTreeView, n, XMLNode.ChildNodes[i], aLevel - 1);
    end;
  end;
end;

procedure AttributeListUpdate(aTreeView: TTreeView; aListView: TListView;
  aLines: TStrings);
var
  XMLNode: IXMLNode;
  NodeList: IXMLNodeList;
  ListItem: TListItem;
  i: integer;
begin
  if not assigned(aTreeView.Selected) then
    exit;

  XMLNode := IXMLNode(aTreeView.Selected.Data);

  if not assigned(XMLNode) then
    exit;

  NodeList := XMLNode.AttributeNodes;
  aListView.Items.Clear;
  for i := 0 to NodeList.Count - 1 do
  begin
    ListItem := aListView.Items.Add;
    ListItem.Caption := NodeList[i].LocalName;
    ListItem.SubItems.Add( NodeList[i].NodeValue);
    ListItem.Data := pointer(  NodeList[i]);
  end;

  if aListView.Items.Count > 0 then
    aListView.ItemIndex := 0;

  AttributeValueUpdate( aListView, aLines);
end;

procedure AttributeValueUpdate(aListView: TListView; aLines: TStrings);
var
  ListItem: TListItem;
  XMLNode: IXMLNode;
begin
  if aListView.ItemIndex >= 0 then
  begin
    ListItem := aListView.Items[aListView.ItemIndex];
    XMLNode := IXMLNode(ListItem.Data);
    if assigned(XMLNode) then
      aLines.Text := XMLNode.NodeValue
    else
      aLines.Clear;
  end else
    aLines.Clear;
end;

procedure AttributeValueApply(aTreeView: TTreeView; aListView: TListView;
  aLines: TStrings);
var
  ListItem: TListItem;
  XMLNode: IXMLNode;
begin
  if aListView.ItemIndex = -1 then
    exit;

  ListItem := aListView.Items[aListView.ItemIndex];
  XMLNode := IXMLNode(ListItem.Data);
  if assigned(XMLNode) then
  begin
    XMLNode.NodeValue := aLines.Text;
    AttributeListUpdate(aTreeView, aListView, aLines);
  end;
end;

end.
