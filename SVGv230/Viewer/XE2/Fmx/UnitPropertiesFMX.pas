unit UnitPropertiesFMX;

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
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Rtti,
{$IFDEF Ver240Up}
  System.Actions,
  FMX.ActnList,
  FMX.StdCtrls,
{$ENDIF}
  Xml.XMLIntf,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.Layouts,
  FMX.Memo,
  FMX.TabControl,
  FMX.TreeView,
  FMX.Grid,
  BVE.SVG2Types,
  BVE.SVG2Intf,
  BVE.ViewerCore.FMX,
  BVE.XMLTreeView.FMX,
  UnitViewerFMX;

type
  TfrmProperties = class(TForm)
    Panel2: TPanel;
    Label1: TLabel;
    bParseSVG: TButton;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabItem3: TTabItem;
    TreeView1: TTreeView;
    Splitter1: TSplitter;
    Layout1: TLayout;
    Layout2: TLayout;
    Panel1: TPanel;
    Memo2: TMemo;
    Splitter2: TSplitter;
    Change: TButton;
    Memo1: TMemo;
    GroupBox1: TGroupBox;
    cbFilters: TCheckBox;
    cbClipPaths: TCheckBox;
    Memo3: TMemo;
    StringGrid1: TStringGrid;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    cbAutoViewbox: TCheckBox;
    procedure FormDestroy(Sender: TObject);
    procedure TreeView1Click(Sender: TObject);
    procedure StringGrid1Click(Sender: TObject);
    procedure SVG2Agent1ErrorNode(Sender: TObject; aNode: IXMLNode;
      const aErrorNo: Integer; const aErrorMsg: string);
    procedure Memo1Change(Sender: TObject);
    procedure Memo1Exit(Sender: TObject);
    procedure ChangeClick(Sender: TObject);
    procedure bParseSVGClick(Sender: TObject);
    procedure cbAutoViewboxChange(Sender: TObject);
    procedure cbClipPathsChange(Sender: TObject);
    procedure cbFiltersChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FTextChanged : boolean;
    FSVGSelectControl : TSVGSelectControl;
    FSVGDoc: ISVGDoc;
    procedure SetSVGSelectControl(const Value: TSVGSelectControl);
    procedure UpdateAll;
    procedure UpdateOptions;
    procedure UpdateCaption;
    procedure UpdateStatistics;
    procedure UpdateText;
    procedure UpdateTreeView;
    procedure ApplyText;
    procedure TreeNodeExpand( Sender : TObject);
  public
    property SVGSelectControl : TSVGSelectControl read FSVGSelectControl write SetSVGSelectControl;
  end;

var
  frmProperties: TfrmProperties;

implementation
uses
  BVE.SVG2Doc,
  BVE.SVG2SaxParser,
  BVE.SVG2Elements.FMX;

{$R *.fmx}

procedure TfrmProperties.ApplyText;
begin
  if assigned(FSVGSelectControl) then begin
    FSVGDoc.XML := Memo1.Lines;
    FSVGSelectControl.SVGControl.SVG := Memo1.Lines;
    UpdateAll;
  end;
end;

procedure TfrmProperties.bParseSVGClick(Sender: TObject);
begin
  if assigned(FSVGSelectControl) then begin

    if FTextChanged then
      ApplyText;

    FSVGSelectControl.ParseSVG;
  end;
end;

procedure TfrmProperties.cbAutoViewboxChange(Sender: TObject);
begin
  if assigned(FSVGSelectControl) then begin
    FSVGSelectControl.AutoViewbox := cbAutoViewBox.IsChecked;
    FSVGSelectControl.Repaint;
  end;
end;

procedure TfrmProperties.cbClipPathsChange(Sender: TObject);
begin
  if assigned(FSVGSelectControl) then begin
    if cbClippaths.IsChecked then
      FSVGSelectControl.RenderOptions := FSVGSelectControl.RenderOptions + [sroClipPath]
    else
      FSVGSelectControl.RenderOptions := FSVGSelectControl.RenderOptions - [sroClipPath];
    FSVGSelectControl.Repaint;
  end;
end;

procedure TfrmProperties.cbFiltersChange(Sender: TObject);
begin
  if assigned(FSVGSelectControl) then begin
    if cbFilters.IsChecked then
      FSVGSelectControl.RenderOptions := FSVGSelectControl.RenderOptions + [sroFilters]
    else
      FSVGSelectControl.RenderOptions := FSVGSelectControl.RenderOptions - [sroFilters];
    FSVGSelectControl.Repaint;
  end;
end;

procedure TfrmProperties.ChangeClick(Sender: TObject);
begin
  AttributeValueApply( TreeView1, StringGrid1, Memo2.Lines);
  Memo1.Lines.Assign(FSVGDoc.XML);
  FSVGSelectControl.SVGControl.SVG := FSVGDoc.XML;
end;

procedure TfrmProperties.FormActivate(Sender: TObject);
begin
  UpdateAll;
end;

procedure TfrmProperties.FormCreate(Sender: TObject);
begin
  FSVGDoc := TSVG2Doc.Create(self);
end;

procedure TfrmProperties.FormDestroy(Sender: TObject);
begin
  FSVGSelectControl := nil;
end;

procedure TfrmProperties.Memo1Change(Sender: TObject);
begin
  FTextChanged := True;
end;

procedure TfrmProperties.SetSVGSelectControl(const Value: TSVGSelectControl);
begin
  if FSVGSelectControl <> Value then begin
    FSVGSelectControl := Value;
    if Visible then begin
      UpdateAll;
    end;
  end;
end;

procedure TfrmProperties.StringGrid1Click(Sender: TObject);
begin
   AttributeValueUpdate( TreeView1, StringGrid1, Memo2.Lines);
end;

procedure TfrmProperties.SVG2Agent1ErrorNode(Sender: TObject; aNode: IXMLNode;
  const aErrorNo: Integer; const aErrorMsg: string);
begin
  Memo3.Lines.Add( aErrorMsg)
end;

procedure TfrmProperties.UpdateAll;
begin
  UpdateCaption;
  UpdateText;
  UpdateOptions;
  UpdateTreeView;
  UpdateStatistics;
end;

procedure TfrmProperties.UpdateCaption;
begin
  if assigned(FSVGSelectControl) then begin
    Label1.Text := FSVGSelectControl.SVGControl.FileName
  end else begin
    Label1.Text := 'No svg selected'
  end;
end;

procedure TfrmProperties.UpdateOptions;
begin
  if assigned(FSVGSelectControl) then
  begin
    cbAutoViewbox.IsChecked := FSVGSelectControl.AutoViewbox;
    cbFilters.IsChecked := sroFilters in FSVGSelectControl.RenderOptions;
    cbClippaths.IsChecked := sroClippath in FSVGSelectControl.RenderOptions;
  end;
end;

procedure TfrmProperties.UpdateStatistics;
begin
  if assigned(FSVGSelectControl) then
  begin
    Memo3.Lines.Clear;
    Memo3.Lines.Add('');
    Memo3.Lines.Add('--------------------------------------------');
    Memo3.Lines.Add(FSVGSelectControl.SVGControl.FileName);
    Memo3.Lines.Add('');
    Memo3.Lines.AddStrings(FSVGSelectControl.Statistics);
    Memo3.Lines.Add('--------------------------------------------');
  end;
end;

procedure TfrmProperties.UpdateText;
begin
  if assigned(FSVGSelectControl) then
  begin
    if FSVGSelectControl.SVGControl.SVG.Count <> 0 then
      Memo1.Lines.Assign(FSVGSelectControl.SVGControl.SVG)
    else
      Memo1.Lines.LoadFromFile(FSVGSelectControl.SVGControl.Filename);
  end else
    Memo1.Lines.Clear;
  FTextChanged := False;
end;

procedure TfrmProperties.UpdateTreeView;
begin
 if assigned(FSVGSelectControl) then
  begin
     if FSVGSelectControl.SVGControl.SVG.Count <> 0 then
     begin
       FSVGDoc.FileName := '';
       FSVGDoc.XML := FSVGSelectControl.SVGControl.SVG
     end else begin
       FSVGDoc.XML.Clear;
       FSVGDoc.FileName := FSVGSelectControl.SVGControl.Filename;
     end;
    FSVGDoc.Active := True;

    TreeViewUpdate( TreeView1, StringGrid1, FSVGDoc.DocumentElement, TreeNodeExpand)
  end else
    TreeViewUpdate( TreeView1, StringGrid1, nil, TreeNodeExpand);
end;

procedure TfrmProperties.TreeNodeExpand(Sender: TObject);
begin
  if Sender is TTreeNode then
    TreeViewExpandNode( TreeView1, Sender as TTreeNode, TreeNodeExpand);
end;

procedure TfrmProperties.TreeView1Click(Sender: TObject);
begin
  AttributeListUpdate( TreeView1, StringGrid1, Memo2.Lines);
end;

procedure TfrmProperties.Memo1Exit(Sender: TObject);
begin
 if FTextChanged then
   ApplyText;
end;

end.
