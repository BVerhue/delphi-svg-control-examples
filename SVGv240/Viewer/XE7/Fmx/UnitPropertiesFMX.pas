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

{$Include '..\..\Demo common\DemoAppCompilerSettings.inc'}

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Actions,
  System.Rtti,
  System.IOUtils,
  Xml.XMLIntf,
  FMX.Types,
{$IFDEF VER260Up}
  FMX.Graphics,
{$ENDIF}
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.Layouts,
  FMX.Memo,
  FMX.StdCtrls,
  FMX.ActnList,
  FMX.TabControl,
  FMX.TreeView,
  FMX.Grid,
  FMX.Objects,
  FMX.ListBox,
  BVE.SVG2CSSUtility,
  BVE.SVG2Types,
  BVE.SVG2Intf,
  BVE.ViewerCore.FMX,
  BVE.XMLTreeView.FMX,
  UnitViewerFMX;

type
  TfrmProperties = class(TForm)
    Memo1: TMemo;
    Panel2: TPanel;
    Label1: TLabel;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabItem3: TTabItem;
    Button1: TButton;
    ActionList1: TActionList;
    aParseSVG: TAction;
    aAttributeChange: TAction;
    TreeView1: TTreeView;
    Splitter1: TSplitter;
    Layout1: TLayout;
    Memo2: TMemo;
    Panel1: TPanel;
    Button2: TButton;
    Splitter2: TSplitter;
    StringGrid1: TStringGrid;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    cbFilters: TCheckBox;
    cbClippaths: TCheckBox;
    GroupBox1: TGroupBox;
    Memo3: TMemo;
    cbAutoViewbox: TCheckBox;
    procedure aParseSVGExecute(Sender: TObject);
    procedure aAttributeChangeExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TreeView1Click(Sender: TObject);
    procedure StringGrid1Click(Sender: TObject);
    procedure SVG2Agent1ErrorNode(Sender: TObject; aNode: IXMLNode;
      const aErrorNo: Integer; const aErrorMsg: string);
    procedure Memo1Change(Sender: TObject);
    procedure Memo1Exit(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbClippathsChange(Sender: TObject);
    procedure cbAutoViewboxChange(Sender: TObject);
    procedure cbFiltersChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FTextChanged : boolean;
    FSVGSelectControl : TSVGSelectControl;
    FSVGDoc: ISVGDoc;
    procedure SetSVGSelectControl(const Value: TSVGSelectControl);
    procedure UpdateCaption;
    procedure UpdateText;
    procedure UpdateTreeView;
    procedure UpdateOptions;
    procedure UpdateStatistics;
    procedure UpdateAll;
    procedure ApplyText;
    procedure TreeNodeExpand( Sender : TObject);
  public
    property SVGSelectControl : TSVGSelectControl read FSVGSelectControl write SetSVGSelectControl;
  end;

var
  frmProperties: TfrmProperties;

implementation
uses
  System.IniFiles,
  BVE.SVG2Doc,
  BVE.SVG2SaxParser,
  BVE.SVG2Elements.FMX;

{$R *.fmx}

procedure TfrmProperties.aAttributeChangeExecute(Sender: TObject);
begin
  AttributeValueApply( TreeView1, StringGrid1, Memo2.Lines);
  Memo1.Lines.Assign(FSVGDoc.XML);
  FSVGSelectControl.SVGControl.SVG := FSVGDoc.XML;
end;

procedure TfrmProperties.aParseSVGExecute(Sender: TObject);
begin
  if assigned(FSVGSelectControl) then begin

    if FTextChanged then
      ApplyText;

    FSVGSelectControl.ParseSVG;
  end;
end;

procedure TfrmProperties.ApplyText;
begin
  if assigned(FSVGSelectControl) then begin
    FSVGDoc.XML := Memo1.Lines;
    FSVGSelectControl.SVGControl.SVG := Memo1.Lines;
    UpdateAll;
  end;
end;

procedure TfrmProperties.cbClippathsChange(Sender: TObject);
begin
  if assigned(FSVGSelectControl) then begin
    if cbClippaths.IsChecked then
      FSVGSelectControl.RenderOptions := FSVGSelectControl.RenderOptions + [sroClipPath]
    else
      FSVGSelectControl.RenderOptions := FSVGSelectControl.RenderOptions - [sroClipPath];
    FSVGSelectControl.Repaint;
  end;
end;

procedure TfrmProperties.cbAutoViewboxChange(Sender: TObject);
begin
  if assigned(FSVGSelectControl) then begin
    FSVGSelectControl.AutoViewbox := cbAutoViewBox.IsChecked;
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

procedure TfrmProperties.FormCreate(Sender: TObject);
begin
  FSVGDoc := TSVG2Doc.Create(self);
end;

procedure TfrmProperties.FormDestroy(Sender: TObject);
begin
  FSVGSelectControl := nil;
end;

procedure TfrmProperties.FormShow(Sender: TObject);
begin
  UpdateAll;
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
