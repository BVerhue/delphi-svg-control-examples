unit UnitProperties;

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
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  {$IFDEF Ver240Up}
  System.Actions,
  {$ENDIF}
  Xml.XMLIntf,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.PlatformDefaultStyleActnCtrls,
  Vcl.ActnList,
  Vcl.ActnMan,
  Vcl.ToolWin,
  Vcl.ActnCtrls,
  BVE.ViewerCore.VCL;

type
  TForm2 = class(TSVGViewerPropertiesForm)
    Panel1: TPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TreeView1: TTreeView;
    Splitter1: TSplitter;
    Memo1: TMemo;
    ActionManager1: TActionManager;
    Memo2: TMemo;
    Panel3: TPanel;
    Splitter2: TSplitter;
    ListView1: TListView;
    ActionToolBar1: TActionToolBar;
    ActionToolBar2: TActionToolBar;
    Panel4: TPanel;
    aParseSVG: TAction;
    aAttributeChange: TAction;
    TabSheet3: TTabSheet;
    GroupBox1: TGroupBox;
    cbFilters: TCheckBox;
    cbClippaths: TCheckBox;
    Memo3: TMemo;
    cbAutoViewbox: TCheckBox;
    cbEvents: TCheckBox;
    procedure FormCreate(Sender: TObject);
  end;

var
  Form2: TForm2;

implementation
uses
  UnitViewer;

{$R *.dfm}

{ TForm2 }

procedure TForm2.FormCreate(Sender: TObject);
begin
  Form1.FormProperties := Self;

  ConnectControls(aAttributeChange, aParseSVG, Memo2, Memo3, Memo1, Panel1,
    cbFilters, cbAutoViewbox, cbEvents, cbClippaths, Listview1, Treeview1);
end;

end.
