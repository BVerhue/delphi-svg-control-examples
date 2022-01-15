unit UnitPrintPreview;

// ------------------------------------------------------------------------------
//
//                          SVG Control Package 2.0
//                       Copyright (c) 2015 Bruno Verhue
//
// ------------------------------------------------------------------------------

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
///    Platform specific print preview form definition
///  </summary>
///  <remarks>
///    The SVG Print Preview need at least version v2.40 update 9 of the SVG library
///  </remarks>

interface
uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  {-$IF CompilerVersion >= 24}
  System.Actions,
  {-$IFEND}
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ActnList,
  Vcl.PlatformDefaultStyleActnCtrls,
  Vcl.ActnMan,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  BVE.SVGPrintPreviewFormVCL;

type
  TfrmPrintPreview = class(TSVGPrintPreviewForm)
    Panel1: TPanel;
    Splitter1: TSplitter;
    PageSetupDialog1: TPageSetupDialog;
    eOutputDevice: TEdit;
    Label1: TLabel;
    Button1: TButton;
    ActionManager1: TActionManager;
    aOrientationLandscape: TAction;
    aOrientationPortait: TAction;
    aPrint: TAction;
    aCancel: TAction;
    aPrinterSelect: TAction;
    Panel2: TPanel;
    Button2: TButton;
    Button3: TButton;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    ePagesHorizontal: TEdit;
    ePagesVertical: TEdit;
    eMarginTop: TEdit;
    eMarginRight: TEdit;
    eMarginBottom: TEdit;
    eGlueEdge: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    cbUnits: TComboBox;
    GroupBox3: TGroupBox;
    cbAutoViewbox: TCheckBox;
    cbAlign: TComboBox;
    cbMeetOrSlice: TComboBox;
    Label10: TLabel;
    Label11: TLabel;
    eMarginLeft: TEdit;
    GroupBox4: TGroupBox;
    cbIdenticalMargins: TCheckBox;
    aIdenticalMargins: TAction;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmPrintPreview: TfrmPrintPreview;

implementation

{$R *.dfm}

procedure TfrmPrintPreview.FormCreate(Sender: TObject);
begin
  ActionOrientationLandscape := aOrientationLandscape;
  ActionOrientationPortait := aOrientationPortait;
  ActionPrint := aPrint;
  ActionCancel := aCancel;
  ActionPrinterSelect := aPrinterSelect;
  ActionIdenticalMargins := aIdenticalMargins;

  ComboBoxUnits := cbUnits;
  CheckBoxAutoViewbox := cbAutoViewBox;
  ComboBoxAlign := cbAlign;
  ComboBoxMeetOrSlice := cbMeetOrSlice;

  EditOutputDevice := eOutputDevice;
  EditPagesHorizontal := ePagesHorizontal;
  EditPagesVertical := ePagesVertical;
  EditMarginLeft := eMarginLeft;
  EditMarginTop := eMarginTop;
  EditMarginRight := eMarginRight;
  EditMarginBottom := eMarginBottom;
  EditGlueEdge := eGlueEdge;
end;

end.
