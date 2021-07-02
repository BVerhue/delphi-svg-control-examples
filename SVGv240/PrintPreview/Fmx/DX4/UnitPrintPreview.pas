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
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Actions,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.ListBox,
  FMX.Edit,
  FMX.ActnList,
  FMX.Controls.Presentation,
  BVE.SVGPrintPreviewFormFMX;

type
  TfrmPrintPreview = class(TSVGPrintPreviewForm)
    Panel1: TPanel;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    ActionList1: TActionList;
    aOrientationLandscape: TAction;
    aOrientationPortait: TAction;
    aPrint: TAction;
    aCancel: TAction;
    aPrinterSelect: TAction;
    aIdenticalMargins: TAction;
    Button3: TButton;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    cbUnits: TComboBox;
    Label1: TLabel;
    eMarginLeft: TEdit;
    eMarginTop: TEdit;
    eMarginRight: TEdit;
    eMarginBottom: TEdit;
    CheckBox1: TCheckBox;
    eGlueEdge: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    cbAutoViewbox: TCheckBox;
    cbAlign: TComboBox;
    cbMeetOrSlice: TComboBox;
    Label7: TLabel;
    Label8: TLabel;
    ePagesHorizontal: TEdit;
    ePagesVertical: TEdit;
    Label9: TLabel;
    Label10: TLabel;
    lOutputDevice: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmPrintPreview: TfrmPrintPreview;

implementation

{$R *.fmx}

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
