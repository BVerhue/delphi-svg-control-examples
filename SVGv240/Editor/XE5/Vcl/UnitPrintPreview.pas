unit UnitPrintPreview;

// ------------------------------------------------------------------------------
//
//                          SVG Control Package 2.0
//                       Copyright (c) 2015 Bruno Verhue
//
// ------------------------------------------------------------------------------

// The SVG Editor need at least version v2.40 update 9 of the SVG library

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Actions,
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
    RadioGroup1: TRadioGroup;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    ePagesHorizontal: TEdit;
    ePagesVertical: TEdit;
    eMarginLeft: TEdit;
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
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmPrintPreview: TfrmPrintPreview;

implementation
uses
  UnitEditor;

{$R *.dfm}

procedure TfrmPrintPreview.FormCreate(Sender: TObject);
begin
  ActionOrientationLandscape := aOrientationLandscape;
  ActionOrientationPortait := aOrientationPortait;
  ActionPrint := aPrint;
  ActionCancel := aCancel;
  ActionPrinterSelect := aPrinterSelect;

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

  frmEditor.FormPrintPreview := Self;
end;

end.
