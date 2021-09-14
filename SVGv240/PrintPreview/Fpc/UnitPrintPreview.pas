unit UnitPrintPreview;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  ActnList,
  BVE.SVGPrintPreviewFormVCL;

type

  { TfrmPrintPreview }

  TfrmPrintPreview = class(TSVGPrintPreviewForm)
    aCancel: TAction;
    ActionList1: TActionList;
    aIdenticalMargins: TAction;
    aOrientationLandscape: TAction;
    aOrientationPortait: TAction;
    aPrint: TAction;
    aPrinterSelect: TAction;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    cbAlign: TComboBox;
    cbAutoViewbox: TCheckBox;
    cbIdenticalMargins: TCheckBox;
    cbMeetOrSlice: TComboBox;
    cbUnits: TComboBox;
    eGlueEdge: TEdit;
    eMarginBottom: TEdit;
    eMarginLeft: TEdit;
    eMarginRight: TEdit;
    eMarginTop: TEdit;
    eOutputDevice: TEdit;
    ePagesHorizontal: TEdit;
    ePagesVertical: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure RadioButton1Click(Sender: TObject);
    procedure RadioButton2Click(Sender: TObject);
  private

  public

  end;

var
  frmPrintPreview: TfrmPrintPreview;

implementation

{$R *.lfm}

{ TfrmPrintPreview }

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

procedure TfrmPrintPreview.RadioButton1Click(Sender: TObject);
begin
  aOrientationPortait.Execute;
end;

procedure TfrmPrintPreview.RadioButton2Click(Sender: TObject);
begin
  aOrientationLandscape.Execute;
end;

end.

