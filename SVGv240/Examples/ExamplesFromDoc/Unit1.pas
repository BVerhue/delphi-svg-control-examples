unit Unit1;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls;
{$ELSE}
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls;
{$ENDIF}

type
  TFormClass = class of TForm;

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Button14: TButton;
    Button15: TButton;
    Button16: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure Button16Click(Sender: TObject);
    procedure Button17Click(Sender: TObject);
  private
    { Private declarations }
  public
    procedure ShowForm(aFormClass: TFormClass);
  end;

var
  Form1: TForm1;

implementation
uses
  UnitFilenameAssignment,
  UnitDigitalClock,
  UnitFilterGraph,
  UnitTSVG2GraphicAndTImage,
  UnitRenderProgrammatically,
  UnitFindElementEasy,
  UnitFindElementHard,
  UnitChangeAttribute,
  UnitSettingStyleAttributes,
  UnitAddRemoveElements,
  UnitElementUnderMousepointer,
  UnitMouseEvents,
  UnitLoadingFilesFromInternet,
  UnitAddSVGFragment,
  UnitAnimationControl,
  UnitTextToPath,
  UnitParseAndRenderInThread;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TForm1.Button10Click(Sender: TObject);
begin
  ShowForm(TfrmAddRemoveElements);
end;

procedure TForm1.Button11Click(Sender: TObject);
begin
  ShowForm(TfrmElementUnderMousepointer);
end;

procedure TForm1.Button12Click(Sender: TObject);
begin
  ShowForm(TfrmMouseEvents);
end;

procedure TForm1.Button13Click(Sender: TObject);
begin
  ShowForm(TfrmLoadingFilesFromInternet);
end;

procedure TForm1.Button14Click(Sender: TObject);
begin
  ShowForm(TfrmAddSVGFragment);
end;

procedure TForm1.Button15Click(Sender: TObject);
begin
  ShowForm(TfrmAnimationControl);
end;

procedure TForm1.Button16Click(Sender: TObject);
begin
  ShowForm(TfrmTextToPath);
end;

procedure TForm1.Button17Click(Sender: TObject);
begin
  ShowForm(TfrmParseAndRenderInThread);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowForm(TfrmFilenameAssignment);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  ShowForm(TfrmDigitalClock);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  ShowForm(TfrmFilterGraph);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  ShowForm(TfrmTSVG2GraphicAndTImage);
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  ShowForm(TfrmRenderProgrammatically);
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  ShowForm(TfrmFindElementEasy);
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  ShowForm(TfrmFindElementHard);
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
  ShowForm(TfrmChangeAttribute);
end;

procedure TForm1.Button9Click(Sender: TObject);
begin
  ShowForm(TfrmSettingStyleAttributes);
end;

procedure TForm1.ShowForm(aFormClass: TFormClass);
var
  Form: TForm;
begin
  Form := aFormClass.Create(Application);
  try
    Form.ShowModal;
  finally
    Form.Free;
  end;
end;

end.
