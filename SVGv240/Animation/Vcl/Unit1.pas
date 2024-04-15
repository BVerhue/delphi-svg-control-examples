unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BVE.SVG2Doc, BVE.SVG2Control.VCL,
  BVE.SVG2Image.VCL, Vcl.StdCtrls, Vcl.ExtCtrls, System.Actions, Vcl.ActnList,
  Vcl.PlatformDefaultStyleActnCtrls, Vcl.ActnMan, Vcl.ToolWin, Vcl.ActnCtrls,
  Vcl.ComCtrls;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    cbStart: TCheckBox;
    cbPause: TCheckBox;
    SVG2Image1: TSVG2Image;
    SVG2AnimationTimer1: TSVG2AnimationTimer;
    OpenDialog1: TOpenDialog;
    ActionManager1: TActionManager;
    aLoadSVG: TAction;
    aStart: TAction;
    aPause: TAction;
    ActionToolBar1: TActionToolBar;
    TrackBar1: TTrackBar;
    lTime: TLabel;
    procedure aLoadSVGExecute(Sender: TObject);
    procedure aStartExecute(Sender: TObject);
    procedure aPauseExecute(Sender: TObject);
    procedure SVG2AnimationTimer1Sample(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.aLoadSVGExecute(Sender: TObject);
var
  sl: TStringList;
begin
  if OpenDialog1.Execute then
  begin
    sl := TStringList.Create;
    try
      sl.LoadFromFile(OPenDialog1.FileName);

      SVG2Image1.SVG.Assign(sl);
    finally
      sl.Free;
    end;
  end;
end;

procedure TForm1.aPauseExecute(Sender: TObject);
begin
  aPause.Checked := not aPause.Checked;
  SVG2AnimationTimer1.IsPaused := aPause.Checked;
end;

procedure TForm1.aStartExecute(Sender: TObject);
begin
  aStart.Checked := not aStart.Checked;
  SVG2AnimationTimer1.IsStarted := aStart.Checked;
end;

procedure TForm1.SVG2AnimationTimer1Sample(Sender: TObject);
begin
  lTime.Caption := Format(' Time: %5.1f   FPS: %3.0f ',
    [SVG2Image1.AnimationTime / 1000, SVG2AnimationTimer1.FPS]);

  if Integer(SVG2Image1.AnimationTime) > Trackbar1.Max then
    Trackbar1.Max := Trackbar1.Max + 5000;

  if not SVG2AnimationTimer1.IsPaused then
    Trackbar1.Position := SVG2Image1.AnimationTime;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  SVG2Image1.AnimationTime := Trackbar1.Position;
  SVG2AnimationTimer1Sample(Self);
end;

end.
