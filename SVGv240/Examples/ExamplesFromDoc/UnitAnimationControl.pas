unit UnitAnimationControl;

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
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  BVE.SVG2Control.VCL,
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
  ExtCtrls,
  StdCtrls,
  ComCtrls,
  BVE.SVG2Control.FPC,
{$ENDIF}
  BVE.SVG2Doc;

type
  TfrmAnimationControl = class(TForm)
    Panel1: TPanel;
    cbStart: TCheckBox;
    cbPause: TCheckBox;
    TrackBar1: TTrackBar;
    Label1: TLabel;
    SVG2AnimationTimer1: TSVG2AnimationTimer;
    SVG2WinControl1: TSVG2WinControl;
    procedure SVG2AnimationTimer1Sample(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure cbPauseClick(Sender: TObject);
    procedure cbStartClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure UpdateControls;
  end;

var
  frmAnimationControl: TfrmAnimationControl;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TfrmAnimationControl.cbPauseClick(Sender: TObject);
begin
  SVG2AnimationTimer1.IsPaused := cbPause.Checked;
end;

procedure TfrmAnimationControl.cbStartClick(Sender: TObject);
begin
  SVG2AnimationTimer1.IsStarted := cbStart.Checked;
end;

procedure TfrmAnimationControl.SVG2AnimationTimer1Sample(Sender: TObject);
begin
  UpdateControls;
end;

procedure TfrmAnimationControl.TrackBar1Change(Sender: TObject);
begin
  if SVG2AnimationTimer1.IsPaused then
  begin
    SVG2WinControl1.AnimationTime := TrackBar1.Position;
    UpdateControls;
  end;
end;

procedure TfrmAnimationControl.UpdateControls;
begin
  Label1.Caption := Format('Time: %4.1f  FPS: %3.1f',
    [SVG2WinControl1.AnimationTime / 1000, SVG2AnimationTimer1.FPS]);

  if not SVG2AnimationTimer1.IsPaused then
    TrackBar1.Position := SVG2WinControl1.AnimationTime;
end;

end.
