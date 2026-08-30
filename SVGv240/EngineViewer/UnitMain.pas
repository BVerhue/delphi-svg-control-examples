unit UnitMain;

//------------------------------------------------------------------------------
//
//  Displays the animated engine SVG in a TSVG2Image and lets the running speed
//  be set with a track bar.
//
//  TSVG2AnimationTimer has no speed setting: it runs its own thread at a fixed
//  frame rate. So instead the timer is put in the paused state, which shuts
//  that thread down, and the SMIL clock is driven from here with AdvanceFrame.
//  Feeding it (elapsed time * speed) scales the whole animation, keeping every
//  animation in the document in step with each other.
//
//------------------------------------------------------------------------------

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  System.Diagnostics,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  BVE.SVG2Types,
  BVE.SVG2Intf,
  BVE.SVG2Doc,
  BVE.SVG2Elements.VCL,
  BVE.SVG2Control.VCL,
  BVE.SVG2Image.VCL;

type
  TfrmMain = class(TForm)
    SVG2Image1: TSVG2Image;
    SVG2AnimationTimer1: TSVG2AnimationTimer;
    pnlControls: TPanel;
    lblSpeedCaption: TLabel;
    lblReadout: TLabel;
    tbSpeed: TTrackBar;
    btnReset: TButton;
    tmrTick: TTimer;
    OpenDialog1: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tbSpeedChange(Sender: TObject);
    procedure tmrTickTimer(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure SVG2Image1AfterParse(Sender: TObject);
  private
    FClock: TStopwatch;
    FCarry: Double;      // sub millisecond remainder, see tmrTickTimer
    FSpeed: Double;      // 1.0 is the timing as authored in the SVG

    function FindInFolder(const aDir, aFileName, aMask: string): string;
    function LocateSVG(const aFileName, aMask: string): string;

    procedure LoadEngine;
    procedure UpdateReadout;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

const
  /// <summary>
  ///  The document plays one complete four stroke cycle, which is two crank
  ///  revolutions, in four seconds. Used to convert the speed factor into
  ///  something recognisable.
  /// </summary>
  CycleSeconds = 4.0;
  RevsPerCycle = 2.0;

  SVGFileName = 'engine-combined-animated.svg';

  /// <summary>
  ///  Accepted as well, so that a renamed or versioned copy of the document
  ///  is still picked up without having to browse for it.
  /// </summary>
  SVGFileMask = 'engine-combined*.svg';

  /// <summary>
  ///  The documents live in this folder next to the project. It is looked for
  ///  at every level walked up from the executable, so running from the IDE
  ///  output folder finds it just as well as running next to it.
  /// </summary>
  SVGSubFolder = 'Img';

  /// <summary>
  ///  Longest time step accepted in one tick. Without this the animation would
  ///  jump forward after the message loop has been blocked, by dragging the
  ///  window or by sitting on a breakpoint.
  /// </summary>
  MaxStepMSec = 250;

//------------------------------------------------------------------------------
//
//                                   TfrmMain
//
//------------------------------------------------------------------------------

function TfrmMain.FindInFolder(const aDir, aFileName, aMask: string): string;
var
  Rec: TSearchRec;
begin
  Result := aDir + aFileName;

  if FileExists(Result) then
    Exit;

  // Fall back to the mask, so a renamed or versioned copy of the document is
  // still picked up without having to browse for it.

  if FindFirst(aDir + aMask, faAnyFile and not faDirectory, Rec) = 0 then
  try
    Result := aDir + Rec.Name;
    Exit;
  finally
    FindClose(Rec);
  end;

  Result := '';
end;

function TfrmMain.LocateSVG(const aFileName, aMask: string): string;
var
  Dir: string;
  i: Integer;
begin
  // Walk up from the executable, so the document is found both when running
  // from the IDE output folder and when the exe sits next to it.

  Dir := ExtractFilePath(ParamStr(0));

  for i := 0 to 4 do
  begin
    Result := FindInFolder(IncludeTrailingPathDelimiter(Dir + SVGSubFolder),
      aFileName, aMask);

    if Result <> '' then
      Exit;

    Result := FindInFolder(Dir, aFileName, aMask);

    if Result <> '' then
      Exit;

    Dir := ExtractFilePath(ExcludeTrailingPathDelimiter(Dir));

    if Dir = '' then
      Break;
  end;

  Result := '';
end;

procedure TfrmMain.LoadEngine;
var
  FileName: string;
begin
  FileName := LocateSVG(SVGFileName, SVGFileMask);

  if FileName = '' then
  begin
    OpenDialog1.InitialDir := ExtractFilePath(ParamStr(0));

    if not OpenDialog1.Execute then
    begin
      lblReadout.Caption := 'No document loaded';
      Exit;
    end;

    FileName := OpenDialog1.FileName;
  end;

  SVG2Image1.Filename := FileName;

  Caption := Format('Engine viewer - %s', [ExtractFileName(FileName)]);
end;

procedure TfrmMain.UpdateReadout;
begin
  if FSpeed <= 0 then
  begin
    lblReadout.Caption := 'Stopped';
    Exit;
  end;

  lblReadout.Caption := Format(
    '%.0f %%      %.1f rpm      one cycle in %.2f s',
    [FSpeed * 100,
     RevsPerCycle / CycleSeconds * 60 * FSpeed,
     CycleSeconds / FSpeed]);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FSpeed := tbSpeed.Position / 100;
  FCarry := 0;

  LoadEngine;

  UpdateReadout;

  FClock := TStopwatch.StartNew;
  tmrTick.Enabled := True;
end;

procedure TfrmMain.SVG2Image1AfterParse(Sender: TObject);
begin
  // The timer state has to be applied here rather than in FormCreate. Setting
  // Filename does not parse straight away, and TSVGRoot.DoAnimationTimerStart
  // only reaches the time container "if assigned(Doc)", so starting it any
  // earlier is silently ignored and nothing ever animates.

  // Forced through the off state so that a reload starts the new document's
  // time container as well.

  SVG2AnimationTimer1.IsPaused := False;
  SVG2AnimationTimer1.IsStarted := False;

  // Start the SMIL time containers, then pause, which destroys the timer
  // thread. From here on AdvanceFrame is the only thing moving the clock.

  SVG2AnimationTimer1.IsStarted := True;
  SVG2AnimationTimer1.IsPaused := True;

  FCarry := 0;
  FClock := TStopwatch.StartNew;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  tmrTick.Enabled := False;

  SVG2AnimationTimer1.IsPaused := False;
  SVG2AnimationTimer1.IsStarted := False;
end;

procedure TfrmMain.tmrTickTimer(Sender: TObject);
var
  Elapsed: Double;
  Step: Cardinal;
begin
  Elapsed := FClock.Elapsed.TotalMilliseconds;
  FClock := TStopwatch.StartNew;

  if Elapsed > MaxStepMSec then
    Elapsed := MaxStepMSec;

  // AdvanceFrame takes whole milliseconds, so the remainder has to be carried
  // over to the next tick. Without that, every speed below about 7% would
  // truncate to zero and the engine would never move at all.

  FCarry := FCarry + Elapsed * FSpeed;

  if FCarry < 1 then
    Exit;

  Step := Trunc(FCarry);
  FCarry := FCarry - Step;

  SVG2AnimationTimer1.AdvanceFrame(Step);
end;

procedure TfrmMain.tbSpeedChange(Sender: TObject);
begin
  FSpeed := tbSpeed.Position / 100;

  // Drop whatever was carried, so releasing the slider at zero stops at once

  FCarry := 0;

  UpdateReadout;
end;

procedure TfrmMain.btnResetClick(Sender: TObject);
begin
  tbSpeed.Position := 100;
end;

end.
