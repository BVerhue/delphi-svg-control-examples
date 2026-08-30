program EngineViewer;

uses
  Vcl.Forms,
  UnitMain in 'UnitMain.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Engine viewer';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
