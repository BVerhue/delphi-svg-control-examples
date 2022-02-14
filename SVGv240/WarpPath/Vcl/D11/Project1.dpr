program Project1;

uses
  Vcl.Forms,
  Unit1 in '..\Source\Unit1.pas' {Form1},
  BVE.SVGWarpPath in '..\Source\BVE.SVGWarpPath.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
