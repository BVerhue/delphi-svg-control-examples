program RenderAnimatedSVG;

uses
  Vcl.Forms,
  UnitRenderAnimatedSVG in 'UnitRenderAnimatedSVG.pas' {Form1},
  BVE.SVGFrameRendererVCL in '..\..\Common\BVE.SVGFrameRendererVCL.pas';

{$R *.res}

begin
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
