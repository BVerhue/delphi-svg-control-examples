program RenderAnimatedSVG;

uses
  Vcl.Forms,
  UnitRenderAnimatedSVG in 'UnitRenderAnimatedSVG.pas' {frmRenderAnimatedSVG},
  BVE.SVGFrameRendererVCL in '..\..\Common\BVE.SVGFrameRendererVCL.pas';

{$R *.res}

begin
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmRenderAnimatedSVG, frmRenderAnimatedSVG);
  Application.Run;
end.
