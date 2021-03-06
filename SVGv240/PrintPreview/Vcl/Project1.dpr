program Project1;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  UnitPrintPreview in 'UnitPrintPreview.pas' {frmPrintPreview},
  BVE.SVGPrintPreviewFormVCL in '..\Common\BVE.SVGPrintPreviewFormVCL.pas',
  BVE.SVGPrintPreviewVCL in '..\Common\BVE.SVGPrintPreviewVCL.pas';

{$R *.res}

begin
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TfrmPrintPreview, frmPrintPreview);
  Application.Run;
end.
