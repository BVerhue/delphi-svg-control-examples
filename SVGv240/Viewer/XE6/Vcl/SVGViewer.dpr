program SVGViewer;

uses
  Vcl.Forms,
  UnitViewer in 'UnitViewer.pas' {Form1},
  UnitProperties in 'UnitProperties.pas' {Form2},
  BVE.ViewerCore.VCL in '..\..\Demo common\BVE.ViewerCore.VCL.pas',
  BVE.XMLTreeView.VCL in '..\..\Demo common\BVE.XMLTreeView.VCL.pas',
  UnitAbout in 'UnitAbout.pas' {Form3},
  BVE.SVGPrintPreviewFormVCL in '..\..\..\PrintPreview\Common\BVE.SVGPrintPreviewFormVCL.pas',
  BVE.SVGPrintPreviewVCL in '..\..\..\PrintPreview\Common\BVE.SVGPrintPreviewVCL.pas',
  UnitPrintPreview in '..\..\..\PrintPreview\Vcl\UnitPrintPreview.pas' {frmPrintPreview};

{$R *.res}

begin
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TForm3, Form3);
  Application.CreateForm(TfrmPrintPreview, frmPrintPreview);
  Application.Run;
end.
