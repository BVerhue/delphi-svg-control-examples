program SVGViewerFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitViewerFMX in 'UnitViewerFMX.pas' {frmSVGViewerFMX},
  UnitViewerCommon.FMX in '..\Demo common\UnitViewerCommon.FMX.pas',
  UnitInfo in 'UnitInfo.pas' {frmInfo},
  BVE.SVGPrintPreviewFMX in '..\..\PrintPreview\Common\BVE.SVGPrintPreviewFMX.pas',
  BVE.SVGPrintPreviewFormFMX in '..\..\PrintPreview\Common\BVE.SVGPrintPreviewFormFMX.pas',
  // The print preview form of the newest PrintPreview example; its .pas is the same for XE8 and later
  UnitPrintPreview in '..\..\PrintPreview\Fmx\D11\UnitPrintPreview.pas' {frmPrintPreview};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmSVGViewerFMX, frmSVGViewerFMX);
  Application.CreateForm(TfrmInfo, frmInfo);
  Application.CreateForm(TfrmPrintPreview, frmPrintPreview);
  Application.Run;
end.
