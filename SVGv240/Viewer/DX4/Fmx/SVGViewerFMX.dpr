program SVGViewerFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitViewerFMX in 'UnitViewerFMX.pas' {frmSVGViewerFMX},
  UnitViewerCommon.FMX in '..\..\Demo common\UnitViewerCommon.FMX.pas',
  UnitInfo in 'UnitInfo.pas' {frmInfo},
  UnitPrintPreview in '..\..\..\PrintPreview\Fmx\DX4\UnitPrintPreview.pas' {frmPrintPreview},
  BVE.SVGPrintPreviewFMX in '..\..\..\PrintPreview\Common\BVE.SVGPrintPreviewFMX.pas',
  BVE.SVGPrintPreviewFormFMX in '..\..\..\PrintPreview\Common\BVE.SVGPrintPreviewFormFMX.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmSVGViewerFMX, frmSVGViewerFMX);
  Application.CreateForm(TfrmInfo, frmInfo);
  Application.CreateForm(TfrmPrintPreview, frmPrintPreview);
  Application.Run;
end.
