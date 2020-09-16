program SVGViewerFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitViewerFMX in 'UnitViewerFMX.pas' {frmSVGViewerFMX},
  UnitViewerCommon.FMX in '..\..\Demo common\UnitViewerCommon.FMX.pas',
  UnitInfo in 'UnitInfo.pas' {frmInfo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmSVGViewerFMX, frmSVGViewerFMX);
  Application.CreateForm(TfrmInfo, frmInfo);
  Application.Run;
end.
