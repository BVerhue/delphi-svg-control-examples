program SGVViewer;

uses
  FMX.Forms,
  UnitViewerFMX in 'UnitViewerFMX.pas' {frmViewer},
  UnitPropertiesFMX in 'UnitPropertiesFMX.pas' {frmProperties},
  BVE.Selection.FMX in '..\..\Demo common\BVE.Selection.FMX.pas',
  BVE.ViewerCore.FMX in '..\..\Demo common\BVE.ViewerCore.FMX.pas',
  BVE.XMLTreeView.FMX in '..\..\Demo common\BVE.XMLTreeView.FMX.pas';

{$R *.res}

begin
{$IFDEF DEBUG}
  // This may result in an access violation, See QC#125171
  //ReportMemoryLeaksOnShutdown := True;
{$ENDIF}

  Application.Initialize;
  Application.CreateForm(TfrmViewer, frmViewer);
  Application.CreateForm(TfrmProperties, frmProperties);
  Application.Run;
end.
