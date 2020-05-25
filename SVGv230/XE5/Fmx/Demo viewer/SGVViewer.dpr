program SGVViewer;

uses
  FMX.Forms,
  UnitPropertiesFMX in 'UnitPropertiesFMX.pas' {frmProperties},
  UnitViewerFMX in 'UnitViewerFMX.pas' {frmViewer},
  BVE.SVG2Attributes in '..\..\..\Common\BVE.SVG2Attributes.pas',
  BVE.SVG2CSSUtility in '..\..\..\Common\BVE.SVG2CSSUtility.pas',
  BVE.SVG2Doc in '..\..\..\Common\BVE.SVG2Doc.pas',
  BVE.SVG2Elements in '..\..\..\Common\BVE.SVG2Elements.pas',
  BVE.SVG2GeomUtility in '..\..\..\Common\BVE.SVG2GeomUtility.pas',
  BVE.SVG2Intf in '..\..\..\Common\BVE.SVG2Intf.pas',
  BVE.SVG2ParseUtility in '..\..\..\Common\BVE.SVG2ParseUtility.pas',
  BVE.SVG2PathData in '..\..\..\Common\BVE.SVG2PathData.pas',
  BVE.SVG2Types in '..\..\..\Common\BVE.SVG2Types.pas',
  BVE.SVG2Elements.FMX in '..\..\..\Common\Fmx\BVE.SVG2Elements.FMX.pas',
  BVE.Selection.FMX in '..\..\..\Demo common\BVE.Selection.FMX.pas',
  BVE.ViewerCore.FMX in '..\..\..\Demo common\BVE.ViewerCore.FMX.pas',
  BVE.XMLTreeView.FMX in '..\..\..\Demo common\BVE.XMLTreeView.FMX.pas',
  BVE.SVG2Control.FMX in '..\..\..\Common\Fmx\BVE.SVG2Control.FMX.pas',
  System.SysUtils,
  BVE.SVG2SaxParser in '..\..\..\Common\BVE.SVG2SaxParser.pas',
  BVE.SVG2XMLReader in '..\..\..\Common\BVE.SVG2XMLReader.pas',
  BVE.SVG2FilterUtility in '..\..\..\Common\BVE.SVG2FilterUtility.pas',
  BVE.SVG2Bidi in '..\..\..\Common\BVE.SVG2Bidi.pas',
  BVE.SVG2Context in '..\..\..\Common\BVE.SVG2Context.pas';

{$R *.res}

begin
{$IFDEF DEBUG}
  // This may result in an access violation, See QC#125171
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}

  Application.Initialize;
  Application.CreateForm(TfrmViewer, frmViewer);
  Application.CreateForm(TfrmProperties, frmProperties);
  Application.Run;
end.
