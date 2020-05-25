program SVGViewerFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitViewerFMX in 'UnitViewerFMX.pas' {frmSVGViewerFMX},
  BVE.SVG2Control.FMX in '..\..\..\Common\Fmx\BVE.SVG2Control.FMX.pas',
  BVE.SVG2Elements.FMX in '..\..\..\Common\Fmx\BVE.SVG2Elements.FMX.pas',
  BVE.SVG2Image.FMX in '..\..\..\Common\Fmx\BVE.SVG2Image.FMX.pas',
  BVE.SVG2ImageList.FMX in '..\..\..\Common\Fmx\BVE.SVG2ImageList.FMX.pas',
  UnitViewerCommon.FMX in '..\..\..\Demo common\UnitViewerCommon.FMX.pas',
  UnitInfo in 'UnitInfo.pas' {frmInfo},
  BVE.SVG2Attributes in '..\..\..\Common\BVE.SVG2Attributes.pas',
  BVE.SVG2Bidi in '..\..\..\Common\BVE.SVG2Bidi.pas',
  BVE.SVG2Context in '..\..\..\Common\BVE.SVG2Context.pas',
  BVE.SVG2CSSUtility in '..\..\..\Common\BVE.SVG2CSSUtility.pas',
  BVE.SVG2Doc in '..\..\..\Common\BVE.SVG2Doc.pas',
  BVE.SVG2Elements in '..\..\..\Common\BVE.SVG2Elements.pas',
  BVE.SVG2FilterUtility in '..\..\..\Common\BVE.SVG2FilterUtility.pas',
  BVE.SVG2GeomUtility in '..\..\..\Common\BVE.SVG2GeomUtility.pas',
  BVE.SVG2Intf in '..\..\..\Common\BVE.SVG2Intf.pas',
  BVE.SVG2ParseUtility in '..\..\..\Common\BVE.SVG2ParseUtility.pas',
  BVE.SVG2PathData in '..\..\..\Common\BVE.SVG2PathData.pas',
  BVE.SVG2SaxParser in '..\..\..\Common\BVE.SVG2SaxParser.pas',
  BVE.SVG2Types in '..\..\..\Common\BVE.SVG2Types.pas',
  BVE.SVG2XMLReader in '..\..\..\Common\BVE.SVG2XMLReader.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmSVGViewerFMX, frmSVGViewerFMX);
  Application.CreateForm(TfrmInfo, frmInfo);
  Application.Run;
end.
