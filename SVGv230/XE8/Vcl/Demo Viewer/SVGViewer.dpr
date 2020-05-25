program SVGViewer;

uses
  Vcl.Forms,
  UnitViewer in 'UnitViewer.pas' {Form1},
  BVE.SVG2Types in '..\..\..\Common\BVE.SVG2Types.pas',
  BVE.SVG2CSSUtility in '..\..\..\Common\BVE.SVG2CSSUtility.pas',
  BVE.SVG2Attributes in '..\..\..\Common\BVE.SVG2Attributes.pas',
  BVE.SVG2Doc in '..\..\..\Common\BVE.SVG2Doc.pas',
  BVE.SVG2Elements in '..\..\..\Common\BVE.SVG2Elements.pas',
  BVE.SVG2GeomUtility in '..\..\..\Common\BVE.SVG2GeomUtility.pas',
  BVE.SVG2Intf in '..\..\..\Common\BVE.SVG2Intf.pas',
  BVE.SVG2ParseUtility in '..\..\..\Common\BVE.SVG2ParseUtility.pas',
  BVE.SVG2PathData in '..\..\..\Common\BVE.SVG2PathData.pas',
  BVE.SVG2Elements.VCL in '..\..\..\Common\Vcl\BVE.SVG2Elements.VCL.pas',
  UnitProperties in 'UnitProperties.pas' {Form2},
  BVE.ViewerCore.VCL in '..\..\..\Demo common\BVE.ViewerCore.VCL.pas',
  BVE.XMLTreeView.VCL in '..\..\..\Demo common\BVE.XMLTreeView.VCL.pas',
  BVE.SVG2Control.VCL in '..\..\..\Common\Vcl\BVE.SVG2Control.VCL.pas',
  BVE.SVG2ImageList.VCL in '..\..\..\Common\Vcl\BVE.SVG2ImageList.VCL.pas',
  BVE.SVG2SaxParser in '..\..\..\Common\BVE.SVG2SaxParser.pas',
  BVE.SVG2XMLReader in '..\..\..\Common\BVE.SVG2XMLReader.pas',
  BVE.SVG2FilterUtility in '..\..\..\Common\BVE.SVG2FilterUtility.pas',
  UnitAbout in 'UnitAbout.pas' {Form3},
  BVE.SVG2Image.VCL in '..\..\..\Common\Vcl\BVE.SVG2Image.VCL.pas',
  BVE.SVG2Graphic.VCL in '..\..\..\Common\Vcl\BVE.SVG2Graphic.VCL.pas',
  BVE.SVG2Bidi in '..\..\..\Common\BVE.SVG2Bidi.pas',
  BVE.SVG2Context in '..\..\..\Common\BVE.SVG2Context.pas';

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
  Application.Run;
end.
