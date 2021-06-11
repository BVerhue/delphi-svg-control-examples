program SVGEditor;

uses
  Vcl.Forms,
  UnitEditor in 'UnitEditor.pas' {frmEditor},
  BVE.SVGToolVCL in '..\..\Common\BVE.SVGToolVCL.pas',
  BVE.SVGEditorVCL in '..\..\Common\BVE.SVGEditorVCL.pas',
  BVE.SVGEditorFormVCL in '..\..\Common\BVE.SVGEditorFormVCL.pas',
  BVE.SVG2PathGeometry in '..\..\Common\BVE.SVG2PathGeometry.pas',
  UnitPrintPreview in '..\..\..\PrintPreview\Vcl\UnitPrintPreview.pas' {frmPrintPreview},
  BVE.SVGPrintPreviewFormVCL in '..\..\..\PrintPreview\Common\BVE.SVGPrintPreviewFormVCL.pas',
  BVE.SVGPrintPreviewVCL in '..\..\..\PrintPreview\Common\BVE.SVGPrintPreviewVCL.pas';

{$R *.res}

begin
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmEditor, frmEditor);
  Application.Run;
end.
