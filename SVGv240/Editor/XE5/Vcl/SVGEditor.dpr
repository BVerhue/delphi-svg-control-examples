program SVGEditor;

uses
  Vcl.Forms,
  UnitEditor in 'UnitEditor.pas' {frmEditor},
  BVE.SVGToolVCL in '..\..\Common\BVE.SVGToolVCL.pas',
  BVE.SVGEditorVCL in '..\..\Common\BVE.SVGEditorVCL.pas',
  BVE.SVGEditorFormVCL in '..\..\Common\BVE.SVGEditorFormVCL.pas',
  UnitPrintPreview in 'UnitPrintPreview.pas' {frmPrintPreview},
  BVE.SVGPrintPreviewFormVCL in '..\..\Common\BVE.SVGPrintPreviewFormVCL.pas',
  BVE.SVGPrintPreviewVCL in '..\..\Common\BVE.SVGPrintPreviewVCL.pas';

{$R *.res}

begin
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmEditor, frmEditor);
  Application.CreateForm(TfrmPrintPreview, frmPrintPreview);
  Application.Run;
end.
