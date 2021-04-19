program SVGEditor;

uses
  Vcl.Forms,
  UnitEditor in 'UnitEditor.pas' {Form1},
  BVE.SVGToolVCL in '..\..\Common\BVE.SVGToolVCL.pas',
  BVE.SVGEditorVCL in '..\..\Common\BVE.SVGEditorVCL.pas',
  BVE.SVGEditorFormVCL in '..\..\Common\BVE.SVGEditorFormVCL.pas';

{$R *.res}

begin
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
