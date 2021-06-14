program Project1;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
  Forms, Interfaces,
  Unit1 in 'Unit1.pas' {Form1},
  UnitPrintPreview in 'UnitPrintPreview.pas' {frmPrintPreview},
  BVE.SVGPrintPreviewFormVCL in '..\Common\BVE.SVGPrintPreviewFormVCL.pas',
  BVE.SVGPrintPreviewVCL in '..\Common\BVE.SVGPrintPreviewVCL.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TfrmPrintPreview, frmPrintPreview);
  Application.Run;
end.
