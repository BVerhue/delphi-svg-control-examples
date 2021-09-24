program Project1;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  UnitPrintPreview in 'UnitPrintPreview.pas' {frmPrintPreview},
  BVE.SVGPrintPreviewFormFMX in '..\..\Common\BVE.SVGPrintPreviewFormFMX.pas',
  BVE.SVGPrintPreviewFMX in '..\..\Common\BVE.SVGPrintPreviewFMX.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TfrmPrintPreview, frmPrintPreview);
  Application.Run;
end.
