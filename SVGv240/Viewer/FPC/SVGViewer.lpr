program SVGViewer;

{$Mode objfpc}{$H+}
{-$Define DEBUG}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, BVE.SVG2Graphic.FPC, BVE.SVG2Control.FPC, Unit1, unit2, unit3
  { you can add units after this }
  {$IFDEF DEBUG}
  ,SysUtils
  {$ENDIF}
  ;

{$R *.res}

begin
{$IFDEF DEBUG}
  if FileExists('D:\Crash20140412\Documenten\Delphi\SVG\SVGDev\SVGv230\FPC\Demo Viewer\heap.trc') then
    DeleteFile('D:\Crash20140412\Documenten\Delphi\SVG\SVGDev\SVGv230\FPC\Demo Viewer\heap.trc');
  SetHeapTraceOutput('D:\Crash20140412\Documenten\Delphi\SVG\SVGDev\SVGv230\FPC\Demo Viewer\heap.trc');
{$ENDIF}

  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.

