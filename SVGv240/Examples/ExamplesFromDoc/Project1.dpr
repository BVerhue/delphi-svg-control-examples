program Project1;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
  {$IFnDEF FPC}
  Vcl.Forms,
  {$ELSE}
  Forms, Interfaces,
  {$ENDIF }
  Unit1 in 'Unit1.pas' {Form1},
  UnitFilenameAssignment in 'UnitFilenameAssignment.pas' {frmFilenameAssignment},
  UnitDigitalClock in 'UnitDigitalClock.pas' {frmDigitalClock},
  UnitFilterGraph in 'UnitFilterGraph.pas' {frmFilterGraph},
  UnitTSVG2GraphicAndTImage in 'UnitTSVG2GraphicAndTImage.pas' {frmTSVG2GraphicAndTImage},
  UnitRenderProgrammatically in 'UnitRenderProgrammatically.pas' {frmRenderProgrammatically},
  UnitFindElementEasy in 'UnitFindElementEasy.pas' {frmFindElementEasy},
  UnitFindElementHard in 'UnitFindElementHard.pas' {frmFindElementHard},
  UnitChangeAttribute in 'UnitChangeAttribute.pas' {frmChangeAttribute},
  UnitSettingStyleAttributes in 'UnitSettingStyleAttributes.pas' {frmSettingStyleAttributes},
  UnitAddRemoveElements in 'UnitAddRemoveElements.pas' {frmAddRemoveElements},
  UnitElementUnderMousepointer in 'UnitElementUnderMousepointer.pas' {frmElementUnderMousepointer},
  UnitMouseEvents in 'UnitMouseEvents.pas' {frmMouseEvents},
  UnitLoadingFilesFromInternet in 'UnitLoadingFilesFromInternet.pas' {frmLoadingFilesFromInternet},
  UnitAddSVGFragment in 'UnitAddSVGFragment.pas' {frmAddSVGFragment};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
