unit UnitFindElementEasy;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  BVE.SVG2Image.VCL;
{$ELSE}
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  BVE.SVG2Image.FPC;
{$ENDIF}

type

  { TfrmFindElementEasy }

  TfrmFindElementEasy = class(TForm)
    SVG2Image1: TSVG2Image;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmFindElementEasy: TfrmFindElementEasy;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

uses
  BVE.SVG2Intf;

const
  svg_text =
     '<svg viewBox="0 0 200 80">'
     + '<g font-family="courier new" text-anchor="middle">'
       + '<text id="text1" x="100" y="30" font-size="16" fill="blue" >Press a key...</text>'
       + '<text id="text2" x="100" y="60" font-size="24" fill="red" ></text>'
     + '</g>'
     + '<rect x="1" y="1" width="198" height="78" fill="none" stroke="blue" stroke-width="2" />'
   + '</svg>';

procedure TfrmFindElementEasy.FormCreate(Sender: TObject);
begin
  SVG2Image1.SVG.Text := svg_text;
end;

procedure TfrmFindElementEasy.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Element: ISVGElement;
  ShiftValue: string;
begin
  // Find element with id = 'text1'
  Element := SVG2Image1.SVGRoot.Element['text1'];
  if assigned(Element) then
  begin
    ShiftValue := '';
    if ssShift in Shift then
      ShiftValue := ShiftValue + '[Shift]';
    if ssAlt in Shift then
      ShiftValue := ShiftValue + '[Alt]';
    if ssCtrl in Shift then
      ShiftValue := ShiftValue + '[Ctrl]';

    Element.Text := ShiftValue;
  end;

  // Find element with id = 'text2'
  Element := SVG2Image1.SVGRoot.Element['text2'];
  if assigned(Element) then
  begin
    Element.Text := IntToStr(Key);
  end;

  SVG2Image1.Repaint;
end;

end.
