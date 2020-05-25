unit UnitSettingStyleAttributes;

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

  { TfrmSettingStyleAttributes }

  TfrmSettingStyleAttributes = class(TForm)
    SVG2Image1: TSVG2Image;
    procedure FormCreate(Sender: TObject);
    procedure SVG2Image1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSettingStyleAttributes: TfrmSettingStyleAttributes;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

uses
  BVE.SVG2Intf;

const
  svg_style1 =
      ' text {'
     + ' font-family: "courier new";'
     + ' font-size: 24;'
     + ' }';

  svg_style2 =
      ' text {'
     + ' font-family: "arial";'
     + ' font-size: 16;'
     + ' }';

  svg_text =
     '<svg viewBox="0 0 200 80">'
     + '<style id="stylesheet" type="text/css">'
     + svg_style1
     + '</style>'
     + '<g text-anchor="middle">'
       + '<text id="text1" x="100" y="30" fill="blue" >Blue</text>'
       + '<text id="text2" x="100" y="60" fill="red" >Red</text>'
     + '</g>'
     + '<rect x="1" y="1" width="198" height="78" fill="none" stroke="blue" stroke-width="2" />'
   + '</svg>';

procedure TfrmSettingStyleAttributes.FormCreate(Sender: TObject);
begin
  SVG2Image1.SVG.Text := svg_text;
end;

procedure TfrmSettingStyleAttributes.SVG2Image1Click(Sender: TObject);
var
  Element: ISVGElement;
begin
  // Change inline style (highes priority)

  Element := SVG2Image1.SVGRoot.Element['text2'];
  if assigned(Element) then
    Element.Attributes['style'] := 'font-family: "Times New Roman"; font-size: 24';

  // Change style sheet (medium priority)

  Element := SVG2Image1.SVGRoot.Element['stylesheet'];
  if assigned(Element) then
    Element.Text := svg_style2;

  // Change attribute (lowest priority, this will have no effect)

  Element := SVG2Image1.SVGRoot.Element['text1'];
  if assigned(Element) then
    Element.Attributes['font-family'] := 'Georgia';

  SVG2Image1.Repaint;
end;

end.
