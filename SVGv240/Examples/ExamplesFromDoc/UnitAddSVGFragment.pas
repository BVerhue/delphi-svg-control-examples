unit UnitAddSVGFragment;

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
  BVE.SVG2Control.VCL,
  BVE.SVG2Image.VCL;
{$ELSE}
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  BVE.SVG2Control.FPC,
  BVE.SVG2Image.FPC;
{$ENDIF}

type
  TfrmAddSVGFragment = class(TForm)
    SVG2Image1: TSVG2Image;
    procedure SVG2Image1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmAddSVGFragment: TfrmAddSVGFragment;

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
     '<?xml version="1.0" encoding="UTF-8" standalone="no"?>'
     + '<svg xmlns:svg="http://www.w3.org/2000/svg" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 200 80">'
     + '<g text-anchor="middle">'
       + '<rect x="20" y="20" width="160" height="40" fill="#101010" stroke="#CCCCCC" stroke-width="2" />'
       + '<text id="text" x="100" y="45" fill="white" >Click me...</text>'
     + '</g>'
     + '<rect x="1" y="1" width="198" height="78" fill="none" stroke="blue" stroke-width="1" />'
   + '</svg>';

procedure TfrmAddSVGFragment.FormCreate(Sender: TObject);
begin
  SVG2Image1.SVG.Text := svg_text;
end;

procedure TfrmAddSVGFragment.SVG2Image1Click(Sender: TObject);
var
  Element: ISVGElement;
  Temp: string;
begin
  Element := SVG2Image1.SVGRoot.Element['text'];
  if assigned(Element) then
  begin
    Element.ChildNodes.Clear;
    Element.AddFragment('<tspan fill="yellow">Button <tspan fill="red" font-weight="bold" text-decoration="underline">clicked!</tspan></tspan>');

    Temp := SVG2Image1.SVGRoot.Doc.XML.Text;

    SVG2Image1.Repaint;
  end;
end;

end.
