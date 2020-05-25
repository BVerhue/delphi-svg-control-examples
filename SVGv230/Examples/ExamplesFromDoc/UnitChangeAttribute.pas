unit UnitChangeAttribute;

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
  Vcl.ExtCtrls,
  BVE.SVG2Image.VCL;
{$ELSE}
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  BVE.SVG2Image.FPC;
{$ENDIF}

type

  { TfrmChangeAttribute }

  TfrmChangeAttribute = class(TForm)
    SVG2Image1: TSVG2Image;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure SVG2Image1AfterParse(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    t: integer;
  end;

var
  frmChangeAttribute: TfrmChangeAttribute;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

uses
  BVE.SVG2Intf;

const
  svg_transform =
    'translate(%3.2f,%3.2f) rotate(%3.2f)';

  svg_orbit =
     '<svg width="200" height="200" viewBox="0 0 200 200">'
     + '<ellipse cx="100" cy="100" rx="100" ry="100" fill="none" stroke="blue" stroke-width="2" />'
     + '<g id="g1">'
       + '<circle cx="0" cy="0" r="20" fill="none" stroke="blue" stroke-width="2" />'
       + '<text x="0" y="12" text-anchor="middle" font-size="36" fill="blue" >A</text>'
     + '</g>'
   + '</svg>';

procedure TfrmChangeAttribute.FormCreate(Sender: TObject);
begin
  SVG2Image1.SVG.Text := svg_orbit;
end;

procedure TfrmChangeAttribute.SVG2Image1AfterParse(Sender: TObject);
begin
  t := 0;
  Timer1.Enabled := True;
end;

procedure TfrmChangeAttribute.Timer1Timer(Sender: TObject);
var
  Element: ISVGElement;
  A1, A2, Ar: Double;
begin
  Element := SVG2Image1.SVGRoot.Element['g1'];
  if assigned(Element) then
  begin
    A1 := 360*(t mod 100)/100;
    Ar := A1*PI/180;

    A2 := -A1*100/20;

    Element.Attributes['transform'] :=
      Format(svg_transform, [100+80*Cos(Ar), 100+80*Sin(Ar), A2]);
  end;

  Inc(t);

  SVG2Image1.Repaint;
end;

end.
