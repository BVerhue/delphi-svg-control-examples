unit Unit1;

// -----------------------------------------------------------------------------
//
// Random SVG star
//
// B.J.H. Verhue
//
// -----------------------------------------------------------------------------

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ExtCtrls,
  BVE.SVG2Image.VCL;

type
  TForm1 = class(TForm)
    SVG2Image1: TSVG2Image;
    Timer1: TTimer;
    procedure Timer1Timer(Sender: TObject);
    procedure SVG2Image1Click(Sender: TObject);
  private
    FTime: integer;
    function LFO(const f: single): single;
    function LFO2(const s, f: single): single;
  public
    function StarPath(const aEdgeCount: integer; const aRIn, aROut, aOuterEdge: single): string;
    procedure Update;
  end;

var
  Form1: TForm1;

implementation
uses
  Vcl.Clipbrd,
  System.Math;

{$R *.dfm}

{ TForm1 }

function TForm1.LFO(const f: single): single;
begin
  // Low frequency oscilator

  Result := Sin(2* PI * f * FTime / 1000);
end;

function TForm1.LFO2(const s, f: single): single;
begin
  // Coupled LFO's to simulate randomness

  Result := LFO(s * LFO(f));
end;

function SVGColor(const aR, aG, aB: single): string;
begin
  Result := '#'
     + IntToHex(Round(255 * Abs(aR)), 2)
     + IntToHex(Round(255 * Abs(aG)), 2)
     + IntToHex(Round(255 * Abs(aB)), 2);
end;

function TForm1.StarPath(const aEdgeCount: integer; const aRIn, aROut,
  aOuterEdge: single): string;
var
  i: integer;
  InnerAngle, OuterAngle, X, Y: single;
begin
  // Create starshaped pathdata
  // aEdgeCount : number of edges or points
  // aRIn       : radius of star core
  // aROuter    : outer radius of star
  // aOuterEdge : width of star point

  Result := '';

  InnerAngle := 2 * PI / aEdgeCount;
  OuterAngle := arctan2(aOuterEdge, aROut);

  for i := 0 to aEdgeCount - 1 do
  begin
    X := aRIn * Sin(i * InnerAngle);
    Y := aRIn * Cos(i * InnerAngle);

    if i = 0 then
      Result := Result + Format('M%0:.2f,%1:.2f', [X, Y])
    else
      Result := Result + Format('L%0:.2f,%1:.2f', [X, Y]);

    X := aROut * Sin((i + 0.5 - OuterAngle) * InnerAngle);
    Y := aROut * Cos((i + 0.5 - OuterAngle) * InnerAngle);
    Result := Result + Format('L%0:.2f,%1:.2f', [X, Y]);

    X := aROut * Sin((i + 0.5 + OuterAngle) * InnerAngle);
    Y := aROut * Cos((i + 0.5 + OuterAngle) * InnerAngle);
    Result := Result + Format('L%0:.2f,%1:.2f', [X, Y]);

  end;
  Result := Result + 'Z';
end;

procedure TForm1.SVG2Image1Click(Sender: TObject);
begin
  Timer1.Enabled := not Timer1.Enabled;

  // Put SVG on clipboard if paused

  if not Timer1.Enabled then
    Clipboard.AsText := SVG2Image1.SVG.Text;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  // Timer interval is set to 25ms, FPS=40

  Update;
  Inc(FTime, Timer1.Interval);
end;

procedure TForm1.Update;
begin
  // Create SVG content and assign to SVG2Image

  SVG2Image1.SVG.Text :=
      '<svg version="1.1" id="star"'
    + ' viewBox="-100 -100 200 200"'
    + ' xmlns="http://www.w3.org/2000/svg">'

  // Defs section with a random radial gradient

    + ' <defs>'
      + ' <radialGradient id="radGrad1">'
      + ' <stop offset="0%" stop-color="'
          + SVGColor(LFO2(0.01, 0.02), LFO2(0.015, 0.03), LFO2(0.008, 0.015))
          +  '" />'
      + ' <stop offset="100%" stop-color="'
          + SVGColor(LFO2(0.02, 0.01), LFO2(0.025, 0.015), LFO2(0.03, 0.008))
          + '" />'
      + ' </radialGradient>'
    + '</defs>'

  // Path element with random starshape

    + '<path stroke="pink" fill="url(#radGrad1)" stroke-width="3" d="'

    + StarPath(
        Round(19 + 16 * LFO2(0.01, 0.01)),
        50 + 30 * LFO2(0.03, 0.005),
        80 + 10 * LFO2(0.008, 0.01),
        16 + 16 * LFO2(0.01, 0.002))

    + '"/>'

    + '</svg>';

  SVG2Image1.Repaint;
end;

initialization
  // Set decimal seperator for "Format" function
  FormatSettings.DecimalSeparator := '.';

end.
