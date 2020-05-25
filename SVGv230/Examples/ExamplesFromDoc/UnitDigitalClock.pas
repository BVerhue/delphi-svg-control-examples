unit UnitDigitalClock;

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
  TfrmDigitalClock = class(TForm)
    SVG2Image1: TSVG2Image;
    Timer1: TTimer;
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmDigitalClock: TfrmDigitalClock;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

const
  svg_clock =
     '<svg xmlns="http://www.w3.org/2000/svg" version="1.1"'
    + ' viewBox="0 0 200 80">'
     + '<g font-family="courier new" font-size="24" text-anchor="middle">'
       + '<text x="100" y="30" fill="blue" >%s</text>'
       + '<text x="100" y="60" fill="blue" >%s</text>'
     + '</g>'
     + '<rect x="1" y="1" width="198" height="78" fill="none" stroke="blue" stroke-width="2" />'
   + '</svg>';

procedure TfrmDigitalClock.Timer1Timer(Sender: TObject);
begin
  SVG2Image1.SVG.Text := Format(svg_clock, [DateToStr(Now), TimeToStr(Now)]);
  SVG2Image1.Repaint;
end;

end.
