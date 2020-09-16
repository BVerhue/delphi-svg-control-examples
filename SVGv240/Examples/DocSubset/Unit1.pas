unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BVE.SVG2Image.VCL, Vcl.StdCtrls,
  Vcl.ExtCtrls, BVE.SVG2Control.VCL;

type
  TForm1 = class(TForm)
    SVG2Image1: TSVG2Image;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure SVG2Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SVG2Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses
  System.Math;

{$R *.dfm}

// You can download the cards SVG from this location:
// http://svg-cards.sourceforge.net/

// Then, set the SVG2Image1 FileName property to the downloaded svg file

// A couple of cards to show at random

const
  cards: array[0..9] of string = ('7_diamond', '10_spade', '1_spade',
     '3_heart', 'queen_heart', '5_diamond', '2_club', 'jack_diamond',
     'king_club', 'black_joker');


procedure TForm1.SVG2Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  SVG2Image1.RootID := 'back';
end;

procedure TForm1.SVG2Image1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i: integer;
begin
  i := RandomRange(0, 9);
  Caption := cards[i];
  SVG2Image1.RootID := cards[i];
end;

end.
