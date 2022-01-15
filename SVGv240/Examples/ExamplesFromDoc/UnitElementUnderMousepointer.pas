unit UnitElementUnderMousepointer;

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
  BVE.SVG2Image.VCL, BVE.SVG2Control.VCL;
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
  TfrmElementUnderMousepointer = class(TForm)
    SVG2Image1: TSVG2Image;
    procedure SVG2Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmElementUnderMousepointer: TfrmElementUnderMousepointer;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

uses
  BVE.SVG2Types,
  BVE.SVG2Intf;

procedure TfrmElementUnderMousepointer.FormCreate(Sender: TObject);
begin
  SVG2Image1.Filename := '..\..\..\..\Img\animated-clock.svg';
end;

procedure TfrmElementUnderMousepointer.SVG2Image1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  SVGObject: ISVGObject;
begin
  SVGObject := SVG2Image1.ObjectAtPt(SVGPoint(X, Y), FALSE);
  if assigned(SVGObject) then
    Caption := SVGObject.LocalName + ' id:' + SVGObject.ID
  else
    Caption := 'No object';
end;

end.
