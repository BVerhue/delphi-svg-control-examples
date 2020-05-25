unit UnitTSVG2GraphicAndTImage;

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
  Vcl.ExtCtrls;
{$ELSE}
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls;
{$ENDIF}

type

  { TfrmTSVG2GraphicAndTImage }

  TfrmTSVG2GraphicAndTImage = class(TForm)
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmTSVG2GraphicAndTImage: TfrmTSVG2GraphicAndTImage;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

uses
{$IFnDEF FPC}
  BVE.SVG2Graphic.VCL;
{$ELSE}
  BVE.SVG2Graphic.FPC;
{$ENDIF}

procedure TfrmTSVG2GraphicAndTImage.FormCreate(Sender: TObject);
var
  Graphic: TSVG2Graphic;
begin
  Graphic := TSVG2Graphic.Create;
  try
    Graphic.LoadFromFile('..\..\..\..\Img\Butterfly.svg');
    Image1.Picture.Assign(Graphic);
  finally
    Graphic.Free;
  end;
end;

end.
