unit UnitFilenameAssignment;

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
  TfrmFilenameAssignment = class(TForm)
    SVG2Image1: TSVG2Image;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmFilenameAssignment: TfrmFilenameAssignment;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TfrmFilenameAssignment.FormCreate(Sender: TObject);
begin
  SVG2Image1.Filename := '..\..\..\..\Img\Butterfly.svg';
end;

end.
