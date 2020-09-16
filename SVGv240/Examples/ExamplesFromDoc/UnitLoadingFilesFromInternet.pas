unit UnitLoadingFilesFromInternet;

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
  Vcl.StdCtrls,
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
  StdCtrls,
  BVE.SVG2Image.FPC;
{$ENDIF}

type

  { TfrmLoadingFilesFromInternet }

  TfrmLoadingFilesFromInternet = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    SVG2Image1: TSVG2Image;
    Image1: TImage;
    Button1: TButton;
    Label2: TLabel;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FStep: integer;
    Fsl: TStringList;
    FPath: string;
  end;

var
  frmLoadingFilesFromInternet: TfrmLoadingFilesFromInternet;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

uses
{$IFnDEF FPC}
  PngImage,
{$ELSE}
{$ENDIF}
  BVE.SVG2SaxParser,
  BVE.SVG2Types;

procedure TfrmLoadingFilesFromInternet.Button1Click(Sender: TObject);
var
  p: integer;
  Filename: string;
  MemStream: TMemoryStream;
{$IFnDEF FPC}
  Png: TPngImage;
{$ELSE}
  Png: TPortableNetworkGraphic;
{$ENDIF}
begin
  if FStep >= Fsl.Count then
    FStep := 0;

  if FStep < Fsl.Count then
  begin
    FileName := Fsl[FStep];
    Caption := FileName;

    try
      // Load the SVG file

      SVG2Image1.FileName := FPath + 'svg/' + FileName;

      // Load the corresponding Png file

      MemStream := TMemoryStream.Create;
      try
        p := Pos('.', FileName);
{$IFnDEF FPC}
        Png := TPngImage.Create;
{$ELSE}
        Png := TPortableNetworkGraphic.Create;
{$ENDIF}
        try
          TSVGSaxParser.LoadFromInternet(FPath + 'png/' + copy(FileName, 1, p - 1) + '.png', MemStream);
          MemStream.Position := 0;
          Png.LoadFromStream(MemStream);
          Image1.Picture.Graphic := Png;
        finally
          Png.Free;
        end;

      finally
        MemStream.Free;
      end;

      except on E:Exception do
      begin
        Caption := Caption + ' Error: ' + E.Message;
      end;
    end;
  end;

  FStep := FStep + 1;
end;

procedure TfrmLoadingFilesFromInternet.FormCreate(Sender: TObject);
var
  MemStream: TMemoryStream;
  i: integer;
begin
  SVG2Image1.RenderOptions := [sroClippath, sroFilters];

  FPath := 'https://dev.w3.org/SVG/profiles/1.1F2/test/';

  Fsl := TStringList.Create;
  MemStream := TMemoryStream.Create;
  try
    // Load the list of files in the SVG1.1 test suite

    TSVGSaxParser.LoadFromInternet(FPath + '/svg/basic-files.txt', MemStream);
    TSVGSaxParser.LoadFromInternet(FPath + '/svg/tiny-files.txt', MemStream);
    TSVGSaxParser.LoadFromInternet(FPath + '/svg/full-files.txt', MemStream);

    MemStream.Position := 0;
    Fsl.LoadFromStream(MemStream);

    // Delete the testfiles that are not supprted
    i := 0;
    while i < Fsl.Count do
    begin
      // Note: not all test svg's will render correctly, filter out the
      // svg groups that are not supported in any case.

      if (Pos('animate-', Fsl[i])<>0)
      or (Pos('interact-', Fsl[i])<>0)
      or (Pos('script-', Fsl[i])<>0)
      or (Pos('struct-dom-', Fsl[i])<>0)
      or (Pos('text-dom-', Fsl[i])<>0)
      or (Pos('text-tselect-', Fsl[i])<>0)
      then
        Fsl.Delete(i)
      else
        Inc(i);
    end;


    Fsl.Sort;
  finally
    MemStream.Free;
  end;

  FStep := 0;
  Button1Click(Self);
end;

end.
