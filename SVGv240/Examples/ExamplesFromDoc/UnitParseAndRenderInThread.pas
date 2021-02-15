unit UnitParseAndRenderInThread;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  BVE.SVG2Intf;

type
  TfrmParseAndRenderInThread = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Memo1: TMemo;
    Splitter1: TSplitter;
    ScrollBox1: TScrollBox;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ScrollBox1Resize(Sender: TObject);
  private
    FFiles: TStringList;
    FImageWidth: Integer;
    FImageHeight: Integer;
    FPath: string;
    FCurIndex: Integer;

    procedure SetPath(const Value: string);
  protected
    procedure CalcBounds(aImage: TImage);

    procedure ParserThreadTerminate(Sender: TObject);
    procedure RendererThreadTerminate(Sender: TObject);

    procedure ParseNext;

    procedure RealignImages;
  public
    procedure ReadDirectory;

    property Path: string read FPath write SetPath;
  end;

  TParserThread = class(TThread)
  private
    FFilename: string;
    FRoot: ISVGRoot;
    FResult: string;
  protected
    procedure Execute; override;
  public
    constructor Create(const aFilename: string);
    destructor Destroy; override;

    property Filename: string read FFilename;
    property Root: ISVGRoot read FRoot;
    property Result: string read FResult;
  end;

  TRenderThread = class(TThread)
  private
    FFilename: string;
    FRoot: ISVGRoot;
    FContext: ISVGRenderContext;
    FBitmap: TBitmap;
    FResult: string;
  protected
    procedure Execute; override;
  public
    constructor Create(const aFilename: string; aRoot: ISVGRoot;
      aBitmap: TBitmap);
    destructor Destroy; override;

    property Filename: string read FFilename;
    property Bitmap: TBitmap read FBitmap;
    property Result: string read FResult;
  end;

var
  frmParseAndRenderInThread: TfrmParseAndRenderInThread;

implementation
uses
  BVE.SVG2Types,
  BVE.SVG2SaxParser,
  BVE.SVG2Elements,
  BVE.SVG2Elements.VCL;

{$R *.dfm}

const
  RESULT_OK = 'Ok';

{ TParserThread }

constructor TParserThread.Create(const aFilename: string);
begin
  FreeOnTerminate := True;

  FFilename := aFilename;

  inherited Create(True);
end;

destructor TParserThread.Destroy;
begin
  inherited;
end;

procedure TParserThread.Execute;
var
  Parser: TSVGSaxParser;
begin
  try
    FRoot := TSVGRootVCL.Create;

    Parser := TSVGSaxParser.Create(nil);
    try
      Parser.Parse(FFilename, FRoot);
    finally
      Parser.Free;
    end;

    FResult := RESULT_OK;

    except on E:Exception do
    begin
      FResult := E.Message;
    end;
  end;
end;

{ TRenderThread }

constructor TRenderThread.Create(const aFilename: string; aRoot: ISVGRoot;
  aBitmap: TBitmap);
begin
  FreeOnTerminate := True;

  FFilename := aFilename;
  FRoot := aRoot;
  FBitmap := aBitmap;

  FContext := TSVGRenderContextManager.CreateRenderContextBitmap(FBitmap);

  inherited Create(True);
end;

destructor TRenderThread.Destroy;
begin
  inherited;
end;

procedure TRenderThread.Execute;
begin
  try
    FContext.BeginScene;
    try
      SVGRenderToRenderContext(
        FRoot,
        FContext,
        FBitmap.Width,
        FBitmap.Height);
    finally
      FContext.EndScene;
    end;

    FResult := RESULT_OK;

    except on E:Exception do
    begin
      FResult := E.Message;
    end;
  end;
end;

{ TForm2 }

procedure TfrmParseAndRenderInThread.Button1Click(Sender: TObject);
begin
  //FPath := '\\langroup\users\Userdata\bverhue1\Mijn documenten\Mijn afbeeldingen\SVG\flat-color-icons-master\flat-color-icons-master\svg\';
  //Path := '\\langroup\users\Userdata\bverhue1\Mijn documenten\Mijn afbeeldingen\SVG\Google\';
  Path := '\\langroup\users\Userdata\bverhue1\Mijn documenten\Mijn afbeeldingen\SVG';
  ReadDirectory;
end;

procedure TfrmParseAndRenderInThread.CalcBounds(aImage: TImage);
var
  W: Integer;
begin
  W := Scrollbox1.Width div FImageWidth;

  aImage.Left := (aImage.Tag mod W) * FImageWidth - Scrollbox1.HorzScrollBar.Position;
  aImage.Top := (aImage.Tag div W) * FImageHeight - Scrollbox1.VertScrollBar.Position;
end;

procedure TfrmParseAndRenderInThread.FormCreate(Sender: TObject);
begin
  FFiles := TStringList.Create;

  FImageWidth := 64;
  FImageHeight := 64;

  TSVGRenderContextManager.InitializeGraphics;
end;

procedure TfrmParseAndRenderInThread.FormDestroy(Sender: TObject);
begin
  TSVGRenderContextManager.FinalizeGraphics;

  FFiles.Free;
end;

procedure TfrmParseAndRenderInThread.ParseNext;
var
  ParserThread: TParserThread;
begin
  if FCurIndex < FFiles.Count then
  begin
    ParserThread := TParserThread.Create(FPath + FFiles[FCurIndex]);
    ParserThread.OnTerminate := ParserThreadTerminate;
    ParserThread.Start;
  end;
end;

procedure TfrmParseAndRenderInThread.ParserThreadTerminate(Sender: TObject);
var
  ParserThread: TParserThread;
  RenderThread: TRenderThread;
  Filename: string;
  Bitmap: TBitmap;
begin
  // This function is executed in the main thread (executed from OnTerminate)

  if Sender is TParserThread then
    ParserThread := Sender as TParserThread
  else
    Exit;

  Filename := ExtractFilename(ParserThread.FFilename);

  if ParserThread.Result = RESULT_OK then
  begin
    Memo1.Lines.Add('Parsing: ' + Filename + ' ok, rendering...');

    Bitmap := TSVGRenderContextManager.CreateCompatibleBitmap(FImageWidth, FImageHeight, True);

    RenderThread := TRenderThread.Create(ParserThread.Filename, ParserThread.Root, Bitmap);
    RenderThread.OnTerminate := RendererThreadTerminate;
    RenderThread.Start;
  end else
    Memo1.Lines.Add('Parsing: ' + Filename + ' Error: ' + ParserThread.Result);

  Inc(FCurIndex);
  ParseNext;
end;

procedure TfrmParseAndRenderInThread.ReadDirectory;
var
  sr: TSearchRec;
begin
  FFiles.Clear;

  if FindFirst(FPath + '*.svg', faNormal, sr) = 0 then
    try
      repeat
        FFiles.Add(sr.Name);
      until FindNext(sr) <> 0;
    finally
      FindClose(sr);
    end;

  FCurIndex := 0;
  ParseNext;
end;

procedure TfrmParseAndRenderInThread.RealignImages;
var
  i: Integer;
  Image: TImage;
begin
  for i := 0 to Scrollbox1.ControlCount - 1 do
  begin
    if Scrollbox1.Controls[i] is TImage then
    begin
      Image := Scrollbox1.Controls[i] as TImage;
      CalcBounds(Image);
    end;
  end;
end;

procedure TfrmParseAndRenderInThread.RendererThreadTerminate(Sender: TObject);
var
  RenderThread: TRenderThread;
  Filename: string;
  Bitmap: TBitmap;
  Image: TImage;
  Index: Integer;
begin
  // This function is executed in the main thread (executed from OnTerminate)

  if Sender is TRenderThread then
    RenderThread := Sender as TRenderThread
  else
    Exit;

  Filename := ExtractFilename(RenderThread.Filename);
  Bitmap := RenderThread.Bitmap;
  try

    if RenderThread.Result = RESULT_OK then
    begin
      Memo1.Lines.Add('Rendering: ' + Filename + ' ok.');

      Image := TImage.Create(Self);
      Image.Parent := Scrollbox1;
      Image.Width := Bitmap.Width;
      Image.Height := Bitmap.Height;
      Image.Picture.Assign(Bitmap);

      Index := FFiles.IndexOf(Filename);
      Image.Tag := Index;

      CalcBounds(Image);

      Scrollbox1.Invalidate;

    end else
      Memo1.Lines.Add('Rendering: ' + Filename + ' Error: ' + RenderThread.Result);

  finally
    Bitmap.Free;
  end;
end;

procedure TfrmParseAndRenderInThread.ScrollBox1Resize(Sender: TObject);
begin
  RealignImages;
end;

procedure TfrmParseAndRenderInThread.SetPath(const Value: string);
begin
  FPath := Value;
  if FPath[Length(FPath) - 1] <> PathDelim then
    FPath := FPath + PathDelim;
end;

end.
