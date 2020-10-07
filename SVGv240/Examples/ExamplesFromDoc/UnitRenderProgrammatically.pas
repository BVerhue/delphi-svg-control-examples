unit UnitRenderProgrammatically;

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
  Vcl.StdCtrls;
{$ELSE}
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  StdCtrls,
  Dialogs;
{$ENDIF}

type
  TfrmRenderProgrammatically = class(TForm)
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmRenderProgrammatically: TfrmRenderProgrammatically;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

uses
{$IFnDEF FPC}
  BVE.SVG2Elements.VCL,
{$ELSE}
  BVE.SVG2Elements.FPC,
{$ENDIF}
  BVE.SVG2Intf,
  BVE.SVG2Types,
  BVE.SVG2SaxParser;

procedure TfrmRenderProgrammatically.Button1Click(Sender: TObject);
{$IFnDEF FPC}
var
  FileName: string;
  SVGParser: TSVGSaxParser;
  SVGRoot: ISVGRoot;
  Bitmap: TBitmap;
  R: TSVGRect;
begin
  // Code for Delphi VCL

  if OpenDialog1.Execute then
  begin
    FileName := OpenDialog1.FileName;

    // Create a root to store the SVG rendering tree

    SVGRoot := TSVGRootVcl.Create;

    // Create a SAX parser instance

    SVGParser := TSVGSaxParser.Create(nil);
    try

      // Parse SVG document and build the rendering tree

      SVGParser.Parse(FileName, SVGRoot);

      // Create a bitmap

      Bitmap := TBitmap.Create;
      try
        Bitmap.PixelFormat := TPixelFormat.pf32bit;   // 32bit bitmap
        Bitmap.AlphaFormat := TAlphaFormat.afDefined; // Enable alpha channel

        R := SVGRoot.CalcIntrinsicSize(SVGRect(0, 0, Width, Height)); // Calc size of SVG Graphic passing the height of the container (Form)

        Bitmap.SetSize(Round(R.Width), Round(R.Height)); // Set the bitmap size

        Bitmap.Canvas.Brush.Color := clNone; // Fill background with transparent
        Bitmap.Canvas.FillRect(Rect(0, 0, Bitmap.Width, Bitmap.Height));

        // Render the SVG onto the bitmap

        SVGRenderToBitmap(
          SVGRoot, // The root containing the rendering tree
          Bitmap   // The destination bitmap
          );

        // Do something with the bitmap...
        Canvas.Draw(0, 0, Bitmap);

      finally
        Bitmap.Free;
      end;

    finally
      SVGParser.Free;
    end;
  end;
end;
{$ELSE}
var
  FileName: string;
  SVGParser: TSVGSaxParser;
  SVGRoot: ISVGRoot;
  IntfBitmap: ISVGIntfBitmap;
  Bitmap: TBitmap;
begin
  // Code for FPC Lazarus

  if OpenDialog1.Execute then
  begin
    FileName := OpenDialog1.FileName;

    // Create a root to store the SVG rendering tree

    SVGRoot := TSVGRootFpc.Create;

    // Create a SAX parser instance

    SVGParser := TSVGSaxParser.Create(nil);
    try

      // Parse SVG document and build the rendering tree

      SVGParser.Parse(FileName, SVGRoot);

      // Create an interface bitmap.
      // Because of differences in widget set implementations we cannot
      // render to a bitmap directly, but must use an interface with the
      // appropriate implementation.

      IntfBitmap := SVGCreateIntfBitmap(480, 320);

      // Render the SVG onto the interface bitmap

      SVGRenderToBitmap(
        SVGRoot,    // The root containing the rendering tree
        IntfBitmap  // The destination bitmap
        );

      // Create a bitmap from the interface bitmap
      Bitmap := IntfBitmap.CreateBitmap;
      try

        // Do something with the bitmap...
        Canvas.Draw(30, 30, Bitmap);

      finally
        Bitmap.Free;
      end;

    finally
      SVGParser.Free;
    end;
  end;
end;
{$ENDIF}

end.
