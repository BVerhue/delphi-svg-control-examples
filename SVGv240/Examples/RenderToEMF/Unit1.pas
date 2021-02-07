unit Unit1;

//------------------------------------------------------------------------------
//
//                          SVG Control Package 2.0
//
//                      Example render SVG to EMF file
//
//------------------------------------------------------------------------------


interface
uses
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

type
  TForm1 = class(TForm)
    OpenDialog1: TOpenDialog;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    procedure RenderToEmf(aFilename: string);
  end;

var
  Form1: TForm1;

implementation
uses
  BVE.SVG2Types,
  BVE.SVG2Intf,
  BVE.SVG2SaxParser,
  BVE.SVG2Elements,
  BVE.SVG2Elements.Vcl,
  BVE.SVG2ContextGP;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
 if OpenDialog1.Execute then
   RenderToEmf(OpenDialog1.FileName);
end;

procedure TForm1.RenderToEmf(aFilename: string);
var
  Filename: string;
  SVGParser: TSVGSaxParser;
  SVGRoot: ISVGRoot;
  R: TSVGRect;
  W, H: integer;

  procedure Render;
  var
    RC: ISVGRenderContext;
    RCGP: TSVGContextGP;
  begin
    // Define a size...

    W := 250;
    H := 250;

    // ...or calc the intrinsic size of the SVG (optional)

    RC := TSVGContextGP.Create(W, H);
    R := SVGRoot.CalcIntrinsicSize(RC, SVGRect(0, 0, W, H));

    // Now we create the GDI+ rendercontext for rendering to EMF

    RCGP := TSVGContextGP.Create(Filename, Round(R.Width), Round(R.Height));
    RC := RCGP;

    // Choose how text should be rendered in the EMF file

    // All glyphs will be converted to paths, so there will be no text in the
    // EMF file.

    //RCGP.TextFormattingOptions := [];

    // Every placed character will start a new chunk of text

    //RCGP.TextFormattingOptions := [tfoStrings];

    // If all characters in the text are placed, this is considered one chunk (default)

    RCGP.TextFormattingOptions := [tfoStringsWithPlacedCharacters];

    // Render the EMF file

    RC.BeginScene;
    try
      SVGRenderToRenderContext(
        SVGRoot,
        RC,
        Round(R.Width), Round(R.Height),
        [
          // Choose if you need filters, or clippaths. These will result in
          // parts of the EMF file being replaced with bitmaps.

          //sroFilters,
          //sroClippath
        ]
        );
    finally
      RC.EndScene;
    end;
  end;

begin
  // Render to EMF is GDI+ functionality.
  // - The radial gradient is not very good in GDI+.
  // - Filters, clippaths etc will be rendered to a embedded bitmap

  // Instructions:

  // Compile application with only {$Define SVGGDIP} and {$Define SVGFontGDI}
  // enabled in ..\Vcl\ContextSettingsVCL.inc


  // Create a root to store the SVG rendering tree

  SVGRoot := TSVGRootVCL.Create;

  // Create a SAX parser instance

  SVGParser := TSVGSaxParser.Create(nil);
  try

    Filename := ChangeFileExt(aFilename, '.emf');

    // Parse SVG document and build the rendering tree

    SVGParser.Parse(aFileName, SVGRoot);

    Render;

  finally
    SVGParser.Free;
  end;
end;

end.
