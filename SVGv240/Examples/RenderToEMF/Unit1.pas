unit Unit1;

//------------------------------------------------------------------------------
//
//                          SVG Control Package 2.0
//
//                      Example render SVG to EMF file
//
//------------------------------------------------------------------------------

// Requires Delphi SVG Package 2.4 update 25 or later.


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
    procedure RenderToEmfManually(aFilename: string);
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
  BVE.SVG2ExportEMF.Vcl,
  BVE.SVG2ContextGP;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
 if OpenDialog1.Execute then
   RenderToEmf(OpenDialog1.FileName);
end;

procedure TForm1.RenderToEmf(aFilename: string);
begin
  // Rendering to EMF is GDI+ functionality:
  // - The radial gradient is not very good in GDI+.
  // - Filters, clippaths etc will be rendered to an embedded bitmap.

  // Instructions:

  // Enable {$Define SVGGDIP} and {$Define SVGFontGDI} in
  // ..\Vcl\ContextSettingsVCL.inc, IN ADDITION to the render context your
  // application uses.

  // Your application does not have to be a GDI+ application. Since v2.4
  // update 25 the export switches the resource factory of the document to
  // GDI+ for the duration of the render pass and restores it afterwards, so
  // the user interface keeps rendering on Direct2D or whichever backend you
  // selected.

  // Before that, an EMF written by a Direct2D application came out empty: the
  // GDI+ metafile writer silently skips path data and font faces that were
  // created by another backend.

  SVGFileRenderToEmfFile(
    aFilename,
    ChangeFileExt(aFilename, '.emf'),

    // Width and height in pixels. Pass 0 for both to use the intrinsic size of
    // the SVG document.

    0, 0,

    // Choose how text should be rendered in the EMF file:
    //
    //   etmPaths                        all glyphs are converted to paths, so
    //                                   there will be no text in the EMF file
    //   etmStrings                      every placed character starts a new
    //                                   chunk of text
    //   etmStringsWithPlacedCharacters  if all characters in the text are
    //                                   placed, this is one chunk (default)

    etmStringsWithPlacedCharacters,

    // Choose if you need filters or clippaths. These result in parts of the
    // EMF file being replaced with bitmaps.

    [
      //sroFilters,
      //sroClippath
    ]);
end;

procedure TForm1.RenderToEmfManually(aFilename: string);
var
  Filename: string;
  SVGParser: TSVGSaxParser;
  SVGRoot: ISVGRoot;
  SaveFactory: ISVGResourceFactory;
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

    RCGP.TextFormattingOptions := [tfoStringsWithPlacedCharacters];

    // Render the EMF file

    RC.BeginScene;
    try
      SVGRenderToRenderContext(
        SVGRoot,
        RC,
        Round(R.Width), Round(R.Height),
        []);
    finally
      RC.EndScene;
    end;
  end;

begin
  // The same thing without the helper, for when you need control over the
  // render context itself.

  // The resource factory has to be switched to the same backend that writes
  // the metafile. Handing path data from another backend to the GDI+ writer
  // produces an empty EMF, and handing it to a Direct2D context is an access
  // violation.

  SVGRoot := TSVGRootVCL.Create;

  SVGParser := TSVGSaxParser.Create(nil);
  try
    Filename := ChangeFileExt(aFilename, '.emf');

    // Parse SVG document and build the rendering tree

    SVGParser.Parse(aFileName, SVGRoot);

    SaveFactory := SVGRoot.ResourceFactory;

    SVGRoot.ResourceFactory :=
      TSVGRenderContextManager.CreateResourceFactory(rcGDIPlus, fsGDI);
    try
      Render;
    finally
      if SaveFactory = TSVGRenderContextManager.ResourceFactory then
        SVGRoot.ResourceFactory := nil
      else
        SVGRoot.ResourceFactory := SaveFactory;
    end;
  finally
    SVGParser.Free;
  end;
end;

end.
