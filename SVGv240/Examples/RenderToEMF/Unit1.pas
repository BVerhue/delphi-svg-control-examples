unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

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
  begin
    // Define a size

    W := 250;
    H := 250;

    // or calc the inttrinsic size of the SVG (optional)

    RC := TSVGContextGP.Create(W, H);
    R := SVGRoot.CalcIntrinsicSize(RC, SVGRect(0, 0, W, H));

    RC := TSVGContextGP.Create(Filename, Round(R.Width), Round(R.Height));
    RC.BeginScene;
    try
      SVGRenderToRenderContext(
        SVGRoot,
        RC,
        Round(R.Width), Round(R.Height));
    finally
      RC.EndScene;
    end;
  end;

begin

  // Instructions:
  //
  // Compile application with {$Define SVGGDIP} enabled in ..\Vcl\ContextSettingsVCL.inc
  //
  // Render to EMF is GDI+ functionality.
  // - The radial gradient is not very good in GDI+.
  // - Filters, clippaths etc will be rendered to a embedded bitmap

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
