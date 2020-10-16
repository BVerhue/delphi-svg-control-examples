unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

  // In case of the demo package add the folder containing the dcu files
  // to the search path, for example:
  // C:\Downloads\SVGDemoDX2_DX3_220\DX3\Dcu

type
  TForm1 = class(TForm)
    Button1: TButton;
    Image1: TImage;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  System.UITypes,
  BVE.SVG2SaxParser,
  BVE.SVG2Intf,
  BVE.SVG2Context,
  BVE.SVG2Types,
  BVE.SVG2Elements,
  BVE.SVG2Elements.VCL;

const
  svg_text =
    '<?xml version="1.0" encoding="UTF-8" standalone="no"?>'
  + '<svg xmlns="http://www.w3.org/2000/svg" id="test_svg" version="1.1" width="480" height="320" viewBox="0 215 120 80">'
    + '<g id="layer1">'
      + '<text id="txt" x="55" y="225" font-family="sans-serif" font-size="8">'
        + '<tspan id="tspan1">A star group</tspan>'
        + '<tspan id="tspan2" x="55" dy="1em">an ellipse</tspan>'
        + '<tspan id="tspan3" x="55" dy="1em">and some text</tspan>'
      + '</text>'
      + '<ellipse id="ellipse" transform="rotate(25)" cx="200" cy="215" rx="25" ry="7.5" fill="#00ff00" stroke="#000000" stroke-width="0.75" />'
      + '<g id="star_group">'
       + ' <path id="star1" fill="#ff2a2a" stroke="#000000" stroke-width="0.75"'
          + ' d="m 41.010416,262.03718 -4.104352,-0.60791 -3.016279,2.84909 -0.690161,-4.09132 -3.641728,-1.98824 3.677809,-1.92067 0.765567,-4.07789 2.963173,2.90429 4.114874,-0.53204 -1.846468,3.71562 z" />'
        + '<path id="star2" fill="#ffcc00" stroke="#000000" stroke-width="0.75"'
          + ' d="m 25.31441,257.03553 -3.967414,-6.11754 -7.330735,-0.71095 4.676652,-5.60961 -1.579348,-7.09179 6.857743,2.6506 6.354646,-3.67203 -0.438334,7.24778 5.506734,4.82236 -7.128647,1.82876 z" />'
      + '</g>'
    + '</g>'
  + '</svg>';

procedure TForm1.Button1Click(Sender: TObject);
var
  SVGRoot: ISVGRoot;
  sl: TStringList;
  SVGParser: TSVGSaxParser;
  Bitmap: TBitmap;
  RC: ISVGRenderContext;
  ParentBounds, Bounds: TSVGRect;

  procedure DrawBounds(const aID: string; const aColor: TSVGColor);
  var
    SVGObject: ISVGObject;
    i: Integer;
  begin
    if Supports(SVGRoot.FindElement(aID), ISVGObject, SVGObject)
    and (SVGObject.CacheList.Count > 0) then
    begin
      for i := 0 to SVGObject.CacheList.Count - 1 do
      begin
        Bounds := SVGObject.CacheList[i].ScreenBBox;
        Bounds.Inflate(5, 5);

        // Create a brush for stroking the outline

        RC.ApplyStroke(TSVGSolidBrush.Create(aColor), 2.0);

        // Draw a rectangle around the element

        RC.DrawRect(Bounds, 5, 5);
      end;
    end;

  end;

begin
  // Step 1
  // Create a VCL SVG Root object

  SVGRoot := TSVGRootVCL.Create;

  // Put the SVG text in a stringlist and parse

  sl := TStringList.Create;
  SVGParser := TSVGSaxParser.Create(nil);
  try
    sl.Text := svg_text;

    SVGParser.Parse(sl, SVGRoot);

  finally
    SVGParser.Free;
    sl.Free;
  end;

  // Step 2
  // Create a bitmap, we don't know how big it needs to be, so initially
  // we make it the minimum size: 1 by 1

  Bitmap := TBitmap.Create;
  try
    // Define some parent bounds, in case the SVG graphic has dimensions in percentages

    ParentBounds := SVGRect(0, 0, Image1.Width, Image1.Height);

    // Now we can calculate the dimensions of the SVG graphic

    Bounds := SVGRoot.CalcIntrinsicSize(ParentBounds);

    // Resize the bitmap and make it 32bit transparent

    Bitmap.SetSize(Round(Bounds.Width), Round(Bounds.Height));
    Bitmap.PixelFormat := TPixelFormat.pf32bit;
    Bitmap.AlphaFormat := TAlphaFormat.afPremultiplied;

    // Recreate the render context with the newly sized Bitmap!!

    RC := TSVGRenderContextManager.CreateRenderContextBitmap(Bitmap);

    // Step 3
    // Draw the SVG on the bitmap

    RC.BeginScene;
    try
      RC.Clear(0); // Clear the bitmap with transparent color
      RC.Matrix := TSVGMatrix.CreateIdentity;

      SVGRenderToRenderContext(
        SVGRoot,
        RC,
        Bounds.Width* 2,
        Bounds.Height,
        [sroEvents],   // Force the creation of an object state tree
        FALSE          // No auto scaling in this case
        );

      // Find the text element on ID and return the screen bounding box
      DrawBounds('txt', SVGColorRed);

      // Find the star group element on ID and return the screen bounding box
      DrawBounds('star_group', SVGColorBlue);

      // Find the ellipse element on ID and return the screen bounding box
      DrawBounds('ellipse', SVGColorGreen);

    finally
      RC.EndScene;
    end;

    // Step 5
    // Assign to Image1

    Image1.Picture.Assign(Bitmap);
  finally
    Bitmap.Free;
  end;
end;

end.
