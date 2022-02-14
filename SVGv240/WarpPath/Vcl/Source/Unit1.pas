unit Unit1;

// -----------------------------------------------------------------------------
//
//  Path transformation demo
//
//  B.J.H. Verhue 13-2-2022
//
// -----------------------------------------------------------------------------

// The SVG control package is needed to compile this.
// You can also compile with the free demo package, which can be downloaded
// from here: https://www.bverhue.nl/delphisvg

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Xml.XMLDom,
  Xml.XMLIntf,
  BVE.SVG2Types,
  BVE.SVG2Intf,
  BVE.SVG2PathData,
  BVE.SVG2Control.VCL,
  BVE.SVG2Image.VCL;

type
  TForm1 = class(TForm)
    SVG2Image1: TSVG2Image;
    Panel1: TPanel;
    bLoad: TButton;
    bWarp: TButton;
    procedure bLoadClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure bWarpClick(Sender: TObject);
  public const
  private
    FSvgPath: string;
    FSvgFilename: string;
  public
    procedure LoadSVG(const aFilename: string);
    procedure Warp;
  end;

var
  Form1: TForm1;

implementation
uses
  BVE.SVGWarpPath;

{$R *.dfm}

{ TForm1 }

procedure TForm1.bLoadClick(Sender: TObject);
begin
  LoadSVG(FSvgPath + FSvgFilename);
end;

procedure TForm1.bWarpClick(Sender: TObject);
begin
  Warp;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FSvgPath := '..\..\..\..\Svg\';
  FSvgFilename := 'Delphi27.svg';

  // We need to include sroTextToPath, to convert text elements to
  // path elements. This happens on rendering.

  SVG2Image1.RenderOptions := [sroFilters, sroClippath, sroTextToPath];
end;

procedure TForm1.LoadSVG(const aFilename: string);
begin
  // Clear the root

  SVG2Image1.SVGRoot.Clear;
  SVG2Image1.Filename := '';

  // Load the SVG, the SVG will automatically be rendered.
  // Because [sroTextToPath] is set in the renderoptions, any text is
  // converted to paths on rendering.

  SVG2Image1.Filename := aFilename;

  // We need to do another render after the text is converted to paths to
  // let the system recalculate the cache info of the new path elements.
  // The cache info that is needed for warping is the local transformation
  // matrix.

  SVG2Image1.Repaint;
end;

procedure TForm1.Warp;
var
  Doc: IXMLDocument;
  Group: ISVGGroup;
  Bounds: TSVGRect;
begin
  Doc := SVG2Image1.SVGRoot.Doc;

  if not assigned(Doc) then
    raise Exception.Create('Root does not contain an active document.');

  // Now we need to calculate the size of the group that contains the new paths,
  // so we can normalize the points within the group.

  Group := SVG2Image1.SVGRoot.Group['warp'];
  if assigned(Group) then
    Bounds := Group.CalcChildrenSize(SVG2Image1.SVGRoot)
  else
    raise Exception.Create('Cannot find a group with id="warp".');

  // The procedure "WarpPaths" will traverse the DOM and all the paths that are
  // encountered will be warped with the provided warp function

  WarpPaths(Doc.DocumentElement,

    // Warp function

    function (const aPt: TSVGPoint): TSVGPoint
    var
      NP: TSVGPoint;
      Radius: TSVGFloat;
      RadiusTop: TSVGFloat;
      RadiusBottom: TSVGFloat;
      Scale: TSVGFloat;
      Angle: TSVGFloat;
      AngleStart: TSVGFloat;
    begin
      // Normalize the point within the bounds

      NP := Normalize(aPt, Bounds);

      // The top will be an arc

      RadiusTop := 1;

      // The bottom will be an arc with a larger radius

      RadiusBottom := 8;

      // We interpolate between the top and bottom radius using the normalized
      // Y coord (from 0 to 1).

      Radius := RadiusTop + (RadiusBottom - RadiusTop) * NP.Y;
      if Radius = 0 then
      begin
        Result := aPt;
        Exit;
      end;

      // We will also taper the image, top width is 60% of bottom with.

      Scale := 0.6 + (1.0 - 0.6) * NP.Y;

      // Here we calculate the angle and start angle of the arc.

      AngleStart := ArcCos(Clamp(-0.5 / Radius, -1.0, 1.0));
      Angle := ArcCos(Clamp((-0.5 + NP.X) / Radius, -1.0, 1.0));

      // Calculate the warped point

      Result := SVGPoint(
        Radius * (Cos(Angle) - Cos(AngleStart)) * Scale + (1.0 - Scale) / 2,
        NP.Y - Radius * (Sin(Angle) - Sin(AngleStart))
        );

     // De-normalize the point

     Result := SVGPoint(
        Bounds.Left + Bounds.Width * Result.X,
        Bounds.Top  + Bounds.Height * Result.Y
        );
    end
  );

  SVG2Image1.Repaint;

  // Save the warped SVG

  SVG2Image1.SVGRoot.Doc.SaveToFile(FSvgPath + 'Warped_' + FSvgFilename);
end;

end.
