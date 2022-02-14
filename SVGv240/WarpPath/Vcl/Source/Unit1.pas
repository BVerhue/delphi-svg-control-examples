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
    bWarp: TButton;
    cbWarpFunction: TComboBox;
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

  LoadSVG(FSvgPath + FSvgFilename);
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
  WarpFunctionID: Integer;
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

  WarpFunctionID := cbWarpFunction.ItemIndex;

  WarpPaths(Doc.DocumentElement,

    // Warp function

    function (const aPt: TSVGPoint): TSVGPoint
    var
      N: TSVGPoint;
      Radius: TSVGFloat;
      RadiusTop: TSVGFloat;
      RadiusBottom: TSVGFloat;
      Scale: TSVGFloat;
      Angle: TSVGFloat;
      AngleStart: TSVGFloat;
    begin
      // Normalize the point within the bounds

      N := Normalize(aPt, Bounds);

      case WarpFunctionID of
        0:
          begin
            // Tapering from left to right

            Scale := 1.0 + (0.3 - 1.0) * N.X;

            Result := SVGPoint(
              N.X,
              N.Y * Scale + (1.0 - Scale) / 2);
          end;

        1:
          begin
            // Perspective

            Scale := 0.5 * (1.5 + N.X);

            if Scale = 0 then
            begin
              Result := aPt;
              Exit;
            end;

            Result := SVGPoint(
              0.5 + (N.X - 0.5) / Scale,
              0.5 + (N.Y - 0.5) / Scale);
          end;

        2:
          begin
            // The top will be an arc

            RadiusTop := 1;

            // The bottom will be an arc with a larger radius

            RadiusBottom := 8;

            // We interpolate between the top and bottom radius using the normalized
            // Y coord (from 0 to 1).

            Radius := RadiusTop + (RadiusBottom - RadiusTop) * N.Y;
            if Radius = 0 then
            begin
              Result := aPt;
              Exit;
            end;

            // We will also taper the image, top width is 60% of bottom with.

            Scale := 0.6 + (1.0 - 0.6) * N.Y;

            // Here we calculate the angle and start angle of the arc.

            AngleStart := ArcCos(Clamp(-0.5 / Radius, -1.0, 1.0));
            Angle := ArcCos(Clamp((-0.5 + N.X) / Radius, -1.0, 1.0));

            // Calculate the warped point

            Result := SVGPoint(
              Radius * (Cos(Angle) - Cos(AngleStart)) * Scale + (1.0 - Scale) / 2,
              N.Y - Radius * (Sin(Angle) - Sin(AngleStart))
              );
          end;
      end;

      // De-normalize the point

      Result := DeNormalize(Result, Bounds);
    end
  );

  SVG2Image1.Repaint;

  // Save the warped SVG

  SVG2Image1.SVGRoot.Doc.SaveToFile(FSvgPath + 'Warped_' + FSvgFilename);
end;

end.
