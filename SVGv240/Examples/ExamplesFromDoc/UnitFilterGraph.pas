unit UnitFilterGraph;

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
  BVE.SVG2Image.VCL, BVE.SVG2Control.VCL;
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

  { TfrmFilterGraph }

  TfrmFilterGraph = class(TForm)
    SVG2Image1: TSVG2Image;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmFilterGraph: TfrmFilterGraph;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

uses
  BVE.SVG2Types;

const
  // Filter borowed from here: http://luxor-xul.sourceforge.net/talk/jug-nov-2002/slides.html
  svg_filter = '<filter id="%s" width="%s" height="%s">%s</filter>';
  svg_filter_gaussian =
    '<feGaussianBlur in="%s" result="%s" stdDeviation="%d"  />';
  svg_filter_offset =
    '<feOffset in="%s" result="%s" dx="%d" dy="%d" />';
  svg_filter_composite =
    '<feComposite in="%s" in2="%s" result="%s" operator="%s" />';
  svg_filter_spec_light =
    '<feSpecularLighting in="%s" result="%s"'
    + ' surfaceScale="%d" specularConstant="%d" specularExponent="%d"'
    + ' kernelUnitLength="1" lighting-color = "%s">%s</feSpecularLighting>';
  svg_filter_light_distant =
    '<feDistantLight azimuth="%d" elevation="%d" />';

  svg_filter_text =
      '<svg>'
      + '%s'
      + '<g font-size="100" font-weight="bold">'
        + '<text x="20" y="320" fill="%s" filter="url(#myFilter)">%s</text>'
      + '</g>'
    + '</svg>';

procedure TfrmFilterGraph.FormCreate(Sender: TObject);
var
  Filter: string;
begin
  SVG2Image1.RenderOptions := [sroFilters, sroClippath];

  Filter :=
    Format(svg_filter, ['myFilter', '120%', '150%',
        Format(svg_filter_gaussian, ['SourceAlpha', 'gauss1', 6])
      + Format(svg_filter_offset, ['gauss1', 'offset', 5, 5])
      + Format(svg_filter_composite, ['SourceGraphic', 'offset', 'comp1', 'over'])
      + Format(svg_filter_gaussian, ['SourceAlpha', 'gauss2', 2])
      + Format(svg_filter_spec_light, ['gauss2', 'spec', -3, 1, 16, 'white',
            Format(svg_filter_light_distant, [45, 45])
            ])
      + Format(svg_filter_composite, ['spec', 'SourceGraphic', 'comp2', 'in'])
      + Format(svg_filter_composite, ['comp2', 'comp1', 'comp3', 'over'])
    ]);

  SVG2Image1.SVG.Text := Format(svg_filter_text, [filter, 'red', 'SVG rocks!']);
end;

end.
