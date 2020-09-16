unit UnitAddRemoveElements;

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
  BVE.SVG2Image.VCL,
  BVE.SVG2Control.VCL;
{$ELSE}
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  BVE.SVG2Image.FPC,
  BVE.SVG2Control.FPC;
{$ENDIF}

type
  { TfrmAddRemoveElements }

  TfrmAddRemoveElements = class(TForm)
    SVG2Image1: TSVG2Image;
    procedure FormCreate(Sender: TObject);
    procedure SVG2Image1Click(Sender: TObject);
  private
    FStep: integer;
  end;

var
  frmAddRemoveElements: TfrmAddRemoveElements;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

uses
{$IFnDEF FPC}
  XML.XMLIntf,
{$ENDIF}
  BVE.SVG2Types,
  BVE.SVG2Elements,
  BVE.SVG2Intf;

procedure TfrmAddRemoveElements.FormCreate(Sender: TObject);
begin
  FStep := 0;
end;

procedure TfrmAddRemoveElements.SVG2Image1Click(Sender: TObject);
var
  Node: IXMLNode;
begin
  case FSTep of
  0: begin
       Node := SVGCreateElement(el_svg);
       SVG2Image1.SVGRoot.SVGList.Add(Node);

       Node.Attributes['width'] := '10cm';
       Node.Attributes['height'] := '8cm';
       Node.Attributes['viewBox'] := '0 0 100 80';

       Caption := 'SVG element created';
       Inc(FStep);
     end;

  1: begin
       Node := SVGCreateElement(el_rect);
       SVG2Image1.SVGRoot.SVG.ChildNodes.Add(Node);

       Node.Attributes['id'] := 'rect';
       Node.Attributes['x'] := 0;
       Node.Attributes['y'] := 0;
       Node.Attributes['width'] := 100;
       Node.Attributes['height'] := 80;
       Node.Attributes['style'] := 'fill: yellow; stroke: blue;';

       Caption := 'Rect element created';
       Inc(FStep);
     end;

  2: begin
       Node := SVGCreateElement(el_ellipse);
       SVG2Image1.SVGRoot.SVG.ChildNodes.Add(Node);

       Node.Attributes['cx'] := 50;
       Node.Attributes['cy'] := 40;
       Node.Attributes['rx'] := 50;
       Node.Attributes['ry'] := 40;
       Node.Attributes['style'] := 'fill: none; stroke: red;';

       Caption := 'Ellipse element created';
       Inc(FStep);
     end;

  3: begin
       Node := SVGCreateElement(el_text);
       SVG2Image1.SVGRoot.SVG.ChildNodes.Add(Node);

       Node.Attributes['x'] := 50;
       Node.Attributes['y'] := 45;
       Node.Attributes['text-anchor'] := 'middle';
       Node.Attributes['style'] := 'font-family: "times"; font-size: 24; fill: red;';

       Node.Text := 'Ready...';

       Caption := 'Text element created';
       Inc(FStep);
     end;

  4: begin
       Node := SVG2Image1.SVGRoot.Element['rect'];
       if assigned(Node) then
         SVG2Image1.SVGRoot.SVG.ChildNodes.Remove(Node);

       Caption := 'Rect element removed';
       Inc(FStep);
     end;

  5: begin
       SVG2Image1.SVGRoot.SVG.ChildNodes.Clear;

       Caption := 'SVG child list cleared';
       FStep := 1;
     end;
  end;

  SVG2Image1.Repaint;
end;

end.
