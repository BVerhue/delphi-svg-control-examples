unit UnitAddRemoveElements;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I 'CompilerSettings.inc'}

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
{$ELSE}
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
{$ENDIF}
{$IFDEF SVGLibVersion2400Up}
  BVE.SVG2Doc,
{$ENDIF}
  BVE.SVG2Image.VCL,
  BVE.SVG2Control.VCL;

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
  {$IFDEF SVGLibVersion2400Up}
  Doc: ISVGXMLDocument;
  {$ENDIF}
begin
  case FSTep of
  0: begin
       {$IFDEF SVGLibVersion2400Up}
       SVG2Image1.SVGRoot.Clear;

       Doc := SVG2Image1.SVGRoot.CreateSVGXMLDocument;
       SVG2Image1.SVGRoot.DocAdd(Doc);

       Node := Doc.DocumentElement;
       {$ELSE}
       Node := SVGCreateElement(el_svg);
       SVG2Image1.SVGRoot.SVGList.Add(Node);
       {$ENDIF}

       Node.Attributes['width'] := '10cm';
       Node.Attributes['height'] := '8cm';
       Node.Attributes['viewBox'] := '0 0 100 80';

       Caption := 'SVG element created';
       Inc(FStep);
     end;

  1: begin
       {$IFDEF SVGLibVersion2400Up}
       Node := SVG2Image1.SVGRoot.SVG.AddChild(el_rect, ns_uri_svg);
       {$ELSE}
       Node := SVGCreateElement(el_rect);
       SVG2Image1.SVGRoot.SVG.ChildNodes.Add(Node);
       {$ENDIF}

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
       {$IFDEF SVGLibVersion2400Up}
       Node := SVG2Image1.SVGRoot.SVG.AddChild(el_ellipse, ns_uri_svg);
       {$ELSE}
       Node := SVGCreateElement(el_ellipse);
       SVG2Image1.SVGRoot.SVG.ChildNodes.Add(Node);
       {$ENDIF}

       Node.Attributes['cx'] := 50;
       Node.Attributes['cy'] := 40;
       Node.Attributes['rx'] := 50;
       Node.Attributes['ry'] := 40;
       Node.Attributes['style'] := 'fill: none; stroke: red;';

       Caption := 'Ellipse element created';
       Inc(FStep);
     end;

  3: begin
       {$IFDEF SVGLibVersion2400Up}
       Node := SVG2Image1.SVGRoot.SVG.AddChild(el_text, ns_uri_svg);
       {$ELSE}
       Node := SVGCreateElement(el_text);
       SVG2Image1.SVGRoot.SVG.ChildNodes.Add(Node);
       {$ENDIF}

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
