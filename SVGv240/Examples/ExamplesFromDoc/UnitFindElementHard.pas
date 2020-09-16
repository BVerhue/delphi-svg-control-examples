unit UnitFindElementHard;

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
  Vcl.StdCtrls,
  XML.XMLIntf,
  BVE.SVG2Intf,
  BVE.SVG2Image.VCL;
{$ELSE}
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  BVE.SVG2Intf,
  BVE.SVG2Types,
  BVE.SVG2Image.FPC;
{$ENDIF}

type

  { TfrmFindElementHard }

  TfrmFindElementHard = class(TForm)
    SVG2Image1: TSVG2Image;
    Memo1: TMemo;
    procedure SVG2Image1AfterParse(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure DoNode(aLevel: integer; aNode: IXMLNode);
  public
    { Public declarations }
  end;

var
  frmFindElementHard: TfrmFindElementHard;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

const
  svg_text =
     '<svg viewBox="0 0 200 80">'
     + '<g font-family="courier new" text-anchor="middle">'
       + '<text id="text1" x="100" y="30" font-size="16" fill="blue" >Blue</text>'
       + '<text id="text2" x="100" y="60" font-size="24" fill="red" >Red</text>'
     + '</g>'
     + '<rect x="1" y="1" width="198" height="78" fill="none" stroke="blue" stroke-width="2" />'
   + '</svg>';

type
  TOnNodeEvent = procedure(aLevel: integer; aNode: IXMLNode) of object;

procedure Traverse(aNode: IXMLNode; aLevel: integer; aProcNode: TOnNodeEvent);
var
  i: integer;
begin
  aProcNode(aLevel, aNode);
  for i := 0 to aNode.ChildNodes.Count - 1 do
    Traverse(aNode.ChildNodes[i], aLevel + 1, aProcNode);
end;


procedure TfrmFindElementHard.DoNode(aLevel: integer; aNode: IXMLNode);
var
  Info: string;
begin
  Info := '<' +  aNode.LocalName + '>';
  if aNode.HasAttribute('id') then
    Info := Info + ' id: ' + aNode.Attributes['id'];

  Memo1.Lines.Add(StringOfChar(' ', aLevel*4) + Info);
end;

procedure TfrmFindElementHard.FormCreate(Sender: TObject);
begin
  SVG2Image1.SVG.Text := svg_text;
end;

procedure TfrmFindElementHard.SVG2Image1AfterParse(Sender: TObject);
begin
  Memo1.Clear;
  Traverse(SVG2Image1.SVGRoot.SVG, 0, DoNode);
end;

end.
