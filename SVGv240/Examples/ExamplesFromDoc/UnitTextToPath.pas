unit UnitTextToPath;

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
  System.UITypes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  BVE.SVG2Control.VCL;
{$ELSE}
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  BVE.SVG2Control.FPC;
{$ENDIF}

type
  TfrmTextToPath = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    Button2: TButton;
    Panel2: TPanel;
    SVG2Control1: TSVG2Control;
    Panel3: TPanel;
    Splitter1: TSplitter;
    Panel4: TPanel;
    SVG2Control2: TSVG2Control;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    Memo1: TMemo;
    Memo2: TMemo;
    FontDialog1: TFontDialog;
    lFontAttributes: TLabel;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
  private
  public
    procedure CreateInput;
    procedure CreateOutput;
  end;

var
  frmTextToPath: TfrmTextToPath;

implementation
uses
  BVE.SVG2Types;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

const
  svg_text =
      '<svg version="1" xmlns="http://www.w3.org/2000/svg" width="305" height="105">'
        + '<text x="20" y="50" %s>%s</text>'
    + '</svg>';

{ TfrmTextToPath }

procedure TfrmTextToPath.Button1Click(Sender: TObject);
var
  FontStyle, FontWeight: string;
begin
  if FontDialog1.Execute then
  begin
    // Convert Font settings to SVG Font Settings

    if fsBold in FontDialog1.Font.Style then
      FontWeight := 'bold'
    else
      FontWeight := 'normal';

    if fsItalic in FontDialog1.Font.Style then
      FontStyle := 'italic'
    else
      FontStyle := 'normal';

    lFontAttributes.Caption :=
      Format('font-family="%s" font-size="%d" font-style="%s" font-weight="%s"',
      [FontDialog1.Font.Name, FontDialog1.Font.Size, FontStyle, FontWeight]);

    CreateInput;
  end;

end;

procedure TfrmTextToPath.Button2Click(Sender: TObject);
begin
  CreateOutput;
end;

procedure TfrmTextToPath.CreateInput;
begin
  Memo1.Text := Format(svg_text, [lFontAttributes.Caption, Edit1.Text]);

  // By setting [sroTextToPath] in the render options, a group element is
  // created for every text element. For every glyph in the text, a
  // corresponding path element is added to the group.

  // The grouped path elments are not active, they only become active if
  // we copy the root to another root (see CreateOutput).

  SVG2Control1.RenderOptions := [sroTextToPath];

  // Here we assign the the SVG image text to the control, this will trigger
  // a parse and a render.

  SVG2Control1.SVG.Assign(Memo1.Lines);
end;

procedure TfrmTextToPath.CreateOutput;
begin
  // The moment we copy a root that is created with option [sroTextToPath]
  // to another root, the text elements are replaced by the grouped path elements.

  SVG2Control2.SVGRoot := SVG2Control1.SVGRoot.CreateCopy;

  SVG2Control2.Repaint;
  Memo2.Lines.Assign(SVG2Control2.SVGRoot.Doc.XML);
end;

procedure TfrmTextToPath.Edit1Change(Sender: TObject);
begin
  CreateInput;
end;

procedure TfrmTextToPath.FormCreate(Sender: TObject);
begin
  lFontAttributes.Caption := 'font-family="Times New Roman" font-size="20"';
  CreateInput;
end;

end.
