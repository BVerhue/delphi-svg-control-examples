unit Unit1;

interface

{$Include CompilerSettings.inc}

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
  Vcl.ExtCtrls,
  BVE.SVG2Types,
  BVE.SVG2Intf,
  BVE.SVG2Image.VCL,
  BVE.SVG2Control.VCL;

type
  TForm1 = class(TForm)
    SVG2Image1: TSVG2Image;
    Timer1: TTimer;
    procedure Timer1Timer(Sender: TObject);
  private
    FPath: ISVGPath;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

// Check that the DoubleBuffered property on the form is set to TRUE

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if not assigned(FPath) then
    FPath := SVG2Image1.SVGRoot.Path['path2989']
  else begin
    // Elements have three separate lists that hold style attributes:

    // StyleAttributes: style attributes defined directly on an element, lowest priority
    // StyleSheetStyle: style defined in a css style sheet, higher priority
    // InlineStyle: css defined inline on an element, highest priority

    // In this case the dash offset is part of an inline style definition (see
    // SVG content), so we must set the new value in the InlineStyle list of
    // the element.

    FPath.InlineStyle.DashOffset := FPath.InlineStyle.DashOffset.Add(1);
    SVG2Image1.Repaint;
  end;
end;

end.
