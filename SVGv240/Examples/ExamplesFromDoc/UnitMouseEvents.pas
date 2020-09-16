unit UnitMouseEvents;

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
{$ELSE}
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  BVE.SVG2Image.FPC,
{$ENDIF}
  BVE.SVG2Intf;

type

  { TfrmMouseEvents }

  TfrmMouseEvents = class(TForm)
    SVG2Image1: TSVG2Image;
    procedure SVG2Image1SVGEvent(Sender: TObject; aSVGRoot: ISVGRoot;
      aEvent: ISVGEvent; const aValue: string);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMouseEvents: TfrmMouseEvents;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

uses
  BVE.SVG2Types;

const
  svg_events =
      '<svg width="6cm" height="5cm" viewBox="0 0 600 500"'
     + ' xmlns="http://www.w3.org/2000/svg" version="1.1"'
     + ' xmlns:xlink="http://www.w3.org/1999/xlink">'
     + '<defs>'
       + '<ellipse id="ellipse_large" cx="300" cy="250" rx="300" ry="200" />'
     + '</defs>'
     + '<use id="use_back" onmouseover="MouseOver!" onmouseout="MouseOut!" onmousemove="MouseMove!" fill="red" stroke="black" stroke-width="1" xlink:href="#ellipse_large" />'
     + '<use id="use_left" onmouseover="MouseOver!" onmouseout="MouseOut!" onmousemove="MouseMove!" fill="green" stroke="black" stroke-width="4" transform="translate(-50,250) scale(0.25)" xlink:href="#ellipse_large" />'
     + '<use id="use_right" onmouseover="MouseOver!" onmouseout="MouseOut!" onmousemove="MouseMove!" fill="blue" stroke="black" stroke-width="4" transform="translate(500,250) scale(0.25)" xlink:href="#ellipse_large" />'
   + '</svg>';

procedure TfrmMouseEvents.FormCreate(Sender: TObject);
begin
  SVG2Image1.SVG.Text := svg_events;
end;

procedure TfrmMouseEvents.SVG2Image1SVGEvent(Sender: TObject;
  aSVGRoot: ISVGRoot; aEvent: ISVGEvent; const aValue: string);
var
  Info: string;
begin
  if aEvent.EventType <> etNone then
  begin

    // Only the eventtypes defined in the SVG will be catched!

    Info := aEvent.CurrentTarget.LocalName + ' id: ' + aEvent.CurrentTarget.ID + ' event: ';

    case aEvent.EventType of
      etClick:
        Info := Info + 'click';
      etMouseDown:
        Info := Info + 'mouseDown';
      etMouseUp:
        Info := Info + 'mouseUp';
      etMouseOut:
        Info := Info + 'mouseOut';
      etMouseMove:
        Info := Info + 'mouseMove';
      etMouseOver:
        Info := Info + 'mouseOver';
    end;
  end else
    Info := '';

  Caption := Info;
end;

end.
