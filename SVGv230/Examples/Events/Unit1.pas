unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, BVE.SVG2Image.VCL,
  BVE.SVG2Intf, BVE.SVG2Types;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    SVG2Image1: TSVG2Image;
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    procedure Button2Click(Sender: TObject);
    procedure SVG2Image1SVGEvent(Sender: TObject; aSVGRoot: ISVGRoot;
      aEvent: ISVGEvent; const aValue: string);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation
uses
  Vcl.Clipbrd;

{$R *.dfm}

procedure TForm1.Button2Click(Sender: TObject);
begin
  SVG2Image1.SVG.Text := Clipboard.AsText;
  SVG2Image1.Repaint;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  SVG2Image1.SVG.Text :=
    '<svg width="6cm" height="5cm" viewBox="0 0 600 500"'
  + ' xmlns="http://www.w3.org/2000/svg" version="1.1">'
  + ' <rect id="green_rect" onclick="dummy" fill="green" stroke="black" x="100" y="100" width="100" height="100" />'
  + '</svg>';
  SVG2Image1.Repaint;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  SVG2Image1.SVG.Text :=
    '<svg width="6cm" height="5cm" viewBox="0 0 600 500"'
  + ' xmlns="http://www.w3.org/2000/svg" version="1.1"'
  + ' xmlns:xlink="http://www.w3.org/1999/xlink">'
  + ' <defs>'
  + ' <rect id="rect" width="100" height="100" />'
  + ' </defs>'
  + ' <use id="use1" onclick="green!" fill="green" stroke="black" x="100" y="100" xlink:href="url(#rect)" />'
  + ' <use id="use2" onclick="blue!" fill="blue" stroke="black" x="300" y="100" xlink:href="url(#rect)" />'
  + '</svg>';
  SVG2Image1.Repaint;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  SVG2Image1.SVG.Text :=
    '<svg width="6cm" height="5cm" viewBox="0 0 600 500"'
  + ' xmlns="http://www.w3.org/2000/svg" version="1.1"'
  + ' xmlns:xlink="http://www.w3.org/1999/xlink">'
  + ' <defs>'
  + ' <ellipse id="ellipse_large" cx="300" cy="250" rx="300" ry="200" />'
  + ' </defs>'

  + ' <use id="use_back" onmouseover="MouseOver!" onmouseout="MouseOut!" onmousemove="MouseMove!" fill="red" stroke="black" stroke-width="1" xlink:href="url(#ellipse_large)" />'
  + ' <use id="use_left" onmouseover="MouseOver!" onmouseout="MouseOut!" onmousemove="MouseMove!" fill="green" stroke="black" stroke-width="4" transform="translate(-50,250) scale(0.25)" xlink:href="url(#ellipse_large)" />'
  + ' <use id="use_right" onmouseover="MouseOver!" onmouseout="MouseOut!" onmousemove="MouseMove!" fill="blue" stroke="black" stroke-width="4" transform="translate(500,250) scale(0.25)" xlink:href="url(#ellipse_large)" />'

  + '</svg>';
  SVG2Image1.Repaint;
end;

procedure TForm1.SVG2Image1SVGEvent(Sender: TObject; aSVGRoot: ISVGRoot;
  aEvent: ISVGEvent; const aValue: string);
var
  Line: string;
  MouseEvent: ISVGMouseEvent;
begin
  Memo1.Lines.Clear;

  Line := 'Event properties__________________';
  Memo1.Lines.Add(Line);
  Memo1.Lines.Add('');

  Line  := 'Event type: ';
  case aEvent.EventType of
    etClick:     Line := Line + 'Click';
    etMouseDown: Line := Line + 'MouseDown';
    etMouseUp:   Line := Line + 'MouseUp';
    etMouseOut:  Line := Line + 'MouseOut';
    etMouseMove: Line := Line + 'MouseMove';
    etMouseOver: Line := Line + 'MouseOver';
    else
      Line := Line + 'Unknown';
  end;
  Memo1.Lines.Add(Line);

  Line := 'Target: ';
  if assigned(aEvent.Target) then
    Line := Line + aEvent.Target.ID
  else
    Line := Line + 'None';
  Memo1.Lines.Add(Line);

  Line := 'CurrentTarget: ';
  if assigned(aEvent.CurrentTarget) then
    Line := Line + aEvent.CurrentTarget.ID
  else
    Line := Line + 'None';
  Memo1.Lines.Add(Line);

  Line := 'Event phase: ';
  case aEvent.EventPhase of
    epCapturing: Line := Line + 'Capturing';
    epAtTarget:  Line := Line + 'AtTarget';
    epBubbling:  Line := Line + 'Bubbling';
  end;
  Memo1.Lines.Add(Line);

  Line := Format('Bubbles: %s', [BoolToStr(aEvent.Bubbles)]);
  Memo1.Lines.Add(Line);

  Line := Format('Cancelable: %s', [BoolToStr(aEvent.Cancelable)]);
  Memo1.Lines.Add(Line);

  Line := 'Timestamp:' + IntToStr(aEvent.TimeStamp);
  Memo1.Lines.Add(Line);

  Memo1.Lines.Add('');

  if Supports(aEvent, ISVGMouseEvent, MouseEvent) then
  begin
    Line := 'Mouse event properties____________';
    Memo1.Lines.Add(Line);
    Memo1.Lines.Add('');

    Line := Format('ClientX %.1f ClientY %.1f', [MouseEvent.ClientX, MouseEvent.ClientY]);
    Memo1.Lines.Add(Line);

    Line := Format('ScreenX %.1f ScreenY %.1f', [MouseEvent.ScreenX, MouseEvent.ScreenY]);
    Memo1.Lines.Add(Line);

    Line := 'RelatedTarget: ';
    if assigned(MouseEvent.RelatedTarget) then
      Line := Line + MouseEvent.RelatedTarget.ID
    else
      Line := Line + 'None';
    Memo1.Lines.Add(Line);

    Line := Format('Button: %d', [MouseEvent.Button]);
    Memo1.Lines.Add(Line);

    Line := Format('Detail: %d', [MouseEvent.Detail]);
    Memo1.Lines.Add(Line);

    Line := Format('AltKey: %s', [BoolToStr(MouseEvent.AltKey)]);
    Memo1.Lines.Add(Line);

    Line := Format('CtrlKey: %s', [BoolToStr(MouseEvent.CtrlKey)]);
    Memo1.Lines.Add(Line);

    Line := Format('ShiftKey: %s', [BoolToStr(MouseEvent.ShiftKey)]);
    Memo1.Lines.Add(Line);
  end;
  Memo1.Lines.Add('');
  Memo1.Lines.Add('Event value: ' + aValue);
end;

end.
