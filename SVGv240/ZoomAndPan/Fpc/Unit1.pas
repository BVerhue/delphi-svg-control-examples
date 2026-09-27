unit Unit1;


//------------------------------------------------------------------------------
//
//                          Delphi SVG Package 2.4
//                    Zoom and pan example, FPC / Lazarus
//
//------------------------------------------------------------------------------

{
  The zoom and pan of the Delphi example in ..\Vcl, on Lazarus. Zooming and
  panning are on the control. The view is a transform over the drawing - a
  scale and a translation - so nothing here touches the document, and the SVG
  that was loaded is still exactly the SVG that was loaded.

  Requires the SVG Control Library 2.4 update 25 or later.

    ZoomByWheel   one turn of the wheel, about a point on the control
    MousePan      dragging with the left button moves the view
    PanBy         moves the view by so many pixels of the control
    ZoomToFit     fits everything the drawing paints
    ResetView     back to how it was loaded
    ClientToSVG   a point on the control as a point in the drawing

  The drawing has a blurred shadow. A filter needs an off screen buffer per
  filter primitive, and those grow with the zoom. FilterBufferMaxPixels, on
  the control, is the number of pixels one of them may take: past that the
  filter is rendered at a lower resolution and scaled up, which costs
  sharpness but keeps zooming in bounded instead of failing. Eight megapixels
  by default, zero for no limit.

  The wheel is handled on the form rather than by the control, because a
  TSVG2Image has no window of its own and so is never sent the wheel. The LCL
  hands the form's OnMouseWheel the pointer in client coordinates of the form.
  On Windows, TSVG2WinControl has a window of its own and zooms by itself with
  MouseZoom on.

  Set the following properties:

    On Form1
      KeyPreview = True

    On SVG2Image1
      AspectRatioAlign = arXMidYMid
      AspectRatioMeetOrSlice = arMeet
      AutoViewbox = True
}

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Types,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  LCLType,
  BVE.SVG2Types,
  BVE.SVG2Control.FPC,
  BVE.SVG2Image.FPC;

type

  { TForm1 }

  TForm1 = class(TForm)
    SVG2Image1: TSVG2Image;
    OpenDialog1: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SVG2Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SVG2Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SVG2Image1DblClick(Sender: TObject);
    procedure SVG2Image1ViewChanged(Sender: TObject);
  private
    FPointer: TSVGPoint;                        // Where the pointer last was,
                                                // in the drawing

    procedure ShowState;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

const
  // A site plan with a shadow under the building, so the filter is there to
  // zoom into
  Drawing =
    '<svg xmlns="http://www.w3.org/2000/svg" width="600" height="400" viewBox="0 0 600 400">' +
    '<defs><filter id="shadow" x="-10%" y="-10%" width="130%" height="130%">' +
    '<feGaussianBlur in="SourceAlpha" stdDeviation="6"/>' +
    '<feOffset dx="8" dy="8" result="blur"/>' +
    '<feMerge><feMergeNode in="blur"/><feMergeNode in="SourceGraphic"/></feMerge>' +
    '</filter></defs>' +
    '<rect width="600" height="400" fill="#e8f0e0"/>' +
    '<path d="M0 330 C150 300 300 360 600 320 L600 400 L0 400 Z" fill="#9cc3e6"/>' +
    '<g filter="url(#shadow)">' +
    '<rect x="160" y="80" width="280" height="180" fill="#f4f4f4" stroke="#555" stroke-width="3"/>' +
    '<line x1="300" y1="80" x2="300" y2="260" stroke="#555" stroke-width="2"/>' +
    '<line x1="160" y1="170" x2="300" y2="170" stroke="#555" stroke-width="2"/>' +
    '</g>' +
    '<text x="230" y="130" font-family="Arial" font-size="14" text-anchor="middle">Office</text>' +
    '<text x="230" y="220" font-family="Arial" font-size="14" text-anchor="middle">Store</text>' +
    '<text x="370" y="175" font-family="Arial" font-size="14" text-anchor="middle">Workshop</text>' +
    '<text x="300" y="370" font-family="Arial" font-size="12" text-anchor="middle" fill="#2a5d8a">River</text>' +
    '<circle cx="80" cy="120" r="30" fill="#6aa84f"/><circle cx="520" cy="110" r="24" fill="#6aa84f"/>' +
    '</svg>';

procedure TForm1.FormCreate(Sender: TObject);
begin
  SVG2Image1.SVG.Text := Drawing;

  // Dragging with the left button moves the view. Off by default, because a
  // control that starts moving when an application meant to click on it would
  // be a surprise.

  SVG2Image1.MousePan := True;

  // A notch of the wheel is a tenth in or out, and the view can go from a
  // fiftieth to five hundred times. The drawings this renders survive far more
  // than that; these are limits for a person turning a wheel.

  SVG2Image1.WheelZoomStep := 1.1;
  SVG2Image1.ZoomMin := 0.02;
  SVG2Image1.ZoomMax := 500;

  ShowState;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  P: TPoint;
begin
  // Zoom about the pointer, so whatever is under it stays under it. MousePos
  // is on the form and the control wants its own coordinates.

  P := SVG2Image1.ScreenToClient(ClientToScreen(MousePos));

  if not PtInRect(SVG2Image1.ClientRect, P) then
    Exit;

  SVG2Image1.ZoomByWheel(WheelDelta, SVGPoint(P.X, P.Y));

  Handled := True;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    // Fit everything the drawing paints, which is not the same as the
    // viewBox: a drawing can paint outside it.
    VK_F:
      SVG2Image1.ZoomToFit;

    // Back to how the drawing was loaded.
    VK_R, VK_ESCAPE:
      SVG2Image1.ResetView;

    VK_0:
      SVG2Image1.ZoomFactor := 1;
  end;
end;

procedure TForm1.SVG2Image1DblClick(Sender: TObject);
begin
  // Load another SVG image by double clicking the control

  if OpenDialog1.Execute then
  begin
    // Clear the SVG stringlist property, because it has priority over the
    // Filename property

    SVG2Image1.SVG.Clear;
    SVG2Image1.Filename := OpenDialog1.FileName;

    // The view belongs to the drawing that was being looked at, not to this
    // one.

    SVG2Image1.ResetView;
  end;
end;

procedure TForm1.SVG2Image1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  // Where the pointer is, in the coordinates of the drawing. This is the
  // conversion hit testing uses, so it holds however the view is set - and it
  // keeps being reported while the view is being dragged around.

  FPointer := SVG2Image1.ClientToSVG(SVGPoint(X, Y));

  ShowState;
end;

procedure TForm1.SVG2Image1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // Right click brings what was clicked to the middle. The view moves by the
  // distance from the click to the middle, in pixels of the control, which is
  // what PanBy takes.

  if Button = mbRight then
    SVG2Image1.PanBy(
      SVG2Image1.ClientWidth / 2 - X,
      SVG2Image1.ClientHeight / 2 - Y);
end;

procedure TForm1.SVG2Image1ViewChanged(Sender: TObject);
begin
  // Occurs however the view changed - the wheel, a drag, or any of the
  // methods.

  ShowState;
end;

procedure TForm1.ShowState;
begin
  Caption := Format(
    'Zoom %.2fx   pointer at %.1f %.1f   ' +
    '[wheel] zoom  [drag] pan  [right click] centre  ' +
    '[F] fit  [R] reset  [double click] open',
    [SVG2Image1.ZoomFactor, FPointer.X, FPointer.Y]);
end;

end.
