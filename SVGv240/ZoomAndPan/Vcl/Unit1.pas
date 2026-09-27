unit Unit1;


//------------------------------------------------------------------------------
//
//                          Delphi SVG Package 2.4
//                           Zoom and pan example
//
//------------------------------------------------------------------------------

{
  Zooming and panning are on the control. The view is a transform over the
  drawing - a scale and a translation - so nothing here touches the document,
  and the SVG that was loaded is still exactly the SVG that was loaded.

  Requires Delphi SVG Package 2.4 update 25 or later.

    ZoomByWheel   one turn of the wheel, about a point on the control
    MousePan      dragging with the left button moves the view
    PanBy         moves the view by so many pixels of the control
    ZoomToFit     fits everything the drawing paints
    ResetView     back to how it was loaded
    ClientToSVG   a point on the control as a point in the drawing
    ZoomToElement zooms so that an element fills the control (Ctrl+click)

  A drawing with a filter on it needs an off screen buffer per filter
  primitive, and those grow with the zoom. FilterBufferMaxPixels, on the
  control, is the number of pixels one of them may take: past that the filter
  is rendered at a lower resolution and scaled up, which costs sharpness but
  keeps zooming in bounded instead of failing. Eight megapixels by default,
  zero for no limit.

  The wheel is handled on the form rather than by the control, because a
  TSVG2Image has no window of its own and so is never sent the wheel message.
  One line passes it on.

  Set the following properties:

    On Form:
      DoubleBuffered = True

    On SVG2Image1
      AspectRatioAlign = arXMidYMid
      AspectRatioMeetOrSlice = arMeet
      AutoViewbox = True
}

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Types,
  Xml.XMLIntf,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  BVE.SVG2Types,
  BVE.SVG2Intf,
  BVE.SVG2Control.VCL,
  BVE.SVG2Image.VCL;

type
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
    function IdAt(const aX, aY: Single): string;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
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

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    // Fit everything the drawing paints, which is not the same as the
    // viewBox: a drawing can paint outside it.
    Ord('F'):
      SVG2Image1.ZoomToFit;

    // Back to how the drawing was loaded.
    Ord('R'), VK_ESCAPE:
      SVG2Image1.ResetView;

    Ord('0'):
      SVG2Image1.ZoomFactor := 1;
  end;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  P: TPoint;
begin
  // Zoom about the pointer, so whatever is under it stays under it. MousePos
  // is in screen coordinates and the control wants its own.

  P := SVG2Image1.ScreenToClient(MousePos);

  if not PtInRect(SVG2Image1.ClientRect, P) then
    Exit;

  SVG2Image1.ZoomByWheel(WheelDelta, SVGPoint(P.X, P.Y));

  Handled := True;
end;

procedure TForm1.SVG2Image1DblClick(Sender: TObject);
begin
  // Load a new SVG image by double clicking the TSVG2Image

  if OpenDialog1.Execute then
  begin
    // Clear any SVG present in the SVG stringlist property, because it has
    // priority over the SVG filename property

    SVG2Image1.SVG.Clear;
    SVG2Image1.Filename := OpenDialog1.FileName;

    // The view belongs to the drawing that was being looked at, not to this
    // one.

    SVG2Image1.ResetView;
  end;
end;

procedure TForm1.SVG2Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  // Where the pointer is, in the coordinates of the drawing. This is the
  // conversion hit testing uses, so it holds however the view is set - and it
  // keeps being reported while the view is being dragged around.

  FPointer := SVG2Image1.ClientToSVG(SVGPoint(X, Y));

  ShowState;
end;

procedure TForm1.SVG2Image1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Id: string;
begin
  // Ctrl+click zooms to what was clicked, with a tenth of its size free
  // around it. ZoomToElement takes an id, so the nearest element with one is
  // used: a shape often has no id of its own, the group it belongs to does.

  if (Button = mbLeft) and (ssCtrl in Shift) then
  begin
    Id := IdAt(X, Y);
    if Id <> '' then
      SVG2Image1.ZoomToElement(Id, 0.1);
    Exit;
  end;

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

// The id of the element under a point on the control, or of the nearest
// element around it that has one; empty when there is none
function TForm1.IdAt(const aX, aY: Single): string;
var
  Node: IXMLNode;
  Obj: ISVGObject;
begin
  Result := '';
  Node := SVG2Image1.ObjectAtPt(SVGPoint(aX, aY), False);
  while Assigned(Node) do
  begin
    if Supports(Node, ISVGObject, Obj) and (Obj.ID <> '') then
      Exit(Obj.ID);
    Node := Node.ParentNode;
  end;
end;

procedure TForm1.ShowState;
begin
  Caption := Format(
    'Zoom %.2fx   pointer at %.1f %.1f   ' +
    '[wheel] zoom  [drag] pan  [right click] centre  [ctrl+click] zoom to it  ' +
    '[F] fit  [R] reset  [double click] open',
    [SVG2Image1.ZoomFactor, FPointer.X, FPointer.Y]);
end;

end.
