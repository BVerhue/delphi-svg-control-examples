unit Unit1;


//------------------------------------------------------------------------------
//
//                          Delphi SVG Package 2.4
//                        Zoom and pan example, FMX
//
//------------------------------------------------------------------------------

{
  The same zoom and pan as the VCL examples in ..\Vcl and ..\VclWinControl,
  on the FMX TSVG2Control.

  Requires Delphi SVG Package 2.4 update 25 or later.

  The one thing that is not the same is the wheel. FMX reports it without
  saying where the pointer was, so MouseZoom on this control zooms about the
  middle. To zoom about the pointer, as this example does, leave MouseZoom off
  and handle OnMouseWheel on the control: ask the screen where the pointer is
  and hand that to ZoomByWheel. With MouseZoom on, the control takes the wheel
  itself and OnMouseWheel is not called.

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

  Set the following properties:

    On SVG2Control1
      AspectRatioAlign = arXMidYMid
      AspectRatioMeetOrSlice = arMeet
      AutoViewbox = True
}

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  Xml.XMLIntf,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  BVE.SVG2Types,
  BVE.SVG2Intf,
  BVE.SVG2Control.FMX;

type
  TForm1 = class(TForm)
    SVG2Control1: TSVG2Control;
    OpenDialog1: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure SVG2Control1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
    procedure SVG2Control1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure SVG2Control1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure SVG2Control1DblClick(Sender: TObject);
    procedure SVG2Control1ViewChanged(Sender: TObject);
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

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Dragging with the left button moves the view. Off by default, because a
  // control that starts moving when an application meant to click on it would
  // be a surprise.

  SVG2Control1.MousePan := True;

  // Left off, so that OnMouseWheel is called and can zoom about the pointer.

  SVG2Control1.MouseZoom := False;

  // A notch of the wheel is a tenth in or out, and the view can go from a
  // fiftieth to five hundred times. The drawings this renders survive far more
  // than that; these are limits for a person turning a wheel.

  SVG2Control1.WheelZoomStep := 1.1;
  SVG2Control1.ZoomMin := 0.02;
  SVG2Control1.ZoomMax := 500;

  ShowState;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  // FMX reports a letter in KeyChar and leaves Key at 0, and a key without a
  // character, such as Escape, the other way round.

  if Key = vkEscape then
    SVG2Control1.ResetView
  else
    case UpCase(KeyChar) of
      // Fit everything the drawing paints, which is not the same as the
      // viewBox: a drawing can paint outside it.
      'F':
        SVG2Control1.ZoomToFit;

      // Back to how the drawing was loaded.
      'R':
        SVG2Control1.ResetView;

      '0':
        SVG2Control1.ZoomFactor := 1;
    end;
end;

procedure TForm1.SVG2Control1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
var
  P: TPointF;
begin
  // Zoom about the pointer, so whatever is under it stays under it. FMX does
  // not pass the position with the wheel, so it is asked for, in screen
  // coordinates, and brought into the control's own.

  P := SVG2Control1.ScreenToLocal(Screen.MousePos);

  SVG2Control1.ZoomByWheel(WheelDelta, SVGPoint(P.X, P.Y));

  Handled := True;
end;

procedure TForm1.SVG2Control1DblClick(Sender: TObject);
begin
  // Load a new SVG image by double clicking the control

  if OpenDialog1.Execute then
  begin
    // Clear any SVG present in the SVG stringlist property, because it has
    // priority over the SVG filename property

    SVG2Control1.SVG.Clear;
    SVG2Control1.Filename := OpenDialog1.FileName;

    // The view belongs to the drawing that was being looked at, not to this
    // one.

    SVG2Control1.ResetView;
  end;
end;

procedure TForm1.SVG2Control1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
  // Where the pointer is, in the coordinates of the drawing. This is the
  // conversion hit testing uses, so it holds however the view is set - and it
  // keeps being reported while the view is being dragged around.

  FPointer := SVG2Control1.ClientToSVG(SVGPoint(X, Y));

  ShowState;
end;

procedure TForm1.SVG2Control1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  Id: string;
begin
  // Ctrl+click zooms to what was clicked, with a tenth of its size free
  // around it. ZoomToElement takes an id, so the nearest element with one is
  // used: a shape often has no id of its own, the group it belongs to does.

  if (Button = TMouseButton.mbLeft) and (ssCtrl in Shift) then
  begin
    Id := IdAt(X, Y);
    if Id <> '' then
      SVG2Control1.ZoomToElement(Id, 0.1);
    Exit;
  end;

  // Right click brings what was clicked to the middle. The view moves by the
  // distance from the click to the middle, in pixels of the control, which is
  // what PanBy takes.

  if Button = TMouseButton.mbRight then
    SVG2Control1.PanBy(
      SVG2Control1.Width / 2 - X,
      SVG2Control1.Height / 2 - Y);
end;

procedure TForm1.SVG2Control1ViewChanged(Sender: TObject);
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
  Node := SVG2Control1.ObjectAtPt(TPointF.Create(aX, aY), False);
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
    [SVG2Control1.ZoomFactor, FPointer.X, FPointer.Y]);
end;

end.
