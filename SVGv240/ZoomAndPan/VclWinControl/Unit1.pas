unit Unit1;


//------------------------------------------------------------------------------
//
//                          Delphi SVG Package 2.4
//                  Zoom and pan example, windowed control
//
//------------------------------------------------------------------------------

{
  The same zoom and pan as the TSVG2Image example in ..\Vcl, on a
  TSVG2WinControl instead.

  Requires Delphi SVG Package 2.4 update 25 or later.

  The difference is the window. A TSVG2WinControl has one of its own, so it is
  sent the mouse wheel and, with MouseZoom on, does the zooming itself. There
  is no OnMouseWheel on this form, and there should not be: the VCL offers the
  wheel to the form first, and a form that handles it takes it away from the
  control.

  The control renders with Direct2D into its own window, through a swap chain
  where Direct3D 11 is available. What is not covered by the drawing is painted
  in the Color of the control, which follows the form while ParentColor is on,
  as it is by default.

    MouseZoom     the wheel zooms about the pointer, handled by the control
    MousePan      dragging with the left button moves the view
    PanBy         moves the view by so many pixels of the control
    ZoomToFit     fits everything the drawing paints
    ResetView     back to how it was loaded
    ClientToSVG   a point on the control as a point in the drawing

  Set the following properties:

    On SVG2WinControl1
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
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  BVE.SVG2Types,
  BVE.SVG2Intf,
  BVE.SVG2Control.VCL;

type
  TForm1 = class(TForm)
    SVG2WinControl1: TSVG2WinControl;
    OpenDialog1: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SVG2WinControl1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SVG2WinControl1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SVG2WinControl1DblClick(Sender: TObject);
    procedure SVG2WinControl1ViewChanged(Sender: TObject);
  private
    FPointer: TSVGPoint;                        // Where the pointer last was,
                                                // in the drawing

    procedure ShowState;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  // The wheel zooms about the pointer. The control handles it, so unlike the
  // TSVG2Image example there is nothing to write on the form.

  SVG2WinControl1.MouseZoom := True;

  // Dragging with the left button moves the view. Off by default, because a
  // control that starts moving when an application meant to click on it would
  // be a surprise.

  SVG2WinControl1.MousePan := True;

  // A notch of the wheel is a tenth in or out, and the view can go from a
  // fiftieth to five hundred times. The drawings this renders survive far more
  // than that; these are limits for a person turning a wheel.

  SVG2WinControl1.WheelZoomStep := 1.1;
  SVG2WinControl1.ZoomMin := 0.02;
  SVG2WinControl1.ZoomMax := 500;

  ShowState;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    // Fit everything the drawing paints, which is not the same as the
    // viewBox: a drawing can paint outside it.
    Ord('F'):
      SVG2WinControl1.ZoomToFit;

    // Back to how the drawing was loaded.
    Ord('R'), VK_ESCAPE:
      SVG2WinControl1.ResetView;

    Ord('0'):
      SVG2WinControl1.ZoomFactor := 1;
  end;
end;

procedure TForm1.SVG2WinControl1DblClick(Sender: TObject);
begin
  // Load a new SVG image by double clicking the control

  if OpenDialog1.Execute then
  begin
    // Clear any SVG present in the SVG stringlist property, because it has
    // priority over the SVG filename property

    SVG2WinControl1.SVG.Clear;
    SVG2WinControl1.Filename := OpenDialog1.FileName;

    // The view belongs to the drawing that was being looked at, not to this
    // one.

    SVG2WinControl1.ResetView;
  end;
end;

procedure TForm1.SVG2WinControl1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  // Where the pointer is, in the coordinates of the drawing. This is the
  // conversion hit testing uses, so it holds however the view is set - and it
  // keeps being reported while the view is being dragged around.

  FPointer := SVG2WinControl1.ClientToSVG(SVGPoint(X, Y));

  ShowState;
end;

procedure TForm1.SVG2WinControl1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // Right click brings what was clicked to the middle. The view moves by the
  // distance from the click to the middle, in pixels of the control, which is
  // what PanBy takes.

  if Button = mbRight then
    SVG2WinControl1.PanBy(
      SVG2WinControl1.ClientWidth / 2 - X,
      SVG2WinControl1.ClientHeight / 2 - Y);
end;

procedure TForm1.SVG2WinControl1ViewChanged(Sender: TObject);
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
    [SVG2WinControl1.ZoomFactor, FPointer.X, FPointer.Y]);
end;

end.
