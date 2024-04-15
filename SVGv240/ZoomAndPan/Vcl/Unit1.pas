unit Unit1;


//------------------------------------------------------------------------------
//
//                          Delphi SVG Package 2.4
//                           Zoom and pan example
//
//------------------------------------------------------------------------------

{
  Set the following propeties:

    On Form:
      DoubleBuffered = True

    On SVG2Image2
      AsspectRatioAlign = arXMidYMid
      AspectRatioMeetOrSlice = arSlice or arMeet
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
  BVE.SVG2Control.VCL,
  BVE.SVG2Image.VCL;

type
  TForm1 = class(TForm)
    SVG2Image1: TSVG2Image;
    OpenDialog1: TOpenDialog;
    procedure SVG2Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SVG2Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SVG2Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SVG2Image1AfterParse(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure SVG2Image1DblClick(Sender: TObject);
  private
    FViewBox: TSVGRect;
    FMousePt: TPoint;
    FMouseDown: Boolean;
    FZoomCenter: Boolean;

    function CalcZoomFactor: TSVGFloat;
    function CalcViewBoxPt(const aMousePt: TPoint): TSVGPoint;

    procedure CenterOnMousePt(const aMousePt: TPoint);
    procedure SVGRepaint;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

function TForm1.CalcViewBoxPt(const aMousePt: TPoint): TSVGPoint;
var
  ViewPort: TRect;
  Zoom: TSVGFloat;
begin
  ViewPort := SVG2Image1.ClientRect;

  Zoom := CalcZoomFactor;

  // Convert mousepoint to viewboxpoint

  Result.X := (aMousePt.X - ViewPort.Width/2) * Zoom + (FViewBox.Left + FViewBox.Width/2);
  Result.Y := (aMousePt.Y - ViewPort.Height/2) * Zoom + (FViewBox.Top + FViewBox.Height/2);
end;

function TForm1.CalcZoomFactor: TSVGFloat;
var
  ViewPort: TRect;
  ViewPortRatioXY,
  ViewBoxRatioXY: TSVGFloat;
begin
  // This calculates the zoom factor between between the viewPort and the viewBox

  ViewPort := SVG2Image1.ClientRect;

  // We have to take into consideration the aspect ratio settings

  ViewPortRatioXY := ViewPort.Width / ViewPort.Height;
  ViewBoxRatioXY := FViewBox.Width / FViewBox.Height;

  if SVG2Image1.AspectRatioMeetOrSlice = arSlice then
  begin

    if ViewPortRatioXY > ViewBoxRatioXY then
      Result := FViewBox.Width / ViewPort.Width
    else
      Result := FViewBox.Height / ViewPort.Height;

  end else begin

    if ViewPortRatioXY > ViewBoxRatioXY then
      Result := FViewBox.Height / ViewPort.Height
    else
      Result := FViewBox.Width / ViewPort.Width;

  end;
end;

procedure TForm1.CenterOnMousePt(const aMousePt: TPoint);
var
  Zoom: TSVGFLoat;
  ViewPort: TRect;
  Delta: TSVGPoint;
begin
  // Move the right-clicked viewBox point to the center

  ViewPort := SVG2Image1.ClientRect;

  Zoom := CalcZoomFactor;

  Delta.X := aMousePt.X * Zoom - (ViewPort.Width / 2) * Zoom;
  Delta.Y := aMousePt.Y * Zoom - (ViewPort.Height / 2) * Zoom;

  FViewBox.Left := FViewBox.Left + Delta.X;
  FViewBox.Right := FViewBox.Right + Delta.X;
  FViewBox.Top := FViewBox.Top + Delta.Y;
  FViewBox.Bottom := FViewBox.Bottom + Delta.Y;

  SVGRepaint;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  P: TPoint;
  DeltaZoom: TSVGFloat;
  Sign: TSVGFloat;
  ViewPort: TRect;
  ViewBoxOriginal: TSVGRect;
  Zoom: TSVGFloat;
begin
  // Zoom the SVG in or out on every change of the mousewheel

  if WheelDelta = 0 then
    Exit;

  // Zoom with arbitrary factor

  Sign := WheelDelta / abs(WheelDelta);
  DeltaZoom := -Sign * 0.05;

  ViewBoxOriginal := FViewBox;

  if FZoomCenter then
  begin
    // Optional:

    // If we want to zoom into the center of the viewport, the delta must be
    // applied equally in each direction

    FViewBox.Left := ViewBoxOriginal.Left - ViewBoxOriginal.Width * DeltaZoom;
    FViewBox.Top := ViewBoxOriginal.Top - ViewBoxOriginal.Height * DeltaZoom;
    FViewBox.Right := ViewBoxOriginal.Right + ViewBoxOriginal.Width * DeltaZoom;
    FViewBox.Bottom := ViewBoxOriginal.Bottom + ViewBoxOriginal.Height * DeltaZoom;


  end else begin

    // If we want to zoom into the current mouse position, we have to apply
    // more complex calculations:

    // X, Y       : Point in ViewBox that has to remain the same
    // VPW, VPH   : ViewPort Width and Height
    // Zoom:      : The current Zoom factor
    // DeltaZoom  : The factor that Zoom is increased or decreased
    // VBL, VBT   : ViewBox Left and ViewBox Top value
    // VBW, VBH   : ViewBox Width and Height
    // VBL?, VBT? : The new viewBox Left and Top value that we want to calculate

    // The viewBox width and height is increased/decreased with DeltaZoom.

    // VBW = VBW * (1 - DeltaZoom)
    // VBH = VBH * (1 - DeltaZoom)

    // The mousepoint converted to viewBox coords before increasing the zoom
    // is calculated as:

    // X = (MousePt.X - VPW/2) * Zoom + VBL + VBW/2;
    // Y = (MousePt.Y - VPH/2) * Zoom + VBT + VBH/2;

    // And after increasing zoom as:

    // X = (MousePt.X - VPW/2) * Zoom * (1 - DeltaZoom) + VBL? + VBW/2 * (1 - DeltaZoom)
    // X = (MousePt.Y - VPH/2) * Zoom * (1 - DeltaZoom) + VBT? + VBH/2 * (1 - DeltaZoom)

    // So the new viewBox left and Top values are calculated as:

    // VBL? = (MousePt.X - VPW/2) * Zoom * DeltaZoom + VBL + VBW/2 * DeltaZoom
    // VBT? = (MousePt.Y - VPH/2) * Zoom * DeltaZoom + VBT + VBH/2 * DeltaZoom

    ViewPort := SVG2Image1.ClientRect;

    Zoom := CalcZoomFactor;

    P := SVG2Image1.ScreenToClient(MousePos);

    FViewBox.Left := ((P.X - ViewPort.Width/2) * Zoom * DeltaZoom + ViewBoxOriginal.Left + ViewBoxOriginal.Width/2 * DeltaZoom);
    FViewBox.Top := ((P.Y - ViewPort.Height/2) * Zoom * DeltaZoom + ViewBoxOriginal.Top + ViewBoxOriginal.Height/2 * DeltaZoom);

    FViewBox.Right := FViewBox.Left + ViewBoxOriginal.Width * (1 - DeltaZoom);
    FViewBox.Bottom := FViewBox.Top + ViewBoxOriginal.Height * (1 - DeltaZoom);

  end;

  SVGRepaint;
end;

procedure TForm1.SVG2Image1AfterParse(Sender: TObject);
var
  SVG: ISVG;
  CR: TRect;
begin
  // Get the viewBox from the parsed SVG or create one if it is not defined

  // On the TSVG2Image the following properties need to be set:

  // AutoViewbox = True
  //  This will set the viewPort of the SVG to 100%, so it is always equal to the TSVG2Image ClientRect

  // AspectRatioAlign = arXMidYMid
  //  That is uniform scaling aligned to the middle of the viewPort


  // Get the Outer SVG element

  if not assigned(SVG2Image1.SVGRoot) then
    Exit;

  SVG := SVG2Image1.SVGRoot.SVG;

  if not assigned(SVG) then
    Exit;

  if SVG.ViewBox.IsUndefined then
  begin
    // No viewBox so we will create one with the same size as the viewPort.
    // The viewPort is defined by the dimensions of the outer SVG element.

    CR := SVG2Image1.ClientRect;

    FViewBox := SVG2Image1.SVGRoot.CalcIntrinsicSize(SVGRect(0, 0, CR.Width, CR.Height));
  end else
    FViewBox := SVG.ViewBox;
end;

procedure TForm1.SVG2Image1DblClick(Sender: TObject);
begin
  // Load a new SVG image by dubbel clicking the TSVG2Image

  if OpenDialog1.Execute then
  begin
    // Clear any SVG present in the SVG stringlist property, because it has
    // priority over the SVG filename property

    SVG2Image1.SVG.Clear;

    FViewBox := TSVGRect.CreateUndefined;

    SVG2Image1.Filename := OpenDialog1.FileName;
  end;
end;

procedure TForm1.SVG2Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (mbLeft = Button) and not(ssDouble in Shift) then
  begin
    // Save the mousedown point on mousedown

    FMouseDown := True;
    FMousePt := Point(X, Y);
  end;
end;

procedure TForm1.SVG2Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  Delta: TPoint;
  ViewPort: TRect;
  Zoom: TSVGFLoat;
  ViewBoxPt: TSVGPoint;
begin
  // Convert the mouse coords to viewBox coords and display in caption
  // Move the viewBox if the mouse btn is pressed

  ViewBoxPt := CalcViewBoxPt(Point(X, Y));
  Caption := Format('ViewBox Pt: %3.1f %3.1f', [ViewBoxPt.X, ViewBoxPt.Y]);

  if FMouseDown then
  begin
    // Compensate the mousemovent with the zoom factor so the image moves at
    // the same rate as the mouse

    ViewPort := SVG2Image1.ClientRect;

    Zoom := CalcZoomFactor;

    Delta := Point(FMousePt.X - X, FMousePt.Y - Y);

    // Calculate the new viewport position.

    FViewBox.Left := FViewBox.Left + Delta.X * Zoom;
    FViewBox.Right := FViewBox.Right + Delta.X * Zoom;
    FViewBox.Top := FViewBox.Top + Delta.Y * Zoom;
    FViewBox.Bottom := FViewBox.Bottom + Delta.Y * Zoom;

    // Save the new mousedown position

    FMousePt := Point(X, Y);

    // Set the new viewBox and repaint the SVG image.

    SVGRepaint;
  end;
end;

procedure TForm1.SVG2Image1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FMousePt := Point(0, 0);
  FMouseDown := False;

  if (mbRight = Button) then
    CenterOnMousePt(Point(X, Y));
end;

procedure TForm1.SVGRepaint;
var
  SVG: ISVG;
begin
  // Get the outer SVG element

  if not assigned(SVG2Image1.SVGRoot) then
    Exit;

  SVG := SVG2Image1.SVGRoot.SVG;

  if not assigned(SVG) then
    Exit;

  // Set the viewBox attribute on the outer SVG element

  SVG.ViewBox := FViewBox;
  SVG2Image1.Repaint;
end;

end.
