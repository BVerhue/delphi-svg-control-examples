unit BVE.SVG2PathGeometry;
//------------------------------------------------------------------------------
//
//                          SVG Control Package 2.0
//                     Copyright (c) 2015 Bruno Verhue
//
//------------------------------------------------------------------------------

/// <summary>
///   Unit containing pathe geometry classes.
/// </summary>

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface
uses
  System.SysUtils,
  System.Generics.Collections,
  System.Math,
  BVE.SVG2Types,
  BVE.SVG2Intf,
  BVE.SVG2ParseUtility,
  BVE.SVG2GeomUtility,
  BVE.SVG2PathData;

type
  ISVGPathSegment = interface
    ['{CE946139-3E3E-42A7-A577-7E89D3218F54}']
    function GetEndPoint: TSVGPoint;
    function GetIsRelative: Boolean;
    procedure SetEndPoint(const Value: TSVGPoint);
    procedure SetIsRelative(const Value: Boolean);

    procedure ApplyMatrix(const aMatrix: TSVGMatrix);

    function CalcBBox(const aStartPoint: TSVGPoint): TSVGRect;

    property IsRelative: Boolean read GetIsRelative write SetIsRelative;
    property EndPoint: TSVGPoint read GetEndPoint write SetEndPoint;
  end;

  ISVGLineSegment = interface(ISVGPathSegment)
    ['{F6574BFF-2D28-4782-B2A5-858FEC95F919}']
    function GetPoint: TSVGPoint;
    procedure SetPoint(const Value: TSVGPoint);

    property Point: TSVGPoint read GetPoint write SetPoint;
  end;

  ISVGBezierSegment = interface(ISVGPathSegment)
    ['{B0AFA0DF-D2F8-4340-8F41-C8B8E0D91AB1}']

    function GetPoint1: TSVGPoint;
    function GetPoint2: TSVGPoint;
    function GetPoint3: TSVGPoint;
    procedure SetPoint1(const Value: TSVGPoint);
    procedure SetPoint2(const Value: TSVGPoint);
    procedure SetPoint3(const Value: TSVGPoint);

    property Point1: TSVGPoint read GetPoint1 write SetPoint1;
    property Point2: TSVGPoint read GetPoint2 write SetPoint2;
    property Point3: TSVGPoint read GetPoint3 write SetPoint3;
  end;

  ISVGQuadSegment = interface(ISVGPathSegment)
    ['{FCA983E0-19A4-41FB-B394-BFF2467A2E63}']

    function GetPoint1: TSVGPoint;
    function GetPoint2: TSVGPoint;
    procedure SetPoint1(const Value: TSVGPoint);
    procedure SetPoint2(const Value: TSVGPoint);

    property Point1: TSVGPoint read GetPoint1 write SetPoint1;
    property Point2: TSVGPoint read GetPoint2 write SetPoint2;
  end;

  ISVGArcSegment = interface(ISVGPathSegment)
    ['{3AB2BA25-A8EA-483D-8D7E-2B83D75B4A8F}']

    function GetIsLargeArc: Boolean;
    function GetPoint: TSVGPoint;
    function GetAngle: TSVGFloat;
    function GetRadius: TSVGPoint;
    function GetSweepDirection: Boolean;
    procedure SetIsLargeArc(const Value: Boolean);
    procedure SetPoint(const Value: TSVGPoint);
    procedure SetAngle(const Value: TSVGFloat);
    procedure SetRadius(const Value: TSVGPoint);
    procedure SetSweepDirection(const Value: Boolean);

    function GetVectorAngle(const aP1, aP2: TSVGPoint): TSVGFloat;
    function GetEllipsePointForAngle(const aCP, aR: TSVGPoint; const aAngle,
      aArcAngle: TSVGFloat): TSVGPoint;

    procedure GetCenterParameters(const aStartPoint: TSVGPoint;
      var aCP: TSVGPoint; var aStartAngle, aDeltaAngle: TSVGFloat);
    function SetCenterParameters(const aCP, aR: TSVGPoint;
      const aAngle, aStartAngle, aDeltaAngle: TSVGFloat): TSVGPoint;

    property IsLargeArc: Boolean read GetIsLargeArc write SetIsLargeArc;
    property Point: TSVGPoint read GetPoint write SetPoint;
    property Angle: TSVGFloat read GetAngle write SetAngle;
    property Radius: TSVGPoint read GetRadius write SetRadius;
    property SweepDirection: Boolean read GetSweepDirection write SetSweepDirection;
  end;

  ISVGPathFigure = interface
    ['{26971FD4-190B-4A79-A060-046FF77D84EB}']
    function GetIsClosed: Boolean;
    function GetSegments: TList<ISVGPathSegment>;
    function GetStartPoint: TSVGPoint;
    procedure SetIsClosed(const Value: Boolean);
    procedure SetStartPoint(const Value: TSVGPoint);

    procedure AddLine(const aPoint: TSVGPoint);
    procedure AddBezier(const aPoint1, aPoint2, aPoint3: TSVGPoint);
    procedure AddQuad(const aPoint1, aPoint2: TSVGPoint);
    procedure AddArc(const aRadius: TSVGPoint; const aAngle: TSVGFloat;
      const aLarge, aSweep: Boolean; const aPoint: TSVGPoint);

    procedure ApplyMatrix(const aMatrix: TSVGMatrix);

    property IsClosed: Boolean read GetIsClosed write SetIsClosed;
    property StartPoint: TSVGPoint read GetStartPoint write SetStartPoint;
    property Segments: TList<ISVGPathSegment> read GetSegments;
  end;

  ISVGPathGeometry = interface
    ['{44CD989C-3B9A-4212-86ED-D1ED0B9DEA24}']
    function GetAsString: TSVGUnicodeString;
    function GetFigures: TList<ISVGPathFigure>;
    function GetTransform: TSVGMatrix;
    procedure SetAsString(const Value: TSVGUnicodeString);
    procedure SetTransform(const Value: TSVGMatrix);

    procedure ApplyMatrix(const aMatrix: TSVGMatrix);

    procedure ConvertToPathData(aSink: ISVGPathDataSink);

    property AsString: TSVGUnicodeString read GetAsString write SetAsString;
    property Figures: TList<ISVGPathFigure> read GetFigures;
    property Transform: TSVGMatrix read GetTransform write SetTransform;
  end;

  TSVGPathSegment = class(TInterfacedObject, ISVGPathSegment)
  private
    FIsRelative: Boolean;
  protected
    function GetEndPoint: TSVGPoint; virtual; abstract;
    function GetIsRelative: Boolean;
    procedure SetEndPoint(const Value: TSVGPoint); virtual; abstract;
    procedure SetIsRelative(const Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;

    function CalcBBox(const aStartPoint: TSVGPoint): TSVGRect; virtual; abstract;

    procedure ApplyMatrix(const aMatrix: TSVGMatrix); virtual; abstract;

    property IsRelative: Boolean read GetIsRelative write SetIsRelative;
    property EndPoint: TSVGPoint read GetEndPoint write SetEndPoint;
  end;

  TSVGLineSegment = class(TSVGPathSegment, ISVGLineSegment)
  private
    FPoint: TSVGPoint;
  protected
    function GetEndPoint: TSVGPoint; override;
    function GetPoint: TSVGPoint;
    procedure SetEndPoint(const Value: TSVGPoint); override;
    procedure SetPoint(const Value: TSVGPoint);
  public
    constructor Create;
    destructor Destroy; override;

    function CalcBBox(const aStartPoint: TSVGPoint): TSVGRect; override;

    procedure ApplyMatrix(const aMatrix: TSVGMatrix); override;

    property Point: TSVGPoint read GetPoint write SetPoint;
  end;

  TSVGBezierSegment = class(TSVGPathSegment, ISVGBezierSegment)
  private
    FPoint1: TSVGPoint;
    FPoint2: TSVGPoint;
    FPoint3: TSVGPoint;
  protected
    function GetEndPoint: TSVGPoint; override;
    function GetPoint1: TSVGPoint;
    function GetPoint2: TSVGPoint;
    function GetPoint3: TSVGPoint;
    procedure SetEndPoint(const Value: TSVGPoint); override;
    procedure SetPoint1(const Value: TSVGPoint);
    procedure SetPoint2(const Value: TSVGPoint);
    procedure SetPoint3(const Value: TSVGPoint);
  public
    constructor Create;
    destructor Destroy; override;

    function CalcBBox(const aStartPoint: TSVGPoint): TSVGRect; override;

    procedure ApplyMatrix(const aMatrix: TSVGMatrix); override;

    property Point1: TSVGPoint read GetPoint1 write SetPoint1;
    property Point2: TSVGPoint read GetPoint2 write SetPoint2;
    property Point3: TSVGPoint read GetPoint3 write SetPoint3;
  end;

  TSVGQuadSegment = class(TSVGPathSegment, ISVGQuadSegment)
  private
    FPoint1: TSVGPoint;
    FPoint2: TSVGPoint;
  protected
    function GetEndPoint: TSVGPoint; override;
    function GetPoint1: TSVGPoint;
    function GetPoint2: TSVGPoint;
    procedure SetEndPoint(const Value: TSVGPoint); override;
    procedure SetPoint1(const Value: TSVGPoint);
    procedure SetPoint2(const Value: TSVGPoint);
  public
    constructor Create;
    destructor Destroy; override;

    function CalcBBox(const aStartPoint: TSVGPoint): TSVGRect; override;

    procedure ApplyMatrix(const aMatrix: TSVGMatrix); override;

    property Point1: TSVGPoint read GetPoint1 write SetPoint1;
    property Point2: TSVGPoint read GetPoint2 write SetPoint2;
  end;

  TSVGArcSegment = class(TSVGPathSegment, ISVGArcSegment)
  private
    FIsLargeArc: Boolean;
    FPoint: TSVGPoint;
    FAngle: TSVGFloat;
    FRadius: TSVGPoint;
    FSweepDirection: Boolean;
  protected
    function GetEndPoint: TSVGPoint; override;
    function GetIsLargeArc: Boolean;
    function GetPoint: TSVGPoint;
    function GetAngle: TSVGFloat;
    function GetRadius: TSVGPoint;
    function GetSweepDirection: Boolean;
    procedure SetEndPoint(const Value: TSVGPoint); override;
    procedure SetIsLargeArc(const Value: Boolean);
    procedure SetPoint(const Value: TSVGPoint);
    procedure SetAngle(const Value: TSVGFloat);
    procedure SetRadius(const Value: TSVGPoint);
    procedure SetSweepDirection(const Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;

    function CalcBBox(const aStartPoint: TSVGPoint): TSVGRect; override;

    function GetVectorAngle(const aP1, aP2: TSVGPoint): TSVGFloat;
    function GetEllipsePointForAngle(const aCP, aR: TSVGPoint; const aAngle,
      aArcAngle: TSVGFloat): TSVGPoint;

    procedure GetCenterParameters(const aStartPoint: TSVGPoint;
      var aCP: TSVGPoint; var aStartAngle, aDeltaAngle: TSVGFloat);
    function SetCenterParameters(const aCP, aR: TSVGPoint;
      const aAngle, aStartAngle, aDeltaAngle: TSVGFloat): TSVGPoint;

    procedure ApplyMatrix(const aMatrix: TSVGMatrix); override;

    property IsLargeArc: Boolean read GetIsLargeArc write SetIsLargeArc;
    property Point: TSVGPoint read GetPoint write SetPoint;
    property Angle: TSVGFloat read GetAngle write SetAngle;
    property Radius: TSVGPoint read GetRadius write SetRadius;
    property SweepDirection: Boolean read GetSweepDirection write SetSweepDirection;
  end;

  TSVGPathFigure = class(TInterfacedObject, ISVGPathFigure)
  private
    FSegments: TList<ISVGPathSegment>;
    FStartPoint: TSVGPoint;
    FIsClosed: Boolean;
  protected
    function GetIsClosed: Boolean;
    function GetSegments: TList<ISVGPathSegment>;
    function GetStartPoint: TSVGPoint;
    procedure SetIsClosed(const Value: Boolean);
    procedure SetStartPoint(const Value: TSVGPoint);
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddLine(const aPoint: TSVGPoint);
    procedure AddBezier(const aPoint1, aPoint2, aPoint3: TSVGPoint);
    procedure AddQuad(const aPoint1, aPoint2: TSVGPoint);
    procedure AddArc(const aRadius: TSVGPoint; const aAngle: TSVGFloat;
      const aLarge, aSweep: Boolean; const aPoint: TSVGPoint);

    procedure ApplyMatrix(const aMatrix: TSVGMatrix); virtual;

    property IsClosed: Boolean read GetIsClosed write SetIsClosed;
    property StartPoint: TSVGPoint read GetStartPoint write SetStartPoint;
    property Segments: TList<ISVGPathSegment> read GetSegments;
  end;

  TSVGPathGeometry = class(TInterfacedObject, ISVGPathGeometry, ISVGPathDataSink)
  private
    FFigures: TList<ISVGPathFigure>;
    FTransform: TSVGMatrix;
  protected
    function GetAsString: TSVGUnicodeString;
    function GetFigures: TList<ISVGPathFigure>;
    function GetLastFigure: ISVGPathFigure;
    function GetTransform: TSVGMatrix;
    procedure SetAsString(const Value: TSVGUnicodeString);
    procedure SetTransform(const Value: TSVGMatrix);

    property LastFigure: ISVGPathFigure read GetLastFigure;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ApplyMatrix(const aMatrix: TSVGMatrix);

    procedure BeginFigure;
    procedure EndFigure;

    procedure MoveTo(const aP: TSVGPoint);
    procedure CurveTo(const aCP1, aCP2, aP2: TSVGPoint);
    procedure LineTo(const aP: TSVGPoint);
    procedure ClosePath(const aClosed: Boolean);
    procedure AddQuadCurve(const aP1, aCP, aP2: TSVGPoint);
    procedure AddArc(const aP1, aRadius: TSVGPoint; aAngle: TSVGFloat;
      const aLargeFlag, aSweepFlag: Boolean; const aP2: TSVGPoint);

    procedure ConvertToPathData(aSink: ISVGPathDataSink);

    property AsString: TSVGUnicodeString read GetAsString write SetAsString;
    property Figures: TList<ISVGPathFigure> read GetFigures;
    property Transform: TSVGMatrix read GetTransform write SetTransform;
  end;

implementation

// -----------------------------------------------------------------------------
//
//                            TSVGPathSegment
//
// -----------------------------------------------------------------------------

constructor TSVGPathSegment.Create;
begin
  inherited Create;

  FIsRelative := False;
end;

destructor TSVGPathSegment.Destroy;
begin
  inherited;
end;

function TSVGPathSegment.GetIsRelative: Boolean;
begin
  Result := FIsRelative;
end;

procedure TSVGPathSegment.SetIsRelative(const Value: Boolean);
begin
  if FIsRelative <> Value then
  begin
    FIsRelative := Value;
  end;
end;

// -----------------------------------------------------------------------------
//
//                            TSVGLineSegment
//
// -----------------------------------------------------------------------------

procedure TSVGLineSegment.ApplyMatrix(const aMatrix: TSVGMatrix);
begin
  Point := TransformPoint(Point, aMatrix);
end;

function TSVGLineSegment.CalcBBox(const aStartPoint: TSVGPoint): TSVGRect;
begin
  Result.Left := aStartPoint.X;
  Result.Right := aStartPoint.X;
  Result.Top := aStartPoint.Y;
  Result.Bottom := aStartPoint.Y;

  if Point.X < Result.Left then
    Result.Left := Point.X;

  if Point.X > Result.Right then
    Result.Right := Point.X;

  if Point.Y < Result.Top then
    Result.Top := Point.Y;

  if Point.Y > Result.Bottom then
    Result.Bottom := Point.Y;
end;

constructor TSVGLineSegment.Create;
begin
  inherited Create;

  FPoint := SVGPoint(0, 0);
end;

destructor TSVGLineSegment.Destroy;
begin
  inherited;
end;

function TSVGLineSegment.GetEndPoint: TSVGPoint;
begin
  Result := Point;
end;

function TSVGLineSegment.GetPoint: TSVGPoint;
begin
  Result := FPoint;
end;

procedure TSVGLineSegment.SetEndPoint(const Value: TSVGPoint);
begin
  Point := Value;
end;

procedure TSVGLineSegment.SetPoint(const Value: TSVGPoint);
begin
  if Value <> FPoint then
  begin
    FPoint := Value;
  end;
end;

// -----------------------------------------------------------------------------
//
//                           TSVGBezierSegment
//
// -----------------------------------------------------------------------------

procedure TSVGBezierSegment.ApplyMatrix(const aMatrix: TSVGMatrix);
begin
  Point1 := TransformPoint(Point1, aMatrix);
  Point2 := TransformPoint(Point2, aMatrix);
  Point3 := TransformPoint(Point3, aMatrix);
end;

// https://github.com/adobe-webplatform/Snap.svg/blob/b242f49e6798ac297a3dad0dfb03c0893e394464/src/path.js#L856
function TSVGBezierSegment.CalcBBox(const aStartPoint: TSVGPoint): TSVGRect;
var
  i, Count: Integer;
  a, b, c, t, t1, t2, b2ac, sqrtb2ac: TSVGFLoat;
  mt, x, y: TSVGFloat;
  Values: array[0..3] of TSVGFloat;

  procedure UpdateBounds(const aX, aY: TSVGFloat);
  begin
    if aX < Result.Left then
      Result.Left := aX;

    if aX > Result.Right then
      Result.Right := aX;

    if aY < Result.Top then
      Result.Top := aY;

    if aY > Result.Bottom then
      Result.Bottom := aY;
  end;

begin
  Count := 0;
  for i := 0 to 1 do
  begin
    if (i = 0) then
    begin
      b := 6 * aStartPoint.X - 12 * Point1.X + 6 * Point2.X;
      a := -3 * aStartPoint.X + 9 * Point1.X - 9 * Point2.X + 3 * Point3.X;
      c := 3 * Point1.X - 3 * aStartPoint.X;
    end else begin
      b := 6 * aStartPoint.Y - 12 * Point1.Y + 6 * Point2.Y;
      a := -3 * aStartPoint.Y + 9 * Point1.Y - 9 * Point2.Y + 3 * Point3.Y;
      c := 3 * Point1.Y - 3 * aStartPoint.Y;
    end;

    if abs(a) < 1e-12 then
    begin
      if abs(b) < 1e-12 then
        Continue;

      t := -c / b;
      if (0 < t) and (t < 1) then
      begin
        Values[Count] := t;
        Inc(Count);
      end;

      Continue;
    end;

    b2ac := b * b - 4 * c * a;
    if (b2ac < 0) then
      Continue;

    sqrtb2ac := Sqrt(b2ac);

    t1 := (-b + sqrtb2ac) / (2 * a);
    if (0 < t1) and (t1 < 1) then
    begin
      Values[Count] := t1;
      Inc(Count);
    end;

    t2 := (-b - sqrtb2ac) / (2 * a);
    if (0 < t2) and (t2 < 1) then
    begin
      Values[Count] := t2;
      Inc(Count);
    end;
  end;

  Result.Left := aStartPoint.X;
  Result.Right := aStartPoint.X;
  Result.Top := aStartPoint.Y;
  Result.Bottom := aStartPoint.Y;

  i := Count - 1;
  while (i >= 0) do
  begin
    t := Values[i];
    mt := 1 - t;

    x := mt * mt * mt * aStartPoint.X + 3 * mt * mt * t * Point1.X + 3 * mt * t * t * Point2.X + t * t * t * Point3.X;
    y := mt * mt * mt * aStartPoint.Y + 3 * mt * mt * t * Point1.Y + 3 * mt * t * t * Point2.Y + t * t * t * Point3.Y;

    UpdateBounds(x, y);

    Dec(i);
  end;

  UpdateBounds(Point3.X, Point3.Y);
end;

constructor TSVGBezierSegment.Create;
begin
  inherited Create;

  FPoint1 := SVGPoint(0, 0);
  FPoint2 := SVGPoint(0, 0);
  FPoint3 := SVGPoint(0, 0);
end;

destructor TSVGBezierSegment.Destroy;
begin
  inherited;
end;

function TSVGBezierSegment.GetEndPoint: TSVGPoint;
begin
  Result := Point3;
end;

function TSVGBezierSegment.GetPoint1: TSVGPoint;
begin
  Result := FPoint1;
end;

function TSVGBezierSegment.GetPoint2: TSVGPoint;
begin
  Result := FPoint2;
end;

function TSVGBezierSegment.GetPoint3: TSVGPoint;
begin
  Result := FPoint3;
end;

procedure TSVGBezierSegment.SetEndPoint(const Value: TSVGPoint);
begin
  Point3 := Value;
end;

procedure TSVGBezierSegment.SetPoint1(const Value: TSVGPoint);
begin
  if Value <> FPoint1 then
  begin
    FPoint1 := Value;
  end;
end;

procedure TSVGBezierSegment.SetPoint2(const Value: TSVGPoint);
begin
  if Value <> FPoint2 then
  begin
    FPoint2 := Value;
  end;
end;

procedure TSVGBezierSegment.SetPoint3(const Value: TSVGPoint);
begin
  if Value <> FPoint3 then
  begin
    FPoint3 := Value;
  end;
end;

// -----------------------------------------------------------------------------
//
//                           TSVGQuadSegment
//
// -----------------------------------------------------------------------------

procedure TSVGQuadSegment.ApplyMatrix(const aMatrix: TSVGMatrix);
begin
  Point1 := TransformPoint(Point1, aMatrix);
  Point2 := TransformPoint(Point2, aMatrix);
end;

// https://github.polettix.it/ETOOBUSY/2020/07/17/bbox-quadratic-bezier/
function TSVGQuadSegment.CalcBBox(const aStartPoint: TSVGPoint): TSVGRect;
var
  i, Count: Integer;
  a, b, t: TSVGFLoat;
  mt, x, y: TSVGFloat;
  Values: array[0..1] of TSVGFloat;

  procedure UpdateBounds(const aX, aY: TSVGFloat);
  begin
    if aX < Result.Left then
      Result.Left := aX;

    if aX > Result.Right then
      Result.Right := aX;

    if aY < Result.Top then
      Result.Top := aY;

    if aY > Result.Bottom then
      Result.Bottom := aY;
  end;

begin
  Count := 0;
  for i := 0 to 1 do
  begin
    if (i = 0) then
    begin
      b := aStartPoint.X - 2 * Point1.X + Point2.X;
      a := -aStartPoint.X + Point1.X;
    end else begin
      b := aStartPoint.Y - 2 * Point1.Y + Point2.Y;
      a := -aStartPoint.Y + Point1.Y;
    end;

    if abs(b) > 1e-12 then
    begin
      t := -a / b;

      if (0 < t) and (t < 1) then
      begin
        Values[Count] := t;
        Inc(Count);
      end;
    end;
  end;

  Result.Left := aStartPoint.X;
  Result.Right := aStartPoint.X;
  Result.Top := aStartPoint.Y;
  Result.Bottom := aStartPoint.Y;

  i := Count - 1;
  while (i >= 0) do
  begin
    t := Values[i];
    mt := 1 - t;

    x := mt * mt * aStartPoint.X + 2 * mt * t * Point1.X + t * t * Point2.X;
    y := mt * mt * aStartPoint.Y + 2 * mt * t * Point1.Y + t * t * Point2.Y;

    UpdateBounds(x, y);

    Dec(i);
  end;

  UpdateBounds(Point2.X, Point2.Y);
end;

constructor TSVGQuadSegment.Create;
begin
  inherited Create;

  FPoint1 := SVGPoint(0, 0);
  FPoint2 := SVGPoint(0, 0);
end;

destructor TSVGQuadSegment.Destroy;
begin
  inherited;
end;

function TSVGQuadSegment.GetEndPoint: TSVGPoint;
begin
  Result := Point2;
end;

function TSVGQuadSegment.GetPoint1: TSVGPoint;
begin
  Result := FPoint1;
end;

function TSVGQuadSegment.GetPoint2: TSVGPoint;
begin
  Result := FPoint2;
end;

procedure TSVGQuadSegment.SetEndPoint(const Value: TSVGPoint);
begin
  Point2 := Value;
end;

procedure TSVGQuadSegment.SetPoint1(const Value: TSVGPoint);
begin
  if FPoint1 <> value then
  begin
    FPoint1 := Value;
  end;
end;

procedure TSVGQuadSegment.SetPoint2(const Value: TSVGPoint);
begin
  if FPoint2 <> value then
  begin
    FPoint2 := Value;
  end;
end;

// -----------------------------------------------------------------------------
//
//                           TSVGArcSegment
//
// -----------------------------------------------------------------------------

procedure TSVGArcSegment.ApplyMatrix(const aMatrix: TSVGMatrix);
var
  P1, P2: TSVGPoint;
  RX, RY: TSVGFloat;
begin
  P1 := TransformPoint(SVGPoint(0, 0), aMatrix);
  P2 := TransformPoint(SVGPoint(FRadius.X, 0), aMatrix);

  RX := Sqrt((P2.X - P1.X) * (P2.X - P1.X) + (P2.Y - P1.Y) * (P2.Y - P1.Y));

  P1 := TransformPoint(SVGPoint(0, 0), aMatrix);
  P2 := TransformPoint(SVGPoint(0, FRadius.Y), aMatrix);

  RY := Sqrt((P2.X - P1.X) * (P2.X - P1.X) + (P2.Y - P1.Y) * (P2.Y - P1.Y));

  Point := TransformPoint(Point, aMatrix);
  Radius := SVGPoint(RX, RY);
end;

// http://fridrich.blogspot.com/2011/06/bounding-box-of-svg-elliptical-arc.html
function TSVGArcSegment.CalcBBox(const aStartPoint: TSVGPoint): TSVGRect;
var
  R: TSVGPoint;
  x1Prime, y1Prime: TSVGFloat;
  Radicant, Ratio, Factor: TSVGFloat;
  cxPrime, cyPrime, cx, cy: TSVGFloat;
  xmin, xmax, ymin, ymax: TSVGFloat;
  txmin, txmax, tymin, tymax: TSVGFloat;
  tmpX, tmpY: TSVGFloat;
  Angle1, Angle2: TSVGFloat;
  OtherArc: Boolean;

  procedure UpdateBoundsX(const aX: TSVGFloat);
  begin
    if aX < Result.Left then
      Result.Left := aX;

    if aX > Result.Right then
      Result.Right := aX;
  end;

  procedure UpdateBoundsY(const aY: TSVGFloat);
  begin
    if aY < Result.Top then
      Result.Top := aY;

    if aY > Result.Bottom then
      Result.Bottom := aY;
  end;

  function FMod(const ANumerator, ADenominator: TSVGFloat): TSVGFloat;
  begin
    Result := ANumerator - Trunc(ANumerator / ADenominator) * ADenominator;
  end;

  function getAngle(const bx, by: TSVGFloat): TSVGFloat;
  var
    Sign: TSVGFloat;
  begin
    if by > 0.0 then
      Sign := 1.0
    else
      Sign := -1.0;

    Result := FMod(2*PI + Sign * ArcCos( bx / Sqrt(bx * bx + by * by)), 2*PI);
  end;

  procedure Swap(var aValue1, aValue2: TSVGFloat);
  var
    Temp: TSVGFloat;
  begin
    Temp := aValue1;
    aValue1 := aValue2;
    aValue2 := Temp;
  end;

begin
  Result.Left := aStartPoint.X;
  Result.Right := aStartPoint.X;
  Result.Top := aStartPoint.Y;
  Result.Bottom := aStartPoint.Y;

  if Radius.X < 0.0 then
    R.X := -Radius.X
  else
    R.X := Radius.X;

  if Radius.Y < 0.0 then
    R.Y := -Radius.Y
  else
    R.Y := Radius.Y;

  if (R.X = 0.0) or (R.Y = 0.0) then
  begin
    UpdateBoundsX(Point.X);
    UpdateBoundsY(Point.Y);

    Exit;
  end;

  x1Prime := Cos(Angle)*(aStartPoint.X - Point.X)/2 + Sin(Angle)*(aStartPoint.y - Point.Y)/2;
  y1Prime := -Sin(Angle)*(aStartPoint.X - Point.X)/2 + Cos(Angle)*(aStartPoint.y - Point.Y)/2;

  Radicant := (R.X*R.X*R.Y*R.Y - R.X*R.X*y1Prime*y1Prime - R.Y*R.Y*x1Prime*x1Prime);
  Radicant := Radicant / (R.X*R.X*y1Prime*y1Prime + R.Y*R.Y*x1Prime*x1Prime);

  cxPrime := 0.0;
  cyPrime := 0.0;
  if Radicant < 0.0 then
  begin
    Ratio := R.X/R.Y;
    Radicant := y1Prime * y1Prime + x1Prime * x1Prime / (Ratio * Ratio);
    if Radicant < 0.0 then
    begin
      UpdateBoundsX(Point.X);
      UpdateBoundsY(Point.Y);

      Exit;
    end;
    R.Y := Sqrt(Radicant);
    R.X := Ratio*R.Y;
  end else begin
    if IsLargeArc = SweepDirection then
      Factor := -1.0
    else
      Factor := 1.0;

    Factor := Factor * Sqrt(Radicant);

    cxPrime := Factor * R.X * y1Prime / R.Y;
    cyPrime := -Factor * R.Y * x1Prime / R.X;
  end;

  cx := cxPrime*Cos(Angle) - cyPrime*Sin(Angle) + (aStartPoint.X + Point.X)/2;
  cy := cxPrime*Sin(Angle) + cyPrime*Cos(Angle) + (aStartPoint.Y + Point.Y)/2;

  if (Angle = 0) or (Angle = PI) then
  begin
    xmin := cx - R.X;
    txmin := getAngle(-R.X, 0);
    xmax := cx + R.X;
    txmax := getAngle(R.X, 0);
    ymin := cy - R.Y;
    tymin := getAngle(0, -R.Y);
    ymax := cy + R.Y;
    tymax := getAngle(0, R.Y);
  end else
    if (Angle = PI / 2.0) or (Angle = 3.0*PI/2.0) then
    begin
      xmin := cx - R.Y;
      txmin := getAngle(-R.Y, 0);
      xmax := cx + R.Y;
      txmax := getAngle(R.Y, 0);
      ymin := cy - R.X;
      tymin := getAngle(0, -R.X);
      ymax := cy + R.X;
      tymax := getAngle(0, R.X);
    end else begin
      //txmin = -atan(ry*tan(phi)/rx);
      //txmax = M_PI - atan (ry*tan(phi)/rx);

      txmin := -ArcTan(R.Y * Tan(Angle) / R.X);
      txmax := PI - ArcTan(R.Y * Tan(Angle) / R.X);

      xmin := cx + R.X * Cos(txmin) * Cos(Angle) - R.Y * Sin(txmin) * Sin(Angle);
      xmax := cx + R.X * Cos(txmax) * Cos(Angle) - R.Y * Sin(txmax) * Sin(Angle);

      if (xmin > xmax) then
      begin
        Swap(xmin,xmax);
        Swap(txmin,txmax);
      end;

      tmpY := cy + R.X * Cos(txmin) * Sin(Angle) + R.Y * Sin(txmin) * Cos(Angle);
      txmin := getAngle(xmin - cx, tmpY - cy);

      tmpY := cy + R.X * Cos(txmax) * Sin(Angle) + R.Y * Sin(txmax) * Cos(Angle);
      txmax := getAngle(xmax - cx, tmpY - cy);

      //tymin = atan(ry/(tan(phi)*rx));
      //tymax = atan(ry/(tan(phi)*rx))+M_PI;
      tymin := ArcTan(R.Y / (Tan(Angle) * R.X));
      tymax := ArcTan(R.Y / (Tan(Angle) * R.X)) + PI;

      ymin := cy + R.X * Cos(tymin) * Sin(Angle) + R.Y * Sin(tymin) * Cos(Angle);
      ymax := cy + R.X * Cos(tymax) * Sin(Angle) + R.Y * Sin(tymax) * Cos(Angle);

      if (ymin > ymax) then
      begin
        Swap(ymin,ymax);
        Swap(tymin,tymax);
      end;

      tmpX := cx + R.X * Cos(tymin) * Cos(Angle) - R.Y * Sin(tymin) * Sin(Angle);
      tymin := getAngle(tmpX - cx, ymin - cy);

      tmpX := cx + R.X * Cos(tymax) * Cos(Angle) - R.Y * Sin(tymax) * Sin(Angle);
      tymax := getAngle(tmpX - cx, ymax - cy);
    end;

  Angle1 := getAngle(aStartPoint.X - cx, aStartPoint.Y - cy);
  Angle2 := getAngle(Point.X - cx, Point.Y - cy);

  if not SweepDirection then
    Swap(angle1, angle2);

  OtherArc := False;
  if (angle1 > angle2) then
  begin
    Swap(angle1, angle2);
    OtherArc := True;
  end;

  if ((not OtherArc and ((angle1 > txmin) or (angle2 < txmin)))
  or (OtherArc and not((angle1 > txmin) or (angle2 < txmin)))) then
  begin
    if aStartPoint.X < Point.X then
      xmin := aStartPoint.X
    else
      xmin := Point.X;
  end;

  if ((not OtherArc and ((angle1 > txmax) or (angle2 < txmax)))
  or (OtherArc and not((angle1 > txmax) or (angle2 < txmax)))) then
  begin
    if aStartPoint.X > Point.X then
      xmax := aStartPoint.X
    else
      xmax := Point.X;
  end;

  if ((not OtherArc and ((angle1 > tymin) or (angle2 < tymin)))
  or (OtherArc and not((angle1 > tymin) or (angle2 < tymin)))) then
  begin
    if aStartPoint.Y < Point.Y then
      ymin := aStartPoint.Y
    else
      ymin := Point.Y;
  end;

  if ((not OtherArc and ((angle1 > tymax) or (angle2 < tymax)))
  or (OtherArc and not((angle1 > tymax) or (angle2 < tymax)))) then
  begin
    if aStartPoint.Y > Point.Y then
      ymax := aStartPoint.Y
    else
      ymax := Point.Y;
  end;

  Result.Left := xmin;
  Result.Right := xmax;
  Result.Top := ymin;
  Result.Bottom := ymax;
end;

//https://stackoverflow.com/questions/9017100/calculate-center-of-svg-arc
//https://observablehq.com/@toja/ellipse-and-elliptical-arc-conversion
{function TSVGArcSegment.CalcEllipse(const aStartPoint: TSVGPoint;
  var aCP: TSVGPoint; var aStartAngle, aDeltaAngle: TSVGFloat): Boolean;
var
  s_phi, c_phi, hd_x, hd_y, hs_x, hs_y: TSVGFloat;
  x1_, y1_, rxry, rxy1_, ryx1_, sum_of_sq, coe: TSVGFloat;
  cx_, cy_, xcr1, xcr2, ycr1, ycr2: TSVGFloat;
  PIx2: TSVGFloat;

    function Radian(const ux, uy, vx, vy: TSVGFloat): TSVGFloat;
    var
      dot, md: TSVGFloat;
    begin
      dot := ux * vx + uy * vy;
      md := Sqrt((ux * ux + uy * uy) * (vx * vx + vy * vy));
      Result := ArcCos(dot / md);
      if ux * vy - uy * vx < 0.0 then
        Result := -Result;
    end;

begin
  //var cx,cy,theta1,delta_theta;

  Result := True;

  if (Radius.X = 0.0) or (Radius.Y = 0.0) then
  begin
    Result := False;  // invalid arguments
    Exit;
  end;

  s_phi := Sin(Angle * PI / 180);
  c_phi := Cos(Angle * PI / 180);
  hd_x := (aStartPoint.X - Point.X) / 2.0;   // half diff of x
  hd_y := (aStartPoint.Y - Point.Y) / 2.0;   // half diff of y
  hs_x := (aStartPoint.X + Point.X) / 2.0;   // half sum of x
  hs_y := (aStartPoint.Y + Point.Y) / 2.0;   // half sum of y

  // F6.5.1
  x1_ := c_phi * hd_x + s_phi * hd_y;
  y1_ := c_phi * hd_y - s_phi * hd_x;

  rxry := Radius.X * Radius.Y;
  rxy1_ := Radius.X * y1_;
  ryx1_ := Radius.Y * x1_;
  sum_of_sq := rxy1_ * rxy1_ + ryx1_ * ryx1_;   // sum of square
  coe := Sqrt(abs(rxry * rxry - sum_of_sq) / sum_of_sq);
  if IsLargeArc = SweepDirection then
    coe := -coe;

  // F6.5.2
  cx_ := coe * rxy1_ / Radius.Y;
  cy_ := -coe * ryx1_ / Radius.X;

  // F6.5.3
  aCP.X := c_phi * cx_ - s_phi * cy_ + hs_x;
  aCP.Y := s_phi * cx_ + c_phi * cy_ + hs_y;

  xcr1 := (x1_ - cx_) / Radius.X;
  xcr2 := (x1_ + cx_) / Radius.X;
  ycr1 := (y1_ - cy_) / Radius.Y;
  ycr2 := (y1_ + cy_) / Radius.Y;

  // F6.5.5
  aStartAngle := Radian(1.0, 0.0, xcr1, ycr1);

  // F6.5.6
  aDeltaAngle := Radian(xcr1, ycr1, -xcr2, -ycr2);
  PIx2 := PI * 2.0;
  while( aDeltaAngle > PIx2 ) do
    aDeltaAngle := aDeltaAngle - PIx2;

  while( aDeltaAngle < 0.0 ) do
    aDeltaAngle := aDeltaAngle + PIx2;

  if not SweepDirection then
    aDeltaAngle := aDeltaAngle - PIx2;
end;}

procedure TSVGArcSegment.GetCenterParameters(const aStartPoint: TSVGPoint;
  var aCP: TSVGPoint; var aStartAngle, aDeltaAngle: TSVGFloat);
var
  SinPhi, CosPhi: TSVGFloat;
  x, y, px, py, prx, pry, L, rx, ry, M, Sign, tcx, tcy: TSVGFLoat;
begin
  SinPhi := Sin(Angle * PI / 180);
  CosPhi := Cos(Angle * PI / 180);

  // Step 1: simplify through translation/rotation
  x :=  CosPhi * (aStartPoint.X - Point.X) / 2 + SinPhi * (aStartPoint.Y - Point.Y) / 2;
  y := -SinPhi * (aStartPoint.X - Point.X) / 2 + CosPhi * (aStartPoint.Y - Point.Y) / 2;

  px := x * x;
  py := y * y;
  prx := Radius.X * Radius.X;
  pry := Radius.Y * Radius.Y;

  // correct of out-of-range radii
  L := px / prx + py / pry;

  if (L > 1) then
  begin
    rx := Sqrt(L) * Abs(Radius.X);
    ry := Sqrt(L) * Abs(Radius.Y);

    prx := rx * rx;
    pry := ry * ry;
  end else begin
    rx := Abs(Radius.X);
    ry := Abs(Radius.Y);
  end;

  // Step 2 + 3: compute center
  if IsLargeArc = SweepDirection then
    Sign := -1
  else
    Sign := 1;

  M := Sqrt(Abs(prx * pry - prx * py - pry * px) / (prx * py + pry * px)) * Sign;

  tcx := M * (rx * y) / ry;
  tcy := M * (-ry * x) / rx;

  aCP.X := CosPhi * tcx - SinPhi * tcy + (aStartPoint.X + Point.X) / 2;
  aCP.Y := SinPhi * tcx + CosPhi * tcy + (aStartPoint.Y + Point.Y) / 2;

  // Step 4: compute angles
  aStartAngle := GetVectorAngle(
    SVGPoint(1, 0),
    SVGPoint((x - tcx) / rx, (y - tcy) / ry));

  aDeltaAngle := GetVectorAngle(
      SVGPoint((x - tcx) / rx, (y - tcy) / ry),
      SVGPoint((-x - tcx) / rx, (-y - tcy) / ry));

  while aDeltaAngle > (2 * PI) do
    aDeltaAngle := aDeltaAngle - 2 * PI;

  while aDeltaAngle < 0.0 do
    aDeltaAngle := aDeltaAngle + 2 * PI;

  if (not SweepDirection) and (aDeltaAngle > 0) then
    aDeltaAngle := aDeltaAngle - 2 * PI;

  if SweepDirection and (aDeltaAngle < 0) then
    aDeltaAngle := aDeltaAngle + 2 * PI;
end;

constructor TSVGArcSegment.Create;
begin
  inherited Create;

  FIsLargeArc := False;
  FPoint := SVGPoint(0, 0);
  FAngle := 0;
  FRadius := SVGPoint(0, 0);
  FSweepDirection := False;
end;

destructor TSVGArcSegment.Destroy;
begin
  inherited;
end;

function TSVGArcSegment.GetEllipsePointForAngle(const aCP, aR: TSVGPoint;
  const aAngle, aArcAngle: TSVGFloat): TSVGPoint;
var
  M, N: TSVGFloat;
begin
  M := Abs(aR.X) * Cos(aArcAngle);
  N := Abs(aR.Y) * Sin(aArcAngle);

  Result.X := aCP.X + Cos(aAngle) * M - Sin(aAngle) * N;
  Result.Y := aCP.Y + Sin(aAngle) * M + Cos(aAngle) * N;
end;

function TSVGArcSegment.GetEndPoint: TSVGPoint;
begin
  Result := Point;
end;

function TSVGArcSegment.GetIsLargeArc: Boolean;
begin
  Result := FIsLargeArc;
end;

function TSVGArcSegment.GetPoint: TSVGPoint;
begin
  Result := FPoint;
end;

function TSVGArcSegment.GetAngle: TSVGFloat;
begin
  Result := FAngle;
end;

function TSVGArcSegment.GetRadius: TSVGPoint;
begin
  Result := FRadius;
end;

function TSVGArcSegment.GetSweepDirection: Boolean;
begin
  Result := FSweepDirection;
end;

function TSVGArcSegment.GetVectorAngle(const aP1, aP2: TSVGPoint): TSVGFloat;
var
  Dot, Md: TSVGFloat;
begin
  Dot := aP1.X * aP2.X + aP1.Y * aP2.Y;
  Md := Sqrt((aP1.X * aP1.X + aP1.Y * aP1.Y) * (aP2.X * aP2.X + aP2.Y * aP2.Y));
  Result := ArcCos(Dot / Md);
  if aP1.X * aP2.Y - aP2.Y * aP1.X < 0.0 then
    Result := -Result;
end;

procedure TSVGArcSegment.SetEndPoint(const Value: TSVGPoint);
begin
  Point := Value;
end;

procedure TSVGArcSegment.SetIsLargeArc(const Value: Boolean);
begin
  if FIsLargeArc <> Value then
  begin
    FIsLargeArc := Value;
  end;
end;

procedure TSVGArcSegment.SetPoint(const Value: TSVGPoint);
begin
  if FPoint <> Value then
  begin
    FPoint := Value;
  end;
end;

procedure TSVGArcSegment.SetAngle(const Value: TSVGFloat);
begin
  if FAngle <> Value then
  begin
    FAngle := Value;
  end;
end;

// https://observablehq.com/@toja/ellipse-and-elliptical-arc-conversion
function TSVGArcSegment.SetCenterParameters(const aCP, aR: TSVGPoint;
  const aAngle, aStartAngle, aDeltaAngle: TSVGFloat): TSVGPoint;
begin
  Result := GetEllipsePointForAngle(aCP, aR, aAngle, aStartAngle);
  Point := GetEllipsePointForAngle(aCP, aR, aAngle, aStartAngle + aDeltaAngle);
  Angle := aAngle * 180 / PI;
  Radius := aR;

  IsLargeArc := Abs(aDeltaAngle) > PI;
  SweepDirection := aDeltaAngle > 0;
end;

procedure TSVGArcSegment.SetRadius(const Value: TSVGPoint);
begin
  if FRadius <> Value then
  begin
    FRadius := Value;
  end;
end;

procedure TSVGArcSegment.SetSweepDirection(const Value: Boolean);
begin
  if FSweepDirection <> Value then
  begin
    FSweepDirection := Value;
  end;
end;

// -----------------------------------------------------------------------------
//
//                           TSVGPathFigure
//
// -----------------------------------------------------------------------------

procedure TSVGPathFigure.AddArc(const aRadius: TSVGPoint;
  const aAngle: TSVGFloat; const aLarge, aSweep: Boolean;
  const aPoint: TSVGPoint);
var
  ArcSegment: ISVGArcSegment;
begin
  ArcSegment := TSVGArcSegment.Create;
  Segments.Add(ArcSegment);

  ArcSegment.Radius := aRadius;
  ArcSegment.Angle := aAngle;
  ArcSegment.IsLargeArc := aLarge;
  ArcSegment.SweepDirection := aSweep;
  ArcSegment.Point := aPoint;
end;

procedure TSVGPathFigure.AddBezier(const aPoint1, aPoint2, aPoint3: TSVGPoint);
var
  BezierSegment: ISVGBezierSegment;
begin
  BezierSegment := TSVGBezierSegment.Create;
  Segments.Add(BezierSegment);

  BezierSegment.Point1 := aPoint1;
  BezierSegment.Point2 := aPoint2;
  BezierSegment.Point3 := aPoint3;
end;

procedure TSVGPathFigure.AddLine(const aPoint: TSVGPoint);
var
  LineSegment: ISVGLineSegment;
begin
  LineSegment := TSVGLineSegment.Create;
  Segments.Add(LineSegment);

  LineSegment.Point := aPoint;
end;

procedure TSVGPathFigure.AddQuad(const aPoint1, aPoint2: TSVGPoint);
var
  QuadSegment: ISVGQuadSegment;
begin
  QuadSegment := TSVGQuadSegment.Create;
  Segments.Add(QuadSegment);

  QuadSegment.Point1 := aPoint1;
  QuadSegment.Point2 := aPoint2;
end;

procedure TSVGPathFigure.ApplyMatrix(const aMatrix: TSVGMatrix);
var
  Segment: ISVGPathSegment;
begin
  Startpoint := TransformPoint(StartPoint, aMatrix);

  for Segment in Segments do
    Segment.ApplyMatrix(aMatrix);
end;

constructor TSVGPathFigure.Create;
begin
  inherited Create;

  FSegments := TList<ISVGPathSegment>.Create;
  FStartPoint := SVGPoint(0, 0);
  FIsClosed := False;
end;

destructor TSVGPathFigure.Destroy;
begin
  FSegments.Free;

  inherited;
end;

function TSVGPathFigure.GetIsClosed: Boolean;
begin
  Result := FIsClosed;
end;

function TSVGPathFigure.GetSegments: TList<ISVGPathSegment>;
begin
  Result := FSegments;
end;

function TSVGPathFigure.GetStartPoint: TSVGPoint;
begin
  Result := FStartPoint;
end;

procedure TSVGPathFigure.SetIsClosed(const Value: Boolean);
begin
  FIsClosed := Value;
end;

procedure TSVGPathFigure.SetStartPoint(const Value: TSVGPoint);
begin
  if FStartPoint <> Value then
  begin
    FStartPoint := Value;
  end;
end;

// -----------------------------------------------------------------------------
//
//                          TSVGPathGeometry
//
// -----------------------------------------------------------------------------

procedure TSVGPathGeometry.AddArc(const aP1, aRadius: TSVGPoint;
  aAngle: TSVGFloat; const aLargeFlag, aSweepFlag: Boolean;
  const aP2: TSVGPoint);
begin
  LastFigure.AddArc(aRadius, aAngle, aLargeFlag, aSweepFlag, aP2);
end;

procedure TSVGPathGeometry.AddQuadCurve(const aP1, aCP, aP2: TSVGPoint);
begin
  LastFigure.AddQuad(aCP, aP2);
end;

procedure TSVGPathGeometry.ApplyMatrix(const aMatrix: TSVGMatrix);
var
  Figure: ISVGPathFigure;
begin
  for Figure in FFigures do
    Figure.ApplyMatrix(aMatrix);
end;

procedure TSVGPathGeometry.BeginFigure;
begin
  //
end;

procedure TSVGPathGeometry.ClosePath(const aClosed: Boolean);
begin
  LastFigure.IsClosed := aClosed;
end;

procedure TSVGPathGeometry.ConvertToPathData(aSink: ISVGPathDataSink);
var
  Figure: ISVGPathFigure;
  Segment: ISVGPathSegment;
  LineSegment: ISVGLineSegment;
  BezierSegment: ISVGBezierSegment;
  QuadSegment: ISVGQuadSegment;
  ArcSegment: ISVGArcSegment;
  LastPoint: TSVGPoint;
begin
  aSink.BeginFigure;
  try
    for Figure in FFigures do
    begin
      LastPoint := TransformPoint(Figure.StartPoint, FTransform);
      aSink.MoveTo(LastPoint);

      for Segment in Figure.Segments do
      begin
        if Supports(Segment, ISVGLineSegment, LineSegment) then
        begin
          LastPoint := TransformPoint(LineSegment.Point, FTransform);
          aSink.LineTo(LastPoint);
        end else

        if Supports(Segment, ISVGBezierSegment, BezierSegment) then
        begin
          LastPoint := TransformPoint(BezierSegment.Point3, FTransform);

          aSink.CurveTo(
            TransformPoint(BezierSegment.Point1, FTransform),
            TransformPoint(BezierSegment.Point2, FTransform),
            LastPoint);
        end else

        if Supports(Segment, ISVGQuadSegment, QuadSegment) then
        begin
          aSink.AddQuadCurve(
            LastPoint,
            TransformPoint(QuadSegment.Point1, FTransform),
            TransformPoint(QuadSegment.Point2, FTransform));
          LastPoint := TransformPoint(QuadSegment.Point2, FTransform);
        end else

        if Supports(Segment, ISVGArcSegment, ArcSegment) then
        begin
          aSink.AddArc(
            LastPoint,
            ArcSegment.Radius,
            ArcSegment.Angle,
            ArcSegment.IsLargeArc,
            ArcSegment.SweepDirection,
            TransformPoint(ArcSegment.Point, FTransform));

          LastPoint := TransformPoint(ArcSegment.Point, FTransform);
        end;
      end;

      aSink.ClosePath(Figure.IsClosed);
    end;
  finally
    aSink.EndFigure;
  end;
end;

constructor TSVGPathGeometry.Create;
begin
  inherited Create;

  FTransform := TSVGMatrix.CreateIdentity;
  FFigures := TList<ISVGPathFigure>.Create;
end;

procedure TSVGPathGeometry.CurveTo(const aCP1, aCP2, aP2: TSVGPoint);
begin
  LastFigure.AddBezier(aCP1, aCP2, aP2);
end;

destructor TSVGPathGeometry.Destroy;
begin
  FFigures.Free;

  inherited;
end;

procedure TSVGPathGeometry.EndFigure;
begin
  //
end;

function TSVGPathGeometry.GetAsString: TSVGUnicodeString;
var
  Figure: ISVGPathFigure;
  Segment: ISVGPathSegment;
  LineSegment: ISVGLineSegment;
  BezierSegment: ISVGBezierSegment;
  QuadSegment: ISVGQuadSegment;
  ArcSegment: ISVGArcSegment;
  StringBuilder: TBaseStringBuilder;
  Cmd, LastCmd: TSVGUnicodeChar;
  P1, P2, P3: TSVGPoint;
begin
  Result := '';

  StringBuilder := TBaseStringBuilder.Create;
  try
    for Figure in FFigures do
    begin
      LastCmd := 'L';

      P1 := TransformPoint(Figure.StartPoint, FTransform);

      StringBuilder.Append(Format('%s%g,%g', ['M', P1.X, P1.Y], USFormatSettings));

      for Segment in Figure.Segments do
      begin
        if Supports(Segment, ISVGLineSegment, LineSegment) then
        begin
          StringBuilder.Append(' ');

          if LineSegment.IsRelative then
            Cmd := 'l'
          else
            Cmd := 'L';

          if Cmd <> LastCmd then
            StringBuilder.Append(Cmd);
          LastCmd := Cmd;

          P1 := TransformPoint(LineSegment.Point, FTransform);

          StringBuilder.Append(Format('%g,%g', [P1.X, P1.Y], USFormatSettings));
        end else

        if Supports(Segment, ISVGBezierSegment, BezierSegment) then
        begin
          StringBuilder.Append(' ');

          if BezierSegment.IsRelative then
            Cmd := 'c'
          else
            Cmd := 'C';

          if Cmd <> LastCmd then
            StringBuilder.Append(Cmd);
          LastCmd := Cmd;

          P1 := TransformPoint(BezierSegment.Point1, FTransform);
          P2 := TransformPoint(BezierSegment.Point2, FTransform);
          P3 := TransformPoint(BezierSegment.Point3, FTransform);

          StringBuilder.Append(Format('%g,%g %g,%g %g,%g',
            [P1.X, P1.Y,
             P2.X, P2.Y,
             P3.X, P3.Y],
            USFormatSettings));
        end else

        if Supports(Segment, ISVGQuadSegment, QuadSegment) then
        begin
          StringBuilder.Append(' ');

          if QuadSegment.IsRelative then
            Cmd := 'q'
          else
            Cmd := 'Q';

          if Cmd <> LastCmd then
            StringBuilder.Append(Cmd);
          LastCmd := Cmd;

          P1 := TransformPoint(QuadSegment.Point1, FTransform);
          P2 := TransformPoint(QuadSegment.Point2, FTransform);

          StringBuilder.Append(Format('%g,%g %g,%g',
            [P1.X, P1.Y,
             P2.X, P2.Y],
            USFormatSettings));
        end else

        if Supports(Segment, ISVGArcSegment, ArcSegment) then
        begin
          StringBuilder.Append(' ');

          if ArcSegment.IsRelative then
            Cmd := 'a'
          else
            Cmd := 'A';

          if Cmd <> LastCmd then
            StringBuilder.Append(Cmd);
          LastCmd := Cmd;

          P1 := TransformPoint(ArcSegment.Point, FTransform);

          StringBuilder.Append(Format('%g,%g %g %d %d %g,%g',
            [ArcSegment.Radius.X, ArcSegment.Radius.Y,
             ArcSegment.Angle,
             Integer(ArcSegment.IsLargeArc),
             Integer(ArcSegment.SweepDirection),
             P1.X, P1.Y],
            USFormatSettings));
        end;
      end;

      if Figure.IsClosed then
      begin
        StringBuilder.Append(' Z');
      end;
    end;

    Result := StringBuilder.ToUnicodeString;
  finally
    StringBuilder.Free;
  end;
end;

function TSVGPathGeometry.GetFigures: TList<ISVGPathFigure>;
begin
  Result := FFigures;
end;

function TSVGPathGeometry.GetLastFigure: ISVGPathFigure;
begin
  if Figures.Count = 0 then
    raise Exception.Create('Geometry contains no figures.');

  Result := Figures[Figures.Count - 1];
end;

function TSVGPathGeometry.GetTransform: TSVGMatrix;
begin
  Result := FTransform;
end;

procedure TSVGPathGeometry.LineTo(const aP: TSVGPoint);
begin
  LastFigure.AddLine(aP);
end;

procedure TSVGPathGeometry.MoveTo(const aP: TSVGPoint);
var
  Figure: ISVGPathFigure;
begin
  Figure := TSVGPathFigure.Create;
  Figures.Add(Figure);

  Figure.StartPoint := aP;
end;

procedure TSVGPathGeometry.SetAsString(const Value: TSVGUnicodeString);
var
  Parser: TSVGPathStringParser;
  LastPoint: TSVGPoint;
begin
  Parser := TSVGPathStringParser.Create(Value);
  try
    FFigures.Clear;
    LastPoint := SVGPoint(0, 0);

    Parser.SkipWhiteSpace;
    while Parser.Pos < Parser.Length do
    begin
      Parser.ParsePath(Self, LastPoint);
      Parser.SkipWhiteSpace;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TSVGPathGeometry.SetTransform(const Value: TSVGMatrix);
begin
  FTransform := Value;
end;

end.
