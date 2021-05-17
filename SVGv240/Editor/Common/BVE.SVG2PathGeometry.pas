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

{ TSVGPathSegment }

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

{ TSVGLineSegment }

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

{ TSVGBezierSegment }

procedure TSVGBezierSegment.ApplyMatrix(const aMatrix: TSVGMatrix);
begin
  Point1 := TransformPoint(Point1, aMatrix);
  Point2 := TransformPoint(Point2, aMatrix);
  Point3 := TransformPoint(Point3, aMatrix);
end;

{// https://github.com/adobe-webplatform/Snap.svg/blob/b242f49e6798ac297a3dad0dfb03c0893e394464/src/path.js#L856
function TSVGBezierSegment.CalcBBox(const aStartPoint: TSVGPoint): TSVGRect;
var
  i: Integer;
  a, b, c, t, t1, t2, b2ac, sqrtb2ac: TSVGFLoat;
  mt, x, y: TSVGFloat;
  TValueList: TList<TSVGFloat>;

  procedure UpdateBounds(const aX, aY: TSVGFloat);
  begin
    if aX < Result.Left then
      Result.Left := aX;

    if aX > Result.Right then
      Result.Right := aX;

    if aY < Result.Top then
      Result.Top := aY;

    if aY > Result.Bottom then
      Result.Bottom := aX;
  end;

begin
  TValueList := TList<TSVGFloat>.Create;
  try
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

      if (abs(a) < 1e-12) then
      begin
        if (abs(b) < 1e-12) then
          Continue;

        t := -c / b;
        if (0 < t) and (t < 1) then
            TValueList.Add(t);

        Continue;
      end;

      b2ac := b * b - 4 * c * a;
      sqrtb2ac := Sqrt(b2ac);
      if (b2ac < 0) then
        Continue;

      t1 := (-b + sqrtb2ac) / (2 * a);
      if (0 < t1) and (t1 < 1) then
        TValueList.Add(t1);

      t2 := (-b - sqrtb2ac) / (2 * a);
      if (0 < t2) and (t2 < 1) then
          TValueList.Add(t2);
    end;

    Result.Left := aStartPoint.X;
    Result.Right := aStartPoint.X;
    Result.Top := aStartPoint.Y;
    Result.Bottom := aStartPoint.Y;

    i := TValueList.Count - 1;
    while (i >= 0) do
    begin
      t := TValueList[i];
      mt := 1 - t;

      x := mt * mt * mt * aStartPoint.X + 3 * mt * mt * t * Point1.X + 3 * mt * t * t * Point2.X + t * t * t * Point3.X;
      y := mt * mt * mt * aStartPoint.Y + 3 * mt * mt * t * Point1.Y + 3 * mt * t * t * Point2.Y + t * t * t * Point3.Y;

      UpdateBounds(x, y);

      Dec(i);
    end;

    UpdateBounds(Point3.X, Point3.Y);

  finally
    TValueList.Free;
  end;
end;}

// https://www.iquilezles.org/www/articles/bezierbbox/bezierbbox.htm
function TSVGBezierSegment.CalcBBox(const aStartPoint: TSVGPoint): TSVGRect;
var
  a, b, c, h: TSVGPoint;
  t, s, q: TSVGFloat;

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

begin
  Result.Left := aStartPoint.X;
  Result.Right := aStartPoint.X;
  Result.Top := aStartPoint.Y;
  Result.Bottom := aStartPoint.Y;

  UpdateBoundsX(Point3.X);
  UpdateBoundsY(Point3.Y);

  c.X := -1.0 * aStartPoint.X + 1.0 * Point1.X;
  c.Y := -1.0 * aStartPoint.Y + 1.0 * Point1.Y;
  b.X :=  1.0 * aStartPoint.X - 2.0 * Point1.X + 1.0 * Point2.X;
  b.Y :=  1.0 * aStartPoint.Y - 2.0 * Point1.Y + 1.0 * Point2.Y;
  a.X := -1.0 * aStartPoint.X + 3.0 * Point1.X - 3.0 * Point2.X + 1.0 * Point3.X;
  a.Y := -1.0 * aStartPoint.Y + 3.0 * Point1.Y - 3.0 * Point2.Y + 1.0 * Point3.Y;

  h.X := b.X * b.X - a.X * c.X;
  h.Y := b.Y * b.Y - a.Y * c.Y;

  if h.X > 0.0 then
  begin
    h.X := Sqrt(h.X);

    t := (-b.X - h.X) / a.X;
    if (t > 0.0) and (t < 1.0) then
    begin
      s := 1.0 - t;
      q := s * s * s * aStartPoint.x
        + 3.0 * s * s * t * Point1.x
        + 3.0 * s * t * t * Point2.x
        + t * t * t * Point3.x;
      UpdateBoundsX(q);
    end;

    t := (-b.X + h.X) / a.X;
    if (t > 0.0) and (t < 1.0) then
    begin
      s := 1.0 - t;
      q := s * s * s * aStartPoint.x
        + 3.0 * s * s * t * Point1.x
        + 3.0 * s * t * t * Point2.x
        + t * t * t * Point3.x;
      UpdateBoundsX(q);
    end;
  end;

  if h.Y > 0.0 then
  begin
    h.Y := Sqrt(h.Y);

    t := (-b.Y - h.Y) / a.Y;
    if (t > 0.0) and (t < 1.0) then
    begin
      s := 1.0 - t;
      q := s * s * s * aStartPoint.Y
        + 3.0 * s * s * t * Point1.Y
        + 3.0 * s * t * t * Point2.Y
        + t * t * t * Point3.Y;
      UpdateBoundsY(q);
    end;

    t := (-b.Y + h.Y) / a.Y;
    if (t > 0.0) and (t < 1.0) then
    begin
      s := 1.0 - t;
      q := s * s * s * aStartPoint.Y
        + 3.0 * s * s * t * Point1.Y
        + 3.0 * s * t * t * Point2.Y
        + t * t * t * Point3.Y;
      UpdateBoundsY(q);
    end;
  end;
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

{ TSVGQuadSegment }

procedure TSVGQuadSegment.ApplyMatrix(const aMatrix: TSVGMatrix);
begin
  Point1 := TransformPoint(Point1, aMatrix);
  Point2 := TransformPoint(Point2, aMatrix);
end;

// https://www.iquilezles.org/www/articles/bezierbbox/bezierbbox.htm
function TSVGQuadSegment.CalcBBox(const aStartPoint: TSVGPoint): TSVGRect;
var
  t, s, q: TSVGPoint;

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

  function Clamp(const aValue, aMin, aMax: TSVGFloat): TSVGFloat;
  begin
    if aValue < aMin then
      Result := aMin
    else
      if aValue > aMax then
        Result := aMax
      else
        Result := aValue;
  end;

begin
  Result.Left := aStartPoint.X;
  Result.Right := aStartPoint.X;
  Result.Top := aStartPoint.Y;
  Result.Bottom := aStartPoint.Y;

  UpdateBounds(Point2.X, Point2.Y);

  if (Point1.X < Result.Left) or (Point1.X > Result.Right)
  or (Point1.Y < Result.Top) or (Point1.Y > Result.Bottom) then
  begin
    t.X := Clamp((aStartPoint.X - Point1.X) / (aStartPoint.X - 2.0 * Point1.X + Point2.X), 0.0, 1.0);
    t.Y := Clamp((aStartPoint.Y - Point1.Y) / (aStartPoint.Y - 2.0 * Point1.Y + Point2.Y), 0.0, 1.0);

    s.X := 1.0 - t.X;
    s.Y := 1.0 - t.Y;

    q.X := s.X * s.X * aStartPoint.X + 2.0 * s.X * t.X * Point1.X + t.X * t.X * Point2.X;
    q.Y := s.Y * s.Y * aStartPoint.Y + 2.0 * s.Y * t.Y * Point1.Y + t.Y * t.Y * Point2.Y;

    UpdateBounds(q.X, q.Y);
  end;
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

{ TSVGArcSegment }

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
{var
  R: TSVGPoint;
  x1Prime, y1Prime: TSVGFloat;
  Radicant, Ratio, Factor: TSVGFloat;
  cxPrime, cyPrime, cx, cy: TSVGFloat;
  xmin, xmax, ymin, ymax: TSVGFloat;
  txmin, txmax, tymin, tymax: TSVGFloat;}

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

  {function getAngle(const bx, by: TSVGFloat): TSVGFloat;
  var
    Sign: TSVGFloat;
  begin
    if by > 0.0 then
      Sign := 1.0
    else
      Sign := -1.0;

    Result := (2*PI + Sign * ArcCos( bx / Sqrt(bx * bx + by * by))) mod (2*PI);
  end;}

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

  { TODO

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

  x1Prime := cos(Angle)*(aStartPoint.X - Point.X)/2 + sin(Angle)*(aStartPoint.y - Point.Y)/2;
  y1Prime := -sin(Angle)*(aStartPoint.X - Point.X)/2 + cos(Angle)*(aStartPoint.y - Point.Y)/2;

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

  cx := cxPrime*cos(Angle) - cyPrime*sin(Angle) + (aStartPoint.X + Point.X)/2;
  cy := cxPrime*sin(Angle) + cyPrime*cos(Angle) + (aStartPoint.Y + Point.Y)/2;

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
      txmin := -ArcTan2(R.Y*Tan(Angle)/R.X);
      txmax = PI - ArcTan2(R.Y*Tan(Angle)/R.X);
      xmin = cx + R.X*cos(txmin)*cos(Angle) - R.Y*sin(txmin)*sin(Angle);
      xmax = cx + R.X*cos(txmax)*cos(Angle) - R.Y*sin(txmax)*sin(Angle);
      if (xmin > xmax) then
      begin
        Swap(xmin,xmax);
        Swap(txmin,txmax);
      end;
      double tmpY = cy + RadiusX*cos(txmin)*sin(Angle) + RadiusY*sin(txmin)*cos(Angle);
      txmin = getAngle(xmin - cx, tmpY - cy);
      tmpY = cy + RadiusX*cos(txmax)*sin(Angle) + RadiusY*sin(txmax)*cos(Angle);
      txmax = getAngle(xmax - cx, tmpY - cy);


      tymin = atan(RadiusY/(tan(Angle)*RadiusX));
      tymax = atan(RadiusY/(tan(Angle)*RadiusX))+M_PI;
      ymin = cy + RadiusX*cos(tymin)*sin(Angle) + RadiusY*sin(tymin)*cos(Angle);
      ymax = cy + RadiusX*cos(tymax)*sin(Angle) + RadiusY*sin(tymax)*cos(Angle);
      if (ymin > ymax) then
      begin
        std::swap(ymin,ymax);
        std::swap(tymin,tymax);
      end;
      double tmpX = cx + RadiusX*cos(tymin)*cos(Angle) - RadiusY*sin(tymin)*sin(Angle);
      tymin = getAngle(tmpX - cx, ymin - cy);
      tmpX = cx + RadiusX*cos(tymax)*cos(Angle) - RadiusY*sin(tymax)*sin(Angle);
      tymax = getAngle(tmpX - cx, ymax - cy);
    end;

  double angle1 = getAngle(aStartPoint - cx, y1 - cy);
  double angle2 = getAngle(Point - cx, y2 - cy);

  if (!sweep)
    std::swap(angle1, angle2);

  bool otherArc = false;
  if (angle1 > angle2) then
  begin
    std::swap(angle1, angle2);
    otherArc = true;
  end;

  if ((!otherArc && (angle1 > txmin || angle2 < txmin)) || (otherArc && !(angle1 > txmin || angle2 < txmin)))
    xmin = aStartPoint < Point ? aStartPoint : Point;
  if ((!otherArc && (angle1 > txmax || angle2 < txmax)) || (otherArc && !(angle1 > txmax || angle2 < txmax)))
    xmax = aStartPoint > Point ? aStartPoint : Point;
  if ((!otherArc && (angle1 > tymin || angle2 < tymin)) || (otherArc && !(angle1 > tymin || angle2 < tymin)))
    ymin = y1 < y2 ? y1 : y2;
  if ((!otherArc && (angle1 > tymax || angle2 < tymax)) || (otherArc && !(angle1 > tymax || angle2 < tymax)))
    ymax = y1 > y2 ? y1 : y2;

  }
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

{ TSVGPathFigure }

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

{ TSVGPathGeometry }

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

          if QuadSegment.IsRelative then
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
             P1.X, P2.Y],
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

    Parser.ParsePath(Self, LastPoint);
  finally
    Parser.Free;
  end;
end;

procedure TSVGPathGeometry.SetTransform(const Value: TSVGMatrix);
begin
  FTransform := Value;
end;

end.
