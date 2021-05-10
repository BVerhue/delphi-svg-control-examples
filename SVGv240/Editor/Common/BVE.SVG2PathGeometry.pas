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
  BVE.SVG2Types,
  BVE.SVG2Intf,
  BVE.SVG2ParseUtility,
  BVE.SVG2PathData;

type
  ISVGPathSegment = interface
    ['{CE946139-3E3E-42A7-A577-7E89D3218F54}']
    function GetIsRelative: Boolean;
    procedure SetIsRelative(const Value: Boolean);

    property IsRelative: Boolean read GetIsRelative write SetIsRelative;
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
    function GetRotationAngle: TSVGFloat;
    function GetRadius: TSVGPoint;
    function GetSweepDirection: Boolean;
    procedure SetIsLargeArc(const Value: Boolean);
    procedure SetPoint(const Value: TSVGPoint);
    procedure SetRotationAngle(const Value: TSVGFloat);
    procedure SetRadius(const Value: TSVGPoint);
    procedure SetSweepDirection(const Value: Boolean);

    property IsLargeArc: Boolean read GetIsLargeArc write SetIsLargeArc;
    property Point: TSVGPoint read GetPoint write SetPoint;
    property RotationAngle: TSVGFloat read GetRotationAngle write SetRotationAngle;
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

    property IsClosed: Boolean read GetIsClosed write SetIsClosed;
    property StartPoint: TSVGPoint read GetStartPoint write SetStartPoint;
    property Segments: TList<ISVGPathSegment> read GetSegments;
  end;

  ISVGPathGeometry = interface
    ['{44CD989C-3B9A-4212-86ED-D1ED0B9DEA24}']
    function GetAsString: TSVGUnicodeString;
    function GetFigures: TList<ISVGPathFigure>;
    procedure SetAsString(const Value: TSVGUnicodeString);

    procedure ConvertToPathData(aSink: ISVGPathDataSink);

    property AsString: TSVGUnicodeString read GetAsString write SetAsString;
    property Figures: TList<ISVGPathFigure> read GetFigures;
  end;

  TSVGPathSegment = class(TInterfacedObject, ISVGPathSegment)
  private
    FIsRelative: Boolean;
  protected
    function GetIsRelative: Boolean;
    procedure SetIsRelative(const Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;

    property IsRelative: Boolean read GetIsRelative write SetIsRelative;
  end;

  TSVGLineSegment = class(TSVGPathSegment, ISVGLineSegment)
  private
    FPoint: TSVGPoint;
  protected
    function GetPoint: TSVGPoint;
    procedure SetPoint(const Value: TSVGPoint);
  public
    constructor Create;
    destructor Destroy; override;

    property Point: TSVGPoint read GetPoint write SetPoint;
  end;

  TSVGBezierSegment = class(TSVGPathSegment, ISVGBezierSegment)
  private
    FPoint1: TSVGPoint;
    FPoint2: TSVGPoint;
    FPoint3: TSVGPoint;
  protected
    function GetPoint1: TSVGPoint;
    function GetPoint2: TSVGPoint;
    function GetPoint3: TSVGPoint;
    procedure SetPoint1(const Value: TSVGPoint);
    procedure SetPoint2(const Value: TSVGPoint);
    procedure SetPoint3(const Value: TSVGPoint);
  public
    constructor Create;
    destructor Destroy; override;

    property Point1: TSVGPoint read GetPoint1 write SetPoint1;
    property Point2: TSVGPoint read GetPoint2 write SetPoint2;
    property Point3: TSVGPoint read GetPoint3 write SetPoint3;
  end;

  TSVGQuadSegment = class(TSVGPathSegment, ISVGQuadSegment)
  private
    FPoint1: TSVGPoint;
    FPoint2: TSVGPoint;
  protected
    function GetPoint1: TSVGPoint;
    function GetPoint2: TSVGPoint;
    procedure SetPoint1(const Value: TSVGPoint);
    procedure SetPoint2(const Value: TSVGPoint);
  public
    constructor Create;
    destructor Destroy; override;

    property Point1: TSVGPoint read GetPoint1 write SetPoint1;
    property Point2: TSVGPoint read GetPoint2 write SetPoint2;
  end;

  TSVGArcSegment = class(TSVGPathSegment, ISVGArcSegment)
  private
    FIsLargeArc: Boolean;
    FPoint: TSVGPoint;
    FRotationAngle: TSVGFloat;
    FRadius: TSVGPoint;
    FSweepDirection: Boolean;
  protected
    function GetIsLargeArc: Boolean;
    function GetPoint: TSVGPoint;
    function GetRotationAngle: TSVGFloat;
    function GetRadius: TSVGPoint;
    function GetSweepDirection: Boolean;
    procedure SetIsLargeArc(const Value: Boolean);
    procedure SetPoint(const Value: TSVGPoint);
    procedure SetRotationAngle(const Value: TSVGFloat);
    procedure SetRadius(const Value: TSVGPoint);
    procedure SetSweepDirection(const Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;

    property IsLargeArc: Boolean read GetIsLargeArc write SetIsLargeArc;
    property Point: TSVGPoint read GetPoint write SetPoint;
    property RotationAngle: TSVGFloat read GetRotationAngle write SetRotationAngle;
    property Raidus: TSVGPoint read GetRadius write SetRadius;
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

    property IsClosed: Boolean read GetIsClosed write SetIsClosed;
    property StartPoint: TSVGPoint read GetStartPoint write SetStartPoint;
    property Segments: TList<ISVGPathSegment> read GetSegments;
  end;

  TSVGPathGeometry = class(TInterfacedObject, ISVGPathGeometry, ISVGPathDataSink)
  private
    FFigures: TList<ISVGPathFigure>;
    function GetLastFigure: ISVGPathFigure;
  protected
    function GetAsString: TSVGUnicodeString;
    function GetFigures: TList<ISVGPathFigure>;
    procedure SetAsString(const Value: TSVGUnicodeString);

    property LastFigure: ISVGPathFigure read GetLastFigure;
  public
    constructor Create;
    destructor Destroy; override;

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

    property Figures: TList<ISVGPathFigure> read GetFigures;
    property AsString: TSVGUnicodeString read GetAsString write SetAsString;
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

constructor TSVGLineSegment.Create;
begin
  inherited Create;

  FPoint := SVGPoint(0, 0);
end;

destructor TSVGLineSegment.Destroy;
begin
  inherited;
end;

function TSVGLineSegment.GetPoint: TSVGPoint;
begin
  Result := FPoint;
end;

procedure TSVGLineSegment.SetPoint(const Value: TSVGPoint);
begin
  if Value <> FPoint then
  begin
    FPoint := Value;
  end;
end;

{ TSVGBezierSegment }

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

function TSVGQuadSegment.GetPoint1: TSVGPoint;
begin
  Result := FPoint1;
end;

function TSVGQuadSegment.GetPoint2: TSVGPoint;
begin
  Result := FPoint2;
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

constructor TSVGArcSegment.Create;
begin
  inherited Create;

  FIsLargeArc := False;
  FPoint := SVGPoint(0, 0);
  FRotationAngle := 0;
  FRadius := SVGPoint(0, 0);
  FSweepDirection := False;
end;

destructor TSVGArcSegment.Destroy;
begin
  inherited;
end;

function TSVGArcSegment.GetIsLargeArc: Boolean;
begin
  Result := FIsLargeArc;
end;

function TSVGArcSegment.GetPoint: TSVGPoint;
begin
  Result := FPoint;
end;

function TSVGArcSegment.GetRotationAngle: TSVGFloat;
begin
  Result := FRotationAngle;
end;

function TSVGArcSegment.GetRadius: TSVGPoint;
begin
  Result := FRadius;
end;

function TSVGArcSegment.GetSweepDirection: Boolean;
begin
  Result := FSweepDirection;
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

procedure TSVGArcSegment.SetRotationAngle(const Value: TSVGFloat);
begin
  if FRotationAngle <> Value then
  begin
    FRotationAngle := Value;
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
  ArcSegment.RotationAngle := aAngle;
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
      LastPoint := Figure.StartPoint;
      aSink.MoveTo(LastPoint);

      for Segment in Figure.Segments do
      begin
        if Supports(Segment, ISVGLineSegment, LineSegment) then
        begin
          LastPoint := LineSegment.Point;
          aSink.LineTo(LastPoint);
        end else

        if Supports(Segment, ISVGBezierSegment, BezierSegment) then
        begin
          LastPoint := BezierSegment.Point3;

          aSink.CurveTo(
            BezierSegment.Point1,
            BezierSegment.Point2,
            LastPoint);
        end else

        if Supports(Segment, ISVGQuadSegment, QuadSegment) then
        begin
          aSink.AddQuadCurve(LastPoint, QuadSegment.Point1, QuadSegment.Point2);
          LastPoint := QuadSegment.Point2;
        end else

        if Supports(Segment, ISVGArcSegment, ArcSegment) then
        begin
          aSink.AddArc(
            LastPoint,
            ArcSegment.Radius,
            ArcSegment.RotationAngle,
            ArcSegment.IsLargeArc,
            ArcSegment.SweepDirection,
            ArcSegment.Point);

          LastPoint := ArcSegment.Point;
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
begin
  Result := '';

  StringBuilder := TBaseStringBuilder.Create;
  try
    for Figure in FFigures do
    begin
      LastCmd := 'L';
      StringBuilder.Append(Format('%s%g,%g', ['M', Figure.StartPoint.X, Figure.StartPoint.Y], USFormatSettings));

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

          StringBuilder.Append(Format('%g,%g', [LineSegment.Point.X, LineSegment.Point.Y], USFormatSettings));
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

          StringBuilder.Append(Format('%g,%g %g,%g %g,%g',
            [BezierSegment.Point1.X, BezierSegment.Point1.Y,
             BezierSegment.Point2.X, BezierSegment.Point2.Y,
             BezierSegment.Point3.X, BezierSegment.Point3.Y],
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

          StringBuilder.Append(Format('%g,%g %g,%g',
            [QuadSegment.Point1.X, QuadSegment.Point1.Y,
             QuadSegment.Point2.X, QuadSegment.Point2.Y],
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

          StringBuilder.Append(Format('%g,%g %g %d %d %g,%g',
            [ArcSegment.Radius.X, ArcSegment.Radius.Y,
             ArcSegment.RotationAngle,
             Integer(ArcSegment.IsLargeArc),
             Integer(ArcSegment.SweepDirection),
             ArcSegment.Point.X, ArcSegment.Point.Y],
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

end.
