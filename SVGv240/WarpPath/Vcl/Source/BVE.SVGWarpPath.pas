unit BVE.SVGWarpPath;

// -----------------------------------------------------------------------------
//
//  Path transformation demo
//
//  B.J.H. Verhue 13-2-2022
//
// -----------------------------------------------------------------------------

// The SVG control package is needed to compile this.
// You can also compile with the free demo package, which can be downloaded
// from here: https://www.bverhue.nl/delphisvg

interface
uses
  System.SysUtils,
  Xml.XMLDom,
  XML.XMLIntf,
  BVE.SVG2Types;

type
  TProcWarp = reference to function(const aPoint: TSVGPoint): TSVGPoint;

  procedure WarpPaths(const aDocumentElement: IXMLNode;
    aWarpProc: TProcWarp);

  function Clamp(const aValue, aMin, aMax: TSVGFloat): TSVGFloat;
  function Normalize(const aPt: TSVGPoint; aBounds: TSVGRect): TSVGPoint;


implementation
uses
  BVE.SVG2Intf,
  BVe.SVG2PathData;

const
  MaxError = 0.001;
  MaxIterationCount = 2000;

var
  IterationCount: Integer;

procedure CalculateBezierCoefficients(const aBezier: TSVGCubicBezier; out A, B, C: TSVGPoint);
begin
  C.X := 3.0 * (aBezier.C1.X - aBezier.P1.X);
  C.Y := 3.0 * (aBezier.C1.Y - aBezier.P1.Y);
  B.X := 3.0 * (aBezier.C2.X - aBezier.C1.X) - C.X;
  B.Y := 3.0 * (aBezier.C2.Y - aBezier.C1.Y) - C.Y;
  A.X := aBezier.P2.X - aBezier.P1.X - C.X - B.X;
  A.Y := aBezier.P2.Y - aBezier.P1.Y - C.Y - B.Y;
end;

function PointOnBezier(const A, B, C, D: TSVGPoint; const t: TSVGFloat): TSVGPoint;
var
  t_t: TSVGFloat;
  t_t_t: TSVGFloat;
begin
  t_t := t * t;
  t_t_t := t_t * t;

  Result.X := A.X * t_t_t + B.X * t_t + C.X * t + D.X;
  Result.Y := A.Y * t_t_t + B.Y * t_t + C.Y * t + D.Y;
end;

function DistanceBetween2Pts(const aP0, aP1: TSVGPoint): TSVGFloat;
begin
  Result := Sqrt(
      (aP0.x - aP1.x) * (aP0.x - aP1.x)
    + (aP0.Y - aP1.Y) * (aP0.Y - aP1.Y));
end;

procedure SubdivideCubic(const aCubic: TSVGCubicBezier; const t: TSVGFloat;
  out aLeft, aRight: TSVGCubicBezier);
var
  U, V: TSVGFloat;
  B, S, F, C, E, D: TSVGPoint;
begin
  U := 1 - t;
  V := t;

  B := aCubic.P1 * U + aCubic.C1 * V;
  S := aCubic.C1 * U + aCubic.C2 * V;
  F := aCubic.C2 * U + aCubic.P2 * V;

  C := B * U + S * V;
  E := S * U + F * V;
  D := C * U + E * V;

  aLeft.P1 := aCubic.P1;
  aLeft.C1 := B;
  aLeft.C2 := C;
  aLeft.P2 := D;

  aRight.P1 := D;
  aRight.C1 := E;
  aRight.C2 := F;
  aRight.P2 := aCubic.P2;
end;

function Normalize(const aPt: TSVGPoint; aBounds: TSVGRect): TSVGPoint;
begin
  Result := SVGPoint(
    (aPt.X - aBounds.Left) / aBounds.Width,
    (aPt.Y - aBounds.Top) / aBounds.Height);
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

procedure InterpolateCubic(aCubic: TSVGCubicBezier;
  aDest: ISVGPathDataUtil; aWarpProc: TProcWarp);
var
  M, M1, M2, A, B, C: TSVGPoint;
  CubicW, Cubic1, Cubic2: TSVGCubicBezier;
  D: TSVGFloat;
begin
  Inc(IterationCount);
  if IterationCount > MaxIterationCount then
    Exit;

  // Calculate the midpoint of the cubic bezier

  CalculateBezierCoefficients(aCubic, A, B, C);
  M := PointOnBezier(A, B, C, aCubic.P1, 0.5);

  // Warp the midpoint

  M1 := aWarpProc(M);

  // Calculate the warped cubic bezier

  CubicW.P1 := aWarpProc(aCubic.P1);
  CubicW.C1 := aWarpProc(aCubic.C1);
  CubicW.C2 := aWarpProc(aCubic.C2);
  CubicW.P2 := aWarpProc(aCubic.P2);

  // Calculate the midpoint of the warped cubic bezier

  CalculateBezierCoefficients(CubicW, A, B, C);
  M2 := PointOnBezier(A, B, C, CubicW.P1, 0.5);

  // Calculate the error

  D := DistanceBetween2Pts(M1, M2);

  if D > MaxError then
  begin
    // The error is too big, so split the cubic bezier and continue on the
    // two new cubic beziers

    SubdivideCubic(aCubic, 0.5, Cubic1, Cubic2);

    InterpolateCubic(Cubic1, aDest, aWarpProc);
    InterpolateCubic(Cubic2, aDest, aWarpProc);
  end else begin
    // Error is small enough, add the warped cubic bezier to the pathpoint list

    aDest.Data.Add(TSVGPathPoint.Create(ppCurveTo, CubicW.C1));
    aDest.Data.Add(TSVGPathPoint.Create(ppCurveTo, CubicW.C2));
    aDest.Data.Add(TSVGPathPoint.Create(ppCurveTo, CubicW.P2));
  end;
end;

procedure InterpolateLine(aP0, aP1: TSVGPoint; aDest: ISVGPathDataUtil;
  aWarpProc: TProcWarp);
var
  M, M1, M2, P2: TSVGPoint;
  D: TSVGFloat;
begin
  Inc(IterationCount);
  if IterationCount > MaxIterationCount then
    Exit;

  // Calculate the midpoint of the line

  M := TSVGPoint.Interpolate(0.5, aP0, aP1);

  // Warp the midpoint

  M1 := aWarpProc(M);

  // Calculate the warped line, and calculate the midpoint

  P2 := aWarpProc(aP1);
  M2 := TSVGPoint.Interpolate(0.5, aWarpProc(aP0), P2);

  // Calculate the error
  D := DistanceBetween2Pts(M1, M2);
  if D > MaxError then
  begin
    // Too bit, split the line

    InterpolateLine(aP0, M, aDest, aWarpProc);
    InterpolateLine(M, aP1, aDest, aWarpProc);
  end else
    // Small enough, add to the pathpoint list

    aDest.Data.Add(TSVGPathPoint.Create(ppLineTo, P2));
end;

procedure WarpPaths(const aDocumentElement: IXMLNode;
  aWarpProc: TProcWarp);

  procedure DoPath(aParentDomNode: IDOMNode; aPath: ISVGPath);
  var
    i: Integer;
    Pt, PrevPt: TSVGPoint;
    Cubic: TSVGCubicBezier;
    PathData, NewPathData: ISVGPathDataUtil;
  begin
    // Convert the d attribute to a pathpoint list

    PathData := TSVGPathDataUtil.Create;
    PathData.ParseSVGPathString(aPath.Attributes['d']);

    // Here we use the cache to get the local transformation matrix.
    // We transform all the points in the pathpoint list with this matrix.

    if aPath.CacheList.Count > 0 then
      PathData.ApplyMatrix(aPath.CacheList[0].UserMatrix);

    // Create a new pathpoint list to hold the warped points.

    NewPathData := TSVGPathDataUtil.Create;

    // Loop through the segments of the pathpoint list.

    i := 0;
    while i < PathData.Data.Count do
    begin
      case PathData.Data[i].Kind of
        ppMoveTo:
          begin
            Pt := PathData.Data[i].Point;
            NewPathData.Data.Add(TSVGPathPoint.Create(ppMoveTo, aWarpProc(Pt)));
          end;

        ppLineTo:
          begin
            Pt := PathData.Data[i].Point;

            IterationCount := 0;
            InterpolateLine(PrevPt, Pt, NewPathData, aWarpProc);
          end;

        ppCurveTo:
          begin
            Cubic.P1 := PrevPt;

            Cubic.C1 := PathData.Data[i].Point;

            Inc(i);
            Cubic.C2 := PathData.Data[i].Point;

            Inc(i);
            Pt := PathData.Data[i].Point;
            Cubic.P2 := Pt;

            IterationCount := 0;
            InterpolateCubic(Cubic, NewPathData, aWarpProc);
          end;

        ppClose:
          begin
            NewPathData.Data.Add(PathData.Data[i]);
          end;
      end;

      PrevPt := Pt;
      Inc(i);
    end;

    // Convert the warped pathpoint list back to a string.

    aPath.setAttribute('d', NewPathData.GetSVGPathString);

    // Since we already applied all transforms on the path points, we can
    // remove the transform attribute from the new path elment.

    aPath.Attributes['transform'] := '';
  end;

  procedure DoNode(aParentDomNode: IDOMNode; aNode: IXMLNode);
  var
    i: Integer;
    DomNode: IDOMNode;
    Path: ISVGPath;
  begin
    // Recursively process the childnodes

    for i := 0 to aNode.ChildNodes.Count - 1 do
    begin
      // If the node is a path element, than warp the path's d attribute

      if Supports(aNode.ChildNodes[i], ISVGPath, Path) then
        DoPath(DomNode, Path)
      else
        DoNode(DomNode, aNode.ChildNodes[i]);
    end;
  end;

begin
  DoNode(nil, aDocumentElement);
end;


end.
