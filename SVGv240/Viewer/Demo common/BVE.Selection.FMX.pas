unit BVE.Selection.FMX;

// [The "BSD licence"]
//
//  Copyright (c) 2013 Bruno Verhue
//  All rights reserved.
//
//  Redistribution and use in source and binary forms, with or without
//  modification, are permitted provided that the following conditions
//  are met:
//  1. Redistributions of source code must retain the above copyright
//     notice, this list of conditions and the following disclaimer.
//  2. Redistributions in binary form must reproduce the above copyright
//     notice, this list of conditions and the following disclaimer in the
//     documentation and/or other materials provided with the distribution.
//  3. The name of the author may not be used to endorse or promote products
//     derived from this software without specific prior written permission.
//
//  THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
//  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
//  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
//  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
//  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
//  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
//  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
//  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
//  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
//  THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

/// <summary>
///   Unit containing a set of classes for handling graphic controls on screen
/// </summary>

{$Include '..\Common\CompilerSettings.inc'}

interface
uses
  System.Types,
  System.UITypes,
  System.Classes,
{$IFDEF VER270Up}
  System.Math.Vectors,
{$ENDIF}
  System.Generics.Collections,
  FMX.Types,
  FMX.Controls,
{$IFDEF VER260Up}
  FMX.Graphics,
{$ENDIF}
  FMX.Objects,
  FMX.Layouts;

type
  TMarkerType = (mtPoint, mtCross, mtRect, mtCircle);

  TSelectControl = class;

  TMarker = class(TControl)
  private
    FMarkerType: TMarkerType;
    FColor: TAlphaColor;
    FDimension: single;
    FMouseDown: TPointF;
    procedure SetColor(const Value: TAlphaColor);
    procedure SetMarkerType(const Value: TMarkerType);
    procedure SetDimension(const Value: single);
  protected
    function GetPoint: TPointF; virtual; abstract;
    procedure SetPoint(const Value: TPointF); virtual; abstract;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;

    procedure Draw(aCanvas : TCanvas);

    procedure UpdateBounds;

    procedure Paint; override;
    property Point: TPointF read GetPoint write SetPoint;
  published
    property Dimenstion: single read FDimension write SetDimension;
    property MarkerType: TMarkerType read FMarkerType write SetMarkerType;
    property Color: TAlphaColor read FColor write SetColor;
  end;

  TBoundsMarker = class( TMarker)
  private
    FIndex: integer;
    FSelectControl: TSelectControl;
  protected
    function GetPoint: TPointF; override;
    procedure SetPoint(const Value: TPointF); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;

    procedure SetPointIndex(const aIndex : integer);
  end;

  TSelectionList = class(TObjectList<TSelectControl>)
  private
    FActive : boolean;
    FPosX, FPosY : single;
  public
    constructor Create(AOwnsObjects: Boolean);
    destructor Destroy; override;

    procedure MoveDelta(const dx, dy : single);
    procedure UnselectAll;

    property Active: boolean read FActive write FActive;
    property PosX: single read FPosX write FPosX;
    property Posy: single read FPosY write FPosY;
  end;

  TSelectControl = class(TControl)
  private
    FMargin: single;
    FMarkerMoving: boolean;
    FBoundsMarkerList: TList<TBoundsMarker>;
    FBoundsMarkersVisible: boolean;
    FMouseDown: TPointF;
    FSelected: boolean;
    FMoveable: boolean;
    FContent: TLayout;
    FSelectionList: TSelectionList;
    function GetTag: smallint;
    procedure SetTag(const Value : smallint);
    function GetBoundsMarkersVisible: boolean;
    procedure SetBoundsMarkersVisible(const Value: boolean);
    function GetBoundsMarkerPoint(aIndex: integer): TPointF;
    procedure SetBoundsMarkerPoint(aIndex: integer; const Value: TPointF);
    procedure SetMargin(const Value: single);
    procedure SetMoveable(const Value: boolean);
  protected
    function GetBoundingRect: TRectF;
    function GetAbsoluteClientRect: TRectF;
  public
    constructor Create(AOwner: TComponent; aSelectionList : TSelectionList); reintroduce; virtual;
    destructor Destroy; override;

    procedure CreateBoundsMarkers;
    procedure ClearBoundsMarkers;

    procedure UpdateBoundsMarkers;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;

    procedure Paint; override;
    procedure PaintChildren; override;

    procedure Resize; override;

    procedure Select;
    procedure Deselect;

    procedure MoveDelta(const dx, dy: single);
    procedure ResizeClientRect(const dw, dh: single);
    procedure SetPosition(const aValue: TPointF);
    procedure SetSize(const aValue: TRectF);

    property BoundsMarkerPoint[index : integer]: TPointF read GetBoundsMarkerPoint write SetBoundsMarkerPoint;
  published
    property BoundsMarkersVisible: boolean read GetBoundsMarkersVisible write SetBoundsMarkersVisible;
    property AbsoluteClientRect: TRectF read GetAbsoluteClientRect;
    property Margin: single read FMargin write SetMargin;
    property Selected: boolean read FSelected;
    property Moveable: boolean read FMoveable write SetMoveable;
    property Content: TLayout read FContent;
    property Tag: smallint read GetTag write SetTag;
  end;

implementation

{ TMarker }

constructor TMarker.Create(AOwner : TComponent);
begin
  inherited;

  Hittest := True;
  AutoCapture := True;
  FDimension := 4;

  FMarkerType := mtCircle;
  FColor := TAlphaColorRec.Black;
end;

destructor TMarker.Destroy;
begin
  inherited;
end;

procedure TMarker.Draw(aCanvas: TCanvas);
var Save: TCanvasSaveState;
    p1: TPointF;
begin
  Save := Canvas.SaveState;
  try
    case FMarkerType of
      mtPoint:
        begin
          aCanvas.Fill.Color := FColor;
          aCanvas.Fill.Kind := TBrushKind.bkSolid;
          aCanvas.Stroke.Kind := TBrushKind.bkNone;
          p1 := Point;
          aCanvas.FillEllipse(RectF(0, 0, Width, Height), 1);
        end;
      mtCross:
        begin
          aCanvas.Fill.Kind := TBrushKind.bkNone;
          aCanvas.Stroke.Color := FColor;
          aCanvas.Stroke.Kind := TBrushKind.bkSolid;
          p1 := Point;
          aCanvas.DrawLine(PointF(0, 0), PointF( Width, Height), 1);
          aCanvas.DrawLine(PointF(0, Height), PointF( Width, 0), 1 );

        end;
      mtRect:
        begin
          aCanvas.Fill.Color := FColor;
          aCanvas.Fill.Kind := TBrushKind.bkSolid;
          //aCanvas.Fill.Kind := TBrushKind.None;
          //aCanvas.Stroke.Color := FColor;
          aCanvas.Stroke.Kind := TBrushKind.bkSolid;
          p1 := Point;
          aCanvas.FillRect(RectF(0, 0, Width, Height), 0, 0, [], 1);
        end;
      mtCircle:
        begin
          aCanvas.Fill.Kind := TBrushKind.bkNone;
          aCanvas.Stroke.Color := FColor;
          aCanvas.Stroke.Kind := TBrushKind.bkSolid;
          p1 := Point;
          aCanvas.DrawEllipse(RectF(0, 0, Width, Height), 1);
        end;
    end;
  finally
    aCanvas.RestoreState(Save);
  end;
end;

procedure TMarker.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  if ssLeft in Shift then begin
    FMouseDown := PointF( X, Y);
  end;

  inherited;
end;

procedure TMarker.MouseMove(Shift: TShiftState; X, Y: Single);
var dx, dy : single;
    p : TPointF;
begin
  if ssLeft in Shift then begin
    dx := X - FMouseDown.X;
    dy := Y - FMouseDown.Y;

    p := Point;
    p.X := p.X + dx;
    p.Y := p.Y + dy;
    Point := p;
  end;
  inherited;
end;

procedure TMarker.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;
end;

procedure TMarker.Paint;
begin
  Canvas.BeginScene;
  try
    Draw( Canvas);
  finally
    Canvas.EndScene;
  end;
  inherited;
end;

procedure TMarker.SetColor(const Value: TAlphaColor);
begin
  if FColor <> Value then begin
    FColor := Value;
    Repaint;
  end;
end;

procedure TMarker.SetDimension(const Value: single);
begin
  FDimension := Value;
  Repaint;
end;

procedure TMarker.SetMarkerType(const Value: TMarkerType);
begin
  if FMarkerType <> Value then begin
    FMarkerType := Value;
    Repaint;
  end;
end;

procedure TMarker.UpdateBounds;
var p : TPointF;
begin
  p := GetPoint;
  SetBounds( p.X - FDimension,
             p.Y - FDimension,
             FDimension * 2,
             FDimension * 2);
end;

{ TBoundsMarker }

constructor TBoundsMarker.Create(AOwner: TComponent);
begin
  inherited;
  FIndex := 0;
end;

destructor TBoundsMarker.Destroy;
begin
  inherited;
end;

function TBoundsMarker.GetPoint: TPointF;
begin
  if assigned(FSelectControl) then begin
    Result := FSelectControl.BoundsMarkerPoint[ FIndex];
  end else
    Result := PointF(0,0);
end;

procedure TBoundsMarker.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

  if ssLeft in Shift then begin
    //if assigned(FSelectControl) then
    //  FSelectControl.NotifyObservers( evtBeforeMoveMarker, self);
  end;
end;

procedure TBoundsMarker.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

  //if assigned(FSelectControl) then
  //  FSelectControl.NotifyObservers( evtAfterMoveMarker, self);
end;

procedure TBoundsMarker.SetPointIndex(const aIndex: integer);
begin
  FIndex := aIndex;
end;

procedure TBoundsMarker.SetPoint(const Value: TPointF);
begin
  if assigned(FSelectControl) then begin
    FSelectControl.BoundsMarkerPoint[ FIndex] := Value;
  end;
end;

{ TSelectControl }

constructor TSelectControl.Create(AOwner: TComponent; aSelectionList : TSelectionList);
begin
  inherited Create(AOwner);

  FContent := TLayout.Create(self);
  FContent.Parent := self;
  FContent.ClipChildren := True;

  FBoundsMarkerList := TList<TBoundsMarker>.Create;

  FBoundsMarkersVisible := True;

  FMarkerMoving := False;

  FMargin := 10;

  FSelectionList := aSelectionList;
  FSelectionList.Add( self);
  FSelected := False;

  Hittest := True;
  //AutoCapture := True;

  FMoveable := True;
end;

procedure TSelectControl.CreateBoundsMarkers;
var BoundsMarker: TBoundsMarker;
    i: integer;
begin
  ClearBoundsMarkers;
  for i := 0 to 3 do begin
    BoundsMarker := TBoundsMarker.Create(self);
    BoundsMarker.Tag := i;
    BoundsMarker.MarkerType := mtCircle;
    BoundsMarker.Color := TAlphaColorRec.Blue;
    BoundsMarker.SetPointIndex( i);
    BoundsMarker.FSelectControl := self;
    BoundsMarker.Parent := self;
    BoundsMarker.Visible := FBoundsMarkersVisible;
    FBoundsMarkerList.Add(BoundsMarker);
    UpdateBoundsMarkers;
  end;
end;

procedure TSelectControl.Deselect;
begin
  FSelected := False;
  Repaint;
end;

destructor TSelectControl.Destroy;
begin
  if assigned(FSelectionList) then
    FSelectionList.Remove(Self);

  ClearBoundsMarkers;

  FBoundsMarkerList.Free;
  inherited;
end;

function TSelectControl.GetBoundingRect: TRectF;
begin
  inherited;
end;

function TSelectControl.GetBoundsMarkerPoint( aIndex: integer): TPointF;
var R: TRectF;
begin
  R := AbsoluteClientRect;
  case aIndex of
  0 : Result := PointF( R.Left, R.Top);
  1 : Result := PointF( R.Right, R.Top);
  2 : Result := PointF( R.Right, R.Bottom);
  3 : Result := PointF( R.Left, R.Bottom);
  end;
end;

function TSelectControl.GetBoundsMarkersVisible: boolean;
begin
  Result := FBoundsMarkersVisible;
end;

function TSelectControl.GetAbsoluteClientRect: TRectF;
begin
  Result.Left := FMargin;
  Result.Top := FMargin;
  Result.Width := Width - FMargin*2;
  Result.Height := Height - FMargin*2;
end;

function TSelectControl.GetTag: smallint;
begin
  Result := Tag;
end;

procedure TSelectControl.ClearBoundsMarkers;
var Marker: TBoundsMarker;
begin
  for Marker in FBoundsMarkerList do
    Marker.Free;
  FBoundsMarkerList.Clear;
end;

procedure TSelectControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  if ssLeft in Shift then begin
    FMouseDown := PointF( X, Y);
  end;

  if not( ssShift in Shift)  then
    if assigned(FSelectionList) then
       FSelectionList.UnselectAll;

  Select;

  inherited;
end;

procedure TSelectControl.MouseMove(Shift: TShiftState; X, Y: Single);
var dx, dy: single;
begin
  if ssLeft in Shift then begin
    dx := X - FMouseDown.X;
    dy := Y - FMouseDown.Y;

    if FMoveable and Selected then
      if assigned(FSelectionList) then
        FSelectionList.MoveDelta( dx, dy)
      else
        MoveDelta( dx, dy);
  end;

  inherited;
end;

procedure TSelectControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

end;

procedure TSelectControl.MoveDelta(const dx, dy: single);
begin
  if not FMoveable then
    exit;

  Position.X := Position.X + dx;
  Position.Y := Position.Y + dy;
  UpdateBoundsMarkers;
end;

procedure TSelectControl.Resize;
begin
  inherited;

  FContent.SetBounds(FMargin, FMargin, Width - FMargin * 2, Height - FMargin * 2);

  if not FMarkerMoving then begin
    if FBoundsMarkerList.Count = 0 then
      CreateBoundsMarkers
    else
      UpdateBoundsMarkers;
  end;
end;

procedure TSelectControl.ResizeClientRect(const dw, dh: single);
begin
  if (dw = 0) and (dh = 0) then
    exit;

  Width := Width + dw;
  Height := Height + dh;
  UpdateBoundsMarkers;
end;

procedure TSelectControl.Paint;
var State : TCanvasSaveState;
begin
  State := Canvas.SaveState;
  Canvas.BeginScene;
  try
    if FBoundsMarkersVisible then begin
{$IFDEF Ver230Down}
      Canvas.StrokeDash := TStrokeDash.sdDash;
{$ELSE}
      Canvas.Stroke.Dash := TStrokeDash.sdDash;
{$ENDIF}
      if FSelected then
        Canvas.Stroke.Color := TAlphaColorRec.Blue
      else
        Canvas.Stroke.Color := TAlphaColorRec.Gray;

      Canvas.DrawRect( AbsoluteClientRect, 0, 0, [], 1);
    end;
  finally
    Canvas.EndScene;
    Canvas.RestoreState(State);
  end;
end;

procedure TSelectControl.PaintChildren;
var SaveMatrix: TMatrix;
begin
  SaveMatrix := Canvas.Matrix;
  try
    inherited;
  finally
    Canvas.SetMatrix( SaveMatrix);
  end;
end;

procedure TSelectControl.Select;
begin
  FSelected := True;
  Repaint;
end;

procedure TSelectControl.SetBoundsMarkerPoint( aIndex: integer;
  const Value: TPointF);
var R: TRectF;
begin
  R := AbsoluteClientRect;
  case aIndex of
    0 : SetSize(
            RectF( Value.X,
                   Value.Y,
                   R.Right,
                   R.Bottom));
    1 : SetSize(
            RectF( R.Left,
                   Value.Y,
                   Value.X,
                   R.Bottom));
    2 : SetSize(
           RectF( R.Left,
                  R.Top,
                  Value.X,
                  Value.Y));
    3 : SetSize(
           RectF( Value.X,
                  R.Top,
                  R.Right,
                  Value.Y));
  end;
end;

procedure TSelectControl.SetBoundsMarkersVisible(const Value: boolean);
var Marker : TBoundsMarker;
begin
  if Value <> FBoundsMarkersVisible then begin
    FBoundsMarkersVisible := Value;
    for Marker in FBoundsMarkerList do
      Marker.Visible := FBoundsMarkersVisible;
    Repaint;
  end;
end;

procedure TSelectControl.SetMargin(const Value: single);
begin
  FMargin := Value;
  Repaint;
end;

procedure TSelectControl.SetMoveable(const Value: boolean);
begin
  FMoveable := Value;
end;

procedure TSelectControl.SetPosition(const aValue: TPointF);
var R : TRectF;
begin
  FMarkerMoving := True;
  try
    R := AbsoluteCLientRect;
    MoveDelta( aValue.X - R.Left, aValue.Y - R.Top);
  finally
    FMarkerMoving := False;
  end;
end;

procedure TSelectControl.SetSize(const aValue: TRectF);
var R : TRectF;
    dx, dy, dw, dh : single;
begin
  FMarkerMoving := True;
  try
    R := AbsoluteCLientRect;
    dx := aValue.Left - R.Left;
    dy := aValue.Top - R.Top;
    dw := aValue.Right - R.Right - dx;
    dh := aValue.Bottom - R.Bottom - dy;

    MoveDelta( dx, dy);
    ResizeClientRect( dw, dh);
  finally
    FMarkerMoving := False;
  end;
end;

procedure TSelectControl.SetTag(const Value: smallint);
begin
  Tag := Value;
end;

procedure TSelectControl.UpdateBoundsMarkers;
var BoundsMarker : TBoundsMarker;
begin
  BeginUpdate;
  try
    for BoundsMarker in FBoundsMarkerList do
      BoundsMarker.UpdateBounds;
  finally
    EndUpdate;
  end;
end;

{ TSelectionList }

constructor TSelectionList.Create(AOwnsObjects: Boolean);
begin
  inherited;
end;

destructor TSelectionList.Destroy;
var
  i : integer;
begin
  for i := 0 to Count - 1 do
    Items[i].FSelectionList := nil;

  inherited;
end;

procedure TSelectionList.MoveDelta(const dx, dy: single);
var
  i : integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].Selected then
      Items[i].MoveDelta( dx, dy);
end;

procedure TSelectionList.UnselectAll;
var i : integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Deselect;
end;

end.
