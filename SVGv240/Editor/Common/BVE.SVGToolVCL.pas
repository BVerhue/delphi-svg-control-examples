unit BVE.SVGToolVCL;

// ------------------------------------------------------------------------------
//
//                          SVG Control Package 2.0
//                       Copyright (c) 2015 Bruno Verhue
//
// ------------------------------------------------------------------------------

// The SVG Editor needs at least version v2.40 update 9 of the SVG library

interface
uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.UITypes,
  System.Generics.Collections,
  System.Math,
  Vcl.Controls,
  BVE.SVG2Elements,
  BVE.SVG2Context,
  BVE.SVG2Types,
  BVE.SVG2Intf,
  BVE.SVG2Control.VCL;

const
  svg_tool =
    '<?xml version="1.0" standalone="no"?>'
    + '<svg width="%4.2f" height="%4.2f" xmlns="http://www.w3.org/2000/svg" version="1.1">'
    + '<rect x="%4.2f" y="%4.2f" width="%4.2f" height="%4.2f" fill="none" stroke="blue" stroke-width="1" />'
    + '</svg>';

  svg_tool_handle =
    '<?xml version="1.0" standalone="no"?>'
    + '<svg viewBox="0 0 100 100" xmlns="http://www.w3.org/2000/svg" version="1.1">'
    + '<rect x="0" y="0" width="100" height="100" fill="red" stroke="none" stroke-width="1" />'
    + '</svg>';

  svg_tool_line =
    '<?xml version="1.0" standalone="no"?>'
    + '<svg width="%4.2f" height="%4.2f" xmlns="http://www.w3.org/2000/svg" version="1.1">'
    + '<line x1="%4.2f" y1="%4.2f" x2="%4.2f" y2="%4.2f" fill="none" stroke="blue" stroke-width="1" />'
    + '</svg>';

type
  TSVGTool = class;

  TSVGToolHandle = class(TSVG2Control)
  private
    FSize: Integer;
    FMouseDown: TPoint;
    FIndex: integer;
    FTool: TSVGTool;
    FSVG: string;
  protected
    function GetIsFocused: Boolean;
    function GetPoint: TPoint; virtual;
    procedure SetPoint(const Value: TPoint); virtual;
  public
    constructor Create(aTool: TSVGTool; const aIndex: Integer; const aSize: Integer = 4;
      const aSVG: string = svg_tool_handle); reintroduce; virtual;
    destructor Destroy; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure SetFocus;

    procedure Resize; override;

    procedure UpdateBounds;

    property IsFocused: Boolean read GetIsFocused;
    property Point: TPoint read GetPoint write SetPoint;
  end;

  TSVGToolLine = class(TSVG2Control)
  private
    FMargin: Integer;
    FIndex1: integer;
    FIndex2: integer;
    FTool: TSVGTool;
    FP1, FP2: TPoint;
    FSVG: string;
  protected
    function GetPoint2: TPoint;
    function GetPoint1: TPoint; virtual;
  public
    constructor Create(aTool: TSVGTool; const aIndex1, aIndex2: Integer;
      const aSVG: string = svg_tool_line); reintroduce; virtual;
    destructor Destroy; override;

    procedure Resize; override;

    procedure UpdateBounds;

    property Point1: TPoint read GetPoint1;
    property Point2: TPoint read GetPoint2;
  end;

  TSVGTool = class(TSVG2Control)
  private
    FMargin: Integer;
    FHandleMoving: Boolean;
    FHandleList: TList<TSVGToolHandle>;
    FHandlesVisible: Boolean;
    FLineList: TList<TSVGToolLine>;
    FMouseDown: TPoint;
    FSelected: Boolean;
    FMoveable: Boolean;
    FIsChanged: Boolean;
    FFocusedHandle: TSVGToolHandle;
  protected
    procedure MoveDelta(const aDx, aDy: Integer);
    procedure SetSize(const aValue: TRect);

    function GetAbsoluteContentRect: TRect;
    function GetContentRect: TRect;
    function GetFocusedHandle: TSVGToolHandle;
    function GetHandlePoint(const aIndex: Integer): TPoint; virtual;
    function GetHandlesVisible: Boolean;

    procedure SetAbsoluteContentRect(const Value: TRect);
    procedure SetFocusedHandle(const Value: TSVGToolHandle);
    procedure SetHandlesVisible(const Value: Boolean);
    procedure SetHandlePoint(const aIndex: integer; const Value: TPoint); virtual;
    procedure SetMargin(const Value: Integer);
    procedure SetMoveable(const Value: Boolean);
    procedure SetParent(aValue: TWinControl); override;

    procedure DoCreateToolParts; virtual;
    procedure DoSetHandlePoint(const aIndex: integer; const Value: TPoint); virtual;
    procedure DoSetFocusedHandle(const aHandle: TSVGToolHandle); virtual;
    procedure DoMovePosition(const aDx, aDy: Integer); virtual;

    procedure UpdateToolHandles;
    procedure UpdateToolLines;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CreateToolParts;
    procedure ClearToolParts;

    procedure UpdateToolParts;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure Resize; override;

    procedure Select;
    procedure Deselect;

    procedure SetFocus; virtual;

    property HandlePoint[const aIndex : integer]: TPoint read GetHandlePoint write SetHandlePoint;

    property AbsoluteContentRect: TRect read GetAbsoluteContentRect write SetAbsoluteContentRect;
    property ContentRect: TRect read GetContentRect;
    property FocusedHandle: TSVGToolHandle read GetFocusedHandle write SetFocusedHandle;
    property HandleList: TList<TSVGToolHandle> read FHandleList;
    property HandlesVisible: Boolean read GetHandlesVisible write SetHandlesVisible;
    property IsChanged: Boolean read FIsChanged write FISChanged;
    property LineList: TList<TSVGToolLine> read FLineList;
    property Margin: Integer read FMargin write SetMargin;
    property Moveable: Boolean read FMoveable write SetMoveable;
    property Selected: Boolean read FSelected;
  end;

implementation

// -----------------------------------------------------------------------------
//
//                           TSVGToolHandle
//
// -----------------------------------------------------------------------------

constructor TSVGToolHandle.Create(aTool: TSVGTool; const aIndex: Integer;
  const aSize: Integer = 4; const aSVG: string = svg_tool_handle);
begin
  inherited Create(aTool);

  FTool := aTool;
  FSize := aSize;
  FIndex := aIndex;

  FSVG := aSVG;
end;

destructor TSVGToolHandle.Destroy;
begin
  inherited;
end;

function TSVGToolHandle.GetIsFocused: Boolean;
begin
  Result := FTool.FocusedHandle = Self;
end;

function TSVGToolHandle.GetPoint: TPoint;
begin
  if assigned(FTool) then
  begin
    Result := FTool.HandlePoint[FIndex];
  end else
    Result := TPoint.Create(0,0);
end;

procedure TSVGToolHandle.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if ssLeft in Shift then
  begin
    FMouseDown := TPoint.Create(X, Y);

    SetFocus;
  end;
end;

procedure TSVGToolHandle.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Dx, Dy: Integer;
  P: TPoint;
begin
  if ssLeft in Shift then
  begin
    Dx := X - FMouseDown.X;
    Dy := Y - FMouseDown.Y;

    if (Dx <> 0) or (Dy <> 0) then
    begin
      SetBounds(Left + Dx, Top + Dy, FSize * 2, FSize * 2);

      P := Point;
      P.X := P.X + Dx;
      P.Y := P.Y + Dy;
      Point := P;
    end;
  end;

  inherited;
end;

procedure TSVGToolHandle.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
end;

procedure TSVGToolHandle.Resize;
begin
  inherited;

  SVG.Text := FSVG;
end;

procedure TSVGToolHandle.SetFocus;
begin
  FTool.FocusedHandle := Self;
end;

procedure TSVGToolHandle.SetPoint(const Value: TPoint);
begin
  if assigned(FTool) then
  begin
    FTool.HandlePoint[FIndex] := Value;
  end;
end;

procedure TSVGToolHandle.UpdateBounds;
var
  P: TPoint;
begin
  P := GetPoint;
  P.X := P.X + FTool.Left;
  P.Y := P.Y + FTool.Top;
  SetBounds(P.X - FSize, P.Y - FSize, FSize * 2, FSize * 2);
end;

// -----------------------------------------------------------------------------
//
//                             TSVGToolLine
//
// -----------------------------------------------------------------------------

constructor TSVGToolLine.Create(aTool: TSVGTool; const aIndex1,
  aIndex2: Integer; const aSVG: string = svg_tool_line);
begin
  inherited Create(aTool);

  FMargin := 10;

  FTool := aTool;
  FIndex1 := aIndex1;
  FIndex2 := aIndex2;

  FSVG := aSVG;
end;

destructor TSVGToolLine.Destroy;
begin
  inherited;
end;

function TSVGToolLine.GetPoint1: TPoint;
begin
  if assigned(FTool) then
    Result := FTool.HandlePoint[FIndex1]
  else
    Result := TPoint.Create(0,0);
end;

function TSVGToolLine.GetPoint2: TPoint;
begin
  if assigned(FTool) then
    Result := FTool.HandlePoint[FIndex2]
  else
    Result := TPoint.Create(0,0);
end;

procedure TSVGToolLine.Resize;
begin
  inherited;

  SVG.Text := FSVG;

  SVG.Text := Format(svg_tool_line,
    [1.0 * Width,
     1.0 * Height,
     1.0 * FP1.X,
     1.0 * FP1.Y,
     1.0 * FP2.X,
     1.0 * FP2.Y],
     USFormatSettings);
end;

procedure TSVGToolLine.UpdateBounds;
var
  R: TRect;
begin
  FP1 := GetPoint1;
  FP2 := GetPoint2;

  R.Left := Min(FP1.X, FP2.X) + FTool.Left - FMargin;
  R.Top := Min(FP1.Y, FP2.Y) + FTool.Top - FMargin;
  R.Right := Max(FP1.X, FP2.X) + FTool.Left + FMargin;
  R.Bottom := Max(FP1.Y, FP2.Y) + FTool.Top + FMargin;

  FP1.X := FP1.X + FTool.Left - R.Left;
  FP1.Y := FP1.Y + FTool.Top - R.Top;
  FP2.X := FP2.X + FTool.Left - R.Left;
  FP2.Y := FP2.Y + FTool.Top - R.Top;

  SetBounds(R.Left, R.Top, R.Width, R.Height);
end;

// -----------------------------------------------------------------------------
//
//                               TSVGTool
//
// -----------------------------------------------------------------------------

procedure TSVGTool.ClearToolParts;
var
  Handle: TSVGToolHandle;
  Line: TSVGToolLine;
begin
  for Line in FLineList do
    Line.Free;
  FLineList.Clear;

  for Handle in FHandleList do
    Handle.Free;
  FHandleList.Clear;
end;

constructor TSVGTool.Create(AOwner: TComponent);
begin
  FHandleList := TList<TSVGToolHandle>.Create;
  FLineList := TList<TSVGToolLine>.Create;

  FHandlesVisible := True;
  FHandleMoving := False;
  FMargin := 10;
  FSelected := False;
  FMoveable := True;
  FIsChanged := False;
  FFocusedHandle := nil;

  inherited;
end;

procedure TSVGTool.CreateToolParts;
begin
  ClearToolParts;
  DoCreateToolParts;
  UpdateToolParts;
end;

procedure TSVGTool.Deselect;
begin
  FSelected := False;
end;

destructor TSVGTool.Destroy;
begin
  ClearToolParts;

  FLineList.Free;
  FHandleList.Free;

  inherited;
end;

procedure TSVGTool.DoCreateToolParts;
var
  i: integer;
  Handle: TSVGToolHandle;
begin
  for i := 0 to 3 do
  begin
    Handle := TSVGToolHandle.Create(Self, i);
    Handle.Parent := Parent;
    FHandleList.Add(Handle);
  end;
end;

procedure TSVGTool.DoSetFocusedHandle(const aHandle: TSVGToolHandle);
begin
  SetFocus;
end;

procedure TSVGTool.DoSetHandlePoint(const aIndex: integer; const Value: TPoint);
var
  R: TRect;
begin
  R := ContentRect;

  case aIndex of
    0 : SetSize(
          TRect.Create(Value.X, Value.Y, R.Right, R.Bottom));
    1 : SetSize(
          TRect.Create(R.Left, Value.Y, Value.X, R.Bottom));
    2 : SetSize(
          TRect.Create(R.Left, R.Top, Value.X, Value.Y));
    3 : SetSize(
          TRect.Create(Value.X, R.Top, R.Right, Value.Y));
  end;
end;

procedure TSVGTool.DoMovePosition(const aDx, aDy: Integer);
begin
  FIsChanged := True;

  SetBounds(Left + aDx, Top + aDy, Width, Height);
end;

function TSVGTool.GetAbsoluteContentRect: TRect;
begin
  Result.Left := Left + FMargin;
  Result.Top := Top + FMargin;
  Result.Width := Width - FMargin * 2;
  Result.Height := Height - FMargin * 2;
end;

function TSVGTool.GetContentRect: TRect;
begin
  Result.Left := FMargin;
  Result.Top := FMargin;
  Result.Width := Width - FMargin * 2;
  Result.Height := Height - FMargin * 2;
end;

function TSVGTool.GetFocusedHandle: TSVGToolHandle;
begin
  Result := FFocusedHandle;
end;

function TSVGTool.GetHandlePoint(const aIndex: Integer): TPoint;
var
  R: TRect;
begin
  R := ContentRect;

  case aIndex of
  0 : Result := TPoint.Create(R.Left, R.Top);
  1 : Result := TPoint.Create(R.Right, R.Top);
  2 : Result := TPoint.Create(R.Right, R.Bottom);
  3 : Result := TPoint.Create(R.Left, R.Bottom);
  end;
end;

function TSVGTool.GetHandlesVisible: Boolean;
begin
  Result := FHandlesVisible;
end;

procedure TSVGTool.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if ssLeft in Shift then
  begin
    FMouseDown := TPoint.Create(X, Y);
  end;

  SetFocus;

  Select;

  inherited;
end;

procedure TSVGTool.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Dx, Dy: Integer;
begin
  if ssLeft in Shift then
  begin
    Dx := X - FMouseDown.X;
    Dy := Y - FMouseDown.Y;

    if FMoveable and Selected then
       MoveDelta(Dx, Dy);
  end;

  inherited;
end;

procedure TSVGTool.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
end;

procedure TSVGTool.MoveDelta(const aDx, aDy: Integer);
begin
  if not FMoveable then
    Exit;

  DoMovePosition(aDx, aDy);

  UpdateToolParts;
end;

procedure TSVGTool.Resize;
begin
  inherited;

  if not FHandleMoving then
  begin
    // If the resize is not initiated by a handle, then we update the handles

    if FHandleList.Count = 0 then
      CreateToolParts
    else
      UpdateToolParts;
  end;

  SVG.Text := Format(svg_tool,
    [1.0 * Width,
     1.0 * Height,
     1.0 * FMargin,
     1.0 * FMargin,
     Width - FMargin * 2.0,
     Height - FMargin * 2.0], USFormatSettings);
end;

procedure TSVGTool.Select;
begin
  FSelected := True;
end;

procedure TSVGTool.SetAbsoluteContentRect(const Value: TRect);
begin
  SetBounds(
    Value.Left - Margin,
    Value.Top - Margin,
    Value.Width + Margin * 2,
    Value.Height + Margin * 2);
end;

procedure TSVGTool.SetFocus;
begin
  //
end;

procedure TSVGTool.SetFocusedHandle(const Value: TSVGToolHandle);
begin
  if Value <> FFocusedHandle then
  begin
    FFocusedHandle := Value;

    DoSetFocusedHandle(FFocusedHandle);
  end;
end;

procedure TSVGTool.SetHandlePoint(const aIndex: integer;
  const Value: TPoint);
begin
  FIsChanged := True;
  FHandleMoving := True;

  DoSetHandlePoint(aIndex, Value);

  UpdateToolLines;
end;

procedure TSVGTool.SetHandlesVisible(const Value: Boolean);
begin
  if Value <> FHandlesVisible then
  begin
    FHandlesVisible := Value;
  end;
end;

procedure TSVGTool.SetMargin(const Value: Integer);
begin
  FMargin := Value;
end;

procedure TSVGTool.SetMoveable(const Value: Boolean);
begin
  FMoveable := Value;
end;

procedure TSVGTool.SetParent(aValue: TWinControl);
var
  Handle: TSVGToolHandle;
begin
  inherited;

  for Handle in FHandleList do
    Handle.Parent := aValue;
end;

procedure TSVGTool.SetSize(const aValue: TRect);
var
  R: TRect;
  Dx, Dy, Dw, Dh: Integer;
begin
  R := ContentRect;

  Dx := aValue.Left - R.Left;
  Dy := aValue.Top - R.Top;
  Dw := aValue.Right - R.Right - Dx;
  Dh := aValue.Bottom - R.Bottom - Dy;

  SetBounds(Left + Dx, Top + Dy, Width + Dw, Height + Dh);

  UpdateToolParts;
end;

procedure TSVGTool.UpdateToolHandles;
var
  Handle: TSVGToolHandle;
begin
  for Handle in FHandleList do
    Handle.UpdateBounds;
end;

procedure TSVGTool.UpdateToolLines;
var
  Line: TSVGToolLine;
begin
  for Line in FLineList do
    Line.UpdateBounds;
end;

procedure TSVGTool.UpdateToolParts;
begin
  UpdateToolLines;
  UpdateToolHandles;
end;

end.
