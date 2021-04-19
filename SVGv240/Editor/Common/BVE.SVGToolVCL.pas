unit BVE.SVGToolVCL;

// ------------------------------------------------------------------------------
//
//                          SVG Control Package 2.0
//                       Copyright (c) 2015 Bruno Verhue
//
// ------------------------------------------------------------------------------

interface
uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.UITypes,
  System.Generics.Collections,
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

  svg_handle =
    '<?xml version="1.0" standalone="no"?>'
    + '<svg viewBox="0 0 100 100" xmlns="http://www.w3.org/2000/svg" version="1.1">'
    + '<rect x="0" y="0" width="100" height="100" fill="red" stroke="none" stroke-width="1" />'
    + '</svg>';

type
  TSVGTool = class;

  TSVGHandle = class(TSVG2Control)
  private
    FSize: Integer;
    FMouseDown: TPoint;
    FIndex: integer;
    FTool: TSVGTool;
    FSVG: string;
  protected
    function GetPoint: TPoint; virtual;
    procedure SetPoint(const Value: TPoint); virtual;
  public
    constructor Create(aTool: TSVGTool; const aIndex: Integer; const aSize: Integer = 4;
      const aSVG: string = svg_handle); reintroduce; virtual;
    destructor Destroy; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure Resize; override;

    procedure UpdateBounds;

    property Point: TPoint read GetPoint write SetPoint;
  end;

  TSVGTool = class(TSVG2Control)
  private
    FMargin: Integer;
    FHandleMoving: Boolean;
    FHandleList: TList<TSVGHandle>;
    FHandlesVisible: Boolean;
    FMouseDown: TPoint;
    FSelected: Boolean;
    FMoveable: Boolean;

    procedure MoveDelta(const aDx, aDy: Integer);
    procedure SetSize(const aValue: TRect);
  protected
    function GetAbsoluteContentRect: TRect;
    function GetContentRect: TRect;
    function GetHandlePoint(const aIndex: Integer): TPoint; virtual;
    function GetHandlesVisible: Boolean;

    procedure SetAbsoluteContentRect(const Value: TRect);
    procedure SetHandlesVisible(const Value: Boolean);
    procedure SetHandlePoint(const aIndex: integer; const Value: TPoint); virtual;
    procedure SetMargin(const Value: Integer);
    procedure SetMoveable(const Value: Boolean);
    procedure SetParent(aValue: TWinControl); override;


    procedure DoCreateHandles; virtual;
    procedure DoSetHandlePoint(const aIndex: integer; const Value: TPoint); virtual;
    procedure DoMovePosition(const aDx, aDy: Integer); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CreateHandles;
    procedure ClearHandles;

    procedure UpdateHandles;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure Resize; override;

    procedure Select;
    procedure Deselect;

    property HandlePoint[const aIndex : integer]: TPoint read GetHandlePoint write SetHandlePoint;

    property HandleList: TList<TSVGHandle> read FHandleList;
    property HandlesVisible: Boolean read GetHandlesVisible write SetHandlesVisible;
    property AbsoluteContentRect: TRect read GetAbsoluteContentRect write SetAbsoluteContentRect;
    property ContentRect: TRect read GetContentRect;
    property Margin: Integer read FMargin write SetMargin;
    property Selected: Boolean read FSelected;
    property Moveable: Boolean read FMoveable write SetMoveable;
  end;

implementation

// -----------------------------------------------------------------------------
//
//                           TSVGHandle
//
// -----------------------------------------------------------------------------

constructor TSVGHandle.Create(aTool: TSVGTool; const aIndex: Integer;
  const aSize: Integer = 4; const aSVG: string = svg_handle);
begin
  inherited Create(aTool);

  FTool := aTool;
  FSize := aSize;
  FIndex := aIndex;

  FSVG := aSVG;
end;

destructor TSVGHandle.Destroy;
begin
  inherited;
end;

function TSVGHandle.GetPoint: TPoint;
begin
  if assigned(FTool) then
  begin
    Result := FTool.HandlePoint[FIndex];
  end else
    Result := TPoint.Create(0,0);
end;

procedure TSVGHandle.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if ssLeft in Shift then
  begin
    FMouseDown := TPoint.Create(X, Y);
  end;
end;

procedure TSVGHandle.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Dx, Dy: Integer;
  P: TPoint;
begin
  if ssLeft in Shift then
  begin
    Dx := X - FMouseDown.X;
    Dy := Y - FMouseDown.Y;

    P := Point;
    P.X := P.X + Dx;
    P.Y := P.Y + Dy;
    Point := P;
  end;

  inherited;
end;

procedure TSVGHandle.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
end;

procedure TSVGHandle.Resize;
begin
  inherited;

  SVG.Text := FSVG;
end;

procedure TSVGHandle.SetPoint(const Value: TPoint);
begin
  if assigned(FTool) then
  begin
    FTool.HandlePoint[FIndex] := Value;
  end;
end;

procedure TSVGHandle.UpdateBounds;
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
//                               TSVGTool
//
// -----------------------------------------------------------------------------

procedure TSVGTool.ClearHandles;
var
  Handle: TSVGHandle;
begin
  for Handle in FHandleList do
    Handle.Free;
  FHandleList.Clear;
end;

constructor TSVGTool.Create(AOwner: TComponent);
begin
  FHandleList := TList<TSVGHandle>.Create;

  FHandlesVisible := True;
  FHandleMoving := False;
  FMargin := 10;
  FSelected := False;
  FMoveable := True;

  inherited;
end;

procedure TSVGTool.CreateHandles;
begin
  ClearHandles;
  DoCreateHandles;
  UpdateHandles;
end;

procedure TSVGTool.Deselect;
begin
  FSelected := False;
end;

destructor TSVGTool.Destroy;
begin
  FHandleList.Free;

  inherited;
end;

procedure TSVGTool.DoCreateHandles;
var
  i: integer;
  Handle: TSVGHandle;
begin
  for i := 0 to 3 do
  begin
    Handle := TSVGHandle.Create(Self, i);
    Handle.Parent := Parent;
    FHandleList.Add(Handle);
  end;
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

  UpdateHandles;
end;

procedure TSVGTool.Resize;
begin
  inherited;

  if not FHandleMoving then
  begin
    // If the resize is not initiated by a handle, then we update the handles

    if FHandleList.Count = 0 then
      CreateHandles
    else
      UpdateHandles;
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

procedure TSVGTool.SetHandlePoint(const aIndex: integer;
  const Value: TPoint);
begin
  DoSetHandlePoint(aIndex, Value);
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
  Handle: TSVGHandle;
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

  UpdateHandles;
end;

procedure TSVGTool.UpdateHandles;
var
  Handle: TSVGHandle;
begin
  for Handle in FHandleList do
    Handle.UpdateBounds;
end;

end.
