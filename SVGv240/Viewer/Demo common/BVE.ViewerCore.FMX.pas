unit BVE.ViewerCore.FMX;

//------------------------------------------------------------------------------
//
//                             SVG Control 2.0
//                     Copyright (c) 2015 Bruno Verhue
//
//------------------------------------------------------------------------------

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

///  <summary>
///    This unit contains the core functionality of the "SVG Viewer demo".
///  </summary>

{$Include '..\Common\CompilerSettings.inc'}

interface
uses
  System.Types,
  System.Classes,
  System.UITypes,
  System.SysUtils,
  Xml.XMLIntf,
  FMX.Types,
  BVE.SVG2Types,
  BVE.SVG2Intf,
  BVE.SVG2Control.FMX,
  BVE.Selection.FMX;

type
  // A container control for the svg graphic

  TSVGSelectControl = class(TSelectControl)
  private
    FSVGControl: TSVG2Control;
    FAutoViewbox: boolean;
    FRenderOptions: TSVGRenderOptions;
    FStatistics: TStringList;

    procedure ReCreateSVGControl;
    procedure SVGMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure SVGMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure SVGMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure SetStatistics(const Value: TStrings);
    function GetStatistics: TStrings;
  protected
    procedure SetSVGControl(const Value: TSVG2Control);
    procedure SetAutoViewBox(const Value: boolean);
    procedure SetRenderOptions(const Value: TSVGRenderOptions);
    procedure AfterParse(Sender: TObject);
  public
    constructor Create(AOwner: TComponent; aSelectionList : TSelectionList); override;
    destructor Destroy; override;

    function CreateCopy(AOwner: TComponent; aSelectionList: TSelectionList): TSVGSelectControl;
    procedure Assign(Source: TPersistent); override;

    procedure UpdateBounds;

    procedure LoadSVG(aFileName : string);
    procedure ParseSVG;
  published
    property SVGControl: TSVG2Control read FSVGControl write SetSVGControl;
    property AutoViewbox: boolean read FAutoViewBox write SetAutoViewBox;
    property RenderOptions: TSVGRenderOptions read FRenderOptions write SetRenderOptions;
    property Statistics: TStrings read GetStatistics write SetStatistics;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  // Example of interactive SVG

  TSVGAnimatedClock = class(TSVG2Control)
  private
    FGearSeconds: ISVGObject;
    FGear7_5: ISVGObject;
    FGearMinutes: ISVGObject;
    FGear180: ISVGObject;
    FGearHours: ISVGObject;

    FHandSecond: ISVGObject;
    FPathHandSecond: ISVGObject;

    FHandMinute: ISVGObject;
    FPathHandMinute: ISVGObject;

    FHandHour: ISVGObject;
    FPathhandHour: ISVGObject;

    FHour, FMin, FSec: single;
    FTimer: TTimer;

    FMouseDownPoint: TPointF;
    FSelectedObject: ISVGObject;

    procedure SetLayoutAngle(aLayout: ISVGObject; aAngle: single);
    procedure AddDeltaSec(dm: single);
    procedure AddDeltaMin(dm: single);
    procedure AddDeltaHour(dm: single);
    procedure UpdateClock;
    procedure SetTime(Sender: TObject);
    function GetValid: boolean;
  protected
    procedure DoAfterParse; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init;

    procedure Assign(Source: TPersistent); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;

    procedure ProcessMouseMove(aSVGObject: ISVGObject; Shift: TShiftState; dX, dY: Single);
  end;

implementation
uses
  System.Math,
{$IFDEF Ver270Up}
  System.Math.Vectors,
{$ENDIF}
  Xml.XMLDoc,
  BVE.SVG2SaxParser,
  BVE.SVG2GeomUtility,
  BVE.SVG2Doc;

//------------------------------------------------------------------------------
//
//                            TSVGSelectControl
//
//------------------------------------------------------------------------------

procedure TSVGSelectControl.AfterParse(Sender: TObject);
begin
  FSVGControl.CalcSize;
end;

procedure TSVGSelectControl.Assign(Source: TPersistent);
begin
  if Source is TSVGSelectControl then
  begin
    FSVGControl.Assign((Source as TSVGSelectControl).FSVGControl);
    FStatistics.Assign((Source as TSVGSelectControl).FStatistics);
    Width := (Source as TSVGSelectControl).Width;
    Height := (Source as TSVGSelectControl).Height;
    UpdateBounds;
  end;
end;

constructor TSVGSelectControl.Create(AOwner: TComponent; aSelectionList : TSelectionList);
begin
  inherited;

  Width := 100;
  Height := 80;

  FStatistics := TStringList.Create;
  AutoViewbox := False;
  RenderOptions := [sroClippath];

  ReCreateSVGControl;
end;

function TSVGSelectControl.CreateCopy(AOwner: TComponent;
  aSelectionList: TSelectionList): TSVGSelectControl;
begin
  Result := TSVGSelectControl.Create(AOwner, aSelectionList);
  Result.ReCreateSVGControl;
  Result.Assign( self);
end;

destructor TSVGSelectControl.Destroy;
begin
  FStatistics.Free;
  if assigned(FSVGControl) then
    FSVGControl.Free;
  inherited;
end;

function TSVGSelectControl.GetStatistics: TStrings;
begin
  Result := FStatistics;
end;

procedure TSVGSelectControl.ReCreateSVGControl;
var
  Name, Filename: string;
begin
  if assigned(FSVGControl) then
  begin
    FileName := FSVGControl.FileName;
    Name := Lowercase(ExtractFilename(FileName))
  end else begin
    FileName := '';
    Name := '';
  end;

  if (Name = 'animated-clock.svg') then
  begin
    if assigned(FSVGControl) and not(FSVGControl is TSVGAnimatedClock) then
    begin
      FSVGControl.Free;
      FSVGControl := nil;
    end;
    if not assigned(FSVGControl) then
      FSVGControl := TSVGAnimatedClock.Create(nil);
  end else
    if (Name <> 'animated-clock.svg') then
    begin
      if assigned(FSVGControl) and (FSVGControl is TSVGAnimatedClock) then
      begin
        FSVGControl.Free;
        FSVGControl := nil;
      end;
      if not assigned(FSVGControl) then
        FSVGControl := TSVG2Control.Create(nil);
    end;

  FSVGControl.FileName := FileName;
  FSVGControl.AutoViewbox := FAutoViewbox;
  if Name = 'animated-clock.svg' then
    FSVGControl.RenderOptions := [sroEvents]
  else
    FSVGControl.RenderOptions := FRenderOptions;
  FSVGControl.Parent := Content;
  FSVGControl.HitTest := True;
  FSVGControl.AutoCapture := True;

  FSVGControl.OnMouseDown := SVGMouseDown;
  FSVGControl.OnMouseMove := SVGMouseMove;
  FSVGControl.OnMouseUp := SVGMouseUp;
  FSVGControl.OnAfterParse := AfterParse;
end;

procedure TSVGSelectControl.LoadSVG(aFileName: string);
begin
  FSVGControl.FileName := aFileName;
  ReCreateSVGControl;
end;

procedure TSVGSelectControl.ParseSVG;
var
  SVGParser: TSVGSaxParser;
begin
  SVGParser := FSVGControl.SVGParser;
  FSVGControl.ParseSVG;
  UpdateBounds;
  FSVGControl.Align := TalignLayout.alClient;
  FStatistics.Clear;
  SVGParser.ReportStatistics(FStatistics);
end;

procedure TSVGSelectControl.SetAutoViewBox(const Value: boolean);
begin
  FAutoViewBox := Value;
  if assigned(FSVGControl) then
    FSVGControl.AutoViewbox := FAutoViewBox;
end;

procedure TSVGSelectControl.SetRenderOptions(const Value: TSVGRenderOptions);
begin
  FRenderOptions := Value;
  if assigned(FSVGControl) then
    FSVGControl.RenderOptions := FRenderOptions;
end;

procedure TSVGSelectControl.SetStatistics(const Value: TStrings);
begin
  FStatistics.Assign(Value);
end;

procedure TSVGSelectControl.SetSVGControl(const Value: TSVG2Control);
begin
  if assigned(FSVGControl) then
    FSVGControl.Free;

  FSVGControl := Value;

  if assigned(FSVGControl) then
  begin
    FSVGControl.Parent := Content;
    FSVGControl.AutoViewbox := FAutoViewBox;
    FSVGControl.RenderOptions := FRenderOptions;
    UpdateBounds;
  end;
end;

procedure TSVGSelectControl.SVGMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  MouseDown(Button, Shift, FSVGControl.Position.X + Margin + X, FSVGControl.Position.Y + Margin + Y);
end;

procedure TSVGSelectControl.SVGMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
  MouseMove(Shift, FSVGControl.Position.X + Margin + X, FSVGControl.Position.Y + Margin + Y);
end;

procedure TSVGSelectControl.SVGMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  MouseUp(Button, Shift, FSVGControl.Position.X + Margin + X, FSVGControl.Position.Y + Margin + Y);
end;

procedure TSVGSelectControl.UpdateBounds;
begin
  SetBounds(50, 50, SVGControl.Width + Margin*2, SVGControl.Height + Margin*2);
end;


//------------------------------------------------------------------------------
//
//                               TSVGAnimatedClock
//
//------------------------------------------------------------------------------

constructor TSVGAnimatedClock.Create(AOwner: TComponent);
begin
  inherited;

  FTimer := TTimer.Create(self);
  FTimer.Interval := 1000;
  FTimer.Enabled := False;
  FTimer.OnTimer := SetTime;

  FSelectedObject := nil;
end;

destructor TSVGAnimatedClock.Destroy;
begin
  FSelectedObject := nil;
  inherited;
end;

procedure TSVGAnimatedClock.DoAfterParse;
begin
  Init;
  inherited;
end;

procedure TSVGAnimatedClock.Assign(Source: TPersistent);
begin
  inherited;

  if Source is TSVGAnimatedClock then
    Init;
end;

function TSVGAnimatedClock.GetValid: boolean;
begin
  Result := assigned(FGearSeconds)
        and assigned(FGear7_5)
        and assigned(FGearMinutes)
        and assigned(FGear180)
        and assigned(FGearHours)
        and assigned(FHandSecond)
        and assigned(FPathHandSecond)
        and assigned(FHandMinute)
        and assigned(FPathHandMinute)
        and assigned(FHandHour)
        and assigned(FPathHandHour);
end;

procedure TSVGAnimatedClock.Init;
begin
  // Set pointers to interactive/animated SVG parts
  FGearSeconds := SVGRoot.SVGObject['RotSeconds'];
  FGear7_5 := SVGRoot.SVGObject['RotGear7_5'];
  FGearMinutes := SVGRoot.SVGObject['RotMinutes'];
  FGear180 := SVGRoot.SVGObject['RotGear180'];
  FGearHours := SVGRoot.SVGObject['RotHours'];

  // The interactive SVG objects should have Hittest = TRUE otherwise they
  // are ignored by TSVGGroup.ObjectAt() or mouseevents

  FHandSecond := SVGRoot.SVGObject['RotSecondHand'];
  FPathHandSecond := SVGRoot.SVGObject['SecondHandPath'];

  FHandMinute := SVGRoot.SVGObject['RotMinuteHand'];
  FPathHandMinute := SVGRoot.SVGObject['MinuteHandPath'];

  FHandHour:= SVGRoot.SVGObject['RotHourHand'];
  FPathHandHour := SVGRoot.SVGObject['HourHandPath'];

  if GetValid then
  begin
    FPathHandSecond.HitTest := True;
    FPathHandMinute.HitTest := True;
    FPathHandHour.HitTest := True;
    FTimer.Enabled := True;
  end;

  FSelectedObject := nil;
end;

procedure TSVGAnimatedClock.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if ssLeft in Shift then
  begin
    FSelectedObject := ObjectAtPt( PointF( X, Y));
    FMouseDownPoint := PointF( X, Y);
  end;

  inherited;
end;

procedure TSVGAnimatedClock.MouseMove(Shift: TShiftState; X, Y: Single);
var
  dx, dy: single;
begin
  if ssLeft in Shift then
  begin

    if assigned(FSelectedObject) then
    begin
      dx := X - FMouseDownPoint.X;
      dy := Y - FMouseDownPoint.Y;
      FMouseDownPoint := PointF(X, Y);

      ProcessMouseMove(FSelectedObject, Shift, dx, dy);
    end else
      inherited;

  end else
    inherited;
end;

procedure TSVGAnimatedClock.ProcessMouseMove(aSVGObject: ISVGObject;
  Shift: TShiftState; dX, dY: single);
begin
  if aSVGObject.ID = 'SecondHandPath' then
  begin
    AddDeltaSec( -dy / 23);
  end;

  if aSVGObject.ID = 'MinuteHandPath' then
  begin
    AddDeltaMin( -dy / 23);
  end;

  if aSVGObject.ID = 'HourHandPath' then
  begin
    AddDeltaHour( -dy / 23);
  end;
end;

procedure TSVGAnimatedClock.AddDeltaSec(dm : single);
begin
  FTimer.Enabled := False;

  if FSec + dm > 60 then
  begin
    AddDeltaMin(1);
    FSec := trunc(FSec + dm - 60);
  end else
    if FSec + dm < 0 then
    begin
      AddDeltaMin(-1);
      FSec := FSec + dm + 60;
    end else
      FSec := FSec + dm;

  UpdateClock;
end;

procedure TSVGAnimatedClock.AddDeltaMin(dm: single);
begin
  FTimer.Enabled := False;

  if FMin + dm > 60 then
  begin
    AddDeltaHour(1);
    FMin := trunc(Fmin + dm - 60);
  end else
    if FMin + dm < 0 then
    begin
      AddDeltaHour(-1);
      FMin := FMin + dm + 60;
    end else
      FMin := FMin + dm;

  UpdateClock;
end;

procedure TSVGAnimatedClock.AddDeltaHour(dm: single);
begin
  FTimer.Enabled := False;

  if FHour + dm > 24 then
  begin
    FHour := trunc(FHour + dm - 24);
  end else
    if FHour + dm < 0 then
    begin
      FHour := FHour + dm + 24;
    end else
      FHour := FHour + dm;

  UpdateClock;
end;

procedure TSVGAnimatedClock.UpdateClock;
var
  a_sec, a_7_5, a_min, a_180, a_hours: single;
  tot_s: single;
begin
  if not GetValid then
    exit;

  tot_s := FMin * 60 + FHour * 3600 + FSec;

  a_sec   :=   (tot_s * 6) - 360 * trunc( tot_s * 6 / 360);
  a_7_5   := -((tot_s * 6 / 7.5) - 360 * trunc(tot_s * 6 / 7.5 / 360));
  a_min   :=   (tot_s * 6 /  60) - 360 * trunc(tot_s * 6 /  60 / 360);
  a_180   := -((tot_s * 6 / 180) - 360 * trunc(tot_s * 6 / 180 / 360));
  a_hours :=   (tot_s * 6 / 720) - 360 * trunc(tot_s * 6 / 720 / 360);

  SetLayoutAngle(FGearSeconds, a_sec);
  SetLayoutAngle(FGear7_5, a_7_5);
  SetLayoutAngle(FGearMinutes, a_min);
  SetLayoutAngle(FGear180, a_180);
  SetLayoutAngle(FGearHours, a_hours);
  SetLayoutAngle(FHandSecond, a_sec);
  SetLayoutAngle(FHandMinute, a_min);
  SetLayoutAngle(FHandHour, a_hours);

  Repaint;
end;

procedure TSVGAnimatedClock.SetLayoutAngle(aLayout: ISVGObject; aAngle: single);
begin
  //aLayout.Transform(1,1,0,0, aAngle*PI/180, 0,0);
  aLayout.Attributes['transform'] := Format('rotate(%f)', [aAngle]);
end;

procedure TSVGAnimatedClock.SetTime(Sender : TObject);
var
  h, m, s, ms: word;
begin
  DecodeTime(Now, h, m, s, ms);
  FHour := h;
  FMin := m;
  FSec := s;
  UpdateClock;
end;

end.
