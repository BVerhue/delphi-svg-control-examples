unit Unit1;

// -----------------------------------------------------------------------------
//
// Data binding: driving a process drawing from an application
//
// The drawing, process.svg, is a schematic of a small chemical process. Nothing
// in it moves by itself. Every level, needle, colour and readout is written by
// this application through the SVGBindings collection the control publishes.
//
// What the example shows:
//
//   by id         a single element: a tank level, a gauge needle, a readout
//   by class      a whole group at once: every pipe, every alarm lamp
//   by attribute  the instrument bubbles, selected on the data-tag the plant
//                 knows them by rather than on an id
//
//   attributes    positions, sizes, colours and transforms
//   text          the numbers under the gauges and the plant status
//
// B.J.H. Verhue
//
// -----------------------------------------------------------------------------

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls,
  BVE.SVG2Types,
  BVE.SVG2Intf,
  BVE.SVG2Bindings,
  BVE.SVG2Image.VCL;

type
  TForm1 = class(TForm)
    pnlControls: TPanel;
    SVG2Image1: TSVG2Image;
    Timer1: TTimer;
    lblTemp: TLabel;
    tbTemp: TTrackBar;
    lblPress: TLabel;
    tbPress: TTrackBar;
    lblFlow: TLabel;
    tbFlow: TTrackBar;
    lblFeed: TLabel;
    tbFeed: TTrackBar;
    lblReactor: TLabel;
    tbReactor: TTrackBar;
    lblProduct: TLabel;
    tbProduct: TTrackBar;
    cbPump: TCheckBox;
    cbHeater: TCheckBox;
    cbValve: TCheckBox;
    cbAlarm: TCheckBox;
    cbHighlight: TCheckBox;
    lblHint: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ControlChanged(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    // Bindings are kept so the values can be written by name later. Everything
    // here is created once, in CreateBindings, and never looked up again.

    FFeedY, FFeedH: TSVGBinding;              // Feed tank level
    FReactorY, FReactorH: TSVGBinding;        // Reactor level
    FProductY, FProductH: TSVGBinding;        // Product tank level

    FPump: TSVGBinding;                       // Pump impeller rotation
    FAgitator: TSVGBinding;                   // Agitator rotation
    FValve: TSVGBinding;                      // Valve handle rotation
    FHeater: TSVGBinding;                     // Heater element colour

    FNeedleTemp: TSVGBinding;                 // Gauge needles
    FNeedlePress: TSVGBinding;
    FNeedleFlow: TSVGBinding;

    FTextTemp: TSVGBinding;                   // Readouts, written as text
    FTextPress: TSVGBinding;
    FTextFlow: TSVGBinding;
    FTextFeed: TSVGBinding;
    FTextProduct: TSVGBinding;
    FTextStatus: TSVGBinding;

    FPipes: TSVGBinding;                      // Every pipe, by class
    FAlarms: TSVGBinding;                     // Every alarm lamp, by class
    FInstruments: TSVGBinding;                // Every bubble, by data-tag

    FRotation: Double;                        // Where the rotating parts stand

    procedure LoadDrawing;
    procedure CreateBindings;

    function Running: Boolean;

    procedure SetLevel(const aY, aHeight: TSVGBinding;
      const aTop, aBottom, aPercent: Double);
  public
    procedure UpdatePlant;
  end;

var
  Form1: TForm1;

implementation

uses
  System.IOUtils,
  System.Math;

{$R *.dfm}

const
  // The inside of each vessel, in the coordinates of the drawing: a level of
  // 100% fills from Bottom up to Top.
  FeedTop      = 124.0;   FeedBottom    = 296.0;
  ReactorTop   = 144.0;   ReactorBottom = 316.0;
  ProductTop   = 144.0;   ProductBottom = 316.0;

  // Full scale of the three gauges, and the sweep of their needles.
  TempFullScale  = 200.0;                     // degrees C
  PressFullScale = 10.0;                      // bar
  FlowFullScale  = 50.0;                      // m3/h
  NeedleSweep    = 240.0;                     // degrees, -120 to +120

  ColorPipeIdle    = '#9aa5b1';
  ColorPipeFlowing = '#3f9e5a';
  ColorLampOff     = '#c8d0d8';
  ColorLampAlarm   = '#d64545';
  ColorHeaterOff   = '#dfe5ec';
  ColorInstrument  = '#5b6b7c';
  ColorHighlight   = '#e08a2e';

var
  // Anything that is SVG syntax rather than words - a transform, a coordinate,
  // a length - is written with a dot, whatever the machine's locale says. The
  // readouts below are the other case: those are text for a person to read, so
  // they follow the locale and show 20,5 to a reader who writes it that way.
  //
  // A number assigned to a binding as a number, such as the levels here, needs
  // none of this: the library formats it for SVG itself.
  SvgFmt: TFormatSettings;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  LoadDrawing;
  CreateBindings;

  UpdatePlant;
end;

procedure TForm1.LoadDrawing;
var
  Dir, FileName: string;
  i: Integer;
begin
  // The drawing sits next to the project, while the executable is built into a
  // subfolder of it, so look upwards for it.

  Dir := ExtractFilePath(ParamStr(0));

  for i := 0 to 4 do
  begin
    FileName := TPath.Combine(Dir, 'process.svg');

    if FileExists(FileName) then
    begin
      SVG2Image1.SVG.LoadFromFile(FileName);
      Exit;
    end;

    Dir := ExtractFilePath(ExcludeTrailingPathDelimiter(Dir));
  end;

  raise Exception.Create('process.svg was not found next to the application');
end;

procedure TForm1.CreateBindings;
var
  Bindings: TSVGBindingCollection;
begin
  Bindings := SVG2Image1.SVGBindings;

  // --- by id, writing an attribute -----------------------------------------
  //
  // A level is two bindings on one element: the rectangle grows upwards, so
  // both its height and its top edge move.

  FFeedY       := Bindings.Bind('tank1-level', 'y');
  FFeedH       := Bindings.Bind('tank1-level', 'height');
  FReactorY    := Bindings.Bind('reactor-level', 'y');
  FReactorH    := Bindings.Bind('reactor-level', 'height');
  FProductY    := Bindings.Bind('tank2-level', 'y');
  FProductH    := Bindings.Bind('tank2-level', 'height');

  // Anything that moves is a transform. The drawing decides where the pivot
  // is; the application only supplies the angle.

  FPump        := Bindings.Bind('pump-impeller', 'transform');
  FAgitator    := Bindings.Bind('agitator', 'transform');
  FValve       := Bindings.Bind('valve-handle', 'transform');

  FNeedleTemp  := Bindings.Bind('gauge-temp-needle', 'transform');
  FNeedlePress := Bindings.Bind('gauge-press-needle', 'transform');
  FNeedleFlow  := Bindings.Bind('gauge-flow-needle', 'transform');

  FHeater      := Bindings.Bind('heater-glow', 'fill');

  // --- by id, writing text --------------------------------------------------
  //
  // tkText replaces the text content of the element rather than an attribute,
  // which is how a value becomes a label.

  FTextTemp    := Bindings.Bind('readout-temp', '', skID, tkText);
  FTextPress   := Bindings.Bind('readout-press', '', skID, tkText);
  FTextFlow    := Bindings.Bind('readout-flow', '', skID, tkText);
  FTextFeed    := Bindings.Bind('readout-level1', '', skID, tkText);
  FTextProduct := Bindings.Bind('readout-level2', '', skID, tkText);
  FTextStatus  := Bindings.Bind('readout-status', '', skID, tkText);

  // --- by class -------------------------------------------------------------
  //
  // One binding, every element carrying the class. The drawing can gain another
  // pipe or another lamp without this code changing.

  FPipes       := Bindings.Bind('pipe', 'stroke', skClass);
  FAlarms      := Bindings.Bind('alarm', 'fill', skClass);

  // --- by attribute ---------------------------------------------------------
  //
  // The instrument bubbles are selected on the tag the plant knows them by.
  // MatchSelectorValue False takes every element that carries a data-tag at
  // all, whatever its value.

  FInstruments := Bindings.Bind('data-tag', 'stroke', skAttribute);
  FInstruments.MatchSelectorValue := False;
end;

function TForm1.Running: Boolean;
begin
  Result := cbPump.Checked and cbValve.Checked;
end;

procedure TForm1.SetLevel(const aY, aHeight: TSVGBinding;
  const aTop, aBottom, aPercent: Double);
var
  h: Double;
begin
  h := (aBottom - aTop) * aPercent / 100;

  aHeight.Value := h;
  aY.Value := aBottom - h;
end;

procedure TForm1.UpdatePlant;
var
  Temp, Press, Flow: Double;
  Feed, Reactor, Product: Double;
  Heat: Double;
  Status: string;
begin
  Temp    := tbTemp.Position;
  Press   := tbPress.Position / 10;
  Flow    := tbFlow.Position / 10;
  Feed    := tbFeed.Position;
  Reactor := tbReactor.Position;
  Product := tbProduct.Position;

  lblTemp.Caption    := Format('Temperature   %.0f °C', [Temp]);
  lblPress.Caption   := Format('Pressure   %.1f bar', [Press]);
  lblFlow.Caption    := Format('Flow   %.1f m³/h', [Flow]);
  lblFeed.Caption    := Format('Feed tank T-101   %.0f %%', [Feed]);
  lblReactor.Caption := Format('Reactor R-101   %.0f %%', [Reactor]);
  lblProduct.Caption := Format('Product tank T-102   %.0f %%', [Product]);

  // Levels

  SetLevel(FFeedY, FFeedH, FeedTop, FeedBottom, Feed);
  SetLevel(FReactorY, FReactorH, ReactorTop, ReactorBottom, Reactor);
  SetLevel(FProductY, FProductH, ProductTop, ProductBottom, Product);

  // Gauges. A needle stands at full scale counter clockwise and sweeps
  // clockwise, so zero is at -120 degrees.

  FNeedleTemp.Value := Format('rotate(%.1f 150 470)',
    [-NeedleSweep / 2 + NeedleSweep * Min(Temp / TempFullScale, 1)], SvgFmt);
  FNeedlePress.Value := Format('rotate(%.1f 400 470)',
    [-NeedleSweep / 2 + NeedleSweep * Min(Press / PressFullScale, 1)], SvgFmt);
  FNeedleFlow.Value := Format('rotate(%.1f 650 470)',
    [-NeedleSweep / 2 + NeedleSweep * Min(Flow / FlowFullScale, 1)], SvgFmt);

  // Readouts

  FTextTemp.Value    := Format('%.0f °C', [Temp]);
  FTextPress.Value   := Format('%.1f bar', [Press]);
  FTextFlow.Value    := Format('%.1f m³/h', [Flow]);
  FTextFeed.Value    := Format('%.0f %%', [Feed]);
  FTextProduct.Value := Format('%.0f %%', [Product]);

  // Equipment

  FValve.Value := Format('rotate(%d 700 184)',
    [IfThen(cbValve.Checked, 90, 0)], SvgFmt);

  if cbHeater.Checked then
  begin
    // From the cold colour to a hot one, by temperature.
    Heat := Min(Temp / TempFullScale, 1);
    FHeater.Value := Format('#%.2x%.2x%.2x',
      [223 + Round((214 - 223) * Heat),
       229 + Round(( 90 - 229) * Heat),
       236 + Round(( 60 - 236) * Heat)], SvgFmt);
  end else
    FHeater.Value := ColorHeaterOff;

  // Groups

  if Running then
    FPipes.Value := ColorPipeFlowing
  else
    FPipes.Value := ColorPipeIdle;

  if cbAlarm.Checked then
    FAlarms.Value := ColorLampAlarm
  else
    FAlarms.Value := ColorLampOff;

  if cbHighlight.Checked then
    FInstruments.Value := ColorHighlight
  else
    FInstruments.Value := ColorInstrument;

  // Status

  if cbAlarm.Checked then
    Status := 'ALARM'
  else
    if Running then
      Status := 'RUNNING'
    else
      Status := 'STOPPED';

  FTextStatus.Value := Status;

  Timer1.Enabled := cbPump.Checked;
end;

procedure TForm1.ControlChanged(Sender: TObject);
begin
  UpdatePlant;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  // Only the two rotating parts are written here. Assigning a binding's Value
  // repaints the control, so there is nothing else to do.

  FRotation := FRotation + 6 + tbFlow.Position / 25;

  if FRotation >= 360 then
    FRotation := FRotation - 360;

  FPump.Value := Format('rotate(%.1f 230 350)', [FRotation], SvgFmt);
  FAgitator.Value := Format('rotate(%.1f 550 250)', [FRotation * 0.6], SvgFmt);
end;

initialization

  SvgFmt := TFormatSettings.Create;
  SvgFmt.DecimalSeparator := '.';

end.
