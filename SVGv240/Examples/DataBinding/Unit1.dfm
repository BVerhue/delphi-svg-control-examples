object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'SVG data binding - chemical process'
  ClientHeight = 800
  ClientWidth = 1288
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 15
  object pnlControls: TPanel
    Left = 988
    Top = 0
    Width = 300
    Height = 800
    Align = alRight
    BevelOuter = bvNone
    Padding.Left = 16
    Padding.Top = 16
    Padding.Right = 16
    ParentBackground = False
    TabOrder = 0
    object lblTemp: TLabel
      Left = 16
      Top = 16
      Width = 268
      Height = 15
      AutoSize = False
      Caption = 'Temperature'
    end
    object lblPress: TLabel
      Left = 16
      Top = 76
      Width = 268
      Height = 15
      AutoSize = False
      Caption = 'Pressure'
    end
    object lblFlow: TLabel
      Left = 16
      Top = 136
      Width = 268
      Height = 15
      AutoSize = False
      Caption = 'Flow'
    end
    object lblFeed: TLabel
      Left = 16
      Top = 196
      Width = 268
      Height = 15
      AutoSize = False
      Caption = 'Feed tank T-101'
    end
    object lblReactor: TLabel
      Left = 16
      Top = 256
      Width = 268
      Height = 15
      AutoSize = False
      Caption = 'Reactor R-101'
    end
    object lblProduct: TLabel
      Left = 16
      Top = 316
      Width = 268
      Height = 15
      AutoSize = False
      Caption = 'Product tank T-102'
    end
    object lblPerf: TLabel
      Left = 34
      Top = 562
      Width = 250
      Height = 15
      AutoSize = False
      Caption = 'Painting ...'
    end
    object lblHint: TLabel
      Left = 16
      Top = 596
      Width = 268
      Height = 190
      AutoSize = False
      Caption =
        'Every control on this panel writes to the drawing through the SVG' +
        'Bindings collection of the image. Levels, needles and colours are' +
        ' attributes; the numbers under the gauges and the plant status ar' +
        'e the text content of an element.'#13#10#13#10'The pipes are driven by cl' +
        'ass and the instrument bubbles by their data-tag attribute. The a' +
        'larm lamps are selected both ways: each vessel lights its own lam' +
        'p above 90%, and the lamp test lights all of them at once.'#13#10#13#10 +
        'Persistent buffers keep the rendered result of everything that d' +
        'oes not change - the vessels, the pipework, the gauge faces - an' +
        'd redraw only what the application writes to. Turn it off to see' +
        ' what that is worth here. Animation gains the same way.'
      WordWrap = True
    end
    object tbTemp: TTrackBar
      Left = 16
      Top = 34
      Width = 268
      Height = 34
      Max = 200
      Frequency = 25
      Position = 120
      TabOrder = 0
      TickStyle = tsNone
      OnChange = ControlChanged
    end
    object tbPress: TTrackBar
      Left = 16
      Top = 94
      Width = 268
      Height = 34
      Max = 100
      Frequency = 10
      Position = 35
      TabOrder = 1
      TickStyle = tsNone
      OnChange = ControlChanged
    end
    object tbFlow: TTrackBar
      Left = 16
      Top = 154
      Width = 268
      Height = 34
      Max = 500
      Frequency = 50
      Position = 180
      TabOrder = 2
      TickStyle = tsNone
      OnChange = ControlChanged
    end
    object tbFeed: TTrackBar
      Left = 16
      Top = 214
      Width = 268
      Height = 34
      Max = 100
      Frequency = 10
      Position = 60
      TabOrder = 3
      TickStyle = tsNone
      OnChange = ControlChanged
    end
    object tbReactor: TTrackBar
      Left = 16
      Top = 274
      Width = 268
      Height = 34
      Max = 100
      Frequency = 10
      Position = 67
      TabOrder = 4
      TickStyle = tsNone
      OnChange = ControlChanged
    end
    object tbProduct: TTrackBar
      Left = 16
      Top = 334
      Width = 268
      Height = 34
      Max = 100
      Frequency = 10
      Position = 44
      TabOrder = 5
      TickStyle = tsNone
      OnChange = ControlChanged
    end
    object cbPump: TCheckBox
      Left = 16
      Top = 390
      Width = 268
      Height = 21
      Caption = 'Pump P-101 running'
      Checked = True
      State = cbChecked
      TabOrder = 6
      OnClick = ControlChanged
    end
    object cbHeater: TCheckBox
      Left = 16
      Top = 418
      Width = 268
      Height = 21
      Caption = 'Heater H-101 on'
      Checked = True
      State = cbChecked
      TabOrder = 7
      OnClick = ControlChanged
    end
    object cbValve: TCheckBox
      Left = 16
      Top = 446
      Width = 268
      Height = 21
      Caption = 'Valve V-101 open'
      Checked = True
      State = cbChecked
      TabOrder = 8
      OnClick = ControlChanged
    end
    object cbAlarm: TCheckBox
      Left = 16
      Top = 474
      Width = 268
      Height = 21
      Caption = 'Lamp test - light every alarm'
      TabOrder = 9
      OnClick = ControlChanged
    end
    object cbHighlight: TCheckBox
      Left = 16
      Top = 502
      Width = 268
      Height = 21
      Caption = 'Highlight instruments'
      TabOrder = 10
      OnClick = ControlChanged
    end
    object cbBuffers: TCheckBox
      Left = 16
      Top = 538
      Width = 268
      Height = 21
      Caption = 'Persistent buffers (sroPersistentBuffers)'
      Checked = True
      State = cbChecked
      TabOrder = 11
      OnClick = ControlChanged
    end
  end
  object SVG2Image1: TSVG2Image
    Left = 0
    Top = 0
    Width = 988
    Height = 800
    Align = alClient
    AutoViewbox = True
    RenderOptions = [sroClippath, sroFilters, sroPersistentBuffers]
    Padding.Left = 8
    Padding.Top = 8
    Padding.Right = 8
    Padding.Bottom = 8
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 40
    OnTimer = Timer1Timer
    Left = 24
    Top = 24
  end
end
