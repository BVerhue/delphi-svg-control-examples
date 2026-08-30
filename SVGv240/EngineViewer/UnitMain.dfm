object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Engine viewer'
  ClientHeight = 745
  ClientWidth = 1220
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object SVG2Image1: TSVG2Image
    Left = 0
    Top = 0
    Width = 1220
    Height = 649
    Align = alClient
    Color = clWhite
    ParentColor = False
    AnimationTimer = SVG2AnimationTimer1
    OnAfterParse = SVG2Image1AfterParse
  end
  object pnlControls: TPanel
    Left = 0
    Top = 649
    Width = 1220
    Height = 96
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      1220
      96)
    object lblSpeedCaption: TLabel
      Left = 24
      Top = 22
      Width = 32
      Height = 15
      Caption = 'Speed'
    end
    object lblReadout: TLabel
      Left = 24
      Top = 60
      Width = 41
      Height = 15
      Caption = 'readout'
    end
    object tbSpeed: TTrackBar
      Left = 72
      Top = 16
      Width = 985
      Height = 33
      Anchors = [akLeft, akTop, akRight]
      Max = 300
      Frequency = 25
      Position = 100
      PositionToolTip = ptBottom
      TabOrder = 0
      OnChange = tbSpeedChange
    end
    object btnReset: TButton
      Left = 1076
      Top = 18
      Width = 120
      Height = 29
      Anchors = [akTop, akRight]
      Caption = 'Reset to 100 %'
      TabOrder = 1
      OnClick = btnResetClick
    end
  end
  object SVG2AnimationTimer1: TSVG2AnimationTimer
    SampleInterval = 125
    Left = 792
    Top = 592
  end
  object tmrTick: TTimer
    Enabled = False
    Interval = 15
    OnTimer = tmrTickTimer
    Left = 720
    Top = 592
  end
  object OpenDialog1: TOpenDialog
    Filter = 'SVG documents|*.svg|All files|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Title = 'Locate engine-cutaway-animated.svg'
    Left = 648
    Top = 592
  end
end
