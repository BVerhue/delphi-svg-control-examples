object frmPrintPreview: TfrmPrintPreview
  Left = 0
  Top = 0
  Caption = 'Print preview'
  ClientHeight = 641
  ClientWidth = 675
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 249
    Top = 0
    Height = 641
    ExplicitLeft = 328
    ExplicitTop = 328
    ExplicitHeight = 100
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 249
    Height = 641
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 13
      Width = 68
      Height = 13
      Caption = 'Output device'
    end
    object eOutputDevice: TEdit
      Left = 16
      Top = 32
      Width = 209
      Height = 21
      Color = clBtnFace
      TabOrder = 0
    end
    object Button1: TButton
      Left = 16
      Top = 64
      Width = 97
      Height = 25
      Action = aPrinterSelect
      TabOrder = 1
    end
    object Panel2: TPanel
      Left = 0
      Top = 610
      Width = 249
      Height = 31
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
      object Button2: TButton
        AlignWithMargins = True
        Left = 90
        Top = 3
        Width = 75
        Height = 25
        Action = aPrint
        Align = alRight
        TabOrder = 0
      end
      object Button3: TButton
        AlignWithMargins = True
        Left = 171
        Top = 3
        Width = 75
        Height = 25
        Action = aCancel
        Align = alRight
        TabOrder = 1
      end
    end
    object RadioGroup1: TRadioGroup
      Left = 16
      Top = 103
      Width = 209
      Height = 58
      Caption = 'Orientation'
      TabOrder = 3
    end
    object RadioButton1: TRadioButton
      Left = 40
      Top = 128
      Width = 73
      Height = 17
      Action = aOrientationPortait
      TabOrder = 4
      TabStop = True
    end
    object RadioButton2: TRadioButton
      Left = 120
      Top = 128
      Width = 73
      Height = 17
      Action = aOrientationLandscape
      TabOrder = 5
    end
    object GroupBox1: TGroupBox
      Left = 16
      Top = 167
      Width = 209
      Height = 162
      Caption = 'Margins'
      TabOrder = 6
      object Label4: TLabel
        Left = 21
        Top = 56
        Width = 19
        Height = 13
        Caption = 'Left'
      end
      object Label5: TLabel
        Left = 108
        Top = 56
        Width = 18
        Height = 13
        Caption = 'Top'
      end
      object Label6: TLabel
        Left = 21
        Top = 83
        Width = 25
        Height = 13
        Caption = 'Right'
      end
      object Label7: TLabel
        Left = 108
        Top = 83
        Width = 34
        Height = 13
        Caption = 'Bottom'
      end
      object Label8: TLabel
        Left = 21
        Top = 124
        Width = 48
        Height = 13
        Caption = 'Glue edge'
      end
      object Label9: TLabel
        Left = 21
        Top = 24
        Width = 24
        Height = 13
        Caption = 'Units'
      end
      object eMarginTop: TEdit
        Left = 153
        Top = 53
        Width = 32
        Height = 21
        Alignment = taRightJustify
        TabOrder = 2
        Text = '0'
      end
      object eMarginRight: TEdit
        Left = 61
        Top = 80
        Width = 32
        Height = 21
        Alignment = taRightJustify
        TabOrder = 3
        Text = '0'
      end
      object eMarginBottom: TEdit
        Left = 154
        Top = 80
        Width = 32
        Height = 21
        Alignment = taRightJustify
        TabOrder = 4
        Text = '0'
      end
      object eGlueEdge: TEdit
        Left = 94
        Top = 121
        Width = 32
        Height = 21
        Alignment = taRightJustify
        TabOrder = 5
        Text = '0'
      end
      object cbUnits: TComboBox
        Left = 61
        Top = 21
        Width = 55
        Height = 21
        TabOrder = 0
        Text = 'mm'
      end
      object eMarginLeft: TEdit
        Left = 61
        Top = 53
        Width = 32
        Height = 21
        Alignment = taRightJustify
        TabOrder = 1
        Text = '0'
      end
    end
    object GroupBox2: TGroupBox
      Left = 16
      Top = 464
      Width = 209
      Height = 105
      Caption = 'Pages'
      TabOrder = 7
      object Label2: TLabel
        Left = 21
        Top = 32
        Width = 48
        Height = 13
        Caption = 'Horizontal'
      end
      object Label3: TLabel
        Left = 21
        Top = 59
        Width = 35
        Height = 13
        Caption = 'Vertical'
      end
      object ePagesHorizontal: TEdit
        Left = 94
        Top = 29
        Width = 32
        Height = 21
        Alignment = taRightJustify
        TabOrder = 0
        Text = '1'
      end
      object ePagesVertical: TEdit
        Left = 94
        Top = 56
        Width = 32
        Height = 21
        Alignment = taRightJustify
        TabOrder = 1
        Text = '1'
      end
    end
    object GroupBox3: TGroupBox
      Left = 16
      Top = 335
      Width = 209
      Height = 123
      Caption = 'Scale / Aspect ratio'
      TabOrder = 8
      object Label10: TLabel
        Left = 21
        Top = 58
        Width = 23
        Height = 13
        Caption = 'Align'
      end
      object Label11: TLabel
        Left = 21
        Top = 85
        Width = 60
        Height = 13
        Caption = 'Meet or slice'
      end
      object cbAutoViewbox: TCheckBox
        Left = 21
        Top = 32
        Width = 96
        Height = 17
        Caption = 'Auto viewbox'
        TabOrder = 0
      end
      object cbAlign: TComboBox
        Left = 94
        Top = 55
        Width = 88
        Height = 21
        TabOrder = 1
      end
      object cbMeetOrSlice: TComboBox
        Left = 94
        Top = 82
        Width = 87
        Height = 21
        TabOrder = 2
      end
    end
  end
  object PageSetupDialog1: TPageSetupDialog
    MinMarginLeft = 0
    MinMarginTop = 0
    MinMarginRight = 0
    MinMarginBottom = 0
    MarginLeft = 2500
    MarginTop = 2500
    MarginRight = 2500
    MarginBottom = 2500
    PageWidth = 21000
    PageHeight = 29700
    Left = 256
    Top = 120
  end
  object ActionManager1: TActionManager
    Left = 256
    Top = 168
    StyleName = 'Platform Default'
    object aOrientationLandscape: TAction
      Caption = 'Landscape'
      GroupIndex = 1
    end
    object aOrientationPortait: TAction
      Caption = 'Portrait'
      Checked = True
      GroupIndex = 1
    end
    object aPrint: TAction
      Caption = 'Print'
    end
    object aCancel: TAction
      Caption = 'Cancel'
    end
    object aPrinterSelect: TAction
      Caption = 'Select printer...'
    end
  end
end
