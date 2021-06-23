object frmPrintPreview: TfrmPrintPreview
  Left = 0
  Top = 0
  Caption = 'Print preview'
  ClientHeight = 664
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
    Height = 664
    ExplicitLeft = 328
    ExplicitTop = 328
    ExplicitHeight = 100
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 249
    Height = 664
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      AlignWithMargins = True
      Left = 16
      Top = 12
      Width = 230
      Height = 13
      Margins.Left = 16
      Margins.Top = 12
      Align = alTop
      Caption = 'Output device'
      ExplicitWidth = 68
    end
    object eOutputDevice: TEdit
      AlignWithMargins = True
      Left = 16
      Top = 32
      Width = 217
      Height = 21
      Margins.Left = 16
      Margins.Top = 4
      Margins.Right = 16
      Margins.Bottom = 0
      Align = alTop
      Color = clBtnFace
      TabOrder = 0
    end
    object Button1: TButton
      AlignWithMargins = True
      Left = 16
      Top = 57
      Width = 217
      Height = 25
      Margins.Left = 16
      Margins.Top = 4
      Margins.Right = 16
      Margins.Bottom = 0
      Action = aPrinterSelect
      Align = alTop
      TabOrder = 1
    end
    object GroupBox1: TGroupBox
      AlignWithMargins = True
      Left = 16
      Top = 168
      Width = 217
      Height = 198
      Margins.Left = 16
      Margins.Top = 16
      Margins.Right = 16
      Margins.Bottom = 0
      Align = alTop
      Caption = 'Margins'
      TabOrder = 3
      object Label4: TLabel
        Left = 21
        Top = 64
        Width = 23
        Height = 13
        Caption = 'Left:'
      end
      object Label5: TLabel
        Left = 108
        Top = 64
        Width = 22
        Height = 13
        Caption = 'Top:'
      end
      object Label6: TLabel
        Left = 21
        Top = 93
        Width = 29
        Height = 13
        Caption = 'Right:'
      end
      object Label7: TLabel
        Left = 108
        Top = 93
        Width = 38
        Height = 13
        Caption = 'Bottom:'
      end
      object Label8: TLabel
        Left = 21
        Top = 161
        Width = 52
        Height = 13
        Caption = 'Glue edge:'
      end
      object Label9: TLabel
        Left = 21
        Top = 30
        Width = 28
        Height = 13
        Caption = 'Units:'
      end
      object eMarginTop: TEdit
        Left = 153
        Top = 61
        Width = 32
        Height = 21
        Alignment = taCenter
        TabOrder = 2
        Text = '0'
      end
      object eMarginRight: TEdit
        Left = 61
        Top = 90
        Width = 32
        Height = 21
        Alignment = taCenter
        TabOrder = 3
        Text = '0'
      end
      object eMarginBottom: TEdit
        Left = 154
        Top = 90
        Width = 32
        Height = 21
        Alignment = taCenter
        TabOrder = 4
        Text = '0'
      end
      object eGlueEdge: TEdit
        Left = 94
        Top = 158
        Width = 32
        Height = 21
        Alignment = taCenter
        TabOrder = 5
        Text = '0'
      end
      object cbUnits: TComboBox
        Left = 61
        Top = 27
        Width = 55
        Height = 21
        Style = csDropDownList
        TabOrder = 0
      end
      object eMarginLeft: TEdit
        Left = 61
        Top = 61
        Width = 32
        Height = 21
        Alignment = taCenter
        TabOrder = 1
        Text = '0'
      end
      object cbIdenticalMargins: TCheckBox
        Left = 21
        Top = 128
        Width = 97
        Height = 17
        Action = aIdenticalMargins
        State = cbChecked
        TabOrder = 6
      end
    end
    object GroupBox2: TGroupBox
      AlignWithMargins = True
      Left = 16
      Top = 521
      Width = 217
      Height = 100
      Margins.Left = 16
      Margins.Top = 16
      Margins.Right = 16
      Margins.Bottom = 0
      Align = alTop
      Caption = 'Pages'
      TabOrder = 4
      object Label2: TLabel
        Left = 21
        Top = 32
        Width = 52
        Height = 13
        Caption = 'Horizontal:'
      end
      object Label3: TLabel
        Left = 21
        Top = 59
        Width = 39
        Height = 13
        Caption = 'Vertical:'
      end
      object ePagesHorizontal: TEdit
        Left = 94
        Top = 29
        Width = 32
        Height = 21
        Alignment = taCenter
        TabOrder = 0
        Text = '1'
      end
      object ePagesVertical: TEdit
        Left = 94
        Top = 56
        Width = 32
        Height = 21
        Alignment = taCenter
        TabOrder = 1
        Text = '1'
      end
    end
    object GroupBox3: TGroupBox
      AlignWithMargins = True
      Left = 16
      Top = 382
      Width = 217
      Height = 123
      Margins.Left = 16
      Margins.Top = 16
      Margins.Right = 16
      Margins.Bottom = 0
      Align = alTop
      Caption = 'Scale / Aspect ratio'
      TabOrder = 5
      object Label10: TLabel
        Left = 21
        Top = 58
        Width = 27
        Height = 13
        Caption = 'Align:'
      end
      object Label11: TLabel
        Left = 21
        Top = 85
        Width = 64
        Height = 13
        Caption = 'Meet or slice:'
      end
      object cbAutoViewbox: TCheckBox
        Left = 21
        Top = 30
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
        Style = csDropDownList
        TabOrder = 1
      end
      object cbMeetOrSlice: TComboBox
        Left = 94
        Top = 82
        Width = 87
        Height = 21
        Style = csDropDownList
        TabOrder = 2
      end
    end
    object GroupBox4: TGroupBox
      AlignWithMargins = True
      Left = 16
      Top = 98
      Width = 217
      Height = 54
      Margins.Left = 16
      Margins.Top = 16
      Margins.Right = 16
      Margins.Bottom = 0
      Align = alTop
      Caption = 'Orientation'
      TabOrder = 6
      object RadioButton1: TRadioButton
        AlignWithMargins = True
        Left = 18
        Top = 15
        Width = 83
        Height = 31
        Margins.Left = 16
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 6
        Action = aOrientationPortait
        Align = alLeft
        TabOrder = 0
        TabStop = True
      end
      object RadioButton2: TRadioButton
        AlignWithMargins = True
        Left = 111
        Top = 15
        Width = 104
        Height = 31
        Margins.Left = 4
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 6
        Action = aOrientationLandscape
        Align = alRight
        TabOrder = 1
      end
    end
    object Panel2: TPanel
      Left = 0
      Top = 633
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
    object aIdenticalMargins: TAction
      Caption = 'Identical margins'
      Checked = True
    end
  end
end
