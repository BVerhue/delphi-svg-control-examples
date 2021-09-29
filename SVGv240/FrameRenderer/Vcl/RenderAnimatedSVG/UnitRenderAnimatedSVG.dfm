object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Render animated SVG'
  ClientHeight = 652
  ClientWidth = 960
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 15
  object Splitter1: TSplitter
    Left = 249
    Top = 0
    Height = 652
    ExplicitLeft = 488
    ExplicitTop = 272
    ExplicitHeight = 100
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 249
    Height = 652
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object GroupBox1: TGroupBox
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 243
      Height = 454
      Align = alTop
      Caption = 'SVG settings'
      TabOrder = 0
      object GroupBox4: TGroupBox
        AlignWithMargins = True
        Left = 5
        Top = 295
        Width = 233
        Height = 76
        Align = alTop
        Caption = 'Render options'
        TabOrder = 0
        object cbClippaths: TCheckBox
          Left = 94
          Top = 24
          Width = 87
          Height = 17
          Caption = 'Clippaths'
          Checked = True
          State = cbChecked
          TabOrder = 0
          OnClick = cbClippathsClick
        end
        object cbFilters: TCheckBox
          Left = 94
          Top = 47
          Width = 87
          Height = 17
          Caption = 'Filters'
          Checked = True
          State = cbChecked
          TabOrder = 1
          OnClick = cbFiltersClick
        end
      end
      object GroupBox5: TGroupBox
        AlignWithMargins = True
        Left = 5
        Top = 183
        Width = 233
        Height = 106
        Align = alTop
        Caption = 'Aspect ratio'
        TabOrder = 1
        object Label12: TLabel
          Left = 15
          Top = 43
          Width = 28
          Height = 15
          Caption = 'Align'
        end
        object Label13: TLabel
          Left = 15
          Top = 72
          Width = 67
          Height = 15
          Caption = 'Meet or slice'
        end
        object cbAutoViewbox: TCheckBox
          Left = 96
          Top = 17
          Width = 97
          Height = 17
          Caption = 'Auto viewbox'
          TabOrder = 0
          OnClick = cbAutoViewboxClick
        end
        object cbAspectRatioAlign: TComboBox
          Left = 96
          Top = 40
          Width = 103
          Height = 23
          ItemIndex = 5
          TabOrder = 1
          Text = 'X-Mid Y-Mid'
          OnChange = cbAspectRatioAlignChange
          Items.Strings = (
            'None'
            'X-Min Y-Min'
            'X-Mid Y-Min'
            'X-Max Y-Min'
            'X-Min Y-Mid'
            'X-Mid Y-Mid'
            'X-Max Y-Mid'
            'X-Min Y-Max'
            'X-Mid Y-Max'
            'X-Max Y-Max')
        end
        object cbAspectRatioMeetOrSlice: TComboBox
          Left = 96
          Top = 69
          Width = 103
          Height = 23
          ItemIndex = 0
          TabOrder = 2
          Text = 'Meet'
          OnChange = cbAspectRatioMeetOrSliceChange
          Items.Strings = (
            'Meet'
            'Slice')
        end
      end
      object GroupBox6: TGroupBox
        AlignWithMargins = True
        Left = 5
        Top = 377
        Width = 233
        Height = 56
        Align = alTop
        Caption = 'Background color'
        TabOrder = 2
        object cbBackgroundColor: TColorBox
          Left = 95
          Top = 19
          Width = 103
          Height = 22
          DefaultColorColor = clNone
          NoneColorColor = clNone
          Selected = clNone
          Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone]
          TabOrder = 0
          OnClick = cbBackgroundColorClick
        end
      end
      object GroupBox7: TGroupBox
        AlignWithMargins = True
        Left = 5
        Top = 95
        Width = 233
        Height = 82
        Align = alTop
        Caption = 'Size'
        TabOrder = 3
        object Label5: TLabel
          Left = 15
          Top = 22
          Width = 32
          Height = 15
          Caption = 'Width'
        end
        object Label7: TLabel
          Left = 15
          Top = 51
          Width = 36
          Height = 15
          Caption = 'Height'
        end
        object eWidth: TEdit
          Left = 96
          Top = 19
          Width = 103
          Height = 23
          NumbersOnly = True
          TabOrder = 0
          Text = '480'
          OnExit = eWidthExit
        end
        object eHeight: TEdit
          Left = 96
          Top = 48
          Width = 103
          Height = 23
          NumbersOnly = True
          TabOrder = 1
          Text = '360'
          OnExit = eHeightExit
        end
      end
      object GroupBox2: TGroupBox
        AlignWithMargins = True
        Left = 5
        Top = 20
        Width = 233
        Height = 69
        Align = alTop
        Caption = 'File'
        TabOrder = 4
        object lblFilenameIn: TLabel
          Left = 95
          Top = 31
          Width = 9
          Height = 15
          Caption = '...'
        end
        object Button1: TButton
          Left = 21
          Top = 27
          Width = 68
          Height = 25
          Action = aOpenSVG
          TabOrder = 0
        end
      end
    end
    object PageControl1: TPageControl
      AlignWithMargins = True
      Left = 3
      Top = 463
      Width = 243
      Height = 186
      Align = alClient
      TabOrder = 1
    end
  end
  object Panel3: TPanel
    Left = 252
    Top = 0
    Width = 708
    Height = 652
    Align = alClient
    BevelOuter = bvNone
    Color = clWindow
    ParentBackground = False
    TabOrder = 1
    object Image1: TImage
      Left = 0
      Top = 64
      Width = 708
      Height = 547
      Align = alClient
      Center = True
      ExplicitLeft = 280
      ExplicitTop = 192
      ExplicitWidth = 105
      ExplicitHeight = 105
    end
    object Panel2: TPanel
      Left = 0
      Top = 611
      Width = 708
      Height = 41
      Align = alBottom
      BevelOuter = bvNone
      ParentBackground = False
      TabOrder = 0
      object Label3: TLabel
        Left = 22
        Top = 13
        Width = 110
        Height = 15
        Caption = 'Animation time (ms)'
      end
      object eTime: TEdit
        Left = 145
        Top = 10
        Width = 72
        Height = 23
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 0
        Text = '0'
      end
    end
    object GroupBox3: TGroupBox
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 702
      Height = 58
      Align = alTop
      Caption = 'Recording'
      Color = clBtnFace
      ParentBackground = False
      ParentColor = False
      TabOrder = 1
      object Label1: TLabel
        Left = 23
        Top = 21
        Width = 73
        Height = 15
        Caption = 'Duration (ms)'
      end
      object Label2: TLabel
        Left = 181
        Top = 21
        Width = 99
        Height = 15
        Caption = 'Frames per second'
      end
      object Button2: TButton
        Left = 334
        Top = 17
        Width = 75
        Height = 25
        Action = aRecord
        TabOrder = 0
      end
      object eDuration: TEdit
        Left = 102
        Top = 18
        Width = 65
        Height = 23
        NumbersOnly = True
        TabOrder = 1
        Text = '2000'
      end
      object eFPS: TEdit
        Left = 286
        Top = 18
        Width = 33
        Height = 23
        NumbersOnly = True
        TabOrder = 2
        Text = '40'
      end
    end
  end
  object ActionManager1: TActionManager
    ActionBars = <
      item
        Items = <
          item
            Action = aOpenSVG
          end
          item
            Action = aRecord
            Caption = '&Record'
          end>
      end>
    Left = 472
    Top = 144
    StyleName = 'Platform Default'
    object aRecord: TAction
      Caption = 'Record'
      OnExecute = aRecordExecute
    end
    object aOpenSVG: TAction
      Caption = 'Open...'
      OnExecute = aOpenSVGExecute
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 376
    Top = 144
  end
end
