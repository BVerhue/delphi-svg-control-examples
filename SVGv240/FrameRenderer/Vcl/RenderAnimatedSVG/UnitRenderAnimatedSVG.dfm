object frmRenderAnimatedSVG: TfrmRenderAnimatedSVG
  Left = 0
  Top = 0
  Caption = 'Render animated SVG'
  ClientHeight = 500
  ClientWidth = 811
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
    Height = 500
    ExplicitLeft = 488
    ExplicitTop = 272
    ExplicitHeight = 100
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 249
    Height = 500
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object TabControl1: TTabControl
      Left = 0
      Top = 217
      Width = 249
      Height = 283
      Align = alClient
      TabOrder = 0
      OnChange = TabControl1Change
      ExplicitLeft = -16
      ExplicitTop = 152
      ExplicitWidth = 289
      ExplicitHeight = 193
      object vleTarget: TValueListEditor
        Left = 4
        Top = 6
        Width = 241
        Height = 273
        Align = alClient
        TabOrder = 0
        OnEditButtonClick = vleTargetEditButtonClick
        OnValidate = vleTargetValidate
        ExplicitHeight = 225
        ColWidths = (
          111
          124)
      end
    end
    object vleSVG: TValueListEditor
      Left = 0
      Top = 0
      Width = 249
      Height = 217
      Align = alTop
      TabOrder = 1
      OnEditButtonClick = vleSVGEditButtonClick
      OnValidate = vleSVGValidate
      ColWidths = (
        119
        124)
    end
  end
  object Panel3: TPanel
    Left = 252
    Top = 0
    Width = 559
    Height = 500
    Align = alClient
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 1
    object PaintBox1: TPaintBox
      Left = 0
      Top = 64
      Width = 559
      Height = 395
      Align = alClient
      OnPaint = PaintBox1Paint
      ExplicitLeft = 352
      ExplicitTop = 256
      ExplicitWidth = 105
      ExplicitHeight = 105
    end
    object Panel2: TPanel
      Left = 0
      Top = 459
      Width = 559
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
      Width = 553
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
    Left = 376
    Top = 96
    StyleName = 'Platform Default'
    object aRecord: TAction
      Caption = 'Record'
      OnExecute = aRecordExecute
    end
    object aOpenSVG: TAction
      Caption = 'Open...'
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 376
    Top = 144
  end
  object ColorDialog1: TColorDialog
    Left = 372
    Top = 200
  end
end
