object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object SVG2Image1: TSVG2Image
    Left = 0
    Top = 41
    Width = 624
    Height = 400
    Align = alClient
    ExplicitLeft = 56
    ExplicitTop = 40
    ExplicitWidth = 489
    ExplicitHeight = 369
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 624
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object bWarp: TButton
      Left = 116
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Warp'
      TabOrder = 0
      OnClick = bWarpClick
    end
    object cbWarpFunction: TComboBox
      Left = 9
      Top = 10
      Width = 96
      Height = 23
      ItemIndex = 2
      TabOrder = 1
      Text = 'Arcs'
      Items.Strings = (
        'Taper'
        'Perspective'
        'Arcs')
    end
  end
end
