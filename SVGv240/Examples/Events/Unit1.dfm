object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 616
  ClientWidth = 690
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object SVG2Image1: TSVG2Image
    Left = 0
    Top = 41
    Width = 690
    Height = 263
    Align = alClient
    BufferQuality = bqHighQuality
    RenderOptions = [sroClippath, sroEvents]
    AutoViewBox = True
    OnSVGEvent = SVG2Image1SVGEvent
    ExplicitHeight = 296
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 690
    Height = 41
    Align = alTop
    TabOrder = 0
    object Button2: TButton
      Left = 19
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Paste SVG'
      TabOrder = 0
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 240
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Test 1'
      TabOrder = 1
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 336
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Test 2'
      TabOrder = 2
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 432
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Test 3'
      TabOrder = 3
      OnClick = Button5Click
    end
  end
  object Memo1: TMemo
    Left = 0
    Top = 304
    Width = 690
    Height = 312
    Align = alBottom
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object OpenDialog1: TOpenDialog
    Left = 88
    Top = 56
  end
end
