object frmFindElementHard: TfrmFindElementHard
  Left = 0
  Top = 0
  Caption = 'frmFindElementHard'
  ClientHeight = 299
  ClientWidth = 635
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
  object SVG2Image1: TSVG2Image
    Left = 185
    Top = 0
    Width = 450
    Height = 299
    Align = alClient
    BufferQuality = bqHighQuality
    AutoViewBox = True
    OnAfterParse = SVG2Image1AfterParse
    ExplicitLeft = 240
    ExplicitTop = 96
    ExplicitWidth = 105
    ExplicitHeight = 105
  end
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 185
    Height = 299
    Align = alLeft
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
    ExplicitLeft = 88
    ExplicitTop = 72
    ExplicitHeight = 89
  end
end
