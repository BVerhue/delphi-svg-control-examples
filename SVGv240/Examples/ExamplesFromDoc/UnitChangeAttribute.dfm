object frmChangeAttribute: TfrmChangeAttribute
  Left = 0
  Top = 0
  Caption = 'frmChangeAttribute'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  DoubleBuffered = True
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
    Left = 0
    Top = 0
    Width = 635
    Height = 299
    Align = alClient
    OnAfterParse = SVG2Image1AfterParse
    ExplicitLeft = 112
    ExplicitTop = 88
    ExplicitWidth = 105
    ExplicitHeight = 105
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 25
    OnTimer = Timer1Timer
    Left = 16
    Top = 16
  end
end
