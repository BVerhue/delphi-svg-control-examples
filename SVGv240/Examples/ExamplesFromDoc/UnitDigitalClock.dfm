object frmDigitalClock: TfrmDigitalClock
  Left = 0
  Top = 0
  Caption = 'frmDigitalClock'
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
  PixelsPerInch = 96
  TextHeight = 13
  object SVG2Image1: TSVG2Image
    Left = 0
    Top = 0
    Width = 635
    Height = 299
    Align = alClient
    BufferQuality = bqHighQuality
    AutoViewBox = True
    ExplicitLeft = 352
    ExplicitTop = 96
    ExplicitWidth = 105
    ExplicitHeight = 105
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 16
    Top = 16
  end
end
