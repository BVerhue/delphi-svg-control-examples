object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 459
  ClientWidth = 726
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
    Top = 65
    Width = 726
    Height = 394
    Align = alClient
    OnMouseDown = SVG2Image1MouseDown
    OnMouseUp = SVG2Image1MouseUp
    BufferQuality = bqHighQuality
    AutoViewBox = True
    ExplicitLeft = 32
    ExplicitTop = 56
    ExplicitWidth = 625
    ExplicitHeight = 369
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 726
    Height = 65
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 720
      Height = 13
      Align = alTop
      Caption = 
        '1. Download the cards SVG from this location: http://svg-cards.s' +
        'ourceforge.net/'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 387
    end
    object Label2: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 22
      Width = 720
      Height = 13
      Align = alTop
      Caption = 
        '2. Set the SVG2Image1 FileName property to the downloaded svg fi' +
        'le'
      ExplicitLeft = 0
      ExplicitTop = 13
      ExplicitWidth = 333
    end
    object Label3: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 41
      Width = 720
      Height = 13
      Align = alTop
      Caption = '3. Click on Imag1 to draw a card.'
      ExplicitLeft = 0
      ExplicitTop = 26
      ExplicitWidth = 159
    end
  end
end
