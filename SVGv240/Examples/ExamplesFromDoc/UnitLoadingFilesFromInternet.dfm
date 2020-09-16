object frmLoadingFilesFromInternet: TfrmLoadingFilesFromInternet
  Left = 0
  Top = 0
  Caption = 'frmLoadingFilesFromInternet'
  ClientHeight = 477
  ClientWidth = 986
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
    Left = 8
    Top = 78
    Width = 480
    Height = 360
    BufferQuality = bqHighQuality
    AutoViewBox = True
  end
  object Image1: TImage
    Left = 494
    Top = 78
    Width = 480
    Height = 360
  end
  object Label2: TLabel
    Left = 8
    Top = 59
    Width = 50
    Height = 13
    Caption = 'SVG image'
  end
  object Label3: TLabel
    Left = 494
    Top = 59
    Width = 49
    Height = 13
    Caption = 'Png image'
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 986
    Height = 41
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 1
      Top = 1
      Width = 984
      Height = 39
      Align = alClient
      Alignment = taCenter
      Caption = 
        'Requirements for downloading files directly from the internet: D' +
        'efine InternetAccess must be enabeld in CompilerSettings.inc. Fo' +
        'r SSL (https) libeay32.dll and ssleay32.dll must  be available o' +
        'n the system. Download from https://indy.fulgan.com/SSL/'
      WordWrap = True
      ExplicitWidth = 981
      ExplicitHeight = 26
    end
  end
  object Button1: TButton
    Left = 8
    Top = 444
    Width = 75
    Height = 25
    Caption = 'Next'
    TabOrder = 1
    OnClick = Button1Click
  end
end
