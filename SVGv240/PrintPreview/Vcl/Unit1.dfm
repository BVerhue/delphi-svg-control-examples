object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 466
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object SVG2Image1: TSVG2Image
    Left = 0
    Top = 0
    Width = 635
    Height = 466
    Align = alClient
    ExplicitLeft = 216
    ExplicitTop = 176
    ExplicitWidth = 105
    ExplicitHeight = 105
  end
  object ActionManager1: TActionManager
    Left = 56
    Top = 40
    StyleName = 'Platform Default'
    object aLoadSVG: TAction
      Caption = 'Load SVG...'
      OnExecute = aLoadSVGExecute
    end
    object aPrintSVG: TAction
      Caption = 'Print...'
      OnExecute = aPrintSVGExecute
    end
    object aQuit: TAction
      Caption = 'Quit'
      OnExecute = aQuitExecute
    end
  end
  object MainMenu1: TMainMenu
    Left = 144
    Top = 40
    object File1: TMenuItem
      Caption = 'File'
      object LoadSVG1: TMenuItem
        Action = aLoadSVG
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Print1: TMenuItem
        Action = aPrintSVG
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Quit1: TMenuItem
        Action = aQuit
      end
    end
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Filter = 'Scalable Vector Graphics (*.svg)|*.svg'
    Left = 239
    Top = 40
  end
end
