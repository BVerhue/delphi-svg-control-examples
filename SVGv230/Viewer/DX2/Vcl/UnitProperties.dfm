object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'SVG properties'
  ClientHeight = 465
  ClientWidth = 719
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
  object Panel1: TPanel
    Left = 0
    Top = 23
    Width = 719
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    Caption = 'Panel1'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 48
    Width = 719
    Height = 417
    ActivePage = TabSheet1
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'Structure'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      object Splitter1: TSplitter
        Left = 265
        Top = 0
        Height = 389
        ExplicitLeft = 216
        ExplicitTop = -2
        ExplicitHeight = 371
      end
      object Panel3: TPanel
        Left = 268
        Top = 0
        Width = 443
        Height = 389
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object Splitter2: TSplitter
          Left = 0
          Top = 265
          Width = 443
          Height = 3
          Cursor = crVSplit
          Align = alBottom
          ExplicitTop = 0
          ExplicitWidth = 282
        end
        object ListView1: TListView
          Left = 0
          Top = 0
          Width = 443
          Height = 265
          Align = alClient
          Columns = <
            item
              Caption = 'Attribute'
              Width = 150
            end
            item
              Caption = 'Value'
              Width = 250
            end>
          TabOrder = 0
          ViewStyle = vsReport
        end
        object Panel4: TPanel
          Left = 0
          Top = 268
          Width = 443
          Height = 121
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 1
          object Memo2: TMemo
            Left = 0
            Top = 0
            Width = 443
            Height = 98
            Align = alClient
            ScrollBars = ssBoth
            TabOrder = 0
          end
          object ActionToolBar2: TActionToolBar
            Left = 0
            Top = 98
            Width = 443
            Height = 23
            ActionManager = ActionManager1
            Align = alBottom
            Caption = 'ActionToolBar2'
            Color = clMenuBar
            ColorMap.DisabledFontColor = 7171437
            ColorMap.HighlightColor = clWhite
            ColorMap.BtnSelectedFont = clBlack
            ColorMap.UnusedColor = clWhite
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            Spacing = 0
          end
        end
      end
      object TreeView1: TTreeView
        Left = 0
        Top = 0
        Width = 265
        Height = 389
        Align = alLeft
        Indent = 19
        TabOrder = 1
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Text'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ImageIndex = 1
      ParentFont = False
      object Memo1: TMemo
        Left = 0
        Top = 0
        Width = 711
        Height = 389
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          'Memo1')
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Options'
      ImageIndex = 2
      object GroupBox1: TGroupBox
        Left = 0
        Top = 0
        Width = 711
        Height = 129
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Parse options'
        Padding.Left = 10
        Padding.Top = 10
        Padding.Right = 10
        Padding.Bottom = 10
        TabOrder = 0
        object cbFilters: TCheckBox
          Left = 12
          Top = 25
          Width = 687
          Height = 20
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          Caption = 'Filters'
          TabOrder = 0
        end
        object cbClippaths: TCheckBox
          Left = 12
          Top = 45
          Width = 687
          Height = 20
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          Caption = 'Clippaths'
          Checked = True
          State = cbChecked
          TabOrder = 1
        end
        object cbAutoViewbox: TCheckBox
          Left = 12
          Top = 65
          Width = 687
          Height = 17
          Align = alTop
          Caption = 'Auto viewbox'
          Checked = True
          State = cbChecked
          TabOrder = 2
        end
        object cbEvents: TCheckBox
          Left = 12
          Top = 82
          Width = 687
          Height = 17
          Align = alTop
          Caption = 'Events'
          TabOrder = 3
        end
      end
      object Memo3: TMemo
        Left = 0
        Top = 129
        Width = 711
        Height = 260
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 1
      end
    end
  end
  object ActionToolBar1: TActionToolBar
    Left = 0
    Top = 0
    Width = 719
    Height = 23
    ActionManager = ActionManager1
    Caption = 'ActionToolBar1'
    Color = clMenuBar
    ColorMap.DisabledFontColor = 7171437
    ColorMap.HighlightColor = clWhite
    ColorMap.BtnSelectedFont = clBlack
    ColorMap.UnusedColor = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Spacing = 0
  end
  object ActionManager1: TActionManager
    ActionBars = <
      item
        Items = <
          item
            Action = aParseSVG
            Caption = '&Parse SVG'
          end>
        ActionBar = ActionToolBar1
      end
      item
        Items = <
          item
            Action = aAttributeChange
            Caption = '&Change'
          end>
        ActionBar = ActionToolBar2
      end>
    Left = 496
    Top = 80
    StyleName = 'Platform Default'
    object aParseSVG: TAction
      Caption = 'Parse SVG'
    end
    object aAttributeChange: TAction
      Caption = 'Change'
    end
  end
end
