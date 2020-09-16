object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 507
  ClientWidth = 708
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
    Left = 32
    Top = 24
    Width = 561
    Height = 401
    SVG.Strings = (
      '<?xml version="1.0" encoding="UTF-8" standalone="no"?>'
      '<svg'
      '   xmlns:svg="http://www.w3.org/2000/svg"'
      '   xmlns="http://www.w3.org/2000/svg"'
      '   version="1.1"'
      '   width="255.59375"'
      '   height="180.53125"'
      '   id="svg2">'
      '  <g'
      '     transform="translate(-76.4375,-50.0625)"'
      '     id="layer1">'
      '    <path'
      
        '       d="M 128.57143,72.362183 C 78.361562,119.92943 57.142857,' +
        '218.07647 122.85714,223.79075 c 65.71429,5.71429 94.28572,0 97.1' +
        '4286,-42.85714 2.85714,-42.85714 48.57143,42.85714 85.71429,-17.' +
        '14286 37.14285,-60 23.59099,-52.29371 0,-85.714282 C 271.42858,2' +
        '9.50504 241.03297,105.91716 207.14286,103.79075 178.99021,102.02' +
        '434 182.85714,20.933611 128.57143,72.362183 z"'
      '       id="path2989"'
      
        '       style="fill:none;stroke:#000000;stroke-width:10.5;stroke-' +
        'linecap:butt;stroke-linejoin:miter;stroke-miterlimit:4;stroke-op' +
        'acity:1;stroke-dasharray:42, 42;stroke-dashoffset:0" />'
      '  </g>'
      '</svg>')
  end
  object Timer1: TTimer
    Interval = 10
    OnTimer = Timer1Timer
    Left = 632
    Top = 32
  end
end
