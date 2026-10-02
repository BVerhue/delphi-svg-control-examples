object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'SVG Viewer Info'
  ClientHeight = 407
  ClientWidth = 462
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object SVG2Image1: TSVG2Image
    Left = 0
    Top = 0
    Width = 462
    Height = 373
    Align = alClient
    RenderOptions = [sroClippath, sroEvents]
    SVG.Strings = (
      
        '<svg version="1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink=' +
        ' "http://www.w3.org/1999/xlink"'
      '  viewBox="0 0 500 400">'
      '  '
      '  <defs>'
      
        #9'  <svg id="svg_logo" xmlns="http://www.w3.org/2000/svg" xmlns:x' +
        'link="http://www.w3.org/1999/xlink" viewBox="0 0 330 82">'
      
        #9'  <title>SVG logo combined with the W3C logo, set horizontally<' +
        '/title>'
      
        #9'  <desc>The logo combines three entities displayed horizontally' +
        ': the W3C logo with the text '#39'W3C'#39'; the drawing of a flower or s' +
        'tar shape with eight arms; and the text '#39'SVG'#39'. These three entit' +
        'ies are set horizontally.</desc>'
      #9'  '
      #9'  <metadata>'
      
        #9#9'<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns' +
        '#" xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#" xmlns:cc="' +
        'http://creativecommons.org/ns#" xmlns:xhtml="http://www.w3.org/1' +
        '999/xhtml/vocab#" xmlns:dc="http://purl.org/dc/elements/1.1/">'
      #9#9'  <cc:Work rdf:about="">'
      #9#9#9'<dc:title>SVG logo combined with the W3C logo</dc:title>'
      #9#9#9'<dc:format>image/svg+xml</dc:format>'
      
        #9#9#9'<rdfs:seeAlso rdf:resource="http://www.w3.org/2007/10/sw-logo' +
        's.html"/>'
      #9#9#9'<dc:date>2007-11-01</dc:date>'
      
        #9#9#9'<xhtml:license rdf:resource="http://www.w3.org/Consortium/Leg' +
        'al/2002/copyright-documents-20021231"/>'
      
        #9#9#9'<cc:morePermissions rdf:resource="http://www.w3.org/2007/10/s' +
        'w-logos.html#LogoWithW3C"/>'
      #9#9#9'<cc:attributionURL rdf:reource="http://www.w3.org/2001/sw/"/>'
      
        #9#9#9'<dc:description>The logo combines three entities displayed ho' +
        'rizontally: the W3C logo with the text '#39'W3C'#39'; the drawing of a f' +
        'lower or star shape with eight arms; and the text '#39'SVG'#39'. These t' +
        'hree entities are set horizontally.'
      #9#9#9#9'</dc:description>'
      #9#9'  </cc:Work>'
      #9#9'</rdf:RDF>'
      #9'  </metadata>'
      #9'  '
      
        #9'  <text x="0" y="75" font-size="83" fill-opacity="0" font-famil' +
        'y="Trebuchet" letter-spacing="-12">W3C</text>'
      
        #9'  <text x="180" y="75" font-size="83" fill-opacity="0" font-fam' +
        'ily="Trebuchet" font-weight="bold">SVG</text>'
      #9'  <defs>'
      #9#9'<g id="SVG" fill="#005A9C">'
      
        #9#9'  <path id="S" d="M 5.482,31.319 C2.163,28.001 0.109,23.419 0.' +
        '109,18.358 C0.109,8.232 8.322,0.024 18.443,0.024 C28.569,0.024 3' +
        '6.782,8.232 36.782,18.358 L26.042,18.358 C26.042,14.164 22.638,1' +
        '0.765 18.443,10.765 C14.249,10.765 10.850,14.164 10.850,18.358 C' +
        '10.850,20.453 11.701,22.351 13.070,23.721 L13.075,23.721 C14.450' +
        ',25.101 15.595,25.500 18.443,25.952 L18.443,25.952 C23.509,26.47' +
        '9 28.091,28.006 31.409,31.324 L31.409,31.324 C34.728,34.643 36.7' +
        '82,39.225 36.782,44.286 C36.782,54.412 28.569,62.625 18.443,62.6' +
        '25 C8.322,62.625 0.109,54.412 0.109,44.286 L10.850,44.286 C10.85' +
        '0,48.480 14.249,51.884 18.443,51.884 C22.638,51.884 26.042,48.48' +
        '0 26.042,44.286 C26.042,42.191 25.191,40.298 23.821,38.923 L23.8' +
        '16,38.923 C22.441,37.548 20.468,37.074 18.443,36.697 L18.443,36.' +
        '692 C13.533,35.939 8.800,34.638 5.482,31.319 L5.482,31.319 L5.48' +
        '2,31.319 Z"/>'
      
        #9#9'  <path id="V" d="M 73.452,0.024 L60.482,62.625 L49.742,62.625' +
        ' L36.782,0.024 L47.522,0.024 L55.122,36.687 L62.712,0.024 L73.45' +
        '2,0.024 Z"/>'
      
        #9#9'  <path id="G" d="M 91.792,25.952 L110.126,25.952 L110.126,44.' +
        '286 L110.131,44.286 C110.131,54.413 101.918,62.626 91.792,62.626' +
        ' C81.665,62.626 73.458,54.413 73.458,44.286 L73.458,44.286 L73.4' +
        '58,18.359 L73.453,18.359 C73.453,8.233 81.665,0.025 91.792,0.025' +
        ' C101.913,0.025 110.126,8.233 110.126,18.359 L99.385,18.359 C99.' +
        '385,14.169 95.981,10.765 91.792,10.765 C87.597,10.765 84.198,14.' +
        '169 84.198,18.359 L84.198,44.286 L84.198,44.286 C84.198,48.481 8' +
        '7.597,51.880 91.792,51.880 C95.981,51.880 99.380,48.481 99.385,4' +
        '4.291 L99.385,44.286 L99.385,36.698 L91.792,36.698 L91.792,25.95' +
        '2 L91.792,25.952 Z"/>'
      #9#9'</g>'
      #9'  </defs>'
      
        #9'  <g shape-rendering="geometricPrecision" text-rendering="geome' +
        'tricPrecision" image-rendering="optimizeQuality">'
      #9#9'<g>'
      #9#9'  <g id="w3c-logo">'
      #9#9#9'<g>'
      #9#9#9'  <title>W3</title>'
      
        #9#9#9'  <path d="M33.695,10.802l12.062,41.016l12.067-41.016h8.731L4' +
        '6.587,78.188h-0.831l-12.48-41.759L20.797,78.188     h-0.832L0,10' +
        '.802h8.736l12.061,41.016l8.154-27.618l-3.993-13.397H33.695z" fil' +
        'l="#005A9C"/>'
      
        #9#9#9'  <path d="M91.355,56.557c0,6.104-1.624,11.234-4.862,15.394c-' +
        '3.248,4.158-7.45,6.237-12.607,6.237     c-3.882,0-7.263-1.238-10' +
        '.148-3.702c-2.885-2.47-5.02-5.812-6.406-10.022l6.82-2.829c1.001,' +
        '2.552,2.317,4.562,3.953,6.028     c1.636,1.469,3.56,2.207,5.781,' +
        '2.207c2.329,0,4.3-1.306,5.909-3.911c1.609-2.606,2.411-5.738,2.41' +
        '1-9.401     c0-4.049-0.861-7.179-2.582-9.399c-1.995-2.604-5.129-' +
        '3.912-9.397-3.912h-3.327v-3.991l11.646-20.133H64.484l-3.911,6.65' +
        '5h-2.493     V10.802h32.441v4.075l-12.31,21.217c4.324,1.385,7.59' +
        '6,3.911,9.815,7.571C90.246,47.324,91.355,51.618,91.355,56.557z" ' +
        'fill="#005A9C"/>'
      #9#9#9'</g>'
      #9#9#9'<g>'
      #9#9#9'  <title>C</title>'
      
        #9#9#9'  <path d="M125.211,10.425l1.414,8.6l-5.008,9.583c0,0-1.924-4' +
        '.064-5.117-6.314     c-2.693-1.899-4.447-2.309-7.186-1.746c-3.52' +
        '7,0.73-7.516,4.938-9.258,10.13c-2.084,6.21-2.104,9.218-2.178,11.' +
        '978     c-0.115,4.428,0.58,7.043,0.58,7.043s-3.04-5.626-3.011-13' +
        '.866c0.018-5.882,0.947-11.218,3.666-16.479     c2.396-4.627,5.95' +
        '-7.404,9.109-7.728c3.264-0.343,5.848,1.229,7.841,2.938c2.089,1.7' +
        '88,4.213,5.698,4.213,5.698L125.211,10.425z" fill="#221B0A"/>'
      
        #9#9#9'  <path d="M125.823,59.099c0,0-2.208,3.957-3.589,5.48c-1.379,' +
        '1.524-3.849,4.209-6.896,5.555     c-3.049,1.343-4.646,1.598-7.66' +
        '1,1.306c-3.01-0.29-5.807-2.032-6.786-2.764c-0.979-0.722-3.486-2.' +
        '864-4.897-4.854     c-1.42-2-3.634-5.995-3.634-5.995s1.233,4.001' +
        ',2.007,5.699c0.442,0.977,1.81,3.965,3.749,6.572     c1.805,2.425' +
        ',5.315,6.604,10.652,7.545c5.336,0.945,9.002-1.449,9.907-2.031c0.' +
        '907-0.578,2.819-2.178,4.032-3.475     c1.264-1.351,2.459-3.079,3' +
        '.116-4.108c0.487-0.758,1.276-2.286,1.276-2.286L125.823,59.099z" ' +
        'fill="#221B0A"/>'
      #9#9#9'</g>'
      #9#9#9'<g>'
      #9#9#9'  <title>Registered Trademark</title>'
      
        #9#9#9'  <path d="M132.592,5.201c2.493,0,4.485,2.032,4.485,4.525c0,2' +
        '.533-1.992,4.543-4.505,4.543    c-2.491,0-4.524-2.01-4.524-4.543' +
        'c0-2.493,2.033-4.525,4.524-4.525H132.592z M132.554,6.107c-1.889,' +
        '0-3.417,1.629-3.417,3.639    c0,2.029,1.528,3.619,3.436,3.619c1.' +
        '912,0.019,3.46-1.59,3.46-3.619c0-2.01-1.548-3.639-3.46-3.639H132' +
        '.554z M131.791,12.361    h-1.067V7.332c0.401-0.058,0.846-0.141,1' +
        '.61-0.141c0.862,0,1.387,0.141,1.726,0.404c0.28,0.221,0.445,0.563' +
        ',0.445,1.085    c0,0.603-0.423,1.024-0.966,1.166v0.042c0.441,0.0' +
        '78,0.724,0.479,0.801,1.226c0.103,0.783,0.203,1.085,0.284,1.247h-' +
        '1.104    c-0.123-0.183-0.203-0.625-0.305-1.31c-0.077-0.542-0.4-0' +
        '.763-0.942-0.763h-0.481V12.361z M131.791,9.463h0.5    c0.624,0,1' +
        '.105-0.199,1.105-0.723c0-0.421-0.301-0.744-1.025-0.744c-0.261,0-' +
        '0.441,0-0.58,0.021V9.463z" fill="#221B0A"/>'
      #9#9#9'</g>'
      #9#9'  </g>'
      #9#9'  <g id="logo" transform="scale(0.24) translate(550, 35)">'
      #9#9#9'<g stroke-width="38.0086" stroke="#000">'
      #9#9#9'  <g id="svgstar" transform="translate(150, 150)">'
      
        #9#9#9#9'<path id="svgbar" fill="#EDA921" d="M-84.1487,-15.8513 a22.4' +
        '171,22.4171 0 1 0 0,31.7026 h168.2974 a22.4171,22.4171 0 1 0 0,-' +
        '31.7026 Z"/>'
      #9#9#9#9'<use xlink:href="#svgbar" transform="rotate(45)"/>'
      #9#9#9#9'<use xlink:href="#svgbar" transform="rotate(90)"/>'
      #9#9#9#9'<use xlink:href="#svgbar" transform="rotate(135)"/>'
      #9#9#9'  </g>'
      #9#9#9'</g>'
      #9#9#9'<use xlink:href="#svgstar"/>'
      #9#9'  </g>'
      #9#9'  <g id="SVG-label">'
      
        #9#9#9'<use xlink:href="#SVG" transform="scale(1.08) translate(195,1' +
        '0)"/>'
      #9#9'  </g>'
      #9#9'</g>'
      #9'  </g>'
      #9'</svg>'
      '  </defs>'
      '    '
      #9
      
        '  <rect x="10" y="10" width="480" height="360" fill="none" strok' +
        'e="black"/>'
      '  '
      
        '  <text x="40" y="100" font-family="roboto" font-size="20">SVG v' +
        'iewer application</text>'
      '  <g font-family="roboto" font-size="16">'
      
        '    <text x="40" y="150">The viewer is made with the "Delphi SVG' +
        ' Control" package,</text>'
      
        #9'<text x="40" y="180">this is a SVG rendering implementation for' +
        ' the Delphi</text>'
      #9'<text x="40" y="210">programming language.</text>'
      #9'<text x="40" y="270">Visit the website for more info: </text>'
      
        #9'<text x="40" y="300" id="link" onmouseover="dummy" onmouseout="' +
        'dummy" onclick="dummy">www.bverhue.nl/delphisvg/</text>'#9
      '  </g>    '
      '  <g font-family="roboto" font-size="10">'
      
        '    <text x="40" y="350">SVG icons from https://github.com/icons' +
        '8/flat-color-icons/tree/master/svg</text>'
      '  </g>'
      
        '  <image id="img_svg_logo" onmousedown="dummy" onmouseup="dummy"' +
        ' x="280" y="10" width="200" height="80" xlink:href="url(#svg_log' +
        'o)"  />  '
      '  '
      '</svg>')
    ExplicitLeft = 224
    ExplicitTop = 104
    ExplicitWidth = 100
    ExplicitHeight = 41
  end
  object Panel1: TPanel
    Left = 0
    Top = 373
    Width = 462
    Height = 34
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Button1: TButton
      AlignWithMargins = True
      Left = 384
      Top = 3
      Width = 75
      Height = 28
      Align = alRight
      Caption = 'Ok'
      ModalResult = 1
      TabOrder = 0
    end
  end
end
