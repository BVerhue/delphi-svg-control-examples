unit Unit1;

// ------------------------------------------------------------------------------
//
//                          SVG Control Package 2.0
//                       Copyright (c) 2015 Bruno Verhue
//
// ------------------------------------------------------------------------------

// [The "BSD licence"]
//
//  Copyright (c) 2013 Bruno Verhue
//  All rights reserved.
//
//  Redistribution and use in source and binary forms, with or without
//  modification, are permitted provided that the following conditions
//  are met:
//  1. Redistributions of source code must retain the above copyright
//     notice, this list of conditions and the following disclaimer.
//  2. Redistributions in binary form must reproduce the above copyright
//     notice, this list of conditions and the following disclaimer in the
//     documentation and/or other materials provided with the distribution.
//  3. The name of the author may not be used to endorse or promote products
//     derived from this software without specific prior written permission.
//
//  THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
//  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
//  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
//  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
//  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
//  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
//  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
//  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
//  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
//  THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

///  <summary>
///    Platform specific print preview form definition
///  </summary>
///  <remarks>
///    The SVG Print Preview need at least version v2.40 update 9 of the SVG library
///  </remarks>

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Actions,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.ActnList,
  FMX.Menus,
  BVE.SVG2Control.FMX,
  BVE.SVG2Image.FMX;

type
  TForm1 = class(TForm)
    ActionList1: TActionList;
    MainMenu1: TMainMenu;
    SVG2Image1: TSVG2Image;
    OpenDialog1: TOpenDialog;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    aLoadSVG: TAction;
    aPrintSVG: TAction;
    aQuit: TAction;
    procedure aLoadSVGExecute(Sender: TObject);
    procedure aPrintSVGExecute(Sender: TObject);
    procedure aQuitExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SVG2Image1DragOver(Sender: TObject; const Data: TDragObject;
      const Point: TPointF; var Operation: TDragOperation);
    procedure SVG2Image1DragDrop(Sender: TObject; const Data: TDragObject;
      const Point: TPointF);
  private
    { Private declarations }
  public
    procedure LoadSVG(const aFilename: string);
  end;

var
  Form1: TForm1;

implementation
uses
  UnitPrintPreview;

const
  svg_drop =
  '<?xml version="1.0" standalone="no"?>'
  + '<svg viewBox="0 0 1000 300" xmlns="http://www.w3.org/2000/svg" version="1.1">'
  + '<g font-family="Arial" font-size="45" >'
  + '<text x="500" y="150" fill="black" text-anchor="middle">Drop an SVG...</text>'
  + '</g>'
  + '</svg>';

{$R *.fmx}

procedure TForm1.aLoadSVGExecute(Sender: TObject);
begin
  if OpenDialog1.Execute then
    LoadSVG(OpenDialog1.FileName);
end;

procedure TForm1.aPrintSVGExecute(Sender: TObject);
begin
  frmPrintPreview.Root := SVG2Image1.SVGRoot;
  frmPrintPreview.Show;
end;

procedure TForm1.aQuitExecute(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SVG2Image1.SVG.Text := svg_drop;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
//
end;

procedure TForm1.LoadSVG(const aFilename: string);
begin
  SVG2Image1.SVG.Clear;
  SVG2Image1.Filename := aFilename;
end;

procedure TForm1.SVG2Image1DragDrop(Sender: TObject; const Data: TDragObject;
  const Point: TPointF);
var
  i: integer;
begin
  for i := 0 to Length(Data.Files) - 1 do
  begin
    LoadSVG(Data.Files[i]);
    Break;
  end;
end;

procedure TForm1.SVG2Image1DragOver(Sender: TObject; const Data: TDragObject;
  const Point: TPointF; var Operation: TDragOperation);
begin
  if Length(Data.Files) > 0 then
    Operation := TDragOperation.Copy
  else
    Operation := TDragOperation.None;
end;

end.
