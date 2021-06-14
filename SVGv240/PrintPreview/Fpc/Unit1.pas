unit Unit1;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

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
///    Example application using the print preview form/control
///  </summary>
///  <remarks>
///    The SVG Print Preview need at least version v2.40 update 9 of the SVG library
///  </remarks>

interface

uses
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ActnList,
  Menus,
  ExtDlgs,
  BVE.SVG2Control.FPC,
  BVE.SVG2Image.FPC;

type

  { TForm1 }

  TForm1 = class(TForm)
    aLoadSVG: TAction;
    aPrintSVG: TAction;
    aQuit: TAction;
    SVG2Image1: TSVG2Image;
    ActionList1: TActionList;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    LoadSVG1: TMenuItem;
    N1: TMenuItem;
    Print1: TMenuItem;
    N2: TMenuItem;
    Quit1: TMenuItem;
    OpenPictureDialog1: TOpenPictureDialog;
    procedure aLoadSVGExecute(Sender: TObject);
    procedure aPrintSVGExecute(Sender: TObject);
    procedure aQuitExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    {$IFnDEF FPC}
    procedure WMDROPFILES(var Msg: TWMDropFiles); message WM_DROPFILES;
    {$ELSE}
    procedure OnDropFiles(Sender: TObject; const FileNames: array of String);
    {$ENDIF}
  public
    procedure LoadSVG(const aFilename: string);
  end;

var
  Form1: TForm1;

implementation
uses
  {$IFnDEF FPC}
  ShellAPI,
  {$ENDIF}
  UnitPrintPreview;

const
  svg_drop =
  '<?xml version="1.0" standalone="no"?>'
  + '<svg viewBox="0 0 1000 300" xmlns="http://www.w3.org/2000/svg" version="1.1">'
  + '<g font-family="Arial" font-size="45" >'
  + '<text x="500" y="150" fill="black" text-anchor="middle">Drop ''n SVG...</text>'
  + '</g>'
  + '</svg>';

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TForm1.aLoadSVGExecute(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
    LoadSVG(OpenPictureDialog1.FileName);
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

  {$IFnDEF FPC}
  DragAcceptFiles(Handle, True);
  {$ELSE}
  Application.AddOnDropFilesHandler(OnDropFiles);
  {$ENDIF}
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  {$IFDEF FPC}
  Application.RemoveOnDropFilesHandler(OnDropFiles);
  {$ENDIF}
end;

procedure TForm1.LoadSVG(const aFilename: string);
begin
  SVG2Image1.SVG.Clear;
  SVG2Image1.Filename := aFilename;
end;

{$IFnDEF FPC}
procedure TForm1.WMDROPFILES(var Msg: TWMDropFiles);
var
  i, FileCount: integer;
  l: integer;
  FileName: string;
begin
  FileCount := DragQueryFile(Msg.Drop, $FFFFFFFF, nil, 0);

  for i := 0 to FileCount - 1 do
  begin
    l := DragQueryFile(Msg.Drop, i, nil, 0);
    SetLength(Filename, l);
    DragQueryFile(Msg.Drop, i, PChar(FileName), l + 1);

    LoadSVG(FileName);
  end;

  DragFinish(Msg.Drop);
end;
{$ELSE}
procedure TForm1.OnDropFiles(Sender: TObject; const FileNames: array of String);
var
  i, FileCount: integer;
begin
  FileCount := Length(FileNames);

  for i := 0 to FileCount - 1 do
    LoadSVG(FileNames[i]);
end;
{$ENDIF}

end.
