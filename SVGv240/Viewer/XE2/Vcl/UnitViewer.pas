unit UnitViewer;

// ------------------------------------------------------------------------------
//
//                              SVG Control 2.0
//                       Copyright (c) 2015 Bruno Verhue
//
// ------------------------------------------------------------------------------

// [The "BSD licence"]
//
// Copyright (c) 2013 Bruno Verhue
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
// 1. Redistributions of source code must retain the above copyright
//    notice, this list of conditions and the following disclaimer.
// 2. Redistributions in binary form must reproduce the above copyright
//    notice, this list of conditions and the following disclaimer in the
//    documentation and/or other materials provided with the distribution.
// 3. The name of the author may not be used to endorse or promote products
//    derived from this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
// OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
// IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
// INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
// NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
// THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

// -----------------------------------------------------------------------------
//
// Demo application "SVG viewer"
//
// Demonstrates:
//
// - Loading and parsing SVG documents
// - Copying SVG control
// - Finding rendering elements and changing properties

// When loading the "animatied clock", it will have some interactive
// functionality: it will be set and updated to the current time, and you
// can move the clock hands by mouse.
//
// -----------------------------------------------------------------------------

{$Include '..\..\Demo common\DemoAppCompilerSettings.inc'}

interface
uses
  Winapi.Windows,
  Winapi.Messages,
  System.Types,
  System.SysUtils,
  System.Variants,
  System.Classes,
  {$IFDEF Ver240Up}
  System.Actions,
  {$ENDIF}
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ToolWin,
  Vcl.ComCtrls,
  Vcl.ActnList,
  Vcl.PlatformDefaultStyleActnCtrls,
  Vcl.ActnMan,
  Vcl.StdCtrls,
  Vcl.ActnCtrls,
  Vcl.ExtCtrls,
  Vcl.ImgList,
  Vcl.Clipbrd,
  Vcl.ExtDlgs,
  BVE.ViewerCore.Vcl,
  BVE.SVG2ImageList.Vcl;

type
  TForm1 = class(TSVGViewerForm)
    ScrollBox1: TScrollBox;
    ActionManager1: TActionManager;
    aOpen: TAction;
    aEdit: TAction;
    ActionToolBar1: TActionToolBar;
    aCopy: TAction;
    SVG2ImageList1: TSVG2ImageList;
    aNew: TAction;
    aPaste: TAction;
    aCopyDirect: TAction;
    aAbout: TAction;
    aEnableFilters: TAction;
    aRemove: TAction;
    aAutoViewBox: TAction;
    StatusBar1: TStatusBar;
    aExport: TAction;
    OpenPictureDialog1: TOpenPictureDialog;
    SavePictureDialog1: TSavePictureDialog;
    ilDisabled: TSVG2LinkedImageList;
    ilNormal: TSVG2LinkedImageList;
    aAnimationStart: TAction;
    aAnimationPause: TAction;
    aEnableClippaths: TAction;
    aEnableEvents: TAction;
    aEnablePersistentBuffers: TAction;
    aEnableTextToPath: TAction;
    Panel1: TPanel;
    lblTime: TLabel;
    TrackBar1: TTrackBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    Panel2: TPanel;
    Label1: TLabel;
    lblRenderContext: TLabel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    aPrint: TAction;
    procedure FormCreate(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  ConnectControls(
    aOpen,
    aEdit,
    aCopy,
    aNew,
    aPaste,
    aCopyDirect,
    aAbout,
    aRemove,
    aAutoViewBox,
    aExport,
    aEnableFilters,
    aEnableClippaths,
    aEnableEvents,
    aEnablePersistentBuffers,
    aEnableTextToPath,
    aAnimationStart,
    aAnimationPause,
    aPrint,
    OpenPictureDialog1,
    Scrollbox1,
    Statusbar1,
    TrackBar1,
    lblTime,
    lblRenderContext);

end;

end.
