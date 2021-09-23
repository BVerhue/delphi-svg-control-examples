unit Unit1;

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

{$mode objfpc}
{$H+}

interface
uses
  Classes,
  SysUtils,
  FileUtil,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  ActnList,
  ComCtrls,
  ExtDlgs,
  BVE.SVG2Types,
  BVE.SVG2FilterUtility,
  BVE.SVG2Graphic.FPC,
  BVE.SVG2Control.FPC,
  BVE.SVG2ImageList.FPC,
  BVE.SVG2Image.FPC,
  BVE.ViewerCore.VCL;

type

  { TForm1 }

  TForm1 = class(TSVGViewerForm)
    aAnimationPause: TAction;
    aAnimationStart: TAction;
    aCopy: TAction;
    aCopyDirect: TAction;
    aAbout: TAction;
    aAutoViewbox: TAction;
    aPrint: TAction;
    aEnableClippaths: TAction;
    aEnableEvents: TAction;
    aEnablePersistentBuffers: TAction;
    aEnableTextToPath: TAction;
    aExport: TAction;
    aRemove: TAction;
    aEnableFilters: TAction;
    aPaste: TAction;
    aNew: TAction;
    aEdit: TAction;
    aOpen: TAction;
    ActionList1: TActionList;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    Label1: TLabel;
    lblRenderContext: TLabel;
    lblTime: TLabel;
    OpenPictureDialog1: TOpenPictureDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    ScrollBox1: TScrollBox;
    StatusBar1: TStatusBar;
    SVG2ImageList1: TSVG2ImageList;
    ilNormal: TSVG2LinkedImageList;
    ilDisabled: TSVG2LinkedImageList;
    ilHot: TSVG2LinkedImageList;
    ToolBar1: TToolBar;
    ToolBar2: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    TrackBar1: TTrackBar;
    procedure FormCreate(Sender: TObject);
  public

  end;

var
  Form1: TForm1;

implementation
uses
  LCLIntf;

{$R *.lfm}

{ TForm1 }

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

