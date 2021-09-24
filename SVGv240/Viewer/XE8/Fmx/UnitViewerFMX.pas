unit UnitViewerFMX;

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

interface
uses
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Actions,
  System.ImageList,
  System.Messaging,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.ActnList,
  FMX.Controls.Presentation,
  FMX.Layouts,
  FMX.ImgList,
  FMX.Objects,
  FMX.ListBox,
  FMX.Edit,
  FMX.EditBox,
  FMX.SpinBox,
  BVE.SVG2Intf,
  BVE.SVG2ImageList.FMX,
  UnitViewerCommon.FMX;

type
  TfrmSVGViewerFMX = class(TSVGViewerForm)
    Layout1: TLayout;
    Layout2: TLayout;
    Panel1: TPanel;
    ActionList1: TActionList;
    ToolBar1: TToolBar;
    aLoad: TAction;
    aCopy: TAction;
    aPaste: TAction;
    aCopyDirect: TAction;
    aRemove: TAction;
    SVG2ImageList1: TSVG2ImageList;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Rectangle1: TRectangle;
    ToolBar2: TToolBar;
    ListBox1: TListBox;
    ScrollBox: TScrollBox;
    lSize: TLayout;
    lZoom: TLayout;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    aAddOk: TAction;
    aAddCancel: TAction;
    aAdd: TAction;
    SVG2ImageList2: TSVG2ImageList;
    Label1: TLabel;
    OpenDialog1: TOpenDialog;
    Layout3: TLayout;
    TrackBar1: TTrackBar;
    sbRotate: TSpinBox;
    Layout4: TLayout;
    Layout5: TLayout;
    Label2: TLabel;
    cbAutoViewbox: TCheckBox;
    cbAspectRatio: TComboBox;
    cbMeetOrSlice: TComboBox;
    Layout6: TLayout;
    sbOpacity: TSpinBox;
    Label3: TLabel;
    Layout7: TLayout;
    cbClippath: TCheckBox;
    cbFilters: TCheckBox;
    Layout8: TLayout;
    cbMouseEvents: TCheckBox;
    Button9: TButton;
    TimerAndroidLoad: TTimer;
    TimerZoom: TTimer;
    aZoomReset: TAction;
    aZoomDec: TAction;
    aZoomInc: TAction;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    aInfo: TAction;
    aAnimationStart: TAction;
    aAnimationPause: TAction;
    Panel2: TPanel;
    TrackBar2: TTrackBar;
    LabelTime: TLabel;
    bAnimationStart: TButton;
    bAnimationPause: TButton;
    aPrint: TAction;
    Button13: TButton;
    procedure FormCreate(Sender: TObject);
  end;

var
  frmSVGViewerFMX: TfrmSVGViewerFMX;

implementation

{$R *.fmx}

procedure TfrmSVGViewerFMX.FormCreate(Sender: TObject);
begin
  ConnectControls(
    aAddOk,
    aAddCancel,
    aAdd,
    aCopy,
    aCopyDirect,
    aInfo,
    aLoad,
    aPaste,
    aRemove,
    aZoomReset,
    aZoomDec,
    aZoomInc,
    aPrint,
    bAnimationStart,
    bAnimationPause,
    cbAutoViewbox,
    cbClippath,
    cbFilters,
    cbMouseEvents,
    cbAspectRatio,
    cbMeetOrSlice,
    sbRotate,
    sbOpacity,
    Scrollbox,
    Layout2,
    lSize,
    lZoom,
    Listbox1,
    SVG2ImageList1,
    SVG2ImageList2,
    OpenDialog1,
    TrackBar1,
    Label1,
    Trackbar2,
    LabelTime);
end;

end.
