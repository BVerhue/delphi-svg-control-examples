unit BVE.SVGPrintPreviewFormFMX;

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
///    Print preview form definition
///  </summary>
///  <remarks>
///    The SVG Print Preview need at least version v2.40 update 9 of the SVG library
///  </remarks>

// See here a discussion on multipage in SVG:
//   https://wiki.inkscape.org/wiki/index.php/Multipage

interface
uses
  System.SysUtils,
  System.Variants,
  System.Types,
  System.Classes,
  System.Generics.Collections,
  System.Actions,
  System.Math,
  System.UITypes,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.ActnList,
  FMX.StdCtrls,
  FMX.Edit,
  FMX.ListBox,
  FMX.Printer,
  BVE.SVG2Intf,
  BVE.SVG2Types,
  BVE.SVGPrintPreviewFMX;

type
  TSVGPrintPreviewForm = class(TForm)
  private
    FActionOrientationLandscape: TAction;
    FActionCancel: TAction;
    FActionPrint: TAction;
    FActionPrinterSelect: TAction;
    FActionOrientationPortait: TAction;
    FActionIdenticalMargins: TAction;

    FEditOutputDevice: TEdit;
    FEditPagesHorizontal: TEdit;
    FEditPagesVertical: TEdit;
    FEditMarginLeft: TEdit;
    FEditMarginTop: TEdit;
    FEditMarginRight: TEdit;
    FEditMarginBottom: TEdit;
    FEditGlueEdge: TEdit;

    FComboBoxUnits: TComboBox;
    FComboboxAlign: TComboBox;
    FComboboxMeetOrSlice: TComboBox;

    FCheckBoxAutoViewbox: TCheckBox;

    FPrintDialog: TPrintDialog;
    FPrintPreview: TSVGPrintPreview;

    FTimerUpdate: TTimer;

    FRecursion: Integer;

  protected
    function GetRoot: ISVGRoot;
    procedure SetRoot(const Value: ISVGRoot);

    procedure SetActionCancel(const Value: TAction);
    procedure SetActionIdenticalMargins(const Value: TAction);
    procedure SetActionOrientationLandscape(const Value: TAction);
    procedure SetActionOrientationPortait(const Value: TAction);
    procedure SetActionPrint(const Value: TAction);
    procedure SetActionPrinterSelect(const Value: TAction);

    procedure SetCheckBoxAutoViewbox(const Value: TCheckBox);

    procedure SetComboBoxUnits(const Value: TComboBox);
    procedure SetComboboxAlign(const Value: TComboBox);
    procedure SetComboboxMeetOrSlice(const Value: TComboBox);

    procedure SetEditOutputDevice(const Value: TEdit);
    procedure SetEditPagesHorizontal(const Value: TEdit);
    procedure SetEditPagesVertical(const Value: TEdit);
    procedure SetEditGlueEdge(const Value: TEdit);
    procedure SetEditMarginBottom(const Value: TEdit);
    procedure SetEditMarginLeft(const Value: TEdit);
    procedure SetEditMarginRight(const Value: TEdit);
    procedure SetEditMarginTop(const Value: TEdit);

    //procedure EditNumberKeyPress(Sender: TObject; var Key: Char);
    procedure EditNumberKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
    procedure EditIntegerKeyPress(Sender: TObject; var Key: Char);
    procedure EditIntegerKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);

    procedure ActionIdenticalMarginsExecute(Sender: TObject);
    procedure ActionOrientationLandscapeExecute(Sender: TObject);
    procedure ActionOrientationPortaitExecute(Sender: TObject);
    procedure ActionPrintExecute(Sender: TObject);
    procedure ActionCancelExecute(Sender: TObject);
    procedure ActionPrinterSelectExecute(Sender: TObject);

    procedure MarginChange(Sender: TObject);
    procedure PageSettingsChange(Sender: TObject);

    procedure TimerUpdateTimer(Sender: TObject);

    procedure DoShow; override;

    procedure PreviewSetProperties;

    procedure UpdatePreview(const aImmediately: Boolean = False);
    procedure EnableActions;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property ActionOrientationLandscape: TAction read FActionOrientationLandscape write SetActionOrientationLandscape;
    property ActionOrientationPortait: TAction read FActionOrientationPortait write SetActionOrientationPortait;
    property ActionPrint: TAction read FActionPrint write SetActionPrint;
    property ActionCancel: TAction read FActionCancel write SetActionCancel;
    property ActionPrinterSelect: TAction read FActionPrinterSelect write SetActionPrinterSelect;
    property ActionIdenticalMargins: TAction read FActionIdenticalMargins write SetActionIdenticalMargins;

    property ComboBoxUnits: TComboBox read FComboBoxUnits write SetComboBoxUnits;
    property ComboboxAlign: TComboBox read FComboboxAlign write SetComboboxAlign;
    property ComboboxMeetOrSlice: TComboBox read FComboboxMeetOrSlice write SetComboboxMeetOrSlice;

    property CheckBoxAutoViewbox: TCheckBox read FCheckBoxAutoViewbox write SetCheckBoxAutoViewbox;

    property EditOutputDevice: TEdit read FEditOutputDevice write SetEditOutputDevice;
    property EditPagesHorizontal: TEdit read FEditPagesHorizontal write SetEditPagesHorizontal;
    property EditPagesVertical: TEdit read FEditPagesVertical write SetEditPagesVertical;
    property EditMarginLeft: TEdit read FEditMarginLeft write SetEditMarginLeft;
    property EditMarginTop: TEdit read FEditMarginTop write SetEditMarginTop;
    property EditMarginRight: TEdit read FEditMarginRight write SetEditMarginRight;
    property EditMarginBottom: TEdit read FEditMarginBottom write SetEditMarginBottom;
    property EditGlueEdge: TEdit read FEditGlueEdge write SetEditGlueEdge;

    property Root: ISVGRoot read GetRoot write SetRoot;
  end;

implementation

procedure TSVGPrintPreviewForm.ActionCancelExecute(Sender: TObject);
begin
  ModalResult := mrOk;
  Close;
end;

procedure TSVGPrintPreviewForm.ActionIdenticalMarginsExecute(Sender: TObject);
begin
  //
end;

procedure TSVGPrintPreviewForm.ActionOrientationLandscapeExecute(
  Sender: TObject);
begin
  if FRecursion > 0 then
    Exit;

  Printer.Orientation := TPrinterOrientation.poLandscape;

  FPrintPreview.Repaint;
end;

procedure TSVGPrintPreviewForm.ActionOrientationPortaitExecute(Sender: TObject);
begin
  if FRecursion > 0 then
    Exit;

  Printer.Orientation := TPrinterOrientation.poPortrait;

  FPrintPreview.Repaint;
end;

procedure TSVGPrintPreviewForm.ActionPrinterSelectExecute(Sender: TObject);
begin
  if FPrintDialog.Execute then
  begin
    FPrintPreview.Repaint;

    EnableActions;
  end;
end;

procedure TSVGPrintPreviewForm.ActionPrintExecute(Sender: TObject);
begin
  FPrintPreview.Print('SVG Editor');
end;

constructor TSVGPrintPreviewForm.Create(AOwner: TComponent);
begin
  inherited;

  FRecursion := 0;

  FPrintDialog := TPrintDialog.Create(Self);

  FPrintPreview := TSVGPrintPreview.Create(Self);
  FPrintPreview.Parent := Self;
  FPrintPreview.Align := TAlignLayout.Client;

  FTimerUpdate := TTimer.Create(Self);
  FTimerUpdate.Enabled := False;
  FTimerUpdate.Interval := 300;
  FTimerUpdate.OnTimer := TimerUpdateTimer;
end;

destructor TSVGPrintPreviewForm.Destroy;
begin
  inherited;
end;

procedure TSVGPrintPreviewForm.DoShow;
begin
  inherited;

  EnableActions;
end;

procedure TSVGPrintPreviewForm.EditIntegerKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: WideChar; Shift: TShiftState);
var
  ValueInt: Integer;
begin
  if not(Sender is TEdit) then
    Exit;

  if not TryStrToInt((Sender as TEdit).Text, ValueInt) then
    Exit;

  case Key of
    VkUP:
      Inc(ValueInt);
    VkDOWN:
      if ValueInt > 0 then
        Dec(ValueInt);
    else
      Exit;
  end;

  (Sender as TEdit).Text := IntToStr(ValueInt);
end;

procedure TSVGPrintPreviewForm.EditIntegerKeyPress(Sender: TObject;
  var Key: Char);
begin
  case Key of
    '0'..'9':;
    #8, #9:;
    else
      Key := #0;
  end;
end;

procedure TSVGPrintPreviewForm.EditNumberKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: WideChar; Shift: TShiftState);
var
  ValueFloat: TSVGFloat;
begin
  if not(Sender is TEdit) then
    Exit;

  if not TryStrToFloat((Sender as TEdit).Text, ValueFloat, USFormatSettings) then
    Exit;

  case Key of
    VkUP:
      ValueFloat := ValueFloat + 1;
    VkDOWN:
      if ValueFloat > 0 then
        ValueFloat := ValueFloat - 1;
    else
      Exit;
  end;

  (Sender as TEdit).Text := FloatToStr(ValueFloat, USFormatSettings);
end;

{procedure TSVGPrintPreviewForm.EditNumberKeyPress(Sender: TObject;
  var Key: Char);
begin
  case Key of
    '0'..'9':;
    #8, #9:;
    '.':
      begin
        if Pos('.', TEdit(Sender).Text) > 0 then
          Key := #0;
      end
    else
      Key := #0;
  end;
end;}

procedure TSVGPrintPreviewForm.EnableActions;
begin
  Inc(FRecursion);
  try
    FEditOutputDevice.Text := Printer.ActivePrinter.Device;

    if Printer.Orientation = TPrinterOrientation.poPortrait then
      ActionOrientationPortait.Checked := True
    else
      ActionOrientationLandscape.Checked := True;
  finally
    Dec(FRecursion);
  end;
end;

function TSVGPrintPreviewForm.GetRoot: ISVGRoot;
begin
  Result := FPrintPreview.Root;
end;

procedure TSVGPrintPreviewForm.MarginChange(Sender: TObject);
var
  Edit: TEdit;
begin
  if FRecursion > 0 then
    Exit;

  if ActionIdenticalMargins.Checked and (Sender is TEdit) then
  begin
    Edit := Sender as TEdit;

    Inc(FRecursion);
    try
      if EditMarginLeft <> Edit then
        EditMarginLeft.Text := Edit.Text;

      if EditMarginTop <> Edit then
        EditMarginTop.Text := Edit.Text;

      if EditMarginRight <> Edit then
        EditMarginRight.Text := Edit.Text;

      if EditMarginBottom <> Edit then
        EditMarginBottom.Text := Edit.Text;

    finally
      Dec(FRecursion);
    end;
  end;

  UpdatePreview;
end;

procedure TSVGPrintPreviewForm.PageSettingsChange(Sender: TObject);
begin
  UpdatePreview;
end;

procedure TSVGPrintPreviewForm.PreviewSetProperties;
var
  ValueInt: Integer;
  ValueFloat: TSVGFloat;
begin
  if not TryStrToInt(EditPagesHorizontal.Text, ValueInt) then
    ValueInt := 1;

  FPrintPreview.PagesHorizontal := ValueInt;;

  if not TryStrToInt(EditPagesVertical.Text, ValueInt) then
    ValueInt := 1;

  FPrintPreview.PagesVertical := ValueInt;

  if not TryStrToFloat(EditMarginLeft.Text, ValueFloat, USFormatSettings) then
    ValueFloat := 0;

  FPrintPreview.MarginLeft := ValueFloat;

  if not TryStrToFloat(EditMarginRight.Text, ValueFloat, USFormatSettings) then
    ValueFloat := 0;

  FPrintPreview.MarginRight := ValueFloat;

  if not TryStrToFloat(EditMarginTop.Text, ValueFloat, USFormatSettings) then
    ValueFloat := 0;

  FPrintPreview.MarginTop := ValueFloat;

  if not TryStrToFloat(EditMarginBottom.Text, ValueFloat, USFormatSettings) then
    ValueFloat := 0;

  FPrintPreview.MarginBottom := ValueFloat;

  if not TryStrToFloat(EditGlueEdge.Text, ValueFloat, USFormatSettings) then
    ValueFloat := 0;

  FPrintPreview.GlueEdge := ValueFloat;

  FPrintPreview.Units := TSVGPrintUnits(ComboboxUnits.ItemIndex);

  FPrintPreview.AutoViewbox := CheckboxAutoViewbox.IsChecked;
  FPrintPreview.AspectRatioAlign := TSVGAspectRatioAlign(ComboBoxAlign.ItemIndex);
  FPrintPreview.AspectRatioMeetOrSlice := TSVGAspectRatioMeetOrSlice(ComboBoxMeetOrSlice.ItemIndex);
end;

procedure TSVGPrintPreviewForm.SetActionCancel(const Value: TAction);
begin
  FActionCancel := Value;
  if assigned(FActionCancel) then
    FActionCancel.OnExecute := ActionCancelExecute;
end;

procedure TSVGPrintPreviewForm.SetActionIdenticalMargins(const Value: TAction);
begin
  FActionIdenticalMargins := Value;
  if assigned(FActionIdenticalMargins) then
  begin
    FActionIdenticalMargins.OnExecute := ActionIdenticalMarginsExecute;
    FActionIdenticalMargins.AutoCheck := True;
  end;
end;

procedure TSVGPrintPreviewForm.SetActionOrientationLandscape(
  const Value: TAction);
begin
  FActionOrientationLandscape := Value;
  if assigned(FActionOrientationLandscape) then
    FActionOrientationLandscape.OnExecute := ActionOrientationLandscapeExecute;
end;

procedure TSVGPrintPreviewForm.SetActionOrientationPortait(
  const Value: TAction);
begin
  FActionOrientationPortait := Value;
  if assigned(FActionOrientationPortait) then
    FActionOrientationPortait.OnExecute := ActionOrientationPortaitExecute;
end;

procedure TSVGPrintPreviewForm.SetActionPrint(const Value: TAction);
begin
  FActionPrint := Value;
  if assigned(FActionPrint) then
    FActionPrint.OnExecute := ActionPrintExecute;
end;

procedure TSVGPrintPreviewForm.SetActionPrinterSelect(const Value: TAction);
begin
  FActionPrinterSelect := Value;
  if assigned(FActionPrinterSelect) then
    FActionPrinterSelect.OnExecute := ActionPrinterSelectExecute;
end;

procedure TSVGPrintPreviewForm.SetCheckBoxAutoViewbox(const Value: TCheckBox);
begin
  FCheckBoxAutoViewbox := Value;
  if assigned(FCheckboxAutoViewBox) then
    FCheckboxAutoViewbox.OnClick := PageSettingsChange;
end;

procedure TSVGPrintPreviewForm.SetComboboxAlign(const Value: TComboBox);
begin
  FComboboxAlign := Value;
  if assigned(FComboboxAlign) then
  begin
    FComboBoxAlign.Items.Clear;
    FComboBoxAlign.Items.Add('None');
    FComboBoxAlign.Items.Add('X-Min Y-Min');
    FComboBoxAlign.Items.Add('X-Mid Y-Min');
    FComboBoxAlign.Items.Add('X-Max Y-Min');
    FComboBoxAlign.Items.Add('X-Min Y-Mid');
    FComboBoxAlign.Items.Add('X-Mid Y-Mid');
    FComboBoxAlign.Items.Add('X-Max Y-Mid');
    FComboBoxAlign.Items.Add('X-Min Y-Max');
    FComboBoxAlign.Items.Add('X-Mid Y-Max');
    FComboBoxAlign.Items.Add('X-Max Y-Max');
    FComboBoxAlign.ItemIndex := Ord(TSVGAspectRatioAlign.arXMidYMid);

    FComboBoxAlign.OnChange := PageSettingsChange;
  end;
end;

procedure TSVGPrintPreviewForm.SetComboboxMeetOrSlice(const Value: TComboBox);
begin
  FComboboxMeetOrSlice := Value;
  if assigned(FComboboxMeetOrSlice) then
  begin
    FComboboxMeetOrSlice.Items.Clear;

    FComboboxMeetOrSlice.Items.Add('Meet');
    FComboboxMeetOrSlice.Items.Add('Slice');
    FComboboxMeetOrSlice.ItemIndex := Ord(TSVGAspectRatioMeetOrSlice.arMeet);

    FComboboxMeetOrSlice.OnChange := PageSettingsChange;
  end;
end;

procedure TSVGPrintPreviewForm.SetComboBoxUnits(const Value: TComboBox);
begin
  FComboBoxUnits := Value;
  if assigned(FComboBoxUnits) then
  begin
    FComboBoxUnits.Items.Clear;
    FComboBoxUnits.Items.Add('mm');
    FComboBoxUnits.Items.Add('cm');
    FComboBoxUnits.Items.Add('inch');
    FComboBoxUnits.Items.Add('px');
    FComboBoxUnits.ItemIndex := Ord(TSVGPrintUnits.puMm);

    FComboBoxUnits.OnChange := PageSettingsChange;
  end;
end;

procedure TSVGPrintPreviewForm.SetEditGlueEdge(const Value: TEdit);
begin
  FEditGlueEdge := Value;
  if assigned(FEditGlueEdge) then
  begin
    //FEditGlueEdge.OnKeyPress := EditNumberKeyPress;
    FEditGlueEdge.OnKeyDown := EditNumberKeyDown;
    FEditGlueEdge.OnChange := PageSettingsChange;
  end;
end;

procedure TSVGPrintPreviewForm.SetEditMarginBottom(const Value: TEdit);
begin
  FEditMarginBottom := Value;
  if assigned(FEditMarginBottom) then
  begin
    //FEditMarginBottom.OnKeyPress := EditNumberKeyPress;
    FEditMarginBottom.OnKeyDown := EditNumberKeyDown;
    FEditMarginBottom.OnChange := MarginChange;
  end;
end;

procedure TSVGPrintPreviewForm.SetEditMarginLeft(const Value: TEdit);
begin
  FEditMarginLeft := Value;
  if assigned(FEditMarginLeft) then
  begin
    //FEditMarginLeft.OnKeyPress := EditNumberKeyPress;
    FEditMarginLeft.OnKeyDown := EditNumberKeyDown;
    FEditMarginLeft.OnChange := MarginChange;
  end;
end;

procedure TSVGPrintPreviewForm.SetEditMarginRight(const Value: TEdit);
begin
  FEditMarginRight := Value;
  if assigned(FEditMarginRight) then
  begin
    //FEditMarginRight.OnKeyPress := EditNumberKeyPress;
    FEditMarginRight.OnKeyDown := EditNumberKeyDown;
    FEditMarginRight.OnChange := MarginChange;
  end;
end;

procedure TSVGPrintPreviewForm.SetEditMarginTop(const Value: TEdit);
begin
  FEditMarginTop := Value;
  if assigned(FEditMarginTop) then
  begin
    //FEditMarginTop.OnKeyPress := EditNumberKeyPress;
    FEditMarginTop.OnKeyDown := EditNumberKeyDown;
    FEditMarginTop.OnChange := MarginChange;
  end;
end;

procedure TSVGPrintPreviewForm.SetEditOutputDevice(const Value: TEdit);
begin
  FEditOutputDevice := Value;
  if assigned(FEditOutputDevice) then
  begin
    FEditOutputDevice.ReadOnly := True;
  end;
end;

procedure TSVGPrintPreviewForm.SetEditPagesHorizontal(const Value: TEdit);
begin
  FEditPagesHorizontal := Value;
  if assigned(FEditPagesHorizontal) then
  begin
    //FEditPagesHorizontal.OnKeyPress := EditIntegerKeyPress;
    FEditPagesHorizontal.OnKeyDown := EditIntegerKeyDown;
    FEditPagesHorizontal.OnChange := PageSettingsChange;
  end;
end;

procedure TSVGPrintPreviewForm.SetEditPagesVertical(const Value: TEdit);
begin
  FEditPagesVertical := Value;
  if assigned(FEditPagesVertical) then
  begin
    //FEditPagesVertical.OnKeyPress := EditIntegerKeyPress;
    FEditPagesVertical.OnKeyDown := EditIntegerKeyDown;
    FEditPagesVertical.OnChange := PageSettingsChange;
  end;
end;

procedure TSVGPrintPreviewForm.SetRoot(const Value: ISVGRoot);
begin
  FPrintPreview.Root := Value;
end;

procedure TSVGPrintPreviewForm.TimerUpdateTimer(Sender: TObject);
begin
  FTimerUpdate.Enabled := False;

  PreviewSetProperties;
end;

procedure TSVGPrintPreviewForm.UpdatePreview(const aImmediately: Boolean);
begin
  if aImmediately then
  begin
    PreviewSetProperties;
  end else begin
    FTimerUpdate.Enabled := False;
    FTimerUpdate.Enabled := True;
  end;
end;


end.
