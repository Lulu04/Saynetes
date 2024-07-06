unit u_edit_otheraction;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, ExtCtrls,
  StdCtrls, Spin, ComCtrls, lcl_utils, u_common, frame_velocity,
  frame_trackbar, frame_trackbar_customized;

type

  { TFormOtherAction }

  TFormOtherAction = class(TForm)
    BAdd1: TSpeedButton;
    BAdd2: TSpeedButton;
    BAdd3: TSpeedButton;
    BAdd4: TSpeedButton;
    BHelpStretch: TSpeedButton;
    BHelpStart: TSpeedButton;
    BHelpStop: TSpeedButton;
    BHelpLoop: TSpeedButton;
    FSE1: TFloatSpinEdit;
    Label1: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Label25: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    LBStart: TListBox;
    LBStretch: TListBox;
    LBStop: TListBox;
    Notebook1: TNotebook;
    PageStretchTime: TPage;
    PageLoop: TPage;
    PageStop: TPage;
    PageStart: TPage;
    Panel1: TPanel;
    Panel2: TPanel;
    Shape1: TShape;
    Shape2: TShape;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    procedure BAdd1Click(Sender: TObject);
    procedure BHelpStartClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure TB1Change(Sender: TObject);
  private
    FNoteBookManager: TNoteBookManager;
    Frame_Velocity1: TFrame_Velocity;
    FrameStretchSpeed: TFrameTBSequenceStretchSpeed;
    FCmdDuration: single;
    FCmds: TCmdList;
    FShortReadableString: string;
    procedure UpdateWidgets;
  public
    procedure Fill;
    procedure UpdateStringAfterLanguageChange;

    property Cmds: TCmdList read FCmds;
    property ShortReadableString: string read FShortReadableString;
    property CmdDuration: single read FCmdDuration;
  end;

var
  FormOtherAction: TFormOtherAction;

implementation

uses u_list_sequence, u_utils, u_resource_string, u_edit_sequence, form_help,
  LCLType;

{$R *.lfm}

{ TFormOtherAction }

procedure TFormOtherAction.FormCreate(Sender: TObject);
begin
  Frame_Velocity1 := TFrame_Velocity.Create(Self);
  Frame_Velocity1.Parent := Panel1;
  Frame_Velocity1.Align := alClient;

  FrameStretchSpeed := TFrameTBSequenceStretchSpeed.Create(Self, Panel2);
  FrameStretchSpeed.Init(trHorizontal, False, False, False);
  FrameStretchSpeed.Value := 1.0;
  FrameStretchSpeed.OnChange := @TB1Change;

  FNoteBookManager := TNoteBookManager.Create(Notebook1);
  with FNoteBookManager do
  begin
    SetActivatedColors($0003C4FC, clBlack);
    SetDeactivatedColors($00484848, $00EAEAEA);
    LinkButtonToPage(SpeedButton1, PageStart);
    LinkButtonToPage(SpeedButton2, PageStop);
    LinkButtonToPage(SpeedButton3, PageLoop);
    LinkButtonToPage(SpeedButton4, PageStretchTime);
    ActivePage(PageStart);
  end;
end;

procedure TFormOtherAction.FormDestroy(Sender: TObject);
begin
  FNoteBookManager.Free;
end;

procedure TFormOtherAction.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    ModalResult := mrCancel;
end;

procedure TFormOtherAction.FormShow(Sender: TObject);
begin
  UpdateWidgets;
end;

procedure TFormOtherAction.SpeedButton5Click(Sender: TObject);
begin
  FrameStretchSpeed.Value := 1.0;
  TB1Change(NIL);
end;

procedure TFormOtherAction.TB1Change(Sender: TObject);
var pt: TSequence;
begin
  UpdateWidgets;
  if LBStretch.ItemIndex <> -1 then
  begin
    pt := Sequences.GetSequenceByIndex(LBStretch.ItemIndex);
    pt.TimeStretchFactor.ChangeTo(FrameStretchSpeed.Value, 0.2);
  end;
end;

procedure TFormOtherAction.UpdateWidgets;
begin
  Label11.Caption := StrechTimeToString(FrameStretchSpeed.Value);
end;

procedure TFormOtherAction.BAdd1Click(Sender: TObject);
var pt: TSequence;
  procedure BeepListBox(aLB: TListBox);
  var c: TColor;
  begin
    c := aLB.Color;
    aLB.Color := PercentColor(c, 0.5);
    Application.processMessages;
    Sleep(100);
    aLB.Color := c;
    Application.processMessages;
  end;

begin
  FCmdDuration := 0;

  if Notebook1.PageIndex=Notebook1.IndexOf(PageStart) then
  begin
    if (LBStart.ItemIndex = -1) or (LBStart.ItemIndex = FormSequenceEdition.EditingTopIndex) then begin
      BeepListBox(LBStart);
      exit;
    end;
    pt := Sequences.GetSequenceByIndex(LBStart.ItemIndex);
    FCmds := CmdStartTop(pt.ID);
    FShortReadableString := SStartSequence+' '+pt.Name;
  end;

  if Notebook1.PageIndex=Notebook1.IndexOf(PageStop) then
  begin
    if (LBStop.ItemIndex = -1) or (LBStop.ItemIndex = FormSequenceEdition.EditingTopIndex) then begin
      BeepListBox(LBStop);
      exit;
    end;
    pt := Sequences.GetSequenceByIndex(LBStop.ItemIndex);
    FCmds := CmdStopTop(pt.ID);
    FShortReadableString := SStopSequence+' '+pt.Name;
  end;

  if Notebook1.PageIndex = Notebook1.IndexOf(PageLoop) then
  begin
    FCmds := CmdLoop;
    FShortReadableString := SBackToBegin;
  end;

  if Notebook1.PageIndex = Notebook1.IndexOf(PageStretchTime) then
  begin
    if (LBStretch.ItemIndex = -1) or (LBStretch.ItemIndex = FormSequenceEdition.EditingTopIndex) then begin
      BeepListBox(LBStretch);
      exit;
    end;
    pt := Sequences.GetSequenceByIndex(LBStretch.ItemIndex);
    FCmds := CmdTopStretchTime(pt.ID, FrameStretchSpeed.Value, FSE1.Value, Frame_Velocity1.SelectedCurveID);
    FShortReadableString := SStretchTime+' '+SOn_+' '+pt.Name+' '+SCoef+' '+FormatFloat('0.00', FrameStretchSpeed.Value);
    FCmdDuration := FSE1.Value;
  end;

  ModalResult := mrOk;
end;

procedure TFormOtherAction.BHelpStartClick(Sender: TObject);
begin
  if Sender = BHelpStart then _ShowHelp(HelpSequencerOtherStart, BHelpStart);
  if Sender = BHelpStop then _ShowHelp(HelpSequencerOtherStop, BHelpStart);
  if Sender = BHelpLoop then _ShowHelp(HelpSequencerOtherLoop, BHelpStart);
  if Sender = BHelpStretch then _ShowHelp(HelpSequencerOtherStretch, BHelpStretch);
end;

procedure TFormOtherAction.Fill;
var i: integer;
begin
  Frame_Velocity1.UpdateList;

  LBStart.LockSelectionChange;
  LBStart.Clear;
  for i:=0 to Sequences.Count-1 do
    LBStart.Items.Add(Sequences.GetSequenceByIndex(i).Name);

  LBStart.UnlockSelectionChange;

  LBStop.LockSelectionChange;
  LBStop.Clear;
  for i:=0 to Sequences.Count-1 do
    LBStop.Items.Add(Sequences.GetSequenceByIndex(i).Name);
  LBStop.UnlockSelectionChange;

  LBStretch.LockSelectionChange;
  LBStretch.Clear;
  for i:=0 to Sequences.Count-1 do
    LBStretch.Items.Add(Sequences.GetSequenceByIndex(i).Name);
  LBStretch.UnlockSelectionChange;
end;

procedure TFormOtherAction.UpdateStringAfterLanguageChange;
begin
  BAdd1.Caption := SAdd;
  BAdd2.Caption := SAdd;
  BAdd3.Caption := SAdd;
  BAdd4.Caption := SAdd;
end;

end.

