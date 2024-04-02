unit u_edit_otheraction;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, ExtCtrls,
  StdCtrls, Spin, ComCtrls, u_notebook_util, u_common, frame_velocity;

type

  { TFormOtherAction }

  TFormOtherAction = class(TForm)
    BAdd1: TSpeedButton;
    BAdd2: TSpeedButton;
    BAdd3: TSpeedButton;
    BAdd4: TSpeedButton;
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
    Shape1: TShape;
    Shape2: TShape;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    TB1: TTrackBar;
    procedure BAdd1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure TB1Change(Sender: TObject);
  private
    FNoteBookManager: TNoteBookManager;
    Frame_Velocity1: TFrame_Velocity;
    FCmdDuration: single;
    FCmds: TCmdList;
    FShortReadableString: string;
    function GetStretchValue: single;
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

uses u_list_top, u_utils, u_resource_string, u_edit_sequence, LCLType;

{$R *.lfm}

{ TFormOtherAction }

procedure TFormOtherAction.FormCreate(Sender: TObject);
begin
  Frame_Velocity1 := TFrame_Velocity.Create(Self);
  Frame_Velocity1.Parent := Panel1;
  Frame_Velocity1.Align := alClient;

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
  TB1.Position := 100;
end;

procedure TFormOtherAction.TB1Change(Sender: TObject);
var pt: TSequence;
begin
  UpdateWidgets;
  if LBStretch.ItemIndex <> -1 then
  begin
    pt := Sequences.GetTopByIndex(LBStretch.ItemIndex);
    pt.TimeStretchFactor.ChangeTo(GetStretchValue, 0.2);
  end;
end;

function TFormOtherAction.GetStretchValue: single;
begin
  Result := TB1.Position/100;
end;

procedure TFormOtherAction.UpdateWidgets;
var s: string;
begin
  //stretch time
  if GetStretchValue < 1 then
    s := SSlower+' '+FormatFloat('0.00', GetStretchValue)
  else
    if GetStretchValue = 1 then
      s := SNormal
    else
      s := SFaster+' '+FormatFloat('0.00', GetStretchValue);
  Label11.Caption := s;
end;

procedure TFormOtherAction.BAdd1Click(Sender: TObject);
var pt: TSequence;
begin
  FCmdDuration := 0;

  if Notebook1.PageIndex=Notebook1.IndexOf(PageStart) then
  begin
    if LBStart.ItemIndex = -1 then
      exit;
    pt := Sequences.GetTopByIndex(LBStart.ItemIndex);
    FCmds := CmdStartTop(pt.ID);
    FShortReadableString := SStartSequence+' '+pt.Name;
  end;

  if Notebook1.PageIndex=Notebook1.IndexOf(PageStop) then
  begin
    if LBStop.ItemIndex = -1 then
      exit;
    pt := Sequences.GetTopByIndex(LBStop.ItemIndex);
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
    if LBStretch.ItemIndex = -1 then
      exit;
    pt := Sequences.GetTopByIndex(LBStretch.ItemIndex);
    FCmds := CmdTopStretchTime(pt.ID, GetStretchValue, FSE1.Value, Frame_Velocity1.SelectedCurveID);
    FShortReadableString := SStretchTime+' '+SOn_+' '+pt.Name+' '+SCoef+' '+FormatFloat('0.00', GetStretchValue);
    FCmdDuration := FSE1.Value;
  end;

  ModalResult := mrOk;
end;

procedure TFormOtherAction.Fill;
var i: integer;
begin
  Frame_Velocity1.UpdateList;

  LBStart.LockSelectionChange;
  LBStart.Clear;
  for i:=0 to Sequences.Count-1 do
    if i <> FormSequenceEdition.EditingTopIndex then LBStart.Items.Add(Sequences.GetTopByIndex(i).Name);

  LBStart.UnlockSelectionChange;

  LBStop.LockSelectionChange;
  LBStop.Clear;
  for i:=0 to Sequences.Count-1 do
    if i <> FormSequenceEdition.EditingTopIndex then LBStop.Items.Add(Sequences.GetTopByIndex(i).Name);
  LBStop.UnlockSelectionChange;

  LBStretch.LockSelectionChange;
  LBStretch.Clear;
  for i:=0 to Sequences.Count-1 do
    if i <> FormSequenceEdition.EditingTopIndex then LBStretch.Items.Add(Sequences.GetTopByIndex(i).Name);
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

