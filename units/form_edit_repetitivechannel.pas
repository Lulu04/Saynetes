unit form_edit_repetitivechannel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, Spin,
  u_notebook_util, u_common, u_list_dmxuniverse, frame_cb_channeltype,
  frame_viewfixturechannels, u_presetmanager;

type

  TOnAddRepetitiveChannel = procedure(const aChannelToAdd: TFixLibAvailableChannel) of object;

  { TFormEditRepetitiveChannel }

  TFormEditRepetitiveChannel = class(TForm)
    BClear: TSpeedButton;
    BOK: TSpeedButton;
    BPreset: TSpeedButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    RB1: TRadioButton;
    RB2: TRadioButton;
    RB6: TRadioButton;
    RB7: TRadioButton;
    RB8: TRadioButton;
    SE1: TSpinEdit;
    SE2: TSpinEdit;
    BAdd: TSpeedButton;
    procedure BAddClick(Sender: TObject);
    procedure BClearClick(Sender: TObject);
    procedure BOKClick(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    CheckedLabelManager: TCheckedLabelManager;
    FrameCBChannelType: TFrameCBChannelType;
    FrameViewDMXFixtureChannels: TFrameViewDMXFixtureChannels;
    FChannels: TFixLibAvailableChannels;
    FOnAddChannel: TOnAddRepetitiveChannel;
    FPreviewing: boolean;
    procedure ProcessUserChangeChannelNameEvent(Sender: TObject; aChanIndex: integer; const aNewName: string);
    function GetEndIndex: integer;
    function GetStartIndex: integer;
  private
    FPresetManager: TPresetManager;
    function PatternToPreset: string;
    procedure PresetToPattern(const A: TStringArray);
  public
    function GetChannelName(const aChannelName: string; aIndex: integer): string;

    property StartIndex: integer read GetStartIndex;
    property EndIndex: integer read GetEndIndex;
    property OnAddChannel: TOnAddRepetitiveChannel read FOnAddChannel write FOnAddChannel;
  end;


implementation
uses LCLType, u_resource_string, u_project_manager;

{$R *.lfm}

{ TFormEditRepetitiveChannel }

procedure TFormEditRepetitiveChannel.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then ModalResult := mrCancel;
end;

procedure TFormEditRepetitiveChannel.ProcessUserChangeChannelNameEvent(
  Sender: TObject; aChanIndex: integer; const aNewName: string);
begin
  FChannels[aChanIndex].NameID := aNewName;
  CheckBox1Change(NIL);
end;

function TFormEditRepetitiveChannel.GetEndIndex: integer;
begin
  Result := SE2.Value;
end;

function TFormEditRepetitiveChannel.GetStartIndex: integer;
begin
  Result := SE1.Value;
end;

function TFormEditRepetitiveChannel.PatternToPreset: string;
var i: integer;
begin
  Result := '';
  for i:=0 To High(FChannels) do begin
    if Result <> '' then Result := Result + '|';
    Result := Result + FChannels[i].NameID + '~' +
                       Ord(FChannels[i].ChanType).ToString + '~' +
                       FChannels[i].Ranges[0].Text;
  end;
end;

procedure TFormEditRepetitiveChannel.PresetToPattern(const A: TStringArray);
var i: integer;
  B: TStringArray;
begin
  FChannels := NIL;
  FrameViewDMXFixtureChannels.Clear;

  try
    SetLength(FChannels, Length(A));
    for i:=0 to High(A) do begin
      B := A[i].Split(['~']);
      FChannels[i].InitDefault;
      FChannels[i].NameID := B[0];
      FChannels[i].ChanType := TChannelType(B[1].ToInteger);
      FChannels[i].InitDefaultSingleRange;
      FChannels[i].Ranges[0].Text := B[2];

      FrameViewDMXFixtureChannels.AddChannel(@FChannels[i]);
      CheckBox1Change(NIL);
  end;
  except
    FChannels := NIL;
    FrameViewDMXFixtureChannels.Clear;
  end;
end;

function TFormEditRepetitiveChannel.GetChannelName(const aChannelName: string; aIndex: integer): string;
var prefix, suffix: string;
begin
  prefix := '';

  if CheckBox1.Checked then prefix := prefix + Edit1.Text;
  if RB1.Checked then prefix := prefix + aIndex.ToString + ' ';

  suffix := '';
  if RB2.Checked then suffix := suffix + ' ' + aIndex.ToString;
  if CheckBox2.Checked then suffix := suffix + Edit2.Text;

  if RB7.Checked then Result := prefix + aChannelName + suffix + aIndex.ToString
    else if RB8.Checked then Result := aIndex.ToString + prefix + aChannelName + suffix
      else Result := prefix + aChannelName + suffix;
end;

procedure TFormEditRepetitiveChannel.FormCreate(Sender: TObject);
begin
  CheckedLabelManager := TCheckedLabelManager.Create;
  CheckedLabelManager.CaptureLabelClick(Label4);
  CheckedLabelManager.CaptureLabelClick(Label7);
  CheckedLabelManager.CaptureLabelClick(Label8);
  CheckedLabelManager.CaptureLabelClick(Label14);
  CheckedLabelManager.CaptureLabelClick(Label15);
  CheckedLabelManager.CaptureLabelClick(Label13);

  FrameCBChannelType := TFrameCBChannelType.Create(Self);
  FrameCBChannelType.Parent := Panel4;
  FrameCBChannelType.Align := alClient;
  FrameCBChannelType.FillForPresetChannel;

  FrameViewDMXFixtureChannels := TFrameViewDMXFixtureChannels.Create(Self);
  FrameViewDMXFixtureChannels.Parent := Panel5;
  FrameViewDMXFixtureChannels.Align := alClient;
  FrameViewDMXFixtureChannels.EditionEnabled := True;
  FrameViewDMXFixtureChannels.OnUserChangeChannelName := @ProcessUserChangeChannelNameEvent;

  FPresetManager := TPresetManager.Create(Self);
  FPresetManager.Init1(SRepetitiveChannelPresets, BPreset,
                  ConcatPaths([Project.AppPresetsFolder, 'RepetitiveChannel'+PRESET_FILE_EXTENSION]));
  FPresetManager.Init2(@PresetToPattern, @PatternToPreset);

  // manual translation
  BOK.Caption := SOk;
  Label1.Caption := sAddRepetitiveChannels;
  BPreset.Caption := SPreset_;
end;

procedure TFormEditRepetitiveChannel.CheckBox1Change(Sender: TObject);
var i, j: integer;
  //s, s1: string;
begin
  if FPreviewing then exit;
  FPreviewing := True;

  if (SE1.Value > SE2.Value) or (SE2.Value < SE1.Value) then SE2.Value := SE1.Value + 1;

  Memo1.Clear;
  for i:=SE1.Value to SE2.Value do begin
    for j:=0 to High(FChannels) do begin
      Memo1.Lines.Add(GetChannelName(FChannels[j].NameID, i));
    end;
{    s := '';

    if CheckBox1.Checked then s := s + TrimLeft(Edit1.Text);
    if RB1.Checked then s := s + i.ToString + ' ';

    s1 := '';
    if RB2.Checked then s1 := s1 + ' ' + i.ToString;
    if CheckBox2.Checked then s1 := s1 + TrimRight(Edit2.Text);

    if RB3.Checked or RB4.Checked or RB5.Checked then begin
      Memo1.Lines.Add(GetChannelName('Red', i));
      Memo1.Lines.Add(GetChannelName('Green', i));
      Memo1.Lines.Add(GetChannelName('Blue', i));
    end;
    if RB4.Checked or RB5.Checked then Memo1.Lines.Add(GetChannelName('White', i));
    if RB5.Checked then begin
      Memo1.Lines.Add(GetChannelName('Amber', i));
      Memo1.Lines.Add(GetChannelName('UV', i));
    end;  }
  end;

  FPreviewing := False;
end;

procedure TFormEditRepetitiveChannel.BOKClick(Sender: TObject);
var i, j: integer;
  chan: TFixLibAvailableChannel;
begin
  if Length(FChannels) = 0 then exit;

  for i:=SE1.Value to SE2.Value do begin
    for j:=0 to High(FChannels) do begin
      chan := FChannels[j];
      chan.NameID := GetChannelName(chan.NameID, i);
      FOnAddChannel(chan);
    end;
  end;

  ModalResult := mrOk;
end;

procedure TFormEditRepetitiveChannel.BAddClick(Sender: TObject);
var chanType: TChannelType;
  readeable, tex: string;
  i: integer;
begin
  if FrameCBChannelType.ItemIndex = -1 then exit;
  FrameCBChannelType.GetData(chanType, readeable, tex);

  i := Length(FChannels);
  SetLength(FChannels, i+1);
  FChannels[i].InitDefault;
  FChannels[i].ChanType := chanType;
  FChannels[i].NameID := readeable;
  FChannels[i].InitDefaultSingleRange;
  FChannels[i].Ranges[0].Text := tex;

  FrameViewDMXFixtureChannels.AddChannel(@FChannels[i]);

  CheckBox1Change(NIL);
end;

procedure TFormEditRepetitiveChannel.BClearClick(Sender: TObject);
begin
  FChannels := NIL;
  FrameViewDMXFixtureChannels.Clear;
  CheckBox1Change(NIL);
end;

end.

