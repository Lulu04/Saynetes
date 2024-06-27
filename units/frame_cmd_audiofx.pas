unit frame_cmd_audiofx;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Buttons, ComCtrls,
  LCLTranslator,
  ALSound, u_audio_manager, frame_viewaudiolist, u_common, u_presetmanager,
  u_notebook_util, frame_led, frame_buttononoff, frame_trackbar,
  frame_trackbar_customized;

type


  { TFrameCmdAudioFX }

  TFrameCmdAudioFX = class(TFrame)
    BListen: TSpeedButton;
    BStopAll: TSpeedButton;
    BAddCmd: TBitBtn;
    BPreset: TSpeedButton;
    CheckBox1: TCheckBox;
    ComboBox10: TComboBox;
    ComboBox5: TComboBox;
    ComboBox6: TComboBox;
    ComboBox7: TComboBox;
    ComboBox8: TComboBox;
    ComboBox9: TComboBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label3: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label4: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    Label44: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    Label49: TLabel;
    Label5: TLabel;
    Label50: TLabel;
    Label51: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Notebook1: TNotebook;
    PageMicro: TPage;
    PageFile: TPage;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    Panel14: TPanel;
    Panel15: TPanel;
    Panel16: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    Shape10: TShape;
    Shape11: TShape;
    Shape12: TShape;
    Shape13: TShape;
    Shape14: TShape;
    Shape15: TShape;
    Shape16: TShape;
    Shape17: TShape;
    Shape18: TShape;
    Shape19: TShape;
    Shape2: TShape;
    Shape20: TShape;
    Shape21: TShape;
    Shape22: TShape;
    Shape23: TShape;
    Shape4: TShape;
    Shape5: TShape;
    Shape7: TShape;
    Shape8: TShape;
    Shape9: TShape;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    Timer1: TTimer;
    procedure BListenClick(Sender: TObject);
    procedure BStopAllClick(Sender: TObject);
    procedure BAddCmdClick(Sender: TObject);
    procedure ComboBox5Select(Sender: TObject);
    procedure ComboBox6Select(Sender: TObject);
    procedure Panel13Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    FrameViewAudioList1: TFrameViewAudioList;
    FSelectedSounds: ArrayOfSound;
    FFlag_LockPresetSelectionChange: boolean;
    FPresetIndex: array[0..2] of integer; // to save the last preset selected
    FrameLed: array[0..2] of TFrameLed;
    FrameButton: Array[0..2] of TFrameButtonOnOff;

    procedure FillPresetList(aCB: TComboBox; aEffectIndex: integer);

    procedure ReconstructEffectChain;
    procedure CreateEffect(Index: integer);
private
    FEffectsState: array[0..2] of boolean;
    procedure UpdatePanelEffects;
  private
    function GetEffectType(Index: integer): TALSEffectType;
    function GetEffectCount: integer;
    function AutoPlay: boolean;
    procedure ProcessFileSelectionChange(Sender: TObject);
    procedure GenerateCmdForApply;
    procedure GenerateCmdForRemove;
    procedure DoOnAddCmd;

    procedure StartPlayback(aFromBegining: boolean);
    function TargetIsFile: boolean;
  private
    FAudioFXPresetManager: TPresetManager;
    function AudioFXToPreset: string;
    procedure PresetToAudioFX(const A: TStringArray);
  private
    FCmdDuration: single;
    FCmds: TCmdList;
    FOnAddCmd: TNotifyEvent;
    FShortReadableString: string;
  private
    FToogleSpeedButtonManager: TToggleSpeedButtonManager;
    FNoteBookManager: TNoteBookManager;
    FrameTBDryWet: TFrameTBAudioDryWet;
  public
    procedure ProcessSourceChangeEvent(Sender: TObject);
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init;

    property OnAddCmd: TNotifyEvent read FOnAddCmd write FOnAddCmd;
    property Cmds: TCmdList read FCmds;
    property ShortReadableString: string read FShortReadableString;
    property CmdDuration: single read FCmdDuration;
  end;

implementation
uses u_logfile, u_resource_string, u_utils, u_helper, Graphics,
  u_apputils, Dialogs;

{$R *.lfm}

{ TFrameCmdAudioFX }

procedure TFrameCmdAudioFX.BListenClick(Sender: TObject);
begin
  if TargetIsFile and (FrameViewAudioList1.SelectedCount = 0) then
    exit;

  ReconstructEffectChain;
  StartPlayback(True);
end;

procedure TFrameCmdAudioFX.BStopAllClick(Sender: TObject);
begin
  SoundManager.StopAllSound(True);
  SoundManager.DeleteAllEffects(True);
end;

procedure TFrameCmdAudioFX.BAddCmdClick(Sender: TObject);
begin
  if (FrameViewAudioList1.SelectedCount = 0) and TargetIsFile then
    exit;

  if (GetEffectCount = 0) and
     FToogleSpeedButtonManager.Checked[SpeedButton6] then
    exit;

  SoundManager.StopAllSound(True);
  SoundManager.DeleteAllEffects(True);

  FSelectedSounds := FrameViewAudioList1.GetSelected;

  if FToogleSpeedButtonManager.Checked[SpeedButton6] then
    GenerateCmdForApply
  else GenerateCmdForRemove;
end;

procedure TFrameCmdAudioFX.ComboBox5Select(Sender: TObject);
var
  cb, presetCB: TComboBox;
  oldLock: boolean;
begin
  // User have selected an effect.
  cb := Sender as TComboBox;

  // Fill the presets list according to the selected effect.
  oldLock := FFlag_LockPresetSelectionChange;
  FFlag_LockPresetSelectionChange := True;

  // Retrieve the corresponding preset's combobox.
  case cb.Tag of
    0: presetCB := ComboBox6;
    1: presetCB := ComboBox8;
    2: presetCB := ComboBox10;
  end;
  // And fill it with the effect's preset.
  FillPresetList( presetCB, cb.ItemIndex );
   // Select the previous preset used by the user for this effect.
  presetCB.ItemIndex := FPresetIndex[cb.Tag];

  FFlag_LockPresetSelectionChange := oldLock;

  ReconstructEffectChain;
end;

procedure TFrameCmdAudioFX.ComboBox6Select(Sender: TObject);
var
  cb: TComboBox;
begin
  cb := Sender as TComboBox;

  // Save the preset index selected by the user
  FPresetIndex[cb.Tag] := cb.ItemIndex;

  ReconstructEffectChain;
end;

procedure TFrameCmdAudioFX.Panel13Click(Sender: TObject);
var
  pa: TPanel;
begin
  pa := Sender as TPanel;
  FEffectsState[pa.Tag] := not FEffectsState[pa.Tag];
  UpdatePanelEffects;

  ReconstructEffectChain;
end;

procedure TFrameCmdAudioFX.SpeedButton6Click(Sender: TObject);
begin
  Panel4.Enabled := not FToogleSpeedButtonManager.Checked[SpeedButton7];
  ReconstructEffectChain;
end;

procedure TFrameCmdAudioFX.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  if Timer1.Tag = 0 then
    Timer1.Tag := 1
  else
    Timer1.Tag := 0;
  FrameLed[0].UpdateLed(Timer1.Tag = 1);
  FrameLed[1].UpdateLed(Timer1.Tag = 1);
  FrameLed[2].UpdateLed(Timer1.Tag = 1);

  Timer1.Enabled := True;
end;

procedure TFrameCmdAudioFX.TrackBar1Change(Sender: TObject);
begin
  if FFlag_LockPresetSelectionChange then
    exit;

  if TargetIsFile then
    FrameViewAudioList1.SetDryWetOnSelected(FrameTBDryWet.Value)
  else
    SoundManager.SetDryWetOn(CAPTURE_IDAUDIO, FrameTBDryWet.Value);
end;

procedure TFrameCmdAudioFX.FillPresetList(aCB: TComboBox; aEffectIndex: integer);
begin
  aCB.Clear;
  case aEffectIndex of
    0: aCB.Items.AddStrings( SoundManager.AutoWahPresetList );
    1: aCB.Items.AddStrings( SoundManager.ChorusPresetList );
    2: aCB.Items.AddStrings( SoundManager.FlangerPresetList );
    3: aCB.Items.AddStrings( SoundManager.CompressorPresetList );
    4: aCB.Items.AddStrings( SoundManager.DistortionPresetList );
    5: aCB.Items.AddStrings( SoundManager.EchoPresetList );
    6: aCB.Items.AddStrings( SoundManager.EqualizerPresetList );
    7: aCB.Items.AddStrings( SoundManager.FreqShifterPresetList );
    8: aCB.Items.AddStrings( SoundManager.PitchShifterPresetList );
    9: aCB.Items.AddStrings( SoundManager.RingModulatorPresetList );
   10: aCB.Items.AddStrings( SoundManager.VocalMorpherPresetList );
   11: aCB.Items.AddStrings( SoundManager.EAXReverbPresetList );
  end;
end;

procedure TFrameCmdAudioFX.ReconstructEffectChain;
begin
  if FFlag_LockPresetSelectionChange or
     ((FrameViewAudioList1.SelectedCount = 0) and TargetIsFile) then
    exit;

  // Delete all previous effects on all sounds.
  SoundManager.DeleteAllEffects(True);
  if not Panel4.Enabled then
    exit;

  if GetEffectCount > 0 then
  begin
    if FEffectsState[0] then
      CreateEffect(0);
    if FEffectsState[1] then
      CreateEffect(1);
    if FEffectsState[2] then
      CreateEffect(2);

    if TargetIsFile then
    begin
      FrameViewAudioList1.ChainEffectOnSelected;
      // Apply Dry/Wet gain.
      FrameViewAudioList1.SetDryWetOnSelected(FrameTBDryWet.Value);
    end
    else
    begin
      SoundManager.ConstructChainOn( CAPTURE_IDAUDIO );
      SoundManager.SetDryWetOn(CAPTURE_IDAUDIO, FrameTBDryWet.Value);
    end;
  end;

  if AutoPlay then
    StartPlayback(False);
end;

procedure TFrameCmdAudioFX.CreateEffect(Index: integer);
var
  effectIndex, presetIndex: integer;
  effectType: TALSEffectType;
begin
  case Index of
    0: effectIndex := ComboBox5.ItemIndex;
    1: effectIndex := ComboBox7.ItemIndex;
    2: effectIndex := ComboBox9.ItemIndex;
  end;

  case Index of
    0: presetIndex := ComboBox6.ItemIndex;
    1: presetIndex := ComboBox8.ItemIndex;
    2: presetIndex := ComboBox10.ItemIndex;
  end;
  if presetIndex = -1 then
    presetIndex := 0;

  case effectIndex of
    0: effectType := AL_EFFECT_AUTOWAH;
    1: effectType := AL_EFFECT_CHORUS;
    2: effectType := AL_EFFECT_FLANGER;
    3: effectType := AL_EFFECT_COMPRESSOR;
    4: effectType := AL_EFFECT_DISTORTION;
    5: effectType := AL_EFFECT_ECHO;
    6: effectType := AL_EFFECT_EQUALIZER;
    7: effectType := AL_EFFECT_FREQUENCYSHIFTER;
    8: effectType := AL_EFFECT_PITCHSHIFTER;
    9: effectType := AL_EFFECT_RINGMODULATOR;
   10: effectType := AL_EFFECT_VOCALMORPHER;
   11: effectType := AL_EFFECT_EAXREVERB;
  end;

  if TargetIsFile then
    FrameViewAudioList1.CreateEffectOnSelected(effectType, presetIndex)
  else
    SoundManager.AddEffectOn(CAPTURE_IDAUDIO, effectType, presetIndex);
end;

procedure TFrameCmdAudioFX.UpdatePanelEffects;
begin
  ComboBox5.Enabled := FEffectsState[0];
  ComboBox6.Enabled := FEffectsState[0];

  ComboBox7.Enabled := FEffectsState[1];
  ComboBox8.Enabled := FEffectsState[1];

  ComboBox9.Enabled := FEffectsState[2];
  ComboBox10.Enabled := FEffectsState[2];

  FrameButton[0].State := FEffectsState[0];
  FrameButton[1].State := FEffectsState[1];
  FrameButton[2].State := FEffectsState[2];

  FrameLed[0].State := FEffectsState[0];
  FrameLed[1].State := FEffectsState[1];
  FrameLed[2].State := FEffectsState[2];
end;

function TFrameCmdAudioFX.GetEffectType(Index: integer): TALSEffectType;
begin
  case Index of
   0: Result := AL_EFFECT_AUTOWAH;
   1: Result := AL_EFFECT_CHORUS;
   2: Result := AL_EFFECT_FLANGER;
   3: Result := AL_EFFECT_COMPRESSOR;
   4: Result := AL_EFFECT_DISTORTION;
   5: Result := AL_EFFECT_ECHO;
   6: Result := AL_EFFECT_EQUALIZER;
   7: Result := AL_EFFECT_FREQUENCYSHIFTER;
   8: Result := AL_EFFECT_PITCHSHIFTER;
   9: Result := AL_EFFECT_RINGMODULATOR;
  10: Result := AL_EFFECT_VOCALMORPHER;
  11: Result := AL_EFFECT_EAXREVERB;
  end;
end;

function TFrameCmdAudioFX.GetEffectCount: integer;
begin
  Result := 0;
  if FEffectsState[0] then inc(Result);
  if FEffectsState[1] then inc(Result);
  if FEffectsState[2] then inc(Result);
end;

function TFrameCmdAudioFX.AutoPlay: boolean;
begin
  Result := CheckBox1.Checked;
end;

procedure TFrameCmdAudioFX.ProcessFileSelectionChange(Sender: TObject);
begin
  SoundManager.StopAllSound(True);
  SoundManager.DeleteAllEffects(True);

  if AutoPlay and
     not FToogleSpeedButtonManager.Checked[SpeedButton7] then
    ReconstructEffectChain;
end;

procedure TFrameCmdAudioFX.GenerateCmdForApply;
var i: integer;
  snd: TALSSound;
  wetdry: single;
begin
  FCmds := '';
  wetdry := FrameTBDryWet.Value;

  if TargetIsFile then
  begin
    for i:=0 to High(FSelectedSounds) do
    begin
      snd:=FSelectedSounds[i];
      FCmds.ConcatCmd( CmdTitleAudioApplyFX(snd.Tag, wetdry, GetEffectCount));
      if FEffectsState[0] then
        FCmds.ConcatCmd(CmdAudioFXPreset(GetEffectType(ComboBox5.ItemIndex), ComboBox6.ItemIndex));
      if FEffectsState[1] then
        FCmds.ConcatCmd(CmdAudioFXPreset(GetEffectType(ComboBox7.ItemIndex), ComboBox8.ItemIndex));
      if FEffectsState[2] then
        FCmds.ConcatCmd(CmdAudioFXPreset(GetEffectType(ComboBox9.ItemIndex), ComboBox10.ItemIndex));
    end;
  end
  else
  begin
    FCmds.ConcatCmd( CmdTitleAudioCaptureApplyFX(wetdry, GetEffectCount));
    if FEffectsState[0] then
      FCmds.ConcatCmd(CmdAudioCaptureFXPreset(GetEffectType(ComboBox5.ItemIndex), ComboBox6.ItemIndex));
    if FEffectsState[1] then
      FCmds.ConcatCmd(CmdAudioCaptureFXPreset(GetEffectType(ComboBox7.ItemIndex), ComboBox8.ItemIndex));
    if FEffectsState[2] then
      FCmds.ConcatCmd(CmdAudioCaptureFXPreset(GetEffectType(ComboBox9.ItemIndex), ComboBox10.ItemIndex));
  end;

  if TargetIsFile then
    FShortReadableString:=SAudioConnectEffect+' '
  else
    FShortReadableString:=SAudioCaptureConnectEffect+' ';

  if FEffectsState[0] then
    FShortReadableString += NameOfAudioFXName[ComboBox5.ItemIndex];

  if FEffectsState[1] then
  begin
    if FEffectsState[0] then
      FShortReadableString+='+';
    FShortReadableString += NameOfAudioFXName[ComboBox7.ItemIndex];
  end;

  if FEffectsState[2] then
  begin
    if FEffectsState[0] or FEffectsState[1] then
      FShortReadableString+='+';
    FShortReadableString += NameOfAudioFXName[ComboBox9.ItemIndex];
  end;

  if TargetIsFile then
  begin
    FShortReadableString+=' '+SOn_+' ';
    if Length(FSelectedSounds) > 1 then
      FShortReadableString:=FShortReadableString+SMultiple
    else
      FShortReadableString:=FShortReadableString+ExtractFileName(snd.Filename);
  end;

  FCmdDuration:=0.0;
  DoOnAddCmd;
end;

procedure TFrameCmdAudioFX.GenerateCmdForRemove;
var i: integer;
  snd: TALSSound;
begin
  FCmds := '';

  if TargetIsFile then
  begin
    for i:=0 to High(FSelectedSounds) do
    begin
      snd:=FSelectedSounds[i];
      FCmds.ConcatCmd( CmdAudioRemoveFX(snd.Tag));
    end;

    FShortReadableString:=SAudioDisconnectEffect+' '+SOn_+' ';
    if Length(FSelectedSounds)>1
      then FShortReadableString:=FShortReadableString+SMultiple
      else FShortReadableString:=FShortReadableString+ExtractFileName(snd.Filename);
  end
  else
  begin
    FCmds.ConcatCmd( CmdAudioCaptureRemoveFX );
    FShortReadableString := SAudioCaptureDisconnectEffect;
  end;

  FCmdDuration:=0.0;
  DoOnAddCmd;
end;

procedure TFrameCmdAudioFX.DoOnAddCmd;
begin
  if FOnAddCmd<>NIL
    then FOnAddCmd(Self);
end;

procedure TFrameCmdAudioFX.StartPlayback(aFromBegining: boolean);
begin
  if TargetIsFile then
    FrameViewAudioList1.PlaySelected(aFromBegining)
  else
    SoundManager.StartCaptureToPlayback;
end;

function TFrameCmdAudioFX.TargetIsFile: boolean;
begin
  Result := NoteBook1.Page[NoteBook1.PageIndex] = PageFile;
end;

function TFrameCmdAudioFX.AudioFXToPreset: string;
begin
  Result := GetEffectCount.ToString+PRESET_SEPARATOR+
            FormatFloatWithDot('0.000', FrameTBDryWet.Value);
  if FEffectsState[0] then
    Result := Result+PRESET_SEPARATOR+ComboBox5.ItemIndex.ToString+
                     PRESET_SEPARATOR+ComboBox6.ItemIndex.ToString;

  if FEffectsState[1] then
    Result := Result+PRESET_SEPARATOR+ComboBox7.ItemIndex.ToString+
                     PRESET_SEPARATOR+ComboBox8.ItemIndex.ToString;

  if FEffectsState[2] then
    Result := Result+PRESET_SEPARATOR+ComboBox9.ItemIndex.ToString+
                     PRESET_SEPARATOR+ComboBox10.ItemIndex.ToString;
end;

procedure TFrameCmdAudioFX.PresetToAudioFX(const A: TStringArray);
var
  c, k: integer;
begin
  if Length(A) < 2 then exit;

  FFlag_LockPresetSelectionChange := True;

  c := A[0].ToInteger;
  FrameTBDryWet.Value := StringToSingle(A[1]);

  k := 2;
  FEffectsState[0] := c > 0;
  if c > 0 then
  begin
    ComboBox5.ItemIndex := A[k].ToInteger;
    FillPresetList(ComboBox6, ComboBox5.ItemIndex);
    ComboBox6.ItemIndex := A[k+1].ToInteger;
    inc(k, 2);
    dec(c);
  end;

  FEffectsState[1] := c > 0;
  if c > 0 then
  begin
    ComboBox7.ItemIndex := A[k].ToInteger;
    FillPresetList(ComboBox8, ComboBox7.ItemIndex);
    ComboBox8.ItemIndex := A[k+1].ToInteger;
    inc(k, 2);
    dec(c);
  end;

  FEffectsState[2] := c > 0;
  if c > 0 then
  begin
    ComboBox9.ItemIndex := A[k].ToInteger;
    FillPresetList(ComboBox10, ComboBox9.ItemIndex);
    ComboBox10.ItemIndex := A[k+1].ToInteger;
    inc(k, 2);
    dec(c);
  end;

  FFlag_LockPresetSelectionChange := False;

  UpdatePanelEffects;
  ReconstructEffectChain;
end;

procedure TFrameCmdAudioFX.ProcessSourceChangeEvent(Sender: TObject);
begin
  SoundManager.StopAllSound(True);
  SoundManager.DeleteAllEffects(True);

  if not TargetIsFile then
  begin
    SoundManager.StartCaptureToPlayback;
    ReconstructEffectChain;
    if SoundManager.CaptureToPlaybackIsReady then
      Label13.Caption := SReady
    else
      Label13.Caption := SoundManager.CaptureStrError;
  end;
end;

constructor TFrameCmdAudioFX.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FrameViewAudioList1:=TFrameViewAudioList.Create(Self);
  FrameViewAudioList1.Parent:=Panel2;
  FrameViewAudioList1.Align:=alClient;
  FrameViewAudioList1.MouseCanMoveItem := False;
  FrameViewAudioList1.MultiSelect:=True;
  FrameViewAudioList1.OnSelectionChange:=@ProcessFileSelectionChange;

  FAudioFXPresetManager := TPresetManager.Create(Self);
  FAudioFXPresetManager.Init1(SAudioFXPresets, BPreset, GetAppAudioPresetsFile);
  FAudioFXPresetManager.Init2(@PresetToAudioFX, @AudioFXToPreset);

  FNoteBookManager := TNoteBookManager.Create(Notebook1);
  with FNoteBookManager do
  begin
    SetActivatedColors($0003C4FC, clBlack);
    SetDeactivatedColors($00484848, $00EAEAEA);
    LinkButtonToPage(SpeedButton4, PageFile);
    LinkButtonToPage(SpeedButton5, PageMicro);
    ActivePage( PageFile );
    OnSelectionChange := @ProcessSourceChangeEvent;
  end;

  FToogleSpeedButtonManager:=TToggleSpeedButtonManager.Create;
  with FToogleSpeedButtonManager do
  begin
    ToggleType := tsbLikeRadioButton;
    SetActivatedColors($0003C4FC, $00151515);
    SetDeactivatedColors($00556666, $00EAEAEA);
    SetImageIndexes(3, -1);
    Add(SpeedButton6, True);
    Add(SpeedButton7, FALSE);
  end;

  CheckedLabelManager.CaptureLabelClick( Label2 );

  FrameLed[0] := TFrameLed.Create(Self);
  with FrameLed[0] do begin
    AssociateToPanel(Panel10);
    GreenType;
    BlinkWhenOn := True;
    DontUseInternalTimer;
  end;

  FrameLed[1] := TFrameLed.Create(Self);
  with FrameLed[1] do begin
    AssociateToPanel(Panel11);
    GreenType;
    BlinkWhenOn := True;
    DontUseInternalTimer;
  end;

  FrameLed[2] := TFrameLed.Create(Self);
  with FrameLed[2] do begin
    AssociateToPanel(Panel12);
    GreenType;
    BlinkWhenOn := True;
    DontUseInternalTimer;
  end;

  FrameButton[0] := TFrameButtonOnOff.Create(Self);
  FrameButton[0].AssociateWith(Panel13);

  FrameButton[1] := TFrameButtonOnOff.Create(Self);
  FrameButton[1].AssociateWith(Panel14);

  FrameButton[2] := TFrameButtonOnOff.Create(Self);
  FrameButton[2].AssociateWith(Panel15);

  FEffectsState[0] := True;
  FEffectsState[1] := False;
  FEffectsState[2] := False;
  UpdatePanelEffects;

  FrameTBDryWet := TFrameTBAudioDryWet.Create(Self, Panel16);
  FrameTBDryWet.Init(trHorizontal, False, False, False);
  FrameTBDryWet.Value := 0.5;
  FrameTBDryWet.OnChange := @TrackBar1Change;

  Timer1.Enabled := True;

  {$ifdef Linux}
  // small font for Linux
  Label1.Font.Height := 18;
  ComboBox5.Font.Height := 14;
  Label7.Font.Height := 14;
  ComboBox6.Font.Height := 11;

  Label8.Font.Height := 18;
  ComboBox7.Font.Height := 14;
  Label9.Font.Height := 14;
  ComboBox8.Font.Height := 11;

  Label10.Font.Height := 18;
  ComboBox9.Font.Height := 14;
  Label11.Font.Height := 14;
  ComboBox10.Font.Height := 11;
  {$endif}
end;

destructor TFrameCmdAudioFX.Destroy;
begin
  FToogleSpeedButtonManager.Free;
  FNoteBookManager.Free;
  inherited Destroy;
end;

procedure TFrameCmdAudioFX.Init;
var i: integer;
begin
  FAudioFXPresetManager.Load;
  FrameViewAudioList1.Fill;

  // Fill combobox with Effect names
  FFlag_LockPresetSelectionChange := True;
  if ComboBox5.Items.Count <> Length(NameOfAudioFXName) then
  begin
    ComboBox5.Clear;
    ComboBox7.Clear;
    ComboBox9.Clear;
    for i:=Low(NameOfAudioFXName) to High(NameOfAudioFXName) do
    begin
      ComboBox5.Items.Add(NameOfAudioFXName[i]);
      ComboBox7.Items.Add(NameOfAudioFXName[i]);
      ComboBox9.Items.Add(NameOfAudioFXName[i]);
    end;
    ComboBox5.ItemIndex := 0;
    ComboBox7.ItemIndex := 0;
    ComboBox9.ItemIndex := 0;
  end;
  // Fill preset
  if ComboBox6.Items.Count=0 then
  begin
    FillPresetList(ComboBox6, 0);
    ComboBox6.ItemIndex := 0;
  end;
  if ComboBox8.Items.Count=0 then
  begin
    FillPresetList(ComboBox8, 0);
    ComboBox8.ItemIndex := 0;
  end;
  if ComboBox10.Items.Count=0 then
  begin
    FillPresetList(ComboBox10, 0);
    ComboBox10.ItemIndex := 0;
  end;
  FFlag_LockPresetSelectionChange := False;

  // translation
  BPreset.Caption := SPreset_;
  BAddCmd.Caption := SAdd;
  Label20.Caption := SDry_;
  Label22.Caption := SWet_;
  Label21.Caption := SDryWet;
  Label7.Caption := SPreset;
  Label9.Caption := SPreset;
  Label11.Caption := SPreset;
  SpeedButton5.Caption := SAudioCapture_;
  SpeedButton4.Caption := SFiles_;
end;


end.

