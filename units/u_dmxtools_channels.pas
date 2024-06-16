unit u_dmxtools_channels;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, Spin, ComCtrls, LCLTranslator, frame_velocity, u_notebook_util,
  u_list_dmxuniverse, frame_fx_channelchaser, u_common,
  frame_viewchannelslist, u_presetmanager, frame_viewprojectors,
  frame_trackbar;

type

  { TFormDMXChannelsTools }

  TFormDMXChannelsTools = class(TForm)
    BAdd1: TSpeedButton;
    BAdd2: TSpeedButton;
    BAdd3: TSpeedButton;
    BAdd4: TSpeedButton;
    BAdd6: TSpeedButton;
    ButtonAudioFollowerPreset: TSpeedButton;
    ComboBox1: TComboBox;
    FloatSpinEdit1: TFloatSpinEdit;
    FloatSpinEdit2: TFloatSpinEdit;
    FloatSpinEdit3: TFloatSpinEdit;
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
    Label25: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label3: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Notebook1: TNotebook;
    PageFlash: TPage;
    PageChaser: TPage;
    PageStop: TPage;
    PageCopy: TPage;
    PageAudioFollower: TPage;
    PageFlame: TPage;
    PageDimmer: TPage;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    Shape1: TShape;
    Shape2: TShape;
    SpeedButton1: TSpeedButton;
    SpeedButton10: TSpeedButton;
    BFlashPreview: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    BAdd5: TSpeedButton;
    BAudioPlay: TSpeedButton;
    BAudioStop: TSpeedButton;
    SpeedButton8: TSpeedButton;
    ButtonFlamePreset: TSpeedButton;
    TB1: TTrackBar;
    TB2: TTrackBar;
    TB3: TTrackBar;
    TB4: TTrackBar;
    TB5: TTrackBar;
    TB6: TTrackBar;
    TB7: TTrackBar;
    procedure BAdd1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure RadioButton1Change(Sender: TObject);
    procedure BFlashPreviewClick(Sender: TObject);
    procedure BAudioPlayClick(Sender: TObject);
    procedure BAudioStopClick(Sender: TObject);
    procedure TB1Change(Sender: TObject);
  private
    FFirstShown: boolean;
    Frame_Velocity1: TFrame_Velocity;
    FrameViewChannelsList1: TFrameViewChannelsList;
    FNoteBookManager: TNoteBookManager;
    FCheckedLabelManager: TCheckedLabelManager;
    FTargetChannels: ArrayOfDmxChannels;
    FSourceChannelForCopy: TDMXChannel;
    procedure GetTargetChannels;
    procedure StopAll;
    procedure ProcessPageSelectionChange(Sender: TObject);
    procedure ProcessSourceCopySelectionChange(Sender: TObject);
    procedure UpdateWidgets;
    procedure GenerateCmdForDimmer;
    procedure GenerateCmdForFlame;
    procedure GenerateCmdForAudioFollower;
    procedure GenerateCmdForFlash;
    procedure GenerateCmdForCopy;
    procedure GenerateCmdForStopEffect;
  private
    procedure ApplyEffectOnTargetChannels;
    procedure StopEffectOnSelected;
  private
    FFlamePresetManager: TPresetManager;
    function FlameToPreset: string;
    procedure PresetToFlame(const A: TStringArray);
  private
    FAudioFollowerPresetManager: TPresetManager;
    function AudioFollowerToPreset: string;
    procedure PresetToAudioFollower(const A: TStringArray);
  private
    FrameTrackBar1: TFrameTrackBar;
  private
    FTargetViewProjector: TFrameViewProjector;
    FGUIMode: TGUIMode;
    procedure SetGUIMode(AValue: TGUIMode);
  public
    FrameFXChannelChaser1: TFrameFXChannelChaser;
    FShortReadable: string;
    FCmd: TSingleCmd;
    FDuration: single;

    procedure UpdateEditMode;
    procedure UpdateStringAfterLanguageChange;

    procedure UserHaveSelected(aChan: TDMXChannel);
    procedure UserHaveRemoved(aChan: TDMXChannel);
    procedure ClearSelectedChannels;

    property GUIMode: TGUIMode read FGUIMode write SetGUIMode;
  end;

var
  FormDMXChannelsTools: TFormDMXChannelsTools;

implementation

uses LCLType, ALSound, u_resource_string, u_utils,
  u_audio_manager, u_project_manager, u_helper, u_sequence_player,
  u_add_action_dmx, u_mainform;

{$R *.lfm}

{ TFormDMXChannelsTools }

procedure TFormDMXChannelsTools.FormShow(Sender: TObject);
var i: integer;
begin
  if FFirstShown then
  begin
    FNoteBookManager.ActivePage(PageDimmer);
    FFirstShown := False;
  end;

  ButtonFlamePreset.Hint := SPresetButtonHint;
  ButtonAudioFollowerPreset.Hint := SPresetButtonHint;

  ButtonFlamePreset.Caption := SPreset_;
  ButtonAudioFollowerPreset.Caption := SPreset_;

  SpeedButton1.Caption := SDimmer;
  SpeedButton2.Caption := SFlame;
  SpeedButton8.Caption := SChaser;
  SpeedButton3.Caption := SAudioFollower;
  SpeedButton10.Caption := SFlash;
  SpeedButton4.Caption := SCopy;
  SpeedButton5.Caption := SStop;

  Label17.Caption := SpeedButton5.Hint;
  Label31.Caption := SSeconds_;

  Frame_Velocity1.UpdateList;
  FrameFXChannelChaser1.Fill;

  FrameViewChannelsList1.Fill;

  ComboBox1.Clear;
  for i:=0 to SoundManager.Count-1 do
    ComboBox1.Items.Add(ExtractFileName(SoundManager.GetSoundByIndex(i).Filename));

  UpdateWidgets;
end;

procedure TFormDMXChannelsTools.RadioButton1Change(Sender: TObject);
begin
  if Sender = RadioButton1 then
    FrameTrackBar1.Init(trHorizontal, False, RadioButton2.Checked, True);

  if Sender = FloatSpinEdit2 then
    if FloatSpinEdit3.Value < FloatSpinEdit2.Value then
      FloatSpinEdit3.Value := FloatSpinEdit2.Value;
  if Sender = FloatSpinEdit3 then
    if FloatSpinEdit2.Value > FloatSpinEdit3.Value then
      FloatSpinEdit2.Value := FloatSpinEdit3.Value;

  UpdateWidgets;
end;

procedure TFormDMXChannelsTools.BFlashPreviewClick(Sender: TObject);
begin
  ApplyEffectOnTargetChannels;
end;

procedure TFormDMXChannelsTools.BAudioPlayClick(Sender: TObject);
var snd: TALSSound;
begin
  UpdateWidgets;
  ApplyEffectOnTargetChannels;
  snd := SoundManager.GetSoundByIndex(ComboBox1.ItemIndex);
  if snd=NIL then
    exit;

  if snd.State <> ALS_PLAYING then
    snd.Play;
end;

procedure TFormDMXChannelsTools.BAudioStopClick(Sender: TObject);
begin
  SoundManager.StopAllSound;
end;

procedure TFormDMXChannelsTools.TB1Change(Sender: TObject);
begin
  if Sender = TB1 then
   if TB1.Position <= TB3.Position then
     TB3.Position := TB1.Position-1;
  if Sender = TB3 then
   if TB3.Position >= TB1.Position then
     TB1.Position := TB3.Position+1;

  ApplyEffectOnTargetChannels;
  UpdateWidgets;
end;

procedure TFormDMXChannelsTools.StopAll;
var i: integer;
  snd: TALSSound;
begin
  SeqPlayer.StopPreview;
  i := ComboBox1.ItemIndex;
  if i >- 1 then
  begin
    snd := SoundManager.GetSoundByIndex(ComboBox1.ItemIndex);
    if snd <> NIL then
      snd.Stop;
  end;

  GetTargetChannels;
  for i:=0 to High(FTargetChannels) do
   FTargetChannels[i].StopEffect;
end;

procedure TFormDMXChannelsTools.ProcessPageSelectionChange(Sender: TObject);
begin
  if Notebook1.PageIndex = Notebook1.IndexOf(PageChaser) then
    FrameFXChannelChaser1.Fill;

  if Notebook1.PageIndex <> Notebook1.IndexOf(PageCopy) then
    FTargetViewProjector.FrameViewDMXCursors1.SetSourceChannelForCopy(NIL)
  else
    ProcessSourceCopySelectionChange(NIL);

  if (Notebook1.PageIndex = Notebook1.IndexOf(PageFlame)) or
     (Notebook1.PageIndex = Notebook1.IndexOf(PageCopy)) or
     (Notebook1.PageIndex = Notebook1.IndexOf(PageStop)) then
    ApplyEffectOnTargetChannels;
end;

procedure TFormDMXChannelsTools.ProcessSourceCopySelectionChange(Sender: TObject);
begin
  if FrameViewChannelsList1.SelCount = 0 then
  begin
    Label19.Caption := SNone;
    Label20.Caption := '';
    Label21.Caption := '';
    FTargetViewProjector.FrameViewDMXCursors1.SetSourceChannelForCopy(NIL);
    StopEffectOnSelected;
  end
  else
  begin
    Label19.Caption := FrameViewChannelsList1.Selected[0].Fixture.Name;
    Label20.Caption := FrameViewChannelsList1.Selected[0].Fixture.Description;
    Label21.Caption := FrameViewChannelsList1.Selected[0].Name;
    FTargetViewProjector.FrameViewDMXCursors1.SetSourceChannelForCopy(FrameViewChannelsList1.Selected[0]);
    ApplyEffectOnTargetChannels;
    FTargetViewProjector.FrameViewDMXCursors1.RedrawAll;
  end;
end;

procedure TFormDMXChannelsTools.UpdateWidgets;
begin
  // audio follower
  if TB4.Position < 0 then
    Label8.Caption := TB4.Position.ToString
  else
    Label8.Caption := '+'+TB4.Position.ToString;
  Label9.Caption := TB5.Position.ToString+' ('+FormatFloat('0.0', TB5.Position/TB5.Max*100)+'%)';
  Label10.Caption := FormatFloat('0.00', TB6.Position/10)+SSec;
  // flame
  Label11.Caption := FormatFloat('0.0', TB1.Position/TB1.Max*100)+'%';
  Label13.Caption := FormatFloat('0.0', TB3.Position/TB3.Max*100)+'%';
  Label12.Caption := FormatFloat('0.00', TB2.Position/100)+SSec;
  Label15.Caption := FormatFloat('0.0', TB7.Position/TB7.Max*100)+'%';
  // flash
  FloatSpinEdit3.Enabled := RadioButton4.Checked;
end;

procedure TFormDMXChannelsTools.GenerateCmdForDimmer;
var i: integer;
  chan: TDMXChannel;
begin
  FCmd := CmdTitleDMXDimmer(FloatSpinEdit1.Value, Frame_Velocity1.SelectedCurveID);

  GetTargetChannels;
  for i:=0 to High(FTargetChannels) do
  begin
    chan := FTargetChannels[i];
    FCmd.ConcatCmd(CmdDMXDimmer( chan.Universe.ID,
                                 chan.Fixture.ID,
                                 chan.Index,
                                 chan.PercentValue,
                                 FloatSpinEdit1.Value,
                                 Frame_Velocity1.SelectedCurveID));
  end;
  // DMXDIMMER IDuniverse dmxadress percent duration IDcurve
  FShortReadable := SDMXDimmer;
  if Length(FTargetChannels) > 1 then
    FShortReadable := FShortReadable + ' '+SMultiple
  else begin
    if chan.Fixture.Description <> '' then FShortReadable := FShortReadable + ' '+chan.Fixture.Description;
    if FTargetChannels[0].Fixture.ChannelsCount > 1 then FShortReadable := FShortReadable + ' '+chan.Name;
    FShortReadable := FShortReadable + ' '+STo+' '+FormatFloat('0.00',chan.PercentValue*100);
  end;

  FDuration := FloatSpinEdit1.Value
end;

procedure TFormDMXChannelsTools.GenerateCmdForFlame;
var i: integer;
  chan: TDMXChannel;
begin
  FCmd := CmdTitleDMXFlame(TB3.Position/TB3.Max,
                          TB1.Position/TB1.Max,
                          TB2.Position/100,
                          TB7.Position/TB7.Max);
  GetTargetChannels;
  for i:=0 to High(FTargetChannels) do
  begin
    chan := FTargetChannels[i];
    FCmd.ConcatCmd(CmdDMXFlame(chan.Universe.ID,
                   chan.Fixture.ID,
                   chan.Index,
                   TB3.Position/TB3.Max,
                   TB1.Position/TB1.Max,
                   TB2.Position/100,
                   TB7.Position/TB7.Max));
  end;
  // DMXFLAMME IDuniverse IDFixture dmxadress LevelMin LevelMax Speed Soften Addmode
  FShortReadable := SDMXFlame;
  if Length(FTargetChannels) > 1 then
    FShortReadable := FShortReadable + ' '+SMultiple
  else begin
    if chan.Fixture.Description <> '' then FShortReadable := FShortReadable + ' '+chan.Fixture.Description;
    if FTargetChannels[0].Fixture.ChannelsCount > 1 then FShortReadable := FShortReadable + ' '+chan.Name;
  end;
  FDuration := 0.0;
end;

procedure TFormDMXChannelsTools.GenerateCmdForAudioFollower;
var i: integer;
  chan: TDMXChannel;
  snd: TALSSound;
begin
  snd := SoundManager.GetSoundByIndex(ComboBox1.ItemIndex);
  FCmd := CmdTitleDMXAudioFollower(snd.Tag, TB4.Position/10, TB5.Position/TB5.Max, TB6.Position/10);

  GetTargetChannels;
  for i:=0 to High(FTargetChannels) do
  begin
    chan := FTargetChannels[i];
    FCmd.ConcatCmd(CmdDMXAudioFollower(chan.Universe.ID, chan.Fixture.ID, chan.Index,
                   snd.Tag, TB4.Position/10, TB5.Position/TB5.Max, TB6.Position/10));
  end;
  // DMXSUIVEURAUDIO IDuniverse IDFixture dmxadress IDaudio gainF MaxPercentF SoftenTimeF
  FShortReadable := SDMXAudioFollower+' '+ExtractFileName(snd.Filename)+' '+SOn_;
  if Length(FTargetChannels) > 1 then
    FShortReadable := FShortReadable + ' '+SMultiple
  else begin
    if chan.Fixture.Description <> '' then FShortReadable := FShortReadable + ' '+chan.Fixture.Description;

    if FTargetChannels[0].Fixture.ChannelsCount > 1 then FShortReadable := FShortReadable+' '+chan.Name;
  end;
  FDuration := 0.0;
end;

procedure TFormDMXChannelsTools.GenerateCmdForFlash;
var i: integer;
  chan: TDMXChannel;
  vmin, vmax, dmin, dmax: single;
begin
  vmax :=FrameTrackBar1.PercentValue;
  if RadioButton1.Checked then
    vmin := vmax
  else
    vmin := FrameTrackBar1.IntervalMin;

  dmin := FloatSpinEdit2.Value;
  if RadioButton3.Checked then
    dmax := dmin
  else
    dmax := FloatSpinEdit3.Value;
  FCmd := CmdTitleDMXFlash(vmin, vmax, dmin, dmax);

  GetTargetChannels;
  for i:=0 to High(FTargetChannels) do
  begin
    chan := FTargetChannels[i];
    FCmd.ConcatCmd(CmdDMXFlash( chan.Universe.ID, chan.Fixture.ID, chan.Index,
                                vmin, vmax, dmin, dmax));
  end;
  // DMXFlash IDuniverse IDFixture ChanIndex LevelMin LevelMax DurationMin DurationMax
  FShortReadable := SDMXFlash+' ';
  if RadioButton2.Checked then
    FShortReadable := FShortReadable+SRandomValue+' '+
       DMXPercentToString(vmin)+' - '+DMXPercentToString(vmax)+' '
  else
    FShortReadable := FShortReadable+STo+' '+DMXPercentToString(vmax)+' ';
  if RadioButton4.Checked then
    FShortReadable := FShortReadable+SRandomDuration+' '+DurationToString(dmin)+' - '+DurationToString(dmax)
  else
    FShortReadable := FShortReadable+SIn+' '+DurationToString(dmax);

  if Length(FTargetChannels) > 1 then
    FShortReadable := FShortReadable+' '+SOn+' '+SMultiple
  else begin
    FShortReadable := FShortReadable+' '+SOn;
    if chan.Fixture.Description <> '' then FShortReadable := FShortReadable + ' '+chan.Fixture.Description;
    if FTargetChannels[0].Fixture.ChannelsCount > 1 then FShortReadable := FShortReadable+' '+chan.Name;
  end;
  FDuration := dmax;
end;

procedure TFormDMXChannelsTools.GenerateCmdForCopy;
var i: integer;
  chan, targetChan: TDMXChannel;
begin
  FCmd := CmdTitleDMXCopy(FSourceChannelForCopy.Universe.ID,
                         FSourceChannelForCopy.Fixture.ID,
                         FSourceChannelForCopy.Index);

  GetTargetChannels;
  for i:=0 to High(FTargetChannels) do
  begin
    chan := FTargetChannels[i];
    if chan.Selected and (chan <> FSourceChannelForCopy) then begin
      FCmd.ConcatCmd(CmdDMXCopy(FSourceChannelForCopy.Universe.ID,
                     FSourceChannelForCopy.Fixture.ID,
                     FSourceChannelForCopy.Index,
                     chan.Universe.ID, chan.Fixture.ID, chan.Index));
      targetChan := chan;
    end;
  end;
  // DMXCOPIECANAL SourceIDuniverse SourceIDFixture Sourcedmxadress TargetIDUniverse TargetIDFixture Targetdmxadress
  FShortReadable := SDMXCopy+' ';
  if FSourceChannelForCopy.Fixture.Description <> '' then FShortReadable := FShortReadable + FSourceChannelForCopy.Fixture.Description+' ';
  FShortReadable := FShortReadable+FSourceChannelForCopy.Name+' '+SOn_;
  if Length(FTargetChannels) > 2 then
    FShortReadable := FShortReadable + ' '+SMultiple
  else begin
    if targetChan.Fixture.Description <> '' then FShortReadable := FShortReadable + ' '+targetChan.Fixture.Description;
    if FTargetChannels[0].Fixture.ChannelsCount > 1 then FShortReadable := FShortReadable + ' '+targetChan.Name;
  end;
  FDuration := 0.0;
end;

procedure TFormDMXChannelsTools.GenerateCmdForStopEffect;
var i: integer;
  chan: TDMXChannel;
begin
  FCmd := CmdTitleDMXStopEffect;
  GetTargetChannels;
  for i:=0 to High(FTargetChannels) do
  begin
    chan := FTargetChannels[i];
    FCmd.ConcatCmd(CmdDMXStopEffect(chan.Universe.ID, chan.Fixture.ID, chan.Index));
  end;
  // DMXSTOPEFFET IDuniverse IDFixture dmxadress
  FShortReadable := SDMXStopEffect+' '+Son_;
  if Length(FTargetChannels) > 1 then
    FShortReadable := FShortReadable + ' '+SMultiple
  else begin
    if chan.Fixture.Description <> '' then FShortReadable := FShortReadable + ' '+chan.Fixture.Description;
    if FTargetChannels[0].Fixture.ChannelsCount > 1 then FShortReadable := FShortReadable + ' '+chan.Name;
  end;
  FDuration := 0.0;
end;

procedure TFormDMXChannelsTools.ApplyEffectOnTargetChannels;
var i: integer;
  sourcechan: TDMXChannel;
  snd: TALSSound;
  vmin, vmax, dmin, dmax: single;
begin
  GetTargetChannels;
  if Length(FTargetChannels) = 0 then exit;

  if Notebook1.PageIndex=Notebook1.IndexOf(PageFlame) then
    for i:=0 to High(FTargetChannels) do
    begin
      FTargetChannels[i].StartFlame(TB3.Position/TB3.Max,
                                    TB1.Position/TB1.Max,
                                    TB2.Position/100,
                                    TB7.Position/TB7.Max);
    end;

  if Notebook1.PageIndex = Notebook1.IndexOf(PageAudioFollower) then
  begin
    snd := SoundManager.GetSoundByIndex(ComboBox1.ItemIndex);
    if snd = NIL then
      exit;
    for i:=0 to High(FTargetChannels) do
      FTargetChannels[i].StartAudioFollower(snd.Tag, TB4.Position/10,
                       TB5.Position/TB5.Max, TB6.Position/10);
  end;

  if Notebook1.PageIndex = Notebook1.IndexOf(PageCopy) then
  begin
    sourcechan := FTargetViewProjector.FrameViewDMXCursors1.GetSourceChannelForCopy;
    for i:=0 to High(FTargetChannels) do
     if FTargetChannels[i].Selected then
       FTargetChannels[i].StartCopy(sourcechan);
  end;

  if Notebook1.PageIndex = Notebook1.IndexOf(PageStop) then
    StopEffectOnSelected;

  if Notebook1.PageIndex = Notebook1.IndexOf(PageFlash) then
    for i:=0 to High(FTargetChannels) do
    begin
      vmax := FrameTrackBar1.PercentValue;
      if RadioButton1.Checked then
        vmin := vmax
      else
        vmin := FrameTrackBar1.IntervalMin;
      dmin := FloatSpinEdit2.Value;
      if RadioButton3.Checked then
        dmax := dmin
      else
        dmax := FloatSpinEdit3.Value;
      FTargetChannels[i].StartFlash(vmin, vmax, dmin, dmax);
    end;
end;

procedure TFormDMXChannelsTools.StopEffectOnSelected;
var
  i: Integer;
begin
  for i:=0 to High(FTargetChannels) do
   if FTargetChannels[i].Selected then
     FTargetChannels[i].StopEffect;
end;

function TFormDMXChannelsTools.FlameToPreset: string;
begin
  Result := TB1.Position.ToString+PRESET_SEPARATOR+
            TB3.Position.ToString+PRESET_SEPARATOR+
            TB2.Position.ToString+PRESET_SEPARATOR+
            TB7.Position.ToString;
end;

procedure TFormDMXChannelsTools.PresetToFlame(const A: TStringArray);
begin
  if Length(A) <> 4 then exit;
  TB1.Position := A[0].ToInteger;
  TB3.Position := A[1].ToInteger;
  TB2.Position := A[2].ToInteger;
  TB7.Position := A[3].ToInteger;
  UpdateWidgets;
  ApplyEffectOnTargetChannels;
end;

function TFormDMXChannelsTools.AudioFollowerToPreset: string;
begin
  Result := TB4.Position.ToString+PRESET_SEPARATOR+
            TB5.Position.ToString+PRESET_SEPARATOR+
            TB6.Position.ToString;
end;

procedure TFormDMXChannelsTools.PresetToAudioFollower(const A: TStringArray);
begin
  if Length(A) <> 3 then exit;
  TB4.Position := A[0].ToInteger;
  TB5.Position := A[1].ToInteger;
  TB6.Position := A[2].ToInteger;
  UpdateWidgets;
end;

procedure TFormDMXChannelsTools.SetGUIMode(AValue: TGUIMode);
begin
  UpdateStringAfterLanguageChange;
  FFlamePresetManager.Load;
  FAudioFollowerPresetManager.Load;

  FGUIMode := AValue;
  FrameFXChannelChaser1.GUIMode := AValue;

  case FGUIMode of
    guiMainDMX:
      begin
        BAdd1.Caption := SCreateSequence;
        BAdd2.Caption := SCreateSequence;
        BAdd3.Caption := SCreateSequence;
        BAdd4.Caption := SCreateSequence;
        BAdd5.Caption := SCreateSequence;
        BAdd6.Caption := SCreateSequence;
        FTargetViewProjector := FormMain.FrameViewProjector1;
        FrameViewChannelsList1.FTargetViewProjector := FTargetViewProjector;
      end;

    guiEditSequence:
      begin
        BAdd1.Caption := SAdd;
        BAdd2.Caption := SAdd;
        BAdd3.Caption := SAdd;
        BAdd4.Caption := SAdd;
        BAdd5.Caption := SAdd;
        BAdd6.Caption := SAdd;
        FTargetViewProjector := FormAddDMXAction.FrameViewProjector1;
        FrameViewChannelsList1.FTargetViewProjector := FTargetViewProjector;
      end;
  end;
end;

procedure TFormDMXChannelsTools.UpdateEditMode;
begin
  BAdd1.Visible := Project.Options.EditMode;
  BAdd2.Visible := Project.Options.EditMode;
  BAdd3.Visible := Project.Options.EditMode;
  BAdd4.Visible := Project.Options.EditMode;
  BAdd5.Visible := Project.Options.EditMode;
  FrameFXChannelChaser1.UpdateEditMode;
end;

procedure TFormDMXChannelsTools.UpdateStringAfterLanguageChange;
begin
  Label1.Caption := SDurationInSecond;
  Label25.Caption := SVelocity;
  Label4.Caption := SLevelMin;
  Label2.Caption := SLevelMax;
  Label3.Caption := SWaitTime;
  Label14.Caption := SSoften;
  Label23.Caption := SFollow;
  Label5.Caption := sGain;
  Label6.Caption := SBrightnessMax;
  Label7.Caption := SSoftenOn;
  Label28.Caption := SFixedValue;
  Label29.Caption := SRandomValueBetween;
  Label22.Caption := SFixedDuration;
  Label30.Caption := SRandomDurationBetween;

  FFlamePresetManager.UpdateStringAfterLanguageChange;
  FAudioFollowerPresetManager.UpdateStringAfterLanguageChange;
end;

procedure TFormDMXChannelsTools.UserHaveSelected(aChan: TDMXChannel);
begin
  FrameFXChannelChaser1.UserHaveSelected(aChan);
  FrameViewChannelsList1.AddChannel(aChan);
end;

procedure TFormDMXChannelsTools.UserHaveRemoved(aChan: TDMXChannel);
begin
  FrameFXChannelChaser1.UserHaveRemoved(aChan);
  FrameViewChannelsList1.RemoveChannel(aChan);
end;

procedure TFormDMXChannelsTools.ClearSelectedChannels;
begin
  FrameFXChannelChaser1.ClearSelectedChannels;
  FrameViewChannelsList1.Clear;
end;

procedure TFormDMXChannelsTools.GetTargetChannels;
begin
  FTargetChannels := FTargetViewProjector.FrameViewDMXCursors1.GetTargetChannels;
end;

procedure TFormDMXChannelsTools.FormCreate(Sender: TObject);
begin
  FrameFXChannelChaser1 := TFrameFXChannelChaser.Create(Self);
  FrameFXChannelChaser1.Parent := Panel1;
  FrameFXChannelChaser1.Align := alClient;

  Frame_Velocity1 := TFrame_Velocity.Create(Self);
  Frame_Velocity1.Name := 'Frame_Velocity1';
  Frame_Velocity1.Parent := Panel3;
  Frame_Velocity1.Align := alClient;

  FrameViewChannelsList1 := TFrameViewChannelsList.Create(Self);
  FrameViewChannelsList1.Parent := Panel2;
  FrameViewChannelsList1.Align := alClient;
  FrameViewChannelsList1.MultiSelect := FALSE;
  FrameViewChannelsList1.OnSelectionChange := @ProcessSourceCopySelectionChange;

  FNoteBookManager := TNoteBookManager.Create(Notebook1);
  FNoteBookManager.SetActivatedColors($0003C4FC, clBlack);
  FNoteBookManager.SetDeactivatedColors($00484848, $00EAEAEA);
  FNoteBookManager.LinkButtonToPage(SpeedButton1, PageDimmer);
  FNoteBookManager.LinkButtonToPage(SpeedButton2, PageFlame);
  FNoteBookManager.LinkButtonToPage(SpeedButton3, PageAudioFollower);
  FNoteBookManager.LinkButtonToPage(SpeedButton4, PageCopy);
  FNoteBookManager.LinkButtonToPage(SpeedButton5, PageStop);
  FNoteBookManager.LinkButtonToPage(SpeedButton8, PageChaser);
  FNoteBookManager.LinkButtonToPage(SpeedButton10, PageFlash);
  FNoteBookManager.OnSelectionChange := @ProcessPageSelectionChange;

  FCheckedLabelManager := TCheckedLabelManager.Create;
  FCheckedLabelManager.CaptureLabelClick(Label28);
  FCheckedLabelManager.CaptureLabelClick(Label29);
  FCheckedLabelManager.CaptureLabelClick(Label22);
  FCheckedLabelManager.CaptureLabelClick(Label30);

  FrameTrackBar1 := TFrameTrackBar.Create(Self);
  FrameTrackBar1.Parent := Panel5;
  FrameTrackBar1.Align := alClient;
  FrameTrackBar1.Init(trHorizontal, False, False, True);
  FrameTrackBar1.OnChange := @RadioButton1Change;

  FFlamePresetManager := TPresetManager.Create(Self);
  FFlamePresetManager.Init1(SFlamePresets, ButtonFlamePreset,
                  ConcatPaths([Project.AppPresetsFolder, 'Flame'+PRESET_FILE_EXTENSION]));
  FFlamePresetManager.Init2(@PresetToFlame, @FlameToPreset);

  FAudioFollowerPresetManager := TPresetManager.Create(Self);
  FAudioFollowerPresetManager.Init1(SAudioFollowerPresets, ButtonAudioFollowerPreset,
                  ConcatPaths([Project.AppPresetsFolder, 'AudioFollower'+PRESET_FILE_EXTENSION]));
  FAudioFollowerPresetManager.Init2(@PresetToAudioFollower, @AudioFollowerToPreset);

  FFirstShown := True;
end;

procedure TFormDMXChannelsTools.BAdd1Click(Sender: TObject);
begin
  GetTargetChannels;
  if length(FTargetChannels) = 0 then exit;

  if Notebook1.PageIndex = Notebook1.IndexOf(PageDimmer) then
    GenerateCmdForDimmer;

  if Notebook1.PageIndex = Notebook1.IndexOf(PageFlame) then
    GenerateCmdForFlame;

  if Notebook1.PageIndex = Notebook1.IndexOf(PageAudioFollower) then
  begin
    if ComboBox1.ItemIndex = -1 then exit;
    GenerateCmdForAudioFollower;
  end;

  if Notebook1.PageIndex = Notebook1.IndexOf(PageFlash) then
    GenerateCmdForFlash;

  if Notebook1.PageIndex = Notebook1.IndexOf(PageCopy) then
  begin
    FSourceChannelForCopy := NIL;
    if FrameViewChannelsList1.SelCount <> 1 then exit;
    if Length(FTargetChannels) < 2 then exit;
    FSourceChannelForCopy := FrameViewChannelsList1.Selected[0];
    GenerateCmdForCopy;
  end;

  if Notebook1.PageIndex = Notebook1.IndexOf(PageStop) then
    GenerateCmdForStopEffect;

  case FGUIMode of
    guiMainDMX:
      begin
        FTargetViewProjector.FrameViewDMXCursors1.SetSourceChannelForCopy(NIL);
        FTargetViewProjector.RegisterCmd(FCmd, FShortReadable, FDuration);
    end;

    guiEditSequence:
      begin
        FormAddDMXAction.FrameViewProjector1.FrameViewDMXCursors1.SetSourceChannelForCopy(NIL);
        FormAddDMXAction.FrameViewProjector1.RegisterCmd(FCmd, FShortReadable, FDuration);
      end;
  end;

//  ModalResult := mrOk;
  Hide;
end;

procedure TFormDMXChannelsTools.FormDestroy(Sender: TObject);
begin
  FNoteBookManager.Free;
  FCheckedLabelManager.Free;
  FFlamePresetManager.Free;
  FAudioFollowerPresetManager.Free;
end;

procedure TFormDMXChannelsTools.FormHide(Sender: TObject);
begin
  SeqPlayer.StopPreview;
end;

procedure TFormDMXChannelsTools.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    ModalResult := mrCancel;
end;

end.

