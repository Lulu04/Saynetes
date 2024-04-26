unit u_edit_singleaction;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, ExtCtrls,
  StdCtrls, Spin, ComCtrls,
  u_common, frame_velocity, frame_color_palette, frame_trackbar,
  u_notebook_util;

type

  { TFormEditSingleAction }

  TFormEditSingleAction = class(TForm)
    BPanNormal: TButton;
    BPanCenterCap: TButton;
    BPitchNormal: TButton;
    CBAudio1: TComboBox;
    CBSequence: TComboBox;
    CBAudio2: TComboBox;
    CBAudio3: TComboBox;
    FloatSpinEdit2: TFloatSpinEdit;
    FloatSpinEdit3: TFloatSpinEdit;
    FloatSpinEdit4: TFloatSpinEdit;
    FloatSpinEdit5: TFloatSpinEdit;
    FSE1: TFloatSpinEdit;
    FSE2: TFloatSpinEdit;
    FSE3: TFloatSpinEdit;
    FSE4: TFloatSpinEdit;
    FSE5: TFloatSpinEdit;
    FSE6: TFloatSpinEdit;
    FSE7: TFloatSpinEdit;
    FSE8: TFloatSpinEdit;
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
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LabelCurve: TLabel;
    LabelCurve1: TLabel;
    LabelCurve2: TLabel;
    LabelCurve3: TLabel;
    LabelPan: TLabel;
    LabelPan1: TLabel;
    LabelPitch: TLabel;
    LabelVolume: TLabel;
    LabelVolume1: TLabel;
    LabelDryWetCap: TLabel;
    lblattendre10: TLabel;
    lblattendre11: TLabel;
    lblattendre12: TLabel;
    lblattendre13: TLabel;
    lblattendre9: TLabel;
    lblVol1: TLabel;
    lblVol2: TLabel;
    lblVol3: TLabel;
    lblVol4: TLabel;
    lblVol5: TLabel;
    lblVol6: TLabel;
    NB: TNotebook;
    BCancel: TSpeedButton;
    BOk: TSpeedButton;
    PageDMXRGB: TPage;
    PageAudioCapture: TPage;
    PageNotEditable: TPage;
    PageDMXChannel: TPage;
    PageAudio: TPage;
    PageSequence: TPage;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    PanelRGBFlash: TPanel;
    PanelRGBFollower: TPanel;
    PanelRGBFlame: TPanel;
    PanelCurveDMXRGB: TPanel;
    PanelPalette: TPanel;
    PanelCurveAudio: TPanel;
    PanelCurveAudioCap: TPanel;
    PanelFlash: TPanel;
    PanelAudioFollower: TPanel;
    PanelFlame: TPanel;
    PanelDimmer: TPanel;
    PanelPan: TPanel;
    PanelPanCap: TPanel;
    PanelPitch: TPanel;
    PanelVolume: TPanel;
    PanelRGBDuration: TPanel;
    PanelVolumeCap: TPanel;
    PanelDryWetCap: TPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    RadioButton6: TRadioButton;
    RadioButton7: TRadioButton;
    RadioButton8: TRadioButton;
    Shape1: TShape;
    SpeedButton5: TSpeedButton;
    TB1: TTrackBar;
    TB10: TTrackBar;
    TB11: TTrackBar;
    TB12: TTrackBar;
    TB13: TTrackBar;
    TB2: TTrackBar;
    TB3: TTrackBar;
    TB4: TTrackBar;
    TB5: TTrackBar;
    TB6: TTrackBar;
    TB7: TTrackBar;
    TB8: TTrackBar;
    TB9: TTrackBar;
    TBPan: TTrackBar;
    TBPanCap: TTrackBar;
    TBPitch: TTrackBar;
    TBVol: TTrackBar;
    TBVolCap: TTrackBar;
    TBDryWetCap: TTrackBar;
    TrackBar1: TTrackBar;
    procedure BOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure FSE8Change(Sender: TObject);
    procedure TB1Change(Sender: TObject);
    procedure TBVolCapChange(Sender: TObject);
    procedure TBVolChange(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    FParams: TParamArray;
    FCmd: integer;
    FInitializing: boolean;
    function GetCmd: TSingleCmd;
    procedure SetCmd(AValue: TSingleCmd);
    procedure AdjustFormHeight(aBottomPanel: TPanel);
  private
    FrameVelocity1, FrameVelocity2, FrameVelocity3, FrameVelocity4, FrameVelocity5: TFrame_Velocity;
    FrameTrackBar1, FrameTrackBar2: TFrameTrackBar;
    FrameColorPalette1: TFrame_ColorPalette;
    CheckedLabelManager: TCheckedLabelManager;
    function GetCmdIsEditable: boolean;
    function GetVolume: single;
    function GetPan: single;
    function GetPitch: single;
    procedure UpdateVolumeLabel;
    procedure UpdatePanLabel;
    procedure UpdatePitchLabel;
    function GetStretchValue: single;
  public

    property Cmd: TSingleCmd read GetCmd write SetCmd;
    property CmdIsEditable: boolean read GetCmdIsEditable;
  end;


implementation
uses LCLType, u_helper, u_list_top, u_audio_manager,
  u_resource_string, u_utils, u_logfile;

{$R *.lfm}

{ TFormEditSingleAction }

procedure TFormEditSingleAction.BOkClick(Sender: TObject);
begin
  if Sender = BCancel then begin
    ModalResult := mrCancel;
    exit;
  end;

  if Sender = BOk then begin

    ModalResult := mrOk;
  end;
end;

procedure TFormEditSingleAction.FormCreate(Sender: TObject);
var i: Integer;
begin
  // sequence
  CBSequence.Clear;
  for i:=0 to Sequences.Count-1 do begin
    CBSequence.Items.Add(Sequences.GetTopByIndex(i).Name);
  end;
  SpeedButton5.Caption := SReset;
  FrameVelocity1 := TFrame_Velocity.Create(Self);
  FrameVelocity1.Name := 'FrameVelocity1';
  FrameVelocity1.Parent := Panel1;
  FrameVelocity1.Align := alClient;
  FrameVelocity1.UpdateList;
  FrameVelocity1.OnChange := @TB1Change;
  Label4.Caption := SSequence;
  SpeedButton5.Caption := SReset;
  Label6.Caption := SDurationInSecond;
  Label25.Caption := SVelocity;

  // audio
  CBAudio1.Clear;
  CBAudio2.Clear;
  CBAudio3.Clear;
  for i:=0 to SoundManager.Count-1 do begin
    CBAudio1.Items.Add(SoundManager.GetSoundFileNameByIndex(i));
    CBAudio2.Items.Add(SoundManager.GetSoundFileNameByIndex(i));
    CBAudio3.Items.Add(SoundManager.GetSoundFileNameByIndex(i));
  end;
  FrameVelocity2 := TFrame_Velocity.Create(Self);
  FrameVelocity2.Name := 'FrameVelocity2';
  FrameVelocity2.Parent := Panel3;
  FrameVelocity2.Align := alClient;
  FrameVelocity2.UpdateList;
  FrameVelocity2.OnChange := @TBVolChange;
  BPanNormal.Caption := SCenter;
  LabelCurve.Caption := SVelocity;
  lblVol1.Caption := SIn;
  lblattendre9.Caption := SSeconds_;
  lblVol3.Caption := SIn;
  lblattendre11.Caption := SSeconds_;
  lblVol4.Caption := SIn;
  lblattendre13.Caption := SSeconds_;

  // capture
  FrameVelocity4 := TFrame_Velocity.Create(Self);
  FrameVelocity4.Name := 'FrameVelocity4';
  FrameVelocity4.Parent := Panel7;
  FrameVelocity4.Align := alClient;
  FrameVelocity4.UpdateList;
  FrameVelocity4.OnChange := @TBVolCapChange;
  BPanCenterCap.Caption := SCenter;
  lblvol2.Caption := SIn;
  lblattendre10.Caption := SSeconds_;
  lblVol5.Caption := SIn;
  lblattendre12.Caption := SSeconds_;
  LabelCurve2.Caption := SVelocity;

  // dmx
  FrameVelocity3 := TFrame_Velocity.Create(Self);
  FrameVelocity3.Name := 'FrameVelocity3';
  FrameVelocity3.Parent := Panel4;
  FrameVelocity3.Align := alClient;
  FrameVelocity3.UpdateList;
  FrameVelocity3.OnChange := @TrackBar1Change;
  // dmx dimmer
  Label7.Caption := SValue;
  LabelCurve1.Caption := SCurve;
  Label8.Caption := SDurationInSecond;
  // dmx flame
  Label12.Caption := SLevelMin;
  Label9.Caption := SLevelMax;
  Label10.Caption := SWaitTime;
  Label16.Caption := SSoften;
  // dmx audio follower
  Label24.Caption := SFollow;
  Label18.Caption := sGain;
  Label20.Caption := SBrightnessMax;
  Label22.Caption := SSoftenOn;
  // dmx flash
  FrameTrackBar1 := TFrameTrackBar.Create(Self);
  FrameTrackBar1.Name := 'FrameTrackBar1';
  FrameTrackBar1.Parent := Panel5;
  FrameTrackBar1.Align := alClient;
  FrameTrackBar1.Init(trHorizontal, False, False, True);
  FrameTrackBar1.OnChange := @TrackBar1Change;
  Label28.Caption := SFixedValue;
  Label29.Caption := SRandomValueBetween;
  Label26.Caption := SFixedDuration;
  Label30.Caption := SRandomDurationBetween;
  Label31.Caption := SSeconds_;

  // dmx rgb
  FrameColorPalette1 := TFrame_ColorPalette.Create(Self);
  FrameColorPalette1.Parent := PanelPalette;
  FrameColorPalette1.Align := alClient;
  FrameColorPalette1.OnChange := @FSE8Change;
  FrameVelocity5 := TFrame_Velocity.Create(Self);
  FrameVelocity5.Name := 'FrameVelocity5';
  FrameVelocity5.Parent := Panel8;
  FrameVelocity5.Align := alClient;
  FrameVelocity5.UpdateList;
  FrameVelocity5.OnChange := @FSE8Change;
  // duration
  lblVol6.Caption := SDurationInSecond;
  LabelCurve3.Caption := SVelocity;
  // flame rgb
  Label32.Caption := SWaitTime;
  Label34.Caption := SAmplitude;
  Label36.Caption := SSoften;
  // follower rgb
  Label38.Caption := SGain;
  Label40.Caption := SSoftenOn;
  // dmx flash rgb
  FrameTrackBar2 := TFrameTrackBar.Create(Self);
  FrameTrackBar2.Name := 'FrameTrackBar2';
  FrameTrackBar2.Parent := Panel10;
  FrameTrackBar2.Align := alClient;
  FrameTrackBar2.Init(trHorizontal, False, False, True);
  FrameTrackBar2.OnChange := @FSE8Change;
  Label45.Caption := SFixedIntensity;
  Label46.Caption := SRandomIntensityBetween;
  Label42.Caption := SFixedDuration;
  Label43.Caption := SRandomDurationBetween;
  Label44.Caption := SSeconds_;

  CheckedLabelManager := TCheckedLabelManager.Create;
  CheckedLabelManager.CaptureLabelClick(Label45);
  CheckedLabelManager.CaptureLabelClick(Label46);
  CheckedLabelManager.CaptureLabelClick(Label42);
  CheckedLabelManager.CaptureLabelClick(Label43);
  CheckedLabelManager.CaptureLabelClick(Label28);
  CheckedLabelManager.CaptureLabelClick(Label29);
  CheckedLabelManager.CaptureLabelClick(Label26);
  CheckedLabelManager.CaptureLabelClick(Label30);
end;

procedure TFormEditSingleAction.FormDestroy(Sender: TObject);
begin
  CheckedLabelManager.Free;
end;

procedure TFormEditSingleAction.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then ModalResult := mrCancel;
end;

procedure TFormEditSingleAction.FormShow(Sender: TObject);
begin
  FrameColorPalette1.InitOnShow;
end;

procedure TFormEditSingleAction.FSE8Change(Sender: TObject);
begin
  // flame
  Label33.Caption := FormatFloat('0.00', TB9.Position/100)+SSec;
  Label35.Caption := FormatFloat('0.0', TB10.Position/TB10.Max*100)+'%';
  Label37.Caption := FormatFloat('0.0', TB11.Position/TB11.Max*100)+'%';
  // follower
  if TB12.Position < 0 then Label39.Caption := TB12.Position.ToString
    else Label39.Caption := '+'+TB12.Position.ToString;
  Label41.Caption := FormatFloat('0.00', TB13.Position/10)+SSec;
  // flash
  if Sender = RadioButton7 then
    FrameTrackBar2.Init(trHorizontal, False, RadioButton8.Checked, True);
  if Sender = FloatSpinEdit4 then
    if FloatSpinEdit5.Value < FloatSpinEdit4.Value then FloatSpinEdit5.Value := FloatSpinEdit4.Value;
  if Sender = FloatSpinEdit5 then
    if FloatSpinEdit4.Value > FloatSpinEdit5.Value then FloatSpinEdit4.Value := FloatSpinEdit5.Value;
  FloatSpinEdit5.Enabled := RadioButton6.Checked;


  if FInitializing then exit;

  case FCmd of
    CMD_DMX_DIMMERRGB: begin // CMD_DMX_DIMMERRGB IDuniverse IDFixture Color Duration CurveID
      FParams[3] := integer(FrameColorPalette1.ShapeColor.ToColor).ToString;
      FParams[4] := FormatFloatWithDot('0.00', FSE8.Value);
      FParams[5] := FrameVelocity5.SelectedCurveID.ToString;
    end;
    TITLECMD_DMX_DIMMERRGB: begin  // TITLECMD_DMX_DIMMERRGB Color Duration CurveID
      FParams[1] := integer(FrameColorPalette1.ShapeColor.ToColor).ToString;
      FParams[2] := FormatFloatWithDot('0.00', FSE8.Value);
      FParams[3] := FrameVelocity5.SelectedCurveID.ToString;
    end;
    CMD_DMX_FLAMERGB: begin // CMD_DMX_FLAMERGB IDuniverse IDFixture Color WaitTime Amplitude Soften
      FParams[3] := integer(FrameColorPalette1.ShapeColor.ToColor).ToString;
      FParams[4] := FormatFloatWithDot('0.00', TB9.Position/100);
      FParams[5] := FormatFloatWithDot('0.00', TB10.Position/TB10.Max);
      FParams[6] := FormatFloatWithDot('0.00', TB11.Position/TB11.Max);
    end;
    TITLECMD_DMX_FLAMERGB: begin // TITLECMD_DMX_FLAMERGB Color WaitTime Amplitude Soften
      FParams[1] := integer(FrameColorPalette1.ShapeColor.ToColor).ToString;
      FParams[2] := FormatFloatWithDot('0.00', TB9.Position/100);
      FParams[3] := FormatFloatWithDot('0.00', TB10.Position/TB10.Max);
      FParams[4] := FormatFloatWithDot('0.00', TB11.Position/TB11.Max);
    end;
    CMD_DMX_AUDIOFOLLOWERRGB: begin // CMD_DMX_AUDIOFOLLOWERRGB IDuniverse IDFixture IDaudio Color Gain SoftenTime
      FParams[3] := SoundManager.GetSoundByIndex(CBAudio3.ItemIndex).Tag.ToString;
      FParams[4] := integer(FrameColorPalette1.ShapeColor.ToColor).ToString;
      FParams[5] := FormatFloatWithDot('0.00', TB12.Position/10);
      FParams[6] := FormatFloatWithDot('0.00', TB13.Position/10);
    end;
    TITLECMD_DMX_AUDIOFOLLOWERRGB: begin // TITLECMD_DMX_AUDIOFOLLOWERRGB IDaudio Color Gain SoftenTime
      FParams[1] := SoundManager.GetSoundByIndex(CBAudio3.ItemIndex).Tag.ToString;
      FParams[2] := integer(FrameColorPalette1.ShapeColor.ToColor).ToString;
      FParams[3] := FormatFloatWithDot('0.00', TB12.Position/10);
      FParams[4] := FormatFloatWithDot('0.00', TB13.Position/10);
    end;
    TITLECMD_DMX_FLASHRGB: begin // TITLECMD_DMX_FLASHRGB Color pcMin pcMax DurationMin DurationMax
      FParams[1] := integer(FrameColorPalette1.ShapeColor.ToColor).ToString;
      FParams[2] := FormatFloatWithDot('0.00', FrameTrackBar2.IntervalMin);
      FParams[3] := FormatFloatWithDot('0.00', FrameTrackBar2.IntervalMax);
      FParams[4] := FormatFloatWithDot('0.00', FloatSpinEdit4.Value);
      if RadioButton5.Checked then FParams[5] := FParams[4]
        else FParams[5] := FormatFloatWithDot('0.00', FloatSpinEdit5.Value);
    end;
    CMD_DMX_FLASHRGB: begin // CMD_DMX_FLASHRGB IDuniverse IDFixture Color pcMin pcMax DurationMin DurationMax
      FParams[3] := integer(FrameColorPalette1.ShapeColor.ToColor).ToString;
      FParams[4] := FormatFloatWithDot('0.00', FrameTrackBar2.IntervalMin);
      FParams[5] := FormatFloatWithDot('0.00', FrameTrackBar2.IntervalMax);
      FParams[6] := FormatFloatWithDot('0.00', FloatSpinEdit4.Value);
      if RadioButton5.Checked then FParams[7] := FParams[6]
        else FParams[7] := FormatFloatWithDot('0.00', FloatSpinEdit5.Value);
    end;
  end;//case
end;

procedure TFormEditSingleAction.TB1Change(Sender: TObject);
begin
  if Sender = SpeedButton5 then TB1.Position := 100;
  Label11.Caption := StrechTimeToString(GetStretchValue);

  if FInitializing then exit;

  if CBSequence.ItemIndex = -1 then exit;
  case FCmd of
    CMD_STARTSEQUENCE, CMD_STOPSEQUENCE: begin  // CMD_STARTSEQUENCE IDseq
      FParams[1] := Sequences.Items[CBSequence.ItemIndex].ID.ToString;
    end;
    CMD_SEQUENCESTRETCHTIME: begin // ATOPSTRETCHTIME IDseq StretchValueF DurationF CurveID
      FParams[1] := Sequences.Items[CBSequence.ItemIndex].ID.ToString;
      FParams[2] := FormatFloatWithDot('0.00', GetStretchValue);
      FParams[3] := FormatFloatWithDot('0.00', FSE4.Value);
      FParams[4] := FrameVelocity1.SelectedCurveID.ToString;
    end;
  end;
end;

procedure TFormEditSingleAction.TBVolCapChange(Sender: TObject);
var vol, pan, drywet: single;
begin
  if Sender = BPanCenterCap then TBPanCap.Position := 0;
  vol := TBVolCap.Position/TBVolCap.Max;
  pan := TBPanCap.Position/100;
  drywet := TBDryWetCap.Position/TBDryWetCap.Max;
  LabelVolume1.Caption := SVolume+' '+VolumeToStringPercent(vol);
  LabelPan1.Caption := SPan+' '+PanToStringPercent(pan);
  LabelDryWetCap.Caption := SDryWet+' '+DryWetToStringPercent(drywet);

  if FInitializing then exit;

  case FCmd of
    CMD_AUDIO_CAPTURE_SETVOLUME: begin  // CMD_AUDIO_CAPTURE_SETVOLUME volume duration IDcurve
      FParams[1] := FormatFloatWithDot('0.00', vol);
      FParams[2] := FormatFloatWithDot('0.00', FSE6.Value);
      FParams[3] := FrameVelocity4.SelectedCurveID.ToString;
    end;
    CMD_AUDIO_CAPTURE_SETPAN: begin  // CMD_AUDIO_CAPTURE_SETPAN panning duration IDcurve
      FParams[1] := FormatFloatWithDot('0.00', pan);
      FParams[2] := FormatFloatWithDot('0.00', FSE7.Value);
      FParams[3] := FrameVelocity4.SelectedCurveID.ToString;
    end;
    TITLECMD_AUDIO_CAPTURE_APPLYFX: begin  // TITLECMD_AUDIO_CAPTURE_APPLYFX dry/wet EffectCount
      FParams[1] := FormatFloatWithDot('0.00', drywet);
    end;
  end;//case
end;

procedure TFormEditSingleAction.TBVolChange(Sender: TObject);
begin
  if Sender = BPanNormal then TBPan.Position := 0;
  if Sender = BPitchNormal then TBPitch.Position := 100;
  UpdateVolumeLabel;
  UpdatePanLabel;
  UpdatePitchLabel;

  if FInitializing then exit;

  case FCmd of
   CMD_AUDIO_PLAY: begin  // AUDIOLECTURE IDaudio volume panning
     if CBAudio1.ItemIndex = -1 then exit;
     FParams[1] := SoundManager.GetSoundByIndex(CBAudio1.ItemIndex).Tag.ToString;
     FParams[2] := FormatFloatWithDot('0.00', GetVolume);
     FParams[3] := FormatFloatWithDot('0.00', GetPan);
   end;
   CMD_AUDIO_STOP, CMD_AUDIO_PAUSE: begin  // AUDIOSTOP IDaudio
     if CBAudio1.ItemIndex = -1 then exit;
     FParams[1] := SoundManager.GetSoundByIndex(CBAudio1.ItemIndex).Tag.ToString;
   end;
   CMD_AUDIO_FADEIN, CMD_AUDIO_SETVOLUME: begin // AUDIOFADEIN IDaudio volume duration IDcurve
     if CBAudio1.ItemIndex = -1 then exit;
     FParams[1] := SoundManager.GetSoundByIndex(CBAudio1.ItemIndex).Tag.ToString;
     FParams[2] := FormatFloatWithDot('0.00', GetVolume);
     FParams[3] := FormatFloatWithDot('0.00', FSE1.Value);
     FParams[4] := FrameVelocity2.SelectedCurveID.ToString;
   end;
   CMD_AUDIO_FADEOUT: begin // AUDIOFADEOUT IDaudio duration IDcurve
     if CBAudio1.ItemIndex = -1 then exit;
     FParams[1] := SoundManager.GetSoundByIndex(CBAudio1.ItemIndex).Tag.ToString;
     FParams[2] := FormatFloatWithDot('0.00', FSE1.Value);
     FParams[3] := FrameVelocity2.SelectedCurveID.ToString;
   end;
   CMD_AUDIO_SETPAN: begin // AUDIOFIXEPAN IDaudio panning duration IDcurve
     if CBAudio1.ItemIndex = -1 then exit;
     FParams[1] := SoundManager.GetSoundByIndex(CBAudio1.ItemIndex).Tag.ToString;
     FParams[2] := FormatFloatWithDot('0.00', GetPan);
     FParams[3] := FormatFloatWithDot('0.00', FSE2.Value);
     FParams[4] := FrameVelocity2.SelectedCurveID.ToString;
   end;
   CMD_AUDIO_SETPITCH: begin // AUDIOFIXEFREQ IDaudio frequence duration IDcurve
     if CBAudio1.ItemIndex = -1 then exit;
     FParams[1] := SoundManager.GetSoundByIndex(CBAudio1.ItemIndex).Tag.ToString;
     FParams[2] := FormatFloatWithDot('0.00', GetPitch);
     FParams[3] := FormatFloatWithDot('0.00', FSE3.Value);
     FParams[4] := FrameVelocity2.SelectedCurveID.ToString;
   end;
   TITLECMD_AUDIO_APPLYFX: begin // TITLECMD_AUDIO_APPLYFX  IDaudio  dry/wet  EffectCount
     if CBAudio1.ItemIndex = -1 then exit;
     FParams[1] := SoundManager.GetSoundByIndex(CBAudio1.ItemIndex).Tag.ToString;
     FParams[2] := FormatFloatWithDot('0.00', GetVolume);
   end;
  end;
end;

procedure TFormEditSingleAction.TrackBar1Change(Sender: TObject);
begin
  // dimmer
  Label7.Caption := SValue+' '+TrackBar1.Position.ToString+' ('+FormatFloat('0.0', TrackBar1.Position*100/255)+'%)';
  // flame
  if Sender = TB2 then
   if TB2.Position <= TB4.Position then
     TB4.Position := TB2.Position-1;
  if Sender = TB4 then
   if TB4.Position >= TB2.Position then
     TB2.Position := TB4.Position+1;
  Label13.Caption := FormatFloat('0.0', TB2.Position/TB2.Max*100)+'%';
  Label15.Caption := FormatFloat('0.0', TB4.Position/TB4.Max*100)+'%';
  Label14.Caption := FormatFloat('0.00', TB3.Position/100)+SSec;
  Label17.Caption := FormatFloat('0.0', TB7.Position/TB7.Max*100)+'%';
  // audio follower
  if TB5.Position < 0 then
    Label19.Caption := TB5.Position.ToString
  else
    Label19.Caption := '+'+TB5.Position.ToString;
  Label21.Caption := TB6.Position.ToString+' ('+FormatFloat('0.0', TB6.Position/TB6.Max*10)+'%)';
  Label23.Caption := FormatFloat('0.00', TB8.Position/10)+SSec;
  // flash
  if Sender = RadioButton1 then
    FrameTrackBar1.Init(trHorizontal, False, RadioButton2.Checked, True);
  if Sender = FloatSpinEdit2 then
    if FloatSpinEdit3.Value < FloatSpinEdit2.Value then FloatSpinEdit3.Value := FloatSpinEdit2.Value;
  if Sender = FloatSpinEdit3 then
    if FloatSpinEdit2.Value > FloatSpinEdit3.Value then FloatSpinEdit2.Value := FloatSpinEdit3.Value;
  FloatSpinEdit3.Enabled := RadioButton4.Checked;

  if FInitializing then exit;

  case FCmd of
   CMD_DMX_DIMMER: begin  // CMD_DMX_DIMMER IDuniverse IDFixture ChanIndex PercentF DurationF CurveID
     FParams[4] := FormatFloatWithDot('0.00', TrackBar1.Position/255);
     FParams[5] := FormatFloatWithDot('0.00', FSE5.Value);
     FParams[6] := FrameVelocity3.SelectedCurveID.ToString;
   end;
   TITLECMD_DMX_DIMMER: begin // TITLECMD_DMX_DIMMER Duration CurveID
     FParams[1] := FormatFloatWithDot('0.00', FSE5.Value);
     FParams[2] := FrameVelocity3.SelectedCurveID.ToString;
   end;
   CMD_DMX_FLAME: begin // CMD_DMX_FLAME IDuniverse IDFixture ChanIndex LevelMin LevelMax Speed Soften
     FParams[4] := FormatFloatWithDot('0.00', TB4.Position/TB4.Max);
     FParams[5] := FormatFloatWithDot('0.00', TB2.Position/TB2.Max);
     FParams[6] := FormatFloatWithDot('0.00', TB3.Position/100);
     FParams[7] := FormatFloatWithDot('0.00', TB4.Position/TB7.Max);
   end;
   TITLECMD_DMX_FLAME: begin  // TITLECMD_DMX_FLAME  LevelMin LevelMax Speed Soften
     FParams[1] := FormatFloatWithDot('0.00', TB4.Position/TB4.Max);
     FParams[2] := FormatFloatWithDot('0.00', TB2.Position/TB2.Max);
     FParams[3] := FormatFloatWithDot('0.00', TB3.Position/100);
     FParams[4] := FormatFloatWithDot('0.00', TB4.Position/TB7.Max);
   end;
   CMD_DMX_AUDIOFOLLOWER: begin // CMD_DMX_AUDIOFOLLOWER IDuniverse IDFixture ChanIndex IDaudio Gain MaxPercent SoftenTime
     FParams[4] := SoundManager.IndexToID(CBAudio2.ItemIndex).ToString;
     FParams[5] := FormatFloatWithDot('0.00', TB5.Position/10);
     FParams[6] := FormatFloatWithDot('0.00', TB6.Position/TB6.Max);
     FParams[7] := FormatFloatWithDot('0.00', TB8.Position/10);
   end;
   TITLECMD_DMX_AUDIOFOLLOWER: begin // TITLECMD_DMX_AUDIOFOLLOWER IDaudio Gain MaxPercent SoftenTime
     FParams[1] := SoundManager.IndexToID(CBAudio2.ItemIndex).ToString;
     FParams[2] := FormatFloatWithDot('0.00', TB5.Position/10);
     FParams[3] := FormatFloatWithDot('0.00', TB6.Position/TB6.Max);
     FParams[4] := FormatFloatWithDot('0.00', TB8.Position/10);
   end;
   CMD_DMX_FLASH: begin // CMD_DMX_FLASH IDuniverse IDFixture ChanIndex LevelMin LevelMax DurationMin DurationMax
     FParams[4] := FormatFloatWithDot('0.00', FrameTrackBar1.IntervalMin);
     FParams[5] := FormatFloatWithDot('0.00', FrameTrackBar1.IntervalMax);
     FParams[6] := FormatFloatWithDot('0.00', FloatSpinEdit2.Value);
     if RadioButton3.Checked then FParams[7] := FParams[6]
       else FParams[7] := FormatFloatWithDot('0.00', FloatSpinEdit3.Value);
   end;
   TITLECMD_DMX_FLASH: begin // TITLECMD_DMX_FLASH LevelMin LevelMax DurationMin DurationMax
     FParams[1] := FormatFloatWithDot('0.00', FrameTrackBar1.IntervalMin);
     FParams[2] := FormatFloatWithDot('0.00', FrameTrackBar1.IntervalMax);
     FParams[3] := FormatFloatWithDot('0.00', FloatSpinEdit2.Value);
     if RadioButton3.Checked then FParams[4] := FParams[3]
       else FParams[4] := FormatFloatWithDot('0.00', FloatSpinEdit3.Value);
   end;
  end;
end;

function TFormEditSingleAction.GetCmd: TSingleCmd;
begin
  Result := FParams.PackToCmd;
end;

procedure TFormEditSingleAction.SetCmd(AValue: TSingleCmd);
begin
  FParams := AValue.SplitToParamArray;
  if (Length(FParams)= 0) or
     not tryStrToInt(FParams[0], FCmd) then begin
     exit;
  end;

  FInitializing := True;

  try
    case FCmd of
     CMD_STARTSEQUENCE: begin  // CMD_STARTSEQUENCE IDseq
       NB.PageIndex := NB.IndexOf(PageSequence);
       Label2.Caption := SStartSequence;
       CBSequence.ItemIndex := Sequences.IDToIndex(FParams[1].ToInteger);
       AdjustFormHeight(NIL);
     end;

     CMD_STOPSEQUENCE: begin  // CMD_STOPSEQUENCE IDseq
       NB.PageIndex := NB.IndexOf(PageSequence);
       Label2.Caption := SStopSequence;
       CBSequence.ItemIndex := Sequences.IDToIndex(FParams[1].ToInteger);
       AdjustFormHeight(NIL);
     end;

     CMD_SEQUENCESTRETCHTIME: begin // ATOPSTRETCHTIME IDseq StretchValueF DurationF CurveID
       NB.PageIndex := NB.IndexOf(PageSequence);
       Label2.Caption := SStretchTime;
       CBSequence.ItemIndex := Sequences.IDToIndex(FParams[1].ToInteger);
       Panel2.Visible := True;
       AdjustFormHeight(Panel2);
       TB1.Position := Round(StringToSingle(FParams[2])*100);
       FSE4.Value := StringToSingle(FParams[3]);
       FrameVelocity1.SelectedCurveID := FParams[4].ToInteger;
       TB1Change(NIL);
     end;

     CMD_AUDIO_PLAY: begin  // AUDIOLECTURE IDaudio volume panning
       PanelVolume.Visible := True;
       lblVol1.Visible := False;
       FSE1.Visible := False;
       lblattendre9.Visible := False;
       lblVol3.Visible := False;
       FSE2.Visible := False;
       lblattendre11.Visible := False;
       PanelPan.Visible := True;
       AdjustFormHeight(PanelPan);
       NB.PageIndex := NB.IndexOf(PageAudio);
       Label2.Caption := SAudioPlay;
       CBAudio1.ItemIndex := SoundManager.IDToIndex(FParams[1].ToInteger);
       TBVol.Position := Round(StringToSingle(FParams[2])*TBVol.Max);
       TBPan.Position := Round(StringToSingle(FParams[3])*TBPan.Max);
       TBVolChange(NIL);
     end;

     CMD_AUDIO_STOP: begin  // AUDIOSTOP IDaudio
       NB.PageIndex := NB.IndexOf(PageAudio);
       Label2.Caption := SAudioStop;
       CBAudio1.ItemIndex := SoundManager.IDToIndex(FParams[1].ToInteger);
       AdjustFormHeight(NIL);
     end;

     CMD_AUDIO_PAUSE: begin // AUDIOPAUSE IDaudio
       NB.PageIndex := NB.IndexOf(PageAudio);
       Label2.Caption := SAudioPause;
       CBAudio1.ItemIndex := SoundManager.IDToIndex(FParams[1].ToInteger);
       AdjustFormHeight(NIL);
     end;

     CMD_AUDIO_FADEIN: begin // AUDIOFADEIN IDaudio volume duration IDcurve
       PanelVolume.Visible := True;
       PanelVolume.Left := ScaleDesignToForm(8);
       PanelVolume.Top := ScaleDesignToForm(40);
       PanelCurveAudio.Visible := True;
       PanelCurveAudio.Top := PanelVolume.Top+PanelVolume.Height+ScaleDesignToForm(5);
       AdjustFormHeight(PanelCurveAudio);
       NB.PageIndex := NB.IndexOf(PageAudio);
       Label2.Caption := SAudioFadeIn;
       CBAudio1.ItemIndex := SoundManager.IDToIndex(FParams[1].ToInteger);
       TBVol.Position := Round(StringToSingle(FParams[2])*TBVol.Max);
       FSE1.Value := StringToSingle(FParams[3]);
       FrameVelocity2.SelectedCurveID := FParams[4].ToInteger;
       TBVolChange(NIL);
     end;

     CMD_AUDIO_FADEOUT: begin // AUDIOFADEOUT IDaudio duration IDcurve
       PanelVolume.Visible := True;
       PanelVolume.Left := ScaleDesignToForm(8);
       PanelVolume.Top := ScaleDesignToForm(40);
       PanelCurveAudio.Visible := True;
       PanelCurveAudio.Top := PanelVolume.Top+PanelVolume.Height+ScaleDesignToForm(5);
       AdjustFormHeight(PanelCurveAudio);
       LabelVolume.Visible := False;
       TBVol.Visible := False;
       NB.PageIndex := NB.IndexOf(PageAudio);
       Label2.Caption := SAudioFadeOut;
       CBAudio1.ItemIndex := SoundManager.IDToIndex(FParams[1].ToInteger);
       FSE1.Value := StringToSingle(FParams[2]);
       FrameVelocity2.SelectedCurveID := FParams[3].ToInteger;
     end;

     CMD_AUDIO_SETVOLUME: begin // AUDIOFIXEVOLUME IDaudio volume duration IDcurve
       PanelVolume.Visible := True;
       PanelVolume.Left := ScaleDesignToForm(8);
       PanelVolume.Top := ScaleDesignToForm(40);
       PanelCurveAudio.Visible := True;
       PanelCurveAudio.Top := PanelVolume.Top+PanelVolume.Height+ScaleDesignToForm(5);
       AdjustFormHeight(PanelCurveAudio);
       NB.PageIndex := NB.IndexOf(PageAudio);
       Label2.Caption := SAudioSetVolume;
       CBAudio1.ItemIndex := SoundManager.IDToIndex(FParams[1].ToInteger);
       TBVol.Position := Round(StringToSingle(FParams[2])*TBVol.Max);
       FSE1.Value := StringToSingle(FParams[3]);
       FrameVelocity2.SelectedCurveID := FParams[4].ToInteger;
       TBVolChange(NIL);
     end;

     CMD_AUDIO_SETPAN: begin // AUDIOFIXEPAN IDaudio panning duration IDcurve
       PanelPan.Visible := True;
       PanelPan.Left := ScaleDesignToForm(8);
       PanelPan.Top := ScaleDesignToForm(40);
       PanelCurveAudio.Visible := True;
       PanelCurveAudio.Top := PanelPan.Top+PanelPan.Height+ScaleDesignToForm(5);
       AdjustFormHeight(PanelCurveAudio);
       NB.PageIndex := NB.IndexOf(PageAudio);
       Label2.Caption := SAudioSetPan;
       CBAudio1.ItemIndex := SoundManager.IDToIndex(FParams[1].ToInteger);
       TBPan.Position := Round(StringToSingle(FParams[2])*100);
       FSE2.Value := StringToSingle(FParams[3]);
       FrameVelocity2.SelectedCurveID := FParams[4].ToInteger;
       TBVolChange(NIL);
     end;

     CMD_AUDIO_SETPITCH: begin // AUDIOFIXEFREQ IDaudio frequence duration IDcurve
       PanelPitch.Visible := True;
       PanelPitch.Left := ScaleDesignToForm(8);
       PanelPitch.Top := ScaleDesignToForm(40);
       PanelCurveAudio.Visible := True;
       PanelCurveAudio.Top := PanelPitch.Top+PanelPitch.Height+ScaleDesignToForm(5);
       AdjustFormHeight(PanelCurveAudio);
       NB.PageIndex := NB.IndexOf(PageAudio);
       Label2.Caption := SAudioSetFreq;
       CBAudio1.ItemIndex := SoundManager.IDToIndex(FParams[1].ToInteger);
       TBPitch.Position := Round(StringToSingle(FParams[2])*100);
       FSE3.Value := StringToSingle(FParams[3]);
       FrameVelocity2.SelectedCurveID := FParams[4].ToInteger;
       TBVolChange(NIL);
     end;

     TITLECMD_AUDIO_APPLYFX: begin // TITLECMD_AUDIO_APPLYFX  IDaudio  dry/wet  EffectCount
       NB.PageIndex := NB.IndexOf(PageAudio);
       Label2.Caption := SAudioConnectEffect;
       CBAudio1.ItemIndex := SoundManager.IDToIndex(FParams[1].ToInteger);
       PanelVolume.Visible := True;
       lblvol1.Visible := False;
       FSE1.Visible := False;
       lblattendre9.Visible := False;
       AdjustFormHeight(PanelVolume);
       TBVol.Max := 200;
       TBVol.Position := Round(StringToSingle(FParams[2])*TBVol.Max);
       TBVolChange(NIL);
     end;

     CMD_AUDIO_FXPRESET: begin // CMD_AUDIO_FXPRESET  effectType  presetIndex
       NB.PageIndex := NB.IndexOf(PageAudio);
       Label2.Caption := SAudioConnectEffect;
       // to do
     end;

     CMD_AUDIO_CAPTURE_SETVOLUME: begin // CMD_AUDIO_CAPTURE_SETVOLUME volume duration IDcurve
       NB.PageIndex := NB.IndexOf(PageAudioCapture);
       Label2.Caption := SAudioCaptureSetVolume;
       PanelVolumeCap.Visible := True;
       PanelCurveAudioCap.Visible := True;
       AdjustFormHeight(PanelCurveAudioCap);
       TBVolCap.Position := Round(StringToSingle(FParams[1])*TBVolCap.Max);
       FSE6.Value := StringToSingle(FParams[2]);
       FrameVelocity4.SelectedCurveID := FParams[3].ToInteger;
       TBVolCapChange(NIL);
     end;

     CMD_AUDIO_CAPTURE_SETPAN: begin // CMD_AUDIO_CAPTURE_SETPAN pan duration IDcurve
       NB.PageIndex := NB.IndexOf(PageAudioCapture);
       Label2.Caption := SAudioCaptureSetPan;
       PanelPanCap.Visible := True;
       PanelPanCap.Left := ScaleDesignToForm(8);
       PanelCurveAudioCap.Visible := True;
       AdjustFormHeight(PanelCurveAudioCap);
       TBPanCap.Position := Round(StringToSingle(FParams[1])*100);
       FSE7.Value := StringToSingle(FParams[2]);
       FrameVelocity4.SelectedCurveID := FParams[3].ToInteger;
       TBVolCapChange(NIL);
     end;

     TITLECMD_AUDIO_CAPTURE_APPLYFX: begin  // TITLECMD_AUDIO_CAPTURE_APPLYFX dry/wet EffectCount
       NB.PageIndex := NB.IndexOf(PageAudioCapture);
       Label2.Caption := SAudioCaptureConnectEffect;
       PanelDryWetCap.Visible := True;
       PanelDryWetCap.Left := ScaleDesignToForm(8);
       PanelDryWetCap.Top :=  ScaleDesignToForm(40);
       AdjustFormHeight(PanelDryWetCap);
       TBDryWetCap.Position := Round(StringToSingle(FParams[1])*TBDryWetCap.Max);
       TBVolCapChange(NIL);
     end;

     // dmx stuff
     CMD_DMX_DIMMER: begin  // CMD_DMX_DIMMER IDuniverse IDFixture ChanIndex PercentF DurationF CurveID
       PanelDimmer.Visible := True;
       PanelDimmer.Left := ScaleDesignToForm(8);
       PanelDimmer.Top := ScaleDesignToForm(8);
       AdjustFormHeight(PanelDimmer);
       NB.PageIndex := NB.IndexOf(PageDMXChannel);
       Label2.Caption := SDMXDimmer;
       TrackBar1.Position := Round(StringToSingle(FParams[4])*255);
       FSE5.Value := StringToSingle(FParams[5]);
       FrameVelocity3.SelectedCurveID := FParams[6].ToInteger;
       TrackBar1Change(NIL);
       PanelFlame.Visible := False;
     end;
     TITLECMD_DMX_DIMMER: begin  // TITLECMD_DMX_DIMMER Duration CurveID
       PanelDimmer.Visible := True;
       PanelDimmer.Left := ScaleDesignToForm(8);
       PanelDimmer.Top := ScaleDesignToForm(8);
       AdjustFormHeight(PanelDimmer);
       Label7.Visible := False;
       TrackBar1.Visible := False;
       Label8.Top := (PanelDimmer.Height - Label8.Height) div 2;
       NB.PageIndex := NB.IndexOf(PageDMXChannel);
       Label2.Caption := SDMXDimmer;
       FSE5.Value := StringToSingle(FParams[1]);
       FrameVelocity3.SelectedCurveID := FParams[2].ToInteger;
       TrackBar1Change(NIL);
     end;

     CMD_DMX_FLAME: begin  // CMD_DMX_FLAME IDuniverse IDFixture ChanIndex LevelMin LevelMax Speed Soften
       PanelFlame.Visible := True;
       PanelFlame.Left := ScaleDesignToForm(8);
       PanelFlame.Top := ScaleDesignToForm(8);
       AdjustFormHeight(PanelFlame);
       NB.PageIndex := NB.IndexOf(PageDMXChannel);
       Label2.Caption := SDMXFlame;
       TB4.Position := Round(StringToSingle(FParams[4])*TB4.Max);
       TB2.Position := Round(StringToSingle(FParams[5])*TB2.Max);
       TB3.Position := Round(StringToSingle(FParams[6])*100);
       TB7.Position := Round(StringToSingle(FParams[7])*TB7.Max);
       TrackBar1Change(NIL);
     end;
     TITLECMD_DMX_FLAME: begin  // TITLECMD_DMX_FLAME  LevelMin LevelMax Speed Soften
       PanelFlame.Visible := True;
       PanelFlame.Left := ScaleDesignToForm(8);
       PanelFlame.Top := ScaleDesignToForm(8);
       AdjustFormHeight(PanelFlame);
       NB.PageIndex := NB.IndexOf(PageDMXChannel);
       Label2.Caption := SDMXFlame;
       TB4.Position := Round(StringToSingle(FParams[1])*TB4.Max);
       TB2.Position := Round(StringToSingle(FParams[2])*TB2.Max);
       TB3.Position := Round(StringToSingle(FParams[3])*100);
       TB7.Position := Round(StringToSingle(FParams[4])*TB7.Max);
       TrackBar1Change(NIL);
     end;
     CMD_DMX_AUDIOFOLLOWER: begin // CMD_DMX_AUDIOFOLLOWER IDuniverse IDFixture ChanIndex IDaudio Gain MaxPercent SoftenTime
       PanelAudioFollower.Visible := True;
       PanelAudioFollower.Top := ScaleDesignToForm(8);
       PanelAudioFollower.Left := ScaleDesignToForm(8);
       AdjustFormHeight(PanelAudioFollower);
       NB.PageIndex := NB.IndexOf(PageDMXChannel);
       Label2.Caption := SDMXAudioFollower;
       CBAudio2.ItemIndex := SoundManager.IDToIndex(FParams[4].ToInteger);
       TB5.Position := Round(StringToSingle(FParams[5])*10);
       TB6.Position := Round(StringToSingle(FParams[6])*TB6.Max);
       TB8.Position := Round(StringToSingle(FParams[7])*10);
       TrackBar1Change(NIL);
     end;
     TITLECMD_DMX_AUDIOFOLLOWER: begin // TITLECMD_DMX_AUDIOFOLLOWER IDaudio Gain MaxPercent SoftenTime
       PanelAudioFollower.Visible := True;
       PanelAudioFollower.Top := ScaleDesignToForm(8);
       PanelAudioFollower.Left := ScaleDesignToForm(8);
       AdjustFormHeight(PanelAudioFollower);
       NB.PageIndex := NB.IndexOf(PageDMXChannel);
       Label2.Caption := SDMXAudioFollower;
       CBAudio2.ItemIndex := SoundManager.IDToIndex(FParams[1].ToInteger);
       TB5.Position := Round(StringToSingle(FParams[2])*10);
       TB6.Position := Round(StringToSingle(FParams[3])*TB6.Max);
       TB8.Position := Round(StringToSingle(FParams[4])*10);
       TrackBar1Change(NIL);
     end;
     CMD_DMX_FLASH: begin // CMD_DMX_FLASH IDuniverse IDFixture ChanIndex LevelMin LevelMax DurationMin DurationMax
       PanelFlash.Visible := True;
       PanelFlash.Top := ScaleDesignToForm(8);
       PanelFlash.Left := ScaleDesignToForm(8);
       AdjustFormHeight(PanelFlash);
       NB.PageIndex := NB.IndexOf(PageDMXChannel);
       Label2.Caption := SDMXFlash;
       if FParams[4] = FParams[5] then RadioButton1.Checked := True else RadioButton2.Checked := True;
       if FParams[6] = FParams[7] then RadioButton3.Checked := True else RadioButton4.Checked := True;
       FrameTrackBar1.Init(trHorizontal, False, RadioButton2.Checked, True);
       FrameTrackBar1.IntervalMin := StringToSingle(FParams[4]);
       FrameTrackBar1.IntervalMax := StringToSingle(FParams[5]);
       FloatSpinEdit2.Value := StringToSingle(FParams[6]);
       FloatSpinEdit3.Value := StringToSingle(FParams[7]);
     end;
     TITLECMD_DMX_FLASH: begin // TITLECMD_DMX_FLASH LevelMin LevelMax DurationMin DurationMax
       PanelFlash.Visible := True;
       PanelFlash.Top := ScaleDesignToForm(8);
       PanelFlash.Left := ScaleDesignToForm(8);
       AdjustFormHeight(PanelFlash);
       NB.PageIndex := NB.IndexOf(PageDMXChannel);
       Label2.Caption := SDMXFlash;
       if FParams[1] = FParams[2] then RadioButton1.Checked := True else RadioButton2.Checked := True;
       if FParams[3] = FParams[4] then RadioButton3.Checked := True else RadioButton4.Checked := True;
       FrameTrackBar1.Init(trHorizontal, False, RadioButton2.Checked, True);
       FrameTrackBar1.IntervalMin := StringToSingle(FParams[1]);
       FrameTrackBar1.IntervalMax := StringToSingle(FParams[2]);
       FloatSpinEdit2.Value := StringToSingle(FParams[3]);
       FloatSpinEdit3.Value := StringToSingle(FParams[4]);
     end;

     CMD_DMX_DIMMERRGB: begin  // CMD_DMX_DIMMERRGB IDuniverse IDFixture Color Duration CurveID
       PanelPalette.Visible := True;
       PanelRGBDuration.Visible := True;
       PanelRGBDuration.Left := ScaleDesignToForm(8);
       PanelRGBDuration.Top := PanelPalette.Top+PanelPalette.Height+ScaleDesignToForm(8);
       PanelCurveDMXRGB.Visible := True;
       PanelCurveDMXRGB.Left := ScaleDesignToForm(8);
       PanelCurveDMXRGB.Top := PanelRGBDuration.Top+PanelRGBDuration.Height+ScaleDesignToForm(8);
       AdjustFormHeight(PanelCurveDMXRGB);
       NB.PageIndex := NB.IndexOf(PageDMXRGB);
       Label2.Caption := SDMXDimmerRGB;
       FrameColorPalette1.SelectedColor := TColor(FParams[3].ToInteger);
       FSE8.Value := StringToSingle(FParams[4]);
       FrameVelocity5.SelectedCurveID := FParams[5].ToInteger;
     end;
     TITLECMD_DMX_DIMMERRGB: begin  // TITLECMD_DMX_DIMMERRGB Color Duration CurveID
       PanelPalette.Visible := True;
       PanelRGBDuration.Visible := True;
       PanelRGBDuration.Left := ScaleDesignToForm(8);
       PanelRGBDuration.Top := PanelPalette.Top+PanelPalette.Height+ScaleDesignToForm(8);
       PanelCurveDMXRGB.Visible := True;
       PanelCurveDMXRGB.Left := ScaleDesignToForm(8);
       PanelCurveDMXRGB.Top := PanelRGBDuration.Top+PanelRGBDuration.Height+ScaleDesignToForm(8);
       AdjustFormHeight(PanelCurveDMXRGB);
       NB.PageIndex := NB.IndexOf(PageDMXRGB);
       Label2.Caption := SDMXDimmerRGB;
       FrameColorPalette1.SelectedColor := TColor(FParams[1].ToInteger);
       FSE8.Value := StringToSingle(FParams[2]);
       FrameVelocity5.SelectedCurveID := FParams[3].ToInteger;
     end;
     CMD_DMX_FLAMERGB: begin // CMD_DMX_FLAMERGB IDuniverse IDFixture Color Speed Amplitude Soften
       PanelPalette.Visible := True;
       PanelRGBFlame.Visible := True;
       PanelRGBFlame.Left := ScaleDesignToForm(8);
       PanelRGBFlame.Top := PanelPalette.Top+PanelPalette.Height+ScaleDesignToForm(8);
       AdjustFormHeight(PanelRGBFlame);
       NB.PageIndex := NB.IndexOf(PageDMXRGB);
       Label2.Caption := SDMXFlameRGB;
       FrameColorPalette1.SelectedColor := TColor(FParams[3].ToInteger);
       TB9.Position := Round(StringToSingle(FParams[4])*100);
       TB10.Position := Round(StringToSingle(FParams[5])*TB10.Max);
       TB11.Position := Round(StringToSingle(FParams[6])*TB11.Max);
       FSE8Change(NIL);
     end;
     TITLECMD_DMX_FLAMERGB: begin // TITLECMD_DMX_FLAMERGB Color Speed Amplitude Soften
       PanelPalette.Visible := True;
       PanelRGBFlame.Visible := True;
       PanelRGBFlame.Left := ScaleDesignToForm(8);
       PanelRGBFlame.Top := PanelPalette.Top+PanelPalette.Height+ScaleDesignToForm(8);
       AdjustFormHeight(PanelRGBFlame);
       NB.PageIndex := NB.IndexOf(PageDMXRGB);
       Label2.Caption := SDMXFlameRGB;
       FrameColorPalette1.SelectedColor := TColor(FParams[1].ToInteger);
       TB9.Position := Round(StringToSingle(FParams[2])*100);
       TB10.Position := Round(StringToSingle(FParams[3])*TB10.Max);
       TB11.Position := Round(StringToSingle(FParams[4])*TB11.Max);
       FSE8Change(NIL);
     end;
     CMD_DMX_AUDIOFOLLOWERRGB: begin // CMD_DMX_AUDIOFOLLOWERRGB IDuniverse IDFixture IDaudio Color Gain SoftenTime
       PanelPalette.Visible := True;
       PanelRGBFollower.Visible := True;
       PanelRGBFollower.Left := ScaleDesignToForm(8);
       PanelRGBFollower.Top := PanelPalette.Top+PanelPalette.Height+ScaleDesignToForm(8);
       AdjustFormHeight(PanelRGBFollower);
       NB.PageIndex := NB.IndexOf(PageDMXRGB);
       Label2.Caption := SDMXAudioFollowerRGB;
       CBAudio3.ItemIndex := SoundManager.IDToIndex(FParams[3].ToInteger);
       FrameColorPalette1.SelectedColor := TColor(FParams[4].ToInteger);
       TB12.Position := Round(StringToSingle(FParams[5])*10);
       TB13.Position := Round(StringToSingle(FParams[6])*10);
       FSE8Change(NIL);
     end;
     TITLECMD_DMX_AUDIOFOLLOWERRGB: begin // TITLECMD_DMX_AUDIOFOLLOWERRGB IDaudio Color Gain SoftenTime
       PanelPalette.Visible := True;
       PanelRGBFollower.Visible := True;
       PanelRGBFollower.Left := ScaleDesignToForm(8);
       PanelRGBFollower.Top := PanelPalette.Top+PanelPalette.Height+ScaleDesignToForm(8);
       AdjustFormHeight(PanelRGBFollower);
       NB.PageIndex := NB.IndexOf(PageDMXRGB);
       Label2.Caption := SDMXAudioFollowerRGB;
       CBAudio3.ItemIndex := SoundManager.IDToIndex(FParams[1].ToInteger);
       FrameColorPalette1.SelectedColor := TColor(FParams[2].ToInteger);
       TB12.Position := Round(StringToSingle(FParams[3])*10);
       TB13.Position := Round(StringToSingle(FParams[4])*10);
       FSE8Change(NIL);
     end;
     TITLECMD_DMX_FLASHRGB: begin // TITLECMD_DMX_FLASHRGB Color pcMin pcMax DurationMin DurationMax
       PanelPalette.Visible := True;
       PanelRGBFlash.Visible := True;
       PanelRGBFlash.Left := ScaleDesignToForm(8);
       PanelRGBFlash.Top := PanelPalette.Top+PanelPalette.Height+ScaleDesignToForm(8);
       AdjustFormHeight(PanelRGBFlash);
       NB.PageIndex := NB.IndexOf(PageDMXRGB);
       Label2.Caption := SDMXFlashRGB;
       FrameColorPalette1.SelectedColor := TColor(FParams[1].ToInteger);
       if FParams[2] = FParams[3] then RadioButton7.Checked := True else RadioButton8.Checked := True;
       if FParams[4] = FParams[5] then RadioButton5.Checked := True else RadioButton6.Checked := True;
       FrameTrackBar2.Init(trHorizontal, False, RadioButton8.Checked, True);
       FrameTrackBar2.IntervalMin := StringToSingle(FParams[2]);
       FrameTrackBar2.IntervalMax := StringToSingle(FParams[3]);
       FloatSpinEdit4.Value := StringToSingle(FParams[4]);
       FloatSpinEdit5.Value := StringToSingle(FParams[5]);
       FSE8Change(NIL);
     end;
     CMD_DMX_FLASHRGB: begin // CMD_DMX_FLASHRGB IDuniverse IDFixture Color pcMin pcMax DurationMin DurationMax
       PanelPalette.Visible := True;
       PanelRGBFlash.Visible := True;
       PanelRGBFlash.Left := ScaleDesignToForm(8);
       PanelRGBFlash.Top := PanelPalette.Top+PanelPalette.Height+ScaleDesignToForm(8);
       AdjustFormHeight(PanelRGBFlash);
       NB.PageIndex := NB.IndexOf(PageDMXRGB);
       Label2.Caption := SDMXFlashRGB;
       FrameColorPalette1.SelectedColor := TColor(FParams[3].ToInteger);
       if FParams[4] = FParams[5] then RadioButton7.Checked := True else RadioButton8.Checked := True;
       if FParams[6] = FParams[7] then RadioButton5.Checked := True else RadioButton6.Checked := True;
       FrameTrackBar2.Init(trHorizontal, False, RadioButton8.Checked, True);
       FrameTrackBar2.IntervalMin := StringToSingle(FParams[4]);
       FrameTrackBar2.IntervalMax := StringToSingle(FParams[5]);
       FloatSpinEdit4.Value := StringToSingle(FParams[6]);
       FloatSpinEdit5.Value := StringToSingle(FParams[7]);
       FSE8Change(NIL);
     end;

     else begin
       NB.PageIndex := NB.IndexOf(PageNotEditable);
       Label1.Visible := False;
       Label2.Visible := False;
     end;
    end;//case
  except
    ModalResult := mrCancel;
    exit;
  end;
  FInitializing := False;
end;

procedure TFormEditSingleAction.AdjustFormHeight(aBottomPanel: TPanel);
var p: TPoint;
begin
  if aBottomPanel <> NIL then begin
    p.x := 0;
    p.y := aBottomPanel.Height;
    p := aBottomPanel.ClientToParent(p, Self);
    NB.Height := p.y;
    p.y := p.y+ScaleDesignToForm(10)+BOk.Height+ScaleDesignToForm(5);
  end else p.y := ScaleDesignToForm(110);
  ClientHeight := p.y;
end;

function TFormEditSingleAction.GetVolume: single;
begin
  Result := TBVol.Position/TBVol.Max;
end;

function TFormEditSingleAction.GetCmdIsEditable: boolean;
begin
  Result := (FCmd <> CMD_AUDIO_REMOVEFX) and
            (FCmd <> CMD_AUDIO_CAPTURE_START) and
            (FCmd <> CMD_AUDIO_CAPTURE_STOP) and
            (FCmd <> CMD_AUDIO_CAPTURE_REMOVEFX) and
            (FCmd <> CMD_INTERNALDMXWAVE) and
            (FCmd <> CMD_DMX_STOPEFFECT) and
            (FCmd <> TITLECMD_DMX_STOPEFFECT) and
            (FCmd <> CMD_DMX_COPYCHANNEL) and
            (FCmd <> TITLECMD_DMX_COPYCHANNEL) and
            (FCmd <> CMD_DMX_STOPEFFECTRGB) and
            (FCmd <> TITLECMD_DMX_STOPEFFECTRGB) and
            (FCmd <> TITLECMD_DMX_COPYRGB) and
            (FCmd <> CMD_DMX_COPYRGB);
end;

function TFormEditSingleAction.GetPan: single;
begin
  Result := TBPan.Position/100;
end;

function TFormEditSingleAction.GetPitch: single;
begin
  Result := TBPitch.Position/100;
end;

procedure TFormEditSingleAction.UpdateVolumeLabel;
begin
  if FCmd = TITLECMD_AUDIO_APPLYFX then LabelVolume.Caption := SDryWet+'  '+DryWetToStringPercent(GetVolume)
    else LabelVolume.Caption := SVolume+' '+VolumeToStringPercent(GetVolume);
end;

procedure TFormEditSingleAction.UpdatePanLabel;
begin
  LabelPan.Caption := SPan+' '+PanToStringPercent(GetPan);
end;

procedure TFormEditSingleAction.UpdatePitchLabel;
begin
  LabelPitch.Caption := SPitch+' '+PitchToString( GetPitch );
end;

function TFormEditSingleAction.GetStretchValue: single;
begin
  Result := TB1.Position/100;
end;

end.

