unit u_edit_singleaction;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, ExtCtrls,
  StdCtrls, Spin, ComCtrls,
  u_common, frame_velocity, frame_color_palette, frame_trackbar, frame_trackbar_customized,
  lcl_utils, frame_viewfixtureslist;

type

  { TFormEditSingleAction }

  TFormEditSingleAction = class(TForm)
    BPanCenterCap: TSpeedButton;
    BPanNormal: TSpeedButton;
    BPitchNormal: TSpeedButton;
    CBAudio1: TComboBox;
    CBSequence: TComboBox;
    CBAudio2: TComboBox;
    CBAudio3: TComboBox;
    CBUni1: TComboBox;
    CBUni2: TComboBox;
    FloatSpinEdit2: TFloatSpinEdit;
    FloatSpinEdit3: TFloatSpinEdit;
    FloatSpinEdit4: TFloatSpinEdit;
    FloatSpinEdit5: TFloatSpinEdit;
    FloatSpinEdit6: TFloatSpinEdit;
    FloatSpinEdit7: TFloatSpinEdit;
    FloatSpinEdit8: TFloatSpinEdit;
    FloatSpinEdit9: TFloatSpinEdit;
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
    Label47: TLabel;
    Label48: TLabel;
    Label49: TLabel;
    Label5: TLabel;
    Label50: TLabel;
    Label51: TLabel;
    Label52: TLabel;
    Label53: TLabel;
    Label54: TLabel;
    Label55: TLabel;
    Label56: TLabel;
    Label57: TLabel;
    Label58: TLabel;
    Label59: TLabel;
    Label6: TLabel;
    Label60: TLabel;
    Label61: TLabel;
    Label62: TLabel;
    Label63: TLabel;
    Label64: TLabel;
    Label65: TLabel;
    Label66: TLabel;
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
    LBChannel1: TListBox;
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
    LBFixture1: TListBox;
    NB: TNotebook;
    BOk: TSpeedButton;
    PageDMXRGB: TPage;
    PageAudioCapture: TPage;
    PageNotEditable: TPage;
    PageDMXChannel: TPage;
    PageAudio: TPage;
    PageSequence: TPage;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    Panel14: TPanel;
    Panel15: TPanel;
    Panel16: TPanel;
    Panel17: TPanel;
    Panel18: TPanel;
    Panel19: TPanel;
    Panel2: TPanel;
    Panel20: TPanel;
    Panel21: TPanel;
    Panel22: TPanel;
    Panel23: TPanel;
    Panel24: TPanel;
    Panel25: TPanel;
    Panel26: TPanel;
    Panel27: TPanel;
    Panel28: TPanel;
    Panel29: TPanel;
    Panel3: TPanel;
    Panel30: TPanel;
    Panel33: TPanel;
    Panel34: TPanel;
    Panel35: TPanel;
    Panel36: TPanel;
    PanelChooseChannel: TPanel;
    PanelChooseFixture: TPanel;
    PanelWaveRGB: TPanel;
    Panel31: TPanel;
    Panel32: TPanel;
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
    PanelWave: TPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    RadioButton6: TRadioButton;
    RadioButton7: TRadioButton;
    RadioButton8: TRadioButton;
    Shape1: TShape;
    Shape3: TShape;
    Shape4: TShape;
    SpeedButton5: TSpeedButton;
    procedure BOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure FSE8Change(Sender: TObject);
    procedure Shape3MouseDown(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
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
    procedure FillLBFixture(aUniID: integer; aLB: TListBox; aFixtureID: integer=-1);
    procedure FillLBChannel(aUniID, aFixtureID: integer; aLB: TListBox; aChanIndex: integer=-1);
  private
    FrameVelocity1, FrameVelocity2, FrameVelocity3, FrameVelocity4, FrameVelocity5,
    FrameVelocity6, FrameVelocity7, FrameVelocity8, FrameVelocity9: TFrame_Velocity;
    FrameTBSequenceStretchSpeed: TFrameTBSequenceStretchSpeed;
    FrameTBVol: TFrameTBAudioVolume;
    FrameTBPan: TFrameTBAudioPan;
    FrameTBPitch: TFrameTBAudioPitch;
    FrameTBDryWetCap: TFrameTBAudioDryWet;
    FrameTBVolCap: TFrameTBAudioVolume;
    FrameTBPanCap: TFrameTBAudioPan;
    FrameTBDmxFlashLevels, FrameTBDmxFlashRGBIntensity: TFrameTrackBar;
    FrameTBFollowerGain, FrameTBFollowerRGBGain: TFrameTBDmxAudioFollowerGain;
    FrameTBFollowerMax: TFrameTBDmxAudioFollowerBrightness;
    FrameTBFollowerSoften, FrameTBFollowerRGBSoften: TFrameTBDmxAudioFollowerSoften;
    FrameTBFlameLevels: TFrameTrackBar;
    FrameTBFlameWait: TFrameTBDmxFlameWait;
    FrameTBFlameSoften: TFrameTBDmxFlameSoften;
    FrameTBDimmer: TFrameTBDmxLevel;
    FrameTBDmxFlameRGBWait: TFrameTBDmxFlameRGBWait;
    FrameTBDmxFlameRGBAmplitude: TFrameTBDmxFlameRGBAmplitude;
    FrameTBDmxFlameRGBSoften: TFrameTBDmxFlameRGBSoften;
    FrameTBDmxWaveLevel1, FrameTBDmxWaveLevel2: TFrameTBDmxLevel;
    FrameColorPalette1: TFrame_ColorPalette;
    CheckedLabelManager: TCheckedLabelManager;
    FrameViewFixtureListRGB: TFrameViewFixturesList;
    function GetCmdIsEditable: boolean;
    procedure UpdateVolumeLabel;
    procedure UpdatePanLabel;
    procedure UpdatePitchLabel;
  public

    property Cmd: TSingleCmd read GetCmd write SetCmd;
    property CmdIsEditable: boolean read GetCmdIsEditable;
  end;


implementation
uses LCLType, u_helper, u_list_sequence, u_audio_manager,
  u_resource_string, u_utils, u_logfile, u_list_dmxuniverse;

{$R *.lfm}

{ TFormEditSingleAction }

procedure TFormEditSingleAction.BOkClick(Sender: TObject);
begin
  if Sender = BOk then
    ModalResult := mrOk;
end;

procedure TFormEditSingleAction.FormCreate(Sender: TObject);
var i: Integer;
begin
  // sequence
  CBSequence.Clear;
  for i:=0 to Sequences.Count-1 do begin
    CBSequence.Items.Add(Sequences.GetSequenceByIndex(i).Name);
  end;
  FrameVelocity1 := TFrame_Velocity.Create(Self);
  FrameVelocity1.Name := 'FrameVelocity1';
  FrameVelocity1.Parent := Panel1;
  FrameVelocity1.Align := alClient;
  FrameVelocity1.UpdateList;
  FrameVelocity1.OnChange := @TB1Change;

  FrameTBSequenceStretchSpeed := TFrameTBSequenceStretchSpeed.Create(Self, Panel24);
  FrameTBSequenceStretchSpeed.Init(trHorizontal, False, False, False);
  FrameTBSequenceStretchSpeed.Value := 1.0;
  FrameTBSequenceStretchSpeed.OnChange := @TB1Change;

  Label4.Caption := SSequence;
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
  LabelCurve.Caption := SVelocity;
  lblVol1.Caption := SIn;
  lblattendre9.Caption := SSeconds_;
  lblVol3.Caption := SIn;
  lblattendre11.Caption := SSeconds_;
  lblVol4.Caption := SIn;
  lblattendre13.Caption := SSeconds_;
  FrameTBVol := TFrameTBAudioVolume.Create(Self, Panel23);
  FrameTBVol.Init(trHorizontal, False, False, False);
  FrameTBVol.Value := 1.0;
  FrameTBVol.OnChange := @TBVolChange;
  FrameTBPan := TFrameTBAudioPan.Create(Self, Panel22);
  FrameTBPan.Init(trHorizontal, False, False, False);
  FrameTBPan.Value := 0.0;
  FrameTBPan.OnChange := @TBVolChange;
  FrameTBPitch := TFrameTBAudioPitch.Create(Self, Panel21);
  FrameTBPitch.Init(trHorizontal, False, False, False);
  FrameTBPitch.Value := 1.0;
  FrameTBPitch.OnChange := @TBVolChange;

  // capture
  FrameVelocity4 := TFrame_Velocity.Create(Self);
  FrameVelocity4.Name := 'FrameVelocity4';
  FrameVelocity4.Parent := Panel7;
  FrameVelocity4.Align := alClient;
  FrameVelocity4.UpdateList;
  FrameVelocity4.OnChange := @TBVolCapChange;

  FrameTBDryWetCap := TFrameTBAudioDryWet.Create(Self, Panel11);
  FrameTBDryWetCap.Init(trHorizontal, False, False, False);
  FrameTBDryWetCap.Value := 0.5;
  FrameTBDryWetCap.OnChange := @TBVolCapChange;

  FrameTBVolCap := TFrameTBAudioVolume.Create(Self, Panel12);
  FrameTBVolCap.Init(trHorizontal, False, False, False);
  FrameTBVolCap.Value := 1.0;
  FrameTBVolCap.OnChange := @TBVolCapChange;

  FrameTBPanCap := TFrameTBAudioPan.Create(Self, Panel13);
  FrameTBPanCap.Init(trHorizontal, False, False, False);
  FrameTBPanCap.Value := 0.0;
  FrameTBPanCap.OnChange := @TBVolCapChange;

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
  FrameTBDimmer := TFrameTBDmxLevel.Create(Self, Panel20);
  FrameTBDimmer.Init(trHorizontal, False, False, False);
  FrameTBDimmer.OnChange := @TrackBar1Change;
  // dmx flame
  Label12.Caption := SLevels;
  Label10.Caption := SWaitTime;
  Label16.Caption := SSoften;
  FrameTBFlameLevels := TFrameTrackBar.Create(Self, Panel17);
  FrameTBFlameLevels.Init(trHorizontal, False, True, True);
  FrameTBFlameLevels.PercentMin := 0.25;
  FrameTBFlameLevels.PercentMax := 0.75;
  FrameTBFlameLevels.OnChange := @TrackBar1Change;
  FrameTBFlameWait := TFrameTBDmxFlameWait.Create(Self, Panel18);
  FrameTBFlameWait.Init(trHorizontal, False, False, False);
  FrameTBFlameWait.Value := 0.5;
  FrameTBFlameWait.OnChange := @TrackBar1Change;
  FrameTBFlameSoften := TFrameTBDmxFlameSoften.Create(Self, Panel19);
  FrameTBFlameSoften.Init(trHorizontal, False, False, False);
  FrameTBFlameSoften.Value := 1.0;
  FrameTBFlameSoften.OnChange := @TrackBar1Change;
  // dmx audio follower
  FrameTBFollowerGain := TFrameTBDmxAudioFollowerGain.Create(Self, Panel14);
  FrameTBFollowerGain.Init(trHorizontal, False, False, False);
  FrameTBFollowerGain.Value := 0.0;
  FrameTBFollowerGain.OnChange := @TrackBar1Change;
  FrameTBFollowerMax := TFrameTBDmxAudioFollowerBrightness.Create(Self, Panel15);
  FrameTBFollowerMax.Init(trHorizontal, False, False, False);
  FrameTBFollowerMax.Value := 0.75;
  FrameTBFollowerMax.OnChange := @TrackBar1Change;
  FrameTBFollowerSoften := TFrameTBDmxAudioFollowerSoften.Create(Self, Panel16);
  FrameTBFollowerSoften.Init(trHorizontal, False, False, False);
  FrameTBFollowerSoften.Value := 0.5;
  FrameTBFollowerSoften.OnChange := @TrackBar1Change;
  Label24.Caption := SFollow;
  Label18.Caption := sGain;
  Label20.Caption := SBrightnessMax;
  Label22.Caption := SSoftenOn;
  // dmx flash
  FrameTBDmxFlashLevels := TFrameTrackBar.Create(Self, Panel5);
  FrameTBDmxFlashLevels.Init(trHorizontal, False, False, True);
  FrameTBDmxFlashLevels.OnChange := @TrackBar1Change;
  Label28.Caption := SFixedValue;
  Label29.Caption := SRandomValueBetween;
  Label26.Caption := SFixedDuration;
  Label30.Caption := SRandomDurationBetween;
  Label31.Caption := SSeconds_;
  //dmx wave
  FrameTBDmxWaveLevel1 := TFrameTBDmxLevel.Create(Self, Panel30);
  FrameTBDmxWaveLevel1.Init(trHorizontal, False, False, False);
  FrameTBDmxWaveLevel1.OnChange := @TrackBar1Change;
  FrameTBDmxWaveLevel2 := TFrameTBDmxLevel.Create(Self, Panel35);
  FrameTBDmxWaveLevel2.Init(trHorizontal, False, False, False);
  FrameTBDmxWaveLevel2.OnChange := @TrackBar1Change;
  FrameVelocity8 := TFrame_Velocity.Create(Self);
  FrameVelocity8.Name := 'FrameVelocity8';
  FrameVelocity8.Parent := Panel33;
  FrameVelocity8.Align := alClient;
  FrameVelocity8.UpdateList;
  FrameVelocity8.OnChange := @TrackBar1Change;
  FrameVelocity9 := TFrame_Velocity.Create(Self);
  FrameVelocity9.Name := 'FrameVelocity9';
  FrameVelocity9.Parent := Panel34;
  FrameVelocity9.Align := alClient;
  FrameVelocity9.UpdateList;
  FrameVelocity9.OnChange := @TrackBar1Change;
  Label51.Caption := SLevel + ' 1';
  Label53.Caption := SLevel + ' 2';
  Label15.Caption := SDuration + ' 1';
  Label55.Caption := SDuration + ' 2';
  Label52.Caption := SVelocity + ' 1';
  Label54.Caption := SVelocity + ' 2';
  Label56.Caption := SSec;
  Label57.Caption := SSec;
  // dmx channel
  Label62.Caption := SUniverse;
  Label63.Caption := SFixture;
  Label64.Caption := SChannel;
  FInitializing := True;
  CBUni1.Clear;
  for i:=0 to UniverseManager.Count-1 do
    CBUni1.Items.Add(UniverseManager.Universes[i].Name);

  // dmx rgb
  Label65.Caption := SUniverse;
  Label66.Caption := SFixture;
  FInitializing := True;
  CBUni2.Clear;
  for i:=0 to UniverseManager.Count-1 do
    CBUni2.Items.Add(UniverseManager.Universes[i].Name);
  FrameViewFixtureListRGB := TFrameViewFixturesList.Create(Self);
  FrameViewFixtureListRGB.Name := 'FrameViewFixtureListRGB';
  FrameViewFixtureListRGB.Parent := Panel36;
  FrameViewFixtureListRGB.Align := alClient;
  FrameViewFixtureListRGB.OnSelectionChange := @FSE8Change;

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
  FrameTBDmxFlameRGBWait := TFrameTBDmxFlameRGBWait.Create(Self, Panel27);
  FrameTBDmxFlameRGBWait.Init(trHorizontal, False, False, False);
  FrameTBDmxFlameRGBWait.Value := 0.25;
  FrameTBDmxFlameRGBWait.OnChange := @FSE8Change;
  FrameTBDmxFlameRGBAmplitude := TFrameTBDmxFlameRGBAmplitude.Create(Self, Panel28);
  FrameTBDmxFlameRGBAmplitude.Init(trHorizontal, False, False, False);
  FrameTBDmxFlameRGBAmplitude.Value := 0.5;
  FrameTBDmxFlameRGBAmplitude.OnChange := @FSE8Change;
  FrameTBDmxFlameRGBSoften := TFrameTBDmxFlameRGBSoften.Create(Self, Panel29);
  FrameTBDmxFlameRGBSoften.Init(trHorizontal, False, False, False);
  FrameTBDmxFlameRGBSoften.Value := 0.25;
  FrameTBDmxFlameRGBSoften.OnChange := @FSE8Change;

  Label32.Caption := SWaitTime;
  Label34.Caption := SAmplitude;
  Label36.Caption := SSoften;
  // follower rgb
  FrameTBFollowerRGBGain := TFrameTBDmxAudioFollowerGain.Create(Self, Panel25);
  FrameTBFollowerRGBGain.Init(trHorizontal, False, False, False);
  FrameTBFollowerRGBGain.Value := 0.0;
  FrameTBFollowerRGBGain.OnChange := @FSE8Change;
  FrameTBFollowerRGBSoften := TFrameTBDmxAudioFollowerSoften.Create(Self, Panel26);
  FrameTBFollowerRGBSoften.Init(trHorizontal, False, False, False);
  FrameTBFollowerRGBSoften.Value := 0.5;
  FrameTBFollowerRGBSoften.OnChange := @FSE8Change;
  Label38.Caption := SGain;
  Label40.Caption := SSoftenOn;
  // dmx flash rgb
  FrameTBDmxFlashRGBIntensity := TFrameTrackBar.Create(Self, Panel10);
  FrameTBDmxFlashRGBIntensity.Init(trHorizontal, False, False, True);
  FrameTBDmxFlashRGBIntensity.OnChange := @FSE8Change;
  Label45.Caption := SFixedIntensity;
  Label46.Caption := SRandomIntensityBetween;
  Label42.Caption := SFixedDuration;
  Label43.Caption := SRandomDurationBetween;
  Label44.Caption := SSeconds_;

  // dmx wave rgb
  FrameVelocity6 := TFrame_Velocity.Create(Self);
  FrameVelocity6.Name := 'FrameVelocity6';
  FrameVelocity6.Parent := Panel31;
  FrameVelocity6.Align := alClient;
  FrameVelocity6.UpdateList;
  FrameVelocity6.OnChange := @FSE8Change;
  FrameVelocity7 := TFrame_Velocity.Create(Self);
  FrameVelocity7.Name := 'FrameVelocity7';
  FrameVelocity7.Parent := Panel32;
  FrameVelocity7.Align := alClient;
  FrameVelocity7.UpdateList;
  FrameVelocity7.OnChange := @FSE8Change;
  Label13.Caption := SColor + ' 1';
  Label48.Caption := SColor + ' 2';
  Label9.Caption := SDuration + ' 1';
  Label50.Caption := SDuration + ' 2';
  Label47.Caption := SVelocity + ' 1';
  Label49.Caption := SVelocity + ' 2';
  Label58.Caption := SSec;
  Label59.Caption := SSec;
  Shape3.Hint := SClickToCaptureCurrentColor;
  Shape4.Hint := SClickToCaptureCurrentColor;


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
var uni: TDMXUniverse;
  fix: TDMXFixture;
  i: integer;
  A: ArrayOfDmxFixtures;
begin
  if CBUni2.ItemIndex <> -1 then uni := UniverseManager.Universes[CBUni2.ItemIndex]
    else uni := NIL;

  if Sender = CBUni2 then
    FrameViewFixtureListRGB.FillWithRGBFixture(uni);

  A := FrameViewFixtureListRGB.Selected;
  if Length(A) <> 0 then fix := A[0]
    else fix := NIL;

  // flame
  Label33.Caption := FrameTBDmxFlameRGBWait.GetLegend;
  Label35.Caption := FrameTBDmxFlameRGBAmplitude.GetLegend;
  Label37.Caption := FrameTBDmxFlameRGBSoften.GetLegend;
  // follower
  Label39.Caption := FrameTBFollowerRGBGain.GetLegend;
  Label41.Caption := FrameTBFollowerRGBSoften.GetLegend;
  // flash
  if Sender = RadioButton7 then
    FrameTBDmxFlashRGBIntensity.Init(trHorizontal, False, RadioButton8.Checked, True);
  if Sender = FloatSpinEdit4 then
    if FloatSpinEdit5.Value < FloatSpinEdit4.Value then FloatSpinEdit5.Value := FloatSpinEdit4.Value;
  if Sender = FloatSpinEdit5 then
    if FloatSpinEdit4.Value > FloatSpinEdit5.Value then FloatSpinEdit4.Value := FloatSpinEdit5.Value;
  FloatSpinEdit5.Enabled := RadioButton6.Checked;

  if FInitializing then exit;

  case FCmd of
    CMD_DMX_DIMMERRGB,      // CMD_DMX_xxxRGB IDuniverse IDFixture ...
    CMD_DMX_FLAMERGB,
    CMD_DMX_AUDIOFOLLOWERRGB,
    CMD_DMX_FLASHRGB,
    TITLECMD_DMX_COPYRGB,
    CMD_DMX_COPYRGB: begin
      if (uni = NIL) or (fix = NIL) then exit;
      if FCmd = CMD_DMX_COPYRGB then i := 3
        else i := 1;
      FParams[i] := uni.ID.ToString;
      FParams[i+1] := fix.ID.ToString;
    end;

    TITLECMD_DMX_DIMMERRGB: begin  // TITLECMD_DMX_DIMMERRGB Color Duration CurveID
      FParams[1] := integer(FrameColorPalette1.ShapeColor.ToColor).ToString;
      FParams[2] := FormatFloatWithDot('0.00', FSE8.Value);
      FParams[3] := FrameVelocity5.SelectedCurveID.ToString;
    end;

    TITLECMD_DMX_FLAMERGB: begin // TITLECMD_DMX_FLAMERGB Color WaitTime Amplitude Soften
      FParams[1] := integer(FrameColorPalette1.ShapeColor.ToColor).ToString;
      FParams[2] := FormatFloatWithDot('0.00', FrameTBDmxFlameRGBWait.Value);
      FParams[3] := FormatFloatWithDot('0.00', FrameTBDmxFlameRGBAmplitude.Value);
      FParams[4] := FormatFloatWithDot('0.00', FrameTBDmxFlameRGBSoften.Value);
    end;

    TITLECMD_DMX_AUDIOFOLLOWERRGB: begin // TITLECMD_DMX_AUDIOFOLLOWERRGB IDaudio Color Gain SoftenTime
      FParams[1] := SoundManager.GetSoundByIndex(CBAudio3.ItemIndex).Tag.ToString;
      FParams[2] := integer(FrameColorPalette1.ShapeColor.ToColor).ToString;
      FParams[3] := FormatFloatWithDot('0.00', FrameTBFollowerRGBGain.Value);
      FParams[4] := FormatFloatWithDot('0.00', FrameTBFollowerRGBSoften.Value);
    end;

    TITLECMD_DMX_FLASHRGB: begin // TITLECMD_DMX_FLASHRGB Color pcMin pcMax DurationMin DurationMax
      FParams[1] := integer(FrameColorPalette1.ShapeColor.ToColor).ToString;
      FParams[2] := FormatFloatWithDot('0.00', FrameTBDmxFlashRGBIntensity.PercentMin);
      FParams[3] := FormatFloatWithDot('0.00', FrameTBDmxFlashRGBIntensity.PercentMax);
      FParams[4] := FormatFloatWithDot('0.00', FloatSpinEdit4.Value);
      if RadioButton5.Checked then FParams[5] := FParams[4]
        else FParams[5] := FormatFloatWithDot('0.00', FloatSpinEdit5.Value);
    end;

    TITLECMD_DMX_WAVERGB: begin // TITLECMD_DMX_WAVERGB Color1 Duration1 CurveID1 Color2 Duration2 CurveID2
      FParams[1] := integer(Shape3.Brush.Color).ToString;
      FParams[2] := FormatFloatWithDot('0.00', FloatSpinEdit6.Value);
      FParams[3] := FrameVelocity6.SelectedCurveID.ToString;
      FParams[4] := integer(Shape4.Brush.Color).ToString;
      FParams[5] := FormatFloatWithDot('0.00', FloatSpinEdit7.Value);
      FParams[6] := FrameVelocity7.SelectedCurveID.ToString;
    end;
  end;//case
end;

procedure TFormEditSingleAction.Shape3MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Sender = Shape3 then Shape3.Brush.Color := FrameColorPalette1.SelectedColor;
  if Sender = Shape4 then Shape4.Brush.Color := FrameColorPalette1.SelectedColor;
  FSE8Change(NIL);
end;

procedure TFormEditSingleAction.TB1Change(Sender: TObject);
begin
  if Sender = SpeedButton5 then FrameTBSequenceStretchSpeed.Value := 1.0;
  Label11.Caption := StrechTimeToString(FrameTBSequenceStretchSpeed.Value);

  if FInitializing then exit;

  if CBSequence.ItemIndex = -1 then exit;
  case FCmd of
    CMD_STARTSEQUENCE, CMD_STOPSEQUENCE: begin  // CMD_STARTSEQUENCE IDseq
      FParams[1] := Sequences.Items[CBSequence.ItemIndex].ID.ToString;
    end;
    CMD_SEQUENCESTRETCHTIME: begin // ATOPSTRETCHTIME IDseq StretchValueF DurationF CurveID
      FParams[1] := Sequences.Items[CBSequence.ItemIndex].ID.ToString;
      FParams[2] := FormatFloatWithDot('0.00', FrameTBSequenceStretchSpeed.Value);
      FParams[3] := FormatFloatWithDot('0.00', FSE4.Value);
      FParams[4] := FrameVelocity1.SelectedCurveID.ToString;
    end;
  end;
end;

procedure TFormEditSingleAction.TBVolCapChange(Sender: TObject);
var vol, pan, drywet: single;
begin
  if Sender = BPanCenterCap then FrameTBPanCap.Value := 0.0;
  vol := FrameTBVolCap.Value;
  pan := FrameTBPanCap.Value;
  drywet := FrameTBDryWetCap.Value;
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
  if Sender = BPanNormal then FrameTBPan.Value := 0.0;
  if Sender = BPitchNormal then FrameTBPitch.Value := 1.0;
  UpdateVolumeLabel;
  UpdatePanLabel;
  UpdatePitchLabel;

  if FInitializing then exit;

  case FCmd of
   CMD_AUDIO_PLAY: begin  // AUDIOLECTURE IDaudio volume panning
     if CBAudio1.ItemIndex = -1 then exit;
     FParams[1] := SoundManager.GetSoundByIndex(CBAudio1.ItemIndex).Tag.ToString;
     FParams[2] := FormatFloatWithDot('0.00', FrameTBVol.PercentValue);
     FParams[3] := FormatFloatWithDot('0.00', FrameTBPan.Value);
   end;
   CMD_AUDIO_STOP, CMD_AUDIO_PAUSE: begin  // AUDIOSTOP IDaudio
     if CBAudio1.ItemIndex = -1 then exit;
     FParams[1] := SoundManager.GetSoundByIndex(CBAudio1.ItemIndex).Tag.ToString;
   end;
   CMD_AUDIO_FADEIN, CMD_AUDIO_SETVOLUME: begin // AUDIOFADEIN IDaudio volume duration IDcurve
     if CBAudio1.ItemIndex = -1 then exit;
     FParams[1] := SoundManager.GetSoundByIndex(CBAudio1.ItemIndex).Tag.ToString;
     FParams[2] := FormatFloatWithDot('0.00', FrameTBVol.PercentValue);
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
     FParams[2] := FormatFloatWithDot('0.00', FrameTBPan.Value);
     FParams[3] := FormatFloatWithDot('0.00', FSE2.Value);
     FParams[4] := FrameVelocity2.SelectedCurveID.ToString;
   end;
   CMD_AUDIO_SETPITCH: begin // AUDIOFIXEFREQ IDaudio frequence duration IDcurve
     if CBAudio1.ItemIndex = -1 then exit;
     FParams[1] := SoundManager.GetSoundByIndex(CBAudio1.ItemIndex).Tag.ToString;
     FParams[2] := FormatFloatWithDot('0.00', FrameTBPitch.Value);
     FParams[3] := FormatFloatWithDot('0.00', FSE3.Value);
     FParams[4] := FrameVelocity2.SelectedCurveID.ToString;
   end;
   TITLECMD_AUDIO_APPLYFX: begin // TITLECMD_AUDIO_APPLYFX  IDaudio  dry/wet  EffectCount
     if CBAudio1.ItemIndex = -1 then exit;
     FParams[1] := SoundManager.GetSoundByIndex(CBAudio1.ItemIndex).Tag.ToString;
     FParams[2] := FormatFloatWithDot('0.00', FrameTBVol.PercentValue);
   end;
  end;
end;

procedure TFormEditSingleAction.TrackBar1Change(Sender: TObject);
var uni: TDMXUniverse;
  fix: TDMXFixture;
  chan, i: integer;
begin
  if CBUni1.ItemIndex <> -1 then uni := UniverseManager.Universes[CBUni1.ItemIndex]
    else uni := NIL;

  if Sender = CBUni1 then begin
    if uni <> NIL then FillLBFixture(uni.ID, LBFixture1)
      else LBFixture1.Clear;
    LBChannel1.Clear;
  end;

  if (LBFixture1.ItemIndex <> -1) and (uni <> NIL) then fix := uni.Fixtures[LBFixture1.ItemIndex]
    else fix := NIL;

  if (Sender = LBFixture1) and (CBUni1.ItemIndex <> -1) then
    if (uni <> NIL) and (fix <> NIL) then FillLBChannel(uni.ID, fix.ID, LBChannel1)
      else LBChannel1.Clear;

  chan := LBChannel1.ItemIndex;

  // dimmer
  Label7.Caption := FrameTBDimmer.GetLegend;
  // flame
  Label14.Caption := FrameTBFlameWait.GetLegend;
  Label17.Caption := FrameTBFlameSoften.GetLegend;
  // audio follower
  Label19.Caption := FrameTBFollowerGain.GetLegend;
  Label21.Caption := FrameTBFollowerMax.GetLegend;
  Label23.Caption := FrameTBFollowerSoften.GetLegend;
  // flash
  if Sender = RadioButton1 then
    FrameTBDmxFlashLevels.Init(trHorizontal, False, RadioButton2.Checked, True);
  if Sender = FloatSpinEdit2 then
    if FloatSpinEdit3.Value < FloatSpinEdit2.Value then FloatSpinEdit3.Value := FloatSpinEdit2.Value;
  if Sender = FloatSpinEdit3 then
    if FloatSpinEdit2.Value > FloatSpinEdit3.Value then FloatSpinEdit2.Value := FloatSpinEdit3.Value;
  FloatSpinEdit3.Enabled := RadioButton4.Checked;
  // wave
  Label60.Caption := FrameTBDmxWaveLevel1.GetLegend;
  Label61.Caption := FrameTBDmxWaveLevel2.GetLegend;


  if FInitializing then exit;

  case FCmd of
   CMD_DMX_DIMMER,    // CMD_xxx IDuniverse IDFixture ChanIndex ...
   CMD_DMX_FLAME,
   CMD_DMX_AUDIOFOLLOWER,
   CMD_DMX_FLASH,
   TITLECMD_DMX_COPYCHANNEL,
   CMD_DMX_COPYCHANNEL: begin
     if (uni = NIL) or (fix = NIL) or (chan = -1) then exit;
     if FCmd = CMD_DMX_COPYCHANNEL then i := 4
       else i := 1;
     FParams[i] := uni.ID.ToString;
     FParams[i+1] := fix.ID.ToString;
     FParams[i+2] := chan.ToString;
     if FCmd = CMD_DMX_DIMMER then
       FParams[4] := FormatFloatWithDot('0.00', FrameTBDimmer.Value);
   end;
   TITLECMD_DMX_DIMMER: begin // TITLECMD_DMX_DIMMER Duration CurveID
     FParams[1] := FormatFloatWithDot('0.00', FSE5.Value);
     FParams[2] := FrameVelocity3.SelectedCurveID.ToString;
   end;
   TITLECMD_DMX_FLAME: begin  // TITLECMD_DMX_FLAME  LevelMin LevelMax Speed Soften
     FParams[1] := FormatFloatWithDot('0.00', FrameTBFlameLevels.PercentMin);
     FParams[2] := FormatFloatWithDot('0.00', FrameTBFlameLevels.PercentMax);
     FParams[3] := FormatFloatWithDot('0.00', FrameTBFlameWait.Value);
     FParams[4] := FormatFloatWithDot('0.00', FrameTBFlameSoften.PercentValue);
   end;

   TITLECMD_DMX_AUDIOFOLLOWER: begin // TITLECMD_DMX_AUDIOFOLLOWER IDaudio Gain MaxPercent SoftenTime
     FParams[1] := SoundManager.IndexToID(CBAudio2.ItemIndex).ToString;
     FParams[2] := FormatFloatWithDot('0.00', FrameTBFollowerGain.Value);
     FParams[3] := FormatFloatWithDot('0.00', FrameTBFollowerMax.Value);
     FParams[4] := FormatFloatWithDot('0.00', FrameTBFollowerSoften.Value);
   end;

   TITLECMD_DMX_FLASH: begin // TITLECMD_DMX_FLASH LevelMin LevelMax DurationMin DurationMax
     FParams[1] := FormatFloatWithDot('0.00', FrameTBDmxFlashLevels.PercentMin);
     FParams[2] := FormatFloatWithDot('0.00', FrameTBDmxFlashLevels.PercentMax);
     FParams[3] := FormatFloatWithDot('0.00', FloatSpinEdit2.Value);
     if RadioButton3.Checked then FParams[4] := FParams[3]
       else FParams[4] := FormatFloatWithDot('0.00', FloatSpinEdit3.Value);
   end;
   TITLECMD_DMX_WAVE: begin // TITLECMD_DMX_WAVE Level1 Duration1 CurveID1 Level2 Duration2 CurveID2
     FParams[1] := FormatFloatWithDot('0.00', FrameTBDmxWaveLevel1.Value);
     FParams[2] := FormatFloatWithDot('0.00', FloatSpinEdit8.Value);
     FParams[3] := FrameVelocity8.SelectedCurveID.ToString;
     FParams[4] := FormatFloatWithDot('0.00', FrameTBDmxWaveLevel2.Value);
     FParams[5] := FormatFloatWithDot('0.00', FloatSpinEdit9.Value);
     FParams[6] := FrameVelocity9.SelectedCurveID.ToString;
   end;
  end;
end;

function TFormEditSingleAction.GetCmd: TSingleCmd;
begin
  Result := FParams.PackToCmd;
end;

procedure TFormEditSingleAction.SetCmd(AValue: TSingleCmd);
  procedure ShowPanelChooseChannel(IndexOfIDUniverse: integer; const aTitle: string);
  begin
    PanelChooseChannel.Visible := True;
    PanelChooseChannel.Left := ScaleDesignToForm(8);
    PanelChooseChannel.Top := ScaleDesignToForm(8);
    PanelChooseChannel.Height := ScaleDesignToForm(400);
    AdjustFormHeight(PanelChooseChannel);
    NB.PageIndex := NB.IndexOf(PageDMXChannel);
    Label2.Caption := aTitle;
    CBUni1.ItemIndex :=  UniverseManager.IDToIndex(FParams[IndexOfIDUniverse].ToInteger);
    FillLBFixture(FParams[IndexOfIDUniverse].ToInteger, LBFixture1, FParams[IndexOfIDUniverse+1].ToInteger);
    FillLBChannel(FParams[IndexOfIDUniverse].ToInteger, FParams[IndexOfIDUniverse+1].ToInteger, LBChannel1, FParams[IndexOfIDUniverse+2].ToInteger);
    TrackBar1Change(NIL);
  end;
  procedure ShowPanelChooseFixtureRGB(IndexOfParamIDUniverse: integer; const aTitle: string);
  var uni: TDMXUniverse;
  begin
    PanelChooseFixture.Visible := True;
    PanelChooseFixture.Top := ScaleDesignToForm(8);
    PanelChooseFixture.Height := ScaleDesignToForm(400);
    AdjustFormHeight(PanelChooseFixture);
    NB.PageIndex := NB.IndexOf(PageDMXRGB);
    Label2.Caption := aTitle;
    CBUni2.ItemIndex :=  UniverseManager.IDToIndex(FParams[IndexOfParamIDUniverse].ToInteger);
    uni := UniverseManager.GetUniverseByID(FParams[IndexOfParamIDUniverse].ToInteger);
    FrameViewFixtureListRGB.FillWithRGBFixture(uni);
    FrameViewFixtureListRGB.SelectFixture(FParams[IndexOfParamIDUniverse].ToInteger);
    FSE8Change(NIL);
  end;

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
       FrameTBSequenceStretchSpeed.Value := StringToSingle(FParams[2]);
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
       FrameTBVol.PercentValue := StringToSingle(FParams[2]);
       FrameTBPan.Value := StringToSingle(FParams[3]);
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
       FrameTBVol.PercentValue := StringToSingle(FParams[2]);
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
       FrameTBVol.Visible := False;
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
       FrameTBVol.PercentValue := StringToSingle(FParams[2]);
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
       FrameTBPan.Value := StringToSingle(FParams[2]);
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
       FrameTBPitch.Value := StringToSingle(FParams[2]);
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
       FrameTBVol.PercentValue := StringToSingle(FParams[2]);
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
       FrameTBVolCap.PercentValue := StringToSingle(FParams[1]);
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
       FrameTBPanCap.Value := StringToSingle(FParams[1]);
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
       FrameTBDryWetCap.PercentValue := StringToSingle(FParams[1]);
       TBVolCapChange(NIL);
     end;

     // dmx stuff
     CMD_DMX_DIMMER: begin  // CMD_DMX_DIMMER IDuniverse IDFixture ChanIndex PercentF DurationF CurveID
       PanelDimmer.Visible := True;
       PanelDimmer.Left := ScaleDesignToForm(8);
       PanelDimmer.Top := ScaleDesignToForm(8);
       PanelDimmer.Height := Panel20.Top + Panel20.Height + ScaleDesignToForm(8);
       Label8.Visible := False;
       FSE5.Visible := False;
       FrameVelocity3.Visible := False;
       LabelCurve1.Visible := False;
       FrameTBDimmer.Value := StringToSingle(FParams[4]);

       ShowPanelChooseChannel(1, SDMXDimmer);
       PanelChooseChannel.Top := PanelDimmer.Top + PanelDimmer.Height + ScaleDesignToForm(8);
       PanelChooseChannel.Height := ScaleDesignToForm(400);
       AdjustFormHeight(PanelChooseChannel);
     end;
     CMD_DMX_FLAME: ShowPanelChooseChannel(1, SDMXFlame);
     CMD_DMX_AUDIOFOLLOWER: ShowPanelChooseChannel(1, SDMXAudioFollower);
     CMD_DMX_FLASH: ShowPanelChooseChannel(1, SDMXFlash);
     TITLECMD_DMX_COPYCHANNEL: ShowPanelChooseChannel(1, SDMXCopy);
     CMD_DMX_COPYCHANNEL:ShowPanelChooseChannel(4, SDMXCopy);


     TITLECMD_DMX_DIMMER: begin  // TITLECMD_DMX_DIMMER Duration CurveID
       PanelDimmer.Visible := True;
       PanelDimmer.Left := ScaleDesignToForm(8);
       PanelDimmer.Top := ScaleDesignToForm(8);
       AdjustFormHeight(PanelDimmer);
       Label7.Visible := False;
       FrameTBDimmer.Visible := False;
       Label8.Top := (PanelDimmer.Height - Label8.Height) div 2;
       NB.PageIndex := NB.IndexOf(PageDMXChannel);
       Label2.Caption := SDMXDimmer;
       FSE5.Value := StringToSingle(FParams[1]);
       FrameVelocity3.SelectedCurveID := FParams[2].ToInteger;
       TrackBar1Change(NIL);
     end;
     TITLECMD_DMX_WAVE: begin // TITLECMD_DMX_WAVE Level1 Duration1 CurveID1 Level2 Duration2 CurveID2
       PanelWave.Visible := True;
       PanelWave.Left := (ClientWidth - PanelWave.Width) div 2;
       PanelWave.Top := ScaleDesignToForm(8);
       AdjustFormHeight(PanelWave);
       NB.PageIndex := NB.IndexOf(PageDMXChannel);
       Label2.Caption := SDMXWave;
       FrameTBDmxWaveLevel1.Value := StringToSingle(FParams[1]);
       FloatSpinEdit8.Value := StringToSingle(FParams[2]);
       FrameVelocity8.SelectedCurveID := FParams[3].ToInteger;
       FrameTBDmxWaveLevel2.Value := StringToSingle(FParams[4]);
       FloatSpinEdit9.Value := StringToSingle(FParams[5]);
       FrameVelocity9.SelectedCurveID := FParams[6].ToInteger;
       TrackBar1Change(NIL);
     end;

     TITLECMD_DMX_FLAME: begin  // TITLECMD_DMX_FLAME  LevelMin LevelMax Speed Soften
       PanelFlame.Visible := True;
       PanelFlame.Left := ScaleDesignToForm(8);
       PanelFlame.Top := ScaleDesignToForm(8);
       AdjustFormHeight(PanelFlame);
       NB.PageIndex := NB.IndexOf(PageDMXChannel);
       Label2.Caption := SDMXFlame;
       FrameTBFlameLevels.PercentMin := StringToSingle(FParams[1]);
       FrameTBFlameLevels.PercentMax := StringToSingle(FParams[2]);
       FrameTBFlameWait.Value := StringToSingle(FParams[3]);
       FrameTBFlameSoften.PercentValue := StringToSingle(FParams[4]);
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
       FrameTBFollowerGain.Value := StringToSingle(FParams[2]);
       FrameTBFollowerMax.Value := StringToSingle(FParams[3]);
       FrameTBFollowerSoften.Value := StringToSingle(FParams[4]);
       TrackBar1Change(NIL);
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
       FrameTBDmxFlashLevels.Init(trHorizontal, False, RadioButton2.Checked, True);
       FrameTBDmxFlashLevels.PercentMin := StringToSingle(FParams[1]);
       FrameTBDmxFlashLevels.PercentMax := StringToSingle(FParams[2]);
       FloatSpinEdit2.Value := StringToSingle(FParams[3]);
       FloatSpinEdit3.Value := StringToSingle(FParams[4]);
     end;

     CMD_DMX_DIMMERRGB: ShowPanelChooseFixtureRGB(1, SDMXDimmerRGB);
     CMD_DMX_FLAMERGB: ShowPanelChooseFixtureRGB(1, SDMXFlameRGB);
     CMD_DMX_AUDIOFOLLOWERRGB: ShowPanelChooseFixtureRGB(1,SDMXAudioFollowerRGB);
     CMD_DMX_FLASHRGB: ShowPanelChooseFixtureRGB(1,SDMXFlashRGB);
     TITLECMD_DMX_COPYRGB: ShowPanelChooseFixtureRGB(1, SDMXCopyRGB);
     CMD_DMX_COPYRGB: ShowPanelChooseFixtureRGB(4, SDMXCopyRGB);

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

     TITLECMD_DMX_FLAMERGB: begin // TITLECMD_DMX_FLAMERGB Color Speed Amplitude Soften
       PanelPalette.Visible := True;
       PanelRGBFlame.Visible := True;
       PanelRGBFlame.Left := ScaleDesignToForm(8);
       PanelRGBFlame.Top := PanelPalette.Top+PanelPalette.Height+ScaleDesignToForm(8);
       AdjustFormHeight(PanelRGBFlame);
       NB.PageIndex := NB.IndexOf(PageDMXRGB);
       Label2.Caption := SDMXFlameRGB;
       FrameColorPalette1.SelectedColor := TColor(FParams[1].ToInteger);
       FrameTBDmxFlameRGBWait.Value := StringToSingle(FParams[2]);
       FrameTBDmxFlameRGBAmplitude.Value := StringToSingle(FParams[3]);
       FrameTBDmxFlameRGBSoften.Value := StringToSingle(FParams[4]);
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
       FrameTBFollowerRGBGain.Value := StringToSingle(FParams[3]);
       FrameTBFollowerRGBSoften.Value := StringToSingle(FParams[4]);
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
       FrameTBDmxFlashRGBIntensity.Init(trHorizontal, False, RadioButton8.Checked, True);
       FrameTBDmxFlashRGBIntensity.PercentMin := StringToSingle(FParams[2]);
       FrameTBDmxFlashRGBIntensity.PercentMax := StringToSingle(FParams[3]);
       FloatSpinEdit4.Value := StringToSingle(FParams[4]);
       FloatSpinEdit5.Value := StringToSingle(FParams[5]);
       FSE8Change(NIL);
     end;

     TITLECMD_DMX_WAVERGB: begin   // TITLECMD_DMX_WAVERGB Color1 Duration1 CurveID1 Color2 Duration2 CurveID2
       PanelPalette.Visible := True;
       PanelWaveRGB.Visible := True;
       PanelWaveRGB.Left := (ClientWidth - PanelWaveRGB.Width) div 2;
       PanelWaveRGB.Top := PanelPalette.Top + PanelPalette.Height + ScaleDesignToForm(8);
       AdjustFormHeight(PanelWaveRGB);
       NB.PageIndex := NB.IndexOf(PageDMXRGB);
       Label2.Caption := SDMXWaveRGB;
       FrameColorPalette1.SelectedColor := TColor(FParams[1].ToInteger);
       Shape3.Brush.Color := TColor(FParams[1].ToInteger);
       Shape4.Brush.Color := TColor(FParams[4].ToInteger);
       FloatSpinEdit6.Value := StringToSingle(FParams[2]);
       FloatSpinEdit7.Value := StringToSingle(FParams[5]);
       FrameVelocity6.SelectedCurveID := FParams[3].ToInteger;
       FrameVelocity7.SelectedCurveID := FParams[6].ToInteger;
     end

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

procedure TFormEditSingleAction.FillLBFixture(aUniID: integer; aLB: TListBox; aFixtureID: integer);
var uni: TDMXUniverse;
  i: integer;
begin
  aLB.Clear;
  uni := UniverseManager.GetUniverseByID(aUniID);
  if uni = NIL then exit;

  for i:=0 to uni.FixturesCount-1 do
    aLB.Items.Add(uni.Fixtures[i].Name+' - '+uni.Fixtures[i].Description);

  // sets the right item index
  if aFixtureID > -1 then
    aLB.Itemindex := uni.FixtureIDToIndex(aFixtureID);
end;

procedure TFormEditSingleAction.FillLBChannel(aUniID, aFixtureID: integer; aLB: TListBox; aChanIndex: integer);
var uni: TDMXUniverse;
  fix: TDMXFixture;
  i: integer;
begin
  aLB.Clear;
  uni := UniverseManager.GetUniverseByID(aUniID);
  if uni = NIL then exit;
  fix := uni.Fixture_GetByID(aFixtureID);
  if fix = NIL then exit;

  for i:=0 to fix.ChannelsCount-1 do
    aLB.Items.Add(fix.Channels[i].Name);

  // sets the right item index
  if aChanIndex > -1 then
    aLB.Itemindex := aChanIndex;
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
            (FCmd <> CMD_DMX_STOPEFFECTRGB) and
            (FCmd <> TITLECMD_DMX_STOPEFFECTRGB) and
            (FCmd <> TITLECMD_DMX_COPYRGB) and
            (FCmd <> CMD_DMX_COPYRGB);
end;

procedure TFormEditSingleAction.UpdateVolumeLabel;
begin
  if FCmd = TITLECMD_AUDIO_APPLYFX then LabelVolume.Caption := SDryWet+'  '+DryWetToStringPercent(FrameTBVol.PercentValue)
    else LabelVolume.Caption := SVolume+' '+VolumeToStringPercent(FrameTBVol.PercentValue);
end;

procedure TFormEditSingleAction.UpdatePanLabel;
begin
  LabelPan.Caption := SPan+' '+PanToStringPercent(FrameTBPan.Value);
end;

procedure TFormEditSingleAction.UpdatePitchLabel;
begin
  LabelPitch.Caption := SPitch+' '+PitchToString(FrameTBPitch.Value);
end;

end.

