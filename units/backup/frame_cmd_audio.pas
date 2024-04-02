unit frame_cmd_audio;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Buttons, Spin,
  ComCtrls, LCLType, LCLTranslator,
  frame_viewaudiolist,
  u_common, frame_velocity, u_audio_manager, u_notebook_util;

type

  { TFrameCmdAudio }

  TFrameCmdAudio = class(TFrame)
    BAdd: TBitBtn;
    BPitchNormal: TButton;
    BPanNormal: TButton;
    FSE1: TFloatSpinEdit;
    FSE2: TFloatSpinEdit;
    FSE3: TFloatSpinEdit;
    Label13: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    lblattendre11: TLabel;
    lblattendre13: TLabel;
    lblattendre9: TLabel;
    LabelCurve: TLabel;
    LabelPan: TLabel;
    LabelPitch: TLabel;
    LabelVolume: TLabel;
    lblVol1: TLabel;
    lblVol3: TLabel;
    lblVol4: TLabel;
    Notebook1: TNotebook;
    PageCapture: TPage;
    PageFile: TPage;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel9: TPanel;
    PanelVolume: TPanel;
    PanelPan: TPanel;
    PanelPitch: TPanel;
    PanelCurve: TPanel;
    BListen: TSpeedButton;
    BStopAll: TSpeedButton;
    SpeedButton1: TSpeedButton;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
    SpeedButton12: TSpeedButton;
    SpeedButton13: TSpeedButton;
    SpeedButton14: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    TBPan: TTrackBar;
    TBPitch: TTrackBar;
    TBVol: TTrackBar;
    procedure BListenClick(Sender: TObject);
    procedure BPanNormalClick(Sender: TObject);
    procedure BStopAllClick(Sender: TObject);
    procedure BAddClick(Sender: TObject);
    procedure BPitchNormalClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure TBPanChange(Sender: TObject);
    procedure TBPitchChange(Sender: TObject);
    procedure TBVolChange(Sender: TObject);
  private
    FCmds: TCmdList;
    FCmdDuration: single;
    FSelectedSounds: ArrayOfSound;
    FOnAddCmd: TNotifyEvent;
    Frame_Velocity1: TFrame_Velocity;
    FShortReadableString: string;
    FUseCmdTitle: boolean;
    FSelectedActionIndex: integer;
    procedure DoOnAddCmd;
    procedure ProcessSelectionChange(Sender: TObject);
    procedure GenerateCmdForPlayFile;
    procedure GenerateCmdForPauseFile;
    procedure GenerateCmdForStopFile;
    procedure GenerateCmdForFadeInFile;
    procedure GenerateCmdForFadeOutFile;
    procedure GenerateCmdForVolumeFile;
    procedure GenerateCmdForPanFile;
    procedure GenerateCmdForPitchFile;

    procedure GenerateCmdForStartCapture;
    procedure GenerateCmdForStopCapture;
    procedure GenerateCmdForVolumeCapture;
    procedure GenerateCmdForPanCapture;

    function GetVolume: single;
    function GetPan: single;
    function GetPitch: single;
    procedure UpdateVolumeLabel;
    procedure UpdatePanLabel;
    procedure UpdatePitchLabel;
    procedure UpdatePanels;
  private
    FFileSBManager,
    FCaptureSBManager: TToggleSpeedButtonManager;
    FNoteBookManager: TNoteBookManager;
    procedure StartPlayback(aFromBegining: boolean);
    function TargetIsFile: boolean;
  public
    FrameViewAudioList1: TFrameViewAudioList;
    procedure ProcessSourceChangeEvent(Sender: TObject);
    constructor Create( TheOwner: TComponent ); override;
    destructor Destroy; override;
    procedure EraseBackground({%H-}DC: HDC); override;

    procedure Init;
    procedure Sel_None;

    property OnAddCmd: TNotifyEvent read FOnAddCmd write FOnAddCmd;
    property Cmds: TCmdList read FCmds;
    property ShortReadableString: string read FShortReadableString;
    property CmdDuration: single read FCmdDuration;
    // if set to true, send a title action before the action
    property UseCmdTitle: boolean read FUseCmdTitle write FUseCmdTitle default TRUE;
  end;

implementation

uses u_resource_string, u_utils, u_helper, ALSound, LCLHelper, Graphics,
  frame_bglvirtualscreen_sequencer;

{$R *.lfm}

{ TFrameCmdAudio }

procedure TFrameCmdAudio.SpeedButton1Click(Sender: TObject);
begin
  FSelectedActionIndex := TSpeedButton(Sender).Tag;
  UpdatePanels;
end;

procedure TFrameCmdAudio.TBPanChange(Sender: TObject);
begin
  if TargetIsFile then
    FrameViewAudioList1.SetPanOnSelected( GetPan )
  else
    SoundManager.CaptureSetPan( GetPan );
  UpdatePanLabel;
end;

procedure TFrameCmdAudio.TBPitchChange(Sender: TObject);
begin
 FrameViewAudioList1.SetPitchOnSelected( GetPitch );
 UpdatePitchLabel;
end;

procedure TFrameCmdAudio.TBVolChange(Sender: TObject);
begin
  if TargetIsFile then
    FrameViewAudioList1.SetVolumeOnSelected( GetVolume )
  else
    SoundManager.CaptureSetVolume( GetVolume );
  UpdateVolumeLabel;
end;

procedure TFrameCmdAudio.DoOnAddCmd;
begin
  if FOnAddCmd<>NIL
    then FOnAddCmd(Self);
end;

procedure TFrameCmdAudio.ProcessSelectionChange(Sender: TObject);
var snd: TALSSound;
begin
  snd:=FrameViewAudioList1.FirstSelected;
  if snd<>NIL then begin
    TBVol.Position:=TBVol.Max;
    TBPan.Position:=0;
    TBPitch.Position:=100;
  end;
end;

procedure TFrameCmdAudio.GenerateCmdForPlayFile;
var i: integer;
  snd: TALSSound;
begin
  FCmds:=CmdTitleAudioPlay;

  for i:=0 to High(FSelectedSounds) do begin
    snd:=FSelectedSounds[i];
    FCmds.ConcatCmd(CmdAudioPlay(snd.Tag, GetVolume, GetPan));
  end;

  FShortReadableString:=SAudioPlay+' ';
  if Length(FSelectedSounds)>1
    then FShortReadableString:=FShortReadableString+SMultiple
    else FShortReadableString:=FShortReadableString+ExtractFileName(snd.Filename);
  FCmdDuration:=0.0;
end;

procedure TFrameCmdAudio.GenerateCmdForPauseFile;
var i: integer;
  snd: TALSSound;
begin
  FCmds:=CmdTitleAudioPause;

  for i:=0 to High(FSelectedSounds) do begin
    snd:=FSelectedSounds[i];
    FCmds.ConcatCmd(CmdAudioPause(snd.Tag));
  end;

  FShortReadableString:=SAudioPause+' ';
  if Length(FSelectedSounds)>1
    then FShortReadableString:=FShortReadableString+SMultiple
    else FShortReadableString:=FShortReadableString+ExtractFileName(snd.Filename);
  FCmdDuration:=0.0;
end;

procedure TFrameCmdAudio.GenerateCmdForStopFile;
var i: integer;
  snd: TALSSound;
begin
  FCmds:=CmdTitleAudioStop;

  for i:=0 to High(FSelectedSounds) do begin
    snd:=FSelectedSounds[i];
    FCmds.ConcatCmd(CmdAudioStop(snd.Tag));
  end;

  FShortReadableString:=SAudioStop+' ';
  if Length(FSelectedSounds)>1
    then FShortReadableString:=FShortReadableString+SMultiple
    else FShortReadableString:=FShortReadableString+ExtractFileName(snd.Filename);
  FCmdDuration:=0.0;
end;

procedure TFrameCmdAudio.GenerateCmdForFadeInFile;
var i: integer;
  snd: TALSSound;
begin
  FCmds:=CmdTitleAudioFadeIN;

  for i:=0 to High(FSelectedSounds) do begin
    snd:=FSelectedSounds[i];
    FCmds.ConcatCmd(CmdAudioFadeIN(snd.Tag, GetVolume, FSE1.Value, Frame_Velocity1.SelectedCurveID));
  end;

  FShortReadableString:=SAudioFadeIn+' ';
  if Length(FSelectedSounds)>1
    then FShortReadableString:=FShortReadableString+SMultiple
    else FShortReadableString:=FShortReadableString+ExtractFileName(snd.Filename);
  FCmdDuration:=FSE1.Value;
end;

procedure TFrameCmdAudio.GenerateCmdForFadeOutFile;
var i: integer;
  snd: TALSSound;
begin
  FCmds:=CmdTitleAudioFadeOUT;

  for i:=0 to High(FSelectedSounds) do begin
    snd:=FSelectedSounds[i];
    FCmds.ConcatCmd(CmdAudioFadeOUT(snd.Tag, FSE1.Value, Frame_Velocity1.SelectedCurveID));
  end;

  FShortReadableString:=SAudioFadeOut+' ';
  if Length(FSelectedSounds)>1
    then FShortReadableString:=FShortReadableString+SMultiple
    else FShortReadableString:=FShortReadableString+ExtractFileName(snd.Filename);
  FCmdDuration:=FSE1.Value;
end;

procedure TFrameCmdAudio.GenerateCmdForVolumeFile;
var i: integer;
  snd: TALSSound;
begin
  FCmds:=CmdTitleAudioChangeVolume;

  for i:=0 to High(FSelectedSounds) do begin
    snd:=FSelectedSounds[i];
    FCmds.ConcatCmd(CmdAudioChangeVolume(snd.Tag, GetVolume, FSE1.Value, Frame_Velocity1.SelectedCurveID));
  end;

  FShortReadableString:=SAudioSetVolume+' ';
  if Length(FSelectedSounds)>1
    then FShortReadableString:=FShortReadableString+SMultiple
    else FShortReadableString:=FShortReadableString+ExtractFileName(snd.Filename);
  FShortReadableString:=FShortReadableString+' '+STo+' '+VolumeToStringPercent(TBVol.Percent);
  FCmdDuration:=FSE1.Value;
end;

procedure TFrameCmdAudio.GenerateCmdForPanFile;
var i: integer;
  snd: TALSSound;
begin
  FCmds:=CmdTitleAudioChangePan;

  for i:=0 to High(FSelectedSounds) do begin
    snd:=FSelectedSounds[i];
    FCmds.ConcatCmd(CmdAudioChangePan(snd.Tag, GetPan, FSE2.Value, Frame_Velocity1.SelectedCurveID));
  end;

  FShortReadableString:=SAudioSetPan+' ';
  if Length(FSelectedSounds)>1
    then FShortReadableString:=FShortReadableString+SMultiple
    else FShortReadableString:=FShortReadableString+ExtractFileName(snd.Filename);
  FShortReadableString:=FShortReadableString+' '+STo+' '+PanToStringPercent(TBPan.Position/100);
  FCmdDuration:=FSE2.Value;
end;

procedure TFrameCmdAudio.GenerateCmdForPitchFile;
var i: integer;
  snd: TALSSound;
begin
  FCmds:=CmdTitleAudioChangePitch;

  for i:=0 to High(FSelectedSounds) do begin
    snd:=FSelectedSounds[i];
    FCmds.ConcatCmd(CmdAudioChangePitch(snd.Tag, GetPitch, FSE3.Value, Frame_Velocity1.SelectedCurveID));
  end;

  FShortReadableString:=SAudioSetFreq+' ';
  if Length(FSelectedSounds)>1
    then FShortReadableString:=FShortReadableString+SMultiple
    else FShortReadableString:=FShortReadableString+ExtractFileName(snd.Filename);
  FShortReadableString:=FShortReadableString+' '+STo+' '+PitchToString(TBPitch.Position);
  FCmdDuration:=FSE3.Value;
end;

procedure TFrameCmdAudio.GenerateCmdForStartCapture;
begin
  FCmds := CmdAudioCaptureStart;
  FShortReadableString:=SAudioCaptureStart;
  FCmdDuration := 0;
end;

procedure TFrameCmdAudio.GenerateCmdForStopCapture;
begin
  FCmds := CmdAudioCaptureStop;
  FShortReadableString:=SAudioCaptureStop;
  FCmdDuration := 0;
end;

procedure TFrameCmdAudio.GenerateCmdForVolumeCapture;
begin
  FCmds:=CmdAudioCaptureChangeVolume(GetVolume, FSE1.Value, Frame_Velocity1.SelectedCurveID);
  FShortReadableString:=SAudioCaptureSetVolume+' '+STo+' '+VolumeToStringPercent(TBVol.Percent);
  FCmdDuration := FSE1.Value;
end;

procedure TFrameCmdAudio.GenerateCmdForPanCapture;
begin
  FCmds:=CmdAudioCaptureChangePan(GetPan, FSE1.Value, Frame_Velocity1.SelectedCurveID);
  FShortReadableString:=SAudioCaptureSetPan+' '+STo+' '+PanToStringPercent(TBPan.Position/100);
  FCmdDuration := FSE1.Value;
end;

function TFrameCmdAudio.GetVolume: single;
begin
  Result := TBVol.Position/TBVol.Max;
end;

function TFrameCmdAudio.GetPan: single;
begin
  Result := TBPan.Position/100;
end;

function TFrameCmdAudio.GetPitch: single;
begin
  Result := TBPitch.Position/100;
end;

procedure TFrameCmdAudio.UpdateVolumeLabel;
begin
  LabelVolume.Caption := SVolume+' '+VolumeToStringPercent( GetVolume );
end;

procedure TFrameCmdAudio.UpdatePanLabel;
begin
  LabelPan.Caption := SPan+' '+PanToStringPercent(GetPan);
end;

procedure TFrameCmdAudio.UpdatePitchLabel;
begin
  LabelPitch.Caption := SPitch+' '+PitchToString( GetPitch );
end;

procedure TFrameCmdAudio.UpdatePanels;
begin
  // volume
  PanelVolume.Visible := (TargetIsFile and (FSelectedActionIndex in [0,3,6,7] )) or
        (not TargetIsFile and (FSelectedActionIndex = 2));
  if PanelVolume.Visible then begin
    LabelVolume.Visible := not (TargetIsFile and FFileSBManager.Checked[SpeedButton10]);
    TBVol.Visible := LabelVolume.Visible;
    lblVol1.Visible := FFileSBManager.Checked[SpeedButton6] or
                       FFileSBManager.Checked[SpeedButton9];
    FSE1.Visible := lblVol1.Visible;
    lblattendre9.Visible := lblVol1.Visible;
  end;

  // panning
  PanelPan.Visible := (TargetIsFile and (FSelectedActionIndex in [0,4])) or
         (not TargetIsFile and (FSelectedActionIndex = 3));
  if PanelPan.Visible then begin
    lblVol3.Visible := FFileSBManager.Checked[SpeedButton7];
    FSE2.Visible := lblVol3.Visible;
    lblattendre11.Visible := lblVol3.Visible;
  end;

  // pitch
  PanelPitch.Visible := TargetIsFile and (FSelectedActionIndex=5);

  // velocity curve
  PanelCurve.Visible := (TargetIsFile and (FSelectedActionIndex in [3..7])) or
          (not TargetIsFile and (FSelectedActionIndex in [2,3]));
end;

procedure TFrameCmdAudio.ProcessSourceChangeEvent(Sender: TObject);
begin
  SoundManager.StopAllSound;
  SoundManager.DeleteAllEffects;

  if not TargetIsFile then
  begin
    SoundManager.StartCaptureToPlayback;
    if SoundManager.CaptureToPlaybackIsReady then
      Label13.Caption := 'Ready'
    else
      Label13.Caption := SoundManager.CaptureStrError;
  end;

  UpdatePanels;
end;

procedure TFrameCmdAudio.StartPlayback(aFromBegining: boolean);
begin
  if TargetIsFile then
    FrameViewAudioList1.PlaySelected(aFromBegining)
  else
    SoundManager.StartCaptureToPlayback;
end;

function TFrameCmdAudio.TargetIsFile: boolean;
begin
  Result := NoteBook1.Page[NoteBook1.PageIndex] = PageFile;
end;

constructor TFrameCmdAudio.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FrameViewAudioList1:=TFrameViewAudioList.Create(Self);
  FrameViewAudioList1.Parent:=Panel2;
  FrameViewAudioList1.Align:=alClient;
  FrameViewAudioList1.MultiSelect:=FALSE;
  FrameViewAudioList1.OnSelectionChange:=@ProcessSelectionChange;

  Frame_Velocity1:=TFrame_Velocity.Create(Self);
  Frame_Velocity1.Parent:=Panel3;
  Frame_Velocity1.Align:=alClient;

  FNoteBookManager := TNoteBookManager.Create(Notebook1);
  with FNoteBookManager do
  begin
    SetActivatedColors($0003C4FC, clBlack);
    SetDeactivatedColors($00484848, $00EAEAEA);
    LinkButtonToPage(SpeedButton4, PageFile);
    LinkButtonToPage(SpeedButton5, PageCapture);
    ActivePage( PageFile );
    OnSelectionChange := @ProcessSourceChangeEvent;
  end;

  FFileSBManager:=TToggleSpeedButtonManager.Create;
  with FFileSBManager do
  begin
    ToggleType := tsbLikeRadioButton;
    SetActivatedColors($0003C4FC, $00151515);
    SetDeactivatedColors($00556666, $00EAEAEA);
    SetImageIndexes(3, -1);
    Add(SpeedButton1, True);
    Add(SpeedButton2, FALSE);
    Add(SpeedButton3, FALSE);
    Add(SpeedButton6, FALSE);
    Add(SpeedButton7, FALSE);
    Add(SpeedButton8, FALSE);
    Add(SpeedButton9, FALSE);
    Add(SpeedButton10, FALSE);
  end;

  FCaptureSBManager:=TToggleSpeedButtonManager.Create;
  with FCaptureSBManager do
  begin
    ToggleType := tsbLikeRadioButton;
    SetActivatedColors($0003C4FC, $00151515);
    SetDeactivatedColors($00556666, $00EAEAEA);
    SetImageIndexes(3, -1);
    Add(SpeedButton11, True);
    Add(SpeedButton12, FALSE);
    Add(SpeedButton13, FALSE);
    Add(SpeedButton14, FALSE);
  end;

  FSelectedActionIndex := 0;
  UpdatePanels;


  FUseCmdTitle:=TRUE;
end;

destructor TFrameCmdAudio.Destroy;
begin
  FNoteBookManager.Free;
  FFileSBManager.Free;
  FCaptureSBManager.Free;
  inherited Destroy;
end;

procedure TFrameCmdAudio.EraseBackground(DC: HDC);
begin
// do nothing here
end;

procedure TFrameCmdAudio.Init;
begin
  Frame_Velocity1.UpdateList;
  FrameViewAudioList1.Fill;

  UpdateVolumeLabel;
  UpdatePanLabel;
  UpdatePitchLabel;
  lblVol3.Caption := SIn;
  lblattendre11.Caption := SSeconds_;
  lblVol4.Caption := SIn;
  lblattendre13.Caption := SSeconds_;
  LabelCurve.Caption := SCurve_;
  lblVol1.Caption := SIn;
  lblattendre9.Caption := SSeconds_;
  BPitchNormal.Caption := SNormal;
  BPanNormal.Caption := SCenter_;
  SpeedButton4.Caption := SFiles_;
  SpeedButton5.Caption := SAudioCapture_;
  BAdd.Caption := SAdd;
end;

procedure TFrameCmdAudio.Sel_None;
begin
  FrameViewAudioList1.Sel_None;
end;

procedure TFrameCmdAudio.BListenClick(Sender: TObject);
begin
  StartPlayback(True);
end;

procedure TFrameCmdAudio.BPanNormalClick(Sender: TObject);
begin
 TBPan.Position := 0;
 FrameViewAudioList1.SetPanOnSelected(0);
end;

procedure TFrameCmdAudio.BStopAllClick(Sender: TObject);
begin
 SoundManager.StopAllSound;
 SoundManager.DeleteAllEffects;
end;

procedure TFrameCmdAudio.BAddClick(Sender: TObject);
begin
  if TargetIsFile then
  begin
    if FrameViewAudioList1.SelectedCount = 0 then
      exit;

    FSelectedSounds := FrameViewAudioList1.GetSelected;

    case FSelectedActionIndex of
      0: GenerateCmdForPlayFile;
      1: GenerateCmdForPauseFile;
      2: GenerateCmdForStopFile;
      3: GenerateCmdForVolumeFile;
      4: GenerateCmdForPanFile;
      5: GenerateCmdForPitchFile;
      6: GenerateCmdForFadeInFile;
      7: GenerateCmdForFadeOutFile;
    end;
  end
  else
  begin
    case FSelectedActionIndex of
      0: GenerateCmdForStartCapture;
      1: GenerateCmdForStopCapture;
      2: GenerateCmdForVolumeCapture;
      3: GenerateCmdForPanCapture;
    end;
  end;

  SoundManager.StopAllSound;
  SoundManager.DeleteAllEffects;
  DoOnAddCmd;
end;

procedure TFrameCmdAudio.BPitchNormalClick(Sender: TObject);
begin
 TBPitch.Position := 100;
 FrameViewAudioList1.SetNormalPitchOnSelected;
end;

end.

