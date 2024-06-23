unit frame_main_audio;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Graphics, LCLType, ExtCtrls,
  Buttons, ComCtrls, Dialogs, Menus, LCLTranslator,
  ALSound, frame_viewaudiolist,
  u_audio_manager,
  frame_audiocapture;

type

  { TFrameMainAudio }

  TFrameMainAudio = class(TFrame)
    BPanCenter: TSpeedButton;
    BPitchNormal: TSpeedButton;
    Label20: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    lbl_freq: TLabel;
    lbl_pan: TLabel;
    lbl_volume: TLabel;
    MILoop: TMenuItem;
    MIRemoveEffect: TMenuItem;
    MI_Delete: TMenuItem;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    PopupMenu1: TPopupMenu;
    SB_fadein10s: TSpeedButton;
    SB_fadein1s: TSpeedButton;
    SB_fadein4s: TSpeedButton;
    SB_fadein7s: TSpeedButton;
    SB_fadeout10s: TSpeedButton;
    SB_fadeout1s: TSpeedButton;
    SB_fadeout4s: TSpeedButton;
    SB_fadeout7s: TSpeedButton;
    Separator1: TMenuItem;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    TBPan: TTrackBar;
    TBPitch: TTrackBar;
    TBVolume: TTrackBar;
    Timer1: TTimer;
    procedure BPanCenterClick(Sender: TObject);
    procedure BPitchNormalClick(Sender: TObject);
    procedure MILoopClick(Sender: TObject);
    procedure MIRemoveEffectClick(Sender: TObject);
    procedure MI_DeleteClick(Sender: TObject);
    procedure Panel5Resize(Sender: TObject);
    procedure SB_fadein1sClick(Sender: TObject);
    procedure SB_fadeout1sClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure TBPanChange(Sender: TObject);
    procedure TBPitchChange(Sender: TObject);
    procedure TBVolumeChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    function GetMouseIsOver: boolean;
    function SelectedSound: TALSSound;
    procedure ProcessSoundRightClick(Sender: TObject);

    function CursorToVolume: single;
    procedure VolumeToCursor(AValue: single);
    function CursorToPan: single;
    procedure PanToCursor(AValue: single);
    function CursorToPitch: single;
    procedure PitchToCursor(AValue: single);
    procedure RedockPanels;
  public
    FrameAudioCapture1: TFrameAudioCapture;
    FrameViewAudioList1: TFrameViewAudioList;

    constructor Create(aOwner: TComponent); override;
    procedure EraseBackground({%H-}DC: HDC); override;

    function MouseIsOver: boolean;
    procedure ProcessKeyDown(Key: Word; Shift: TShiftState);
    procedure ProcessKeyUp(Key: Word; Shift: TShiftState);

    procedure Fill;

    // call it when user change between EDIT and SHOW mode
    procedure UpdateEditMode;
    // call it when user change the visibility of capture panel, control panel
    procedure UpdateLayout;
    procedure UpdateStringAfterLanguageChange;
  end;

implementation
uses u_utils, u_resource_string, u_project_manager, u_common, u_userdialogs,
  u_mainform, VelocityCurve, Math;

{$R *.lfm}

{ TFrameMainAudio }

procedure TFrameMainAudio.BPanCenterClick(Sender: TObject);
var snd: TALSSound;
begin
 snd := SelectedSound;
 if snd <> NIL then
   snd.Pan.Value := ALS_PAN_CENTER
 else
   TBPan.Position := 0;
end;

procedure TFrameMainAudio.BPitchNormalClick(Sender: TObject);
var snd: TALSSound;
begin
 snd := SelectedSound;
 if snd <> NIL then
   snd.Pitch.Value := ALS_PITCH_NORMAL
 else
   TBPitch.Position := 100;
end;

procedure TFrameMainAudio.MILoopClick(Sender: TObject);
begin
  if FrameViewAudioList1.SelectedCount = 0 then
    exit;

  FrameViewAudioList1.ToogleLoopOnSelected;
  Project.Save;
end;

procedure TFrameMainAudio.MIRemoveEffectClick(Sender: TObject);
begin
  if FrameViewAudioList1.SelectedCount = 0 then
    exit;

  FrameViewAudioList1.RemoveEffectsOnSelected;
end;

procedure TFrameMainAudio.MI_DeleteClick(Sender: TObject);
begin
  if FrameViewAudioList1.SelectedCount = 0 then
    exit;

  if AskConfirmation(SRemoveAudioFileFromProject + lineending +
                  SFileWillBeDeletedFromProjectStorage,
                  SOk, SCancel, mtWarning) = mrOk then
  begin
    FrameViewAudioList1.DeleteSelected;
    Project.Save;
    FormMain.CheckSequenceError;
  end;
end;

procedure TFrameMainAudio.Panel5Resize(Sender: TObject);
begin
  RedockPanels;
end;

procedure TFrameMainAudio.SB_fadein1sClick(Sender: TObject);
var snd: TALSSound;
 b: TSpeedButton;
begin
  b := Sender as TSpeedButton;
  snd := SelectedSound;
  if snd <> NIL
    then snd.FadeIn(1.0, b.Tag, ALS_StartFastEndSlow);
end;

procedure TFrameMainAudio.SB_fadeout1sClick(Sender: TObject);
var snd: TALSSound;
 b: TSpeedButton;
begin
  b := Sender as TSpeedButton;
  snd := SelectedSound;
  if snd <> NIL
    then snd.FadeOut(b.Tag, ALS_StartFastEndSlow);
end;

procedure TFrameMainAudio.SpeedButton1Click(Sender: TObject);
var i, j: integer;
 snd: TALSSound;
 s, source, target, newPath: string;
 flag: boolean;
begin
 OpenDialog1.Filter := ALSManager.DialogFileFilters(SAudioFiles, SAllFiles);
 if OpenDialog1.Execute then
 begin
   Panel6.Visible:=TRUE;
   flag := FALSE;
   FrameViewAudioList1.LB.LockSelectionChange;
   for i:=0 to OpenDialog1.Files.count-1 do begin
     Label6.Caption := inttostr(i+1)+'/'+inttostr(OpenDialog1.Files.Count);
     Application.ProcessMessages;

     source := OpenDialog1.Files.Strings[i];
     target := ExtractFileName(source);
     target := ChangeFileExt(target, '');
     target := ReplaceForbidenCharByUnderscore(target, FORBIDENCHARS);
     target := ChangeFileExt(target, ExtractFileExt(source));

     if Project.AudioStorage.IsStored( target ) then begin
       // the file already exists, we add a prefix number to its name to diferentiate them
       j := 0;
       repeat
        inc(j);
        s := ChangeFileExt(target, '');
        s := s+'_'+j.ToString;
        s := ChangeFileExt(s, ExtractFileExt(target));
       until not Project.AudioStorage.IsStored( s );
        // notify if the file already exists in the project
        ShowMess(STheFile+' "'+target+'" '+SIsAlreadyImported+
             lineending+SItsNameIsReplacedBy+' "'+s+'"', SOk, mtWarning);
        target := s;
     end;

     // make a copy of the file into audio project folder
     if Project.AudioStorage.StoreFileExt( source, target, newPath ) then
     begin
       snd := SoundManager.AddStream( newPath ); // construct the peak file
       FrameViewAudioList1.LB.ItemIndex := FrameViewAudioList1.LB.Items.Add( snd.Tag.ToString );
       flag := TRUE;
     end else ShowMess(SAnErrorOccurredWhileImportingTheFileToTheProject+
                 LINEENDING+newPath+LINEENDING+SDisqueIsFullOrWriteProtected, SOk, mtError);

   end;
   Panel6.Visible := FALSE;
   FrameViewAudioList1.LB.UnlockSelectionChange;
   if flag then
     Project.Save;
 end;
 FrameViewAudioList1.LB.SetFocus;
end;

procedure TFrameMainAudio.SpeedButton2Click(Sender: TObject);
begin
 SoundManager.StopAllSound(False);
 SoundManager.DeleteAllEffects(False);
end;

procedure TFrameMainAudio.TBPanChange(Sender: TObject);
var snd: TALSSound;
 v: single;
begin
  v := CursorToPan;
  lbl_pan.Caption := PanToStringPercent(v);

  if Sender = TBPan then
  begin
    snd := SelectedSound;
    if snd <> NIL then
      snd.Pan.Value := v;
  end;
end;

procedure TFrameMainAudio.TBPitchChange(Sender: TObject);
var snd: TALSSound;
 v: single;
begin
  v := CursorToPitch;
  lbl_freq.Caption := PitchToString(v);

  if Sender = TBPitch then
  begin
    snd := SelectedSound;
    if snd <> NIL then
      snd.Pitch.Value := v;
  end;
end;

procedure TFrameMainAudio.TBVolumeChange(Sender: TObject);
var snd: TALSSound;
 v: single;
begin
  v := CursorToVolume;
  lbl_volume.Caption := VolumeToStringPercent(v);

  if Sender = TBVolume then begin
    snd := SelectedSound;
    if snd <> NIL then
      snd.Volume.Value := v;
  end;
end;

procedure TFrameMainAudio.Timer1Timer(Sender: TObject);
var snd: TALSSound;
 callback: TNotifyEvent;
begin
  Timer1.Enabled:=FALSE;
  // update the position of the trackbar for the selected audio
  snd := SelectedSound;
  if snd <> NIL then
  begin
    // volume
    callback := TBVolume.OnChange;
    TBVolume.OnChange := NIL;
    VolumeToCursor(snd.Volume.Value);
    TBVolume.OnChange := callback;
    lbl_volume.Caption := VolumeToStringPercent(snd.Volume.Value);

    // pan
    callback := TBPan.OnChange;
    TBPan.OnChange := NIL;
    PanToCursor(snd.Pan.Value);
    TBPan.OnChange := callback;
    lbl_pan.Caption := PanToStringPercent(snd.Pan.Value);

    // frequency
    callback := TBPitch.OnChange;
    TBPitch.OnChange := NIL;
    PitchToCursor(snd.Pitch.Value);
    TBPitch.OnChange := callback;
    lbl_freq.Caption := PitchToString(snd.Pitch.Value);
  end;

  Timer1.Enabled:=TRUE;
end;

function TFrameMainAudio.GetMouseIsOver: boolean;
begin
  Result := FrameViewAudioList1.MouseIsOver;
end;

function TFrameMainAudio.SelectedSound: TALSSound;
begin
  Result := FrameViewAudioList1.FirstSelected;
end;

procedure TFrameMainAudio.ProcessSoundRightClick(Sender: TObject);
begin
  if not Project.Options.EditMode then
    exit;

  PopupMenu1.PopUp;
end;

function TFrameMainAudio.CursorToVolume: single;
begin
  Result := TBVolume.Position/TBVolume.Max;
  Result := Result*Result;
end;

procedure TFrameMainAudio.VolumeToCursor(AValue: single);
begin
  TBVolume.Position := Round(Sqrt(AValue)*TBVolume.Max);
end;

function TFrameMainAudio.CursorToPan: single;
begin
  Result := TBPan.Position*0.01;
end;

procedure TFrameMainAudio.PanToCursor(AValue: single);
begin
  TBPan.Position := Round(AValue*100);
end;

function TFrameMainAudio.CursorToPitch: single;
begin
  Result := TBPitch.Position*0.01;
end;

procedure TFrameMainAudio.PitchToCursor(AValue: single);
begin
  TBPitch.Position := Round(AValue*100);
end;

procedure TFrameMainAudio.RedockPanels;
var r: TRect;
  v, defaultHeight, margin: integer;
begin
  defaultHeight := ScaleDesignToForm(230);
  margin := ScaleDesignToForm(5);

  if not Project.Options.MainViewShowAudioCapturePanel and
     not Project.Options.MainViewShowAudioControlPanel then begin
    // hide both sound capture and sound control panels
    Panel5.Height := 0;
  end else
  if Project.Options.MainViewShowAudioCapturePanel and
     Project.Options.MainViewShowAudioControlPanel then begin
    Panel2.Visible := True;
    Panel1.Visible := True;
    // show both capture and control panels
    if Panel5.ClientRect.Width >= ScaleDesignToForm(500) then begin
      //place the two panel horizontaly
      Panel5.Height := defaultHeight;
      r := Panel5.ClientRect;
      v := r.CenterPoint.X - margin div 2;
      Panel2.SetBounds(r.Left+margin, r.Top+margin, v-(r.Left+margin), r.Bottom-margin);
      Panel1.SetBounds(v+margin, r.Top+margin, r.Right-(v+margin*2), r.Bottom-margin);
    end else begin
      // place the two panel vertically
      Panel5.Height := Min(ClientHeight*2 div 3+3*margin, defaultHeight*2+3*margin);
      r := Panel5.ClientRect;
      v := r.CenterPoint.Y - margin div 2;
      Panel1.SetBounds(r.Left+margin, r.Top+margin, r.Right-(r.Left+margin*2), v-(r.Top+margin));
      Panel2.SetBounds(r.Left+margin, v+margin, r.Right-(r.Left+margin*2), r.Bottom-(v+margin*2));
    end;
  end else
  if not Project.Options.MainViewShowAudioCapturePanel and
     Project.Options.MainViewShowAudioControlPanel then begin
    // show only sound control panel
    Panel2.Visible := False;
    Panel1.Visible := True;
    Panel5.Height := defaultHeight;
    r := Panel5.ClientRect;
    v := Min(ScaleDesignToForm(375){default panel width}, Panel5.ClientWidth);
    Panel1.SetBounds(r.CenterPoint.X-v div 2, r.Top+margin, v, r.Height-margin*2);
  end else begin
    // show only sound capture panel
    Panel2.Visible := True;
    Panel1.Visible := False;
    Panel5.Height := defaultHeight;
    r := Panel5.ClientRect;
    v := Min(ScaleDesignToForm(375){default panel width}, Panel5.ClientWidth);
    Panel2.SetBounds(r.CenterPoint.X-v div 2, r.Top+margin, v, r.Height-margin*2);
  end;
end;

constructor TFrameMainAudio.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FrameViewAudioList1 := TFrameViewAudioList.Create(Self);
  FrameViewAudioList1.Parent := Panel3;
  FrameViewAudioList1.Align := alClient;
  FrameViewAudioList1.ItemHeight := Round(30*GetCanvasScaleFactor);
  FrameViewAudioList1.MultiSelect := FALSE;
  FrameViewAudioList1.OnItemRightClick := @ProcessSoundRightClick;

  FrameAudioCapture1 := TFrameAudioCapture.Create(Self);
  FrameAudioCapture1.Parent := Panel2;
  FrameAudioCapture1.Align := alClient;
end;

procedure TFrameMainAudio.EraseBackground(DC: HDC);
begin
  inherited EraseBackground(DC);
end;

function TFrameMainAudio.MouseIsOver: boolean;
begin
  Result := FrameViewAudioList1.MouseIsOver;
end;

procedure TFrameMainAudio.ProcessKeyDown(Key: Word; Shift: TShiftState);
begin
  FrameViewAudioList1.ProcessKeyDown(Key, Shift);
end;

procedure TFrameMainAudio.ProcessKeyUp(Key: Word; Shift: TShiftState);
begin
  FrameViewAudioList1.ProcessKeyUp(Key, Shift);
end;

procedure TFrameMainAudio.Fill;
begin
  FrameViewAudioList1.Fill;
  FrameAudioCapture1.Fill;

  Enabled := Project.IsReady;

  SB_fadein4s.Hint := SB_fadein1s.Hint;
  SB_fadein7s.Hint := SB_fadein1s.Hint;
  SB_fadein10s.Hint := SB_fadein1s.Hint;

  SB_fadeout4s.Hint := SB_fadeout1s.Hint;
  SB_fadeout7s.Hint := SB_fadeout1s.Hint;
  SB_fadeout10s.Hint := SB_fadeout1s.Hint;

  // show cursor's values
  TBVolumeChange(NIL);
  TBPanChange(NIL);
  TBpitchChange(NIL);

  UpdateStringAfterLanguageChange;

  RedockPanels;
end;

procedure TFrameMainAudio.UpdateEditMode;
begin
  // FrameViewAudioList1.MouseCanMoveItem := Project.Prefs.EditMode;
   FrameViewAudioList1.MultiSelect := Project.Options.EditMode;

   SpeedButton1.Visible := Project.Options.EditMode;
end;

procedure TFrameMainAudio.UpdateLayout;
begin
  RedockPanels;
end;

procedure TFrameMainAudio.UpdateStringAfterLanguageChange;
var v: Single;
begin
  FrameAudioCapture1.UpdateStringAfterLanguageChange;

  v := CursorToVolume;
  lbl_volume.Caption := VolumeToStringPercent(v);
  v := CursorToPan;
  lbl_pan.Caption := PanToStringPercent(v);
  v := CursorToPitch;
  lbl_freq.Caption := PitchToString(v);

  Label3.Caption := SVolume;
  Label4.Caption := SPan;
  Label20.Caption := SPitch;
  BPanCenter.Hint := SCenter_;
  BPitchNormal.Hint := SNormal_;

  MI_Delete.Caption := SDelete;
  MILoop.Caption := SLoop;
end;

end.

