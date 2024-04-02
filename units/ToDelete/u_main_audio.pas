unit u_main_audio;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Graphics, LCLType, ExtCtrls,
  Buttons, ComCtrls, Dialogs, Menus, LCLTranslator,
  ALSound, frame_viewaudiolist,
  u_audio_manager,
  frame_audiocapture;

type

  { TFormMainAudio }

  TFormMainAudio = class(TForm)
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
    MIRemoveEffect: TMenuItem;
    MILoop: TMenuItem;
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
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    TBPitch: TTrackBar;
    TBPan: TTrackBar;
    TBVolume: TTrackBar;
    Timer1: TTimer;
    procedure BPanCenterClick(Sender: TObject);
    procedure BPitchNormalClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MILoopClick(Sender: TObject);
    procedure MIRemoveEffectClick(Sender: TObject);
    procedure MI_DeleteClick(Sender: TObject);
    procedure Panel5Resize(Sender: TObject);
    procedure SB_fadein1sClick(Sender: TObject);
    procedure SB_fadeout1sClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure TBPitchChange(Sender: TObject);
    procedure TBPanChange(Sender: TObject);
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
    procedure EraseBackground({%H-}DC: HDC); override;


    // call it when user change between EDIT and SHOW mode
    procedure UpdateEditMode;

    procedure Fill;

    property MouseIsOver: boolean read GetMouseIsOver;
  end;

var
  FormMainAudio: TFormMainAudio;

implementation
uses u_utils, u_resource_string, u_project_manager, u_common, u_userdialogs,
  u_mainform, VelocityCurve, Math;

{$R *.lfm}

{ TFormMainAudio }

procedure TFormMainAudio.FormCreate(Sender: TObject);
begin
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

procedure TFormMainAudio.BPanCenterClick(Sender: TObject);
var snd: TALSSound;
begin
 snd := SelectedSound;
 if snd <> NIL then
   snd.Pan.Value := ALS_PAN_CENTER
 else
   TBPan.Position := 0;
end;

procedure TFormMainAudio.BPitchNormalClick(Sender: TObject);
var snd: TALSSound;
begin
 snd := SelectedSound;
 if snd <> NIL then
   snd.Pitch.Value := ALS_PITCH_NORMAL
 else
   TBPitch.Position := 100;
end;

procedure TFormMainAudio.FormActivate(Sender: TObject);
begin
  RedockPanels;
end;

procedure TFormMainAudio.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key in [VK_F1, VK_F2, VK_F3] then
   FormMain.FormKeyDown(Self, Key, Shift)
  else
    FrameViewAudioList1.ProcessKeyDown(Key, Shift);
end;

procedure TFormMainAudio.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  FrameViewAudioList1.ProcessKeyUp(Key, Shift);
end;

procedure TFormMainAudio.FormResize(Sender: TObject);
begin
  Panel5.Width := Round(ClientWidth*0.3);
end;

procedure TFormMainAudio.FormShow(Sender: TObject);
begin
  Enabled := Project.IsReady;

  FrameViewAudioList1.Fill;
  FrameAudioCapture1.Fill;

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

  MI_Delete.Caption := SDelete;
end;

procedure TFormMainAudio.MILoopClick(Sender: TObject);
begin
  if FrameViewAudioList1.SelectedCount = 0 then
    exit;

  FrameViewAudioList1.ToogleLoopOnSelected;
  Project.Save;
end;

procedure TFormMainAudio.MIRemoveEffectClick(Sender: TObject);
begin
  if FrameViewAudioList1.SelectedCount = 0 then
    exit;

  FrameViewAudioList1.RemoveEffectsOnSelected;
end;

procedure TFormMainAudio.MI_DeleteClick(Sender: TObject);
begin
 if FrameViewAudioList1.SelectedCount = 0 then
   exit;

 if AskConfirmation(SRemoveAudioFileFromProject + lineending +
                 SFileWillBeDeletedFromProjectStorage,
                 SOk, SCancel, mtWarning) = mrOk then
 begin
   FrameViewAudioList1.DeleteSelected;
   Project.Save;
 end;
end;

procedure TFormMainAudio.Panel5Resize(Sender: TObject);
begin
  RedockPanels;
end;

procedure TFormMainAudio.SB_fadein1sClick(Sender: TObject);
var snd: TALSSound;
 b: TSpeedButton;
begin
  b := Sender as TSpeedButton;
  snd := SelectedSound;
  if snd <> NIL
    then snd.FadeIn(1.0, b.Tag, ALS_StartFastEndSlow);
end;

procedure TFormMainAudio.SB_fadeout1sClick(Sender: TObject);
var snd: TALSSound;
 b: TSpeedButton;
begin
  b := Sender as TSpeedButton;
  snd := SelectedSound;
  if snd <> NIL
    then snd.FadeOut(b.Tag, ALS_StartFastEndSlow);
end;

procedure TFormMainAudio.SpeedButton1Click(Sender: TObject);
var i, j: integer;
 snd: TALSSound;
 s, source, target, newPath:string;
 flag: boolean;
begin
 OpenDialog1.Filter := ALSManager.DialogFileFilters(True);
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
       snd := SoundManager.AddStream( newPath );
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

procedure TFormMainAudio.SpeedButton2Click(Sender: TObject);
begin
  SoundManager.StopAllSound;
  SoundManager.DeleteAllEffects;
end;

procedure TFormMainAudio.TBPitchChange(Sender: TObject);
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

procedure TFormMainAudio.TBPanChange(Sender: TObject);
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

procedure TFormMainAudio.TBVolumeChange(Sender: TObject);
var snd: TALSSound;
 v: single;
begin
  v := CursorToVolume;
  lbl_volume.Caption := VolumeToStringPercent(v);

  if Sender = TBVolume then
  begin
    snd := SelectedSound;
    if snd <> NIL then
      snd.Volume.Value := v;
  end;
end;

procedure TFormMainAudio.Timer1Timer(Sender: TObject);
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

function TFormMainAudio.GetMouseIsOver: boolean;
begin
  Result := FrameViewAudioList1.MouseIsOver;
end;

function TFormMainAudio.SelectedSound: TALSSound;
begin
  Result := FrameViewAudioList1.FirstSelected;
end;

procedure TFormMainAudio.ProcessSoundRightClick(Sender: TObject);
begin
  if not Project.Options.EditMode then
    exit;

  PopupMenu1.PopUp;
end;

function TFormMainAudio.CursorToVolume: single;
begin
  Result := TBVolume.Position/TBVolume.Max;
  Result := Result*Result;
end;

procedure TFormMainAudio.VolumeToCursor(AValue: single);
begin
  TBVolume.Position := Round(Sqrt(AValue)*TBVolume.Max);
end;

function TFormMainAudio.CursorToPan: single;
begin
  Result := TBPan.Position*0.01;
end;

procedure TFormMainAudio.PanToCursor(AValue: single);
begin
  TBPan.Position := Round(AValue*100);
end;

function TFormMainAudio.CursorToPitch: single;
begin
  Result := TBPitch.Position*0.01;
end;

procedure TFormMainAudio.PitchToCursor(AValue: single);
begin
  TBPitch.Position := Round(AValue*100);
end;

procedure TFormMainAudio.RedockPanels;
var r: TRect;
  v, defaultHeight, margin: integer;
begin
  defaultHeight := Round(GetCanvasScaleFactor*230);
  margin := Round(GetCanvasScaleFactor*5);

  // re-arrange capture and sound control panel
  if Panel5.ClientRect.Width >= 500 then
  begin
    //place the two panel horizontaly
    Panel5.Height := defaultHeight;
    r := Panel5.ClientRect;
    v := r.CenterPoint.X - margin div 2;
    Panel2.SetBounds(r.Left+margin, r.Top+margin, v-(r.Left+margin), r.Bottom-margin);
    Panel1.SetBounds(v+margin, r.Top+margin, r.Right-(v+margin*2), r.Bottom-margin);
  end
  else begin
    // place the two panel vertically
    Panel5.Height := Min(FormMainAudio.ClientHeight div 3*2+15, defaultHeight*2+15);
    r := Panel5.ClientRect;
    v := r.CenterPoint.Y - margin div 2;
    Panel1.SetBounds(r.Left+margin, r.Top+margin, r.Right-(r.Left+margin*2), v-(r.Top+margin));
    Panel2.SetBounds(r.Left+margin, v+margin, r.Right-(r.Left+margin*2), r.Bottom-(v+margin*2));
  end;
end;

procedure TFormMainAudio.EraseBackground(DC: HDC);
begin
end;

procedure TFormMainAudio.UpdateEditMode;
begin
  // FrameViewAudioList1.MouseCanMoveItem := Project.Prefs.EditMode;
   FrameViewAudioList1.MultiSelect := Project.Options.EditMode;

   SpeedButton1.Visible := Project.Options.EditMode;
end;

procedure TFormMainAudio.Fill;
begin
  FrameViewAudioList1.Fill;
  FrameAudioCapture1.Fill;

  Label3.Caption := SVolume;
  Label4.Caption := SPan;
  Label20.Caption := SPitch;
  BPanCenter.Caption := SCenter_;
  BPitchNormal.Caption := SNormal_;
  MI_Delete.Caption := SDelete;
  MILoop.Caption := SLoop;
end;

end.

