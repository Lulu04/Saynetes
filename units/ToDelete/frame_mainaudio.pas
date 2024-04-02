unit frame_mainaudio;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Graphics, LCLType,
  ExtCtrls, Buttons, ComCtrls, Dialogs, Menus,
  ALSound, frame_viewaudiolist, u_audio_manager;

type

  { TFrameMainAudio }

  TFrameMainAudio = class(TFrame)
    Label1: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lbl_freq: TLabel;
    lbl_pan: TLabel;
    lbl_volume: TLabel;
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
    BPitchNormal: TSpeedButton;
    BPanCenter: TSpeedButton;
    Shape3: TShape;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton5: TSpeedButton;
    TBPitch: TTrackBar;
    TB_Audiopan: TTrackBar;
    TB_Audiovolume: TTrackBar;
    Timer1: TTimer;
    procedure FrameResize(Sender: TObject);
    procedure MI_DeleteClick(Sender: TObject);
    procedure SB_fadein1sClick(Sender: TObject);
    procedure SB_fadeout1sClick(Sender: TObject);
    procedure BPitchNormalClick(Sender: TObject);
    procedure BPanCenterClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure TBPitchChange(Sender: TObject);
    procedure TB_AudiopanChange(Sender: TObject);
    procedure TB_AudiovolumeChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    function GetMouseIsOver: boolean;
    function SelectedSound: TALSSound;
    procedure ProcessSoundRightClick(Sender: TObject);
  private
    FCaptureFlash: integer;
  public
    FrameViewAudioList1: TFrameViewAudioList;
    constructor Create(TheOwner: TComponent); override;
    procedure EraseBackground({%H-}DC: HDC); override;

    procedure ProcessKeyDown(var Key: Word; {%H-}Shift: TShiftState);
    procedure ProcessKeyUp(var Key: Word; {%H-}Shift: TShiftState);
    procedure Fill;

    // call it when user change between EDIT and SHOW mode
    procedure UpdateEditMode;

    property MouseIsOver: boolean read GetMouseIsOver;
  end;

implementation
uses u_utils, u_resource_string, u_project_manager, u_common, u_userdialogs,
  VelocityCurve;

{$R *.lfm}

{ TFrameMainAudio }

procedure TFrameMainAudio.SB_fadein1sClick(Sender: TObject);
var o: TALSSound;
 b: TSpeedButton;
begin
  b:=Sender as TSpeedButton;
  o:=SelectedSound;
  if o<>NIL
    then o.FadeIn(1.0, b.Tag, ALS_StartFastEndSlow);
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
  end;
end;

procedure TFrameMainAudio.FrameResize(Sender: TObject);
begin
  Panel5.Width := Round(ClientWidth*0.3);
end;

procedure TFrameMainAudio.SB_fadeout1sClick(Sender: TObject);
var o: TALSSound;
 b: TSpeedButton;
begin
 b:=Sender as TSpeedButton;
  o:=SelectedSound;
  if o<>NIL
    then o.FadeOut(b.Tag, ALS_StartFastEndSlow);
end;

procedure TFrameMainAudio.BPitchNormalClick(Sender: TObject);
var o: TALSSound;
begin
 o:=SelectedSound;
 if o<>NIL
   then o.Pitch.Value := ALS_PITCH_NORMAL;
end;

procedure TFrameMainAudio.BPanCenterClick(Sender: TObject);
var o: TALSSound;
begin
 o:=SelectedSound;
 if o<>NIL
   then o.Pan.Value:=ALS_PAN_CENTER;
end;

procedure TFrameMainAudio.SpeedButton1Click(Sender: TObject);
var i, j: integer;
 o: TALSSound;
 s, source, target, newPath:string;
 flag: boolean;
begin
 OpenDialog1.Filter := ALSManager.DialogFileFilters(True);
 if OpenDialog1.Execute then
 begin
   Panel6.Visible:=TRUE;
   flag:=FALSE;
   FrameViewAudioList1.LB.LockSelectionChange;
   for i:=0 to OpenDialog1.Files.count-1 do begin
     Label6.Caption := inttostr(i+1)+'/'+inttostr(OpenDialog1.Files.Count);
     Application.ProcessMessages;

     source:=OpenDialog1.Files.Strings[i];
     target:=ExtractFileName(source);
     target:=ChangeFileExt(target, '');
     target:=ReplaceForbidenCharByUnderscore(target, FORBIDENCHARS);
     target:=ChangeFileExt(target, ExtractFileExt(source));

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
       o := SoundManager.AddStream( newPath );
       FrameViewAudioList1.LB.ItemIndex := FrameViewAudioList1.LB.Items.Add( o.Tag.ToString );
       flag:=TRUE;
     end else ShowMess(SAnErrorOccurredWhileImportingTheFileToTheProject+
                 LINEENDING+newPath+LINEENDING+SDisqueIsFullOrWriteProtected, SOk, mtError);

   end;
   Panel6.Visible:=FALSE;
   FrameViewAudioList1.LB.UnlockSelectionChange;
   if flag then Project.Save;
 end;
 FrameViewAudioList1.LB.SetFocus;
end;

procedure TFrameMainAudio.SpeedButton2Click(Sender: TObject);
begin
  SoundManager.ResetState;
end;

procedure TFrameMainAudio.SpeedButton3Click(Sender: TObject);
begin
  if SoundManager.CaptureToPlaybackIsReady then
    SoundManager.StopCaptureToPlayback
  else
    SoundManager.StartCaptureToPlayback;

  if SoundManager.CaptureToPlaybackIsReady then
  begin
    Label2.Caption := SOn;
    Label2.Font.Color := clBlack;
    Panel2.Color := $00845440;
    FCaptureFlash := 6;
  end
  else
  begin
    Label2.Caption := SOff;
    Label2.Font.Color := Label1.Font.Color;
    Panel2.Color := $00563729;
  end;
end;

procedure TFrameMainAudio.TBPitchChange(Sender: TObject);
var o: TALSSound;
begin
  lbl_freq.Caption:=PitchToString(TBPitch.Position/100);
  o:=SelectedSound;
  if o<>NIL
    then o.Pitch.Value:=TBPitch.Position/100;
end;

procedure TFrameMainAudio.TB_AudiopanChange(Sender: TObject);
var o: TALSSound;
begin
  lbl_pan.Caption:=PanToStringPercent(TB_Audiopan.Position/100);
  o:=SelectedSound;
  if o<>NIL
    then o.Pan.Value:=TB_Audiopan.Position*0.01;
end;

procedure TFrameMainAudio.TB_AudiovolumeChange(Sender: TObject);
var o: TALSSound;
 v: single;
begin
  v:=TB_Audiovolume.Position/TB_Audiovolume.Max;
  lbl_volume.Caption:=VolumeToStringPercent(v)+'%';
  o:=SelectedSound;
  if o<>NIL
    then o.Volume.Value:=v;
end;

procedure TFrameMainAudio.Timer1Timer(Sender: TObject);
var o: TALSSound;
 callback: TNotifyEvent;
begin
  Timer1.Enabled:=FALSE;
  // update the position of the trackbar for the selected audio
  o:=SelectedSound;
  if o<>NIL then
  begin
    // volume
    callback:=TB_Audiovolume.OnChange;
    TB_Audiovolume.OnChange:=NIL;
    TB_Audiovolume.Position:=Round(o.Volume.Value*TB_Audiovolume.Max);
    TB_Audiovolume.OnChange:=callback;
    lbl_volume.Caption:=VolumeToStringPercent(o.Volume.Value);
    // pan
    callback:=TB_Audiopan.OnChange;
    TB_Audiopan.OnChange:=NIL;
    TB_Audiopan.Position:=Round(o.Pan.Value*100);
    TB_Audiopan.OnChange:=callback;
    lbl_pan.Caption:=PanToStringPercent(o.Pan.Value);
    // frequency
    callback:=TBPitch.OnChange;
    TBPitch.OnChange:=NIL;
    TBPitch.Position:=Round(o.Pitch.Value*100);
    TBPitch.OnChange:=callback;
    lbl_freq.Caption:= PitchToString(o.Pitch.Value);
  end;

  // Capture panel
  if SoundManager.CaptureToPlaybackIsReady then
  begin
    dec(FCaptureFlash);
    if FCaptureFlash = 0 then
    begin
      FCaptureFlash := 6;
      case Label2.Tag of
       0: begin
         Label2.Tag := 1;
         Label2.Font.Color := $00C69C8A;
       end;
       1: begin
         Label2.Tag := 0;
         Label2.Font.Color := Panel2.Color;
       end;
      end;
    end;
  end;

  Timer1.Enabled:=TRUE;
end;

function TFrameMainAudio.SelectedSound: TALSSound;
begin
  Result:=FrameViewAudioList1.FirstSelected;
end;

procedure TFrameMainAudio.ProcessSoundRightClick(Sender: TObject);
begin
  if not Project.Prefs.EditMode then
    exit;

  PopupMenu1.PopUp;
end;

function TFrameMainAudio.GetMouseIsOver: boolean;
begin
  Result:=FrameViewAudioList1.MouseIsOver;
end;

constructor TFrameMainAudio.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FrameViewAudioList1:=TFrameViewAudioList.Create(Self);
  FrameViewAudioList1.Parent:=Panel3;
  FrameViewAudioList1.Align:=alClient;
  FrameViewAudioList1.ItemHeight:=30;
  FrameViewAudioList1.MultiSelect:=FALSE;
  FrameViewAudioList1.OnItemRightClick:=@ProcessSoundRightClick;
end;

procedure TFrameMainAudio.EraseBackground(DC: HDC);
begin
  // do nothing here
end;

procedure TFrameMainAudio.ProcessKeyDown(var Key: Word; Shift: TShiftState);
begin
  FrameViewAudioList1.ProcessKeyDown(Key, Shift);
end;

procedure TFrameMainAudio.ProcessKeyUp(var Key: Word; Shift: TShiftState);
begin
  FrameViewAudioList1.ProcessKeyUp(Key, Shift);
end;

procedure TFrameMainAudio.Fill;
begin
 FrameViewAudioList1.Fill;
end;

procedure TFrameMainAudio.UpdateEditMode;
begin
// FrameViewAudioList1.MouseCanMoveItem := Project.Prefs.EditMode;
 FrameViewAudioList1.MultiSelect := Project.Prefs.EditMode;

 SpeedButton1.Visible := Project.Prefs.EditMode;
end;

end.

