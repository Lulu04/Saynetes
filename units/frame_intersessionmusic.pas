unit frame_intersessionmusic;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, FileCtrl, Menus, StdCtrls,
  Buttons, ComCtrls, LCLTranslator, Types,
  frame_led;

type

  { TFrameIntersessionMusic }

  TFrameIntersessionMusic = class(TFrame)
    FLBPlaylist: TFileListBox;
    Label10: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Label7: TLabel;
    Label1: TLabel;
    MenuItem1: TMenuItem;
    MIDeletePlaylist: TMenuItem;
    MIModifyPlaylist: TMenuItem;
    MINewPlaylist: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    PopupPlaylist: TPopupMenu;
    Shape2: TShape;
    Shape3: TShape;
    BPrevious: TSpeedButton;
    BNext: TSpeedButton;
    SpeedButton3: TSpeedButton;
    Timer1: TTimer;
    TB1: TTrackBar;
    procedure BPreviousMouseEnter(Sender: TObject);
    procedure FLBPlaylistDrawItem({%H-}Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure FLBPlaylistMouseLeave(Sender: TObject);
    procedure FLBPlaylistMouseMove(Sender: TObject; {%H-}Shift: TShiftState; {%H-}X, Y: Integer);
    procedure FLBPlaylistSelectionChange(Sender: TObject; {%H-}User: boolean);
    procedure MIDeletePlaylistClick(Sender: TObject);
    procedure MIModifyPlaylistClick(Sender: TObject);
    procedure MINewPlaylistClick(Sender: TObject);
    procedure PopupPlaylistPopup(Sender: TObject);
    procedure BPreviousClick(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure TB1Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FrameLed1: TFrameLed;
    FPreviousPlaylist: string;
    procedure LoadPlaylist(const aFilename: string);
    function PlaylistIsPlaying: boolean;
    procedure UpdateWidgets;
    function GetVolume: single;
    procedure Fill;
    procedure StopPlaylist;
  private
    FItemIndexUnderMouse: integer;
  public
    constructor Create(TheOwner: TComponent); override;

    procedure UpdateStringAfterLanguageChange;
    // returns the volume sets by user
    function Volume: single;
  end;

implementation
uses ALSound, u_audio_manager, LCLType, Graphics,
  u_common, u_createplaylist, u_userdialogs,
  u_resource_string, u_utils, u_apputils, u_program_options, VelocityCurve,
  System.UITypes, LazFileUtils;

{$R *.lfm}

{ TFrameIntersessionMusic }

procedure TFrameIntersessionMusic.FLBPlaylistSelectionChange(Sender: TObject; User: boolean);
var s: string;
begin
  if FLBPlaylist.ItemIndex <> -1 then
  begin
    s := ConcatPaths([FLBPlaylist.Directory, FLBPlaylist.GetSelectedText]);
    if FPreviousPlaylist = s then
      exit;

    FPreviousPlaylist := s;
    LoadPlaylist(s);
  end;

  UpdateWidgets;
end;

procedure TFrameIntersessionMusic.FLBPlaylistDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
begin
  with FLBPlaylist.Canvas do
  begin
    if State >= [odSelected] then
    begin
      Brush.Color := clHighLight;
      Font.Color := clWhite;
    end
    else begin
      if Index Mod 2 = 0 then
        Brush.Color := FLBPlaylist.Color
      else
        Brush.Color := u_utils.PercentColor(FLBPlaylist.Color, 0.25);
      Font.Color :=FLBPlaylist.Font.Color;// $009FD1EC;
    end;
    // render dot rectangle if mouse is over item
    if Index = FItemIndexUnderMouse then
    begin
      Pen.Style := psDot;
      Pen.Color := u_utils.PercentColor(FLBPlaylist.Color,1); //RGBToColor(200,200,150);
      Rectangle(ARect.Left-1, ARect.Top, ARect.Right+1, ARect.Bottom);
    end
    else FillRect(ARect);

    Brush.Style := bsClear;
    TextOut(ARect.Left+3, aRect.Top, ChangeFileExt(FLBPlaylist.Items.Strings[Index], ''));
  end;
end;

procedure TFrameIntersessionMusic.FLBPlaylistMouseLeave(Sender: TObject);
begin
  FItemIndexUnderMouse := -1;
  FLBPlaylist.Invalidate;
end;

procedure TFrameIntersessionMusic.BPreviousMouseEnter(Sender: TObject);
begin
  BNext.Hint := SNext;
  BPrevious.Hint := SPrevious;
end;

procedure TFrameIntersessionMusic.FLBPlaylistMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
begin
  // check if the mouse is over an item
  i := FLBPlaylist.GetIndexAtY(Y);
  if i <> FItemIndexUnderMouse then
  begin
    if i = -1 then
      FLBPlaylist.Cursor := crDefault
    else
      FLBPlaylist.Cursor := crHandPoint;
    FItemIndexUnderMouse := i;
    FLBPlaylist.Invalidate;
  end;
end;

procedure TFrameIntersessionMusic.MIDeletePlaylistClick(Sender: TObject);
var f: string;
begin
  if FLBPlaylist.ItemIndex = -1 then exit;

  if AskConfirmation(SAskDeletePlaylist, SYes, SNo, mtWarning) = mrOk then
  begin
    StopPlaylist;
    f := ConcatPaths([FLBPlaylist.Directory, FLBPlaylist.GetSelectedText]);
    LazFileUtils.DeleteFileUTF8(f);
    FLBPlaylist.UpdateFileList;
  end;
end;

procedure TFrameIntersessionMusic.MIModifyPlaylistClick(Sender: TObject);
var F: TFormCreatePlaylist;
  s: string;
  i: integer;
begin
  if FLBPlaylist.ItemIndex = -1 then exit;

  StopPlaylist;
  s := ConcatPaths([FLBPlaylist.Directory, FLBPlaylist.GetSelectedText]);

  F := TFormCreatePlaylist.Create(NIL);
  try
    F.SetModeModify(s);
    if F.ShowModal = mrOk then
    begin
      FLBPlaylist.ItemIndex := -1;
      FLBPlaylist.UpdateFileList;
      i := FLBPlaylist.Items.IndexOf(F.ModifiedName);
      if i <> -1 then begin
        FLBPlaylist.ItemIndex := i;
     //   s := ConcatPaths([FLBPlaylist.Directory, FLBPlaylist.GetSelectedText]);
     //   LoadPlaylist(s);
      end;
      UpdateWidgets;
    end;
  finally
    F.Free;
  end;
end;

procedure TFrameIntersessionMusic.MINewPlaylistClick(Sender: TObject);
var F: TFormCreatePlaylist;
  i: Integer;
begin
  StopPlaylist;
  F := TFormCreatePlaylist.Create(NIL);
  F.SetModeNew;
  try
    if F.ShowModal = mrOk then begin
      FLBPlaylist.UpdateFileList;
      i := FLBPlaylist.Items.IndexOf(F.ModifiedName);
      if i <> -1 then FLBPlaylist.ItemIndex := i;
    end;
  finally
    F.Free;
  end;
end;

procedure TFrameIntersessionMusic.PopupPlaylistPopup(Sender: TObject);
begin
  MIModifyPlaylist.Enabled := FLBPlaylist.ItemIndex <> -1;
  MIDeletePlaylist.Enabled := FLBPlaylist.ItemIndex <> -1;
end;

procedure TFrameIntersessionMusic.BPreviousClick(Sender: TObject);
begin
  if Sender = BNext then
    SoundManager.PlayList.Next(1)
  else
    SoundManager.PlayList.Previous(1);
end;

procedure TFrameIntersessionMusic.SpeedButton3Click(Sender: TObject);
begin
  case SoundManager.PlayList.State of
    ALS_STOPPED, ALS_PAUSED:
      begin
       if SoundManager.Playlist.Count = 0 then
       begin
         DoRedFlashOnWinControl(FLBPlaylist);
         exit;
       end;

       Timer1.Enabled := TRUE;
       if SoundManager.Playlist.State = ALS_PAUSED then
         SoundManager.Playlist.Play(7)
       else
       begin
         SoundManager.Playlist.Play(0);
         SoundManager.Playlist.Volume := GetVolume;
       end;
       FrameLed1.State := True;
      end;
    ALS_PLAYING:
      begin
       Timer1.Enabled := FALSE;
       SoundManager.Playlist.Pause(7);
       FrameLed1.State := False;
      end;
  end;//case
  UpdateWidgets;
end;

procedure TFrameIntersessionMusic.TB1Change(Sender: TObject);
begin
  SoundManager.Playlist.Volume := GetVolume;
  UpdateWidgets;
end;

procedure TFrameIntersessionMusic.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := FALSE;
  if PlaylistIsPlaying then
    UpdateWidgets;
  Timer1.Enabled := TRUE;
end;

procedure TFrameIntersessionMusic.LoadPlaylist(const aFilename: string);
var t: TStringList;
  i: integer;
  flagOn: boolean;
begin
  t:=TStringList.Create;
  try
    try
      t.LoadFromFile(aFilename);
      flagOn := SoundManager.Playlist.State = ALS_PLAYING;
      SoundManager.Playlist.Clear;
      for i:=0 to t.Count-1 do
       SoundManager.Playlist.Add(t.Strings[i]);
      if flagOn then
        SoundManager.Playlist.Play(0);
    except
      ShowMess(SErrorLoadingPlaylist, SOk, mtError);
    end;
  finally
    t.Free;
  end;
end;

function TFrameIntersessionMusic.PlaylistIsPlaying: boolean;
begin
  Result := SoundManager.Playlist.State = ALS_PLAYING;
end;

procedure TFrameIntersessionMusic.UpdateWidgets;
begin
  Label7.Caption := SVolume+LineEnding+FormatFloat('0.0', GetVolume*100)+'%';

  case SoundManager.PlayList.State of
   ALS_PLAYING:
     begin
       Label10.Caption := SPlaying;
       Label11.Caption := ExtractFilename(SoundManager.Playlist.CurrentFile)+' - '+
                       (SoundManager.Playlist.CurrentIndex+1).ToString+'/'+
                       SoundManager.Playlist.Count.ToString;
       Shape3.Brush.Color := $0003C4FC;
       //Label1.Font.Color := $0003C4FC;
     end;
   ALS_PAUSED:
     begin
      Label10.Caption := SPaused;
      Label11.Caption := ExtractFilename(SoundManager.Playlist.CurrentFile)+' - '+
                      (SoundManager.Playlist.CurrentIndex+1).ToString+'/'+
                      SoundManager.Playlist.Count.ToString;
      Shape3.Brush.Color := $00D6D6D6;
      //Label1.Font.Color := $00111111;
     end;
   ALS_STOPPED:
     begin
      Label10.Caption := SStopped;
      Label11.Caption := ' ';
      Shape3.Brush.Color := $00D6D6D6;
      //Label1.Font.Color := $00111111;
     end;
  end;
end;

function TFrameIntersessionMusic.GetVolume: single;
begin
  Result := (TB1.Position/TB1.Max)*(TB1.Position/TB1.Max);
end;

procedure TFrameIntersessionMusic.Fill;
begin
  FLBPlaylist.Mask := '*'+PLAYLIST_FILE_EXTENSION;

  FLBPlaylist.Directory := GetPlaylistsFolder;
  FLBPlaylist.UpdateFileList;

  UpdateWidgets;

  Label2.Caption := SPlaylist;
  Label7.Caption := SVolume;

  TB1.Position := Round(ProgramOptions.IntersessionMusicVolume*TB1.Max);

  FItemIndexUnderMouse := -1;
end;

constructor TFrameIntersessionMusic.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Fill;
  UpdateWidgets;

  FrameLed1 := TFrameLed.Create(Self);
  FrameLed1.AssociateToPanel(Panel2);
  FrameLed1.GreenType;
  FrameLed1.BlinkWhenOn := True;
end;

procedure TFrameIntersessionMusic.StopPlaylist;
begin
  Timer1.Enabled := FALSE;
  SoundManager.Playlist.Stop(0);
  FrameLed1.State := False;
  UpdateWidgets;
end;

procedure TFrameIntersessionMusic.UpdateStringAfterLanguageChange;
begin
  BNext.Hint := SNext;
  BPrevious.Hint := SPrevious;
end;

function TFrameIntersessionMusic.Volume: single;
begin
  Result := TB1.Position / TB1.Max;
end;

end.

