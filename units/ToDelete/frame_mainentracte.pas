unit frame_mainentracte;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, FileCtrl, ComCtrls,
  LCLTranslator, ExtCtrls, Buttons, EditBtn, LCLType, Menus,
  ALSound, u_audio_manager;

type

  { TFrameMainEntracte }

  TFrameMainEntracte = class(TFrame)
    DirectoryEdit1: TDirectoryEdit;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label1: TLabel;
    FLBProjects: TFileListBox;
    FLBPlaylist: TFileListBox;
    MenuItem1: TMenuItem;
    MINewPlaylist: TMenuItem;
    MIDeletePlaylist: TMenuItem;
    MIModifyPlaylist: TMenuItem;
    Panel1: TPanel;
    PopupPlaylist: TPopupMenu;
    Shape1: TShape;
    SpeedButton1: TSpeedButton;
    ButtonOpenProject: TSpeedButton;
    TB1: TTrackBar;
    Timer1: TTimer;
    procedure DirectoryEdit1AcceptDirectory(Sender: TObject; var Value: String);
    procedure FLBPlaylistSelectionChange(Sender: TObject; {%H-}User: boolean);
    procedure FLBProjectsDblClick(Sender: TObject);
    procedure MIDeletePlaylistClick(Sender: TObject);
    procedure MIModifyPlaylistClick(Sender: TObject);
    procedure MINewPlaylistClick(Sender: TObject);
    procedure ButtonOpenProjectClick(Sender: TObject);
    procedure PopupPlaylistPopup(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure TB1Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FPreviousPlaylist: string;
    procedure LoadPlaylist(const aFilename: string);
    function PlaylistIsPlaying: boolean;
    procedure UpdateWidgets;
  public
    procedure EraseBackground({%H-}DC: HDC); override;
    procedure Fill;

    procedure StopPlaylist;
  end;

implementation

uses u_common, u_project_manager, u_createplaylist, u_userdialogs,
  u_resource_string, u_list_dmxuniverse, VelocityCurve, System.UITypes, LazFileUtils;

{$R *.lfm}

{ TFrameMainEntracte }

procedure TFrameMainEntracte.TB1Change(Sender: TObject);
begin
  SoundManager.Playlist.Volume:=TB1.Position/TB1.Max;
  UpdateWidgets;
end;

procedure TFrameMainEntracte.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled:=FALSE;
  if PlaylistIsPlaying then begin
    Label8.Caption:=ExtractFilename(SoundManager.Playlist.CurrentFile)+' - '+
                    (SoundManager.Playlist.CurrentIndex+1).ToString+'/'+
                    SoundManager.Playlist.Count.ToString;
  end;
  Timer1.Enabled:=TRUE;
end;

procedure TFrameMainEntracte.LoadPlaylist(const aFilename: string);
var t: TStringList;
  i: integer;
begin
  t:=TStringList.Create;
  try
    t.LoadFromFile(aFilename);
    SoundManager.Playlist.Clear;
    for i:=0 to t.Count-1 do
     SoundManager.Playlist.Add(t.Strings[i]);
  except
    ShowMess(SErrorLoadingPlaylist, SOk, mtError);
  end;
  t.Free;
end;

function TFrameMainEntracte.PlaylistIsPlaying: boolean;
begin
  Result:=SpeedButton1.Tag=1;
end;

procedure TFrameMainEntracte.MIDeletePlaylistClick(Sender: TObject);
var f: string;
begin
  if FLBPlaylist.ItemIndex=-1 then exit;
  if AskConfirmation(SAskDeletePlaylist, SYes, SNo, mtWarning)=mrOk then begin
    f:=ConcatPaths([FLBPlaylist.Directory, FLBPlaylist.GetSelectedText]);
    LazFileUtils.DeleteFileUTF8(f);
    FLBPlaylist.UpdateFileList;
  end;
end;

procedure TFrameMainEntracte.FLBPlaylistSelectionChange(Sender: TObject;
  User: boolean);
var s:string;
begin
  if FLBPlaylist.ItemIndex<>-1 then begin
    s:=ConcatPaths([FLBPlaylist.Directory, FLBPlaylist.GetSelectedText]);
    if FPreviousPlaylist=s then exit;
    FPreviousPlaylist:=s;
    LoadPlaylist(s);
    Label9.Caption:=SStopped;
    SpeedButton1.Tag:=0;
    SpeedButton1.Caption:=SStartIntersession;
    Label8.Caption:=ExtractFilename(SoundManager.Playlist.CurrentFile)+' - '+
                    (SoundManager.Playlist.CurrentIndex+1).ToString+'/'+
                    SoundManager.Playlist.Count.ToString;
  end;
  UpdateWidgets;
end;

procedure TFrameMainEntracte.FLBProjectsDblClick(Sender: TObject);
begin
  if FLBProjects.ItemIndex<>-1
    then ButtonOpenProjectClick(NIL);
end;

procedure TFrameMainEntracte.DirectoryEdit1AcceptDirectory(Sender: TObject; var Value: String);
begin
  FLBProjects.Directory:=Value;
  Project.ProgramPrefs.WorkingFolder:=Value;
end;

procedure TFrameMainEntracte.MIModifyPlaylistClick(Sender: TObject);
var F: TFormCreatePlaylist;
  s: string;
begin
  if FLBPlaylist.ItemIndex=-1 then exit;
  s:=ConcatPaths([FLBPlaylist.Directory, FLBPlaylist.GetSelectedText]);
  F:=TFormCreatePlaylist.Create(NIL);
  F.SetModeModify(s);
  if F.ShowModal=mrOk then begin
    LoadPlaylist(s);
    Label9.Caption:=SStopped;
    SpeedButton1.Tag:=0;
    SpeedButton1.Caption:=SStartIntersession;
    Label8.Caption:=ExtractFilename(SoundManager.Playlist.CurrentFile)+' - '+
                    (SoundManager.Playlist.CurrentIndex+1).ToString+'/'+
                    SoundManager.Playlist.Count.ToString;
  end;
  F.Free;
end;

procedure TFrameMainEntracte.MINewPlaylistClick(Sender: TObject);
var F: TFormCreatePlaylist;
begin
  F:=TFormCreatePlaylist.Create(NIL);
  F.SetModeNew;
  if F.ShowModal=mrOk then begin
    FLBPlaylist.UpdateFileList;
  end;
  F.Free;
end;

procedure TFrameMainEntracte.ButtonOpenProjectClick(Sender: TObject);
var i: integer;
begin
  i:=FLBProjects.ItemIndex;
  if i=-1 then exit;

  Project.KeepUniverseManager := UniverseManager.SameDMXConfiguration(FLBProjects.FileName);

  Project.Load(FLBProjects.FileName);

  Project.KeepUniverseManager := FALSE;
end;

procedure TFrameMainEntracte.PopupPlaylistPopup(Sender: TObject);
begin
  MIModifyPlaylist.Enabled:=FLBPlaylist.ItemIndex<>-1;
  MIDeletePlaylist.Enabled:=FLBPlaylist.ItemIndex<>-1;
end;

procedure TFrameMainEntracte.SpeedButton1Click(Sender: TObject);
begin
  case SpeedButton1.Tag of
    0: begin
     if SoundManager.Playlist.Count=0 then exit;
     Label9.Caption:=SPlaying;
     SpeedButton1.Tag:=1;
     SpeedButton1.Caption:=SStopIntersession;
     Timer1.Enabled:=TRUE;
     if SoundManager.Playlist.State=ALS_PAUSED
       then SoundManager.Playlist.Play(7)
       else SoundManager.Playlist.Play(0);
    end;
    1: begin
     Timer1.Enabled:=FALSE;
     SoundManager.Playlist.Pause(7);
     SpeedButton1.Tag:=0;
     SpeedButton1.Caption:=SStartIntersession;
     Label9.Caption:=SPaused;
    end;
  end;//case

  UpdateWidgets;
end;

procedure TFrameMainEntracte.UpdateWidgets;
begin
  Label1.Caption:=FormatFloat('0.0', TB1.Position/TB1.Max*100)+'%';

  Label8.Visible:=FLBPlaylist.ItemIndex<>-1;
  Label9.Visible:=Label8.Visible;
end;

procedure TFrameMainEntracte.EraseBackground(DC: HDC);
begin
// do nothing here
end;

procedure TFrameMainEntracte.Fill;
begin
  DirectoryEdit1.Directory:=Project.ProgramPrefs.WorkingFolder;
  FLBProjects.Directory:=Project.ProgramPrefs.WorkingFolder;
  FLBProjects.UpdateFileList;

  FLBPlaylist.Mask:='*'+PLAYLIST_FILE_EXTENSION;
//  FLBPlaylist.Directory:=Project.ProgramPrefs.SaveFolder;
  FLBPlaylist.Directory:=Project.PlaylistsFolder;
  FLBPlaylist.UpdateFileList;
end;

procedure TFrameMainEntracte.StopPlaylist;
begin
  Timer1.Enabled:=FALSE;
  SoundManager.Playlist.Stop(0);
  SpeedButton1.Tag:=0;
  UpdateWidgets;
end;

end.

