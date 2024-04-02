unit u_main_intersession;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, Menus, EditBtn, FileCtrl, ComCtrls, LCLType,
  ALSound, u_audio_manager;

type

  { TFormMainIntersession }

  TFormMainIntersession = class(TForm)
    ButtonOpenProject: TSpeedButton;
    DirectoryEdit1: TDirectoryEdit;
    FLBProjects: TFileListBox;
    Label7: TLabel;
    Panel1: TPanel;
    Shape1: TShape;
    procedure ButtonOpenProjectClick(Sender: TObject);
    procedure DirectoryEdit1AcceptDirectory(Sender: TObject; var Value: String);
    procedure FLBProjectsDblClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    procedure UpdateWidgets;
  public
    procedure EraseBackground({%H-}DC: HDC); override;
    procedure Fill;

    procedure StopPlaylist;
  end;

var
  FormMainIntersession: TFormMainIntersession;

implementation
uses u_common, u_project_manager,
  u_list_dmxuniverse, u_mainform, VelocityCurve, LazFileUtils;

{$R *.lfm}

{ TFormMainIntersession }

procedure TFormMainIntersession.FLBProjectsDblClick(Sender: TObject);
begin
  if FLBProjects.ItemIndex <> -1 then
    ButtonOpenProjectClick(NIL);
end;

procedure TFormMainIntersession.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key in [VK_F1, VK_F2, VK_F3] then
    FormMain.FormKeyDown(Self, Key, Shift);
end;

procedure TFormMainIntersession.FormShow(Sender: TObject);
begin
  Fill;
end;

procedure TFormMainIntersession.DirectoryEdit1AcceptDirectory(Sender: TObject; var Value: String);
begin
  FLBProjects.Directory:=Value;
  Project.ProgramPrefs.WorkingFolder:=Value;
end;

procedure TFormMainIntersession.ButtonOpenProjectClick(Sender: TObject);
var i: integer;
begin
  i:=FLBProjects.ItemIndex;
  if i=-1 then exit;

  Project.KeepUniverseManager := UniverseManager.SameDMXConfiguration(FLBProjects.FileName);

  Project.Load(FLBProjects.FileName);

  Project.KeepUniverseManager := FALSE;
end;

procedure TFormMainIntersession.UpdateWidgets;
begin

end;

procedure TFormMainIntersession.EraseBackground(DC: HDC);
begin
end;

procedure TFormMainIntersession.Fill;
begin
  DirectoryEdit1.Directory := Project.ProgramPrefs.WorkingFolder;
  FLBProjects.Directory:=Project.ProgramPrefs.WorkingFolder;
  FLBProjects.UpdateFileList;
end;

procedure TFormMainIntersession.StopPlaylist;
begin
  SoundManager.Playlist.Stop(0);
  UpdateWidgets;
end;

end.

