unit frame_viewprojectfolder;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, Buttons, FileCtrl,
  StdCtrls, LCLTranslator, Dialogs, Menus, Types;

type

  { TFrameViewProjectFolder }

  TFrameViewProjectFolder = class(TFrame)
    BHelp: TSpeedButton;
    ButtonOpenProject: TSpeedButton;
    FLBProjects: TFileListBox;
    Label1: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    MIRename: TMenuItem;
    Panel1: TPanel;
    Panel3: TPanel;
    PopupProject: TPopupMenu;
    SDD1: TSelectDirectoryDialog;
    BSearchProjectFolder: TSpeedButton;
    BDeleteProject: TSpeedButton;
    BRenameProject: TSpeedButton;
    procedure BHelpClick(Sender: TObject);
    procedure BRenameProjectClick(Sender: TObject);
    procedure ButtonOpenProjectClick(Sender: TObject);
    procedure DirectoryEdit1AcceptDirectory(Sender: TObject; var Value: String);
    procedure FLBProjectsDblClick(Sender: TObject);
    procedure FLBProjectsDrawItem({%H-}Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure FLBProjectsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FLBProjectsKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FLBProjectsMouseLeave(Sender: TObject);
    procedure FLBProjectsMouseMove(Sender: TObject; {%H-}Shift: TShiftState; {%H-}X, Y: Integer);
    procedure BSearchProjectFolderClick(Sender: TObject);
    procedure MIRenameClick(Sender: TObject);
  private
    FItemIndexUnderMouse: integer;
    procedure DoRenameProjectSelected;
    procedure DoDeleteProjectSelected;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Fill;
    // call it when user change between EDIT and SHOW mode
    procedure UpdateEditMode;
  end;

implementation
uses u_common, u_project_manager, VelocityCurve, u_utils, u_program_options,
  form_help, u_resource_string, u_userdialogs, u_datamodule, u_mainform,
  form_confirmdeleteproject, Graphics, LazFileUtils, LCLType,
  utilitaire_fichier;

{$R *.lfm}

{ TFrameViewProjectFolder }

procedure TFrameViewProjectFolder.DirectoryEdit1AcceptDirectory(Sender: TObject; var Value: String);
begin
  FLBProjects.Directory := Value;
  ProgramOptions.WorkingFolder := Value;
end;

procedure TFrameViewProjectFolder.ButtonOpenProjectClick(Sender: TObject);
var f: string;
  i: integer;
begin
  i := FLBProjects.ItemIndex;
  if i = -1 then exit;

  try
    Screen.BeginTempCursor(crHourGlass);
    Enabled := False;
    f := FLBProjects.FileName;

    // if the current and new project are in the same folder, we don't load the
    // universes to keep the current values of lightning.
    Project.KeepUniverseManager := Project.IsReady and
       (ExtractFilePath(Project.Filename) = ExtractFilePath(f));

    Project.Load( f );

  finally
    Project.KeepUniverseManager := FALSE;
    Enabled := True;
    Screen.EndTempCursor(crHourGlass);

   // FLBProjects.ItemIndex := i;
  end;
end;

procedure TFrameViewProjectFolder.BHelpClick(Sender: TObject);
begin
  _ShowHelp(HelpProjectList, BHelp);
end;

procedure TFrameViewProjectFolder.BRenameProjectClick(Sender: TObject);
begin
  if FLBProjects.ItemIndex = -1 then exit;

  if Sender = BRenameProject then
    DoRenameProjectSelected;

  if Sender = BDeleteProject then
    DoDeleteProjectSelected;
end;

procedure TFrameViewProjectFolder.FLBProjectsDblClick(Sender: TObject);
begin
  ButtonOpenProjectClick(NIL);
end;

procedure TFrameViewProjectFolder.FLBProjectsDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
begin
  with FLBProjects.Canvas do
  begin
    if State >= [odSelected] then
    begin
      Brush.Color := clHighLight;
      Font.Color := clWhite;
    end
    else begin
      if Index Mod 2 = 0 then
        Brush.Color := FLBProjects.Color
      else
        Brush.Color := PercentColorRelative(FLBProjects.Color, 0.25);
      Font.Color := FLBProjects.Font.Color; // $009FD1EC; // $00EAEAEA;
    end;
    // render dot rectangle if mouse is over item
    if Index = FItemIndexUnderMouse then
    begin
      Pen.Style := psDot;
      Pen.Color := PercentColorRelative(FLBProjects.Color, 1);
      Rectangle(ARect.Left-1, ARect.Top, ARect.Right+1, ARect.Bottom);
    end
    else FillRect(ARect);

    if FLBProjects.Items.Strings[Index] = ExtractFileName(Project.FileName) then
      DataModule1.ImageList1.Draw(FLBProjects.Canvas, aRect.Left, aRect.Top+(aRect.Height-DataModule1.ImageList1.Height)div 2, 54);

    Brush.Style := bsClear;
    TextOut(ARect.Left+DataModule1.ImageList1.Width+ScaleDesignToForm(3), aRect.Top, FLBProjects.Items.Strings[Index]);
  end;
end;

procedure TFrameViewProjectFolder.FLBProjectsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key in [VK_UP, VK_DOWN, VK_RIGHT, VK_LEFT] then Key := VK_UNKNOWN;
end;

procedure TFrameViewProjectFolder.FLBProjectsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key in [VK_UP, VK_DOWN, VK_RIGHT, VK_LEFT] then Key := VK_UNKNOWN;
end;

procedure TFrameViewProjectFolder.FLBProjectsMouseLeave(Sender: TObject);
begin
  FItemIndexUnderMouse := -1;
  FLBProjects.Invalidate;
end;

procedure TFrameViewProjectFolder.FLBProjectsMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
begin
  // check if the mouse is over an item
  i := FLBProjects.GetIndexAtY(Y);
  if i <> FItemIndexUnderMouse then
  begin
    if i = -1 then
      FLBProjects.Cursor := crDefault
    else
      FLBProjects.Cursor := crHandPoint;
    FItemIndexUnderMouse := i;
    FLBProjects.Invalidate;
  end;
end;

procedure TFrameViewProjectFolder.BSearchProjectFolderClick(Sender: TObject);
begin
  if DirectoryExists(ProgramOptions.WorkingFolder) then SDD1.InitialDir := ProgramOptions.WorkingFolder
    else SDD1.InitialDir := '';
  if not SDD1.Execute then exit;
  Label1.Caption := SDD1.FileName;
  Label1.Hint := SDD1.FileName;
  FLBProjects.Directory := SDD1.FileName;
  FLBProjects.UpdateFileList;
end;

procedure TFrameViewProjectFolder.MIRenameClick(Sender: TObject);
begin
  if Sender = MIRename then ;
end;

procedure TFrameViewProjectFolder.DoRenameProjectSelected;
var n, oldName, oldFile, newFile, oldDataFolder, newDataFolder: string;
  flagProjectIsOpened: boolean;
begin
  oldFile := FLBProjects.FileName;
  oldName := ChangeFileExt(ExtractFileName(oldFile), '');
  n := oldName;
  if UserInputFileName(SProjectName, SOk, SCancel, n, mtConfirmation) <> mrOk then exit;
  if n = oldName then exit;

  newFile := ConcatPaths([ExtractFilePath(FLBProjects.FileName), n]);
  newFile := ChangeFileExt(newFile, PROJECT_FILE_EXTENSION);
  if FileExists(newFile) then exit;

  // if the project to rename is the current opened, we close it -> stops all audio streams
  flagProjectIsOpened := Project.Filename = FLBProjects.FileName;
  oldDataFolder := ChangeFileExt(oldFile, '')+'Data';
  newDataFolder := ChangeFileExt(newFile, '')+'Data';
  if flagProjectIsOpened then
    if not Project.Close then exit; // the project is modified and user refuse to close it

  // rename project file on the disk
  if not RenommeFichier(oldFile, newFile) then exit;

  // rename project data folder on the disk
  if not RenommerRepertoire(oldDataFolder, newDataFolder) then begin
    RenommeFichier(newFile, oldFile);
    exit;
  end;

  if flagProjectIsOpened then Project.Load(newFile);
  Fill;
end;

procedure TFrameViewProjectFolder.DoDeleteProjectSelected;
var currentFile, currentDataFolder: string;
  flagProjectIsOpened: Boolean;
  F: TFormConfirmationDeleteProject;
begin
  currentFile := FLBProjects.FileName;
  currentDataFolder := ChangeFileExt(currentFile, '')+'Data';

  F := TFormConfirmationDeleteProject.Create(NIL);
  F.SetProjectName(ExtractFileName(currentFile));
  if F.ShowModal <> mrOk then exit;
  F.Free;

  // if the project to rename is the current opened, we close it -> stops all audio streams
  flagProjectIsOpened := Project.Filename = currentFile;
  if flagProjectIsOpened then begin
    Project.HasBeenModified := False;
    Project.Close;
  end;

  MoveFilesToTrash([currentFile]);
  MoveFilesToTrash([currentDataFolder]);

  Fill;
  if flagProjectIsOpened then FormMain.SendMessageToShowStartupWizard;
end;

constructor TFrameViewProjectFolder.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FItemIndexUnderMouse := -1;
end;

procedure TFrameViewProjectFolder.Fill;
var i: Integer;
begin
  if DirectoryExists(ProgramOptions.WorkingFolder) then begin
    Label1.Caption := ProgramOptions.WorkingFolder;
    Label1.Hint := ProgramOptions.WorkingFolder;
    FLBProjects.Directory := ProgramOptions.WorkingFolder;
    FLBProjects.UpdateFileList;
    if Project.IsReady then begin
      i := FLBProjects.Items.IndexOf(Project.NameWithExtension);
      if i <> -1 then FLBProjects.ItemIndex := i;
    end;
  end;
end;

procedure TFrameViewProjectFolder.UpdateEditMode;
begin
  BRenameProject.Visible := Project.Options.EditMode;
  BDeleteProject.Visible := Project.Options.EditMode;
end;

end.

