unit frame_viewprojectfolder;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, Buttons, FileCtrl,
  StdCtrls, LCLTranslator, Dialogs, Types;

type

  { TFrameViewProjectFolder }

  TFrameViewProjectFolder = class(TFrame)
    BHelp: TSpeedButton;
    ButtonOpenProject: TSpeedButton;
    FLBProjects: TFileListBox;
    Label1: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Panel1: TPanel;
    Panel3: TPanel;
    SDD1: TSelectDirectoryDialog;
    BSearchProjectFolder: TSpeedButton;
    procedure BHelpClick(Sender: TObject);
    procedure ButtonOpenProjectClick(Sender: TObject);
    procedure DirectoryEdit1AcceptDirectory(Sender: TObject; var Value: String);
    procedure FLBProjectsDblClick(Sender: TObject);
    procedure FLBProjectsDrawItem({%H-}Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure FLBProjectsMouseLeave(Sender: TObject);
    procedure FLBProjectsMouseMove(Sender: TObject; {%H-}Shift: TShiftState; {%H-}X, Y: Integer);
    procedure BSearchProjectFolderClick(Sender: TObject);
  private
    FItemIndexUnderMouse: integer;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Fill;
  end;

implementation
uses u_common, u_project_manager, VelocityCurve, u_utils, u_program_options,
  form_help, u_resource_string, Graphics, LazFileUtils, LCLType;

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
      Font.Color := $009FD1EC; // $00EAEAEA;
    end;
    // render dot rectangle if mouse is over item
    if Index = FItemIndexUnderMouse then
    begin
      Pen.Style := psDot;
      Pen.Color := PercentColorRelative(FLBProjects.Color, 1);
      Rectangle(ARect.Left-1, ARect.Top, ARect.Right+1, ARect.Bottom);
    end
    else FillRect(ARect);

    Brush.Style := bsClear;
    TextOut(ARect.Left+3, aRect.Top, ChangeFileExt(FLBProjects.Items.Strings[Index], ''));
  end;
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

end.

