unit u_project_manager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  project_util, u_dmxdevice_manager;


type

{ TProjectOptions }

TProjectOptions = class
private
  FCmdListViewColorAsRectangleColor: boolean;
  // Edit mode
  FEditMode: boolean;

  // view action's list
  FCmdListViewDMXAdress: boolean;
  FCmdListViewDMXChannelName: boolean;
  FCmdListViewDMXFixDescription: boolean;
  FCmdListViewDMXFixName: boolean;

  // main view
  FMainViewShowAudioPanel,
  FMainViewShowDMXPanel,
  FMainViewShowAudioControlPanel,
  FMainViewShowAudioCapturePanel: boolean;

  FLockSave: boolean;
  FSaveFolder: string;
  procedure SetCmdListViewColorAsRectangleColor(AValue: boolean);
  procedure SetCmdListViewDMXAdress(AValue: boolean);
  procedure SetCmdListViewDMXChannelName(AValue: boolean);
  procedure SetCmdListViewDMXFixDescription(AValue: boolean);
  procedure SetCmdListViewDMXFixName(AValue: boolean);
  procedure SetEditMode(AValue: boolean);
  function SaveFilename: string;
  procedure SetMainViewShowAudioControlPanel(AValue: boolean);
  procedure SetMainViewShowAudioCapturePanel(AValue: boolean);
  procedure SetMainViewShowAudioPanel(AValue: boolean);
  procedure SetMainViewShowDMXPanel(AValue: boolean);
public
  procedure InitByDefault;
  procedure Save;
  procedure Load;

  procedure LockSave;
  procedure UnlockSave;

  // Edit mode
  property EditMode: boolean read FEditMode write SetEditMode;

  //Main view panel to show
  property MainViewShowAudioPanel: boolean read FMainViewShowAudioPanel write SetMainViewShowAudioPanel;
  property MainViewShowDMXPanel: boolean read FMainViewShowDMXPanel write SetMainViewShowDMXPanel;
  property MainViewShowAudioControlPanel: boolean read FMainViewShowAudioControlPanel write SetMainViewShowAudioControlPanel;
  property MainViewShowAudioCapturePanel: boolean read FMainViewShowAudioCapturePanel write SetMainViewShowAudioCapturePanel;

  // View actions list
  property CmdListViewDMXAdress: boolean read FCmdListViewDMXAdress write SetCmdListViewDMXAdress;
  property CmdListViewDMXFixName: boolean read FCmdListViewDMXFixName write SetCmdListViewDMXFixName;
  property CmdListViewDMXFixDescription: boolean read FCmdListViewDMXFixDescription write SetCmdListViewDMXFixDescription;
  property CmdListViewDMXChannelName: boolean read FCmdListViewDMXChannelName write SetCmdListViewDMXChannelName;
  property CmdListViewColorAsRectangleColor: boolean read FCmdListViewColorAsRectangleColor write SetCmdListViewColorAsRectangleColor;
end;

type

{ TSaynetesProject }

TSaynetesProject = class(TCustomProject)
private
  FAudioStorage: TCustomStorageFolder;
  FImageStorage: TCustomStorageFolder;
  FKeepUniverseManager: boolean;
  Foptions: TProjectOptions;
public
  constructor Create;
  destructor Destroy; override;

  function DoNew: boolean; override;
  procedure DoSave( const aFilename: string ); override;
  function DoLoad( const aFilename: string ): boolean; override;
  procedure DoClose; override;

  procedure OnModifiedChange( {%H-}aState: boolean ); override;
  procedure OnProjectReadyChange( {%H-}aState: boolean ); override;

  procedure InitProjectByDefault;

  procedure UpdateStringAfterLanguageChange;
  function GetFolderCommonData: string;

  property AudioStorage: TCustomStorageFolder read FAudioStorage;
  property ImageStorage: TCustomStorageFolder read FImageStorage;

  property Options: TProjectOptions read Foptions;

  property KeepUniverseManager: boolean read FKeepUniverseManager write FKeepUniverseManager;
end;

var Project: TSaynetesProject;

implementation
uses u_mainform, u_common, u_list_dmxuniverse, u_list_sequence, u_audio_manager,
  u_projectwizard, u_sequence_player, u_userdialogs, u_resource_string,
  u_dmxtools_group, u_logfile, Controls, Dialogs, LCLTranslator, Forms,
  utilitaire_fichier, LazFileUtils, u_dmxtools_channels, u_dmxtools_rgb,
  u_program_options, u_utils, u_edit_otheraction, PropertyUtils;


{ TSaynetesProject }

constructor TSaynetesProject.Create;
begin
  inherited Create(PROJECT_FILE_EXTENSION);
  SetFormCaption( FormMain, APP_NAME );
  AddFilterToDialogs( 'Saynètes file', '*'+PROJECT_FILE_EXTENSION);
  AddFilterToDialogs('All file', '*.*');

  Foptions := TProjectOptions.Create;
  DeviceManager := TDeviceManager.Create;
  DeviceManager.LookForAvailableDevices;
  SoundManager := TSoundManager.Create;
  UniverseManager := TUniverseManager.Create;
  Sequences := TSequenceList.Create;

  SeqPlayer := TSequencePlayer.Create;

  FAudioStorage:=TCustomStorageFolder.Create('audio');
  FImageStorage:=TCustomStorageFolder.Create('image');
end;

destructor TSaynetesProject.Destroy;
begin
  SeqPlayer.StopPreview;
  Sequences.StopAll;
  UniverseManager.StopThread;
  SoundManager.ResetState;

  SeqPlayer.Free;
  UniverseManager.Free;
  SoundManager.Free;
  Log.Info('Destroying Sequence Manager');
  Sequences.Free;
  DeviceManager.Free;
  Log.Info('Destroying project''s preference object');
  Foptions.Free;
  Log.Info('Destroying program''s preference object');
  Log.Info('Destroying audio storage object');
  FAudioStorage.Free;
  Log.Info('Destroying image storage object');
  FImageStorage.Free;
  inherited Destroy;
end;

function TSaynetesProject.DoNew: boolean;
var F: TFormProjectWizard;
  path: string;
begin
 Result := FALSE;
 InitProjectByDefault;

 F := TFormProjectWizard.Create(NIL);
 try
  if F.ShowModal = mrOk then
  begin
    // the path choosen by user exists because we set the option [ofPathMustExist] in F.DirectoryEdit1.DialogOptions
    path := ConcatPaths([F.ProjectPath, F.ProjectName]);
    Project.Filename := path;
    UniverseManager.Load;

    // create a directory with the same name as the project
    path := IncludeTrailingPathDelimiter(path);
    if not CreerRepertoire(path) then
    begin
      ShowMess(SFailToCreateDirectoryNeededByTheProject, SOk, mtError);
      exit;
    end;

    // create the project subfolders in the created directory
   // path := ExtractFilePath(path);
    FAudioStorage.AbsoluteBaseFolder := path;  // audio
    FImageStorage.AbsoluteBaseFolder := path;  // image
    // project options path
    Options.FSaveFolder := path;

    // save the project
    Project.SaveAs( Project.Filename );

    Showmess(SProjectCreatedWithSuccess, SOk, mtInformation);
    Result := TRUE;
  end;
 finally
   F.Free;
 end;
end;

const
  APPNAME_HEADER='[APPLICATION NAME]';
  APPVERSION_HEADER='[APPLICATION VERSION]';

procedure TSaynetesProject.DoSave(const aFilename: string);
var t: TStringList;
begin
  t := TStringList.Create;
  try
   Options.Save;
   if not UniverseManager.Save then
     ShowMess(SFailureWhenSavingDMXUniverseTo+LineEnding+
              GetFolderCommonData, SOk, mtError);
   FormDMXGroup.SaveTo(GetFolderCommonData+COMMON_PROJECT_DMX_GROUP_FILENAME);
   try
     t.Add(APPNAME_HEADER);
     t.Add(APP_NAME);
     t.Add(APPVERSION_HEADER);
     t.Add(APP_VERSION);
     SoundManager.Save(t);
     Sequences.Save(t);
     t.SaveToFile(aFilename);
   except
     Log.Error('TSaynetesProject.DoSave - Failure when saving project to '+aFilename, 2);
     ShowMess(SFailureWhenSavingProjectTo+LineEnding+aFilename, SOk, mtError);
   end;
   // push the last used project in program's preferences file
   ProgramOptions.LastProjectFileNameUsed := aFilename;
   ProgramOptions.WorkingProject := aFilename;
  finally
    t.Free;
  end;
end;

function TSaynetesProject.DoLoad(const aFilename: string): boolean;
var t: TStringList;
  path, projectfolder: string;
  k: integer;
  badFile: boolean;
  fileVersion: string;
begin
  Log.AddEmptyLine;
  Log.Info('Loading project '+aFilename);
  t := TStringList.Create;
  try
   try
     t.LoadFromFile(aFilename);//, TEncoding.UTF8);

     // Checks header
     badFile := t.IndexOf(APPNAME_HEADER) = -1;
     badFile := badFile or (t.IndexOf(APP_NAME) = -1);
     k := t.indexOf(APPVERSION_HEADER);
     badFile := badFile or (k = -1) or (k = t.Count-1);
     if badFile then
     begin
       Showmess(SFileIsNotAProject+' '+lineending+aFilename, SOk, mtError);
       Result := FALSE;
     end
     else
     begin
       inc(k);
       fileVersion := t.Strings[k];
       Log.Info('project made with Saynètes version: '+fileVersion, 1);

       projectfolder := ExtractFileName(aFilename);
       projectfolder := ChangeFileExt(projectfolder, '');
       path := ConcatPaths([ExtractFilePath(aFilename), projectfolder]);
       FAudioStorage.AbsoluteBaseFolder := path;
       FImageStorage.AbsoluteBaseFolder := path;
       Log.Info('project storage: "'+path+'"', 1);

       Result := TRUE;
       SoundManager.Load(t, FAudioStorage.AbsoluteStorageFolder);

       if not KeepUniverseManager then Result := Result and UniverseManager.Load
         else Log.Info('Keep the same universes as previous', 1);

       FormDMXGroup.LoadFrom(GetFolderCommonData+COMMON_PROJECT_DMX_GROUP_FILENAME);

       Sequences.Load(t);
       if Sequences.CheckErrorInSequences then
         ShowMess(SErrorFoundInProjectSequences, SOk, mtWarning);

       Options.FSaveFolder := path;
       Options.Load;

       // push the last used project in program's preferences file
       ProgramOptions.SetLastOpenedProject(aFilename);

       Log.AddEmptyLine;
     end;
   except
     Showmess(SFailToLoadTheProject+' '+lineending+aFilename, SOk, mtError);
     Result := FALSE;
   end;
  finally
    t.Free;
  end;
end;

procedure TSaynetesProject.DoClose;
begin
  InitProjectByDefault;
end;

procedure TSaynetesProject.OnModifiedChange(aState: boolean);
begin
  FormMain.OnProjectModified;
end;

procedure TSaynetesProject.OnProjectReadyChange(aState: boolean);
begin
  FormMain.OnProjectReadyChange;
  UpdateStringAfterLanguageChange;
end;

procedure TSaynetesProject.InitProjectByDefault;
begin
  if not KeepUniverseManager then
  begin
    FormMain.FrameViewProjector1.Sel_None;
    UniverseManager.Clear;
    FormMain.FrameViewProjector1.Redraw;
  end;
  FormMain.FrameViewProjector1.GUIMode := guiMainDmx;
  FormDMXGroup.Clear;
  SoundManager.Clear;
  Sequences.ClearAll;
  Options.InitByDefault;

  FormMain.FrameMainSequence1.Fill;
  FormMain.FrameMainAudio1.Fill;
end;

procedure TSaynetesProject.UpdateStringAfterLanguageChange;
begin
  FormMain.FrameMainAudio1.UpdateStringAfterLanguageChange;
  FormMain.FrameMainSequence1.FrameViewTopList1.UpdateStringAfterLanguageChange;
  FormMain.FrameMainSequence1.FrameIntermissionMusic1.UpdateStringAfterLanguageChange;
  FormMain.FrameViewProjector1.FillComboBoxUniverseToShow;
  FormMain.FrameViewProjector1.FrameViewDMXCursors1.UpdateStringAfterLanguageChange;

  if FormDMXChannelsTools <> NIL then
    FormDMXChannelsTools.FrameFXChannelChaser1.UpdateStringAfterLanguageChange;

  if FormDMXRGBTools <> NIL then begin
    if FormDMXRGBTools.Frame_ColorPalette1 <> NIL then
      FormDMXRGBTools.Frame_ColorPalette1.UpdateStringAfterLanguageChange;
    if FormDMXRGBTools.FrameFXRGBChaser1 <> NIL then
      FormDMXRGBTools.FrameFXRGBChaser1.UpdateStringAfterLanguageChange;
  end;

  if FormOtherAction <> NIL then FormOtherAction.UpdateStringAfterLanguageChange;
end;

function TSaynetesProject.GetFolderCommonData: string;
begin
  Result := ConcatPaths([ExtractFilePath(Filename), COMMON_PROJECT_FOLDER_NAME]);
  Result := IncludeTrailingPathDelimiter(Result);
  if not RepertoireExistant(Result) then
    if not CreerRepertoire(Result) then
      Log.Error('TSaynetesProject.GetFolderCommonData - Failed to create the common folder "'+Result+'"', 2);
end;

{ TProjectOptions }

procedure TProjectOptions.SetEditMode(AValue: boolean);
begin
  if FEditMode=AValue then Exit;
  FEditMode:=AValue;
  // set state according to EDIT MODE
  FormMain.UpdateWidgetState;

  FormDMXChannelsTools.UpdateEditMode;
  FormDMXRGBTools.UpdateEditMode;
  Save;
end;

procedure TProjectOptions.SetCmdListViewDMXAdress(AValue: boolean);
begin
  if FCmdListViewDMXAdress = AValue then Exit;
  FCmdListViewDMXAdress := AValue;
  Save;
end;

procedure TProjectOptions.SetCmdListViewColorAsRectangleColor(AValue: boolean);
begin
  if FCmdListViewColorAsRectangleColor = AValue then Exit;
  FCmdListViewColorAsRectangleColor := AValue;
  Save;
end;

procedure TProjectOptions.SetCmdListViewDMXChannelName(AValue: boolean);
begin
  if FCmdListViewDMXChannelName = AValue then Exit;
  FCmdListViewDMXChannelName := AValue;
  Save;
end;

procedure TProjectOptions.SetCmdListViewDMXFixDescription(AValue: boolean);
begin
  if FCmdListViewDMXFixDescription = AValue then Exit;
  FCmdListViewDMXFixDescription := AValue;
  Save;
end;

procedure TProjectOptions.SetCmdListViewDMXFixName(AValue: boolean);
begin
  if FCmdListViewDMXFixName = AValue then Exit;
  FCmdListViewDMXFixName := AValue;
  Save;
end;

function TProjectOptions.SaveFilename: string;
begin
  Result := ConcatPaths([FSaveFolder, PROJECT_OPTION_FILENAME]);
end;

procedure TProjectOptions.SetMainViewShowAudioControlPanel(AValue: boolean);
begin
  if FMainViewShowAudioControlPanel = AValue then Exit;
  FMainViewShowAudioControlPanel := AValue;
  Save;
end;

procedure TProjectOptions.SetMainViewShowAudioCapturePanel(AValue: boolean);
begin
  if FMainViewShowAudioCapturePanel = AValue then Exit;
  FMainViewShowAudioCapturePanel := AValue;
  Save;
end;

procedure TProjectOptions.SetMainViewShowAudioPanel(AValue: boolean);
begin
  if FMainViewShowAudioPanel = AValue then Exit;
  FMainViewShowAudioPanel := AValue;
  Save
end;

procedure TProjectOptions.SetMainViewShowDMXPanel(AValue: boolean);
begin
  if FMainViewShowDMXPanel = AValue then Exit;
  FMainViewShowDMXPanel := AValue;
  Save
end;

procedure TProjectOptions.InitByDefault;
begin
  EditMode := TRUE;
  FSaveFolder := '';

  FCmdListViewDMXAdress := True;
  FCmdListViewDMXFixName := True;
  FCmdListViewDMXFixDescription := True;
  FCmdListViewDMXChannelName := True;
  FCmdListViewColorAsRectangleColor := True;

  FMainViewShowAudioPanel := True;
  FMainViewShowDMXPanel := True;
  FMainViewShowAudioControlPanel := True;
  FMainViewShowAudioCapturePanel := True;
end;

const
  EDIT_MODE_HEADER='[EDIT MODE]';
  VIEWACTIONLIST_HEADER = '[VIEW ACTION LIST]';
  MAINVIEWPANELTOSHOW_HEADER = '[MAIN VIEW PANEL TO SHOW]';

procedure TProjectOptions.Save;
var t: TStringList;
  f: string;
  prop: TProperties;
begin
  if not Project.IsReady then exit;
  if FLockSave then exit;
  if not DirectoryExists(FSaveFolder) then exit;

  t := TStringList.Create;
  try
    t.Add(EDIT_MODE_HEADER);
    t.Add(BoolToStr(EditMode, 'true', 'false'));

    // projector view
    FormMain.FrameViewProjector1.SaveProjectOptionsTo(t);

    // Frame Main sequence
    FormMain.FrameMainSequence1.SaveProjectOptionsTo(t);

    // cmd list view options
    prop.Init('|');
    prop.Add('ShowDMXAdress', CmdListViewDMXAdress);
    prop.Add('ShowFixtureName', CmdListViewDMXFixName);
    prop.Add('ShowFixtureDescription', CmdListViewDMXFixDescription);
    prop.Add('ShowChannelName', CmdListViewDMXChannelName);
    prop.Add('ShowColorAsRectangleColor', CmdListViewColorAsRectangleColor);
    t.Add(VIEWACTIONLIST_HEADER);
    t.Add(prop.PackedProperty);

    // Main view panel to show
    prop.Init('|');
    prop.Add('ShowAudioPanel', FMainViewShowAudioPanel);
    prop.Add('ShowDMXPanel', FMainViewShowDMXPanel);
    prop.Add('WidthPanelAudio', FormMain.PanelAudio.Width);
    prop.Add('WidthPanelSequence', FormMain.PanelSequence.Width);
    prop.Add('ShowAudioControlPanel', FMainViewShowAudioControlPanel);
    prop.Add('ShowAudioCapturePanel', FMainViewShowAudioCapturePanel);
    t.Add(MAINVIEWPANELTOSHOW_HEADER);
    t.Add(prop.PackedProperty);

    try
      f := SaveFilename;
      t.SaveToFile(f);
    except
      Log.Error('TProjectOptions.Save - Fail to save TStringList to file "'+f+'"');
    end;
  finally
    t.Free;
  end;
end;

procedure TProjectOptions.Load;
var k: integer;
  t: TStringList;
  f: string;
  prop: TProperties;
  bv: boolean;
  procedure LogMissingProperty(const propName: string);
  begin
    Log.Warning('TProjectOptions.Load - '+propName+' property NOT FOUND in '+f);
  end;

begin
  LockSave;

  t := TStringList.Create;
  try
    try
      f := SaveFilename;
      t.LoadFromFile(f);
      Log.Info('Loading project''s options from '+f, 2);
    except
      Log.Error('TProjectOptions.Load - Fail to load TStringList from file "'+f+'"', 2);
      t.Clear;
    end;

    k := t.IndexOf(EDIT_MODE_HEADER);
    if (k <> -1) and (k < t.Count) then
      EditMode := StrToBool(t.Strings[k+1])
    else
      EditMode := True;

    // options for projector view
    FormMain.FrameViewProjector1.LoadProjectOptionsFrom(t);

    // Frame Main sequence
    FormMain.FrameMainSequence1.LoadProjectOptionsFrom(t);

    // option for cmd list view
    k := t.IndexOf(VIEWACTIONLIST_HEADER);
    if (k = -1) or (k = t.Count-1) then
      prop.SetEmpty
    else
      prop.Split(t.Strings[k+1], '|');
    bv := False;// avoid hint
    if not prop.BooleanValueOf('ShowDMXAdress', bv, True) then
      LogMissingProperty('ShowDMXAdress');
    FCmdListViewDMXAdress := bv;
    if not prop.BooleanValueOf('ShowFixtureName', bv, True) then
      LogMissingProperty('ShowFixtureName');
    FCmdListViewDMXFixName := bv;
    if not prop.BooleanValueOf('ShowFixtureDescription', bv, True) then
      LogMissingProperty('ShowFixtureDescription');
    FCmdListViewDMXFixDescription := bv;
    if not prop.BooleanValueOf('ShowChannelName', bv, True) then
      LogMissingProperty('ShowChannelName');
    FCmdListViewDMXChannelName := bv;
    if not prop.BooleanValueOf('ShowColorAsRectangleColor', bv, True) then
      LogMissingProperty('ShowColorAsRectangleColor');
    FCmdListViewColorAsRectangleColor := bv;

    // Main view panel to show
    k := t.IndexOf(MAINVIEWPANELTOSHOW_HEADER);
    if (k = -1) or (k = t.Count-1) then
      prop.SetEmpty
    else
      prop.Split(t.Strings[k+1], '|');
    prop.BooleanValueOf('ShowAudioPanel', FMainViewShowAudioPanel, True);
    prop.BooleanValueOf('ShowDMXPanel', FMainViewShowDMXPanel, True);
    prop.IntegerValueOf('WidthPanelAudio', k, FormMain.Width div 4);
    FormMain.PanelAudio.Width := k;
    prop.IntegerValueOf('WidthPanelSequence', k, FormMain.Width div 4);
    FormMain.PanelSequence.Width := k;
    prop.BooleanValueOf('ShowAudioControlPanel', FMainViewShowAudioControlPanel, True);
    prop.BooleanValueOf('ShowAudioCapturePanel', FMainViewShowAudioCapturePanel, True);

  finally
    t.Free;
  end;

  UnlockSave;
end;

procedure TProjectOptions.LockSave;
begin
  FLockSave := True;
end;

procedure TProjectOptions.UnlockSave;
begin
  FLockSave := False;
end;

end.

