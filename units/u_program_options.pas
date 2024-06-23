unit u_program_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  LCLTranslator, Buttons, StdCtrls, ComCtrls, Spin, DividerBevel,
  u_notebook_util, u_common;

type

  { TFormProgramOptions }

  TFormProgramOptions = class(TForm)
    BApply: TSpeedButton;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    CBStage: TComboBox;
    CBSeats: TComboBox;
    DividerBevel2: TDividerBevel;
    DividerBevel3: TDividerBevel;
    DividerBevel4: TDividerBevel;
    DividerBevel5: TDividerBevel;
    DividerBevel6: TDividerBevel;
    DividerBevel7: TDividerBevel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Notebook1: TNotebook;
    PageDMX: TPage;
    PageAudioDevice: TPage;
    PageSequence: TPage;
    PageAppGeneral: TPage;
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    BOk: TSpeedButton;
    BCancel: TSpeedButton;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    SpinEdit1: TSpinEdit;
    Splitter1: TSplitter;
    TV: TTreeView;
    procedure CBStageSelect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure BOkClick(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure TVSelectionChanged(Sender: TObject);
  private
    FLoadingOptions: boolean;
    FPlaybackDeviceCount, FCaptureDeviceCount: integer;
    procedure RefreshAudioDeviceList;
    procedure UpdateLanguageOnWidgets;
    procedure ProgramOptionsToWidgets;
    procedure WidgetsToProgramOptions;
  private
    FSelectedStageIndex,
    FSelectedSeatIndex: integer;
  public

  end;

type
  TSaynetesLanguage = record
    FullName,
    ShortName: string;
  end;
const
  SupportedLanguages: array [0..1] of TSaynetesLanguage =
     (
       (FullName: 'English'; ShortName: 'en'),
       (FullName: 'Français'; ShortName: 'fr')
     );
type

{ TProgramOptions }

TProgramOptions = class
private
  FLanguage: string;
  FMaxRecentProjectFile: integer;
  FRecentProjects: TStringArray;

  // AUDIO
  FPlaybackDeviceIndex,
  FCaptureDeviceIndex: integer;
  FIntersessionMusicVolume: single;

  // DMX View
  FStageType: TStageType;
  FSeatType: TSeatType;

  // Sequencer
  FKeepOriginVisible: boolean;

  FSaveFolder,
  FSaveFileName: string;
  FWorkingProject,
  FWorkingFolder: string;
  FLockSave: boolean;
  function GetLastProject: string;
  procedure InitByDefault;
  procedure SetKeepOriginVisible(AValue: boolean);
  procedure SetLanguage(AValue: string);
  procedure SetLastProject(AValue: string);
  procedure SetMaxRecentProjectFile(AValue: integer);
  procedure SetSeatType(AValue: TSeatType);
  procedure SetStageType(AValue: TStageType);
  procedure SetWorkingFolder(AValue: string);
  procedure SetWorkingProject(AValue: string);
public
  constructor Create;
  destructor Destroy; override;

  procedure Save;
  procedure Load;

  procedure LockSave;
  procedure UnlockSave;

  procedure RemoveProjectNameFromRecentList(const aProjectName: string);

  // General
  procedure SetLastOpenedProject(const aProjectFileName: string);
  property WorkingProject: string read FWorkingProject write SetWorkingProject;
  property WorkingFolder: string read FWorkingFolder write SetWorkingFolder;

  property LastProjectFileNameUsed: string read GetLastProject write SetLastProject;
  property RecentProjects: TStringArray read FRecentProjects;
  property MaxRecentProjectFile: integer read FMaxRecentProjectFile write SetMaxRecentProjectFile;

  property Language: string read FLanguage write SetLanguage;

  // Audio
  property PlaybackDeviceIndex: integer read FPlaybackDeviceIndex;
  property CaptureDeviceIndex: integer read FCaptureDeviceIndex;
  property IntersessionMusicVolume: single read FIntersessionMusicVolume write FIntersessionMusicVolume;

  // DMX
  property StageType: TStageType read FStageType write SetStageType;
  property SeatType: TSeatType read FSeatType write SetSeatType;

  // Sequencer
  property KeepOriginVisible: boolean read FKeepOriginVisible write SetKeepOriginVisible;

end;

var
  ProgramOptions: TProgramOptions;


implementation
uses LCLType, ALSound, u_project_manager, u_logfile, PropertyUtils,
  u_resource_string, u_apputils, u_dmx_util, BGRABitmap, BGRABitmapTypes,
  Math, Project_util, utilitaire_bgrabitmap;

{$R *.lfm}

{ TProgramOptions }

constructor TProgramOptions.Create;
begin
  FSaveFolder := GetAppConfigFolder;
  FSaveFileName := ConcatPaths([FSaveFolder, APP_CONFIG_FILENAME]);
  if not FileExists(FSaveFileName) then begin
    InitByDefault;
    Save;
  end;
end;

destructor TProgramOptions.Destroy;
begin
  inherited Destroy;
end;

procedure TProgramOptions.InitByDefault;
begin
  // GENERAL
  FRecentProjects := NIL;
  FMaxRecentProjectFile := 5;
  FLanguage := 'en';

  // Audio
  FPlaybackDeviceIndex := 0;
  FCaptureDeviceIndex := 0;
  FIntersessionMusicVolume := 0.5;

  // DMX
  FStageType := stRectangle;
  FSeatType := seatType1;
end;

procedure TProgramOptions.SetKeepOriginVisible(AValue: boolean);
begin
  if FKeepOriginVisible = AValue then Exit;
  FKeepOriginVisible := AValue;
  Save;
end;

procedure TProgramOptions.SetLanguage(AValue: string);
begin
  if FLanguage = AValue then Exit;
  FLanguage := AValue;
  SetDefaultLang(FLanguage);
  Save;

  Project.UpdateStringAfterLanguageChange;
end;

function TProgramOptions.GetLastProject: string;
begin
  if Length(FRecentProjects) > 0 then
    Result := FRecentProjects[0]
  else
    Result := '';
end;

procedure TProgramOptions.SetLastProject(AValue: string);
var i, j: integer;
  flagFound: boolean;
begin
  if not FileExists(AValue) then exit;

  if Length(FRecentProjects) > 0 then
    if FRecentProjects[0] = AValue then exit;

  flagFound := False;
  for i:=0 to High(FRecentProjects) do
    if FRecentProjects[i] = AValue then
    begin
      flagFound := True;
      for j:=i downto 1 do
        FRecentProjects[j] := FRecentProjects[j-1];
      FRecentProjects[0] := AValue;
      break;
    end;

  if not flagFound then
    Insert(AValue, FRecentProjects, 0);

  if Length(FRecentProjects) > FMaxRecentProjectFile then
    Delete(FRecentProjects, FMaxRecentProjectFile, Length(FRecentProjects)-FMaxRecentProjectFile);

  Save;
end;

procedure TProgramOptions.SetMaxRecentProjectFile(AValue: integer);
begin
  if FMaxRecentProjectFile = AValue then Exit;
  FMaxRecentProjectFile := AValue;
  Save;
end;

procedure TProgramOptions.SetSeatType(AValue: TSeatType);
begin
  if FSeatType = AValue then Exit;
  FSeatType := AValue;
end;

procedure TProgramOptions.SetStageType(AValue: TStageType);
begin
  if FStageType = AValue then Exit;
  FStageType := AValue;
end;

procedure TProgramOptions.SetWorkingFolder(AValue: string);
begin
  AValue := ExcludeTrailingPathDelimiter(AValue);
  if FWorkingFolder = AValue then Exit;
  FWorkingFolder := AValue;

  Save;
end;

procedure TProgramOptions.SetWorkingProject(AValue: string);
begin
  if FWorkingProject = AValue then Exit;
  FWorkingProject := AValue;
  Save;
end;

const
  APP_GENERAL_HEADER = '[APPLICATION GENERAL]';
  AUDIO_HEADER = '[AUDIO]';
  DMX_HEADER = '[DMX]';
  SEQUENCER_HEADER = '[SEQUENCER]';

procedure TProgramOptions.Save;
var t: TStringList;
  i: Integer;
  prop: TProperties;
begin
  if FLockSave then exit;

  Log.Info('Saving program options to '+FSaveFileName);
  t := TStringList.Create;
  try
   // Application General
   prop.Init('|');
   prop.Add('Language', FLanguage);
   prop.Add('WorkingProject', FWorkingProject);
   prop.Add('WorkingFolder', FWorkingFolder);
   prop.Add('MaxRecent', FMaxRecentProjectFile);
   for i:=0 to High(FRecentProjects) do
     prop.Add('Recent'+i.ToString, FRecentProjects[i]);

   t.Add(APP_GENERAL_HEADER);
   t.Add(prop.PackedProperty);

   // sequence window
     // height of frames

   // Audio
   prop.Init('|');
   prop.Add('Playback', FPlaybackDeviceIndex);
   prop.Add('Capture', FCaptureDeviceIndex);
   prop.Add('IntersessionMusicVolume', FIntersessionMusicVolume);
   t.Add(AUDIO_HEADER);
   t.Add(prop.PackedProperty);

   // DMX
   prop.Init('|');
   prop.Add('Stage', Ord(FStageType));
   prop.Add('Seat', Ord(FSeatType));
   t.Add(DMX_HEADER);
   t.Add(prop.PackedProperty);

   // Sequencer
   prop.Init('|');
   prop.Add('KeepOriginVisible', FKeepOriginVisible);
   t.Add(SEQUENCER_HEADER);
   t.Add(prop.PackedProperty);

   try
     t.SaveToFile(FSaveFileName);
   except
     Log.Error('TProgramOptions.Save - Error while saving TStringList to file "'+FSaveFileName+'"');
   end;
  finally
    t.Free;
  end;
end;

procedure TProgramOptions.Load;
var i, k: integer;
  t: TStringList;
  prop: TProperties;
  s1: string;
begin
  Log.Info('Loading program options from '+FSaveFileName);
  s1 := '';

  LockSave;
  InitByDefault;
  t := TStringList.Create;
  try
    try
      t.LoadFromFile(FSaveFileName);
    except
      Log.Error('TProgramOptions.Load - Error while loading TStringList from file "'+FSaveFileName+'"');
    end;

    k := t.indexOf(APP_GENERAL_HEADER);
    if (k > -1) and (k < t.Count) then
      prop.Split(t.Strings[k+1], '|')
    else
      prop.SetEmpty;

    if prop.StringValueOf('Language', FLanguage, FLanguage) then
      SetDefaultLang(FLanguage);

    prop.StringValueOf('WorkingProject', s1, WorkingProject);
    if FileExists(s1) then WorkingProject := s1
      else WorkingProject := '';

    prop.StringValueOf('WorkingFolder', s1, WorkingFolder);
    if DirectoryExists(s1) then WorkingFolder := s1
      else WorkingFolder := '';

    prop.IntegerValueOf('MaxRecent', FMaxRecentProjectFile, FMaxRecentProjectFile);
    FRecentProjects := NIL;

    for i:=0 to FMaxRecentProjectFile-1 do
     if prop.StringValueOf('Recent'+i.ToString, s1, '') then
       Insert(s1, FRecentProjects, Length(FRecentProjects));

    // sequence window
      // height of the frame

    // Audio
    k := t.IndexOf(AUDIO_HEADER);
    if (k > -1) and (k < t.Count) then
      prop.Split(t.Strings[k+1], '|')
    else
      prop.SetEmpty;
    prop.integerValueOf('Playback', i, 0);
    FPlaybackDeviceIndex := i;
    prop.integerValueOf('Capture', i, 1);
    FCaptureDeviceIndex := i;
    prop.SingleValueOf('IntersessionMusicVolume', FIntersessionMusicVolume, FIntersessionMusicVolume);

    // DMX
    k := t.IndexOf(DMX_HEADER);
    if (k > -1) and (k < t.Count) then
      prop.Split(t.Strings[k+1], '|')
    else
      prop.SetEmpty;
    prop.integerValueOf('Stage', i, 1);
    FStageType := TStageType(EnsureRange(i, 0, Ord(High(TStageType))));
    prop.integerValueOf('Seat', i, 1);
    FSeatType := TSeatType(EnsureRange(i, 0, Ord(High(TSeatType))));

    // Sequencer
    k := t.IndexOf(SEQUENCER_HEADER);
    if (k > -1) and (k < t.Count) then prop.Split(t.Strings[k+1], '|')
      else prop.SetEmpty;
    prop.BooleanValueOf('KeepOriginVisible', FKeepOriginVisible, True);

  finally
    t.Free;
    UnLockSave;
  end;
end;

procedure TProgramOptions.LockSave;
begin
  FLockSave := True;
end;

procedure TProgramOptions.UnlockSave;
begin
  FLockSave := False;
end;

procedure TProgramOptions.RemoveProjectNameFromRecentList(
  const aProjectName: string);
var i: integer;
begin
  if aProjectName = '' then exit;

  for i:=0 to High(FRecentProjects) do
  begin
    if FRecentProjects[i] = aProjectName then
    begin
      Delete(FRecentProjects, i, 1);
      Save;
      exit;
    end;
  end;
end;

procedure TProgramOptions.SetLastOpenedProject(const aProjectFileName: string);
var flagSave: boolean;
begin
  flagSave := False;
  LockSave;
  if LastProjectFileNameUsed <> aProjectFileName then begin
    LastProjectFileNameUsed := aProjectFileName;
    flagSave := True;
  end;

  if WorkingProject <> aProjectFileName then begin
    WorkingProject := aProjectFileName;
    flagSave := True;
  end;

  if WorkingFolder <> ExcludeTrailingPathDelimiter(ExtractFilePath(aProjectFileName)) then begin
    WorkingFolder := ExtractFilePath(aProjectFileName);
    flagSave := True;
  end;
  UnlockSave;
  if flagSave then Save;
end;

{ TFormProgramOptions }

procedure TFormProgramOptions.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    ModalResult := mrCancel;
end;

procedure TFormProgramOptions.FormCreate(Sender: TObject);
var i: integer;
begin
  ComboBox1.Clear;
  for i:=Low(SupportedLanguages) to High(SupportedLanguages) do
    ComboBox1.Items.Add(SupportedLanguages[i].FullName+' ('+
                        SupportedLanguages[i].ShortName+')');
end;

procedure TFormProgramOptions.CBStageSelect(Sender: TObject);
begin
  PaintBox1.Invalidate;
  if FLoadingOptions then exit;

  if (Sender = CBStage) and (CBStage.ItemIndex <> -1) then begin
    FSelectedStageIndex := CBStage.ItemIndex;
  //  ProgramOptions.StageType := TStageType(CBStage.ItemIndex)
  end;

  if (Sender = CBSeats) and (CBSeats.ItemIndex <> -1) then begin
    FSelectedSeatIndex := CBSeats.ItemIndex;
  //  ProgramOptions.SeatType := TSeatType(CBSeats.ItemIndex);
  end;
end;

procedure TFormProgramOptions.FormShow(Sender: TObject);
begin
  RefreshAudioDeviceList;
  ProgramOptionsToWidgets;
  UpdateLanguageOnWidgets;

  TV.Selected := TV.Items.GetFirstNode;
end;

procedure TFormProgramOptions.BOkClick(Sender: TObject);
begin
  if Sender = BCancel then
  begin
    Close;
    exit;
  end;

  if Sender = BApply then
  begin
    WidgetsToProgramOptions;
    exit;
  end;

  // apply new value and save program options
  WidgetsToProgramOptions;
  ModalResult := mrOk;
end;

procedure TFormProgramOptions.PaintBox1Paint(Sender: TObject);
var ima: TBGRABitmap;
  h, margin: integer;
  f: string;
begin
  with PaintBox1.Canvas do
  begin
    Pen.Color := clActiveCaption;
    Brush.Color := clBlack; // $00242424;
    Brush.Style := bsSolid;
    Rectangle(PaintBox1.ClientRect);
  end;
  margin := ScaleDesignToForm(5);
  h := (PaintBox1.ClientHeight-margin*3) div 2;

  // stage
  f := StageSvgFileFor(TStageType(CBStage.ItemIndex));
  if f <> '' then
  begin
    ima := SVGFileToBGRABitmap(f, -1, h);
    ima.Draw(PaintBox1.Canvas, (PaintBox1.ClientWidth-ima.Width) div 2, margin);
    ima.Free;
  end;

  // seats
  f := SeatSvgFileFor(TSeatType(CBSeats.ItemIndex));
  if f <> '' then
  begin
    ima := SVGFileToBGRABitmap(f, -1, h);
    ima.Draw(PaintBox1.Canvas, (PaintBox1.ClientWidth-ima.Width) div 2, h+margin*2);
    ima.Free;
  end;
end;

procedure TFormProgramOptions.TVSelectionChanged(Sender: TObject);
var nodeParent: TTreeNode;
  txt: string;
begin
  if TV.Selected = NIL then exit;
  txt := 'Selected: '+TV.Selected.Text+' Level '+TV.Selected.Level.ToString+' Index'+TV.Selected.Index.ToString;

  nodeParent := TV.Selected;
  while nodeParent.Level <> 0 do
    nodeParent := nodeParent.Parent;
  txt := txt+' - Parent: '+nodeParent.Text+' Level '+nodeParent.Level.ToString+' Index'+nodeParent.Index.ToString;
  Caption := txt;

  case nodeParent.Index of
   0: // General
     begin
       Notebook1.PageIndex := Notebook1.IndexOf(PageAppGeneral);
     end;
   1: // Sequence
     begin
       Notebook1.PageIndex := Notebook1.IndexOf(PageSequence);
     end;
   2: // Audio
     begin
       Notebook1.PageIndex := Notebook1.IndexOf(PageAudioDevice);
     end;
   3: // DMX
     begin
       Notebook1.PageIndex := Notebook1.IndexOf(PageDMX);
     end;
  end;
end;

procedure TFormProgramOptions.RefreshAudioDeviceList;
var A: TStringArray;
begin
  ComboBox2.Clear;
  A := ALSManager.ListOfPlaybackDeviceName;
  FPlaybackDeviceCount := Length(A);
  if FPlaybackDeviceCount > 0 then ComboBox2.Items.AddStrings(A, False)
    else ComboBox2.Items.Add(SNone);

  ComboBox3.Clear;
  A := ALSManager.ListOfCaptureDeviceName;
  FCaptureDeviceCount := Length(A);
  if FCaptureDeviceCount > 0 then ComboBox3.Items.AddStrings(A, False)
    else ComboBox3.Items.Add(SNone);
end;

procedure TFormProgramOptions.UpdateLanguageOnWidgets;
var n: TTreeNode;
   procedure AddToCB(aCB: TComboBox; aIndex: integer; const aStr: string);
   begin
     if aIndex < aCB.Items.Count then aCB.Items.Strings[aIndex] := aStr
       else aCB.Items.Add(aStr);
   end;

begin
  BOk.Caption := SOk;
  BCancel.Caption := SCancel;
  BApply.Caption := SApply;

  TV.BeginUpdate;
  TV.Items.Clear;
  n := TV.Items.AddFirst(NIL, SGeneral);
  TV.Items.Add(n, SSequence);
  TV.Items.Add(n, SAudio);
  TV.Items.Add(n, SDmx);
  TV.Selected := n;
  TV.EndUpdate;

  Label1.Caption := SRequireTheProgramToBeRestarted;
  Label4.Caption := SRequireTheProgramToBeRestarted;
  Label5.Caption := SRequireTheProgramToBeRestarted;

  AddToCB(CBStage, 0, SNone);
  AddToCB(CBStage, 1, SRectangle);
  AddToCB(CBStage, 2, SQuare);
  AddToCB(CBStage, 3, SHalfCircle);
  AddToCB(CBStage, 4, SEllipse);
  AddToCB(CBStage, 5, SCustom1);

  AddToCB(CBSeats, 0, SNone);
  AddToCB(CBSeats, 1, SSeats1);
  AddToCB(CBSeats, 2, SSeats2);

end;

procedure TFormProgramOptions.ProgramOptionsToWidgets;
var i: integer;
begin
  try
    FLoadingOptions := True;

// Application - General
    // language
    for i:=0 to High(SupportedLanguages) do
      if SupportedLanguages[i].ShortName = ProgramOptions.Language then
      begin
        ComboBox1.ItemIndex := i;
        break;
      end;
    // max recent
    SpinEdit1.Value := ProgramOptions.MaxRecentProjectFile;

// Audio
    if FPlaybackDeviceCount = 0 then ComboBox2.ItemIndex := 0
    else begin
      i := ProgramOptions.FPlaybackDeviceIndex;
      if (i < 0) or (i >= FPlaybackDeviceCount) then i := 0;
      ComboBox2.ItemIndex := i;
    end;
    i := ProgramOptions.FCaptureDeviceIndex;
    if (i < 0) or (i >= FCaptureDeviceCount) then i := 0;
    ComboBox3.ItemIndex := i;

// DMX
    FSelectedStageIndex := Ord(ProgramOptions.StageType);
    CBStage.ItemIndex := FSelectedStageIndex;
    FSelectedSeatIndex := Ord(ProgramOptions.SeatType);
    CBSeats.ItemIndex := FSelectedSeatIndex;
  finally
    FLoadingOptions := False;
  end;

end;

procedure TFormProgramOptions.WidgetsToProgramOptions;
begin
  if FLoadingOptions then exit;

  ProgramOptions.LockSave;
  try
// Application - General
   // language
   ProgramOptions.Language := SupportedLanguages[ComboBox1.ItemIndex].ShortName;
   // max recent
   ProgramOptions.MaxRecentProjectFile := SpinEdit1.Value;

   // Audio
   if FPlaybackDeviceCount = 0 then ProgramOptions.FPlaybackDeviceIndex := -1
     else ProgramOptions.FPlaybackDeviceIndex := ComboBox2.ItemIndex;
   if FCaptureDeviceCount = 0 then ProgramOptions.FCaptureDeviceIndex := -1
     else ProgramOptions.FCaptureDeviceIndex := ComboBox3.ItemIndex;

   // DMX
   ProgramOptions.StageType := TStageType(FSelectedStageIndex);
   ProgramOptions.SeatType := TSeatType(FSelectedSeatIndex);
  finally
    ProgramOptions.UnlockSave;
    ProgramOptions.Save;
  end;
   UpdateLanguageOnWidgets;

end;

end.

