unit u_mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, Menus, LMessages, LCLTranslator, Buttons,
  u_common, lcl_utils, u_apputils,
  frame_main_audio, frame_viewprojectors, frame_main_sequence,
  frame_main_addfixture;


type

  { TFormMain }

  TFormMain = class(TForm)
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MIProjectOptions: TMenuItem;
    MIProjectClose: TMenuItem;
    MIToolDeviceManager: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MIProgramOptions: TMenuItem;
    MIToolDMXLibrary: TMenuItem;
    MIProjectQuit: TMenuItem;
    MIProjectSave: TMenuItem;
    MIProjectNew: TMenuItem;
    MIProjectOpen: TMenuItem;
    Panel1: TPanel;
    PanelAudio: TPanel;
    PanelDMX: TPanel;
    PanelSequence: TPanel;
    BToogleEditMode: TSpeedButton;
    BAudio: TSpeedButton;
    BLight: TSpeedButton;
    Panel4: TPanel;
    Shape4: TShape;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MIProjectCloseClick(Sender: TObject);
    procedure MIProjectNewClick(Sender: TObject);
    procedure MIProjectOpenClick(Sender: TObject);
    procedure MIProjectOptionsClick(Sender: TObject);
    procedure MIProjectQuitClick(Sender: TObject);
    procedure MIProjectSaveClick(Sender: TObject);
    procedure MIToolDeviceManagerClick(Sender: TObject);
    procedure MIToolDMXLibraryClick(Sender: TObject);
    procedure MIProgramOptionsClick(Sender: TObject);
    procedure BToogleEditModeClick(Sender: TObject);
    procedure BAudioClick(Sender: TObject);
    procedure Splitter2Moved(Sender: TObject);
  private
   // procedure PlayerMessageHandler(var Message: TLMessage); message LM_MESSAGE_Player;
   // procedure DMXUniverseMessageHandler(var Message: TLMessage); message LM_MESSAGE_DMXUniverse;
    procedure MainGuiMessageHandler(var Message: TLMessage); message LM_MESSAGE_MainGui;
  private
    FToogleSpeedButtonManager: TToggleSpeedButtonManager;
    FStartup: boolean;
  public
    FrameMainAudio1: TFrameMainAudio;
    FrameViewProjector1: TFrameViewProjector;
    FrameMainSequence1: TFrameMainSequence;
    FrameMainAddFixture1: TFrameMainAddFixture;

    procedure OnProjectModified;
    procedure OnProjectReadyChange;

    procedure UpdateWidgetState;
    procedure UpdateLayout;

    // called when user delete an audio file, a dmx fixture or an universe
    procedure CheckSequenceError;
end;

var
  FormMain: TFormMain;

implementation
uses LCLType, LCLIntf, u_dmx_library, u_project_manager, u_userdialogs,
  u_resource_string, u_devicemanager_form, u_startupwizard, u_logfile,
  u_program_options, u_audio_manager, u_sequence_player, u_list_sequence,
  u_list_dmxuniverse, u_project_options, form_about, form_splash, u_dmx_util;

{$R *.lfm}

{ TFormMain }

procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if FrameMainAudio1.MouseIsOver then
    FrameMainAudio1.ProcessKeyDown(Key, Shift)
  else if FrameMainSequence1.MouseIsOver then
    FrameMainSequence1.ProcessKeyDown(Key, Shift)
  else if FrameViewProjector1.MouseIsOver then
    FrameViewProjector1.ProcessKeyDown(Key, Shift);
end;

procedure TFormMain.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if FrameMainAudio1.MouseIsOver then
    FrameMainAudio1.ProcessKeyUp(Key, Shift)
  else if FrameMainSequence1.MouseIsOver then
    FrameMainSequence1.ProcessKeyUp(Key, Shift)
  else if FrameViewProjector1.MouseIsOver then
    FrameViewProjector1.ProcessKeyUp(Key, Shift);
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  UpdateWidgetState;
  OnProjectModified;

  if not FileExists(ProgramOptions.WorkingProject)
    then PostMessage(Handle, LM_MESSAGE_MainGui, 0, MESS_MainGui_StartupWizard)
    else Project.Load(ProgramOptions.WorkingProject);
end;

procedure TFormMain.MenuItem6Click(Sender: TObject);
var F: TFormAbout;
begin
  F := TFormAbout.Create(NIL);
  try
    F.ShowModal;
  finally
    F.Free;
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FStartup := True;

  FormSplash.TextLoad := SLoadingFixtureImage;
  LoadFixtureImages;
  FormSplash.TextLoad := SLoadingCursorImage;
  LoadCursorImages(ScaleDesignToForm(25), ScaleDesignToForm(35));
  FormSplash.TextLoad := SLoadingDmxEffectsImage;
  LoadDmxEffectsImages(ScaleDesignToForm(14), -1);

  // customize hint window
  CustomizeHintWindow;

  Project := TSaynetesProject.Create;

  FToogleSpeedButtonManager := TToggleSpeedButtonManager.Create;
  with FToogleSpeedButtonManager do begin
    SetImageIndexes(3, -1);
    SetActivatedColors($0003C4FC, clBlack);
    SetDeactivatedColors( $00484848, $00EAEAEA);
    ToggleType := tsbAtLeastOne; //tsbLikeradioButton;
    Add(BAudio, True);
    Add(BLight, True);
  end;

  FrameMainAudio1 := TFrameMainAudio.Create(Self);
  FrameMainAudio1.Parent := PanelAudio;
  FrameMainAudio1.Align := alClient;

  FrameViewProjector1 := TFrameViewProjector.Create(Self);
  FrameViewProjector1.Parent := PanelDMX;
  FrameViewProjector1.Align := alClient;
  //FrameViewProjector1.Splitter1.Top := Round(FrameViewProjector1.ClientHeight*0.6);

  FrameMainSequence1 := TFrameMainSequence.Create(Self);
  FrameMainSequence1.Parent := PanelSequence;
  FrameMainSequence1.Align := alClient;

  FrameMainAddFixture1 := TFrameMainAddFixture.Create(Self);
  FrameMainAddFixture1.Parent := PanelSequence;
  FrameMainAddFixture1.Align := alClient;
  FrameMainAddFixture1.Visible := False;

  Project.UpdateStringAfterLanguageChange;
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := Project.DoUserPromptToSaveProject;

  if CanClose then begin
    Log.AddEmptyLine;
    Log.info('Saynètes: Application Shut down', 0, True);
    Log.Info('Save program''s options', 1);
    ProgramOptions.IntersessionMusicVolume := self.FrameMainSequence1.FrameIntersessionMusic1.Volume;
    ProgramOptions.Save;
    Log.Info('Stop thread universe manager', 1);
    UniverseManager.StopThread;
    Log.Info('Stop all sequences', 1);
    Sequences.StopAll;
    Log.Info('Stop sequence player preview', 1);
    SeqPlayer.StopPreview;
    Log.Info('Sound manager reset state', 1);
    SoundManager.ResetState;

    FrameViewProjector1.FreeOpenGLTextures;
  end;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FToogleSpeedButtonManager.Free;
  Project.Free;
  ProgramOptions.Free;
  FreeFixtureImages;
  FreeCursorImages;
  FreeDmxEffectsImages;
end;

procedure TFormMain.MIProjectCloseClick(Sender: TObject);
begin
  Project.Close;
  ProgramOptions.WorkingProject := '';
  PostMessage(Handle, LM_MESSAGE_MainGui, 0, MESS_MainGui_StartupWizard);
end;

procedure TFormMain.MIProjectNewClick(Sender: TObject);
begin
  Project.New;

  if not Project.IsReady then
    PostMessage(Handle, LM_MESSAGE_MainGui, 0, MESS_MainGui_StartupWizard);
end;

procedure TFormMain.MIProjectOpenClick(Sender: TObject);
begin
  Project.KeepUniverseManager := False;
  Project.Load;

  if not Project.IsReady then
    PostMessage(Handle, LM_MESSAGE_MainGui, 0, MESS_MainGui_StartupWizard)
  else // update main form project's folder
    FrameMainSequence1.FrameViewProjectFolder1.Fill;
end;

procedure TFormMain.MIProjectOptionsClick(Sender: TObject);
var F: TFormProjectOptions;
begin
  F := TFormProjectOptions.Create(NIL);
  try
    if F.ShowModal <> mrOk then exit;
    UpdateLayout;
  finally
    F.Free;
  end;
end;

procedure TFormMain.MIProjectQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.MIProjectSaveClick(Sender: TObject);
begin
  Project.Save;
end;

procedure TFormMain.MIToolDeviceManagerClick(Sender: TObject);
begin
  FormDeviceManager.ShowModal;
end;

procedure TFormMain.MIToolDMXLibraryClick(Sender: TObject);
var F: TFormDMXLibrary;
begin
  F := TFormDMXLibrary.Create(NIL);
  try
    F.ShowModal;
  finally
    F.Free;
  end;
  // update the content
  FrameMainAddFixture1.FrameViewDMXLibrary1.Fill;
end;

procedure TFormMain.MIProgramOptionsClick(Sender: TObject);
var F: TFormProgramOptions;
begin
  F := TFormProgramOptions.Create(NIL);
  try
    F.ShowModal;

    FrameViewProjector1.ForceReconstructOpenGLObjects;
  finally
    F.Free;
  end;
end;

procedure TFormMain.BToogleEditModeClick(Sender: TObject);
begin
  Project.Options.EditMode := not Project.Options.EditMode;

  if Project.Options.EditMode
    then ShowMess(SEntering+' '+SEditMode+LINEENDING+SEditionAllowed, SOk, mtInformation)
    else ShowMess(SEntering+' '+SShowMode+LINEENDING+SEditionNotAllowed, SOk, mtInformation);

  FrameMainAudio1.UpdateEditMode;
  FrameViewProjector1.UpdateEditMode;
end;

procedure TFormMain.BAudioClick(Sender: TObject);
begin
  // save to project's options
  Project.Options.Save;

  if FToogleSpeedButtonManager.Checked[BAudio] and
     not FToogleSpeedButtonManager.Checked[BLight] then
  begin // show only audio panel
    PanelDMX.Width := 0;
    PanelDMX.Visible := False;
    Splitter2.Visible := False;
    PanelAudio.Width := Splitter1.Left-1;
  end
  else if not FToogleSpeedButtonManager.Checked[BAudio] and
          FToogleSpeedButtonManager.Checked[BLight] then
  begin // show only light panel
    PanelAudio.Width := 0;
    Splitter2.Visible := False;
    PanelDMX.Visible := True;
    PanelDMX.Left := 0;
  end
  else begin // show both
    PanelAudio.Width := Splitter1.Left div 2;
    Splitter2.Visible := True;
    PanelDMX.Visible := True;
  end;
end;

procedure TFormMain.Splitter2Moved(Sender: TObject);
begin
  Project.Options.Save;
end;

{
procedure TFormMain.PlayerMessageHandler(var Message: TLMessage);
begin
  if Assigned(TopPlayer) then begin
    TopPlayer.Update;
  end;
end;

procedure TFormMain.DMXUniverseMessageHandler(var Message: TLMessage);
begin
  case Message.lParam of
   MESS_DMXUniverse_Update: UniverseManager.Update;
  end;//case
end;   }

procedure TFormMain.MainGuiMessageHandler(var Message: TLMessage);
var F: TFormStartUpWizard;
begin
  case Message.lParam of
   MESS_MainGui_StartupWizard: begin
     F:=TFormStartUpWizard.Create(NIL);
     try
       if F.ShowModal=mrOk then begin
         if F.NewProject then MIProjectNewClick(NIL);
         if F.OpenRecent<>'' then Project.Load(F.OpenRecent);
         if F.OpenProject then MIProjectOpenClick(NIL);
         if F.Quit then MIProjectQuitClick(NIL);
       end;
     finally
       F.Free;
     end;
   end;
  end;//case
end;

procedure TFormMain.OnProjectModified;
begin
  MIProjectSave.Enabled := Project.HasBeenModified;
  SpeedButton3.Enabled := Project.HasBeenModified;
end;

procedure TFormMain.OnProjectReadyChange;
begin
  PanelAudio.Enabled := Project.IsReady;
  PanelDMX.Enabled := Project.IsReady;

  BToogleEditMode.Enabled := Project.IsReady;

  MIProjectClose.Enabled := Project.IsReady;


  MIProjectOptions.Enabled := Project.IsReady;
  SpeedButton4.Enabled := Project.IsReady;


 // UpdateWidgetState;

  FrameViewProjector1.FillComboBoxUniverseToShow;
  FrameViewProjector1.View_Center;
  FrameViewProjector1.GUIMode := guiMainDmx;

  FrameMainSequence1.Fill;
  FrameMainSequence1.UpdateLayout;

  FrameMainAudio1.Fill;
  FrameMainAudio1.UpdateLayout;
end;

procedure TFormMain.UpdateWidgetState;
begin
  case Project.Options.EditMode of
   TRUE: BToogleEditMode.Caption := SEditMode;
   FALSE: BToogleEditMode.Caption := SShowMode
  end;

  BAudio.Enabled := FrameViewProjector1.GUIMode <> guiPrepaDMX;
  BLight.Enabled := BAudio.Enabled;

  BToogleEditMode.Enabled := FrameViewProjector1.GUIMode <> guiPrepaDMX;

  Panel1.Enabled := FrameViewProjector1.GUIMode <> guiPrepaDMX;

  FrameMainSequence1.Visible := FrameViewProjector1.GUIMode = guiMainDMX;
  FrameMainAddFixture1.Visible := FrameViewProjector1.GUIMode = guiPrepaDMX;
end;

procedure TFormMain.UpdateLayout;
begin
  FrameMainAudio1.UpdateLayout;
  FrameMainSequence1.UpdateLayout;
end;

procedure TFormMain.CheckSequenceError;
begin
  if Sequences.CheckErrorInSequences then
    FrameMainSequence1.FrameViewTopList1.LB.Invalidate;
end;


end.

