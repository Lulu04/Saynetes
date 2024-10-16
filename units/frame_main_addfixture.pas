unit frame_main_addfixture;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, LCLTranslator, LCLType,
  u_common,
  u_list_dmxuniverse, u_resource_string, u_userdialogs, u_project_manager,
  frame_viewdmxlibrary, frame_viewfixturechannels, frame_viewuniverselist,
  u_dmx_util;

type

  { TFrameMainAddFixture }

  TFrameMainAddFixture = class(TFrame)
    BAddUniverse: TSpeedButton;
    BDeleteUniverse: TSpeedButton;
    BDeviceManager: TSpeedButton;
    BHelp: TSpeedButton;
    BLoadFrom: TSpeedButton;
    BOk: TSpeedButton;
    Image1: TImage;
    Label9: TLabel;
    OD1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure BAddUniverseClick(Sender: TObject);
    procedure BDeleteUniverseClick(Sender: TObject);
    procedure BDeviceManagerClick(Sender: TObject);
    procedure BHelpClick(Sender: TObject);
    procedure BLoadFromClick(Sender: TObject);
    procedure BOkClick(Sender: TObject);
  private
    FrameViewUniverseList1: TFrameViewUniverseList;
    procedure ProcessUniverseSelectionChange(Sender: TObject);
    function SelectedUniverseIndex: integer;
  private
    FDMXLibraryFilled: boolean;
    FrameViewDMXFixtureChannels1: TFrameViewDMXFixtureChannels;
    procedure ProcessDMXLibrarySelectionChangeEvent(Sender: TObject; const aFixtureLocation: TFixtureLibraryLocation);
    procedure ProcessDMXLibraryStartDragFixtureEvent(Sender: TObject; const aFixtureLocation: TFixtureLibraryLocation);
    procedure UpdateWidgets;
  public
    FrameViewDMXLibrary1: TFrameViewDMXLibrary;

    constructor Create(aOwner: TComponent); override;
    procedure EraseBackground({%H-}DC: HDC); override;

    procedure OnShow;
  end;

implementation
uses u_mainform, u_devicemanager_form, form_help;

{$R *.lfm}

{ TFrameMainAddFixture }

procedure TFrameMainAddFixture.BAddUniverseClick(Sender: TObject);
begin
  UniverseManager.Add(SUniverse+' '+(UniverseManager.Count+1).ToString);

  FormMain.FrameViewProjector1.FillComboBoxUniverseToShow;
  FormMain.FrameViewProjector1.ExitAddMode;

  FrameViewUniverseList1.AddToView;
  Project.SetModified;
  UpdateWidgets;
end;

procedure TFrameMainAddFixture.BDeleteUniverseClick(Sender: TObject);
var i: integer;
begin
  i := SelectedUniverseIndex;
  if i = -1 then exit;

  if AskConfirmation(SDeleteThisUniverse+lineending+
          UniverseManager.Universes[i].Name, SYes, SNo, mtWarning) <> mrYes then exit;

  FormMain.FrameViewProjector1.Sel_None;
  FrameViewUniverseList1.RemoveSelected;
  UniverseManager.Delete(i);
  FormMain.FrameViewProjector1.FillComboBoxUniverseToShow;
  FormMain.FrameViewProjector1.ExitAddMode;
  Project.SetModified;
  UpdateWidgets;
  FormMain.CheckSequenceError;
end;

procedure TFrameMainAddFixture.BDeviceManagerClick(Sender: TObject);
begin
  FormDeviceManager.ShowModal;
  FrameViewUniverseList1.Fill;
end;

procedure TFrameMainAddFixture.BHelpClick(Sender: TObject);
begin
  _ShowHelp(HelpEditionDMX, BHelp);
end;

procedure TFrameMainAddFixture.BLoadFromClick(Sender: TObject);
begin
  if UniverseManager.TotalFixtureCount > 0 then begin
    // warn the user s/he will lose the current dmx configuration
    if AskConfirmation(SLoadingAnotherDMXConfigurationFromAnotherProject, SYes, SNo, mtWarning) <> mrYes
      then exit;
  end;

  OD1.FileName := ExtractFilePath(Project.Filename);
  if not OD1.Execute then exit;

  // check if the source and target project are in the same folder
  // do nothing if yes.
  if Project.IsReady and
    (ExtractFilePath(Project.Filename) = ExtractFilePath(OD1.FileName)) then exit;

  // ask user confirmation
  if UniverseManager.Count > 0 then
    if AskConfirmation(SOverwriteDMXConf+LineEnding+SContinue+' ?',
       SContinue, SCancel, mtWarning) <> mrYes then exit;

  if UniverseManager.LoadFromProject(OD1.FileName) then
  begin
    ShowMess(SDMXConfSuccessLoaded, SOk, mtCustom);
    Project.SetModified;
    FormMain.FrameViewProjector1.FillComboBoxUniverseToShow;
    FormMain.FrameViewProjector1.View_Center;
  end
  else ShowMess(SErrorWhileImportingDMX+Lineending+OD1.FileName, SOk, mtError);
end;

procedure TFrameMainAddFixture.BOkClick(Sender: TObject);
begin
  FormMain.FrameViewProjector1.GUIMode := guiMainDMX;
  FormMain.FrameViewProjector1.Sel_None;
  FormMain.FrameViewProjector1.Redraw;

  FormMain.UpdateWidgetState;
end;

procedure TFrameMainAddFixture.ProcessUniverseSelectionChange(Sender: TObject);
begin
  UpdateWidgets;
end;

function TFrameMainAddFixture.SelectedUniverseIndex: integer;
begin
  Result := FrameViewUniverseList1.UniverseIndex;
end;

procedure TFrameMainAddFixture.ProcessDMXLibrarySelectionChangeEvent(
  Sender: TObject; const aFixtureLocation: TFixtureLibraryLocation);
var uni: TDmxUniverse;
begin
  ShowFixtureImage(Image1, aFixtureLocation);

  FrameViewDMXFixtureChannels1.ShowFixture(aFixtureLocation, True);
  if FrameViewDMXFixtureChannels1.Ready then begin
//    ShowFixtureImage(Image1, FrameViewDMXFixtureChannels1.FixtureType);
    uni := FrameViewUniverseList1.SelectedUniverse;
    if uni <> NIL then FormMain.FrameViewProjector1.FixtureFilenameToAdd(aFixtureLocation, uni)
      else FormMain.FrameViewProjector1.ExitAddMode;
  end
  else begin
//    Image1.Picture.Assign(NIL);
  end;
end;

procedure TFrameMainAddFixture.ProcessDMXLibraryStartDragFixtureEvent(Sender: TObject;
  const aFixtureLocation: TFixtureLibraryLocation);
var uni: TDmxUniverse;
begin
  uni := FrameViewUniverseList1.SelectedUniverse;

  if (uni = NIL) or not aFixtureLocation.HaveFixtureAndModeOk then FormMain.FrameViewProjector1.ExitAddMode
    else begin
      FormMain.FrameViewProjector1.FixtureFilenameToAdd(aFixtureLocation, uni);
    end;
end;

procedure TFrameMainAddFixture.UpdateWidgets;
begin
  BDeleteUniverse.Enabled := FrameViewUniverseList1.UniverseIndex > -1;
end;

constructor TFrameMainAddFixture.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FrameViewDMXLibrary1 := TFrameViewDMXLibrary.Create(Self);
  FrameViewDMXLibrary1.Parent := Panel6;
  FrameViewDMXLibrary1.Align := alClient;
  FrameViewDMXLibrary1.OnSelectionChange := @ProcessDMXLibrarySelectionChangeEvent;
  FrameViewDMXLibrary1.OnStartDragFixture := @ProcessDMXLibraryStartDragFixtureEvent;

  FrameViewDMXFixtureChannels1 := TFrameViewDMXFixtureChannels.Create(Self);
  FrameViewDMXFixtureChannels1.Parent := Panel3;
  FrameViewDMXFixtureChannels1.Align := alClient;
  FrameViewDMXFixtureChannels1.SelectionEnabled := FALSE;
  FrameViewDMXFixtureChannels1.EditionEnabled := FALSE;

  FrameViewUniverseList1 := TFrameViewUniverseList.Create(Self);
  FrameViewUniverseList1.Parent := Panel2;
  FrameViewUniverseList1.Align := alClient;
  FrameViewUniverseList1.OnSelectionChange := @ProcessUniverseSelectionChange;
end;

procedure TFrameMainAddFixture.EraseBackground(DC: HDC);
begin
end;

procedure TFrameMainAddFixture.OnShow;
begin
  if not FDMXLibraryFilled then
  begin
    FDMXLibraryFilled := True;
    FrameViewDMXLibrary1.Fill;
  end;

  if UniverseManager.Count = 0 then
  begin
    if AskConfirmation(SActuallyThereIsNoUniverse+lineending+SWouldYouLikeToCreateOne,
          SOk, SNo, mtConfirmation) = mrYes then
      BAddUniverseClick(NIL);
  end;

  FrameViewUniverseList1.Fill;
  Label9.Caption := SUniverse;
end;

end.

