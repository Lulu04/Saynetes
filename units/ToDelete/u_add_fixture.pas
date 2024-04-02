unit u_add_fixture;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, LCLTranslator,
  u_common,
  u_list_dmxuniverse, u_resource_string, u_userdialogs, u_project_manager,
  frame_viewdmxlibrary, frame_viewfixturechannels, frame_viewuniverselist,
  u_dmx_util;

type

  { TFormDMXAddFixture }

  TFormDMXAddFixture = class(TForm)
    BAddUniverse: TSpeedButton;
    BDeleteUniverse: TSpeedButton;
    BDeviceManager: TSpeedButton;
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
    procedure BLoadFromClick(Sender: TObject);
    procedure BOkClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    FrameViewUniverseList1: TFrameViewUniverseList;
    procedure ProcessUniverseSelectionChange(Sender: TObject);
    function SelectedUniverseIndex: integer;
  private
    FrameViewDMXLibrary1: TFrameViewDMXLibrary;
    FrameViewDMXFixtureChannels1: TFrameViewDMXFixtureChannels;
    procedure ProcessDMXLibrarySelectionChangeEvent(Sender: TObject; aFileName: string);
    procedure ProcessDMXLibraryStartDragFixtureEvent(Sender: TObject);
    procedure UpdateWidgets;
  public

  end;

var
  FormDMXAddFixture: TFormDMXAddFixture;

implementation

uses u_mainform, LCLType, u_devicemanager_form;

{$R *.lfm}

{ TFormDMXAddFixture }

procedure TFormDMXAddFixture.FormActivate(Sender: TObject);
begin
  // disable window button on main form
  FormMain.UpdateWidgetState;

  if UniverseManager.Count = 0 then
  begin
    if AskConfirmation(SActuallyThereIsNoUniverse+lineending+SWouldYouLikeToCreateOne, SOk, SNo, mtConfirmation)=mrOk then begin
      BAddUniverseClick(NIL);
    end;
  end;
end;

procedure TFormDMXAddFixture.BAddUniverseClick(Sender: TObject);
begin
  UniverseManager.Add(SUniverse+' '+(UniverseManager.Count+1).ToString);

  FormViewProjector.FrameViewProjector1.FillComboBoxUniverseToShow;
  FormViewProjector.FrameViewProjector1.ExitAddMode;

  FrameViewUniverseList1.AddToView;
  Project.SetModified;
  UpdateWidgets;
end;

procedure TFormDMXAddFixture.BDeleteUniverseClick(Sender: TObject);
var i: integer;
begin
  i:=SelectedUniverseIndex;
  if i=-1 then exit;
  if AskConfirmation(SDeleteThisUniverse+lineending+
          UniverseManager.Universes[i]._Name, SYes, SNo, mtWarning)<>mrOk then exit;
  UniverseManager.Delete(i);
  FormViewProjector.FrameViewProjector1.FillComboBoxUniverseToShow;
  FormViewProjector.FrameViewProjector1.ExitAddMode;
  FrameViewUniverseList1.RemoveSelected;
  Project.SetModified;
  UpdateWidgets;
end;

procedure TFormDMXAddFixture.BDeviceManagerClick(Sender: TObject);
begin
  FormDeviceManager.ShowModal;
  FrameViewUniverseList1.Fill;
end;

procedure TFormDMXAddFixture.BLoadFromClick(Sender: TObject);
begin
  OD1.FileName:=ExtractFilePath(Project.Filename);
  if not OD1.Execute then exit;

  if UniverseManager.LoadFromProject(OD1.FileName) then begin
    ShowMess(SDMXConfSuccessLoaded, SOk, mtCustom);
    Project.SetModified;
    FormViewProjector.FrameViewProjector1.FillComboBoxUniverseToShow;
    FormViewProjector.FrameViewProjector1.View_Center;
  end else begin
    ShowMess(SErrorWhileImportingDMX+Lineending+OD1.FileName, SOk, mtError);
  end;
end;

procedure TFormDMXAddFixture.BOkClick(Sender: TObject);
begin
  Close;
end;

procedure TFormDMXAddFixture.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FormViewProjector.FrameViewProjector1.GUIMode := guiMainDMX;
  FormViewProjector.FrameViewProjector1.Sel_None;
  FormViewProjector.FrameViewProjector1.Redraw;

  FormMain.DockFormToTheRight( FormMainSequenceList );
  FormMain.UpdateWidgetState;

  CloseAction := caHide;
end;

procedure TFormDMXAddFixture.FormCreate(Sender: TObject);
begin
  FrameViewDMXLibrary1:=TFrameViewDMXLibrary.Create(Self);
  FrameViewDMXLibrary1.Parent:=Panel6;
  FrameViewDMXLibrary1.Align:=alClient;
  FrameViewDMXLibrary1.UserChangeEnabled:=FALSE;
  FrameViewDMXLibrary1.OnSelectionChange:=@ProcessDMXLibrarySelectionChangeEvent;
  FrameViewDMXLibrary1.OnStartDragFixture:=@ProcessDMXLibraryStartDragFixtureEvent;

  FrameViewDMXFixtureChannels1:=TFrameViewDMXFixtureChannels.Create(Self);
  FrameViewDMXFixtureChannels1.Parent:=Panel3;
  FrameViewDMXFixtureChannels1.Align:=alClient;
  FrameViewDMXFixtureChannels1.SelectionEnabled:=FALSE;
  FrameViewDMXFixtureChannels1.EditionEnabled:=FALSE;

  FrameViewUniverseList1:=TFrameViewUniverseList.Create(Self);
  FrameViewUniverseList1.Parent:=Panel2;
  FrameViewUniverseList1.Align:=alClient;
  FrameViewUniverseList1.OnSelectionChange:=@ProcessUniverseSelectionChange;
end;

procedure TFormDMXAddFixture.FormHide(Sender: TObject);
begin
  FormViewProjector.FrameViewProjector1.ExitAddMode;
  FormViewProjector.FrameViewProjector1.UpdateButtons;
  FormViewProjector.FrameViewProjector1.BAddDMX.Enabled := True;
  UniverseManager.StartThread;

  FrameViewUniverseList1.StopTimer;
end;

procedure TFormDMXAddFixture.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

procedure TFormDMXAddFixture.FormShow(Sender: TObject);
begin
  Panel8.BevelColor := FormMain.Splitter1.Color;
  Panel8.Color := FormMain.Splitter1.Color;

  FrameViewDMXLibrary1.Init;

  // universes list
  FrameViewUniverseList1.Fill;
  UpdateWidgets;

  Label9.Caption := SUniverse;
end;

procedure TFormDMXAddFixture.ProcessUniverseSelectionChange(Sender: TObject);
begin
  UpdateWidgets;
end;

function TFormDMXAddFixture.SelectedUniverseIndex: integer;
begin
  Result := FrameViewUniverseList1.UniverseIndex;
end;

procedure TFormDMXAddFixture.ProcessDMXLibrarySelectionChangeEvent(Sender: TObject; aFileName: string);
var uni: TDmxUniverse;
begin
  FrameViewDMXFixtureChannels1.ShowFixture(aFileName);//fixfilename);
  if aFilename <> '' then
  begin
    ShowFixtureImage(Image1, FrameViewDMXFixtureChannels1.FixtureType);

    uni := FrameViewUniverseList1.SelectedUniverse;

    if uni <> NIL then
      FormViewProjector.FrameViewProjector1.FixtureFilenameToAdd(aFileName, uni)
    else
      FormViewProjector.FrameViewProjector1.ExitAddMode;
  end
  else Image1.Picture.Assign(NIL);
end;

procedure TFormDMXAddFixture.ProcessDMXLibraryStartDragFixtureEvent(Sender: TObject);
var uni: TDmxUniverse;
begin
  uni := FrameViewUniverseList1.SelectedUniverse;

  if uni <> NIL then
    FormViewProjector.FrameViewProjector1.FixtureFilenameToAdd(FrameViewDMXLibrary1.GetSelectedFixtureFileName, uni)
  else
    FormViewProjector.FrameViewProjector1.ExitAddMode;
end;

procedure TFormDMXAddFixture.UpdateWidgets;
begin
  BDeleteUniverse.Enabled:=FrameViewUniverseList1.UniverseIndex>-1;
end;

end.

