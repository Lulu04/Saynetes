unit u_prepa_dmx;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Buttons, ComCtrls, ExtCtrls, StdCtrls, Menus, LCLTranslator, u_common,
  u_list_dmxuniverse, u_resource_string, u_userdialogs, u_project_manager,
  frame_viewdmxlibrary, frame_viewfixturechannels,
  frame_viewuniverselist, frame_viewfixtureinfo,
  u_dmx_util;


type

  { TFormPrepaDMX }

  TFormPrepaDMX = class(TForm)
    BDeviceManager: TSpeedButton;
    Image1: TImage;
    Label9: TLabel;
    OD1: TOpenDialog;
    Panel1: TPanel;
    BLoadFrom: TSpeedButton;
    BAddUniverse: TSpeedButton;
    BDeleteUniverse: TSpeedButton;
    BOk: TSpeedButton;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    procedure BAddUniverseClick(Sender: TObject);
    procedure BDeleteUniverseClick(Sender: TObject);
    procedure BDeviceManagerClick(Sender: TObject);
    procedure BLoadFromClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BOkClick(Sender: TObject);
  private
    FrameViewUniverseList1: TFrameViewUniverseList;
    procedure ProcessUniverseSelectionChange(Sender: TObject);
    function SelectedUniverseIndex: integer;
  private
    FrameViewDMXLibrary1: TFrameViewDMXLibrary;
    FrameViewDMXFixtureChannels1: TFrameViewDMXFixtureChannels;
    FrameFixtureInfo1: TFrameFixtureInfo;
    procedure ProcessDMXLibrarySelectionChangeEvent(Sender: TObject; aFileName: string);
    procedure ProcessDMXLibraryStartDragFixtureEvent(Sender: TObject);
  private
    function SelectedUniverse: TDMXUniverse;
    procedure ProcessProjectorViewFixtureSelectionChange(Sender: TObject);
    procedure processProjectorViewOnDeleteFixture(Sender: TObject);
  public
    procedure UpdateWidgets;

  end;

implementation

uses LCLType, u_devicemanager_form, u_main_viewprojector;

{ TFormPrepaDMX }

procedure TFormPrepaDMX.FormCreate(Sender: TObject);
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

  FrameViewDMXProjectors1:=TFrameViewDMXProjectors.Create(Self);
  FrameViewDMXProjectors1.Parent:=Panel8;
  FrameViewDMXProjectors1.Align:=alClient;
  FrameViewDMXProjectors1.SetViewModePrepaDMX;
  FrameViewDMXProjectors1.OnFixtureSelectionChange:=@ProcessProjectorViewFixtureSelectionChange;
  FrameViewDMXProjectors1.OnDeleteFixture:=@processProjectorViewOnDeleteFixture;

  FrameViewUniverseList1:=TFrameViewUniverseList.Create(Self);
  FrameViewUniverseList1.Parent:=Panel2;
  FrameViewUniverseList1.Align:=alClient;
  FrameViewUniverseList1.OnSelectionChange:=@ProcessUniverseSelectionChange;

  FrameFixtureInfo1:=TFrameFixtureInfo.Create(Self);
  FrameFixtureInfo1.Parent:=Panel9;
  FrameFixtureInfo1.TargetFrameProjector:=FrameViewDMXProjectors1;
  FrameFixtureInfo1.Left:=0;
  FrameFixtureInfo1.Top:=0;
  Panel9.Width:=FrameFixtureInfo1.Width;
  Panel9.Height:=FrameFixtureInfo1.Height;
end;

procedure TFormPrepaDMX.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  FrameViewDMXProjectors1.ProcessKey(Key, Shift);
end;

procedure TFormPrepaDMX.FormResize(Sender: TObject);
begin
end;

procedure TFormPrepaDMX.FormShow(Sender: TObject);
begin
  UniverseManager.Sel_None;
  FrameViewDMXLibrary1.Init;

  FrameViewDMXProjectors1.Sel_None;
  FrameViewDMXProjectors1.FillComboBoxUniverseToShow;
  FrameViewDMXProjectors1.ReloadTexture;
  FrameViewDMXProjectors1.View_Center;
  FrameViewDMXProjectors1.SpeedButton4.Visible:=FALSE;

  Panel9.Visible:=FALSE;

  // universes list
  FrameViewUniverseList1.Fill;
  UpdateWidgets;
end;

procedure TFormPrepaDMX.BOkClick(Sender: TObject);
begin
  Close;
end;

procedure TFormPrepaDMX.ProcessUniverseSelectionChange(Sender: TObject);
begin
  UpdateWidgets;
end;

function TFormPrepaDMX.SelectedUniverseIndex: integer;
begin
  Result:=FrameViewUniverseList1.UniverseIndex;
end;

procedure TFormPrepaDMX.ProcessDMXLibrarySelectionChangeEvent(Sender: TObject; aFileName: string);
//var fixfilename: string;
begin
//  fixfilename:=FrameViewDMXLibrary1.GetSelectedFixtureFileName;
  FrameViewDMXFixtureChannels1.ShowFixture(aFileName);//fixfilename);
  if aFilename<>'' then begin
    ShowFixtureImage(Image1, FrameViewDMXFixtureChannels1.FixtureType);

    if SelectedUniverseIndex<>-1 then begin
      FrameViewDMXProjectors1.FixtureFilenameToAdd(aFileName, SelectedUniverse);
    end else begin
      FrameViewDMXProjectors1.ExitAddMode;
    end;
  end else begin
    Image1.Picture.Assign(NIL);
  end;
end;

procedure TFormPrepaDMX.ProcessDMXLibraryStartDragFixtureEvent(Sender: TObject);
begin
  if SelectedUniverseIndex<>-1 then begin
    FrameViewDMXProjectors1.FixtureFilenameToAdd(FrameViewDMXLibrary1.GetSelectedFixtureFileName, SelectedUniverse);
  end else begin
    FrameViewDMXProjectors1.ExitAddMode;
  end;
end;

function TFormPrepaDMX.SelectedUniverse: TDMXUniverse;
begin
  if SelectedUniverseIndex=-1
  then Result:=NIL
  else Result:=UniverseManager.Universes[SelectedUniverseIndex];
end;

procedure TFormPrepaDMX.ProcessProjectorViewFixtureSelectionChange(Sender: TObject);
begin
  case FrameViewDMXProjectors1.SelectedCount of
    1: begin
        FrameFixtureInfo1.UpdateView;
        Panel9.Height:=FrameFixtureInfo1.ViewHeight;
        Panel9.Visible:=TRUE;
    end;
    else begin
        Panel9.Visible:=FALSE;
    end;
  end;//case
end;

procedure TFormPrepaDMX.processProjectorViewOnDeleteFixture(Sender: TObject);
begin
  Panel9.Visible:=FALSE;
end;

procedure TFormPrepaDMX.UpdateWidgets;
begin
 BDeleteUniverse.Enabled:=FrameViewUniverseList1.UniverseIndex>-1;
end;

procedure TFormPrepaDMX.BAddUniverseClick(Sender: TObject);
begin
  UniverseManager.Add(SUniverse+' '+(UniverseManager.Count+1).ToString);
  FrameViewDMXProjectors1.FillComboBoxUniverseToShow;
  FrameViewDMXProjectors1.ExitAddMode;

  FrameViewUniverseList1.AddToView;
  Project.SetModified;
  UpdateWidgets;
end;

procedure TFormPrepaDMX.BDeleteUniverseClick(Sender: TObject);
var i: integer;
begin
  i:=SelectedUniverseIndex;
  if i=-1 then exit;
  if AskConfirmation(SDeleteThisUniverse+lineending+
          UniverseManager.Universes[i]._Name, SYes, SNo, mtWarning)<>mrOk then exit;
  UniverseManager.Delete(i);
  FrameViewDMXProjectors1.FillComboBoxUniverseToShow;
  FrameViewDMXProjectors1.ExitAddMode;
  FrameViewUniverseList1.RemoveSelected;
  Project.SetModified;
  UpdateWidgets;
end;

procedure TFormPrepaDMX.BDeviceManagerClick(Sender: TObject);
begin
  FormDeviceManager.ShowModal;
  FrameViewUniverseList1.Fill;
end;

procedure TFormPrepaDMX.BLoadFromClick(Sender: TObject);
begin
  OD1.FileName:=ExtractFilePath(Project.Filename);
  if not OD1.Execute then exit;

  if UniverseManager.LoadFromProject(OD1.FileName) then begin
    ShowMess(SDMXConfSuccessLoaded, SOk, mtCustom);
    Project.SetModified;
    FrameViewDMXProjectors1.FillComboBoxUniverseToShow;
    FrameViewDMXProjectors1.View_Center;
  end else begin
    ShowMess(SErrorWhileImportingDMX+Lineending+OD1.FileName, SOk, mtError);
  end;
end;

procedure TFormPrepaDMX.FormActivate(Sender: TObject);
begin
  if UniverseManager.Count=0 then begin
    if AskConfirmation(SActuallyThereIsNoUniverse+lineending+SWouldYouLikeToCreateOne, SOk, SNo, mtConfirmation)=mrOk then begin
      BAddUniverseClick(NIL);
    end;
  end;
end;

procedure TFormPrepaDMX.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FrameViewUniverseList1.StopTimer;
end;

initialization
  {$I u_prepa_dmx.lrs}

end.

