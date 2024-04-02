unit u_dmxtools_group;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, Menus, LCLTranslator,
  u_list_dmxuniverse, u_common, u_notebook_util,
  frame_viewprojectors;

type

  TChannelGroup=class
    Name: string;
    A: array of TChannelPath;
  end;

  TRGBGroup=class
    Name: string;
    A: array of TFixturePath;
  end;

  { TFormDMXGroup }

  TFormDMXGroup = class(TForm)
    BLoadFrom: TSpeedButton;
    LBChannels: TListBox;
    LBRGB: TListBox;
    MIRGBRename: TMenuItem;
    MIRGBDelete: TMenuItem;
    MIChannelRename: TMenuItem;
    MIChannelDelete: TMenuItem;
    Notebook1: TNotebook;
    OD1: TOpenDialog;
    PageRGB: TPage;
    PageChannels: TPage;
    PopChannels: TPopupMenu;
    PopRGB: TPopupMenu;
    Shape1: TShape;
    Shape2: TShape;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    procedure BLoadFromClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var {%H-}Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure LBChannelsMouseDown(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, Y: Integer);
    procedure LBChannelsSelectionChange(Sender: TObject; {%H-}User: boolean);
    procedure LBRGBMouseDown(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, Y: Integer);
    procedure LBRGBSelectionChange(Sender: TObject; {%H-}User: boolean);
    procedure MIChannelDeleteClick(Sender: TObject);
    procedure MIChannelRenameClick(Sender: TObject);
    procedure MIRGBDeleteClick(Sender: TObject);
    procedure MIRGBRenameClick(Sender: TObject);
    procedure PopChannelsPopup(Sender: TObject);
    procedure PopRGBPopup(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
  private
    FNoteBookManager: TNoteBookManager;
    FShiftPressed: boolean;
    FAddingNewGroup: boolean;
    procedure SetGUIMode(AValue: TGUIMode);
    procedure ViewPage(aPage: TPage);
    function GetSelectedChannelGroup: TChannelGroup;
    function GetSelectedRGBGroup: TRGBGroup;
  public
    FTargetViewProjector: TFrameViewProjector;
    procedure AddChannelGroup(Chans: ArrayOfDmxChannels);
    procedure AddRGBGroup(Fixs: ArrayOfDMXFixtures);

    procedure Clear;
    procedure SaveTo(const aFilename: string);
    procedure LoadFrom(const aFilename: string);

    procedure UpdateEditMode;

    property GUIMode: TGUIMode write SetGUIMode;
  end;

var
  FormDMXGroup: TFormDMXGroup;

implementation

uses u_userdialogs, u_resource_string, u_project_manager, u_helper, u_mainform,
  u_add_action_dmx, u_logfile, u_dmx_util, LCLType;

{$R *.lfm}

{ TFormDMXGroup }

procedure TFormDMXGroup.FormCreate(Sender: TObject);
begin
  FNoteBookManager := TNoteBookManager.Create(Notebook1);
  with FNoteBookManager do
  begin
    SetActivatedColors($0003C4FC, clBlack);
    SetDeactivatedColors($00484848, $00EAEAEA);
    LinkButtonToPage(SpeedButton1, PageChannels);
    LinkButtonToPage(SpeedButton2, PageRGB);
    ActivePage(PageChannels);
  end;
end;

procedure TFormDMXGroup.FormDeactivate(Sender: TObject);
begin
  FShiftPressed := False;
end;

procedure TFormDMXGroup.BLoadFromClick(Sender: TObject);
var f: string;
begin
  OD1.FileName := ExtractFilePath(Project.Filename);
  if OD1.Execute then
  begin
    try
      f := ConcatPaths([ExtractFilePath(OD1.FileName),
                        COMMON_PROJECT_FOLDER_NAME,
                        COMMON_PROJECT_DMX_GROUP_FILENAME]);
      if FileExists(f) then
      begin
        LoadFrom(f);
        ShowMess(SDMXGroupsSuccessLoaded, SOk, mtCustom);
        Project.SetModified;
      end;
    except
      ShowMess(SErrorWhileImportingDMXGroups+Lineending+OD1.FileName, SOk, mtError);
    end;
  end;
end;

procedure TFormDMXGroup.FormDestroy(Sender: TObject);
begin
  Clear;
  FNoteBookManager.Free;
end;

procedure TFormDMXGroup.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  FShiftPressed := Shift >= [ssShift];
end;

procedure TFormDMXGroup.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
  FShiftPressed := not(Shift >= [ssShift]);
end;

procedure TFormDMXGroup.FormShow(Sender: TObject);
begin
  SpeedButton1.Caption := SChannels_;
  SpeedButton2.Caption := SRGB;
  MIChannelDelete.Caption := SDelete;
  MIChannelRename.Caption := SRename;
  MIRGBDelete.Caption := SDelete;
  MIRGBRename.Caption := SRename;

  FShiftPressed := False;
end;

procedure TFormDMXGroup.LBChannelsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if LBChannels.GetIndexAtY(Y) = -1 then
    LBChannels.ItemIndex := -1;
end;

procedure TFormDMXGroup.LBChannelsSelectionChange(Sender: TObject; User: boolean );
var cg: TChannelGroup;
  i: integer;
  uni: TDMXUniverse;
  fix: TDMXFixture;
  chan: TDMXChannel;
begin
  if FAddingNewGroup then exit;
  if not FShiftPressed then
  begin
    FTargetViewProjector.Sel_None;
    FTargetViewProjector.Redraw;
  end;

  cg := GetSelectedChannelGroup;
  if cg = NIL then
    exit;

  for i:=0 to High(cg.A) do
  begin
    UniverseManager.RetrieveChannel(cg.A[i].IDUni, cg.A[i].IDFix, cg.A[i].ChanIndex, uni, fix, chan);
    if (uni <> NIL) and (fix <> NIL) and (chan <> NIL) then
    begin
      if not fix.Selected then
      begin
        FTargetViewProjector.AddToSelected(fix);
        fix.UnselectAllChannels;
      end;
      chan.Selected := TRUE;
    end;
  end;
  FTargetViewProjector.FrameViewDMXCursors1.UpdateView;
  FTargetViewProjector.Redraw;
end;

procedure TFormDMXGroup.LBRGBMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if LBRGB.GetIndexAtY(Y) = -1 then
    LBRGB.ItemIndex := -1;
end;

procedure TFormDMXGroup.LBRGBSelectionChange(Sender: TObject; User: boolean);
var rgbg: TRGBGroup;
  i: integer;
  uni: TDMXUniverse;
  fix: TDMXFixture;
begin
  if FAddingNewGroup then exit;
  if not FShiftPressed then
  begin
    FTargetViewProjector.Sel_None;
    FTargetViewProjector.Redraw;
  end;

  rgbg := GetSelectedRGBGroup;
  if rgbg = NIL then
    exit;

  for i:=0 to High(rgbg.A) do
  begin
    UniverseManager.RetrieveFixture(rgbg.A[i].IDUni, rgbg.A[i].IDFix, uni, fix);
    if fix <> NIL then
      FTargetViewProjector.AddToSelected(fix);
  end;
  FTargetViewProjector.Redraw;
end;

procedure TFormDMXGroup.MIChannelDeleteClick(Sender: TObject);
var i: integer;
begin
  i := LBChannels.ItemIndex;
  if i = -1 then
    exit;
  TChannelGroup(LBChannels.Items.Objects[i]).Free;
  LBChannels.Items.Delete(i);
  FTargetViewProjector.Sel_None;
  FTargetViewProjector.Redraw;
  Project.SetModified;
end;

procedure TFormDMXGroup.MIChannelRenameClick(Sender: TObject);
var g: TChannelGroup;
  n: string;
begin
  g := GetSelectedChannelGroup;
  if g = NIL then
    exit;

  n := g.Name;
  if UserInputNoSpecialChar(SNewName, SOk, SCancel, n, mtConfirmation, FALSE)<>mrOk then
    exit;

  g.Name := n;
  LBChannels.Items[LBChannels.ItemIndex] := n;
  LBChannels.Invalidate;
  Project.SetModified;
end;

procedure TFormDMXGroup.MIRGBDeleteClick(Sender: TObject);
var i: integer;
begin
  i := LBRGB.ItemIndex;
  if i = -1 then exit;

  TRGBGroup(LBRGB.Items.Objects[i]).Free;
  LBRGB.Items.Delete(i);
  FTargetViewProjector.Sel_None;
  FTargetViewProjector.Redraw;
  Project.SetModified;
end;

procedure TFormDMXGroup.MIRGBRenameClick(Sender: TObject);
var g: TRGBGroup;
  n: string;
begin
  g := GetSelectedRGBGroup;
  if g = NIL then
    exit;
  n := g.Name;
  if UserInputNoSpecialChar(SNewName, SOk, SCancel, n, mtConfirmation, FALSE)<>mrOk then
    exit;

  g.Name := n;
  LBRGB.Items[LBRGB.ItemIndex] := n;
  LBRGB.Invalidate;
  Project.SetModified;
end;

procedure TFormDMXGroup.PopChannelsPopup(Sender: TObject);
begin
  MIChannelDelete.Enabled := LBChannels.ItemIndex<>-1;
  MIChannelRename.Enabled := LBChannels.ItemIndex<>-1;
end;

procedure TFormDMXGroup.PopRGBPopup(Sender: TObject);
begin
  MIRGBRename.Enabled := LBRGB.ItemIndex<>-1;
  MIRGBDelete.Enabled := LBRGB.ItemIndex<>-1;
end;

procedure TFormDMXGroup.SpeedButton3Click(Sender: TObject);
begin
  FTargetViewProjector.Sel_None;
  FTargetViewProjector.Redraw;
end;

procedure TFormDMXGroup.ViewPage(aPage: TPage);
begin
  FNoteBookManager.ActivePage(aPage);
  if not Visible then
    Show;
end;

procedure TFormDMXGroup.SetGUIMode(AValue: TGUIMode);
begin
  case AValue of
    guiMainDMX: FTargetViewProjector := FormMain.FrameViewProjector1;
    guiEditSequence: FTargetViewProjector := FormAddDMXAction.FrameViewProjector1;
  end;
end;

function TFormDMXGroup.GetSelectedChannelGroup: TChannelGroup;
begin
  if LBChannels.ItemIndex = -1 then
    Result := NIL
  else
    Result := TChannelGroup(LBChannels.Items.Objects[LBChannels.ItemIndex]);
end;

function TFormDMXGroup.GetSelectedRGBGroup: TRGBGroup;
begin
  if LBRGB.ItemIndex = -1 then
    Result := NIL
  else
    Result := TRGBGroup(LBRGB.Items.Objects[LBRGB.ItemIndex]);
end;

procedure TFormDMXGroup.AddChannelGroup(Chans: ArrayOfDmxChannels);
var i: integer;
    o: TChannelGroup;
    n: string;
begin
  if Length(Chans) = 0 then exit;
  n := SGroupOfChannels+(LBChannels.Count+1).ToString;
  if UserInputNoSpecialChar(SNameForTheNewGroup, SOk, SCancel, n, mtConfirmation, FALSE) <> mrOk then
    exit;

  o := TChannelGroup.Create;
  o.Name := n;
  SetLength(o.A, Length(Chans));
  for i:=0 to High(o.A) do
    o.A[i].InitFromChannel(Chans[i]);

  i := LBChannels.Items.AddObject(n, o);
  FAddingNewGroup := True;
  LBChannels.ItemIndex := i;
  FAddingNewGroup := False;
  Project.SetModified;

  ViewPage(PageChannels);
end;

procedure TFormDMXGroup.AddRGBGroup(Fixs: ArrayOfDMXFixtures);
var i: integer;
    o: TRGBGroup;
    n: string;
begin
  if Length(Fixs) = 0 then exit;
  // check if all fixtures have RGB
  for i:=0 to High(Fixs) do
    if not Fixs[i].HasRGBChannel then exit;

  n := SRGBGroup+(LBRGB.Count+1).ToString;
  if UserInputNoSpecialChar(SNameForTheNewRGBGroup, SOk, SCancel, n, mtConfirmation, FALSE) <> mrOk then
    exit;

  o := TRGBGroup.Create;
  o.Name := n;
  SetLength(o.A, Length(Fixs));
  for i:=0 to High(Fixs) do
    o.A[i].InitFromFixture(Fixs[i]);

  i := LBRGB.Items.AddObject(n, o);
  FAddingNewGroup := True;
  LBRGB.ItemIndex := i;
  FAddingNewGroup := False;
  Project.SetModified;

  ViewPage(PageRGB);
end;

procedure TFormDMXGroup.Clear;
begin
  LBChannels.LockSelectionChange;
  while LBChannels.Count>0 do
  begin
    TChannelGroup(LBChannels.Items.Objects[0]).Free;
    LBChannels.Items.Delete(0);
  end;
  LBChannels.UnlockSelectionChange;

  LBRGB.LockSelectionChange;
  while LBRGB.Count > 0 do
  begin
    TRGBGroup(LBRGB.Items.Objects[0]).Free;
    LBRGB.Items.Delete(0);
  end;
  LBRGB.UnlockSelectionChange;
end;

const CHANNELSGROUP_HEADER='[CHANNELS GROUP]';
      RGBGROUP_HEADER='[RGB GROUP]';
procedure TFormDMXGroup.SaveTo(const aFilename: string);
var i, j: integer;
    s: string;
    cg: TChannelGroup;
    rgbg: TRGBGroup;
    t: TStringList;
begin
  t := TStringList.Create;
  try
    if LBChannels.Count>0 then
    begin
      t.Add(CHANNELSGROUP_HEADER);
      t.Add(LBChannels.Count.ToString);
      for i:=0 to LBChannels.Count-1 do
      begin
        cg := TChannelGroup(LBChannels.Items.Objects[i]);
        s := cg.Name;
        for j:=0 to High(cg.A) do
          s.ConcatCmd(cg.A[j].SaveToString);
        t.Add(s);
      end;
    end;

    if LBRGB.Count>0 then
    begin
      t.Add(RGBGROUP_HEADER);
      t.Add(LBRGB.Count.ToString);
      for i:=0 to LBRGB.Count-1 do
      begin
        rgbg := TRGBGroup(LBRGB.Items.Objects[i]);
        s := rgbg.Name;
        for j:=0 to High(rgbg.A) do
          s.ConcatCmd(rgbg.A[j].SaveToString);
        t.Add(s);
      end;
    end;

    try
      t.SaveToFile(aFilename);
    except
      Log.Error('TFormDMXGroup.SaveTo - Fail to save TStringList to file "'+aFilename+'"');
    end;
  finally
    t.Free;
  end;
end;

procedure TFormDMXGroup.LoadFrom(const aFilename: string);
var k, i, j: integer;
    cg: TChannelGroup;
    rgbg: TRGBGroup;
    A: TCmdArray;
    t: TStringList;
begin
  Log.Info('Loading groups', 2);
  Clear;

  t := TStringList.Create;
  try
    try
      t.LoadFromFile(aFilename);
    except
      Log.Error('TFormDMXGroup.LoadFrom - Fail to load TStringList with file "'+aFilename+'"', 3);
    end;

    k := t.IndexOf(CHANNELSGROUP_HEADER);
    if k <> -1 then
    begin
      LBChannels.LockSelectionChange;
      inc(k);
      for i:=0 to t.Strings[k].ToInteger-1 do // count
      begin
        inc(k);
        A := t.Strings[k].SplitToCmdArray;
        cg := TChannelGroup.Create;
        cg.Name := A[0];
        SetLength(cg.A, Length(A)-1);
        for j:=1 to High(A) do
          cg.A[j-1].LoadFromString(A[j]);
        LBChannels.Items.AddObject(cg.Name, cg);
      end;
      LBChannels.UnlockSelectionChange;
    end;

    k := t.IndexOf(RGBGROUP_HEADER);
    if k <> -1 then
    begin
      LBRGB.LockSelectionChange;
      inc(k);
      for i:=0 to t.Strings[k].ToInteger-1 do // count
      begin
        inc(k);
        A := t.Strings[k].SplitToCmdArray;
        rgbg := TRGBGroup.Create;
        rgbg.Name:=A[0];
        SetLength(rgbg.A, Length(A)-1);
        for j:=1 to High(A) do
          rgbg.A[j-1].LoadFromString(A[j]);
        LBRGB.Items.AddObject(rgbg.Name, rgbg);
      end;
      LBRGB.UnlockSelectionChange;
    end;
  finally
    t.Free;
  end;
end;

procedure TFormDMXGroup.UpdateEditMode;
begin
  if Project.Options.EditMode then
  begin
    LBChannels.PopupMenu := PopChannels;
    LBRGB.PopupMenu := PopRGB;
  end
  else begin
    LBChannels.PopupMenu := NIL;
    LBRGB.PopupMenu := NIL;
  end;
end;

end.

