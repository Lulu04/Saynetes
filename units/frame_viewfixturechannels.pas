unit frame_viewfixturechannels;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, LCLType, Menus, LCLTranslator,
  u_list_dmxuniverse, u_common;

type

  TUserRenameChannelCallback = procedure(Sender: TObject; aChanIndex: integer; const aNewName: string) of object;
  TUserWantChannelAliasCallback = procedure(Sender: TObject; aChanIndex: integer; out pAlias: PFixLibAvailableChannel) of object;

  { TFrameViewDMXFixtureChannels }

  TFrameViewDMXFixtureChannels = class(TFrame)
    MISetAsAliasOf: TMenuItem;
    MIRename: TMenuItem;
    PopupMenu1: TPopupMenu;
    TV: TTreeView;
    procedure MIRenameClick(Sender: TObject);
    procedure MISetAsAliasOfClick(Sender: TObject);
    procedure TVMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TVSelectionChanged(Sender: TObject);
  private
    FEditionEnabled, FTreeIsCollapsed: boolean;
    FOnUserWantChannelAlias: TUserWantChannelAliasCallback;
    FOnUserRenameChannel: TUserRenameChannelCallback;
    FFixturetype: TFixturetype;
    FOnSelectionChange: TNotifyEvent;
    FReady: boolean;
    FChannelCount: integer;
    FSelectionEnabled: boolean;
    function GetSelected: TTreeNode;
    procedure SetEditionEnabled(AValue: boolean);
    procedure SetSelectionEnabled(AValue: boolean);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure EraseBackground({%H-}DC: HDC); override;

    procedure Clear;
    procedure ShowFixture(const aFixtureLocation: TFixtureLibraryLocation; aShowCollapsed: boolean);
    procedure AddChannel(p: PFixLibAvailableChannel);

    procedure MoveSelectedUp;
    procedure MoveSelectedDown;

    function SelectedIsChannelName: boolean;
    function SelectedIsRange: boolean;

    property Ready: boolean read FReady; // true after a successfull call to method ShowFixture
    property ChannelCount: integer read FChannelCount write FChannelCount;
    property FixtureType: TFixturetype read FFixturetype;

    property EditionEnabled: boolean read FEditionEnabled write SetEditionEnabled;
    property OnUserRenameChannel: TUserRenameChannelCallback read FOnUserRenameChannel write FOnUserRenameChannel;
    property OnUserWantChannelAlias: TUserWantChannelAliasCallback read FOnUserWantChannelAlias write FOnUserWantChannelAlias;
    property SelectionEnabled: boolean read FSelectionEnabled write SetSelectionEnabled;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
    property Selected: TTreeNode read GetSelected;
  end;

implementation

uses u_resource_string, u_dmxlib_inputrange, u_userdialogs, u_utils, u_helper,
  Dialogs;

{$R *.lfm}

{ TFrameViewDMXFixtureChannels }

procedure TFrameViewDMXFixtureChannels.TVSelectionChanged(Sender: TObject);
begin
  if not FSelectionEnabled
    then TV.Selected:=NIL
    else if FOnSelectionChange<>NIL
           then FOnSelectionChange(Self);
end;

procedure TFrameViewDMXFixtureChannels.TVMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var n: TTreeNode;
begin
  // collapse/develop the tree
  if (Button = mbLeft) and (TV.GetNodeWithExpandSignAt(X, Y) = NIL) then begin
    FTreeIsCollapsed := not FTreeIsCollapsed;

    if TV.Items.Count = 0 then exit;
    n := TV.Items.GetFirstNode;
    for n in TV.Items do
      if FTreeIsCollapsed then n.Collapse(True)
        else n.Expand(False);
  end;

  if EditionEnabled and (Button in [mbLeft,mbRight]) and (TV.GetNodeAt(X, Y) <> NIL) then begin
    PopupMenu1.PopUp;
  end;
end;

procedure TFrameViewDMXFixtureChannels.MIRenameClick(Sender: TObject);
var n: TTreeNode;
  na: String;
begin
  n := TV.Selected;
  if n = NIL then exit;
  if n.Level = 1 then n := n.Parent;
  na := n.Text;
  if UserInputNoSpecialChar(SNewName, SOk, SCancel, na, mtConfirmation, False) = mrOk then begin
    n.Text := na;
    if FOnUserRenameChannel <> NIL then FOnUserRenameChannel(Self, n.Index, na);
  end;
end;

procedure TFrameViewDMXFixtureChannels.MISetAsAliasOfClick(Sender: TObject);
var n: TTreeNode;
  pAlias: PFixLibAvailableChannel;
  i: integer;
begin
  n := TV.Selected;
  if n = NIL then exit;
  if n.Level = 1 then n := n.Parent;

  FOnUserWantChannelAlias(Self, n.Index, pAlias);
  if pAlias = NIL then exit;

  n.ImageIndex := Ord(pAlias^.ChanType); // image associated with channel type
  n.DeleteChildren;
  for i:=0 to High(pAlias^.Ranges) do
    TV.Items.AddChild(n, pAlias^.Ranges[i].ToReadableString);
end;

procedure TFrameViewDMXFixtureChannels.SetSelectionEnabled(AValue: boolean);
begin
  if FSelectionEnabled=AValue then Exit;
  FSelectionEnabled:=AValue;
end;

constructor TFrameViewDMXFixtureChannels.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  // manual translation
  MIRename.Caption := SRename;
  MISetAsAliasOf.Caption := SAliasOf_;
end;

procedure TFrameViewDMXFixtureChannels.SetEditionEnabled(AValue: boolean);
begin
  if FEditionEnabled=AValue then Exit;
  FEditionEnabled:=AValue;
{  if FEditionEnabled then begin
    TV.PopupMenu:=PopupMenu1;
  end else begin
    TV.PopupMenu:=NIL;
  end; }
end;

function TFrameViewDMXFixtureChannels.GetSelected: TTreeNode;
begin
  Result:=TV.Selected;
end;

procedure TFrameViewDMXFixtureChannels.EraseBackground(DC: HDC);
begin
// do nothing
end;

procedure TFrameViewDMXFixtureChannels.Clear;
begin
  TV.Items.Clear;
end;
procedure TFrameViewDMXFixtureChannels.ShowFixture(const aFixtureLocation: TFixtureLibraryLocation; aShowCollapsed: boolean);
var i, j, k: integer;
  n, n1: TTreeNode;
  lf: TLibraryFixture;
  A: TStringArray;
  virtualName: string;
  subChannels: TStringArray;
  p: PFixLibAvailableChannel;
begin
  FReady := FALSE;
  TV.Items.Clear;
  if (aFixtureLocation.SubFolder = '') or
     (aFixtureLocation.FileName = '') or
     (aFixtureLocation.Mode = '') then exit;
  if ExtractFileExt(aFixtureLocation.Filename) <> DMX_LIBRARY_FILE_EXTENSION then exit;

  try
   if not lf.LoadFromFile(aFixtureLocation.AbsolutPath) then exit;
  except
    exit;
  end;

  FFixturetype := lf.General.FixtureType;
  A := lf.GetChannelNamesForMode(aFixtureLocation.Mode);

  for i:=0 to High(A) do begin
    if TrySplitVirtual(A[i], virtualName, subChannels) then begin
      n := TV.Items.Add(TV.Items.GetFirstNode, virtualName);
      n.ImageIndex := Ord(High(TChannelType))+1; // Switch image
      for j:=0 to High(subChannels) do begin
        p := lf.AvailableChannels.GetChannelsByName(subChannels[j]);
        if p = NIL then begin
          TV.Items.Clear;
          exit;
        end;
        n1 := TV.Items.AddChild(n, subChannels[j]);
        n1.ImageIndex := Ord(p^.ChanType); // image associated with channel type
        for k:=0 to High(p^.Ranges) do
          TV.Items.AddChild(n1, p^.Ranges[k].ToReadableString);
      end;
    end else begin
      p := lf.AvailableChannels.GetChannelsByName(A[i]);
      if p = NIL then begin
        TV.Items.Clear;
        exit;
      end;
      n := TV.Items.Add(TV.Items.GetFirstNode, p^.NameID);
      n.ImageIndex := Ord(p^.ChanType); // image associated with channel type
      for j:=0 to High(p^.Ranges) do
        TV.Items.AddChild(n, p^.Ranges[j].ToReadableString);
    end;


    // channel name

  end;
  TV.FullExpand;
  FReady := TRUE;

  if TV.Items.Count = 0 then exit;

   n := TV.Items.GetFirstNode;
   for n in TV.Items do
     if aShowCollapsed then n.Collapse(True)
       else n.Expand(False);

   FTreeIsCollapsed := aShowCollapsed;
end;

procedure TFrameViewDMXFixtureChannels.AddChannel(p: PFixLibAvailableChannel);
var i: integer;
  n: TTreeNode;
begin
  n := TV.Items.Add(TV.Items.GetFirstNode, p^.NameID);
  n.ImageIndex := Ord(p^.ChanType); // image associated with channel type
  for i:=0 to High(p^.Ranges) do
    TV.Items.AddChild(n, p^.Ranges[i].ToReadableString);
end;

procedure TFrameViewDMXFixtureChannels.MoveSelectedUp;
var n: TTreeNode;
begin
  n:=TV.Selected;
  if n=NIL then exit;
  if n.Level=0 then exit;
  if n.Index=0 then exit;
  n.MoveTo(n.GetPrev, naInsert);
end;

procedure TFrameViewDMXFixtureChannels.MoveSelectedDown;
var n: TTreeNode;
begin
  n:=TV.Selected;
  if n=NIL then exit;
  if n.Level=0 then exit;
  if n.Index=n.Parent.Count-1 then exit;
  n.MoveTo(n.GetNext, naInsertBehind);
end;

function TFrameViewDMXFixtureChannels.SelectedIsChannelName: boolean;
begin
  Result:=FALSE;
  if Selected<>NIL
    then Result := Selected.Level=0;
end;

function TFrameViewDMXFixtureChannels.SelectedIsRange: boolean;
begin
  Result:=FALSE;
  if Selected<>NIL
    then Result := Selected.Level=1;
end;


end.

