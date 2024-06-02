unit frame_editmode;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Buttons,
  LCLType,
  u_list_dmxuniverse, frame_viewmodeitem;

type

  { TFrameEditMode }

  TFrameEditMode = class(TFrame)
    BAddSwitchingChannel: TSpeedButton;
    BAddRepetitiveChannel: TSpeedButton;
    BUp: TSpeedButton;
    BDown: TSpeedButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Panel1: TPanel;
    BDeleteMode: TSpeedButton;
    BAddChannel: TSpeedButton;
    PanelTools: TPanel;
    BEditChannel: TSpeedButton;
    BDeleteChannel: TSpeedButton;
    Shape1: TShape;
    procedure BAddRepetitiveChannelClick(Sender: TObject);
    procedure BAddSwitchingChannelClick(Sender: TObject);
    procedure BDeleteModeClick(Sender: TObject);
    procedure BAddChannelClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure FrameMouseLeave(Sender: TObject);
    procedure Panel1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
    procedure ProcessAddRepetitiveChannelEvent(const aChan: TFixLibAvailableChannel);
  private
    FLabelIndexTargetedByPanelTools: integer;
    procedure DoEditChannelFrame(aIndex: integer);
    procedure DoDeleteChannelFrame(aIndex: integer);
    procedure ChanLabelMouseEnter(Sender: TObject);
    procedure ChanLabelMouseLeave(Sender: TObject);
    procedure SetPanelToolPosition(aFrameIndex: integer); // can be -1 to hide panel
    procedure ExchangeChannel(i1, i2: integer);
  private
    FErrorMessage: string;
    FExistingChannels: PFixLibAvailableChannels;
    FModeIndex: integer;
    FOnHeightChange: TNotifyEvent;
    FChanFrames: array of TFrameViewModeItem;

    function GetChannelsUsed: TStringArray;
    function GetVirtualChannelUsed: TStringArray;
    function GetChannelAndVirtualChannelUsed: TStringArray;
    function HaveErrorOnNameOrShortName: boolean;
    function GetHaveError: boolean;
    function GetModeName: string;
    function GetShortModeName: string;
    procedure SetModeIndex(AValue: integer);
    procedure SetModeName(AValue: string);
    procedure SetModified(AValue: boolean);
  public
    procedure DoAddChannelFrame(const aName: string);
    procedure ReplaceChannelName(const aOldName, aNewName: string);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure EraseBackground({%H-}DC: HDC); override;

    procedure InitFrom(const aMode: TFixLibMode);

    property ModeIndex: integer read FModeIndex write SetModeIndex;
    property ExistingChannels: PFixLibAvailableChannels read FExistingChannels write FExistingChannels;

    property OnHeightChange: TNotifyEvent read FOnHeightChange write FOnHeightChange;

    property HaveError: boolean read GetHaveError;
    property ErrorMessage: string read FErrorMessage;
    property ModeName: string read GetModeName write SetModeName;
    property ShortModeName: string read GetShortModeName;

    property ChannelsUsed: TStringArray read GetChannelsUsed;
    property VirtualChannelUsed: TStringArray read GetVirtualChannelUsed;
    property ChannelAndVirtualChannelUsed: TStringArray read GetChannelAndVirtualChannelUsed;

    property Modified: boolean write SetModified;
  end;

implementation
uses Math, form_selectexistingchannel, form_definenewchannel, LCLIntf,
  u_editfixturewizard, u_resource_string, u_helper, u_common,
  u_datamodule, form_defineswitchingchannel,
  form_selectexistingswitchingchannel, form_edit_repetitivechannel, Graphics;

{$R *.lfm}

function ParentForm(aFrameEditMode: TFrameEditMode): TFormFixtureWizard;
var o: TWinControl;
begin
  o := aFrameEditMode;
  repeat
    o := o.Parent;
  until o is TFormFixtureWizard;
  Result := TFormFixtureWizard(o);
end;

{ TFrameEditMode }

procedure TFrameEditMode.BAddSwitchingChannelClick(Sender: TObject);
var FormExisting: TFormSelectExistingSwitchingChannel;
  FormNew: TFormEditSwitchingChannel;
  chanName, A: TStringArray;
  i: SizeInt;
begin
  if HaveErrorOnNameOrShortName then exit;

  chanName := NIL;

  // first ask the user to choose existing virtual channel(s) (if any)
  if (Length(FVirtualChannelInMode) <> 0) and (Length(FVirtualChannelInMode) > Length(VirtualChannelUsed)) then begin
    FormExisting := TFormSelectExistingSwitchingChannel.Create(Self.Parent);
    try
      FormExisting.ModeName := Edit1.Text;
      FormExisting.FillWith(VirtualChannelUsed);
      case FormExisting.ShowModal of
        mrOK: chanName := Copy(FormExisting.UseExistingNames, 0, Length(FormExisting.UseExistingNames));
        mrIgnore:;
        mrCancel: exit;
      end;
    finally
      FormExisting.Free;
    end;
  end;

  if chanName = NIL then begin
    FormNew := TFormEditSwitchingChannel.Create(NIL);
    FormNew.ModeName := Edit1.Text;
    FormNew.FillWith(FExistingChannels);
    FormNew.TargetModeFrame := Self;
    try
      if FormNew.ShowModal = mrOk then begin
        // add the new created virtual channel
        i := Length(FVirtualChannelInMode);
        SetLength(FVirtualChannelInMode, i+1);
        FVirtualChannelInMode[i].InitDefault;
        FVirtualChannelInMode[i].VirtualName := FormNew.VirtualName;
        A := FormNew.SubChannelNames;
        FVirtualChannelInMode[i].SubChannelIDs := Copy(A, 0, Length(A));

        SetLength(chanName, 1);
        chanName[0] := FormNew.PackedVirtualNameAndSubChannelNames;
      end;
    finally
      FormNew.Free;
    end;
  end;

  if chanName = NIL then exit;
  for i:=0 to High(chanName) do
    DoAddChannelFrame(chanName[i]);
  FOnHeightChange(Self);

  Modified := True;
end;

procedure TFrameEditMode.BAddRepetitiveChannelClick(Sender: TObject);
var F: TFormEditRepetitiveChannel;
begin
  if HaveErrorOnNameOrShortName then exit;

  F := TFormEditRepetitiveChannel.Create(Nil);
  F.OnAddChannel := @ProcessAddRepetitiveChannelEvent;
  F.ExistingChannels := ExistingChannels;
  try
    if F.ShowModal = mrOk then begin
      FOnHeightChange(Self);
      Visible := True;
      Modified := True;
    end;
  finally
    F.Free;
  end;
end;

procedure TFrameEditMode.BAddChannelClick(Sender: TObject);
var i: integer;
  FormExisting: TFormSelectExistingChannel;
  FormNew: TFormDefineNewChannel;
  chanName: TStringArray;
begin
  if HaveErrorOnNameOrShortName then exit;

  chanName := NIL;

  // first ask the user to choose existing channel(s) (if any)
  if (Length(FExistingChannels^) <> 0) and (Length(FExistingChannels^) > Length(ChannelsUsed)) then begin
    FormExisting := TFormSelectExistingChannel.Create(Self.Parent);
    try
      FormExisting.ModeName := Edit1.Text;
      FormExisting.FillWith(FExistingChannels, ChannelsUsed);
      case FormExisting.ShowModal of
        mrOK: chanName := Copy(FormExisting.UseExistingNames, 0, Length(FormExisting.UseExistingNames));
        mrIgnore:;
        mrCancel: exit;
      end;
    finally
      FormExisting.Free;
    end;
  end;

  if chanName = NIL then begin
    FormNew := TFormDefineNewChannel.Create(NIL);
    FormNew.ExistingChannel := FExistingChannels;
    try
      if FormNew.ShowModal = mrOk then begin
        // add the new created channel
        i := Length(FExistingChannels^);
        SetLength(FExistingChannels^, i+1);
        FExistingChannels^[i].InitDefault;
        FExistingChannels^[i].LoadFromString(FormNew.GetData);
        SetLength(chanName, 1);
        chanName[0] := FExistingChannels^[i].NameID;
      end;
    finally
      FormNew.Free;
    end;
  end;

  if chanName = NIL then exit;
  for i:=0 to High(chanName) do
    DoAddChannelFrame(chanName[i]);
  FOnHeightChange(Self);

  Modified := True;
end;

procedure TFrameEditMode.Edit1Change(Sender: TObject);
begin
  HaveErrorOnNameOrShortName;
  Modified := True;
end;

procedure TFrameEditMode.FrameMouseLeave(Sender: TObject);
begin
  PanelTools.Visible := False;
end;

procedure TFrameEditMode.DoEditChannelFrame(aIndex: integer);
var FormNew: TFormDefineNewChannel;
  FormEditSwitching: TFormEditSwitchingChannel;
  i, j: integer;
  oldName, packedName: string;
begin
  oldName := FChanFrames[aIndex].ChanName;

  if FChanFrames[aIndex].IsSwitchingChannel then begin
    // edit switching channel
    i := FVirtualChannelInMode.IndexOfVirtualName(oldName);
    if i = -1 then exit;

    FormEditSwitching := TFormEditSwitchingChannel.Create(NIL);
    try
      FormEditSwitching.FillWith(FExistingChannels);
      FormEditSwitching.TargetModeFrame := Self;
      FormEditSwitching.EditExistingChannel(@FVirtualChannelInMode[i]);

      if FormEditSwitching.ShowModal = mrOk then begin
        // replace data in the channel
        packedName := FormEditSwitching.PackedVirtualNameAndSubChannelNames;
        FVirtualChannelInMode[i].InitFromPackedString(packedName);
        // replace new virtual name in all switchers
        FExistingChannels^.ReplaceNameInSwitchDescriptors(oldName, FormEditSwitching.VirtualName);
        // replace new name in all frame (Modes)
        ParentForm(Self).ReplaceChannelNameInAllFrames(oldName, packedName);
        Modified := True;
      end;
    finally
      FormEditSwitching.Free;
    end;

  end else begin
    // edit normal channel
    i := FExistingChannels^.NameToIndex(oldName);
    if i = -1 then exit;

    FormNew := TFormDefineNewChannel.Create(NIL);
    FormNew.ExistingChannel := FExistingChannels;

    FormNew.EditExistingChannel(@FExistingChannels^[i]);
    try
      if FormNew.ShowModal = mrOk then begin
        // replace data in the channel
        FExistingChannels^[i].LoadFromString(FormNew.GetData);
        // replace new name in all frame (Modes)
        ParentForm(Self).ReplaceChannelNameInAllFrames(oldName, FExistingChannels^[i].NameID);
        Modified := True;
      end;
    finally
      FormNew.Free;
    end;
  end;
end;

procedure TFrameEditMode.Panel1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var i, index: integer;
begin
  index := -1;
  for i:=0 to High(FChanFrames) do
    if InRange(Y, FChanFrames[i].Top, FChanFrames[i].Top+FChanFrames[i].Height) then begin
      index := i;
      break;
    end;

  SetPanelToolPosition(index);
end;

procedure TFrameEditMode.ProcessAddRepetitiveChannelEvent(const aChan: TFixLibAvailableChannel);
var i: integer;
begin
  i := Length(FExistingChannels^);
  SetLength(FExistingChannels^, i+1);
  aChan.CopyTo(FExistingChannels^[i]);
  Visible := False;
  DoAddChannelFrame(FExistingChannels^[i].NameID);
end;

procedure TFrameEditMode.DoAddChannelFrame(const aName: string);
var i, xx, yy, w: integer;
  par: TWinControl;
begin
  par := PanelTools.Parent;
  PanelTools.Parent := NIL;

  i := Length(FChanFrames);
  SetLength(FChanFrames, i+1);
  FChanFrames[i] := TFrameViewModeItem.Create(Self);
  FChanFrames[i].Parent := Panel1;

  xx := Label4.Left;
  if i = 0 then yy := Label4.Top + Label4.Height + ScaleDesignToForm(5)
    else yy := FChanFrames[i-1].Top + FChanFrames[i-1].Height{ + ScaleDesignToForm(5)};
  //w := Panel1.ClientWidth - PanelTools.Width - Label4.Left - ScaleDesignToForm(5);
  w := Panel1.ClientWidth - xx*2;
  FChanFrames[i].SetBounds(xx, yy, w, FChanFrames[i].Height);

  FChanFrames[i].ExistingChannels := FExistingChannels;
  FChanFrames[i].Init(i, aName);
  FChanFrames[i].OnMouseEnter := @ChanLabelMouseEnter;
  FChanFrames[i].OnMouseLeave := @ChanLabelMouseLeave;

  ClientHeight := FChanFrames[i].Top + FChanFrames[i].Height +  // ClientHeight + FChanFrames[i].Height;
                  ScaleDesignToForm(10) + BAddChannel.Height + ScaleDesignToForm(10);

  PanelTools.Parent := par;
end;

procedure TFrameEditMode.DoDeleteChannelFrame(aIndex: integer);
var i, h: integer;
begin
  if (aIndex < 0) or (aIndex > High(FChanFrames)) then exit;

  h := FChanFrames[aIndex].Height;
  FChanFrames[aIndex].Free;
  Delete(FChanFrames, aIndex, 1);
  // shift the Top position and the prefix number of the next labels
  for i:=High(FChanFrames) downto aIndex do begin
    FChanFrames[i].Top := FChanFrames[i].Top - h;
    FChanFrames[i].IndexInMode := FChanFrames[i].IndexInMode - 1;
  end;

  // adjust the height of the frame
  ClientHeight := ClientHeight - h;

  // ask the parent scrollbox to adjust its layout
  FOnHeightChange(Self);

  Modified := True;
end;

procedure TFrameEditMode.ChanLabelMouseEnter(Sender: TObject);
begin
  SetPanelToolPosition(TFrameViewModeItem(Sender).IndexInMode);
end;

procedure TFrameEditMode.ChanLabelMouseLeave(Sender: TObject);
var p: TPoint;
begin
  p := PanelTools.ScreenToClient(Mouse.CursorPos);
  if PanelTools.ClientRect.Contains(p) then exit;
  SetPanelToolPosition(-1);
end;

procedure TFrameEditMode.SetPanelToolPosition(aFrameIndex: integer);
begin
  PanelTools.Visible := aFrameIndex <> -1;
  if aFrameIndex <> -1 then begin
    PanelTools.Tag := aFrameIndex;
    PanelTools.Top := FChanFrames[aFrameIndex].Top; // + FChanFrames[aFrameIndex].Height div 2 - PanelTools.Height div 2;
    PanelTools.Left := FChanFrames[aFrameIndex].Left +
                       FChanFrames[aFrameIndex].Width + ScaleDesignToForm(5);
  end;

  FLabelIndexTargetedByPanelTools := aFrameIndex;
end;

procedure TFrameEditMode.SetModeIndex(AValue: integer);
begin
  FModeIndex := AValue;
  Label5.Caption := (AValue+1).ToString;
  BDeleteMode.Visible := AValue <> 0;
end;

procedure TFrameEditMode.SetModeName(AValue: string);
begin
  Edit1.Text := AValue;
end;

procedure TFrameEditMode.SetModified(AValue: boolean);
begin
  if AValue then ParentForm(Self).Modified := True;
end;

procedure TFrameEditMode.ReplaceChannelName(const aOldName, aNewName: string);
var i: integer;
begin
  for i:=0 to High(FChanFrames) do
    FChanFrames[i].ReplaceChannelName(aOldName, aNewName);
end;

constructor TFrameEditMode.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  // manual translation
  Label6.Caption := SNameAlreadyUsed;
  Label7.Caption := SNameAlreadyUsed;
  Label2.Caption := SName;
  BAddRepetitiveChannel.Caption := sRepeatChannels;

  BAddSwitchingChannel.ImageIndex := Ord(High(TChannelType))+1;
  BAddSwitchingChannel.ImageWidth := DataModule1.ImageList1.Width;
end;

procedure TFrameEditMode.EraseBackground(DC: HDC);
begin
  // do nothing
end;

procedure TFrameEditMode.InitFrom(const aMode: TFixLibMode);
var i: integer;
begin
  ModeName := aMode.Name;
  Edit2.Text := aMode.ShortName;
  for i:=0 to high(aMode.ChannelsIDToUse) do
    DoAddChannelFrame(aMode.ChannelsIDToUse[i]);
end;

function TFrameEditMode.GetModeName: string;
begin
  Result := Trim(Edit1.Text);
end;

function TFrameEditMode.GetChannelsUsed: TStringArray;
var i, c: integer;
begin
  Result := NIL;
  if Length(FChanFrames) = 0 then exit;

  // retrieve the count
  c := 0;
  for i:=0 to High(FChanFrames) do
    if not FChanFrames[i].IsSwitchingChannel then inc(c);
  if c = 0 then exit;

  SetLength(Result, c);
  c := 0;
  for i:=0 to High(FChanFrames) do
    if not FChanFrames[i].IsSwitchingChannel then begin
      Result[c] := FChanFrames[i].ChanName;
      inc(c);
    end;
end;

function TFrameEditMode.GetVirtualChannelUsed: TStringArray;
var i, c: integer;
begin
  Result := NIL;
  if Length(FChanFrames) = 0 then exit;

  // retrieve the count
  c := 0;
  for i:=0 to High(FChanFrames) do
    if FChanFrames[i].IsSwitchingChannel then inc(c);
  if c = 0 then exit;

  SetLength(Result, c);
  c := 0;
  for i:=0 to High(FChanFrames) do
    if FChanFrames[i].IsSwitchingChannel then begin
      Result[c] := FChanFrames[i].ChanName;
      inc(c);
    end;
end;

function TFrameEditMode.GetChannelAndVirtualChannelUsed: TStringArray;
var i: integer;
begin
  Result := NIL;
  if Length(FChanFrames) = 0 then exit;

  SetLength(Result, Length(FChanFrames));
  for i:=0 to High(FChanFrames) do
    Result[i] := FChanFrames[i].PackedName;
end;

function TFrameEditMode.HaveErrorOnNameOrShortName: boolean;
var s: string;
begin
  // check mode name
  s := Trim(Edit1.Text);
  Shape1.Visible := s = '';
  if Shape1.Visible then exit(True);
  Label6.Visible := ParentForm(Self).CheckIfModeNameIsUsed(Self, s);
  if Label6.Visible then exit(True);

  // check short mode name
  s := Trim(Edit2.Text);
  if s <> '' then begin
    Label7.Visible := ParentForm(Self).CheckIfModeShortNameIsUsed(Self, s);
    if Label7.Visible then exit(True);
  end;

  Result := False;
end;

function TFrameEditMode.GetHaveError: boolean;
var A, B: TStringArray;
  i, j, k, m: integer;
  virtualNameIsUsed: array of boolean;
  p, pp: PFixLibAvailableChannel;
begin
  FErrorMessage := '';

  if HaveErrorOnNameOrShortName then begin
    FErrorMessage := SMode+' '+(Modeindex+1).ToString+': '+SErrorOnName;
    exit(True);
  end;

  // used channels list
  A := GetChannelsUsed;
  if length(A) = 0 then begin
    FErrorMessage := SMode+' "'+ModeName+'": '+SThisModeIsEmpty;
    exit(True);
  end;

  // check the integrity of each used channels
  for i:=0 to High(A) do begin
    p := FExistingChannels^.GetChannelsByName(A[i]);
    if p = NIL then exit(True);
    if not p^.IsAlias then begin
      if p^.HaveRangesError then begin
        FErrorMessage := SMode+' "'+ModeName+'": "'+SChannel+'" '+p^.NameID+' '+SHaveRangeError;
        exit(True);
      end;
    end else begin
      pp := FExistingChannels^.GetChannelsByName(p^.AliasOfNameID);
      if pp = NIL then begin
        FErrorMessage := SMode+' "'+ModeName+'": "'+SChannel+'" '+p^.NameID+
                         ' '+SIsAnAliasOf+' "'+p^.AliasOfNameID+'" '+ SThatDoesntExists;
        exit(True);
      end;
    end;
  end;

  // checks if the virtual channels defined in this mode are used by at least one another channel
  B := GetVirtualChannelUsed;
  if Length(B) > 0 then begin
    virtualNameIsUsed := NIL;
    SetLength(virtualNameIsUsed, Length(B));
    for i:=0 to High(A) do begin
      p := FExistingChannels^.GetChannelsByName(A[i]);
      if p = NIL then exit(True);
      for j:=0 to High(p^.Ranges) do
        for k:=0 to High(p^.Ranges[j].SwitchDescriptors) do
          for m:=0 to High(B) do begin
            if p^.Ranges[j].SwitchDescriptors[k].SwitchVirtualChannel = B[m] then
              virtualNameIsUsed[m] := True;
        end;
    end;
    for i:=0 to High(virtualNameIsUsed) do
      if not virtualNameIsUsed[i] then begin
        FErrorMessage := SMode+' "'+ModeName+'": '+
                        SVirtualChannel+' "'+FVirtualChannelInMode[i].VirtualName+'" '+SDefinedButNotUsed;
        exit(True);
      end;
  end;

  Result := False;
end;

function TFrameEditMode.GetShortModeName: string;
begin
  Result := Trim(Edit2.Text);
end;

procedure TFrameEditMode.BDeleteModeClick(Sender: TObject);
begin
  if Sender = BDeleteMode then begin
    ParentForm(Self).KillModeFrame(Self);
    exit;
  end;

  if Sender = BEditChannel then begin
    DoEditChannelFrame(PanelTools.Tag);
    PanelTools.Visible := False;
  end;

  if Sender = BDeleteChannel then begin
    DoDeleteChannelFrame(PanelTools.Tag);
    PanelTools.Visible := False;
  end;

  if Sender = BUp then begin
    if FLabelIndexTargetedByPanelTools = 0 then exit;
    ExchangeChannel(FLabelIndexTargetedByPanelTools-1, FLabelIndexTargetedByPanelTools);
    PanelTools.Visible := False;
    Modified := True;
  end;

  if Sender = BDown then begin
    if FLabelIndexTargetedByPanelTools = High(FChanFrames) then exit;
    ExchangeChannel(FLabelIndexTargetedByPanelTools, FLabelIndexTargetedByPanelTools+1);
    PanelTools.Visible := False;
    Modified := True;
  end;
end;

procedure TFrameEditMode.ExchangeChannel(i1, i2: integer);
var o: TFrameViewModeItem;
  y: integer;
  c: TColor;
  i: integer;
begin
  // exchange color background
  c := FChanFrames[i1].Color;
  FChanFrames[i1].Color := FChanFrames[i2].Color;
  FChanFrames[i2].Color := c;

  // exchange y coordinates
  y := FChanFrames[i2].Top;
  FChanFrames[i2].Top := FChanFrames[i1].Top;
  FChanFrames[i1].Top := FChanFrames[i2].Top + FChanFrames[i2].Height;

  // exchange IndexInMode
  i := FChanFrames[i1].IndexInMode;
  FChanFrames[i1].IndexInMode := FChanFrames[i2].IndexInMode;
  FChanFrames[i2].IndexInMode := i;

  // exchange position in list
  o := FChanFrames[i1];
  FChanFrames[i1] := FChanFrames[i2];
  FChanFrames[i2] := o;
end;

end.

