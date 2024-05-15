unit frame_editmode;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Buttons,
  u_list_dmxuniverse, frame_viewmodeitem;

type

  { TFrameEditMode }

  TFrameEditMode = class(TFrame)
    BAddSwitchingChannel: TSpeedButton;
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
    procedure BAddSwitchingChannelClick(Sender: TObject);
    procedure BDeleteModeClick(Sender: TObject);
    procedure BAddChannelClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure FrameMouseLeave(Sender: TObject);
    procedure Panel1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
    FLabelIndexTargetedByPanelTools: integer;
    procedure DoEditChannelFrame(aIndex: integer);
    procedure DoAddChannelFrame(const aName: string);
    procedure DoDeleteChannelFrame(aIndex: integer);
    procedure ChanLabelMouseEnter(Sender: TObject);
    procedure ChanLabelMouseLeave(Sender: TObject);
    procedure SetPanelToolPosition(aFrameIndex: integer); // can be -1 to hide panel
    procedure ExchangeChannel(i1, i2: integer);
  private
    FExistingChannels: PFixLibAvailableChannels;
    FModeIndex: integer;
    FOnHeightChange: TNotifyEvent;
    FChanFrames: array of TFrameViewModeItem;

    function GetChannelsUsed: TStringArray;
    function HaveErrorOnNameOrShortName: boolean;
    function GetHaveError: boolean;
    function GetModeName: string;
    function GetShortModeName: string;
    procedure SetModeIndex(AValue: integer);
    procedure SetModeName(AValue: string);
    procedure SetModified(AValue: boolean);
  public
    procedure ReplaceChannelName(const aOldName, aNewName: string);
  public
    constructor Create(TheOwner: TComponent); override;

    procedure InitFrom(const aMode: TFixLibMode);

    property ModeIndex: integer read FModeIndex write SetModeIndex;
    property ExistingChannels: PFixLibAvailableChannels write FExistingChannels;

    property OnHeightChange: TNotifyEvent read FOnHeightChange write FOnHeightChange;

    property HaveError: boolean read GetHaveError;
    property ModeName: string read GetModeName write SetModeName;
    property ShortModeName: string read GetShortModeName;
    property ChannelsUsed: TStringArray read GetChannelsUsed;

    property Modified: boolean write SetModified;
  end;

implementation
uses Math, form_selectexistingchannel, form_definenewchannel, LCLIntf,
  u_editfixturewizard, u_utils, u_resource_string, u_helper, u_common,
  u_datamodule, form_defineswitchingchannel, Graphics;

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
var F: TFormEditSwitchingChannel;
begin
  if HaveErrorOnNameOrShortName then exit;

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
  i: integer;
  oldName: string;
begin
  FormNew := TFormDefineNewChannel.Create(NIL);
  FormNew.ExistingChannel := FExistingChannels;

  oldName := FChanFrames[aIndex].ChanName;
  i := FExistingChannels^.NameToIndex(oldName);
  if i = -1 then exit;

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

procedure TFrameEditMode.DoAddChannelFrame(const aName: string);
var i, xx, yy, w: integer;
begin
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
  FChanFrames[i].Tag := i;

  ClientHeight := FChanFrames[i].Top + FChanFrames[i].Height +  // ClientHeight + FChanFrames[i].Height;
                  ScaleDesignToForm(10) + BAddChannel.Height + ScaleDesignToForm(10);
end;

procedure TFrameEditMode.DoDeleteChannelFrame(aIndex: integer);
var i, h: integer;
begin
  if (aIndex < 0) or (aIndex > High(FChanFrames)) then exit;

  h := FChanFrames[aIndex].Height;
  FChanFrames[aIndex].Free;
  Delete(FChanFrames, aIndex, 1);
  // shift the Top position, Tag and the prefix number of the next labels
  for i:=High(FChanFrames) downto aIndex do begin
    FChanFrames[i].Top := FChanFrames[i].Top - h;
    FChanFrames[i].Tag := FChanFrames[i].Tag - 1;
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
  SetPanelToolPosition(TFrameViewModeItem(Sender).Tag);
end;

procedure TFrameEditMode.ChanLabelMouseLeave(Sender: TObject);
begin
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
    if FChanFrames[i].ChanName = aOldName then begin
      FChanFrames[i].ChanName := aNewName;
      exit;
    end;
end;

constructor TFrameEditMode.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  // manual translation
  Label6.Caption := SNameAlreadyUsed;
  Label7.Caption := SNameAlreadyUsed;
  Label2.Caption := SName;

  BAddSwitchingChannel.ImageIndex := Ord(High(TChannelType))+1;
  BAddSwitchingChannel.ImageWidth := DataModule1.ImageList1.Width;
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
var i, j: integer;
begin
  Result := NIL;
  if Length(FChanFrames) = 0 then exit;

  SetLength(Result, Length(FChanFrames));
  for i:=0 to High(Result) do
    Result[i] := FChanFrames[i].ChanName;
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
var A: TStringArray;
  i, j: integer;
begin
  if HaveErrorOnNameOrShortName then exit(True);

  // used channels list
  A := GetChannelsUsed;
  if length(A) = 0 then exit(True);
  // check the integrity of each used channels
  for i:=0 to High(A) do begin
    j := FExistingChannels^.NameToIndex(A[i]);
    if j = -1 then exit(True);
    if FExistingChannels^[j].HaveRangesError then exit(True);
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

  // exchange Tag
  i := FChanFrames[i1].Tag;
  FChanFrames[i1].Tag := FChanFrames[i2].Tag;
  FChanFrames[i2].Tag := i;

  // exchange position in list
  o := FChanFrames[i1];
  FChanFrames[i1] := FChanFrames[i2];
  FChanFrames[i2] := o;
end;

end.

