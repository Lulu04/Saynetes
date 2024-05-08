unit frame_editmode;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Buttons,
  u_list_dmxuniverse;

type

  { TFrameEditMode }

  TFrameEditMode = class(TFrame)
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
    procedure BDeleteModeClick(Sender: TObject);
    procedure BAddChannelClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure FrameMouseLeave(Sender: TObject);
    procedure Panel1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
    FLabelIndexTargetedByPanelTools: integer;
    procedure DoEditChannelLabel(aIndex: integer);
    procedure DoAddChannelNameLabel(const aName: string);
    procedure DoDeleteChannelLabel(aIndex: integer);
    procedure ChanLabelMouseEnter(Sender: TObject);
    procedure ChanLabelMouseLeave(Sender: TObject);
    procedure SetPanelToolPosition(aLabelIndex: integer); // can be -1 to hide panel
    function PackChannelName(aIndex: integer; const aName: string): string;
    procedure SplitChannelName(aCaption: string; out aIndex: integer; out aName: string);
    procedure ExchangeChannel(i1, i2: integer);
  private
    FExistingChannels: PFixLibAvailableChannels;
    FModeIndex: integer;
    FOnHeightChange: TNotifyEvent;
    FChanLabel: array of TLabel;
    FCounterForChannelLabelName: integer;

    function GetChannelsUsed: TStringArray;
    function HaveErrorOnNameOrShortName: boolean;
    function GetHaveError: boolean;
    function GetModeName: string;
    function GetShortModeName: string;
    procedure SetModeIndex(AValue: integer);
    procedure SetModeName(AValue: string);
    procedure SetModified(AValue: boolean);
  public
    procedure ReplaceChannelName(const aOldNameNoPrefix, aNewNameNoPrefix: string);
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
  u_editfixturewizard, u_utils, u_resource_string, u_helper, Graphics;

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
    DoAddChannelNameLabel(chanName[i]);
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

procedure TFrameEditMode.DoEditChannelLabel(aIndex: integer);
var FormNew: TFormDefineNewChannel;
  i: integer;
  oldName: string;
begin
  FormNew := TFormDefineNewChannel.Create(NIL);
  FormNew.ExistingChannel := FExistingChannels;

  oldName := '';
  SplitChannelName(FChanLabel[aIndex].Caption, i, oldName);
  i := FExistingChannels^.NameToIndex(oldName);
  //if i = -1 then exit;
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
  for i:=0 to High(FChanLabel) do
    if InRange(Y, FChanLabel[i].Top, FChanLabel[i].Top+FChanLabel[i].Height) then begin
      index := i;
      break;
    end;

  SetPanelToolPosition(index);
end;

procedure TFrameEditMode.DoAddChannelNameLabel(const aName: string);
var i: integer;
begin
  i := Length(FChanLabel);
  SetLength(FChanLabel, i+1);
  FChanLabel[i] := TLabel.Create(Self);
  FChanLabel[i].Name := 'MyLabel'+FCounterForChannelLabelName.ToString;
  FChanLabel[i].Parent := Panel1;
  FChanLabel[i].Left := BAddChannel.Left;
  FChanLabel[i].Top := Label4.Top + Label4.Height + ScaleDesignToForm(5)+ i*Label4.Font.Height;//BAddChannel.Top;
  FChanLabel[i].AutoSize := False;
  FChanLabel[i].Width := Panel1.ClientWidth div 2;
  FChanLabel[i].Height := Label4.Font.Height;
  FChanLabel[i].Font.Height := Label4.Font.Height;
  FChanLabel[i].Caption := PackChannelName(FCounterForChannelLabelName, aName);
  if Odd(i) then FChanLabel[i].Color := PercentColor(Panel1.Color, -0.1)
    else FChanLabel[i].Color := PercentColor(Panel1.Color, 0.1);
  FChanLabel[i].OnMouseEnter := @ChanLabelMouseEnter;
  FChanLabel[i].OnMouseLeave := @ChanLabelMouseLeave;
  FChanLabel[i].Tag := i;

  ClientHeight := ClientHeight + FChanLabel[i].Height;
  inc(FCounterForChannelLabelName);
end;

procedure TFrameEditMode.DoDeleteChannelLabel(aIndex: integer);
var i, h, prefix: integer;
  na: string;
begin
  if (aIndex < 0) or (aIndex > High(FChanLabel)) then exit;
  h := FChanLabel[aIndex].Height;
  FChanLabel[aIndex].Free;
  Delete(FChanLabel, aIndex, 1);
  // shift the Top position, Tag and the prefix number of the next labels
  for i:=High(FChanLabel) downto aIndex do begin
    FChanLabel[i].Top := FChanLabel[i].Top - h;
    FChanLabel[i].Tag := FChanLabel[i].Tag - 1;
    SplitChannelName(FChanLabel[i].Caption, prefix, na);
    FChanLabel[i].Caption := PackChannelName(prefix-1, na);
  end;

  // adjust the height of the frame
  ClientHeight := ClientHeight - h;

  // ask the parent scrollbox to adjust its layout
  FOnHeightChange(Self);

  Modified := True;
end;

procedure TFrameEditMode.ChanLabelMouseEnter(Sender: TObject);
begin
  SetPanelToolPosition(TLabel(Sender).Tag);
end;

procedure TFrameEditMode.ChanLabelMouseLeave(Sender: TObject);
begin
  SetPanelToolPosition(-1);
end;

procedure TFrameEditMode.SetPanelToolPosition(aLabelIndex: integer);
begin
  PanelTools.Visible := aLabelIndex <> -1;
  if aLabelIndex <> -1 then begin
    PanelTools.Tag := aLabelIndex;
    PanelTools.Top := FChanLabel[aLabelIndex].Top + FChanLabel[aLabelIndex].Height div 2 - PanelTools.Height div 2;
    PanelTools.Left := FChanLabel[aLabelIndex].Left + FChanLabel[aLabelIndex].Width + ScaleDesignToForm(5);
  end;

  FLabelIndexTargetedByPanelTools := aLabelIndex;
end;

function TFrameEditMode.PackChannelName(aIndex: integer; const aName: string): string;
begin
  Result := (aIndex+1).ToString + '. ' + aName;
end;

procedure TFrameEditMode.SplitChannelName(aCaption: string; out aIndex: integer; out aName: string);
var i: integer;
begin
  i := Pos(' ', aCaption);
  aName := Copy(aCaption, i+1, Length(aCaption)-i);
  aIndex := Copy(aCaption, 1, i-2).ToInteger - 1;
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

procedure TFrameEditMode.ReplaceChannelName(const aOldNameNoPrefix, aNewNameNoPrefix: string);
var i, index: integer;
  na: string;
begin
  for i:=0 to High(FChanLabel) do begin
    SplitChannelName(FChanLabel[i].Caption, index, na);
    if na = aOldNameNoPrefix then begin
      FChanLabel[i].Caption := PackChannelName(index, aNewNameNoPrefix);
      exit;
    end;
  end;
end;

constructor TFrameEditMode.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  // manual translation
  Label6.Caption := SNameAlreadyUsed;
  Label7.Caption := SNameAlreadyUsed;
  Label2.Caption := SName;
end;

procedure TFrameEditMode.InitFrom(const aMode: TFixLibMode);
var i: integer;
begin
  ModeName := aMode.Name;
  Edit2.Text := aMode.ShortName;
  for i:=0 to high(aMode.ChannelsIDToUse) do
    DoAddChannelNameLabel(aMode.ChannelsIDToUse[i]);
end;

function TFrameEditMode.GetModeName: string;
begin
  Result := Trim(Edit1.Text);
end;

function TFrameEditMode.GetChannelsUsed: TStringArray;
var i, j: integer;
begin
  Result := NIL;
  if Length(FChanLabel) = 0 then exit;

  SetLength(Result, Length(FChanLabel));
  for i:=0 to High(Result) do begin
    j := Pos(' ', FChanLabel[i].Caption) + 1; // pos of the character next the first space 'xx. Name'
    Result[i] := Copy(FChanLabel[i].Caption, j, Length(FChanLabel[i].Caption)-j+1);
  end;
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
    if FExistingChannels^[j].HaveError then exit(True);
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
    DoEditChannelLabel(PanelTools.Tag);
    PanelTools.Visible := False;
  end;

  if Sender = BDeleteChannel then begin
    DoDeleteChannelLabel(PanelTools.Tag);
    PanelTools.Visible := False;
  end;

  if Sender = BUp then begin
    if FLabelIndexTargetedByPanelTools = 0 then exit;
    ExchangeChannel(FLabelIndexTargetedByPanelTools, FLabelIndexTargetedByPanelTools-1);
    PanelTools.Visible := False;
    Modified := True;
  end;

  if Sender = BDown then begin
    if FLabelIndexTargetedByPanelTools = High(FChanLabel) then exit;
    ExchangeChannel(FLabelIndexTargetedByPanelTools, FLabelIndexTargetedByPanelTools+1);
    PanelTools.Visible := False;
    Modified := True;
  end;
end;

procedure TFrameEditMode.ExchangeChannel(i1, i2: integer);
var o: TLabel;
  y, prefix1, prefix2: integer;
  name1, name2: string;
  c: TColor;
  i: integer;
begin
  // exchange color background
  c := FChanLabel[i1].Color;
  FChanLabel[i1].Color := FChanLabel[i2].Color;
  FChanLabel[i2].Color := c;

  // exchange y coordinates
  y := FChanLabel[i1].Top;
  FChanLabel[i1].Top := FChanLabel[i2].Top;
  FChanLabel[i2].Top := y;

  // exchange prefix
  SplitChannelName(FChanLabel[i1].Caption, prefix1, name1);
  SplitChannelName(FChanLabel[i2].Caption, prefix2, name2);
  FChanLabel[i1].Caption := PackChannelName(prefix2, name1);
  FChanLabel[i2].Caption := PackChannelName(prefix1, name2);

  // exchange Tag
  i := FChanLabel[i1].Tag;
  FChanLabel[i1].Tag := FChanLabel[i2].Tag;
  FChanLabel[i2].Tag := i;

  // exchange position in list
  o := FChanLabel[i1];
  FChanLabel[i1] := FChanLabel[i2];
  FChanLabel[i2] := o;
end;

end.

