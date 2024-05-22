unit form_definenewchannel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin, Grids, Buttons, LCLTranslator, LCLType,
  u_list_dmxuniverse, frame_cb_channeltype, u_common, u_notebook_util,
  frame_editrange;


type

  { TFormDefineNewChannel }

  TFormDefineNewChannel = class(TForm)
    BOK: TSpeedButton;
    BCancel: TSpeedButton;
    Edit1: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    NB: TNotebook;
    PageCustom: TPage;
    PagePreset: TPage;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    SB: TScrollBox;
    SE1: TSpinEdit;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    Splitter1: TSplitter;
    procedure BOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure RadioButton1Change(Sender: TObject);
    procedure Splitter1Moved(Sender: TObject);
  private
    CheckedLabelManager: TCheckedLabelManager;
    FrameCBChannelType1, FrameCBChannelType2: TFrameCBChannelType;
    FSelectedChannelType: TChannelType;
    procedure ProcessChannelTypeChangeEvent(Sender: TObject);
  private
    FLines: array of TFrameEditRange;
    FCounterForLineNames: integer;
    FLockOnBeginEndValueChange: boolean;
    procedure AdjustCellsWidth;
    procedure AdjustCellsHeight;
    procedure ProcessLineHeightChangeEvent(Sender: TObject);
    procedure ProcessCopyPreviousSwitcherEvent(Sender: TObject);
    procedure ProcessLineBeginEndChangeEvent(Sender: TObject);
    procedure DoAddLine;
    procedure DoDeleteLine(aIndex: integer);
    procedure DoDeleteAllLines;
  private
    FExistingChannels: PFixLibAvailableChannels;
    FChannelDef: TFixLibAvailableChannel;
    FEditingChannel: boolean;
    function ThereIsError: boolean;
  public

    procedure EditExistingChannel(p: PFixLibAvailableChannel);
    function GetData: string;

    property ExistingChannel: PFixLibAvailableChannels write FExistingChannels;
  end;


implementation

uses u_resource_string, Math;

{$R *.lfm}

{ TFormDefineNewChannel }

procedure TFormDefineNewChannel.FormCreate(Sender: TObject);
begin
  // manual translation
  BOK.Caption := sOk;
  BCancel.Caption := sCancel;
  Label1.Caption := SCreateNew;
  Label2.Caption := SName;
  Label5.Caption := SNameAlreadyUsed;


  Label7.Caption := SMin;
  Label8.Caption := SMax;
  Label9.Caption := SDescription;
  Label11.Caption := SExtra;
  Label10.Caption := SSwitchers;

  FrameCBChannelType1 := TFrameCBChannelType.Create(Self);
  FrameCBChannelType1.Name := 'FrameCBChannelType1';
  FrameCBChannelType1.Parent := PagePreset;
  FrameCBChannelType1.Align := alClient;
  FrameCBChannelType1.FillForPresetChannel;
  FrameCBChannelType1.OnChange := @ProcessChannelTypeChangeEvent;
  FrameCBChannelType1.CB.DropDownCount := 20;

  FrameCBChannelType2 := TFrameCBChannelType.Create(Self);
  FrameCBChannelType2.Name := 'FrameCBChannelType2';
  FrameCBChannelType2.Parent := PageCustom;
  FrameCBChannelType2.Align := alClient;
  FrameCBChannelType2.FillWithMinimalChannelTypes;
  FrameCBChannelType2.ItemIndex := 0;
  FrameCBChannelType2.OnChange := @ProcessChannelTypeChangeEvent;
  FrameCBChannelType2.CB.DropDownCount := 20;

  // add the first line
  DoAddLine;

  CheckedLabelManager := TCheckedLabelManager.Create;
  CheckedLabelManager.CaptureLabelClick(Label3);
  CheckedLabelManager.CaptureLabelClick(Label6);

  NB.PageIndex := NB.IndexOf(PagePreset);
end;

procedure TFormDefineNewChannel.FormDestroy(Sender: TObject);
begin
  FreeAndNil(CheckedLabelManager);
end;

procedure TFormDefineNewChannel.RadioButton1Change(Sender: TObject);
begin
  if RadioButton1.Checked then NB.PageIndex := NB.IndexOf(PagePreset)
    else NB.PageIndex := NB.IndexOf(PageCustom);
end;

procedure TFormDefineNewChannel.Splitter1Moved(Sender: TObject);
begin
  AdjustCellsWidth;
end;

procedure TFormDefineNewChannel.ProcessChannelTypeChangeEvent(Sender: TObject);
var o: TFrameCBChannelType;
  readable, txt: string;
begin
  o := Sender as TFrameCBChannelType;
  if o.ItemIndex = -1 then exit;

  o.GetData(FSelectedChannelType, readable, txt);

  if o = FrameCBChannelType1 then begin
    Edit1.Text := readable;

    if Length(FLines) > 1 then begin
      DoDeleteAllLines;
      DoAddLine;
    end;
    FLockOnBeginEndValueChange := True;
    FLines[0].BeginValue := 0;
    FLines[0].EndValue := 255;
    FLockOnBeginEndValueChange := False;
    FLines[0].Description := txt;
    FLines[0].Extra := '';
  end;
end;

procedure TFormDefineNewChannel.AdjustCellsWidth;
var i: integer;
begin
  for i:=0 to High(FLines) do begin
    FLines[i].Edit3.Width := Panel5.Width;
    FLines[i].FrameSwitcher.Width := Panel6.Width;
  end;
end;

procedure TFormDefineNewChannel.AdjustCellsHeight;
var i: integer;
begin
  for i:=1 to High(FLines) do
    FLines[i].Top := FLines[i-1].Top + FLines[i-1].Height;
end;

procedure TFormDefineNewChannel.ProcessLineHeightChangeEvent(Sender: TObject);
begin
  AdjustCellsHeight;
end;

procedure TFormDefineNewChannel.ProcessCopyPreviousSwitcherEvent(Sender: TObject);
var cur, prev: TFrameEditRange;
begin
  cur := TFrameEditRange(Sender);
  if cur.Index = 0 then exit;
  prev := FLines[cur.Index-1];
  cur.FrameSwitcher.InitFromText(prev.FrameSwitcher.ToText);
end;

procedure TFormDefineNewChannel.ProcessLineBeginEndChangeEvent(Sender: TObject);
var current, prev, nex: TFrameEditRange;
  i, v: integer;
  procedure SetAsLastLine(aIndex: integer);
  var j: integer;
  begin
    for j:= High(FLines) downto aIndex+1 do
      DoDeleteLine(j);
  end;
begin
  if FLockOnBeginEndValueChange then exit;
  FLockOnBeginEndValueChange := True;

  current := TFrameEditRange(Sender);
  i := current.Index;
  if i = 0 then prev := NIL
    else prev := FLines[i-1];
  if i = High(FLines) then nex := NIL
    else nex := FLines[i+1];

  // Min must be equal to previous Max+1
  if (prev <> NIL) {and (current.BeginValue <= prev.EndValue)} then
    current.BeginValue := prev.EndValue + 1;

  // next Min must be equal to Max+1
  if current.EndValue < 255 then begin
    if nex = NIL then begin
      DoAddLine;
      nex := FLines[i+1];
    end;
    nex.BeginValue := current.EndValue + 1;
    nex.EndValue := 255;
  end else SetAsLastLine(i);

  FLockOnBeginEndValueChange := False;
end;

procedure TFormDefineNewChannel.DoAddLine;
var i: integer;
begin
  i := Length(FLines);
  SetLength(FLines, i+1);
  FLines[i] := TFrameEditRange.Create(Self);
  FLines[i].Name := 'Line'+FCounterForLineNames.ToString;
  FLines[i].Parent := SB;
  if i = 0 then FLines[i].Top := 0
    else FLines[i].Top := FLines[i-1].Top + FLines[i-1].Height;

  FLines[i].Width := Panel2.Width;
  FLines[i].Edit3.Width := Panel5.Width;
  FLines[i].Edit4.Width := Panel7.Width;
  FLines[i].FrameSwitcher.Left := Panel6.Left;
  FLines[i].FrameSwitcher.Width := Panel6.Width;

  FLines[i].OnHeightChange := @ProcessLineHeightChangeEvent;
  FLines[i].OnCopyPreviousSwitcher := @ProcessCopyPreviousSwitcherEvent;
  FLines[i].OnBeginEndChange := @ProcessLineBeginEndChangeEvent;
  FLines[i].Index := i;

  if i = 0 then begin
    FLines[i].BeginValue := 0;
    FLines[i].EndValue := 255;
  end;

  inc(FCounterForLineNames);
end;

procedure TFormDefineNewChannel.DoDeleteLine(aIndex: integer);
var i: integer;
begin
  FLines[aIndex].Free;
  Delete(FLines, aIndex, 1);
  for i:=aIndex to High(FLines) do
    FLines[i].Index := FLines[i].Index - 1;
end;

procedure TFormDefineNewChannel.DoDeleteAllLines;
var i: integer;
begin
  for i:=High(FLines) downto 0 do
    DoDeleteLine(i);
end;

function TFormDefineNewChannel.ThereIsError: boolean;
var i, b, e, current: integer;
  s: string;
begin
  s := Trim(Edit1.Text);
  if s = '' then begin
    Edit1.Color := clRed;
    Application.ProcessMessages;
    Sleep(100);
    Edit1.Color := clDefault;
    exit(True);
  end;

  // check if the name don't already exist
  if not FEditingChannel then begin
    Label5.Visible := False;
    for i:=0 to High(FExistingChannels^) do
      if FExistingChannels^[i].NameID = s then begin
        Label5.Visible := True;
        exit(True);
      end;
  end;

  // check ranges
  current := 0;
  for i:=0 to High(FLines) do begin
    b := FLines[i].BeginValue;
    e := FLines[i].EndValue;
    if not InRange(b, 0, 255) then exit(True);
    if not InRange(e, 0, 255) then exit(True);
    if b > e then exit(True);
    if b <> current then exit(True);

    if e = 255 then break; // end
    current := e + 1;
  end;

  Result := False;
end;

procedure TFormDefineNewChannel.EditExistingChannel(p: PFixLibAvailableChannel);
var i: integer;
  s: string;
  o: TFrameEditRange;
begin
  FEditingChannel := True;
  Edit1.Text := p^.NameID;
  SE1.Value := p^.DefaultValue;
  RadioButton2.Checked := True;
  FrameCBChannelType2.SelectedType := p^.ChanType;
  FSelectedChannelType := p^.ChanType;

  DoDeleteLine(0);
  FLockOnBeginEndValueChange := True;
  for i:=0 to High(p^.Ranges) do begin
    DoAddLine;
    o := FLines[i];
    o.BeginValue := p^.Ranges[i].BeginValue;
    o.EndValue := p^.Ranges[i].EndValue;
    o.Description := p^.Ranges[i].Text;
    o.Extra := p^.Ranges[i].Extra;
    o.FrameSwitcher.InitFromText(p^.Ranges[i].GetSwitchsAsText);
  end;
  FLockOnBeginEndValueChange := False;
end;

function TFormDefineNewChannel.GetData: string;
var i: integer;
begin
  FChannelDef.InitDefault;
  FChannelDef.NameID := Trim(Edit1.Text);
  FChannelDef.DefaultValue := SE1.Value;
  FChannelDef.ChanType := FSelectedChannelType;

  FChannelDef.Ranges := NIL;
  SetLength(FChannelDef.Ranges, Length(FLines));
  for i:=0 to High(FLines) do begin
    FChannelDef.Ranges[i].InitDefault;
    FChannelDef.Ranges[i].BeginValue := FLines[i].BeginValue;
    FChannelDef.Ranges[i].EndValue := FLines[i].EndValue;
    FChannelDef.Ranges[i].Text := FLines[i].Description;
    FChannelDef.Ranges[i].Extra := FLines[i].Extra;
    FLines[i].InitRange(@FChannelDef.Ranges[i]);
    if FChannelDef.Ranges[i].EndValue = 255 then break;
  end;

  Result := FChannelDef.SaveToString;
end;

procedure TFormDefineNewChannel.BOKClick(Sender: TObject);
begin
  if Sender = BOK then begin
    if ThereIsError then exit;
    ModalResult := mrOk;
  end;

  if Sender = BCancel then begin
    ModalResult := mrCancel;
  end;
end;

end.

