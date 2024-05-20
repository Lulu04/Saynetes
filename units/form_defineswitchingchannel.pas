unit form_defineswitchingchannel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  CheckLst, Buttons,
  u_list_dmxuniverse, frame_editmode;

type

  { TFormEditSwitchingChannel }

  TFormEditSwitchingChannel = class(TForm)
    BOk: TSpeedButton;
    BNew: TSpeedButton;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    LB: TCheckListBox;
    Panel1: TPanel;
    procedure BNewClick(Sender: TObject);
    procedure BOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LBSelectionChange(Sender: TObject; User: boolean);
  private
    FEditingChannel: boolean;
    FTargetModeFrame: TFrameEditMode;
    function GetPackedVirtualNameAndSubChannelNames: string;
    function GetSubChannelNames: TStringArray;
    function GetVirtualName: string;
    procedure SetModeName(AValue: string);
  public
    procedure EditExistingChannel(p: PVirtualChannelForSwitch);

    procedure FillWith(p: PFixLibAvailableChannels);
    property ModeName: string write SetModeName;
    property TargetModeFrame: TFrameEditMode read FTargetModeFrame write FTargetModeFrame;

    property VirtualName: string read GetVirtualName;
    property SubChannelNames: TStringArray read GetSubChannelNames;
    property PackedVirtualNameAndSubChannelNames: string read GetPackedVirtualNameAndSubChannelNames;
  end;


implementation

uses u_resource_string, form_definenewchannel, u_utils;

{$R *.lfm}

{ TFormEditSwitchingChannel }

procedure TFormEditSwitchingChannel.FormCreate(Sender: TObject);
begin
  // manual translations
  BOk.Caption := SOk;
  BNew.Caption := SCreateNew;
  Label6.Caption := SNameAlreadyUsed;
  Label1.Caption := SAddVirtualChannelToMode;
end;

procedure TFormEditSwitchingChannel.LBSelectionChange(Sender: TObject; User: boolean);
begin
  Label7.Visible := False;
end;

procedure TFormEditSwitchingChannel.BNewClick(Sender: TObject);
var FormNew: TFormDefineNewChannel;
  chanName: TStringArray;
  i: integer;
begin
  chanName := NIL;
  FormNew := TFormDefineNewChannel.Create(NIL);
  FormNew.ExistingChannel := TargetModeFrame.ExistingChannels;
  try
    if FormNew.ShowModal = mrOk then begin
      // add the new created channel
      i := Length(TargetModeFrame.ExistingChannels^);
      SetLength(TargetModeFrame.ExistingChannels^, i+1);
      TargetModeFrame.ExistingChannels^[i].InitDefault;
      TargetModeFrame.ExistingChannels^[i].LoadFromString(FormNew.GetData);
      SetLength(chanName, 1);
      chanName[0] := TargetModeFrame.ExistingChannels^[i].NameID;
    end;
  finally
    FormNew.Free;
  end;

  // add new created channel name to the checklistbox
  if chanName = NIL then exit;
  for i:=0 to High(chanName) do
    LB.Items.Add(chanName[i]);

{  for i:=0 to High(chanName) do
    TargetModeFrame.DoAddChannelFrame(chanName[i]); }

  TargetModeFrame.Modified := True;
end;

procedure TFormEditSwitchingChannel.BOkClick(Sender: TObject);
var i, c: integer;
begin
  if (Trim(Edit1.Text) = '') or Label6.Visible then begin
    DoRedFlashOnEdit(Edit1);
    exit;
  end;

  // check if the virtual name don't already exists
  if not FEditingChannel then begin
    for i:=0 to High(FVirtualChannelInMode) do
      if FVirtualChannelInMode[i].VirtualName = Trim(Edit1.Text) then begin
        Label6.Visible := True;
        DoRedFlashOnEdit(Edit1);
        exit;
      end;
  end;

  // check if at least two channels are checked in the list
  c := 0;
  for i:=0 to LB.Count-1 do
    if LB.Checked[i] then inc(c);
  if c < 2 then begin
    Label7.Visible := True;
    exit;
  end;

  ModalResult := mrOk;
end;

function TFormEditSwitchingChannel.GetPackedVirtualNameAndSubChannelNames: string;
var
  A: TStringArray;
  i: Integer;
begin
  Result := VirtualName+':';
  A := SubChannelNames;
  for i:=0 to High(A) do begin
    Result := Result + A[i];
    if i < High(A) then Result := Result + ';';
  end;
end;

function TFormEditSwitchingChannel.GetSubChannelNames: TStringArray;
var i, j: integer;
begin
  Result := NIL;
  for i:=0 to LB.Count-1 do
    if LB.Checked[i] then begin
      j := Length(Result);
      SetLength(Result, j+1);
      Result[j] := LB.Items.Strings[i];
    end;
end;

function TFormEditSwitchingChannel.GetVirtualName: string;
begin
  Result := Trim(Edit1.Text);
end;

procedure TFormEditSwitchingChannel.SetModeName(AValue: string);
begin
  Label5.Caption := AValue;
end;

procedure TFormEditSwitchingChannel.EditExistingChannel(p: PVirtualChannelForSwitch);
var i, j: integer;
begin
  FEditingChannel := True;
  Edit1.Text := p^.VirtualName;

  // check the used sub-channels
  for i:=0 to High(p^.SubChannelIDs) do begin
    j := LB.Items.IndexOf(p^.SubChannelIDs[i]);
    if j <> -1 then LB.Checked[j] := True;
  end;
end;

procedure TFormEditSwitchingChannel.FillWith(p: PFixLibAvailableChannels);
var i: Integer;
begin
  LB.Clear;
  for i:=0 to High(p^) do
    LB.Items.Add(p^[i].NameID);
end;

end.

