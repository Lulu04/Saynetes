unit form_definenewchannel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin, Grids, Buttons, LCLTranslator, LCLType,
  u_list_dmxuniverse, frame_cb_channeltype, u_common, u_notebook_util;

type

  { TFormDefineNewChannel }

  TFormDefineNewChannel = class(TForm)
    BOK: TSpeedButton;
    BCancel: TSpeedButton;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    NB: TNotebook;
    PageCustom: TPage;
    PagePreset: TPage;
    Panel1: TPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    SE1: TSpinEdit;
    SG: TStringGrid;
    procedure BOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure RadioButton1Change(Sender: TObject);
    procedure SGSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
    procedure SGSelectEditor(Sender: TObject; aCol, aRow: Integer; var Editor: TWinControl);
    procedure SGUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure SGValidateEntry(Sender: TObject; aCol, aRow: Integer; const OldValue: string; var NewValue: String);
  private
    CheckedLabelManager: TCheckedLabelManager;
    FrameCBChannelType1, FrameCBChannelType2: TFrameCBChannelType;
    FSelectedChannelType: TChannelType;
    procedure ProcessChannelTypeChangeEvent(Sender: TObject);
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
  Label1.Caption := SCreateNewChannel;
  Label2.Caption := SName;
  SG.Columns.Items[0].Title.Caption := SMin;
  SG.Columns.Items[1].Title.Caption := SMax;
  SG.Columns.Items[2].Title.Caption := SDescription;

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

  // force first plage
  SG.Cells[0,1] := '0';
  SG.Cells[1,1] := '255';

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

procedure TFormDefineNewChannel.SGSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
begin
  // avoid the first cell edition
  CanSelect := not((aCol = 0) and (aRow = 1));
end;

procedure TFormDefineNewChannel.SGSelectEditor(Sender: TObject; aCol,
  aRow: Integer; var Editor: TWinControl);
begin
  // set the background of the editor
  Editor.Color := RGBToColor(20,20,20);
end;

procedure TFormDefineNewChannel.SGUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin

  if (SG.Col in [0..1]) then begin // min max -> only number
    if Length(UTF8Key) <> 1 then UTF8Key := ''
      else if not (UTF8Key[1] in ['0'..'9',chr(VK_BACK)]) then UTF8Key := '';
  end;

  if SG.Col = 2 then begin  // only letter and ,%^#-_+
    if Length(UTF8Key) <> 1 then UTF8Key := ''
      else if not (UTF8Key[1] in ['a'..'z', 'A'..'Z', '0'..'9', ' ', '.', ',', '%', '^', '#', '-', '_', '+',
                                  '(', ')', '[', ']', '{', '}', chr(VK_BACK)]) then UTF8Key := '';
  end;
end;

procedure TFormDefineNewChannel.SGValidateEntry(Sender: TObject; aCol,
  aRow: Integer; const OldValue: string; var NewValue: String);
var v, v1: integer;
  procedure NeedRowAfter(aRowIndex: integer);
  begin
    if aRowIndex = SG.RowCount-1 then
      SG.RowCount := SG.RowCount + 1;
  end;
  procedure SetAsLastRow(aRowIndex: integer);
  begin
    SG.RowCount := aRowIndex + 1;
  end;

begin
  if not (aCol in [0..1]) or (aRow = 0) then exit;

  // check if not a number
  if not TryStrToInt(SG.Cells[aCol, aRow], v) then begin
    NewValue := OldValue;
    exit;
  end;
{  // check if the number is not in range [0..255]
  if v < 1 then begin
    NewValue := '1';
    exit;
  end; }
  if v > 255 then begin
    NewValue := '255';
    SetAsLastRow(aRow);
    exit;
  end;

  // Min must be equal to previous Max+1
  if (aCol = 0) and (aRow > 2) then
    if TryStrToInt(SG.Cells[1, aRow-1], v1) then begin
      NewValue := (v1+1).ToString;
      exit;
    end;

  if (aCol = 1) and (aRow > 0) then begin
    // Max must be greater or equal to Min
    if TryStrToInt(SG.Cells[0,aRow], v1) then
      if v < v1 then begin
        v := v1;
        SG.Cells[1,aRow] := v.ToString;
      end;

    // next Min must be equal to Max+1
    if v < 255 then begin
      NeedRowAfter(aRow);
      SG.Cells[0,aRow+1] := (v+1).ToString;
      if SG.Cells[1,aRow+1] = '' then begin
        SG.Cells[1,aRow+1] := '255';
        SetAsLastRow(aRow+1);
      end;
    end else SetAsLastRow(aRow);  // max = 255 -> last row
  end;
end;

procedure TFormDefineNewChannel.ProcessChannelTypeChangeEvent(Sender: TObject);
var o: TFrameCBChannelType;
  readable, extra: string;
begin
  o := Sender as TFrameCBChannelType;
  if o.ItemIndex = -1 then exit;

  o.GetData(FSelectedChannelType, readable, extra);

  if o = FrameCBChannelType1 then begin
    Edit1.Text := readable;
    SG.RowCount := 2;
    SG.Cells[0,1] := '0';
    SG.Cells[1,1] := '255';
    SG.Cells[2,1] := {readable +} extra;
  end;
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

  // first begin value must be 0
  if not TryStrToInt(SG.Cells[0,1], b) then exit(True);
  if b <> 0 then exit(True);
  // check ranges
  current := 0;
  for i:=1 to SG.RowCount-1 do begin
    if not TryStrToInt(SG.Cells[0,i], b) then exit(True);
    if not TryStrToInt(SG.Cells[1,i], e) then exit(True);
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
begin
  FEditingChannel := True;
  Edit1.Text := p^.NameID;
  SE1.Value := p^.DefaultValue;
  RadioButton2.Checked := True;
  FrameCBChannelType2.SelectedType := p^.ChanType;
  FSelectedChannelType := p^.ChanType;
  SG.RowCount := Length(p^.Ranges)+1;
  for i:=0 to High(p^.Ranges) do begin
    SG.Cells[0,i+1] := p^.Ranges[i].BeginValue.ToString;
    SG.Cells[1,i+1] := p^.Ranges[i].EndValue.ToString;
    s := p^.Ranges[i].Text;
    if p^.Ranges[i].Extra <> '' then s := s + ' ' + p^.Ranges[i].Extra;
    SG.Cells[2,i+1] := s;
  end;
end;

function TFormDefineNewChannel.GetData: string;
var i: integer;
begin
  FChannelDef.InitDefault;
  FChannelDef.NameID := Trim(Edit1.Text);
  FChannelDef.DefaultValue := SE1.Value;
  FChannelDef.ChanType := FSelectedChannelType;

  FChannelDef.Ranges := NIL;
  for i:=1 to SG.RowCount-1 do begin
    SetLength(FChannelDef.Ranges, Length(FChannelDef.Ranges)+1);
    FChannelDef.Ranges[i-1].InitDefault;
    FChannelDef.Ranges[i-1].BeginValue := SG.Cells[0,i].ToInteger;
    FChannelDef.Ranges[i-1].EndValue := SG.Cells[1,i].ToInteger;
    FChannelDef.Ranges[i-1].Text := SG.Cells[2,i];
    if FChannelDef.Ranges[i-1].EndValue = 255 then break;
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

