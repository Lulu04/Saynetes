unit form_edit_repetitivechannel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, Spin,
  u_notebook_util, u_common, u_list_dmxuniverse;

type

  TOnAddRepetitiveChannel = procedure(const aChannelToAdd: TFixLibAvailableChannel) of object;

  { TFormEditRepetitiveChannel }

  TFormEditRepetitiveChannel = class(TForm)
    BOK: TSpeedButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
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
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    RB3: TRadioButton;
    RB1: TRadioButton;
    RB2: TRadioButton;
    RB4: TRadioButton;
    SE1: TSpinEdit;
    SE2: TSpinEdit;
    procedure BOKClick(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    CheckedLabelManager: TCheckedLabelManager;
    FOnAddChannel: TOnAddRepetitiveChannel;
    FPreviewing: boolean;
    function GetEndIndex: integer;
    function GetStartIndex: integer;
  public
    function GetChannelName(const aChannelName: string; aIndex: integer): string;

    property StartIndex: integer read GetStartIndex;
    property EndIndex: integer read GetEndIndex;
    property OnAddChannel: TOnAddRepetitiveChannel read FOnAddChannel write FOnAddChannel;
  end;


implementation
uses LCLType, u_resource_string;

{$R *.lfm}

{ TFormEditRepetitiveChannel }

procedure TFormEditRepetitiveChannel.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then ModalResult := mrCancel;
end;

function TFormEditRepetitiveChannel.GetEndIndex: integer;
begin
  Result := SE2.Value;
end;

function TFormEditRepetitiveChannel.GetStartIndex: integer;
begin
  Result := SE1.Value;
end;

function TFormEditRepetitiveChannel.GetChannelName(const aChannelName: string; aIndex: integer): string;
var s, s1: string;
begin
  s := '';

  if CheckBox1.Checked then s := s + TrimLeft(Edit1.Text);
  if RB1.Checked then s := s + aIndex.ToString + ' ';

  s1 := '';
  if RB2.Checked then s1 := s1 + ' ' + aIndex.ToString;
  if CheckBox2.Checked then s1 := s1 + TrimRight(Edit2.Text);

  Result := s + aChannelName + s1;
end;

procedure TFormEditRepetitiveChannel.FormCreate(Sender: TObject);
begin
  CheckedLabelManager := TCheckedLabelManager.Create;
  CheckedLabelManager.CaptureLabelClick(Label2);
  CheckedLabelManager.CaptureLabelClick(Label4);
  CheckedLabelManager.CaptureLabelClick(Label7);
  CheckedLabelManager.CaptureLabelClick(Label8);

  // manual translation
  BOK.Caption := SOk;
  Label1.Caption := sAddRepetitiveChannels;
end;

procedure TFormEditRepetitiveChannel.CheckBox1Change(Sender: TObject);
var i: integer;
  s, s1: string;
begin
  if FPreviewing then exit;
  FPreviewing := True;

  if (SE1.Value > SE2.Value) or (SE2.Value < SE1.Value) then SE2.Value := SE1.Value + 1;

  Memo1.Clear;
  for i:=SE1.Value to SE2.Value do begin
    s := '';

    if CheckBox1.Checked then s := s + TrimLeft(Edit1.Text);
    if RB1.Checked then s := s + i.ToString + ' ';

    s1 := '';
    if RB2.Checked then s1 := s1 + ' ' + i.ToString;
    if CheckBox2.Checked then s1 := s1 + TrimRight(Edit2.Text);

    if RB3.Checked or RB4.Checked then begin
      Memo1.Lines.Add(GetChannelName('Red', i));
      Memo1.Lines.Add(GetChannelName('Green', i));
      Memo1.Lines.Add(GetChannelName('Blue', i));
    end;
    if RB4.Checked then Memo1.Lines.Add(GetChannelName('White', i));
  end;

  FPreviewing := False;
end;

procedure TFormEditRepetitiveChannel.BOKClick(Sender: TObject);
var i: integer;
  chan: TFixLibAvailableChannel;
  procedure SendChannel;
  begin
    chan.NameID := GetChannelName(chan.NameID, i);
    FOnAddChannel(chan);
  end;

begin
  for i:=SE1.Value to SE2.Value do begin
    chan.InitAsRed;
    SendChannel;
    chan.InitAsGreen;
    SendChannel;
    chan.InitAsBlue;
    SendChannel;
    if RB4.Checked then begin
      chan.ChanType := ctWhite;
      chan.NameID := 'White';
      SendChannel;
    end;
  end;

  ModalResult := mrOk;
end;

end.

