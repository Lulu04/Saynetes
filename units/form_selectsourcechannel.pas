unit form_selectsourcechannel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons,
  u_list_dmxuniverse;

type

  { TFormSelectChannel }

  TFormSelectChannel = class(TForm)
    BHelp: TSpeedButton;
    BOk: TSpeedButton;
    Label2: TLabel;
    LB: TListBox;
    Panel1: TPanel;
    procedure BHelpClick(Sender: TObject);
    procedure BOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure LBSelectionChange(Sender: TObject; User: boolean);
  private
    FExistingChannels: PFixLibAvailableChannels;
    function GetSelected: PFixLibAvailableChannel;
  public
    procedure FillWith(p: PFixLibAvailableChannels);

    property Selected: PFixLibAvailableChannel read GetSelected;
  end;


implementation
uses LCLType, u_resource_string, form_help;

{$R *.lfm}

{ TFormSelectChannel }

procedure TFormSelectChannel.BOkClick(Sender: TObject);
begin
  if LB.ItemIndex = -1 then exit;
  ModalResult := mrOk;
end;

procedure TFormSelectChannel.BHelpClick(Sender: TObject);
begin
  _ShowHelp(HelpSelectSourceChannel, BHelp);
end;

procedure TFormSelectChannel.FormCreate(Sender: TObject);
begin
  // manual translations
  BOk.Caption := SOk;
end;

procedure TFormSelectChannel.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then ModalResult := mrCancel;
end;

procedure TFormSelectChannel.LBSelectionChange(Sender: TObject; User: boolean);
begin
  BOk.Enabled := LB.ItemIndex <> -1;
end;

function TFormSelectChannel.GetSelected: PFixLibAvailableChannel;
var i: integer;
begin
  i := LB.ItemIndex;
  if (i < 0) or (i > High(FExistingChannels^)) then Result := NIL
    else Result := @FExistingChannels^[i];
end;

procedure TFormSelectChannel.FillWith(p: PFixLibAvailableChannels);
var i: integer;
begin
  LB.Clear;
  for i:=0 to High(p^) do
    LB.Items.Add(p^[i].NameID);

  LB.ItemIndex := -1;
  BOk.Enabled := False;

  FExistingChannels := p;
end;

end.

