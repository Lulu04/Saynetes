unit form_selectexistingchannel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, u_list_dmxuniverse;

type

  { TFormSelectExistingChannel }

  TFormSelectExistingChannel = class(TForm)
    BAddExisting: TSpeedButton;
    BCreateNew: TSpeedButton;
    BHelp: TSpeedButton;
    Label1: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    LB: TListBox;
    Panel1: TPanel;
    procedure BAddExistingClick(Sender: TObject);
    procedure BHelpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure LBSelectionChange(Sender: TObject; User: boolean);
  private
    FExistingChannels: PFixLibAvailableChannels;
    procedure SetModeName(AValue: string);

  public
    // contains the names of the channels to re-use, in case of user click 'Add existing'
    UseExistingNames: TStringArray;

    procedure FillWith(p: PFixLibAvailableChannels; const aChannelNamesAlreadyUsedByMode: TStringArray);
    property ModeName: string write SetModeName;
  end;


implementation
uses LCLType, u_resource_string, form_help;

{$R *.lfm}

{ TFormSelectExistingChannel }

procedure TFormSelectExistingChannel.BAddExistingClick(Sender: TObject);
var i, k: integer;
begin
  if Sender = BAddExisting then begin
    if LB.SelCount = 0 then exit;
    UseExistingNames := NIL;
    k := 0;
    SetLength(UseExistingNames, LB.SelCount);
    for i:=0 to LB.Count-1 do
      if LB.Selected[i] then begin
        UseExistingNames[k] := LB.Items.Strings[i];
        inc(k);
      end;
    ModalResult := mrOk;
  end;

  if Sender = BCreateNew then begin
    ModalResult := mrIgnore;
  end;
end;

procedure TFormSelectExistingChannel.BHelpClick(Sender: TObject);
begin
  _ShowHelp(HelpSelectExistingChannel, BHelp);
end;

procedure TFormSelectExistingChannel.FormCreate(Sender: TObject);
begin
  // manual translation
  BCreateNew.Caption := SCreateNew;
end;

procedure TFormSelectExistingChannel.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then ModalResult := mrCancel;
end;

procedure TFormSelectExistingChannel.LBSelectionChange(Sender: TObject; User: boolean);
begin
  BAddExisting.Enabled := LB.ItemIndex <> -1;
end;

procedure TFormSelectExistingChannel.SetModeName(AValue: string);
begin
  Label5.Caption := '"'+AValue+'"';
end;

procedure TFormSelectExistingChannel.FillWith(p: PFixLibAvailableChannels; const aChannelNamesAlreadyUsedByMode: TStringArray);
var i, j: integer;
  flagAlreadyUsed: boolean;
begin
  LB.Clear;
  for i:=0 to High(p^) do begin
    flagAlreadyUsed := False;
    for j:=0 to High(aChannelNamesAlreadyUsedByMode) do
      flagAlreadyUsed := flagAlreadyUsed or (p^[i].NameID = aChannelNamesAlreadyUsedByMode[j]);
    if not flagAlreadyUsed then LB.Items.Add(p^[i].NameID);
  end;

  LB.ItemIndex := -1;
  BAddExisting.Enabled := False;

  FExistingChannels := p;
end;

end.

