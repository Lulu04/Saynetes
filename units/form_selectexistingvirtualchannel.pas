unit form_selectexistingvirtualchannel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons;

type

  { TFormSelectExistingVirtualChannel }

  TFormSelectExistingVirtualChannel = class(TForm)
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
    procedure SetModeName(AValue: string);

  public
    // contains the names of the virtual channels to re-use, in case of user click 'Add existing'
    UseExistingNames: TStringArray;

    procedure FillWith(const aVirtualNamesAlreadyUsedByMode: TStringArray);

    property ModeName: string write SetModeName;
  end;


implementation
uses LCLType, u_resource_string, u_list_dmxuniverse, u_helper, form_help;

{$R *.lfm}

{ TFormSelectExistingVirtualChannel }

procedure TFormSelectExistingVirtualChannel.FormCreate(Sender: TObject);
begin
  // manual translation
  BCreateNew.Caption := SCreateNew;
  Label1.Caption := SAddVirtualChannelToMode;
end;

procedure TFormSelectExistingVirtualChannel.BAddExistingClick(Sender: TObject);
var i, j, k: integer;
begin
  if Sender = BAddExisting then begin
    if LB.SelCount = 0 then exit;
    UseExistingNames := NIL;
    k := 0;
    SetLength(UseExistingNames, LB.SelCount);
    for i:=0 to LB.Count-1 do
      if LB.Selected[i] then begin
        j := FVirtualChannelInMode.IndexOfVirtualName(LB.Items.Strings[i]);
        UseExistingNames[k] := FVirtualChannelInMode[j].PackToString; //LB.Items.Strings[i];
        inc(k);
      end;
    ModalResult := mrOk;
  end;

  if Sender = BCreateNew then begin
    ModalResult := mrIgnore;
  end;
end;

procedure TFormSelectExistingVirtualChannel.BHelpClick(Sender: TObject);
begin
  _ShowHelp(HelpSelectExistingSwitchingChannel, BHelp);
end;

procedure TFormSelectExistingVirtualChannel.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then ModalResult := mrCancel;
end;

procedure TFormSelectExistingVirtualChannel.LBSelectionChange(Sender: TObject; User: boolean);
begin
  BAddExisting.Enabled := LB.ItemIndex <> -1;
end;

procedure TFormSelectExistingVirtualChannel.SetModeName(AValue: string);
begin
  Label5.Caption := '"'+AValue+'"';
end;

procedure TFormSelectExistingVirtualChannel.FillWith(const aVirtualNamesAlreadyUsedByMode: TStringArray);
var i, j: integer;
  flagAlreadyUsed: boolean;
begin
  LB.Clear;
  for i:=0 to High(FVirtualChannelInMode) do begin
    flagAlreadyUsed := False;
    for j:=0 to High(aVirtualNamesAlreadyUsedByMode) do
      flagAlreadyUsed := flagAlreadyUsed or (FVirtualChannelInMode[i].VirtualName = aVirtualNamesAlreadyUsedByMode[j]);
    if not flagAlreadyUsed then LB.Items.Add(FVirtualChannelInMode[i].VirtualName);
  end;

  LB.ItemIndex := -1;
  BAddExisting.Enabled := False;
end;

end.

