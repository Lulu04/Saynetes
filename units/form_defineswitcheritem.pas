unit form_defineswitcheritem;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ExtCtrls, BGRABitmap, u_list_dmxuniverse;

type

  { TFormDefineSwitcher }

  TFormDefineSwitcher = class(TForm)
    BCancel: TSpeedButton;
    BOK: TSpeedButton;
    Image1: TImage;
    Label4: TLabel;
    Label5: TLabel;
    LBVirtual: TListBox;
    LBSub: TListBox;
    Shape1: TShape;
    procedure BOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LBVirtualSelectionChange(Sender: TObject; User: boolean);
  private
    FInitializing, FEditing: Boolean;
    FSubChannelName: string;
    FVirtualName: string;
  public
    procedure EditSwitcher(const aVirtualName, aSubChannel: string);

    property VirtualName: string read FVirtualName;
    property SubChannelName: string read FSubChannelName;
  end;

var
  FormDefineSwitcher: TFormDefineSwitcher;

implementation

uses u_utils, u_apputils, u_resource_string, utilitaire_bgrabitmap;

{$R *.lfm}

{ TFormDefineSwitcher }

procedure TFormDefineSwitcher.FormCreate(Sender: TObject);
var i: integer;
  ima: TBGRABitmap;
  bmp: TBitmap;
begin
  // manual translation
  BOk.Caption := SOk;
  BCancel.Caption := SCancel;

  // fill virtual name list
  FInitializing := True;
  LBVirtual.Clear;
  for i:=0 to High(FVirtualChannelInMode) do
    LBVirtual.Items.Add(FVirtualChannelInMode[i].VirtualName);
  FInitializing := False;

  // image
  ima := SVGFileToBGRABitmap(GetAppChannelImagesFolder+'Switch.svg', Image1.ClientWidth, Image1.ClientHeight);
  bmp := TBitmap.Create;
  ima.AssignToBitmap(bmp);
  Image1.Picture.Assign(bmp);
  ima.Free;
  bmp.Free;
end;

procedure TFormDefineSwitcher.LBVirtualSelectionChange(Sender: TObject; User: boolean);
var i, j: integer;
begin
  if FInitializing then exit;
  i := LBVirtual.ItemIndex;
  if i = -1 then exit;

  LBSub.Clear;
  for j:=0 to High(FVirtualChannelInMode[i].SubChannelIDs) do
    LBSub.Items.Add(FVirtualChannelInMode[i].SubChannelIDs[j]);
end;

procedure TFormDefineSwitcher.EditSwitcher(const aVirtualName, aSubChannel: string);
var i: integer;
begin
  FEditing := True;
  Caption := SEditSwitcher;

  i := LBVirtual.Items.IndexOf(aVirtualName);
  LBVirtual.ItemIndex := i;

  i := LBSub.Items.IndexOf(aSubChannel);
  LBSub.ItemIndex := i;
end;

procedure TFormDefineSwitcher.BOKClick(Sender: TObject);
begin
  if Sender = BOK then begin
    if LBVirtual.ItemIndex = -1 then begin
      DoRedFlashOnWinControl(LBVirtual);
      exit;
    end;

    if LBSub.ItemIndex = -1 then begin
      DoRedFlashOnWinControl(LBSub);
      exit;
    end;

    FVirtualName := LBVirtual.GetSelectedText;
    FSubChannelName := LBSub.GetSelectedText;
    ModalResult := mrOk;
  end;

  if Sender = BCancel then begin
    ModalResult := mrCancel;
  end;
end;

end.

