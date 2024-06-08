unit form_newmanufacturer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, LCLTranslator,
  u_common;

type

  { TFormNewManufacturer }

  TFormNewManufacturer = class(TForm)
    BOK: TSpeedButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Panel1: TPanel;
    procedure BOKClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FManufacturers: PManufacturers;
    FManufacturerName, FFolderName, FURL: string;
  public
    property Manufacturers: PManufacturers read FManufacturers write FManufacturers;

    property Name: string read FManufacturerName;
    property Folder: string read FFolderName;
    property URL: string read FURL;
  end;


implementation

uses u_resource_string, u_utils, u_helper, u_userdialogs, u_apputils, LCLType,
  utilitaire_fichier;

{$R *.lfm}

{ TFormNewManufacturer }

procedure TFormNewManufacturer.Edit1Change(Sender: TObject);
begin
  if Sender = Edit1 then Label5.Visible := False;
  if Sender = Edit2 then Label6.Visible := False;
end;

procedure TFormNewManufacturer.FormCreate(Sender: TObject);
begin
  // manual translations
  BOK.Caption := SOk;
end;

procedure TFormNewManufacturer.BOKClick(Sender: TObject);
var i: integer;
  flagBeep: boolean;
begin
  FManufacturerName := Trim(Edit1.Text);
  if FManufacturerName = '' then begin
    DoRedFlashOnEdit(Edit1);
    exit;
  end;

  FFolderName := Trim(Edit2.Text);
  flagBeep := FFolderName = '';
  if not flagBeep then
    for i:=1 to Length(FFolderName) do
      flagBeep := flagBeep or not (FFolderName[i] in ['a'..'z', '-']);
  if flagBeep then begin
    DoRedFlashOnEdit(Edit2);
    exit;
  end;

  // check if manufacturer name already exists
  if Manufacturers^.IndexOfName(FManufacturerName) <> -1 then begin
    Label5.Visible := True;
    exit;
  end;

  // check if folder already exists
  if Manufacturers^.IndexOfFolder(FFolderName) <> -1 then begin
    Label6.Visible := True;
    exit;
  end;

  FURL := Trim(Edit3.Text);

  // create the new folder
  try
    if not CreerRepertoire(GetAppDMXLibraryFolder+FFolderName) then begin
      ShowMess(SUnableToCreateManufacturerFolder, SOk, mtError);
      exit;
    end;
  finally
  end;
  // add and save
  Manufacturers^.AddAndSave(FManufacturerName, FFolderName, FURL);
  ModalResult := mrOk;
end;

procedure TFormNewManufacturer.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then ModalResult := mrCancel;
end;

end.

