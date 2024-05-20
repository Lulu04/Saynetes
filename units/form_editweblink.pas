unit form_editweblink;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls,
  ExtCtrls;

type

  { TFormEditWebLink }

  TFormEditWebLink = class(TForm)
    BCancel: TSpeedButton;
    BOK: TSpeedButton;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    Label18: TLabel;
    Label19: TLabel;
    Shape1: TShape;
    procedure BOKClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    function getLinkType: string;
    function getUrl: string;

  public
    property LinkType: string read getLinkType;
    property Url: string read getUrl;
  end;

var
  FormEditWebLink: TFormEditWebLink;

implementation
uses LCLType;

{$R *.lfm}

{ TFormEditWebLink }

procedure TFormEditWebLink.BOKClick(Sender: TObject);
begin
  if Sender = BOK then begin
    if Trim(Edit1.Text) = '' then begin
      Edit1.Color := clRed;
      Application.ProcessMessages;
      Sleep(200);
      Edit1.Color := clDefault;
      exit;
    end;
    ModalResult := mrOk;
  end;

  if Sender = BCancel then begin
    ModalResult := mrCancel;
  end;
end;

procedure TFormEditWebLink.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then ModalResult := mrCancel;
end;

procedure TFormEditWebLink.FormShow(Sender: TObject);
begin
  ComboBox1.ItemIndex := 0;
end;

function TFormEditWebLink.getLinkType: string;
begin
  Result := ComboBox1.Text;
end;

function TFormEditWebLink.getUrl: string;
begin
  Result := Trim(Edit1.Text);
end;

end.

