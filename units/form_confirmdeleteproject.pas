unit form_confirmdeleteproject;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons,
  BGRABitmap, BGRABitmaptypes, utilitaire_bgrabitmap;

type

  { TFormConfirmationDeleteProject }

  TFormConfirmationDeleteProject = class(TForm)
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    PB: TPaintBox;
    BOk: TSpeedButton;
    Shape1: TShape;
    procedure BOkClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure PBPaint(Sender: TObject);
  private
    FImage: TBGRABitmap;
  public
    procedure SetProjectName(const s: string);
  end;


implementation
uses LCLType, u_apputils, u_resource_string;

{$R *.lfm}

{ TFormConfirmationDeleteProject }

procedure TFormConfirmationDeleteProject.FormKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then ModalResult := mrCancel;
end;

procedure TFormConfirmationDeleteProject.FormShow(Sender: TObject);
begin
  BOk.Caption := SDelete;
  Edit1.Text := '';
  Edit1.SetFocus;
end;

procedure TFormConfirmationDeleteProject.PBPaint(Sender: TObject);
begin
  if FImage = NIL then
    try
      FImage := SVGFileToBGRABitmap(GetAppIconImagesFolder+'DlgWarning.svg', PB.ClientWidth, PB.ClientHeight);
    except
      FImage := TBGRABitmap.Create(PB.ClientWidth, PB.ClientHeight, BGRAPixelTransparent);
    end;

  FImage.Draw(PB.Canvas, 0, 0, False);
end;

procedure TFormConfirmationDeleteProject.SetProjectName(const s: string);
begin
  Label3.Caption := s;
end;

procedure TFormConfirmationDeleteProject.BOkClick(Sender: TObject);
begin
  if Edit1.Text = SDelete then
    ModalResult := mrOk;
end;

procedure TFormConfirmationDeleteProject.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FImage);
end;

end.

