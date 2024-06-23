unit form_about;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  LCLTranslator, Buttons,
  BGRABitmap, BGRABitmapTypes;

type

  { TFormAbout }

  TFormAbout = class(TForm)
    BCancel: TSpeedButton;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    PaintBox1: TPaintBox;
    procedure BCancelClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
  private
    FLogo: TBGRABitmap;
  public

  end;

var
  FormAbout: TFormAbout;

implementation

uses u_resource_string, u_common, u_utils;

{$R *.lfm}

{ TFormAbout }

procedure TFormAbout.FormShow(Sender: TObject);
begin
  BCancel.Caption := SClose;

  Memo1.Lines.Add(SProgramVersion+' '+APP_VERSION);
  Memo1.Lines.Add(SWrittenBy+' Lulu 2021-2024');
  Memo1.Lines.Add('-- '+SCredits+' --');
  Memo1.Lines.Add('');
  Memo1.Lines.Add(SIconAppBy+' Coralie Ambert');
  Memo1.Lines.Add('');
  Memo1.Lines.Add(SApplicationWrittenWith+' FreePascal/Lazarus');
  Memo1.Lines.Add('       https://www.lazarus-ide.org/');
  Memo1.Lines.Add('');
  Memo1.Lines.Add('BGRABitmap');
  Memo1.Lines.Add('       https://bgrabitmap.github.io/');
  Memo1.Lines.Add('');
  Memo1.Lines.Add('LibSndFile');
  Memo1.Lines.Add('       https://libsndfile.github.io/libsndfile/');
  Memo1.Lines.Add('');
  Memo1.Lines.Add('OpenALSoft');
  Memo1.Lines.Add('       https://openal-soft.org/');
  Memo1.Lines.Add('');
  Memo1.Lines.Add('HIDAPI');
  Memo1.Lines.Add('       https://github.com/dioannidis/HIDAPI.pas');
  Memo1.Lines.Add('');
  Memo1.Lines.Add('Open Fixture Library (OFL)');
  Memo1.Lines.Add('       https://open-fixture-library.org/');
  Memo1.Lines.Add('');
end;

procedure TFormAbout.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FLogo);
end;

procedure TFormAbout.BCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TFormAbout.PaintBox1Paint(Sender: TObject);
begin
  if FLogo = NIL then
    FLogo := GetLogoImage(PaintBox1.ClientWidth, PaintBox1.ClientHeight);

  FLogo.Draw(PaintBox1.Canvas, 0, 0, False);
end;

end.

