unit form_splash;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  BGRABitmap, BGRABitmapTypes;

type

  { TFormSplash }

  TFormSplash = class(TForm)
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    PB: TPaintBox;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PBPaint(Sender: TObject);
  private
    FImage: TBGRABitmap;
    procedure SetTextLoad(AValue: string);

  public
    property TextLoad: string write SetTextLoad;
  end;

var
  FormSplash: TFormSplash;

implementation
uses u_apputils, utilitaire_bgrabitmap;

{$R *.lfm}

{ TFormSplash }

procedure TFormSplash.FormShow(Sender: TObject);
begin
 {$ifdef Windows}
  Label1.Font.Name := 'Verdana';         //default
 {$endif}

  TextLoad := ' ';
end;

procedure TFormSplash.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FImage);
end;

procedure TFormSplash.PBPaint(Sender: TObject);
begin
  if FImage = NIL then
    try
      FImage := SVGFileToBGRABitmap(GetAppImagesFolder+'SplashImage.svg', PB.ClientWidth, PB.ClientHeight);
    except
      FImage := TBGRABitmap.Create(PB.ClientWidth, PB.ClientHeight, BGRAPixelTransparent);
    end;

  FImage.Draw(PB.Canvas, 0, 0, False);
end;

procedure TFormSplash.SetTextLoad(AValue: string);
begin
  Label4.Caption := AValue;
  Application.ProcessMessages;
end;

end.

