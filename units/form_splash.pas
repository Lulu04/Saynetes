unit form_splash;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TFormSplash }

  TFormSplash = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure FormShow(Sender: TObject);
  private
    procedure SetTextLoad(AValue: string);

  public
    property TextLoad: string write SetTextLoad;
  end;

var
  FormSplash: TFormSplash;

implementation
uses u_apputils, BGRABitmap, BGRABitmapTypes, utilitaire_bgrabitmap;

{$R *.lfm}

{ TFormSplash }

procedure TFormSplash.FormShow(Sender: TObject);
var ima: TBGRABitmap;
begin
 try
   ima := SVGFileToBGRABitmap(GetAppImagesFolder+'SplashImage.svg', Image1.Width, Image1.Height);
 except
   ima := TBGRABitmap.Create(Image1.Width, Image1.Height, BGRAPixelTransparent);
 end;

 {$ifdef Windows}
  Label1.Font.Name := 'Verdana';         //default
 {$endif}

  ima.AssignToBitmap(Image1.Picture.Bitmap);
  ima.Free;

  TextLoad := ' ';
end;

procedure TFormSplash.SetTextLoad(AValue: string);
begin
  Label4.Caption := AValue;
  Application.ProcessMessages;
end;

end.

