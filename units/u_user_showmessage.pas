unit u_user_showmessage;

{$mode objfpc}{$H+}
{$WARN 5024 off : Parameter "$1" not used}
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, BGRABitmap, BGRABitmapTypes;

type

  { TFormUserMessage }

  TFormUserMessage = class(TForm)
    PB: TPaintBox;
    Label1: TLabel;
    Shape1: TShape;
    BOk: TSpeedButton;
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Label1Resize(Sender: TObject);
    procedure BOkClick(Sender: TObject);
    procedure PBPaint(Sender: TObject);
  private
    FImage: TBGRABitmap;
  public
    function Init( const mess, sbutton: string; aMsgType: TMsgDlgType ): integer;
  end;



implementation
uses LCLType, u_utils;

{$R *.lfm}

{ TFormUserMessage }

procedure TFormUserMessage.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key=VK_ESCAPE) or (Key=VK_RETURN)
    then ModalResult:=mrCancel;
end;

procedure TFormUserMessage.Label1Resize(Sender: TObject);
begin
  // adjust the height of the windows according to the height of the message
  Height:=Label1.Top+
          Label1.Height+
          BOk.Height*2+
          BOk.Height div 2;
end;

procedure TFormUserMessage.BOkClick(Sender: TObject);
begin
  ModalResult:=mrOk;
end;

procedure TFormUserMessage.PBPaint(Sender: TObject);
begin
  if FImage <> NIL then FImage.Draw(PB.Canvas, 0, 0, False);
end;

function TFormUserMessage.Init(const mess, sbutton: string; aMsgType: TMsgDlgType ): integer;
begin
  Label1.Caption := mess;
  BOk.Caption := sbutton;

  FImage := MsgDlgTypeToBGRABitmap(aMsgType, PB.Width, PB.Height);

  {$ifdef LCLGTK2}
  Hide;
  Application.ProcessMessages;
  {$endif}
  Result := ShowModal;
  FImage.Free;
  FImage := NIL;
end;

end.

