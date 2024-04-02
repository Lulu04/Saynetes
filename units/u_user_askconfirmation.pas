unit u_user_askconfirmation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ExtCtrls, System.UITypes, BGRABitmap, BGRABitmapTypes;

type

  { TFormUserConfirmation }

  TFormUserConfirmation = class(TForm)
    PB: TPaintBox;
    ImageList1: TImageList;
    Label1: TLabel;
    Shape1: TShape;
    BOk: TSpeedButton;
    BCancel: TSpeedButton;
    procedure FormKeyUp({%H-}Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure Label1Resize({%H-}Sender: TObject);
    procedure BOkClick({%H-}Sender: TObject);
    procedure BCancelClick({%H-}Sender: TObject);
    procedure PBPaint(Sender: TObject);
  private
    FImage: TBGRABitmap;
  public
    function Init( const mess, syes, sno: string; aMsgType: TMsgDlgType ): integer;
  end;

var FormUserConfirmation: TFormUserConfirmation;



implementation
uses LCLType, u_utils;


{$R *.lfm}

{ TFormUserConfirmation }

procedure TFormUserConfirmation.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    ModalResult := mrCancel;
end;

procedure TFormUserConfirmation.Label1Resize(Sender: TObject);
begin
  // adjust the height of the windows according to the height of the message
  Height := Label1.Top+
            Label1.Height+
            BOk.Height*2+
            BOk.Height div 2;
end;

procedure TFormUserConfirmation.BOkClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TFormUserConfirmation.BCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFormUserConfirmation.PBPaint(Sender: TObject);
begin
  if FImage <> NIL then FImage.Draw(PB.Canvas, 0, 0, False);
end;

function TFormUserConfirmation.Init( const mess, syes, sno: string; aMsgType: TMsgDlgType ): integer;
begin
  Label1.Caption := mess;
  BOk.Caption := syes;
  BCancel.Caption := sno;
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

