unit u_user_askconfirmation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ExtCtrls, System.UITypes, BGRABitmap, BGRABitmapTypes;

type

  { TFormUserConfirmation }

  TFormUserConfirmation = class(TForm)
    BNo: TSpeedButton;
    PB: TPaintBox;
    ImageList1: TImageList;
    Label1: TLabel;
    Shape1: TShape;
    BYes: TSpeedButton;
    BCancel: TSpeedButton;
    procedure BNoClick(Sender: TObject);
    procedure FormKeyUp({%H-}Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure Label1Resize({%H-}Sender: TObject);
    procedure BYesClick({%H-}Sender: TObject);
    procedure BCancelClick({%H-}Sender: TObject);
    procedure PBPaint(Sender: TObject);
  private
    FImage: TBGRABitmap;
  public
    function Init(const mess, syes, sno: string; aMsgType: TMsgDlgType): integer; overload;
    function Init(const mess, syes, sno, scancel: string; aMsgType: TMsgDlgType): integer; overload;
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
            BYes.Height*2+
            BYes.Height div 2;
end;

procedure TFormUserConfirmation.BYesClick(Sender: TObject);
begin
  ModalResult := mrYes;
end;

procedure TFormUserConfirmation.BNoClick(Sender: TObject);
begin
  ModalResult := mrNo;
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
  BYes.Caption := syes;
  BCancel.Caption := sno;
  FImage := MsgDlgTypeToBGRABitmap(aMsgType, PB.Width, PB.Height);

  // layout
  BCancel.Left := ClientWidth - ScaleDesignToForm(30) - BCancel.Width;
  BYes.Left := BCancel.Left - ScaleDesignToForm(60) - BYes.Width;
  BNo.Visible := False;

  {$ifdef LCLGTK2}
  Hide;
  Application.ProcessMessages;
  {$endif}
  Result := ShowModal;
  FImage.Free;
  FImage := NIL;
end;

function TFormUserConfirmation.Init(const mess, syes, sno, scancel: string; aMsgType: TMsgDlgType): integer;
begin
  Label1.Caption := mess;
  BYes.Caption := syes;
  BNo.Caption := sno;
  BCancel.Caption := scancel;
  FImage := MsgDlgTypeToBGRABitmap(aMsgType, PB.Width, PB.Height);

  // layout
  BCancel.Left := ClientWidth - ScaleDesignToForm(30) - BCancel.Width;
  BNo.Left := BCancel.Left - ScaleDesignToForm(40) - BNo.Width;
  BYes.Left := BNo.Left - ScaleDesignToForm(40) - BYes.Width;

  {$ifdef LCLGTK2}
  Hide;
  Application.ProcessMessages;
  {$endif}
  Result := ShowModal;
  FImage.Free;
  FImage := NIL;
end;

end.

