unit u_user_inputstring;

{$mode objfpc}{$H+}
{$WARN 5024 off : Parameter "$1" not used}
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  BGRABitmap, BGRABitmapTypes, frame_editstring;

type

  { TFormUserInput }

  TFormUserInput = class(TForm)
    PB: TPaintBox;
    Panel1: TPanel;
    Shape1: TShape;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure PBPaint(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
  private
    FImage: TBGRABitmap;
    FrameEditString1: TFrameEditString;
    function GetUserInput: string;

  public
    function Init( const mess, sok, scancel, sinput: string; aMsgType: TMsgDlgType): integer;
    procedure AllowEmptyString;
    procedure ModeAllChar;
    procedure ModeNumberOnly;
    procedure ModeNoSpecialChar;
    procedure ModeFileName;

    property UserInput: string read GetUserInput;
  end;



implementation
uses LCLType, u_user_askconfirmation, u_utils;

{$R *.lfm}

{ TFormUserInput }

procedure TFormUserInput.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN: SpeedButton1Click(NIL);
    VK_ESCAPE: SpeedButton2Click(NIL);
  end;
end;

procedure TFormUserInput.FormShow(Sender: TObject);
begin
  FrameEditString1.SetTheFocus;
end;

procedure TFormUserInput.PBPaint(Sender: TObject);
begin
  if FImage = NIL then exit;
  FImage.Draw(PB.Canvas, 0, 0, False);
end;

procedure TFormUserInput.FormCreate(Sender: TObject);
begin
  FrameEditString1:=TFrameEditString.Create(Self);
  FrameEditString1.Parent:=Panel1;
  FrameEditString1.Align:=alClient;
end;

procedure TFormUserInput.SpeedButton1Click(Sender: TObject);
begin
  if FrameEditString1.TextIsValid
    then ModalResult:=mrOk
    else FrameEditString1.SetTheFocus;
end;

procedure TFormUserInput.SpeedButton2Click(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

function TFormUserInput.GetUserInput: string;
begin
  Result:=FrameEditString1.Text;
end;

function TFormUserInput.Init(const mess, sok, scancel, sinput: string;
  aMsgType: TMsgDlgType): integer;
begin
  FrameEditString1.Title := mess;
  SpeedButton1.Caption := sok;
  SpeedButton2.Caption := scancel;
  FrameEditString1.Text := sinput;

  FImage := MsgDlgTypeToBGRABitmap(aMsgType, PB.Width, PB.Height);

  {$ifdef LCLGTK2}
  Hide;
  Application.ProcessMessages;
  {$endif}
  Result := ShowModal;
  FImage.Free;
  FImage := NIL;
end;

procedure TFormUserInput.AllowEmptyString;
begin
  FrameEditString1.AllowEmptyString;
end;

procedure TFormUserInput.ModeAllChar;
begin
  FrameEditString1.ModeAllChar;
end;

procedure TFormUserInput.ModeNumberOnly;
begin
  FrameEditString1.ModeNumberOnly;
end;

procedure TFormUserInput.ModeNoSpecialChar;
begin
  FrameEditString1.ModeNoSpecialChar;
end;

procedure TFormUserInput.ModeFileName;
begin
  FrameEditString1.ModeFileName;
end;

end.

