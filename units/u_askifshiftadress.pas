unit u_askifshiftadress;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ExtCtrls, LCLTranslator;

type

  { TFormAskIfShiftAdress }

  TFormAskIfShiftAdress = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    Shape1: TShape;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    procedure FormKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure Label2Click(Sender: TObject);
    procedure Label3Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
  private
    function GetShiftAdress: boolean;
  public
    property ShiftAdress: boolean read GetShiftAdress;
  end;

var
  FormAskIfShiftAdress: TFormAskIfShiftAdress;

implementation

uses u_resource_string, LCLType, u_mainform;

{$R *.lfm}

{ TFormAskIfShiftAdress }

procedure TFormAskIfShiftAdress.FormShow(Sender: TObject);
begin
  Caption:=SDelete;
  if FormMain.FrameViewProjector1.SelectedCount = 1 then
    Label1.Caption:=SAreYouSureToDeleteThisFixture
  else
    Label1.Caption:=SAreYouSureToDeleteTheseFixtures;

  SpeedButton1.Caption := sYes;
  SpeedButton2.Caption := sCancel;
end;

procedure TFormAskIfShiftAdress.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    ModalResult := mrCancel;
end;

procedure TFormAskIfShiftAdress.Label2Click(Sender: TObject);
begin
  RadioButton1.Checked := not RadioButton1.Checked;
end;

procedure TFormAskIfShiftAdress.Label3Click(Sender: TObject);
begin
  RadioButton2.Checked := not RadioButton2.Checked;
end;

procedure TFormAskIfShiftAdress.SpeedButton1Click(Sender: TObject);
begin
  if RadioButton1.Checked or RadioButton2.Checked then
    ModalResult := mrOk;
end;

procedure TFormAskIfShiftAdress.SpeedButton2Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

function TFormAskIfShiftAdress.GetShiftAdress: boolean;
begin
  Result := RadioButton1.Checked;
end;

end.

