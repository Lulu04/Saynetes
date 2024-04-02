unit u_dmxlib_inputrange;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  LCLTranslator, StdCtrls, Spin, Buttons, ExtCtrls, frame_editstring;

type

  { TFormInputDMXRange }

  TFormInputDMXRange = class(TForm)
    BCancel: TSpeedButton;
    BOk: TSpeedButton;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    SE1: TSpinEdit;
    SE2: TSpinEdit;
    procedure BCancelClick(Sender: TObject);
    procedure BOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    FrameEditString1: TFrameEditString;
    function GetDescription: string;
    function GetRangeBegin: integer;
    function GetRangeEnd: integer;
    procedure SetDescription(AValue: string);
    procedure SetRangeBegin(AValue: integer);
    procedure SetRangeEnd(AValue: integer);

  public
    property RangeBegin: integer read GetRangeBegin write SetRangeBegin;
    property RangeEnd: integer read GetRangeEnd write SetRangeEnd;
    property Description: string read GetDescription write SetDescription;
  end;


implementation

uses LCLType, u_resource_string, u_utils;

{$R *.lfm}

{ TFormInputDMXRange }

procedure TFormInputDMXRange.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    ModalResult := mrCancel;
end;

procedure TFormInputDMXRange.FormShow(Sender: TObject);
begin
  BOk.Caption := SOk;
  BCancel.Caption := SCancel;
end;

function TFormInputDMXRange.GetDescription: string;
begin
  Result := FrameEditString1.Text;
end;

function TFormInputDMXRange.GetRangeBegin: integer;
begin
  Result := SE1.Value;
end;

function TFormInputDMXRange.GetRangeEnd: integer;
begin
  Result := SE2.Value;
end;

procedure TFormInputDMXRange.SetDescription(AValue: string);
begin
  FrameEditString1.Text := AValue;
end;

procedure TFormInputDMXRange.SetRangeBegin(AValue: integer);
begin
  SE1.Value := AValue;
end;

procedure TFormInputDMXRange.SetRangeEnd(AValue: integer);
begin
  SE2.Value := AValue;
end;

procedure TFormInputDMXRange.BCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFormInputDMXRange.BOkClick(Sender: TObject);
begin
  if (SE1.Value <= SE2.Value) and StringIsValid(FrameEditString1.Text) then
    ModalResult := mrOk;
end;

procedure TFormInputDMXRange.FormCreate(Sender: TObject);
begin
  FrameEditString1 := TFrameEditString.Create(Self);
  FrameEditString1.Parent := Panel1;
  FrameEditString1.Align := alClient;
  FrameEditString1.Title := SDescription;
end;

end.

