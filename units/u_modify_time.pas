unit u_modify_time;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Spin,
  StdCtrls, ExtCtrls, Buttons, LCLTranslator,
  u_notebook_util;

type

  { TForm_ModifyTime }

  TForm_ModifyTime = class(TForm)
    BOk: TSpeedButton;
    FSE: TFloatSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    RB2: TRadioButton;
    RB1: TRadioButton;
    RB3: TRadioButton;
    Shape1: TShape;
    procedure BOkClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    CheckedLabelManager: TCheckedLabelManager;
    function GetShiftNextSteps: boolean;
  public
    procedure EnsureVisiblePosition(aPt: TPoint);

    property ShiftNextSteps: boolean read GetShiftNextSteps;
  end;

implementation
uses Math, u_resource_string, LCLType, LCLIntf;

{ TForm_ModifyTime }

procedure TForm_ModifyTime.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    ModalResult := mrCancel;
end;

procedure TForm_ModifyTime.FormShow(Sender: TObject);
begin
  Label2.Caption := SSeconds_;
  BOk.Caption := SOk;
end;

procedure TForm_ModifyTime.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  RB1.Enabled := TRUE;
  RB2.Enabled := TRUE;
end;

procedure TForm_ModifyTime.FormCreate(Sender: TObject);
begin
  CheckedLabelManager := TCheckedLabelManager.Create;
  CheckedLabelManager.CaptureLabelClick(Label3);
  CheckedLabelManager.CaptureLabelClick(Label4);
  CheckedLabelManager.CaptureLabelClick(Label5);
end;

procedure TForm_ModifyTime.FormDestroy(Sender: TObject);
begin
  CheckedLabelManager.Free;
end;

procedure TForm_ModifyTime.BOkClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

function TForm_ModifyTime.GetShiftNextSteps: boolean;
begin
  Result := RB1.Checked;
end;

procedure TForm_ModifyTime.EnsureVisiblePosition(aPt: TPoint);
begin
  Left := EnsureRange(aPt.x, Monitor.WorkareaRect.Left, Monitor.WorkareaRect.Right-Width-ScaleDesignToForm(10));
  Top := EnsureRange(aPt.y, Monitor.WorkareaRect.Top, Monitor.WorkareaRect.Bottom-Height-ScaleDesignToForm(40));
end;


initialization
  {$I u_modify_time.lrs}

end.

