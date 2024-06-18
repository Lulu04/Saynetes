unit u_change_complex_step_length;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Spin, Buttons, LCLTranslator,
  frame_bglvirtualscreen_sequencer,
  frame_sequencer, u_common;

type

  { TForm_ChangeStepLength }

  TForm_ChangeStepLength = class(TForm)
    BOk: TSpeedButton;
    FSE1: TFloatSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Shape1: TShape;
    procedure BOkClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    FReferenceStep: TSequenceStep;
    FSeq: TFrameSequencer;
  public
    procedure Init( Seq: TFrameSequencer; aReference: TSequenceStep);
  end;


implementation
uses u_project_manager, u_helper, u_resource_string, LCLType;

{ TForm_ChangeStepLength }

procedure TForm_ChangeStepLength.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then ModalResult := mrCancel;
end;

procedure TForm_ChangeStepLength.FormShow(Sender: TObject);
begin
  Label5.Caption := SSeconds_;
  BOk.Caption := SOk;
end;

procedure TForm_ChangeStepLength.BOkClick(Sender: TObject);
var coef: Double;
  A: TCmdArray;
begin
 if FReferenceStep.Duration <> FSE1.Value then begin
   FSeq.Notify(FSeq.Selected, snChanged, SModifyDuration);

   coef := FSE1.Value / FReferenceStep.Duration;
   A := FReferenceStep.CmdList.SplitToCmdArray;
   A.MultiplyAllDurationByCoeff(coef);

   FReferenceStep.CmdList := A.PackToCmdList;
   FReferenceStep.Duration := FSE1.Value;
   Project.SetModified;
   FSeq.Redraw;
   ModalResult := mrOk;
 end;
end;

procedure TForm_ChangeStepLength.Init(Seq: TFrameSequencer; aReference: TSequenceStep );
begin
  FReferenceStep:=aReference;
  FSeq := Seq;
  Label1.Caption:=FReferenceStep.Caption;
  Label4.Caption:=FormatFloat('0.000', FReferenceStep.Duration );
  FSE1.Value:=FReferenceStep.Duration;
  ShowModal;
end;

initialization
  {$I u_change_complex_step_length.lrs}

end.

