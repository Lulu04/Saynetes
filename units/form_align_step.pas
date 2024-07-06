unit form_align_step;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, LCLTranslator,
  frame_bglvirtualscreen_sequencer,
  frame_sequencer,
  u_project_manager;

type

  { TForm_MoveStep }

  TForm_MoveStep = class(TForm)
    BHelp: TSpeedButton;
    Label1: TLabel;
    Label2: TLabel;
    S1: TShape;
    S2: TShape;
    S3: TShape;
    S4: TShape;
    S5: TShape;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    BCancel: TSpeedButton;
    procedure BHelpClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure S4MouseUp(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure BCancelClick(Sender: TObject);
  private
    FReferenceStep: TSequenceStep;
    FSeq: TFrameSequencer;
  public
    procedure Init( Seq: TFrameSequencer; aReference: TSequenceStep);
  end;


implementation

uses u_resource_string, form_help;

{ TForm_MoveStep }

procedure TForm_MoveStep.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key = chr(27) then
    ModalResult := mrCancel;
end;

procedure TForm_MoveStep.BHelpClick(Sender: TObject);
begin
  _ShowHelp(HelpSequencerAlignSteps, BHelp);
end;

procedure TForm_MoveStep.FormShow(Sender: TObject);
begin
  BCancel.Caption := SCancel;
end;

procedure TForm_MoveStep.S4MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var S: TShape;
  step: TCustomSequencerStep;
  delta: single;
begin
  FSeq.Notify(FSeq.Selected, snChanged, SAlign);

  S := Sender as TShape;
  if S = S4 then
  begin
   for step in FSeq.StepList do
     if step.Selected and (step <> FReferenceStep) then
     begin
       delta := FReferenceStep.TimePos-(step.TimePos+step.Duration);
       FSeq.RawApplyTimeOffsetOnStep( step, delta );
     end;

  end
  else
  if S = S5 then
  begin
   for step in FSeq.StepList do
     if step.Selected and (step <> FReferenceStep) then
     begin
       delta := FReferenceStep.TimePos-step.TimePos;
       FSeq.RawApplyTimeOffsetOnStep( step, delta );
     end;
  end
  else
  if S = S3 then
  begin
   for step in FSeq.StepList do
     if step.Selected and (step <> FReferenceStep) then
     begin
       delta := FReferenceStep.TimePos+FReferenceStep.Duration-(step.TimePos+step.Duration);
       FSeq.RawApplyTimeOffsetOnStep( step, delta );
     end;
  end
  else
  begin
   for step in FSeq.StepList do
     if step.Selected and (step <> FReferenceStep) then
     begin
       delta := FReferenceStep.TimePos+FReferenceStep.Duration-step.TimePos;
       FSeq.RawApplyTimeOffsetOnStep( step, delta );
     end;
 end;
 FSeq.StepList.Sort;
 Project.SetModified;
 Close;
end;

procedure TForm_MoveStep.BCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TForm_MoveStep.Init(Seq: TFrameSequencer; aReference: TSequenceStep);
begin
 FReferenceStep := aReference;
 FSeq := Seq;
 Label2.Caption := FReferenceStep.Caption;
 ShowModal;
end;

initialization
  {$I form_align_step.lrs}

end.

