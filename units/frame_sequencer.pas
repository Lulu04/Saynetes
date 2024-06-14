unit frame_sequencer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, StdCtrls, Buttons, LCLTranslator,
  BGRABitmap, BGRABitmapTypes, BGRAOpenGL,
  frame_bglvirtualscreen_sequencer,
  undo_redo_manager, BGLVirtualScreen,
  u_common, u_audioutils;

type

  TUndoRedoItem = record
    Name: string;
    Data: TStepDataList;
    Notification: TSequencerNotification;
  end;
  function CreateUndoRedoItem( const aName, aData: string; aNotification: TSequencerNotification ): TUndoRedoItem;
type

  { TUndoRedoManager }

  TUndoRedoManager = class(specialize TCustomUndoRedoManager<TUndoRedoItem>)
  protected
    procedure DoUndoRedo(ItsUndo: boolean); override;
    // override this method to destroy an item removed because the stack is full
    procedure DestroyItem(constref aItem: TUndoRedoItem); override;
  end;

type
  { TSequenceStep }

  TSequenceStep = class(TCustomSequencerStep)
  private
    FCmdList: TCmdList;
    FErrorMessage: string;
    FHaveError: boolean;
    procedure SetCmdList(AValue: TCmdList);
    procedure DrawErrorSymbol(aParentFrame: TFrameBGLSequencer);
  private
    FAudioCurve: PAudioFileLevel;
  public
    procedure Redraw(aParentFrame: TFrameBGLSequencer; aBGLContext: TBGLContext); override;
    //  '\' is the separator for ID, Caption, TimePos, Top, Group and Cmds.
    function Serialize: string; override;
    procedure Deserialize( const s: string ); override;
    procedure DeserializeA( const A: TStepDataArray; var k: integer );
    //  ';' is the separator between a command and another
    //  ' ' is the separator between command parameters
    property CmdList: TCmdList read FCmdList write SetCmdList;
  public
    // check the cmd list and
    procedure CheckCmdError;
    property HaveError: boolean read FHaveError write FHaveError;
    property ErrorMessage: string read FErrorMessage write FErrorMessage;
  end;


  { TFrameSequencer }

  TFrameSequencer = class(TFrameBGLSequencer)
    MITimeDelete: TMenuItem;
    MITimeModify: TMenuItem;
    MI_SBRearrange: TMenuItem;
    MISBAddOtherAction: TMenuItem;
    MenuItem6: TMenuItem;
    MISBAddDMXAction: TMenuItem;
    MI_StepModifyLength: TMenuItem;
    MI_StepRearrange: TMenuItem;
    MI_StepAlignSteps: TMenuItem;
    MI_SBAddAudioAction: TMenuItem;
    MI_SBZoomAll: TMenuItem;
    MI_StepRename: TMenuItem;
    MI_StepCut: TMenuItem;
    MenuItem4: TMenuItem;
    MI_StepCopy: TMenuItem;
    MI_StepPaste: TMenuItem;
    MI_StepSelectAll: TMenuItem;
    MI_SBPaste: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem8: TMenuItem;
    MI_StepDelete: TMenuItem;
    MI_StepGroup: TMenuItem;
    MI_SBZoomOnSelection: TMenuItem;
    MI_StepUngroup: TMenuItem;
    PopLabel: TPopupMenu;
    PopSB: TPopupMenu;
    PopTime: TPopupMenu;
    Separator1: TMenuItem;
    procedure MITimeDeleteClick(Sender: TObject);
    procedure MITimeModifyClick(Sender: TObject);
    procedure MISBAddOtherActionClick(Sender: TObject);
    procedure MISBAddDMXActionClick(Sender: TObject);
    procedure MI_SBAddAudioActionClick(Sender: TObject);
    procedure MI_SBPasteClick(Sender: TObject);
    procedure MI_SBRearrangeClick(Sender: TObject);
    procedure MI_SBZoomAllClick(Sender: TObject);
    procedure MI_SBZoomOnSelectionClick(Sender: TObject);
    procedure MI_StepCopyClick(Sender: TObject);
    procedure MI_StepCutClick(Sender: TObject);
    procedure MI_StepDeleteClick(Sender: TObject);
    procedure MI_StepGroupClick(Sender: TObject);
    procedure MI_StepModifyLengthClick(Sender: TObject);
    procedure MI_StepAlignStepsClick(Sender: TObject);
    procedure MI_StepPasteClick(Sender: TObject);
    procedure MI_StepRearrangeClick(Sender: TObject);
    procedure MI_StepRenameClick(Sender: TObject);
    procedure MI_StepUngroupClick(Sender: TObject);
  private
    FClickedTimePos: single;
    FClickedY: integer;
    FClickedScreen: TPoint;
    procedure DoSelectTimeInterval(aTimePos: single);
  private
    FUndoRedo: TUndoRedoManager;
    FOnUndoRedoChange: TNotifyEvent;
    procedure ProcessUndoRedoChangeEvent( Sender: TObject );
  protected
    procedure DoSelectionChangeEvent; override;
    procedure DoTimeAreaClickEvent( {%H-}Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}TimePos: single ); override;
    procedure DoEmptyAreaClickEvent( Button: TMouseButton; Shift: TShiftState; TimePos: single ); override;
    procedure DoStepClickEvent( aStep: TCustomSequencerStep; Button: TMouseButton; Shift: TShiftState ); override;
    function DoDuplicateStepEvent( aStep: TCustomSequencerStep ): TCustomSequencerStep; override;
    function DoMergeStepEvent: TCustomSequencerStep; override;
    procedure DoMoveStepEvent; override;
    procedure DoUserChangeDurationEvent; override;
    function DoNeedErrorSymbolCallback: TBGRABitmap; override;
  private
    procedure ProcessPlayerTimeElapsed(Sender: TObject);
    procedure ProcessPlayerEnd(Sender: TObject);
  private
   // procedure ProcessOnAddCmdFromViewProjector(Sender: TObject);
  public
    procedure Notify( const aSteps: ArrayOfCustomSequencerStep; aAction: TSequencerNotification;
        const aDescription: string); override;
    constructor Create( TheOwner: TComponent ); override;
    destructor Destroy; override;
    procedure UpdateWidgetState; virtual;
    procedure ProcessKey(var Key: word; Shift: TShiftState);

    procedure Clear; override;
//    procedure SaveSequences( t: TStrings );
//    procedure LoadSequences( t: TStrings );

    // converti chaque étape en étape contenant une seule commande
    // les étapes contenant plusieurs commandes ou des pauses sont converties en plusieurs étapes
    // d'une seule commande et distribuées au bon endroit sur la chronologie.
    // les effets sont également calculés et transformés en étapes simple.
    function ToCmdListOfSingleCmd: TCmdList;

    function SaveToSequencerInfoList: TSequencerInfoList;
    procedure LoadFromSequencerInfoList( const s: TSequencerInfoList );

    // play the sequence from the begin of the view
    procedure Play;
    // stop playing the sequence
    procedure Stop;

    procedure DoUnDoRedo( ItsUndo: boolean );
    procedure Undo;
    procedure Redo;

    procedure TranslateStrings;

    property UndoRedoManager: TUndoRedoManager read FUndoRedo;
    property OnUndoRedoChange: TNotifyEvent read FOnUndoRedoChange write FOnUndoRedoChange;

    // gives the time position clicked by the user
    property ClickedTimePos: single read FClickedTimePos;
  end;

implementation
uses u_modify_time, u_move_step, u_change_complex_step_length,
  u_resource_string, u_sequence_player, u_userdialogs, u_edit_otheraction,
  u_helper, u_add_action_audio, lclintf, LCLType, VelocityCurve,
  u_edit_sequence, u_add_action_dmx, u_logfile, u_apputils, u_audio_manager,
  utilitaire_bgrabitmap, PropertyUtils;
var
  FWorkingStep: TSequenceStep;

function CreateUndoRedoItem(const aName, aData: string;
  aNotification: TSequencerNotification): TUndoRedoItem;
begin
 Result.Name := aName;
 Result.Data := aData;
 Result.Notification := aNotification;
end;

{ TUndoRedoManager }

procedure TUndoRedoManager.DoUndoRedo(ItsUndo: boolean);
begin

end;

procedure TUndoRedoManager.DestroyItem(constref aItem: TUndoRedoItem);
begin

end;

{ TSequenceStep }

procedure TSequenceStep.SetCmdList(AValue: TCmdList);
var audioID: integer;
begin
  if FCmdList = AValue then Exit;
  FCmdList := AValue;

  // check if cmd is single with Audio-Play
  if FCmdList.IsAudioPlay(audioID) then FAudioCurve := SoundManager.GetAudioCurve(audioID)
    else FAudioCurve := NIL;
end;

procedure TSequenceStep.DrawErrorSymbol(aParentFrame: TFrameBGLSequencer);
var xx: Integer;
begin
  xx := aParentFrame.TimePosToAbscissa(TimePos);
  if (aParentFrame.TextureErrorSymbol <> NIL) then
    aParentFrame.TextureErrorSymbol.Draw(xx, Top);
end;

procedure TSequenceStep.Redraw(aParentFrame: TFrameBGLSequencer; aBGLContext: TBGLContext);
var xBegin, xEnd: Integer;
  timeBegin, yRef, halfHeight, timeDelta: Single;
  peak: TSamplePeak;
begin
  // first, render audio curve
  if FAudioCurve <> NIL then begin
    xBegin := aParentFrame.TimePosToAbscissa(TimePos);
    xEnd := aParentFrame.TimePosToAbscissa(TimePos + FAudioCurve^.Duration);
    if not((xBegin > aBGLContext.Canvas.Width) or (xEnd < 0)) then begin
      timeBegin := 0;
      timeDelta := aParentFrame.AbscissaToTimePos(1);
      halfHeight := aParentFrame.StepHeight / 2;
      yRef := Top + halfHeight;
      repeat
        if (xBegin >= 0) and (xBegin+1 <= aBGLContext.Canvas.Width) then begin
          peak := FAudioCurve^.GetPeakBetween(timeBegin, timeBegin+timeDelta);
          aBGLContext.Canvas.Line(xBegin, yRef-(peak.Positive/32767*halfHeight),
                                  xBegin+1, yRef-(peak.Negative/32768*halfHeight), BGRA(255,77,65));
        end;
        timeBegin := timeBegin + timeDelta;
        inc(xBegin);
      until xBegin > xEnd;
    end;
  end;

  inherited Redraw(aParentFrame, aBGLContext);
  if HaveError then DrawErrorSymbol(aParentFrame);
end;

function TSequenceStep.Serialize: string;
begin
  Result := ID.ToString +STEPDATA_SEPARATOR+
            Caption +STEPDATA_SEPARATOR+
            TimePos.ToString +STEPDATA_SEPARATOR+
            Duration.ToString +STEPDATA_SEPARATOR+
            Top.ToString +STEPDATA_SEPARATOR+
            Group.ToString +STEPDATA_SEPARATOR+
            FCmdList;
end;

procedure TSequenceStep.Deserialize(const s: string);
var A: TStepDataArray;
  k: integer;
begin
  A := s.SplitToStepDataArray;
  k := 0;
  DeserializeA(A, k);
end;

procedure TSequenceStep.DeserializeA(const A: TStepDataArray; var k: integer);
begin
  ID := A[k].ToInteger;
  inc(k);
  Caption := A[k];
  inc(k);
  TimePos := StringToSingle(A[k]);
  inc(k);
  Duration := StringToSingle(A[k]);
  inc(k);
  Top := A[k].ToInteger;
  inc(k);
  Group := A[k].ToInteger;
  inc(k);
  CmdList := A[k];
  inc(k);
end;

procedure TSequenceStep.CheckCmdError;
begin
  FHaveError := False;
  FErrorMessage := '';
  if FCmdList = '' then exit;
  FHaveError := FCmdList.HaveError(FErrorMessage);
end;

  { TFrameSequencer }
// Add Audio action
procedure TFrameSequencer.MI_SBAddAudioActionClick(Sender: TObject);
var s: TSequenceStep;
begin
  if FormAudioAction.ShowModal <> mrOk then exit;

  Sel_SelectNone;
  s := TSequenceStep.Create;
  s.ParentSeq := Self;
  s.TimePos := FClickedTimePos;
  s.Top := (FClickedY div StepFontHeight) * StepFontHeight;
  s.Caption := FormAudioAction.ShortReadableString;
  s.CmdList := FormAudioAction.Cmds;
  s.Duration := FormAudioAction.CmdDuration;
  Add(s, TRUE);
  Redraw;
end;

procedure TFrameSequencer.MISBAddOtherActionClick(Sender: TObject);
var s: TSequenceStep;
begin
  FormOtherAction.Fill;
  if FormOtherAction.ShowModal <> mrOk then exit;

  Sel_SelectNone;
  s := TSequenceStep.Create;
  s.ParentSeq := Self;
  s.TimePos := FClickedTimePos;
  s.Top := FClickedY;
  s.Caption := FormOtherAction.ShortReadableString;
  s.CmdList := FormOtherAction.Cmds;
  s.Duration := FormOtherAction.CmdDuration;
  Add(s, TRUE);
  Redraw;
end;

procedure TFrameSequencer.MITimeModifyClick(Sender: TObject);
var before, after: TCustomSequencerStep;
  delta: Single;
  F: TForm_ModifyTime;
begin
 before := GetStepBefore(FClickedTimePos);
 after := GetStepAfter(FClickedTimePos);
 if before = after then exit;
 if after = NIL then exit;
 if before = NIL
   then delta := after.TimePos
   else delta := after.TimePos - before.TimePos;

 F := TForm_ModifyTime.Create(NIL);
 F.FSE.Value := delta;
 if before = NIL then begin
  F.RB1.Checked := TRUE;
  F.RB2.Enabled := FALSE;
 end;

 F.EnsureVisiblePosition(FClickedScreen);

 if F.ShowModal = mrOk then begin
   delta := F.FSE.Value - delta;
   if delta <> 0 then begin
     if F.RB1.Checked then ShiftStepTimePositionFrom(after, delta);
     if F.RB3.Checked
       then if after.CanApplyTimeOffset(delta)
              then ApplyTimeOffsetOnStep(after, delta);
     if F.RB2.Checked then
       if before.CanApplyTimeOffset(-delta) then
         ApplyTimeOffsetOnStep(before, -delta);
     ForceNoAreaSelected;
     redraw;
   end;
 end;
 F.Free;
end;

procedure TFrameSequencer.MITimeDeleteClick(Sender: TObject);
begin
  DeleteTimeAt(FClickedTimePos);
  ForceNoAreaSelected;
  Redraw;
end;

procedure TFrameSequencer.MISBAddDMXActionClick(Sender: TObject);
var s: TSequenceStep;
  p: TPoint;
  w, h: integer;
begin
  FormAddDMXAction := TFormAddDMXAction.Create(NIL);
  try
    // center the window and a little bit smaller
    p := FormSequenceEdition.ClientToScreen(FormSequenceEdition.ClientRect.CenterPoint);
    w := Round(FormSequenceEdition.ClientWidth*0.6);
    h := Round(FormSequenceEdition.ClientHeight*0.8);
    FormAddDMXAction.SetBounds(p.x-w div 2, p.y-h div 2, w, h);

    if FormAddDMXAction.ShowModal = mrOk then
    begin
      Sel_SelectNone;
      s := TSequenceStep.Create;
      s.ParentSeq := Self;
      s.TimePos := FClickedTimePos;
      s.Top := FClickedY;
      s.Caption := FormAddDMXAction.FrameViewProjector1.ShortReadableString;
      s.CmdList := FormAddDMXAction.FrameViewProjector1.Cmds;
      s.Duration := FormAddDMXAction.FrameViewProjector1.CmdDuration;
      Add(s, TRUE);
      ForceReconstructOpenGLObjects;
      //Redraw;
    end;
  finally
    FormAddDMXAction.Free;
    FormAddDMXAction := NIL;
  end;
end;

procedure TFrameSequencer.MI_SBPasteClick(Sender: TObject);
begin
 ClipBoard_PasteTo( FClickedTimePos );
end;

procedure TFrameSequencer.MI_SBRearrangeClick(Sender: TObject);
begin
  RecomputeVerticalStepsPosition
end;

procedure TFrameSequencer.MI_SBZoomAllClick(Sender: TObject);
begin
  View_All;
end;

procedure TFrameSequencer.MI_SBZoomOnSelectionClick(Sender: TObject);
begin
  View_ZoomOnSelectedArea;
end;

procedure TFrameSequencer.MI_StepCopyClick(Sender: TObject);
begin
  ClipBoard_CopySelection;
end;

procedure TFrameSequencer.MI_StepCutClick(Sender: TObject);
begin
 ClipBoard_CutSelection;
end;

// delete step
procedure TFrameSequencer.MI_StepDeleteClick(Sender: TObject);
begin
 if FWorkingStep <> NIL then
 begin
   Sel_Delete;
   FWorkingStep := NIL;
 end;
end;

procedure TFrameSequencer.MI_StepGroupClick(Sender: TObject);
begin
 Sel_Group;
 Redraw;
end;

procedure TFrameSequencer.MI_StepModifyLengthClick(Sender: TObject);
var F: TForm_ChangeStepLength;
begin
 F := TForm_ChangeStepLength.Create(NIL);
 F.Init( Self, FWorkingStep );
 F.Free;
 FWorkingStep := NIL;
 DoSelectionChangeEvent;
end;

procedure TFrameSequencer.MI_StepAlignStepsClick(Sender: TObject);
var F: TForm_MoveStep;
begin
 F := TForm_MoveStep.Create(NIL);
 F.Init( Self, FWorkingStep );
 F.Free;
 FWorkingStep := NIL;
end;

procedure TFrameSequencer.MI_StepPasteClick(Sender: TObject);
begin
 ClipBoard_PasteTo( Sel_FirstStepSelected.TimePos );
end;

procedure TFrameSequencer.MI_StepRearrangeClick(Sender: TObject);
begin
  Sel_RecomputeVerticalStepsPosition;
end;

procedure TFrameSequencer.MI_StepRenameClick(Sender: TObject);
var N: string;
begin
 N := FWorkingStep.Caption;
 if UserInputNoSpecialChar(SNewName, SOk, SCancel, N, mtCustom, FALSE)=mrOk then begin
   Notify([FWorkingStep], snChanged, SRename);
   FWorkingStep.Caption := N;
 end;
end;

procedure TFrameSequencer.MI_StepUngroupClick(Sender: TObject);
begin
 Sel_Ungroup;
 Redraw;
end;

procedure TFrameSequencer.DoSelectTimeInterval(aTimePos: single);
var before, after: TCustomSequencerStep;
begin
  before := GetStepBefore(aTimePos);
  after := GetStepAfter(aTimePos);
  if before = after then exit;
  if after = NIL then exit;
  Sel_SelectNone;
  if before = NIL then ForceAreaSelected(0, after.TimePos)
    else ForceAreaSelected(before.TimePos, after.TimePos);
  Redraw;
  DoSelectionChangeEvent;
end;

procedure TFrameSequencer.ProcessUndoRedoChangeEvent(Sender: TObject);
begin
  if FOnUndoRedoChange <> NIL then
    FOnUndoRedoChange(self);
end;

procedure TFrameSequencer.DoSelectionChangeEvent;
begin
  UpdateWidgetState;
  inherited DoSelectionChangeEvent;
end;

procedure TFrameSequencer.DoTimeAreaClickEvent(Button: TMouseButton; Shift: TShiftState; TimePos: single);
begin
  DoSelectTimeInterval(TimePos);

  if Button = mbRight then begin
    FClickedTimePos := TimePos;
    FClickedScreen := Mouse.CursorPos;
    MITimeModify.Enabled := AnAreaIsSelected;
    MITimeDelete.Enabled := MITimeModify.Enabled;
    PopTime.PopUp;
  end;
end;

procedure TFrameSequencer.DoEmptyAreaClickEvent(Button: TMouseButton; Shift: TShiftState; TimePos: single);
begin
  if Button = mbRight then begin
    FClickedTimePos := TimePos;
    FClickedY := YLineUnderMouse;
    FClickedScreen := Mouse.CursorPos;
    UpdateWidgetState;
    PopSB.PopUp;
  end;

  inherited DoEmptyAreaClickEvent(Button, Shift, TimePos);
end;

procedure TFrameSequencer.DoStepClickEvent(aStep: TCustomSequencerStep; Button: TMouseButton; Shift: TShiftState);
begin
  if Button = mbRight then begin
    FWorkingStep := aStep as TSequenceStep;
    UpdateWidgetState;
    PopLabel.PopUp;
  end;

  inherited DoStepClickEvent(aStep, Button, Shift);
end;

// because classes inherited from TCustomSequencerStep may have some additionnal stuff
// you have to duplicate it yourself in this callback.
function TFrameSequencer.DoDuplicateStepEvent(aStep: TCustomSequencerStep ): TCustomSequencerStep;
var o, t: TSequenceStep;
begin
  o := aStep as TSequenceStep;
  t := TSequenceStep.Create;
  t.Caption := o.Caption;
  t.Top := o.Top;
  t.TimePos := o.TimePos;
  t.CmdList := o.CmdList;
  t.Duration:= o.Duration;
  Result := TCustomSequencerStep( t );
  // ici, pas d'appel à inherited
end;

function TFrameSequencer.DoMergeStepEvent: TCustomSequencerStep;
var sl: TStepList;
  step, first: TSequenceStep;
  timebase, tp: single;
  cap: string;
  istart, i, y: integer;
begin
 first := Sel_FirstStepSelected as TSequenceStep;
 if first = NIL then exit;
 istart := StepList.IndexOf( first );
 timebase := first.TimePos;
 cap := first.Caption;
 y := first.Top;

 sl := TStepList.Create;
 for i:=istart to StepList.Count-1 do begin
   step := TSequenceStep( StepList.Items[i] );
   if step.Selected then begin
     tp := step.TimePos - timebase;
     step.CmdList.OneStepForOneCmd( sl, tp );
   end;
 end;
 sl.Sort;

 step := TSequenceStep.Create;
 step.CmdList := sl.PackToCmdList;
 step.Duration:= step.CmdList.ComputeCmdListDuration;
 step.TimePos := timebase;
 step.Top := y;
 step.Caption := cap;
 Result := step as TCustomSequencerStep;

 sl.FreeSteps;
 sl.Free;
end;

procedure TFrameSequencer.DoMoveStepEvent;
begin
 inherited DoMoveStepEvent;
end;

procedure TFrameSequencer.DoUserChangeDurationEvent;
var i: integer;
  A: TCmdArray;
begin
 for i:=0 To SelectedCount-1 do
 begin
    A := TSequenceStep(Selected[i]).CmdList.SplitToCmdArray;
    A.SetCmdDuration(Selected[i].Duration);
    TSequenceStep(Selected[i]).CmdList := A.PackToCmdList;
 end;
  inherited DoUserChangeDurationEvent;
end;

function TFrameSequencer.DoNeedErrorSymbolCallback: TBGRABitmap;
begin
  Result := SVGFileToBGRABitmap(GetAppIconImagesFolder+'SequenceErrorSymbol.svg', -1, StepFont.FullHeight);
end;

procedure TFrameSequencer.Notify(const aSteps: ArrayOfCustomSequencerStep;
    aAction: TSequencerNotification; const aDescription: string);
var s: TStepDataList;
  sep: string;
  item: TUndoRedoItem;
  i: Integer;
begin
 // serialize aSteps in single string and push the data in UndoRedo
 sep := '';
 s := '';
 for i:=0 to High(aSteps) do begin
   s += sep+aSteps[i].Serialize;
   sep := STEPDATA_SEPARATOR;
 end;

 sep := aDescription;
 if Length(aSteps) = 1 then
   sep += ' '+aSteps[0].Caption
 else
   sep += ' '+SSeveral;

 item := CreateUndoRedoItem(sep, s, aAction );
 FUndoRedo.PushToUndo( item );
end;

procedure TFrameSequencer.ProcessPlayerTimeElapsed(Sender: TObject);
begin
 UpdatePlayCursorPosition( SeqPlayer.PreviewTimePosition );
end;

procedure TFrameSequencer.ProcessPlayerEnd(Sender: TObject);
begin
 Stop;
end;

constructor TFrameSequencer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  SetOptions([bglsAlternateColorForVStepPosition], TRUE);
  SetOptions([bglsForceVStepPosition], TRUE);

  ColorBackground := RGBToColor(51,51,51);
  ColorBackground1 := BGRA(40,40,40);
  ColorTimeArea := BGRA(5,5,5);
  ColorTimeLegend := BGRA(192,182,172);
  ColorXAxis := BGRA(132,122,112);
  FUndoRedo := TUndoRedoManager.Create;
  FUndoRedo.OnChange := @ProcessUndoRedoChangeEvent;
end;

destructor TFrameSequencer.Destroy;
begin
  FreeAndNil(FUndoRedo);
  inherited Destroy;
end;

procedure TFrameSequencer.UpdateWidgetState;
begin
  MI_StepRename.Enabled := SelectedCount = 1;

  MI_StepAlignSteps.Enabled := SelectedCount > 1;

  case SelectedCount of
    1: if TSequenceStep(Sel_FirstStepSelected).CmdList.IsSingleCmd then
         MI_StepModifyLength.Enabled := FALSE
       else
         MI_StepModifyLength.Enabled := TRUE;

    else MI_StepModifyLength.Enabled := FALSE;
  end;

 // MI_StepModifyLength.Enabled := (SelectedCount = 1) and TSequenceStep(Sel_FirstStepSelected).CmdList.IsSingleCmd;

  // delete step or selection
  if SelectedCount > 1 then MI_StepDelete.Caption := SDeleteSelected
    else MI_StepDelete.Caption := SDeleteThisAction;

  MI_StepCut.Enabled := SelectedCount >= 1;
  MI_StepCopy.Enabled := MI_StepCut.Enabled;
  MI_StepPaste.Enabled := Clipboard_HasData;
  MI_StepSelectAll.Enabled := TRUE;

  MI_StepGroup.Enabled := SelectedCount > 1;
  MI_StepUngroup.Enabled := SelectedCount > 1;

  MI_StepRearrange.Enabled := SelectedCount > 0;

  MI_SBPaste.Enabled := Clipboard_HasData;
  MI_SBZoomAll.Enabled := TRUE;
  MI_SBZoomOnSelection.Enabled := AnAreaIsSelected;
end;

procedure TFrameSequencer.ProcessKey(var Key: word; Shift: TShiftState);
begin
  case Key of
    VK_DELETE: if SelectedCount > 0 then Sel_Delete;
    VK_A: if ssCtrl in Shift then
          begin
            Sel_SelectAll;
            Redraw;
          end
          else if ssAlt in Shift then View_All;
    VK_G: if (ssCtrl in Shift) and (SelectedCount>1) then
          begin
            Sel_Group;
            Redraw;
          end;
    VK_U: if (ssCtrl in Shift) and (SelectedCount>1) then
          begin
            Sel_Ungroup;
            Redraw;
          end;
    VK_X: if (ssCtrl in Shift) and (SelectedCount>0) then ClipBoard_CutSelection;
    VK_C: if (ssCtrl in Shift) and (SelectedCount>0) then ClipBoard_CopySelection;
    VK_V: if (ssCtrl in Shift) and MouseIsOverSequencer and Clipboard_HasData then ClipBoard_PasteTo(XMouseToTime);
    VK_S: if ssAlt in Shift then View_ZoomOnSelectedArea;
    VK_R: if (ssCtrl in Shift) and (SelectedCount=1)
           then MI_StepRenameClick(nil);
    VK_Z: if ssCtrl in Shift then begin
            if ssShift in Shift
              then Redo
              else Undo;
    end;
    VK_UP: if Sel_CanVerticalShift(-StepHeight) then begin
      Sel_VerticalShift(-StepHeight);
      Redraw;
    end;
    VK_DOWN: if Sel_CanVerticalShift(StepHeight) then begin
      Sel_VerticalShift(StepHeight);
      Redraw;
    end;
  end;
end;

procedure TFrameSequencer.Clear;
begin
  inherited;
  if not(csDestroying in ComponentState) then
    FUndoRedo.Clear;
  ClipBoard_Clear;
end;

{procedure TFrameSequencer.SaveSequences(t: TStrings);
begin
  t.Add('[SEQUENCER]');
  SequencerToTStrings(Self, t);
end;  }

{procedure TFrameSequencer.LoadSequences(t: TStrings);
var k: integer;
begin
  k := t.IndexOf('[SEQUENCER]');
  if k <> -1 then
    TStringsToSequencer(t, k+1, Self);
  NeedStepsWidthUpdate;
  View_All;
end; }

function TFrameSequencer.ToCmdListOfSingleCmd: TCmdList;
begin
  Result := StepList.ToCmdListOfSingleCmd;
end;

function TFrameSequencer.SaveToSequencerInfoList: TSequencerInfoList;
var prop: TProperties;
begin
  prop.Init(SEQUENCERINFO_SEPARATOR);
  prop.Add('CurrentID', ID);
  prop.Add('GroupValue', GroupValue);
  //prop.Add('StepData',
  Result := prop.PackedProperty;

  Result := ID.ToString + SEQUENCERINFO_SEPARATOR + // current ID value
            GroupValue.ToString;                    // current groupID value
  StepList.ToStepDataList(Result);
end;

procedure TFrameSequencer.LoadFromSequencerInfoList(const s: TSequencerInfoList);
var A: TSequencerInfoArray;
    step: TSequenceStep;
    i: Integer;
begin
  Clear;
  A := s.SplitToSequencerInfoArray;
  if Length(A) = 0 then exit;
  ID := A[0].ToInteger;
  GroupValue := A[1].ToInteger;
  for i:=2 to High(A) do begin
    step := TSequenceStep.Create;
    step.Deserialize( A[i] );
    step.CheckCmdError;
    RawAdd(step, FALSE);
  end;
  NeedStepsWidthUpdate;
  NeedStepsTopUpdate;
  View_All;
end;

procedure TFrameSequencer.Play;
begin
  if StepList.Count = 0 then exit;
  SeqPlayer.StopPreview;
  SeqPlayer.OnTimeElapsed := @ProcessPlayerTimeElapsed;
  SeqPlayer.OnEndPreview := @ProcessPlayerEnd;
  SeqPlayer.PreviewSequencerInfoList(SaveToSequencerInfoList);
  PlayCursorVisible(TRUE);
end;

procedure TFrameSequencer.Stop;
begin
  SeqPlayer.OnTimeElapsed := NIL;
  SeqPlayer.OnEndPreview := NIL;
  SeqPlayer.StopPreview;
  PlayCursorVisible(FALSE);
end;

procedure TFrameSequencer.DoUnDoRedo(ItsUndo: boolean);
var itemPoped, itemToPush: TUndoRedoItem;
  step, step1: TSequenceStep;
  A: TStepDataArray;
  k, id_: Integer;
  sep: string;
begin
  if ItsUndo then begin
    if not FUndoRedo.UndoAvailable then exit;
    itemPoped := FUndoRedo.PopFromUndo;
  end else begin
    if not FUndoRedo.RedoAvailable then exit;
    itemPoped := FUndoRedo.PopFromRedo;
  end;

  if itemPoped.Notification in [snAdded, snDeleted]
    then itemToPush.Data := itemPoped.Data
    else itemToPush.Data := '';
  itemToPush.Name := itemPoped.Name;
  itemToPush.Notification := itemPoped.Notification;

  A := itemPoped.Data.SplitToStepDataArray;
  k := 0;
  sep:='';
  while k<High(A) do begin
    step := TSequenceStep.Create;
    step.DeserializeA( A, k );
    step.Selected:=FALSE;
   case itemPoped.Notification of
     snAdded: begin
       if ItsUndo then begin
         RawDeleteStepByID( step.ID );
         step.Free;
       end else begin
         RawAdd( step, FALSE, FALSE );
          step.UpdateWidth;
       end;
     end;
     snDeleted: begin
       if ItsUndo then begin
         id_ := step.ID;
         RawAdd( step, FALSE );
         step.ID := id_;
         step.UpdateWidth;
         step.CheckCmdError;
       end else begin
         RawDeleteStepByID( step.ID );
         step.Free;
       end;
     end;
     snChanged: begin
       step1 := StepList.GetItemByID( step.ID ) as TSequenceStep;
       itemToPush.Data := itemToPush.Data + sep + step1.Serialize;
       sep := STEPDATA_SEPARATOR;
       RawReplaceStepByID( step );
       step.CheckCmdError;
     end;
   end;//case
  end;//while

  if ItsUndo then
    FUndoRedo.PushToRedo(itemToPush)
  else
    FUndoRedo.PushToUndo(itemToPush);

  StepList.Sort;
  Redraw;
  DoSelectionChangeEvent;
end;

procedure TFrameSequencer.Undo;
begin
  DoUnDoRedo(TRUE);
end;

procedure TFrameSequencer.Redo;
begin
  DoUnDoRedo(FALSE);
end;

procedure TFrameSequencer.TranslateStrings;
begin
  MI_StepGroup.Caption := SGroup;
  MI_StepRename.Caption := SRename;
  MI_StepDelete.Caption := SDelete;
  MI_StepCut.Caption := SCut;
  MI_StepPaste.Caption := SPaste;
  MI_StepCopy.Caption := SCopy;
  MI_SBPaste.Caption := sPaste;
end;



initialization
  {$I frame_sequencer.lrs}

end.

