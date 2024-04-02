unit frame_sequencer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, StdCtrls, Buttons, LCLTranslator,
  frame_bglvirtualscreen_sequencer,
  undo_redo_manager,
  u_common;

type

  TUndoRedoItem = record
    Name: string;
    Data: TStepDataList;
    Notification: TSequencerNotification;
  end;
  function CreateUndoRedoItem( const aName, aData: string; aNotification: TSequencerNotification ): TUndoRedoItem;
type
  TUndoRedoManager = class(specialize TCustomUndoRedoManager<TUndoRedoItem>);

type
  { TTopStep }

  TTopStep = class(TCustomSequencerStep)
  private
    FCmdList: TCmdList;
    procedure SetCmdList(AValue: TCmdList);
  public
    //  '\' is the separator for ID, Caption, TimePos, Top, Group and Cmds.
    function Serialize: string; override;
    procedure Deserialize( const s: string ); override;
    procedure DeserializeA( const A: TStepDataArray; var k: integer );
    //  ';' is the separator between a command and another
    //  ' ' is the separator between command parameters
    property CmdList: TCmdList read FCmdList write SetCmdList;
  end;


  { TFrameSequencer }

  TFrameSequencer = class(TFrameBGLSequencer)
    MenuItem6: TMenuItem;
    MI_SBSelectTimeInterval: TMenuItem;
    MI_StepModifyLength: TMenuItem;
    MI_StepRearrange: TMenuItem;
    MI_StepMoveInTime: TMenuItem;
    MI_SBAddComplexCmd: TMenuItem;
    MI_SBZoomAll: TMenuItem;
    MI_StepRename: TMenuItem;
    MI_StepCut: TMenuItem;
    MenuItem4: TMenuItem;
    MI_StepCopy: TMenuItem;
    MI_StepPaste: TMenuItem;
    MI_StepSelectAll: TMenuItem;
    MI_SBPaste: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem8: TMenuItem;
    MI_SBModifyTime: TMenuItem;
    MI_StepDelete: TMenuItem;
    MI_SBDeleteTime: TMenuItem;
    MI_StepGroup: TMenuItem;
    MI_StepModify: TMenuItem;
    MI_SBZoomOnSelection: TMenuItem;
    MI_StepUngroup: TMenuItem;
    PopLabel: TPopupMenu;
    PopSB: TPopupMenu;
    procedure MI_SBAddComplexCmdClick(Sender: TObject);
    procedure MI_SBModifyTimeClick(Sender: TObject);
    procedure MI_SBPasteClick(Sender: TObject);
    procedure MI_SBSelectTimeIntervalClick(Sender: TObject);
    procedure MI_SBZoomAllClick(Sender: TObject);
    procedure MI_SBZoomOnSelectionClick(Sender: TObject);
    procedure MI_StepCopyClick(Sender: TObject);
    procedure MI_StepCutClick(Sender: TObject);
    procedure MI_StepDeleteClick(Sender: TObject);
    procedure MI_SBDeleteTimeClick(Sender: TObject);
    procedure MI_StepGroupClick(Sender: TObject);
    procedure MI_StepModifyClick(Sender: TObject);
    procedure MI_StepModifyLengthClick(Sender: TObject);
    procedure MI_StepMoveInTimeClick(Sender: TObject);
    procedure MI_StepPasteClick(Sender: TObject);
    procedure MI_StepRearrangeClick(Sender: TObject);
    procedure MI_StepRenameClick(Sender: TObject);
    procedure MI_StepUngroupClick(Sender: TObject);
  private
    FClickedTimePos: single;
    FClickedY: integer;
    FClickedScreen: TPoint;
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
  private
    procedure ProcessPlayerTimeElapsed(Sender: TObject);
    procedure ProcessPlayerEnd(Sender: TObject);
  public
    procedure Notify( const aSteps: ArrayOfCustomSequencerStep; aAction: TSequencerNotification;
        const aDescription: string); override;
    constructor Create( TheOwner: TComponent ); override;
    destructor Destroy; override;
    procedure UpdateWidgetState; virtual;
    procedure ProcessKey(var Key: word; Shift: TShiftState);

    procedure Clear; override;
    procedure SaveSequences( t: TStrings );
    procedure LoadSequences( t: TStrings );

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

    property UndoRedoManager: TUndoRedoManager read FUndoRedo;
    property OnUndoRedoChange: TNotifyEvent read FOnUndoRedoChange write FOnUndoRedoChange;

    // gives the time position clicked by the user
    property ClickedTimePos: single read FClickedTimePos;
  end;

implementation
uses u_utils, u_project_manager, u_modify_time, u_move_step,
  u_change_complex_step_length, u_resource_string, u_edit_complex_action,
  u_top_player, u_userdialogs, u_edit_simple_action, lclintf, LCLType,
  VelocityCurve, BGRABitmap, BGRABitmapTypes;
var
  FWorkingStep: TTopStep;

function CreateUndoRedoItem(const aName, aData: string;
  aNotification: TSequencerNotification): TUndoRedoItem;
begin
 Result.Name:=aName;
 Result.Data:=aData;
 Result.Notification:=aNotification;
end;

{ TTopStep }

procedure TTopStep.SetCmdList(AValue: TCmdList);
begin
  if FCmdList=AValue then Exit;
  FCmdList:=AValue;
 // Duration := FCmdList.ComputeCmdListDuration;
end;

function TTopStep.Serialize: string;
begin
  Result := ID.ToString +STEPDATA_SEPARATOR+
            Caption +STEPDATA_SEPARATOR+
            TimePos.ToString +STEPDATA_SEPARATOR+
            Duration.ToString +STEPDATA_SEPARATOR+
            Top.ToString +STEPDATA_SEPARATOR+
            Group.ToString +STEPDATA_SEPARATOR+
            FCmdList;
end;

procedure TTopStep.Deserialize(const s: string);
var A: TStepDataArray;
  k: integer;
begin
 A := s.SplitToStepDataArray;
 k :=0;
 DeserializeA( A, k );
end;

procedure TTopStep.DeserializeA(const A: TStepDataArray; var k: integer);
begin
 ID := A[k].ToInteger;
 inc(k);
 Caption := A[k];
 inc(k);
 TimePos := A[k].ToSingle;
 inc(k);
 Duration := A[k].ToSingle;
 inc(k);
 Top := A[k].ToInteger;
 inc(k);
 Group := A[k].ToInteger;
 inc(k);
 CmdList := A[k];
 inc(k);
end;

  { TFrameSequencer }
// Add Step
procedure TFrameSequencer.MI_SBAddComplexCmdClick(Sender: TObject);
var s: TTopStep;
begin
  if FormEditSimpleAction.ShowModal<>mrOk then exit;

  Sel_SelectNone;
  s:=TTopStep.Create;
  s.ParentSeq:=Self;
  s.TimePos:=FClickedTimePos;
  s.Top:=FClickedY;
  s.Caption:=FormEditSimpleAction.FrameCmdAll1.ShortReadableString;
  s.CmdList:=FormEditSimpleAction.FrameCmdAll1.Cmds;
  s.Duration:=FormEditSimpleAction.FrameCmdAll1.CmdDuration;
  Add(s, TRUE);
  Redraw;

  Project.SetModified;

{  FormEditComplexAction.SetAddMode;
  FormEditComplexAction.ActionName:=SAction+' '+(StepList.Count+1).ToString;
  if FormEditComplexAction.ShowModal<>mrOk then exit;

  Sel_SelectNone;
  s:= TTopStep.Create;
  s.ParentSeq:=Self;
  s.TimePos:=FClickedTimePos;
  s.Top:= FClickedY;
  s.Caption:=FormEditComplexAction.ActionName;
  cmds:=FormEditComplexAction.CmdList;
  s.CmdList:=cmds;
  s.Duration:=cmds.ComputeCmdListDuration;
  Add(s, TRUE);
  Redraw; }
end;


procedure TFrameSequencer.MI_SBModifyTimeClick(Sender: TObject);
var before, after: TCustomSequencerStep;
  delta: Single;
  F: TForm_ModifyTime;
begin
 before := GetStepBefore( FClickedTimePos );
 after := GetStepAfter( FClickedTimePos );
 if before = after then exit;
 if after=NIL then exit;
 if before=NIL
   then delta := after.TimePos
   else delta := after.TimePos - before.TimePos;

 F := TForm_ModifyTime.Create(NIL);
 F.FSE.Value := delta;
 if before=NIL then begin
  F.RB1.Checked:=TRUE;
  F.RB2.Enabled:=FALSE;
 end;

 F.EnsureVisiblePosition( FClickedScreen );

 if F.ShowModal = mrOk then begin
   delta := F.FSE.Value - delta;
   if delta<>0 then begin
     if F.RB1.Checked then ShiftStepTimePositionFrom( after, delta );
     if F.RB3.Checked
       then if after.CanApplyTimeOffset( delta )
              then ApplyTimeOffsetOnStep( after, delta );
     if F.RB2.Checked
       then if before.CanApplyTimeOffset( -delta )
              then ApplyTimeOffsetOnStep( before, -delta );
     ForceNoAreaSelected;
     redraw;
   end;
 end;
 F.Free;
end;

procedure TFrameSequencer.MI_SBPasteClick(Sender: TObject);
begin
 ClipBoard_PasteTo( FClickedTimePos );
end;

procedure TFrameSequencer.MI_SBSelectTimeIntervalClick(Sender: TObject);
var before, after: TCustomSequencerStep;
begin
 before := GetStepBefore( FClickedTimePos );
 after := GetStepAfter( FClickedTimePos );
 if before = after then exit;
 if after=NIL then exit;
 Sel_SelectNone;
 if before=NIL
   then ForceAreaSelected(0, after.TimePos)
   else ForceAreaSelected(before.TimePos, after.TimePos);
 Redraw;
 DoSelectionChangeEvent;
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
 if FWorkingStep<>NIL then begin
   Sel_Delete;
   FWorkingStep := NIL;
 end;
end;

procedure TFrameSequencer.MI_SBDeleteTimeClick(Sender: TObject);
begin
 DeleteTimeAt( FClickedTimePos );
 Redraw;
end;

procedure TFrameSequencer.MI_StepGroupClick(Sender: TObject);
begin
 Sel_Group;
end;

procedure TFrameSequencer.MI_StepModifyClick(Sender: TObject);
var cmds: TCmdList;
begin
 if FWorkingStep<>NIL then begin
   FormEditComplexAction.SetModifyMode( FWorkingStep.Caption, FWorkingStep.CmdList );

   if FormEditComplexAction.ShowModal=mrOk then begin;
     Notify([FWorkingStep], snChanged, SModify);
     cmds := FormEditComplexAction.CmdList;
     FWorkingStep.Caption := FormEditComplexAction.ActionName;
     FWorkingStep.CmdList := cmds;
     FWorkingStep.Duration:= cmds.ComputeCmdListDuration;
     Redraw;

     DoSelectionChangeEvent;
   end;
   FWorkingStep := NIL;
 end;
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

procedure TFrameSequencer.MI_StepMoveInTimeClick(Sender: TObject);
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
end;

procedure TFrameSequencer.ProcessUndoRedoChangeEvent(Sender: TObject);
begin
 if FOnUndoRedoChange<>NIL
   then FOnUndoRedoChange( self );
end;

procedure TFrameSequencer.DoSelectionChangeEvent;
begin
 UpdateWidgetState;
 inherited DoSelectionChangeEvent;
end;

procedure TFrameSequencer.DoTimeAreaClickEvent(Button: TMouseButton; Shift: TShiftState; TimePos: single);
begin
end;

procedure TFrameSequencer.DoEmptyAreaClickEvent(Button: TMouseButton;
    Shift: TShiftState; TimePos: single);
begin
 FClickedTimePos:=TimePos;
 FClickedY:= YLineUnderMouse;
 FClickedScreen:=Mouse.CursorPos;
 UpdateWidgetState;
 PopSB.PopUp;
 inherited DoEmptyAreaClickEvent(Button, Shift, TimePos);
end;

procedure TFrameSequencer.DoStepClickEvent(aStep: TCustomSequencerStep;
    Button: TMouseButton; Shift: TShiftState);
begin
  if Button=mbRight then begin
    FWorkingStep := aStep as TTopStep;
    UpdateWidgetState;
    PopLabel.PopUp;
  end;
 inherited DoStepClickEvent(aStep, Button, Shift);
end;

// because classes inherited from TCustomSequencerStep may have some additionnal stuff
// you have to duplicate it yourself in this callback.
function TFrameSequencer.DoDuplicateStepEvent(aStep: TCustomSequencerStep ): TCustomSequencerStep;
var o, t: TTopStep;
begin
  o := aStep as TTopStep;
  t := TTopStep.Create;
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
  step, first: TTopStep;
  timebase, tp: single;
  cap: string;
  istart, i, y: integer;
begin
 first := Sel_FirstStepSelected as TTopStep;
 if first = NIL then exit;
 istart := StepList.IndexOf( first );
 timebase := first.TimePos;
 cap := first.Caption;
 y := first.Top;

 sl := TStepList.Create;
 for i:=istart to StepList.Count-1 do begin
   step := TTopStep( StepList.Items[i] );
   if step.Selected then begin
     tp := step.TimePos - timebase;
     step.CmdList.OneStepForOneCmd( sl, tp );
   end;
 end;
 sl.Sort;

 step := TTopStep.Create;
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
  coeff, curDuration: single;
  A: TCmdArray;
begin
 for i:=0 To SelectedCount-1 do begin
    curDuration := TTopStep(Selected[i]).CmdList.ComputeCmdListDuration;
    coeff := Selected[i].Duration/curDuration;
    A := TTopStep(Selected[i]).CmdList.SplitToCmdArray;
    A.MultiplyAllDurationByCoeff( coeff );
    TTopStep(Selected[i]).CmdList := A.PackToCmdList;
 end;
  inherited DoUserChangeDurationEvent;
end;

procedure TFrameSequencer.Notify(const aSteps: ArrayOfCustomSequencerStep;
    aAction: TSequencerNotification; const aDescription: string);
var s: TStepDataList;
  sep: string;
  item: TUndoRedoItem;
  i: Integer;
begin
 // serialize aSteps in single string and push the data in UndoRedo
 sep:='';
 s := '';
 for i:=0 to High(aSteps) do begin
   s+=sep+aSteps[i].Serialize;
   sep := STEPDATA_SEPARATOR;
 end;

 sep := aDescription;
 if Length(aSteps)=1
   then sep+=' '+aSteps[0].Caption
   else sep+=' plusieurs';

 item:=CreateUndoRedoItem(sep, s, aAction );
 FUndoRedo.PushToUndo( item );
 Project.SetModified;
end;

procedure TFrameSequencer.ProcessPlayerTimeElapsed(Sender: TObject);
begin
 UpdatePlayCursorPosition( TOPPlayer.PlayingTimePosition );
end;

procedure TFrameSequencer.ProcessPlayerEnd(Sender: TObject);
begin
 Stop;
end;

constructor TFrameSequencer.Create(TheOwner: TComponent);
begin
 inherited Create(TheOwner);
 ColorBackground:=RGBToColor(30,30,30);
 ColorTimeArea:=BGRA(38,38,38);
 ColorTimeLegend:=BGRA(192,182,172);
 ColorXAxis:=BGRA(132,122,112);
 FUndoRedo:=TUndoRedoManager.Create;
 FUndoRedo.OnChange:=@ProcessUndoRedoChangeEvent;
end;

destructor TFrameSequencer.Destroy;
begin
 FreeAndNil(FUndoRedo);
 inherited Destroy;
end;

procedure TFrameSequencer.UpdateWidgetState;
begin
 MI_StepModify.Enabled := SelectedCount=1;
 MI_StepRename.Enabled := SelectedCount=1;

 MI_StepMoveInTime.Enabled := SelectedCount>1;

 case SelectedCount of
   1: if TTopStep(Sel_FirstStepSelected).CmdList.IsSingleCmd
          then MI_StepModifyLength.Enabled := FALSE
          else MI_StepModifyLength.Enabled := TRUE;
   else MI_StepModifyLength.Enabled := FALSE;
 end;

 // delete step or selection
 if SelectedCount>1
   then MI_StepDelete.Caption:=SDeleteSelected
   else MI_StepDelete.Caption:=SDeleteThisAction;

 MI_StepCut.Enabled := SelectedCount>=1;
 MI_StepCopy.Enabled := MI_StepCut.Enabled;
 MI_StepPaste.Enabled := Clipboard_HasData;
 MI_StepSelectAll.Enabled := TRUE;

 MI_StepGroup.Enabled := SelectedCount>1;
 MI_StepUngroup.Enabled := SelectedCount>1;

 MI_StepRearrange.Enabled := SelectedCount>0;

 MI_SBAddComplexCmd.Enabled := not AnAreaIsSelected;

 MI_SBPaste.Enabled := Clipboard_HasData;
// MI_SBSelectTimeInterval.Enabled := ;
 MI_SBModifyTime.Enabled := AnAreaIsSelected and (SelectedCount=0);
 MI_SBDeleteTime.Enabled := AnAreaIsSelected and (SelectedCount=0);
 MI_SBZoomAll.Enabled := TRUE;
 MI_SBZoomOnSelection.Enabled := AnAreaIsSelected;

end;

procedure TFrameSequencer.ProcessKey(var Key: word; Shift: TShiftState);
begin
 case Key of
   VK_DELETE: if SelectedCount>0 then Sel_Delete;
   VK_A: if ssCtrl in Shift then begin
           Sel_SelectAll;
           Redraw;
          end else if ssAlt in Shift then View_All;
   VK_G: if (ssCtrl in Shift) and (SelectedCount>1) then begin
     Sel_Group;
     Redraw;
   end;
   VK_U: if (ssCtrl in Shift) and (SelectedCount>1) then begin
     Sel_Ungroup;
     Redraw;
   end;
   VK_X: if (ssCtrl in Shift) and (SelectedCount>0) then ClipBoard_CutSelection;
   VK_C: if (ssCtrl in Shift) and (SelectedCount>0) then ClipBoard_CopySelection;
   VK_V: if (ssCtrl in Shift) and MouseIsOverSequencer and Clipboard_HasData then ClipBoard_PasteTo( XMouseToTime );
   VK_S: if ssCtrl in Shift
          then Project.Save
          else if ssAlt in Shift then View_ZoomOnSelectedArea;
   VK_R: if (ssCtrl in Shift) and (SelectedCount=1)
          then MI_StepRenameClick(nil);
   VK_Z: if ssCtrl in Shift then begin
           if ssShift in Shift
             then Redo
             else Undo;
   end;
   VK_F1: if MouseIsOverSequencer then begin
     FClickedTimePos:=XMouseToTime;
     FClickedY := YLineUnderMouse;
     MI_SBAddComplexCmdClick(NIL);
   end;
   VK_UP: if Sel_CanVerticalShift( -StepHeight ) then begin
     Sel_VerticalShift( -StepHeight );
     Redraw;
   end;
   VK_DOWN: if Sel_CanVerticalShift( StepHeight ) then begin
     Sel_VerticalShift( StepHeight );
     Redraw;
   end;
 end;
end;

procedure TFrameSequencer.Clear;
begin
 inherited;
 if not(csDestroying in ComponentState)
   then FUndoRedo.Clear;
 ClipBoard_Clear;
end;

procedure TFrameSequencer.SaveSequences(t: TStrings);
begin
 t.Add('[SEQUENCER]');
 SequencerToTStrings( Self, t );
end;

procedure TFrameSequencer.LoadSequences(t: TStrings);
var k: integer;
begin
 k := t.IndexOf('[SEQUENCER]');
 if k<>-1 then TStringsToSequencer( t, k+1, Self );
 NeedStepsWidthUpdate;
 View_All;
end;

function TFrameSequencer.ToCmdListOfSingleCmd: TCmdList;
begin
 Result := StepList.ToCmdListOfSingleCmd;
end;

function TFrameSequencer.SaveToSequencerInfoList: TSequencerInfoList;
begin
 Result := ID.ToString + SEQUENCERINFO_SEPARATOR + // current ID value
           GroupValue.ToString;                    // current groupID value
 StepList.ToStepDataList( Result );
end;

procedure TFrameSequencer.LoadFromSequencerInfoList(const s: TSequencerInfoList);
var A: TSequencerInfoArray;
    step: TTopStep;
    i: Integer;
begin
 Clear;
 A := s.SplitToSequencerInfoArray;
 if Length( A )=0 then exit;
 ID := A[0].ToInteger;
 GroupValue := A[1].ToInteger;
 for i:=2 to High(A) do begin
   step := TTopStep.Create;
   step.Deserialize( A[i] );
   RawAdd( step, FALSE );
 end;
 NeedStepsWidthUpdate;
 View_All;
end;

procedure TFrameSequencer.Play;
begin
 if StepList.Count=0 then exit;
 TOPPlayer.Stop;
 TOPPlayer.OnTimeElapsed := @ProcessPlayerTimeElapsed;
 TOPPlayer.OnEnd := @ProcessPlayerEnd;
 TOPPlayer.PlaySequencerInfoList( Self.SaveToSequencerInfoList );
 PlayCursorVisible( TRUE );
end;

procedure TFrameSequencer.Stop;
begin
 TOPPlayer.OnTimeElapsed:=NIL;
 TOPPlayer.OnEnd:=NIL;
 TOPPlayer.Stop;
 PlayCursorVisible( FALSE );
end;

procedure TFrameSequencer.DoUnDoRedo(ItsUndo: boolean);
var ItemFromUndoRedo, ItemForUndoRedo: TUndoRedoItem;
  step, step1: TTopStep;
  A: TStepDataArray;
  k, id_: Integer;
  sep: string;
begin
 if ItsUndo then begin
   if not FUndoRedo.UndoAvailable then exit;
   ItemFromUndoRedo := FUndoRedo.Undo;
 end else begin
   if not FUndoRedo.RedoAvailable then exit;
   ItemFromUndoRedo := FUndoRedo.Redo;
 end;

 if ItemFromUndoRedo.Notification in [snAdded, snDeleted]
   then ItemForUndoRedo.Data := ItemFromUndoRedo.Data
   else ItemForUndoRedo.Data := '';
 ItemForUndoRedo.Name := ItemFromUndoRedo.Name;
 ItemForUndoRedo.Notification := ItemFromUndoRedo.Notification;

 A := ItemFromUndoRedo.Data.SplitToStepDataArray;
 k := 0;
 sep:='';
 while k<High(A) do begin
   step := TTopStep.Create;
   step.DeserializeA( A, k );
   step.Selected:=FALSE;
  case ItemFromUndoRedo.Notification of
    snAdded: begin
      if ItsUndo then begin
        RawDeleteStepByID( step.ID );
        step.Free;
      end else begin
        RawAdd( step, FALSE );
      end;
    end;
    snDeleted: begin
      if ItsUndo then begin
        id_ := step.ID;
        RawAdd( step, FALSE );
        step.ID := id_;
      end else begin
        RawDeleteStepByID( step.ID );
        step.Free;
      end;
    end;
    snChanged: begin
      step1 := StepList.GetItemByID( step.ID ) as TTopStep;
      ItemForUndoRedo.Data := ItemForUndoRedo.Data + sep + step1.Serialize;
      sep:=STEPDATA_SEPARATOR;

      RawReplaceStepByID( step );
    end;
  end;//case
 end;//while

 if ItsUndo
   then FUndoRedo.PushToRedo( ItemForUndoRedo )
   else FUndoRedo.PushToUndo( ItemForUndoRedo );
end;

procedure TFrameSequencer.Undo;
begin
 DoUnDoRedo( TRUE );
 StepList.Sort;
 Redraw;
end;

procedure TFrameSequencer.Redo;
begin
 DoUnDoRedo( FALSE );
 StepList.Sort;
 Redraw;
end;



initialization
  {$I frame_sequencer.lrs}

end.

