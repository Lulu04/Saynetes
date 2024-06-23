unit u_edit_sequence;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  LCLTranslator, StdCtrls, Buttons, ComCtrls, Menus,
  frame_bglvirtualscreen_sequencer, frame_sequencer, frame_viewcmdlist,
  u_common, frame_editstring, u_audio_manager, u_list_dmxuniverse,
  u_notebook_util;

type

  { TFormSequenceEdition }

  TFormSequenceEdition = class(TForm)
    BKeep0Visible: TSpeedButton;
    B_Redo: TSpeedButton;
    B_ShutDown: TSpeedButton;
    B_Undo: TSpeedButton;
    BPreview: TSpeedButton;
    B_ViewAll: TSpeedButton;
    B_ViewSelection: TSpeedButton;
    Panel4: TPanel;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    PanelFx1: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    BStop: TSpeedButton;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure BKeep0VisibleClick(Sender: TObject);
    procedure B_ActionClick(Sender: TObject);
    procedure B_RedoClick(Sender: TObject);
    procedure B_ShutDownClick(Sender: TObject);
    procedure B_UndoClick(Sender: TObject);
    procedure B_ViewAllClick(Sender: TObject);
    procedure B_ViewSelectionClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var {%H-}CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure Panel7Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure BStopClick(Sender: TObject);
    procedure Splitter2Moved(Sender: TObject);
  private
    FrameViewCmdList1: TFrameViewCmdList;
    FrameEditString1: TFrameEditString;
    FModifyMode: boolean;
    //FSequencerInfoListToModify: TSequencerInfoList;
    function GetTopName: string;
    function GetSequencerInfoList: TSequencerInfoList;
    function GetTopDuration: single;
    procedure ProcessSelectionChangeEvent(Sender: TObject);
    procedure ProcessUndoRedoChangeEvent(Sender: TObject);
    procedure ProcessUserChangeStepDurationEvent(Sender: TObject);
    procedure ProcessOnViewChangeEvent(Sender: TObject);
    procedure ProcessFrameEditString1OnEnterPressed(Sender: TObject);
    procedure ProcessFrameViewCmdListOnEditedByUserEvent(Sender: TObject);
    procedure SetTopName(AValue: string);
    procedure UpdateWidgetState;
    procedure RemoveFocusFromFrameEditString1;
  private
    FEditingTopIndex: integer;
    FToogleSpeedButtonManager: TToggleSpeedButtonManager;
  public
    FSeq: TFrameSequencer;
    procedure ProcessEndOfSequencePreview;

    procedure SetAddMode(const aNewName: string);
    procedure SetModifyMode(const aName: string; const aSequencerInfoList: TSequencerInfoList; aTopIndex: integer);

    property TopDuration: single read GetTopDuration;
    property SequencerInfoList: TSequencerInfoList read GetSequencerInfoList;
    property TopName: string read GetTopName write SetTopName;

    property EditingTopIndex: integer read FEditingTopIndex;
  end;

var
  FormSequenceEdition: TFormSequenceEdition;

implementation
uses u_resource_string, u_program_options;

{$R *.lfm}

{ TFormSequenceEdition }

procedure TFormSequenceEdition.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if not FrameEditString1.HaveFocus then FSeq.ProcessKey(Key, Shift);
end;

procedure TFormSequenceEdition.FormShow(Sender: TObject);
begin
  SpeedButton1.Caption := SCancel;
  Label7.Caption := SBegin;
  Label8.Caption := SEnd;
  Label9.Caption := SDuration;
  Label13.Caption := SBegin;
  Label14.Caption := SEnd;
  Label15.Caption := SDuration;
  Label3.Caption := SView;
  Label2.Caption := SSelection;

  FToogleSpeedButtonManager.Checked[BKeep0Visible] := ProgramOptions.KeepOriginVisible;
  FSeq.SetOptions([bglsKeepTimeOriginVisible], ProgramOptions.KeepOriginVisible);

//  B_Undo.Caption := SUndo;
//  B_Redo.Caption := SRedo;

//  FSeq.BGLVirtualScreen1.QueryLoadTextures;
  FSeq.TranslateStrings;
  FSeq.RecomputeVerticalStepsPosition;
  FSeq.UndoRedoManager.Clear; // because 'RecomputeVerticalStepsPosition' can be undone
  FSeq.View_All;

  UpdateWidgetState;
end;

procedure TFormSequenceEdition.Panel7Click(Sender: TObject);
begin
  RemoveFocusFromFrameEditString1;
end;

procedure TFormSequenceEdition.SpeedButton1Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFormSequenceEdition.SpeedButton2Click(Sender: TObject);
begin
  if not FrameEditString1.TextIsValid then
  begin
    FrameEditString1.SetTheFocus;
    exit;
  end;

  if FSeq.StepList.Count = 0 then
    exit;

  ModalResult := mrOk;
end;

procedure TFormSequenceEdition.BStopClick(Sender: TObject);
begin
  FSeq.Stop;
end;

procedure TFormSequenceEdition.Splitter2Moved(Sender: TObject);
begin
  FSeq.RecomputeVerticalStepsPosition;
end;

procedure TFormSequenceEdition.ProcessSelectionChangeEvent(Sender: TObject);
var s: TSequenceStep;
begin
  if FSeq.SelectedCount = 1 then begin
    s := FSeq.Selected[0] as TSequenceStep;
    Label1.Caption := s.Caption;
    FrameViewCmdList1.SetWorkingStep(s, FSeq)
  end else begin
    FrameViewCmdList1.SetWorkingStep(NIL, FSeq);
    Label1.Caption := FSeq.SelectedCount.ToString+' '+SSelected;
  end;

  UpdateWidgetState;
  // selection label info
  if (FSeq.SelectedAreaBeginTime = 0) and (FSeq.SelectedAreaEndTime = 0) then Label12.Caption :=''
    else Label12.Caption := TimeToString(FSeq.SelectedAreaBeginTime);

  if FSeq.SelectedAreaEndTime = 0 then Label16.Caption :=''
    else Label16.Caption := TimeToString(FSeq.SelectedAreaEndTime);

  if FSeq.SelectedAreaDuration = 0 then Label17.Caption :=''
    else Label17.Caption := TimeToString(FSeq.SelectedAreaDuration);
end;

function TFormSequenceEdition.GetTopDuration: single;
var s: TSequenceStep;
begin
  s := TSequenceStep(FSeq.GetLastStep);
  if s <> NIL then
    Result := s.TimePos+s.Duration;
end;

function TFormSequenceEdition.GetSequencerInfoList: TSequencerInfoList;
begin
  Result := FSeq.SaveToSequencerInfoList;
end;

function TFormSequenceEdition.GetTopName: string;
begin
  Result := FrameEditString1.Text;
end;

procedure TFormSequenceEdition.ProcessUndoRedoChangeEvent(Sender: TObject);
begin
  UpdateWidgetState;
end;

procedure TFormSequenceEdition.ProcessUserChangeStepDurationEvent(Sender: TObject);
begin
  ProcessSelectionChangeEvent(Sender);
end;

procedure TFormSequenceEdition.ProcessOnViewChangeEvent(Sender: TObject);
begin
  with FSeq do
  begin
    Label6.Caption := TimeToString( View_BeginTime );
    Label10.Caption := TimeToString( View_EndTime );
    Label11.Caption := TimeToString( View_EndTime-View_BeginTime );
  end;
end;

procedure TFormSequenceEdition.ProcessFrameEditString1OnEnterPressed(
  Sender: TObject);
begin
  FSeq.SetFocus;
end;

procedure TFormSequenceEdition.ProcessFrameViewCmdListOnEditedByUserEvent(Sender: TObject);
begin
  FSeq.Redraw;
end;

procedure TFormSequenceEdition.SetTopName(AValue: string);
begin
  FrameEditString1.Text := AValue;
  FrameEditString1.ClearSelection;
end;

procedure TFormSequenceEdition.UpdateWidgetState;
begin
  B_Undo.Enabled := FSeq.UndoRedoManager.UndoAvailable;
  if B_Undo.Enabled then
    B_Undo.Hint := SUndo+' '+FSeq.UndoRedoManager.UndoPeekCurrent.Name
  else
    B_Undo.Hint := '';

  B_Redo.Enabled := FSeq.UndoRedoManager.RedoAvailable;
  if B_Redo.Enabled then
    B_Redo.Hint := SRedo+' '+FSeq.UndoRedoManager.RedoPeekCurrent.Name
  else
    B_Redo.Hint := '';

  BPreview.Enabled := FSeq.StepList.Count > 0;
  BStop.Enabled := BPreview.Enabled;
  B_ViewAll.Enabled := BPreview.Enabled;
  B_ViewSelection.Enabled := BPreview.Enabled;
end;

procedure TFormSequenceEdition.RemoveFocusFromFrameEditString1;
begin
  FrameViewCmdList1.LB.SetFocus;
end;

procedure TFormSequenceEdition.ProcessEndOfSequencePreview;
begin
  BPreview.Enabled := True;
end;

procedure TFormSequenceEdition.SetAddMode( const aNewName: string );
begin
  FModifyMode := FALSE;
  SpeedButton2.Caption := SAddThisSequence;
  SpeedButton2.ImageIndex := 0; //+

  FrameEditString1.Text := aNewName;
  FrameEditString1.ClearSelection;
  FSeq.UndoRedoManager.Clear;
  FSeq.Clear;

  FEditingTopIndex := -1;
end;

procedure TFormSequenceEdition.SetModifyMode( const aName: string; const aSequencerInfoList: TSequencerInfoList;
  aTopIndex: integer);
begin
  FModifyMode := TRUE;
  SpeedButton2.Caption := SApplyChanges;
  SpeedButton2.ImageIndex := 3;// checked

  FrameEditString1.Text := aName;
  FrameEditString1.ClearSelection;
  FSeq.UndoRedoManager.Clear;
  FSeq.LoadFromSequencerInfoList(aSequencerInfoList);

  FEditingTopIndex := aTopIndex;
end;

procedure TFormSequenceEdition.FormCreate(Sender: TObject);
begin
  FSeq := TFrameSequencer.Create(Self);
  FSeq.Parent := Panel1;
  FSeq.Align := alClient;
  FSeq.OnSelectionChange := @ProcessSelectionChangeEvent;
  FSeq.OnUndoRedoChange := @ProcessUndoRedoChangeEvent;
  FSeq.OnUserChangeStepDuration := @ProcessUserChangeStepDurationEvent;
  FSeq.OnViewChange := @ProcessOnViewChangeEvent;
  FSeq.BGLVirtualScreen1.OnClick := @Panel7Click; // when user click on sequencer, we remove the focus
                                                  // from the sequence name TEdit
  FSeq.SetOptions([bglsKeepTimeOriginVisible], True);

  FrameViewCmdList1 := TFrameViewCmdList.Create(Self);
  FrameViewCmdList1.Parent := Panel2;
  FrameViewCmdList1.Align := alClient;
  FrameViewCmdList1.ItemHeight := 16;
  FrameViewCmdList1.UpdateMenuEntryFromProjectOptions;
  FrameViewCmdList1.OnEditedByUser := @ProcessFrameViewCmdListOnEditedByUserEvent;

  FrameEditString1 := TFrameEditString.Create(Self);
  FrameEditString1.Parent := Panel4;
  FrameEditString1.Align := alClient;
  FrameEditString1.ModeNoSpecialChar;
  FrameEditString1.Title := SSequenceName;
  FrameEditString1.OnEnterPressed := @ProcessFrameEditString1OnEnterPressed;

  FToogleSpeedButtonManager := TToggleSpeedButtonManager.Create;
  FToogleSpeedButtonManager.ToggleType := tsbLikeCheckBox;
  FToogleSpeedButtonManager.SetActivatedColors($0003C4FC, $00272727);
  FToogleSpeedButtonManager.SetDeactivatedColors($00004B62, $00EAEAEA);
  FToogleSpeedButtonManager.Add(BKeep0Visible, True);
end;

procedure TFormSequenceEdition.B_ActionClick(Sender: TObject);
begin
  BPreview.Enabled := False;
  FSeq.Play;
end;

procedure TFormSequenceEdition.BKeep0VisibleClick(Sender: TObject);
var b: boolean;
begin
  b := FToogleSpeedButtonManager.Checked[BKeep0Visible];
  FSeq.SetOptions([bglsKeepTimeOriginVisible], b);

  ProgramOptions.KeepOriginVisible := b;
end;

procedure TFormSequenceEdition.B_RedoClick(Sender: TObject);
begin
  FSeq.Redo;
end;

procedure TFormSequenceEdition.B_ShutDownClick(Sender: TObject);
begin
  //SoundManager.StopCaptureToPlayback;

  SoundManager.DeleteAllEffects(True);
  SoundManager.StopAllSound(True);
  UniverseManager.BlackOut;
end;

procedure TFormSequenceEdition.B_UndoClick(Sender: TObject);
begin
  FSeq.Undo;
end;

procedure TFormSequenceEdition.B_ViewAllClick(Sender: TObject);
begin
  FSeq.View_All;
end;

procedure TFormSequenceEdition.B_ViewSelectionClick(Sender: TObject);
begin
  FSeq.View_ZoomOnSelectedArea;
end;

procedure TFormSequenceEdition.FormCloseQuery(Sender: TObject; var CanClose: boolean );
begin
  FSeq.Stop; // remove callbacks for preview
end;

procedure TFormSequenceEdition.FormDestroy(Sender: TObject);
begin
  FToogleSpeedButtonManager.Free;
end;

end.

