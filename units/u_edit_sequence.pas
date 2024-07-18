unit u_edit_sequence;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  LCLTranslator, StdCtrls, Buttons, ComCtrls, Menus,
  frame_bglvirtualscreen_sequencer, frame_sequencer, frame_viewcmdlist,
  u_common, frame_editstring, u_audio_manager, u_list_dmxuniverse,
  lcl_utils, frame_viewsequencelist;

type

  { TFormSequenceEdition }

  TFormSequenceEdition = class(TForm)
    BHelpCmdlist: TSpeedButton;
    BHelpSequencer: TSpeedButton;
    BKeep0Visible: TSpeedButton;
    BShutdownPopup: TSpeedButton;
    B_Redo: TSpeedButton;
    BStopPreview: TSpeedButton;
    B_Undo: TSpeedButton;
    BStartPreview: TSpeedButton;
    B_ViewAll: TSpeedButton;
    B_ViewSelection: TSpeedButton;
    MIStopAudioAndLight: TMenuItem;
    MIOnlyBlackout: TMenuItem;
    MIStopOnlyAudio: TMenuItem;
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
    PopShutdown: TPopupMenu;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure BHelpSequencerClick(Sender: TObject);
    procedure BKeep0VisibleClick(Sender: TObject);
    procedure B_ActionClick(Sender: TObject);
    procedure B_RedoClick(Sender: TObject);
    procedure BStopPreviewClick(Sender: TObject);
    procedure B_UndoClick(Sender: TObject);
    procedure B_ViewAllClick(Sender: TObject);
    procedure B_ViewSelectionClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var {%H-}CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure Panel7Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure Splitter2Moved(Sender: TObject);
  private type TMode=(mAdd, mInsert, mModify);
  private
    FrameViewCmdList1: TFrameViewCmdList;
    FrameEditString1: TFrameEditString;
    FMode: TMode;
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
    ParentFrameViewSequenceList: TFrameViewSequenceList;
    procedure ProcessEndOfSequencePreview;

    procedure SetAddMode(const aNewName: string);
    procedure SetInsertMode(const aNewName: string);
    procedure SetModifyMode(const aName: string; const aSequencerInfoList: TSequencerInfoList; aTopIndex: integer);

    property TopDuration: single read GetTopDuration;
    property SequencerInfoList: TSequencerInfoList read GetSequencerInfoList;
    property TopName: string read GetTopName write SetTopName;

    property EditingTopIndex: integer read FEditingTopIndex;
  end;

var
  FormSequenceEdition: TFormSequenceEdition;

implementation
uses u_resource_string, u_program_options, form_help, u_list_sequence,
  u_project_manager, u_mainform, LCLType;

{$R *.lfm}

{ TFormSequenceEdition }

procedure TFormSequenceEdition.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var t: single;
begin
  if Key = VK_SPACE then begin
    if FSeq.IsPlaying then BStopPreviewClick(BStopPreview)
        else begin
          if not FSeq.MouseIsOverSequencer then FSeq.Play
            else begin
              t := FSeq.View_BeginTime + FSeq.AbscissaToTimePos(FSeq.BGLVirtualScreen1.ScreenToClient(Mouse.CursorPos).x);
              FSeq.PlayFrom(t);
            end;
        end;
    Key := 0;
    exit;
  end;


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

  BStopPreview.Caption := MIStopOnlyAudio.Caption;
  BStopPreview.Tag := BStopPreview.Tag;

  FToogleSpeedButtonManager.Checked[BKeep0Visible] := ProgramOptions.KeepOriginVisible;
  FSeq.SetOptions([bglsKeepTimeOriginVisible], ProgramOptions.KeepOriginVisible);

  FSeq.RetrieveClipboardContent;

//  B_Undo.Caption := SUndo;
//  B_Redo.Caption := SRedo;

//  FSeq.BGLVirtualScreen1.QueryLoadTextures;
  FSeq.TranslateStrings;
  FSeq.RecomputeVerticalStepsPosition;
  FSeq.UndoRedoManager.Clear; // because 'RecomputeVerticalStepsPosition' can be undone
  FSeq.View_All;

  UpdateWidgetState;

  {$if defined(LCLGTK2)}
  // maximize the window to the size of main form
  SetBounds(FormMain.Top, FormMain.Left, FormMain.Width, FormMain.Height);
  {$else}
  BorderIcons := [biSystemMenu];
  Position := poMainFormCenter;
  WindowState := wsMaximized;
  {$endif}
end;

procedure TFormSequenceEdition.Panel7Click(Sender: TObject);
begin
  RemoveFocusFromFrameEditString1;
end;

procedure TFormSequenceEdition.SpeedButton1Click(Sender: TObject);
begin
  Close;
end;

procedure TFormSequenceEdition.SpeedButton2Click(Sender: TObject);
var seq: TSequence;
begin
  if not FrameEditString1.TextIsValid then
  begin
    FrameEditString1.SetTheFocus;
    exit;
  end;

  if FSeq.StepList.Count = 0 then
    exit;

  case FMode of
    mAdd: begin
      seq := Sequences.AddSequence(TopName, SequencerInfoList);
      ParentFrameViewSequenceList.Add(seq.ID);
    end;

    mInsert: begin
      seq := Sequences.InsertSequence(ParentFrameViewSequenceList.LB.ItemIndex, TopName, SequencerInfoList);
      ParentFrameViewSequenceList.Insert(seq.ID);
    end;

    mModify: begin
     seq := Sequences.GetSequenceByIndex(FEditingTopIndex);
     seq.Stop;
     seq.Name := TopName;
     seq.SequencerInfoList := SequencerInfoList;
     ParentFrameViewSequenceList.RemoveErrorHint;
     FormMain.CheckSequenceError;
    end;
  end;

  Project.SetModified;
  FormMain.FrameViewProjector1.ForceReconstructOpenGLObjects;

  Close;
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

  BStartPreview.Enabled := FSeq.StepList.Count > 0;
  B_ViewAll.Enabled := BStartPreview.Enabled;
  B_ViewSelection.Enabled := BStartPreview.Enabled;
end;

procedure TFormSequenceEdition.RemoveFocusFromFrameEditString1;
begin
  FrameViewCmdList1.LB.SetFocus;
end;

procedure TFormSequenceEdition.ProcessEndOfSequencePreview;
begin
  BStartPreview.Enabled := True;
end;

procedure TFormSequenceEdition.SetAddMode( const aNewName: string );
begin
  FMode := mAdd;
  SpeedButton2.Caption := SAddThisSequence;
  SpeedButton2.ImageIndex := 0; //+

  FrameEditString1.Text := aNewName;
  FrameEditString1.ClearSelection;
  FSeq.UndoRedoManager.Clear;
  FSeq.Clear;

  FEditingTopIndex := -1;
end;

procedure TFormSequenceEdition.SetInsertMode(const aNewName: string);
begin
  FMode := mInsert;
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
  FMode := mModify;
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
  BStartPreview.Enabled := False;
  FSeq.Play;
end;

procedure TFormSequenceEdition.BKeep0VisibleClick(Sender: TObject);
var b: boolean;
begin
  b := FToogleSpeedButtonManager.Checked[BKeep0Visible];
  FSeq.SetOptions([bglsKeepTimeOriginVisible], b);

  ProgramOptions.KeepOriginVisible := b;
end;

procedure TFormSequenceEdition.BHelpSequencerClick(Sender: TObject);
begin
  if Sender = BHelpSequencer then _ShowHelp(HelpSequencer, BHelpSequencer);
  if Sender = BHelpCmdList then _ShowHelp(HelpSequencerCmdList, BHelpSequencer);
end;

procedure TFormSequenceEdition.B_RedoClick(Sender: TObject);
begin
  FSeq.Redo;
end;

procedure TFormSequenceEdition.BStopPreviewClick(Sender: TObject);
var p: TPoint;
  m: TMenuItem;
begin
  if Sender = BShutdownPopup then begin
    p.x := BStopPreview.Left;
    p.y := BStopPreview.Top + BStopPreview.Height;
    p := PanelFx1.ClientToScreen(p);
    PopShutdown.PopUp(p.x, p.y);
  end;

  if (Sender = MIStopOnlyAudio) or (Sender = MIOnlyBlackout) or (Sender = MIStopAudioAndLight) then begin
    m := Sender as TMenuItem;
    BStopPreview.Caption := m.Caption;
    BStopPreview.Tag := m.Tag;
  end;

  if Sender = BStopPreview then begin
    FSeq.Stop;
    case BStopPreview.Tag of
      0: begin
        SoundManager.StopAllSound(False);
      end;
      1: begin
        UniverseManager.BlackOut;
      end;
      2: begin
        SoundManager.StopAllSound(False);
        UniverseManager.BlackOut;
      end;
    end;
  end;
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

procedure TFormSequenceEdition.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TFormSequenceEdition.FormCloseQuery(Sender: TObject; var CanClose: boolean );
begin
  FSeq.Stop; // remove callbacks for preview
  FSeq.SaveClipboardContent; // save the content of clipboard to allow copy/paste from a sequence to another
end;

procedure TFormSequenceEdition.FormDestroy(Sender: TObject);
begin
  FToogleSpeedButtonManager.Free;
end;

end.

