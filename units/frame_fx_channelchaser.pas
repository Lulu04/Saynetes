unit frame_fx_channelchaser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Spin, Buttons,
  ComCtrls, LCLType, LCLTranslator,
  frame_bglvirtualscreen_sequencer, frame_viewchannelslist,
  frame_velocity, u_sequence_player, u_common,
  u_list_dmxuniverse, frame_trackbar;

type

  { TFrameFXChannelChaser }

  TFrameFXChannelChaser = class(TFrame)
    BAdd6: TSpeedButton;
    CBMove: TComboBox;
    CBMode: TComboBox;
    FSE1: TFloatSpinEdit;
    Label2: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Shape6: TShape;
    Shape1: TShape;
    Shape7: TShape;
    Shape8: TShape;
    Shape5: TShape;
    SpeedButton10: TSpeedButton;
    SpeedButton9: TSpeedButton;
    UpDown1: TUpDown;
    procedure BAdd6Click(Sender: TObject);
    procedure Panel4Resize(Sender: TObject);
    procedure Shape5MouseDown(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure Shape5MouseMove(Sender: TObject; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure Shape5MouseUp(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure SpeedButton10Click(Sender: TObject);
    procedure SpeedButton9Click(Sender: TObject);
    procedure TBChange(Sender: TObject);
    procedure UpDown1Click(Sender: TObject; Button: TUDBtnType);
  private
    FMouseOrigin: TPoint;
    function GetDelay: single;
    procedure ResetDelay;
    procedure LoopDragShape5;
  private
    FStepList: TStepList;
    FCmds: TCmdList;
    FShortReadableString: string;
    FCmdDuration: single;
    procedure GenerateForwardMode1(var tp: single);
    procedure GenerateBackwardMode1(var tp: single);
    procedure GeneratePingPongMode1(var tp: single);
    procedure GenerateForwardMode2(var tp: single);
    procedure GenerateBackwardMode2(var tp: single);
    procedure GeneratePingPongMode2(var tp: single);
    procedure GenerateForward(var tp: single);
    procedure GenerateBackward(var tp: single);
    procedure GeneratePingPong(var tp: single);
    procedure GenerateCmdLoop(var tp: single);
    function GenerateCmds: boolean;
  private
    procedure SetGUIMode(AValue: TGUIMode);
  public
    FrameViewChannelsList1: TFrameViewChannelsList;
    Frame_Velocity1: TFrame_Velocity;
    FrameTBMinMax: TFrameTrackBar;
    procedure UpdateWidgets;
  public
//    FTargetViewProjector: TFrameViewProjector;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground({%H-}DC: HDC); override;

    procedure Fill;
    procedure UserHaveSelected(aChan: TDMXChannel);
    procedure UserHaveRemoved(aChan: TDMXChannel);
    procedure ClearSelectedChannels;

    procedure UpdateEditMode;
    procedure UpdateStringAfterLanguageChange;
    property GUIMode: TGUIMode write SetGUIMode;
  end;

implementation

uses u_project_manager, u_resource_string, u_helper,
  u_dmxtools_channels, frame_sequencer, u_mainform, u_add_action_dmx, Math,
  BGRABitmapTypes;

{$R *.lfm}

{ TFrameFXChannelChaser }

procedure TFrameFXChannelChaser.UpDown1Click(Sender: TObject; Button: TUDBtnType );
begin
  case Button of
   btNext: FrameViewChannelsList1.MoveSelectionUp;
   btPrev: FrameViewChannelsList1.MoveSelectionDown;
  end;
end;

function TFrameFXChannelChaser.GetDelay: single;
var coef: single;
begin
  coef := (Shape5.Left-Shape6.Left)/Shape6.Width;
  Result := FSE1.Value*coef;
end;

procedure TFrameFXChannelChaser.ResetDelay;
begin
  Shape5.Left := Shape6.Left+Shape6.Width;
  UpdateWidgets;
end;

procedure TFrameFXChannelChaser.LoopDragShape5;
var delta, xx: integer;
begin
  repeat
    delta := Shape5.ScreenToClient(Mouse.CursorPos).x-FMouseOrigin.x;
    if delta <> 0 then
    begin
      xx := Shape5.Left+delta;
      Shape5.Left := EnsureRange(xx,
                                 Shape6.Left+Trunc(Shape6.Width*0.25),
                                 Panel4.ClientWidth-Shape5.Width);
      UpdateWidgets;
    end;
    Application.ProcessMessages;
    Sleep(1);
  until FMouseOrigin = Point(-10000,-10000);
end;

procedure TFrameFXChannelChaser.TBChange(Sender: TObject);
begin
  UpdateWidgets;
end;

procedure TFrameFXChannelChaser.Panel4Resize(Sender: TObject);
begin
  Shape6.Width := (Panel4.ClientWidth-12) div 4;
  Shape5.Width := Shape6.Width;
  ResetDelay;
end;

procedure TFrameFXChannelChaser.Shape5MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMouseOrigin := Shape5.ScreenToClient(Mouse.CursorPos);
end;

procedure TFrameFXChannelChaser.Shape5MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if FMouseOrigin <> Point(-10000,-10000) then
    LoopDragShape5;
end;

procedure TFrameFXChannelChaser.Shape5MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMouseOrigin := Point(-10000,-10000);
end;

procedure TFrameFXChannelChaser.SpeedButton10Click(Sender: TObject);
begin
  SeqPLayer.StopPreview;
end;

procedure TFrameFXChannelChaser.SpeedButton9Click(Sender: TObject);
begin
  if not GenerateCmds then
    exit;

  SeqPlayer.PreviewCmdList(FCmds);
end;

procedure TFrameFXChannelChaser.BAdd6Click(Sender: TObject);
begin
  if FrameViewChannelsList1.Count < 2 then exit;
  SeqPLayer.StopPreview;
  if not GenerateCmds then
    exit;

  FormDMXChannelsTools.FCmd := FCmds;
  FormDMXChannelsTools.FShortReadable := FShortReadableString;
  FormDMXChannelsTools.FDuration := FCmdDuration;
  FormDMXChannelsTools.BAdd1Click(NIL);
end;

procedure TFrameFXChannelChaser.GenerateForwardMode1(var tp: single);
var i: integer;
begin
  for i:=0 to FrameViewChannelsList1.Count-1 do
  begin
    FStepList.AddCmdDMXDimmer(tp,
                              FrameViewChannelsList1.Channels[i],
                              FrameTBMinMax.PercentMax,
                              FSE1.Value,
                              Frame_Velocity1.SelectedCurveID);
    tp := tp+GetDelay;
  end;
  for i:=0 to FrameViewChannelsList1.Count-1 do
  begin
    FStepList.AddCmdDMXDimmer(tp,
                              FrameViewChannelsList1.Channels[i],
                              FrameTBMinMax.PercentMin,
                              FSE1.Value,
                              Frame_Velocity1.SelectedCurveID);
    tp := tp+GetDelay;
  end;
end;

procedure TFrameFXChannelChaser.GenerateBackwardMode1(var tp: single);
var i: integer;
begin
  for i:=FrameViewChannelsList1.Count-1 downto 0 do
  begin
    FStepList.AddCmdDMXDimmer(tp,
                              FrameViewChannelsList1.Channels[i],
                              FrameTBMinMax.PercentMax,
                              FSE1.Value,
                              Frame_Velocity1.SelectedCurveID);
    tp := tp+GetDelay;
  end;
  for i:=FrameViewChannelsList1.Count-1 downto 0 do
  begin
    FStepList.AddCmdDMXDimmer(tp,
                              FrameViewChannelsList1.Channels[i],
                              FrameTBMinMax.PercentMin,
                              FSE1.Value,
                              Frame_Velocity1.SelectedCurveID);
    tp := tp+GetDelay;
  end;
end;

procedure TFrameFXChannelChaser.GeneratePingPongMode1(var tp: single);
var i: integer;
begin
  for i:=0 to FrameViewChannelsList1.Count-1 do
  begin
    FStepList.AddCmdDMXDimmer(tp,
                              FrameViewChannelsList1.Channels[i],
                              FrameTBMinMax.PercentMax,
                              FSE1.Value,
                              Frame_Velocity1.SelectedCurveID);
    tp := tp+GetDelay;
  end;
  for i:=0 to FrameViewChannelsList1.Count-1 do
  begin
    FStepList.AddCmdDMXDimmer(tp,
                              FrameViewChannelsList1.Channels[i],
                              FrameTBMinMax.PercentMin,
                              FSE1.Value,
                              Frame_Velocity1.SelectedCurveID);
    tp := tp+GetDelay;
  end;

  for i:=FrameViewChannelsList1.Count-1 downto 0 do
  begin
    FStepList.AddCmdDMXDimmer(tp,
                              FrameViewChannelsList1.Channels[i],
                              FrameTBMinMax.PercentMax,
                              FSE1.Value,
                              Frame_Velocity1.SelectedCurveID);
    tp := tp+GetDelay;
  end;
  for i:=FrameViewChannelsList1.Count-1 downto 0 do
  begin
    FStepList.AddCmdDMXDimmer(tp,
                              FrameViewChannelsList1.Channels[i],
                              FrameTBMinMax.PercentMin,
                              FSE1.Value,
                              Frame_Velocity1.SelectedCurveID);
    tp := tp+GetDelay;
  end;
  GenerateCmdLoop(tp);
end;

procedure TFrameFXChannelChaser.GenerateForwardMode2(var tp: single);
var i: integer;
begin
  for i:=0 to FrameViewChannelsList1.Count-1 do
  begin
    FStepList.AddCmdWave(tp,
                         FrameViewChannelsList1.Channels[i],
                         FrameTBMinMax.PercentMax,
                         FSE1.Value,
                         Frame_Velocity1.SelectedCurveID,
                         0.0,
                         FrameTBMinMax.PercentMin,
                         FSE1.Value,
                         Frame_Velocity1.SelectedCurveID);
      tp := tp+GetDelay;
    end;
end;

procedure TFrameFXChannelChaser.GenerateBackwardMode2(var tp: single);
var i: integer;
begin
  for i:=FrameViewChannelsList1.Count-1 downto 0 do
  begin
    FStepList.AddCmdWave(tp,
                         FrameViewChannelsList1.Channels[i],
                         FrameTBMinMax.PercentMax,
                         FSE1.Value,
                         Frame_Velocity1.SelectedCurveID,
                         0.0,
                         FrameTBMinMax.PercentMin,
                         FSE1.Value,
                         Frame_Velocity1.SelectedCurveID);
    tp := tp+GetDelay;
  end;
end;

procedure TFrameFXChannelChaser.GeneratePingPongMode2(var tp: single);
var i: integer;
begin
  for i:=0 to FrameViewChannelsList1.Count-2 do
  begin
    FStepList.AddCmdWave(tp,
                         FrameViewChannelsList1.Channels[i],
                         FrameTBMinMax.PercentMax,
                         FSE1.Value,
                         Frame_Velocity1.SelectedCurveID,
                         0.0,
                         FrameTBMinMax.PercentMin,
                         FSE1.Value,
                         Frame_Velocity1.SelectedCurveID);
    tp := tp+GetDelay;
  end;
  for i:=FrameViewChannelsList1.Count-1 downto 1 do
  begin
    FStepList.AddCmdWave(tp,
                         FrameViewChannelsList1.Channels[i],
                         FrameTBMinMax.PercentMax,
                         FSE1.Value,
                         Frame_Velocity1.SelectedCurveID,
                         0.0,
                         FrameTBMinMax.PercentMin,
                         FSE1.Value,
                         Frame_Velocity1.SelectedCurveID);
    tp := tp+GetDelay;
  end;
  GenerateCmdLoop(tp);
end;

procedure TFrameFXChannelChaser.GenerateForward(var tp: single);
begin
  case CBMode.ItemIndex of
   0: GenerateForwardMode1(tp);
   1: GenerateForwardMode2(tp);
  end;
end;

procedure TFrameFXChannelChaser.GenerateBackward(var tp: single);
begin
  case CBMode.ItemIndex of
   0: GenerateBackwardMode1(tp);
   1: GenerateBackwardMode2(tp);
  end;
end;

procedure TFrameFXChannelChaser.GeneratePingPong(var tp: single);
begin
  case CBMode.ItemIndex of
   0: GeneratePingPongMode1(tp);
   1: GeneratePingPongMode2(tp);
  end;
end;

procedure TFrameFXChannelChaser.GenerateCmdLoop(var tp: single);
begin
  FStepList.AddCmdLoop(tp+0.001);
end;

function TFrameFXChannelChaser.GenerateCmds: boolean;
var timepos: single;
begin
  Result := FALSE;
  if FrameViewChannelsList1.Count = 0 then
    exit;
  if (CBMove.ItemIndex = -1) or (CBMode.ItemIndex = -1) then
    exit;

  FStepList.FreeSteps;
  timepos := 0;

  case CBMove.ItemIndex of
    0: begin  // one shoot
      GenerateForward(timepos);
    end;
    1: begin //loop forward
      GenerateForward(timepos);
      GenerateCmdLoop(timepos);
    end;
    2: begin // loop backward
      GenerateBackward(timepos);
      GenerateCmdLoop(timepos);
    end;
    3: begin // ping pong
      GeneratePingPong(timepos);
    end;
  end;//case

  FStepList.Sort;
  FCmdDuration := TSequenceStep(FStepList.Last).TimePos;
  //FCmdDuration:=FStepList.Duration;
  FCmds := FStepList.ToCmdListOfSingleCmd;
  FShortReadableString:=SDMXChaser+' '+FrameViewChannelsList1.Count.ToString+' '+SChannels;
  Result := TRUE;
end;

procedure TFrameFXChannelChaser.SetGUIMode(AValue: TGUIMode);
begin
  case AValue of
    guiMainDMX:
      begin
        BAdd6.Caption := SCreateSequence;
        FrameViewChannelsList1.FTargetViewProjector := FormMain.FrameViewProjector1;
      end;

    guiEditSequence:
      begin
        BAdd6.Caption := SAdd;
        FrameViewChannelsList1.FTargetViewProjector := FormAddDMXAction.FrameViewProjector1;
      end;
  end;
end;

procedure TFrameFXChannelChaser.UpdateWidgets;
begin
  Label26.Caption := FormatFloat('0.00', GetDelay)+SSec;
  Shape1.Visible := Abs(GetDelay-FSE1.Value) < 0.01;
end;

constructor TFrameFXChannelChaser.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FStepList := TStepList.Create;

  Frame_Velocity1 := TFrame_Velocity.Create(Self);
  Frame_Velocity1.Parent := Panel2;
  Frame_Velocity1.Align := alClient;

  FrameViewChannelsList1 := TFrameViewChannelsList.Create(Self);
  FrameViewChannelsList1.Parent := Panel1;
  FrameViewChannelsList1.Align := alClient;
  FrameViewChannelsList1.MultiSelect := TRUE;

  FrameTBMinMax := TFrameTrackBar.Create(Self, Panel5);
  FrameTBMinMax.Init(trHorizontal, False, True, True);
  FrameTBMinMax.PercentMin := 0.25;
  FrameTBMinMax.PercentMax := 0.75;
  FrameTBMinMax.OnChange := @TBChange;

  CBMove.Clear;
  CBMove.Items.Add(SOneShoot);
  CBMove.Items.Add(SLoopForward);
  CBMove.Items.Add(SLoopBackward);
  CBMove.Items.Add(SPingPong);
  CBMove.ItemIndex := 1;

  CBMode.Clear;
  CBMode.Items.Add(SMode1);
  CBMode.Items.Add(SMode2);
  CBMode.ItemIndex := 1;

  FMouseOrigin:=Point(-10000,-10000);
  ResetDelay;
end;

destructor TFrameFXChannelChaser.Destroy;
begin
  FStepList.FreeSteps;
  FStepList.Free;
  inherited Destroy;
end;

procedure TFrameFXChannelChaser.EraseBackground(DC: HDC);
begin
// do nothing here
end;

procedure TFrameFXChannelChaser.Fill;
begin
  FrameViewChannelsList1.Fill;
  Frame_Velocity1.UpdateList;
  UpdateStringAfterLanguageChange;
end;

procedure TFrameFXChannelChaser.UserHaveSelected(aChan: TDMXChannel);
begin
  FrameViewChannelsList1.AddChannel(aChan);
end;

procedure TFrameFXChannelChaser.UserHaveRemoved(aChan: TDMXChannel);
begin
  FrameViewChannelsList1.RemoveChannel(aChan);
end;

procedure TFrameFXChannelChaser.ClearSelectedChannels;
begin
  FrameViewChannelsList1.Clear;
end;

procedure TFrameFXChannelChaser.UpdateEditMode;
begin
  BAdd6.Visible:=Project.Options.EditMode;
end;

procedure TFrameFXChannelChaser.UpdateStringAfterLanguageChange;
begin
  CBMove.Items.Strings[0] := SOneShoot;
  CBMove.Items.Strings[1] := SLoopForward;
  CBMove.Items.Strings[2] := SLoopBackWard;
  CBMove.Items.Strings[3] := SPingPong;

  CBMode.Items.Strings[0] := SMode1;
  CBMode.Items.Strings[1] := SMode2;

  Label2.Caption := SLevels;
//  Label24.Caption := SCurve_;  <- done automatically
end;

end.

