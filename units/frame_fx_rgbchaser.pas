unit frame_fx_rgbchaser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ComCtrls, Buttons, ExtCtrls,
  LCLType, Spin, Dialogs, Graphics, LCLTranslator,
  frame_viewcolorlist, frame_color_palette,
  frame_bglvirtualscreen_sequencer, u_top_player, u_common,
  frame_viewfixtureslist, frame_velocity,
  u_list_dmxuniverse;

type

  { TFrameFXRGBChaser }

  TFrameFXRGBChaser = class(TFrame)
    BAdd6: TSpeedButton;
    CBMove: TComboBox;
    CheckBox1: TCheckBox;
    ColorButton1: TColorButton;
    FSE1: TFloatSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    Shape5: TShape;
    Shape6: TShape;
    SpeedButton1: TSpeedButton;
    SpeedButton10: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton9: TSpeedButton;
    UpDown1: TUpDown;
    UpDown2: TUpDown;
    procedure BAdd6Click(Sender: TObject);
    procedure FSE1Change(Sender: TObject);
    procedure Label3Click(Sender: TObject);
    procedure Panel4Resize(Sender: TObject);
    procedure Shape5MouseDown(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure Shape5MouseMove(Sender: TObject; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure Shape5MouseUp(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure SpeedButton10Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton9Click(Sender: TObject);
    procedure UpDown1Click(Sender: TObject; Button: TUDBtnType);
    procedure UpDown2Click(Sender: TObject; Button: TUDBtnType);
  private
    FPalette: TFrame_ColorPalette;
    FrameViewColorList1: TFrameViewColorList;
    FrameViewFixturesList1: TFrameViewFixturesList;
    Frame_Velocity1: TFrame_Velocity;
  private
    FMouseOrigin: TPoint;
    function GetDelay: single;
    procedure ResetDelay;
    procedure LoopDragShape5;
    procedure UpdateWidgets;
  private
    FStepList: TStepList;
    FCmds: TCmdList;
    FShortReadableString: string;
    FCmdDuration: single;
    procedure GenerateForwardWithoutBackColor(var tp: single);
    procedure GenerateBackwardWithoutBackColor(var tp: single);
    procedure GeneratePingPongWithoutBackColor(var tp: single);
    procedure GenerateForwardWithBackColor(var tp: single);
    procedure GenerateBackwardWithBackColor(var tp: single);
    procedure GeneratePingPongWithBackColor(var tp: single);

    procedure GenerateForward(var tp: single);
    procedure GenerateBackward(var tp: single);
    procedure GeneratePingPong(var tp: single);
    procedure GenerateCmdLoop(var tp: single);
    function GenerateCmds: boolean;
    function BackColor: TColor;
  private
    procedure SetGUIMode(AValue: TGUIMode);
  public

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground({%H-}DC: HDC); override;

    procedure Fill;
    procedure UserHaveSelected(aFix: TDMXFixture);
    procedure UserHaveRemoved(aFix: TDMXFixture);
    procedure ClearSelectedFixtures;

    procedure UpdateEditMode;
    procedure UpdateStringAfterLanguageChange;

    property Frame_ColorPalette: TFrame_ColorPalette read FPalette write FPalette;
    property GUIMode: TGUIMode write SetGUIMode;
  end;

implementation

uses Math, u_resource_string, u_project_manager,
  u_helper, u_dmxtools_rgb, frame_sequencer, u_mainform, u_add_action_dmx;

{$R *.lfm}

{ TFrameFXRGBChaser }

procedure TFrameFXRGBChaser.SpeedButton1Click(Sender: TObject);
begin
  FrameViewColorList1.Add(FPalette.ShapeColor); //  (FPalette.ChartColor);
end;

procedure TFrameFXRGBChaser.Label3Click(Sender: TObject);
begin
  CheckBox1.Checked:=not CheckBox1.Checked;
end;

procedure TFrameFXRGBChaser.Panel4Resize(Sender: TObject);
begin
  Shape6.Width := (Panel4.ClientWidth-12) div 4;
  Shape5.Width := Shape6.Width;
  ResetDelay;
end;

procedure TFrameFXRGBChaser.BAdd6Click(Sender: TObject);
begin
  TopPLayer.StopPreview;
  if not GenerateCmds then
    exit;

  FormDMXRGBTools.FCmd := FCmds;
  FormDMXRGBTools.FShortReadable := FShortReadableString;
  FormDMXRGBTools.FDuration := FCmdDuration;
  FormDMXRGBTools.BAdd1Click(NIL);
end;

procedure TFrameFXRGBChaser.FSE1Change(Sender: TObject);
begin
  UpdateWidgets;
end;

procedure TFrameFXRGBChaser.Shape5MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMouseOrigin := Shape5.ScreenToClient(Mouse.CursorPos);
end;

procedure TFrameFXRGBChaser.Shape5MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if FMouseOrigin <> Point(-10000,-10000) then
    LoopDragShape5;
end;

procedure TFrameFXRGBChaser.Shape5MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMouseOrigin := Point(-10000,-10000);
end;

procedure TFrameFXRGBChaser.SpeedButton10Click(Sender: TObject);
begin
  TopPLayer.StopPreview;
end;

procedure TFrameFXRGBChaser.SpeedButton2Click(Sender: TObject);
begin
  FrameViewColorList1.DeleteSelection;
end;

procedure TFrameFXRGBChaser.SpeedButton9Click(Sender: TObject);
begin
  if not GenerateCmds then
    exit;

  TopPlayer.PreviewCmdList(FCmds);
end;

procedure TFrameFXRGBChaser.UpDown1Click(Sender: TObject; Button: TUDBtnType);
begin
  case Button of
    btNext: FrameViewColorList1.MoveSelectionUp;
    btPrev: FrameViewColorList1.MoveSelectionDown;
  end;
end;

procedure TFrameFXRGBChaser.UpDown2Click(Sender: TObject; Button: TUDBtnType);
begin
  case Button of
    btNext: FrameViewFixturesList1.MoveSelectionUp;
    btPrev: FrameViewFixturesList1.MoveSelectionDown;
  end;
end;

function TFrameFXRGBChaser.GetDelay: single;
var coef: single;
begin
  coef := (Shape5.Left-Shape6.Left)/Shape6.Width;
  Result := FSE1.Value*coef;
end;

procedure TFrameFXRGBChaser.ResetDelay;
begin
  Shape5.Left := Shape6.Left+Shape6.Width;
  UpdateWidgets;
end;

procedure TFrameFXRGBChaser.LoopDragShape5;
var delta, xx: integer;
begin
  repeat
    delta := Shape5.ScreenToClient(Mouse.CursorPos).x-FMouseOrigin.x;
    if delta <>0  then
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

procedure TFrameFXRGBChaser.UpdateWidgets;
begin
  Label26.Caption := FormatFloat('0.00', GetDelay)+SSec;
  Shape1.Visible := Abs(GetDelay-FSE1.Value)<0.01;
end;

procedure TFrameFXRGBChaser.GenerateForwardWithoutBackColor(var tp: single);
var i, j: integer;
begin
  for i:=0 to FrameViewColorList1.Count-1 do
    for j:=0 to FrameViewFixturesList1.Count-1 do
    begin
      FStepList.AddCmdDMXDimmerRGB(tp,
                                FrameViewFixturesList1.Fixtures[j],
                                FrameViewColorList1.ChartColors[i].ToColor,
                                FSE1.Value,
                                Frame_Velocity1.SelectedCurveID);
      tp := tp+GetDelay;
    end;
end;

procedure TFrameFXRGBChaser.GenerateBackwardWithoutBackColor(var tp: single);
var i, j: integer;
begin
  for i:=0 to FrameViewColorList1.Count-1 do
    for j:=FrameViewFixturesList1.Count-1 downto 0 do
    begin
      FStepList.AddCmdDMXDimmerRGB(tp,
                                FrameViewFixturesList1.Fixtures[j],
                                FrameViewColorList1.ChartColors[i].ToColor,
                                FSE1.Value,
                                Frame_Velocity1.SelectedCurveID);
      tp := tp+GetDelay;
    end;
end;

procedure TFrameFXRGBChaser.GeneratePingPongWithoutBackColor(var tp: single);
begin
  GenerateForwardWithoutBackColor(tp);
  GenerateBackwardWithoutBackColor(tp);
end;

procedure TFrameFXRGBChaser.GenerateForwardWithBackColor(var tp: single);
var i, j: integer;
begin
  for i:=0 to FrameViewColorList1.Count-1 do
    for j:=0 to FrameViewFixturesList1.Count-1 do
    begin
      FStepList.AddCmdDMXDimmerRGB(tp,
                                FrameViewFixturesList1.Fixtures[j],
                                FrameViewColorList1.ChartColors[i].ToColor,
                                FSE1.Value,
                                Frame_Velocity1.SelectedCurveID);
      FStepList.AddCmdDMXDimmerRGB(tp+FSE1.Value,
                                FrameViewFixturesList1.Fixtures[j],
                                ColorButton1.ButtonColor,
                                FSE1.Value,
                                Frame_Velocity1.SelectedCurveID);
      tp := tp+GetDelay;
    end;
end;

procedure TFrameFXRGBChaser.GenerateBackwardWithBackColor(var tp: single);
var i, j: integer;
begin
  for i:=0 to FrameViewColorList1.Count-1 do
    for j:=FrameViewFixturesList1.Count-1 downto 0 do
    begin
      FStepList.AddCmdDMXDimmerRGB(tp,
                                FrameViewFixturesList1.Fixtures[j],
                                FrameViewColorList1.ChartColors[i].ToColor,
                                FSE1.Value,
                                Frame_Velocity1.SelectedCurveID);
      FStepList.AddCmdDMXDimmerRGB(tp+FSE1.Value,
                                FrameViewFixturesList1.Fixtures[j],
                                ColorButton1.ButtonColor,
                                FSE1.Value,
                                Frame_Velocity1.SelectedCurveID);
      tp := tp+GetDelay;
    end;
end;

procedure TFrameFXRGBChaser.GeneratePingPongWithBackColor(var tp: single);
begin
  GenerateForwardWithBackColor(tp);
  GenerateBackwardWithBackColor(tp);
end;

procedure TFrameFXRGBChaser.GenerateForward(var tp: single);
begin
  if CheckBox1.Checked then
    GenerateForwardWithBackColor(tp)
  else
    GenerateForwardWithoutBackColor(tp);
end;

procedure TFrameFXRGBChaser.GenerateBackward(var tp: single);
begin
  if CheckBox1.Checked then
    GenerateBackwardWithBackColor(tp)
  else
    GenerateBackwardWithoutBackColor(tp);
end;

procedure TFrameFXRGBChaser.GeneratePingPong(var tp: single);
begin
  if CheckBox1.Checked
   then GeneratePingPongWithBackColor(tp)
   else GeneratePingPongWithoutBackColor(tp);
end;

procedure TFrameFXRGBChaser.GenerateCmdLoop(var tp: single);
var lastStep: TSequenceStep;
  sec: single;
begin
  lastStep := TSequenceStep(FStepList.Last);
  if lastStep = NIL then
    exit;

  if lastStep.CmdList.IsWait(sec) then
    tp := lastStep.TimePos+sec;
  FStepList.AddCmdLoop(tp);
end;

function TFrameFXRGBChaser.GenerateCmds: boolean;
var timepos: single;
  fix: TDMXFixture;
begin
  Result := FALSE;
  if FrameViewFixturesList1.Count = 0 then exit;
  if FrameViewColorList1.Count = 0 then exit;
  if CBMove.ItemIndex = -1 then exit;

  FStepList.FreeSteps;
  timepos := 0;

  if FrameViewFixturesList1.Count > 1 then
    FShortReadableString := SDMXChaserRGB+' '+FrameViewFixturesList1.Count.ToString+' '+SChannels
  else begin
    FShortReadableString := SDMXChaserRGB+' '+SOn_+' ';
    fix := FrameViewFixturesList1.Fixtures[0];
    if fix.Description <> '' then FShortReadableString := FShortReadableString + fix.Description
      else FShortReadableString := FShortReadableString + fix.Name;
  end;

  case CBMove.ItemIndex of
    0:       // one shoot
      GenerateForward(timepos);

    1:
      begin //loop forward
        GenerateForward(timepos);
        GenerateCmdLoop(timepos);
      end;
    2:
      begin // loop backward
        GenerateBackward(timepos);
        GenerateCmdLoop(timepos);
      end;
    3:
      begin // ping pong
        GeneratePingPong(timepos);
        GenerateCmdLoop(timepos);
      end;
  end;//case

  FStepList.Sort;
  FCmdDuration := FStepList.Duration;
  FCmds := FStepList.ToCmdListOfSingleCmd;
  Result := TRUE;
end;

function TFrameFXRGBChaser.BackColor: TColor;
begin
  Result := ColorButton1.ButtonColor;
end;

procedure TFrameFXRGBChaser.SetGUIMode(AValue: TGUIMode);
begin
  case AValue of
    guiMainDMX:
      begin
        BAdd6.Caption := SCreateSequence;
        FrameViewFixturesList1.FTargetViewProjector := FormMain.FrameViewProjector1;
      end;

    guiEditSequence:
      begin
        BAdd6.Caption := SAdd;
        FrameViewFixturesList1.FTargetViewProjector := FormAddDMXAction.FrameViewProjector1;
      end;
  end;
end;

constructor TFrameFXRGBChaser.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FStepList:=TStepList.Create;

  FrameViewColorList1:=TFrameViewColorList.Create(Self);
  FrameViewColorList1.Parent:=Panel1;
  FrameViewColorList1.Align:=alClient;
  FrameViewColorList1.PresetManager.Init1(SColorPresets,
                  SpeedButton3,
                  ConcatPaths([Project.AppPresetsFolder, 'ColorList'+PRESET_FILE_EXTENSION]));

  FrameViewFixturesList1:=TFrameViewFixturesList.Create(Self);
  FrameViewFixturesList1.Parent:=Panel2;
  FrameViewFixturesList1.Align:=alClient;
  FrameViewFixturesList1.MultiSelect:=TRUE;

  Frame_Velocity1:=TFrame_Velocity.Create(Self);
  Frame_Velocity1.Parent:=Panel3;
  Frame_Velocity1.Align:=alClient;

  CBMove.Clear;
  CBMove.Items.Add(SOneShoot);
  CBMove.Items.Add(SLoopForward);
  CBMove.Items.Add(SLoopBackward);
  CBMove.Items.Add(SPingPong);
  CBMove.ItemIndex := 1;

  FMouseOrigin:=Point(-10000,-10000);
  ResetDelay;
end;

destructor TFrameFXRGBChaser.Destroy;
begin
  FStepList.FreeSteps;
  FStepList.Free;
  inherited Destroy;
end;

procedure TFrameFXRGBChaser.EraseBackground(DC: HDC);
begin
  // do nothing here
end;

procedure TFrameFXRGBChaser.Fill;
begin
  FrameViewFixturesList1.Fill;
  Frame_Velocity1.UpdateList;
  UpdateStringAfterLanguageChange;
end;

procedure TFrameFXRGBChaser.UserHaveSelected(aFix: TDMXFixture);
begin
  FrameViewFixturesList1.AddFixture(aFix);
end;

procedure TFrameFXRGBChaser.UserHaveRemoved(aFix: TDMXFixture);
begin
  FrameViewFixturesList1.RemoveFixture(aFix);
end;

procedure TFrameFXRGBChaser.ClearSelectedFixtures;
begin
  FrameViewFixturesList1.Clear;
end;

procedure TFrameFXRGBChaser.UpdateEditMode;
begin
  BAdd6.Visible:=Project.Options.EditMode;
end;

procedure TFrameFXRGBChaser.UpdateStringAfterLanguageChange;
begin
  CBMove.Items.Strings[0]:=SOneShoot;
  CBMove.Items.Strings[1]:=SLoopForward;
  CBMove.Items.Strings[2]:=SLoopBackWard;
  CBMove.Items.Strings[3]:=SPingPong;
  FrameViewColorList1.UpdateStringAfterLanguageChange;
end;

end.

