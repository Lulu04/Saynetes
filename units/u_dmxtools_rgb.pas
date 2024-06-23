unit u_dmxtools_rgb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, Spin, ComCtrls, LCLTranslator, frame_velocity, u_notebook_util,
  u_list_dmxuniverse, frame_color_palette, u_common,
  frame_fx_rgbchaser, frame_viewfixtureslist, frame_viewprojectors,
  frame_trackbar;

type

  { TFormDMXRGBTools }

  TFormDMXRGBTools = class(TForm)
    BAdd1: TSpeedButton;
    BAdd2: TSpeedButton;
    BAdd3: TSpeedButton;
    BAdd4: TSpeedButton;
    BAdd5: TSpeedButton;
    BAdd6: TSpeedButton;
    CB: TComboBox;
    ComboBox1: TComboBox;
    FloatSpinEdit1: TFloatSpinEdit;
    FloatSpinEdit2: TFloatSpinEdit;
    FloatSpinEdit3: TFloatSpinEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label22: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label3: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Notebook1: TNotebook;
    PageFlash: TPage;
    PageChaser: TPage;
    PageAudioFollower: TPage;
    PageCopy: TPage;
    PageDimmer: TPage;
    PageFlame: TPage;
    PageStop: TPage;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    Shape1: TShape;
    Shape2: TShape;
    SpeedButton1: TSpeedButton;
    SpeedButton11: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    TB2: TTrackBar;
    TB3: TTrackBar;
    TB4: TTrackBar;
    TB6: TTrackBar;
    TB7: TTrackBar;
    procedure BAdd1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ProcessColorChangeEvent(Sender: TObject);
    procedure RadioButton1Change(Sender: TObject);
    procedure SpeedButton7Click(Sender: TObject);
  private
    FFirstShown: boolean;
    Frame_Velocity1: TFrame_Velocity;
    FrameViewFixturesList1: TFrameViewFixturesList;
    FNoteBookManager: TNoteBookManager;
    FCheckedLabelManager: TCheckedLabelManager;
    FTargetFixtures: ArrayOfDmxFixtures;
    procedure GetTargetFixtures;
    procedure ApplyEffectOnTargetFixtures;
    procedure StopAll;
    procedure ProcessPageSelectionChange(Sender: TObject);
    procedure ProcessCopySelectionChange(Sender: TObject);
    procedure UpdateWidgets;
    procedure GenerateCmdForDimmerRGB;
    procedure GenerateCmdForFlameRGB;
    procedure GenerateCmdForAudioFollowerRGB;
    procedure GenerateCmdForCopyRGB;
    procedure GenerateCmdForStopEffectRGB;
    procedure GenerateCmdForFlashRGB;
   private
    FGUIMode: TGUIMode;
    FTargetViewProjector: TFrameViewProjector;
    procedure SetGUIMode(AValue: TGUIMode);
   private
    FFrameTrackBar1: TFrameTrackBar;
  public
    Frame_ColorPalette1: TFrame_ColorPalette;
    FrameFXRGBChaser1: TFrameFXRGBChaser;
    FShortReadable: string;
    FCmd: TSingleCmd;
    FDuration: single;

    procedure UpdateEditMode;

    procedure UserHaveSelected(aFix: TDMXFixture);
    procedure UserHaveRemoved(aFix: TDMXFixture);
    procedure ClearSelectedFixtures;

    property GUIMode: TGUIMode read FGUIMode write SetGUIMode;
  end;

var
  FormDMXRGBTools: TFormDMXRGBTools;

implementation

uses u_utils, u_resource_string, u_audio_manager, u_project_manager,
  u_helper, u_sequence_player, ALSound, u_add_action_dmx, u_mainform;

{$R *.lfm}

{ TFormDMXRGBTools }

procedure TFormDMXRGBTools.FormCreate(Sender: TObject);
begin
  Frame_ColorPalette1 := TFrame_ColorPalette.Create(Self);
  Frame_ColorPalette1.Parent := Panel1;
  Frame_ColorPalette1.Align := alClient;
  Frame_ColorPalette1.OnChange := @ProcessColorChangeEvent;

  Frame_Velocity1 := TFrame_Velocity.Create(Self);
  Frame_Velocity1.Parent := Panel2;
  Frame_Velocity1.Align := alClient;

  FrameFXRGBChaser1 := TFrameFXRGBChaser.Create(Self);
  FrameFXRGBChaser1.Parent := Panel3;
  FrameFXRGBChaser1.Align := alClient;
  FrameFXRGBChaser1.Frame_ColorPalette := Frame_ColorPalette1;

  FrameViewFixturesList1 := TFrameViewFixturesList.Create(Self);
  with FrameViewFixturesList1 do
  begin
    Parent := Panel4;
    Align := alClient;
    MultiSelect := FALSE;
    OnSelectionChange := @ProcessCopySelectionChange;
  end;

  FNoteBookManager := TNoteBookManager.Create(Notebook1);
  with FNoteBookManager do
  begin
    SetActivatedColors($0003C4FC, clBlack);
    SetDeactivatedColors($00484848, $00EAEAEA);
    LinkButtonToPage(SpeedButton1, PageDimmer);
    LinkButtonToPage(SpeedButton2, PageFlame);
    LinkButtonToPage(SpeedButton3, PageAudioFollower);
    LinkButtonToPage(SpeedButton9, PageFlash);
    LinkButtonToPage(SpeedButton4, PageCopy);
    LinkButtonToPage(SpeedButton5, PageStop);
    LinkButtonToPage(SpeedButton8, PageChaser);
    OnSelectionChange := @ProcessPageSelectionChange;
  end;

  FFrameTrackBar1 := TFrameTrackBar.Create(NIL);
  FFrameTrackBar1.Parent := Panel5;
  FFrameTrackBar1.Align := alClient;
  FFrameTrackBar1.Init(trHorizontal, False, True, True);
  FFrameTrackBar1.OnChange := @RadioButton1Change;
  FFrameTrackBar1.Enabled := False;

  FCheckedLabelManager := TCheckedLabelManager.Create;
  FCheckedLabelManager.CaptureLabelClick(Label28);
  FCheckedLabelManager.CaptureLabelClick(Label29);
  FCheckedLabelManager.CaptureLabelClick(Label22);
  FCheckedLabelManager.CaptureLabelClick(Label30);

  FFirstShown := True;
end;

procedure TFormDMXRGBTools.BAdd1Click(Sender: TObject);
begin
  GetTargetFixtures;
  if length(FTargetFixtures) = 0 then exit;

  if Notebook1.PageIndex = Notebook1.IndexOf(PageDimmer) then
    GenerateCmdForDimmerRGB;

  if Notebook1.PageIndex = Notebook1.IndexOf(PageFlame) then
    GenerateCmdForFlameRGB;

  if Notebook1.PageIndex = Notebook1.IndexOf(PageAudioFollower) then
  begin
    if ComboBox1.ItemIndex = -1 then exit;
    GenerateCmdForAudioFollowerRGB;
  end;

  if Notebook1.PageIndex = Notebook1.IndexOf(PageCopy) then
  begin
    if FrameViewFixturesList1.SelCount <> 1 then exit;
    if Length(FTargetFixtures) < 2 then exit;
    GenerateCmdForCopyRGB;
  end;

  if Notebook1.PageIndex = Notebook1.IndexOf(PageStop) then
    GenerateCmdForStopEffectRGB;

  if Notebook1.PageIndex = Notebook1.IndexOf(PageFlash) then
    GenerateCmdForFlashRGB;

  case GUIMode of
    guiMainDMX:
      begin
        FTargetViewProjector.FixtureSourceForCopy := NIL;
        FTargetViewProjector.FrameViewDMXCursors1.SetSourceFixtureForRGBCopy(NIL);
        FTargetViewProjector.RegisterCmd(FCmd, FShortReadable, FDuration);
      end;

    guiEditSequence:
      begin
        FormAddDMXAction.FrameViewProjector1.FixtureSourceForCopy := NIL;
        FormAddDMXAction.FrameViewProjector1.FrameViewDMXCursors1.SetSourceFixtureForRGBCopy(NIL);
        FormAddDMXAction.FrameViewProjector1.RegisterCmd(FCmd, FShortReadable, FDuration);
      end;
  end;

//  ModalResult := mrOk;
  Hide;
end;

procedure TFormDMXRGBTools.FormDestroy(Sender: TObject);
begin
  FNoteBookManager.Free;
  FFrameTrackBar1.Free;
  FreeAndNil(FCheckedLabelManager);
end;

procedure TFormDMXRGBTools.FormHide(Sender: TObject);
begin
  SeqPlayer.StopPreview;
  //StopAll
end;

procedure TFormDMXRGBTools.FormShow(Sender: TObject);
var i: integer;
begin
  if FFirstShown then
  begin
    FFirstShown := False;
    FNoteBookManager.ActivePage(PageDimmer);
  end;

  SpeedButton1.Caption := SDimmer;
  SpeedButton2.Caption := SFlame;
  SpeedButton8.Caption := SChaser;
  SpeedButton3.Caption := SAudioFollower;
  SpeedButton9.Caption := SFlash;
  SpeedButton4.Caption := SCopy;
  SpeedButton5.Caption := SStop;

  Label6.Caption := SpeedButton5.Hint;

  Frame_ColorPalette1.UpdateStringAfterLanguageChange;
  Frame_Velocity1.UpdateList;

  FrameFXRGBChaser1.Fill;

  FrameViewFixturesList1.Fill;

  ComboBox1.Clear;
  for i:=0 to SoundManager.Count-1 do
    ComboBox1.Items.Add(ExtractFileName(SoundManager.GetSoundByIndex(i).Filename));

  UpdateWidgets;
end;

procedure TFormDMXRGBTools.ProcessColorChangeEvent(Sender: TObject);
begin
  UpdateWidgets;
  ApplyEffectOnTargetFixtures;
end;

procedure TFormDMXRGBTools.RadioButton1Change(Sender: TObject);
begin

  UpdateWidgets;
end;

procedure TFormDMXRGBTools.SpeedButton7Click(Sender: TObject);
begin
  SoundManager.StopAllSound(False);
  SeqPlayer.StopPreview;
end;

procedure TFormDMXRGBTools.StopAll;
var i: integer;
  snd: TALSSound;
begin
  SeqPlayer.StopPreview;
  i := ComboBox1.ItemIndex;
  if i > -1 then begin
    snd := SoundManager.GetSoundByIndex(ComboBox1.ItemIndex);
    if snd <> NIL then snd.Stop;
  end;

  GetTargetFixtures;
  for i:=0 to High(FTargetFixtures) do
    FTargetFixtures[i].StopEffectRGB;
end;

procedure TFormDMXRGBTools.ProcessPageSelectionChange(Sender: TObject);
begin
  StopAll;
  Frame_ColorPalette1.Visible := (Notebook1.ActivePageComponent = PageDimmer) or
                                 (Notebook1.ActivePageComponent = PageFlame) or
                                 (Notebook1.ActivePageComponent = PageAudioFollower) or
                                 (Notebook1.ActivePageComponent = PageChaser) or
                                 (Notebook1.ActivePageComponent = PageFlash);

  if Notebook1.PageIndex <> Notebook1.IndexOf(PageCopy) then begin
    FTargetViewProjector.FixtureSourceForCopy := NIL;
    FTargetViewProjector.FrameViewDMXCursors1.SetSourceFixtureForRGBCopy(NIL);
  end else
    ProcessCopySelectionChange(NIL);

  if (Notebook1.PageIndex = Notebook1.IndexOf(PageFlame)) or
     (Notebook1.PageIndex = Notebook1.IndexOf(PageCopy)) or
     (Notebook1.PageIndex = Notebook1.IndexOf(PageStop)) then
    ApplyEffectOnTargetFixtures;

end;

procedure TFormDMXRGBTools.ProcessCopySelectionChange(Sender: TObject);
begin
  if FrameViewFixturesList1.SelCount = 0 then
  begin
    Label19.Caption := SNone;
    Label20.Caption := '';
    FTargetViewProjector.FixtureSourceForCopy := NIL;
    FTargetViewProjector.FrameViewDMXCursors1.SetSourceFixtureForRGBCopy(NIL);
  end
  else
  begin
    Label19.Caption := FrameViewFixturesList1.Selected[0].Name;
    Label20.Caption := FrameViewFixturesList1.Selected[0].Description;
    FTargetViewProjector.FixtureSourceForCopy := FrameViewFixturesList1.Selected[0];
    FTargetViewProjector.FrameViewDMXCursors1.SetSourceFixtureForRGBCopy(FrameViewFixturesList1.Selected[0]);
    ApplyEffectOnTargetFixtures;
  end;
end;

procedure TFormDMXRGBTools.UpdateWidgets;
begin
  // audio follower
  if TB4.Position < 0 then
    Label8.Caption := TB4.Position.ToString
  else
    Label8.Caption := '+'+TB4.Position.ToString;
  Label10.Caption := FormatFloat('0.00', TB6.Position/10)+SSec;
  // flame
  Label12.Caption := FormatFloat('0.00', TB2.Position/100)+SSec;
  Label13.Caption := FormatFloat('0.0', TB3.Position/TB3.Max*100)+'%';
  Label15.Caption := FormatFloat('0.0', TB7.Position/TB7.Max*100)+'%';
  // flash
  FFrameTrackBar1.Enabled := RadioButton2.Checked;
  FloatSpinEdit3.Enabled := RadioButton4.Checked;
  if FloatSpinEdit3.Value < FloatSpinEdit2.Value then
    FloatSpinEdit3.Value := FloatSpinEdit2.Value;
  if FloatSpinEdit2.Value > FloatSpinEdit3.Value then
    FloatSpinEdit2.Value := FloatSpinEdit3.Value;
end;

procedure TFormDMXRGBTools.GenerateCmdForDimmerRGB;
var i: integer;
  fix: TDMXFixture;
begin
  FCmd := CmdTitleDMXDimmerRGB(Frame_ColorPalette1.SelectedColor, FloatSpinEdit1.Value, // action title
                            Frame_Velocity1.SelectedCurveID);

  for i:=0 to High(FTargetFixtures) do
  begin
    fix := FTargetFixtures[i];
    if fix.HasRGBChannel then
      FCmd.ConcatCmd(CmdDMXDimmerRGB(fix.Universe.ID,
                                     fix.ID,
                                     Frame_ColorPalette1.SelectedColor,
                                     FloatSpinEdit1.Value,
                                     Frame_Velocity1.SelectedCurveID));
  end;
  // DMXDIMMERRGB IDuniverse IDFixture percent duration IDcurve
  FShortReadable := SDMXDimmerRGB+' '+SOn_+' ';
  if Length(FTargetFixtures) > 1 then
    FShortReadable := FShortReadable + SMultiple
  else if fix.Description <> '' then FShortReadable := FShortReadable + fix.Description
         else FShortReadable := FShortReadable + fix.Name;
  FDuration := FloatSpinEdit1.Value;
end;

procedure TFormDMXRGBTools.GenerateCmdForFlameRGB;
var i: integer;
  fix: TDMXFixture;
begin
  FCmd := CmdTitleDMXFlameRGB(Frame_ColorPalette1.SelectedColor,
                             TB2.Position/100,
                             TB3.Position/TB3.Max,
                             TB7.Position/TB7.Max);

  for i:=0 to High(FTargetFixtures) do
  begin
    fix := FTargetFixtures[i];
    if fix.HasRGBChannel then
      FCmd.ConcatCmd(CmdDMXFlameRGB(fix.Universe.ID,
                     fix.ID,
                     Frame_ColorPalette1.SelectedColor,
                     TB2.Position/100,
                     TB3.Position/TB3.Max,
                     TB7.Position/TB7.Max));
  end;
  // DMXFLAMMERVB IDuniverse IDFixture dmxadress Color Speed Amplitude Soften
  FShortReadable := SDMXFlameRGB+' '+SOn_+' ';
  if Length(FTargetFixtures) > 1 then
    FShortReadable := FShortReadable + SMultiple
  else if fix.Description <> '' then FShortReadable := FShortReadable + fix.Description
         else FShortReadable := FShortReadable + fix.Name;
  FDuration := 0.0;
end;

procedure TFormDMXRGBTools.GenerateCmdForAudioFollowerRGB;
var i: integer;
  fix: TDMXFixture;
  snd: TALSSound;
begin
  snd := SoundManager.GetSoundByIndex(ComboBox1.ItemIndex);
  if snd = NIL then exit;

  FCmd := CmdTitleDMXAudioFollowerRGB(snd.Tag,
                                      Frame_ColorPalette1.SelectedColor,
                                      TB4.Position/10,
                                      TB6.Position/10);

  for i:=0 to High(FTargetFixtures) do
  begin
    fix := FTargetFixtures[i];
    if fix.HasRGBChannel then
      FCmd.ConcatCmd(CmdDMXAudioFollowerRGB(fix.Universe.ID,
                     fix.ID,
                     snd.Tag,
                     Frame_ColorPalette1.SelectedColor,
                     TB4.Position/10,
                     TB6.Position/10));
  end;
  // DMXSUIVEURAUDIORVB IDuniverse IDFixture IDaudio Color Gain MaxPercent SoftenTime
  FShortReadable := SDMXAudioFollowerRGB+' '+ExtractFileName(snd.Filename)+' '+SOn_+' ';
  if Length(FTargetFixtures) > 1 then
    FShortReadable := FShortReadable + SMultiple
  else if fix.Description <> '' then FShortReadable := FShortReadable + fix.Description
         else FShortReadable := FShortReadable + fix.Name;
  FDuration := 0.0;
end;

procedure TFormDMXRGBTools.GenerateCmdForCopyRGB;
var i: integer;
  fixSource, fixTarget, singleFix: TDMXFixture;
begin
  if FrameViewFixturesList1.SelCount <> 1 then exit;
  fixSource := FrameViewFixturesList1.Selected[0];
  if not fixSource.HasRGBChannel then exit;

  FCmd := CmdTitleDMXCopyRGB(fixSource.Universe.ID, fixSource.ID);

  for i:=0 to High(FTargetFixtures) do
  begin
    fixTarget := FTargetFixtures[i];
    if fixTarget.HasRGBChannel and (fixTarget <> fixSource) then begin
      FCmd.ConcatCmd(CmdDMXCopyRGB(fixSource.Universe.ID,
                                   fixSource.ID,
                                   fixTarget.Universe.ID,
                                   fixTarget.ID));
      singleFix := fixTarget;
    end;
  end;
  FShortReadable := SDMXCopyRGB+' ';
  if fixSource.Description <> '' then FShortReadable := FShortReadable + fixSource.Description
    else FShortReadable := FShortReadable + fixSource.Name;
  FShortReadable := FShortReadable +' '+SOn_+' ';
  if Length(FTargetFixtures) > 2 then
    FShortReadable := FShortReadable + SMultiple
  else if singleFix.Description <> '' then FShortReadable := FShortReadable + singleFix.Description
         else FShortReadable := FShortReadable + singleFix.Name;
  FDuration := 0.0;
end;

procedure TFormDMXRGBTools.GenerateCmdForStopEffectRGB;
var i: integer;
  fix: TDMXFixture;
begin
  FCmd := CmdTitleDMXStopEffectRGB;

  for i:=0 to High(FTargetFixtures) do
  begin
    fix := FTargetFixtures[i];
    if fix.HasRGBChannel then
      FCmd.ConcatCmd(CmdDMXStopEffectRGB(fix.Universe.ID, fix.ID));
  end;
  FShortReadable := SDMXStopEffectRGB+' ';
  if Length(FTargetFixtures) > 1 then
    FShortReadable := FShortReadable + SMultiple
  else if fix.Description <> '' then FShortReadable := FShortReadable + fix.Description
         else FShortReadable := FShortReadable + fix.Name;
  FDuration := 0.0;
end;

procedure TFormDMXRGBTools.GenerateCmdForFlashRGB;
var i: integer;
  fix: TDMXFixture;
  vmin, vmax, dmin, dmax: single;
begin
  vmax := FFrameTrackBar1.PercentValue;
  if RadioButton1.Checked then
    vmin := vmax
  else
    vmin := FFrameTrackBar1.IntervalMin;

  dmin := FloatSpinEdit2.Value;
  if RadioButton3.Checked then
    dmax := dmin
  else
    dmax := FloatSpinEdit3.Value;

  FCmd := CmdTitleDMXFlashRGB(Frame_ColorPalette1.SelectedColor, vmin, vmax, dmin, dmax);

  for i:=0 to High(FTargetFixtures) do
  begin
    fix := FTargetFixtures[i];
    if fix.HasRGBChannel then
      FCmd.ConcatCmd(CmdDMXFlashRGB(fix.Universe.ID, fix.ID, Frame_ColorPalette1.SelectedColor, vmin, vmax, dmin, dmax));
  end;
  FShortReadable := SDMXFlashRGB+' ';
  if Length(FTargetFixtures) > 1 then
    FShortReadable := FShortReadable + SMultiple
  else if fix.Description <> '' then FShortReadable := FShortReadable + fix.Description
         else FShortReadable := FShortReadable + fix.Name;
  FDuration := dmax;
end;

procedure TFormDMXRGBTools.SetGUIMode(AValue: TGUIMode);
begin
  FGUIMode := AValue;
  FrameFXRGBChaser1.GUIMode := AValue;
  case FGUIMode of
    guiMainDMX:
      begin
        BAdd1.Caption := SCreateSequence;
        BAdd2.Caption := SCreateSequence;
        BAdd3.Caption := SCreateSequence;
        BAdd4.Caption := SCreateSequence;
        BAdd5.Caption := SCreateSequence;
        BAdd6.Caption := SCreateSequence;
        FTargetViewProjector := FormMain.FrameViewProjector1;
        FrameViewFixturesList1.FTargetViewProjector := FTargetViewProjector;
      end;

    guiEditSequence:
      begin
        BAdd1.Caption := SAdd;
        BAdd2.Caption := SAdd;
        BAdd3.Caption := SAdd;
        BAdd4.Caption := SAdd;
        BAdd5.Caption := SAdd;
        BAdd6.Caption := SAdd;
        FTargetViewProjector := FormAddDMXAction.FrameViewProjector1;
        FrameViewFixturesList1.FTargetViewProjector := FTargetViewProjector;
      end;
  end;

  Label1.Caption := SDurationInSecond;
  Label3.Caption := SWaitTime;
  Label4.Caption := SAmplitude;
  Label14.Caption := SSoften;
  Label5.Caption := SGain;
  Label7.Caption := SSoftenOn;
  Label28.Caption := SFixedIntensity;
  Label29.Caption := SRandomIntensityBetween;
  Label22.Caption := SFixedDuration;
  Label30.Caption := SRandomDurationBetween;
  Label31.Caption := SSeconds_;
end;

procedure TFormDMXRGBTools.UpdateEditMode;
begin
  BAdd1.Visible := Project.Options.EditMode;
  BAdd2.Visible := Project.Options.EditMode;
  BAdd3.Visible := Project.Options.EditMode;
  BAdd4.Visible := Project.Options.EditMode;
  BAdd5.Visible := Project.Options.EditMode;
  FrameFXRGBChaser1.UpdateEditMode;
end;

procedure TFormDMXRGBTools.UserHaveSelected(aFix: TDMXFixture);
begin
  if aFix.HasRGBChannel then
  begin
    FrameFXRGBChaser1.UserHaveSelected(aFix);
    FrameViewFixturesList1.AddFixture(aFix);
  end;
end;

procedure TFormDMXRGBTools.UserHaveRemoved(aFix: TDMXFixture);
begin
  FrameFXRGBChaser1.UserHaveRemoved(aFix);
  FrameViewFixturesList1.RemoveFixture(aFix);
end;

procedure TFormDMXRGBTools.ClearSelectedFixtures;
begin
  FrameFXRGBChaser1.ClearSelectedFixtures;
  FrameViewFixturesList1.Clear;
end;

procedure TFormDMXRGBTools.GetTargetFixtures;
begin
  FTargetFixtures := FTargetViewProjector.FrameViewDMXCursors1.GetTargetFixtures;
end;

procedure TFormDMXRGBTools.ApplyEffectOnTargetFixtures;
var i: Integer;
  snd: TALSSound;
  vmin, vmax, dmin, dmax: single;
  sourceFix: TDMXFixture;
begin
  GetTargetFixtures;
  if length(FTargetFixtures) = 0 then exit;

  if Notebook1.PageIndex=Notebook1.IndexOf(PageDimmer) then
  begin
    for i:=0 to High(FTargetFixtures) do
    begin
      FTargetFixtures[i].RGBColor := Frame_ColorPalette1.SelectedColor;

      if FTargetFixtures[i].IsVisibleOnViewCursor then
        FTargetViewProjector.FrameViewDMXCursors1.RedrawRGBChannelsOnFixture(FTargetFixtures[i]);
    end;
    if FTargetViewProjector.FShowLevel then
      FTargetViewProjector.Redraw;
  end;

  if Notebook1.PageIndex = Notebook1.IndexOf(PageFlame) then
  begin
    for i:=0 to High(FTargetFixtures) do
      FTargetFixtures[i].StartFlameRGB(Frame_ColorPalette1.SelectedColor,
                                       TB2.Position/100,
                                       TB3.Position/TB3.Max,
                                       TB7.Position/TB7.Max);
   // if FTargetViewProjector.FShowLevel then
   //   FTargetViewProjector.Redraw;
  end;

  if Notebook1.PageIndex = Notebook1.IndexOf(PageAudioFollower) then
  begin
    snd := SoundManager.GetSoundByIndex(ComboBox1.ItemIndex);
    if snd = NIL then
    begin
      for i:=0 to High(FTargetFixtures) do
      begin
        FTargetFixtures[i].RGBColor := Frame_ColorPalette1.SelectedColor;
      end;
    end
    else begin
      if snd.State <> ALS_PLAYING then
        snd.Play;
      for i:=0 to High(FTargetFixtures) do
      begin
        FTargetFixtures[i].StartAudioFollowerRGB(snd.Tag,
                                                 Frame_ColorPalette1.SelectedColor,
                                                 TB4.Position/10,
                                                 TB6.Position/10);
      end;
    end;
    if FTargetViewProjector.FShowLevel then
          FTargetViewProjector.Redraw;
  end;

  if Notebook1.PageIndex = Notebook1.IndexOf(PageCopy) then
  begin
    sourceFix := FTargetViewProjector.FixtureSourceForCopy;
    for i:=0 to High(FTargetFixtures) do
      FTargetFixtures[i].StartCopyRGB(sourceFix);
  end;

  if Notebook1.PageIndex=Notebook1.IndexOf(PageStop) then
  begin

  end;

  if Notebook1.PageIndex=Notebook1.IndexOf(PageFlash) then
  begin

    vmax := FFrameTrackBar1.PercentValue;
    if RadioButton1.Checked then
      vmin := vmax
    else
      vmin := FFrameTrackBar1.IntervalMin;
    dmin := FloatSpinEdit2.Value;
    if RadioButton3.Checked then
      dmax := dmin
    else
      dmax := FloatSpinEdit3.Value;

    for i:=0 to High(FTargetFixtures) do
      FTargetFixtures[i].StartFlashRGB(Frame_ColorPalette1.SelectedColor, vmin, vmax, dmin, dmax);
  end;
end;

end.

