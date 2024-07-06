unit u_dmxtools_rgb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, Spin, ComCtrls, LCLTranslator, frame_velocity, lcl_utils,
  u_list_dmxuniverse, frame_color_palette, u_common,
  frame_fx_rgbchaser, frame_viewfixtureslist, frame_viewprojectors,
  frame_trackbar, frame_trackbar_customized;

type

  { TFormDMXRGBTools }

  TFormDMXRGBTools = class(TForm)
    BAdd1: TSpeedButton;
    BAdd2: TSpeedButton;
    BAdd3: TSpeedButton;
    BAdd4: TSpeedButton;
    BAdd5: TSpeedButton;
    BAdd6: TSpeedButton;
    BAdd7: TSpeedButton;
    BHelpAudioFollower: TSpeedButton;
    BHelpCopy: TSpeedButton;
    BHelpSimpleDimmer: TSpeedButton;
    BHelpStop: TSpeedButton;
    BHelpWaveDimmer: TSpeedButton;
    BHelpFlame: TSpeedButton;
    CB: TComboBox;
    ComboBox1: TComboBox;
    FloatSpinEdit1: TFloatSpinEdit;
    FloatSpinEdit2: TFloatSpinEdit;
    FloatSpinEdit3: TFloatSpinEdit;
    FloatSpinEdit4: TFloatSpinEdit;
    FloatSpinEdit5: TFloatSpinEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
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
    Label9: TLabel;
    Notebook1: TNotebook;
    Notebook2: TNotebook;
    PageWaveDimmer: TPage;
    PageSimpleDimmer: TPage;
    PageFlash: TPage;
    PageChaser: TPage;
    PageAudioFollower: TPage;
    PageCopy: TPage;
    PageDimmer: TPage;
    PageFlame: TPage;
    PageStop: TPage;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    SpeedButton1: TSpeedButton;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
    SpeedButton12: TSpeedButton;
    SpeedButton13: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    procedure BAdd1Click(Sender: TObject);
    procedure BHelpSimpleDimmerClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ProcessColorChangeEvent(Sender: TObject);
    procedure RadioButton1Change(Sender: TObject);
    procedure Shape3MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SpeedButton13Click(Sender: TObject);
    procedure SpeedButton7Click(Sender: TObject);
  private
    FFirstShown: boolean;
    Frame_Velocity1, Frame_Velocity2, Frame_Velocity3: TFrame_Velocity;
    FrameViewFixturesList1: TFrameViewFixturesList;
    FNoteBookManager, FNoteBookManager2: TNoteBookManager;
    FCheckedLabelManager: TCheckedLabelManager;
    FTargetFixtures: ArrayOfDmxFixtures;
    procedure GetTargetFixtures;
    procedure ApplyEffectOnTargetFixtures;
    procedure StopAll;
    procedure ProcessPageSelectionChange(Sender: TObject);
    procedure ProcessCopySelectionChange(Sender: TObject);
    procedure UpdateWidgets;
    procedure GenerateCmdForDimmerRGB;
    procedure GenerateCmdForDimmerWaveRGB;
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
    FrameTBFlameWait: TFrameTBDmxFlameRGBWait;
    FrameTBFlameAmplitude: TFrameTBDmxFlameRGBAmplitude;
    FrameTBFlameSoften: TFrameTBDmxFlameRGBSoften;
    FrameTBFollowGain: TFrameTBDmxAudioFollowerGain;
    FrameTBFollowSoften: TFrameTBDmxAudioFollowerSoften;
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
  u_helper, u_sequence_player, ALSound, u_add_action_dmx, u_mainform, form_help,
  BGRABitmapTypes;

{$R *.lfm}

{ TFormDMXRGBTools }

procedure TFormDMXRGBTools.FormCreate(Sender: TObject);
begin
  Frame_ColorPalette1 := TFrame_ColorPalette.Create(Self);
  Frame_ColorPalette1.Parent := Panel1;
  Frame_ColorPalette1.Align := alClient;
  Frame_ColorPalette1.OnChange := @ProcessColorChangeEvent;

  Frame_Velocity1 := TFrame_Velocity.Create(Self);
  Frame_Velocity1.Name := 'Frame_Velocity1';
  Frame_Velocity1.Parent := Panel2;
  Frame_Velocity1.Align := alClient;

  Frame_Velocity2 := TFrame_Velocity.Create(Self);
  Frame_Velocity2.Name := 'Frame_Velocity2';
  Frame_Velocity2.Parent := Panel12;
  Frame_Velocity2.Align := alClient;

  Frame_Velocity3 := TFrame_Velocity.Create(Self);
  Frame_Velocity3.Name := 'Frame_Velocity3';
  Frame_Velocity3.Parent := Panel13;
  Frame_Velocity3.Align := alClient;

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

  FNoteBookManager2 := TNoteBookManager.Create(Notebook2);
  with FNoteBookManager2 do
  begin
    SetActivatedColors($0003C4FC, clBlack);
    SetDeactivatedColors($00484848, $00EAEAEA);
    LinkButtonToPage(SpeedButton10, PageSimpleDimmer);
    LinkButtonToPage(SpeedButton12, PageWaveDimmer);
  end;


  FFrameTrackBar1 := TFrameTrackBar.Create(Self, Panel5);
  FFrameTrackBar1.Init(trHorizontal, False, True, True);
  FFrameTrackBar1.OnChange := @RadioButton1Change;
  FFrameTrackBar1.Enabled := False;

  FrameTBFlameWait := TFrameTBDmxFlameRGBWait.Create(Self, Panel9);
  FrameTBFlameWait.Init(trHorizontal, False, False, False);
  FrameTBFlameWait.Value := 0.125;
  FrameTBFlameWait.OnChange := @ProcessColorChangeEvent;

  FrameTBFlameAmplitude := TFrameTBDmxFlameRGBAmplitude.Create(Self, Panel8);
  FrameTBFlameAmplitude.Init(trHorizontal, False, False, False);
  FrameTBFlameAmplitude.Value := 0.5;
  FrameTBFlameAmplitude.OnChange := @ProcessColorChangeEvent;

  FrameTBFlameSoften := TFrameTBDmxFlameRGBSoften.Create(Self, Panel7);
  FrameTBFlameSoften.Init(trHorizontal, False, False, False);
  FrameTBFlameSoften.Value := 1.0;
  FrameTBFlameSoften.OnChange := @ProcessColorChangeEvent;

  FrameTBFollowGain := TFrameTBDmxAudioFollowerGain.Create(Self, Panel11);
  FrameTBFollowGain.Init(trHorizontal, False, False, False);
  FrameTBFollowGain.Value := 0.0;
  FrameTBFollowGain.OnChange := @ProcessColorChangeEvent;

  FrameTBFollowSoften := TFrameTBDmxAudioFollowerSoften.Create(Self, Panel10);
  FrameTBFollowSoften.Init(trHorizontal, False, False, False);
  FrameTBFollowSoften.Value := 0.5;
  FrameTBFollowSoften.OnChange := @ProcessColorChangeEvent;


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
    if Notebook2.PageIndex = Notebook2.IndexOf(pageSimpleDimmer) then GenerateCmdForDimmerRGB
      else GenerateCmdForDimmerWaveRGB;

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

procedure TFormDMXRGBTools.BHelpSimpleDimmerClick(Sender: TObject);
begin
  if Sender = BHelpSimpleDimmer then _ShowHelp(HelpRGBSimpleDimmer, BHelpSimpleDimmer);
  if Sender = BHelpWaveDimmer then _ShowHelp(HelpRGBWaveDimmer, BHelpWaveDimmer);
  if Sender = BHelpFlame then _ShowHelp(HelpRGBFlame, BHelpFlame);
  if Sender = BHelpAudioFollower then _ShowHelp(HelpRGBAudioFollower, BHelpAudioFollower);
  if Sender = BHelpCopy then _ShowHelp(HelpRGBCopy, BHelpCopy);
  if Sender = BHelpStop then _ShowHelp(HelpRGBStop, BHelpStop);
end;

procedure TFormDMXRGBTools.FormDestroy(Sender: TObject);
begin
  FNoteBookManager.Free;
  FNoteBookManager2.Free;
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
    FNoteBookManager2.ActivePage(PageSimpleDimmer);
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
  Frame_ColorPalette1.ReloadPresets;
  Frame_Velocity1.UpdateList;
  Frame_Velocity2.UpdateList;
  Frame_Velocity3.UpdateList;

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

procedure TFormDMXRGBTools.Shape3MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Sender = Shape3 then Shape3.Brush.Color := Frame_ColorPalette1.SelectedColor;
  if Sender = Shape4 then Shape4.Brush.Color := Frame_ColorPalette1.SelectedColor;
end;

procedure TFormDMXRGBTools.SpeedButton13Click(Sender: TObject);
var i: integer;
begin
  GetTargetFixtures;
  for i:=0 to High(FTargetFixtures) do
    FTargetFixtures[i].StartWaveRGB(Shape3.Brush.Color, FloatSpinEdit4.Value, Frame_Velocity2.SelectedCurveID,
                                    Shape4.Brush.Color, FloatSpinEdit5.Value, Frame_Velocity3.SelectedCurveID);
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
    ApplyEffectOnTargetFixtures; // must be before the next line !
    FTargetViewProjector.FrameViewDMXCursors1.SetSourceFixtureForRGBCopy(FrameViewFixturesList1.Selected[0]);
  end;
end;

procedure TFormDMXRGBTools.UpdateWidgets;
begin
  // audio follower
  Label8.Caption := FrameTBFollowGain.GetLegend;
  Label10.Caption := FrameTBFollowSoften.GetLegend;
  // flame
  Label12.Caption := FrameTBFlameWait.GetLegend;
  Label13.Caption := FrameTBFlameAmplitude.GetLegend;
  Label15.Caption := FrameTBFlameSoften.GetLegend;
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

procedure TFormDMXRGBTools.GenerateCmdForDimmerWaveRGB;
var i: integer;
  fix: TDMXFixture;
begin
  FCmd := CmdTitleDMXWaveRGB(Shape3.Brush.Color, FloatSpinEdit4.Value, Frame_Velocity2.SelectedCurveID,
                             Shape4.Brush.Color, FloatSpinEdit5.Value, Frame_Velocity3.SelectedCurveID);
  for i:=0 to High(FTargetFixtures) do
  begin
    fix := FTargetFixtures[i];
    if fix.HasRGBChannel then
      FCmd.ConcatCmd(CmdDMXWaveRGB(fix.Universe.ID,
                                   fix.ID,
                                   Shape3.Brush.Color, FloatSpinEdit4.Value, Frame_Velocity2.SelectedCurveID,
                                   Shape4.Brush.Color, FloatSpinEdit5.Value, Frame_Velocity3.SelectedCurveID));
  end;

  FShortReadable := SDMXWaveRGB+' '+SOn_+' ';
  if Length(FTargetFixtures) > 1 then
    FShortReadable := FShortReadable + SMultiple
  else if fix.Description <> '' then FShortReadable := FShortReadable + fix.Description
         else FShortReadable := FShortReadable + fix.Name;

  FDuration := FloatSpinEdit4.Value + FloatSpinEdit5.Value;
end;

procedure TFormDMXRGBTools.GenerateCmdForFlameRGB;
var i: integer;
  fix: TDMXFixture;
begin
  FCmd := CmdTitleDMXFlameRGB(Frame_ColorPalette1.SelectedColor,
                             FrameTBFlameWait.Value,
                             FrameTBFlameAmplitude.Value,
                             FrameTBFlameSoften.Value);

  for i:=0 to High(FTargetFixtures) do
  begin
    fix := FTargetFixtures[i];
    if fix.HasRGBChannel then
      FCmd.ConcatCmd(CmdDMXFlameRGB(fix.Universe.ID,
                     fix.ID,
                     Frame_ColorPalette1.SelectedColor,
                     FrameTBFlameWait.Value,
                     FrameTBFlameAmplitude.Value,
                     FrameTBFlameSoften.Value));
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
                                      FrameTBFollowGain.Value,
                                      FrameTBFollowSoften.Value);

  for i:=0 to High(FTargetFixtures) do
  begin
    fix := FTargetFixtures[i];
    if fix.HasRGBChannel then
      FCmd.ConcatCmd(CmdDMXAudioFollowerRGB(fix.Universe.ID,
                     fix.ID,
                     snd.Tag,
                     Frame_ColorPalette1.SelectedColor,
                     FrameTBFollowGain.Value,
                     FrameTBFollowSoften.Value));
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
  vmax := FFrameTrackBar1.PercentMax;
  if RadioButton1.Checked then
    vmin := vmax
  else
    vmin := FFrameTrackBar1.PercentMin;

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
        BAdd7.Caption := SCreateSequence;
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
        BAdd7.Caption := SAdd;
        FTargetViewProjector := FormAddDMXAction.FrameViewProjector1;
        FrameViewFixturesList1.FTargetViewProjector := FTargetViewProjector;
      end;
  end;

  Label1.Caption := SDuration;
  Label17.Caption := SVelocity;
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
  SpeedButton11.Hint := SPreview;
  SpeedButton10.Caption := SSimple;
  SpeedButton12.Caption := SWave;
  Label11.Caption := SColor+' 1';
  Label9.Caption := SDuration;
  Label16.Caption := SVelocity;
  Label21.Caption := SColor+' 2';
  Label24.Caption := SDuration;
  Label23.Caption := SVelocity;
  SpeedButton13.Hint := SPreview;
  Shape3.Hint := SClickToCaptureCurrentColor;
  Shape4.Hint := SClickToCaptureCurrentColor;
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

  if Notebook1.PageIndex = Notebook1.IndexOf(PageDimmer) then
  begin
    for i:=0 to High(FTargetFixtures) do begin
      FTargetFixtures[i].RGBColor := Frame_ColorPalette1.SelectedColor;

      if FTargetFixtures[i].IsVisibleOnViewCursor then
        FTargetViewProjector.FrameViewDMXCursors1.RedrawRGBChannelsOnFixture(FTargetFixtures[i]);
    end;
    FTargetViewProjector.FrameViewDMXCursors1.ForceRepaint;
    if FTargetViewProjector.FShowLevel then FTargetViewProjector.ForceRepaint; //.Redraw;
  end;

  if Notebook1.PageIndex = Notebook1.IndexOf(PageFlame) then
  begin
    for i:=0 to High(FTargetFixtures) do
      FTargetFixtures[i].StartFlameRGB(Frame_ColorPalette1.SelectedColor,
                                       FrameTBFlameWait.Value,
                                       FrameTBFlameAmplitude.Value,
                                       FrameTBFlameSoften.Value);
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
                                                 FrameTBFollowGain.Value,
                                                 FrameTBFollowSoften.Value);
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

    vmax := FFrameTrackBar1.PercentMax;
    if RadioButton1.Checked then
      vmin := vmax
    else
      vmin := FFrameTrackBar1.PercentMin;
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

