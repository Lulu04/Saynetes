unit frame_viewprojectors;

{$mode ObjFPC}{$H+}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, LCLType, StdCtrls, Menus,
  Buttons, Graphics, frame_bglvirtualscreen_sequencer, Types, LCLTranslator,
  BGRABitmap, BGRABitmapTypes, BGLVirtualScreen, BGRAOpenGL, BGRAOpenGLType,
  BGRASVG, BGRAFontGL,
  u_list_dmxuniverse, u_dmx_util, u_common,
  frame_viewdmxcursors, frame_velocity, u_notebook_util, frame_viewfixtureinfo;


const
  DEFAULT_CURSOR=crDefault;
  SHIFTVIEW_CURSOR=crSize;
  RECTSELECTION_CURSOR=crCross;
  OVERITEM_CURSOR=crHandPoint;
  ROTATION_CURSOR=crHSplit;
  ZOOM_CURSOR=crSizeWE;

  FIXTURE_MINZOOM_VALUE=0.5;
  FIXTURE_MAXZOOM_VALUE=2;

type

  TViewDMXProjectorsMouseState=(msReleased,       // no action
                                msAdding,         // adding a fixture from library
                                msMoving,         // moving the selection
                                msRotate,         // rotate the selection
                                msShiftView,      // shift the view
                                msRectSelection); // rectangular selection

  TViewDMXProjectorSelectionMode=(smNeutral,      // when no selection by user
                                  smMoveItem,     // item selected will be moved
                                  smRotationItem, // item selected will be rotated
                                  smZoomItem);    // item selected will be zoomed

  { TFixtureToAdd }

  TFixtureToAdd = record
    FixtureLocation: TFixtureLibraryLocation;
    TargetUniverse: TDMXUniverse;
    procedure InitEmpty;
    function HaveReferenceToFixture: boolean;
  end;

  { TFrameViewProjector }

  TFrameViewProjector = class(TFrame)
    BAddDMX: TSpeedButton;
    BGLVirtualScreen1: TBGLVirtualScreen;
    BShowDMXAdress: TSpeedButton;
    BShowInfo: TSpeedButton;
    BShowLevels: TSpeedButton;
    BShowRGBSymbol: TSpeedButton;
    BZoomAll: TSpeedButton;
    ComboBox1: TComboBox;
    MenuItem1: TMenuItem;
    MenuItem3: TMenuItem;
    MICreateRGBGroup: TMenuItem;
    MIDelete: TMenuItem;
    MIHFlip: TMenuItem;
    MILockFixture: TMenuItem;
    MIRotation: TMenuItem;
    MIUnlockFixture: TMenuItem;
    MIVFlip: TMenuItem;
    MIZoom: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel9: TPanel;
    PopFixture_ModeEditAction: TPopupMenu;
    PopFixture_ModePrepaDMX: TPopupMenu;
    SpeedButton5: TSpeedButton;
    Splitter1: TSplitter;
    procedure BAddDMXClick(Sender: TObject);
    procedure BGLVirtualScreen1DragOver(Sender, Source: TObject; {%H-}X, {%H-}Y: Integer;
      {%H-}State: TDragState; var Accept: Boolean);
    procedure BGLVirtualScreen1LoadTextures(Sender: TObject; {%H-}BGLContext: TBGLContext);
    procedure BGLVirtualScreen1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BGLVirtualScreen1MouseEnter(Sender: TObject);
    procedure BGLVirtualScreen1MouseLeave(Sender: TObject);
    procedure BGLVirtualScreen1MouseMove(Sender: TObject; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure BGLVirtualScreen1MouseUp(Sender: TObject; Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure BGLVirtualScreen1MouseWheel(Sender: TObject; {%H-}Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure BGLVirtualScreen1Redraw(Sender: TObject; BGLContext: TBGLContext);
    procedure BGLVirtualScreen1Resize(Sender: TObject);
    procedure BGLVirtualScreen1UnloadTextures(Sender: TObject; {%H-}BGLContext: TBGLContext);
    procedure BShowDMXAdressClick(Sender: TObject);
    procedure BZoomAllClick(Sender: TObject);
    procedure ComboBox1Select(Sender: TObject);
    procedure MICreateRGBGroupClick(Sender: TObject);
    procedure MIDeleteClick(Sender: TObject);
    procedure MIHFlipClick(Sender: TObject);
    procedure MILockFixtureClick(Sender: TObject);
    procedure MIRotationClick(Sender: TObject);
    procedure MIUnlockFixtureClick(Sender: TObject);
    procedure MIVFlipClick(Sender: TObject);
    procedure MIZoomClick(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
    procedure PopFixture_ModeEditActionPopup(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure Splitter1Moved(Sender: TObject);
  private
    FViewOrigin: TPointF;
    FZoom: single;
    FColorBackground,
    FColorSelection: TBGRAPixel;
    FState: TViewDMXProjectorsMouseState;
    FSelectionMode: TViewDMXProjectorSelectionMode;
    FSelectionRect,
    FTotalViewRect: TRectF;
    FNeedCenterView: boolean;
    FWorkingFixture: TDMXFixture;
    FMouseIsOver: boolean;
    function ModePrepaDMX: boolean;
    function ModeEditAction: boolean;
    function ModeMainDMX: boolean;
    function MatrixView: TAffineMatrix;
    procedure ComputeTotalViewRect;
    procedure SetZoom(AValue: single);
    function FixtureUnderMouse(X, Y: integer): TDMXFixture;
    function ClientToWord(pt: TPoint): TPointF;
    function ClientToWord(r: TRect): TRectF;
    function WordToClient(pt: TPointF): TPoint;
    function WordToClient(r: TRectF): TRect;
    function ClientPointIsInFixtureCircle({%H-}X, {%H-}Y: integer): boolean;
    procedure LoopRotateFixture;
    procedure LoopZoomFixture;
    procedure LoopMoveFixture;
    procedure LoopShiftView;
    procedure LoopRectangularSelection;
  private
    FFixtureSourceForCopy: TDMXFixture;
    FSelected: ArrayOfDmxFixtures;//array of TDMXFixture;
    procedure UpdateSelected;
    function SomeSelectedHaveRGB: boolean;
    function AllSelectedHaveRGB: boolean;
    procedure InternalAddToSelected(aFix: TDMXFixture);
    function GetSelectedCount: integer;
    procedure InternalSel_None;
    procedure AddFixtureToOtherWindows(aFix: TDMXFixture);
    procedure RemoveFixtureToOtherWindows(aFix: TDMXFixture);
    procedure ClearSelectionOnOtherWindows;

  private
    FOpenGLObjectsNeedToBeReconstruct: boolean;
    FTextureSeats,
    FTextureStage,
    FLockTexture: IBGLTexture;
    FTextures: array of IBGLTexture;
    FDMXAdressFont: IBGLRenderedFont;
    procedure CreateFixturesTextures;
    procedure CreateDMXAdressFont;
    procedure CreateOpenGlObjects;
    procedure DeleteOpenGLObjects;
    procedure CreateSeatsTexture;
    procedure CreateStageTexture;
  private
    function TextureFor(aFt: TFixtureType): IBGLTexture;
    function TextureHalfSize(aFt: TFixtureType): TPointF; overload;
    function TextureHalfSize(aTex: IBGLTexture): TPointF; overload;
    function GetWordFixtureArea(aFix: TDMXFixture): TRectF;
  private
    FFixtureToAdd: TFixtureToAdd;
    FLibraryFixtureToAdd: TLibraryFixture;
    procedure DoAddFixture(X, Y: integer);
  private
    FOnAddCmd: TNotifyEvent;
    FCmd: TCmdList;
    FShortReadableString: string;
    FCmdDuration: single;
    procedure GetUniverseIndexes(out firstuni, lastuni: integer);
  private
    FTargetFixtureInViewCursor: TDMXFixture;
    FToogleSpeedButtonManager: TToggleSpeedButtonManager;
    procedure SetFixtureSourceForCopy(AValue: TDMXFixture);
  private
    FOnDeleteFixture: TNotifyEvent;
    FOnFixtureSelectionChange: TNotifyEvent;
    procedure DoFixtureSelectionChange;
    procedure DoDeleteFixtureEvent;
    procedure ShowPanelFixtureInfo;
  private
    FInvalidateAlreadySent: boolean;
    FGUIMode: TGUIMode;
    procedure SetGUIMode(AValue: TGUIMode);
  public
    FShowLevel,
    FShowRGBSymbol,
    FShowDMXAdress,
    FShowFixtureInfo: boolean;
  public
    FrameFixtureInfo1: TFrameFixtureInfo;
    FrameViewDMXCursors1: TFrameViewDMXCursors;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure ProcessViewDMXCursorsMouseOverFixtureEvent(Sender: TObject; aFixture: TDMXFixture);

    function MouseIsOver: boolean;
    procedure ProcessKeyDown({%H-}Key: word; {%H-}Shift: TShiftState);
    procedure ProcessKeyUp(var Key: Word; Shift: TShiftState);

    procedure EraseBackground({%H-}DC: HDC); override;
    procedure Redraw;

    procedure ForceReconstructOpenGLObjects;
    procedure FreeOpenGLTextures;

    procedure UpdateComboBoxLanguage;

    procedure UpdateEditMode;

    procedure FillComboBoxUniverseToShow;

    procedure FixtureFilenameToAdd(const aFixtureLocation: TFixtureLibraryLocation; aTargetUniverse: TDMXUniverse);
    procedure ExitAddMode;
    function InAddMode: boolean;

    procedure AddToSelected(aFix: TDMXFixture);
    procedure SetSelectionFromUniverseManager;
    procedure Sel_None;
    procedure Sel_All;
    procedure Sel_ShiftPosition(delta: TPointF);
    procedure Sel_Rotate(delta: single);
    procedure Sel_Zoom(delta: single);
    procedure Sel_ToogleFlipH;
    procedure Sel_ToogleFlipV;

    procedure View_Center;
    procedure HideToolsWindows;
    procedure HidePanelFixtureInfo;

    function ChannelsLevelAreVisible: boolean;

    procedure SaveProjectOptionsTo(t: TStringList);
    procedure LoadProjectOptionsFrom(t: TStringList);

    property Zoom: single read FZoom write SetZoom;

    property FixtureSourceForCopy: TDMXFixture read FFixtureSourceForCopy write SetFixtureSourceForCopy;

  public
    procedure RegisterCmd(const aCmd: TSingleCmd; const aShortReadable: string; aDuration: single);
    property OnAddCmd: TNotifyEvent read FOnAddCmd write FOnAddCmd;

    property SelectedCount: integer read GetSelectedCount;
    property Selected: ArrayOfDmxFixtures read FSelected;
    property OnFixtureSelectionChange: TNotifyEvent read FOnFixtureSelectionChange write FOnFixtureSelectionChange;
    property OnDeleteFixture: TNotifyEvent read FOnDeleteFixture write FOnDeleteFixture;
    //
    property Cmds: TCmdList read FCmd;
    // the text that resume the cmd->used to name the action or the top when it is a single cmd
    property ShortReadableString: string read FShortReadableString;
    property CmdDuration: single read FCmdDuration;

    property GUIMode: TGUIMode read FGUIMode write SetGUIMode;

    property ColorBackground: TBGRAPixel read FColorBackground write FColorBackground;
  end;

implementation
uses u_project_manager, u_userdialogs, u_resource_string, u_dmxtools_group,
  u_list_sequence, u_sequence_player, u_dmxtools_rgb, u_askifshiftadress,
  u_mainform, u_logfile, u_program_options, u_apputils, Math,
  ComCtrls, Dialogs, BGRATransform, utilitaire_bgrabitmap, PropertyUtils;


{$R *.lfm}

{ TFixtureToAdd }

procedure TFixtureToAdd.InitEmpty;
begin
  FixtureLocation.InitDefault;
  TargetUniverse := NIL;
end;

function TFixtureToAdd.HaveReferenceToFixture: boolean;
begin
  Result := FixtureLocation.HaveFixtureAndModeOk and (TargetUniverse <> NIL);
end;

{ TFrameViewProjector }

constructor TFrameViewProjector.Create(TheOwner: TComponent);
begin
  Inherited Create(TheOwner);
  {$ifdef MSWINDOWS}
  BGLVirtualScreen1.MultiSampling := 2;
  {$endif}

  FNeedCenterView := TRUE;
  FZoom := 0.62;
  FState := msReleased;
  FSelectionMode := smNeutral;
  SetLength(FSelected, 0);

  FrameViewDMXCursors1 := TFrameViewDMXCursors.Create(Self);
  FrameViewDMXCursors1.Parent := Panel2;
  FrameViewDMXCursors1.Align := alClient;
  FrameViewDMXCursors1.OnMouseOverFixture := @ProcessViewDMXCursorsMouseOverFixtureEvent;
  FrameViewDMXCursors1.ParentViewProjector := Self;

  FrameFixtureInfo1 := TFrameFixtureInfo.Create(Self);
  FrameFixtureInfo1.Parent := Panel9;
  FrameFixtureInfo1.FTargetViewProjector := Self;
  Panel9.Height := FrameFixtureInfo1.Height;
  FrameFixtureInfo1.Align := alClient;
  Panel9.Visible := False;

  FToogleSpeedButtonManager := TToggleSpeedButtonManager.Create;
  FToogleSpeedButtonManager.ToggleType := tsbLikeCheckBox;
  FToogleSpeedButtonManager.SetActivatedColors($0003C4FC, $00272727);
  FToogleSpeedButtonManager.SetDeactivatedColors($00004B62, $00EAEAEA);
  FToogleSpeedButtonManager.Add(BShowRGBSymbol, FALSE);
  FToogleSpeedButtonManager.Add(BShowDMXAdress, FALSE);
  FToogleSpeedButtonManager.Add(BShowLevels, TRUE);
  FToogleSpeedButtonManager.Add(BShowInfo, FALSE);

  FColorBackground := BGRA(21,21,21); // BGRA(51,51,51);
  FColorSelection := BGRA(255,255,200);

end;

destructor TFrameViewProjector.Destroy;
begin
  FToogleSpeedButtonManager.Free;
  inherited Destroy;
end;

procedure TFrameViewProjector.ProcessViewDMXCursorsMouseOverFixtureEvent(
  Sender: TObject; aFixture: TDMXFixture);
begin
  if FTargetFixtureInViewCursor <> aFixture then
  begin
    FTargetFixtureInViewCursor := aFixture;
    Redraw;
  end;
end;

function TFrameViewProjector.MouseIsOver: boolean;
begin
  Result := FMouseIsOver;
end;

procedure TFrameViewProjector.ProcessKeyDown(Key: word; Shift: TShiftState);
begin
end;

procedure TFrameViewProjector.ProcessKeyUp(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_A:
      if ssCtrl in Shift then
      begin
        Sel_All;
        Redraw;
        Key := VK_UNKNOWN;
      end;

    VK_DELETE:
      if FGUIMode = guiPrepaDMX then
        MIDeleteClick(NIL);

    VK_ESCAPE:
      begin
       if InAddMode then
         ExitAddMode
       else if SelectedCount > 0 then
       begin
         Sel_None;
         Redraw;
       end
       else if FGUIMode = guiEditSequence then
         Hide;
      end;
  end;//case
end;

procedure TFrameViewProjector.EraseBackground(DC: HDC);
begin
end;

procedure TFrameViewProjector.Redraw;
begin
  if not FInvalidateAlreadySent then
  begin
    BGLVirtualScreen1.Invalidate;
    FInvalidateAlreadySent := TRUE;
  end;
end;

procedure TFrameViewProjector.ForceReconstructOpenGLObjects;
begin
  FOpenGLObjectsNeedToBeReconstruct := True;
  Redraw;
end;

procedure TFrameViewProjector.FreeOpenGLTextures;
begin
  BGLVirtualScreen1.MakeCurrent;
  BGLVirtualScreen1.UnloadTextures;
end;

procedure TFrameViewProjector.UpdateComboBoxLanguage;
begin
  MIDelete.Caption := SDelete;
end;

procedure TFrameViewProjector.UpdateEditMode;
begin
  BAddDMX.Visible := Project.Options.EditMode;

  FrameViewDMXCursors1.UpdateEditMode;
end;

procedure TFrameViewProjector.FillComboBoxUniverseToShow;
var i: integer;
begin
  i := ComboBox1.ItemIndex;
  ComboBox1.Clear;
  ComboBox1.Items.Add(SAll);
  for i:=0 to UniverseManager.Count-1 do
    ComboBox1.Items.Add(SOnly+UniverseManager.Universes[i].Name);
  ComboBox1.ItemIndex := 0;
  ComboBox1.Enabled := UniverseManager.Count>0;

  UpdateComboBoxLanguage;
end;

procedure TFrameViewProjector.FixtureFilenameToAdd(const aFixtureLocation: TFixtureLibraryLocation;
  aTargetUniverse: TDMXUniverse);
begin
  FFixtureToAdd.InitEmpty;
  FState := msReleased;
  if aTargetUniverse = NIL then exit;
  if not FLibraryFixtureToAdd.LoadFrom(aFixtureLocation) then exit;

  aFixtureLocation.CopyTo(FFixtureToAdd.FixtureLocation);
  FFixtureToAdd.TargetUniverse := aTargetUniverse;
end;

procedure TFrameViewProjector.ExitAddMode;
var fixLocation: TFixtureLibraryLocation;
begin
  fixLocation.InitDefault;
  FixtureFilenameToAdd(fixLocation, NIL);
  Redraw;
end;

function TFrameViewProjector.InAddMode: boolean;
begin
  Result := FFixtureToAdd.HaveReferenceToFixture;
end;

procedure TFrameViewProjector.AddToSelected(aFix: TDMXFixture);
begin
  aFix.Selected := TRUE;
  InternalAddToSelected(aFix);
  FrameViewDMXCursors1.Add(aFix, True);
end;

procedure TFrameViewProjector.SetSelectionFromUniverseManager;
var A: ArrayOfDmxFixtures;
  i: integer;
begin
  A := NIL;
  UniverseManager.GetSelectedFixtures(A);
  if length(A) = 0 then
    Sel_None
  else begin
    for i:=0 to High(A) do
      AddToSelected(A[i]);
  end;

  Redraw;
  FrameViewDMXCursors1.RedrawAll;
end;

procedure TFrameViewProjector.Sel_None;
begin
  InternalSel_None;
  ClearSelectionOnOtherWindows;
end;

procedure TFrameViewProjector.Sel_All;
var i: integer;
begin
  UniverseManager.Sel_All;
  UpdateSelected;

  FrameViewDMXCursors1.Clear;
  for i:=0 to High(FSelected) do
    FrameViewDMXCursors1.Add(FSelected[i], False);
  FrameViewDMXCursors1.UpdateView;

  if FSelectionMode = smNeutral
   then FSelectionMode := smMoveItem;
end;

procedure TFrameViewProjector.Sel_ShiftPosition(delta: TPointF);
var i: integer;
begin
  for i:=0 to High(FSelected) do
    FSelected[i].ScreenPos:=FSelected[i].ScreenPos+delta;
end;

procedure TFrameViewProjector.Sel_Rotate(delta: single);
var i: integer;
begin
  for i:=0 to High(FSelected) do begin
    FSelected[i].Angle:=FSelected[i].Angle+delta;
  end;
end;

procedure TFrameViewProjector.Sel_Zoom(delta: single);
var i: integer;
begin
  for i:=0 to High(FSelected) do
    FSelected[i].Zoom := EnsureRange(FSelected[i].Zoom+delta, FIXTURE_MINZOOM_VALUE, FIXTURE_MAXZOOM_VALUE);
end;

procedure TFrameViewProjector.Sel_ToogleFlipH;
var i: integer;
  flag: boolean;
begin
  flag:=FALSE;
  for i:=0 to High(FSelected) do begin
    FSelected[i].FlipH := not FSelected[i].FlipH;
    flag := TRUE;
  end;
  if flag then Project.SetModified;
end;

procedure TFrameViewProjector.Sel_ToogleFlipV;
var i: integer;
  flag: boolean;
begin
  flag:=FALSE;
  for i:=0 to High(FSelected) do begin
    FSelected[i].FlipV:=not FSelected[i].FlipV;
    flag:=TRUE;
  end;
  if flag then Project.SetModified;
end;

procedure TFrameViewProjector.View_Center;
begin
  FNeedCenterView := TRUE;
  Redraw;
end;

procedure TFrameViewProjector.HideToolsWindows;
begin
  FrameViewDMXCursors1.HideToolsWindows;
end;

procedure TFrameViewProjector.HidePanelFixtureInfo;
begin
  Panel9.Visible := False;
end;

function TFrameViewProjector.ChannelsLevelAreVisible: boolean;
begin
  Result := FToogleSpeedButtonManager.Checked[BShowLevels];
end;

const OPTION_PROJECTOR_VIEW_HEADER='[OPTIONS PROJECTOR VIEW]';
procedure TFrameViewProjector.SaveProjectOptionsTo(t: TStringList);
var prop: TProperties;
begin
  prop.Init('|');
  prop.Add('ShowLevel', FShowLevel);
  prop.Add('ShowRGBSymbol', FShowRGBSymbol);
  prop.Add('ShowAdress', FShowDMXAdress);
  prop.Add('ShowFixtureInfo', FShowFixtureInfo);
  prop.Add('SplitterTop', Splitter1.Top);
  t.Add(OPTION_PROJECTOR_VIEW_HEADER);
  t.Add(prop.PackedProperty);
end;

procedure TFrameViewProjector.LoadProjectOptionsFrom(t: TStringList);
var prop: TProperties;
  bv: boolean;
  k: Integer;
begin
  k := t.IndexOf(OPTION_PROJECTOR_VIEW_HEADER);
  if (k = -1) or (k = t.Count-1) then
    prop.SetEmpty
  else
    prop.Split(t.Strings[k+1], '|');
  bv := False; // avoid hint

  prop.BooleanValueOf('ShowLevel', bv, True);
  FToogleSpeedButtonManager.SetState(BShowLevels, bv);
  FShowLevel := bv;

  prop.BooleanValueOf('ShowRGBSymbol', bv, True);
  FToogleSpeedButtonManager.SetState(BShowRGBSymbol, bv);
  FShowRGBSymbol := bv;

  prop.BooleanValueOf('ShowAdress', bv, True);
  FToogleSpeedButtonManager.SetState(BShowDMXAdress, bv);
  FShowDMXAdress := bv;

  prop.BooleanValueOf('ShowFixtureInfo', bv, True);
  FToogleSpeedButtonManager.SetState(BShowInfo, bv);
  FShowFixtureInfo := bv;

  prop.IntegerValueOf('SplitterTop', k, Round(Height*0.6));
  Splitter1.Top := k;
end;

procedure TFrameViewProjector.RegisterCmd(const aCmd: TSingleCmd;
  const aShortReadable: string; aDuration: single);
begin
  if GUIMode = guiMainDMX then
    FormMain.FrameMainSequence1.AddDMXSequenceFromProjectorView(Self, aCmd, aShortReadable, aDuration)
  else
  if FOnAddCmd <> NIL then
  begin
    FCmd := aCmd;   // the action
    FShortReadableString := aShortReadable;   // the text that resume the action
    FCmdDuration := aDuration;
    FOnAddCmd(Self);

    if FGUIMode = guiEditSequence then
    begin
      HideToolsWindows;
    end;
  end;
end;

procedure TFrameViewProjector.BGLVirtualScreen1DragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if (Source is TTreeView) and FFixtureToAdd.HaveReferenceToFixture and
     ModePrepaDMX then
  begin
    Sel_None;
    FState := msAdding;
    Accept := TRUE;
    Redraw;
  end
  else Accept := FALSE;
end;

procedure TFrameViewProjector.BAddDMXClick(Sender: TObject);
begin
  Screen.BeginWaitCursor;
  try
    BAddDMX.Enabled := False;
    Sequences.StopAll;
    SeqPLayer.StopPreview;

    UniverseManager.BlackOut;
    UniverseManager.Sel_None;
    Sel_None;

    GUIMode := guiPrepaDMX;
    // disable window button on main form
    FormMain.UpdateWidgetState;
    FormMain.FrameMainAddFixture1.OnShow;

    Redraw;
  Finally
    Screen.EndWaitCursor;
  end;
end;

procedure TFrameViewProjector.BGLVirtualScreen1LoadTextures(Sender: TObject;
  BGLContext: TBGLContext);
begin
  CreateOpenGlObjects;
end;

procedure TFrameViewProjector.BGLVirtualScreen1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var fix: TDMXFixture;
begin
  if FState = msReleased then
  begin
    fix := FixtureUnderMouse(X,Y);

    // fixture selection/rotation and drag
    if (fix <> NIL) and (button = mbLeft) then
    begin
      if ssShift in shift then
      begin
        fix.Selected := not fix.Selected;
        if fix.Selected then
        begin
          FrameViewDMXCursors1.Add(fix, True);
          AddFixtureToOtherWindows(fix);
        end
        else
          RemoveFixtureToOtherWindows(fix);
      end
      else if not fix.Selected then
      begin
        Sel_None;
        FSelectionMode := smMoveItem;
        fix.Selected := TRUE;
        FrameViewDMXCursors1.Add(fix, True);
        AddFixtureToOtherWindows(fix);
      end;
      UpdateSelected;
      if GetSelectedCount = 0 then
        FSelectionMode := smNeutral
      else if (FSelectionMode = smNeutral) and ModePrepaDMX then
        FSelectionMode := smMoveItem;
      Redraw;
      if ModePrepaDMX and(FSelectionMode = smMoveItem) then
      begin
        LoopMoveFixture;
      end;
      exit;
    end;

    // start rotation
    if (fix = NIL) and (Button = mbLeft) and (FSelectionMode = smRotationItem) and ModePrepaDMX then
    begin
      LoopRotateFixture;
      exit;
    end;

    // start zoom
    if (fix = NIL) and (Button = mbLeft) and (FSelectionMode = smZoomItem) and ModePrepaDMX then
    begin
      LoopZoomFixture;
      exit;
    end;

    // rectangular selection
    if (fix = NIL) and (Button = mbLeft) then
    begin

      LoopRectangularSelection;
      exit;
    end;

    // shift the view
    if (fix = NIL) and (Button = mbRight) then
    begin
      LoopShiftView;
      exit;
    end;

    // fixture popup menu
    if (fix <> NIL) and (button = mbRight) then
    begin
      if not fix.Selected then
      begin
        Sel_None;
        fix.Selected := TRUE;
        UpdateSelected;
        FrameViewDMXCursors1.Add(fix, True);
        FSelectionMode := smMoveItem;
        Redraw;
      end;
      FWorkingFixture := fix;
      if ModePrepaDMX then
        PopFixture_ModePrepaDMX.PopUp;
      if ModeEditAction or ModeMainDMX then
        PopFixture_ModeEditAction.PopUp;
    end;
  end;
end;

procedure TFrameViewProjector.BGLVirtualScreen1MouseEnter(Sender: TObject);
begin
  FMouseIsOver := True;
end;

procedure TFrameViewProjector.BGLVirtualScreen1MouseLeave(Sender: TObject);
begin
  if InAddMode then
    ExitAddMode;

  FState := msReleased;
  FMouseIsOver := False;
end;

procedure TFrameViewProjector.BGLVirtualScreen1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var fix: TDMXFixture;
begin
  if FState = msReleased then
  begin
    fix := FixtureUnderMouse(X, Y);
    if fix <> NIL then
      BGLVirtualScreen1.Cursor := OVERITEM_CURSOR
    else
      case FSelectionMode of
       //smMoveItem: BGLVirtualScreen1.Cursor:=OVERITEM_CURSOR;
       smRotationItem: BGLVirtualScreen1.Cursor := ROTATION_CURSOR;
       smZoomItem: BGLVirtualScreen1.Cursor := ZOOM_CURSOR;
       else BGLVirtualScreen1.Cursor := DEFAULT_CURSOR;
     end;
  end;

  if FState = msAdding then
  begin
     Redraw;
  end;
end;

procedure TFrameViewProjector.BGLVirtualScreen1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var fix: TDMXFixture;
begin
  fix := FixtureUnderMouse(X, Y);

  if (FState = msAdding) and (Button = mbLeft) then begin
    DoAddFixture(X, Y);
    exit;
  end;
  if (FState = msAdding) and (Button = mbRight) then begin
    ExitAddMode;
    Redraw;
  end;

  if (FState = msReleased) and (fix = NIL) and (Button = mbLeft) then begin
    Sel_None;
    Redraw;
  end;

  if (SelectedCount = 1) and (Button = mbLeft) and ModePrepaDMX and (fix <> NIL) then begin
    // make visible the fixture in the library
    FormMain.FrameMainAddFixture1.FrameViewDMXLibrary1.SelectFixture(fix.FixLibLocation);
  end;

  FState := msReleased;
end;

procedure TFrameViewProjector.BGLVirtualScreen1MouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
var p1, p2: TPointF;
begin
  p1 := ClientToWord(MousePos);
  if WheelDelta < 0 then
    Zoom := Zoom-Zoom*0.2
  else
    Zoom := Zoom+Zoom*0.2;

  // moves the view
  p2 := ClientToWord(MousePos);
  FViewOrigin := FViewOrigin+(p2-p1)*Zoom;
  Redraw;
  Handled := TRUE;
end;

procedure TFrameViewProjector.BGLVirtualScreen1Redraw(Sender: TObject;
  BGLContext: TBGLContext);
var fix: TDMXFixture;
  tex: IBGLTexture;
  u, i, j, firstuni, lastuni: integer;
  p, half: TPointF;
  v, w, h: single;
  m: TAffineMatrix;
  txt: string;
  c: TBGRAPixel;
  alpha: byte;
  fixType: TFixtureType;
begin
  if not BGLVirtualScreen1.MakeCurrent(False) then
    exit;

  FInvalidateAlreadySent := False;

  if FOpenGLObjectsNeedToBeReconstruct then
  begin
    FOpenGLObjectsNeedToBeReconstruct := False;
    DeleteOpenGLObjects;
    CreateOpenGLObjects;
  end;

  if FNeedCenterView then
  begin
    FNeedCenterView := False;
    ComputeTotalViewRect;

    w := BGLVirtualScreen1.ClientWidth / FTotalViewRect.Width;
    h := BGLVirtualScreen1.ClientHeight / FTotalViewRect.Height;
    Zoom := Min(w, h)*0.95;

    FViewOrigin.x := Abs(FTotalViewRect.Left) * FZoom +
                     (BGLVirtualScreen1.ClientWidth - FTotalViewRect.Width*FZoom) * 0.5;
    FViewOrigin.y := Abs(FTotalViewRect.Top) * FZoom +
                     (BGLVirtualScreen1.ClientHeight - FTotalViewRect.Height*FZoom) * 0.5;
  end;

  //BGLVirtualScreen1.MakeCurrent(FALSE);
  with BGLContext.Canvas do
  begin
    // background
    FillRect(0, 0, Width, Height, FColorBackground);
    FTotalViewRect:=RectF(-100,-100,100,100);

    // set global transform matrix
    Matrix := MatrixView;

    // draw the stage
    if FTextureStage <> NIL then
      FTextureStage.Draw(-FTextureStage.Width/2,-FTextureStage.Height/2);

    // draw the seats
    if FTextureSeats <> NIL then
    begin
      FTextureSeats.Draw(-FTextureSeats.Width/2, 300);
      FTotalViewRect := FTotalViewRect.Union(RectF(-FTextureSeats.Width/2, 300, FTextureSeats.Width/2, 300+FTextureSeats.Height));
    end;

    // world origin
    Line(0,-5,0,5,BGRA(255,255,255,200));
    Line(-5,0,5,0,BGRA(255,255,255,200));

    // select which universe to render
    GetUniverseIndexes(firstuni, lastuni);
    if firstuni < 0 then
      exit;

    m := Matrix;
    w := 1+1/FZoom*0.5;
    for u:=lastuni downto firstuni do
    begin
     for i:=UniverseManager.Universes[u].FixturesCount-1 downto 0 do
     begin
       fix := UniverseManager.Universes[u].Fixtures[i];
       tex := TextureFor(fix.FixtureType);
       half := TextureHalfSize(tex);

       // set fixture view matrix
       Translate(fix.ScreenPos.x+half.x, fix.ScreenPos.y+half.y);
       RotateDeg(fix.Angle);
       Translate(-half.x, -half.y);
       Scale(fix.Zoom, fix.Zoom);

       // render state 'mouse over' in cursor view
       if FTargetFixtureInViewCursor=fix then
       begin
         Rectangle(0, 0, tex.Width, tex.Height, FColorSelection, BGRA(255,255,0,80));
       end;

       // render fixture image
       if fix.FlipH then tex.ToggleFlipX;
       if fix.FlipV then tex.ToggleFlipY;
       tex.Draw(0, 0);
       if fix.FlipH then tex.ToggleFlipX;
       if fix.FlipV then tex.ToggleFlipY;

       if fix = FFixtureSourceForCopy
        then c := BGRA(255,80,255)
        else c := FColorSelection;

       // render fixture selected state
       if fix.Selected then
         case FSelectionMode of
           smRotationItem:
             begin
              Translate(half.x, half.y);
              Ellipse(0, 0, Max(half.x, half.y), Max(half.x, half.y), c);
              Translate(-half.x, -half.y);
             end;
           smNeutral, smMoveItem: Rectangle(0, 0, tex.Width, tex.Height, c, w);
           smZoomItem:
             begin
              FillPolyConvex(PointsF([PointF(-10,-10), PointF(-10+w,-10), PointF(10,10), PointF(10-w,10)]), c);
              FillPolyConvex(PointsF([PointF(tex.Width+10,-10), PointF(tex.Width+13,-10), PointF(tex.Width-7,10), PointF(tex.Width-10,10)]), c);
              FillPolyConvex(PointsF([PointF(-10,tex.Height+10), PointF(10,tex.Height-10), PointF(13,tex.Height-10), PointF(-10,tex.Height+10)]), c);
              FillPolyConvex(PointsF([PointF(tex.Width-10,tex.Height-10), PointF(tex.Width-7,tex.Height-10), PointF(tex.Width+13,tex.Height+10), PointF(tex.Width+10,tex.Height+10)]), c);
             end;
         end;

       // render dmx adress
  {     if FShowDMXAdress then
       begin
         Scale(1/fix.Zoom, 1/fix.Zoom);
         Translate(half.x, half.y);
         RotateDeg(-fix.Angle);
         if UniverseManager.Count = 1 then
           txt := ''
         else
           txt := 'U'+(u+1).ToString+':';
         txt := txt+fix.Adress.ToString;
         h := FDMXAdressFont.TextWidth(txt)*1.1;
         Translate(-h*0.5, -half.y+tex.Height*0.80);
         FillRect(0, 0, h, FDMXAdressFont.EmHeight, FColorBackground);
         Translate(h*0.5, 0);
         FDMXAdressFont.TextOut(0, 0, txt, taCenter, tlTop, BGRA(200,200,200));
       end;  }
        // render HAS RGB
        if fix.HasRGBChannel and FShowRGBSymbol then
        begin
          Matrix := m;
          Translate(fix.ScreenPos.x, fix.ScreenPos.y);
          FillRect(5,5,20,25,BGRA(255,100,80));
          FillRect(20,5,35,25,BGRA(80,255,80));
          FillRect(35,5,50,25,BGRA(64,199,255));
        end;
        // render locked state
        if fix.Locked then
        begin
          Matrix := m;
          Translate(fix.ScreenPos.x+tex.Width-FLockTexture.Width, fix.ScreenPos.y);
          FLockTexture.Draw(0, 0);
        end;
        // render channels level
        if FShowLevel then
        begin
          Matrix := m;
          h := tex.Width*0.9/fix.ChannelsCount;
          if h > 15 then h := 15;
          Translate(fix.ScreenPos.x+half.x-h*fix.ChannelsCount*0.5, fix.ScreenPos.y+half.y);
          if FShowDMXAdress then
            alpha := 180
          else
            alpha := 255;
          for j:=0 to fix.ChannelsCount-1 do
          begin
             case fix.Channels[j].ChannelType of
               ctRED: c := BGRA(255,50,50,alpha);
               ctGREEN: c := BGRA(50,255,50,alpha);
               ctBLUE: c := BGRA(64,199,255,alpha);
               ctWHITE: c := BGRA(255,255,255,alpha);
               ctCOLDWHITE: c := BGRA(228,247,255,alpha);
               ctWARMWHITE: c := BGRA(254,255,168,alpha);
               ctAMBER: c := BGRA(255,162,100,alpha);
               ctUV: c := BGRA(240,48,255,alpha);
               ctCYAN: c := BGRA(0,251,255,alpha);
               ctYELLOW: c := BGRA(254,255,0,alpha);
               ctLIME: c := BGRA(192,255,0,alpha);
               ctINDIGO: c := BGRA(147,0,255,alpha);
               ctMAGENTA: c := BGRA(255,0,254,alpha);
               else c := BGRA(220,200,120,alpha);
             end;

             if fix.Channels[j].FFlashIsActive then
               v := fix.Channels[j].FFlashValue
             else
               v := fix.Channels[j].PercentValue;
             FillRect(j*h, half.y-half.y*v, j*h+h-1, half.y, c);
          end;
        end;

       Matrix := m;
       FTotalViewRect:=FTotalViewRect.Union(GetWordFixtureArea(fix));
     end;
    end;

    // Render Adress
    if FShowDMXAdress then
    begin
      for u:=lastuni downto firstuni do
      begin
       for i:=UniverseManager.Universes[u].FixturesCount-1 downto 0 do
       begin
        fix := UniverseManager.Universes[u].Fixtures[i];
        tex := TextureFor(fix.FixtureType);
        half := TextureHalfSize(tex);

        // set fixture view matrix
        Translate(fix.ScreenPos.x+half.x, fix.ScreenPos.y+half.y);
        if UniverseManager.Count = 1 then
          txt := ''
        else
          txt := 'U'+(u+1).ToString+':';
        txt := txt+fix.Adress.ToString;
        h := FDMXAdressFont.TextWidth(txt)*1.1;
        Translate(-h*0.5, -half.y+tex.Height*0.80);
        FillRect(0, 0, h, FDMXAdressFont.EmHeight, FColorBackground);
        Translate(h*0.5, 0);
        FDMXAdressFont.TextOut(0, 0, txt, taCenter, tlTop, BGRA(200,200,200));
        Matrix := m;
       end;
      end;
    end;

    // rectangular area selection
    if FState = msRectSelection then
    begin
      Rectangle(FSelectionRect.Left, FSelectionRect.Top, FSelectionRect.Right, FSelectionRect.Bottom, BGRA(255,255,180), 1.+1/FZoom);
    end;

    // render the fixture to add under mouse cursor
    if FState = msAdding then
    begin
      fixType := FLibraryFixtureToAdd.General.FixtureType;
      tex := TextureFor(fixType);
      w := tex.Width*FZoom;
      h := tex.Height*FZoom;
      p := PointF(BGLVirtualScreen1.ScreenToClient(Mouse.CursorPos));
      p.x := p.x-w*0.5;
      p.y := p.y-h*0.5;
      p := ClientToWord(p.Truncate);

      if (p.x < 0) and FixtureCanFlipH[fixType] then tex.ToggleFlipX;
      if (p.y > 0) and FixtureCanFlipV[fixType] then tex.ToggleFlipY;
      tex.Draw(p.x, p.y);
      if (p.x < 0) and FixtureCanFlipH[fixType] then tex.ToggleFlipX;
      if (p.y > 0) and FixtureCanFlipV[fixType] then tex.ToggleFlipY;
    end;


{Matrix:=AffineMatrixIdentity;
FDMXAdressFont.TextOut(0,0,'FViewOrigin ('+FormatFloat('0.0',FViewOrigin.x)+','+FormatFloat('0.0',FViewOrigin.y),bgrawhite);
FDMXAdressFont.TextOut(0,FDMXAdressFont.EmHeight,'FZoom '+FormatFloat('0.000',FZoom),bgrawhite);
FDMXAdressFont.TextOut(0,FDMXAdressFont.EmHeight*2,'TotalViewRect  TopLeft('+
                       FormatFloat('0.0',FTotalViewRect.left)+','+
                       FormatFloat('0.0',FTotalViewRect.top)+')  Width '+
                       FormatFloat('0.0',FTotalViewRect.Width)+'  Height '+
                       FormatFloat('0.0',FTotalViewRect.Height),bgrawhite);
FDMXAdressFont.TextOut(0,FDMXAdressFont.EmHeight*3,
        'ClientRect ('+BGLVirtualScreen1.ClientRect.Width.ToString+','+
         BGLVirtualScreen1.ClientRect.Height.ToString+')',bgrawhite);  }

  end;
end;

procedure TFrameViewProjector.BGLVirtualScreen1Resize(Sender: TObject);
begin
  View_Center;
end;

procedure TFrameViewProjector.BGLVirtualScreen1UnloadTextures(Sender: TObject;
  BGLContext: TBGLContext);
begin
  DeleteOpenGLObjects;
end;

procedure TFrameViewProjector.BShowDMXAdressClick(Sender: TObject);
begin
  FShowDMXAdress := FToogleSpeedButtonManager.Checked[BShowDMXAdress];
  FShowLevel := FToogleSpeedButtonManager.Checked[BShowLevels];
  FShowRGBSymbol := FToogleSpeedButtonManager.Checked[BShowRGBSymbol];
  FShowFixtureInfo := FToogleSpeedButtonManager.Checked[BShowInfo];

  if Sender = BShowInfo then
    if FShowFixtureInfo then
      ShowPanelFixtureInfo
    else
      HidePanelFixtureInfo;

  Project.Options.Save;
  Redraw;
end;

procedure TFrameViewProjector.BZoomAllClick(Sender: TObject);
begin
  View_Center;
  Redraw;
end;

procedure TFrameViewProjector.ComboBox1Select(Sender: TObject);
begin
  Sel_None;
  Redraw;
end;

procedure TFrameViewProjector.MICreateRGBGroupClick(Sender: TObject);
begin
  FormDMXGroup.AddRGBGroup(FrameViewDMXCursors1.GetTargetFixtures);
end;

procedure TFrameViewProjector.MIDeleteClick(Sender: TObject);
var i: integer;
  uni: TDMXUniverse;
  F: TFormAskIfShiftAdress;
begin
  if GetSelectedCount=0 then
    exit;

  F := TFormAskIfShiftAdress.Create(NIL);
  try
    if F.ShowModal = mrOk then begin
      FrameViewDMXCursors1.Clear;

      for i:=0 to High(FSelected) do begin
        uni := FSelected[i].Universe;
        uni.Fixture_DeleteByID(FSelected[i].ID, F.ShiftAdress);
      end;
      SetLength(FSelected, 0);
      Redraw;
      HidePanelFixtureInfo;
      DoDeleteFixtureEvent;
      Project.SetModified;
      FormMain.CheckSequenceError;
    end;
  finally
    F.Free;
  end;


{  if GetSelectedCount=1
   then txt:=SAreYouSureToDeleteThisFixture
   else txt:=SAreYouSureToDeleteTheseFixtures;
  if AskConfirmation(txt, SYes, SCancel, mtWarning)<>mrOk then exit;

  for i:=0 to High(FSelected) do begin
    uni:=FSelected[i].Universe;
    uni.Fixture_DeleteByID(FSelected[i].ID);
  end;
  SetLength(FSelected, 0);
  Redraw;
  FrameViewDMXCursors1.Clear;
  DoDeleteFixtureEvent;
  Project.SetModified;    }
end;

procedure TFrameViewProjector.MIHFlipClick(Sender: TObject);
begin
  Sel_ToogleFlipH;
  Redraw;
end;

procedure TFrameViewProjector.MILockFixtureClick(Sender: TObject);
var i: integer;
begin
  for i:=0 to High(Selected) do
   Selected[i].Locked := TRUE;

  Redraw;
  FrameViewDMXCursors1.RedrawVisibleCursors;
end;

procedure TFrameViewProjector.MIRotationClick(Sender: TObject);
begin
  FSelectionMode := smRotationItem;
  Redraw;
end;

procedure TFrameViewProjector.MIUnlockFixtureClick(Sender: TObject);
var i: integer;
begin
  for i:=0 to self.SelectedCount-1 do
   Selected[i].Locked := FALSE;

  Redraw;
  FrameViewDMXCursors1.RedrawVisibleCursors;
end;

procedure TFrameViewProjector.MIVFlipClick(Sender: TObject);
begin
  Sel_ToogleFlipV;
  Redraw;
end;

procedure TFrameViewProjector.MIZoomClick(Sender: TObject);
begin
  FSelectionMode := smZoomItem;
  Redraw;
end;

procedure TFrameViewProjector.Panel1Resize(Sender: TObject);
 function LeftToolsWidth: integer;
 begin
   Result := SpeedButton5.Width;
 end;
 function RightToolsWidth: integer;
 begin
   Result := BAddDMX.Left+BAddDMX.Width-BShowLevels.Left;
 end;
begin
  // rearrange the tools on 1 or 2 lines
  if LeftToolsWidth+RightToolsWidth+ScaleDesignToForm(15*3) >= Panel1.Width then
  begin  // 2 lines
    Panel1.Height := ScaleDesignToForm(7+20+4+20+8);

    BShowLevels.Top := Panel1.ClientHeight-ScaleDesignToForm(20+7);
    BShowLevels.Left := (Panel1.ClientWidth-RightToolsWidth) div 2;
  end;

  if LeftToolsWidth+RightToolsWidth+ScaleDesignToForm(15*3) < Panel1.Width then
  begin // 1 line
   Panel1.Height := ScaleDesignToForm(7+20+8);

   BShowLevels.Top := ScaleDesignToForm(7);
   BShowLevels.Left := Panel1.ClientWidth-RightToolsWidth-ScaleDesignToForm(15);
  end;
end;

procedure TFrameViewProjector.PopFixture_ModeEditActionPopup(Sender: TObject);
begin
  MICreateRGBGroup.Enabled := AllSelectedHaveRGB;
end;

procedure TFrameViewProjector.SpeedButton5Click(Sender: TObject);
begin
  SeqPLayer.StopPreview;
  UniverseManager.BlackOut;
  Redraw;
  FrameViewDMXCursors1.RedrawVisibleCursors;
end;

procedure TFrameViewProjector.Splitter1Moved(Sender: TObject);
begin
  Project.Options.Save;
end;

function TFrameViewProjector.ModePrepaDMX: boolean;
begin
  Result := FGUIMode = guiPrepaDMX;
end;

function TFrameViewProjector.ModeEditAction: boolean;
begin
  Result := FGUIMode = guiEditSequence;
end;

function TFrameViewProjector.ModeMainDMX: boolean;
begin
  Result := FGUIMode = guiMainDMX;
end;

function TFrameViewProjector.MatrixView: TAffineMatrix;
begin
  Result := AffineMatrixTranslation(FViewOrigin.x, FViewOrigin.y);
  Result := Result*AffineMatrixScale(FZoom, FZoom);
end;

procedure TFrameViewProjector.ComputeTotalViewRect;
var u, i: integer;
  fix: TDMXFixture;
begin
  FTotalViewRect := RectF(-100,-100,100,100);
  // the stage
  if FTextureStage <> NIL then
  begin
    FTotalViewRect := FTotalViewRect.Union(RectF(-FTextureStage.Width/2, -FTextureStage.Height/2, FTextureStage.Width/2, FTextureStage.Height/2));
  end;
  // the seats
  if FTextureSeats <> NIL then
  begin
    FTotalViewRect := FTotalViewRect.Union(RectF(-FTextureSeats.Width/2, ScaleDesignToForm(300), FTextureSeats.Width/2, ScaleDesignToForm(300)+FTextureSeats.Height));
  end;
  // the fixtures
  for u:=0 to UniverseManager.Count-1 do
   for i:=UniverseManager.Universes[u].FixturesCount-1 downto 0 do
   begin
     fix := UniverseManager.Universes[u].Fixtures[i];
     FTotalViewRect := FTotalViewRect.Union(GetWordFixtureArea(fix));
   end;
end;

procedure TFrameViewProjector.SetZoom(AValue: single);
begin
  FZoom:=EnsureRange(AValue, 0.01, 10);
end;

function TFrameViewProjector.FixtureUnderMouse(X, Y: integer): TDMXFixture;
var u, i, firstuni, lastuni: integer;
  fix: TDmxFixture;
  p: TPointF;
  r: TRectF;
begin
  Result := NIL;
  if FTextures = NIL then
    exit;

  p := ClientToWord(Point(X, Y));
  //p:=PointF(X,Y);

  GetUniverseIndexes(firstuni, lastuni);
  if firstuni < 0 then
    exit;

  for u:=firstuni to lastuni do
   for i:=0 to UniverseManager.Universes[u].FixturesCount-1 do
   begin
     fix := UniverseManager.Universes[u].Fixtures[i];
 {    tex:=TextureFor(fix.FixtureType);

     matrixFixture:=MatrixView*AffineMatrixTranslation(fix.ScreenPos.x+tex.Width*0.5, fix.ScreenPos.y+tex.Height*0.5);
     matrixFixture:=matrixFixture*AffineMatrixRotationDeg(fix.Angle);
     matrixFixture:=matrixFixture*AffineMatrixTranslation(-tex.Width*0.5, -tex.Height*0.5);
     matrixFixture:=matrixFixture*AffineMatrixScale(fix.Zoom, fix.zoom);
     matrixFixture:=AffineMatrixInverse(matrixFixture);
     ptransformed:=matrixFixture*p;  }

     r := GetWordFixtureArea(fix);
     //if r.Contains(ptransformed)then begin
     if r.Contains(p)then
     begin
       Result := fix;
       exit;
     end;
   end;
end;

function TFrameViewProjector.ClientToWord(pt: TPoint): TPointF;
begin
  Result:=AffineMatrixInverse(MatrixView)*PointF(pt);
end;

function TFrameViewProjector.ClientToWord(r: TRect): TRectF;
begin
  Result.TopLeft:=ClientToWord(r.TopLeft);
  Result.BottomRight:=ClientToWord(r.BottomRight);
end;

function TFrameViewProjector.WordToClient(pt: TPointF): TPoint;
var pf: TPointF;
begin
  pf := MatrixView*pt;
  Result.x := Trunc(pf.x);
  Result.y := Trunc(pf.y);
end;

function TFrameViewProjector.WordToClient(r: TRectF): TRect;
begin
  Result.TopLeft := WordToClient(r.TopLeft);
  Result.BottomRight := WordToClient(r.BottomRight);
end;

function TFrameViewProjector.ClientPointIsInFixtureCircle(X, Y: integer): boolean;
var p, center, half: TPointF;
  i: integer;
begin
  Result := FALSE;
  if GetSelectedCount=0 then
    exit;

  p := ClientToWord(BGLVirtualScreen1.ScreenToClient(Mouse.CursorPos));
  for i:=0 to High(FSelected) do
  begin
    half := TextureHalfSize(FSelected[i].FixtureType);
    center := FSelected[i].ScreenPos+half;
    if center.Distance(p) <= Max(half.x, half.y) then
    begin
      Result := TRUE;
      exit;
    end;
  end;
end;

procedure TFrameViewProjector.LoopRotateFixture;
var origin, current, delta: TPoint;
  df: TPointF;
  flagmodif: boolean;
begin
  FState := msRotate;
  BGLVirtualScreen1.Cursor:=ROTATION_CURSOR;
  origin := BGLVirtualScreen1.ScreenToClient(Mouse.CursorPos);
  flagmodif := FALSE;

  repeat
    current := BGLVirtualScreen1.ScreenToClient(Mouse.CursorPos);
    delta := current-origin;
    if delta.x <> 0 then
    begin
      df.x := delta.x*1/FZoom;
      Sel_Rotate(df.x);
      Redraw;
      flagmodif := TRUE;
      origin := current;
    end;
    Application.ProcessMessages;
  until FState = msReleased;

  if flagmodif then
    Project.SetModified;

  if not flagmodif then
  begin
    BGLVirtualScreen1.Cursor := DEFAULT_CURSOR;
    FState := msReleased;
    FSelectionMode := smNeutral;
    Sel_None;
    Redraw;
  end;
end;

procedure TFrameViewProjector.LoopZoomFixture;
var origin, current, delta: TPoint;
  df: TPointF;
  flagmodif: boolean;
begin
  FState := msRotate;
  BGLVirtualScreen1.Cursor:=ZOOM_CURSOR;
  origin := BGLVirtualScreen1.ScreenToClient(Mouse.CursorPos);
  flagmodif := FALSE;

  repeat
    current := BGLVirtualScreen1.ScreenToClient(Mouse.CursorPos);
    delta := current-origin;
    if delta.x <> 0 then
    begin
      df.x := delta.x*1/FZoom;
      Sel_Zoom(df.x*0.02);
      Redraw;
      flagmodif := TRUE;
      origin := current;
    end;
    Application.ProcessMessages;
  until FState = msReleased;

  if flagmodif then
    Project.SetModified;

  if not flagmodif then
  begin
    BGLVirtualScreen1.Cursor := DEFAULT_CURSOR;
    FState := msReleased;
    FSelectionMode := smNeutral;
    Sel_None;
    Redraw;
  end;
end;

procedure TFrameViewProjector.LoopMoveFixture;
var origin, current, delta: TPoint;
  df: TPointF;
  flagmodif: boolean;
begin
  FState := msMoving;
  origin := BGLVirtualScreen1.ScreenToClient(Mouse.CursorPos);
  flagmodif := FALSE;

  repeat
    current := BGLVirtualScreen1.ScreenToClient(Mouse.CursorPos);
    delta := current-origin;
    if (delta.x <> 0) or (delta.y <> 0) then
    begin
      df.x := delta.x*1/FZoom;
      df.y := delta.y*1/FZoom;
      Sel_ShiftPosition(df);
      Redraw;
      flagmodif := TRUE;
      origin := current;
    end;
    Application.ProcessMessages;
  until FState = msReleased;

  if flagmodif then
    Project.SetModified;
{  if not flagmodif then begin
    case FSelectionMode of
      smMoveItem: FSelectionMode:=smRotationItem;
      smRotationItem: FSelectionMode:=smMoveItem;
    end;//case
    Redraw;
  end;  }
end;

procedure TFrameViewProjector.LoopShiftView;
var origin, current, delta: TPoint;
begin
  FState := msShiftView;
  origin := BGLVirtualScreen1.ScreenToClient(Mouse.CursorPos);

  repeat
    current := BGLVirtualScreen1.ScreenToClient(Mouse.CursorPos);
    delta := current-origin;
    if (delta.x <> 0) or (delta.y <> 0) then
    begin
      FViewOrigin := FViewOrigin+PointF(delta);
      Redraw;
      origin := current;
      BGLVirtualScreen1.Cursor := SHIFTVIEW_CURSOR;
    end;
    Application.ProcessMessages;
  until FState = msReleased;

  BGLVirtualScreen1.Cursor := DEFAULT_CURSOR;
end;

procedure TFrameViewProjector.LoopRectangularSelection;
var origin, current, topleft, bottomright: TPointF;
  u, i, firstuni, lastuni: integer;
  fix: TDMXFixture;
  r: TRectF;
  userDoARectangle: boolean;
begin
  FState := msRectSelection;
  BGLVirtualScreen1.Cursor := RECTSELECTION_CURSOR;
  origin := ClientToWord(BGLVirtualScreen1.ScreenToClient(Mouse.CursorPos));
  current := origin;
  FSelectionRect := RectF(origin, origin+PointF(1, 1));
  userDoARectangle := FALSE;
  repeat
    if current <> ClientToWord(BGLVirtualScreen1.ScreenToClient(Mouse.CursorPos)) then
    begin
      current := ClientToWord(BGLVirtualScreen1.ScreenToClient(Mouse.CursorPos));
      userDoARectangle := TRUE;
      topleft.x := Min(origin.x, current.x);
      topleft.y := Min(origin.y, current.y);
      bottomright.x := Max(origin.x, current.x);
      bottomright.y := Max(origin.y, current.y);
      FSelectionRect := RectF(topleft, bottomright);
      Redraw;
    end;
    Application.ProcessMessages;
    Sleep(16);
  until FState = msReleased;

  if userDoARectangle then
  begin
    GetUniverseIndexes(firstuni, lastuni);
    // selects all fixtures in the rectangle
    for u:=firstuni to lastuni do
     for i:=0 to UniverseManager.Universes[u].FixturesCount-1 do
     begin
       fix := UniverseManager.Universes[u].Fixtures[i];
       r := GetWordFixtureArea(fix);
       if FSelectionRect.IntersectsWith(r) then
       begin
         fix.Selected := TRUE;
         FrameViewDMXCursors1.Add(fix, False);
         AddFixtureToOtherWindows(fix);
         if FSelectionMode = smNeutral
          then FSelectionMode := smMoveItem;
       end;
     end;
    FrameViewDMXCursors1.UpdateView;
    UpdateSelected;
  end
  else Sel_None;

  Redraw;
  BGLVirtualScreen1.Cursor := DEFAULT_CURSOR;
end;

procedure TFrameViewProjector.UpdateSelected;
var u, i, firstuni, lastuni: integer;
  fix: TDMXFixture;
begin
  SetLength(FSelected, 0);
  GetUniverseIndexes(firstuni, lastuni);
  for u:=firstuni to lastuni do
   for i:=0 to UniverseManager.Universes[u].FixturesCount-1 do
   begin
     fix := UniverseManager.Universes[u].Fixtures[i];
     if fix.Selected then
       InternalAddToSelected(fix);
   end;
end;

function TFrameViewProjector.SomeSelectedHaveRGB: boolean;
var i: integer;
begin
  Result := FALSE;
  for i:=0 to High(FSelected) do
   if FSelected[i].HasRGBChannel then
   begin
     Result := TRUE;
     exit;
   end;
end;

function TFrameViewProjector.AllSelectedHaveRGB: boolean;
var i: integer;
begin
  if Length(Selected) = 0 then exit(False);

  Result := True;
  for i:=0 to High(FSelected) do
   Result := Result and FSelected[i].HasRGBChannel;
end;

procedure TFrameViewProjector.InternalAddToSelected(aFix: TDMXFixture);
var k: integer;
begin
  k := Length(FSelected);
  SetLength(FSelected, k+1);
  FSelected[k] := aFix;
  DoFixtureSelectionChange;
end;

function TFrameViewProjector.GetSelectedCount: integer;
begin
  Result := Length(FSelected);
end;

procedure TFrameViewProjector.InternalSel_None;
begin
  UniverseManager.Sel_None;
  SetLength(FSelected, 0);
  FSelectionMode := smNeutral;
  DoFixtureSelectionChange;
end;

procedure TFrameViewProjector.AddFixtureToOtherWindows(aFix: TDMXFixture);
begin
  if aFix.HasRGBChannel then
    FormDMXRGBTools.UserHaveSelected(aFix);
end;

procedure TFrameViewProjector.RemoveFixtureToOtherWindows(aFix: TDMXFixture);
begin
  FrameViewDMXCursors1.Remove(aFix);
  FormDMXRGBTools.UserHaveRemoved(aFix);
end;

procedure TFrameViewProjector.ClearSelectionOnOtherWindows;
begin
  FrameViewDMXCursors1.Clear;
  FormDMXRGBTools.ClearSelectedFixtures;
end;

procedure TFrameViewProjector.CreateFixturesTextures;
var i: integer;
  im: TBGRABitmap;
begin
  for i:=0 to High(FTextures) do
    FTextures[i].FreeMemory;

  SetLength(FTextures, ord(High(TFixtureType))+1);
  for i:=0 to High(FTextures) do
    FTextures[i] := BGLTexture(FixtureImages[TFixtureType(i)]);

  im := SVGFileToBGRABitmap(GetAppFixtureImagesFolder+'Lock.svg', -1, -1);
  FLockTexture := BGLTexture(im);
  im.Free;
end;

procedure TFrameViewProjector.CreateDMXAdressFont;
begin
  FDMXAdressFont := NIL;
  FDMXAdressFont := BGLFont('Arial', 36, [fsBold]);
  FDMXAdressFont.Quality := fqSystem;
end;

procedure TFrameViewProjector.CreateOpenGlObjects;
begin
  CreateFixturesTextures;
  CreateDMXAdressFont;
  CreateStageTexture;
  CreateSeatsTexture;
end;

procedure TFrameViewProjector.DeleteOpenGLObjects;
var i: integer;
begin
  FTextureSeats := NIL;
  FTextureStage := NIL;
  FLockTexture := NIL;
  for i:=0 to High(FTextures) do
    FTextures[i] := NIL;
  FTextures := NIL;
  FDMXAdressFont := NIL;
end;

procedure TFrameViewProjector.CreateSeatsTexture;
var ima: TBGRABitmap;
  svg: TBGRASvg;
  aspectratio: single;
  f: string;
begin
  if FTextureSeats <> NIL then
    FTextureSeats.FreeMemory;

  f := SeatSvgFileFor(ProgramOptions.SeatType);
  if f = '' then
  begin
    FTextureSeats := NIL;
    exit;
  end;

  svg := TBGRASvg.Create(f);
  aspectratio := svg.WidthAsPixel/svg.HeightAsPixel;
  ima := TBGRABitmap.Create(trunc(350*aspectratio),350);

  svg.StretchDraw(ima.Canvas2D, taCenter, tlCenter,0, 0, ima.Width, ima.Height);
  FTextureSeats := BGLTexture(ima);
  svg.Free;
  ima.Free;
end;

procedure TFrameViewProjector.CreateStageTexture;
var ima: TBGRABitmap;
  svg: TBGRASvg;
  aspectratio: single;
  f: string;
begin
  if FTextureStage <> NIL then
    FTextureStage.FreeMemory;

  f := StageSvgFileFor(ProgramOptions.StageType);
  if f = '' then
  begin
    FTextureStage := NIL;
    exit;
  end;

  svg := TBGRASvg.Create(f);
  aspectratio := svg.WidthAsPixel/svg.HeightAsPixel;
  ima := TBGRABitmap.Create(trunc(300*aspectratio),300);

  svg.StretchDraw(ima.Canvas2D, taCenter, tlCenter,0, 0, ima.Width, ima.Height);
  FTextureStage := BGLTexture(ima);
  svg.Free;
  ima.Free;
end;

function TFrameViewProjector.TextureFor(aFt: TFixtureType): IBGLTexture;
var i: integer;
begin
  i := Ord(aFt);
  if i > High(FTextures) then
    Result := FTextures[ord(ftOther)]
  else
    Result := FTextures[i];
end;

function TFrameViewProjector.TextureHalfSize(aFt: TFixtureType): TPointF;
var tex: IBGLTexture;
begin
  tex := TextureFor(aFt);
  Result := PointF(tex.Width*0.5, tex.Height*0.5);
end;

function TFrameViewProjector.TextureHalfSize(aTex: IBGLTexture): TPointF;
begin
  Result := PointF(aTex.Width*0.5, aTex.Height*0.5);
end;

function TFrameViewProjector.GetWordFixtureArea(aFix: TDMXFixture): TRectF;
var tex: IBGLTexture;
  //m: TAffineMatrix;
  //ab: TAffineBox;
  //q: TQuad;
begin
  tex := TextureFor(aFix.FixtureType);
  Result.TopLeft := aFix.ScreenPos;
  Result.BottomRight := aFix.ScreenPos+PointF(tex.Width{*aFix.Zoom}, tex.Height{*aFix.Zoom});
end;

procedure TFrameViewProjector.DoAddFixture(X, Y: integer);
var fix: TDMXFixture;
  adress: TDMXAdress;
  targetUni: TDMXUniverse;
  chanCount: integer;
begin
  targetUni := FFixtureToAdd.TargetUniverse;
  if targetUni = NIL then begin
    ExitAddMode;
    exit;
  end;

  // check if there is enough dmx adress available in the target universe to fit all channels
  chanCount := FLibraryFixtureToAdd.GetChannelCountForMode(FFixtureToAdd.FixtureLocation.Mode);
  adress := 0; // avoid compilation hint
  if not targetUni.TryToFindFreeAdressRange(chanCount, adress) then begin
    ShowMess(SUniverseFull+lineending+targetUni.Name, SOk, mtError);
    ExitAddMode;
    exit;
  end;

  fix := targetUni.Fixture_AddFromFixLib(FLibraryFixtureToAdd, FFixtureToAdd.FixtureLocation);
  if fix = NIL then
  begin
    ShowMess(SFailToLoadTheFixtureFromLibrary, SOk, mtError);
    ExitAddMode;
    exit;
  end;

  fix.Adress := adress;
  fix.ScreenPos := ClientToWord(Point(X,Y))-TextureHalfSize(fix.FixtureType);
  fix.FlipH := (fix.ScreenPos.x < 0) and FixtureCanFlipH[fix.FixtureType];
  fix.FlipV := (fix.ScreenPos.y > 0) and FixtureCanFlipV[fix.FixtureType];
  fix.Selected := TRUE;
  targetUni.DoOptimizeUsedChannels;
  InternalAddToSelected(fix);
  FrameViewDMXCursors1.Add(fix, True);
  Redraw;
  Project.SetModified;
end;

procedure TFrameViewProjector.GetUniverseIndexes(out firstuni, lastuni: integer);
begin
  if ComboBox1.ItemIndex=0 then
  begin
    firstuni := 0;
    lastuni := UniverseManager.Count-1;
  end
  else
  begin
    firstuni := ComboBox1.ItemIndex-1;
    lastuni := firstuni;
  end;
end;

procedure TFrameViewProjector.SetFixtureSourceForCopy(AValue: TDMXFixture);
begin
  if FFixtureSourceForCopy = AValue then
    Exit;
  FFixtureSourceForCopy := AValue;
  Redraw;
end;

procedure TFrameViewProjector.DoFixtureSelectionChange;
begin
  if FOnFixtureSelectionChange <> NIL then
    FOnFixtureSelectionChange(Self);

  ShowPanelFixtureInfo;
end;

procedure TFrameViewProjector.DoDeleteFixtureEvent;
begin
  if FOnDeleteFixture <> NIL then
    FOnDeleteFixture(Self);
end;

procedure TFrameViewProjector.ShowPanelFixtureInfo;
begin
  if (FGUIMode in [guiPrepaDMX, guiMainDMX]) and
     FToogleSpeedButtonManager.Checked[BShowInfo] then
  begin
    if SelectedCount = 1 then begin
      FrameFixtureInfo1.UpdateView;
      Panel9.Height := FrameFixtureInfo1.ViewHeight+2;
      Panel9.Visible := TRUE;
    end
    else Panel9.Visible := FALSE;
  end
  else Panel9.Visible := FALSE;
end;

procedure TFrameViewProjector.SetGUIMode(AValue: TGUIMode);
begin
  FGUIMode := AValue;

  HideToolsWindows;
  FrameViewDMXCursors1.GUIMode := FGUIMode;

  case FGUIMode of
    guiMainDMX:
      begin
       BAddDMX.Enabled := True; //Project.IsReady;
      end;

    guiPrepaDMX:
      begin
       BAddDMX.Enabled := FALSE;
       HidePanelFixtureInfo;
      end;

    guiEditSequence:
      begin
       BAddDMX.Enabled := FALSE;
      end;
  end;
end;

end.

