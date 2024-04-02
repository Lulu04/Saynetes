unit frame_viewdmxprojectors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, LCLType, StdCtrls, Menus,
  Buttons, Graphics, frame_bglvirtualscreen_sequencer, Types,
  BGRABitmap, BGRABitmapTypes, BGLVirtualScreen, BGRAOpenGL, BGRAOpenGLType,
  BGRASVG, BGRAFontGL,
  u_list_dmxuniverse, u_dmx_util, u_common,
  frame_viewdmxcursors, frame_velocity, u_notebook_util;


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

  TViewDMXProjectorMode=(opViewModePrepaDMX,
                         opViewModeMainDMX,
                         opViewModeEditAction);

  { TFrameViewDMXProjectors }

  TFrameViewDMXProjectors = class(TFrame)
    BGLVirtualScreen1: TBGLVirtualScreen;
    ComboBox1: TComboBox;
    CBStage: TComboBox;
    CBSeats: TComboBox;
    MenuItem1: TMenuItem;
    MIUnlockFixture: TMenuItem;
    MICreateRGBGroup: TMenuItem;
    MenuItem3: TMenuItem;
    MIZoom: TMenuItem;
    MIRotation: TMenuItem;
    MILockFixture: TMenuItem;
    MIDelete: TMenuItem;
    MIVFlip: TMenuItem;
    MIHFlip: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    PopFixture_ModePrepaDMX: TPopupMenu;
    PopFixture_ModeEditAction: TPopupMenu;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    Splitter1: TSplitter;
    procedure BGLVirtualScreen1DragOver(Sender, Source: TObject; {%H-}X, {%H-}Y: Integer; {%H-}State: TDragState; var Accept: Boolean);
    procedure BGLVirtualScreen1LoadTextures(Sender: TObject; {%H-}BGLContext: TBGLContext);
    procedure BGLVirtualScreen1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BGLVirtualScreen1MouseLeave(Sender: TObject);
    procedure BGLVirtualScreen1MouseMove(Sender: TObject; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure BGLVirtualScreen1MouseUp(Sender: TObject; Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure BGLVirtualScreen1MouseWheel(Sender: TObject; {%H-}Shift: TShiftState; WheelDelta: Integer; {%H-}MousePos: TPoint; var Handled: Boolean);
    procedure BGLVirtualScreen1Redraw(Sender: TObject; BGLContext: TBGLContext);
    procedure BGLVirtualScreen1Resize(Sender: TObject);
    procedure ComboBox1Select(Sender: TObject);
    procedure CBStageSelect(Sender: TObject);
    procedure CBSeatsSelect(Sender: TObject);
    procedure MICreateRGBGroupClick(Sender: TObject);
    procedure MIDeleteClick(Sender: TObject);
    procedure MIHFlipClick(Sender: TObject);
    procedure MILockFixtureClick(Sender: TObject);
    procedure MIRotationClick(Sender: TObject);
    procedure MIUnlockFixtureClick(Sender: TObject);
    procedure MIVFlipClick(Sender: TObject);
    procedure MIZoomClick(Sender: TObject);
    procedure PopFixture_ModeEditActionPopup(Sender: TObject);
    procedure PopFixture_ModePrepaDMXPopup(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
  private
    FViewOrigin: TPointF;
    FZoom: single;
    FColorBackground,
    FColorSelection: TBGRAPixel;
    FState: TViewDMXProjectorsMouseState;
    FMode: TViewDMXProjectorMode;
    FSelectionMode: TViewDMXProjectorSelectionMode;
    FSelectionRect,
    FTotalViewRect: TRectF;
    FNeedCenterView: boolean;
    FWorkingFixture: TDMXFixture;
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
    procedure InternalAddToSelected(aFix: TDMXFixture);
    function GetSelectedCount: integer;
    procedure InternalSel_None;
    procedure AddFixtureToOtherWindows(aFix: TDMXFixture);
    procedure RemoveFixtureToOtherWindows(aFix: TDMXFixture);
    procedure ClearSelectionOnOtherWindows;

  private
    FTextures: array of IBGLTexture;
    FDMXAdressFont: IBGLRenderedFont;
    FLockTexture: IBGLTexture;
    procedure CreateFixturesTextures;
    procedure CreateDMXAdressFont;
    procedure CreateOpenGlContextObjects;
    function TextureFor(aFt: TFixtureType): IBGLTexture;
    function TextureHalfSize(aFt: TFixtureType): TPointF; overload;
    function TextureHalfSize(aTex: IBGLTexture): TPointF; overload;
    function GetWordFixtureArea(aFix: TDMXFixture): TRectF;
  private
    FStageType: TStageType;
    FSeatType: TSeatType;
    FTextureSeats,
    FTextureStage: IBGLTexture;
    procedure CreateSeatsTexture;
    procedure CreateStageTexture;
    procedure SetStageType(AValue: TStageType);
    procedure SetSeatType(AValue: TSeatType);
  private
    FFixtureFilenameToAdd: string;
    FLibraryFixtureToAdd: TLibraryFixture;
    FTargetUniverse: TDMXUniverse;
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
  private
    FInvalidateAlreadySent: boolean;
  public
    FrameViewDMXCursors1: TFrameViewDMXCursors;
    procedure ProcessViewDMXCursorsMouseOverFixtureEvent(Sender: TObject; aFixture: TDMXFixture);

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground({%H-}DC: HDC); override;
    procedure Redraw;

    procedure SetViewModePrepaDMX;
    procedure SetViewModeMainDmx;

    procedure SetModeEditAction;
    procedure UpdateButtons;
    procedure UpdateComboBoxLanguage;

    procedure FillComboBoxUniverseToShow;

    procedure FixtureFilenameToAdd(const aFilename: string; aTargetUniverse: TDMXUniverse);
    procedure ExitAddMode;
    procedure ProcessKey(var Key: Word; Shift: TShiftState);

    procedure AddToSelected(aFix: TDMXFixture);
    procedure Sel_None;
    procedure Sel_All;
    procedure Sel_ShiftPosition(delta: TPointF);
    procedure Sel_Rotate(delta: single);
    procedure Sel_Zoom(delta: single);
    procedure Sel_ToogleFlipH;
    procedure Sel_ToogleFlipV;

    procedure View_Center;
    procedure ReloadTexture;
    procedure HideToolsWindows;

    property Zoom: single read FZoom write SetZoom;
    property StageType: TStageType read FStageType write SetStageType;
    property SeatType: TSeatType read FSeatType write SetSeatType;

    property FixtureSourceForCopy: TDMXFixture read FFixtureSourceForCopy write SetFixtureSourceForCopy;

  public
    procedure DoOnAddCmd(const aCmd: TSingleCmd; const aShortReadable: string; aDuration: single);
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
  end;

implementation

uses u_project_manager, u_userdialogs, u_resource_string, u_dmxtools_group,
  u_prepa_dmx, u_list_top, u_audio_manager, u_top_player, u_dmxtools_rgb,
  u_askifshiftadress, u_add_fixture, Math, ComCtrls, Dialogs, BGRATransform;

{$R *.lfm}

{ TFrameViewDMXProjectors }

procedure TFrameViewDMXProjectors.BGLVirtualScreen1LoadTextures(
  Sender: TObject; BGLContext: TBGLContext);
begin
  CreateOpenGlContextObjects;
end;

procedure TFrameViewDMXProjectors.BGLVirtualScreen1DragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if (Source is TTreeView) and (FFixtureFilenameToAdd<>'') and (FTargetUniverse<>NIL)
      and ModePrepaDMX then begin
    Sel_None;
    FState:=msAdding;
    Accept:=TRUE;
    Redraw;
  end else Accept:=FALSE;
end;

procedure TFrameViewDMXProjectors.BGLVirtualScreen1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var fix: TDMXFixture;
begin
  if FState=msReleased then begin
    fix:=FixtureUnderMouse(X,Y);

    // fixture selection/rotation and drag
    if (fix<>NIL) and (button=mbLeft) then begin
      if ssShift in shift then begin
        fix.Selected:=not fix.Selected;
        if fix.Selected
          then AddFixtureToOtherWindows(fix)
          else RemoveFixtureToOtherWindows(fix);
      end else if not fix.Selected then begin
            Sel_None;
            FSelectionMode:=smMoveItem;
            fix.Selected:=TRUE;
            AddFixtureToOtherWindows(fix);
      end;
      UpdateSelected;
      if GetSelectedCount=0
        then FSelectionMode:=smNeutral
        else if (FSelectionMode=smNeutral) and ModePrepaDMX
              then FSelectionMode:=smMoveItem;
      Redraw;
      if ModePrepaDMX and(FSelectionMode=smMoveItem) then begin
        LoopMoveFixture;
      end;
      exit;
    end;

    // start rotation
    if (fix=NIL) and (Button=mbLeft) and (FSelectionMode=smRotationItem) and ModePrepaDMX then begin
      LoopRotateFixture;
      exit;
    end;

    // start zoom
    if (fix=NIL) and (Button=mbLeft) and (FSelectionMode=smZoomItem) and ModePrepaDMX then begin
      LoopZoomFixture;
      exit;
    end;

    // rectangular selection
    if (fix=NIL) and (Button=mbLeft) then begin
      LoopRectangularSelection;
      exit;
    end;

    // shift the view
    if (fix=NIL) and (Button=mbRight) then begin
      LoopShiftView;
      exit;
    end;

    // fixture popup menu
    if (fix<>NIL) and (button=mbRight) then begin
      if not fix.Selected then begin
        Sel_None;
        fix.Selected:=TRUE;
        UpdateSelected;
        FrameViewDMXCursors1.Add(fix);
        FSelectionMode:=smMoveItem;
        Redraw;
      end;
      FWorkingFixture:=fix;
      if ModePrepaDMX then PopFixture_ModePrepaDMX.PopUp;
      if ModeEditAction or ModeMainDMX then PopFixture_ModeEditAction.PopUp;
    end;
  end;
end;

procedure TFrameViewDMXProjectors.BGLVirtualScreen1MouseLeave(Sender: TObject);
begin
  FState:=msReleased;
end;

procedure TFrameViewDMXProjectors.BGLVirtualScreen1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var fix: TDMXFixture;
begin
  if FState=msReleased then begin
    fix:=FixtureUnderMouse(X, Y);
    if fix<>NIL
     then BGLVirtualScreen1.Cursor:=OVERITEM_CURSOR
     else case FSelectionMode of
       //smMoveItem: BGLVirtualScreen1.Cursor:=OVERITEM_CURSOR;
       smRotationItem: BGLVirtualScreen1.Cursor:=ROTATION_CURSOR;
       smZoomItem: BGLVirtualScreen1.Cursor:=ZOOM_CURSOR;
       else BGLVirtualScreen1.Cursor:=DEFAULT_CURSOR;
     end;
  end;

  if FState=msAdding then begin
     Redraw;
  end;
end;

procedure TFrameViewDMXProjectors.BGLVirtualScreen1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var fix: TDMXFixture;
begin
  fix:=FixtureUnderMouse(X, Y);

  if (FState=msAdding) and (Button=mbLeft) then begin
    DoAddFixture(X, Y);
    exit;
  end;
  if (FState=msAdding) and (Button=mbRight) then begin
    ExitAddMode;
    Redraw;
  end;

  if (FState=msReleased) and (fix=NIL) and (Button=mbLeft) then begin
    Sel_None;
    Redraw;
  end;

  FState:=msReleased;
end;

procedure TFrameViewDMXProjectors.BGLVirtualScreen1MouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
var p1, p2: TPointF;
  origin: TPoint;
begin
  origin:=BGLVirtualScreen1.ScreenToClient(Mouse.CursorPos);
  p1:=ClientToWord(origin);
  if WheelDelta<0
   then Zoom:=Zoom-Zoom*0.1
   else Zoom:=Zoom+Zoom*0.1;
  // moves the view
  p2:=ClientToWord(origin);
  FViewOrigin:=FViewOrigin+p2-p1;
  Redraw;
  Handled:=TRUE;
end;

procedure TFrameViewDMXProjectors.BGLVirtualScreen1Redraw(Sender: TObject; BGLContext: TBGLContext);
var fix: TDMXFixture;
  tex: IBGLTexture;
  u, i, j, firstuni, lastuni: integer;
  p, half: TPointF;
  w, h: single;
  m: TAffineMatrix;
  txt: string;
  c: TBGRAPixel;
  alpha: byte;
begin
  FInvalidateAlreadySent:=FALSE;

  if FNeedCenterView then begin
    FNeedCenterView:=FALSE;
    ComputeTotalViewRect;

    w:=BGLVirtualScreen1.ClientWidth/FTotalViewRect.Width;
    h:=BGLVirtualScreen1.ClientHeight/FTotalViewRect.Height;
    Zoom:=Min(w, h)*0.95;

    FViewOrigin.x:=Abs(FTotalViewRect.Left)*FZoom;
    FViewOrigin.y:=Abs(FTotalViewRect.Top)*FZoom;
  end;
  BGLVirtualScreen1.MakeCurrent(FALSE);
  with BGLContext.Canvas do begin
    // background
    FillRect(0, 0, Width, Height, FColorBackground);
    FTotalViewRect:=RectF(-100,-100,100,100);

    // set global transform matrix
    Matrix:=MatrixView;

    // draw the stage
    if FTextureStage<>NIL then begin
      FTextureStage.Draw(-FTextureStage.Width/2,-FTextureStage.Height/2);
     // FTotalViewRect:=FTotalViewRect.Union(RectF(-FTextureStage.Width/2, -FTextureStage.Height/2, FTextureStage.Width/2, FTextureStage.Height/2));
    end;
    // draw the seats
    if FTextureSeats<>NIL then begin
      FTextureSeats.Draw(-FTextureSeats.Width/2, 300);
      FTotalViewRect:=FTotalViewRect.Union(RectF(-FTextureSeats.Width/2, 300, FTextureSeats.Width/2, 300+FTextureSeats.Height));
    end;

    // world origin
    Line(0,-5,0,5,BGRA(255,255,255,200));
    Line(-5,0,5,0,BGRA(255,255,255,200));

    // select which universe to render
    GetUniverseIndexes(firstuni, lastuni);
    if firstuni<0 then exit;

    m:=Matrix;
    w:=1+1/FZoom*0.5;
    for u:=lastuni downto firstuni do begin
     for i:=UniverseManager.Universes[u].FixturesCount-1 downto 0 do begin
       fix:=UniverseManager.Universes[u].Fixtures[i];
       tex:=TextureFor(fix.FixtureType);
       half:=TextureHalfSize(tex);

       // set fixture view matrix
       Translate(fix.ScreenPos.x+half.x, fix.ScreenPos.y+half.y);
       RotateDeg(fix.Angle);
       Translate(-half.x, -half.y);
       Scale(fix.Zoom, fix.Zoom);

       // render state 'mouse over' in cursor view
       if FTargetFixtureInViewCursor=fix then begin
         Rectangle(0, 0, tex.Width, tex.Height, FColorSelection, BGRA(255,255,0,80));
       end;

       // render fixture image
       if fix.FlipH then tex.ToggleFlipX;
       if fix.FlipV then tex.ToggleFlipY;
       tex.Draw(0, 0);
       if fix.FlipH then tex.ToggleFlipX;
       if fix.FlipV then tex.ToggleFlipY;

       if fix=FFixtureSourceForCopy
        then c:=BGRA(255,80,255)
        else c:=FColorSelection;

       // render fixture selected state
       if fix.Selected then
         case FSelectionMode of
           smRotationItem: begin
              Translate(half.x, half.y);
              Ellipse(0, 0, Max(half.x, half.y), Max(half.x, half.y), c);
              Translate(-half.x, -half.y);
           end;
           smNeutral, smMoveItem: Rectangle(0, 0, tex.Width, tex.Height, c, w);
           smZoomItem: begin
             FillPolyConvex(PointsF([PointF(-10,-10), PointF(-10+w,-10), PointF(10,10), PointF(10-w,10)]), c);
             FillPolyConvex(PointsF([PointF(tex.Width+10,-10), PointF(tex.Width+13,-10), PointF(tex.Width-7,10), PointF(tex.Width-10,10)]), c);
             FillPolyConvex(PointsF([PointF(-10,tex.Height+10), PointF(10,tex.Height-10), PointF(13,tex.Height-10), PointF(-10,tex.Height+10)]), c);
             FillPolyConvex(PointsF([PointF(tex.Width-10,tex.Height-10), PointF(tex.Width-7,tex.Height-10), PointF(tex.Width+13,tex.Height+10), PointF(tex.Width+10,tex.Height+10)]), c);
           end;
         end;

       // render dmx adress
       if Project.Prefs.ProjectorViewShowAdress then begin
         Scale(1/fix.Zoom, 1/fix.Zoom);
         Translate(half.x, half.y);
         RotateDeg(-fix.Angle);
         if UniverseManager.Count=1
           then txt:=''
           else txt:='U'+(u+1).ToString+':';
         txt:=txt+fix.Adress.ToString;
         h:=FDMXAdressFont.TextWidth(txt)*1.1;
         Translate(-h*0.5, -half.y+tex.Height*0.80);
         FillRect(0, 0, h, FDMXAdressFont.EmHeight, FColorBackground);
         Translate(h*0.5, 0);
         FDMXAdressFont.TextOut(0, 0, txt, taCenter, tlTop, BGRA(200,200,200));
       end;
        // render HAS RGB
        if fix.HasRGBChannel and Project.Prefs.ProjectorViewShowRGBSymbol then begin
          Matrix:=m;
          Translate(fix.ScreenPos.x, fix.ScreenPos.y);
          FillRect(10,10,20,20,BGRA(255,0,0,180));
          FillRect(20,10,30,20,BGRA(0,255,0,180));
          FillRect(30,10,40,20,BGRA(0,0,255,180));
        end;
        // render locked state
        if fix.Locked then begin
          Matrix:=m;
          Translate(fix.ScreenPos.x+tex.Width-FLockTexture.Width, fix.ScreenPos.y);
          FLockTexture.Draw(0, 0);
        end;
        // render channels level
        if Project.Prefs.ProjectorViewShowLevel then begin
          Matrix:=m;
          h:=tex.Width*0.9/fix.ChannelsCount;
          if h>15 then h:=15;
          Translate(fix.ScreenPos.x+half.x-h*fix.ChannelsCount*0.5, fix.ScreenPos.y+half.y);
          if SpeedButton2.Tag=1
            then alpha:=180
            else alpha:=255;
          for j:=0 to fix.ChannelsCount-1 do begin
             case fix.Channels[j].ChannelType of
               ctRED: c:=BGRA(255,50,50,alpha);
               ctGreen: c:=BGRA(50,255,50,alpha);
               ctBlue: c:=BGRA(50,100,255,alpha);
               ctWhite: c:=BGRA(255,255,255,alpha);
               ctAmber: c:=BGRA(255,162,100,alpha);
               ctUV: c:=BGRA(240,48,255,alpha);
               else c:=BGRA(200,200,100,alpha);
             end;
            FillRect(j*h, half.y-half.y*fix.Channels[j].PercentValue, j*h+h-1, half.y, c);
          end;
        end;

       Matrix:=m;
       FTotalViewRect:=FTotalViewRect.Union(GetWordFixtureArea(fix));
     end;
    end;

    // rectangular area selection
    if FState=msRectSelection then begin
      Rectangle(FSelectionRect.Left, FSelectionRect.Top, FSelectionRect.Right, FSelectionRect.Bottom, BGRA(255,255,180), 1.+1/FZoom);
    end;

    // render the fixture to add under mouse cursor
    if FState=msAdding then begin
      tex:=TextureFor(FLibraryFixtureToAdd.FixtureType);
      w:=tex.Width*FZoom;
      h:=tex.Height*FZoom;
      p:=PointF(BGLVirtualScreen1.ScreenToClient(Mouse.CursorPos));
      p.x:=p.x-w*0.5;
      p.y:=p.y-h*0.5;
      p:=ClientToWord(p.Truncate);

      if p.x<0 then tex.ToggleFlipX;
      if p.y>0 then tex.ToggleFlipY;
      tex.Draw(p.x, p.y);
      if p.x<0 then tex.ToggleFlipX;
      if p.y>0 then tex.ToggleFlipY;
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

procedure TFrameViewDMXProjectors.BGLVirtualScreen1Resize(Sender: TObject);
begin
  View_Center;
 // Redraw;
end;

procedure TFrameViewDMXProjectors.ComboBox1Select(Sender: TObject);
begin
  Sel_None;
  Redraw;
end;

procedure TFrameViewDMXProjectors.CBStageSelect(Sender: TObject);
begin
  StageType:=TStageType(CBStage.ItemIndex);
  Project.SetModified;
end;

procedure TFrameViewDMXProjectors.CBSeatsSelect(Sender: TObject);
begin
  SeatType:=TSeatType(CBSeats.Itemindex);
  Project.SetModified;
end;

procedure TFrameViewDMXProjectors.MICreateRGBGroupClick(Sender: TObject);
begin
  FormDMXGroup.AddRGBGroup(FrameViewDMXCursors1.GetTargetFixtures);
end;

procedure TFrameViewDMXProjectors.MIDeleteClick(Sender: TObject);
var i: integer;
  uni: TDMXUniverse;
  F: TFormAskIfShiftAdress;
begin
  if GetSelectedCount=0 then exit;

  F:=TFormAskIfShiftAdress.Create(NIL);
 // F.TargetFrameProjector := Self;
  if F.ShowModal=mrOk then begin
    for i:=0 to High(FSelected) do begin
      uni:=FSelected[i].Universe;
      uni.Fixture_DeleteByID(FSelected[i].ID, F.ShiftAdress);
    end;
    SetLength(FSelected, 0);
    Redraw;
    FrameViewDMXCursors1.Clear;
    DoDeleteFixtureEvent;
    Project.SetModified;
  end;
  F.Free;


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

procedure TFrameViewDMXProjectors.MIHFlipClick(Sender: TObject);
begin
  Sel_ToogleFlipH;
  Redraw;
end;

procedure TFrameViewDMXProjectors.MILockFixtureClick(Sender: TObject);
var i: integer;
begin
  for i:=0 to self.SelectedCount-1 do
   Selected[i].Locked:=TRUE;
  Redraw;
  FrameViewDMXCursors1.Redraw;
end;

procedure TFrameViewDMXProjectors.MIRotationClick(Sender: TObject);
begin
  FSelectionMode:=smRotationItem;
  Redraw;
end;

procedure TFrameViewDMXProjectors.MIUnlockFixtureClick(Sender: TObject);
var i: integer;
begin
  for i:=0 to self.SelectedCount-1 do
   Selected[i].Locked:=FALSE;
  Redraw;
  FrameViewDMXCursors1.Redraw;
end;

procedure TFrameViewDMXProjectors.MIVFlipClick(Sender: TObject);
begin
  Sel_ToogleFlipV;
  Redraw;
end;

procedure TFrameViewDMXProjectors.MIZoomClick(Sender: TObject);
begin
  FSelectionMode:=smZoomItem;
  Redraw;
end;

procedure TFrameViewDMXProjectors.PopFixture_ModeEditActionPopup(Sender: TObject );
begin
  MILockFixture.Enabled:=GetSelectedCount>=1;
  MIUnlockFixture.Enabled:=MILockFixture.Enabled;
end;

procedure TFrameViewDMXProjectors.PopFixture_ModePrepaDMXPopup(Sender: TObject);
begin
  MIRotation.Enabled:=GetSelectedCount>0;
  MIZoom.Enabled:=MIRotation.Enabled;
  MIHFlip.Enabled:=MIRotation.Enabled;
  MIVFlip.Enabled:=MIRotation.Enabled;
  MIDelete.Enabled:=MIRotation.Enabled;
end;

procedure TFrameViewDMXProjectors.SpeedButton1Click(Sender: TObject);
begin
  View_Center;
  Redraw;
end;

procedure TFrameViewDMXProjectors.SpeedButton2Click(Sender: TObject);
begin
  Project.Prefs.ProjectorViewShowAdress:=FToogleSpeedButtonManager.Checked[SpeedButton2];

  Project.Prefs.ProjectorViewShowRGBSymbol:=FToogleSpeedButtonManager.Checked[SpeedButton7];

  Project.Prefs.ProjectorViewShowLevel:=FToogleSpeedButtonManager.Checked[SpeedButton8];
  Redraw;
end;

procedure TFrameViewDMXProjectors.SpeedButton4Click(Sender: TObject);
var F: TFormPrepaDMX;
begin
  Sequences.StopAll;
  SoundManager.ResetState;
  UniverseManager.Sel_None;
  UniverseManager.StopThread;

  SpeedButton4.Visible := False;
  CBStage.Visible := True;
  CBSeats.Visible := True;
  F:=TFormPrepaDMX.Create(NIL);
  try
    F.ShowModal;
  Finally
    F.Free;
  end;

  SpeedButton4.Visible := True;
  CBStage.Visible := False;
  CBSeats.Visible := False;

  UpdateButtons;
  FillComboBoxUniverseToShow;
  Sel_None;
  UniverseManager.Sel_None;
  UniverseManager.StartThread;
  Redraw;

 // FrameMainDMX1.RedirectCallBacks;
end;



procedure TFrameViewDMXProjectors.SpeedButton5Click(Sender: TObject);
begin
  TopPLayer.StopPreview;
  UniverseManager.BlackOut;
  Redraw;
  FrameViewDMXCursors1.Redraw;

  FormDMXAddFixture.Show;
end;

function TFrameViewDMXProjectors.ModePrepaDMX: boolean;
begin
  Result:= FMode=opViewModePrepaDMX;
end;

function TFrameViewDMXProjectors.ModeEditAction: boolean;
begin
  Result:= FMode=opViewModeEditAction;
end;

function TFrameViewDMXProjectors.ModeMainDMX: boolean;
begin
  Result:=FMode=opViewModeMainDMX;
end;

function TFrameViewDMXProjectors.MatrixView: TAffineMatrix;
begin
  Result:=AffineMatrixTranslation(FViewOrigin.x, FViewOrigin.y);
  Result:=Result*AffineMatrixScale(FZoom, FZoom);
end;

procedure TFrameViewDMXProjectors.ComputeTotalViewRect;
var u, i: integer;
  fix: TDMXFixture;
begin
  FTotalViewRect:=RectF(-100,-100,100,100);
  // the stage
  if FTextureStage<>NIL then begin
    FTotalViewRect:=FTotalViewRect.Union(RectF(-FTextureStage.Width/2, -FTextureStage.Height/2, FTextureStage.Width/2, FTextureStage.Height/2));
  end;
  // the seats
  if FTextureSeats<>NIL then begin
    FTotalViewRect:=FTotalViewRect.Union(RectF(-FTextureSeats.Width/2, 300, FTextureSeats.Width/2, 300+FTextureSeats.Height));
  end;
  // the fixtures
  for u:=0 to UniverseManager.Count-1 do begin
   for i:=UniverseManager.Universes[u].FixturesCount-1 downto 0 do begin
     fix:=UniverseManager.Universes[u].Fixtures[i];
     FTotalViewRect:=FTotalViewRect.Union(GetWordFixtureArea(fix));
   end;
  end;
end;

procedure TFrameViewDMXProjectors.SetZoom(AValue: single);
begin
  FZoom:=EnsureRange(AValue, 0.01, 10);
end;

function TFrameViewDMXProjectors.FixtureUnderMouse(X, Y: integer): TDMXFixture;
var u, i, firstuni, lastuni: integer;
  fix: TDmxFixture;
  p: TPointF;
  r: TRectF;
begin
  Result:=NIL;
  if FTextures=NIL then exit;

  p:=ClientToWord(Point(X, Y));
  //p:=PointF(X,Y);

  GetUniverseIndexes(firstuni, lastuni);
  if firstuni<0 then exit;
  for u:=firstuni to lastuni do
   for i:=0 to UniverseManager.Universes[u].FixturesCount-1 do begin
     fix:=UniverseManager.Universes[u].Fixtures[i];
 {    tex:=TextureFor(fix.FixtureType);

     matrixFixture:=MatrixView*AffineMatrixTranslation(fix.ScreenPos.x+tex.Width*0.5, fix.ScreenPos.y+tex.Height*0.5);
     matrixFixture:=matrixFixture*AffineMatrixRotationDeg(fix.Angle);
     matrixFixture:=matrixFixture*AffineMatrixTranslation(-tex.Width*0.5, -tex.Height*0.5);
     matrixFixture:=matrixFixture*AffineMatrixScale(fix.Zoom, fix.zoom);
     matrixFixture:=AffineMatrixInverse(matrixFixture);
     ptransformed:=matrixFixture*p;  }

     r:=GetWordFixtureArea(fix);
     //if r.Contains(ptransformed)then begin
     if r.Contains(p)then begin
       Result:=fix;
       exit;
     end;
   end;
end;

function TFrameViewDMXProjectors.ClientToWord(pt: TPoint): TPointF;
begin
  Result:=AffineMatrixInverse(MatrixView)*PointF(pt);
end;

function TFrameViewDMXProjectors.ClientToWord(r: TRect): TRectF;
begin
  Result.TopLeft:=ClientToWord(r.TopLeft);
  Result.BottomRight:=ClientToWord(r.BottomRight);
end;

function TFrameViewDMXProjectors.WordToClient(pt: TPointF): TPoint;
var pf: TPointF;
begin
  pf:=MatrixView*pt;
  Result.x:=Trunc(pf.x);
  Result.y:=Trunc(pf.y);
end;

function TFrameViewDMXProjectors.WordToClient(r: TRectF): TRect;
begin
  Result.TopLeft:=WordToClient(r.TopLeft);
  Result.BottomRight:=WordToClient(r.BottomRight);
end;

function TFrameViewDMXProjectors.ClientPointIsInFixtureCircle(X, Y: integer): boolean;
var p, center, half: TPointF;
  i: integer;
begin
  Result:=FALSE;
  if GetSelectedCount=0 then exit;

  p:=ClientToWord(BGLVirtualScreen1.ScreenToClient(Mouse.CursorPos));
  for i:=0 to High(FSelected) do begin
    half:=TextureHalfSize(FSelected[i].FixtureType);
    center:=FSelected[i].ScreenPos+half;
    if center.Distance(p)<=Max(half.x, half.y) then begin
      Result:=TRUE;
      exit;
    end;
  end;
end;

procedure TFrameViewDMXProjectors.LoopRotateFixture;
var origin, current, delta: TPoint;
  df: TPointF;
  flagmodif: boolean;
begin
  FState:=msRotate;
  BGLVirtualScreen1.Cursor:=ROTATION_CURSOR;
  origin:=BGLVirtualScreen1.ScreenToClient(Mouse.CursorPos);
  flagmodif:=FALSE;

  repeat
    current:=BGLVirtualScreen1.ScreenToClient(Mouse.CursorPos);
    delta:=current-origin;
    if delta.x<>0 then begin
      df.x:=delta.x*1/FZoom;
      Sel_Rotate(df.x);
      Redraw;
      flagmodif:=TRUE;
      origin:=current;
    end;
    Application.ProcessMessages;
  until FState=msReleased;

  if flagmodif then Project.SetModified;
  if not flagmodif then begin
    BGLVirtualScreen1.Cursor:=DEFAULT_CURSOR;
    FState:=msReleased;
    FSelectionMode:=smNeutral;
    Sel_None;
    Redraw;
  end;
end;

procedure TFrameViewDMXProjectors.LoopZoomFixture;
var origin, current, delta: TPoint;
  df: TPointF;
  flagmodif: boolean;
begin
  FState:=msRotate;
  BGLVirtualScreen1.Cursor:=ZOOM_CURSOR;
  origin:=BGLVirtualScreen1.ScreenToClient(Mouse.CursorPos);
  flagmodif:=FALSE;

  repeat
    current:=BGLVirtualScreen1.ScreenToClient(Mouse.CursorPos);
    delta:=current-origin;
    if delta.x<>0 then begin
      df.x:=delta.x*1/FZoom;
      Sel_Zoom(df.x*0.02);
      Redraw;
      flagmodif:=TRUE;
      origin:=current;
    end;
    Application.ProcessMessages;
  until FState=msReleased;

  if flagmodif then Project.SetModified;
  if not flagmodif then begin
    BGLVirtualScreen1.Cursor:=DEFAULT_CURSOR;
    FState:=msReleased;
    FSelectionMode:=smNeutral;
    Sel_None;
    Redraw;
  end;
end;

procedure TFrameViewDMXProjectors.LoopMoveFixture;
var origin, current, delta: TPoint;
  df: TPointF;
  flagmodif: boolean;
begin
  FState:=msMoving;
  origin:=BGLVirtualScreen1.ScreenToClient(Mouse.CursorPos);
  flagmodif:=FALSE;

  repeat
    current:=BGLVirtualScreen1.ScreenToClient(Mouse.CursorPos);
    delta:=current-origin;
    if (delta.x<>0) or (delta.y<>0) then begin
      df.x:=delta.x*1/FZoom;
      df.y:=delta.y*1/FZoom;
      Sel_ShiftPosition(df);
      Redraw;
      flagmodif:=TRUE;
      origin:=current;
    end;
    Application.ProcessMessages;
  until FState=msReleased;

  if flagmodif then Project.SetModified;
{  if not flagmodif then begin
    case FSelectionMode of
      smMoveItem: FSelectionMode:=smRotationItem;
      smRotationItem: FSelectionMode:=smMoveItem;
    end;//case
    Redraw;
  end;  }
end;

procedure TFrameViewDMXProjectors.LoopShiftView;
var origin, current, delta: TPoint;
begin
  FState:=msShiftView;
  origin:=BGLVirtualScreen1.ScreenToClient(Mouse.CursorPos);

  repeat
    current:=BGLVirtualScreen1.ScreenToClient(Mouse.CursorPos);
    delta:=current-origin;
    if (delta.x<>0) or (delta.y<>0) then begin
      FViewOrigin:=FViewOrigin+PointF(delta);
      Redraw;
      origin:=current;
      BGLVirtualScreen1.Cursor:=SHIFTVIEW_CURSOR;
    end;
    Application.ProcessMessages;
  until FState=msReleased;

  BGLVirtualScreen1.Cursor:=DEFAULT_CURSOR;
end;

procedure TFrameViewDMXProjectors.LoopRectangularSelection;
var origin, current, delta, topleft, bottomright: TPointF;
  u, i, firstuni, lastuni: integer;
  fix: TDMXFixture;
  r: TRectF;
  userDoARectangle: boolean;
begin
  FState:=msRectSelection;
  BGLVirtualScreen1.Cursor:=RECTSELECTION_CURSOR;
  origin:=ClientToWord(BGLVirtualScreen1.ScreenToClient(Mouse.CursorPos));
  userDoARectangle:=FALSE;
  repeat
    current:=ClientToWord(BGLVirtualScreen1.ScreenToClient(Mouse.CursorPos));
    delta:=current-origin;
    if (delta.x<>0) or (delta.y<>0) then begin
      userDoARectangle:=TRUE;
      topleft.x:=Min(origin.x, current.x);
      topleft.y:=Min(origin.y, current.y);
      bottomright.x:=Max(origin.x, current.x);
      bottomright.y:=Max(origin.y, current.y);
      FSelectionRect:=RectF(topleft, bottomright);
      Redraw;
    end;
    Application.ProcessMessages;
  until FState=msReleased;

  if userDoARectangle then begin
    GetUniverseIndexes(firstuni, lastuni);
    // selects all fixtures in the rectangle
    for u:=firstuni to lastuni do
     for i:=0 to UniverseManager.Universes[u].FixturesCount-1 do begin
       fix:=UniverseManager.Universes[u].Fixtures[i];
       r:=GetWordFixtureArea(fix);
       if FSelectionRect.IntersectsWith(r) then begin
         fix.Selected:=TRUE;
         AddFixtureToOtherWindows(fix);
         if FSelectionMode=smNeutral
          then FSelectionMode:=smMoveItem;
       end;
     end;
    UpdateSelected;
  end else begin
    Sel_None;
  end;
  Redraw;
  BGLVirtualScreen1.Cursor:=DEFAULT_CURSOR;
end;

procedure TFrameViewDMXProjectors.UpdateSelected;
var u, i, firstuni, lastuni: integer;
  fix: TDMXFixture;
begin
  SetLength(FSelected, 0);
  GetUniverseIndexes(firstuni, lastuni);
  for u:=firstuni to lastuni do
   for i:=0 to UniverseManager.Universes[u].FixturesCount-1 do begin
     fix:=UniverseManager.Universes[u].Fixtures[i];
     if fix.Selected
      then InternalAddToSelected(fix);
   end;
end;

function TFrameViewDMXProjectors.SomeSelectedHaveRGB: boolean;
var i: integer;
begin
  Result:=FALSE;
  for i:=0 to High(FSelected) do
   if FSelected[i].HasRGBChannel then begin
     Result:=TRUE;
     exit;
   end;
end;

procedure TFrameViewDMXProjectors.InternalAddToSelected(aFix: TDMXFixture);
var k: integer;
begin
  k:=Length(FSelected);
  SetLength(FSelected, k+1);
  FSelected[k]:=aFix;
  DoFixtureSelectionChange;
end;

function TFrameViewDMXProjectors.GetSelectedCount: integer;
begin
  Result:=Length(FSelected);
end;

procedure TFrameViewDMXProjectors.InternalSel_None;
begin
  UniverseManager.Sel_None;
  SetLength(FSelected, 0);
  FSelectionMode:=smNeutral;
  DoFixtureSelectionChange;
end;

procedure TFrameViewDMXProjectors.AddFixtureToOtherWindows(aFix: TDMXFixture);
begin
  FrameViewDMXCursors1.Add(aFix);
  FormDMXRGBTools.UserHaveSelected(aFix);
end;

procedure TFrameViewDMXProjectors.RemoveFixtureToOtherWindows(aFix: TDMXFixture );
begin
  FrameViewDMXCursors1.Remove(aFix);
  FormDMXRGBTools.UserHaveRemoved(aFix);
end;

procedure TFrameViewDMXProjectors.ClearSelectionOnOtherWindows;
begin
  FrameViewDMXCursors1.Clear;
  FormDMXRGBTools.ClearSelectedFixtures;
end;

procedure TFrameViewDMXProjectors.CreateFixturesTextures;
var i: integer;
begin
  for i:=0 to High(FTextures) do
    FTextures[i].FreeMemory;

  SetLength(FTextures, ord(High(TFixtureType))+1);
  for i:=0 to High(FTextures) do
    FTextures[i]:=BGLTexture(FixtureImageFileFor(TFixtureType(i)));

  FLockTexture:=BGLTexture(DMXFixtureImagePath+'Lock.png');
end;

procedure TFrameViewDMXProjectors.CreateDMXAdressFont;
begin
  FDMXAdressFont:=NIL;
  FDMXAdressFont:=BGLFont('Arial', 36, [fsBold]);
  FDMXAdressFont.Quality:=fqSystem;
end;

procedure TFrameViewDMXProjectors.CreateOpenGlContextObjects;
begin
  CreateFixturesTextures;
  CreateDMXAdressFont;
  CreateStageTexture;
  CreateSeatsTexture;
end;

function TFrameViewDMXProjectors.TextureFor(aFt: TFixtureType): IBGLTexture;
var i: integer;
begin
  i:=Ord(aFt);
  if i>High(FTextures)
   then Result:=FTextures[ord(ftOther)]
   else Result:=FTextures[i];
end;

function TFrameViewDMXProjectors.TextureHalfSize(aFt: TFixtureType): TPointF;
var tex: IBGLTexture;
begin
  tex:=TextureFor(aFt);
  Result:=PointF(tex.Width*0.5, tex.Height*0.5);
end;

function TFrameViewDMXProjectors.TextureHalfSize(aTex: IBGLTexture): TPointF;
begin
  Result:=PointF(aTex.Width*0.5, aTex.Height*0.5);
end;

function TFrameViewDMXProjectors.GetWordFixtureArea(aFix: TDMXFixture): TRectF;
var tex: IBGLTexture;
  //m: TAffineMatrix;
  //ab: TAffineBox;
  //q: TQuad;
begin
  tex:=TextureFor(aFix.FixtureType);
  Result.TopLeft:=aFix.ScreenPos;
  Result.BottomRight:=aFix.ScreenPos+PointF(tex.Width{*aFix.Zoom}, tex.Height{*aFix.Zoom});
end;

procedure TFrameViewDMXProjectors.CreateSeatsTexture;
var ima: TBGRABitmap;
  svg: TBGRASvg;
  aspectratio: single;
  f: string;
begin
  if FTextureSeats<>NIL then begin
    FTextureSeats.FreeMemory;
  end;

  f:=SeatSvgFileFor(FSeatType);
  if f='' then begin
    FTextureSeats:=NIL;
    exit;
  end;

  svg:=TBGRASvg.Create(f);
  aspectratio:=svg.WidthAsPixel/svg.HeightAsPixel;
  ima:=TBGRABitmap.Create(trunc(350*aspectratio),350);

  svg.StretchDraw(ima.Canvas2D, taCenter, tlCenter,0, 0, ima.Width, ima.Height);
  FTextureSeats:=BGLTexture(ima);
  svg.Free;
  ima.Free;
end;

procedure TFrameViewDMXProjectors.CreateStageTexture;
var ima: TBGRABitmap;
  svg: TBGRASvg;
  aspectratio: single;
  f: string;
begin
  if FTextureStage<>NIL then begin
    FTextureStage.FreeMemory;
  end;

  f:=StageSvgFileFor(FStageType);
  if f='' then begin
    FTextureStage:=NIL;
    exit;
  end;

  svg:=TBGRASvg.Create(f);
  aspectratio:=svg.WidthAsPixel/svg.HeightAsPixel;
  ima:=TBGRABitmap.Create(trunc(300*aspectratio),300);

  svg.StretchDraw(ima.Canvas2D, taCenter, tlCenter,0, 0, ima.Width, ima.Height);
  FTextureStage:=BGLTexture(ima);
  svg.Free;
  ima.Free;
end;

procedure TFrameViewDMXProjectors.SetStageType(AValue: TStageType);
begin
  if AValue=FStageType then exit;
  FStageType:=AValue;
  BGLVirtualScreen1.QueryLoadTextures;
  Redraw;
end;

procedure TFrameViewDMXProjectors.DoAddFixture(X, Y: integer);
var fix: TDMXFixture;
  adress: TDMXAdress;
begin
  // check if there is enough dmx adress available in the target universe to fit all channels
  if not FTargetUniverse.FirstFreeAdress(Length(FLibraryFixtureToAdd.Channels), adress) then begin
    ShowMess(SUniverseFull+lineending+FTargetUniverse._Name, SOk, mtError);
    ExitAddMode;
    exit;
  end;

  fix:=FTargetUniverse.Fixture_AddFromDMXLib(FFixtureFilenameToAdd);
  if fix=NIL then begin
    ShowMess(SFailToLoadTheFixtureFromLibrary, SOk, mtError);
    ExitAddMode;
    exit;
  end;

  fix.Adress:=adress;
  fix.ScreenPos:=ClientToWord(Point(X,Y))-TextureHalfSize(fix.FixtureType);
  fix.FlipH:=fix.ScreenPos.x<0;
  fix.FlipV:=fix.ScreenPos.y>0;
  fix.Selected:=TRUE;
  FTargetUniverse.DoOptimizeUsedChannels;
  InternalAddToSelected(fix);
  FrameViewDMXCursors1.Add(fix);
  Redraw;
  Project.SetModified;
end;

procedure TFrameViewDMXProjectors.DoOnAddCmd(const aCmd: TSingleCmd;
  const aShortReadable: string; aDuration: single);
begin
  if FOnAddCmd<>NIL then begin
    FCmd:=aCmd;   // the action
    FShortReadableString:=aShortReadable;   // the text that resume the action
    FCmdDuration:=aDuration;
    FOnAddCmd(Self);
  end;
end;

procedure TFrameViewDMXProjectors.GetUniverseIndexes(out firstuni, lastuni: integer);
begin
  if ComboBox1.ItemIndex=0 then begin
    firstuni:=0;
    lastuni:=UniverseManager.Count-1;
  end else begin
    firstuni:=ComboBox1.ItemIndex-1;
    lastuni:=firstuni;
  end;
end;

procedure TFrameViewDMXProjectors.SetFixtureSourceForCopy(AValue: TDMXFixture);
begin
  if FFixtureSourceForCopy=AValue then Exit;
  FFixtureSourceForCopy:=AValue;
  Redraw;
end;

procedure TFrameViewDMXProjectors.DoFixtureSelectionChange;
begin
  if FOnFixtureSelectionChange<>NIL
   then FOnFixtureSelectionChange(Self);
end;

procedure TFrameViewDMXProjectors.DoDeleteFixtureEvent;
begin
  if FOnDeleteFixture<>NIL
   then FOnDeleteFixture(Self);
end;

procedure TFrameViewDMXProjectors.UpdateButtons;
begin
  FToogleSpeedButtonManager.SetState(SpeedButton8, Project.Prefs.ProjectorViewShowLevel);
  FToogleSpeedButtonManager.SetState(SpeedButton7, Project.Prefs.ProjectorViewShowRGBSymbol);
  FToogleSpeedButtonManager.SetState(SpeedButton2, Project.Prefs.ProjectorViewShowAdress);
end;

procedure TFrameViewDMXProjectors.UpdateComboBoxLanguage;
begin
  CBStage.Items.Strings[0]:=SNone;
  CBStage.Items.Strings[1]:=SRectangle;
  CBStage.Items.Strings[2]:=SQuare;
  CBStage.Items.Strings[3]:=SHalfCircle;
  CBStage.Items.Strings[4]:=SEllipse;
  CBStage.Items.Strings[5]:=SCustom1;

  CBSeats.Items.Strings[0]:=SNone;
  CBSeats.Items.Strings[1]:=SSeats1;
  CBSeats.Items.Strings[2]:=SSeats2;
end;

procedure TFrameViewDMXProjectors.ProcessViewDMXCursorsMouseOverFixtureEvent(
  Sender: TObject; aFixture: TDMXFixture);
begin
  if FTargetFixtureInViewCursor<>aFixture then begin
    FTargetFixtureInViewCursor:=aFixture;
    Redraw;
  end;
end;

procedure TFrameViewDMXProjectors.SetSeatType(AValue: TSeatType);
begin
  if FSeatType=AValue then Exit;
  FSeatType:=AValue;
  BGLVirtualScreen1.QueryLoadTextures;
  Redraw;
end;

constructor TFrameViewDMXProjectors.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  BGLVirtualScreen1.Color:=BGRA(25,15,10);
  {$ifdef MSWINDOWS}
  BGLVirtualScreen1.MultiSampling:=2;
  {$endif}

  FNeedCenterView:=TRUE;
  FZoom:=0.62;
  FState:=msReleased;
  FSelectionMode:=smNeutral;
  SetLength(FSelected, 0);
  FStageType:=stRectangle;
  FSeatType:=seatType1;

  FrameViewDMXCursors1:=TFrameViewDMXCursors.Create(Self);
  FrameViewDMXCursors1.Parent:=Panel2;
  FrameViewDMXCursors1.Align:=alClient;
  FrameViewDMXCursors1.OnMouseOverFixture:=@ProcessViewDMXCursorsMouseOverFixtureEvent;

  FToogleSpeedButtonManager:=TToggleSpeedButtonManager.Create;
  FToogleSpeedButtonManager.ToogleType := tsbLikeCheckBox;
  FToogleSpeedButtonManager.SetActivatedColors($0003C4FC, $00272727);
  FToogleSpeedButtonManager.SetDeactivatedColors($00004B62, $00EAEAEA);
  FToogleSpeedButtonManager.Add(SpeedButton7, FALSE);
  FToogleSpeedButtonManager.Add(SpeedButton2, FALSE);
  FToogleSpeedButtonManager.Add(SpeedButton8, TRUE);

  FColorBackground:=BGRA(51,51,51);
  FColorSelection:=BGRA(255,255,200);
end;

destructor TFrameViewDMXProjectors.Destroy;
begin
  FToogleSpeedButtonManager.Free;
  SetLength(FTextures, 0);
  inherited Destroy;
end;

procedure TFrameViewDMXProjectors.EraseBackground(DC: HDC);
begin
// do nothing here
end;

procedure TFrameViewDMXProjectors.Redraw;
begin
  if not FInvalidateAlreadySent then begin
    BGLVirtualScreen1.Invalidate;
    FInvalidateAlreadySent:=TRUE;
  end;
end;

procedure TFrameViewDMXProjectors.SetViewModePrepaDMX;
begin
  FMode:=opViewModePrepaDMX;
  CBStage.Visible:=TRUE;
  CBSeats.Visible:=TRUE;
  SpeedButton4.Visible:=FALSE;
  FrameViewDMXCursors1.SetViewModePrepaDMX;

  UpdateButtons;
end;

procedure TFrameViewDMXProjectors.SetViewModeMainDmx;
begin
  FMode:=opViewModeMainDMX;
  CBStage.Visible:=FALSE;
  CBSeats.Visible:=FALSE;
  SpeedButton4.Visible:=Project.IsReady;
  FrameViewDMXCursors1.SetViewModeMainDmx;

  UpdateButtons
end;

procedure TFrameViewDMXProjectors.SetModeEditAction;
begin
  FMode:=opViewModeEditAction;
  CBStage.Visible:=FALSE;
  CBSeats.Visible:=FALSE;
  SpeedButton4.Visible:=FALSE;
  FrameViewDMXCursors1.SetModeEditAction;

  UpdateButtons;
end;

procedure TFrameViewDMXProjectors.FillComboBoxUniverseToShow;
var i: integer;
begin
  i:= ComboBox1.ItemIndex;
  ComboBox1.Clear;
  ComboBox1.Items.Add(SAll);
  for i:=0 to UniverseManager.Count-1 do
    ComboBox1.Items.Add(SOnly+UniverseManager.Universes[i]._Name);
  ComboBox1.ItemIndex:=0;
  ComboBox1.Enabled:=UniverseManager.Count>0;

  UpdateComboBoxLanguage;
end;

procedure TFrameViewDMXProjectors.FixtureFilenameToAdd(const aFilename: string; aTargetUniverse: TDMXUniverse);
begin
  FFixtureFilenameToAdd:='';
  FTargetUniverse:=NIL;
  FState:=msReleased;
  if (aFileName='') or (aTargetUniverse=NIL) then exit;
  if not FLibraryFixtureToAdd.LoadFromFile(aFilename) then exit;

  FFixtureFilenameToAdd:=aFilename;
  FTargetUniverse:=aTargetUniverse;
end;

procedure TFrameViewDMXProjectors.ExitAddMode;
begin
  FixtureFilenameToAdd('', NIL);
  Redraw;
end;

procedure TFrameViewDMXProjectors.ProcessKey(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_A: if ssCtrl in Shift then begin
      Sel_All;
      Redraw;
      Key:=VK_UNKNOWN;
    end;

    VK_DELETE: if FMode=opViewModePrepaDMX
        then MIDeleteClick(NIL);

    VK_ESCAPE: begin
      ExitAddMode;
      Sel_None;
      Redraw;
    end;
  end;//case
end;

procedure TFrameViewDMXProjectors.AddToSelected(aFix: TDMXFixture);
begin
  aFix.Selected:=TRUE;
  InternalAddToSelected(aFix);
  FrameViewDMXCursors1.Add(aFix);
end;

procedure TFrameViewDMXProjectors.Sel_None;
begin
  InternalSel_None;
  ClearSelectionOnOtherWindows;
end;

procedure TFrameViewDMXProjectors.Sel_All;
var i: integer;
begin
  UniverseManager.Sel_All;
  UpdateSelected;

  FrameViewDMXCursors1.Clear;
  for i:=0 to High(FSelected) do
    FrameViewDMXCursors1.Add(FSelected[i]);

  if FSelectionMode=smNeutral
   then FSelectionMode:=smMoveItem;
end;

procedure TFrameViewDMXProjectors.Sel_ShiftPosition(delta: TPointF);
var i: integer;
begin
  for i:=0 to High(FSelected) do
    FSelected[i].ScreenPos:=FSelected[i].ScreenPos+delta;
end;

procedure TFrameViewDMXProjectors.Sel_Rotate(delta: single);
var i: integer;
begin
  for i:=0 to High(FSelected) do begin
    FSelected[i].Angle:=FSelected[i].Angle+delta;
  end;
end;

procedure TFrameViewDMXProjectors.Sel_Zoom(delta: single);
var i: integer;
begin
  for i:=0 to High(FSelected) do begin
    FSelected[i].Zoom:=EnsureRange(FSelected[i].Zoom+delta, FIXTURE_MINZOOM_VALUE, FIXTURE_MAXZOOM_VALUE);
  end;
end;

procedure TFrameViewDMXProjectors.Sel_ToogleFlipH;
var i: integer;
  flag: boolean;
begin
  flag:=FALSE;
  for i:=0 to High(FSelected) do begin
    FSelected[i].FlipH:=not FSelected[i].FlipH;
    flag:=TRUE;
  end;
  if flag then Project.SetModified;
end;

procedure TFrameViewDMXProjectors.Sel_ToogleFlipV;
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

procedure TFrameViewDMXProjectors.View_Center;
begin
  FNeedCenterView:=TRUE;
  Redraw;
end;

procedure TFrameViewDMXProjectors.ReloadTexture;
begin
  BGLVirtualScreen1.QueryLoadTextures;
  FrameViewDMXCursors1.BGLVirtualScreen1.QueryLoadTextures;
  FrameViewDMXCursors1.Redraw;
end;

procedure TFrameViewDMXProjectors.HideToolsWindows;
begin
  FrameViewDMXCursors1.HideToolsWindows;
end;

end.

