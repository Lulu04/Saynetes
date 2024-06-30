unit frame_viewdmxcursors;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Graphics, Dialogs, Menus,
  ExtCtrls, Buttons, Types, Math, LCLTranslator,
  BGRABitmap, BGRABitmapTypes, BGLVirtualScreen, BGRAOpenGL, BGRAOpenGLType,
  u_list_dmxuniverse, u_common, u_utils,
  u_resource_string, u_userdialogs, u_project_manager, lcl_utils,
  u_dmx_util;

type
  TFrameViewDMXCursors=class;

  { TViewCursor }
  PViewCursor=^TViewCursor;
  TViewCursor=record
  private
    function GetCursorPos: integer;
    function GetPercentValue: single;
    procedure SetCursorPos(AValue: integer);
    procedure SetPercentValue(AValue: single);
    function PercentToYCursor(aPercent: single): single;
    function YCursorToPercent(aY: single): single;
  public
    Renderer: TFrameViewDMXCursors;
    ChannelArea,
    EffectArea,
    ValueArea,
    CursorPathArea,
    ChannelNameArea,
    //ChannelRangeArea,
    CursorArea: TRectF;
    Channel: TDMXChannel;
    IsSourceForCopy: boolean;

    // update CursorArea from value in Channel^.PercentValue
    procedure UpdateCursorArea;

    function IsVisible: boolean;

    property Percent: single read GetPercentValue write SetPercentValue;
    property CursorPos: integer read GetCursorPos write SetCursorPos;
  end;

  { TViewFixture }
  PViewFixture=^TViewFixture;
  TViewFixture=record
   private
    Fixture: TDMXFixture;
    Cursors: array of TViewCursor;
    FixtureArea,
    DmxAdressArea,
    FixtureNameArea,
    FixtureDescriptionArea: TRectF;
    function GetSelected: boolean;
    procedure SetSelected(AValue: boolean);
    //function AdjustRect(aR: TRectF): TRectF;
   public
    Renderer: TFrameViewDMXCursors;
    IsSourceForRGBCopy: boolean;
    MouseIsOverFixtureDescriptionArea: boolean;
    procedure InitFrom(aFixture: TDmxFixture; aR: TRectF);
    function ChannelsCount: integer;
    function IsVisible: boolean;
    function MouseOverFixture(X, Y: integer): boolean;
    function MouseOverFixtureDescription(X, Y: integer): boolean;
    function ChannelNameUnderMouse(X, Y: integer): PViewCursor;
    function CursorUnderMouse(X, Y: integer): PViewCursor;

    procedure Redraw(BGLContext: TBGLContext);

    procedure RedrawSimple(BGLContext: TBGLContext);
    procedure DrawBackGroundOn(aTemp: TBGRABitmap);

    property Selected: boolean read GetSelected write SetSelected;
  end;

  TViewDMXCursorsMouseState = (
         msReleased,
         msMovingCursor,     // user is moving a cursor
         msScrollingView,    // user is scrolling the view with middle mouse button
         msSelectingSource); // user is selecting the channel source for the copy effect

  TMouseOverFixtureEvent = procedure(Sender: TObject; aFixture: TDMXFixture) of object;

const
  MIN_ZOOM_VALUE=0;
  MAX_ZOOM_VALUE=10;
type

  { TFrameViewDMXCursors }

  TFrameViewDMXCursors = class(TFrame)
    BGLVirtualScreen1: TBGLVirtualScreen;
    Label1: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MIUnlockChannel: TMenuItem;
    MILockChannel: TMenuItem;
    MICursorUnlock: TMenuItem;
    MICursorLock: TMenuItem;
    MICreateGroup: TMenuItem;
    MISelectOnAll: TMenuItem;
    MIOnlyOnAll: TMenuItem;
    MIAllOnAll: TMenuItem;
    Panel1: TPanel;
    PopChannel: TPopupMenu;
    ScrollBar1: TScrollBar;
    BZero: TSpeedButton;
    BOnlySameType: TSpeedButton;
    BAll: TSpeedButton;
    BOnlySelected: TSpeedButton;
    BRGBTools: TSpeedButton;
    BChannelTools: TSpeedButton;
    BGroup: TSpeedButton;
    BSelectNone: TSpeedButton;
    procedure BGLVirtualScreen1FramesPerSecond(Sender: TObject;
      BGLContext: TBGLContext; FramesPerSecond: integer);
    procedure BGLVirtualScreen1LoadTextures(Sender: TObject; {%H-}BGLContext: TBGLContext);
    procedure BGLVirtualScreen1MouseDown(Sender: TObject; Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure BGLVirtualScreen1MouseLeave(Sender: TObject);
    procedure BGLVirtualScreen1MouseMove(Sender: TObject; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure BGLVirtualScreen1MouseUp(Sender: TObject; Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure BGLVirtualScreen1MouseWheel(Sender: TObject; {%H-}Shift: TShiftState; WheelDelta: Integer; {%H-}MousePos: TPoint; var Handled: Boolean);
    procedure BGLVirtualScreen1Redraw(Sender: TObject; BGLContext: TBGLContext);
    procedure BGLVirtualScreen1Resize(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MICreateGroupClick(Sender: TObject);
    procedure MILockChannelClick(Sender: TObject);
    procedure MIOnlyOnAllClick(Sender: TObject);
    procedure MIAllOnAllClick(Sender: TObject);
    procedure MISelectOnAllClick(Sender: TObject);
    procedure MIUnlockChannelClick(Sender: TObject);
    procedure PopChannelPopup(Sender: TObject);
    procedure ScrollBar1Scroll(Sender: TObject; {%H-}ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure BZeroClick(Sender: TObject);
    procedure BAllClick(Sender: TObject);
    procedure BRGBToolsClick(Sender: TObject);
    procedure BChannelToolsClick(Sender: TObject);
    procedure BGroupClick(Sender: TObject);
    procedure BSelectNoneClick(Sender: TObject);
  private
    TextureCursors: IBGLTexture;
    FAdressFont,
    FFixtureFont,
    FEffectValueFont,
    FChannelNameFont,
    FChannelRangeFont: IBGLRenderedFont;
    FNeedRecreateOpenGlContextObjects: boolean;
    procedure CreateOpenGlContextObjects;
    procedure CreateCursorsTexture;
    procedure CreateFixtureFont(aFontHeight: integer);
    procedure CreateAdressFont(aFontHeight: integer);
    procedure CreateEffectValueFont(aFontHeight: integer);
    procedure CreateChannelNameFont(aFontHeight: integer);
    procedure ChannelRangeFont(aFontHeight: integer);
  protected
    FZoom: integer;
    FChannelWidth: integer;
    // give the width for one channel according to zoom
    function ChannelWidth: integer;
  private
    FMouseState: TViewDMXCursorsMouseState;
    FWorkingFixture: PViewFixture;
    FWorkingCursor: PViewCursor;
    function FixtureUnderMouse(X, Y: integer): PViewFixture;
    procedure InternalChannelSelection(aCur: PViewCursor; aState: boolean);//( aChan: TDMXChannel; aState: boolean);
    procedure InternalMoveCursor(aViewCursor: PViewCursor; delta: integer);
    procedure InternalSourceChannel_None;
    procedure InternalSourceFixture_None;
    procedure UpdateLevelOnViewProjector;
    procedure ProcessLoopMoveCursor;
    procedure ProcessLoopScrollView;
  private
    FCursorPathHeight: integer;
    FView: array of TViewFixture;
    CursorViewTotalArea: TRectF;
    FView_Begin: integer;
    FViewFirstIndex,
    FViewLastIndex: integer;
    function ModeEditSequence: boolean;
    function ModePrepaDMX: boolean;
    function ModeMainDMX: boolean;
    function GetView_End: integer;
    procedure SetView_Begin(AValue: integer);
    procedure SetZoom(AValue: integer);
    procedure UpdateViewFirstLastIndex;
    function SomeSelectedHaveRGB: boolean;
    procedure UpdateScrollBar;
  private
    FFixturesToDraw: ArrayOfDmxFixtures;
    FOnMouseOverFixture: TMouseOverFixtureEvent;
    function AdjustRectF(aR: TRectF): TRectF;
    function AdjustRect(aR: TRectF): TRect;
    procedure ComputeAreaFromFixturesToDraw;
  private
    procedure SourceChannel_None;
    procedure SourceFixture_None;
    procedure SourceChannel_Set(aCur: PViewCursor);
  private
    procedure AddChannelToOtherWindows(aChan: TDMXChannel);
    procedure RemoveChannelToOtherWindows(aChan: TDMXChannel);
    procedure ClearSelectionOnOtherWindows;
  private
    FToogleSpeedButtonManager: TToggleSpeedButtonManager;
    FCopyAll,
    FCopySelected,
    FCopySameType: boolean;
    function GetSourceFixtureForRGBCopy: TDMXFixture;
  private
    FInvalidateAlreadySent: boolean;
    FGUIMode: TGUIMode;
    procedure SetGUIMode(AValue: TGUIMode);
  public
    constructor Create(aOwner: TComponent);override;
    destructor Destroy; override;
    procedure EraseBackground({%H-}DC: HDC); override;

    procedure Redraw;
    procedure UpdateView;

    procedure Clear;
    procedure Add(aFixture: TDmxFixture);
    procedure Remove(aFixture: TDmxFixture);

    procedure SetChannelUnderMouse(aChan: TDMXChannel);

    function FixtureIsShowned(aFix: TDMXFixture): boolean;

    procedure HideToolsWindows;
    procedure SetVisibleChannelToolsWindow(aVisible: boolean);
    procedure SetVisibleRGBToolsWindow(aVisible: boolean);
    procedure SetVisibleGroupToolsWindow(aVisible: boolean);

    function GetTargetChannels: ArrayOfDmxChannels;
    function GetTargetFixtures: ArrayOfDMXFixtures;
    function GetSourceChannelForCopy: TDMXChannel;

    procedure SetSourceFixtureForRGBCopy(AValue: TDMXFixture);
    procedure SetSourceChannelForCopy(AValue: TDMXChannel);
   // property SourceFixtureForRGBCopy: TDMXFixture read GetSourceFixtureForRGBCopy write SetSourceFixtureForRGBCopy;

    procedure UpdateEditMode;

    property Zoom: integer read FZoom write SetZoom;
    // pixel index of the beginning of the view according the horiz. scrollbar position
    property View_Begin: integer read FView_Begin write SetView_Begin;
    // The time in seconds of the end (right) of the view
    property View_End: integer read GetView_End;

    property OnMouseOverFixture: TMouseOverFixtureEvent read FOnMouseOverFixture write FOnMouseOverFixture;

    property GUIMode: TGUIMode read FGUIMode write SetGUIMode;
  end;

implementation
uses u_dmxtools_channels, u_dmxtools_rgb, u_main_viewprojector,
  u_dmxtools_group, u_top_player;

{$R *.lfm}

const
  SPACE_BETWEEN_FIXTURE=15;
  FIXTURE_SPACE_BEFORE_AFTER_CHANNEL=5;
  SPACE_BETWEEN_CHANNEL=2;
var
    FColorBackground,
    FColorFixtureBackground,
    FColorBackgroundAdressDMX,
    FColorDMXAdress,
    FColorEffect,
    FColorValue,
    FColorChannelName,
    FColorChannelRange,
    FColorFixtureName,
    FColorChannelText: TBGRAPixel;
    FChannelUnderMouse: TDMXChannel=NIL;


{ TViewCursor }

function TViewCursor.GetCursorPos: integer;
begin
  Result := Round(CursorArea.Top);
end;

function TViewCursor.GetPercentValue: single;
begin
  Result := YCursorToPercent(CursorArea.Top);
end;

procedure TViewCursor.SetCursorPos(AValue: integer);
var h: single;
begin
  h := Renderer.TextureCursors.Height*0.5;
  AValue := trunc(EnsureRange(AValue, CursorPathArea.Top-h, CursorPathArea.Bottom-h));
  SetPercentValue(YCursorToPercent(AValue));
end;

procedure TViewCursor.SetPercentValue(AValue: single);
begin
  AValue := EnsureRange(AValue, 0.0, 1.0);
  CursorArea.Top := PercentToYCursor(AValue);
  CursorArea.Bottom := CursorArea.Top+Renderer.TextureCursors.Height;
  Channel.PercentValue := AValue;
end;

function TViewCursor.PercentToYCursor(aPercent: single): single;
begin
  Result := CursorPathArea.Height*(1-aPercent)+CursorPathArea.Top;
  Result := Result-Renderer.TextureCursors.FrameHeight*0.5;
end;

function TViewCursor.YCursorToPercent(aY: single): single;
begin
  aY := aY+Renderer.TextureCursors.FrameHeight*0.5;
  Result := 1-(aY-CursorPathArea.Top)/CursorPathArea.Height;
end;

procedure TViewCursor.UpdateCursorArea;
begin
  CursorArea.Top := PercentToYCursor(Channel.PercentValue);
  CursorArea.Bottom := CursorArea.Top+Renderer.TextureCursors.Height;
end;

function TViewCursor.IsVisible: boolean;
var r: TRectF;
begin
  r := Renderer.AdjustRectF(ChannelArea);
  Result := not((r.Left>Renderer.BGLVirtualScreen1.ClientWidth) or
              (r.Right<0));
end;

{ TViewFixture }

procedure TViewFixture.InitFrom(aFixture: TDmxFixture; aR: TRectF);
var i: integer;
    xx, ww: single;
  txt: string;
  r: TRectF;
begin
  Fixture := aFixture;
  SetLength(Cursors, Fixture.ChannelsCount);

  ww := FIXTURE_SPACE_BEFORE_AFTER_CHANNEL*2+Renderer.ChannelWidth*Fixture.ChannelsCount+
        SPACE_BETWEEN_CHANNEL*(Fixture.ChannelsCount-1);

  FixtureArea := RectF(aR.Left, aR.Top+5, aR.Left+ww, aR.Bottom-5);

  txt := '  512  ';//+Fixture^.Adress.ToString+' ';
  DmxAdressArea := RectF(FixtureArea.Left+1,
                       FixtureArea.Top+5,
                       FixtureArea.Left+1+Renderer.FAdressFont.TextWidth(txt),
                       FixtureArea.Top+5+Renderer.FAdressFont.TextHeight(txt));

  FixtureNameArea := RectF(FixtureArea.Left, FixtureArea.Top+1,
                         FixtureArea.Right, FixtureArea.Top+1+Renderer.FFixtureFont.EmHeight{ .FullHeight});

  FixtureDescriptionArea := RectF(FixtureNameArea.Left,
                                FixtureNameArea.Bottom,
                                FixtureNameArea.Right,
                                FixtureNameArea.Bottom+Renderer.FFixtureFont.EmHeight{FullHeight});

  r := RectF(FixtureArea.Left+FIXTURE_SPACE_BEFORE_AFTER_CHANNEL,
                     FixtureDescriptionArea.Bottom,
                     FixtureArea.Left+FIXTURE_SPACE_BEFORE_AFTER_CHANNEL+Renderer.ChannelWidth,
                     FixtureArea.Bottom-5);

  for i:=0 to High(Cursors) do begin
    Cursors[i].ChannelArea := r;
    Cursors[i].Renderer := Renderer;
    Cursors[i].EffectArea := RectF(r.Left, r.Top, r.Right, r.Top+Renderer.FEffectValueFont.EmHeight);
    Cursors[i].ValueArea := RectF(r.Left, Cursors[i].EffectArea.Bottom, r.Right, Cursors[i].EffectArea.Bottom+Renderer.FEffectValueFont.EmHeight{FullHeight});
    Cursors[i].ChannelNameArea := RectF(r.Left,
                                      r.Bottom-Renderer.FChannelNameFont.EmHeight*7,
                                      r.Right,
                                      r.Bottom);
    xx := r.Left+(r.Width-Renderer.TextureCursors.FrameWidth)*0.5; //cursor path is centered on channel area
    Cursors[i].CursorPathArea := RectF(xx, Cursors[i].ValueArea.Bottom+Renderer.TextureCursors.Height*0.5,
                                xx+Renderer.TextureCursors.FrameWidth-1,
                                Cursors[i].ChannelNameArea.Top-Renderer.TextureCursors.Height*0.5);//Cursors[i].ValueArea.Bottom+15+Renderer.TextureCursorPath.Height);
    Cursors[i].CursorArea.Left := Cursors[i].CursorPathArea.Left;
    Cursors[i].CursorArea.Right := Cursors[i].CursorPathArea.Right;

    Cursors[i].Channel := Fixture.Channels[i];
   // Cursors[i].Percent:=0;

    r.Left := r.Left+Renderer.ChannelWidth+SPACE_BETWEEN_CHANNEL;
    r.Right := r.Right+Renderer.ChannelWidth+SPACE_BETWEEN_CHANNEL;
  end;
end;

function TViewFixture.ChannelsCount: integer;
begin
  Result := Length(Cursors);
end;

function TViewFixture.IsVisible: boolean;
var r: TRectF;
begin
  r := Renderer.AdjustRectF(FixtureArea);
  Result := not((r.Left>Renderer.BGLVirtualScreen1.ClientWidth) or
               (r.Right<0));
end;

function TViewFixture.MouseOverFixture(X, Y: integer): boolean;
begin
  Result := FixtureArea.Contains(PointF(X,Y));
end;

function TViewFixture.MouseOverFixtureDescription(X, Y: integer): boolean;
begin
  Result := FixtureDescriptionArea.Contains(PointF(X,Y));
  MouseIsOverFixtureDescriptionArea := Result;
end;

function TViewFixture.CursorUnderMouse(X, Y: integer): PViewCursor;
var i: integer;
begin
  Result := NIL;
  for i:=0 to High(Cursors) do
    if InRange(X, Cursors[i].CursorArea.Left, Cursors[i].CursorArea.Right) and
       InRange(Y, Cursors[i].CursorArea.Top, Cursors[i].CursorArea.Bottom) then
    begin
       Result := @Cursors[i];
       exit;
    end;
end;

function TViewFixture.ChannelNameUnderMouse(X, Y: integer): PViewCursor;
var i: integer;
begin
  Result:=NIL;
  for i:=0 to High(Cursors) do
    if Cursors[i].ChannelNameArea.Contains(PointF(X,Y)) then
    begin
       Result := @Cursors[i];
       exit;
    end;
end;

procedure TViewFixture.Redraw(BGLContext: TBGLContext);
var r, rRenderer: TRectF;
  o: TRoundRectangleOptions;
  i: integer;
  c, c1: TBGRAPixel;
  xx, yy, deltay, w: single;
  flag: boolean;
  txt: string;
begin
  with BGLContext.Canvas do
  begin
    rRenderer := RectF(Renderer.BGLVirtualScreen1.ClientRect);
    // background (optimized)
    r := Renderer.AdjustRectF(FixtureArea);
    o := [];
    if r.Left+20 < rRenderer.Left then
    begin
      r.Left := rRenderer.Left;
      Include(o, rrTopLeftSquare);
      Include(o, rrBottomLeftSquare);
    end;
    if r.Right-20 > rRenderer.Right then
    begin
      r.Right := rRenderer.Right;
      Include(o, rrTopRightSquare);
      Include(o, rrBottomRightSquare);
    end;
    FillRoundRect(r.Left, r.Top, r.Right, r.Bottom, 20, 20, FColorFixtureBackground,o);
    //FillRect(r, FColorFixtureBackground);
    // source for RGB copy
    if IsSourceForRGBCopy then
    begin
      Rectangle(r.Left, r.Top, r.Right, r.Bottom, BGRA(255,80,255));
      Rectangle(r.Left-1, r.Top-1, r.Right+1, r.Bottom+1, BGRA(255,80,255));
    end;

    //FillRect(r, FColorFixtureBackground);
    // dmx adress
    //r:=Renderer.AdjustRectF(DmxAdressArea);
    //RoundRect(r.Left, r.Top, r.Right, r.Bottom, 20, 20, BGRA(100,255,255,200), FColorBackgroundAdressDMX{BGRA(100,255,255,90)},[]);
    //Renderer.FAdressFont.TextRect(r, Fixture.Adress.ToString, taCenter, tlCenter, FColorDMXAdress);

    // fixture name
    r := Renderer.AdjustRectF(FixtureNameArea);
    w := Renderer.FFixtureFont.TextWidth( Fixture._Name );
    Renderer.FFixtureFont.TextOut(r.Left+(r.Width-w)*0.5, r.Top, Fixture._Name, FColorFixtureName);

    // fixture description
    r := Renderer.AdjustRectF(FixtureDescriptionArea);
    w := Renderer.FFixtureFont.TextWidth( Fixture.Description );
    Renderer.FFixtureFont.TextOut(r.Left+(r.Width-w)*0.5, r.Top, Fixture.Description, FColorFixtureName);

    if MouseIsOverFixtureDescriptionArea then
      Rectangle(r.Left, r.Top, r.Right, r.Bottom, ColorToBGRA($00EAEAEA), BGRAPixelTransparent);

    for i:=0 to High(Cursors) do
     if Cursors[i].IsVisible then
     begin
      // current effect
      r := Renderer.AdjustRectF(Cursors[i].EffectArea);
      txt := EffectToText(Cursors[i].Channel.CurrentEffect);
      w := Renderer.FFixtureFont.TextWidth( txt );
      Renderer.FFixtureFont.TextOut(r.Left+(r.Width-w)*0.5, r.Top, txt, FColorFixtureName);

      // cursor value
      r := Renderer.AdjustRectF(Cursors[i].ValueArea);
      txt := Cursors[i].Channel.ByteValue.ToString;
      w := Renderer.FFixtureFont.TextWidth( txt );
      Renderer.FFixtureFont.TextOut(r.Left+(r.Width-w)*0.5, r.Top, txt, FColorFixtureName);

      // cursor path
      r := Renderer.AdjustRectF(Cursors[i].CursorPathArea);
//Rectangle(r.left,r.top,r.right,r.bottom,bgrawhite);
      xx := r.Left+r.Width*0.5-0.5;
      Line(xx,r.Top,xx,r.Bottom, BGRABlack);     // vertical axis
      Line(xx+1,r.Top,xx+1,r.Bottom, BGRABlack);
      yy := r.Top;
      deltay := r.Height/22; // on veux 10 grandes graduations
      flag := TRUE;
      c := ColorToBGRA( PercentColor( BGRAToColor(FColorFixtureBackground), -0.2));
      c1 := ColorToBGRA( PercentColor( BGRAToColor(FColorFixtureBackground), -0.4));
      repeat
       if flag then
         Line(r.Left, yy, r.Right, yy, c)
       else
       begin
         Line(r.Left, yy, r.Left+r.Width*0.25, yy, c1);
         Line(r.Right-r.Width*0.25, yy, r.Right, yy, c1);
       end;
       flag := not flag;
       yy := yy+deltay;
      until yy > r.Bottom;

      // channel under mouse in other windows
      if FChannelUnderMouse = Cursors[i].Channel then
        FillRect(r, BGRA(255,255,0,80));

      // cursor
      Cursors[i].UpdateCursorArea;
      r := Renderer.AdjustRectF(Cursors[i].CursorArea);
      Renderer.TextureCursors.SetFrame(Integer(Cursors[i].Channel.ChannelType)+1);
      Renderer.TextureCursors.Draw(r.Left, r.Top);
      if Cursors[i].Channel.Locked then
      begin
       Renderer.TextureCursors.SetFrame(Integer(Ord(High(TChannelType))+2));
       Renderer.TextureCursors.Draw(r.Left, r.Top);
      end;
      // Channel select state
      if Cursors[i].Channel.Selected and
         (Renderer.GUIMode <> guiPrepaDMX) then
        c := BGRA(255,255,120)
      else
        c := BGRA(0,0,0,0);
      //ChannelName
      r := Renderer.AdjustRectF(Cursors[i].ChannelNameArea);
      RoundRect(r.Left, r.Top, r.Right, r.Bottom, 8, 8, c, FColorChannelName,[]);
      w := Renderer.FChannelNameFont.TextWidth( Cursors[i].Channel._Name );
      Renderer.FChannelNameFont.TextOut(r.Left+(r.Width-w)*0.5, r.Top, Cursors[i].Channel._Name, FColorFixtureName);

      txt := Cursors[i].Channel.CurrentTextRange;
      Renderer.FChannelNameFont.TextRect(r, txt, taCenter, tlCenter, FColorChannelText);
      // Source for Copy
      r := Renderer.AdjustRectF(Cursors[i].ChannelArea);
      if Cursors[i].IsSourceForCopy then
        Rectangle(r.Left, r.Top, r.Right, r.Bottom, BGRA(255,80,255));
     end;
  end;
end;

procedure TViewFixture.RedrawSimple(BGLContext: TBGLContext);
var r, rRenderer: TRectF;
  i: integer;
  c, c1: TBGRAPixel;
  xx, yy, deltay: single;
  flag: boolean;
  txt: string;
begin
  with BGLContext.Canvas do
  begin
    rRenderer := RectF(Renderer.BGLVirtualScreen1.ClientRect);

    // source for RGB copy
    r := Renderer.AdjustRectF(FixtureArea);
    if r.Left+20 < rRenderer.Left then
      r.Left := rRenderer.Left;
    if r.Right-20 > rRenderer.Right then
      r.Right := rRenderer.Right;
    if IsSourceForRGBCopy then
    begin
      Rectangle(r.Left, r.Top, r.Right, r.Bottom, BGRA(255,80,255));
      Rectangle(r.Left-1, r.Top-1, r.Right+1, r.Bottom+1, BGRA(255,80,255));
    end;

    // fixture description
    r := Renderer.AdjustRectF(FixtureDescriptionArea);
    if MouseIsOverFixtureDescriptionArea then
      Rectangle(r.Left, r.Top, r.Right, r.Bottom, ColorToBGRA($00EAEAEA), BGRAPixelTransparent);

    for i:=0 to High(Cursors) do
     if Cursors[i].IsVisible then
     begin
      // current effect
      r := Renderer.AdjustRectF(Cursors[i].EffectArea);
      Renderer.FEffectValueFont.TextRect(r, EffectToText(Cursors[i].Channel.CurrentEffect), taCenter, tlCenter, FColorEffect);

      // cursor value
      r := Renderer.AdjustRectF(Cursors[i].ValueArea);
      Renderer.FEffectValueFont.TextRect(r, Cursors[i].Channel.ByteValue.ToString, taCenter, tlCenter, FColorValue);

      // channel under mouse in other windows
      if FChannelUnderMouse = Cursors[i].Channel then
        FillRect(r, BGRA(255,255,0,80));

      // cursor
      Cursors[i].UpdateCursorArea;
      r := Renderer.AdjustRectF(Cursors[i].CursorArea);
      Renderer.TextureCursors.SetFrame(Integer(Cursors[i].Channel.ChannelType)+1);
      Renderer.TextureCursors.Draw(r.Left, r.Top);
      if Cursors[i].Channel.Locked then
      begin
       Renderer.TextureCursors.SetFrame(Integer(Ord(High(TChannelType))+2));
       Renderer.TextureCursors.Draw(r.Left, r.Top);
      end;

      // Channel select state
      r := Renderer.AdjustRectF(Cursors[i].ChannelNameArea);
      if Cursors[i].Channel.Selected and
         (Renderer.GUIMode <> guiPrepaDMX) then
        RoundRect(r.Left, r.Top, r.Right, r.Bottom, 8, 8, BGRA(255,255,120), FColorChannelName,[]);

      //ChannelName
      txt := Cursors[i].Channel._Name + lineending +
             Cursors[i].Channel.CurrentTextRange;
      Renderer.FChannelNameFont.TextRect(r, txt, taCenter, tlCenter, FColorChannelText);

      // Source for Copy
      r := Renderer.AdjustRectF(Cursors[i].ChannelArea);
      if Cursors[i].IsSourceForCopy then
        Rectangle(r.Left,r.Top,r.Right,r.Bottom, BGRA(255,80,255));
     end;
  end;
end;

procedure TViewFixture.DrawBackGroundOn(aTemp: TBGRABitmap);
var rf, rRenderer: TRectF;
  r: TRect;
  o: TRoundRectangleOptions;
  i: integer;
  c, c1: TBGRAPixel;
  xx, yy, deltay: single;
  flag: boolean;
  txt: string;
begin
  with aTemp do
  begin
    rRenderer := RectF(Renderer.BGLVirtualScreen1.ClientRect);
    // background (optimized)
    rf := Renderer.AdjustRectF(FixtureArea);
    o := [];
    if rf.Left+20 < rRenderer.Left then
    begin
      rf.Left := rRenderer.Left;
      Include(o, rrTopLeftSquare);
      Include(o, rrBottomLeftSquare);
    end;
    if rf.Right-20 > rRenderer.Right then
    begin
      rf.Right := rRenderer.Right;
      Include(o, rrTopRightSquare);
      Include(o, rrBottomRightSquare);
    end;
    FillRoundRectAntialias(rf.Left, rf.Top, rf.Right, rf.Bottom, 20, 20, FColorFixtureBackground, o);

    // fixture name
    r := Renderer.AdjustRect(FixtureNameArea);
    aTemp.FontName := Renderer.FFixtureFont.Name;// 'Arial';
    aTemp.FontHeight := Renderer.FFixtureFont.FullHeight;
    aTemp.FontStyle := Renderer.FFixtureFont.Style;
    aTemp.FontQuality := Renderer.FFixtureFont.Quality;
    aTemp.TextRect(r, Fixture._Name, taCenter, tlTop, FColorFixtureName);

    // fixture description
    r := Renderer.AdjustRect(FixtureDescriptionArea);
    aTemp.TextRect(r, Fixture.Description, taCenter, tlTop, FColorFixtureName);

    // each cursor area
    for i:=0 to High(Cursors) do
     if Cursors[i].IsVisible then
     begin
      // cursor path
      rf := Renderer.AdjustRectF(Cursors[i].CursorPathArea);
//Rectangle(rf.left,rf.top,rf.right,rf.bottom,bgrawhite);
      xx := rf.Left+rf.Width*0.5-0.5;
      DrawLineAntialias(xx, rf.Top, xx, rf.Bottom, BGRABlack, 1); // vertical axis
      DrawLineAntialias(xx+1, rf.Top, xx+1, rf.Bottom, BGRABlack, 1);
      yy := rf.Top;
      deltay := rf.Height/22; // on veux 10 grandes graduations
      flag := TRUE;
      c := ColorToBGRA( PercentColor( BGRAToColor(FColorFixtureBackground), -0.2));
      c1 := ColorToBGRA( PercentColor( BGRAToColor(FColorFixtureBackground), -0.4));
      repeat
       if flag then
         DrawLineAntialias(rf.Left, yy, rf.Right, yy, c, 1)
       else
       begin
         DrawLineAntialias(rf.Left, yy, rf.Left+rf.Width*0.25, yy, c1, 1);
         DrawLineAntialias(rf.Right-rf.Width*0.25, yy, rf.Right, yy, c1, 1);
       end;
       flag := not flag;
       yy := yy+deltay;
      until yy > rf.Bottom;

      //ChannelName
      txt := Cursors[i].Channel._Name+lineending+Cursors[i].Channel.CurrentTextRange;
      rf := Renderer.AdjustRectF(Cursors[i].ChannelNameArea);
      FillRoundRectAntialias(rf.Left, rf.Top, rf.Right, rf.Bottom, 8, 8, FColorChannelName, []);
     end;
  end;
end;

procedure TViewFixture.SetSelected(AValue: boolean);
begin
  Fixture.Selected := AValue;
end;

{function TViewFixture.AdjustRectF(aR: TRectF): TRectF;
begin
  Result.Left:=aR.Left-Renderer.View_Begin;
  Result.Right:=aR.Right-Renderer.View_Begin;
  Result.Top:=aR.Top;
  Result.Bottom:=aR.Bottom;
end;  }

function TViewFixture.GetSelected: boolean;
begin
  Result := Fixture.Selected;
end;

{ TFrameViewDMXCursors }

procedure TFrameViewDMXCursors.BGLVirtualScreen1LoadTextures(Sender: TObject; BGLContext: TBGLContext);
begin
  CreateOpenGlContextObjects;
end;

procedure TFrameViewDMXCursors.BGLVirtualScreen1FramesPerSecond(
  Sender: TObject; BGLContext: TBGLContext; FramesPerSecond: integer);
begin
  Label1.Caption := 'FPS: '+FramesPerSecond.ToString;
end;

procedure TFrameViewDMXCursors.BGLVirtualScreen1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var pcur, pchannelName: PViewCursor;
    pfix: PViewFixture;
begin
  if Length(FView)=0 then exit;
  X := X+View_Begin;
  pfix := FixtureUnderMouse(X, Y);

  if (pfix = NIL) and (Button = mbLeft) then
  begin
    Redraw;
    exit;
  end;

  if (Button = mbRight) and (FMouseState = msReleased) and ModeEditSequence then
  begin
    if pfix = NIL then exit;
    FWorkingFixture := pfix;
    FWorkingCursor := pfix^.ChannelNameUnderMouse(X,Y);
    if FWorkingCursor <> NIL then
      PopChannel.PopUp;
    exit;
  end;

  if (Button = mbLeft) and (FMouseState = msReleased) then
  begin
    FWorkingCursor := pfix^.CursorUnderMouse(X, Y);
    if FWorkingCursor <> NIL then
    begin
      if not FWorkingCursor^.Channel.Locked then
        ProcessLoopMoveCursor;
      exit;
    end;

    pchannelName := pfix^.ChannelNameUnderMouse(X,Y);
    if (pchannelName <> NIL) and not ModePrepaDMX then
    begin
      // channel selection
      InternalChannelSelection(pchannelName, not pchannelName^.Channel.Selected);
      Redraw;
      exit;
    end;
  end;

  if (Button=mbMiddle) and (FMouseState=msReleased) then
     ProcessLoopScrollView;
end;

procedure TFrameViewDMXCursors.BGLVirtualScreen1MouseLeave(Sender: TObject);
begin
  if FOnMouseOverFixture <> NIL then
    FOnMouseOverFixture(Self, NIL);
end;

procedure TFrameViewDMXCursors.BGLVirtualScreen1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var pfix: PViewFixture;
    pCursor, pChannelName: PViewCursor;
    mouseOverChannelName, flag: boolean;
begin
  if Length(FView) = 0 then exit;
  if FViewFirstIndex = -1 then exit;

  X := X+View_Begin;
  pfix := FixtureUnderMouse(X, Y);

  if (FOnMouseOverFixture <> NIL) and not (FMouseState = msMovingCursor) then
    if pfix = NIL then
      FOnMouseOverFixture(Self, NIL)
    else
      FOnMouseOverFixture(Self, pfix^.Fixture);

  if (pfix <> NIL) and (FMouseState = msReleased) then
  begin
    mouseOverChannelName := pfix^.ChannelNameUnderMouse(X, Y) <> NIL;
    flag := False;

    case GUIMode of
      guiPrepaDMX:
        flag := (pfix^.MouseOverFixtureDescription(X, Y)) or mouseOverChannelName;

      guiMainDMX, guiEditSequence:
        flag := mouseOverChannelName;
    end;
{    flag := ModePrepaDMX and
          ((pfix^.MouseOverFixtureDescription(X, Y)) or
           (pChannelName <> NIL));

    flag := flag or (ModeEditSequence and
           (pChannelName <> NIL));   }

    pCursor := pfix^.CursorUnderMouse(X, Y);
    if pCursor <> NIL then
      flag := flag or (not pCursor^.Channel.Locked);

    if flag then
      BGLVirtualScreen1.Cursor := crHandPoint
    else
      BGLVirtualScreen1.Cursor := crDefault;

    if flag then Redraw;
  end;
end;

procedure TFrameViewDMXCursors.BGLVirtualScreen1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var pfix: PViewFixture;
  cur: PViewCursor;
  txt: string;
begin
  if Length(FView) = 0 then exit;
  X := X+View_Begin;

  if (FMouseState = msReleased) and (Button = mbRight) and ModePrepaDMX then
  begin
    pfix := FixtureUnderMouse(X, Y);
    if pfix <> NIL then
    begin
      if pfix^.MouseOverFixtureDescription(X, Y) then
      begin
        // input fixture description from user
        txt := pfix^.Fixture.Description;
        if UserInputNoSpecialChar(SNewDescription, SOk, SCancel, txt, mtConfirmation, TRUE) = mrOk then
        begin
          pfix^.Fixture.Description := txt;
          Redraw;
          Project.SetModified;
        end;
      end
      else
      begin
        cur := pfix^.ChannelNameUnderMouse(X, Y);
        if cur <> NIL then
        begin
          // input channel name from user
          txt := cur^.Channel._Name;
          if UserInputNoSpecialChar(SNewName, SOk, SCancel, txt, mtConfirmation, TRUE) = mrOk then
          begin
            cur^.Channel._Name := txt;
            Redraw;
            Project.SetModified;
          end;
        end;
      end;
    end;
  end;


  FMouseState := msReleased;
end;

procedure TFrameViewDMXCursors.BGLVirtualScreen1MouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  if Length(FView) = 0 then exit;
  if WheelDelta < 0 then
    Zoom := Zoom-2
  else
    Zoom := Zoom+2;
  UpdateView;
  handled := TRUE;
end;

procedure TFrameViewDMXCursors.BGLVirtualScreen1Redraw(Sender: TObject; BGLContext: TBGLContext);
var i: Integer;
begin
  FInvalidateAlreadySent := FALSE;

  if FNeedRecreateOpenGlContextObjects then
  begin
    CreateOpenGlContextObjects;
    ComputeAreaFromFixturesToDraw;
    FNeedRecreateOpenGlContextObjects := FALSE;
  end;

  with BGLContext.Canvas do
  begin
    FillRect(0, 0, Width, Height, FColorBackground);
    if Length(FView) = 0 then exit;
    if FViewFirstIndex = -1 then exit;

    for i:=FViewFirstIndex to FViewLastIndex do
      FView[i].Redraw(BGLContext);
  end;
end;

procedure TFrameViewDMXCursors.BGLVirtualScreen1Resize(Sender: TObject);
begin
  FNeedRecreateOpenGlContextObjects:=TRUE;
  UpdateView;
end;

procedure TFrameViewDMXCursors.MenuItem1Click(Sender: TObject);
var i: integer;
  cur: PViewCursor;
begin
  for i:=0 to High(FWorkingFixture^.Cursors) do
  begin
    cur := @FWorkingFixture^.Cursors[i];
    InternalChannelSelection(cur, cur^.Channel=FWorkingCursor^.Channel);
  end;
  Redraw;
end;

procedure TFrameViewDMXCursors.MenuItem2Click(Sender: TObject);
var i: integer;
begin
  for i:=0 to High(FWorkingFixture^.Cursors) do
   InternalChannelSelection(@FWorkingFixture^.Cursors[i], TRUE);
  Redraw;
end;

procedure TFrameViewDMXCursors.MICreateGroupClick(Sender: TObject);
begin
  FormDMXGroup.AddChannelGroup(GetTargetChannels);
end;

procedure TFrameViewDMXCursors.MIOnlyOnAllClick(Sender: TObject);
var i, j: integer;
  fix: TDMXFixture;
  chantype: TChannelType;
begin
  chantype := FWorkingCursor^.Channel.ChannelType;

  for i:=0 to High(FView) do
  begin
    fix := FView[i].Fixture;
    for j:=0 to High(FView[i].Cursors) do
     InternalChannelSelection(@FView[i].Cursors[j], fix.Channels[j].ChannelType=chantype);
  end;
  Redraw;
end;

procedure TFrameViewDMXCursors.MIAllOnAllClick(Sender: TObject);
var i, j: integer;
begin
  for i:=0 to High(FView) do
    for j:=0 to High(FView[i].Cursors) do
     InternalChannelSelection(@FView[i].Cursors[j], TRUE);
  Redraw;
end;

procedure TFrameViewDMXCursors.MISelectOnAllClick(Sender: TObject);
var i, j: integer;
  chantype: TChannelType;
begin
  chantype := FWorkingCursor^.Channel.ChannelType;
  for i:=0 to High(FView) do
    for j:=0 to High(FView[i].Cursors) do
     if (FView[i].Cursors[j].Channel.ChannelType=chantype) then
       InternalChannelSelection(@FView[i].Cursors[j], TRUE);
  Redraw;
end;

procedure TFrameViewDMXCursors.MILockChannelClick(Sender: TObject);
var i, j: integer;
begin
  for i:=0 to High(FView) do
    for j:=0 to High(FView[i].Cursors) do
     if FView[i].Cursors[j].Channel.Selected then
       FView[i].Cursors[j].Channel.Locked := TRUE;
  Redraw;
  FormViewProjector.Redraw;
  Project.SetModified;
end;

procedure TFrameViewDMXCursors.MIUnlockChannelClick(Sender: TObject);
var i, j: integer;
begin
  for i:=0 to High(FView) do
    for j:=0 to High(FView[i].Cursors) do
     if FView[i].Cursors[j].Channel.Selected then
       FView[i].Cursors[j].Channel.Locked := FALSE;
  Redraw;
  FormViewProjector.Redraw;
  Project.SetModified;
end;

procedure TFrameViewDMXCursors.PopChannelPopup(Sender: TObject);
var A: ArrayOfDmxChannels;
begin
  MIOnlyOnAll.Enabled := Length(FView)>1;
  MIAllOnAll.Enabled := Length(FView)>1;

  A := GetTargetChannels;
  MICreateGroup.Enabled := Length(A)>0;
end;

procedure TFrameViewDMXCursors.ScrollBar1Scroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  View_Begin := ScrollPos;
  Redraw;
end;

procedure TFrameViewDMXCursors.BZeroClick(Sender: TObject);
var i: integer;
begin
  TopPLayer.StopPreview;
  for i:=0 to High(FView) do
   FView[i].Fixture.SetAllChannelsToZero;
  Redraw;
  UpdateLevelOnViewProjector;
end;

procedure TFrameViewDMXCursors.BAllClick(Sender: TObject);
begin
  FCopyAll := FToogleSpeedButtonManager.Checked[BAll];
  FCopySelected := FToogleSpeedButtonManager.Checked[BOnlySelected];
  FCopySameType := FToogleSpeedButtonManager.Checked[BOnlySameType];
end;

procedure TFrameViewDMXCursors.BRGBToolsClick(Sender: TObject);
begin
  SetVisibleRGBToolsWindow(not FormDMXRGBTools.Visible);
end;

procedure TFrameViewDMXCursors.BChannelToolsClick(Sender: TObject);
begin
  SetVisibleChannelToolsWindow(not FormDMXChannelsTools.Visible);
end;

procedure TFrameViewDMXCursors.BGroupClick(Sender: TObject);
begin
  SetVisibleGroupToolsWindow(not FormDMXGroup.Visible);
end;

procedure TFrameViewDMXCursors.BSelectNoneClick(Sender: TObject);
var i, j: integer;
begin
  for i:=0 to High(FView) do
   for j:=0 to FView[i].ChannelsCount-1 do
    FView[i].Cursors[j].Channel.Selected := FALSE;
  Redraw;
end;

procedure TFrameViewDMXCursors.CreateCursorsTexture;
var temp, cur: TBGRABitmap;
  xx: integer;
  procedure AddCursor(ct: TChannelType);
  begin
    try
      try
        cur := TBGRABitmap.Create(DMXCursorImageFileNameFor(ct));
      except
        cur := TBGRABitmap.Create(20,30, BGRAWhite);
      end;
      temp.PutImage(xx, 0, cur, dmSet);
      inc(xx, 20);
    finally
      cur.Free;
    end;
  end;
begin
  TextureCursors:=NIL;
  temp := TBGRABitmap.Create((Ord(High(TChannelType))+2)*20, 30);
  xx := 0;
  AddCursor(ctConfig);
  AddCursor(ctMASTERDIMMER);
  AddCursor(ctDIMMER);
  AddCursor(ctRED);
  AddCursor(ctGREEN);
  AddCursor(ctBLUE);
  AddCursor(ctSTROBE);
  AddCursor(ctPAN);
  AddCursor(ctTILT);
  AddCursor(ctPANTILTSPEED);
  AddCursor(ctGOBO);
  AddCursor(ctGOBOROTATION);
  AddCursor(ctCOLORCHOICE);
  AddCursor(ctWHITE);
  AddCursor(ctAMBER);
  AddCursor(ctUV);
  AddCursor(ctSPEED);
  // locked cursor
  cur := TBGRABitmap.Create(CursorImagePath+'CursorLocked.png');
  temp.PutImage(xx, 0, cur, dmSet);
  inc(xx, 20);
  cur.Free;

  TextureCursors := BGLTexture(temp);
  TextureCursors.SetFrameSize(20, 30);
  temp.Free;
end;

procedure TFrameViewDMXCursors.CreateFixtureFont(aFontHeight: integer);
begin
  FFixtureFont := NIL;
  FFixtureFont := BGLFont('Arial', aFontHeight, []);
  FFixtureFont.Quality := fqSystemClearType;
end;

procedure TFrameViewDMXCursors.CreateAdressFont(aFontHeight: integer);
begin
  FAdressFont := NIL;
  FAdressFont := BGLFont('Arial', aFontHeight, [fsBold]);
  FAdressFont.Quality := fqSystemClearType;
end;

procedure TFrameViewDMXCursors.CreateEffectValueFont(aFontHeight: integer);
begin
  FEffectValueFont := NIL;
  FEffectValueFont := BGLFont('Arial', aFontHeight, []);
  FEffectValueFont.Quality := fqSystemClearType;
end;

procedure TFrameViewDMXCursors.CreateChannelNameFont(aFontHeight: integer);
begin
  FChannelNameFont := NIL;
  FChannelNameFont := BGLFont('Arial', aFontHeight, []);
  FChannelNameFont.Quality := fqSystemClearType;
end;

procedure TFrameViewDMXCursors.ChannelRangeFont(aFontHeight: integer);
begin
  FChannelRangeFont := NIL;
  FChannelRangeFont := BGLFont('Arial', aFontHeight, []);
  FChannelRangeFont.Quality := fqSystemClearType;
end;

function TFrameViewDMXCursors.ChannelWidth: integer;
begin
  Result := FChannelWidth;
end;

function TFrameViewDMXCursors.FixtureUnderMouse(X, Y: integer): PViewFixture;
var i: integer;
begin
  Result := NIL;
  if Length(FView) = 0 then exit;
  if FViewFirstIndex = -1 then exit;

  for i:=FViewFirstIndex to FViewLastIndex do
    if FView[i].MouseOverFixture(X, Y) then
    begin
      Result := PViewFixture(@FView[i]);
      exit;
    end;
end;

procedure TFrameViewDMXCursors.InternalChannelSelection(aCur: PViewCursor; aState: boolean);
begin
  aCur^.Channel.Selected := aState;
  if aCur^.IsSourceForCopy then
    SourceChannel_None;
  case aState of
    TRUE: AddChannelToOtherWindows(aCur^.Channel);
    FALSE: RemoveChannelToOtherWindows(aCur^.Channel);
  end;
end;

procedure TFrameViewDMXCursors.InternalMoveCursor(aViewCursor: PViewCursor; delta: integer);
var i, j: integer;
  flag: boolean;
  sourceChan, targetChan: TDMXChannel;
begin
  aViewCursor^.CursorPos := aViewCursor^.CursorPos+delta;
  sourceChan := aViewCursor^.Channel;
  for i:=0 to High(FView) do
    for j:=0 to High(FView[i].Cursors) do
    begin
      targetChan := FView[i].Cursors[j].Channel;
      // link same type
      flag := FCopySameType and
            ((targetChan.ChannelType = sourceChan.ChannelType)and not targetChan.Locked);
      // link all
      flag := flag or (FCopyAll and not targetChan.Locked);
      // link selected
      flag := flag or (FCopySelected and
                    (targetChan.Selected and not targetChan.Locked));
      if flag then
      begin
        FView[i].Cursors[j].CursorPos := aViewCursor^.CursorPos;
        FView[i].Cursors[j].Channel.CurrentEffect := deNOEFFECT;
      end;
    end;
end;

procedure TFrameViewDMXCursors.InternalSourceChannel_None;
var i, j: integer;
begin
  for i:=0 to High(FView) do
    for j:=0 to High(FView[i].Cursors) do
     FView[i].Cursors[j].IsSourceForCopy := FALSE;
end;

procedure TFrameViewDMXCursors.InternalSourceFixture_None;
var i: integer;
begin
  for i:=0 to High(FView) do
   FView[i].IsSourceForRGBCopy := FALSE;
end;

procedure TFrameViewDMXCursors.UpdateLevelOnViewProjector;
begin
  if Project.ProjectPrefs.ProjectorViewShowLevel then
    FormViewProjector.Redraw;
end;

procedure TFrameViewDMXCursors.ProcessLoopMoveCursor;
var yOrigin, yCurrent, delta: integer;
begin
  FMouseState := msMovingCursor;
  FWorkingCursor^.Channel.HandledByUser := TRUE;
  yOrigin := BGLVirtualScreen1.ScreenToClient(Mouse.CursorPos).y;
  repeat
    yCurrent := BGLVirtualScreen1.ScreenToClient(Mouse.CursorPos).y;
    delta := yCurrent-yOrigin;
    if delta <> 0 then
    begin
      InternalMoveCursor(FWorkingCursor, delta);
      Redraw;
      UpdateLevelOnViewProjector;
      yOrigin := yCurrent;
    end;
    Application.ProcessMessages;
  until FMouseState = msReleased;
  FWorkingCursor^.Channel.HandledByUser := FALSE;
end;

procedure TFrameViewDMXCursors.ProcessLoopScrollView;
var xOrigin, delta: integer;
  accu: single;
begin
  FMouseState := msScrollingView;
  xOrigin := BGLVirtualScreen1.ScreenToClient(Mouse.CursorPos).x;
  accu := View_Begin;
  BGLVirtualScreen1.Cursor := crSizeWE;
  repeat
    delta := BGLVirtualScreen1.ScreenToClient(Mouse.CursorPos).x-xOrigin;
    if delta <> 0 then
    begin
      accu := accu+delta*0.1;
      if accu < 0 then
        accu := 0;
      View_Begin := Trunc(accu);
      Redraw;
      Application.ProcessMessages;
    end;
  until FMouseState = msReleased;
  BGLVirtualScreen1.Cursor := crDefault;
end;

function TFrameViewDMXCursors.GetView_End: integer;
begin
  Result := View_Begin+BGLVirtualScreen1.ClientWidth-1;
end;

function TFrameViewDMXCursors.ModeEditSequence: boolean;
begin
  Result := FGUIMode = guiEditSequence;
end;

function TFrameViewDMXCursors.ModePrepaDMX: boolean;
begin
  Result := FGUIMode = guiPrepaDMX;
end;

function TFrameViewDMXCursors.ModeMainDMX: boolean;
begin
  Result := FGUIMode = guiMainDMX;
end;

procedure TFrameViewDMXCursors.HideToolsWindows;
begin
  SetVisibleChannelToolsWindow(FALSE);
  SetVisibleRGBToolsWindow(FALSE);
  SetVisibleGroupToolsWindow(FALSE);
end;

procedure TFrameViewDMXCursors.SetVisibleChannelToolsWindow(aVisible: boolean);
begin
  case aVisible of
    TRUE: begin
     FormDMXRGBTools.Hide;
     FormDMXChannelsTools.Show;
    end;
    FALSE: begin
      SourceChannel_None;
      FormDMXChannelsTools.Hide;
    end;
  end;//case
end;

procedure TFrameViewDMXCursors.SetVisibleRGBToolsWindow(aVisible: boolean);
begin
  case aVisible of
    TRUE: begin
      FormDMXChannelsTools.Hide;
      FormDMXRGBTools.Show;
    end;
    FALSE: begin
      SourceFixture_None;
      FormDMXRGBTools.Hide;
    end;
  end;//case
end;

procedure TFrameViewDMXCursors.SetVisibleGroupToolsWindow(aVisible: boolean);
begin
  case aVisible of
    TRUE: begin
      FormDMXGroup.Show;
    end;
    FALSE: begin
      FormDMXGroup.Hide;
    end;
  end;//case
end;

function TFrameViewDMXCursors.GetTargetChannels: ArrayOfDmxChannels;
var f, c, k: integer;
  chan: TDMXChannel;
begin
  Result := NIL;
  SetLength(Result, 0);
  k := 0;
  for f:=0 to High(FView) do
    for c:=0 to High(FView[f].Cursors) do
    begin
      chan := FView[f].Cursors[c].Channel;
      if chan.Selected then
      begin
        SetLength(Result, k+1);
        Result[k] := chan;
        inc(k);
      end;
    end;
end;

function TFrameViewDMXCursors.GetTargetFixtures: ArrayOfDMXFixtures;
var i: integer;
begin
  Result := NIL;
  SetLength(Result, Length(FView));
  for i:=0 to High(FView) do
   Result[i] := FView[i].Fixture;
end;

function TFrameViewDMXCursors.GetSourceChannelForCopy: TDMXChannel;
var i, j: integer;
begin
  for i:=0 to High(FView) do
   for j:=0 to High(FView[i].Cursors) do
    if FView[i].Cursors[j].IsSourceForCopy then
    begin
     Result := FView[i].Cursors[j].Channel;
     exit;
    end;
  Result := NIL;
end;

procedure TFrameViewDMXCursors.SetView_Begin(AValue: integer);
begin
  FView_Begin := AValue;

  UpdateViewFirstLastIndex;
  UpdateScrollBar;
end;

procedure TFrameViewDMXCursors.SetZoom(AValue: integer);
begin
  AValue := EnsureRange(AValue, MIN_ZOOM_VALUE, MAX_ZOOM_VALUE);
  if FZoom = AValue then
    Exit;

  FZoom := AValue;
  FNeedRecreateOpenGlContextObjects := TRUE;
end;

procedure TFrameViewDMXCursors.UpdateViewFirstLastIndex;
var i: integer;
begin
  FViewFirstIndex := -1;
  FViewLastIndex := -1;

  for i:=0 to High(FView) do
    if FView[i].IsVisible then
    begin
      FViewFirstIndex := i;
      break;
    end;
  if FViewFirstIndex = -1 then
    exit;
  for i:=High(FView) downto 0 do
    if FView[i].IsVisible then
    begin
      FViewLastIndex := i;
      break;
    end;
end;

function TFrameViewDMXCursors.SomeSelectedHaveRGB: boolean;
var i: integer;
begin
  for i:=0 to High(FView) do
   if FView[i].Fixture.HasRGBChannel then
   begin
     Result := TRUE;
     exit;
   end;
  Result := FALSE;
end;

procedure TFrameViewDMXCursors.UpdateScrollBar;
begin
  ScrollBar1.Max := Trunc(CursorViewTotalArea.Right);
  ScrollBar1.Position := View_Begin;
end;

function TFrameViewDMXCursors.AdjustRectF(aR: TRectF): TRectF;
begin
  Result.Left := aR.Left-View_Begin;
  Result.Right := aR.Right-View_Begin;
  Result.Top := aR.Top;
  Result.Bottom := aR.Bottom;
end;

function TFrameViewDMXCursors.AdjustRect(aR: TRectF): TRect;
begin
  Result.Left := Round(aR.Left-View_Begin);
  Result.Right := Round(aR.Right-View_Begin);
  Result.Top := Round(aR.Top);
  Result.Bottom := Round(aR.Bottom);
end;

procedure TFrameViewDMXCursors.ComputeAreaFromFixturesToDraw;
var i: integer;
  r: TRectF;
begin
  SetLength(FView, Length(FFixturesToDraw));

  CursorViewTotalArea := RectF(0,0,1,BGLVirtualScreen1.ClientHeight);

  r.Left := SPACE_BETWEEN_FIXTURE;
  r.Top := CursorViewTotalArea.Top;
  r.Bottom := CursorViewTotalArea.Height;

  for i:=0 to High(FFixturesToDraw) do
  begin
    FView[i].Renderer := Self;
    FView[i].InitFrom(FFixturesToDraw[i], r);

    r.Left := r.Left+FView[i].FixtureArea.Width+SPACE_BETWEEN_FIXTURE;
    r.Right := r.Right+FView[i].FixtureArea.Width+SPACE_BETWEEN_FIXTURE;
    CursorViewTotalArea.Right := r.Left;
  end;
end;

procedure TFrameViewDMXCursors.UpdateView;
var dx: integer;
begin
  ComputeAreaFromFixturesToDraw;
  // if possible center the view on fixture
  dx:=Trunc(CursorViewTotalArea.Width)-BGLVirtualScreen1.ClientWidth;
  if dx = 0 then
    View_Begin:=0
  else if dx < 0 then
    View_Begin := dx div 2
  else
      //here the view can fit the whole fixture-> we ensure the view show some of them
    View_Begin := EnsureRange(View_Begin, Trunc(CursorViewTotalArea.Left),
                              Trunc(CursorViewTotalArea.Right)-BGLVirtualScreen1.ClientWidth);
  Redraw;
end;

procedure TFrameViewDMXCursors.SourceChannel_None;
begin
  InternalSourceChannel_None;
end;

procedure TFrameViewDMXCursors.SourceFixture_None;
begin
  InternalSourceFixture_None;
end;

procedure TFrameViewDMXCursors.SourceChannel_Set(aCur: PViewCursor);
begin
  InternalSourceChannel_None;
  aCur^.IsSourceForCopy := TRUE;
end;

procedure TFrameViewDMXCursors.AddChannelToOtherWindows(aChan: TDMXChannel);
begin
  FormDMXChannelsTools.UserHaveSelected(aChan);
end;

procedure TFrameViewDMXCursors.RemoveChannelToOtherWindows(aChan: TDMXChannel);
begin
  FormDMXChannelsTools.UserHaveRemoved(aChan);
end;

procedure TFrameViewDMXCursors.ClearSelectionOnOtherWindows;
begin
  FormDMXChannelsTools.ClearSelectedChannels;
end;

function TFrameViewDMXCursors.GetSourceFixtureForRGBCopy: TDMXFixture;
var i: integer;
begin
  for i:=0 to High(FView) do
   if FView[i].IsSourceForRGBCopy then
   begin
    Result := FView[i].Fixture;
    exit;
   end;
  Result := NIL;
end;

procedure TFrameViewDMXCursors.SetGUIMode(AValue: TGUIMode);
begin
  FGUIMode := AValue;

  case FGUIMode of
    guiMainDMX:
      begin
        BRGBTools.Visible := True;
        BChannelTools.Visible := True;
        BGroup.Visible := True;
      end;

    guiPrepaDMX:
      begin
        BRGBTools.Visible := False;
        BChannelTools.Visible := False;
        BGroup.Visible := True;
      end;

    guiEditSequence:
      begin
        BRGBTools.Visible := True;
        BChannelTools.Visible := True;
        BGroup.Visible := True;
      end;
  end;
end;

procedure TFrameViewDMXCursors.SetSourceFixtureForRGBCopy(AValue: TDMXFixture);
var i: integer;
begin
  for i:=0 to High(FView) do
   FView[i].IsSourceForRGBCopy := FView[i].Fixture=AValue;
  Redraw;
end;

procedure TFrameViewDMXCursors.SetSourceChannelForCopy(AValue: TDMXChannel);
var i, j: integer;
begin
  for i:=0 to High(FView) do
    for j:=0 to High(FView[i].Cursors) do
      FView[i].Cursors[j].IsSourceForCopy := FView[i].Cursors[j].Channel = AValue;
  Redraw;
end;

procedure TFrameViewDMXCursors.UpdateEditMode;
begin
  FormDMXChannelsTools.UpdateEditMode;
  FormDMXRGBTools.UpdateEditMode;
  FormDMXGroup.UpdateEditMode
end;

procedure TFrameViewDMXCursors.CreateOpenGlContextObjects;
  function ApplyZoomOnFontHeight(aFH: integer): integer;
  begin
    Result := Trunc(aFH+Zoom*0.4);
    Result := EnsureRange(Result, 6, 30);
  end;
begin
  CreateCursorsTexture;

  CreateFixtureFont(ApplyZoomOnFontHeight(7));
  CreateAdressFont(ApplyZoomOnFontHeight(7));
  CreateEffectValueFont(ApplyZoomOnFontHeight(6));
  CreateChannelNameFont(ApplyZoomOnFontHeight(6));
  ChannelRangeFont(ApplyZoomOnFontHeight(6));

  FChannelWidth := Trunc(TextureCursors.FrameWidth+Zoom*4);//50+trunc((Zoom-MAX_ZOOM_VALUE div 2)*5);
  if FChannelWidth < TextureCursors.FrameWidth then
    FChannelWidth := TextureCursors.FrameWidth;
end;

constructor TFrameViewDMXCursors.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FCursorPathHeight := 255;
  FMouseState := msReleased;
  FGUIMode := guiMainDMX;
  FChannelWidth := 50;
  FZoom := 5;

  BGLVirtualScreen1.Color := RGBToColor(25,15,10);
  {$ifdef MSWINDOWS}
  BGLVirtualScreen1.MultiSampling := 2;
  {$endif}

  FToogleSpeedButtonManager := TToggleSpeedButtonManager.Create;
  FToogleSpeedButtonManager.ToogleType := tsbNoneOrOne;
  FToogleSpeedButtonManager.SetActivatedColors($0003C4FC, $00272727);
  FToogleSpeedButtonManager.SetDeactivatedColors($00004B62, $00EAEAEA);
  FToogleSpeedButtonManager.Add(BOnlySameType, FALSE);
  FToogleSpeedButtonManager.Add(BAll, FALSE);
  FToogleSpeedButtonManager.Add(BOnlySelected, FALSE);

  FColorBackground := BGRA(51,51,51);
  FColorFixtureBackground := BGRA(97,90,78);
  FColorBackgroundAdressDMX := ColorToBGRA(PercentColor(BGRAToColor(FColorFixtureBackground), -0.5));
  FColorBackgroundAdressDMX.alpha:=200;
  FColorDMXAdress := BGRA(255,255,200,200);
  FColorFixtureName := ColorToBGRA($009FD1EC);
  FColorEffect := BGRA(255,255,255,230);
  FColorValue := BGRAWhite;
  FColorChannelName := BGRA(0,0,0);//ColorToBGRA($006FBDE3);//  BGRA(80,50,25);
  FColorChannelName.alpha:=70;
  FColorChannelRange := BGRA(0,0,0);//ColorToBGRA($002D9FD7);//  BGRA(70,40,20);
  FColorChannelRange.alpha := 60;
  FColorChannelText := FColorFixtureName;//BGRA(220,200,200);
end;

destructor TFrameViewDMXCursors.Destroy;
begin
  FToogleSpeedButtonManager.Free;
  inherited Destroy;
end;

procedure TFrameViewDMXCursors.EraseBackground(DC: HDC);
begin
//do nothing here
end;

procedure TFrameViewDMXCursors.Redraw;
begin
  if not FInvalidateAlreadySent then
  begin
    BGLVirtualScreen1.Invalidate;
    FInvalidateAlreadySent:=TRUE;
  end;
end;

procedure TFrameViewDMXCursors.Clear;
begin
  SourceChannel_None;
  SourceFixture_None;
  if Length(FFixturesToDraw)=0 then
    exit;

  SetLength(FFixturesToDraw, 0);
  UpdateView;
  ClearSelectionOnOtherWindows;
end;

procedure TFrameViewDMXCursors.Add(aFixture: TDmxFixture);
var i: integer;
begin
  // we don't add the fixture if it's already in the list
  if FixtureIsShowned(aFixture) then
    exit;

  i := Length(FFixturesToDraw);
  SetLength(FFixturesToDraw, i+1);
  FFixturesToDraw[i] := aFixture;
  UpdateView;

  for i:=0 to aFixture.ChannelsCount-1 do
    AddChannelToOtherWindows(aFixture.Channels[i]);
end;

procedure TFrameViewDMXCursors.Remove(aFixture: TDmxFixture);
var i: integer;
begin
  for i:=0 to aFixture.ChannelsCount-1 do
    RemoveChannelToOtherWindows(aFixture.Channels[i]);

  for i:=0 to High(FFixturesToDraw) do
    if FFixturesToDraw[i] = aFixture then
    begin
      Delete(FFixturesToDraw, i, 1);
      UpdateView;
      exit;
    end;
end;

procedure TFrameViewDMXCursors.SetChannelUnderMouse(aChan: TDMXChannel);
begin
  FChannelUnderMouse := aChan;
  Redraw;
end;

function TFrameViewDMXCursors.FixtureIsShowned(aFix: TDMXFixture): boolean;
var i: integer;
begin
  for i:=0 to High(FFixturesToDraw) do
    if FFixturesToDraw[i]=aFix then
    begin
      Result := TRUE;
      exit;
    end;
  Result := FALSE;
end;

end.

