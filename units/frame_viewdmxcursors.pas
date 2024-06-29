unit frame_viewdmxcursors;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Graphics, Dialogs, Menus,
  ExtCtrls, Buttons, Types, Math, LCLTranslator, ComCtrls,
  BGRABitmap, BGRABitmapTypes,
  u_list_dmxuniverse, u_common, u_utils,
  u_resource_string, u_userdialogs, u_project_manager, u_notebook_util,
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
    function PercentToYCursor(aPercent: single): integer;
    function YCursorToPercent(aY: integer): single;
  public
    Renderer: TFrameViewDMXCursors;
    ChannelArea,
    EffectArea,
    ValueArea,
    CursorPathArea,
    ChannelNameArea,
    CursorArea,
    CursorAreaLastDrawn,
    BackgroundArea: TRect;
    Channel: TDMXChannel;
    IsSourceForCopy: boolean;

    // update CursorArea from value in Channel^.PercentValue
    procedure UpdateCursorArea;

    procedure UpdateIsVisibleOnView;
    procedure DrawDynamicPartOn(aCanvas: TCanvas; aDrawAll: boolean);

    property Percent: single read GetPercentValue write SetPercentValue;
    property CursorPos: integer read GetCursorPos write SetCursorPos;
  end;

  { TViewFixture }
  PViewFixture = ^TViewFixture;
  TViewFixture = record
   private
    Fixture: TDMXFixture;
    Cursors: array of TViewCursor;
    FixtureArea,
    DmxAdressArea,
    FixtureNameArea,
    FixtureDescriptionArea,
    BackgroundArea: TRect;
    function GetSelected: boolean;
    procedure SetSelected(AValue: boolean);
   public
    Renderer: TFrameViewDMXCursors;
    IsSourceForRGBCopy: boolean;
    MouseIsOverFixtureDescriptionArea: boolean;
    procedure InitFrom(aFixture: TDmxFixture; aR: TRect);
    function ChannelsCount: integer;
    procedure UpdateIsVisibleOnView;
    function MouseOverFixture(X, Y: integer): boolean;
    function MouseOverFixtureDescription(X, Y: integer): boolean;
    function ChannelNameUnderMouse(X, Y: integer): PViewCursor;
    function CursorUnderMouse(X, Y: integer): PViewCursor;

    procedure DrawBackgroundOn(aCanvas: TCanvas);
    procedure DrawDynamicPartOn(aCanvas: TCanvas; aDrawAll: boolean);

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
    PB: TPaintBox;
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
    procedure Panel1Resize(Sender: TObject);
    procedure PBMouseDown(Sender: TObject; Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure PBMouseLeave(Sender: TObject);
    procedure PBMouseMove(Sender: TObject; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure PBMouseUp(Sender: TObject; Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure PBMouseWheel(Sender: TObject; {%H-}Shift: TShiftState; WheelDelta: Integer; {%H-}MousePos: TPoint; var Handled: Boolean);
    procedure PBPaint(Sender: TObject);
    procedure PBResize(Sender: TObject);
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
    ImageLock,
    FCursorBackGround: TBGRABitmap;
    FTextStyleForTextRect: TTextStyle;

    FAdressFont,
    FFixtureFont,
    FEffectValueFont,
    FChannelNameFont,
    FChannelRangeFont: TFont;
    procedure CreateGraphicObjects;
    procedure DeleteGraphicObjects;
    procedure AdjustGraphicObjectsForZoom;
    procedure AdjustCursorBackGroundImage(aCursorPathHeight: integer);
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
    FTotalViewArea: TRect;
    FView_Begin: integer;
    FViewFirstIndex,
    FViewLastIndex: integer;
    function ModeEditSequence: boolean;
    function ModePrepaDMX: boolean;
    function ModeMainDMX: boolean;
    function GetView_End: integer;
    procedure SetView_Begin(AValue: integer);
    procedure SetZoom(AValue: integer);
    function SomeSelectedHaveRGB: boolean;
    procedure UpdateScrollBar;
  private
    FFixturesToDraw: ArrayOfDmxFixtures;
    FOnMouseOverFixture: TMouseOverFixtureEvent;
    function AdjustRect(aR: TRect): TRect;
    function AdjustX(aX: integer): integer;
    procedure ComputeAreaFromFixturesToDraw;
    procedure UpdateWhoIsVisibleOnView;
//    procedure UpdateViewFirstLastIndex;
  private
    procedure SourceChannel_None;
    procedure SourceFixture_None;
    procedure SourceChannel_Set(aCur: PViewCursor);
  private
    procedure AddChannelToOtherWindows(aChan: TDMXChannel);
    procedure RemoveChannelToOtherWindows(aChan: TDMXChannel);
    procedure ClearChannelOnOtherWindows;
  private
    FToogleSpeedButtonManager: TToggleSpeedButtonManager;
    FCopyAll,
    FCopySelected,
    FCopySameType: boolean;
    FCurrentSourceChannelForCopy: TDMXChannel;
    function GetSourceFixtureForRGBCopy: TDMXFixture;
  private
    FInvalidateAlreadySent: boolean;
    FGUIMode: TGUIMode;
    FParentViewProjector: TFrame;
    procedure SetGUIMode(AValue: TGUIMode);
  private
    FImage: TBitmap;
    procedure DrawBackgroundOnFImage;
  public
    constructor Create(aOwner: TComponent);override;
    destructor Destroy; override;
    procedure EraseBackground({%H-}DC: HDC); override;

    procedure RedrawAll;
    procedure RedrawVisibleFixtures;
    procedure RedrawVisibleCursors;

    // redraw only 1 fixture. before call it, check if the fixture is visible
    // on the cursor view.
    procedure RedrawFixture(aFix: TDMXFixture);
    procedure RedrawRGBChannelsOnFixture(aFix: TDMXFixture);
    // used in Universe manager to update the position of the cursor and text.
    procedure RedrawCursor(aChan: TDMXChannel; aDrawAll: boolean);

    procedure UpdateView;
    procedure UpdateStringAfterLanguageChange;

    procedure Clear;
    procedure Add(aFixture: TDmxFixture; aUpdateView: boolean);
    procedure Remove(aFixture: TDmxFixture);

    // highlight the channel under the mouse in an another tool's window
    procedure SetChannelUnderMouse(aChan: TDMXChannel);

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
    property ParentViewProjector: TFrame read FParentViewProjector write FParentViewProjector;
  end;

implementation
uses u_dmxtools_channels, u_dmxtools_rgb,
  u_dmxtools_group, u_sequence_player, frame_viewprojectors, u_apputils,
  utilitaire_bgrabitmap;

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
    FColorChannelNameBackGround,
    FColorChannelRange,
    FColorFixtureName,
    FColorDescription,
    FColorChannelText: TColor;
    FChannelUnderMouse: TDMXChannel=NIL;


{ TViewCursor }

function TViewCursor.GetCursorPos: integer;
begin
  Result := CursorArea.Top;
end;

function TViewCursor.GetPercentValue: single;
begin
  Result := YCursorToPercent(CursorArea.Top);
end;

procedure TViewCursor.SetCursorPos(AValue: integer);
begin
  AValue := EnsureRange(AValue, CursorPathArea.Top, CursorPathArea.Bottom);
  SetPercentValue(YCursorToPercent(AValue));
end;

procedure TViewCursor.SetPercentValue(AValue: single);
begin
  AValue := EnsureRange(AValue, 0.0, 1.0);
  CursorArea.Top := PercentToYCursor(AValue);
  CursorArea.Bottom := CursorArea.Top+ImageCursorSize.cy;
  Channel.PercentValue := AValue;
end;

function TViewCursor.PercentToYCursor(aPercent: single): integer;
begin
  Result := Round(CursorPathArea.Height*(1-aPercent)+CursorPathArea.Top);
end;

function TViewCursor.YCursorToPercent(aY: integer): single;
begin
  Result := 1-(aY-CursorPathArea.Top)/CursorPathArea.Height;
end;

procedure TViewCursor.UpdateCursorArea;
begin
  if Channel.FFlashIsActive then
    CursorArea.Top := PercentToYCursor(Channel.FFlashValue)
  else
    CursorArea.Top := PercentToYCursor(Channel.PercentValue);
  CursorArea.Bottom := CursorArea.Top + ImageCursorSize.cy;
end;

procedure TViewCursor.UpdateIsVisibleOnView;
var r: TRect;
begin
  r := Renderer.AdjustRect(ChannelArea);
  Channel.IsVisibleOnViewCursor := not((r.Left > Renderer.PB.ClientWidth) or
                                       (r.Right <= 0));
end;

procedure TViewCursor.DrawDynamicPartOn(aCanvas: TCanvas; aDrawAll: boolean);
var
  r, r1: TRect;
  txt: String;
  b: byte;
  w: Integer;
  ima: TBGRABitmap;
    procedure UseFont(aFont: TFont);
    begin
      aCanvas.Font.Name := aFont.Name;
      aCanvas.Font.Height := aFont.Height;
      aCanvas.Font.Style := aFont.Style;
    end;
begin
  with aCanvas do
  begin
   Renderer.FTextStyleForTextRect.Wordbreak := False; // can't go to the next line

   // Effect icon
   if (Channel.EffectPainted <> Ord(Channel.CurrentEffect)) or aDrawAll then begin
     Channel.EffectPainted := Ord(Channel.CurrentEffect);
     // erase
     r := Renderer.AdjustRect(EffectArea);
     r.Right := r.Right + 1;
     r.Bottom := r.Bottom + 1;
     Brush.Color := FColorFixtureBackground;
     Brush.Style := bsSolid;
     Pen.Style := psClear;
     Rectangle(r);
     // draw
     ima := GetDMXEffectImageFor(Channel.CurrentEffect);
     if ima <> NIL then ima.Draw(aCanvas, r.Left, r.Top, False);
   end;

   if Channel.FFlashIsActive then b := PercentToDMXByte(Channel.FFlashValue)
     else b := PercentToDMXByte(Channel.PercentValue);

   if (Channel.ByteValuePainted <> b) or
      (Channel.LockedPainted = Channel.Locked) or
      aDrawAll then begin
     Channel.ByteValuePainted := b;
     // erase previous value
     r := Renderer.AdjustRect(ValueArea);
     Brush.Color := FColorFixtureBackground;
     Brush.Style := bsSolid;
     Pen.Style := psClear;
     Rectangle(r);
     // draw cursor value
     txt := b.ToString;
     w := Font.GetTextWidth( txt );
     Font.Color := FColorValue;
     TextOut(r.Left+(r.Width-w) shr 1, r.Top, txt);

     if aDrawAll then begin
       r := Renderer.AdjustRect(CursorPathArea);
       Renderer.FCursorBackGround.Draw(aCanvas, r.Left, r.Top);
     end else begin
       // erase only previous cursor rect
       r1 := Renderer.AdjustRect(CursorAreaLastDrawn);
       r.Left := 0;
       r.Right := CursorArea.width;
       r.Top := CursorAreaLastDrawn.Top - CursorPathArea.Top;
       r.Bottom := CursorAreaLastDrawn.Bottom - CursorPathArea.Top;
       Renderer.FCursorBackGround.DrawPart(r, aCanvas, r1.Left, r1.Top, True);
     end;

     // draw cursor
     UpdateCursorArea;
     CursorAreaLastDrawn := CursorArea;
     r := Renderer.AdjustRect(CursorArea);
     ImageCursors[Channel.ChannelType].Draw(aCanvas, r.Left, r.Top, False);
     if Channel.Locked then
       Renderer.ImageLock.Draw(aCanvas, r.Left, r.Top, False);
     Channel.LockedPainted := Channel.Locked;
   end;

   // channel under mouse in other windows
   if FChannelUnderMouse = Channel then
   begin
    Brush.Style := bsClear;
    Pen.Style := psSolid;
    Pen.Color := $0003C4FC;
    Pen.Width := 1;
    r := Renderer.AdjustRect(CursorPathArea);
    r.Bottom := r.Top + Renderer.FCursorBackGround.Height;
    Rectangle(r.Left+1, r.Top+1, r.Right-1, r.Bottom);
   end;

   r := Renderer.AdjustRect(ChannelNameArea);
   w := Channel.CurrentRangeIndex;
   // Channel select state
   if (Channel.RangeIndexPainted <> w) or
      (Channel.Selected <> Channel.SelectedPainted) or
       aDrawAll then
   begin
    if Channel.Selected and (Renderer.GUIMode <> guiPrepaDMX) then
      Pen.Color := $0003C4FC // RGBToColor(255,255,120)
    else
      Pen.Color := FColorChannelNameBackGround;
    Pen.Width := 1;
    Pen.Style := psSolid;
    Brush.Style := bsSolid;
    Brush.Color := FColorChannelNameBackGround;
    Rectangle(r);
    //RoundRect(r, 8, 8);

    Channel.RangeIndexPainted := w;
    Channel.SelectedPainted := Channel.Selected;

    // channel name and range text
    UseFont(Renderer.FChannelNameFont);
    //Brush.Style := bsClear;
    Font.Color := FColorChannelText;
    txt := Channel.Name + LineEnding + Channel.TextRange(w);
    Renderer.FTextStyleForTextRect.Wordbreak := True; // can go to the next line
    TextRect(r, 0, 0, txt, Renderer.FTextStyleForTextRect);
   end;
  end;
end;

{ TViewFixture }

procedure TViewFixture.InitFrom(aFixture: TDmxFixture; aR: TRect);
var i: integer;
    xx, ww: integer;
  txt: string;
  r: TRect;
begin
  Fixture := aFixture;
  SetLength(Cursors, Fixture.ChannelsCount);

  ww := FIXTURE_SPACE_BEFORE_AFTER_CHANNEL*2+Renderer.ChannelWidth*Fixture.ChannelsCount+
        SPACE_BETWEEN_CHANNEL*(Fixture.ChannelsCount-1);


  FixtureArea := Rect(aR.Left, aR.Top+5, aR.Left+ww, aR.Bottom-5);
  BackgroundArea := FixtureArea;

  txt := '  512  ';//+Fixture^.Adress.ToString+' ';
  DmxAdressArea := Rect(FixtureArea.Left+1,
                       FixtureArea.Top+5,
                       FixtureArea.Left+1+Renderer.FAdressFont.GetTextWidth(txt),
                       FixtureArea.Top+5+Renderer.FAdressFont.GetTextHeight(txt));

  FixtureNameArea := Rect(FixtureArea.Left, FixtureArea.Top+1,
                         FixtureArea.Right, FixtureArea.Top+1+Renderer.FFixtureFont.Height);

  FixtureDescriptionArea := Rect(FixtureNameArea.Left,
                                FixtureNameArea.Bottom,
                                FixtureNameArea.Right,
                                FixtureNameArea.Bottom+Renderer.FFixtureFont.Height);

  r := Rect(FixtureArea.Left+FIXTURE_SPACE_BEFORE_AFTER_CHANNEL,
            FixtureDescriptionArea.Bottom,
            FixtureArea.Left+FIXTURE_SPACE_BEFORE_AFTER_CHANNEL+Renderer.ChannelWidth,
            FixtureArea.Bottom-5);


  for i:=0 to High(Cursors) do begin
    Cursors[i].BackgroundArea := r;

    Cursors[i].Channel := Fixture.Channels[i];

    Cursors[i].ChannelArea := r;
    Cursors[i].Renderer := Renderer;
    xx := r.Left+(r.Width-ImageDmxEffects[0].Width) div 2;
    Cursors[i].EffectArea := Rect(xx, r.Top, xx+ImageDmxEffects[0].Width, r.Top+ImageDmxEffects[0].Height);
    Cursors[i].ValueArea := Rect(r.Left, Cursors[i].EffectArea.Bottom, r.Right, Cursors[i].EffectArea.Bottom+Renderer.FEffectValueFont.Height);
    Cursors[i].ChannelNameArea := Rect(r.Left,
                                      r.Bottom-Renderer.FChannelNameFont.Height*7,
                                      r.Right,
                                      r.Bottom);
    xx := r.Left+(r.Width-ImageCursorSize.cx) div 2; //cursor path is H centered on channel area
    Cursors[i].CursorPathArea := Rect(xx, Cursors[i].ValueArea.Bottom,
                                xx+ImageCursorSize.cx,
                                Cursors[i].ChannelNameArea.Top-ImageCursorSize.cy-1);

    // adjust cursor image background
   if Renderer.FCursorBackGround.Height <> Cursors[i].CursorPathArea.Height then
      Renderer.AdjustCursorBackGroundImage( Cursors[i].CursorPathArea.Height );

    Cursors[i].CursorArea.Left := Cursors[i].CursorPathArea.Left;
    Cursors[i].CursorArea.Right := Cursors[i].CursorPathArea.Right;
    Cursors[i].UpdateCursorArea;
    Cursors[i].CursorAreaLastDrawn := Cursors[i].CursorArea;

    r.Left := r.Left+Renderer.ChannelWidth+SPACE_BETWEEN_CHANNEL;
    r.Right := r.Right+Renderer.ChannelWidth+SPACE_BETWEEN_CHANNEL;
  end;
end;

function TViewFixture.ChannelsCount: integer;
begin
  Result := Length(Cursors);
end;

procedure TViewFixture.UpdateIsVisibleOnView;
var r: TRect;
begin
  r := Renderer.AdjustRect(FixtureArea);
  Fixture.IsVisibleOnViewCursor := not((r.Left > Renderer.PB.ClientWidth) or
                                       (r.Right < 0));
end;

function TViewFixture.MouseOverFixture(X, Y: integer): boolean;
begin
  Result := FixtureArea.Contains(Point(X,Y));
end;

function TViewFixture.MouseOverFixtureDescription(X, Y: integer): boolean;
begin
  Result := FixtureDescriptionArea.Contains(Point(X,Y));
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
    if Cursors[i].ChannelNameArea.Contains(Point(X,Y)) then
    begin
       Result := @Cursors[i];
       exit;
    end;
end;

procedure TViewFixture.DrawBackgroundOn(aCanvas: TCanvas);
var r: TRect;
  i: integer;
  procedure UseFont(aFont: TFont);
  begin
    aCanvas.Font.Name := aFont.Name;
    aCanvas.Font.Height := aFont.Height;
    aCanvas.Font.Style := aFont.Style;
  end;

begin
  with aCanvas do
  begin
    // fixture background
    r := Renderer.AdjustRect(FixtureArea);
    Pen.Style := psClear;
    Brush.Color := FColorFixtureBackground;
    RoundRect(r, 20, 20);

    // source for RGB copy
    if IsSourceForRGBCopy then
    begin
      Brush.Style := bsClear;
      Pen.Color := RGBToColor(255,80,255);
      Pen.Width := 2;
      Pen.Style := psSolid;
      Rectangle(r);
      Pen.Width := 1;
    end;

    //FillRect(r, FColorFixtureBackground);
    // dmx adress
    //r:=Renderer.AdjustRectF(DmxAdressArea);
    //RoundRect(r, 20, 20);// BGRA(100,255,255,200), FColorBackgroundAdressDMX{BGRA(100,255,255,90)},[]);
    //Renderer.FAdressFont.TextRect(r, Fixture.Adress.ToString, taCenter, tlCenter, FColorDMXAdress);

    Brush.Style := bsClear;
    Renderer.FTextStyleForTextRect.Wordbreak := False; // don't go to the next line

    // fixture name
    r := Renderer.AdjustRect(FixtureNameArea);
    UseFont( Renderer.FFixtureFont );
    Font.Color := FColorFixtureName;
    TextRect(r, 0, 0, Fixture.Name, Renderer.FTextStyleForTextRect);

    // fixture description
    r := Renderer.AdjustRect(FixtureDescriptionArea);
    if Renderer.ModePrepaDMX then begin
      Brush.Style := bsSolid;
      Brush.Color := u_utils.PercentColor(FColorFixtureBackground, -0.3);
      Pen.Style := psClear;
      Rectangle(r.Left, r.Top, r.Right, r.Bottom);
      Brush.Style := bsClear;
    end;
    Font.Color := FColorDescription;
    TextRect(r, 0, 0, Fixture.Description, Renderer.FTextStyleForTextRect);

    Renderer.FTextStyleForTextRect.Wordbreak := True; // can go to the next line

    for i:=0 to High(Cursors) do
     if Cursors[i].Channel.IsVisibleOnViewCursor then
     begin
      // Cursor path
      r := Renderer.AdjustRect(Cursors[i].CursorPathArea);
      Renderer.FCursorBackGround.Draw(aCanvas, r.Left, r.Top-(ImageCursorSize.cy shr 1));

      // Channel rect
      r := Renderer.AdjustRect(Cursors[i].ChannelNameArea);
      Pen.Style := psClear;
      Brush.Style := bsSolid;
      Brush.Color := FColorChannelNameBackGround;
      RoundRect(r, 8, 8);

      // Channel Source for Copy
      r := Renderer.AdjustRect(Cursors[i].ChannelArea);
      if Cursors[i].IsSourceForCopy then
      begin
       Pen.Color := RGBToColor(255,80,255);
       Pen.Style := psSolid;
       Brush.Style := bsClear;
       Rectangle(r.Left-1, r.Top-1, r.Right+1, r.Bottom+1);
      end;
     end;

  end;
end;

procedure TViewFixture.DrawDynamicPartOn(aCanvas: TCanvas; aDrawAll: boolean);
var r: TRect;
  i: integer;
  procedure UseFont(aFont: TFont);
  begin
    aCanvas.Font.Name := aFont.Name;
    aCanvas.Font.Height := aFont.Height;
    aCanvas.Font.Style := aFont.Style;
  end;

begin
  with aCanvas do
  begin
    // source for RGB copy
    if IsSourceForRGBCopy then
    begin
      r := Renderer.AdjustRect(FixtureArea);
      Brush.Style := bsClear;
      Pen.Color := RGBToColor(255,80,255);
      Pen.Width := 2;
      Pen.Style := psSolid;
      Rectangle(r);
      Pen.Width := 1;
    end;

    Brush.Style := bsClear;
   // Renderer.FTextStyleForTextRect.Wordbreak := True; // can go to the next line

    for i:=0 to High(Cursors) do
     if Cursors[i].Channel.IsVisibleOnViewCursor then
      Cursors[i].DrawDynamicPartOn(aCanvas, aDrawAll);
  end;
end;

procedure TViewFixture.SetSelected(AValue: boolean);
begin
  Fixture.Selected := AValue;
end;

function TViewFixture.GetSelected: boolean;
begin
  Result := Fixture.Selected;
end;

{ TFrameViewDMXCursors }

procedure TFrameViewDMXCursors.PBMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var pchannelName: PViewCursor;
    pfix: PViewFixture;
begin
  if Length(FView)=0 then exit;
  X := X+View_Begin;
  pfix := FixtureUnderMouse(X, Y);

  if (pfix = NIL) and (Button = mbLeft) then
  begin
    //Redraw;
    exit;
  end;

  if (Button = mbRight) and (FMouseState = msReleased) then
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
      pchannelName^.DrawDynamicPartOn(FImage.Canvas, False);
      RedrawAll;
      exit;
    end;
  end;

  if (Button=mbMiddle) and (FMouseState=msReleased) then
     ProcessLoopScrollView;
end;

procedure TFrameViewDMXCursors.Panel1Resize(Sender: TObject);
  function LeftToolsWidth: integer;
  begin
    Result := BSelectNone.Left+BSelectNone.Width-BZero.Left;
  end;
  function RightToolsWidth: integer;
  begin
    Result := BRGBTools.Left+BRGBTools.Width-BGroup.Left;
  end;

begin
  // rearrange the tools on 1 or 2 lines
  if LeftToolsWidth+RightToolsWidth+ScaleDesignToForm(15*3) >= Panel1.Width then
  begin  // 2 lines
    Panel1.Height := ScaleDesignToForm(7+20+4+20+8);

    BZero.Top := ScaleDesignToForm(7);
    BZero.Left := (Panel1.ClientWidth-LeftToolsWidth) div 2;

    BGroup.Top := Panel1.ClientHeight-ScaleDesignToForm(20+7);
    BGroup.Left := (Panel1.ClientWidth-RightToolsWidth) div 2;
  end;

  if LeftToolsWidth+RightToolsWidth+ScaleDesignToForm(15*3) < Panel1.Width then
  begin // 1 line
   Panel1.Height := ScaleDesignToForm(7+20+8);

   BZero.Top := ScaleDesignToForm(7);
   BZero.Left := ScaleDesignToForm(15);

   BGroup.Top := ScaleDesignToForm(7);
   BGroup.Left := Panel1.ClientWidth-RightToolsWidth-ScaleDesignToForm(15);
  end;
end;

procedure TFrameViewDMXCursors.PBMouseLeave(Sender: TObject);
begin
  if FOnMouseOverFixture <> NIL then
    FOnMouseOverFixture(Self, NIL);
end;

procedure TFrameViewDMXCursors.PBMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var pfix: PViewFixture;
    pCursor: PViewCursor;
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
      guiPrepaDMX: begin
        flag := (pfix^.MouseOverFixtureDescription(X, Y)) or mouseOverChannelName;
      end;

      guiMainDMX,
      guiEditSequence:
        flag := mouseOverChannelName;
    end;

    pCursor := pfix^.CursorUnderMouse(X, Y);
    if pCursor <> NIL then
      flag := flag or (not pCursor^.Channel.Locked);

    if flag then
      PB.Cursor := crHandPoint
    else
      PB.Cursor := crDefault;

   // if flag then Redraw;
  end;
end;

procedure TFrameViewDMXCursors.PBMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var pfix: PViewFixture;
  cur: PViewCursor;
  txt: string;
begin
  if Length(FView) = 0 then exit;
  X := X+View_Begin;

  if (FMouseState = msReleased) and (Button in [mbRight, mbLeft]) and ModePrepaDMX then
  begin
    pfix := FixtureUnderMouse(X, Y);
    if pfix <> NIL then
    begin
      if pfix^.MouseOverFixtureDescription(X, Y) then
      begin
        // user want to change fixture description
        txt := pfix^.Fixture.Description;
        if UserInputNoSpecialChar(SNewDescription, SOk, SCancel, txt, mtConfirmation, TRUE) = mrOk then
        begin
          pfix^.Fixture.Description := txt;
          RedrawVisibleFixtures;
          Project.SetModified;
        end;
      end
      else
      begin
        cur := pfix^.ChannelNameUnderMouse(X, Y);
        if cur <> NIL then
        begin
          // user want to change channel name
          txt := cur^.Channel.UserDefinedName;
          if UserInputNoSpecialChar(SNewName, SOk, SCancel, txt, mtConfirmation, TRUE) = mrOk then
          begin
            cur^.Channel.UserDefinedName := txt;
            RedrawVisibleFixtures;
            Project.SetModified;
          end;
        end;
      end;
    end;
  end;


  FMouseState := msReleased;
end;

procedure TFrameViewDMXCursors.PBMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  if Length(FView) = 0 then exit;
  if WheelDelta < 0 then
    Zoom := Zoom-2
  else
    Zoom := Zoom+2;
  AdjustGraphicObjectsForZoom;
  UpdateView;
  handled := TRUE;
end;

procedure TFrameViewDMXCursors.PBPaint(Sender: TObject);
var r: TRect;
begin
  FInvalidateAlreadySent := FALSE;

  //PB.Canvas.CopyRect(PB.ClientRect, FImage.Canvas, PB.ClientRect);
  r := PB.Canvas.ClipRect;
  PB.Canvas.CopyRect(r, FImage.Canvas, r);
end;

procedure TFrameViewDMXCursors.PBResize(Sender: TObject);
begin
  FImage.SetSize(PB.ClientWidth, PB.ClientHeight);
  UpdateView;
end;

procedure TFrameViewDMXCursors.MenuItem1Click(Sender: TObject);
var i: integer;
  cur: PViewCursor;
begin
  for i:=0 to High(FWorkingFixture^.Cursors) do
  begin
    cur := @FWorkingFixture^.Cursors[i];
    InternalChannelSelection(cur, cur^.Channel = FWorkingCursor^.Channel);
  end;
  RedrawVisibleFixtures;
  RedrawAll;
end;

procedure TFrameViewDMXCursors.MenuItem2Click(Sender: TObject);
var i: integer;
begin
  for i:=0 to High(FWorkingFixture^.Cursors) do
   InternalChannelSelection(@FWorkingFixture^.Cursors[i], TRUE);
  RedrawVisibleFixtures;
  RedrawAll;
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
  RedrawVisibleFixtures;
  RedrawAll;
end;

procedure TFrameViewDMXCursors.MIAllOnAllClick(Sender: TObject);
var i, j: integer;
begin
  for i:=0 to High(FView) do
    for j:=0 to High(FView[i].Cursors) do
     InternalChannelSelection(@FView[i].Cursors[j], TRUE);
  RedrawVisibleFixtures;
  RedrawAll;
end;

procedure TFrameViewDMXCursors.MISelectOnAllClick(Sender: TObject);
var i, j: integer;
  chantype: TChannelType;
begin
  chantype := FWorkingCursor^.Channel.ChannelType;
  for i:=0 to High(FView) do
    for j:=0 to High(FView[i].Cursors) do
     if (FView[i].Cursors[j].Channel.ChannelType = chantype) then
       InternalChannelSelection(@FView[i].Cursors[j], TRUE);

  RedrawVisibleFixtures;
  RedrawAll;
end;

procedure TFrameViewDMXCursors.MILockChannelClick(Sender: TObject);
var i, j: integer;
begin
  for i:=0 to High(FView) do
    for j:=0 to High(FView[i].Cursors) do
     if FView[i].Cursors[j].Channel.Selected then
       FView[i].Cursors[j].Channel.Locked := TRUE;

  RedrawVisibleFixtures;
  RedrawAll;
  TFrameViewprojector(ParentViewProjector).Redraw;
  Project.SetModified;
end;

procedure TFrameViewDMXCursors.MIUnlockChannelClick(Sender: TObject);
var i, j: integer;
begin
  for i:=0 to High(FView) do
    for j:=0 to High(FView[i].Cursors) do
     if FView[i].Cursors[j].Channel.Selected then
       FView[i].Cursors[j].Channel.Locked := FALSE;

  RedrawVisibleFixtures;
  RedrawAll;
  TFrameViewprojector(ParentViewProjector).Redraw;
  Project.SetModified;
end;

procedure TFrameViewDMXCursors.PopChannelPopup(Sender: TObject);
var A: ArrayOfDmxChannels;
begin
  MIOnlyOnAll.Enabled := Length(FView) > 1;
  MIAllOnAll.Enabled := Length(FView) > 1;

  A := GetTargetChannels;
  MICreateGroup.Enabled := Length(A) > 0;
end;

procedure TFrameViewDMXCursors.ScrollBar1Scroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  // update the view only if the scrollbar is moved with a delta of 10 to
  // minimize a little bit the processor's load.
  if (ScrollPos = 0) or (Abs(ScrollPos-View_Begin) > 10) then
  begin
    View_Begin := ScrollPos;
    UpdateWhoIsVisibleOnView;
    RedrawVisibleFixtures;
    RedrawAll;
  end;
end;

procedure TFrameViewDMXCursors.BZeroClick(Sender: TObject);
var i: integer;
begin
  SeqPLayer.StopPreview;
  for i:=0 to High(FView) do
   FView[i].Fixture.SetAllChannelsToZero;
  RedrawVisibleFixtures;
  RedrawAll;
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
   begin
    FView[i].Cursors[j].Channel.Selected := FALSE;
    FView[i].Cursors[j].DrawDynamicPartOn(FImage.Canvas, False);
   end;

  RedrawAll;
  ClearChannelOnOtherWindows;
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

  aViewCursor^.DrawDynamicPartOn(FImage.Canvas, False);

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
        FView[i].Cursors[j].DrawDynamicPartOn(FImage.Canvas, False);
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
  if TFrameViewprojector(ParentViewProjector).FShowLevel then
    TFrameViewprojector(ParentViewProjector).Redraw;
end;

procedure TFrameViewDMXCursors.ProcessLoopMoveCursor;
var yOrigin, yCurrent, delta: integer;
  p: TPoint;
begin
  FMouseState := msMovingCursor;
  FWorkingCursor^.Channel.HandledByUser := TRUE;
  yOrigin := PB.ScreenToClient(Mouse.CursorPos).y;
  repeat
    yCurrent := EnsureRange(PB.ScreenToClient(Mouse.CursorPos).y,
                            FWorkingCursor^.CursorPathArea.Top,
                            FWorkingCursor^.CursorPathArea.Bottom+FWorkingCursor^.CursorArea.Height-1);

    delta := yCurrent-yOrigin;
    if delta <> 0 then
    begin
      InternalMoveCursor(FWorkingCursor, delta);
      UpdateLevelOnViewProjector;
      yOrigin := yCurrent;
      RedrawAll;
    end
    else Sleep(1);
    Application.ProcessMessages;
  until FMouseState = msReleased;
  FWorkingCursor^.Channel.HandledByUser := FALSE;
end;

procedure TFrameViewDMXCursors.ProcessLoopScrollView;
var xOrigin, delta: integer;
  accu: single;
begin
  FMouseState := msScrollingView;
  xOrigin := PB.ScreenToClient(Mouse.CursorPos).x;
  accu := View_Begin;
  PB.Cursor := crSizeWE;
  repeat
    delta := PB.ScreenToClient(Mouse.CursorPos).x-xOrigin;
    if delta <> 0 then
    begin
      accu := accu+delta*0.1;
      if accu < 0 then
        accu := 0;
      View_Begin := Trunc(accu);
      UpdateWhoIsVisibleOnView;
      RedrawVisibleFixtures;
      RedrawAll;
    end;
    Application.ProcessMessages;
  until FMouseState = msReleased;
  PB.Cursor := crDefault;
end;

function TFrameViewDMXCursors.GetView_End: integer;
begin
  Result := View_Begin+PB.ClientWidth-1;
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
     FormDMXChannelsTools.GUIMode := GUIMode;
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
      FormDMXRGBTools.GUIMode := GUIMode;
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
      FormDMXGroup.FTargetViewProjector := TFrameViewProjector(ParentViewProjector);
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

  UpdateWhoIsVisibleOnView;
  UpdateScrollBar;
end;

procedure TFrameViewDMXCursors.SetZoom(AValue: integer);
begin
  AValue := EnsureRange(AValue, MIN_ZOOM_VALUE, MAX_ZOOM_VALUE);
  if FZoom = AValue then
    Exit;

  FZoom := AValue;
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
  if FTotalViewArea.Right >= 0 then
    ScrollBar1.Max := FTotalViewArea.Right
  else
    ScrollBar1.Max := 100;

  if View_Begin >= 0 then
    ScrollBar1.Position := View_Begin
  else
    ScrollBar1.Position := 0;
end;

function TFrameViewDMXCursors.AdjustRect(aR: TRect): TRect;
begin
  Result.Left := aR.Left-View_Begin;
  Result.Right := aR.Right-View_Begin;
  Result.Top := aR.Top;
  Result.Bottom := aR.Bottom;
end;

function TFrameViewDMXCursors.AdjustX(aX: integer): integer;
begin
  Result := aX - View_Begin;
end;

procedure TFrameViewDMXCursors.ComputeAreaFromFixturesToDraw;
var i: integer;
  r: TRect;
begin
  SetLength(FView, Length(FFixturesToDraw));

  FTotalViewArea := Rect(0, 0, 1, PB.ClientHeight);

  r.Left := SPACE_BETWEEN_FIXTURE;
  r.Top := FTotalViewArea.Top;
  r.Bottom := FTotalViewArea.Height;

  for i:=0 to High(FFixturesToDraw) do
  begin
    FView[i].Renderer := Self;
    FView[i].InitFrom(FFixturesToDraw[i], r);

    r.Left := r.Left+FView[i].FixtureArea.Width+SPACE_BETWEEN_FIXTURE;
    r.Right := r.Right+FView[i].FixtureArea.Width+SPACE_BETWEEN_FIXTURE;
    FTotalViewArea.Right := r.Left;

if FTotalViewArea.Right < FTotalViewArea.Left then
  raise exception.create('valeur négative pour FTotalViewArea.Right');
  end;

  UpdateWhoIsVisibleOnView;
end;

procedure TFrameViewDMXCursors.UpdateWhoIsVisibleOnView;
var i, j: integer;
begin
  FViewFirstIndex := -1;
  FViewLastIndex := -1;

  for i:=0 to High(FView) do
  begin
    FView[i].UpdateIsVisibleOnView;
    for j:=0 to High(FView[i].Cursors) do
      FView[i].Cursors[j].UpdateIsVisibleOnView;

    if FView[i].Fixture.IsVisibleOnViewCursor and (FViewFirstIndex = -1) then
      FViewFirstIndex := i;
 end;

  if FViewFirstIndex = -1 then
    exit;
  for i:=High(FView) downto 0 do
    if FView[i].Fixture.IsVisibleOnViewCursor then
    begin
      FViewLastIndex := i;
      exit;
    end;
end;

procedure TFrameViewDMXCursors.UpdateView;
var dx: integer;
begin
  ComputeAreaFromFixturesToDraw;
  // if possible center the view on fixture
  dx := FTotalViewArea.Width-PB.ClientWidth;
  if dx = 0 then
    View_Begin := 0
  else if dx < 0 then
    View_Begin := dx div 2
  else
      //here the view can't fit the whole fixture-> we ensure the view show some of them
    View_Begin := EnsureRange(View_Begin, FTotalViewArea.Left, FTotalViewArea.Right-PB.ClientWidth);

  RedrawVisibleFixtures;
  RedrawAll;
end;

procedure TFrameViewDMXCursors.UpdateStringAfterLanguageChange;
begin
  BRGBTools.Caption := SRGB;
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

procedure TFrameViewDMXCursors.ClearChannelOnOtherWindows;
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

  FormDMXChannelsTools.GUIMode := AValue;
  FormDMXRGBTools.GUIMode := AValue;
  FormDMXGroup.GUIMode := AValue;

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

  UpdateStringAfterLanguageChange;
end;

procedure TFrameViewDMXCursors.DrawBackgroundOnFImage;
begin
  with FImage.Canvas do begin
    Pen.Style := psClear;
    Brush.Color := FColorBackground;
    Rectangle(0, 0, FImage.Width, FImage.Height);
  end;
end;

procedure TFrameViewDMXCursors.SetSourceFixtureForRGBCopy(AValue: TDMXFixture);
var i: integer;
begin
  for i:=0 to High(FView) do
   FView[i].IsSourceForRGBCopy := FView[i].Fixture=AValue;
  RedrawVisibleFixtures;
  RedrawAll;
end;

procedure TFrameViewDMXCursors.SetSourceChannelForCopy(AValue: TDMXChannel);
var i, j: integer;
begin
  if AValue = FCurrentSourceChannelForCopy then exit;
  FCurrentSourceChannelForCopy := AValue;

  for i:=0 to High(FView) do
    for j:=0 to High(FView[i].Cursors) do
      FView[i].Cursors[j].IsSourceForCopy := FView[i].Cursors[j].Channel = AValue;
  RedrawVisibleCursors;
end;

procedure TFrameViewDMXCursors.UpdateEditMode;
begin
  FormDMXChannelsTools.UpdateEditMode;
  FormDMXRGBTools.UpdateEditMode;
  FormDMXGroup.UpdateEditMode
end;

procedure TFrameViewDMXCursors.CreateGraphicObjects;
begin
  // locked cursor
  try
    ImageLock := SVGFileToBGRABitmap(GetAppFixtureImagesFolder+'Lock.svg', ScaleDesignToForm(25), -1);
  except
    ImageLock := TBGRABitmap.Create(ScaleDesignToForm(25),ScaleDesignToForm(35), BGRAWhite);
  end;

  FFixtureFont := TFont.Create;
  FFixtureFont.Name := 'Arial';
  FFixtureFont.Style := [];

  FAdressFont := TFont.Create;
  FAdressFont.Name := 'Arial';
  FAdressFont.Style := [fsBold];

  FEffectValueFont := TFont.Create;
  FEffectValueFont.Name :='Arial';
  FEffectValueFont.Style := [];

  FChannelNameFont := TFont.Create;
  FChannelNameFont.Name := 'Arial';
  FChannelNameFont.Style := [];

  FChannelRangeFont := TFont.Create;
  FChannelRangeFont.Name := 'Arial';
  FChannelRangeFont.Style := [];

  FCursorBackGround := TBGRABitmap.Create(1, 1);
end;

procedure TFrameViewDMXCursors.DeleteGraphicObjects;
begin
  ImageLock.Free;
  FCursorBackGround.Free;

  if FAdressFont <> NIL then FAdressFont.Free;
  if FFixtureFont <> NIL then FFixtureFont.Free;
  if FEffectValueFont <> NIL then FEffectValueFont.Free;
  if FChannelNameFont <> NIL then FChannelNameFont.Free;
  if FChannelRangeFont <> NIL then FChannelRangeFont.Free;
end;

procedure TFrameViewDMXCursors.AdjustGraphicObjectsForZoom;
  function ApplyZoomOnFontHeight(aFH: integer): integer;
  begin
    Result := Trunc(aFH+Zoom*0.8);
    Result := EnsureRange(Result, 6, 30);
  end;
begin
  FFixtureFont.Height := ApplyZoomOnFontHeight(10);
  FAdressFont.Height := ApplyZoomOnFontHeight(10);
  FEffectValueFont.Height := ApplyZoomOnFontHeight(8);
  FChannelNameFont.Height := ApplyZoomOnFontHeight(8);
  FChannelRangeFont.Height := ApplyZoomOnFontHeight(8);

  FChannelWidth := Trunc(ImageCursorSize.cx+Zoom*4);//50+trunc((Zoom-MAX_ZOOM_VALUE div 2)*5);
  if FChannelWidth < ImageCursorSize.cx then
    FChannelWidth := ImageCursorSize.cx;
end;

procedure TFrameViewDMXCursors.AdjustCursorBackGroundImage(aCursorPathHeight: integer);
var i, x, y: integer;
  yy, deltay: single;
  c: TBGRAPixel;
  gradCount: integer;
const GRADUATION_COUNT = 10;
begin
  if aCursorPathHeight < 0 then aCursorPathHeight := 0;
  // resize
  FCursorBackGround.SetSize(ImageCursorSize.cx, aCursorPathHeight + ImageCursorSize.cy);
  // background
  FCursorBackGround.Fill(FColorFixtureBackground);

  // Cursor path
  x := FCursorBackGround.Width shr 1;
  y := ImageCursorSize.cy shr 1;

  c := BGRA(0,0,0,150);
  FCursorBackGround.DrawVertLine(x, y, y+aCursorPathHeight, c);     // vertical axis
  FCursorBackGround.DrawVertLine(x+1, y, y+aCursorPathHeight, c);
  // number of cursor graduations
  gradCount := GRADUATION_COUNT;
  repeat
   deltay := aCursorPathHeight / gradCount; // we want 10 big graduations
   if deltay < ScaleDesignToForm(15) then dec(gradCount);
  until (deltay >= ScaleDesignToForm(15)) or (gradCount = 0);
  if gradCount > 0 then begin
    c := BGRA(0,0,0,80);
    yy := y;
    for i:=0 to gradCount-1 do begin
      y := Round(yy);
      FCursorBackGround.DrawLine(0, y, ImageCursorSize.cx, y, c, true);

      y := Round(yy + deltay * 0.5);
      FCursorBackGround.DrawLine(ImageCursorSize.cx div 4, y, ImageCursorSize.cx *3div 4, y, c, True);
      yy := yy + deltay;
    end;
    y := Round(yy);
    FCursorBackGround.DrawLine(0, y, ImageCursorSize.cx, y, c, true);
  end;
end;

constructor TFrameViewDMXCursors.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FCursorPathHeight := 255;
  FMouseState := msReleased;
  FGUIMode := guiMainDMX;
  FChannelWidth := 50;
  FZoom := 5;
  FImage := TBitmap.Create;

  CreateGraphicObjects;
  AdjustGraphicObjectsForZoom;

  FToogleSpeedButtonManager := TToggleSpeedButtonManager.Create;
  FToogleSpeedButtonManager.ToggleType := tsbNoneOrOne;
  FToogleSpeedButtonManager.SetActivatedColors($0003C4FC, $00272727);
  FToogleSpeedButtonManager.SetDeactivatedColors($00004B62, $00EAEAEA);
  FToogleSpeedButtonManager.Add(BOnlySameType, FALSE);
  FToogleSpeedButtonManager.Add(BAll, FALSE);
  FToogleSpeedButtonManager.Add(BOnlySelected, FALSE);

  FColorBackground := RGBToColor(21,21,21); //RGBToColor(51,51,51);
  FColorFixtureBackground := RGBToColor(57,55,53); // RGBToColor(97,90,78);
  FColorChannelNameBackGround := u_utils.PercentColor(FColorFixtureBackground, -0.4);
  FColorBackgroundAdressDMX := u_utils.PercentColor(FColorFixtureBackground, -0.5);
  FColorDMXAdress := RGBToColor(255,255,200);
  FColorFixtureName := $00C0C0C0;
  FColorDescription := $00EAEAEA;
  FColorEffect := RGBToColor(255,128,64);
  FColorValue := clWhite;
  FColorChannelName := $006FBDE3;//  BGRA(80,50,25);
  FColorChannelRange := $002D9FD7;//  BGRA(70,40,20);
  FColorChannelText := $00EAEAEA; // FColorFixtureName;

  FTextStyleForTextRect.SingleLine := FALSE;
  FTextStyleForTextRect.Alignment := taCenter;
  FTextStyleForTextRect.Layout := Graphics.tlCenter;
  FTextStyleForTextRect.Wordbreak := TRUE;
  FTextStyleForTextRect.Opaque := FALSE;
  FTextStyleForTextRect.Clipping := TRUE;
  FTextStyleForTextRect.SystemFont := FALSE;
end;

destructor TFrameViewDMXCursors.Destroy;
begin
  DeleteGraphicObjects;
  FToogleSpeedButtonManager.Free;
  FreeAndNil(FImage);
  inherited Destroy;
end;

procedure TFrameViewDMXCursors.EraseBackground(DC: HDC);
begin
//do nothing here
end;

procedure TFrameViewDMXCursors.RedrawAll;
begin
  if not FInvalidateAlreadySent then
  begin
    FInvalidateAlreadySent := TRUE;
    PB.Invalidate;
  end;
end;

procedure TFrameViewDMXCursors.RedrawVisibleFixtures;
var i: Integer;
begin
  DrawBackgroundOnFImage;
  if (Length(FView) > 0) and (FViewFirstIndex <> -1) then
  begin
   for i:=FViewFirstIndex to FViewLastIndex do begin
      FView[i].DrawBackgroundOn( FImage.Canvas );
      FView[i].DrawDynamicPartOn(FImage.Canvas, True);
   end;
  end;
  RedrawAll;
end;

procedure TFrameViewDMXCursors.RedrawVisibleCursors;
var i: Integer;
begin
  if (Length(FView) > 0) and (FViewFirstIndex <> -1) then
   for i:=FViewFirstIndex to FViewLastIndex do
     FView[i].DrawDynamicPartOn(FImage.Canvas, True);
  RedrawAll;
end;

procedure TFrameViewDMXCursors.Clear;
var i: integer;
begin
  SourceChannel_None;
  SourceFixture_None;
  if Length(FFixturesToDraw)=0 then
    exit;

  // all fixture are no longer visible on cursor view
  for i:=0 to High(FFixturesToDraw) do
  begin
   FFixturesToDraw[i].ClearIsVisibleOnViewCursorOnAllChannels;
   FFixturesToDraw[i].UnselectAllChannels;
  end;

  SetLength(FFixturesToDraw, 0);
  UpdateView;
  ClearChannelOnOtherWindows;
end;

procedure TFrameViewDMXCursors.Add(aFixture: TDmxFixture; aUpdateView: boolean);
var i: integer;
begin
  // we don't add the fixture if it's already in the list
  for i:=0 to High(FFixturesToDraw) do
    if FFixturesToDraw[i] = aFixture then
    begin
      if aUpdateView then UpdateView;
      exit;
    end;

  i := Length(FFixturesToDraw);
  SetLength(FFixturesToDraw, i+1);
  FFixturesToDraw[i] := aFixture;

  // if the fixture have only one channel, we select it
  if not aFixture.Locked and (aFixture.ChannelsCount = 1) then aFixture.Channels[0].Selected := True;

  if aUpdateView then
    UpdateView;

{  for i:=0 to aFixture.ChannelsCount-1 do
    AddChannelToOtherWindows(aFixture.Channels[i]); }
end;

procedure TFrameViewDMXCursors.Remove(aFixture: TDmxFixture);
var i: integer;
begin
  aFixture.ClearIsVisibleOnViewCursorOnAllChannels;

  for i:=0 to aFixture.ChannelsCount-1 do
    if aFixture.Channels[i].Selected then
      RemoveChannelToOtherWindows(aFixture.Channels[i]);

  aFixture.UnselectAllChannels;

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
  if FChannelUnderMouse = aChan then exit;

  FChannelUnderMouse := aChan;
  RedrawVisibleFixtures;
  RedrawAll;
end;

procedure TFrameViewDMXCursors.RedrawFixture(aFix: TDMXFixture);
var i: Integer;
begin
  if (Length(FView) = 0) or
     (FViewFirstIndex = -1) then
    exit;

  for i:=FViewFirstIndex to FViewLastIndex do
    if FView[i].Fixture = aFix then
    begin
      FView[i].DrawBackgroundOn(FImage.Canvas);
      FView[i].DrawDynamicPartOn(FImage.Canvas, False);
      exit;
    end;
end;

procedure TFrameViewDMXCursors.RedrawRGBChannelsOnFixture(aFix: TDMXFixture);
var i, j: Integer;
begin
  if (Length(FView) = 0) or
     (FViewFirstIndex = -1) then
    exit;

  for i:=FViewFirstIndex to FViewLastIndex do
    if (FView[i].Fixture = aFix) then
    begin
      if not FView[i].Fixture.HasRGBChannel then exit;
      j := aFix.RedChannelIndex;
      FView[i].Cursors[j].DrawDynamicPartOn(FImage.Canvas, False);
      j := aFix.GreenChannelIndex;
      FView[i].Cursors[j].DrawDynamicPartOn(FImage.Canvas, False);
      j := aFix.BlueChannelIndex;
      FView[i].Cursors[j].DrawDynamicPartOn(FImage.Canvas, False);
      exit;
    end;
end;

procedure TFrameViewDMXCursors.RedrawCursor(aChan: TDMXChannel; aDrawAll: boolean);
var i, j: Integer;
begin
  if (Length(FView) = 0) or
     (FViewFirstIndex = -1) then
    exit;

  for i:=FViewFirstIndex to FViewLastIndex do
    if FView[i].Fixture = aChan.Fixture then
      for j:=0 to High(FView[i].Cursors) do
        if FView[i].Cursors[j].Channel = aChan then
        begin
          FView[i].Cursors[j].DrawDynamicPartOn(FImage.Canvas, aDrawAll);
          exit;
        end;
end;

end.

