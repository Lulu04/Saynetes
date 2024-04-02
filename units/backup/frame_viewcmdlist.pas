unit frame_viewcmdlist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Types, Graphics, LCLType,
  LCLTranslator, Menus, ExtCtrls, u_common, frame_sequencer;



type

  { TFrameViewCmdList }

  TFrameViewCmdList = class(TFrame)
    LB: TListBox;
    MIColorAsRectangleColor: TMenuItem;
    MIColorAsText: TMenuItem;
    MIColorRepresentation: TMenuItem;
    MIDeleteSelection: TMenuItem;
    MIEdit: TMenuItem;
    MIViewChannelName: TMenuItem;
    MIViewDescription: TMenuItem;
    MIViewFixtureName: TMenuItem;
    MIViewAdress: TMenuItem;
    MIView: TMenuItem;
    PopupMenu1: TPopupMenu;
    procedure LBDrawItem({%H-}Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure LBKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure LBMouseLeave(Sender: TObject);
    procedure LBMouseMove(Sender: TObject; {%H-}Shift: TShiftState; {%H-}X, Y: Integer);
    procedure LBMouseUp(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure MIColorAsTextClick(Sender: TObject);
    procedure MIDeleteSelectionClick(Sender: TObject);
    procedure MIViewUniverseClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
  private
    coul_action_audio,
    coul_action_dmx,
    coul_action_divers,
    coul_action_photo,
    coul_action_video,
    coul_action_animation: TColor;
    FReadOnly: boolean;
    FLoading: boolean;
    function GetCmdList: TCmdList;
    procedure SetCmdList(AValue: TCmdList);
  private
    FClipBoard: TStringList;
    procedure ClipBoard_CutSelection;
    procedure ClipBoard_CopySelection;
    procedure ClipBoard_PasteBefore(aIndex: integer);
    procedure ClipBoard_PasteAfter(aIndex: integer);
    procedure ClipBoard_Clear;
    function Clipboard_HasData: boolean;
  private
    FItemIndexUnderMouse: integer;
    FImage: TImage;
    function GetItemHeight: integer;
    procedure SetItemHeight(AValue: integer);
  private
    FWorkingStep: TSequenceStep;
    FParentSeq: TFrameSequencer;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground({%H-}DC: HDC); override;

    procedure UpdateMenuEntryFromProjectOptions;

    procedure Clear;
    procedure SetWorkingStep(aStep: TSequenceStep; aParentSeq: TFrameSequencer);

    property CmdList: TCmdList read GetCmdList write SetCmdList;

    property ReadOnly: boolean read FReadOnly write FReadOnly;
    property ItemHeight: integer read GetItemHeight write SetItemHeight;
  end;

implementation
uses u_utils, VelocityCurve, ALSound,
  u_audio_manager, u_list_dmxuniverse, u_resource_string,
  u_list_top, u_project_manager, u_helper, u_dmx_util,
  frame_bglvirtualscreen_sequencer, u_program_options;

{$R *.lfm}

{ TFrameViewCmdList }

procedure TFrameViewCmdList.LBDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var txt, txt2: string;
 cmd, x, decalage: integer;
 c: TColor;
 A: TParamArray;
   // gives the formatted value of percentage in dmx command
   function DMXPercent(v: string): string;
   begin
    Result := FormatFloat('0.0', StringToSingle(v)*100)+'%';
   end;
   function DurationValue(v: string): string;
   begin
    Result := FormatFloat('0.00', StringToSingle(v))+' '+SSec;
   end;
   function FormatFloat1Decimal(v: string): string;
   begin
    Result := FormatFloat('0.0', StringToSingle(v));
   end;
   function FormatFloat2Decimal(v: string): string;
   begin
    Result := FormatFloat('0.00', StringToSingle(v));
   end;

   function FormatVolume( v: string ): string;
   begin
    Result := SVolume+' '+VolumeToStringPercent(StringToSingle(v));
   end;

   function FormatPan( p: string ): string;
   begin
    Result := SPan+' '+PanToStringPercent(StringToSingle(p));
   end;

   function FormatAudioFile( strID: string ): string;
   var o: TALSSound;
   begin
    o := SoundManager.GetSoundByID( strID.ToInteger );
    if o = NIL then
      Result := SAudioFileNotFound
    else
      Result := ExtractFileName( o.Filename );
   end;

   function FormatFixtureName(StrIDUni, StrIDFix: string): string;
   var uni: TDMXUniverse;
    fix: TDMXFixture;
    f: boolean;
   begin
     if not UniverseManager.RetrieveFixture(StrIDUni.ToInteger, StrIDFix.ToInteger, uni, fix) then
     begin
       Result := SUniverse+'['+StrIDUni+'] '+SFixture+'['+StrIDFix+'] '+SNotFound;
       exit;
     end;
     Result := '';
     f := FALSE;
     if Project.Options.CmdListViewDMXAdress then
     begin
       Result := uni.ShortName+':'+fix.Adress.ToString;  // universe short name + dmx adress
       f := TRUE;
     end;
     if Project.Options.CmdListViewDMXFixName then
     begin
       if f then Result := Result+' - ';
       Result := Result+fix._Name; // fixture name
       f := TRUE;
     end;
     if Project.Options.CmdListViewDMXFixDescription then
     begin
       if f then Result := Result+' - ';
       Result := Result+fix.Description;  // fixture description
       f := TRUE;
     end;
   end;

   function FormatDmxTrack(StrIDUni, StrIDFix, StrAdr: string): string;
   var uni: TDMXUniverse;
    fix: TDMXFixture;
    chan: TDMXChannel;
    f: boolean;
   begin
    if not UniverseManager.RetrieveChannel(StrIDUni.ToInteger, StrIDFix.ToInteger, StrAdr.ToInteger, uni, fix, chan) then
    begin
      Result := SUniverse+'['+StrIDUni+'] '+SFixture+'['+StrIDFix+'] '+
                SAdress+'['+StrAdr+'] '+SNotFound;
      exit;
    end;
    f := FALSE;
    Result := '';
    if Project.Options.CmdListViewDMXAdress then
    begin
      Result := uni.ShortName+':'+StrAdr;  // universe short name + dmx adress
      f := TRUE;
    end;
    if Project.Options.CmdListViewDMXFixName then
    begin
      if f then
        Result := Result+' - ';
      Result := Result+fix._Name; // fixture name
      f := TRUE;
    end;
    if Project.Options.CmdListViewDMXFixDescription then
    begin
      if f then
        Result := Result+' - ';
      Result := Result+fix.Description;  // fixture description
      f := TRUE;
    end;
    if Project.Options.CmdListViewDMXChannelName then
    begin
      if f then
        Result := Result+' - ';
      Result := Result+chan._Name; // nom du canal
    end;
   end;

   function RenderTitle(const aTitle, aParamTitle: string; aColor: TColor): integer;
   begin
aColor:=RGBToColor(192,182,172);
     with LB.Canvas do
     begin
      Brush.Style := bsClear;
      Font.Color := aColor;
      Font.Style := [fsBold];
      Result := arect.Left+ScaleDesignToForm(8);
      TextOut(Result, arect.Top, aTitle);
      if Length(aParamTitle) = 0 then
      begin
        Result := Result+Font.GetTextWidth(aTitle);
        exit;
      end;
      Result := Result+Font.GetTextWidth(aTitle+'   ');
      Font.Style := [];
      TextOut(Result, arect.Top, aParamTitle);
      Result := Result+Font.GetTextWidth(aParamTitle);
     end;
   end;

   function RenderCmdText(const aTxt: string; aX: integer; aColor: TColor): integer;
   begin
aColor:=RGBToColor(192,182,172);
    with LB.Canvas do
    begin
     Font.Color := aColor;
     Font.Style := [];
     Brush.Style := bsClear;
     TextOut(aX, arect.Top, aTxt);
     Result := aX+Font.GetTextWidth(aTxt);
    end;
   end;

   function RenderColor(ax: integer; strCouleur: string): integer;
   var cp, cb, c: TColor;
   begin
    c := strtoint(strCouleur);
    if Project.Options.CmdListViewColorAsRectangleColor then
      with LB.Canvas do begin
       cb := Brush.Color;
       cp := Pen.Color;
       Brush.Color := c;
       Pen.Color := PercentColorRelative(c, 0.5);  //clGray;
       Brush.Style := bsSolid;
       Rectangle(ax, arect.Top+1, ax+ScaleDesignToForm(20), arect.Bottom-1);
       Pen.Color := cp;
       Brush.Color := cb;
       Result := ax+ScaleDesignToForm(30);
     end else begin
       Result := RenderCmdText(Format('(%d,%d,%d) ', [Red(c), Green(c), Blue(c)]), ax, RGBToColor(192,182,172));
    end;
   end;

   procedure RenderCurve(ax, ay: integer; strIDCourbure: string);
   var id: word;
   begin
    id := strtoint(strIDCourbure);
    if VelocityCurveList.ValidCurveID(id) then
    begin
      VelocityCurveList.GetCurveByID(id).DrawOn(FImage);
      LB.Canvas.Draw(ax, ay, FImage.Picture.Bitmap);
    end
    else
    begin
      LB.Canvas.Font.Color := clWhite;
      LB.Canvas.TextOut(ax, ay, ' ??'+SCurve+'?? ');
    end;
   end;

   procedure RenderBackground;//(aColor: TColor);
   begin
    if State >= [odSelected] then
      LB.Canvas.Brush.Color := clHighLight//PercentColor(LB.Color, 1.2) // ligne sélectionnée
    else
      LB.Canvas.Brush.Color := LB.Color;//  PercentColor(aColor, -0.7);
    if Index=FItemIndexUnderMouse then
    begin
      // render dot rectangle if mouse is over item
      LB.Canvas.Pen.Style := psDot;
      LB.Canvas.Pen.Color := RGBToColor(120,120,80);
    end
    else LB.Canvas.Pen.Style := psClear;
    LB.Canvas.Rectangle(ARect.Left-1, ARect.Top, ARect.Right+1, ARect.Bottom);
   end;

begin
  decalage := ScaleDesignToForm(20);
  x := arect.Left+decalage;
  A := LB.Items[Index].SplitToParamArray;
  with LB.Canvas do
  begin

   if State >= [odSelected] then
     Brush.Color := PercentColor(LB.Color, 1.5) // ligne sélectionnée
   else
     Brush.Color := LB.Color;
   FillRect(Arect);

   cmd := A.ParamArray0ToCmd;
   case cmd of
    CMD_WAIT: begin  // AWAIT duration
      RenderBackground;
      txt := A[1];
      if StringToSingle(A[1]) > 1 then
        txt := txt + ' '+SSeconds
      else
        txt := txt + ' '+SSecond;
      RenderTitle( SWait, txt, coul_action_divers);
    end;

    CMD_LOOP: begin
      RenderBackground;
      RenderTitle(SBackToBegin, '', coul_action_divers);
    end;

    CMD_STARTSEQUENCE: begin // CMD_STARTSEQUENCE IDSeq
      RenderBackground;
      RenderTitle(SStartSequence, Sequences.GetNameByID(A[1].ToInteger), coul_action_divers);
    end;

    CMD_STOPSEQUENCE: begin  // CMD_STOPSEQUENCE IDSeq
      RenderBackground;
      RenderTitle(SStopSequence, Sequences.GetNameByID(A[1].ToInteger), coul_action_divers);
    end;

    CMD_SEQUENCESTRETCHTIME: begin // CMD_SEQUENCESTRETCHTIME IDSeq StretchValue Duration CurveID
      RenderBackground;
      txt := Sequences.GetNameByID(A[1].ToInteger)+' '+STo+' '+FormatFloat2Decimal(A[2])+' '+SIn+' '+DurationValue(A[3]);
      RenderTitle(SStretchTime, txt, coul_action_divers);
    end;

    TITLECMD_AUDIO_PLAY, TITLECMD_AUDIO_STOP, TITLECMD_AUDIO_PAUSE,
    TITLECMD_AUDIO_FADEIN, TITLECMD_AUDIO_FADEOUT, TITLECMD_AUDIO_SETVOLUME,
    TITLECMD_AUDIO_SETPAN, TITLECMD_AUDIO_SETPITCH: begin
        case cmd of
         TITLECMD_AUDIO_PLAY : txt := SAudioPlay;
         TITLECMD_AUDIO_STOP    : txt := SAudioStop;
         TITLECMD_AUDIO_PAUSE   : txt := SAudioPause;
         TITLECMD_AUDIO_FADEIN  : txt := SAudioFadeIn;
         TITLECMD_AUDIO_FADEOUT : txt := SAudioFadeOut;
         TITLECMD_AUDIO_SETVOLUME : txt := SAudioSetVolume;
         TITLECMD_AUDIO_SETPAN : txt := SAudioSetPan;
         TITLECMD_AUDIO_SETPITCH : txt := SAudioSetFreq;
        end;//case
        RenderBackground;
        RenderTitle( txt, '', coul_action_audio);
    end;

    CMD_AUDIO_PLAY: begin // CMD_AUDIO_PLAY IDaudio volume panoramique
        RenderBackground;
        txt := FormatAudioFile(A[1])+'  '+
               FormatVolume(A[2])+' '+
               FormatPan(A[3]);
        Font.Height := Font.Height-1;
        RenderCmdText(txt, x, coul_action_audio);
    end;

    CMD_AUDIO_STOP, CMD_AUDIO_PAUSE: begin  // cmd IDaudio
        RenderBackground;
        Font.Height := Font.Height-1;
        RenderCmdText(FormatAudioFile(A[1]), x, coul_action_audio);
    end;

    CMD_AUDIO_FADEIN: begin // CMD_AUDIO_FADEIN IDaudio volume duration IDcurve
        RenderBackground;
        Font.Height := Font.Height-1;
        txt := FormatAudioFile( A[1] ) + '  ' +
               FormatVolume( A[2] );
        if StringToSingle( A[3] ) > 1
          then txt := txt + '   '+SIn+' ' + A[3] + SSeconds
          else txt := txt + '   '+SIn+' ' + A[3] + SSecond;
        x:=RenderCmdText( txt, x, coul_action_audio)+10;
        RenderCurve( x, arect.Top, A[4] );
    end;

    CMD_AUDIO_FADEOUT: begin // CMD_AUDIO_FADEOUT IDaudio duration IDcurve
        RenderBackground;
        Font.Height := Font.Height - 1;
        txt := FormatAudioFile( A[1] );
        if StringToSingle ( A[2] ) > 1
          then txt := txt + '   '+SIn+' ' + A[2] + ' '+SSeconds
          else txt := txt + '   '+SIn+' ' + A[2] + ' '+SSecond;
        x:=RenderCmdText( txt, x, coul_action_audio)+10;
        RenderCurve( x, arect.Top, A[3] );
    end;

    CMD_AUDIO_SETVOLUME: begin // CMD_AUDIO_SETVOLUME IDaudio volume duration IDcurve
        RenderBackground;
        Font.Height := Font.Height - 1 ;
        txt := FormatAudioFile( A[1] );
        txt := txt + '   '+STo+' ' + FormatVolume(A[2]);
        if StringToSingle ( A[3] ) > 1
          then txt := txt + '   '+SIn+' ' + A[3] + ' '+SSeconds
          else txt := txt + '   '+SIn+' ' + A[3] + ' '+SSecond;
        x:=RenderCmdText( txt, x, coul_action_audio)+10;
        RenderCurve( x, arect.Top, A[4] );
    end;

    CMD_AUDIO_SETPAN: begin// CMD_AUDIO_SETPAN IDaudio pan duration IDcurve
      RenderBackground;
      Font.Height := Font.Height-1;
      txt := FormatAudioFile(A[1])+'   '+FormatPan(A[2]);
      if StringToSingle(A[3]) > 1 then
        txt := txt+'   '+SIn+'   '+A[3]+' '+SSeconds
      else
        txt := txt+'   '+SIn+'   '+A[3]+' '+SSecond;
      x:=RenderCmdText( txt, x, coul_action_audio)+10;
      RenderCurve( x, arect.Top, A[4] );
    end;

    CMD_AUDIO_SETPITCH: begin   // CMD_AUDIO_SETPITCH IDaudio pitch duration IDcurve
      RenderBackground;
      Font.Height := Font.Height-1;
      txt := FormatAudioFile(A[1]) + '   '+STo+' ' + A[2] + ' '+SHz;
      if StringToSingle ( A[3] )>1 then
        txt := txt+'   '+SIn+'   '+A[3]+' '+SSeconds
      else
        txt := txt+'   '+SIn+'   '+A[3]+' '+SSecond;
      x:=RenderCmdText( txt, x, coul_action_audio)+10;
      RenderCurve( x, arect.Top, A[4] );
    end;

    TITLECMD_AUDIO_APPLYFX: begin // TITLECMD_AUDIO_APPLYFX IDaudio dry/wet EffectCount
      RenderBackground;
      txt2 := SOn_ + ' ' + FormatAudioFile(A[1]) +' '+ SDryWet+' '+FormatFloat2Decimal(A[2]);
      RenderTitle( SAudioConnectEffect, txt2, coul_action_audio);
    end;

    CMD_AUDIO_FXPRESET,
    CMD_AUDIO_CAPTURE_FXPRESET: begin // CMD  effectType  presetIndex
      RenderBackground;
      txt := AudioFXToString(TALSEffectType(A[1].ToInteger), A[2].ToInteger);
      x := RenderCmdText(txt, x, coul_action_audio);
    end;

    CMD_AUDIO_REMOVEFX: begin // CMD_AUDIO_REMOVEFX  IDaudio
      RenderBackground;
      txt := SAudioDisconnectEffect+' '+SOn_ + ' ' + FormatAudioFile(A[1]);
      x := RenderCmdText(txt, x, coul_action_audio);
    end;

    CMD_AUDIO_CAPTURE_START: begin // CMD_AUDIO_CAPTURE_START
      RenderBackground;
      x := RenderCmdText(SAudioCaptureStart, x, coul_action_audio);
    end;

    CMD_AUDIO_CAPTURE_STOP: begin // CMD_AUDIO_CAPTURE_STOP
       RenderBackground;
       x := RenderCmdText(SAudioCaptureStop, x, coul_action_audio);
     end;

    CMD_AUDIO_CAPTURE_SETVOLUME: begin // CMD_AUDIO_CAPTURE_SETVOLUME volume duration IDcurve
      RenderBackground;
      txt := SAudioCaptureSetVolume+' '+STo+' '+FormatVolume(A[1])+' '+SIn+' '+
             DurationValue(A[2])+'  ';
      x := RenderCmdText(txt, x, coul_action_audio);
      RenderCurve(x, arect.Top, A[3]);
    end;

    CMD_AUDIO_CAPTURE_SETPAN: begin // CMD_AUDIO_CAPTURE_SETPAN  panning duration IDcurve
     RenderBackground;
     txt := SAudioCaptureSetPan+' '+STo+' '+FormatPan(A[1])+' '+SIn+' '+
            DurationValue(A[2])+'  ';
     x := RenderCmdText(txt, x, coul_action_audio);
     RenderCurve(x, arect.Top, A[3]);
    end;

    TITLECMD_AUDIO_CAPTURE_APPLYFX: begin // TITLECMD_AUDIO_APPLYFX  IDaudio  dry/wet  EffectCount
      RenderBackground;
      txt2 := SDryWet + ' ' + FormatFloat2Decimal(A[2]);
      RenderTitle( SAudioCaptureConnectEffect, txt2, coul_action_audio);
    end;

    CMD_AUDIO_CAPTURE_REMOVEFX: begin // CMD_AUDIO_CAPTURE_REMOVEFX
      RenderBackground;
      txt := SAudioCaptureDisconnectEffect;
      x := RenderCmdText(txt, x, coul_action_audio);
    end;

    TITLECMD_DMX_DIMMER: begin  // TITLECMD_DMX_DIMMER  duration IDcurve
        RenderBackground;
        txt:=SIn+' '+DurationValue(A[1])+'  ';
        x:=RenderTitle(SDMXDimmer, txt, coul_action_dmx);
        RenderCurve(x, arect.Top, A[2]);
    end;

    CMD_DMX_DIMMER: begin  // CMD_DMX_DIMMER IDuniverse IDFixture dmxadress percent duration IDcurve
        RenderBackground;
        Font.Height:=Font.Height-1;
        txt:=FormatDmxTrack(A[1], A[2], A[3])+' '+STo+' '+DMXPercent(A[4]);
        x:=RenderCmdText(txt, x, coul_action_dmx);
    end;

    CMD_INTERNALDMXWAVE: begin// CMD_INTERNALDMXWAVE IDuniverse IDFixture dmxadress percent1 duration1 IDcurve1 percent2 duration2 IDcurve2
     RenderBackground;
     Font.Height:=Font.Height-1;
     txt:=SDMXWave+' '+SOn_+' '+FormatDmxTrack(A[1], A[2], A[3])+' '+STo+' '+DMXPercent(A[4])+'<->'+DMXPercent(A[7]);
     x:=RenderCmdText(txt, x, coul_action_dmx);
    end;

    TITLECMD_DMX_FLASH: begin  // TITLECMD_DMX_FLASH LevelMin LevelMax DurationMin DurationMax
     RenderBackground;
     if A[1] <> A[2] then
       txt := SRandomValue+' '+DMXPercent(A[1])+' - '+DMXPercent(A[2])+' '
     else
       txt := STo+' '+DMXPercent(A[1])+' ';
     if A[3] <> A[4] then
       txt := txt+SRandomDuration+' '+DurationValue(A[3])+' - '+DurationValue(A[4])
     else
       txt := txt+SIn+' '+DurationValue(A[3]);
     RenderTitle( SDMXFlash, txt, coul_action_dmx );
    end;
    CMD_DMX_FLASH: begin // CMD_DMX_FLASH IDuniverse IDFixture ChanIndex
                         //               LevelMin LevelMax DurationMin DurationMax
     RenderBackground;
     Font.Height := Font.Height-1;
     txt := FormatDmxTrack(A[1], A[2], A[3]); // +' ';
    { if A[1] <> A[2] then
       txt := txt+SRandomValue+' '+FormatFloat1Decimal(A[1])+'% - '+FormatFloat1Decimal(A[2])+'% '
     else
       txt := txt+STo+' '+A[1]+'% ';
     if A[3] <> A[4] then
       txt := txt+SRandomDuration+' '+DurationValue(A[3])+' - '+DurationValue(A[4])
     else
       txt := txt+SIn+' '+DurationValue(A[3]); }
      RenderCmdText(txt, x, coul_action_dmx);
    end;

    TITLECMD_DMX_STOPEFFECT: RenderTitle(SDMXStopEffect, SOn_, coul_action_dmx);

    CMD_DMX_STOPEFFECT: begin // CMD_DMX_STOPEFFECT IDuniverse IDFixture ChanIndex
      RenderBackground;
      txt:=FormatDmxTrack(A[1], A[2], A[3]);
      RenderCmdText(txt, x, coul_action_dmx);
    end;

    TITLECMD_DMX_COPYCHANNEL: begin // TITLECMD_DMX_COPYCHANNEL IDUniverseSource IDFixtureSource dmxadresssource
      RenderBackground;
      txt:=FormatDmxTrack(A[1], A[2], A[3]);
      RenderTitle(SDMXCopy, txt, coul_action_dmx);
    end;

    CMD_DMX_COPYCHANNEL: begin // CMD_DMX_COPYCHANNEL IDUniverseSource IDFixtureSource dmxadresssource universdestination adressedmxdestination
       RenderBackground;
        Font.Height:=Font.Height-1;
        txt:=SOn_+' '+FormatDmxTrack(A[4], A[5], A[6]);
        RenderCmdText(txt, x, coul_action_dmx);
    end;

    TITLECMD_DMX_FLAME: begin  // TITLECMD_DMX_FLAME LevelMin LevelMax Speed Soften
     RenderBackground;
     txt:=SMin+' '+DMXPercent(A[1])+' '+SMax+' '+DMXPercent(A[2])+' '+SSpeed+' '+DurationValue(A[2])+' '+
          SSoften+' '+DMXPercent(A[4]);
     RenderTitle(SDMXFlame, txt, coul_action_dmx);
    end;

    CMD_DMX_FLAME: begin  // CMD_DMX_FLAME IDuniverse IDFixture dmxadress LevelMin LevelMax Speed Soften
       RenderBackground;
        Font.Height:=Font.Height-1;
        txt:=FormatDmxTrack(A[1],A[2],A[3]);
        RenderCmdText(txt, x, coul_action_dmx);
    end;

    TITLECMD_DMX_FLAMERGB: begin // TITLECMD_DMX_FLAMERGB Color Speed Amplitude Soften
       RenderBackground;
        x:=RenderTitle(SDMXFlameRGB+' ', '', coul_action_dmx);
        x:=RenderColor(x, A[1]);
        txt:=SSpeed+' '+DurationValue(A[2])+' '+SAmplitude+' '+DMXPercent(A[3])+' '+
             SSoften+' '+DMXPercent(A[4]);
        x:=RenderCmdText(txt, x, coul_action_dmx);
    end;

    CMD_DMX_FLAMERGB: begin   // CMD_DMX_FLAMERGB IDuniverse IDFixture Color Speed Amplitude Soften
       RenderBackground;
        Font.Height:=Font.Height-1;
        RenderCmdText(FormatFixtureName(A[1], A[2]), x, coul_action_dmx);
    end;

    TITLECMD_DMX_DIMMERRGB: begin // TITLECMD_DMX_DIMMERRGB color duration IDcurve
        RenderBackground;
        x:=RenderTitle(SDMXDimmerRGB+' ', '', coul_action_dmx);
        x:=RenderColor(x, A[1]);
        txt:=SIn+' ' + DurationValue(A[2]);
        x:=RenderCmdText(txt, x, coul_action_dmx)+10;
        RenderCurve(x, arect.Top+1, A[3]);
    end;

    CMD_DMX_DIMMERRGB: begin // CMD_DMX_DIMMERRGB IDuniverse IDFixture color duration IDcurve
        RenderBackground;
        Font.Height:=Font.Height-1;
        RenderCmdText(FormatFixtureName(A[1], A[2]), x, coul_action_dmx);
    end;

    TITLECMD_DMX_STOPEFFECTRGB: begin
        RenderBackground;
        RenderTitle(SDMXStopEffectRGB, '', coul_action_dmx);
    end;

    CMD_DMX_STOPEFFECTRGB: begin // CMD_DMX_STOPEFFECTRGB IDuniverse IDFixture
        RenderBackground;
        Font.Height:=Font.Height-1;
        RenderCmdText(FormatFixtureName(A[1], A[2]), x, coul_action_dmx);
    end;

    TITLECMD_DMX_COPYRGB: begin  // TITLECMD_DMX_COPYRGB SourceIDuniverse SourceIDFixture
        RenderBackground;
        RenderTitle(SDMXCopyRGB, FormatFixtureName(A[1], A[2]), coul_action_dmx);
    end;
    CMD_DMX_COPYRGB: begin // CMD_DMX_COPYRGB SourceIDuniverse SourceIDFixture TargetIDuniverse TargetIDFixture
        RenderBackground;
        Font.Height:=Font.Height-1;
        txt:= SOn_+'  '+FormatFixtureName(A[3], A[4]);
        RenderCmdText(txt, x, coul_action_dmx);
    end;

    TITLECMD_DMX_AUDIOFOLLOWER: begin // TITLECMD_DMX_AUDIOFOLLOWER IDaudio gain MaxPercent SoftenTime
        RenderBackground;
        txt:=''''+FormatAudioFile(A[1])+''' '+SGain+' '+A[2]+' '+SMax+' '+
             DMXPercent(A[3])+' '+SSoftenOn+' '+DurationValue(A[4]);
        RenderTitle(SDMXAudioFollower, txt, coul_action_dmx);
    end;

    CMD_DMX_AUDIOFOLLOWER: begin // CMD_DMX_AUDIOFOLLOWER IDuniverse IDFixture dmxadress IDaudio gainF MaxPercentF SmoothTimeF
        RenderBackground;
        txt := FormatDmxTrack(A[1], A[2], A[3]);
        Font.Height:=Font.Height-1;
        RenderCmdText(txt, x, coul_action_dmx);
    end;

    TITLECMD_DMX_AUDIOFOLLOWERRGB: begin  // TITLECMD_DMX_AUDIOFOLLOWERRGB IDaudio Color Gain SoftenTime
        RenderBackground;
        x:=RenderTitle(SDMXAudioFollowerRGB+' ', '', coul_action_dmx);
        x:=RenderColor(x, A[2]);
        txt:=SOn_+' '''+FormatAudioFile(A[1])+''' '+SGain+' '+FormatFloat1Decimal(A[3])+' '+SSoftenOn+' '+DurationValue(A[4]);
        RenderCmdText(txt, x, coul_action_dmx);
    end;

    CMD_DMX_AUDIOFOLLOWERRGB: begin // CMD_DMX_AUDIOFOLLOWERRGB IDuniverse IDFixture IDaudio Color Gain SoftenTime
        RenderBackground;
        Font.Height:=Font.Height-1;
        txt:=FormatFixtureName(A[1], A[2]);
        RenderCmdText( txt, x, coul_action_dmx);
    end;

    TITLECMD_DMX_FLASHRGB: begin // TITLECMD_DMX_FLASHRGB Color pcMin pcMax DurationMin DurationMax
     RenderBackground;
     x:=RenderTitle(SDMXFlashRGB+' ', '', coul_action_dmx);
     x:=RenderColor(x, A[1]);
     if A[2] <> A[3] then
       txt := SRandomIntensity+' '+DMXPercent(A[2])+' - '+DMXPercent(A[3])+' ';
     if A[4] <> A[5] then
       txt := txt+SRandomDuration+' '+DurationValue(A[4])+' - '+DurationValue(A[5])
     else
       txt := txt+SIn+' '+DurationValue(A[4]);
     x:=RenderCmdText(txt, x, coul_action_dmx);
    end;
    CMD_DMX_FLASHRGB: begin // CMD_DMX_FLASHRGB IDuniverse IDFixture Color pcMin pcMax DurationMin DurationMax
      RenderBackground;
      Font.Height := Font.Height-1;
      RenderCmdText(FormatFixtureName(A[1], A[2]), x, coul_action_dmx);
    end;

    PHOTOFADEINCOULEUR: begin        // 40 couleur durée alpha
        txt := SScreen+' - ';
        Font.Color := coul_action_photo ;
        Font.Style := [fsBold] ;
        TextOut( arect.Left + 8 , arect.Top , txt ) ;
        x := Font.GetTextWidth( txt ) ;
        Font.Height := Font.Height - 1 ;
        txt := SFade+' ' ;
        Font.Style := [] ;
        TextOut( arect.Left + 8 + x , arect.Top , txt ) ;
        x := x + Font.GetTextWidth( txt ) ;
        Pen.Color := clBlack ;
        c := Brush.Color ;
        Brush.Color := TColor ( strtoint ( A[1] ) ) ;
        Rectangle( arect.Left + 8 + x , arect.Top , arect.Left + 8 + x + 15 , arect.Bottom ) ;
        Brush.Color := c ;
        txt := 'en ' + DurationValue(A[2]);
        txt := txt + ' '+SFinalOpacity+' ' + A[3] ;
        TextOut( arect.Left + 8 + x + 20 , arect.Top , txt ) ;
    end;
    PHOTOFADEINIMAGE :           // 41 zoom couleur durée alpha FichierImage
       begin
        txt := SScreen+' - ' ;
        Font.Color := coul_action_photo ;
        Font.Style := [fsBold] ;
        TextOut( arect.Left + 8 , arect.Top , txt ) ;
        x := Font.GetTextWidth( txt ) ;
        Font.Height := Font.Height - 1 ;
        txt := SImage+' + ' ;
        Font.Style := [] ;
        TextOut( arect.Left + 8 + x , arect.Top , txt ) ;
        x := x + Font.GetTextWidth( txt ) ;
        Pen.Color := clBlack ;
        c := Brush.Color ;
        Brush.Color := TColor ( strtoint ( A[2] ) ) ;
        Rectangle( arect.Left + 8 + x , arect.Top , arect.Left + 8 + x + 15 , arect.Bottom ) ;
        Brush.Color := c ;
        txt := SZoom+ ' ';
        if StringToSingle ( A[1] ) = -1 then txt := txt + SMaxi+' '
        else txt := txt + A[1] + '% ' ;
        txt := txt + ' ' + DurationValue(A[3])+' ';
        txt := txt + ' '+SOpacity+' ' + A[4] + ' - ';
        txt := txt + ExtractFileName( A[5] );
        TextOut( arect.Left + 8 + x + 20, arect.Top, txt );
       end;
    PHOTOFADEOUT :           // 42 durée
       begin
        txt := SPhoto+' - ';
        Font.Color := coul_action_photo ;
        Font.Style := [fsBold] ;
        TextOut( arect.Left + 8 , arect.Top , txt ) ;
        x := Font.GetTextWidth( txt ) ;
        Font.Height := Font.Height - 1 ;
        Font.Style := [] ;
        txt := SFadeOutIn+' ' + DurationValue(A[1]);
        TextOut( arect.Left + 8 + x , arect.Top , txt ) ;
       end;
    VIDEOLECTURE :           // videolecture loop(0-1) volume fichier
       begin
        txt := SVideo+' - ' ;
        Font.Color := coul_action_video ;
        Font.Style := [fsBold] ;
        TextOut( arect.Left + 8 , arect.Top , txt ) ;
        x := Font.GetTextWidth( txt ) ;
        Font.Height := Font.Height - 1 ;
        Font.Style := [] ;
        txt := SPlay+' ' ;
        if A[1]='1' then txt := SInLoop+', ' ;
        txt := txt + SVolume+' ' + A[2] + '% - ' ;
        txt := txt + ExtractFileName(A[3]);
        TextOut( arect.Left + 8 + x, arect.Top, txt );
       end;
    VIDEOPAUSE:           // videopause
       begin
        txt := SVideo+' - ' ;
        Font.Color := coul_action_video ;
        Font.Style := [fsBold] ;
        TextOut( arect.Left + 8 , arect.Top , txt ) ;
        x := Font.GetTextWidth( txt ) ;
        Font.Height := Font.Height - 1 ;
        Font.Style := [] ;
        TextOut( arect.Left + 8 + x , arect.Top , SPause ) ;
       end;
    VIDEOREPRENDRE :      // videoreprendre
       begin
        txt := SVideo+' - ' ;
        Font.Color := coul_action_video ;
        Font.Style := [fsBold] ;
        TextOut( arect.Left + 8 , arect.Top , txt ) ;
        x := Font.GetTextWidth( txt ) ;
        Font.Height := Font.Height - 1 ;
        Font.Style := [] ;
        TextOut( arect.Left + 8 + x , arect.Top , SResume ) ;
       end;
    VIDEOSTOP :           // videostop
       begin
        txt := SVideo+' - ' ;
        Font.Color := coul_action_video ;
        Font.Style := [fsBold] ;
        TextOut( arect.Left + 8 , arect.Top , txt ) ;
        x := Font.GetTextWidth( txt ) ;
        Font.Height := Font.Height - 1 ;
        Font.Style := [] ;
        TextOut( arect.Left + 8 + x , arect.Top , SStop ) ;
       end;
    VIDEOFIXEVOLUME :          // videofixevolume volume
       begin
        txt := SVideo+' - ' ;
        Font.Color := coul_action_video ;
        Font.Style := [fsBold] ;
        TextOut( arect.Left + 8 , arect.Top , txt ) ;
        x := Font.GetTextWidth( txt ) ;
        Font.Height := Font.Height - 1 ;
        Font.Style := [] ;
        TextOut( arect.Left + 8 + x , arect.Top , SSetVolumeTo+' ' + A[1] + '%' ) ;
       end;
    VIDEOFADEVOLUME :           // videovolume volume duree
       begin
        txt := SVideo+' - ';
        Font.Color := coul_action_video ;
        Font.Style := [fsBold] ;
        TextOut( arect.Left + 8 , arect.Top , txt ) ;
        x := Font.GetTextWidth( txt ) ;
        Font.Height := Font.Height - 1 ;
        Font.Style := [] ;
        txt := SChangeVolumeTo+' ' + A[1] + '% en ' + DurationValue(A[2]);
        TextOut( arect.Left + 8 + x , arect.Top , txt ) ;
       end;
    VIDEOPOSITION :           // videoposition position
       begin
        txt := SVideo+' - ' ;
        Font.Color := coul_action_video ;
        Font.Style := [fsBold] ;
        TextOut( arect.Left + 8 , arect.Top , txt ) ;
        x := Font.GetTextWidth( txt ) ;
        Font.Height := Font.Height - 1 ;
        Font.Style := [] ;
        TextOut( arect.Left + 8 + x , arect.Top , SJumpToPosition+' ' + A[1] + '%' ) ;
       end;
   end;//case
//   Font.Height:=Font.Height+1;
  end;//with
end;

procedure TFrameViewCmdList.LBKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
 case Key of
   VK_DELETE:
      begin
        MIDeleteSelectionClick(NIL);
        Key := VK_UNKNOWN;
   end;
 end;//case

end;

procedure TFrameViewCmdList.LBMouseLeave(Sender: TObject);
begin
//  FormViewProjector.FrameViewProjector1.ProcessViewDMXCursorsMouseOverFixtureEvent(Self, NIL);
  FItemIndexUnderMouse := -1;
  LB.Invalidate;
end;

procedure TFrameViewCmdList.LBMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var i: integer;
  fix: TDMXFixture;
begin
  i := LB.GetIndexAtY(Y);
  if i <> FItemIndexUnderMouse then
  begin
    FItemIndexUnderMouse := i;
    LB.Invalidate;
  end;

  if i <> -1 then
    fix := GetFixtureFromCmd(LB.Items.Strings[i])
  else
    fix := NIL;
//  FormViewProjector.FrameViewProjector1.ProcessViewDMXCursorsMouseOverFixtureEvent(Self, fix);
end;

procedure TFrameViewCmdList.LBMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var index: Integer;
begin
  index := LB.GetIndexAtXY(X,Y);
  if index = -1 then
    exit;

 { if LB.Items.Strings[index].IsTitle then begin
    // select the block started by the title
    LB.Selected[index]:=TRUE;
    repeat
      inc(index);
      if index=LB.Count then exit;
      if LB.Items.Strings[index].IsTitle then exit;
      LB.Selected[index]:=TRUE;
    until FALSE;
  end else begin   }
    // select only one action
  //  LB.Selected[index]:=TRUE;
 // end;
end;

procedure TFrameViewCmdList.MIColorAsTextClick(Sender: TObject);
var mi: TMenuItem;
begin
 if FLoading then exit;
  mi := TMenuItem(Sender);
  mi.Checked := True;
  Project.Options.CmdListViewColorAsRectangleColor := MIColorAsRectangleColor.Checked;
  LB.Invalidate;
end;

procedure TFrameViewCmdList.MIDeleteSelectionClick(Sender: TObject);
begin
  if ReadOnly then exit;
  if FWorkingStep = NIL then exit;

  FParentSeq.Notify([FWorkingStep], snChanged, SModify);

  LB.DeleteSelected;
  FWorkingStep.CmdList := GetCmdList;
end;

procedure TFrameViewCmdList.MIViewUniverseClick(Sender: TObject);
var mi: TMenuItem;
begin
 if FLoading then exit;
  mi := TMenuItem(Sender);
  mi.Checked := not mi.Checked;
  if mi = MIViewAdress then
    Project.Options.CmdListViewDMXAdress := mi.Checked;
  if mi = MIViewFixtureName then
    Project.Options.CmdListViewDMXFixName := mi.Checked;
  if mi = MIViewDescription then
    Project.Options.CmdListViewDMXFixDescription := mi.Checked;
  if mi=MIViewChannelName then
    Project.Options.CmdListViewDMXChannelName := mi.Checked;
  LB.Invalidate;
end;

procedure TFrameViewCmdList.PopupMenu1Popup(Sender: TObject);
begin
  MIEdit.Enabled := LB.SelCount=1;
  MIDeleteSelection.Enabled := LB.SelCount>0;

  MIViewAdress.Checked := Project.Options.CmdListViewDMXAdress;
  MIViewFixtureName.Checked := Project.Options.CmdListViewDMXFixName;
  MIViewDescription.Checked := Project.Options.CmdListViewDMXFixDescription;
  MIViewChannelName.Checked := Project.Options.CmdListViewDMXChannelName;
end;

function TFrameViewCmdList.GetCmdList: TCmdList;
var
  i: Integer;
begin
  Result := '';
  for i:=0 to LB.Count-1 do
    Result.ConcatCmd( LB.Items.Strings[i] );
end;

procedure TFrameViewCmdList.SetCmdList(AValue: TCmdList);
var
  A: TCmdArray;
  cmd: String;
begin
  LB.Clear;
  A := AValue.SplitToCmdArray;
  if Length(A) = 0 then
    exit;
  LB.LockSelectionChange;
  for cmd in A do
    LB.Items.Add( cmd );
  LB.UnlockSelectionChange;
end;

procedure TFrameViewCmdList.ClipBoard_CutSelection;
begin
  ClipBoard_CopySelection;
  LB.DeleteSelected;
end;

procedure TFrameViewCmdList.ClipBoard_CopySelection;
var i: integer;
begin
 FClipBoard.Clear;
 for i:=0 to LB.Count-1 do
   if LB.Selected[i] then
     FClipBoard.Add(LB.Items.Strings[i]);
end;

procedure TFrameViewCmdList.ClipBoard_PasteBefore(aIndex: integer);
begin
  ClipBoard_PasteAfter(aIndex-1);
end;

procedure TFrameViewCmdList.ClipBoard_PasteAfter(aIndex: integer);
var i: integer;
begin
  if aIndex >= LB.Count-1 then
  begin
    // add at the end
    for i:=0 to FClipBoard.Count-1 do
      LB.Items.Add(FClipBoard.Strings[i]);
  end
  else
  begin
    if aIndex < -1 then
      aIndex := -1;
    for i:=FClipBoard.Count-1 downto 0 do
      LB.Items.Insert(aIndex+1, FClipBoard.Strings[i]);
  end;
end;

procedure TFrameViewCmdList.ClipBoard_Clear;
begin
  FClipBoard.Clear;
end;

function TFrameViewCmdList.Clipboard_HasData: boolean;
begin
  Result := FClipBoard.Count>0;
end;

function TFrameViewCmdList.GetItemHeight: integer;
begin
  Result := LB.ItemHeight;
end;

procedure TFrameViewCmdList.SetItemHeight(AValue: integer);
begin
  LB.ItemHeight := AValue;
  LB.Font.Height := AValue;
end;

constructor TFrameViewCmdList.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FClipBoard := TStringList.Create;
  FItemIndexUnderMouse:=-1;
  FImage := TImage.Create(NIL);
  FImage.Width := 35;
  FImage.Height := 19;
  coul_action_audio := RGBToColor(100,200,255);
  coul_action_dmx  := RGBToColor(210,180,150);
  coul_action_divers := RGBToColor(37,245,89);
  coul_action_photo := RGBToColor (255,165,105);
  coul_action_video := RGBToColor(255,200,100); // clHighLight ;
  coul_action_animation := RGBToColor (255,174,230 );
end;

destructor TFrameViewCmdList.Destroy;
begin
  FImage.Free;
  FClipBoard.Free;
  inherited Destroy;
end;

procedure TFrameViewCmdList.EraseBackground(DC: HDC);
begin
// nothing here
end;

procedure TFrameViewCmdList.UpdateMenuEntryFromProjectOptions;
begin
  FLoading := True;
  MIViewAdress.Checked := Project.Options.CmdListViewDMXAdress;
  MIViewFixtureName.Checked := Project.Options.CmdListViewDMXFixName;
  MIViewDescription.Checked := Project.Options.CmdListViewDMXFixDescription;
  MIViewChannelName.Checked := Project.Options.CmdListViewDMXChannelName;
  MIColorAsRectangleColor.Checked := Project.Options.CmdListViewColorAsRectangleColor;
  FLoading := False;
end;

procedure TFrameViewCmdList.Clear;
begin
  LB.Clear;
  FWorkingStep := NIL;
end;

procedure TFrameViewCmdList.SetWorkingStep(aStep: TSequenceStep;
  aParentSeq: TFrameSequencer);
begin
  FWorkingStep := aStep;
  FParentSeq := aParentSeq;

  if FWorkingStep <> NIL then
    CmdList := FWorkingStep.CmdList
  else
    Clear;
end;

end.

