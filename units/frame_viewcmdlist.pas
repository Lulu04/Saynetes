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
    procedure LBKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure LBKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure LBMouseDown(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, Y: Integer);
    procedure LBMouseLeave(Sender: TObject);
    procedure LBMouseMove(Sender: TObject; {%H-}Shift: TShiftState; {%H-}X, Y: Integer);
    procedure LBMouseUp(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure MIColorAsTextClick(Sender: TObject);
    procedure MIDeleteSelectionClick(Sender: TObject);
    procedure MIEditClick(Sender: TObject);
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
    procedure DoCallbackOnEditedByUser;
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
    function GetItemHeight: integer;
    procedure SetItemHeight(AValue: integer);
  private
    FOnEditedByUser: TNotifyEvent;
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
    property OnEditedByUser: TNotifyEvent read FOnEditedByUser write FOnEditedByUser;

    property ReadOnly: boolean read FReadOnly write FReadOnly;
    property ItemHeight: integer read GetItemHeight write SetItemHeight;
  end;

implementation
uses u_utils, VelocityCurve, ALSound, u_audio_manager, u_list_dmxuniverse,
  u_resource_string, u_list_sequence, u_project_manager, u_helper, u_dmx_util,
  frame_bglvirtualscreen_sequencer, u_program_options, u_edit_singleaction,
  u_logfile, BGRABitmap;

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
   function DurationValue(d: string): string;
   var v: single;
   begin
    v := StringToSingle(d);
    Result := FormatFloat('0.00', v)+SSec;
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

   function FormatPitch(p: string): string;
   begin
    Result := PitchToString(StringToSingle(p));
   end;

   function FormatDryWet(p: string): string;
   begin
    Result := SDryWet+'  '+DryWetToStringPercent(StringToSingle(p));
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
       Result := Result+fix.Name; // fixture name
       f := TRUE;
     end;
     if Project.Options.CmdListViewDMXFixDescription then
     begin
       if f then Result := Result+' - ';
       Result := Result+fix.Description;  // fixture description
       f := TRUE;
     end;
     if Result = '' then Result := fix.Name;
   end;

   function FormatDmxTrack(StrIDUni, StrIDFix, StrAdr: string): string;
   const SEP = ' - ';
   var uni: TDMXUniverse;
    fix: TDMXFixture;
    chan: TDMXChannel;
   begin
    if not UniverseManager.RetrieveChannel(StrIDUni.ToInteger, StrIDFix.ToInteger, StrAdr.ToInteger, uni, fix, chan) then
    begin
      Result := SUniverse+'['+StrIDUni+'] '+SFixture+'['+StrIDFix+'] '+
                SAdress+'['+StrAdr+'] '+SNotFound;
      exit;
    end;
    Result := '';
    if Project.Options.CmdListViewDMXAdress then
      Result.Concat(uni.ShortName+':'+StrAdr, SEP);  // universe short name + dmx adress

    if Project.Options.CmdListViewDMXFixName then
      Result.Concat(fix.Name, SEP); // fixture name

    if Project.Options.CmdListViewDMXFixDescription then
      if Trim(fix.Description) <> '' then Result.Concat(fix.Description, SEP)  // fixture description
        else if not Project.Options.CmdListViewDMXFixName then Result.Concat(fix.Name, SEP);

    if Project.Options.CmdListViewDMXChannelName then
      Result.Concat(chan.Name, SEP); // channel name

    if Trim(Result) = '' then Result := fix.Name;
   end;

   function RenderTitle(const aTitle, aParamTitle: string; aColor: TColor): integer;
   begin
aColor:=RGBToColor(220,220,220);
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
aColor:=RGBToColor(220,220,220);
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
       Pen.Color := clWhite;
       Pen.Style := psDot;
       Brush.Style := bsSolid;
       Rectangle(ax, arect.Top+1, ax+ScaleDesignToForm(20), arect.Bottom-1);
       Pen.Color := cp;
       Brush.Color := cb;
       Pen.Style := psSolid;
       Result := ax+ScaleDesignToForm(30);
     end else begin
       Result := RenderCmdText(Format('(%d,%d,%d) ', [Red(c), Green(c), Blue(c)]), ax, RGBToColor(192,182,172));
    end;
   end;

   function RenderCurve(ax, ay: integer; strIDCourbure: string): integer;
   var id: word;
    ima: TBGRABitmap;
    s: string;
   begin
    id := strtoint(strIDCourbure);
    if VelocityCurveList.ValidCurveID(id) then
    begin
      ima := VelocityCurveList.GetCurveByID(id).GetBGRABitmapImage(LB.Font.Height*2, LB.Font.Height, True);
      ima.Draw(LB.Canvas, ax, ay, True);
      Result := ax + ima.Width;
    end
    else
    begin
      LB.Canvas.Font.Color := clWhite;
      s := ' ??'+SCurve+'?? ';
      LB.Canvas.TextOut(ax, ay, s);
      Result := ax + LB.Canvas.TextWidth(s);
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

    CMD_SEQUENCESTRETCHTIME: begin // CMD_SEQUENCESTRETCHTIME IDSeq StretchValueF DurationF CurveID
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
        txt := txt + '   '+SIn+' ' + DurationValue(A[3]);
        x := RenderCmdText( txt, x, coul_action_audio)+10;
        RenderCurve(x, arect.Top, A[4]);
    end;

    CMD_AUDIO_FADEOUT: begin // CMD_AUDIO_FADEOUT IDaudio duration IDcurve
        RenderBackground;
        Font.Height := Font.Height - 1;
        txt := FormatAudioFile( A[1] );
        txt := txt + '   '+SIn+' '+DurationValue(A[2]);
        x := RenderCmdText( txt, x, coul_action_audio)+10;
        RenderCurve(x, arect.Top, A[3]);
    end;

    CMD_AUDIO_SETVOLUME: begin // CMD_AUDIO_SETVOLUME IDaudio volume duration IDcurve
        RenderBackground;
        Font.Height := Font.Height - 1 ;
        txt := FormatAudioFile( A[1] );
        txt := txt + '   '+STo+' ' + FormatVolume(A[2]);
        txt := txt + '   '+SIn+' ' + DurationValue(A[3]);
        x := RenderCmdText( txt, x, coul_action_audio)+10;
        RenderCurve(x, arect.Top, A[4]);
    end;

    CMD_AUDIO_SETPAN: begin// CMD_AUDIO_SETPAN IDaudio pan duration IDcurve
      RenderBackground;
      Font.Height := Font.Height-1;
      txt := FormatAudioFile(A[1])+'   '+FormatPan(A[2]);
      txt := txt+'   '+SIn+'   '+DurationValue(A[3]);
      x:=RenderCmdText( txt, x, coul_action_audio)+10;
      RenderCurve( x, arect.Top, A[4] );
    end;

    CMD_AUDIO_SETPITCH: begin   // CMD_AUDIO_SETPITCH IDaudio pitch duration IDcurve
      RenderBackground;
      Font.Height := Font.Height-1;
      txt := FormatAudioFile(A[1]) + '   '+STo+' ' + FormatPitch(A[2]);
      txt := txt+'   '+SIn+'   '+DurationValue(A[3]);
      x := RenderCmdText( txt, x, coul_action_audio)+10;
      RenderCurve( x, arect.Top, A[4] );
    end;

    TITLECMD_AUDIO_APPLYFX: begin // TITLECMD_AUDIO_APPLYFX IDaudio dry/wet EffectCount
      RenderBackground;
      txt2 := SOn_ + ' ' + FormatAudioFile(A[1]) +' '+ FormatDryWet(A[2]);
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

    TITLECMD_AUDIO_CAPTURE_APPLYFX: begin // TITLECMD_AUDIO_CAPTURE_APPLYFX dry/wet EffectCount
      RenderBackground;
      txt2 := FormatDryWet(A[1]);
      RenderTitle( SAudioCaptureConnectEffect, txt2, coul_action_audio);
    end;

    CMD_AUDIO_CAPTURE_REMOVEFX: begin // CMD_AUDIO_CAPTURE_REMOVEFX
      RenderBackground;
      txt := SAudioCaptureDisconnectEffect;
      x := RenderCmdText(txt, x, coul_action_audio);
    end;

    TITLECMD_DMX_DIMMER: begin  // TITLECMD_DMX_DIMMER  duration IDcurve
        RenderBackground;
        txt := SIn+' '+DurationValue(A[1])+'  ';
        x := RenderTitle(SDMXDimmer, txt, coul_action_dmx);
        RenderCurve(x, arect.Top, A[2]);
    end;

    CMD_DMX_DIMMER: begin  // CMD_DMX_DIMMER IDuniverse IDFixture dmxadress percent duration IDcurve
      RenderBackground;
      Font.Height := Font.Height-1;
      txt := FormatDmxTrack(A[1], A[2], A[3])+' '+STo+' '+DMXPercent(A[4]);
      x := RenderCmdText(txt, x, coul_action_dmx);
    end;

    TITLECMD_DMX_WAVE: begin // TITLECMD_DMX_WAVE Level1 Duration1 CurveID1 Level2 Duration2 CurveID2
      RenderBackground;
      txt := DMXPercent(A[1])+' '+SIn+' '+DurationValue(A[2])+' ';
      x := RenderTitle(SDMXWave, txt, coul_action_dmx);
      x := RenderCurve(x, arect.Top, A[3]);
      txt := ' '+SThen+' '+DMXPercent(A[4])+' '+SIn+' '+DurationValue(A[5])+' ';
      x := RenderCmdText(txt, x, coul_action_dmx);
      x := RenderCurve(x, arect.Top, A[6]);
    end;

    CMD_DMX_WAVE: begin // CMD_DMX_WAVE IDuniverse IDFixture ChanIndex Level1 Duration1 CurveID1 Level2 Duration2 CurveID2
      RenderBackground;
      Font.Height := Font.Height-1;
      txt := FormatDmxTrack(A[1], A[2], A[3]);
      x := RenderCmdText(txt, x, coul_action_dmx);
    end;

    CMD_INTERNALDMXWAVE: begin// CMD_INTERNALDMXWAVE IDuniverse IDFixture dmxadress percent1 duration1 IDcurve1 percent2 duration2 IDcurve2
     RenderBackground;
     Font.Height:=Font.Height-1;
     txt:=SDMXWave+' '+SOn_+' '+FormatDmxTrack(A[1], A[2], A[3])+' '+STo+' '+DMXPercent(A[4])+'<->'+DMXPercent(A[7]);
     x:=RenderCmdText(txt, x, coul_action_dmx);
    end;

    TITLECMD_DMX_FLASH: begin  // TITLECMD_DMX_FLASH LevelMin LevelMax DurationMin DurationMax
     RenderBackground;
     if A[1] <> A[2] then txt := SRandomValue+' '+DMXPercent(A[1])+' - '+DMXPercent(A[2])+' '
       else txt := STo+' '+DMXPercent(A[1])+' ';
     if A[3] <> A[4] then txt := txt+SRandomDuration+' '+DurationValue(A[3])+' - '+DurationValue(A[4])
       else txt := txt+SIn+' '+DurationValue(A[3]);
     RenderTitle( SDMXFlash, txt, coul_action_dmx );
    end;
    CMD_DMX_FLASH: begin // CMD_DMX_FLASH IDuniverse IDFixture ChanIndex
                         //               LevelMin LevelMax DurationMin DurationMax
     RenderBackground;
     Font.Height := Font.Height-1;
     txt := FormatDmxTrack(A[1], A[2], A[3]); // +' ';
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

    TITLECMD_DMX_FLAME: begin  // TITLECMD_DMX_FLAME LevelMin LevelMax WaitTimeF Soften
     RenderBackground;
     txt := SMin+' '+DMXPercent(A[1])+' '+SMax+' '+DMXPercent(A[2])+' '+SWaitTime+' '+DurationValue(A[3])+' '+
           SSoften+' '+DMXPercent(A[4]);
     RenderTitle(SDMXFlame, txt, coul_action_dmx);
    end;

    CMD_DMX_FLAME: begin  // CMD_DMX_FLAME IDuniverse IDFixture dmxadress LevelMin LevelMax Speed Soften
       RenderBackground;
        Font.Height := Font.Height - 1;
        txt := FormatDmxTrack(A[1], A[2], A[3]);
        RenderCmdText(txt, x, coul_action_dmx);
    end;

    TITLECMD_DMX_FLAMERGB: begin // TITLECMD_DMX_FLAMERGB Color WaitTime Amplitude Soften
       RenderBackground;
        x := RenderTitle(SDMXFlameRGB+' ', '', coul_action_dmx);
        x := RenderColor(x, A[1]);
        txt := SWaitTime+' '+DurationValue(A[2])+' '+SAmplitude+' '+DMXPercent(A[3])+' '+
             SSoften+' '+DMXPercent(A[4]);
        x := RenderCmdText(txt, x, coul_action_dmx);
    end;

    CMD_DMX_FLAMERGB: begin   // CMD_DMX_FLAMERGB IDuniverse IDFixture Color WaitTime Amplitude Soften
       RenderBackground;
        Font.Height := Font.Height-1;
        RenderCmdText(FormatFixtureName(A[1], A[2]), x, coul_action_dmx);
    end;

    TITLECMD_DMX_DIMMERRGB: begin // TITLECMD_DMX_DIMMERRGB color duration IDcurve
        RenderBackground;
        x := RenderTitle(SDMXDimmerRGB+' ', '', coul_action_dmx);
        x := RenderColor(x, A[1]);
        txt := SIn+' ' + DurationValue(A[2]);
        x := RenderCmdText(txt, x, coul_action_dmx)+10;
        RenderCurve(x, arect.Top+1, A[3]);
    end;

    CMD_DMX_DIMMERRGB: begin // CMD_DMX_DIMMERRGB IDuniverse IDFixture color duration IDcurve
        RenderBackground;
        Font.Height := Font.Height-1;
        RenderCmdText(FormatFixtureName(A[1], A[2]), x, coul_action_dmx);
    end;

    TITLECMD_DMX_STOPEFFECTRGB: begin
        RenderBackground;
        RenderTitle(SDMXStopEffectRGB, '', coul_action_dmx);
    end;

    CMD_DMX_STOPEFFECTRGB: begin // CMD_DMX_STOPEFFECTRGB IDuniverse IDFixture
        RenderBackground;
        Font.Height := Font.Height-1;
        RenderCmdText(FormatFixtureName(A[1], A[2]), x, coul_action_dmx);
    end;

    TITLECMD_DMX_COPYRGB: begin  // TITLECMD_DMX_COPYRGB SourceIDuniverse SourceIDFixture
        RenderBackground;
        RenderTitle(SDMXCopyRGB, FormatFixtureName(A[1], A[2]), coul_action_dmx);
    end;
    CMD_DMX_COPYRGB: begin // CMD_DMX_COPYRGB SourceIDuniverse SourceIDFixture TargetIDuniverse TargetIDFixture
        RenderBackground;
        Font.Height := Font.Height-1;
        txt := SOn_+'  '+FormatFixtureName(A[3], A[4]);
        RenderCmdText(txt, x, coul_action_dmx);
    end;

    TITLECMD_DMX_AUDIOFOLLOWER: begin // TITLECMD_DMX_AUDIOFOLLOWER IDaudio gain MaxPercent SoftenTime
        RenderBackground;
        txt := ''''+FormatAudioFile(A[1])+''' '+SGain+' '+A[2]+' '+SMax+' '+
               DMXPercent(A[3])+' '+SSoftenOn+' '+DurationValue(A[4]);
        RenderTitle(SDMXAudioFollower, txt, coul_action_dmx);
    end;

    CMD_DMX_AUDIOFOLLOWER: begin // CMD_DMX_AUDIOFOLLOWER IDuniverse IDFixture dmxadress IDaudio gainF MaxPercentF SmoothTimeF
        RenderBackground;
        txt := FormatDmxTrack(A[1], A[2], A[3]);
        Font.Height := Font.Height-1;
        RenderCmdText(txt, x, coul_action_dmx);
    end;

    TITLECMD_DMX_AUDIOFOLLOWERRGB: begin  // TITLECMD_DMX_AUDIOFOLLOWERRGB IDaudio Color Gain SoftenTime
        RenderBackground;
        x := RenderTitle(SDMXAudioFollowerRGB+' ', '', coul_action_dmx);
        x := RenderColor(x, A[2]);
        txt := SOn_+' '''+FormatAudioFile(A[1])+''' '+SGain+' '+FormatFloat1Decimal(A[3])+' '+SSoftenOn+' '+DurationValue(A[4]);
        RenderCmdText(txt, x, coul_action_dmx);
    end;

    CMD_DMX_AUDIOFOLLOWERRGB: begin // CMD_DMX_AUDIOFOLLOWERRGB IDuniverse IDFixture IDaudio Color Gain SoftenTime
        RenderBackground;
        Font.Height := Font.Height-1;
        txt := FormatFixtureName(A[1], A[2]);
        RenderCmdText(txt, x, coul_action_dmx);
    end;

    TITLECMD_DMX_FLASHRGB: begin // TITLECMD_DMX_FLASHRGB Color pcMin pcMax DurationMin DurationMax
     RenderBackground;
     x := RenderTitle(SDMXFlashRGB+' ', '', coul_action_dmx);
     x := RenderColor(x, A[1]);
     if A[2] <> A[3] then txt := SRandomIntensity+' '+DMXPercent(A[2])+' - '+DMXPercent(A[3])+' '
       else txt := SFixedIntensity+' '+DMXPercent(A[2]);
     if A[4] <> A[5] then txt := txt+SRandomDuration+' '+DurationValue(A[4])+' - '+DurationValue(A[5])
       else txt := txt+SIn+' '+DurationValue(A[4]);
     x := RenderCmdText(txt, x, coul_action_dmx);
    end;
    CMD_DMX_FLASHRGB: begin // CMD_DMX_FLASHRGB IDuniverse IDFixture Color pcMin pcMax DurationMin DurationMax
      RenderBackground;
      Font.Height := Font.Height-1;
      RenderCmdText(FormatFixtureName(A[1], A[2]), x, coul_action_dmx);
    end;

    TITLECMD_DMX_WAVERGB: begin // TITLECMD_DMX_WAVERGB Color1 Duration1 CurveID1 Color2 Duration2 CurveID2
      RenderBackground;
      x := RenderTitle(SDMXWaveRGB+' ', '', coul_action_dmx);
      x := RenderColor(x, A[1]);
      x := RenderCmdText(SIn+' '+DurationValue(A[2]), x, coul_action_dmx);
      x := RenderCurve(x, arect.Top+1, A[3]);
      x := RenderCmdText(' '+SThen+' ', x, coul_action_dmx);
      x := RenderColor(x, A[4]);
      x := RenderCmdText(SIn+' '+DurationValue(A[5]), x, coul_action_dmx);
      x := RenderCurve(x, arect.Top+1, A[6]);
    end;

    CMD_DMX_WAVERGB: begin // CMD_DMX_WAVERGB IDuniverse IDFixture Color1 Duration1 CurveID1 Color2 Duration2 CurveID2
      RenderBackground;
      Font.Height := Font.Height-1;
      RenderCmdText(FormatFixtureName(A[1], A[2]), x, coul_action_dmx);
    end;

    IMAGEFADEINCOLOR: begin        // 40 Color Duration Alpha
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
    IMAGEFADEIN: begin  // 41 Zoom bgColor Duration alpha FichierImage
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
    IMAGEFADEOUT: begin       // 42 Duration
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

procedure TFrameViewCmdList.LBKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  // avoid selection change when user use this keys
  if Key in [VK_LEFT, VK_UP, VK_RIGHT, VK_DOWN] then Key := VK_UNKNOWN;
end;

procedure TFrameViewCmdList.LBKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
 case Key of
   VK_DELETE: begin
     MIDeleteSelectionClick(NIL);
     Key := VK_UNKNOWN;
   end;
   VK_LEFT, VK_UP, VK_RIGHT, VK_DOWN: Key := VK_UNKNOWN;
 end;//case

end;

procedure TFrameViewCmdList.LBMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var i: integer;
begin
  i := LB.GetIndexAtY(Y);

  // click on empty area = unselect all
  if (Button = mbLeft) and (i = -1) then
    LB.ItemIndex := -1;

  // right click on item = select it (except if it is already selected)
  if (Button = mbRight) and (i <> -1) then
    if not LB.Selected[i] then
      LB.ItemIndex := i;
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
var index, cmd: Integer;
begin
  index := LB.GetIndexAtXY(X,Y);
  if index = -1 then
    exit;

  if LB.Items.Strings[index].IsTitle then begin
    cmd := LB.Items.Strings[index].GetOnlyCmd-1000;
    // select the block started by the title
    LB.Selected[index] := TRUE;
    repeat
      inc(index);
      if index = LB.Count then exit;
      if LB.Items.Strings[index].GetOnlyCmd <> cmd then exit;
      LB.Selected[index] := TRUE;
    until FALSE;
  end else begin
    // select only one action
    LB.Selected[index] := TRUE;
  end;
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
  FWorkingStep.CheckCmdError;
  DoCallbackOnEditedByUser;
end;

procedure TFrameViewCmdList.MIEditClick(Sender: TObject);
var F: TFormEditSingleAction;
  indexFirstSelected, i: integer;
  flagSingleCmd, flagHaveTitle: boolean;
  s: TSingleCmd;
begin
  if ReadOnly then exit;
  if FWorkingStep = NIL then exit;
  if LB.SelCount = 0 then exit;

  // retrieve the index of the first selected item and check if all selected lines are single cmd
  indexFirstSelected := -1;
  flagSingleCmd := True;
  for i:=0 to LB.Count-1 do
    if LB.Selected[i] then begin
      if indexFirstSelected = -1 then indexFirstSelected := i;
      flagSingleCmd := flagSingleCmd and LB.Items.Strings[i].IsSingleCmd;
    end;
  if not flagSingleCmd then exit;
  flagHaveTitle := LB.Items.Strings[indexFirstSelected].IsTitle;
  if (LB.SelCount > 1) and not flagHaveTitle then exit;

  F := TFormEditSingleAction.Create(NIL);
  F.Cmd := LB.Items.Strings[indexFirstSelected];
  if F.CmdIsEditable then begin
    if F.ShowModal = mrOk then begin
      LB.Items.Strings[indexFirstSelected] := F.Cmd;

      // modify action param from title param
      if (LB.SelCount > 1) and flagHaveTitle then begin
        i := indexFirstSelected + 1;
        while (i < LB.Count) and LB.Selected[i] do begin
          s := LB.Items.Strings[i];
          s.ChangeParamFromTitleParam(F.Cmd);
          LB.Items.Strings[i] := s;
          inc(i);
        end;
      end;

      LB.Invalidate;
      FWorkingStep.CmdList := GetCmdList; // save changes in the sequence
      FWorkingStep.Duration := FWorkingStep.CmdList.ComputeCmdListDuration;
      FWorkingStep.UpdateWidth;
      FWorkingStep.CheckCmdError;
      DoCallbackOnEditedByUser;
    end;
  end;
  F.Free;
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
  MIEdit.Enabled := LB.SelCount > 0;
  MIDeleteSelection.Enabled := (LB.SelCount > 0) and (LB.SelCount < LB.Count);

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

procedure TFrameViewCmdList.DoCallbackOnEditedByUser;
begin
  if FOnEditedByUser <> NIL then FOnEditedByUser(Self);
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
  FItemIndexUnderMouse := -1;
  coul_action_audio := RGBToColor(100,200,255);
  coul_action_dmx  := RGBToColor(210,180,150);
  coul_action_divers := RGBToColor(37,245,89);
  coul_action_photo := RGBToColor (255,165,105);
  coul_action_video := RGBToColor(255,200,100); // clHighLight ;
  coul_action_animation := RGBToColor (255,174,230 );
end;

destructor TFrameViewCmdList.Destroy;
begin
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

