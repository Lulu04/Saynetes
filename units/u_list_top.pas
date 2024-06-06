unit u_list_top;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fgl,
  u_common, VelocityCurve;


type
  TTopList = class;
  { TSequence }

  TSequence = class
  private
    FErrorMessage: string;
    FHaveError: boolean;
    FSequencerInfoList: TSequencerInfoList;
    FIsLooped: boolean;
    procedure SetSequencerInfoList(AValue: TSequencerInfoList);
  public
     Name: string;
     ID: Cardinal;
     // field for exec
     Running: boolean;
     CmdArray: TCmdArray;
     LineIndex: integer;
     WaitSec,
     Clock: single;    // time accumulator since the top is running. Loop command reset it.
     TimeStretchFactor: TFParam;
     constructor Create;
     destructor Destroy; override;
     procedure InitByDefault;
     procedure RunAsCmdList;
     procedure RunAsSequencerInfoList;
     procedure LoopToBegin;
     procedure Stop;
     procedure NextLine;
     function EndOfPlay: boolean;
     function Duplicate: TSequence;

     property SequencerInfoList: TSequencerInfoList read FSequencerInfoList write SetSequencerInfoList;
     property IsLooped: boolean read FIsLooped;

  public
     // scan the actions in the sequence and return true if there is an error
     // like dmx fixture not found, audio not found, sequence not found...
     // the two following property are initilized
     function CheckError(aParentList: TTopList): boolean;
     property HaveError: boolean read FHaveError write FHaveError;
     property ErrorMessage: string read FErrorMessage write FErrorMessage;
  end;


  { TTopList }

  TTopList = class(specialize TFPGObjectList<TSequence>)   //class(specialize TList<TSequence>)
  private
    FID: cardinal;
  public
    procedure ClearAll;
    function NextID: cardinal;

    function NameAlreadyExists(const aName: string): boolean;
    function IsValidIndex(aIndex: integer): boolean;

    function AddTop(const aName: string; const aSequencerInfoList: TSequencerInfoList): TSequence;
    function InsertTop(aIndex: integer; const aName: string; const aSequencerInfoList: TSequencerInfoList): TSequence;
    function GetTopByID(aID: cardinal): TSequence;
    function GetSequenceByStrID(const aStrID: string): TSequence;
    function GetTopByIndex(aIndex: integer): TSequence;

    function GetNameByID(aID: cardinal): string;
    function IDToIndex(aID: cardinal): integer;

    procedure Save(temp: TStrings);
    procedure Load(temp: TStrings);

    // checks the integrity of each action in all sequences and return True if an error is found.
    function CheckErrorInSequences: boolean;

    procedure StopAll;

    function Duplicate(aID: cardinal): TSequence;

    property ID: cardinal read FID write FID;
  end;

var
  Sequences: TTopList;

implementation

uses u_resource_string, u_helper, u_utils, u_logfile, u_list_dmxuniverse,
  u_audio_manager, Math, ALSound;

{ TSequence }

procedure TSequence.SetSequencerInfoList(AValue: TSequencerInfoList);
begin
  FSequencerInfoList := AValue;
  FIsLooped := AValue.SequencerInfoListIsLooped;
end;

constructor TSequence.Create;
begin
  TimeStretchFactor := TFParam.Create;
  InitByDefault;
end;

destructor TSequence.Destroy;
begin
  TimeStretchFactor.Free;
  inherited Destroy;
end;

procedure TSequence.InitByDefault;
begin
  Running := FALSE;
  WaitSec := 0;
  Clock := 0;
  LineIndex := 0;
  TimeStretchFactor.Value := 1.0;
  FIsLooped := False;
end;

procedure TSequence.RunAsCmdList;
begin
  CmdArray:=SequencerInfoList.SplitToCmdArray;
  LineIndex:=0;
  WaitSec:=0.0;
  Clock:=0.0;
  if Length(CmdArray)>0
    then Running:=TRUE;
end;

procedure TSequence.RunAsSequencerInfoList;
var cmds: TCmdList;
begin
  cmds:=SequencerInfoList.SequencerInfoListToCmdListOfSingleCmd;
  CmdArray:=cmds.SplitToCmdArray;
  LineIndex:=0;
  WaitSec:=0.0;
  Clock:=0.0;
  if Length(CmdArray)>0
    then Running:=TRUE;
end;

procedure TSequence.LoopToBegin;
begin
  LineIndex:=-1;
  Clock:=0.0;
end;

procedure TSequence.Stop;
begin
  Running:=FALSE;
end;

procedure TSequence.NextLine;
begin
  inc(LineIndex);
end;

function TSequence.EndOfPlay: boolean;
begin
  Result:= LineIndex>=Length(CmdArray);
end;

function TSequence.Duplicate: TSequence;
begin
  Result:=TSequence.Create;
  Result.Name:=Name;
  Result.ID:=ID;
  Result.SequencerInfoList:=SequencerInfoList;
end;

function TSequence.CheckError(aParentList: TTopList): boolean;
var cmds: TCmdList;
  i: integer;
  mess: string;
  vSingle: single;
  vInteger: integer;

  function ErrorOnCmd(const aCmd: string; out errMess: string): boolean;
  var A: TParamArray;
    B: TCmdArray;
    j, paramCount, cmd: integer;
     function ErrorOnParamCount(aCount: integer; const aStrCmd: string): boolean;
     begin
       if Length(A) <> aCount then begin
         errMess := aStrCmd+': '+SWrongNumberOfParameter;
         Result := True;
       end else Result := False;
     end;
     function ErrorOnAudioID(const aAudioID, aStrCmd: string): boolean;
     begin
       if SoundManager.GetSoundByStrID(aAudioID) = NIL then begin
         errMess := aStrCmd+': '+SAudioNotFound;
         Result := True;
       end else Result := False;
     end;
     function ErrorOnSequenceID(const aSeqID, aStrCmd: string): boolean;
     begin
       if aParentList.GetSequenceByStrID(aSeqID) = NIL then begin
         errMess := aStrCmd+' '+aSeqID+' << '+SNotFound;
         Result := True;
       end else Result := False;
     end;
     function ErrorOnSingleRange(const aStrSingle: string; aMin, aMax: single; const aStrCmd, aStrBadValue: string): boolean;
     begin
       try
         vSingle := StringToSingle(aStrSingle);
         if InRange(vSingle, aMin, aMax) then exit(False);
       except
       end;
       errMess := aStrCmd+' '+aStrBadValue;
       Result := True;
     end;
     function ErrorOnIntegerRange(const aStrInt: string; aMin, aMax: integer; const aStrCmd, aStrBadValue: string): boolean;
     begin
       if TryStrToInt(aStrInt, vInteger) and InRange(vInteger, aMin, aMax) then exit(False);
       errMess := aStrCmd+' '+aStrBadValue;
       Result := True;
     end;

     function ErrorOnVolume(const aStrVolume, aStrCmd: string): boolean;
     begin
       Result := ErrorOnSingleRange(aStrVolume, 0.0, 1.0, aStrCmd, SBadVolumeValue);
     end;
     function ErrorOnPan(const aStrPan, aStrCmd: string): boolean;
     begin
       Result := ErrorOnSingleRange(aStrPan, -1.0, 1.0, aStrCmd, SBadPanValue);
     end;
     function ErrorOnPitch(const aStrPitch, aStrCmd: string): boolean;
     begin
       Result := ErrorOnSingleRange(aStrPitch, ALS_PITCH_MIN, ALS_PITCH_MAX, aStrCmd, SBadPitchValue);
     end;
     function ErrorOnDuration(const aStrDuration, aStrCmd: string): boolean;
     begin
       Result := ErrorOnSingleRange(aStrDuration, 0.0, Single.MaxValue, aStrCmd, SDurationIsNegative);
     end;
     function ErrorOnDryWet(const aStrDryWetValue, aStrCmd: string): boolean;
     begin
       Result := ErrorOnSingleRange(aStrDryWetValue, 0.0, 1.0, aStrCmd, SBadDryWetValue);
     end;
     function ErrorOnAudioEffectCount(const aStrCount, aStrCmd: string): boolean;
     begin
       Result := ErrorOnIntegerRange(aStrCount, 1, 3, aStrCmd, SBadAudioEffectCountValue);
     end;
     function ErrorOnAudioEffectType(const aStrType, aStrCmd: string): boolean;
     begin
       if TryStrToInt(aStrType, vInteger) and
         ((vInteger in [Ord(ALSound.AL_EFFECT_NONE)..Ord(ALSound.AL_EFFECT_EQUALIZER)]) or
          (vInteger = Ord(ALSound.AL_EFFECT_EAXREVERB))) then exit(False);
       errMess := aStrCmd+': '+SBadEffectTypeValue;
       Result := True;
     end;
     function ErrorOnAudioPreset(const aStrIndex, aStrCmd: string): boolean;
     begin
       if TryStrToInt(aStrIndex, vInteger) then exit(False);
       errMess := aStrCmd+': '+SBadAudioPresetIndex;
       Result := True;
     end;

     function ErrorOnCurve(const aStrCurve, aStrCmd: string): boolean;
     begin
       if TryStrToInt(aStrCurve, vInteger) and CurveIDIsValid(vInteger) then exit(False);
       errMess := aStrCmd+': '+SBadVelocityCurveValue;
       Result := True;
     end;
     function ErrorOnDmxAdress(const aStrUni, aStrFix, aStrChanIndex, aStrCmd: string): boolean; overload;
     var uni: TDmxUniverse;
       fix: TDmxFixture;
     begin
       uni := UniverseManager.GetUniverseByStrID(aStrUni);
       if uni = NIL then begin
         errMess := aStrCmd+': '+SUniverseNotFound;
         exit(True);
       end;
       fix := uni.GetFixtureByStrID(aStrFix);
       if fix = NIL then begin
         errMess := aStrCmd+': '+SFixtureNotFound;
         exit(True);
       end;
       if fix.GetChannelByStrIndex(aStrChanIndex) = NIL then begin
         errMess := aStrCmd+': '+SChannelNotFound;
         exit(True);
       end;
       Result := False;
     end;
     function ErrorOnDmxAdress(const aStrUni, aStrFix, aStrCmd: string): boolean; overload;
     var uni: TDmxUniverse;
       fix: TDmxFixture;
     begin
       uni := UniverseManager.GetUniverseByStrID(aStrUni);
       if uni = NIL then begin
         errMess := aStrCmd+': '+SUniverseNotFound;
         exit(True);
       end;
       fix := uni.GetFixtureByStrID(aStrFix);
       if uni.GetFixtureByStrID(aStrFix) = NIL then begin
         errMess := aStrCmd+': '+SFixtureNotFound;
         exit(True);
       end;
       Result := False;
     end;
     function ErrorOnColor(const aStrColor, aStrCmd: string): boolean;
     begin
       if TryStrToInt(aStrColor, vInteger) and (vInteger >= 0) and (vInteger <= $FFFFFF) then exit(False);
       errMess := aStrCmd+': '+SBadColorValue;
       Result := True;
     end;

  begin
    Result := False;
    errMess := '';
    if not aCmd.IsSingleCmd then begin
      // its a complex action
      B := aCmd.SplitToCmdArray;
      for j:=0 to High(B) do begin
        Result := Result or ErrorOnCmd(B[j], errMess);
        if Result then exit;
      end;
    end else begin
      // its a single action
      A := aCmd.SplitToParamArray;
      paramCount := Length(A);
      if not TryStrToInt(A[0], cmd) then begin
        errMess := SUnrecognizedAction+' "'+A[0]+'"';
        exit(True);
      end;
      case cmd of
        TITLECMD_AUDIO_PLAY: begin
          if ErrorOnParamCount(1, SAudioPlay) then exit(True);
        end;
        TITLECMD_AUDIO_STOP: begin
          if ErrorOnParamCount(1, SAudioStop) then exit(True);
        end;
        TITLECMD_AUDIO_PAUSE: begin
          if ErrorOnParamCount(1, SAudioPause) then exit(True);
        end;
        TITLECMD_AUDIO_FADEIN: begin
          if ErrorOnParamCount(1, SAudioFadeIn) then exit(True);
        end;
        TITLECMD_AUDIO_FADEOUT: begin
          if ErrorOnParamCount(1, SAudioFadeOut) then exit(True);
        end;
        TITLECMD_AUDIO_SETVOLUME: begin
          if ErrorOnParamCount(1, SAudioSetVolume) then exit(True);
        end;
        TITLECMD_AUDIO_SETPAN: begin
          if ErrorOnParamCount(1, SAudioSetPan) then exit(True);
        end;
        TITLECMD_AUDIO_SETPITCH: begin
          if ErrorOnParamCount(1, SAudioSetFreq) then exit(True);
        end;

        CMD_AUDIO_PLAY: begin // CMD_AUDIO_PLAY IDaudio volume panning
          if ErrorOnParamCount(4, SAudioPlay) then exit(True);
          if ErrorOnAudioID(A[1], SAudioPlay) then exit(True);
          if SoundManager.GetSoundByStrID(A[1]) = NIL then begin
            errMess := SAudioPlay+' '+A[1]+' << '+SNotFound;
            exit(True);
          end;
          if ErrorOnVolume(A[2], SAudioPlay) then exit(True);
        end;

        CMD_AUDIO_STOP: begin  // CMD_AUDIO_STOP IDaudio
          if ErrorOnParamCount(2, SAudioStop) then exit(True);
          if ErrorOnAudioID(A[1], SAudioStop) then exit(True);
        end;

        CMD_AUDIO_PAUSE: begin  // CMD_AUDIO_PAUSE IDaudio
          if ErrorOnParamCount(2, SAudioPause) then exit(True);
          if ErrorOnAudioID(A[1], SAudioPause) then exit(True);
        end;

        CMD_AUDIO_FADEIN: begin  // CMD_AUDIO_FADEIN IDaudio volume duration IDcurve
          if ErrorOnParamCount(5, SAudioFadeIn) then exit(True);
          if ErrorOnAudioID(A[1], SAudioFadeIn) then exit(True);
          if ErrorOnVolume(A[2], SAudioFadeIn) then exit(True);
          if ErrorOnDuration(A[3], SAudioFadeIn) then exit(True);
          if ErrorOnCurve(A[4], SAudioFadeIn) then exit(True);
        end;

        CMD_AUDIO_FADEOUT: begin  // CMD_AUDIO_FADEOUT IDaudio duration IDcurve
          if ErrorOnParamCount(4, SAudioFadeOut) then exit(True);
          if ErrorOnAudioID(A[1], SAudioFadeOut) then exit(True);
          if ErrorOnDuration(A[2], SAudioFadeOut) then exit(True);
          if ErrorOnCurve(A[3], SAudioFadeOut) then exit(True);
        end;

        CMD_AUDIO_SETVOLUME: begin  // CMD_AUDIO_SETVOLUME IDaudio volume duration IDcurve
          if ErrorOnParamCount(5, SAudioSetVolume) then exit(True);
          if ErrorOnAudioID(A[1], SAudioSetVolume) then exit(True);
          if ErrorOnVolume(A[2], SAudioSetVolume) then exit(True);
          if ErrorOnDuration(A[3], SAudioSetVolume) then exit(True);
          if ErrorOnCurve(A[4], SAudioSetVolume) then exit(True);
        end;

        CMD_AUDIO_SETPAN: begin  // CMD_AUDIO_SETPAN IDaudio panning duration IDcurve
          if ErrorOnParamCount(5, SAudioSetPan) then exit(True);
          if ErrorOnAudioID(A[1], SAudioSetPan) then exit(True);
          if ErrorOnPan(A[2], SAudioSetPan) then exit(True);
          if ErrorOnDuration(A[3], SAudioSetPan) then exit(True);
          if ErrorOnCurve(A[4], SAudioSetPan) then exit(True);
        end;

        CMD_AUDIO_SETPITCH: begin  // CMD_AUDIO_SETPITCH IDaudio frequence duration IDcurve
          if ErrorOnParamCount(5, SAudioSetFreq) then exit(True);
          if ErrorOnAudioID(A[1], SAudioSetFreq) then exit(True);
          if ErrorOnPitch(A[2], SAudioSetFreq) then exit(True);
          if ErrorOnDuration(A[3], SAudioSetFreq) then exit(True);
          if ErrorOnCurve(A[4], SAudioSetFreq) then exit(True);
        end;

        TITLECMD_AUDIO_APPLYFX: begin // TITLECMD_AUDIO_APPLYFX  IDaudio  dry/wet  EffectCount
          if ErrorOnParamCount(4, SAudioConnectEffect) then exit(True);
          if ErrorOnAudioID(A[1], SAudioConnectEffect) then exit(True);
          if ErrorOnDryWet(A[2], SAudioConnectEffect) then exit(True);
          if ErrorOnAudioEffectCount(A[3], SAudioConnectEffect) then exit(True);
        end;

        CMD_AUDIO_FXPRESET: begin // CMD_AUDIO_FXPRESET  effectType  presetIndex
          if ErrorOnParamCount(3, SAudioConnectEffect) then exit(True);
          if ErrorOnAudioEffectType(A[1], SAudioConnectEffect) then exit(True);
          if ErrorOnAudioPreset(A[2], SAudioConnectEffect) then exit(True);
        end;

        CMD_AUDIO_REMOVEFX: begin // CMD_AUDIO_REMOVEFX  IDaudio
          if ErrorOnParamCount(2, SAudioDisconnectEffect) then exit(True);
          if ErrorOnAudioID(A[1], SAudioDisconnectEffect) then exit(True);
        end;

        CMD_AUDIO_CAPTURE_START: begin
          if ErrorOnParamCount(1, SAudioCaptureStart) then exit(True);
        end;

        CMD_AUDIO_CAPTURE_STOP: begin
          if ErrorOnParamCount(1, SAudioCaptureStop) then exit(True);
        end;

        CMD_AUDIO_CAPTURE_SETVOLUME: begin // CMD_AUDIO_CAPTURE_SETVOLUME volume duration IDcurve
          if ErrorOnParamCount(4, SAudioCaptureSetVolume) then exit(True);
          if ErrorOnVolume(A[1], SAudioCaptureSetVolume) then exit(True);
          if ErrorOnDuration(A[2], SAudioCaptureSetVolume) then exit(True);
          if ErrorOnCurve(A[3], SAudioCaptureSetVolume) then exit(True);
        end;

        CMD_AUDIO_CAPTURE_SETPAN: begin // CMD_AUDIO_CAPTURE_SETPAN  panning duration IDcurve
          if ErrorOnParamCount(4, SAudioCaptureSetPan) then exit(True);
          if ErrorOnPan(A[1], SAudioCaptureSetPan) then exit(True);
          if ErrorOnDuration(A[2], SAudioCaptureSetPan) then exit(True);
          if ErrorOnCurve(A[3], SAudioCaptureSetPan) then exit(True);
        end;

        TITLECMD_AUDIO_CAPTURE_APPLYFX: begin // TITLECMD_AUDIO_CAPTURE_APPLYFX  dry/wet  EffectCount
          if ErrorOnParamCount(3, SAudioCaptureConnectEffect) then exit(True);
          if ErrorOnDryWet(A[1], SAudioCaptureConnectEffect) then exit(True);
          if ErrorOnAudioEffectCount(A[2], SAudioCaptureConnectEffect) then exit(True);
        end;

        CMD_AUDIO_CAPTURE_FXPRESET: begin // CMD_AUDIO_CAPTURE_FXPRESET  effectType  presetIndex
          if ErrorOnParamCount(3, SAudioCaptureConnectEffect) then exit(True);
          if ErrorOnAudioEffectType(A[1], SAudioCaptureConnectEffect) then exit(True);
          if ErrorOnAudioPreset(A[2], SAudioCaptureConnectEffect) then exit(True);
        end;

        CMD_AUDIO_CAPTURE_REMOVEFX: begin // CMD_AUDIO_CAPTURE_REMOVEFX
          if ErrorOnParamCount(1, SAudioCaptureDisconnectEffect) then exit(True);
        end;

        CMD_WAIT: begin  // CMD_WAIT DurationF
          if ErrorOnParamCount(2, SWait) then exit(True);
          if ErrorOnDuration(A[1], SWait) then exit(True);
        end;
        CMD_LOOP: begin  // CMD_LOOP
          if ErrorOnParamCount(1, SWait) then exit(True);
        end;
        CMD_STARTSEQUENCE: begin  // CMD_STARTSEQUENCE IDSeq
          if ErrorOnParamCount(2, SStartSequence) then exit(True);
          if ErrorOnSequenceID(A[1], SStartSequence) then exit(True);
        end;
        CMD_STOPSEQUENCE: begin  // CMD_STOPSEQUENCE IDSeq
          if ErrorOnParamCount(2, SStopSequence) then exit(True);
          if ErrorOnSequenceID(A[1], SStopSequence) then exit(True);
        end;
        CMD_SEQUENCESTRETCHTIME: begin  // CMD_SEQUENCESTRETCHTIME IDSeq StretchValueF DurationF CurveID
          if ErrorOnParamCount(5, SStretchTime) then exit(True);
          if ErrorOnSequenceID(A[1], SStretchTime) then exit(True);
          if ErrorOnSingleRange(A[2], 0.25, 7.0, SStretchTime, SBadStretchTimeValue) then exit(True);
          if ErrorOnDuration(A[3], SStretchTime) then exit(True);
          if ErrorOnCurve(A[4], SStretchTime) then exit(True);
        end;

        CMD_DMX_DIMMER: begin  // CMD_DMX_DIMMER IDuniverse IDFixture ChanIndex PercentF DurationF CurveID
          if ErrorOnParamCount(7, SDMXDimmer) then exit(True);
          if ErrorOnDmxAdress(A[1], A[2], A[3], SDMXDimmer) then exit(True);
          if ErrorOnSingleRange(A[4], 0.0, 1.0, SDMXDimmer, SBadPercentValue) then exit(True);
          if ErrorOnDuration(A[5], SDMXDimmer) then exit(True);
          if ErrorOnCurve(A[6], SDMXDimmer) then exit(True);
        end;

        TITLECMD_DMX_DIMMER: begin  // TITLECMD_DMX_DIMMER Duration CurveID
          if ErrorOnParamCount(3, SDMXDimmer) then exit(True);
          if ErrorOnDuration(A[1], SDMXDimmer) then exit(True);
          if ErrorOnCurve(A[2], SDMXDimmer) then exit(True);
        end;

        CMD_DMX_FLAME: begin  // CMD_DMX_FLAME IDuniverse IDFixture ChanIndex LevelMin LevelMax Speed Soften
          if ErrorOnParamCount(8, SDMXFlame) then exit(True);
          if ErrorOnDmxAdress(A[1], A[2], A[3], SDMXFlame) then exit(True);
          if ErrorOnSingleRange(A[4], 0.0, 1.0, SDMXFlame, SBadMinPercent) then exit(True);
          if ErrorOnSingleRange(A[5], 0.0, 1.0, SDMXFlame, SBadMaxPercent) then exit(True);
          if ErrorOnSingleRange(A[6], 0.05, 2.0, SDMXFlame, SBadWaitTime) then exit(True);
          if ErrorOnSingleRange(A[7], 0.0, 1.0, SDMXFlame, SBadSoften) then exit(True);
        end;

        TITLECMD_DMX_FLAME: begin  // TITLECMD_DMX_FLAME  LevelMinF LevelMaxF WaitTimeF SoftenF
          if ErrorOnParamCount(5, SDMXFlame) then exit(True);
          if ErrorOnSingleRange(A[1], 0.0, 1.0, SDMXFlame, SBadMinPercent) then exit(True);
          if ErrorOnSingleRange(A[2], 0.0, 1.0, SDMXFlame, SBadMaxPercent) then exit(True);
          if ErrorOnSingleRange(A[3], 0.05, 2.0, SDMXFlame, SBadWaitTime) then exit(True);
          if ErrorOnSingleRange(A[4], 0.0, 1.0, SDMXFlame, SBadSoften) then exit(True);
        end;

        CMD_DMX_STOPEFFECT: begin  // CMD_DMX_STOPEFFECT IDuniverse IDFixture ChanIndex
          if ErrorOnParamCount(4, SDMXStopEffect) then exit(True);
          if ErrorOnDmxAdress(A[1], A[2], A[3], SDMXStopEffect) then exit(True);
        end;

        TITLECMD_DMX_STOPEFFECT: begin
          if ErrorOnParamCount(1, SDMXStopEffect) then exit(True);
        end;

        CMD_DMX_DIMMERRGB: begin  // CMD_DMX_DIMMERRGB IDuniverse IDFixture Color Duration CurveID
          if ErrorOnParamCount(6, SDMXDimmerRGB) then exit(True);
          if ErrorOnDmxAdress(A[1], A[2], SDMXDimmerRGB) then exit(True);
          if ErrorOnColor(A[3], SDMXDimmerRGB) then exit(True);
          if ErrorOnDuration(A[4], SDMXDimmerRGB) then exit(True);
          if ErrorOnCurve(A[5], SDMXDimmerRGB) then exit(True);
        end;

        TITLECMD_DMX_DIMMERRGB: begin  // TITLECMD_DMX_DIMMERRGB Color Duration CurveID
          if ErrorOnParamCount(4, SDMXDimmerRGB) then exit(True);
          if ErrorOnColor(A[1], SDMXDimmerRGB) then exit(True);
          if ErrorOnDuration(A[2], SDMXDimmerRGB) then exit(True);
          if ErrorOnCurve(A[3], SDMXDimmerRGB) then exit(True);
        end;

        CMD_DMX_FLAMERGB: begin // CMD_DMX_FLAMERGB IDuniverse IDFixture Color Speed Amplitude Soften
          if ErrorOnParamCount(7, SDMXFlameRGB) then exit(True);
          if ErrorOnDmxAdress(A[1], A[2], SDMXFlameRGB) then exit(True);
          if ErrorOnColor(A[3], SDMXFlameRGB) then exit(True);
          if ErrorOnSingleRange(A[4], 0.05, 2.0, SDMXFlameRGB, SBadWaitTime) then exit(True);
          if ErrorOnSingleRange(A[5], 0.0, 1.0, SDMXFlameRGB, SBadAmplitude) then exit(True);
          if ErrorOnSingleRange(A[6], 0.0, 1.0, SDMXFlameRGB, SBadSoften) then exit(True);
        end;

        TITLECMD_DMX_FLAMERGB: begin // TITLECMD_DMX_FLAMERGB Color Speed Amplitude Soften
          if ErrorOnParamCount(5, SDMXFlameRGB) then exit(True);
          if ErrorOnColor(A[1], SDMXFlameRGB) then exit(True);
          if ErrorOnSingleRange(A[2], 0.05, 2.0, SDMXFlameRGB, SBadWaitTime) then exit(True);
          if ErrorOnSingleRange(A[3], 0.0, 1.0, SDMXFlameRGB, SBadAmplitude) then exit(True);
          if ErrorOnSingleRange(A[4], 0.0, 1.0, SDMXFlameRGB, SBadSoften) then exit(True);
        end;

        CMD_DMX_AUDIOFOLLOWERRGB: begin // CMD_DMX_AUDIOFOLLOWERRGB IDuniverse IDFixture IDaudio Color Gain SoftenTime
          if ErrorOnParamCount(7, SDMXAudioFollowerRGB) then exit(True);
          if ErrorOnDmxAdress(A[1], A[2], SDMXAudioFollowerRGB) then exit(True);
          if ErrorOnAudioID(A[3], SDMXAudioFollowerRGB) then exit(True);
          if ErrorOnColor(A[4], SDMXAudioFollowerRGB) then exit(True);
          if ErrorOnSingleRange(A[5], -1.0, 3.0, SDMXAudioFollowerRGB, SBadGain) then exit(True);
          if ErrorOnSingleRange(A[6], 0.1, 1.0, SDMXAudioFollowerRGB, SBadSoften) then exit(True);
        end;

        TITLECMD_DMX_AUDIOFOLLOWERRGB: begin // TITLECMD_DMX_AUDIOFOLLOWERRGB IDaudio Color Gain SoftenTime
          if ErrorOnParamCount(5, SDMXAudioFollowerRGB) then exit(True);
          if ErrorOnAudioID(A[1], SDMXAudioFollowerRGB) then exit(True);
          if ErrorOnColor(A[2], SDMXAudioFollowerRGB) then exit(True);
          if ErrorOnSingleRange(A[3], -1.0, 3.0, SDMXAudioFollowerRGB, SBadGain) then exit(True);
          if ErrorOnSingleRange(A[4], 0.1, 1.0, SDMXAudioFollowerRGB, SBadSoften) then exit(True);
        end;

        CMD_DMX_AUDIOFOLLOWER: begin // CMD_DMX_AUDIOFOLLOWER IDuniverse IDFixture ChanIndex IDaudio Gain MaxPercent SoftenTime
          if ErrorOnParamCount(8, SDMXAudioFollower) then exit(True);
          if ErrorOnDmxAdress(A[1], A[2], A[3], SDMXAudioFollower) then exit(True);
          if ErrorOnAudioID(A[4], SDMXAudioFollower) then exit(True);
          if ErrorOnSingleRange(A[5], -1.0, 3.0, SDMXAudioFollower, SBadGain) then exit(True);
          if ErrorOnSingleRange(A[6], 0.0, 1.0, SDMXAudioFollower, SBadBrightness) then exit(True);
          if ErrorOnSingleRange(A[7], 0.1, 1.0, SDMXAudioFollower, SBadSoften) then exit(True);
        end;

        TITLECMD_DMX_AUDIOFOLLOWER: begin // TITLECMD_DMX_AUDIOFOLLOWER IDaudio Gain MaxPercent SoftenTime
          if ErrorOnParamCount(5, SDMXAudioFollower) then exit(True);
          if ErrorOnAudioID(A[1], SDMXAudioFollower) then exit(True);
          if ErrorOnSingleRange(A[2], -1.0, 3.0, SDMXAudioFollower, SBadGain) then exit(True);
          if ErrorOnSingleRange(A[3], 0.0, 1.0, SDMXAudioFollower, SBadBrightness) then exit(True);
          if ErrorOnSingleRange(A[4], 0.1, 1.0, SDMXAudioFollower, SBadSoften) then exit(True);
        end;

        CMD_DMX_FLASH: begin // CMD_DMX_FLASH IDuniverse IDFixture ChanIndex LevelMin LevelMax DurationMin DurationMax
          if ErrorOnParamCount(8, SDMXFlash) then exit(True);
          if ErrorOnDmxAdress(A[1], A[2], A[3], SDMXFlash) then exit(True);
          if ErrorOnSingleRange(A[4], 0.0, 1.0, SDMXFlash, SBadLevelMin) then exit(True);
          if ErrorOnSingleRange(A[5], 0.0, 1.0, SDMXFlash, SBadLevelMax) then exit(True);
          if ErrorOnSingleRange(A[6], 0.0, 1.0, SDMXFlash, SBadDurationMin) then exit(True);
          if ErrorOnSingleRange(A[7], 0.0, 1.0, SDMXFlash, SBadDurationMax) then exit(True);
        end;

        TITLECMD_DMX_FLASH: begin // TITLECMD_DMX_FLASH LevelMin LevelMax DurationMin DurationMax
          if ErrorOnParamCount(5, SDMXFlash) then exit(True);
          if ErrorOnSingleRange(A[1], 0.0, 1.0, SDMXFlash, SBadLevelMin) then exit(True);
          if ErrorOnSingleRange(A[2], 0.0, 1.0, SDMXFlash, SBadLevelMax) then exit(True);
          if ErrorOnSingleRange(A[3], 0.1, 1000.0, SDMXFlash, SBadDurationMin) then exit(True);
          if ErrorOnSingleRange(A[4], 0.1, 1000.0, SDMXFlash, SBadDurationMax) then exit(True);
        end;

        CMD_DMX_STOPEFFECTRGB: begin // CMD_DMX_STOPEFFECTRGB IDuniverse IDFixture
          if ErrorOnParamCount(3, SDMXStopEffectRGB) then exit(True);
          if ErrorOnDmxAdress(A[1], A[2], SDMXStopEffectRGB) then exit(True);
        end;

        TITLECMD_DMX_STOPEFFECTRGB: begin
          if ErrorOnParamCount(1, SDMXStopEffectRGB) then exit(True);
        end;

        CMD_DMX_COPYCHANNEL: begin // CMD_DMX_COPYCHANNEL SourceIDuniverse SourceIDFixture SourceChanIndex
                                   //                     TargetIDUniverse TargetIDFixture TargetChanIndex
          if ErrorOnParamCount(7, SDMXCopy) then exit(True);
          if ErrorOnDmxAdress(A[1], A[2], A[3], SDMXCopy) then exit(True);
          if ErrorOnDmxAdress(A[4], A[5], A[6], SDMXCopy) then exit(True);
        end;

        TITLECMD_DMX_COPYCHANNEL: begin // TITLECMD_DMX_COPYCHANNEL SourceIDuniverse SourceIDFixture SourceChanIndex
          if ErrorOnParamCount(4, SDMXCopy) then exit(True);
          if ErrorOnDmxAdress(A[1], A[2], A[3], SDMXCopy) then exit(True);
        end;

        TITLECMD_DMX_COPYRGB: begin // TITLECMD_DMX_COPYRGB SourceIDuniverse SourceIDFixture
          if ErrorOnParamCount(3, SDMXCopyRGB) then exit(True);
          if ErrorOnDmxAdress(A[1], A[2], SDMXCopyRGB) then exit(True);
        end;

        CMD_DMX_COPYRGB: begin // CMD_DMX_COPYRGB SourceIDuniverse SourceIDFixture TargetIDUniverse TargetIDFixture
          if ErrorOnParamCount(5, SDMXCopyRGB) then exit(True);
          if ErrorOnDmxAdress(A[1], A[2], SDMXCopyRGB) then exit(True);
          if ErrorOnDmxAdress(A[3], A[4], SDMXCopyRGB) then exit(True);
        end;

        TITLECMD_DMX_FLASHRGB: begin // TITLECMD_DMX_FLASHRGB Color pcMin pcMax DurationMin DurationMax
          if ErrorOnParamCount(6, SDMXFlashRGB) then exit(True);
          if ErrorOnColor(A[1], SDMXFlashRGB) then exit(True);
          if ErrorOnSingleRange(A[2], 0.0, 1.0, SDMXFlashRGB, SBadLevelMin) then exit(True);
          if ErrorOnSingleRange(A[3], 0.0, 1.0, SDMXFlashRGB, SBadLevelMax) then exit(True);
          if ErrorOnSingleRange(A[4], 0.1, 1000.0, SDMXFlashRGB, SBadDurationMin) then exit(True);
          if ErrorOnSingleRange(A[5], 0.1, 1000.0, SDMXFlashRGB, SBadDurationMax) then exit(True);
        end;

        CMD_DMX_FLASHRGB: begin // CMD_DMX_FLASHRGB IDuniverse IDFixture Color pcMin pcMax DurationMin DurationMax
          if ErrorOnParamCount(8, SDMXFlashRGB) then exit(True);
          if ErrorOnDmxAdress(A[1], A[2], SDMXFlashRGB) then exit(True);
          if ErrorOnColor(A[3], SDMXFlashRGB) then exit(True);
          if ErrorOnSingleRange(A[2], 0.0, 1.0, SDMXFlashRGB, SBadLevelMin) then exit(True);
          if ErrorOnSingleRange(A[3], 0.0, 1.0, SDMXFlashRGB, SBadLevelMax) then exit(True);
          if ErrorOnSingleRange(A[4], 0.1, 1000.0, SDMXFlashRGB, SBadDurationMin) then exit(True);
          if ErrorOnSingleRange(A[5], 0.1, 1000.0, SDMXFlashRGB, SBadDurationMax) then exit(True);
        end;

        else begin
          errMess := SUnrecognizedAction+' ('+cmd.ToString+')';
          exit(True);
        end;
      end;// case cmd
    end;
  end;

begin
  if SequencerInfoList = '' then begin
    Log.Warning('Sequence "'+Name+'" is empty', 1);
    exit(True);
  end;

  cmds := SequencerInfoList.SequencerInfoListToCmdListOfSingleCmd;
  CmdArray := cmds.SplitToCmdArray;
  cmds := '';
  Result := False;
  for i:=0 to High(CmdArray) do begin
    if ErrorOnCmd(CmdArray[i], mess) then begin
      if not HaveError then begin
        HaveError := True;
        ErrorMessage := mess;
      end;
      Log.Warning('Sequence "'+Name+' error detected: '+mess);
      Result := True;
    end;
  end;

  CmdArray := NIL;
end;

{ TTopList }

procedure TTopList.ClearAll;
begin
  Clear;
  FID:=0;
end;

function TTopList.NextID: cardinal;
begin
  inc(FID);
  Result:=FID;
end;

function TTopList.GetTopByIndex(aIndex: integer): TSequence;
begin
  if (aindex < 0) or (aindex >= Count)
    then Result := NIL
    else Result := Items[aindex];
end;

function TTopList.NameAlreadyExists(const aName: string): boolean;
var i: integer;
begin
 for i:=0 to Count-1 do
  if Items[i].Name = aName then
  begin
    Result := TRUE;
    exit;
  end;

 Result := FALSE;
end;

function TTopList.IsValidIndex(aIndex: integer): boolean;
begin
 Result := (aIndex>=0) and (aIndex<Count) and (Count>0);
end;

function TTopList.AddTop(const aName: string; const aSequencerInfoList: TSequencerInfoList): TSequence;
begin
  Result := TSequence.Create;
  Result.Name := aName;
  Result.SequencerInfoList := aSequencerInfoList;
  Result.ID := NextID;
  Add(Result);
end;

function TTopList.InsertTop(aIndex: integer; const aName: string;
  const aSequencerInfoList: TSequencerInfoList): TSequence;
begin
  Result := TSequence.Create;
  Result.Name := aName;
  Result.SequencerInfoList := aSequencerInfoList;
  Result.ID := NextID;
  Insert(aIndex, Result);
end;

function TTopList.GetTopByID(aID: cardinal): TSequence;
var i: Integer;
begin
  for i:=0 to Count-1 do
   if GetTopByIndex(i).ID = aID then begin
           Result := GetTopByIndex(i);
           exit;
   end;
  Result := NIL;
end;

function TTopList.GetSequenceByStrID(const aStrID: string): TSequence;
var i: integer;
begin
  if TryStrToInt(aStrID, i) then Result := GetTopByID(i)
    else Result := NIL;
end;

function TTopList.GetNameByID(aID: cardinal): string;
var i: integer;
begin
 for i:=0 to Count-1 do
  if GetTopByIndex(i).ID = aID then begin
    Result := GetTopByIndex(i).Name;
    exit;
  end;

 Result := SUnknowSequence;
end;

function TTopList.IDToIndex(aID: cardinal): integer;
var i: integer;
begin
  for i:=0 to Count-1 do
   if GetTopByIndex(i).ID = aID then begin
     Result := i;
     exit;
   end;
  Result := -1;
end;

const SEQUENCE_HEADER = '[SEQUENCE]';
procedure TTopList.Save(temp: TStrings);
var i: Integer;
  prop: TPackProperty;
begin
  if Count = 0 then
    exit;

  prop.Init('|');
  prop.Add('CurrentID', integer(FID));
  prop.Add('Count', Count);
  temp.Add(SEQUENCE_HEADER);
  temp.Add(prop.PackedProperty);

  for i:=0 to Count-1 do
  begin
    prop.Init('#');
    prop.Add('Name', Items[i].Name);
    prop.Add('ID', integer(Items[i].ID));
    prop.Add('Data', Items[i].SequencerInfoList);
    temp.Add(prop.PackedProperty);
  end;


{
 temp.Add('[TOP]');
 temp.Add( FID.ToString );   // current ID value
 temp.Add( Count.ToString ); // count

 for i:=0 to Count-1 do
 begin
   temp.Add(Items[i].Name);             // name
   temp.Add(Items[i].ID.ToString);      // ID
   temp.Add(Items[i].SequencerInfoList);  // TSequencerInfoList
 end; }
end;

procedure TTopList.Load(temp: TStrings);
var c, k, vi: integer;
  o: TSequence;
  prop: TSplitProperty;
  s1, s2: string;
  flagError: boolean;
  procedure LogMissingProperty(const apropName: string);
  begin
    Log.Error('TTopList.Load - Property '+apropName+' not found', 3);
    flagError := True;
  end;
begin
  Log.Info('Loading sequences', 2);
  c := 0; // avoid hint
  vi := 0;
  s1 := '';
  s2 := '';
  ClearAll;
  k := temp.IndexOf(SEQUENCE_HEADER);
  if (k = -1) or (k = temp.Count-1) then
  begin
    Log.Info('no sequence found', 3);
    exit;
  end;
  prop.Split(temp.Strings[k+1], '|');
  if not prop.IntegerValueOf('CurrentID', vi, 0) then
    LogMissingProperty('CurrentID')
  else
    FID := vi;
  if not prop.IntegerValueOf('Count', c, 0) then
    LogMissingProperty('Count');

  if flagError then exit;

  inc(k, 2);
  while (c > 0) and (k < temp.Count) do
  begin
    prop.Split(temp.Strings[k], '#');
    if not prop.StringValueOf('Name', s1, '') then
      LogMissingProperty('Name');
    if not prop.IntegerValueOf('ID', vi, 0) then
      LogMissingProperty('ID');
    if not prop.StringValueOf('Data', s2, '') then
      LogMissingProperty('Data');

    if not flagError then
    begin
      o := TSequence.Create;
      o.Name := s1;
      o.ID := vi;
      o.SequencerInfoList := s2;
      Add(o);
    end;
    inc(k);
    dec(c);
  end;
end;

function TTopList.CheckErrorInSequences: boolean;
var i: integer;
  seq: TSequence;
begin
  Result := False;
  for i:=0 to Count-1 do begin
    seq := GetTopByIndex(i);
    if seq.CheckError(Self) then Result := True;
  end;
end;

procedure TTopList.StopAll;
var i: integer;
begin
  for i:=0 to Count-1 do
    GetTopByIndex(i).Stop;
end;

function TTopList.Duplicate(aID: cardinal): TSequence;
var source: TSequence;
  i :integer;
  txt: string;
begin
  Result := NIL;
  source := GetTopByID(aID);
  if source = NIL then exit;

  Result := source.Duplicate;
  Result.ID := NextID;
  Add(Result);

  i := 0;
  repeat
    inc(i);
    txt := ChangeFileExt(Result.Name,'')+'.'+SCopy_+i.ToString;
  until not NameAlreadyExists(txt);
  Result.Name := txt;
end;

end.

