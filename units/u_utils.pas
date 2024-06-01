unit u_utils;

{$mode objfpc}{$H+}
{$modeswitch TypeHelpers}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, ComCtrls, Graphics, LCLType, syncobjs, System.UITypes,
  StdCtrls, Controls,
  BGRABitmap, BGRABitmapTypes,
  u_common,
  frame_bglvirtualscreen_sequencer,
  frame_sequencer,
  ALSound, u_audio_manager;


// construct a TSequencerInfoList
function ConstructTSequencerInfoList(const aCmds: TCmdList;
                                     const aShortReadable: string;
                                     aCmdDuration: single): TSequencerInfoList;

function FormatFloatWithDot(const aFmt: string; aValue: single): string;
// convert the string to single. The string can have '.' or ','
function StringToSingle(aStr: string): single;

function CmdWait(aDuration: single): TSingleCmd;
function CmdLoop: TSingleCmd;
function CmdStartTop( aTopID: cardinal ): TSingleCmd;
function CmdStopTop( aTopID: cardinal ): TSingleCmd;
function CmdTopStretchTime(aTopID: cardinal; aStretchValue, aDuration: single; aCurveID: word): TSingleCmd;

function CmdTitleAudioPlay: TSingleCmd;
function CmdAudioPlay( aID: TSoundID; aVolume, aPan: single ):TSingleCmd;
function CmdTitleAudioStop: TSingleCmd;
function CmdAudioStop( aID: TSoundID ):TSingleCmd;
function CmdTitleAudioPause: TSingleCmd;
function CmdAudioPause( aID: TSoundID ):TSingleCmd;
function CmdTitleAudioFadeIN: TSingleCmd;
function CmdAudioFadeIN( aID: TSoundID; aVolume, aDuration: single; aCurveID: word ): TSingleCmd;
function CmdTitleAudioFadeOUT: TSingleCmd;
function CmdAudioFadeOUT( aID: TSoundID; aDuration: single; aCurveID: word ): TSingleCmd;
function CmdTitleAudioChangeVolume:TSingleCmd;
function CmdAudioChangeVolume( aID: TSoundID; aNewVolume, aDuration: single; aCurveID: word): TSingleCmd;
function CmdTitleAudioChangePan: TSingleCmd;
function CmdAudioChangePan( aID: TSoundID; aNewPan, aDuration: single; aCurveID: word): TSingleCmd;
function CmdTitleAudioChangePitch: TSingleCmd;
function CmdAudioChangePitch( aID: TSoundID; aNewPitch, aDuration: single; aCurveID: word): TSingleCmd;

function CmdTitleAudioApplyFX(aID: TSoundID; aDryWet: single; aCount: cardinal): TSingleCmd;
function CmdAudioFXPreset(aEffectType: TALSEffectType; aPresetIndex: integer): TSingleCmd;
function CmdAudioRemoveFX(aID: TSoundID): TSingleCmd;

function CmdAudioCaptureStart: TSingleCmd;
function CmdAudioCaptureStop: TSingleCmd;
function CmdAudioCaptureChangeVolume(aNewVolume, aDuration: single; aCurveID: word): TSingleCmd;
function CmdAudioCaptureChangePan(aNewPan, aDuration: single; aCurveID: word): TSingleCmd;
function CmdTitleAudioCaptureApplyFX(aDryWet: single; aCount: cardinal): TSingleCmd;
function CmdAudioCaptureFXPreset(aEffectType: TALSEffectType; aPresetIndex: integer): TSingleCmd;
function CmdAudioCaptureRemoveFX: TSingleCmd;


function CmdDMXInternalWave(aIDUni, aIDFix: cardinal; aChanIndex: integer;
                                 aPercent1, aDuration1: single; aCurveID1: word;
                                 aKeepTime: single;
                                 aPercent2, aDuration2: single; aCurveID2: word): TSingleCmd;

function CmdTitleDMXDimmer(aDuration: single; aCurveID: word): TSingleCmd;
function CmdDMXDimmer(aIDUni, aIDFix: cardinal; aChanIndex: integer; aPercent, aDuration: single; aCurveID: word): TSingleCmd;

function CmdTitleDMXFlame(aLevelMin, aLevelMax, aWaitTime, aSoften: single): TSingleCmd;
function CmdDMXFlame(aIDUni, aIDFix: cardinal; aChanIndex: integer;
                     aLevelMin, aLevelMax, aWaitTime, aSoften: single): TSingleCmd;

function CmdTitleDMXAudioFollower(aIDAudio: TSoundID; aGain, aMaxPercent, aSoftenTime: single): TSingleCmd;
function CmdDMXAudioFollower(aUni, aIDFix: cardinal; aChanIndex: integer; aIDAudio: TSoundID; aGain, aMaxPercent, aSoftenTime: single): TSingleCmd;

function CmdTitleDMXStopEffect: TSingleCmd;
function CmdDMXStopEffect(aUni, aIDFix: cardinal; aChanIndex: integer): TSingleCmd;

function CmdTitleDMXCopy(aSourceIDUni, aSourceIDFix: cardinal; aSourceChanIndex: integer): TSingleCmd;
function CmdDMXCopy(aSourceIDUni, aSourceIDFix: cardinal; aSourceChanIndex: integer;
                    aTargetIDUni, aTargetIDFix: cardinal; aTargetChanIndex: integer): TSingleCmd;

function CmdTitleDMXFlash(aLevelMin, aLevelMax, aDurationMin, aDurationMax: single ): TSingleCmd;
function CmdDMXFlash(aIDUni, aIDFix: cardinal; aChanIndex: integer;
                     aLevelMin, aLevelMax, aDurationMin, aDurationMax: single):TSingleCmd;

function CmdTitleDMXDimmerRGB(aColor: TColor; aDuration: single; aCurveID: word): TSingleCmd;
function CmdDMXDimmerRGB(aUni, aIDFix: cardinal; aColor: TColor; aDuration: single; aCurveID: word): TSingleCmd;

function CmdTitleDMXStopEffectRGB: TSingleCmd;
function CmdDMXStopEffectRGB(aUni, aIDFix: cardinal): TSingleCmd;

function CmdTitleDMXAudioFollowerRGB(aIDAudio: TSoundID; aColor: TColor; aGain, aSoftenTime: single): TSingleCmd;
function CmdDMXAudioFollowerRGB(aUni, aIDFix: cardinal; aIDAudio: TSoundID; aColor: TColor; aGain, aSoftenTime: single): TSingleCmd;

function CmdTitleDMXFlameRGB(aColor: TColor; aSpeed, aAmplitude, aSoften: single): TSingleCmd;
function CmdDMXFlameRGB(aIDUni, aIDFix: cardinal;
                        aColor: TColor; aSpeed, aAmplitude, aSoften: single): TSingleCmd;

function CmdTitleDMXFlashRGB(aColor: TColor; apcMin, apcMax, aDurationMin, aDurationMax: single): TSingleCmd;
function CmdDMXFlashRGB(aIDUni, aIDFix: cardinal; aColor: TColor; apcMin, apcMax, aDurationMin, aDurationMax: single): TSingleCmd;

function CmdTitleDMXCopyRGB(aIDSourceUni, aIDSourceFix: cardinal): TSingleCmd;
function CmdDMXCopyRGB(aIDSourceUni, aIDSourceFix, aIDTargetUni, aIDTargetFix: cardinal): TSingleCmd;


// Return the color:  c + c*percent   with percent]-1..1[
function PercentColor( c: Tcolor; percent: single ): Tcolor;
// if c is brightness, return c - c*percent
// if c is darkness, return c + c*percent
function PercentColorRelative(c: TColor; Absolutepercent: single): TColor;
// return the sum r+g+b
function SumRGB( c: TColor ): integer;

function BGRAPixelToString( p: TBGRAPixel ): string;
function StringToBGRAPixel( const s: string ): TBGRAPixel;


// sauve le contenu d'un sequenceur dans un TString. Pas d'entête.
procedure SequencerToTStrings( aSequencer: TFrameBGLSequencer; aTemp: TStrings );
// recharge un séquenceur à partir d'un TStrings.
// aStartIndex est l'index de la première donnée dans le TStrings
procedure TStringsToSequencer( aTemp: TStrings; aStartIndex: integer; aSequencer: TFrameBGLSequencer );




// returns FALSE if the string have at least one characters not allowed for filename usage
// see const FORBIDENCHARS in u_common
function StringIsValid(const s: string):boolean;

// no space or FORBIDENCHARS for filename
function FileNameIsValid(const aFileName: string): boolean;


function StringIsValid(const s: string; const ForbidenChars: string): boolean;

function ReplaceForbidenCharByUnderscore(const s: string; const aForbidenChars: string): string;

// aVolume[0..1] -> x.x%
function VolumeToStringPercent(const aVolume: single): string;
// aPan[-1..1] =>    100% Left   CENTER   100% Right
function PanToStringPercent(const aPan: single): string;
// aFreq  => xHz
//function FreqToString(const aFreq: integer): string;
// aPitch [0.1 to 4.0]        0.1(lower)   NORMAL    4.0(higher)
function PitchToString(const aPitch: single): string;
// aDryWet is [0..1] range
// return a string like 20%/80%
function DryWetToStringPercent(const aDryWet: single): string;
// => EAXReverb  preset  DEFAULT
function AudioFXToString(aEffectType: TALSEffectType; aPresetIndex: integer): string;
function AudioFXName(aEffectType: TALSEffectType): string;
function AudioFXPresetName(aEffectType: TALSEffectType; aPresetIndex: integer): string;

// return a string: slower/normal/faster x.xx
function StrechTimeToString(const aValue: single): string;

function PercentToDMXByte(const aPercent: single): byte;
function DMXByteToPercent(aValue: byte): single;
function DMXPercentToString(aPercent: single): string;

function DurationToString(d: single): string;


function GetLogoImage(aWidth, aHeight: integer): TBGRABitmap;
function MsgDlgTypeToBGRABitmap(aMsgType: TMsgDlgType; aWidth, aHeight: integer): TBGRABitmap;

procedure DoRedFlashOnEdit(aEdit: TEdit);
procedure DoRedFlashOnWinControl(aControl: TWinControl);

type
  { TTimedThread }

  TTimedThread=class(TThread)
  private
    FPeriodMS: integer;
    FTargetHandle: HWND;
    FMessage,
    FLParam,
    FWParam: cardinal;
  private
    FMode: integer;
    FProcToCall: TThreadMethod;
  private
    FEvent: TEvent;
    procedure CreateEvent;
    procedure FreeEvent;
  protected
    procedure Execute; override;
  public
    // the thread post a message to a TForm every period of time=> not work well under Linux
    // if aPeriod=0 there is no pause
    constructor Create(aPeriodMs: integer; aTargetHandle: HWND; aMessage, aWParam, aLParam: cardinal; aStart: boolean);

    // the thread use synchronize to interact with other things
    // if aPeriod=0 there is no pause
    constructor CreateSynchronize(aPeriodMs: integer; aProcSynchronize: TThreadMethod; aStart: boolean);

    // the thread use  callback
    // if aPeriod=0 there is no pause
    constructor CreateCallback(aPeriodMs: integer; aProcToCall: TThreadMethod; aStart: boolean);

    constructor CreateQueue(aPeriodMs: integer; aProcToQueue: TThreadMethod; aStart: boolean);
    destructor Destroy; override;
  end;


  { TAverage }

  TAverage=class
  private
    FValues: array of single;
    FHead,
    FTail: integer;
    FSum: single;
    function GetCount: integer;
    procedure SetCount(AValue: integer);
  public
    procedure Clear;
    procedure Push(AValue: single);
    function Average: single;
    property Count: integer read GetCount write SetCount;
  end;


{ TSplitProperty }
// helper to split string and retrieve individual property and their values

TSplitProperty = record
 private
  A: TStringArray;
  function ValueIndexOf(const aName: string; out index: integer): boolean;
 public
  procedure SetEmpty;
  // Split and return the number of 'blocks' found.
  function Split(const s: string; aSeparator: Char): integer;
  // Return True if the property is defined
  function PropertyIsPresent(const aPropertyName: string): boolean;
  // Return true if all names property in the passed array are presents.
  function CheckProperties(aPropertyNames: TStringArray): boolean;
  // Return True if aPropertyName is found and set its value to Value
  function StringValueOf(const aPropertyName: string;
                         var Value: string;
                         const aDefault: string): boolean;
  function BooleanValueOf(const aPropertyName: string;
                          var Value: boolean;
                          aDefault: boolean): boolean;
  function IntegerValueOf(const aPropertyName: string;
                          var Value: integer;
                          aDefault: integer): boolean;
  function SingleValueOf(const aPropertyName: string;
                         var Value: single;
                         aDefault: single): boolean;
end;

{ TPackProperty }

TPackProperty = record
 private
  FPackedProperty: string;
  FSeparator: char;
 procedure ConcatProperty(const n, v: string);
 public
  procedure Init(aSeparator: char);
  procedure Add(const aPropertyName, aValue: string);
  procedure Add(const aPropertyName: string; aValue: boolean);
  procedure Add(const aPropertyName: string; aValue: integer);
  procedure Add(const aPropertyName: string; aValue: single);
  property PackedProperty: string read FPackedProperty;
end;



implementation
uses VelocityCurve, u_resource_string, u_apputils, LCLIntf, dateutils, Math,
  utilitaire_bgrabitmap, Forms;


procedure SequencerToTStrings(aSequencer: TFrameBGLSequencer; aTemp: TStrings);
var i: Integer;
begin
 aTemp.Add( aSequencer.ID.ToString );    // current ID value
 aTemp.Add(aSequencer.GroupValue.ToString ); // current group value
 with aSequencer.StepList do begin
    aTemp.Add( Count.ToString );  // count
    for i:=0 to Count-1 do
     aTemp.Add( TSequenceStep(Items[i]).Serialize );  // steps
 end;
end;

procedure TStringsToSequencer(aTemp: TStrings; aStartIndex: integer; aSequencer: TFrameBGLSequencer);
var c: integer;
  step: TSequenceStep;
begin
 aSequencer.Clear;
 if aStartIndex<>-1 then begin
   aSequencer.ID := aTemp.Strings[aStartIndex].ToInteger;
   inc(aStartIndex);
   aSequencer.GroupValue := aTemp.Strings[aStartIndex].ToInteger;
   inc(aStartIndex);
   c := aTemp.Strings[aStartIndex].ToInteger;
   while c>0 do begin
        inc(aStartIndex);
        step := TSequenceStep.Create;
        step.Deserialize( aTemp.Strings[aStartIndex] );
        aSequencer.RawAdd( step, FALSE );
        dec(c);
   end;
 end;
end;

function StringIsValid(const s: string): boolean;
begin
  Result:= StringIsValid(s, FORBIDENCHARS);
end;

function FileNameIsValid(const aFileName: string): boolean;
begin
 Result:= StringIsValid(aFileName, FILENAMEFORBIDENCHARS);
end;

function StringIsValid(const s: string; const ForbidenChars: string): boolean;
var i: Integer;
begin
  Result:=TRUE;
  for i:=1 to Length(s) do
   if Pos(s[i], ForbidenChars)>0 then begin
     Result:=FALSE;
     exit;
   end;
end;

function ReplaceForbidenCharByUnderscore(const s: string; const aForbidenChars: string): string;
var i: integer;
begin
  Result := s;
  for i:=1 to Length(aForbidenChars) do
    Result.Replace(aForbidenChars[i], '_', [rfReplaceAll]);
end;

function VolumeToStringPercent(const aVolume: single): string;
begin
  Result:=FormatFloatWithDot('0.0', aVolume*100 )+'%';
end;

function PanToStringPercent(const aPan: single): string;
begin
 if aPan<0
   then Result:=Trunc(Abs(aPan*100)).ToString+'% '+SLeft
   else if aPan = 0
         then Result:=SCenter
         else Result:=Trunc(aPan*100).ToString+'% '+SRight;
end;

{function FreqToString(const aFreq: integer): string;
begin
  Result:=aFreq.ToString+SHz;
end; }

function PitchToString(const aPitch: single): string;
begin
 if aPitch = 1.0 then
   Result := SNormal
 else if aPitch > 1.0 then
   Result := FormatFloatWithDot('0.00', aPitch)+'('+SFaster+')'
 else
   Result := FormatFloatWithDot('0.00', aPitch)+'('+SSlower+')';
end;

function DryWetToStringPercent(const aDryWet: single): string;
var dry: single;
begin
  dry := 1.0 - aDryWet;
  Result := FormatFloat('0.0', dry*100)+'%/'+FormatFloat('0.0', aDryWet*100)+'%';
end;

function AudioFXToString(aEffectType: TALSEffectType; aPresetIndex: integer): string;
var A: TStringArray;
begin
  Result := AudioFXName(aEffectType);
  A := NIL;
  case aEffectType of
    AL_EFFECT_CHORUS: A := SoundManager.ChorusPresetList;
    AL_EFFECT_DISTORTION: A := SoundManager.DistortionPresetList;
    AL_EFFECT_ECHO: A := SoundManager.EchoPresetList;
    AL_EFFECT_FLANGER: A := SoundManager.FlangerPresetList;
    AL_EFFECT_FREQUENCYSHIFTER: A := SoundManager.FreqShifterPresetList;
    AL_EFFECT_VOCALMORPHER: A := SoundManager.VocalMorpherPresetList;
    AL_EFFECT_PITCHSHIFTER: A := SoundManager.PitchShifterPresetList;
    AL_EFFECT_RINGMODULATOR: A := SoundManager.RingModulatorPresetList;
    AL_EFFECT_AUTOWAH: A := SoundManager.AutoWahPresetList;
    AL_EFFECT_COMPRESSOR: ;
    AL_EFFECT_EQUALIZER: A := SoundManager.EqualizerPresetList;
    AL_EFFECT_EAXREVERB: A := SoundManager.EAXReverbPresetList;
  end;//case

  if A<>NIL then begin
    if aPresetIndex > High(A) then
      Result := Result +'.'+SUnknownPreset
    else
      Result := Result +'.'+A[aPresetIndex];
  end;
end;

function AudioFXName(aEffectType: TALSEffectType): string;
begin
 case aEffectType of
   AL_EFFECT_CHORUS: Result := NameOfAudioFXName[1];
   AL_EFFECT_DISTORTION: Result := NameOfAudioFXName[4];
   AL_EFFECT_ECHO: Result := NameOfAudioFXName[5];
   AL_EFFECT_FLANGER: Result := NameOfAudioFXName[2];
   AL_EFFECT_FREQUENCYSHIFTER: Result := NameOfAudioFXName[7];
   AL_EFFECT_VOCALMORPHER: Result := NameOfAudioFXName[10];
   AL_EFFECT_PITCHSHIFTER: Result := NameOfAudioFXName[8];
   AL_EFFECT_RINGMODULATOR: Result := NameOfAudioFXName[9];
   AL_EFFECT_AUTOWAH: Result := NameOfAudioFXName[0];
   AL_EFFECT_COMPRESSOR: Result := NameOfAudioFXName[3];
   AL_EFFECT_EQUALIZER: Result := NameOfAudioFXName[6];
   AL_EFFECT_EAXREVERB: Result := NameOfAudioFXName[11];
  else Result := SUnknownEffect;
 end;//case
end;

function AudioFXPresetName(aEffectType: TALSEffectType; aPresetIndex: integer): string;
var A: TStringArray;
begin
  A := NIL;
  Result := '';

  case aEffectType of
    AL_EFFECT_CHORUS: A := SoundManager.ChorusPresetList;
    AL_EFFECT_DISTORTION: A := SoundManager.DistortionPresetList;
    AL_EFFECT_ECHO: A := SoundManager.EchoPresetList;
    AL_EFFECT_FLANGER: A := SoundManager.FlangerPresetList;
    AL_EFFECT_FREQUENCYSHIFTER: A := SoundManager.FreqShifterPresetList;
    AL_EFFECT_VOCALMORPHER: A := SoundManager.VocalMorpherPresetList;
    AL_EFFECT_PITCHSHIFTER: A := SoundManager.PitchShifterPresetList;
    AL_EFFECT_RINGMODULATOR: A := SoundManager.RingModulatorPresetList;
    AL_EFFECT_AUTOWAH: A := SoundManager.AutoWahPresetList;
    AL_EFFECT_COMPRESSOR: exit;
    AL_EFFECT_EQUALIZER: A := SoundManager.EqualizerPresetList;
    AL_EFFECT_EAXREVERB: A := SoundManager.EAXReverbPresetList;
  end;//case

  if A<>NIL then
  begin
    if (aPresetIndex > High(A)) or (aPresetIndex < 0) then
      Result := SUnknownPreset
    else
      Result := A[aPresetIndex];
  end;
end;

function StrechTimeToString(const aValue: single): string;
begin
  if aValue < 1 then
    Result := SSlower+' '+FormatFloat('0.00', aValue)
  else
  if aValue = 1 then
    Result := SNormal
  else
    Result := SFaster+' '+FormatFloat('0.00', aValue);
end;

function PercentToDMXByte(const aPercent: single): byte;
begin
  Result:=byte(Round(aPercent*255));
end;

function DMXByteToPercent(aValue: byte): single;
begin
  Result:=aValue/255;
end;

function DMXPercentToString(aPercent: single): string;
begin
  Result:=FormatFloatWithDot('0.0', aPercent*100)+'%';
end;

function DurationToString(d: single): string;
begin
  Result := FormatFloat('0.00', d)+' '+SSec;
end;

function GetLogoImage(aWidth, aHeight: integer): TBGRABitmap;
begin
 try
   Result := SVGFileToBGRABitmap(GetAppImagesFolder+'logo.svg', aWidth, aHeight);
 except
   Result := TBGRABitmap.Create(aWidth, aHeight, BGRAPixelTransparent);
 end;
end;

function MsgDlgTypeToBGRABitmap(aMsgType: TMsgDlgType; aWidth, aHeight: integer): TBGRABitmap;
begin
  if aWidth < aHeight then aHeight := -1
    else if aHeight < aWidth then aWidth := -1;

  try
    case aMsgType of
      mtWarning: Result := SVGFileToBGRABitmap(GetAppIconImagesFolder+'DlgWarning.svg', aWidth, aHeight);
      mtError: Result := SVGFileToBGRABitmap(GetAppIconImagesFolder+'DlgError.svg', aWidth, aHeight);
      mtInformation: Result := SVGFileToBGRABitmap(GetAppIconImagesFolder+'DlgInformation.svg', aWidth, aHeight);
      mtConfirmation: Result := SVGFileToBGRABitmap(GetAppIconImagesFolder+'DlgQuestion.svg', aWidth, aHeight);
      else Result := TBGRABitmap.Create(aWidth, aHeight, BGRAPixelTransparent);
    end;
  except
    Result := TBGRABitmap.Create(aWidth, aHeight, BGRAPixelTransparent);
  end;
end;

procedure DoRedFlashOnEdit(aEdit: TEdit);
var c: TColor;
begin
  c := aEdit.Color;
  aEdit.Color := clRed;
  Application.ProcessMessages;
  Sleep(200);
  aEdit.Color := c;
  Application.ProcessMessages;
end;

procedure DoRedFlashOnWinControl(aControl: TWinControl);
var c: TColor;
begin
  c := aControl.Color;
  aControl.Color := clRed;
  Application.ProcessMessages;
  Sleep(200);
  aControl.Color := c;
  Application.ProcessMessages;
end;

function ConstructTSequencerInfoList(const aCmds: TCmdList;
  const aShortReadable: string; aCmdDuration: single): TSequencerInfoList;
var step: TSequenceStep;
begin
  Result:='1'+SEQUENCERINFO_SEPARATOR+ // ID
          '0'+SEQUENCERINFO_SEPARATOR; // group
  step:=TSequenceStep.Create;
  step.CmdList:=aCmds;
  step.Caption:=aShortReadable;
  step.Duration:=aCmdDuration;
  step.TimePos:=0;
  step.ID:=1;
  step.Top:=0;
  step.Group:=0;
  step.Width:=100;
  Result:=Result+step.Serialize;
  step.Free;
end;

function FormatFloatWithDot(const aFmt: string; aValue: single): string;
var i: integer;
begin
  Result := FormatFloat(aFmt, aValue);
  i := Pos(',', Result);
  if i > 0 then
    Result[i] := '.';
end;

function StringToSingle(aStr: string): single;
var i: integer;
  fs: TFormatSettings;
begin
  try
    fs.DecimalSeparator := '.';
    Result := StrToFloat(aStr, fs);
  except
    fs.DecimalSeparator := ',';
    Result := StrToFloat(aStr, fs);
  end;
end;

function CmdWait(aDuration: single): TSingleCmd;
begin
  Result:=CMD_WAIT.ToString+PARAM_SEPARATOR+
          FormatFloatWithDot('0.000', aDuration);
end;

function CmdLoop: TSingleCmd;
begin
  Result:=CMD_LOOP.ToString;
end;

function CmdStartTop(aTopID: cardinal): TSingleCmd;
begin
  Result:=CMD_STARTSEQUENCE.ToString+PARAM_SEPARATOR+
          aTopID.ToString;
end;

function CmdStopTop(aTopID: cardinal): TSingleCmd;
begin
 Result:=CMD_STOPSEQUENCE.ToString+PARAM_SEPARATOR+
         aTopID.ToString;
end;

function CmdTopStretchTime(aTopID: cardinal; aStretchValue, aDuration: single;
  aCurveID: word): TSingleCmd;
begin
  Result:=CMD_SEQUENCESTRETCHTIME.ToString+PARAM_SEPARATOR+
          aTopID.ToString+PARAM_SEPARATOR+
          FormatFloatWithDot('0.000', aStretchValue)+PARAM_SEPARATOR+
          FormatFloatWithDot('0.000', aDuration)+PARAM_SEPARATOR+
          aCurveID.ToString;
end;

function CmdTitleAudioPlay: TSingleCmd;
begin
  Result:=TITLECMD_AUDIO_PLAY.ToString;
end;

function CmdAudioPlay(aID: TSoundID; aVolume, aPan: single): TSingleCmd;
begin
  Result:=CMD_AUDIO_PLAY.ToString+PARAM_SEPARATOR+
          aID.ToString+PARAM_SEPARATOR+
          FormatFloatWithDot('0.000', aVolume)+PARAM_SEPARATOR+
          FormatFloatWithDot('0.000', aPan);
end;

function CmdTitleAudioStop: TSingleCmd;
begin
  Result:=TITLECMD_AUDIO_STOP.ToString;
end;

function CmdAudioStop(aID: TSoundID): TSingleCmd;
begin
  Result:=CMD_AUDIO_STOP.ToString+PARAM_SEPARATOR+
          aID.ToString;
end;

function CmdTitleAudioPause: TSingleCmd;
begin
 Result:=TITLECMD_AUDIO_PAUSE.ToString;
end;

function CmdAudioPause(aID: TSoundID): TSingleCmd;
begin
 Result:=CMD_AUDIO_PAUSE.ToString+PARAM_SEPARATOR+
         aID.ToString;
end;

function CmdTitleAudioFadeIN: TSingleCmd;
begin
  Result:=TITLECMD_AUDIO_FADEIN.ToString;
end;

function CmdAudioFadeIN(aID: TSoundID; aVolume, aDuration: single; aCurveID: word): TSingleCmd;
begin
  Result:=CMD_AUDIO_FADEIN.ToString+PARAM_SEPARATOR+
          aID.ToString+PARAM_SEPARATOR+
          FormatFloatWithDot('0.000', aVolume)+PARAM_SEPARATOR+
          FormatFloatWithDot('0.000', aDuration)+PARAM_SEPARATOR+
          aCurveID.ToString;
end;

function CmdTitleAudioFadeOUT: TSingleCmd;
begin
 Result:=TITLECMD_AUDIO_FADEOUT.ToString;
end;

function CmdAudioFadeOUT(aID: TSoundID; aDuration: single; aCurveID: word): TSingleCmd;
begin
  Result:=CMD_AUDIO_FADEOUT.ToString+PARAM_SEPARATOR+
          aID.ToString+PARAM_SEPARATOR+
          FormatFloatWithDot('0.000', aDuration)+PARAM_SEPARATOR+
          aCurveID.ToString;
end;

function CmdTitleAudioChangeVolume: TSingleCmd;
begin
  Result:=TITLECMD_AUDIO_SETVOLUME.ToString;
end;

function CmdAudioChangeVolume(aID: TSoundID; aNewVolume, aDuration: single; aCurveID: word): TSingleCmd;
begin
  Result:=CMD_AUDIO_SETVOLUME.ToString+PARAM_SEPARATOR+
          aID.ToString+PARAM_SEPARATOR+
          FormatFloatWithDot('0.000', aNewVolume)+PARAM_SEPARATOR+
          FormatFloatWithDot('0.000', aDuration)+PARAM_SEPARATOR+
          aCurveID.ToString;
end;

function CmdTitleAudioChangePan: TSingleCmd;
begin
 Result:=TITLECMD_AUDIO_SETPAN.ToString;
end;

function CmdAudioChangePan(aID: TSoundID; aNewPan, aDuration: single; aCurveID: word ): TSingleCmd;
begin
 Result:=CMD_AUDIO_SETPAN.ToString+PARAM_SEPARATOR+
         aID.ToString+PARAM_SEPARATOR+
         FormatFloatWithDot('0.000', aNewPan)+PARAM_SEPARATOR+
         FormatFloatWithDot('0.000', aDuration)+PARAM_SEPARATOR+
         aCurveID.ToString;
end;

function CmdTitleAudioChangePitch: TSingleCmd;
begin
 Result:=TITLECMD_AUDIO_SETPITCH.ToString;
end;

function CmdAudioChangePitch(aID: TSoundID; aNewPitch, aDuration: single; aCurveID: word): TSingleCmd;
begin
 Result:=CMD_AUDIO_SETPITCH.ToString+PARAM_SEPARATOR+
         aID.ToString+PARAM_SEPARATOR+
         FormatFloatWithDot('0.0000', aNewPitch)+PARAM_SEPARATOR+
         FormatFloatWithDot('0.000', aDuration)+PARAM_SEPARATOR+
         aCurveID.ToString;
end;

function CmdTitleAudioApplyFX(aID: TSoundID; aDryWet: single; aCount: cardinal): TSingleCmd;
begin
  Result := TITLECMD_AUDIO_APPLYFX.ToString+PARAM_SEPARATOR+
            aID.ToString+PARAM_SEPARATOR+
            FormatFloatWithDot('0.000', aDryWet)+PARAM_SEPARATOR+
            aCount.ToString;
end;

function CmdAudioFXPreset(aEffectType: TALSEffectType; aPresetIndex: integer): TSingleCmd;
begin
  Result := CMD_AUDIO_FXPRESET.ToString+PARAM_SEPARATOR+
            integer(aEffectType).ToString+PARAM_SEPARATOR+
            aPresetIndex.ToString;
end;

function CmdAudioRemoveFX(aID: TSoundID): TSingleCmd;
begin
  Result := CMD_AUDIO_REMOVEFX.ToString+PARAM_SEPARATOR+
            aID.ToString;
end;

function CmdAudioCaptureStart: TSingleCmd;
begin
  Result := CMD_AUDIO_CAPTURE_START.ToString;
end;

function CmdAudioCaptureStop: TSingleCmd;
begin
   Result := CMD_AUDIO_CAPTURE_STOP.ToString;
end;

function CmdAudioCaptureChangeVolume(aNewVolume, aDuration: single; aCurveID: word): TSingleCmd;
begin
  Result := CMD_AUDIO_CAPTURE_SETVOLUME.ToString+PARAM_SEPARATOR+
            FormatFloatWithDot('0.000', aNewVolume)+PARAM_SEPARATOR+
            FormatFloatWithDot('0.000', aDuration)+PARAM_SEPARATOR+
            aCurveID.ToString;
end;

function CmdAudioCaptureChangePan(aNewPan, aDuration: single; aCurveID: word): TSingleCmd;
begin
 Result:=CMD_AUDIO_CAPTURE_SETPAN.ToString+PARAM_SEPARATOR+
         FormatFloatWithDot('0.000', aNewPan)+PARAM_SEPARATOR+
         FormatFloatWithDot('0.000', aDuration)+PARAM_SEPARATOR+
         aCurveID.ToString;
end;

function CmdTitleAudioCaptureApplyFX(aDryWet: single; aCount: cardinal): TSingleCmd;
begin
 Result := TITLECMD_AUDIO_CAPTURE_APPLYFX.ToString+PARAM_SEPARATOR+
           FormatFloatWithDot('0.000', aDryWet)+PARAM_SEPARATOR+
           aCount.ToString;
end;

function CmdAudioCaptureFXPreset(aEffectType: TALSEffectType;
  aPresetIndex: integer): TSingleCmd;
begin
 Result := CMD_AUDIO_CAPTURE_FXPRESET.ToString+PARAM_SEPARATOR+
           integer(aEffectType).ToString+PARAM_SEPARATOR+
           aPresetIndex.ToString;
end;

function CmdAudioCaptureRemoveFX: TSingleCmd;
begin
  Result := CMD_AUDIO_CAPTURE_REMOVEFX.ToString;
end;

function CmdDMXInternalWave(aIDUni, aIDFix: cardinal; aChanIndex: integer;
  aPercent1, aDuration1: single; aCurveID1: word; aKeepTime: single; aPercent2,
  aDuration2: single; aCurveID2: word): TSingleCmd;
begin
  Result:=CMD_INTERNALDMXWAVE.ToString+PARAM_SEPARATOR+
          aIDUni.ToString+PARAM_SEPARATOR+
          aIDFix.ToString+PARAM_SEPARATOR+
          aChanIndex.ToString+PARAM_SEPARATOR+
          FormatFloatWithDot('0.000', aPercent1)+PARAM_SEPARATOR+
          FormatFloatWithDot('0.000', aDuration1)+PARAM_SEPARATOR+
          aCurveID1.ToString+PARAM_SEPARATOR+
          FormatFloatWithDot('0.000', aKeepTime)+PARAM_SEPARATOR+
          FormatFloatWithDot('0.000', aPercent2)+PARAM_SEPARATOR+
          FormatFloatWithDot('0.000', aDuration2)+PARAM_SEPARATOR+
          aCurveID2.ToString;
end;

function CmdTitleDMXDimmer(aDuration: single; aCurveID: word): TSingleCmd;
begin
  Result:=TITLECMD_DMX_DIMMER.ToString+PARAM_SEPARATOR+
          FormatFloatWithDot('0.000', aDuration)+PARAM_SEPARATOR+
          aCurveID.ToString;
end;

function CmdDMXDimmer(aIDUni, aIDFix: cardinal; aChanIndex: integer;
  aPercent, aDuration: single; aCurveID: word): TSingleCmd;
begin
 Result := CMD_DMX_DIMMER.ToString+PARAM_SEPARATOR+
           aIDUni.ToString+PARAM_SEPARATOR+
           aIDFix.ToString+PARAM_SEPARATOR+
           aChanIndex.ToString+PARAM_SEPARATOR+
           FormatFloatWithDot('0.000', aPercent)+PARAM_SEPARATOR+
           FormatFloatWithDot('0.000', aDuration)+PARAM_SEPARATOR+
           aCurveID.ToString;
end;

function CmdTitleDMXFlame(aLevelMin, aLevelMax, aWaitTime, aSoften: single): TSingleCmd;
begin
  Result:=TITLECMD_DMX_FLAME.ToString+PARAM_SEPARATOR+
          FormatFloatWithDot('0.000', aLevelMin)+PARAM_SEPARATOR+
          FormatFloatWithDot('0.000', aLevelMax)+PARAM_SEPARATOR+
          FormatFloatWithDot('0.000', aWaitTime)+PARAM_SEPARATOR+
          FormatFloatWithDot('0.000', aSoften);
end;

function CmdDMXFlame(aIDUni, aIDFix: cardinal; aChanIndex: integer;
  aLevelMin, aLevelMax, aWaitTime, aSoften: single): TSingleCmd;
begin
 Result:=CMD_DMX_FLAME.ToString+PARAM_SEPARATOR+
         aIDUni.ToString+PARAM_SEPARATOR+
         aIDFix.ToString+PARAM_SEPARATOR+
         aChanIndex.ToString+PARAM_SEPARATOR+
         FormatFloatWithDot('0.000', aLevelMin)+PARAM_SEPARATOR+
         FormatFloatWithDot('0.000', aLevelMax)+PARAM_SEPARATOR+
         FormatFloatWithDot('0.000', aWaitTime)+PARAM_SEPARATOR+
         FormatFloatWithDot('0.000', aSoften);
end;

function CmdTitleDMXAudioFollower(aIDAudio: TSoundID; aGain, aMaxPercent,
  aSoftenTime: single): TSingleCmd;
begin
  Result:=TITLECMD_DMX_AUDIOFOLLOWER.ToString+PARAM_SEPARATOR+
          aIDAudio.ToString+PARAM_SEPARATOR+
          FormatFloatWithDot('0.000', aGain)+PARAM_SEPARATOR+
          FormatFloatWithDot('0.000', aMaxPercent)+PARAM_SEPARATOR+
          FormatFloatWithDot('0.000', aSoftenTime);
end;

function CmdDMXAudioFollower(aUni, aIDFix: cardinal; aChanIndex: integer;
  aIDAudio: TSoundID; aGain, aMaxPercent, aSoftenTime: single): TSingleCmd;
begin
  Result:=CMD_DMX_AUDIOFOLLOWER.ToString+PARAM_SEPARATOR+
          aUni.ToString+PARAM_SEPARATOR+
          aIDFix.ToString+PARAM_SEPARATOR+
          aChanIndex.ToString+PARAM_SEPARATOR+
          aIDAudio.ToString+PARAM_SEPARATOR+
          FormatFloatWithDot('0.000', aGain)+PARAM_SEPARATOR+
          FormatFloatWithDot('0.000', aMaxPercent)+PARAM_SEPARATOR+
          FormatFloatWithDot('0.000', aSoftenTime);
end;

function CmdTitleDMXStopEffect: TSingleCmd;
begin
  Result:=TITLECMD_DMX_STOPEFFECT.ToString;
end;

function CmdDMXStopEffect(aUni, aIDFix: cardinal; aChanIndex: integer): TSingleCmd;
begin
  Result:=CMD_DMX_STOPEFFECT.ToString+PARAM_SEPARATOR+
          aUni.ToString+PARAM_SEPARATOR+
          aIDFix.ToString+PARAM_SEPARATOR+
          aChanIndex.ToString;
end;

function CmdTitleDMXFlash(aLevelMin, aLevelMax, aDurationMin, aDurationMax: single ): TSingleCmd;
begin
  Result:=TITLECMD_DMX_FLASH.ToString+PARAM_SEPARATOR+
          FormatFloatWithDot('0.000', aLevelMin)+PARAM_SEPARATOR+
          FormatFloatWithDot('0.000', aLevelMax)+PARAM_SEPARATOR+
          FormatFloatWithDot('0.000', aDurationMin)+PARAM_SEPARATOR+
          FormatFloatWithDot('0.000', aDurationMax);
end;

function CmdDMXFlash(aIDUni, aIDFix: cardinal; aChanIndex: integer;
                     aLevelMin, aLevelMax, aDurationMin, aDurationMax: single): TSingleCmd;
begin
  Result:=CMD_DMX_FLASH.ToString+PARAM_SEPARATOR+
          aIDUni.ToString+PARAM_SEPARATOR+
          aIDFix.ToString+PARAM_SEPARATOR+
          aChanIndex.ToString+PARAM_SEPARATOR+
          FormatFloatWithDot('0.000', aLevelMin)+PARAM_SEPARATOR+
          FormatFloatWithDot('0.000', aLevelMax)+PARAM_SEPARATOR+
          FormatFloatWithDot('0.000', aDurationMin)+PARAM_SEPARATOR+
          FormatFloatWithDot('0.000', aDurationMax);
end;

function CmdTitleDMXCopy(aSourceIDUni, aSourceIDFix: cardinal; aSourceChanIndex: integer): TSingleCmd;
begin
  Result:=TITLECMD_DMX_COPYCHANNEL.ToString+PARAM_SEPARATOR+
          aSourceIDUni.ToString+PARAM_SEPARATOR+
          aSourceIDFix.ToString+PARAM_SEPARATOR+
          aSourceChanIndex.ToString;
end;

function CmdDMXCopy(aSourceIDUni, aSourceIDFix: cardinal; aSourceChanIndex: integer;
                    aTargetIDUni, aTargetIDFix: cardinal; aTargetChanIndex: integer): TSingleCmd;
begin
 Result:=CMD_DMX_COPYCHANNEL.ToString+PARAM_SEPARATOR+
         aSourceIDUni.ToString+PARAM_SEPARATOR+
         aSourceIDFix.ToString+PARAM_SEPARATOR+
         aSourceChanIndex.ToString+PARAM_SEPARATOR+
         aTargetIDUni.ToString+PARAM_SEPARATOR+
         aTargetIDFix.ToString+PARAM_SEPARATOR+
         aTargetChanIndex.ToString;
end;

function CmdTitleDMXDimmerRGB(aColor: TColor; aDuration: single; aCurveID: word): string;
begin
 Result := TITLECMD_DMX_DIMMERRGB.ToString+PARAM_SEPARATOR+
           integer(aColor).ToString+PARAM_SEPARATOR+
           FormatFloatWithDot('0.000', aDuration)+PARAM_SEPARATOR+
           aCurveID.ToString;
end;

function CmdDMXDimmerRGB(aUni, aIDFix: cardinal; aColor: TColor;
                    aDuration: single; aCurveID: word): string;
begin
 Result := CMD_DMX_DIMMERRGB.ToString+PARAM_SEPARATOR+
           aUni.ToString+PARAM_SEPARATOR+
           aIDFix.ToString+PARAM_SEPARATOR+
           integer(aColor).ToString+PARAM_SEPARATOR+
           FormatFloatWithDot('0.000', aDuration)+PARAM_SEPARATOR+
           aCurveID.ToString;
end;

function CmdTitleDMXStopEffectRGB: TSingleCmd;
begin
  Result:=TITLECMD_DMX_STOPEFFECTRGB.ToString;
end;

function CmdDMXStopEffectRGB(aUni, aIDFix: cardinal): TSingleCmd;
begin
  Result:=CMD_DMX_STOPEFFECTRGB.ToString+PARAM_SEPARATOR+
          aUni.ToString+PARAM_SEPARATOR+
          aIDFix.ToString;
end;

function CmdTitleDMXAudioFollowerRGB(aIDAudio: TSoundID; aColor: TColor; aGain, aSoftenTime: single): TSingleCmd;
begin
  Result:=TITLECMD_DMX_AUDIOFOLLOWERRGB.ToString+PARAM_SEPARATOR+
          aIDAudio.ToString+PARAM_SEPARATOR+
          integer(aColor).ToString+PARAM_SEPARATOR+
          FormatFloatWithDot('0.000', aGain)+PARAM_SEPARATOR+
          FormatFloatWithDot('0.000', aSoftenTime);
end;

function CmdDMXAudioFollowerRGB(aUni, aIDFix: cardinal; aIDAudio: TSoundID; aColor: TColor; aGain, aSoftenTime: single): TSingleCmd;
begin
  Result:=CMD_DMX_AUDIOFOLLOWERRGB.ToString+PARAM_SEPARATOR+
          aUni.ToString+PARAM_SEPARATOR+
          aIDFix.ToString+PARAM_SEPARATOR+
          aIDAudio.ToString+PARAM_SEPARATOR+
          integer(aColor).ToString+PARAM_SEPARATOR+
          FormatFloatWithDot('0.000', aGain)+PARAM_SEPARATOR+
          FormatFloatWithDot('0.000', aSoftenTime);
end;

function CmdTitleDMXFlameRGB(aColor: TColor; aSpeed, aAmplitude,
  aSoften: single): TSingleCmd;
begin
  Result:=TITLECMD_DMX_FLAMERGB.ToString+PARAM_SEPARATOR+
          integer(aColor).ToString+PARAM_SEPARATOR+
          FormatFloatWithDot('0.000', aSpeed)+PARAM_SEPARATOR+
          FormatFloatWithDot('0.000', aAmplitude)+PARAM_SEPARATOR+
          FormatFloatWithDot('0.000', aSoften);
end;

function CmdDMXFlameRGB(aIDUni, aIDFix: cardinal;
  aColor: TColor; aSpeed, aAmplitude, aSoften: single): TSingleCmd;
begin
 Result:=CMD_DMX_FLAMERGB.ToString+PARAM_SEPARATOR+
         aIDUni.ToString+PARAM_SEPARATOR+
         aIDFix.ToString+PARAM_SEPARATOR+
         integer(aColor).ToString+PARAM_SEPARATOR+
         FormatFloatWithDot('0.000', aSpeed)+PARAM_SEPARATOR+
         FormatFloatWithDot('0.000', aAmplitude)+PARAM_SEPARATOR+
         FormatFloatWithDot('0.000', aSoften);
end;

function CmdTitleDMXFlashRGB(aColor: TColor; apcMin, apcMax, aDurationMin, aDurationMax: single): TSingleCmd;
begin
  Result := TITLECMD_DMX_FLASHRGB.ToString+PARAM_SEPARATOR+
            IntToStr(aColor)+PARAM_SEPARATOR+
            FormatFloatWithDot('0.000', apcMin)+PARAM_SEPARATOR+
            FormatFloatWithDot('0.000', apcMax)+PARAM_SEPARATOR+
            FormatFloatWithDot('0.000', aDurationMin)+PARAM_SEPARATOR+
            FormatFloatWithDot('0.000', aDurationMax);
end;

function CmdDMXFlashRGB(aIDUni, aIDFix: cardinal; aColor: TColor;
  apcMin, apcMax, aDurationMin, aDurationMax: single): TSingleCmd;
begin
  Result := CMD_DMX_FLASHRGB.ToString+PARAM_SEPARATOR+
           aIDUni.ToString+PARAM_SEPARATOR+
           aIDFix.ToString+PARAM_SEPARATOR+
           IntToStr(aColor)+PARAM_SEPARATOR+
           FormatFloatWithDot('0.000', apcMin)+PARAM_SEPARATOR+
           FormatFloatWithDot('0.000', apcMax)+PARAM_SEPARATOR+
           FormatFloatWithDot('0.000', aDurationMin)+PARAM_SEPARATOR+
           FormatFloatWithDot('0.000', aDurationMax);
end;

function CmdTitleDMXCopyRGB(aIDSourceUni, aIDSourceFix: cardinal): TSingleCmd;
begin
  Result:=TITLECMD_DMX_COPYRGB.ToString+PARAM_SEPARATOR+
          aIDSourceUni.ToString+PARAM_SEPARATOR+
          aIDSourceFix.ToString;
end;

function CmdDMXCopyRGB(aIDSourceUni, aIDSourceFix, aIDTargetUni,
  aIDTargetFix: cardinal): TSingleCmd;
begin
 Result:=CMD_DMX_COPYRGB.ToString+PARAM_SEPARATOR+
         aIDSourceUni.ToString+PARAM_SEPARATOR+
         aIDSourceFix.ToString+PARAM_SEPARATOR+
         aIDTargetUni.ToString+PARAM_SEPARATOR+
         aIDTargetFix.ToString;
end;

function PercentColor(c: Tcolor; percent: single): Tcolor;
var r, g, b: integer;
begin
  r := Red(c);
  g := Green(c);
  b := Blue(c);
  r := EnsureRange(Round(r + ( r * percent )), 0, 255);
  g := EnsureRange(Round(g + ( g * percent )), 0, 255);
  b := EnsureRange(Round(b + ( b * percent )), 0, 255);
  Result := RGBToColor(r, g, b);
end;

function PercentColorRelative(c: TColor; Absolutepercent: single): TColor;
var r, g, b: integer;
  s: single;
begin
  r := Red(c);
  g := Green(c);
  b := Blue(c);
  if (r+g+b)/(255*3) > 0.5 then
    s := -1
  else
    s := 1;
  r := EnsureRange(Round(r + r * Absolutepercent * s), 0, 255);
  g := EnsureRange(Round(g + g * Absolutepercent * s), 0, 255);
  b := EnsureRange(Round(b + b * Absolutepercent * s), 0, 255);
  Result := RGBToColor(r, g, b);
end;

function SumRGB(c: TColor): integer;
begin
 Result := Red(c)+Green(c)+Blue(c);
end;

function BGRAPixelToString(p: TBGRAPixel): string;
var v: DWord;
begin
  v:= (p.red<<24) or (p.green<<16) or (p.blue<<8) or p.alpha;
  Result:=v.ToString;
end;

function StringToBGRAPixel(const s: string): TBGRAPixel;
var v: LongInt;
begin
  if TryStrToInt(s, v)
    then Result:= BGRA((v and $FF000000)>>24,(v and $00FF0000)>>16,(v and $0000FF00)>>8,v and $FF)
    else Result:= BGRA(100,100,100);
end;

{ TPackProperty }

procedure TPackProperty.ConcatProperty(const n, v: string);
begin
  if Length(FPackedProperty) <> 0 then
    FPackedProperty := FPackedProperty + FSeparator;
  FPackedProperty := FPackedProperty + n + FSeparator + v;
end;

procedure TPackProperty.Init(aSeparator: char);
begin
  FPackedProperty := '';
  FSeparator := aSeparator;
end;

procedure TPackProperty.Add(const aPropertyName, aValue: string);
begin
  ConcatProperty(aPropertyName, AValue);
end;

procedure TPackProperty.Add(const aPropertyName: string; aValue: boolean);
begin
 ConcatProperty(aPropertyName, BoolToStr(aValue, 'true','false'));
end;

procedure TPackProperty.Add(const aPropertyName: string; aValue: integer);
begin
 ConcatProperty(aPropertyName, aValue.ToString);
end;

procedure TPackProperty.Add(const aPropertyName: string; aValue: single);
begin
 ConcatProperty(aPropertyName, FormatFloatWithDot('0.000', aValue));
end;


{ TSplitProperty }

function TSplitProperty.ValueIndexOf(const aName: string; out index: integer): boolean;
var i: integer;
begin
  i := 0;
  while i < High(A) do
  begin
     if A[i] = aName then
     begin
       index := i+1;
       Result := True;
       exit;
     end;
     inc(i, 2);
  end;
  Result := False;
end;

procedure TSplitProperty.SetEmpty;
begin
  A := NIL;
end;

function TSplitProperty.Split(const s: string; aSeparator: Char): integer;
begin
  A := s.Split([aSeparator]);
  Result := Length(A);
end;

function TSplitProperty.PropertyIsPresent(const aPropertyName: string): boolean;
var i: integer;
begin
  Result := ValueIndexOf(aPropertyName, i);
end;

function TSplitProperty.CheckProperties(aPropertyNames: TStringArray): boolean;
var i, j: integer;
begin
  for i:=0 to High(aPropertyNames) do
    if not ValueIndexOf(aPropertyNames[i], j) then
    begin
      Result := False;
      exit;
    end;
  Result := True;
end;

function TSplitProperty.StringValueOf(const aPropertyName: string; var Value: string; const aDefault: string): boolean;
var i: integer;
begin
  Result := ValueIndexOf(aPropertyName, i);
  if Result then
    Value := A[i]
  else
    Value := aDefault;
end;

function TSplitProperty.BooleanValueOf(const aPropertyName: string; var Value: boolean; aDefault: boolean): boolean;
var i: integer;
begin
  Result := ValueIndexOf(aPropertyName, i);
  if Result then
    Value := A[i] = 'true'
  else
  Value := aDefault;
end;

function TSplitProperty.IntegerValueOf(const aPropertyName: string;
  var Value: integer; aDefault: integer): boolean;
var i, v: integer;
begin
  Result := ValueIndexOf(aPropertyName, i);
  if Result then
    if TryStrToInt(A[i], v) then
      Value := v
    else
      Value := aDefault;
end;

function TSplitProperty.SingleValueOf(const aPropertyName: string;
  var Value: single; aDefault: single): boolean;
var i: integer;
  v: single;
begin
  Result := ValueIndexOf(aPropertyName, i);
  if Result then
    Value := StringToSingle(A[i])
  else
    Value := aDefault;
end;



{ TAverage }

function TAverage.GetCount: integer;
begin
  Result:=Length(FValues);
end;

procedure TAverage.SetCount(AValue: integer);
begin
  SetLength(FValues,AValue);
  Clear;
end;

procedure TAverage.Clear;
var i: integer;
begin
  for i:=0 to High(FValues) do
   FValues[i]:=0.0;
  FHead:=0;
  FTail:=0;
  FSum:=0;
end;

procedure TAverage.Push(AValue: single);
begin
  if Length(FValues)<2 then begin
    FSum:=AValue;
    exit;
  end;

  FSum:=FSum+AValue-FValues[FTail];

  inc(FHead);
  if FHead>=Count then FHead:=0;
  FValues[FHead]:=AValue;

  if FHead=FTail then begin
    inc(FTail);
    if FTail>=Count then FTail:=0;
  end;
end;

function TAverage.Average: single;
begin
 if Length(FValues)<2
   then Result:=FSum
   else Result:=FSum/Count;
end;

{ TTimedThread }

procedure TTimedThread.CreateEvent;
begin
  FEvent := TEvent.Create(NIL, True, False, '');
end;

procedure TTimedThread.FreeEvent;
begin
  FreeAndNil(FEvent);
end;

procedure TTimedThread.Execute;
var timeOrigin, timeNow: TDateTime;
    AccuMs, deltaMs: int64;
begin
  timeOrigin := Now();
  AccuMs := 0;

  while not Terminated do begin

    timeNow := Now;
    AccuMs := AccuMs + MilliSecondsBetween( timeNow, timeOrigin );

    if AccuMs >= FPeriodMS then
    begin
      AccuMs := AccuMs - FPeriodMS;
      timeOrigin := timeNow;
      case FMode of
        0: PostMessage(FTargetHandle, FMessage, FWParam, FLParam);
        1: Synchronize(FProcToCall);
        2: FProcToCall();
        3: Queue(FProcToCall);
      end;//case
    end
    else
    begin
      deltaMs := FPeriodMS-AccuMs;
      if deltaMs > 1 then
        FEvent.WaitFor( deltaMs shr 1 );
    end;
   end;
end;

{var TOrigin, TNow, DeltaT: QWord;
begin
  TOrigin:=GetTickCount64;
  while not Terminated do begin
    TNow:=GetTickCount64;
    DeltaT := TNow-TOrigin;
    if DeltaT>=FPeriodMS then begin
      TOrigin+=FPeriodMS;
      case FMode of
        0: PostMessage(FTargetHandle, FMessage, FWParam, FLParam);
        1: Synchronize(FProcSynchronize);
        2: FProcToCall();
      end;//case
    end else sleep(1);
   end;
end;   }

constructor TTimedThread.Create(aPeriodMs: integer; aTargetHandle: HWND; aMessage, aWParam, aLParam: cardinal; aStart: boolean);
begin
  inherited Create(TRUE);
 // Priority:=tpHighest;
  FPeriodMS:=aPeriodMs;
  FTargetHandle:=aTargetHandle;
  FMessage:=aMessage;
  FLParam:=aLParam;
  FWParam:=aWParam;
  FMode:=0;
  CreateEvent;
  if aStart then Start;
end;

constructor TTimedThread.CreateSynchronize(aPeriodMs: integer;
  aProcSynchronize: TThreadMethod; aStart: boolean);
begin
  inherited Create(TRUE);
//  Priority:=tpHighest;
  FPeriodMS:=aPeriodMs;
  FProcToCall:=aProcSynchronize;
  FMode:=1;
  CreateEvent;
  if aStart then Start;
end;

constructor TTimedThread.CreateCallback(aPeriodMs: integer;
  aProcToCall: TThreadMethod; aStart: boolean);
begin
  inherited Create(TRUE);
//  Priority:=tpHighest;
  FPeriodMS:=aPeriodMs;
  FProcToCall:=aProcToCall;
  FMode:=2;
  CreateEvent;
  if aStart then Start;
end;

constructor TTimedThread.CreateQueue(aPeriodMs: integer;
  aProcToQueue: TThreadMethod; aStart: boolean);
begin
  inherited Create(TRUE);
//  Priority:=tpHighest;
  FPeriodMS := aPeriodMs;
  FProcToCall := aProcToQueue;
  FMode := 3;
  CreateEvent;
  if aStart then Start;
end;

destructor TTimedThread.Destroy;
begin
  FreeEvent;
  inherited Destroy;
end;


end.

