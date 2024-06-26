unit u_audio_manager;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils,
  ALSound, u_audioutils;

const
    AUDIO_PARAM_SEPARATOR='|';

type

  TALSEffect = ALSound.TALSEffect;
  TALSSound = ALSound.TALSSound;
  TALSEffectType = ALSound.TALSEffectType;

  ArrayOfSound = array of TALSSound;
  ArrayOfEffect = array of TALSEffect;
  TSoundManager = class;

  TSoundID = integer;

  // we record all created sound's ID and effects

  { TSoundItem }

  TSoundItem = record
    Parent: TSoundManager;
    SoundID: TSoundID;
    Effects: TFPList;
    EffectNames: string;
    EffectNamesArray: TStringArray;//array of string;
    Peaks: TAudioFileLevel;
    // add and chain the effect
    procedure AddEffect(const aEffect: TALSEffect; const aEffectName, aPresetName: string);
    procedure SetDryWet(AValue: single);
    procedure DeleteEffects;
    procedure FreeEffectsList;
  end;
  PSoundItem = ^TSoundItem;


  { TSoundManager }

  TSoundManager=class
  private
    FPlaybackContext: TALSPlaybackContext;
    FCaptureContext: TALSCaptureContext;
    FCapturedSound: TALSPlaybackCapturedSound;
    FIDValue: TSoundID;
    function GetCount: integer;
    function GetPlayList: TALSPlaylist;
    procedure ResetIdValue;
    function GetNextIdValue: TSoundID;
  private
    FSoundItems: TFPList;
    procedure RegisterSoundToItemList(aSnd: TALSSound);
    procedure DeleteSoundFromItemList(aID: TSoundID);
    procedure DeleteAllSoundFromItemList(aDeleteAlsoCaptureSound: boolean);
    function GetItemByID(aID: TSoundID): PSoundItem;

    procedure AddEffectToSoundItem(aID: TSoundID; const aEffect: TALSEffect; const aEffectName, aPresetName: string);
    procedure DeleteAllEffectOnSoundItem(aID: TSoundID);
  private
    FAutoWahProp: TALSAutoWahProperties;
    FChorusProp: TALSChorusProperties;
    FFlangerProp: TALSFlangerProperties;
    FCompressorProp: TALSCompressorProperties;
    FDistortionProp: TALSDistortionProperties;
    FEchoProp: TALSEchoProperties;
    FEqualizerProp: TALSEqualizerProperties;
    FFreqShifterProp: TALSFreqShifterProperties;
    FPitchShifterProp: TALSPitchShifterProperties;
    FRingModulatorProp: TALSRingModulatorProperties;
    FVocalMorpherProp: TALSVocalMorpherProperties;
    FEAXReverbProp: TEAXReverbProperties;

    FEffectChain_SoundID: TSoundID;
    FEffectChain_DryWet: single;
    FEffectChain_EffectCount: integer;

  public
    constructor Create;
    destructor Destroy; override;

    // remove all effects and sounds, except effects and playback applyed on capture
    procedure Clear;
    procedure Save(aStringList: TStringList);
    procedure Load(aStringList: TStringList; aAbsolutPathToAdd: string);

    function AddStream(const aFilename: string): TALSSound;
    function AddSound(const aFilename: string): TALSSound;
    procedure DeleteSoundByID(aID: TSoundID);

    procedure StopAllSound(aStopAlsoCapturePlayback: boolean);
    procedure DeleteEffectsOnAllSounds(aDeleteAlsoCaptureEffect: boolean);
    procedure ResetPanOnAllSounds(aAlsoOnCapturePlayback: boolean);
    procedure ResetPitchOnAllSounds(aAlsoOnCapturePlayback: boolean);
    procedure DeleteAllSounds(aDeleteAlsoCaptureSound: boolean);

    function AutoWahPresetList: TStringArray;
    function ChorusPresetList: TStringArray;
    function FlangerPresetList: TStringArray;
    function CompressorPresetList: TStringArray;
    function DistortionPresetList: TStringArray;
    function EchoPresetList: TStringArray;
    function EqualizerPresetList: TStringArray;
    function FreqShifterPresetList: TStringArray;
    function PitchShifterPresetList: TStringArray;
    function RingModulatorPresetList: TStringArray;
    function VocalMorpherPresetList: TStringArray;
    function EAXReverbPresetList: TStringArray;

    function AddEffectOn(aID: TSoundID; aEffectType: TALSEffectType; aPresetIndex: integer): TALSEffect;
    procedure ConstructChainOn(aID: TSoundID);
    procedure SetDryWetOn(aID: TSoundID; aValue: single);
    procedure DeleteEffectsOn(aID: TSoundID);
    function GetEffectsNamesOn(aID: TSoundID): string;
    function GetEffectsNamesArrayOn(aID: TSoundID): TStringArray;

    procedure ToogleLoopModeOn(aID: TSoundID);


    // routines for sequence's execution
    procedure PrepareEffectChain(aID: TSoundID; aDryWet: single; aEffectCount: integer);
    procedure AddToEffectChain(aEffectType: TALSEffectType; aPresetIndex: integer);

    procedure StartCaptureToPlayback;
    procedure SetOnCaptureBufferEvent( aProc: TALSOnCaptureBuffer );
    procedure SetCapturePreAmp(AValue: single);
    function GetCapturePreAmp: single;
    procedure SetCaptureVolume(AValue: single);
    function GetCaptureVolume: single;
    procedure SetCapturePan(AValue: single);
    function GetCapturePan: single;
    function CaptureToPlaybackIsReady: boolean;
    function CaptureStrError: string;
    procedure StopCaptureToPlayback;
    procedure CaptureSetVolume(aValue: single);
    function CaptureGetVolume: single;
    procedure CaptureSetPan(aValue: single);

    // Stop all sounds, delete all effects, stop capture
    procedure ResetState;

    function IDToIndex(aID: TSoundID): integer;
    function IndexToID(aIndex: integer): TSoundID;
    function GetSoundByIndex(aIndex: integer): TALSSound;
    function GetSoundByID(aID: TSoundID): TALSSound;
    function GetSoundByStrID(aID: string): TALSSound;
    function GetSoundFileNameByIndex(aIndex: TSoundID): string;
    function GetSoundFileNameByID(aID: TSoundID): string;
    function GetLevel(aID: TSoundID): single;
    function GetAudioCurve(aID: TSoundID): PAudioFileLevel;

    property Count: integer read GetCount;
    property PlayList: TALSPlaylist read GetPlayList;
    property PlaybackContext: TALSPlaybackContext read FPlaybackContext;
  end;

var
  SoundManager: TSoundManager;


type

  { TAudioPreset }

  TAudioPreset = record
    Name: string;
    DryWet: single;
    EffectCount: integer;
    Effect1, Preset1,
    Effect2, Preset2,
    Effect3, Preset3: integer;
    procedure InitDefault;
    procedure InitFromString(s: string);
  end;

  function EffectIndexToALSoundEffectType(aEffectIndex: integer): TALSEffectType;

implementation
uses LazUTF8, Forms, u_resource_string, u_logfile, u_utils, u_common,
  u_program_options, PropertyUtils;
const
  AUDIO_HEADER='[AUDIO FILES]';

{ TAudioPreset }

procedure TAudioPreset.InitDefault;
begin
  Name := '';
  DryWet := 0.5;
  EffectCount := 0;
  Effect1 := -1;
  Preset1 := -1;
  Effect2 := -1;
  Preset2 := -1;
  Effect3 := -1;
  Preset3 := -1;
end;

procedure TAudioPreset.InitFromString(s: string);
var A: TStringArray;
  effect, preset: integer;
begin
  InitDefault;
  A := s.Split(['|']);

  if Length(A) < 3 then exit;
  Name := A[0];
  if not TryStrToInt(A[1], EffectCount) then exit;
  DryWet := StringToSingle(A[2]);

  if (EffectCount >= 1) and (Length(A) >= 5) then begin
    if not TryStrToInt(A[3], effect) then exit;
    if not TryStrToInt(A[4], preset) then exit;
    Effect1 := effect;
    Preset1 := preset;
  end;

  if (EffectCount >= 2) and (Length(A) >= 7) then begin
    if not TryStrToInt(A[5], effect) then exit;
    if not TryStrToInt(A[6], preset) then exit;
    Effect2 := effect;
    Preset2 := preset;
  end;

  if (EffectCount = 3) and (Length(A) = 9) then begin
    if not TryStrToInt(A[7], effect) then exit;
    if not TryStrToInt(A[8], preset) then exit;
    Effect3 := effect;
    Preset3 := preset;
  end;
end;

function EffectIndexToALSoundEffectType(aEffectIndex: integer): TALSEffectType;
begin
  case aEffectIndex of
    0: Result := AL_EFFECT_AUTOWAH;
    1: Result := AL_EFFECT_CHORUS;
    2: Result := AL_EFFECT_FLANGER;
    3: Result := AL_EFFECT_COMPRESSOR;
    4: Result := AL_EFFECT_DISTORTION;
    5: Result := AL_EFFECT_ECHO;
    6: Result := AL_EFFECT_EQUALIZER;
    7: Result := AL_EFFECT_FREQUENCYSHIFTER;
    8: Result := AL_EFFECT_PITCHSHIFTER;
    9: Result := AL_EFFECT_RINGMODULATOR;
   10: Result := AL_EFFECT_VOCALMORPHER;
   11: Result := AL_EFFECT_EAXREVERB;
   else Result := AL_EFFECT_NONE;
  end;
end;

{ TSoundItem }

procedure TSoundItem.AddEffect(const aEffect: TALSEffect; const aEffectName, aPresetName: string);
var p: PALSEffect;
  k: integer;
begin
  New( p );
  Move(aEffect, p^, SizeOf(TALSEffect));
  Effects.Add( p );

  if Length(EffectNames) = 0 then
    EffectNames := aEffectName+'.'+LowerCase(aPresetName)
  else
    EffectNames := EffectNames+'   '+aEffectName+'.'+LowerCase(aPresetName);

  k := Length(EffectNamesArray);
  SetLength(EffectNamesArray, k+1);
  EffectNamesArray[k] := aEffectName+'.'+LowerCase(aPresetName);
end;

procedure TSoundItem.SetDryWet(AValue: single);
var
  snd: TALSSound;
begin
  if Effects.Count=0 then exit;

  snd := Parent.GetSoundByID(SoundID);
  if snd <> NIL then
    snd.SetEffectDryWetVolume(PALSEffect(Effects.Items[0])^, AValue);
end;

procedure TSoundItem.DeleteEffects;
var snd: TALSSound;
begin
  snd := Parent.GetSoundByID(SoundID);
  if snd <> NIL then
    snd.RemoveAllEffects;

  while Effects.Count > 0 do begin
    Parent.FPlaybackContext.DeleteEffect(PALSEffect(Effects.Items[0])^);
    Dispose(PALSEffect(Effects[0]));
    Effects.Delete(0);
  end;
  EffectNames := '';
  EffectNamesArray := NIL;
end;

procedure TSoundItem.FreeEffectsList;
begin
  DeleteEffects;
  FreeAndNil(Effects);
end;

{ TSoundManager }

procedure TSoundManager.ResetIdValue;
begin
  FIDValue := 0;
end;

function TSoundManager.GetCount: integer;
begin
  Result := FPlaybackContext.SoundCount;
end;

function TSoundManager.GetPlayList: TALSPlaylist;
begin
  Result := FPlaybackContext.Playlist;
end;

function TSoundManager.GetNextIdValue: TSoundID;
begin
  inc( FIDValue );
  Result := FIDValue;
end;

procedure TSoundManager.RegisterSoundToItemList(aSnd: TALSSound);
var p: PSoundItem;
begin
  New(p);
  p^.Parent := Self;
  p^.SoundID := aSnd.Tag;
  p^.Effects := TFPList.Create;
  p^.EffectNames := '';
  p^.EffectNamesArray := NIL;

  p^.Peaks.InitDefault;
  if aSnd.Tag <> CAPTURE_IDAUDIO then begin
    p^.Peaks.LoadFromFile(aSnd.Filename);
  end;

  FSoundItems.Add(p);
  //Log.Debug('ID'+aSnd.Tag.ToString+' '+aSnd.Filename, 3);
end;

procedure TSoundManager.DeleteSoundFromItemList(aID: TSoundID);
var i: integer;
  item: PSoundItem;
begin
 for i:=0 to FSoundItems.Count-1 do
 begin
   item := PSoundItem(FSoundItems.Items[i]);
   if item^.SoundID = aID then
   begin
     item^.FreeEffectsList;
     Dispose( item );
     FSoundItems.Delete( i );
     exit;
   end;
 end;
end;

procedure TSoundManager.DeleteAllSoundFromItemList(aDeleteAlsoCaptureSound: boolean);
var item: PSoundItem;
  i: integer;
begin
  for i:=FSoundItems.Count-1 downto 0 do begin
    item := PSoundItem(FSoundItems.Items[i]);

    if ((item^.SoundID = CAPTURE_IDAUDIO) and aDeleteAlsoCaptureSound) or
       (item^.SoundID <> CAPTURE_IDAUDIO) then begin
      item^.FreeEffectsList;
      Dispose(item);
      FSoundItems.Delete(i);
    end;
  end;
end;

function TSoundManager.GetItemByID(aID: TSoundID): PSoundItem;
var i: integer;
begin
  for i:=0 to FSoundItems.Count-1 do begin
    Result := PSoundItem(FSoundItems.Items[i]);
    if Result^.SoundID = aID then exit;
  end;
  Result := NIL;
end;

procedure TSoundManager.AddEffectToSoundItem(aID: TSoundID;
  const aEffect: TALSEffect; const aEffectName, aPresetName: string);
var item: PSoundItem;
  i: integer;
begin
  for i:=0 to FSoundItems.Count-1 do begin
    item := PSoundItem(FSoundItems[i]);
    if item^.SoundID = aID then begin
      item^.AddEffect(aEffect, aEffectName, aPresetName);
      exit;
    end;
  end;
end;

procedure TSoundManager.DeleteAllEffectOnSoundItem(aID: TSoundID);
var item: PSoundItem;
  i: integer;
  snd: TALSSound;
begin
  for i:=0 to FSoundItems.Count-1 do begin
    item := PSoundItem(FSoundItems[i]);
    if item^.SoundID = aID then begin
      snd := GetSoundByID(item^.SoundID);
      if snd <> NIL then
        snd.RemoveAllEffects;

      item^.DeleteEffects;
      exit;
    end;
  end;
end;

function TSoundManager.GetSoundByID(aID: TSoundID): TALSSound;
var i: integer;
begin
{  if aID = CAPTURE_IDAUDIO then
    Result := FCapturedSound
  else
  begin  }
    for i:=0 to FPlaybackContext.SoundCount-1 do
      if FPlaybackContext.Sounds[i].Tag = aID then begin
        Result := FPlaybackContext.Sounds[i];
        exit;
      end;
    Result := NIL;
//  end;
end;

function TSoundManager.GetSoundByStrID(aID: string): TALSSound;
var i: integer;
begin
  if not TryStrToInt(aID, i) then Result := NIL
    else Result := GetSoundByID(i);
end;

function TSoundManager.GetSoundFileNameByIndex(aIndex: TSoundID): string;
begin
  if (aIndex >= 0) and (aIndex < Count) then
    Result := ExtractFileName(FPlaybackContext.Sounds[aIndex].Filename)
  else
    Result := SUnknownAudio;
end;

constructor TSoundManager.Create;
var
  attribs: TALSContextAttributes;
  i: integer;
  A: TStringArray;
begin
  {$ifdef MSWINDOWS}
    {$ifdef CPU386}
      ALSManager.SetLibrariesSubFolder('i386-win32');
    {$endif}
    {$ifdef CPU64}
      ALSManager.SetLibrariesSubFolder('x86_64-win64');
    {$endif}
  {$endif}

  {$ifdef LINUX}
  {$ifdef CPU386}
    ALSManager.SetLibrariesSubFolder('i386-linux');
  {$endif}
  {$ifdef CPU64}
    ALSManager.SetLibrariesSubFolder('x86_64-linux');
  {$endif}
  {$endif}

  ALSManager.LoadLibraries;
  ALSManager.VolumeMode := ALS_VOLUME_MODE_SQUARED;

  i := ProgramOptions.PlaybackDeviceIndex;
  A := ALSManager.ListOfPlaybackDeviceName;
  if (i < 0) or (i > High(A)) then FPlaybackContext := ALSManager.CreateDefaultPlaybackContext
  else begin
    attribs.InitDefault;
    attribs.ContextUseFloat := True;
    FPlaybackContext := ALSManager.CreatePlaybackContext(i, attribs);
  end;

  if FPlaybackContext.Error then
    Log.Error('CreatePlaybackContext - Error while creating the playback context '+FPlaybackContext.StrError)
  else
    Log.Info('Sound Manager created');

  FSoundItems := TFPList.Create;

  FEffectChain_SoundID := -1;
end;

destructor TSoundManager.Destroy;
var c: integer;
begin
  Log.Info('Destroying Sound Manager');

  DeleteEffectsOnAllSounds(True);
  StopCaptureToPlayback;
  StopAllSound(True);

  DeleteAllSoundFromItemList(True);
  Log.Info('Freed all sounds from item list', 1);
  FSoundItems.Free;

  c := FPlaybackContext.SoundCount;
  FPlaybackContext.Free;
  Log.Info('freed playback context', 1);

  if c > 0 then
    Log.Info('freed '+c.ToString+' sound(s)', 1);
  inherited Destroy;
end;

procedure TSoundManager.Clear;
begin
  DeleteEffectsOnAllSounds(False);
  DeleteAllSounds(False);
end;

procedure TSoundManager.Save(aStringList: TStringList);
var i, k, c: integer;
  snd: TALSSound;
  prop: TProperties;
begin
 if FPlaybackContext.SoundCount = 0 then exit;

 k := aStringList.Add( AUDIO_HEADER );
 c := 0;
 for i:=0 to FPlaybackContext.SoundCount-1 do
 begin
    snd := FPlaybackContext.Sounds[i];

    if snd.Tag >= 0 then // don't save capture stream
    begin
      inc(c);
      prop.Init(AUDIO_PARAM_SEPARATOR);
      prop.Add('TYPE', BoolToStr(snd is TALSStreamBufferSound, 'streamed', 'memory'));
      prop.Add('FILENAME', ExtractFileName(snd.Filename));
      prop.Add('ID', snd.Tag);
      prop.Add('LOOP', snd.Loop);
      aStringList.Add( prop.PackedProperty );
    end;
 end;
 aStringList.Insert(k+1, c.ToString );

end;

procedure TSoundManager.Load(aStringList: TStringList; aAbsolutPathToAdd: string);
var
  nb, k, v, i: integer;
  snd: TALSSound;
  audioType: string;
  fname: string;
  prop: TProperties;
  vbool: boolean;
begin
 audioType := '';
 fname := '';
 v := 0;
 vbool := False;

 Clear;
 k := aStringList.IndexOf( AUDIO_HEADER );
 if k=-1 then
 begin
   Log.Info('project don''t have audio file', 2);
   exit;
 end;

 Log.Info('TSoundManager.Load - Loading audio', 2);
 inc(k);
 nb := aStringList.Strings[k].ToInteger;
 if nb=0 then
 begin
   Log.Warning('There is 0 audio file to load', 3);
   exit;
 end;

 i := 1;
 repeat
  inc(k);
  prop.Split(aStringList.Strings[k], AUDIO_PARAM_SEPARATOR);

  if not prop.StringValueOf('TYPE', audioType, 'streamed') then
    Log.Error('TYPE property NOT FOUND for sound N°'+i.ToString, 3);

  if not prop.StringValueOf('FILENAME', fname, '') then
    Log.Error('FILENAME property NOT FOUND for sound N°'+i.ToString, 3);
  if aAbsolutPathToAdd <> ''
    then fname := ConcatPaths([aAbsolutPathToAdd, fname]);

  case audioType of
    'memory': snd := FPlaybackContext.AddSound(fname, True); //AddSound(fname);
  else
    snd := FPlaybackContext.AddStream(fname, True); // AddStream(fname);
  end;

  if not prop.IntegerValueOf('ID', v, 0) then
    Log.Error('ID property NOT FOUND for '+fname, 3);
  snd.Tag := v;
  RegisterSoundToItemList(snd);
  if FIDValue < snd.Tag then FIDValue := snd.Tag; // adjust the current ID

  if not prop.BooleanValueOf('LOOP', vbool, False) then
    Log.Error('LOOP property NOT FOUND for '+fname, 3);
  snd.Loop := vbool;

  dec(nb);
  inc(i);
 until nb = 0;
end;

function TSoundManager.AddStream(const aFilename: string): TALSSound;
begin
  Result := FPlaybackContext.AddStream(aFilename, True);
  Result.Tag := GetNextIdValue;
  RegisterSoundToItemList(Result);
end;

function TSoundManager.AddSound(const aFilename: string): TALSSound;
begin
  Result := FPlaybackContext.AddSound(aFilename, True);
  Result.Tag := GetNextIdValue;
  RegisterSoundToItemList(Result);
end;

procedure TSoundManager.DeleteSoundByID(aID: TSoundID);
var snd: TALSSound;
begin
  snd := GetSoundByID(aID);
  if snd <> NIL then
    FPlaybackContext.Delete(snd);

  DeleteSoundFromItemList(aID);
end;

procedure TSoundManager.StopAllSound(aStopAlsoCapturePlayback: boolean);
var i: integer;
begin
  if aStopAlsoCapturePlayback then FPlaybackContext.StopAllSound
    else begin
      for i:=0 to FPlaybackContext.SoundCount-1 do
        if FPlaybackContext.Sounds[i].Tag <> CAPTURE_IDAUDIO then
          FPlaybackContext.Sounds[i].Stop;
    end;
end;

procedure TSoundManager.DeleteEffectsOnAllSounds(aDeleteAlsoCaptureEffect: boolean);
var i: integer;
  sndItem: PSoundItem;
begin
  for i:=0 to FSoundItems.Count-1 do begin
    sndItem := PSoundItem(FSoundItems.Items[i]);
    if ((sndItem^.SoundID = CAPTURE_IDAUDIO) and aDeleteAlsoCaptureEffect) or
       (sndItem^.SoundID <> CAPTURE_IDAUDIO) then
      sndItem^.DeleteEffects;
  end;
end;

procedure TSoundManager.ResetPanOnAllSounds(aAlsoOnCapturePlayback: boolean);
var i: integer;
  sndItem: PSoundItem;
  snd: TALSSound;
begin
  for i:=0 to FSoundItems.Count-1 do begin
    sndItem := PSoundItem(FSoundItems.Items[i]);
    if ((sndItem^.SoundID = CAPTURE_IDAUDIO) and aAlsoOnCapturePlayback) or
       (sndItem^.SoundID <> CAPTURE_IDAUDIO) then begin
      snd := GetSoundByID(sndItem^.SoundID);
      if snd <> NIL then snd.Pan.Value := ALSound.ALS_PAN_CENTER;
    end;
  end;
end;

procedure TSoundManager.ResetPitchOnAllSounds(aAlsoOnCapturePlayback: boolean);
var i: integer;
  sndItem: PSoundItem;
  snd: TALSSound;
begin
  for i:=0 to FSoundItems.Count-1 do begin
    sndItem := PSoundItem(FSoundItems.Items[i]);
    if ((sndItem^.SoundID = CAPTURE_IDAUDIO) and aAlsoOnCapturePlayback) or
       (sndItem^.SoundID <> CAPTURE_IDAUDIO) then begin
      snd := GetSoundByID(sndItem^.SoundID);
      if snd <> NIL then snd.Pitch.Value := ALSound.ALS_PITCH_NORMAL;
    end;
  end;
end;

procedure TSoundManager.DeleteAllSounds(aDeleteAlsoCaptureSound: boolean);
var i: integer;
begin
  DeleteAllSoundFromItemList(aDeleteAlsoCaptureSound);

  ResetIdValue;

  if aDeleteAlsoCaptureSound then FPlaybackContext.DeleteAll
    else begin
      for i:=FPlaybackContext.SoundCount-1 downto 0 do
        if FPlaybackContext.Sounds[i].Tag <> CAPTURE_IDAUDIO then
          FPlaybackContext.Delete(FPlaybackContext.Sounds[i]);
    end;
end;

procedure TSoundManager.ToogleLoopModeOn(aID: TSoundID);
var
  snd: TALSSound;
begin
  snd := GetSoundByID(aID);
  if snd <> NIL then
    snd.Loop := not snd.Loop;
end;

function TSoundManager.AutoWahPresetList: TStringArray;
begin
  Result := FAutoWahProp.PresetList;
end;

function TSoundManager.ChorusPresetList: TStringArray;
begin
  Result := FChorusProp.PresetList;
end;

function TSoundManager.FlangerPresetList: TStringArray;
begin
  Result := FFlangerProp.PresetList;
end;

function TSoundManager.CompressorPresetList: TStringArray;
begin
  Result := FCompressorProp.PresetList;
end;

function TSoundManager.DistortionPresetList: TStringArray;
begin
  Result := FDistortionProp.PresetList;
end;

function TSoundManager.EchoPresetList: TStringArray;
begin
  Result := FEchoProp.PresetList;
end;

function TSoundManager.EqualizerPresetList: TStringArray;
begin
  Result := FEqualizerProp.PresetList;
end;

function TSoundManager.FreqShifterPresetList: TStringArray;
begin
  Result := FFreqShifterProp.PresetList;
end;

function TSoundManager.PitchShifterPresetList: TStringArray;
begin
  Result := FPitchShifterProp.PresetList;
end;

function TSoundManager.RingModulatorPresetList: TStringArray;
begin
  Result := FRingModulatorProp.PresetList;
end;

function TSoundManager.VocalMorpherPresetList: TStringArray;
begin
  Result := FVocalMorpherProp.PresetList;
end;

function TSoundManager.EAXReverbPresetList: TStringArray;
begin
  Result := FEAXReverbProp.PresetList;
end;

function TSoundManager.AddEffectOn(aID: TSoundID; aEffectType: TALSEffectType; aPresetIndex: integer): TALSEffect;
var snd: TALSSound;
  txt: String;
begin
  case aEffectType of
    AL_EFFECT_AUTOWAH:
      begin
        FAutoWahProp.InitWithPreset(aPresetIndex);
        Result := FPlaybackContext.CreateEffect(AL_EFFECT_AUTOWAH, FAutoWahProp );
      end;
    AL_EFFECT_CHORUS:
      begin
        FChorusProp.InitWithPreset(aPresetIndex);
        Result := FPlaybackContext.CreateEffect(AL_EFFECT_CHORUS, FChorusProp );
      end;
    AL_EFFECT_FLANGER:
      begin
        FFlangerProp.InitWithPreset(aPresetIndex);
        Result := FPlaybackContext.CreateEffect(AL_EFFECT_FLANGER, FFlangerProp );
      end;
    AL_EFFECT_COMPRESSOR:
      begin
        FCompressorProp.InitWithPreset(aPresetIndex);
        Result := FPlaybackContext.CreateEffect(AL_EFFECT_COMPRESSOR, FCompressorProp );
      end;
    AL_EFFECT_DISTORTION:
      begin
        FDistortionProp.InitWithPreset(aPresetIndex);
        Result := FPlaybackContext.CreateEffect(AL_EFFECT_DISTORTION, FDistortionProp );
      end;
    AL_EFFECT_ECHO:
      begin
        FEchoProp.InitWithPreset(aPresetIndex);
        Result := FPlaybackContext.CreateEffect(AL_EFFECT_ECHO, FEchoProp );
      end;
    AL_EFFECT_EQUALIZER:
      begin
        FEqualizerProp.InitWithPreset(aPresetIndex);
        Result := FPlaybackContext.CreateEffect(AL_EFFECT_EQUALIZER, FEqualizerProp );
      end;
    AL_EFFECT_FREQUENCYSHIFTER:
      begin
        FFreqShifterProp.InitWithPreset(aPresetIndex);
        Result := FPlaybackContext.CreateEffect(AL_EFFECT_FREQUENCYSHIFTER, FFreqShifterProp );
      end;
    AL_EFFECT_PITCHSHIFTER:
      begin
        FPitchShifterProp.InitWithPreset(aPresetIndex);
        Result := FPlaybackContext.CreateEffect(AL_EFFECT_PITCHSHIFTER, FPitchShifterProp );
      end;
    AL_EFFECT_RINGMODULATOR:
      begin
        FRingModulatorProp.InitWithPreset(aPresetIndex);
        Result := FPlaybackContext.CreateEffect(AL_EFFECT_RINGMODULATOR, FRingModulatorProp );
      end;
    AL_EFFECT_VOCALMORPHER:
      begin
        FVocalMorpherProp.InitWithPreset(aPresetIndex);
        Result := FPlaybackContext.CreateEffect(AL_EFFECT_VOCALMORPHER, FVocalMorpherProp );
      end;
    AL_EFFECT_EAXREVERB:
      begin
        FEAXReverbProp.InitWithPreset(aPresetIndex);
        Result := FPlaybackContext.CreateEffect(AL_EFFECT_EAXREVERB, FEAXReverbProp );
      end;
  end;

  if not Result.Ready then begin
    snd := GetSoundByID(aID);
    if snd <> NIL then txt := ' for '+snd.Filename
      else txt := ' for a sound that does not exist in the list (ID'+aID.ToString+')';
    Log.Warning('Failed to create audio effect '+AudioFXToString(aEffectType, aPresetIndex)+txt);
  end;

  AddEffectToSoundItem(aID, Result, AudioFXName(aEffectType), AudioFXPresetName(aEffectType, aPresetIndex));
end;

procedure TSoundManager.ConstructChainOn(aID: TSoundID);
var i, j: integer;
  snd: TALSSound;
  item: PSoundItem;
begin
  for i:=0 to FSoundItems.Count-1 do
  begin
    item := PSoundItem(FSoundItems[i]);

    if item^.SoundID = aID then
    begin
Log.Debug('Construct Chain effect on ID'+aID.ToString+' with '+item^.Effects.Count.ToString+' effect(s)');
     // chain and assign the effects to the sound
     if item^.Effects.Count > 1 then
     begin
       for j:=0 to item^.Effects.Count-2 do
         PALSEffect(item^.Effects[j])^.ChainWith( PALSEffect(item^.Effects[j+1])^ );
     end;
     snd := GetSoundByID(aID);
     if snd <> NIL then
       snd.ApplyEffect( PALSEffect(item^.Effects[0])^ );
     exit;
    end;
  end;
end;

procedure TSoundManager.SetDryWetOn(aID: TSoundID; aValue: single);
var
  item: PSoundItem;
  i: integer;
begin
  for i:=0 to FSoundItems.Count-1 do
  begin
    item := PSoundItem(FSoundItems[i]);
    if item^.SoundID = aID then
    begin
      item^.SetDryWet(aValue);
      exit;
    end;
  end;
end;

procedure TSoundManager.DeleteEffectsOn(aID: TSoundID);
begin
//Log.Debug('TSoundManager.DeleteEffectsOn '+aID.ToString);
  DeleteAllEffectOnSoundItem(aID);
end;

function TSoundManager.GetEffectsNamesOn(aID: TSoundID): string;
var
  item: PSoundItem;
  i: integer;
begin
  for i:=0 to FSoundItems.Count-1 do
  begin
    item := PSoundItem(FSoundItems[i]);
    if item^.SoundID = aID then
    begin
      Result := item^.EffectNames;
      exit;
    end;
  end;
  Result := '';
end;

function TSoundManager.GetEffectsNamesArrayOn(aID: TSoundID): TStringArray;
var item: PSoundItem;
  i: integer;
begin
  for i:=0 to FSoundItems.Count-1 do
  begin
    item := PSoundItem(FSoundItems[i]);
    if item^.SoundID = aID then
    begin
      Result := item^.EffectNamesArray;
      exit;
    end;
  end;
  Result := NIL;
end;

procedure TSoundManager.PrepareEffectChain(aID: TSoundID; aDryWet: single; aEffectCount: integer);
begin
  FEffectChain_SoundID := aID;
  FEffectChain_DryWet := aDryWet;
  FEffectChain_EffectCount := aEffectCount;
//Log.AddEmptyLine();
//Log.Debug('Prepare Effect Chain on ID'+aID.ToString+' DryWet '+FormatFloatWithDot('0.00', aDryWet)+
//   ' effect: '+aEffectCount.ToString);
  DeleteEffectsOn(FEffectChain_SoundID);
end;

procedure TSoundManager.AddToEffectChain(aEffectType: TALSEffectType; aPresetIndex: integer);
var snd: TALSSound;
begin
  snd := GetSoundByID(FEffectChain_SoundID);
  if snd = NIL then exit;

  AddEffectOn(FEffectChain_SoundID, aEffectType, aPresetIndex);

//Log.Debug('    FEffectChain_EffectCount '+FEffectChain_EffectCount.ToString);
  if FEffectChain_EffectCount > 0 then begin
    dec(FEffectChain_EffectCount);
    if FEffectChain_EffectCount = 0 then begin
      ConstructChainOn(FEffectChain_SoundID);
      SetDryWetOn(FEffectChain_SoundID, FEffectChain_DryWet);
    end;
  end;
end;

procedure TSoundManager.StartCaptureToPlayback;
var i: integer;
  A: TStringArray;
begin
  if FCaptureContext <> NIL then
    exit;

  A := ALSManager.ListOfCaptureDeviceName;
  i := ProgramOptions.CaptureDeviceIndex;
  if (i < 0) or (i > High(A)) then FCaptureContext := ALSManager.CreateDefaultCaptureContext
  else FCaptureContext := ALSManager.CreateCaptureContext(i, 44100, ALS_CAPTUREFORMAT_STEREO16, 0.100);

  FCapturedSound := FCaptureContext.PrepareToPlayback( FPlaybackContext );
  FCapturedSound.Tag := CAPTURE_IDAUDIO;
  FCapturedSound.SetFileName(SCapture);

  RegisterSoundToItemList( FCapturedSound );
  FCaptureContext.StartCapture;
  FCaptureContext.MonitoringEnabled := True;

  if FCaptureContext.Error then begin
    Log.Error('Error while opening the default capture device');
    Log.Error(FCaptureContext.StrError, 1);
    if Length(ALSManager.ListOfCaptureDeviceName) = 0 then
      Log.Error('No audio capture device found', 1)
    else
      Log.Info('Default capture device: '+ALSManager.DefaultCaptureDeviceName, 1);
  end
  else begin
    Log.Info('Audio capture to playback started');
    if FCapturedSound.Error then
      Log.Error('Capture to playback error: '+FCapturedSound.StrError, 1);
  end;
end;

procedure TSoundManager.SetOnCaptureBufferEvent(aProc: TALSOnCaptureBuffer);
begin
  FCaptureContext.OnCaptureBuffer := aProc;
end;

procedure TSoundManager.SetCapturePreAmp(AValue: single);
begin
  if FCaptureContext <> NIL then
    FCaptureContext.PreAmp := AValue;
end;

function TSoundManager.GetCapturePreAmp: single;
begin
  if FCaptureContext <> NIL then
    Result := FCaptureContext.PreAmp;
end;

procedure TSoundManager.SetCaptureVolume(AValue: single);
begin
  if FCapturedSound <> NIL then
    FCapturedSound.Volume.Value := AValue;
end;

function TSoundManager.GetCaptureVolume: single;
begin
  if (FCaptureContext <> NIL) and (FCapturedSound <> NIL) then
    Result := FCapturedSound.Volume.Value;
end;

procedure TSoundManager.SetCapturePan(AValue: single);
begin
  if FCapturedSound <> NIL then
    FCapturedSound.Pan.Value := AValue;
end;

function TSoundManager.GetCapturePan: single;
begin
  if (FCaptureContext <> NIL) and (FCapturedSound <> NIL) then
    Result := FCapturedSound.Pan.Value;
end;

function TSoundManager.CaptureToPlaybackIsReady: boolean;
begin
  Result := FCaptureContext <> NIL;
  if Result then
    Result := not FCaptureContext.Error;
  if Result then
    Result := FCapturedSound <> NIL;
  if Result then
    Result := not FCapturedSound.Error;
end;

function TSoundManager.CaptureStrError: string;
begin
  if FCaptureContext=NIL then
    Result := 'Capture context not created'
  else if FCaptureContext.Error then
    Result := FCaptureContext.StrError
  else if FCapturedSound.Error then
    Result := FCapturedSound.StrError
  else Result := '';
end;

procedure TSoundManager.StopCaptureToPlayback;
begin
  if FCaptureContext = NIL then exit;

  // delete effect from sound before stopping capture
  // because StopCapture destroy the playback sound's object.
  DeleteAllEffectOnSoundItem( FCapturedSound.Tag );
  DeleteSoundFromItemList( FCapturedSound.Tag );

  FCaptureContext.OnCaptureBuffer := NIL;

  FCaptureContext.StopCapture;

  if FCaptureContext.CaptureError then begin
    Log.Info('Error while stopping audio capture to playback', 1);
    Log.Error(FCaptureContext.StrCaptureError, 1);
  end;

  FCapturedSound := NIL;
  FCaptureContext.Free;
  FCaptureContext := NIL;
end;

procedure TSoundManager.CaptureSetVolume(aValue: single);
begin
  if (FCaptureContext = NIL) or (FCapturedSound = NIL) then exit;
  if FCaptureContext.State <> ALS_RECORDING then exit;
  FCapturedSound.Volume.Value := aValue;
end;

function TSoundManager.CaptureGetVolume: single;
begin
  if (FCaptureContext = NIL) or (FCapturedSound = NIL) then Result := 0
    else Result := FCapturedSound.Volume.Value;
end;

procedure TSoundManager.CaptureSetPan(aValue: single);
begin
  if (FCaptureContext = NIL) or (FCapturedSound = NIL) then exit;
  if FCaptureContext.State <> ALS_RECORDING then exit;
  FCapturedSound.Pan.Value := aValue;
end;

procedure TSoundManager.ResetState;
begin
  StopAllSound(True);
  DeleteEffectsOnAllSounds(True);
  StopCaptureToPlayback;
end;

function TSoundManager.IDToIndex(aID: TSoundID): integer;
var i: integer;
begin
  for i:=0 to FPlaybackContext.SoundCount-1 do
    if FPlaybackContext.Sounds[i].Tag = aID then begin
      Result := i;
      exit;
    end;
  Result := -1;
end;

function TSoundManager.IndexToID(aIndex: integer): TSoundID;
var snd: TALSSound;
begin
  snd := GetSoundByIndex(aIndex);
  if snd <> NIL then Result := snd.Tag
    else Result := -1;
end;

function TSoundManager.GetSoundByIndex(aIndex: integer): TALSSound;
begin
  if (aIndex >= 0) and (aIndex < Count) then
    Result := FPlaybackContext.Sounds[aIndex]
  else
    Result := NIL;
end;

function TSoundManager.GetSoundFileNameByID(aID: TSoundID): string;
var snd: TALSSound;
begin
  snd := GetSoundByID(aID);
  if snd = NIL
    then Result := SUnknownAudio
    else Result := ExtractFileName(snd.Filename);
end;

function TSoundManager.GetLevel(aID: TSoundID): single;
var snd: TALSSound;
  i: integer;
  s: single;
begin
  if aID = CAPTURE_IDAUDIO then begin
    if not CaptureToPlaybackIsReady then
      Result := 0
    else begin
      Result := FCaptureContext.ChannelsLevel[0];
      if Result < FCaptureContext.ChannelsLevel[1] then
        Result := FCaptureContext.ChannelsLevel[1];
    end;
  end
  else begin
    snd := GetSoundByID(aID);
    if (snd = NIL) or snd.Error then
      Result:=0.0
    else begin
      // compute the average of channel's levels
      s := 0;
      for i:=0 to snd.ChannelCount-1 do
        s := s+snd.ChannelsLevel[i];
      Result := s / snd.ChannelCount;
    end;
  end;
end;

function TSoundManager.GetAudioCurve(aID: TSoundID): PAudioFileLevel;
var item: PSoundItem;
begin
  item := GetItemByID(aID);
  if item <> NIL then Result := @item^.Peaks
    else Result := NIL;
end;


end.

