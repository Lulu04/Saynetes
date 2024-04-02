unit u_top_player;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  u_common, u_list_top, u_utils;


type


{ TTOPPlayer }

TTOPPlayer=class
private
  ThreadAction: TTimedThread;
  procedure StartThread;
  procedure StopThread;
private
  FOnTimeElapsed,
  FOnEndPreview: TNotifyEvent;
  FRunningTime,
  FTimeZero: QWORD;
  FPlaying: boolean;
  procedure DoTimeElapsedEvent;
  procedure DoEndPreviewEvent;
private
  FMsAccu: integer;
  FPreview: TSequence;
  function GetPreviewPlaying: boolean;

public
  constructor Create;
  destructor Destroy; override;
  procedure Update;

  procedure PreviewCmdList( const aCmds: TCmdList );
  procedure PreviewSequencerInfoList( aSequencerInfoList: TSequencerInfoList );

  procedure StopPreview;

  function PosPercent: single; // give the percentage [0..1] between current line index and its max value
  function PreviewTimePosition: single; // gives the current playing time position

  property PreviewPlaying: boolean read GetPreviewPlaying;
  property OnEndPreview: TNotifyEvent read FOnEndPreview write FOnEndPreview;

  property OnTimeElapsed: TNotifyEvent read FOnTimeElapsed write FOnTimeElapsed;
end;

var TopPlayer: TTOPPlayer;



  procedure ExecuteCmd( const aCmd: TSingleCmd );
  procedure ExecuteCmd( const A: TParamArray );

implementation
uses LCLIntf, Graphics, u_list_dmxuniverse,
  u_audio_manager, u_helper, u_logfile, u_mainform, ALSound;


var snd: TALSSound;

procedure ExecuteCmd(const aCmd: TSingleCmd);
begin
  ExecuteCmd( aCmd.SplitToParamArray );
end;

procedure ExecuteCmd(const A: TParamArray);
var cmd: longint;
  uni, uni2: TDmxUniverse;
  fix, fix2: TDMXFixture;
  chan, chan2: TDMXChannel;
  pt: TSequence;
  procedure LogError(aMess: string);
  var i: integer;
  begin
    aMess := aMess+lineending+'      Received:';
    for i:=0 to High(A) do
      aMess := aMess+' '+A[i];
    Log.Error(aMess);
  end;
begin
  if Length(A)= 0 then exit;
  if not tryStrToInt(A[0], cmd ) then
  begin
    LogError('ExecuteCmd: Command not recognized');
    exit;
  end;

  try
    case cmd of
     CMD_STARTSEQUENCE: begin
       pt := Sequences.GetTopByID(A[1].ToInteger);
       if pt = NIL then exit;
       if not pt.Running then
         pt.RunAsSequencerInfoList;
     end;

     CMD_STOPSEQUENCE: begin
       pt := Sequences.GetTopByID(A[1].ToInteger);
       if pt = NIL then exit;
       if pt.Running then
         pt.Stop;
     end;

     CMD_SEQUENCESTRETCHTIME: begin // ATOPSTRETCHTIME IDTop StretchValue Duration CurveID
       pt := Sequences.GetTopByID(A[1].ToInteger);
       if pt = NIL then exit;
       pt.TimeStretchFactor.ChangeTo(StringToSingle(A[2]), StringToSingle(A[3]), A[4].ToInteger);
     end;

     CMD_AUDIO_PLAY: begin  // AUDIOLECTURE IDaudio volume panning
       snd := SoundManager.GetSoundByID(A[1].ToInteger);
       if snd = NIL then exit;
       snd.Volume.Value := StringToSingle(A[2]);
       snd.Pan.Value := StringToSingle(A[3]);
       snd.Play( snd.State <> ALS_PAUSED );
     end;

     CMD_AUDIO_STOP: begin  // AUDIOSTOP IDaudio
       snd := SoundManager.GetSoundByID(A[1].ToInteger);
       if snd <> NIL then
         snd.Stop;
     end;

     CMD_AUDIO_PAUSE: begin // AUDIOPAUSE IDaudio
       snd := SoundManager.GetSoundByID(A[1].ToInteger);
       if snd <> NIL then
         snd.Pause;
     end;

     CMD_AUDIO_FADEIN: begin // AUDIOFADEIN IDaudio volume duration IDcurve
       snd := SoundManager.GetSoundByID(A[1].ToInteger);
       if snd <> NIL then
         snd.FadeIn(StringToSingle(A[2]), StringToSingle(A[3]), TALSCurveID(A[4].ToInteger));
     end;

     CMD_AUDIO_FADEOUT: begin // AUDIOFADEOUT IDaudio duration IDcurve
       snd := SoundManager.GetSoundByID(A[1].ToInteger);
       if snd <> NIL then
         snd.FadeOut( StringToSingle(A[2]), TALSCurveID(A[3].ToInteger) );
     end;

     CMD_AUDIO_SETVOLUME: begin // AUDIOFIXEVOLUME IDaudio volume duration IDcurve
       snd := SoundManager.GetSoundByID(A[1].ToInteger);
       if snd <> NIL then
         snd.Volume.ChangeTo( StringToSingle(A[2]), StringToSingle(A[3]), TALSCurveID(A[4].ToInteger) );
     end;

     CMD_AUDIO_SETPAN: begin // AUDIOFIXEPAN IDaudio panning duration IDcurve
       snd := SoundManager.GetSoundByID(A[1].ToInteger);
       if snd <> NIL then
         snd.Pan.ChangeTo( StringToSingle(A[2]), StringToSingle(A[3]), TALSCurveID(A[4].ToInteger) );
     end;

     CMD_AUDIO_SETPITCH: begin // AUDIOFIXEFREQ IDaudio frequence duration IDcurve
       snd := SoundManager.GetSoundByID(A[1].ToInteger);
       if snd <> NIL then
         snd.Pitch.ChangeTo( StringToSingle(A[2]), StringToSingle(A[3]), TALSCurveID(A[4].ToInteger) );
     end;

     TITLECMD_AUDIO_APPLYFX: begin // TITLECMD_AUDIO_APPLYFX  IDaudio  dry/wet  EffectCount
       SoundManager.PrepareEffectChain( A[1].ToInteger, StringToSingle(A[2]), A[3].ToInteger );
     end;

     CMD_AUDIO_FXPRESET: begin // CMD_AUDIO_FXPRESET  effectType  presetIndex
       SoundManager.AddToEffectChain(TALSEffectType(A[1].ToInteger), A[2].ToInteger);
     end;

     CMD_AUDIO_REMOVEFX: begin // CMD_AUDIO_REMOVEFX  IDaudio
       SoundManager.DeleteEffectsOn( A[1].ToInteger );
     end;

     CMD_AUDIO_CAPTURE_START: begin
       SoundManager.StartCaptureToPlayback;
       SoundManager.SetCapturePreAmp(FormMain.FrameMainAudio1.FrameAudioCapture1.PreAmp);
       FormMain.FrameMainAudio1.FrameAudioCapture1.UpdateVisual;
     end;

     CMD_AUDIO_CAPTURE_STOP: begin
       SoundManager.StopCaptureToPlayback;
       FormMain.FrameMainAudio1.FrameAudioCapture1.UpdateVisual;
     end;

     CMD_AUDIO_CAPTURE_SETVOLUME: begin // CMD_AUDIO_CAPTURE_SETVOLUME volume duration IDcurve
       snd := SoundManager.GetSoundByID( CAPTURE_IDAUDIO );
       if snd <> NIL then
         snd.Volume.ChangeTo( StringToSingle(A[1]), StringToSingle(A[2]), TALSCurveID(A[3].ToInteger) );
     end;

     CMD_AUDIO_CAPTURE_SETPAN: begin // CMD_AUDIO_CAPTURE_SETPAN volume duration IDcurve
       snd := SoundManager.GetSoundByID( CAPTURE_IDAUDIO );
       if snd <> NIL then
         snd.Pan.ChangeTo( StringToSingle(A[1]), StringToSingle(A[2]), TALSCurveID(A[3].ToInteger) );
     end;

     TITLECMD_AUDIO_CAPTURE_APPLYFX: begin
       SoundManager.PrepareEffectChain( CAPTURE_IDAUDIO, StringToSingle(A[1]), A[2].ToInteger );
     end;

     CMD_AUDIO_CAPTURE_REMOVEFX: begin
       SoundManager.DeleteEffectsOn( CAPTURE_IDAUDIO );
     end;

     CMD_INTERNALDMXWAVE: begin // INTERNALDMXWAVE IDuniverse IDFixture ChanIndex percent1 duration1 IDcurve1 keeptime percent2 duration2 IDcurve2
       if UniverseManager.RetrieveChannel(A[1].ToInteger, A[2].ToInteger,
                                          A[3].ToInteger, uni, fix, chan) then
         chan.StartInternalWave(StringToSingle(A[4]), StringToSingle(A[5]),
                                A[6].ToInteger, StringToSingle(A[7]),
                                StringToSingle(A[8]), StringToSingle(A[9]),
                                A[10].ToInteger);
     end;

     CMD_DMX_DIMMER: begin  // DMXDIMMER IDuniverse IDFixture ChanIndex percent duration IDcurve
       if UniverseManager.RetrieveChannel(A[1].ToInteger, A[2].ToInteger,
                                          A[3].ToInteger, uni, fix, chan) then
         chan.StartDimmer(StringToSingle(A[4]), StringToSingle(A[5]), A[6].ToInteger);
     end;

     CMD_DMX_FLAME: begin  // DMXFLAME IDuniverse IDFixture ChanIndex LevelMin LevelMax Speed Soften
       if UniverseManager.RetrieveChannel(A[1].ToInteger, A[2].ToInteger,
                                          A[3].ToInteger, uni, fix, chan) then
         chan.StartFlame(StringToSingle(A[4]), StringToSingle(A[5]),
                         StringToSingle(A[6]), StringToSingle(A[7]));
     end;

     CMD_DMX_AUDIOFOLLOWER: begin // DMXAUDIOFOLLOWER IDuniverse IDFixture ChanIndex IDaudio gainF MaxPercentF SoftenTimeF
       if UniverseManager.RetrieveChannel(A[1].ToInteger, A[2].ToInteger,
                                          A[3].ToInteger, uni, fix, chan) then
         chan.StartAudioFollower(A[4].ToInteger, StringToSingle(A[5]),
                                 StringToSingle(A[6]), StringToSingle(A[7]));
     end;

     CMD_DMX_FLASH: begin // CMD_DMX_FLASH IDuniverse IDFixture ChanIndex
                          //               LevelMin LevelMax DurationMin DurationMax
       if UniverseManager.RetrieveChannel(A[1].ToInteger, A[2].ToInteger,
                                          A[3].ToInteger, uni, fix, chan) then
         chan.StartFlash(StringToSingle(A[4]), StringToSingle(A[5]),
                         StringToSingle(A[6]), StringToSingle(A[7]));
     end;

     CMD_DMX_COPYCHANNEL: begin // DMXCOPYCHANNEL SourceIDuniverse SourceIDFixture SourceChanIndex TargetIDUniverse TargetIDFixture TargetChanIndex
       if not UniverseManager.RetrieveChannel(A[1].ToInteger, A[2].ToInteger, A[3].ToInteger, uni, fix, chan)
         then exit;
       if not UniverseManager.RetrieveChannel(A[4].ToInteger, A[5].ToInteger, A[6].ToInteger, uni2, fix2, chan2)
         then exit;
       chan2.StartCopy(chan);
     end;

     CMD_DMX_STOPEFFECT: begin  // DMXSTOPEFFECT IDuniverse IDFixture ChanIndex
       if UniverseManager.RetrieveChannel(A[1].ToInteger, A[2].ToInteger,
                                          A[3].ToInteger, uni, fix, chan) then
         chan.StopEffect;
     end;

     CMD_DMX_DIMMERRGB: begin   // DMXDIMMERRGB IDuniverse IDFixture Color Duration IDcurve
       if UniverseManager.RetrieveFixture(A[1].ToInteger, A[2].ToInteger,
                                          uni, fix) then
         fix.StartDimmerRGB(TColor(A[3].ToInteger), StringToSingle(A[4]),
                            A[5].ToInteger);
     end;

     CMD_DMX_FLAMERGB: begin  // DMXFLAMERGB IDuniverse IDFixture Color Speed Amplitude Soften
       if UniverseManager.RetrieveFixture(A[1].ToInteger, A[2].ToInteger,
                                          uni, fix) then
         fix.StartFlameRGB(TColor(A[3].ToInteger), StringToSingle(A[4]),
                           StringToSingle(A[5]), StringToSingle(A[6]));
     end;

     CMD_DMX_AUDIOFOLLOWERRGB: begin  // DMXAUDIOFOLLOWERRGB IDuniverse IDFixture IDaudio Color Gain SoftenTime
       if UniverseManager.RetrieveFixture(A[1].ToInteger, A[2].ToInteger,
                                          uni, fix) then
         fix.StartAudioFollowerRGB(A[3].ToInteger, A[4].ToInteger,
                                   StringToSingle(A[5]), StringToSingle(A[6]));
     end;

     CMD_DMX_STOPEFFECTRGB: begin  // DMXSTOPEFFECTRGB IDuniverse IDFixture
       if UniverseManager.RetrieveFixture(A[1].ToInteger, A[2].ToInteger,
                                          uni, fix) then
         fix.StopEffectRGB;
     end;

     CMD_DMX_COPYRGB: begin  // DMXCOPYRGB SourceIDuniverse SourceIDFixture TargetIDUniverse TargetIDFixture
       if not UniverseManager.RetrieveFixture(A[1].ToInteger, A[2].ToInteger, uni, fix)
         then exit;
       if not UniverseManager.RetrieveFixture(A[3].ToInteger, A[4].ToInteger, uni2, fix2)
         then exit;
       fix2.StartCopyRGB(fix);
     end;

     CMD_DMX_FLASHRGB: begin  // CMD_DMX_FLASHRGB IDuniverse IDFixture Color pcMin pcMax DurationMin DurationMax
       if UniverseManager.RetrieveFixture(A[1].ToInteger, A[2].ToInteger,
                                          uni, fix) then
         fix.StartFlashRGB(TColor(A[3].ToInteger), StringToSingle(A[4]), StringToSingle(A[5]),
                            StringToSingle(A[6]), StringToSingle(A[7]));
     end;

    end;//case
  except
    LogError('ExecuteCmd: An exception occurs while decoding an action');
  end;
end;

{ TTOPPlayer }

procedure TTOPPlayer.StartThread;
begin
  //ThreadAction:=TTimedThread.Create(30, FormMain.Handle, LM_MESSAGE_Player, 0, 0, TRUE);
  ThreadAction:=TTimedThread.CreateSynchronize(30, @Update, TRUE);
end;

procedure TTOPPlayer.StopThread;
begin
  if ThreadAction<>NIL then begin
    ThreadAction.Terminate;
    ThreadAction.WaitFor;
    ThreadAction.Free;
    ThreadAction:=NIL;
  end;
end;

procedure TTOPPlayer.DoTimeElapsedEvent;
begin
  if FOnTimeElapsed<>NIL then FOnTimeElapsed(self);
end;

procedure TTOPPlayer.DoEndPreviewEvent;
begin
  if FOnEndPreview<>NIL then FOnEndPreview(self);
end;

function TTOPPlayer.GetPreviewPlaying: boolean;
begin
  Result:=FPreview.Running;
end;

constructor TTOPPlayer.Create;
begin
  FPreview:=TSequence.Create;
  StartThread;
  Log.Info('Sequence Player Created');
end;

destructor TTOPPlayer.Destroy;
begin
  StopThread;
  FPreview.Free;
  Log.Info('Destroying Sequence Player');
  inherited Destroy;
end;

procedure TTOPPlayer.Update;
var TimeNow, deltaT: QWord;
  deltaSec: single;
  P: TStringArray;
  pt: TSequence;
  i: Integer;
  loopAlreadyDone: boolean;
begin
 TimeNow := GetTickCount64;
 deltaT := TimeNow - FRunningTime;
 if deltaT = 0 then
   exit;

 FRunningTime += deltaT;

 FMsAccu += deltaT;
 if FMsAccu > 50 then
 begin
   FMsAccu -= 50;
   DoTimeElapsedEvent;
 end;

 deltaSec := deltaT*0.001;
 // scan all top
 for i:=-1 to Sequences.Count-1 do
 begin
   if i = -1 then
     pt := FPreview
   else
     pt := Sequences.GetTopByIndex(i);

   pt.TimeStretchFactor.OnElapse(deltaSec);
   if pt.Running then
   begin
     pt.Clock := pt.Clock+deltaSec*pt.TimeStretchFactor.Value;
     if pt.WaitSec > 0 then
       pt.WaitSec -= deltaSec*pt.TimeStretchFactor.Value;
     if (pt.WaitSec <= 0) and (Length(pt.CmdArray) > 0) then
     begin
           loopAlreadyDone := False;
           repeat
              P := pt.CmdArray[pt.LineIndex].SplitToParamArray; // read and split one cmd

              case P[0].ToInteger of
                CMD_WAIT: begin
                  pt.WaitSec += StringToSingle(P[1]); // trick to take in account the small pause value<deltaT
                  loopAlreadyDone := False;
                end;
                CMD_LOOP: begin
                  if loopAlreadyDone then pt.WaitSec := 10 // avoid infinite loop in case of sequence with only loop action.
                    else pt.LoopToBegin;
                  loopAlreadyDone := True;
                end
                else begin
                  ExecuteCmd( P );
                //  loopAlreadyDone := False;
                end;
              end;

              pt.NextLine;
           until pt.EndOfPlay or (pt.WaitSec>0);

           if pt.EndOfPlay then
           begin
             pt.Stop;
             if i=-1 then DoEndPreviewEvent;
           end;
     end;
   end;
 end;
end;

procedure TTOPPlayer.PreviewCmdList(const aCmds: TCmdList);
begin
  FPreview.InitByDefault;
  FPreview.SequencerInfoList:=aCmds;
  FPreview.RunAsCmdList;
  FMsAccu := 0;
  FRunningTime := GetTickCount64;
  FTimeZero := FRunningTime;
end;

procedure TTOPPlayer.PreviewSequencerInfoList(aSequencerInfoList: TSequencerInfoList);
begin
  FPreview.InitByDefault;
  FPreview.SequencerInfoList:=aSequencerInfoList;
  FPreview.RunAsSequencerInfoList;
  FMsAccu := 0;
  FRunningTime := GetTickCount64;
  FTimeZero := FRunningTime;
end;

procedure TTOPPlayer.StopPreview;
begin
 FPlaying := FALSE;
 FPreview.Stop;
 OnTimeElapsed:=NIL;
 OnEndPreview:=NIL;
end;

function TTOPPlayer.PosPercent: single;
begin
 if not FPreview.Running
   then Result := 0
   else Result := FPreview.LineIndex/High(FPreview.CmdArray);
end;

function TTOPPlayer.PreviewTimePosition: single;
begin
 Result:=FPreview.Clock;
end;

end.

