unit u_helper;

{$mode objfpc}{$H+}
{$modeswitch TypeHelpers}

interface

uses
  Classes, SysUtils, Graphics,
  u_common, u_list_dmxuniverse,
  frame_bglvirtualscreen_sequencer;

type

{ TCmdListHelper }

TCmdListHelper = type helper(TStringHelper) for TCmdList
  function SplitToArray( aCharSeparator: char ): TStringArray;
public
  function GetOnlyCmd: integer;
  function IsSingleCmd: boolean;
  function IsTitle: boolean;
  function IsWait(out sec: single): boolean;
  procedure ChangeParamFromTitleParam(const aTitleCmd: TSingleCmd);
  function SplitToParamArray: TParamArray;
  procedure ConcatCmd( aCmd: TSingleCmd );
  procedure ConcatCmdList( aCmdList: TCmdList );
public
  function SplitToCmdArray: TCmdArray;
  function ComputeCmdListDuration: single;
  // pour chaque commande de la liste, on ajoute une étape au séquenceur cible
  // utile pour 'aplatir les cmds avant une exécution
  procedure OneStepForOneCmd( aSLTarget: TStepList; var aTimePos: single );
public
  function SplitToStepDataArray: TStepDataArray;
public
  function SplitToSequencerInfoArray: TSequencerInfoArray;
  function SequencerInfoListToCmdListOfSingleCmd: string;
  function SequencerInfoListIsLooped: boolean;
end;

{ TSplittedCmdsHelper }

TSplittedCmdsHelper = type helper for TStringArray
  function MergeToString( aCharSeparator: char; index: integer=0 ): string;
  function PackToCmd: TSingleCmd;
  function PackToCmdList: TCmdList;
  procedure MultiplyAllPauseByCoeff( aCoef: double );
  // modify the duration of all actions
  procedure SetCmdDuration(aDuration: single);
  procedure MultiplyAllDurationByCoeff( aCoef: double );

  // convertie le premier élément du TParamArray en Cmd
  // renvoie UNKNOW_COMMAND si le TParamArray est non valide
  function ParamArray0ToCmd: integer;
  // aFromIndex is 0 based
  function RePackToString( aFromIndex: integer; const aSeparator: string ): String;
end;

{ TTStepListHelper }

TTStepListHelper = class helper for TStepList
  // Converti le TStepList (Self) en TStepList.
  // Les étapes complexes sont converties en TSingleCmd afin de pouvoir les traiter une par une
  function ToStepListOfSingleCmd: TStepList;
  // Converti le TStepList (Self) en TCmdList.
  // Les étapes complexes sont converties en TSingleCmd afin de pouvoir les traiter une par une
  function ToCmdListOfSingleCmd: TCmdList;
  // converti le TStepList (Self) en TCmdList.
  // Les étapes complexes ne sont pas convertie en TSingleCmd
  function PackToCmdList: TCmdList;

  procedure ToStepDataList( var OutString: TSequencerInfoList );
  procedure SelectedToStepDataList( var OutString: TSequencerInfoList );

  // chaser and other function utils
  function Duration: single;
  procedure AddCmdWait(aTimePos, aDuration: single);
  procedure AddCmdLoop(aTimePos: single);
  procedure AddCmdDMXDimmer(aTimePos: single;
                            aChan: TDMXChannel;
                            aPercent,
                            aDuration: single;
                            aCurveID: word);
  procedure AddCmdWave(aTimePos: single;
                       aChan: TDMXChannel;
                       aPercent1, aDuration1: single; aCurveID1: word;
                       aKeepTime,
                       aPercent2, aDuration2: single; aCurveID2: word);
  procedure AddCmdDMXDimmerRGB(aTimePos: single;
                               aFix: TDMXFixture;
                               aColor: TColor;
                               aDuration: single;
                               aCurveID: word);
end;


implementation

uses frame_sequencer, u_utils;


{ TTStepListHelper }

function TTStepListHelper.ToStepListOfSingleCmd: TStepList;
var tp: single;
  step: TSequenceStep;
  i: integer;
begin
 Result := TStepList.Create;
 for i:=0 to Self.Count-1 do begin
   step := TSequenceStep(Self.Items[i]);
   tp := step.TimePos;
   step.CmdList.OneStepForOneCmd( Result, tp );
 end;
end;

function TTStepListHelper.ToCmdListOfSingleCmd: TCmdList;
var sl: TStepList;
begin
 sl := ToStepListOfSingleCmd;
 Result := sl.PackToCmdList;

 sl.FreeSteps;
 sl.Free;
end;

function TTStepListHelper.PackToCmdList: TCmdList;
var i: integer;
  timePos, deltaTime: single;
  sep: string;
  procedure AddToResult( aCmd: TSingleCmd );
  begin
   Result += sep + aCmd;
   sep := CMD_SEPARATOR;
  end;
begin
 Result := '';
 timePos := 0;
 sep := '';
 for i:=0 to Self.Count-1 do begin
  if Self.Items[i].TimePos = timePos
    then AddToResult( TSequenceStep(Self.Items[i]).CmdList )
    else begin
      deltaTime := Self.Items[i].TimePos - timePos;
      AddToResult( CmdWait( deltaTime ) );
      AddToResult( TSequenceStep(Self.Items[i]).CmdList );
      timePos := Self.Items[i].TimePos;
    end;
 end;
end;

procedure TTStepListHelper.ToStepDataList(var OutString: TSequencerInfoList);
var i: Integer;
  sep: string;
  step: TSequenceStep;
begin
 if OutString = '' then
   sep := ''
 else
   sep := SEQUENCERINFO_SEPARATOR;

 for i:=0 to Self.Count-1 do begin
   step := Self.Items[i] as TSequenceStep;
   OutString += sep + step.Serialize;
   sep := SEQUENCERINFO_SEPARATOR;
 end;
end;

procedure TTStepListHelper.SelectedToStepDataList( var OutString: TSequencerInfoList);
var i: Integer;
  sep: string;
  step: TSequenceStep;
  timeBase: single;
begin
 if OutString = '' then
   sep := ''
 else
   sep := SEQUENCERINFO_SEPARATOR;

 timeBase := -1;
 for i:=0 to Self.Count-1 do begin
   step := Self.Items[i] as TSequenceStep;
   if step.Selected then begin
     if timeBase<0
       then timeBase := step.TimePos;
     step.TimePos := step.TimePos - timeBase;
     OutString += sep + step.Serialize;
     step.TimePos := step.TimePos + timeBase;
     sep := SEQUENCERINFO_SEPARATOR;
   end;
 end;
end;

function TTStepListHelper.Duration: single;
var s: TSequenceStep;
  i: integer;
begin
  Result:=0.0;
  for i:=0 to Count-1 do begin
    s:=TSequenceStep(Items[i]);
    if Result<s.TimePos+s.Duration
      then Result:=s.TimePos+s.Duration;
  end;
end;

procedure TTStepListHelper.AddCmdWait(aTimePos, aDuration: single);
var s: TSequenceStep;
begin
  s := TSequenceStep.Create;
  s.CmdList := CmdWait(aDuration);
  s.TimePos := aTimePos;
  s.Duration := aDuration;
  Add(s);
end;

procedure TTStepListHelper.AddCmdLoop(aTimePos: single);
var s: TSequenceStep;
begin
  s := TSequenceStep.Create;
  s.CmdList := CmdLoop;
  s.TimePos := aTimePos;
  s.Duration := 0;
  Add(s);
end;

procedure TTStepListHelper.AddCmdDMXDimmer(aTimePos: single;
  aChan: TDMXChannel; aPercent, aDuration: single; aCurveID: word);
var s: TSequenceStep;
begin
  s := TSequenceStep.Create;
  s.CmdList := CmdDMXDimmer(aChan.Universe.ID, aChan.Fixture.ID, aChan.Index, aPercent, aDuration, aCurveID);
  s.TimePos := aTimePos;
  s.Duration := aDuration;
  Add(s);
end;

procedure TTStepListHelper.AddCmdWave(aTimePos: single; aChan: TDMXChannel;
  aPercent1, aDuration1: single; aCurveID1: word; aKeepTime, aPercent2,
  aDuration2: single; aCurveID2: word);
var s: TSequenceStep;
begin
  s := TSequenceStep.Create;
  s.CmdList := CmdDMXInternalWave(aChan.Universe.ID, aChan.Fixture.ID, aChan.Index,
                                aPercent1, aDuration1, aCurveID1,
                                aKeepTime,
                                aPercent2, aDuration2, aCurveID2);
  s.TimePos := aTimePos;
  s.Duration := aDuration1+aDuration2;
  Add(s);
end;

procedure TTStepListHelper.AddCmdDMXDimmerRGB(aTimePos: single;
  aFix: TDMXFixture; aColor: TColor; aDuration: single; aCurveID: word);
var s: TSequenceStep;
begin
  s := TSequenceStep.Create;
  s.CmdList := CmdDMXDimmerRGB(aFix.Universe.ID, aFix.ID, aColor, aDuration, aCurveID);
  s.TimePos := aTimePos;
  s.Duration := aDuration;
  Add(s);
end;

{ TSplittedCmdsHelper }

function TSplittedCmdsHelper.MergeToString(aCharSeparator: char; index: integer ): string;
var i: integer;
begin
 Result := '';
 if index > Length(Self) then exit;
 for i:=index to Length(Self)-1 do
  if i = index then
    Result := Self[i]
  else
    Result := Result + aCharSeparator + Self[i];
end;

function TSplittedCmdsHelper.PackToCmd: TSingleCmd;
begin
 Result := MergeToString( PARAM_SEPARATOR, 0 );
end;

function TSplittedCmdsHelper.PackToCmdList: TCmdList;
begin
 Result := MergeToString( CMD_SEPARATOR, 0 );
end;

procedure TSplittedCmdsHelper.MultiplyAllPauseByCoeff(aCoef: double);
var i: integer;
  waitValue: single;
begin
 for i:=0 to Length(Self)-1 do
  if Self[i].IsWait(waitValue)
    then Self[i] := CmdWait(waitValue*aCoef);
end;

procedure TSplittedCmdsHelper.SetCmdDuration(aDuration: single);
var i, index: integer;
  A: TParamArray;
  strduration: string;
  dur1, dur2, keep, v: single;
begin
  strduration := FormatFloatWithDot('0.000', aDuration);

  for i:=0 to Length(Self)-1 do begin
    A := Self[i].SplitToParamArray;
    case A[0].ToInteger of
      CMD_WAIT,  // CMD_WAIT DurationF
      TITLECMD_DMX_DIMMER:  // TITLECMD_DMX_DIMMER Duration CurveID
        index := 1;

      CMD_AUDIO_FADEOUT,  // AUDIOFADEOUT IDaudio duration IDcurve
      CMD_AUDIO_CAPTURE_SETVOLUME, // CMD_AUDIO_CAPTURE_SETVOLUME volume duration IDcurve
      CMD_AUDIO_CAPTURE_SETPAN, // CMD_AUDIO_CAPTURE_SETPAN  panning duration IDcurve
      TITLECMD_DMX_DIMMERRGB:  // TITLECMD_DMX_DIMMERRGB Color Duration CurveID
        index := 2;

      CMD_AUDIO_FADEIN,    // AUDIOFADEIN IDaudio volume duration IDcurve
      CMD_AUDIO_SETVOLUME, // AUDIOFIXEVOLUME IDaudio volume duration IDcurve
      CMD_AUDIO_SETPAN,  // AUDIOFIXEPAN IDaudio panning duration IDcurve
      CMD_AUDIO_SETPITCH,  // AUDIOFIXEFREQ IDaudio frequence duration IDcurve
      CMD_SEQUENCESTRETCHTIME:  // CMD_SEQUENCESTRETCHTIME IDSeq StretchValueF DurationF CurveID
         index := 3;

      CMD_DMX_DIMMERRGB:  // CMD_DMX_DIMMERRGB IDuniverse IDFixture Color Duration CurveID
         index := 4;

      CMD_DMX_DIMMER:  // CMD_DMX_DIMMER IDuniverse IDFixture ChanIndex PercentF DurationF CurveID
         index := 5;

      CMD_DMX_FLASH: begin // CMD_DMX_FLASH IDuniverse IDFixture ChanIndex LevelMin LevelMax DurationMin DurationMax
        if A[6] = A[7] then A[6] := strduration;
        A[7] := strduration;
        index := -1;
        Self[i] := A.PackToCmd;
      end;
      TITLECMD_DMX_FLASH: begin // TITLECMD_DMX_FLASH LevelMin LevelMax DurationMin DurationMax
        if A[3] = A[4] then A[3] := strduration;
        A[4] := strduration;
        index := -1;
        Self[i] := A.PackToCmd;
      end;

      TITLECMD_DMX_FLASHRGB: begin // TITLECMD_DMX_FLASHRGB Color pcMin pcMax DurationMin DurationMax
        if A[4] = A[5] then A[4] := strduration;
        A[5] := strduration;
        index := -1;
        Self[i] := A.PackToCmd;
      end;
      CMD_DMX_FLASHRGB: begin // CMD_DMX_FLASHRGB IDuniverse IDFixture Color pcMin pcMax DurationMin DurationMax
        if A[6] = A[7] then A[6] := strduration;
        A[7] := strduration;
        index := -1;
        Self[i] := A.PackToCmd;
      end;
      CMD_INTERNALDMXWAVE: begin //INTERNALDMXWAVE IDuniverse IDFixture ChanIndex
                                    //                Percent1 Duration1 CurveID1
                                    //                KeepTime
                                    //                Percent2 Duration2 CurveID2
        dur1 := StringToSingle(A[5]);
        dur2 := StringToSingle(A[9]);
        keep := StringToSingle(A[7]);
        v := dur1;
        if v < dur2 then v := dur2;
        if v < keep then v := keep;
        if v <> 0 then v := aDuration/v;
        A[5] := FormatFloatWithDot('0.000', dur1*v);
        A[9] := FormatFloatWithDot('0.000', dur2*v);
        A[7] := FormatFloatWithDot('0.000', keep*v);
        index := -1;
        Self[i] := A.PackToCmd;
      end;
    end;//case

    if index <> -1 then begin
      A[index] := strduration;
      Self[i] := A.PackToCmd;
    end;
  end;
 end;

procedure TSplittedCmdsHelper.MultiplyAllDurationByCoeff(aCoef: double);
var i: integer;
  A: TParamArray;
begin
 for i:=0 to Length(Self)-1 do begin
   A := Self[i].SplitToParamArray;
   case A[0].ToInteger of
    CMD_WAIT: Self[i] := CmdWait(StringToSingle(A[1])*aCoef);
    //CMD_GRADATEUR2: Self[i] := CmdGradateur2(A[1].ToInteger, A[2].ToSingle*aCoef, A[3].ToSingle);
    //CMD_GRADARVB: Self[i] := CmdGradaRVB(A[1].ToInteger, A[2].ToInteger, A[3].ToSingle*aCoef);
    end;//case
 end;
end;

function TSplittedCmdsHelper.ParamArray0ToCmd: integer;
var v: longint;
begin
  if TryStrToInt(Self[0], v)
    then Result := v
    else Result := CMD_UNKNOW;
end;

function TSplittedCmdsHelper.RePackToString(aFromIndex: integer; const aSeparator: string): String;
var t: string;
  i: integer;
begin
  Result := '';
  t := '';
  for i:=aFromIndex to High(Self) do
  begin
   Result := Result+Self[i]+t;
   if i = aFromIndex then t := aSeparator;
  end;
end;

{ TCmdListHelper }

function TCmdListHelper.SplitToArray( aCharSeparator: char ): TStringArray;
var k, i: integer;
  function EndOfString: boolean;
  begin
   Result := k>Self.Length;
  end;
  function CurrentIsSeparator: boolean;
  begin
   Result := Self[k] = aCharSeparator;
  end;

  procedure SkipOneSeparator;
  begin
   if EndOfString
     then exit
      else if Self[k] = aCharSeparator
            then inc(k);
  end;
  procedure SkipSeparator;
  begin
   repeat
    if EndOfString
      then exit
      else if Self[k] = aCharSeparator
            then inc(k)
            else exit;
   until false;
  end;
  procedure GetWord;
  begin
   inc(i);
   SetLength( Result, i+1 );
   Result[i] := '';
   if EndOfString then exit;
   if CurrentIsSeparator then exit;
   repeat
    Result[i] := Result[i]+Self[k];
    inc(k);
    if EndOfString then exit;
   until Self[k] = aCharSeparator;
  end;
begin
 Result := self.Split([aCharSeparator]);
 exit;



 SetLength( Result{%H-}, 0 );
 if Self.Length = 0 then exit;

 i := -1;
 k := 1;
 GetWord;
 repeat
  SkipOneSeparator;
  GetWord;
 until EndOfString;
end;

function TCmdListHelper.GetOnlyCmd: integer;
var A: TParamArray;
begin
 if Self = '' then
   Result := CMD_UNKNOW
 else begin
  A := Self.SplitToParamArray;
  Result := A[0].ToInteger;
 end;
end;

function TCmdListHelper.IsSingleCmd: boolean;
begin
 Result := Pos( CMD_SEPARATOR, Self )=0;
end;

function TCmdListHelper.IsTitle: boolean;
begin
  Result := Self.GetOnlyCmd >= 1000;
end;

function TCmdListHelper.IsWait(out sec: single): boolean;
var A: TParamArray;
begin
 A := Self.SplitToParamArray;
 Result := FALSE;
 if High(A) = 1 then begin
   Result := A[0] = CMD_WAIT.ToString;
   if Result then sec := StringToSingle(A[1]);
 end;
end;

procedure TCmdListHelper.ChangeParamFromTitleParam(const aTitleCmd: TSingleCmd);
var A, B: TParamArray;
  cmd: integer;
begin
  if not aTitleCmd.IsSingleCmd or not aTitleCmd.IsTitle then exit;

  A := Self.SplitToParamArray;
  B := aTitleCmd.SplitToParamArray;
  if TryStrToInt(B[0], cmd) then
    case cmd of
      TITLECMD_AUDIO_PLAY:;
      TITLECMD_AUDIO_STOP:;
      TITLECMD_AUDIO_PAUSE:;
      TITLECMD_AUDIO_FADEIN:;
      TITLECMD_AUDIO_FADEOUT:;
      TITLECMD_AUDIO_SETVOLUME:;
      TITLECMD_AUDIO_SETPAN:;
      TITLECMD_AUDIO_SETPITCH:;
      TITLECMD_AUDIO_APPLYFX:;

      TITLECMD_DMX_DIMMER: begin  // TITLECMD_DMX_DIMMER Duration CurveID
                                  // CMD_DMX_DIMMER IDuniverse IDFixture ChanIndex PercentF DurationF CurveID
        A[5] := B[1];
        A[6] := B[2];
      end;
      TITLECMD_DMX_FLAME: begin  // TITLECMD_DMX_FLAME  LevelMin LevelMax Speed Soften
                                 // CMD_DMX_FLAME IDuniverse IDFixture ChanIndex LevelMin LevelMax Speed Soften
        A[4] := B[1];
        A[5] := B[2];
        A[6] := B[3];
        A[7] := B[4];
      end;
      TITLECMD_DMX_STOPEFFECT:;
      TITLECMD_DMX_COPYCHANNEL:;
      TITLECMD_DMX_AUDIOFOLLOWER: begin // TITLECMD_DMX_AUDIOFOLLOWER IDaudio Gain MaxPercent SoftenTime
                                        // CMD_DMX_AUDIOFOLLOWER IDuniverse IDFixture ChanIndex IDaudio Gain MaxPercent SoftenTime
        A[4] := B[1];
        A[5] := B[2];
        A[6] := B[3];
        A[7] := B[4];
      end;
      TITLECMD_DMX_FLASH: begin // TITLECMD_DMX_FLASH LevelMin LevelMax DurationMin DurationMax
                                // CMD_DMX_FLASH IDuniverse IDFixture ChanIndex LevelMin LevelMax DurationMin DurationMax
        A[4] := B[1];
        A[5] := B[2];
        A[6] := B[3];
        A[7] := B[4];
      end;
      TITLECMD_DMX_DIMMERRGB: begin  // TITLECMD_DMX_DIMMERRGB Color Duration CurveID
                                     // CMD_DMX_DIMMERRGB IDuniverse IDFixture Color Duration CurveID
        A[3] := B[1];
        A[4] := B[2];
        A[5] := B[3];
      end;
      TITLECMD_DMX_FLAMERGB: begin // TITLECMD_DMX_FLAMERGB Color Speed Amplitude Soften
                                   // CMD_DMX_FLAMERGB IDuniverse IDFixture Color Speed Amplitude Soften
      end;
      TITLECMD_DMX_AUDIOFOLLOWERRGB: begin // TITLECMD_DMX_AUDIOFOLLOWERRGB IDaudio Color Gain SoftenTime
                                           // CMD_DMX_AUDIOFOLLOWERRGB IDuniverse IDFixture IDaudio Color Gain SoftenTime
        A[3] := B[1];
        A[4] := B[2];
        A[5] := B[3];
        A[6] := B[4];
      end;
      TITLECMD_DMX_STOPEFFECTRGB:;
      TITLECMD_DMX_COPYRGB:;


      TITLECMD_DMX_FLASHRGB: begin // TITLECMD_DMX_FLASHRGB Color pcMin pcMax DurationMin DurationMax
                                   // CMD_DMX_FLASHRGB IDuniverse IDFixture Color pcMin pcMax DurationMin DurationMax
        A[3] := B[1];
        A[4] := B[2];
        A[5] := B[3];
        A[6] := B[4];
        A[7] := B[5];
      end;
    end;
  Self := A.PackToCmd;
end;

function TCmdListHelper.SplitToParamArray: TParamArray;
begin
 Result := Self.SplitToArray( PARAM_SEPARATOR );
end;

procedure TCmdListHelper.ConcatCmd(aCmd: TSingleCmd);
begin
  if aCmd.Length = 0 then exit;
  if Self.Length = 0
    then Self := aCmd
    else Self := Self+CMD_SEPARATOR+aCmd;
end;

procedure TCmdListHelper.ConcatCmdList(aCmdList: TCmdList);
begin
  ConcatCmd( aCmdList );
end;

function TCmdListHelper.SplitToCmdArray: TCmdArray;
begin
 Result := Self.SplitToArray( CMD_SEPARATOR );
end;

function TCmdListHelper.ComputeCmdListDuration: single;
var cmds: TCmdList;
  A: TCmdArray;
  cmd: TSingleCmd;
  B: TParamArray;
  sl: TStepList;
  pos: single;
  step: TSequenceStep;
   procedure AddstepAtPos( aPos: single );
  begin
   step := TSequenceStep.Create;
   step.TimePos := aPos;
   step.CmdList:=CmdLoop;
   sl.Add(step);
  end;
begin
 Result:=0;
 cmds := Self;
 // rajoute une commande sans durée (CMD_FIN) pour marquer une éventuelle pause de fin
 cmds.ConcatCmd(CmdLoop);
 // éclate en tableau de Cmd
 A := cmds.SplitToCmdArray;
 cmds := '';

 sl:=TStepList.Create;
 pos:=0;
 // on distribue les cmd dans le StepList
 // et on rajoute une commande sans durée (CMD_FIN) à la fin de chaque step avec durée
  for cmd in A do begin
    B:=cmd.SplitToParamArray;
    case B[0].ToInteger of

      CMD_WAIT:  // CMD_WAIT DurationF
//      TITLECMD_DMX_DIMMER: // TITLECMD_DMX_DIMMER Duration CurveID
         pos+= StringToSingle(B[1]);

      CMD_AUDIO_FADEOUT,  // AUDIOFADEOUT IDaudio duration IDcurve
      CMD_AUDIO_CAPTURE_SETVOLUME, // CMD_AUDIO_CAPTURE_SETVOLUME volume duration IDcurve
      CMD_AUDIO_CAPTURE_SETPAN: // CMD_AUDIO_CAPTURE_SETPAN  panning duration IDcurve
//      TITLECMD_DMX_DIMMERRGB:  // TITLECMD_DMX_DIMMERRGB Color Duration CurveID
        pos+= StringToSingle(B[2]);

      CMD_AUDIO_FADEIN,    // AUDIOFADEIN IDaudio volume duration IDcurve
      CMD_AUDIO_SETVOLUME, // AUDIOFIXEVOLUME IDaudio volume duration IDcurve
      CMD_AUDIO_SETPAN,  // AUDIOFIXEPAN IDaudio panning duration IDcurve
      CMD_AUDIO_SETPITCH,  // AUDIOFIXEFREQ IDaudio frequence duration IDcurve
      CMD_SEQUENCESTRETCHTIME:  // CMD_SEQUENCESTRETCHTIME IDSeq StretchValueF DurationF CurveID
         pos+= StringToSingle(B[3]);

      CMD_DMX_DIMMERRGB:  // CMD_DMX_DIMMERRGB IDuniverse IDFixture Color Duration CurveID
         pos+= StringToSingle(B[4]);

      CMD_DMX_DIMMER:  // CMD_DMX_DIMMER IDuniverse IDFixture ChanIndex PercentF DurationF CurveID
         pos+= StringToSingle(B[5]);

      CMD_DMX_FLASH, // CMD_DMX_FLASH IDuniverse IDFixture ChanIndex LevelMin LevelMax DurationMin DurationMax
      CMD_DMX_FLASHRGB: // CMD_DMX_FLASHRGB IDuniverse IDFixture Color pcMin pcMax DurationMin DurationMax
         pos+= StringToSingle(B[7]);

//      TITLECMD_DMX_FLASH: // TITLECMD_DMX_FLASH LevelMin LevelMax DurationMin DurationMax
//         pos+= StringToSingle(B[4]);

//      TITLECMD_DMX_FLASHRGB: // TITLECMD_DMX_FLASHRGB Color pcMin pcMax DurationMin DurationMax
//         pos+= StringToSingle(B[5]);

      CMD_INTERNALDMXWAVE: //INTERNALDMXWAVE IDuniverse IDFixture ChanIndex
                                    //                Percent1 Duration1 CurveID1
                                    //                KeepTime
                                    //                Percent2 Duration2 CurveID2
        pos := pos + StringToSingle(A[5]) + StringToSingle(A[7]) + StringToSingle(A[9]);

      else AddstepAtPos( pos );
    end;//case
  end;
 sl.Sort;
 if sl.Count > 0
   then Result := sl.Last.TimePos;

 sl.FreeSteps;
 sl.Free;
end;

function TCmdListHelper.SplitToStepDataArray: TStepDataArray;
begin
 Result := Self.SplitToArray( STEPDATA_SEPARATOR );
end;

function TCmdListHelper.SplitToSequencerInfoArray: TSequencerInfoArray;
begin
 Result := Self.SplitToArray( SEQUENCERINFO_SEPARATOR );
end;

function TCmdListHelper.SequencerInfoListToCmdListOfSingleCmd: string;
var A: TSequencerInfoArray;
  temp: TStepList;
  step: TSequenceStep;
  tp: single;
  i: Integer;
begin
 Result := '';
 A := Self.SplitToSequencerInfoArray;
 if High(A)=-1 then exit;

 step := TSequenceStep.Create;
 temp:= TStepList.Create;
 for i:=2 to High(A) do begin  // skip ID and GroupValue
  step.Deserialize( A[i] );
  tp := step.TimePos;
  step.CmdList.OneStepForOneCmd( temp, tp );
 end;
 temp.Sort;
 SetLength(A, 0 ); // free unused memory
 step.Free;

 Result := temp.PackToCmdList;

 temp.FreeSteps;
 temp.Free;
end;

function TCmdListHelper.SequencerInfoListIsLooped: boolean;
var
  cmds: String;
  A: TCmdArray;
  cmdLoop: string;
  i: integer;
begin
  cmds := Self.SequencerInfoListToCmdListOfSingleCmd;
  A := cmds.SplitToCmdArray;
  cmds := '';
  cmdLoop := CMD_LOOP.ToString;

  for i:=0 to High(A) do
   if A[i] = cmdLoop then
   begin
     Result := True;
     exit;
   end;

  Result := False;
end;

procedure TCmdListHelper.OneStepForOneCmd(aSLTarget: TStepList; var aTimePos: single );
var A: TCmdArray;
  i: Integer;
  step: TSequenceStep;
  tp: single;
  B: TParamArray;
const _deltaTime = 0.00001;
begin
 // a CmdList can be:
 //    - a single Cmd -> wait or any basic action
 //    - a list of Cmd -> explore the list using recursivity

 tp := aTimePos;
 A := Self.SplitToCmdArray;
 for i:=0 to High(A) do
 begin
  if not A[i].IsSingleCmd then
    A[i].OneStepForOneCmd( aSLTarget, tp ) // explore the list using recursivity
  else
  begin
    B:=A[i].SplitToParamArray;
    if B[0]=CMD_WAIT.ToString then
      tp := tp+StringToSingle(B[1])
    else
    begin
      // it's a basic action
      step:= TSequenceStep.Create;
      step.TimePos := tp;
      step.CmdList := A[i];
      aSLTarget.Add( step );
tp := tp + _deltaTime;// Shift the action to keep them in the same order...!!
    end;
  end;
 end;
 aSLTarget.Sort;
end;


end.

