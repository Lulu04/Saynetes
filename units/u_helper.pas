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
  function IsSingleCmd: boolean;
  function IsTitle: boolean;
  function IsWait(out sec: single): boolean;
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
  // modifie les durées des pause, gradateur2 et gradaRVB
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
begin
  strduration := FormatFloat('0.000', aDuration);

  for i:=0 to Length(Self)-1 do begin
    A := Self[i].SplitToParamArray;
    case A[0].ToInteger of
     CMD_WAIT,
     TITLECMD_DMX_DIMMER: index := 1;

     CMD_AUDIO_FADEOUT,
     TITLECMD_DMX_DIMMERRGB,
     TITLECMD_DMX_FLASH,
     TITLECMD_DMX_FLASHRGB,
     CMD_AUDIO_CAPTURE_SETVOLUME,
     CMD_AUDIO_CAPTURE_SETPAN: index := 2;

     CMD_AUDIO_FADEIN,
     CMD_AUDIO_SETVOLUME,
     CMD_AUDIO_SETPAN,
     CMD_AUDIO_SETPITCH,
     CMD_SEQUENCESTRETCHTIME: index := 3;

     CMD_DMX_DIMMERRGB,
     CMD_DMX_FLASHRGB: index := 4;

     CMD_DMX_DIMMER,
     CMD_DMX_FLASH: index := 5;

     else index := -1;
    end;//case

    if index <> -1 then
    begin
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

function TCmdListHelper.IsSingleCmd: boolean;
begin
 Result := Pos( CMD_SEPARATOR, Self )=0;
end;

function TCmdListHelper.IsTitle: boolean;
var A: TParamArray;
begin
 if Self = '' then
   Result := FALSE
 else begin
  A := Self.SplitToParamArray;
  Result := A[0].ToInteger >= 1000;
 end;
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
      //CMD_GRADATEUR2: AddstepAtPos( pos+B[2].ToSingle );
      //CMD_GRADARVB: AddstepAtPos( pos+B[3].ToSingle );
      CMD_WAIT: pos+= StringToSingle(B[1]);
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

