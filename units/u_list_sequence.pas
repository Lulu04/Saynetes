unit u_list_sequence;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fgl,
  u_common, VelocityCurve;


type
  TSequenceList = class;
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
     procedure Update(const aElapsedTime: single);
     procedure LoopToBegin;
     procedure Stop;
     procedure NextLine;
     function EndOfPlay: boolean;
     function Duplicate: TSequence;

     property SequencerInfoList: TSequencerInfoList read FSequencerInfoList write SetSequencerInfoList;
     property IsLooped: boolean read FIsLooped;

  public
     // scan the actions in the SequencerInfoList property and return true if there is an error
     // like dmx fixture not found, audio not found, sequence not found...
     // the two following property are initilized
     function CheckError(aParentList: TSequenceList): boolean;
     property HaveError: boolean read FHaveError write FHaveError;
     property ErrorMessage: string read FErrorMessage write FErrorMessage;
  end;


  { TSequenceList }

  TSequenceList = class(specialize TFPGObjectList<TSequence>)   //class(specialize TList<TSequence>)
  private
    FID: cardinal;
  public
    procedure ClearAll;
    function NextID: cardinal;

    function NameAlreadyExists(const aName: string): boolean;
    function IsValidIndex(aIndex: integer): boolean;

    function AddSequence(const aName: string; const aSequencerInfoList: TSequencerInfoList): TSequence;
    function InsertSequence(aIndex: integer; const aName: string; const aSequencerInfoList: TSequencerInfoList): TSequence;
    function GetSequenceByID(aID: cardinal): TSequence;
    function GetSequenceByStrID(const aStrID: string): TSequence;
    function GetSequenceByIndex(aIndex: integer): TSequence;
    function GetSequenceByName(const aName: string): TSequence;

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
  Sequences: TSequenceList;

implementation

uses u_resource_string, u_helper, u_logfile, u_sequence_player,
  ALSound, PropertyUtils;

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
  CmdArray := SequencerInfoList.SplitToCmdArray;
  LineIndex := 0;
  WaitSec := 0.0;
  Clock := 0.0;
  if Length(CmdArray) > 0
    then Running := TRUE;
end;

procedure TSequence.RunAsSequencerInfoList;
var cmds: TCmdList;
begin
  cmds := SequencerInfoList.SequencerInfoListToCmdListOfSingleCmd;
  CmdArray := cmds.SplitToCmdArray;
  LineIndex := 0;
  WaitSec := 0.0;
  Clock := 0.0;
  if Length(CmdArray) > 0
    then Running := TRUE;
end;

procedure TSequence.Update(const aElapsedTime: single);
var loopAlreadyDone: boolean;
  A: TStringArray;
  cmd: Integer;
begin
  TimeStretchFactor.OnElapse(aElapsedTime); // update strech factor value
  if not Running or (Length(CmdArray) = 0) then exit;

  Clock := Clock + aElapsedTime * TimeStretchFactor.Value;
  if WaitSec > 0 then
    WaitSec := WaitSec - (aElapsedTime * TimeStretchFactor.Value);
  if WaitSec <= 0 then begin
    loopAlreadyDone := False;
    repeat
      if EndOfPlay then break;
      A := CmdArray[LineIndex].SplitToParamArray; // read and split one cmd
      if (Length(A) > 0) and TryStrToInt(A[0], cmd) then
        case cmd of
          CMD_WAIT: begin
            WaitSec += StringToSingle(A[1]); // trick to take in account the small pause value shortest than deltaMS
            loopAlreadyDone := False;
          end;
          CMD_LOOP: begin
            if loopAlreadyDone then WaitSec := 10 // avoid infinite loop in case of sequence with only loop action.
              else LoopToBegin;
            loopAlreadyDone := True;
          end
          else begin
            ExecuteCmd(A);
            //Log.Debug('execute "'+CmdArray[LineIndex]+'"');
          end;
        end;
      NextLine;
    until EndOfPlay or (WaitSec > 0);
    if EndOfPlay then Stop;
  end;
end;

procedure TSequence.LoopToBegin;
begin
  LineIndex := -1;
  Clock := 0.0;
end;

procedure TSequence.Stop;
begin
  Running := FALSE;
end;

procedure TSequence.NextLine;
begin
  inc(LineIndex);
end;

function TSequence.EndOfPlay: boolean;
begin
  Result:= LineIndex >= Length(CmdArray);
end;

function TSequence.Duplicate: TSequence;
begin
  Result := TSequence.Create;
  Result.Name := Name;
  Result.ID := ID;
  Result.SequencerInfoList := SequencerInfoList;
  Result.FIsLooped := FIsLooped;
  Result.FErrorMessage := FErrorMessage;
  Result.FHaveError := FHaveError;
end;

function TSequence.CheckError(aParentList: TSequenceList): boolean;
var cmds: TCmdList;
    haveDuration: boolean;

  function ErrorOnCmd(const aCmd: string; out errMess: string): boolean;
  var A: TParamArray;
    B: TCmdArray;
    j: integer;
  begin
    Result := False;
    errMess := '';
    if not aCmd.IsSingleCmd then begin
      // its a complex action
      B := aCmd.SplitToCmdArray;
      for j:=0 to High(B) do begin
        if ErrorOnCmd(B[j], errMess) then Result := True;
        if Result then exit;
      end;
    end else begin
      // its a single action
      A := aCmd.SplitToParamArray;
      Result := A.ParamArrayHaveError(errMess, haveDuration);
    end;
  end;

begin
  HaveError := False;
  ErrorMessage := '';
  haveDuration := False;

  if SequencerInfoList = '' then begin
    Log.Warning('Sequence "'+Name+'" is empty', 1);
    exit(True);
  end;

  cmds := SequencerInfoList.SequencerInfoListToCmdListOfSingleCmd;
  HaveError := cmds.HaveError(FErrorMessage, haveDuration);
  Result := HaveError;
  if Result then Log.Warning('Error in "'+Name+'": '+FErrorMessage, 3);
end;

{ TSequenceList }

procedure TSequenceList.ClearAll;
begin
  Clear;
  FID := 0;
end;

function TSequenceList.NextID: cardinal;
begin
  inc(FID);
  Result := FID;
end;

function TSequenceList.GetSequenceByIndex(aIndex: integer): TSequence;
begin
  if (aindex < 0) or (aindex >= Count)
    then Result := NIL
    else Result := Items[aindex];
end;

function TSequenceList.GetSequenceByName(const aName: string): TSequence;
var i: integer;
begin
  for i:=0 to self.Count-1 do
    if TSequence(Items[i]).Name = aName then begin
      Result := TSequence(Items[i]);
      exit;
    end;
  Result := NIL;
end;

function TSequenceList.NameAlreadyExists(const aName: string): boolean;
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

function TSequenceList.IsValidIndex(aIndex: integer): boolean;
begin
 Result := (aIndex >= 0) and (aIndex < Count) and (Count > 0);
end;

function TSequenceList.AddSequence(const aName: string; const aSequencerInfoList: TSequencerInfoList): TSequence;
begin
  Result := TSequence.Create;
  Result.Name := aName;
  Result.SequencerInfoList := aSequencerInfoList;
  Result.ID := NextID;
  Add(Result);
end;

function TSequenceList.InsertSequence(aIndex: integer; const aName: string;
  const aSequencerInfoList: TSequencerInfoList): TSequence;
begin
  Result := TSequence.Create;
  Result.Name := aName;
  Result.SequencerInfoList := aSequencerInfoList;
  Result.ID := NextID;
  Insert(aIndex, Result);
end;

function TSequenceList.GetSequenceByID(aID: cardinal): TSequence;
var i: Integer;
begin
  for i:=0 to Count-1 do
   if GetSequenceByIndex(i).ID = aID then begin
     Result := GetSequenceByIndex(i);
     exit;
   end;
  Result := NIL;
end;

function TSequenceList.GetSequenceByStrID(const aStrID: string): TSequence;
var i: integer;
begin
  if TryStrToInt(aStrID, i) then Result := GetSequenceByID(i)
    else Result := NIL;
end;

function TSequenceList.GetNameByID(aID: cardinal): string;
var i: integer;
begin
 for i:=0 to Count-1 do
  if GetSequenceByIndex(i).ID = aID then begin
    Result := GetSequenceByIndex(i).Name;
    exit;
  end;

 Result := SUnknowSequence;
end;

function TSequenceList.IDToIndex(aID: cardinal): integer;
var i: integer;
begin
  for i:=0 to Count-1 do
   if GetSequenceByIndex(i).ID = aID then begin
     Result := i;
     exit;
   end;
  Result := -1;
end;

const SEQUENCE_HEADER = '[SEQUENCE]';
procedure TSequenceList.Save(temp: TStrings);
var i: Integer;
  prop: TProperties;
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
end;

procedure TSequenceList.Load(temp: TStrings);
var c, k, vi: integer;
  o: TSequence;
  prop: TProperties;
  s1, s2: string;
  flagError: boolean;
  procedure LogMissingProperty(const apropName: string);
  begin
    Log.Error('TSequenceList.Load - Property '+apropName+' not found', 3);
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

function TSequenceList.CheckErrorInSequences: boolean;
var i: integer;
  seq: TSequence;
begin
  Result := False;
  for i:=0 to Count-1 do begin
    seq := GetSequenceByIndex(i);
    if seq.CheckError(Self) then Result := True;
  end;
end;

procedure TSequenceList.StopAll;
var i: integer;
begin
  for i:=0 to Count-1 do
    GetSequenceByIndex(i).Stop;
end;

function TSequenceList.Duplicate(aID: cardinal): TSequence;
var source: TSequence;
  i :integer;
  txt: string;
begin
  Result := NIL;
  source := GetSequenceByID(aID);
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

