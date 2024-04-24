unit u_list_top;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fgl,
  u_common, VelocityCurve;


type
  { TSequence }

  TSequence=class
  private
    FSequencerInfoList: TSequencerInfoList;
    FIsLooped: boolean;
    procedure SetSequencerInfoList(AValue: TSequencerInfoList);
  //published
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
  end;


  { TTopList }

  TTopList=class(specialize TFPGObjectList<TSequence>)   //class(specialize TList<TSequence>)
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
    function GetTopByIndex(aIndex: integer): TSequence;

    function GetNameByID(aID: cardinal): string;
    function IDToIndex(aID: cardinal): integer;

    procedure Save(temp: TStrings);
    procedure Load(temp: TStrings);

    procedure StopAll;

    function Duplicate(aID: cardinal): TSequence;

    property ID: cardinal read FID write FID;
  end;

var
  Sequences: TTopList;

implementation

uses u_resource_string, u_helper, u_utils, u_logfile;

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

{
  FID := temp.Strings[k+1].ToInteger;    // ID value
  c := temp.Strings[k+2].ToInteger;    // count
  inc(k,3);
  while c > 0 do begin
    o := TSequence.Create;
    o.Name := temp.Strings[k];                 // name
    o.ID := temp.Strings[k+1].ToInteger;       // ID
    o.SequencerInfoList := temp.Strings[k+2];    // TSequencerInfoList
    Add(o);
    inc(k,3);
    dec(c);
  end;
}
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

