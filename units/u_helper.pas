unit u_helper;

{$mode objfpc}{$H+}
{$modeswitch TypeHelpers}

interface

uses
  Classes, SysUtils, Graphics, StdCtrls,
  u_common, u_list_dmxuniverse,
  frame_bglvirtualscreen_sequencer;

type

{ TCmdListHelper }

TCmdListHelper = type helper(TStringHelper) for TCmdList
  function SplitToArray(aCharSeparator: char): TStringArray;
public
  function GetOnlyCmd: integer;
  function IsSingleCmd: boolean;
  function IsTitle: boolean;
  function IsWait(out sec: single): boolean;
  function IsAudioPlay(out aAudioID: integer): boolean; // title + action
  procedure ChangeParamFromTitleParam(const aTitleCmd: TSingleCmd);
  function SplitToParamArray: TParamArray;
  procedure ConcatCmd(aCmd: TSingleCmd);
  procedure ConcatCmdList(aCmdList: TCmdList);
  // scan the single command or the group of several commands and return True if an error is found,
  // errMess is actualized with an error message
  function HaveError(out errMess: string): boolean;
public
  function SplitToCmdArray: TCmdArray;
  function ComputeCmdListDuration: single;
  // pour chaque commande de la liste, on ajoute une étape au séquenceur cible
  // utile pour 'aplatir les cmds avant une exécution
  procedure OneStepForOneCmd( aSLTarget: TStepList; var aTimePos: single; var shiftTimeToKeepOrder: single);
public
  function SplitToStepDataArray: TStepDataArray;
public
  function SplitToSequencerInfoArray: TSequencerInfoArray;
  function SequencerInfoListToCmdListOfSingleCmd: string;
  function SequencerInfoListIsLooped: boolean;
end;

{ TSplittedCmdsHelper }

TSplittedCmdsHelper = type helper for TStringArray
  function MergeToString(aCharSeparator: char; index: integer=0): string;
  function PackToCmd: TSingleCmd;
  function PackToCmdList: TCmdList;
  procedure MultiplyAllPauseByCoeff(aCoef: double);
  // modify the duration of all actions
  procedure SetCmdDuration(aDuration: single);
  procedure MultiplyAllDurationByCoeff(aCoef: double);

  // converts the first item of the TParamArray in Cmd and return it.
  // Return CMD_UNKNOW if the conversion fails.
  function ParamArray0ToCmd: integer;
  // aFromIndex is 0 based
  function RePackToString(aFromIndex: integer; const aSeparator: string): String;

  // scan the comman parameters and return True if an error is found
  // errMess return an error message
  function ParamArrayHaveError(out errMess: string): boolean;
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

{ TArrayOfChannelRangeHelper }

TArrayOfChannelRangeHelper = type helper for TArrayOfChannelRange
  function GetRangeFromByteValue(aValue: byte): PChannelRange;
end;

{ TManufacturersHelper }

TManufacturersHelper = type helper for TManufacturers
  procedure Load;
  procedure AddAndSave(const aManufacturerName, aFolderName, aWebLink: string);
  procedure FillComboBoxWithName(aCB: TComboBox);
  function IndexOfName(const aManufacturerName: string): integer;
  function IndexOfFolder(const aManufacturerFolder: string): integer;
end;


{ TFixLibAvailableChannelsHelper }

TFixLibAvailableChannelsHelper = type helper for TFixLibAvailableChannels
  function GetChannelsByName(const aName: string): PFixLibAvailableChannel;
  function NameToIndex(const aName: string): integer;
  procedure ReplaceNameInSwitchDescriptors(const aOldName, aNewName: string);
end;


{ TWebLinksHelper }

TWebLinksHelper = type helper for TWebLinks
  procedure LoadFrom(t: TStringList);
  procedure SaveTo(t: TStringList);
  procedure CopyTo(var aTarget: TWebLinks);
end;


{ TVirtualChannelForSwitchsHelper }

TVirtualChannelForSwitchsHelper = type helper for TVirtualChannelForSwitchs
  function IndexOfVirtualName(const aVirtualName: string): integer;
  function IndexOfSubChannel(const aVirtualName, aSubChannel: string): integer;
  // we must have single instance of virtual name
  function AddVirtualName(const aVirtualChannelName: string): integer;
  // if aVirtualName doesn't exists in self, the function returns aVirtualName
  // else it returns a formatted string like: aVirtualName:SubChannel1;SubChannel2;...
  function FormatVirtualIfNeeded(const aVirtualName: string): string;
  // Split a formatted virtual name in its virtual name and sub-channel names
  // returns the parameters and True if sucess
  function TrySplitVirtual(const aFormattedVirtualName: string;
                           out aVirtualName: string;
                           out aSubChannels: TStringArray): boolean;
  // search aOldName in virtual name and sub-channelsID, if found, replace it by the new one
  procedure ReplaceChannelName(const aOldName, aNewName: string);
end;

{ TFixLibModesHelper }

TFixLibModesHelper = type helper for TFixLibModes
  function IndexOf(const aModeName: string): integer;
  function GetChannelUsed(const aModeName: string): TStringArray;
  procedure SaveModesTo(t: TStringList);
  function LoadModesFrom(t: TStringList): boolean;
end;

implementation

uses frame_sequencer, u_utils, u_apputils, u_logfile, u_resource_string,
  u_audio_manager, u_list_sequence, PropertyUtils, Math, ALSound, VelocityCurve;

{ TFixLibModesHelper }

function TFixLibModesHelper.IndexOf(const aModeName: string): integer;
var i: integer;
begin
  for i:=0 to High(Self) do
    if Self[i].Name = aModeName then exit(i);
  Result := -1;
end;

function TFixLibModesHelper.GetChannelUsed(const aModeName: string): TStringArray;
var i, j: integer;
begin
  Result := NIL;
  i := IndexOf(aModeName);
  if i = -1 then exit;
  SetLength(Result, Length(Self[i].ChannelsIDToUse));
  for j:=0 to High(Result) do
    Result[j] := Self[i].ChannelsIDToUse[j];
end;

procedure TFixLibModesHelper.SaveModesTo(t: TStringList);
var i: integer;
begin
  t.Add('[MODES]');
  for i:=0 to High(Self) do
    t.Add(Self[i].SaveToString);
  t.Add('[END_MODES]');
end;

function TFixLibModesHelper.LoadModesFrom(t: TStringList): boolean;
var i, k, kEnd: Integer;
begin
  Self := NIL;
  k := t.IndexOf('[MODES]');
  kEnd := t.IndexOf('[END_MODES]');

  if (k = -1) or (kEnd = -1) or (kEnd <= k) or (kEnd-k < 2) then begin
    Log.Error('Header block [MODES] [END_MODES] not found or empty', 1);
    exit(False);
  end;

  try
    SetLength(Self, kEnd-k-1);
    for i:=0 to High(Self) do begin
      inc(k);
      Self[i].LoadFromString(t.Strings[k]);
    end;
    Result := True;
  except
    Result := False;
  end;
end;

{ TVirtualChannelForSwitchsHelper }

function TVirtualChannelForSwitchsHelper.IndexOfVirtualName(const aVirtualName: string): integer;
var i: integer;
begin
  for i:=0 to High(Self) do
    if Self[i].VirtualName = aVirtualName then exit(i);
  Result := -1;
end;

function TVirtualChannelForSwitchsHelper.IndexOfSubChannel(const aVirtualName, aSubChannel: string): integer;
var i, j: integer;
begin
  i := IndexOfVirtualName(aVirtualName);
  if i = -1 then exit(-1);

  for j:=0 to High(Self[i].SubChannelIDs) do
    if Self[i].SubChannelIDs[j] = aSubChannel then exit(i);

  Result := -1;
end;

function TVirtualChannelForSwitchsHelper.AddVirtualName(const aVirtualChannelName: string): integer;
var i: integer;
begin
  i := IndexOfVirtualName(aVirtualChannelName);
  if i <> -1 then exit(i);

  i := Length(Self);
  SetLength(Self, i+1);
  Self[i].InitDefault;
  Self[i].VirtualName := aVirtualChannelName;
  Result := i;
end;

function TVirtualChannelForSwitchsHelper.FormatVirtualIfNeeded(const aVirtualName: string): string;
var i, j: integer;
begin
  Result := aVirtualName;

  i := IndexOfVirtualName(aVirtualName);
  if i = -1 then exit;

  Result := aVirtualName+':';
  for j:=0 to High(Self[i].SubChannelIDs) do begin
    Result := Result + Self[i].SubChannelIDs[j];
    if j < High(Self[i].SubChannelIDs) then Result := Result+';';
  end;

end;

function TVirtualChannelForSwitchsHelper.TrySplitVirtual(const aFormattedVirtualName: string;
  out aVirtualName: string; out aSubChannels: TStringArray): boolean;
var A: TStringArray;
begin
  A := aFormattedVirtualName.Split([':']);
  if Length(A) <> 2 then exit(False);
  aVirtualName := A[0];

  aSubChannels := A[1].Split([';']);
  if Length(aSubChannels) = 0 then exit(False);
  Result := True;
end;

procedure TVirtualChannelForSwitchsHelper.ReplaceChannelName(const aOldName, aNewName: string);
var n: TVirtualChannelForSwitch;
begin
  for n in Self do n.ReplaceChannelName(aOldName, aNewName);
end;

{ TArrayOfChannelRangeHelper }

function TArrayOfChannelRangeHelper.GetRangeFromByteValue(aValue: byte): PChannelRange;
var i: integer;
begin
  for i:=0 to High(Self) do
    if (aValue >= Self[i].BeginValue) and (aValue <= Self[i].EndValue) then begin
      Result := @Self[i];
      exit;
    end;
  Result := NIL;
end;

{ TWebLinksHelper }

procedure TWebLinksHelper.LoadFrom(t: TStringList);
var i, k: integer;
  prop: TProperties;
  s: string;
begin
  Self := NIL;
  k := t.IndexOf('[LINKS]');
  if (k = -1) or (k = t.Count-1) then exit;

  prop.Split(t.Strings[k+1], '|');
  // retrieve the number of links
  k := 0;
  repeat
   inc(k);
  until not prop.CheckProperties(['LinkType'+k.ToString]);

  SetLength(Self, k-1);
  s := '';
  for i:=0 to High(Self) do begin
    prop.StringValueOf('LinkType'+(i+1).ToString, s, '');
    Self[i].LinkType := s;
    prop.StringValueOf('Url'+(i+1).ToString, s, '');
    Self[i].Url := s;
  end;
end;

procedure TWebLinksHelper.SaveTo(t: TStringList);
var i: integer;
  prop: TProperties;
begin
  if Length(Self) = 0 then exit;

  prop.Init('|');
  for i:=0 to High(Self) do begin
    prop.Add('LinkType'+(i+1).ToString, Self[i].LinkType);
    prop.Add('Url'+(i+1).ToString, Self[i].Url);
  end;
  t.Add('[LINKS]');
  t.Add(prop.PackedProperty);
end;

procedure TWebLinksHelper.CopyTo(var aTarget: TWebLinks);
var i: integer;
begin
  aTarget := NIL;
  SetLength(aTarget, Length(Self));
  for i:=0 to High(Self) do
    Self[i].CopyTo(aTarget[i]);
end;

{ TFixLibAvailableChannelsHelper }

function TFixLibAvailableChannelsHelper.GetChannelsByName(const aName: string): PFixLibAvailableChannel;
var i: integer;
begin
  for i:=0 to High(Self) do
    if Self[i].NameID = aName then begin
      Result := @Self[i];
      exit;
    end;
  Result := NIL;
end;

function TFixLibAvailableChannelsHelper.NameToIndex(const aName: string): integer;
var i: integer;
begin
  for i:=0 to High(Self) do
    if Self[i].NameID = aName then exit(i);
  Result := -1;
end;

procedure TFixLibAvailableChannelsHelper.ReplaceNameInSwitchDescriptors(const aOldName, aNewName: string);
var chan: TFixLibAvailableChannel;
  rang: TFixLibSingleRange;
  i: integer;
  swi: PFixLibSwitchDescriptor;
begin
  for chan in Self do
    for rang in chan.Ranges do
      for i:=0 to High(rang.SwitchDescriptors) do begin
        swi := @rang.SwitchDescriptors[i];
        if swi^.SwitchVirtualChannel = aOldName then swi^.SwitchVirtualChannel := aNewName
          else if swi^.SwitchToSubChannel = aOldName then swi^.SwitchToSubChannel := aNewName;
      end;
end;

{ TManufacturersHelper }

procedure TManufacturersHelper.Load;
var t: TStringList;
  i, j: integer;
  prop: TProperties;
  nam, folder, web: string;
begin
  nam := '';
  folder := '';
  Self := NIL;
  t := TStringList.Create;
  try
    t.LoadFromFile(GetAppDMXLibraryFolder + MANUFACTURER_LIST);
    SetLength(Self, t.Count);
    j := 0;
    for i:=0 to t.Count-1 do begin
      prop.Split(t.Strings[i], '|');
      if prop.StringValueOf('Name', nam, '') and
         prop.StringValueOf('Folder', folder, '') then begin
        Self[j].Folder := folder;
        Self[j].Name := nam;
        if prop.StringValueOf('Web', web, '') then Self[j].WebSite := web
          else Self[j].WebSite := '';
        inc(j);
      end;
    end;
    if j < t.Count then SetLength(Self, j);
  finally
    t.Free;
  end;
end;

procedure TManufacturersHelper.AddAndSave(const aManufacturerName, aFolderName, aWebLink: string);
var t: TStringList;
  i: integer;
  rec: TFixtureManufacturer;
begin
  if Length(Self) = 0 then Load;
  if IndexOfName(aManufacturerName) <> -1 then exit;
  if IndexOfFolder(aFolderName) <> -1 then exit;

  // search the index where to insert the entry in the array to keep it sorted by manufacturer folder
  for i:=0 to High(Self)+1 do
    if i <= High(Self) then
      if CompareStr(Self[i].Folder, aFolderName) > 0 then break;

  // insert the new entry
  rec.Name := aManufacturerName;
  rec.Folder := aFolderName;
  rec.WebSite := aWebLink;
  Insert(rec, Self, i);

  // pack the array and save
  t := TStringList.Create;
  for i:=0 to High(Self) do
    t.Add('Folder|'+Self[i].Folder+'|Name|'+Self[i].Name+'|Web|'+Self[i].WebSite);
  try
    t.SaveToFile(GetAppDMXLibraryFolder + MANUFACTURER_LIST);
  finally
    t.Free;
  end;
end;

procedure TManufacturersHelper.FillComboBoxWithName(aCB: TComboBox);
var i: integer;
begin
  aCB.Clear;
  for i:=0 to High(Self) do
    aCB.Items.Add(Self[i].Name);
end;

function TManufacturersHelper.IndexOfName(const aManufacturerName: string): integer;
var i: integer;
begin
  for i:=0 to High(Self) do
    if Self[i].Name = aManufacturerName then exit(i);
  Result := -1;
end;

function TManufacturersHelper.IndexOfFolder(const aManufacturerFolder: string): integer;
var i: integer;
begin
  for i:=0 to High(Self) do
    if Self[i].Folder = aManufacturerFolder then exit(i);
  Result := -1;
end;


{ TTStepListHelper }

function TTStepListHelper.ToStepListOfSingleCmd: TStepList;
var tp, shiftTimeToKeepOrder: single;
  step: TSequenceStep;
  i: integer;
begin
 Result := TStepList.Create;
 shiftTimeToKeepOrder := 0.0;
 for i:=0 to Self.Count-1 do begin
   step := TSequenceStep(Self.Items[i]);
   tp := step.TimePos;
   step.CmdList.OneStepForOneCmd(Result, tp, shiftTimeToKeepOrder);
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
   //prop.Add('
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
      else index := -1;
    end;//case

    if index <> -1 then begin
      A[index] := strduration;
      Self[i] := A.PackToCmd;
    end;
  end;
end;

procedure TSplittedCmdsHelper.MultiplyAllDurationByCoeff(aCoef: double);
var i, index1, index2, index3: integer;
  A: TParamArray;
  dur: single;
   procedure ProcessIndex(aIndex: integer);
   begin
     if aIndex = -1 then exit;
     dur := StringToSingle(A[aIndex]);
     A[aIndex] := FormatFloatWithDot('0.000', dur * aCoef);
   end;
begin
  for i:=0 to Length(Self)-1 do begin
    index1 := -1;
    index2 := -1;
    index3 := -1;
    A := Self[i].SplitToParamArray;
    case A[0].ToInteger of
      CMD_WAIT,  // CMD_WAIT DurationF
      TITLECMD_DMX_DIMMER:  // TITLECMD_DMX_DIMMER Duration CurveID
        index1 := 1;

      CMD_AUDIO_FADEOUT,  // AUDIOFADEOUT IDaudio duration IDcurve
      CMD_AUDIO_CAPTURE_SETVOLUME, // CMD_AUDIO_CAPTURE_SETVOLUME volume duration IDcurve
      CMD_AUDIO_CAPTURE_SETPAN, // CMD_AUDIO_CAPTURE_SETPAN  panning duration IDcurve
      TITLECMD_DMX_DIMMERRGB:  // TITLECMD_DMX_DIMMERRGB Color Duration CurveID
        index1 := 2;

      CMD_AUDIO_FADEIN,    // AUDIOFADEIN IDaudio volume duration IDcurve
      CMD_AUDIO_SETVOLUME, // AUDIOFIXEVOLUME IDaudio volume duration IDcurve
      CMD_AUDIO_SETPAN,  // AUDIOFIXEPAN IDaudio panning duration IDcurve
      CMD_AUDIO_SETPITCH,  // AUDIOFIXEFREQ IDaudio frequence duration IDcurve
      CMD_SEQUENCESTRETCHTIME:  // CMD_SEQUENCESTRETCHTIME IDSeq StretchValueF DurationF CurveID
         index1 := 3;

      CMD_DMX_DIMMERRGB:  // CMD_DMX_DIMMERRGB IDuniverse IDFixture Color Duration CurveID
         index1 := 4;

      CMD_DMX_DIMMER:  // CMD_DMX_DIMMER IDuniverse IDFixture ChanIndex PercentF DurationF CurveID
         index1 := 5;

      TITLECMD_DMX_FLASH: begin // TITLECMD_DMX_FLASH LevelMin LevelMax DurationMin DurationMax
        index1 := 3;
        index2 := 4;
      end;

      TITLECMD_DMX_FLASHRGB: begin // TITLECMD_DMX_FLASHRGB Color pcMin pcMax DurationMin DurationMax
        index1 := 4;
        index2 := 5;
      end;
      CMD_DMX_FLASHRGB,
      CMD_DMX_FLASH: begin // CMD_DMX_FLASHRGB IDuniverse IDFixture Color pcMin pcMax DurationMin DurationMax
        index1 := 6;
        index2 := 7;
      end;
      CMD_INTERNALDMXWAVE: begin //INTERNALDMXWAVE IDuniverse IDFixture ChanIndex
                                    //                Percent1 Duration1 CurveID1
                                    //                KeepTime
                                    //                Percent2 Duration2 CurveID2
        index1 := 5;
        index2 := 7;
        index3 := 9;
      end;
    end;//case

    ProcessIndex(index1);
    ProcessIndex(index2);
    ProcessIndex(index3);
    Self[i] := A.PackToCmd;
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

function TSplittedCmdsHelper.ParamArrayHaveError(out errMess: string): boolean;
var cmd: Integer;
  vInteger: integer;
  vSingle: single;
  function ErrorOnParamCount(aCount: integer; const aStrCmd: string): boolean;
  begin
    if Length(Self) <> aCount then begin
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
    if Sequences.GetSequenceByStrID(aSeqID) = NIL then begin
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
  begin
    uni := UniverseManager.GetUniverseByStrID(aStrUni);
    if uni = NIL then begin
      errMess := aStrCmd+': '+SUniverseNotFound;
      exit(True);
    end;
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

  cmd := ParamArray0ToCmd;
  if cmd = CMD_UNKNOW then begin
    errMess := SUnrecognizedAction+' "'+Self[0]+'"';
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
      if ErrorOnAudioID(Self[1], SAudioPlay) then exit(True);
      if SoundManager.GetSoundByStrID(Self[1]) = NIL then begin
        errMess := SAudioPlay+' '+Self[1]+' << '+SNotFound;
        exit(True);
      end;
      if ErrorOnVolume(Self[2], SAudioPlay) then exit(True);
    end;

    CMD_AUDIO_STOP: begin  // CMD_AUDIO_STOP IDaudio
      if ErrorOnParamCount(2, SAudioStop) then exit(True);
      if ErrorOnAudioID(Self[1], SAudioStop) then exit(True);
    end;

    CMD_AUDIO_PAUSE: begin  // CMD_AUDIO_PAUSE IDaudio
      if ErrorOnParamCount(2, SAudioPause) then exit(True);
      if ErrorOnAudioID(Self[1], SAudioPause) then exit(True);
    end;

    CMD_AUDIO_FADEIN: begin  // CMD_AUDIO_FADEIN IDaudio volume duration IDcurve
      if ErrorOnParamCount(5, SAudioFadeIn) then exit(True);
      if ErrorOnAudioID(Self[1], SAudioFadeIn) then exit(True);
      if ErrorOnVolume(Self[2], SAudioFadeIn) then exit(True);
      if ErrorOnDuration(Self[3], SAudioFadeIn) then exit(True);
      if ErrorOnCurve(Self[4], SAudioFadeIn) then exit(True);
    end;

    CMD_AUDIO_FADEOUT: begin  // CMD_AUDIO_FADEOUT IDaudio duration IDcurve
      if ErrorOnParamCount(4, SAudioFadeOut) then exit(True);
      if ErrorOnAudioID(Self[1], SAudioFadeOut) then exit(True);
      if ErrorOnDuration(Self[2], SAudioFadeOut) then exit(True);
      if ErrorOnCurve(Self[3], SAudioFadeOut) then exit(True);
    end;

    CMD_AUDIO_SETVOLUME: begin  // CMD_AUDIO_SETVOLUME IDaudio volume duration IDcurve
      if ErrorOnParamCount(5, SAudioSetVolume) then exit(True);
      if ErrorOnAudioID(Self[1], SAudioSetVolume) then exit(True);
      if ErrorOnVolume(Self[2], SAudioSetVolume) then exit(True);
      if ErrorOnDuration(Self[3], SAudioSetVolume) then exit(True);
      if ErrorOnCurve(Self[4], SAudioSetVolume) then exit(True);
    end;

    CMD_AUDIO_SETPAN: begin  // CMD_AUDIO_SETPAN IDaudio panning duration IDcurve
      if ErrorOnParamCount(5, SAudioSetPan) then exit(True);
      if ErrorOnAudioID(Self[1], SAudioSetPan) then exit(True);
      if ErrorOnPan(Self[2], SAudioSetPan) then exit(True);
      if ErrorOnDuration(Self[3], SAudioSetPan) then exit(True);
      if ErrorOnCurve(Self[4], SAudioSetPan) then exit(True);
    end;

    CMD_AUDIO_SETPITCH: begin  // CMD_AUDIO_SETPITCH IDaudio frequence duration IDcurve
      if ErrorOnParamCount(5, SAudioSetFreq) then exit(True);
      if ErrorOnAudioID(Self[1], SAudioSetFreq) then exit(True);
      if ErrorOnPitch(Self[2], SAudioSetFreq) then exit(True);
      if ErrorOnDuration(Self[3], SAudioSetFreq) then exit(True);
      if ErrorOnCurve(Self[4], SAudioSetFreq) then exit(True);
    end;

    TITLECMD_AUDIO_APPLYFX: begin // TITLECMD_AUDIO_APPLYFX  IDaudio  dry/wet  EffectCount
      if ErrorOnParamCount(4, SAudioConnectEffect) then exit(True);
      if ErrorOnAudioID(Self[1], SAudioConnectEffect) then exit(True);
      if ErrorOnDryWet(Self[2], SAudioConnectEffect) then exit(True);
      if ErrorOnAudioEffectCount(Self[3], SAudioConnectEffect) then exit(True);
    end;

    CMD_AUDIO_FXPRESET: begin // CMD_AUDIO_FXPRESET  effectType  presetIndex
      if ErrorOnParamCount(3, SAudioConnectEffect) then exit(True);
      if ErrorOnAudioEffectType(Self[1], SAudioConnectEffect) then exit(True);
      if ErrorOnAudioPreset(Self[2], SAudioConnectEffect) then exit(True);
    end;

    CMD_AUDIO_REMOVEFX: begin // CMD_AUDIO_REMOVEFX  IDaudio
      if ErrorOnParamCount(2, SAudioDisconnectEffect) then exit(True);
      if ErrorOnAudioID(Self[1], SAudioDisconnectEffect) then exit(True);
    end;

    CMD_AUDIO_CAPTURE_START: begin
      if ErrorOnParamCount(1, SAudioCaptureStart) then exit(True);
    end;

    CMD_AUDIO_CAPTURE_STOP: begin
      if ErrorOnParamCount(1, SAudioCaptureStop) then exit(True);
    end;

    CMD_AUDIO_CAPTURE_SETVOLUME: begin // CMD_AUDIO_CAPTURE_SETVOLUME volume duration IDcurve
      if ErrorOnParamCount(4, SAudioCaptureSetVolume) then exit(True);
      if ErrorOnVolume(Self[1], SAudioCaptureSetVolume) then exit(True);
      if ErrorOnDuration(Self[2], SAudioCaptureSetVolume) then exit(True);
      if ErrorOnCurve(Self[3], SAudioCaptureSetVolume) then exit(True);
    end;

    CMD_AUDIO_CAPTURE_SETPAN: begin // CMD_AUDIO_CAPTURE_SETPAN  panning duration IDcurve
      if ErrorOnParamCount(4, SAudioCaptureSetPan) then exit(True);
      if ErrorOnPan(Self[1], SAudioCaptureSetPan) then exit(True);
      if ErrorOnDuration(Self[2], SAudioCaptureSetPan) then exit(True);
      if ErrorOnCurve(Self[3], SAudioCaptureSetPan) then exit(True);
    end;

    TITLECMD_AUDIO_CAPTURE_APPLYFX: begin // TITLECMD_AUDIO_CAPTURE_APPLYFX  dry/wet  EffectCount
      if ErrorOnParamCount(3, SAudioCaptureConnectEffect) then exit(True);
      if ErrorOnDryWet(Self[1], SAudioCaptureConnectEffect) then exit(True);
      if ErrorOnAudioEffectCount(Self[2], SAudioCaptureConnectEffect) then exit(True);
    end;

    CMD_AUDIO_CAPTURE_FXPRESET: begin // CMD_AUDIO_CAPTURE_FXPRESET  effectType  presetIndex
      if ErrorOnParamCount(3, SAudioCaptureConnectEffect) then exit(True);
      if ErrorOnAudioEffectType(Self[1], SAudioCaptureConnectEffect) then exit(True);
      if ErrorOnAudioPreset(Self[2], SAudioCaptureConnectEffect) then exit(True);
    end;

    CMD_AUDIO_CAPTURE_REMOVEFX: begin // CMD_AUDIO_CAPTURE_REMOVEFX
      if ErrorOnParamCount(1, SAudioCaptureDisconnectEffect) then exit(True);
    end;

    CMD_WAIT: begin  // CMD_WAIT DurationF
      if ErrorOnParamCount(2, SWait) then exit(True);
      if ErrorOnDuration(Self[1], SWait) then exit(True);
    end;
    CMD_LOOP: begin  // CMD_LOOP
      if ErrorOnParamCount(1, SWait) then exit(True);
    end;
    CMD_STARTSEQUENCE: begin  // CMD_STARTSEQUENCE IDSeq
      if ErrorOnParamCount(2, SStartSequence) then exit(True);
      if ErrorOnSequenceID(Self[1], SStartSequence) then exit(True);
    end;
    CMD_STOPSEQUENCE: begin  // CMD_STOPSEQUENCE IDSeq
      if ErrorOnParamCount(2, SStopSequence) then exit(True);
      if ErrorOnSequenceID(Self[1], SStopSequence) then exit(True);
    end;
    CMD_SEQUENCESTRETCHTIME: begin  // CMD_SEQUENCESTRETCHTIME IDSeq StretchValueF DurationF CurveID
      if ErrorOnParamCount(5, SStretchTime) then exit(True);
      if ErrorOnSequenceID(Self[1], SStretchTime) then exit(True);
      if ErrorOnSingleRange(Self[2], 0.25, 7.0, SStretchTime, SBadStretchTimeValue) then exit(True);
      if ErrorOnDuration(Self[3], SStretchTime) then exit(True);
      if ErrorOnCurve(Self[4], SStretchTime) then exit(True);
    end;

    CMD_DMX_DIMMER: begin  // CMD_DMX_DIMMER IDuniverse IDFixture ChanIndex PercentF DurationF CurveID
      if ErrorOnParamCount(7, SDMXDimmer) then exit(True);
      if ErrorOnDmxAdress(Self[1], Self[2], Self[3], SDMXDimmer) then exit(True);
      if ErrorOnSingleRange(Self[4], 0.0, 1.0, SDMXDimmer, SBadPercentValue) then exit(True);
      if ErrorOnDuration(Self[5], SDMXDimmer) then exit(True);
      if ErrorOnCurve(Self[6], SDMXDimmer) then exit(True);
    end;

    TITLECMD_DMX_DIMMER: begin  // TITLECMD_DMX_DIMMER Duration CurveID
      if ErrorOnParamCount(3, SDMXDimmer) then exit(True);
      if ErrorOnDuration(Self[1], SDMXDimmer) then exit(True);
      if ErrorOnCurve(Self[2], SDMXDimmer) then exit(True);
    end;

    CMD_DMX_FLAME: begin  // CMD_DMX_FLAME IDuniverse IDFixture ChanIndex LevelMin LevelMax Speed Soften
      if ErrorOnParamCount(8, SDMXFlame) then exit(True);
      if ErrorOnDmxAdress(Self[1], Self[2], Self[3], SDMXFlame) then exit(True);
      if ErrorOnSingleRange(Self[4], 0.0, 1.0, SDMXFlame, SBadMinPercent) then exit(True);
      if ErrorOnSingleRange(Self[5], 0.0, 1.0, SDMXFlame, SBadMaxPercent) then exit(True);
      if ErrorOnSingleRange(Self[6], 0.05, 2.0, SDMXFlame, SBadWaitTime) then exit(True);
      if ErrorOnSingleRange(Self[7], 0.0, 1.0, SDMXFlame, SBadSoften) then exit(True);
    end;

    TITLECMD_DMX_FLAME: begin  // TITLECMD_DMX_FLAME  LevelMinF LevelMaxF WaitTimeF SoftenF
      if ErrorOnParamCount(5, SDMXFlame) then exit(True);
      if ErrorOnSingleRange(Self[1], 0.0, 1.0, SDMXFlame, SBadMinPercent) then exit(True);
      if ErrorOnSingleRange(Self[2], 0.0, 1.0, SDMXFlame, SBadMaxPercent) then exit(True);
      if ErrorOnSingleRange(Self[3], 0.05, 2.0, SDMXFlame, SBadWaitTime) then exit(True);
      if ErrorOnSingleRange(Self[4], 0.0, 1.0, SDMXFlame, SBadSoften) then exit(True);
    end;

    CMD_DMX_STOPEFFECT: begin  // CMD_DMX_STOPEFFECT IDuniverse IDFixture ChanIndex
      if ErrorOnParamCount(4, SDMXStopEffect) then exit(True);
      if ErrorOnDmxAdress(Self[1], Self[2], Self[3], SDMXStopEffect) then exit(True);
    end;

    TITLECMD_DMX_STOPEFFECT: begin
      if ErrorOnParamCount(1, SDMXStopEffect) then exit(True);
    end;

    CMD_DMX_DIMMERRGB: begin  // CMD_DMX_DIMMERRGB IDuniverse IDFixture Color Duration CurveID
      if ErrorOnParamCount(6, SDMXDimmerRGB) then exit(True);
      if ErrorOnDmxAdress(Self[1], Self[2], SDMXDimmerRGB) then exit(True);
      if ErrorOnColor(Self[3], SDMXDimmerRGB) then exit(True);
      if ErrorOnDuration(Self[4], SDMXDimmerRGB) then exit(True);
      if ErrorOnCurve(Self[5], SDMXDimmerRGB) then exit(True);
    end;

    TITLECMD_DMX_DIMMERRGB: begin  // TITLECMD_DMX_DIMMERRGB Color Duration CurveID
      if ErrorOnParamCount(4, SDMXDimmerRGB) then exit(True);
      if ErrorOnColor(Self[1], SDMXDimmerRGB) then exit(True);
      if ErrorOnDuration(Self[2], SDMXDimmerRGB) then exit(True);
      if ErrorOnCurve(Self[3], SDMXDimmerRGB) then exit(True);
    end;

    CMD_DMX_FLAMERGB: begin // CMD_DMX_FLAMERGB IDuniverse IDFixture Color Speed Amplitude Soften
      if ErrorOnParamCount(7, SDMXFlameRGB) then exit(True);
      if ErrorOnDmxAdress(Self[1], Self[2], SDMXFlameRGB) then exit(True);
      if ErrorOnColor(Self[3], SDMXFlameRGB) then exit(True);
      if ErrorOnSingleRange(Self[4], 0.05, 2.0, SDMXFlameRGB, SBadWaitTime) then exit(True);
      if ErrorOnSingleRange(Self[5], 0.0, 1.0, SDMXFlameRGB, SBadAmplitude) then exit(True);
      if ErrorOnSingleRange(Self[6], 0.0, 1.0, SDMXFlameRGB, SBadSoften) then exit(True);
    end;

    TITLECMD_DMX_FLAMERGB: begin // TITLECMD_DMX_FLAMERGB Color Speed Amplitude Soften
      if ErrorOnParamCount(5, SDMXFlameRGB) then exit(True);
      if ErrorOnColor(Self[1], SDMXFlameRGB) then exit(True);
      if ErrorOnSingleRange(Self[2], 0.05, 2.0, SDMXFlameRGB, SBadWaitTime) then exit(True);
      if ErrorOnSingleRange(Self[3], 0.0, 1.0, SDMXFlameRGB, SBadAmplitude) then exit(True);
      if ErrorOnSingleRange(Self[4], 0.0, 1.0, SDMXFlameRGB, SBadSoften) then exit(True);
    end;

    CMD_DMX_AUDIOFOLLOWERRGB: begin // CMD_DMX_AUDIOFOLLOWERRGB IDuniverse IDFixture IDaudio Color Gain SoftenTime
      if ErrorOnParamCount(7, SDMXAudioFollowerRGB) then exit(True);
      if ErrorOnDmxAdress(Self[1], Self[2], SDMXAudioFollowerRGB) then exit(True);
      if ErrorOnAudioID(Self[3], SDMXAudioFollowerRGB) then exit(True);
      if ErrorOnColor(Self[4], SDMXAudioFollowerRGB) then exit(True);
      if ErrorOnSingleRange(Self[5], -1.0, 3.0, SDMXAudioFollowerRGB, SBadGain) then exit(True);
      if ErrorOnSingleRange(Self[6], 0.1, 1.0, SDMXAudioFollowerRGB, SBadSoften) then exit(True);
    end;

    TITLECMD_DMX_AUDIOFOLLOWERRGB: begin // TITLECMD_DMX_AUDIOFOLLOWERRGB IDaudio Color Gain SoftenTime
      if ErrorOnParamCount(5, SDMXAudioFollowerRGB) then exit(True);
      if ErrorOnAudioID(Self[1], SDMXAudioFollowerRGB) then exit(True);
      if ErrorOnColor(Self[2], SDMXAudioFollowerRGB) then exit(True);
      if ErrorOnSingleRange(Self[3], -1.0, 3.0, SDMXAudioFollowerRGB, SBadGain) then exit(True);
      if ErrorOnSingleRange(Self[4], 0.1, 1.0, SDMXAudioFollowerRGB, SBadSoften) then exit(True);
    end;

    CMD_DMX_AUDIOFOLLOWER: begin // CMD_DMX_AUDIOFOLLOWER IDuniverse IDFixture ChanIndex IDaudio Gain MaxPercent SoftenTime
      if ErrorOnParamCount(8, SDMXAudioFollower) then exit(True);
      if ErrorOnDmxAdress(Self[1], Self[2], Self[3], SDMXAudioFollower) then exit(True);
      if ErrorOnAudioID(Self[4], SDMXAudioFollower) then exit(True);
      if ErrorOnSingleRange(Self[5], -1.0, 3.0, SDMXAudioFollower, SBadGain) then exit(True);
      if ErrorOnSingleRange(Self[6], 0.0, 1.0, SDMXAudioFollower, SBadBrightness) then exit(True);
      if ErrorOnSingleRange(Self[7], 0.1, 1.0, SDMXAudioFollower, SBadSoften) then exit(True);
    end;

    TITLECMD_DMX_AUDIOFOLLOWER: begin // TITLECMD_DMX_AUDIOFOLLOWER IDaudio Gain MaxPercent SoftenTime
      if ErrorOnParamCount(5, SDMXAudioFollower) then exit(True);
      if ErrorOnAudioID(Self[1], SDMXAudioFollower) then exit(True);
      if ErrorOnSingleRange(Self[2], -1.0, 3.0, SDMXAudioFollower, SBadGain) then exit(True);
      if ErrorOnSingleRange(Self[3], 0.0, 1.0, SDMXAudioFollower, SBadBrightness) then exit(True);
      if ErrorOnSingleRange(Self[4], 0.1, 1.0, SDMXAudioFollower, SBadSoften) then exit(True);
    end;

    CMD_DMX_FLASH: begin // CMD_DMX_FLASH IDuniverse IDFixture ChanIndex LevelMin LevelMax DurationMin DurationMax
      if ErrorOnParamCount(8, SDMXFlash) then exit(True);
      if ErrorOnDmxAdress(Self[1], Self[2], Self[3], SDMXFlash) then exit(True);
      if ErrorOnSingleRange(Self[4], 0.0, 1.0, SDMXFlash, SBadLevelMin) then exit(True);
      if ErrorOnSingleRange(Self[5], 0.0, 1.0, SDMXFlash, SBadLevelMax) then exit(True);
      if ErrorOnSingleRange(Self[6], 0.0, 1.0, SDMXFlash, SBadDurationMin) then exit(True);
      if ErrorOnSingleRange(Self[7], 0.0, 1.0, SDMXFlash, SBadDurationMax) then exit(True);
    end;

    TITLECMD_DMX_FLASH: begin // TITLECMD_DMX_FLASH LevelMin LevelMax DurationMin DurationMax
      if ErrorOnParamCount(5, SDMXFlash) then exit(True);
      if ErrorOnSingleRange(Self[1], 0.0, 1.0, SDMXFlash, SBadLevelMin) then exit(True);
      if ErrorOnSingleRange(Self[2], 0.0, 1.0, SDMXFlash, SBadLevelMax) then exit(True);
      if ErrorOnSingleRange(Self[3], 0.1, 1000.0, SDMXFlash, SBadDurationMin) then exit(True);
      if ErrorOnSingleRange(Self[4], 0.1, 1000.0, SDMXFlash, SBadDurationMax) then exit(True);
    end;

    CMD_DMX_STOPEFFECTRGB: begin // CMD_DMX_STOPEFFECTRGB IDuniverse IDFixture
      if ErrorOnParamCount(3, SDMXStopEffectRGB) then exit(True);
      if ErrorOnDmxAdress(Self[1], Self[2], SDMXStopEffectRGB) then exit(True);
    end;

    TITLECMD_DMX_STOPEFFECTRGB: begin
      if ErrorOnParamCount(1, SDMXStopEffectRGB) then exit(True);
    end;

    CMD_DMX_COPYCHANNEL: begin // CMD_DMX_COPYCHANNEL SourceIDuniverse SourceIDFixture SourceChanIndex
                               //                     TargetIDUniverse TargetIDFixture TargetChanIndex
      if ErrorOnParamCount(7, SDMXCopy) then exit(True);
      if ErrorOnDmxAdress(Self[1], Self[2], Self[3], SDMXCopy) then exit(True);
      if ErrorOnDmxAdress(Self[4], Self[5], Self[6], SDMXCopy) then exit(True);
    end;

    TITLECMD_DMX_COPYCHANNEL: begin // TITLECMD_DMX_COPYCHANNEL SourceIDuniverse SourceIDFixture SourceChanIndex
      if ErrorOnParamCount(4, SDMXCopy) then exit(True);
      if ErrorOnDmxAdress(Self[1], Self[2], Self[3], SDMXCopy) then exit(True);
    end;

    TITLECMD_DMX_COPYRGB: begin // TITLECMD_DMX_COPYRGB SourceIDuniverse SourceIDFixture
      if ErrorOnParamCount(3, SDMXCopyRGB) then exit(True);
      if ErrorOnDmxAdress(Self[1], Self[2], SDMXCopyRGB) then exit(True);
    end;

    CMD_DMX_COPYRGB: begin // CMD_DMX_COPYRGB SourceIDuniverse SourceIDFixture TargetIDUniverse TargetIDFixture
      if ErrorOnParamCount(5, SDMXCopyRGB) then exit(True);
      if ErrorOnDmxAdress(Self[1], Self[2], SDMXCopyRGB) then exit(True);
      if ErrorOnDmxAdress(Self[3], Self[4], SDMXCopyRGB) then exit(True);
    end;

    TITLECMD_DMX_FLASHRGB: begin // TITLECMD_DMX_FLASHRGB Color pcMin pcMax DurationMin DurationMax
      if ErrorOnParamCount(6, SDMXFlashRGB) then exit(True);
      if ErrorOnColor(Self[1], SDMXFlashRGB) then exit(True);
      if ErrorOnSingleRange(Self[2], 0.0, 1.0, SDMXFlashRGB, SBadLevelMin) then exit(True);
      if ErrorOnSingleRange(Self[3], 0.0, 1.0, SDMXFlashRGB, SBadLevelMax) then exit(True);
      if ErrorOnSingleRange(Self[4], 0.1, 1000.0, SDMXFlashRGB, SBadDurationMin) then exit(True);
      if ErrorOnSingleRange(Self[5], 0.1, 1000.0, SDMXFlashRGB, SBadDurationMax) then exit(True);
    end;

    CMD_DMX_FLASHRGB: begin // CMD_DMX_FLASHRGB IDuniverse IDFixture Color pcMin pcMax DurationMin DurationMax
      if ErrorOnParamCount(8, SDMXFlashRGB) then exit(True);
      if ErrorOnDmxAdress(Self[1], Self[2], SDMXFlashRGB) then exit(True);
      if ErrorOnColor(Self[3], SDMXFlashRGB) then exit(True);
      if ErrorOnSingleRange(Self[2], 0.0, 1.0, SDMXFlashRGB, SBadLevelMin) then exit(True);
      if ErrorOnSingleRange(Self[3], 0.0, 1.0, SDMXFlashRGB, SBadLevelMax) then exit(True);
      if ErrorOnSingleRange(Self[4], 0.1, 1000.0, SDMXFlashRGB, SBadDurationMin) then exit(True);
      if ErrorOnSingleRange(Self[5], 0.1, 1000.0, SDMXFlashRGB, SBadDurationMax) then exit(True);
    end;

    CMD_INTERNALDMXWAVE: begin //INTERNALDMXWAVE IDuniverse IDFixture ChanIndex
                               //                Percent1 Duration1 CurveID1
                               //                KeepTime
                               //                Percent2 Duration2 CurveID2
      if ErrorOnParamCount(11, SDMXWave) then exit(True);
      if ErrorOnDmxAdress(Self[1], Self[2], Self[3], SDMXWave) then exit(True);
      if ErrorOnSingleRange(Self[4], 0.0, 1.0, SDMXWave, SBadPercentValue) then exit(True);
      if ErrorOnDuration(Self[5], SDMXWave) then exit(True);
      if ErrorOnCurve(Self[6], SDMXWave) then exit(True);
      if ErrorOnDuration(Self[7], SDMXWave) then exit(True);
      if ErrorOnSingleRange(Self[8], 0.0, 1.0, SDMXWave, SBadPercentValue) then exit(True);
      if ErrorOnDuration(Self[9], SDMXWave) then exit(True);
      if ErrorOnCurve(Self[10], SDMXWave) then exit(True);
    end;

    else begin
      errMess := SUnrecognizedAction+' ('+cmd.ToString+')';
      exit(True);
    end;
  end;// case cmd
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
  Result := A.ParamArray0ToCmd;
 end;
end;

function TCmdListHelper.IsSingleCmd: boolean;
begin
 Result := Pos( CMD_SEPARATOR, Self ) = 0;
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

function TCmdListHelper.IsAudioPlay(out aAudioID: integer): boolean;
var A: TCmdArray;
  B: TParamArray;
  i, cmd: Integer;
begin
  Result := FALSE;
  if Self.IsSingleCmd then exit;

  A := Self.SplitToCmdArray;
  try
    for i:=0 to High(A) do begin
      B := A[i].SplitToParamArray;
      if TryStrToInt(B[0], cmd) and (cmd = CMD_AUDIO_PLAY) and TryStrToInt(B[1], aAudioID) then exit(True);
    end;
  except
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

function TCmdListHelper.HaveError(out errMess: string): boolean;
var i: integer;
  B: TCmdArray;
  A: TParamArray;
begin
  Result := False;
  errMess := '';

  if Self.IsSingleCmd then begin
    A := Self.SplitToParamArray;
    Result := A.ParamArrayHaveError(errMess);
  end else begin
    B := Self.SplitToCmdArray;
    for i:=0 to High(B) do
      if B[i].HaveError(errMess) then exit(True);
  end;
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

      CMD_WAIT,  // CMD_WAIT DurationF
      TITLECMD_DMX_DIMMER: // TITLECMD_DMX_DIMMER Duration CurveID
         pos+= StringToSingle(B[1]);

      CMD_AUDIO_FADEOUT,  // AUDIOFADEOUT IDaudio duration IDcurve
      CMD_AUDIO_CAPTURE_SETVOLUME, // CMD_AUDIO_CAPTURE_SETVOLUME volume duration IDcurve
      CMD_AUDIO_CAPTURE_SETPAN, // CMD_AUDIO_CAPTURE_SETPAN  panning duration IDcurve
      TITLECMD_DMX_DIMMERRGB:  // TITLECMD_DMX_DIMMERRGB Color Duration CurveID
        pos+= StringToSingle(B[2]);

      CMD_AUDIO_FADEIN,    // AUDIOFADEIN IDaudio volume duration IDcurve
      CMD_AUDIO_SETVOLUME, // AUDIOFIXEVOLUME IDaudio volume duration IDcurve
      CMD_AUDIO_SETPAN,  // AUDIOFIXEPAN IDaudio panning duration IDcurve
      CMD_AUDIO_SETPITCH,  // AUDIOFIXEFREQ IDaudio frequence duration IDcurve
      CMD_SEQUENCESTRETCHTIME:  // CMD_SEQUENCESTRETCHTIME IDSeq StretchValueF DurationF CurveID
         pos+= StringToSingle(B[3]);

//      CMD_DMX_DIMMERRGB:  // CMD_DMX_DIMMERRGB IDuniverse IDFixture Color Duration CurveID
//         pos+= StringToSingle(B[4]);

//      CMD_DMX_DIMMER:  // CMD_DMX_DIMMER IDuniverse IDFixture ChanIndex PercentF DurationF CurveID
//         pos+= StringToSingle(B[5]);

//      CMD_DMX_FLASH, // CMD_DMX_FLASH IDuniverse IDFixture ChanIndex LevelMin LevelMax DurationMin DurationMax
//      CMD_DMX_FLASHRGB: // CMD_DMX_FLASHRGB IDuniverse IDFixture Color pcMin pcMax DurationMin DurationMax
//         pos+= StringToSingle(B[7]);

      TITLECMD_DMX_FLASH: // TITLECMD_DMX_FLASH LevelMin LevelMax DurationMin DurationMax
         pos+= StringToSingle(B[4]);

      TITLECMD_DMX_FLASHRGB: // TITLECMD_DMX_FLASHRGB Color pcMin pcMax DurationMin DurationMax
         pos+= StringToSingle(B[5]);

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
  tp, shiftTimeToKeepOrder: single;
  i: Integer;
begin
 Result := '';
 A := Self.SplitToSequencerInfoArray;
 if High(A)=-1 then exit;

 step := TSequenceStep.Create;
 temp:= TStepList.Create;
 shiftTimeToKeepOrder := 0.0;
 for i:=2 to High(A) do begin  // skip ID and GroupValue
  step.Deserialize( A[i] );
  tp := step.TimePos + shiftTimeToKeepOrder;
  step.CmdList.OneStepForOneCmd( temp, tp, shiftTimeToKeepOrder );
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

procedure TCmdListHelper.OneStepForOneCmd(aSLTarget: TStepList; var aTimePos: single; var shiftTimeToKeepOrder: single);
var A: TCmdArray;
  i: Integer;
  step: TSequenceStep;
  tp: single;
  B: TParamArray;
const _deltaTimeAmount = 0.00001;
begin
 // a CmdList can be:
 //    - a single Cmd -> wait or any basic action
 //    - a list of Cmd -> explore the list using recursivity

 tp := aTimePos;
 A := Self.SplitToCmdArray;
 for i:=0 to High(A) do
 begin
  if not A[i].IsSingleCmd then
    A[i].OneStepForOneCmd( aSLTarget, tp, shiftTimeToKeepOrder ) // explore the list using recursivity
  else
  begin
    B:=A[i].SplitToParamArray;
    if B[0]=CMD_WAIT.ToString then begin
      tp := tp+StringToSingle(B[1]);
      shiftTimeToKeepOrder := 0.0;
    end
    else
    begin
      // it's a basic action
      step:= TSequenceStep.Create;
      step.TimePos := tp + shiftTimeToKeepOrder;
      step.CmdList := A[i];
      aSLTarget.Add( step );
      tp := tp + shiftTimeToKeepOrder;// Shift the action to keep them in the same order while sorting...!!
      shiftTimeToKeepOrder := shiftTimeToKeepOrder + _deltaTimeAmount;
    end;
  end;
 end;
 aSLTarget.Sort;
end;


end.

