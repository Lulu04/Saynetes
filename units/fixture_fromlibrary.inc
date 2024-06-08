{$ifdef SectionInterface}

{ TFixtureLibraryLocation }

TFixtureLibraryLocation = record
  SubFolder,    // the library sub-folder where the fixture is
  Filename,     // the filename .dmx of the fixture
  Mode: string; // the mode needed
  procedure InitDefault;
  procedure CopyTo(var aFixLibLocation: TFixtureLibraryLocation);
  function SaveToString: string;
  function LoadFromString(const s: string): boolean;
  // concat the subfolder and the filename
  function RelativePathInLibrary: string;
  function AbsolutPath: string;
  // return True if the record locates an existing .dmx file
  function HaveFixtureOk: boolean;
  // return true if the record locates an existing .dmx file and an existing mode
  function HaveFixtureAndModeOk: boolean;
end;

{ TWebLink }

TWebLink = record
  LinkType,
  Url: string;
  procedure CopyTo(var aTarget: TWebLink);
  procedure ExchangeWith(var aTarget: TWebLink);
end;
TWebLinks = array of TWebLink;


// On some fixtures, some dips must be set to ON or OFF to enable dmx control mode
TDipSwitchFunction = (dsfAdress, // dip sets the adress
                      dsfOn,     // dip must be set to ON
                      dsfOff     // dip must be set to OFF
                     );
ArrayOfDipSwitchFunction = array of TDipSwitchFunction;
{ TDipSwitch }

  TDipSwitch = record
  const DIPSWITCH_HEADER = '[DIPSWITCH]';
  var
    OnIsUp: boolean; // true if the up position means ON
    MSBIsLeft: boolean; // true if the msb bit is on the left
    Functions: ArrayOfDipSwitchFunction;
    procedure InitDefault;
    procedure LoadFrom(aDipSwitch: TDipSwitch);
    procedure LoadFrom(t: TStringList);
    procedure SaveTo(t: TStringList);
    function AdressBitCount: integer;
end;


{ TFixLibGeneral }

  TFixLibGeneral = record
  const SECTION_HEADER='[GENERAL]';
  var
    ManufacturerName, // the real name of the manufacturer
    FixtureName,
    Authors,
    Creator: string;
    FixtureType: TFixtureType;
    procedure InitDefault;
    procedure CopyFrom(const Another: TFixLibGeneral);
    procedure SaveTo(t: TStringList);
    function LoadFrom(t: TStringList): boolean;
  end;

{ TFixLibPhysical }

  TFixLibPhysical = record
  const SECTION_HEADER='[PHYSICAL]';
  var
    Width,
    Height,
    Depth,
    Power: integer;
    Weight,
    Connector,
    Bulb: string;
    Lumens: integer;
    Lens: string;
    LensMinDegree,
    LensMaxDegree: single;
    procedure InitDefault;
    procedure CopyFrom(const Another: TFixLibPhysical);
    procedure SaveTo(t: TStringList);
    procedure LoadFrom(t: TStringList);
    procedure DecodePropertyFrom(const prop: TProperties);
  end;

  { TFixLibSwitchDescriptor }
  PFixLibSwitchDescriptor = ^TFixLibSwitchDescriptor;
  TFixLibSwitchDescriptor = record
    SwitchVirtualChannel: string; // ex: "Strobe / Program Speed / Sound Sensitivity" (from mode definition)
    SwitchToSubChannel: string;   // ex: "Program Speed"

    IndexOfVirtualChannel: integer;
    IndexOfSubChannel: integer;

    function SaveToString: string;
    procedure LoadFromString(const s: string);
  end;


  { TFixLibSingleRange }

  TFixLibSingleRange = record
    BeginValue,
    EndValue: byte;
    Text,
    Extra: string;

    SwitchDescriptors: array of TFixLibSwitchDescriptor;
    procedure AddChannelSwitch(const aVirtualName, aSubChannelName: string);

    procedure InitDefault;
    function SaveToString: string;
    function LoadFromString(const s: string): boolean;
    procedure CopyTo(var aTarget: TFixLibSingleRange);
//    procedure CopyTo(var aTarget: TChannelRange);
    function ToReadableString: string;
    function HaveSwitch: boolean;
    procedure SetSwitchIndexes(const aVirtualName: string; aIndex: integer; const aSubChannels: TStringArray);
    // Returns a string with virtualName0:SwitchToSubChannel0 + LineEnding + virtualName1:SwitchToSubChannel1...
    function GetSwitchsAsText: string;
end;
  PFixLibSingleRange = ^TFixLibSingleRange;

  { TFixLibAvailableChannel }

  PFixLibAvailableChannel = ^TFixLibAvailableChannel;
  TFixLibAvailableChannel = record
    NameID: string;
    DefaultValue: byte;
    ChanType: TChannelType;
    Ranges: array of TFixLibSingleRange;
    AliasOfNameID: string;

    procedure InitDefault;
    procedure InitDefaultSingleRange;
    procedure InitAsRed;
    procedure InitAsGreen;
    procedure InitAsBlue;
    function SaveToString: string;
    function LoadFromString(const s: string): boolean;
    procedure CopyTo(var aTarget: TFixLibAvailableChannel);
    // return True if this channel have at least 1 switch descriptor
    function HaveSwitchDescriptor: boolean;
    // checks the integrity of each channel ranges
    function HaveRangesError: boolean;

    function IsAlias: boolean;
  end;
  TFixLibAvailableChannels = array of TFixLibAvailableChannel;
  PFixLibAvailableChannels = ^TFixLibAvailableChannels;


{ TVirtualChannelForSwitch }
PVirtualChannelForSwitch = ^TVirtualChannelForSwitch;
TVirtualChannelForSwitch = record
  VirtualName: string;
  SubChannelIDs: TStringArray;      // list of used channel
  procedure InitDefault;
  // add single instance of channel ID
  procedure AddSubChannel(const aID: string);

  procedure ReplaceChannelName(const aOldName, aNewName: string);

  // gives "VirtualName:SubChannel1;SubChannel2;..."
  function PackToString: string;
  procedure InitFromPackedString(const s: string);
end;
TVirtualChannelForSwitchs = array of TVirtualChannelForSwitch;

var
  FVirtualChannelInMode: TVirtualChannelForSwitchs;

type

{ TFixLibMode }
  PFixLibMode = ^TFixLibMode;
  TFixLibMode = record
    Name,
    ShortName: string;
    ChannelsIDToUse: {array of }TStringArray;
    procedure InitDefault;
    function SaveToString: string;
    procedure LoadFromString(const s: string);
  end;
  TFixLibModes = array of TFixLibMode;
  PFixLibModes = ^TFixLibModes;

{ TLibraryFixture }

TLibraryFixture = record
  WebLinks: TWebLinks;
  General: TFixLibGeneral;
  Physical: TFixLibPhysical;
  DipSwitchs: TDipSwitch;
  AvailableChannels: TFixLibAvailableChannels;
  Modes: TFixLibModes;

  procedure InitDefault;
  function LoadFromFile(const aFileName: string): boolean;
  function LoadFrom(aLibraryLocation: TFixtureLibraryLocation): boolean;
  function SaveToFile(const aFileName: string): boolean;

  function GetChannelNamesForMode(const aModeName: string): TStringArray;
  function GetChannelCountForMode(const aMode: string): integer;
end;


procedure SaveAvailableChannelsTo(t: TStringList; p: PFixLibAvailableChannels);
function LoadAvailableChannelsFrom(t: TStringList; p: PFixLibAvailableChannels): boolean;
function IndexOfChannel(const aNameID: string; p: PFixLibAvailableChannels): integer;

// Split a formatted virtual name in its virtual name and sub-channel names
// returns the parameters and True if sucess
function TrySplitVirtual(const aFormattedVirtualName: string;
                         out aVirtualName: string;
                         out aSubChannels: TStringArray): boolean;
// check if the mode exists in the specified fixture definition file without loading all the fixture data
function ModeExistsInFixture(const aFixtureFile, aModeName: string): boolean;

{$endif}
{$ifdef SectionImplementation}

const SNotInitialized = 'not initialized';

procedure SaveAvailableChannelsTo(t: TStringList; p: PFixLibAvailableChannels);
var i: integer;
begin
  t.Add('[CHAN_DEF]');
  for i:=0 to High(p^) do t.Add(p^[i].SaveToString);
  t.Add('[END_CHAN_DEF]');
end;

function LoadAvailableChannelsFrom(t: TStringList; p: PFixLibAvailableChannels): boolean;
var i, k, kEnd: Integer;
  pSource: PFixLibAvailableChannel;
  na: string;
begin
  p^ := NIL;
  k := t.IndexOf('[CHAN_DEF]');
  kEnd := t.IndexOf('[END_CHAN_DEF]');

  if (k = -1) or (kEnd = -1) or (kEnd <= k) or (kEnd-k < 2) then begin
    Log.Error('[CHAN_DEF]/[END_CHAN_DEF] block not found', 1);
    exit(False);
  end;

  try
    Result := True;
    SetLength(p^, kEnd-k-1);
    for i:=0 to High(p^) do begin
      inc(k);
      Result := Result and p^[i].LoadFromString(t.Strings[k]);
    end;

    // Fill alias channel with data from their source
    if Result then begin
      for i:=0 to High(p^) do begin
        if Length(p^[i].AliasOfNameID) > 0 then begin
          pSource := p^.GetChannelsByName(p^[i].AliasOfNameID);
          if pSource = NIL then begin
            Log.Error('Try to define the alias channel "'+p^[i].NameID+'" from "'+p^[i].AliasOfNameID+'" but channel "'+p^[i].AliasOfNameID+'" not found', 1);
            Result := False;
            break;
          end else begin
            na := p^[i].NameID;
            p^[i].LoadFromString(pSource^.SaveToString);
            p^[i].NameID := na;
            p^[i].AliasOfNameID := pSource^.NameID;
          end;
        end;
      end;
    end;
  except
    Result := False;
  end;
end;

function IndexOfChannel(const aNameID: string; p: PFixLibAvailableChannels): integer;
var i: integer;
begin
  for i:=0 to High(p^) do
    if p^[i].NameID = aNameID then begin
      Result := i;
      exit;
    end;

  Result := -1;
end;

function TrySplitVirtual(const aFormattedVirtualName: string; out
  aVirtualName: string; out aSubChannels: TStringArray): boolean;
var A: TStringArray;
begin
  A := aFormattedVirtualName.Split([':']);
  if Length(A) <> 2 then exit(False);
  aVirtualName := A[0];

  aSubChannels := A[1].Split([';']);
  if Length(aSubChannels) = 0 then exit(False);
  Result := True;
end;

function ModeExistsInFixture(const aFixtureFile, aModeName: string): boolean;
var t: TStringlist;
  k, kEnd: integer;
  prop: TProperties;
  s: string;
begin
  if not FileExists(aFixtureFile) then exit(False);

  t := TStringlist.Create;
  try
    try
      t.LoadFromFile(aFixtureFile);
      k := t.IndexOf('[MODES]');
      kEnd := t.IndexOf('[END_MODES]');
      if (kEnd = -1) or (k = -1) then exit(False);
      s := '';
      repeat
        inc(k);
        prop.Split(t.Strings[k], '|');
        if prop.StringValueOf('Name', s, '') and (s = aModeName) then exit(True);
      until k = kEnd-1;
      Result := False;
    except
      exit(False);
    end;
  finally
    t.Free;
  end;
end;

{ TSubChannel }

procedure TSubChannel.CopyFrom(p: PFixLibAvailableChannel);
var i: integer;
begin
  Name := p^.NameID;
  ChannelType := p^.ChanType;
  DefaultValue := p^.DefaultValue;
  Ranges := NIL;
  SetLength(Ranges, Length(p^.Ranges));
  for i:=0 to High(Ranges) do
    Ranges[i].CopyFrom(@p^.Ranges[i]);
end;

{ TSwitchDescriptor }

procedure TSwitchDescriptor.CopyFrom(p: PFixLibSwitchDescriptor);
begin
  ChannelIndexToSwitch := p^.IndexOfVirtualChannel;
  SubChannelIndex := p^.IndexOfSubChannel;
end;

{ TVirtualChannelForSwitch }

procedure TVirtualChannelForSwitch.InitDefault;
begin
  VirtualName := '';
  SubChannelIDs := NIL;
end;

procedure TVirtualChannelForSwitch.AddSubChannel(const aID: string);
var i: integer;
begin
  for i:=0 to High(SubChannelIDs) do
    if SubChannelIDs[i] = aID then exit;
  i := Length(SubChannelIDs);
  SetLength(SubChannelIDs, i+1);
  SubChannelIDs[i] := aID;
end;

procedure TVirtualChannelForSwitch.ReplaceChannelName(const aOldName, aNewName: string);
var i: integer;
begin
  if VirtualName = aOldName then VirtualName := aNewName
    else begin
      // search in sub-channels
      for i:=0 to High(SubChannelIDs) do
        if SubChannelIDs[i] = aOldName then begin
          SubChannelIDs[i] := aNewName;
          exit;
        end;
    end;
end;

function TVirtualChannelForSwitch.PackToString: string;
var i :integer;
  sep: char;
begin
  Result := VirtualName;
  if Length(SubChannelIDs) = 0 then exit;

  sep := ':';
  for i:=0 to High(SubChannelIDs) do begin
    Result := Result + sep + SubChannelIDs[i];
    sep := ';';
  end;
end;

procedure TVirtualChannelForSwitch.InitFromPackedString(const s: string);
var A, B: TStringArray;
begin
  InitDefault;
  if s = '' then exit;
  A := s.Split([':']);
  if Length(A) >= 1 then VirtualName := A[0];
  if Length(A) = 2 then begin
    B := A[1].Split([';']);
    SubChannelIDs := Copy(B, 0, Length(B));
  end;
end;

{ TFixLibSwitchDescriptor }

function TFixLibSwitchDescriptor.SaveToString: string;
begin
  Result := SwitchVirtualChannel+':'+SwitchToSubChannel;
end;

procedure TFixLibSwitchDescriptor.LoadFromString(const s: string);
var A: TStringArray;
  i: integer;
begin
  A := s.Split([':']);
  if Length(A) <> 2 then begin
    SwitchVirtualChannel := '';
    SwitchToSubChannel := '';
  end else begin
    SwitchVirtualChannel := A[0];
    SwitchToSubChannel := A[1];

    // add entries to the list of virtual channel for switch
    i := FVirtualChannelInMode.AddVirtualName(SwitchVirtualChannel);
    FVirtualChannelInMode[i].AddSubChannel(SwitchToSubChannel);
  end;
  IndexOfVirtualChannel := -1;
  IndexOfSubChannel := -1;
end;

{ TWebLink }

procedure TWebLink.CopyTo(var aTarget: TWebLink);
begin
  aTarget.LinkType := LinkType;
  aTarget.Url := Url;
end;

procedure TWebLink.ExchangeWith(var aTarget: TWebLink);
var s: string;
begin
  s := aTarget.LinkType;
  aTarget.LinkType := LinkType;
  LinkType := s;
  s := aTarget.Url;
  aTarget.Url := Url;
  Url := s;
end;

{ TFixtureLibraryLocation }

procedure TFixtureLibraryLocation.InitDefault;
begin
  SubFolder := '';
  Filename := '';
  Mode := '';
end;

procedure TFixtureLibraryLocation.CopyTo(var aFixLibLocation: TFixtureLibraryLocation);
begin
  aFixLibLocation.SubFolder := SubFolder;
  aFixLibLocation.Filename := Filename;
  aFixLibLocation.Mode := Mode;
end;

function TFixtureLibraryLocation.SaveToString: string;
var prop: TProperties;
begin
  prop.Init(FIXTURELOCATION_SEPARATOR);
  prop.Add('SubFolder', SubFolder);
  prop.Add('Filename', Filename);
  prop.Add('Mode', Mode);
  Result := prop.PackedProperty;
end;

function TFixtureLibraryLocation.LoadFromString(const s: string): boolean;
var prop: TProperties;
begin
  Result := True;
  prop.Split(s, FIXTURELOCATION_SEPARATOR);
  Result := Result and prop.StringValueOf('SubFolder', SubFolder, '');
  Result := Result and prop.StringValueOf('Filename', Filename, '');
  Result := Result and prop.StringValueOf('Mode', Mode, '');
  SubFolder := AdjustDirectorySeparator(SubFolder);
end;

function TFixtureLibraryLocation.RelativePathInLibrary: string;
begin
  Result := ConcatPaths([SubFolder, Filename]);
end;

function TFixtureLibraryLocation.AbsolutPath: string;
begin
  Result := ConcatPaths([GetAppDMXLibraryFolder, RelativePathInLibrary]);
end;

function TFixtureLibraryLocation.HaveFixtureOk: boolean;
var f: string;
begin
  f := AbsolutPath;
  Result := FileExists(f) and (ExtractFileExt(f) = DMX_LIBRARY_FILE_EXTENSION);
end;

function TFixtureLibraryLocation.HaveFixtureAndModeOk: boolean;
begin
  Result := ModeExistsInFixture(AbsolutPath, Mode);
end;

{ TLibraryFixture }

procedure TLibraryFixture.InitDefault;
begin
  General.InitDefault;
  Physical.InitDefault;
  DipSwitchs.InitDefault;
  AvailableChannels := NIL;
  Modes := NIL;
  WebLinks := NIL;

  FVirtualChannelInMode := NIL;
end;

function TLibraryFixture.LoadFromFile(const aFileName: string): boolean;
var t: TStringList;
    i, j, m, k: integer;
    f, rawName, virtualName: string;
    subChannels: TStringArray;
begin
  InitDefault;
  Result := False;
  t := TStringList.Create;
  f := IncludeTrailingPathDelimiter(ExtractFileName(ExcludeTrailingPathDelimiter(ExtractFilePath(aFileName))));
  Log.Info('Loading fixture "'+f+ExtractFileName(aFileName)+'"', 1);
  try
    try
      t.LoadFromFile(aFileName);
      k := t.indexOf('[SAYNETE]');
      if k = -1 then begin
        Log.Error('Header [SAYNETE] not found', 2);
        exit(False);
      end;

      WebLinks.LoadFrom(t);

      if not General.LoadFrom(t) then exit(False);

      Physical.LoadFrom(t);
      DipSwitchs.LoadFrom(t);
      if not LoadAvailableChannelsFrom(t, @AvailableChannels) then exit(False);
      if not Modes.LoadModesFrom(t) then exit(False);

      // Check if the channels ID used by the modes are defined
      for m:=0 to High(Modes) do
        for i:=0 to High(Modes[m].ChannelsIDToUse) do begin
          rawName := Modes[m].ChannelsIDToUse[i];
          if TrySplitVirtual(rawName, virtualName, subChannels) then begin
            for j:=0 to High(subChannels) do
              if IndexOfChannel(subChannels[j], @AvailableChannels) = -1 then begin
                Log.Error('Sub-channel "'+virtualName+':'+subChannels[j]+'" used by mode "'+Modes[m].Name+'" is not defined !',1);
                exit(False);
              end;
          end else if IndexOfChannel(rawName, @AvailableChannels) = -1 then begin
            Log.Error('Channel "'+rawName+'" used by mode "'+Modes[m].Name+'" is not defined !',1);
            exit(False);
          end;
        end;

      Result := True;
    except
      On E: Exception do begin
        Result := False;
        Log.Error('Exception occurs "'+E.Message+'"', 1);
      end;
    end;
  finally
    t.Free;
  end;
end;

function TLibraryFixture.LoadFrom(aLibraryLocation: TFixtureLibraryLocation): boolean;
var f, virtualName: string;
    subChannels: TStringArray;
    i, k, m, n: integer;
begin
  f := ConcatPaths([GetAppDMXLibraryFolder + aLibraryLocation.RelativePathInLibrary]);
  f := aLibraryLocation.AbsolutPath;
  Result := LoadFromFile(f);
  if not Result then exit;

  if aLibraryLocation.Mode = '' then exit;
  // converts switching channel IDs to Index
  m := Modes.IndexOf(aLibraryLocation.Mode);
  if m = -1 then begin
    Log.Error('Mode "'+aLibraryLocation.Mode+'" not found', 3);
    exit(False);
  end;
  for i:=0 to High(Modes[m].ChannelsIDToUse) do begin  // i = index of TDMXChannel in this mode
    if TrySplitVirtual(Modes[m].ChannelsIDToUse[i], virtualName, subChannels) then begin
      // scan all ranges in all available channels
      for k:=0 to High(AvailableChannels) do begin
        for n:=0 to High(AvailableChannels[k].Ranges) do
          if AvailableChannels[k].Ranges[n].HaveSwitch then begin
            AvailableChannels[k].Ranges[n].SetSwitchIndexes(virtualName, i, subChannels);
          end;
      end;
    end;
  end;
end;

function TLibraryFixture.SaveToFile(const aFileName: string): boolean;
var t: TStringList;
begin
  Result := FALSE;
  t := TStringList.Create;
  try
    t.Add('[SAYNETE]');
    t.Add('Version|'+APP_VERSION);
    WebLinks.SaveTo(t);
    General.SaveTo(t);
    Physical.SaveTo(t);
    DipSwitchs.SaveTo(t);
    SaveAvailableChannelsTo(t, @AvailableChannels);
    Modes.SaveModesTo(t);
    try
      t.SaveToFile(aFileName);
      Result := TRUE;
    except
      On E: Exception do begin
        Log.Error('TLibraryFixture.SaveToFile('+aFileName+')');
        Log.Error('Exception occurs "'+E.Message+'"', 1);
      end;
    end;
  finally
    t.Free;
  end;
end;

function TLibraryFixture.GetChannelNamesForMode(const aModeName: string): TStringArray;
begin
  Result := NIL;
  Result := Modes.GetChannelUsed(aModeName);
end;

function TLibraryFixture.GetChannelCountForMode(const aMode: string): integer;
var j: integer;
begin
  Result := 0;
  j := Modes.IndexOf(aMode);
  if j <> -1 then Result := Length(Modes[j].ChannelsIDToUse);
end;

{ TFixLibMode }

procedure TFixLibMode.InitDefault;
begin
  Name := '';
  ShortName := '';
  ChannelsIDToUse := NIL;
end;

function TFixLibMode.SaveToString: string;
var prop: TProperties;
  temp: string;
  i: integer;
begin
  prop.Init('|');
  prop.Add('Name', Name);
  if ShortName <> '' then prop.Add('ShortName', ShortName);

  temp := '';
  for i:=0 to High(ChannelsIDToUse) do
    if i = 0 then temp := ChannelsIDToUse[i]
      else temp := temp + '~' + ChannelsIDToUse[i];

  prop.Add('Content', temp);
  Result := prop.PackedProperty;
end;

procedure TFixLibMode.LoadFromString(const s: string);
var prop: TProperties;
  temp: string;
  A: TStringArray;
  i: Integer;
  procedure LogError(aProp: string);
  begin
    Log.Error('Missing property '+aProp, 1);
  end;

begin
  temp := '';
  InitDefault;
  prop.Split(s, '|');
  if not prop.StringValueOf('Name', Name, '') then LogError('Name');
  prop.StringValueOf('ShortName', ShortName, Name);

  if not prop.StringValueOf('Content', temp, '') then LogError('Content');
  if Length(temp) > 0 then begin
    A := temp.Split(['~']);
    SetLength(ChannelsIDToUse, Length(A));
    for i:=0 to High(A) do
      ChannelsIDToUse[i] := A[i];
  end;
end;

{ TFixLibSingleRange }

procedure TFixLibSingleRange.AddChannelSwitch(const aVirtualName, aSubChannelName: string);
var i: integer;
begin
  i := Length(SwitchDescriptors);
  SetLength(SwitchDescriptors, i+1);
  SwitchDescriptors[i].SwitchVirtualChannel := aVirtualName;
  SwitchDescriptors[i].SwitchToSubChannel := aSubChannelName;

  // add entries to the list of virtual channel for switch
  i := FVirtualChannelInMode.AddVirtualName(aVirtualName);
  FVirtualChannelInMode[i].AddSubChannel(aSubChannelName);
end;

procedure TFixLibSingleRange.InitDefault;
begin
  BeginValue := 0;
  EndValue := 255;
  Text := SNotInitialized;
  Extra := '';
  SwitchDescriptors := NIL;
end;

function TFixLibSingleRange.SaveToString: string;
var prop: TProperties;
  i: Integer;
begin
  prop.Init('~');
  prop.Add('Begin', BeginValue);
  prop.Add('End', EndValue);
  prop.Add('Txt', Text);
  if Extra <> '' then prop.Add('Extra', Extra);

  for i:=0 to High(SwitchDescriptors) do
    prop.Add('Switch'+(i+1).ToString, SwitchDescriptors[i].SaveToString);

  Result := prop.PackedProperty;
end;

function TFixLibSingleRange.LoadFromString(const s: string): boolean;
var prop: TProperties;
  i, vi: Integer;
  s1: string;
begin
  Result := False;
  vi := 0;
  InitDefault;
  try
    prop.Split(s, '~');
    prop.IntegerValueOf('Begin', vi, BeginValue);
    BeginValue := vi;
    prop.IntegerValueOf('End', vi, EndValue);
    EndValue := vi;
    prop.StringValueOf('Txt', Text, Text);
    prop.StringValueOf('Extra', Extra, Extra);

    // read switchs data
    i := 0;
    repeat
      inc(i);
    until not prop.CheckProperties(['Switch'+i.ToString]);
    dec(i);
    if i > 0 then begin
      s1 := '';
      SetLength(SwitchDescriptors, i);
      for i:=0 to High(SwitchDescriptors) do begin
        prop.StringValueOf('Switch'+(i+1).ToString, s1, '');
        SwitchDescriptors[i].LoadFromString(s1); // update also virtual channels list (FVirtualChannelInMode)
      end;
    end else SwitchDescriptors := NIL;

    Result := True;
  except
    On E :Exception do begin
      Log.Error('exception occurs while reading range values from "'+s+'"'+ E.Message, 3);
    end;
  end;
end;

procedure TFixLibSingleRange.CopyTo(var aTarget: TFixLibSingleRange);
var i: integer;
begin
  aTarget.BeginValue := BeginValue;
  aTarget.EndValue := EndValue;
  aTarget.Text := Text;
  aTarget.Extra := Extra;

  aTarget.SwitchDescriptors := NIL;
  SetLength(aTarget.SwitchDescriptors, Length(SwitchDescriptors));
  for i:=0 to High(SwitchDescriptors) do begin
    aTarget.SwitchDescriptors[i].SwitchVirtualChannel := SwitchDescriptors[i].SwitchVirtualChannel;
    aTarget.SwitchDescriptors[i].SwitchToSubChannel := SwitchDescriptors[i].SwitchToSubChannel;
    aTarget.SwitchDescriptors[i].IndexOfVirtualChannel := SwitchDescriptors[i].IndexOfVirtualChannel;
    aTarget.SwitchDescriptors[i].IndexOfSubChannel := SwitchDescriptors[i].IndexOfSubChannel;
  end;
end;

{procedure TFixLibSingleRange.CopyTo(var aTarget: TChannelRange);
var i: integer;
begin
  aTarget.BeginValue := BeginValue;
  aTarget.EndValue := EndValue;
  aTarget.Text := Text;
  aTarget.Extra := Extra;
  aTarget.SwitchDescriptors := NIL;
  SetLength(aTarget.SwitchDescriptors, Length(SwitchDescriptors));
  for i:=0 to High(SwitchDescriptors) do begin
    aTarget.SwitchDescriptors[i].ChannelIndexToSwitch := SwitchDescriptors[i].IndexOfVirtualChannel;
    aTarget.SwitchDescriptors[i].SubChannelIndex := SwitchDescriptors[i].IndexOfSubChannel;
  end;
end; }

function TFixLibSingleRange.ToReadableString: string;
begin
  Result := BeginValue.ToString+'..'+EndValue.ToString+'  :  '+Text+' '+Extra;
end;

function TFixLibSingleRange.HaveSwitch: boolean;
begin
  Result := Length(SwitchDescriptors) > 0;
end;

procedure TFixLibSingleRange.SetSwitchIndexes(const aVirtualName: string;
  aIndex: integer; const aSubChannels: TStringArray);
  var i, j: integer;
begin
  for i:=0 to High(SwitchDescriptors) do
    if SwitchDescriptors[i].SwitchVirtualChannel = aVirtualName then begin
      SwitchDescriptors[i].IndexOfVirtualChannel := aIndex;
      for j:=0 to High(aSubChannels) do
        if SwitchDescriptors[i].SwitchToSubChannel = aSubChannels[j] then
          SwitchDescriptors[i].IndexOfSubChannel := j;
    end;
end;

function TFixLibSingleRange.GetSwitchsAsText: string;
var i: integer;
begin
  Result := '';
  if Length(SwitchDescriptors) = 0 then exit;
  for i:=0 to High(SwitchDescriptors) do begin
    Result := Result + SwitchDescriptors[i].SwitchVirtualChannel+':'+SwitchDescriptors[i].SwitchToSubChannel;
    if i < High(SwitchDescriptors) then Result := Result + LineEnding;
  end;
end;


{ TFixLibAvailableChannel }

procedure TFixLibAvailableChannel.InitDefault;
begin
  NameID := SNotInitialized;
  DefaultValue := 0;
  ChanType := ctConfig;
  Ranges := NIL;
  AliasOfNameID := '';
end;

procedure TFixLibAvailableChannel.InitDefaultSingleRange;
begin
  Ranges := NIL;
  SetLength(Ranges, 1);
  Ranges[0].InitDefault;
end;

procedure TFixLibAvailableChannel.InitAsRed;
begin
  InitDefault;
  NameID := 'Red';
  ChanType := ctRed;
  InitDefaultSingleRange;
  Ranges[0].Text := '0..100%';
end;

procedure TFixLibAvailableChannel.InitAsGreen;
begin
  InitDefault;
  NameID := 'Green';
  ChanType := ctGreen;
  InitDefaultSingleRange;
  Ranges[0].Text := '0..100%';
end;

procedure TFixLibAvailableChannel.InitAsBlue;
begin
  InitDefault;
  NameID := 'Blue';
  ChanType := ctBlue;
  InitDefaultSingleRange;
  Ranges[0].Text := '0..100%';
end;

function TFixLibAvailableChannel.SaveToString: string;
var prop: TProperties;
  i: integer;
begin
  prop.Init('|');
  prop.Add('ID', NameID);

  if Length(AliasOfNameID) > 0 then begin
    prop.Add('AliasOf', AliasOfNameID);
  end else begin
    prop.Add('Type', Ord(ChanType));
    if DefaultValue <> 0 then prop.Add('DefaultValue', DefaultValue);
    // we save range only if it's not an empty 0..255 range
    if (Length(Ranges) >= 1) and ((Ranges[0].EndValue <> 255) or
       (Ranges[0].Text <> '') or (Ranges[0].Extra <> '')) then
      for i:=0 to High(Ranges) do
        prop.Add('R'+(i+1).ToString, Ranges[i].SaveToString);
  end;

  Result := prop.PackedProperty;
end;

function TFixLibAvailableChannel.LoadFromString(const s: string): boolean;
var prop: TProperties;
  vi, i: integer;
  temp: string;
begin
  Result := False;
  vi := 0;
  temp := '';
  InitDefault;
  prop.Split(s, '|');

  try
    prop.StringValueOf('ID', NameID, NameID);

    if prop.StringValueOf('AliasOf', temp, '') then begin
      AliasOfNameID := temp;
      Result := True;
    end else begin
      prop.IntegerValueOf('Type', vi, Ord(ChanType));
      ChanType := TChannelType(vi);
      prop.IntegerValueOf('DefaultValue', vi, DefaultValue);
      DefaultValue := vi;
      // search the number of ranges present
      vi := 0;
      repeat
        inc(vi);
      until not prop.CheckProperties(['R'+vi.ToString]);

      Result := True;
      if vi-1 = 0 then begin
        // no range data found -> we force a single one
        SetLength(Ranges, 1);
        Ranges[0].InitDefault;
        Ranges[0].Text := '';
      end else begin
        // read range's data
        SetLength(Ranges, vi-1);
        for i:=0 to High(Ranges) do begin
          prop.StringValueOf('R'+(i+1).ToString, temp, '');
          Result := Result and Ranges[i].LoadFromString(temp);
        end;
      end;
    end;
  except
    On E :Exception do begin
      Log.Error('TFixLibAvailableChannel.LoadFromString - exception occurs while reading channel''s data "'+s+'"'+ E.Message, 3);
      Result := False;
    end;
  end;
end;

procedure TFixLibAvailableChannel.CopyTo(var aTarget: TFixLibAvailableChannel);
var i: integer;
begin
  aTarget.NameID := NameID;
  aTarget.AliasOfNameID := AliasOfNameID;
  aTarget.DefaultValue := DefaultValue;
  aTarget.ChanType := ChanType;
  SetLength(aTarget.Ranges, Length(Ranges));
  for i:=0 to High(Ranges) do
    Ranges[i].CopyTo(aTarget.Ranges[i]);
end;

function TFixLibAvailableChannel.HaveSwitchDescriptor: boolean;
var i: integer;
begin
  for i:=0 to High(Ranges) do
    if Ranges[i].HaveSwitch then exit(True);
  Result := False;
end;

function TFixLibAvailableChannel.HaveRangesError: boolean;
var i: integer;
  current: integer;
begin
  if Trim(NameID) = '' then exit(True);
  if Length(Ranges) = 0 then exit(True);
  current := 0;
  for i:=0 to High(Ranges) do begin
    if Ranges[i].BeginValue <> current then exit(True);
    if Ranges[i].BeginValue > Ranges[i].EndValue then exit(True);
    current := Ranges[i].EndValue + 1;
  end;
  if current <> 256 then exit(True);
  Result := False;
end;

function TFixLibAvailableChannel.IsAlias: boolean;
begin
  Result := Length(AliasOfNameID) > 0;
end;

{ TFixLibPhysical }

procedure TFixLibPhysical.InitDefault;
begin
  Width := 0;
  Height := 0;
  Depth := 0;
  Weight := '0';
  Power := 0;
  Connector := '';
  Bulb := '';
  Lumens := 0;
  Lens := '';
  LensMinDegree := 0;
  LensMaxDegree := 0;
end;

procedure TFixLibPhysical.CopyFrom(const Another: TFixLibPhysical);
begin
  Width := Another.Width;
  Height := Another.Height;
  Depth := Another.Depth;
  Weight := Another.Weight;
  Power := Another.Power;
  Connector := Another.Connector;
  Bulb := Copy(Another.Bulb, 1, Length(Another.Bulb));
  Lumens := Another.Lumens;
  Lens := Copy(Another.Lens, 1, Length(Another.Lens));
  LensMinDegree := Another.LensMinDegree;
  LensMaxDegree := Another.LensMaxDegree;
end;

procedure TFixLibPhysical.SaveTo(t: TStringList);
var prop: TProperties;
begin
  prop.Init('|');
  prop.Add('Width', Width);
  prop.Add('Height', Height);
  prop.Add('Depth', Depth);
  prop.Add('Weight', Weight);
  prop.Add('Power', Power);
  if Connector <> '' then prop.Add('Connector', Connector);
  if Bulb <> '' then prop.Add('Bulb', Bulb);
  if Lumens <> 0 then prop.Add('Lumens', Lumens);
  if Lens <> '' then prop.Add('Lens', Lens);
  if LensMinDegree <> 0 then prop.Add('LensMinDegree', LensMinDegree);
  if LensMaxDegree <> 0 then prop.Add('LensMaxDegree', LensMaxDegree);
  t.Add(SECTION_HEADER);
  t.Add(prop.PackedProperty);
end;

procedure TFixLibPhysical.LoadFrom(t: TStringList);
var prop: TProperties;
  k: integer;
begin
  InitDefault;
  k := t.IndexOf(SECTION_HEADER);

  if (k = -1) or (k = t.Count-1) then begin
    Log.Error('Header '+SECTION_HEADER+' not found', 1);
    exit;
  end;

  prop.Split(t.Strings[k+1], '|');
  DecodePropertyFrom(prop);
end;

procedure TFixLibPhysical.DecodePropertyFrom(const prop: TProperties);
begin
  prop.IntegerValueOf('Width', Width, Width);
  prop.IntegerValueOf('Height', Height, Height);
  prop.IntegerValueOf('Depth', Depth, Depth);
  prop.StringValueOf('Weight', Weight, Weight);
  prop.IntegerValueOf('Power', Power, Power);
  prop.StringValueOf('Connector', Connector, Connector);
  prop.StringValueOf('Bulb', Bulb, Bulb);
  prop.IntegerValueOf('Lumens', Lumens, Lumens);
  prop.StringValueOf('Lens', Lens, Lens);
  prop.SingleValueOf('LensMinDegree', LensMinDegree, LensMinDegree);
  prop.SingleValueOf('LensMaxDegree', LensMaxDegree, LensMaxDegree);
end;

{ TFixLibGeneral }

procedure TFixLibGeneral.InitDefault;
begin
  ManufacturerName := SNotInitialized;
  FixtureName := SNotInitialized;
  FixtureType := ftOther;
  Authors := SNotInitialized;
  Creator := SNotInitialized;
end;

procedure TFixLibGeneral.CopyFrom(const Another: TFixLibGeneral);
begin
  ManufacturerName := Another.ManufacturerName;
  FixtureName := Another.FixtureName;
  FixtureType := Another.FixtureType;
  Authors := Another.Authors;
  Creator := Another.Creator;
end;

procedure TFixLibGeneral.SaveTo(t: TStringList);
var prop: TProperties;
begin
  prop.Init('|');
  prop.Add('Manufacturer', ManufacturerName);
  prop.Add('Name', FixtureName);
  prop.Add('Type', Ord(FixtureType));
  prop.Add('Authors', Authors);
  prop.Add('Creator', Creator);
  t.Add(SECTION_HEADER);
  t.Add(prop.PackedProperty);
end;

function TFixLibGeneral.LoadFrom(t: TStringList): boolean;
var prop: TProperties;
  flagError: boolean;
  k, vi: integer;
  procedure LogMissingProperty(const aProp: string);
  var s: string;
  begin
    if not flagError then
      s := SECTION_HEADER+' property "'+aProp+'" not found in "'+t.Strings[k+1]+'"'
    else
      s := SECTION_HEADER+' property "'+aProp+'" not found';
    Log.Error(s, 1);
    flagError := True;
  end;

begin
  InitDefault;
  Result := False;
  k := t.IndexOf(SECTION_HEADER);
  if (k = -1) or (k = t.Count-1) then begin
    Log.Error('Header '+SECTION_HEADER+' not found or empty', 1);
    exit;
  end;

  flagError := False;
  prop.Split(t.Strings[k+1], '|');
  if not prop.StringValueOf('Manufacturer', ManufacturerName, SNotInitialized) then
    LogMissingProperty('Manufacturer');
  if not prop.StringValueOf('Name', FixtureName, SNotInitialized) then
    LogMissingProperty('Name');
  vi := Ord(FixtureType);
  if not prop.IntegerValueOf('Type', vi, vi) then LogMissingProperty('Type')
    else FixtureType := TFixtureType(vi);
  if not prop.StringValueOf('Authors', Authors, SNotInitialized) then
    LogMissingProperty('Authors');
  if not prop.StringValueOf('Creator', Creator, SNotInitialized) then
    LogMissingProperty('Creator');
  Result := not flagError;
end;

{ TDipSwitch }

procedure TDipSwitch.InitDefault;
begin
  OnIsUp := TRUE;
  SetLength(Functions, 0);
end;

procedure TDipSwitch.LoadFrom(aDipSwitch: TDipSwitch);
var i: integer;
begin
  OnIsUp := aDipSwitch.OnIsUp;
  MSBIsLeft := aDipSwitch.MSBIsLeft;
  SetLength(Functions, Length(aDipSwitch.Functions));
  for i:=0 to High(Functions) do
    Functions[i] := aDipSwitch.Functions[i];
end;

procedure TDipSwitch.LoadFrom(t: TStringList);
var k, vi: integer;
  prop: TProperties;
begin
  InitDefault;
  k := t.indexOf(DIPSWITCH_HEADER);
  if (k = -1) or (k = t.Count-1) then exit;

  try
    prop.Split(t.Strings[k+1], '|');
    prop.BooleanValueOf('OnIsUp', OnIsUp, OnIsUp);
    prop.BooleanValueOf('MSBIsLeft', MSBIsLeft, MSBIsLeft);
    k := 0;
    Functions := NIL;
    while prop.CheckProperties(['F'+k.ToString]) do begin
      vi := 0;
      prop.IntegerValueOf('F'+k.ToString, vi, vi);
      SetLength(Functions, Length(Functions)+1);
      Functions[high(Functions)] := TDipSwitchFunction(vi);
      inc(k);
    end;
  except
    InitDefault;
  end;
end;

procedure TDipSwitch.SaveTo(t: TStringList);
var i: integer;
  prop: TProperties;
begin
  if Length(Functions)=0 then exit;

  prop.Init('|');
  prop.Add('OnIsUp', OnIsUp);
  prop.Add('MSBIsLeft', MSBIsLeft);
  for i:=0 to High(Functions) do
    prop.Add('F'+i.ToString, Ord(Functions[i]));

  t.Add(DIPSWITCH_HEADER);
  t.Add(prop.PackedProperty);
end;

function TDipSwitch.AdressBitCount: integer;
var i: integer;
begin
  Result := 0;
  for i:=0 to High(Functions) do
    if Functions[i] = dsfAdress then inc(Result);
end;

{$endif}

