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
  // return True if AbsolutPath return an existing file and this file have the '.dmx' extension
  function IsFilled: boolean;
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


  { TFixLibSingleRange }

  TFixLibSingleRange = record
    BeginValue,
    EndValue: byte;
    Text,
    Extra: string;
    procedure InitDefault;
    function SaveToString: string;
    function LoadFromString(const s: string): boolean;
    procedure CopyTo(var aTarget: TFixLibSingleRange);
    procedure CopyTo(var aTarget: TChannelRange);
    function ToReadableString: string;
  end;
  PFixLibSingleRange = ^TFixLibSingleRange;

  { TFixLibAvailableChannel }

  PFixLibAvailableChannel = ^TFixLibAvailableChannel;
  TFixLibAvailableChannel = record
    NameID: string;
    DefaultValue: byte;
    ChanType: TChannelType;
    Ranges: array of TFixLibSingleRange;

    procedure InitDefault;
    function SaveToString: string;
    function LoadFromString(const s: string): boolean;
    procedure CopyTo(var aTarget: TFixLibAvailableChannel);
    // checks the integrity of each channel ranges
    function HaveError: boolean;
  end;
  TFixLibAvailableChannels = array of TFixLibAvailableChannel;
  PFixLibAvailableChannels = ^TFixLibAvailableChannels;

  { TFixLibMode }
  PFixLibMode = ^TFixLibMode;
  TFixLibMode = record
    Name,
    ShortName: string;
    ChannelsIDToUse: TStringArray;
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

  function GetChannelsForMode(const aMode: string): TFixLibAvailableChannels;
  function GetChannelCountForMode(const aMode: string): integer;
end;


procedure SaveAvailableChannelsTo(t: TStringList; p: PFixLibAvailableChannels);
function LoadAvailableChannelsFrom(t: TStringList; p: PFixLibAvailableChannels): boolean;
function IndexOfChannel(const aNameID: string; p: PFixLibAvailableChannels): integer;

procedure SaveModesTo(t: TStringList; p: PFixLibModes);
function LoadModesFrom(t: TStringList; p: PFixLibModes): boolean;
function IndexOfMode(const aModeName: string; p: PFixLibModes): integer;


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

procedure SaveModesTo(t: TStringList; p: PFixLibModes);
var i: integer;
begin
  t.Add('[MODES]');
  for i:=0 to High(p^) do
    t.Add(p^[i].SaveToString);

  t.Add('[END_MODES]');
end;

function LoadModesFrom(t: TStringList; p: PFixLibModes): boolean;
var i, k, kEnd: Integer;
begin
  p^ := Nil;
  k := t.IndexOf('[MODES]');
  kEnd := t.IndexOf('[END_MODES]');

  if (k = -1) or (kEnd = -1) or (kEnd <= k) or (kEnd-k < 2) then begin
    Log.Error('Header block [MODES] [END_MODES] not found or empty', 1);
    exit(False);
  end;

  try
    SetLength(p^, kEnd-k-1);
    for i:=0 to High(p^) do begin
      inc(k);
      p^[i].LoadFromString(t.Strings[k]);
    end;
    Result := True;
  except
    Result := False;
  end;
end;

function IndexOfMode(const aModeName: string; p: PFixLibModes): integer;
var i: integer;
begin
  for i:=0 to High(p^) do
    if p^[i].Name = aModeName then begin
      Result := i;
      exit;
    end;
  Result := -1;
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

function TFixtureLibraryLocation.IsFilled: boolean;
var f: string;
begin
  f := AbsolutPath;
  Result := FileExists(f) and (ExtractFileExt(f) = DMX_LIBRARY_FILE_EXTENSION);
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
end;

function TLibraryFixture.LoadFromFile(const aFileName: string): boolean;
var t: TStringList;
    i, m, k: integer;
    f: string;
begin
  InitDefault;
  Result := False;
  t := TStringList.Create;
  f := RepertoireParent(ExtractFilePath(aFileName));
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
      if not LoadModesFrom(t, @Modes) then exit(False);

      // Check if the channels ID used by the modes are defined
      for m:=0 to High(Modes) do
        for i:=0 to High(Modes[m].ChannelsIDToUse) do
          if IndexOfChannel(Modes[m].ChannelsIDToUse[i], @AvailableChannels) = -1 then begin
            Log.Error('Channel "'+Modes[m].ChannelsIDToUse[i]+'" used by mode "'+Modes[m].Name+'" is not defined !',1);
            exit(False);
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
var f: string;
begin
  f := ConcatPaths([GetAppDMXLibraryFolder + aLibraryLocation.RelativePathInLibrary]);
  f := aLibraryLocation.AbsolutPath;
  Result := LoadFromFile(f);
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
    SaveModesTo(t, @Modes);
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

function TLibraryFixture.GetChannelsForMode(const aMode: string): TFixLibAvailableChannels;
var i, j, k: integer;
begin
  Result := NIL;
  j := IndexOfMode(aMode, @Modes);
  if j = -1 then exit;
  SetLength(Result, Length(Modes[j].ChannelsIDToUse));

  for i:=0 to High(Result) do begin
    // retrieve the index of the channel name used in the mode
    k := IndexOfChannel(Modes[j].ChannelsIDToUse[i], @AvailableChannels);
    if k = -1 then begin
      Result := NIL;
      exit;
    end;
    // copy the content of AvailableChannels[k] into Result[i]
    AvailableChannels[k].CopyTo(Result[i]);
  end;
end;

function TLibraryFixture.GetChannelCountForMode(const aMode: string): integer;
var j: integer;
begin
  Result := 0;
  j := IndexOfMode(aMode, @Modes);
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
  if not prop.StringValueOf('ShortName', ShortName, Name) then LogError('ShortName');

  if not prop.StringValueOf('Content', temp, '') then LogError('Content');
  if Length(temp) > 0 then begin
    A := temp.Split(['~']);
    SetLength(ChannelsIDToUse, Length(A));
    for i:=0 to High(A) do
      ChannelsIDToUse[i] := A[i];
  end;
end;

{ TFixLibSingleRange }

procedure TFixLibSingleRange.InitDefault;
begin
  BeginValue := 0;
  EndValue := 255;
  Text := SNotInitialized;
  Extra := '';
end;

function TFixLibSingleRange.SaveToString: string;
var prop: TProperties;
begin
  prop.Init('~');
  prop.Add('Begin', BeginValue);
  prop.Add('End', EndValue);
  prop.Add('Txt', Text);
  if Extra <> '' then prop.Add('Extra', Extra);
  Result := prop.PackedProperty;
end;

function TFixLibSingleRange.LoadFromString(const s: string): boolean;
var prop: TProperties;
  vi: Integer;
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
    Result := True;
  except
    On E :Exception do begin
      Log.Error('exception occurs while reading range values "'+s+'"'+ E.Message, 3);
    end;
  end;
end;

procedure TFixLibSingleRange.CopyTo(var aTarget: TFixLibSingleRange);
begin
  aTarget.BeginValue := BeginValue;
  aTarget.EndValue := EndValue;
  aTarget.Text := Text;
  aTarget.Extra := Extra;
end;

procedure TFixLibSingleRange.CopyTo(var aTarget: TChannelRange);
begin
  aTarget.BeginValue := BeginValue;
  aTarget.EndValue := EndValue;
  aTarget.Text := Text;
  aTarget.Extra := Extra;
end;

function TFixLibSingleRange.ToReadableString: string;
begin
  Result := EncodeDMXChannelRange(BeginValue, EndValue, Text+' '+Extra);
end;


{ TFixLibAvailableChannel }

procedure TFixLibAvailableChannel.InitDefault;
begin
  NameID := SNotInitialized;
  DefaultValue := 0;
  ChanType := ctConfig;
end;

function TFixLibAvailableChannel.SaveToString: string;
var prop: TProperties;
  i: integer;
begin
  prop.Init('|');
  prop.Add('ID', NameID);
  prop.Add('Type', Ord(ChanType));
  if DefaultValue <> 0 then prop.Add('DefaultValue', DefaultValue);
  // we save range only if it's not an empty 0..255 range
  if (Length(Ranges) >= 1) and ((Ranges[0].EndValue <> 255) or
     (Ranges[0].Text <> '') or (Ranges[0].Extra <> '')) then
    for i:=0 to High(Ranges) do
      prop.Add('R'+(i+1).ToString, Ranges[i].SaveToString);

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
  aTarget.DefaultValue := DefaultValue;
  aTarget.ChanType := ChanType;
  SetLength(aTarget.Ranges, Length(Ranges));
  for i:=0 to High(Ranges) do
    Ranges[i].CopyTo(aTarget.Ranges[i]);
end;

function TFixLibAvailableChannel.HaveError: boolean;
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

