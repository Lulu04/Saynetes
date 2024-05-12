unit Unit1;

{$mode objfpc}{$H+}
{$modeSwitch advancedRecords}
{$modeSwitch typehelpers}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, Menus, fpjson, jsonparser, u_common;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CBConvertOnlyExisting: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    LB: TListBox;
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    PB: TProgressBar;
    Splitter1: TSplitter;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure DeleteEmptyFolderInConvertedFolder;
    procedure DeleteLBEntriesWith16BitRangeValues(aList: TStringList);
    procedure ConvertManufacturers;
    function OFLPath: string;
    function ConvertedPath: string;
    function SayneteDMXLibraryPath: string;
    function ManufacturerNameOf(const f: string): string;
    function Convert(const SrcFilename, DstFilename: string): boolean;
  public

  end;

var
  Form1: TForm1;

implementation
uses utilitaire_fichier, PropertyUtils, jsonscanner, u_list_dmxuniverse, u_logfile;

{$R *.lfm}

var
  FWheelsErrorCount,
  FMatrixErrorCount,
  FSwitchErrorCount: integer;
  FChannelNotTyped: TStringList;



procedure LogMessage(const aMess: string);
begin
  Form1.Memo1.Lines.Add(aMess);
  Application.ProcessMessages;
end;

type

  { TGeneral }

  TGeneral = record
    Manufacturer,
    Name,
    Authors,
    Creator: string;
    FixtureType: TFixtureType;
    procedure InitDefault;
    procedure SaveTo(t: TStringList);
  end;

  { TPhysical }

  TPhysical = record
    Width,
    Height,
    Depth,
    Power: integer;
    Weight: string;
    Connector,
    Bulb: string;
    Lumens: integer;
    Lens: string;
    LensMinDegree,
    LensMaxDegree: double;
    procedure InitDefault;
    function InitFrom(aItem: TJSONData): boolean;
    procedure SaveTo(t: TStringList);
  end;

  TWebLink = record
    LinkType,
    Url: string;
  end;
  TWebLinks = array of TWebLink;

  { TSlotDescriptor }

  TSlotDescriptor = record
    Txt,
    Extra: string;
    procedure InitDefault;
  end;

  { TWheel }

  TWheel = record
    SlotID: string;
    Slots: array of TSlotDescriptor;
    procedure InitDefault;
  end;

  { TSingleRange }

  TSingleRange = record
    BeginValue,
    EndValue: integer;
    Txt,
    Extra: string;

    procedure InitDefault;
    function SaveToString: string;
  end;
  TRanges = array of TSingleRange;


  { TAvailableChannel }

  PAvailableChannel = ^TAvailableChannel;
  TAvailableChannel = record
  private
    FID: string;
    procedure SetID(AValue: string);
  public
    BitWidthForNumber: integer; // 0 means total bit width of coarse plus fine(s) channel(s)
    DefaultValue: integer;
    ChanType: TChannelType;
    Ranges: TRanges;

    FineChannelIndexes: array of integer; // contains the index of the fine(s) channel aliases

    procedure InitDefault;
    procedure SetSingleRange(const aText: string);
    function HaveFineChannels: boolean;
    function SaveToString: string;
    property ID: string read FID write SetID;
  end;
  TAvailableChannels = array of TAvailableChannel;


type

  // for override some physical property in "modes"
  TProperty = record
    PropName,
    PropValue: string;
  end;

  { TFixtureMode }

  TFixtureMode = record
    Name,
    ShortName: string;
    ChannelsIDToUse: TStringArray;
    PhysicalOverridenProperty: array of TProperty;
    procedure InitDefault;
    procedure AddPhysicalOverridenProperty(const aPropName, aPropValue: string);
    function SaveToString: string;
  end;
  TFixtureModes = array of TFixtureMode;

  { TMatrix }

  TMatrix = record
    X, Y: integer;
    procedure InitDefault;
    procedure SaveTo(t: TStringList);
  end;

  { TFixtureFromLibrary }

  TFixtureFromLibrary = record
  private
    FSourceWheelIndex: integer;
    FChannelNotUsedIsAdded: boolean;
    FHaveColorChannel: boolean;
    // because OFL modes can have null entry to specify a not used channel...
    procedure AddNotUsedChannelIfNeeded;
  public
    Web: TWebLinks;
    General: TGeneral;
    Physical: TPhysical;
    Matrix: TMatrix;
    Wheels: array of TWheel;
    AvailableChannels: TAvailableChannels;
    Modes: TFixtureModes;
    procedure InitDefault;
    function ProcessItemCapability(aItem: TJSONData; aChanIndex: integer): boolean;
    function TranslateCapabilityType(aItem: TJSONData;
                 const aOFLType: string;
                 out chanType: TChannelType; out txt: string;
                 out extra: string): boolean;
    function TranslateSlotType(aItem: TJSONData;
                 const aOFLType: string;
                 {out chanType: TChannelType;} out txt: string;
                 out extra: string): boolean;
    function AddRangeFrom(aChanIndex: integer; aItem: TJSONData): boolean;

    function InitWheelsFrom(aItem: TJSONData): boolean;

    function InitAvailableChannelsfrom(aItem: TJSONData): boolean;
    function InitModesFrom(aArr: TJSONArray): boolean;
    function IndexOfAvailableChannelName(const aName: string): integer;
    function IndexOfWheel(const aWheelID: string): integer;

    procedure SaveTo(t: TStringList);
    function SaveTo(const aFilename: string): boolean;

    property HaveColorChannel: boolean read FHaveColorChannel; // used to set the right fixture type
  end;


{ TFixtureMode }

procedure TFixtureMode.InitDefault;
begin
  Name := '';
  ChannelsIDToUse := NIL;
  PhysicalOverridenProperty := NIL;
end;

procedure TFixtureMode.AddPhysicalOverridenProperty(const aPropName,
  aPropValue: string);
begin
  SetLength(PhysicalOverridenProperty, Length(PhysicalOverridenProperty)+1);
  with PhysicalOverridenProperty[High(PhysicalOverridenProperty)] do begin
    PropName := aPropName;
    PropValue := aPropValue;
  end;
end;

function TFixtureMode.SaveToString: string;
var prop: PropertyUtils.TProperties;
  s: string;
  i: integer;
begin
  prop.Init('|');

  prop.Add('Name', Name);
  if ShortName <> Name then prop.Add('ShortName', ShortName);

  s := '';
  for i:=0 to High(ChannelsIDToUse) do
  begin
    s := s + ChannelsIDToUse[i];
    if i < High(ChannelsIDToUse) then s := s+'~';
  end;
  prop.Add('Content', s);

  if Length(PhysicalOverridenProperty) > 0 then begin
    s := '';
    for i:=0 to High(PhysicalOverridenProperty) do begin
      s := s+PhysicalOverridenProperty[i].PropName+'~'+
             PhysicalOverridenProperty[i].PropValue;
      if i < High(PhysicalOverridenProperty) then s := s+'~';
    end;
    prop.Add('Physical', s);
  end;

  Result := prop.PackedProperty;
end;

{ TMatrix }

procedure TMatrix.InitDefault;
begin
  X := 1;
  Y := 1;
end;

procedure TMatrix.SaveTo(t: TStringList);
var s: string;
begin
  if (X=1) and (Y=1) then exit;
  t.Add('[MATRIX]');
  s := 'X|'+X.ToString+
       '|Y|'+Y.ToString;
  t.Add(s);
end;

{ TSlotDescriptor }

procedure TSlotDescriptor.InitDefault;
begin
  Txt := '';
  Extra := '';
end;

{ TWheel }

procedure TWheel.InitDefault;
begin
  SlotID := '';
  Slots := NIL;
end;


{ TSingleRange }

procedure TSingleRange.InitDefault;
begin
  BeginValue := 0;
  EndValue := 0;
  txt := '';
end;

function TSingleRange.SaveToString: string;
var prop: PropertyUtils.TProperties;
begin
  prop.Init('~');
  prop.Add('Begin', BeginValue);
  prop.Add('End', EndValue);
  prop.Add('Txt', txt);
  if Extra <> '' then prop.Add('Extra', Extra);
  Result := prop.PackedProperty;
end;

{ TAvailableChannel }

procedure TAvailableChannel.SetID(AValue: string);
var s: string;
begin
  FID := AValue;
  s := LowerCase(AValue);
  case s of
    'config', 'configuration', 'maintenance', 'mode', 'auto programs',
    'programs', 'program', 'auto program selection', 'reset', 'dimmer curve',
    'program speed / sound control', 'special functions', 'function', 'functions', 'internal programs',
    'function selection', 'control', 'factory reset',
    'built in programs', 'channel functions', 'led programs',
    'effect', 'effect speed / sound sensitivity', 'auto/sound programs',
    'lens function select', 'lens control', 'geometry', 'custom masking',
    'freeze', 'fixture control settings': ChanType := ctCONFIG;

    'master', 'master fine', 'master dimmer', 'master dimmer fine', 'master dimmer fine value',
    'master intensity', 'master intensity fine': ChanType := ctMASTERDIMMER;

    'dimmer', 'dimmer fine value', 'intensity', 'intensity fine', 'intensity fine value',
    'value', 'value fine', 'fog', 'haze', 'fan', 'light output', 'dimmer / strobe',
    'power', 'fade in', 'fade out', 'haze / fog', 'fog output', 'fog on/off',
    'fog emission', 'beam dimmer', 'output', 'hue', 'hue fine value',
    'saturation', 'saturation fine value', 'dimmer 2', 'dimmer 2 fine value': ChanType := ctDIMMER;

    'red', 'red fine', 'red fine value', 'red 1', 'red 2', 'beam red': ChanType := ctRED;
    'green', 'green fine', 'green fine value', 'green 1', 'green 2', 'beam green': ChanType := ctGREEN;
    'blue', 'blue fine', 'blue fine value', 'blue 1', 'blue 2', 'beam blue': ChanType := ctBLUE;

    'strobe', 'strobe fine', 'strobe effect', 'shutter / strobe', 'shutter',
    'led strobe', 'laser strobe', 'linear strobe', 'fx strobe', 'electronic shutter effect',
    'flash rate', 'strobe 16bit', 'strobe 16bit fine value': ChanType := ctSTROBE;

    'pan', 'pan fine', 'pan fine value', 'pan 1', 'pan 2', 'horizontal position',
    'lens h shift': ChanType := ctPAN;

    'tilt', 'tilt fine', 'tilt fine value', 'tilt 1', 'tilt 2', 'vertical position',
    'tilt spin', 'lens v shift', 'tilt all', 'tilt all fine value',
    'tilt 1 fine value', 'tilt 2 fine value', 'tilt 3', 'tilt 3 fine value',
    'tilt 4', 'tilt 4 fine value': ChanType := ctTILT;

    'pan/tilt speed', 'pan/tilt speed fine', 'pan/tilt macro speed': ChanType := ctPANTILTSPEED;
    'gobo', 'gobo all', 'raster pattern', 'red gobo', 'green gobo', 'blue gobo': ChanType := ctGOBO;
    'gobo rotation', 'gobo rotation fine': ChanType := ctGOBOROTATION;

    'color', 'colors', 'color macros', 'color presets', 'color walking',
    'virtual color wheel', 'constant color', 'cct', 'ctc', 'derby colors',
    'color programs', 'rgb macros', 'rgbw presets' ,'rgbw presets / chase',
    'led color saturation', 'color selection', 'color presets / temperature',
    'tint', 'user colors', 'beam ctc', 'cct fine value': ChanType := ctCOLORCHOICE;

    'white', 'white fine', 'white fine value', 'white 1', 'white 2',
    'natural white', 'natural white fine value', 'beam white': ChanType := ctWHITE;

    'amber', 'amber fine': ChanType := ctAMBER;

    'uv', 'uv fine': ChanType := ctUV;

    'speed', 'speed fine', 'function speed', 'color effect speed', 'effect speed',
    'color speed', 'derby speed', 'program speed', 'program duration', 'fade time',
    'auto speed', 'dimmer speed', 'movement speed', 'chase speed', 'led speed',
    'rotation speed', 'flash duration': ChanType := ctSPEED;

    'no function', 'notused': ChanType := ctNOFUNCTION;
    'cyan', 'cyan fine', 'cyan fine value': ChanType := ctCYAN;
    'magenta', 'magenta fine', 'magenta fine value': ChanType := ctMAGENTA;
    'yellow', 'yellow fine', 'yellow fine value': ChanType := ctYELLOW;
    'lime', 'lime fine': ChanType := ctLIME;
    'indigo', 'indigo fine': ChanType := ctINDIGO;
    'warm white', 'warm white fine', 'warm white fine value', 'intensity warm white': ChanType := ctWARMWHITE;
    'cold white', 'cold white fine', 'intensity cold white', 'intensitycoldwhite': ChanType := ctCOLDWHITE;
    'iris', 'iris fine': ChanType := ctIRIS;
    'blade insertion', 'blade insertion fine', 'blade', 'blade fine value': ChanType := ctBLADEINSERTION;

    'color temperature', 'color temperature fine', 'color temperature fine value',
    'color temperature 2', 'color temperature 2 fine value': ChanType := ctCOLORTEMPERATURE;

    'strobe speed', 'strobe speed fine', 'shutter speed', 'shutter speed fine': ChanType := ctSTROBESPEED;
    'sound sensitivity': ChanType := ctSOUNDSENSITIVITY;
    'blade rotation', 'blade rotation fine': ChanType := ctBLADEROTATION;
    'zoom', 'zoom fine', 'zoom fine value', 'lens zoom': ChanType := ctZOOM;
    'focus', 'focus fine', 'lens focus': ChanType := ctFOCUS;

    'rotation', 'rotation fine', 'motor rotation', 'rotating derby', 'rotating laser',
    'rotation red', 'rotation green', 'rotation blue': ChanType := ctROTATION;

    'pan speed', 'pan speed fine': ChanType := ctPANSPEED;
    'tilt speed', 'tilt speed fine': ChanType := ctTILTSPEED;
    else FChannelNotTyped.Add(s);
  end;
end;

procedure TAvailableChannel.InitDefault;
begin
  FID := '';
  BitWidthForNumber := 0;
  DefaultValue := 0;
  Ranges := NIL;
  FineChannelIndexes := NIL;
end;

procedure TAvailableChannel.SetSingleRange(const aText: string);
begin
  SetLength(Ranges, 1);
  Ranges[0].txt := aText;
  Ranges[0].BeginValue := 0;
  Ranges[0].EndValue := 255;
end;

function TAvailableChannel.HaveFineChannels: boolean;
begin
  Result := Length(FineChannelIndexes) > 0;
end;

function TAvailableChannel.SaveToString: string;
var prop: PropertyUtils.TProperties;
  i: integer;
  flag: boolean;
begin
  prop.Init('|');
  prop.Add('ID', ID);
  prop.Add('Type', Ord(ChanType));
  if DefaultValue <> 0 then prop.Add('DefaultValue', DefaultValue);

  // no data for an single empty range
  flag :=  (Length(Ranges) = 1) and (Ranges[0].BeginValue = 0) and
           (Ranges[0].EndValue = 255) and (Trim(Ranges[0].Txt) = '') and
           (Trim(Ranges[0].Extra) = '');

  if not flag then
    for i:=0 to High(Ranges) do
      prop.Add('R'+(i+1).ToString, Ranges[i].SaveToString);

  Result := prop.PackedProperty;
end;

{ TGeneral }

procedure TGeneral.InitDefault;
begin
  Manufacturer := '';
  Name := '';
  FixtureType := ftOther;
  Authors := '';
  Creator := '';
end;

procedure TGeneral.SaveTo(t: TStringList);
var prop: PropertyUtils.TProperties;
begin
  prop.Init('|');
  prop.Add('Manufacturer', Manufacturer);
  prop.Add('Name', Name);
  prop.Add('Type', Ord(FixtureType));
  if Authors <> '' then prop.Add('Authors', Authors);
  if Creator <> '' then prop.Add('Creator', Creator);
  t.Add('[GENERAL]');
  t.Add(prop.PackedProperty);
end;

{ TPhysical }

procedure TPhysical.InitDefault;
begin
  Width := 0;
  Height := 0;
  Depth := 0;
  Weight := '0';
  Power := 0;
  Lumens := 0;
  Connector := '';
  Bulb := '';
  Lens := '';
  LensMinDegree := 0;
  LensMaxDegree := 0;
end;

function TPhysical.InitFrom(aItem: TJSONData): boolean;
var sub, sub2: TJSONData;
  arr: TJSONArray;
  w: double;
begin
  Result := False;

  sub := aItem.FindPath('dimensions');
  if sub = NIL then
  begin
    //LogMessage('  PHYSICAL - "dimensions" not found');
    //exit;
  end
  else begin
    arr := TJSONArray(sub);
    Width := Arr.Items[0].AsInteger;
    Height := Arr.Items[1].AsInteger;
    Depth := Arr.Items[2].AsInteger;
  end;

  sub := aItem.FindPath('weight');
  if sub = NIL then
  begin
    //LogMessage('  PHYSICAL - "weight" not found');
    //exit;
  end
  else begin
    w := sub.AsFloat;
    Weight := FormatFloatWithDot('0.00', w);
  end;

  sub := aItem.FindPath('power');
  if sub = NIL then
  begin
   // LogMessage('  PHYSICAL - "power" not found');
    //exit;
  end
  else Power := sub.AsInteger;

  sub := aItem.FindPath('DMXconnector');
  if sub = NIL then
  begin
    //LogMessage('  PHYSICAL - "DMXconnector" not found');
    //exit;
  end
  else Connector := sub.AsString;

  sub := aItem.FindPath('bulb');
  if sub = NIL then
  begin
    //LogMessage('  PHYSICAL - "bulb" not found');
    //exit;
  end
  else begin
    sub2 := sub.FindPath('type');
    if sub2 = NIL then
    begin
      //LogMessage('  PHYSICAL - "bulb/type" not found');
      //exit;
    end
    else Bulb := sub2.AsString;
    sub2 := sub.FindPath('lumens');
    if sub2 <> NIL then
      Lumens := sub2.AsInteger;
  end;

  sub := aItem.FindPath('lens');
  if sub <> NIL then begin
    sub2 := sub.FindPath('name');
    if sub2 <> NIL then Lens := sub2.AsString;
    sub2 := sub.FindPath('degreesMinMax');
    if sub2 <> NIL then begin
      arr := TJSONArray(sub2);
      LensMinDegree := arr.Items[0].AsFloat;
      LensMaxDegree := arr.Items[1].AsFloat;
    end;
  end;

  Result := True;
end;

procedure TPhysical.SaveTo(t: TStringList);
var prop: PropertyUtils.TProperties;
begin
  prop.Init('|');
  if Width <> 0 then prop.Add('Width', Width);
  if Height <> 0 then prop.Add('Height', Height);
  if Depth <> 0 then prop.Add('Depth', Depth);
  if Weight <> '0' then prop.Add('Weight', Weight);
  if Power <> 0 then prop.Add('Power', Power);
  if Connector <> '' then prop.Add('Connector', Connector);
  if Bulb <> '' then prop.Add('Bulb', bulb);
  if Lumens <> 0 then prop.Add('Lumens', Lumens);
  if Lens <> '' then prop.Add('Lens', Lens);
  if LensMinDegree <> 0 then prop.Add('LensMinDegree', LensMinDegree);
  if LensMaxDegree <> 0 then prop.Add('LensMaxDegree', LensMaxDegree);

  t.Add('[PHYSICAL]');
  t.Add(prop.PackedProperty);
end;


{ TFixtureFromLibrary }

procedure TFixtureFromLibrary.InitDefault;
begin
  Web := NIL;
  General.InitDefault;
  Physical.InitDefault;
  AvailableChannels := NIL;
  Wheels := NIL;
  Matrix.InitDefault;
end;

function TFixtureFromLibrary.ProcessItemCapability(aItem: TJSONData; aChanIndex: integer): boolean;
var sub: TJSONData;
  i: integer;
  chanType: TChannelType;
  txt, extra, s: string;
begin
  Result := False;
  for i:=0 to aItem.Count-1 do
  begin
    sub := aItem.Items[i];
    s := TJSONObject(aItem).Names[i];
    case s of
      'type': begin
        Result := TranslateCapabilityType(aItem, sub.AsString, chanType, txt, extra);
        if not Result then exit;
        AvailableChannels[aChanIndex].ChanType := chanType;
        AvailableChannels[aChanIndex].SetSingleRange(txt);
      end;
    end;
  end;
end;

function TFixtureFromLibrary.TranslateCapabilityType(aItem: TJSONData; const aOFLType: string;
  out chanType: TChannelType; out txt: string; out extra: string): boolean;
var sub: TJSONData;
  prop: string;
  wheelIndex: Integer;
  procedure ConcatToText(const s: string);
  begin
    if Length(txt) = 0 then
      txt := s
    else
      txt := txt+' '+s;
  end;
  // search for this property only
  function HasSteppedProperty(const aPropName: string): boolean;
  begin
    prop := '';
    sub := aItem.FindPath(aPropName);
    Result := sub <> NIL;
    if Result then prop := sub.AsString;
  end;
  // search for this property + propertyStart + propertyEnd
  function HasRangedProperty(const aPropName: string): boolean;
  begin
    prop := '';
    Result := False;
    sub := aItem.FindPath(aPropName);
    Result := Result or (sub <> NIL);
    if Result then prop := sub.AsString;

    sub := aItem.FindPath(aPropName+'Start');
    Result := Result or (sub <> NIL);
    if sub <> NIL then begin
      if prop <> '' then prop := prop+' ';
      prop := prop+sub.AsString;
      sub := aItem.FindPath(aPropName+'End');
      if sub <> NIL then begin
        if prop <> '' then prop := prop+'..';
        prop := prop+sub.AsString;
      end;
    end;
  end;
  // search for this propertyStart + propertyEnd, not property !
  function HasRangedProperty_OnWheel(const aPropName: string): boolean;
  var slotStart, slotEnd: integer;
  begin
    prop := '';
    slotStart := -1;
    slotEnd := -1;
    Result := False;

    sub := aItem.FindPath(aPropName+'Start');
    Result := Result or (sub <> NIL);
    if sub <> NIL then
    begin
      slotStart := sub.AsInteger-1;
      sub := aItem.FindPath(aPropName+'End');
      Result := Result or (sub <> NIL);
      if sub <> NIL then
      begin
        slotEnd := sub.AsInteger-1;
        prop := Wheels[wheelIndex].Slots[slotStart].Txt;
        if slotEnd <= High(Wheels[wheelIndex].Slots) then
          prop := prop+'..'+Wheels[wheelIndex].Slots[slotEnd].Txt;
      end;
    end;
  end;

  function HasArrayProperty(const aPropName: string): boolean;
  var i: integer;
    arr: TJSONArray;
  begin
    prop := '';
    Result := False;
    sub := aItem.FindPath(aPropName);
    Result := sub <> NIL;
    if not Result then exit;
    Result := sub.JSONType = jtArray;
    if Result then begin
      arr := TJSONArray(sub);
      for i:=0 to arr.Count-1 do
        if i=0 then prop := arr.Items[i].AsString
          else prop := '_'+arr.Items[i].AsString;
    end;
  end;
  procedure CheckPropertyWheel;
  begin
    wheelIndex := -1;
    if HasSteppedProperty('wheel') then
      wheelIndex := IndexOfWheel(prop);
    if wheelIndex = -1 then wheelIndex := FSourceWheelIndex;
    if wheelIndex = -1 then begin
      LogMessage('Capability "WheelSlot" - Impossible to retrieve the target wheel''s name');
      Result := False;
    end;
  end;
  procedure CheckPropertySlotNumber;
  var iSlot: integer;
    //fSlot: single;
  begin
    if HasSteppedProperty('slotNumber')  then begin
      if not TryStrToInt(prop, iSlot) then begin
        iSlot := Trunc(sub.AsFloat)-1;
        //fSlot := sub.AsFloat; // StringToSingle(prop);
        //iSlot := Trunc(fSlot);
        ConcatToText('Split '+Wheels[wheelIndex].Slots[iSlot].Txt);
        if iSlot+1 <= High(Wheels[wheelIndex].Slots) then
          txt := txt+' / '+Wheels[wheelIndex].Slots[iSlot+1].Txt;
        extra := Wheels[wheelIndex].Slots[iSlot].Extra;
      end else begin
        dec(iSlot);
        ConcatToText(Wheels[wheelIndex].Slots[iSlot].Txt);
        extra := Wheels[wheelIndex].Slots[iSlot].Extra;
      end;
    end
    else if HasRangedProperty_OnWheel('slotNumber') then ConcatToText(prop);
  end;

begin
  Result := True;
  txt := '';
  extra := '';
  case aOFLType of
    'NoFunction': begin
      chanType := ctNOFUNCTION;
      txt := 'No function';
    end;
    'ShutterStrobe': begin
      chanType := ctSTROBE;
      if HasSteppedProperty('shutterEffect') then ConcatToText(prop);
      if HasSteppedProperty('soundControlled') then ConcatToText('Sound controlled');
      if HasRangedProperty('speed') then ConcatToText(prop);
      if HasRangedProperty('duration') then ConcatToText(prop);
      if HasSteppedProperty('randomTiming') then ConcatToText('Random timing');
      if txt = '' then txt := 'Strobe';
    end;
    'StrobeSpeed': begin
      chanType := ctSTROBESPEED;
      if HasRangedProperty('speed') then ConcatToText(prop)
      else txt := 'Strobe speed';
    end;
    'StrobeDuration': begin
      chanType := ctSTROBESPEED;
      if HasRangedProperty('duration') then ConcatToText(prop)
      else txt := 'Strobe duration';
    end;
    'Intensity': begin
      chanType := ctMASTERDIMMER;
      if HasRangedProperty('brightness') then ConcatToText(prop)
      else txt := '0..100%';
    end;
    'ColorIntensity': begin
      chanType := ctDimmer; // default
      if HasSteppedProperty('color') then begin
        txt := prop;
        case prop of
          'Red': begin
            chanType := ctRED;
            txt := '0..100%';
          end;
          'Green': begin
            chanType := ctGREEN;
            txt := '0..100%';
          end;
          'Blue': begin
            chanType := ctBLUE;
            txt := '0..100%';
          end;
          'Cyan': begin
            chanType := ctCYAN;
            txt := '0..100%';
          end;
          'Magenta': begin
            chanType := ctMAGENTA;
            txt := '0..100%';
          end;
          'Yellow': begin
            chanType := ctYELLOW;
            txt := '0..100%';
          end;
          'Amber': begin
            chanType := ctAMBER;
            txt := '0..100%';
          end;
          'White': begin
            chanType := ctWHITE;
            txt := '0..100%';
          end;
          'Warm White': begin
            chanType := ctWARMWHITE;
            txt := '0..100%';
          end;
          'Cold White': begin
            chanType := ctCOLDWHITE;
            txt := '0..100%';
          end;
          'UV': begin
            chanType := ctUV;
            txt := '0..100%';
          end;
          'Lime': begin
            chanType := ctLIME;
            txt := '0..100%';
          end;
          'Indigo': begin
            chanType := ctINDIGO;
            txt := '0..100%';
          end;
        end;
      end;
      if HasRangedProperty('brightness') then ConcatToText(prop);
      if txt = '' then txt := 'Color intensity';
    end;
    'ColorPreset': begin
      chanType := ctCOLORCHOICE;
      if HasRangedProperty('color') then txt := 'Color';
      if HasArrayProperty('colors') then
        extra := prop;
      if HasRangedProperty('colorTemperature') then ConcatToText('Color temperature '+prop);
      if (txt = '') and (extra='') then txt := 'Preset';
    end;
    'ColorTemperature': begin
      chanType := ctCOLORTEMPERATURE;
      txt := ''; //'Color Temperature';
      if HasRangedProperty('colorTemperature') then ConcatToText(prop);
    end;
    'Pan': begin
      chanType := ctPAN;
      if HasRangedProperty('angle') then ConcatToText(prop);
      if txt = '' then txt := 'Angle';
    end;
    'PanContinuous': begin
      chanType := ctPAN;
      txt := 'Continuous';
      if HasRangedProperty('speed') then ConcatToText(prop);
    end;
    'Tilt': begin
      chanType := ctTILT;
      if HasRangedProperty('angle') then ConcatToText(prop);
      if txt = '' then txt := 'Angle';
    end;
    'TiltContinuous': begin
      chanType := ctTilt;
      txt := 'Continuous';
      if HasRangedProperty('speed') then ConcatToText(prop);
    end;
    'PanTiltSpeed': begin
      chanType := ctPANTILTSPEED;

      if HasRangedProperty('speed') then ConcatToText(prop);
      if HasRangedProperty('duration') then ConcatToText(prop);
      if txt = '' then txt := 'Pan/Tilt speed';
    end;
    'WheelSlot': begin
      chanType := ctConfig;
      CheckPropertyWheel;
      CheckPropertySlotNumber;

      if txt = '' then txt := 'Wheel';
    end;

    'WheelShake': begin
      chanType := ctGOBOROTATION;   // not finished
      if HasSteppedProperty('isShaking')  then begin
        if prop = 'wheel' then ConcatToText('Shake all')
        else ConcatToText('Shake range');
      end;
      CheckPropertyWheel;
      CheckPropertySlotNumber;
      if HasRangedProperty('shakeSpeed') then ConcatToText(prop);
      if HasRangedProperty('shakeAngle') then ConcatToText(prop);

      if txt = '' then txt := 'Shake';
    end;

    'WheelSlotRotation': begin
      chanType := ctGOBOROTATION;
      CheckPropertyWheel;
      if HasSteppedProperty('slotNumber')  then begin
        ConcatToText(Wheels[wheelIndex].Slots[prop.ToInteger-1].Txt);
        extra := Wheels[wheelIndex].Slots[prop.ToInteger-1].Extra;
      end
      else if HasRangedProperty_OnWheel('slotNumber') then ConcatToText(prop);
      if HasRangedProperty('speed') then ConcatToText(prop);
      if HasRangedProperty('angle') then ConcatToText(prop);
      if txt = '' then txt := 'Wheel rotation';
    end;

    'WheelRotation': begin
      chanType := ctGOBOROTATION;
      CheckPropertyWheel;
      if HasRangedProperty('speed') then ConcatToText(prop);
      if HasRangedProperty('angle') then ConcatToText(prop);
      if txt = '' then txt := 'Wheel rotation';
    end;

    'Effect': begin
      chanType := ctConfig;
      if HasSteppedProperty('effectName') then ConcatToText(prop);
      if HasSteppedProperty('effectPreset') then ConcatToText(prop);
      if HasRangedProperty('speed') then ConcatToText(prop);
      if HasRangedProperty('duration') then ConcatToText(prop);
      if HasRangedProperty('parameter') then ConcatToText(prop);
      if HasSteppedProperty('soundControlled') and (prop='true') then ConcatToText('Sound controlled');
      if HasRangedProperty('soundSensitivity') then ConcatToText(prop);
      if txt = '' then txt := 'Effect';
    end;
    'EffectSpeed': begin
      chanType := ctSPEED;
      if HasRangedProperty('speed') then ConcatToText(prop);
      if txt = '' then txt := 'Effect speed';
    end;
    'EffectDuration': begin
      chanType := ctSPEED;
      if HasRangedProperty('duration') then ConcatToText(prop);
      if txt = '' then txt := 'Effect duration';
    end;
    'EffectParameter': begin
      chanType := ctCONFIG;

      if HasRangedProperty('parameter') then ConcatToText(prop);
      if txt = '' then txt := 'Effect parameter';
    end;
    'SoundSensitivity': begin
      chanType := ctSOUNDSENSITIVITY;
      txt := 'Sound sensitivity';
      if HasRangedProperty('soundSensitivity') then ConcatToText(prop);
    end;
    'BeamAngle': begin
      chanType := ctCONFIG;
      txt := 'Beam angle';
      if HasRangedProperty('angle') then ConcatToText(prop);
    end;
    'BeamPosition': begin
      chanType := ctCONFIG;
      txt := 'Beam position';
      if HasRangedProperty('horizontalAngle') then ConcatToText(prop);
      if HasRangedProperty('verticalAngle') then ConcatToText(prop);
    end;
    'Focus': begin
      chanType := ctFOCUS;
      txt := 'Focus';
      if HasRangedProperty('distance') then ConcatToText(prop);
    end;
    'Zoom': begin
      chanType := ctZOOM;
      txt := 'Zoom';
      if HasRangedProperty('angle') then ConcatToText(prop);
    end;
    'Iris': begin
      chanType := ctIRIS;
      txt := 'Iris';
      if HasRangedProperty('openPercent') then ConcatToText('open '+prop);
    end;
    'IrisEffect': begin
      chanType := ctIRIS;
      //txt := 'Iris effect';
      if HasSteppedProperty('effectName') then ConcatToText(prop);
      if HasRangedProperty('speed') then ConcatToText(prop);
    end;
    'Frost': begin
      chanType := ctCONFIG;
      txt := 'Frost';
      if HasRangedProperty('frostIntensity') then ConcatToText(prop);
    end;
    'FrostEffect': begin
      chanType := ctCONFIG;
      txt := 'Frost effect';
      if HasSteppedProperty('effectName') then ConcatToText(prop);
      if HasRangedProperty('speed') then ConcatToText(prop);
    end;
    'Prism': begin
      chanType := ctCONFIG;
      txt := 'Prism';
      if HasRangedProperty('speed') then ConcatToText(prop);
      if HasRangedProperty('angle') then ConcatToText(prop);
    end;
    'PrismRotation': begin
      chanType := ctROTATION;
      txt := 'Prism rotation';
      if HasRangedProperty('speed') then ConcatToText(prop);
      if HasRangedProperty('angle') then ConcatToText(prop);
    end;
    'BladeInsertion': begin
      chanType := ctBLADEINSERTION;
      txt := 'Blade insertion';
      if HasSteppedProperty('blade') then ConcatToText(prop);
      if HasRangedProperty('insertion') then ConcatToText(prop);
    end;
    'BladeRotation': begin
      chanType := ctBLADEROTATION;
      txt := 'Blade rotation';
      if HasSteppedProperty('blade') then ConcatToText(prop);
      if HasRangedProperty('angle') then ConcatToText(prop);
    end;
    'BladeSystemRotation': begin
      chanType := ctBLADEROTATION;
      txt := 'Blade system rotation';
      if HasRangedProperty('angle') then ConcatToText(prop);
    end;
    'Fog': begin
      chanType := ctCONFIG;
      if HasSteppedProperty('fogType') then ConcatToText(prop)
      else txt := 'Fog';
      if HasRangedProperty('fogOutput') then ConcatToText(prop);
    end;
    'FogOutput': begin
      chanType := ctCONFIG;
      txt := 'Fog output';
      if HasRangedProperty('fogOutput') then ConcatToText(prop);
    end;
    'FogType': begin
      chanType := ctCONFIG;
      if HasSteppedProperty('fogType') then ConcatToText(prop)
      else txt := 'Fog/Haze';
    end;
    'Rotation': begin
      chanType := ctROTATION;
      txt := 'Rotation';
      if HasRangedProperty('speed') then ConcatToText(prop);
      if HasRangedProperty('angle') then ConcatToText(prop);
    end;
    'Speed': begin
      chanType := ctSPEED;
      txt := 'Speed';
      if HasRangedProperty('speed') then ConcatToText(prop);
    end;
    'Time': begin
      chanType := ctSPEED;
      txt := 'Time';
      if HasRangedProperty('time') then ConcatToText(prop);
    end;
    'Maintenance': begin
      chanType := ctCONFIG;
      //txt := 'Maintenance';
      if HasRangedProperty('parameter') then ConcatToText(prop);
      if HasSteppedProperty('hold') then ConcatToText(prop);
    end;
    'Generic': begin
      chanType := ctCONFIG;
      //txt := 'Generic';
    end;
    else Result := False;
  end;

  if HasRangedProperty('comment') then ConcatToText(prop);
end;

function TFixtureFromLibrary.TranslateSlotType(aItem: TJSONData;
  const aOFLType: string; {out chanType: TChannelType;} out txt: string; out extra: string): boolean;
var sub: TJSONData;
  prop: string;
  // search for this property only
  function HasSteppedProperty(const aPropName: string): boolean;
  begin
    prop := '';
    sub := aItem.FindPath(aPropName);
    Result := sub <> NIL;
    if Result then prop := sub.AsString;
  end;
  // search for 'resource' property node
  function HasSteppedPropertyGoboResource: boolean;
  var sub1: TJSONData;
  begin
    prop := '';
    sub := aItem.FindPath('resource');
    Result := sub <> NIL;
    if Result then begin
      sub1 := sub.FindPath('name');
      if sub1 <> NIL then prop := sub1.AsString;
    end;
  end;

  // search for this property + propertyStart + propertyEnd
  function HasRangedProperty(const aPropName: string): boolean;
  begin
    prop := '';
    Result := False;
    sub := aItem.FindPath(aPropName);
    Result := Result or (sub <> NIL);
    if Result then prop := sub.AsString;

    sub := aItem.FindPath(aPropName+'Start');
    Result := Result or (sub <> NIL);
    if sub <> NIL then begin
      if prop <> '' then prop := prop+' ';
      prop := prop+sub.AsString;
      sub := aItem.FindPath(aPropName+'End');
      if sub <> NIL then begin
        if prop <> '' then prop := prop+'..';
        prop := prop+sub.AsString;
      end;
    end;
  end;
  function HasArrayProperty(const aPropName: string): boolean;
  var i: integer;
    arr: TJSONArray;
  begin
    prop := '';
    Result := False;
    sub := aItem.FindPath(aPropName);
    Result := sub <> NIL;
    if not Result then exit;
    Result := sub.JSONType = jtArray;
    if Result then begin
      arr := TJSONArray(sub);
      for i:=0 to arr.Count-1 do
        if i=0 then prop := arr.Items[i].AsString
          else prop := '_'+arr.Items[i].AsString;
    end;
  end;

  procedure ConcatToText(const s: string);
  begin
    if Length(txt) = 0 then
      txt := s
    else
      txt := txt+' '+s;
  end;

begin
  Result := True;
  txt := '';
  extra := '';
  case aOFLType of
    'Open': begin
      //chanType := ctCONFIG;
      txt := 'Open';
      if HasRangedProperty('fogOutput') then ConcatToText(prop);
    end;
    'Closed': begin
      txt := 'Closed';
    end;
    'Color': begin
      if HasSteppedProperty('name') then ConcatToText(prop);
      if HasArrayProperty('colors') then extra := prop;
      if txt = '' then txt := 'Color';
    end;
    'Gobo': begin
      if HasSteppedProperty('name') then ConcatToText(prop);
      if HasSteppedPropertyGoboResource then extra := prop;
      if txt = '' then txt := 'Gobo';
    end;
    'Prism': begin
      if HasSteppedProperty('name') then ConcatToText(prop);
      if HasSteppedProperty('facets') then extra := prop;
      if txt = '' then txt := 'Prism';
    end;
    'Iris': begin
      if HasSteppedProperty('openPercent') then ConcatToText(prop);
      if txt = '' then txt := 'Iris';
    end;
    'Frost': begin
      if HasSteppedProperty('frostIntensity') then ConcatToText(prop);
      if txt = '' then txt := 'Frost';
    end;
    'AnimationGoboStart': begin
      if HasSteppedProperty('name') then ConcatToText(prop);
      if txt = '' then txt := 'Gobo animation START';
    end;
    'AnimationGoboEnd': begin
      ConcatToText('Gobo animation END');
    end
    else Result := False;
  end;//case

end;

function TFixtureFromLibrary.AddRangeFrom(aChanIndex: integer; aItem: TJSONData): boolean;
var
  i, iRange: Integer;
  sub: TJSONData;
  arr: TJSONArray;
  s, txt, extra: String;
  chanType: TChannelType;
begin
  Result := False;

  for i:=0 to aItem.Count-1 do  // parse each property of a range
  begin
    sub := aItem.Items[i];
    s := TJSONObject(aItem).Names[i];
    case s of

      'dmxRange': begin
        SetLength(AvailableChannels[aChanIndex].Ranges, Length(AvailableChannels[aChanIndex].Ranges)+1);
        iRange := High(AvailableChannels[aChanIndex].Ranges);
        AvailableChannels[aChanIndex].Ranges[iRange].InitDefault;
        arr := TJSONArray(sub);
        if arr.Count = 2 then
        begin
          AvailableChannels[aChanIndex].Ranges[iRange].BeginValue := arr.Items[0].AsInteger;
          AvailableChannels[aChanIndex].Ranges[iRange].EndValue := arr.Items[1].AsInteger;
        end
        else begin
          LogMessage('Capabilities/dmxRange: array doesn''t have 2 items "'+aItem.AsJSON+'"');
          exit;
        end;
      end;

      'type': begin
        if TranslateCapabilityType(aItem, sub.AsString, chanType, txt, extra) then
        begin
          AvailableChannels[aChanIndex].Ranges[iRange].txt := txt;
          AvailableChannels[aChanIndex].Ranges[iRange].Extra := extra;
        end
        else begin
          LogMessage('Capabilities/type: Fail to retrieve information "'+sub.AsString+'"');
          exit;
        end;
      end;

      'switchChannels': begin
        LogMessage('-> HAVE SWITCH CHANNEL');
        inc(FSwitchErrorCount);
        exit;
      end;
    end;
  end;
  Result := True;
end;

function TFixtureFromLibrary.InitWheelsFrom(aItem: TJSONData): boolean;
var item, sub, sub1: TJSONData;
  arr: TJSONArray;
  i, j, k, iSlot: integer;
  s, s1, txt, extra: string;
begin
  Result := False;

  Wheels := NIL;
  for i:=0 to aItem.Count-1 do  // parse all wheel definition
  begin
    SetLength(Wheels, Length(Wheels)+1);
    Wheels[i].InitDefault;
    item := aItem.Items[i];
    s := TJSONObject(aItem).Names[i];
    Wheels[i].SlotID := s;

    for j:=0 to item.Count-1 do // parse one wheel definition
    begin
      sub := item.Items[j];
      s := TJSONObject(item).Names[j];
      case s of
        'slots': begin
          arr := TJSONArray(sub);
          for k:=0 to arr.Count-1 do
          begin
            sub1 := arr.Items[k];
            sub1 := sub1.FindPath('type');
            s1 := sub1.AsString;
            if not TranslateSlotType(arr.Items[k], s1, txt, extra) then exit(False);
            if txt = 'Gobo' then txt := txt + ' '+(iSlot+1).ToString;
            iSlot := Length(Wheels[i].Slots);
            SetLength(Wheels[i].Slots, iSlot+1);
            Wheels[i].Slots[iSlot].InitDefault;
            Wheels[i].Slots[iSlot].Txt := txt;
            Wheels[i].Slots[iSlot].Extra := extra;
          end;
        end;
        'direction': begin
          LogMessage('InitWheelsFrom - property "direction" found but not understood');
          exit;
        end
        else raise Exception.Create('InitWheelsFrom - found property "'+s+'" not implemented');
      end;//case
    end;// for j
  end;// for i
  Result := True;

  // DEBUG: dump the wheel data
  if Result then begin
    LogMessage('InitWheelsFrom() - '+IntToStr(Length(Wheels))+' wheels defined');
    for i:=0 to High(Wheels) do begin
      LogMessage('  '+Wheels[i].SlotID);
      s := '';
      for j:=0 to high(Wheels[i].Slots) do begin
        LogMessage('    Txt="'+Wheels[i].Slots[j].Txt+'"');
        LogMessage('    Extra="'+Wheels[i].Slots[j].Extra+'"');
      end;
    end;
  end;
end;

function TFixtureFromLibrary.InitAvailableChannelsfrom(aItem: TJSONData): boolean;
var item, sub, subCap: TJSONData;
  arr: TJSONArray;
  i, j, k, fineIndex, iChan: integer;
  s, s1: string;
//  defv: integer;
  v: double;
  coarseChannel: PAvailableChannel;
  coarseChannelIndex: integer;
  rawDefaultValue: DWord;
  procedure CreateNewChannel;
  begin
    iChan := Length(AvailableChannels);
    SetLength(AvailableChannels, ichan+1);
    AvailableChannels[iChan].InitDefault;
  end;

begin
  Result := True;

  AvailableChannels := NIL;

  iChan := 0;
  for i:=0 to aItem.Count-1 do  // parse all available channels
  begin
    CreateNewChannel;
    coarseChannel := @AvailableChannels[iChan]; // keep the instance of the coarse channel
    coarseChannelIndex := iChan;

    item := aItem.Items[i];

    s := TJSONObject(aItem).Names[i];
    coarseChannel^.ID := s;  // try also to init the channel type

    s1 := Lowercase(s);
    FHaveColorChannel := (s1 ='red') or (s1 = 'green') or (s1 = 'blue') or (s1 = 'amber') or
                         (s1 = 'uv') or (s1 = 'cyan') or (s1 = 'magenta') or (s1 = 'yellow') or
                         (s1 = 'lime') or (s1 = 'indigo');

    if (s1 = 'saturation') or (s1 = 'hue') then coarseChannel^.ChanType := ctDIMMER;

    // check if the name of the channel is the same as a wheel
    FSourceWheelIndex := IndexOfWheel(coarseChannel^.ID);

    for j:=0 to item.Count-1 do   // parse fields of the channel
    begin
      sub := item.Items[j];
      s := TJSONObject(item).Names[j];
      case s of
        'defaultValue': begin
          case sub.JSONType of
            jtNumber: coarseChannel^.DefaultValue := sub.AsInteger;
            jtString: begin
              s1 := sub.AsString;
              if s1.Contains('%') then begin
                Delete(s1, Pos('%', s1), Length(s1)-Pos('%', s1)+1);
                case Length(coarseChannel^.FineChannelIndexes) of
                  0: v := $FF;
                  1: v := $FFFF;
                  2: v := $FFFFFF;
                  3: v := $FFFFFFFF;
                end;
                coarseChannel^.DefaultValue := Round(StringToSingle(s1)/100*v);
              end
              else begin
                Raise Exception.Create('Not implemented!!');
                Result := False;
              end;
            end;
          end;
        end;
        'constant':;// the channels must be set to a constant value
        'precedence':; // not yet understand what it is...
        'highlightValue':; // not implemented

        'fineChannelAliases': begin // array of name
          rawDefaultValue := coarseChannel^.DefaultValue;

          arr := TJSONArray(sub);
          SetLength(coarseChannel^.FineChannelIndexes, arr.Count);

          for k:=0 to arr.Count-1 do begin
            CreateNewChannel;
            coarseChannel := @AvailableChannels[coarseChannelIndex]; // necessary !
            coarseChannel^.FineChannelIndexes[k] := iChan; // keep the index of the fine channel
          end;
        end;

        'dmxValueResolution': begin
          case sub.AsString of
            '8bit': coarseChannel^.BitWidthForNumber := 8;
           else begin
             LogMessage('InitAvailableChannelsFrom - property "dmxValueResolution" value not supported "'+
                         sub.AsString+'"');
             Result := False;
           end;
          end;
        end;

        'capability': begin   // channel with single range
          Result := ProcessItemCapability(sub, coarseChannelIndex); //iChan);   //i);
        end;

        'capabilities': begin  // channel with multiple ranges
          coarseChannel^.Ranges := NIL;
          arr := TJSONArray(sub);
          for k:=0 to arr.Count-1 do begin // parse each range
            subCap := arr.Items[k];
            if not AddRangeFrom(coarseChannelIndex, subCap) then begin //(iChan, subCap) then begin
              LogMessage('InitAvailableChannelsFrom() - Fail to retrieve range info "'+subCap.AsJSON);
              Result := False;
            end;
            if not Result then break;
          end;
        end;
      end;
      if not Result then break;

    end;// for j parse channel fields

    if coarseChannel^.HaveFineChannels then begin
      // initialize fine channels
LogMessage('COARSE CHANNEL: ChanType:'+IntToStr(Ord(coarseChannel^.ChanType)));
      for k:=High(coarseChannel^.FineChannelIndexes) downto 0 do begin
        fineIndex := coarseChannel^.FineChannelIndexes[k]; // index of a fine channel
        AvailableChannels[fineIndex].ChanType := coarseChannel^.ChanType;
        AvailableChannels[fineIndex].ID := coarseChannel^.ID+' fine';
LogMessage('setting type:'+IntToStr(Ord(coarseChannel^.ChanType))+' on '+AvailableChannels[fineIndex].ID);
        // distribute the default value on coarse and fine channels
        AvailableChannels[fineIndex].DefaultValue := (rawDefaultValue div ($100 shl (8*Length(coarseChannel^.FineChannelIndexes)))) {and $000000FF};
        rawDefaultValue := rawDefaultValue shr 8;
      end;
      coarseChannel^.DefaultValue := rawDefaultValue and $FF;
    end;


    if not Result then break;
  end;
end;

procedure TFixtureFromLibrary.AddNotUsedChannelIfNeeded;
begin
  if FChannelNotUsedIsAdded then exit;
  FChannelNotUsedIsAdded := True;
  // add a 'Not used' channel
  SetLength(AvailableChannels, Length(AvailableChannels)+1);
  with AvailableChannels[High(AvailableChannels)] do begin
    InitDefault;
    ID := 'NotUsed';
    ChanType := ctNOFUNCTION;
    SetSingleRange('No function');
  end;
end;

function TFixtureFromLibrary.InitModesFrom(aArr: TJSONArray): boolean;
var
  i, j, vi: Integer;
  sub, sub1: TJSONData;
  arr: TJSONArray;
  s: String;
  itemEnum, itemEnum1, itemEnum2 : TJSONEnum;
  vs: TJSONStringType;
  vf: TJSONFloat;
begin
  Result := False;

  Modes := NIL;
  SetLength(Modes, aArr.Count);

  i := 0;
  for itemEnum in aArr do  // parse each mode
  begin
    Modes[i].InitDefault;
//LogMessage('name');
    sub := itemEnum.Value.FindPath('name');
    if sub = NIL then begin
      LogMessage('  MODES - "name" not found "'+TJSONData(aArr).AsString+'"');
      exit;
    end;
    Modes[i].Name := sub.AsString;

// LogMessage('shortName');
    Modes[i].ShortName := Modes[i].Name;
    sub := itemEnum.Value.FindPath('shortName');
    if sub <> NIL then
      Modes[i].ShortName := sub.AsString;


    sub := itemEnum.Value.FindPath('physical');
    if sub <> NIL then begin
      for itemEnum1 in sub do
        case itemEnum1.Key of
          'dimensions': begin
            arr := TJSONArray(itemEnum1.Value);
            vi := arr.Items[0].AsInteger;
            if vi <> Physical.Width then
              Modes[i].AddPhysicalOverridenProperty('Width', vi.ToString);
            vi := arr.Items[1].AsInteger;
            if vi <> Physical.Height then
              Modes[i].AddPhysicalOverridenProperty('Height', vi.ToString);
            vi := arr.Items[2].AsInteger;
            if vi <> Physical.Depth then
              Modes[i].AddPhysicalOverridenProperty('Depth', vi.ToString);
          end;
          'weight': begin
            vf := itemEnum1.Value.AsFloat;
            if vf <> StringToSingle(Physical.Weight) then
              Modes[i].AddPhysicalOverridenProperty('Weight', formatFloatWithDot('0.00', vf));
          end;
          'power': begin
            vi := itemEnum1.Value.AsInteger;
            if vi <> Physical.Power then
              Modes[i].AddPhysicalOverridenProperty('Power', vi.ToString);
          end;
          'bulb': begin
            for itemEnum2 in itemEnum1.Value do begin
              case itemEnum2.Key of
                'type': begin
                  vs := itemEnum2.Value.AsString;
                  if vs <> Physical.Bulb then
                    Modes[i].AddPhysicalOverridenProperty('Bulb', vs);
                end;
                'lumens': begin
                  vi := itemEnum2.Value.AsInteger;
                  if vi <> Physical.Lumens then
                    Modes[i].AddPhysicalOverridenProperty('Lumens', vi.ToString);
                end;
              end;
            end;
          end;
          'lens':begin
            sub1 := sub.FindPath('name');
            if sub1 <> NIL then begin
              vs := sub1.AsString;
              if vs <> Physical.Lens then
                Modes[i].AddPhysicalOverridenProperty('Lens', vs);
            end;
            sub1 := sub.FindPath('degreesMinMax');
            if sub1 <> NIL then begin
              arr := TJSONArray(sub1);
              vf := arr.Items[0].AsFloat;
              if vf <> Physical.LensMinDegree then
                Modes[i].AddPhysicalOverridenProperty('LensMinDegree',
                       FormatFloatWithDot('0.0', vf));
              vf := arr.Items[1].AsFloat;
              if vf <> Physical.LensMaxDegree then
                Modes[i].AddPhysicalOverridenProperty('LensMaxDegree',
                       FormatFloatWithDot('0.0', vf));
            end;
          end;
        end;
    end;

//LogMessage('channels');
    sub := itemEnum.Value.FindPath('channels');
    if sub = NIL then begin
      LogMessage('  MODES - "channels" not found "'+TJSONData(aArr).AsString+'"');
      exit;
    end;
    arr := TJSONArray(sub);
    SetLength(Modes[i].ChannelsIDToUse, arr.Count);

    for j:=0 to arr.Count-1 do
    begin
      if arr.Items[j].IsNull then begin// entry in modes can be null...
        AddNotUsedChannelIfNeeded;
        s := 'NotUsed'
      end else
        s := arr.Items[j].AsString;
      Modes[i].ChannelsIDToUse[j] := s;
    end;
    inc(i);
  end;
  Result := True;
end;

function TFixtureFromLibrary.IndexOfAvailableChannelName(const aName: string): integer;
var
  i: Integer;
begin
  for i:=0 to High(AvailableChannels) do
    if AvailableChannels[i].ID = aName then begin
      Result := i;
      exit;
    end;
  Result := -1;
end;

function TFixtureFromLibrary.IndexOfWheel(const aWheelID: string): integer;
var
  i: Integer;
begin
  for i:=0 to High(Wheels) do
    if Wheels[i].SlotID = aWheelID then begin
      Result := i;
      exit;
    end;
  Result := -1;
end;

procedure TFixtureFromLibrary.SaveTo(t: TStringList);
var i: Integer;
  prop: TProperties;
begin
  t.Add('[SAYNETE]');
  t.Add('Version|v3.1.0');

  if Length(Web) > 0 then begin
    prop.Init('|');
    for i:=0 to High(Web) do begin
      prop.Add('LinkType'+(i+1).ToString, Web[i].LinkType);
      prop.Add('Url'+(i+1).ToString, Web[i].Url);
    end;
    t.Add('[LINKS]');
    t.Add(prop.PackedProperty);
  end;

  General.SaveTo(t);
  Physical.SaveTo(t);
  Matrix.SaveTo(t);

  t.Add('[CHAN_DEF]');
  for i:=0 to High(AvailableChannels) do
    t.Add(AvailableChannels[i].SaveToString);
  t.Add('[END_CHAN_DEF]');

  t.Add('[MODES]');
  for i:=0 to High(Modes) do
    t.Add(Modes[i].SaveToString);
  t.Add('[END_MODES]');
end;

function TFixtureFromLibrary.SaveTo(const aFilename: string): boolean;
var
  t: TStringList;
begin
  Result := False;

  t := TStringList.Create;
  try
    SaveTo(t);
    try
      t.SaveToFile(aFilename);
      Result := True;
    except
    end;
  finally
    t.Free;
  end;
end;


{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  i, c, tot: Integer;
  fSrc, fDest, current: string;
begin
  // empty the dest folder
  VideLeRepertoire(ConvertedPath);

  if CBConvertOnlyExisting.Checked then begin
    // delete all existing fixture definition that exists in Saynète DMXLibrary
    for i:=LB.Count-1 downto 0 do begin
      current := LB.Items.Strings[i];
      if ExtractFileExt(current) = '.json' then begin
        fDest := ConcatPaths([SayneteDMXLibraryPath, ChangeFileExt(current, '.dmx')]);
        if FileExists(fDest) then LB.Items.Delete(i);
      end;
    end;
  end;

  // If they don't exists, we create all sub-folders in destination
  // and delete them from the list
  for i:=LB.Count-1 downto 0 do
  begin
    fSrc := ConcatPaths([OFLPath, LB.Items.Strings[i]]);
    if IsFolder(fSrc) then
    begin
      if not RepertoireExistant(ConcatPaths([ConvertedPath, LB.Items.Strings[i]])) then
        CreerRepertoire(ConcatPaths([ConvertedPath, LB.Items.Strings[i]]));
      LB.Items.Delete(i);
    end;
  end;

  tot := LB.Count;
  Label2.Caption := 'Found: '+tot.ToString+' fixtures';
  PB.Max := tot;
  Application.ProcessMessages;
  Memo1.Visible := False;

  FChannelNotTyped := TStringList.Create;

  // Try to convert each JSON file
  FWheelsErrorCount := 0;
  FMatrixErrorCount := 0;
  FSwitchErrorCount := 0;
  c := 0;
  for i:=LB.Count-1 downto 0 do
  begin
    PB.Position := PB.Max-(i+1);
    Application.ProcessMessages;

    fSrc := ConcatPaths([OFLPath, LB.Items.Strings[i]]);
    fDest := ChangeFileExt(ConcatPaths([ConvertedPath, LB.Items.Strings[i]]), '.dmx');

    if Convert(fSrc, fDest) then inc(c)
      else LogMessage('FAIL: '+fSrc);
    LogMessage(' ');
  end;

  Memo1.Visible := True;
  Label3.Caption := 'Done: '+c.ToString;
  Label4.Caption := 'Remains: '+(tot-c).ToString;
  Label5.Caption := 'SwitchChannel error: '+FSwitchErrorCount.ToString;
  Label6.Caption := 'Wheels error: '+FWheelsErrorCount.ToString;
  Label7.Caption := 'Matrix error: '+FMatrixErrorCount.ToString;


  // delete empty folder in Converted
  if CBConvertOnlyExisting.Checked then begin
    DeleteEmptyFolderInConvertedFolder;
  end;

  // convert the manufacturer list
  ConvertManufacturers;

  // save the non converted channel types
  FChannelNotTyped.SaveToFile(Application.Location+'not_recognized_channel_type.txt');
  FChannelNotTyped.Free;
end;

procedure TForm1.Button2Click(Sender: TObject);
var t: TStringList;
  libFix: TLibraryFixture;
  i: Integer;
  f: string;
begin
  t := ContenuDuRepertoire(SayneteDMXLibraryPath, '.dmx', True, True);
  Log := TLog.Create(Application.Location+'library.log');
  for i:=0 to t.Count-1 do begin
    if ExtractFileExt(t.Strings[i]) <> '.dmx' then continue;

    f := ConcatPaths([SayneteDMXLibraryPath, t.Strings[i]]);
    libFix.InitDefault;
    if not libFix.LoadFromFile(f) then
      raise exception.Create('impossible de charger '+t.Strings[i]);
    libFix.General.Creator := 'OFL';
    if not libFix.SaveToFile(ConcatPaths([SayneteDMXLibraryPath, t.Strings[i]])) then
      raise exception.create('impossible de sauver '+t.Strings[i]);
  end;
  t.Free;
  Log.Free;
  ShowMessage('Done');
end;

procedure TForm1.FormShow(Sender: TObject);
var F: TStringList;
  i: integer;
begin
  F := ContenuDuRepertoire(OFLPath, '.json', True, True);
  // delete fixture with 16 bits range values
  DeleteLBEntriesWith16BitRangeValues(F);
  // delete manufacturers.json file from the list
  for i:=0 to F.Count-1 do
    if F.Strings[i] = 'manufacturers.json' then begin
      F.Delete(i);
      break;
    end;

  LB.Items.Assign(F);
  F.Free;
end;

procedure TForm1.DeleteEmptyFolderInConvertedFolder;
var t: TStringList;
  i: integer;
  f: string;
begin
  t := GetDirectoryContent(ConvertedPath, ['.ImpossibleExtension'], True, 0);
  for i:=0 to t.Count-1 do begin
    f := ConcatPaths([ConvertedPath, t.Strings[i]]);
    if IsFolder(f) and RepertoireEstVide(f) then
      SupprimeRepertoire(f);
  end;
  t.Free;
end;

procedure TForm1.DeleteLBEntriesWith16BitRangeValues(aList: TStringList);
var i: integer;
  f: string;
begin
  for i:=aList.Count-1 downto 0 do begin
    f := aList.Strings[i];
    if (Pos('arri', f) = 1) or
       (Pos('dmg-lumiere', f) = 1) or
       (Pos('fiilex', f) = 1) or
       (Pos('generic\', f) = 1) or
       (Pos('lupo\', f) = 1)
      then aList.Delete(i);
  end;
end;

procedure TForm1.ConvertManufacturers;
var stream: TFileStream;
  FParser: TJSONParser;
  m, s: string;
  wholeData, sub: TJSONData;
  i: integer;
  prop: TProperties;
  t: TStringList;
begin
  // Open a stream and parse the manufacturer file
  m := ConcatPaths([OFLPath, 'manufacturers.json']);
  stream := TFileStream.Create(m, fmOpenRead);
  t := TStringList.Create;
  try
    FParser := TJSONParser.Create(stream, [joUTF8]);
    wholeData := FParser.Parse;

    for i:=1 to wholeData.Count-1 do begin
      prop.Init('|');

      s := TJSONObject(wholeData).Names[i];
      prop.Add('Folder', s);

      sub := wholeData.Items[i].FindPath('name');
      prop.Add('Name', sub.AsString);

      sub := wholeData.Items[i].FindPath('website');
      if sub <> NIL then prop.Add('Web', sub.AsString);

      sub := wholeData.Items[i].FindPath('rdmId');
      if sub <> NIL then prop.Add('RDM', sub.AsString);

      t.Add(prop.PackedProperty);
    end;

    try
      m := ConcatPaths([ConvertedPath,'manufacturers.txt']);
      t.SaveToFile(m);
      ShowMessage('Manufacturer list created !');
    except
      ShowMessage('Enable to save the manufacturer file'+LineEnding+m);
    end;

  finally
    FParser.Free;
    wholeData.Free;
    stream.Free;
    t.Free;
  end;
end;

function TForm1.OFLPath: string;
begin
  Result := ConcatPaths([Application.Location, 'OFL']);
end;

function TForm1.ConvertedPath: string;
begin
  Result := ConcatPaths([Application.Location, 'ConvertedFixtures']);
end;

function TForm1.SayneteDMXLibraryPath: string;
begin
  Result := Application.Location+'..'+PathDelim+'Binary'+PathDelim+'Data'+PathDelim+'DMXLibrary';
  Result := ExpandFileName(Result);
end;

function TForm1.ManufacturerNameOf(const f: string): string;
var  Fstr: TFileStream;
  FParser: TJSONParser;
  m: String;
  wholeData, item: TJSONData;
begin
  Result := '';

  // Open a stream and parse the source file
  m := ConcatPaths([OFLPath, 'manufacturers.json']);
  Fstr := TFileStream.Create(m, fmOpenRead);
  try
    FParser := TJSONParser.Create(Fstr, [joUTF8]);
    wholeData := FParser.Parse;

    m := ExcludeTrailingPathDelimiter(NomDuDernierSousRepertoire(f));
    item := wholeData.FindPath(m);
    if item <> NIL then
      Result := item.FindPath('name').AsString;
  finally
    FParser.Free;
    wholeData.Free;
    Fstr.Free;
  end;
end;

function TForm1.Convert(const SrcFilename, DstFilename: string): boolean;
var
  fix: TFixtureFromLibrary;
  Fstr: TFileStream;
  FParser: TJSONParser;
  wholeData,
  sub: TJSONData;
  arr: TJSONArray;
  itemEnum, itemEnum2: TJSONEnum;
  j: integer;
  s, OFLFixtureType: string;
begin
  LogMessage('-- Converting '+SrcFilename);
  Result := True;

  fix.InitDefault;
  fix.General.Creator := 'OFL';

  s := ManufacturerNameOf(SrcFilename);
  if s = '' then
  begin
    LogMessage('Name of manufacturer for file "'+SrcFilename+'" not found in file "manufacturers.json"');
    Result := False;
    exit;
  end;
  fix.General.Manufacturer := s;

  try
    // Open a stream and parse the source file
    Fstr := TFileStream.Create(SrcFilename, fmOpenRead);
    try
      FParser := TJSONParser.Create(Fstr, [joUTF8]);
      wholeData := FParser.Parse;

      // Parse each entries
      for itemEnum in wholeData do
      begin
        case itemEnum.Key of
          '$schema': ;
          'shortName':;
          'comment':;
          'helpWanted':;
          'rdm':; // ignored
          'fixtureKey':;
          'manufacturerKey':;
          'oflURL':;
          'links': begin
            for itemEnum2 in itemEnum.Value do begin
              case itemEnum2.Key of
                'manual': begin
                  arr := TJSONArray(itemEnum2.Value);
                  for j:=0 to arr.Count-1 do begin
                    SetLength(fix.Web, Length(fix.Web)+1);
                    fix.Web[High(fix.Web)].LinkType := 'manual';
                    fix.Web[High(fix.Web)].Url := arr.Items[j].AsString;
                  end;
                end;
                'productPage': begin
                  arr := TJSONArray(itemEnum2.Value);
                  for j:=0 to arr.Count-1 do begin
                    SetLength(fix.Web, Length(fix.Web)+1);
                    fix.Web[High(fix.Web)].LinkType := 'product page';
                    fix.Web[High(fix.Web)].Url := arr.Items[j].AsString;
                  end;
                end;
                'video': begin
                  arr := TJSONArray(itemEnum2.Value);
                  for j:=0 to arr.Count-1 do begin
                    SetLength(fix.Web, Length(fix.Web)+1);
                    fix.Web[High(fix.Web)].LinkType := 'video';
                    fix.Web[High(fix.Web)].Url := arr.Items[j].AsString;
                  end;
                end;
                'other': begin
                  arr := TJSONArray(itemEnum2.Value);
                  for j:=0 to arr.Count-1 do begin
                    SetLength(fix.Web, Length(fix.Web)+1);
                    fix.Web[High(fix.Web)].LinkType := 'other';
                    fix.Web[High(fix.Web)].Url := arr.Items[j].AsString;
                  end;
                end;
                else raise exception.create('forgot to implement !');
              end;//case
            end;
          end;
          'name': fix.General.Name := itemEnum.Value.AsString;
          'categories': begin
            arr := TJSONArray(itemEnum.Value);
            //fix.General.FixtureType := arr.Items[0].AsString;
            OFLFixtureType := arr.Items[0].AsString;
          end;
          'meta': begin
            sub := itemEnum.Value.FindPath('authors');
            if sub = NIL then
            begin
              LogMessage(SrcFilename+' - "authors" not found');
              Result := False;
            end;
            arr := TJSONArray(sub);
            for j:=0 to arr.Count-1 do
              if j = 0 then fix.General.Authors := arr.Items[j].AsString
                else fix.General.Authors := fix.General.Authors+', '+arr.Items[j].AsString;
          end;
          'physical': begin
             Result := fix.Physical.InitFrom(itemEnum.Value);
             if not Result then
               LogMessage(SrcFilename+' - "physical" not found');
          end;
          'matrix': begin
            for itemEnum2 in itemEnum.Value do begin
              case itemEnum2.Key of
                'pixelCount': begin
                  arr := TJSONArray(itemEnum2.Value);
                  fix.Matrix.X := arr.Items[0].AsInteger;
                  fix.Matrix.Y := arr.Items[1].AsInteger;
                end;
                'pixelKeys': begin
                  arr := TJSONArray(itemEnum2.Value);
                  fix.Matrix.X := TJSONArray(arr.Items[0]).Count;
                  fix.Matrix.Y := arr.Count;
                end;
                'pixelGroups': begin
                  LogMessage('  property "matrix/pixelGroups" found but not implemented!');
                  Result := False;
                end;
              end;//case
            end;

            LogMessage('  property "'+itemEnum.Key+'":"'{+item.AsJSON}+'" found but not implemented!');
            Result := False;
            inc(FMatrixErrorCount);
          end;
          'wheels': begin
            Result := fix.InitWheelsFrom(itemEnum.Value);
            if not Result then begin
              LogMessage('  property "'+itemEnum.Key+'":"'+itemEnum.Value.AsJSON+'" fail ti InitWheelsFrom');
              inc(FWheelsErrorCount);
            end;
          end;
          'availableChannels': begin
            Result := fix.InitAvailableChannelsfrom(itemEnum.Value);
          end;
          'modes': begin
            arr := TJSONArray(itemEnum.Value);
            Result := fix.InitModesFrom(arr);
          end;
          else begin
            LogMessage(SrcFilename);
            LogMessage('  property "'+itemEnum.Key+'" found but not implemented!');
            Result := False;
          end;
        end;//case
        if not Result then break;
        end;

        // we try to set the right fixture type for Saynètes
        case OFLFixtureType of
          'Barrel Scanner', 'Scanner': fix.General.FixtureType := ftScanner;
          'Moving Head': fix.General.FixtureType := ftMovingHead;
          'Fan': fix.General.FixtureType := ftFan;
          'Hazer', 'Smoke': fix.General.FixtureType := ftSmokeMachine;
          'Laser': fix.General.FixtureType := ftLaser;
          'Matrix': begin
            if fix.HaveColorChannel then fix.General.FixtureType := ftMatrixWithColoredLed
              else fix.General.FixtureType := ftMatrixTransparentLed;
          end;
          'Pixel Bar':begin
            if fix.HaveColorChannel then fix.General.FixtureType := ftBarColoredLed
              else fix.General.FixtureType := ftLedBarTransparentLed;
          end;
          'Other': fix.General.FixtureType := ftOther;
          'Blinder':;
          'Color Changer':;// fix.General.FixtureType := ftParLongTransparentLed;
          'Dimmer':;
          'Effect':;
          'Flower':;
          'Stand':;
          'Strobe':;
        end;
    finally
      FStr.Free;
      wholeData.Free;
      FParser.Free;
    end;
  except
    on E: Exception do begin
     LogMessage('an exception occurs: '+E.Message);
     Result := False;
    end;
  end;

  if not Result then exit;

  // save the converted data
  if not fix.SaveTo(DstFilename) then
  begin
    LogMessage('Error while saving converted fixture to file:');
    LogMessage('  '+DstFilename);
    exit;
  end;

  Result := True;
end;


end.

