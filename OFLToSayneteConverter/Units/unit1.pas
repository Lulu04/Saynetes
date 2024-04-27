unit Unit1;

{$mode objfpc}{$H+}
{$modeSwitch advancedRecords}
{$modeSwitch typehelpers}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  fpjson, jsonparser,
  u_common;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    LB: TListBox;
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function OriginalPath: string;
    function DestPath: string;
    function ManufacturerNameOf(const f: string): string;
    function Convert(const SrcFilename, DstFilename: string): boolean;
  public

  end;

var
  Form1: TForm1;

implementation
uses utilitaire_fichier, PropertyUtils;

{$R *.lfm}

var
  FWheelsErrorCount,
  FMatrixErrorCount: integer;



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
    FixType,
    Authors,
    Creator: string;
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

  { TSlotDescriptor }

  TSlotDescriptor = record
    Txt,
    Extra: string;
    Symbol: word;
    procedure InitDefault;
  end;

  { TWheel }

  TWheel = record
    SlotID: string;
    Slots: array of TSlotDescriptor;
    procedure InitDefault;
  end;

  { TSwitchDescriptor }

  TSwitchDescriptor = record
    SwitchID,
    TargetID: string;
    function SaveToString: string;
  end;

  { TSingleRange }

  TSingleRange = record
    BeginValue,
    EndValue: integer;
    Symbol: word;
    Txt,
    Extra: string;
    Switchs: array of TSwitchDescriptor;

    procedure InitDefault;
    procedure AddSwitch(const aSwitchID, aTargetID: string);
    function SaveToString: string;
  end;
  TRanges = array of TSingleRange;


  { TAvailableChannel }

  TAvailableChannel = record
    ID: string;
    BitWidthForNumber: integer; // 0 means total bit width of coarse plus fine(s) channel(s)
    DefaultValue: integer;
    ChanType: TChannelType;
    Ranges: TRanges;

    FineChannelAliases: TStringArray; // contains the ID of the fine(s) channel aliases

    procedure InitDefault;
    procedure SetSingleRange(aSymbol: word; const aText: string);
    function HaveFineChannels: boolean;
    function SaveToString: string;
  end;
  TAvailableChannels = array of TAvailableChannel;

  { TVirtualSwitchingChannel }

  TVirtualSwitchingChannel = record
    VirtualName: string;
    PossibleTargetChannels: TStringArray;
    function SaveToString: string; // used to save "modes"
  end;
  TVirtualSwitchingChannels =  array of TVirtualSwitchingChannel;

  { HVirtualSwitchingChannels }

  HVirtualSwitchingChannels = type helper for TVirtualSwitchingChannels
    procedure Add(const aVirtualName, aPossibleTargetChannelName: string);
    function IsVirtualSwitchingChannel(const aChannelName: string): boolean;
    function SaveToString(const aVirtualChannelName: string): string;
  end;

var
    FVirtualSwitchingChannels: TVirtualSwitchingChannels;

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
    // because OFL modes can have null entry to specify a not used channel...
    procedure AddNotUsedChannelIfNeeded;
  public
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
                 out symbol: word; out extra: string): boolean;
    function TranslateSlotType(aItem: TJSONData;
                 const aOFLType: string;
                 out chanType: TChannelType; out txt: string;
                 out symbol: word; out extra: string): boolean;
    function AddRangeFrom(aChanIndex: integer; aItem: TJSONData): boolean;

    function InitWheelsFrom(aItem: TJSONData): boolean;

    function InitAvailableChannelsfrom(aItem: TJSONData): boolean;
    function InitModesFrom(aArr: TJSONArray): boolean;
    function IndexOfAvailableChannelName(const aName: string): integer;
    function IndexOfWheel(const aWheelID: string): integer;

    procedure SaveTo(t: TStringList);
    function SaveTo(const aFilename: string): boolean;
  end;

{ TVirtualSwitchingChannel }

function TVirtualSwitchingChannel.SaveToString: string;
var s: String;
begin
  Result := VirtualName;
  for s in PossibleTargetChannels do
    Result := Result+'#'+s;
end;

{ HVirtualSwitchingChannels }

procedure HVirtualSwitchingChannels.Add(const aVirtualName, aPossibleTargetChannelName: string);
var i, j: integer;
begin
  // aVirtualName is already in the array ?
  for i:=0 to High(Self) do
    with Self[i] do
      if VirtualName = aVirtualName then begin
        // do nothing if aPossibleTargetChannelName is already present
        for j:=0 to High(PossibleTargetChannels) do
          if PossibleTargetChannels[j] = aPossibleTargetChannelName then exit;
        // add the target channel
        SetLength(PossibleTargetChannels, Length(PossibleTargetChannels)+1);
        PossibleTargetChannels[High(PossibleTargetChannels)] := aPossibleTargetChannelName;
        exit;
      end;

  // Add an entry
  SetLength(Self, Length(Self)+1);
  with Self[High(Self)] do begin
    VirtualName := aVirtualName;
    SetLength(PossibleTargetChannels, 1);
    PossibleTargetChannels[0] := aPossibleTargetChannelName;
  end;

end;

function HVirtualSwitchingChannels.IsVirtualSwitchingChannel(const aChannelName: string): boolean;
var v: TVirtualSwitchingChannel;
begin
  Result := False;
  for v in Self do
    Result := Result or (v.VirtualName = aChannelName);
end;

function HVirtualSwitchingChannels.SaveToString(const aVirtualChannelName: string): string;
var v: TVirtualSwitchingChannel;
begin
  for v in Self do
    if v.VirtualName = aVirtualChannelName then begin
      Result := v.SaveToString;
      exit;
    end;
  Raise Exception.Create(aVirtualChannelName+' is not a virtual switching channel');
end;

{ TSwitchDescriptor }

function TSwitchDescriptor.SaveToString: string;
begin
  Result := SwitchID+':'+TargetID;
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
var prop: TPackProperty;
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
  Symbol := 0;
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
  Symbol := 0;
  txt := '';
  Switchs := NIL;
end;

procedure TSingleRange.AddSwitch(const aSwitchID, aTargetID: string);
begin
  SetLength(Switchs, Length(Switchs)+1);
  with Switchs[High(Switchs)] do begin
    SwitchID := aSwitchID;
    TargetID := aTargetID;
  end;
end;

function TSingleRange.SaveToString: string;
var prop: TPackProperty;
  i: integer;
  s: string;
begin
  prop.Init('~');
  prop.Add('Begin', BeginValue);
  prop.Add('End', EndValue);
  prop.Add('Symbol', Symbol);
  prop.Add('Txt', txt);
  if Extra <> '' then prop.Add('Extra', Extra);
  if Length(Switchs) > 0 then begin
    s := '';
    for i:=0 to High(Switchs) do begin
      s := s+Switchs[i].SaveToString;
      if i < High(Switchs) then s := s+';';
    end;
    prop.Add('Switch', s);
  end;

  Result := prop.PackedProperty;
end;

{ TAvailableChannel }

procedure TAvailableChannel.InitDefault;
begin
  ID := '';
  BitWidthForNumber := 0;
  DefaultValue := 0;
  Ranges := NIL;
  FineChannelAliases := NIL;
end;

procedure TAvailableChannel.SetSingleRange(aSymbol: word; const aText: string);
begin
  SetLength(Ranges, 1);
  Ranges[0].Symbol := aSymbol;
  Ranges[0].txt := aText;
  Ranges[0].BeginValue := 0;
  Ranges[0].EndValue := 255;
end;

function TAvailableChannel.HaveFineChannels: boolean;
begin
  Result := Length(FineChannelAliases) > 0;
end;

function TAvailableChannel.SaveToString: string;
var prop: TPackProperty;
  i: integer;
begin
  prop.Init('|');
  prop.Add('ID', ID);
  prop.Add('Type', Ord(ChanType));
  if DefaultValue <> 0 then prop.Add('DefaultValue', DefaultValue);

  for i:=0 to High(Ranges) do
    prop.Add('R'+(i+1).ToString, Ranges[i].SaveToString);

  Result := prop.PackedProperty;
end;

{ TGeneral }

procedure TGeneral.InitDefault;
begin
  Manufacturer := '';
  Name := '';
  FixType := '';
  Authors := '';
  Creator := '';
end;

procedure TGeneral.SaveTo(t: TStringList);
var prop: TPackProperty;
begin
  prop.Init('|');
  prop.Add('Manufacturer', Manufacturer);
  prop.Add('Name', Name);
  prop.Add('Type', FixType);
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
var prop: TPackProperty;
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
  General.InitDefault;
  Physical.InitDefault;
  AvailableChannels := NIL;
  Wheels := NIL;
  FVirtualSwitchingChannels := NIL;
  Matrix.InitDefault;
end;

function TFixtureFromLibrary.ProcessItemCapability(aItem: TJSONData; aChanIndex: integer): boolean;
var sub: TJSONData;
  i: integer;
  chanType: TChannelType;
  symbol: word;
  txt, extra, s: string;
begin
  Result := False;
  for i:=0 to aItem.Count-1 do
  begin
    sub := aItem.Items[i];
    s := TJSONObject(aItem).Names[i];
    case s of
      'type': begin
        Result := TranslateCapabilityType(aItem, sub.AsString, chanType, txt, symbol, extra);
        if not Result then exit;
        AvailableChannels[aChanIndex].ChanType := chanType;
        AvailableChannels[aChanIndex].SetSingleRange(0, txt);
      end;
    end;
  end;
end;

function TFixtureFromLibrary.TranslateCapabilityType(aItem: TJSONData; const aOFLType: string;
  out chanType: TChannelType; out txt: string; out symbol: word; out extra: string): boolean;
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
  begin
    if HasSteppedProperty('slotNumber')  then begin
      if not TryStrToInt(prop, iSlot) then begin
iSlot:=Trunc(sub.AsFloat)-1;
       // iSlot := Trunc(StringToSingle(prop));
        ConcatToText(Wheels[wheelIndex].Slots[iSlot].Txt);
        if iSlot+1 <= High(Wheels[wheelIndex].Slots) then
          txt := txt+'..'+Wheels[wheelIndex].Slots[iSlot+1].Txt;
        extra := Wheels[wheelIndex].Slots[iSlot].Extra;
        symbol := Wheels[wheelIndex].Slots[iSlot].Symbol;
      end
      else begin
        dec(iSlot);
        ConcatToText(Wheels[wheelIndex].Slots[iSlot].Txt);
        extra := Wheels[wheelIndex].Slots[iSlot].Extra;
        symbol := Wheels[wheelIndex].Slots[iSlot].Symbol;
      end;
    end
    else if HasRangedProperty_OnWheel('slotNumber') then ConcatToText(prop);
  end;

begin
  Result := True;
  txt := '';
  extra := '';
  symbol := 0;
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
      chanType := ctSpeed;
      if HasRangedProperty('speed') then ConcatToText(prop)
      else txt := 'Strobe speed';
    end;
    'StrobeDuration': begin
      chanType := ctSpeed;
      if HasRangedProperty('duration') then ConcatToText(prop)
      else txt := 'Strobe duration';
    end;
    'Intensity': begin
      chanType := ctDimmer;
      if HasRangedProperty('brightness') then ConcatToText(prop)
      else txt := 'Intensity';
    end;
    'ColorIntensity': begin
      chanType := ctDimmer; // default
      if HasSteppedProperty('color') then begin
        txt := prop;
        case prop of
          'Red': chanType := ctRED;
          'Green': chanType := ctGREEN;
          'Blue': chanType := ctBLUE;
          'Cyan': chanType := ctCYAN;
          'Magenta': chanType := ctMAGENTA;
          'Yellow': chanType := ctYELLOW;
          'Amber': chanType := ctAMBER;
          'White': chanType := ctWHITE;
          'Warm White': chanType := ctWARMWHITE;
          'Cold White': chanType := ctCOLDWHITE;
          'UV': chanType := ctUV;
          'Lime': chanType := ctLIME;
          'Indigo': chanType := ctINDIGO;
          end;
      end;
      if HasRangedProperty('brightness') then ConcatToText(prop);
      if txt = '' then txt := 'Color intensity';
    end;
    'ColorPreset': begin
      chanType := ctConfig;
      if HasRangedProperty('color') then txt := 'Color';
      if HasArrayProperty('colors') then
        extra := 'Colors_'+prop;
      if HasRangedProperty('colorTemperature') then ConcatToText('Color temperature '+prop);
      if txt = '' then txt := 'Color Preset';
    end;
    'ColorTemperature': begin
      chanType := ctConfig;
      txt := 'Color Temperature';
      if HasRangedProperty('colorTemperature') then ConcatToText(prop);
    end;
    'Pan': begin
      chanType := ctPan;
      if HasRangedProperty('angle') then ConcatToText(prop);
      if txt = '' then txt := 'Angle';
    end;
    'PanContinuous': begin
      chanType := ctPan;
      txt := 'Continuous';
      if HasRangedProperty('speed') then ConcatToText(prop);
    end;
    'Tilt': begin
      chanType := ctTilt;
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
      chanType := ctGOBOROTATION;   // TO DO
      CheckPropertyWheel;
      if HasSteppedProperty('slotNumber')  then begin
        ConcatToText(Wheels[wheelIndex].Slots[prop.ToInteger-1].Txt);
        extra := Wheels[wheelIndex].Slots[prop.ToInteger-1].Extra;
        symbol := Wheels[wheelIndex].Slots[prop.ToInteger-1].Symbol;
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
      chanType := ctCONFIG;
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
      chanType := ctCONFIG;
      txt := 'Focus';
      if HasRangedProperty('distance') then ConcatToText(prop);
    end;
    'Zoom': begin
      chanType := ctCONFIG;
      txt := 'Zoom';
      if HasRangedProperty('angle') then ConcatToText(prop);
    end;
    'Iris': begin
      chanType := ctCONFIG;
      txt := 'Iris';
      if HasRangedProperty('openPercent') then ConcatToText('open '+prop);
    end;
    'IrisEffect': begin
      chanType := ctCONFIG;
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
      chanType := ctCONFIG;
      txt := 'Prism rotation';
      if HasRangedProperty('speed') then ConcatToText(prop);
      if HasRangedProperty('angle') then ConcatToText(prop);
    end;
    'BladeInsertion': begin
      chanType := ctCONFIG;
      txt := 'Blade insertion';
      if HasSteppedProperty('blade') then ConcatToText(prop);
      if HasRangedProperty('insertion') then ConcatToText(prop);
    end;
    'BladeRotation': begin
      chanType := ctCONFIG;
      txt := 'Blade rotation';
      if HasSteppedProperty('blade') then ConcatToText(prop);
      if HasRangedProperty('angle') then ConcatToText(prop);
    end;
    'BladeSystemRotation': begin
      chanType := ctCONFIG;
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
      chanType := ctCONFIG;
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
  const aOFLType: string; out chanType: TChannelType; out txt: string; out
  symbol: word; out extra: string): boolean;
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
  symbol := 0;
  case aOFLType of
    'Open': begin
      chanType := ctCONFIG;
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
      if HasSteppedProperty('resource') then extra := prop;
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
  symbol: word;
  enum1: TJSONEnum;
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
        if TranslateCapabilityType(aItem, sub.AsString, chanType, txt, symbol, extra) then
        begin
          AvailableChannels[aChanIndex].Ranges[iRange].Symbol := symbol;
          AvailableChannels[aChanIndex].Ranges[iRange].txt := txt;
          AvailableChannels[aChanIndex].Ranges[iRange].Extra := extra;
        end
        else begin
          LogMessage('Capabilities/type: Fail to retrieve information "'+sub.AsString+'"');
          exit;
        end;
      end;

      'switchChannels': begin
        for enum1 in sub do begin
          FVirtualSwitchingChannels.Add(enum1.Key, enum1.Value.AsString);

          AvailableChannels[aChanIndex].Ranges[iRange].AddSwitch(enum1.Key, enum1.Value.AsString);
        end;

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
  chanType: TChannelType;
  symbol: word;
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
            if not TranslateSlotType(sub1, s1, chanType, txt, symbol,extra) then
              exit;
            SetLength(Wheels[i].Slots, Length(Wheels[i].Slots)+1);
            iSlot := High(Wheels[i].Slots);
            Wheels[i].Slots[iSlot].InitDefault;
            Wheels[i].Slots[iSlot].Txt := txt;
            Wheels[i].Slots[iSlot].Extra := extra;
            Wheels[i].Slots[iSlot].Symbol := symbol;
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
end;

function TFixtureFromLibrary.InitAvailableChannelsfrom(aItem: TJSONData): boolean;
var item, sub, subCap: TJSONData;
  arr: TJSONArray;
  i, j, k, iChan: integer;
  s, s1: string;
  defv: integer;
  v: double;
begin
  Result := True;

  AvailableChannels := NIL;

  iChan := 0;
  for i:=0 to aItem.Count-1 do  // parse all available channels
  begin
    SetLength(AvailableChannels, Length(AvailableChannels)+1);
    AvailableChannels[iChan].InitDefault;
    item := aItem.Items[i];

    s := TJSONObject(aItem).Names[i];
    AvailableChannels[iChan].ID := s;

    // check if the name of the channel is the same as a wheel
    FSourceWheelIndex := IndexOfWheel(AvailableChannels[iChan].ID);

    for j:=0 to item.Count-1 do   // parse one channel
    begin
      sub := item.Items[j];
      s := TJSONObject(item).Names[j];
      case s of
        'defaultValue': begin
          case sub.JSONType of
            jtNumber: AvailableChannels[iChan].DefaultValue := sub.AsInteger;
            jtString: begin
              s1 := sub.AsString;
              if s1.Contains('%') then begin
                Delete(s1, Pos('%', s1), Length(s1)-Pos('%', s1)+1);
                case Length(AvailableChannels[iChan].FineChannelAliases) of
                  0: v := $FF;
                  1: v := $FFFF;
                  2: v := $FFFFFF;
                  3: v := $FFFFFFFF;
                end;
                AvailableChannels[iChan].DefaultValue := Round(StringToSingle(s1)/100*v);
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
          arr := TJSONArray(sub);

          SetLength(AvailableChannels[iChan].FineChannelAliases, arr.Count);
          for k:=0 to arr.Count-1 do
            AvailableChannels[iChan].FineChannelAliases[k] := arr.Items[k].AsString; // name of the fine channel alias
        end;

        'dmxValueResolution': begin
          case sub.AsString of
            '8bit': AvailableChannels[iChan].BitWidthForNumber := 8;
           else begin
             LogMessage('InitAvailableChannelsFrom - property "dmxValueResolution" value not supported "'+
                         sub.AsString+'"');
             Result := False;
           end;
          end;
        end;

        'capability': begin
          Result := ProcessItemCapability(sub, i);
        end;

        'capabilities': begin
          AvailableChannels[iChan].Ranges := NIL;
          arr := TJSONArray(sub);
          for k:=0 to arr.Count-1 do begin // parse each range
            subCap := arr.Items[k];
            if not AddRangeFrom(iChan, subCap) then begin
              LogMessage('InitAvailableChannelsFrom() - Fail to retrieve range info "'+subCap.AsJSON);
              Result := False;
            end;
            if not Result then break;
          end;
        end;
      end;
      if not Result then break;
    end;

    // if the last created channel have fine channel alias(es), then we put them
    // in the list as normal available channels, so that they can be visible for
    // 'modes'
    if Result and
       (Length(AvailableChannels[iChan].FineChannelAliases) > 0) then begin
       for j:=0 to High(AvailableChannels[iChan].FineChannelAliases) do
       begin
         SetLength(AvailableChannels, Length(AvailableChannels)+1);
         with AvailableChannels[High(AvailableChannels)] do begin
           InitDefault;
           ID := AvailableChannels[iChan].FineChannelAliases[j];
           ChanType := AvailableChannels[iChan].ChanType;
           for k:=0 to High(AvailableChannels[iChan].Ranges) do
           begin
             SetLength(Ranges, Length(Ranges)+1);
             Ranges[k].InitDefault;
             Ranges[k].BeginValue := AvailableChannels[iChan].Ranges[k].BeginValue;
             Ranges[k].EndValue := AvailableChannels[iChan].Ranges[k].EndValue;
             Ranges[k].Symbol := AvailableChannels[iChan].Ranges[k].Symbol;
             Ranges[k].Txt := AvailableChannels[iChan].Ranges[k].Txt;
             Ranges[k].Extra := AvailableChannels[iChan].Ranges[k].Extra;
           end;
         end;
       end;
       // distribute the default value on the coarse and fine(s) channels
       if (AvailableChannels[iChan].BitWidthForNumber = 0) and
          (AvailableChannels[iChan].DefaultValue <> 0) then begin
         defv := AvailableChannels[iChan].DefaultValue;
         for j:=High(AvailableChannels[iChan].FineChannelAliases) downto -1 do
         begin
           AvailableChannels[iChan+j+1].DefaultValue := byte(defv and $FF);
           defv := defv shr 8;
         end;
       end;
       j := Length(AvailableChannels[iChan].FineChannelAliases);
       inc(iChan, j);
    end;
    inc(iChan);


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
    SetSingleRange(0, 'No function');
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
      if arr.Items[j].IsNull then  begin// entry in modes can be null...
        AddNotUsedChannelIfNeeded;
        s := 'NotUsed'
      end else
        s := arr.Items[j].AsString;
      Modes[i].ChannelsIDToUse[j] := s;


      if not FVirtualSwitchingChannels.IsVirtualSwitchingChannel(s) and
         (IndexOfAvailableChannelName(s) = -1) and
         (s <> 'NotUsed') then begin
        LogMessage('  MODES - "modes" use channel "'+s+
             '" that is not registered in available or switching channels');
        exit;
      end;
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
begin
  t.Add('[SAYNETE]');
  t.Add('Version|v3.1.0');
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
  fSrc, fDest: string;
begin
  // empty the dest folder
  VideLeRepertoire(DestPath);

  // If they don't exists, we create all sub-folders in destination
  // and delete them from the list
  for i:=LB.Count-1 downto 0 do
  begin
    fSrc := ConcatPaths([OriginalPath, LB.Items.Strings[i]]);
    if IsFolder(fSrc) then
    begin
      if not RepertoireExistant(ConcatPaths([DestPath, LB.Items.Strings[i]])) then
        CreerRepertoire(ConcatPaths([DestPath, LB.Items.Strings[i]]));
      LB.Items.Delete(i);
    end;
  end;

  tot := LB.Count;
  Label2.Caption := 'Total: '+tot.ToString+' fixtures';
  Application.ProcessMessages;
//  Memo1.Visible := False;

  // Try to convert each JSON file
  FWheelsErrorCount := 0;
  FMatrixErrorCount := 0;
  c := 0;
  for i:=LB.Count-1 downto 0 do
  begin
    fSrc := ConcatPaths([OriginalPath, LB.Items.Strings[i]]);
    fDest := ChangeFileExt(ConcatPaths([DestPath, LB.Items.Strings[i]]), '.dmx');
//ShowMessage('Try to convert'+LineEnding+fSrc);
    if Convert(fSrc, fDest) then
    begin
      inc(c);
      //LogMessage('SUCCESS: '+fDest);
    end
    else LogMessage('FAIL: '+fSrc);
  end;
  Memo1.Visible := True;
  Label3.Caption := 'Done: '+c.ToString;
  Label4.Caption := 'Remains: '+(tot-c).ToString;
  Label6.Caption := 'Wheels error: '+FWheelsErrorCount.ToString;
  Label7.Caption := 'Matrix error: '+FMatrixErrorCount.ToString;
end;

procedure TForm1.FormShow(Sender: TObject);
var
  F: TStringList;
begin
  F := ContenuDuRepertoire(OriginalPath, '.json', True, True);
  LB.Items.Assign(F);
  F.Free;
end;

function TForm1.OriginalPath: string;
begin
  Result := ConcatPaths([Application.Location, 'OFL']);
end;

function TForm1.DestPath: string;
begin
  Result := ConcatPaths([Application.Location, 'ConvertedFixtures']);
end;

function TForm1.ManufacturerNameOf(const f: string): string;
var  Fstr: TFileStream;
  FParser: TJSONParser;
  m: String;
  wholeData, item: TJSONData;
begin
  Result := '';

  // Open a stream and parse the source file
  m := ConcatPaths([OriginalPath, 'manufacturers.json']);
  Fstr := TFileStream.Create(m, fmOpenRead);
  try
    FParser := TJSONParser.Create(Fstr);
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
  s: string;
begin
  LogMessage('Converting '+SrcFilename);
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
      FParser := TJSONParser.Create(Fstr);
      wholeData := FParser.Parse;

      // Parse each entries
      for itemEnum in wholeData do
      begin
        case itemEnum.Key of
          '$schema': ;
          'links':;
          'shortName':;
          'comment':;
          'helpWanted':;
          'rdm':; // ignored
          'name': fix.General.Name := itemEnum.Value.AsString;
          'categories': begin
            arr := TJSONArray(itemEnum.Value);
            fix.General.FixType := arr.Items[0].AsString;
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
              if j = 0 then
                fix.General.Authors := arr.Items[j].AsString
              else
                fix.General.Authors := fix.General.Authors+', '+arr.Items[j].AsString;
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
              end;
            end;

            LogMessage('  property "'+itemEnum.Key+'":"'{+item.AsJSON}+'" found but not implemented!');
            Result := False;
            inc(FMatrixErrorCount);
          end;
          'wheels': begin
            //Result := fix.InitWheelsFrom(item);
            LogMessage('  property "'+itemEnum.Key+'":"'{+item.AsJSON}+'" found but not implemented!');
            Result := False;
            inc(FWheelsErrorCount);
          end;
          'availableChannels': begin
            Result := fix.InitAvailableChannelsfrom(itemEnum.Value);
          end;
          'modes': begin
            arr := TJSONArray(itemEnum.Value);
            Result := fix.InitModesFrom(arr);
          end

          else begin
            LogMessage(SrcFilename);
            LogMessage('  property "'+itemEnum.Key+'" found but not implemented!');
            Result := False;
          end;
        end;//case
        if not Result then break;
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

