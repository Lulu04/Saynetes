unit u_list_dmxuniverse;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, Controls, Dialogs, Forms, Graphics,
  fgl,
  BGRABitmapTypes,
  u_dmxdevice_manager,
  u_common, VelocityCurve, u_utils, u_audio_manager;



const
  DMX_REFRESH_PERIOD_MS=50;


  FIXTURESEPARATOR='|';
  CHANNELRANGESEPARATOR='\';
  CHANNELRANGEPARAMSEPARATOR=';';

type

  // définitions des effets lumineux dmx
  TDmxEffect=(deNOEFFECT,
              deDimmer,
              deFlame,
              deAudioFollower,
              deCopy,
              deFlameRGB,
              deAudioFollowerRGB,
              deCopyRGB,
              deFlash
             );


  TDmxUniverse = class;
  TDMXFixture = class;

  { TChannelRange }

  TChannelRange = record
    private
      FText: string;
      procedure SetText(AValue: string);
    public
      BeginValue,
      EndValue: byte;
      Symbol: word;    // graphic Symbol id
      function Duplicate: TChannelRange;
      // Decode/Encode are reserved for dmx library
      procedure Decode(s: string);
      function Encode: string;
      property Text: string read FText write SetText; // range description
    end;


  { TBaseDMXChannel }
  TBaseDMXChannel = class
   private
    FPercentValue: single;
    procedure SetPercentValue(AValue: single);
   public
    EffectPainted: integer;
    ByteValuePainted: integer;
    SelectedPainted: boolean;
    RangeIndexPainted: integer;
   public
    Name: string;
    Ranges: array of TChannelRange;
    ChannelType: TChannelType;
    Universe: TDmxUniverse;
    Fixture: TDMXFixture;
    Adress: TDMXAdress;

    Selected: boolean;

    LastByteSendedValue: integer;
    Freezed: boolean;
    HandledByUser: boolean; // =TRUE if channel value is currently modified by user
    CurrentEffect: TDmxEffect;
    ValueFromEffect: single; // [0..1]

    constructor Create;
    destructor Destroy; override;

    function CurrentRangeIndex: integer;
    function TextRange(aRangeIndex: integer): string;
    function CurrentTextRange: string;

    function ByteValue: byte;
    // [0..1]
    property PercentValue: single read FPercentValue write SetPercentValue;
  end;

  { TDMXChannel }

  TDMXChannel = class(TBaseDMXChannel)
  private
    FDimmer: TFParam;
    procedure CreateDimmer;
  private
    FAF_IDAudio: TSoundID;
    FAF_Gain,
    FAF_MaxPercent: single;
    FAF_Average: TAverage;
  private
    FFlameLevelMin,
    FFlameLevelMax,
    FFlameSpeed,
    FFlameSoften: single;
    FFlameWaitTime: single;
  private
    FCopySourceChannel: TDMXChannel;
  private
    FLocked: boolean;
    FWaveKeepTime,
    FWavePercent2,
    FWaveDuration2: single;
    FWaveIDCurve2: word;
    FWaveActivated: boolean;
  private
    function GetIndex: integer;
    procedure SetLocked(AValue: boolean);
  private  // RGB stuff
    FColorQuantity: single; //[0..1]
  private
    FIsVisibleOnViewCursor: boolean;
  public
    FFlashValue, FFlashDuration: single;
    FFlashIsActive: boolean;
  public
    LockedPainted: boolean;
    destructor Destroy; override;

    procedure Update(const aElapsedTime: single);

    procedure StartDimmer(aPercent, aDuration: single; aCurveID: word);
    procedure StartFlame(aLevelMin, aLevelMax, aSpeed, aSoften: single);
    procedure StartAudioFollower(aIDAudio: TSoundID; aGain, aMaxPercent, aSoftenTime: single);
    procedure StartFlash(aLevelMin, aLevelMax, aDurationMin, aDurationMax: single);
    procedure StartCopy(aSourceChannel: TDMXChannel);
    procedure StartInternalWave( aPercent1, aDuration1: single; aCurveID1: word;
                                 aKeepTime: single;
                                 aPercent2, aDuration2: single; aCurveID2: word);
    procedure StopEffect;

    property Index: integer read GetIndex; // channel's index in the fixture
    property Locked: boolean read FLocked write SetLocked; // lock the channel to its current value
    property IsVisibleOnViewCursor: boolean read FIsVisibleOnViewCursor write FIsVisibleOnViewCursor;
  end;



  TDMXChannelsList = class(specialize TFPGObjectList<TDmxChannel>);
  ArrayOfDmxChannels = array of TDmxChannel;

  // On some fixtures, some dips must be set to ON or OFF to enable dmx control mode
  TDipSwitchFunction = (dsfAdress, // dip sets the adress
                        dsfOn,     // dip must be set to ON
                        dsfOff     // dip must be set to OFF
                       );
  ArrayOfDipSwitchFunction = array of TDipSwitchFunction;
  { TDipSwitch }

  TDipSwitch = record
    OnIsUp: boolean; // true if the up position means ON
    MSBIsLeft: boolean; // true if the msb bit is on the left
    Functions: ArrayOfDipSwitchFunction;
    procedure InitByDefault;
    procedure LoadFrom(aDipSwitch: TDipSwitch);
    procedure LoadFrom(t: TStringList);
    procedure SaveTo(t: TStringList);
    function AdressBitCount: integer;
  end;


  { TDMXFixture }

  TDMXFixture = class
   private
     FChannels: TDMXChannelsList;
     FUniverse: TDmxUniverse;
     FAdress: TDMXAdress;
     FDipSwitchs: TDipSwitch;
     FRedChannelIndex,
     FGreenChannelIndex,
     FBlueChannelIndex: integer;
     FHASRGBChannel,
     FSelected: boolean;
     function GetChannel(index: integer): TDMXChannel;
     function GetLastAdress: TDMXAdress;
     procedure SetSelected(AValue: boolean);
     function GetSelected: boolean;
     procedure SetUniverse(AValue: TDmxUniverse);
     procedure SetAdress(AValue: TDMXAdress);
     function GetAdress: TDMXAdress;
   private
      CurrentFixtureEffect: TDMXEffect;
      FRGBFlameSpeed,
      FRGBFlameAmplitude,
      FRGBFlameSoften,
      FRGBFlameWaitTime,
      FRGBFlameNewAmplitude: single;
      FRGBFlame_CurveID: word;
      FRGBFlameChannelCanComputeNewValue: boolean;
      procedure InitEffectRGBRef(aEffect: TDMXEffect; aColor: TColor);
   private
      FRGBAF_IDAudio: TSoundID;
      FRGBAF_Gain: single;
      FRGBAF_Average: TAverage;
      FRGBAF_NewValue: single;
      function GetHaveAdressDipSwitch: boolean;
      function GetLocked: boolean;
      function GetRGBColor: TColor;
      procedure SetLocked(AValue: boolean);
      procedure SetRGBColor(AValue: TColor);
   private
      FIsVisibleOnViewCursor: boolean;
      function GetBlueChannel: TDMXChannel;
      function GetGreenChannel: TDMXChannel;
      function GetRedChannel: TDMXChannel;
   public
     // Parameters for projector view
     ScreenPos: TPointF; // top left coordinates
     Angle: single;
     Zoom: single;
     FlipH, FlipV,
     Selected: boolean;
   public
     Name: string;
     Description: string;
     ID: cardinal;
     FullFilename: string;
     Power,
     Weight: integer;
     FixtureType: TFixtureType;

     constructor Create;
     destructor Destroy; override;

     procedure UpdateHasRGB;

     function SaveToString: string;
     function LoadFromString(const s: string): boolean;

     function ChannelsCount: integer;

     // -1 if not found
     function IndexOfChannel( aChan: TDMXChannel): integer;

     procedure UnselectAllChannels;
     function GetChannelByIndex(aChanIndex: integer): TDMXChannel;
     procedure SetAllChannelsToZero;

     procedure Update(const aElapsedTime: single);

     procedure StartDimmerRGB(aColor: TColor; aDuration: single; aCurveID: word);
     procedure StartFlameRGB(aColor: TColor; aSpeed, aAmplitude, aSoften: single);
     procedure StartAudioFollowerRGB( aIDAudio: TSoundID; aColor: TColor; aGain, aSoftenTime: single);
     procedure StartCopyRGB(aSourceFixture: TDMXFixture);
     procedure StartFlashRGB(aColor: TColor; apcMin, apcMax, aDurationMin, aDurationMax: single);
     procedure StopEffectRGB;

     procedure ClearIsVisibleOnViewCursorOnAllChannels;

     property Channels[index: integer]:TDMXChannel read GetChannel; default;
     property Universe: TDmxUniverse read FUniverse write SetUniverse;
     property Adress: TDMXAdress read GetAdress write SetAdress;
     property LastAdress: TDMXAdress read GetLastAdress;
     property HasRGBChannel: boolean read FHASRGBChannel;
     property RedChannel: TDMXChannel read GetRedChannel;
     property GreenChannel: TDMXChannel read GetGreenChannel;
     property BlueChannel: TDMXChannel read GetBlueChannel;
     property RedChannelIndex: integer read FRedChannelIndex;
     property GreenChannelIndex: integer read FGreenChannelIndex;
     property BlueChannelIndex: integer read FBlueChannelIndex;
     property RGBColor: TColor read GetRGBColor write SetRGBColor;

     property HaveAdressDipSwitch: boolean read GetHaveAdressDipSwitch;
     property DipSwitchs: TDipSwitch read FDipSwitchs;

     property Locked: boolean read GetLocked write SetLocked; // lock the channels of the fixture to their current values

     property IsVisibleOnViewCursor: boolean read FIsVisibleOnViewCursor write FIsVisibleOnViewCursor;
  end;

  TDMXFixtureList = class(specialize TFPGObjectList<TDMXFixture>);
  ArrayOfDmxFixtures = array of TDmxFixture;

  TDMXMapCell = boolean;
  TArrayOfDMXMapCell = array of TDMXMapCell;

  { TDmxUniverse }
  TDmxUniverse = class
  private
     FFixtures:TDMXFixtureList;
     FNeedToBeRedraw: boolean;
     function GetFixture(index: integer): TDMXFixture;
     function GetSize: integer;
  private // map
     FMap: TArrayOfDMXMapCell;
     FOptimizeUsedChannels: boolean;
     procedure SetOptimizeUsedChannels(AValue: boolean);
  public
     ID: cardinal;
     Name,
     ShortName: string;    // U1, U2...
     Color: TBGRAPixel;
     FirstAdress: integer; // first dmx adress available for this universe
     LastAdress: integer;  // last dmx adress

     constructor Create;
     destructor Destroy; override;

     // adressing
     function LastUsedAdress: TDMXAdress;
     function FirstFreeAdress(aChannelCount: integer; out aAdress: TDMXAdress): boolean;
     function ErrorInAdressing: boolean;
     // return TRUE if aAdress is a valid DMX adress [MIN_DMX_ADRESS..MAX_DMX_ADRESS]
     function IsValidAdress(const aAdress: TDMXAdress): boolean;

     // Fixture
     function FixturesCount: integer;
     procedure Fixture_Add(f: TDmxFixture);
     function Fixture_AddFromDMXLib(const aFullFileName: string): TDmxFixture;
     function Fixture_GetByID(aID: cardinal): TDmxFixture;
     function Fixture_WhichContainsThisAdress(aAdress: TDMXAdress): TDMXFixture;

     function  Fixture_GetByAdress(aAdress: TDMXAdress): TDmxFixture;
     procedure Fixture_DeleteByIndex(Index: integer; ShiftOtherAdress: boolean=TRUE);
     procedure Fixture_DeleteByID(aID: cardinal; ShiftOtherAdress: boolean=TRUE);

     procedure Fixture_Extract(aFix: TDMXFixture);
     // Universe
     procedure Clear;
     procedure SaveTo(t: TStringList; aIndex: integer);
     function LoadFrom(t: TStringList; aIndex: integer; aInitDevice: boolean=TRUE): boolean;
     // selection
     procedure Sel_None;
     procedure Sel_All;

     property Fixtures[index: integer]: TDMXFixture read GetFixture;

  public
     DevicePath: TDevicePath;
     function HaveDevice: boolean;
     function IsConnected: boolean;
     function DeviceName: string;
     function DeviceSerialNumber: string;
     function DeviceNameSerialPort: string;
     function DevicePortCanChangeItsDirection: boolean;
     function DevicePortDirection: TPortDirection;
     function UsedChannelCount: integer;
     procedure SendToDevice(aAdress: TDMXAdress; aValue: byte);
     procedure DoOptimizeUsedChannels;
     property Size: integer read GetSize;
     property OptimizeUsedChannels: boolean read FOptimizeUsedChannels write SetOptimizeUsedChannels;
  public
     procedure Update(const aElapsedTime: single);
     property NeedToBeRedraw: boolean read FNeedToBeRedraw write FNeedToBeRedraw;
  end;


{ TUniverseManager}
TDMXUniverseList = class(specialize TFPGObjectList<TDmxUniverse>);

TUniverseManager = class
  public
     class var UniverseIDValue: cardinal;
     class var FixtureIDValue: cardinal;
     class procedure ResetUniverseIDValue; static;
     class function NextUniverseIDValue: cardinal; static;
     class procedure ResetFixtureIDValue; static;
     class function NextFixtureIDValue: cardinal; static;
  private
     FUniverses: TDMXUniverseList;
     function GetCount: integer;
     function GetTotalFixtureCount: integer;
     function GetUniverse(index: integer): TDmxUniverse;
  private
     FTimeOrigin: QWORD;
     FThreadAction: TTimedThread;
  public
     constructor Create;
     destructor Destroy; override;
     procedure StartThread;
     procedure StopThread;

     function RetrieveFixture(aIDUniverse, aIDFixture: cardinal;
                              out uni: TDMXUniverse;
                              out fix: TDMXFixture): boolean;
     function RetrieveChannel(aIDUniverse, aIDFixture: cardinal; aChanIndex: integer;
                              out uni: TDMXUniverse;
                              out fix: TDMXFixture;
                              out chan: TDMXChannel): boolean;
  // Universe List
     procedure Clear;
     function IndexOf(aUniverse: TDMXUniverse): integer;
     function GetUniverseByID(aID: cardinal): TDMXUniverse;
     function UniverseIDToUniverseIndex(aID: cardinal): integer;
     function Add(const aName: string): TDmxUniverse;
     procedure Delete(aIndex: integer);
     function ValidIndex(aIndex: integer): boolean;

     function Save: boolean;
     procedure SaveTo(t: TStringList);
     // return TRUE if the specified project contains the same dmx configuration than the actual opened project
     //   - same universe count
     //   - same fixture at same adress with the same ID
     function SameDMXConfiguration(const aProjectFilename: string): boolean;
     function Load(aInitDevice: boolean=True): boolean;
     function LoadFrom(t: TStringList; aInitDevice: boolean=TRUE): boolean;
     function LoadFromProject(const aProjectFilename: string): boolean;
  // Fixture
     procedure Fixture_DeleteByID(aID: cardinal; ShiftOtherAdress: boolean=TRUE);

  // selection
     procedure Sel_None;
     procedure Sel_All;
     procedure GetSelectedFixtures(var A: ArrayOfDmxFixtures);

     // set all dmx channels to 0, stop all effects
     procedure BlackOut;

     // compute effects
     procedure Update;


     property Universes[index: integer]: TDmxUniverse read GetUniverse;
     property Count: integer read GetCount;
     property TotalFixtureCount: integer read GetTotalFixtureCount;
 end;


var
 UniverseManager: TUniverseManager;


implementation
uses u_resource_string, u_helper, u_logfile, u_dmx_util,
  Math, LCLIntf, u_global_var, u_project_manager, u_apputils;

{ TDipSwitch }
const
      DIPSWITCH_HEADER = '[DIPSWITCH]';


procedure TDipSwitch.InitByDefault;
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
var A: TStringArray;
  i, k: integer;
begin
  k := t.indexOf(DIPSWITCH_HEADER);
  if k = -1 then
   InitByDefault
  else
  begin
    A := t.Strings[k+1].SplitToArray(DIPSWITCH_SEPARATOR);
    OnIsUp := A[0]='OnIsUp';
    MSBIsLeft := A[1]='MSBIsLeft';
    SetLength(Functions, A[2].ToInteger);
    k := 3;
    for i:=0 to High(Functions) do begin
      Functions[i] := TDipSwitchFunction(A[k].ToInteger);
      inc(k);
    end;
  end;
end;

procedure TDipSwitch.SaveTo(t: TStringList);
var i: integer;
  s: string;
begin
  if Length(Functions)=0 then
    exit;

  t.Add(DIPSWITCH_HEADER);
  if OnIsUp then
    s := 'OnIsUp'+DIPSWITCH_SEPARATOR
  else
    s := 'OnIsDown'+DIPSWITCH_SEPARATOR;
  if MSBIsLeft then
    s := s+'MSBIsLeft'+DIPSWITCH_SEPARATOR
  else
    s := s+'MSBIsRight'+DIPSWITCH_SEPARATOR;
  s := s+Length(Functions).ToString;
  for i:=0 to High(Functions) do
    s := s+DIPSWITCH_SEPARATOR+Ord(Functions[i]).ToString;
  t.Add(s);
end;

function TDipSwitch.AdressBitCount: integer;
var i: integer;
begin
  Result := 0;
  for i:=0 to High(Functions) do
    if Functions[i]=dsfAdress then
      inc(Result);
end;

{ TDMXChannel }

procedure TDMXChannel.CreateDimmer;
begin
  if FDimmer = NIL then
    FDimmer := TFParam.Create;
end;

function TDMXChannel.GetIndex: integer;
begin
  Result := Fixture.IndexOfChannel(self);
end;

procedure TDMXChannel.SetLocked(AValue: boolean);
begin
  if FLocked = AValue then Exit;
  FLocked := AValue;
end;

destructor TDMXChannel.Destroy;
begin
  if FDimmer <> NIL then
    FDimmer.Free;
  if FAF_Average<>NIL then
    FAF_Average.Free;
  inherited Destroy;
end;

procedure TDMXChannel.Update(const aElapsedTime: single);
var b: byte;
 vf: single;
 flagUpdateCursorView: boolean;
begin
  flagUpdateCursorView := False;

  if HandledByUser then
  begin
    // user handles the cursor-> effect is cancelled
    if CurrentEffect in [deFlameRGB, deAudioFollowerRGB] then
      Fixture.CurrentFixtureEffect := deNOEFFECT;

    flagUpdateCursorView := CurrentEffect <> deNOEFFECT;
    CurrentEffect := deNOEFFECT;
    ValueFromEffect := PercentValue;
  end
  else if CurrentEffect <> deNOEFFECT then
  begin
    // compute effect
    case CurrentEffect of
      deDimmer:
        begin
          FDimmer.OnElapse(aElapsedTime);
          ValueFromEffect := FDimmer.Value;
          if FDimmer.State = psNO_CHANGE then
          begin
            if FWaveActivated then
            begin
              if FWaveKeepTime > 0 then
                FWaveKeepTime := FWaveKeepTime-aElapsedTime
              else
              begin
                StartDimmer(FWavePercent2, FWaveDuration2, FWaveIDCurve2);
                FWaveActivated := FALSE;
              end;
            end
            else CurrentEffect := deNOEFFECT;
          end;
      end;

      deAudioFollower:
        begin
          FAF_Average.Push(SoundManager.GetLevel(FAF_IDAudio));
          ValueFromEffect := FAF_Average.Average;
          ValueFromEffect := ValueFromEffect+ValueFromEffect*FAF_Gain;
          ValueFromEffect := EnsureRange(ValueFromEffect, 0.0, FAF_MaxPercent);
      end;

      deFlame:
        begin
          FFlameWaitTime := FFlameWaitTime-aElapsedTime;
          if FFlameWaitTime <= 0 then
          begin
            // recompute another wait time
            FFlameWaitTime := DMX_REFRESH_PERIOD_MS*0.001+random*FFlameSpeed;

            //recompute new value
            vf := (FFlameLevelMax-FFlameLevelMin)*random+FFlameLevelMin;
            vf := EnsureRange(vf, 0.0, 1.0);

            FDimmer.ChangeTo(vf, FFlameWaitTime*FFlameSoften, Random(5));
          end
          else FDimmer.OnElapse(aElapsedTime);
        ValueFromEffect := FDimmer.Value;
      end;

      deCopy:
        begin
          ValueFromEffect:=FCopySourceChannel.PercentValue;
      end;

      deFlameRGB:
        begin
          with Fixture do
            if FRGBFlameChannelCanComputeNewValue then
            begin
              vf := FColorQuantity - FRGBFlameNewAmplitude*FColorQuantity;
              vf := EnsureRange(vf, 0.0, 1.0);
              FDimmer.ChangeTo(vf, Fixture.FRGBFlameWaitTime*Fixture.FRGBFlameSoften, FRGBFlame_CurveID);
            end;

          FDimmer.OnElapse(aElapsedTime);
          ValueFromEffect := FDimmer.Value;
      end;

      deAudioFollowerRGB:
        begin
          ValueFromEffect := EnsureRange(Fixture.FRGBAF_NewValue*FColorQuantity, 0.0, 1.0);
        end;


    end;//case

    flagUpdateCursorView := ((PercentToDMXByte(ValueFromEffect) <> ByteValue) and not FLocked)
                             or (CurrentEffect = deNOEFFECT);

    if not FLocked then
      PercentValue := ValueFromEffect;
  end; // end compute effects


  if FFlashIsActive then
  begin
    FFlashDuration := FFlashDuration - aElapsedTime;
    flagUpdateCursorView := flagUpdateCursorView or
        (not FLocked and ((PercentValue <> FFlashValue) or (FFlashDuration <= 0)));
    b := PercentToDMXByte(FFlashValue);
    FFlashIsActive := FFlashDuration > 0;
    flagUpdateCursorView := flagUpdateCursorView or not FFlashIsActive;
  end
  else b := PercentToDMXByte(FPercentValue);


  // redraw on projector view
  Universe.NeedToBeRedraw := Universe.NeedToBeRedraw or flagUpdateCursorView;
  // redraw on cursor view
  if flagUpdateCursorView and FIsVisibleOnViewCursor then
    FProjectorViewToRefreshForThreadUniverse.FrameViewDMXCursors1.RedrawCursor(self);

  // Send value to DMX device
  if LastByteSendedValue <> b then
  begin
    Universe.SendToDevice(Adress, b);
    LastByteSendedValue := b;
  end;
end;

procedure TDMXChannel.StartDimmer(aPercent, aDuration: single; aCurveID: word);
begin
  if (aPercent=PercentValue) or (HandledByUser) then
    exit;

  CreateDimmer;
  FWaveActivated := FALSE;

  if aDuration < 0.01 then
  begin
    PercentValue := aPercent;
    ValueFromEffect := aPercent;
    FDimmer.Value := aPercent;
    Universe.NeedToBeRedraw := TRUE;
  end
  else
  begin
    FDimmer.Value := PercentValue;
    FDimmer.ChangeTo(aPercent, aDuration, aCurveID);
    CurrentEffect := deDimmer;
  end;
end;

procedure TDMXChannel.StartFlame(aLevelMin, aLevelMax, aSpeed, aSoften: single);
begin
  if HandledByUser then
    exit;

  CreateDimmer;
  FWaveActivated := FALSE;

  FFlameLevelMin := aLevelMin;
  FFlameLevelMax := aLevelMax;
  FFlameSpeed := aSpeed;
  FFlameSoften := aSoften;

  FDimmer.Value := PercentValue;
  CurrentEffect := deFlame;
end;

procedure TDMXChannel.StartAudioFollower(aIDAudio: TSoundID; aGain,
  aMaxPercent, aSoftenTime: single);
begin
  FWaveActivated := FALSE;
  if FAF_Average = NIL then
    FAF_Average := TAverage.Create;
  FAF_IDAudio := aIDAudio;
  FAF_Gain := aGain;
  FAF_MaxPercent := aMaxPercent;
  FAF_Average.Count := Round(aSoftenTime/(DMX_REFRESH_PERIOD_MS*0.001));
  CurrentEffect := deAudioFollower;
end;

procedure TDMXChannel.StartFlash(aLevelMin, aLevelMax, aDurationMin, aDurationMax: single);
begin
  if (aLevelMin < 0) or (aLevelMin > 1) or
     (aLevelMax < 0) or (aLevelMax > 1) or
     (aDurationMin < 0) or (aDurationMax < 0) or
     (aDurationMin > aDurationMax) then
  begin
    Log.Error('TDMXChannel.StartFlash - Received bad parameter: LevelMin='+
              FormatFloatWithDot('0.000', aLevelMin)+
              ' LevelMax='+FormatFloatWithDot('0.000', aLevelMax)+
              ' DurationMin='+FormatFloatWithDot('0.000', aDurationMin)+
              ' DurationMax='+FormatFloatWithDot('0.000', aDurationMax));
    exit;
  end;

  if aLevelMin <> aLevelMax then
    FFlashValue := aLevelMin+Random*(aLevelMax-aLevelMin)
  else
    FFlashValue := aLevelMax;

  if aDurationMin <> aDurationMax then
    FFlashDuration := aDurationMin+Random*(aDurationMax-aDurationMin)
  else
    FFlashDuration := aDurationMax;

  FFlashIsActive := True;
end;

procedure TDMXChannel.StartCopy(aSourceChannel: TDMXChannel);
begin
  if (aSourceChannel <> NIL) and (aSourceChannel <> Self) then
  begin
    FCopySourceChannel := aSourceChannel;
    CurrentEffect := deCopy;
    FWaveActivated := FALSE;
    Universe.FNeedToBeRedraw := TRUE;
  end;
end;

procedure TDMXChannel.StartInternalWave(aPercent1, aDuration1: single;
  aCurveID1: word; aKeepTime: single; aPercent2, aDuration2: single;
  aCurveID2: word);
begin
  CreateDimmer;
  FDimmer.Value := PercentValue;
  FDimmer.ChangeTo(aPercent1, aDuration1, aCurveID1);
  CurrentEffect := deDimmer;

  FWaveActivated := TRUE;
  FWaveKeepTime := aKeepTime;
  FWavePercent2 := aPercent2;
  FWaveDuration2 := aDuration2;
  FWaveIDCurve2 := aCurveID2;
end;

procedure TDMXChannel.StopEffect;
begin
  CurrentEffect := deNOEFFECT;
  FWaveActivated := FALSE;
end;


{ TBaseDMXChannel }

procedure TBaseDMXChannel.SetPercentValue(AValue: single);
begin
  FPercentValue := AValue;
end;

constructor TBaseDMXChannel.Create;
begin
 Name := SUnknown;
 Adress := 0;
 ChannelType := ctDimmer;
 SetLength(Ranges, 1);
 Ranges[0].BeginValue := 0;
 Ranges[0].EndValue := 255;
 Ranges[0].Text := SUnknown;
 Selected := FALSE;
 Freezed := FALSE;
 HandledByUser := FALSE;
 CurrentEffect := deNOEFFECT;
 ValueFromEffect := 0.0;
 PercentValue := 0.0;
 LastByteSendedValue := -1;
 RangeIndexPainted := -1;
end;

destructor TBaseDMXChannel.Destroy;
begin
  inherited Destroy;
end;

function TBaseDMXChannel.CurrentRangeIndex: integer;
var b: byte;
    i: integer;
begin
  b := ByteValue;
  for i:=0 to High(Ranges) do
   if (b >= Ranges[i].BeginValue) and (b <= Ranges[i].EndValue) then
   begin
     Result := i;
     exit;
   end;
  Result := -1;
end;

function TBaseDMXChannel.TextRange(aRangeIndex: integer): string;
begin
  if (aRangeIndex >= 0) and (aRangeIndex <= High(Ranges)) then
    Result := Ranges[aRangeIndex].FText
  else
    Result := SNotDefined;
end;

function TBaseDMXChannel.CurrentTextRange: string;
var b: byte;
    i: integer;
begin
  b := ByteValue;
  for i:=0 to High(Ranges) do
   if (b >= Ranges[i].BeginValue) and (b <= Ranges[i].EndValue) then
   begin
     Result := Ranges[i].FText;
     exit;
   end;
  Result := SNotDefined;
end;

function TBaseDMXChannel.ByteValue: byte;
begin
  Result := PercentToDMXByte(FPercentValue);
end;


{ TChannelRange }

procedure TChannelRange.SetText(AValue: string);
begin
  if FText = AValue then Exit;
  FText := AValue;
  FText := ReplaceForbidenCharByUnderscore(FText, FIXTURESEPARATOR+CHANNELRANGESEPARATOR+CHANNELRANGEPARAMSEPARATOR);
end;

function TChannelRange.Duplicate: TChannelRange;
begin
 Result.BeginValue := BeginValue;
 Result.EndValue := EndValue;
 Result.Text := Text;
 Result.Symbol := Symbol;
end;

procedure TChannelRange.Decode(s: string);
var be, en: integer;
    des: string;
begin
  DecodeDMXChannelRange(s, be, en, des);
  BeginValue := be;       // 1 based
  EndValue := en;
  Text := des;
end;

function TChannelRange.Encode: string;
begin
 Result := EncodeDMXChannelRange(BeginValue, EndValue, Text);
end;

function TDMXFixture.SaveToString: string;
var chan: TDMXChannel;
    prop: TPackProperty;
    i: integer;
begin
  prop.Init(FIXTURESEPARATOR);
  prop.Add('Fixture', PathRelativeToDMXLibrary(FullFilename));
  prop.Add('ID', integer(ID));
  prop.Add('Adress', Adress);
  prop.Add('Description', Description);
  prop.Add('ScreenPosX', ScreenPos.x);
  prop.Add('ScreenPosY', ScreenPos.y);
  prop.Add('Angle', Angle);
  prop.Add('Zoom', Zoom);
  prop.Add('FlipH', FlipH);
  prop.Add('FlipV', FlipV);
  prop.Add('ChannelCount', FChannels.Count);
  i := 1;
  for chan in FChannels do
  begin
    prop.Add('ChanName'+i.ToString, chan.Name);
    prop.Add('Locked'+i.ToString, chan.Locked);
    inc(i);
  end;

  Result := prop.PackedProperty

{ Result := PathRelativeToDMXLibrary(FullFilename)+FIXTURESEPARATOR+
           ID.ToString+FIXTURESEPARATOR+
           Adress.ToString+FIXTURESEPARATOR+
           Description+FIXTURESEPARATOR+
           FormatFloat('0.000', ScreenPos.x)+FIXTURESEPARATOR+
           FormatFloat('0.000', ScreenPos.y)+FIXTURESEPARATOR+
           FormatFloat('0.000', Angle)+FIXTURESEPARATOR+
           FormatFloat('0.000', Zoom)+FIXTURESEPARATOR+
           FlipH.ToInteger.ToString+FIXTURESEPARATOR+
           FlipV.ToInteger.ToString+FIXTURESEPARATOR+
           FChannels.Count.ToString;
 for chan in FChannels do
   Result := Result+FIXTURESEPARATOR+chan.Name+
                  FIXTURESEPARATOR+chan.Locked.ToInteger.ToString;  }
end;

function TDMXFixture.LoadFromString(const s: string): boolean;
var i, c, vi: integer;
  libfix: TLibraryFixture;
  chan: TDMXChannel;

  prop: TSplitProperty;
  s1: string;
  flag, vb: boolean;
  vs: single;
  procedure LogMissingProperty(const apropName: string);
  begin
    Log.Error('TDMXFixture.LoadFromString - Property '+apropName+' not found', 3);
  end;

begin
  vi := 0;
  c := 0;
  vb := False;
  vs := 0;
  s1 := '';

  prop.Split(s, FIXTURESEPARATOR);

  try
    flag := prop.StringValueOf('Fixture', s1, '');
    if not flag then
      LogMissingProperty('Fixture')
    else
    begin
      s1 := AdjustDirectorySeparator(s1);
      flag := libfix.LoadFromFile(GetAppDMXLibraryFolder+s1);
      if flag then
      begin
        Name := libfix.Name;
        Power := libfix.Power;
        FixtureType := libfix.FixtureType;
        FDipSwitchs.LoadFrom(libfix.DipSwitch);
      end
      else
      begin
        Log.Error('TDMXFixture.LoadFromString - Fail to load fixture "'+s1+'" from library');
        Name := SFixtureNotInLibrary;
        Power := 0;
        FixtureType := ftOther;
        FDipSwitchs.InitByDefault;
      end;
    end;

    FullFilename := ConcatPaths([GetAppDMXLibraryFolder, s1]);
    if not prop.IntegerValueOf('ID', integer(ID), 0) then
      LogMissingProperty('ID');

    if not prop.StringValueOf('Description', Description, '') then
      LogMissingProperty('Description');

    if not prop.SingleValueOf('ScreenPosX', vs, 0) then
      LogMissingProperty('ScreenPosX');
    ScreenPos.X := vs;

    if not prop.SingleValueOf('ScreenPosY', vs, 0) then
      LogMissingProperty('ScreenPosY');
    ScreenPos.Y := vs;

    if not prop.SingleValueOf('Angle', Angle, 0.0) then
      LogMissingProperty('Angle');

    if not prop.SingleValueOf('Zoom', Zoom, 1.0) then
      LogMissingProperty('Zoom');

    if not prop.BooleanValueOf('FlipH', FlipH, False) then
      LogMissingProperty('FlipH');

    if not prop.BooleanValueOf('FlipV', FlipV, False) then
      LogMissingProperty('FlipV');

    if not prop.IntegerValueOf('ChannelCount', c, 1) then
      LogMissingProperty('ChannelCount');

    for i:=1 to c do
     begin
      chan := TDMXChannel.Create;
      chan.Fixture := Self;
      FChannels.Add(chan);

      if not prop.StringValueOf('ChanName'+i.ToString, s1, SUnknown) then
        LogMissingProperty('ChanName'+i.ToString);
      chan.Name := s1;

      if not prop.BooleanValueOf('Locked'+i.ToString, vb, False) then
        LogMissingProperty('Locked'+i.ToString);
      chan.Locked := vb;

      chan.Universe := Universe;

      if flag then
      begin
        chan.ChannelType := libfix.Channels[i-1].ChannelType;
        chan.Ranges := libfix.Channels[i-1].Ranges;
      end;
    end;

    if not prop.IntegerValueOf('Adress', vi, 1) then
      LogMissingProperty('Adress');
    Adress := vi; // to do after channels creation !!
    UpdateHasRGB;
    Result := TRUE;
  except
    Result := FALSE;
  end;

{
  try
    A := s.Split([FIXTURESEPARATOR]);
    flag := libfix.LoadFromFile(DMXLibraryPath+AdjustDirectorySeparator(A[0]));
    if flag then
    begin
      Name := libfix.Name;
      Power := libfix.Power;
      FixtureType := libfix.FixtureType;
      FDipSwitchs.LoadFrom(libfix.DipSwitch);
    end
    else
    begin
      Name := SFixtureNotInLibrary;
      Power := 0;
      FixtureType := ftOther;
    end;

    FullFilename := ConcatPaths([DMXLibraryPath, A[0]]);
    ID := A[1].ToInteger;
    Description := A[3];
    ScreenPos.x := StringToSingle(A[4]);
    ScreenPos.y := StringToSingle(A[5]);
    Angle := StringToSingle(A[6]);
    Zoom := StringToSingle(A[7]);
    FlipH := A[8].ToInteger.ToBoolean;
    FlipV := A[9].ToInteger.ToBoolean;
    c := A[10].ToInteger;
    k := 11;
    for i:=0 to c-1 do
     begin
      chan := TDMXChannel.Create;
      chan.Fixture := Self;
      FChannels.Add(chan);
      chan.Name := A[k];
      inc(k);
      chan.Universe := Universe;

      if High(A) >= k then
      begin
        chan.Locked := A[k].ToInteger.ToBoolean;
        inc(k);
      end;

      if flag then
      begin
         chan.ChannelType := libfix.Channels[i].ChannelType;
         chan.Ranges := libfix.Channels[i].Ranges;
      end;

    end;
    Adress := A[2].ToInteger; // to do after channels creation
    UpdateHasRGB;
    Result := TRUE;
  except
    Result := FALSE;
  end;
}
end;

constructor TDMXFixture.Create;
begin
  FChannels := TDMXChannelsList.Create(TRUE);
  ScreenPos := PointF(0,0);
  Angle := 0;
  Zoom := 1.0;
  FlipH := FALSE;
  FlipV := FALSE;
  ID := 0;
  FullFilename := '';
  FixtureType := ftOther;
  Power := 0;
  FUniverse := NIL;
  FAdress := 0;
  Name := '';
  Description := '';
  FHASRGBChannel := FALSE;
  FDipSwitchs.InitByDefault;
end;

destructor TDMXFixture.Destroy;
begin
 if FRGBAF_Average <> NIL then
   FRGBAF_Average.Free;

  FChannels.Free;
  inherited Destroy;
end;

procedure TDMXFixture.SetUniverse(AValue: TDmxUniverse);
var chan: TDMXChannel;
begin
 FUniverse := AValue;
 for chan in FChannels do
   chan.Universe := AValue;
end;

function TDMXFixture.GetLastAdress: TDMXAdress;
begin
  Result := Adress+ChannelsCount-1;
end;

function TDMXFixture.GetChannel(index: integer): TDMXChannel;
begin
  Result := FChannels.Items[index];
end;

procedure TDMXFixture.SetSelected(AValue: boolean);
begin
  FSelected := AValue;
end;

function TDMXFixture.GetSelected: boolean;
begin
  Result := FSelected;
end;

procedure TDMXFixture.SetAdress(AValue: TDMXAdress);
var i: integer;
begin
  FAdress:=AVAlue;
  for i:=0 to FChannels.Count-1 do
    Channels[i].Adress := AVAlue+i;
end;

function TDMXFixture.GetAdress: TDMXAdress;
begin
  Result := FAdress;
end;

procedure TDMXFixture.InitEffectRGBRef(aEffect: TDMXEffect; aColor: TColor);
begin
  if not HasRGBChannel then exit;

  Channels[FRedChannelIndex].CurrentEffect := aEffect;
  Channels[FRedChannelIndex].FColorQuantity := Red(aColor)/255;

  Channels[FGreenChannelIndex].CurrentEffect := aEffect;
  Channels[FGreenChannelIndex].FColorQuantity := Green(aColor)/255;

  Channels[FBlueChannelIndex].CurrentEffect := aEffect;
  Channels[FBlueChannelIndex].FColorQuantity := Blue(aColor)/255;

{  FEffectRedRef := Red(aColor)/255;
  FEffectGreenRef := Green(aColor)/255;
  FEffectBlueRef := Blue(aColor)/255;     }
end;

function TDMXFixture.GetRGBColor: TColor;
begin
  if HasRGBChannel then
    Result := RGBToColor(FChannels[FRedChannelIndex].ByteValue,
                       FChannels[FGreenChannelIndex].ByteValue,
                       FChannels[FBlueChannelIndex].ByteValue)
  else Result := clNone;
end;

function TDMXFixture.GetLocked: boolean;
var i: integer;
begin
  Result := FALSE;
  for i:=0 to ChannelsCount-1 do
   Result := Result or Channels[i].Locked;
end;

function TDMXFixture.GetHaveAdressDipSwitch: boolean;
begin
  Result := Length(FDipSwitchs.Functions) > 0;
end;

procedure TDMXFixture.SetLocked(AValue: boolean);
var i: integer;
begin
  for i:=0 to ChannelsCount-1 do
   Channels[i].Locked := AValue;
end;

procedure TDMXFixture.SetRGBColor(AValue: TColor);
begin
  if HasRGBChannel then
  begin
    FChannels[FRedChannelIndex].PercentValue := Red(AValue)/255;
    FChannels[FRedChannelIndex].CurrentEffect := deNOEFFECT;
    FChannels[FGreenChannelIndex].PercentValue := Green(AValue)/255;
    FChannels[FGreenChannelIndex].CurrentEffect := deNOEFFECT;
    FChannels[FBlueChannelIndex].PercentValue := Blue(AValue)/255;
    FChannels[FBlueChannelIndex].CurrentEffect := deNOEFFECT;
  end;
end;

function TDMXFixture.GetBlueChannel: TDMXChannel;
begin
  if HasRGBChannel then
    Result := Channels[FRedChannelIndex]
  else
  Result := NIL;
end;

function TDMXFixture.GetGreenChannel: TDMXChannel;
begin
  if HasRGBChannel then
    Result := Channels[FGreenChannelIndex]
  else
  Result := NIL;
end;

function TDMXFixture.GetRedChannel: TDMXChannel;
begin
  if HasRGBChannel then
    Result := Channels[FBlueChannelIndex]
  else
  Result := NIL;
end;

function TDMXFixture.ChannelsCount: integer;
begin
  Result := FChannels.Count;
end;

function TDMXFixture.IndexOfChannel(aChan: TDMXChannel): integer;
begin
  Result := FChannels.IndexOf(aChan);
end;

procedure TDMXFixture.UnselectAllChannels;
var i: integer;
begin
  for i:=0 to ChannelsCount-1 do
    Channels[i].Selected := FALSE;
end;

function TDMXFixture.GetChannelByIndex(aChanIndex: integer): TDMXChannel;
begin
  if (aChanIndex < 0) or (aChanIndex > self.ChannelsCount-1) then
    Result := NIL
  else
    Result := Channels[aChanIndex];
end;

procedure TDMXFixture.SetAllChannelsToZero;
var chan: TDMXChannel;
begin
  CurrentFixtureEffect := deNOEFFECT;
  for chan in FChannels do
  begin
    chan.PercentValue := 0.0;
    chan.CurrentEffect := deNOEFFECT;
  end;
end;

procedure TDMXFixture.Update(const aElapsedTime: single);
var chan: TDMXChannel;
begin
  // compute fixture effect
  case CurrentFixtureEffect of
    deAudioFollowerRGB:
      begin
        FRGBAF_Average.Push(SoundManager.GetLevel(FRGBAF_IDAudio));
        FRGBAF_NewValue := FRGBAF_Average.Average;
        FRGBAF_NewValue := FRGBAF_NewValue + FRGBAF_NewValue*FRGBAF_Gain;
    end;

    deFlameRGB:
      begin
        FRGBFlameWaitTime := FRGBFlameWaitTime-aElapsedTime;
        FRGBFlameChannelCanComputeNewValue := FRGBFlameWaitTime <= 0;
        if FRGBFlameChannelCanComputeNewValue then
        begin
          // recompute new parameters
          FRGBFlameWaitTime := FRGBFlameSpeed+ random*FRGBFlameSpeed*0.5;//DMX_REFRESH_PERIOD_MS*0.001+random*FFlameSpeed;
          FRGBFlameNewAmplitude := FRGBFlameAmplitude*random;
          FRGBFlame_CurveID := Random(5);
        end;
    end;
  end;//case

  for chan in FChannels do
   chan.Update(aElapsedTime);
end;

procedure TDMXFixture.StartDimmerRGB(aColor: TColor; aDuration: single; aCurveID: word);
begin
  if not HasRGBChannel then exit;

  FChannels[FRedChannelIndex].StartDimmer(Red(aColor)/255, aDuration, aCurveID);
  FChannels[FGreenChannelIndex].StartDimmer(Green(aColor)/255, aDuration, aCurveID);
  FChannels[FBlueChannelIndex].StartDimmer(Blue(aColor)/255, aDuration, aCurveID);
end;

procedure TDMXFixture.StartFlameRGB(aColor: TColor; aSpeed, aAmplitude, aSoften: single);
begin
  if not HasRGBChannel then exit;

  Channels[FRedChannelIndex].CreateDimmer;
  Channels[FGreenChannelIndex].CreateDimmer;
  Channels[FBlueChannelIndex].CreateDimmer;

  InitEffectRGBRef(deFlameRGB, aColor);

  FRGBFlameSpeed := aSpeed;
  FRGBFlameAmplitude := aAmplitude;
  FRGBFlameSoften := aSoften;
  //FRGBFlameWaitTime:=0;

  CurrentFixtureEffect := deFlameRGB;

{  Channels[FRedChannelIndex].CurrentEffect := deFlameRGB;
  Channels[FGreenChannelIndex].CurrentEffect := deFlameRGB;
  Channels[FBlueChannelIndex].CurrentEffect := deFlameRGB;  }

  Channels[FRedChannelIndex].FDimmer.Value := Channels[FRedChannelIndex].PercentValue;
  Channels[FGreenChannelIndex].FDimmer.Value := Channels[FGreenChannelIndex].PercentValue;
  Channels[FBlueChannelIndex].FDimmer.Value := Channels[FBlueChannelIndex].PercentValue;
end;

procedure TDMXFixture.StartAudioFollowerRGB(aIDAudio: TSoundID; aColor: TColor;
  aGain, aSoftenTime: single);
begin
  if not HasRGBChannel then exit;

  if FRGBAF_Average = NIL then
    FRGBAF_Average := TAverage.Create;

  InitEffectRGBRef(deAudioFollowerRGB, aColor);

  FRGBAF_IDAudio := aIDAudio;
  FRGBAF_Gain := aGain;
  FRGBAF_Average.Count := Round(aSoftenTime/(DMX_REFRESH_PERIOD_MS*0.001));
  CurrentFixtureEffect := deAudioFollowerRGB;
end;

procedure TDMXFixture.StartCopyRGB(aSourceFixture: TDMXFixture);
begin
  if not HasRGBChannel then exit;
  if aSourceFixture = NIL then exit;
  if not aSourceFixture.HasRGBChannel then exit;

  Channels[FRedChannelIndex].StartCopy(aSourceFixture.Channels[aSourceFixture.FRedChannelIndex]);
  Channels[FGreenChannelIndex].StartCopy(aSourceFixture.Channels[aSourceFixture.FGreenChannelIndex]);
  Channels[FBlueChannelIndex].StartCopy(aSourceFixture.Channels[aSourceFixture.FBlueChannelIndex]);
end;

procedure TDMXFixture.StartFlashRGB(aColor: TColor; apcMin, apcMax, aDurationMin, aDurationMax: single);
var intensity, duration, v: single;
begin
  if not HasRGBChannel then exit;
  if (apcMin < 0) or (apcMin > 1) or
     (apcMax < 0) or (apcMax > 1) or
     (apcMin > apcMax) or
     (aDurationMin < 0) or (aDurationMax < 0) or
     (aDurationMin > aDurationMax) then
  begin
    Log.Error('TDMXFixture.StartFlashRGB - Received bad parameter: pcMin='+
              FormatFloatWithDot('0.000', apcMin)+
              ' pcMax='+FormatFloatWithDot('0.000', apcMax)+
              ' DurationMin='+FormatFloatWithDot('0.000', aDurationMin)+
              ' DurationMax='+FormatFloatWithDot('0.000', aDurationMax));
    exit;
  end;

  if apcMin = apcMax then intensity := apcMin
    else intensity := apcMin + Random*(apcMax-apcMin);

  if aDurationMin = aDurationMax then duration := aDurationMin
    else duration := aDurationMin+Random*(aDurationMax-aDurationMin);

  v := Red(aColor)/255*intensity;
  Channels[FRedChannelIndex].StartFlash(v, v, duration, duration);
  v := Green(aColor)/255*intensity;
  Channels[FGreenChannelIndex].StartFlash(v, v, duration, duration);
  v := Blue(aColor)/255*intensity;
  Channels[FBlueChannelIndex].StartFlash(v, v, duration, duration);
end;

procedure TDMXFixture.StopEffectRGB;
begin
  if not HasRGBChannel then exit;

  CurrentFixtureEffect := deNOEFFECT;
  Channels[FRedChannelIndex].StopEffect;
  Channels[FGreenChannelIndex].StopEffect;
  Channels[FBlueChannelIndex].StopEffect;
end;

procedure TDMXFixture.ClearIsVisibleOnViewCursorOnAllChannels;
var i: Integer;
begin
  IsVisibleOnViewCursor := False;
  for i:=0 to ChannelsCount-1 do
    Channels[i].IsVisibleOnViewCursor := False;
end;

procedure TDMXFixture.UpdateHasRGB;
var i: integer;
begin
 // search for rgb channels
 FRedChannelIndex := -1; FGreenChannelIndex := -1; FBlueChannelIndex := -1;
 for i:=0 to FChannels.Count-1 do
 begin
   if Channels[i].ChannelType=ctRED then
     FRedChannelIndex:=i;
   if Channels[i].ChannelType=ctGREEN then
     FGreenChannelIndex:=i;
   if Channels[i].ChannelType=ctBLUE then
     FBlueChannelIndex:=i;
 end;
 FHasRGBChannel := (FRedChannelIndex <> -1) and
                   (FGreenChannelIndex <> -1) and
                   (FBlueChannelIndex <> -1);
end;


{ TDmxUniverse }

procedure TDmxUniverse.Fixture_Add(f: TDmxFixture);
var i: integer;
begin
  f.ID := TUniverseManager.NextFixtureIDValue;
  FFixtures.Add(f);
  f.Universe := Self;
  f.UpdateHasRGB;
  for i:=0 to f.FChannels.Count-1 do
   f.Channels[i].Fixture := f;
  DoOptimizeUsedChannels;
end;

function TDmxUniverse.Fixture_AddFromDMXLib(const aFullFileName: string): TDmxFixture;
var libfix: TLibraryFixture;
  f: TDmxFixture;
  chan: TDMXChannel;
  i: integer;
begin
  Result := NIL;
  if not libfix.LoadFromFile(aFullFileName) then exit;

  f := TDmxFixture.Create;
  // copy channels parameters

  for i:=0 to High(libfix.Channels) do
  begin
    chan := TDMXChannel.Create;
    chan.Name := libfix.Channels[i].Name;
    chan.ChannelType := libfix.Channels[i].ChannelType;
    chan.Ranges := libfix.Channels[i].Ranges;
    f.FChannels.Add(chan);
  end;

  f.FixtureType := libfix.FixtureType;
  f.Power := libfix.Power;
  f.Name := libfix.Name;
  f.FullFilename := aFullFileName;
  f.Universe := Self;
  f.DipSwitchs.LoadFrom(libfix.DipSwitch);

  Fixture_Add(f);
  Result := f;
end;

function TDmxUniverse.Fixture_GetByID(aID: cardinal): TDmxFixture;
var fix: TDMXFixture;
begin
 for fix in FFixtures do
   if fix.ID = aID then
   begin
     Result := fix;
     exit;
   end;

 Result := NIL;
end;

function TDmxUniverse.Fixture_WhichContainsThisAdress(aAdress: TDMXAdress ): TDMXFixture;
var fix: TDMXFixture;
begin
  for fix in FFixtures do
    if (aAdress >= fix.Adress) and (aAdress <= fix.LastAdress) then
    begin
      Result := fix;
      exit;
    end;

  Result := NIL;
end;

function TDmxUniverse.Fixture_GetByAdress(aAdress: TDMXAdress): TDmxFixture;
var fix: TDMXFixture;
begin
  for fix in FFixtures do
    if fix.Adress = aAdress then
    begin
       Result := fix;
       exit;
    end;

  Result := NIL;
end;

procedure TDmxUniverse.Fixture_DeleteByIndex(Index: integer; ShiftOtherAdress: boolean );
var i, cc: integer;
begin
 if (Index < 0) or (Index >= FFixtures.Count) then exit;
 cc := Fixtures[Index].ChannelsCount;
 FFixtures.Delete(Index);
 if ShiftOtherAdress then
   for i:=Index to FFixtures.Count-1 do
     Fixtures[i].Adress := Fixtures[i].Adress-cc;
 DoOptimizeUsedChannels;
end;

procedure TDmxUniverse.Fixture_DeleteByID(aID: cardinal; ShiftOtherAdress: boolean);
var i, j, cc:integer;
begin
  for i:=0 to FFixtures.Count-1 do
    if Fixtures[i].ID=aID then
    begin
      //Fixture_DeleteByIndex(i, ShiftOtherAdress);
      cc := Fixtures[i].ChannelsCount;
      FFixtures.Delete(i);
      if ShiftOtherAdress then
        for j:=i to FFixtures.Count-1 do
          Fixtures[j].Adress := Fixtures[j].Adress-cc;
      DoOptimizeUsedChannels;
      exit;
    end;
end;

procedure TDmxUniverse.Fixture_Extract(aFix: TDMXFixture);
begin
  FFixtures.Extract(aFix);
  DoOptimizeUsedChannels;
end;

procedure TDmxUniverse.Clear;
begin
  FFixtures.Clear;
end;

function TDmxUniverse.GetFixture(index: integer): TDMXFixture;
begin
  Result := FFixtures.Items[index];
end;

function TDmxUniverse.GetSize: integer;
begin
  Result := LastAdress-FirstAdress+1;
end;

procedure TDmxUniverse.SetOptimizeUsedChannels(AValue: boolean);
begin
  if FOptimizeUsedChannels=AValue then Exit;
  FOptimizeUsedChannels := AValue;
  DoOptimizeUsedChannels;
end;

procedure TDmxUniverse.DoOptimizeUsedChannels;
begin
  if FOptimizeUsedChannels then
    DeviceManager.Device[DevicePath.DeviceIndex].UsedChannelCount[DevicePath.PortIndex] := LastUsedAdress
  else
    DeviceManager.Device[DevicePath.DeviceIndex].UsedChannelCount[DevicePath.PortIndex] := LastAdress;
end;

constructor TDmxUniverse.Create;
begin
  FFixtures := TDMXFixtureList.Create(TRUE);
  Name := '---';
  Color := BGRA(35,20,20);
  DevicePath.InitByDefault;
  FirstAdress := 1;
  LastAdress := 512;
  SetLength(FMap, LastAdress-FirstAdress+1);
  FOptimizeUsedChannels := TRUE;
end;

destructor TDmxUniverse.Destroy;
begin
  FFixtures.Free;
  inherited Destroy;
end;


function TDmxUniverse.LastUsedAdress: TDMXAdress;
var fix: TDMXFixture;
begin
  Result := FirstAdress;
  for fix in FFixtures do
    if Result < fix.LastAdress then
      Result := fix.LastAdress;
end;

function TDmxUniverse.FirstFreeAdress(aChannelCount: integer; out aAdress: TDMXAdress): boolean;
var fix: TDMXFixture;
  A:array of boolean;
  i, c: integer;
begin
  A := NIL;
  SetLength(A, LastAdress-FirstAdress+1);
  for i:=0 to High(A) do
    A[i] := FALSE;
  for fix in FFixtures do
    for i:=fix.Adress-1 to fix.Adress-1+fix.ChannelsCount-1 do
      A[i] := TRUE;

  c:=0;
  for i:=FirstAdress to LastAdress do
    if A[i-1] then
      c:=0
    else
    begin
      if c=0 then aAdress:=i;
      inc(c);
      if c = aChannelCount then
      begin
        Result := TRUE;
        exit;
      end;
    end;

  Result := FALSE;
end;

function TDmxUniverse.ErrorInAdressing: boolean;
var fix: TDMXFixture;
  A:array of boolean;
  i: integer;
begin
  A := NIL;
  SetLength(A, LastAdress-FirstAdress+1);
  for i:=0 to High(A) do
    A[i] := FALSE;

  for fix in FFixtures do
    for i:=fix.Adress-1 to fix.Adress-1+fix.ChannelsCount-1 do
      if A[i] then
      begin
        Result := TRUE;
        exit;
      end
      else A[i] := TRUE;

  Result := FALSE;
end;

function TDmxUniverse.IsValidAdress(const aAdress: TDMXAdress): boolean;
begin
  Result := (aAdress>=FirstAdress) and (aAdress<=LastAdress);
end;

function TDmxUniverse.FixturesCount: integer;
begin
  Result := FFixtures.Count;
end;

const UNIVERSEHEAD = '[UNIVERSE ';
      UNIVERSETRAIL = ']';

procedure TDmxUniverse.SaveTo(t: TStringList; aIndex: integer);
var fix: TDMXFixture;
  dev: TBaseDMXDevice;
  prop: TPackProperty;
begin
  prop.Init('|');
  prop.Add('ID', integer(ID));
  prop.Add('Name', Name);
  prop.Add('Color', BGRAPixelToString(Color));
  prop.Add('First', FirstAdress);
  prop.Add('Last', LastAdress);
  dev := DeviceManager.Device[DevicePath.DeviceIndex];
  prop.Add('Device', dev.Name);
  prop.Add('Serial', dev.SerialNumber);
  prop.Add('Port', DevicePath.PortIndex);
  prop.Add('Optimized', BoolToStr(FOptimizeUsedChannels, 'yes', 'no'));
  prop.Add('FixtureCount', FFixtures.Count);

  t.Add(UNIVERSEHEAD+aIndex.ToString+UNIVERSETRAIL);
  t.Add(prop.PackedProperty);
  for fix in FFixtures do
    t.Add( fix.SaveToString );
end;

function TDmxUniverse.LoadFrom(t: TStringList; aIndex: integer; aInitDevice: boolean): boolean;
var vi, i, k, c: integer;
  fix: TDMXFixture;
  dev: TBaseDMXDevice;
  prop: TSplitProperty;
  s1, s2: string;
  procedure LogMissingProperty(const apropName: string);
  begin
    Log.Error('TDmxUniverse.LoadFrom - Property '+apropName+' not found', 3);
  end;

begin
  s1 := ''; // avoid hint
  s2 := '';
  vi := 0;
  c := 0;

  Result := FALSE;
  Clear;
  ShortName := 'U'+(aIndex+1).ToString;

  k := t.IndexOf(UNIVERSEHEAD+aIndex.ToString+UNIVERSETRAIL);

  if (k = -1) or (k = t.Count-1) then
  begin
    Log.Error('TDmxUniverse.LoadFrom - Section for universe '+aIndex.ToString+' not found...');
    prop.SetEmpty;
    inc(k);
  end
  else begin
    prop.Split(t.Strings[k+1], '|');
    Log.Info('loading universe'+(aIndex+1).Tostring, 2);
    inc(k, 2);
  end;

  try
    if not prop.IntegerValueOf('ID', integer(ID), 0) then
      LogMissingProperty('ID');

    if not prop.StringValueOf('Name', Name, SUniverse+aIndex.ToString) then
      LogMissingProperty('Name');

    if not prop.StringValueOf('Color', s1, '0') then
      LogMissingProperty('Color');
    Color := StringToBGRAPixel(s1);

    if not prop.IntegerValueOf('First', FirstAdress, 1) then
      LogMissingProperty('First');

    if not prop.IntegerValueOf('Last', LastAdress, 512) then
      LogMissingProperty('Last');

    if aInitDevice then
    begin
      if not prop.StringValueOf('Device', s1, NODEVICE_NAME) then
        LogMissingProperty('Device');

      if not prop.StringValueOf('Serial', s2, '') then
        LogMissingProperty('Serial');
      DevicePath.DeviceIndex:=DeviceManager.GetDeviceIndexByNameSerial(s1, s2); // device index

      if DevicePath.DeviceIndex<>INVALID_DMXDEVICE_INDEX then
      begin
        if not prop.IntegerValueOf('Port', vi, 0) then
          LogMissingProperty('Port');
        DevicePath.PortIndex := vi;

        dev := DeviceManager.GetDeviceByPath(DevicePath);
        dev.PortDirection[DevicePath.PortIndex] := pdOut;

        if not prop.StringValueOf('Optimized', s1, 'yes') then
          LogMissingProperty('Optimized');
        OptimizeUsedChannels := s1 = 'yes';
      end
      else DevicePath.PortIndex := INVALID_DMXDEVICEPORT_INDEX;
      Log.Info('connected to device "'+DevicePath.DeviceNameSerialPort+'"', 3);
    end;

    if not prop.IntegerValueOf('FixtureCount', c, 0) then
      LogMissingProperty('FixtureCount')
    else Log.Info(c.ToString+' fixture(s)', 3);
    for i:=0 to c-1 do
    begin
      fix := TDMXFixture.Create;
      fix.Universe := Self;
      if not fix.LoadFromString(t.Strings[k]) then
      begin
        Log.Error('Fail to Load the fixture from string "'+t.Strings[k]+'"', 3);
        fix.Free;
        Clear;
        exit;
      end
      else begin
        FFixtures.Add(fix);
        if TUniverseManager.FixtureIDValue < fix.ID then
          TUniverseManager.FixtureIDValue := fix.ID;
      end;
      inc(k);
    end;
    DoOptimizeUsedChannels;
    Result := TRUE;
  except
    Log.Error('Exception occured while loading universe U'+(aIndex+1).ToString, 3);
  end;
end;

procedure TDmxUniverse.Sel_None;
var fix: TDMXFixture;
begin
  for fix in FFixtures do
   fix.Selected := FALSE;
end;

procedure TDmxUniverse.Sel_All;
var fix: TDMXFixture;
begin
  for fix in FFixtures do
   fix.Selected := TRUE;
end;

function TDmxUniverse.HaveDevice: boolean;
begin
  Result := DevicePath.IsAssignedToDevice;
end;

function TDmxUniverse.IsConnected: boolean;
begin
  if HaveDevice then
    Result := DeviceManager.DevicePortIsOpen(DevicePath)
  else
    Result := FALSE;
end;

function TDmxUniverse.DeviceName: string;
begin
  Result := DeviceManager.Device[DevicePath.DeviceIndex].Name;
end;

function TDmxUniverse.DeviceSerialNumber: string;
begin
  Result := DeviceManager.Device[DevicePath.DeviceIndex].SerialNumber;
end;

function TDmxUniverse.DeviceNameSerialPort: string;
var dev: TBaseDMXDevice;
begin
  dev := DeviceManager.Device[DevicePath.DeviceIndex];
  Result := dev.Name+' - '+dev.SerialNumber;
  if dev.PortCount > 1 then
    Result := Result+' - '+SDevicePort+' '+DevicePath.PortIndex.ToString;
end;

{function TDmxUniverse.DisconnectDevice: boolean;
begin
  Result:=DeviceManager.Device[DevicePath.DeviceIndex].Close(DevicePath.PortIndex);
end;     }

{function TDmxUniverse.ConnectDevice: boolean;
begin
 Result:=DeviceManager.Device[DevicePath.DeviceIndex].Open(DevicePath.PortIndex);
end;  }

function TDmxUniverse.UsedChannelCount: integer;
begin
  Result := DeviceManager.Device[DevicePath.DeviceIndex].UsedChannelCount[DevicePath.PortIndex];
end;

function TDmxUniverse.DevicePortCanChangeItsDirection: boolean;
begin
  Result := DeviceManager.Device[DevicePath.DeviceIndex].PortDirectionCanChange[DevicePath.PortIndex];
end;

function TDmxUniverse.DevicePortDirection: TPortDirection;
begin
  Result := DeviceManager.Device[DevicePath.DeviceIndex].PortDirection[DevicePath.PortIndex];
end;

procedure TDmxUniverse.SendToDevice(aAdress: TDMXAdress; aValue: byte);
var dev: TBaseDMXDevice;
begin
  dev := DeviceManager.Device[DevicePath.DeviceIndex];
  dev.UpdateChannel(DevicePath.PortIndex, aAdress, aValue);
end;

procedure TDmxUniverse.Update(const aElapsedTime: single);
var fix: TDMXFixture;
  dev: TBaseDMXDevice;
begin
  dev := DeviceManager.Device[DevicePath.DeviceIndex];
  for fix in FFixtures do
   fix.Update(aElapsedTime);
  dev.SendAll(DevicePath.PortIndex);
end;




//-------------------------------------------------------------------------------------
//
//                              TUniverseManager
//
//


{ TUniverseManager }

{function TUniverseManager.Duplicate: TUniverseManager;
begin
 Result := Self;
end;  }

function TUniverseManager.GetUniverseByID(aID: cardinal): TDMXUniverse;
var uni: TDMXUniverse;
begin
  Result := NIL;
  for uni in FUniverses do
   if uni.ID = aID then
   begin
      Result := uni;
      exit;
   end;
end;

function TUniverseManager.UniverseIDToUniverseIndex(aID: cardinal): integer;
var i: integer;
begin
  Result := -1;
  for i:=0 to Count-1 do
   if Universes[i].ID = aID then
   begin
     Result := i;
     exit;
   end;
end;

function TUniverseManager.Add(const aName: string): TDmxUniverse;
begin
 Result := TDmxUniverse.Create;
 Result.ID := NextUniverseIDValue;
 Result.Name := aName;
 Result.ShortName := 'U'+FUniverses.Add(Result).ToString;
end;

procedure TUniverseManager.Delete(aIndex: integer);
var i: integer;
begin
  if not ValidIndex(aIndex) then exit;

  FUniverses.Delete(aIndex);

  for i:=0 to FUniverses.Count-1 do
    FUniverses.Items[i].ShortName := 'U'+(i+1).ToString;

  if FUniverses.Count=0 then
  begin
    ResetUniverseIDValue;
    ResetFixtureIDValue;
  end;
end;

function TUniverseManager.ValidIndex(aIndex: integer): boolean;
begin
  Result := (aIndex >= 0) and (aIndex < FUniverses.Count);
end;

function TUniverseManager.GetUniverse(index: integer): TDmxUniverse;
begin
  Result := FUniverses.Items[index];
end;

procedure TUniverseManager.StartThread;
begin
  if FThreadAction = NIL then
    FThreadAction := TTimedThread.CreateQueue(DMX_REFRESH_PERIOD_MS, @Update, TRUE);
    //FThreadAction := TTimedThread.CreateSynchronize(DMX_REFRESH_PERIOD_MS, @Update, TRUE);
end;

procedure TUniverseManager.StopThread;
begin
  if FThreadAction <> NIL then
  begin
    FThreadAction.Terminate;
    FThreadAction.WaitFor;
    FThreadAction.Free;
    FThreadAction := NIL;
  end;
end;

constructor TUniverseManager.Create;
begin
  FUniverses := TDMXUniverseList.Create(TRUE);
  FTimeOrigin := GetTickCount64;
  StartThread;
  Log.Info('Universe Manager created');
end;

destructor TUniverseManager.Destroy;
begin
  Log.Info('Destroying Universe Manager');
  FUniverses.Free;
  inherited Destroy;
end;

function TUniverseManager.RetrieveFixture(aIDUniverse, aIDFixture: cardinal;
  out uni: TDMXUniverse; out fix: TDMXFixture): boolean;
begin
  Result := FALSE;
  uni := UniverseManager.GetUniverseByID(aIDUniverse);
  if uni = NIL then exit;
  fix := uni.Fixture_GetByID(aIDFixture);
  if fix = NIL then exit;
  Result := TRUE;
end;

function TUniverseManager.RetrieveChannel(aIDUniverse, aIDFixture: cardinal;
  aChanIndex: integer; out uni: TDMXUniverse; out fix: TDMXFixture; out
  chan: TDMXChannel): boolean;
begin
  Result := FALSE;
  if RetrieveFixture(aIDUniverse, aIDFixture, uni, fix) then
  begin
    chan := fix.GetChannelByIndex(aChanIndex);
    if chan <> NIL then
      Result := TRUE;
  end;
end;

procedure TUniverseManager.Clear;
begin
  FUniverses.Clear;
  ResetUniverseIDValue;
  ResetFixtureIDValue;
end;

function TUniverseManager.IndexOf(aUniverse: TDMXUniverse): integer;
begin
  Result := FUniverses.IndexOf(aUniverse);
end;

class procedure TUniverseManager.ResetUniverseIDValue;
begin
  UniverseIDValue := 0;
end;

class function TUniverseManager.NextUniverseIDValue: cardinal;
begin
  inc(UniverseIDValue);
  Result := UniverseIDValue;
end;

class procedure TUniverseManager.ResetFixtureIDValue;
begin
  FixtureIDValue := 0;
end;

class function TUniverseManager.NextFixtureIDValue: cardinal;
begin
  inc(FixtureIDValue);
  Result := FixtureIDValue;
end;

function TUniverseManager.GetTotalFixtureCount: integer;
var uni: TDMXUniverse;
begin
 Result := 0;
 for uni in FUniverses do
  Result += uni.FixturesCount;
end;

function TUniverseManager.GetCount: integer;
begin
  Result := FUniverses.Count;
end;

const UNIVERSE_HEADER='[UNIVERSE]';
function TUniverseManager.Save: boolean;
var i: integer;
  prop: TPackProperty;
  f: string;
  t: TStringList;
begin
  if Count = 0 then exit(True);

  t := TStringList.Create;
  try
    try
      prop.Init('|');
      prop.Add('CurrentUniverseID', integer(UniverseIDValue));
      prop.Add('CurrentFixtureID', integer(FixtureIDValue));
      prop.Add('Count', Count);

      t.Add(UNIVERSE_HEADER);
      t.Add(prop.PackedProperty);
      for i:=0 to FUniverses.Count-1 do
        FUniverses[i].SaveTo(t, i);

      f := ConcatPaths([Project.GetFolderCommonData, COMMON_PROJECT_DMX_FILENAME]);
      t.SaveToFile(f);
      Result := True;
    except
      Log.Error('TUniverseManager.Save - Failed to save DMX universes to file "'+f+'"', 2);
      Result := False;
    end;
  finally
    t.Free;
  end;
end;

procedure TUniverseManager.SaveTo(t: TStringList);
var i: integer;
  prop: TPackProperty;
begin
  if Count = 0 then exit;

  prop.Init('|');
  prop.Add('CurrentUniverseID', integer(UniverseIDValue));
  prop.Add('CurrentFixtureID', integer(FixtureIDValue));
  prop.Add('Count', Count);

  t.Add(UNIVERSE_HEADER);
  t.Add(prop.PackedProperty);
  for i:=0 to FUniverses.Count-1 do
    FUniverses[i].SaveTo(t, i);

{
  t.Add(UNIVERSECOUNTHEADER);
  t.Add(Count.ToString);

  for i:=0 to FUniverses.Count-1 do
  begin
    FUniverses[i].SaveTo(t, i);
  end;  }
end;

function TUniverseManager.SameDMXConfiguration(const aProjectFilename: string): boolean;
var t: TStringList;
  i, j: integer;
  temp: TUniverseManager;
  curUni, tempUni: TDMXUniverse;
  curFix, tempFix: TDMXFixture;
//  prop: TSplitProperty;
begin
  Result := FALSE;
  t := TStringList.Create;
  temp := TUniverseManager.Create;
  try
    try
      t.LoadFromFile(aProjectFilename);
      temp.LoadFrom(t, FALSE);
      Result := temp.Count=Count;   // same universes count ?
      if Result then
      begin
        for i:=0 to Count-1 do
        begin
          curUni := Universes[i];
          tempUni := temp.Universes[i];
          Result := Result and (curUni.FixturesCount = tempUni.FixturesCount); // same fixtures count ?
          if Result then
          begin
            for j:=0 to curUni.FixturesCount-1 do
            begin
              curFix := curUni.Fixtures[j];
              tempFix := tempUni.Fixtures[j];
              Result := Result and                // same fixture adress and ID ?
                       (curFix.Adress = tempFix.Adress) and
                       (curFix.ID = tempFix.ID);
            end;
          end;
        end;
      end;
    except
      Result := FALSE;
    end;

    if Result then
    begin
      // copy the name
      for i:=0 to Count-1 do
      begin
        Universes[i].Name := temp.Universes[i].Name; // keep the new universe name
        for j:=0 to curUni.FixturesCount-1 do
          curUni.Fixtures[j].Description := tempUni.Fixtures[j].Description; // keep the new fixture description
      end;
    end;

  finally
    t.Free;
    temp.StopThread;
    temp.Free;
  end;
end;

function TUniverseManager.Load(aInitDevice: boolean): boolean;
var t: TStringList;
  f: string;
  procedure LogMissingProperty(const apropName: string);
  begin
    Log.Error('TUniverseManager.Load - property '+apropName+' not found', 3);
  end;
begin
  Log.Info('Loading universe', 2);

  Clear;
  Result := False;
  f := ConcatPaths([Project.GetFolderCommonData, COMMON_PROJECT_DMX_FILENAME]);
  if not FileExists(f) then exit(True);

  t := TStringList.Create;
  try
    try
      t.LoadFromFile(f);
      Result := LoadFrom(t, aInitDevice);
    except
      Log.Error('TUniverseManager.Load - Failure when loading '+f, 3);
    end;
  finally
    t.Free;
  end;
end;

function TUniverseManager.LoadFrom(t: TStringList; aInitDevice: boolean): boolean;
var i, k, c, vi: integer;
  uni: TDMXUniverse;
  prop: TSplitProperty;
  procedure LogMissingProperty(const apropName: string);
  begin
    Log.Error('TUniverseManager.LoadFrom - property '+apropName+' not found', 3);
  end;

begin
  c := 0;
  vi := 0;

  Clear;
  k := t.IndexOf(UNIVERSE_HEADER);
  if k = -1 then
  begin
    Result := TRUE;
    exit;
  end;

  if k = t.Count-1 then
  begin
    log.Error('TUniverseManager.LoadFrom - Line with universe data is missing', 3);
    Result := False;
    exit;
  end;

  prop.Split(t.Strings[k+1], '|');
  if not prop.IntegerValueOf('CurrentUniverseID', vi, UniverseIDValue) then
    LogMissingProperty('CurrentUniverseID');
  UniverseIDValue := vi;

  if not prop.IntegerValueOf('CurrentFixtureID', vi, FixtureIDValue) then
    LogMissingProperty('CurrentFixtureID');
  FixtureIDValue := vi;

  if not prop.IntegerValueOf('Count', c, 0) then
    LogMissingProperty('Count');

  for i:=0 to c-1 do
  begin
    uni := TDMXUniverse.Create;
    FUniverses.Add(uni);
    if not uni.LoadFrom(t, i, aInitDevice) then
    begin
      Clear;
      exit;
    end;

    if UniverseIDValue < uni.ID then
      UniverseIDValue := uni.ID;
  end;
  Result := TRUE;
end;

function TUniverseManager.LoadFromProject(const aProjectFilename: string): boolean;
var t: TStringList;
  f: string;
begin
  Result := FALSE;

  f := ExtractFilePath(aProjectFilename);
  f := ConcatPaths([f, COMMON_PROJECT_FOLDER_NAME, COMMON_PROJECT_DMX_FILENAME]);
  if not FileExists(f) then exit;

  t := TStringList.Create;
  try
    try
      t.LoadFromFile(f);
      LoadFrom(t);
      Result := TRUE;
    except
      Log.Error('TUniverseManager.LoadFromProject - An exception occurs');
      Log.Error('loading from project "'+aProjectFilename+'"', 2);
      Log.error('DMX file "'+f+'"', 2);
    end;
  finally
    t.Free;
  end;
end;

procedure TUniverseManager.Fixture_DeleteByID(aID: cardinal; ShiftOtherAdress: boolean);
var uni: TDMXUniverse;
begin
  for uni in FUniverses do
    uni.Fixture_DeleteByID(aID, ShiftOtherAdress);
end;

procedure TUniverseManager.Sel_None;
var uni: TDMXUniverse;
begin
  for uni in FUniverses do
   uni.Sel_None;
end;

procedure TUniverseManager.Sel_All;
var uni: TDMXUniverse;
begin
  for uni in FUniverses do
   uni.Sel_All;
end;

procedure TUniverseManager.GetSelectedFixtures(var A: ArrayOfDmxFixtures);
var i, k: integer;
  uni: TDMXUniverse;
begin
  SetLength(A, 0);
  k := 0;
  for uni in FUniverses do
   for i:=0 to uni.FixturesCount-1 do
     if uni.Fixtures[i].Selected then
     begin
        SetLength(A, k+1);
        A[k] := uni.Fixtures[i];
        inc(k);
     end;
end;

procedure TUniverseManager.BlackOut;
var uni: TDMXUniverse;
  fix: TDMXFixture;
begin
 for uni in FUniverses do
  for fix in uni.FFixtures do
  begin
    fix.CurrentFixtureEffect := deNOEFFECT;
    fix.SetAllChannelsToZero;

  end;
end;

procedure TUniverseManager.Update;
var elapsedSec: single;
    timeNow: QWORD;
    uni: TDMXUniverse;
    flagredraw: boolean;
begin
  timeNow := GetTickCount64;
  elapsedSec := (timeNow-FTimeOrigin)*0.001;
  FTimeOrigin := timeNow;
  flagredraw := FALSE;
  for uni in FUniverses do
  begin
    uni.Update(elapsedSec);
    flagredraw := flagredraw or uni.NeedToBeRedraw;
    uni.NeedToBeRedraw := FALSE;
  end;

  // query refresh projectors
  if flagredraw and FProjectorViewToRefreshForThreadUniverse.ChannelsLevelAreVisible then
      FProjectorViewToRefreshForThreadUniverse.Redraw;
end;


end.

