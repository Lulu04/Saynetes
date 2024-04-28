unit u_dmx_util;

{$mode objfpc}{$H+}
{$modeswitch TypeHelpers}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, ComCtrls, ExtCtrls, Forms, Graphics, LazUTF8,
  u_common, u_list_dmxuniverse, u_resource_string, BGRABitmap, BGRABitmapTypes;


  //------------------------------
  //  BIBLIOTHEQUE DMX
  //------------------------------
type

  // fixture records to manage dmx library
  TLibraryFixtureChannel=record
    Name: string;
    ChannelType: TChannelType;
    Ranges: array of TChannelRange;
  end;

  ArrayOfLibraryFixtureChannel=array of TLibraryFixtureChannel;


  { TLibraryFixture }

  TLibraryFixture=record
    FixtureType: TFixtureType;
    Power: integer;
    Name: string;
    Channels: ArrayOfLibraryFixtureChannel;
    DipSwitch: TDipSwitch;
    function LoadFromFile( const aFileName: string ): boolean;
    function SaveToFile( const aFileName: string ): boolean;
  end;

  function PathRelativeToDMXLibrary(const aFullFileName: string): string;
  // DirectorySeparator is : under Unix '/'
  //                         under Windows '\'
  // So, in case of the project was made from an OS and loaded under another OS
  // we need to replace this separator by the right one
  function AdjustDirectorySeparator(const aPath: string): string;


  // stage and seats
  function StageSvgFileFor(aStageType: TStageType): string;
  function SeatSvgFileFor(aSeatType: TSeatType): string;

  // gives an svg filename according the TFixtureType passed as parameter
  function FixtureSVGFileFor(aFT:TFixtureType): string;
  function FixtureNameFor(aFT:TFixtureType): string;
  function FixtureTypeToBGRA(aFT: TFixtureType): TBGRABitmap;

  // fill a TImage with an image according an TFixtureType passed as parameter
  procedure ShowFixtureImage( aImage:TImage; aFT:TFixtureType );

  function DMXCursorImageFileNameFor(aCT: TChanneltype): string;

  function EffectToText(aFX: TDmxEffect): string;
  function IsChannelEffect(aFX: TDmxEffect): boolean;

  // decode a dmx channel range 'begin-->end : description'
  // and return the 3 values
  procedure DecodeDMXChannelRange( const s: string; out be, en: integer; out description: string );
  function EncodeDMXChannelRange( be, en: integer; const description: string ): string;


  function GetFixtureFromCmd(const aCmd: TSingleCmd): TDMXFixture;

type

  { TChannelPathHelper }

  TChannelPathHelper=type helper for TChannelPath
    procedure InitFromChannel(aChan: TDMXChannel);
    function SaveToString: string;
    procedure LoadFromString(const s: string);
  end;

  { TFixturePathHelper }

  TFixturePathHelper=type helper for TFixturePath
    procedure InitFromFixture(aFix: TDMXFixture);
    function SaveToString: string;
    procedure LoadFromString(const s: string);
  end;

  { TDevicePathHelper }

  TDevicePathHelper=type helper for TDevicePath
    procedure InitByDefault;
    function Path(aDeviceIndex, aPortIndex: integer): TDevicePath;
    function DeviceName: string;
    function SerialNumber: string;
    function DeviceNameSerialPort: string;
    function IsAssignedToDevice: boolean;
  end;

implementation
uses LazFileUtils, u_helper, u_apputils, u_dmxdevice_manager, utilitaire_bgrabitmap;


function PathRelativeToDMXLibrary(const aFullFileName: string): string;
begin
  Result:=ExtractRelativePath(GetAppDMXLibraryFolder, aFullFileName);
end;

function AdjustDirectorySeparator(const aPath: string): string;
var p: integer;
begin
  Result:=aPath;
  case DIRECTORYSEPARATOR of
    '/': begin
      // we are under Unix
      repeat
       p:=Pos('\', Result);
       if p>0 then Result[p]:=DIRECTORYSEPARATOR;
      until p=0;
    end;
    '\': begin
      // we are under Windows
      repeat
       p:=Pos('/', Result);
       if p>0 then Result[p]:=DIRECTORYSEPARATOR;
      until p=0;
    end;
  end;
end;

function StageSvgFileFor(aStageType: TStageType): string;
begin
  Result := GetAppStageImagesFolder;
  case aStageType of
    stNone: Result:='';
    stRectangle: Result+='StageRectangle.svg';
    stSquare: Result+='StageSquare.svg';
    stHalfCircle: Result+='StageHalfCircle.svg';
    stEllipse: Result+='StageEllipse.svg';
    stCustom1: Result+='StageCustom1.svg';
    else Result:='';
  end;
end;

function SeatSvgFileFor(aSeatType: TSeatType): string;
begin
  Result := GetAppStageImagesFolder;
  case aSeatType of
    seatType1: Result+='Seats1.svg';
    seatType2: Result+='Seats2.svg';
    else Result:='';
  end;
end;

function FixtureSVGFileFor(aFT:TFixtureType): string;
begin
 Result := GetAppFixtureImagesFolder;
 case aFT of
  ftOther: Result += '00_Other.svg';
  ftPlanConvex: Result += '01_PC.svg';
  ftParShortBulb: Result += '02_ParShortBulb.svg';
  ftHalogen: Result += '03_Halogen.svg';
  ftParLongTransparentLed: Result += '04_ParLongTransparentLed.svg';
  ftBarColoredLed: Result += '05_BarColoredLed.svg';
  ftProfile: Result += '06_Profile.svg';
  ftColorChanger: Result += '07_ColorChanger.svg';
  ftScanner: Result += '08_Scanner.svg';
  ftMovingHead: Result += '09_MovingHead.svg';
  ftSmokeMachine: Result += '10_SmokeMachine.svg';
  ftBubbleMachine: Result += '11_BubbleMachine.svg';
  ftDimmer1Channel: Result+='12_Dimmer1Channel.svg';
  ftDimmer4Channels: Result+='13_Dimmer4Channels.svg';
  ftMatrixTransparentLed: Result+='14_MatrixTransparentLed.svg';
  ftMatrixWithColoredLed: Result+='15_MatrixColoredLed.svg';
  ftParShortTransparentLed: Result+='16_ParShortTransparentLed.svg';
  ftLedParLongWithColoredLed: Result+='17_ParLongColoredLed.svg';
  ftLedParShortWithColoredLed: Result+='18_ParShortColoredLed.svg';
  ftLedBarTransparentLed: Result+='19_BarTransparentLed.svg';
  ftParLongBulb: Result+='20_ParLongBulb.svg';
  else Raise exception.Create('forgot to implement!');
 end;//case
end;

function FixtureNameFor(aFT: TFixtureType): string;
begin
  case aFT of
   ftOther: Result := SOther;
   ftPlanConvex: Result := SPlanConvex;
   ftParShortBulb: Result := SParShortBulb;
   ftHalogen: Result := SHalogen;
   ftParLongTransparentLed: Result := SParLongTransparentLed;
   ftBarColoredLed: Result := SBarColoredLed;
   ftProfile: Result := SProfile;
   ftColorChanger: Result := SColorChanger;
   ftScanner: Result := SScanner;
   ftMovingHead: Result := SMovingHead;
   ftSmokeMachine: Result := SSmokeMachine;
   ftBubbleMachine: Result := SBubbleMachine;
   ftDimmer1Channel: Result := SDimmer1Channel;
   ftDimmer4Channels: Result := SDimmer4Channels;
   ftMatrixTransparentLed: Result := SMatrixTransparentLed;
   ftMatrixWithColoredLed: Result := SMatrixColoredLed;
   ftParShortTransparentLed: Result := SParShortTransparentLed;
   ftLedParLongWithColoredLed: Result := SLedParLongColoredLed;
   ftLedParShortWithColoredLed: Result := SLedParShortColoredLed;
   ftLedBarTransparentLed: Result := SLedBarTransparentLed;
   ftParLongBulb: Result := SParLongBulb;
   else Raise exception.Create('forgot to implement!');
  end;//case
end;

function FixtureTypeToBGRA(aFT: TFixtureType): TBGRABitmap;
begin
  Result := SVGFileToBGRABitmap(FixtureSVGFileFor(aFT), -1, -1);
end;

procedure ShowFixtureImage(aImage: TImage; aFT:TFixtureType);
var ima: TBGRABitmap;
    bmp: TBitmap;
     function LoadBitmapFromFile(AFileName: String): TCustomBitmap;
     var
       Stream: TStream;
       GraphicClass: TGraphicClass;
     begin
       Result := nil;
       Stream := nil;
       try
         Stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
         GraphicClass := GetGraphicClassForFileExtension(ExtractFileExt(AFileName));
         if (GraphicClass <> nil) and (GraphicClass.InheritsFrom(TCustomBitmap)) then
         begin
           Result := TCustomBitmap(GraphicClass.Create);
           Result.LoadFromStream(Stream);
         end;
       finally
         Stream.Free;
       end;
     end;
begin
  ima := SVGFileToBGRABitmap(FixtureSVGFileFor(aFT), aImage.ClientWidth, aImage.ClientHeight);
  bmp := TBitmap.Create;
  ima.AssignToBitmap(bmp);
  aImage.Picture.Assign(bmp);
  ima.Free;
  bmp.Free;
  aImage.Tag := ord(aFT);
end;

function DMXCursorImageFileNameFor(aCT: TChanneltype): string;
begin
  Result := GetAppCursorImagesFolder;
  case aCT of
    ctCONFIG: Result:=Result+'CursorConfig.svg';
    ctMASTERDIMMER: Result:=Result+'CursorMasterDimmer.svg';
    ctDIMMER: Result:=Result+'CursorDefault.svg';
    ctRED: Result:=Result+'CursorRed.svg';
    ctGREEN: Result:=Result+'CursorGreen.svg';
    ctBLUE: Result:=Result+'CursorBlue.svg';
    ctSTROBE: Result:=Result+'CursorStrobe.svg';
    ctPAN: Result:=Result+'CursorPan.svg';
    ctTILT: Result:=Result+'CursorTilt.svg';
    ctPANTILTSPEED: Result:=Result+'CursorSpeedPanTilt.svg';
    ctGOBO: Result:=Result+'CursorGobo.svg';
    ctGOBOROTATION: Result:=Result+'CursorGoboRotation.svg';
    ctCOLORCHOICE: Result:=Result+'CursorColorChanger.svg';
    ctWHITE: Result:=Result+'CursorWhite.svg';
    ctAMBER: Result:=Result+'CursorAmber.svg';
    ctUV: Result:=Result+'CursorUV.svg';
    ctSPEED: Result:=Result+'CursorSpeedPanTilt.svg';
    ctNOFUNCTION: Result:=Result+'CursorNoFunction.svg';
    ctCYAN: Result:=Result+'CursorCyan.svg';
    ctMAGENTA: Result:=Result+'CursorMagenta.svg';
    ctYELLOW: Result:=Result+'CursorYellow.svg';
    ctLIME: Result:=Result+'CursorLime.svg';
    ctINDIGO: Result:=Result+'CursorIndigo.svg';
    ctWARMWHITE: Result:=Result+'CursorWarmWhite.svg';
    ctCOLDWHITE: Result:=Result+'CursorColdWhite.svg';
    ctIRIS: Result:=Result+'CursorIris.svg';
    ctBLADEINSERTION: Result:=Result+'CursorBladeInsertion.svg';
    ctCOLORTEMPERATURE: Result:=Result+'CursorColorTemperature.svg';
    CTSTROBESPEED: Result:=Result+'CursorStrobeSpeed.svg';
    ctSOUNDSENSITIVITY: Result:=Result+'CursorSoundSensitivity.svg';
    ctBLADEROTATION: Result:=Result+'CursorBladeRotation.svg';
    ctZOOM: Result:=Result+'CursorZoom.svg';
    ctFOCUS: Result:=Result+'CursorFocus.svg';
    else Result:='';
  end;//case
end;

function EffectToText(aFX: TDmxEffect): string;
begin
  case aFX of
    deNOEFFECT: Result:='';
    deDimmer: Result:=SDimmer;
    deFlame: Result:=SFlame;
    deAudioFollower: Result:=SAudioFollow;
    deCopy: Result:=SCopy;
    deFlameRGB: Result:=SFlameRGB;
    deAudioFollowerRGB: Result:=SAudioFollow;
    deCopyRGB: Result:=SCopyRGB;
    deFlash: Result:=SFlash;
    else Result:=SUnknown;
  end;
end;

function IsChannelEffect(aFX: TDmxEffect): boolean;
begin
  case aFX of
    deDimmer,
    deFlame,
    deAudioFollower,
    deCopy,
    deFlash: Result:=TRUE;
    else Result:=FALSE;
  end;
end;

// decode a dmx channel range 'begin-->end : description'
procedure DecodeDMXChannelRange(const s: string; out be, en: integer; out description: string);
var i, j: integer;
begin
  i:=Pos('-', s)-1;    // 1 based 0 if not found
  be:=Copy(s,1,i).ToInteger;       // 1 based
  inc(i,4);
  j:=Pos(' ', s);
  en:=Copy(s,i,j-i).ToInteger;
  inc(j,3);
  if j<Length(s)
    then description:=Copy(s,j,Length(s)-j+1)
    else description:='';
end;

function EncodeDMXChannelRange(be, en: integer; const description: string ): string;
begin
  Result:=be.ToString+'-->'+en.ToString+' : '+description;
end;

function GetFixtureFromCmd(const aCmd: TSingleCmd): TDMXFixture;
var A: TParamArray;
  uni: TDMXUniverse;

begin
  A:=aCmd.SplitToParamArray;
  case A[0].ToInteger of
    CMD_DMX_DIMMER,
    CMD_DMX_FLAME,
    CMD_DMX_STOPEFFECT,
    CMD_DMX_AUDIOFOLLOWER,
    CMD_DMX_DIMMERRGB,
    CMD_DMX_FLAMERGB,
    CMD_DMX_AUDIOFOLLOWERRGB,
    CMD_DMX_STOPEFFECTRGB,
    TITLECMD_DMX_COPYCHANNEL,
    TITLECMD_DMX_COPYRGB,
    CMD_INTERNALDMXWAVE: begin
      UniverseManager.RetrieveFixture(A[1].ToInteger, A[2].ToInteger, uni, Result);
    end;

    CMD_DMX_COPYCHANNEL:
      UniverseManager.RetrieveFixture(A[4].ToInteger, A[5].ToInteger, uni, Result);

    CMD_DMX_COPYRGB:
      UniverseManager.RetrieveFixture(A[3].ToInteger, A[4].ToInteger, uni, Result);

    else Result:=NIL;
  end;//case
end;

{ TDevicePathHelper }

procedure TDevicePathHelper.InitByDefault;
begin
  DeviceIndex := INVALID_DMXDEVICE_INDEX;
  Portindex := 0;
end;

function TDevicePathHelper.Path(aDeviceIndex, aPortIndex: integer): TDevicePath;
begin
  Result.DeviceIndex:=aDeviceIndex;
  result.PortIndex:=aPortindex;
end;

function TDevicePathHelper.DeviceName: string;
begin
  Result:=DeviceManager.Device[DeviceIndex].Name;
end;

function TDevicePathHelper.SerialNumber: string;
begin
  Result:=DeviceManager.Device[DeviceIndex].SerialNumber;
end;

function TDevicePathHelper.DeviceNameSerialPort: string;
var dev: TBaseDMXDevice;
begin
  dev := DeviceManager.Device[DeviceIndex];
  Result := dev.Name+' - '+dev.SerialNumber;
  if dev.PortCount > 1 then
    Result := Result+' - '+SDevicePort+' '+PortIndex.ToString;
end;

function TDevicePathHelper.IsAssignedToDevice: boolean;
begin
  Result:=(DeviceIndex<>INVALID_DMXDEVICE_INDEX) or
          (PortIndex<>INVALID_DMXDEVICEPORT_INDEX);
end;

{ TFixturePathHelper }

procedure TFixturePathHelper.InitFromFixture(aFix: TDMXFixture);
begin
  IDUni:=aFix.Universe.ID;
  IDFix:=aFix.ID;
end;

function TFixturePathHelper.SaveToString: string;
begin
  Result:=IDUni.ToString+PARAM_SEPARATOR+
          IDFix.ToString;
end;

procedure TFixturePathHelper.LoadFromString(const s: string);
var A: TParamArray;
begin
  A:=s.SplitToParamArray;
  IDUni:=A[0].ToInteger;
  IDFix:=A[1].ToInteger;
end;

{ TChannelPathHelper }

procedure TChannelPathHelper.InitFromChannel(aChan: TDMXChannel);
begin
  IDUni:=aChan.Universe.ID;
  IDFix:=aChan.Fixture.ID;
  ChanIndex:=aChan.Index;
end;

function TChannelPathHelper.SaveToString: string;
begin
  Result:=IDUni.ToString+PARAM_SEPARATOR+
          IDFix.ToString+PARAM_SEPARATOR+
          ChanIndex.ToString;
end;

procedure TChannelPathHelper.LoadFromString(const s: string);
var A: TParamArray;
begin
  A:=s.SplitToParamArray;
  IDUni:=A[0].ToInteger;
  IDFix:=A[1].ToInteger;
  ChanIndex:=A[2].ToInteger;
end;

{ TLibraryFixture }

function TLibraryFixture.LoadFromFile(const aFileName: string): boolean;
var t: TStringList;
    i, j, k, ft: integer;
begin
  DipSwitch.InitByDefault;
  t:=TStringList.Create;
  try
    t.LoadFromFile(aFileName);
    if not TryStrToInt( t.Strings[0], ft )   // fixture type
      then ft:=ord(ftOther);
    FixtureType:=TFixtureType(ft);
    Power:=t.Strings[1].ToInteger; // power
    Name:=t.Strings[2];            // name
    SetLength(Channels, t.Strings[3].ToInteger);   // channel count
    k:=4;
    for i:=0 to High(Channels) do begin
      Channels[i].ChannelType:=TChannelType(t.Strings[k].ToInteger); // channel type
      Channels[i].Name:=t.Strings[k+1];  // channel name
      SetLength(Channels[i].Ranges, t.Strings[k+2].ToInteger); // range count
      inc(k,3);
      for j:=0 to High(Channels[i].Ranges) do begin
        Channels[i].Ranges[j].Decode(t.Strings[k]);
        inc(k);
      end;
    end;
    DipSwitch.LoadFrom(t);
    Result:=TRUE;
  except
    SetLength(Channels, 1);
    Channels[0].ChannelType:=ctDimmer; // channel type
    Channels[0].Name:=SUnknown;  // channel name
    SetLength(Channels[0].Ranges, 1); // range count
    with Channels[0].Ranges[0] do begin
      BeginValue:=0;
      EndValue:=255;
      Text:=SUnknown;
      Symbol:=0;
    end;
    Result:=FALSE;
  end;
  t.Free;
end;

function TLibraryFixture.SaveToFile(const aFileName: string): boolean;
var t: TStringList;
    i, j: integer;
begin
  t:=TStringList.Create;
  t.Add(Ord(FixtureType).ToString);
  t.Add(Power.ToString);
  t.Add(Name);
  t.Add(Length(Channels).ToString);
  for i:=0 to High(Channels) do begin
    t.Add(Ord(Channels[i].ChannelType).ToString);
    t.Add(Channels[i].Name);
    t.Add(Length(Channels[i].Ranges).ToString);
    for j:=0 to High(Channels[i].Ranges) do
      t.Add(Channels[i].Ranges[j].Encode);
  end;
  DipSwitch.SaveTo(t);
  try
    t.SaveToFile(aFileName);
    Result:=TRUE;
  except
    Result:=FALSE;
  end;
  t.Free;
end;

end.

