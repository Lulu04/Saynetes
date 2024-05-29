unit u_dmx_util;

{$mode objfpc}{$H+}
{$modeswitch TypeHelpers}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, ComCtrls, ExtCtrls, Forms, Graphics, LazUTF8,
  u_common, u_list_dmxuniverse, u_resource_string, BGRABitmap, BGRABitmapTypes;


  //------------------------------
  //  DMX LIBRARY
  //------------------------------

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
  function FixtureSVGFileFor(aFT: TFixtureType): string;
  function FixtureNameFor(aFT: TFixtureType): string;
  function FixtureTypeToBGRA(aFT: TFixtureType): TBGRABitmap;
  procedure LoadFixtureImages;
  procedure FreeFixtureImages;
  procedure LoadCursorImages(aWidth, aHeight: integer);
  procedure FreeCursorImages;

  // fill a TImage with an image according an TFixtureType passed as parameter
  procedure ShowFixtureImage(aImage: TImage; aFT: TFixtureType);
  procedure ShowFixtureImage(aImage: TImage; const aFixtureLocation: TFixtureLibraryLocation);

  function DMXCursorImageFileNameFor(aCT: TChanneltype): string;

  function EffectToText(aFX: TDmxEffect): string;
  function IsChannelEffect(aFX: TDmxEffect): boolean;

  function GetFixtureFromCmd(const aCmd: TSingleCmd): TDMXFixture;


  function GetFixtureModeNames(const aFixtureFilename: string): TStringArray;

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
uses LazFileUtils, u_helper, u_apputils, u_logfile,
  u_dmxdevice_manager, utilitaire_bgrabitmap;


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
  ftFan: Result+='21_Fan.svg';
  ftLaser: Result+='22_Laser.svg';
  ftParSmallTransparentLed: Result+='23_ParSmallTransparentLed.svg';
  ftFlower01: Result+='24_Flower01.svg';
  ftParSquareSingleTransparentLed: Result+='25_ParSquareSingleTransparentLed.svg';
  ftParSquareMultipleTransparentLed: Result+='26_ParSquareMultipleTransparentLed.svg';
  ftParSquareMultipleColoredLed: Result+='27_ParSquareMultipleColoredLed.svg';
  ftParRectangularMultipleTransparentLed: Result+='28_ParRectangularMultipleTransparentLed.svg';
  ftParRectangularMultipleColoredLed: Result+='29_ParRectangularMultipleColoredLed.svg';
  ftBarShortTransparentLed: Result+='30_BarShortTransparentLed.svg';
  ftBarShortColoredLed: Result+='31_BarShortColoredLed.svg';
  ftFlower02: Result+='32_Flower02.svg';
  ftFlower03: Result+='33_Flower03.svg';
  ftStand01: Result+='34_Stand01.svg';
  ftStand02: Result+='35_Stand02.svg';
  ftStand03: Result+='36_Stand03.svg';
  ftBarShortx2TransparentLed: Result+='37_BarShortx2TransparentLed.svg';
  ftBarShortx2ColoredLed: Result+='38_BarShortx2ColoredLed.svg';
  ftBarx2TransparentLed: Result+='39_Barx2TransparentLed.svg';
  ftBarx2ColoredLed: Result+='40_Barx2ColoredLed.svg';
  ftMovingHead02: Result+='41_MovingHead02.svg';
  ftMovingHead03: Result+='42_MovingHead03.svg';
  ftBarMediumTransparentLed: Result+='43_BarMediumTransparentLed.svg';
  ftBarx2MediumTransparentLed: Result+='44_Barx2MediumTransparentLed.svg';
  ftBarMediumColoredLed: Result+='46_BarMediumColoredLed.svg';
  ftBarx2MediumColoredLed: Result+='47_Barx2MediumColoredLed.svg';
  ftMovingHead04: Result+='48_MovingHead04.svg';
  ftMovingHead05: Result+='49_MovingHead05.svg';
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
   ftFan: Result := SFan;
   ftLaser: Result := SLaser;
   ftParSmallTransparentLed: Result := SSmallPar;
   ftFlower01: Result := SFlower01;
   ftParSquareSingleTransparentLed: Result := SSquareParSingleTransparentLed;
   ftParSquareMultipleTransparentLed: Result := SSquareParMultipleTransparentLed;
   ftParSquareMultipleColoredLed: Result := SquareParMultipleColoredLed;
   ftParRectangularMultipleTransparentLed: Result := SParRectangularMultipleTransparentLed;
   ftParRectangularMultipleColoredLed: Result := SParRectangularMultipleColoredLed;
   ftBarShortTransparentLed: Result := SBarShortTransparentLed;
   ftBarShortColoredLed: Result := SBarShortColoredLed;
   ftFlower02: Result := SFlower02;
   ftFlower03: Result := SFlower03;
   ftStand01: Result := SStand01;
   ftStand02: Result := SStand02;
   ftStand03: Result := SStand03;
   ftBarShortx2TransparentLed: Result := SBarShortx2TransparentLed;
   ftBarShortx2ColoredLed: Result := SBarShortx2ColoredLed;
   ftBarx2TransparentLed: Result := SBarx2TransparentLed;
   ftBarx2ColoredLed: Result := SBarx2ColoredLed;
   ftMovingHead02: Result := SMovingHead+' 2';
   ftMovingHead03: Result := SMovingHead+' 3';
   ftBarMediumTransparentLed: Result := SBarMediumTransparentLed;
   ftBarx2MediumTransparentLed: Result := SBarx2MediumTransparentLed;
   ftBarMediumColoredLed: Result := SBarMediumColoredLed;
   ftBarx2MediumColoredLed: Result := SBarx2MediumColoredLed;
   ftMovingHead04: Result := SMovingHead+' 4';
   ftMovingHead05: Result := SMovingHead+' 5';
   else Raise exception.Create('forgot to implement!');
  end;//case
end;

function FixtureTypeToBGRA(aFT: TFixtureType): TBGRABitmap;
begin
  Result := SVGFileToBGRABitmap(FixtureSVGFileFor(aFT), -1, -1);
end;

procedure LoadFixtureImages;
var i: TFixtureType;
begin
  for i in TFixturetype do
    FixtureImages[i] := FixtureTypeToBGRA(i);
end;

procedure FreeFixtureImages;
var i: TFixtureType;
begin
  for i in TFixturetype do begin
    FixtureImages[i].Free;
    FixtureImages[i] := NIL;
  end;
end;

procedure LoadCursorImages(aWidth, aHeight: integer);
var i: TChannelType;
  f: String;
begin
  for i in TChannelType do begin
    try
      f := DMXCursorImageFileNameFor(i);
      ImageCursors[i] := SVGFileToBGRABitmap(f, aWidth, aHeight);
    except
      ImageCursors[i] := TBGRABitmap.Create(aWidth, aHeight, BGRAWhite);
    end;
  end;
  ImageCursorSize.cx := ImageCursors[ctConfig].Width;
  ImageCursorSize.cy := ImageCursors[ctConfig].Height;
end;

procedure FreeCursorImages;
var i: TChannelType;
begin
  for i in TChannelType do begin
    ImageCursors[i].Free;
    ImageCursors[i] := NIL;
  end;
end;

procedure ShowFixtureImage(aImage: TImage; aFT:TFixtureType);
var ima: TBGRABitmap;
    bmp: TBitmap;
{     function LoadBitmapFromFile(AFileName: String): TCustomBitmap;
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
     end; }
begin
  ima := SVGFileToBGRABitmap(FixtureSVGFileFor(aFT), aImage.ClientWidth, aImage.ClientHeight);
  bmp := TBitmap.Create;
  ima.AssignToBitmap(bmp);
  aImage.Picture.Assign(bmp);
  ima.Free;
  bmp.Free;
  aImage.Tag := ord(aFT);
end;

procedure ShowFixtureImage(aImage: TImage; const aFixtureLocation: TFixtureLibraryLocation);
var fixLib: TLibraryFixture;
begin
  fixlib.InitDefault;
  if fixLib.LoadFrom(aFixtureLocation) then ShowFixtureImage(aImage, fixLib.General.FixtureType)
    else aImage.Picture.Assign(NIL);
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
    ctSPEED: Result:=Result+'CursorSpeed.svg';
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
    ctROTATION: Result:=Result+'CursorRotation.svg';
    ctPANSPEED: Result:=Result+'CursorPanSpeed.svg';
    ctTILTSPEED: Result:=Result+'CursorTiltSpeed.svg';
    ctFAN: Result:=Result+'CursorFan.svg';
    ctSMOKE: Result:=Result+'CursorSmoke.svg';
    ctPANTILT: Result:=Result+'CursorPanTilt.svg';
    ctPANCONTINUOUS: Result:=Result+'CursorPanContinuous.svg';
    ctTILTCONTINUOUS: Result:=Result+'CursorTiltContinuous.svg';
    ctPRISM: Result:=Result+'CursorPrism.svg';
    ctPRISMROTATION: Result:=Result+'CursorPrismRotation.svg';
    ctLASER: Result:=Result+'CursorLaser.svg';
    ctLASERROTATION: Result:=Result+'CursorLaserRotation.svg';
    ctLASERSTROBE: Result:=Result+'CursorLaserStrobe.svg';
    ctGOBOSHAKE: Result:=Result+'CursorGoboShake.svg';
    ctFROST: Result:=Result+'CursorFrost.svg';
    ctSOUNDCONTROLED: Result:=Result+'CursorSoundControled.svg';
    else Raise exception.Create('forgot to implement!');
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

function GetFixtureModeNames(const aFixtureFilename: string): TStringArray;
var t: TStringList;
  i: integer;
  modes: TFixLibModes;
begin
  Result := NIL;
  t := TStringList.Create;
  try
    t.LoadFromFile(aFixtureFilename);
    modes.LoadModesFrom(t);
    SetLength(Result, Length(modes));
      for i:=0 to High(modes) do
        Result[i] := modes[i].Name;
  finally
    t.Free;
  end;
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

end.

