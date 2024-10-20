unit u_common;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, LMessages, Graphics, BGRABitmap, Types;

const
  APP_NAME: string = 'Saynète';
  APP_VERSION: string = '3.1.2';
  APP_CONFIG_FOLDER = 'Saynetes'; // the folder created by app to save program options, log file...
  APP_CONFIG_FILENAME = 'Saynetes.cfg'; // The program's options saving file

  URL_FOR_VERSION_ON_GITHUB = 'https://raw.githubusercontent.com/Lulu04/Saynetes/main/version.txt';
  URL_FOR_LATEST_RELEASE_ON_GITHUB = 'https://github.com/Lulu04/Saynetes/releases/latest';


  // sub-folder (located in project folder) where are saved the data common to
  // all projects in the working directory
  COMMON_PROJECT_FOLDER_NAME = 'CommonData';

  // The DMX universes are shared by all projects in the working directory.
  COMMON_PROJECT_DMX_FILENAME = 'DMX.Universe';
  // The DMX groups are shared by all projects in the working directory
  COMMON_PROJECT_DMX_GROUP_FILENAME = 'DMX.Group';

  // The name of the file where project's options are saved. This file is located
  // in project folder
  PROJECT_OPTION_FILENAME = 'Options.txt';

// message definition
const
  LM_MESSAGE_MainGui = LM_USER + 1;
     MESS_MainGui_StartupWizard = 0;

  LM_MESSAGE_EditFixtureWizard = LM_USER + 2;
     MESS_DeleteModeFrame = 0;
{  LM_MESSAGE_DMXUniverse=LM_USER+2;
     MESS_DMXUniverse_Update=0;
  LM_MESSAGE_Player=LM_USER+3;  }

type
  TGUIMode = (guiMainDMX,
              guiPrepaDMX,
              guiEditSequence);


type
  TSingleCmd = string;  // a single command with parameters separated by 'SPACE'
  TParamArray = TStringArray; //  <- splitted TSingleCmd

  // group of several commands.  e.g: 'CMD_STARTTOP 12;Wait 5.0;CMD_STOPTOP 12'
  TCmdList = string;
  TCmdArray = TStringArray; //  <- splitted TCmdList

  TStepDataList = string;    // steps info packed in a string
  TStepDataArray = TStringArray; //  <- splitted TStepDataList

  TSequencerInfoList = string; // sequencer info and its steps packed in a string
  TSequencerInfoArray = TStringArray; //  <- splitted TSequencerInfoList

  TArrayOfInteger = array of integer;
  TArrayOfSingle = array of single;


  TDMXAdress = word;
  TChannelPath = record
    IDUni,
    IDFix: cardinal;
    ChanIndex: integer;
  end;
  TFixturePath = record
    IDUni,
    IDFix: cardinal;
  end;

  { TDevicePath }

  TDevicePath = record
    DeviceIndex,
    PortIndex: integer;
    class operator =(a,b: TDevicePath): boolean;
  end;
  TArrayOfDevicePath = array of TDevicePath;


const
  SEQUENCERINFO_SEPARATOR = '|';
  STEPDATA_SEPARATOR = '\';
  CMD_SEPARATOR = ';';
  PARAM_SEPARATOR = ' ';

  FORBIDENCHARS:string='|\;:*$#&~';
  FILENAMEFORBIDENCHARS=' /|\;,.:*?!%$#&';

  PRESET_SEPARATOR = '|';

  FIXTURELOCATION_SEPARATOR = '~';

  DECIMAL_CHARACTERS: array[0..1] of char=(',', '.');

const
   PROJECT_FILE_EXTENSION = '.say';
   DMX_LIBRARY_FILE_EXTENSION = '.dmx';
   PEAK_AUDIO_FILE_EXTENSION = '.pk';

   PLAYLIST_FOLDER = 'Playlists';
   PLAYLIST_FILE_EXTENSION = '.playlist';

   FACTORY_PRESETS_FOLDER = 'FactoryPresets';
   USER_PRESETS_FOLDER = 'Presets';
   PRESET_FILE_EXTENSION = '.preset';

   PROJECT_EXAMPLE_FOLDER = 'Examples';

const
   NameOfAudioFXName: array[0..11] of string=(
           'AUTOWAH',
           'CHORUS',
           'FLANGER',
           'COMPRESSOR',
           'DISTORTION',
           'ECHO',
           'EQUALIZER',
           'FREQUENCY SHIFTER',
           'PITCH SHIFTER',
           'RING MODULATOR',
           'VOCAL MORPHER',
           'EAXREVERB');

const
// Available commands that can be executed by a sequence.

   CMD_UNKNOW = 0;

   // TOP AUDIO
   CMD_AUDIO_PLAY           =  1    ;  // CMD_AUDIO_PLAY IDaudio volume panning
   TITLECMD_AUDIO_PLAY      = 1001  ;
   CMD_AUDIO_STOP           =  2    ;  // CMD_AUDIO_STOP IDaudio
   TITLECMD_AUDIO_STOP      = 1002  ;
   CMD_AUDIO_PAUSE          =  3    ;  // CMD_AUDIO_PAUSE IDaudio
   TITLECMD_AUDIO_PAUSE     = 1003  ;
   CMD_AUDIO_FADEIN         =  4    ;  // CMD_AUDIO_FADEIN IDaudio volume duration IDcurve
   TITLECMD_AUDIO_FADEIN    = 1004  ;
   CMD_AUDIO_FADEOUT        =  5    ;  // CMD_AUDIO_FADEOUT IDaudio duration IDcurve
   TITLECMD_AUDIO_FADEOUT   = 1005  ;
   CMD_AUDIO_SETVOLUME      =  6    ;  // CMD_AUDIO_SETVOLUME IDaudio volume duration IDcurve
   TITLECMD_AUDIO_SETVOLUME = 1006  ;
   CMD_AUDIO_SETPAN         =  7    ;  // CMD_AUDIO_SETPAN IDaudio panning duration IDcurve
   TITLECMD_AUDIO_SETPAN    = 1007  ;
   CMD_AUDIO_SETPITCH       =  8    ;  // CMD_AUDIO_SETPITCH IDaudio frequence duration IDcurve
   TITLECMD_AUDIO_SETPITCH  = 1008  ;

   TITLECMD_AUDIO_APPLYFX = 1020; // TITLECMD_AUDIO_APPLYFX  IDaudio  dry/wet  EffectCount
   CMD_AUDIO_FXPRESET     =   20; // CMD_AUDIO_FXPRESET  effectType  presetIndex
   CMD_AUDIO_REMOVEFX     =   21; // CMD_AUDIO_REMOVEFX  IDaudio

   // Capture to playback on default capture device only.
   CAPTURE_IDAUDIO = -10;
   CMD_AUDIO_CAPTURE_START        = 30; // CMD_AUDIO_CAPTURE_START
   CMD_AUDIO_CAPTURE_STOP         = 31; // CMD_AUDIO_CAPTURE_STOP
   CMD_AUDIO_CAPTURE_SETVOLUME    = 32; // CMD_AUDIO_CAPTURE_SETVOLUME volume duration IDcurve
   CMD_AUDIO_CAPTURE_SETPAN       = 33; // CMD_AUDIO_CAPTURE_SETPAN  panning duration IDcurve
   TITLECMD_AUDIO_CAPTURE_APPLYFX = 34; // TITLECMD_AUDIO_CAPTURE_APPLYFX  dry/wet  EffectCount
   CMD_AUDIO_CAPTURE_FXPRESET     = 35; // CMD_AUDIO_CAPTURE_FXPRESET  effectType  presetIndex
   CMD_AUDIO_CAPTURE_REMOVEFX     = 36; // CMD_AUDIO_CAPTURE_REMOVEFX

   // Other

   CMD_WAIT           = 100;  // CMD_WAIT DurationF
   CMD_LOOP           = 101;  // CMD_LOOP
   CMD_STARTSEQUENCE       = 102;  // CMD_STARTSEQUENCE IDSeq
   CMD_STOPSEQUENCE        = 103;  // CMD_STOPSEQUENCE IDSeq
   CMD_SEQUENCESTRETCHTIME = 104;  // CMD_SEQUENCESTRETCHTIME IDSeq StretchValueF DurationF CurveID

   // DMX

   CMD_DMX_DIMMER       =  200;  // CMD_DMX_DIMMER IDuniverse IDFixture ChanIndex PercentF DurationF CurveID
   TITLECMD_DMX_DIMMER  = 1200;  // TITLECMD_DMX_DIMMER Duration CurveID

   CMD_DMX_FLAME        =  201;  // CMD_DMX_FLAME IDuniverse IDFixture ChanIndex LevelMin LevelMax WaitTime Soften
   TITLECMD_DMX_FLAME   = 1201;  // TITLECMD_DMX_FLAME  LevelMinF LevelMaxF WaitTimeF SoftenF
                                   // LevelMin: single  range 0.0 to 1.0
                                   // LevelMax: single  range 0.0 to 1.0
                                   // WaitTime: single  range 0.05 to 2.0
                                   // Soften: single range 0.0 to 1.0

   CMD_DMX_STOPEFFECT         =  203;  // CMD_DMX_STOPEFFECT IDuniverse IDFixture ChanIndex
   TITLECMD_DMX_STOPEFFECT    = 1203;

   CMD_DMX_DIMMERRGB          =  204;  // CMD_DMX_DIMMERRGB IDuniverse IDFixture Color Duration CurveID
   TITLECMD_DMX_DIMMERRGB     = 1204;  // TITLECMD_DMX_DIMMERRGB Color Duration CurveID

   CMD_DMX_FLAMERGB           =  205; // CMD_DMX_FLAMERGB IDuniverse IDFixture Color Speed Amplitude Soften
   TITLECMD_DMX_FLAMERGB      = 1205; // TITLECMD_DMX_FLAMERGB Color Speed Amplitude Soften

   CMD_DMX_AUDIOFOLLOWERRGB        =  207; // CMD_DMX_AUDIOFOLLOWERRGB IDuniverse IDFixture IDaudio Color Gain SoftenTime
   TITLECMD_DMX_AUDIOFOLLOWERRGB   = 1207; // TITLECMD_DMX_AUDIOFOLLOWERRGB IDaudio Color Gain SoftenTime

   CMD_DMX_AUDIOFOLLOWER           =  208; // CMD_DMX_AUDIOFOLLOWER IDuniverse IDFixture ChanIndex IDaudio Gain MaxPercent SoftenTime
   TITLECMD_DMX_AUDIOFOLLOWER      = 1208; // TITLECMD_DMX_AUDIOFOLLOWER IDaudio Gain MaxPercent SoftenTime
                                           // Gain: single  range -1.0 to 3.0
                                           // MaxPercent: single  range 0.0 to 1.0    %
                                           // SoftenTime: single   range 0.1 to 1.0   seconds

   CMD_DMX_FLASH      =  209; // CMD_DMX_FLASH IDuniverse IDFixture ChanIndex
                              //               LevelMin LevelMax DurationMin DurationMax
                              // if LevelMin <> LevelMax -> random value between this bounds
                              // if DurationMin <> DurationMax -> random duration between this bounds
   TITLECMD_DMX_FLASH = 1209; // TITLECMD_DMX_FLASH LevelMin LevelMax DurationMin DurationMax

   CMD_DMX_STOPEFFECTRGB      =  210; // CMD_DMX_STOPEFFECTRGB IDuniverse IDFixture
   TITLECMD_DMX_STOPEFFECTRGB = 1210; // TITLECMD_DMX_STOPEFFECTRGB

   CMD_DMX_COPYCHANNEL        =  211; // CMD_DMX_COPYCHANNEL SourceIDuniverse SourceIDFixture SourceChanIndex
                                      //                     TargetIDUniverse TargetIDFixture TargetChanIndex
   TITLECMD_DMX_COPYCHANNEL   = 1211; // TITLECMD_DMX_COPYCHANNEL SourceIDuniverse SourceIDFixture SourceChanIndex

   TITLECMD_DMX_COPYRGB  = 1212; // TITLECMD_DMX_COPYRGB SourceIDuniverse SourceIDFixture
   CMD_DMX_COPYRGB       =  212; // CMD_DMX_COPYRGB SourceIDuniverse SourceIDFixture TargetIDUniverse TargetIDFixture

   TITLECMD_DMX_FLASHRGB = 1213; // TITLECMD_DMX_FLASHRGB Color pcMin pcMax DurationMin DurationMax
   CMD_DMX_FLASHRGB      =  213; // CMD_DMX_FLASHRGB IDuniverse IDFixture Color pcMin pcMax DurationMin DurationMax

   CMD_INTERNALDMXWAVE   =  214; // INTERNALDMXWAVE IDuniverse IDFixture ChanIndex
                                 //                 Percent1 Duration1 CurveID1
                                 //                 KeepTime
                                 //                 Percent2 Duration2 CurveID2

   TITLECMD_DMX_WAVERGB  =1215; // TITLECMD_DMX_WAVERGB Color1 Duration1 CurveID1
                                //                      Color2 Duration2 CurveID2
   CMD_DMX_WAVERGB  = 215; // CMD_DMX_WAVERGB IDuniverse IDFixture
                           //                 Color1 Duration1 CurveID1
                           //                 Color2 Duration2 CurveID2

   TITLECMD_DMX_WAVE = 1216; // TITLECMD_DMX_WAVE Level1 Duration1 CurveID1 Level2 Duration2 CurveID2
   CMD_DMX_WAVE = 216; // CMD_DMX_WAVE IDuniverse IDFixture ChanIndex
                       //              Level1 Duration1 CurveID1 Level2 Duration2 CurveID2

   // Image on external screen

   IMAGEFADEINCOLOR = 300;  // IMAGEFADEINCOLOR Color Duration alpha
   IMAGEFADEIN      = 301;  // IMAGEFADEIN Zoom bgColor Duration
   IMAGEFADEOUT     = 302;  // IMAGEFADEOUT Duration


   // Video on external screen

   VIDEOLECTURE        = 350;
   VIDEOPAUSE          = 351;
   VIDEOREPRENDRE      = 352;
   VIDEOSTOP           = 353;
   VIDEOFIXEVOLUME     = 354;
   VIDEOFADEVOLUME     = 355;  // VIDEOFADEVOLUME volume durée
   VIDEOPOSITION       = 356;

type

    // enum of possible fixture image
    // KEEP THIS ORDER !!
    TFixtureType = ( ftOther=0,
                     ftPlanConvex,     // plan convex
                     ftParShortBulb,   // short PAR with bulb
                     ftHalogen,        // halogen
                     ftParLongTransparentLed, // PAR with transparent led long version
                     ftBarColoredLed,
                     ftProfile,        // découpe
                     ftColorChanger,
                     ftScanner,
                     ftMovingHead,
                     ftSmokeMachine,
                     ftBubbleMachine,
                     ftDimmer1Channel,
                     ftDimmer4Channels,
                     ftMatrixTransparentLed,
                     ftMatrixWithColoredLed,   //15
                     ftParShortTransparentLed, // PAR with transparent led short version
                     ftLedParLongWithColoredLed,  // PAR with colored led RGB long version
                     ftLedParShortWithColoredLed, // PAR with colored led RGB short version
                     ftLedBarTransparentLed,
                     ftParLongBulb,     // long PAR with bulb
                     ftFan,      // 21
                     ftLaser,
                     ftParSmallTransparentLed,
                     ftFlower01,
                     ftParSquareSingleTransparentLed,  // square par single transparent led
                     ftParSquareMultipleTransparentLed, // square par with multiple transparent led
                     ftParSquareMultipleColoredLed,  // square par with multiple colored led
                     ftParRectangularMultipleTransparentLed,
                     ftParRectangularMultipleColoredLed,
                     ftBarShortTransparentLed,  // 30
                     ftBarShortColoredLed,
                     ftFlower02,
                     ftFlower03,
                     ftStand01,
                     ftStand02,   // 35
                     ftStand03,
                     ftBarShortx2TransparentLed,
                     ftBarShortx2ColoredLed,
                     ftBarx2TransparentLed,
                     ftBarx2ColoredLed,   // 40
                     ftMovingHead02,
                     ftMovingHead03,
                     ftBarMediumTransparentLed,
                     ftBarx2MediumTransparentLed,
                     ftBarMediumColoredLed,  // 45
                     ftBarx2MediumColoredLed,
                     ftMovingHead04,
                     ftMovingHead05
                     );

const
  FixtureDisplayOrder: array[Low(TFixtureType)..High(TFixtureType)] of TFixtureType=(
                   ftOther, ftParSmallTransparentLed,
                   ftParShortTransparentLed, ftParLongTransparentLed,
                   ftLedParShortWithColoredLed, ftLedParLongWithColoredLed,
                   ftParSquareSingleTransparentLed,
                   ftParSquareMultipleTransparentLed,
                   ftParSquareMultipleColoredLed,
                   ftParRectangularMultipleTransparentLed,
                   ftParRectangularMultipleColoredLed,

                   ftBarShortTransparentLed, ftBarMediumTransparentLed, ftLedBarTransparentLed,
                   ftBarShortx2TransparentLed, ftBarx2MediumTransparentLed, ftBarx2TransparentLed,
                   ftBarShortColoredLed, ftBarMediumColoredLed, ftBarColoredLed,
                   ftBarShortx2ColoredLed, ftBarx2MediumColoredLed, ftBarx2ColoredLed,

                   ftMatrixTransparentLed, ftMatrixWithColoredLed,
                   ftParShortBulb, ftParLongBulb,
                   ftPlanConvex,
                   ftHalogen,
                   ftProfile,
                   ftScanner,

                   ftMovingHead, ftMovingHead02, ftMovingHead03, ftMovingHead04, ftMovingHead05,
                   ftLaser,

                   ftFlower01, ftFlower02, ftFlower03,

                   ftStand01, ftStand02, ftStand03,

                   ftColorChanger,
                   ftSmokeMachine, ftBubbleMachine, ftFan,
                   ftDimmer1Channel,
                   ftDimmer4Channels);

  FixtureCanFlipH: array[TFixtureType] of boolean =(
               False,
               True,   // ftPlanConvex
               True,   // ftParShortBulb
               True,   // ftHalogen
               True,   // ftParLongTransparentLed
               False,  // ftBarColoredLed
               False,  // ftProfile
               True,   // ftColorChanger
               True,   // ftScanner
               True,   // ftMovingHead
               True,   // ftSmokeMachine
               True,   // ftBubbleMachine
               False,  // ftDimmer1Channel
               False,  // ftDimmer4Channel
               True,   // ftMatrixTransparentLed
               True,   // ftMatrixWithColoredLed
               True,   // ftParShortTransparentLed
               True,   // ftLedParLongWithColoredLed
               True,   // ftLedParShortWithColoredLed
               False,  // ftLedBarTransparentLed
               True,   // ftParLongBulb
               True,   // ftFan
               True,   // ftLaser
               True,   // ftParSmallTransparentLed
               True,   // ftFlower01
               True,   // ftParSquareSingleTransparentLed
               True,   // ftParSquareMultipleTransparentLed
               True,   // ftParSquareMultipleColoredLed
               True,   // ftParRectangularMultipleTransparentLed
               True,   // ftParRectangularMultipleColoredLed
               False,  // ftBarShortTransparentLed
               False,  // ftBarShortColoredLed
               True,   // ftFlower02
               True,   // ftFlower03
               True,   // ftStand01
               True,   // ftStand02
               True,   // ftStand03
               False,  // ftBarShortx2TransparentLed,
               False,  // ftBarShortx2ColoredLed,
               False,  // ftBarx2TransparentLed,
               False,  // ftBarx2ColoredLed
               True,   // ftMovingHead02
               True,   // ftMovingHead03
               False,  // ftBarMediumTransparentLed
               False,  // ftBarx2MediumTransparentLed
               False,  // ftBarMediumColoredLed
               False,  // ftBarx2MediumColoredLed
               True,   // ftMovingHead04
               True    // ftMovingHead05
               );

  FixtureCanFlipV: array[TFixtureType] of boolean =(
               False,  // ftOther
               True,   // ftPlanConvex
               True,   // ftParShortBulb
               True,   // ftHalogen
               True,   // ftParLongTransparentLed
               False,  // ftBarColoredLed
               True,   // ftProfile
               False,  // ftColorChanger
               False,  // ftScanner
               False,  // ftMovingHead
               False,  // ftSmokeMachine
               False,  // ftBubbleMachine
               False,  // ftDimmer1Channel
               False,  // ftDimmer4Channel
               False,  // ftMatrixTransparentLed
               False,  // ftMatrixWithColoredLed
               True,   // ftParShortTransparentLed
               True,   // ftLedParLongWithColoredLed
               True,   // ftLedParShortWithColoredLed
               False,  // ftLedBarTransparentLed
               True,   // ftParLongBulb
               False,  // ftFan
               False,  // ftLaser
               True,   // ftParSmallTransparentLed
               True,   // ftFlower01
               True,   // ftParSquareSingleTransparentLed
               True,   // ftParSquareMultipleTransparentLed
               True,   // ftParSquareMultipleColoredLed
               True,   // ftParRectangularMultipleTransparentLed
               True,   // ftParRectangularMultipleColoredLed
               False,  // ftBarShortTransparentLed
               False,  // ftBarShortColoredLed
               True,   // ftFlower02
               True,   // ftFlower03
               False,  // ftStand01
               False,  // ftStand02
               False,  // ftStand03
               False,  // ftBarShortx2TransparentLed,
               False,  // ftBarShortx2ColoredLed,
               False,  // ftBarx2TransparentLed,
               False,  // ftBarx2ColoredLed
               False,  // ftMovingHead02
               False,  // ftMovingHead03
               False,  // ftBarMediumTransparentLed
               False,  // ftBarx2MediumTransparentLed
               False,  // ftBarMediumColoredLed
               False,  // ftBarx2MediumColoredLed
               False,  // ftMovingHead04
               False   // ftMovingHead05
               );
type

    // enum for channels type
    // KEEP this order !!
    TChannelType = ( ctCONFIG=0,
                     ctMASTERDIMMER,
                     ctDIMMER,
                     ctRED,
                     ctGREEN,
                     ctBLUE,         // 5
                     ctSTROBE,
                     ctPAN,
                     ctTILT,
                     ctPANTILTSPEED,
                     ctGOBO,        // 10
                     ctGOBOROTATION,
                     ctCOLORCHOICE,
                     ctWHITE,
                     ctAMBER,
                     ctUV,           // 15
                     ctSPEED,
                     ctNOFUNCTION,
                     ctCYAN,
                     ctMAGENTA,
                     ctYELLOW,      // 20
                     ctLIME,
                     ctINDIGO,
                     ctWARMWHITE,
                     ctCOLDWHITE,
                     ctIRIS,          // 25
                     ctBLADEINSERTION,
                     ctCOLORTEMPERATURE,
                     ctSTROBESPEED,
                     ctSOUNDSENSITIVITY,
                     ctBLADEROTATION,   // 30
                     ctZOOM,
                     ctFOCUS,
                     ctROTATION,
                     ctPANSPEED,
                     ctTILTSPEED,       // 35
                     ctFAN,
                     ctSMOKE,
                     ctPANTILT,
                     ctPANCONTINUOUS,
                     ctTILTCONTINUOUS,  // 40
                     ctPRISM,
                     ctPRISMROTATION,
                     ctLASER,
                     ctLASERROTATION,
                     ctLASERSTROBE,     // 45
                     ctGOBOSHAKE,
                     ctFROST,
                     ctSOUNDCONTROLED,
                     ctEFFECT,
                     ctEFFECTSPEED     // 50
                   );
var
  FixtureImages: array[TFixtureType] of TBGRABitmap;
  ImageCursors: array[TChannelType] of TBGRABitmap;
  ImageCursorSize: TSize;

const
  DMX_EFFECT_IMAGE_COUNT = 6;
  ImageDmxEffectsNames: array[0..DMX_EFFECT_IMAGE_COUNT-1] of string = (
         'Dimmer.svg',    'Flame.svg',    'AudioFollower.svg',    'Copy.svg',
         'FlameRGB.svg',    'AudioFollowerRGB.svg');
var
  ImageDmxEffects: array[0..DMX_EFFECT_IMAGE_COUNT-1] of TBGRABitmap;

type

    // stage shape rendered in the stage view
    TStageType=( stNone=0,
                 stRectangle,
                 stSquare,
                 stHalfCircle,
                 stEllipse,
                 stCustom1
               );
    // type of shape for the seats in the stage view
    TSeatType=( seatNone=0,
                seatType1,
                seatType2
              );


const
    MANUFACTURER_LIST = 'manufacturers.txt';
type
  TFixtureManufacturer = record
    Folder,
    Name,
    WebSite: string;
  end;
  TManufacturers = array of TFixtureManufacturer;
  PManufacturers = ^TManufacturers;

implementation


{ TDevicePath }

class operator TDevicePath.=(a, b: TDevicePath): boolean;
begin
  Result := (a.DeviceIndex = b.DeviceIndex) and (a.PortIndex = b.PortIndex);
end;

end.

