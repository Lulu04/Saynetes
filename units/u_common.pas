unit u_common;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LMessages, Graphics;

const
  APP_NAME: string = 'Saynète';
  APP_VERSION: string = '3.1.0';
  APP_CONFIG_FILENAME = 'Saynetes.ini'; // The program's options saving file


  // sub-folder (located in project folder) where are saved the data common to
  // all projects in the working directory
  COMMON_PROJECT_FOLDER_NAME = 'CommonData';

  // The DMX universes saving file is common for all projects in the working directory.
  COMMON_PROJECT_DMX_FILENAME = 'DMX.Universe';
  // The DMX groups are common for all projects in the working directory
  COMMON_PROJECT_DMX_GROUP_FILENAME = 'DMX.Group';

  // The name of the file where project's options are saved. This file is located
  // in project folder
  PROJECT_OPTION_FILENAME = 'Options.txt';

// message definition
const
  LM_MESSAGE_MainGui=LM_USER+1;
     MESS_MainGui_StartupWizard=0;
{  LM_MESSAGE_DMXUniverse=LM_USER+2;
     MESS_DMXUniverse_Update=0;
  LM_MESSAGE_Player=LM_USER+3;  }

type
  TGUIMode = (guiMainDMX,
              guiPrepaDMX,
              guiEditSequence);


type
  TSingleCmd = string;  // a single command
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

  TDevicePath = record
    DeviceIndex,
    PortIndex: integer;
  end;
  TArrayOfDevicePath = array of TDevicePath;


const
  SEQUENCERINFO_SEPARATOR = '|';
  STEPDATA_SEPARATOR = '\';
  CMD_SEPARATOR = ';';
  PARAM_SEPARATOR = ' ';

  FORBIDENCHARS:string='|\;:*$#&';
  FILENAMEFORBIDENCHARS=' /|\;,.:*?!%$#&';

  PRESET_SEPARATOR = '|';

  DIPSWITCH_SEPARATOR = ';';

  DECIMAL_CHARACTERS: array[0..1] of char=(',', '.');


const
   PROJECT_FILE_EXTENSION = '.say';
   DMX_LIBRARY_FILE_EXTENSION = '.dmx';

   PLAYLIST_FOLDER = 'Playlists';
   PLAYLIST_FILE_EXTENSION = '.playlist';

   PRESET_FOLDER = 'Presets';
   PRESET_FILE_EXTENSION = '.preset';

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
   CMD_AUDIO_PLAY           =  1    ;  // AUDIOLECTURE IDaudio volume panning
   TITLECMD_AUDIO_PLAY      = 1001  ;
   CMD_AUDIO_STOP           =  2    ;  // AUDIOSTOP IDaudio
   TITLECMD_AUDIO_STOP      = 1002  ;
   CMD_AUDIO_PAUSE          =  3    ;  // AUDIOPAUSE IDaudio
   TITLECMD_AUDIO_PAUSE     = 1003  ;
   CMD_AUDIO_FADEIN         =  4    ;  // AUDIOFADEIN IDaudio volume duration IDcurve
   TITLECMD_AUDIO_FADEIN    = 1004  ;
   CMD_AUDIO_FADEOUT        =  5    ;  // AUDIOFADEOUT IDaudio duration IDcurve
   TITLECMD_AUDIO_FADEOUT   = 1005  ;
   CMD_AUDIO_SETVOLUME      =  6    ;  // AUDIOFIXEVOLUME IDaudio volume duration IDcurve
   TITLECMD_AUDIO_SETVOLUME = 1006  ;
   CMD_AUDIO_SETPAN         =  7    ;  // AUDIOFIXEPAN IDaudio panning duration IDcurve
   TITLECMD_AUDIO_SETPAN    = 1007  ;
   CMD_AUDIO_SETPITCH       =  8    ;  // AUDIOFIXEFREQ IDaudio frequence duration IDcurve
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

   CMD_DMX_FLAME        =  201;  // CMD_DMX_FLAME IDuniverse IDFixture ChanIndex LevelMin LevelMax Speed Soften
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
                                           // MaxPercent: single  range 0.0 to 1.1    %
                                           // SoftenTime: single   range 0.1 to 1.0   seconds

   CMD_DMX_FLASH      =  209; // CMD_DMX_FLASH IDuniverse IDFixture ChanIndex
                              //               LevelMin LevelMax DurationMin DurationMax
                              // if LevelMin <> LevelMax -> random value between this bounds
                              // if DurationMin <> DurationMax -> random duration between this bounds
   TITLECMD_DMX_FLASH = 1209; // TITLECMD_DMX_FLASH LevelMin LevelMax DurationMin DurationMax

   CMD_DMX_STOPEFFECTRGB      =  210; // CMD_DMX_STOPEFFECTRGB IDuniverse IDFixture
   TITLECMD_DMX_STOPEFFECTRGB = 1210; // TITLECMD_DMX_STOPEFFECTRGB

   CMD_DMX_COPYCHANNEL        =  211; // CMD_DMX_COPYCHANNEL SourceIDuniverse SourceIDFixture SourceChanIndex
   TITLECMD_DMX_COPYCHANNEL   = 1211; // TITLECMD_DMX_COPYCHANNEL SourceIDuniverse SourceIDFixture SourceChanIndex
                                      //                TargetIDUniverse TargetIDFixture TargetChanIndex

   TITLECMD_DMX_COPYRGB  = 1212; // TITLECMD_DMX_COPYRGB SourceIDuniverse SourceIDFixture
   CMD_DMX_COPYRGB       =  212; // CMD_DMX_COPYRGB SourceIDuniverse SourceIDFixture TargetIDUniverse TargetIDFixture

   TITLECMD_DMX_FLASHRGB = 1213; // TITLECMD_DMX_FLASHRGB Color pcMin pcMax DurationMin DurationMax
   CMD_DMX_FLASHRGB      =  213; // CMD_DMX_FLASHRGB IDuniverse IDFixture Color pcMin pcMax DurationMin DurationMax

   CMD_INTERNALDMXWAVE   =  214; //INTERNALDMXWAVE IDuniverse IDFixture ChanIndex
                                 //                Percent1 Duration1 CurveID1
                                 //                KeepTime
                                 //                Percent2 Duration2 CurveID2

   // Image on external screen

   PHOTOFADEINCOULEUR = 300;  // PHOTOFADEINCOULEUR couleurfond durée alpha
   PHOTOFADEINIMAGE   = 301;  // PHOTOFADEINIMAGE zoom couleurfond durée
   PHOTOFADEOUT       = 302;  // PHOTOFADEOUT durée


   // Video on external screen

   VIDEOLECTURE        = 350;
   VIDEOPAUSE          = 351;
   VIDEOREPRENDRE      = 352;
   VIDEOSTOP           = 353;
   VIDEOFIXEVOLUME     = 354;
   VIDEOFADEVOLUME     = 355;  // VIDEOFADEVOLUME volume durée
   VIDEOPOSITION       = 356;

  FichierCouleur    =    'Data'+DirectorySeparator+'ColorList.col';  // liste des couleurs rvb

  APPLICATION_PRESET_PATH = 'Data'+DirectorySeparator+'PRESET'+DirectorySeparator;

  CHEMINBIBLIOTHEQUEDMX = 'Data'+DirectorySeparator+'DMXLibrary'+DirectorySeparator;
  CHEMIN_FICHIER_IMAGE_BIBLIOTHEQUE = 'Data'+DirectorySeparator+'icons'+DirectorySeparator+
                                      'FixtureImages'+DirectorySeparator;


  LM_MESSAGE_AffichageDMXPrincipal = LM_USER + 1 ;   // message pour l'affichage dmx principal


type

    // énumération des types d'appareils dmx possibles
    // KEEP THIS ORDER !!
    TFixtureType = ( ftOther=0,
                     ftPlanConvex,     // projecteur plan convex
                     ftParShortBulb,   // short PAR with bulb
                     ftHalogen,        // projecteur halogène
                     ftParLongTransparentLed, // PAR with transparent led long version
                     ftBarColoredLed,  // rampe à leds
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
                     ftParLongBulb     // long PAR with bulb
                         );
const
  FixtureDisplayOrder: array[Low(TFixtureType)..High(TFixtureType)] of TFixtureType=(
                   ftOther,
                   ftParShortTransparentLed, ftParLongTransparentLed,
                   ftLedParShortWithColoredLed, ftLedParLongWithColoredLed,
                   ftLedBarTransparentLed, ftBarColoredLed,
                   ftMatrixTransparentLed, ftMatrixWithColoredLed,
                   ftParShortBulb, ftParLongBulb,
                   ftPlanConvex,
                   ftHalogen,
                   ftProfile,
                   ftScanner, ftMovingHead,
                   ftColorChanger,
                   ftSmokeMachine, ftBubbleMachine,
                   ftDimmer1Channel,
                   ftDimmer4Channels);

type

    // enum for channels type
    // KEEP this order !!
    TChannelType = ( ctCONFIG=0,
                     ctMASTERDIMMER,
                     ctDIMMER,
                     ctRED,
                     ctGREEN,
                     ctBLUE,
                     ctSTROBE,
                     ctPAN,
                     ctTILT,
                     ctPANTILTSPEED,
                     ctGOBO,
                     ctGOBOROTATION,
                     ctCOLORCHOICE,
                     ctWHITE,
                     ctAMBER,
                     ctUV,
                     ctSPEED,        // 16
                     ctNOFUNCTION,
                     ctCYAN,
                     ctMAGENTA,
                     ctYELLOW,
                     ctLIME,
                     ctINDIGO,
                     ctWARMWHITE,
                     ctCOLDWHITE,
                     ctIRIS,
                     ctBLADEINSERTION
                   );

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


implementation


end.

