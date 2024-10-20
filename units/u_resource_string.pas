unit u_resource_string;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;


resourcestring
  SErrorAccessingInternet='Error accessing internet';
  SNoNewVersion='No new version';
  SCurrentVersion='current version';
  SIsAvailable='is available';
  SDoYouWantToQuitAndOpenURLToDownloadIt='Do you want to quit the program and open the website to download the it ?';
  SWrittenBy='Written by';
  SCredits='Credits';
  SIconAppBy='Application icon by';
  SApplicationWrittenIn='Application written in';
  SWith='with';
  //SMailSentSuccessfully='Mail sent successfully';
  //SFailToSendMail='Fail to send mail';
  //SAskUserToSendDefinitionByMail='Do you agree to e-mail this definition to the author of Saynètes? It will be included in the next version of the software.';

  SLoadingFixtureImage='loading DMX fixture images';
  SLoadingCursorImage='loading DMX cursor images';
  SLoadingProgramIcon='loading program icons';
  SLoadingDmxChannelIcon='loading DMX channel icons';
  SLoadingDmxEffectsImage='loading DMX effect images';

  SEditMode='EDIT MODE';
  SShowMode='SHOW MODE';
  SEntering='Entering';
  SEditionAllowed='Edition of audio, dmx and sequences... is now allowed';
  SEditionNotAllowed='Edition of audio, dmx and sequences is no longer allowed';

  SRemoveAudioFileFromProject='Remove audio file(s) from project ?';
  SFileWillBeDeletedFromProjectStorage='The file(s) will be deleted from the project storage and will no longer be available';

  SFailureWhenSavingProjectTo='Failure when saving project to';
  SFailureWhenSavingDMXUniverseTo='Failure when saving DMX universe to';

  SPrevious='Previous';
  SNext='Next';
  SFinish='Finish';
  STheFile='The file';
  sIsAlreadyImported='is already imported';
  SItsNameIsReplacedBy='Its name is replaced by';
  SDeleteThisUniverse='Delete this universe ?';
  SUniverseFull='Universe full or not enough free adress available';
  SFailToLoadTheFixtureFromLibrary='Fail to load the fixture from library !!';
  SAreYouSureToDeleteThisFixture='Are you sure you want to delete this fixture ?';
  SAreYouSureToDeleteTheseFixtures='Are you sure you want to delete these fixtures ?';
  SFixtureNotInLibrary='fixture not in library';
  SActuallyThereIsNoUniverse='Actually, there is no DMX universe.';
  SWouldYouLikeToCreateOne='Would you like to create one ?';
  SAll='all';
  SAll_='All';
  SOnly='only ';

  SRequireTheProgramToBeRestarted='Requires the program to be restarted.';

  SYes='Yes';
  SNo='No';
  SOk='Ok';
  SSave='Save';
  SClose='Close';
  SCancel='Cancel';
  SApply='Apply';
  SAdd='Add';
  SDelete='Delete';
  SModify='Modify';
  SEdit='Edit';
  SRename='Rename';
  SImport='Import';
  SAliasOf_='Set as an alias of...';
  SAliasOf='Alias of';
  SIsAnAliasOf='is an alias of';
  SThatDoesntExists='that doesn''t exists';
  SUpdate='Update';
  SDefault='Default';
  SCut='Cut';
  SPaste='Paste';
  SUndo='Undo';
  SRedo='Redo';
  SContinue='Continue';
  SCreateSequence='Create sequence';
  SDescription='Description';
  SExtra='Extra';
  SSwitchers='Switchers';
  sAddNewSwitcher='Add new switcher';
  SNewName='New name:';
  SNewDescription='New description:';
  SSequenceName='Sequence name';
  SActionName='Action Name';
  SNotAnAction='Not an action...';
  SActionWithBadFormat='Action with bad format...';

  SAreYouSureToDeleteThisSequence='Are you sure you want to delete this sequence ?';
  SAddThisSequence='Add this sequence';
  SApplyChanges='Apply changes';
  SSequence='Sequence';
  SSeq='Seq';
  Srunning='running';
  SGeneral='General';
  SDmx='DMX';
  SAudio='Audio';
  SMainView='Main view';

  SAddAction='Add action';
  SApplyTheModifications='Apply the modifications';
  SAction='Action';

  SAnErrorOccurredWhileRemovingTheFileFromDisk='An error occurred while removing the file from disk';

  SUseBadVirtualChannelName='use bad virtual channel name';
  SUseBadSubChannelName='use bad sub-channel name';
  STheSwitcherInRange='the switcher in range';
  SHaveRangeError='have dmx range error';
  SThisModeIsEmpty='This mode is empty or have only virtual channel';
  SErrorOnName='Error on name';
  SDefinedButNotUsed='defined but not used';
  SMode='Mode';
  SVirtualChannel='Virtual channel';
  SSubChannel='Sub-channel';
  SChannel='Channel';
  SChannels='channels';
  SChannels_='Channels';
  SRGB='RGB';
  SGroup='Group';
  SGroupOfChannels='Group of channels';
  SNameForTheNewGroup='Name for the new group:';
  SRGBGroup='RGB Group';
  SNameForTheNewRGBGroup='Name for the new RGB group:';
  SRed='Red';
  SGreen='Green';
  SBlue='Blue';
  SAddMultiple='Add multiple';
  SMultipleChannels='Multiple channels';

  SFixtureEdition='Fixture edition';
  SManufacturer='Manufacturer';
  SAuthors='Authors';
  SDimensions='Dimensions';
  SWidth='Width';
  SHeight='Height';
  SDepth='Depth';
  SWeight='Weight';
  SConnector='Connector';
  SBulb='Bulb';
  SLumens='Lumens';
  SLens='Lens';
  SMinDegree='Min degree';
  SMaxDegree='Max degree';
  SWebLinks='Web links';
  SManufacturerName='Manufacturer name:';
  SNameOfTheFixture='Name of the fixture:';
  SModeName='Mode name (optional):';
  SAFixtureMustHaveAtLeastOneChannel='A fixture must have at least one channel';
  SMustHaveAtLeastOneRange=' must have at least one range.';
  SAsReminderTheRangesMustCoverTheInterval='As a reminder, the ranges must cover the interval 0 to 255';
  SFirstRangeShouldStartFrom0=' first range should start from 0';
  SLastRangeShouldEndOn255=' last range should end on 255';
  SStartValueMustBeGreaterThanEndValue=' start value must be lower than end value';
  SADiscontinuityAppearsOnTheRange=' a discontinuity appears on the range';
  STheFixtureWasCreatedSuccessfully='The fixture was created successfully !';
  SPower='Power';
  SWatt='Watt';

  SVolume='Volume';
  SPan='Pan';
  SLeft='LEFT';
  SRight='RIGHT';
  SCenter='CENTER';
  SCenter_='Center';
  SPitch='Pitch';
  SCurve='CURVE';
  SCurve_='Curve';
  SVelocity='Velocity';
  SReset='RESET';
  SAudioFileNotFound='Audio file not found !!';
  SAnErrorOccurredWhileImportingTheFileToTheProject='An error occurred while importing the file to the project !!';
  SDisqueIsFullOrWriteProtected='Disk is full or write protected.';
  SSelectedItemWillBeRemovedFromProject='The selected items will be removed from the project. Continue ?';
  SUnknowCommand='UNKNOW COMMAND !!';
  SArgumentOutOfRange='Argument out of range !!';
  SUnknowSequence='UNKNOW SEQUENCE !!';
  SUniverse='Universe';
  SFixture='Fixture';
  SFixtures='fixtures';
  SAdress='Adress';
  SFile='File';
  SNotFound='NOT FOUND !!';
  SDMXFixture='DMX FIXTURE AT';
  SDMXChannel='DMX CHANNEL AT';
  SDMXLibrary='DMX library';
  SMoveTheFolder='Move the folder';
  SMoveTheFile='Move the file';
  SNameOfTheNewFolder='Name of the new folder :';
  SEnableToCreateTheNewFolder='Unable to create the new folder';
  SWarning='Warning';
  SYouWillLoseAllTheContentOfTheFolder='YOU WILL LOSE ALL THE CONTENTS OF THE FOLDER';
  SFailedToDeleteTheFolder='Failed to delete the folder';
  SFailedToRenameTheFolder='Failed to rename the folder';
  SYouAreAboutToDelete='You are about to delete';
  SFailedToRemoveTheFixture='Failed to remove the fixture';
  SFailedToRenameTheFixture='Failed to rename the fixture';
  SFixtureFileAlreadyExists='There is already a fixture with the same name. The new one is renamed as';
  SOverwritesPreviousDataFor='Overwrites previous data for';
  SNewNameFor='New name for';
  SNotDefined='not defined';
  SAnErrorOccuredWhileCreatingTheManufacturerFolderInTheDMXLibrary='An error occured while creating the manufacturer folder in the dmx library !!';
  SAnErrorOccuredWhileSaving='An error occured while saving';
  SFileIsNotAProject='This file is not a SAYNÈTE project';
  SFailToLoadTheProject='Fail to load the project';
  SFailToSaveTheProject='Fail to save the project';
  SErrorFoundInProjectSequences='Error(s) found in the project sequences.'+LineEnding+'Please check';
  SAdressConflictInDMX='Adress conflict found in dmx universe.'+LineEnding+'Please check';
  SFailToCreateDirectoryNeededByTheProject='Fail to create the directory needed by the project';
  SPleaseNoSpaceOrSpecialCharacters='please no space or special characters';
  SPleaseNoSpecialCharacters='please no special characters';
  SProjectName='Project name';
  SProjectCreatedWithSuccess='Project created with success !';
  SOverwriteDMXConf='Your current DMX configuration will be overwrite';
  SDMXConfSuccessLoaded='DMX configuration successfully loaded';
  SErrorWhileImportingDMX='An error occurred while importing the DMX configuration from the project';
  SErrorLoadingPlaylist='Error while opening this playlist';
  SDMXGroupsSuccessLoaded='DMX groups successfully loaded';
  SErrorWhileImportingDMXGroups='An error occurred while importing the DMX groups from the project';
  SAskMoveFixtureOnAnotherUniverse='Are you sure to move this fixture on another universe ?';
  SLoadingAnotherDMXConfigurationFromAnotherProject='Importing the DMX configuration of another project will overwrite the current configuration.'+LineEnding+'Do you want to continue?';

  SNone='None';
  SRectangle='Rectangle';
  SQuare='Square';
  SHalfCircle='Half circle';
  SEllipse='Ellipse';
  SCustom1='Custom1';

  SSeats1='Seats1';
  SSeats2='Seats2';

  sRangesGenerator='Ranges generator';
  SSelection='Selection';
  SView='View';
  SBegin='Begin';
  SEnd='End';
  SRange='Range';
  SCount='Count';
  SFunctionality='Functionality';
  SPreview='Preview';
  SDuration='Duration';
  SModifyDuration='Modify duration';
  SAlign='Align';
  SSecond='second';
  SSeconds='seconds';
  SSeconds_='second(s)';
  SSec='sec';
  SHz='Hz';
  SOff='OFF';
  SOn='ON';
  Son_='on';
  SIn='in';
  SFrom='from';
  STo='to';
  SMini='Mini';
  SMaxi='Maxi';
  SColor='Color';
  SFade='Fade';
  SSelected='selected';
  SDeleteThisAction='Delete this action';
  SDeleteSelected='Delete the selection';
  SReady='Ready';

  SWait='WAIT';
  SBackToBegin='BACK TO THE BEGINNING';
  SStartSequence='START';
  SStopSequence='STOP';
  SStretchTime='STRETCH TIME';
  SCoef='Coef';
  SSlower='slower';
  SFaster='faster';
  SNormal='normal';
  SNormal_='Normal';
  SValue='Value';
  SDurationInSecond='Duration in second';

  SAudioPlay='AUDIO - PLAY';
  SAudioStop='AUDIO - STOP';
  SAudioPause='AUDIO - PAUSE';
  SAudioFadeIn='AUDIO - FADEIN';
  SAudioFadeOut='AUDIO - FADEOUT';
  SAudioSetVolume='AUDIO - SET VOLUME';
  SAudioSetPan='AUDIO - SET PAN';
  SAudioSetFreq='AUDIO - SET PITCH';
  SAudioConnectEffect='AUDIO - CONNECT';
  SAudioDisconnectEffect='AUDIO - DISCONNECT EFFECT';
  SAudioCaptureStart='AUDIO CAPTURE - START';
  SAudioCaptureStop='AUDIO CAPTURE - STOP';
  SAudioCaptureSetVolume='AUDIO CAPTURE - SET VOLUME';
  SAudioCaptureSetPan='AUDIO CAPTURE - SET PAN';
  SAudioCaptureConnectEffect='AUDIO CAPTURE - CONNECT EFFECT';
  SAudioCaptureDisconnectEffect='AUDIO CAPTURE - DISCONNECT EFFECT';
  SDryWet='Dry/Wet';
  SDry_='DRY';
  SWet_='WET';
  SToRight='to RIGHT';
  SToLeft='to LEFT';
  SToCenter='to CENTER';
  SPreAmp='Pre-amp';
  SAudioCapture_='AUDIO CAPTURE';
  SFiles_='FILES';
  SFX='FX';

  SUnknownEffect='!! UNKNOWN EFFECT !!';
  SUnknownPreset='!! UNKNOWN PRESET !!';

  SPreset='Preset';
  SPreset_='PRESET';
  SColorPresets='Color presets';
  SFlamePresets='Flame presets';
  SAudioFollowerPresets='Audio Follower presets';
  SAudioFXPresets='Audio Effect presets';
  SRepetitiveChannelPresets='Repetitive channel presets';
  SReplacePresetData='Replace the data of this preset ?';
  SDeleteThisPreset='Delete this preset ?';
  SPresetButtonHint='With the presets, you can save your settings'+lineending+'to use them later';


  SHideEffects='Hide effects';
  SShowEffects='Show effects';

{  SChannelMasterDimmer='Master Dimmer';
//  SChannelDimmer='Dimmer';
  SChannelRed='Red';
  SChannelGreen='Green';
  SChannelBlue='Blue';
  SChannelWhite='White';
  SChannelAmber='Amber';
  SChannelUV='Ultraviolet';
  SChannelConfig='Config';
  SChannelStrobe='Strobe';
  SChannelPan='Pan';
  SChannelTilt='Tilt';
  SChannelSpeedPanTilt='Speed Pan/Tilt';
  SChannelGobo='Gobo';
  SChannelRotationGobo='Rotation Gobo';
  SChannelColorChanger='Color Changer';
  SChannelSpeed='Speed';
  SChannelNoFunction='No function';
  SChannelCyan='Cyan';
  SChannelMagenta='Magenta';
  SChannelYellow='Yellow';
  SChannelLime='Lime';
  SChannelIndigo='Indigo';
  SChannelWarmWhite='Warm white';
  SChannelColdWhite='Cold white';
  SChannelIris='Iris';
  SChannelBladeInsertion='Blade insertion';
  SChannelColorTemperature='Color temperature';
  SChannelStrobeSpeed='Strobe speed';
  SChannelSoundSensitivity='Sound sensitivity';
  SChannelBladRotation='Blade rotation';
  SChannelZoom='Zoom';
  SChannelFocus='Focus';
  SChannelRotation='Rotation';  }


  SDimmer='Dimmer';
  SFlame='Flame';
  SChaser='Chaser';
  SAudioFollower='Audio'+LineEnding+'Follower';
//  SAudioFollow='Audio';
//  SFlameRGB='Flame RGB';
  SFlash='Flash';
  SCopy='Copy';
  SCopy_='copy';
//  SCopyRGB='Copy RGB';
  SUnknown='Unknown';
  SError='Error';
  SSerialNumber='Serial number';
  SDevicePort='Port';
  SDevicePortIn='IN';
  SDevicePortOut='OUT';
  SNoDeviceFound='No device found...';
  SFound='Found';
  SDevices='devices';

  SOneShoot='ONE SHOOT';
  SLoopForward='LOOP FORWARD';
  SLoopBackward='LOOP BACKWARD';
  SPingPong='PING PONG';
  SMode1='MODE 1';
  SMode2='MODE 2';

  SDMXDimmer='DMX - DIMMER';
  SDMXWave='DMX - WAVE';
  SDMXFlame='DMX - FLAME';
  SDMXAudioFollower='DMX - AUDIO FOLLOWER';
  SDMXStopEffect='DMX - STOP EFFECT';
  SDMXCopy='DMX - COPY';
  SDMXChaser='DMX - CHASER';
  SDMXFadeFlame='DMX - FLAME + FADEIN';
  SDMXFlash='DMX - FLASH';
  SDMXDimmerRGB='DMX - DIMMER RGB';
  SDMXFlameRGB='DMX - FLAME RGB';
  SDMXAudioFollowerRGB='DMX - AUDIO FOLLOWER RGB';
  SDMXStopEffectRGB='DMX - STOP EFFECT RGB';
  SDMXFlameRGBFade='DMX - FLAME RGB + FADEIN';
  SDMXChaserRGB='DMX - CHASER RGB';
  SDMXCopyRGB='DMX - COPY RGB';
  SDMXFlashRGB='DMX - FLASH RGB';
  SDMXWaveRGB='DMX - WAVE RGB';
  SCrackling='Crackling';
  SDimmerRGB='Dimmer RGB';
  SRGBColorFrom='RGB color from';
  SSimple='Simple';
  SWave='Wave';
  SThen='then';
  SClickToCaptureCurrentColor='Click to capture the current color';
  SFollow='Follow';
  SGain='Gain';
  SBrightnessMax='Brightness max';
  SSoftenOn='Soften on';
  SSoften='Soften';
  SWaitTime='Wait time';
  SLevels='Levels';
  SLevelMin='Level min';
  SLevelMax='Level max';
  SMin='Min';
  SMax='Max';
  SLevel='Level';
  SSpeed='Speed';
  SAmplitude='Amplitude';
  SSubstract='Substract';
  SMultiple='multiple';
  SSeveral='several';
  SRandomValue='Random value';
  SRandomIntensity='Random intensity';
  SRandomDuration='Random duration';
  SFixedValue='Fixed value';
  SRandomValueBetween='Random value between';
  SFixedDuration='Fixed duration';
  SRandomDurationBetween='Random duration between';
  SFixedIntensity='Fixed intensity';
  SRandomIntensityBetween='Random intensity between';

  SScreen='SCREEN';
  SFinalOpacity='Final opacity';
  SOpacity='Opacity';
  SImage='Image';
  SZoom='Zoom';
  SPhoto='PHOTO';
  SFadeOutIn='Fadeout in';
  SVideo='Video';
  SPlay='Play';
  SStop='Stop';
  SLoop='Loop';
  SPaused='paused';
  SPause='Pause';
  SResume='Resume';
  SInLoop='loop mode';
  SPlaying='playing';
  SStopped='stopped';
  SCapture='Capture';

  SAudioFiles = 'Audio files';
  SAllFiles = 'All files';

  SPlaylist='Playlist';
  SCannotSavePlaylist='Cannot save the playlist';
  SPlaylistSavedSuccess='Playlist saved successfully';
  SNameForThePlaylist='Name for the playlist';
  SAskDeletePlaylist='Delete this playlist ?';

  SSetVolumeTo='Set volume to';
  SChangeVolumeTo='Change volume to';
  SJumpToPosition='Jump to position';

  SUnknownAction='Unknown action !!';
  SUnknownAudio='Unknown audio !!';

  SNotConnected='NOT CONNECTED';

  SCanNotOpenTheFile='Can not open the file';
  STheFileIsCorrupted='The file is corrupted...';

  SCreateNew='Create new';
  SNameAlreadyUsed='name already used';
  SName='Name';
  SAskDeleteThisLink='Delete this link ?';
  SAskToOverwriteFixtureFilename='A fixture with this filename already exists, do you want to overwrite it ?'+
                                 '(its content will be lost)';
  SThisFilenamAlreadyExists='this filename already exists !';
  SThisFilenameIsUsedBySaynetes='this filename is used by Saynètes !';
  SSaveTheChangeBeforeQuit='Save the change before quit?';
  SUnableToLoadFixtureData='Unable to load fixture data...';
  SOr='or';
  SEditSwitcher='Edit switcher';
  SAddVirtualChannelToMode='Add virtual channel to mode';

// error message for sequences
  SUnrecognizedAction='unrecognized action';
  SWrongNumberOfParameter='wrong number of parameters';
  SAudioNotFound='Audio not found';
  SBadVolumeValue='bad volume value';
  SBadPanValue='bad pan value';
  SBadPitchValue='bad pitch value';
  SDurationIsNegative='duration have negative value';
  SBadVelocityCurveValue='bad velocity curve value';
  SBadDryWetValue='bad dry/wet value';
  SBadAudioEffectCountValue='effect count is not equal to 1, 2 or 3';
  SBadEffectTypeValue='bad effect type value';
  SBadAudioPresetIndex='bad preset index';
  SBadStretchTimeValue='bad stretch time value';
  SUniverseNotFound='universe not found';
  SFixtureNotFound='fixture not found';
  SChannelNotFound='channel not found';
  SBadPercentValue='percentage error';
  SBadMinPercent='bad percentage min value';
  SBadMaxPercent='bad percentage max value';
  SBadWaitTime='bad wait time value';
  SBadSoften='bad soften value';
  SBadColorValue='bad color value';
  SBadAmplitude='bad amplitude';
  SBadGain='bad gain value';
  SBadBrightness='bad max brightness value';
  SBadLevelMin='bad min level value';
  SBadLevelMax='bad max level value';
  SBadDurationMin='bad min duration value';
  SBadDurationMax='bad max duration value';

  SUnableToCreateManufacturerFolder='Unable to create the manufacturer folder';
  SUnableToSaveTheFixture='Unable to save the fixture...';
  SUnableToDeleteTheProject='Unable to delete the project...';

// fixture name
  SOther='Other';
  SPlanConvex='PC';
  SParShortBulb='Short PAR with bulb';
  SHalogen='Halogen';
  SParLongTransparentLed='Long PAR transparent LED';
  SBarColoredLed='Bar colored LED';
  SProfile='Profile';
  SColorChanger='Color changer';
  SScanner='Scanner';
  SMovingHead='Moving head';
  SSmokeMachine='Smoke machine';
  SBubbleMachine='Bubble machine';
  SDimmer1Channel='Dimmer 1 channel';
  SDimmer4Channels='Dimmer 4 channels';
  SMatrixTransparentLed='Matrix transparent LED';
  SMatrixColoredLed='Matrix colored LED';
  SParShortTransparentLed='Short PAR transparent LED';
  SLedParLongColoredLed='Long PAR colored LED';
  SLedParShortColoredLed='Short PAR colored LED';
  SLedBarTransparentLed='Bar transparent LED';
  SParLongBulb='Long PAR with bulb';
  SFan='Fan';
  SLaser='Laser';
  SSmallPar='Small PAR';
  SFlower01='Flower 1';
  SSquareParSingleTransparentLed='Square PAR with single transparent LED';
  SSquareParMultipleTransparentLed='Square PAR with multiple transparent LED';
  SquareParMultipleColoredLed='Square PAR with multiple colored LED';
  SParRectangularMultipleTransparentLed='Rectangular PAR with multiple transparent LED';
  SParRectangularMultipleColoredLed='Rectangular PAR with multiple colored LED';
  SBarShortTransparentLed='Short bar transparent LED';
  SBarShortColoredLed='Short bar colored LED';
  SFlower02='Flower 2';
  SFlower03='Flower 3';
  SStand01='Stand x2';
  SStand02='Stand x3';
  SStand03='Stand x4';
  SBarShortx2TransparentLed='Short bar with multiple rows and transparent LED';
  SBarShortx2ColoredLed='Short bar with multiple rows and colored LED';
  SBarx2TransparentLed='Long bar with multiple rows and transparent LED';
  SBarx2ColoredLed='Long bar with multiple rows and colored LED';
  SBarMediumTransparentLed='Medium bar transparent LED';
  SBarx2MediumTransparentLed='Medium bar with multiple rows and transparent LED';
  SBarMediumColoredLed='Medium bar colored LED';
  SBarx2MediumColoredLed='Medium bar with multiple rows and colored LED';


  SHelp='Help';
  HelpIntermissionMusic='This module allow you to control the intermission music.'+LineEnding+
                        'Left click on the list to select a playlist.'+LineEnding+
                        'Click ON/OFF button to start/pause the selected playlist.'+LineEnding+
                        'Use green arrow to jump to the previous/next music in the playlist.'+LineEnding+
                        'Right click on the list to show pop-up menu.';

  HelpIntermissionCreatePlaylist='Here you can create or modify a playlist for the intermission music.'+LineEnding+
               'NOTE: playlist are only a list of path to musics on the disk, musics are not copied.'+LineEnding+
               'Use Add buttons for add an item to the playlist.'+LineEnding+
               'Delete to delete an item.'+LineEnding+
               'Arrow to move up or down an item.'+LineEnding+
               'When the playlist is done, enter its name then click Save.';

  HelpProjectList='This is the list of projects found in the selected working folder.'+LineEnding+
                  'Projects saved in a same folder share the same DMX configuration, and loading a project from here don''t affect the current lighting. It is usefull feature e.g. for festivals where different shows are performed one after the other.'+LineEnding+
                   '- Select a folder.'+LineEnding+
                   '- Select a project then click Open button, or double click on a project to open it.';


  HelpAudioCapture='This module allow you to playback audio captured from a microphone or line-in.'+LineEnding+
                   'Click ON/OFF button to start/stop the module.'+LineEnding+
                   'Pre-amp is the pre-amplification to adjust the input level.'+LineEnding+
                   'Sets the pan with the pan cursor.'+LineEnding+
                   'Sets the output volume with the volume cursor.'+LineEnding+
                   'Use FX button to add audio effect to the captured audio.';

  HelpSoundControl='This module allow you to show and control parameters for the selected audio file.'+LineEnding+
                   'Select an audio in the list to see its parameters.'+LineEnding+
                   'Parameters can be modified by a sequence or manually with the mouse.';

  HelpSoundList='This is the list of audio files used by your project.'+LineEnding+
                'Audio can be played/stopped by a sequence.'+LineEnding+
                'SPACE to manually start/stop the selected audio: mouse must be over the audio list.'+LineEnding+
                'Button ''+'' to add audio to your project. A window appear to select a file on the disk. Saynètes make a copy of the original audio file and insert it in the project.'+LineEnding+
                'Right click on an existing audio to show pop-up menu.';

  HelpSequenceList='This is the list of the sequences.'+LineEnding+
                   'SELECT a sequence with left click.'+LineEnding+
                   'SPACE to start it: mouse must be over the sequence list.'+LineEnding+
                   'Right click to show pop-up menu.'+LineEnding+
                   'You can drag a sequence up or down to change its position in the list.';

  HelpImportSequence='This tool allow you to import sequences from another project.'+LineEnding+
                   '- Click the button at top left to open the project from which you are going to import.'+LineEnding+
                   '- Checks the sequences you want to import.'+LineEnding+
                   '- Click Import button.'+LineEnding+
                   'WARNING: Because Saynètes reference sequences, sounds and fixtures by ID number and not by their names, import sequence that have action with such reference (start a sequence, play a sound,...) may not work as expected.'+LineEnding+
                   'REMEDY: When you import a sequence from another project, check it.'+LineEnding+
                   'NOTE: Projects located in a same folder, share their dmx configuration. So, there is no problem importing a sequence containing only dmx actions from such a project.';

  HelpViewProjector='This is the projector view: it represents the stage, the seats for the spectators and the dmx fixtures used in you project. The Stage and seats shape can be changed from the program option window.'+LineEnding+
                    'MOVE THE VIEW with right click on empty area and move the mouse.'+LineEnding+
                    'ZOOM with mouse  wheel.'+LineEnding+
                    'EDIT the dmx fixtures by clicking on the button at the top right.'+LineEnding+
                    'SELECT a fixture with left click.'+LineEnding+
                    'SELECT SEVERAL FIXTURES at once by dragging a rectangle around them.'+LineEnding+
                    'ADD a DESCRIPTION to your fixtures: activate the button ''Show/Hide the fixture information panel'' and select a fixture. In the panel that appears, enter the description.'+LineEnding+
                    'Right click on a fixture to show pop-up menu.'+LineEnding+
                    '    - Create rgb group: groups created with this, can be quickly re-selected from the group tool window.'+LineEnding+
                    '    - Lock: the fixture channels can not be changed.'+LineEnding+
                    '    - Unlock: the fixture channels can be changed.';

  HelpViewCursor='This is the channel view: it represents the channels of the selected fixtures in the projector view.'+LineEnding+
                 'ZOOM the view with mouse wheel.'+LineEnding+
                 'MOVE the view:'+LineEnding+
                 '    - with the scroll bar at the bottom.'+LineEnding+
                 '    - keep the middle mouse button pressed and move the mouse to the left or right.'+LineEnding+
                 'MOVE CURSOR by dragging them. NOTE: moving manually a cursor cancel the current effect.'+LineEnding+
                 'SELECT/UNSELECT a channel by clicking on the panel name at the bottom.'+LineEnding+
                 'Right click on the panel name to show pop-up menu:'+LineEnding+
                 '    - Several tools to select channels.'+LineEnding+
                 '    - Create channels group: groups created with this, can be quickly re-selected from the group tool window.'+LineEnding+
                 '    - Lock: the selected channels can not be changed.'+LineEnding+
                 '    - Unlock: the selected channels can be changed.';

  HelpEditionDMX='You are in the DMX editing mode.'+LineEnding+
                 'EXIT this mode by clicking the ''Done'' button.'+LineEnding+
                 'At the top you have the list of DMX universes and tools for:'+LineEnding+
                 '    - add a new universe.'+LineEnding+
                 '    - delete an universe.'+LineEnding+
                 '    - connect an universe to a device.'+LineEnding+
                 '    - you can also load the DMX configuration from another project.'+LineEnding+
                 'At the middle, there is the DMX fixtures library: retrieve your fixture by selecting a manufacturer, the fixture name, then select a mode name.'+LineEnding+
                 'ADD a fixture to your project: when you have selected a fixture name and a mode name, drag this mode name into the projector view and release the mouse button. You can now add as many time as you want the fixture around the stage with mouse left click.'+LineEnding+
                 'DELETE a fixture: right click to show the pop-up menu, then Delete.'+LineEnding+
                 'SELECT a fixture with left click.'+LineEnding+
                 'MOVE a fixture: select it then drag it where you want.'+LineEnding+
                 'Right click on a fixture to show pop-up menu:'+LineEnding+
                 '    - Rotation to change its direction.'+LineEnding+
                 '    - Zoom to change its size.'+LineEnding+
                 '    - Horizontal flip.'+LineEnding+
                 '    - Vertical flip.'+LineEnding+
                 'CHANGE DMX ADRESS of a fixture: activate the button ''Show/Hide the fixture information panel'' and select a fixture. In the panel that appears, change the adress value. If an address overlap is detected, a red exclamation mark will blink at top right of the screen.';

  HelpDMXGroup='Retrieve here the groups (channels and RGB) you have previously defined.'+LineEnding+
               'CHOOSE the type of group you want to recall by clicking Channels or RGB button, then select an item in the list.'+LineEnding+
               'ADD a group to the current selection by holding down the SHIFT key.'+LineEnding+
               'Right click on item of the list to:'+LineEnding+
               '    - RENAME the selected group.'+LineEnding+
               '    - DELETE the selected group.';

  HelpChannelSimpleDimmer='This is a basic dimmer effect applyed to the selected channels.'+LineEnding+
                          'Used in a sequence, this effect change the current value of a channel to the new one with respect of the duration and the velocity curve.'+LineEnding+
                          'Final values of the dimmer are those that you sets on the channels view.'+LineEnding+
                          'Duration can be equal to zero, in that case the dimmer will jump immediatly to the value.'+LineEnding+
                          'This effect have no effect in live and should only be used when creating a sequence.';

  HelpChannelWaveDimmer='A double dimmer effect.'+LineEnding+
                        'This effect change the values of the selected channels (in the channel view) to a first level 1, with respect of the duration 1 and velocity curve 1, then change them to level 2 with respect of the duration 2 and velocity curve 2.'+LineEnding+
                        'Duration can be equal to zero, in that case the dimmer will jump immediatly to the value.';

  _HelpChannelFlame='This effect simulates the flickering of a flame on the selected channels.'+LineEnding+
                   'LEVELS: controls the flickering range.'+LineEnding+
                   'WAIT TIME: controls the effect speed.'+LineEnding+
                   'SOFTEN: controls the smoothness of the effect.';


  HelpContinuousEffect='This effect is continuous, it stops only when you move the channel cursor manually or if a sequence apply another effect on the channel.';
  HelpPreset='You can save the setting in a preset: click the PRESET button, click Add, enter a name for the setting.'+LineEnding+
             'To recall a preset, click on the PRESET button then click on its name.';

  _HelpChannelAudioFollower='This effect sets the intensity of the selected channels proportionally to an audio level.'+LineEnding+
                   'In the list, select the audio to follow.'+LineEnding+
                   'Clic Play to start the playback'+LineEnding+
                   'GAIN: controls the amplitude of variations.'+LineEnding+
                   'BRIGHTNESS MAX: define the maximum value not to be exceeded.'+LineEnding+
                   'SOFTEN: controls the smoothness of variations.';

  HelpChannelCopy='This effect copies the value of one channel to one or more other channels.'+LineEnding+
                  'In the channels list, select the source.';

  HelpChannelFlash='Flash briefly applies a value to the channels, holds it for a while and then restores the previous values.'+LineEnding+
                   'This effect can be used at the same time as another.'+LineEnding+
                   'FIXED VALUE: sets the value to apply briefly.'+LineEnding+
                   'RANDOM VALUE: define an interval for the calculation of the random values.'+LineEnding+
                   'FIXED DURATION: controls holding time, here always the same.'+LineEnding+
                   'RANDOM DURATION: define an interval of time for the calculation of the random duration.'+LineEnding+
                   'Clic the preview button to see the result.';

  HelpChannelStop='Stops the current effect on selected channels.'+LineEnding+
                  'Don''t stop a chaser because a chaser is not an effect but a sequence.';

  HelpChannelChaser='This effect is not really an effect, because it creates a sequence containing the required actions.';

  HelpRGBSimpleDimmer='A simple dimmer to apply RGB color to a fixture.'+LineEnding+
         'Used in a sequence, this effect change the current RGB fixture color to the new one with respect of the duration and the velocity curve.'+LineEnding+
         'Duration can be equal to zero, in that case the dimmer will jump immediatly to the color.'+LineEnding+
         'This effect should only be used when creating a sequence.';

  HelpRGBWaveDimmer='A double RGB dimmer effect.'+LineEnding+
                    'This effect change the current color of the selected fixtures to color 1, with respect of the duration 1 and velocity curve 1, then change them to color 2 with respect of the duration 2 and velocity curve 2.'+LineEnding+
                    'Duration can be equal to zero, in that case the dimmer will jump immediatly to the color.';

  HelpRGBFlame='This effect simulates the flickering of a flame on the selected fixture.'+LineEnding+
               'WAIT TIME: controls the effect speed.'+LineEnding+
               'AMPLITUDE: controls the amplitude of the flickering.'+LineEnding+
               'SOFTEN: controls the smoothness of the effect.';

  HelpRGBAudioFollower='This effect sets the intensity of a RGB color on the selected fixtures proportionally to an audio level.'+LineEnding+
         'Select a color.'+LineEnding+
         'In the list, select the audio to follow.'+LineEnding+
         'Clic Play to start the playback'+LineEnding+
         'GAIN: controls the amplitude of variations.'+LineEnding+
         'SOFTEN: controls the smoothness of variations.';

  HelpRGBCopy='This effect copies the RGB color of one fixture to one or more other fixtures (that have RGB channel of course).'+LineEnding+
              'In the fixture list, select the source.';

  HelpRGBChaser='This effect is not really an effect, because it creates a sequence containing the required actions.';

  HelpRGBStop='Stops the current RGB effect on selected fixtures.'+LineEnding+
              'Don''t stop an RGB chaser because a chaser is not an effect but a sequence.';

  HelpSequencer='The sequencer is the heart of Saynètes: this is where you define the sequences to build your show.'+LineEnding+
              'A sequence is made up of steps. Steps have a title to describe them and are displayed on the time line.'+LineEnding+
              'Steps are made up of actions of type audio, DMX or other. Actions contained in one step are displayed in the panel at top left of the screen.'+LineEnding+
              'Place steps on the time line to construct the sequence.'+LineEnding+
              LineEnding+
              'THE VIEW'+LineEnding+
              '  - ZOOM with mouse wheel.'+LineEnding+
              '  - MOVE it with'+LineEnding+
              '      . The scroll bar at the bottom.'+LineEnding+
              '      . Holds the middle mouse button pressed and move the mouse to the left/right.'+LineEnding+
              LineEnding+
              'ADD STEP with right click on empty area of the time line and choose its type.'+LineEnding+
              LineEnding+
              'SELECTION'+LineEnding+
              '  - LEFT CLICK on a step to select it.'+LineEnding+
              '  - SHIFT + LEFT CLICK to add a step to the current selection.'+LineEnding+
              '  - LEFT CLICK on an empty area and drag the mouse to the right or left to select an area: all steps in this area will be selected.'+LineEnding+
              LineEnding+
              'MOVE STEP'+LineEnding+
              '  - HOLDS LEFT CLICK on the selected steps and drag them where you want on the time line.'+LineEnding+
              '  - UP/DOWN keys moves the selection vertically.'+LineEnding+
              '  - ALIGN: right click on one selected step and click Align in the pop-up menu.'+LineEnding+
              LineEnding+
              'KEYBOARD SHORTCUT'+LineEnding+
              '  - UP: moves the selection up one line.'+LineEnding+
              '  - DOWN: moves the selection down one line.'+LineEnding+
              '  - ALT A: zoom the view to see all steps.'+LineEnding+
              '  - ALT S: zoom on the selection area (in blue).'+LineEnding+
              '  - CTRL A: select all steps in the sequence.'+LineEnding+
              '  - DELETE: delete the selected steps.'+LineEnding+
              '  - CTRL G: group the selected steps.'+LineEnding+
              '  - CTRL U: ungroup the selected group of steps.'+LineEnding+
              '  - CTRL R: rename a step.'+LineEnding+
              '  - CTRL X: cut the selected steps in the clipboard.'+LineEnding+
              '  - CTRL C: copy the selected steps in the clipboard.'+LineEnding+
              '  - CTRL V: paste the clipboard content at the mouse cursor position.'+LineEnding+
              '  - CTRL Z: undo.'+LineEnding+
              '  - CTRL SHIFT Z: redo.'+LineEnding+
              '  - SPACE: start the sequence preview from the mouse cursor position. If the mouse is outside the time line the sequence starts from the beginning.'+LineEnding+
              LineEnding+
              'CHANGING THE DURATION OF A STEP'+LineEnding+
              '  - right click on the selected steps, in the pop-up click Change duration.'+LineEnding+
              '  - Select one or several steps, then holds ALT key pressed and drag one step to the right to increase the duration, to the left to decrease it. Durations are changed proportionnaly to their initial values.'+LineEnding+
              LineEnding+
              'CHANGING TIME BETWEEN TWO STEPS'+LineEnding+
              'At the bottom of the time line where the time legend is displayed, right click on the interval between two steps: this interval is selected and a pop-up menu appears.'+LineEnding+
              LineEnding+
              'DUPLICATE THE SELECTED STEPS'+LineEnding+
              'Keep CTRL pressed and drag one of the selected steps.';

   HelpSequencerCmdList='A step is made of actions and this panel displays them.'+LineEnding+
               'You can modify the action: right click on the action to show pop-up menu:'+LineEnding+
               '  - Edit: to edit the parameters of this action. Not all actions are editable.'+LineEnding+
               '  - Delete to delete the selected actions.';

   HelpSequencerAlignSteps='Align several steps with one click.'+LineEnding+
               'The step you right-clicked becomes the reference step. It is shown at the top.'+LineEnding+
               'The hatched rectangles show where the other steps will be moved.'+LineEnding+
               'Just click one of the hatched rectangle to perform the alignment.';


   HelpSequencerCmdAudio='This is the list of actions you can perform on audio file or on the Capture Module.'+LineEnding+
               '1 - First choose the target: it can be a file or the Capture Module.'+LineEnding+
               '2 - Choose the action you want to perform on the target.'+LineEnding+
               '3 - If any, sets the parameters as you want.'+LineEnding+
               '4 - Click Add button to add the step to the sequence.'+LineEnding+
               LineEnding+
               'NOTES:'+LineEnding+
               '  - If a sound is paused, you can resume it with PAUSE or PLAY.'+LineEnding+
               '  - FadeOut lower the volume until 0% then stop the sound.';


   _HelpSequencerCmdAudioFX='Here you can apply nice effects to your sounds or to the Capture Module.'+LineEnding+
               '1 - First choose the target: it can be a file or the Capture Module.'+LineEnding+
               '2 - Choose the action:'+LineEnding+
               '     .CONNECT: to apply effects to the target.'+LineEnding+
               '     .DISCONNECT: to remove current effects that you have previously applyed to the target.'+LineEnding+
               '3 - Activate up to 3 ALS-fx (module effect) simultaneously.'+LineEnding+
               '4 - On each ALS-fx module activated, select the type of effect then a preset.'+LineEnding+
               '5 - Sets the Dry/Wet: it adjusts the proportion of original sound with the transformed one.'+LineEnding+
               LineEnding+
               'NOTE: effects connected on a sound will be removed only when an action Disconnect will be performed on this sound.'+LineEnding+
               '      You can also remove effects manually from the audio panel on the main view.';


   HelpSequencerOtherStart='This action run another sequence.'+LineEnding+
               'The calling sequence is not stopped: both sequences run in parallel.'+LineEnding+
               'Choose the sequence to start, then click Add.';

   HelpSequencerOtherStop='This action stops the execution of another sequence.'+LineEnding+
               'Choose the sequence to stop, then click Add'+LineEnding+
               'NOTE: stops a sequence that is not running have no effect.';

   HelpSequencerOtherLoop='This action force the sequence to re-start from the beginning: the sequence is looped.'+LineEnding+
                          'All steps after this one on the time line will be ignored.'+LineEnding+
                          'Just click Add to create the step in the sequence.';

   HelpSequencerOtherStretch='This action speeds up or slows down the execution of another sequence.'+LineEnding+
                          'This feature is experimental...'+LineEnding+
                          '- Select the sequence to stretch.'+LineEnding+
                          '- Sets the wanted speed.'+LineEnding+
                          '- Sets the duration to reach this speed.'+LineEnding+
                          '- Select the velocity curve to use.'+LineEnding+
                          '- Click Add button to create the step in the sequence.';


   HelpDeviceManagerDevice='In this window you can connect the universes to dmx devices.'+LineEnding+
            '- At the bottom right:'+LineEnding+
            '   . Check which device you want to search then click the Search button.'+LineEnding+
            '   . Saynètes try to retrieve the available supported devices.'+LineEnding+
            '- At top right:'+LineEnding+
            '   . The list display the devices found.'+LineEnding+
            '   . A device can have multiple ports.'+LineEnding+
            '   . If the device allow it, you can set a port direction (IN or OUT).'+LineEnding+
            '- At the top left:'+LineEnding+
            '   . Click into ''Connected to'' column to connect an universe to a device.'+LineEnding+
            '   . Connection can be optimized: e.g. in an universe, only adresses from 1 to 120 are occupied. Checking ''Optimized'' will send only the 120 channel values to the device. Unchecking it will send the whole 512 channel values. NOTE: not all devices support this feature.';

   HelpDeviceManagerMonitoring='Here you can visualize the channel values sent or recieved by a device port.'+LineEnding+
            'Just select the device and its port in the list.'+LineEnding+
            'Move the mouse over the grid cells to show infos.';


   HelpFixtureWizardGeneral='Here, define the general characteristics of the fixture.'+LineEnding+
            '- Choose a manufacturer in the list.'+LineEnding+
            '- Enter the name of the fixture.'+LineEnding+
            '- Enter your name or pseudo (optionnal).'+LineEnding+
            '- Choose a graphic representation for the fixture.'+LineEnding+
            '- Its recommended to add at least a web link for the manual of the fixture: click on the ''+'' button at top right.';


   HelpFixtureWizardPhysical='Enter here the technical specifications of the fixture.'+LineEnding+
            'Power and Connector must be defined.'+LineEnding+
            'If the fixture have dip switch to select its DMX adress, check ''yes''.';


   HelpFixtureWizardDipSwitch='Dip switch definition:'+LineEnding+
            '- Click ''+'' to add a switch.'+LineEnding+
            '- Click ''-'' to remove a switch.'+LineEnding+
            '- Reverse the bit order or the ON/OFF position to match what you see on the fixture.'+LineEnding+
            '- Right click on a switch show a pop-up menu:'+LineEnding+
            '   . Used for adress: select it if the switch is intended to define the adress.'+LineEnding+
            '   . Must be ON: select it if this switch must be ON.'+LineEnding+
            '   . Must be OFF: select it if this switch must be OFF.';

   _HelpFixtureWizardMode='Define here the modes as they are defined in the fixture manual.'+LineEnding+
            '- Add a mode by clicking on ADD MODE button.'+LineEnding+
            '- Delete a mode by clicking on the red cross at the top right of the mode panel.'+LineEnding+
            LineEnding+
            '- Enter the mode name.'+LineEnding+
            '- Enter a short name for this mode (optionnal).'+LineEnding+
            '- Click on button:'+LineEnding+
            '   . Add channel: to add a new channel or a channel already defined in another mode.'+LineEnding+
            '   . Add virtual channel: to add a new virtual channel or a virtual channel already defined in another mode (see below).'+LineEnding+
            '   . Add multiple: to add several channels at once.';

   HelpVirtualChannel='VIRTUAL CHANNEL, SUB-CHANNEL and SWITCHER'+LineEnding+
            'Virtual channel is a channel whose the definition change according to the value of another channel (the switcher).'+LineEnding+
            'Example: a fixture have a channel A named Program and a channel B whose the name can be:'+LineEnding+
            '     - Program Speed if channel A value is between 1..127.'+LineEnding+
            '     - Sound Sensitivity if channel A value is between 128..255.'+LineEnding+
            'To define this kind of complex channels in Saynètes, we have to define:'+LineEnding+
            '   - a virtual channel named ''ProgramSpeed/SoundSensitivity'' that contains:'+LineEnding+
            '     . a sub-channel named ''Program Speed''.'+LineEnding+
            '     . a sub-channel named ''Sound Sensitivity''.'+LineEnding+
            '   - switchers in the channel A definition like:'+LineEnding+
            '     . range 0..127: a switcher that switch the virtual channel ''ProgramSpeed/SoundSensitivity'' to the sub-channel ''Program Speed''.'+LineEnding+
            '     . range 128..255: a switcher that switch the virtual channel ''ProgramSpeed/SoundSensitivity'' to the sub-channel ''Sound Sensitivity''.'+LineEnding+
            'NOTE:'+LineEnding+
            '  . The name of a virtual channel is never displayed on the main dmx view, only its sub-channels names are.'+LineEnding+
            '  . A simple example with one single switching channel: American DJ - Dotz Par - mode 9 channel.'+LineEnding+
            '  . A complex example with several switching channels: American DJ - Flat Par QA12 - mode 8 channel.';

   HelpFixtureWizardSave='Here you can check for errors in the fixture definition.'+LineEnding+
            'Saynètes will show messages to help you find them.'+LineEnding+
            'Click on Save button to save the definition in the library and close the window.';

   _HelpEditVirtualChannel='To create a new virtual channel:'+LineEnding+
            '- Enter its name (try to be explicit).'+LineEnding+
            '- Check its sub-channels in the list.'+LineEnding+
            '- If the sub-channels have not yet been defined, click on the button Create new.'+LineEnding+
            '- When all sub-channels are checked, click Ok.';

   _HelpDefineSwitcherItem='To add a switcher:'+LineEnding+
            '- At the left, select the virtual channel to switch. If it doesn''t appear its because you have to define it first.'+LineEnding+
            '- At the right, select the sub-channel to switch to.'+LineEnding+
            'NOTE: first you have to define a virtual channel before adding the switchers into the dmx range.';

   HelpRangesGenerator='Use the range generator to define a series of ranges separated by the same offset.'+LineEnding+
            'e.g. 0..9 Program 1, 10..19 Program 2, 20..29 program 3.'+LineEnding+
            'Begin value: this is the value to start from (in our example: 0).'+LineEnding+
            'Range width: this is the value that separates each range (in our example: 10).'+LineEnding+
            'Count: the range count to generate (in our example: 3).'+LineEnding+
            'Name: the description to generate for each range, # to insert the index.'+LineEnding+
            'Start index: sets the start index value (in our example: 1).'+LineEnding+
            'Check the preview and adjust if necessary.'+LineEnding+
            'Click Ok to generate the ranges.';

   HelpSelectSourceChannel='Select the channel from which you wish to import ranges, or the source channel to define the alias.';

   HelpSelectExistingChannel='Select an existing channel in the list.'+LineEnding+
            'If the channel is not yet defined, click Create new button.';

   HelpEditMultipleChannel='This tool help you to define quickly redundant channels. It use presets to save and retrieve the channel pattern.'+LineEnding+
            'PATTERN'+LineEnding+
            '  - Recall a preset by clicking on the Preset button.'+LineEnding+
            '  - If no preset match your need:'+LineEnding+
            '     . Select in the list the type of the channel you want to add to the pattern.'+LineEnding+
            '     . Click ''+'' button to add the selected.'+LineEnding+
            '     . Right click on the added item to rename it or to define it as an alias of another channel. This last function avoids repeating the same channel definition several times under different names.'+LineEnding+
            '  - Check Use prefix if you need a prefix to be inserted before the channel names.'+LineEnding+
            '  - Check Use suffix if you need to add a suffix after the channel names.'+LineEnding+
            '  - Sets the start and end index.'+LineEnding+
            '  - Choose an index option that fit your need.'+LineEnding+
            '  - Check the preview.'+LineEnding+
            '  - Click Ok to generate the channels.'+LineEnding+
            LineEnding+
            'TIP'+LineEnding+
            'It is common to define Red, Green and Blue channels, for example. instead to define them one by one, use this tool:'+LineEnding+
            '  - recall the preset RGB.'+LineEnding+
            '  - uncheck use prefix and suffix.'+LineEnding+
            '  - Check ''do not use index''.'+LineEnding+
            '  - Click Ok.';

   _HelpSelectExistingSwitchingChannel='Select an existing virtual channel.'+LineEnding+
            'if it hasn''t yet been defined, click on the button Create new.';

   _HelpDefineNewChannel='This window allow you to define a new channel.'+LineEnding+
            LineEnding+
            '- You want this channel to be an alias of another one:'+LineEnding+
            '   . Enter the name for this channel.'+LineEnding+
            '   . Click the Sets as an alias of... button.'+LineEnding+
            '   . Select a channel in the list.'+LineEnding+
            LineEnding+
            '- This new channel is not an alias:'+LineEnding+
            '   . Select a preset to quickly sets the channel parameters.'+LineEnding+
            '   . Or enter the channel name, and choose an icon.'+LineEnding+
            '   . Enter the default value of the channel.'+LineEnding+
            LineEnding+
            'RANGES'+LineEnding+
            'To define the ranges of the channel you can:'+LineEnding+
            '- Enter each range manually: sets the begin and the end of the range, enter a description, extra contain value like color in hexadecimal format #rrggbb.'+LineEnding+
            '- Use the ranges generator to help you to define repetitive items.'+LineEnding+
            '- Import the ranges from another channel by clicking Import ranges from another channel...'+LineEnding+
            LineEnding+
            'SWITCHERS'+LineEnding+
            'Define the switchers for this channel by clicking on:'+LineEnding+
            '  - ''+'' to add new switcher.'+LineEnding+
            '  - ''down arrow'' to copy the switcher on the previous range.'+LineEnding+
            'A switcher can be:'+LineEnding+
            '  - Removed: move the mouse over the switcher and click the red cross button at its right.'+LineEnding+
            '  - Modified: move the mouse over the switcher and click the pen button at its right.';

   _HelpDMXLibrary='This is the DMX library.'+LineEnding+
                  'Click the arrow to the left of a manufacturer name to see the list of its fixtures.'+LineEnding+
                  'Click the arrow to the left of a fixture name to see the list of the available modes.'+LineEnding+
                  'Select a mode to see the definition of its channels.'+LineEnding+
                  'Add a new fixture: click the button ''New fixture'''+LineEnding+
                  'Modify a fixture: click the button ''Edit...'''+LineEnding+
                  'Open the dmx library folder in the file explorer: click the button with the magnifier. You can first select a fixture to open its folder.'+LineEnding+
                  'A fixture definition is a file with the extension ''.dmx''';

   SParticipate='You can participate improving Saynètes by sending you DMX fixture definitions to lulutech@gmx.fr'+LineEnding+
                'They will be added in the next version of this software.'+LineEnding+
                'Thank you very much!';

  function HelpChannelFlame: string;
  function HelpChannelAudioFollower: string;
  function HelpSequencerCmdAudioFX: string;
  function HelpFixtureWizardMode: string;
  function HelpEditVirtualChannel: string;
  function HelpDefineSwitcherItem: string;
  function HelpSelectExistingSwitchingChannel: string;
  function HelpDefineNewChannel: string;
  function HelpDMXLibrary: string;

implementation

function HelpChannelFlame: string;
begin
  Result := _HelpChannelFlame + LineEnding + LineEnding +
            HelpContinuousEffect + LineEnding + LineEnding +
            HelpPreset;
end;

function HelpChannelAudioFollower: string;
begin
  Result := _HelpChannelAudioFollower + LineEnding + LineEnding +
            HelpContinuousEffect + LineEnding + LineEnding +
            HelpPreset;
end;

function HelpSequencerCmdAudioFX: string;
begin
  Result := _HelpSequencerCmdAudioFX + LineEnding + LineEnding +
            HelpPreset;
end;

function HelpFixtureWizardMode: string;
begin
  Result := _HelpFixtureWizardMode + LineEnding + LineEnding +
            HelpVirtualChannel;
end;

function HelpEditVirtualChannel: string;
begin
  Result := _HelpEditVirtualChannel + LineEnding + LineEnding +
            HelpVirtualChannel;
end;

function HelpDefineSwitcherItem: string;
begin
  Result := _HelpDefineSwitcherItem + LineEnding + LineEnding +
            HelpVirtualChannel;
end;

function HelpSelectExistingSwitchingChannel: string;
begin
  Result := _HelpSelectExistingSwitchingChannel + LineEnding + LineEnding +
            HelpVirtualChannel;
end;

function HelpDefineNewChannel: string;
begin
  Result := _HelpDefineNewChannel + LineEnding + LineEnding +
            HelpVirtualChannel;
end;

function HelpDMXLibrary: string;
begin
  Result := _HelpDMXLibrary + LineEnding + LineEnding +
           SParticipate;
end;


end.

