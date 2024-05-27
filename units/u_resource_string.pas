unit u_resource_string;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;


resourcestring
  SProgramVersion='Program version';
  SWrittenBy='Written by';
  SCredits='Credits';
  SIconAppBy='Application icon by';

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

  SAddAction='Add action';
  SApplyTheModifications='Apply the modifications';
  SAction='Action';

  SAnErrorOccurredWhileRemovingTheFileFromDisk='An error occurred while removing the file from disk';

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
  sAddRepetitiveChannels='Add repetitive channels';

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
  SAudioConnectEffect='AUDIO - CONNECT EFFECT';
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
  SAudioFollow='Audio';
  SFlameRGB='Flame RGB';
  SFlash='Flash';
  SCopy='Copy';
  SCopy_='copy';
  SCopyRGB='Copy RGB';
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
  SCrackling='Crackling';
  SDimmerRGB='Dimmer RGB';
  SRGBColorFrom='RGB color from';
  SFollow='Follow';
  SGain='Gain';
  SBrightnessMax='Brightness max';
  SSoftenOn='Soften on';
  SSoften='Soften';
  SWaitTime='Wait time';
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
  SSaveTheChangeBeforeQuit='Save the change before quit?';
  SUnableToLoadFixtureData='Unable to load fixture data...';
  SOr='or';
  SEditSwitcher='Edit switcher';
  SAddVirtualChannelToMode='Add virtual channel to mode';

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

implementation
// exclusion i18n
{
TForm_ChangeStepLength.Label4.Caption
TForm_MoveStep.Label2.Caption
TFormTopEdition.Label1.Caption
TFormTopEdition.Label10.Caption
TFormTopEdition.Label11.Caption
TFormTopEdition.Label12.Caption
TFormTopEdition.Label16.Caption
TFormTopEdition.Label17.Caption
TFormTopEdition.Label6.Caption
FrameCmdAudio.lblVol.Caption
FrameCmdAudio.lblPan.Caption
FrameCmdAudio.lblPitch.Caption
TFormEditFixture.E1.Text
TFormEditFixture.Label11.Caption
TFormEditFixture.Label2.Caption
TFormFixtureWizard.E1.Text
TFormFixtureWizard.E2.Text
TFormFixtureWizard.E3.Text
TFormFixtureWizard.E4.Text
TFormFixtureWizard.Label2.Caption
TFormFixtureWizard.Label3.Caption
TFormFixtureWizard.Label15.Caption
TFormFixtureWizard.Label16.Caption
TFormFixtureWizard.Label9.Caption
TFormFixtureWizard.Label14.Caption
TFormOtherAction.Label11.Caption
TFormSequenceEdition.Label1.Caption
TFormSequenceEdition.Label2.Caption
TFormSequenceEdition.Label6.Caption
TFormSequenceEdition.Label10.Caption
TFormSequenceEdition.Label11.Caption
TFormSequenceEdition.Label12.Caption
TFormSequenceEdition.Label16.Caption
TFormSequenceEdition.Label17.Caption
TFormSequenceEdition.Label21.Caption
TFormUserConfirmation.Label1.Caption
TFormUserConfirmation.Caption
TFormUserInput.Caption
TFormUserInput.Edit1.Text
TFormUserInput.Label1.Caption
TFormUserMessage.Label1.Caption
TFormUserMessage.Caption
TFrameCmdAudio.lblfrequency.Caption
TFrameCmdAudio.lblpan.Caption
TFrameCmdAudio.lblvol.Caption
TFrameMainEntracte.Label1.Caption
TFrameMainEntracte.Label8.Caption
TFrameMainEntracte.Label9.Caption
TFrameFixtureInfo.Label1.Caption
TFrameFixtureInfo.Label2.Caption
TFrameFixtureInfo.Label3.Caption
TFrameFixtureInfo.Label4.Caption
TFrameFixtureInfo.Label5.Caption
TFrameFixtureInfo.Edit1.Text
TFrameFixtureInfo.Edit2.Text
TFrameFXChannelChaser.Label11.Caption
TFrameFXChannelChaser.Label13.Caption
TFrameMainAudio.lbl_pan.Caption
TFrameMainAudio.lbl_volume.Caption
TFrameMainAudio.lbl
TFrameViewDipSwitch.Label1.Caption
TFrameViewDipSwitchs.Label17.Caption
TFormDMXLibrary.lblfixturepower.Caption
TFormDMXLibrary.lblfixturename.Caption
TFormDMXChannelsTools.Label8.Caption
TFormDMXChannelsTools.Label9.Caption
TFormDMXChannelsTools.Label10.Caption
TFormDMXChannelsTools.Label11.Caption
TFormDMXChannelsTools.Label12.Caption
TFormDMXChannelsTools.Label13.Caption
TFormDMXChannelsTools.Label15.Caption
TFormDMXChannelsTools.Label19.Caption
TFormDMXChannelsTools.Label20.Caption
TFormDMXChannelsTools.Label21.Caption
TFormDMXRGBTools.Label8.Caption
TFormDMXRGBTools.Label10.Caption
TFormDMXRGBTools.Label12.Caption
TFormDMXRGBTools.Label13.Caption
TFormDMXRGBTools.Label15.Caption
TFormDMXRGBTools.Label19.Caption
TFormDMXRGBTools.Label20.Caption
TFormToolFixtureInfo.Caption
TFormToolFixtureInfo.Label1.Caption
TFormToolFixtureInfo.Label2.Caption
TFormToolFixtureInfo.Label3.Caption
TFormToolFixtureInfo.Label4.Caption
TFormToolFixtureInfo.Label5.Caption
TFormToolFixtureInfo.Edit1.Text
TFormToolFixtureInfo.Edit2.Text
TFormInputDMXRange.Caption
TFormPrepaAudio.Label6.Caption
TFormAskIfShiftAdress.Caption
TFormAskIfShiftAdress.Label1.Caption
TFormAskIfShiftAdress.SpeedButton1.Caption
TFormAskIfShiftAdress.SpeedButton2.Caption
}

end.

