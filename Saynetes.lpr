program Saynetes;

{$mode objfpc}{$H+}
{$define UseCThreads}
uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, SysUtils, u_mainform, u_audio_manager, u_edit_sequence, u_dmx_library,
  frame_sequencer, u_common, lazcontrols, frame_viewcmdlist, u_resource_string,
  u_list_sequence, frame_cmd_audio, frame_viewaudiolist, u_projectwizard,
  frame_viewsequencelist, u_sequence_player, frame_viewdmxlibrary,
  frame_viewfixturechannels, u_editfixturewizard, u_dmxlib_inputrange,
  u_user_askconfirmation, u_user_showmessage, u_user_inputstring, u_userdialogs,
  frame_editstring, frame_viewdmxcursors, u_dmxtools_channels,
  u_dmxtools_rgb, frame_viewuniverselist,
  frame_viewchannelslist, u_dmxtools_group, u_edit_otheraction,
  frame_fx_channelchaser, u_helper, u_add_action_audio, u_devicemanager_form,
  u_ftdi_based, ftd2xx, u_startupwizard, u_program_options, u_createplaylist,
  frame_viewcolorlist, frame_fx_rgbchaser, frame_viewfixtureslist,
  u_presetmanager, frame_viewfixtureinfo, u_askifshiftadress, u_project_manager,
  frame_viewdmxdipswitchs, u_logfile, frame_cmd_audiofx, frame_trackbar,
  u_datamodule, frame_audiolevels, frame_intermissionmusic, frame_audiocapture,
  frame_viewprojectfolder, frame_led, frame_buttononoff, u_add_action_dmx,
  frame_viewprojectors, u_global_var, frame_main_audio, frame_main_sequence,
  frame_main_addfixture, u_project_options, project_util, form_about,
u_apputils, frame_viewfixtureimage, form_changefixtureimage,
u_edit_singleaction, frame_viewfixtureoverview, frame_editmode,
form_selectexistingchannel, form_definenewchannel, frame_cb_channeltype,
form_editweblink, frame_viewmodeitem, form_definesvirtualchannel,
form_selectexistingvirtualchannel, form_defineswitcheritem,
frame_view_switcheritem, frame_editrange, form_edit_repetitivechannel,
form_selectsourcechannel, form_rangesgenerator, u_dmx_util, form_splash,
form_newmanufacturer, BGRABitmapTypes, frame_trackbar_customized, form_help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  DefaultFormatSettings.decimalSeparator := '.';
  Randomize();
  Application.Scaled:=True;
  Application.Initialize;

  // check if the application is portable.
  // This change the location of the config file, log file, user presets...
  CheckIfAppIsPortable;
  // check if the folder to save application configuration file, log file, etc...
  // exists. if not, try to create it.
  CheckAppConfigFolder;

  Log := TLog.Create(GetUserConfigFolder + 'saynetes.log');
  Log.DeleteLogFile;
  if ApplicationIsPortable then Log.Info('Saynètes '+APP_VERSION+' run in portable mode')
    else Log.Info('Saynètes '+APP_VERSION+' is installed on this computer');
  Log.Info('Running '+AppBitness+' application on '+OSName, 0, True);

  ProgramOptions := TProgramOptions.Create;
  ProgramOptions.Load;

  TFrameTrackBar.SetGlobalCursorColors(BGRA(167,202,253), BGRA(167,202,253,128));

  // create splash window
  FormSplash := TFormSplash.Create(Application);
  FormSplash.Show;
  FormSplash.Update;
  application.ProcessMessages;

  Log.AddEmptyLine;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TDataModule1, DataModule1);
  Application.CreateForm(TFormUserConfirmation, FormUserConfirmation);
  Application.CreateForm(TFormDMXRGBTools, FormDMXRGBTools);
  Application.CreateForm(TFormDMXChannelsTools, FormDMXChannelsTools);
  Application.CreateForm(TFormDMXGroup, FormDMXGroup);
  Application.CreateForm(TFormOtherAction, FormOtherAction);
  Application.CreateForm(TFormAudioAction, FormAudioAction);
  Application.CreateForm(TFormDeviceManager, FormDeviceManager);

  FProjectorViewToRefreshForThreadUniverse := FormMain.FrameViewProjector1;

  FormSplash.close;
  FormSplash.Release;
  Application.Run;

end.

