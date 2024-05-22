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
  u_list_top, frame_cmd_audio, frame_viewaudiolist, u_projectwizard,
  frame_viewtoplist, u_top_player, frame_viewdmxlibrary,
  frame_viewfixturechannels, u_editfixturewizard, u_dmxlib_inputrange,
  u_user_askconfirmation, u_user_showmessage, u_user_inputstring, u_userdialogs,
  frame_editstring, frame_viewdmxcursors, u_dmxtools_channels,
  u_notebook_util, u_dmxtools_rgb, frame_viewuniverselist,
  frame_viewchannelslist, u_dmxtools_group, u_edit_otheraction,
  frame_fx_channelchaser, u_helper, u_add_action_audio, u_devicemanager_form,
  u_ftdi_based, ftd2xx, u_startupwizard, u_program_options, u_createplaylist,
  frame_viewcolorlist, frame_fx_rgbchaser, frame_viewfixtureslist,
  u_presetmanager, frame_viewfixtureinfo, u_askifshiftadress, u_project_manager,
  frame_viewdmxdipswitchs, u_logfile, frame_cmd_audiofx, frame_trackbar,
  u_datamodule, frame_audiolevels, frame_intersessionmusic, frame_audiocapture,
  frame_viewprojectfolder, frame_led, frame_buttononoff, u_add_action_dmx,
  frame_viewprojectors, u_global_var, frame_main_audio, frame_main_sequence,
  frame_main_addfixture, u_project_options, project_util, form_about,
u_apputils, frame_viewfixtureimage, form_changefixtureimage,
u_edit_singleaction, frame_viewfixtureoverview, frame_editmode,
form_selectexistingchannel, form_definenewchannel, frame_cb_channeltype,
form_editweblink, frame_viewmodeitem, form_defineswitchingchannel,
form_selectexistingswitchingchannel, form_defineswitcheritem,
frame_view_switcheritem, frame_editrange, form_edit_repetitivechannel;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  DefaultFormatSettings.decimalSeparator := '.';
  Randomize();
  Application.Scaled:=True;
  Application.Initialize;

  Log:=TLog.Create(ConcatPaths([Application.Location, 'Saynetes.log']));
  Log.DeleteLogFile;
  Log.Info('Saynète: Starting application', 0, True);
  ProgramOptions := TProgramOptions.Create;
  ProgramOptions.Load;

  Log.AddEmptyLine;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormUserConfirmation, FormUserConfirmation);
  Application.CreateForm(TFormDMXRGBTools, FormDMXRGBTools);
  Application.CreateForm(TFormDMXChannelsTools, FormDMXChannelsTools);
  Application.CreateForm(TFormDMXGroup, FormDMXGroup);
  Application.CreateForm(TFormOtherAction, FormOtherAction);
  Application.CreateForm(TFormAudioAction, FormAudioAction);
  Application.CreateForm(TFormDeviceManager, FormDeviceManager);
  Application.CreateForm(TDataModule1, DataModule1);

  FProjectorViewToRefreshForThreadUniverse := FormMain.FrameViewProjector1;
  Application.Run;
end.

