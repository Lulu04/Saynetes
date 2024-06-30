unit u_apputils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

// The following (sub)folders are located in the application binary folder
function GetAppDataFolder: string;
function GetAppDMXLibraryFolder: string;
function GetAppImagesFolder: string;
function GetAppFactoryPresetsFolder: string;
function GetAppStageImagesFolder: string;
function GetAppFixtureImagesFolder: string;
function GetAppCursorImagesFolder: string;
function GetAppChannelImagesFolder: string;
function GetAppIconImagesFolder: string;
function GetAppDMXEffectImagesFolder: string;


procedure CheckAppConfigFolder;
// The following (sub)folder are located in:
//   for Windows platform: ProgramData\Saynètes
//   for Linux platform: ~/.config
function GetUserConfigFolder: string;
function GetPlaylistsFolder: string;
function GetPresetsFolder: string;
function GetFileUserAudioPresets: string;

function GetDemoProjectFile: string;

implementation
uses Forms, u_common, project_util, utilitaire_fichier, Graphics;

var
  FAppConfigFolder: string='';

function GetAppDataFolder: string;
begin
  Result := IncludeTrailingPathDelimiter(Application.Location)+'Data'+DirectorySeparator;
end;

function GetAppImagesFolder: string;
begin
  Result := GetAppDataFolder+'Images'+DirectorySeparator;
end;

function GetAppDMXLibraryFolder: string;
begin
  Result := GetAppDataFolder+'DMXLibrary'+DirectorySeparator;
end;

function GetAppFactoryPresetsFolder: string;
begin
  Result := GetAppDataFolder+FACTORY_PRESETS_FOLDER+DirectorySeparator;
end;

function GetAppStageImagesFolder: string;
begin
  Result := GetAppImagesFolder+'StageShapes'+DirectorySeparator;
end;

function GetAppFixtureImagesFolder: string;
begin
  Result := GetAppImagesFolder+'Fixtures'+DirectorySeparator;
end;

function GetAppCursorImagesFolder: string;
begin
  Result := GetAppImagesFolder+'Cursors'+DirectorySeparator;
end;

function GetAppChannelImagesFolder: string;
begin
  Result := GetAppImagesFolder+'Channels'+DirectorySeparator;
end;

function GetAppIconImagesFolder: string;
begin
  Result := GetAppImagesFolder+'Icons'+DirectorySeparator;
end;

function GetAppDMXEffectImagesFolder: string;
begin
  Result := GetAppImagesFolder+'DmxEffects'+DirectorySeparator;
end;

procedure CheckAppConfigFolder;
var folder, source: string;
begin
  FAppConfigFolder := CreateAppFolder(APP_CONFIG_FOLDER);

  if RepertoireExistant(FAppConfigFolder) then begin
    // check if preset folder exists
    folder := ConcatPaths([FAppConfigFolder, USER_PRESETS_FOLDER]);
    if not RepertoireExistant(folder) then begin
      // copy the factory preset
      source := GetAppFactoryPresetsFolder;
      if RepertoireExistant(source) then begin
        CopieRepertoire(source, FAppConfigFolder, True, False);
        RenommerRepertoire(FAppConfigFolder+FACTORY_PRESETS_FOLDER, FAppConfigFolder+USER_PRESETS_FOLDER);
      end;
    end;

    // check if playlists folder exists
    folder := ConcatPaths([FAppConfigFolder, PLAYLIST_FOLDER]);
    if not RepertoireExistant(folder) then CreerRepertoire(folder);
  end else FAppConfigFolder:='';
end;

function GetUserConfigFolder: string;
begin
  Result := FAppConfigFolder;
end;

function GetPlaylistsFolder: string;
begin
  Result := GetUserConfigFolder+PLAYLIST_FOLDER+DirectorySeparator;
end;

function GetPresetsFolder: string;
begin
  Result := GetUserConfigFolder+USER_PRESETS_FOLDER+DirectorySeparator;
end;

function GetFileUserAudioPresets: string;
begin
  Result := ConcatPaths([GetPresetsFolder, 'AudioEffect'+PRESET_FILE_EXTENSION]);
end;

function GetDemoProjectFile: string;
const demoFile = 'Demo'+DirectorySeparator+'ProjectExample.say';
var f: string;
begin
  Result := '';
  {$ifdef Windows}
  // try in ProgramData\Saynetes\   <= app installed with innosetup
  f := GetUserConfigFolder + demoFile;
  if FichierExistant(f) then begin
    Result := f;
    exit;
  end;

  // try in executable location     <= portable version of the application (zip)
  f := Application.Location + demoFile;
  if FichierExistant(f) then
    Result := f;
  {$endif}
  {$ifdef Linux}
   raise exception.create('to do');
  {$endif}
end;

end.

