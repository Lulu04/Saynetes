unit u_apputils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

function AppBitness: string;
function OSName: string;

// The following (sub)folders are located in the application binary folder
function GetAppDataFolder: string;
function GetAppImagesFolder: string;
function GetAppFactoryPresetsFolder: string;
function GetAppStageImagesFolder: string;
function GetAppFixtureImagesFolder: string;
function GetAppCursorImagesFolder: string;
function GetAppChannelImagesFolder: string;
function GetAppIconImagesFolder: string;
function GetAppDMXEffectImagesFolder: string;

{ For Windows, we have two mode: application can be portable or installable (with InnoSetup)
  - Portable mode: no installation, simply unzip the downloaded package.
      . the Demo and DMXLibrary folder are located in the executable folder.
      . DMXLibrary folder, log file and program options are located in the executable folder.

  - Installable: only for Windows, using InnoSetup.
      .the Demo and DMXLibrary folders are located in: C:\ProgramData\Saynetes
      . DMXLibrary folder, log file and program options are located in C:\ProgramData\Saynetes.
}

var
  ApplicationIsPortable: boolean = False;

procedure CheckIfAppIsPortable;
procedure CheckAppConfigFolder;

// The following (sub)folder are located in:
//   for Windows platform: its depend if application is installed or portable
//                          - installed: ProgramData\Saynètes
//                          - portable: into the executable folder
//   for Linux platform: into the executable folder (application is not installed)
function GetUserConfigFolder: string;
function GetDMXLibraryFolder: string;
function GetPlaylistsFolder: string;
function GetPresetsFolder: string;
function GetFileUserAudioPresets: string;

function GetDemoProjectFile: string;


implementation
uses Forms, u_common, project_util, utilitaire_fichier, Graphics;

var
  FAppConfigFolder: string='';

function AppBitness: string;
begin
  Result := '';
  {$if defined(CPUX86_64)}
  Result := '64b';
  {$endif}
  {$if defined(CPUi386)}
  Result := '32b';
  {$endif}
end;

function OSName: string;
begin
  Result := 'Platform unknow';
  {$if defined(WINDOWS)}
  Result := 'Windows';
  {$endif}
  {$if defined(Linux)}
  Result := 'Linux';
  {$endif}
end;

function GetAppDataFolder: string;
begin
  Result := IncludeTrailingPathDelimiter(Application.Location)+'Data'+DirectorySeparator;
end;

function GetAppImagesFolder: string;
begin
  Result := GetAppDataFolder+'Images'+DirectorySeparator;
end;

function GetDMXLibraryFolder: string;
var f: string;
begin
  f := Application.Location + 'DMXLibrary';
  if DirectoryExists(f) then Result := f + DirectorySeparator
    else Result := GetUserConfigFolder + 'DMXLibrary' + DirectorySeparator;

//  Result := GetAppDataFolder+'DMXLibrary'+DirectorySeparator;
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

procedure CheckIfAppIsPortable;
begin
  {$if defined(Windows)}
  // application is in portable mode if the Demo folder is located in the executable folder
  ApplicationIsPortable := RepertoireExistant(Application.Location + 'Demo');
  {$elseif defined(Linux)}
  ApplicationIsPortable := True;
  {$else}
  {$error This platform is not supported}
  {$endif}
end;

procedure CheckAppConfigFolder;
var folder, source: string;
begin
  if ApplicationIsPortable then FAppConfigFolder := Application.Location
    else FAppConfigFolder := CreateAppFolder(APP_CONFIG_FOLDER);

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
  Result := GetPresetsFolder+'AudioEffect'+PRESET_FILE_EXTENSION;
end;

function GetDemoProjectFile: string;
const demoFile = 'Demo'+DirectorySeparator+'01-BeforeTheShow.say';
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
  // try in executable location     <= portable version of the application (zip)
  f := Application.Location + demoFile;
  if FichierExistant(f) then
    Result := f;
  {$endif}
end;

end.

