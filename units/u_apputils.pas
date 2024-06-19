unit u_apputils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

function GetAppDataFolder: string;
function GetAppDMXLibraryFolder: string;
function GetAppImagesFolder: string;
function GetAppStageImagesFolder: string;
function GetAppFixtureImagesFolder: string;
function GetAppCursorImagesFolder: string;
function GetAppChannelImagesFolder: string;
function GetAppIconImagesFolder: string;
function GetAppDMXEffectImagesFolder: string;

procedure CheckAppConfigFolder;
function GetAppConfigFolder: string;
function GetPlaylistsFolder: string;
function GetPresetsFolder: string;

function GetAppAudioPresetsFile: string;


implementation
uses Forms, u_common, u_project_manager, project_util, utilitaire_fichier;

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
    folder := ConcatPaths([FAppConfigFolder, PRESET_FOLDER]);
    if not RepertoireExistant(folder) then begin
      // copy the factory preset
      source := IncludeTrailingPathDelimiter(ConcatPaths([Application.Location, PRESET_FOLDER]));
      if RepertoireExistant(source) then
          CopieRepertoire(source, FAppConfigFolder, True, False);
    end;

    // check if playlists folder exists
    folder := ConcatPaths([FAppConfigFolder, PLAYLIST_FOLDER]);
    if not RepertoireExistant(folder) then CreerRepertoire(folder);
  end else FAppConfigFolder:='';
end;

function GetAppConfigFolder: string;
begin
  Result := IncludeTrailingPathDelimiter(FAppConfigFolder);
end;

function GetPlaylistsFolder: string;
begin
  Result := GetAppConfigFolder+PLAYLIST_FOLDER+DirectorySeparator;
end;

function GetPresetsFolder: string;
begin
  Result := GetAppConfigFolder+PRESET_FOLDER+DirectorySeparator;
end;

function GetAppAudioPresetsFile: string;
begin
  Result := ConcatPaths([GetPresetsFolder, 'AudioEffect'+PRESET_FILE_EXTENSION]);
end;

end.

