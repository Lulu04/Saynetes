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

function GetAppAudioPresetsFile: string;


implementation
uses Forms, u_common, u_project_manager;

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

function GetAppAudioPresetsFile: string;
begin
  Result := ConcatPaths([Project.AppPresetsFolder, 'AudioEffect'+PRESET_FILE_EXTENSION]);
end;

end.

