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


implementation
uses Forms;

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
  Result := GetAppImagesFolder+'Stage'+DirectorySeparator;
end;

function GetAppFixtureImagesFolder: string;
begin
  Result := GetAppImagesFolder+'FixtureImages'+DirectorySeparator;
end;

function GetAppCursorImagesFolder: string;
begin
  Result := GetAppImagesFolder+'Cursors'+DirectorySeparator;
end;

end.

