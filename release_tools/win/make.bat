@echo off

set "EXENAME=Saynetes.exe"
set "BINARYFOLDER=C:\Pascal\Saynetes\Binary\"
set "BINARYFILE=%BINARYFOLDER%%EXENAME%"
set "LAZARUS_PROJECT=C:\Pascal\Saynetes\Saynetes.lpi"
set "DEMO_FOLDER=C:\Pascal\Saynetes\DEMO\"

rem retrieves the app version
pushd ..\..
set /p VERSION=<version.txt
popd


rem delete binary file
if exist "C:\Pascal\Saynetes\Binary\Saynetes.exe" (
  del /q "C:\Pascal\Saynetes\Binary\Saynetes.exe"
)

rem delete dbg file
if exist "C:\Pascal\Saynetes\Binary\Saynetes.dbg" (
  del /q "C:\Pascal\Saynetes\Binary\Saynetes.dbg"
)

rem delete linux exe file
if exist "C:\Pascal\Saynetes\Binary\Saynetes" (
  del /q "C:\Pascal\Saynetes\Binary\Saynetes"
)


rem compile lazarus project
echo Compiling %EXENAME% version %VERSION% for x86_64
"C:\lazarus\lazbuild.exe" --build-all --quiet --widgetset=win32 --cpu=x86_64 --build-mode=Win64_Release --no-write-project %LAZARUS_PROJECT% >NUL 2>NUL

rem check if binary was build
if not exist "C:\Pascal\Saynetes\Binary\Saynetes.exe" (
  echo COMPILATION ERROR FOR TARGET x86_64
  pause
  exit /b
)
echo success
echo.

rem invoke InnoSetup 64b
echo constructing installable 64b version with InnoSetup
"C:\Program Files (x86)\Inno Setup 6\iscc.exe" /Qp "C:\Pascal\Saynetes\release_tools\win\Win64_InnoScript.iss"
echo.

echo constructing 64b zip portable version
rem copy Binary folder to a temp Saynetes folder
xcopy %BINARYFOLDER% "C:\Pascal\Saynetes\release_tools\win\Saynetes_%VERSION%" /s /e /i /q
rem copy the demo folder into this Saynetes folder
xcopy %DEMO_FOLDER% "C:\Pascal\Saynetes\release_tools\win\Saynetes_%VERSION%\Demo" /s /e /i /q
rem delete unecessary folder
rmdir /s /q "C:\Pascal\Saynetes\release_tools\win\Saynetes_%VERSION%\i386-linux"
rmdir /s /q "C:\Pascal\Saynetes\release_tools\win\Saynetes_%VERSION%\i386-win32"
rmdir /s /q "C:\Pascal\Saynetes\release_tools\win\Saynetes_%VERSION%\x86_64-linux"

echo compressing
tar.exe -a -c -f "..\Saynetes_%VERSION%_Windows64_Portable.zip" "Saynetes_%VERSION%"

rem delete temporary Saynetes folder
rmdir /s /q "C:\Pascal\Saynetes\release_tools\win\Saynetes_%VERSION%"
echo done
echo.

rem delete binary file
if exist "C:\Pascal\Saynetes\Binary\Saynetes.exe" (
  del /q "C:\Pascal\Saynetes\Binary\Saynetes.exe"
)
rem delete dbg file
if exist "C:\Pascal\Saynetes\Binary\Saynetes.dbg" (
  del /q "C:\Pascal\Saynetes\Binary\Saynetes.dbg"
)

rem compile lazarus project
echo Compiling %EXENAME% version %VERSION% for i386
"C:\lazarus\lazbuild.exe" --build-all --quiet --widgetset=win32 --cpu=i386 --build-mode=Win32_Release --no-write-project %LAZARUS_PROJECT% >NUL 2>NUL

rem check if binary was build
if not exist "C:\Pascal\Saynetes\Binary\Saynetes.exe" (
  echo COMPILATION ERROR FOR TARGET i386
  pause
  exit /b
)
echo success
echo.

rem invoke InnoSetup 32b
echo constructing installable 32b version with InnoSetup
"C:\Program Files (x86)\Inno Setup 6\iscc.exe" /Qp "C:\Pascal\Saynetes\release_tools\win\Win32_InnoScript.iss"
echo.

echo constructing 32b zip portable version
rem copy Binary folder to a temp Saynetes folder
xcopy %BINARYFOLDER% "C:\Pascal\Saynetes\release_tools\win\Saynetes_%VERSION%" /s /e /i /q
rem copy the demo folder into this Saynetes folder
xcopy %DEMO_FOLDER% "C:\Pascal\Saynetes\release_tools\win\Saynetes_%VERSION%\Demo" /s /e /i /q
rem delete unecessary folder
rmdir /s /q "C:\Pascal\Saynetes\release_tools\win\Saynetes_%VERSION%\i386-linux"
rmdir /s /q "C:\Pascal\Saynetes\release_tools\win\Saynetes_%VERSION%\x86_64-win64"
rmdir /s /q "C:\Pascal\Saynetes\release_tools\win\Saynetes_%VERSION%\x86_64-linux"

echo compressing
tar.exe -a -c -f "..\Saynetes_%VERSION%_Windows32_Portable.zip" "Saynetes_%VERSION%"

rem delete temporary Saynetes folder
rmdir /s /q "C:\Pascal\Saynetes\release_tools\win\Saynetes_%VERSION%"
echo.

rem delete binary file
if exist "C:\Pascal\Saynetes\Binary\Saynetes.exe" (
  del /q "C:\Pascal\Saynetes\Binary\Saynetes.exe"
)

echo done
