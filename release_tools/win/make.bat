@echo off


set "EXENAME=Saynetes.exe"
set "BINARYFOLDER=C:\Pascal\Saynetes\Binary\"
set "BINARYFILE=%BINARYFOLDER%%EXENAME%"
set "LAZARUS_PROJECT=C:\Pascal\Saynetes\Saynetes.lpi"
set "DEMO_FOLDER=C:\Pascal\Saynetes\DEMO\"

rem delete binary file
if exist "C:\Pascal\Saynetes\Binary\Saynetes.exe" (
  del /q "C:\Pascal\Saynetes\Binary\Saynetes.exe"
)

rem delete dbg file
if exist "C:\Pascal\Saynetes\Binary\Saynetes.dbg" (
  del /q "C:\Pascal\Saynetes\Binary\Saynetes.dbg"
)

rem compile lazarus project
echo Compiling %EXENAME% for x86_64
"C:\lazarus\lazbuild.exe" --build-all --quiet --widgetset=win32 --cpu=x86_64 --build-mode=Win64_Release --no-write-project %LAZARUS_PROJECT% >NUL 2>NUL

rem check if binary was build
if not exist "C:\Pascal\Saynetes\Binary\Saynetes.exe" (
  echo COMPILATION ERROR FOR TARGET x86_64
  pause
  exit /b
)
echo success

rem invoke InnoSetup
rem echo constructing installable version with InnoSetup
rem "C:\Program Files (x86)\Inno Setup 6\iscc.exe" /Qp "C:\Pascal\Saynetes\ReleaseTools\win\ScriptForWin64.iss"

echo.

echo constructing zip portable version
rem copy the demo folder into Binary folder
xcopy %DEMO_FOLDER% "%BINARYFOLDER%Demo" /s /e /i /q

rem compress
pushd ..\..
rename Binary Saynetes
tar.exe -a -c -f C:\Pascal\Saynetes\release_tools\Saynetes_win64.zip Saynetes
rename Saynetes Binary
popd

rem delete demo sub-folder from binary folder
rmdir /s /q "%BINARYFOLDER%Demo"


exit /b
pause