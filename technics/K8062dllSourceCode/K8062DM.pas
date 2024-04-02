unit K8062DM;

interface

uses
  Windows, SysUtils;
 
PROCEDURE StartDevice; stdcall;
PROCEDURE StopDevice; stdcall;
PROCEDURE SetData(Channel: Longint ; Data: Longint); stdcall;
PROCEDURE SetChannelCount(Count: Longint); stdcall;
procedure SetShareData(address,data: Longint); stdcall;
function GetShareData(address:Longint): Longint; stdcall;

implementation

// global DLL data
var
  ShareData: ^longint; // shared
  hMapFile: THandle;
  running:boolean = false;

const
  VirtualFileName = 'ShareK8062Data';
  DataSize = 2500;

// shared data read and write

procedure StartDevice; stdcall;
var h,i:integer;
begin
    SetShareData(1,222);
    H := WinExec(PChar('K8062E.exe'), SW_RESTORE);
end;

procedure StopDevice; stdcall;
begin
  SetShareData(0,333);
end;


PROCEDURE SetChannelCount(Count: Longint); stdcall;
begin
  if Count>0 then SetShareData(2,Count);
end;


PROCEDURE SetData(Channel: Longint ; Data: Longint); stdcall;
begin
  if channel>0 then SetShareData(Channel+10,Data);
end;

procedure SetShareData (address,data: Longint);
var p:pointer;
ret:longint;
begin
  p:=ShareData;
  asm
    push  edi
    mov   edi,p
    add   edi,address
    add   edi,address
    add   edi,address
    add   edi,address
    mov   ret,edi
    mov   eax,data
    mov   ds:[edi],eax
    pop   edi
  end;
end;

function GetShareData (Address:Longint): Longint; stdcall;
var p:pointer;
k:longint;
begin
  p:= ShareData;
  asm
    push  edi
    mov   edi,p
    add   edi,address
    add   edi,address
    add   edi,address
    add   edi,address
    mov   eax,ds:[edi]
    mov   k,eax
    pop   edi
  end;
  Result:=k;
end;

initialization
  //create memory mapped file
  hMapFile := CreateFileMapping ($FFFFFFFF, nil,
    Page_ReadWrite, 0, DataSize, VirtualFileName);
  if hMapFile = 0 then
    raise Exception.Create ('Error creating memory mapped file');
  // get the pointer to the actual data
  ShareData := MapViewOfFile (
    hMapFile, File_Map_Write, 0, 0, DataSize);

finalization
  UnmapViewOfFile (ShareData);
  CloseHandle (hMapFile);
end.
