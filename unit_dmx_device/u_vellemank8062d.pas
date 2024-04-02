unit u_vellemank8062d;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil,
  hidapi,
  u_dmxdevice_manager;


const
  // library filename
  {$ifdef MSWINDOWS}
    {$ifdef CPU386}
      LIBHIDAPI = 'i386-win32\hidapi.dll';
    {$endif}
    {$ifdef CPU64}
      LIBHIDAPI = 'x86_64-win64\hidapi.dll';
    {$endif}
  {$endif}

  {$ifdef LINUX}
  {$ifdef CPU386}
    LIBHIDAPI = 'i386-linux\libhidapi-libusb.so.0.12.0';
  {$endif}
  {$ifdef CPU64}
    LIBHIDAPI = 'x86_64-linux/libhidapi-libusb.so.0.0.0';
  {$endif}
  {$endif}


type

  { TK8062Thread }

  TK8062Thread=class(TThread)
  private
    FPeriodMS: integer;
    FHidDevice: PHidDevice;
    Buf_tx: array [0..64] of byte;
    FDmxDataOut: array[0..523] of byte;
    procedure DoWrite;
    procedure SendDmx;
  protected
    procedure Execute; override;
  public
    ChannelCount: integer;
    Constructor Create(aPeriodMs: integer; aStart: boolean; aHidDevice: PHidDevice);
    procedure SetChannel(aAdr: integer; aLevel: byte);
  end;


  { TVelleman_K8062D_DMXHardware }

  TVelleman_K8062D_DMXHardware=class(TBaseDMXDevice)
  private
    FK8062Thread: TK8062Thread;
    FHidDevice: PHidDevice;
    FUsbPath: string;
  protected
    procedure DoInit; override;
    procedure DoFreeMem; override;
    function DoOpen(aPortindex: integer): boolean; override;
    function DoClose(aPortindex: integer): boolean; override;
    function DoSetPortDirection(aPortindex: integer; aDir: TPortDirection): boolean; override;
    function DoSetUsedChannelCount(aPortindex, aValue: integer): boolean; override;
    function DoUpdateChannel(aPortindex, aDMXAdress, aValue: integer): boolean; override;
    function DoSendAll(aPortindex: integer): boolean; override;
    function DoReceiveData(aPortindex: integer): boolean; override;
    function DoGetName: string; override;
    function DoGetSerialNumber: string; override;
  public
    procedure InitFromUSBPath( const aPath: string);
  end;


  { TK8062Manager }

  TK8062Manager=class
  private
    FReady: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LookForDevice;

    property Ready: boolean read FReady;
  end;



{
  // original Velleman K8062 dll
  //
  //
  procedure StartDevice; stdcall; external 'K8062d.dll';
  procedure SetData(Channel: Longint ; Data: Longint); stdcall; external 'K8062d.dll';
  procedure SetChannelCount(Count: Longint); stdcall; external 'K8062d.dll';
  procedure StopDevice; stdcall; external 'K8062d.dll';

}

implementation
uses Forms;

const
  K8062_VENDOR_ID = $10cf;
  K8062_PRODUCT_ID = $8062;


{ TK8062Thread }

procedure TK8062Thread.DoWrite;
var Written: SizeInt;
  ToWrite: SizeInt;
begin
  Buf_tx[0] := 0;
  ToWrite := 9;
  Written := FHidDevice^.Write(buf_tx, ToWrite);
end;

procedure TK8062Thread.SendDmx;
var i, max_ch, n: integer;
  function GetShareData(aIndex: integer): byte; inline;
  begin
    Result := FDmxDataOut[aIndex];
  end;

begin
  Buf_tx[0] := 0;
  max_ch := ChannelCount;
  if max_ch < 8 then max_ch := 8;
  if max_ch > 512 then max_ch := 512;
  i := 0;
  repeat
    inc(i);
  until (GetShareData(i+10) > 0) or (i = 100) or (i = max_ch-5);
  Buf_tx[1] := 4;    //send start code + 6 bytes of data
  Buf_tx[2] := i;    //number of zeroes (incl. start code = 0)
  Buf_tx[3] := GetShareData(i+10);
  Buf_tx[4] := GetShareData(i+11);
  Buf_tx[5] := GetShareData(i+12);
  Buf_tx[6] := GetShareData(i+13);
  Buf_tx[7] := GetShareData(i+14);
  Buf_tx[8] := GetShareData(i+15);
  DoWrite;
  i := i+6;
  repeat
    if max_ch-i < 6 then
    begin
      Buf_tx[1] := 3;    //send one byte of data
      Buf_tx[2] := GetShareData(i+10);
      DoWrite;
      inc(i);
    end;
    if (max_ch-i >= 6) and (GetShareData(i+10) > 0) then
    begin
      Buf_tx[1] := 2;    //send 7 bytes of data
      Buf_tx[2] := GetShareData(i+10);
      Buf_tx[3] := GetShareData(i+11);
      Buf_tx[4] := GetShareData(i+12);
      Buf_tx[5] := GetShareData(i+13);
      Buf_tx[6] := GetShareData(i+14);
      Buf_tx[7] := GetShareData(i+15);
      Buf_tx[8] := GetShareData(i+16);
      DoWrite;
      i := i+7;
    end;
    if (max_ch-i >= 6) and (GetShareData(i+10) = 0) then
    begin
      n := 0;
      repeat
        inc(i);
        inc(n);
      until (GetShareData(i+10) > 0) or (n = 100) or (i >= max_ch-6);
      Buf_tx[1] := 5;    //send n zeroes + 6 bytes of data
      Buf_tx[2] := n;    //number of zeroes to send
      Buf_tx[3] := GetShareData(i+10);
      Buf_tx[4] := GetShareData(i+11);
      Buf_tx[5] := GetShareData(i+12);
      Buf_tx[6] := GetShareData(i+13);
      Buf_tx[7] := GetShareData(i+14);
      Buf_tx[8] := GetShareData(i+15);
      DoWrite;
      i := i+6;
    end;
  until i > max_ch;
end;

procedure TK8062Thread.Execute;
var TOrigin, TNow, DeltaT: QWord;
begin
  TOrigin := GetTickCount64;
  while not Terminated do
  begin
    TNow := GetTickCount64;
    DeltaT := TNow-TOrigin;
    if DeltaT >= FPeriodMS then
    begin
      SendDmx;
      TOrigin += FPeriodMS;
    end
    else sleep(1);
   end;
end;

constructor TK8062Thread.Create(aPeriodMs: integer; aStart: boolean;
                   aHidDevice: PHidDevice);
begin
  inherited Create(TRUE);
  Priority := tpHighest;//tpNormal;
  FPeriodMS := aPeriodMs;
  FHidDevice := aHidDevice;
  ChannelCount := 512;
  if aStart then
    Start;
end;

procedure TK8062Thread.SetChannel(aAdr: integer; aLevel: byte);
begin
  FDmxDataOut[aAdr+10] := aLevel;
end;

{ TVelleman_K8062D_DMXHardware }

procedure TVelleman_K8062D_DMXHardware.DoInit;
begin
  FDeviceType := dtVelleman_K8062D;
  SetLength(FPorts, 1);
  FPorts[0].InitByDefault;

  FPorts[0].Direction := pdOut;
  FPorts[0].DirectionCanChange := FALSE;
  FPorts[0].UsedChannel := 22;
  FPorts[0].MinUsedChannel := 22;
  FPorts[0].MaxUsedChannel := 512;
end;

procedure TVelleman_K8062D_DMXHardware.DoFreeMem;
begin
  if FK8062Thread <> NIL then
  begin
    FK8062Thread.Terminate;
    FK8062Thread.WaitFor;
    FK8062Thread.Free;
    FK8062Thread := NIL;
  end;
end;

function TVelleman_K8062D_DMXHardware.DoOpen(aPortindex: integer): boolean;
begin
  if FK8062Thread = NIL then
  begin
    FK8062Thread := TK8062Thread.Create(25, FALSE, FHidDevice);
    FK8062Thread.ChannelCount := FPorts[0].UsedChannel;
    FK8062Thread.Start;
    FPorts[0].IsOpen := TRUE;
  end;
  Result := TRUE;
end;

function TVelleman_K8062D_DMXHardware.DoClose(aPortindex: integer): boolean;
begin
  if FK8062Thread<>NIL then
  begin
    FK8062Thread.Terminate;
    FK8062Thread.WaitFor;
    FK8062Thread.Free;
    FK8062Thread := NIL;
    FHidDevice^.Close;
    FPorts[0].IsOpen := FALSE;
  end;
  Result := TRUE;
end;

function TVelleman_K8062D_DMXHardware.DoSetPortDirection(aPortindex: integer; aDir: TPortDirection): boolean;
begin
  Result := aDir = pdOut;
end;

function TVelleman_K8062D_DMXHardware.DoUpdateChannel(aPortindex, aDMXAdress, aValue: integer): boolean ;
begin
  if FK8062Thread <> NIL then
    FK8062Thread.SetChannel(aDMXAdress, byte(aValue));
 Result := TRUE;
end;

function TVelleman_K8062D_DMXHardware.DoSendAll(aPortindex: integer): boolean;
begin
  Result := TRUE;
end;

function TVelleman_K8062D_DMXHardware.DoSetUsedChannelCount(aPortindex, aValue: integer): boolean;
begin
  if aValue < 22 then aValue := 22;
  if aValue > 512 then aValue := 512;
  if FK8062Thread <> NIL then
    FK8062Thread.ChannelCount := aValue;
  Result := TRUE;
end;

function TVelleman_K8062D_DMXHardware.DoReceiveData(aPortindex: integer): boolean;
begin
  Result := FALSE;
end;

function TVelleman_K8062D_DMXHardware.DoGetName: string;
begin
  if Assigned(FHidDevice) then
    Result := 'USB K8062' //string(FHidDevice^.GetProductString)
  else
    Result := '';
end;

function TVelleman_K8062D_DMXHardware.DoGetSerialNumber: string;
begin
  Result := 'xxxx';// K8062 don't have a serial number
end;

procedure TVelleman_K8062D_DMXHardware.InitFromUSBPath(const aPath: string);
begin
  FUsbPath := aPath;
  FHidDevice := THidDevice.OpenPath(aPath);
  Open(0);
end;

{ TK8062Manager }

constructor TK8062Manager.Create;
begin
  FReady := HidInit(ConcatPaths([{ParamStr(0)} Application.Location, LIBHIDAPI])) > 0;
end;

destructor TK8062Manager.Destroy;
begin
  HidExit;
  inherited Destroy;
end;

procedure TK8062Manager.LookForDevice;
var Paths: TStringList;
  LinkedList, Item: PHidDeviceInfo;
  o: TVelleman_K8062D_DMXHardware;
  i: integer;
begin
  Paths := TStringList.Create();
  LinkedList := THidDeviceInfo.Enumerate(K8062_VENDOR_ID, K8062_PRODUCT_ID);
  Item := LinkedList;
  while Assigned(Item) do
  begin
    Paths.Append(Item^.Path);
    Item := Item^.Next;
  end;
  if Assigned(LinkedList) then
    LinkedList^.Free;
  for i:=0 to Paths.Count-1 do
  begin
    o := TVelleman_K8062D_DMXHardware.Create;
    o.InitFromUSBPath(Paths.Strings[i]);
    DeviceManager.RegisterDevice(o);
  end;

  Paths.Free;
end;

end.

