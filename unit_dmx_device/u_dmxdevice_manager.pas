unit u_dmxdevice_manager;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses Classes, SysUtils, Dialogs,
  u_common;


const
  INVALID_DMXDEVICE_INDEX=-1;
  INVALID_DMXDEVICEPORT_INDEX=-1;
  NODEVICE_NAME='_no_device_';
type

// Definition of the device known by the program
TDMXDeviceType=(dtNo_Device,
                dtVelleman_K8062D,
                dtENTTEC_OPENDMX,
                dtENTTEC_USBDMXPRO,
                dtFakeMultiPort
                );

TDMXBuffer=packed array[1..512] of byte;

TPortDirection=(pdIn=0, pdOut=1);

function PortDirectionToString(aDir: TPortDirection): string;

type

{ TDMXDevicePort }

TDMXDevicePort=record
  IsOpen: boolean;
  DMXBuffer: TDMXBuffer;
  BufferChanged: boolean;
  Direction: TPortDirection;
  DirectionCanChange: boolean;
  MinUsedChannel,
  MaxUsedChannel,
  UsedChannel: integer;
  procedure InitByDefault;
  procedure ResetBuffer;
end;
TArrayOfDMXDevicePort=array of TDMXDevicePort;

{ abstract class that define the common methods which each device driver must implement
     A device can manage one or several ports (dmx universes).
     Each port have a direction IN or OUT. This direction can be changed or not, according to the device specifications
     Because a device can have more than one port, a port index is specified
}
TDMXDeviceAbstract=class abstract
protected
  // In this method, for each port of the device, we have to initialize value for:
  //    - MinUsedChannel                <- the minimum channel count to use
  //    - MaxUsedChannel                <- the maximum channel count to use
  //    - default UsedChannel value     <- the default channel count to use
  //    - default Direction value       <- the direction of the port when the device start
  //    - DirectionCanChange value      <- the capability of the device port to change its direction
  procedure DoInit; virtual; abstract;
  procedure DoFreeMem; virtual abstract;

  function DoOpen(aPortindex: integer): boolean; virtual; abstract;
  function DoClose(aPortindex: integer): boolean; virtual; abstract;
  // return TRUE if the device port have successfully sets the new direction
  function DoSetPortDirection(aPortindex: integer; aDir: TPortDirection): boolean; virtual; abstract;
  // return TRUE if the device port can accept the new channel count
  function DoSetUsedChannelCount(aPortindex, aCount: integer): boolean; virtual; abstract;
  // return TRUE if the driver have successfully updated the device channel's value
  function DoUpdateChannel(aPortindex, aDMXAdress, aValue: integer): boolean; virtual; abstract;
  // return TRUE if the driver have successfully sent all value to the device
  function DoSendAll(aPortindex: integer): boolean; virtual; abstract;
  // return TRUE if the data have been successfully received
  function DoReceiveData(aPortindex: integer): boolean; virtual; abstract;
  // the name of the device
  function DoGetName: string; virtual; abstract;
  // the serial number of the device
  function DoGetSerialNumber: string; virtual; abstract;
end;


{ TBaseDMXDevice }

TBaseDMXDevice = class(TDMXDeviceAbstract)
private
  function GetDmxBuffer(aPortindex: integer): PByte;
 // FIsOpen: boolean;
  function GetMinUsedChannelCount(aPortindex: integer): integer;
  function GetMaxUsedChannelCount(aPortindex: integer): integer;
  function GetPortDirection(aPortindex: integer): TPortDirection;
  function GetPortDirectionCanChange(aPortIndex: integer): boolean;
  function GetPortIsOpen(aPortIndex: integer): boolean;
  procedure SetPortDirection(aPortindex: integer; AValue: TPortDirection);
  procedure SetUsedChannelCount(aPortindex: integer; AValue: integer);
  function GetUsedChannelCount(aPortindex: integer): integer;
  function ValidPortIndex(aPortIndex: integer): boolean;
  function ValidPortIndexAndPortIsOpened(aPortIndex: integer): boolean;
protected
  FDeviceType: TDMXDeviceType;
  FPorts: TArrayOfDMXDevicePort;
  function GetPortCount: integer;
public
  constructor Create;
  destructor Destroy; override;

  function Open(aPortindex: integer): boolean; // True if success
  // close a single port, if this feature is available
  function Close(aPortindex: integer): boolean;
  // close all ports of the device
  procedure CloseAllPort;

  function UpdateChannel(aPortindex, aDMXAdress, aValue: integer): boolean;
  function SendAll(aPortindex: integer): boolean;

  function ReceiveData(aPortindex: integer): boolean;
  property DmxBuffer[aPortindex: integer]: PByte read GetDmxBuffer;

  //procedure SaveDeviceListConfiguredAsInputTo(t: TStringList);
  //procedure LoadDeviceListConfiguredAsInputFrom(t: TStringList);

  property DeviceType: TDMXDeviceType read FDeviceType;
  property Name: string read DoGetName;
  property SerialNumber: string read DoGetSerialNumber;
  property PortCount: integer read GetPortCount;
  property PortIsOpen[aPortIndex: integer]: boolean read GetPortIsOpen;
  property PortDirectionCanChange[aPortIndex: integer]: boolean read GetPortDirectionCanChange;
  property PortDirection[aPortindex: integer]: TPortDirection read GetPortDirection write SetPortDirection;
  property UsedChannelCount[aPortindex: integer]: integer read GetUsedChannelCount write SetUsedChannelCount;
  property MinUsedChannelCount[aPortindex: integer]: integer read GetMinUsedChannelCount;
  property MaxUsedChannelCount[aPortindex: integer]: integer read GetMaxUsedChannelCount;

end;


{ TNoDevice }

TNoDevice = class(TBaseDMXDevice)
protected
  procedure DoInit; override;
  procedure DoFreeMem; override;
  function DoOpen({%H-}aPortindex: integer): boolean; override;
  function DoClose({%H-}aPortindex: integer): boolean ; override;
  function DoSetPortDirection({%H-}aPortindex: integer; {%H-}aDir: TPortDirection): boolean; override;
  function DoSetUsedChannelCount({%H-}aPortindex, {%H-}aValue: integer): boolean; override;
  function DoUpdateChannel({%H-}aPortindex, {%H-}aDMXAdress, {%H-}aValue: integer): boolean; override;
  function DoSendAll({%H-}aPortindex: integer): boolean; override;
  function DoReceiveData({%H-}aPortindex: integer): boolean; override;
  function DoGetName: string; override;
  function DoGetSerialNumber: string; override;
end;




{ TFakeDeviceMultiPort }

TFakeDeviceMultiPort = class(TBaseDMXDevice)
protected
  procedure DoInit; override;
  procedure DoFreeMem; override;
  function DoOpen({%H-}aPortindex: integer): boolean; override;
  function DoClose({%H-}aPortindex: integer): boolean ; override;
  function DoSetPortDirection({%H-}aPortindex: integer; {%H-}aDir: TPortDirection): boolean; override;
  function DoSetUsedChannelCount({%H-}aPortindex, {%H-}aValue: integer): boolean; override;
  function DoUpdateChannel({%H-}aPortindex, {%H-}aDMXAdress, {%H-}aValue: integer): boolean; override;
  function DoSendAll({%H-}aPortindex: integer): boolean; override;
  function DoReceiveData({%H-}aPortindex: integer): boolean; override;
  function DoGetName: string; override;
  function DoGetSerialNumber: string; override;
end;

{ TENTTEC_OPENDMX_DMXHardware }
{
TENTTEC_OPENDMX_DMXHardware=class(TBaseDMXDevice)
private
  FFTDeviceDescription: string;
  FFTDeviceSerialNo: string;
  FFTNumDevice: integer;
  DriverEnttecOpenDmx: TDriverEnttecOpenDmx ;
protected
  procedure DoInit; override;
  procedure FreeMem; override;
  function DoOpen: boolean; override;
  function DoClose: boolean; override;
  function DoUpdateChannel(aDMXAdress, aValue: integer): boolean; override;
  function DoSendAll: boolean; override;
  procedure SetUsedChannelCount(aValue: integer ); override;
  function GetUsedChannelCount: integer; override;
  procedure DoReceiveData; override;
  function DoGetName: string; override;
  function DoGetSerialNumber: string; override;
  function GetIsConnected: boolean; override;
  function GetUniverseCount: integer; override;
  function GetUniverseDirection(index: integer): TUniverseDirection; override;
  procedure SetUniverseDirection(index: integer; aDir: TUniverseDirection); override;
end;   }


{TDeviceManager}
TDeviceManager=class
 private
  procedure LookForVellemanK8062;
  procedure LookForFTDIBasedDevice;
 private
  FListDeviceFound: TList;
  FNoDevice : TNoDevice; // if a requested device is not found we routes to this
                         // virtual device that do nothing
  procedure ClearList;
  function GetCount: integer;
  function GetDMXDevice( Index: integer): TBaseDMXDevice;
  function GetPortCount: integer;

 public
  Constructor Create;
  Destructor Destroy; override;

  function IsValidIndex(Index: integer): boolean;

  procedure LookForAvailableDevices;

  // gives the list of device path, the 'fake' device included
  function GetDevicesPath(aIncludeNoDevice: boolean): TArrayOfDevicePath;

  procedure CloseAll;

  function GetDeviceIndexByNameSerial(const aName, aSerialNumber: string): integer;
  function GetDeviceByPath(aPath: TDevicePath): TBaseDMXDevice;

  function DevicePortIsOpen(aDevicePath: TDevicePath): boolean;

  procedure RegisterDevice(aDevice: TBaseDMXDevice);

  property Count: integer read GetCount; // gives the number of device found
  property PortCount: integer read GetPortCount; // gives the total ports count available
  property Device[Index:integer]: TBaseDMXDevice read GetDMXDevice;
end;

var
  DeviceManager: TDeviceManager;


implementation
uses u_vellemank8062d, // VELLEMAN K8062D
     u_ftdi_based, u_resource_string, u_logfile, Math;

var
    FK8062Manager: TK8062Manager;
    FTDIManager: TFTDIManager;

function PortDirectionToString(aDir: TPortDirection): string;
begin
  case aDir of
    pdOut: Result := SDevicePortOut;
    pdIn: Result := SDevicePortIn;
  end;
end;


{ TFakeDeviceMultiPort }

procedure TFakeDeviceMultiPort.DoInit;
var i: integer;
begin
  FDeviceType := dtFakeMultiPort;
  SetLength(FPorts, 3);
  for i:=0 to High(FPorts) do
   FPorts[i].InitByDefault;

  for i:=0 to High(FPorts) do begin
    FPorts[i].Direction := pdOut;
    FPorts[i].DirectionCanChange := TRUE;
    FPorts[i].UsedChannel := 512;
    FPorts[i].MinUsedChannel := 1;
    FPorts[i].MaxUsedChannel := 512;
  end;
  i:=High(FPorts);
  FPorts[i].Direction := pdIn;
  FPorts[i].DirectionCanChange := FALSE;
  FPorts[i].UsedChannel := 512;
  dec(i);
  FPorts[i].Direction := pdOut;
  FPorts[i].DirectionCanChange := FALSE;
  FPorts[i].UsedChannel := 256;
  FPorts[i].MinUsedChannel := 1;
  FPorts[i].MaxUsedChannel := 256;

  for i:=0 to High(FPorts) do
    Open(i);
end;

procedure TFakeDeviceMultiPort.DoFreeMem;
begin
end;

function TFakeDeviceMultiPort.DoOpen(aPortindex: integer): boolean;
begin
  FPorts[aPortindex].IsOpen := TRUE;
  Result := TRUE;
end;

function TFakeDeviceMultiPort.DoClose(aPortindex: integer): boolean;
begin
  FPorts[aPortindex].IsOpen := FALSE;
  Result := TRUE;
end;

function TFakeDeviceMultiPort.DoSetPortDirection(aPortindex: integer;
  aDir: TPortDirection): boolean;
begin
  Result := TRUE
end;

function TFakeDeviceMultiPort.DoSetUsedChannelCount(aPortindex, aValue: integer): boolean;
begin
  Result := TRUE;
end;

function TFakeDeviceMultiPort.DoUpdateChannel(aPortindex, aDMXAdress, aValue: integer): boolean;
begin
  Result := TRUE;
end;

function TFakeDeviceMultiPort.DoSendAll(aPortindex: integer): boolean;
begin
  Result := TRUE;
end;

function TFakeDeviceMultiPort.DoReceiveData(aPortindex: integer): boolean;
begin
  Result := TRUE;
end;

function TFakeDeviceMultiPort.DoGetName: string;
begin
  Result := 'FAKE Device Multi port';
end;

function TFakeDeviceMultiPort.DoGetSerialNumber: string;
begin
  Result := '5555';
end;

{ TDMXDevicePort }

procedure TDMXDevicePort.InitByDefault;
begin
  IsOpen := FALSE;
  ResetBuffer;
  UsedChannel := 512;
  Direction := pdOut;
  DirectionCanChange := FALSE;
end;

procedure TDMXDevicePort.ResetBuffer;
var i: integer;
begin
  for i:=Low(DMXBuffer) to High(DMXBuffer) do
   DMXBuffer[i] := 0;
  BufferChanged := TRUE;
end;

{ TNoDevice }

procedure TNoDevice.DoInit;
begin
 FDeviceType := dtNo_Device;
 SetLength(FPorts, 1);
 FPorts[0].InitByDefault;
 FPorts[0].IsOpen := TRUE;

 FPorts[0].Direction := pdOut;
 FPorts[0].DirectionCanChange := FALSE;
 FPorts[0].UsedChannel := 512;
 FPorts[0].MinUsedChannel := 1;
 FPorts[0].MaxUsedChannel := 512;
end;

procedure TNoDevice.DoFreeMem;
begin
end;

function TNoDevice.DoOpen(aPortindex: integer): boolean;
begin
 Result := True;
end;

function TNoDevice.DoClose(aPortindex: integer): boolean;
begin
 Result := True;
end;

function TNoDevice.DoSetPortDirection(aPortindex: integer; aDir: TPortDirection): boolean;
begin
  Result := FALSE;
end;

function TNoDevice.DoSetUsedChannelCount(aPortindex, aValue: integer): boolean;
begin
  Result := FALSE;
end;

function TNoDevice.DoUpdateChannel(aPortindex, aDMXAdress, aValue: integer): boolean;
begin
 Result := TRUE;
end;

function TNoDevice.DoSendAll(aPortindex: integer): boolean;
begin
  Result := TRUE;
end;

function TNoDevice.DoReceiveData(aPortindex: integer): boolean;
begin
  Result := FALSE;
end;

function TNoDevice.DoGetName: string;
begin
 Result := NODEVICE_NAME;
end;

function TNoDevice.DoGetSerialNumber: string;
begin
  Result := '';
end;

{ TENTTEC_OPENDMX_DMXHardware }
{

procedure TENTTEC_OPENDMX_DMXHardware.DoInit;
begin
 FDeviceType:=dtENTTEC_OPENDMX;
 FFTDeviceDescription:='---';
 FFTDeviceSerialNo:='---';
 FFTNumDevice:=-2;
 DriverEnttecOpenDmx:= NIL;
end;

procedure TENTTEC_OPENDMX_DMXHardware.FreeMem;
begin
end;

function TENTTEC_OPENDMX_DMXHardware.DoOpen: boolean;
begin
  if DriverEnttecOpenDmx=NIL then begin
         DriverEnttecOpenDmx:=TDriverEnttecOpenDmx.Create;
         Result:=DriverEnttecOpenDmx.OuvreInterface(FFTNumDevice);
  end else Result:= TRUE;
end;

function TENTTEC_OPENDMX_DMXHardware.DoClose: boolean;
begin
  if Assigned(DriverEnttecOpenDmx) then begin
    Result:=DriverEnttecOpenDmx.FermeInterface;
    DriverEnttecOpenDmx.free;
    DriverEnttecOpenDmx:=NIL;
  end else Result:=TRUE;
end;

function TENTTEC_OPENDMX_DMXHardware.DoUpdateChannel(aDMXAdress, aValue: integer): boolean;
begin
  if Assigned(DriverEnttecOpenDmx)
    then DriverEnttecOpenDmx.Envoi(aDMXAdress ,aValue);
  Result:=TRUE;
end;

function TENTTEC_OPENDMX_DMXHardware.DoSendAll: boolean;
begin
  Result:=TRUE;
end;

procedure TENTTEC_OPENDMX_DMXHardware.SetUsedChannelCount(aValue: integer);
begin
end;

function TENTTEC_OPENDMX_DMXHardware.GetUsedChannelCount: integer;
begin
  Result:=512;
end;

procedure TENTTEC_OPENDMX_DMXHardware.DoReceiveData;
begin
end;

function TENTTEC_OPENDMX_DMXHardware.DoGetName: string;
begin
  Result:=FFTDeviceDescription;
end;

function TENTTEC_OPENDMX_DMXHardware.DoGetSerialNumber: string;
begin
  Result:='???';
end;

function TENTTEC_OPENDMX_DMXHardware.GetIsConnected: boolean;
begin
  Result:=FIsOpen;
end;

function TENTTEC_OPENDMX_DMXHardware.GetUniverseCount: integer;
begin
  Result:=1;
end;

function TENTTEC_OPENDMX_DMXHardware.GetUniverseDirection(index: integer
  ): TUniverseDirection;
begin
  Result:=udOut;
end;

procedure TENTTEC_OPENDMX_DMXHardware.SetUniverseDirection(index: integer;
  aDir: TUniverseDirection);
begin
end;
}
{ TBaseDMXDevice }

function TBaseDMXDevice.ValidPortIndex(aPortIndex: integer): boolean;
begin
  Result := (aPortIndex >= Low(FPorts)) and (aPortIndex <= High(FPorts));
end;

procedure TBaseDMXDevice.SetUsedChannelCount(aPortindex: integer; AValue: integer);
begin
  if not ValidPortIndex(aPortindex) then
    exit;

  AValue := EnsureRange(AValue, FPorts[aPortindex].MinUsedChannel, FPorts[aPortindex].MaxUsedChannel);
  if FPorts[aPortindex].UsedChannel=AValue then
    exit;

  if DoSetUsedChannelCount(aPortindex, AValue) then
  begin
    FPorts[aPortindex].UsedChannel := AValue;
    Log.Debug(Name+' sets used channel to '+AValue.ToString)
  end
  else Log.Error(Name+' failed to set used channel to '+AValue.ToString);
end;

function TBaseDMXDevice.GetPortDirection(aPortindex: integer): TPortDirection;
begin
  if ValidPortIndex(aPortindex) then
    Result := FPorts[aPortindex].Direction
  else
    Result := pdOut;
end;

function TBaseDMXDevice.GetDmxBuffer(aPortindex: integer): PByte;
begin
  if ValidPortIndex(aPortindex) then
    Result := PByte(@FPorts[aPortindex].DMXBuffer[Low(FPorts[aPortindex].DMXBuffer)])
  else
    Result := NIL;
end;

function TBaseDMXDevice.GetMinUsedChannelCount(aPortindex: integer): integer;
begin
  if ValidPortIndex(aPortindex) then
    Result := FPorts[aPortindex].MinUsedChannel
  else
    Result := 1;
end;

function TBaseDMXDevice.GetMaxUsedChannelCount(aPortindex: integer): integer;
begin
  if ValidPortIndex(aPortindex) then
    Result := FPorts[aPortindex].MaxUsedChannel
  else
    Result := 1;
end;

function TBaseDMXDevice.GetPortDirectionCanChange(aPortIndex: integer): boolean;
begin
  if ValidPortIndex(aPortindex) then
    Result := FPorts[aPortIndex].DirectionCanChange
  else
    Result := FALSE;
end;

function TBaseDMXDevice.GetPortIsOpen(aPortIndex: integer): boolean;
begin
  if ValidPortIndex(aPortindex) then
    Result := FPorts[aPortIndex].IsOpen
  else
    Result := FALSE;
end;

procedure TBaseDMXDevice.SetPortDirection(aPortindex: integer; AValue: TPortDirection);
var s: string;
begin
  if not ValidPortIndex(aPortindex) then exit;
  if not FPorts[aPortindex].DirectionCanChange then exit;
  if FPorts[aPortindex].Direction = AValue then exit;

  // clear buffer
  FPorts[aPortindex].ResetBuffer;

  if AValue = pdIn then
    s := 'input' else s:='output';
  Log.Debug(Name+' Trying to set port '+aPortindex.ToString+' direction to '+s);

  if DoSetPortDirection(aPortindex, AValue) then
  begin
    if PortCount > 1
      then Log.Debug(Name+' Port '+aPortindex.ToString+' sets to '+s)
      else Log.Debug(Name+' sets to '+s);
  end
  else
  begin
    if PortCount > 1
      then Log.Error(Name+' Failed to set port '+aPortindex.ToString+' to '+s)
      else Log.Error(Name+' Failed to set to '+s);
  end;

  FPorts[aPortindex].Direction := AValue;
  FPorts[aPortindex].ResetBuffer;
end;

function TBaseDMXDevice.ValidPortIndexAndPortIsOpened(aPortIndex: integer ): boolean;
begin
  Result := (aPortIndex >= Low(FPorts)) and (aPortIndex <= High(FPorts));
  if Result then
    Result := FPorts[aPortindex].IsOpen;
end;

function TBaseDMXDevice.GetPortCount: integer;
begin
  Result := Length(FPorts);
end;

function TBaseDMXDevice.GetUsedChannelCount(aPortindex: integer): integer;
begin
  if ValidPortIndex(aPortindex) then
    Result := FPorts[aPortindex].UsedChannel
  else
    Result := 0;
end;

constructor TBaseDMXDevice.Create;
begin
  DoInit;
end;

destructor TBaseDMXDevice.Destroy;
begin
  CloseAllPort;
  DoFreeMem;
  inherited Destroy;
end;

function TBaseDMXDevice.Open(aPortindex: integer): boolean;
begin
  if not ValidPortIndex(aPortindex) then
  begin
    Result := FALSE;
    exit;
  end;

  if FPorts[aPortindex].IsOpen then  // interface is already opened
  begin
    Result := TRUE;
    exit;
  end;

  FPorts[aPortindex].IsOpen := DoOpen(aPortindex);
  Result := FPorts[aPortindex].IsOpen;
end;

function TBaseDMXDevice.Close(aPortindex: integer): boolean;
begin
  if ValidPortIndexAndPortIsOpened(aPortindex) then
  begin
    Result := DoClose(aPortindex);
    FPorts[aPortindex].IsOpen := not Result;
    if Result then
      Log.Debug(Name+' port '+aPortindex.ToString+' closed')
    else
      Log.Error(Name+' failed to close the port '+aPortindex.ToString);
  end
  else Result := FALSE;
end;

procedure TBaseDMXDevice.CloseAllPort;
var i: integer;
begin
  for i:=0 to High(FPorts) do
    Close(i);
end;

function TBaseDMXDevice.UpdateChannel(aPortindex, aDMXAdress, aValue: integer): boolean;
begin
  Result := FALSE;
  if not ValidPortIndexAndPortIsOpened(aPortindex) then exit;
  if FPorts[aPortindex].Direction = pdIn then exit;

  if FPorts[aPortindex].DMXBuffer[aDMXAdress] = aValue then
    Result := TRUE
  else
  begin
    FPorts[aPortindex].DMXBuffer[aDMXAdress] := byte(aValue);
    FPorts[aPortindex].BufferChanged := TRUE;
    Result := DoUpdateChannel(aPortindex, aDMXAdress, AValue);
  end;
end;

function TBaseDMXDevice.SendAll(aPortindex: integer): boolean;
begin
  Result := FALSE;
  if not ValidPortIndexAndPortIsOpened(aPortindex) then exit;
  if FPorts[aPortindex].Direction = pdIn then exit;

  if FPorts[aPortindex].BufferChanged then
  begin
    FPorts[aPortindex].BufferChanged := FALSE;
    Result := DoSendAll(aPortindex)
  end
  else Result := TRUE;
end;

function TBaseDMXDevice.ReceiveData(aPortindex: integer): boolean;
begin
  Result := FALSE;
  if not ValidPortIndexAndPortIsOpened(aPortindex) then exit;
  if FPorts[aPortindex].Direction = pdIn then
    Result := DoReceiveData(aPortindex);
end;




{ TDeviceManager }

constructor TDeviceManager.Create;
begin
 FListDeviceFound := TList.Create;
 FNoDevice := TNoDevice.Create;
 Log.Info('Device Manager created');
end;

destructor TDeviceManager.Destroy;
begin
 Log.Info('Destroying Device Manager...');
 ClearList;

 if FK8062Manager <> NIL then
  FK8062Manager.Free;
 if FTDIManager <> NIL then
   FTDIManager.Free;

 FListDeviceFound.Free;
 FNoDevice.Free;
 inherited Destroy;
end;

procedure TDeviceManager.LookForVellemanK8062;
begin
  if FK8062Manager <> NIL then
    FK8062Manager.Free;
  FK8062Manager := TK8062Manager.Create;

  // fill the device list with all Velleman 8062 detected
  Log.Debug('    Start looking for K8062 ...');
  FK8062Manager.LookForDevice;
end;

procedure TDeviceManager.LookForFTDIBasedDevice;
begin
  if FTDIManager = NIL then
    FTDIManager := TFTDIManager.Create;
  Log.Debug('    Start looking for FTDI based device...');
  FTDIManager.LookForDevice;
end;

procedure TDeviceManager.ClearList;
var i: integer;
begin
 for i:=0 to FListDeviceFound.Count-1 do
   TBaseDMXDevice(FListDeviceFound.Items[i]).Free;

 FListDeviceFound.Clear;
end;

function TDeviceManager.GetCount: integer;
begin
 Result := FListDeviceFound.Count;
end;

function TDeviceManager.GetDMXDevice(Index: integer): TBaseDMXDevice;
begin
 if not IsValidIndex(Index) then
   Result := FNoDevice
 else
   Result := TBaseDMXDevice(FListDeviceFound.Items[Index]);
end;

function TDeviceManager.GetPortCount: integer;
var i: integer;
begin
  Result := 0;
  for i:=0 to Count-1 do
    Result := Result+Device[i].PortCount;
end;

function TDeviceManager.IsValidIndex(Index: integer): boolean;
begin
 Result := (Index >= 0) and (Index < FListDeviceFound.Count);
end;

procedure TDeviceManager.LookForAvailableDevices;
var fake: TFakeDeviceMultiPort;
begin
  Log.Debug('    Clear list of device');
  ClearList;
  LookForVellemanK8062;
  LookForFTDIBasedDevice;

  fake := TFakeDeviceMultiPort.Create;
  RegisterDevice(fake);
end;

function TDeviceManager.GetDevicesPath(aIncludeNoDevice: boolean): TArrayOfDevicePath;
var i, j, c, k: integer;
begin
  c := 0;
  for i:=0 to Count-1 do
   c := c+Device[i].PortCount;
  if aIncludeNoDevice then
    inc(c);
  Result := NIL;
  SetLength(Result, c);

  if aIncludeNoDevice then
    j := -1
  else
    j := 0;
  k := 0;
  for i:=j to Count-1 do
   for c:=0 to Device[i].PortCount-1 do
    begin
     Result[k].DeviceIndex := i;
     Result[k].PortIndex := c;
     inc(k);
   end;
end;

procedure TDeviceManager.CloseAll;
var i: integer;
begin
  for i:=0 to FListDeviceFound.Count-1 do
    Device[i].CloseAllPort;
end;

function TDeviceManager.GetDeviceIndexByNameSerial(const aName, aSerialNumber: string): integer;
var i: Integer;
begin
  for i:=0 to FListDeviceFound.Count-1 do
   if (Device[i].Name = aName) and
      (Device[i].SerialNumber = aSerialNumber) then
   begin
     Result := i;
     exit;
   end;

  Result := INVALID_DMXDEVICE_INDEX;
end;

function TDeviceManager.GetDeviceByPath(aPath: TDevicePath): TBaseDMXDevice;
begin
  Result := Device[aPath.DeviceIndex];
end;

function TDeviceManager.DevicePortIsOpen(aDevicePath: TDevicePath): boolean;
var dev: TBaseDMXDevice;
begin
  dev := GetDeviceByPath(aDevicePath);
  Result := dev.PortIsOpen[aDevicePath.PortIndex];
end;

procedure TDeviceManager.RegisterDevice(aDevice: TBaseDMXDevice);
begin
  FListDeviceFound.Add(aDevice);
  Log.Info('    Found '+aDevice.Name+' - Serial '+aDevice.SerialNumber);
end;

end.

