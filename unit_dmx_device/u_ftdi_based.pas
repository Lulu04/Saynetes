unit u_ftdi_based;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  u_dmxdevice_manager, u_utils;

const

// library filename
{$ifdef MSWINDOWS}
  {$ifdef CPU386}
    FTDI_LIBRARY_NAME = 'i386-win32\ftd2xx.dll';
  {$endif}
  {$ifdef CPU64}
    FTDI_LIBRARY_NAME = 'x86_64-win64\ftd2xx64.dll';
  {$endif}
{$endif}

{$ifdef LINUX}
  FTDI_LIBRARY_NAME = 'x86_64-linux/libftd2xx.so.1.4.24';
{$endif}

// Enttec Pro definitions
GET_WIDGET_PARAMS = 3 ;
GET_WIDGET_SN = 10 ;
GET_WIDGET_PARAMS_REPLY = 3 ;
SET_WIDGET_PARAMS = 4 ;
SET_DMX_RX_MODE = 5 ;
SET_DMX_TX_MODE = 6 ;
SEND_DMX_RDM_TX = 7 ;
RECEIVE_DMX_ON_CHANGE = 8 ;
RECEIVED_DMX_COS_TYPE = 9 ;
ONE_BYTE = 1 ;
DMX_START_MESS = $7E ;
DMX_END_MESS = $E7 ;
OFFSET = $FF ;
DMX_HEADER_LENGTH = 4 ;
BYTE_LENGTH = 8 ;
HEADER_RDM_LABEL = 5 ;
NO_RESPONSE = 0 ;
DMX_PACKET_SIZE = 512 ;

MAX_PROS = 20 ;
SEND_NOW = 0 ;
//TRUE 1
//FALSE 0
HEAD = 0 ;
IO_ERROR = 9 ;

DMX_DATA_LENGTH = 513 ; // Includes the start code


type

  DMXUSBPROParamsType=record
    FirmwareLSB,
    FirmwareMSB,
    BreakTime,
    MaBTime,
    RefreshRate: byte;
  end;

  DMXUSBPROSetParamsType=record
    UserSizeLSB,
    UserSizeMSB,
    BreakTime,          // [9..127] * 10.67 microsecond
    MaBTime,            // [1..127] * 10.67 microsecond
    RefreshRate: byte ; // [1..40] dmx frame per sec
  end;


  ReceivedDmxCosStruct=record
    start_changed_byte_number: byte;
    changed_byte_array: array [0..4] of byte;
    changed_byte_data: array[0..39] of byte;
  end;


type

  { TEnttec_UsbDmxPro }

  TEnttec_UsbDmxPro=class(TBaseDMXDevice)
  private
    FDeviceIndex: DWord;                // ftdi device index
    FHandle: DWord;                     // ftdi device handle
    FPRO_Params: DMXUSBPROParamsType;
    FName: string;
    FSerialString: string;
    FDmxData: array[0..599] of byte; // buffer with enough space to send or receive data
    FDataIn: array[0..599] of byte;
    function FTDI_SendMessage(aLabel: integer; data: pointer; length: integer): boolean;
    function FTDI_ReceiveData(aLabel: integer; data: PByte; expected_length,
                              discard: integer; out aWritten: integer): boolean;
    procedure FTDI_PurgeBuffer;
  private
    procedure GetProSerialNumber;
  private
    FInputThread: TTimedThread;
    procedure StartInputThread;
    procedure StopInputThread;
    procedure InputThreadProcess;
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
    procedure InitFromIndex(aFTIndex:dword);
  end;

{
  TEnttec_OpenDmx=class(TBaseDMXDevice)
  private
    FDeviceIndex: DWord;                // ftdi device index
    FHandle: DWord;                     // ftdi device handle
    FName: string;
    FSerialNumber: array[0..3] of byte; // serial number from ftdi chip
    FDmxDataOut: array[0..512] of byte; // start code+dmx
    FDmxDataIn: array[0..512] of byte; // start code+dmx
//    function FTDI_SendData(alabel: integer; data: pointer; length: integer): boolean;
//    function FTDI_ReceiveData(alabel: integer; data: pointer; expected_length: integer): boolean;
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
  //  procedure InitFromIndex(aFTIndex:dword);
  end;
}

  { TFTDIManager }

  TFTDIManager=class
  private
    FLibLoaded: boolean;
    function GetFTDIChipCount: integer;
    function GetFTDIChipDescription(index: integer): string;
    function GetFTDIChipSerial(index: integer): string;
    procedure RegisterENTTEC_DMXUSBPRO(index: dword);
    procedure RegisterENTTEC_OPENDMX(index: dword);
  public
    constructor Create;
    destructor Destroy; override;
    procedure LookForDevice;
  end;

implementation

uses u_resource_string, u_logfile, Forms, ftd2xx{$ifdef LINUX}, Process, Unix{$endif};

{ TEnttec_UsbDmxPro }

function TEnttec_UsbDmxPro.FTDI_SendMessage(aLabel: integer; data: pointer;
  length: integer): boolean;
var end_code: byte;
 res: FT_STATUS;
 written: DWORD;
 header: array[0..DMX_HEADER_LENGTH-1] of byte;
begin
 Result:=FALSE;
 res:=0;
 // Form Packet Header
 header[0]:=DMX_START_MESS;
 header[1]:=aLabel;
 header[2]:=byte(length and OFFSET);
 header[3]:=byte(length shr BYTE_LENGTH);
 // Write The Header
 written:=0;
 res:=FT_Write(FHandle, @header, DMX_HEADER_LENGTH, @written);
 if written<>DMX_HEADER_LENGTH then exit;

 // Write The Data
 if (length<>0) and (data<>NIL) then begin
   written:=0;
   res:=FT_Write(FHandle, data, length, @written);
   if written<>length then exit;
 end;
 // Write End Code
 written:=0;
 end_code:=DMX_END_MESS;
 res:=FT_Write(FHandle, @end_code, ONE_BYTE, @written);
 if written<>ONE_BYTE then exit;

 Result:=res=FT_OK;
end;

function TEnttec_UsbDmxPro.FTDI_ReceiveData(aLabel: integer; data: PByte;
  expected_length, discard: integer; out aWritten: integer): boolean;
var res: FT_STATUS;
 packetLength, bytes_read: DWORD;
 octet: byte;
 i: Integer;
begin
 aWritten:=0;
 Result:=FALSE;
 packetLength:=0;
 bytes_read:=0;
 octet:=0;
 // Check for Start Code and matching Label
 while(octet<>aLabel) do
 begin
   while octet<>DMX_START_MESS do
    begin
      res:=FT_Read(FHandle, @octet, ONE_BYTE, @bytes_read);
      if bytes_read=NO_RESPONSE then begin
        Log.Error('ENTTEC PRO-FTDI_ReceiveData: byte received is not DMX_START_MESS');
        exit;
      end;
    end;
   res:=FT_Read(FHandle, @octet, ONE_BYTE, @bytes_read);
   if bytes_read=NO_RESPONSE then begin
     Log.Error('ENTTEC PRO-FTDI_ReceiveData: label not match');
     exit;
   end;
 end;
 // Read the rest of the Header Byte by Byte -- Get Length
 res:=FT_Read(FHandle, @octet, ONE_BYTE, @bytes_read);
 if bytes_read=NO_RESPONSE then begin
   Log.Error('ENTTEC PRO-FTDI_ReceiveData: Failed to read the first byte of the length');
   exit;
 end;
 packetLength:=octet;
 res:=FT_Read(FHandle, @octet, ONE_BYTE, @bytes_read);
 if res<>FT_OK then begin
   Log.Error('ENTTEC PRO-FTDI_ReceiveData: Failed to read the second byte of the length');
   exit;
 end;
 packetLength:=packetLength+(octet shl BYTE_LENGTH);

 // if needed, discard some byte
 if discard>0 then begin
  res:=FT_Read(FHandle, @FDataIn[0], discard, @bytes_read);
  if bytes_read<>discard then begin
    Log.Error('ENTTEC PRO-FTDI_ReceiveData: Failed to read the bytes to discard');
    exit;
  end;
 end;
 packetLength:=packetLength-discard;

 // Read the actual Response Data
 res:=FT_Read(FHandle, @FDataIn[0], packetLength, @bytes_read);
 if bytes_read<>packetLength then begin
   Log.Error('ENTTEC PRO-FTDI_ReceiveData: Failed to read the data');
   exit;
 end;

 // Check The End Code
 res:=FT_Read(FHandle, @octet, ONE_BYTE, @bytes_read);
 if bytes_read=NO_RESPONSE then begin
   Log.Error('ENTTEC PRO-FTDI_ReceiveData: Failed to read the end code');
   exit;
 end;
 if octet<>DMX_END_MESS then begin
   Log.Error('ENTTEC PRO-FTDI_ReceiveData: expected end code but byte not match');
   exit;
 end;

 if expected_length>packetLength
   then expected_length:=packetLength;

 // Copy The Data read to the buffer passed
 for i:=0 to expected_length-1 do begin
   data^:=FDataIn[i];
   inc(data);
 end;
 aWritten:=expected_length;
 Result:=TRUE;
end;

procedure TEnttec_UsbDmxPro.FTDI_PurgeBuffer;
begin
  FT_Purge (FHandle, FT_PURGE_TX);
  FT_Purge (FHandle, FT_PURGE_RX);
end;

procedure TEnttec_UsbDmxPro.GetProSerialNumber;
var  res: FT_STATUS;
 buf: array of byte;
 c: DWord;
begin
 FSerialString := '????';
 buf := NIL;
 SetLength(buf, 5);
 buf[0]:=DMX_START_MESS;
 buf[1]:=GET_WIDGET_SN;
 buf[2]:=0;
 buf[3]:=0;
 buf[4]:=DMX_END_MESS;
 c:=0;
 res:=FT_Write(FHandle, @buf[0], Length(buf), @c);
 if (res<>FT_OK) or (c<>Length(buf)) then begin
   Log.Warning(Name+' don''t accept serial request');
 end else begin
   SetLength(buf, 9);
   c:=0;
   res:=FT_Read(FHandle, @buf[0], Length(buf), @c);
   if (res=FT_OK) and (c=Length(buf))
     then FSerialString := IntToHex(buf[7], 2)+IntToHex(buf[6], 2)+
                         IntToHex(buf[5], 2)+IntToHex(buf[4], 2)
     else begin
       Log.Warning(Name+' don''t give response to serial request');
     end;
 end;
end;

procedure TEnttec_UsbDmxPro.StartInputThread;
begin
  if FInputThread=NIL then begin
   FInputThread:=TTimedThread.CreateCallback(0, @InputThreadProcess, TRUE);
   Log.Debug(Name+' Thread receive is running');
  end;
end;

procedure TEnttec_UsbDmxPro.StopInputThread;
begin
  if FInputThread<>NIL then begin
    FInputThread.Terminate;
    FInputThread.WaitFor;
    FInputThread.Free;
    FInputThread:=NIL;
    Log.Debug(Name+' Thread receive is stopped');
  end;
end;

procedure TEnttec_UsbDmxPro.InputThreadProcess;
var bytesWaiting, bytes: integer;
 packetLength, bytes_read: DWord;
 buf: array[0..4] of byte;
begin
  bytesWaiting:=0;
  FT_GetQueueStatus(FHandle, @bytesWaiting);
  if bytesWaiting>25 then begin
    FTDI_ReceiveData(SET_DMX_RX_MODE, @FPorts[0].DMXBuffer[1], 512, 2, bytes);
  if bytes <>512 then Log.Warning(Name+' Received '+bytes.ToString+'/512');
  end else Sleep(5);

  exit;








  bytesWaiting:=0;
  FT_GetQueueStatus(FHandle, @bytesWaiting);
  if bytesWaiting>25 then begin
    // Check for Start message
    buf[0]:=0;
    while buf[0]<>DMX_START_MESS do begin
      bytes_read:=0;
      FT_Read(FHandle, @buf[0], 1, @bytes_read);
      if bytes_read<>1 then exit;
    end;

    // check label
    FT_Read(FHandle, @buf[0], 1, @bytes_read);
    if buf[0]<>SET_DMX_RX_MODE then begin
      Log.Error(Name+' Receive unrecognized label '+buf[0].ToString);
      exit;
    end;

    // Message length
    FT_Read(FHandle, @buf[0], 2, @bytes_read);
    if bytes_read<>2 then exit;
    packetLength:=buf[0]+buf[1] shl 1;

    // check status
    FT_Read(FHandle, @buf[0], 1, @bytes_read);
    if (buf[0] AND $01)=$01 then Log.Warning(Name+' Widget receive queue overflowed');
    if (buf[0] AND $02)=$02 then Log.Warning(Name+' Widget receive overrun occurred');

    // check DMX Start code
    FT_Read(FHandle, @buf[0], 1, @bytes_read);
    if buf[0]<>0 then Log.Error(Name+' Received invalid DMX Start Code '+buf[0].ToString);

    // read the data
    packetLength:=packetLength-2;
Log.Debug(Name+' Try to read '+packetLength.ToString+' bytes');
    FT_Read(FHandle, @FPorts[0].DMXBuffer[1], packetLength, @bytes_read);
    if bytes_read<>packetLength then begin
      Log.Error(NAME+' Failed to receive the DMX data. Got '+bytes_read.ToString+'/'+packetLength.ToString);
      exit;
    end;

 {   // Check The End Code
    FT_Read(FHandle, @buf[0], 1, @bytes_read);
    if bytes_read<>1 then begin
      Log.Error('ENTTEC PRO-FTDI_ReceiveData: Failed to read the End Message Code');
      exit;
    end;
    if buf[0]<>DMX_END_MESS then begin
      Log.Error(Name+' Received invalid End Message Code '+buf[0].ToString);
    end;  }
  end else sleep(10);
end;

procedure TEnttec_UsbDmxPro.DoInit;
begin
  FDeviceType:=dtENTTEC_USBDMXPRO;
  SetLength(FPorts, 1);
  FPorts[0].InitByDefault;
  FPorts[0].MinUsedChannel:=24;
  FPorts[0].MaxUsedChannel:=512;
  FPorts[0].UsedChannel:=512;
  FPorts[0].Direction:=pdOut;
  FPorts[0].DirectionCanChange:=TRUE;
end;

procedure TEnttec_UsbDmxPro.DoFreeMem;
begin
 StopInputThread;
end;

function TEnttec_UsbDmxPro.DoOpen(aPortindex: integer): boolean;
var
 tries: integer;
 FLongueurUserConfData: word;
 ftStatus: FT_STATUS;
 bread: integer;
begin
 Result:=FALSE;

 // Try 3 times
 tries:=0;
 repeat
  // Open the PRO
  ftStatus:=FT_Open(FDeviceIndex, @FHandle);
  if ftStatus<>FT_OK then sleep(750);
  inc(tries);
 until (ftStatus=FT_OK) or (tries>3);
 if ftStatus<>FT_OK then exit;

 FT_SetTimeouts(FHandle, 120, 100);
 // Purges the buffer
 FTDI_PurgeBuffer;
 // Send Get Widget Parameters to get Device Info
 FLongueurUserConfData := 0 ;
 if not FTDI_SendMessage(GET_WIDGET_PARAMS, @FLongueurUserConfData, 2)
   then begin
         FT_Purge(FHandle, FT_PURGE_TX);
         if not FTDI_SendMessage(GET_WIDGET_PARAMS, @FLongueurUserConfData, 2)
           then begin
                 FT_Close(FHandle);
                 exit;
                end;
 end;
 // Receive Widget Response
 if not FTDI_ReceiveData(GET_WIDGET_PARAMS_REPLY, @FPRO_Params, sizeof(DMXUSBPROSetParamsType), 0, bread)
   then begin
         // Receive Widget Response packet
         if not FTDI_ReceiveData(GET_WIDGET_PARAMS_REPLY, @FPRO_Params, sizeof(DMXUSBPROParamsType), 0, bread)
           then begin
                 FT_Close(FHandle);
                 exit;
                end;
        end;
 Result:=TRUE;
 // at startup, if a dmx signal is entering the pro, it interferes with
 // before to get the serial number, we set the device to OUTPUT and purge all buffers
 FDmxData[0]:=0;
 FTDI_SendMessage(SET_DMX_TX_MODE, @FDmxData, 25);
 FTDI_PurgeBuffer;
 // GET PRO's serial number
 GetProSerialNumber;
end;

function TEnttec_UsbDmxPro.DoClose(aPortindex: integer): boolean;
begin
 Result:=FT_Close(FHandle)=FT_OK;
end;

function TEnttec_UsbDmxPro.DoSetPortDirection(aPortindex: integer; aDir: TPortDirection): boolean;
var send_allways_flag: byte;
begin
  case aDir of
    pdOut: begin
      // stop the input thread
      StopInputThread;
      // set the device in DMX OUT mode
      FTDI_PurgeBuffer;
      DoSendAll(aPortindex);
      Result:=TRUE;
    end;
    pdIn: begin
      // set the device in DMX IN mode
      send_allways_flag:=0;
      Result:=FTDI_SendMessage(RECEIVE_DMX_ON_CHANGE, @send_allways_flag, 1);
      if Result then begin
        FTDI_PurgeBuffer;
        StartInputThread;
      end else Log.Error(Name+' Failed to set port 0 to INPUT')
    end;
  end;
end;

function TEnttec_UsbDmxPro.DoUpdateChannel(aPortindex, aDMXAdress, aValue: integer ): boolean;
begin
  FDmxData[aDMXAdress]:=aValue;
  Result:=TRUE;
end;

function TEnttec_UsbDmxPro.DoSendAll(aPortindex: integer): boolean;
begin
  FDmxData[0]:=0; // DMX start code
  Result:=FTDI_SendMessage(SET_DMX_TX_MODE, @FDmxData, FPorts[0].UsedChannel+1);
end;

function TEnttec_UsbDmxPro.DoSetUsedChannelCount(aPortindex, aValue: integer): boolean;
begin
  Result:=TRUE;
end;

function TEnttec_UsbDmxPro.DoReceiveData(aPortindex: integer): boolean;
var byteWaiting: DWORD;
 bytes: integer;
begin
  // since the PRO return 514 byte, we discard 2 bytes from the responses
// Log.Debug('ENTTEC PRO-DoReceiveData: before FTDI_ReceiveData ');
  Result:=FTDI_ReceiveData(SET_DMX_RX_MODE, @FPorts[aPortindex].DMXBuffer[1], 512, 2, bytes);
//  Log.Debug('ENTTEC PRO-DoReceiveData: '+bytes.ToString+'/512');

  FT_GetQueueStatus(FHandle, @byteWaiting);
  Log.Debug(Name+' bytes waiting to be read '+byteWaiting.ToString);

{  // if there are more than one frame (514 byte) waiting in the receive buffer, we discard them
  // if we don't do that, the latency increase excessively
  if FT_GetQueueStatus(FHandle, @byteWaiting)=FT_OK
    then if byteWaiting>519 then begin
      FTDI_PurgeBuffer;
      Log.Info(Name+' purge buffer while receiving to avoid latency');
    end;   }
end;

function TEnttec_UsbDmxPro.DoGetName: string;
begin
  Result:=FName;
end;

function TEnttec_UsbDmxPro.DoGetSerialNumber: string;
begin
  Result:=FSerialString;
end;

procedure TEnttec_UsbDmxPro.InitFromIndex(aFTIndex: dword);
begin
  FDeviceIndex:=aFTIndex;
  GetFTDeviceDescription(aFTIndex);
  FName:=FT_Device_String;
  Open(aFTIndex);  // init serial number
end;

{ TFTDIManager }

function TFTDIManager.GetFTDIChipCount: integer;
begin
  Result:=FT_Device_Count;
end;

function TFTDIManager.GetFTDIChipDescription(index: integer): string;
begin
  if GetFTDeviceDescription(index)=FT_OK
    then Result:=FT_Device_String
    else Result:=SError;
end;

function TFTDIManager.GetFTDIChipSerial(index: integer): string;
begin
  if GetFTDeviceSerialNo(index)=FT_OK
    then Result:=FT_Device_String
    else Result:=SError;
end;

procedure TFTDIManager.RegisterENTTEC_DMXUSBPRO(index: dword);
var d: TEnttec_UsbDmxPro;
begin
  d:=TEnttec_UsbDmxPro.Create;
  d.InitFromIndex(index);
  DeviceManager.RegisterDevice(d);
end;

procedure TFTDIManager.RegisterENTTEC_OPENDMX(index: dword);
begin

end;

constructor TFTDIManager.Create;
{$ifdef LINUX} var p: TProcess; {$endif}
begin
  FLibLoaded := Load_FTDLibrary(ConcatPaths([Application.Location, FTDI_LIBRARY_NAME]));
  {$ifdef LINUX}
  p := TProcess.Create(nil);
  p.CommandLine := 'rmmod ftdi_sio';
  p.Options := p.Options + [poWaitOnExit];
  p.Execute;
  p.CommandLine := 'rmmod usbserial';
  p.Execute;
  p.Free;
//  if RunCommand(ConcatPaths([Application.Location,'LinuxFTDIScript.sh']), s)
//    then ShowMess(s,'RunCommand OK');
  // unload the built in ftdi usb to serial driver from kernel
  // necessary to use ftd2xx driver
  //fpSystem('sudo rmmod ftdi_sio');
  //fpSystem('sudo rmmod usbserial');
  {$endif}
end;

destructor TFTDIManager.Destroy;
begin
  Unload_FTDLibrary;
  inherited Destroy;
end;

procedure TFTDIManager.LookForDevice;
var des: string;
 i: integer;

 devInfo: array of FT_Device_Info_Node;
 numDevs: DWord;
 ftHandleTemp: FT_HANDLE;
 Flags,
 ID,
 _Type,
 LocId: DWORD;
 SerialNumber: array[0..15] of char;
 Description: array[0..63] of char;
 begin
  if not FLibLoaded then exit;

  FT_CreateDeviceInfoList(@numDevs);
  if numDevs=0 then exit;

  for i:=0 to numDevs-1 do begin
    if FT_GetDeviceInfoDetail(i, @Flags, @_Type, @ID, @LocId, @SerialNumber[0], @Description[0], @ftHandleTemp)=FT_OK then begin
      des := Description;
      case des of
        'DMX USB PRO': RegisterENTTEC_DMXUSBPRO(i);
        'OPEN DMX': RegisterENTTEC_OPENDMX(i);
      end;//case
    end;
  end;

{  SetLength(devInfo, numDevs);
  if FT_GetDeviceInfoList(@devInfo[0], @numDevs)=FT_OK then begin
    for i:=0 to numDevs-1 do begin
      des:=devInfo[i].Description;
      case des of
        'DMX USB PRO': RegisterENTTEC_DMXUSBPRO(i);
        'OPEN DMX': RegisterENTTEC_OPENDMX(i);
      end;//case
    end;
  end; }

 {  for i:=0 to numDevs-1 do begin
    des:=GetFTDIChipDescription(i);
    case des of
      'DMX USB PRO': RegisterENTTEC_DMXUSBPRO(i);
      'OPEN DMX': RegisterENTTEC_OPENDMX(i);
    end;//case
  end;
  exit;   }


{ GetFTDeviceCount;
 if GetFTDIChipCount=0 then exit;
 for i:=0 to GetFTDIChipCount-1 do begin
   des:=GetFTDIChipDescription(i);
   case des of
     'DMX USB PRO': RegisterENTTEC_DMXUSBPRO(i);
     'OPEN DMX': RegisterENTTEC_OPENDMX(i);
   end;//case
 end; }
end;

end.

