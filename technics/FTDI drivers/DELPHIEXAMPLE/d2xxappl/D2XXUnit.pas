unit D2XXUnit;

interface
Uses Windows,Forms,Dialogs;

Type FT_Result = Integer;

// Exported Functions
Function GetFTDeviceCount : FT_Result;
Function GetFTDeviceDescription( DeviceIndex : DWord ) : FT_Result;
Function GetFTDeviceSerialNo( DeviceIndex : DWord ) : FT_Result;
Function Open_USB_Device : FT_Result;
Function Close_USB_Device : FT_Result;
Function Write_USB_Device_Buffer( Write_Count : Integer ) : Integer;
Function Read_USB_Device_Buffer( Read_Count : Integer ) : Integer;
Function Reset_USB_Device : FT_Result;
Function Purge_USB_Device_Out : FT_Result;
Function Purge_USB_Device_In : FT_Result;
Function Set_USB_Device_RTS : FT_Result;
Function Clr_USB_Device_RTS : FT_Result;
Function Set_USB_Device_DTR : FT_Result;
Function Clr_USB_Device_DTR : FT_Result;
Function Set_USB_Device_BaudRate : FT_Result;
Function Set_USB_Device_DataCharacteristics : FT_Result;
Function Set_USB_Device_FlowControl : FT_Result;
Function Get_USB_Device_ModemStatus : FT_Result;
Function Set_USB_Device_Chars : FT_Result;
Function Set_USB_Device_TimeOuts(ReadTimeOut,WriteTimeOut:DWord) : FT_Result;
Function Get_USB_Device_QueueStatus : FT_Result;
Function Open_USB_Device_By_Serial_Number( Serial_Number : string ) : FT_Result;
Function Open_USB_Device_By_Device_Description( Device_Description : string ) : FT_Result;

Var
// Port Handle Returned by the Open Function
// Used by the Subsequent Function Calls
    FT_HANDLE : DWord = 0;
// Used to handle multiple device instances in future
// versions. Must be set to 0 for now.
    PV_Device : DWord = 0;
// Holding Variables for the current settings
// Can be configured visually using the CFGUnit Unit
// or manually before calling SetUp_USB_Device
    FT_Current_Baud : Dword;
    FT_Current_DataBits : Byte;
    FT_Current_StopBits : Byte;
    FT_Current_Parity : Byte;
    FT_Current_FlowControl : Word;
    FT_RTS_On : Boolean;
    FT_DTR_On : Boolean;
    FT_Event_On : Boolean;
    FT_Error_On : Boolean;
    FT_XON_Value : Byte = $11;
    FT_XOFF_Value : Byte = $13;
    FT_EVENT_Value : Byte = $0;
    FT_ERROR_Value : Byte = $0;
// Used by CFGUnit to flag a bad value
    FT_SetupError : Boolean;
// Used to Return the current Modem Status
    FT_Modem_Status : DWord;
//  Used to return the number of bytes pending
//  in the Rx Buffer Queue
    FT_Q_Bytes : DWord;
//  Used to Enable / Disable the Error Report Dialog
    FT_Enable_Error_Report : Boolean = True;

Const
// FT_Result Values
    FT_OK = 0;
    FT_INVALID_HANDLE = 1;
    FT_DEVICE_NOT_FOUND = 2;
    FT_DEVICE_NOT_OPENED = 3;
    FT_IO_ERROR = 4;
    FT_INSUFFICIENT_RESOURCES = 5;
    FT_INVALID_PARAMETER = 6;
    FT_SUCCESS = FT_OK;
// FT_Open_Ex Flags
    FT_OPEN_BY_SERIAL_NUMBER = 1;
    FT_OPEN_BY_DESCRIPTION = 2;
// FT_List_Devices Flags
    FT_LIST_NUMBER_ONLY = $80000000;
    FT_LIST_BY_INDEX = $40000000;
    FT_LIST_ALL = $20000000;
// Baud Rate Selection
    FT_BAUD_300 = 300;
    FT_BAUD_600 = 600;
    FT_BAUD_1200 = 1200;
    FT_BAUD_2400 = 2400;
    FT_BAUD_4800 = 4800;
    FT_BAUD_9600 = 9600;
    FT_BAUD_14400 = 14400;
    FT_BAUD_19200 = 19200;
    FT_BAUD_38400 = 38400;
    FT_BAUD_57600 = 57600;
    FT_BAUD_115200 = 115200;
    FT_BAUD_230400 = 230400;
    FT_BAUD_460800 = 460800;
    FT_BAUD_921600 = 921600;
// Data Bits Selection
    FT_DATA_BITS_7 = 7;
    FT_DATA_BITS_8 = 8;
// Stop Bits Selection
    FT_STOP_BITS_1 = 0;
    FT_STOP_BITS_2 = 2;
// Parity Selection
    FT_PARITY_NONE = 0;
    FT_PARITY_ODD = 1;
    FT_PARITY_EVEN = 2;
    FT_PARITY_MARK = 3;
    FT_PARITY_SPACE = 4;
// Flow Control Selection
    FT_FLOW_NONE = $0000;
    FT_FLOW_RTS_CTS = $0100;
    FT_FLOW_DTR_DSR = $0200;
    FT_FLOW_XON_XOFF = $0400;
// Purge Commands
    FT_PURGE_RX = 1;
    FT_PURGE_TX = 2;
// IO Buffer Sizes
    FT_In_Buffer_Size = $8000;    // 32k
    FT_In_Buffer_Index = FT_In_Buffer_Size - 1;
    FT_Out_Buffer_Size = $8000;    // 32k
    FT_Out_Buffer_Index = FT_Out_Buffer_Size - 1;
// DLL Name
    FT_DLL_Name = 'ftd2xx.dll';
var
// Declare Input and Output Buffers
   FT_In_Buffer : Array[0..FT_In_Buffer_Index] of byte;
   FT_Out_Buffer : Array[0..FT_Out_Buffer_Index] of byte;
// A variable used to detect time-outs
// Attach a timer to the main project form
// which decrements this every 10mS if
// FT_TimeOut_Count <> 0
   FT_TimeOut_Count : Integer = 0;
// Used to determine how many bytes were
// actually received by FT_Read_Device_All
// in the case of a time-out
   FT_All_Bytes_Received : Integer = 0;
   FT_IO_Status : Ft_Result = FT_OK;
// Used By FT_ListDevices
   FT_Device_Count : DWord;
   FT_Device_String_Buffer : Array [1..50] of Char;
   FT_Device_String : String;


implementation

function FT_Open(PVDevice:Integer; ftHandle:Pointer ) : FT_Result ; stdcall ; External FT_DLL_Name name 'FT_Open';
function FT_Close(ftHandle:Dword) : FT_Result ; stdcall ; External FT_DLL_Name name 'FT_Close';
function FT_Read(ftHandle:Dword; FTInBuf : Pointer; BufferSize : LongInt; ResultPtr : Pointer ) : FT_Result ; stdcall ; External FT_DLL_Name name 'FT_Read';
function FT_Write(ftHandle:Dword; FTOutBuf : Pointer; BufferSize : LongInt; ResultPtr : Pointer ) : FT_Result ; stdcall ; External FT_DLL_Name name 'FT_Write';
function FT_SetBaudRate(ftHandle:Dword;BaudRate:DWord) : FT_Result ; stdcall ; External FT_DLL_Name name 'FT_SetBaudRate';
function FT_SetDataCharacteristics(ftHandle:Dword;WordLength,StopBits,Parity:Byte) : FT_Result ; stdcall ; External FT_DLL_Name name 'FT_SetDataCharacteristics';
function FT_SetFlowControl(ftHandle:Dword;FlowControl:Word;XonChar,XoffChar:Byte) : FT_Result ; stdcall ; External FT_DLL_Name name 'FT_SetFlowControl';
function FT_ResetDevice(ftHandle:Dword) : FT_Result ; stdcall ; External FT_DLL_Name name 'FT_ResetDevice';
function FT_SetDtr(ftHandle:Dword) : FT_Result ; stdcall ; External FT_DLL_Name name 'FT_SetDtr';
function FT_ClrDtr(ftHandle:Dword) : FT_Result ; stdcall ; External FT_DLL_Name name 'FT_ClrDtr';
function FT_SetRts(ftHandle:Dword) : FT_Result ; stdcall ; External FT_DLL_Name name 'FT_SetRts';
function FT_ClrRts(ftHandle:Dword) : FT_Result ; stdcall ; External FT_DLL_Name name 'FT_ClrRts';
function FT_GetModemStatus(ftHandle:Dword;ModemStatus:Pointer) : FT_Result ; stdcall ; External FT_DLL_Name name 'FT_GetModemStatus';
function FT_SetChars(ftHandle:Dword;EventChar,EventCharEnabled,ErrorChar,ErrorCharEnabled : Byte) : FT_Result ; stdcall ; External FT_DLL_Name name 'FT_SetChars';
function FT_Purge(ftHandle:Dword;Mask:Dword) : FT_Result ; stdcall ; External FT_DLL_Name name 'FT_Purge';
function FT_SetTimeouts(ftHandle:Dword;ReadTimeout,WriteTimeout:Dword) : FT_Result ; stdcall ; External FT_DLL_Name name 'FT_SetTimeouts';
function FT_GetQueueStatus(ftHandle:Dword;RxBytes:Pointer) : FT_Result ; stdcall ; External FT_DLL_Name name 'FT_GetQueueStatus';
function FT_GetNumDevices(pvArg1:Pointer;pvArg2:Pointer;dwFlags:Dword) : FT_Result ; stdcall ; External FT_DLL_Name name 'FT_ListDevices';
function FT_ListDevices(pvArg1:Dword;pvArg2:Pointer;dwFlags:Dword) : FT_Result ; stdcall ; External FT_DLL_Name name 'FT_ListDevices';
function FT_OpenEx(pvArg1:Pointer;dwFlags:Dword;ftHandle:Pointer) : FT_Result ; stdcall ; External FT_DLL_Name name 'FT_OpenEx';

Procedure FT_Error_Report(ErrStr: String; PortStatus : Integer);
Var Str : String;
Begin
If Not FT_Enable_Error_Report then Exit;
If PortStatus = FT_OK then Exit;
Case PortStatus of
    FT_INVALID_HANDLE : Str := ErrStr+' - Invalid Handle...';
    FT_DEVICE_NOT_FOUND : Str := ErrStr+' - Device Not Found....';
    FT_DEVICE_NOT_OPENED : Str := ErrStr+' - Device Not Opened...';
    FT_IO_ERROR : Str := ErrStr+' - General IO Error...';
    FT_INSUFFICIENT_RESOURCES : Str := ErrStr+' - Insufficient Resources...';
    FT_INVALID_PARAMETER : Str := ErrStr+' - Invalid Parameter ...';
    End;
MessageDlg(Str, mtError, [mbOk], 0);
End;

Function GetDeviceString : String;
Var I : Integer;
Begin
Result := ''; I := 1;
FT_Device_String_Buffer[50] := Chr(0); // Just in case !
While FT_Device_String_Buffer[I] <> Chr(0) do
  Begin
  Result := Result + FT_Device_String_Buffer[I];
  Inc(I);
  End;
End;

Procedure SetDeviceString ( S : String );
Var I,L : Integer;
Begin
FT_Device_String_Buffer[1] := Chr(0);
L := Length(S);  If L > 49 then L := 49;
If L = 0 then Exit;
For I := 1 to L do FT_Device_String_Buffer[I] := S[I];
FT_Device_String_Buffer[L+1] := Chr(0);
End;

Function GetFTDeviceCount : FT_Result;
Begin
Result := FT_GetNumDevices(@FT_Device_Count,Nil,FT_LIST_NUMBER_ONLY);
If Result <> FT_OK then FT_Error_Report('GetFTDeviceCount',Result);
End;

Function GetFTDeviceSerialNo( DeviceIndex : DWord ) : FT_Result;
Begin
Result := FT_ListDevices(DeviceIndex,@FT_Device_String_Buffer,(FT_OPEN_BY_SERIAL_NUMBER or FT_LIST_BY_INDEX));
If Result = FT_OK then FT_Device_String := GetDeviceString;
If Result <> FT_OK then FT_Error_Report('GetFTDeviceSerialNo',Result);
End;

Function GetFTDeviceDescription( DeviceIndex : DWord ) : FT_Result;
Begin
Result := FT_ListDevices(DeviceIndex,@FT_Device_String_Buffer,(FT_OPEN_BY_DESCRIPTION or FT_LIST_BY_INDEX));
If Result = FT_OK then FT_Device_String := GetDeviceString;
If Result <> FT_OK then FT_Error_Report('GetFTDeviceDescription',Result);
End;

Function Open_USB_Device : FT_Result;
Begin
Result := FT_Open(PV_Device,@FT_Handle);
If Result <> FT_OK then FT_Error_Report('FT_Open',Result);
End;

Function Open_USB_Device_By_Serial_Number( Serial_Number : string ) : FT_Result;
Begin
SetDeviceString(Serial_Number);
Result := FT_OpenEx(@FT_Device_String_Buffer,FT_OPEN_BY_SERIAL_NUMBER,@FT_Handle);
If Result <> FT_OK then FT_Error_Report('Open_USB_Device_By_Serial_Number',Result);
End;

Function Open_USB_Device_By_Device_Description( Device_Description : string ) : FT_Result;
Begin
SetDeviceString(Device_Description);
Result := FT_OpenEx(@FT_Device_String_Buffer,FT_OPEN_BY_DESCRIPTION,@FT_Handle);
If Result <> FT_OK then FT_Error_Report('Open_USB_Device_By_Device_Description',Result);
End;

Function Close_USB_Device : FT_Result;
Begin
Result :=  FT_Close(FT_Handle);
If Result <> FT_OK then FT_Error_Report('FT_Close',Result);
End;

Function Reset_USB_Device : FT_Result;
Begin
Result :=  FT_ResetDevice(FT_Handle);
If Result <> FT_OK then FT_Error_Report('FT_ResetDevice',Result);
End;

Function Purge_USB_Device_Out : FT_Result;
Begin
Result :=  FT_Purge(FT_Handle,FT_PURGE_TX);
If Result <> FT_OK then FT_Error_Report('FT_Purge TX',Result);
End;

Function Purge_USB_Device_In : FT_Result;
Begin
Result :=  FT_Purge(FT_Handle,FT_PURGE_RX);
If Result <> FT_OK then FT_Error_Report('FT_Purge RX',Result);
End;

Function Set_USB_Device_RTS : FT_Result;
Begin
Result :=  FT_SetRTS(FT_Handle);
If Result <> FT_OK then FT_Error_Report('FT_SetRTS',Result);
End;

Function Clr_USB_Device_RTS : FT_Result;
Begin
Result :=  FT_ClrRTS(FT_Handle);
If Result <> FT_OK then FT_Error_Report('FT_ClrRTS',Result);
End;

Function Set_USB_Device_DTR : FT_Result;
Begin
Result :=  FT_SetDTR(FT_Handle);
If Result <> FT_OK then FT_Error_Report('FT_SetDTR',Result);
End;

Function Clr_USB_Device_DTR : FT_Result;
Begin
Result :=  FT_ClrDTR(FT_Handle);
If Result <> FT_OK then FT_Error_Report('FT_ClrDTR',Result);
End;

Function Set_USB_Device_BaudRate : FT_Result;
Begin
Result :=  FT_SetBaudRate(FT_Handle,FT_Current_Baud);
If Result <> FT_OK then FT_Error_Report('FT_SetBaudRate',Result);
End;

Function Set_USB_Device_DataCharacteristics : FT_Result;
Begin
Result :=  FT_SetDataCharacteristics(FT_Handle,FT_Current_DataBits,FT_Current_StopBits,FT_Current_Parity);
If Result <> FT_OK then FT_Error_Report('FT_SetDataCharacteristics',Result);
End;

Function Set_USB_Device_FlowControl : FT_Result;
Begin
Result :=  FT_SetFlowControl(FT_Handle,FT_Current_FlowControl,FT_XON_Value,FT_XOFF_Value);
If Result <> FT_OK then FT_Error_Report('FT_SetFlowControl',Result);
End;

Function Get_USB_Device_ModemStatus : FT_Result;
Begin
Result :=  FT_GetModemStatus(FT_Handle,@FT_Modem_Status);
If Result <> FT_OK then FT_Error_Report('FT_GetModemStatus',Result);
End;

Function Set_USB_Device_Chars : FT_Result;
Var Events_On,Errors_On : Byte;
Begin
If FT_Event_On then Events_On := 1 else Events_On := 0;
If FT_Error_On then Errors_On := 1 else Errors_On := 0;
Result :=  FT_SetChars(FT_Handle,FT_EVENT_Value,Events_On,FT_ERROR_Value,Errors_On);
If Result <> FT_OK then FT_Error_Report('FT_SetChars',Result);
End;

Function Set_USB_Device_TimeOuts(ReadTimeOut,WriteTimeOut:DWord) : FT_Result;
Begin
Result :=  FT_SetTimeouts(FT_Handle,ReadTimeout,WriteTimeout);
If Result <> FT_OK then FT_Error_Report('FT_SetTimeouts',Result);
End;

Function Get_USB_Device_QueueStatus : FT_Result;
Begin
Result :=  FT_GetQueueStatus(FT_Handle,@FT_Q_Bytes);
If Result <> FT_OK then FT_Error_Report('FT_GetQueueStatus',Result);
End;

function Write_USB_Device_Buffer( Write_Count : Integer ) : Integer;
// Writes Write_Count Bytes from FT_Out_Buffer to the USB device
// Function returns the number of bytes actually sent
// In this example, Write_Count should be 32k bytes max
Var Write_Result : Integer;
Begin
FT_IO_Status := FT_Write(FT_Handle,@FT_Out_Buffer,Write_Count,@Write_Result);
If FT_IO_Status <> FT_OK then FT_Error_Report('FT_Write',FT_IO_Status);
Result := Write_Result;
End;

function Read_USB_Device_Buffer( Read_Count : Integer ) : Integer;
// Reads Read_Count Bytes ( or less ) from the USB device to the FT_In_Buffer
// Function returns the number of bytes actually received  which may range from zero
// to the actual number of bytes requested, depending on how many have been received
// at the time of the request + the read timeout value.

Var Read_Result : Integer;
Begin
FT_IO_Status := FT_Read(FT_Handle,@FT_In_Buffer,Read_Count,@Read_Result);
If FT_IO_Status <> FT_OK then FT_Error_Report('FT_Read',FT_IO_Status);
Result := Read_Result;
End;


end.
