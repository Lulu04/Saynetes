unit ftd2xx;

{

Copyright © 2001-2021 Future Technology Devices International Limited

THIS SOFTWARE IS PROVIDED BY FUTURE TECHNOLOGY DEVICES INTERNATIONAL LIMITED "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL
FUTURE TECHNOLOGY DEVICES INTERNATIONAL LIMITED BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
OF SUBSTITUTE GOODS OR SERVICES LOSS OF USE, DATA, OR PROFITS OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

FTDI DRIVERS MAY BE USED ONLY IN CONJUNCTION WITH PRODUCTS BASED ON FTDI PARTS.

FTDI DRIVERS MAY BE DISTRIBUTED IN ANY FORM AS LONG AS LICENSE INFORMATION IS NOT MODIFIED.

IF A CUSTOM VENDOR ID AND/OR PRODUCT ID OR DESCRIPTION STRING ARE USED, IT IS THE
RESPONSIBILITY OF THE PRODUCT MANUFACTURER TO MAINTAIN ANY CHANGES AND SUBSEQUENT WHQL
RE-CERTIFICATION AS A RESULT OF MAKING THESE CHANGES.


Module Name:

ftd2xx.h

Abstract:

Native USB device driver for FTDI FT232x, FT245x, FT2232x, FT4232x, FT2233H and FT4233H devices
FTD2XX library definitions

Environment:

kernel & user mode
}

{
 adapted for FreePascal/Lazarus by Lulu - 2022
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dynlibs,
{$ifdef MSWINDOWS}
  windows,
{$endif}
  Dialogs;

const
// library filename
{$ifdef MSWINDOWS}
  {$ifdef CPU386}
    FT_LIB_Name = 'i386-win32\ftd2xx.dll';
  {$endif}
  {$ifdef CPU64}
    FT_LIB_Name = 'x86_64-win64\ftd2xx64.dll';
  {$endif}
{$endif}

{$ifdef LINUX}
  FT_LIB_Name = 'x86_64-linux/libftd2xx.so.1.4.24';
{$endif}

// The following ifdef block is the standard way of creating macros
// which make exporting from a DLL simpler.  All files within this DLL
// are compiled with the FTD2XX_EXPORTS symbol defined on the command line.
// This symbol should not be defined on any project that uses this DLL.
// This way any other project whose source files include this file see
// FTD2XX_API functions as being imported from a DLL, whereas this DLL
// sees symbols defined with this macro as being exported.
{
#ifdef FTD2XX_EXPORTS
#define FTD2XX_API __declspec(dllexport)
#elif defined(FTD2XX_STATIC)
// Avoid decorations when linking statically to D2XX.
#define FTD2XX_API
// Static D2XX depends on these Windows libs:
#pragma comment(lib, "setupapi.lib")
#pragma comment(lib, "advapi32.lib")
#pragma comment(lib, "user32.lib")
#else
#define FTD2XX_API __declspec(dllimport)
#endif

#else // _WIN32
// Compiling on non-Windows platform.
#include "WinTypes.h"
// No decorations needed.
#define FTD2XX_API

{$endif}  // Windows
}

{$ifdef Linux}
type
  HANDLE = System.THandle;
  OVERLAPPED = record
     Internal : DWORD;
     InternalHigh : DWORD;
     Offset : DWORD;
     OffsetHigh : DWORD;
     hEvent : HANDLE;
  end;
  LPOVERLAPPED = ^OVERLAPPED;
  ULONG = cardinal;
  SECURITY_ATTRIBUTES = record
       nLength : DWORD;
       lpSecurityDescriptor : Pointer;
       bInheritHandle : LongBool;
    end;
  LPSECURITY_ATTRIBUTES = ^SECURITY_ATTRIBUTES;
  PULONG  = ^cardinal;
  LPWORD = ^Word;
  UShort = Word;
  LPLONG = ^longint;
{$endif}

  type
    FT_HANDLE = DWord;
    FT_STATUS = DWord;
    FT_Result = DWord;

    PFT_HANDLE = ^FT_HANDLE;
    LPDWORD=^DWord;


  const
//
// Device status
//
    FT_OK = 0;
    FT_INVALID_HANDLE = 1;
    FT_DEVICE_NOT_FOUND = 2;
    FT_DEVICE_NOT_OPENED = 3;
    FT_IO_ERROR = 4;
    FT_INSUFFICIENT_RESOURCES = 5;
    FT_INVALID_PARAMETER = 6;
    FT_INVALID_BAUD_RATE = 7;

    FT_DEVICE_NOT_OPENED_FOR_ERASE = 8;
    FT_DEVICE_NOT_OPENED_FOR_WRITE = 9;
    FT_FAILED_TO_WRITE_DEVICE = 10;
    FT_EEPROM_READ_FAILED = 11;
    FT_EEPROM_WRITE_FAILED = 12;
    FT_EEPROM_ERASE_FAILED = 13;
    FT_EEPROM_NOT_PRESENT = 14;
    FT_EEPROM_NOT_PROGRAMMED = 15;
    FT_INVALID_ARGS = 16;
    FT_NOT_SUPPORTED = 17;
    FT_OTHER_ERROR = 18;
    FT_DEVICE_LIST_NOT_READY = 19;



//function FT_SUCCESS(status: FT_STATUS): boolean;// ((status) == FT_OK)

//
// FT_OpenEx Flags
//
    FT_OPEN_BY_SERIAL_NUMBER = 1;
    FT_OPEN_BY_DESCRIPTION = 2;
    FT_OPEN_BY_LOCATION = 4;
    FT_OPEN_MASK = FT_OPEN_BY_SERIAL_NUMBER or
                   FT_OPEN_BY_DESCRIPTION or
                   FT_OPEN_BY_LOCATION;

//
// FT_ListDevices Flags (used in conjunction with FT_OpenEx Flags
//
    FT_LIST_NUMBER_ONLY = $80000000;
    FT_LIST_BY_INDEX = $40000000;
    FT_LIST_ALL = $20000000;
    FT_LIST_MASK = FT_LIST_NUMBER_ONLY or FT_LIST_BY_INDEX or FT_LIST_ALL;

//
// Baud Rates
//
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

//
// Word Lengths
//
    FT_BITS_8: byte = 8;
    FT_BITS_7: byte = 7;

//
// Stop Bits
//
    FT_STOP_BITS_1: byte = 0;
    FT_STOP_BITS_2: byte = 2;

//
// Parity
//
    FT_PARITY_NONE: byte = 0;
    FT_PARITY_ODD: byte = 1;
    FT_PARITY_EVEN: byte = 2;
    FT_PARITY_MARK: byte = 3;
    FT_PARITY_SPACE: byte = 4;

//
// Flow Control
//
    FT_FLOW_NONE = $0000;
    FT_FLOW_RTS_CTS = $0100;
    FT_FLOW_DTR_DSR = $0200;
    FT_FLOW_XON_XOFF = $0400;

//
// Purge rx and tx buffers
//
    FT_PURGE_RX	= 1;
    FT_PURGE_TX = 2;

//
// Events
//
//typedef void(*PFT_EVENT_HANDLER)(DWORD, DWORD);
  const
    FT_EVENT_RXCHAR = 1;
    FT_EVENT_MODEM_STATUS = 2;
    FT_EVENT_LINE_STATUS = 4;

//
// Timeouts
//
    FT_DEFAULT_RX_TIMEOUT = 300;
    FT_DEFAULT_TX_TIMEOUT = 300;

//
// Device types
//
  type
    FT_DEVICE = cardinal; //typedef ULONG	FT_DEVICE;
    PFT_DEVICE=^FT_DEVICE;
  const
    FT_DEVICE_BM = 0;
    FT_DEVICE_AM = 1;
    FT_DEVICE_100AX = 2;
    FT_DEVICE_UNKNOWN = 3;
    FT_DEVICE_2232C = 4;
    FT_DEVICE_232R = 5;
    FT_DEVICE_2232H = 6;
    FT_DEVICE_4232H = 7;
    FT_DEVICE_232H = 8;
    FT_DEVICE_X_SERIES = 9;
    FT_DEVICE_4222H_0 = 10;
    FT_DEVICE_4222H_1_2 = 11;
    FT_DEVICE_4222H_3 = 12;
    FT_DEVICE_4222_PROG = 13;
    FT_DEVICE_900 = 14;
    FT_DEVICE_930 = 15;
    FT_DEVICE_UMFTPD3A = 16;
    FT_DEVICE_2233HP = 17;
    FT_DEVICE_4233HP = 18;
    FT_DEVICE_2232HP = 19;
    FT_DEVICE_4232HP = 20;
    FT_DEVICE_233HP = 21;
    FT_DEVICE_232HP = 22;
    FT_DEVICE_2232HA = 23;
    FT_DEVICE_4232HA = 24;
    FT_DEVICE_232RN = 25;

//
// Bit Modes
//
    FT_BITMODE_RESET = $00;
    FT_BITMODE_ASYNC_BITBANG = $01;
    FT_BITMODE_MPSSE = $02;
    FT_BITMODE_SYNC_BITBANG = $04;
    FT_BITMODE_MCU_HOST = $08;
    FT_BITMODE_FAST_SERIAL = $10;
    FT_BITMODE_CBUS_BITBANG = $20;
    FT_BITMODE_SYNC_FIFO = $40;

//
// FT232R CBUS Options EEPROM values
//
    FT_232R_CBUS_TXDEN = $00;	//	Tx Data Enable
    FT_232R_CBUS_PWRON = $01;	//	Power On
    FT_232R_CBUS_RXLED = $02;	//	Rx LED
    FT_232R_CBUS_TXLED = $03;	//	Tx LED
    FT_232R_CBUS_TXRXLED = $04;	//	Tx and Rx LED
    FT_232R_CBUS_SLEEP = $05;	//	Sleep
    FT_232R_CBUS_CLK48 = $06;	//	48MHz clock
    FT_232R_CBUS_CLK24 = $07;	//	24MHz clock
    FT_232R_CBUS_CLK12 = $08;	//	12MHz clock
    FT_232R_CBUS_CLK6 = $09;	//	6MHz clock
    FT_232R_CBUS_IOMODE = $0A;	//	IO Mode for CBUS bit-bang
    FT_232R_CBUS_BITBANG_WR = $0B;	//	Bit-bang write strobe
    FT_232R_CBUS_BITBANG_RD = $0C;	//	Bit-bang read strobe

//
// FT232H CBUS Options EEPROM values
//
    FT_232H_CBUS_TRISTATE = $00;	//	Tristate
    FT_232H_CBUS_TXLED = $01;	//	Tx LED
    FT_232H_CBUS_RXLED = $02;	//	Rx LED
    FT_232H_CBUS_TXRXLED = $03;	//	Tx and Rx LED
    FT_232H_CBUS_PWREN = $04;	//	Power Enable
    FT_232H_CBUS_SLEEP = $05;	//	Sleep
    FT_232H_CBUS_DRIVE_0 = $06;	//	Drive pin to logic 0
    FT_232H_CBUS_DRIVE_1 = $07;	//	Drive pin to logic 1
    FT_232H_CBUS_IOMODE = $08;	//	IO Mode for CBUS bit-bang
    FT_232H_CBUS_TXDEN = $09;	//	Tx Data Enable
    FT_232H_CBUS_CLK30 = $0A;	//	30MHz clock
    FT_232H_CBUS_CLK15 = $0B;	//	15MHz clock
    FT_232H_CBUS_CLK7_5 = $0C;	//	7.5MHz clock

//
// FT X Series CBUS Options EEPROM values
//
    FT_X_SERIES_CBUS_TRISTATE = $00;	//	Tristate
    FT_X_SERIES_CBUS_TXLED = $01;	//	Tx LED
    FT_X_SERIES_CBUS_RXLED = $02;	//	Rx LED
    FT_X_SERIES_CBUS_TXRXLED = $03;	//	Tx and Rx LED
    FT_X_SERIES_CBUS_PWREN = $04;	//	Power Enable
    FT_X_SERIES_CBUS_SLEEP = $05;	//	Sleep
    FT_X_SERIES_CBUS_DRIVE_0 = $06;	//	Drive pin to logic 0
    FT_X_SERIES_CBUS_DRIVE_1 = $07;	//	Drive pin to logic 1
    FT_X_SERIES_CBUS_IOMODE = $08;	//	IO Mode for CBUS bit-bang
    FT_X_SERIES_CBUS_TXDEN = $09;	//	Tx Data Enable
    FT_X_SERIES_CBUS_CLK24 = $0A;	//	24MHz clock
    FT_X_SERIES_CBUS_CLK12 = $0B;	//	12MHz clock
    FT_X_SERIES_CBUS_CLK6 = $0C;	//	6MHz clock
    FT_X_SERIES_CBUS_BCD_CHARGER = $0D;	//	Battery charger detected
    FT_X_SERIES_CBUS_BCD_CHARGER_N = $0E;	//	Battery charger detected inverted
    FT_X_SERIES_CBUS_I2C_TXE = $0F;	//	I2C Tx empty
    FT_X_SERIES_CBUS_I2C_RXF = $10;	//	I2C Rx full
    FT_X_SERIES_CBUS_VBUS_SENSE = $11;	//	Detect VBUS
    FT_X_SERIES_CBUS_BITBANG_WR = $12;	//	Bit-bang write strobe
    FT_X_SERIES_CBUS_BITBANG_RD = $13;	//	Bit-bang read strobe
    FT_X_SERIES_CBUS_TIMESTAMP = $14;	//	Toggle output when a USB SOF token is received
    FT_X_SERIES_CBUS_KEEP_AWAKE = $15;	//


// Driver types
    FT_DRIVER_TYPE_D2XX = 0;
    FT_DRIVER_TYPE_VCP = 1;

{
#ifdef FTD2XX_STATIC
	FTD2XX_API
		FT_STATUS WINAPI FT_Initialise(
		void
		);

	FTD2XX_API
		void WINAPI FT_Finalise(
		void
		);
#endif // FTD2XX_STATIC
}
{
    function FT_Open(Index: Integer; ftHandle: PFT_HANDLE):FT_Result; stdcall; External FT_DLL_Name name 'FT_Open';
    function FT_OpenEx(pvArg1:Pointer; dwFlags:Dword; ftHandle:Pointer):FT_Result; stdcall; External FT_DLL_Name name 'FT_OpenEx';
    function FT_ListDevices(pvArg1:Dword; pvArg2:Pointer; dwFlags:Dword):FT_Result; stdcall; External FT_DLL_Name name 'FT_ListDevices';
    function FT_Close(ftHandle: FT_HANDLE):FT_Result; stdcall; External FT_DLL_Name name 'FT_Close';
    function FT_Read(ftHandle: FT_HANDLE; FTInBuf: Pointer; dwBytesToRead: DWord; BytesReturned: LPDWORD):FT_Result; stdcall; External FT_DLL_Name name 'FT_Read';
    function FT_Write(ftHandle: FT_HANDLE; FTOutBuf:Pointer; BufferSize:LongInt; BytesWritten: LPDWORD):FT_Result; stdcall; External FT_DLL_Name name 'FT_Write';
    function FT_IoCtl(ftHandle: FT_HANDLE; dwIoControlCode: DWord; lpInBuf: Pointer; nInBufSize: DWord;
                  lpOutBuf: Pointer; nOutBufSize: DWord; lpBytesReturned:LPDWORD; lpOverlapped: LPOVERLAPPED):FT_Result; stdcall; External FT_DLL_Name name 'FT_IoCtl';
    function FT_SetBaudRate(ftHandle: FT_HANDLE; BaudRate:DWord):FT_Result; stdcall; External FT_DLL_Name name 'FT_SetBaudRate';
    function FT_SetDivisor(ftHandle: FT_HANDLE; Divisor:DWord):FT_Result; stdcall; External FT_DLL_Name name 'FT_SetDivisor';
    function FT_SetDataCharacteristics(ftHandle:FT_HANDLE; WordLength,StopBits,Parity:Byte):FT_Result; stdcall; External FT_DLL_Name name 'FT_SetDataCharacteristics';
    function FT_SetFlowControl(ftHandle:FT_HANDLE; FlowControl:Word; XonChar,XoffChar:Byte):FT_Result; stdcall; External FT_DLL_Name name 'FT_SetFlowControl';
    function FT_ResetDevice(ftHandle:FT_HANDLE):FT_Result; stdcall; External FT_DLL_Name name 'FT_ResetDevice';
    function FT_SetDtr(ftHandle:FT_HANDLE):FT_Result; stdcall; External FT_DLL_Name name 'FT_SetDtr';
    function FT_ClrDtr(ftHandle:FT_HANDLE):FT_Result; stdcall; External FT_DLL_Name name 'FT_ClrDtr';
    function FT_SetRts(ftHandle:FT_HANDLE):FT_Result; stdcall; External FT_DLL_Name name 'FT_SetRts';
    function FT_ClrRts(ftHandle:FT_HANDLE):FT_Result; stdcall; External FT_DLL_Name name 'FT_ClrRts';
    function FT_GetModemStatus(ftHandle:FT_HANDLE; ModemStatus:Pointer):FT_Result; stdcall; External FT_DLL_Name name 'FT_GetModemStatus';
    function FT_SetChars(ftHandle:FT_HANDLE; EventChar,EventCharEnabled,ErrorChar,ErrorCharEnabled:Byte):FT_Result; stdcall; External FT_DLL_Name name 'FT_SetChars';
    function FT_Purge(ftHandle:FT_HANDLE; Mask:Dword):FT_Result; stdcall; External FT_DLL_Name name 'FT_Purge';
    function FT_SetTimeouts(ftHandle:FT_HANDLE; ReadTimeout,WriteTimeout:Dword):FT_Result; stdcall; External FT_DLL_Name name 'FT_SetTimeouts';
    function FT_GetQueueStatus(ftHandle:FT_HANDLE; RxBytes:LPDWORD):FT_Result; stdcall; External FT_DLL_Name name 'FT_GetQueueStatus';
    function FT_SetEventNotification(ftHandle:FT_HANDLE; EventMask:DWord; pvArgs:Pointer):FT_Result; stdcall; External FT_DLL_Name name 'FT_SetEventNotification';
    function FT_GetStatus(ftHandle:FT_HANDLE; RxBytes,TxBytes,EventStatus:PWord):FT_Result; stdcall; External FT_DLL_Name name 'FT_GetStatus';
    function FT_SetBreakOn(ftHandle:FT_HANDLE) : FT_Result; stdcall; External FT_DLL_Name name 'FT_SetBreakOn';
    function FT_SetBreakOff(ftHandle:FT_HANDLE) : FT_Result; stdcall; External FT_DLL_Name name 'FT_SetBreakOff';
    function FT_SetWaitMask(ftHandle:FT_HANDLE; Mask: DWord): FT_Result; stdcall; External FT_DLL_Name name 'FT_SetWaitMask';
    function FT_WaitOnMask(ftHandle:FT_HANDLE; pMask: PDWord): FT_Result; stdcall; External FT_DLL_Name name 'FT_WaitOnMask';
    function FT_GetEventStatus(ftHandle:FT_HANDLE;dwEventDWord: PDWord): FT_Result; stdcall; External FT_DLL_Name name 'FT_GetEventStatus';
    function FT_ReadEE(ftHandle:FT_HANDLE; WordAddr:DWord; WordRead:PWord):FT_Result; stdcall; External FT_DLL_Name name 'FT_ReadEE';
    function FT_WriteEE(ftHandle:FT_HANDLE; WordAddr:DWord; WordData:word):FT_Result; stdcall; External FT_DLL_Name name 'FT_WriteEE';
    function FT_EraseEE(ftHandle:FT_HANDLE):FT_Result; stdcall; External FT_DLL_Name name 'FT_EraseEE';
}
  type
       //
	// structure to hold program data for FT_EE_Program, FT_EE_ProgramEx, FT_EE_Read 
	// and FT_EE_ReadEx functions
	//
	FT_PROGRAM_DATA=record
          Signature1: DWORD;			// Header - must be 0x00000000
          Signature2:DWORD;			// Header - must be 0xffffffff
          Version:DWORD;				// Header - FT_PROGRAM_DATA version
		//			0 = original
		//			1 = FT2232 extensions
		//			2 = FT232R extensions
		//			3 = FT2232H extensions
		//			4 = FT4232H extensions
		//			5 = FT232H extensions

		VendorId: WORD;				// 0x0403
		ProductId: WORD;				// 0x6001
		Manufacturer: PChar;			// "FTDI"
		ManufacturerId: PChar;		// "FT"
		Description: PChar;			// "USB HS Serial Converter"
		SerialNumber: PChar;			// "FT000001" if fixed, or NULL
		MaxPower:WORD;				// 0 < MaxPower <= 500
		PnP:WORD;					// 0 = disabled, 1 = enabled
		SelfPowered:WORD;			// 0 = bus powered, 1 = self powered
		RemoteWakeup:WORD;			// 0 = not capable, 1 = capable
		//
		// Rev4 (FT232B) extensions
		//
		Rev4:byte;					// non-zero if Rev4 chip, zero otherwise
		IsoIn:byte;				// non-zero if in endpoint is isochronous
		IsoOut:byte;				// non-zero if out endpoint is isochronous
		PullDownEnable:byte;		// non-zero if pull down enabled
		SerNumEnable:byte;			// non-zero if serial number to be used
		USBVersionEnable:byte;		// non-zero if chip uses USBVersion
		USBVersion:WORD;			// BCD (0x0200 => USB2)
		//
		// Rev 5 (FT2232) extensions
		//
		Rev5,					// non-zero if Rev5 chip, zero otherwise
		IsoInA,				// non-zero if in endpoint is isochronous
		IsoInB,				// non-zero if in endpoint is isochronous
		IsoOutA,				// non-zero if out endpoint is isochronous
		IsoOutB,				// non-zero if out endpoint is isochronous
		PullDownEnable5,		// non-zero if pull down enabled
		SerNumEnable5,		// non-zero if serial number to be used
		USBVersionEnable5:byte;	// non-zero if chip uses USBVersion
		USBVersion5:WORD;			// BCD (0x0200 => USB2)
		AIsHighCurrent,		// non-zero if interface is high current
		BIsHighCurrent,		// non-zero if interface is high current
		IFAIsFifo,			// non-zero if interface is 245 FIFO
		IFAIsFifoTar,			// non-zero if interface is 245 FIFO CPU target
		IFAIsFastSer,			// non-zero if interface is Fast serial
		AIsVCP,				// non-zero if interface is to use VCP drivers
		IFBIsFifo,			// non-zero if interface is 245 FIFO
		IFBIsFifoTar,			// non-zero if interface is 245 FIFO CPU target
		IFBIsFastSer,			// non-zero if interface is Fast serial
		BIsVCP:byte;				// non-zero if interface is to use VCP drivers
		//
		// Rev 6 (FT232R) extensions
		//
		UseExtOsc,			// Use External Oscillator
		HighDriveIOs,			// High Drive I/Os
		EndpointSize,			// Endpoint size
		PullDownEnableR,		// non-zero if pull down enabled
		SerNumEnableR,		// non-zero if serial number to be used
		InvertTXD,			// non-zero if invert TXD
		InvertRXD,			// non-zero if invert RXD
		InvertRTS,			// non-zero if invert RTS
		InvertCTS,			// non-zero if invert CTS
		InvertDTR,			// non-zero if invert DTR
		InvertDSR,			// non-zero if invert DSR
		InvertDCD,			// non-zero if invert DCD
		InvertRI,				// non-zero if invert RI
		Cbus0,				// Cbus Mux control
		Cbus1,				// Cbus Mux control
		Cbus2,				// Cbus Mux control
		Cbus3,				// Cbus Mux control
		Cbus4,				// Cbus Mux control
		RIsD2XX:byte;				// non-zero if using D2XX driver
		//
		// Rev 7 (FT2232H) Extensions
		//
		PullDownEnable7,		// non-zero if pull down enabled
		SerNumEnable7,		// non-zero if serial number to be used
		ALSlowSlew,			// non-zero if AL pins have slow slew
		ALSchmittInput,		// non-zero if AL pins are Schmitt input
		ALDriveCurrent,		// valid values are 4mA, 8mA, 12mA, 16mA
		AHSlowSlew,			// non-zero if AH pins have slow slew
		AHSchmittInput,		// non-zero if AH pins are Schmitt input
		AHDriveCurrent,		// valid values are 4mA, 8mA, 12mA, 16mA
		BLSlowSlew,			// non-zero if BL pins have slow slew
		BLSchmittInput,		// non-zero if BL pins are Schmitt input
		BLDriveCurrent,		// valid values are 4mA, 8mA, 12mA, 16mA
		BHSlowSlew,			// non-zero if BH pins have slow slew
		BHSchmittInput,		// non-zero if BH pins are Schmitt input
		BHDriveCurrent,		// valid values are 4mA, 8mA, 12mA, 16mA
		IFAIsFifo7,			// non-zero if interface is 245 FIFO
		IFAIsFifoTar7,		// non-zero if interface is 245 FIFO CPU target
		IFAIsFastSer7,		// non-zero if interface is Fast serial
		AIsVCP7,				// non-zero if interface is to use VCP drivers
		IFBIsFifo7,			// non-zero if interface is 245 FIFO
		IFBIsFifoTar7,		// non-zero if interface is 245 FIFO CPU target
		IFBIsFastSer7,		// non-zero if interface is Fast serial
		BIsVCP7,				// non-zero if interface is to use VCP drivers
		PowerSaveEnable:byte;		// non-zero if using BCBUS7 to save power for self-powered designs
		//
		// Rev 8 (FT4232H) Extensions
		//
		PullDownEnable8,		// non-zero if pull down enabled
		SerNumEnable8,		// non-zero if serial number to be used
		ASlowSlew,			// non-zero if A pins have slow slew
		ASchmittInput,		// non-zero if A pins are Schmitt input
		ADriveCurrent,		// valid values are 4mA, 8mA, 12mA, 16mA
		BSlowSlew,			// non-zero if B pins have slow slew
		BSchmittInput,		// non-zero if B pins are Schmitt input
		BDriveCurrent,		// valid values are 4mA, 8mA, 12mA, 16mA
		CSlowSlew,			// non-zero if C pins have slow slew
		CSchmittInput,		// non-zero if C pins are Schmitt input
		CDriveCurrent,		// valid values are 4mA, 8mA, 12mA, 16mA
		DSlowSlew,			// non-zero if D pins have slow slew
		DSchmittInput,		// non-zero if D pins are Schmitt input
		DDriveCurrent,		// valid values are 4mA, 8mA, 12mA, 16mA
		ARIIsTXDEN,			// non-zero if port A uses RI as RS485 TXDEN
		BRIIsTXDEN,			// non-zero if port B uses RI as RS485 TXDEN
		CRIIsTXDEN,			// non-zero if port C uses RI as RS485 TXDEN
		DRIIsTXDEN,			// non-zero if port D uses RI as RS485 TXDEN
		AIsVCP8,				// non-zero if interface is to use VCP drivers
		BIsVCP8,				// non-zero if interface is to use VCP drivers
		CIsVCP8,				// non-zero if interface is to use VCP drivers
		DIsVCP8:byte;				// non-zero if interface is to use VCP drivers
		//
		// Rev 9 (FT232H) Extensions
		//
		PullDownEnableH,		// non-zero if pull down enabled
		SerNumEnableH,		// non-zero if serial number to be used
		ACSlowSlewH,			// non-zero if AC pins have slow slew
		ACSchmittInputH,		// non-zero if AC pins are Schmitt input
		ACDriveCurrentH,		// valid values are 4mA, 8mA, 12mA, 16mA
		ADSlowSlewH,			// non-zero if AD pins have slow slew
		ADSchmittInputH,		// non-zero if AD pins are Schmitt input
		ADDriveCurrentH,		// valid values are 4mA, 8mA, 12mA, 16mA
		Cbus0H,				// Cbus Mux control
		Cbus1H,				// Cbus Mux control
		Cbus2H,				// Cbus Mux control
		Cbus3H,				// Cbus Mux control
		Cbus4H,				// Cbus Mux control
		Cbus5H,				// Cbus Mux control
		Cbus6H,				// Cbus Mux control
		Cbus7H,				// Cbus Mux control
		Cbus8H,				// Cbus Mux control
		Cbus9H,				// Cbus Mux control
		IsFifoH,				// non-zero if interface is 245 FIFO
		IsFifoTarH,			// non-zero if interface is 245 FIFO CPU target
		IsFastSerH,			// non-zero if interface is Fast serial
		IsFT1248H,			// non-zero if interface is FT1248
		FT1248CpolH,			// FT1248 clock polarity - clock idle high (1) or clock idle low (0)
		FT1248LsbH,			// FT1248 data is LSB (1) or MSB (0)
		FT1248FlowControlH,	// FT1248 flow control enable
		IsVCPH,				// non-zero if interface is to use VCP drivers
		PowerSaveEnableH:byte;		// non-zero if using ACBUS7 to save power for self-powered designs

	end;
        PFT_PROGRAM_DATA=^FT_PROGRAM_DATA;
{    function FT_EE_Program(ftHandle:FT_HANDLE; pEEData:PFT_PROGRAM_DATA):FT_Result; stdcall; External FT_DLL_Name name 'FT_EE_Program';
    function FT_EE_ProgramEx(ftHandle:FT_HANDLE;
                             pData: PFT_PROGRAM_DATA;
                             Manufacturer,
                             ManufacturerId,
                             Description,
                             SerialNumber: PChar):FT_Result; stdcall; External FT_DLL_Name name 'FT_EE_ProgramEx';

    function FT_EE_Read(ftHandle: FT_HANDLE;
                        pData:PFT_PROGRAM_DATA):FT_Result; stdcall; External FT_DLL_Name name 'FT_EE_Read';
    function FT_EE_ReadEx(ftHandle: FT_HANDLE;
                          pData: PFT_PROGRAM_DATA;
                          Manufacturer,
                          ManufacturerId,
                          Description,
                          SerialNumber: PChar):FT_Result; stdcall; External FT_DLL_Name name 'FT_EE_ReadEx';
    function FT_EE_UASize(ftHandle: FT_HANDLE;
                          UASize:LPDWORD):FT_Result; stdcall; External FT_DLL_Name name 'FT_EE_UASize';
    function FT_EE_UAWrite(ftHandle: FT_HANDLE;
                           pucData:PByte;
                           DataLen:DWord):FT_Result; stdcall; External FT_DLL_Name name 'FT_EE_UAWrite';
    function FT_EE_UARead(ftHandle: FT_HANDLE;
                          pucData: PByte;
                          DataLen: DWord;
                          BytesRead: LPDWORD):FT_Result; stdcall; External FT_DLL_Name name 'FT_EE_UARead';
}
  type


    FT_EEPROM_HEADER=record
		deviceType: FT_DEVICE;		// FTxxxx device type to be programmed
		// Device descriptor options
		VendorId,				// 0x0403
		ProductId: WORD;				// 0x6001
		SerNumEnable: byte;			// non-zero if serial number to be used
		// Config descriptor options
		MaxPower: WORD;				// 0 < MaxPower <= 500
		SelfPowered,			// 0 = bus powered, 1 = self powered
		RemoteWakeup,			// 0 = not capable, 1 = capable
		// Hardware options
		PullDownEnable: byte;		// non-zero if pull down in suspend enabled
	end;
    PFT_EEPROM_HEADER=^FT_EEPROM_HEADER;


	// FT232B EEPROM structure for use with FT_EEPROM_Read and FT_EEPROM_Program
    FT_EEPROM_232B=record
        // Common header
        common: FT_EEPROM_HEADER;	// common elements for all device EEPROMs
    end;
    PFT_EEPROM_232B=^FT_EEPROM_232B;


    // FT2232 EEPROM structure for use with FT_EEPROM_Read and FT_EEPROM_Program
    FT_EEPROM_2232=record
		// Common header
		common: FT_EEPROM_HEADER;	// common elements for all device EEPROMs
		// Drive options
		AIsHighCurrent,		// non-zero if interface is high current
		BIsHighCurrent,		// non-zero if interface is high current
		// Hardware options
		AIsFifo,				// non-zero if interface is 245 FIFO
		AIsFifoTar,			// non-zero if interface is 245 FIFO CPU target
		AIsFastSer,			// non-zero if interface is Fast serial
		BIsFifo,				// non-zero if interface is 245 FIFO
		BIsFifoTar,			// non-zero if interface is 245 FIFO CPU target
		BIsFastSer,			// non-zero if interface is Fast serial
		// Driver option
		ADriverType,			// non-zero if interface is to use VCP drivers
		BDriverType: byte			// non-zero if interface is to use VCP drivers
    end;
    PFT_EEPROM_2232=^FT_EEPROM_2232;


    // FT232R EEPROM structure for use with FT_EEPROM_Read and FT_EEPROM_Program
    FT_EEPROM_232R=record
		// Common header
		common: FT_EEPROM_HEADER;	// common elements for all device EEPROMs
		// Drive options
		IsHighCurrent,		// non-zero if interface is high current
		// Hardware options
		UseExtOsc,			// Use External Oscillator
		InvertTXD,			// non-zero if invert TXD
		InvertRXD,			// non-zero if invert RXD
		InvertRTS,			// non-zero if invert RTS
		InvertCTS,			// non-zero if invert CTS
		InvertDTR,			// non-zero if invert DTR
		InvertDSR,			// non-zero if invert DSR
		InvertDCD,			// non-zero if invert DCD
		InvertRI,				// non-zero if invert RI
		Cbus0,				// Cbus Mux control
		Cbus1,				// Cbus Mux control
		Cbus2,				// Cbus Mux control
		Cbus3,				// Cbus Mux control
		Cbus4,				// Cbus Mux control
		// Driver option
		DriverType: byte;			// non-zero if using D2XX driver
    end;
    PFT_EEPROM_232R=^FT_EEPROM_232R;

{
	// FT2232H EEPROM structure for use with FT_EEPROM_Read and FT_EEPROM_Program
	typedef struct ft_eeprom_2232h {
		// Common header
		FT_EEPROM_HEADER common;	// common elements for all device EEPROMs
		// Drive options
		UCHAR ALSlowSlew;			// non-zero if AL pins have slow slew
		UCHAR ALSchmittInput;		// non-zero if AL pins are Schmitt input
		UCHAR ALDriveCurrent;		// valid values are 4mA, 8mA, 12mA, 16mA
		UCHAR AHSlowSlew;			// non-zero if AH pins have slow slew
		UCHAR AHSchmittInput;		// non-zero if AH pins are Schmitt input
		UCHAR AHDriveCurrent;		// valid values are 4mA, 8mA, 12mA, 16mA
		UCHAR BLSlowSlew;			// non-zero if BL pins have slow slew
		UCHAR BLSchmittInput;		// non-zero if BL pins are Schmitt input
		UCHAR BLDriveCurrent;		// valid values are 4mA, 8mA, 12mA, 16mA
		UCHAR BHSlowSlew;			// non-zero if BH pins have slow slew
		UCHAR BHSchmittInput;		// non-zero if BH pins are Schmitt input
		UCHAR BHDriveCurrent;		// valid values are 4mA, 8mA, 12mA, 16mA
		// Hardware options
		UCHAR AIsFifo;				// non-zero if interface is 245 FIFO
		UCHAR AIsFifoTar;			// non-zero if interface is 245 FIFO CPU target
		UCHAR AIsFastSer;			// non-zero if interface is Fast serial
		UCHAR BIsFifo;				// non-zero if interface is 245 FIFO
		UCHAR BIsFifoTar;			// non-zero if interface is 245 FIFO CPU target
		UCHAR BIsFastSer;			// non-zero if interface is Fast serial
		UCHAR PowerSaveEnable;		// non-zero if using BCBUS7 to save power for self-powered designs
		// Driver option
		UCHAR ADriverType;			// non-zero if interface is to use VCP drivers
		UCHAR BDriverType;			// non-zero if interface is to use VCP drivers
	} FT_EEPROM_2232H, *PFT_EEPROM_2232H;


	// FT4232H EEPROM structure for use with FT_EEPROM_Read and FT_EEPROM_Program
	typedef struct ft_eeprom_4232h {
		// Common header
		FT_EEPROM_HEADER common;	// common elements for all device EEPROMs
		// Drive options
		UCHAR ASlowSlew;			// non-zero if A pins have slow slew
		UCHAR ASchmittInput;		// non-zero if A pins are Schmitt input
		UCHAR ADriveCurrent;		// valid values are 4mA, 8mA, 12mA, 16mA
		UCHAR BSlowSlew;			// non-zero if B pins have slow slew
		UCHAR BSchmittInput;		// non-zero if B pins are Schmitt input
		UCHAR BDriveCurrent;		// valid values are 4mA, 8mA, 12mA, 16mA
		UCHAR CSlowSlew;			// non-zero if C pins have slow slew
		UCHAR CSchmittInput;		// non-zero if C pins are Schmitt input
		UCHAR CDriveCurrent;		// valid values are 4mA, 8mA, 12mA, 16mA
		UCHAR DSlowSlew;			// non-zero if D pins have slow slew
		UCHAR DSchmittInput;		// non-zero if D pins are Schmitt input
		UCHAR DDriveCurrent;		// valid values are 4mA, 8mA, 12mA, 16mA
		// Hardware options
		UCHAR ARIIsTXDEN;			// non-zero if port A uses RI as RS485 TXDEN
		UCHAR BRIIsTXDEN;			// non-zero if port B uses RI as RS485 TXDEN
		UCHAR CRIIsTXDEN;			// non-zero if port C uses RI as RS485 TXDEN
		UCHAR DRIIsTXDEN;			// non-zero if port D uses RI as RS485 TXDEN
		// Driver option
		UCHAR ADriverType;			// non-zero if interface is to use VCP drivers
		UCHAR BDriverType;			// non-zero if interface is to use VCP drivers
		UCHAR CDriverType;			// non-zero if interface is to use VCP drivers
		UCHAR DDriverType;			// non-zero if interface is to use VCP drivers
	} FT_EEPROM_4232H, *PFT_EEPROM_4232H;


	// FT232H EEPROM structure for use with FT_EEPROM_Read and FT_EEPROM_Program
	typedef struct ft_eeprom_232h {
		// Common header
		FT_EEPROM_HEADER common;	// common elements for all device EEPROMs
		// Drive options
		UCHAR ACSlowSlew;			// non-zero if AC bus pins have slow slew
		UCHAR ACSchmittInput;		// non-zero if AC bus pins are Schmitt input
		UCHAR ACDriveCurrent;		// valid values are 4mA, 8mA, 12mA, 16mA
		UCHAR ADSlowSlew;			// non-zero if AD bus pins have slow slew
		UCHAR ADSchmittInput;		// non-zero if AD bus pins are Schmitt input
		UCHAR ADDriveCurrent;		// valid values are 4mA, 8mA, 12mA, 16mA
		// CBUS options
		UCHAR Cbus0;				// Cbus Mux control
		UCHAR Cbus1;				// Cbus Mux control
		UCHAR Cbus2;				// Cbus Mux control
		UCHAR Cbus3;				// Cbus Mux control
		UCHAR Cbus4;				// Cbus Mux control
		UCHAR Cbus5;				// Cbus Mux control
		UCHAR Cbus6;				// Cbus Mux control
		UCHAR Cbus7;				// Cbus Mux control
		UCHAR Cbus8;				// Cbus Mux control
		UCHAR Cbus9;				// Cbus Mux control
		// FT1248 options
		UCHAR FT1248Cpol;			// FT1248 clock polarity - clock idle high (1) or clock idle low (0)
		UCHAR FT1248Lsb;			// FT1248 data is LSB (1) or MSB (0)
		UCHAR FT1248FlowControl;	// FT1248 flow control enable
		// Hardware options
		UCHAR IsFifo;				// non-zero if interface is 245 FIFO
		UCHAR IsFifoTar;			// non-zero if interface is 245 FIFO CPU target
		UCHAR IsFastSer;			// non-zero if interface is Fast serial
		UCHAR IsFT1248;			// non-zero if interface is FT1248
		UCHAR PowerSaveEnable;		// 
		// Driver option
		UCHAR DriverType;			// non-zero if interface is to use VCP drivers
	} FT_EEPROM_232H, *PFT_EEPROM_232H;


	// FT X Series EEPROM structure for use with FT_EEPROM_Read and FT_EEPROM_Program
	typedef struct ft_eeprom_x_series {
		// Common header
		FT_EEPROM_HEADER common;	// common elements for all device EEPROMs
		// Drive options
		UCHAR ACSlowSlew;			// non-zero if AC bus pins have slow slew
		UCHAR ACSchmittInput;		// non-zero if AC bus pins are Schmitt input
		UCHAR ACDriveCurrent;		// valid values are 4mA, 8mA, 12mA, 16mA
		UCHAR ADSlowSlew;			// non-zero if AD bus pins have slow slew
		UCHAR ADSchmittInput;		// non-zero if AD bus pins are Schmitt input
		UCHAR ADDriveCurrent;		// valid values are 4mA, 8mA, 12mA, 16mA
		// CBUS options
		UCHAR Cbus0;				// Cbus Mux control
		UCHAR Cbus1;				// Cbus Mux control
		UCHAR Cbus2;				// Cbus Mux control
		UCHAR Cbus3;				// Cbus Mux control
		UCHAR Cbus4;				// Cbus Mux control
		UCHAR Cbus5;				// Cbus Mux control
		UCHAR Cbus6;				// Cbus Mux control
		// UART signal options
		UCHAR InvertTXD;			// non-zero if invert TXD
		UCHAR InvertRXD;			// non-zero if invert RXD
		UCHAR InvertRTS;			// non-zero if invert RTS
		UCHAR InvertCTS;			// non-zero if invert CTS
		UCHAR InvertDTR;			// non-zero if invert DTR
		UCHAR InvertDSR;			// non-zero if invert DSR
		UCHAR InvertDCD;			// non-zero if invert DCD
		UCHAR InvertRI;				// non-zero if invert RI
		// Battery Charge Detect options
		UCHAR BCDEnable;			// Enable Battery Charger Detection
		UCHAR BCDForceCbusPWREN;	// asserts the power enable signal on CBUS when charging port detected
		UCHAR BCDDisableSleep;		// forces the device never to go into sleep mode
		// I2C options
		WORD I2CSlaveAddress;		// I2C slave device address
		DWORD I2CDeviceId;			// I2C device ID
		UCHAR I2CDisableSchmitt;	// Disable I2C Schmitt trigger
		// FT1248 options
		UCHAR FT1248Cpol;			// FT1248 clock polarity - clock idle high (1) or clock idle low (0)
		UCHAR FT1248Lsb;			// FT1248 data is LSB (1) or MSB (0)
		UCHAR FT1248FlowControl;	// FT1248 flow control enable
		// Hardware options
		UCHAR RS485EchoSuppress;	// 
		UCHAR PowerSaveEnable;		// 
		// Driver option
		UCHAR DriverType;			// non-zero if interface is to use VCP drivers
	} FT_EEPROM_X_SERIES, *PFT_EEPROM_X_SERIES;


	// FT4222H EEPROM structure for use with FT_EEPROM_Read and FT_EEPROM_Program
	typedef struct ft_eeprom_4222h {
		// Common header
		FT_EEPROM_HEADER common;	// common elements for all device EEPROMs
		CHAR Revision;				// 'A', 'B', 'C', or 'D'.
		UCHAR I2C_Slave_Address;
		// Suspend
		UCHAR SPISuspend;			// 0 for "Disable SPI, tristate pins", 2 for "Keep SPI pin status", 3 for "Enable SPI pin control"
		UCHAR SuspendOutPol;		// 0 for negative, 1 for positive (not implemented on Rev A)
		UCHAR EnableSuspendOut;		// non-zero to enable (not implemented on Rev A)
		// QSPI
		UCHAR Clock_SlowSlew;		// non-zero if clock pin has slow slew
		UCHAR Clock_Drive;			// valid values are 4mA, 8mA, 12mA, 16mA
		UCHAR IO0_SlowSlew;			// non-zero if IO0 pin has slow slew
		UCHAR IO1_SlowSlew;			// non-zero if IO1 pin has slow slew
		UCHAR IO2_SlowSlew;			// non-zero if IO2 pin has slow slew
		UCHAR IO3_SlowSlew;			// non-zero if IO3 pin has slow slew
		UCHAR IO_Drive; 			// valid values are 4mA, 8mA, 12mA, 16mA
		UCHAR SlaveSelect_PullUp;	// non-zero to enable pull up
		UCHAR SlaveSelect_PullDown;	// non-zero to enable pull down
		UCHAR SlaveSelect_Drive;	// valid values are 4mA, 8mA, 12mA, 16mA
		UCHAR SlaveSelect_SlowSlew;	// non-zero if slave select pin has slow slew
		UCHAR MISO_Suspend;			// 2 for push-low, 3 for push high, 0 and 1 reserved
		UCHAR SIMO_Suspend;			// 2 for push-low, 3 for push high, 0 and 1 reserved
		UCHAR IO2_IO3_Suspend;		// 2 for push-low, 3 for push high, 0 and 1 reserved
		UCHAR SlaveSelect_Suspend;	// 0 for no-change (not implemented on Rev A), 2 for push-low, 3 for push high, 1 reserved
		// GPIO
		UCHAR GPIO0_Drive;			// valid values are 4mA, 8mA, 12mA, 16mA
		UCHAR GPIO1_Drive;			// valid values are 4mA, 8mA, 12mA, 16mA
		UCHAR GPIO2_Drive;			// valid values are 4mA, 8mA, 12mA, 16mA
		UCHAR GPIO3_Drive;			// valid values are 4mA, 8mA, 12mA, 16mA
		UCHAR GPIO0_SlowSlew;		// non-zero if IO0 pin has slow slew
		UCHAR GPIO1_SlowSlew;		// non-zero if IO0 pin has slow slew
		UCHAR GPIO2_SlowSlew;		// non-zero if IO0 pin has slow slew
		UCHAR GPIO3_SlowSlew;		// non-zero if IO0 pin has slow slew
		UCHAR GPIO0_PullDown;		// non-zero to enable pull down
		UCHAR GPIO1_PullDown;		// non-zero to enable pull down
		UCHAR GPIO2_PullDown;		// non-zero to enable pull down
		UCHAR GPIO3_PullDown;		// non-zero to enable pull down
		UCHAR GPIO0_PullUp;			// non-zero to enable pull up
		UCHAR GPIO1_PullUp;			// non-zero to enable pull up
		UCHAR GPIO2_PullUp;			// non-zero to enable pull up
		UCHAR GPIO3_PullUp;			// non-zero to enable pull up
		UCHAR GPIO0_OpenDrain;		// non-zero to enable open drain
		UCHAR GPIO1_OpenDrain;		// non-zero to enable open drain
		UCHAR GPIO2_OpenDrain;		// non-zero to enable open drain
		UCHAR GPIO3_OpenDrain;		// non-zero to enable open drain
		UCHAR GPIO0_Suspend;		// 0 for no-change, 1 for input (not implemented on Rev A), 2 for push-low, 3 for push high
		UCHAR GPIO1_Suspend;		// 0 for no-change, 1 for input (not implemented on Rev A), 2 for push-low, 3 for push high
		UCHAR GPIO2_Suspend;		// 0 for no-change, 1 for input (not implemented on Rev A), 2 for push-low, 3 for push high
		UCHAR GPIO3_Suspend;		// 0 for no-change, 1 for input (not implemented on Rev A), 2 for push-low, 3 for push high
		UCHAR FallingEdge;			// non-zero to change GPIO on falling edge
		// BCD
		UCHAR BCD_Disable;			// non-zero to disable BCD
		UCHAR BCD_OutputActiveLow;	// non-zero to set BCD output active low
		UCHAR BCD_Drive;			// valid values are 4mA, 8mA, 12mA, 16mA
	} FT_EEPROM_4222H, *PFT_EEPROM_4222H;


	// Power Delivery structures for use with FT_EEPROM_Read and FT_EEPROM_Program
	// PDO Configuration structure, mA supported values 0 to 10230mA, mV supported values 0 to 51100mV
	// This is part of the FT_EEPROM_PD structure.
	typedef struct ft_eeprom_PD_PDO_mv_ma {
		USHORT PDO1ma;	// PDO1 mA
		USHORT PDO1mv;	// PDO1 mV
		USHORT PDO2ma;	// PDO2 mA
		USHORT PDO2mv;	// PDO2 mV
		USHORT PDO3ma;	// PDO3 mA
		USHORT PDO3mv;	// PDO3 mV
		USHORT PDO4ma;	// PDO4 mA
		USHORT PDO4mv;	// PDO4 mV
		USHORT PDO5ma;	// PDO5 mA (FTx233HP only)
		USHORT PDO5mv;	// PDO5 mV (FTx233HP only)
		USHORT PDO6ma;	// PDO6 mA (FTx233HP only)
		USHORT PDO6mv;	// PDO6 mV (FTx233HP only)
		USHORT PDO7ma;	// PDO7 mA (FTx233HP only)
		USHORT PDO7mv;	// PDO7 mV (FTx233HP only)
	} FT_EEPROM_PD_PDO_mv_ma;

	// PD EEPROM structure for use with FT_EEPROM_Read and FT_EEPROM_Program
	// This is appended to the end of the base device structure. e_g. 
	//		struct {
	//			FT_EEPROM_xxx base;
	//			FT_EEPROM_PD pd;
	//		};
	// Device GPIO values are:
	//	FTx233HP - 0 to 7, 15 for N/A
	//	FTx232HP - 0 to 3, 15 for N/A
	typedef struct ft_eeprom_pd {
		// Configuration
		UCHAR srprs;		// non-zero to enable Sink Request Power Role Swap
		UCHAR sraprs;		// non-zero to enable Sink Accept PR Swap
		UCHAR srrprs;		// non-zero to enable Source Request PR SWAP
		UCHAR saprs;		// non-zero to enable Source Accept PR SWAP
		UCHAR vconns;		// non-zero to enable vConn Swap
		UCHAR passthru;		// non-zero to enable Pass Through (FTx233HP only)
		UCHAR extmcu;		// non-zero to enable External MCU
		UCHAR pd2en;		// non-zero to enable PD2 (FTx233HP only)
		UCHAR pd1autoclk;	// non-zero to enable PD1 Auto Clock
		UCHAR pd2autoclk;	// non-zero to enable PD2 Auto Clock (FTx233HP only)
		UCHAR useefuse;		// non-zero to Use EFUSE
		UCHAR extvconn;		// non-zero to enable External vConn

		// GPIO Configuration
		UCHAR count;		// GPIO Count, supported values are 0 to 7 
		UCHAR gpio1;		// GPIO Number 1, supports device GPIO values
		UCHAR gpio2;		// GPIO Number 2, supports device GPIO values
		UCHAR gpio3;		// GPIO Number 3, supports device GPIO values
		UCHAR gpio4;		// GPIO Number 4, supports device GPIO values
		UCHAR gpio5;		// GPIO Number 5, supports device GPIO values (FTx233HP only)
		UCHAR gpio6;		// GPIO Number 6, supports device GPIO values (FTx233HP only)
		UCHAR gpio7;		// GPIO Number 7, supports device GPIO values (FTx233HP only)
		UCHAR pd1lden;		// PD1 Load Enable, supports device GPIO values
		UCHAR pd2lden;		// PD2 Load Enable, supports device GPIO values (FTx233HP only)
		UCHAR dispin;		// Discharge Pin, supports device GPIO values
		UCHAR disenbm;		// Discharge Enable BM, 0 for "Drive Hi", 1 for "Drive Low", 2 for "Input Mode", 3 for "Don't Care"
		UCHAR disdisbm;		// Discharge Disable BM, 0 for "Drive Hi", 1 for "Drive Low", 2 for "Input Mode", 3 for "Don't Care"
		UCHAR ccselect;		// CC Select Indicator, supports device GPIO values

		// ISET Configuration
		UCHAR iset1;		// ISET1, supports device GPIO values
		UCHAR iset2;		// ISET2, supports device GPIO values
		UCHAR iset3;		// ISET3, supports device GPIO values
		UCHAR extiset;		// non-zero to enable EXTEND_ISET
		UCHAR isetpd2;		// non-zero to enable ISET_PD2
		UCHAR iseten;		// non-zero to set ISET_ENABLED

		// BM Configuration, 0 for "Drive Hi", 1 for "Drive Low", 2 for "Input Mode", 3 for "Don't Care"
		UCHAR PDO1_GPIO[7];		// PDO1 GPIO1 to GPIO7
		UCHAR PDO2_GPIO[7];		// PDO2 GPIO1 to GPIO7
		UCHAR PDO3_GPIO[7];		// PDO3 GPIO1 to GPIO7
		UCHAR PDO4_GPIO[7];		// PDO4 GPIO1 to GPIO7
		UCHAR PDO5_GPIO[7];		// PDO5 GPIO1 to GPIO7 (FTx233HP only)
		UCHAR PDO6_GPIO[7];		// PDO6 GPIO1 to GPIO7 (FTx233HP only)
		UCHAR PDO7_GPIO[7];		// PDO7 GPIO1 to GPIO7 (FTx233HP only)
		UCHAR VSET0V_GPIO[7];	// PDO7 GPIO1 to GPIO7
		UCHAR VSAFE5V_GPIO[7];	// PDO7 GPIO1 to GPIO7

		FT_EEPROM_PD_PDO_mv_ma BM_PDO_Sink;
		FT_EEPROM_PD_PDO_mv_ma BM_PDO_Source;
		FT_EEPROM_PD_PDO_mv_ma BM_PDO_Sink_2; // (FTx233HP only)

		// PD Timers
		UCHAR srt;			// Sender Response Timer
		UCHAR hrt;			// Hard Reset Timer
		UCHAR sct;			// Source Capability Timer
		UCHAR dit;			// Discover Identity Timer
		USHORT srcrt;		// Source Recover Timer
		USHORT trt;			// Transition Timer
		USHORT sofft;		// Source off timer
		USHORT nrt;			// No Response Timer
		USHORT swct;		// Sink Wait Capability Timer
		USHORT snkrt;		// Sink Request Timer
		UCHAR dt;			// Discharge Timer
		UCHAR cnst;			// Chunk not supported timer
		USHORT it;			// Idle Timer

		// PD Control
		UCHAR i2caddr;		// I2C Address (hex)
		UINT prou;			// Power Reserved for OWN use
		UINT trim1;			// TRIM1
		UINT trim2;			// TRIM2
		UCHAR extdc;		// non-zero to enable ETERNAL_DC_POWER
	} FT_EEPROM_PD, *PFT_EEPROM_PD;

	// FT2233HP EEPROM structure for use with FT_EEPROM_Read and FT_EEPROM_Program
	// FT2232H with power delivery
	typedef struct _ft_eeprom_2233hp
	{
		FT_EEPROM_2232H	ft2232h;
		FT_EEPROM_PD	pd;
	} FT_EEPROM_2233HP, *PFT_EEPROM_2233HP;

	// FT4233HP EEPROM structure for use with FT_EEPROM_Read and FT_EEPROM_Program
	// FT4232H with power delivery
	typedef struct _ft_eeprom_4233hp
	{
		FT_EEPROM_4232H	ft4232h;
		FT_EEPROM_PD	pd;
	} FT_EEPROM_4233HP, *PFT_EEPROM_4233HP;

	// FT2232HP EEPROM structure for use with FT_EEPROM_Read and FT_EEPROM_Program
	// FT2232H with power delivery
	typedef struct _ft_eeprom_2232hp
	{
		FT_EEPROM_2232H	ft2232h;
		FT_EEPROM_PD	pd;
	} FT_EEPROM_2232HP, *PFT_EEPROM_2232HP;

	// FT4232HP EEPROM structure for use with FT_EEPROM_Read and FT_EEPROM_Program
	// FT4232H with power delivery
	typedef struct _ft_eeprom_4232hp
	{
		FT_EEPROM_4232H	ft4232h;
		FT_EEPROM_PD	pd;
	} FT_EEPROM_4232HP, *PFT_EEPROM_4232HP;

	// FT233HP EEPROM structure for use with FT_EEPROM_Read and FT_EEPROM_Program
	// FT233H with power delivery
	typedef struct _ft_eeprom_233hp
	{
		FT_EEPROM_232H	ft232h;
		FT_EEPROM_PD	pd;
	} FT_EEPROM_233HP, *PFT_EEPROM_233HP;

	// FT232HP EEPROM structure for use with FT_EEPROM_Read and FT_EEPROM_Program
	// FT232H with power delivery
	typedef struct _ft_eeprom_232hp
	{
		FT_EEPROM_232H	ft232h;
		FT_EEPROM_PD	pd;
	} FT_EEPROM_232HP, *PFT_EEPROM_232HP;
}
{
    function FT_EEPROM_Read(ftHandle: FT_HANDLE;
                            eepromData: PByte;
                            eepromDataSize: DWord;
                            Manufacturer,
                            ManufacturerId,
                            Description,
                            SerialNumber: PChar):FT_Result; stdcall; External FT_DLL_Name name 'FT_EEPROM_Read';

    function FT_EEPROM_Program(ftHandle: FT_HANDLE;
                            eepromData: PByte;
                            eepromDataSize: DWord;
                            Manufacturer,
                            ManufacturerId,
                            Description,
                            SerialNumber: PChar):FT_Result; stdcall; External FT_DLL_Name name 'FT_EEPROM_Program';

    function FT_SetLatencyTimer(ftHandle: FT_HANDLE;
                                Latency: Byte):FT_Result; stdcall; External FT_DLL_Name name 'FT_SetLatencyTimer';

    function FT_GetLatencyTimer(ftHandle: FT_HANDLE;
                                Latency: PByte):FT_Result; stdcall; External FT_DLL_Name name 'FT_GetLatencyTimer';

    function FT_SetBitMode(ftHandle:FT_HANDLE;
                           Mask,
                           Enable: Byte):FT_Result; stdcall; External FT_DLL_Name name 'FT_SetBitMode';

    function FT_GetBitMode(ftHandle: FT_HANDLE;
                           BitMode: PByte):FT_Result; stdcall; External FT_DLL_Name name 'FT_GetBitMode';

    function FT_SetUSBParameters(ftHandle: FT_HANDLE;
                                 ulInTransferSize,
                                 ulOutTransferSize: ULONG):FT_Result; stdcall; External FT_DLL_Name name 'FT_SetUSBParameters';

    function FT_SetDeadmanTimeout(ftHandle: FT_HANDLE;
                                  ulDeadmanTimeout: ULONG): FT_Result; stdcall; External FT_DLL_Name name 'FT_SetDeadmanTimeout';


{$ifndef Windows}
	// Extra functions for non-Windows platforms to compensate
	// for lack of .INF file to specify Vendor and Product IDs.
    function FT_SetVIDPID(dwVID, dwPID: DWORD): FT_Result; stdcall; External FT_DLL_Name name 'FT_SetVIDPID';
    function FT_GetVIDPID(pdwVID, pdwPID: PDWORD): FT_Result; stdcall; External FT_DLL_Name name 'FT_GetVIDPID';
    function FT_GetDeviceLocId(ftHandle: FT_HANDLE;
                               lpdwLocId: PDWORD): FT_Result; stdcall; External FT_DLL_Name name 'FT_GetDeviceLocId';
{$endif}

    function FT_GetDeviceInfo(ftHandle: FT_HANDLE;
                              lpftDevice: PFT_DEVICE;
                              lpdwID: LPDWORD;
                              SerialNumber,
                              Description: PCHAR;
                              Dummy: Pointer): FT_Result; stdcall; External FT_DLL_Name name 'FT_GetDeviceInfo';
    function FT_StopInTask(ftHandle: FT_HANDLE) : FT_Result; stdcall; External FT_DLL_Name name 'FT_StopInTask';
    function FT_RestartInTask(ftHandle: FT_HANDLE) : FT_Result; stdcall; External FT_DLL_Name name 'FT_RestartInTask';
    function FT_SetResetPipeRetryCount(ftHandle: FT_HANDLE;
                                       RetryCount: Dword):FT_Result; stdcall; External FT_DLL_Name name 'FT_SetResetPipeRetryCount';
    function FT_ResetPort(ftHandle: FT_HANDLE) : FT_Result; stdcall; External FT_DLL_Name name 'FT_ResetPort';
    function FT_CyclePort(ftHandle: FT_HANDLE) : FT_Result; stdcall; External 'FTD2XX.DLL' name 'FT_CyclePort';




	//
	// Win32-type functions
	//
    function FT_W32_CreateFile(lpszName: PChar;
                               dwAccess: DWORD ;
                               dwShareMode: DWORD;
           	               lpSecurityAttributes: LPSECURITY_ATTRIBUTES;
                               dwCreate: DWORD;
           	               dwAttrsAndFlags: DWORD;
                               hTemplate: HANDLE): FT_HANDLE ;stdcall ; External FT_DLL_Name name 'FT_W32_CreateFile';
    function FT_W32_CloseHandle(ftHandle: FT_HANDLE):boolean; stdcall ; External FT_DLL_Name name 'FT_W32_CloseHandle';
    function FT_W32_ReadFile(ftHandle: FT_HANDLE;
                             lpBuffer: Pointer;
                             nBufferSize: DWord;
                             lpBytesReturned: LPDWORD;
                             alpOverlapped: LPOVERLAPPED): boolean; stdcall; External FT_DLL_Name name 'FT_W32_ReadFile';
    function FT_W32_WriteFile(ftHandle: FT_HANDLE;
                              lpBuffer: Pointer;
                              nBufferSize: DWORD;
                              lpBytesWritten: PDWord;
                              alpOverlapped: LPOVERLAPPED ):boolean; stdcall ; External FT_DLL_Name name 'FT_W32_WriteFile' ;
    function FT_W32_GetLastError(ftHandle :FT_HANDLE):DWord ;stdcall ; External FT_DLL_Name name 'FT_W32_GetLastError' ;

    function FT_W32_GetOverlappedResult(ftHandle: FT_HANDLE;
                                        alpOverlapped: LPOVERLAPPED;
                                        lpdwBytesTransferred: LPDWORD;
                                        bWait:boolean): boolean ;stdcall ; External FT_DLL_Name name 'FT_W32_GetOverlappedResult' ;
    function FT_W32_CancelIo(ftHandle: FT_HANDLE): boolean;stdcall ; External FT_DLL_Name name 'FT_W32_CancelIo';
}

  type
	//
	// Win32 COMM API type functions
	//
    FTCOMSTAT = record
        fCtsHold  : DWord ;//= 1 ;
        fRlsdHold : DWord ;//= 1 ;
        fDsrHold  : DWord ;//= 1 ;
        fXoffHold : DWord ;//= 1 ;
        fXoffSent : DWord ;//= 1 ;
        fEof      : DWord ;//= 1 ;
        fTxim     : DWord ;//= 1 ;
        fReserved : DWord ;//= 25 ;
        cbInQue   : DWord ;
        cbOutQue  : DWord ;
    end;
    LPFTCOMSTAT = ^FTCOMSTAT ;

    FTDCB = record
             DCBlength    :   DWORD ;      // sizeof(FTDCB)
             BaudRate     :   DWORD ;      // Baudrate at which running
             fBinary      :   DWORD ;//= 1 ;  // Binary Mode (skip EOF check)
             fParity      :   DWORD ;//= 1 ;  // Enable parity checking
             fOutxCtsFlow :   DWORD ;//= 1 ;  // CTS handshaking on output
             fOutxDsrFlow :   DWORD ;//= 1 ;  // DSR handshaking on output
             fDtrControl  :   DWORD ;//= 2 ;  // DTR Flow control
             fDsrSensitivity   : DWORD ;//=  1 ; // DSR Sensitivity
             fTXContinueOnXoff : DWORD ;//=  1 ; // Continue TX when Xoff sent
             fOutX             : DWORD ;//=  1 ; // Enable output X-ON/X-OFF
             fInX              : DWORD ;//=  1 ; // Enable input X-ON/X-OFF
             fErrorChar        : DWORD ;//=  1 ; // Enable Err Replacement
             fNull             : DWORD ;//=  1 ; // Enable Null stripping
             fRtsControl       : DWORD ;//=  2 ; // Rts Flow control
             fAbortOnError     : DWORD ;//=  1 ; // Abort all reads and writes on Error
             fDummy2           : DWORD ;//= 17 ; // Reserved
             wReserved         : WORD       ; // Not currently used
             XonLim            : WORD       ; // Transmit X-ON threshold
             XoffLim           : WORD       ; // Transmit X-OFF threshold
             ByteSize          : BYTE       ; // Number of bits/byte, 4-8
             Parity            : BYTE       ; // 0-4=None,Odd,Even,Mark,Space
             StopBits          : BYTE       ; // 0,1,2 = 1, 1.5, 2
             XonChar           : char       ; // Tx and Rx X-ON character
             XoffChar          : char       ; // Tx and Rx X-OFF character
             ErrorChar         : char       ; // Error replacement char
             EofChar           : char       ; // End of Input character
             EvtChar           : char       ; // Received Event character
             wReserved1        : WORD       ; // Fill for now.
        end;
    LPFTDCB = ^FTDCB;

    FTTIMEOUTSend = record
                     ReadIntervalTimeout        : DWORD ;  // Maximum time between read chars.
                     ReadTotalTimeoutMultiplier : DWORD ;  // Multiplier of characters.
                     ReadTotalTimeoutConstant   : DWORD ;  // Constant in milliseconds.
                     WriteTotalTimeoutMultiplier: DWORD ;  // Multiplier of characters.
                     WriteTotalTimeoutConstant  : DWORD ;  // Constant in milliseconds.
                    end;
    LPFTTIMEOUTS = ^FTTIMEOUTSend;

{
    function FT_W32_ClearCommBreak(ftHandle: FT_HANDLE): boolean; stdcall ; External FT_DLL_Name name 'FT_W32_ClearCommBreak' ;
    function FT_W32_ClearCommError(ftHandle: FT_HANDLE;
                                   lpdwErrors: LPDWORD;
                                   lpftComstat: LPFTCOMSTAT): boolean;stdcall; External FT_DLL_Name name 'FT_W32_ClearCommError';
    function FT_W32_EscapeCommFunction(ftHandle: FT_HANDLE;
                                       dwFunc: DWORD): FT_Result; stdcall; External FT_DLL_Name name 'FT_W32_EscapeCommFunction' ;

    function FT_W32_GetCommModemStatus(ftHandle: FT_HANDLE;
                                       lpdwModemStatus: LPDWORD): boolean;stdcall; External FT_DLL_Name name 'FT_W32_GetCommModemStatus';

    function FT_W32_GetCommState(ftHandle: FT_HANDLE;
                                 lpftDcb: LPFTDCB): boolean;stdcall; External FT_DLL_Name name 'FT_W32_GetCommState';

    function FT_W32_GetCommTimeouts(ftHandle: FT_HANDLE;
                                    pTimeouts: LPFTTIMEOUTS): boolean;stdcall; External FT_DLL_Name name 'FT_W32_GetCommTimeouts';

    function FT_W32_PurgeComm(ftHandle: FT_HANDLE;
                              dwMask: DWORD): boolean;stdcall; External FT_DLL_Name name 'FT_W32_PurgeComm';

    function FT_W32_SetCommBreak(ftHandle: FT_HANDLE): boolean; stdcall; External FT_DLL_Name name 'FT_W32_SetCommBreak';

    function FT_W32_SetCommMask(ftHandle: FT_HANDLE;
                                ulEventMask: ULONG): boolean; stdcall; External FT_DLL_Name name 'FT_W32_SetCommMask';

    function FT_W32_GetCommMask(ftHandle: FT_HANDLE;
                                lpdwEventMask: LPDWORD): boolean; stdcall; External FT_DLL_Name name 'FT_W32_GetCommMask';

    function FT_W32_SetCommState(ftHandle: FT_HANDLE;
                                 lpftDcb: LPFTDCB): boolean; stdcall; External FT_DLL_Name name 'FT_W32_SetCommState';

    function FT_W32_SetCommTimeouts(ftHandle: FT_HANDLE;
                                    pTimeouts: LPFTTIMEOUTS): boolean; stdcall; External FT_DLL_Name name 'FT_W32_SetCommTimeouts';

    function FT_W32_SetupComm(ftHandle: FT_HANDLE;
                              dwReadBufferSize: DWORD;
                              dwWriteBufferSize: DWORD): boolean; stdcall; External FT_DLL_Name name 'FT_W32_SetupComm';

    function FT_W32_WaitCommEvent(ftHandle: FT_HANDLE;
                                  pulEvent: PULONG;
                                  alpOverlapped: LPOVERLAPPED): boolean;stdcall ; External FT_DLL_Name name 'FT_W32_WaitCommEvent';

}

  type
	//
	// Device information
	//
    FT_Device_Info_Node = record
      Flags         : DWord;
      DeviceType    : Dword;
      ID            : DWord;
      LocID         : DWord;
      SerialNumber  : array [0..15] of Char;
      Description   : array [0..63] of Char;
      ftHandle  : DWord;
    end;
    PFT_Device_Info_Node=^FT_Device_Info_Node;
  const
    // Device information flags
    FT_FLAGS_OPENED = 1;
    FT_FLAGS_HISPEED = 2;


{

    function FT_CreateDeviceInfoList(lpdwNumDevs: LPDWORD):FT_Result; stdcall; External FT_DLL_Name name 'FT_CreateDeviceInfoList';

    function FT_GetDeviceInfoList(pDevice_Info_node: PFT_Device_Info_Node;
                                  lpdwNumDevs: LPDWORD): FT_Result; stdcall; External FT_DLL_Name name 'FT_GetDeviceInfoList';

    function FT_GetDeviceInfoDetail(Index: DWord;
                                    Flags,
                                    DevType,
                                    ID,
                                    LocID: LPDWORD;
                                    SerialNumber,
                                    Description: Pointer;
                                    DevHandle: PFT_HANDLE): FT_Result; stdcall; External FT_DLL_Name name 'FT_GetDeviceInfoDetail';
    //
    // Version information
    //
    function FT_GetDriverVersion(ftHandle: FT_HANDLE;
                                 lpdwVersion: LPDWORD): FT_Result; stdcall; External FT_DLL_Name name 'FT_GetDriverVersion';

    function FT_GetLibraryVersion(lpdwVersion: LPDWORD): FT_Result; stdcall; External FT_DLL_Name name 'FT_GetLibraryVersion';

    function FT_Rescan(): FT_Result; stdcall; External FT_DLL_Name name 'FT_Rescan';
    function FT_Reload(wVid,
                       wPid: Word): FT_Result; stdcall; External FT_DLL_Name name 'FT_Reload';

    function FT_GetComPortNumber(ftHandle: FT_HANDLE;
                                 lpdwComPortNumber: LPLONG): FT_Result; stdcall; External FT_DLL_Name name 'FT_GetComPortNumber';
    //
    // FT232H additional EEPROM functions
    //
    function FT_EE_ReadConfig(ftHandle: FT_HANDLE;
                              ucAddress: Byte;
                              pucValue: PByte): FT_Result; stdcall; External FT_DLL_Name name 'FT_EE_ReadConfig';

    function FT_EE_WriteConfig(ftHandle: FT_HANDLE;
                              ucAddress,
                              ucValue: Byte): FT_Result; stdcall; External FT_DLL_Name name 'FT_EE_WriteConfig';

    function FT_EE_ReadECC(ftHandle: FT_HANDLE;
                           ucOption: Byte;
                           lpwValue: LPWORD): FT_Result; stdcall; External FT_DLL_Name name 'FT_EE_ReadECC';

    function FT_GetQueueStatusEx(ftHandle: FT_HANDLE;
                                dwRxBytes: PDWord): FT_Result; stdcall; External FT_DLL_Name name 'FT_GetQueueStatusEx';

    function FT_ComPortIdle(ftHandle: FT_HANDLE): FT_Result; stdcall; External FT_DLL_Name name 'FT_ComPortIdle';

    function FT_ComPortCancelIdle(ftHandle: FT_HANDLE): FT_Result; stdcall; External FT_DLL_Name name 'FT_ComPortCancelIdle';

    function FT_VendorCmdGet(ftHandle: FT_HANDLE;
                             Request: Byte;
                             Buf: PByte;
                             Len: UShort): FT_Result; stdcall; External FT_DLL_Name name 'FT_VendorCmdGet';

   function FT_VendorCmdSet(ftHandle: FT_HANDLE;
                            Request: Byte;
                            Buf: PByte;
                            Len: UShort): FT_Result; stdcall; External FT_DLL_Name name 'FT_VendorCmdSet';

    function FT_VendorCmdGetEx(ftHandle: FT_HANDLE;
                                 wValue: USHORT;
                                 Buf: PByte;
                                 Len: UShort): FT_Result; stdcall; External FT_DLL_Name name 'FT_VendorCmdGetEx';

    function FT_VendorCmdSetEx(ftHandle: FT_HANDLE;
                               wValue: USHORT;
                               Buf: PByte;
                               Len: UShort): FT_Result; stdcall; External FT_DLL_Name name 'FT_VendorCmdSetEx';
}

  function Load_FTDLibrary(const aFTDILibraryFilename: string=''): boolean;
  procedure Unload_FTDLibrary;

  var
  //Classic functions
  FT_Open:function(Index: Integer; ftHandle: PFT_HANDLE):FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_OpenEx:function(pvArg1:Pointer; dwFlags:Dword; ftHandle:Pointer):FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_OpenByLocation:function(pvArg1:Pointer; dwFlags:Dword; ftHandle:Pointer):FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_ListDevices:function(pvArg1:Dword; pvArg2:Pointer; dwFlags:Dword):FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_GetNumDevices:function(pvArg1:Pointer{Dword}; pvArg2:Pointer; dwFlags:Dword):FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_Close:function(ftHandle: FT_HANDLE):FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_Read:function(ftHandle: FT_HANDLE; FTInBuf: Pointer; dwBytesToRead: DWord; BytesReturned: LPDWORD):FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_Write:function(ftHandle: FT_HANDLE; FTOutBuf:Pointer; BufferSize:LongInt; BytesWritten: LPDWORD):FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_IoCtl:function(ftHandle: FT_HANDLE; dwIoControlCode: DWord; lpInBuf: Pointer; nInBufSize: DWord; lpOutBuf: Pointer; nOutBufSize: DWord; lpBytesReturned:LPDWORD; lpOverlapped: LPOVERLAPPED):FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_SetBaudRate:function(ftHandle: FT_HANDLE; BaudRate:DWord):FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_SetDivisor:function(ftHandle: FT_HANDLE; Divisor:DWord):FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_SetDataCharacteristics:function(ftHandle:FT_HANDLE; WordLength,StopBits,Parity:Byte):FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_SetFlowControl:function(ftHandle:FT_HANDLE; FlowControl:Word; XonChar,XoffChar:Byte):FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_ResetDevice:function(ftHandle:FT_HANDLE):FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_SetDtr:function(ftHandle:FT_HANDLE):FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_ClrDtr:function(ftHandle:FT_HANDLE):FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_SetRts:function(ftHandle:FT_HANDLE):FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_ClrRts:function(ftHandle:FT_HANDLE):FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_GetModemStatus:function(ftHandle:FT_HANDLE; ModemStatus:Pointer):FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_SetChars:function(ftHandle:FT_HANDLE; EventChar,EventCharEnabled,ErrorChar,ErrorCharEnabled:Byte):FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_Purge:function(ftHandle:FT_HANDLE; Mask:Dword):FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_SetTimeouts:function(ftHandle:FT_HANDLE; ReadTimeout,WriteTimeout:Dword):FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_GetQueueStatus:function(ftHandle:FT_HANDLE; RxBytes:LPDWORD):FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_SetEventNotification:function(ftHandle:FT_HANDLE; EventMask:DWord; pvArgs:Pointer):FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_GetStatus:function(ftHandle:FT_HANDLE; RxBytes,TxBytes,EventStatus:PWord):FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_SetBreakOn:function(ftHandle:FT_HANDLE) : FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_SetBreakOff:function(ftHandle:FT_HANDLE) : FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_SetWaitMask:function(ftHandle:FT_HANDLE; Mask: DWord): FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_WaitOnMask:function(ftHandle:FT_HANDLE; pMask: PDWord): FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_GetEventStatus:function(ftHandle:FT_HANDLE;dwEventDWord: PDWord): FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  // EEPROM function
  FT_ReadEE:function(ftHandle:FT_HANDLE; WordAddr:DWord; WordRead:PWord):FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_WriteEE:function(ftHandle:FT_HANDLE; WordAddr:DWord; WordData:word):FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_EraseEE:function(ftHandle:FT_HANDLE):FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_EE_Program:function(ftHandle:FT_HANDLE; pEEData:PFT_PROGRAM_DATA):FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_EE_ProgramEx:function(ftHandle:FT_HANDLE; pData: PFT_PROGRAM_DATA; Manufacturer, ManufacturerId, Description, SerialNumber: PChar):FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_EE_Read:function(ftHandle: FT_HANDLE; pData:PFT_PROGRAM_DATA):FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_EE_ReadEx:function(ftHandle: FT_HANDLE; pData: PFT_PROGRAM_DATA; Manufacturer, ManufacturerId, Description, SerialNumber: PChar):FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_EE_UASize:function(ftHandle: FT_HANDLE; UASize:LPDWORD):FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_EE_UAWrite:function(ftHandle: FT_HANDLE; pucData:PByte; DataLen:DWord):FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_EE_UARead:function(ftHandle: FT_HANDLE; pucData: PByte; DataLen: DWord; BytesRead: LPDWORD):FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_EEPROM_Read:function(ftHandle: FT_HANDLE; eepromData: PByte; eepromDataSize: DWord; Manufacturer, ManufacturerId, Description, SerialNumber: PChar):FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_EEPROM_Program:function(ftHandle: FT_HANDLE; eepromData: PByte; eepromDataSize: DWord; Manufacturer, ManufacturerId, Description, SerialNumber: PChar):FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  // FT2232C, FT232BM and FT245BM Extended API Function
  FT_SetLatencyTimer:function(ftHandle: FT_HANDLE; Latency: Byte):FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_GetLatencyTimer:function(ftHandle: FT_HANDLE; Latency: PByte):FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_SetBitMode:function(ftHandle:FT_HANDLE;  Mask, Enable: Byte):FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_GetBitMode:function(ftHandle: FT_HANDLE; BitMode: PByte):FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_SetUSBParameters:function(ftHandle: FT_HANDLE; ulInTransferSize, ulOutTransferSize: ULONG):FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_SetDeadmanTimeout:function(ftHandle: FT_HANDLE; ulDeadmanTimeout: ULONG): FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  {$ifndef Windows}
  // Extra functions for non-Windows platforms to compensate
  // for lack of .INF file to specify Vendor and Product IDs.
  FT_SetVIDPID:function(dwVID, dwPID: DWORD): FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_GetVIDPID:function(pdwVID, pdwPID: PDWORD): FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_GetDeviceLocId:function(ftHandle: FT_HANDLE; lpdwLocId: PDWORD): FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  {$endif}
  FT_GetDeviceInfo:function(ftHandle: FT_HANDLE; lpftDevice: PFT_DEVICE; lpdwID: LPDWORD; SerialNumber, Description: PCHAR; Dummy: Pointer): FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_StopInTask:function(ftHandle: FT_HANDLE) : FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_RestartInTask:function(ftHandle: FT_HANDLE) : FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_SetResetPipeRetryCount:function(ftHandle: FT_HANDLE; RetryCount: Dword):FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_ResetPort:function(ftHandle: FT_HANDLE) : FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_CyclePort:function(ftHandle: FT_HANDLE) : FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  // Win32-type functions
  FT_W32_CreateFile:function(lpszName: PChar; dwAccess: DWORD; dwShareMode: DWORD; lpSecurityAttributes: LPSECURITY_ATTRIBUTES; dwCreate: DWORD; dwAttrsAndFlags: DWORD; hTemplate: HANDLE): FT_HANDLE ; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_W32_CloseHandle:function(ftHandle: FT_HANDLE):boolean; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_W32_ReadFile:function(ftHandle: FT_HANDLE; lpBuffer: Pointer; nBufferSize: DWord; lpBytesReturned: LPDWORD; lpOverlapped: LPOVERLAPPED): boolean; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_W32_WriteFile:function(ftHandle: FT_HANDLE; lpBuffer: Pointer; nBufferSize: DWORD; lpBytesWritten: PDWord; alpOverlapped: LPOVERLAPPED ):boolean; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_W32_GetLastError:function(ftHandle :FT_HANDLE):DWord ; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_W32_GetOverlappedResult:function(ftHandle: FT_HANDLE; alpOverlapped: LPOVERLAPPED; lpdwBytesTransferred: LPDWORD; bWait:boolean): boolean; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_W32_CancelIo:function(ftHandle: FT_HANDLE): boolean; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_W32_ClearCommBreak:function(ftHandle: FT_HANDLE): boolean; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_W32_ClearCommError:function(ftHandle: FT_HANDLE; lpdwErrors: LPDWORD; lpftComstat: LPFTCOMSTAT): boolean; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_W32_EscapeCommFunction:function(ftHandle: FT_HANDLE; dwFunc: DWORD): FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_W32_GetCommModemStatus:function(ftHandle: FT_HANDLE; lpdwModemStatus: LPDWORD): boolean; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_W32_GetCommState:function(ftHandle: FT_HANDLE; lpftDcb: LPFTDCB): boolean; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_W32_GetCommTimeouts:function(ftHandle: FT_HANDLE; pTimeouts: LPFTTIMEOUTS): boolean; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_W32_PurgeComm:function(ftHandle: FT_HANDLE; dwMask: DWORD): boolean; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_W32_SetCommBreak:function(ftHandle: FT_HANDLE): boolean; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_W32_SetCommMask:function(ftHandle: FT_HANDLE; ulEventMask: ULONG): boolean; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  //FT_W32_GetCommMask:function(ftHandle: FT_HANDLE; lpdwEventMask: LPDWORD): boolean; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_W32_SetCommState:function(ftHandle: FT_HANDLE; lpftDcb: LPFTDCB): boolean; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_W32_SetCommTimeouts:function(ftHandle: FT_HANDLE; pTimeouts: LPFTTIMEOUTS): boolean; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_W32_SetupComm:function(ftHandle: FT_HANDLE; dwReadBufferSize, dwWriteBufferSize: DWORD): boolean; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_W32_WaitCommEvent:function(ftHandle: FT_HANDLE; pulEvent: PULONG; alpOverlapped: LPOVERLAPPED): boolean; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};

  FT_CreateDeviceInfoList:function(lpdwNumDevs: LPDWORD):FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_GetDeviceInfoList:function(pDevice_Info_node: PFT_Device_Info_Node; lpdwNumDevs: LPDWORD): FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_GetDeviceInfoDetail:function(Index: DWord; Flags, DevType, ID, LocID: LPDWORD; SerialNumber, Description: Pointer; DevHandle: PFT_HANDLE): FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  // Version information
  FT_GetDriverVersion:function(ftHandle: FT_HANDLE; lpdwVersion: LPDWORD): FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_GetLibraryVersion:function(lpdwVersion: LPDWORD): FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_Rescan: function(): FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_Reload:function(wVid, wPid: Word): FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_GetComPortNumber:function(ftHandle: FT_HANDLE; lpdwComPortNumber: LPLONG): FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  // FT232H additional EEPROM functions
  FT_EE_ReadConfig:function(ftHandle: FT_HANDLE; ucAddress: Byte; pucValue: PByte): FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_EE_WriteConfig:function(ftHandle: FT_HANDLE; ucAddress, ucValue: Byte): FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_EE_ReadECC:function(ftHandle: FT_HANDLE; ucOption: Byte; lpwValue: LPWORD): FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_GetQueueStatusEx:function(ftHandle: FT_HANDLE; dwRxBytes: PDWord): FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_ComPortIdle:function(ftHandle: FT_HANDLE): FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_ComPortCancelIdle:function(ftHandle: FT_HANDLE): FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_VendorCmdGet:function(ftHandle: FT_HANDLE; Request: Byte; Buf: PByte; Len: UShort): FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_VendorCmdSet:function(ftHandle: FT_HANDLE; Request: Byte; Buf: PByte; Len: UShort): FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_VendorCmdGetEx:function(ftHandle: FT_HANDLE; wValue: USHORT; Buf: PByte; Len: UShort): FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};
  FT_VendorCmdSetEx:function(ftHandle: FT_HANDLE; wValue: USHORT; Buf: PByte; Len: UShort): FT_Result; {$IFDEF windows}stdcall{$ELSE}cdecl{$ENDIF};


  // Exported Functions
  // Classic Functions
  Function GetFTDeviceCount : FT_Result;
  Function GetFTDeviceSerialNo(DeviceIndex:DWord) : FT_Result;
  Function GetFTDeviceDescription(DeviceIndex:DWord) : FT_Result;
  Function GetFTDeviceLocation(DeviceIndex:DWord) : FT_Result;
  {
  Function Open_USB_Device : FT_Result;
  Function Open_USB_Device_By_Serial_Number(Serial_Number:string) : FT_Result;
  Function Open_USB_Device_By_Device_Description(Device_Description:string) : FT_Result;
  Function Open_USB_Device_By_Device_Location(Location:DWord) : FT_Result;
  Function Close_USB_Device : FT_Result;
  Function Read_USB_Device_Buffer(Read_Count:Integer) : Integer;
  Function Write_USB_Device_Buffer(Write_Count:Integer) : Integer;
  Function Reset_USB_Device : FT_Result;
  Function Set_USB_Device_BaudRate : FT_Result;
  Function Set_USB_Device_BaudRate_Divisor(Divisor:Dword) : FT_Result;
  Function Set_USB_Device_DataCharacteristics : FT_Result;
  Function Set_USB_Device_FlowControl : FT_Result;
  Function Set_USB_Device_RTS : FT_Result;
  Function Clr_USB_Device_RTS : FT_Result;
  Function Set_USB_Device_DTR : FT_Result;
  Function Clr_USB_Device_DTR : FT_Result;
  Function Get_USB_Device_ModemStatus : FT_Result;
  Function Set_USB_Device_Chars : FT_Result;
  Function Purge_USB_Device_Out : FT_Result;
  Function Purge_USB_Device_In : FT_Result;
  Function Set_USB_Device_TimeOuts(ReadTimeOut,WriteTimeOut:DWord) : FT_Result;
  Function Get_USB_Device_QueueStatus : FT_Result;
  Function Set_USB_Device_Break_On : FT_Result;
  Function Set_USB_Device_Break_Off : FT_Result;
  Function Get_USB_Device_Status : FT_Result;
  Function Set_USB_Device_Event_Notification(EventMask:DWord) : FT_Result;
  Function USB_FT_GetDeviceInfo(DevType,ID:DWord; SerialNumber,Description:array of char) : FT_Result;
  Function Set_USB_Device_Reset_Pipe_Retry_Count(RetryCount:DWord) : FT_Result;
  Function Stop_USB_Device_InTask : FT_Result;
  Function Restart_USB_Device_InTask : FT_Result;
  Function Reset_USB_Port : FT_Result;
  Function Cycle_USB_Port : FT_Result;
  Function Create_USB_Device_List : FT_Result;
  Function Get_USB_Device_List : FT_Result;
  Function Get_USB_Device_List_Detail(Index:DWord) : FT_Result;
  // EEPROM Functions
  function USB_FT_EE_Read : FT_Result;
  function USB_FT_C_EE_Read : FT_Result;
  function USB_FT_R_EE_Read : FT_Result;
  function USB_FT_EE_Program : FT_Result;
  function USB_FT_ReadEE(WordAddr:Dword) : FT_Result;
  function USB_FT_WriteEE(WordAddr:Dword;WordData:Word) : FT_Result;
  function USB_FT_EraseEE : FT_Result;
  function USB_FT_EE_UARead : FT_Result;
  function USB_FT_EE_UAWrite : FT_Result;
  function USB_FT_EE_UASize : FT_Result;
  // FT2232C, FT232BM and FT245BM Extended API Functions
  Function Get_USB_Device_LatencyTimer : FT_Result;
  Function Set_USB_Device_LatencyTimer(Latency : Byte) : FT_Result;
  Function Get_USB_Device_BitMode(var BitMode:Byte) : FT_Result;
  Function Set_USB_Device_BitMode(Mask, Enable:Byte) : FT_Result;
  Function Set_USB_Parameters(InSize, OutSize:Dword) : FT_Result;

  type TDWordptr = ^DWord;
  Function Get_USB_Driver_Version(DrVersion :  TDWordptr): FT_Result;
  Function Get_USB_Library_Version(LbVersion :  TDWordptr): FT_Result;
}

var
  FT_Enable_Error_Report: boolean=TRUE;

  // Used By FT_ListDevices
  FT_Device_Count : DWord;
  FT_Device_String : String;


implementation

var
  FTDLibraryHandle:TLibHandle=0;

  function Load_FTDLibrary(const aFTDILibraryFilename: string): boolean;
  var flag: boolean;
    function GetProc(const aName: string): Pointer;
    begin
      Result:=DynLibs.GetProcedureAddress(FTDLibraryHandle, PChar(aName));
      flag := flag and (Result<>NIL);
        if Result=NIL
          then ShowMessage(aName+'  not found');
    end;
  begin
    if FTDLibraryHandle<>DynLibs.NilHandle then begin
      Result:=TRUE;
      exit;
    end;

    if Length(aFTDILibraryFilename)=0
      then FTDLibraryHandle:=DynLibs.LoadLibrary(UnicodeString(FT_LIB_Name))
      else FTDLibraryHandle:=DynLibs.LoadLibrary(UnicodeString(aFTDILibraryFilename));

    if FTDLibraryHandle<>DynLibs.NilHandle then begin
      flag:=TRUE;
      Pointer(FT_GetNumDevices) := GetProc('FT_ListDevices');
      Pointer(FT_ListDevices) := GetProc('FT_ListDevices');
      Pointer(FT_Open) := GetProc('FT_Open');
      Pointer(FT_OpenEx) := GetProc('FT_OpenEx');
      Pointer(FT_OpenByLocation) := GetProc('FT_OpenEx');
      Pointer(FT_Close) := GetProc('FT_Close');
      Pointer(FT_Read) := GetProc('FT_Read');
      Pointer(FT_Write) := GetProc('FT_Write');
      Pointer(FT_ResetDevice) := GetProc('FT_ResetDevice');
      Pointer(FT_IoCtl) := GetProc('FT_IoCtl');
      Pointer(FT_SetBaudRate) := GetProc('FT_SetBaudRate');
      Pointer(FT_SetDivisor) := GetProc('FT_SetDivisor');
      Pointer(FT_SetDataCharacteristics) := GetProc('FT_SetDataCharacteristics');
      Pointer(FT_SetFlowControl) := GetProc('FT_SetFlowControl');
      Pointer(FT_SetDtr) := GetProc('FT_SetDtr');
      Pointer(FT_ClrDtr) := GetProc('FT_ClrDtr');
      Pointer(FT_SetRts) := GetProc('FT_SetRts');
      Pointer(FT_ClrRts) := GetProc('FT_ClrRts');
      Pointer(FT_GetModemStatus) := GetProc('FT_GetModemStatus');
      Pointer(FT_SetChars) := GetProc('FT_SetChars');
      Pointer(FT_Purge) := GetProc('FT_Purge');
      Pointer(FT_SetTimeouts) := GetProc('FT_SetTimeouts');
      Pointer(FT_GetQueueStatus) := GetProc('FT_GetQueueStatus');
      Pointer(FT_SetBreakOn) := GetProc('FT_SetBreakOn');
      Pointer(FT_SetBreakOff) := GetProc('FT_SetBreakOff');
      Pointer(FT_GetStatus) := GetProc('FT_GetStatus');
      Pointer(FT_SetWaitMask) := GetProc('FT_SetWaitMask');
      Pointer(FT_SetEventNotification) := GetProc('FT_SetEventNotification');
      Pointer(FT_GetDeviceInfo) := GetProc('FT_GetDeviceInfo');
      Pointer(FT_SetResetPipeRetryCount) := GetProc('FT_SetResetPipeRetryCount');
      Pointer(FT_StopInTask) := GetProc('FT_StopInTask');
      Pointer(FT_RestartInTask) := GetProc('FT_RestartInTask');
      Pointer(FT_ResetPort) := GetProc('FT_ResetPort');
      Pointer(FT_CyclePort) := GetProc('FT_CyclePort');
      Pointer(FT_CreateDeviceInfoList) := GetProc('FT_CreateDeviceInfoList');
      Pointer(FT_GetDeviceInfoList) := GetProc('FT_GetDeviceInfoList');
      Pointer(FT_GetDeviceInfoDetail) := GetProc('FT_GetDeviceInfoDetail');
      Pointer(FT_GetDriverVersion) := GetProc('FT_GetDriverVersion');
      Pointer(FT_GetLibraryVersion) := GetProc('FT_GetLibraryVersion');
      Pointer(FT_WaitOnMask) := GetProc('FT_WaitOnMask');
      Pointer(FT_GetEventStatus) := GetProc('FT_GetEventStatus');
      // EEPROM functions
      Pointer(FT_EE_Read) := GetProc('FT_EE_Read');
      Pointer(FT_EE_ReadEx) := GetProc('FT_EE_ReadEx');
      Pointer(FT_EE_Program) := GetProc('FT_EE_Program');
      Pointer(FT_EE_ProgramEx) := GetProc('FT_EE_ProgramEx');
      // EEPROM primitives - you need an NDA for EEPROM checksum
      Pointer(FT_ReadEE) := GetProc('FT_ReadEE');
      Pointer(FT_WriteEE) := GetProc('FT_WriteEE');
      Pointer(FT_EraseEE) := GetProc('FT_EraseEE');
      Pointer(FT_EE_UARead) := GetProc('FT_EE_UARead');
      Pointer(FT_EE_UAWrite) := GetProc('FT_EE_UAWrite');
      Pointer(FT_EE_UASize) := GetProc('FT_EE_UASize');
      Pointer(FT_EEPROM_Read) := GetProc('FT_EEPROM_Read');
      Pointer(FT_EEPROM_Program) := GetProc('FT_EEPROM_Program');
      // FT2232C, FT232BM and FT245BM Extended API Functions
      Pointer(FT_GetLatencyTimer) := GetProc('FT_GetLatencyTimer');
      Pointer(FT_SetLatencyTimer) := GetProc('FT_SetLatencyTimer');
      Pointer(FT_GetBitMode) := GetProc('FT_GetBitMode');
      Pointer(FT_SetBitMode) := GetProc('FT_SetBitMode');
      Pointer(FT_SetUSBParameters) := GetProc('FT_SetUSBParameters');
      Pointer(FT_SetDeadmanTimeout) := GetProc('FT_SetDeadmanTimeout');
      {$ifndef Windows}
      // Extra functions for non-Windows platforms to compensate
      // for lack of .INF file to specify Vendor and Product IDs.
      Pointer(FT_SetVIDPID) := GetProc('FT_SetVIDPID');
      Pointer(FT_GetVIDPID) := GetProc('FT_GetVIDPID');
      Pointer(FT_GetDeviceLocId) := GetProc('FT_GetDeviceLocId');
      {$endif}
      // Win32-type functions
      Pointer(FT_W32_CreateFile) := GetProc('FT_W32_CreateFile');
      Pointer(FT_W32_CloseHandle) := GetProc('FT_W32_CloseHandle');
      Pointer(FT_W32_ReadFile) := GetProc('FT_W32_ReadFile');
      Pointer(FT_W32_WriteFile) := GetProc('FT_W32_WriteFile');
      Pointer(FT_W32_GetLastError) := GetProc('FT_W32_GetLastError');
      Pointer(FT_W32_GetOverlappedResult) := GetProc('FT_W32_GetOverlappedResult');
      Pointer(FT_W32_CancelIo) := GetProc('FT_W32_CancelIo');
      Pointer(FT_W32_ClearCommBreak) := GetProc('FT_W32_ClearCommBreak');
      Pointer(FT_W32_ClearCommError) := GetProc('FT_W32_ClearCommError');
      Pointer(FT_W32_EscapeCommFunction) := GetProc('FT_W32_EscapeCommFunction');
      Pointer(FT_W32_GetCommModemStatus) := GetProc('FT_W32_GetCommModemStatus');
      Pointer(FT_W32_GetCommState) := GetProc('FT_W32_GetCommState');
      Pointer(FT_W32_GetCommTimeouts) := GetProc('FT_W32_GetCommTimeouts');
      Pointer(FT_W32_PurgeComm) := GetProc('FT_W32_PurgeComm');
      Pointer(FT_W32_SetCommBreak) := GetProc('FT_W32_SetCommBreak');
      Pointer(FT_W32_SetCommMask) := GetProc('FT_W32_SetCommMask');
      //Pointer(FT_W32_GetCommMask) := GetProc('FT_W32_GetCommMask');
      Pointer(FT_W32_SetCommState) := GetProc('FT_W32_SetCommState');
      Pointer(FT_W32_SetCommTimeouts) := GetProc('FT_W32_SetCommTimeouts');
      Pointer(FT_W32_SetupComm) := GetProc('FT_W32_SetupComm');
      Pointer(FT_W32_WaitCommEvent) := GetProc('FT_W32_WaitCommEvent');
      // Version information
      Pointer(FT_GetDriverVersion) := GetProc('FT_GetDriverVersion');
      Pointer(FT_Rescan) := GetProc('FT_Rescan');
      Pointer(FT_Reload) := GetProc('FT_Reload');
      Pointer(FT_GetComPortNumber) := GetProc('FT_GetComPortNumber');
      // FT232H additional EEPROM functions
      Pointer(FT_EE_ReadConfig) := GetProc('FT_EE_ReadConfig');
      Pointer(FT_EE_WriteConfig) := GetProc('FT_EE_WriteConfig');
      Pointer(FT_EE_ReadECC) := GetProc('FT_EE_ReadECC');
      Pointer(FT_GetQueueStatusEx) := GetProc('FT_GetQueueStatusEx');
      Pointer(FT_ComPortIdle) := GetProc('FT_ComPortIdle');
      Pointer(FT_ComPortCancelIdle) := GetProc('FT_ComPortCancelIdle');
      Pointer(FT_VendorCmdGet) := GetProc('FT_VendorCmdGet');
      Pointer(FT_VendorCmdSet) := GetProc('FT_VendorCmdSet');
      Pointer(FT_VendorCmdGetEx) := GetProc('FT_VendorCmdGetEx');
      Pointer(FT_VendorCmdSetEx) := GetProc('FT_VendorCmdSetEx');
    end else flag:=FALSE;
    Result:=flag;
  end;

  procedure Unload_FTDLibrary;
  begin
    if FTDLibraryHandle<>DynLibs.NilHandle then begin
      DynLibs.UnloadLibrary(FTDLibraryHandle);
      FTDLibraryHandle:=DynLibs.NilHandle;
    end;
  end;

const
// IO Buffer Sizes
    FT_In_Buffer_Size = $10000;    // 64k
    FT_In_Buffer_Index = FT_In_Buffer_Size - 1;
    FT_Out_Buffer_Size = $10000;    // 64k
    FT_Out_Buffer_Index = FT_Out_Buffer_Size - 1;

var
// Declare Input and Output Buffers
//   FT_In_Buffer : Array[0..FT_In_Buffer_Index] of Byte;
//   FT_Out_Buffer : Array[0..FT_Out_Buffer_Index] of Byte;
// A variable used to detect time-outs
// Attach a timer to the main project form
// which decrements this every 10mS if
// FT_TimeOut_Count <> 0
//   FT_TimeOut_Count : Integer = 0;
// Used to determine how many bytes were
// actually received by FT_Read_Device_All
// in the case of a time-out
//   FT_All_Bytes_Received : Integer = 0;
//   FT_IO_Status : Ft_Result = FT_OK;
// Used By FT_ListDevices
   //FT_Device_Count : DWord;
//   FT_Device_String_Buffer : array [1..50] of Char;
   FT_Device_Location : DWord;
//   USB_Device_Info_Node : FT_Device_Info_Node;
//   FT_Event_Handle : DWord;

   _Description:  array [0..630] of char;
   _SerialNumber:  array [0..15] of char;
   _LocID : DWord;



  Procedure FT_Error_Report(ErrStr: String; PortStatus : Integer);
  Var Str : String;
  Begin
  If Not FT_Enable_Error_Report then Exit;
  If PortStatus = FT_OK then Exit;
  Case PortStatus of
      FT_INVALID_HANDLE : Str := ErrStr+' - Invalid handle...';
      FT_DEVICE_NOT_FOUND : Str := ErrStr+' - Device not found...';
      FT_DEVICE_NOT_OPENED : Str := ErrStr+' - Device not opened...';
      FT_IO_ERROR : Str := ErrStr+' - General IO error...';
      FT_INSUFFICIENT_RESOURCES : Str := ErrStr+' - Insufficient resources...';
      FT_INVALID_PARAMETER : Str := ErrStr+' - Invalid parameter...';
      FT_INVALID_BAUD_RATE : Str := ErrStr+' - Invalid baud rate...';
      FT_DEVICE_NOT_OPENED_FOR_ERASE : Str := ErrStr+' Device not opened for erase...';
      FT_DEVICE_NOT_OPENED_FOR_WRITE : Str := ErrStr+' Device not opened for write...';
      FT_FAILED_TO_WRITE_DEVICE : Str := ErrStr+' - Failed to write...';
      FT_EEPROM_READ_FAILED : Str := ErrStr+' - EEPROM read failed...';
      FT_EEPROM_WRITE_FAILED : Str := ErrStr+' - EEPROM write failed...';
      FT_EEPROM_ERASE_FAILED : Str := ErrStr+' - EEPROM erase failed...';
      FT_EEPROM_NOT_PRESENT : Str := ErrStr+' - EEPROM not present...';
      FT_EEPROM_NOT_PROGRAMMED : Str := ErrStr+' - EEPROM not programmed...';
      FT_INVALID_ARGS : Str := ErrStr+' - Invalid arguments...';
      FT_OTHER_ERROR : Str := ErrStr+' - Other error ...';
      End;
  MessageDlg(Str, mtError, [mbOk], 0);
  End;


  // FTD2XX functions from here
  Function GetFTDeviceCount : FT_Result;
  Begin
  Result := FT_GetNumDevices(@FT_Device_Count,Nil,FT_LIST_NUMBER_ONLY);
  If Result <> FT_OK then FT_Error_Report('GetFTDeviceCount',Result);
  End;

  Function GetFTDeviceSerialNo(DeviceIndex:DWord) : FT_Result;
  Begin
  Result := FT_ListDevices(DeviceIndex,@_SerialNumber,(FT_OPEN_BY_SERIAL_NUMBER or FT_LIST_BY_INDEX));
  If Result = FT_OK then FT_Device_String := _SerialNumber;
  If Result <> FT_OK then FT_Error_Report('GetFTDeviceSerialNo',Result);
  End;


  Function GetFTDeviceDescription(DeviceIndex:DWord) : FT_Result;
  Begin
  Result := FT_ListDevices(DeviceIndex,@_Description,(FT_OPEN_BY_DESCRIPTION or FT_LIST_BY_INDEX));
  If Result = FT_OK then FT_Device_String := _Description;
  If Result <> FT_OK then FT_Error_Report('GetFTDeviceDescription',Result);
  End;


  Function GetFTDeviceLocation(DeviceIndex:DWord) : FT_Result;
  Begin
  Result := FT_ListDevices(DeviceIndex,@_LocID,(FT_OPEN_BY_LOCATION or FT_LIST_BY_INDEX));
  If Result = FT_OK then FT_Device_Location := _LocID;
  If Result <> FT_OK then FT_Error_Report('GetFTDeviceLocation',Result);
  End;

{
  Function Open_USB_Device : FT_Result;
  Var
    DevIndex : DWord;
  Begin
  DevIndex := 0;
  Result := FT_Open(DevIndex,@FT_Handle);
  If Result <> FT_OK then FT_Error_Report('FT_Open',Result);
  End;


  Function Open_USB_Device_By_Serial_Number(Serial_Number:string) : FT_Result;
  Begin
  SetDeviceString(Serial_Number);
  Result := FT_OpenEx(@FT_Device_String_Buffer,FT_OPEN_BY_SERIAL_NUMBER,@FT_Handle);
  If Result <> FT_OK then FT_Error_Report('Open_USB_Device_By_Serial_Number',Result);
  End;


  Function Open_USB_Device_By_Device_Description(Device_Description:string) : FT_Result;
  Begin
  SetDeviceString(Device_Description);
  Result := FT_OpenEx(@FT_Device_String_Buffer,FT_OPEN_BY_DESCRIPTION,@FT_Handle);
  If Result <> FT_OK then FT_Error_Report('Open_USB_Device_By_Device_Description',Result);
  End;


  Function Open_USB_Device_By_Device_Location(Location:DWord) : FT_Result;
  Begin
  Result := FT_OpenByLocation(Location,FT_OPEN_BY_LOCATION,@FT_Handle);
  If Result <> FT_OK then FT_Error_Report('Open_USB_Device_By_Device_Location',Result);
  End;


  Function Close_USB_Device : FT_Result;
  Begin
  Result :=  FT_Close(FT_Handle);
  If Result <> FT_OK then FT_Error_Report('FT_Close',Result);
  End;


  function Read_USB_Device_Buffer( Read_Count : Integer ) : Integer;
  // Reads Read_Count Bytes (or less) from the USB device to the FT_In_Buffer
  // Function returns the number of bytes actually received  which may range from zero
  // to the actual number of bytes requested, depending on how many have been received
  // at the time of the request + the read timeout value.
  Var Read_Result : Integer;
  Begin

  if (read_count = 1) then
    begin
    read_result := read_count;
    end;
  FT_IO_Status := FT_Read(FT_Handle,@FT_In_Buffer,Read_Count,@Read_Result);
  If FT_IO_Status <> FT_OK then FT_Error_Report('FT_Read',FT_IO_Status);
  Result := Read_Result;
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


  Function Reset_USB_Device : FT_Result;
  Begin
  Result :=  FT_ResetDevice(FT_Handle);
  If Result <> FT_OK then FT_Error_Report('FT_ResetDevice',Result);
  End;


  Function Set_USB_Device_BaudRate : FT_Result;
  Begin
  Result :=  FT_SetBaudRate(FT_Handle,FT_Current_Baud);
  If Result <> FT_OK then FT_Error_Report('FT_SetBaudRate',Result);
  End;


  Function Set_USB_Device_BaudRate_Divisor(Divisor:Dword) : FT_Result;
  Begin
  Result :=  FT_SetDivisor(FT_Handle,Divisor);
  If Result <> FT_OK then FT_Error_Report('FT_SetDivisor',Result);
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


  Function Purge_USB_Device_Out : FT_Result;
  Begin
  Result :=  FT_Purge(FT_Handle,FT_PURGE_RX);
  If Result <> FT_OK then FT_Error_Report('FT_Purge RX',Result);
  End;

  Function Purge_USB_Device_In : FT_Result;
  Begin
  Result :=  FT_Purge(FT_Handle,FT_PURGE_TX);
  If Result <> FT_OK then FT_Error_Report('FT_Purge TX',Result);
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


  Function Set_USB_Device_Break_On : FT_Result;
  Begin
  Result :=  FT_SetBreakOn(FT_Handle);
  If Result <> FT_OK then FT_Error_Report('FT_SetBreakOn',Result);
  End;


  Function Set_USB_Device_Break_Off : FT_Result;
  Begin
  Result :=  FT_SetBreakOff(FT_Handle);
  If Result <> FT_OK then FT_Error_Report('FT_SetBreakOff',Result);
  End;


  Function Get_USB_Device_Status : FT_Result;
  Begin
  Result :=  FT_GetStatus(FT_Handle, @FT_Q_Bytes, @FT_TxQ_Bytes, @FT_Event_Status);
  If Result <> FT_OK then FT_Error_Report('FT_GetStatus',Result);
  End;


  Function Set_USB_Device_Event_Notification(EventMask:DWord) : FT_Result;
  Begin
  Result := FT_SetEventNotification(FT_Handle,EventMask,FT_Event_Handle);
  If Result <> FT_OK then FT_Error_Report('FT_SetEventNotification ',Result);
  End;


  Function USB_FT_GetDeviceInfo(DevType,ID:DWord; SerialNumber,Description:array of char) : FT_Result;
  begin
  Result := FT_GetDeviceInfo(FT_Handle,@DevType,@ID,@SerialNumber,@Description,Nil);
  If Result <> FT_OK then FT_Error_Report('FT_GetDeviceInfo ',Result);
  end;


  Function Set_USB_Device_Reset_Pipe_Retry_Count(RetryCount:DWord) : FT_Result;
  Begin
  Result :=  FT_SetResetPiperetryCount(FT_Handle, RetryCount);
  If Result <> FT_OK then FT_Error_Report('FT_SetResetPipeRetryCount',Result);
  End;


  Function Stop_USB_Device_InTask : FT_Result;
  Begin
  Result :=  FT_StopInTask(FT_Handle);
  If Result <> FT_OK then FT_Error_Report('FT_StopInTask',Result);
  End;


  Function Restart_USB_Device_InTask : FT_Result;
  Begin
  Result :=  FT_RestartInTask(FT_Handle);
  If Result <> FT_OK then FT_Error_Report('FT_RestartInTask',Result);
  End;


  Function Reset_USB_Port : FT_Result;
  Begin
  Result :=  FT_ResetPort(FT_Handle);
  If Result <> FT_OK then FT_Error_Report('FT_ResetPort',Result);
  End;


  Function Cycle_USB_Port : FT_Result;
  Begin
  Result :=  FT_CyclePort(FT_Handle);
  If Result <> FT_OK then FT_Error_Report('FT_CyclePort',Result);
  End;


  Function Create_USB_Device_List : FT_Result;
  Begin
  Result :=  FT_CreateDeviceInfoList(@FT_Device_Count);
  If Result <> FT_OK then FT_Error_Report('FT_CreateDeviceInfoList',Result);
  End;


  Function Get_USB_Device_List : FT_Result;
  Begin
  SetLength(FT_DeviceInfoList,FT_Device_Count);
  Result :=  FT_GetDeviceInfoList(@FT_DeviceInfoList, @FT_Device_Count);
  If Result <> FT_OK then FT_Error_Report('FT_GetDeviceInfoList',Result);
  End;

  Function Get_USB_Driver_Version(DrVersion : TDWordPtr) : FT_Result;
  Begin
     Result :=  FT_GetDriverVersion(FT_Handle, DrVersion);
     If Result <> FT_OK then FT_Error_Report('FT_GetDriverVersion',Result);
  End;

  Function Get_USB_Library_Version(LbVersion : TDWordPtr) : FT_Result;
  Begin
     Result :=  FT_GetLibraryVersion(LbVersion);
     If Result <> FT_OK then FT_Error_Report('FT_GetLibraryVersion',Result);
  End;

}

end.


