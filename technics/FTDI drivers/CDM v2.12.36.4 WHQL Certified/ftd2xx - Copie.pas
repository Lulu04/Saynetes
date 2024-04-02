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
 adapted for Lazarus by Lulu - 2022
}

uses
  Classes, SysUtils,
{$ifdef Windows}
  windows,
{$endif}
  ctypes;


// library filename
{$ifdef Windows}
  {$ifdef CPU32}
    FT_DLL_Name = 'ftd2xx.DLL';
  {$else}
    FT_DLL_Name = 'ftd2xx64.DLL';
  {$endif}
{$endif}

{$ifdef Linux}
  FT_DLL_Name = 'libftd2xx.so.1.4.24';
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
    function FT_EE_Program(ftHandle:FT_HANDLE; pEEData:PFT_PROGRAM_DATA):FT_Result; stdcall; External FT_DLL_Name name 'FT_EE_Program';
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

implementation

end.


