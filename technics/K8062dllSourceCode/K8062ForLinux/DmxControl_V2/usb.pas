unit usb;

interface

{$LINKLIB c}
{$LINKLIB usb}

{
  Automatically converted by H2Pas 1.0.0 from usb.h
  The following command line parameters were used:
    -d
    -s
    -l
    usb.o
    -o
    usb.pas
    -v
    usb.h
		
		Edition manuelle - Jean-Luc NEDELEC
			Juin 2010

}

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


  const
	PATH_MAX=4096;
	//
	USB_CLASS_PER_INTERFACE = 0;     
	USB_CLASS_AUDIO = 1;     
	USB_CLASS_COMM = 2;     
	USB_CLASS_HID = 3;     
	USB_CLASS_PRINTER = 7;     
	USB_CLASS_PTP = 6;     
	USB_CLASS_MASS_STORAGE = 8;     
	USB_CLASS_HUB = 9;     
	USB_CLASS_DATA = 10;     
	USB_CLASS_VENDOR_SPEC = $ff;     
	USB_DT_DEVICE = $01;     
	USB_DT_CONFIG = $02;     
	USB_DT_STRING = $03;     
	USB_DT_INTERFACE = $04;     
	USB_DT_ENDPOINT = $05;     
	USB_DT_HID = $21;     
	USB_DT_REPORT = $22;     
	USB_DT_PHYSICAL = $23;     
	USB_DT_HUB = $29;     
	USB_DT_DEVICE_SIZE = 18;     
	USB_DT_CONFIG_SIZE = 9;     
	USB_DT_INTERFACE_SIZE = 9;     
	USB_DT_ENDPOINT_SIZE = 7;     
	USB_DT_ENDPOINT_AUDIO_SIZE = 9;     
	USB_DT_HUB_NONVAR_SIZE = 7;     
	// 
	USB_MAXENDPOINTS = 32;  
	//
	USB_ENDPOINT_ADDRESS_MASK = $0f;     
	USB_ENDPOINT_DIR_MASK = $80;     
	USB_ENDPOINT_TYPE_MASK = $03;     
	USB_ENDPOINT_TYPE_CONTROL = 0;     
	USB_ENDPOINT_TYPE_ISOCHRONOUS = 1;     
	USB_ENDPOINT_TYPE_BULK = 2;     
	USB_ENDPOINT_TYPE_INTERRUPT = 3;     
	USB_MAXINTERFACES = 32;     
	//
	USB_MAXALTSETTING = 128;     
	//
	USB_MAXCONFIG = 8;     
	//
	USB_REQ_GET_STATUS = $00;     
	USB_REQ_CLEAR_FEATURE = $01;     
	USB_REQ_SET_FEATURE = $03;     
	USB_REQ_SET_ADDRESS = $05;     
	USB_REQ_GET_DESCRIPTOR = $06;     
	USB_REQ_SET_DESCRIPTOR = $07;     
	USB_REQ_GET_CONFIGURATION = $08;     
	USB_REQ_SET_CONFIGURATION = $09;     
	USB_REQ_GET_INTERFACE = $0A;     
	USB_REQ_SET_INTERFACE = $0B;     
	USB_REQ_SYNCH_FRAME = $0C;     
	USB_TYPE_STANDARD = $00 shl 5;     
	USB_TYPE_CLASS = $01 shl 5;     
	USB_TYPE_VENDOR = $02 shl 5;     
	USB_TYPE_RESERVED = $03 shl 5;     
	USB_RECIP_DEVICE = $00;     
	USB_RECIP_INTERFACE = $01;     
	USB_RECIP_ENDPOINT = $02;     
	USB_RECIP_OTHER = $03;     
	USB_ENDPOINT_IN = $80;     
	USB_ENDPOINT_OUT = $00;     
	USB_ERROR_BEGIN = 500000;     
	//
	LIBUSB_HAS_GET_DRIVER_NP = 1;       
	LIBUSB_HAS_DETACH_KERNEL_DRIVER_NP = 1;       

Type

	Pusb_descriptor_header = ^usb_descriptor_header;
	Pusb_string_descriptor = ^usb_string_descriptor;
	Pusb_hid_descriptor = ^usb_hid_descriptor;
	Pusb_endpoint_descriptor = ^usb_endpoint_descriptor;  
	Pusb_interface_descriptor = ^usb_interface_descriptor;
	Pusb_interface = ^usb_interface;
	Pusb_config_descriptor = ^usb_config_descriptor;
	Pusb_device_descriptor = ^usb_device_descriptor;
	Pusb_ctrl_setup = ^usb_ctrl_setup;
	Pusb_devices = ^usb_devices;
	Pusb_bus = ^usb_bus;
	Pusb_dev_handle = ^usb_dev_handle;
      

	usb_descriptor_header  = record
          bLength : Byte;
          bDescriptorType : Byte;
	end;

	usb_string_descriptor = record
          bLength : Byte;
          bDescriptorType : Byte;
          wData : array[0..0] of Word;
	end;

	usb_hid_descriptor = record
          bLength : Byte;
          bDescriptorType : Byte;
          bcdHID : Word;
          bCountryCode : Byte;
          bNumDescriptors : Byte;
	end;

	usb_endpoint_descriptor = record
          bLength : Byte;
          bDescriptorType : Byte;
          bEndpointAddress : Byte;
          bmAttributes : Byte;
          wMaxPacketSize : Word;
          bInterval : Byte;
          bRefresh : Byte;
          bSynchAddress : Byte;
          extra : ^byte;
          extralen : longint;
	end;

	usb_interface_descriptor = record
          bLength : Byte;
          bDescriptorType : Byte;
          bInterfaceNumber : Byte;
          bAlternateSetting : Byte;
          bNumEndpoints : Byte;
          bInterfaceClass : Byte;
          bInterfaceSubClass : Byte;
          bInterfaceProtocol : Byte;
          iInterface : Byte;
          endpoint : Pusb_endpoint_descriptor;
          extra : ^byte;
          extralen : longint;
	end;

	usb_interface = record
          altsetting : Pusb_interface_descriptor;
          num_altsetting : longint;
	end;

	usb_config_descriptor = record
          bLength : Byte;
          bDescriptorType : Byte;
          wTotalLength : Word;
          bNumInterfaces : Byte;
          bConfigurationValue : Byte;
          iConfiguration : Byte;
          bmAttributes : Byte;
          MaxPower : Byte;
          Linterface : Pusb_interface;
          extra : ^byte;
          extralen : longint;
	end;

	usb_device_descriptor = record
          bLength : Byte;
          bDescriptorType : Byte;
          bcdUSB : Word;
          bDeviceClass : Byte;
          bDeviceSubClass : Byte;
          bDeviceProtocol : Byte;
          bMaxPacketSize0 : Byte;
          idVendor : Word;
          idProduct : Word;
          bcdDevice : Word;
          iManufacturer : Byte;
          iProduct : Byte;
          iSerialNumber : Byte;
          bNumConfigurations : Byte;
	end;

	usb_ctrl_setup = record
          bRequestType : Byte;
          bRequest : Byte;
          wValue : Word;
          wIndex : Word;
          wLength : Word;
	end;

//{$if 0}
// (* error 
//#define USB_LE16_TO_CPU(x) do { x = ((x & 0xff) << 8) | ((x & 0xff00) >> 8); } while(0)
//in declaration at line 227 *)
//(* error 
//#define USB_LE16_TO_CPU(x) do { x = ((x & 0xff) << 8) | ((x & 0xff00) >> 8); } while(0)
//{$else}
//in define line 229 *) */
//{$endif}
	
	usb_devices = record
            next : Pusb_devices;
            prev : Pusb_devices;
            filename : array[0..(PATH_MAX+1)-1] of char;
            bus : Pusb_bus;
            descriptor : usb_device_descriptor;
            config : Pusb_config_descriptor;
            dev : pointer;
            devnum : Byte;
            num_children : byte;
            children : Pusb_devices;
         end;

	usb_bus = record
            next : Pusb_bus;
            prev : Pusb_bus;
            dirname : array[0..(PATH_MAX+1)-1] of char;
            devices : Pusb_devices;
            location : Word;
            root_dev : Pusb_devices;
         end;

	usb_dev_handle = record
           {undefined structure}
         end;



	var  usb_busses : Pusb_bus;cvar;external;

{ C++ extern C conditionnal removed }


    function usb_open(dev:Pusb_devices):Pusb_dev_handle;cdecl;external;
    function usb_close(dev:Pusb_dev_handle):longint;cdecl;external;
    function usb_get_string(dev:Pusb_dev_handle; index:longint; langid:longint; buf:pchar; buflen:word):longint;cdecl;external;
    function usb_get_string_simple(dev:Pusb_dev_handle; index:longint; buf:pchar; buflen:word):longint;cdecl;external;
    function usb_get_descriptor_by_endpoint(dev:Pusb_dev_handle; ep:longint; _type:byte; index:byte; var buf:pointer; 
               size:longint):longint;cdecl;external;
    function usb_get_descriptor(dev:Pusb_dev_handle; _type:byte; index:byte; var buf:pointer; size:longint):longint;cdecl;external;
    function usb_bulk_write(dev:Pusb_dev_handle; ep:longint; bytes:pchar; size:longint; timeout:longint):longint;cdecl;external;
    function usb_bulk_read(dev:Pusb_dev_handle; ep:longint; bytes:pchar; size:longint; timeout:longint):longint;cdecl;external;
    function usb_interrupt_write(dev:Pusb_dev_handle; ep:longint; bytes:pchar; size:longint; timeout:longint):longint;cdecl;external;
    function usb_interrupt_read(dev:Pusb_dev_handle; ep:longint; bytes:pchar; size:longint; timeout:longint):longint;cdecl;external;
    function usb_control_msg(dev:Pusb_dev_handle; requesttype:longint; request:longint; value:longint; index:longint; 
               bytes:pchar; size:longint; timeout:longint):longint;cdecl;external;
    function usb_set_configuration(dev:Pusb_dev_handle; configuration:longint):longint;cdecl;external;
    function usb_claim_interface(dev:Pusb_dev_handle; Linterface:longint):longint;cdecl;external;
    function usb_release_interface(dev:Pusb_dev_handle; Linterface:longint):longint;cdecl;external;
    function usb_set_altinterface(dev:Pusb_dev_handle; alternate:longint):longint;cdecl;external;
    function usb_resetep(dev:Pusb_dev_handle; ep:dword):longint;cdecl;external;
    function usb_clear_halt(dev:Pusb_dev_handle; ep:dword):longint;cdecl;external;
    function usb_reset(dev:Pusb_dev_handle):longint;cdecl;external;
    function usb_get_driver_np(dev:Pusb_dev_handle; Linterface:longint; name:pchar; namelen:dword):longint;cdecl;external;
    function usb_detach_kernel_driver_np(dev:Pusb_dev_handle; Linterface:longint):longint;cdecl;external;
    function usb_strerror:Pchar;cdecl;external;
    procedure usb_init;cdecl;external;
    procedure usb_set_debug(level:longint);cdecl;external;
    function usb_find_busses:longint;cdecl;external;
    function usb_find_devices:longint;cdecl;external;
    function usb_get_busses:Pusb_Bus;cdecl; external;
    function usb_device(Dev:Pusb_dev_handle):Pusb_Devices; cdecl; external;

implementation


end.
