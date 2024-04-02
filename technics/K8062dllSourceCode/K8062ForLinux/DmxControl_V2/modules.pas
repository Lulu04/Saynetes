{  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as
  published by the Free Software Foundation; either version 3 of
  the License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Lesser General Public License for more details. }

unit Modules;                //interfaçage  differents  modules

{$mode objfpc}{$H+}

interface

uses
    SysUtils, Dialogs, k8062, Dummy, Usb;

    procedure Dmx_Write_Data(x,y:longint);         //ecriture data dmx
    Function  Dmx_Read(x:longint):longint;         //lecture data dmx
    Procedure Dmx_MaxChannel(Nc:longint);          //fixe nombre max canaux
    Procedure Dmx_AllZero;                         //tout a zero
    Procedure Dmx_Stop;                            //arret module
    Procedure Dmx_SetModule;			   //fixe l'interface en cours
    Function  Dmx_Usb_Device:PUSB_Dev_Handle;	   //renvoi Handle periph usb

implementation

var Module : Integer;
    Udev : PUSB_Dev_Handle;

procedure Dmx_Write_data(x,y:longint);             //ecriture data dmx
begin
     case Module of
     	  0:Dummy_Write(x,y);
     	  1:k8062_Write(10+x,y);                   //data commence a +10
     end;
end;

Function  Dmx_Read(x:longint):longint;         	   //lecture data dmx
begin
     	  Dmx_Read:=0;
end;

Procedure Dmx_MaxChannel(Nc:longint);          	   //fixe nombre max canaux
begin
     case Module of
     	  0:Dummy_MaxChannel(nc);
     	  1:k8062_MaxChannel(nc);
     end;
end;

Procedure Dmx_AllZero;                         	   //tout a zero
begin
     case Module of
     	  0:Dummy_AllZero;
     	  1:k8062_AllZero;
     end;
end;

Procedure Dmx_stop;                            	   //arret module
begin
     case Module of
     	  0:Dummy_Stop_Dmx;
     	  1:k8062_Stop_Dmx;
     end;
end;

Procedure Dmx_SetModule;   			   //fixe l'interface en cours
begin
     ShowMessage('Choix du module '+IntToStr(Module));
     case Module of
     	  0:Dummy_Init;
     	  1:k8062_Init;
     end;
end;

procedure Dmx_FindModule;
var  	Busses : PUSB_Bus;
    	Dev    : PUSB_Devices;
    	Found  : PUSB_Devices;

begin
     	Module:=0;	       			   //suppose pas de modules trouve
        Usb_init;
        if usb_find_Busses > 0  then
        begin
             if usb_find_Devices > 0 then
             begin
             	  Busses:= Usb_Get_Busses;
    	     	  While Busses <> Nil do
    	     	  Begin
      	     	       Dev := Busses^.Devices;
      	     	       While Dev <> Nil do
             	       Begin
             	       	    if (Dev^.Descriptor.idVendor=$10cf) and
                                    (Dev^.Descriptor.idProduct=$8062) then
                            begin
                                Module:=1;  //k8062
		          	//udev:=usb_open(dev);
			  	// #if defined(LIBUSB_HAS_GET_DRIVER_NP) && defined(LIBUSB_HAS_DETACH_KERNEL_DRIVER_NP)
	   		  	// usb_detach_kernel_driver_np( udev, 0);
			  	// #endif
	   		  	// usb_set_configuration(udev, 1);
	   		  	// usb_claim_interface(udev, 0);
                            end;
                            //
			    Dev := Dev^.Next;
             	       End;
      	     	       Busses := Busses^.Next;
    	     	  End;
             End;
        End;
        Dmx_SetModule;				//module
end;

Function  Dmx_Usb_Device:PUSB_Dev_Handle;	   	//renvoi Handle periph usb
begin
     	Dmx_Usb_Device:=udev
end;

begin
      Dmx_FindModule;
end.

