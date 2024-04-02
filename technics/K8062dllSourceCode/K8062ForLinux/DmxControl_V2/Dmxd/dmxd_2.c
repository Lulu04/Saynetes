/*  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as
  published by the Free Software Foundation; either version 3 of
  the License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Lesser General Public License for more details. */

/*******************************************/
/* Velleman K8062 DMX controller for Linux */
/* Daemon for VM116/K8062                  */
/*                                         */
/* Compile with gcc -o dmxd dmxd_2.c -lusb */
/* (c) Denis Moreaux 2008                  */
/* Denis Moreaux <vapula@endor.be>         */
/*  -modif  Dmxcontrol 2010          */
/*                                         */ 
/*  !!!!ce daemon doit etre lance avec les */
/*  !!!! droits root                       */ 
/*******************************************/


#include <usb.h>
#include <unistd.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <string.h>

struct usb_bus *bus;
struct usb_device *dev;
usb_dev_handle *udev;
int *channels;
int *maxchannel;
int *shutdown;
int *caps;
int *shm;
int shmid;

void write_command ( unsigned char *data )
{
	usb_interrupt_write(udev,0x1,(char*)data,8,20);
}

void sendDMX()
{
  int i, n;
  unsigned char data[8];
  int m;

  m=*maxchannel;
  for (i=0;(i<100) && !channels[i] && (i < m - 6);i++); 

  data[0] = 4; /* Start of data, number of leading 0 and 6 channels */
  data[1] = i+1;
  data[2] = channels[i];
  data[3] = channels[i+1];
  data[4] = channels[i+2];
  data[5] = channels[i+3];
  data[6] = channels[i+4];
  data[7] = channels[i+5];
  write_command(data);
  i+=6;

  while (i < m - 7) {
    if (!channels[i]) {
      for(n=i+1;(n < m - 6) && (n-i<100) && !channels[n] ;n++);
	data[0] = 5;
	data[1] = n-i;
	data[2] = channels[n];
	data[3] = channels[n+1];
	data[4] = channels[n+2];
	data[5] = channels[n+3];
	data[6] = channels[n+4];
	data[7] = channels[n+5];
	write_command(data);
	i=n+6;
    } else {
      data[0] = 2; /* 7 channels */
      data[1] = channels[i];
      data[2] = channels[i+1];
      data[3] = channels[i+2];
      data[4] = channels[i+3];
      data[5] = channels[i+4];
      data[6] = channels[i+5];
      data[7] = channels[i+6];
      write_command(data);
      i+=7;
    }
  }
  
  for(;i < m;i++) {
    data[0] = 3; /* send one channel */
    data[1] = channels[i];
    write_command(data);
  }
}

int initUSB()
{
  usb_init();
  usb_find_busses();
  usb_find_devices();

  for (bus = usb_busses; bus; bus = bus->next) {
    for (dev = bus->devices; dev; dev = dev->next) {
      if ( (dev->descriptor.idVendor == 0x10cf) &&
	   (dev->descriptor.idProduct == 0x8062 ) ) {
	   udev=usb_open(dev);
	#if defined(LIBUSB_HAS_GET_DRIVER_NP) && defined(LIBUSB_HAS_DETACH_KERNEL_DRIVER_NP)
	   usb_detach_kernel_driver_np( udev, 0);
	#endif
	   usb_set_configuration(udev, 1);
	   usb_claim_interface(udev, 0);
	   return 1;
      }
    }
  }
  return 0;
}

void initSHM()
{
    
    shmid=shmget(0x56444D58,sizeof(int)*522,IPC_CREAT | 0666);
    shm=(int *)shmat(shmid,NULL,0);
    memset(shm,0,sizeof(int)*522);
    maxchannel=shm;
    shutdown=shm+1;
    caps=shm+2;
    channels=shm+10;
    *shutdown=0; /* Run mode */
    *caps=0; /* Services available flags */
    *maxchannel=22;
}

void release()
{
    usb_close(udev);
    shmdt(shm);
    shmctl(shmid,IPC_RMID,NULL);
}

int main() {
	struct timespec ts;
	ts.tv_sec=0;
	ts.tv_nsec=250000;	/* 25ms*/
	int errno;
	errno=0;			//suppose pas d'erreur
	//printf("Demarrage interface DMX2 nanosleep() \n");
	if (initUSB()) {
	initSHM();
	while(*shutdown != 1) 
		{
			if (*maxchannel<22)
			*maxchannel=22;
			sendDMX();
			nanosleep(&ts,NULL);
		}
	memset(channels,0,512*sizeof(int));
	*maxchannel=512;
	sendDMX();
	release();
	} 
	else 
	{
		errno=-1;		//erreur device
		printf("No DMX device found\n");
	}
	return errno;		//return le code erreur
}
