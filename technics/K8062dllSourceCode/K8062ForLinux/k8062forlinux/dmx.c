/*******************************************/
/* Velleman K8062 DMX controller for Linux */
/* Helper functions for VM116/K8062        */
/*                                         */
/* Compile with gcc -o dmxd dmxd.c -lusb   */
/* (c) Denis Moreaux 2008                  */
/* Denis Moreaux <vapula@endor.be>         */
/*                                         */
/*******************************************/

#include <stdio.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include "dmx.h"

int *dmx_maxchannels;
int *dmx_shutdown;
int *dmx_caps;
int *dmx_channels;

int *shm;
int shmid;

void dmx_open()
{
    
    shmid=shmget(0x56444D58,sizeof(int)*522, 0666);
    shm=(int *)shmat(shmid,NULL,0);
    dmx_maxchannels=shm;
    dmx_shutdown=shm+1;
    dmx_caps=shm+2;
    dmx_channels=shm+10;
}

void dmx_close()
{
    shmdt(shm);
}

