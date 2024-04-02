{  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as
  published by the Free Software Foundation; either version 3 of
  the License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Lesser General Public License for more details. }
  
unit K8062;

interface
         type Tdmxdata = array[0..511] of byte;
//*******************************************************************************************************

         Function  K8062_Read(x:longint):longint;       //lecture octet shm
         Procedure K8062_Write(x,y:longint);	        //ecriture octet shm
         Procedure K8062_MaxChannel(Nc:longint);        //fixe nombre max canaux
         Procedure K8062_AllZero;                       //Tous les canaux a zero
         Procedure K8062_Stop_dmx;			//arret demon dmx + programme
	 procedure k8062_init;                          //init

//*******************************************************************************************************

implementation
	uses ipc, sysutils, dialogs;



type	plongint = ^longint;       			//longint correspond au integer du c pour le 64 bits

var  	key:longword;
	segsize,shmid :longint;
	segptr:plongint;

procedure K8062_stop_dmx;
begin
	K8062_Write(1,1);				//ecriture
end;

procedure k8062_init;
begin
	key :=$56444D58;  				//clef shm k8062
	segsize:=2088;
	shmid := shmget(key,segsize,0);		
	If shmid = -1 then
	begin
		Showmessage('Memoire partagee non trouvee');
		halt(1);
	end;
	segptr:=shmat(shmid,nil,0);			//segptr : adresse physique
	if longint(segptr)=-1 then
	begin
		ShowMessage('Erreur Attribution memoire');
		halt(2);
	end;
end;

procedure K8062_MaxChannel(Nc:Longint);                    //fixe Nombre canaux max
begin;
      if (Nc>22) and (Nc<511) then
         K8062_Write(0,Nc);
end;

Procedure K8062_Write(x,y:longint);
begin
	segptr[x]:=y;
end;

Function K8062_Read(x:longint):longint;
begin
	K8062_Read:=segptr[x];
end;


Procedure K8062_AllZero;                            //Tous les canaux a zero
          var x:integer;
begin
     for x:=0 to 511 do
         K8062_Write(x+10,0);
end;

end.
