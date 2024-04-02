{  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as
  published by the Free Software Foundation; either version 3 of
  the License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Lesser General Public License for more details. }

unit Dummy;  	     //utilisation deboguage

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, extctrls;

         Function  Dummy_Read(x:longint):longint;       //lecture octet shm
         Procedure Dummy_Write(x,y:longint);	        //ecriture octet shm
         Procedure Dummy_MaxChannel(Nc:longint);        //fixe nombre max canaux
         Procedure Dummy_AllZero;                       //Tous les canaux a zero
         Procedure Dummy_Stop_dmx;			//arret demon dmx + programme
	 procedure Dummy_init;                          //init

implementation
	 var f:TextFile;
             filename:string;

Function  Dummy_Read(x:longint):longint;       		//lecture octet shm
begin
     if filename<>'' then
     begin
          Writeln(f,'Lecture Position '+InttoStr(x));
     end;
     Dummy_read:=0;
end;

Procedure Dummy_Write(x,y:longint);	        	//ecriture octet shm
begin
     if filename<>'' then
     begin
       Writeln(f,'Ecriture Position '+InttoStr(x)+ ' Valeur = '+InttoStr(y));
     end;
end;

Procedure Dummy_MaxChannel(Nc:longint);        		//fixe nombre max canaux
begin
     if filename<>'' then
     begin
       writeln(f);
       Writeln(f,'Fixation nombre de canaux a '+Inttostr(nc));
       writeln(f);
     end;
end;

Procedure Dummy_AllZero;                       		//Tous les canaux a zero
begin
     if filename<>'' then
     begin
       writeln(f);
       Writeln(f,'Tous canaux dmx a 0');
       writeln(f);
     end;
end;

Procedure Dummy_init;                          		//init
begin
     Filename:='./debug.txt';
     AssignFile(f,Filename);                            //creation fichier
     Rewrite(f);
     Writeln(f,'****** Debut Capture Dmx fichier '+filename+' ******');
     writeln(f);
end;


Procedure Dummy_Stop_dmx;				//arret dmx + programme
begin
     if filename<>'' then
     begin
       writeln(f);
       Writeln(f,'****** Arret capture ******');
       CloseFile(f);
       Filename:='';
     end;
end;

end.

