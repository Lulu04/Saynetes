{  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as
  published by the Free Software Foundation; either version 3 of
  the License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Lesser General Public License for more details. }

Unit DmxUtils;

interface

 uses Modules, dialogs, sysutils;

	Type

        TabScene = Array[0..511] of word;        //stocke les scenes (index effets des appareils)

        TEffect = record               	 	 // type record of unite
              Enom : string;                     // Nom effet
              Erec : array [0..15] of byte;      // valeur dmx
        end;

        TabEffect = array of Teffect;            //type tableau dynamique

 	TUrec = record                           // pour sauvegarde et restauration
              Ustartadr,Unbrvoie:word;
    	      Unom:string;                       //descriptif
	      Utype:string;		  	 //type materiel
    	      Uvoie:array[0..15] of string[50];  //assignation des voies
        end;

        TSrec = record
              Snom:string;     	    		 //nom de la scene
  	      Stimer:longint;		      	 //temps timer passage trame suivante
              Sscene:TabScene;                   //composition scene
        end;


  	Ptrunit = ^TdmxUnit;
   	TdmxUnit = Object
    	      Constructor create;
    	      Destructor Destroy;virtual;
    	      Procedure SetName(str:string);
	      Procedure SetType(str:string);
    	      Procedure SetNbrVoie(nbr:byte);
    	      Procedure SetNameVoie(num:byte;str:string);
    	      Procedure SetStartAdr(start:integer);       //fonction place dans la liste
              procedure AddEffect(Efrec:TEffect);         //ajoute effet
              procedure DelEffect(num:integer);		  //efface effet
              procedure SetEffect(num:integer);           //fixe effet et ecriture ipc
              procedure ModEffect(num:integer;Efrec:Teffect);
       	      procedure SetRecUnit(RecUnit:Turec);	  //pour load
              Function  GetEffect:integer;                //lecture index effet
	      Function  GetNbrEffect:integer;             //Nombre effet enregistres
       	      Function 	GetName:string;
	      Function 	GetType:string;
	      Function  GetNameVoie(num:byte):string;
    	      Function  GetNbrVoie:byte;
    	      Function  GetStartAdr:integer;         	      //adresse enregistrée
              Function  GetRecUnit:TUrec;  	   	      //pour save car record zone private
              Function  GetEffectUnit(num:integer):TEffect;   //car effet en zone private
              Function  GetLow:integer;			      //indice bas tableau
              Function  GetHigh:integer;                      //indice haut tableau dynamique

  	private
              Unitrec: TUrec;			 //caracteristique unite;
              Effect: TabEffect;          	 //tableau dynamique des effets enregistrables
              IndexEffect:integer;		 //index effet en cours

        public
              property Name:string Read Getname Write Setname;
              property DmxAdr:integer Read GetStartAdr Write SetStartAdr;
              property TypeUnit:string Read GetType Write SetType;
              property NbrVoie:byte Read GetNbrVoie Write SetNbrVoie;
              property Effet:integer Read GetEffect Write SetEffect;
              { public declarations }
        end;


        Type

   	Ptrscene = ^Tdmxscene;
  	TdmxScene = Object

              Constructor Create;
              Destructor Destroy;virtual;
              Procedure SetName(str:string);
              Procedure SetTimer(val:integer);
              Function GetName:string;
	      Function GetTimer:integer;
              Procedure SetScene(Trec:TSrec);
              Function GetScene:TSrec;

 	private
	      Tscenerec:TSrec;

        public
	      property Name:string read GetName Write SetName;
              property Timer:integer read GetTimer write SetTimer;
              property Scene:Tsrec read GetScene write SetScene;

           { public declarations }
        end;


   implementation

// ******************************************************************************
// ********************** definition materiel ************************************
// ******************************************************************************

Constructor TdmxUnit.Create;
	    var x:byte;
begin
     Unitrec.Unbrvoie:=0;
     Unitrec.Ustartadr:=0;
     Unitrec.Unom:='';
     Unitrec.UType:='';
     for x:=0 to 15 do
          Unitrec.UVoie[x]:='';
     Setlength(Effect,1);           //creation tableau dynamique
     Effect[0].Enom:='BlackOut';    //creation par defaut indice 0=blackout;
     for x:=0 to 15 do
           Effect[0].Erec[x]:=0;
     Effet:=0;                     //fixation effet 0
end;

Destructor TdmxUnit.Destroy;
begin
     SetLength(Effect,0);
end;

Procedure TdmxUnit.SetName(str:string);
begin
    if (str <> '') then
        Unitrec.UNom:=str;
end;

Procedure TdmxUnit.SetType(str:string);
begin
     if (str<>'') then
     	Unitrec.Utype:=str;
end;

Procedure TdmxUnit.SetNbrVoie(nbr:byte);
begin
     If (nbr>0) and (nbr<17) then
         Unitrec.UNbrVoie:=nbr;
end;

Procedure TdmxUnit.SetNameVoie(num:byte;str:string);
begin
     if (num >= 0) and (num <= 15) then
     	Unitrec.Uvoie[num]:=str;
end;

Procedure TdmxUnit.SetStartAdr(start:integer);
begin
     if (start>0) and (start<512) then
     	Unitrec.UstartAdr:=start;
end;

Function TdmxUnit.GetName:string;
begin
     GetName:=Unitrec.Unom;
end;

Function TdmxUnit.GetType:string;
begin
     GetType:=Unitrec.Utype;
end;

Function  Tdmxunit.GetNameVoie(num:byte):string;
begin
     GetNameVoie:=Unitrec.Uvoie[num];
end;

Function  TdmxUnit.GetNbrVoie:byte;
begin
     GetNbrVoie:=Unitrec.Unbrvoie;
end;

Function  TdmxUnit.GetStartAdr:integer;
begin
     GetStartAdr:=Unitrec.Ustartadr;
end;

procedure TdmxUnit.AddEffect(Efrec:TEffect);
	  var h:integer;
begin
     h:=High(Effect);
     inc(h,2);
     Setlength(Effect,h);                              	 //ajoute un element au tableau
     Effect[High(Effect)].Enom:=Efrec.Enom;
     Effect[High(Effect)].Erec:=Efrec.Erec;
end;

function  TdmxUnit.GetNbrEffect:integer;
begin
     GetNbrEffect:=high(Effect)+1;                        // nombre effets (maxindex tableau+1)
end;

procedure TdmxUnit.SetEffect(num:integer);                //ecriture effet ipc
	  var x,y:integer;
begin
     if (num >= Low(Effect)) and (num  <= high(Effect))  then  //securite
     begin
     	  IndexEffect:=num;
          x:=Unitrec.Ustartadr-1;
          for y:=0 to Unitrec.Unbrvoie-1 do
	      Dmx_Write_data(x+y,Effect[num].Erec[y]);
     end;
end;

procedure TdmxUnit.ModEffect(num:integer;Efrec:Teffect);

begin
     if (num >= Low(Effect)) and (num  <= high(Effect))  then  //securite
     begin
	  Effect[num].Erec:=Efrec.Erec;
	  Effect[num].Enom:=Efrec.Enom;
	  SetEffect(num);
     end;
end;

procedure Tdmxunit.DelEffect(num:integer);
	  var x:integer;
begin
     if (num  <= high(Effect)) and  (num >= Low(Effect)) then  //securite
     begin
     	  for x:=num to high(Effect)-1 do
              Effect[x]:=Effect[x+1];
          SetLength(Effect,High(Effect)-1);  		       //diminue taille tableau
     end;
end;

Function TdmxUnit.GetEffect:integer;
begin
     GetEffect:=IndexEffect;
end;

Function  Tdmxunit.GetRecUnit:TUrec;  		  	   //car record zone private
begin
     GetRecUnit:=UnitRec;
end;

Function  TdmxUnit.GetEffectUnit(num:integer):TEffect;   //car effet en zone private
begin
      	 GetEffectUnit:=Effect[num];
end;

procedure Tdmxunit.SetRecUnit(RecUnit:Turec);	  	 //pour load
begin
     UnitRec:=RecUnit;
end;

Function  Tdmxunit.GetLow:integer;			      //indice bas tableau
begin
     Getlow:=low(Effect);
end;

Function  Tdmxunit.GetHigh:integer;                           //indice haut tableau dynamique
begin
     GetHigh:=high(Effect);
end;

// ******************************************************************************
// ********************** definition trames *************************************
// ******************************************************************************

Constructor TdmxScene.Create;
begin
            Tscenerec.Snom:='';
	    Tscenerec.Stimer:=0;
end;

Destructor TdmxScene.Destroy;
begin
end;

Procedure TdmxScene.SetName(str:string);
begin
     if str<>'' then
     	Tscenerec.Snom:=str;
end;

Procedure TdmxScene.SetTimer(val:integer);
begin
     if (val>0) and (val<30000) then
     	Tscenerec.Stimer:=val;
end;

Function TdmxScene.GetName:string;
begin
     GetName:=Tscenerec.Snom;
end;

Function TdmxScene.GetTimer:integer;
begin
     GetTimer:=Tscenerec.Stimer;
end;

Procedure TdmxScene.SetScene(Trec:TSrec);
begin
     TsceneRec:=TRec;
end;

Function TdmxScene.GetScene:TSrec;
begin
     GetScene:=TSceneRec;
end;

Initialization;

end.
