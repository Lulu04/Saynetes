{  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as
  published by the Free Software Foundation; either version 3 of
  the License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Lesser General Public License for more details. }


unit Main; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, Menus, DmxUtils, Modules, Buttons;

type

  { TUnivers }

  TUnivers = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    ListView1: TListView;
    ListView2: TListView;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    StatusBar1: TStatusBar;

    // ************ gestion commune ***************
    Procedure SetFileName(str:string);
    Procedure Save;                                         //sauve univers
    Procedure Load;                                         //charge univers
    procedure InfoStatus;                                   //information statusbar
    Function  StrToNum(str:string):longint;                 //conversion securisee chaine vers longint

    // ************ gestion Unite Dmx *************
    procedure RecalcUnit;   	    		   	    //recalcul adresse dmx
    procedure SupprimUnit;
    Function  GetptrUnit(idx:integer):PtrUnit;
    Function  GetSelectedUnit:integer;

    // ************ gestion Scenes   ***************
    procedure AddScene;
    procedure ModScene;
    procedure DelScene;
    Function GetSelectedScene:longint;	   	            //renvoi index trame selected
    Function GetPtrScene(idx:longint):PtrScene;
    procedure Recalcscene;

    // ************* gestion Evenements ************
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure MenuItem2Click(Sender: TObject);              //load
    procedure MenuItem3Click(Sender: TObject);              //save
    procedure MenuItem4Click(Sender: TObject);              //quit
    procedure MenuItem6Click(Sender: TObject);              //aide
    procedure MenuItem8Click(Sender: TObject);              //dechargement module et arret
    procedure MenuItem9Click(Sender: TObject);              //a propos
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure ListView2Change(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure ListView2DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ListView2DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);

  private
     Filename:string;
    { private declarations }
  public
    { public declarations }
  end; 

var
  Univers: TUnivers;
  it:TlistItem;

implementation
     uses Geffet, Gmat;

// ***************************** Gestion generale **********************************

Function  Tunivers.StrToNum(str:string):longint;
	  var val:longint;
begin
     try
     	val:=StrToInt(str);
     Except
     	  ShowMessage('Erreur dans la saisie de la valeur '+str);
          val:=0;
     	  raise;
     end;
     if val<0 then
     begin
     	  ShowMessage('Valeur toujours superieure a 0 !!');
          val:=0;
     end;
     StrToNum:=val;
end;

Procedure TUnivers.SetFileName(str:string);
begin
   if str<>'' then Filename:=str;
end;

procedure TUnivers.MenuItem8Click(Sender: TObject);                  //dechargement module k8062
begin
     Dmx_stop;
     close;
end;

procedure TUnivers.MenuItem6Click(Sender: TObject);                  //aide
begin
     //en cours ....
end;

procedure TUnivers.MenuItem9Click(Sender: TObject);                  // a propos
begin
     showmessage('Dmxcontrol pour module k8062'+chr(13)+'      Vers-0.2 - 2010');
end;

Procedure TUnivers.Save;
          var f:longint;
	      Punit:PtrUnit;
              Pscene:PtrScene;
              r,x,y,nbrunit,nbrtrame:longint;
	      NbrEffet:integer;
begin
     if filename<>'' then
     begin
        nbrunit:=ListView1.Items.count;
        nbrtrame:=ListView2.Items.Count;
	//
        f:=FileCreate(filename);                                    //creation fichier
        r:=filewrite(f,nbrunit,Sizeof(Longint));                    //sauvegarde nombre record
        r:=Filewrite(f,nbrtrame,Sizeof(longint));                   //puis nombre de trames
        //
        if nbrunit>0 then                                           //si quelque chose a sauver
        begin;
          for x:=0 to nbrunit-1 do                                  //puis les ecrits
          begin;
             //
             Punit:=ListView1.Items.Item[x].data;
             //
             r:=Filewrite(f,Punit^.GetRecUnit,sizeof(TUrec));
             //sauve caracteristique
             //
             NbrEffet:=Punit^.GetNbrEffect-1;
	     //ShowMessage('Sauvegarde effet = '+InttoStr(NbrEffet));
             r:=Filewrite(f,NbrEffet,Sizeof(integer));                        //ecrit nombre effet
	     for y:=Punit^.GetLow to Punit^.GetHigh do                        //puis les effets
             	 r:=Filewrite(f,Punit^.GetEffectUnit(y),Sizeof(Teffect));
          end;
        end;
        //
        if nbrtrame>0 then
        begin
           for x:=0 to nbrtrame-1 do                          		      //puis les ecrits
           begin;
               //
               PScene:=ListView2.Items.Item[x].data;
                //
               r:=filewrite(f,Pscene^.GetScene,sizeof(Pscene^.GetScene));     //sauve les scenes
           end;
        end;
	//
        fileclose(f);
     end;
end;

Procedure TUnivers.Load;
          var f:longint;
	      Punit:PtrUnit;
              PScene:PtrScene;
              Runit:TUrec;
              RScene:TSrec;
              Reffect:TEffect;
              r,x,y,nbrunit,nbrtrame:longint;
              nbreffet:integer;
	      it:Tlistitem;
begin
     If filename<>'' then
     begin
          if FileExists(filename) then
          begin
               f:=FileOpen(filename,fmOpenread);             //ouverture fichier
               r:=FileRead(f,nbrunit,sizeof(longint));       //lit d'abord nombre unite
               r:=FileRead(f,nbrtrame,sizeof(longint));      //puis nombre trames
               if nbrunit>0 then
               begin
                    for x:=0 to nbrunit-1 do
                    begin
                         r:=Fileread(f,Runit,sizeof(Runit));
                         Punit:=new(Ptrunit,create);
                         Punit^.SetRecUnit(Runit);  	     //sauve record dans unit
                         //
                         it:=ListView1.Items.add;
                         it.Data:=Punit;
                         it.Caption:=IntToStr(Punit^.DmxAdr);
                         it.SubItems.Text:=PUnit^.Name;
                         r:=Fileread(f,Nbreffet,sizeof(integer));   //lit nombre effets
			 //Showmessage('Restauration effets= '+InttoStr(Nbreffet));
    			 for y:=0 to Nbreffet do  		    //effet 0 intouchable
                         begin
			     r:=Fileread(f,Reffect,Sizeof(Reffect)); //puis les effets
                             if y>0 then Punit^.AddEffect(Reffect);  //inutile car 0 deja cree
                         end;
                    end;
                    RecalcUnit;
                    ListView1.Items.Item[0].Selected:=true ;
               end;
               if nbrtrame>0 then
               begin
                    for x:=0 to nbrtrame-1 do
                    begin
                         r:=Fileread(f,Rscene,sizeof(Rscene));
                         PScene:=new(PtrScene,create);
			 //
                         Pscene^.Scene:=Rscene;
                         it:=ListView2.Items.Add;
                         it.data:=PScene;
			 It.Caption:=IntToStr(x);
			 It.SubItems.Text:=Rscene.Snom;
			 //
                    end;
                    ListView2.Items.Item[0].Selected:=true ;
               end;
               InfoStatus;
               fileclose(f);
          end;
     end;
end;

procedure TUnivers.InfoStatus;
begin
     //en attente
end;

// ***************************** Gestion materiel **********************************

procedure Tunivers.RecalcUnit;	                              //recalcule startadr suite modif unit
	  var x,y:integer;
      	      it:Tlistitem;
	      Punit:PtrUnit;
begin
      y:=1;
      for x:=0 to ListView1.Items.count-1 do
      begin
      	   Punit:=GetPtrUnit(x);
           Punit^.DmxAdr:=y;
           y:=y+Punit^.NbrVoie;
           //
           it:=ListView1.Items.Item[x];
	   it.Caption:=IntToStr(Punit^.DmxAdr);
      end;
      Dmx_MaxChannel(y);                          //unit k8062
end;

Function Tunivers.GetptrUnit(idx:integer):PtrUnit;     	      //renvoi adresse unite avec index
begin
     if (idx>=0) and (idx<ListView1.Items.Count) then
	 GetPtrunit:=ListView1.Items.Item[idx].Data
     else
     	 GetPtrUnit:=Nil;                                     //pas trouve
end;

Function Tunivers.GetSelectedUnit:integer; 	      	      //renvoi index unit selected
	 var x:integer;
begin
     GetselectedUnit:=-1;			 	      //pas trouvé
     for x:=0 to ListView1.Items.Count-1 do
     	 if Listview1.Items.Item[x].Selected then
       	    GetSelectedUnit:=x;
end;

Procedure Tunivers.SupprimUnit;        	                      //suppression unite
	  var x:integer;
	      Punit:Ptrunit;
begin
     x:=GetSelectedUnit;
     if x>=0 then
     begin
     	  Punit:=ListView1.Items.Item[x].Data;
	  dispose(Punit,Destroy);		      		      //suppression noeud
	  ListView1.Items.Item[x].Delete;			      //autodestruction
	  Recalcunit;						      //recalcul unite
     end;
end;

// ***************************** Gestion Scenes    **********************************

Function Tunivers.GetPtrScene(idx:longint):PtrScene;          //renvoi adresse trame avec index
begin
     if (idx>=0) and (idx<ListView2.Items.Count) then
	 GetPtrScene:=ListView2.Items.Item[idx].Data
     else
     	 GetPtrScene:=Nil;
end;

Function Tunivers.GetSelectedScene:longint;		      // renvoi index trame selected
	 var x:longint;
begin
     GetselectedScene:=-1;				      //pas trouvé
     for x:=0 to ListView2.Items.Count-1 do
     	 if Listview2.Items.Item[x].Selected then
       	    GetSelectedScene:=x;
end;

procedure Tunivers.AddScene;
	  var PScene:PtrScene;
       	      Punit:PtrUnit;
              Srec:Tsrec;
              userstring:string;
	      x:longint;
begin
     InputQuery ('Saisie du nom scene','', userstring);
     if Userstring<>'' then
     begin
     	  Srec.Snom:=userstring;
	  for x:=0 to (ListView1.Items.Count-1) do
          begin
	       Punit:=GetPtrUnit(x);
	       Srec.Sscene[x]:=Punit^.Effet;               //sauve valeur effet position unit
          end;
	  //
     	  Pscene:=new(PTrScene,create);
          Pscene^.SetScene(Srec);
          //
   	  it:=ListView2.Items.Add;
          it.Data:=Pscene;
          it.Caption:=IntToStr(ListView2.Items.Count);
	  it.SubItems.Text:=Pscene^.Name;
	  it.Selected:=true;
     end;
end;

procedure Tunivers.ModScene;
	  var PScene:PtrScene;
       	      Punit:PtrUnit;
              Srec:Tsrec;
              userstring:string;
	      x:longint;
begin
     Pscene:=GetPtrScene(GetselectedScene);
     Srec:=Pscene^.GetScene;
     UserString:=Srec.Snom;
     InputQuery ('Modification du nom scene','', userstring);
     if Userstring<>'' then
     begin
     	  Srec.Snom:=userstring;
	  for x:=0 to (ListView1.Items.Count-1) do
          begin
	       Punit:=GetPtrUnit(x);
	       Srec.Sscene[x]:=Punit^.Effet;               //sauve valeur effet position unit
          end;
	  //
          Pscene^.SetScene(Srec);
          //
   	  it:=ListView2.Items.Item[GetSelectedScene];
	  it.SubItems.Text:=Pscene^.Name;
	  it.Selected:=true;
     end;
end;

procedure Tunivers.DelScene;
	  var Pscene:PtrScene;
begin
     Pscene:=GetPtrScene(GetSelectedScene);
     If Pscene<>nil then
     begin
     	  Dispose(PScene,destroy);
	  ListView2.Items.Item[GetSelectedScene].Delete;
     end;
end;

procedure Tunivers.Recalcscene;
	  var x:longint;
	      it:TlistItem;
begin
     for x:=0 to ListView2.Items.Count-1  do
     begin
     	  it:=ListView2.Items.Item[x];
	  it.Caption:=IntToStr(x);
     end;
end;
// **************************** Gestion evenements **********************************

procedure TUnivers.FormClose(Sender: TObject; var CloseAction: TCloseAction);
	  var x:longint;
              Punit:PtrUnit;
	      PScene:PTrScene;
begin
     // sauvegarde si possible
     Save;
     Dmx_AllZero;
     //
     for x:=0 to ListView1.Items.Count-1 do
     begin
          Punit:=ListView1.Items.Item[x].Data;
	  Dispose(Punit,destroy);
     end;
     for x:=0 to ListView2.Items.Count-1 do
     begin
          PScene:=ListView2.Items.Item[x].Data;
	  Dispose(PScene,destroy);
     end;
end;

procedure TUnivers.MenuItem2Click(Sender: TObject);             //Menu Charger
begin
        if OpenDialog1.Execute then
   	begin;
     	      SetFileName(OpenDialog1.FileName);
     	      load;
        end;
end;

procedure TUnivers.MenuItem3Click(Sender: TObject);             //Menu Sauver
begin
        if SaveDialog1.Execute then
   	begin;
     	      SetFileName(saveDialog1.FileName);
     	      save;
        end;
end;

procedure TUnivers.MenuItem4Click(Sender: TObject);             //Menu quitter
begin
    close;
end;

procedure TUnivers.Button1Click(Sender: TObject);               //Ajout materiel
begin
       GestMat.Init(-1);
       GestMat.show;
end;

procedure TUnivers.Button2Click(Sender: TObject);               //Modif Materiel
begin
       GestMat.Init(GetSelectedUnit);
       GestMat.Show;
end;

procedure TUnivers.Button3Click(Sender: TObject);               //sup materiel
begin
       SupprimUnit;
end;

procedure TUnivers.Button4Click(Sender: TObject);    		//ajout scene
begin
       Addscene;
end;

procedure TUnivers.Button5Click(Sender: TObject);         	//modif scene
begin
      ModScene;
end;

procedure TUnivers.Button6Click(Sender: TObject);         	//supp scene
begin
      DelScene;
end;

procedure TUnivers.Button7Click(Sender: TObject);        	//effets materiel
begin
      GestEffet.Show;
end;

procedure TUnivers.ListView2Change(Sender: TObject; Item: TListItem;    //changement listview2
  Change: TItemChange);
  	  var Punit:PtrUnit;
              Pscene:PtrScene;
	      Rscene:TsRec;
              x:integer;
begin
     if ListView2.Items.Item[GetSelectedScene].Selected then
     begin
          Pscene:=GetPtrScene(GetSelectedScene);
	  Rscene:=Pscene^.GetScene;
          for x:=0 to ListView1.Items.Count-1 do
          begin
	       Punit:=GetPtrUnit(x);
	       Punit^.Effet:=Rscene.Sscene[x];
          end;
     end;
end;

procedure TUnivers.ListView2DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept:=(Sender is TListView);
end;

procedure TUnivers.ListView2DragDrop(Sender, Source: TObject; X, Y: Integer);
	  var   CursorPos: TPoint;
      	  	NewIndex: Integer;
		PScene:PtrScene;
		SelScene:longint;
		RecScene:TsRec;
		it:TlistItem;
begin
    try
         CursorPos := Point(X, Y);
         with TListView(Sender) do
         begin
            if GetItemAt(CursorPos.X,CursorPos.Y)=nil then NewIndex:=-1
            else
            begin
                 NewIndex := GetItemAt(CursorPos.X,CursorPos.Y).Index;    //position finale
            end;
            // zone de travail pour modifier listview
            //showmessage('index start = '+Inttostr(getSelectedtrame)+'  Index fin ='+inttostr(Newindex));

            SelScene:=GetSelectedScene;
            if (Newindex <> SelScene) and (NewIndex <> -1) then
            begin
                 // sauve la trame a deplacer dans dmxtrame
                 Pscene:=GetPtrscene(SelScene);
		 it:=ListView2.Items.Item[SelScene];
		 RecScene:=Pscene^.GetScene;	    	     //sauve record
                 // effacement data
                 dispose(PScene,destroy);                    //effacement data
                 it.Delete;                                  //effacement node
                 // puis insertion NewIndex
                 it:=ListView2.Items.Insert(NewIndex);
                 it.SubItems.Add('');
                 New(PScene,Create);
                 it.data:=PScene;
		 Pscene^.SetScene(RecScene);
            	 it.SubItems.Text:=PScene^.GetName;
                 it.Selected:=true;
            end;
         end;
         RecalcScene;
      Except
            Showmessage('Erreur deplacement trame');
      end;
end;

initialization
  {$I main.lrs}

end.

