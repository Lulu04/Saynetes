{  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as
  published by the Free Software Foundation; either version 3 of
  the License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Lesser General Public License for more details. }

unit Geffet; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, DmxUtils, Buttons;

type

  { TGestEffet }

  TGestEffet = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ListBox1: TListBox;
    TrackBar1: TTrackBar;
    TrackBar10: TTrackBar;
    TrackBar11: TTrackBar;
    TrackBar12: TTrackBar;
    TrackBar13: TTrackBar;
    TrackBar14: TTrackBar;
    TrackBar15: TTrackBar;
    TrackBar16: TTrackBar;
    TrackBar2: TTrackBar;
    TrackBar3: TTrackBar;
    TrackBar4: TTrackBar;
    TrackBar5: TTrackBar;
    TrackBar6: TTrackBar;
    TrackBar7: TTrackBar;
    TrackBar8: TTrackBar;
    TrackBar9: TTrackBar;
    procedure Button3Click(Sender: TObject);
    procedure Init;
    procedure FormShow(Sender: TObject);
    procedure ListBox1SelectionChange(Sender: TObject; User: boolean);
    procedure ChangeEffet(num:integer;valeur:byte);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure TrackBar3Change(Sender: TObject);
    procedure TrackBar4Change(Sender: TObject);
    procedure TrackBar5Change(Sender: TObject);
    procedure TrackBar6Change(Sender: TObject);
    procedure TrackBar7Change(Sender: TObject);
    procedure TrackBar8Change(Sender: TObject);
    procedure TrackBar9Change(Sender: TObject);
    procedure TrackBar10Change(Sender: TObject);
    procedure TrackBar11Change(Sender: TObject);
    procedure TrackBar12Change(Sender: TObject);
    procedure TrackBar13Change(Sender: TObject);
    procedure TrackBar14Change(Sender: TObject);
    procedure TrackBar15Change(Sender: TObject);
    procedure TrackBar16Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);                                //ajout effet
    procedure Button2Click(Sender: TObject);                                //supp effet

  private
    TabLab:array[0..15] of Tlabel;
    TabTrack:array[0..15] of TTrackBar;
    { private declarations }
  public
    { public declarations }
  end; 

var
  GestEffet: TGestEffet;

implementation
	Uses Main;
{ TGestEffet }
procedure TGestEffet.init;
	  var x:integer;
     	      Punit:Ptrunit;
	      effetencours:integer;
begin
     //
     TabLab[0]:=Label1;
     TabLab[1]:=Label2;
     TabLab[2]:=Label3;
     TabLab[3]:=Label4;
     TabLab[4]:=Label5;
     TabLab[5]:=Label6;
     TabLab[6]:=Label7;
     TabLab[7]:=Label8;
     TabLab[8]:=Label9;
     TabLab[9]:=Label10;
     TabLab[10]:=Label11;
     TabLab[11]:=Label12;
     TabLab[12]:=Label13;
     TabLab[13]:=Label14;
     TabLab[14]:=Label15;
     TabLab[15]:=Label16;
     //
     TabTrack[0]:=TrackBar1;
     TabTrack[1]:=TrackBar2;
     TabTrack[2]:=TrackBar3;
     TabTrack[3]:=TrackBar4;
     TabTrack[4]:=TrackBar5;
     TabTrack[5]:=TrackBar6;
     TabTrack[6]:=TrackBar7;
     TabTrack[7]:=TrackBar8;
     TabTrack[8]:=TrackBar9;
     TabTrack[9]:=TrackBar10;
     TabTrack[10]:=TrackBar11;
     TabTrack[11]:=TrackBar12;
     TabTrack[12]:=TrackBar13;
     TabTrack[13]:=TrackBar14;
     TabTrack[14]:=TrackBar15;
     TabTrack[15]:=TrackBar16;
     //
     for x:=0 to 15 do
     begin
     	  TabLab[x].Caption:='';
	  TabLab[x].Enabled:=false;
	  TabTrack[x].Enabled:=false;
	  TabTrack[x].Position:=0;
     end;
     ListBox1.Clear;		       	//vide la listbox
     Punit:=Univers.GetPtrUnit(Univers.GetSelectedUnit);
     effetencours:=Punit^.Effet;
     //

     for x:=Punit^.GetLow to PUnit^.GetHigh do
        ListBox1.Items.Add(Punit^.GetEffectUnit(x).Enom);  //affiche effet disponibles
     //
     //showmessage('Effet en cours= '+intToStr(effetencours));
     ListBox1.ItemIndex:=Effetencours;

     for x:=0 to Punit^.NbrVoie-1 do
     begin
       	 TabTrack[x].Position:=Punit^.GetEffectUnit(Effetencours).Erec[x];
     	 TabTrack[x].Enabled:=true;
         TabLab[x].Enabled:=true;
         Tablab[x].Caption:=Punit^.GetNameVoie(x);
     end;

     GroupBox1.Caption:='Gestion '+Punit^.Name;
end;

procedure TGestEffet.Button3Click(Sender: TObject);                         //ok
begin
     	  Close;
end;

procedure TGestEffet.Button1Click(Sender: TObject);                   	     //ajout effet
	  var userstring:string;
	      Efrec:TEffect;
	      Punit:PtrUnit;
	      x:integer;
begin
     InputQuery ('Saisie du nom effet','', userstring);
     if userstring<>'' then
     begin
     	  Punit:=univers.GetPtrUnit(univers.GetselectedUnit);
     	  Efrec.Enom:=userstring;                                          //sauve nom effet
          for x:=0 to Punit^.NbrVoie-1 do                                  //et position trackbar
              Efrec.Erec[x]:=TabTrack[x].Position;
          Punit^.AddEffect(Efrec);
          listBox1.clear;
          for x:=Punit^.GetLow to Punit^.GetHigh do
	          ListBox1.Items.Add(Punit^.GetEffectUnit(x).Enom);  	   //affiche effet disponibles

          ListBox1.ItemIndex:=Punit^.GetHigh;
     end
     else Listbox1.ItemIndex:=0;
end;

procedure TGestEffet.Button2Click(Sender: TObject);                       //effacement effet
	  var x:integer;
	      Punit:PtrUnit;
begin
      Punit:=Univers.GetPtrUnit(Univers.GetSelectedUnit);
      x:=ListBox1.ItemIndex;		  				  //Blackout non effacable
      if x>0 then
      begin
          Punit^.DelEffect(x);
	  ListBox1.Items.Delete(x);
	  if x < ListBox1.Items.Count-1 then dec(x);
          ListBox1.ItemIndex:=x;   					  //selection
      end;
end;

procedure TGestEffet.FormShow(Sender: TObject);
begin
     Init;
end;

procedure TGestEffet.ListBox1SelectionChange(Sender: TObject; User: boolean);
	  var x,y:integer;
	      efrec:Teffect;
	      Punit:PtrUnit;
begin
     y:=ListBox1.ItemIndex;
     Punit:=Univers.GetPtrUnit(Univers.GetSelectedUnit);
     if (y>=Punit^.GetLow) and (y<=Punit^.GetHigh) then
     begin
     	  efrec:=Punit^.GetEffectUnit(y);
     	  for x:=0 to Punit^.NbrVoie-1 do                                          //position trackbar
     	      TabTrack[x].Position:=Efrec.Erec[x];
     	  Punit^.SetEffect(y);
     end;

end;

procedure TGestEffet.ChangeEffet(num:integer;valeur:byte);
	  var PUnit:PtrUnit;
	      Efrec:Teffect;
begin
          if ListBox1.Itemindex>0 then			  		//pas de modif index 0 blackout
          begin
               Punit:=Univers.GetptrUnit(Univers.GetSelectedUnit);
               Efrec:=Punit^.GetEffectUnit(ListBox1.ItemIndex);
	       Efrec.Erec[num]:=valeur;
	       Punit^.ModEffect(ListBox1.ItemIndex,Efrec);
          end;
end;

procedure TGestEffet.TrackBar1Change(Sender: TObject);
begin
     if Trackbar1.Enabled then
          ChangeEffet(0,TrackBar1.Position);
end;

procedure TGestEffet.TrackBar2Change(Sender: TObject);
begin
     if Trackbar2.Enabled then
          ChangeEffet(1,TrackBar2.Position);
end;

procedure TGestEffet.TrackBar3Change(Sender: TObject);
begin
     if Trackbar3.Enabled then
          ChangeEffet(2,TrackBar3.Position);
end;

procedure TGestEffet.TrackBar4Change(Sender: TObject);
begin
     if Trackbar4.Enabled then
          ChangeEffet(3,TrackBar4.Position);
end;

procedure TGestEffet.TrackBar5Change(Sender: TObject);
begin
     if Trackbar5.Enabled then
          ChangeEffet(4,TrackBar5.Position);
end;

procedure TGestEffet.TrackBar6Change(Sender: TObject);
begin
     if Trackbar6.Enabled then
          ChangeEffet(5,TrackBar6.Position);
end;

procedure TGestEffet.TrackBar7Change(Sender: TObject);
begin
     if Trackbar7.Enabled then
          ChangeEffet(6,TrackBar7.Position);
end;

procedure TGestEffet.TrackBar8Change(Sender: TObject);
begin
     if Trackbar8.Enabled then
          ChangeEffet(7,TrackBar8.Position);
end;

procedure TGestEffet.TrackBar9Change(Sender: TObject);
begin
     if Trackbar9.Enabled then
          ChangeEffet(8,TrackBar9.Position);
end;

procedure TGestEffet.TrackBar10Change(Sender: TObject);
begin
     if Trackbar10.Enabled then
          ChangeEffet(9,TrackBar10.Position);
end;

procedure TGestEffet.TrackBar11Change(Sender: TObject);
begin
     if Trackbar11.Enabled then
          ChangeEffet(10,TrackBar11.Position);
end;

procedure TGestEffet.TrackBar12Change(Sender: TObject);
begin
     if Trackbar12.Enabled then
          ChangeEffet(11,TrackBar12.Position);
end;

procedure TGestEffet.TrackBar13Change(Sender: TObject);
begin
     if Trackbar13.Enabled then
          ChangeEffet(12,TrackBar13.Position);
end;

procedure TGestEffet.TrackBar14Change(Sender: TObject);
begin
     if Trackbar14.Enabled then
          ChangeEffet(13,TrackBar14.Position);
end;

procedure TGestEffet.TrackBar15Change(Sender: TObject);
begin
     if Trackbar15.Enabled then
          ChangeEffet(14,TrackBar15.Position);
end;

procedure TGestEffet.TrackBar16Change(Sender: TObject);
begin
     if Trackbar16.Enabled then
          ChangeEffet(15,TrackBar16.Position);
end;

initialization
  {$I geffet.lrs}

end.

