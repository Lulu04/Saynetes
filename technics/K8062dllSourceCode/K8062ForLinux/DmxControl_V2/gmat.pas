{  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as
  published by the Free Software Foundation; either version 3 of
  the License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Lesser General Public License for more details. }

unit Gmat; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls;

type

  { TGestMat }

  TGestMat = class(TForm)
    Button1: TButton;
    Button2: TButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    LabeledEdit1: TLabeledEdit;
    LabeledEdit10: TLabeledEdit;
    LabeledEdit11: TLabeledEdit;
    LabeledEdit12: TLabeledEdit;
    LabeledEdit13: TLabeledEdit;
    LabeledEdit14: TLabeledEdit;
    LabeledEdit15: TLabeledEdit;
    LabeledEdit16: TLabeledEdit;
    LabeledEdit17: TLabeledEdit;
    LabeledEdit18: TLabeledEdit;
    LabeledEdit2: TLabeledEdit;
    LabeledEdit3: TLabeledEdit;
    LabeledEdit4: TLabeledEdit;
    LabeledEdit5: TLabeledEdit;
    LabeledEdit6: TLabeledEdit;
    LabeledEdit7: TLabeledEdit;
    LabeledEdit8: TLabeledEdit;
    LabeledEdit9: TLabeledEdit;
    procedure Init(index:integer);
    procedure Validation;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    Function GetValideVoie:byte;
  private
     Tabedit:array[0..15] of TlabeledEdit;
     Modif:integer;
    { private declarations }
  public
    { public declarations }
  end; 

var
  GestMat: TGestMat;

implementation
    uses DmxUtils, Main;

{ TGestMat }
procedure TGestMat.Init(index:integer);
	  var x:byte;
	      Punit:Ptrunit;
begin
     Modif:=index;   		       //transmet valeur index
     Tabedit[0]:=LabeledEdit3;
     Tabedit[1]:=LabeledEdit4;
     Tabedit[2]:=LabeledEdit5;
     Tabedit[3]:=LabeledEdit6;
     Tabedit[4]:=LabeledEdit7;
     Tabedit[5]:=LabeledEdit8;
     Tabedit[6]:=LabeledEdit9;
     Tabedit[7]:=LabeledEdit10;
     Tabedit[8]:=LabeledEdit11;
     Tabedit[9]:=LabeledEdit12;
     Tabedit[10]:=LabeledEdit13;
     Tabedit[11]:=LabeledEdit14;
     Tabedit[12]:=LabeledEdit15;
     Tabedit[13]:=LabeledEdit16;
     Tabedit[14]:=LabeledEdit17;
     Tabedit[15]:=LabeledEdit18;
     //
     LabeledEdit1.Text:='';
     LabeledEdit2.Text:='';
     //
     for x:=0 to 15 do
     	 Tabedit[x].Text:='';

     if modif >= 0 then	     	//c'est une modif;
     begin
          PUnit:=Univers.GetPtrUnit(modif);
          LabeledEdit1.Text:=Punit^.Name;
	  LabeledEdit2.Text:=Punit^.TypeUnit;
	  for x:=0 to 15 do
              Tabedit[x].Text:=Punit^.GetNameVoie(x);
     end;
end;

procedure TGestMat.Validation;
	  Var Punit:PtrUnit;
	      x:byte;
begin
     	 if modif<0 then    			  	 //c'est un ajout;
         begin
              Punit:=new(PtrUnit,Create);
    	      Punit^.Name:=LabeledEdit1.Text;
              Punit^.TypeUnit:=LabeledEdit2.Text;
              Punit^.NbrVoie:=GetValideVoie;
	      For x:=0 to 15 do
              	  Punit^.SetNameVoie(x,TabEdit[x].Text);
	      it:=Univers.ListView1.Items.Add;
     	      it.Data:=Punit;
    	      it.SubItems.Text:=Punit^.Name;
         end
         else
         begin                                           //c'est une modif
              Punit:=Univers.GetptrUnit(modif);
    	      Punit^.Name:=LabeledEdit1.Text;
              Punit^.TypeUnit:=LabeledEdit2.Text;
              Punit^.NbrVoie:=GetValideVoie;
	      For x:=0 to 15 do
                 Punit^.SetNameVoie(x,TabEdit[x].Text);
              it:=Univers.ListView1.Items.Item[modif];
              it.Data:=Punit;
    	      it.SubItems.Text:=Punit^.Name;
         end;
	 Univers.RecalcUnit;
end;


procedure TGestMat.Button1Click(Sender: TObject);              //Validation
begin
     	 Validation;
         close;
end;

procedure TGestMat.Button2Click(Sender: TObject);              //annulation
begin
     	 close;
end;

Function TGestMat.GetValideVoie:Byte;
	 var x,y:byte;
begin
     y:=0;
     for x:=0 to 15 do
         if TabEdit[x].Text <> '' then inc(y);
     GetValideVoie:=y;
end;

initialization
  {$I gmat.lrs}

end.

