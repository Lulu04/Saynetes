unit u_startupwizard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls,
  ExtCtrls;

type

  { TFormStartUpWizard }

  TFormStartUpWizard = class(TForm)
    CB1: TComboBox;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    Shape1: TShape;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    procedure CB1Select(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
  private
    FOpenRecent: string;
    FNew,
    FOpen,
    FQuit: boolean;
  public
    property OpenRecent: string read FOpenRecent;
    property NewProject: boolean read FNew;
    property OpenProject: boolean read FOpen;
    property Quit: boolean read FQuit;
  end;

//var
 // FormStartUpWizard: TFormStartUpWizard;

implementation
uses LCLType, u_userdialogs, u_resource_string,
  u_program_options;

{$R *.lfm}

{ TFormStartUpWizard }

procedure TFormStartUpWizard.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key=VK_ESCAPE then ModalResult:=mrCancel;
end;

procedure TFormStartUpWizard.CB1Select(Sender: TObject);
begin
  if CB1.ItemIndex=-1 then exit;

  FOpenRecent:=CB1.Items.Strings[CB1.ItemIndex];
  if not FileExists(FOpenRecent) then begin
    ShowMess(SFile+Lineending+FOpenRecent+Lineending+SNotFound, SOk, mtError);
    ProgramOptions.RemoveProjectNameFromRecentList(FOpenRecent);
    FOpenRecent:='';
    CB1.Items.Delete(CB1.ItemIndex);
  end else ModalResult:=mrOk;
end;

procedure TFormStartUpWizard.FormShow(Sender: TObject);
var i: integer;
begin
  CB1.Clear;
  for i:=0 to High(ProgramOptions.RecentProjects) do
    if ProgramOptions.RecentProjects[i]<>''
      then CB1.Items.Add(ProgramOptions.RecentProjects[i]);
end;

procedure TFormStartUpWizard.SpeedButton1Click(Sender: TObject);
begin
  FNew:=TRUE;
  ModalResult:=mrOk;
end;

procedure TFormStartUpWizard.SpeedButton2Click(Sender: TObject);
begin
  FOpen:=TRUE;
  ModalResult:=mrOk;
end;

procedure TFormStartUpWizard.SpeedButton3Click(Sender: TObject);
begin
  FQuit:=TRUE;
  ModalResult:=mrOk;
end;

end.

