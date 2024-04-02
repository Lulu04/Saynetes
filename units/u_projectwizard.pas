unit u_projectwizard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, ExtCtrls,
  StdCtrls, EditBtn, LCLTranslator, frame_editstring;

type

  { TFormProjectWizard }

  TFormProjectWizard = class(TForm)
    B_Cancel: TSpeedButton;
    B_Next: TSpeedButton;
    B_Previous: TSpeedButton;
    DirectoryEdit1: TDirectoryEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    NB1: TNotebook;
    PageFinish: TPage;
    PageProjectName: TPage;
    PageWelcome: TPage;
    Panel1: TPanel;
    StaticText1: TStaticText;
    procedure B_CancelClick(Sender: TObject);
    procedure B_NextClick(Sender: TObject);
    procedure B_PreviousClick(Sender: TObject);
    procedure DirectoryEdit1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    FrameEditString1: TFrameEditString;
    function GetProjectName: string;
    function GetProjectPath: string;
    procedure UpdateButton;
    function ProjectNameIsValid: boolean;
    procedure ProcessTextChange(Sender: TObject);
  public
    property ProjectPath: string read GetProjectPath;
    property ProjectName: string read GetProjectName;
  end;

var
  FormProjectWizard: TFormProjectWizard;

implementation

uses u_resource_string, u_common, u_utils, LazFileUtils,
  LCLType;

{$R *.lfm}

{ TFormProjectWizard }

procedure TFormProjectWizard.B_NextClick(Sender: TObject);
begin
  if NB1.PageIndex=NB1.PageCount-1 then begin
    ModalResult := mrOk;
    exit;
  end else begin
    NB1.PageIndex:=NB1.PageIndex+1;
    UpdateButton;
  end;
end;

procedure TFormProjectWizard.B_PreviousClick(Sender: TObject);
begin
  if NB1.PageIndex>0
    then NB1.PageIndex:=NB1.PageIndex-1;
  UpdateButton;
end;

procedure TFormProjectWizard.B_CancelClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TFormProjectWizard.DirectoryEdit1Change(Sender: TObject);
begin
  UpdateButton;
end;

procedure TFormProjectWizard.FormCreate(Sender: TObject);
begin
  FrameEditString1:=TFrameEditString.Create(Self);
  FrameEditString1.Parent:=Panel1;
  FrameEditString1.Align:=alClient;
  FrameEditString1.OnTextChange:=@ProcessTextChange;
  FrameEditString1.ModeFileName;
  FrameEditString1.Title:=SProjectName;
end;

procedure TFormProjectWizard.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_ESCAPE then ModalResult:=mrCancel;
end;

procedure TFormProjectWizard.FormShow(Sender: TObject);
begin
  NB1.PageIndex:=0;
  FrameEditString1.Text:='';
  UpdateButton;

  B_Cancel.Caption := SCancel;
  B_Next.Caption := SNext;
  B_Previous.Caption := SPrevious;
end;

function TFormProjectWizard.GetProjectName: string;
begin
  Result:=FrameEditString1.Text;
end;

function TFormProjectWizard.GetProjectPath: string;
begin
  Result:=DirectoryEdit1.Directory;
end;

procedure TFormProjectWizard.UpdateButton;
begin
  B_Previous.Enabled := NB1.PageIndex>0;
  if NB1.PageIndex=(NB1.PageCount-1)
    then B_Next.Caption:=SFinish
    else B_Next.Caption:=SNext;

  case NB1.PageIndex of
   0: B_Next.Enabled:=TRUE;
   1: B_Next.Enabled:=ProjectNameIsValid and DirectoryExistsUTF8(DirectoryEdit1.Directory);
   2: ModalResult := mrOk;
  end;
end;

function TFormProjectWizard.ProjectNameIsValid: boolean;
begin
  Result:=(FrameEditString1.Text<>'') and StringIsValid(FrameEditString1.Text);
end;

procedure TFormProjectWizard.ProcessTextChange(Sender: TObject);
begin
  UpdateButton;
end;

end.

