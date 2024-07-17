unit form_viewlog;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons,
  LCL_Utils;

type

  { TFormViewLogFile }

  TFormViewLogFile = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    Panel1: TPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    BSendLogByMail: TSpeedButton;
    BOpenLogFolder: TSpeedButton;
    procedure BOpenLogFolderClick(Sender: TObject);
    procedure BSendLogByMailClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure RadioButton1Change(Sender: TObject);
  private
    CheckedLabelManager: TCheckedLabelManager;
    function GetSelectedLogFile: string;
    procedure UpdateMemoContent;
  public

  end;


implementation
uses LCLType, u_web, u_apputils, u_userdialogs, u_resource_string, u_logfile,
  LCLIntf;

{$R *.lfm}

{ TFormViewLogFile }

procedure TFormViewLogFile.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then Close;
end;

procedure TFormViewLogFile.FormShow(Sender: TObject);
begin
  UpdateMemoContent;
end;

procedure TFormViewLogFile.RadioButton1Change(Sender: TObject);
begin
  UpdateMemoContent;
end;

function TFormViewLogFile.GetSelectedLogFile: string;
begin
  if RadioButton1.Checked then result := Log.GetContentAsString //; GetUserConfigFolder + 'saynetes.log'
    else Result := GetUserConfigFolder + 'saynetes_previous_session.log';
end;

procedure TFormViewLogFile.UpdateMemoContent;
begin
  Memo1.Clear;
  if RadioButton1.Checked then Memo1.Lines.AddText(Log.GetContentAsString)
    else Memo1.Lines.LoadFromFile(GetUserConfigFolder + 'saynetes_previous_session.log');
end;

procedure TFormViewLogFile.FormCreate(Sender: TObject);
begin
  CheckedLabelManager := TCheckedLabelManager.Create;
  CheckedLabelManager.CaptureLabelsClick([Label1, Label2]);
end;

procedure TFormViewLogFile.BSendLogByMailClick(Sender: TObject);
var f: string;
  res: Boolean;
begin
  if Memo1.Lines.Count = 0 then exit;

  Screen.BeginWaitCursor;
  try
    res := SendMail(Memo1.Lines.Text, []);
  finally
    Screen.EndWaitCursor;
  end;
  if res then ShowMess(SMailSentSuccessfully, SOk, mtInformation)
    else ShowMess(SFailToSendMail, SOk, mtError);
end;

procedure TFormViewLogFile.BOpenLogFolderClick(Sender: TObject);
begin
  OpenDocument(GetUserConfigFolder);
end;

procedure TFormViewLogFile.FormDestroy(Sender: TObject);
begin
  CheckedLabelManager.Free;
end;

end.

