unit u_userdialogs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs;


procedure ShowMess(const mess, sbutton: string; aMsgType: TMsgDlgType=mtCustom);

function AskConfirmation(const mess, syes, sno: string; aMsgType: TMsgDlgType): integer;

function UserIntegerInput(const mess, sok, scancel: string;
                           var number: integer;
                           aMsgType: TMsgDlgType): integer;

// no space and no special char
function UserInputFileName(const mess, sok, scancel: string;
                           var sinput: string;
                           aMsgType: TMsgDlgType): integer;

// no special char
function UserInputNoSpecialChar(const mess, sok, scancel: string;
                          var sinput: string;
                          aMsgType: TMsgDlgType;
                          aAllowEmptyString: boolean): integer;

// all char input
function UserInputAllChar(const mess, sok, scancel: string;
                          var sinput: string;
                          aMsgType: TMsgDlgType;
                          aAllowEmptyString: boolean): integer;

implementation

uses Controls,
     u_user_askconfirmation,
     u_user_showmessage,
     u_user_inputstring, u_common;

procedure ShowMess(const mess, sbutton: string; aMsgType: TMsgDlgType);
var F: TFormUserMessage;
begin
  F:=TFormUserMessage.Create(NIL);
  F.Init(mess, sbutton, aMsgType);
  F.Free;
end;

function AskConfirmation(const mess, syes, sno: string; aMsgType: TMsgDlgType): integer;
begin
  Result:=FormUserConfirmation.Init(mess, syes, sno, aMsgType);
end;

function UserInputFileName(const mess, sok, scancel: string;
  var sinput: string; aMsgType: TMsgDlgType): integer;
var F: TFormUserInput;
begin
  F:=TFormUserInput.Create(NIL);
  F.ModeFileName;

  Result:=F.Init(mess, sok, scancel, sinput, aMsgType);
  if Result=mrOk then sinput:=F.UserInput;
  F.Free;
end;

function UserInputNoSpecialChar(const mess, sok, scancel: string; var sinput: string;
  aMsgType: TMsgDlgType; aAllowEmptyString: boolean): integer;
var F: TFormUserInput;
begin
  F:=TFormUserInput.Create(NIL);
  F.ModeNoSpecialChar;
  if aAllowEmptyString then F.AllowEmptyString;

  Result:=F.Init(mess, sok, scancel, sinput, aMsgType);
  if Result=mrOk then sinput:=F.UserInput;
  F.Free;
end;

function UserInputAllChar(const mess, sok, scancel: string; var sinput: string;
  aMsgType: TMsgDlgType; aAllowEmptyString: boolean): integer;
var F: TFormUserInput;
begin
  F:=TFormUserInput.Create(NIL);
  F.ModeAllChar;
  if aAllowEmptyString then F.AllowEmptyString;

  Result:=F.Init(mess, sok, scancel, sinput, aMsgType);
  if Result=mrOk then sinput:=F.UserInput;
  F.Free;
end;

function UserIntegerInput(const mess, sok, scancel: string;
  var number: integer; aMsgType: TMsgDlgType): integer;
var F: TFormUserInput;
  snum: string;
  v, old: integer;
begin
  old:=number;
  snum:=number.ToString;

  F:=TFormUserInput.Create(NIL);
  F.ModeNumberOnly;
  Result:=F.Init(mess, sok, scancel, snum, aMsgType);
  if Result=mrOk then begin
    if TryStrToInt(snum, v)
      then number:=v
      else number:=old;
  end;
  F.Free;
end;

end.

