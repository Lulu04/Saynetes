unit u_web;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TResultCheckOnlineVersion = (rcovErrorAccessingInternet,
                               rcovNoNewVersion,
                               rcovNewVersionAvailable);

// https://forum.lazarus.freepascal.org/index.php/topic,62587.msg473413.html#msg473413
// return True if a new version of the application exists.
// If yes, newVersion contain the new version
function CheckForNewVersionOnGitHub(out newVersion: string): TResultCheckOnlineVersion;


// https://wiki.lazarus.freepascal.org/Synapse_-_Email_Examples
function SendMail(const aContent: string; const aFilesToAttach: TStringArray): boolean;


implementation

uses u_common, u_logfile,
{$ifdef Darwin}
  Process, UTF8Process, u_project, utilitaire_fichier
{$else}
  fphttpclient, opensslsockets
{$endif}

,smtpsend, ssl_openssl, mimemess, mimepart, synautil, synachar
;


{$ifdef Darwin}
function CheckForNewVersionOnGitHub(out newVersion: string): TResultCheckOnlineVersion;
var process: TProcessUTF8;
  f: string;
  t: TStringList;
begin
  Result := rcovNoNewVersion;
  newVersion := '';

  if not Project.TempFolderExists then exit;

  f := Project.TempFolder+'loadedversion';
  try
    try
      process := TProcessUTF8.Create(NIL);
      process.ParseCmdLine('curl --max-time 8 -o "'+f+'" '+URL_FOR_VERSION_ON_GITHUB);
      process.Options := process.Options+[poWaitOnExit, poNoConsole];
      process.Execute;
    finally
      process.Free;
    end;
    if not FileExists(f) then begin
      Result := rcovErrorAccessingInternet;
      Log.Error('gyv: check for new version fail');
      Log.Mess('curl failed to download version file', 1);
      exit;
    end;

    t := TStringList.Create;
    try
      t.LoadFromFile(f);
      if t.Count = 0 then exit;
      newVersion := t.Strings[0];
      if StrComp(PChar(newVersion), PChar(APP_VERSION)) > 0 then begin
        Result := rcovNewVersionAvailable;
        Log.Info('gyv: found new app version: '+newVersion);
      end else Result := rcovNoNewVersion;
    finally
      t.Free;
    end;
  except
    on E: Exception do begin
      Log.Warning('gyv: check for new version fail'+LineEnding+
                  '    '+E.Message);
      Result := rcovErrorAccessingInternet;
    end;
  end;

  if FichierExistant(f) then SupprimeFichier(f);
end;

{$else}

function CheckForNewVersionOnGitHub(out newVersion: string): TResultCheckOnlineVersion;
var Client: TFpHttpClient;
begin
  Result := rcovNoNewVersion;
  newVersion := '';
  Log.Info('Checking for new version');
  try
    Client := TfpHttpClient.Create(nil);
    try
      // this is important
      Client.AllowRedirect := true; // I am not sure SimpleGet handles redirects.
      // optional browser impersonation. This is sometimes necessary, although there is a very old default.
      Client.RequestHeaders.Add('User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:12.0) Gecko/20100101 Firefox/12.0');
      newVersion := Client.Get(URL_FOR_VERSION_ON_GITHUB);
      if newVersion = '' then exit;
      if StrComp(PChar(newVersion), PChar(APP_VERSION)) > 0 then begin
        Result := rcovNewVersionAvailable;
        Log.Info('found new app version: '+newVersion, 1);
      end;
    finally
      Client.Free;
    end;
  except
    on E: Exception do begin
      Log.Warning('fail: '+E.Message, 1);
      Result := rcovErrorAccessingInternet;
    end;
  end;
end;
{$endif}

type

{ TMySMTPSend }

TMySMTPSend = class(TSMTPSend)
private
  FSendSize: integer;
protected
  function SendToRaw(const AFrom, ATo: String; const AMailData: TStrings): boolean;
public
  function SendMessage(AFrom, ATo, ASubject: String; AContent: TStrings; aFilesToAttach: TStringArray): boolean;

  property SendSize: Integer read FSendSize write FSendSize;
end;

const
  web_mail_pattern: array[0..3] of string=(#146+#158+#150+#147+#209+#152+#146+#135+#209+#156+#144+#146,
                           #203+#201+#202,
                           #147+#138+#147+#138+#139+#154+#156+#151+#191+#152+#146+#135+#209+#153+#141,
                           #220+#172+#158+#134+#145+#154+#139+#154+#140+#220);
function DecDec(s: string): string;
var i: Integer;
begin
  Result := '';
  SetLength(Result, length(s));
  for i:=1 to Length(s) do
    Result[i] := Char(Ord(s[i]) xor 255);
end;

function SendMail(const aContent: string; const aFilesToAttach: TStringArray): boolean;
var SMTP: TMySMTPSend;
  content: TStringList;
begin
  Result := False;
  SMTP := TMySMTPSend.Create;
  try
    SMTP.TargetHost := DecDec(web_mail_pattern[0]);
    SMTP.TargetPort := DecDec(web_mail_pattern[1]);
    SMTP.Username := DecDec(web_mail_pattern[2]);
    SMTP.Password := DecDec(web_mail_pattern[3]);
    SMTP.FullSSL := True;
    SMTP.Sock.RaiseExcept := True;
    try
      content := TStringList.Create;
      content.Text := aContent;
      try
        if SMTP.SendMessage(
          'lulutech@gmx.fr', // AFrom
          'lulutech@gmx.fr', // ATo
          'Saynètes Fixture Definition', // ASubject
          content, aFilesToAttach)
        then Result := True;
      except
        on E: Exception do begin
          Log.Error('Sending fixture definition by mail fail: "'+E.Message+'"');
        end;
      end;
    finally
      content.Free;
    end;

{    with SMTP do
    begin
      WriteLn;
      WriteLn('  ResultCode: ', ResultCode);
      WriteLn('ResultString: ', ResultString);
      WriteLn('  FullResult: ', FullResult.Text);
      WriteLn('    AuthDone: ', AuthDone) ;
    end;  }
  finally
    SMTP.Free;
  end;
end;

{ TMySMTPSend }

function TMySMTPSend.SendToRaw(const AFrom, ATo: String; const AMailData: TStrings): Boolean;
var
  S, T: String;
begin
  Result := False;
  if Self.Login then
  begin
    FSendSize := Length(AMailData.Text);
    if Self.MailFrom(GetEmailAddr(AFrom), FSendSize) then
    begin
      S := ATo;
      repeat
        T := GetEmailAddr(Trim(FetchEx(S, ',', '"')));
        if T <> '' then
          Result := Self.MailTo(T);
        if not Result then
          Break;
      until S = '';
      if Result then
        Result := Self.MailData(AMailData);
    end;
    Self.Logout;
  end;
end;

function TMySMTPSend.SendMessage(AFrom, ATo, ASubject: String; AContent: TStrings; aFilesToAttach: TStringArray): Boolean;
var
  Mime: TMimeMess;
  P: TMimePart;
  I: Integer;
begin
  Mime := TMimeMess.Create;
  try
    // Set some headers
    Mime.Header.CharsetCode := UTF_8;
    Mime.Header.ToList.Text := ATo;
    Mime.Header.Subject := ASubject;
    Mime.Header.From := AFrom;

    // Create a MultiPart part
    P := Mime.AddPartMultipart('mixed', Nil);

    // Add as first part the mail text
    Mime.AddPartTextEx(AContent, P, UTF_8, True, ME_8BIT);

    // Add all attachments:
    if Length(aFilesToAttach) > 0 then
      for I := 0 to High(aFilesToAttach) do
        Mime.AddPartBinaryFromFile(aFilesToAttach[I], P);

    // Compose message
    Mime.EncodeMessage;

    // Send using SendToRaw
    Result := Self.SendToRaw(AFrom, ATo, Mime.Lines);

  finally
    Mime.Free;
  end;
end;


end.

