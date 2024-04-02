unit u_logfile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;


type

  { TLog }

  TLog=class
  private const
    FORMAT_DATETIME_DEFAULT = 'yyyy-mm-dd hh:nn:ss';
    MARGIN_STRING = '   ';
  private
    FCS: TRTLCriticalSection;
    procedure EnterCS;
    procedure LeaveCS;
  private
    FFileName: string;
    FFile: TextFile;
    FFileIsOpened: boolean;
    procedure OpenLogFile;
    procedure CloseLogFile;
    procedure Add(const aMsg: string; ShowDateTime: boolean=False);
    function Margin(aMarginCount: integer): string;
  public
    constructor Create(aFileName: string);
    destructor Destroy; override;

    procedure DeleteLogFile;
    procedure AddEmptyLine(aSeparator: string='');
    procedure Info(const aMsg: string; aMarginCount: integer=0; ShowDateTime: boolean=False);
    procedure Warning(const aMsg: string; aMarginCount: integer=0; ShowDateTime: boolean=False);
    procedure Error(const aMsg: string; aMarginCount: integer=0; ShowDateTime: boolean=False);
    procedure Debug(const aMsg: string; aMarginCount: integer=0; ShowDateTime: boolean=False);
  end;

var
  Log: TLog;

implementation

{ TLog }

procedure TLog.EnterCS;
begin
  EnterCriticalSection(FCS);
end;

procedure TLog.LeaveCS;
begin
  LeaveCriticalSection(FCS);
end;

procedure TLog.OpenLogFile;
begin
  AssignFile(FFile, FFilename);
  if not FileExists(FFileName)
    then Rewrite(FFile)
    else Append(FFile);
  FFileIsOpened:=TRUE;
end;

procedure TLog.CloseLogFile;
begin
  if FFileIsOpened then begin
    Flush(FFile);
    Close(FFile);
    FFileIsOpened:=FALSE;
  end;
end;

constructor TLog.Create(aFileName: string);
begin
  FFileName:=aFileName;
  InitCriticalSection(FCS);
end;

destructor TLog.Destroy;
begin
  CloseLogFile;
  DoneCriticalSection(FCS);
  inherited Destroy;
end;

procedure TLog.DeleteLogFile;
begin
  CloseLogFile;
  if FileExists(FFilename)
    then DeleteFile(FFilename);
end;

procedure TLog.AddEmptyLine(aSeparator: string);
begin
  EnterCS;
  try
    if not FFileIsOpened then
      OpenLogFile;

    Writeln(FFile, aSeparator);
    Flush(FFile);
  finally
    LeaveCS;
  end;
end;

procedure TLog.Add(const aMsg: string; ShowDateTime: boolean);
begin
  EnterCS;
  try
    if not FFileIsOpened then
      OpenLogFile;

    if ShowDateTime then
      Writeln(FFile, Format('%s [%s]',
        [aMsg, FormatDateTime(FORMAT_DATETIME_DEFAULT, Now)]))
    else
      Writeln(FFile, aMsg);
    Flush(FFile);
  finally
    LeaveCS;
  end;
end;

function TLog.Margin(aMarginCount: integer): string;
var
  i: integer;
begin
  Result := '';
  for i:=0 to aMarginCount-1 do
    Result := Result + MARGIN_STRING;
end;

procedure TLog.Debug(const aMsg: string; aMarginCount: integer; ShowDateTime: boolean);
begin
{$ifdef Debug}
  Add('[DD]  '+Margin(aMarginCount)+aMsg, ShowDateTime);
{$endif}
end;

procedure TLog.Error(const aMsg: string; aMarginCount: integer; ShowDateTime: boolean);
begin
  Add('[EE]  '+Margin(aMarginCount)+aMsg, ShowDateTime);
end;

procedure TLog.Warning(const aMsg: string; aMarginCount: integer; ShowDateTime: boolean);
begin
  Add('[WW]  '+Margin(aMarginCount)+aMsg, ShowDateTime);
end;

procedure TLog.Info(const aMsg: string; aMarginCount: integer; ShowDateTime: boolean);
begin
  Add('[II]  '+Margin(aMarginCount)+aMsg, ShowDateTime);
end;

Finalization
if Log <> NIL then
begin
  Log.Add('Peace');
  Log.Free;
end;

end.

