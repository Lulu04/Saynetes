unit driver_enttec_opendmx;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs,
{$ifdef Windows}
  //windows,
{$endif}
  ftd2xx;





type


{ TOpenDmxThread }
// c'est une classe qui encapsule un thread qui envoie les trames dmx à répétition sur l'interface OPENDMX
TOpenDmxThread = class(TThread)
  constructor Create(CreateSuspended : boolean);
  protected
    procedure Execute; override;
  private
   inter_frame_delay: integer;
   FFlagFinish: boolean;
  public
   ftHandle: DWord;
   DMXData: array [0..512] of byte; // include startcode
   procedure SetRefreshRate(rate: integer);
   property FlagFinish: boolean read FFlagFinish;
end;


{ TDriverEnttecOpenDmx }

TDriverEnttecOpenDmx = class
  private
    FftHandle: DWord;
    FFrequenceDmx: integer; // Fréquence des trames DMX de 1 à 44 Hz
    procedure fixe_frequencedmx( aF: integer);
  public
    FThreadEnvoiTrame : TOpenDmxThread ; // thread pour l'envoi des trames à répétitions
    constructor Create;
    destructor Destroy; override;
    function OuvreInterface ( aNumDevice : DWord ) : boolean ; // ouvre une interface et fixe la fréquence des trames dmx
    function FermeInterface : boolean ;
    procedure Envoi ( aAdresseDMX : integer ; aValeur: byte ) ;   // envoi une valeur sur un canal

    property FrequenceTrameDmx : integer read FFrequenceDmx write fixe_frequencedmx ; // Fréquence des trames DMX de 1 à 44 Hz
end;


implementation


{ TOpenDmxThread }

constructor TOpenDmxThread.Create(CreateSuspended: boolean);
var i: integer;
begin
  inherited Create( CreateSuspended );
  FreeOnTerminate:=FALSE;
  inter_frame_delay:=30;
  for i:=low(DMXDATA) to high(DMXDATA) do
    DMXDATA[i]:=0; // on initialise le startcode et la trame à zéro
end;


// corps du thread
procedure TOpenDmxThread.Execute;
var bytesWritten: integer;
begin
  FFlagFinish:=FALSE;
  FT_ClrRts(ftHandle);
  while not Terminated do
  begin
   FT_W32_SetCommBreak(ftHandle);
   FT_W32_ClearCommBreak(ftHandle);
   FT_W32_WriteFile(ftHandle, @DMXDATA, 513, @bytesWritten, nil);
   Sleep(inter_frame_delay);
  end;
  FFlagFinish:=TRUE;
end;

procedure TOpenDmxThread.SetRefreshRate(rate: integer);
var packet_time: double;
begin
 if rate>44 then rate:=44;
 if rate<1  then rate:=1;
 packet_time:=(1/rate)*1000;
 inter_frame_delay:=trunc(packet_time)-20;
end;








{ TDriverEnttecOpenDmx }

constructor TDriverEnttecOpenDmx.create;
begin
 inherited create ;
 FThreadEnvoiTrame := NIL ;
 FFrequenceDmx := 30 ;
end;

destructor TDriverEnttecOpenDmx.destroy;
begin
 FThreadEnvoiTrame.Terminate;
 inherited destroy;
end;

// on fixe la fréquence des trames dmx
procedure TDriverEnttecOpenDmx.fixe_frequencedmx(aF: integer);
begin
 if aF > 44 then aF := 44 ;
 if aF < 1  then aF := 1 ;
 FThreadEnvoiTrame.SetRefreshRate ( aF ) ;
 FFrequenceDmx := aF ;
end;

// ouvre l'interface OpenDmx
function TDriverEnttecOpenDmx.OuvreInterface ( aNumDevice : DWord ) : boolean ;
var
 buf   : array[0..64] of char ; //string ;
 ft_DCB : FTDCB ;
begin
 FT_ListDevices ( aNumDevice , @buf , FT_LIST_BY_INDEX or FT_OPEN_BY_SERIAL_NUMBER ) ;
 FftHandle := FT_W32_CreateFile ( PChar ( Buf ) , GENERIC_READ or GENERIC_WRITE , 0 , NIL ,
                               OPEN_EXISTING , FILE_ATTRIBUTE_NORMAL or FT_OPEN_BY_SERIAL_NUMBER , 0 ) ;
 if FftHandle = INVALID_HANDLE_VALUE
   then begin
         showmessage('Enttec OPEN DMX - Ouverture de l''interface' + lineending + 'Erreur lors de ''FT_W32_CreateFile''...' ) ;
         Result := FALSE ;
         exit ;
        end;
 if not FT_W32_GetCommState ( FftHandle , @ft_DCB )
   then begin
         showmessage('Enttec OPEN DMX - Ouverture de l''interface' + lineending + 'Erreur lors de ''FT_W32_GetCommState''...' ) ;
         FT_W32_CloseHandle ( FftHandle ) ;
         Result := FALSE ;
         exit ;
        end;
 ft_DCB.BaudRate := 250000;
 ft_DCB.Parity := FT_PARITY_NONE;
 ft_DCB.StopBits := FT_STOP_BITS_2;
 ft_DCB.ByteSize := FT_BITS_8;
 ft_DCB.fOutX := 0 ;
 ft_DCB.fInX := 0 ;
 ft_DCB.fErrorChar := 0 ;
 ft_DCB.fBinary := 1 ;
 ft_DCB.fRtsControl := 0 ;
 ft_DCB.fAbortOnError := 0 ;
 if not FT_W32_SetCommState ( FftHandle , @ft_DCB )
   then begin
         showmessage('Enttec OPEN DMX - Ouverture de l''interface' + lineending + 'Erreur lors de ''FT_W32_SetCommState''...' ) ;
         FT_W32_CloseHandle ( FftHandle ) ;
         Result := FALSE ;
         exit ;
        end;
 if not FT_W32_PurgeComm ( FftHandle , FT_PURGE_TX or FT_PURGE_RX )
   then showmessage('Enttec OPEN DMX - Ouverture de l''interface' + lineending + 'Erreur lors de ''FT_W32_PurgeComm''') ;
 // on créé le thread pour l'envoi des trames à répétitions
 FThreadEnvoiTrame := TOpenDmxThread.Create(TRUE);
 FThreadEnvoiTrame.ftHandle:=FftHandle;
 FThreadEnvoiTrame.SetRefreshRate(FFrequenceDmx);
 FThreadEnvoiTrame.Start;
 Result:=TRUE;
end;

function TDriverEnttecOpenDmx.FermeInterface : boolean ;
begin
 if FThreadEnvoiTrame<>NIL then begin
   FThreadEnvoiTrame.Terminate;
   while not FThreadEnvoiTrame.FlagFinish do;
   FThreadEnvoiTrame.Free;
   FThreadEnvoiTrame:=NIL;
 end;
 Result := FT_W32_CloseHandle ( FftHandle ) ;
end;

procedure TDriverEnttecOpenDmx.Envoi ( aAdresseDMX : integer ; aValeur: byte ) ;
begin
 FThreadEnvoiTrame.DMXData[aAdresseDmx] := aValeur ;
end;








end.

