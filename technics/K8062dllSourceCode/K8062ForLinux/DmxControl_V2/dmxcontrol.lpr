program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  { you can add units after this } Usb, Main, LResources, DmxUtils, K8062, Gmat, Modules ,
Dummy, Geffet, TAChartLazarusPkg;

{$IFDEF WINDOWS}{$R dmxcontrol.rc}{$ENDIF}

begin
  {$I dmxcontrol.lrs}
  Application.CreateForm(TUnivers, Univers);
  Application.CreateForm(TGestMat, GestMat);
  Application.CreateForm(TGestEffet, GestEffet);
  Application.Run;
end.

