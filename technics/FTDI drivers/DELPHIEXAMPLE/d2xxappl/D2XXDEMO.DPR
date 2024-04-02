program D2XXDemo;

uses
  Forms,
  Main in 'Main.pas' {Form1},
  CfgUnit in 'CfgUnit.pas' {SetupForm},
  D2XXUnit in 'D2XXUnit.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TSetupForm, SetupForm);
  Application.Run;
end.
