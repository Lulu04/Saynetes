program DMX_demo;

uses
  Forms,
  DMX_demo1 in 'DMX_demo1.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
