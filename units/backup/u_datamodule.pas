unit u_datamodule;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls;

type

  { TDataModule1 }

  TDataModule1 = class(TDataModule)
    ApplicationProperties1: TApplicationProperties;
    ILChannelType: TImageList;
    ILTreeView: TImageList;
    ImageList1: TImageList;
    ImageList2: TImageList;
    ImageList3: TImageList;
    ImageListAudio: TImageList;
    procedure ApplicationProperties1Exception(Sender: TObject; E: Exception);
  private

  public

  end;

var
  DataModule1: TDataModule1;

implementation

uses u_logfile;

{$R *.lfm}

{ TDataModule1 }

procedure TDataModule1.ApplicationProperties1Exception(Sender: TObject;
  E: Exception);
begin
  Log.Error(E.Message);
end;

end.

