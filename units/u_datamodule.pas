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
    procedure DataModuleCreate(Sender: TObject);
  private
    FPath: string;
    procedure AddImageToImageList(const aSVGFilename: string; aIL: TImageList);
  public
    procedure RedrawImageForChannelTree;
  end;

var
  DataModule1: TDataModule1;

implementation

uses u_logfile, u_mainform, u_apputils, utilitaire_bgrabitmap, BGRABitmap;

{$R *.lfm}

{ TDataModule1 }

procedure TDataModule1.ApplicationProperties1Exception(Sender: TObject;
  E: Exception);
begin
  Log.Error(E.Message);
end;

procedure TDataModule1.DataModuleCreate(Sender: TObject);
begin
  RedrawImageForChannelTree;
end;

procedure TDataModule1.AddImageToImageList(const aSVGFilename: string; aIL: TImageList);
var ima: TBGRABitmap;
begin
  ima := SVGFileToBGRABitmap(FPath+aSVGFilename, aIL.Width, -1);
  aIL.Add(ima.Bitmap, NIL);
  ima.Free;
end;

procedure TDataModule1.RedrawImageForChannelTree;
begin
  ILChannelType.BeginUpdate;
  ILChannelType.Clear;
  ILChannelType.Width := FormMain.ScaleDesignToForm(20);
  ILChannelType.Height := ILChannelType.Width;

  FPath := GetAppChannelImagesFolder;

  AddImageToImageList('Config.svg', ILChannelType);
  AddImageToImageList('MasterDimmer.svg', ILChannelType);
  AddImageToImageList('Dimmer.svg', ILChannelType);
  AddImageToImageList('Red.svg', ILChannelType);
  AddImageToImageList('Green.svg', ILChannelType);
  AddImageToImageList('Blue.svg', ILChannelType);
  AddImageToImageList('Strobe.svg', ILChannelType);
  AddImageToImageList('Pan.svg', ILChannelType);
  AddImageToImageList('Tilt.svg', ILChannelType);
  AddImageToImageList('SpeedPanTilt.svg', ILChannelType);
  AddImageToImageList('Gobo.svg', ILChannelType);
  AddImageToImageList('RotationGobo.svg', ILChannelType);
  AddImageToImageList('ColorChanger.svg', ILChannelType);
  AddImageToImageList('White.svg', ILChannelType);
  AddImageToImageList('Amber.svg', ILChannelType);
  AddImageToImageList('Ultraviolet.svg', ILChannelType);
  AddImageToImageList('Speed.svg', ILChannelType);
  AddImageToImageList('NoFunction.svg', ILChannelType);
  AddImageToImageList('Cyan.svg', ILChannelType);
  AddImageToImageList('Magenta.svg', ILChannelType);
  AddImageToImageList('Yellow.svg', ILChannelType);
  AddImageToImageList('Lime.svg', ILChannelType);
  AddImageToImageList('Indigo.svg', ILChannelType);
  AddImageToImageList('WarmWhite.svg', ILChannelType);
  AddImageToImageList('ColdWhite.svg', ILChannelType);
  AddImageToImageList('Iris.svg', ILChannelType);
  AddImageToImageList('BladeInsertion.svg', ILChannelType);
  ILChannelType.EndUpdate;
end;

end.

