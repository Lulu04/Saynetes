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
    procedure ApplicationProperties1QueryEndSession(var Cancel: Boolean);
    procedure DataModuleCreate(Sender: TObject);
  private
    FPath: string;
    procedure AddImageToImageList(const aSVGFilename: string; aIL: TImageList);
  public
    procedure RedrawImageForDmxLibraryTree;
    procedure RedrawImageForChannelTree;
    procedure RedrawImageForProgramButtons;
  end;

var
  DataModule1: TDataModule1;

implementation

uses u_logfile, u_mainform, u_apputils, form_splash, u_resource_string,
  utilitaire_bgrabitmap, BGRABitmap;

{$R *.lfm}

{ TDataModule1 }

procedure TDataModule1.ApplicationProperties1Exception(Sender: TObject;
  E: Exception);
begin
  Log.Error(E.Message);
end;

procedure TDataModule1.ApplicationProperties1QueryEndSession(var Cancel: Boolean);
begin
  Cancel := True;
end;

procedure TDataModule1.DataModuleCreate(Sender: TObject);
begin
  RedrawImageForDmxLibraryTree;
  RedrawImageForChannelTree;
  RedrawImageForProgramButtons;
end;

procedure TDataModule1.AddImageToImageList(const aSVGFilename: string; aIL: TImageList);
var ima: TBGRABitmap;
begin
  ima := SVGFileToBGRABitmap(FPath+aSVGFilename, aIL.Width, -1);
  aIL.Add(ima.Bitmap, NIL);
  ima.Free;
end;

procedure TDataModule1.RedrawImageForDmxLibraryTree;
begin
  ILTreeView.BeginUpdate;
  ILTreeView.Clear;
  ILTreeView.Width := FormMain.ScaleDesignToForm(16);
  ILTreeView.Height := ILTreeView.Width;

  FPath := GetAppIconImagesFolder;

  AddImageToImageList('Library.svg', ILTreeView);
  AddImageToImageList('Folder.svg', ILTreeView);
  AddImageToImageList('SingleProjector.svg', ILTreeView);
  AddImageToImageList('DmxMode.svg', ILTreeView);
  ILTreeView.EndUpdate;
end;

procedure TDataModule1.RedrawImageForChannelTree;
begin
  FormSplash.TextLoad := SLoadingDmxChannelIcon;

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
  AddImageToImageList('ColorTemperature.svg', ILChannelType);
  AddImageToImageList('StrobeSpeed.svg', ILChannelType);
  AddImageToImageList('SoundSensitivity.svg', ILChannelType);
  AddImageToImageList('BladeRotation.svg', ILChannelType);
  AddImageToImageList('Zoom.svg', ILChannelType);
  AddImageToImageList('Focus.svg', ILChannelType);
  AddImageToImageList('Rotation.svg', ILChannelType);
  AddImageToImageList('PanSpeed.svg', ILChannelType);
  AddImageToImageList('TiltSpeed.svg', ILChannelType);
  AddImageToImageList('Fan.svg', ILChannelType);
  AddImageToImageList('Smoke.svg', ILChannelType);
  AddImageToImageList('PanTilt.svg', ILChannelType);
  AddImageToImageList('PanContinuous.svg', ILChannelType);
  AddImageToImageList('TiltContinuous.svg', ILChannelType);
  AddImageToImageList('Prism.svg', ILChannelType);
  AddImageToImageList('PrismRotation.svg', ILChannelType);
  AddImageToImageList('Laser.svg', ILChannelType);
  AddImageToImageList('LaserRotation.svg', ILChannelType);
  AddImageToImageList('LaserStrobe.svg', ILChannelType);
  AddImageToImageList('GoboShake.svg', ILChannelType);
  AddImageToImageList('Frost.svg', ILChannelType);
  AddImageToImageList('SoundControled.svg', ILChannelType);
  AddImageToImageList('Effect.svg', ILChannelType);
  AddImageToImageList('EffectSpeed.svg', ILChannelType);

  AddImageToImageList('SwitchingChannel.svg', ILChannelType);
  AddImageToImageList('Switch.svg', ILChannelType);
  ILChannelType.EndUpdate;
end;

procedure TDataModule1.RedrawImageForProgramButtons;
begin
  FormSplash.TextLoad := SLoadingProgramIcon;

  ImageList1.BeginUpdate;
  ImageList1.Clear;
  ImageList1.Width := FormMain.ScaleDesignToForm(20);
  ImageList1.Height := ImageList1.Width;

  FPath := GetAppIconImagesFolder;

  AddImageToImageList('Add.svg', ImageList1);
  AddImageToImageList('Substract.svg', ImageList1);
  AddImageToImageList('Cancel.svg', ImageList1);
  AddImageToImageList('Checked.svg', ImageList1);
  AddImageToImageList('RightGreenArrow.svg', ImageList1);
  AddImageToImageList('LeftGreenArrow.svg', ImageList1);     // 5
  AddImageToImageList('Edit.svg', ImageList1);
  AddImageToImageList('ClearBrush.svg', ImageList1);
  AddImageToImageList('Redo.svg', ImageList1);
  AddImageToImageList('Undo.svg', ImageList1);
  AddImageToImageList('UpGreenArrow.svg', ImageList1);      // 10
  AddImageToImageList('DownGreenArrow.svg', ImageList1);
  AddImageToImageList('OpenFile.svg', ImageList1);
  AddImageToImageList('SaveFile.svg', ImageList1);
  AddImageToImageList('Cissor.svg', ImageList1);
  AddImageToImageList('Play.svg', ImageList1);        // 15
  AddImageToImageList('Stop.svg', ImageList1);
  AddImageToImageList('Connect.svg', ImageList1);
  AddImageToImageList('Loop.svg', ImageList1);
  AddImageToImageList('Magnify.svg', ImageList1);
  AddImageToImageList('DownBlackArrow.svg', ImageList1);   // 20
  AddImageToImageList('Link.svg', ImageList1);
  AddImageToImageList('RGB.svg', ImageList1);
  AddImageToImageList('Levels.svg', ImageList1);
  AddImageToImageList('ZoomOnSelection.svg', ImageList1);
  AddImageToImageList('ZoomAll.svg', ImageList1);     // 25
  AddImageToImageList('InsertSequence.svg', ImageList1);
  AddImageToImageList('ViewAll.svg', ImageList1);
  AddImageToImageList('DmxEdit.svg', ImageList1);
  AddImageToImageList('DmxBlackout.svg', ImageList1);
  AddImageToImageList('OnOff.svg', ImageList1);    // 30
  AddImageToImageList('AddMusic.svg', ImageList1);
  AddImageToImageList('Library.svg', ImageList1);
  AddImageToImageList('SearchFolder.svg', ImageList1);
  AddImageToImageList('Lock.svg', ImageList1);
  AddImageToImageList('Unlock.svg', ImageList1);   // 35
  AddImageToImageList('DmxGroup.svg', ImageList1);
  AddImageToImageList('ProgramOptions.svg', ImageList1);
  AddImageToImageList('DmxInfo.svg', ImageList1);
  AddImageToImageList('ProjectNew.svg', ImageList1);
  AddImageToImageList('Options.svg', ImageList1);   // 40
  AddImageToImageList('TrashCan.svg', ImageList1);
  AddImageToImageList('MagicWand.svg', ImageList1);
  AddImageToImageList('KeepOriginVisible.svg', ImageList1);
  AddImageToImageList('ModifyTime.svg', ImageList1);
  AddImageToImageList('DeleteTime.svg', ImageList1);  // 45
  AddImageToImageList('SetToDefault.svg', ImageList1);
  AddImageToImageList('DownGreenArrowForPopup.svg', ImageList1);
  AddImageToImageList('HelpButton.svg', ImageList1);
  AddImageToImageList('Reverse.svg', ImageList1);
  AddImageToImageList('Clone.svg', ImageList1);   // 50
  ImageList1.EndUpdate;
end;

end.

