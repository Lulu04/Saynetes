unit u_dmx_library;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LCLTranslator,
  ExtCtrls, Buttons, StdCtrls, ComCtrls,
  frame_viewdmxlibrary, frame_viewfixturechannels;

type

  { TFormDMXLibrary }

  TFormDMXLibrary = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    LBLFixtureName: TLabel;
    LBLFixturePower: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
  private
    FrameViewDMXLibrary1: TFrameViewDMXLibrary;
    procedure ProcessViewDMXLibrarySelectionChangeEvent(Sender: TObject; aFilename: string);
    procedure ProcessViewDMXLibraryMoveItem(Sender: TObject; aNode: TTreeNode; const aTargetPath: string; var Accept: boolean);
  private
    FrameViewDMXFixtureChannels1: TFrameViewDMXFixtureChannels;

  public

  end;

//var
//  FormDMXLibrary: TFormDMXLibrary;

implementation
uses LCLType, u_resource_string, u_newfixturewizard, u_common, u_editfixture,
  u_userdialogs, u_dmx_util;

{$R *.lfm}

{ TFormDMXLibrary }

procedure TFormDMXLibrary.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

procedure TFormDMXLibrary.FormShow(Sender: TObject);
begin
  FrameViewDMXLibrary1.Fill;

  SpeedButton3.Caption := SClose;
  Label1.Caption := SChannels_;
end;

procedure TFormDMXLibrary.SpeedButton1Click(Sender: TObject);
var F: TFormFixtureWizard;
begin
  F := TFormFixtureWizard.Create(NIL);
  try
    if F.ShowModal = mrOk then
    begin
      FrameViewDMXLibrary1.Fill;
      FrameViewDMXLibrary1.SetSelected(ChangeFileExt(ExtractFileName(F.SavedFilename),''));
    end;
  finally
    F.Free;
  end;
end;

procedure TFormDMXLibrary.SpeedButton2Click(Sender: TObject);
var F: TFormEditFixture;
begin
  if FrameViewDMXLibrary1.GetSelectedFixtureFileName = '' then
    exit;

  try
    F := TFormEditFixture.Create(NIL);
    F.Edit(FrameViewDMXLibrary1.GetSelectedFixtureFileName);
    if F.ShowModal = mrOk then
      FrameViewDMXFixtureChannels1.ShowFixture(FrameViewDMXLibrary1.GetSelectedFixtureFileName);
  finally
    F.Free;
  end;
end;

procedure TFormDMXLibrary.SpeedButton3Click(Sender: TObject);
begin
  Close;
end;

procedure TFormDMXLibrary.ProcessViewDMXLibrarySelectionChangeEvent(Sender: TObject; aFilename: string);
begin
  FrameViewDMXFixtureChannels1.ShowFixture( aFilename );

  if FrameViewDMXFixtureChannels1.Ready then
  begin
    LBLFixtureName.Caption := FrameViewDMXFixtureChannels1.FixtureName;
    LBLFixturePower.Caption := FrameViewDMXFixtureChannels1.Power.ToString+' Watt';
    ShowFixtureImage(Image1, FrameViewDMXFixtureChannels1.FixtureType);
    Image1.Visible := TRUE;
    SpeedButton2.Enabled := TRUE;
  end
  else begin
    LBLFixtureName.Caption := '';
    LBLFixturePower.Caption := '';
    Image1.Visible := FALSE;
    SpeedButton2.Enabled := FALSE;
  end;
end;

procedure TFormDMXLibrary.ProcessViewDMXLibraryMoveItem(Sender: TObject; aNode: TTreeNode;
  const aTargetPath: string; var Accept: boolean);
var s: string;
begin
  if FrameViewDMXLibrary1.ItsAFolder(aNode) then
    s := lineending+SMoveTheFolder
  else
    s := lineending+SMoveTheFile;
  s := s+' '+aNode.Text+' '+STo+lineending+aTargetPath+lineending+'?';
  Accept := AskConfirmation(s, SYes, SNo, mtWarning) = mrOk;
end;

procedure TFormDMXLibrary.FormCreate(Sender: TObject);
begin
  FrameViewDMXLibrary1 := TFrameViewDMXLibrary.Create(Self);
  FrameViewDMXLibrary1.Parent := Panel1;
  FrameViewDMXLibrary1.Align := alClient;
  FrameViewDMXLibrary1.OnSelectionChange := @ProcessViewDMXLibrarySelectionChangeEvent;
  FrameViewDMXLibrary1.OnMoveItem := @ProcessViewDMXLibraryMoveItem;
  FrameViewDMXLibrary1.UserChangeEnabled := TRUE;

  FrameViewDMXFixtureChannels1 := TFrameViewDMXFixtureChannels.Create(Self);
  FrameViewDMXFixtureChannels1.Parent := Panel2;
  FrameViewDMXFixtureChannels1.Align := alClient;
end;

end.

