unit u_dmx_library;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LCLTranslator,
  ExtCtrls, Buttons, StdCtrls, ComCtrls, frame_viewdmxlibrary,
  frame_viewfixtureoverview, frame_viewfixturechannels, u_list_dmxuniverse;

type

  { TFormDMXLibrary }

  TFormDMXLibrary = class(TForm)
    Label1: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Shape1: TShape;
    BNewFixture: TSpeedButton;
    BEditFixture: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure BNewFixtureClick(Sender: TObject);
    procedure BEditFixtureClick(Sender: TObject);
  private
    FrameViewDMXLibrary1: TFrameViewDMXLibrary;
    procedure ProcessViewDMXLibrarySelectionChangeEvent(Sender: TObject; const aFixtureLocation: TFixtureLibraryLocation);
    procedure ProcessViewDMXLibraryMoveItem(Sender: TObject; aNode: TTreeNode; const aTargetPath: string; var Accept: boolean);
  private
    FrameFixtureOverview: TFrameFixtureOverview;
    FrameViewDMXFixtureChannels1: TFrameViewDMXFixtureChannels;

  public

  end;

//var
//  FormDMXLibrary: TFormDMXLibrary;

implementation
uses LCLType, u_resource_string, u_editfixturewizard, u_common,
  u_userdialogs;

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

  Label1.Caption := SChannels_;
end;

procedure TFormDMXLibrary.BNewFixtureClick(Sender: TObject);
var F: TFormFixtureWizard;
begin
  F := TFormFixtureWizard.Create(NIL);
  try
    if F.ShowModal = mrOk then
    begin
      FrameViewDMXLibrary1.Fill;
      // select the new fixture
      //FrameViewDMXLibrary1.SetSelected(ChangeFileExt(ExtractFileName(F.SavedFilename),''));
    end;
  finally
    F.Free;
  end;
end;

procedure TFormDMXLibrary.BEditFixtureClick(Sender: TObject);
var F: TFormFixtureWizard;
  fixLocation: TFixtureLibraryLocation;
begin
  if not FrameViewDMXLibrary1.SelectedIsMode and not FrameViewDMXLibrary1.SelectedIsFile then exit;
  fixLocation := FrameViewDMXLibrary1.SelectedFixtureLocation;

  try
    F := TFormFixtureWizard.Create(NIL);
    F.EditExistingFixture(fixLocation);
    if F.ShowModal = mrOk then begin
      FrameFixtureOverview.ShowFixture(fixLocation);
      FrameViewDMXFixtureChannels1.ShowFixture(fixLocation, True);
    end;
  finally
    F.Free;
  end;
end;

procedure TFormDMXLibrary.ProcessViewDMXLibrarySelectionChangeEvent(Sender: TObject;
  const aFixtureLocation: TFixtureLibraryLocation);
begin
  FrameFixtureOverview.Visible := aFixtureLocation.Filename <> '';
  FrameViewDMXFixtureChannels1.Visible := aFixtureLocation.Mode <> '';

  if FrameFixtureOverview.Visible then FrameFixtureOverview.ShowFixture(aFixtureLocation);
  if FrameViewDMXFixtureChannels1.Visible then FrameViewDMXFixtureChannels1.ShowFixture(aFixtureLocation, True);

  BEditFixture.Enabled := FrameFixtureOverview.Visible;
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
  FrameViewDMXLibrary1.OnMoveItem := NIL; //@ProcessViewDMXLibraryMoveItem;
  FrameViewDMXLibrary1.UserChangeEnabled := False; //TRUE;

  FrameFixtureOverview := TFrameFixtureOverview.Create(Self);
  FrameFixtureOverview.Parent := Panel3;
  FrameFixtureOverview.Align := alClient;
  FrameFixtureOverview.Visible := False;

  FrameViewDMXFixtureChannels1 := TFrameViewDMXFixtureChannels.Create(Self);
  FrameViewDMXFixtureChannels1.Parent := Panel2;
  FrameViewDMXFixtureChannels1.Align := alClient;
  FrameViewDMXFixtureChannels1.Visible := False;
end;

end.

