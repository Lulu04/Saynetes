unit u_editfixture;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LCLType, ExtCtrls,
  LCLTranslator, Buttons, ComCtrls, StdCtrls, frame_viewfixturechannels;

type

  { TFormEditFixture }

  TFormEditFixture = class(TForm)
    E1: TEdit;
    Image1: TImage;
    Label11: TLabel;
    Label12: TLabel;
    Label2: TLabel;
    Label6: TLabel;
    Panel1: TPanel;
    Shape1: TShape;
    BCancel: TSpeedButton;
    BSave: TSpeedButton;
    SpeedButton1: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    UpDown1: TUpDown;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure BCancelClick(Sender: TObject);
    procedure BSaveClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure UpDown1Click(Sender: TObject; Button: TUDBtnType);
  private
    FFileName: string;
    FrameViewDMXFixtureChannels1: TFrameViewDMXFixtureChannels;
    function ParametersAreOk: boolean;
  public
    procedure Edit(const aFilename: string);
  end;

var
  FormEditFixture: TFormEditFixture;

implementation

uses u_dmx_util, u_userdialogs, u_resource_string, form_changefixtureimage;

{$R *.lfm}

{ TFormEditFixture }

procedure TFormEditFixture.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key=VK_ESCAPE then ModalResult:=mrCancel;
end;

procedure TFormEditFixture.FormShow(Sender: TObject);
begin
  BCancel.Caption := SCancel;
  BSave.Caption := SSave;
  Label6.Caption := SWatt;
  Label11.Caption := SNameOfTheFixture;
end;

procedure TFormEditFixture.BCancelClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TFormEditFixture.BSaveClick(Sender: TObject);
var libfix: TLibraryFixture;
begin
  if ParametersAreOk then begin
    if AskConfirmation(SOverwritesPreviousDataFor+lineending+
                       ExtractFileName(FFileName)+lineending,
                       SContinue, SCancel, mtWarning)<>mrOk then exit;
    libfix:=FrameViewDMXFixtureChannels1.ToLibraryFixture;
    libfix.Power:=StrToInt(E1.Text);
    try
      libfix.SaveToFile(FFileName);
      ModalResult:=mrOk;
    except
      ShowMess(SAnErrorOccuredWhileSaving+lineending+
               ExtractFileName(FFileName)+lineending+
               SDisqueIsFullOrWriteProtected, SOk, mtError);
    end;
  end;
end;

procedure TFormEditFixture.SpeedButton1Click(Sender: TObject);
var F: TFormChangeFixtureImage;
begin
  F := TFormChangeFixtureImage.Create(NIL);
  if F.ShowModal = mrOk then begin
    FrameViewDMXFixtureChannels1.FixtureType := F.SelectedFixtureType;
    ShowFixtureImage(Image1, FrameViewDMXFixtureChannels1.FixtureType);
  end;
  F.Free;
end;

procedure TFormEditFixture.SpeedButton3Click(Sender: TObject);
begin
  FrameViewDMXFixtureChannels1.MIDeleteRangeClick(NIL);
end;

procedure TFormEditFixture.SpeedButton4Click(Sender: TObject);
begin
  FrameViewDMXFixtureChannels1.MIAddRangeClick(NIL);
end;

procedure TFormEditFixture.SpeedButton5Click(Sender: TObject);
begin
  FrameViewDMXFixtureChannels1.MI_RenameChannelClick(NIL);
end;

procedure TFormEditFixture.UpDown1Click(Sender: TObject; Button: TUDBtnType);
begin
  if Button=btNext
   then FrameViewDMXFixtureChannels1.MoveSelectedUp
   else FrameViewDMXFixtureChannels1.MoveSelectedDown;
end;

function TFormEditFixture.ParametersAreOk: boolean;
var s: string;
  v: integer;
begin
  if not TryStrToInt(E1.Text, v)
    then v:=-1;
  if v<=0 then begin
    Label2.Caption:=SPowerValueMustBeAnIntegerGreaterThanZero;
    Label2.Visible:=TRUE;
    Result:=FALSE;
    exit;
  end;

  if FrameViewDMXFixtureChannels1.CheckError(s) then begin
    Label2.Caption:=s;
    Label2.Visible:=TRUE;
    Result:=FALSE;
  end else begin
    Label2.Visible:=FALSE;
    Result:=TRUE;
  end;
end;

procedure TFormEditFixture.Edit(const aFilename: string);
begin
  FFileName:=aFilename;
  FrameViewDMXFixtureChannels1.ShowFixture(aFilename, True);
  Label11.Caption:=FrameViewDMXFixtureChannels1.FixtureName;
  E1.Text:=FrameViewDMXFixtureChannels1.Power.ToString;
  ShowFixtureImage(Image1, FrameViewDMXFixtureChannels1.FixtureType);
end;

procedure TFormEditFixture.FormCreate(Sender: TObject);
begin
  FrameViewDMXFixtureChannels1:=TFrameViewDMXFixtureChannels.Create(Self);
  FrameViewDMXFixtureChannels1.Parent:=Panel1;
  FrameViewDMXFixtureChannels1.Align:=alClient;
  FrameViewDMXFixtureChannels1.EditionEnabled:=TRUE;
  FrameViewDMXFixtureChannels1.SelectionEnabled:=TRUE;
end;

end.

