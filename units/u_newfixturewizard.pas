unit u_newfixturewizard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, LCLTranslator, Spin, ComCtrls, Menus, u_common,
  frame_viewfixturechannels, frame_editstring, frame_viewdmxdipswitchs,
  frame_viewfixtureimage;

type

  { TFormFixtureWizard }

  TFormFixtureWizard = class(TForm)
    B_Cancel: TSpeedButton;
    B_Next: TSpeedButton;
    B_Previous: TSpeedButton;
    E1: TEdit;
    Image13: TImage;
    Image14: TImage;
    Image15: TImage;
    Image16: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    NB1: TNotebook;
    PageFixtureImage: TPage;
    PagePower_DipSwitch: TPage;
    PageChannelCount: TPage;
    PageNames: TPage;
    PageChannelSettings: TPage;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    PanelDipSwitch: TPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    ScrollBox1: TScrollBox;
    SE1: TSpinEdit;
    Shape1: TShape;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    UpDown1: TUpDown;
    procedure B_CancelClick(Sender: TObject);
    procedure B_NextClick(Sender: TObject);
    procedure B_PreviousClick(Sender: TObject);
    procedure E1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure Label14Click(Sender: TObject);
    procedure Label9Click(Sender: TObject);
    procedure RadioButton2Change(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
  private
    function GetFixtureModeName: string;
    function GetFixtureName: string;
    function GetManufacturer: string;
  private
    FrameViewDMXFixtureChannels1: TFrameViewDMXFixtureChannels;
    FFrameFixtureImages: array[TFixtureType] of TFrameFixtureImage;
    FFixtureType: TFixtureType;
    FChannelsCount: integer;
    procedure UpdateButton;
    procedure UpdateFixtureImages;
    procedure ProcessFrameFixtureImageChange(Sender: TFrameFixtureImage);
    procedure CreateTVItems;
    procedure ProcessChannelSelectionChangeEvent(Sender: TObject);
  private
    FrameEditString1,
    FrameEditString2,
    FrameEditString3: TFrameEditString;
    FrameViewDipSwitchs1: TFrameViewDipSwitchs;
    procedure ProcessFrameEditStringTextChangeEvent(Sender: TObject);
  private
    FSavedFilename: string;
    procedure DoSaveFixture;
    function SaveTo(const aFilename: string): boolean;
  public
    property ManufacturerName: string read GetManufacturer;
    property FixtureName: string read GetFixtureName;
    property FixtureModeName: string read GetFixtureModeName;
    property SavedFilename: string read FSavedFilename;
  end;

implementation

uses LCLType, u_resource_string, u_utils, u_userdialogs, u_apputils, u_dmx_util,
  utilitaire_fichier;

{$R *.lfm}

{ TFormFixtureWizard }

procedure TFormFixtureWizard.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_ESCAPE then ModalResult:=mrCancel;
end;

procedure TFormFixtureWizard.FormShow(Sender: TObject);
begin
  UpdateFixtureImages;
  FFrameFixtureImages[Low(TFixturetype)].Checked := True;
  FFixtureType := ftOther;

  SE1.Value:=1;
  FChannelsCount:=1;

  E1.Text:='0';
  FrameEditString1.Text:='';
  FrameEditString2.Text:='';
  FrameEditString3.Text:='';

  FrameViewDMXFixtureChannels1.Clear;

  Label6.Caption := sWatt;
  Label14.Caption:=SYes;
  Label9.Caption:=SNo;
  Label15.Caption := SOn;
  Label16.Caption := SOff;
  B_Cancel.Caption := SCancel;
  B_Next.Caption := SNext;
  B_Previous.Caption := SPrevious;

  NB1.PageIndex:=0;
  UpdateButton;
end;

procedure TFormFixtureWizard.Label14Click(Sender: TObject);
begin
  RadioButton2.Checked := TRUE;
end;

procedure TFormFixtureWizard.Label9Click(Sender: TObject);
begin
  RadioButton1.Checked := TRUE;
end;

procedure TFormFixtureWizard.RadioButton2Change(Sender: TObject);
begin
  PanelDipSwitch.Visible:=RadioButton2.Checked;
end;

procedure TFormFixtureWizard.SpeedButton1Click(Sender: TObject);
begin
  FrameViewDMXFixtureChannels1.MIAddRangeClick(NIL);
end;

procedure TFormFixtureWizard.SpeedButton2Click(Sender: TObject);
begin
  FrameViewDMXFixtureChannels1.MIDeleteRangeClick(NIL)
end;

procedure TFormFixtureWizard.SpeedButton3Click(Sender: TObject);
begin
  FrameViewDMXFixtureChannels1.MI_RenameChannelClick(NIL);
end;

procedure TFormFixtureWizard.SpeedButton4Click(Sender: TObject);
begin
  FrameViewDipSwitchs1.MSBIsLeft:=not FrameViewDipSwitchs1.MSBIsLeft;
end;

procedure TFormFixtureWizard.SpeedButton5Click(Sender: TObject);
begin
  if Label15.Caption=SOn then begin
    Label15.Caption:=SOff;
    Label16.Caption:=SON;
    FrameViewDipSwitchs1.OnIsUp:=FALSE;
  end else begin
    Label15.Caption:=SON;
    Label16.Caption:=SOff;
    FrameViewDipSwitchs1.OnIsUp:=TRUE;
  end;
end;

procedure TFormFixtureWizard.SpeedButton6Click(Sender: TObject);
begin
  if Sender=SpeedButton6
    then FrameViewDipSwitchs1.Count := FrameViewDipSwitchs1.Count+1
    else FrameViewDipSwitchs1.Count := FrameViewDipSwitchs1.Count-1;
end;

function TFormFixtureWizard.GetFixtureModeName: string;
begin
  Result := FrameEditString3.Text;
end;

function TFormFixtureWizard.GetFixtureName: string;
begin
  Result := FrameEditString2.Text;
end;

function TFormFixtureWizard.GetManufacturer: string;
begin
  Result := FrameEditString1.Text;
end;

procedure TFormFixtureWizard.UpdateFixtureImages;
var i: TFixtureType;
  xx, yy, c: integer;
begin
  xx := 0;
  yy := 0;
  c := 0;

  for i in FixtureDisplayOrder do begin
    FFrameFixtureImages[i] := TFrameFixtureImage.Create(Self);
    FFrameFixtureImages[i].Name := 'FrameFixtureImage'+Ord(i).ToString;
    FFrameFixtureImages[i].Parent := ScrollBox1;
    FFrameFixtureImages[i].Left := xx;
    FFrameFixtureImages[i].Top := yy;
    FFrameFixtureImages[i].InitWith(i);
    FFrameFixtureImages[i].OnChange := @ProcessFrameFixtureImageChange;
    xx := xx + FFrameFixtureImages[i].Width + (ScrollBox1.ClientWidth - FFrameFixtureImages[i].Width*3) div 3; // ScaleDesignToForm(20);
    inc(c);
    if c = 3 then begin
      c := 0;
      xx := 0;
      yy := yy + FFrameFixtureImages[i].Height + ScaleDesignToForm(10);
    end;
  end;
end;

procedure TFormFixtureWizard.ProcessFrameFixtureImageChange(Sender: TFrameFixtureImage);
var i: TFixtureType;
begin
  if Sender.Checked then begin
    FFixtureType := Sender.FixtureType;
    // uncheck other image
    for i:=Low(TFixtureType) to High(TFixturetype) do
      if (FFrameFixtureImages[i] <> Sender) and (FFrameFixtureImages[i].Checked) then
        FFrameFixtureImages[i].Checked := False;
  end;
end;

procedure TFormFixtureWizard.CreateTVItems;
var i: integer;
  TV: TTreeView;
  n: TTreeNode;
begin
  TV := FrameViewDMXFixtureChannels1.TV;
  if TV.Items.TopLvlCount=SE1.Value
    then exit;

  if TV.Items.TopLvlCount<SE1.Value then begin
    for i:=0 to SE1.Value-TV.Items.TopLvlCount-1 do begin
      n:=TV.Items.Add( TV.Items.GetFirstNode, SChannel+' '+(TV.Items.TopLvlCount+1).ToString);
      n.ImageIndex:=ord(ctCONFIG);
        // create one default channel range '0-->255 : '
        TV.Items.AddChild(n, EncodeDMXChannelRange(0, 255, ''));
    end;
  end else begin
    while TV.Items.TopLvlCount>SE1.Value do
      TV.Items[TV.Items.TopLvlCount-1].Delete;
  end;
  TV.FullExpand;

  FrameViewDMXFixtureChannels1.ChannelCount:=SE1.Value;
end;

procedure TFormFixtureWizard.ProcessChannelSelectionChangeEvent(Sender: TObject );
begin
  if FrameViewDMXFixtureChannels1.Selected<>NIL then begin
    SpeedButton3.Enabled:=FrameViewDMXFixtureChannels1.SelectedIsChannelName;
    SpeedButton1.Enabled:=TRUE;
    SpeedButton2.Enabled:=FrameViewDMXFixtureChannels1.SelectedIsRange;
  end else begin
      SpeedButton3.Enabled:=FALSE;
      SpeedButton1.Enabled:=FALSE;
      SpeedButton2.Enabled:=FALSE;
  end;
end;

procedure TFormFixtureWizard.ProcessFrameEditStringTextChangeEvent(Sender: TObject);
begin
  B_Next.Enabled := FrameEditString1.TextIsValid and (Trim(FrameEditString1.Text) <> '') and
                    FrameEditString2.TextIsValid and (Trim(FrameEditString2.Text) <> '') and
                    FrameEditString3.TextIsValid;
end;

procedure TFormFixtureWizard.DoSaveFixture;
var path, fullFilename, temp: string;
  flag: boolean;
  i: integer;
begin
  FSavedFilename:='';
  // if folder with a manufacturer name not exists, create it
  path:=ConcatPaths([GetAppDMXLibraryFolder, ReplaceForbidenCharByUnderscore(ManufacturerName,' ')]);
  flag:=RepertoireExistant(path);
  if not flag
    then flag :=CreerRepertoire(path);
  if not flag then begin
    ShowMess(SAnErrorOccuredWhileCreatingTheManufacturerFolderInTheDMXLibrary, SOk, mtError);
    exit;
  end;

  temp:=ReplaceForbidenCharByUnderscore(FixtureName,' ');
  if FixtureModeName<>''
    then temp:=temp+'_'+ReplaceForbidenCharByUnderscore(FixtureModeName,' ');
  temp:=temp+DMX_LIBRARY_FILE_EXTENSION;

  fullFilename:=ConcatPaths([path, temp]);
  // if there is a file with the same name, we add a trailing number to differentiate it
  if FichierExistant(fullFilename) then begin
    i:=0;
    repeat
      inc(i);
      temp:=ChangeFileExt(fullFilename,'')+i.ToString+DMX_LIBRARY_FILE_EXTENSION;
    until not FichierExistant(temp);
    ShowMess(SFixtureFileAlreadyExists+LINEENDING+temp, SOk, mtWarning);
    fullFilename:=temp;
  end;

  if not SaveTo(fullFilename) then begin
    ShowMess(SAnErrorOccuredWhileSaving+lineending+
             ExtractFileName(fullFilename)+lineending+
             SDisqueIsFullOrWriteProtected, SOk, mtError);
    exit;
  end;
  FSavedFilename:=fullFilename;
  // ask to the user if he/she want to define another mode for this same fixture
  if AskConfirmation(STheFixtureWasCreatedSuccessfully+lineending+lineending+
                     SWouldYouLikeToDefineAnotherModeForThisFixture,
                     SYes, SNo, mtConfirmation)=mrOk then begin
    //B_Next.Caption:=SNext;
    NB1.PageIndex:=NB1.IndexOf(PageNames);
    UpdateButton;
  end else ModalResult:=mrOk;
end;

function TFormFixtureWizard.SaveTo(const aFilename: string): boolean;
var libfix: TLibraryFixture;
  i: integer;
begin
  FrameViewDMXFixtureChannels1.FillChannels({%H-}libfix.Channels);
  if FixtureModeName<>''
    then libfix.Name:=FixtureName+'_'+FixtureModeName
    else libfix.Name:=FixtureName;
  libfix.Power:=StrToInt(E1.Text);
  libfix.FixtureType:=FFixtureType;
  // dipswitch infos
  if RadioButton2.Checked then begin
    libfix.DipSwitch.OnIsUp:=FrameViewDipSwitchs1.OnIsUp;
    libfix.DipSwitch.MSBIsLeft:=FrameViewDipSwitchs1.MSBIsLeft;
    SetLength(libfix.DipSwitch.Functions, FrameViewDipSwitchs1.Count);
    for i:=0 to FrameViewDipSwitchs1.Count-1 do begin
      libfix.DipSwitch.Functions[i]:=FrameViewDipSwitchs1.Switchs[i].SwitchFunction;
    end;
  end;

  Result:=libfix.SaveToFile(aFilename);
end;

procedure TFormFixtureWizard.B_CancelClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TFormFixtureWizard.UpdateButton;
var v: integer;
begin
  B_Previous.Enabled := NB1.PageIndex>0;
  if NB1.PageIndex=(NB1.PageCount-1)
    then B_Next.Caption:=SFinish
    else B_Next.Caption:=SNext;

  if NB1.ActivePageComponent=PageFixtureImage then begin
    B_Next.Enabled:=TRUE;
  end;

  if NB1.ActivePageComponent=PagePower_DipSwitch then begin
      ShowFixtureImage(Image13, FFixtureType );
      B_Next.Enabled:=TryStrToInt(E1.Text, v);
      if B_Next.Enabled then B_Next.Enabled:=v>0;
      Label3.Visible:=not B_Next.Enabled;
      Label3.Caption:=SPowerValueMustBeAnIntegerGreaterThanZero;
  end;

  if NB1.ActivePageComponent=PageChannelCount then begin
      ShowFixtureImage(Image14, FFixtureType );
    end;

  if NB1.ActivePageComponent = PageNames then begin
      ShowFixtureImage(Image15, FFixtureType );
      B_Next.Enabled := FrameEditString1.TextIsValid and (Trim(FrameEditString1.Text) <> '') and
                        FrameEditString2.TextIsValid and (Trim(FrameEditString2.Text) <> '') and
                        FrameEditString3.TextIsValid;
    end;

  if NB1.ActivePageComponent=PageChannelSettings then begin
      ShowFixtureImage(Image16, FFixtureType );
      Label11.Caption:=FrameEditString1.Text;
      Label12.Caption:=FrameEditString2.Text;
      Label13.Caption:=FrameEditString3.Text;
      CreateTVItems;
    end;
end;

procedure TFormFixtureWizard.B_NextClick(Sender: TObject);
var s: string;
begin
  if NB1.ActivePageComponent = PageChannelSettings then begin
    if not FrameViewDMXFixtureChannels1.CheckError(s)
      then DoSaveFixture
      else begin
         Label2.Caption := s;
         Label2.Visible := TRUE;
    end;
  end
  else
  if NB1.ActivePageComponent = PageFixtureImage then NB1.PageIndex := NB1.IndexOf(PagePower_DipSwitch)
  else
  if NB1.ActivePageComponent = PagePower_DipSwitch then NB1.PageIndex := NB1.IndexOf(PageNames)
  else
  if NB1.ActivePageComponent = PageNames then begin
    if FrameEditString1.TextIsValid and (Trim(FrameEditString1.Text) <> '') and
       FrameEditString2.TextIsValid and (Trim(FrameEditString2.Text) <> '') and
       FrameEditString3.TextIsValid then
      NB1.PageIndex := NB1.IndexOf(PageChannelCount);
  end
  else
  if NB1.ActivePageComponent = PageChannelCount then NB1.PageIndex := NB1.IndexOf(PageChannelSettings);

  UpdateButton;
end;

procedure TFormFixtureWizard.B_PreviousClick(Sender: TObject);
begin
  case NB1.PageIndex of
    0: ;
    1: NB1.PageIndex:=0;
    2: NB1.PageIndex:=3;
    3: NB1.PageIndex:=1;
    4: NB1.PageIndex:=2;
  end;//case
  UpdateButton;
end;

procedure TFormFixtureWizard.E1Change(Sender: TObject);
begin
  UpdateButton;
end;

procedure TFormFixtureWizard.FormCreate(Sender: TObject);
begin
  FrameViewDMXFixtureChannels1:=TFrameViewDMXFixtureChannels.Create(Self);
  FrameViewDMXFixtureChannels1.Parent:=Panel1;
  FrameViewDMXFixtureChannels1.Align:=alClient;
  FrameViewDMXFixtureChannels1.EditionEnabled:=TRUE;
  FrameViewDMXFixtureChannels1.SelectionEnabled:=TRUE;
  FrameViewDMXFixtureChannels1.OnSelectionChange:=@ProcessChannelSelectionChangeEvent;

  FrameEditString1:=TFrameEditString.Create(Self);
  FrameEditString1.Name:='FrameEditString1';
  FrameEditString1.Parent:=Panel2;
  FrameEditString1.Align:=alClient;
  FrameEditString1.ModeFileName; // ModeNoSpecialChar;
  FrameEditString1.Title:=SManufacturerName;
  FrameEditString1.FontHeight:=20;
  FrameEditString1.OnTextChange := @ProcessFrameEditStringTextChangeEvent;

  FrameEditString2:=TFrameEditString.Create(Self);
  FrameEditString2.Name:='FrameEditString2';
  FrameEditString2.Parent:=Panel3;
  FrameEditString2.Align:=alClient;
  FrameEditString2.ModeNoSpecialChar;
  FrameEditString2.Title:=SNameOfTheFixture;
  FrameEditString2.FontHeight := 20;
  FrameEditString2.OnTextChange := @ProcessFrameEditStringTextChangeEvent;

  FrameEditString3:=TFrameEditString.Create(Self);
  FrameEditString3.Name:='FrameEditString3';
  FrameEditString3.Parent:=Panel4;
  FrameEditString3.Align:=alClient;
  FrameEditString3.ModeNoSpecialChar;
  FrameEditString3.AllowEmptyString;
  FrameEditString3.Title:=SModeName;
  FrameEditString3.FontHeight:=20;
  FrameEditString3.OnTextChange := @ProcessFrameEditStringTextChangeEvent;

  FrameViewDipSwitchs1:=TFrameViewDipSwitchs.Create(Self);
  FrameViewDipSwitchs1.Parent:=Panel5;
  FrameViewDipSwitchs1.Align:=alClient;
  FrameViewDipSwitchs1.Editable:=TRUE;
end;

end.

