unit u_editfixturewizard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, LCLTranslator, Spin, ComCtrls, Menus, LMessages, LCLType,
  u_common, frame_viewdmxdipswitchs, frame_viewfixtureimage, u_notebook_util,
  u_list_dmxuniverse, frame_editmode;

type

  { TFormFixtureWizard }

  TFormFixtureWizard = class(TForm)
    BAddManufacturer: TSpeedButton;
    BAddWebLink: TSpeedButton;
    BDeleteWebLink: TSpeedButton;
    BSave: TSpeedButton;
    BCheckAndSave: TSpeedButton;
    BPhysical: TSpeedButton;
    BModes: TSpeedButton;
    CBLens: TComboBox;
    CBConnector: TComboBox;
    CBManufacturers: TComboBox;
    Edit1: TEdit;
    Edit7: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    EditAuthors: TEdit;
    Edit9: TEdit;
    FSE2: TFloatSpinEdit;
    FSE3: TFloatSpinEdit;
    Image1: TImage;
    Image13: TImage;
    Image14: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label3: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label4: TLabel;
    Label40: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    NB1: TNotebook;
    PageFinish: TPage;
    PageModes: TPage;
    PageGeneral: TPage;
    PagePhysical: TPage;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel5: TPanel;
    PanelDipSwitch: TPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    ScrollBox1: TScrollBox;
    ScrollBox2: TScrollBox;
    LBWebLink: TListBox;
    Shape1: TShape;
    BGeneral: TSpeedButton;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    BAddMode: TSpeedButton;
    Timer1: TTimer;
    UpDown1: TUpDown;
    procedure BAddManufacturerClick(Sender: TObject);
    procedure BSaveClick(Sender: TObject);
    procedure CBConnectorChange(Sender: TObject);
    procedure CBManufacturersChange(Sender: TObject);
    procedure EditAuthorsUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LBWebLinkDblClick(Sender: TObject);
    procedure RadioButton2Change(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure BAddModeClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure UpDown1Click(Sender: TObject; Button: TUDBtnType);
  private
    //FrameViewDMXFixtureChannels1: TFrameViewDMXFixtureChannels;
    //FrameEditString1: TFrameEditString;
    FFrameFixtureImages: array[TFixtureType] of TFrameFixtureImage;
    FFixtureType: TFixtureType;
    procedure UpdateFixtureImages;
    procedure ProcessFrameFixtureImageChange(Sender: TFrameFixtureImage);
    procedure ProcessPageChangeEvent(Sender: TObject);
    procedure MessageHandler(var Message: TLMessage); message LM_MESSAGE_EditFixtureWizard;
    procedure DoSave;
  private
    NoteBookManager: TNoteBookManager;
    CheckedLabelManager: TCheckedLabelManager;
    FrameViewDipSwitchs1: TFrameViewDipSwitchs;
    ModeFrames: array of TFrameEditMode;  // the modes defined by the user
    FModeFrameToKill: TFrameEditMode;
    FCounterForModeFrameName: integer;
    procedure DoCreateFrame(aTargetFrameIndex: integer; p: PFixLibMode);
    procedure DoDeleteModeFrame;
    procedure AdjustAddButtonPosition;
    procedure ProcessModeChangeHeightEvent(Sender: TObject);
  private
    FManufacturers: TManufacturers;
    FEditingExistingFixture: boolean;
    FFixtureLocationToSave: TFixtureLibraryLocation;
    procedure LoadFixtureToEdit;
    function FormatFixtureNameToFilename(s: string): string;
    function GetOutputFilename: string;
  private
    FSavedFilename: TFixtureLibraryLocation;
    function GeneralDataAreOK: boolean;
    function PhysicalDataAreOK: boolean;
    function ModesAndChannelsDataAreOK: boolean;
  private
    FExistingChannels: TFixLibAvailableChannels; // the channels defined by the user
    FIsModified, FInitializing: boolean;
    FWebLinks: TWebLinks; // the web links defined by the user
    procedure SetIsModified(AValue: boolean);
  public // called by instance of TFrameEditMode
    function CheckIfModeNameIsUsed(aCaller: TFrameEditMode; const aModeName: string): boolean;
    function CheckIfModeShortNameIsUsed(aCaller: TFrameEditMode; const aModeShortName: string): boolean;
    procedure KillModeFrame(aCaller: TFrameEditMode);
    procedure ReplaceChannelNameInAllFrames(const aOldNameNoPrefix, aNewNameNoPrefix: string);
  public
    procedure EditExistingFixture(aFixLocation: TFixtureLibraryLocation);

    property SavedFixLocation: TFixtureLibraryLocation read FSavedFilename;
    property Modified: boolean read FIsModified write SetIsModified;
  end;

implementation

uses u_resource_string, u_userdialogs, u_apputils, u_helper, LCLIntf,
  u_datamodule, form_editweblink, u_dmx_util, LazUTF8, Math, utilitaire_bgrabitmap;

{$R *.lfm}

{ TFormFixtureWizard }

procedure TFormFixtureWizard.FormShow(Sender: TObject);
begin
  UpdateFixtureImages;
  if FEditingExistingFixture then begin
    LoadFixtureToEdit;
  end else begin
    FFrameFixtureImages[Low(TFixturetype)].Checked := True;
    FFixtureType := ftOther;
    // create one empty mode
    ModeFrames := NIL;
    SetLength(ModeFrames, 1);
    DoCreateFrame(0, NIL);
  end;

  // manual translation
  Label17.Caption := SManufacturer+':';
  BAddManufacturer.Caption := sAdd;
  Label18.Caption := SFixture+':';
  Label19.Caption := SAuthors+':';
  Label23.Caption := SDimensions+':';
  Label21.Caption := SWidth+' x '+SHeight+' x '+SDepth;
  Label28.Caption := SWeight+':';
  Label31.Caption := SPower;
  Label32.Caption := SWatt;
  Label33.Caption := SConnector;
  Label34.Caption := SBulb;
  Label37.Caption := SLumens;
  Label38.Caption := SLens;
  Label39.Caption := SMinDegree;
  Label40.Caption := SMaxDegree;

  Label14.Caption:=SYes;
  Label9.Caption:=SNo;
  Label15.Caption := SOn;
  Label16.Caption := SOff;

  Label22.Caption := SWebLinks;
end;

procedure TFormFixtureWizard.LBWebLinkDblClick(Sender: TObject);
begin
  if LBWebLink.ItemIndex = -1 then exit;
  try
    OpenUrl(FWebLinks[LBWebLink.ItemIndex].Url);
  except
  end;
end;

procedure TFormFixtureWizard.RadioButton2Change(Sender: TObject);
begin
  PanelDipSwitch.Visible := RadioButton2.Checked;
  Modified := True;
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

procedure TFormFixtureWizard.BAddModeClick(Sender: TObject);
var i: SizeInt;
begin
  i := Length(ModeFrames);
  SetLength(ModeFrames, i+1);
  DoCreateFrame(i, NIL);

  Modified := True;
end;

procedure TFormFixtureWizard.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  if Panel1.Tag = 1 then begin
    case Timer1.Tag of
      0: begin
        Timer1.Tag := 1;
        Panel1.Visible := True;
        Panel2.Visible := True;
      end;
      1: begin
        Timer1.Tag := 0;
        Panel1.Visible := False;
        Panel2.Visible := False;
      end;
    end;
  end else begin
    Panel1.Visible := False;
    Panel2.Visible := False;
  end;

  Timer1.Enabled := True;
end;

procedure TFormFixtureWizard.UpDown1Click(Sender: TObject; Button: TUDBtnType);
var i: integer;
  s: string;
  procedure ExchangeLinks(i1, i2: integer);
  begin
    LBWebLink.Items.Exchange(i1, i2);
    FWebLinks[i1].ExchangeWith(FWebLinks[i2]);
    LBWebLink.ItemIndex := i2;
  end;

begin
  i := LBWebLink.ItemIndex;
  if i = -1 then exit;

  if (Button = btNext) and (i > 0) then ExchangeLinks(i, i-1);
  if (Button = btPrev) and (i < LBWebLink.Count-1) then ExchangeLinks(i, i+1);
  Modified := True;
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

    Modified := True;
  end;
end;

procedure TFormFixtureWizard.ProcessPageChangeEvent(Sender: TObject);
var imageIndex: integer;
begin
  if NB1.PageIndex = NB1.IndexOf(PageGeneral) then begin
    GeneralDataAreOK;
  end else
  if NB1.PageIndex = NB1.IndexOf(PagePhysical) then begin
    ShowFixtureImage(Image13, FFixtureType);
    PhysicalDataAreOK;
  end else
  if NB1.PageIndex = NB1.IndexOf(PageModes) then begin
    ModesAndChannelsDataAreOK;
  end else
  if NB1.PageIndex = NB1.IndexOf(PageFinish) then begin
    ShowFixtureImage(Image14, FFixtureType);

    Image1.Width := DataModule1.ImageList1.Width;
    Image1.Height := DataModule1.ImageList1.Height;
    if GeneralDataAreOK then imageIndex := 3 else imageIndex := 2;
    DataModule1.ImageList1.GetBitmap(imageIndex, Image1.Picture.Bitmap);

    Image2.Width := DataModule1.ImageList1.Width;
    Image2.Height := DataModule1.ImageList1.Height;
    if PhysicalDataAreOK then imageIndex := 3 else imageIndex := 2;
    DataModule1.ImageList1.GetBitmap(imageIndex, Image2.Picture.Bitmap);

    Image3.Width := DataModule1.ImageList1.Width;
    Image3.Height := DataModule1.ImageList1.Height;
    if ModesAndChannelsDataAreOK then imageIndex := 3 else imageIndex := 2;
    DataModule1.ImageList1.GetBitmap(imageIndex, Image3.Picture.Bitmap);

    if GeneralDataAreOK then begin
      Label13.Caption := FManufacturers[CBManufacturers.ItemIndex].Folder;
      Label20.Caption := FormatFixtureNameToFilename(Edit7.Text);
    end else begin
      Label13.Caption := '';
      Label20.Caption := '';
    end;

    BSave.Enabled := GeneralDataAreOK and PhysicalDataAreOK and ModesAndChannelsDataAreOK;
    Label8.Visible := not BSave.Enabled;
  end;
end;

procedure TFormFixtureWizard.MessageHandler(var Message: TLMessage);
begin
  case Message.lParam of
   MESS_DeleteModeFrame: DoDeleteModeFrame;

  end;//case
end;

procedure TFormFixtureWizard.DoSave;
var i, j: integer;
  libFix: TLibraryFixture;
  A, chanUsed: TStringArray;
  f: string;
  p: PFixLibAvailableChannel;
  procedure AddExclusiveToA(const na: string);
  var ii: integer;
  begin
    for ii:=0 to High(A) do
      if A[ii] = na then exit;
    ii := Length(A);
    SetLength(A, ii+1);
    A[ii] := na;
  end;
begin
  // web links
  libFix.WebLinks := NIL;
  FWebLinks.CopyTo(libFix.WebLinks);

  // general
  with libFix.General do begin
    InitDefault;
    ManufacturerName := CBManufacturers.Text;
    FixtureName := Trim(Edit7.Text);
    Authors := Trim(EditAuthors.Text);
    Creator := 'Saynètes '+APP_VERSION;
    FixtureType := FFixtureType;
  end;

  // physical
  with libFix.Physical do begin
    InitDefault;
    Width := StrToInt(Edit1.Text);
    Height := StrToInt(Edit2.Text);
    Depth := StrToInt(Edit3.Text);
    Power := StrToInt(Edit5.Text);
    Weight := Edit4.Text;
    Connector := CBConnector.Text;
    Bulb := Edit9.Text;
    Lumens := StrToInt(Edit6.Text);
    Lens := CBLens.Text;
    LensMinDegree := FSE2.Value;
    LensMaxDegree := FSE3.Value;
  end;

  // dip switch
  libFix.DipSwitchs.InitDefault;
  if RadioButton2.Checked then
    with libFix.DipSwitchs do begin
      OnIsUp := FrameViewDipSwitchs1.OnIsUp;
      MSBIsLeft := FrameViewDipSwitchs1.MSBIsLeft;
      Functions := NIL;
      SetLength(Functions, FrameViewDipSwitchs1.Count);
      for i:=0 to High(Functions) do
        Functions[i] := FrameViewDipSwitchs1.Switchs[i].SwitchFunction;
    end;

  // modes
  libFix.Modes := NIL;
  SetLength(libFix.Modes, Length(ModeFrames));
  for i:=0 to High(ModeFrames) do
    with libFix.Modes[i] do begin
      InitDefault;
      Name := ModeFrames[i].ModeName;
      ShortName := ModeFrames[i].ShortModeName;
      ChannelsIDToUse := NIL;
      A := ModeFrames[i].ChannelsUsed;
      SetLength(ChannelsIDToUse, Length(A));
      for j:=0 to High(A) do ChannelsIDToUse[j] := A[j];
    end;

  // channels used by modes
  // we keep only the channels used by modes
  A := NIL;
  for i:=0 to High(ModeFrames) do begin
    chanUsed := ModeFrames[i].ChannelsUsed;
    for j:=0 to High(chanUsed) do
      AddExclusiveToA(chanUsed[j]);
  end;

  libFix.AvailableChannels := NIL;
  SetLength(libFix.AvailableChannels, Length(A));
  for i:=0 to High(A) do begin
    p := FExistingChannels.GetChannelsByName(A[i]);
    if p = NIL then raise exception.create('error in algorithm');
    p^.CopyTo(libFix.AvailableChannels[i]);
  end;


  // save
  f := GetOutputFilename;
  if not FEditingExistingFixture and FileExists(f) then
    if AskConfirmation(SAskToOverwriteFixtureFilename, SYes, SCancel, mtWarning) <> mrOk then exit;

  if not libFix.SaveToFile(f) then ShowMess('Enable to save the fixture...', SClose, mtError)
    else begin
      FIsModified := False;
      ModalResult := mrOk;
    end;
end;

procedure TFormFixtureWizard.DoCreateFrame(aTargetFrameIndex: integer; p: PFixLibMode);
var i, j, y: integer;
begin
  i := aTargetFrameIndex;
  ModeFrames[i] := TFrameEditMode.Create(Self);
  ModeFrames[i].Name := 'FrameEditMode'+FCounterForModeFrameName.ToString;
  ModeFrames[i].Parent := ScrollBox2;
  ModeFrames[i].Width := (ScrollBox2.ClientWidth - ScaleDesignToForm(5)*3) div 2;
  if not Odd(i) then ModeFrames[i].Left := ScaleDesignToForm(5)
    else ModeFrames[i].Left := ScrollBox2.ClientWidth - ModeFrames[i].Width - ScaleDesignToForm(5);

  if i < 2 then y := 0
    else y := ModeFrames[i-2].Top + ModeFrames[i-2].Height + ScaleDesignToForm(5);

  ModeFrames[i].Top := y;
  ModeFrames[i].ModeIndex := i;
  ModeFrames[i].ExistingChannels := @FExistingChannels;
  ModeFrames[i].OnHeightChange := @ProcessModeChangeHeightEvent;

  if p <> NIL then ModeFrames[i].InitFrom(p^);

  AdjustAddButtonPosition;
  inc(FCounterForModeFrameName);
end;

procedure TFormFixtureWizard.DoDeleteModeFrame;
var deletedIndex, i, y: integer;
  o: TFrameEditMode;
begin
  if FModeFrameToKill = NIL then exit;
  deletedIndex := FModeFrameToKill.ModeIndex;
  FModeFrameToKill.Free;
  FModeFrameToKill := NIL;
  Delete(ModeFrames, deletedIndex, 1);

  // shift next frames indexes
  for i:=deletedIndex to high(ModeFrames) do begin
    o := ModeFrames[i];
    if o.ModeIndex <> i then begin
      o.ModeIndex := i;

      if not Odd(i) then ModeFrames[i].Left := ScaleDesignToForm(5)
        else ModeFrames[i].Left := ScrollBox2.ClientWidth - ModeFrames[i].Width - ScaleDesignToForm(5);

      if i < 2 then y := 0
        else if not Odd(i) then y := ModeFrames[i-1].Top + ModeFrames[i-1].Height + ScaleDesignToForm(5)
               else y := ModeFrames[i-1].Top;
      ModeFrames[i].Top := y;
    end;
  end;
  AdjustAddButtonPosition;

  Modified := True;
end;

procedure TFormFixtureWizard.AdjustAddButtonPosition;
var i, y: integer;
begin
  y := 0;
  for i:=0 to High(ModeFrames) do
    y := Max(y, ModeFrames[i].Top + ModeFrames[i].Height + ScaleDesignToForm(5));
  BAddmode.Top := y;
end;

procedure TFormFixtureWizard.ProcessModeChangeHeightEvent(Sender: TObject);
var o: TFrameEditMode;
  i, y: integer;
begin
  o := TFrameEditMode(Sender);
  y := o.Top + o.Height;
  for i:=o.ModeIndex+2 to High(ModeFrames) do begin
    if Odd(i) and Odd(o.ModeIndex) then ModeFrames[i].Top := ModeFrames[i-2].Top + ModeFrames[i-2].Height + ScaleDesignToForm(5);
    if not Odd(i) and not Odd(o.ModeIndex) then ModeFrames[i].Top := ModeFrames[i-2].Top + ModeFrames[i-2].Height + ScaleDesignToForm(5);
  end;
  AdjustAddButtonPosition;
end;

function TFormFixtureWizard.FormatFixtureNameToFilename(s: string): string;
var curP, endP: PChar;
  len: integer;
begin
  s := LowerCase(Trim(s));
  Result := '';
  curP := PChar(s);
  endP := curP + Length(S);
  while curP < endP do begin
    len := UTF8CodepointSize(curP);
    if (len = 1) and (curP^ in ['a'..'z','0'..'9','-']) then Result := Result + curP^
      else Result := Result + '-';
    inc(curP, len);
  end;

  if Result <> '' then Result := Result + DMX_LIBRARY_FILE_EXTENSION;
end;

function TFormFixtureWizard.GetOutputFilename: string;
begin
  Result := ConcatPaths([GetAppDMXLibraryFolder,
                    FManufacturers[CBManufacturers.ItemIndex].Folder,
                    ChangeFileExt(FormatFixtureNameToFilename(Edit7.Text), DMX_LIBRARY_FILE_EXTENSION)]);
end;

function TFormFixtureWizard.GeneralDataAreOK: boolean;
begin
  Result := (CBManufacturers.ItemIndex <> -1) and
            (Trim(Edit7.Text) <> '');
end;

function TFormFixtureWizard.PhysicalDataAreOK: boolean;
begin
  Shape3.Visible := CBConnector.ItemIndex = -1;
  Shape4.Visible := Edit5.Text = '';

  Result := (Trim(Edit1.Text) <> '') and
            (Trim(Edit2.Text) <> '') and
            (Trim(Edit3.Text) <> '') and
            //(Trim(Edit4.Text) <> '') and
            (Edit4.Color = clDefault) and
            (Trim(Edit5.Text) <> '') and
            //(Trim(Edit9.Text) <> '') and
            //(Trim(Edit6.Text) <> '') and
            (CBConnector.ItemIndex <> -1);
end;

function TFormFixtureWizard.ModesAndChannelsDataAreOK: boolean;
var i: integer;
begin
  if length(ModeFrames) = 0 then exit(False);

  for i:=0 to High(ModeFrames) do
    if ModeFrames[i].HaveError then exit(False);

  Result := True;
end;

procedure TFormFixtureWizard.SetIsModified(AValue: boolean);
begin
  if FInitializing then exit;
  FIsModified := AValue;
end;

function TFormFixtureWizard.CheckIfModeNameIsUsed(aCaller: TFrameEditMode; const aModeName: string): boolean;
var i: integer;
begin
  for i:=0 to High(ModeFrames) do
    if (ModeFrames[i] <> aCaller) and (ModeFrames[i].ModeName = aModeName) then exit(True);
  Result := False;
end;

function TFormFixtureWizard.CheckIfModeShortNameIsUsed(aCaller: TFrameEditMode; const aModeShortName: string): boolean;
var i: integer;
begin
  for i:=0 to High(ModeFrames) do
    if (ModeFrames[i] <> aCaller) and (ModeFrames[i].ShortModeName = aModeShortName) then exit(True);
  Result := False;
end;

procedure TFormFixtureWizard.KillModeFrame(aCaller: TFrameEditMode);
begin
  FModeFrameToKill := aCaller;
  PostMessage(Handle, LM_MESSAGE_EditFixtureWizard, 0, MESS_DeleteModeFrame);
end;

procedure TFormFixtureWizard.ReplaceChannelNameInAllFrames(const aOldNameNoPrefix, aNewNameNoPrefix: string);
var i: integer;
begin
  for i:=0 to High(ModeFrames) do
    ModeFrames[i].ReplaceChannelName(aOldNameNoPrefix, aNewNameNoPrefix);
end;

procedure TFormFixtureWizard.EditExistingFixture(aFixLocation: TFixtureLibraryLocation);
begin
  FEditingExistingFixture := True;
  FFixtureLocationToSave.InitDefault;
  aFixLocation.CopyTo(FFixtureLocationToSave);
end;

procedure TFormFixtureWizard.LoadFixtureToEdit;
var libFix: TLibraryFixture;
  i: integer;
  j: TFixtureType;
begin
  // we load the fixture to edit
  libFix.InitDefault;
  if not libFix.LoadFrom(FFixtureLocationToSave) then exit;

  FInitializing := True;
  try
    // dispatch data into gui
    // General
    CBManufacturers.ItemIndex := FManufacturers.IndexOfName(libFix.General.ManufacturerName);
    Edit7.Text := libFix.General.FixtureName;
    FFixtureType := libFix.General.FixtureType;
    EditAuthors.Text := libFix.General.Authors;
    for j in TFixtureType do begin  // select the right image
      FFrameFixtureImages[j].Checked := FFrameFixtureImages[j].FixtureType = FFixtureType;
     // if FFrameFixtureImages[j].Checked then ScrollBox1.ScrollBy(0, FFrameFixtureImages[j].Top);
    end;

    // Web links
    FWebLinks := NIL;
    libFix.WebLinks.CopyTo(FWebLinks);
    LBWebLink.Clear;
    for i:=0 to High(libFix.WebLinks) do
      LBWebLink.Items.Add(libFix.WebLinks[i].LinkType);

    // Physical
    Edit1.Text := libFix.Physical.Width.ToString;
    Edit2.Text := libFix.Physical.Height.ToString;
    Edit3.Text := libFix.Physical.Depth.ToString;
    Edit4.Text := libFix.Physical.Weight;
    Edit5.Text := libFix.Physical.Power.ToString;
    Edit9.Text := libFix.Physical.Bulb;
    Edit6.Text := libFix.Physical.Lumens.ToString;
    CBConnector.Text := libFix.Physical.Connector;
    CBLens.Text := libFix.Physical.Lens;
    FSE2.Value := libFix.Physical.LensMinDegree;
    FSE3.Value := libFix.Physical.LensMaxDegree;

    // Dip switchs
    RadioButton2.Checked := Length(libFix.DipSwitchs.Functions) > 0;
    PanelDipSwitch.Visible := RadioButton2.Checked;
    if RadioButton2.Checked then begin
      FrameViewDipSwitchs1.OnIsUp := libFix.DipSwitchs.OnIsUp;
      FrameViewDipSwitchs1.MSBIsLeft := libFix.DipSwitchs.MSBIsLeft;
      FrameViewDipSwitchs1.Count := Length(libFix.DipSwitchs.Functions);
      for i:=0 to FrameViewDipSwitchs1.Count-1 do
        FrameViewDipSwitchs1.SetSwitchFunction(i, libFix.DipSwitchs.Functions[i]);
    end;

    // Available channels
    FExistingChannels := NIL;
    SetLength(FExistingChannels, Length(libFix.AvailableChannels));
    for i:=0 to High(FExistingChannels) do
      libFix.AvailableChannels[i].CopyTo(FExistingChannels[i]);

    // Modes
    ModeFrames := NIL;
    for i:=0 to High(libFix.Modes) do begin
      SetLength(ModeFrames, Length(ModeFrames)+1);
      DoCreateFrame(i, @libFix.Modes[i]);
    end;
  finally
    FInitializing := False;
  end;
end;

procedure TFormFixtureWizard.CBConnectorChange(Sender: TObject);
var s: string;
  flag: boolean;
  i: integer;
begin
  if Sender = Edit4 then begin
    s := Edit4.Text;
    flag := True;
    if s <> '' then
      for i:=1 to Length(s) do
        flag := flag and (s[i] in ['0'..'9','.',',']);
    if not flag then Edit4.Color := clRed
      else Edit4.Color := clDefault;
  end;

  Modified := True;

  PhysicalDataAreOK;
end;

procedure TFormFixtureWizard.BSaveClick(Sender: TObject);
begin
  DoSave;
end;

procedure TFormFixtureWizard.BAddManufacturerClick(Sender: TObject);
var F: TFormEditWebLink;
begin
  if Sender = BAddManufacturer then begin

  end;

  if Sender = BAddWebLink then begin
    F := TFormEditWebLink.Create(NIL);
    if F.ShowModal = mrOk then begin
      SetLength(FWebLinks, Length(FWebLinks)+1);
      FWebLinks[High(FWebLinks)].LinkType := F.LinkType;
      FWebLinks[High(FWebLinks)].Url := F.Url;
      LBWebLink.ItemIndex := LBWebLink.Items.Add(F.LinkType);
      Modified := True;
    end;
    F.Free;
  end;

  if Sender = BDeleteWebLink then begin
    if LBWebLink.ItemIndex = -1 then exit;
    if AskConfirmation(SAskDeleteThisLink, SDelete, SCancel, mtConfirmation) <> mrOk then exit;
    Delete(FWebLinks, LBWebLink.ItemIndex, 1);
    LBWebLink.Items.Delete(LBWebLink.ItemIndex);
    Modified := True;
  end;
end;

procedure TFormFixtureWizard.CBManufacturersChange(Sender: TObject);
begin
  Label7.Caption := FormatFixtureNameToFilename(Edit7.Text);
  if not FEditingExistingFixture and FileExists(GetOutputFilename) then Panel1.Tag := 1
    else Panel1.Tag := 0;

  Modified := True;
end;

procedure TFormFixtureWizard.EditAuthorsUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
var flag: boolean;
  c: Char;
begin
  flag := Length(UTF8Key) = 1;
  if flag then begin
    c := UTF8Key[1];
    flag := (c in ['a'..'z', 'A'..'Z', '0'..'9', ',', '-', '_', ' ', chr(VK_BACK)]);
  end;
  if not flag then UTF8Key := '';
end;

procedure TFormFixtureWizard.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if Modified and GeneralDataAreOK and PhysicalDataAreOK and ModesAndChannelsDataAreOK then begin
    case AskConfirmation(SSaveTheChangeBeforeQuit, SOk, SNo, SCancel, mtConfirmation) of
      mrYes: begin
        Dosave;
        CanClose := True;
      end;

      mrNo: CanClose := True;

      mrCancel: CanClose := False;
    end;
  end;
end;

procedure TFormFixtureWizard.FormCreate(Sender: TObject);
begin
  FInitializing := True;
  // Fill combobox with existing manufacturers in dmx library
  FManufacturers.Load;
  FManufacturers.FillComboBoxWithName(CBManufacturers);

  CheckedLabelManager := TCheckedLabelManager.Create;
  CheckedLabelManager.CaptureLabelClick(Label14);
  CheckedLabelManager.CaptureLabelClick(Label9);

  NoteBookManager := TNoteBookManager.Create(NB1);
  NoteBookManager.LinkButtonToPage(BGeneral, PageGeneral);
  NoteBookManager.LinkButtonToPage(BPhysical, PagePhysical);
  NoteBookManager.LinkButtonToPage(BModes, PageModes);
  NoteBookManager.LinkButtonToPage(BCheckAndSave, PageFinish);
  NoteBookManager.SetDeactivatedColors(PageGeneral.Color, Label19.Font.Color);
  NoteBookManager.ActivePage(PageGeneral);
  NoteBookManager.OnSelectionChange := @ProcessPageChangeEvent;

{  FrameViewDMXFixtureChannels1 := TFrameViewDMXFixtureChannels.Create(Self);
  FrameViewDMXFixtureChannels1.Parent := Panel1;
  FrameViewDMXFixtureChannels1.Align := alClient;
  FrameViewDMXFixtureChannels1.EditionEnabled := TRUE;
  FrameViewDMXFixtureChannels1.SelectionEnabled := TRUE;
  FrameViewDMXFixtureChannels1.OnSelectionChange := @ProcessChannelSelectionChangeEvent;  }


{  FrameEditString1 := TFrameEditString.Create(Self);
  FrameEditString1.Name := 'FrameEditString1';
  FrameEditString1.Parent := Panel2;
  FrameEditString1.Align := alClient;
  FrameEditString1.ModeFileName; // ModeNoSpecialChar;
  FrameEditString1.Title := SNameOfTheFixture;
  FrameEditString1.FontHeight := 20;
  FrameEditString1.Panel1.Color := Panel2.Color;
  FrameEditString1.Edit1.CharCase := ecLowerCase;
  FrameEditString1.OnTextChange := @CBManufacturersChange;  }

  FrameViewDipSwitchs1:=TFrameViewDipSwitchs.Create(Self);
  FrameViewDipSwitchs1.Parent:=Panel5;
  FrameViewDipSwitchs1.Align:=alClient;
  FrameViewDipSwitchs1.Editable:=TRUE;

  // default value
  Edit1.Text := '0';
  Edit2.Text := '0';
  Edit3.Text := '0';
  Edit4.Text := '0';
  Edit6.Text := '0';

  DataModule1.ImageList1.GetBitmap(0, Image4.Picture.Bitmap);
  SVGFileToTImage(Image4, GetAppImagesFolder+'DlgWarning.svg');
  SVGFileToTImage(Image5, GetAppImagesFolder+'DlgWarning.svg');

  // manual translations
  Label24.Caption := SThisFilenamAlreadyExists;
  Label27.Caption := SThisFilenamAlreadyExists;
  FInitializing := False;
end;

procedure TFormFixtureWizard.FormDestroy(Sender: TObject);
begin
  FreeAndNil(CheckedLabelManager);
  FreeAndNil(NoteBookManager);
end;

end.

