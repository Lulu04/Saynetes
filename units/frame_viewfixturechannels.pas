unit frame_viewfixturechannels;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, LCLType, Menus, LCLTranslator,
  u_list_dmxuniverse, u_common, u_dmx_util;

type

  { TFrameViewDMXFixtureChannels }

  TFrameViewDMXFixtureChannels = class(TFrame)
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    MIAddRange: TMenuItem;
    MIDeleteRange: TMenuItem;
    MIModifyRange: TMenuItem;
    MI_RenameChannel: TMenuItem;
    PopupMenu1: TPopupMenu;
    TV: TTreeView;
    procedure MenuItem10Click(Sender: TObject);
    procedure MenuItem11Click(Sender: TObject);
    procedure MenuItem12Click(Sender: TObject);
    procedure MenuItem13Click(Sender: TObject);
    procedure MenuItem14Click(Sender: TObject);
    procedure MenuItem15Click(Sender: TObject);
    procedure MenuItem16Click(Sender: TObject);
    procedure MenuItem17Click(Sender: TObject);
    procedure MenuItem18Click(Sender: TObject);
    procedure MenuItem19Click(Sender: TObject);
    procedure MenuItem20Click(Sender: TObject);
    procedure MenuItem21Click(Sender: TObject);
    procedure MenuItem22Click(Sender: TObject);
    procedure MenuItem23Click(Sender: TObject);
    procedure MenuItem24Click(Sender: TObject);
    procedure MenuItem25Click(Sender: TObject);
    procedure MenuItem26Click(Sender: TObject);
    procedure MenuItem27Click(Sender: TObject);
    procedure MenuItem28Click(Sender: TObject);
    procedure MenuItem29Click(Sender: TObject);
    procedure MenuItem30Click(Sender: TObject);
    procedure MenuItem31Click(Sender: TObject);
    procedure MenuItem32Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
    procedure MIAddRangeClick(Sender: TObject);
    procedure MIDeleteRangeClick(Sender: TObject);
    procedure MIModifyRangeClick(Sender: TObject);
    procedure MI_RenameChannelClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure TVMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TVSelectionChanged(Sender: TObject);
  private
    FEditionEnabled: boolean;
    FOnSelectionChange: TNotifyEvent;
    FReady: boolean;
    FFixtureType: TFixtureType;
    FPower: integer;
    FFixtureName: string;
    FChannelCount: integer;
    FSelectionEnabled: boolean;
    function GetSelected: TTreeNode;
    procedure SetEditionEnabled(AValue: boolean);
    procedure SetSelectionEnabled(AValue: boolean);
  public
    procedure EraseBackground({%H-}DC: HDC); override;

    procedure Clear;
    procedure ShowFixture( const aFileName: string );
    // fills a TLibraryFixture from data read and modified from method  ShowFixture
    function ToLibraryFixture: TLibraryFixture;

    // fills a TLibraryFixture.Channels from the current editing
    procedure FillChannels(var Channels:ArrayOfLibraryFixtureChannel);

    procedure MoveSelectedUp;
    procedure MoveSelectedDown;

    // check if there is range error on each channel.
    // return TRUE if an error is found. ErrorMess is the formatted error for the user
    function CheckError(out ErrorMess: string): boolean;

    function SelectedIsChannelName: boolean;
    function SelectedIsRange: boolean;


    property Ready: boolean read FReady; // true after a successfull call to method ShowFixture
    property FixtureType: TFixtureType read FFixtureType write FFixturetype;
    property Power: integer read FPower;
    property FixtureName: string read FFixtureName;
    property ChannelCount: integer read FChannelCount write FChannelCount;

    property EditionEnabled: boolean read FEditionEnabled write SetEditionEnabled;
    property SelectionEnabled: boolean read FSelectionEnabled write SetSelectionEnabled;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
    property Selected: TTreeNode read GetSelected;
  end;

implementation

uses u_resource_string, u_dmxlib_inputrange, u_userdialogs, u_utils, Dialogs;

{$R *.lfm}

{ TFrameViewDMXFixtureChannels }

procedure TFrameViewDMXFixtureChannels.TVSelectionChanged(Sender: TObject);
begin
  if not FSelectionEnabled
    then TV.Selected:=NIL
    else if FOnSelectionChange<>NIL
           then FOnSelectionChange(Self);
end;

procedure TFrameViewDMXFixtureChannels.MenuItem12Click(Sender: TObject);
begin
  if TV.Selected=NIL then exit;
  TV.Selected.ImageIndex:=ord(ctCONFIG);
  TV.Selected.Text:='Config';
end;

procedure TFrameViewDMXFixtureChannels.MenuItem13Click(Sender: TObject);
begin
  if TV.Selected=NIL then exit;
  TV.Selected.ImageIndex:=ord(ctPAN);
  TV.Selected.Text:='Pan';
end;

procedure TFrameViewDMXFixtureChannels.MenuItem14Click(Sender: TObject);
begin
  if TV.Selected=NIL then exit;
  TV.Selected.ImageIndex:=ord(ctTILT);
  TV.Selected.Text:='Tilt';
end;

procedure TFrameViewDMXFixtureChannels.MenuItem15Click(Sender: TObject);
begin
  if TV.Selected=NIL then exit;
  TV.Selected.ImageIndex:=ord(ctPANTILTSPEED);
  TV.Selected.Text:='Speed Pan/Tilt';
end;

procedure TFrameViewDMXFixtureChannels.MenuItem16Click(Sender: TObject);
begin
  if TV.Selected=NIL then exit;
  TV.Selected.ImageIndex:=ord(ctGOBO);
  TV.Selected.Text:='GOBO';
end;

procedure TFrameViewDMXFixtureChannels.MenuItem17Click(Sender: TObject);
begin
  if TV.Selected=NIL then exit;
  TV.Selected.ImageIndex:=ord(ctGOBOROTATION);
  TV.Selected.Text:='GOBO Rotation';
end;

procedure TFrameViewDMXFixtureChannels.MenuItem18Click(Sender: TObject);
begin
  if TV.Selected=NIL then exit;
  TV.Selected.ImageIndex:=ord(ctCOLORCHOICE);
  TV.Selected.Text:='Color changer';
end;

procedure TFrameViewDMXFixtureChannels.MenuItem19Click(Sender: TObject);
begin
  if TV.Selected=NIL then exit;
  TV.Selected.ImageIndex:=ord(ctUV);
  TV.Selected.Text:='Ultraviolet';
end;

procedure TFrameViewDMXFixtureChannels.MenuItem20Click(Sender: TObject);
begin
  if TV.Selected=NIL then exit;
  TV.Selected.ImageIndex:=ord(ctSPEED);
  TV.Selected.Text:='Speed';
end;

procedure TFrameViewDMXFixtureChannels.MenuItem21Click(Sender: TObject);
begin
  if TV.Selected=NIL then exit;
  TV.Selected.ImageIndex:=ord(ctSPEED);
  TV.Selected.Text:='Speed';
end;

procedure TFrameViewDMXFixtureChannels.MenuItem22Click(Sender: TObject);
begin
  if TV.Selected=NIL then exit;
  TV.Selected.ImageIndex:=ord(ctNOFUNCTION);
  TV.Selected.Text:='No function';
end;

procedure TFrameViewDMXFixtureChannels.MenuItem23Click(Sender: TObject);
begin
  if TV.Selected=NIL then exit;
  TV.Selected.ImageIndex:=ord(ctCYAN);
  TV.Selected.Text:='Cyan';
end;

procedure TFrameViewDMXFixtureChannels.MenuItem24Click(Sender: TObject);
begin
  if TV.Selected=NIL then exit;
  TV.Selected.ImageIndex:=ord(ctMAGENTA);
  TV.Selected.Text:='Magenta';
end;

procedure TFrameViewDMXFixtureChannels.MenuItem25Click(Sender: TObject);
begin
  if TV.Selected=NIL then exit;
  TV.Selected.ImageIndex:=ord(ctYELLOW);
  TV.Selected.Text:='Yellow';
end;

procedure TFrameViewDMXFixtureChannels.MenuItem26Click(Sender: TObject);
begin
  if TV.Selected=NIL then exit;
  TV.Selected.ImageIndex:=ord(ctLIME);
  TV.Selected.Text:='Lime';
end;

procedure TFrameViewDMXFixtureChannels.MenuItem27Click(Sender: TObject);
begin
  if TV.Selected=NIL then exit;
  TV.Selected.ImageIndex:=ord(ctINDIGO);
  TV.Selected.Text:='Indigo';
end;

procedure TFrameViewDMXFixtureChannels.MenuItem28Click(Sender: TObject);
begin
  if TV.Selected=NIL then exit;
  TV.Selected.ImageIndex:=ord(ctWARMWHITE);
  TV.Selected.Text:='Warm white';
end;

procedure TFrameViewDMXFixtureChannels.MenuItem29Click(Sender: TObject);
begin
  if TV.Selected=NIL then exit;
  TV.Selected.ImageIndex:=ord(ctCOLDWHITE);
  TV.Selected.Text:='Cold white';
end;

procedure TFrameViewDMXFixtureChannels.MenuItem30Click(Sender: TObject);
begin
  if TV.Selected=NIL then exit;
  TV.Selected.ImageIndex:=ord(ctIRIS);
  TV.Selected.Text:='Iris';
end;

procedure TFrameViewDMXFixtureChannels.MenuItem31Click(Sender: TObject);
begin
  if TV.Selected=NIL then exit;
  TV.Selected.ImageIndex:=ord(ctBLADEINSERTION);
  TV.Selected.Text:='Blade insertion';
end;

procedure TFrameViewDMXFixtureChannels.MenuItem32Click(Sender: TObject);
begin
  if TV.Selected=NIL then exit;
  TV.Selected.ImageIndex:=ord(ctCOLORTEMPERATURE);
  TV.Selected.Text:='Color temperature';
end;

procedure TFrameViewDMXFixtureChannels.MenuItem4Click(Sender: TObject);
begin
  if TV.Selected=NIL then exit;
  TV.Selected.ImageIndex:=ord(ctRED);
  TV.Selected.Text:='Red';
end;

procedure TFrameViewDMXFixtureChannels.MenuItem5Click(Sender: TObject);
begin
  if TV.Selected=NIL then exit;
  TV.Selected.ImageIndex:=ord(ctGREEN);
  TV.Selected.Text:='Green';
end;

procedure TFrameViewDMXFixtureChannels.MenuItem6Click(Sender: TObject);
begin
  if TV.Selected=NIL then exit;
  TV.Selected.ImageIndex:=ord(ctBLUE);
  TV.Selected.Text:='Blue';
end;

procedure TFrameViewDMXFixtureChannels.MenuItem7Click(Sender: TObject);
begin
  if TV.Selected=NIL then exit;
  TV.Selected.ImageIndex:=ord(ctWHITE);
  TV.Selected.Text:='White';
end;

procedure TFrameViewDMXFixtureChannels.MenuItem8Click(Sender: TObject);
begin
  if TV.Selected=NIL then exit;
  TV.Selected.ImageIndex:=ord(ctMASTERDIMMER);
  TV.Selected.Text:='Master Dimmer';
end;

procedure TFrameViewDMXFixtureChannels.MenuItem9Click(Sender: TObject);
begin
  if TV.Selected=NIL then exit;
  TV.Selected.ImageIndex:=ord(ctAMBER);
  TV.Selected.Text:='Amber';
end;

procedure TFrameViewDMXFixtureChannels.MIAddRangeClick(Sender: TObject);
var txt: string;
  n: TTreeNode;
  F: TFormInputDMXRange;
begin
  if TV.Selected=NIL then exit;

  F:=TFormInputDMXRange.Create(NIL);
  if F.ShowModal = mrOk then begin
         if TV.Selected.Level > 0
           then n := TV.Selected.Parent
           else n := TV.Selected;
         txt := EncodeDMXChannelRange(F.RangeBegin, F.RangeEnd, F.Description);
         with TV.Items.AddChild( n, txt) do begin
           MakeVisible;
           Selected := TRUE;
         end;
  end;
  F.Free;
end;

procedure TFrameViewDMXFixtureChannels.MIDeleteRangeClick(Sender: TObject);
begin
  if Selected=NIL then exit;
  if SelectedIsRange
    then Selected.Delete;
end;

procedure TFrameViewDMXFixtureChannels.MIModifyRangeClick(Sender: TObject);
var n: TTreeNode;
   F: TFormInputDMXRange;
   be, en: integer;
   des: string;
begin
  n:=Selected;
  if n=NIL then exit;
  if n.Level=0 then exit;

  DecodeDMXChannelRange( n.Text, be, en, des);

  F:=TFormInputDMXRange.Create(NIL);
  F.RangeBegin := be;
  F.RangeEnd := en;
  F.Description := des;
  if F.ShowModal = mrOk
     then n.Text:=EncodeDMXChannelRange(F.RangeBegin, F.RangeEnd, F.Description);
  F.Free;
end;

procedure TFrameViewDMXFixtureChannels.MI_RenameChannelClick(Sender: TObject);
var n: TTreeNode;
  nam: string;
begin
  n:=Selected;
  if n=NIL then exit;
  if n.Level>0
    then n:=n.Parent;
  if n=NIL then exit;

  nam:=n.Text;
  if UserInputNoSpecialChar(SNewName, SOk, SCancel, nam, mtConfirmation, FALSE)=mrOk
    then n.Text:=nam;
end;

procedure TFrameViewDMXFixtureChannels.PopupMenu1Popup(Sender: TObject);
begin
  MenuItem10.Caption := SDimmer;
end;

procedure TFrameViewDMXFixtureChannels.TVMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FEditionEnabled and (Button = mbRight) and (TV.Selected <> NIL) then begin
    MenuItem3.Enabled := TV.Selected.Level = 0;
    MI_RenameChannel.Enabled := MenuItem3.Enabled;

    MIAddRange.Enabled := TV.Selected.Level = 1;
    MIModifyRange.Enabled := MIAddRange.Enabled;
    MIDeleteRange.Enabled := MIAddRange.Enabled;
    PopupMenu1.PopUp;
  end;
end;

procedure TFrameViewDMXFixtureChannels.MenuItem10Click(Sender: TObject);
begin
  if TV.Selected=NIL then exit;
  TV.Selected.ImageIndex:=ord(ctDIMMER);
  TV.Selected.Text:='Dimmer';
end;

procedure TFrameViewDMXFixtureChannels.MenuItem11Click(Sender: TObject);
begin
  if TV.Selected=NIL then exit;
  TV.Selected.ImageIndex:=ord(ctSTROBE);
  TV.Selected.Text:='Strobe';
end;

procedure TFrameViewDMXFixtureChannels.SetSelectionEnabled(AValue: boolean);
begin
  if FSelectionEnabled=AValue then Exit;
  FSelectionEnabled:=AValue;
end;

procedure TFrameViewDMXFixtureChannels.SetEditionEnabled(AValue: boolean);
begin
  if FEditionEnabled=AValue then Exit;
  FEditionEnabled:=AValue;
{  if FEditionEnabled then begin
    TV.PopupMenu:=PopupMenu1;
  end else begin
    TV.PopupMenu:=NIL;
  end; }
end;

function TFrameViewDMXFixtureChannels.GetSelected: TTreeNode;
begin
  Result:=TV.Selected;
end;

procedure TFrameViewDMXFixtureChannels.EraseBackground(DC: HDC);
begin
// do nothing
end;

procedure TFrameViewDMXFixtureChannels.Clear;
begin
  TV.Items.Clear;
end;
procedure TFrameViewDMXFixtureChannels.ShowFixture(const aFileName: string);
var i, j: integer;
  n: TTreeNode;
  lf: TLibraryFixture;
begin
  FReady := FALSE;
  TV.Items.Clear;
  if aFileName = '' then exit;

  if not lf.LoadFromFile(aFileName) then exit;

  FFixtureType := lf.FixtureType;
  FPower := lf.Power; // power
  FFixtureName := lf.Name;            // name
  FChannelCount:=Length(lf.Channels); // channel count
  for i:=0 to High(lf.Channels) do begin
      n := TV.Items.Add( TV.Items.GetFirstNode, lf.Channels[i].Name ); // channel name
      //if channel name is a known word, we translate it in the current app language
      case LowerCase(n.Text) of
        'red': n.Text:=SChannelRed;
        'green': n.Text:=SChannelGreen;
        'blue': n.Text:=SChannelBlue;
        'white': n.Text:=SChannelWhite;
        'amber': n.Text:=SChannelAmber;
        'ultraviolet': n.Text:=SChannelUV;
        'master dimmer': n.Text:=SChannelMasterDimmer;
        'dimmer': n.Text:=SDimmer;
        'config': n.Text:=SChannelConfig;
        'strobe': n.Text:=SChannelStrobe;
        'pan': n.Text:=SChannelPan;
        'tilt': n.Text:=SChannelTilt;
        'speed pan/tilt': n.Text:=SChannelSpeedPanTilt;
        'gobo': n.Text:=SChannelGobo;
        'rotation gobo': n.Text:=SChannelRotationGobo;
        'color changer': n.Text:=SChannelColorChanger;
        'speed': n.Text:=SChannelSpeed;
      end;//case

      n.ImageIndex:=Ord(lf.Channels[i].ChannelType); // image associated with channel type
      for j:=0 to High(lf.Channels[i].Ranges) do
        TV.Items.AddChild( n, lf.Channels[i].Ranges[j].Encode );
  end;
  TV.FullExpand;
  FReady:=TRUE;
end;

function TFrameViewDMXFixtureChannels.ToLibraryFixture: TLibraryFixture;
begin
  Result.Name:=self.FixtureName;
  Result.FixtureType:=self.FixtureType;
  Result.Power:=Power;
  FillChannels(Result.Channels);
end;

procedure TFrameViewDMXFixtureChannels.FillChannels( var Channels: ArrayOfLibraryFixtureChannel);
var i, j: integer;
  n: TTreeNode;
begin
  SetLength(Channels, TV.Items.TopLvlCount);
  for i:=0 to TV.Items.TopLvlCount-1 do begin
    n := TV.Items.TopLvlItems[i];
    Channels[i].ChannelType:=TChannelType(n.ImageIndex);
    Channels[i].Name:=n.Text;
    SetLength(Channels[i].Ranges, n.Count);
    for j:=0 to n.Count-1 do
      Channels[i].Ranges[j].Decode(n.Items[j].Text);
  end;
end;

procedure TFrameViewDMXFixtureChannels.MoveSelectedUp;
var n: TTreeNode;
begin
  n:=TV.Selected;
  if n=NIL then exit;
  if n.Level=0 then exit;
  if n.Index=0 then exit;
  n.MoveTo(n.GetPrev, naInsert);
end;

procedure TFrameViewDMXFixtureChannels.MoveSelectedDown;
var n: TTreeNode;
begin
  n:=TV.Selected;
  if n=NIL then exit;
  if n.Level=0 then exit;
  if n.Index=n.Parent.Count-1 then exit;
  n.MoveTo(n.GetNext, naInsertBehind);
end;

function TFrameViewDMXFixtureChannels.CheckError(out ErrorMess: string): boolean;
var i, j: integer;
    n, r: TTreeNode;
    be, en, startvalue: integer;
    des: string;
begin
  Result:=FALSE;
  ErrorMess:='';

  // check if there is at least one channel
  if TV.Items.Count=0 then begin
    ErrorMess:=SAFixtureMustHaveAtLeastOneChannel;
    Result:=TRUE;
    exit;
  end;

  for i:=0 to TV.Items.TopLvlCount-1 do begin
    n := TV.Items.TopLvlItems[i];
    // check if the channels name don't have forbidden characters
    if not StringIsValid(n.Text) then begin
      ErrorMess:=n.Text+lineending+
                 SPleaseNoSpaceOrSpecialCharacters+FORBIDENCHARS;
      TV.Selected:=n;
      TV.MakeSelectionVisible;
      Result:=TRUE;
      exit;
    end;
    //check if channel have at least one range
    if n.Count<1 then begin
      ErrorMess:=n.Text+SMustHaveAtLeastOneRange+lineending+
                 SAsReminderTheRangesMustCoverTheInterval;
      TV.Selected:=n;
      TV.MakeSelectionVisible;
      Result:=TRUE;
      exit;
    end;
    //check if the first channel range begin at 0
    DecodeDMXChannelRange(n.Items[0].Text, be, en, des);
    if be<>0 then begin
      ErrorMess:=n.Text+SFirstRangeShouldStartFrom0+lineending+
                 SAsReminderTheRangesMustCoverTheInterval;
      TV.Selected:=n.Items[0];
      TV.MakeSelectionVisible;
      Result:=TRUE;
      exit;
    end;
    //check if the last channel range end at 0
    DecodeDMXChannelRange(n.Items[n.Count-1].Text, be, en, des);
    if en<>255 then begin
      ErrorMess:=n.Text+SLastRangeShouldEndOn255+lineending+
                 SAsReminderTheRangesMustCoverTheInterval;
      TV.Selected:=n.Items[n.Count-1];
      TV.MakeSelectionVisible;
      Result:=TRUE;
      exit;
    end;
    startvalue:=0;
    for j:=0 to n.Count-1 do begin
      r := n.Items[j];
      DecodeDMXChannelRange(r.Text, be, en, des);
      // check if be<en
      if be>en then begin
        ErrorMess:=n.Text+SStartValueMustBeGreaterThanEndValue;
        Result:=TRUE;
        TV.Selected:=r;
        TV.MakeSelectionVisible;
        exit;
      end;
      // check discontinuity
      if be<>startvalue then begin
        ErrorMess:=n.Text+SADiscontinuityAppearsOnTheRange;
        Result:=TRUE;
        TV.Selected:=r;
        TV.MakeSelectionVisible;
        exit;
      end;
      startvalue:=en+1;
    end;
    startvalue:=0;
  end;
end;

function TFrameViewDMXFixtureChannels.SelectedIsChannelName: boolean;
begin
  Result:=FALSE;
  if Selected<>NIL
    then Result := Selected.Level=0;
end;

function TFrameViewDMXFixtureChannels.SelectedIsRange: boolean;
begin
  Result:=FALSE;
  if Selected<>NIL
    then Result := Selected.Level=1;
end;


end.

