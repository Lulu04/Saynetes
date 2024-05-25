unit frame_viewmodeitem;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Graphics, Buttons,
  u_list_dmxuniverse, u_common;

type

  { TFrameViewModeItem }

  TFrameViewModeItem = class(TFrame)
    BEditSubChannel: TSpeedButton;
    Image1: TImage;
    Image2: TImage;
    Label4: TLabel;
    Label5: TLabel;
    Panel1: TPanel;
    procedure BEditSubChannelClick(Sender: TObject);
    procedure BEditSubChannelMouseLeave(Sender: TObject);
    procedure Panel1MouseEnter(Sender: TObject);
    procedure Panel1MouseLeave(Sender: TObject);
  private
    class var FCounterForName: integer;
  private
    FExistingChannels: PFixLibAvailableChannels;
    FIndexInMode: integer;
    FPackedName: string;
    FIsSwitchingChannel: boolean;
    function GetChanName: string;
    procedure SetChanName(AValue: string);
    procedure SetIndexInMode(AValue: integer);
  private
    procedure ProcessSubChannelMouseEnterEvent(Sender: TObject);
    procedure ProcessSubChannelMouseLeaveEvent(Sender: TObject);
    procedure SetImage(aTarget: TImage; aImageIndexInILChannelType: integer);
  public
    // aOldName is the name (or virtual name) of the channel
    // aNewName is the new name or the packed virtualName:SubChannel1;SubChannel2;...
    procedure ReplaceChannelName(const aOldName, aNewName: string);
  public
    constructor Create(TheOwner: TComponent); override;

    procedure Init(aIndex: integer; const aChanName: string);
    property ExistingChannels: PFixLibAvailableChannels read FExistingChannels write FExistingChannels;

    property IsSwitchingChannel: boolean read FIsSwitchingChannel write FIsSwitchingChannel;
    property IndexInMode: integer read FIndexInMode write SetIndexInMode;
    // the name (or virtual name) of the channel
    property ChanName: string read GetChanName write SetChanName;

    // returns the name for a normal channel or the packed name for a switching channel
    property PackedName: string read FPackedName;
  end;

implementation

uses u_datamodule, u_utils, u_helper, u_resource_string, form_definenewchannel,
  u_editfixturewizard, frame_editmode;

{$R *.lfm}

function ParentForm(aFrame: TFrameViewModeItem): TFormFixtureWizard;
var o: TWinControl;
begin
  o := aFrame;
  repeat
    o := o.Parent;
  until o is TFormFixtureWizard;
  Result := TFormFixtureWizard(o);
end;

function ParentEditMode(aFrame: TFrameViewModeItem): TFrameEditMode;
var o: TWinControl;
begin
  o := aFrame;
  repeat
    o := o.Parent;
  until o is TFrameEditMode;
  Result := TFrameEditMode(o);
end;

{ TFrameViewModeItem }

procedure TFrameViewModeItem.SetIndexInMode(AValue: integer);
begin
  FIndexInMode := AValue;
  Label4.Caption := (FIndexInMode+1).ToString+'.';

  if Odd(FIndexInMode) then Panel1.Color := PercentColor(Color, -0.1)
    else Panel1.Color := PercentColor(Color, 0.1);

  Tag := AValue;
end;

procedure TFrameViewModeItem.ProcessSubChannelMouseEnterEvent(Sender: TObject);
begin
  BEditSubChannel.Visible := True;
  BEditSubChannel.Top := TLabel(Sender).Top;
  BEditSubChannel.Left := TLabel(Sender).Left + TLabel(Sender).Width;
  BEditSubChannel.Tag := TLabel(Sender).Tag;
end;

procedure TFrameViewModeItem.ProcessSubChannelMouseLeaveEvent(Sender: TObject);
var p: TPoint;
begin
  p := BEditSubChannel.ScreenToClient(Mouse.CursorPos);
  if BEditSubChannel.ClientRect.Contains(p) then exit;

  BEditSubChannel.Visible := False;
end;

procedure TFrameViewModeItem.SetImage(aTarget: TImage; aImageIndexInILChannelType: integer);
var ima: TBitmap;
begin
  ima := TBitmap.Create;
  DataModule1.ILChannelType.GetBitmap(aImageIndexInILChannelType, ima);
  aTarget.Picture.Bitmap.Assign(ima);
  ima.Free;
end;

procedure TFrameViewModeItem.ReplaceChannelName(const aOldName, aNewName: string);
begin
  if ChanName = aOldName then SetChanName(aNewName);
end;

procedure TFrameViewModeItem.Panel1MouseEnter(Sender: TObject);
begin
  OnMouseEnter(Self);
end;

procedure TFrameViewModeItem.BEditSubChannelMouseLeave(Sender: TObject);
begin
  BEditSubChannel.Visible := False;
end;

procedure TFrameViewModeItem.BEditSubChannelClick(Sender: TObject);
var F: TFormDefineNewChannel;
  i: integer;
  con: TControl;
  oldName: string;
begin
  // retrieve the label with the channel name to edit
  con := Panel1.ControlAtPos(Point(BEditSubChannel.Left-1, BEditSubChannel.Top), True);
  if (con = NIL) or not (con is TLabel) then exit;

  oldName := TLabel(con).Caption;
  i := FExistingChannels^.NameToIndex(oldName);
  if i = -1 then exit;

  F := TFormDefineNewChannel.Create(NIL);
  F.ExistingChannel := FExistingChannels;

  F.EditExistingChannel(@FExistingChannels^[i]);
  try
    if F.ShowModal = mrOk then begin
      // replace data in the channel
      FExistingChannels^[i].LoadFromString(F.GetData);
      // replace new name in all frame (Modes)
      ParentForm(Self).ReplaceChannelNameInAllFrames(oldName, FExistingChannels^[i].NameID);
      ParentEditMode(Self).Modified := True;
    end;
  finally
    F.Free;
  end;
end;

procedure TFrameViewModeItem.Panel1MouseLeave(Sender: TObject);
begin
  OnMouseLeave(Self);
end;

function TFrameViewModeItem.GetChanName: string;
begin
  Result := Label5.Caption;
end;

procedure TFrameViewModeItem.SetChanName(AValue: string);
var i: integer;
begin
  // clear all sub-channels objects gui
  for i:=Panel1.ControlCount-1 downto 0 do begin
    if Panel1.Controls[i].Name.StartsWith('MyImage', False) or
       Panel1.Controls[i].Name.StartsWith('MyLabel', False) or
       Panel1.Controls[i].Name.StartsWith('MyOrLabel', False) then
         Panel1.Controls[i].Free;
  end;

  // then reconstruct
  Init(IndexInMode, AValue);
end;

constructor TFrameViewModeItem.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Image1.Width := DataModule1.ILChannelType.Width;
  Image1.Height := DataModule1.ILChannelType.Height;
  Image2.Width := DataModule1.ILChannelType.Width;
  Image2.Height := DataModule1.ILChannelType.Height;

  inc(FCounterForName);
  Name := 'MyFrame'+FCounterForName.ToString;

  BEditSubChannel.Hint := SEdit;
end;

procedure TFrameViewModeItem.Init(aIndex: integer; const aChanName: string);
var virtualName: string;
  subChannels: TStringArray;
  i, j, xx, yy: integer;
  p: PFixLibAvailableChannel;
  s: string;
  tima: TImage;
begin
  IndexInMode := aIndex;
  FPackedName := aChanName;

  SetImage(Image1, Ord(High(TChannelType))+1);

  if TrySplitVirtual(aChanName, virtualName, subChannels) then begin
    // it's switching channel
    FIsSwitchingChannel := True;
    Label5.Caption := virtualName;
    yy := Label5.Top + Label5.Height + ScaleDesignToForm(3);
    for i:=0 to High(subChannels) do begin
      p := FExistingChannels^.GetChannelsByName(subChannels[i]);
      j := FExistingChannels^.NameToIndex(subChannels[i]);

      s := subChannels[i];
      if p = NIL then s := s + ' << '+SNotFound;
      xx := Label5.Left;
      tima := TImage.Create(Self);
      with tima do begin
        Name := 'MyImage'+i.ToString;
        Parent := Panel1;
        SetBounds(xx, yy, Image1.Width, image1.Height);
        if p <> NIL then SetImage(tima, Ord(p^.ChanType));
        Tag := j;
      end;
      xx := xx + Image1.Width + ScaleDesignToForm(5);

      with TLabel.Create(Self) do begin
        Name := 'MyLabel'+i.ToString;
        Parent := Panel1;
        Left := xx;
        Top := yy;
        Font.Height := Label5.Font.Height;
        Caption := s;
        OnMouseEnter := @ProcessSubChannelMouseEnterEvent;
        OnMouseLeave := @ProcessSubChannelMouseLeaveEvent;
        Tag := j;
      end;

      // label 'or'
      if i > 0 then
        with TLabel.Create(Self) do begin
          Name := 'MyOrLabel'+i.ToString;
          Parent := Panel1;
          //Font.Height := Label5.Font.Height;
          Caption := SOr;
          Left := Label5.Left - Canvas.GetTextWidth(SOr) - ScaleDesignToForm(10);
          Top := yy - Round(Canvas.GetTextHeight(SOr) * 0.7);
          Font.Color := PercentColor(Label5.Font.Color, -0.3);
          Tag := j;
        end;

      yy := yy + Label5.Height + ScaleDesignToForm(5);
    end;

    ClientHeight := yy; // ClientHeight + FChanLabel[i].Height;
  end else begin
    // it's normal single channel
    FIsSwitchingChannel := False;
    p := FExistingChannels^.GetChannelsByName(aChanName);
    s := aChanName;
    if p = NIL then s := s + ' << '+SNotFound;
    Label5.Caption := s;
    if p <> NIL then begin
      SetImage(Image1, Ord(p^.ChanType));
      // check if this channel have switch descriptor
      if p^.HaveSwitchDescriptor then SetImage(Image2, Ord(High(TChannelType))+2);
    end;
  end;
end;

end.

