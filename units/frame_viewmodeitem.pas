unit frame_viewmodeitem;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Graphics,
  u_list_dmxuniverse, u_common;

type

  { TFrameViewModeItem }

  TFrameViewModeItem = class(TFrame)
    Image1: TImage;
    Label4: TLabel;
    Label5: TLabel;
    Panel1: TPanel;
    procedure Panel1MouseEnter(Sender: TObject);
    procedure Panel1MouseLeave(Sender: TObject);
  private
    class var FCounterForName: integer;
  private
    FExistingChannels: PFixLibAvailableChannels;
    FIndexInMode: integer;
    FIsSwitchingChannel: boolean;
    function GetChanName: string;
    procedure SetChanName(AValue: string);
    procedure SetIndexInMode(AValue: integer);

  public
    constructor Create(TheOwner: TComponent); override;

    procedure Init(aIndex: integer; const aChanName: string);
    property ExistingChannels: PFixLibAvailableChannels read FExistingChannels write FExistingChannels;

    property IsSwitchingChannel: boolean read FIsSwitchingChannel write FIsSwitchingChannel;
    property IndexInMode: integer read FIndexInMode write SetIndexInMode;
    // the name (or virtual name) of the channel
    property ChanName: string read GetChanName write SetChanName;
  end;

implementation

uses u_datamodule, u_utils, u_helper, u_resource_string;

{$R *.lfm}

{ TFrameViewModeItem }

procedure TFrameViewModeItem.SetIndexInMode(AValue: integer);
begin
  FIndexInMode := AValue;
  Label4.Caption := (FIndexInMode+1).ToString+'.';

  if Odd(FIndexInMode) then Panel1.Color := PercentColor(Color, -0.1)
    else Panel1.Color := PercentColor(Color, 0.1);

  Tag := AValue;
end;

procedure TFrameViewModeItem.Panel1MouseEnter(Sender: TObject);
begin
  OnMouseEnter(Self);
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
begin
  Label5.Caption := AValue;
end;

constructor TFrameViewModeItem.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Image1.Width := DataModule1.ILChannelType.Width;
  Image1.Height := DataModule1.ILChannelType.Height;

  inc(FCounterForName);
  Name := 'MyFrame'+FCounterForName.ToString;
end;

procedure TFrameViewModeItem.Init(aIndex: integer; const aChanName: string);
var ima: TBitmap;
  virtualName: string;
  subChannels: TStringArray;
  i, xx, yy: integer;
  p: PFixLibAvailableChannel;
  s: string;
begin
  IndexInMode := aIndex;

  ima := TBitmap.Create;
  DataModule1.ILChannelType.GetBitmap(Ord(High(TChannelType))+1, ima);
  Image1.Picture.Bitmap.Assign(ima);
  ima.Free;

  if TrySplitVirtual(aChanName, virtualName, subChannels) then begin
    // it's switching channel
    Label5.Caption := virtualName;
    yy := Label5.Top + Label5.Height + ScaleDesignToForm(3);
    for i:=0 to High(subChannels) do begin
      p := FExistingChannels^.GetChannelsByName(subChannels[i]);
      s := subChannels[i];
      if p = NIL then s := s + ' << '+SNotFound;
      xx := Label5.Left;
      with TImage.Create(Self) do begin
        Name := 'MyImage'+i.ToString;
        Parent := Panel1;
        SetBounds(xx, yy, Image1.Width, image1.Height);
        if p <> NIL then begin
          ima := TBitmap.Create;
          DataModule1.ILChannelType.GetBitmap(Ord(p^.ChanType), ima);
          Picture.Bitmap.Assign(ima);
          ima.Free;
        end;
      end;
      xx := xx + Image1.Width + ScaleDesignToForm(5);

      with TLabel.Create(Self) do begin
        Name := 'MyLabel'+i.ToString;
        Parent := Panel1;
        Left := xx;
        Top := yy;
        Font.Height := Label5.Font.Height;
        Caption := s;
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
        end;

      yy := yy + Label5.Height + ScaleDesignToForm(5);
    end;

    ClientHeight := yy; // ClientHeight + FChanLabel[i].Height;
  end else begin
    // it's normal single channel
    p := FExistingChannels^.GetChannelsByName(aChanName);
    s := aChanName;
    if p = NIL then s := s + ' << '+SNotFound;
    Label5.Caption := s;

    if p <> NIL then begin
      ima := TBitmap.Create;
      DataModule1.ILChannelType.GetBitmap(Ord(p^.ChanType), ima);
      Image1.Picture.Bitmap.Assign(ima);
      ima.Free;
    end;
  end;
end;

end.

