unit frame_viewdmxdipswitchs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, Graphics, Menus,
  BGRABitmap, BGRABitmapTypes,
  u_list_dmxuniverse, u_common;

type

  TSwitchDescriptor = record
    Area: TRect;
    SwitchFunction: TDipSwitchFunction;
  end;

  ArrayOfSwitchDescriptor = array of TSwitchDescriptor;

  { TFrameViewDipSwitchs }

  TFrameViewDipSwitchs = class(TFrame)
    MIAdressSetting: TMenuItem;
    MIMustBeOFF: TMenuItem;
    MIMustBeON: TMenuItem;
    PB1: TPaintBox;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    procedure MIAdressSettingClick(Sender: TObject);
    procedure MIMustBeOFFClick(Sender: TObject);
    procedure MIMustBeONClick(Sender: TObject);
    procedure PB1MouseDown(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure PB1MouseMove(Sender: TObject; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure PB1Paint(Sender: TObject);
    procedure PB1Resize(Sender: TObject);
  private
    procedure SetCount(AValue: integer);
  private
    FMSBIsLeft,
    FOnIsUp: boolean;
    FAlignment: TAlignment;
    procedure SetAlignment(AValue: TAlignment);
    procedure SetMSBIsLeft(AValue: boolean);
    procedure SetOnIsUp(AValue: boolean);
  private
    FEditable: boolean;
    FTemp: TBGRABitmap;
    FSwitchs: ArrayOfSwitchDescriptor;
    FAdress: integer;
    FWorkingSwitchIndex: integer;
    function AdressToBinary: string;
    function DipSwitchFunctionToBodySwitchColor(aFunction:TDipSwitchFunction): TBGRAPixel;
    function GetCount: integer;
    procedure UpdateImage;
    function SwitchIndexAtPos(X, Y: integer): integer;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure ShowAdress(aAdress: TDMXAdress; aDipSwitch: TDipSwitch);

    property Count: integer read GetCount write SetCount;
    property Switchs: ArrayOfSwitchDescriptor read FSwitchs;
    property MSBIsLeft: boolean read FMSBIsLeft write SetMSBIsLeft;
    property OnIsUp: boolean read FOnIsUp write SetOnIsUp;
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property Editable: boolean read FEditable write FEditable;
  end;

implementation
uses Math, u_resource_string, u_logfile;

{$R *.lfm}

{ TFrameViewDipSwitchs }

procedure TFrameViewDipSwitchs.SetCount(AValue: integer);
var i: integer;
begin
  AValue:=EnsureRange(AValue, 1, 11);
  SetLength(FSwitchs, AValue);
  for i:=0 to AValue-1 do FSwitchs[i].SwitchFunction:=dsfAdress;
  UpdateImage;
end;

procedure TFrameViewDipSwitchs.SetAlignment(AValue: TAlignment);
begin
  if FAlignment=AValue then Exit;
  FAlignment:=AValue;
  UpdateImage;
end;

procedure TFrameViewDipSwitchs.SetMSBIsLeft(AValue: boolean);
begin
  FMSBIsLeft:=AValue;
  UpdateImage;
end;

procedure TFrameViewDipSwitchs.SetOnIsUp(AValue: boolean);
begin
  if FOnIsUp=AValue then exit;
  FOnIsUp:=AValue;
  UpdateImage;
end;

function TFrameViewDipSwitchs.AdressToBinary: string;
var s: string;
  i: integer;
begin
  s:=BinStr(FAdress, Count);
  if MSBIsLeft
    then Result:=s
    else begin
      Result:='';
      for i:=Length(s) downto 1 do Result+=s[i];
    end;
end;

function TFrameViewDipSwitchs.DipSwitchFunctionToBodySwitchColor(aFunction:TDipSwitchFunction): TBGRAPixel;
begin
  case aFunction of
    dsfAdress: Result := BGRA(210,210,172);//243,234,109);
    dsfOn: Result := BGRA(109,243,109);
    dsfOff: Result := BGRA(243,109,109);
  end;
end;

function TFrameViewDipSwitchs.GetCount: integer;
begin
  Result:=Length(FSwitchs);
end;

procedure TFrameViewDipSwitchs.UpdateImage;
var i, itemWidth, Hmargin, xx, yy,
  yyBitNumber, firstBitNumber, stepBitNumber,
   itemHeight: integer;
  diameter: integer;
  bin: String;
begin
  FTemp.SetSize(Panel1.ClientWidth, Panel1.ClientHeight);
  FTemp.Fill(ColorToBGRA(Panel1.Color));
  if Count=0 then exit;

  FTemp.FontHeight:=Panel1.ClientHeight div 6;
  FTemp.FontStyle:=[fsBold];

  itemWidth:=FTemp.Width div Count;
  if itemWidth>38 then itemWidth:=38;
  Hmargin:=Round(itemWidth*0.1);
  itemWidth:=itemWidth-Hmargin;

  itemHeight:=Round(FTemp.Height - FTemp.FontFullHeight*2);

  case FAlignment of
    taLeftJustify: xx:=Hmargin;
    taCenter: xx:=(FTemp.Width - (Count*itemWidth + (Count-1)*Hmargin)) div 2;
    taRightJustify: xx:=FTemp.Width - Count*(itemWidth + Hmargin);
  end;

  if FONIsUp then begin
    yy:=0;
    yyBitNumber:=FTemp.Height-FTemp.FontFullHeight;
  end else begin
    yy:=FTemp.Height-FTemp.FontFullHeight;
    yyBitNumber:=0;
  end;
  if MSBIsLeft then begin
    firstBitNumber:=Count;
    stepBitNumber:=-1;
  end else begin
    firstBitNumber:=1;
    stepBitNumber:=1;
  end;

  // render 'ON'
  FTemp.TextOut(xx, yy, SOn, $00EAEAEA);

  bin:=AdressToBinary;
  diameter:=Round(itemWidth*0.7);

  for i:=0 to High(FSwitchs) do begin
    // keep switch area for later use
    FSwitchs[i].Area.TopLeft:=Point(xx, FTemp.FontFullHeight);
    FSwitchs[i].Area.BottomRight:=Point(xx+itemWidth, FTemp.FontFullHeight+itemHeight);

    // render Switch body
    FTemp.RoundRectAntialias(FSwitchs[i].Area.Left, FSwitchs[i].Area.Top,
                             FSwitchs[i].Area.BottomRight.X, FSwitchs[i].Area.BottomRight.Y,
                             itemWidth*0.2, itemWidth*0.2,
                             BGRA(26,26,26), 1.5,
                             DipSwitchFunctionToBodySwitchColor(FSwitchs[i].SwitchFunction));
    // render Switch handle
    yy:=Round(itemHeight*0.06);  // margin
    case OnIsUp of
      TRUE: if (FSwitchs[i].SwitchFunction=dsfON) or
              ((FSwitchs[i].SwitchFunction=dsfAdress)and(bin[i+1]='1'))
              then yy:=FSwitchs[i].Area.Top+diameter div 2+yy
              else yy:=FSwitchs[i].Area.Top+itemHeight-diameter div 2-yy;
      FALSE: if (FSwitchs[i].SwitchFunction=dsfON) or
              ((FSwitchs[i].SwitchFunction=dsfAdress)and(bin[i+1]='1'))
              then yy:=FSwitchs[i].Area.Top+itemHeight-diameter div 2-yy
              else yy:=FSwitchs[i].Area.Top+diameter div 2+yy;
    end;//case
    FTemp.FillEllipseAntialias(xx+itemWidth/2, yy, diameter/2, diameter/2.5, BGRA(26,26,26));

    // render bit number
    FTemp.TextOut(FSwitchs[i].Area.CenterPoint.X, yyBitNumber, firstBitNumber.ToString, ColorToBGRA($00EAEAEA), taCenter);
    firstBitNumber:=firstBitNumber+stepBitNumber;

    xx:=xx+itemWidth+Hmargin;
  end;
  PB1.Invalidate;
end;

function TFrameViewDipSwitchs.SwitchIndexAtPos(X, Y: integer): integer;
var i: integer;
begin
  for i:=0 to High(FSwitchs) do
    if FSwitchs[i].Area.Contains(Point(X,Y)) then begin
      Result:=i;
      exit;
    end;
  Result:=-1;
end;

constructor TFrameViewDipSwitchs.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FMSBIsLeft:=TRUE;
  FOnIsUp:=TRUE;
  FAlignment:=taCenter;
  FTemp:=TBGRABitmap.Create(1,1);
  Count:=9;
end;

destructor TFrameViewDipSwitchs.Destroy;
begin
  FTemp.Free;
  inherited Destroy;
end;

procedure TFrameViewDipSwitchs.ShowAdress(aAdress: TDMXAdress; aDipSwitch: TDipSwitch);
var i: integer;
begin
  FMSBIsLeft:=aDipSwitch.MSBIsLeft;
  FONIsUp:=aDipSwitch.OnIsUp;
  FAdress:=aAdress;
  SetLength(FSwitchs, Length(aDipSwitch.Functions));
  for i:=0 to High(FSwitchs) do
    FSwitchs[i].SwitchFunction:=aDipSwitch.Functions[i];
  UpdateImage;
end;

procedure TFrameViewDipSwitchs.PB1Paint(Sender: TObject);
begin
  FTemp.Draw(PB1.Canvas, 0, 0);
end;

procedure TFrameViewDipSwitchs.PB1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not FEditable then exit;
  FWorkingSwitchIndex:=SwitchIndexAtPos(X, Y);
  if ((Button=mbRight)or(Button=mbLeft)) and (FWorkingSwitchIndex<>-1) then begin
    PopupMenu1.PopUp;
  end;
end;

procedure TFrameViewDipSwitchs.MIAdressSettingClick(Sender: TObject);
begin
  if (FWorkingSwitchIndex<0) or (FWorkingSwitchIndex>=Count) then exit;
  FSwitchs[FWorkingSwitchIndex].SwitchFunction:=dsfAdress;
  UpdateImage;
end;

procedure TFrameViewDipSwitchs.MIMustBeOFFClick(Sender: TObject);
begin
  if (FWorkingSwitchIndex<0) or (FWorkingSwitchIndex>=Count) then exit;
  FSwitchs[FWorkingSwitchIndex].SwitchFunction:=dsfOff;
  UpdateImage;
end;

procedure TFrameViewDipSwitchs.MIMustBeONClick(Sender: TObject);
begin
  if (FWorkingSwitchIndex<0) or (FWorkingSwitchIndex>=Count) then exit;
  FSwitchs[FWorkingSwitchIndex].SwitchFunction:=dsfOn;
  UpdateImage;
end;

procedure TFrameViewDipSwitchs.PB1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if not FEditable then exit;
  if SwitchIndexAtPos(X, Y)<>-1
    then PB1.Cursor:=crHandPoint
    else PB1.Cursor:=crDefault;
end;

procedure TFrameViewDipSwitchs.PB1Resize(Sender: TObject);
begin
  UpdateImage;
end;

end.

