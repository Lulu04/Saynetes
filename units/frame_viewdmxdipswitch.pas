unit frame_viewdmxdipswitch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Menus, u_dmx_util,
  Graphics, u_list_dmxuniverse;

type

  { TFrameViewDipSwitch }

  TFrameViewDipSwitch = class(TFrame)
    Label1: TLabel;
    MIMustBeOFF: TMenuItem;
    MIMustBeON: TMenuItem;
    MIAdressSetting: TMenuItem;
    PopupMenu1: TPopupMenu;
    Shape1: TShape;
    Shape2: TShape;
    procedure MIAdressSettingClick(Sender: TObject);
    procedure MIMustBeOFFClick(Sender: TObject);
    procedure MIMustBeONClick(Sender: TObject);
    procedure Shape2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FOnIsUp: boolean;
    FSwitchFunction: TDipSwitchFunction;
    FCircleColor: TColor;
    function GetBitNumber: integer;
    procedure SetBitNumber(AValue: integer);
    procedure SetSwitchFunction(AValue: TDipSwitchFunction);
    procedure UpdateColor;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure SetCircleColor(aColor: TColor);// color of the circle in the switch
    procedure SetPosition(aUp: boolean);
    procedure SetOnIsUp(AValue: boolean);
    property BitNumber: integer read GetBitNumber write SetBitNumber;
    property SwitchFunction: TDipSwitchFunction read FSwitchFunction write SetSwitchFunction;
  end;

  ArrayOfFrameViewDipSwitch=array of TFrameViewDipSwitch;

implementation

{$R *.lfm}

{ TFrameViewDipSwitch }

procedure TFrameViewDipSwitch.MIAdressSettingClick(Sender: TObject);
begin
  SwitchFunction:=dsfAdress;
end;

procedure TFrameViewDipSwitch.MIMustBeOFFClick(Sender: TObject);
begin
  SwitchFunction:=dsfOff;
end;

procedure TFrameViewDipSwitch.MIMustBeONClick(Sender: TObject);
begin
  SwitchFunction:=dsfOn;
end;

procedure TFrameViewDipSwitch.Shape2MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbRight then begin
    PopupMenu1.PopUp;
  end;
end;

function TFrameViewDipSwitch.GetBitNumber: integer;
begin
  Result:=string(Label1.Caption).ToInteger;
end;

procedure TFrameViewDipSwitch.SetBitNumber(AValue: integer);
begin
  Label1.Caption:=AValue.ToString;
end;

procedure TFrameViewDipSwitch.SetSwitchFunction(AValue: TDipSwitchFunction);
begin
  if FSwitchFunction=AValue then Exit;
  FSwitchFunction:=AValue;
  UpdateColor;
end;

procedure TFrameViewDipSwitch.UpdateColor;
begin
  case FSwitchFunction of
    dsfAdress: begin
      Shape1.Brush.Color:=FCircleColor;
      Shape2.Brush.Color:=RGBToColor(243,234,109);
      SetPosition(FALSE);
    end;
    dsfOn: begin
      Shape1.Brush.Color:=RGBToColor(26,50,26);
      Shape2.Brush.Color:=RGBToColor(139,252,101);
      SetPosition(FOnIsUp);
    end;
    dsfOff: begin
      Shape1.Brush.Color:=RGBToColor(50,26,26);
      Shape2.Brush.Color:=RGBToColor(252,139,101);
      SetPosition(not FOnIsUp);
    end;
  end;
end;

constructor TFrameViewDipSwitch.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FSwitchFunction:=dsfAdress;
  FCircleColor:=RGBToColor(199,185,16);
  SetPosition(FALSE);
end;

procedure TFrameViewDipSwitch.SetCircleColor(aColor: TColor);
begin
  FCircleColor:=aColor;
  Shape1.Brush.Color:=aColor;
end;

procedure TFrameViewDipSwitch.SetPosition(aUp: boolean);
var margin: integer;
begin
  margin:=Round(Shape2.Height*0.1);
  case aUp of
    TRUE: Shape1.Top:=Shape2.Top+margin;
    FALSE: Shape1.Top:=Shape2.Top+Shape2.Height-margin-Shape1.Height;
  end;
end;

procedure TFrameViewDipSwitch.SetOnIsUp(AValue: boolean);
begin
  if FOnIsUp=AValue then exit;
  FOnIsUp:=AValue;
  case FOnIsUp of
    TRUE: begin
      Shape2.Top:=0;
      Label1.Top:=Shape2.Height;
    end;
    FALSE: begin
      Label1.Top:=0;
      Shape2.Top:=Label1.Font.Height;
    end;
  end;

  case FSwitchFunction of
    dsfAdress, dsfOn: SetPosition(FOnIsUp);
    dsfOff: SetPosition(not FOnIsUp);
  end;
end;

end.

