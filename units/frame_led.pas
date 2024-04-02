unit frame_led;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, Graphics;

type

  { TFrameLed }

  TFrameLed = class(TFrame)
    Shape5: TShape;
    Timer1: TTimer;
    procedure Timer1Timer(Sender: TObject);
  private
    class var FInstanceCounter: integer;
    function NextInstanceName: string;
  private
    FState,
    FBlinkWhenOn,
    FUseInternalTimer: boolean;
    FBrushActivatedColor,
    FPenActivatedColor,
    FBrushDeactivatedColor,
    FPenDeactivatedColor: TColor;
    function GetMsInterval: Cardinal;
    procedure SetActivatedColors;
    procedure SetDeactivatedColors;
    procedure SetMSInterval(AValue: Cardinal);
    procedure SetState(AValue: boolean);
  public
    constructor Create(aOwner: TComponent); override;
    procedure AssociateToPanel(aPanel: TPanel);

    procedure GreenType;
    procedure Toggle;

    // To blink, the led don't use an internal timer, but you have to declare
    // your own timer and call UpdateLed method in it's OnTimer event.
    // Use this strategy if you want to synchronize several leds.
    procedure DontUseInternalTimer;
    procedure UpdateLed(aLight: boolean);

    property BlinkWhenOn: boolean read FBlinkWhenOn write FBlinkWhenOn;
    property State: boolean read FState write SetState;
    property MsInterval: Cardinal read GetMsInterval write SetMSInterval;
  end;

implementation

{$R *.lfm}

{ TFrameLed }

procedure TFrameLed.Timer1Timer(Sender: TObject);
begin
  if not FState or not FBlinkWhenOn then exit;

  if Shape5.Brush.Color = FBrushDeactivatedColor then
    SetActivatedColors
  else
    SetDeactivatedColors;
end;

function TFrameLed.NextInstanceName: string;
begin
  inc(FInstanceCounter);
  Result := 'FrameLed'+FInstanceCounter.ToString;
end;

procedure TFrameLed.SetActivatedColors;
begin
  Shape5.Brush.Color := FBrushActivatedColor;
  Shape5.Pen.Color := FPenActivatedColor;
end;

function TFrameLed.GetMsInterval: Cardinal;
begin
  Result := Timer1.Interval;
end;

procedure TFrameLed.SetDeactivatedColors;
begin
  Shape5.Brush.Color := FBrushDeactivatedColor;
  Shape5.Pen.Color := FPenDeactivatedColor;
end;

procedure TFrameLed.SetMSInterval(AValue: Cardinal);
begin
  Timer1.Interval := AValue;
end;

procedure TFrameLed.SetState(AValue: boolean);
begin
  if FState = AValue then Exit;
  FState := AValue;

  if FState then
  begin
    SetActivatedColors;
    if FBlinkWhenOn and FUseInternalTimer then
      Timer1.Enabled := True;
  end
  else begin
    Timer1.Enabled := False;
    SetDeactivatedColors;
  end;
end;

constructor TFrameLed.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  Name := NextInstanceName;
  FUseInternalTimer := True;
end;

procedure TFrameLed.AssociateToPanel(aPanel: TPanel);
begin
  Parent := aPanel;
  Align := alClient;
end;

procedure TFrameLed.GreenType;
begin
  FBrushActivatedColor := $00A8FFB3;
  FPenActivatedColor := $006AFF7D;
  FBrushDeactivatedColor := $00233C22;
  FPenDeactivatedColor := $004E6157;
end;


procedure TFrameLed.Toggle;
begin
  State := not State;
end;

procedure TFrameLed.DontUseInternalTimer;
begin
  FUseInternalTimer := False;
end;

procedure TFrameLed.UpdateLed(aLight: boolean);
begin
  if not FState or not FBlinkWhenOn then
    exit;

  if aLight then
    SetActivatedColors
  else
    SetDeactivatedColors;
end;

end.

