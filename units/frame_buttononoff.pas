unit frame_buttononoff;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, Buttons, Graphics;

type

  { TFrameButtonOnOff }

  TFrameButtonOnOff = class(TFrame)
    Shape3: TShape;
    SpeedButton3: TSpeedButton;
    procedure SpeedButton3Click(Sender: TObject);
  private
    class var FInstanceCounter: integer;
    function NextInstanceName: string;
  private
    FState: boolean;
    FBrushActivatedColor,
    FPenActivatedColor,
    FBrushDeactivatedColor,
    FPenDeactivatedColor: TColor;
    procedure SetState(AValue: boolean);
  public
    constructor Create(aOwner: TComponent); override;

    // The panel must handle its OnClick event because the frame's button OnClick
    // is redirected to the panel's one.
    procedure AssociateWith(aPanel: TPanel);

    procedure SetActivatedColors(aPenColor, aBrushColor: TColor);
    procedure SetDeactivatedColors(aPenColor, aBrushColor: TColor);

    property State: boolean read FState write SetState;

  end;

implementation

{$R *.lfm}

{ TFrameButtonOnOff }

procedure TFrameButtonOnOff.SpeedButton3Click(Sender: TObject);
begin
  Parent.OnClick(Parent);
end;

function TFrameButtonOnOff.NextInstanceName: string;
begin
  inc(FInstanceCounter);
  Result := {'FrameButtonOnOff'}Name+FInstanceCounter.ToString;
end;

procedure TFrameButtonOnOff.SetState(AValue: boolean);
begin
  if FState=AValue then Exit;
  FState:=AValue;
  if FState then
  begin
    Shape3.Brush.Color := FBrushActivatedColor;
    Shape3.Pen.Color := FPenActivatedColor;
  end
  else begin
    Shape3.Brush.Color := FBrushDeactivatedColor;
    Shape3.Pen.Color := FPenDeactivatedColor;
  end;
end;

constructor TFrameButtonOnOff.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  Name := NextInstanceName;

  FBrushActivatedColor := $0003C4FC;
  FPenActivatedColor := $00363636;
  FBrushDeactivatedColor := $00D6D6D6;
  FPenDeactivatedColor := $00363636;
end;

procedure TFrameButtonOnOff.AssociateWith(aPanel: TPanel);
begin
  Parent := aPanel;
  Align := alClient;
end;

procedure TFrameButtonOnOff.SetActivatedColors(aPenColor, aBrushColor: TColor);
begin
  FBrushActivatedColor := aBrushColor;
  FPenActivatedColor := aPenColor;
end;

procedure TFrameButtonOnOff.SetDeactivatedColors(aPenColor, aBrushColor: TColor);
begin
  FBrushDeactivatedColor := aBrushColor;
  FPenDeactivatedColor := aPenColor;
end;

end.

