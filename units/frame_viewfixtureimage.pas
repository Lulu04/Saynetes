unit frame_viewfixtureimage;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, LCLType,
  BGRABitmap, BGRABitmapTypes, u_common;

type

  TFrameFixtureImage = class;
  TFrameFixtureImageOnChangeEvent = procedure(Sender: TFrameFixtureImage) of object;

  { TFrameFixtureImage }

  TFrameFixtureImage = class(TFrame)
    PB: TPaintBox;
    Shape1: TShape;
    ST: TLabel;
    procedure PBPaint(Sender: TObject);
    procedure STClick(Sender: TObject);
  private
    FImage: TBGRABitmap;
    FFixtureType: TFixtureType;
    FOnChange: TFrameFixtureImageOnChangeEvent;
    function GetChecked: boolean;
    procedure SetChecked(AValue: boolean);

  public
    destructor Destroy; override;
    procedure EraseBackground({%H-}DC: HDC); override;

    procedure InitWith(aFT: TFixtureType);
    property FixtureType: TFixtureType read FFixtureType;
    property Checked: boolean read GetChecked write SetChecked;
    property OnChange: TFrameFixtureImageOnChangeEvent read FOnChange write FOnChange;
  end;

implementation
uses Graphics, utilitaire_bgrabitmap, u_dmx_util;

{$R *.lfm}

{ TFrameFixtureImage }

procedure TFrameFixtureImage.STClick(Sender: TObject);
begin
  if Checked then exit;
  SetChecked(True);
  FOnChange(Self);
end;

procedure TFrameFixtureImage.PBPaint(Sender: TObject);
begin
  if FImage <> NIL then FImage.Draw(PB.Canvas, 0, 0);
end;

function TFrameFixtureImage.GetChecked: boolean;
begin
  Result := Shape1.Visible;
end;

procedure TFrameFixtureImage.SetChecked(AValue: boolean);
begin
  if AValue then begin
    Shape1.Visible := True;
    ST.Font.Color := clWhite;
  end else begin
    Shape1.Visible := False;
    ST.Font.Color := clBlack;
  end;
end;

destructor TFrameFixtureImage.Destroy;
begin
  if FImage <> NIL then FImage.Free;
  FImage := NIL;
  inherited Destroy;
end;

procedure TFrameFixtureImage.EraseBackground(DC: HDC);
begin
  //
end;

procedure TFrameFixtureImage.InitWith(aFT: TFixtureType);
begin
  if FImage <> NIL then FImage.Free;

  FImage := SVGFileToBGRABitmap(FixtureSVGFileFor(aFT), PB.ClientWidth, PB.ClientHeight);
  ST.Caption := FixtureNameFor(aFT);
  FFixtureType := aFT;
  SetChecked(False);
  PB.Invalidate;
end;

end.

