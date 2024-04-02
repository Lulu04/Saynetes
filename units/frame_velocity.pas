unit frame_velocity;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, StdCtrls, ExtCtrls, Types,
  LCLType, LCLTranslator;

type

  { TFrame_Velocity }

  TFrame_Velocity = class(TFrame)
    CB: TComboBox;
    Panel1: TPanel;
    procedure CBChange(Sender: TObject);
    procedure CBDrawItem({%H-}Control: TWinControl; Index: Integer; ARect: TRect;
      {%H-}State: TOwnerDrawState);
    procedure FrameResize(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
  private
    FOnChange: TNotifyEvent;
    procedure DoOnChange;
    function GetSelectedCurveID: integer;
    function GetSelectedCurveName: string;
    procedure SetSelectedCurveID(AValue: integer);
  public
    procedure EraseBackground({%H-}DC: HDC); override;

    procedure UpdateList;
    function SelectionReady: boolean;

    property SelectedCurveID: integer read GetSelectedCurveID write SetSelectedCurveID;
    property SelectedCurveName: string read GetSelectedCurveName;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation
uses VelocityCurve,
  BGRABitmap, BGRABitmapTypes, Graphics;

{ TFrame_Velocity }

procedure TFrame_Velocity.Panel1Resize(Sender: TObject);
begin
  CB.ItemHeight := Panel1.ClientHeight;
end;

procedure TFrame_Velocity.DoOnChange;
begin
  if FOnChange <> NIL then
    FOnChange( Self );
end;

function TFrame_Velocity.GetSelectedCurveID: integer;
begin
  Result := CB.ItemIndex;
end;

function TFrame_Velocity.GetSelectedCurveName: string;
begin
  if VelocityCurveList.ValidCurveIndex( CB.ItemIndex ) then
    Result := VelocityCurveList.GetCurveByIndex( CB.ItemIndex ).Name
  else
    Result := '???';
end;

procedure TFrame_Velocity.SetSelectedCurveID(AValue: integer);
begin
  CB.ItemIndex := AValue;
end;

procedure TFrame_Velocity.CBDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var curve: TDataCurve;
  ima: TBGRABitmap;
  i: integer;
begin
  i := CB.Items.Strings[Index].ToInteger;
  curve := VelocityCurveList.GetCurveByID( i );
  ima := curve.GetBGRABitmapImage( ARect.Width-1, ARect.Height-2, TRUE );
//  if State >= [odSelected] then
//    ima.RectangleAntialias( 0, 0, ima.Width, ima.Height, ColorToBGRA(clHighLight),1.5);   // ligne sélectionnée
  ima.Draw(CB.Canvas, ARect.Left, ARect.Top, TRUE);
end;

procedure TFrame_Velocity.FrameResize(Sender: TObject);
begin
  CB.ItemHeight := Height-6;
end;

procedure TFrame_Velocity.CBChange(Sender: TObject);
begin
  DoOnChange;
end;

procedure TFrame_Velocity.EraseBackground(DC: HDC);
begin
end;

procedure TFrame_Velocity.UpdateList;
var i, k: Integer;
begin
  k := CB.ItemIndex;
  CB.Clear;
  for i:=0 to VelocityCurveList.Count-1 do
    CB.Items.Add( VelocityCurveList.GetCurveByIndex(i).ID.ToString );
  if k = -1 then
    k := 0;
  CB.ItemIndex := k;
end;

function TFrame_Velocity.SelectionReady: boolean;
begin
  Result := CB.ItemIndex<>-1;
end;

initialization
  {$I frame_velocity.lrs}

end.

