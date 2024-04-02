unit frame_viewuniverselist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Menus, ExtCtrls, Types,
  u_list_dmxuniverse;

type

  { TFrameViewUniverseList }

  TFrameViewUniverseList = class(TFrame)
    LB: TListBox;
    Timer1: TTimer;
    procedure LBDrawItem({%H-}Control: TWinControl; Index: Integer; ARect: TRect;
      State: TOwnerDrawState);
    procedure LBKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure LBKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure LBSelectionChange(Sender: TObject; User: boolean);
    procedure Timer1Timer(Sender: TObject);
  private
    FFlagBlink: boolean;
    FOnSelectionChange: TNotifyEvent;
    function GetSelectedUniverse: TDMXUniverse;
    function GetUniverseIndex: integer;
  public
    procedure Fill;
    procedure StopTimer;

    procedure AddToView;
    procedure RemoveSelected;

    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
    property UniverseIndex: integer read GetUniverseIndex;
    property SelectedUniverse: TDMXUniverse read GetSelectedUniverse;
  end;

implementation
uses Graphics, LCLType, u_resource_string, u_datamodule,
  u_utils, u_dmx_util, u_dmxdevice_manager;

{$R *.lfm}

{ TFrameViewUniverseList }

procedure TFrameViewUniverseList.LBDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var txt: string;
  xx, yy: integer;
  uni: TDMXUniverse;
begin
  uni := UniverseManager.Universes[Index];

  with LB.Canvas do
  begin
    if State>=[odSelected] then
    begin
      Brush.Color := clHighLight;
      Font.Color := clWhite;
    end
    else begin
      if Index Mod 2 = 0 then
        Brush.Color := LB.Color
      else
        Brush.Color := PercentColor(LB.Color,0.3);
      Font.Color := $00EAEAEA;
    end;
    FillRect(Arect);

    // universe name
    txt := uni.ShortName+' - '+uni.Name;
    Font.Height := 16;
    Font.Style := [fsBold];
    xx := ARect.Left+5;
    TextOut(xx, ARect.Top, txt);
    xx := xx+TextWidth(txt);
    if FFlagBlink and uni.ErrorInAdressing then
     DataModule1.ImageList2.Draw(LB.Canvas, xx, ARect.Top, 0);

    // occupied adress
    Font.Height := 14;
    txt := (uni.LastUsedAdress).ToString+'/'+uni.LastAdress.ToString;
    xx := ARect.Right-5-TextWidth(txt);
    TextOut(xx, ARect.Top+2, txt);

    // device name
    xx := xx+Font.GetTextWidth(txt)+15;
    yy := ARect.Top+Font.Height;
    Font.Height := 12;
    txt := uni.DevicePath.DeviceNameSerialPort;
    if uni.DevicePath.DeviceIndex <> INVALID_DMXDEVICE_INDEX then
    begin
      if uni.IsConnected then
      begin
        txt := txt+' '+SOk;
        Font.Style := [];
        Font.Color := RGBToColor(220,220,172);
        TextOut(ARect.Left+5, yy, txt);
      end
      else
      begin
        if uni.HaveDevice then
          txt := txt+' '+SOff;
        Font.Style := [fsBold];
        Font.Color := RGBToColor(255,182,172);
        TextOut(ARect.Left+5, yy, txt);
      end;
    end;
  end;
end;

procedure TFrameViewUniverseList.LBKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_SPACE then
    Key := VK_UNKNOWN;
end;

procedure TFrameViewUniverseList.LBKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_SPACE then
    Key := VK_UNKNOWN;
end;

procedure TFrameViewUniverseList.LBSelectionChange(Sender: TObject;
  User: boolean);
begin
  if FOnSelectionChange <> NIL then
    FOnSelectionChange(Self);
end;

procedure TFrameViewUniverseList.Timer1Timer(Sender: TObject);
begin
  FFlagBlink := not FFlagBlink;
  LB.Invalidate;
end;

function TFrameViewUniverseList.GetUniverseIndex: integer;
begin
  Result := LB.ItemIndex;
end;

function TFrameViewUniverseList.GetSelectedUniverse: TDMXUniverse;
begin
  if LB.ItemIndex = -1 then
    Result := NIL
  else
    Result := UniverseManager.Universes[LB.ItemIndex];
end;

procedure TFrameViewUniverseList.Fill;
var i: integer;
begin
  LB.LockSelectionChange;
  LB.Clear;
  for i:=0 to UniverseManager.Count-1 do
    LB.Items.Add(' ');
  if LB.Count > 0 then
    LB.ItemIndex := 0;
  LB.UnlockSelectionChange;
  Timer1.Enabled := TRUE;
end;

procedure TFrameViewUniverseList.StopTimer;
begin
  Timer1.Enabled := FALSE;
end;

procedure TFrameViewUniverseList.AddToView;
begin
  LB.ItemIndex := LB.Items.Add(' ');
end;

procedure TFrameViewUniverseList.RemoveSelected;
begin
  if LB.ItemIndex = -1 then
    exit;

  LB.DeleteSelected;
end;

end.

