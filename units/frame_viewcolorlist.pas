unit frame_viewcolorlist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, lightcolorgradient,
  u_presetmanager, Types, LCLType;

type

  { TFrameViewColorList }

  TFrameViewColorList = class(TFrame)
    LB: TListBox;
    procedure LBDrawItem({%H-}Control: TWinControl; Index: Integer; ARect: TRect;
      State: TOwnerDrawState);
    procedure LBKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure LBKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
  private
    function GetChartColor(Index: integer): TColorChartColor;
    function GetCount: integer;
  private
    FPresetManager: TPresetManager;
    function ColorListToPreset: string;
    procedure PresetToColorList(const A: TStringArray);
  public
    constructor Create( TheOwner: TComponent ); override;
    destructor Destroy; override;
    procedure EraseBackground({%H-}DC: HDC); override;

    procedure Clear;
    procedure Add(c: TColorChartColor);
    procedure DeleteSelection;
    procedure MoveSelectionUp;
    procedure MoveSelectionDown;

    procedure UpdateStringAfterLanguageChange;

    property Count: integer read GetCount;
    property ChartColors[Index: integer]: TColorChartColor read GetChartColor;
    property PresetManager: TPresetManager read FPresetManager;
  end;

implementation
uses LCLHelper, u_common;

{$R *.lfm}

{ TFrameViewColorList }

procedure TFrameViewColorList.LBDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var c: TColorChartColor;
    x: integer;
begin
  // a color is made of: the base color +' '+ a percentage [0..1]
  c.InitFromString(LB.Items.Strings[Index]);
  with LB.Canvas do
  begin
    Brush.Color := c.ToColor;
    fillrect(Arect);
    if State >= [odSelected] then
    begin
      Pen.Color := c.ToColor xor $00FFFFFF;
      x := arect.Left-10;
      repeat
        Line( x, arect.Top, x+10, arect.Bottom );
        x := x+10;
      until x > ARect.Right+10;
    end;
  end;
end;

procedure TFrameViewColorList.LBKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_SPACE then
    Key := VK_UNKNOWN;
end;

procedure TFrameViewColorList.LBKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_SPACE then
    Key := VK_UNKNOWN;
end;

function TFrameViewColorList.GetChartColor(Index: integer): TColorChartColor;
begin
  Result.InitFromString(LB.Items.Strings[Index]);
end;

function TFrameViewColorList.GetCount: integer;
begin
  Result := LB.Count;
end;

function TFrameViewColorList.ColorListToPreset: string;
var i: integer;
begin
  Result := '';
  for i:=0 to Count-1 do
  begin
    if i > 0 then
      Result := Result+PRESET_SEPARATOR;
    Result := Result+LB.Items.Strings[i];
  end;
end;

procedure TFrameViewColorList.PresetToColorList(const A: TStringArray);
var i: integer;
begin
  Clear;
  for i:=0 to High(A) do
    LB.Items.Add(A[i]);
end;

constructor TFrameViewColorList.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FPresetManager := TPresetManager.Create(NIL);
  FPresetManager.Init2(@PresetToColorList, @ColorListToPreset);
end;

destructor TFrameViewColorList.Destroy;
begin
  FPresetManager.Free;
  inherited Destroy;
end;

procedure TFrameViewColorList.EraseBackground(DC: HDC);
begin
// do nothing here
end;

procedure TFrameViewColorList.Clear;
begin
  LB.Clear;
end;

procedure TFrameViewColorList.Add(c: TColorChartColor);
begin
  LB.Items.Add(c.ToString);
end;

procedure TFrameViewColorList.DeleteSelection;
begin
  LB.DeleteSelected;
end;

procedure TFrameViewColorList.MoveSelectionUp;
begin
  LB.MoveSelectionUp;
end;

procedure TFrameViewColorList.MoveSelectionDown;
begin
  LB.MoveSelectionDown;
end;

procedure TFrameViewColorList.UpdateStringAfterLanguageChange;
begin
  FPresetManager.UpdateStringAfterLanguageChange;
end;

end.

