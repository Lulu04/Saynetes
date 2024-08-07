unit frame_viewfixtureslist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, LCLType, StdCtrls, Types,
  u_list_dmxuniverse, frame_viewprojectors;

type

  { TFrameViewFixturesList }

  TFrameViewFixturesList = class(TFrame)
    LB: TListBox;
    procedure LBDrawItem({%H-}Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure LBKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure LBKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure LBMouseDown(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, Y: Integer);
    procedure LBMouseLeave(Sender: TObject);
    procedure LBMouseMove(Sender: TObject; {%H-}Shift: TShiftState; {%H-}X, Y: Integer);
    procedure LBSelectionChange(Sender: TObject; {%H-}User: boolean);
  private
    FItemIndexUnderMouse: integer;
    FOnSelectionChange: TNotifyEvent;
    function GetFixture(index: integer): TDMXFixture;
    function GetCount: integer;
    function GetMultiSelect: boolean;
    function GetSelCount: integer;
    function GetSelected: ArrayOfDmxFixtures;
    procedure SetMultiSelect(AValue: boolean);
    procedure DoSelectionChange;
  public
    FTargetViewProjector: TFrameViewProjector;
    constructor Create(TheOwner: TComponent); override;
    procedure EraseBackground({%H-}DC: HDC); override;

    procedure FillWithSelected;

    procedure FillWithRGBFixture(aUni: TDMXUniverse);
    procedure SelectFixture(aFixtureID: integer);

    procedure Clear;
    procedure AddFixture(aFix: TDMXFixture);
    procedure RemoveFixture(aFix: TDMXFixture);

    procedure MoveSelectionUp;
    procedure MoveSelectionDown;

    property Count: integer read GetCount;
    property Fixtures[index: integer]: TDMXFixture read GetFixture;
    property MultiSelect: boolean read GetMultiSelect write SetMultiSelect;
    property SelCount: integer read GetSelCount;
    property Selected: ArrayOfDmxFixtures read GetSelected;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
  end;

implementation
uses Graphics, LCLHelper, u_helper;

{$R *.lfm}

{ TFrameViewFixturesList }

procedure TFrameViewFixturesList.LBDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var fix: TDMXFixture;
  txt: string;
const SEP=' - ';
begin
  fix := TDMXFixture(LB.Items.Objects[Index]);

  txt := '';
  if UniverseManager.Count > 1 then txt := fix.Universe.ShortName+':'+fix.Adress.ToString;
  txt.Concat(fix.Name, SEP);
  txt.Concat(fix.Description, SEP);

  with LB.Canvas do begin
    Brush.Color := LB.Color;
    FillRect(ARect);
    Brush.Style := bsClear;
    if State >= [odSelected] then begin
    // render rectangle if selected
      Pen.Style := psSolid;
      Pen.Color := RGBToColor(255,80,255);
      Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
    end
    else
    if Index = FItemIndexUnderMouse then begin
      // render dot rectangle if mouse is over item
      Pen.Style := psDot;
      Pen.Color := RGBToColor(200,200,150);
      Rectangle(ARect.Left-1, ARect.Top, ARect.Right+1, ARect.Bottom);
     end;

    Font.Color := $00EAEAEA;
    TextOut(ARect.Left+3, ARect.Top, txt);
  end;
end;

procedure TFrameViewFixturesList.LBKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_SPACE then
    Key := VK_UNKNOWN;
end;

procedure TFrameViewFixturesList.LBKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_SPACE then
    Key := VK_UNKNOWN;
end;

procedure TFrameViewFixturesList.LBMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i: integer;
begin
  i := LB.GetIndexAtY(Y);

  // click on empty area = unselect all
  if (Button = mbLeft) and (i = -1) then
    LB.ItemIndex := -1;
end;

procedure TFrameViewFixturesList.LBMouseLeave(Sender: TObject);
begin
  FItemIndexUnderMouse := -1;
  LB.Invalidate;
  if FTargetViewProjector <> NIL then
    FTargetViewProjector.ProcessViewDMXCursorsMouseOverFixtureEvent(Self, NIL);
end;

procedure TFrameViewFixturesList.LBMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var i: integer;
    fix: TDMXFixture;
begin
  i := LB.GetIndexAtY(Y);
  if i <> FItemIndexUnderMouse then
  begin
    FItemIndexUnderMouse := i;
    LB.Invalidate;
  end;

  if i <> -1 then
    fix := TDMXFixture(LB.Items.Objects[i])
  else
    fix := NIL;
  if FTargetViewProjector <> NIL then
    FTargetViewProjector.ProcessViewDMXCursorsMouseOverFixtureEvent(Self, fix);
end;

procedure TFrameViewFixturesList.LBSelectionChange(Sender: TObject; User: boolean);
begin
  DoSelectionChange;
end;

function TFrameViewFixturesList.GetFixture(index: integer): TDMXFixture;
begin
  Result := TDMXFixture(LB.Items.Objects[index]);
end;

function TFrameViewFixturesList.GetCount: integer;
begin
  Result := LB.Count;
end;

function TFrameViewFixturesList.GetMultiSelect: boolean;
begin
  Result := LB.MultiSelect;
end;

function TFrameViewFixturesList.GetSelCount: integer;
var i: integer;
begin
  Result := 0;
  for i:=0 to LB.Count-1 do
    if LB.Selected[i] then inc(Result);
end;

function TFrameViewFixturesList.GetSelected: ArrayOfDmxFixtures;
var i, k: integer;
begin
  Result := NIL;
  i := SelCount;
  if i = 0 then exit;

  SetLength(Result, i);
  k := 0;
  for i:=0 to LB.Count-1 do
    if LB.Selected[i] then
    begin
      Result[k] := TDMXFixture(LB.Items.Objects[i]);
      inc(k);
    end;
end;

procedure TFrameViewFixturesList.SetMultiSelect(AValue: boolean);
begin
  LB.MultiSelect := AValue;
end;

procedure TFrameViewFixturesList.DoSelectionChange;
begin
  if FOnSelectionChange <> NIL then
    FOnSelectionChange(Self);
end;

constructor TFrameViewFixturesList.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FItemIndexUnderMouse := -1;
end;

procedure TFrameViewFixturesList.EraseBackground(DC: HDC);
begin
  inherited EraseBackground(DC);
end;

procedure TFrameViewFixturesList.FillWithSelected;
var A: arrayOfDMXFixtures;
    i: integer;
begin
  LB.LockSelectionChange;
  LB.Clear;
  A := FTargetViewProjector.FrameViewDMXCursors1.GetTargetFixtures;
  for i:=0 to High(A) do
    LB.Items.AddObject('', A[i]);
  LB.UnlockSelectionChange;
end;

procedure TFrameViewFixturesList.FillWithRGBFixture(aUni: TDMXUniverse);
var i: integer;
begin
  LB.Clear;
  if aUni = NIL then exit;

  LB.LockSelectionChange;
  for i:=0 to aUni.FixturesCount-1 do
    if aUni.Fixtures[i].HasRGBChannel then LB.Items.AddObject('', aUni.Fixtures[i]);
  LB.UnlockSelectionChange;
end;

procedure TFrameViewFixturesList.SelectFixture(aFixtureID: integer);
var i: integer;
begin
  for i:=0 to LB.Count-1 do
    if TDMXFixture(LB.Items.Objects[i]).ID = aFixtureID then begin
      LB.ItemIndex := i;
      exit;
    end;
end;

procedure TFrameViewFixturesList.Clear;
var flag: boolean;
begin
  flag := LB.SelCount<>0;
  LB.Clear;
  if flag then
    DoSelectionChange;
end;

procedure TFrameViewFixturesList.AddFixture(aFix: TDMXFixture);
begin
  if LB.Items.IndexOfObject(aFix) = -1 then
    LB.Items.AddObject('', aFix);
end;

procedure TFrameViewFixturesList.RemoveFixture(aFix: TDMXFixture);
var i: Integer;
    flag: boolean;
begin
  i := LB.Items.IndexOfObject(aFix);
  if i = -1 then
    exit;

  flag := LB.Selected[i];
  LB.Items.Delete(i);
  if flag then
    DoselectionChange;
end;

procedure TFrameViewFixturesList.MoveSelectionUp;
begin
  LB.MoveSelectionUp;
end;

procedure TFrameViewFixturesList.MoveSelectionDown;
begin
  LB.MoveSelectionDown;
end;

end.

