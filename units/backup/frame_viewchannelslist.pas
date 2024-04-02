unit frame_viewchannelslist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, LCLType, Types,
  u_list_dmxuniverse,
  frame_viewprojectors;

type

  { TFrameViewChannelsList }

  TFrameViewChannelsList = class(TFrame)
    LB: TListBox;
    procedure LBDrawItem({%H-}Control: TWinControl; Index: Integer; ARect: TRect;
      State: TOwnerDrawState);
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
    function GetChannel(index: integer): TDMXChannel;
    function GetCount: integer;
    function GetMultiSelect: boolean;
    function GetSelCount: integer;
    function GetSelected: ArrayOfDmxChannels;
    procedure SetMultiSelect(AValue: boolean);
    procedure DoSelectionChange;
  public
    FTargetViewProjector: TFrameViewProjector;
    constructor Create(TheOwner: TComponent); override;
    procedure EraseBackground({%H-}DC: HDC); override;
    procedure Fill;

    procedure Clear;
    procedure AddChannel(aChan: TDMXChannel);
    procedure RemoveChannel(aChan: TDMXChannel);

    procedure MoveSelectionUp;
    procedure MoveSelectionDown;

    property Count: integer read GetCount;
    property Channels[index: integer]: TDMXChannel read GetChannel;
    property MultiSelect: boolean read GetMultiSelect write SetMultiSelect;
    property SelCount: integer read GetSelCount;
    property Selected: ArrayOfDmxChannels read GetSelected;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
  end;

implementation

uses Graphics, LCLHelper;

{$R *.lfm}

{ TFrameViewChannelsList }

procedure TFrameViewChannelsList.LBDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var chan: TDMXChannel;
  txt: string;
begin
  chan := TDMXChannel(LB.Items.Objects[Index]);

  with LB.Canvas do begin
    txt := chan.Universe.ShortName+':'+chan.Adress.ToString+' - '+
        chan.Fixture.Description+' - '+chan._Name;

    if State >= [odSelected] then
      Brush.Color := clHighLight // ligne sélectionnée
    else
      Brush.Color := LB.Color;

    if Index = FItemIndexUnderMouse then
    begin
       // render dot rectangle if mouse is over item
       Pen.Style := psDot;
       Pen.Color := RGBToColor(200,200,150);
       Rectangle(ARect.Left-1, ARect.Top, ARect.Right+1, ARect.Bottom);
    end
    else
    begin
       //Pen.Style:=psClear;
       FillRect(ARect);
    end;
    Brush.Style := bsClear;
    Font.Color := $00EAEAEA;
    TextOut(ARect.Left+3, ARect.Top, txt);
  end;
end;

procedure TFrameViewChannelsList.LBKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_SPACE then
    Key := VK_UNKNOWN;
end;

procedure TFrameViewChannelsList.LBKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_SPACE then
    Key := VK_UNKNOWN;
end;

procedure TFrameViewChannelsList.LBMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i: integer;
begin
  i := LB.GetIndexAtY(Y);

  // click on empty area = unselect all
  if (Button = mbLeft) and (i = -1) then
    LB.ItemIndex := -1;
end;

procedure TFrameViewChannelsList.LBMouseLeave(Sender: TObject);
begin
  FItemIndexUnderMouse := -1;
  LB.Invalidate;
  FTargetViewProjector.ProcessViewDMXCursorsMouseOverFixtureEvent(Self, NIL);
  FTargetViewProjector.FrameViewDMXCursors1.SetChannelUnderMouse(NIL);
end;

procedure TFrameViewChannelsList.LBMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var i: integer;
    fix: TDMXFixture;
    chan: TDMXChannel;
begin
  i := LB.GetIndexAtY(Y);
  if i <> FItemIndexUnderMouse then begin
    FItemIndexUnderMouse := i;
    LB.Invalidate;
  end;

  if i <> -1 then
  begin
    chan := TDMXChannel(LB.Items.Objects[i]);
    fix := chan.Fixture;
  end
  else
  begin
    chan := NIL;
    fix := NIL;
  end;
  FTargetViewProjector.ProcessViewDMXCursorsMouseOverFixtureEvent(Self, fix);
  FTargetViewProjector.FrameViewDMXCursors1.SetChannelUnderMouse(chan);
end;

procedure TFrameViewChannelsList.LBSelectionChange(Sender: TObject; User: boolean);
begin
  DoSelectionChange;
end;

function TFrameViewChannelsList.GetCount: integer;
begin
  Result := LB.Count;
end;

function TFrameViewChannelsList.GetMultiSelect: boolean;
begin
  Result := LB.MultiSelect;
end;

function TFrameViewChannelsList.GetSelCount: integer;
begin
  Result := LB.SelCount;
end;

function TFrameViewChannelsList.GetSelected: ArrayOfDmxChannels;
var i, k: integer;
begin
  Result := NIL;
  SetLength(Result, LB.SelCount);
  k := 0;
  for i:=0 to LB.Count-1 do
    if LB.Selected[i] then
    begin
      Result[k] := TDMXChannel(LB.Items.Objects[i]);
      inc(k);
    end;
end;

procedure TFrameViewChannelsList.SetMultiSelect(AValue: boolean);
begin
  LB.MultiSelect := AValue;
end;

procedure TFrameViewChannelsList.DoSelectionChange;
begin
  if FOnSelectionChange <> NIL then
    FOnSelectionChange(Self);
end;

function TFrameViewChannelsList.GetChannel(index: integer): TDMXChannel;
begin
  Result := TDMXChannel(LB.Items.Objects[index]);
end;

constructor TFrameViewChannelsList.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FItemIndexUnderMouse := -1;
end;

procedure TFrameViewChannelsList.EraseBackground(DC: HDC);
begin
//do nothing here
end;

procedure TFrameViewChannelsList.Fill;
var A: arrayOfDMXChannels;
    i: integer;
begin
  LB.LockSelectionChange;

  LB.Clear;
  A := FTargetViewProjector.FrameViewDMXCursors1.GetTargetChannels;
  for i:=0 to High(A) do
    LB.Items.AddObject('', A[i]);

  LB.UnlockSelectionChange;
end;

procedure TFrameViewChannelsList.Clear;
var flag: boolean;
begin
  flag := LB.SelCount<>0;
  LB.Clear;
  if flag then
    DoSelectionChange;
end;

procedure TFrameViewChannelsList.AddChannel(aChan: TDMXChannel);
begin
  if LB.Items.IndexOfObject(aChan) = -1 then
    LB.Items.AddObject('', aChan);
end;

procedure TFrameViewChannelsList.RemoveChannel(aChan: TDMXChannel);
var i: Integer;
    flag: boolean;
begin
  i := LB.Items.IndexOfObject(aChan);
  if i = -1 then
    exit;

  flag := LB.Selected[i];
  LB.Items.Delete(i);
  if flag then
    DoSelectionChange;
end;

procedure TFrameViewChannelsList.MoveSelectionUp;
begin
  LB.MoveSelectionUp;
end;

procedure TFrameViewChannelsList.MoveSelectionDown;
begin
  LB.MoveSelectionDown;
end;

end.

