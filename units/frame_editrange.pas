unit frame_editrange;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, LCLType,
  frame_view_switcheritem, u_list_dmxuniverse, System.UITypes;

type

  TEditTypeForKeyUpDown = (kudBegin=0, kudEnd, kudText, kudExtra);
  TOnEditKeyUpDownCallback = procedure(Sender: TObject; aEditType: TEditTypeForKeyUpDown; aSelStart: integer; aKey: Word) of object;

  { TFrameEditRange }

  TFrameEditRange = class(TFrame)
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Panel1: TPanel;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    Shape5: TShape;
    procedure Edit1EditingDone(Sender: TObject);
    procedure Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FIndex: integer;
    FOnCopyPreviousSwitcher: TNotifyEvent;
    FOnEditKeyUpDown: TOnEditKeyUpDownCallback;
    FOnHeightChange: TNotifyEvent;
    FOnValueChange: TNotifyEvent;
    FLockEditBeginEndDone: boolean;
    function GetBeginValue: integer;
    function GetDescription: string;
    function GetEndValue: integer;
    function GetExtra: string;
    procedure ProcessFrameSwitcherHeightChangeEvent(Sender: TObject);
    procedure ProcessFrameSwitcherCopyPreviousEvent(Sender: TObject);
    procedure SetBeginValue(AValue: integer);
    procedure SetDescription(AValue: string);
    procedure SetEndValue(AValue: integer);
    procedure SetExtra(AValue: string);
    procedure SetIndex(AValue: integer);
  public
    FrameSwitcher: TFrameViewSwitcherItems;
    constructor Create(TheOwner: TComponent); override;
    procedure EraseBackground({%H-}DC: HDC); override;
    procedure SetFocusOnEdit(aEditType: TEditTypeForKeyUpDown; aSelStart: integer);

    procedure InitRange(p: PFixLibSingleRange);

    property Index: integer read FIndex write SetIndex;
    property BeginValue: integer read GetBeginValue write SetBeginValue;
    property EndValue: integer read GetEndValue write SetEndValue;
    property Description: string read GetDescription write SetDescription;
    property Extra: string read GetExtra write SetExtra;

    property OnHeightChange: TNotifyEvent read FOnHeightChange write FOnHeightChange;
    property OnCopyPreviousSwitcher: TNotifyEvent read FOnCopyPreviousSwitcher write FOnCopyPreviousSwitcher;
    property OnBeginEndChange: TNotifyEvent read FOnValueChange write FOnValueChange;

    property OnEditKeyUpDown: TOnEditKeyUpDownCallback read FOnEditKeyUpDown write FOnEditKeyUpDown;
  end;

implementation
uses Math, u_utils, LazUTF8;

{$R *.lfm}

{ TFrameEditRange }

procedure TFrameEditRange.ProcessFrameSwitcherHeightChangeEvent(Sender: TObject);
begin
  ClientHeight := Max(ScaleDesignToForm(27), FrameSwitcher.Height);
  FOnHeightChange(Self);
end;

procedure TFrameEditRange.ProcessFrameSwitcherCopyPreviousEvent(Sender: TObject);
begin
  if Index = 0 then exit;
  FOnCopyPreviousSwitcher(Self);
end;

procedure TFrameEditRange.Edit1EditingDone(Sender: TObject);
var vb, ve: integer;
begin
  if FLockEditBeginEndDone then exit;
  FLockEditBeginEndDone := True;

  vb := StrToInt(Edit1.Text);
  ve := StrToInt(Edit2.Text);

  if vb < 0 then begin
    vb := 0;
    Edit1.Text := '0';
  end;
  if vb > 255 then begin
    vb := 255;
    Edit1.Text := '255';
  end;

  if ve < 0 then begin
    ve := 0;
    Edit2.Text := '0';
  end;
  if ve > 255 then begin
    ve := 255;
    Edit2.Text := '255';
  end;

  if vb > ve then Edit2.Text := Edit1.Text;
  if ve < vb then Edit1.Text := Edit2.Text;
  FLockEditBeginEndDone := False;

  FOnValueChange(Self);
end;

procedure TFrameEditRange.Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var e: TEdit;
begin
  e := Sender as TEdit;
  case Key of
    VK_UP, VK_DOWN: begin
      if FOnEditKeyUpDown <> NIL then
        FOnEditKeyUpDown(Self, TEditTypeForKeyUpDown(e.Tag), 10000, Key);
      key := 0;
    end;

    VK_LEFT: begin
      if (Sender = Edit1) and (Edit1.SelStart = 0) then begin
        // shift to the previous line, edit EXTRA
        if FOnEditKeyUpDown <> NIL then
          FOnEditKeyUpDown(Self, kudExtra, e.SelStart, VK_UP);
        key := 0;
      end else
      if (Sender = Edit2) and (Edit2.SelStart = 0) then begin
        Edit1.SetFocus;
        Edit1.SelStart := UTF8Length(Edit1.Text);
        Key := 0;
      end else
      if (Sender = Edit3) and (Edit3.SelStart = 0) then begin
        Edit2.SetFocus;
        Edit2.SelStart := UTF8Length(Edit2.Text);
        Key := 0;
      end else
      if (Sender = Edit4) and (Edit4.SelStart = 0) then begin
        Edit3.SetFocus;
        Edit3.SelStart := UTF8Length(Edit3.Text);
        Key := 0;
      end;
    end;

    VK_RIGHT: begin
      if (Sender = Edit4) and (Edit4.SelStart = UTF8Length(Edit4.Text)) then begin
        // shift to the next line, edit BEGIN
        if FOnEditKeyUpDown <> NIL then
          FOnEditKeyUpDown(Self, kudBegin, 0, VK_DOWN);
        key := 0;
      end else
      if (Sender = Edit1) and (Edit1.SelStart = UTF8Length(Edit1.Text)) then begin
        Edit2.SetFocus;
        Edit2.SelStart := 0;
        Key := 0;
      end else
      if (Sender = Edit2) and (Edit2.SelStart = UTF8Length(Edit2.Text)) then begin
        Edit3.SetFocus;
        Edit3.SelStart := 0;
        Key := 0;
      end else
      if (Sender = Edit3) and (Edit3.SelStart = UTF8Length(Edit3.Text)) then begin
        Edit4.SetFocus;
        Edit4.SelStart := 0;
        Key := 0;
      end;
    end;
  end;//case
end;

function TFrameEditRange.GetBeginValue: integer;
begin
  Result := StrToInt(Edit1.Text);
end;

function TFrameEditRange.GetDescription: string;
begin
  Result := Trim(Edit3.Text);
end;

function TFrameEditRange.GetEndValue: integer;
begin
  Result := StrToInt(Edit2.Text);
end;

function TFrameEditRange.GetExtra: string;
begin
  Result := Trim(Edit4.Text);
end;

procedure TFrameEditRange.SetBeginValue(AValue: integer);
begin
  Edit1.Text := AValue.ToString;
end;

procedure TFrameEditRange.SetDescription(AValue: string);
begin
  Edit3.Text := AValue;
end;

procedure TFrameEditRange.SetEndValue(AValue: integer);
begin
  Edit2.Text := AValue.ToString;
end;

procedure TFrameEditRange.SetExtra(AValue: string);
begin
  Edit4.Text := AValue;
end;

procedure TFrameEditRange.SetIndex(AValue: integer);
var c: TColor;
begin
  FIndex := AValue;
  // prevent the user to change the begin value '0'
  if AValue = 0 then Edit1.ReadOnly := True;

  // alternates color
  if Odd(AValue) then c := PercentColor(Parent.Color, -0.2)
    else c := Parent.Color;
  Panel1.Color := c;
  Edit1.Color := c;
  Edit2.Color := c;
  Edit3.Color := c;
  Edit4.Color := c;
  FrameSwitcher.Panel1.Color := c;
end;

constructor TFrameEditRange.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FrameSwitcher := TFrameViewSwitcherItems.Create(Self);
  FrameSwitcher.Parent := Panel1;
  FrameSwitcher.SetBounds(Shape5.Left+Shape5.Width, 0,
                          Panel1.ClientWidth-(Shape5.Left+Shape5.Width), FrameSwitcher.Height);
  FrameSwitcher.OnHeightChange := @ProcessFrameSwitcherHeightChangeEvent;
  FrameSwitcher.OnCopyPrevious := @ProcessFrameSwitcherCopyPreviousEvent;

  FrameSwitcher.Anchors := [akTop, akLeft, akRight];

  FrameSwitcher.AnchorSide[akLeft].Control := Shape5;
  FrameSwitcher.AnchorSide[akLeft].Side := asrRight;

  FrameSwitcher.AnchorSide[akRight].Control := Panel1;
  FrameSwitcher.AnchorSide[akRight].Side := asrRight;

end;

procedure TFrameEditRange.EraseBackground(DC: HDC);
begin
  //
end;

procedure TFrameEditRange.SetFocusOnEdit(aEditType: TEditTypeForKeyUpDown; aSelStart: integer);
begin
  try
    case aEditType of
      kudBegin: begin
        Edit1.SetFocus;
        Edit1.SelStart := aSelStart;
      end;

      kudEnd: begin
        Edit2.SetFocus;
        Edit2.SelStart := aSelStart;
      end;

      kudText: begin
        Edit3.SetFocus;
        Edit3.SelStart := aSelStart;
      end;

      kudExtra: begin
        Edit4.SetFocus;
        Edit4.SelStart := aSelStart;
      end;
    end;
  except
  end;
end;

procedure TFrameEditRange.InitRange(p: PFixLibSingleRange);
begin
  p^.InitDefault;
  p^.BeginValue := BeginValue;
  p^.EndValue := EndValue;
  p^.Text := Description;
  p^.Extra := Extra;
  FrameSwitcher.SetSwitcherForRange(p);
end;

end.

