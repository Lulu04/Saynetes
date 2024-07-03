unit frame_trackbar;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, ComCtrls, LCLType, Graphics,
  BGRABitmap, BGRABitmapTypes;

type
  TTrackBarOrientation = (trHorizontal, trVertical);

  { TFrameTrackBar }

  TFrameTrackBar = class(TFrame)
    PB: TPaintBox;
    procedure PBMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PBMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PBMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PBPaint(Sender: TObject);
    procedure PBResize(Sender: TObject);
  private type
    TFrameTrackBarCursor = record
      PercentValue: single;
      R: TRect;
      CursorImage: TBGRABitmap;
    end;
    PFrameTrackBarCursor = ^TFrameTrackBarCursor;
  private
    class var FGlobalCursorFillColor, FGlobalCursorOutlineColor: TBGRAPixel;
    class var FInstanceCount: integer;
  private
    FOrientation: TTrackBarOrientation;
    FReversed,
    FIntervalMode,
    FShowValue: Boolean;
    FCursorMax,
    FCursorMin: TFrameTrackBarCursor;
    FWorkingCursor: PFrameTrackBarCursor;
    FCursorPositionRange: single;
    FOnChange: TNotifyEvent;

    FMouseCanMoveCursor, FMouseMovingCursor: boolean;
    FClickOrigin: TPoint;
    FCursorFillColor, FCursorOutlineColor: TBGRAPixel;
    procedure LoopUserMoveCursor;
    function GetCursorFillColor: TBGRAPixel;
    function GetCursorOutlineColor: TBGRAPixel;
    procedure SetCursorFillColor(AValue: TBGRAPixel);
    procedure SetCursorOutlineColor(AValue: TBGRAPixel);
    function AxisWidth: integer;
    procedure UpdateCursorsImage;
    procedure DeleteCursorsImage;
    procedure UpdateCursorsRect;
    procedure ProcessValueChange;
    function GetPercentValue: single;
    function GetPercentMin: single;
    procedure SetPercentValue(AValue: single);
    procedure SetPercentMin(AValue: single);
  public
    constructor Create(TheOwner: TComponent; aParent: TWinControl); overload;
    destructor Destroy; override;
    procedure EraseBackground({%H-}DC: HDC); override;

    // TTrackBarOrientation = trVertical or trHorizontal
    procedure Init(aOrientation: TTrackBarOrientation; aReversed, aActiveIntervalMode, aShowValue: boolean);

    // sets the cursor colors used for each new instance created
    class procedure SetGlobalCursorColors(aFillColor, aOutlineColor: TBGRAPixel);
    // Sets the color for this instance only
    procedure SetInstanceCursorColors(aFillColor, aOutlineColor: TBGRAPixel);

    function GetLegend: string; virtual;
    function GetLegendMin: string; virtual;

    // The percentage value when the interval mode is not activated. Range is [0..1]
    // If interval mode is not activated, this property refer to PercentMax
    property PercentValue: single read GetPercentValue write SetPercentValue;
    // The Min percentage value selected by the user when the interval mode is activated. Range is [0..1]
    // If interval mode is not activated, this property refer to PercentValue
    property PercentMin: single read GetPercentMin write SetPercentMin;
    // The Max percentage value selected by the user when the interval mode is activated. Range is [0..1]
    // If interval mode is not activated, this property refer to PercentValue
    property PercentMax: single read GetPercentValue write SetPercentValue;

    // Sets the color for this instance only
    property CursorFillColor: TBGRAPixel read GetCursorFillColor write SetCursorFillColor;
    property CursorOutlineColor: TBGRAPixel read GetCursorOutlineColor write SetCursorOutlineColor;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property UserMovingCursor: boolean read FMouseMovingCursor;
  end;
implementation
uses Math, u_utils;

{$R *.lfm}

{ TFrameTrackBar }

procedure TFrameTrackBar.PBMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FClickOrigin := Mouse.CursorPos;

  if FCursorMax.R.Contains(Point(X, Y)) then
  begin
    FMouseCanMoveCursor := True;
    FWorkingCursor := @FCursorMax;
  end;

  if FIntervalMode and FCursorMin.R.Contains(Point(X, Y)) then
  begin
    FMouseCanMoveCursor := True;
    FWorkingCursor := @FCursorMin;
  end;
end;

procedure TFrameTrackBar.PBMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var delta: integer;
begin
  if FMouseCanMoveCursor then LoopUserMoveCursor
  else begin
    if FCursorMax.R.Contains(Point(X, Y)) or
       (FCursorMin.R.Contains(Point(X, Y)) and FIntervalMode) then PB.Cursor := crHandPoint
      else PB.Cursor := crDefault;
  end;
end;

procedure TFrameTrackBar.PBMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FMouseCanMoveCursor := False;
  FMouseMovingCursor := False;
end;

procedure TFrameTrackBar.PBPaint(Sender: TObject);
var t: TBGRABitmap;
  x, y: integer;
  txt: String;
begin
  with PB.Canvas do begin
    // axis
    if Enabled then
      Pen.Color := PercentColorRelative(Self.Parent.Color, 0.5)
    else
      Pen.Color := clGrayText;
    if FOrientation = trHorizontal then
    begin
      y := PB.ClientHeight div 4;
      x := FCursorMax.R.Width shr 1;
      if FIntervalMode then begin
        Line(x, y, x, PB.ClientHeight-y);
        Line(PB.ClientWidth-x, y, PB.ClientWidth-x, PB.ClientHeight-y);
      end;
      y := PB.ClientHeight shr 1;
      Line(x, y, PB.ClientWidth-x, y);
      Line(x, y+1, PB.ClientWidth-x, y+1);
      if FIntervalMode and Enabled then
      begin
        Pen.Color := FCursorOutlineColor;
        Line(FCursorMin.R.CenterPoint.x, y, FCursorMax.R.CenterPoint.x, y);
        Line(FCursorMin.R.CenterPoint.x, y+1, FCursorMax.R.CenterPoint.x, y+1);
      end;
    end
    else begin
      x := PB.ClientWidth div 4;
      y := FCursorMax.R.Height shr 1;
      if FIntervalMode then begin
        Line(x, y, PB.ClientWidth-x, y);
        Line(x, PB.ClientHeight-y, PB.ClientWidth-x, PB.ClientHeight-y);
      end;
      x := PB.ClientWidth shr 1;
      Line(x, y, x, PB.ClientHeight-y);
      Line(x+1, y, x+1, PB.ClientHeight-y);
      if FIntervalMode and Enabled then
      begin
        Pen.Color := FCursorOutlineColor; ;
        Line(x, FCursorMin.R.CenterPoint.y, x, FCursorMax.R.CenterPoint.y);
        Line(x+1, FCursorMin.R.CenterPoint.y, x+1, FCursorMax.R.CenterPoint.y);
      end;
    end;
  end;

  // cursors
  if Enabled then
    FCursorMax.CursorImage.Draw(PB.Canvas, FCursorMax.R.Left, FCursorMax.R.Top, False)
  else begin
    t := FCursorMax.CursorImage.FilterGrayscale;
    t.Draw(PB.Canvas, FCursorMax.R.Left, FCursorMax.R.Top, False);
    t.Free;
  end;
  if FIntervalMode then
    if Enabled then
      FCursorMin.CursorImage.Draw(PB.Canvas, FCursorMin.R.Left, FCursorMin.R.Top, False)
    else begin
      t := FCursorMin.CursorImage.FilterGrayscale;
      t.Draw(PB.Canvas, FCursorMin.R.Left, FCursorMin.R.Top, False);
      t.Free;
    end;

  // label value
  if FShowValue then
  with PB.Canvas do begin
    Brush.Style := bsClear;

    if Enabled then Font.Color := Self.Font.Color
      else Font.Color := clGrayText;

    txt := GetLegend;
    if FOrientation = trHorizontal then
    begin
      x := GetTextWidth(txt);
      if FCursorMax.R.Right+x <= PB.ClientWidth then
        x := FCursorMax.R.Right
      else
        x := FCursorMax.R.Left-x;
      TextOut(x, FCursorMax.R.Top, txt);
    end
    else begin
      x := GetTextHeight(txt);
      if FCursorMax.R.Top-x >= 0 then
        x := FCursorMax.R.Top-x
      else
        x := FCursorMax.R.Bottom;
      TextOut(FCursorMax.R.Left, x, txt);
    end;

    if FIntervalMode then begin
      txt := GetLegendMin; //FormatFloat('0.0', PercentMin*100)+'%';

      if FOrientation = trHorizontal then
      begin
        x := GetTextWidth(txt);
        if FCursorMin.R.Left-x >= 0 then
          x := FCursorMin.R.Left-x
        else
          x := FCursorMin.R.Right;
        TextOut(x, FCursorMin.R.Top, txt);
      end
      else begin
        x := GetTextHeight(txt);
        if FCursorMin.R.Top-x >= 0 then
          x := FCursorMin.R.Top-x
        else
          x := FCursorMin.R.Bottom;
        TextOut(FCursorMin.R.Left, x, txt);
      end;
    end;
  end;
end;

procedure TFrameTrackBar.PBResize(Sender: TObject);
begin
  UpdateCursorsImage;

  if FOrientation = trHorizontal then
    FCursorPositionRange := PB.ClientWidth-FCursorMax.CursorImage.Width
  else
    FCursorPositionRange := PB.ClientHeight-FCursorMax.CursorImage.Height;

  UpdateCursorsRect;
end;

procedure TFrameTrackBar.LoopUserMoveCursor;
var delta: integer;
  p: TPoint;
begin
  if FMouseMovingCursor then exit;
  FMouseMovingCursor := True;
  FClickOrigin := PB.ScreenToClient(Mouse.CursorPos);

  repeat
    p := PB.ScreenToClient(Mouse.CursorPos);
    p.x := EnsureRange(p.x, 0, PB.ClientWidth);
    p.y := EnsureRange(p.y, 0, PB.ClientHeight);

    case FOrientation of
      trHorizontal: begin
        delta := p.x - FClickOrigin.x;
        FWorkingCursor^.PercentValue := EnsureRange((FWorkingCursor^.R.Left+delta)/FCursorPositionRange, 0, 1);
        if FWorkingCursor = @FCursorMax then
          if FCursorMin.PercentValue > FCursorMax.PercentValue then
             FCursorMin.PercentValue := FCursorMax.PercentValue;
        if FWorkingCursor = @FCursorMin then
          if FCursorMax.PercentValue < FCursorMin.PercentValue then
             FCursorMax.PercentValue := FCursorMin.PercentValue;
        ProcessValueChange;

        FClickOrigin.x := p.x;
      end;

      trVertical: begin
        delta := p.y - FClickOrigin.y;
        FWorkingCursor^.PercentValue := EnsureRange((FWorkingCursor^.R.Top+delta)/FCursorPositionRange, 0, 1);
        ProcessValueChange;
        FClickOrigin.y := p.y;
      end;
    end;

    Application.ProcessMessages;
    Sleep(1);
  until not FMouseMovingCursor;
end;

function TFrameTrackBar.AxisWidth: integer;
begin
  Result := ScaleDesignToForm(2);
end;

function TFrameTrackBar.GetPercentValue: single;
begin
  Result := FCursorMax.PercentValue;
  if FReversed then Result := 1.0 - Result;
end;

function TFrameTrackBar.GetCursorFillColor: TBGRAPixel;
begin
  Result := FCursorFillColor;
end;

function TFrameTrackBar.GetCursorOutlineColor: TBGRAPixel;
begin
  Result := FCursorOutlineColor;
end;

function TFrameTrackBar.GetPercentMin: single;
begin
  if FIntervalMode then
    Result := FCursorMin.PercentValue
  else
    Result := FCursorMax.PercentValue;

  if FReversed then Result := 1.0 - Result;
end;

procedure TFrameTrackBar.SetCursorFillColor(AValue: TBGRAPixel);
begin
  if FCursorFillColor = AValue then Exit;
  FCursorFillColor := AValue;
  UpdateCursorsImage;
  PB.Invalidate;
end;

procedure TFrameTrackBar.SetCursorOutlineColor(AValue: TBGRAPixel);
begin
  if FCursorOutlineColor = AValue then Exit;
  FCursorOutlineColor := AValue;
  UpdateCursorsImage;
  PB.Invalidate;
end;

procedure TFrameTrackBar.SetPercentValue(AValue: single);
begin
  AValue := EnsureRange(AValue, 0, 1);
  if FReversed then AValue := 1.0 - AValue;
  if FCursorMax.PercentValue = AValue then exit;

  FCursorMax.PercentValue := AValue;
  ProcessValueChange;
end;

procedure TFrameTrackBar.SetPercentMin(AValue: single);
begin
  if not FIntervalMode then SetPercentValue(AValue)
  else begin
    AValue := EnsureRange(AValue, 0, 1);
    if FReversed then AValue := 1.0 - AValue;
    if FCursorMin.PercentValue = AValue then exit;

    FCursorMin.PercentValue := AValue;
    ProcessValueChange;
  end;
end;

procedure TFrameTrackBar.UpdateCursorsImage;
var w, h: integer;
begin
  DeleteCursorsImage;

  if not FIntervalMode then
  begin
    if FOrientation = trHorizontal then
    begin
      w := EnsureRange(Round(PB.Height*0.05), ScaleDesignToForm(12), ScaleDesignToForm(16));
      h := PB.Height;
    end
    else begin
      w := PB.Width;
      h := EnsureRange(Round(PB.Width*0.05), ScaleDesignToForm(12), ScaleDesignToForm(16));
    end;
    FCursorMax.CursorImage := TBGRABitmap.Create(w, h, BGRAPixelTransparent);
    FCursorMax.CursorImage.RoundRectAntialias(0, 0, w-1, h-1,
          Min(w-1, h-1)*0.5, Min(w-1, h-1)*0.5,
          FCursorOutlineColor, 1, FCursorFillColor);
  end
  else begin
    if FOrientation = trHorizontal then
    begin
      w := EnsureRange(Round(PB.ClientHeight*0.05), ScaleDesignToForm(12), ScaleDesignToForm(16));
      h := (PB.ClientHeight - AxisWidth) div 2;
    end
    else begin
      w := (PB.ClientWidth - AxisWidth) div 2;
      h := EnsureRange(Round(PB.ClientWidth*0.05), ScaleDesignToForm(12), ScaleDesignToForm(16));
    end;
    FCursorMax.CursorImage := TBGRABitmap.Create(w, h, BGRAPixelTransparent);
    FCursorMax.CursorImage.DrawPolygonAntialias(
              [PointF(0,0), PointF(w-1,0), PointF(w-1,h/2), PointF(w/2,h-1),
               PointF(0,h/2), PointF(0,0)],
              FCursorOutlineColor, 1, FCursorFillColor);

    FCursorMin.CursorImage := TBGRABitmap.Create(w, h, BGRAPixelTransparent);
    FCursorMin.CursorImage.DrawPolygonAntialias(
              [PointF(0,h-1), PointF(0,h/2), PointF(w/2,0), PointF(w-1,h/2),
               PointF(w-1,h-1), PointF(0,h-1)],
              FCursorOutlineColor, 1, FCursorFillColor);
  end;
end;

procedure TFrameTrackBar.UpdateCursorsRect;
begin
  case FOrientation of
  trHorizontal:
    begin;
      FCursorMax.R.Left := Round(FCursorPositionRange*FCursorMax.PercentValue);
      if FIntervalMode then
      begin
        FCursorMax.R.Top := 0
      end
      else begin
        FCursorMax.R.Top := (PB.Height-FCursorMax.CursorImage.Height) div 2;
      end;
      FCursorMax.R.Width := FCursorMax.CursorImage.Width;
      FCursorMax.R.Height := FCursorMax.CursorImage.Height;

      if FIntervalMode then
      begin
        FCursorMin.R.Left := Round(FCursorPositionRange*FCursorMin.PercentValue);
        FCursorMin.R.Top := PB.Height - FCursorMin.CursorImage.Height;
        FCursorMin.R.Width := FCursorMin.CursorImage.Width;
        FCursorMin.R.Height := FCursorMin.CursorImage.Height;
      end;
    end;
  trVertical:
    begin
      if FIntervalMode then
        FCursorMax.R.Left := 0
      else
        FCursorMax.R.Left := (PB.Width-FCursorMax.CursorImage.Width) div 2;
      FCursorMax.R.Top := Round(FCursorPositionRange*FCursorMax.PercentValue);
      FCursorMax.R.Width := FCursorMax.CursorImage.Width;
      FCursorMax.R.Height := FCursorMax.CursorImage.Height;

      if FIntervalMode then
      begin
        FCursorMin.R.Left := PB.Width - FCursorMin.CursorImage.Width;
        FCursorMin.R.Top := Round(FCursorPositionRange*FCursorMin.PercentValue);
        FCursorMin.R.Width := FCursorMin.CursorImage.Width;
        FCursorMin.R.Height := FCursorMin.CursorImage.Height;
      end;
    end;
  end;
end;

procedure TFrameTrackBar.DeleteCursorsImage;
begin
  if FCursorMax.CursorImage <> NIL then
    FCursorMax.CursorImage.Free;
  FCursorMax.CursorImage := NIL;

  if FCursorMin.CursorImage <> NIL then
    FCursorMin.CursorImage.Free;
  FCursorMin.CursorImage := NIL;
end;

procedure TFrameTrackBar.ProcessValueChange;
begin
  UpdateCursorsRect;
  PB.Invalidate;

  if FOnChange <> NIL then FOnChange(Self);
end;

destructor TFrameTrackBar.Destroy;
begin
  DeleteCursorsImage;
  inherited Destroy;
end;

constructor TFrameTrackBar.Create(TheOwner: TComponent; aParent: TWinControl);
begin
  inherited Create(TheOwner);
  if FInstanceCount = 0 then begin
    FGlobalCursorFillColor := BGRA(255,128,0);
    FGlobalCursorOutlineColor := BGRA(192,96,0);
  end;
  inc(FInstanceCount);
  Name := 'FrameTrackBar'+FInstanceCount.ToString;
  Parent := aParent;
  Align := alClient;
  FOrientation := trHorizontal;
  UpdateCursorsImage;

  UpdateCursorsRect;
  FCursorMax.PercentValue := 1.0;
  FCursorMin.PercentValue := 0.0;

  FCursorFillColor := FGlobalCursorFillColor;
  FCursorOutlineColor := FGlobalCursorOutlineColor;
end;

procedure TFrameTrackBar.EraseBackground(DC: HDC);
begin
end;

procedure TFrameTrackBar.Init(aOrientation: TTrackBarOrientation; aReversed,
  aActiveIntervalMode, aShowValue: boolean);
begin
  FOrientation := aOrientation;
  FReversed := aReversed;
  FIntervalMode := aActiveIntervalMode;
  FShowValue := aShowValue;

  UpdateCursorsImage;
  UpdateCursorsRect;
  PB.Invalidate;
end;

class procedure TFrameTrackBar.SetGlobalCursorColors(aFillColor, aOutlineColor: TBGRAPixel);
begin
  FGlobalCursorFillColor := aFillColor;
  FGlobalCursorOutlineColor := aOutlineColor;
  inc(FInstanceCount);
end;

procedure TFrameTrackBar.SetInstanceCursorColors(aFillColor, aOutlineColor: TBGRAPixel);
begin
  FCursorFillColor := aFillColor;
  FCursorOutlineColor := aOutlineColor;
  UpdateCursorsImage;
  PB.Invalidate;
end;

function TFrameTrackBar.GetLegend: string;
begin
  Result := FormatFloat('0.0', PercentValue*100)+'%';
end;

function TFrameTrackBar.GetLegendMin: string;
begin
  Result := FormatFloat('0.0', PercentMin*100)+'%';
end;

end.

