unit u_customized_form;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, Spin, frame_trackbar,
  BGRABitmap, BGRABitmapTypes, BGRAGradients;

type

  { TFormCustomized }

  TFormCustomized = class(TForm)
    BEnlargeWindow: TSpeedButton;
    BReduceWindow: TSpeedButton;
    CheckBox1: TCheckBox;
    ColorButton1: TColorButton;
    ColorButton2: TColorButton;
    ColorButton3: TColorButton;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    FontDialog1: TFontDialog;
    FrameTrackBar1: TFrameTrackBar;
    FrameTrackBar2: TFrameTrackBar;
    FSE1: TFloatSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    ListBox1: TListBox;
    ListBox2: TListBox;
    ListBox3: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    PB: TPaintBox;
    BCloseWindow: TSpeedButton;
    SpeedButton1: TSpeedButton;
    SpeedButton10: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    SpinEdit1: TSpinEdit;
    procedure BCloseWindowClick(Sender: TObject);
    procedure BCloseWindowMouseEnter(Sender: TObject);
    procedure BCloseWindowMouseLeave(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure ColorButton1ColorChanged(Sender: TObject);
    procedure ColorButton2ColorChanged(Sender: TObject);
    procedure ColorButton3ColorChanged(Sender: TObject);
    procedure FontDialog1ApplyClicked(Sender: TObject);
    procedure FormChangeBounds(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure PBChangeBounds(Sender: TObject);
    procedure PBMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PBMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PBMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PBPaint(Sender: TObject);
    procedure PBResize(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
  private
  type
    TMouseCaptureType=(mctNone,
                       mctTitleBar,
                       mctLeftHandle,
                       mctRightHandle,
                       mctTopHandle,
                       mctBottomHandle,
                       mctBottomLeftHandle,
                       mctBottomRightHandle);
  var
    FMouseCaptureType: TMouseCaptureType;
  private
    FTitleBarBackGround: TBGRABitmap;
    FTitleBarTopColor,
    FTitleBarBottomColor: TColor;
    procedure PrepareGradientImage;
  private
    FClickOrigin: TPoint;
    FFrameWidth: integer;
    FSizeable: boolean;
    FLeftRectHandle,
    FRightRectHandle,
    FBottomRectHandle,
    FTopRectHandle,
    FBottomLeftRectHandle,
    FBottomRightRectHandle: TRect;
    procedure RecomputeObjectsRect;

    function PercentColor(aColor: TColor; aPercentOffset: single): TColor;
    function GetBackGroundColor(aLevel: integer): TColor;
    procedure ApplyBackGroundOnControls(const aControl:TWinControl; aLevel: integer);
    procedure DoApplyFont(const aControl:TWinControl; aFont: TFont);
    procedure ProcessLabelAnchoredToCheckBoxOrRadioButton(Sender: TObject);
    procedure SetFrameWidth(AValue: integer);
  protected
    procedure SetColor(Value: TColor); override;
  public
    procedure EraseBackground(DC: HDC); override;

  public
    // The label must be anchored FROM THE LEFT on a TCheckBox or TRadioButton
    procedure LabelClickToCheckedObject(aLabel: TLabel);

  public // Color controls
    procedure SetTitleBarColors(aTopColor: TColor; aBottomPercent: single);

    procedure ApplyFont(aFont: TFont);

    property FrameWidth: integer read FFrameWidth write SetFrameWidth;
    property Sizeable: boolean read FSizeable write FSizeable;
end;

var
  FormCustomized: TFormCustomized;

implementation
uses Math;

{$R *.lfm}

{ TFormCustomized }

procedure TFormCustomized.FormCreate(Sender: TObject);
begin
  FTitleBarBackGround := TBGRABitmap.Create(1, 1);
  FMouseCaptureType := mctNone;

  SetTitleBarColors($0003C4FC, 0.5);
  FFrameWidth := 1;

 LabelClickToCheckedObject(Label4);
 FrameTrackBar2.Orientation := trVertical;
end;

procedure TFormCustomized.BCloseWindowClick(Sender: TObject);
begin
  Close;
end;

procedure TFormCustomized.BCloseWindowMouseEnter(Sender: TObject);
begin
  BCloseWindow.Color := RGBToColor(255,200,200);
  BCloseWindow.Transparent := False;
end;

procedure TFormCustomized.BCloseWindowMouseLeave(Sender: TObject);
begin
  BCloseWindow.Color := clBtnFace;
  BCloseWindow.Transparent := True;
end;

procedure TFormCustomized.CheckBox1Change(Sender: TObject);
begin
end;

procedure TFormCustomized.ColorButton1ColorChanged(Sender: TObject);
begin
  SetTitleBarColors(ColorButton1.ButtonColor, FSE1.Value);
end;

procedure TFormCustomized.ColorButton2ColorChanged(Sender: TObject);
begin
  Color := ColorButton2.ButtonColor;
end;

procedure TFormCustomized.ColorButton3ColorChanged(Sender: TObject);
begin

end;

procedure TFormCustomized.FontDialog1ApplyClicked(Sender: TObject);
begin
  ApplyFont(FontDialog1.Font);
end;

procedure TFormCustomized.FormChangeBounds(Sender: TObject);
begin
  RecomputeObjectsRect;
end;

procedure TFormCustomized.FormDestroy(Sender: TObject);
begin
  if FTitleBarBackGround <> NIL then
    FTitleBarBackGround.Free;
end;

procedure TFormCustomized.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (FMouseCaptureType = mctNone) and FSizeable then
  begin
    FClickOrigin := Mouse.CursorPos;

    if FBottomLeftRectHandle.Contains(Point(X, Y)) then
    begin
      FMouseCaptureType := mctBottomLeftHandle;
      Cursor := crSizeNESW;
    end;

    if FBottomRightRectHandle.Contains(Point(X, Y)) then
    begin
      FMouseCaptureType := mctBottomRightHandle;
      Cursor := crSizeNWSE;
    end;

    if FLeftRectHandle.Contains(Point(X, Y)) then
    begin
      FMouseCaptureType := mctLeftHandle;
      Cursor := crSizeWE;
    end;

    if FRightRectHandle.Contains(Point(X, Y)) then
    begin
      FMouseCaptureType := mctRightHandle;
      Cursor := crSizeWE;
    end;

    if FBottomRectHandle.Contains(Point(X, Y)) then
    begin
      FMouseCaptureType := mctBottomHandle;
      Cursor := crSizeNS;
    end;
  end;
end;

procedure TFormCustomized.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  mp: TPoint;
  dx, dy: integer;
  r: TRect;
  le,t,ri,bo: integer;
  flagChanged: boolean;
begin
  if FSizeable then
  begin
    mp := Mouse.CursorPos;
    dx := mp.x-FClickOrigin.x;
    dy := mp.y-FClickOrigin.y;

    le := Left;
    t := Top;
    ri := Left+ClientWidth-1;
    bo := Top+ClientHeight-1;

    case FMouseCaptureType of
      mctNone:
        begin
          if FBottomLeftRectHandle.Contains(Point(X, Y)) then
            Cursor := crSizeNESW
          else
          if FBottomRightRectHandle.Contains(Point(X, Y)) then
            Cursor := crSizeNWSE
          else
          if FLeftRectHandle.Contains( Point(X, Y) ) or
             FRightRectHandle.Contains( Point(X, Y) ) then
             Cursor := crSizeWE
          else
          if FTopRectHandle.Contains( Point(X, Y) ) or
             FBottomRectHandle.Contains( Point(X, Y) ) then
           Cursor := crSizeNS
          else Cursor := crDefault;
        end;

      mctLeftHandle:
        begin // Resize from the left
          if Width-dx > 100 then
          begin
            SetBounds(Left+dx, Top, Width-dx, Height);
            FClickOrigin := mp;
          end;
        end;

      mctRightHandle:
        begin // Resize from the right
          if Width+dx > 100 then
          begin
            SetBounds(Left, Top, Width+dx, Height);
            FClickOrigin := mp;
          end;
        end;

      mctBottomHandle: // Resize from the bottom
        begin
          if Height+dy > 100 then
          begin
            SetBounds(Left, Top, Width, Height+dy);
            FClickOrigin := mp;
          end;
        end;

      mctBottomLeftHandle: // Resize from bottom left corner
        begin
          flagChanged := False;

          if Width-dx > 100 then
          begin
            flagChanged := True;
            le := le+dx;
          end;
          if Height+dy > 100 then
          begin
            flagChanged := True;
            bo := bo+dy;
          end;
          if flagChanged then
          begin
            SetBounds(le, t, ri-le+1, bo-t+1);
            FClickOrigin := mp;
          end;
        end;

      mctBottomRightHandle: // Resize from bottom right corner
        begin
          flagChanged := False;

          if Width+dx > 100 then
          begin
            flagChanged := True;
            ri := ri+dx;
          end;
          if Height+dy > 100 then
          begin
            flagChanged := True;
            bo := bo+dy;
          end;
          if flagChanged then
          begin
            SetBounds(le, t, ri-le+1, bo-t+1);
            FClickOrigin := mp;
          end;
        end;
    end;
  end;
end;

procedure TFormCustomized.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Cursor := crDefault;
  FMouseCaptureType := mctNone;
end;

procedure TFormCustomized.FormPaint(Sender: TObject);
var w: integer;
begin
  if FFrameWidth > 0 then
  begin
    w := Round(FFrameWidth*GetCanvasScaleFactor);
    with Canvas do
    begin
      Pen.Color := FTitleBarBottomColor;
      if w = 1 then
      begin
        Line(0, 0, 0, Self.Height);
        Line(Self.Width-1, 0, Self.Width-1, Self.Height);
        Line(0, Self.Height-1, Self.Width-1, Self.Height-1);
      end
      else
      begin
        Brush.Color := FTitleBarBottomColor;
        Rectangle(0, 0, w, Self.Height);
        Rectangle(Self.Width-w, 0, Self.Width, Self.Height);
        Rectangle(0, Self.Height-w, Self.Width-1, Self.Height);
      end;
    end;
  end;
end;

procedure TFormCustomized.FormResize(Sender: TObject);
begin
  RecomputeObjectsRect;
end;

procedure TFormCustomized.PBChangeBounds(Sender: TObject);
begin
  RecomputeObjectsRect;
end;

procedure TFormCustomized.PBMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FMouseCaptureType = mctNone then
  begin
    if PB.ClientRect.Contains(Point(X, Y)) then
    begin
      FClickOrigin := Mouse.CursorPos;
      FMouseCaptureType := mctTitleBar;
      PB.Cursor := crSize;
    end;

    if FTopRectHandle.Contains(Point(X, Y)) then
    begin
      FMouseCaptureType := mctTopHandle;
      PB.Cursor := crSizeNS;
    end;

  end;
end;

procedure TFormCustomized.PBMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var p, mp: TPoint;
  dx, dy: LongInt;
begin
  case FMouseCaptureType of
    mctNone: //
      if FTopRectHandle.Contains(PB.ScreenToClient(Mouse.CursorPos)) then
        PB.Cursor := crSizeNS
      else
        PB.Cursor := crHandPoint;

    mctTitleBar: // move window
      begin
        p := Mouse.CursorPos - FClickOrigin;
        FClickOrigin := Mouse.CursorPos;
        Left := Left + p.x;
        Top := Top + p.y;
      end;

    mctTopHandle: // resize window from top
      begin
        mp := Mouse.CursorPos;
        dx := mp.x-FClickOrigin.x;
        dy := mp.y-FClickOrigin.y;
        if Height-dy > 100 then
        begin
          SetBounds(Left, Top+dy, Width, Height-dy);
          FClickOrigin := mp;
        end;
      end;
  end;
end;

procedure TFormCustomized.PBMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  case FMouseCaptureType of
    mctTitleBar:
      begin
        FMouseCaptureType := mctNone;
        PB.Cursor := crHandPoint;
      end;

    mctTopHandle:
      begin
        FMouseCaptureType := mctNone;
        PB.Cursor := crHandPoint;
      end;
  end;
end;

procedure TFormCustomized.PBPaint(Sender: TObject);
begin
  if (FTitleBarBackGround.Width<>PB.ClientWidth) or
     (FTitleBarBackGround.Height<>PB.ClientHeight) then
    PrepareGradientImage;

  FTitleBarBackGround.Draw(PB.Canvas, 0, 0);
end;

procedure TFormCustomized.PBResize(Sender: TObject);
begin
  PrepareGradientImage;
  RecomputeObjectsRect;
end;

procedure TFormCustomized.SpeedButton1Click(Sender: TObject);
begin
  if not FontDialog1.Execute then exit;
  ApplyFont(FontDialog1.Font);
end;

procedure TFormCustomized.SpinEdit1Change(Sender: TObject);
begin
  FrameWidth := SpinEdit1.Value;
end;

procedure TFormCustomized.PrepareGradientImage;
var
  GInfo: TnGradientInfo;
begin
  if FTitleBarBackGround <> NIL then
    FTitleBarBackGround.Free;

  GInfo := nGradientInfo(FTitleBarTopColor,
                         FTitleBarBottomColor,
                         gdVertical, 1.0);
  FTitleBarBackGround := nGradientAlphaFill(PB.ClientWidth, PB.ClientHeight,
                                            gdVertical, [GInfo]);

  with FTitleBarBackGround do
  begin
    FontName := 'Arial';
    FontFullHeight := Round(FTitleBarBackGround.Height*0.68);
    FontAntialias := True;
    TextOut(5, (FTitleBarBackGround.Height-FontFullHeight) div 2, Self.Caption, BGRABlack);
  end;
end;

procedure TFormCustomized.RecomputeObjectsRect;
begin
  FLeftRectHandle.TopLeft := Point(ClientRect.Left, ClientRect.Top+4);
  FLeftRectHandle.BottomRight := Point(ClientRect.Left+4, ClientRect.Bottom-4);

  FRightRectHandle.TopLeft := Point(ClientRect.Right-4, ClientRect.Top+4);
  FRightRectHandle.BottomRight := Point(ClientRect.Right, ClientRect.Bottom-4);

  FBottomRectHandle.TopLeft := Point(ClientRect.Left+4, ClientRect.Bottom-4);
  FBottomRectHandle.BottomRight := Point(ClientRect.Right-4, ClientRect.Bottom);

  FBottomLeftRectHandle.TopLeft := Point(ClientRect.Left, ClientRect.Bottom-4);
  FBottomLeftRectHandle.BottomRight := Point(ClientRect.Left+4, ClientRect.Bottom);

  FBottomRightRectHandle.TopLeft := Point(ClientRect.Right-4, ClientRect.Bottom-4);
  FBottomRightRectHandle.BottomRight := Point(ClientRect.Right, ClientRect.Bottom);


  FTopRectHandle.TopLeft := Point(PB.Left, PB.Top);
  FTopRectHandle.BottomRight := Point(PB.Left+PB.Width, PB.Top+4);
end;

function TFormCustomized.PercentColor(aColor: TColor; aPercentOffset: single): TColor;
var r,g,b: byte;
begin
  r := Red(aColor);
  r := EnsureRange(Round(r+r*aPercentOffset), 0, 255);
  g := Green(aColor);
  g := EnsureRange(Round(g+g*aPercentOffset), 0, 255);
  b := Blue(aColor);
  b := EnsureRange(Round(b+b*aPercentOffset), 0, 255);
  Result := RGBToColor(r, g, b);
end;

function TFormCustomized.GetBackGroundColor(aLevel: integer): TColor;
begin
{  if aLevel=0 then
    Result := Color
  else} if aLevel mod 2 = 1 then
    Result := PercentColor(Color, 0.1)
  else
    Result := PercentColor(Color, -0.1);
end;

procedure TFormCustomized.ApplyBackGroundOnControls(const aControl: TWinControl; aLevel: integer);
var
  i: Integer;
  ctrl: TControl;
begin
  if csDestroying in aControl.ComponentState then
    exit;

  for i:=0 to aControl.ControlCount-1 do
  begin
    ctrl := aControl.Controls[i];
    if (ctrl is TPanel) or
       (ctrl is TListBox) or
       (ctrl is TComboBox) or
       (ctrl is TEdit) or
       (ctrl is TSpinEdit) or
       (ctrl is TFloatSpinEdit) or
       (ctrl is TSpeedButton) then
      ctrl.Color := GetBackGroundColor(aLevel);


    if aControl.Controls[i] is TWinControl then
      ApplyBackGroundOnControls(TwinControl(aControl.Controls[i]), aLevel+1);
  end;
end;

procedure TFormCustomized.DoApplyFont(const aControl: TWinControl; aFont: TFont);
var
  i: Integer;
  ctrl: TControl;
  fs: TFontStyles;
begin
  if csDestroying in aControl.ComponentState then
    exit;

  for i:=0 to aControl.ControlCount-1 do
  begin
    ctrl := aControl.Controls[i];
    if (ctrl is TLabel) or
       (ctrl is TListBox) or
       (ctrl is TComboBox) or
       (ctrl is TEdit) or
       (ctrl is TSpinEdit) or
       (ctrl is TFloatSpinEdit) or
       (ctrl is TSpeedButton) then
    begin
      fs := aFont.Style;
      aFont.Style := ctrl.Font.Style;
      ctrl.Font := aFont;
      aFont.Style := fs;
    end;

    if aControl.Controls[i] is TWinControl then
      DoApplyFont(TwinControl(aControl.Controls[i]), aFont);
  end;
end;

procedure TFormCustomized.ProcessLabelAnchoredToCheckBoxOrRadioButton(Sender: TObject);
var L: TLabel;
begin
  if not (Sender is TLabel) then exit;
  if csDestroying in ComponentState then
    exit;

  L := Sender as TLabel;
  if L.AnchorSideLeft.Control is TCheckBox then
    TCheckBox(L.AnchorSideLeft.Control).Checked := not TCheckBox(L.AnchorSideLeft.Control).Checked
  else
  if L.AnchorSideLeft.Control is TRadioButton then
    TRadioButton(L.AnchorSideLeft.Control).Checked := True;
end;

procedure TFormCustomized.SetFrameWidth(AValue: integer);
begin
  if FFrameWidth=AValue then Exit;
  FFrameWidth:=AValue;
  Invalidate;
end;

procedure TFormCustomized.SetColor(Value: TColor);
begin
  inherited SetColor(Value);
  ApplyBackGroundOnControls(Self, 0);
end;

procedure TFormCustomized.EraseBackground(DC: HDC);
begin
  inherited EraseBackground(DC);
end;

procedure TFormCustomized.LabelClickToCheckedObject(aLabel: TLabel);
begin
  aLabel.OnClick := @ProcessLabelAnchoredToCheckBoxOrRadioButton;
end;


procedure TFormCustomized.SetTitleBarColors(aTopColor: TColor; aBottomPercent: single);
begin
  FTitleBarTopColor := aTopColor;
  FTitleBarBottomColor := RGBToColor(
                   Round(Red(ColorButton1.ButtonColor)*aBottomPercent),
                   Round(Green(ColorButton1.ButtonColor)*aBottomPercent),
                   Round(Blue(ColorButton1.ButtonColor)*aBottomPercent));
  PrepareGradientImage;

  if FFrameWidth > 0 then
    Invalidate
  else
   PB.Invalidate;
end;

procedure TFormCustomized.ApplyFont(aFont: TFont);
begin
  DoApplyFont(Self, aFont);
end;

end.

