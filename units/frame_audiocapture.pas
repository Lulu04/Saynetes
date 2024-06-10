unit frame_audiocapture;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, Buttons, ComCtrls, LCLType,
  StdCtrls, LCLTranslator,
  frame_audiolevels, frame_led, Types;

type

  { TFrameAudioCapture }

  TFrameAudioCapture = class(TFrame)
    CBFX: TComboBox;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel7: TPanel;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    BRemoveFX: TSpeedButton;
    BAddFX: TSpeedButton;
    BOnOff: TSpeedButton;
    SpeedButton4: TSpeedButton;
    Timer1: TTimer;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    TrackBar3: TTrackBar;
    procedure BRemoveFXClick(Sender: TObject);
    procedure BAddFXClick(Sender: TObject);
    procedure CBFXCloseUp(Sender: TObject);
    procedure CBFXDrawItem(Control: TWinControl; Index: Integer; ARect: TRect;
      State: TOwnerDrawState);
    procedure CBFXSelect(Sender: TObject);
    procedure Panel2Click(Sender: TObject);
    procedure BOnOffClick(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure TrackBar3Change(Sender: TObject);
  private
    FModuleIsActivated: boolean;
    FrameCaptureLevels1: TFrameAudioLevels;
    FrameLed1: TFrameLed;
    function CursorToPreAmp: single;
    function CursorToPan: single;
    procedure PanToCursor(AValue: single);
    function CursorToVolume: single;
    procedure VolumeToCursor(AValue: single);
    procedure DoRemoveEffect;
  public
    constructor Create(aOwner: TComponent); override;
    procedure EraseBackground({%H-}DC: HDC); override;

    procedure Fill; // update trackbar position, label caption and presets list
    procedure UpdateStringAfterLanguageChange;

    procedure UpdateVisual;
    property PreAmp: single read CursorToPreAmp; // pre amp value

  end;

implementation
uses u_resource_string, u_common, u_utils, u_audio_manager, u_apputils,
  BGRABitmap, utilitaire_bgrabitmap, Graphics;
{$R *.lfm}

{ TFrameAudioCapture }

procedure TFrameAudioCapture.BOnOffClick(Sender: TObject);
begin
  // activate/deactivate the capture only if user clicks on the button
  if Sender = BOnOff then
  begin
    if SoundManager.CaptureToPlaybackIsReady then
      SoundManager.StopCaptureToPlayback
    else
    begin
      SoundManager.StartCaptureToPlayback;
      TrackBar1Change(NIL); // apply preamp
    end;
  end;

  UpdateVisual;
end;

procedure TFrameAudioCapture.BRemoveFXClick(Sender: TObject);
begin
  DoRemoveEffect;
end;

procedure TFrameAudioCapture.BAddFXClick(Sender: TObject);
begin
  CBFX.Top := BAddFX.Top;
  CBFX.Visible := True;
  CBFX.DroppedDown := True;
end;

procedure TFrameAudioCapture.CBFXCloseUp(Sender: TObject);
begin
  CBFX.Visible := False;
end;

procedure TFrameAudioCapture.CBFXDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var o: TAudioPreset;
begin
  o.InitFromString(CBFX.Items.Strings[Index]);
  with CBFX.Canvas do begin
    if State >= [odSelected] then Brush.Color := clHighLight
      else Brush.Color := CBFX.Color;
    Brush.Style := bsSolid;
    FillRect(ARect);
    TextOut(ARect.Left, ARect.Top, o.Name);
  end;
end;

procedure TFrameAudioCapture.CBFXSelect(Sender: TObject);
var i: integer;
  o: TAudioPreset;
begin
  i := CBFX.ItemIndex;
  CBFX.Visible := False;

  o.InitFromString(CBFX.Items.Strings[i]);

  DoRemoveEffect;
  if o.EffectCount > 0 then begin
    if o.EffectCount >= 1 then
      SoundManager.AddEffectOn(CAPTURE_IDAUDIO, EffectIndexToALSoundEffectType(o.Effect1), o.Preset1);
    if o.EffectCount >= 2 then
      SoundManager.AddEffectOn(CAPTURE_IDAUDIO, EffectIndexToALSoundEffectType(o.Effect2), o.Preset2);
    if o.EffectCount >= 3 then
      SoundManager.AddEffectOn(CAPTURE_IDAUDIO, EffectIndexToALSoundEffectType(o.Effect3), o.Preset3);
    SoundManager.ConstructChainOn(CAPTURE_IDAUDIO);
    SoundManager.SetDryWetOn(CAPTURE_IDAUDIO, o.DryWet);
  end;
end;

procedure TFrameAudioCapture.Panel2Click(Sender: TObject);
begin
  CBFX.Visible := False;
end;

procedure TFrameAudioCapture.SpeedButton4Click(Sender: TObject);
begin
  TrackBar2.Position := 0;
end;

procedure TFrameAudioCapture.Timer1Timer(Sender: TObject);
var callback: TNotifyEvent;
  A: TStringArray;
begin
  Timer1.Enabled := False;
  // On/Off
  if SoundManager.CaptureToPlaybackIsReady <> FModuleIsActivated then
    UpdateVisual;

  if SoundManager.CaptureToPlaybackIsReady then
  begin
    // volume
    callback := TrackBar3.OnChange;
    TrackBar3.OnChange := NIL;
    VolumeToCursor(SoundManager.CaptureGetVolume);//Round(SoundManager.CaptureGetVolume*TrackBar3.Max);
    TrackBar3.OnChange := callback;
    TrackBar3Change(NIL);

    // pan
    callback := TrackBar2.OnChange;
    TrackBar2.OnChange := NIL;
    PanToCursor(SoundManager.GetCapturePan);
    TrackBar2.OnChange := callback;
    TrackBar2Change(NIL);

    // effect names
    A := SoundManager.GetEffectsNamesArrayOn(CAPTURE_IDAUDIO);
    case Length(A) of
     1:
       begin
         Label3.Caption := A[0];
         Label4.Caption := ' ';
         Label5.Caption := ' ';
       end;
     2:
       begin
         Label3.Caption := A[0];
         Label4.Caption := A[1];
         Label5.Caption := ' ';
       end;
     3:
       begin
         Label3.Caption := A[0];
         Label4.Caption := A[1];
         Label5.Caption := A[2];
       end;
     else
       begin
         Label3.Caption := ' ';
         Label4.Caption := ' ';
         Label5.Caption := ' ';
       end;
    end;
  end;

  Timer1.Enabled := TRUE;
end;

procedure TFrameAudioCapture.TrackBar1Change(Sender: TObject);
begin
  if Sender = TrackBar1 then
    SoundManager.SetCapturePreAmp(CursorToPreAmp);
  Label7.Caption := SPreAmp+LineEnding+'x'+FormatFloat('0.0', CursorToPreAmp);
end;

procedure TFrameAudioCapture.TrackBar2Change(Sender: TObject);
begin
  if Sender <> NIL then
    SoundManager.SetCapturePan(CursorToPan);
  Label2.Caption := SPan+' '+PanToStringPercent(CursorToPan);
end;

procedure TFrameAudioCapture.TrackBar3Change(Sender: TObject);
begin
  if Sender <> NIL then
    SoundManager.SetCaptureVolume(CursorToVolume);
  Label8.Caption := SVolume+LineEnding+VolumeToStringPercent(CursorToVolume);
end;

function TFrameAudioCapture.CursorToPreAmp: single;
begin
  Result := TrackBar1.Position*0.01;
  Result := Result * Result;
end;

function TFrameAudioCapture.CursorToPan: single;
begin
  Result := TrackBar2.Position*0.01;
end;

procedure TFrameAudioCapture.PanToCursor(AValue: single);
begin
  TrackBar2.Position := Round(AValue*TrackBar2.Max);
end;

function TFrameAudioCapture.CursorToVolume: single;
begin
  Result := TrackBar3.Position/TrackBar3.Max;
  Result := Result*Result;
end;

procedure TFrameAudioCapture.VolumeToCursor(AValue: single);
begin
   TrackBar3.Position := Round(Sqrt(AValue)*TrackBar3.Max);
end;

procedure TFrameAudioCapture.DoRemoveEffect;
begin
  SoundManager.DeleteEffectsOn(CAPTURE_IDAUDIO);
  Label3.Caption := ' ';
  Label4.Caption := ' ';
  Label5.Caption := ' ';
end;

constructor TFrameAudioCapture.Create(aOwner: TComponent);
var ima: TBGRABitmap;
begin
  inherited Create(aOwner);

  FrameCaptureLevels1 := TFrameAudioLevels.Create(Self);
  FrameCaptureLevels1.Parent := Panel7;
  FrameCaptureLevels1.Align := alClient;

  FrameLed1 := TFrameLed.Create(Self);
  FrameLed1.AssociateToPanel(Panel1);
  FrameLed1.GreenType;
  FrameLed1.BlinkWhenOn := True;

  Label3.Caption := ' ';
  Label4.Caption := ' ';
  Label5.Caption := ' ';

  ima := SVGFileToBGRABitmap(GetAppIconImagesFolder+'MicroNotFound.svg', Image1.ClientWidth, Image1.ClientHeight);
  ima.AssignToBitmap(Image1.Picture.Bitmap);
  ima.Free;
end;

procedure TFrameAudioCapture.EraseBackground(DC: HDC);
begin
end;

procedure TFrameAudioCapture.Fill;
begin
  TrackBar1Change(NIL);
  TrackBar2Change(NIL);

  // load audio presets in combobox
  CBFX.Clear;
  try
    CBFX.Items.LoadFromFile(GetAppAudioPresetsFile);
  except
  end;
end;

procedure TFrameAudioCapture.UpdateStringAfterLanguageChange;
begin
  SpeedButton4.Caption := SCenter_;
  Label1.Caption := SAudioCapture_;
  Label8.Caption := SVolume;
  TrackBar1Change(NIL);
  TrackBar2Change(NIL);
end;

procedure TFrameAudioCapture.UpdateVisual;
begin
  if SoundManager.CaptureToPlaybackIsReady then
  begin
    Shape3.Brush.Color := $0003C4FC;
    FModuleIsActivated := True;
    SoundManager.SetOnCaptureBufferEvent( @FrameCaptureLevels1.UpdateProgressBar );
    FrameLed1.State := True;
    Image1.Visible := False;
  end
  else
  begin
    Shape3.Brush.Color := $00D6D6D6;
    FrameCaptureLevels1.SetToZero;
    FrameLed1.State := False;
    if not FModuleIsActivated then Image1.Visible := True;
    FModuleIsActivated := False;
  end;

  BAddFX.Enabled := FModuleIsActivated;
  BRemoveFX.Enabled := FModuleIsActivated;
  if not FModuleIsActivated then CBFX.Visible := False;
end;

end.

