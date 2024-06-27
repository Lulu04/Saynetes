unit frame_audiolevels;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, ComCtrls,
  ALSound;

type

  { TFrameAudioLevels }

  TFrameAudioLevels = class(TFrame)
    Panel1: TPanel;
    Panel2: TPanel;
    ProgressBar1: TProgressBar;
    ProgressBar2: TProgressBar;
    Shape1: TShape;
    Timer1: TTimer;
    procedure Shape1Resize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FCurrentLeftLevel,
    FCurrentRightLevel: single;
    FShowDecibel: boolean;
  public
    procedure UpdateProgressBar(Sender: TALSCaptureContext; const aBuf: TALSCaptureFrameBuffer);
    procedure SetToZero;

    property ShowDecibel: boolean write FShowDecibel;
  end;

implementation
uses Graphics;

{$R *.lfm}

{ TFrameAudioLevels }

procedure TFrameAudioLevels.Timer1Timer(Sender: TObject);
begin
  if Panel1.Tag > 0 then
    Panel1.Tag := Panel1.Tag -1
  else
    Panel1.Color := $00CAF9CE;

  if Panel2.Tag > 0 then
    Panel2.Tag := Panel2.Tag -1
  else
    Panel2.Color := $00CAF9CE;
end;

procedure TFrameAudioLevels.Shape1Resize(Sender: TObject);
var
  w: Integer;
begin
  w := (Shape1.Width -5-5-3) div 2;
  if w < 2 then
    w := 2;

  ProgressBar1.Width := w;
  ProgressBar2.Width := w;
end;

procedure TFrameAudioLevels.UpdateProgressBar(Sender: TALSCaptureContext; const aBuf: TALSCaptureFrameBuffer);
var
  v: single;

  procedure MarkAsClipped(aPanel: TPanel);
  begin
    aPanel.Color := RGBToColor(255,50,0);
    aPanel.Tag := 6;
  end;

begin
  if not FShowDecibel then
  begin
    // Update progress bar - Percent mode
    v := aBuf.ChannelsLevel[0];
    if v >= FCurrentLeftLevel then
      FCurrentLeftLevel := v
    else
      FCurrentLeftLevel := FCurrentLeftLevel-(FCurrentLeftLevel-v)*0.1;

    v := aBuf.ChannelsLevel[1];
    if v >= FCurrentRightLevel then
      FCurrentRightLevel := v
    else
      FCurrentRightLevel := FCurrentRightLevel-(FCurrentRightLevel-v)*0.1;

    ProgressBar1.Position := Round(FCurrentLeftLevel*100);
    ProgressBar2.Position := Round(FCurrentRightLevel*100);
  end
  else
  begin
    // Update progress bar - Decibel mode
    v := aBuf.ChannelsLeveldB[0];
    if v >= FCurrentLeftLevel then
      FCurrentLeftLevel := v
    else
      FCurrentLeftLevel := FCurrentLeftLevel-(FCurrentLeftLevel-v)*0.1;

    v := aBuf.ChannelsLeveldB[1];
    if v >= FCurrentRightLevel then
      FCurrentRightLevel := v
    else
      FCurrentRightLevel := FCurrentRightLevel-(FCurrentRightLevel-v)*0.1;

    ProgressBar1.Position := Round(FCurrentLeftLevel);
    ProgressBar2.Position := Round(FCurrentRightLevel);
  end;

  // if the max amplitude is reach the corresponding panel becomes red to signal
  // the audio can be clipped.
  if aBuf.ChannelsLevel[0] >= 1.0 then
    MarkAsClipped( Panel1 );

  if aBuf.ChannelsLevel[1] >= 1.0 then
    MarkAsClipped( Panel2 );
end;

procedure TFrameAudioLevels.SetToZero;
begin
  ProgressBar1.Position := 0;
  ProgressBar2.Position := 0;
end;

end.

