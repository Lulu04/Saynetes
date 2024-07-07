unit frame_trackbar_customized;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, frame_trackbar;

// here, we  declare TFrameTrackBar descendant that need customized range value
type

{ TCustomFrameTB }

TCustomFrameTB = class(TFrameTrackBar)
private
  function GetValue: single; virtual;
  procedure SetValue(AValue: single); virtual;
public
  // The value of the trackbar expressed in the needed range.
  // example: -1 to +1 for a Pan trackbar
  property Value: single read GetValue write SetValue;
end;

// Audio

{ TFrameTBAudioVolume }

TFrameTBAudioVolume = class(TCustomFrameTB);

{ TFrameTBAudioPan }

TFrameTBAudioPan = class(TCustomFrameTB)
private
  function GetValue: single; override;
  procedure SetValue(AValue: single); override;
public
  function GetLegend: string; override;
end;

{ TFrameTBAudioPitch }

TFrameTBAudioPitch = class(TCustomFrameTB)
private
  function GetValue: single; override;
  procedure SetValue(AValue: single); override;
public
  function GetLegend: string; override;
end;

TFrameTBAudioCapturePreAmp = class(TCustomFrameTB)
private
  function GetValue: single; override;
  procedure SetValue(AValue: single); override;
end;

TFrameTBAudioDryWet = class(TCustomFrameTB);

// Sequence

TFrameTBSequenceStretchSpeed = class(TCustomFrameTB)
private
  function GetValue: single; override;
  procedure SetValue(AValue: single); override;
end;

// dmx channel

{ TFrameTBDmxLevel }
// Value: 0.0 to 1.0
TFrameTBDmxLevel = class(TCustomFrameTB)
  function GetLegend: string; override;
end;



{ TFrameTBDmxFlameWait }
// Value: 0.05 to 2.0
TFrameTBDmxFlameWait = class(TCustomFrameTB)
private
  function GetValue: single; override;
  procedure SetValue(AValue: single); override;
public
  function GetLegend: string; override;
end;


{ TFrameTBDmxFlameSoften }
// Value: 0.0 to 1.0
TFrameTBDmxFlameSoften = class(TCustomFrameTB)
  function GetLegend: string; override;
end;


{ TFrameTBDmxAudioFollowerGain }
// Value: -1.0 to 3.0
TFrameTBDmxAudioFollowerGain = class(TCustomFrameTB)
private
  function GetValue: single; override;
  procedure SetValue(AValue: single); override;
public
  function GetLegend: string; override;
end;

// Value: 0.0 to 1.0
TFrameTBDmxAudioFollowerBrightness = class(TFrameTBDmxLevel);

// Value: 0.1 to 1.0

{ TFrameTBDmxAudioFollowerSoften }

TFrameTBDmxAudioFollowerSoften = class(TCustomFrameTB)
private
  function GetValue: single; override;
  procedure SetValue(AValue: single); override;
public
  function GetLegend: string; override;
end;


// dmx RGB

// Value: 0.05 to 2.0
TFrameTBDmxFlameRGBWait = class(TFrameTBDmxFlameWait);

// Value: 0.0 to 1.0

{ TFrameTBDmxFlameRGBAmplitude }

TFrameTBDmxFlameRGBAmplitude = class(TCustomFrameTB)
  function GetLegend: string; override;
end;

// Value: 0.0 to 1.0
TFrameTBDmxFlameRGBSoften = class(TFrameTBDmxFlameSoften);



implementation

uses u_resource_string, u_utils;

{ TFrameTBDmxFlameRGBAmplitude }

function TFrameTBDmxFlameRGBAmplitude.GetLegend: string;
begin
  Result := FormatFloat('0.0', Value*100)+'%';
end;

{ TFrameTBDmxFlameSoften }

function TFrameTBDmxFlameSoften.GetLegend: string;
begin
  Result := FormatFloat('0.0', Value*100)+'%';
end;

{ TFrameTBDmxLevel }

function TFrameTBDmxLevel.GetLegend: string;
var v: single;
begin
  v := Value;
  Result := Round(v*255).ToString+'  ('+FormatFloat('0.0', v*100)+'%)';
end;

{ TFrameTBSequenceStretchSpeed }

function TFrameTBSequenceStretchSpeed.GetValue: single;
begin
  Result := PercentValue * 6.95 + 0.5;        // 0.5 to 7.0
end;

procedure TFrameTBSequenceStretchSpeed.SetValue(AValue: single);
begin
  PercentValue := (AValue - 0.5) / 6.95;
end;

{ TCustomFrameTB }

function TCustomFrameTB.GetValue: single;
begin
  Result := PercentValue;
end;

procedure TCustomFrameTB.SetValue(AValue: single);
begin
  PercentValue := AValue;
end;

{ TFrameTBDmxAudioFollowerSoften }

function TFrameTBDmxAudioFollowerSoften.GetValue: single;
begin
  Result := PercentValue * 0.9 + 0.1;
end;

procedure TFrameTBDmxAudioFollowerSoften.SetValue(AValue: single);
begin
  PercentValue := (AValue - 0.1) / 0.9;
end;

function TFrameTBDmxAudioFollowerSoften.GetLegend: string;
begin
  Result := FormatFloat('0.00', Value) + ' '+SSec;
end;

{ TFrameTBDmxAudioFollowerGain }

function TFrameTBDmxAudioFollowerGain.GetValue: single;
begin
  Result := PercentValue * 4.0 - 1.0;
  Result := Round(Result*100 / 5)*0.01*5;
end;

procedure TFrameTBDmxAudioFollowerGain.SetValue(AValue: single);
begin
  PercentValue := (AValue + 1.0) * 0.25;
end;

function TFrameTBDmxAudioFollowerGain.GetLegend: string;
var v: single;
begin
  v := Value;
  Result := FormatFloat('0.00', v);
  if v > 0 then Result := '+' + Result;
end;

{ TFrameTBAudioCapturePreAmp }

function TFrameTBAudioCapturePreAmp.GetValue: single;
begin
  Result := PercentValue * 8.0;
end;

procedure TFrameTBAudioCapturePreAmp.SetValue(AValue: single);
begin
  PercentValue := AValue / 8.0;
end;

{ TFrameTBDmxFlameWait }

function TFrameTBDmxFlameWait.GetValue: single;
begin
  Result := PercentValue * 1.95 + 0.05;
end;

procedure TFrameTBDmxFlameWait.SetValue(AValue: single);
begin
  PercentValue := (AValue - 0.05) / 1.95;
end;

function TFrameTBDmxFlameWait.GetLegend: string;
begin
  Result := FormatFloat('0.00', Value)+' '+SSec;
end;

{ TFrameTBAudioPitch }

function TFrameTBAudioPitch.GetValue: single;
begin
  Result := PercentValue * 3.9 + 0.1;
end;

procedure TFrameTBAudioPitch.SetValue(AValue: single);
begin
  PercentValue := (AValue - 0.1) / 3.9;
end;

function TFrameTBAudioPitch.GetLegend: string;
begin
  Result := PitchToString(Value);
end;

{ TFrameTBAudioPan }

function TFrameTBAudioPan.GetValue: single;
begin
  Result := PercentValue * 2 - 1.0;
  Result := Round(Result * 100) * 0.01;
end;

procedure TFrameTBAudioPan.SetValue(AValue: single);
begin
  PercentValue := (AValue + 1.0) * 0.5;
end;

function TFrameTBAudioPan.GetLegend: string;
begin
  Result := PanToStringPercent(Value);
end;

end.

