unit frame_color_palette;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, ExtCtrls, StdCtrls, Spin,
  Graphics, LCLType, Buttons, LCLTranslator,
  lightcolorgradient, u_presetmanager;

type

  { TFrame_ColorPalette }

  TFrame_ColorPalette = class(TFrame)
    BPreset: TSpeedButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    PB1: TPaintBox;
    PB2: TPaintBox;
    SE1: TSpinEdit;
    SE2: TSpinEdit;
    SE3: TSpinEdit;
    Shape1: TShape;
    SpeedButton1: TSpeedButton;
    procedure SpeedButton1Click(Sender: TObject);
  private
   FOnChange: TNotifyEvent;
   FGradient: TLightColorGradient;
   function GetChartColor: TColorChartColor;
   function GetSelectedColor: TColor;
   procedure ProcessColorChange( Sender: TObject );
   procedure SetChartColor(AValue: TColorChartColor);
   procedure SetSelectedColor(AValue: TColor);
  private
   FPresetManager: TPresetManager;
   function ColorToPreset: string;
   function GetShapeColor: TColorChartColor;
   procedure PresetToColor(const A: TStringArray);
  public
   constructor Create( TheOwner: TComponent ); override;
   destructor Destroy; override;
   procedure EraseBackground({%H-}DC: HDC); override;

   procedure InitOnShow;
   procedure UpdateStringAfterLanguageChange;

   property SelectedColor: TColor read GetSelectedColor write SetSelectedColor;
   // gives the base color and the percentage applyed on by the cursor of color chart
   property ChartColor: TColorChartColor read GetChartColor write SetChartColor;
   // gives the color seen on the rectangle shape, the percentage field is 0.
   property ShapeColor: TColorChartColor read GetShapeColor;
   property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

uses u_resource_string, u_project_manager, u_common;

{ TFrame_ColorPalette }

{procedure TFrame_ColorPalette.FrameResize(Sender: TObject);
begin
 PB1.Width := Width div 2;
 PB1.Top := 0;
 PB1.Left := 0;
end;   }

procedure TFrame_ColorPalette.SpeedButton1Click(Sender: TObject);
begin
  FGradient.ResetCursorToMiddle;
end;

procedure TFrame_ColorPalette.ProcessColorChange(Sender: TObject);
begin
  if FOnChange <> NIL then
    FOnChange( Self );
end;

function TFrame_ColorPalette.GetChartColor: TColorChartColor;
begin
  Result := FGradient.ChartColor;
end;

function TFrame_ColorPalette.GetSelectedColor: TColor;
begin
  Result := FGradient.SelectedColor;
end;

procedure TFrame_ColorPalette.SetChartColor(AValue: TColorChartColor);
begin
  FGradient.ChartColor := AValue;
end;

procedure TFrame_ColorPalette.SetSelectedColor(AValue: TColor);
begin
  FGradient.SelectedColor := AValue;
end;

function TFrame_ColorPalette.ColorToPreset: string;
begin
  Result := FGradient.ChartColor.ToString;
end;

function TFrame_ColorPalette.GetShapeColor: TColorChartColor;
begin
  Result.Percentage := 0.0;
  Result.BaseColor := Shape1.Brush.Color;
end;

procedure TFrame_ColorPalette.PresetToColor(const A: TStringArray);
var c: TColorChartColor;
begin
  c.Percentage := 0.5; // to avoid hint compilation
  c.InitFromString(A[0]);
  FGradient.ChartColor := c;
end;

constructor TFrame_ColorPalette.Create( TheOwner: TComponent );
begin
  inherited Create( TheOwner );
  FGradient := TLightColorGradient.Create;
  FGradient.AssignPaintBox( PB1, PB2 );
  FGradient.AssignSpinEditAndShape( SE1, SE2, SE3, Shape1 );
  FGradient.OnColorChange := @ProcessColorChange;

  FPresetManager := TPresetManager.Create(Self);
  FPresetManager.Init1(SColorPresets, BPreset,
                  ConcatPaths([Project.AppPresetsFolder, 'SingleColor'+PRESET_FILE_EXTENSION]));
  FPresetManager.Init2(@PresetToColor, @ColorToPreset);
end;

destructor TFrame_ColorPalette.Destroy;
begin
  FGradient.Free;
  inherited Destroy;
end;

procedure TFrame_ColorPalette.EraseBackground(DC: HDC);
begin
end;

procedure TFrame_ColorPalette.InitOnShow;
begin
  UpdateStringAfterLanguageChange;

  PB1.Width := Width div 2;
end;

procedure TFrame_ColorPalette.UpdateStringAfterLanguageChange;
begin
  Label1.Caption := SRed;
  Label2.Caption := SGreen;
  Label3.Caption := SBlue;
  FPresetManager.UpdateStringAfterLanguageChange;
end;

initialization
  {$I frame_color_palette.lrs}

end.

