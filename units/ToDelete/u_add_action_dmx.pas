unit u_add_action_dmx;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs;

type

  { TFormDMXAction }

  TFormDMXAction = class(TForm)
    procedure FormShow(Sender: TObject);
  private
    procedure ProcessAddCmd(Sender: TObject);
    procedure ProcessUniverseManagerQueryRefreshViewEvent;
  public
  end;

var
  FormDMXAction: TFormDMXAction;

implementation

uses u_dmxtools_channels, u_dmxtools_rgb, u_main_viewprojector, u_common;

{$R *.lfm}

{ TFormDMXAction }

procedure TFormDMXAction.FormShow(Sender: TObject);
begin
  with FormViewProjector do begin
    FillComboBoxUniverseToShow;
    //ReloadTexture;
    Sel_None;
    Redraw;
    FrameViewDMXCursors1.Redraw;
    GUIMode := guiEditSequence;
  end;

  FormDMXChannelsTools.InitForTopEdition;
  FormDMXRGBTools.InitForTopEdition;

//  UniverseManager.OnRefreshView := @ProcessUniverseManagerQueryRefreshViewEvent;
end;

procedure TFormDMXAction.ProcessAddCmd(Sender: TObject);
begin
  FormViewProjector.HideToolsWindows;
  ModalResult:=mrOk;
end;

procedure TFormDMXAction.ProcessUniverseManagerQueryRefreshViewEvent;
begin
  if FormViewProjector.ChannelsLevelAreVisible then
    FormViewProjector.Redraw;

  FormViewProjector.FrameViewDMXCursors1.Redraw;
end;

end.

