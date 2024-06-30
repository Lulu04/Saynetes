unit u_add_action_dmx;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  frame_viewprojectors;

type

  { TFormAddDMXAction }

  TFormAddDMXAction = class(TForm)
    Panel1: TPanel;
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    procedure ProcessOnAddCmdFromViewProjector(Sender: TObject);
  public
    FrameViewProjector1: TFrameViewProjector;
  end;


var
  FormAddDMXAction: TFormAddDMXAction;

implementation

uses u_common, u_global_var, u_mainform, u_list_dmxuniverse,
  u_dmxtools_channels, u_dmxtools_rgb, u_dmxtools_group;

{$R *.lfm}

{ TFormAddDMXAction }

procedure TFormAddDMXAction.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FormDMXChannelsTools.ClearSelectedChannels;
  FormDMXRGBTools.ClearSelectedFixtures;
  FrameViewProjector1.HideToolsWindows;

  UniverseManager.StopThread;
  FProjectorViewToRefreshForThreadUniverse := FormMain.FrameViewProjector1;
  UniverseManager.StartThread;
end;

procedure TFormAddDMXAction.FormShow(Sender: TObject);
begin
  if FrameViewProjector1 = NIL then
  begin
    FrameViewProjector1 := TFrameViewProjector.Create(Self);
    FrameViewProjector1.Parent := Self;
    FrameViewProjector1.Align := alClient;
    FrameViewProjector1.OnAddCmd := @ProcessOnAddCmdFromViewProjector;
    FrameViewProjector1.GUIMode := guiEditSequence;
    FrameViewProjector1.Splitter1.Top := Round(FrameViewProjector1.ClientHeight*0.6);
  end;

  UniverseManager.StopThread;
  FProjectorViewToRefreshForThreadUniverse := FrameViewProjector1;
  UniverseManager.StartThread;

  FrameViewProjector1.FillComboBoxUniverseToShow;

  FrameViewProjector1.SetSelectionFromUniverseManager;
//  FrameViewProjector1.Redraw;
end;

procedure TFormAddDMXAction.ProcessOnAddCmdFromViewProjector(Sender: TObject);
begin
  ModalResult := mrOk;
end;

end.

