unit u_add_action_dmx;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  frame_viewprojectors, frame_sequencer;

type

  { TFormAddDMXAction }

  TFormAddDMXAction = class(TForm)
    Panel1: TPanel;
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormDeactivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure ProcessOnAddCmdFromViewProjector(Sender: TObject);
  public
    FrameViewProjector1: TFrameViewProjector;
    ParentSequencer: TFrameSequencer;
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
  FormMain.FrameViewProjector1.ReloadWidgetStateFromClassVar;
  FProjectorViewToRefreshForThreadUniverse := FormMain.FrameViewProjector1;
  UniverseManager.StartThread;

  CloseAction := caFree;
end;

procedure TFormAddDMXAction.FormDeactivate(Sender: TObject);
begin
  {$if defined(LCLGTK2)}
  Show;
  {$endif}
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
{$if defined(LCLGTK2)}var s: TSequenceStep;{$endif}
begin
  {$if defined(LCLGTK2)}
  ParentSequencer.Sel_SelectNone;
  s := TSequenceStep.Create;
  s.ParentSeq := ParentSequencer;
  s.TimePos := ParentSequencer.ClickedTimePos;
  s.Top := ParentSequencer.ClickedY;
  s.Caption := FrameViewProjector1.ShortReadableString;
  s.CmdList := FrameViewProjector1.Cmds;
  s.Duration := FrameViewProjector1.CmdDuration;
  ParentSequencer.Add(s, TRUE);
  ParentSequencer.ForceReconstructOpenGLObjects;
  Close;
  {$else}
  ModalResult := mrOk;
  {$endif}
end;

end.

