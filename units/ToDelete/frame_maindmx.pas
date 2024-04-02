unit frame_maindmx;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, LCLType;

type

  { TFrameMainDMX }

  TFrameMainDMX = class(TFrame)
    Panel1: TPanel;
  private
    procedure ProcessAddCmdEvent(Sender: TObject);
  public
    procedure ProcessUniverseManagerQueryRefreshViewEvent;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure EraseBackground({%H-}DC: HDC); override;

    procedure RedirectCallBacks;
    procedure Fill;
    procedure Redraw;

    // call it when user change between EDIT and SHOW mode
    procedure UpdateEditMode;
  end;

implementation

uses u_dmxtools_channels, u_dmxtools_rgb, u_mainform, u_list_top, u_utils,
  u_project_manager, u_dmxtools_group, u_list_dmxuniverse, u_main_viewprojector;

{$R *.lfm}

{ TFrameMainDMX }

procedure TFrameMainDMX.ProcessAddCmdEvent(Sender: TObject);
begin
  with Sequences.AddTop(FrameViewDMXProjectors1.ShortReadableString,
              ConstructTSequencerInfoList(FrameViewDMXProjectors1.Cmds,
                            FrameViewDMXProjectors1.ShortReadableString,
                            FrameViewDMXProjectors1.CmdDuration)) do
     FormMain.FrameViewTopList1.Add(ID);

  Project.SetModified;
end;

procedure TFrameMainDMX.ProcessUniverseManagerQueryRefreshViewEvent;
begin
 // if FrameViewDMXProjectors1.ShowChannelsLevel
 //   then FrameViewDMXProjectors1.Redraw;
  FrameViewDMXProjectors1.FrameViewDMXCursors1.Redraw;
end;

constructor TFrameMainDMX.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FrameViewDMXProjectors1:=TFrameViewDMXProjectors.Create(Self);
  FrameViewDMXProjectors1.Parent:=Panel1;
  FrameViewDMXProjectors1.Align:=alClient;
  FrameViewDMXProjectors1.SetViewModeMainDMX;
end;

procedure TFrameMainDMX.EraseBackground(DC: HDC);
begin
// do nothing here
end;

procedure TFrameMainDMX.RedirectCallBacks;
begin
  FormDMXChannelsTools.InitForMainDmxView;
  FormDMXRGBTools.InitForMainDmxView;

  FormDMXChannelsTools.TargetFrameProjector:=FrameViewDMXProjectors1;
  FormDMXRGBTools.TargetFrameProjector:=FrameViewDMXProjectors1;
  FormDMXGroup.TargetFrameProjector:=FrameViewDMXProjectors1;

  FrameViewDMXProjectors1.OnAddCmd:=@ProcessAddCmdEvent;
  FrameViewDMXProjectors1.Sel_None;
  Redraw;

  UniverseManager.OnRefreshView:=@ProcessUniverseManagerQueryRefreshViewEvent;
end;

procedure TFrameMainDMX.Fill;
begin
  FrameViewDMXProjectors1.FillComboBoxUniverseToShow;
  FrameViewDMXProjectors1.Sel_None;
  FrameViewDMXProjectors1.UpdateButtons;
  FrameViewDMXProjectors1.Redraw;
end;

procedure TFrameMainDMX.Redraw;
begin
  FrameViewDMXProjectors1.Redraw;
  FrameViewDMXProjectors1.FrameViewDMXCursors1.Redraw;
end;

procedure TFrameMainDMX.UpdateEditMode;
begin
  FrameViewDMXProjectors1.UpdateButtons;
end;

end.

