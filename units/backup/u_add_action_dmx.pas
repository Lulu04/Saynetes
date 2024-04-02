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
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure ProcessOnAddCmdFromViewProjector(Sender: TObject);
  public
    FrameViewProjector1: TFrameViewProjector;
  end;


var
  FormAddDMXAction: TFormAddDMXAction;

implementation

uses u_common, u_global_var, u_mainform, u_list_dmxuniverse;

{$R *.lfm}

{ TFormAddDMXAction }

procedure TFormAddDMXAction.FormCreate(Sender: TObject);
begin
{  FrameViewProjector1 := TFrameViewProjector.Create(Self);
  FrameViewProjector1.Parent := Self;
  FrameViewProjector1.Align := alClient;
  FrameViewProjector1.OnAddCmd := @ProcessOnAddCmdFromViewProjector;
  FrameViewProjector1.GUIMode := guiEditSequence;   }
end;

procedure TFormAddDMXAction.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
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
    FrameViewProjector1.Splitter1.Top := ;
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

