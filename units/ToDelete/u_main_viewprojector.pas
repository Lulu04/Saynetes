unit u_main_viewprojector;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, LCLType,  Menus,
  Buttons, Graphics, frame_bglvirtualscreen_sequencer,  LCLTranslator,
  BGRABitmap, BGRABitmapTypes, BGLVirtualScreen, BGRAOpenGL,
  BGRASVG,
  u_common, frame_velocity, u_notebook_util, frame_viewprojectors;

type
  { TFormViewProjector }

  TFormViewProjector = class(TForm)
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
  public
    FrameViewProjector1: TFrameViewProjector;
  end;

var
  FormViewProjector: TFormViewProjector;

implementation
uses u_project_manager, u_resource_string, u_mainform, ComCtrls, Dialogs;

{$R *.lfm}

{ TFormViewProjector }

procedure TFormViewProjector.FormCreate(Sender: TObject);
begin
  FrameViewProjector1 := TFrameViewProjector.Create(Self);
  FrameViewProjector1.Parent := Panel1;
  FrameViewProjector1.Align := alClient;
end;

procedure TFormViewProjector.FormDestroy(Sender: TObject);
begin
end;

procedure TFormViewProjector.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key in [VK_F1,VK_F2,VK_F3] then
    FormMain.FormKeyDown(Self, Key, Shift);
end;

procedure TFormViewProjector.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  FrameViewProjector1.ProcessKeyUp(Key, Shift);
end;

procedure TFormViewProjector.FormShow(Sender: TObject);
begin
  Enabled := Project.IsReady;

  FrameViewProjector1.MIDelete.Caption := SDelete+'...';

  FrameViewProjector1.Redraw;
end;

end.

