unit u_add_action_audio;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, ExtCtrls,
  frame_cmd_audio, frame_cmd_audiofx, u_notebook_util, u_common;

type

  { TFormAudioAction }

  TFormAudioAction = class(TForm)
    Notebook1: TNotebook;
    PageEffects: TPage;
    PageSounds: TPage;
    Shape1: TShape;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure PageEffectsBeforeShow(Sender: TObject; {%H-}ANewPage: TPage;
      {%H-}ANewIndex: Integer);
    procedure PageSoundsBeforeShow(Sender: TObject; {%H-}ANewPage: TPage;
      {%H-}ANewIndex: Integer);
  private
    FCmds: TCmdList;
    FCmdDuration: single;
    FShortReadableString: string;
    procedure ProcessOnAddCmd(Sender: TObject);
    procedure ProcessAnAddCmdFX(Sender: TObject);
    procedure ProcessPageSelectionChange(Sender: TObject);
   public
    FrameCmdAudio1: TFrameCmdAudio;
    FrameCmdAudioFX1: TFrameCmdAudioFX;
    FNoteBookManager: TNoteBookManager;

    procedure FillAudioList;

    property Cmds: TCmdList read FCmds;
    property ShortReadableString: string read FShortReadableString;
    property CmdDuration: single read FCmdDuration;
  end;

var
  FormAudioAction: TFormAudioAction;

implementation
uses u_audio_manager, LCLType;

{$R *.lfm}

{ TFormAudioAction }

procedure TFormAudioAction.FormCreate(Sender: TObject);
begin
  FrameCmdAudio1 := TFrameCmdAudio.Create(Self);
  FrameCmdAudio1.Parent := PageSounds;
  FrameCmdAudio1.Align := alClient;
  FrameCmdAudio1.OnAddCmd := @ProcessOnAddCmd;

  FrameCmdAudioFX1 := TFrameCmdAudioFX.Create(Self);
  FrameCmdAudioFX1.Parent := PageEffects;
  FrameCmdAudioFX1.Align := alClient;
  FrameCmdAudioFX1.OnAddCmd := @ProcessAnAddCmdFX;

  FNoteBookManager := TNoteBookManager.Create(Notebook1);
  with FNoteBookManager do
  begin
    SetActivatedColors($0003C4FC, clBlack);
    SetDeactivatedColors($00484848, $00EAEAEA);
    LinkButtonToPage(SpeedButton1, PageSounds);
    LinkButtonToPage(SpeedButton2, PageEffects);
    OnSelectionChange := @ProcessPageSelectionChange;
    ActivePage(PageSounds);
  end;
end;

procedure TFormAudioAction.FormDestroy(Sender: TObject);
begin
  FNoteBookManager.Free;
end;

procedure TFormAudioAction.FormHide(Sender: TObject);
begin
  FrameCmdAudio1.FrameViewAudioList1.StopTimer;

  SoundManager.StopAllSound(True);
  SoundManager.DeleteAllEffects(True);
end;

procedure TFormAudioAction.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

procedure TFormAudioAction.FormShow(Sender: TObject);
begin
  FillAudioList;
end;

procedure TFormAudioAction.PageEffectsBeforeShow(Sender: TObject;
  ANewPage: TPage; ANewIndex: Integer);
begin
  FrameCmdAudioFX1.ProcessSourceChangeEvent(NIL);
end;

procedure TFormAudioAction.PageSoundsBeforeShow(Sender: TObject;
  ANewPage: TPage; ANewIndex: Integer);
begin
  FrameCmdAudio1.ProcessSourceChangeEvent(NIL);
end;

procedure TFormAudioAction.ProcessOnAddCmd(Sender: TObject);
begin
  FCmds := FrameCmdAudio1.Cmds;
  FCmdDuration := FrameCmdAudio1.CmdDuration;
  FShortReadableString := FrameCmdAudio1.ShortReadableString;

//  SoundManager.DeleteAllEffects;
//  SoundManager.StopAllSound;

  ModalResult := mrOk;
end;

procedure TFormAudioAction.ProcessAnAddCmdFX(Sender: TObject);
begin
  FCmds := FrameCmdAudioFX1.Cmds;
  FCmdDuration := FrameCmdAudioFX1.CmdDuration;
  FShortReadableString := FrameCmdAudioFX1.ShortReadableString;

//  SoundManager.DeleteAllEffects;
//  SoundManager.StopAllSound;

  ModalResult := mrOk;
end;

procedure TFormAudioAction.ProcessPageSelectionChange(Sender: TObject);
begin
  SoundManager.StopAllSound(True);
  SoundManager.DeleteAllEffects(True);
end;

procedure TFormAudioAction.FillAudioList;
begin
  FrameCmdAudio1.Init;
  FrameCmdAudioFX1.Init;
end;

end.

