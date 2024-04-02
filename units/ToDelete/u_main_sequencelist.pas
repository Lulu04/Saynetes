unit u_main_sequencelist;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, LCLTranslator,
  frame_viewtoplist,
  frame_viewprojectfolder,
  frame_intersessionmusic,
  u_common;

type

  { TFormMainSequenceList }

  TFormMainSequenceList = class(TForm)
    Label9: TLabel;
    Panel2: TPanel;
    Panel1: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
    procedure Panel3Resize(Sender: TObject);
  private
    FHeightSequenceListPercent,
    FHeightProjectListPercent: single;
  public
    FrameViewTopList1: TFrameViewTopList;
    FrameIntersessionMusic1: TFrameIntersessionMusic;
    FrameViewProjectFolder1: TFrameViewProjectFolder;

    procedure AddDMXSequenceFromProjectorView(Sender: TObject;
       const aCmd: TSingleCmd; const aShortReadable: string; aDuration: single);

    procedure Fill;

    procedure SaveProgramPreferencesTo(t: TStringList);
    procedure LoadProgramPreferencesFrom(t: TStringList);

  end;

var
  FormMainSequenceList: TFormMainSequenceList;

implementation
uses u_project_manager, u_mainform, u_list_top, u_utils, u_logfile, LCLType,
  Math;

{$R *.lfm}

{ TFormMainSequenceList }

procedure TFormMainSequenceList.FormCreate(Sender: TObject);
begin
  FHeightSequenceListPercent := 3/5;
  FHeightProjectListPercent := 1/5;

  FrameViewTopList1 := TFrameViewTopList.Create(Self);
  FrameViewTopList1.Parent := Panel2;
  FrameViewTopList1.Align := alClient;
  FrameViewTopList1.ItemHeight := 30;

  FrameViewProjectFolder1 := TFrameViewProjectFolder.Create(Self);
  FrameViewProjectFolder1.Parent := Panel3;
  FrameViewProjectFolder1.Align := alClient;

  FrameIntersessionMusic1 := TFrameIntersessionMusic.Create(Self);
  FrameIntersessionMusic1.Parent := Panel4;
  FrameIntersessionMusic1.Align := alClient;

  FormMain.FRightAttachedForm := Self;
end;

procedure TFormMainSequenceList.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key in [VK_F1, VK_F2, VK_F3] then
    FormMain.FormKeyDown(Self, Key, Shift)
  else
    FrameViewTopList1.ProcessKeyDown(Key, Shift);
end;

procedure TFormMainSequenceList.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  FrameViewTopList1.ProcessKeyUp(Key, Shift);
end;

procedure TFormMainSequenceList.FormResize(Sender: TObject);
var v: integer;
begin
  if ClientHeight = 0 then exit;

  v := Round(ClientHeight*FHeightSequenceListPercent);
  Panel1.Height := Round(ClientHeight*FHeightSequenceListPercent);

  v := Round(ClientHeight*FHeightProjectListPercent);
  Panel3.Height := Round(ClientHeight*FHeightProjectListPercent);
end;

procedure TFormMainSequenceList.FormShow(Sender: TObject);
begin
  Panel1.Enabled := Project.IsReady;

  FrameViewTopList1.MouseCanMoveItem := Project.Options.EditMode;

  Panel1.Height := Round(ClientHeight*FHeightSequenceListPercent);

  Panel3.Height := Round(ClientHeight*FHeightProjectListPercent);
end;

procedure TFormMainSequenceList.Panel1Resize(Sender: TObject);
begin
  if (ClientHeight > 0) and (ClientHeight > Panel1.Height) then
    FHeightSequenceListPercent := Panel1.Height/ClientHeight;
end;

procedure TFormMainSequenceList.Panel3Resize(Sender: TObject);
begin
  if (ClientHeight > 0) and (ClientHeight > Panel3.Height) then
    FHeightProjectListPercent := Panel3.Height/ClientHeight;
end;

procedure TFormMainSequenceList.AddDMXSequenceFromProjectorView(Sender: TObject;
  const aCmd: TSingleCmd; const aShortReadable: string; aDuration: single);
begin
  with Sequences.AddTop(aShortReadable,
              ConstructTSequencerInfoList(aCmd, aShortReadable, aDuration)) do
     FrameViewTopList1.Add(ID);

  Project.SetModified;
end;

procedure TFormMainSequenceList.Fill;
begin
  FrameViewTopList1.Fill;
  Panel1.Enabled := Project.IsReady;

  FrameViewProjectFolder1.Fill;
end;

const WINDOW_SEQUENCE_LIST_HEADER = '[MAIN SEQUENCE WINDOW]';
procedure TFormMainSequenceList.SaveProgramPreferencesTo(t: TStringList);
var prop: TPackProperty;
begin
  prop.Init('|');
  if FHeightSequenceListPercent < 1 then
    prop.Add('SequenceHeightPercent', FHeightSequenceListPercent);
  if FHeightProjectListPercent < 1 then
    prop.Add('ProjectHeightPercent', FHeightProjectListPercent);

  t.Add(WINDOW_SEQUENCE_LIST_HEADER);
  t.Add(prop.PackedProperty);
end;

procedure TFormMainSequenceList.LoadProgramPreferencesFrom(t: TStringList);
var prop: TSplitProperty;
  k: integer;
begin
  k := t.IndexOf(WINDOW_SEQUENCE_LIST_HEADER);
  if (k = -1) or (k = t.Count-1) then
    prop.SetEmpty
  else
    prop.Split(t.Strings[k+1], '|');

  if not prop.SingleValueOf('SequenceHeightPercent', FHeightSequenceListPercent, 1/5) then
    Log.Warning('TFormMainSequenceList.LoadProgramPreferences - SequenceHeightPercent property NOT FOUND');

  if not prop.SingleValueOf('ProjectHeightPercent', FHeightProjectListPercent, 1/5) then
    Log.Warning('TFormMainSequenceList.LoadProgramPreferences - ProjectHeightPercent property NOT FOUND');
end;

end.

