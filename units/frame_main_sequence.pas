unit frame_main_sequence;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, LCLTranslator, LCLType,
  frame_viewtoplist,
  frame_viewprojectfolder,
  frame_intermissionmusic,
  u_common;

type

  { TFrameMainSequence }

  TFrameMainSequence = class(TFrame)
    Label9: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure Splitter1Moved(Sender: TObject);
  private
    FRedocking: boolean;
    procedure RedockPanels;
  public
    FrameViewTopList1: TFrameViewTopList;
    FrameIntermissionMusic1: TFrameIntermissionMusic;
    FrameViewProjectFolder1: TFrameViewProjectFolder;

    constructor Create(aOwner: TComponent); override;
    procedure EraseBackground({%H-}DC: HDC); override;

    procedure Fill;

    function MouseIsOver: boolean;
    procedure ProcessKeyDown(Key: Word; Shift: TShiftState);
    procedure ProcessKeyUp(Key: Word; Shift: TShiftState);

    procedure AddDMXSequenceFromProjectorView(Sender: TObject;
       const aCmd: TSingleCmd; const aShortReadable: string; aDuration: single);


    procedure SaveProjectOptionsTo(t: TStringList);
    procedure LoadProjectOptionsFrom(t: TStringList);

    procedure UpdateLayout;
  end;

implementation
uses u_project_manager, u_list_sequence, u_utils, u_logfile, PropertyUtils;

{$R *.lfm}

{ TFrameMainSequence }

procedure TFrameMainSequence.Splitter1Moved(Sender: TObject);
begin
  if FRedocking then exit;

  if Sender = Splitter1 then
    FWantedTopSplitter1 := Splitter1.Top-1;
  if Sender = Splitter2 then
    FWantedTopSplitter2 := Splitter2.Top-1;

  Project.Options.Save;
  //RedockPanels;
end;

procedure TFrameMainSequence.RedockPanels;
begin
  FRedocking := True;
  Splitter1.Top := FWantedTopSplitter1;
  Splitter2.Top := FWantedTopSplitter2;
  FRedocking := False;
end;

constructor TFrameMainSequence.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FrameViewTopList1 := TFrameViewTopList.Create(Self);
  FrameViewTopList1.Parent := Panel2;
  FrameViewTopList1.Align := alClient;
  FrameViewTopList1.ItemHeight := 30;

  FrameViewProjectFolder1 := TFrameViewProjectFolder.Create(Self);
  FrameViewProjectFolder1.Parent := Panel3;
  FrameViewProjectFolder1.Align := alClient;

  FrameIntermissionMusic1 := TFrameIntermissionMusic.Create(Self);
  FrameIntermissionMusic1.Parent := Panel4;
  FrameIntermissionMusic1.Align := alClient;


  FWantedTopSplitter1 := Panel1.Height;
  FWantedTopSplitter2 := Panel3.Height;
end;

procedure TFrameMainSequence.EraseBackground(DC: HDC);
begin
end;

procedure TFrameMainSequence.Fill;
begin
  Panel1.Enabled := Project.IsReady;
  FrameViewTopList1.Fill;
  FrameViewProjectFolder1.Fill;
  FrameViewTopList1.MouseCanMoveItem := Project.Options.EditMode;
end;

function TFrameMainSequence.MouseIsOver: boolean;
begin
  Result := FrameViewTopList1.MouseIsOver;
end;

procedure TFrameMainSequence.ProcessKeyDown(Key: Word; Shift: TShiftState);
begin
  FrameViewTopList1.ProcessKeyDown(Key, Shift);
end;

procedure TFrameMainSequence.ProcessKeyUp(Key: Word; Shift: TShiftState);
begin
  FrameViewTopList1.ProcessKeyUp(Key, Shift);
end;

procedure TFrameMainSequence.AddDMXSequenceFromProjectorView(Sender: TObject;
  const aCmd: TSingleCmd; const aShortReadable: string; aDuration: single);
begin
  with Sequences.AddSequence(aShortReadable,
              ConstructTSequencerInfoList(aCmd, aShortReadable, aDuration)) do
     FrameViewTopList1.Add(ID);

  Project.SetModified;
end;

const PROJECTOPTION_FRAMESEQUENCER_HEADER='[FRAME_SEQUENCER]';
procedure TFrameMainSequence.SaveProjectOptionsTo(t: TStringList);
var prop: TProperties;
begin
  prop.Init('|');
  prop.Add('HeightPanelProjectList', FWantedTopSplitter2); // Panel3.Height);
  prop.Add('HeightPanelSequenceList', FWantedTopSplitter1); // Panel1.Height);
  t.Add(PROJECTOPTION_FRAMESEQUENCER_HEADER);
  t.Add(prop.PackedProperty);
end;

procedure TFrameMainSequence.LoadProjectOptionsFrom(t: TStringList);
var prop: TProperties;
  k: Integer;
begin
  k := t.IndexOf(PROJECTOPTION_FRAMESEQUENCER_HEADER);
  if (k = -1) or (k = t.Count-1) then
    prop.SetEmpty
  else
    prop.Split(t.Strings[k+1], '|');

  prop.IntegerValueOf('HeightPanelSequenceList', k, Height*3 div 5);
  Panel1.Height := k;
  FWantedTopSplitter1 := k;
  prop.IntegerValueOf('HeightPanelProjectList', k, Height div 5);
  Panel3.Height := k;
  FWantedTopSplitter2 := k;
end;

procedure TFrameMainSequence.UpdateLayout;
begin
  RedockPanels;
end;

end.

