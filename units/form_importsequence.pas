unit form_importsequence;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Buttons,
  StdCtrls,
  u_list_sequence;

type

  { TFormImportSequence }

  TFormImportSequence = class(TForm)
    BHelp: TSpeedButton;
    BImport: TSpeedButton;
    BSelectAll: TSpeedButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    OD1: TOpenDialog;
    BOpenProject: TSpeedButton;
    BSelectNone: TSpeedButton;
    TV: TTreeView;
    procedure BHelpClick(Sender: TObject);
    procedure BImportClick(Sender: TObject);
    procedure BOpenProjectClick(Sender: TObject);
    procedure BSelectAllClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure TVMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TVSelectionChanged(Sender: TObject);
  private
    FImportedSequences: TSequenceList;
    function GetSelectedSequences: TStringArray;
  public

  end;


implementation

uses u_resource_string, u_project_manager, u_program_options, u_common,
  form_help, LCLType;

{$R *.lfm}

{ TFormImportSequence }

procedure TFormImportSequence.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then ModalResult := mrCancel;
end;

procedure TFormImportSequence.BOpenProjectClick(Sender: TObject);
var i: integer;
  t: TStringList;
  n, n1: TTreeNode;
begin
  Label2.Caption := ' ';
  TV.Items.Clear;
  FImportedSequences.ClearAll;
  BSelectAll.Enabled := False;
  BSelectNone.Enabled := False;
  BImport.Enabled := False;

  OD1.InitialDir := ExtractFilePath(ProgramOptions.LastProjectFileNameUsed);

  if not OD1.Execute then begin
    ModalResult := mrCancel;
    exit;
  end;
  if OD1.FileName = Project.Filename then exit;
  if ExtractFileExt(OD1.FileName) <> PROJECT_FILE_EXTENSION then exit;
  Label2.Caption := OD1.FileName;

  t := TStringList.Create;
  try
    t.LoadFromFile(OD1.FileName);
    FImportedSequences.Load(t);
  finally
    t.Free;
  end;
  if FImportedSequences.Count = 0 then exit;

  // populates the treeview with the loaded sequences
  TV.BeginUpdate;
  n := TV.Items.Add(NIL, '');
  for i:=0 to FImportedSequences.Count-1 do begin
    n1 := TV.Items.AddChild(n, FImportedSequences.GetSequenceByIndex(i).Name);
    n1.ImageIndex := 53; // unchecked
    n1.SelectedIndex := 53;
  end;
  TV.EndUpdate;
  TV.Items.GetFirstNode.Expand(False);

  BSelectAll.Enabled := True;
  BSelectNone.Enabled := True;
end;

procedure TFormImportSequence.BImportClick(Sender: TObject);
var i: integer;
  A: TStringArray;
  importedSeq: TSequence;
begin
  A := GetSelectedSequences;
  if Length(A) = 0 then exit;

  for i:=0 to High(A) do begin
    importedSeq := FImportedSequences.GetSequenceByName(A[i]);
    if importedSeq <> NIL then begin
      Sequences.AddSequence(importedSeq.Name, importedSeq.SequencerInfoList);
    end;
  end;

  Sequences.CheckErrorInSequences;
  Project.SetModified;
  ModalResult := mrOk;
end;

procedure TFormImportSequence.BHelpClick(Sender: TObject);
begin
  _ShowHelp(HelpImportSequence, BHelp);
end;

procedure TFormImportSequence.BSelectAllClick(Sender: TObject);
var i, imaIndex: integer;
  n: TTreeNode;
begin
  if Sender = BSelectAll then imaIndex := 52
    else imaIndex := 53;
  n := TV.Items.GetFirstNode;
  if n = NIL then exit;

  for i:=0 to n.Count-1 do begin
    n.Items[i].ImageIndex := imaIndex;
    n.SelectedIndex := imaIndex;
  end;

  BImport.Enabled := Length(GetSelectedSequences) > 0;
end;

procedure TFormImportSequence.FormCreate(Sender: TObject);
begin
  FImportedSequences := TSequenceList.Create;
end;

procedure TFormImportSequence.FormDestroy(Sender: TObject);
begin
  FImportedSequences.Free;
end;

procedure TFormImportSequence.FormShow(Sender: TObject);
begin
  Label2.Caption := ' ';
  BSelectNone.Caption := SNone;
  BSelectAll.Caption := SAll_;
  BImport.Caption := SImport;
end;

procedure TFormImportSequence.TVMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var n: TTreeNode;
begin
  n := TV.GetNodeAt(X, Y);
  if n = NIL then exit;

  if n.ImageIndex = 53 then begin
    n.SelectedIndex := 52;
    n.ImageIndex := 52;
  end else begin
    n.SelectedIndex := 53;
    n.ImageIndex := 53;
  end;

  BImport.Enabled := Length(GetSelectedSequences) > 0;
end;

procedure TFormImportSequence.TVSelectionChanged(Sender: TObject);
begin
  TV.Selected := NIL;
end;

function TFormImportSequence.GetSelectedSequences: TStringArray;
var n: TTreeNode;
  i, c: integer;
begin
  Result := NIL;
  n := TV.Items.GetFirstNode;
  if n = NIL then exit;
  if n.Count = 0 then exit;

  // retrieve the selected count
  c := 0;
  for i:=0 to n.Count-1 do
    if n.Items[i].ImageIndex = 52 then
      inc(c);

  if c = 0 then exit;
  SetLength(Result, c);

  c := 0;
  for i:=0 to n.Count-1 do
    if n.Items[i].ImageIndex = 52 then begin
      Result[c] := n.Items[i].Text;
      inc(c);
    end;
end;

end.

