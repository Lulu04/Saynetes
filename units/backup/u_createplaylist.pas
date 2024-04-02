unit u_createplaylist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LCLType, StdCtrls,
  ComCtrls, Buttons, ExtCtrls, LCLTranslator,
  ALSound, u_audio_manager, frame_editstring;

type

  { TFormCreatePlaylist }

  TFormCreatePlaylist = class(TForm)
    LB: TListBox;
    OD1: TOpenDialog;
    Panel1: TPanel;
    Shape1: TShape;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    BSave: TSpeedButton;
    BCancel: TSpeedButton;
    UpDown1: TUpDown;
    procedure BCancelClick(Sender: TObject);
    procedure BSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure LBKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure UpDown1Click(Sender: TObject; Button: TUDBtnType);
  private
    FMusic: TALSSound;
    FIndexPlayed: integer;
    procedure PlayMusic(Index: integer);
    procedure StopMusic;
  private
    FrameEditString1: TFrameEditString;
    FModeModify: boolean;
    FPreviousFilename: string;
    function GetModifiedName: string;
  public
    procedure SetModeNew;
    procedure SetModeModify(const aFilename: string);
    property ModifiedName: string read GetModifiedName;
  end;

var
  FormCreatePlaylist: TFormCreatePlaylist;

implementation

uses LCLHelper, LazFileUtils, u_project_manager, u_common,
  u_userdialogs, u_resource_string;

{$R *.lfm}

{ TFormCreatePlaylist }

procedure TFormCreatePlaylist.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then ModalResult := mrCancel;
end;

procedure TFormCreatePlaylist.FormShow(Sender: TObject);
begin
  SpeedButton1.Caption := SAdd;
  SpeedButton2.Caption := SDelete;
  BCancel.Caption := SCancel;
  BSave.Caption := SSave;
end;

procedure TFormCreatePlaylist.BCancelClick(Sender: TObject);
begin
  StopMusic;
  ModalResult := mrCancel;
end;

procedure TFormCreatePlaylist.BSaveClick(Sender: TObject);
var f: string;
  flagError: boolean;
begin
  if not FrameEditString1.TextIsValid then exit;
  if LB.Count = 0 then exit;
  StopMusic;

  if FModeModify and FileExists(FPreviousFilename) then DeleteFile(FPreviousFilename);


  flagError := FALSE;
  if DirectoryExistsUTF8(Project.PlaylistsFolder) then begin
    try
      f := ConcatPaths([Project.PlaylistsFolder, FrameEditString1.Text+PLAYLIST_FILE_EXTENSION]);
      LB.Items.SaveToFile(f);
      ShowMess(SPlaylistSavedSuccess, SOk, mtCustom);
      ModalResult := mrOk;
    except
      flagError := TRUE;
    end;
  end else flagError := TRUE;

  if flagError
    then ShowMess(SCannotSavePlaylist, SOk, mtError);
end;

procedure TFormCreatePlaylist.FormCreate(Sender: TObject);
begin
  FrameEditString1 := TFrameEditString.Create(Self);
  FrameEditString1.Parent := Panel1;
  FrameEditString1.Align := alClient;
  FrameEditString1.Title := SNameForThePlaylist;
  FrameEditString1.ModeFileName;
end;

procedure TFormCreatePlaylist.LBKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (LB.SelCount > 0) and (Key = VK_DELETE)
    then LB.DeleteSelected;
  if (LB.SelCount = 1) and (Key = VK_SPACE) then begin
    if (FMusic = NIL) or (FIndexPlayed <> LB.ItemIndex) then begin
      PlayMusic(LB.ItemIndex);
      FIndexPlayed := LB.ItemIndex;
    end else StopMusic;
  end;
end;

procedure TFormCreatePlaylist.SpeedButton1Click(Sender: TObject);
var i: integer;
begin
  if not OD1.Execute then exit;
  for i:=0 to OD1.Files.count-1 do
    LB.Items.Add(OD1.Files.Strings[i]);
end;

procedure TFormCreatePlaylist.SpeedButton2Click(Sender: TObject);
begin
  if LB.SelCount = 0 then exit;
  LB.DeleteSelected;
end;

procedure TFormCreatePlaylist.UpDown1Click(Sender: TObject; Button: TUDBtnType);
begin
  if LB.SelCount = 0 then exit;
  if Button = btPrev
    then LB.MoveSelectionDown
    else LB.MoveSelectionUp;
end;

procedure TFormCreatePlaylist.PlayMusic(Index: integer);
begin
  StopMusic;
  FMusic := SoundManager.AddStream(LB.Items.Strings[Index]);
  if FMusic <> NIL
    then FMusic.Play(TRUE);
end;

procedure TFormCreatePlaylist.StopMusic;
begin
  if FMusic <> NIL then begin
    FMusic.Kill;
    FMusic := NIL;
    FIndexPlayed := -1;
  end;
end;

function TFormCreatePlaylist.GetModifiedName: string;
begin
  Result := FrameEditString1.Text;
end;

procedure TFormCreatePlaylist.SetModeNew;
begin
  LB.Clear;
  FrameEditString1.Text := '';
  FModeModify := False;
end;

procedure TFormCreatePlaylist.SetModeModify(const aFilename: string);
begin
  LB.Items.LoadFromFile(aFilename);
  FrameEditString1.Text := ChangeFileExt(ExtractFilename(aFilename), '');
  FModeModify := True;
  FPreviousFilename := aFilename;
end;

end.

