unit u_presetmanager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, Buttons,
  StdCtrls, LCLTranslator;

type

  TPresetToWidget = procedure(const A: TStringArray) of Object;
  TWidgetToPreset = function: string of Object;

  { TPresetManager }

  TPresetManager = class(TForm)
    LB: TListBox;
    MenuItem1: TMenuItem;
    MIManager: TMenuItem;
    MIAdd: TMenuItem;
    PopupMenu1: TPopupMenu;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LBSelectionChange(Sender: TObject; {%H-}User: boolean);
    procedure MIAddClick(Sender: TObject);
    procedure MIManagerClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
  private
    FList: TStringList;
   // FUserButton: TSpeedButton;
    FPresetToWidget: TPresetToWidget;
    FWidgetToPreset: TWidgetToPreset;
    FFilename: string;
    procedure ProcessUserButtonClick(Sender: TObject);
    procedure ProcessPresetClic(Sender: TObject);
    procedure Clear;
    function MergeArray(const A: TStringArray): string;
    procedure UpdateWidgets;
  public
    procedure Init1(const aTitle: string; aUserButton: TSpeedButton;
                    const aFilename: string);
    procedure Init2(aPresetToWidget: TPresetToWidget;
                    aWidgetToPreset: TWidgetToPreset);
    procedure Save;
    procedure Load;
    procedure UpdateStringAfterLanguageChange;
  end;


implementation

uses u_userdialogs, u_common, u_resource_string;

{$R *.lfm}

{ TPresetManager }

procedure TPresetManager.ProcessPresetClic(Sender: TObject);
var m: TMenuItem;
  A: TStringArray;
  i: integer;
begin
 m:=Sender as TMenuItem;
 i:=PopupMenu1.items.IndexOf( m );
 A:=FList.Strings[i-3].Split([PRESET_SEPARATOR]);
 Delete(A, 0, 1); // delete the name of the preset
 FPresetToWidget(A);
end;

procedure TPresetManager.Clear;
begin
 LB.Clear;
 while PopupMenu1.Items.Count>3 do
   PopupMenu1.Items.Delete(3);
 FList.Clear;
end;

function TPresetManager.MergeArray(const A: TStringArray): string;
var i: integer;
begin
  Result:='';
  for i:=0 to High(A) do begin
    if i<>0 then Result:=Result+PRESET_SEPARATOR;
    Result:=Result+A[i];
  end;
end;

procedure TPresetManager.UpdateWidgets;
begin
  SpeedButton1.Enabled:=LB.ItemIndex>-1;
  SpeedButton2.Enabled:=SpeedButton1.Enabled;
  SpeedButton3.Enabled:=SpeedButton1.Enabled;
end;

procedure TPresetManager.MIManagerClick(Sender: TObject);
begin
  ShowModal;
end;

procedure TPresetManager.SpeedButton1Click(Sender: TObject);
var i: integer;
  A: TStringArray;
begin
  i:=LB.ItemIndex;
  if i=-1 then exit;
  if AskConfirmation(SReplacePresetData, SYes, SNo, mtConfirmation)=mrOk then begin
    A:=FList.Strings[i].Split([PRESET_SEPARATOR]);
    FList.Strings[i]:=A[0]+PRESET_SEPARATOR+FWidgetToPreset();
    Save;
  end;
end;

procedure TPresetManager.SpeedButton2Click(Sender: TObject);
var i: integer;
begin
  i:=LB.ItemIndex;
  if i=-1 then exit;
  if AskConfirmation(SDeleteThisPreset, SYes, SNo, mtConfirmation)=mrOk then begin
    FList.Delete(i);
    PopupMenu1.Items.Delete(i+3);
    LB.Items.Delete(i);
    Save;
  end;
end;

procedure TPresetManager.SpeedButton3Click(Sender: TObject);
var i: Integer;
  n: string;
  men: TMenuItem;
  A: TStringArray;
begin
  i:=LB.ItemIndex;
  if i=-1 then exit;
  n:=LB.Items.Strings[i];
  if UserInputNoSpecialChar(SNewName, SOk, SCancel, n, mtCustom, FALSE)=mrOk then begin
    men:=PopupMenu1.Items[i+3];
    men.Caption:=n;
    LB.Items.Strings[i]:=n;
    A:=FList.Strings[i].Split([PRESET_SEPARATOR]);
    A[0]:=n;
    FList.Strings[i]:=MergeArray(A);
    Save;
  end;
end;

procedure TPresetManager.SpeedButton4Click(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TPresetManager.MIAddClick(Sender: TObject);
var n: string;
  men: TMenuItem;
begin
 n := '';
  if UserInputNoSpecialChar('Name for the new preset:', SOk, SCancel, n, mtCustom, FALSE)<>mrOk then exit;
  FList.Add(n + PRESET_SEPARATOR + FWidgetToPreset());
  LB.Items.Add( n );
  men:=TMenuItem.Create(Self);
  men.Caption:=n;
  men.OnClick:=@ProcessPresetClic;
  PopupMenu1.Items.Add( men );
  Save;
end;

procedure TPresetManager.FormCreate(Sender: TObject);
begin
  FList:=TStringList.Create;
end;

procedure TPresetManager.FormDestroy(Sender: TObject);
begin
  FList.Free;
end;

procedure TPresetManager.FormShow(Sender: TObject);
begin
  UpdateStringAfterLanguageChange;
  UpdateWidgets;
end;

procedure TPresetManager.LBSelectionChange(Sender: TObject; User: boolean);
begin
  UpdateWidgets;
end;

procedure TPresetManager.ProcessUserButtonClick(Sender: TObject);
begin
  PopupMenu1.PopUp;
end;

procedure TPresetManager.Init1(const aTitle: string; aUserButton: TSpeedButton;
  const aFilename: string);
begin
  Caption:=aTitle;
  aUserButton.OnClick:=@ProcessUserButtonClick;
  FFilename:=aFilename;
  Load;
end;

procedure TPresetManager.Init2(aPresetToWidget: TPresetToWidget; aWidgetToPreset: TWidgetToPreset);
begin
  FPresetToWidget:=aPresetToWidget;
  FWidgetToPreset:=aWidgetToPreset;
end;

procedure TPresetManager.Save;
begin
  if FList.Count=0 then exit;
  try
    FList.SaveToFile(FFilename);
  finally
  end;
end;

procedure TPresetManager.Load;
var i: integer;
  m: TMenuItem;
  A: TStringArray;
begin
 Clear;
 if not FileExists(FFilename) then exit;

 FList.LoadFromFile(FFilename);
 // sets the presets on the listbox and in the menu
 for i:=0 to FList.Count-1 do begin
   A:=FList.Strings[i].Split([PRESET_SEPARATOR]);
   LB.Items.Add(A[0]);
   m:=TMenuItem.Create(Self);
   m.Caption:=A[0];
   m.OnClick:=@ProcessPresetClic;
   PopupMenu1.Items.Add(m);
 end;
end;

procedure TPresetManager.UpdateStringAfterLanguageChange;
begin
  SpeedButton1.Caption := SUpdate;
  SpeedButton2.Caption := SDelete;
  SpeedButton3.Caption := SRename;
  SpeedButton4.Caption := SClose;
  MIAdd.Caption := SAdd;
end;

end.

