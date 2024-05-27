unit form_rangesgenerator;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, Spin;

type

  { TFormRangesGenerator }

  TFormRangesGenerator = class(TForm)
    BOK: TSpeedButton;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    LB: TListBox;
    Panel1: TPanel;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    SpinEdit4: TSpinEdit;
    procedure BOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SpinEdit1Change(Sender: TObject);
  private
    FGenerating: Boolean;
    function GetBeginValue: integer;
    function GetCount: integer;
    function GetRangeWidth: integer;
  public
    function GetFunctionality(aIndex: integer): string;

    property BeginValue: integer read GetBeginValue;
    property RangeWidth: integer read GetRangeWidth;
    property Count: integer read GetCount;
  end;

var
  FormRangesGenerator: TFormRangesGenerator;

implementation

uses u_resource_string, LCLType;

{$R *.lfm}

{ TFormRangesGenerator }

procedure TFormRangesGenerator.FormCreate(Sender: TObject);
begin
  // Manual translation
  Label1.Caption := sRangesGenerator;
  Label4.Caption := SCount;
  Label5.Caption := SPreview;
  Label6.Caption := SName;
  BOk.Caption := SOk;
  Edit1.Text := SFunctionality + ' #';
end;

procedure TFormRangesGenerator.BOKClick(Sender: TObject);
begin
  if Sender = BOk then begin
    ModalResult := mrOk;
  end;
end;

procedure TFormRangesGenerator.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then ModalResult := mrCancel;
end;

procedure TFormRangesGenerator.SpinEdit1Change(Sender: TObject);
var i, current: Integer;
begin
  if FGenerating then exit;
  FGenerating := True;

  if SpinEdit1.Value + SpinEdit2.Value * SpinEdit3.Value-1 > 255 then begin
    SpinEdit3.Value := (255 - SpinEdit1.Value) div SpinEdit2.Value;
  end;

  LB.Clear;
  BOk.Enabled := SpinEdit3.Value > 0;
  if SpinEdit3.Value = 0 then exit;

  current := SpinEdit1.Value;
  for i:=1 to SpinEdit3.Value do begin
    LB.Items.Add(current.ToString+'..'+(current+SpinEdit2.Value-1).ToString+' : '+ GetFunctionality(i));
    current := current + SpinEdit2.Value;
  end;

  FGenerating := False;
end;

function TFormRangesGenerator.GetCount: integer;
begin
  Result := SpinEdit3.Value;
end;

function TFormRangesGenerator.GetRangeWidth: integer;
begin
  Result := SpinEdit2.Value;
end;

function TFormRangesGenerator.GetBeginValue: integer;
begin
  Result := SpinEdit1.Value;
end;

function TFormRangesGenerator.GetFunctionality(aIndex: integer): string;
begin
  Result := Trim(Edit1.Text);
  if Result = '' then exit;
  Result := Result.Replace('#', (aIndex+SpinEdit4.Value).ToString, [rfReplaceAll]);
end;

end.

