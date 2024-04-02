unit frame_editstring;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Graphics;

type

  { TFrameEditString }

  TFrameEditString = class(TFrame)
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    Panel1: TPanel;
    procedure Edit1Change(Sender: TObject);
    procedure Edit1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FOnTextChange: TNotifyEvent;
    function GetFontHeight: integer;
    function GetText: string;
    function GetTitle: string;
    procedure SetFontHeight(AValue: integer);
    procedure SetText(AValue: string);
    procedure SetTitle(AValue: string);
  private
    FForbiddenChars: string;
    FMode: integer; // 0= all char     1= no special    2=filename
    FAllowEmptyString: boolean;
    FOnEnterPressed: TNotifyEvent;
    function GetFontStyle: TFontStyles;
    function GetHaveFocus: boolean;
    function GetReadOnly: boolean;
    function GetTextIsValid: boolean;
    procedure SetFontStyle(AValue: TFontStyles);
    procedure SetReadOnly(AValue: boolean);
  public
    procedure SetTheFocus;
    procedure ClearSelection;

    procedure AllowEmptyString;
    procedure ModeAllChar;
    procedure ModeNumberOnly;
    procedure ModeNoSpecialChar;
    procedure ModeFileName;

    property Title: string read GetTitle write SetTitle;
    property Text: string read GetText write SetText;

    property FontHeight: integer read GetFontHeight write SetFontHeight;
    property FontStyles: TFontStyles read GetFontStyle write SetFontStyle;

    property OnTextChange: TNotifyEvent read FOnTextChange write FOnTextChange;
    property TextIsValid: boolean read GetTextIsValid;
    property HaveFocus: boolean read GetHaveFocus;
    property OnEnterPressed: TNotifyEvent read FOnEnterPressed write FOnEnterPressed;
    property ReadOnly: boolean read GetReadOnly write SetReadOnly;
  end;

implementation

uses u_resource_string, u_utils, u_common, LCLType;

{$R *.lfm}

{ TFrameEditString }

procedure TFrameEditString.Edit1Change(Sender: TObject);
begin
  Label2.Visible := not TextIsValid;
  if FOnTextChange <> NIL then
    FOnTextChange(Self);
end;

procedure TFrameEditString.Edit1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    if FOnEnterPressed <> NIL then
      FOnEnterPressed(Self);
end;

function TFrameEditString.GetFontHeight: integer;
begin
  Result := Label1.Font.Height;
end;

function TFrameEditString.GetText: string;
begin
  Result := Edit1.Text;
end;

function TFrameEditString.GetTitle: string;
begin
  Result := Label1.Caption;
end;

procedure TFrameEditString.SetFontHeight(AValue: integer);
begin
  Label1.Font.Height := AValue;
  Edit1.Font.Height := AValue;
  Height := AValue*2+Label2.Font.Height+2;
end;

procedure TFrameEditString.SetText(AValue: string);
begin
  Edit1.Text := AValue;
  Edit1Change(NIL);
end;

procedure TFrameEditString.SetTitle(AValue: string);
begin
  Label1.Caption := AValue;
end;

function TFrameEditString.GetTextIsValid: boolean;
var v: integer;
begin
  if Edit1.NumbersOnly then
  begin
    Result := TryStrToInt(Edit1.Text, v);
    exit;
  end;

  //FMode: integer; // 0= all char     1= no special    2=filename
  case FMode of
    0: Result := (Edit1.Text<>'') or FAllowEmptyString;
    1:
      begin
        if FAllowEmptyString and (Edit1.Text='') then
          Result := TRUE
        else
          Result := StringIsValid(Edit1.Text);
    end;
    2: Result := FileNameIsValid(Edit1.Text);
  end;//case
end;

procedure TFrameEditString.SetFontStyle(AValue: TFontStyles);
begin
  Edit1.Font.Style := AValue;
end;

procedure TFrameEditString.SetReadOnly(AValue: boolean);
begin
  Edit1.ReadOnly := AValue;
end;

function TFrameEditString.GetHaveFocus: boolean;
begin
  Result := Edit1.Focused;
end;

function TFrameEditString.GetFontStyle: TFontStyles;
begin
  Result := Edit1.Font.Style;
end;

function TFrameEditString.GetReadOnly: boolean;
begin
  Result := Edit1.ReadOnly;
end;

procedure TFrameEditString.SetTheFocus;
begin
  Edit1.SelectAll;
  Edit1.SetFocus;
end;

procedure TFrameEditString.ClearSelection;
begin
  Edit1.ClearSelection;
end;

procedure TFrameEditString.AllowEmptyString;
begin
  FAllowEmptyString := TRUE;
end;

procedure TFrameEditString.ModeAllChar;
begin
  Label2.Caption := '';
  FForbiddenChars := '';
  FMode := 0;
end;

procedure TFrameEditString.ModeNumberOnly;
begin
  Edit1.NumbersOnly := TRUE;
end;

procedure TFrameEditString.ModeNoSpecialChar;
begin
  FForbiddenChars := FORBIDENCHARS;
  Label2.Caption := SPleaseNoSpecialCharacters+' '+FORBIDENCHARS;
  FMode := 1;
end;

procedure TFrameEditString.ModeFileName;
begin
  FForbiddenChars := FILENAMEFORBIDENCHARS;
  Label2.Caption := SPleaseNoSpaceOrSpecialCharacters+FORBIDENCHARS;
  FMode := 2;
end;

end.

