unit form_help;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons;

type

  { TFormHelp }

  TFormHelp = class(TForm)
    Memo1: TMemo;
    Shape1: TShape;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDeactivate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    procedure AdjustFont;
  public

  end;

procedure _ShowHelp(const aMultiLineTextHelp: string; aHelpButton: TSpeedButton;
                    ShowBigWindow: boolean=False);


implementation
uses Math, LCLType, u_resource_string;

procedure _ShowHelp(const aMultiLineTextHelp: string; aHelpButton: TSpeedButton;
                    ShowBigWindow: boolean);
var p: TPoint;
  w, h: integer;
begin
  p := aHelpButton.ClientRect.CenterPoint;
  p := aHelpButton.ClientToScreen(p);

  with TFormHelp.Create(NIL) do begin
    if ShowBigWindow then begin
      w := ScaleDesignToForm(800);
      h := ScaleDesignToForm(400);
    end else begin
      w := ScaleDesignToForm(450);
      h := ScaleDesignToForm(200);
    end;

    SetBounds(left, top, w, h);
    Memo1.Clear;
    Memo1.Lines.AddText(aMultiLineTextHelp);
    Show;
  end;
end;

{$R *.lfm}

{ TFormHelp }

procedure TFormHelp.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TFormHelp.FormDeactivate(Sender: TObject);
begin
  Close;
end;

procedure TFormHelp.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then Close;
end;

procedure TFormHelp.FormShow(Sender: TObject);
begin
  AdjustFont;
  Caption := SHelp;
end;

procedure TFormHelp.AdjustFont;
begin
  exit;
{$if defined(LCLGTK2) or defined(LCLCOCOA)}
  Memo1.Font.Name := 'Arial';
  Memo1.Font.Style := [fsBold];
  Memo1.Font.Height := ScaleDesignToForm(15-2);
{$endif}
{$if defined(Windows)}
  Memo1.Font.Name := 'Arial';
  Memo1.Font.Style := [];
  Memo1.Font.Height := ScaleDesignToForm(15);
{$endif}
end;

end.

