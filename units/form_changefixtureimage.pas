unit form_changefixtureimage;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  u_common, frame_viewfixtureimage;

type

  { TFormChangeFixtureImage }

  TFormChangeFixtureImage = class(TForm)
    ScrollBox1: TScrollBox;
    procedure FormKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    FFixtureType: TFixtureType;
    FFrameFixtureImages: array[TFixtureType] of TFrameFixtureImage;
    procedure UpdateFixtureImages;
    procedure ProcessFrameFixtureImageChange(Sender: TFrameFixtureImage);
  public
    property SelectedFixtureType: TFixtureType read FFixtureType;
  end;


implementation
uses LCLType;

{$R *.lfm}

{ TFormChangeFixtureImage }

procedure TFormChangeFixtureImage.FormShow(Sender: TObject);
begin
  UpdateFixtureImages;
end;

procedure TFormChangeFixtureImage.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then ModalResult := mrCancel;
end;

procedure TFormChangeFixtureImage.UpdateFixtureImages;
var i: TFixtureType;
  xx, yy, c: integer;
begin
  xx := 0;
  yy := 0;
  c := 0;

  for i in FixtureDisplayOrder do begin
    FFrameFixtureImages[i] := TFrameFixtureImage.Create(Self);
    FFrameFixtureImages[i].Name := 'FrameFixtureImage'+Ord(i).ToString;
    FFrameFixtureImages[i].Parent := ScrollBox1;
    FFrameFixtureImages[i].Left := xx;
    FFrameFixtureImages[i].Top := yy;
    FFrameFixtureImages[i].InitWith(i);
    FFrameFixtureImages[i].OnChange := @ProcessFrameFixtureImageChange;
    xx := xx + FFrameFixtureImages[i].Width + (ScrollBox1.ClientWidth - FFrameFixtureImages[i].Width*3) div 3; // ScaleDesignToForm(20);
    inc(c);
    if c = 3 then begin
      c := 0;
      xx := 0;
      yy := yy + FFrameFixtureImages[i].Height + ScaleDesignToForm(10);
    end;
  end;
end;

procedure TFormChangeFixtureImage.ProcessFrameFixtureImageChange(Sender: TFrameFixtureImage);
begin
  if Sender.Checked then begin
    FFixtureType := Sender.FixtureType;
    ModalResult := mrOk;
  end;
end;

end.

