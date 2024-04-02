unit u_notebook_util;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, Buttons, ExtCtrls,  StdCtrls, Graphics;


type

  { TNoteBookManager
    link a TSpeedButton with a page of a NoteBook. When user click on the
    button, the corresponding page is shown and its background and font colors
    change.
  }

  TNoteBookManager=class
  private
    FOnSelectionChange: TNotifyEvent;
    FTargetNoteBook: TNoteBook;
    FButtons: array of TSpeedButton;
    FPages: array of TPage;
    FActivatedButtonColor,
    FActivatedFontColor,
    FDeactivatedButtonColor,
    FDeactivatedFontColor: TColor;
    procedure ProcessButtonClick(Sender: TObject);
    procedure SetButtonState(Index: integer; aActivate: boolean);
  public
    constructor Create(aTargetNoteBook: TNoteBook);
    procedure LinkButtonToPage(aButton: TSpeedButton; aPage: TPage);
    procedure ActivePage(aPage: TPage);

    procedure SetActivatedColors(aButtonColor, aFontColor: TColor);
    procedure SetDeactivatedColors(aButtonColor, aFontColor: TColor);

    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
  end;



  { TToggleSpeedButtonManager }
  TToggleSpeedButtonType=(tsbLikeRadioButton, // only 1 checked
                          tsbLikeCheckBox,    // none or multiple checked
                          tsbNoneOrOne,       // none or only one checked
                          tsbAtLeastOne);     // at least one checked
  TToggleSpeedButtonManager=class
  private
    FType: TToggleSpeedButtonType;
    FButtons: array of TSpeedButton;
    FOnClicks: array of TNotifyEvent;
    FChecked: array of boolean;
    FActivatedButtonColor,
    FActivatedFontColor,
    FDeactivatedButtonColor,
    FDeactivatedFontColor: TColor;
    FActivatedImageIndex,
    FDeactivatedImageIndex: integer;
    FApplyImageIndexes: boolean;
    function GetChecked(aButton: TSpeedButton): boolean;
    function IndexOf(aButton: TSpeedButton): integer;
    procedure ProcessButtonClick(Sender: TObject);
    procedure Activate(index: integer);
    procedure Deactivate(index: integer);
    procedure SetChecked(aButton: TSpeedButton; AValue: boolean);
  public
    constructor Create;
    procedure Add(aButton: TSpeedButton; aActivate: boolean);

    procedure SetState(aButton: TSpeedButton; aState: boolean);
    procedure ToogleState(aButton: TSpeedButton);

    procedure SetActivatedColors(aButtonColor, aFontColor: TColor);
    procedure SetDeactivatedColors(aButtonColor, aFontColor: TColor);
    // Sets the index of image's index to apply when a button is toogle
    // The button's image index are not changed if you don't call this method.
    procedure SetImageIndexes( aActivatedImageIndex, aDeactivatedImageIndex: integer);

    property Checked[aButton: TSpeedButton]: boolean read GetChecked write SetChecked;
    property ToggleType: TToggleSpeedButtonType read FType write FType;
  end;


  { TLabelShapeManager }

  TLabelShapeManager=class
  private
    FLabels: array of TLabel;
    FShapes: array of TShape;
    FOnClicks: array of TNotifyEvent;
    function IndexOf(aLabel: TLabel): integer;
    procedure ProcessLabelClick(Sender: TObject);
    procedure Activate(index: integer);
    procedure Deactivate(index: integer);
  public
    procedure LinkLabelToShape(aLabel: TLabel; aShape: TShape; aChecked: boolean=FALSE);
  end;



  { TCheckedLabelManager
    Capture a click on a TLabel. When the TLabel is clicked, the state of
    TCheckBox or TRadioButton anchored to its left is toggled.
    Use it to apply some color on a caption of a TCheckBox or TRadioButton.
  }

  TCheckedLabelManager=class
  private
    procedure ProcessLabelClick(Sender: TObject);
  public
    // The label must be anchored FROM THE LEFT on a TCheckBox or TRadioButton.
    // The Hint of Radio button or checkbox is copied to the label
    procedure CaptureLabelClick(aLabel: TLabel);
  end;
var
  CheckedLabelManager: TCheckedLabelManager;

implementation

uses Controls;

{ TCheckedLabelManager }

procedure TCheckedLabelManager.ProcessLabelClick
  (Sender: TObject);
var L: TLabel;
begin
  if not(Sender is TLabel) then exit;

  L := Sender as TLabel;
  if L.AnchorSideLeft.Control is TCheckBox then
    TCheckBox(L.AnchorSideLeft.Control).Checked := not TCheckBox(L.AnchorSideLeft.Control).Checked
  else
  if L.AnchorSideLeft.Control is TRadioButton then
    TRadioButton(L.AnchorSideLeft.Control).Checked := True;
end;

procedure TCheckedLabelManager.CaptureLabelClick(aLabel: TLabel);
begin
  aLabel.OnClick := @ProcessLabelClick;

  // Copy hint
  if aLabel.AnchorSideLeft.Control <> NIL then
  begin
    aLabel.ShowHint := aLabel.AnchorSideLeft.Control.ShowHint;
    aLabel.Hint := aLabel.AnchorSideLeft.Control.Hint;
  end;
end;

{ TToggleSpeedButtonManager }

function TToggleSpeedButtonManager.IndexOf(aButton: TSpeedButton): integer;
var i: integer;
begin
  for i:=0 to High(FButtons) do
   if FButtons[i]=aButton then begin
     Result:=i;
     exit;
   end;
  Result:=-1;
end;

function TToggleSpeedButtonManager.GetChecked(aButton: TSpeedButton): boolean;
var
  i: Integer;
begin
  i := IndexOf( aButton );
  if i = -1 then
    Result := False
  else
    Result :=FChecked[i];
end;

procedure TToggleSpeedButtonManager.ProcessButtonClick(Sender: TObject);
var i, j: integer;
  anotherIsChecked: boolean;
begin
  i:=IndexOf(Sender as TSpeedButton);
  if i=-1 then exit;

  case FType of
    tsbLikeRadioButton:
      begin
        Activate(i);
        for j:=0 to High(FButtons) do
         if j<>i then Deactivate(j);
      end;
    tsbLikeCheckBox:
      begin
        case FChecked[i] of
          False: Activate(i);
          True: Deactivate(i);
        end;
      end;
    tsbNoneOrOne:
      begin
        case FChecked[i] of
          False: Activate(i);
          True: Deactivate(i);
        end;
        if FChecked[i] then
          for j:=0 to High(FButtons) do
           if j <> i then
             Deactivate(j);
      end;
    tsbAtLeastOne:
      begin
        anotherIsChecked := False;
        for j:=0 to High(FChecked) do
          if j <> i then
            anotherIsChecked := anotherIsChecked or FChecked[j];

        case FChecked[i] of
          False: Activate(i);
          True: if anotherIsChecked then Deactivate(i);
        end;
      end;
  end;

  if FOnClicks[i]<>NIL
    then FOnClicks[i](FButtons[i]);
end;

procedure TToggleSpeedButtonManager.Activate(index: integer);
begin
  FButtons[index].Color := FActivatedButtonColor;
  FButtons[index].Font.Color := FActivatedFontColor;

  if FApplyImageIndexes then
    FButtons[index].ImageIndex := FActivatedImageIndex;

  FChecked[index] := True;
end;

procedure TToggleSpeedButtonManager.Deactivate(index: integer);
begin
  FButtons[index].Color := FDeactivatedButtonColor;
  FButtons[index].Font.Color := FDeactivatedFontColor;

  if FApplyImageIndexes then
    FButtons[index].ImageIndex := FDeactivatedImageIndex;

  FChecked[index] := False;
end;

procedure TToggleSpeedButtonManager.SetChecked(aButton: TSpeedButton; AValue: boolean);
var i: integer;
begin
  i := IndexOf(aButton);
  if i = -1 then exit;
  if AValue then
    Activate(i)
  else
    Deactivate(i);
end;

constructor TToggleSpeedButtonManager.Create;
begin
  FType := tsbLikeCheckBox;

  FActivatedButtonColor := RGBToColor(224,224,85);
  FActivatedFontColor := clBlack;

  FDeactivatedButtonColor := $00525252;
  FDeactivatedFontColor:= $00ACB6C0;

  FActivatedImageIndex := -1;
  FDeactivatedImageIndex := -1;
end;

procedure TToggleSpeedButtonManager.Add(aButton: TSpeedButton; aActivate: boolean);
var k: integer;
begin
  k:=Length(FButtons);
  SetLength(FButtons, k+1);
  SetLength(FOnClicks, k+1);
  SetLength(FChecked, k+1);

  FButtons[k]:=aButton;
  FOnClicks[k]:=aButton.OnClick;
  FButtons[k].OnClick:=@ProcessButtonClick;
  FButtons[k].Transparent:=FALSE;
  FButtons[k].Flat:=TRUE;

  if aActivate
    then Activate(k)
    else Deactivate(k);
end;

procedure TToggleSpeedButtonManager.SetState(aButton: TSpeedButton; aState: boolean);
var i: integer;
begin
  i := IndexOf(aButton);
  if i = -1 then exit;
  if aState then
    Activate(i)
  else
    Deactivate(i);
end;

procedure TToggleSpeedButtonManager.ToogleState(aButton: TSpeedButton);
var i: integer;
begin
  i := IndexOf(aButton);
  if i <> -1 then
    ProcessButtonClick(FButtons[i]);
 { if FChecked[i] then
    Deactivate(i)
  else
    Activate(i);   }
end;

procedure TToggleSpeedButtonManager.SetActivatedColors(aButtonColor, aFontColor: TColor);
begin
  FActivatedButtonColor := aButtonColor;
  FActivatedFontColor := aFontColor;
end;

procedure TToggleSpeedButtonManager.SetDeactivatedColors(aButtonColor, aFontColor: TColor);
begin
  FDeactivatedButtonColor := aButtonColor;
  FDeactivatedFontColor := aFontColor;
end;

procedure TToggleSpeedButtonManager.SetImageIndexes(aActivatedImageIndex,
  aDeactivatedImageIndex: integer);
begin
  FApplyImageIndexes := True;
  FActivatedImageIndex := aActivatedImageIndex;
  FDeactivatedImageIndex := aDeactivatedImageIndex;
end;

{ TLabelShapeManager }

function TLabelShapeManager.IndexOf(aLabel: TLabel): integer;
var i: integer;
begin
  for i:=0 to High(FLabels) do
   if FLabels[i]=aLabel then begin
     Result:=i;
     exit;
   end;
  Result:=-1;
end;

procedure TLabelShapeManager.ProcessLabelClick(Sender: TObject);
var i: integer;
begin
  i:=IndexOf(Sender as TLabel);
  if i=-1 then exit;
  case FLabels[i].Tag of
    0: Activate(i);
    1: Deactivate(i);
  end;
  if FOnClicks[i]<>NIL
    then FOnClicks[i](FLabels[i]);
end;

procedure TLabelShapeManager.Activate(index: integer);
begin
  FLabels[index].Tag:=1;
  FLabels[index].Font.Color:=clBlack;
  FShapes[index].Brush.Color:=$00C9D1ED;
  FShapes[index].Pen.Color:=clBlack;
end;

procedure TLabelShapeManager.Deactivate(index: integer);
begin
  FLabels[index].Tag:=0;
  FLabels[index].Font.Color:=$00ACB6C0;
  FShapes[index].Brush.Color:=$00525252;
  FShapes[index].Pen.Color:=$00757575;
end;

procedure TLabelShapeManager.LinkLabelToShape(aLabel: TLabel; aShape: TShape; aChecked: boolean);
var k: integer;
begin
  k:=Length(FLabels);
  SetLength(FLabels, k+1);
  SetLength(FShapes, k+1);
  SetLength(FOnClicks, k+1);
  FLabels[k]:=aLabel;
  FShapes[k]:=aShape;
  FOnClicks[k]:=aLabel.OnClick;
  aLabel.Tag:=0;
  aLabel.OnClick:=@ProcessLabelClick;
  aLabel.Cursor:=crHandPoint;
  if aChecked
    then Activate(k)
    else Deactivate(k);
end;

{ TNoteBookManager }

procedure TNoteBookManager.ProcessButtonClick(Sender: TObject);
var i, j: integer;
  sb: TSpeedButton;
begin
  sb:=Sender as TSpeedButton;
  i:=sb.Tag;
  FTargetNoteBook.PageIndex:=FTargetNoteBook.IndexOf(FPages[i]);
  for j:=0 to High(FButtons) do
   SetButtonState(j, FButtons[j].Tag=sb.Tag);

  if FOnSelectionChange<>NIL
    then FOnSelectionChange(FTargetNoteBook);
end;

procedure TNoteBookManager.SetButtonState(Index: integer; aActivate: boolean);
begin
  with FButtons[Index] do
    case aActivate of
      FALSE: begin
        Color:=FDeactivatedButtonColor;
        Font.Color := FDeactivatedFontColor;
      end;
      TRUE: begin
        Color := FActivatedButtonColor;
        Font.Color := FActivatedFontColor;
      end;
    end;
end;

constructor TNoteBookManager.Create(aTargetNoteBook: TNoteBook);
begin
  FTargetNoteBook := aTargetNoteBook;
  SetLength(FButtons, 0);
  SetLength(FPages, 0);

  FActivatedButtonColor := clWhite;
  FActivatedFontColor := clBlack;
  FDeactivatedButtonColor := $00484848;
  FDeactivatedFontColor := $00EAEAEA;

end;

procedure TNoteBookManager.LinkButtonToPage(aButton: TSpeedButton; aPage: TPage);
var k: integer;
begin
  k:=Length(FButtons);
  SetLength(FButtons, k+1);
  FButtons[k]:=aButton;
  SetLength(FPages, k+1);
  FPages[k]:=aPage;

  with FButtons[k] do begin
    Tag:=k;
    OnClick:=@ProcessButtonClick;
    SetButtonState(k, FALSE);
    Cursor:=crHandPoint;
Transparent:=FALSE;
Flat:=TRUE;
  end;
end;

procedure TNoteBookManager.ActivePage(aPage: TPage);
var i: integer;
begin
  for i:=0 to High(FPages) do
   if FPages[i]=aPage then begin
     ProcessButtonClick(FButtons[i]);
     exit;
   end;
end;

procedure TNoteBookManager.SetActivatedColors(aButtonColor, aFontColor: TColor);
begin
  FActivatedButtonColor := aButtonColor;
  FActivatedFontColor := aFontColor;
end;

procedure TNoteBookManager.SetDeactivatedColors(aButtonColor, aFontColor: TColor);
begin
  FDeactivatedButtonColor := aButtonColor;
  FDeactivatedFontColor := aFontColor;
end;

Initialization
 CheckedLabelManager := TCheckedLabelManager.Create;
Finalization
 CheckedLabelManager.Free;
end.

