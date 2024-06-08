unit frame_viewtoplist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Types, LCLType,
  LCLTranslator, ExtCtrls, Menus,
  BGRABitmap, BGRABitmapTypes,
  u_list_top;

type

  { TFrameViewTopList }

  TFrameViewTopList = class(TFrame)
    LB: TListBox;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MILoadFromAnotherProject: TMenuItem;
    MIStop: TMenuItem;
    MIResetSpeed: TMenuItem;
    MIInsertSequence: TMenuItem;
    MIRename: TMenuItem;
    MIDelete: TMenuItem;
    MIMerge: TMenuItem;
    MIDuplicate: TMenuItem;
    MIEdit: TMenuItem;
    MINewSequence: TMenuItem;
    PopupMenu1: TPopupMenu;
    Timer1: TTimer;
    procedure LBDrawItem({%H-}Control: TWinControl; Index: Integer; ARect: TRect;
      State: TOwnerDrawState);
    procedure LBKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure LBKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure LBMouseDown(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure LBMouseEnter(Sender: TObject);
    procedure LBMouseLeave(Sender: TObject);
    procedure LBMouseMove(Sender: TObject; {%H-}Shift: TShiftState; {%H-}X, Y: Integer);
    procedure LBMouseUp(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, Y: Integer);
    procedure LBSelectionChange(Sender: TObject; {%H-}User: boolean);
    procedure MIDeleteClick(Sender: TObject);
    procedure MIDuplicateClick(Sender: TObject);
    procedure MIEditClick(Sender: TObject);
    procedure MIInsertSequenceClick(Sender: TObject);
    procedure MINewSequenceClick(Sender: TObject);
    procedure MIResetSpeedClick(Sender: TObject);
    procedure MIRenameClick(Sender: TObject);
    procedure MIStopClick(Sender: TObject);
    procedure PopupMenu1Close(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    function GetSelCount: integer;
    function GetSelectedTop: TSequence;
  private
    FSpacePressed,
    FMouseIsOver,
    FPopupIsVisible: boolean;
    FItemIndexUnderMouse: integer;

    FMouseCanMoveItem: boolean;
    FMouseOrigin: TPoint;
    FLeftClickedIndex: integer;
  private
    FErrorImage: TBGRABitmap;
    FNameFontHeight,
    FInfoFontHeight: integer;
    function GetItemHeight: integer;
    procedure SetItemHeight(AValue: integer);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground({%H-}DC: HDC); override;
    procedure ProcessKeyDown(var Key: Word; {%H-}Shift: TShiftState);
    procedure ProcessKeyUp(var Key: Word; {%H-}Shift: TShiftState);

    procedure Fill;

    procedure Add(aID: cardinal);
    procedure Insert(aID: cardinal);
    procedure DeleteSelection;
    // return TRUE if the offset was applyed
    function MoveSelection( aOffset: integer): boolean;

    property MouseCanMoveItem: boolean read FMouseCanMoveItem write FMouseCanMoveItem;
    property ItemHeight: integer read GetItemHeight write SetItemHeight;

    property SelCount: integer read GetSelCount;
    property SelectedTop: TSequence read GetSelectedTop;
    property MouseIsOver: boolean read FMouseIsOver;
  end;

implementation

uses u_project_manager, u_resource_string, u_edit_sequence, u_userdialogs,
  u_audio_manager, u_mainform, u_common, u_apputils, Graphics,
  Dialogs, LCLHelper, Math, utilitaire_bgrabitmap;

{$R *.lfm}

{ TFrameViewTopList }

procedure TFrameViewTopList.LBDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var seq: TSequence;
  i, y, w, xx: integer;
  p: TPoint;
  SF: double;
begin
  SF := GetCanvasScaleFactor;

  with LB.Canvas do
  begin

   if State >= [odSelected] then
   begin
     if FMouseIsOver or FPopupIsVisible then
       Brush.Color := clHighLight
     else
       Brush.Color := RGBToColor(94,128,130);//clGray;
   end
   else
   begin
     if Index Mod 2 = 0 then
       Brush.Color := LB.Color
     else
       Brush.Color := PercentColor(LB.Color,0.15);
   end;

   if Index = FItemIndexUnderMouse then
   begin
     // render dot rectangle if mouse is over item
     Pen.Style := psDot;
     Pen.Color := PercentColor(LB.Color,0.95); //RGBToColor(200,200,150);
     Rectangle(ARect.Left-1, ARect.Top, ARect.Right+1, ARect.Bottom);
   end
   else FillRect(ARect);

   // render drag target position
   if FLeftClickedIndex <> -1 then
   begin
     p := LB.ScreenToClient(Mouse.CursorPos);
     p.y := EnsureRange(p.y, 0, LB.ClientHeight);
     i := LB.GetIndexAtY(p.Y);
     if i = Index then
     begin
      Pen.Style := psClear;
      Brush.Color := clHighLight;
      if i > FLeftClickedIndex then
        Rectangle(ARect.Left-1, ARect.Bottom-3, ARect.Right+1, ARect.Bottom)
      else
        Rectangle(ARect.Left-1, ARect.Top, ARect.Right+1, ARect.Top+3);
     end;
   end;
   Brush.Style := bsClear;

   seq := Sequences.GetTopByID( LB.Items.Strings[Index].ToInteger );
   if seq <> NIL then
   begin
     // render error icon
     if seq.HaveError then begin
       FErrorImage.Draw(LB.Canvas, aRect.Left, aRect.Top+FNameFontHeight-FErrorImage.Height, False);
       xx := aRect.Left + FErrorImage.Width + ScaleDesignToForm(2);
     end else xx := aRect.Left + ScaleDesignToForm(8);

     if seq.Running then begin
       Font.Color := $0000FFFF;
       Font.Style := [fsBold];
     end else begin
       Font.Color := $00EAEAEA;
       Font.Style := [];
     end;

     // Render name
     Font.Height := FNameFontHeight;
     TextOut(xx, aRect.Top, ExtractFileName(seq.Name));

     y := aRect.Bottom - FInfoFontHeight;
     Font.Height := FInfoFontHeight;
     Font.Color := $00EAEAEA;
     // render looped state
     w := Font.GetTextWidth(SLoop);
     if seq.IsLooped then
       Textout( aRect.Right-w-Round(10*SF), y, SLoop);

     // Render Running state
     if seq.Running then
       Textout( aRect.Left+Round(10*SF), y, Srunning);

     w := Font.GetTextWidth(Srunning)+Round(20*SF);
     // render time stretch factor
     if seq.TimeStretchFactor.Value <> 1.0 then
       Textout( aRect.Left+w, y, 'x'+FormatFloat('0.00', seq.TimeStretchFactor.Value));


     Font.Height := LB.ItemHeight;
   end;
 end;
end;

procedure TFrameViewTopList.LBKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_SPACE then
    Key := VK_UNKNOWN;
end;

procedure TFrameViewTopList.LBKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_SPACE then
    Key := VK_UNKNOWN;
end;

procedure TFrameViewTopList.LBMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var i: integer;
begin
  i := LB.GetIndexAtY(Y);

  // click on empty area = unselect all
  if (Button = mbLeft) and (i = -1) then
    LB.ItemIndex := -1;

  // prepare for drag
  if (Button = mbLeft) and (i <> -1) and FMouseCanMoveItem and
     (LB.Count > 1) then
  begin
    FLeftClickedIndex := i;
    FMouseOrigin := LB.ClientToScreen(Point(X,Y));
  end
  else FLeftClickedIndex := -1;

  // right click on item = select it
  if (Button = mbRight) and (i <> -1) then
    LB.ItemIndex := i;
end;

procedure TFrameViewTopList.LBMouseEnter(Sender: TObject);
begin
 FMouseIsOver := TRUE;
 LB.Invalidate;
end;

procedure TFrameViewTopList.LBMouseLeave(Sender: TObject);
begin
 if FLeftClickedIndex <> -1 then
   exit;

 FItemIndexUnderMouse := -1;
 FMouseIsOver := FALSE;
 FSpacePressed := FALSE;
 LB.Cursor := crDefault;
 LB.Invalidate;
end;

procedure TFrameViewTopList.LBMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var i: integer;
begin
  // dragging an item
  if (FLeftClickedIndex <> -1) and (LB.ItemIndex <> FLeftClickedIndex) then
  begin
    LB.ItemIndex := FLeftClickedIndex;
    LB.Invalidate;
    exit;
  end;

  // mouse is over an item
  i := LB.GetIndexAtY(Y);
  if i <> FItemIndexUnderMouse then
  begin
    if i = -1 then
      LB.Cursor := crDefault
    else
      LB.Cursor := crHandPoint;
    FItemIndexUnderMouse := i;
    LB.Invalidate;
  end;
end;

procedure TFrameViewTopList.LBMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var i: integer;
begin
 if (Button = mbLeft) and (FLeftClickedIndex <> -1) then
 begin
   Y := EnsureRange(Y, 0, LB.ClientHeight);
   i := LB.GetIndexAtY(Y);
   if (i <> -1) and (i <> FLeftClickedIndex) then
   begin
     LB.MoveSelection(i-FLeftClickedIndex);
     Sequences.Move(FLeftClickedIndex, i);
     Project.SetModified;
   end;
   FLeftClickedIndex := -1;
   LB.Invalidate;
 end;
end;

procedure TFrameViewTopList.LBSelectionChange(Sender: TObject; User: boolean);
var i: integer;
  seq: TSequence;
begin
  if LB.ItemIndex = -1 then exit;
  if not TryStrToInt(LB.GetSelectedText, i) then exit;

  seq := Sequences.GetTopByID(i);
  if seq = NIL then exit;

  if seq.HaveError then LB.Hint := seq.ErrorMessage
    else LB.Hint := '';
end;

procedure TFrameViewTopList.MIDeleteClick(Sender: TObject);
var t: TSequence;
begin
  t := SelectedTop;
  if t = NIL then
    exit;

  if AskConfirmation(SAreYouSureToDeleteThisSequence, SYes, SCancel, mtWarning)<>mrOk then
    exit;
  DeleteSelection;
  Project.SetModified;
end;

procedure TFrameViewTopList.MIDuplicateClick(Sender: TObject);
var t: TSequence;
begin
  t := SelectedTop;
  if t = NIL then
    exit;

  t := Sequences.Duplicate(t.ID);
  if t = NIL then
    exit;
  Add(t.ID);
  Project.SetModified;
end;

procedure TFrameViewTopList.MIEditClick(Sender: TObject);
var seq: TSequence;
  flagError: Boolean;
begin
  seq := SelectedTop;
  if seq = NIL then exit;
  flagError := seq.HaveError;

  FormMain.FrameViewProjector1.FreeOpenGLTextures;

  FormSequenceEdition := TFormSequenceEdition.Create(Self);
  try
    FormSequenceEdition.SetModifyMode(seq.Name, seq.SequencerInfoList, LB.ItemIndex);
    if FormSequenceEdition.ShowModal = mrOk then begin
      seq.Stop;
      seq.Name := FormSequenceEdition.TopName;
      seq.SequencerInfoList := FormSequenceEdition.SequencerInfoList;
      Project.SetModified;

      if flagError then FormMain.CheckSequenceError;
    end;
  finally
    FormSequenceEdition.Free;
    FormSequenceEdition := NIL;
  end;

  FormMain.FrameViewProjector1.ForceReconstructOpenGLObjects;
end;

procedure TFrameViewTopList.MIInsertSequenceClick(Sender: TObject);
begin
 SoundManager.StopAllSound;
 SoundManager.DeleteAllEffects;
 Sequences.StopAll;

 FormSequenceEdition := TFormSequenceEdition.Create(Self);
 try
   FormSequenceEdition.SetAddMode( SSequence+' '+(Sequences.Count+1).ToString );
   if FormSequenceEdition.ShowModal = mrOk then
   begin
     with Sequences.InsertTop(LB.ItemIndex, FormSequenceEdition.TopName, FormSequenceEdition.SequencerInfoList) do
       Insert(ID);
     Project.SetModified;
   end;
 finally
   FormSequenceEdition.Free;
   FormSequenceEdition := NIL;
 end;

 FormMain.FrameViewProjector1.ForceReconstructOpenGLObjects;
end;

procedure TFrameViewTopList.MINewSequenceClick(Sender: TObject);
var seq: TSequence;
begin
 // keep the capture ON
  SoundManager.StopAllSound;
  SoundManager.DeleteAllEffects;

  Sequences.StopAll;

  FormSequenceEdition := TFormSequenceEdition.Create(Self);
  try
    FormSequenceEdition.SetAddMode( SSequence+' '+(Sequences.Count+1).ToString );
    if FormSequenceEdition.ShowModal = mrOk then
    begin
      seq := Sequences.AddTop(FormSequenceEdition.TopName, FormSequenceEdition.SequencerInfoList);
      Add(seq.ID);
      Project.SetModified;
    end;
  finally
    FormSequenceEdition.Free;
    FormSequenceEdition := NIL;
  end;

  FormMain.FrameViewProjector1.ForceReconstructOpenGLObjects;
end;

procedure TFrameViewTopList.MIResetSpeedClick(Sender: TObject);
var t: TSequence;
begin
  t := GetSelectedTop;
  if t = NIL then
    exit;
  t.TimeStretchFactor.Value := 1.0;
end;

procedure TFrameViewTopList.MIRenameClick(Sender: TObject);
var t: TSequence;
  n: string;
begin
  t := GetSelectedTop;
  if t = NIL then
    exit;
  n := t.Name;
  if UserInputNoSpecialChar(SNewName, SOk, SCancel, n, mtConfirmation, FALSE) <> mrOk then
    exit;
  if t.Name <> n then
  begin
    t.Name := n;
    Project.SetModified;
  end;
end;

procedure TFrameViewTopList.MIStopClick(Sender: TObject);
var t: TSequence;
begin
  t := GetSelectedTop;
  if t = NIL then
    exit;
  if t.Running
    then t.Stop;
end;

procedure TFrameViewTopList.PopupMenu1Close(Sender: TObject);
begin
  FPopupIsVisible := False;
end;

procedure TFrameViewTopList.PopupMenu1Popup(Sender: TObject);
var seq: TSequence;
begin
  FPopupIsVisible := True;

  seq := GetSelectedTop;
  if seq = NIL then
  begin
    MIStop.Enabled := False;
    MIResetSpeed.Enabled := False;
  end
  else begin
    MIStop.Enabled := seq.Running;
    MIResetSpeed.Enabled := seq.TimeStretchFactor.Value <> 1.0;
  end;

  MenuItem1.Visible := Project.Options.EditMode;
  MenuItem2.Visible := Project.Options.EditMode;

  MINewSequence.Enabled := Project.IsReady and Project.Options.EditMode;

  MIInsertSequence.Enabled := (LB.Itemindex <> -1) and Project.Options.EditMode;

  MIEdit.Enabled := (SelCount = 1) and Project.Options.EditMode;

  MIDuplicate.Enabled := (SelCount = 1) and Project.Options.EditMode;

  MIMerge.Enabled := (SelCount > 1) and Project.Options.EditMode;

  MIDelete.Enabled := (SelCount > 0) and Project.Options.EditMode;

  MIRename.Enabled := (SelCount = 1) and Project.Options.EditMode;

  MILoadFromAnotherProject.Enabled := Project.IsReady and Project.Options.EditMode;
end;

procedure TFrameViewTopList.Timer1Timer(Sender: TObject);
begin
 if FLeftClickedIndex = -1 then
   LB.Invalidate;
end;

function TFrameViewTopList.GetSelCount: integer;
begin
  Result := LB.SelCount;
end;

function TFrameViewTopList.GetSelectedTop: TSequence;
begin
  if LB.SelCount <> 1 then
    Result := NIL
  else
    Result := Sequences.GetTopByIndex(LB.ItemIndex);
end;

function TFrameViewTopList.GetItemHeight: integer;
begin
  Result := LB.ItemHeight;
end;

procedure TFrameViewTopList.SetItemHeight(AValue: integer);
begin
  LB.ItemHeight := EnsureRange(AValue, 20, 50);
  LB.Font.Height := LB.ItemHeight;
  FNameFontHeight := Round(LB.ItemHeight/1.5);
  FInfoFontHeight := Round(LB.ItemHeight/2.5);
end;

constructor TFrameViewTopList.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ItemHeight := 25;
  FItemIndexUnderMouse := -1;
  FLeftClickedIndex := -1;

  FErrorImage := SVGFileToBGRABitmap(GetAppIconImagesFolder+'SequenceErrorSymbol.svg', -1, ScaleDesignToForm(16));
end;

destructor TFrameViewTopList.Destroy;
begin
  FreeAndNil(FErrorImage);
  inherited Destroy;
end;

procedure TFrameViewTopList.ProcessKeyDown(var Key: Word; Shift: TShiftState);
var i, id: integer;
  pt: TSequence;
begin
  i := LB.ItemIndex;
  if i = -1 then
    exit;
  if not FMouseIsOver then
    exit;

  case Key of
    VK_SPACE: if not FSpacePressed then
    begin
      FSpacePressed := TRUE;
      id := LB.Items.Strings[i].ToInteger;
      pt := Sequences.GetTopByID(id);
      if pt <> NIL then
      begin
        if not pt.Running then
          pt.RunAsSequencerInfoList;
      end;
      Key := VK_UNKNOWN;
    end;

    VK_UP: if LB.Count > 0 then
    begin
      if i > 0 then
        LB.ItemIndex := i-1;
      Key := VK_UNKNOWN;
    end;

    VK_DOWN: if LB.Count>0 then
    begin
      if i < LB.Count-1 then
        LB.ItemIndex := i+1;
      Key := VK_UNKNOWN;
    end;
  end;// case
end;

procedure TFrameViewTopList.ProcessKeyUp(var Key: Word; Shift: TShiftState);
begin
  if not FMouseIsOver then
    exit;

  if Key = VK_SPACE then
    FSpacePressed := FALSE;
end;

procedure TFrameViewTopList.EraseBackground(DC: HDC);
begin
// do nothing
end;

procedure TFrameViewTopList.Fill;
var i: Integer;
begin
  LB.LockSelectionChange;
  LB.Clear;

  for i:=0 to Sequences.Count-1 do
    LB.Items.Add( Sequences.GetTopByIndex(i).ID.ToString );

  LB.UnlockSelectionChange;
  FLeftClickedIndex := -1;

  MIDelete.Caption := SDelete;
  MIStop.Caption := SStop;
end;

procedure TFrameViewTopList.Add(aID: cardinal);
begin
  LB.ItemIndex := LB.Items.Add( aID.ToString );
end;

procedure TFrameViewTopList.Insert(aID: cardinal);
var i: integer;
begin
  i := LB.ItemIndex;
  LB.Items.Insert(i, aID.ToString);
  LB.ItemIndex := i;
end;

procedure TFrameViewTopList.DeleteSelection;
var i: Integer;
  flag: boolean;
begin
  if LB.SelCount=0 then
    exit;
  flag := FALSE;
  for i:=LB.Count-1 downto 0 do
   if LB.Selected[i] then
   begin
     Sequences.Delete(i);
     LB.Items.Delete(i);
     flag := TRUE;
   end;
  if flag then
    Project.SetModified;
end;

function TFrameViewTopList.MoveSelection(aOffset: integer): boolean;
var i, index, newindex: integer;
begin
  Result := FALSE;
  if (LB.SelCount = 0) or (aOffset = 0) then
    exit;

  if aOffset<0 then
  begin
    // move up
    index := LB.FirstSelectedIndex;
    if index = 0 then
      exit;
    newindex := index+aOffset;
    if newindex < 0 then
      newindex := 0;
    aOffset := newindex-index;
    for i:=index to LB.Count-1 do
      if LB.Selected[i] then
        Sequences.Move(i, i+aOffset);
  end
  else begin
    // move down
    index := LB.LastSelectedIndex;
    if index = LB.Count-1 then
      exit;
    newindex := index+aOffset;
    if newindex > LB.Count-1 then
      newindex := LB.Count-1;
    aOffset := newindex-index;
    for i:=LB.Count-1 downto index do
      if LB.Selected[i] then
        Sequences.Move(i, i+aOffset);
  end;
  LB.MoveSelection(aOffset);
  Result := TRUE;
end;

end.

