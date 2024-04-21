unit frame_viewaudiolist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Types, LCLType, Graphics,
  LCLTranslator, ExtCtrls,
  ALSound, u_audio_manager;

type

  { TFrameViewAudioList }

  TFrameViewAudioList = class(TFrame)
    LB: TListBox;
    Timer1: TTimer;
    procedure LBDrawItem({%H-}Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure LBKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure LBKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure LBMouseDown(Sender: TObject; Button: TMouseButton;{%H-}Shift: TShiftState; X, Y: Integer);
    procedure LBMouseEnter(Sender: TObject);
    procedure LBMouseLeave(Sender: TObject);
    procedure LBMouseMove(Sender: TObject; {%H-}Shift: TShiftState; {%H-}X, Y: Integer);
    procedure LBMouseUp(Sender: TObject; Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, Y: Integer);
    procedure LBResize(Sender: TObject);
    procedure LBSelectionChange(Sender: TObject; {%H-}User: boolean);
    procedure Timer1Timer(Sender: TObject);
  private
    FOnSelectionChange,
    FOnItemRightClick: TNotifyEvent;
    function GetCount: integer;
    function GetMultiSelect: boolean;
    function GetSelectedCount: integer;
    procedure SetMultiSelect(AValue: boolean);
  private
    FMouseCanMoveItem: boolean;
    FMouseOrigin: TPoint;
    FLeftClickedIndex: integer;
  private
    FSpacePressed,
    FMouseIsOver: boolean;
    FItemIndexUnderMouse: integer;
    c_nomstop,
    c_nombouclestop,
    c_nomplay,
    c_nompausehaut,
    c_nompausebas,
    c_nombouclehaut,
    c_nombouclebas,
    c_infoboucle,
    c_infopausehaut,
    c_infopausebas: TColor;
    FFlashCounter: integer;
    FFlashState: boolean;
  private
    FNameFontHeight,
    FInfoFontHeight: integer;
    function GetItemHeight: integer;
    procedure SetItemHeight(AValue: integer);
    procedure SetMouseCanMoveItem(AValue: boolean);
    procedure ErrorMultiSelectAndMoveItem;
  private
    FFileNameAreaWidth: integer;
    FTextStyleForTextRect: TTextStyle;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure EraseBackground({%H-}DC: HDC); override;

    procedure Redraw;

    procedure Fill;
    procedure StopTimer;

    procedure ProcessKeyDown(var Key: Word; {%H-}Shift: TShiftState);
    procedure ProcessKeyUp(var Key: Word; {%H-}Shift: TShiftState);

    procedure PlaySelected(aFromBegining: boolean);
    procedure StopSelected;
    procedure PauseSelected;
    procedure CreateEffectOnSelected(aEffectType: TALSEffectType; aPresetIndex: integer);
    procedure ChainEffectOnSelected;
    procedure RemoveEffectsOnSelected;

    procedure SetNormalPitchOnSelected;
    procedure SetVolumeOnSelected( aVol: single);
    procedure SetPanOnSelected( aPan: single);
    procedure SetPitchOnSelected( aValue: single );
    procedure SetDryWetOnSelected(aValue: single);

    procedure ToogleLoopOnSelected;

    procedure Sel_None;
    function FirstSelected: TALSSound;
    function GetSelected: ArrayOfSound;
    procedure DeleteSelected;

    property MouseCanMoveItem: boolean read FMouseCanMoveItem write SetMouseCanMoveItem;
    property MultiSelect: boolean read GetMultiSelect write SetMultiSelect;
    property Count: integer read GetCount;
    property SelectedCount: integer read GetSelectedCount;

    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
    property OnItemRightClick: TNotifyEvent read FOnItemRightClick write FOnItemRightClick;
    property MouseIsOver: boolean read FMouseIsOver;
    property ItemHeight: integer read GetItemHeight write SetItemHeight;
  end;

implementation
uses LCLHelper, u_resource_string, u_utils, u_project_manager, u_common,
  u_datamodule, VelocityCurve, Math, LazUTF8;

{$R *.lfm}

{ TFrameViewAudioList }

procedure TFrameViewAudioList.LBDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var txt: string;
 xx, yy, i, margin: integer;
 snd: TALSSound;
 p: TPoint;
begin
 with LB.Canvas do
 begin
   if State >= [odSelected] then
   begin
     if FMouseIsOver then
       Brush.Color := clHighLight
     else
       Brush.Color := RGBToColor(94,128,130);
     Font.Color := clWhite;
   end
   else begin
     if Index Mod 2 = 0 then
       Brush.Color := LB.Color
     else
       Brush.Color := PercentColor(LB.Color,0.15);
     Font.Color := $009FD1EC;
   end;
   // render dot rectangle if mouse is over item
   if Index = FItemIndexUnderMouse then
   begin
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
     i := LB.GetIndexAtY(p.y);
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

   snd := SoundManager.GetSoundByID(LB.Items.Strings[Index].ToInteger);

   margin := Round(DataModule1.ImageList1.Width*0.4);
   // render loop symbol
   if snd.Loop then
   begin
     DataModule1.ImageList1.Draw(LB.Canvas, ARect.Left,
        ARect.Top+(FNameFontHeight-DataModule1.ImageList1.Height) div 2, 40);
       // Rect(xx, ARect.Top, xx+loopImageWidth, ARect.Top+FNameFontHeight));
   end;
   xx := ARect.Left + DataModule1.ImageList1.Width + margin;

   // render audio filename
   Font.Height := FNameFontHeight;

   if snd = NIL then
   begin
     txt := SUnknownAudio;
     Font.Color := c_nomstop;
     Font.Style := [];
   end
   else begin
     case snd.State of
       ALS_PLAYING:
         begin
           if snd.Loop then
             Font.Color := c_nombouclehaut
           else
             Font.Color := c_nomplay;
           Font.Style := [fsBold];
           txt := ExtractFilename(snd.Filename);
       end;

       ALS_PAUSED:
         begin
           if snd.Loop then
           begin
             if FFlashState then
               Font.Color := c_nombouclehaut
             else
               Font.Color := c_nombouclebas;
           end
           else
           begin
             if FFlashState then
               Font.Color := c_nompausehaut
             else
               Font.Color := c_nompausebas;
           end;
           Font.Style := [fsBold];
           txt := ExtractFilename(snd.Filename);
         end;

       ALS_STOPPED:
         begin // render stopped audio tracks
           if snd.Loop then
             Font.Color := c_nombouclestop
           else
             Font.Color := c_nomstop;
           Font.Style := [];
           txt := (ExtractFilename(snd.Filename);
         end;
     end;//case
     end;

     TextRect(Rect(xx, ARect.Top,
                   xx+FFileNameAreaWidth, ARect.Bottom-FInfoFontHeight),
                   xx, ARect.Top, txt, FTextStyleForTextRect);
     if snd <> NIL then
     begin
       xx := xx + FFileNameAreaWidth + margin;
       Font.Height := FInfoFontHeight;
       // render sound state infos
       case snd.State of
        ALS_PAUSED:
          begin
            yy := ARect.Top;
            Font.Style := [];
            if FFlashState then
              Font.Color := c_infopausehaut
            else
              Font.Color := c_infopausebas;
            TextOut(xx, yy, SPaused);
            txt := inttostr(Round(snd.TimePosition*100 / snd.TotalDuration))+ '%';
            xx := xx + (Font.GetTextWidth( SPaused )-Font.GetTextWidth(txt)) div 2;
            TextOut(xx, yy+FInfoFontHeight, txt);
        end;
        ALS_PLAYING:
          begin
            yy := ARect.Top;
            Font.Color := c_infopausehaut;
            Font.Style := [fsBold];
            TextOut(xx, yy, SPlaying);
            txt := inttostr(Round(snd.TimePosition*100 / snd.TotalDuration))+ '%';
            xx := xx + (Font.GetTextWidth( SPlaying )-Font.GetTextWidth(txt)) div 2;
            TextOut(xx, yy+FInfoFontHeight, txt);
          end;
        ALS_STOPPED:
          begin
            yy := ARect.Top + FNameFontHeight - FInfoFontHeight;
            Font.Color:=c_nomstop;
            Font.Style:=[];
            TextOut(xx, yy, SStop);
        end;
       end;//case

       // render effect names
       Brush.Style := bsClear;
       yy := arect.Bottom-FInfoFontHeight;
       Font.Color := $00EAEAEA; //c_infopausehaut;
       Font.Style := [];
       TextOut(arect.Left + 10, yy, SoundManager.GetEffectsNamesOn(LB.Items.Strings[Index].ToInteger));
     end;
  end;
end;

procedure TFrameViewAudioList.LBKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_SPACE then
    Key := VK_UNKNOWN;
end;

procedure TFrameViewAudioList.LBKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_SPACE then
    Key := VK_UNKNOWN;
end;

procedure TFrameViewAudioList.LBMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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

  // right click on item = select it (except if it is already selected)
  if (Button = mbRight) and (i <> -1) then
    if not LB.Selected[i] then
      LB.ItemIndex := i;
end;

procedure TFrameViewAudioList.LBMouseEnter(Sender: TObject);
begin
  FMouseIsOver := TRUE;
  LB.Invalidate;
end;

procedure TFrameViewAudioList.LBMouseLeave(Sender: TObject);
begin
  if FLeftClickedIndex <> -1 then
    exit;

  FItemIndexUnderMouse := -1;
  FMouseIsOver := FALSE;
  FSpacePressed := FALSE;
  LB.Cursor := crDefault;
  LB.Invalidate;
end;

procedure TFrameViewAudioList.LBMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var i: integer;
begin
  // dragging an item
  if (FLeftClickedIndex <> -1) and (LB.ItemIndex <> FLeftClickedIndex) then
  begin
    LB.ItemIndex := FLeftClickedIndex;
    LB.Invalidate;
    exit;
  end;

 // check if the mouse is over an item
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

procedure TFrameViewAudioList.LBMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var i: integer;
 flagCanMove: boolean;
begin
 if (Button = mbLeft) and (FLeftClickedIndex <> -1) then
 begin
   Y := EnsureRange(Y, 0, LB.ClientHeight);
   i := LB.GetIndexAtY(Y);
   // user can moves item only if one is selected
   flagCanMove := (i <> -1) and (i <> FLeftClickedIndex);

   if flagCanMove then
   begin
     LB.MoveSelection(i-FLeftClickedIndex);
{ #todo : l'ancien comportement était de déplacer les items de la liste en même temps que ceux de L'audio manager. Il ne faut que garder le déplacement dans la liste puis à la fin reconstruire la liste pour l'audio manager (uniquement pour les playlist entracte) }
//     SoundManager.MoveByIndex(FLeftClickedIndex, i);
     Project.SetModified;
   end;
   FLeftClickedIndex := -1;
   LB.Invalidate;
 end;

 if (Button = mbRight) then
 begin
   Y := EnsureRange(Y, 0, LB.ClientHeight);
   i := LB.GetIndexAtY(Y);
   if (i <> -1) and (FOnItemRightClick <> NIL) then
     FOnItemRightClick(Self);
 end;
end;

procedure TFrameViewAudioList.LBResize(Sender: TObject);
begin
  FFileNameAreaWidth := Round(LB.ClientWidth*0.7);
end;

procedure TFrameViewAudioList.LBSelectionChange(Sender: TObject; User: boolean);
begin
  if FOnSelectionChange <> NIL then
    FOnSelectionChange(Self);
end;

procedure TFrameViewAudioList.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := FALSE;
  inc(FFlashCounter);
  if FFlashCounter >= 3 then
  begin
    FFlashState := not FFlashState;
    FFlashCounter := 0;
    if LB.Count > 0 then
      LB.Invalidate;
  end;
  Timer1.Enabled := TRUE;
end;

function TFrameViewAudioList.GetCount: integer;
begin
  Result := LB.Count;
end;

function TFrameViewAudioList.GetMultiSelect: boolean;
begin
  Result := LB.MultiSelect;
end;

function TFrameViewAudioList.GetSelectedCount: integer;
begin
  Result := LB.SelCount;
end;

procedure TFrameViewAudioList.SetMultiSelect(AValue: boolean);
begin
  if MouseCanMoveItem then
    ErrorMultiSelectAndMoveItem;

  LB.MultiSelect := AValue;
  LB.ExtendedSelect := AValue;
end;

function TFrameViewAudioList.GetItemHeight: integer;
begin
  Result := LB.ItemHeight;
end;

procedure TFrameViewAudioList.SetItemHeight(AValue: integer);
begin
  LB.ItemHeight := EnsureRange(AValue, 20, 50);
 // LB.Font.Height := Round(LB.ItemHeight*0.4);
  FNameFontHeight := Round(LB.ItemHeight*0.6);
  FInfoFontHeight := Round(LB.ItemHeight*0.4);
end;

procedure TFrameViewAudioList.SetMouseCanMoveItem(AValue: boolean);
begin
  if MultiSelect then
    ErrorMultiSelectAndMoveItem;

  if FMouseCanMoveItem = AValue then
    Exit;
  FMouseCanMoveItem := AValue;
end;

procedure TFrameViewAudioList.ErrorMultiSelectAndMoveItem;
begin
  Raise Exception.Create('TFrameViewAudioList: Cannot have both "MultiSelect" and "MouseCanMoveItem"');
end;

constructor TFrameViewAudioList.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FItemIndexUnderMouse := -1;
  FLeftClickedIndex := -1;
  ItemHeight := 30;

  c_nomstop := $00EAEAEA;
  c_nombouclestop := c_nomstop;
  c_nomplay := clYellow;
  c_nompausehaut := clYellow;
  c_nompausebas := PercentColor(c_nompausehaut, -0.4);
  c_nombouclehaut := clYellow;
  c_nombouclebas := PercentColor( c_nombouclehaut, -0.4);
  c_infoboucle := RGBToColor(75,250,0);
  c_infopausehaut := clYellow;
  c_infopausebas := PercentColor(c_nompausehaut, -0.6);

  FTextStyleForTextRect.Alignment := taLeftJustify;
  FTextStyleForTextRect.Layout := Graphics.tlCenter;
  FTextStyleForTextRect.SingleLine := FALSE;
  FTextStyleForTextRect.Wordbreak := TRUE;
  FTextStyleForTextRect.Clipping := TRUE;
  FTextStyleForTextRect.Opaque := FALSE;
  FTextStyleForTextRect.SystemFont := FALSE;
end;

procedure TFrameViewAudioList.EraseBackground(DC: HDC);
begin
// do nothing here
end;

procedure TFrameViewAudioList.Redraw;
begin
  LB.Invalidate;
end;

procedure TFrameViewAudioList.Fill;
var i: Integer;
begin
  LB.LockSelectionChange;
  LB.Clear;

  for i:=0 to SoundManager.Count-1 do
    if SoundManager.GetSoundByIndex(i).Tag <> CAPTURE_IDAUDIO then
      LB.Items.Add( SoundManager.GetSoundByIndex(i).Tag.ToString );

  LB.UnlockSelectionChange;
  Timer1.Enabled := TRUE;

  FLeftClickedIndex := -1;
  FItemIndexUnderMouse := -1;
  LB.ItemIndex := -1;
end;

procedure TFrameViewAudioList.StopTimer;
begin
  Timer1.Enabled := FALSE;
end;

procedure TFrameViewAudioList.ProcessKeyDown(var Key: Word; Shift: TShiftState);
var S: ArrayOfSound;
begin
  S := GetSelected;
  if Length(S) = 0 then
    exit;

  case Key of
    VK_SPACE:  // sound on/off
      begin
       if not FSpacePressed then
       begin
        if not S[0].Error then
        begin
          if (S[0].State = ALS_PLAYING) or (S[0].State = ALS_PAUSED) then
            StopSelected
          else
            PlaySelected(True);
          LB.Invalidate;
        end;
        FSpacePressed := TRUE;
      end;
      Key := VK_UNKNOWN;
    end;

    VK_P:        // pause
      begin
       if (S[0].State = ALS_PLAYING) or (S[0].State = ALS_PAUSED) then
       begin
        PauseSelected;
        LB.Invalidate;
      end;
    end;
  end;//case
end;

procedure TFrameViewAudioList.ProcessKeyUp(var Key: Word; Shift: TShiftState);
begin
  case Key of
   VK_SPACE: FSpacePressed := FALSE;
  end;
end;

procedure TFrameViewAudioList.PlaySelected(aFromBegining: boolean);
var i, id: Integer;
  o: TALSSound;
begin
  for i:=0 to LB.Count-1 do
    if LB.Selected[i] then
    begin
      id := LB.Items.Strings[i].ToInteger;
      if id <> CAPTURE_IDAUDIO then
      begin
        o := SoundManager.GetSoundByID( id );
        if o <> NIL then
          o.Play( aFromBegining );
      end;
    end;
end;

procedure TFrameViewAudioList.StopSelected;
var i, id: Integer;
  o: TALSSound;
begin
  for i:=0 to LB.Count-1 do
    if LB.Selected[i] then
    begin
      id := LB.Items.Strings[i].ToInteger;
      if id <> CAPTURE_IDAUDIO then
      begin
        o := SoundManager.GetSoundByID( id );
        if o <> NIL then
          o.Stop;
      end;
    end;
end;

procedure TFrameViewAudioList.PauseSelected;
var i, id: Integer;
  o: TALSSound;
begin
  for i:=0 to LB.Count-1 do
    if LB.Selected[i] then
    begin
      id := LB.Items.Strings[i].ToInteger;
      if id <> CAPTURE_IDAUDIO then
      begin
       o := SoundManager.GetSoundByID( id );
       if o <> NIL then
         o.Pause;
      end;
    end;
end;

procedure TFrameViewAudioList.CreateEffectOnSelected(aEffectType: TALSEffectType; aPresetIndex: integer);
var snd: TALSSound;
  sel: ArrayOfSound;
begin
  sel := GetSelected;
  for snd in sel do
    SoundManager.AddEffectOn(snd.Tag, aEffectType, aPresetIndex);
end;

procedure TFrameViewAudioList.ChainEffectOnSelected;
var snd: TALSSound;
  sel: ArrayOfSound;
begin
  sel := GetSelected;
  for snd in sel do
    SoundManager.ConstructChainOn(snd.Tag);
end;

procedure TFrameViewAudioList.RemoveEffectsOnSelected;
var snd: TALSSound;
  sel: ArrayOfSound;
begin
  sel := GetSelected;
  for snd in sel do
    SoundManager.DeleteEffectsOn(snd.Tag);
end;


procedure TFrameViewAudioList.SetNormalPitchOnSelected;
var i: Integer;
  snd: TALSSound;
begin
 for i:=0 to LB.Count-1 do
   if LB.Selected[i] then
   begin
     snd := SoundManager.GetSoundByID(LB.Items.Strings[i].ToInteger);
     if snd <> NIL then
       snd.Pitch.Value := ALS_PITCH_NORMAL;
   end;
end;

procedure TFrameViewAudioList.SetVolumeOnSelected(aVol: single);
var i: Integer;
  snd: TALSSound;
begin
  for i:=0 to LB.Count-1 do
    if LB.Selected[i] then
    begin
      snd := SoundManager.GetSoundByID(LB.Items.Strings[i].ToInteger);
      if snd <> NIL then
        snd.Volume.Value := aVol;
    end;
end;

procedure TFrameViewAudioList.SetPanOnSelected(aPan: single);
var i: Integer;
  snd: TALSSound;
begin
  for i:=0 to LB.Count-1 do
    if LB.Selected[i] then
    begin
      snd := SoundManager.GetSoundByID(LB.Items.Strings[i].ToInteger);
      if snd <> NIL then
        snd.Pan.Value := aPan;
    end;
end;

procedure TFrameViewAudioList.SetPitchOnSelected(aValue: single);
var i: Integer;
  snd: TALSSound;
begin
  for i:=0 to LB.Count-1 do
    if LB.Selected[i] then
    begin
      snd := SoundManager.GetSoundByID(LB.Items.Strings[i].ToInteger);
      if snd <> NIL then
        snd.Pitch.Value := aValue;
    end;
end;

procedure TFrameViewAudioList.SetDryWetOnSelected(aValue: single);
var snd: TALSSound;
  sel: ArrayOfSound;
begin
  sel := GetSelected;
  for snd in sel do
    SoundManager.SetDryWetOn(snd.Tag, aValue);
end;

procedure TFrameViewAudioList.ToogleLoopOnSelected;
var snd: TALSSound;
  sel: ArrayOfSound;
begin
  sel := GetSelected;
  for snd in sel do
    SoundManager.ToogleLoopModeOn(snd.Tag);
end;

function TFrameViewAudioList.FirstSelected: TALSSound;
begin
  if LB.SelCount = 0 then
    Result := NIL
  else
    Result := SoundManager.GetSoundByID(LB.Items.Strings[LB.FirstSelectedIndex].ToInteger);
end;

procedure TFrameViewAudioList.Sel_None;
begin
  LB.ItemIndex := -1;
end;

function TFrameViewAudioList.GetSelected: ArrayOfSound;
var i, k: integer;
begin
 Result := NIL;
  SetLength(Result, 0);
  for i:=0 to LB.Count-1 do
    if LB.Selected[i] then
    begin
      k := Length(Result);
      SetLength(Result, k+1);
      Result[k] := SoundManager.GetSoundByID(LB.Items.Strings[i].ToInteger);
    end;
end;

procedure TFrameViewAudioList.DeleteSelected;
var i: integer;
  id: integer;
  snd: TALSSound;
  f: string;
begin
  for i:=LB.Count-1 downto 0 do
    if LB.Selected[i] then
    begin
      id := LB.Items.Strings[i].ToInteger;
      snd := SoundManager.GetSoundByID(id);
      f := ExtractFileName(snd.Filename);
      // first remove from audio manager to release the file
      SoundManager.DeleteSoundByID(id);
      // remove from storage
      Project.AudioStorage.DeleteFile(f);
    end;
  // remove from listbox
  LB.DeleteSelected;
end;


end.

