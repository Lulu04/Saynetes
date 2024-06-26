unit frame_view_switcheritem;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Buttons, LCLType,
  u_list_dmxuniverse;

type

  { TFrameViewSwitcherItems }

  TFrameViewSwitcherItems = class(TFrame)
    BCopyPrevious: TSpeedButton;
    BDeleteSwitcher: TSpeedButton;
    BEditSwitcher: TSpeedButton;
    Panel1: TPanel;
    BAdd: TSpeedButton;
    PanelTools: TPanel;
    Shape4: TShape;
    Shape5: TShape;
    Timer1: TTimer;
    procedure BAddClick(Sender: TObject);
    procedure BCopyPreviousClick(Sender: TObject);
    procedure BEditSwitcherClick(Sender: TObject);
    procedure Panel1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
  private
    FOnCopyPrevious: TNotifyEvent;
    FOnHeightChange: TNotifyEvent;
    FVirtualNames: array of TLabel;
    FSeparator: array of TLabel;
    FSubNames: array of TLabel;
    FCounterForLabelNames: integer;
    procedure DoAddSwitcherItem(const aVirtualName, aSubChannelName: string);
    procedure DoDeleteSwitcherItem(aIndex: integer);
    function GetSwitcherCount: integer;
    procedure SetPanelToolPosition(aIndex: integer);
    procedure ProcessLabelMouseEnterEvent(Sender: TOBject);
    procedure ProcessLabelMouseLeaveEvent(Sender: TOBject);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure EraseBackground({%H-}DC: HDC); override;

    // init from 'virtualName1:subchannelName1'+LineEnding+
    //           'virtualName2:subchannelName2'+...
    procedure InitFromText(const s: string);
    // Gives 'virtualName1:subchannelName1'+LineEnding+
    //       'virtualName2:subchannelName2'+...
    function ToText: string;

    procedure SetSwitcherForRange(p: PFixLibSingleRange);

    property OnHeightChange: TNotifyEvent read FOnHeightChange write FOnHeightChange;
    property OnCopyPrevious: TNotifyEvent read FOnCopyPrevious write FOnCopyPrevious;
    property SwitcherCount: integer read GetSwitcherCount;
  end;

implementation

uses u_resource_string, form_defineswitcheritem;

{$R *.lfm}

{ TFrameViewSwitcherItems }

procedure TFrameViewSwitcherItems.BAddClick(Sender: TObject);
var F: TFormDefineSwitcher;
begin
  F := TFormDefineSwitcher.Create(Self);
  if F.ShowModal = mrOk then begin
    DoAddSwitcherItem(F.VirtualName, F.SubChannelName);
    ClientHeight := ClientHeight + FVirtualNames[0].Height;
    FOnHeightChange(Self);
    BCopyPrevious.Visible := False;
  end;
  F.Free;
end;

procedure TFrameViewSwitcherItems.BCopyPreviousClick(Sender: TObject);
begin
  BCopyPrevious.Visible := False;
  FOnCopyPrevious(Self);
end;

procedure TFrameViewSwitcherItems.BEditSwitcherClick(Sender: TObject);
var F: TFormDefineSwitcher;
  i, xx: integer;
begin
  if Sender = BEditSwitcher then begin
    i := PanelTools.Tag;
    if i = -1 then exit;
    F := TFormDefineSwitcher.Create(Self);
    F.EditSwitcher(FVirtualNames[i].Caption, FSubNames[i].Caption);
    if F.ShowModal = mrOk then begin
      xx := 0;
      FVirtualNames[i].Caption := F.VirtualName;
      xx := xx + Panel1.Canvas.GetTextWidth(F.VirtualName) + ScaleDesignToForm(2);
      FSeparator[i].Left := xx;
      FSubNames[i].Caption := F.SubChannelName;
      xx := xx + Panel1.Canvas.GetTextWidth(':') + ScaleDesignToForm(2);
      FSubNames[i].Left := xx;
      SetPanelToolPosition(-1);
    end;
  end;

  if Sender = BDeleteSwitcher then begin
    DoDeleteSwitcherItem(PanelTools.Tag);
    SetPanelToolPosition(-1);
  end;
end;

procedure TFrameViewSwitcherItems.Panel1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var i: integer;
begin
  if Length(FVirtualNames) = 0 then exit;
  i := Y div FVirtualNames[0].Height;
  if (i < 0) or (i >= Length(FVirtualNames)) then i := -1;
  SetPanelToolPosition(i);
end;

procedure TFrameViewSwitcherItems.Timer1Timer(Sender: TObject);
var p: TPoint;
begin
  Timer1.Enabled := False;
  p := Panel1.ScreenToClient(Mouse.CursorPos);
  if not Panel1.ClientRect.Contains(p) then
    SetPanelToolPosition(-1);
  Timer1.Enabled := True;
end;

procedure TFrameViewSwitcherItems.DoAddSwitcherItem(const aVirtualName, aSubChannelName: string);
var i, xx: integer;
begin
  i := Length(FVirtualNames);
  SetLength(FVirtualNames, i+1);
  SetLength(FSeparator, i+1);
  SetLength(FSubNames, i+1);

  xx := 0;
  // create label virtual name
  FVirtualNames[i] := TLabel.Create(Self);
  FVirtualNames[i].Name := 'MyVirtual'+FCounterForLabelNames.ToString;
  FVirtualNames[i].Parent := Panel1;
  FVirtualNames[i].Caption := aVirtualName;
  FVirtualNames[i].Left := xx;
  FVirtualNames[i].Top := FVirtualNames[i].Height * i;
  FVirtualNames[i].OnMouseEnter := @ProcessLabelMouseEnterEvent;
  FVirtualNames[i].OnMouseLeave := @ProcessLabelMouseLeaveEvent;
  FVirtualNames[i].Tag := i;

  xx := xx + Panel1.Canvas.GetTextWidth(aVirtualName) + ScaleDesignToForm(2);

  // create label ':'
  FSeparator[i] := TLabel.Create(Self);
  FSeparator[i].Name := 'Separator'+FCounterForLabelNames.ToString;
  FSeparator[i].Parent := Panel1;
  FSeparator[i].Caption := ':';
  FSeparator[i].Left := xx;
  FSeparator[i].Top := FVirtualNames[i].Top;
  FSeparator[i].OnMouseEnter := @ProcessLabelMouseEnterEvent;
  FSeparator[i].OnMouseLeave := @ProcessLabelMouseLeaveEvent;
  FSeparator[i].Tag := i;

  xx := xx + Panel1.Canvas.GetTextWidth(':') + ScaleDesignToForm(2);

  // create label sub-channel
  FSubNames[i] := TLabel.Create(Self);
  FSubNames[i].Name := 'Sub'+FCounterForLabelNames.ToString;
  FSubNames[i].Parent := Panel1;
  FSubNames[i].Caption := aSubChannelName;
  FSubNames[i].Left := xx;
  FSubNames[i].Top := FVirtualNames[i].Top;
  FSubNames[i].OnMouseEnter := @ProcessLabelMouseEnterEvent;
  FSubNames[i].OnMouseLeave := @ProcessLabelMouseLeaveEvent;
  FSubNames[i].Tag := i;

  inc(FCounterForLabelNames);
end;

procedure TFrameViewSwitcherItems.DoDeleteSwitcherItem(aIndex: integer);
var i, h: integer;
begin
  if (aIndex < 0) or (aIndex >= Length(FVirtualNames)) then exit;

  // free TLabels and array entries
  h := FVirtualNames[aIndex].Height;
  ClientHeight := ClientHeight - h;
  FVirtualNames[aIndex].Free;
  Delete(FVirtualNames, aIndex, 1);
  FSeparator[aIndex].Free;
  Delete(FSeparator, aIndex, 1);
  FSubNames[aIndex].Free;
  Delete(FSubNames, aIndex, 1);

  // decrement tag (index) and adjust y coordinates
  for i:=aIndex to High(FVirtualNames) do begin
    FVirtualNames[i].Tag := FVirtualNames[i].Tag - 1;
    FVirtualNames[i].Top := FVirtualNames[i].Top - h;
    FSeparator[i].Tag := FSeparator[i].Tag - 1;
    FSeparator[i].Top := FSeparator[i].Top - h;
    FSubNames[i].Tag := FSubNames[i].Tag - 1;
    FSubNames[i].Top := FSubNames[i].Top - h;
  end;

  FOnHeightChange(Self);
  BCopyPrevious.Visible := Length(FVirtualNames) = 0;
end;

function TFrameViewSwitcherItems.GetSwitcherCount: integer;
begin
  Result := Length(FVirtualNames);
end;

procedure TFrameViewSwitcherItems.SetPanelToolPosition(aIndex: integer);
var y: integer;
begin
  PanelTools.Tag := aIndex;
  PanelTools.Visible := aIndex <> -1;
  if PanelTools.Visible then begin
    if aIndex = 0 then y := 0
      else y := FVirtualNames[aIndex].Top + FVirtualNames[aIndex].Height div 2 - PanelTools.Height div 2;
    PanelTools.Top := y;
  end;
end;

procedure TFrameViewSwitcherItems.ProcessLabelMouseEnterEvent(Sender: TOBject);
begin
  SetPanelToolPosition(TLabel(Sender).Tag);
end;

procedure TFrameViewSwitcherItems.ProcessLabelMouseLeaveEvent(Sender: TOBject);
begin
  SetPanelToolPosition(-1);
end;

constructor TFrameViewSwitcherItems.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  SetPanelToolPosition(-1);

  // manual translation
  BAdd.Hint := sAddNewSwitcher;
  BEditSwitcher.Hint := SModify;
  BDeleteSwitcher.Hint := SDelete;
end;

procedure TFrameViewSwitcherItems.EraseBackground(DC: HDC);
begin
  //
end;

procedure TFrameViewSwitcherItems.InitFromText(const s: string);
var i: integer;
  A, B: TStringArray;
begin
  if s = '' then exit;
  BCopyPrevious.Visible := False;

  A := s.Split([LineEnding]);
  for i:=0 to High(A) do begin
    B := A[i].Split([':']);
    if Length(B) = 2 then begin
      DoAddSwitcherItem(B[0], B[1]);
      ClientHeight := ClientHeight + FVirtualNames[0].Height;
    end;
  end;

  FOnHeightChange(Self);
end;

function TFrameViewSwitcherItems.ToText: string;
var i: integer;
begin
  Result := '';
  for i:=0 to High(FVirtualNames) do begin
    Result := Result + FVirtualNames[i].Caption+':'+FSubNames[i].Caption;
    if i < High(FVirtualNames) then Result := Result + LineEnding;
  end;
end;

procedure TFrameViewSwitcherItems.SetSwitcherForRange(p: PFixLibSingleRange);
var i: integer;
begin
  if Length(FVirtualNames) = 0 then exit;

  SetLength(p^.SwitchDescriptors, Length(FVirtualNames));
  for i:=0 to High(FVirtualNames) do begin
    p^.SwitchDescriptors[i].SwitchVirtualChannel := FVirtualNames[i].Caption;
    p^.SwitchDescriptors[i].SwitchToSubChannel := FSubNames[i].Caption;
  end;
end;

end.

