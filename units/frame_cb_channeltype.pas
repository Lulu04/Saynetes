unit frame_cb_channeltype;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls,
  u_common, Types;

type

  { TFrameCBChannelType }

  TFrameCBChannelType = class(TFrame)
    CB: TComboBox;
    procedure CBChange(Sender: TObject);
    procedure CBDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
  private
    FOnChange: TNotifyEvent;
    FInitializing: boolean;
    function GetItemIndex: integer;
    function GetSelectedType: TChannelType;
    procedure SetItemIndex(AValue: integer);
    procedure SetSelectedType(AValue: TChannelType);
    procedure SplitItemData(aIndex: integer; out aChanType: TChannelType; out aReadable, aExtra: string);
  public
    constructor Create(TheOwner: TComponent); override;

    procedure FillForPresetChannel;
    procedure FillWithMinimalChannelTypes;

    procedure GetData(out aChanType: TChannelType; out aReadable, aExtra: string);

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property ItemIndex: integer read GetItemIndex write SetItemIndex;
    property SelectedType: TChannelType read GetSelectedType write SetSelectedType;
  end;

implementation

uses u_datamodule, GraphType, LCLType, Graphics;

{$R *.lfm}

{ TFrameCBChannelType }

procedure TFrameCBChannelType.CBDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var chanType: TChannelType;
  readable, extra: string;
  c: TColor;
begin
  SplitItemData(Index, chanType, readable, extra);

  if State >= [odSelected] then CB.Canvas.Brush.Color := clHighLight
    else CB.Canvas.Brush.Color := CB.Color;
  CB.Canvas.Brush.Style := bsSolid;
  CB.Canvas.FillRect(ARect);

  DataModule1.ILChannelType.Draw(CB.Canvas, ARect.Left+ScaleDesignToForm(3), ARect.Top, Ord(chanType));
  CB.Canvas.Brush.Style := bsClear;
  CB.Canvas.TextOut(ARect.Left + DataModule1.ILChannelType.Width + ScaleDesignToForm(8),
                    ARect.Top, readable+extra);
end;

procedure TFrameCBChannelType.CBChange(Sender: TObject);
begin
  if FInitializing then exit;
  if FOnChange <> NIL then FOnChange(Self);
end;

function TFrameCBChannelType.GetItemIndex: integer;
begin
  Result := CB.ItemIndex;
end;

function TFrameCBChannelType.GetSelectedType: TChannelType;
var chanType: TChannelType;
  tex, extra: string;
begin
  if CB.ItemIndex = -1 then exit(ctNOFUNCTION);

  SplitItemData(CB.ItemIndex, chanType, tex, extra);
  Result := chantype;
end;

procedure TFrameCBChannelType.SetItemIndex(AValue: integer);
begin
  CB.ItemIndex := AValue;
end;

procedure TFrameCBChannelType.SetSelectedType(AValue: TChannelType);
var i: integer;
  chanType: TChannelType;
  tex, extra: string;
begin
  for i:=0 to CB.Items.Count-1 do begin
    SplitItemData(i, chanType, tex, extra);
    if chanType = AValue then begin
      CB.ItemIndex := i;
      exit;
    end;
  end;

end;

procedure TFrameCBChannelType.SplitItemData(aIndex: integer; out
  aChanType: TChannelType; out aReadable, aExtra: string);
var A: TStringArray;
begin
  aReadable := '';
  aExtra := '';
  aChanType := ctNOFUNCTION;

  if aIndex > -1 then begin
    A := CB.Items.Strings[aIndex].Split(['|']);
    aChanType := TChannelType(A[0].ToInteger);
    if Length(A) >= 2 then aReadable := A[1];
    if Length(A) = 3 then aExtra := A[2];
  end;
end;

constructor TFrameCBChannelType.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  CB.ItemHeight := DataModule1.ILChannelType.Height+ScaleDesignToForm(3);
end;

procedure TFrameCBChannelType.FillForPresetChannel;
begin
  FInitializing := True;
  CB.Clear;
  CB.Items.Add(Ord(ctMASTERDIMMER).ToString+'|Master dimmer|0..100%');
  CB.Items.Add(Ord(ctMASTERDIMMER).ToString+'|Master dimmer fine');
  CB.Items.Add(Ord(ctDIMMER).ToString+'|Dimmer|0..100%');
  CB.Items.Add(Ord(ctDIMMER).ToString+'|Dimmer fine');

  CB.Items.Add(Ord(ctRED).ToString+'|Red|0..100%');
  CB.Items.Add(Ord(ctRED).ToString+'|Red fine');
  CB.Items.Add(Ord(ctGREEN).ToString+'|Green|0..100%');
  CB.Items.Add(Ord(ctGREEN).ToString+'|Green fine');
  CB.Items.Add(Ord(ctBLUE).ToString+'|Blue|0..100%');
  CB.Items.Add(Ord(ctBLUE).ToString+'|Blue fine');
  CB.Items.Add(Ord(ctWHITE).ToString+'|White|0..100%');
  CB.Items.Add(Ord(ctWHITE).ToString+'|White fine');
  CB.Items.Add(Ord(ctAMBER).ToString+'|Amber|0..100%');
  CB.Items.Add(Ord(ctAMBER).ToString+'|Amber fine');
  CB.Items.Add(Ord(ctUV).ToString+'|UV|0..100%');
  CB.Items.Add(Ord(ctUV).ToString+'|UV fine');
  CB.Items.Add(Ord(ctCYAN).ToString+'|Cyan|0..100%');
  CB.Items.Add(Ord(ctCYAN).ToString+'|Cyan fine');
  CB.Items.Add(Ord(ctMAGENTA).ToString+'|Magenta|0..100%');
  CB.Items.Add(Ord(ctMAGENTA).ToString+'|Magenta fine');
  CB.Items.Add(Ord(ctYELLOW).ToString+'|Yellow|0..100%');
  CB.Items.Add(Ord(ctYELLOW).ToString+'|Yellow fine');
  CB.Items.Add(Ord(ctLIME).ToString+'|Lime|0..100%');
  CB.Items.Add(Ord(ctLIME).ToString+'|Lime fine');
  CB.Items.Add(Ord(ctINDIGO).ToString+'|Indigo|0..100%');
  CB.Items.Add(Ord(ctINDIGO).ToString+'|Indigo fine');
  CB.Items.Add(Ord(ctWARMWHITE).ToString+'|Warm white|0..100%');
  CB.Items.Add(Ord(ctWARMWHITE).ToString+'|Warm white fine');
  CB.Items.Add(Ord(ctCOLDWHITE).ToString+'|Cold white|0..100%');
  CB.Items.Add(Ord(ctCOLDWHITE).ToString+'|Cold white fine');

  CB.Items.Add(Ord(ctCOLORTEMPERATURE).ToString+'|Color temperature');

  CB.Items.Add(Ord(ctDIMMER).ToString+'|Hue|0..100%');
  CB.Items.Add(Ord(ctDIMMER).ToString+'|Hue fine');
  CB.Items.Add(Ord(ctDIMMER).ToString+'|Saturation|0..100%');
  CB.Items.Add(Ord(ctDIMMER).ToString+'|Saturation fine');
  CB.Items.Add(Ord(ctDIMMER).ToString+'|Lightness|0..100%');
  CB.Items.Add(Ord(ctDIMMER).ToString+'|Lightness fine');
  CB.Items.Add(Ord(ctDIMMER).ToString+'|Value|0..100%');
  CB.Items.Add(Ord(ctDIMMER).ToString+'|Value fine');

  CB.Items.Add(Ord(ctCOLORCHOICE).ToString+'|Color macro');
  CB.Items.Add(Ord(ctCOLORCHOICE).ToString+'|Color wheel');
  CB.Items.Add(Ord(ctCOLORCHOICE).ToString+'|Color wheel fine');
  CB.Items.Add(Ord(ctCOLORCHOICE).ToString+'|RGB mixer');
  CB.Items.Add(Ord(ctCOLORCHOICE).ToString+'|CTO mixer');
  CB.Items.Add(Ord(ctCOLORCHOICE).ToString+'|CTB mixer');
  CB.Items.Add(Ord(ctCOLORCHOICE).ToString+'|CTC mixer');

  CB.Items.Add(Ord(ctCONFIG).ToString+'|Maintenance');
  CB.Items.Add(Ord(ctSTROBE).ToString+'|Strobe');
  CB.Items.Add(Ord(ctSTROBESPEED).ToString+'|Strobe speed');

  CB.Items.Add(Ord(ctPAN).ToString+'|Pan');
  CB.Items.Add(Ord(ctPAN).ToString+'|Pan fine');
  CB.Items.Add(Ord(ctTILT).ToString+'|Tilt');
  CB.Items.Add(Ord(ctTILT).ToString+'|Tilt fine');
  CB.Items.Add(Ord(ctPAN).ToString+'|X axis');
  CB.Items.Add(Ord(ctTILT).ToString+'|Y axis');
  CB.Items.Add(Ord(ctPANSPEED).ToString+'|Pan speed|Slow..Fast');
  CB.Items.Add(Ord(ctPANSPEED).ToString+'|Pan speed|Fast..Slow');
  CB.Items.Add(Ord(ctTILTSPEED).ToString+'|Tilt speed|Slow..Fast');
  CB.Items.Add(Ord(ctTILTSPEED).ToString+'|Tilt speed|Fast..Slow');
  CB.Items.Add(Ord(ctPANTILTSPEED).ToString+'|Pan/Tilt speed|Slow..Fast');
  CB.Items.Add(Ord(ctPANTILTSPEED).ToString+'|Pan/Tilt speed|Fast..Slow');
  CB.Items.Add(Ord(ctPANCONTINUOUS).ToString+'|Pan continuous');
  CB.Items.Add(Ord(ctTILTCONTINUOUS).ToString+'|Tilt continuous');


  CB.Items.Add(Ord(ctPANTILT).ToString+'|Movement macros');
  CB.Items.Add(Ord(ctPANTILTSPEED).ToString+'|Movement macros speed|Slow..Fast');
  CB.Items.Add(Ord(ctPANTILTSPEED).ToString+'|Movement macros speed|Fast..Slow');

  CB.Items.Add(Ord(ctGOBO).ToString+'|Gobo');
  CB.Items.Add(Ord(ctGOBOROTATION).ToString+'|Gobo rotation');
  CB.Items.Add(Ord(ctIRIS).ToString+'|Iris');
  CB.Items.Add(Ord(ctBLADEINSERTION).ToString+'|Blade insertion');
  CB.Items.Add(Ord(ctBLADEROTATION).ToString+'|Blade rotation');
  CB.Items.Add(Ord(ctZOOM).ToString+'|Zoom');
  CB.Items.Add(Ord(ctFOCUS).ToString+'|Focus');
  CB.Items.Add(Ord(ctSOUNDSENSITIVITY).ToString+'|Sound sensitivity');

  CB.Items.Add(Ord(ctROTATION).ToString+'|Rotation');
  CB.Items.Add(Ord(ctSPEED).ToString+'|Speed');

  CB.Items.Add(Ord(ctSMOKE).ToString+'|Smoke');
  CB.Items.Add(Ord(ctFAN).ToString+'|Fan');

  CB.Items.Add(Ord(ctNOFUNCTION).ToString+'|No function');

  FInitializing := False;
end;

procedure TFrameCBChannelType.FillWithMinimalChannelTypes;
begin
  FInitializing := True;
  CB.Clear;
  CB.Items.Add(Ord(ctMASTERDIMMER).ToString);
  CB.Items.Add(Ord(ctDIMMER).ToString);

  CB.Items.Add(Ord(ctRED).ToString);
  CB.Items.Add(Ord(ctGREEN).ToString);
  CB.Items.Add(Ord(ctBLUE).ToString);
  CB.Items.Add(Ord(ctWHITE).ToString);
  CB.Items.Add(Ord(ctAMBER).ToString);
  CB.Items.Add(Ord(ctUV).ToString);
  CB.Items.Add(Ord(ctCYAN).ToString);
  CB.Items.Add(Ord(ctMAGENTA).ToString);
  CB.Items.Add(Ord(ctYELLOW).ToString);
  CB.Items.Add(Ord(ctLIME).ToString);
  CB.Items.Add(Ord(ctINDIGO).ToString);
  CB.Items.Add(Ord(ctWARMWHITE).ToString);
  CB.Items.Add(Ord(ctCOLDWHITE).ToString);

  CB.Items.Add(Ord(ctCOLORTEMPERATURE).ToString);

  CB.Items.Add(Ord(ctCOLORCHOICE).ToString);

  CB.Items.Add(Ord(ctCONFIG).ToString);
  CB.Items.Add(Ord(ctSTROBE).ToString);
  CB.Items.Add(Ord(ctSTROBESPEED).ToString);

  CB.Items.Add(Ord(ctPAN).ToString);
  CB.Items.Add(Ord(ctTILT).ToString);
  CB.Items.Add(Ord(ctPANTILT).ToString);
  CB.Items.Add(Ord(ctPANTILTSPEED).ToString);
  CB.Items.Add(Ord(ctPANCONTINUOUS).ToString);
  CB.Items.Add(Ord(ctTILTCONTINUOUS).ToString);

  CB.Items.Add(Ord(ctGOBO).ToString);
  CB.Items.Add(Ord(ctGOBOROTATION).ToString);
  CB.Items.Add(Ord(ctIRIS).ToString);
  CB.Items.Add(Ord(ctBLADEINSERTION).ToString);
  CB.Items.Add(Ord(ctBLADEROTATION).ToString);
  CB.Items.Add(Ord(ctZOOM).ToString);
  CB.Items.Add(Ord(ctFOCUS).ToString);
  CB.Items.Add(Ord(ctSOUNDSENSITIVITY).ToString);

  CB.Items.Add(Ord(ctROTATION).ToString);
  CB.Items.Add(Ord(ctSPEED).ToString);

  CB.Items.Add(Ord(ctSMOKE).ToString);
  CB.Items.Add(Ord(ctFAN).ToString);

  CB.Items.Add(Ord(ctNOFUNCTION).ToString);

  FInitializing := False;
end;

procedure TFrameCBChannelType.GetData(out aChanType: TChannelType; out aReadable, aExtra: string);
begin
  SplitItemData(CB.ItemIndex, aChanType, aReadable, aExtra);
end;

end.

