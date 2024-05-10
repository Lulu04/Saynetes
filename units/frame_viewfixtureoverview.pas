unit frame_viewfixtureoverview;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, LCLTranslator, ExtCtrls, StdCtrls,
  u_list_dmxuniverse;

type
  // here we show only GENERAL and PHYSICAL data of the fixture

  { TFrameFixtureOverview }

  TFrameFixtureOverview = class(TFrame)
    Image1: TImage;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LBLFixtureName: TLabel;
    LBLFixturePower: TLabel;
    LBLFixturePower1: TLabel;
    LBLFixturePower2: TLabel;
    LBLFixturePower3: TLabel;
    LBLFixturePower4: TLabel;
    LBLFixturePower5: TLabel;
    LBLFixturePower6: TLabel;
    LBLFixturePower7: TLabel;
    LBLManufacturer: TLabel;
    Panel3: TPanel;
    SBLinks: TScrollBox;
  private
    FWebLinks: TWebLinks;
    FWebLabels: array of TLabel;
    procedure ProcessWebLabelClick(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;

    procedure ShowFixture(const aFixtureLocation: TFixtureLibraryLocation);

  end;

implementation

uses u_resource_string, u_helper, u_dmx_util, LCLIntf;

{$R *.lfm}

{ TFrameFixtureOverview }

procedure TFrameFixtureOverview.ProcessWebLabelClick(Sender: TObject);
begin
  try
    OpenURL(FWebLinks[TLabel(Sender).Tag].Url);
  except
  end;
end;

constructor TFrameFixtureOverview.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  // translation
  Label3.Caption := SDimensions+':';
  Label2.Caption := SPower+':';
  Label4.Caption := LowerCase(SWidth)+' x '+LowerCase(SHeight)+' x '+LowerCase(SDepth);
  Label7.Caption := SWeight+':';
  Label10.Caption := SConnector+':';
  Label11.Caption := SBulb+':';
  Label12.Caption := SLumens+':';
  Label13.Caption := SLens+':';
  Label14.Caption := SMinDegree+':';
  Label15.Caption := SMaxDegree+':';
  Label16.Caption := SWebLinks;
  Label17.Caption := SWatt;
  Label18.Caption := SAuthors+':';
end;

procedure TFrameFixtureOverview.ShowFixture(const aFixtureLocation: TFixtureLibraryLocation);
var t: TStringList;
  general: TFixLibGeneral;
  physical: TFixLibPhysical;
  i: Integer;
begin
  if not FileExists(aFixtureLocation.AbsolutPath) then exit;
  FWebLinks := NIL;
  general.InitDefault;
  physical.InitDefault;
  t := TStringList.Create;
  try
    try
      t.LoadFromFile(aFixtureLocation.AbsolutPath);
      FWebLinks.LoadFrom(t);
      general.LoadFrom(t);
      physical.LoadFrom(t);

      LBLManufacturer.Caption := general.ManufacturerName;
      LBLFixtureName.Caption := general.FixtureName;
      LBLFixturePower7.Caption := general.Authors + ' / ' + general.Creator;
      Label5.Caption := physical.Width.ToString+' x '+physical.Height.ToString+' x '+physical.Depth.ToString;
      Label8.Caption := physical.Weight;
      LBLFixturePower.Caption := physical.Power.ToString;
      LBLFixturePower1.Caption := physical.Connector;
      LBLFixturePower2.Caption := physical.Bulb;
      LBLFixturePower3.Caption := physical.Lumens.ToString;
      LBLFixturePower4.Caption := physical.Lens;
      LBLFixturePower5.Caption := FormatFloat('0.0', physical.LensMinDegree);
      LBLFixturePower6.Caption := FormatFloat('0.0', physical.LensMaxDegree);
      ShowFixtureImage(Image1, general.FixtureType);

      // delete the previous web labels
      for i:=0 to High(FWebLabels) do FWebLabels[i].Free;
      FWebLabels := NIL;
      SetLength(FWebLabels, Length(FWebLinks));
      for i:=0 to High(FWebLinks) do begin
        FWebLabels[i] := TLabel.Create(Self);
        FWebLabels[i].Name := 'WebLabel'+i.ToString;
        FWebLabels[i].Parent := SBLinks;
        FWebLabels[i].Left := ScaleDesignToForm(5);
        FWebLabels[i].Top := i*Label16.Height;
        FWebLabels[i].Caption := FWebLinks[i].LinkType;
        FWebLabels[i].Cursor := crHandPoint;
        FWebLabels[i].OnClick := @ProcessWebLabelClick;
        FWebLabels[i].Tag := i;
      end;
    except
    end;
  finally
    t.Free;
  end;
end;

end.

