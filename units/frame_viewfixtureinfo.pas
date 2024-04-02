unit frame_viewfixtureinfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, LCLType, ExtCtrls, StdCtrls, Spin,
  Buttons, frame_editstring, u_list_dmxuniverse,
  frame_viewdmxdipswitchs;

type

  { TFrameFixtureInfo }

  TFrameFixtureInfo = class(TFrame)
    CBUni: TComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    SE1: TSpinEdit;
    procedure SE1Change(Sender: TObject);
  private
    FOriginalFullHeight: integer;
    FrameEditString1: TFrameEditString;
    FrameViewDipSwitchs1: TFrameViewDipSwitchs;
    FTargetFixture: TDMXFixture;
    FUpdatingView: boolean;
    procedure ProcessDescriptionChange(Sender: TObject);
  public
    FTargetViewProjector: TFrame;
    constructor Create(TheOwner: TComponent); override;

    procedure UpdateView;

    function ViewHeight: integer;
  end;

implementation

uses u_project_manager, u_resource_string, u_common, Graphics,
  frame_viewprojectors;

{$R *.lfm}

{ TFrameFixtureInfo }

procedure TFrameFixtureInfo.SE1Change(Sender: TObject);
begin
  if FTargetFixture = NIL then exit;
  if FUpdatingView then exit;

  FTargetFixture.Adress := SE1.Value;
  Edit1.Text := FTargetFixture.LastAdress.ToString;
  Project.SetModified;
  UpdateView;
end;

procedure TFrameFixtureInfo.ProcessDescriptionChange(Sender: TObject);
begin
  if FUpdatingView then exit;

  if (FTargetFixture <> NIL) and
     (FrameEditString1.TextIsValid) then
  begin
    if FrameEditString1.Text <> FTargetFixture.Description then
    begin
      FTargetFixture.Description := FrameEditString1.Text;
      Project.SetModified;
      TFrameViewProjector(FTargetViewProjector).Redraw;
      TFrameViewProjector(FTargetViewProjector).FrameViewDMXCursors1.RedrawVisibleFixtures;
    end;
  end;
end;

constructor TFrameFixtureInfo.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FrameEditString1 := TFrameEditString.Create(Self);
  FrameEditString1.Parent := Panel2;
  FrameEditString1.Align := alClient;
  FrameEditString1.Title := SDescription;
  FrameEditString1.FontStyles := [];
  FrameEditString1.FontHeight := 0;
  FrameEditString1.ModeNoSpecialChar;
  FrameEditString1.AllowEmptyString;
  FrameEditString1.OnTextChange := @ProcessDescriptionChange;

  FrameViewDipSwitchs1 := TFrameViewDipSwitchs.Create(Self);
  FrameViewDipSwitchs1.Parent := Panel3;
  FrameViewDipSwitchs1.Align := alClient;
  FrameViewDipSwitchs1.Editable := FALSE;

  FOriginalFullHeight := -1;
end;

procedure TFrameFixtureInfo.UpdateView;
var i: integer;
begin
  FUpdatingView := True;

  // Enable edition for guiPrepaDMX  mode
  FrameEditString1.ReadOnly := False;//FormViewProjector.FrameViewProjector1.GUIMode <> guiPrepaDMX;// not Project.ProjectPrefs.EditMode;
  CBUni.Enabled := TFrameViewProjector(FTargetViewProjector).GUIMode = guiPrepaDMX;
  SE1.Enabled := TFrameViewProjector(FTargetViewProjector).GUIMode = guiPrepaDMX;

  // update caption language
  Label2.Caption := SUniverse;
  Label3.Caption := SAdress;
  Label4.Caption := STo;
  Label5.Caption := SChannel;

  // update universe combobox
  CBUni.Clear;
  for i:=0 to UniverseManager.Count-1 do
    CBUni.Items.Add(UniverseManager.Universes[i].Name);

  FTargetFixture := TFrameViewProjector(FTargetViewProjector).Selected[0];
  Label1.Caption := FTargetFixture.Name;
  FrameEditString1.Text := FTargetFixture.Description;
  CBUni.ItemIndex := UniverseManager.IndexOf(FTargetFixture.Universe);
  SE1.Value := FTargetFixture.Adress;
  Edit1.Text := FTargetFixture.LastAdress.ToString;
  Edit2.Text := FTargetFixture.ChannelsCount.ToString;

  if FTargetFixture.HaveAdressDipSwitch then
    FrameViewDipSwitchs1.ShowAdress(FTargetFixture.Adress, FTargetFixture.DipSwitchs);

  FUpdatingView := False;
end;

function TFrameFixtureInfo.ViewHeight: integer;
begin
  if FOriginalFullHeight = -1 then
    FOriginalFullHeight := Self.Height;

  if TFrameViewProjector(FTargetViewProjector).Selected[0].HaveAdressDipSwitch then
    Result := FOriginalFullHeight//Self.Height//Panel1.Height
  else
    Result := SE1.Top+SE1.Height+2;
end;

end.

