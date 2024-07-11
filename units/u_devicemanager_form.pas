unit u_devicemanager_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, Grids, Spin, lcl_utils,
  u_dmxdevice_manager, u_list_dmxuniverse, u_common;

type

  { TFormDeviceManager }

  TFormDeviceManager = class(TForm)
    BHelp: TSpeedButton;
    BHelpMonitoring: TSpeedButton;
    BViewInputDMX: TSpeedButton;
    BDMXDevices: TSpeedButton;
    CB2: TCheckBox;
    CB3: TCheckBox;
    CBDeviceList: TComboBox;
    CBDeviceDirection: TComboBox;
    CB1: TCheckBox;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    NB1: TNotebook;
    PageMonitoring: TPage;
    PageDMXDevices: TPage;
    Panel1: TPanel;
    Panel2: TPanel;
    SG2: TStringGrid;
    SpeedButton1: TSpeedButton;
    SG1: TStringGrid;
    BSearch: TSpeedButton;
    SE1: TSpinEdit;
    SG3: TStringGrid;
    Timer1: TTimer;
    procedure BHelpClick(Sender: TObject);
    procedure BSearchClick(Sender: TObject);
    procedure CB1Change(Sender: TObject);
    procedure CBDeviceDirectionSelect(Sender: TObject);
    procedure CBDeviceListSelect(Sender: TObject);
    procedure ComboBox1Select(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure SE1EditingDone(Sender: TObject);
    procedure SG1CheckboxToggled({%H-}sender: TObject; aCol, aRow: Integer; {%H-}aState: TCheckboxState);
    procedure SG1SelectEditor(Sender: TObject; aCol, aRow: Integer; var Editor: TWinControl);
    procedure SG2SelectEditor(Sender: TObject; aCol, aRow: Integer; var Editor: TWinControl);
    procedure SG3GetCellHint(Sender: TObject; ACol, ARow: Integer; var HintText: String);
    procedure SpeedButton1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FNoteBookManager: TNoteBookManager;
    CheckedLabelManager1: TCheckedLabelManager;
    FInitializing: boolean;
    function ValidRow(aRow: integer): boolean;
    function SelectedUniverse: TDMXUniverse;
    procedure FillCBDeviceList;
    procedure FillUniverseGrid;
    procedure FillDevicegrid;

    procedure FillComboBoxInputDevice;
    procedure UpdateInputView;
  public


  end;

var
  FormDeviceManager: TFormDeviceManager;

implementation
uses LCLType, u_project_manager, u_resource_string, u_userdialogs, form_help,
  u_program_options, u_dmx_util;

{$R *.lfm}

{ TFormDeviceManager }

procedure TFormDeviceManager.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

procedure TFormDeviceManager.FormShow(Sender: TObject);
begin
  FInitializing := True;
  CB1.Checked := ProgramOptions.LookForVellemanK8062;
  CB2.Checked := ProgramOptions.LookForEnttecUSBDMXPRO;
  CB3.Checked := ProgramOptions.LookForEnttecOpenDmx;
  FInitializing := False;

  FillDevicegrid;
  FillUniverseGrid;

  FillComboBoxInputDevice;
end;

procedure TFormDeviceManager.SE1EditingDone(Sender: TObject);
var uni: TDmxUniverse;
  dev: TBaseDMXDevice;
begin
  uni := SelectedUniverse;
  if uni = NIl then
    exit;

  dev := DeviceManager.GetDeviceByPath(uni.DevicePath);
  dev.UsedChannelCount[uni.DevicePath.PortIndex] := SE1.Value;
  SG1.Cells[3,SG1.Row] := dev.UsedChannelCount[uni.DevicePath.PortIndex].ToString;
  SE1.Visible := FALSE;
end;

procedure TFormDeviceManager.SG1CheckboxToggled(sender: TObject; aCol,
  aRow: Integer; aState: TCheckboxState);
var uni: TDMXUniverse;
begin
  if aCol <> 3 then exit;
  uni := SelectedUniverse;
  if uni = NIL then exit;
  if not uni.DevicePath.IsAssignedToDevice then exit;

  with uni do
  begin
    OptimizeUsedChannels := not OptimizeUsedChannels;
    if OptimizeUsedChannels then
      SG1.Cells[aCol,aRow] := '1'
    else
      SG1.Cells[aCol,aRow] := '0';
    SG1.Cells[2,aRow] := uni.UsedChannelCount.ToString;
    Project.SetModified;
  end;
end;

procedure TFormDeviceManager.SG1SelectEditor(Sender: TObject; aCol,
  aRow: Integer; var Editor: TWinControl);
var uni: TDmxUniverse;
begin
  if not ValidRow(aRow) then exit;
  uni := SelectedUniverse;
  case aCol of
    1:
      begin
        CBDeviceList.BoundsRect := SG1.CellRect(aCol,aRow);
        CBDeviceList.ItemIndex := uni.DevicePath.DeviceIndex+1;
        Editor := CBDeviceList;
    end;
  end;//case
end;

procedure TFormDeviceManager.SG2SelectEditor(Sender: TObject; aCol,
  aRow: Integer; var Editor: TWinControl);
var udev: TBaseDMXDevice;
    D: TArrayOfDevicePath;
    iport: integer;
begin
  if (aRow < 1) and (aRow > DeviceManager.Count) then exit;
  D := DeviceManager.GetDevicesPath(FALSE);
  iport := D[aRow-1].PortIndex;
  udev := DeviceManager.GetDeviceByPath(D[aRow-1]);
  case aCol of
    1:
      begin
        if not udev.PortDirectionCanChange[iport] then exit;
        CBDeviceDirection.BoundsRect := SG2.CellRect(aCol,aRow);
        CBDeviceDirection.ItemIndex := integer(udev.PortDirection[iport]);
        Editor := CBDeviceDirection;
    end;
  end;//case
end;

procedure TFormDeviceManager.SG3GetCellHint(Sender: TObject; ACol,
  ARow: Integer; var HintText: String);
var adr: integer;
begin
  adr := ARow*32+ACol+1;
  HintText:=SAdress+' '+adr.ToString;
end;

procedure TFormDeviceManager.SpeedButton1Click(Sender: TObject);
begin
  Close;
end;

procedure TFormDeviceManager.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := FALSE;
  UpdateInputView;
  Timer1.Enabled := TRUE;
end;

function TFormDeviceManager.ValidRow(aRow: integer): boolean;
begin
  Result:=(aRow >= 1) and (aRow <= UniverseManager.Count);
end;

function TFormDeviceManager.SelectedUniverse: TDMXUniverse;
begin
  if ValidRow(SG1.Row) then
    Result := UniverseManager.Universes[SG1.Row-1]
  else
    Result := NIL;
end;

procedure TFormDeviceManager.FillCBDeviceList;
var i: integer;
  D: TArrayOfDevicePath;
begin
  CBDeviceList.Clear;
  D := DeviceManager.GetDevicesPath(TRUE);
  for i:=0 to High(D) do
    CBDeviceList.Items.Add(D[i].DeviceNameSerialPort);
end;

procedure TFormDeviceManager.FillUniverseGrid;
var i, idev, iport: integer;
  uni: TDMXUniverse;
  dev: TBaseDMXDevice;
begin
  FInitializing := True;

  // fill the device combobox
  FillCBDeviceList;

  // fill the rows
  SG1.Clean([gzNormal]);
  SG1.RowCount := UniverseManager.Count+1;
  for i:=0 to UniverseManager.Count-1 do
  begin
    SG1.RowHeights[i+1] := 30;
    uni := UniverseManager.Universes[i];
    idev := uni.DevicePath.DeviceIndex;
    iport := uni.DevicePath.PortIndex;
    dev := DeviceManager.Device[idev];

    // universe name
    SG1.Cells[0,i+1] := uni.ShortName+' - '+uni.Name;
    // device name
    SG1.Cells[1,i+1] := uni.DevicePath.DeviceNameSerialPort;
    // channel count
    SG1.Cells[2,i+1] := dev.UsedChannelCount[iport].ToString;
    // optimized
    if uni.OptimizeUsedChannels then
      SG1.Cells[3,i+1] := '1'
    else
      SG1.Cells[3,i+1] := '0';
  end;
  FInitializing := False;
end;

procedure TFormDeviceManager.FillDevicegrid;
var dev: TBaseDMXDevice;
  D: TArrayOfDevicePath;
  i, iport: integer;
begin
  // fill the rows
  SG2.Clean([gzNormal]);
  SG2.RowCount := DeviceManager.PortCount+1;
  D := DeviceManager.GetDevicesPath(False);

  for i:=0 to High(D) do
  begin
    SG2.RowHeights[i+1] := 30;
    dev := DeviceManager.GetDeviceByPath(D[i]);
    iport := D[i].PortIndex;
    // device name
    SG2.Cells[0,i+1] := D[i].DeviceNameSerialPort;
    // direction
    SG2.Cells[1,i+1] := PortDirectionToString(dev.PortDirection[iport]);
    // status
    if dev.PortIsOpen[iport] then
      SG2.Cells[2,i+1] := SOn
    else
      SG2.Cells[2,i+1] := SOff;
  end;
end;

procedure TFormDeviceManager.FillComboBoxInputDevice;
var i: integer;
  D: TArrayOfDevicePath;
  dev: TBaseDMXDevice;
  s: string;
begin
  D := DeviceManager.GetDevicesPath(FALSE);
  ComboBox1.Clear;
  for i:=0 to High(D) do
  begin
    dev := DeviceManager.GetDeviceByPath(D[i]);
    if dev.PortDirection[D[i].PortIndex] = pdIn then
      s := 'INPUT'
    else
      s := 'OUTPUT';
    ComboBox1.Items.Add(D[i].DeviceNameSerialPort+' '+s);
  end;
end;

procedure TFormDeviceManager.UpdateInputView;
var dev: TBaseDMXDevice;
  p: PByte;
  r, c, i: Integer;
  D: TArrayOfDevicePath;
begin
  SG3.BeginUpdate;
  if ComboBox1.ItemIndex=-1 then
  begin
    for r:=0 to SG3.RowCount-1 do
      for c:=0 to SG3.ColCount-1 do
        SG3.Cells[c,r] := '';
  end
  else
  begin
    i := ComboBox1.ItemIndex;
    D := DeviceManager.GetDevicesPath(FALSE);
    dev := DeviceManager.GetDeviceByPath(D[i]);
    p := PByte(dev.DmxBuffer[D[i].PortIndex]);
    for r:=0 to SG3.RowCount-1 do
      for c:=0 to SG3.ColCount-1 do
      begin
        if p <> NIL then
        begin
          SG3.Cells[c,r] := p^.ToString;
          inc(p);
        end
        else SG3.Cells[c,r] := '';
      end;
  end;
  SG3.EndUpdate(TRUE);
end;

procedure TFormDeviceManager.FormCreate(Sender: TObject);
begin
  FNoteBookManager := TNoteBookManager.Create(NB1);
  FNoteBookManager.LinkButtonToPage(BDMXDevices, PageDMXDevices);
  FNoteBookManager.LinkButtonToPage(BViewInputDMX, PageMonitoring);
  FNoteBookManager.ActivePage(PageDMXDevices);

  CheckedLabelManager1 := TCheckedLabelManager.Create;
  CheckedLabelManager1.CaptureLabelClick(Label5);
  CheckedLabelManager1.CaptureLabelClick(Label6);
  CheckedLabelManager1.CaptureLabelClick(Label7);

  CBDeviceDirection.Items.Add(SDevicePortIn);
  CBDeviceDirection.Items.Add(SDevicePortOut);
end;

procedure TFormDeviceManager.CBDeviceListSelect(Sender: TObject);
var uni: TDMXUniverse;
  i: integer;
  idev, iport: integer;
  D: TArrayOfDevicePath;
begin
  i := CBDeviceList.ItemIndex;
  if i = -1 then exit;
  D := DeviceManager.GetDevicesPath(TRUE);

  uni := UniverseManager.Universes[SG1.Row-1];
  idev := D[i].DeviceIndex;
  iport := D[i].PortIndex;

  // same choice -> do nothing
  if (uni.DevicePath.DeviceIndex = idev) and
     (uni.DevicePath.PortIndex = iport) then
    exit;

  // reset device path on other universe with the selected device
  for i:=0 to UniverseManager.Count-1 do
    with UniverseManager.Universes[i] do
      if (DevicePath.DeviceIndex = idev) and
         (DevicePath.PortIndex = iport) then
      begin
        DevicePath.InitByDefault;
        OptimizeUsedChannels := FALSE;
        SG1.Cells[1,i+1] := DeviceManager.Device[INVALID_DMXDEVICE_INDEX].Name;
        // update on view: channels used + optimized
        SG1.Cells[2,i+1] := UsedChannelCount.ToString;
        SG1.Cells[3,i+1] := '0';
      end;

  // then connect the target universe with the selected device
  uni.DevicePath.DeviceIndex := idev;
  uni.DevicePath.PortIndex := iport;
  SG1.Cells[SG1.Col,SG1.Row] := CBDeviceList.Text;

  // update on view: channels used + optimized
  SG1.Cells[2,SG1.Row] := uni.UsedChannelCount.ToString;
  if uni.OptimizeUsedChannels then begin
    SG1.Cells[3,SG1.Row] := '1'
  end else begin
    SG1.Cells[3,SG1.Row] := '0';
  end;

  Project.SetModified;
end;

procedure TFormDeviceManager.ComboBox1Select(Sender: TObject);
begin
  UpdateInputView;
end;

procedure TFormDeviceManager.CBDeviceDirectionSelect(Sender: TObject);
var i: integer;
  dev: TBaseDMXDevice;
  D: TArrayOfDevicePath;
  selectedPath: TDevicePath;
begin
  if CBDeviceDirection.ItemIndex = -1 then exit;
  if (SG2.Row < 1) or (SG2.Row > DeviceManager.PortCount) then exit;

  D := DeviceManager.GetDevicesPath(FALSE);
  selectedPath := D[SG2.Row-1];
  dev := DeviceManager.GetDeviceByPath(selectedPath);
  dev.PortDirection[selectedPath.PortIndex] := TPortDirection(CBDeviceDirection.ItemIndex);
  SG2.Cells[1,SG2.Row] := PortDirectionToString(dev.PortDirection[selectedPath.PortIndex]);
  // dissociate universe from device configured as input
  if dev.PortDirection[selectedPath.PortIndex] = pdIn then
  begin
    for i:=0 to UniverseManager.Count-1 do
      with UniverseManager.Universes[i] do
        if (DevicePath.DeviceIndex = selectedPath.DeviceIndex) and
           (DevicePath.PortIndex = selectedPath.PortIndex) then
           begin
             DevicePath.InitByDefault;
             OptimizeUsedChannels := FALSE;
           end;

    FillUniverseGrid;
  end;
end;

procedure TFormDeviceManager.BSearchClick(Sender: TObject);
var i: integer;
  txt: string;
begin
  // dissociates all universe from their device
  for i:=0 to UniverseManager.Count-1 do
    UniverseManager.Universes[i].DevicePath.InitByDefault;

  DeviceManager.LookForAvailableDevices;

  if DeviceManager.Count = 0 then
    ShowMess(SNoDeviceFound, SOk, mtWarning)
  else begin
    txt := SFound;
    if DeviceManager.Count > 1 then
      txt := txt+' '+DeviceManager.Count.ToString+' '+SDevices;
    for i:=0 to DeviceManager.Count-1 do
    begin
      txt := txt+LINEENDING+DeviceManager.Device[i].Name;
      if DeviceManager.Device[i].SerialNumber <> '' then
        txt := txt+' - '+DeviceManager.Device[i].SerialNumber;
    end;
    ShowMess(txt, SOk, mtInformation);
  end;
  FillUniverseGrid;
  FillDevicegrid;
end;

procedure TFormDeviceManager.CB1Change(Sender: TObject);
begin
  if FInitializing then exit;
  if Sender = CB1 then ProgramOptions.LookForVellemanK8062 := CB1.Checked;
  if Sender = CB2 then ProgramOptions.LookForEnttecUSBDMXPRO := CB2.Checked;
  if Sender = CB3 then ProgramOptions.LookForEnttecOpenDmx := CB3.Checked;
end;

procedure TFormDeviceManager.BHelpClick(Sender: TObject);
begin
  if Sender = BHelp then _ShowHelp(HelpDeviceManagerDevice, BHelp);
  if Sender = BHelpMonitoring then _ShowHelp(HelpDeviceManagerMonitoring, BHelpMonitoring);
end;

procedure TFormDeviceManager.FormDestroy(Sender: TObject);
begin
  FNoteBookManager.Free;
  CheckedLabelManager1.free;
end;

end.

