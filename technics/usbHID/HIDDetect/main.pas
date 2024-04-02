unit Main;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls
  {$ifdef Unix}
  ,usbcontroller;
  {$else}
  ,JvHidControllerClass;
  {$endif}


type

  { TForm1 }

  TForm1 = class(TForm)
    btnHIDCreate: TButton;
    btnHIDEnable: TButton;
    btnInfo: TButton;
    Memo1: TMemo;
    procedure btnHIDCreateClick(Sender: TObject);
    procedure btnHIDEnableClick(Sender: TObject);
    procedure btnInfoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FUSBHID: TJvHidDeviceController;
    FDeviceList: TList;
    FK8062Count: integer;
    procedure ClearDeviceList;
    procedure ProcessHIDCtrlChange(Sender: TObject);
    function ProcessHIDCtrlEnumerate(HidDev: TJvHidDevice; const Index: LongInt): Boolean;

  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.btnHIDCreateClick(Sender: TObject);
begin
  TButton(Sender).Enabled:=False;
  Memo1.Lines.Append('HID Created.');
  FUSBHID:=TJvHidDeviceController.Create(NIL);
  FUSBHID.OnDeviceChange:=@ProcessHIDCtrlChange;
  FUSBHID.OnEnumerate:=@ProcessHIDCtrlEnumerate;

  btnHIDEnable.Enabled:=True;
end;

procedure TForm1.btnHIDEnableClick(Sender: TObject);
begin
  TButton(Sender).Enabled:=False;
  Memo1.Lines.Append('HID Enabled.');
  FUSBHID.Enabled:=TRUE;
  btnInfo.Enabled:=True;
end;

procedure TForm1.btnInfoClick(Sender: TObject);
var S:string;
begin
  ClearDeviceList;
  Memo1.Clear;
  FK8062Count:=FUSBHID.Enumerate;
  S:='8062 count = '+FUSBHID.CountByID($10CF, $8062).ToString;
  Memo1.Lines.Append(S);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FDeviceList:=TList.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  ClearDeviceList;
  FDeviceList.Free;
  if Assigned(FUSBHID)
    then FUSBHID.Free;
end;

procedure TForm1.ClearDeviceList;
begin
  while FDeviceList.Count>0 do begin
    TJvHidDevice(FDeviceList.Items[0]).Free;
    FDeviceList.Delete(0);
  end;
  FK8062Count:=0;
end;

procedure TForm1.ProcessHIDCtrlChange(Sender: TObject);
begin
  ClearDeviceList;
  Memo1.Clear;
  FK8062Count:=FUSBHID.Enumerate;
end;

function TForm1.ProcessHIDCtrlEnumerate(HidDev: TJvHidDevice; const Index: LongInt): Boolean;
var
  Dev: TJvHidDevice;
begin
  if (HidDev.Attributes.VendorID=$10CF) and (HidDev.Attributes.ProductID=$8062) then begin
      Memo1.Lines.Add(Format('Device VID=%x PID=%x VersionNumber=%x',
        [HidDev.Attributes.VendorID,
         HidDev.Attributes.ProductID,
         HidDev.Attributes.VersionNumber]));
      FUSBHID.CheckOutByIndex(Dev, Index);
      FDeviceList.Add(Dev);
      Memo1.Lines.Add(Dev.VendorName+' - '+Dev.ProductName+' - '+Dev.SerialNumber);
  end;
  Result := True;
end;

end.

