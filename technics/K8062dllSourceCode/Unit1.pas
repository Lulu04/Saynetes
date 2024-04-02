unit Unit1;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms,
  Math, StdCtrls,Dialogs,
  HidControllerClass, ExtCtrls, Fasttime32, JvHidControllerClass;

type
  TForm1u = class(TForm)
    HidCtl: TJvHidDeviceController;
    FastTimer1: TFastTimer;
    ListBox1: TListBox;
    procedure HidCtlDeviceChange(Sender: TObject);
    function HidCtlEnumerate(const HidDev: TJvHidDevice;
      const Index: Integer): Boolean;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FastTimer1Timer(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    function GetShareData(Address:Longint):Longint ;
    procedure SetShareData (Address, Data:Longint);

  private
  public
    DevList: TList;
    procedure DoRead;
    procedure DoWrite;
  end;

var
  Form1: TForm1u;
  TheDev: TJvHidDevice;
  timed,viewed,device_found,u:boolean;
  Buf_tx: array [0..64] of Byte;
  Buf_rx: array [0..64] of Byte;
  ShareData: ^longint; // shared
  hMapFile: THandle;
  Gain: array [1..4] of longint;
  LEDon: byte;

const
  VirtualFileName = 'ShareK8062Data';
  DataSize = 2500 {size of data area};
  
implementation

//uses Unit2;

{$R *.DFM}

procedure TForm1u.HidCtlDeviceChange(Sender: TObject);
var
  I: Integer;
begin
  if Assigned(ListBox1) then
  begin
    if DevList <> nil then
    begin
      for I := 0 to DevList.Count-1 do
      begin
        TheDev := DevList.Items[I];
        TheDev.Free;
      end;
      DevList.Clear;
    end
    else
      DevList := TList.Create;
    ListBox1.Clear;
    HidCtl.Enumerate;
    if ListBox1.Items.Count > 0 then
    begin
      ListBox1.ItemIndex := 0;
      TheDev := DevList.Items[ListBox1.ItemIndex];
    end;
  end;
end;

function TForm1u.HidCtlEnumerate(const HidDev: TJvHidDevice;
  const Index: Integer): Boolean;
var
  Dev: TJvHidDevice;
begin
  if Assigned(ListBox1) then
  begin
    if (HidDev.Attributes.VendorID=$10CF) and (HidDev.Attributes.ProductID=$8062)
    then
    begin
      ListBox1.Items.Add(Format('Device VID=%x PID=%x',
        [HidDev.Attributes.VendorID,
         HidDev.Attributes.ProductID]));
      HidCtl.CheckOutByIndex(Dev, Index);
      DevList.Add(Dev);
    end;
  end;
  Result := True;
end;

procedure TForm1u.FormDestroy(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to DevList.Count-1 do
  begin
    TheDev := DevList.Items[I];
    HidCtl.CheckIn(TheDev);
  end;
  DevList.Free;
  UnmapViewOfFile (ShareData);
  CloseHandle (hMapFile);
end;

procedure TForm1u.DoRead;
var
  ToRead: Cardinal;
  Read: Cardinal;   
begin
  if (ListBox1.Items.Count > 0) then
  begin
    ToRead := TheDev.Caps.InputReportByteLength;
    TheDev.ReadFile(Buf_rx, ToRead, Read);
  end;
end;

procedure TForm1u.DoWrite;
var
  Written: Cardinal;
  ToWrite: Cardinal;     
begin
  if (ListBox1.Items.Count > 0) then
  begin
    Buf_tx[0] := 0;
    ToWrite := 9;
    //Buf_tx[1]:=5;
    TheDev := DevList.Items[0];
    Written := 0;
    TheDev.WriteFile(Buf_tx, ToWrite, Written);
  end;
end;


procedure TForm1u.FormCreate(Sender: TObject);
var i:integer;
begin
  device_found:=false;
  //create memory mapped file
  hMapFile := CreateFileMapping ($FFFFFFFF, nil,
    Page_ReadWrite, 0, DataSize, VirtualFileName);
  if hMapFile = 0 then
    raise Exception.Create ('Error creating memory mapped file');
  // get the pointer to the actual data
  ShareData := MapViewOfFile (
    hMapFile, File_Map_Write, 0, 0, DataSize);
  timed:=false;
  viewed:=false;
  LEDon:=0;
  FastTimer1.enabled:=true;
  for i:=0 to 10 do Buf_tx[i]:=0;
  for i:=0 to 10 do Buf_rx[i]:=0;
  for i:=1 to 4 do gain[i]:=1;
end;



procedure TForm1u.SetShareData (address,data: Longint);
var p:pointer;
ret:longint;
begin
  p:=ShareData;
  asm
    push  edi
    mov   edi,p
    add   edi,address
    add   edi,address
    add   edi,address
    add   edi,address
    mov   ret,edi
    mov   eax,data
    mov   ds:[edi],eax
    pop   edi
  end;
end;

function TForm1u.GetShareData (Address:Longint): Longint;
var p:pointer;
k:longint;
begin
  p:= ShareData;
  asm
    push  edi
    mov   edi,p
    add   edi,address
    add   edi,address
    add   edi,address
    add   edi,address
    mov   eax,ds:[edi]
    mov   k,eax
    pop   edi
  end;   
  Result:=k;
end;

procedure TForm1u.FastTimer1Timer(Sender: TObject);
var i,j,max_ch,n:integer;
begin
  FastTimer1.enabled:=false;
  //Form1.visible:=true;
  if not timed then
  begin
    if GetShareData(1)<>222 then
    begin
      Form1.visible:=false;
      FastTimer1.enabled:=false;
      Application.MessageBox('This is the I/O routine for K8062 USB communication.' +
      chr(13)+'This is not a stand-alone program.', 'K8062E.EXE', mb_OK );
      close;
    end;  
    if GetShareData(0)=123 then close;
    SetShareData (0,123);
    for i:=2 to 523 do SetShareData(i,0);
    timed:=true;
    form1.hide;
  end;
   if GetShareData(0)=333 then close;
   max_ch:=GetShareData(2);
   if max_ch<8 then max_ch:=8;
   if max_ch>512 then max_ch:=512;
   i:=0;
   repeat
     inc(i);
   until (GetShareData(i+10)>0) or (i=100) or (i=max_ch-5);
   Buf_tx[1]:=4;    //send start code + 6 bytes of data
   Buf_tx[2]:=i;    //number of zeroes (incl. start code = 0)
   Buf_tx[3]:=GetShareData(i+10);
   Buf_tx[4]:=GetShareData(i+11);
   Buf_tx[5]:=GetShareData(i+12);
   Buf_tx[6]:=GetShareData(i+13);
   Buf_tx[7]:=GetShareData(i+14);
   Buf_tx[8]:=GetShareData(i+15);
   DoWrite;
   i:=i+6;
   repeat
     if max_ch-i<6 then
     begin
       Buf_tx[1]:=3;    //send one byte of data
       Buf_tx[2]:=GetShareData(i+10);
       DoWrite;
       inc(i);
     end;
     if (max_ch-i>=6) and (GetShareData(i+10)>0) then
     begin
       Buf_tx[1]:=2;    //send 7 bytes of data
       Buf_tx[2]:=GetShareData(i+10);
       Buf_tx[3]:=GetShareData(i+11);
       Buf_tx[4]:=GetShareData(i+12);
       Buf_tx[5]:=GetShareData(i+13);
       Buf_tx[6]:=GetShareData(i+14);
       Buf_tx[7]:=GetShareData(i+15);
       Buf_tx[8]:=GetShareData(i+16);
       DoWrite;
       i:=i+7;
     end;
     if (max_ch-i>=6) and (GetShareData(i+10)=0) then
     begin
       n:=0;
       repeat
         inc(i);
         inc(n);
       until (GetShareData(i+10)>0) or (n=100) or (i>=max_ch-6);
       Buf_tx[1]:=5;    //send n zeroes + 6 bytes of data
       Buf_tx[2]:=n;    //number of zeroes to send
       Buf_tx[3]:=GetShareData(i+10);
       Buf_tx[4]:=GetShareData(i+11);
       Buf_tx[5]:=GetShareData(i+12);
       Buf_tx[6]:=GetShareData(i+13);
       Buf_tx[7]:=GetShareData(i+14);
       Buf_tx[8]:=GetShareData(i+15);
       DoWrite;
       i:=i+6;
     end;
   until  (i>=max_ch+1);
   FastTimer1.enabled:=true;
end;

procedure TForm1u.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  SetShareData(0,333);
  repeat until  GetShareData(0)=333;
end;

end.
