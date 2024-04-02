unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ImgList, ToolWin, ComCtrls, StdCtrls, ExtCtrls, ActnList, Buttons;

type
  TForm1 = class(TForm)
    StatusBar1: TStatusBar;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Panel1: TPanel;
    Memo1: TMemo;
    Help1: TMenuItem;
    About1: TMenuItem;
    ActionList1: TActionList;
    FTPort_Configure: TAction;
    Timer1: TTimer;
    Device1: TMenuItem;
    Configure1: TMenuItem;
    FTSendFile: TAction;
    FTReceiveFile: TAction;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    SendFile1: TMenuItem;
    ReceiveFile1: TMenuItem;
    N1: TMenuItem;
    FTQuit: TAction;
    FTResend: TAction;
    SndRxvCmp: TAction;
    N2: TMenuItem;
    Exit1: TMenuItem;
    ReSend1: TMenuItem;
    SendRxvCompare1: TMenuItem;
    ListView1: TListView;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    procedure FormShow(Sender: TObject);
    procedure Memo1KeyPress(Sender: TObject; var Key: Char);
    procedure FTPort_ConfigureExecute(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FTSendFileExecute(Sender: TObject);
    procedure FTReceiveFileExecute(Sender: TObject);
    procedure FTQuitExecute(Sender: TObject);
    procedure FTResendExecute(Sender: TObject);
    procedure SndRxvCmpExecute(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure ListView1Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses CfgUnit,D2XXUnit;

var
 DevicePresent : Boolean;
 Selected_Device_Serial_Number : String;
 Selected_Device_Description : String; 

{$R *.DFM}

procedure TForm1.FormShow(Sender: TObject);
var S : String; I : Integer; DeviceIndex : DWord; LV : TListItem;
begin
Memo1.Clear;
FT_Enable_Error_Report := true; // Error reporting = on
SaveDialog1.InitialDir := ExtractFilePath(Application.ExeName);
OpenDialog1.InitialDir := ExtractFilePath(Application.ExeName);
DevicePresent := False;
Memo1.Enabled := False;
FTSendFile.Enabled := False;
FTReceiveFile.Enabled := False;
FTPort_Configure.Enabled := False;
Timer1.Enabled := True;
FTSendFile.enabled := false;
FTReceiveFile.Enabled := false;
FTResend.Enabled := false;
SndRxvCmp.Enabled := false;
ListView1.Items.clear;
GetFTDeviceCount;
S := IntToStr(FT_Device_Count);
Caption := 'D2XX Delphi Demo - '+S+' Device(s) Present ...';
DeviceIndex := 0;
If FT_Device_Count > 0 then
  For I := 1 to FT_Device_Count do
  Begin
  LV := ListView1.Items.Add;
  LV.Caption := 'Device '+IntToStr(I);
  GetFTDeviceSerialNo( DeviceIndex );
  LV.SubItems.Add(FT_Device_String);
  GetFTDeviceDescription ( DeviceIndex );
  LV.SubItems.Add(FT_Device_String);
  DeviceIndex := DeviceIndex + 1;
  End;
end;

procedure TForm1.Memo1KeyPress(Sender: TObject; var Key: Char);
begin
// Caption := Key;
Key := Chr(0);
end;

procedure TForm1.FTPort_ConfigureExecute(Sender: TObject);
begin
Repeat
SetupForm.ShowModal;
If FT_SetUperror then
  MessageDlg('Configuration Error ...', mtError, [mbOk], 0);
Until Not FT_SetUpError;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
Var PortStatus : FT_Result;  S : String; DeviceIndex : DWord;  I : Integer;
begin

Exit; //debug
FT_Enable_Error_Report := False; // Turn off error dialog
If Not DevicePresent then
  Begin
  PortStatus := Close_USB_Device; // In case device was already open
  PortStatus := Open_USB_Device;  // Try and open device
  If PortStatus = FT_OK then      // Device is Now Present !
    Begin
    DevicePresent := True;
    Caption := 'D2XX Delphi Demo - Device Present ...';
    Memo1.Enabled := True;
    FTPort_Configure.Enabled := True;
    FTSendFile.Enabled := True;
    FTReceiveFile.Enabled := True;
    Reset_USB_Device;     // warning - this will destroy any pending data.
    Set_USB_Device_TimeOuts(500,500); // read and write timeouts = 500mS
    End;
  End
else
  Begin
  PortStatus := Get_USB_Device_QueueStatus;
  If PortStatus <> FT_OK then
    Begin   // Device has been Unplugged
    DevicePresent := False;
    Caption := 'D2XX Delphi Demo - No Device Present ...';
    Memo1.Enabled := False;
    FTSendFile.Enabled := False;
    FTReceiveFile.Enabled := False;
    FTPort_Configure.Enabled := False;
    End;
  End;

end;

procedure TForm1.FTSendFileExecute(Sender: TObject);
Var OpenFile : File;  OpenFileName : String;
    FC1,Total,I : Integer;
    S : String;
begin
If OpenDialog1.Execute then
  Begin
  Total := 0;
  FT_Enable_Error_Report := true; // Enable Error Reporting
  Timer1.Enabled := False;  // Stop Polling for Device Present
  OpenFileName := OpenDialog1.FileName;
  OpenDialog1.InitialDir := ExtractFilePath(OpenFilename);
  Memo1.Lines.Add('Opening - '+OpenFileName);
  AssignFile(OpenFile,OpenFileName);
  Reset(OpenFile,1);
    Repeat
    Application.ProcessMessages;
    BlockRead(OpenFile,FT_Out_Buffer,FT_Out_Buffer_Size,FC1);
    StatusBar1.Panels[0].Text := '   Bytes Sent = '+IntToStr(Total);
    If FC1 <> 0 then
      Begin
      I := Write_USB_Device_Buffer( FC1 );
      If I <> FC1 then Memo1.Lines.Add('USB Device Write TimeOut ...');
      Total := Total + I;
      End;
    Until ( FC1 <> FT_Out_Buffer_Size ); // Last Block of File ...
  S := IntToStr(Total)+ ' Bytes Sent ...';
  Memo1.Lines.Add(S);
  CloseFile(OpenFile);
  Timer1.Enabled := True;  // Resume Polling for Device Present
  End;
end;

procedure TForm1.FTReceiveFileExecute(Sender: TObject);
Var SaveFile : File;
    BytesWrote,Total,I : Integer;
    SaveFileName : String;
    PortStatus : FT_Result;
begin
If SaveDialog1.Execute then
  Begin
  FT_Enable_Error_Report := false; // Disable Error Reporting
  Timer1.Enabled := False;  // Stop Polling for Device Present
  SaveFileName := SaveDialog1.FileName;
  AssignFile(SaveFile,SaveFileName);
  SaveDialog1.InitialDir := ExtractFilePath(SaveFileName);
  ReWrite(SaveFile,1);
  Total := 0;
  StatusBar1.Panels[0].Text := 'Waiting for Data ...';
  Repeat
  Application.ProcessMessages;
  PortStatus := Get_USB_Device_QueueStatus;
  If PortStatus <> FT_OK then     // Device no longer present ...
    Begin
    CloseFile(SaveFile);
    Timer1.Enabled := True;
    StatusBar1.Panels[0].Text := '';
    Exit;
    End;
  Until FT_Q_Bytes > 0;
  Repeat
  Application.ProcessMessages;
  I := Read_USB_Device_Buffer(FT_In_Buffer_Size);
  Total := Total + I;
  StatusBar1.Panels[0].Text := 'Bytes Received = '+IntToStr(Total);
  If I > 0 then BlockWrite(SaveFile,FT_In_Buffer,I,BytesWrote);
  Until I = 0; // Nothing more to read !!!
  Memo1.Lines.Add('Done ...');
  CloseFile(SaveFile);
  Timer1.Enabled := True;  // Resume Polling for Device Present
  End;
end;

procedure TForm1.FTQuitExecute(Sender: TObject);
begin
If DevicePresent then Close_USB_Device;
Close;
end;

procedure TForm1.FTResendExecute(Sender: TObject);
Var SaveFile,OpenFile : File;
    BytesWrote,Total,I,FC1: Integer;
    SaveFileName,S : String;
    PortStatus : FT_Result;
begin
FT_Enable_Error_Report := false; // Disable Error Reporting
Timer1.Enabled := False;  // Stop Polling for Device Present
SaveFileName := 'SaveFile.tmp';
AssignFile(SaveFile,SaveFileName);
ReWrite(SaveFile,1);
Total := 0;
StatusBar1.Panels[0].Text := 'Waiting for Data ...';
Repeat
Application.ProcessMessages;
PortStatus := Get_USB_Device_QueueStatus;
If PortStatus <> FT_OK then     // Device no longer present ...
  Begin
  CloseFile(SaveFile);
  Timer1.Enabled := True;
  StatusBar1.Panels[0].Text := '';
  Exit;
  End;
Until FT_Q_Bytes > 0;
Repeat
Application.ProcessMessages;
I := Read_USB_Device_Buffer(FT_In_Buffer_Size);
Total := Total + I;
StatusBar1.Panels[0].Text := 'Bytes Received = '+IntToStr(Total);
If I > 0 then BlockWrite(SaveFile,FT_In_Buffer,I,BytesWrote);
Until I = 0; // Nothing more to read !!!
CloseFile(SaveFile);
Total := 0;
Memo1.Lines.Add('Opening - '+SaveFileName);
AssignFile(OpenFile,SaveFileName);
Reset(OpenFile,1);
Repeat
Application.ProcessMessages;
BlockRead(OpenFile,FT_Out_Buffer,FT_Out_Buffer_Size,FC1);
Total := Total + FC1;
StatusBar1.Panels[0].Text := '   Bytes Sent = '+IntToStr(Total);
If FC1 <> 0 then
  Begin
  I := Write_USB_Device_Buffer( FC1 );
  If I <> FC1 then Memo1.Lines.Add('USB Device Write TimeOut ...');
  End;
Until ( FC1 <> FT_Out_Buffer_Size ); // Last Block of File ...
S := IntToStr(Total)+ ' Bytes Sent ...';
Memo1.Lines.Add(S);
CloseFile(OpenFile);
Timer1.Enabled := True;  // Resume Polling for Device Present
end;

procedure TForm1.SndRxvCmpExecute(Sender: TObject);
Var OpenFile,SaveFile : File;  OpenFileName,SaveFileName : String;
    FC1,FC2,Total,I,SendTotal,BytesWrote : Integer;
    S : String;
    PortStatus : FT_Result;
    PassTest : Boolean;   
begin
If OpenDialog1.Execute then
  Begin
  Total := 0;
  FT_Enable_Error_Report := true; // Enable Error Reporting
  Timer1.Enabled := False;  // Stop Polling for Device Present
  OpenFileName := OpenDialog1.FileName;
  OpenDialog1.InitialDir := ExtractFilePath(OpenFilename);
  Memo1.Lines.Add('Opening - '+OpenFileName);
  AssignFile(OpenFile,OpenFileName);
  Reset(OpenFile,1);
    Repeat
    Application.ProcessMessages;
    BlockRead(OpenFile,FT_Out_Buffer,FT_Out_Buffer_Size,FC1);
    Total := Total + FC1;
    StatusBar1.Panels[0].Text := '   Bytes Sent = '+IntToStr(Total);
    StatusBar1.Update;
    Application.ProcessMessages;
    If FC1 <> 0 then
      Begin
      I := Write_USB_Device_Buffer( FC1 );
      If I <> FC1 then
        Begin
        Memo1.Lines.Add('USB Device Write TimeOut ...');
//        CloseFile(OpenFile);
//        Timer1.Enabled := True;
//        StatusBar1.Panels[0].Text := '';
//        Exit;
        End;
      End;
    Until ( FC1 <> FT_Out_Buffer_Size ); // Last Block of File ...
  S := IntToStr(Total)+ ' Bytes Sent ...';
  Memo1.Lines.Add(S);
  CloseFile(OpenFile);
  SendTotal := Total;
  SaveFileName := 'SaveFile.tmp';
  AssignFile(SaveFile,SaveFileName);
  ReWrite(SaveFile,1);
  Total := 0;
  StatusBar1.Panels[0].Text := 'Waiting for Data ...';
  Repeat
  Application.ProcessMessages;
  PortStatus := Get_USB_Device_QueueStatus;
  If PortStatus <> FT_OK then     // Device no longer present ...
    Begin
    CloseFile(SaveFile);
    Timer1.Enabled := True;
    StatusBar1.Panels[0].Text := '';
    Exit;
    End;
  Until FT_Q_Bytes > 0;
  Repeat
  Application.ProcessMessages;
  I := Read_USB_Device_Buffer(FT_In_Buffer_Size);
  Total := Total + I;
  StatusBar1.Panels[0].Text := 'Bytes Received = '+IntToStr(Total);
  If I > 0 then BlockWrite(SaveFile,FT_In_Buffer,I,BytesWrote);
  Until I = 0; // Nothing more to read !!!
  CloseFile(SaveFile);
  End;
If SendTotal <> Total then Memo1.Lines.Add('Test Failed - Bytes Sent <> Bytes Received') else
  Begin
  Memo1.Lines.Add('Comparing Data -');
  Passtest := True;
  Reset(OpenFile,1);
  Reset(SaveFile,1);
  Repeat
  BlockRead(OpenFile,FT_Out_Buffer,FT_In_Buffer_Size,FC1);
  BlockRead(SaveFile,FT_In_Buffer,FT_In_Buffer_Size,FC2);
  If FC1 <> FC2 then Memo1.Lines.Add('File Block Comapre Mismatch');
  If FC1 <> 0 then
    Begin
    For I := 0 to ( FC1 - 1 ) do
      If FT_Out_Buffer[I] <> FT_In_Buffer[I] then Passtest := False;
    End;
  Until FC1 <> FT_In_Buffer_Size;
  CloseFile(OpenFile);
  CloseFile(SaveFile);
  If PassTest then  Memo1.Lines.Add('Data Compares O.K. - PASS !') else
                    Memo1.Lines.Add('Data Compare Error - Fail !')
  End;
end;


procedure TForm1.BitBtn2Click(Sender: TObject);
var S:String; DeviceIndex : DWord; I : Integer; LV : TListItem;
begin
ListView1.Items.clear;
GetFTDeviceCount;
S := IntToStr(FT_Device_Count);
Caption := 'D2XX Delphi Demo - '+S+' Device(s) Present ...';
DeviceIndex := 0;
If FT_Device_Count > 0 then
  For I := 1 to FT_Device_Count do
  Begin
  LV := ListView1.Items.Add;
  LV.Caption := 'Device '+IntToStr(I);
  GetFTDeviceSerialNo( DeviceIndex );
  LV.SubItems.Add(FT_Device_String);
  GetFTDeviceDescription ( DeviceIndex );
  LV.SubItems.Add(FT_Device_String);
  DeviceIndex := DeviceIndex + 1;
  End;
end;

procedure TForm1.BitBtn4Click(Sender: TObject);
begin
Close;
end;

procedure TForm1.ListView1Click(Sender: TObject);
var LV : TListItem;
begin
If Listview1.SelCount > 0 then
  Begin
  LV := ListView1.Selected;
  BitBtn1.Enabled := True;
  Selected_Device_Serial_Number := LV.SubItems[0];
  Selected_Device_Description := LV.SubItems[1];
  End else
  BitBtn1.Enabled := False;
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
Caption := Selected_Device_Serial_Number;
If Open_USB_Device_By_Serial_Number(Selected_Device_Serial_Number) = FT_OK then
//Caption := Selected_Device_Description;
//If Open_USB_Device_By_Device_Description(Selected_Device_Description) = FT_OK then
  Begin
  BitBtn1.Enabled := False;
  BitBtn2.Enabled := False;
  BitBtn3.Enabled := true;
  BitBtn4.Enabled := False;
  FTSendFile.enabled := true;
  FTReceiveFile.Enabled := true;
  FTResend.Enabled := true;
  SndRxvCmp.Enabled := true;
  End;
end;

procedure TForm1.BitBtn3Click(Sender: TObject);
begin
Caption := 'D2XX Delphi Demo';
Close_USB_Device;
BitBtn1.Enabled := False;
BitBtn2.Enabled := True;
BitBtn3.Enabled := False;
BitBtn4.Enabled := True;
FTSendFile.enabled := false;
FTReceiveFile.Enabled := false;
FTResend.Enabled := false;
SndRxvCmp.Enabled := false;
end;

end.
