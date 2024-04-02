unit DMX_demo1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Grids, Menus, Buttons;

type
  TForm1 = class(TForm)
    ScrollBar1: TScrollBar;
    Shape1: TShape;
    Label2: TLabel;
    Shape2: TShape;
    Label1: TLabel;
    ComboBox1: TComboBox;
    Label3: TLabel;
    Label5: TLabel;
    StringGrid1: TStringGrid;
    Button1: TButton;
    Button4: TButton;
    ComboBox2: TComboBox;
    Label6: TLabel;
    Label7: TLabel;
    ComboBox3: TComboBox;
    Label4: TLabel;
    Label8: TLabel;
    SpeedButton1: TSpeedButton;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    SaveShow1: TMenuItem;
    OpenShow1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Timer1: TTimer;
    ScrollBar2: TScrollBar;
    CheckBox1: TCheckBox;
    Button2: TButton;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
    procedure CreateControls;
    procedure ComboBox1Change(Sender: TObject);
    procedure StringGrid1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SpeedButton1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ScrollBar2Change(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure SaveShow1Click(Sender: TObject);
    procedure OpenShow1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

  private
    { Private declarations }
  public
    { Public declarations }
    Labels: array [0..32] of TLabel;
    Labels1: array [0..32] of TLabel;
    Scrollbars: array [0..7] of TScrollBar;
    DMX_data: array [0..512] of integer;
    max_ch,sb1_left,start:integer;
    time_step,fade_time:real;
    timed,stop:boolean;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}
PROCEDURE StartDevice; stdcall; external 'K8062d.dll';
PROCEDURE SetData(Channel: Longint ; Data: Longint); stdcall; external 'K8062d.dll';
PROCEDURE SetChannelCount(Count: Longint); stdcall; external 'K8062d.dll';
PROCEDURE StopDevice; stdcall; external 'K8062d.dll';

procedure TForm1.FormCreate(Sender: TObject);
begin
  stop:=true;
  max_ch:=16;
  time_step:=1;
  fade_time:=0;
  CreateControls;
  StartDevice;
  sb1_left:=ScrollBar1.left;
  start:=1;
end;

procedure TForm1.ScrollBar1Change(Sender: TObject);
var i:integer;
begin
  with Sender as TScrollbar do
  begin
    labels[Tag].caption:=inttostr(255-Position);
    DMX_data[start+Tag]:=255-Position;
    //SetData(start+Tag,255-position);
    for i:=1 to max_ch do SetData(i,DMX_data[i]);
  end;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
var max,code,i:integer;
begin
  val(ComboBox1.text,max, code);
  if (code=0) then
  begin
    if max<8 then max:=8;
    max_ch:=max;
    StringGrid1.ColCount:=max_ch+3;
    for i:=1 to max_ch+3 do StringGrid1.Cells[i+2,0]:=inttostr(i);
    SetChannelCount(max_ch);
    ScrollBar2.max:=max_ch-7;
  end;
end;

procedure TForm1.CreateControls;
var
  I, J: Integer;
begin
  for I := 0 to 7 do
  begin
     Scrollbars[I]:=TScrollbar.Create(self);
      with Scrollbars[I] do
      begin
        Visible  := (i<8);
        Left     := ScrollBar1.Left + I*(ScrollBar1.Width+10);
        Top      := ScrollBar1.Top;
        Width    := ScrollBar1.Width;
        Kind     := ScrollBar1.Kind;
        Height   := ScrollBar1.Height;
        Max      := ScrollBar1.Max;
        Tag      := i;
        Position := 255;
        OnChange   := ScrollBar1Change;
        Parent := ScrollBar1.Parent;
      end;
  end;
  for I := 0 to 7 do
  begin
    Labels[I] := TLabel.Create(Self);
    with Labels[I] do
    begin
        Caption  := '00';
        Font.Color:=clBlack;
        Color    := clWhite;
        Left     := Label1.Left + I*(ScrollBar1.Width+10);
        Top      := Label1.Top;
        Width    := Label1.Width;
        Height   := Label1.Height;
        Parent := Label1.Parent;
    end;
    Labels1[I] := TLabel.Create(Self);
    with Labels1[I] do
    begin
        Caption  := Inttostr(i+1);
        Font.Color:=clWhite;
        Color    := clGray;
        Left     := Label2.Left + I*(ScrollBar1.Width+10);
        Top      := Label2.Top;
        Width    := Label2.Width;
        Height   := Label2.Height;
        Parent := Label2.Parent;
    end;
  end;
  for i:=1 to max_ch do   StringGrid1.Cells[i+2,0]:=inttostr(i);
  StringGrid1.ColWidths[0]:=50;
  StringGrid1.ColWidths[1]:=50;
  StringGrid1.ColWidths[2]:=50;
  StringGrid1.Cells[0,0]:='Scene';
  StringGrid1.Cells[1,0]:='Time [s]';
  StringGrid1.Cells[2,0]:='Fade [s]';
  for i:=0 to 512 do DMX_data[i]:=0;
end;

procedure TForm1.StringGrid1Click(Sender: TObject);
begin
  if stop then
  begin
    Button2.enabled:=not(StringGrid1.row+1=StringGrid1.RowCount);
    Button4.enabled:=Button2.enabled;
    if Button4.enabled then Button1.caption:='Insert>>' else
    Button1.caption:='Add Scene>>'
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  i: Integer;
  s:string;
begin
  StringGrid1.RowCount:=StringGrid1.RowCount+1;
  for i:=StringGrid1.RowCount downto StringGrid1.row+1 do
  StringGrid1.Rows[i]:=StringGrid1.Rows[i-1];
  for i:=1 to max_ch do StringGrid1.Cells[i+2,StringGrid1.row]:=
  inttostr(DMX_data[i]);
  str(time_step:2:1,s);
  StringGrid1.Cells[1,StringGrid1.row]:=s;
  str(fade_time:2:1,s);
  StringGrid1.Cells[2,StringGrid1.row]:=s;
  StringGrid1.row:=StringGrid1.row+1;
  Button4.enabled:=not(StringGrid1.row+1=StringGrid1.RowCount);
  for i:=1 to StringGrid1.RowCount-2 do StringGrid1.Cells[0,i]:=inttostr(i);
  StringGrid1.LeftCol:=start+2;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  i: Integer;
begin
  if StringGrid1.RowCount>2 then
  begin
    for i:= StringGrid1.row to StringGrid1.RowCount do
    StringGrid1.Rows[i]:=StringGrid1.Rows[i+1];
    StringGrid1.RowCount:=StringGrid1.RowCount-1;
  end;
  Button4.enabled:=not(StringGrid1.row+1=StringGrid1.RowCount);
  Button2.enabled:=Button4.enabled;
  if not Button4.enabled then Button1.caption:='Add Scene>>';
  for i:=1 to StringGrid1.RowCount-2 do StringGrid1.Cells[0,i]:=inttostr(i);
  StringGrid1.LeftCol:=start+2;
end;

procedure TForm1.ComboBox2Change(Sender: TObject);
var code:integer;
fl:real;
s:string;
begin
  s:=ComboBox2.text;
  if (Pos(',', s) > 0) then  S[Pos(',', S)] := '.';
  Val(s, fl, Code);
  if code=0 then
  begin
    time_step:=fl;
    str(fl:2:1,s);
    StringGrid1.Cells[1,StringGrid1.row]:=s;
  end;
end;

procedure TForm1.ComboBox3Change(Sender: TObject);
var code,i:integer;
fl:real;
s:string;
begin
  s:=ComboBox3.text;
  if (Pos(',', s) > 0) then  S[Pos(',', S)] := '.';
  Val(s, fl, Code);
  if code=0 then
  begin
    fade_time:=fl;
    str(fl:2:1,s);
    StringGrid1.Cells[2,StringGrid1.row]:=s;
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  StopDevice;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
var i,j,m,n,tmp,code:integer;
f:real;
incr: array[0..512] of real;
temp: array[0..512] of integer;
diff: array[0..512] of integer;
label exit1;
begin
  Button1.enabled:=false;
  Button2.enabled:=false;
  Button4.enabled:=false;
  ComboBox1.enabled:=false;
  ComboBox2.enabled:=false;
  ComboBox3.enabled:=false;
  if SpeedButton1.Down and (StringGrid1.RowCount>2) then
  begin
    stop:=false;
    i:=1;
    repeat
      StringGrid1.row:=i;
      StringGrid1.LeftCol:=start+2;
      for j:=1 to max_ch do SetData(j,strtointdef(StringGrid1.Cells[j+2,i],0));
      val(StringGrid1.Cells[1,i],f,code);
      timer1.interval:=round(1000*f);
      timed:=(f<0.05);
      timer1.enabled:=true;
      repeat
        application.processmessages;
      until timed or stop;
      if stop then exit;
      val(StringGrid1.Cells[2,i],f,code);
      if f>0.05 then
      begin
        for tmp:=1 to max_ch do
        begin
          temp[tmp]:=strtointdef(StringGrid1.Cells[tmp+2,i],0);
          incr[tmp]:=temp[tmp];
          if i=StringGrid1.RowCount-2 then m:=1 else m:=i+1;
          diff[tmp]:=strtointdef(StringGrid1.Cells[tmp+2,m],0)-strtointdef(StringGrid1.Cells[tmp+2,i],0);
        end;
        j:=round(f/0.05);
        n:=j;
        timer1.interval:=50;
        repeat
          timed:=false;
          timer1.enabled:=true;
          repeat
            application.processmessages;
          until timed or stop;
          if stop then goto exit1;
          for tmp:=1 to max_ch do
          begin
            incr[tmp]:=incr[tmp]+diff[tmp]/n;
            //StringGrid1.Cells[tmp+2,i]:=inttostr(round(incr[tmp]));
            SetData(tmp,round(incr[tmp]));
          end;
          dec(j);
        until j=0;
        exit1:
        //for tmp:=1 to max_ch do StringGrid1.Cells[tmp+2,i]:=inttostr(temp[tmp]);
      end;
      inc(i);
      if (i=StringGrid1.RowCount-1) and (CheckBox1.checked) then i:=1;
      if (i=StringGrid1.RowCount-1) and not (CheckBox1.checked) then stop:=true;
    until stop;
    StringGrid1.row:=i;
    SpeedButton1.Down:=false;
  end
  else stop:=true;
  ComboBox1.enabled:=true;
  ComboBox2.enabled:=true;
  ComboBox3.enabled:=true;
  Button1.enabled:=true;
  StringGrid1Click(self);

end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  timer1.enabled:=false;
  timed:=true;
end;

procedure TForm1.ScrollBar2Change(Sender: TObject);
var i:integer;
begin
  start:=ScrollBar2.position;
  for i:=0 to 7 do
  begin  
    labels1[i].caption:=inttostr(start+i);
    Scrollbars[i].position:=255-DMX_data[start+i];
    labels[i].caption:=inttostr(255-Scrollbars[i].position);
  end;
  StringGrid1.LeftCol:=start+2;
end;

procedure TForm1.Button2Click(Sender: TObject);
var i:integer;
begin
  ComboBox2.text:=StringGrid1.Cells[1,StringGrid1.row];
  ComboBox3.text:=StringGrid1.Cells[2,StringGrid1.row];
  for i:=1 to max_ch do DMX_data[i]:=strtointdef(StringGrid1.Cells[i+2,StringGrid1.row],0);
  ScrollBar2Change(self);
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.SaveShow1Click(Sender: TObject);
var f:textfile;
i,j:integer;
begin
  if SaveDialog1.execute then
  begin
    AssignFile(f,SaveDialog1.filename);
    Rewrite(f);
    Writeln(f,inttostr(max_ch));
    Writeln(f,inttostr(StringGrid1.RowCount));
    for j:=0 to StringGrid1.RowCount-1 do           
      for i:=0 to max_ch+2 do
        Writeln(f,StringGrid1.cells[i,j]);
    CloseFile(f);
  end;
end;

procedure TForm1.OpenShow1Click(Sender: TObject);
var f:textfile;
i,j:integer;
s:string;
begin
  if OpenDialog1.execute then
  begin
    AssignFile(f,OpenDialog1.filename);
    {$I-}
    Reset(f);
    {$I+}
    i:=IOResult;
    if i=0 then
    begin
      Readln(f,s); 
      ComboBox1.text:=s;
      ComboBox1Change(self);
      Readln(f,s);
      StringGrid1.RowCount:=strtointdef(s,2);
      for j:=0 to StringGrid1.RowCount-1 do
      for i:=0 to max_ch+2 do
      begin
        Readln(f,s);
        StringGrid1.cells[i,j]:=s;  
      end;
    end;
    CloseFile(f);
  end;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if SpeedButton1.down then
  begin
    SpeedButton1.down:=false;
    SpeedButton1Click(self);
  end;
end;

end.
