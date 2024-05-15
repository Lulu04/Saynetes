unit form_defineswitchingchannel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  CheckLst, Buttons;

type

  { TFormEditSwitchingChannel }

  TFormEditSwitchingChannel = class(TForm)
    BOk: TSpeedButton;
    BNew: TSpeedButton;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    LB: TCheckListBox;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;


implementation

uses u_resource_string;

{$R *.lfm}

{ TFormEditSwitchingChannel }

procedure TFormEditSwitchingChannel.FormCreate(Sender: TObject);
begin
  // manual translations
  BOk.Caption := SOk;
  BNew.Caption := SCreateNewChannel;
end;

end.

