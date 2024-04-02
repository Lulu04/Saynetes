unit frame_fixture_image;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls;

type

  { TFrameFixtureImage }

  TFrameFixtureImage = class(TFrame)
    PB: TPaintBox;
    Shape1: TShape;
    ST: TStaticText;
  private
    function GetText: string;
    procedure SetText(AValue: string);

  public
    property Text: string read GetText write SetText;

  end;

implementation
uses u_utils;

{$R *.lfm}

{ TFrameFixtureImage }

function TFrameFixtureImage.GetText: string;
begin
  Result := ST.Caption;
end;

procedure TFrameFixtureImage.SetText(AValue: string);
begin
  ST.Caption := AValue;
end;

end.

