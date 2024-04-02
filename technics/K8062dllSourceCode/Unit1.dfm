object Form1u: TForm1u
  Left = 134
  Top = -10
  Width = 456
  Height = 380
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'SimpleHIDWrite'
  Color = clBtnFace
  Constraints.MinHeight = 380
  Constraints.MinWidth = 456
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Visible = True
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 16
  object ListBox1: TListBox
    Left = 4
    Top = 4
    Width = 441
    Height = 73
    ItemHeight = 16
    TabOrder = 0
  end
  object HidCtl: TJvHidDeviceController
    OnEnumerate = HidCtlEnumerate
    OnDeviceChange = HidCtlDeviceChange
    Left = 308
    Top = 16
  end
  object FastTimer1: TFastTimer
    OnTimer = FastTimer1Timer
    Enabled = False
    Interval = 1
    Resolution = 5
    Left = 272
    Top = 16
  end
end
