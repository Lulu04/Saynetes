unit u_project_options;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  ComCtrls, StdCtrls, u_notebook_util, DividerBevel;

type

  { TFormProjectOptions }

  TFormProjectOptions = class(TForm)
    BApply: TSpeedButton;
    BCancel: TSpeedButton;
    BOk: TSpeedButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    DividerBevel3: TDividerBevel;
    DividerBevel5: TDividerBevel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Notebook1: TNotebook;
    PageSequence: TPage;
    PageMainView: TPage;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    Splitter1: TSplitter;
    TV: TTreeView;
    procedure BOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TVSelectionChanged(Sender: TObject);
  private
    CheckedLabelManager: TCheckedLabelManager;
  private
    FLoadingOptions: boolean;
    procedure UpdateLanguageOnWidgets;
    procedure ProjectOptionsToWidgets;
    procedure WidgetsToProjectOptions;
  public

  end;

var
  FormProjectOptions: TFormProjectOptions;

implementation

uses u_resource_string, u_project_manager, u_mainform;

{$R *.lfm}

{ TFormProjectOptions }

procedure TFormProjectOptions.FormShow(Sender: TObject);
begin
  UpdateLanguageOnWidgets;

  ProjectOptionsToWidgets;

  TV.Selected := TV.Items.GetFirstNode;
end;

procedure TFormProjectOptions.TVSelectionChanged(Sender: TObject);
var nodeParent: TTreeNode;
begin
//  txt := 'Selected: '+TV.Selected.Text+' Level '+TV.Selected.Level.ToString+' Index'+TV.Selected.Index.ToString;

  nodeParent := TV.Selected;
  while nodeParent.Level <> 0 do
    nodeParent := nodeParent.Parent;
//  txt := txt+' - Parent: '+nodeParent.Text+' Level '+nodeParent.Level.ToString+' Index'+nodeParent.Index.ToString;
//  Caption := txt;

  case nodeParent.Index of
   0: // Main view
     begin
       Notebook1.PageIndex := Notebook1.IndexOf(PageMainView);
     end;
   1: // Sequence
     begin
       Notebook1.PageIndex := Notebook1.IndexOf(PageSequence);
     end;

  end;
end;

procedure TFormProjectOptions.BOkClick(Sender: TObject);
begin
  if Sender = BCancel then
  begin
    Close;
    exit;
  end;

  if Sender = BApply then
  begin
    WidgetsToProjectOptions;
    FormMain.UpdateLayout;
    exit;
  end;

  // apply new value and save project options
  WidgetsToProjectOptions;
  ModalResult := mrOk;
end;

procedure TFormProjectOptions.FormCreate(Sender: TObject);
begin
  CheckedLabelManager := TCheckedLabelManager.Create;
  CheckedLabelManager.CaptureLabelClick(Label1);
  CheckedLabelManager.CaptureLabelClick(Label2);

  CheckedLabelManager.CaptureLabelClick(Label3);
  CheckedLabelManager.CaptureLabelClick(Label5);
  CheckedLabelManager.CaptureLabelClick(Label6);
  CheckedLabelManager.CaptureLabelClick(Label7);
  CheckedLabelManager.CaptureLabelClick(Label8);
  CheckedLabelManager.CaptureLabelClick(Label9);
end;

procedure TFormProjectOptions.FormDestroy(Sender: TObject);
begin
  CheckedLabelManager.Free;
end;

procedure TFormProjectOptions.UpdateLanguageOnWidgets;
var n: TTreeNode;
begin
  BOk.Caption := SOk;
  BCancel.Caption := SCancel;
  BApply.Caption := SApply;

  TV.BeginUpdate;
  TV.Items.Clear;
  n := TV.Items.AddFirst(NIL, SMainView);
  TV.Items.Add(n, SSequence);
  TV.Selected := n;
  TV.EndUpdate;
end;

procedure TFormProjectOptions.ProjectOptionsToWidgets;
begin
  FLoadingOptions := True;
  try
    // Main View
    CheckBox2.Checked := Project.Options.MainViewShowAudioControlPanel;
    CheckBox1.Checked := Project.Options.MainViewShowAudioCapturePanel;

    // Sequence action list
    CheckBox3.Checked := Project.Options.CmdListViewDMXAdress;
    CheckBox5.Checked := Project.Options.CmdListViewDMXFixName;
    CheckBox6.Checked := Project.Options.CmdListViewDMXFixDescription;
    CheckBox7.Checked := Project.Options.CmdListViewDMXChannelName;
    if Project.Options.CmdListViewColorAsRectangleColor then RadioButton1.Checked := True
      else RadioButton2.Checked := True;

  finally
    FLoadingOptions := False;
    Project.Options.Save;
  end;
end;

procedure TFormProjectOptions.WidgetsToProjectOptions;
begin
  if FLoadingOptions then exit;

  // Main View
  Project.Options.MainViewShowAudioControlPanel := CheckBox2.Checked;
  Project.Options.MainViewShowAudioCapturePanel := CheckBox1.Checked;

  // Sequence action list
  Project.Options.CmdListViewDMXAdress := CheckBox3.Checked;
  Project.Options.CmdListViewDMXFixName := CheckBox5.Checked;
  Project.Options.CmdListViewDMXFixDescription := CheckBox6.Checked;
  Project.Options.CmdListViewDMXChannelName := CheckBox7.Checked;
  Project.Options.CmdListViewColorAsRectangleColor := RadioButton1.Checked;
end;

end.

