unit frame_viewdmxlibrary;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, LCLType, Menus,
  LCLTranslator, ExtCtrls,
  u_list_dmxuniverse, u_common;

type

  TMoveItemEvent = procedure(Sender: TObject; aNode: TTreeNode; const aTargetPath: string; var Accept: boolean) of object;
  TSelectionChangeEvent = procedure(Sender: TObject; const aFixLocation: TFixtureLibraryLocation) of object;
  TStartDragFixtureEvent = TSelectionChangeEvent;

  { TFrameViewDMXLibrary }

  TFrameViewDMXLibrary = class(TFrame)
    MICollapseTree: TMenuItem;
    MIDevelop: TMenuItem;
    MIRenameFolder: TMenuItem;
    MIRenameFixture: TMenuItem;
    MINewFolder: TMenuItem;
    MIDeleteFolder: TMenuItem;
    MenuItem4: TMenuItem;
    MIDeleteFixture: TMenuItem;
    Separator1: TMenuItem;
    TV: TTreeView;
    procedure TVClick(Sender: TObject);
    procedure TVSelectionChanged(Sender: TObject);
    procedure TVStartDrag(Sender: TObject; var {%H-}DragObject: TDragObject);
  private
    FOnMoveItem: TMoveItemEvent;
    FOnSelectionChange: TSelectionChangeEvent;
    FOnStartDragFixture: TStartDragFixtureEvent;
    function GetSelectedFixtureLocation: TFixtureLibraryLocation;
    function SortProc( Node1, Node2: TTreeNode ): integer;
    procedure FillTreeViewWithLibraryContent;
    function RelativePathForNode(aNode: TTreeNode): string;
    function AbsolutePathForNode(aNode: TTreeNode): string;
    function ItsTheRoot( aNode: TTreeNode ): boolean;
  public
    procedure EraseBackground({%H-}DC: HDC); override;
    // gives '' if no selection
    function GetSelectedManufacturerFolder: string;
    // gives '' if its a folder or no selection
    function GetSelectedFixtureFileName: string;
    // return '' if a mode is not selected
    function GetSelectedFixtureMode: string;

    procedure Fill;
    // update the list of modes for the current selected item
    procedure UpdateSelectedAfterEdition;

    function ItsAFolder(aNode: TTreeNode): boolean;
    function ItsAFile(aNode: TTreeNode): boolean;
    function ItsAMode(aNode: TTreeNode): boolean;

    function SelectedIsFile: boolean;
    function SelectedIsMode: boolean;

    procedure SetSelected(const aNodeText: string);
    // search the fixture in the treeview, select it and make it visible
    procedure SelectFixture(const aFixtureLocation: TFixtureLibraryLocation);

    // use this property only if function SelectedIsMode return True
    property SelectedFixtureLocation: TFixtureLibraryLocation read GetSelectedFixtureLocation;
    property OnSelectionChange: TSelectionChangeEvent read FOnSelectionChange write FOnSelectionChange;
    property OnMoveItem: TMoveItemEvent read FOnMoveItem write FOnMoveItem;
    property OnStartDragFixture: TStartDragFixtureEvent read FOnStartDragFixture write FOnStartDragFixture;
  end;

implementation

uses u_resource_string, u_userdialogs, u_logfile, u_apputils,
  u_dmx_util, utilitaire_fichier, LazFileUtils, Dialogs;

{$R *.lfm}

{ TFrameViewDMXLibrary }

procedure TFrameViewDMXLibrary.TVClick(Sender: TObject);
var p: TPoint;
  n: TTreeNode;
begin
 // here, we intercept user click on the root node
 // and we expand/collapse the treeview
  p := TV.ScreenToClient(Mouse.CursorPos);
  n := TV.GetNodeAt(p.x, p.y);
  if n <> TV.Items.GetFirstNode then
    exit;

  if TV.Tag=0 then
  begin
    for n in TV.Items do
      if n <> TV.Items.GetFirstNode then
        n.Expand(False);
    TV.Tag := 1;
  end
  else begin
    for n in TV.Items do
      if n <> TV.Items.GetFirstNode then
        n.Collapse(False);
    TV.Tag := 0;
  end;
end;

procedure TFrameViewDMXLibrary.TVSelectionChanged(Sender: TObject);
begin
  if FOnSelectionChange = NIL then exit;
  FOnSelectionChange(Self, GetSelectedFixtureLocation);
end;

procedure TFrameViewDMXLibrary.TVStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
  if not ItsAMode(TV.Selected) then exit;
  if FOnStartDragFixture <> NIL then
    FOnStartDragFixture(Self, GetSelectedFixtureLocation);
end;

function TFrameViewDMXLibrary.SortProc(Node1, Node2: TTreeNode): integer;
begin
 {Files before folders}
 if( ItsAFile(Node1) and ItsAFolder(Node2) ) then
 begin
   Result := 1;
   exit;
 end;
 {Folder after file}
 if( ItsAFolder(Node1) and ItsAFile(Node2) ) then
 begin
   Result := -1;
   exit;
 end;
 {Nodes are of the same type, so do a normal alpha sort}
 Result := StrIComp( PChar(Node1.Text), PChar(Node2.Text) );
end;

function TFrameViewDMXLibrary.GetSelectedManufacturerFolder: string;
var n: TTreeNode;
begin
  Result := '';
  n := TV.Selected;
  if n = NIL then exit;
  if ItsAMode(n) then n := n.Parent;
  if ItsAFile(n) then n := n.Parent;

  Result := AbsolutePathForNode(n);
end;

function TFrameViewDMXLibrary.GetSelectedFixtureFileName: string;
var n: TTreeNode;
begin
  Result := '';
  n := TV.Selected;
  if n = NIL then exit;
  if ItsAMode(n) then n := n.Parent;
  if not ItsAFile(n) then exit;

  Result := ConcatPaths([AbsolutePathForNode(n), n.Text]);
  Result := ChangeFileExt(Result, DMX_LIBRARY_FILE_EXTENSION);
end;

function TFrameViewDMXLibrary.GetSelectedFixtureMode: string;
begin
  Result := '';
  if TV.Selected = NIL then exit;
  if not ItsAMode(TV.Selected) then exit;

  Result := TV.Selected.Text;
end;

function TFrameViewDMXLibrary.GetSelectedFixtureLocation: TFixtureLibraryLocation;
var n: TTreeNode;
begin
  Result.InitDefault;
  n := TV.Selected;
  if n = NIL then exit;

  if not ItsTheRoot(n) then begin
    Result.SubFolder := RelativePathForNode(n);
    if ItsAFile(n) then begin
      Result.Filename := ChangeFileExt(n.Text, DMX_LIBRARY_FILE_EXTENSION);
    end else
    if ItsAMode(n) then begin
      Result.Filename := ChangeFileExt(n.Parent.Text, DMX_LIBRARY_FILE_EXTENSION);
      Result.Mode := n.Text;
    end;
  end;
end;

procedure TFrameViewDMXLibrary.FillTreeViewWithLibraryContent;
var c: integer;
  procedure ScanFolder (folder: string; aNode: TTreeNode);
  var sr: TSearchRec;
    n, n1: TTreeNode;
    modes: TStringArray;
    i: Integer;
  begin
   if LazFileUtils.FindFirstUTF8(folder + DirectorySeparator + '*', faAnyFile, sr) = 0 then
   begin
    repeat
     if Sr.Attr and faDirectory > 0 then
     begin
       if not((Sr.Name = '.') or (Sr.Name = '..')) then
       begin
         // one found a folder
         n := TV.Items.AddChild( aNode, Sr.Name);

         n.SelectedIndex := 1;  // icon folder
         n.ImageIndex := 1;     // icon folder
         n.MakeVisible;
         n.Collapse(True);
         ScanFolder(folder + DirectorySeparator + Sr.Name, n);
       end;
     end
     else if LowerCase(ExtractFileExt(sr.Name)) = DMX_LIBRARY_FILE_EXTENSION then
     begin
       // found dmx fixture file
      inc(c);
       n := TV.Items.AddChild( aNode, ChangeFileExt(Sr.Name, ''));
       // set icon
       n.SelectedIndex := 2;
       n.ImageIndex := 2;
       // add each modes
       modes := GetFixtureModeNames(ConcatPaths([folder, Sr.Name]));
       for i:=0 to High(modes) do begin
         n1 := TV.Items.AddChild( n, modes[i]);
         n1.SelectedIndex := 3;
         n1.ImageIndex := 3;
       end;
     end;
    until LazFileUtils.FindNextUTF8(Sr) <> 0;
   end;
   LazFileUtils.FindCloseUTF8(Sr);
  end;

begin
 c := 0;
 TV.BeginUpdate;
 TV.Items.Clear; // clear the TreeView
 with TV.Items.AddFirst(nil, SDMXLibrary) do
   begin
    ImageIndex := 0;
    SelectedIndex := 0;
   end;
 ScanFolder(GetDMXLibraryFolder, TV.Items.GetFirstNode);
 TV.Items.GetFirstNode.Text := SDMXLibrary+' - '+c.ToString+' '+SFixtures;
 TV.EndUpdate;
end;

function TFrameViewDMXLibrary.RelativePathForNode(aNode: TTreeNode): string;
var n: TTreeNode;
begin
  Result := '';
  if aNode = NIL then exit;
  if ItsTheRoot(aNode) then exit;

  // check if the node is a Mode name, we switch to its parent (filename)
  if ItsAMode(aNode) then aNode := aNode.Parent;
  // check if the node is a file, we switch to its parent (folder)
  if ItsAFile(aNode) then aNode := aNode.Parent;

  // reconstruct the full path from parents of selected item
  Result := aNode.Text;
  n := aNode;
  while n.Level > 1 do begin
    n := n.Parent;
    Result := ConcatPaths([n.Text, Result]);
  end;
end;

function TFrameViewDMXLibrary.AbsolutePathForNode(aNode: TTreeNode): string;
begin
 if aNode = NIL then
   Result := ''
 else begin
   Result := ConcatPaths([GetDMXLibraryFolder, RelativePathForNode( aNode )]);
   if ItsAFolder( aNode ) then
     Result := IncludeTrailingPathDelimiter(Result);
 end;
end;

function TFrameViewDMXLibrary.ItsTheRoot(aNode: TTreeNode): boolean;
begin
 if aNode = NIL then
 begin
   Result := FALSE;
   exit;
 end
 else Result := aNode.ImageIndex = 0;
end;

function TFrameViewDMXLibrary.ItsAFile(aNode: TTreeNode): boolean;
begin
 if aNode = NIL then
 begin
   Result := FALSE;
   exit;
 end
 else Result := aNode.ImageIndex = 2;
end;

function TFrameViewDMXLibrary.ItsAFolder(aNode: TTreeNode): boolean;
begin
 if aNode = NIL then
 begin
   Result := FALSE;
   exit;
 end
 else Result := aNode.ImageIndex = 1;
end;

function TFrameViewDMXLibrary.ItsAMode(aNode: TTreeNode): boolean;
begin
  if aNode = NIL then Result := False
    else Result := aNode.ImageIndex = 3;
end;

function TFrameViewDMXLibrary.SelectedIsFile: boolean;
begin
  Result := ItsAFile(TV.Selected);
end;

function TFrameViewDMXLibrary.SelectedIsMode: boolean;
begin
 Result := ItsAMode(TV.Selected);
end;

procedure TFrameViewDMXLibrary.SetSelected(const aNodeText: string);
begin
  TV.Selected := TV.Items.FindNodeWithText(aNodeText);
  TV.MakeSelectionVisible;
end;

procedure TFrameViewDMXLibrary.SelectFixture(const aFixtureLocation: TFixtureLibraryLocation);
var nodeFolder, nodeFilename, nodeMode: TTreeNode;
begin
  // search the node with folder
  nodeFolder := TV.Items.FindNodeWithText(aFixtureLocation.SubFolder);
  if nodeFolder = NIL then exit;
  nodeFolder.Expand(False);

  // search the sub-node with filename
  nodeFilename := nodeFolder.FindNode(ChangeFileExt(aFixtureLocation.Filename, ''));
  if nodeFilename <> NIL then nodeFilename.Expand(False);

  // search the sub-node with mode
  if nodeFilename <> NIL then begin
    nodeMode := nodeFilename.FindNode(aFixtureLocation.Mode);
    if nodeMode <> NIL then nodeMode.Expand(False);
  end else nodeMode := NIL;

  if nodeMode <> NIL then TV.Selected := nodeMode
    else if nodeFilename <> NIL then TV.Selected := nodeFilename
      else TV.Selected := nodeFolder;
  TV.MakeSelectionVisible;
end;

procedure TFrameViewDMXLibrary.EraseBackground(DC: HDC);
begin
// do nothing here
end;

procedure TFrameViewDMXLibrary.Fill;
begin
  try
    FillTreeViewWithLibraryContent;
    TV.CustomSort(@SortProc);
    TV.Items.GetFirstNode.MakeVisible;
  except
    on E: Exception do
    begin
      Log.Error('TFrameViewDMXLibrary.Fill - An exception occurs: '+E.Message);
      Log.Error('Path: '+GetDMXLibraryFolder, 1);
      ShowMess('Error while reading the fixture library'+LineEnding+
                GetDMXLibraryFolder+LineEnding+E.Message, SClose, mtError);
    end;
  end;
end;

procedure TFrameViewDMXLibrary.UpdateSelectedAfterEdition;
var n, n1: TTreeNode;
  modes: TStringArray;
  i: Integer;
  f: string;
begin
  n := TV.Selected;
  if n = NIL then exit;
  if ItsAMode(n) then n := n.Parent;
  if not ItsAFile(n) then exit;

  f := IncludeTrailingPathDelimiter(AbsolutePathForNode(n));
  f := f + ChangeFileExt(n.Text, DMX_LIBRARY_FILE_EXTENSION);

  // add each modes
  modes := GetFixtureModeNames(f);
  n.DeleteChildren;
  for i:=0 to High(modes) do begin
    n1 := TV.Items.AddChild( n, modes[i]);
    n1.SelectedIndex := 3;
    n1.ImageIndex := 3;
  end;
  n.Expand(False);
  TV.Selected := n;
  TV.MakeSelectionVisible;
end;

end.

