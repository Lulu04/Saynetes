unit frame_viewdmxlibrary;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, LCLType, Menus,
  LCLTranslator, ExtCtrls, u_list_dmxuniverse;

type

  TMoveItemEvent=procedure(Sender: TObject; aNode: TTreeNode; const aTargetPath: string; var Accept: boolean) of object;
  TSelectionChangeEvent=procedure(Sender: TObject; const aFixLocation: TFixtureLibraryLocation) of object;

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
    PopupMenu1: TPopupMenu;
    Separator1: TMenuItem;
    TV: TTreeView;
    procedure MIDevelopClick(Sender: TObject);
    procedure MINewFolderClick(Sender: TObject);
    procedure MIDeleteFolderClick(Sender: TObject);
    procedure MIDeleteFixtureClick(Sender: TObject);
    procedure MIRenameFixtureClick(Sender: TObject);
    procedure MIRenameFolderClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure TVClick(Sender: TObject);
    procedure TVDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure TVDragOver(Sender, Source: TObject; {%H-}X, {%H-}Y: Integer; {%H-}State: TDragState; var Accept: Boolean);
    procedure TVSelectionChanged(Sender: TObject);
    procedure TVStartDrag(Sender: TObject; var {%H-}DragObject: TDragObject);
  private
    FOnMoveItem: TMoveItemEvent;
    FOnSelectionChange: TSelectionChangeEvent;
    FOnStartDragFixture: TNotifyEvent;
    FUserChangeEnabled: boolean;
    function GetSelectedFixtureLocation: TFixtureLibraryLocation;
    procedure SetUserChangeEnabled(AValue: boolean);
    function SortProc( Node1, Node2: TTreeNode ): integer;
    procedure FillTreeViewWithLibraryContent;
    function RelativePathForNode(aNode: TTreeNode): string;
    function AbsolutePathForNode(aNode: TTreeNode): string;
    function ItsTheRoot( aNode: TTreeNode ): boolean;
  public
    procedure EraseBackground({%H-}DC: HDC); override;
    // gives '' if its a folder or no selection
    function GetSelectedFixtureFileName: string;
    // return '' if a mode is not selected
    function GetSelectedFixtureMode: string;

    procedure Fill;
    function ItsAFolder(aNode: TTreeNode): boolean;
    function ItsAFile(aNode: TTreeNode): boolean;
    function ItsAMode(aNode: TTreeNode): boolean;

    function SelectedIsFile: boolean;
    function SelectedIsMode: boolean;

    procedure SetSelected(const aNodeText: string);

    // use this property only if function SelectedIsMode return True
    property SelectedFixtureLocation: TFixtureLibraryLocation read GetSelectedFixtureLocation;
    property UserChangeEnabled: boolean read FUserChangeEnabled write SetUserChangeEnabled;
    property OnSelectionChange: TSelectionChangeEvent read FOnSelectionChange write FOnSelectionChange;
    property OnMoveItem: TMoveItemEvent read FOnMoveItem write FOnMoveItem;
    property OnStartDragFixture: TNotifyEvent read FOnStartDragFixture write FOnStartDragFixture;
  end;

implementation

uses u_resource_string, u_userdialogs, u_common, u_logfile, u_apputils,
  u_dmx_util, utilitaire_fichier, LazFileUtils, Dialogs;

{$R *.lfm}

{ TFrameViewDMXLibrary }

procedure TFrameViewDMXLibrary.TVDragDrop(Sender, Source: TObject; X, Y: Integer);
var SourceNode, TargetNode: TTreeNode;
  SourcePath, TargetPath: string;
  Accept: boolean;
begin
 if Sender <> Source then exit; // moves only inside TTreeView

 SourceNode := TV.Selected;
 TargetNode := TV.GetNodeAt(x,y);
 if TargetNode = NIL then
   TargetNode := TV.Items.GetFirstNode;
 if (SourceNode = NIL) or ( TargetNode = NIL) or (SourceNode = TargetNode) then
   exit;

 SourcePath := AbsolutePathForNode(TV.Selected);
 if ItsTheRoot( TargetNode ) then
   TargetPath := GetAppDMXLibraryFolder
 else if ItsAFolder( TargetNode ) then
        TargetPath := AbsolutePathForNode( TargetNode )
      else
        TargetPath := ExtractFilePath( AbsolutePathForNode( TargetNode ));

 if ExtractFilePath(SourcePath) = ExtractFilePath(TargetPath) then
   exit; // move in same folder -> do nothing

 if ItsAFolder( SourceNode ) then
   if ParentPath( SourcePath ) = TargetPath then
     exit;

  if FOnMoveItem = NIL then exit;
  Accept := TRUE;
  FOnMoveItem(Self, SourceNode, TargetPath, Accept);
  if not Accept then exit;
 try
  if ItsAFile( SourceNode ) then begin
    // move a file
    if CopieFichier(SourcePath, TargetPath) then
      SupprimeFichier(SourcePath);
    //DeplaceFichier( SourcePath, TargetPath  )  // move a file
  end else begin
    // move a folder
    CopieRepertoire( SourcePath, TargetPath, TRUE, TRUE );
    SupprimeRepertoire( SourcePath );
  end;
 except
  // showmess('erreur');
 end;
 FillTreeViewWithLibraryContent;
 TV.CustomSort( @SortProc );
end;

procedure TFrameViewDMXLibrary.MINewFolderClick(Sender: TObject);
var fol: string;
 new: string;
 i: integer;
begin
  fol := '';
  if UserInputFileName(SNameOfTheNewFolder, SOk, SCancel, fol, mtConfirmation) <> mrOk then
    exit;

  new := ConcatPaths([GetAppDMXLibraryFolder, fol]);
  if TV.Selected <> nil then
  begin
    if ItsAFolder( TV.Selected ) then
      new := ConcatPaths([AbsolutePathForNode( TV.Selected ), fol])
    else
      new := ConcatPaths([ExtractFilePath( AbsolutePathForNode( TV.Selected )), fol ]);
  end;
  if not CreerRepertoire( new ) then
  begin
    ShowMess( SEnableToCreateTheNewFolder+lineending+new, SOk, mtCustom );
    exit;
  end;
 // on re-scan la bibliothèque
 FillTreeViewWithLibraryContent;

 // select the new created folder
 new := UpCase( new );
 for i:=0 to TV.Items.Count-1 do
  if TV.Items.Item[i].Text = new then
  begin
    TV.Items.Item[i].Selected := TRUE;
    TV.Items.Item[i].MakeVisible;
  end;
 TV.CustomSort( @SortProc );
end;

procedure TFrameViewDMXLibrary.MIDevelopClick(Sender: TObject);
var n: TTreeNode;
begin
  if TV.Items.Count = 0 then exit;

  n := TV.Items.GetFirstNode;
  if Sender = MIDevelop then begin
    for n in TV.Items do if n.ImageIndex = 1 then n.Expand(False);
  end else begin
    for n in TV.Items do if n.ImageIndex = 1 then n.Collapse(True);
  end;
end;

procedure TFrameViewDMXLibrary.MIDeleteFolderClick(Sender: TObject);
var fol: string;
begin
 if TV.Selected = nil then exit;
 if not ItsAFolder( TV.Selected ) then exit;
 fol := AbsolutePathForNode( TV.Selected );

 if AskConfirmation( SYouWillLoseAllTheContentOfTheFolder+' ' + TV.Selected.Text +
                     lineending, SContinue, SNo, mtWarning)=mrCancel then exit;

 if not SupprimeRepertoire( fol ) then
 begin
   ShowMess( SFailedToDeleteTheFolder+lineending+fol, SOk, mtError);
   exit;
 end;
 // re-scan the library
 FillTreeViewWithLibraryContent;
 TV.CustomSort( @SortProc );
end;

procedure TFrameViewDMXLibrary.MIDeleteFixtureClick(Sender: TObject);
var filename: string;
begin
 if TV.Selected = NIL then exit;
 if not ItsAFile( TV.Selected) then exit;
 if AskConfirmation(SYouAreAboutToDelete+' ' + TV.Selected.Text +
                 lineending, SContinue, SNo, mtConfirmation)=mrCancel then exit;

 filename := AbsolutePathForNode(TV.Selected);
 if not DeleteFile(filename) then// SupprimeFichier( filename ) then
 begin
   showmess( SFailedToRemoveTheFixture + lineending + TV.Selected.Text, SOk, mtError );
   exit;
 end;

 // re-scan the library
 FillTreeViewWithLibraryContent;
 TV.CustomSort(@SortProc);
end;

procedure TFrameViewDMXLibrary.MIRenameFixtureClick(Sender: TObject);
var n, old: string;
 path: string;
begin
 if TV.Selected = NIL then exit;
 if ItsAFolder( TV.Selected) then exit;


 old := AbsolutePathForNode(TV.Selected);
 path := ExtractFilePath(old);

 n := TV.Selected.Text;
 if UserInputFileName(SNewName, SOk, SCancel, n, mtConfirmation) = mrOk then
 begin
   n := ChangeFileExt(n, DMX_LIBRARY_FILE_EXTENSION);
   n := ConcatPaths([path,n]);
   if not RenommeFichier( old, n) then
     ShowMess(SFailedToRenameTheFixture+lineending+ExtractFilename(old), SOk, mtError)
   else
     TV.Selected.Text := ChangeFileExt(ExtractFilename(n), '');
 end;
end;

procedure TFrameViewDMXLibrary.MIRenameFolderClick(Sender: TObject);
var n, old, ne: string;
begin
 if TV.Selected = NIL then exit;
 if ItsAFile( TV.Selected) then exit;

 old := AbsolutePathForNode(TV.Selected);
 n := TV.Selected.Text;
 if UserInputFileName(SNewName, SOk, SCancel, n, mtConfirmation)=mrOk then
 begin
   ne := ConcatPaths([RepertoireParent(old),n]);
   if not RenommerRepertoire( old, ne) then
     ShowMess(SFailedToRenameTheFolder+lineending+old, SOk, mtError)
   else
     TV.Selected.Text := n;
 end;
end;

procedure TFrameViewDMXLibrary.PopupMenu1Popup(Sender: TObject);
begin
  MIDeleteFolder.Enabled := ItsAFolder(TV.Selected);
  MIRenameFolder.Enabled := MIDeleteFolder.Enabled;

  MIDeleteFixture.Enabled := ItsAFile(TV.Selected);
  MIRenameFixture.Enabled := MIDeleteFixture.Enabled;
end;

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

procedure TFrameViewDMXLibrary.TVDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
 Accept := ( Sender = TV ) and ( Source = TV ) and FUserChangeEnabled;
end;

procedure TFrameViewDMXLibrary.TVSelectionChanged(Sender: TObject);
begin
  if FOnSelectionChange = NIL then exit;
  FOnSelectionChange(Self, GetSelectedFixtureLocation);
end;

procedure TFrameViewDMXLibrary.TVStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
  if not ItsAFile(TV.Selected) then exit;
  if FOnStartDragFixture <> NIL then
    FOnStartDragFixture(Self);
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
// Result := AnsiStrIComp( PChar(UTF8ToSys(Node1.Text)), PChar(UTF8ToSys(Node2.Text)) );
end;

function TFrameViewDMXLibrary.GetSelectedFixtureFileName: string;
var n: TTreeNode;
begin
  Result := '';
  n := TV.Selected;
  if n = NIL then exit;
  if ItsAMode(n) then n := n.Parent;
  if not ItsAFile(n) then exit;

  Result := AbsolutePathForNode(n);
end;

function TFrameViewDMXLibrary.GetSelectedFixtureMode: string;
begin
  Result := '';
  if TV.Selected = NIL then exit;
  if not ItsAMode(TV.Selected) then exit;

  Result := TV.Selected.Text;
end;

procedure TFrameViewDMXLibrary.SetUserChangeEnabled(AValue: boolean);
begin
  if FUserChangeEnabled = AValue then Exit;
  FUserChangeEnabled := AValue;
  if not FUserChangeEnabled then
     TV.PopupMenu := NIL
  else
    TV.PopupMenu := PopupMenu1;
end;

function TFrameViewDMXLibrary.GetSelectedFixtureLocation: TFixtureLibraryLocation;
var n: TTreeNode;
begin
  n := TV.Selected;

  Result.InitDefault;
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
  procedure ScanFolder ( folder: string; aNode: TTreeNode );
  var sr: TSearchRec;
    n, n1: TTreeNode;
    modes: TStringArray;
    i: Integer;
  begin
   if LazFileUtils.FindFirstUTF8( folder + DirectorySeparator + '*', faAnyFile, sr ) = 0 then
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
         n.Collapse( True );
         ScanFolder( folder + DirectorySeparator + Sr.Name, n );
       end;
     end
     else if LowerCase( ExtractFileExt( sr.Name )) = DMX_LIBRARY_FILE_EXTENSION then
     begin
       // one found a dmx fixture file
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
   LazFileUtils.FindCloseUTF8( Sr );
  end;

begin
 TV.BeginUpdate;
 TV.Items.Clear; // clear the TreeView
 with TV.Items.AddFirst( nil, SDMXLibrary ) do
   begin
    Selected := true;
    ImageIndex := 0;
    SelectedIndex := 0;
   end;
 ScanFolder( GetAppDMXLibraryFolder, TV.Items.GetFirstNode );
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
   Result := ConcatPaths([GetAppDMXLibraryFolder, RelativePathForNode( aNode )]);
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
      Log.Error('Path: '+GetAppDMXLibraryFolder, 1);
      ShowMess('Error while reading the fixture library'+LineEnding+
                GetAppDMXLibraryFolder+LineEnding+E.Message, SClose, mtError);
    end;
  end;
end;

end.

