unit UnitPatchManager;

{$I delphi_version.inc}

interface
uses
{$IFDEF G2_VER200_up}
  PlatformDefaultStyleActnCtrls,
{$ELSE}
  XPStyleActnCtrls,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Forms, Dialogs, StdCtrls, ActnList,  ExtCtrls, ComCtrls, Tabs,
  g2_types, g2_database, g2_file, g2_classes,
  ActnMan, Controls, DOM, XMLRead, XMLWrite;

const
  MAXBUFFER = 4096;

  COL_NAME = 0;
  COL_CATEGORY = 1;
  COL_SLOT = 2;

  COL_FILE = 0;
  COL_DATE = 1;
  COL_PATH = 2;

type
  TSearchThread = class(TThread)
  private
    { Private declarations }
  protected
    procedure Execute; override;
    procedure AddFile;
    //procedure ShowFile;
  public
    FCurrSr : TSearchRec;
    FCurrPath, FPath, FMask, FSearch : string;
    FPos : integer;
  end;

  TfrmPatchManager = class(TForm)
    ActionManager1: TActionManager;
    aReadDir: TAction;
    aSearch: TAction;
    aLoadPatch: TAction;
    lvExternalPatch: TListView;
    TabControl1: TTabControl;
    aShowPerfs: TAction;
    aShowPatches: TAction;
    lvInternal: TListView;
    aRestore: TAction;
    aReadDirPerf: TAction;
    lvExternalPerf: TListView;
    aLoadPerf: TAction;
    procedure aReadDirExecute(Sender: TObject);
    procedure aSearchExecute(Sender: TObject);
    procedure aLoadPatchExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvExternalPatchColumnClick(Sender: TObject; Column: TListColumn);
    procedure lvExternalCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure lvExternalPatchKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure aShowPerfsExecute(Sender: TObject);
    procedure aShowPatchesExecute(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
    procedure lvInternalColumnClick(Sender: TObject; Column: TListColumn);
    procedure lvInternalCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure aRestoreExecute(Sender: TObject);
    procedure lvInternalKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure lvExternalPerfKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lvExternalPerfColumnClick(Sender: TObject; Column: TListColumn);
    procedure aLoadPerfExecute(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
    FSearchThread : TSearchThread;
    FExternalSortCol : integer;
    FInternalSortCol : integer;
    procedure AddFile( Path, Filename: string; Datum : TDateTime);
    procedure AddSlot( BankItem : TBankItem);
    //procedure ShowFile(path, filename: string);
    procedure OnSearchThreadTerminate(Sender: TObject);
    //procedure OpenFile;
    procedure LoadIniXML;
  end;

var
  frmPatchManager: TfrmPatchManager;

implementation

{$R *.dfm}

uses UnitG2Editor, UnitSettings;

{ TSearchThread }

function FindFirstOccurance(content: string; var index2: integer; b : char): boolean;
begin
  while (index2 <= Length(Content)) and (UpperCase(content[index2]) <> UpperCase(b)) do
    inc(index2);
  result := (index2 <= Length(Content));
end;

function FindNextOccurrance(content, sText: string; index1: integer; index2: integer): boolean;
begin
  if index1 > length(sText) then
  // end of text mask, return a match
    result := true
  else
    if sText[index1] = '*' then
    begin
    // if nothing follows the *, we are at the end of the text mask
      if index1+1 <= Length(sText) then
      begin
      // skip the buffer until the rest of the mask is matched
        if FindFirstOccurance(content, index2, sText[index1+1]) then
          result := FindNextOccurrance(content, sText, index1+1, index2)
        else
        // no match of the rest of the mask has been found, return false
          result := false;
      end
      else
      // return a match
        result := true;
    end
    else
    begin
    // perform a mask-buffer byte comparison
      if UpperCase(content[index2]) = UpperCase(sText[index1]) then
      // if equal, test the next byte
        result := FindNextOccurrance(content, sText, index1+1, index2+1)
      else
      // if not equal, return false
        result := false;
    end;
end;

function Match(content, sText: string): integer;
var i : integer;
    found : boolean;
begin
  found := false;
  if sText = '' then begin
    result := -1;
    exit;
  end;

  i := 1;
  while sText[1] = '*' do
   sText := copy(sText, 2, length(sText)-1);

  while not(found) and FindFirstOccurance(content, i, sText[1]) do
  begin
    if FindNextOccurrance(content, sText, 1, i) then
      found := true
    else
      inc(i);
  end;

  if found then
    result := i
  else
    result := -1;
end;


procedure TSearchThread.Execute;

  {procedure SearchFile(filename : string);
  var buffer : array[0..MAXBUFFER-1] of char;
      FileStream : TFileStream;
      BytesToRead, BytesRead, offset, pos : integer;
      i, l : integer;
      s : string;
  begin
    FileStream := TFileStream.Create(filename, fmOpenRead);
    try
      BytesToRead := MAXBUFFER div 2;

      offset := 0;
      pos := 0;
      BytesRead := 0;

      // Buffer legen
      for i := 0 to MAXBUFFER - 1 do
        buffer[i] := #0;

      repeat
        // Copieer 2e helft naar 1e helft
        for i := 0 to BytesToRead - 1 do
          buffer[i] := buffer[BytesToRead + i];

        // Lees in 2e helft van buffer
        BytesRead := FileStream.Read(buffer[BytesToRead], BytesToRead);

        l := Length(FSearch);
        // Komt de tekst voor?
        s := '';
        for i := BytesToRead - l to MAXBUFFER - 1 do begin
          if Length(s) < l then
            s := s + buffer[i]
          else begin
            s := copy(s, 2, l - 1) + buffer[i];
            if UpperCase(s) = UpperCase(FSearch) then begin
              FPos := offset + (i - BytesToRead) - l + 1;
              Synchronize(AddFile);
            end;
          end;
        end;

        offset := offset + BytesRead;

      until BytesRead < BytesToRead;

    finally
      FileStream.Free;
    end;
  end;}

  procedure SearchDir(path : string);
  var sr : TSearchRec;
  begin
    if FindFirst(path + '*.*', faAnyFile, sr) = 0 then begin
      repeat
        FCurrPath := path;
        FCurrSr := sr;
        if (sr.Attr and faDirectory) = 0 then begin
          // Geen subdirectory
          {if Match(sr.Name, FMask)<>-1 then begin
            Synchronize(ShowFile);
            SearchFile(path + sr.Name);
          end;}
          Synchronize(AddFile);
        end else begin
          // subdirectory
          if (sr.Name <> '.') and (sr.Name <> '..') then
            SearchDir(path + sr.Name + '\');
        end;

      until (FindNext(sr) <> 0) or Terminated;
      FindClose(sr);
    end;
  end;

begin
  { Place thread code here }
  SearchDir(FPath);
end;

procedure TfrmPatchManager.OnSearchThreadTerminate(Sender: TObject);
begin
  FSearchThread := nil;
end;

procedure TSearchThread.AddFile;
var sr : TSearchRec;
begin
  //if frmPatchManager.TabControl1.TabIndex = 0 then
  {$IFDEF G2_VER200_up}
    // Don't know exactly in what version this was changed
    frmPatchManager.AddFile(FCurrPath, FCurrSr.Name, FCurrSr.TimeStamp);
  {$ELSE}
    frmPatchManager.AddFile(FCurrPath, FCurrSr.Name, FileDateToDateTime(FCurrSr.Time));
  {$ENDIF}
end;

procedure TfrmPatchManager.aSearchExecute(Sender: TObject);
begin
  {if Length(ePath.Text)> 2 then begin

    ListView1.Clear;

    if ePath.Text[Length(ePath.Text)] <> '\'  then
      ePath.Text := ePath.Text + '\';

    SearchThread := TSearchThread.Create(True);
    SearchThread.FPath := ePath.Text;
    SearchThread.FSearch := eSearch.Text;
    SearchThread.FMask := eMask.Text;
    SearchThread.OnTerminate := OnSearchThreadTerminate;
    SearchThread.FreeOnTerminate := True;
    SearchThread.Resume;
    aSearchStart.Enabled := False;
    aSearchStop.Enabled := True;
    Invalidate;
  end;}
end;

procedure TfrmPatchManager.aLoadPatchExecute(Sender: TObject);
var G2 : TG2;
begin
  G2 := frmG2Main.SelectedG2;
  if assigned(G2) then
    G2.LoadFileStream( lvExternalPatch.Selected.SubItems[1] + lvExternalPatch.Selected.Caption);
  frmG2Main.SetFocus;
end;

procedure TfrmPatchManager.aLoadPerfExecute(Sender: TObject);
var G2 : TG2;
begin
  G2 := frmG2Main.SelectedG2;
  if assigned(G2) then
    G2.LoadFileStream( lvExternalPerf.Selected.SubItems[1] + lvExternalPerf.Selected.Caption);
  frmG2Main.SetFocus;
end;

procedure TfrmPatchManager.aRestoreExecute(Sender: TObject);
var BankItem : TBankItem;
    G2 : TG2;
begin
 BankItem := TBankItem(lvInternal.Selected.Data);
  G2 := frmG2Main.SelectedG2;
  if assigned(G2) then begin
    if BankItem.Mode = 1 then
      G2.Performance.SendRetrieveMessage( 4, BankItem.Bank, BankItem.Patch)
    else
      G2.Performance.SendRetrieveMessage( G2.SelectedSlotIndex, BankItem.Bank, BankItem.Patch);
  end;
  frmG2Main.SetFocus;
end;

procedure TfrmPatchManager.aReadDirExecute(Sender: TObject);
begin
  lvExternalPatch.Items.Clear;
  lvExternalPerf.Items.Clear;

  if Length(frmSettings.eRootFolder.Text)> 2 then begin
    if frmSettings.eRootFolder.Text[Length(frmSettings.eRootFolder.Text)] <> '\'  then
      frmSettings.eRootFolder.Text := frmSettings.eRootFolder.Text + '\';

    FSearchThread := TSearchThread.Create(True);
    FSearchThread.FPath := frmSettings.eRootFolder.Text;
    FSearchThread.OnTerminate := OnSearchThreadTerminate;
    FSearchThread.FreeOnTerminate := True;
{$IFDEF G2_VER200_up}
    FSearchThread.Start;
{$ELSE}
    FSearchThread.Resume;
{$ENDIF}
  end;
  Invalidate;
end;

function Pad( s : AnsiString; l : integer): AnsiString;
begin
  Result := s;
  while Length(Result) < l do
    Result := ' ' + Result;
end;

procedure TfrmPatchManager.aShowPatchesExecute(Sender: TObject);
var i : integer;
    G2 : TG2;
begin
  if assigned(FSearchThread) then
    FSearchThread.Terminate;

  lvInternal.Clear;
  lvExternalPerf.Visible := False;
  lvExternalPatch.Visible := False;
  lvInternal.Visible := True;
  lvInternal.Align := alClient;

  lvInternal.Clear;

  G2 := frmG2Main.SelectedG2;
  if assigned(G2) then begin
    if G2.USBActive then begin
      for i := 0 to G2.BankList.Count - 1 do begin
        if G2.BankList[i].Mode = 0 then
          AddSlot( G2.BankList[i]);
      end;
    end else begin
      // No usb, just fill the list with bank and slot no's

    end;
  end;
  lvInternal.AlphaSort;
end;

procedure TfrmPatchManager.aShowPerfsExecute(Sender: TObject);
var i : integer;
    G2 : TG2;
begin
  if assigned(FSearchThread) then
    FSearchThread.Terminate;

  lvInternal.Clear;
  lvExternalPerf.Visible := False;
  lvExternalPatch.Visible := False;
  lvInternal.Visible := True;
  lvInternal.Align := alClient;

  lvInternal.Clear;

  G2 := frmG2Main.SelectedG2;
  if assigned(G2) then begin
    for i := 0 to G2.BankList.Count - 1 do begin
      if G2.BankList[i].Mode = 1 then
        AddSlot( G2.BankList[i]);
    end;
  end;
  lvInternal.AlphaSort;
end;

procedure TfrmPatchManager.FormCreate(Sender: TObject);
begin
  LoadIniXML;
end;

procedure TfrmPatchManager.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE  then
    Close;
end;

procedure TfrmPatchManager.FormShow(Sender: TObject);
begin
  TabControl1Change(Self);
end;

procedure TfrmPatchManager.LoadIniXML;
var Doc : TXMLDocument;
    RootNode : TDOMNode;
    PatchManagerSettingsNode : TXMLPatchManagerSettingsType;
    FormSettingsNode : TXMLFormSettingsType;
begin
  if not FileExists('G2_editor_ini.xml') then
    exit;

  Doc := TXMLDocument.Create;
  try
    ReadXMLFile( Doc, 'G2_editor_ini.xml');

    RootNode := Doc.FindNode('G2_Editor_settings');
    if assigned(RootNode) then begin

      PatchManagerSettingsNode := TXMLPatchManagerSettingsType(RootNode.FindNode('PatchManagerSettings'));
      if assigned(PatchManagerSettingsNode) then begin
        FExternalSortCol := PatchManagerSettingsNode.ExternalSortCol;
        FInternalSortCol := PatchManagerSettingsNode.InternalSortCol;
        if PatchManagerSettingsNode.SelectedTab < TabControl1.Tabs.Count then
          TabControl1.TabIndex := PatchManagerSettingsNode.SelectedTab;
      end;

      FormSettingsNode := TXMLFormSettingsType(RootNode.FindNode('PatchManagerForm'));
      if assigned(FormSettingsNode) then begin
        SetFormPosition( self,
                         FormSettingsNode.PosX,
                         FormSettingsNode.PosY,
                         FormSettingsNode.SizeX,
                         FormSettingsNode.SizeY);
        Visible := FormSettingsNode.Visible;
      end;
    end;
  finally
    Doc.Free;
  end;
end;

procedure TfrmPatchManager.lvExternalPatchColumnClick(Sender: TObject; Column: TListColumn);
begin
  FExternalSortCol := Column.Index;
  lvExternalPatch.AlphaSort;
end;

procedure TfrmPatchManager.lvExternalPerfColumnClick(Sender: TObject; Column: TListColumn);
begin
  FExternalSortCol := Column.Index;
  lvExternalPerf.AlphaSort;
end;

procedure TfrmPatchManager.lvExternalCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
var intItem1,
    intItem2: string;
begin
   case FExternalSortCol of
   1 : begin
         intItem1 := Uppercase(Item1.SubItems[0]);
         intItem2 := Uppercase(Item2.SubItems[0]);
       end;
   2 : begin
         intItem1 := Uppercase(Item1.SubItems[1]);
         intItem2 := Uppercase(Item2.SubItems[1]);
       end;
   else begin
       intItem1 := Uppercase(Item1.Caption);
       intItem2 := Uppercase(Item2.Caption);
     end;
   end;

   if intItem1 < intItem2 then
     Compare := -1
   else
   if intItem1 > intItem2 then
     Compare := 1
   else // intItem1 = intItem2
     Compare := 0;
end;

procedure TfrmPatchManager.lvExternalPatchKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    aLoadPatchExecute(self);
end;

procedure TfrmPatchManager.lvExternalPerfKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    aLoadPerfExecute(self);
end;

procedure TfrmPatchManager.lvInternalColumnClick(Sender: TObject; Column: TListColumn);
begin
  FInternalSortCol := Column.Index;
  lvInternal.AlphaSort;
end;

procedure TfrmPatchManager.lvInternalCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
var
   intItem1,
   intItem2: string;
begin
   case FInternalSortCol of
   1 : begin
         intItem1 := Uppercase(Item1.SubItems[0]);
         intItem2 := Uppercase(Item2.SubItems[0]);
       end;
   2 : begin
         intItem1 := Uppercase(Item1.SubItems[1]);
         intItem2 := Uppercase(Item2.SubItems[1]);
       end;
   else begin
       intItem1 := Uppercase(Item1.Caption);
       intItem2 := Uppercase(Item2.Caption);
     end;
   end;

   if intItem1 < intItem2 then
     Compare := -1
   else
   if intItem1 > intItem2 then
     Compare := 1
   else // intItem1 = intItem2
     Compare := 0;
end;

procedure TfrmPatchManager.lvInternalKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    aRestoreExecute(self);
end;

procedure TfrmPatchManager.TabControl1Change(Sender: TObject);
begin
  if assigned(FSearchThread) then
    FSearchThread.Terminate;

  case TabControl1.TabIndex of
  0 : begin
        lvInternal.Visible := False;
        lvExternalPatch.Visible := False;
        lvExternalPerf.Visible := True;
        lvExternalPerf.Align := alClient;

        aReadDirExecute(self);
      end;
  1 : begin
        lvInternal.Visible := False;
        lvExternalPerf.Visible := False;
        lvExternalPatch.Visible := True;
        lvExternalPatch.Align := alClient;

        aReadDirExecute(self);
      end;
  2 : begin

        aShowPerfsExecute(self);
      end;
  3 : begin

        aShowPatchesExecute(self);
      end;
  end;
end;

procedure TfrmPatchManager.AddFile(path, filename: string; datum : TDateTime);
var ListItem : TListItem;
    ext : string;
begin
  ListItem := nil;
  ext := ExtractFileExt(filename);
  if lowercase(ext) = '.pch2' then
    ListItem := lvExternalPatch.Items.Add
  else
    if lowercase(ext) = '.prf2' then
      ListItem := lvExternalPerf.Items.Add;
  if assigned(ListItem) then begin
    ListItem.Caption := filename;
    ListItem.SubItems.Add(DateTimeToStr(datum));
    ListItem.SubItems.Add(path);
  end;
end;

procedure TfrmPatchManager.AddSlot( BankItem : TBankItem);
var ListItem : TListItem;
begin
  ListItem := lvInternal.Items.Add;
  ListItem.Caption := string(BankItem.PatchName);
  ListItem.SubItems.Add( CATEGORIES[ BankItem.Category]);
  ListItem.SubItems.Add( string(Pad(AnsiString(IntToStr( BankItem.Bank)), 2) + ':'
                              + Pad(AnsiString(IntToStr( BankItem.Patch)), 3)));
  ListItem.Data := BankItem;
end;

end.
