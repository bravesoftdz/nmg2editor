unit UnitPatchBuffer;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, g2_graph, g2_file, g2_mess, g2_usb,
  g2_midi, g2_classes, Vcl.ExtCtrls, Vcl.ActnList,
  Vcl.PlatformDefaultStyleActnCtrls, Vcl.ActnMan, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TfrmPatchBuffer = class(TForm)
    lvPatchList: TListView;
    Panel1: TPanel;
    Button1: TButton;
    ActionManager1: TActionManager;
    aFormClose: TAction;
    Splitter1: TSplitter;
    Panel2: TPanel;
    G2Buffer: TG2;
    sbVaBuffer: TG2GraphScrollBox;
    sbFXBuffer: TG2GraphScrollBox;
    Splitter2: TSplitter;
    OpenDialog1: TOpenDialog;
    aPatchAdd: TAction;
    Button2: TButton;
    aSelectVA: TAction;
    aSelectFX: TAction;
    aLoadPatch: TAction;
    aCopySelectionToClipboard: TAction;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    procedure aFormCloseExecute(Sender: TObject);
    procedure aPatchAddExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure aSelectVAExecute(Sender: TObject);
    procedure aSelectFXExecute(Sender: TObject);
    procedure aLoadPatchExecute(Sender: TObject);
    procedure aCopySelectionToClipboardExecute(Sender: TObject);
    procedure lvPatchListSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmPatchBuffer: TfrmPatchBuffer;

implementation

{$R *.dfm}

procedure TfrmPatchBuffer.aCopySelectionToClipboardExecute(Sender: TObject);
begin
//
end;

procedure TfrmPatchBuffer.aFormCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmPatchBuffer.aLoadPatchExecute(Sender: TObject);
begin
//
end;

procedure TfrmPatchBuffer.aPatchAddExecute(Sender: TObject);
var Patch : TG2GraphPatch;
    FileStream : TFileStream;
    i : integer;
begin
  OpenDialog1.Options := [ofAllowMultiSelect, ofFileMustExist];
  OpenDialog1.Filter := 'patch files (*.pch2)|*.pch2|extended patch files (*.pch2x)|*.pch2x|sysex files (*.syx)|*.syx|bin files (*.bin)|*.bin';
  if OpenDialog1.Execute then begin
    for i := 0 to OpenDialog1.Files.Count - 1 do begin
      Patch := TG2GraphPatch.Create(self);
      try
        Filestream := TFileStream.Create( OpenDialog1.Files[i], fmOpenRead);
        try
          Patch.LoadFromFile( FileStream, nil);
          lvPatchList.AddItem( ExtractFileName(OpenDialog1.Files[i]), Patch);
        finally
          FileStream.Free;
        end;
      except on E:Exception do begin
          Patch.Free;
        end;
      end;
    end;
  end;
end;

procedure TfrmPatchBuffer.aSelectFXExecute(Sender: TObject);
begin
//
end;

procedure TfrmPatchBuffer.aSelectVAExecute(Sender: TObject);
begin
//
end;

procedure TfrmPatchBuffer.FormCreate(Sender: TObject);
begin
  G2Buffer.LoadModuleDefs('');
end;

procedure TfrmPatchBuffer.lvPatchListSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var ListItem : TListItem;
begin
  if lvPatchList.ItemIndex <> -1 then begin
    ListItem := lvPatchList.Items[ lvPatchList.ItemIndex];
    G2Buffer.SelectedSlot.SendSetPatchMessage( ListItem.Caption, TG2GraphPatch(ListItem.Data));
  end;
end;

end.
