unit UnitPatchNotes;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, JawsCtrls,
  g2_classes, g2_usb, g2_database, DOM, XMLRead, XMLWrite;

type
  TfrmPatchNotes = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure AEdit1Change(Sender: TObject);
  private
    { Private declarations }
    FDisableControls : boolean;
  public
    { Public declarations }
    procedure LoadIniXML;
    procedure UpdateControls;
  end;

var
  frmPatchNotes: TfrmPatchNotes;

implementation
uses
  UnitG2Editor;

{$R *.dfm}

{ TfrmPatchNotes }

procedure TfrmPatchNotes.AEdit1Change(Sender: TObject);
var G2 : TG2;
begin
  if FDisableControls then
    exit;

  G2 := frmG2Main.SelectedG2;
  if not assigned(G2) then
    exit;

  G2.SelectedPatch.MessSetPatchNotes( Memo1.Lines);
end;

procedure TfrmPatchNotes.FormCreate(Sender: TObject);
begin
  FDisableControls := False;

  LoadIniXML;
end;

procedure TfrmPatchNotes.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

procedure TfrmPatchNotes.LoadIniXML;
var Doc : TXMLDocument;
    RootNode : TDOMNode;
    FormSettingsNode : TXMLFormSettingsType;
begin
  if not FileExists('G2_editor_ini.xml') then
    exit;

  Doc := TXMLDocument.Create;
  try
    ReadXMLFile( Doc, 'G2_editor_ini.xml');

    RootNode := Doc.FindNode('G2_Editor_settings');
    if assigned(RootNode) then begin
      FormSettingsNode := TXMLFormSettingsType(RootNode.FindNode('PatchNotesForm'));
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

procedure TfrmPatchNotes.UpdateControls;
var G2 : TG2;
    OldSelStart : integer;
begin
  exit; // Doesn't work yet, something goes wrong with carriage returns...

{  FDisableControls := True;
  try
    G2 := frmG2Main.SelectedG2;
    if not assigned(G2) then
      exit;

    OldSelStart := DMemo1.SelStart;
    G2.SelectedPatch.PatchNotes.GetLines( DMemo1.Lines);
    DMemo1.SelStart := OldSelStart;
  finally
    FDisableControls := False;
  end;}

end;

end.
