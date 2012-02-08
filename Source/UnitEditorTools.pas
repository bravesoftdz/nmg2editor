unit UnitEditorTools;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ExtCtrls, g2_types, StdCtrls,
  ImgList, g2_database, g2_graph, DOM, XMLRead, XMLWrite;

type
  TfrmEditorTools = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    Panel14: TPanel;
    Panel15: TPanel;
    Panel16: TPanel;
    Panel17: TPanel;
    Panel18: TPanel;
    Panel19: TPanel;
    Panel20: TPanel;
    Panel21: TPanel;
    Panel22: TPanel;
    Panel23: TPanel;
    Panel24: TPanel;
    Panel25: TPanel;
    btCablesRed: TG2GraphButtonText;
    btCablesBlue: TG2GraphButtonText;
    btCablesYellow: TG2GraphButtonText;
    btCablesOrange: TG2GraphButtonText;
    btCablesGreen: TG2GraphButtonText;
    btCablesPurple: TG2GraphButtonText;
    btCablesWhite: TG2GraphButtonText;
    Label1: TLabel;
    procedure FormShow(Sender: TObject);
    procedure PanelClick(Sender: TObject);
    procedure btCablesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FDisableControls : boolean;
    procedure LoadIniXML;
    procedure UpdateControls;
  end;

var
  frmEditorTools: TfrmEditorTools;

implementation

{$R *.dfm}

uses UnitG2Editor;

procedure TfrmEditorTools.FormCreate(Sender: TObject);
begin
  LoadIniXML;
end;

procedure TfrmEditorTools.LoadIniXML;
var Doc : TXMLDocument;
    RootNode : TDOMNode;
    PatchManagerSettingsNode : TXMLPatchManagerSettingsType;
    FormSettingsNode : TXMLFormSettingsType;
begin
  Doc := TXMLDocument.Create;
  try
    ReadXMLFile( Doc, 'G2_editor_ini.xml');

    RootNode := Doc.FindNode('G2_Editor_settings');
    if assigned(RootNode) then begin
      FormSettingsNode := TXMLFormSettingsType(RootNode.FindNode('EditorToolsForm'));
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


procedure TfrmEditorTools.btCablesClick(Sender: TObject);
begin
  if FDisableControls then
    exit;

  frmG2Main.G2.SelectedPatch.PatchDescription.RedVisible := btCablesRed.Value;
  frmG2Main.G2.SelectedPatch.PatchDescription.BlueVisible := btCablesBlue.Value;
  frmG2Main.G2.SelectedPatch.PatchDescription.YellowVisible := btCablesYellow.Value;
  frmG2Main.G2.SelectedPatch.PatchDescription.OrangeVisible := btCablesOrange.Value;
  frmG2Main.G2.SelectedPatch.PatchDescription.GreenVisible := btCablesGreen.Value;
  frmG2Main.G2.SelectedPatch.PatchDescription.PurpleVisible := btCablesPurple.Value;
  frmG2Main.G2.SelectedPatch.PatchDescription.WhiteVisible := btCablesWhite.Value;
  frmG2Main.sbFX.Invalidate;
  frmG2Main.sbVA.Invalidate;
end;

procedure TfrmEditorTools.FormShow(Sender: TObject);
var i : integer;
begin
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TPanel then
      (Components[i] as TPanel).Color := ModuleColors[(Components[i] as TPanel).Tag];


end;

procedure TfrmEditorTools.PanelClick(Sender: TObject);
begin
  with Sender as TPanel do
    frmG2Main.SetSelectedModuleColor( (Sender as TPanel).Tag);
end;

procedure TfrmEditorTools.UpdateControls;
begin
  FDisableControls := True;
  try
    btCablesRed.Value := frmG2Main.G2.SelectedPatch.PatchDescription.RedVisible;
    btCablesBlue.Value := frmG2Main.G2.SelectedPatch.PatchDescription.BlueVisible;
    btCablesYellow.Value := frmG2Main.G2.SelectedPatch.PatchDescription.YellowVisible;
    btCablesOrange.Value := frmG2Main.G2.SelectedPatch.PatchDescription.OrangeVisible;
    btCablesGreen.Value := frmG2Main.G2.SelectedPatch.PatchDescription.GreenVisible;
    btCablesPurple.Value := frmG2Main.G2.SelectedPatch.PatchDescription.PurpleVisible;
    btCablesWhite.Value := frmG2Main.G2.SelectedPatch.PatchDescription.WhiteVisible;

  finally
    FDisableControls := False;
  end;
end;

end.
