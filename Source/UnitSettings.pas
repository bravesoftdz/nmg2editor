unit UnitSettings;

{$I delphi_version.inc}

interface
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  IdBaseComponent, IdComponent, IdUDPBase,
  IdUDPClient, StdCtrls, IdSocketHandle, IdUDPServer, IdGlobal, ExtCtrls,
  g2_types, g2_database, g2_file, OSCUtils, ComCtrls, DOM, XMLRead, XMLWrite;

type
  TfrmSettings = class(TForm)
    Memo1: TMemo;
    IdUDPServer1: TIdUDPServer;
    Button2: TButton;
    Panel1: TPanel;
    eOSCServerIP: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    eOSCHostPort: TEdit;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    ePort: TEdit;
    Label3: TLabel;
    eHost: TEdit;
    Label4: TLabel;
    TabSheet3: TTabSheet;
    eRootFolder: TEdit;
    Label5: TLabel;
    Button1: TButton;
    procedure Button2Click(Sender: TObject);
    procedure IdUDPServer1Status(ASender: TObject; const AStatus: TIdStatus;
      const AStatusText: string);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure LoadIniXML;
    procedure udpServerDeviceUDPRead(AThread: TIdUDPListenerThread; AData: TIdBytes; ABinding: TIdSocketHandle);
  end;

var
  frmSettings: TfrmSettings;

implementation

{$R *.dfm}

uses UnitG2Editor;

procedure TfrmSettings.FormCreate(Sender: TObject);
begin
  LoadIniXML;
  IdUDPServer1.OnUDPRead := udpServerDeviceUDPRead;
end;

procedure TfrmSettings.FormShow(Sender: TObject);
begin
  eHost.Text := frmG2Main.G2.Host;
  ePort.Text := IntTostr(frmG2Main.G2.Port);
end;

procedure TfrmSettings.LoadIniXML;
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
        eRootFolder.Text := PatchManagerSettingsNode.BaseFolder;
      end;

      FormSettingsNode := TXMLFormSettingsType(RootNode.FindNode('SettingsForm'));
      if assigned(FormSettingsNode) then begin
        Left := FormSettingsNode.PosX;
        Top := FormSettingsNode.PosY;
        Width := FormSettingsNode.SizeX;
        Height := FormSettingsNode.SizeY;
        Visible := FormSettingsNode.Visible;
      end;
    end;
  finally
    Doc.Free;
  end;
end;


procedure TfrmSettings.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  frmG2Main.G2.Host := eHost.Text;
  frmG2Main.G2.Port := StrToInt(ePort.Text);
end;

procedure TfrmSettings.udpServerDeviceUDPRead(AThread: TIdUDPListenerThread; AData: TIdBytes; ABinding: TIdSocketHandle);
var i, p, c, b, KnobIndex : integer;
    address : AnsiString;
    Knob : TKnob;
    OscBundle : TOSCBundle;
    OscMessage : TOSCMessage;
    OscPacket : TOSCPacket;
begin
  OscPacket := TOSCPacket.Unpack(@AData[0], Length(AData));
  try
    if OscPacket is TOSCBundle then begin
      //dump_buffer( Memo1, AData, Length(AData));

      for p := 0 to 4 do
        for c := 0 to 2 do
          for b := 0 to 7 do begin
            address := AnsiString('/' + PARAM_PAGE_NAMES[p] + '/' + IntToStr(c) + '/' + IntToStr(b));
            OscMessage := (OscPacket as TOSCBundle).MatchAddress(Address);
              if assigned(OscMessage) then begin
                OscMessage.Decode;
                KnobIndex := (c-1) * 8 + p * 8 * 3 + (b-1);
                Knob := frmG2Main.G2.SelectedPatch.GetKnob( KnobIndex);
                if assigned(Knob) and (Knob.IsAssigned = 1) then
                for i := 0 to OscMessage.ArgumentCount - 1 do begin
                  Memo1.Lines.Add(string(Address + ':' + OscMessage.Argument[i]));
                  Knob.Parameter.SetParameterValue( trunc(127 * StrToFloat(string(OscMessage.Argument[i]))));
                end;
              end;
          end;
    end;
    if OscPacket is TOSCMessage then begin
      frmG2Main.G2.dump_buffer( AData, Length(AData));
    end;
  finally
     OSCPacket.Free;
  end;
end;

procedure TfrmSettings.Button2Click(Sender: TObject);
begin
  if IdUDPServer1.Active then
    IdUDPServer1.Active := False
  else
    IdUDPServer1.Active := True;
end;

procedure TfrmSettings.IdUDPServer1Status(ASender: TObject; const AStatus: TIdStatus; const AStatusText: string);
begin
  Memo1.Lines.Add( AStatusText)
end;

end.
