unit UnitSettings;

//  ////////////////////////////////////////////////////////////////////////////
//
//  Copyright (C) 2011 Bruno Verhue
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//  ////////////////////////////////////////////////////////////////////////////

//  ////////////////////////////////////////////////////////////////////////////
//
//  This unit needs the Delphi MIDI I/O components, download at https://bitbucket.org/h4ndy/midiio-dev/overview
//
//  ////////////////////////////////////////////////////////////////////////////

{$I delphi_version.inc}

interface
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  IdBaseComponent, IdComponent, IdUDPBase,
  IdUDPClient, StdCtrls, IdSocketHandle, IdUDPServer, IdGlobal, ExtCtrls,
  g2_types, g2_database, g2_file, g2_classes, OSCUtils, ComCtrls, DOM, XMLRead, XMLWrite,
  MidiDeviceComboBox, FileCtrl;

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
    bSelectRootFolder: TButton;
    TabSheet4: TTabSheet;
    Label6: TLabel;
    Label7: TLabel;
    cbIsServer: TCheckBox;
    cbMidiEnabled: TCheckBox;
    eTimerBroadcastLedMessages: TEdit;
    Label8: TLabel;
    cbMidiInDevices: TComboBox;
    cbMidiOutDevices: TComboBox;
    cbCtrlMidiInDevices: TComboBox;
    Label9: TLabel;
    cbCtrlMidiEnabled: TCheckBox;
    TabSheet5: TTabSheet;
    cbSlotStripColor: TColorBox;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    cbSlotStripInverseColor: TColorBox;
    cbSlotStripDisabledColor: TColorBox;
    cbHighLightColor: TColorBox;
    eCableThickness: TEdit;
    cbLogEnabled: TCheckBox;
    procedure Button2Click(Sender: TObject);
    procedure IdUDPServer1Status(ASender: TObject; const AStatus: TIdStatus;
      const AStatusText: string);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cbIsServerClick(Sender: TObject);
    procedure cbMidiEnabledClick(Sender: TObject);
    procedure bSelectRootFolderClick(Sender: TObject);
    procedure cbMidiInDevicesSelect(Sender: TObject);
    procedure cbMidiOutDevicesSelect(Sender: TObject);
    procedure cbCtrlMidiInDevicesSelect(Sender: TObject);
    procedure cbCtrlMidiEnabledClick(Sender: TObject);
    procedure cbLogEnabledClick(Sender: TObject);
    procedure cbSlotStripColorChange(Sender: TObject);
    procedure cbSlotStripInverseColorChange(Sender: TObject);
    procedure cbSlotStripDisabledColorChange(Sender: TObject);
    procedure cbHighLightColorChange(Sender: TObject);
  private
    { Private declarations }
    FDisableControls : boolean;
  public
    { Public declarations }
    procedure LoadIniXML;
    procedure udpServerDeviceUDPRead(AThread: TIdUDPListenerThread; AData: TIdBytes; ABinding: TIdSocketHandle);
    procedure UpdateControls;
  end;

var
  frmSettings: TfrmSettings;

implementation

{$R *.dfm}

uses UnitG2Editor;

procedure TfrmSettings.FormCreate(Sender: TObject);
var G2 : TG2;
begin
  G2 := frmG2Main.SelectedG2;
  if not assigned(G2) then
    exit;

  try
    G2.GetInDevices(cbMidiInDevices.Items);
  finally
    try
      G2.GetOutDevices(cbMidiOutDevices.Items);
    finally
      try
        G2.GetInDevices(cbCtrlMidiInDevices.Items);
      finally
        LoadIniXML;
        IdUDPServer1.OnUDPRead := udpServerDeviceUDPRead;
        FDisableControls := False;
      end;
    end;
  end;
end;

procedure TfrmSettings.FormShow(Sender: TObject);
begin
  UpdateControls;
end;

procedure TfrmSettings.LoadIniXML;
var Doc : TXMLDocument;
    RootNode, SynthNode : TDOMNode;
    mi, mo : integer;
    PatchManagerSettingsNode : TXMLPatchManagerSettingsType;
    MidiSettingsNode : TXMLMidiSettingsType;
    CtrlMidiSettingsNode : TXMLCtrlMidiSettingsType;
    FormSettingsNode : TXMLFormSettingsType;
    G2 : TG2;
    i : integer;
begin
  if not FileExists('G2_editor_ini.xml') then
    exit;

  Doc := TXMLDocument.Create;
  try
    ReadXMLFile( Doc, 'G2_editor_ini.xml');

    RootNode := Doc.FindNode('G2_Editor_settings');
    if assigned(RootNode) then begin

      for i := 0 to frmG2Main.FG2List.Count - 1 do begin

        SynthNode := RootNode.FindNode('G2_Synth_' + IntToStr(i+1));
        if assigned(SynthNode) then begin

          G2 := frmG2Main.FG2List[i] as TG2;

          if assigned(G2) then begin

            MidiSettingsNode := TXMLMidiSettingsType( SynthNode.FindNode('MIDI_settings'));
            if assigned( MidiSettingsNode) then begin
              cbMidiEnabled.Checked := False;

              mi := 0;
              while (mi<cbMidiInDevices.Items.Count) and (cbMidiInDevices.Items[mi] <> MidiSettingsNode.MidiInDevice) do
                inc(mi);

              mo := 0;
              while (mo<cbMidiOutDevices.Items.Count) and (cbMidiOutDevices.Items[mo] <> MidiSettingsNode.MidiOutDevice) do
                inc(mo);

              if (mi<cbMidiInDevices.Items.Count) and (mo<cbMidiOutDevices.Items.Count) then begin
                try
                  try
                    cbMidiInDevices.ItemIndex := mi;
                    G2.MidiInput.ChangeDevice( cbMidiInDevices.ItemIndex, False);
                    try
                      cbMidiOutDevices.ItemIndex := mo;
                      G2.MidiOutput.ChangeDevice( cbMidiOutDevices.ItemIndex, False);
                      G2.MidiEnabled := MidiSettingsNode.MidiEnabled;
                    except on E:Exception do begin
                        ShowMessage( E.Message);
                        G2.MidiEnabled := False;
                     end;
                    end;
                  except on E:Exception do begin
                      ShowMessage( E.Message);
                      G2.MidiEnabled := False;
                    end;
                  end;

                finally
                  cbMidiEnabled.Checked := G2.MidiEnabled;
                end;
              end else
                G2.MidiEnabled := False;
            end;
          end;
        end;
      end;

      CtrlMidiSettingsNode := TXMLCtrlMidiSettingsType( RootNode.FindNode('CTRL_MIDI_settings'));
      if assigned( CtrlMidiSettingsNode) then begin
        cbCtrlMidiEnabled.Checked := False;

        mi := 0;
        while (mi<cbCtrlMidiInDevices.Items.Count) and (cbCtrlMidiInDevices.Items[mi] <> CtrlMidiSettingsNode.CtrlMidiInDevice) do
          inc(mi);

        if (mi<cbCtrlMidiInDevices.Items.Count) then begin
          try
            try
              cbCtrlMidiInDevices.ItemIndex := mi;
              frmG2Main.CtrlMidiInput.ChangeDevice( cbCtrlMidiInDevices.ItemIndex, False);
              frmG2Main.CtrlMidiEnabled := CtrlMidiSettingsNode.CtrlMidiEnabled;
            except on E:Exception do begin
                ShowMessage( E.Message);
                frmG2Main.CtrlMidiEnabled := False;
              end;
            end;

          finally
            cbCtrlMidiEnabled.Checked := frmG2Main.CtrlMidiEnabled;
          end;
        end else
          frmG2Main.CtrlMidiEnabled := False;
      end;

      PatchManagerSettingsNode := TXMLPatchManagerSettingsType(RootNode.FindNode('PatchManagerSettings'));
      if assigned(PatchManagerSettingsNode) then begin
        eRootFolder.Text := String(PatchManagerSettingsNode.BaseFolder);
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

procedure TfrmSettings.cbMidiEnabledClick(Sender: TObject);
var G2 : TG2;
begin
  if FDisableControls then
    exit;

  G2 := frmG2Main.SelectedG2;
  if assigned(G2) then begin
    try
      G2.MidiEnabled := cbMidiEnabled.Checked;
    finally
      UpdateControls;
    end;
  end;
end;

procedure TfrmSettings.cbMidiInDevicesSelect(Sender: TObject);
var G2 : TG2;
begin
  if FDisableControls then
    exit;

  G2 := frmG2Main.SelectedG2;
  if assigned(G2) then begin
    if cbMidiInDevices.ItemIndex > -1  then
      G2.MidiInput.ChangeDevice( cbMidiInDevices.ItemIndex, cbMidiEnabled.Checked)
    else
      cbMidiInDevices.Text := '';
  end;
end;

procedure TfrmSettings.cbMidiOutDevicesSelect(Sender: TObject);
var G2 : TG2;
begin
  if FDisableControls then
    exit;

  G2 := frmG2Main.SelectedG2;
  if assigned(G2) then begin
    if cbMidiOutDevices.ItemIndex > -1  then
      G2.MidiOutput.ChangeDevice( cbMidiOutDevices.ItemIndex, cbMidiEnabled.Checked)
    else
      cbMidiOutDevices.Text := '';
  end;
end;

procedure TfrmSettings.cbCtrlMidiEnabledClick(Sender: TObject);
begin
  if FDisableControls then
    exit;

  try
    frmG2Main.CtrlMidiEnabled := cbCtrlMidiEnabled.Checked;
  finally
    UpdateControls;
  end;
end;

procedure TfrmSettings.cbCtrlMidiInDevicesSelect(Sender: TObject);
var G2 : TG2;
begin
  if FDisableControls then
    exit;

  if cbCtrlMidiInDevices.ItemIndex > -1  then
    frmG2Main.CtrlMidiInput.ChangeDevice( cbCtrlMidiInDevices.ItemIndex, cbCtrlMidiEnabled.Checked)
  else
    cbCtrlMidiInDevices.Text := '';
end;

procedure TfrmSettings.cbSlotStripColorChange(Sender: TObject);
begin
  if FDisableControls then
    exit;

  G_SlotStripColor := cbSlotStripColor.Selected;

  frmG2Main.SetEditorSettings( G_SlotStripColor, G_SlotStripInverseColor, G_SlotStripDisabledColor, G_HighLightColor, G_CableThickness);
end;

procedure TfrmSettings.cbSlotStripDisabledColorChange(Sender: TObject);
begin
  if FDisableControls then
    exit;

  G_SlotStripDisabledColor := cbSlotStripDisabledColor.Selected;

  frmG2Main.SetEditorSettings( G_SlotStripColor, G_SlotStripInverseColor, G_SlotStripDisabledColor, G_HighLightColor, G_CableThickness);
end;

procedure TfrmSettings.cbSlotStripInverseColorChange(Sender: TObject);
begin
  if FDisableControls then
    exit;

  G_SlotStripInverseColor := cbSlotStripInverseColor.Selected;

  frmG2Main.SetEditorSettings( G_SlotStripColor, G_SlotStripInverseColor, G_SlotStripDisabledColor, G_HighLightColor, G_CableThickness);
end;

procedure TfrmSettings.cbHighLightColorChange(Sender: TObject);
begin
  if FDisableControls then
    exit;

  G_HighLightColor := cbHighLightColor.Selected;

  frmG2Main.SetEditorSettings( G_SlotStripColor, G_SlotStripInverseColor, G_SlotStripDisabledColor, G_HighLightColor, G_CableThickness);
end;

procedure TfrmSettings.cbIsServerClick(Sender: TObject);
var G2 : TG2;
begin
  if FDisableControls then
    exit;

  G2 := frmG2Main.SelectedG2;
  if assigned(G2) then begin
    try
      G2.IsServer := cbIsServer.Checked;
    finally
      UpdateControls;
    end;
  end;
end;

procedure TfrmSettings.cbLogEnabledClick(Sender: TObject);
var G2 : TG2;
begin
  if FDisableControls then
    exit;

  G2 := frmG2Main.SelectedG2;
  if not assigned(G2) then
    exit;

  if cbLogEnabled.Checked then
    G2.LogLevel := 1
  else
    G2.LogLevel := 0;
end;

procedure TfrmSettings.FormClose(Sender: TObject; var Action: TCloseAction);
var G2 : TG2;
    i, c, ledtimer : integer;
begin
  G2 := frmG2Main.SelectedG2;
  if assigned(G2) then begin
    G2.Host := eHost.Text;
    G2.Port := StrToInt(ePort.Text);
    val( eTimerBroadcastLedMessages.Text, ledtimer, c);
    if c = 0 then
      G2.TimerBroadcastLedMessages := ledtimer;

    val( eCableThickness.Text, i, c);
    if c = 0 then begin
      if G_CableThickness <> i then begin
        G_CableThickness := i;
        frmG2Main.SetEditorSettings( G_SlotStripColor, G_SlotStripInverseColor, G_SlotStripDisabledColor, G_HighLightColor, G_CableThickness);
        frmG2Main.ShakeCables;
      end;
    end;
  end;
end;


procedure TfrmSettings.UpdateControls;
var G2 : TG2;
begin
  G2 := frmG2Main.SelectedG2;
  if not assigned(G2) then
    exit;

  FDisableControls := True;
  try
    // TCP-IP
    cbIsServer.Checked := G2.IsServer;
    eHost.Text := G2.Host;
    ePort.Text := IntTostr(G2.Port);
    if cbIsServer.Checked then begin
      eHost.Enabled := False;
    end else
      eHost.Enabled := True;
    eTimerBroadcastLedMessages.Text := IntToStr(G2.TimerBroadcastLedMessages);

    cbMidiInDevices.Text := G2.MidiInput.ProductName;
    cbMidiOutDevices.Text := G2.MidiOutput.ProductName;
    cbCtrlMidiInDevices.Text := frmG2Main.CtrlMidiInput.ProductName;
    cbLogEnabled.Checked := G2.LogLevel = 1;
    eCableThickness.Text := IntToStr(G_CableThickness);
    cbSlotStripColor.Selected := G_SlotStripColor;
    cbSlotStripInverseColor.Selected := G_SlotStripInverseColor;
    cbSlotStripDisabledColor.Selected := G_SlotStripDisabledColor;
    cbHighLightColor.Selected := G_HighLightColor;
  finally
    FDisableControls := False;
  end;
end;

procedure TfrmSettings.bSelectRootFolderClick(Sender: TObject);
var FDir : string;
begin
  if Win32MajorVersion >= 6 then
    with TFileOpenDialog.Create(nil) do
      try
        Title := 'Select Directory';
        Options := [fdoPickFolders, fdoPathMustExist, fdoForceFileSystem]; // YMMV
        OkButtonLabel := 'Select';
        DefaultFolder := FDir;
        FileName := FDir;
        if Execute then
          eRootFolder.Text := FileName;
      finally
        Free;
      end
  else
    if SelectDirectory('Select Directory', ExtractFileDrive(FDir), FDir,
               [sdNewUI, sdNewFolder]) then
      eRootFolder.Text := FDir;
end;

// OSC interface, someting for later...

procedure TfrmSettings.udpServerDeviceUDPRead(AThread: TIdUDPListenerThread; AData: TIdBytes; ABinding: TIdSocketHandle);
var i, p, c, b, KnobIndex : integer;
    address : AnsiString;
    Knob : TKnob;
    OscBundle : TOSCBundle;
    OscMessage : TOSCMessage;
    OscPacket : TOSCPacket;
    G2 : TG2;
begin
  G2 := frmG2Main.SelectedG2;
  if not assigned(G2) then
    exit;

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
                Knob := G2.SelectedPatch.GetKnob( KnobIndex);
                if assigned(Knob) and (Knob.IsAssigned = 1) then
                for i := 0 to OscMessage.ArgumentCount - 1 do begin
                  Memo1.Lines.Add(string(Address + ':' + OscMessage.Argument[i]));
                  Knob.Parameter.SetParameterValue( trunc(127 * StrToFloat(string(OscMessage.Argument[i]))));
                end;
              end;
          end;
    end;
    if OscPacket is TOSCMessage then begin
      G2.dump_buffer( AData, Length(AData));
    end;
  finally
     OSCPacket.Free;
  end;
end;

procedure TfrmSettings.Button2Click(Sender: TObject);
begin
  if FDisableControls then
    exit;

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
