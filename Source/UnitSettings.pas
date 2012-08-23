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
  MidiDeviceComboBox, FileCtrl, JawsCtrls;

const
   EXPLENATIONS : array[0..19] of string =
      ('Is Server : If checked, the G2 VST or other OS G2 editors can connect to this editor using the TCP-IP settings. Only the server editor can have a direct USB connection to the G2 synth, so in a normal setup this one should be checked.',
       'Port : The port number for the server editor.',
       'Host : TCP-IP address of the PC the server editor is running on.',
       'Timer broadcast led messages : The timer interval the server is sending led update messages to the clients (set higher to prevent network overload).',
       'Midi enabled : Check to enable the midi ports of the editor to send and receive sysex messages. Please note : controller midi for the G2 synth is controlled in the Synth settings dialog!',
       'Sysex midi in : The midi port to receive sysex messages into the editor.',
       'Sysex midi out : The midi port to send sysex messages.',
       'Ctrl midi enabled : Check to enable receiving controller midi for the editor. Please note : controller midi for the G2 synth is controlled in the Synth settings dialog!',
       'Ctrl midi in : The midi port to recieve controller midi messages for the editor.',
       'Log enabled : Check to enable the log. Please note: this will slowdown the loading of patches somewhat.',
       'Cable thickness : Enter a number to control the cable thickness in the patch windows.',
       'Slot strip color : Select a color for the slot strips.',
       'Slot strip inverse color : Select a color for the slot strip when selected.',
       'Slot strip disabled color : Select a color for the slot strip when disabled.',
       'Highlight color : Select a color for the module controls that are highlighted.',
       'Led color : Select a color for the leds.',
       'Only text menus : For use with JAWS (screenreading software for visualy disabled users) this should be checked, otherwise JAWS will not read out the menu options',
       'Patch root folder : Set the root folder of your g2 patch library on disk for use in the patch manager.',
       'Module help file : Set to "Nord Modular G2 Editor v1.62.chm".',
       'G2ools folder : Set to the folder containing the g2ools executables.');

type
  TfrmSettings = class(TForm)
    Memo1: TMemo;
    IdUDPServer1: TIdUDPServer;
    Button2: TButton;
    Panel1: TPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    bSelectPtachRootFolder: TButton;
    TabSheet4: TTabSheet;
    cbIsServer: TCheckBox;
    cbMidiEnabled: TCheckBox;
    cbCtrlMidiEnabled: TCheckBox;
    TabSheet5: TTabSheet;
    cbSlotStripColor: TColorBox;
    cbSlotStripInverseColor: TColorBox;
    cbSlotStripDisabledColor: TColorBox;
    cbHighLightColor: TColorBox;
    cbLogEnabled: TCheckBox;
    cbOnlyTextMenus: TCheckBox;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    StaticText3: TStaticText;
    StaticText4: TStaticText;
    StaticText5: TStaticText;
    StaticText6: TStaticText;
    StaticText7: TStaticText;
    StaticText8: TStaticText;
    StaticText9: TStaticText;
    StaticText10: TStaticText;
    StaticText11: TStaticText;
    StaticText12: TStaticText;
    StaticText13: TStaticText;
    StaticText14: TStaticText;
    ePort: DEdit;
    eHost: DEdit;
    eTimerBroadcastLedMessages: DEdit;
    eCableThickness: DEdit;
    ePatchRootFolder: DEdit;
    eOSCServerIP: DEdit;
    eOSCHostPort: DEdit;
    cbMidiInDevices: DCombobox;
    cbMidiOutDevices: DCombobox;
    cbCtrlMidiInDevices: DCombobox;
    Panel2: TPanel;
    lExplenation: TLabel;
    cbLedColor: TColorBox;
    StaticText15: TStaticText;
    StaticText16: TStaticText;
    StaticText17: TStaticText;
    eModuleHelpFile: TEdit;
    eG2oolsFolder: TEdit;
    bSelectModuleHelpFile: TButton;
    bSelectG2oolsFolder: TButton;
    OpenDialog1: TOpenDialog;
    bMidiMapping: TButton;
    procedure Button2Click(Sender: TObject);
    procedure IdUDPServer1Status(ASender: TObject; const AStatus: TIdStatus;
      const AStatusText: string);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cbIsServerClick(Sender: TObject);
    procedure cbMidiEnabledClick(Sender: TObject);
    procedure bSelectPtachRootFolderClick(Sender: TObject);
    procedure cbMidiInDevicesSelect(Sender: TObject);
    procedure cbMidiOutDevicesSelect(Sender: TObject);
    procedure cbCtrlMidiInDevicesSelect(Sender: TObject);
    procedure cbCtrlMidiEnabledClick(Sender: TObject);
    procedure cbLogEnabledClick(Sender: TObject);
    procedure cbSlotStripColorChange(Sender: TObject);
    procedure cbSlotStripInverseColorChange(Sender: TObject);
    procedure cbSlotStripDisabledColorChange(Sender: TObject);
    procedure cbHighLightColorChange(Sender: TObject);
    procedure cbOnlyTextMenusClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure cbIsServerEnter(Sender: TObject);
    procedure ePortEnter(Sender: TObject);
    procedure eHostEnter(Sender: TObject);
    procedure eTimerBroadcastLedMessagesEnter(Sender: TObject);
    procedure cbMidiEnabledEnter(Sender: TObject);
    procedure cbMidiInDevicesEnter(Sender: TObject);
    procedure cbMidiOutDevicesEnter(Sender: TObject);
    procedure cbCtrlMidiEnabledEnter(Sender: TObject);
    procedure cbCtrlMidiInDevicesEnter(Sender: TObject);
    procedure cbLogEnabledEnter(Sender: TObject);
    procedure eCableThicknessEnter(Sender: TObject);
    procedure cbSlotStripColorEnter(Sender: TObject);
    procedure cbSlotStripInverseColorEnter(Sender: TObject);
    procedure cbSlotStripDisabledColorEnter(Sender: TObject);
    procedure cbHighLightColorEnter(Sender: TObject);
    procedure cbLedColorEnter(Sender: TObject);
    procedure cbOnlyTextMenusEnter(Sender: TObject);
    procedure ePatchRootFolderEnter(Sender: TObject);
    procedure eModuleHelpFileEnter(Sender: TObject);
    procedure eG2oolsFolderEnter(Sender: TObject);
    procedure bSelectModuleHelpFileClick(Sender: TObject);
    procedure bSelectG2oolsFolderClick(Sender: TObject);
    procedure cbLedColorChange(Sender: TObject);
    procedure bMidiMappingClick(Sender: TObject);
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

uses UnitG2Editor, UnitMidiMapping;

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

procedure TfrmSettings.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
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
    DirSettingsNode : TXMLDirectorySettingsType;
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
              frmMidiMapping.CtrlMidiInput.ChangeDevice( cbCtrlMidiInDevices.ItemIndex, False);
              frmMidiMapping.CtrlMidiEnabled := CtrlMidiSettingsNode.CtrlMidiEnabled;
            except on E:Exception do begin
                ShowMessage( E.Message);
                frmMidiMapping.CtrlMidiEnabled := False;
              end;
            end;

          finally
            cbCtrlMidiEnabled.Checked := frmMidiMapping.CtrlMidiEnabled;
          end;
        end else
          frmMidiMapping.CtrlMidiEnabled := False;
      end;

      DirSettingsNode := TXMLDirectorySettingsType(RootNode.FindNode('DirectorySettings'));
      if assigned(DirSettingsNode) then begin
        eG2oolsFolder.Text := String(DirSettingsNode.G2oolsFolder);
        eModuleHelpFile.Text := String(DirSettingsNode.ModuleHelpFile);
      end;

      PatchManagerSettingsNode := TXMLPatchManagerSettingsType(RootNode.FindNode('PatchManagerSettings'));
      if assigned(PatchManagerSettingsNode) then begin
        ePatchRootFolder.Text := String(PatchManagerSettingsNode.BaseFolder);
      end;

      FormSettingsNode := TXMLFormSettingsType(RootNode.FindNode('SettingsForm'));
      if assigned(FormSettingsNode) then begin
        Left := FormSettingsNode.PosX;
        Top := FormSettingsNode.PosY;
        {Width := FormSettingsNode.SizeX;
        Height := FormSettingsNode.SizeY;}
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

procedure TfrmSettings.cbMidiEnabledEnter(Sender: TObject);
begin
  lExplenation.Caption := EXPLENATIONS[4];
end;

procedure TfrmSettings.cbMidiInDevicesEnter(Sender: TObject);
begin
  lExplenation.Caption := EXPLENATIONS[5];
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

procedure TfrmSettings.cbMidiOutDevicesEnter(Sender: TObject);
begin
  lExplenation.Caption := EXPLENATIONS[6];
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
    frmMidiMapping.CtrlMidiEnabled := cbCtrlMidiEnabled.Checked;
  finally
    UpdateControls;
  end;
end;

procedure TfrmSettings.cbCtrlMidiEnabledEnter(Sender: TObject);
begin
  lExplenation.Caption := EXPLENATIONS[7];
end;

procedure TfrmSettings.cbCtrlMidiInDevicesEnter(Sender: TObject);
begin
  lExplenation.Caption := EXPLENATIONS[8];
end;

procedure TfrmSettings.cbCtrlMidiInDevicesSelect(Sender: TObject);
var G2 : TG2;
begin
  if FDisableControls then
    exit;

  if cbCtrlMidiInDevices.ItemIndex > -1  then
    frmMidiMapping.CtrlMidiInput.ChangeDevice( cbCtrlMidiInDevices.ItemIndex, cbCtrlMidiEnabled.Checked)
  else
    cbCtrlMidiInDevices.Text := '';
end;

procedure TfrmSettings.cbSlotStripColorChange(Sender: TObject);
begin
  if FDisableControls then
    exit;

  G_SlotStripColor := cbSlotStripColor.Selected;

  frmG2Main.SetEditorSettings( G_SlotStripColor, G_SlotStripInverseColor, G_SlotStripDisabledColor, G_HighLightColor, G_LedColor, G_CableThickness);
end;

procedure TfrmSettings.cbSlotStripColorEnter(Sender: TObject);
begin
  lExplenation.Caption := EXPLENATIONS[11];
end;

procedure TfrmSettings.cbSlotStripDisabledColorChange(Sender: TObject);
begin
  if FDisableControls then
    exit;

  G_SlotStripDisabledColor := cbSlotStripDisabledColor.Selected;

  frmG2Main.SetEditorSettings( G_SlotStripColor, G_SlotStripInverseColor, G_SlotStripDisabledColor, G_HighLightColor, G_LedColor, G_CableThickness);
end;

procedure TfrmSettings.cbSlotStripDisabledColorEnter(Sender: TObject);
begin
  lExplenation.Caption := EXPLENATIONS[13];
end;

procedure TfrmSettings.cbSlotStripInverseColorChange(Sender: TObject);
begin
  if FDisableControls then
    exit;

  G_SlotStripInverseColor := cbSlotStripInverseColor.Selected;

  frmG2Main.SetEditorSettings( G_SlotStripColor, G_SlotStripInverseColor, G_SlotStripDisabledColor, G_HighLightColor, G_LedColor, G_CableThickness);
end;

procedure TfrmSettings.cbSlotStripInverseColorEnter(Sender: TObject);
begin
  lExplenation.Caption := EXPLENATIONS[12];
end;

procedure TfrmSettings.cbLedColorChange(Sender: TObject);
begin
  if FDisableControls then
    exit;

  G_LedColor := cbLedColor.Selected;

  frmG2Main.SetEditorSettings( G_SlotStripColor, G_SlotStripInverseColor, G_SlotStripDisabledColor, G_HighLightColor, G_LedColor, G_CableThickness);
end;

procedure TfrmSettings.cbLedColorEnter(Sender: TObject);
begin
  lExplenation.Caption := EXPLENATIONS[15];
end;

procedure TfrmSettings.eCableThicknessEnter(Sender: TObject);
begin
  lExplenation.Caption := EXPLENATIONS[10];
end;

procedure TfrmSettings.eG2oolsFolderEnter(Sender: TObject);
begin
  lExplenation.Caption := EXPLENATIONS[19];
end;

procedure TfrmSettings.eHostEnter(Sender: TObject);
begin
  lExplenation.Caption := EXPLENATIONS[2];
end;

procedure TfrmSettings.eModuleHelpFileEnter(Sender: TObject);
begin
  lExplenation.Caption := EXPLENATIONS[18];
end;

procedure TfrmSettings.ePatchRootFolderEnter(Sender: TObject);
begin
  lExplenation.Caption := EXPLENATIONS[17];
end;

procedure TfrmSettings.ePortEnter(Sender: TObject);
begin
  lExplenation.Caption := EXPLENATIONS[1];
end;

procedure TfrmSettings.eTimerBroadcastLedMessagesEnter(Sender: TObject);
begin
  lExplenation.Caption := EXPLENATIONS[3];
end;

procedure TfrmSettings.cbHighLightColorChange(Sender: TObject);
begin
  if FDisableControls then
    exit;

  G_HighLightColor := cbHighLightColor.Selected;

  frmG2Main.SetEditorSettings( G_SlotStripColor, G_SlotStripInverseColor, G_SlotStripDisabledColor, G_HighLightColor, G_LedColor, G_CableThickness);
end;

procedure TfrmSettings.cbHighLightColorEnter(Sender: TObject);
begin
  lExplenation.Caption := EXPLENATIONS[14];
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

procedure TfrmSettings.cbIsServerEnter(Sender: TObject);
begin
  lExplenation.Caption := EXPLENATIONS[0];
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

procedure TfrmSettings.cbLogEnabledEnter(Sender: TObject);
begin
  lExplenation.Caption := EXPLENATIONS[9];
end;

procedure TfrmSettings.cbOnlyTextMenusClick(Sender: TObject);
var G2 : TG2;
begin
  if FDisableControls then
    exit;

  if cbOnlyTextMenus.Checked then
    frmG2Main.OnlyTextMenus := True
  else
    frmG2Main.OnlyTextMenus := False;
end;

procedure TfrmSettings.cbOnlyTextMenusEnter(Sender: TObject);
begin
  lExplenation.Caption := EXPLENATIONS[16];
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
        frmG2Main.SetEditorSettings( G_SlotStripColor, G_SlotStripInverseColor, G_SlotStripDisabledColor, G_HighLightColor, G_LedColor, G_CableThickness);
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
    cbCtrlMidiInDevices.Text := frmMidiMapping.CtrlMidiInput.ProductName;
    cbLogEnabled.Checked := G2.LogLevel = 1;
    eCableThickness.Text := IntToStr(G_CableThickness);
    cbSlotStripColor.Selected := G_SlotStripColor;
    cbSlotStripInverseColor.Selected := G_SlotStripInverseColor;
    cbSlotStripDisabledColor.Selected := G_SlotStripDisabledColor;
    cbHighLightColor.Selected := G_HighLightColor;
    cbLedColor.Selected := G_LedColor;
    cbOnlyTextMenus.Checked := frmG2Main.OnlyTextMenus;
  finally
    FDisableControls := False;
  end;
end;

procedure TfrmSettings.bMidiMappingClick(Sender: TObject);
begin
  frmMidiMapping.Show;
end;

procedure TfrmSettings.bSelectG2oolsFolderClick(Sender: TObject);
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
          eG2oolsFolder.Text := FileName;
      finally
        Free;
      end
  else
    if SelectDirectory('Select Directory', ExtractFileDrive(FDir), FDir,
               [sdNewUI, sdNewFolder]) then
      eG2oolsFolder.Text := FDir;
  frmG2Main.UpdateControls;
end;

procedure TfrmSettings.bSelectModuleHelpFileClick(Sender: TObject);
var FDir : string;
begin
  OpenDialog1.Filter := 'compiled help files (*.chm)|*.chm';
  if OpenDialog1.Execute then
      eModuleHelpFile.Text := OpenDialog1.FileName;;
  frmG2Main.UpdateControls;
end;

procedure TfrmSettings.bSelectPtachRootFolderClick(Sender: TObject);
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
          ePatchRootFolder.Text := FileName;
      finally
        Free;
      end
  else
    if SelectDirectory('Select Directory', ExtractFileDrive(FDir), FDir,
               [sdNewUI, sdNewFolder]) then
      ePatchRootFolder.Text := FDir;
  frmG2Main.UpdateControls;
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
