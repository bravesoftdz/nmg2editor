program G2_editor_D2007;

uses
  Forms,
  G2_file in '..\..\Source\Common\G2_file.pas',
  G2_Classes in '..\..\Source\Common\G2_Classes.pas',
  G2_Types in '..\..\Source\Common\G2_Types.pas',
  G2_Graph in '..\..\Source\Common\G2_Graph.pas',
  G2_Database in '..\..\Source\Common\G2_Database.pas',
  graph_util_vcl in '..\..\Source\Common\graph_util_vcl.pas',
  UnitG2Editor in '..\..\Source\Windows\UnitG2Editor.pas' {frmG2Main},
  UnitPatchSettings in '..\..\Source\Windows\UnitPatchSettings.pas' {frmPatchSettings},
  UnitLog in '..\..\Source\Windows\UnitLog.pas' {frmLog},
  UnitParameterPages in '..\..\Source\Windows\UnitParameterPages.pas' {frmParameterPages},
  UnitSeqGrid in '..\..\Source\Windows\UnitSeqGrid.pas' {frmSeqGrid},
  UnitSynthSettings in '..\..\Source\Windows\UnitSynthSettings.pas' {frmSynthSettings},
  UnitPerfSettings in '..\..\Source\Windows\UnitPerfSettings.pas' {frmPerfSettings},
  UnitEditLabel in '..\..\Source\Windows\UnitEditLabel.pas' {frmEditLabel},
  UnitEditorTools in '..\..\Source\Windows\UnitEditorTools.pas' {frmEditorTools},
  UnitPatchManager in '..\..\Source\Windows\UnitPatchManager.pas' {frmPatchManager},
  UnitModuleDef in '..\..\Source\Windows\UnitModuleDef.pas' {frmModuleDef},
  UnitSettings in '..\..\Source\Windows\UnitSettings.pas' {frmSettings},
  UnitPatchNotes in '..\..\Source\Windows\UnitPatchNotes.pas' {frmPatchNotes},
  UnitMidiMapping in '..\..\Source\Windows\UnitMidiMapping.pas' {frmMidiMapping},
  UnitPatchBrowser in '..\..\Source\Windows\UnitPatchBrowser.pas' {frmPatchBrowser},
  UnitPatchBrowserFilterModules in '..\..\Source\Windows\UnitPatchBrowserFilterModules.pas' {frmPatchBrowserModuleFilter},
  OSCUtils in '..\..\Source\Common\OSCUtils.pas',
  dom in '..\..\Source\Third_party_code\FPC_XML\dom.pas',
  uriparser in '..\..\Source\Third_party_code\FPC_XML\uriparser.pas',
  xmlread in '..\..\Source\Third_party_code\FPC_XML\xmlread.pas',
  xmlutils in '..\..\Source\Third_party_code\FPC_XML\xmlutils.pas',
  xmlwrite in '..\..\Source\Third_party_code\FPC_XML\xmlwrite.pas',
  g2_mess in '..\..\Source\Common\g2_mess.pas',
  fastbitmap in '..\..\Source\Common\fastbitmap.pas',
  g2_midi in '..\..\Source\Common\g2_midi.pas',
  CircBuf in '..\..\Source\Third_party_code\MIDIIO\CircBuf.pas',
  MidiCallback in '..\..\Source\Third_party_code\MIDIIO\MidiCallback.pas',
  MidiCons in '..\..\Source\Third_party_code\MIDIIO\MidiCons.pas',
  MidiDefs in '..\..\Source\Third_party_code\MIDIIO\MidiDefs.pas',
  MidiIn in '..\..\Source\Third_party_code\MIDIIO\MidiIn.pas',
  MidiKeyPatchArray in '..\..\Source\Third_party_code\MIDIIO\MidiKeyPatchArray.pas',
  MidiOut in '..\..\Source\Third_party_code\MIDIIO\MidiOut.pas',
  MidiType in '..\..\Source\Third_party_code\MIDIIO\MidiType.pas',
  G2_USB in '..\..\Source\Common\G2_USB.pas',
  LibUSBWinDyn in '..\..\Source\Common\LibUSBWinDyn.pas',
  JawsCtrls in '..\..\Source\Common\JawsCtrls.pas',
  MusicalKeyboard in '..\..\Source\Common\MusicalKeyboard.pas';

{$R *.res}

begin
  Application.Initialize;
{$IFDEF FPC}
{$ELSE}
  Application.MainFormOnTaskbar := True;
{$ENDIF}
  Application.CreateForm(TfrmG2Main, frmG2Main);
  Application.CreateForm(TfrmSettings, frmSettings);
  Application.CreateForm(TfrmPatchSettings, frmPatchSettings);
  Application.CreateForm(TfrmLog, frmLog);
  Application.CreateForm(TfrmParameterPages, frmParameterPages);
  Application.CreateForm(TfrmSeqGrid, frmSeqGrid);
  Application.CreateForm(TfrmSynthSettings, frmSynthSettings);
  Application.CreateForm(TfrmPerfSettings, frmPerfSettings);
  Application.CreateForm(TfrmEditLabel, frmEditLabel);
  Application.CreateForm(TfrmEditorTools, frmEditorTools);
  Application.CreateForm(TfrmPatchManager, frmPatchManager);
  Application.CreateForm(TfrmModuleDef, frmModuleDef);
  Application.CreateForm(TfrmPatchNotes, frmPatchNotes);
  Application.CreateForm(TfrmMidiMapping, frmMidiMapping);
  Application.CreateForm(TfrmPatchBrowser, frmPatchBrowser);
  Application.CreateForm(TfrmPatchBrowserModuleFilter, frmPatchBrowserModuleFilter);
  Application.Run;
end.
