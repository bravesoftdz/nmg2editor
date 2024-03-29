{$J-,H+,T-,P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}

library G2_vst_2007;
// This is the main project, you normally should not change
// anything here! Press Ctrl-F12 to access the main files!

uses
  DAEffectX in '..\..\Source\Third_party_code\TobyBear_vst_template\CommonVST\DAEffectX.pas',
  DAudioEffect in '..\..\Source\Third_party_code\TobyBear_vst_template\CommonVST\DAudioEffect.pas',
  DAudioEffectX in '..\..\Source\Third_party_code\TobyBear_vst_template\CommonVST\DAudioEffectX.pas',
  DDspUtils in '..\..\Source\Third_party_code\TobyBear_vst_template\CommonVST\DDspUtils.pas',
  DVstTemplate in '..\..\Source\Third_party_code\TobyBear_vst_template\CommonVST\DVstTemplate.pas',
  DVstUtils in '..\..\Source\Third_party_code\TobyBear_vst_template\CommonVST\DVstUtils.pas',
  uEditor in '..\..\Source\VST\uEditor.pas' {PluginEditorWindow},
  uPlugin in '..\..\Source\VST\uPlugin.pas',
  DAEffect in '..\..\Source\Third_party_code\TobyBear_vst_template\CommonVST\DAEffect.pas',
  g2_classes in '..\..\Source\Common\g2_classes.pas',
  g2_database in '..\..\Source\Common\g2_database.pas',
  g2_file in '..\..\Source\Common\g2_file.pas',
  g2_graph in '..\..\Source\Common\g2_graph.pas',
  g2_mess in '..\..\Source\Common\g2_mess.pas',
  g2_midi in '..\..\Source\Common\g2_midi.pas',
  g2_types in '..\..\Source\Common\g2_types.pas',
  G2_USB in '..\..\Source\Common\G2_USB.pas',
  dom in '..\..\Source\Third_party_code\FPC_XML\dom.pas',
  uriparser in '..\..\Source\Third_party_code\FPC_XML\uriparser.pas',
  xmlread in '..\..\Source\Third_party_code\FPC_XML\xmlread.pas',
  xmlutils in '..\..\Source\Third_party_code\FPC_XML\xmlutils.pas',
  xmlwrite in '..\..\Source\Third_party_code\FPC_XML\xmlwrite.pas',
  MidiType in '..\..\Source\Third_party_code\MIDIIO\MidiType.pas',
  MidiDefs in '..\..\Source\Third_party_code\MIDIIO\MidiDefs.pas',
  CircBuf in '..\..\Source\Third_party_code\MIDIIO\CircBuf.pas',
  graph_util_vcl in '..\..\Source\Common\graph_util_vcl.pas',
  fastbitmap in '..\..\Source\Common\fastbitmap.pas',
  LibUSBWinDyn in '..\..\Source\Common\LibUSBWinDyn.pas';

var Effect : APlugin;
    Oome   : Boolean;

function main(audioMaster: TAudioMasterCallbackFunc): PAEffect; cdecl; export;
begin
  // get vst version
  if audioMaster(nil, audioMasterVersion, 0, 0, nil, 0) = 0 then begin
    Result := nil;
    Exit;
  end;

  Effect := APlugin.Create(audioMaster);
  if not Assigned(Effect) then begin
    Result := nil;
    Exit;
  end;

  if oome then begin
    Effect.Free;
    Result := nil;
    Exit;
  end;

  Result := Effect.Effect;
end;

exports
  Main name 'main';

begin
end.
