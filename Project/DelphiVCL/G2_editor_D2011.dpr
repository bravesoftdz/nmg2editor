program G2_editor_D2011;

uses
  Forms,
  g2_file in '..\..\Source\NMG2Controls\g2_file.pas',
  g2_classes in '..\..\Source\VCLG2Controls\g2_classes.pas',
  g2_types in '..\..\Source\NMG2Controls\g2_types.pas',
  g2_graph in '..\..\Source\VCLG2Controls\g2_graph.pas',
  g2_usb in '..\..\Source\NMG2Controls\g2_usb.pas',
  g2_database in '..\..\Source\NMG2Controls\g2_database.pas',
  UnitG2Editor in '..\..\Source\UnitG2Editor.pas' {frmG2Main},
  UnitPatchSettings in '..\..\Source\UnitPatchSettings.pas' {frmPatchSettings},
  UnitLog in '..\..\Source\UnitLog.pas' {frmLog},
  UnitParameterPages in '..\..\Source\UnitParameterPages.pas' {frmParameterPages},
  UnitSeqGrid in '..\..\Source\UnitSeqGrid.pas' {frmSeqGrid},
  UnitSynthSettings in '..\..\Source\UnitSynthSettings.pas' {frmSynthSettings},
  UnitPerfSettings in '..\..\Source\UnitPerfSettings.pas' {frmPerfSettings},
  UnitEditLabel in '..\..\Source\UnitEditLabel.pas' {frmEditLabel},
  UnitSettings in '..\..\Source\UnitSettings.pas' {frmSettings},
  OSCUtils in '..\..\Source\OSC\OSCUtils.pas',
  UnitEditorTools in '..\..\Source\UnitEditorTools.pas' {frmEditorTools},
  g2_midi in '..\..\Source\NMG2Controls\g2_midi.pas',
  UnitPatchManager in '..\..\Source\UnitPatchManager.pas' {frmPatchManager},
  g2_mess in '..\..\Source\NMG2Controls\g2_mess.pas',
  UnitModuleDef in '..\..\Source\UnitModuleDef.pas' {frmModuleDef},
  UnitPatchNotes in '..\..\Source\UnitPatchNotes.pas' {frmPatchNotes};

{$R *.res}

begin
  Application.Initialize;
{$IFDEF FPC}
{$ELSE}
  Application.MainFormOnTaskbar := True;
{$ENDIF}
  Application.CreateForm(TfrmG2Main, frmG2Main);
  Application.CreateForm(TfrmPatchSettings, frmPatchSettings);
  Application.CreateForm(TfrmLog, frmLog);
  Application.CreateForm(TfrmParameterPages, frmParameterPages);
  Application.CreateForm(TfrmSeqGrid, frmSeqGrid);
  Application.CreateForm(TfrmSynthSettings, frmSynthSettings);
  Application.CreateForm(TfrmPerfSettings, frmPerfSettings);
  Application.CreateForm(TfrmEditLabel, frmEditLabel);
  Application.CreateForm(TfrmSettings, frmSettings);
  Application.CreateForm(TfrmEditorTools, frmEditorTools);
  Application.CreateForm(TfrmPatchManager, frmPatchManager);
  Application.CreateForm(TfrmModuleDef, frmModuleDef);
  Application.CreateForm(TfrmPatchNotes, frmPatchNotes);
  Application.Run;
end.
