program G2_editor_D2007;

uses
  Forms,
  G2_file in '..\..\Source\NMG2Controls\G2_file.pas',
  G2_Classes in '..\..\Source\NMG2Controls\G2_Classes.pas',
  G2_Types in '..\..\Source\NMG2Controls\G2_Types.pas',
  G2_Graph in '..\..\Source\NMG2Controls\G2_Graph.pas',
  G2_USB in '..\..\Source\NMG2Controls\G2_USB.pas',
  G2_Database in '..\..\Source\NMG2Controls\G2_Database.pas',
  UnitG2Editor in '..\..\Source\UnitG2Editor.pas' {frmG2Main},
  UnitPatchSettings in '..\..\Source\UnitPatchSettings.pas' {frmPatchSettings},
  UnitLog in '..\..\Source\UnitLog.pas' {frmLog},
  UnitParameterPages in '..\..\Source\UnitParameterPages.pas' {frmParameterPages},
  UnitSeqGrid in '..\..\Source\UnitSeqGrid.pas' {frmSeqGrid},
  UnitSynthSettings in '..\..\Source\UnitSynthSettings.pas' {frmSynthSettings},
  UnitPerfSettings in '..\..\Source\UnitPerfSettings.pas' {frmPerfSettings},
  UnitEditLabel in '..\..\Source\UnitEditLabel.pas' {frmEditLabel},
  graph_util_vcl in '..\..\Source\NMG2Controls\graph_util_vcl.pas',
  UnitConnSettings in '..\..\Source\UnitConnSettings.pas' {frmCommSettings},
  UnitEditorTools in '..\..\Source\UnitEditorTools.pas' {frmEditorTools},
  UnitPatchManager in '..\..\Source\UnitPatchManager.pas' {frmPatchManager},
  UnitSettings in '..\..\Source\UnitSettings.pas' {frmSettings};

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
  Application.CreateForm(TfrmCommSettings, frmCommSettings);
  Application.CreateForm(TfrmEditorTools, frmEditorTools);
  Application.CreateForm(TfrmPatchManager, frmPatchManager);
  Application.CreateForm(TfrmSettings, frmSettings);
  Application.Run;
end.
