program Test_vst_D2011;

uses
  Forms,
  UnitTestVst in '..\..\Source\VST\UnitTestVst.pas' {Form1},
  uEditor in '..\..\Source\VST\uEditor.pas' {PluginEditorWindow},
  g2_usb in '..\..\Source\NMG2Controls\g2_usb.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
