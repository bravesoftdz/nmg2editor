program G2_editor_FMX;
uses
  FMX.Forms,
  UnitG2EditorFMX in '..\..\Source\UnitG2EditorFMX.pas' {Form1},
  g2_types in '..\..\Source\NMG2Controls\g2_types.pas',
  g2_file in '..\..\Source\NMG2Controls\g2_file.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
