program G2_editor_FMX;
uses
  FMX.Forms,
  FMX.Types,
  UnitG2EditorFMX in '..\..\Source\FMX\UnitG2EditorFMX.pas' {Form1},
  g2_types in '..\..\Source\Common\g2_types.pas',
  g2_file in '..\..\Source\Common\g2_file.pas',
  dom in '..\..\Source\Third_party_code\FPC_XML\dom.pas',
  uriparser in '..\..\Source\Third_party_code\FPC_XML\uriparser.pas',
  xmlread in '..\..\Source\Third_party_code\FPC_XML\xmlread.pas',
  xmlutils in '..\..\Source\Third_party_code\FPC_XML\xmlutils.pas',
  xmlwrite in '..\..\Source\Third_party_code\FPC_XML\xmlwrite.pas',
  g2_database in '..\..\Source\Common\g2_database.pas',
  G2FMXGraph in '..\..\Source\Common\G2FMXGraph.pas',
  G2_USB in '..\..\Source\Common\G2_USB.pas',
  LibUSBWinDyn in '..\..\Source\Common\LibUSBWinDyn.pas',
  g2_mess in '..\..\Source\Common\g2_mess.pas';

{$R *.res}

begin
  Application.Initialize;

  GlobalUseHWEffects := False;
  GlobalUseDirect2D := False;
  GlobalUseDirect2DSoftware := False;
  GlobalDisableFocusEffect := True;

  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
