program G2_USB_Test;

uses
  Vcl.Forms,
  UnitG2USBTest in '..\..\Source\Windows\UnitG2USBTest.pas' {Form1},
  g2_file in '..\..\Source\Common\g2_file.pas',
  g2_mess in '..\..\Source\Common\g2_mess.pas',
  g2_types in '..\..\Source\Common\g2_types.pas',
  G2_USB in '..\..\Source\Common\G2_USB.pas',
  LibUSBWinDyn in '..\..\Source\Common\LibUSBWinDyn.pas',
  dom in '..\..\Source\Third_party_code\FPC_XML\dom.pas',
  uriparser in '..\..\Source\Third_party_code\FPC_XML\uriparser.pas',
  xmlread in '..\..\Source\Third_party_code\FPC_XML\xmlread.pas',
  xmlutils in '..\..\Source\Third_party_code\FPC_XML\xmlutils.pas',
  xmlwrite in '..\..\Source\Third_party_code\FPC_XML\xmlwrite.pas',
  g2_database in '..\..\Source\Common\g2_database.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
