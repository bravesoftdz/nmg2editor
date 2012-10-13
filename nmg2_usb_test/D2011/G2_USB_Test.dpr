program G2_USB_Test;

uses
  Vcl.Forms,
  UnitG2USBTest in '..\..\Source\UnitG2USBTest.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
