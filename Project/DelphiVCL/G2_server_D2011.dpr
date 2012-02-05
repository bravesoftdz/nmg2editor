program G2_server_D2011;

uses
  Forms,
  UnitG2Server in '..\..\Source\UnitG2Server.pas' {frmG2Server};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmG2Server, frmG2Server);
  Application.Run;
end.
