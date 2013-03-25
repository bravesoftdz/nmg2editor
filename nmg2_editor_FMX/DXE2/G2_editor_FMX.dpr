program G2_editor_FMX;

uses
  FMX.Forms,
  FMX.Types,
  UnitG2EditorFMX in '..\UnitG2EditorFMX.pas' {frmSVGTest},
  SVGControl in '..\..\..\SVG\SVGControl.pas',
  dom in '..\..\Source\Third_party_code\FPC_XML\dom.pas',
  uriparser in '..\..\Source\Third_party_code\FPC_XML\uriparser.pas',
  xmlread in '..\..\Source\Third_party_code\FPC_XML\xmlread.pas',
  xmlutils in '..\..\Source\Third_party_code\FPC_XML\xmlutils.pas',
  xmlwrite in '..\..\Source\Third_party_code\FPC_XML\xmlwrite.pas',
  CSSUtility in '..\..\..\SVG\CSSUtility.pas',
  g2_types in '..\..\Source\Common\g2_types.pas',
  g2_database in '..\..\Source\Common\g2_database.pas',
  g2_file in '..\..\Source\Common\g2_file.pas',
  g2_mess in '..\..\Source\Common\g2_mess.pas',
  G2_USB in '..\..\Source\Common\G2_USB.pas',
  LibUSBWinDyn in '..\..\Source\Common\LibUSBWinDyn.pas',
  graph_util_fmx in '..\..\Source\Common\graph_util_fmx.pas',
  g2_graph_FMX in '..\g2_graph_FMX.pas';

{$R *.res}

begin
  {GlobalUseDirect2D := false;
  GlobalUseHWEffects := true;}

  GlobalUseDirect2D := true;
  GlobalUseHWEffects := false;

  {GlobalUseHWEffects := true;
  GlobalUseDirect2D := False;
  GlobalUseDirect2DSoftware := False;
  GlobalDisableFocusEffect := True;}

  Application.Initialize;
  Application.CreateForm(TfrmSVGTest, frmSVGTest);
  Application.Run;
end.
