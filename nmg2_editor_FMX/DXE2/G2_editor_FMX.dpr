program G2_editor_FMX;

uses
  FMX.Forms,
  FMX.Types,
  UnitG2EditorFMX in '..\UnitG2EditorFMX.pas' {frmSVGTest},
  dom in '..\..\Source\Third_party_code\FPC_XML\dom.pas',
  uriparser in '..\..\Source\Third_party_code\FPC_XML\uriparser.pas',
  xmlread in '..\..\Source\Third_party_code\FPC_XML\xmlread.pas',
  xmlutils in '..\..\Source\Third_party_code\FPC_XML\xmlutils.pas',
  xmlwrite in '..\..\Source\Third_party_code\FPC_XML\xmlwrite.pas',
  g2_types in '..\..\Source\Common\g2_types.pas',
  g2_database in '..\..\Source\Common\g2_database.pas',
  g2_file in '..\..\Source\Common\g2_file.pas',
  g2_mess in '..\..\Source\Common\g2_mess.pas',
  G2_USB in '..\..\Source\Common\G2_USB.pas',
  LibUSBWinDyn in '..\..\Source\Common\LibUSBWinDyn.pas',
  g2_graph_FMX in '..\g2_graph_FMX.pas',
  BVE.SVGControl in '..\..\..\SVG\Common\BVE.SVGControl.pas',
  BVE.CSSUtility in '..\..\..\SVG\Common\BVE.CSSUtility.pas',
  BVE.SVGXMLWrapperDelphi in '..\..\..\SVG\Common\BVE.SVGXMLWrapperDelphi.pas',
  UnitLog in '..\UnitLog.pas' {frmLog};

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
  Application.CreateForm(TfrmLog, frmLog);
  Application.Run;
end.
