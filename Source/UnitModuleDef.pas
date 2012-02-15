unit UnitModuleDef;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Grids, Vcl.ExtCtrls,
  DOM, XMLWrite, g2_database;

type
  TfrmModuleDef = class(TForm)
    Panel1: TPanel;
    sgParams: TStringGrid;
    Panel2: TPanel;
    Label1: TLabel;
    eModuleName: TEdit;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    slParamAttr : TStringList;
    FModuleType,
    FModuleIndex : integer;
    procedure UpdateControls;
    procedure UpdateModuleDef;
  end;

var
  frmModuleDef: TfrmModuleDef;

implementation

{$R *.dfm}

uses UnitG2Editor;

{ TForm1 }

procedure TfrmModuleDef.Button1Click(Sender: TObject);
begin
  UpdateModuleDef;
end;

procedure TfrmModuleDef.FormCreate(Sender: TObject);
begin
  slParamAttr := TStringList.Create;
end;

procedure TfrmModuleDef.FormDestroy(Sender: TObject);
begin
  slParamAttr.Free;
end;

procedure TfrmModuleDef.FormShow(Sender: TObject);
begin
  UpdateControls;
end;

procedure TfrmModuleDef.UpdateControls;
var m, p, i : integer;
    ParamNode : TDOMElement;
    Node : TDOMNode;
begin
  FModuleIndex := -1;
  // Make a list of all posible parameter attributes
  slParamAttr.Clear;
  for m := 0 to frmG2Main.G2.FModuleDefList.Count - 1 do begin
    if FModuleType = frmG2Main.G2.FModuleDefList.ModuleDef[m].ModuleType then
      FModuleIndex := m;

    if frmG2Main.G2.FModuleDefList.ModuleDef[m].Params <> nil then
      for p := 0 to frmG2Main.G2.FModuleDefList.ModuleDef[m].Params.Count - 1 do begin
        ParamNode := frmG2Main.G2.FModuleDefList.ModuleDef[m].Params[p];
        Node := ParamNode.FirstChild;
        while Node <> nil do begin
          if slParamAttr.IndexOf( Node.NodeName) = -1 then
            slParamAttr.Add( Node.NodeName);
          Node := Node.NextSibling;
        end;
      end;
  end;

  if FModuleIndex <> -1 then begin
    eModuleName.Text := frmG2Main.G2.FModuleDefList.ModuleDef[FModuleIndex].LongName;
    if frmG2Main.G2.FModuleDefList.ModuleDef[FModuleIndex].Params <> nil then begin
      sgParams.ColCount := slParamAttr.Count + 1;
      sgParams.RowCount := frmG2Main.G2.FModuleDefList.ModuleDef[FModuleIndex].Params.Count + 1;
      for p := 0 to slParamAttr.Count - 1 do
        sgParams.Cells[p + 1, 0] := slParamAttr[p];
      for p := 0 to frmG2Main.G2.FModuleDefList.ModuleDef[FModuleIndex].Params.Count - 1 do begin
        sgParams.Cells[0, p + 1] := IntToStr(p);
        ParamNode := frmG2Main.G2.FModuleDefList.ModuleDef[FModuleIndex].Params[p];
        for i := 0 to slParamAttr.Count - 1 do begin
          Node := ParamNode.FindNode( slParamAttr[i]);
          if Node <> nil then
            sgParams.Cells[i + 1, p + 1] := Node.TextContent
          else
            sgParams.Cells[i + 1, p + 1] := '';
        end;
      end;
    end;
  end else
    raise Exception.Create('Moduletype ' + IntToStr(FModuleType) + ' not found.');
end;

procedure TfrmModuleDef.UpdateModuleDef;
var p, i : integer;
    ParamNode : TDOMElement;
    Node : TDOMNode;
    new_filename : string;
begin
  for p := 0 to frmG2Main.G2.FModuleDefList.ModuleDef[FModuleIndex].Params.Count - 1 do begin
    ParamNode := frmG2Main.G2.FModuleDefList.ModuleDef[FModuleIndex].Params[p];
    for i := 0 to slParamAttr.Count - 1 do begin
      Node := ParamNode.FindNode( slParamAttr[i]);
      if Node <> nil then begin
        if sgParams.Cells[i + 1, p + 1] = '' then begin
          ParamNode.DetachChild(Node);
          Node.Free;
        end else
          Node.TextContent := sgParams.Cells[i + 1, p + 1];
      end else
        if sgParams.Cells[i + 1, p + 1] <> '' then begin
          Node := TDOMDocument(ParamNode.OwnerDocument).CreateElement(slParamAttr[i]);
          ParamNode.AppendChild( Node);
          Node.TextContent := sgParams.Cells[i + 1, p + 1];
        end;
    end;
  end;

  new_filename := 'ModuleDef_new.xml';
  WriteXMLFile( frmG2Main.G2.FXMLModuleDefs, new_filename);
end;

end.
