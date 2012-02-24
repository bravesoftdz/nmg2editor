unit UnitModuleDef;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Grids, Vcl.ExtCtrls,
  DOM, XMLWrite, g2_types, g2_database, g2_graph, g2_file, g2_mess, g2_usb, g2_classes,
  Vcl.ActnList, Vcl.PlatformDefaultStyleActnCtrls, Vcl.ActnMan;

type
  TfrmModuleDef = class(TForm)
    Panel1: TPanel;
    sgParams: TStringGrid;
    Panel2: TPanel;
    Label1: TLabel;
    eModuleName: TEdit;
    Button1: TButton;
    G2_module_def: TG2;
    G2GraphScrollBox1: TG2GraphScrollBox;
    Button2: TButton;
    ActionManager1: TActionManager;
    aExtractModuleInfo: TAction;
    lbModules: TListBox;
    Panel3: TPanel;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure aNextModuleExecute(Sender: TObject);
    procedure aPrevModuleExecute(Sender: TObject);
    procedure aExtractModuleInfoExecute(Sender: TObject);
    procedure lbModulesClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    slParamAttr : TStringList;
    FModuleType,
    FModuleIndex : integer;
    FModule : TG2GraphModule;
    procedure LoadDatabase;
    procedure UpdateControls;
    procedure UpdateModuleDef;
    procedure ExtractTextEdit;
    procedure LoadModule;
    procedure UnloadModule;
  end;

var
  frmModuleDef: TfrmModuleDef;

implementation

{$R *.dfm}

uses UnitG2Editor;

{ TForm1 }

procedure TfrmModuleDef.aExtractModuleInfoExecute(Sender: TObject);
begin
  ExtractTextEdit;
end;

procedure TfrmModuleDef.aNextModuleExecute(Sender: TObject);
begin
//
end;

procedure TfrmModuleDef.aPrevModuleExecute(Sender: TObject);
begin
//
end;

procedure TfrmModuleDef.Button1Click(Sender: TObject);
begin
  UpdateModuleDef;
  frmG2Main.G2.LoadModuleDefs('');
end;

procedure TfrmModuleDef.FormCreate(Sender: TObject);
begin
  LoadDatabase;
end;

procedure TfrmModuleDef.FormDestroy(Sender: TObject);
begin
  slParamAttr.Free;
end;

procedure TfrmModuleDef.FormShow(Sender: TObject);
begin
  UpdateControls;
end;

procedure TfrmModuleDef.lbModulesClick(Sender: TObject);
begin
  if lbModules.ItemIndex = -1 then
    exit;

  FModuleType := TXMLModuleDefType(lbModules.Items.Objects[lbModules.ItemIndex]).ModuleType;
  UpdateControls;
end;

procedure TfrmModuleDef.LoadDatabase;
var m, p, i : integer;
    ParamNode : TDOMElement;
    Node : TDOMNode;
begin
  lbModules.Clear;
  G2_module_def.LoadModuleDefs('');
  slParamAttr := TStringList.Create;
  FModuleIndex := -1;
  // Make a list of all posible parameter attributes
  slParamAttr.Clear;
  for m := 0 to G2_module_def.FModuleDefList.Count - 1 do begin
    lbModules.Items.Add(string(G2_module_def.FModuleDefList.ModuleDef[m].LongName));
    lbModules.Items.Objects[m] := G2_module_def.FModuleDefList.ModuleDef[m];
    if G2_module_def.FModuleDefList.ModuleDef[m].Params <> nil then
      for p := 0 to G2_module_def.FModuleDefList.ModuleDef[m].Params.Count - 1 do begin
        ParamNode := G2_module_def.FModuleDefList.ModuleDef[m].Params[p];
        Node := ParamNode.FirstChild;
        while Node <> nil do begin
          if slParamAttr.IndexOf( Node.NodeName) = -1 then
            slParamAttr.Add( Node.NodeName);
          Node := Node.NextSibling;
        end;
      end;
  end;
end;

procedure TfrmModuleDef.LoadModule;
begin
  FModule := G2_module_def.SelectedPatch.CreateModule( ltVA, 1, FModuleType) as TG2GraphModule;
  if assigned(FModule) then begin
    G2_module_def.SelectedPatch.AddModuleToPatch( ltVA, FModule);
    G2_module_def.SelectedPatch.SortLeds;
  end;
end;

procedure TfrmModuleDef.UnloadModule;
begin
  try
    G2_module_def.SelectedPatch.RemoveFromLedList( ltVA, FModule.ModuleIndex);
    G2_module_def.SelectedPatch.DeleteModuleFromPatch(ltVA, FModule);
  finally
    FModule := nil;
  end;
end;

procedure TfrmModuleDef.UpdateControls;
var m, p, i : integer;
    ParamNode : TDOMElement;
    Node : TDOMNode;
begin
  if FModule <> nil then
    UnloadModule;

  FModuleIndex := -1;
  for m := 0 to G2_module_def.FModuleDefList.Count - 1 do begin
    if FModuleType = G2_module_def.FModuleDefList.ModuleDef[m].ModuleType then
      FModuleIndex := m;
  end;

  if FModuleIndex <> -1 then begin
    LoadModule;
    eModuleName.Text := string(G2_module_def.FModuleDefList.ModuleDef[FModuleIndex].LongName);
    if G2_module_def.FModuleDefList.ModuleDef[FModuleIndex].Params <> nil then begin
      sgParams.ColCount := slParamAttr.Count + 1;
      sgParams.RowCount := G2_module_def.FModuleDefList.ModuleDef[FModuleIndex].Params.Count + 1;
      for p := 0 to slParamAttr.Count - 1 do
        sgParams.Cells[p + 1, 0] := slParamAttr[p];
      for p := 0 to G2_module_def.FModuleDefList.ModuleDef[FModuleIndex].Params.Count - 1 do begin
        sgParams.Cells[0, p + 1] := IntToStr(p);
        ParamNode := G2_module_def.FModuleDefList.ModuleDef[FModuleIndex].Params[p];
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
  for p := 0 to G2_module_def.FModuleDefList.ModuleDef[FModuleIndex].Params.Count - 1 do begin
    ParamNode := G2_module_def.FModuleDefList.ModuleDef[FModuleIndex].Params[p];
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

  new_filename := 'ModuleDef.xml';
  WriteXMLFile( G2_module_def.FXMLModuleDefs, new_filename);
end;

procedure TfrmModuleDef.ExtractTextEdit;
var Control : TG2GraphChildControl;
    i, j, k, l, p, ParamIndex : Integer;
    ButtonText : string;
    ParamNode : TXMLParamType;
    ParamDefNode : TXMLParamDefType;
    Node : TDOMNode;
    new_filename : string;
begin
  // I used this to get some info out of the paneld definitions
  for i := 0 to G2_module_def.FModuleDefList.Count - 1 do begin

    FModuleType := G2_module_def.FModuleDefList.ModuleDef[i].ModuleType;
    LoadModule;

    try
      for j := 0 to FModule.ParameterCount - 1 do begin
        k := 0;
        while (k < FModule.Panel.ChildControlsCount) and (FModule.Panel.GraphChildControls[k].Parameter <> FModule.Parameter[j]) do
          inc(k);

        if (k < FModule.Panel.ChildControlsCount) then begin
          Control := FModule.Panel.GraphChildControls[k];

          // Add buttontext to xml database
          if (control is TG2GraphButton) then begin
            if (Control as TG2GraphButton).ButtonText.Count > 0 then begin

              ButtonText := '';
              for l := 0 to (Control as TG2GraphButton).ButtonText.Count - 1 do
                ButtonText := ButtonText + (Control as TG2GraphButton).ButtonText[l] + ';';

              ParamIndex := (Control as TG2GraphButton).Parameter.ParamIndex;
              ParamNode := G2_module_def.FModuleDefList.ModuleDef[i].Params[ParamIndex];

              // Zoek parameter type op
              p := 0;
              while (p < G2_module_def.FParamDefList.Count) and (G2_module_def.FParamDefList.ParamDef[p].Id  <>  ParamNode.Get_Id) do
                inc(p);

              if (p < G2_module_def.FParamDefList.Count) then begin
                ParamDefNode := G2_module_def.FParamDefList.ParamDef[p];

                Node := ParamDefNode.FindNode( 'ButtonText');
                if Node <> nil then begin
                  if ButtonText = '' then begin
                    ParamDefNode.DetachChild(Node);
                    Node.Free;
                  end else
                    Node.TextContent := ButtonText;
                end else
                  if ButtonText <> '' then begin
                     Node := TDOMDocument(ParamDefNode.OwnerDocument).CreateElement('ButtonText');
                     ParamDefNode.AppendChild( Node);
                     Node.TextContent := ButtonText;
                  end;
              end;
            end;
          end;

        end;
      end;
    finally
      UnloadModule
    end;
  end;
  new_filename := 'ParamDef_new.xml';
  WriteXMLFile( G2_module_def.FXMLParamDefs, new_filename);
end;


end.
