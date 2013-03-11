unit UnitModuleDef;

//  ////////////////////////////////////////////////////////////////////////////
//
//  Copyright (C) 2011 Bruno Verhue
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//  ////////////////////////////////////////////////////////////////////////////

//  ////////////////////////////////////////////////////////////////////////////
//  Tool for editing the ModuleDef.XML file
//  ////////////////////////////////////////////////////////////////////////////

interface

uses
{$IFDEF G2_VER220_up}
  WinAPI.Windows, WinAPI.Messages, System.SysUtils, System.Variants,
  System.Classes, VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs,
  VCL.StdCtrls, VCL.Grids, VCL.ExtCtrls, VCL.ActnList, VCL.ActnMan,
  VCL.XPStyleActnCtrls, VCL.PlatformDefaultStyleActnCtrls,
{$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, Grids, ExtCtrls,
  ActnList, ActnMan, XPStyleActnCtrls,
{$ENDIF}
  DOM, XMLWrite, XMLRead, g2_types, g2_database, g2_graph, g2_file, g2_mess, g2_usb,
  g2_classes, g2_midi;

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
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure aNextModuleExecute(Sender: TObject);
    procedure aPrevModuleExecute(Sender: TObject);
    procedure aExtractModuleInfoExecute(Sender: TObject);
    procedure lbModulesClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
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
    procedure CreateSVG( aFileName : string);
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
var G2 : TG2;
    i : integer;
begin
  UpdateModuleDef;
  for i := 0 to frmG2Main.FG2List.Count - 1 do begin
    G2 := frmG2Main.FG2List[i] as TG2;
    G2.LoadModuleDefs('');
  end;
end;

procedure TfrmModuleDef.Button3Click(Sender: TObject);
begin
  CreateSVG('g2_graphics.svg');
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

procedure TfrmModuleDef.CreateSVG( aFileName : string);
var Control : TG2GraphChildControl;
    RadioButton : TG2GraphButtonRadio;
    i, j, k : integer;
    knob_section, buttontext_section, buttonflat_section, textfield_section,
    module_section, mr, mc, btc, bfc, ki, tfc : integer;
    new_filename : string;
    Doc : TXMLDocument;
    RootNode, DefsNode, GMainNode, GModuleDefNode,
    GTextFieldSectionNode, GTextFieldDefNode, TextFieldNode,
    GButtonTextSectionNode, GButtonTextDefNode, ButtonTextNode,
    GButtonFlatSectionNode, GButtonFlatDefNode, ButtonFlatNode : TDomNode;

    procedure CreateLabel( aNode : TDomNode; x, y : integer; aText : string; aFontSize : integer);
    var S : TStringStream;
    begin
      S := TStringStream.Create(
         '<g id="g2_label">'
       + '  <desc>Label for G2 editor</desc>'
       + '  <text id="g2_label_text" x="' + IntToStr(x) + '" y="' + IntToStr(y+aFontSize) + '"'
       + '        style="font-size:' + IntToStr(aFontSize) + 'px;font-style:normal;font-weight:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Sans"'
       + '        xml:space="preserve">'
       + '     <tspan id="g2_label_text_span"  x="' + IntToStr(x) + '" y="' + IntToStr(y+aFontSize) + '">'
       + aText
       + '     </tspan>'
       + '  </text>'
       + '</g>');
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    procedure CreateTextField( aNode : TDomNode; x, y, Width, Height : integer);
    var S : TStringStream;
        bw : integer;
    begin
      bw := 1;
      S := TStringStream.Create(
         '<g id="g2_textfield_' + IntToStr(Width) + 'x' + IntToStr(Height) + '">'
       + '  <desc>TextField for G2 editor</desc>'
       + '  <g id="g2_textfield_parts">'
       + '     <rect id="g2_textfield_bevel" fill="gray" stroke="none" x="' + IntToStr(x) + '" y="' + IntToStr(y) + '" width="' + IntToStr(Width)+ '" height="' + IntToStr(Height) + '" />'
       + '     <rect id="g2_textfield_window" fill="black" stroke="none" x="' + IntToStr(x+bw) + '" y="' + IntToStr(y+bw) + '" width="' + IntToStr(Width - bw*2) + '" height="' + IntToStr(Height - bw*2) + '" />'
       + '  </g>'
       + '</g>');
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    procedure CreateButtonText( aNode : TDomNode; Width, Height : integer);
    var S : TStringStream;
        bw : integer;
    begin
      bw := 1;
      S := TStringStream.Create(
         '<g id="g2_buttontext_' + IntToStr(Width) + 'x' + IntToStr(Height) + '">'
       + '  <desc>Button text for G2 editor</desc>'
       + '  <rect id="g2_buttontext_sel" x="0" y="0" width="' + IntTostr(Width) + '" height="' + IntToStr(Height) + '" fill="none" stroke="blue" stroke-width="0.2"/>'
       + '  <g id="g2_buttontext_parts">'
       + '    <rect id="g2_buttontext_face" x="0" y="0" width="' + IntTostr(Width) + '" height="' + IntToStr(Height) + '" fill="gray" stroke="none"/>'
       + '    <g id="g2_buttontext_bevel">'
       + '      <path id="g2_buttontext_bevel_lt" fill="white" stroke="none"'
       + '        d="M 0,0 h ' + IntToStr(Width) + ' v ' + IntTostr(Height)+ ' l -1,-1 v ' + IntToStr(-(Height - bw*2)) + ' h ' + IntToStr(-(Width-bw*2)) + ' z">'
       + '      </path>'
       + '      <path id="g2_buttontext_bevel_br" fill="black" stroke="none"'
       + '        d="M 0,0 v ' + IntTostr(Height)+ ' h ' + IntToStr(Width) + ' l -1,-1 h' + IntToStr(-(Width-bw*2)) + ' v' + IntToStr(-(Height - bw*2)) + ' z">'
       + '      </path>'
       + '    </g>'
       + '  </g>'
       + '</g>');
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    procedure CreateButtonFlat( aNode : TDomNode; Width, Height : integer);
    var S : TStringStream;
        bw : integer;
    begin
      bw := 1;
      S := TStringStream.Create(
         '<g id="g2_buttonflat_' + IntToStr(Width) + 'x' + IntToStr(Height) + '">'
       + '  <desc>Button flat for G2 editor</desc>'
       + '  <g id="g2_buttonflat_parts">'
       + '     <rect id="g2_buttonflat_face" fill="gray" stroke="black" x="0" y="0" width="' + IntToStr(Width) + '" height="' + IntToStr(Height) + '" />'
       + '  </g>'
       + '</g>');
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    procedure CreateKnobBig( aNode : TDomNode);
    var S : TStringStream;
    begin
      S := TStringStream.Create(
         '<g id="g2_knobbig">'
       + '  <desc>Knob big for G2 editor</desc>'
       + '  <rect id="g2_knobbig_sel" x="1" y="1" width="22" height="26" fill="none" stroke="blue" stroke-width="0.2"/>'
       + '  <g transform="translate(12,14)">'
       + '    <g>'
       + '      <path id="g2_knobbig_morph" d="M0,0 v-10 a10,10 0 0,0 -10,10 z" fill="red" stroke="none" opacity="1" />'
       + '    </g>'
       + '    <g id="g2_knobbig_parts">'
       + '      <circle id="g2_knobbig_face" cx="0" cy="0" r="10" fill="gray" stroke="blue" stroke-width="0.5" opacity="0.5"  />'
       + '      <rect id="g2_knobbig_needle" fill="blue" stroke="blue" x="-0.5" y="-10" width="1" height="9" />'
       + '    </g>'
       + '  </g>'
       + '</g>');
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    procedure CreateConnector( aNode : TDomNode);
    var S : TStringStream;
    begin
      S := TStringStream.Create(
         '<g id="g2_connector">'
       + '  <desc>Connector for G2 editor</desc>'
       + '  <rect id="g2_connector_sel" x="0" y="0" width="12" height="12" fill="none" stroke="blue" stroke-width="0.2"/>'
       + '   <g transform="translate(6,6)">'
       + '     <g id="g2_connector_parts">'
       + '       <circle id="g2_connector_border" cx="0" cy="0" r="6" fill="red" stroke="none" opacity="1"  />'
       + '       <circle id="g2_connector_hole" cx="0" cy="0" r="3" fill="black" stroke="none" opacity="1"  />'
       + '     </g>'
       + '   </g>'
       + '</g>');
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    procedure CreateModule( aNode : TDomNode);
    var GL1Node, DescNode, PanelNode, UseNode : TDOMNode;
        l, m, x, y, w, h, dx, dy : integer;
    begin
      GL1Node := Doc.CreateElement('g');
      aNode.AppendChild(Gl1Node);
      TDOMElement(GL1Node).SetAttribute('id', 'g2_module_' + IntToStr(FModule.TypeID));

      DescNode := Doc.CreateElement('desc');
      GL1Node.AppendChild(DescNode);
      DescNode.AppendChild(Doc.CreateTextNode('Module ' + IntToStr(FModule.TypeID) + ', ' + FModule.ModuleName));

      PanelNode := Doc.CreateElement('rect');
      GL1Node.AppendChild(PanelNode);
      TDOMElement(PanelNode).SetAttribute('id', 'g2_module_panel_' + IntToStr(FModule.TypeID));
      TDOMElement(PanelNode).SetAttribute('fill', 'white');
      TDOMElement(PanelNode).SetAttribute('stroke', 'black');
      TDOMElement(PanelNode).SetAttribute('x', '0');
      TDOMElement(PanelNode).SetAttribute('y', '0');
      TDOMElement(PanelNode).SetAttribute('width', IntToStr(FModule.Panel.Width));
      TDOMElement(PanelNode).SetAttribute('height', IntToStr(FModule.Panel.Height));

      for l := 0 to FModule.Panel.ChildControlsCount - 1 do begin
        Control :=  FModule.Panel.GraphChildControls[l];

        if Control is TG2GraphLabel then begin
          CreateLabel( GL1Node, Control.Left, Control.Top, (Control as TG2GraphLabel).Caption, (Control as TG2GraphLabel).Font.Size);
        end;

        if Control is TG2GraphDisplay then begin

          TextFieldNode := GTextFieldSectionNode.FirstChild;
          while (TextFieldNode <> nil) and (TDomELement(TextFieldNode).GetAttribute('id') <>  'g2_textfield_def_' + IntToStr(Control.Width) + 'x' + IntToStr(Control.Height)) do
            TextFieldNode := TextFieldNode.NextSibling;

          if not assigned(TextFieldNode) then begin
            GTextFieldDefNode := Doc.CreateElement('g');
            GTextFieldSectionNode.AppendChild(GTextFieldDefNode);
            TDOMElement(GTextFieldDefNode).SetAttribute('id', 'g2_textfield_def_' + IntToStr(Control.Width) + 'x' + IntToStr(Control.Height));
            TDOMElement(GTextFieldDefNode).SetAttribute('transform', 'translate(' + IntToStr(tfc) + ',' + IntToStr(textfield_section) + ')');
            CreateTextField( GTextFieldDefNode, 0, 0, Control.Width, Control.Height);
            tfc := tfc + Control.Width + 30;
          end;

          UseNode := Doc.CreateElement('use');
          Gl1Node.AppendChild(UseNode);
          TDOMElement(UseNode).SetAttribute('id', 'g2_module_' + IntToStr(FModule.TypeID) + '_ctrl_' + IntToStr(l));
          TDOMElement(UseNode).SetAttribute('xlink:href', '#g2_textfield_' + IntToStr(Control.Width) + 'x' + IntToStr(Control.Height));
          TDOMElement(UseNode).SetAttribute('transform', 'translate(' + IntToStr(Control.Left) + ',' + IntToStr(Control.Top) + ')');
          TDOMElement(UseNode).SetAttribute('x', '0');
          TDOMElement(UseNode).SetAttribute('y', '0');

          //CreateTextField( GL1Node, Control.Left, Control.Top, Control.Width, Control.Height);
        end;

        if Control is TG2GraphButtonText then begin

          ButtonTextNode := GButtonTextSectionNode.FirstChild;
          while (ButtonTextNode <> nil) and (TDomELement(ButtonTextNode).GetAttribute('id') <>  'g2_buttontext_def_' + IntToStr(Control.Width) + 'x' + IntToStr(Control.Height)) do
            ButtonTextNode := ButtonTextNode.NextSibling;

          if not assigned(ButtonTextNode) then begin
            GButtonTextDefNode := Doc.CreateElement('g');
            GButtonTextSectionNode.AppendChild(GButtonTextDefNode);
            TDOMElement(GButtonTextDefNode).SetAttribute('id', 'g2_buttontext_def_' + IntToStr(Control.Width) + 'x' + IntToStr(Control.Height));
            TDOMElement(GButtonTextDefNode).SetAttribute('transform', 'translate(' + IntToStr(btc) + ',' + IntToStr(buttontext_section) + ')');
            CreateButtonText( GButtonTextDefNode, Control.Width, Control.Height);
            btc := btc + Control.Width + 30;
          end;

          UseNode := Doc.CreateElement('use');
          Gl1Node.AppendChild(UseNode);
          TDOMElement(UseNode).SetAttribute('id', 'g2_module_' + IntToStr(FModule.TypeID) + '_ctrl_' + IntToStr(l));
          TDOMElement(UseNode).SetAttribute('xlink:href', '#g2_buttontext_' + IntToStr(Control.Width) + 'x' + IntToStr(Control.Height));
          TDOMElement(UseNode).SetAttribute('transform', 'translate(' + IntToStr(Control.Left) + ',' + IntToStr(Control.Top) + ')');
          TDOMElement(UseNode).SetAttribute('x', '0');
          TDOMElement(UseNode).SetAttribute('y', '0');
        end;

        if Control is TG2GraphButtonFlat then begin

          ButtonFlatNode := GButtonFlatSectionNode.FirstChild;
          while (ButtonFlatNode <> nil) and (TDomELement(ButtonFlatNode).GetAttribute('id') <>  'g2_buttonflat_def_' + IntToStr(Control.Width) + 'x' + IntToStr(Control.Height)) do
            ButtonFlatNode := ButtonFlatNode.NextSibling;

          if not assigned(ButtonFlatNode) then begin
            GButtonFlatDefNode := Doc.CreateElement('g');
            GButtonFlatSectionNode.AppendChild(GButtonFlatDefNode);
            TDOMElement(GButtonFlatDefNode).SetAttribute('id', 'g2_buttonflat_def_' + IntToStr(Control.Width) + 'x' + IntToStr(Control.Height));
            TDOMElement(GButtonFlatDefNode).SetAttribute('transform', 'translate(' + IntToStr(bfc) + ',' + IntToStr(buttonflat_section) + ')');
            CreateButtonFlat( GButtonFlatDefNode, Control.Width, Control.Height);
            bfc := bfc + Control.Width + 30;
          end;

          UseNode := Doc.CreateElement('use');
          Gl1Node.AppendChild(UseNode);
          TDOMElement(UseNode).SetAttribute('id', 'g2_module_' + IntToStr(FModule.TypeID) + '_ctrl_' + IntToStr(l));
          TDOMElement(UseNode).SetAttribute('xlink:href', '#g2_buttonflat_' + IntToStr(Control.Width) + 'x' + IntToStr(Control.Height));
          TDOMElement(UseNode).SetAttribute('transform', 'translate(' + IntToStr(Control.Left) + ',' + IntToStr(Control.Top) + ')');
          TDOMElement(UseNode).SetAttribute('x', '0');
          TDOMElement(UseNode).SetAttribute('y', '0');
        end;

        if Control is TG2GraphButtonRadio then begin
          RadioButton := Control as TG2GraphButtonRadio;

          if RadioButton.ButtonCount > 0 then begin

            if RadioButton.Orientation = otHorizontal then begin
              w := RadioButton.Width div RadioButton.ButtonCount - 1;
              h := RadioButton.Height;
              if RadioButton.UpsideDown then begin
                dx := w;
                dy := 0;
              end else begin
                dx := -w;
                dy := 0;
              end;
            end else begin
              w := RadioButton.Width;
              h := RadioButton.Height div RadioButton.ButtonCount - 1;
              if RadioButton.UpsideDown then begin
                dx := 0;
                dy := h;
              end else begin
                dx := 0;
                dy := -h;
              end;
            end;

            ButtonFlatNode := GButtonFlatSectionNode.FirstChild;
            while (ButtonFlatNode <> nil) and (TDomELement(ButtonFlatNode).GetAttribute('id') <>  'g2_buttonflat_def_' + IntToStr(w) + 'x' + IntToStr(h)) do
              ButtonFlatNode := ButtonFlatNode.NextSibling;

            if not assigned(ButtonFlatNode) then begin
              GButtonFlatDefNode := Doc.CreateElement('g');
              GButtonFlatSectionNode.AppendChild(GButtonFlatDefNode);
              TDOMElement(GButtonFlatDefNode).SetAttribute('id', 'g2_buttonflat_def_' + IntToStr(w) + 'x' + IntToStr(h));
              TDOMElement(GButtonFlatDefNode).SetAttribute('transform', 'translate(' + IntToStr(bfc) + ',' + IntToStr(buttonflat_section) + ')');
              CreateButtonFlat( GButtonFlatDefNode, w, h);
              bfc := bfc + Control.Width + 30;
            end;

            x := RadioButton.Left;
            y := RadioButton.Top;
            for m := 0 to RadioButton.ButtonCount - 1 do begin
              UseNode := Doc.CreateElement('use');
              Gl1Node.AppendChild(UseNode);
              TDOMElement(UseNode).SetAttribute('id', 'g2_module_' + IntToStr(FModule.TypeID) + '_ctrl_' + IntToStr(l) + '_el_'+ IntToStr(m));
              TDOMElement(UseNode).SetAttribute('xlink:href', '#g2_buttonflat_' + IntToStr(w) + 'x' + IntToStr(h));
              TDOMElement(UseNode).SetAttribute('transform', 'translate(' + IntToStr(x) + ',' + IntToStr(y) + ')');
              TDOMElement(UseNode).SetAttribute('x', '0');
              TDOMElement(UseNode).SetAttribute('y', '0');
              x := x + dx;
              y := y + dy;
            end;
          end;
        end;

        if Control is TG2GraphKnob then begin
          UseNode := Doc.CreateElement('use');
          Gl1Node.AppendChild(UseNode);
          TDOMElement(UseNode).SetAttribute('id', 'g2_module_' + IntToStr(FModule.TypeID) + '_ctrl_' + IntToStr(l));
          TDOMElement(UseNode).SetAttribute('xlink:href', '#g2_knobbig');
          TDOMElement(UseNode).SetAttribute('transform', 'translate(' + IntToStr(Control.Left) + ',' + IntToStr(Control.Top) + ')');
          TDOMElement(UseNode).SetAttribute('x', '0');
          TDOMElement(UseNode).SetAttribute('y', '0');
        end;

        if Control is TG2GraphConnector then begin
          UseNode := Doc.CreateElement('use');
          Gl1Node.AppendChild(UseNode);
          TDOMElement(UseNode).SetAttribute('id', 'g2_module_' + IntToStr(FModule.TypeID) + '_ctrl_' + IntToStr(l));
          TDOMElement(UseNode).SetAttribute('xlink:href', '#g2_connector');
          TDOMElement(UseNode).SetAttribute('transform', 'translate(' + IntToStr(Control.Left) + ',' + IntToStr(Control.Top) + ')');
          TDOMElement(UseNode).SetAttribute('x', '0');
          TDOMElement(UseNode).SetAttribute('y', '0');
        end;
      end;
    end;

begin
  Doc := TXMLDocument.Create;
  try
    RootNode := Doc.CreateElement('svg');
    Doc.AppendChild(RootNode);
    TDOMElement(RootNode).SetAttribute('xmlns:dc','http://purl.org/dc/elements/1.1/');
    TDOMElement(RootNode).SetAttribute('xmlns:cc','http://creativecommons.org/ns#');
    TDOMElement(RootNode).SetAttribute('xmlns:rdf','http://www.w3.org/1999/02/22-rdf-syntax-ns#');
    TDOMElement(RootNode).SetAttribute('xmlns:svg','http://www.w3.org/2000/svg');
    TDOMElement(RootNode).SetAttribute('xmlns','http://www.w3.org/2000/svg');
    TDOMElement(RootNode).SetAttribute('xmlns:xlink','http://www.w3.org/1999/xlink');

    DefsNode := Doc.CreateElement('defs');
    RootNode.AppendChild( DefsNode);
    TDOMElement(DefsNode).SetAttribute('id', 'g2_defs');

    knob_section := 0;
    buttontext_section := 50;
    buttonflat_section := 100;
    textfield_section := 150;
    module_section := 200;
    mr := 0;
    mc := 0;
    btc := 0;
    bfc := 0;
    ki := 0;
    tfc := 0;

    CreateKnobBig( DefsNode);
    CreateConnector( DefsNode);
    //CreateButton( DefsNode, 12, 12);

    GMainNode := Doc.CreateElement('g');
    RootNode.AppendChild(GMainNode);
    TDOMElement(GMainNode).SetAttribute('id', 'g2_main');

    GTextFieldSectionNode := Doc.CreateElement('g');
    GMainNode.AppendChild(GTextFieldSectionNode);
    TDOMElement(GTextFieldSectionNode).SetAttribute('id', 'g2_textfield_section');

    GButtonTextSectionNode := Doc.CreateElement('g');
    GMainNode.AppendChild(GButtonTextSectionNode);
    TDOMElement(GButtonTextSectionNode).SetAttribute('id', 'g2_buttontext_section');

    GButtonFlatSectionNode := Doc.CreateElement('g');
    GMainNode.AppendChild(GButtonFlatSectionNode);
    TDOMElement(GButtonFlatSectionNode).SetAttribute('id', 'g2_buttonflat_section');

    for i := 0 to G2_module_def.FModuleDefList.Count - 1 do begin

      GModuleDefNode := Doc.CreateElement('g');
      GMainNode.AppendChild(GModuleDefNode);
      TDOMElement(GModuleDefNode).SetAttribute('id', 'g2_module_def_' + IntToStr(i));
      TDOMElement(GModuleDefNode).SetAttribute('transform', 'translate(' + IntToStr(mc * 300) + ',' + IntToStr(module_section + mr*80) + ')');

      inc(mr);
      if mr > 21 then begin
        inc(mc);
        mr := 0;
      end;

      FModuleType := G2_module_def.FModuleDefList.ModuleDef[i].ModuleType;
      LoadModule;
      try
        CreateModule( GModuleDefNode);

        for j := 0 to FModule.Panel.ChildControlsCount - 1 do begin
          Control :=  FModule.Panel.GraphChildControls[j];

          // Extract bitmaps
          if Control is TG2GraphBitmap then begin
            for k := 0 to (Control as TG2GraphBitmap).ImageList.Count - 1 do begin
              (Control as TG2GraphBitmap).ImageList.Items[k].SaveToFile('C:\Users\Bruno\Delphi\nmg2editor\v0.25\ExtrImg\Module_' + IntToStr(FModule.TypeID) + '_ControlID_' + IntToStr(Control.ID) + '_No_' +  IntToStr(k) + '.bmp');
            end;
          end;

          if Control is TG2GraphButton then begin
            for k := 0 to (Control as TG2GraphButton).ImageList.Count - 1 do begin
              (Control as TG2GraphButton).ImageList.Items[k].SaveToFile('C:\Users\Bruno\Delphi\nmg2editor\v0.25\ExtrImg\Module_' + IntToStr(FModule.TypeID) + '_ControlID_' + IntToStr(Control.ID) + '_No_' +  IntToStr(k) + '.bmp');
            end;
          end;

          if Control is TG2GraphPartSelector then begin
            for k := 0 to (Control as TG2GraphPartSelector).ImageList.Count - 1 do begin
              (Control as TG2GraphPartSelector).ImageList.Items[k].SaveToFile('C:\Users\Bruno\Delphi\nmg2editor\v0.25\ExtrImg\Module_' + IntToStr(FModule.TypeID) + '_ControlID_' + IntToStr(Control.ID) + '_No_' +  IntToStr(k) + '.bmp');
            end;
          end;

        end;

      finally
        UnloadModule
      end;
    end;
    WriteXMLFile( Doc, aFileName);
  finally
    Doc.Free;
  end;
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
