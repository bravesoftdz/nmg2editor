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
  System.Classes, System.Contnrs, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, VCL.Grids, VCL.ExtCtrls, VCL.ActnList, VCL.ActnMan,
  VCL.XPStyleActnCtrls, VCL.PlatformDefaultStyleActnCtrls,
{$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, Grids, ExtCtrls,
  ActnList, ActnMan, XPStyleActnCtrls, Contnrs,
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
    procedure CreateSVG;
    procedure LoadModule;
    procedure UnloadModule;
  end;

var
  frmModuleDef: TfrmModuleDef;

implementation

{$R *.dfm}

uses UnitG2Editor, graph_util_vcl;

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
  CreateSVG;
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

procedure TfrmModuleDef.CreateSVG;
const
  cBtnFace = '#c8b7b7';
  cBtnSideDark = '#6c5353';
  cBtnSideMedium = '#ac9393';
  cBtnSideLight = '#e3dbdb';

  cKnobFace = '#ececec';
  cKnobBorder = '#000000';

  cSldrKnobFace = '#333333';
  cSldrKnobSideDark = '#000000';
  cSldrKnobSideMedium = '#4d4d4d';
  cSldrKnobSideLight = '#e6e6e6';

  cPartSelWindow = '#e9c6af';
  cPartSelBtn = '#deaa87';

  idSymbol = 'symbol';
  idLabel = 'label';
  idLedGreen = 'ledGreen';
  idLedSequencer = 'ledSeq';
  idTxtField = 'txtField';
  idBtnText = 'btnText';
  idBtnFlat = 'btnFlat';
  idBtnIncDecHorz = 'btnIncDecHorz';
  idBtnIncDecVert = 'btnIncDecVert';
  idSlider = 'slider';
  idKnobBig = 'knobBig';
  idKnobMedium = 'knobMedium';
  idKnobResetMedium = 'knobResetmedium';
  idKnobReset = 'knobReset';
  idKnobSmall = 'knobSmall';
  idConnector = 'connRedIn';
  idPartSel = 'partSel';
  idModule = 'module';

var Control : TG2GraphChildControl;
    RadioButton : TG2GraphButtonRadio;
    BitMapList : TObjectList;
    Bitmap : TBitmap;
    i, j, k : integer;
    symbol_section, knob_section, buttontext_section, buttonflat_section,
    textfield_section, led_section, btnIncDec_section, partsel_section,
    module_section, mr, mc, btc, bfc, kc, tfc, sc, lc, bidc, psc : integer;
    new_filename : string;
    Doc : TXMLDocument;
    RootNode, DefsNode, GMainNode,
    GSymbolSectionNode, GSymbolDefNode, SymbolNode,
    GTextFieldSectionNode, GTextFieldDefNode, TextFieldNode,
    GButtonTextSectionNode, GButtonTextDefNode, ButtonTextNode,
    GButtonFlatSectionNode, GButtonFlatDefNode, ButtonFlatNode,
    GKnobSectionNode, GKnobDefNode, KnobNode,
    GLedSectionNode, GLedDefNode, LedNode,
    GBtnIncDecSectionNode, GBtnIncDecDefNode, BtnIncDecNode,
    GPartSelSectionNode, GPartSelDefNode, PartSelNode,
    GModuleSectionNode, GModuleDefNode, ModuleNode : TDomNode;

    function FindBitmap( aBitmap : TBitmap): integer;
    begin
      Result := 0;
      while (Result < BitMapList.Count) and not(CompareBitmap( aBitmap, BitMapList.Items[Result] as TBitmap)) do
        inc(Result);

      if (Result >= BitMapList.Count) then
        Result := -1;
    end;

    function NiceBevel( aBaseName : string; aWidth, aHeight, aBevelWidth, aBevelHeight : single; aColor1, aColor2 : string): string;
    var dw, dh : single;
    begin
      dw := aBevelWidth / 1.5;
      dh := aBevelHeight / 1.5;

      Result :=
         '<path id="' + aBaseName + '_bevel_l" fill="' + aColor1 + '" stroke="none"'
       + '  d="M 0,0'
           + ' c 0,0 ' + FloatToStr(aBevelWidth) + ','  + FloatToStr(dw) + ' ' + FloatToStr(aBevelWidth) + ',' + FloatToStr(aBevelWidth+dw)
             + ' 0,' + FloatToStr(aBevelWidth + dw) + ' 0,' + FloatToStr(aHeight - (aBevelWidth+dw)*2 - dw - aBevelWidth) + ' 0,' + FloatToStr(aHeight - (aBevelWidth+dw)*2)
             + ' 0,' + FloatToStr(dw) + ' ' + FloatToStr(-aBevelWidth) + ',' + FloatToStr(aBevelWidth+dw) + ' ' + FloatToStr(-aBevelWidth) + ',' + FloatToStr(aBevelWidth+dw)
           + ' z">'
       + '</path>'
       + '<path id="' + aBaseName + '_bevel_t" fill="' + aColor1 + '" stroke="none"'
       + '  d="M ' + FloatToStr(aWidth) + ',0'
           + ' c 0,0 ' + FloatToStr(-dh) + ','  + FloatToStr(aBevelHeight) + ' ' + FloatToStr(-aBevelHeight-dh) + ',' + FloatToStr(aBevelHeight)
             + ' ' + FloatToStr(-aBevelHeight - dh) + ',0 ' + FloatToStr(-aWidth + (aBevelHeight+dh)*2 + dh + aBevelHeight) + ',0 ' + FloatToStr(-aWidth + (aBevelHeight+dh)*2) + ',0'
             + ' ' + FloatToStr(-dh) + ',0 ' + FloatToStr(-aBevelHeight-dh) + ',' + FloatToStr(-aBevelHeight) + ' ' + FloatToStr(-aBevelHeight-dh) + ',' + FloatToStr(-aBevelHeight)
           + ' z">'
       + '</path>'
       + '<path id="' + aBaseName + '_bevel_r" fill="' + aColor2 + '" stroke="none"'
       + '  d="M ' + FloatToStr(aWidth) + ',' + FloatToStr(aHeight)
           + ' c 0,0 ' + FloatToStr(-aBevelWidth) + ','  + FloatToStr(-dw) + ' ' + FloatToStr(-aBevelWidth) + ',' + FloatToStr(-aBevelWidth-dw)
             + ' 0,' + FloatToStr(-aBevelWidth - dw) + ' 0,' + FloatToStr(-aHeight + (aBevelWidth+dw)*2 + dw + aBevelWidth) + ' 0,' + FloatToStr(-aHeight + (aBevelWidth+dw)*2)
             + ' 0,' + FloatToStr(-dw) + ' ' + FloatToStr(aBevelWidth) + ',' + FloatToStr(-aBevelWidth-dw) + ' ' + FloatToStr(aBevelWidth) + ',' + FloatToStr(-aBevelWidth-dw)
           + ' z">'
       + '</path>'
       + '<path id="' + aBaseName + '_bevel_b" fill="' + aColor2 + '" stroke="none"'
       + '  d="M 0,' + FloatToStr(aHeight)
           + ' c 0,0 ' + FloatToStr(dh) + ','  + FloatToStr(-aBevelHeight) + ' ' + FloatToStr(aBevelHeight+dh) + ',' + FloatToStr(-aBevelHeight)
             + ' ' + FloatToStr(aBevelHeight + dh) + ',0 ' + FloatToStr(aWidth - (aBevelHeight+dh)*2 - dh - aBevelHeight) + ',0 ' + FloatToStr(aWidth - (aBevelHeight+dh)*2) + ',0'
             + ' ' + FloatToStr(dh) + ',0 ' + FloatToStr(aBevelHeight+dh) + ',' + FloatToStr(aBevelHeight) + ' ' + FloatToStr(aBevelHeight+dh) + ',' + FloatToStr(aBevelHeight)
           + ' z">'
       + '</path>';
    end;

    function Bevel( aBaseName : string; aWidth, aHeight, aBevelWidth : integer; aColor1, aColor2 : string): string;
    begin
       Result :=
         '<path id="' + aBaseName + '_bevel_l" fill="' + aColor1 + '" stroke="none"'
       + '  d="M 0,0 l ' + IntToStr(aBevelWidth) + ','  + IntToStr(aBevelWidth) + ' v ' + IntToStr((aHeight - aBevelWidth*2)) + ' l ' + IntToStr(-aBevelWidth) + ','  + IntToStr(aBevelWidth) + ' z">'
       + '</path>'
       + '<path id="' + aBaseName + '_bevel_t" fill="' + aColor1 + '" stroke="none"'
       + '  d="M 0,0 h ' + IntToStr(aWidth) + ' l ' + IntToStr(-aBevelWidth) + ','  + IntToStr(aBevelWidth) + ' h ' + IntToStr(-(aWidth - aBevelWidth*2)) + ' z">'
       + '</path>'
       + '<path id="' + aBaseName + '_bevel_r" fill="' + aColor2 + '" stroke="none"'
       + '  d="M ' + IntToStr(aWidth) + ',0 v ' + IntToStr(aHeight) + ' l ' + IntToStr(-aBevelWidth) + ','  + IntToStr(-aBevelWidth) + ' v ' + IntToStr(-(aHeight - aBevelWidth*2)) + ' z">'
       + '</path>'
       + '<path id="' + aBaseName + '_bevel_b" fill="' + aColor2 + '" stroke="none"'
       + '  d="M 0, ' + IntToStr(aHeight) + ' l ' + IntToStr(aBevelWidth) + ','  + IntToStr(-aBevelWidth) + ' h ' + IntToStr((aWidth - aBevelWidth*2)) + ' l ' + IntToStr(aBevelWidth) + ','  + IntToStr(aBevelWidth) +' z">'
       + '</path>';
    end;

    procedure CreateLabel( aNode : TDomNode; x, y : integer; aText : string; aFontSize : integer);
    var S : TStringStream;
        id : string;
    begin
      id := idLabel;

      S := TStringStream.Create(
         '<g id="' + id + '">'
       + '  <desc>Label for G2 editor</desc>'
       + '  <text id="' + id + '_text' + '" x="' + IntToStr(x) + '" y="' + IntToStr(y+aFontSize) + '"'
       + '        style="font-size:' + IntToStr(aFontSize) + 'px;font-style:normal;font-weight:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Sans"'
       + '        xml:space="preserve">'
       + '     <tspan id="' + id + '_span"  x="' + IntToStr(x) + '" y="' + IntToStr(y+aFontSize) + '">'
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

    function Use( aID, aRef : string; dx, dy : single): string;
    begin
      Result :=
         ' <use'
       + ' id="' + aID + '"'
       + ' xlink:href="#' + aRef + '"'
       + ' transform="translate(' + FloatToStr(dx) + ',' + FloatToStr(dy) + ')"/>'
    end;

    procedure CreateUse( aNode : TDomNode; aID, aRef : string; dx, dy : integer);
    var UseNode : TDomNode;
    begin
      UseNode := Doc.CreateElement('use');
      aNode.AppendChild(UseNode);
      TDOMElement(UseNode).SetAttribute('id', aID);
      TDOMElement(UseNode).SetAttribute('xlink:href', '#' + aRef);
      TDOMElement(UseNode).SetAttribute('transform', 'translate(' + IntToStr(dx) + ',' + IntToStr(dy) + ')');
      TDOMElement(UseNode).SetAttribute('x', '0');
      TDOMElement(UseNode).SetAttribute('y', '0');
    end;

    procedure CreateSymbol( aNode : TDomNode; index : string; aWidth, aHeight : integer);
    var S : TStringStream;
        id : string;
    begin
      id := idSymbol + '_' + index;
      S := TStringStream.Create(
         '<g id="' + id + '">'
       + '  <desc>Symbol for G2 editor</desc>'
       + '  <g id="' + id + '_sel">'
       + '     <rect id="' + id + '_rect" fill="none" stroke="blue" stroke-width="0.2" x="0" y="0" width="' + IntToStr(aWidth)+ '" height="' + IntToStr(aHeight) + '" />'
       + '  </g>'
       + '</g>');
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    function AddSymbolFromBitmap( aBitmap : TBitmap): integer;
    var id : string;
    begin
      Result := FindBitmap(aBitmap);
      if Result = - 1 then begin

        Bitmap := TBitmap.Create;
        Bitmap.Assign(aBitmap);
        BitMapList.Add(Bitmap);
        Result := BitmapList.Count -1;

        id := idSymbol + '_' + IntToStr(Result) + '_def';

        SymbolNode := GSymbolSectionNode.FirstChild;
        while (SymbolNode <> nil) and (TDomELement(SymbolNode).GetAttribute('id') <>  id) do
          SymbolNode := SymbolNode.NextSibling;

        if not assigned(SymbolNode) then begin
          GSymbolDefNode := Doc.CreateElement('g');
          GSymbolSectionNode.AppendChild(GSymbolDefNode);
          TDOMElement(GSymbolDefNode).SetAttribute('id', id);
          TDOMElement(GSymbolDefNode).SetAttribute('transform', 'translate(' + IntToStr(sc) + ',' + IntToStr(symbol_section) + ')');
          CreateSymbol( GSymbolDefNode, IntToStr(Result), (BitmapList[Result] as TBitmap).Width, (BitmapList[Result] as TBitmap).Height);
          sc := sc + (BitmapList[Result] as TBitmap).Width + 20;
        end;
      end;
    end;

    procedure CreateLedSeq( aNode : TDomNode);
    var S : TStringStream;
        bw : integer;
        id : string;
    begin
      id := idLedSequencer;

      bw := 1;
      S := TStringStream.Create(
         '<g id="' + id + '">'
       + '  <desc>Led sequencer for G2 editor</desc>'
       + '  <g id="' + id + '_parts">'
       + '     <rect id="' + id + '_bevel" fill="black" stroke="none" x="0" y="0" width="12" height="6" />'
       + '     <rect id="' + id + '_window" fill="green" stroke="none" x="' + IntToStr(bw) + '" y="' + IntToStr(bw) + '" width="' + IntToStr(12 - bw*2) + '" height="' + IntToStr(6 - bw*2) + '" />'
       + '  </g>'
       + '</g>');
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    procedure CreateLedGreen( aNode : TDomNode);
    var S : TStringStream;
        bw : integer;
        id : string;
    begin
      id := idLedGreen;

      bw := 1;
      S := TStringStream.Create(
         '<g id="' + id + '">'
       + '  <desc>Led green for G2 editor</desc>'
       + '  <g id="' + id + '_parts">'
       + '     <rect id="' + id + '_bevel" fill="black" stroke="none" x="0" y="0" width="7" height="7" />'
       + '     <rect id="' + id + '_window" fill="green" stroke="none" x="' + IntToStr(bw) + '" y="' + IntToStr(bw) + '" width="' + IntToStr(7 - bw*2) + '" height="' + IntToStr(7 - bw*2) + '" />'
       + '  </g>'
       + '</g>');
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    function GetIDTxtField( aWidth, aHeight : single): string;
    begin
      Result := idTxtField + '_' + FloatToStr(aWidth) + 'x' + FloatToStr(aHeight);
    end;

    procedure CreateTextField( aNode : TDomNode; x, y, Width, Height : single);
    var S : TStringStream;
        id : string;
        bw : integer;
    begin
      bw := 1;
      id := GetIDTxtField( Width, Height);
      S := TStringStream.Create(
         '<g id="' + id + '">'
       + '  <desc>TextField for G2 editor</desc>'
       + '  <g id="' + id + '_parts">'
       + '     <rect id="' + id + '_bevel" fill="gray" stroke="none" x="' + FloatToStr(x) + '" y="' + FloatToStr(y) + '" width="' + FloatToStr(Width)+ '" height="' + FloatToStr(Height) + '" />'
       + '     <rect id="' + id + '_window" fill="black" stroke="none" x="' + FloatToStr(x+bw) + '" y="' + FloatToStr(y+bw) + '" width="' + FloatToStr(Width - bw*2) + '" height="' + FloatToStr(Height - bw*2) + '" />'
       + '  </g>'
       + '</g>');
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    procedure FindOrAddTextField( aWidth, aHeight : single);
    var id : string;
    begin
      id := idTxtField + '_def_' + FloatToStr(aWidth) + 'x' + FloatToStr(aHeight);

      TextFieldNode := GTextFieldSectionNode.FirstChild;
      while (TextFieldNode <> nil) and (TDomELement(TextFieldNode).GetAttribute('id') <>  id) do
        TextFieldNode := TextFieldNode.NextSibling;

      if not assigned(TextFieldNode) then begin
        GTextFieldDefNode := Doc.CreateElement('g');
        GTextFieldSectionNode.AppendChild(GTextFieldDefNode);
        TDOMElement(GTextFieldDefNode).SetAttribute('id', id);
        TDOMElement(GTextFieldDefNode).SetAttribute('transform', 'translate(' + IntToStr(tfc) + ',' + IntToStr(textfield_section) + ')');
        CreateTextField( GTextFieldDefNode, 0, 0, Control.Width, Control.Height);
        tfc := tfc + Control.Width + 30;
      end;
    end;

    function GetIDBtnText( aBaseName : string; aWidth, aHeight, aBevelWidth : single): string;
    begin
      Result :=  aBaseName + '_' + FloatToStr(aWidth) + 'x' + FloatToStr(aHeight) + 'x' + FloatToStr(aBevelWidth);
    end;

    function NiceButton( aBaseName : string; aWidth, aHeight, aBevelWidth, aBevelHeight : single; aColSideLight, aColSideDark, aColBG, aColFace : string ): string;
    var id : string;
    begin
       id := GetIDBtnText( aBaseName, aWidth, aHeight, aBevelWidth);
       Result :=
         '<g id="' + id + '">'

       + '  <desc>Button text for G2 editor</desc>'

       + '  <rect id="' + id + '_sel" x="0" y="0" width="' + FloatToStr(aWidth) + '" height="' + FloatToStr(aHeight) + '" fill="none" stroke="blue" stroke-width="0.2"/>'

       + '  <g id="' + id + '_parts">'

       + '    <rect id="' + id + '_bg" x="0" y="0" width="' + FloatToStr(aWidth) + '" height="' + FloatToStr(aHeight) + '" fill="' + aColBG + '" stroke="black" stroke-width="0.5"/>'

       + '    <g id="' + id + '_bevel">'
       + NiceBevel( id, aWidth, aHeight, aBevelWidth, aBevelHeight, aColSideLight, aColSideDark)
       + '    </g>'

       + '    <rect id="' + id + '_face" x="' + FloatToStr(aBevelWidth) + '" y="' + FloatToStr(aBevelHeight) + '" width="' + FloatToStr(aWidth-aBevelWidth*2) + '" height="' + FloatToStr(aHeight-aBevelHeight*2) + '" rx="0.75" fill="' + aColFace + '" stroke="none"/>'

       + '  </g>'
       + '</g>';
    end;

    procedure CreateButtonText( aNode : TDomNode; aWidth, aHeight, aBevelWidth : single);
    var S : TStringStream;
        bw : integer;
    begin
      bw := 1;
      {S := TStringStream.Create(
         '<g id="g2_buttontext_' + IntToStr(Width) + 'x' + IntToStr(Height) + '">'
       + '  <desc>Button text for G2 editor</desc>'
       + '  <rect id="g2_buttontext_sel" x="0" y="0" width="' + IntTostr(Width) + '" height="' + IntToStr(Height) + '" fill="none" stroke="blue" stroke-width="0.2"/>'
       + '  <g id="g2_buttontext_parts">'
       + '    <rect id="g2_buttontext_face" x="0" y="0" width="' + IntTostr(Width) + '" height="' + IntToStr(Height) + '" fill="lightgray" stroke="none"/>'
       + '    <g id="g2_buttontext_bevel">'
       + Bevel( 'g2_buttontext', Width, Height, bw, 'white', 'darkgray')
       + '    </g>'
       + '  </g>'
       + '</g>');}
       S := TStringStream.Create( NiceButton( idBtnText, aWidth, aHeight, aBevelWidth, aBevelWidth, cBtnSideLight, cBtnSideDark, cBtnSideMedium, cBtnFace));
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    procedure FindOrAddButtonText( aWidth, aHeight, aBevelWidth : single);
    var id : string;
    begin
      id := idBtnText + '_def_' + FloatToStr(aWidth) + 'x' + FloatToStr(aHeight) + 'x' + FloatToStr(aBevelWidth);

      ButtonTextNode := GButtonTextSectionNode.FirstChild;
      while (ButtonTextNode <> nil) and (TDomELement(ButtonTextNode).GetAttribute('id') <> id) do
        ButtonTextNode := ButtonTextNode.NextSibling;

      if not assigned(ButtonTextNode) then begin
        GButtonTextDefNode := Doc.CreateElement('g');
        GButtonTextSectionNode.AppendChild(GButtonTextDefNode);
        TDOMElement(GButtonTextDefNode).SetAttribute('id', id);
        TDOMElement(GButtonTextDefNode).SetAttribute('transform', 'translate(' + IntToStr(btc) + ',' + IntToStr(buttontext_section) + ')');
        CreateButtonText( GButtonTextDefNode, aWidth, aHeight, aBevelWidth);
        btc := btc + trunc(aWidth) + 20;
      end;
    end;

    procedure CreateBtnIncDecHorz( aNode : TDomNode);
    var S : TStringStream;
        w, h, d : single;
    begin
      w := 11;
      h := 11;
      d := 1;
      FindOrAddButtonText(w, h, d);

      S := TStringStream.Create(
         '<g id="' + idBtnIncDecHorz + '">'
       + ' <desc>Button inc-dec horizontal for G2 editor</desc>'
       + ' <g id="' + idBtnIncDecHorz + '_parts">'

       + Use( idBtnIncDecHorz + '_dec', GetIDBtnText( idBtnText, w, h, d), 0, 0)
       + Use( idBtnIncDecHorz + '_inc', GetIDBtnText( idBtnText, w, h, d), w, 0)

       + ' </g>'
       + '</g>');
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    procedure CreateBtnIncDecVert( aNode : TDomNode);
    var S : TStringStream;
        w, h, d : single;
    begin
      w := 11;
      h := 9;
      d := 1;
      FindOrAddButtonText(w, h, d);

      S := TStringStream.Create(
         '<g id="' + idBtnIncDecVert + '">'
       + ' <desc>Button inc-dec vertical for G2 editor</desc>'
       + ' <g id="g2_btnIncDecVert_parts">'

       + Use( idBtnIncDecVert + '_dec', GetIDBtnText( idBtnText, w, h, d), 0, 0)
       + Use( idBtnIncDecVert + '_inc', GetIDBtnText( idBtnText, w, h, d), 0, h)

       + ' </g>'
       + '</g>');
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    function GetIDBtnFlat( aWidth, aHeight : single): string;
    begin
      Result := idBtnFlat + '_' + FloatToStr(aWidth) + 'x' + FloatToStr(aHeight);
    end;

    procedure CreateButtonFlat( aNode : TDomNode; aWidth, aHeight : integer);
    var S : TStringStream;
        bw : integer;
        id : string;
    begin
      bw := 1;
      id := GetIDBtnFlat( aWidth, aHeight);
      S := TStringStream.Create(
         '<g id="' + id + '">'
       + '  <desc>Button flat for G2 editor</desc>'
       + '  <g id="' + id + '_parts">'
       + '     <rect id="' + id + '_face" fill="lightgray" stroke="black" x="0" y="0" width="' + IntToStr(aWidth) + '" height="' + IntToStr(aHeight) + '" />'
       + '  </g>'
       + '</g>');
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    procedure FindOrAddButtonFlat( aWidth, aHeight : integer);
    var id : string;
    begin
      id := idBtnFlat + '_def_'  + IntToStr(aWidth) + 'x' + IntToStr(aHeight);

       ButtonFlatNode := GButtonFlatSectionNode.FirstChild;
       while (ButtonFlatNode <> nil) and (TDomELement(ButtonFlatNode).GetAttribute('id') <>  id) do
         ButtonFlatNode := ButtonFlatNode.NextSibling;

       if not assigned(ButtonFlatNode) then begin
         GButtonFlatDefNode := Doc.CreateElement('g');
         GButtonFlatSectionNode.AppendChild(GButtonFlatDefNode);
         TDOMElement(GButtonFlatDefNode).SetAttribute('id', id);
         TDOMElement(GButtonFlatDefNode).SetAttribute('transform', 'translate(' + IntToStr(bfc) + ',' + IntToStr(buttonflat_section) + ')');
         CreateButtonFlat( GButtonFlatDefNode, aWidth, aHeight);
         bfc := bfc + aWidth + 30;
       end;
    end;

    function GetIDPartSel( aWidth, aHeight : single): string;
    begin
      Result := idPartSel + '_' + FloatToStr(aWidth) + 'x' + FloatToStr(aHeight);
    end;

    procedure CreatePartSelector( aNode : TDomNode; aWidth, aHeight : integer);
    var S : TStringStream;
        bw : integer;
        id : string;
    begin
      id := GetIDPartSel( aWidth, aHeight);
      bw := 9;
      S := TStringStream.Create(
         '<g id="' + id + '">'
       + '  <desc>Part selector for G2 editor</desc>'
       + '  <g id="' + id + '_parts">'
       + '     <rect id="' + id + '_window" fill="' + cPartselWindow + '" stroke="black" x="0" y="0" width="' + IntToStr(aWidth) + '" height="' + IntToStr(aHeight) + '" />'
       + '     <rect id="' + id + '_button" fill="' + cPartselBtn + '" stroke="black" x="' +  IntToStr(aWidth - bw) + '" y="0" width="' + IntToStr(bw) + '" height="' + IntToStr(aHeight) + '" />'
       + '  </g>'
       + '</g>');
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    procedure FindOrAddPartSel( aWidth, aHeight : integer);
    var id : string;
    begin
      id := idPartsel + '_def_'  + IntToStr(aWidth) + 'x' + IntToStr(aHeight);

       PartSelNode := GPartSelSectionNode.FirstChild;
       while (PartSelNode <> nil) and (TDomELement(PartSelNode).GetAttribute('id') <>  id) do
         PartSelNode := PartSelNode.NextSibling;

       if not assigned(PartSelNode) then begin
         GPartSelDefNode := Doc.CreateElement('g');
         GPartSelSectionNode.AppendChild(GPartSelDefNode);
         TDOMElement(GPartSelDefNode).SetAttribute('id', id);
         TDOMElement(GPartSelDefNode).SetAttribute('transform', 'translate(' + IntToStr(psc) + ',' + IntToStr(partsel_section) + ')');
         CreatePartSelector( GPartSelDefNode, aWidth, aHeight);
         psc := psc + aWidth + 30;
       end;
    end;

    function Gradient( id : string; x1, y1, x2, y2 : single; Color1, Color2 : string ): string;
    begin
      Result :=
        '<linearGradient id="' + id + '" gradientUnits="userSpaceOnUse" x1="' + FloatToStr(x1) + '" y1="' + FloatToStr(y1) + '" x2="' + FloatToStr(x2) + '" y2="' + FloatToStr(y2) + '" >'
      + ' <stop style="stop-color:' + Color1 + ';stop-opacity:1" offset="0" id="' + id + 'stop0" />'
      + ' <stop style="stop-color:' + Color2 + ';stop-opacity:1" offset="1" id="' + id + 'stop1" />'
      + ' </linearGradient>';
    end;

    procedure CreateKnobSideGradient( aNode : TDomNode; r : single);
    var id : string;
        S : TStringStream;
    begin
      id := 'knobSideGradient';
      S := TStringStream.Create( Gradient( id, r/2, r/2, -r/2, -r/2, '#333333', '#ffffff'));
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    procedure CreateKnobBig( aNode : TDomNode);
    var S : TStringStream;
        id : string;
        w, h, c_x, c_y, r_side, r_face : single;
    begin
      // url(#linearGradient26483)

      w := 22;
      h := 26;
      r_side := 11;
      r_face := 8;
      c_x := 11;
      c_y := 11;

      id := idKnobBig;
      S := TStringStream.Create(
         '<g id="' + id + '">'
       + '  <desc>Knob big for G2 editor</desc>'
       + '  <rect id="' + id + '_sel" x="0" y="0" width="' + FLoatToStr(w) + '" height="' + FloatToStr(h) + '" fill="none" stroke="blue" stroke-width="0.2"/>'
       + '  <g transform="translate(' + FloatToStr(c_x) + ',' + FloatToStr(c_y) + ')">'
       + '    <g id="g' + id + '_face">'
       + '      <circle id="' + id + '_side" cx="0" cy="0" r="' + FloatToStr(r_side) + '" fill="url(#knobSideGradient)" stroke="' + cKnobBorder + '" stroke-width="1"  />'
       + '      <circle id="' + id + '_face" cx="0" cy="0" r="' + FloatToStr(r_face) + '" fill="' + cKnobFace + '" stroke="none"/>'
       + '    </g>'
       + '    <g id="g' + id + '_needle">'
       + '      <rect id="' + id + '_needle" fill="' + cKnobBorder + '" stroke="' + cKnobBorder + '" x="-0.1" y="' + FloatToStr(-(r_face-1)) + '" width="0.2" height="' + FloatToStr(r_face-1) + '" />'
       + '    </g>'
       + '    <g id="g' + id + '_morph">'
       + '      <path id="' + id + '_morph" d="M0,0 v' + FLoatToStr(-r_side) + ' a' + FloatToStr(r_side) + ',' + FLoatToStr(r_side) + ' 0 0,0 ' + FloatToStr(-r_side) + ',' + FloatToStr(r_side) + ' z" fill="red" stroke="none" opacity="0.5" />'
       + '    </g>'
       + '  </g>'
       + '</g>');
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    procedure CreateKnobMedium( aNode : TDomNode);
    var S : TStringStream;
        id : string;
    begin
      id := idKnobMedium;
      S := TStringStream.Create(
         '<g id="' + id + '">'
       + '  <desc>Knob medium for G2 editor</desc>'
       + '  <rect id="' + id + '_sel" x="0" y="0" width="20" height="24" fill="none" stroke="blue" stroke-width="0.2"/>'
       + '  <g transform="translate(10,10)">'
       + '    <g id="g' + id + '_face">'
       + '      <circle id="' + id + '_face" cx="0" cy="0" r="10" fill="' + cKnobFace + '" stroke="' + cKnobBorder + '" stroke-width="1" />'
       + '    </g>'
       + '    <g id="g' + id + '_needle">'
       + '      <rect id="' + id + '_needle" fill="' + cKnobBorder +'" stroke="' + cKnobBorder + '" x="-0.1" y="-9" width="0.2" height="9" />'
       + '    </g>'
       + '    <g id="g' + id + '_morph">'
       + '      <path id="' + id + '_morph" d="M0,0 v-10 a10,10 0 0,0 -10,10 z" fill="red" stroke="none" opacity="0.5" />'
       + '    </g>'
       + '  </g>'
       + '</g>');
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    procedure CreateKnobResetMedium( aNode : TDomNode);
    var S : TStringStream;
        id : string;
    begin
      id := idKnobResetMedium;
      S := TStringStream.Create(
         '<g id="' + id + '">'
       + '  <desc>Knob reset medium for G2 editor</desc>'
       + '  <rect id="' + id + '_sel" x="0" y="0" width="20" height="30" fill="none" stroke="blue" stroke-width="0.2"/>'
       + '  <g>'
       + '    <path id="' + id + '_cntrBtn" d="M5,0 h10 l -5,4 z" fill="lime" stroke="black" opacity="1" />'
       + '  </g>'
       + '  <g transform="translate(10,16)">'
       + '    <g id="g' + id + '_face">'
       + '      <circle id="' + id + '_face" cx="0" cy="0" r="10" fill="' + cKnobFace + '" stroke="' + cKnobBorder + '" stroke-width="1"  />'
       + '    </g>'
       + '    <g id="g' + id + '_morph">'
       + '      <rect id="' + id + '_needle" fill="' + cKnobBorder + '" stroke="' + cKnobBorder + '" x="-0.1" y="-9" width="0.2" height="9" />'
       + '    </g>'
       + '    <g id="g' + id + '_morph">'
       + '      <path id="' + id + '_morph" d="M0,0 v-10 a10,10 0 0,0 -10,10 z" fill="red" stroke="none" opacity="0.5" />'
       + '    </g>'
       + '  </g>'
       + '</g>');
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    procedure CreateKnobReset( aNode : TDomNode);
    var S : TStringStream;
        id : string;
    begin
      id := idKnobReset;
      S := TStringStream.Create(
         '<g id="' + id + '">'
       + '  <desc>Knob reset for G2 editor</desc>'
       + '  <rect id="' + id + '_sel" x="0" y="0" width="18" height="26" fill="none" stroke="blue" stroke-width="0.2"/>'
       + '  <g>'
       + '    <path id="' + id + '_btn" d="M4,0 h10 l -5,4 z" fill="lime" stroke="black" opacity="1" />'
       + '  </g>'
       + '  <g transform="translate(9,14)">'
       + '    <g id="g' + id + '_face">'
       + '      <circle id="' + id + '_face" cx="0" cy="0" r="9" fill="' + cKnobFace + '" stroke="' + cKnobBorder + '" stroke-width="1"  />'
       + '    </g>'
       + '    <g id="g' + id + '_needle">'
       + '      <rect id="' + id + '_needle" fill="' + cKnobBorder + '" stroke="' + cKnobBorder + '" x="-0.1" y="-8" width="0.2" height="8" />'
       + '    </g>'
       + '    <g id="g' + id + '_morph">'
       + '      <path id="' + id + '_morph" d="M0,0 v-9 a9,9 0 0,0 -9,9 z" fill="red" stroke="none" opacity="0.5" />'
       + '    </g>'
       + '  </g>'
       + '</g>');
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    procedure CreateKnobSmall( aNode : TDomNode);
    var S : TStringStream;
        id : string;
    begin
      id := idKnobSmall;
      S := TStringStream.Create(
         '<g id="' + id + '">'
       + '  <desc>Knob small for G2 editor</desc>'
       + '  <rect id="' + id + '_sel" x="0" y="0" width="18" height="22" fill="none" stroke="blue" stroke-width="0.2"/>'
       + '  <g transform="translate(9,9)">'
       + '    <g id="g' + id +'_face">'
       + '      <circle id="' + id + '_face" cx="0" cy="0" r="9" fill="' + cKnobFace + '" stroke="' + cKnobBorder + '" stroke-width="1"  />'
       + '    </g>'
       + '    <g id="g' + id + '_needle">'
       + '      <rect id="' + id + '_needle" fill="' + cKnobBorder + '" stroke="' + cKnobBorder + '" x="-0.1" y="-8" width="0.2" height="8" />'
       + '    </g>'
       + '    <g id="g' + id + '_morph">'
       + '      <path id="' + id + '_morph" d="M0,0 v-9 a9,9 0 0,0 -9,9 z" fill="red" stroke="none" opacity="0.5" />'
       + '    </g>'
       + '  </g>'
       + '</g>');
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    procedure CreateKnobSliderKnob( aNode : TDomNode; aWidth, aHeight, aBevelWidth, aBevelHeight : single);
    var S : TStringStream;
        bw : integer;
    begin
      bw := 1;
      S := TStringStream.Create( NiceButton( idSlider + '_knob', aWidth, aHeight, aBevelWidth, aBevelHeight,
                  cSldrKnobSideLight, cSldrKnobSideDark, cSldrKnobSideMedium, cSldrKnobFace));
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    procedure CreateKnobSlider( aNode : TDomNode);
    var S : TStringStream;
        bw : integer;
    begin
      bw := 1;
      S := TStringStream.Create(
         '<g id="' + idSlider + '">'
       + '  <desc>Knob slider for G2 editor</desc>'
       + '  <g id="' + idSlider + '_parts">'
       + '     <rect id="' + idSlider + '_bevel" fill="gray" stroke="none" x="0" y="0" width="11" height="45" />'
       + '     <rect id="' + idSlider + '_face" fill="lightgray" stroke="none" x="' + IntToStr(bw) + '" y="' + IntToStr(bw) + '" width="' + IntToStr(11 - bw*2) + '" height="' + IntToStr(45 - bw*2) + '" />'
       //+ '     <rect id="' + idSlider + '_btn" fill="gray" stroke="none" x="0" y="40" width="11" height="5" />'
       + Use( idSlider + '_knob', GetIDBtnText( idSlider + '_knob', 11, 6, 0.5), 0, 0)
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
        id : string;
    begin
      id := idConnector;
      S := TStringStream.Create(
         '<g id="' + id + '">'
       + '  <desc>Connector for G2 editor</desc>'
       + '  <rect id="' + id + '_sel" x="0" y="0" width="10" height="10" fill="none" stroke="blue" stroke-width="0.2"/>'
       + '   <g transform="translate(5,5)">'
       + '     <g id="' + id + '_parts">'
       + '       <circle id="' + id + '_border" cx="0" cy="0" r="5" fill="red" stroke="none" opacity="1"  />'
       + '       <circle id="' + id + '_hole" cx="0" cy="0" r="3" fill="black" stroke="none" opacity="1"  />'
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
    var GModuleNode, GCtrlNode, DescNode, PanelNode, UseNode : TDOMNode;
        l, m, x, y, w, h, dx, dy, symbol_id : integer;
        module_id, control_id, ref_id : string;
    begin
      module_id := idModule + '_' + IntToStr(FModule.TypeID);

      GModuleNode := Doc.CreateElement('g');
      aNode.AppendChild(GModuleNode);
      TDOMElement(GModuleNode).SetAttribute('id', module_id);

      DescNode := Doc.CreateElement('desc');
      GModuleNode.AppendChild(DescNode);
      DescNode.AppendChild(Doc.CreateTextNode('Module ' + IntToStr(FModule.TypeID) + ', ' + FModule.ModuleName));

      PanelNode := Doc.CreateElement('rect');
      GModuleNode.AppendChild(PanelNode);
      TDOMElement(PanelNode).SetAttribute('id', module_id + '_panel_' + IntToStr(FModule.TypeID));
      TDOMElement(PanelNode).SetAttribute('fill', 'white');
      TDOMElement(PanelNode).SetAttribute('stroke', 'black');
      TDOMElement(PanelNode).SetAttribute('x', '0');
      TDOMElement(PanelNode).SetAttribute('y', '0');
      TDOMElement(PanelNode).SetAttribute('width', IntToStr(FModule.Panel.Width));
      TDOMElement(PanelNode).SetAttribute('height', IntToStr(FModule.Panel.Height));

      for l := 0 to FModule.Panel.ChildControlsCount - 1 do begin

        control_id := module_id + '_ctrl_' + IntToStr(l);

        Control :=  FModule.Panel.GraphChildControls[l];

        if Control is TG2GraphLabel then begin
          CreateLabel( GModuleNode, Control.Left, Control.Top, (Control as TG2GraphLabel).Caption, (Control as TG2GraphLabel).Font.Size);
        end;

        if Control is TG2GraphDisplay then begin
          FindOrAddTextField( Control.Width, Control.Height);

          CreateUse( GModuleNode, control_id, GetIDTxtField(Control.Width, Control.Height), Control.Left, Control.Top);
        end;

        if Control is TG2GraphButtonText then begin

          FindOrAddButtonText( Control.Width, Control.Height, 2);

          GCtrlNode := Doc.CreateElement('g');
          GModuleNode.AppendChild(GCtrlNode);
          TDOMElement(GCtrlNode).SetAttribute('id', 'g2_module_' + IntToStr(FModule.TypeID) + '_gctrl_' + IntToStr(l));

          CreateUse( GCtrlNode, control_id, GetIDBtnText( idBtnText, Control.Width, Control.Height, 2), Control.Left, Control.Top);

          for m := 0 to (Control as TG2GraphButton).ImageList.Count - 1 do begin
            symbol_id := AddSymbolFromBitmap( (Control as TG2GraphButton).ImageList.Items[m]);
            w := (BitmapList[symbol_id] as TBitmap).Width;
            h := (BitmapList[symbol_id] as TBitmap).Height;

            CreateUse( GCtrlNode, control_id + '_symbol_' + IntToStr(m), idSymbol + '_' + IntToStr(symbol_id), Control.Left + Control.Width div 2 - w div 2, Control.Top + Control.Height div 2 - h div 2);
          end;
        end;

        if Control is TG2GraphButtonIncDec then begin

          if (Control as TG2GraphButtonIncDec).Orientation = otHorizontal then begin

            CreateUse( GModuleNode, control_id, idBtnIncDecHorz, Control.Left, Control.Top);
          end else begin
            CreateUse( GModuleNode, control_id, idBtnIncDecVert, Control.Left, Control.Top);
          end;
        end;

        if Control is TG2GraphButtonFlat then begin

          FindOrAddButtonFlat( Control.Width, Control.Height);

          for m := 0 to (Control as TG2GraphButton).ImageList.Count - 1 do begin
            AddSymbolFromBitmap( (Control as TG2GraphButton).ImageList.Items[m]);
          end;

          CreateUse( GModuleNode, control_id, GetIDBtnFlat( Control.Width, Control.Height), Control.Left, Control.Top);
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

            FindOrAddButtonFlat( w, h);

            x := RadioButton.Left;
            y := RadioButton.Top;
            for m := 0 to RadioButton.ButtonCount - 1 do begin
              CreateUse( GModuleNode, control_id + '_el_'+ IntToStr(m), GetIDBtnFlat( w, h), x, y);
              x := x + dx;
              y := y + dy;
            end;
          end;
        end;

        if Control is TG2GraphKnob then begin

          if (Control as TG2GraphKnob).KnobType = ktSlider then begin

            CreateUse( GModuleNode, control_id + '_el_1', idSlider, Control.Left, Control.Top);
            CreateUse( GModuleNode, control_id + '_el_2', idBtnIncDecVert, Control.Left, Control.Top + 46);
          end else begin
            case (Control as TG2GraphKnob).KnobType of
              ktBig : ref_id := idKnobBig;
              ktMedium : ref_id := idKnobMedium;
              ktResetMedium : ref_id := idKnobResetMedium;
              ktReset : ref_id := idKnobReset;
              ktSmall : ref_id := idKnobSmall;
            end;
            CreateUse( GModuleNode, control_id, ref_id, Control.Left, Control.Top);
          end;
        end;

        if Control is TG2GraphPartSelector then begin

          FindOrAddPartSel( Control.Width, Control.Height);

          CreateUse( GModuleNode, control_id, GetIDPartSel( Control.Width, Control.Height), Control.Left, Control.Top);
        end;

        if Control is TG2GraphLedGreen then begin
          case (Control as TG2GraphLedGreen).FType of
            ltGreen : ref_id := idLedGreen;
            ltSequencer : ref_id := idLedSequencer;
          end;
          CreateUse( GModuleNode, control_id, ref_id, Control.Left, Control.Top);
        end;


        if Control is TG2GraphConnector then begin
          CreateUse( GModuleNode, control_id, idConnector, Control.Left, Control.Top);
        end;
      end;
    end;

begin
  Doc := TXMLDocument.Create;
  BitMapList := TObjectList.Create(True);
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
    led_section := 50;
    buttontext_section := 100;
    buttonflat_section := 150;
    textfield_section := 200;
    symbol_section := 250;
    btnIncDec_section := 300;
    partsel_section := 350;
    module_section := 400;

    mr := 0;
    mc := 0;
    btc := 0;
    bfc := 0;
    kc := 0;
    tfc := 0;
    sc := 0;
    lc := 0;
    bidc := 0;
    psc := 0;

    CreateConnector( DefsNode);
    CreateKnobSideGradient( DefsNode, 11);

    GMainNode := Doc.CreateElement('g');
    RootNode.AppendChild(GMainNode);
    TDOMElement(GMainNode).SetAttribute('id', 'g2_main');

    GKnobSectionNode := Doc.CreateElement('g');
    GMainNode.AppendChild(GKnobSectionNode);
    TDOMElement(GKnobSectionNode).SetAttribute('id', 'g2_knob_section');

    GKnobDefNode := Doc.CreateElement('g');
    GKnobSectionNode.AppendChild(GKnobDefNode);
    TDOMElement(GKnobDefNode).SetAttribute('id', 'g2_knob_def_big');
    TDOMElement(GKnobDefNode).SetAttribute('transform', 'translate(' + IntToStr(kc) + ',' + IntToStr(knob_section) + ')');
    CreateKnobBig( GKnobDefNode);
    kc := kc + 40;

    GKnobDefNode := Doc.CreateElement('g');
    GKnobSectionNode.AppendChild(GKnobDefNode);
    TDOMElement(GKnobDefNode).SetAttribute('id', 'g2_knob_def_medium');
    TDOMElement(GKnobDefNode).SetAttribute('transform', 'translate(' + IntToStr(kc) + ',' + IntToStr(knob_section) + ')');
    CreateKnobMedium( GKnobDefNode);
    kc := kc + 40;

    GKnobDefNode := Doc.CreateElement('g');
    GKnobSectionNode.AppendChild(GKnobDefNode);
    TDOMElement(GKnobDefNode).SetAttribute('id', 'g2_knob_def_resetmedium');
    TDOMElement(GKnobDefNode).SetAttribute('transform', 'translate(' + IntToStr(kc) + ',' + IntToStr(knob_section) + ')');
    CreateKnobResetMedium( GKnobDefNode);
    kc := kc + 40;

    GKnobDefNode := Doc.CreateElement('g');
    GKnobSectionNode.AppendChild(GKnobDefNode);
    TDOMElement(GKnobDefNode).SetAttribute('id', 'g2_knob_def_reset');
    TDOMElement(GKnobDefNode).SetAttribute('transform', 'translate(' + IntToStr(kc) + ',' + IntToStr(knob_section) + ')');
    CreateKnobReset( GKnobDefNode);
    kc := kc + 40;

    GKnobDefNode := Doc.CreateElement('g');
    GKnobSectionNode.AppendChild(GKnobDefNode);
    TDOMElement(GKnobDefNode).SetAttribute('id', 'g2_knob_def_small');
    TDOMElement(GKnobDefNode).SetAttribute('transform', 'translate(' + IntToStr(kc) + ',' + IntToStr(knob_section) + ')');
    CreateKnobSmall( GKnobDefNode);
    kc := kc + 40;

    GKnobDefNode := Doc.CreateElement('g');
    GKnobSectionNode.AppendChild(GKnobDefNode);
    TDOMElement(GKnobDefNode).SetAttribute('id', 'g2_knob_def_slider_knob');
    TDOMElement(GKnobDefNode).SetAttribute('transform', 'translate(' + IntToStr(kc) + ',' + IntToStr(knob_section) + ')');
    CreateKnobSliderKnob( GKnobDefNode, 11, 6, 0.5, 1.5);
    kc := kc + 40;

    GKnobDefNode := Doc.CreateElement('g');
    GKnobSectionNode.AppendChild(GKnobDefNode);
    TDOMElement(GKnobDefNode).SetAttribute('id', 'g2_knob_def_slider');
    TDOMElement(GKnobDefNode).SetAttribute('transform', 'translate(' + IntToStr(kc) + ',' + IntToStr(knob_section) + ')');
    CreateKnobSlider( GKnobDefNode);
    kc := kc + 40;

    GLedSectionNode := Doc.CreateElement('g');
    GMainNode.AppendChild(GLedSectionNode);
    TDOMElement(GLedSectionNode).SetAttribute('id', 'g2_led_section');

    GLedDefNode := Doc.CreateElement('g');
    GLedSectionNode.AppendChild(GLedDefNode);
    TDOMElement(GLedDefNode).SetAttribute('id', 'g2_led_def_seq');
    TDOMElement(GLedDefNode).SetAttribute('transform', 'translate(' + IntToStr(lc) + ',' + IntToStr(led_section) + ')');
    CreateLedSeq( GLedDefNode);
    lc := lc + 40;

    GLedDefNode := Doc.CreateElement('g');
    GLedSectionNode.AppendChild(GLedDefNode);
    TDOMElement(GLedDefNode).SetAttribute('id', 'g2_led_def_green');
    TDOMElement(GLedDefNode).SetAttribute('transform', 'translate(' + IntToStr(lc) + ',' + IntToStr(led_section) + ')');
    CreateLedGreen( GLedDefNode);
    lc := lc + 40;

    GTextFieldSectionNode := Doc.CreateElement('g');
    GMainNode.AppendChild(GTextFieldSectionNode);
    TDOMElement(GTextFieldSectionNode).SetAttribute('id', 'g2_textfield_section');

    GButtonTextSectionNode := Doc.CreateElement('g');
    GMainNode.AppendChild(GButtonTextSectionNode);
    TDOMElement(GButtonTextSectionNode).SetAttribute('id', 'g2_buttontext_section');

    GButtonFlatSectionNode := Doc.CreateElement('g');
    GMainNode.AppendChild(GButtonFlatSectionNode);
    TDOMElement(GButtonFlatSectionNode).SetAttribute('id', 'g2_buttonflat_section');

    GBtnIncDecSectionNode := Doc.CreateElement('g');
    GMainNode.AppendChild(GBtnIncDecSectionNode);
    TDOMElement(GBtnIncDecSectionNode).SetAttribute('id', 'g2_btnIncdec_section');

    GBtnIncDecDefNode := Doc.CreateElement('g');
    GBtnIncDecSectionNode.AppendChild(GBtnIncDecDefNode);
    TDOMElement(GBtnIncDecDefNode).SetAttribute('id', 'g2_btnIncdec_def_horz');
    TDOMElement(GBtnIncDecDefNode).SetAttribute('transform', 'translate(' + IntToStr(bidc) + ',' + IntToStr(btnIncDec_section) + ')');
    CreateBtnIncDecHorz( GBtnIncDecDefNode);
    bidc := bidc + 40;

    GBtnIncDecDefNode := Doc.CreateElement('g');
    GBtnIncDecSectionNode.AppendChild(GBtnIncDecDefNode);
    TDOMElement(GBtnIncDecDefNode).SetAttribute('id', 'g2_btnIncdec_def_vert');
    TDOMElement(GBtnIncDecDefNode).SetAttribute('transform', 'translate(' + IntToStr(bidc) + ',' + IntToStr(btnIncDec_section) + ')');
    CreateBtnIncDecVert( GBtnIncDecDefNode);
    bidc := bidc + 40;

    GPartSelSectionNode := Doc.CreateElement('g');
    GMainNode.AppendChild(GPartSelSectionNode);
    TDOMElement(GPartSelSectionNode).SetAttribute('id', 'g2_partsel_section');

    GSymbolSectionNode := Doc.CreateElement('g');
    GMainNode.AppendChild(GSymbolSectionNode);
    TDOMElement(GSymbolSectionNode).SetAttribute('id', 'g2_symbol_section');

    GModuleSectionNode := Doc.CreateElement('g');
    GMainNode.AppendChild(GModuleSectionNode);
    TDOMElement(GModuleSectionNode).SetAttribute('id', 'g2_module_section');

    for i := 0 to G2_module_def.FModuleDefList.Count - 1 do begin

      GModuleDefNode := Doc.CreateElement('g');
      GModuleSectionNode.AppendChild(GModuleDefNode);
      TDOMElement(GModuleDefNode).SetAttribute('id', idModule + '_' + IntToStr(i));
      TDOMElement(GModuleDefNode).SetAttribute('transform', 'translate(' + IntToStr(mc * 300) + ',' + IntToStr(module_section + mr*160) + ')');

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
              if FindBitmap((Control as TG2GraphBitmap).ImageList.Items[k]) = -1 then begin
                Bitmap := TBitmap.Create;
                Bitmap.Assign((Control as TG2GraphBitmap).ImageList.Items[k]);
                BitMapList.Add(Bitmap);
              end;
              //(Control as TG2GraphBitmap).ImageList.Items[k].SaveToFile('C:\Users\Bruno\Delphi\nmg2editor\v0.25\ExtrImg\Module_' + IntToStr(FModule.TypeID) + '_ControlID_' + IntToStr(Control.ID) + '_No_' +  IntToStr(k) + '.bmp');
            end;
          end;

          if Control is TG2GraphButton then begin
            for k := 0 to (Control as TG2GraphButton).ImageList.Count - 1 do begin
              if FindBitmap((Control as TG2GraphButton).ImageList.Items[k]) = -1 then begin
                Bitmap := TBitmap.Create;
                Bitmap.Assign((Control as TG2GraphButton).ImageList.Items[k]);
                BitMapList.Add(Bitmap);
              end;
              //(Control as TG2GraphButton).ImageList.Items[k].SaveToFile('C:\Users\Bruno\Delphi\nmg2editor\v0.25\ExtrImg\Module_' + IntToStr(FModule.TypeID) + '_ControlID_' + IntToStr(Control.ID) + '_No_' +  IntToStr(k) + '.bmp');
            end;
          end;

          if Control is TG2GraphPartSelector then begin
            for k := 0 to (Control as TG2GraphPartSelector).ImageList.Count - 1 do begin
              if FindBitmap((Control as TG2GraphPartSelector).ImageList.Items[k]) = -1 then begin
                Bitmap := TBitmap.Create;
                Bitmap.Assign((Control as TG2GraphPartSelector).ImageList.Items[k]);
                BitMapList.Add(Bitmap);
              end;
              //(Control as TG2GraphPartSelector).ImageList.Items[k].SaveToFile('C:\Users\Bruno\Delphi\nmg2editor\v0.25\ExtrImg\Module_' + IntToStr(FModule.TypeID) + '_ControlID_' + IntToStr(Control.ID) + '_No_' +  IntToStr(k) + '.bmp');
            end;
          end;
        end;

      finally
        UnloadModule
      end;
    end;
    for i := 0 to BitmapList.Count - 1 do
      (BitmapList.Items[i] as TBitmap).SaveToFile('Skin\Symbol_' + IntToStr(i) + '.bmp');

    WriteXMLFile( Doc, 'Skin\g2_graphics.svg');
  finally
    BitMapList.Free;
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
