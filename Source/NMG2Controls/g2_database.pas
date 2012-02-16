unit g2_database;

interface
uses
  SysUtils, DOM, XMLRead, XMLWrite;

type
  TXMLModuleDefListType = class;
  TXMLModuleDefType = class;
  TXMLConnectorListType = class;
  TXMLConnectorType = class;
  TXMLParamListType = class;
  TXMLParamType = class;
  TXMLParamDefListType = class;
  TXMLParamDefType = class;

  TXMLTCPSettingsType = class;

  TXMLModuleDefListType = class(TDOMElementList)
  protected
    function Get_ModuleDef(Index: Integer): TXMLModuleDefType;
   public
     constructor Create(ANode: TDOMNode); overload;
     property ModuleDef[Index: Integer]: TXMLModuleDefType read Get_ModuleDef;
  end;

  TXMLModuleDefType = class(TDOMElement)
  protected
    function Get_ModuleType: Integer;
    function Get_IsLed: Integer;
    function Get_Uprate: Integer;
    function Get_Page: AnsiString;
    function Get_PageIndex: Integer;
    function Get_ShortName: AnsiString;
    function Get_LongName: AnsiString;
    function Get_Height: Integer;
    function Get_Inputs: TXMLConnectorListType;
    function Get_Outputs: TXMLConnectorListType;
    function Get_Params: TXMLParamListType;
    function Get_Modes: TXMLParamListType;
  public
    property ModuleType: Integer read Get_ModuleType;
    property IsLed: Integer read Get_IsLed;
    property Uprate: Integer read Get_Uprate;
    property Page: AnsiString read Get_Page;
    property PageIndex: Integer read Get_PageIndex;
    property ShortName: AnsiString read Get_ShortName;
    property LongName: AnsiString read Get_LongName;
    property Height: Integer read Get_Height;
    property Inputs: TXMLConnectorListType read Get_Inputs;
    property Outputs: TXMLConnectorListType read Get_Outputs;
    property Params: TXMLParamListType read Get_Params;
    property Modes: TXMLParamListType read Get_Modes;
  end;

  TXMLConnectorListType = class(TDOMElementList)
  protected
    function Get_Connector(Index: Integer): TXMLConnectorType;
  public
    constructor Create(ANode: TDOMNode); overload;
    property Connector[Index: Integer]: TXMLConnectorType read Get_Connector; default;
  end;

  TXMLConnectorType = class(TDOMElement)
    function Get_Name: AnsiString;
    function Get_Type_: AnsiString;
    property Name: AnsiString read Get_Name;
    property Type_: AnsiString read Get_Type_;
  end;

  TXMLParamListType = class(TDOMElementList)
  protected
    function Get_Param(Index: Integer): TXMLParamType;
  public
    constructor Create(ANode: TDOMNode); overload;
    property Param[Index: Integer]: TXMLParamType read Get_Param; default;
  end;

  TXMLParamType = class(TDOMElement)
    function Get_Id: Integer;
    function Get_Name: AnsiString;
    function Get_DefaultValue: Integer;
    function Get_ParamLabel: AnsiString;
    function Get_DefaultKnob : Integer;
    function Get_ButtonParamIndex : Integer;
    property Id: Integer read Get_Id;
    property Name: AnsiString read Get_Name;
    property DefaultValue: Integer read Get_DefaultValue;
    property ParamLabel: AnsiString read Get_ParamLabel;
    property DefaultKnob : Integer read Get_DefaultKnob;
    property ButtonParamIndex : Integer read Get_ButtonParamIndex;
  end;

  TXMLParamDefListType = class(TDOMElementList)
  protected
    function Get_ParamDef(Index: Integer): TXMLParamDefType;
  public
    constructor Create(ANode: TDOMNode); overload;
    property ParamDef[Index: Integer]: TXMLParamDefType read Get_ParamDef; default;
  end;

  TXMLParamDefType = class(TDOMElement)
  protected
    function Get_Id: Integer;
    function Get_ParamType: Integer;
    function Get_RangeType: AnsiString;
    function Get_LowValue: Integer;
    function Get_HighValue: Integer;
    function Get_DefaultValue: Integer;
    function Get_Definitions: AnsiString;
    function Get_Comments: AnsiString;
  public
    property Id: Integer read Get_Id;
    property ParamType: Integer read Get_ParamType;
    property RangeType: AnsiString read Get_RangeType;
    property LowValue: Integer read Get_LowValue;
    property HighValue: Integer read Get_HighValue;
    property DefaultValue: Integer read Get_DefaultValue;
    property Definitions: AnsiString read Get_Definitions;
    property Comments: AnsiString read Get_Comments;
  end;

  TXMLTCPSettingsListType = class(TDOMElementList)
  protected
    function Get_TCPSettings(Index: Integer): TXMLTCPSettingsType;
   public
     constructor Create(ANode: TDOMNode); overload;
     property TCPSettings[Index: Integer]: TXMLTCPSettingsType read Get_TCPSettings;
  end;

  TXMLTCPSettingsType = class(TDOMElement)
  protected
    function Get_IP: string;
    function Get_Port: integer;
    procedure Set_IP( aValue : string);
    procedure Set_Port( aValue : integer);
  public
    property IP: string read Get_IP write Set_IP;
    property Port: integer read Get_Port write Set_Port;
  end;

  TXMLFormSettingsType = class(TDOMElement)
  protected
    function Get_PosX: integer;
    function Get_PosY: integer;
    function Get_SizeX: integer;
    function Get_SizeY: integer;
    function Get_Visible : boolean;
    procedure Set_PosX( aValue : integer);
    procedure Set_PosY( aValue : integer);
    procedure Set_SizeX( aValue : integer);
    procedure Set_SizeY( aValue : integer);
    procedure Set_Visible( aValue : boolean);
  public
    property PosX: integer read Get_PosX write Set_PosX;
    property PosY: integer read Get_PosY write Set_PosY;
    property SizeX: integer read Get_SizeX write Set_SizeX;
    property SizeY: integer read Get_SizeY write Set_SizeY;
    property Visible: boolean read Get_Visible write Set_Visible;
  end;

  TXMLPatchManagerSettingsType = class(TDOMElement)
  protected
    function Get_BaseFolder: string;
    procedure Set_BaseFolder( aValue : string);
    function Get_ExternalSortCol: integer;
    procedure Set_ExternalSortCol( aValue : integer);
    function Get_InternalSortCol: integer;
    procedure Set_InternalSortCol( aValue : integer);
    function Get_SelectedTab: integer;
    procedure Set_SelectedTab( aValue : integer);
  public
    property BaseFolder: string read Get_BaseFolder write Set_BaseFolder;
    property ExternalSortCol : integer read Get_ExternalSortCol write Set_ExternalSortCol;
    property InternalSortCol : integer read Get_InternalSortCol write Set_InternalSortCol;
    property SelectedTab : integer read Get_SelectedTab write Set_SelectedTab;
  end;

implementation

function GetInt( aValue : string): integer;
var Code : integer;
begin
  val( aValue, Result, Code);
  if Code <> 0 then
    Result := 0;
end;

{ TXMLModuleDefListType }

constructor TXMLModuleDefListType.Create(ANode: TDOMNode);
begin
  inherited Create(aNode,'ModuleDef');
end;

function TXMLModuleDefListType.Get_ModuleDef(Index: Integer): TXMLModuleDefType;
begin
  Result := TXMLModuleDefType(Item[Index]);
end;

{ TXMLModuleDefType }

function TXMLModuleDefType.Get_ModuleType: Integer;
begin
  Result := GetInt(FindNode('ModuleType').FirstChild.NodeValue);
end;

function TXMLModuleDefType.Get_IsLed: Integer;
begin
  Result := GetInt(FindNode('IsLed').FirstChild.NodeValue);
end;

function TXMLModuleDefType.Get_Uprate: Integer;
begin
  Result := GetInt(FindNode('Uprate').FirstChild.NodeValue);
end;

function TXMLModuleDefType.Get_Page: AnsiString;
begin
  Result := AnsiString(FindNode('Page').FirstChild.NodeValue);
end;

function TXMLModuleDefType.Get_PageIndex: Integer;
begin
  Result := GetInt(FindNode('PageIndex').FirstChild.NodeValue);
end;

function TXMLModuleDefType.Get_ShortName: AnsiString;
begin
  Result := AnsiString(FindNode('ShortName').FirstChild.NodeValue);
end;

function TXMLModuleDefType.Get_LongName: AnsiString;
begin
  Result := AnsiString(FindNode('LongName').FirstChild.NodeValue);
end;

function TXMLModuleDefType.Get_Height: Integer;
begin
  Result := GetInt(FindNode('Height').FirstChild.NodeValue);
end;

function TXMLModuleDefType.Get_Inputs: TXMLConnectorListType;
var n : TDOMNode;
begin
  n := FindNode('Inputs');
  if assigned(n) then
    Result := TXMLConnectorListType.Create( n)
  else
    Result := nil;
end;

function TXMLModuleDefType.Get_Outputs: TXMLConnectorListType;
var n : TDOMNode;
begin
  n := FindNode('Outputs');
  if assigned(n) then
    Result := TXMLConnectorListType.Create( n)
  else
    Result := nil;
end;

function TXMLModuleDefType.Get_Params: TXMLParamListType;
var n : TDOMNode;
begin
  n := FindNode('Params');
  if assigned(n) then
    Result := TXMLParamListType.Create( n)
  else
    Result := nil;
end;

function TXMLModuleDefType.Get_Modes: TXMLParamListType;
var n : TDOMNode;
begin
  n := FindNode('Modes');
  if assigned(n) then
    Result := TXMLParamListType.Create( n)
  else
    Result := nil;
end;

{ TXMLConnectorListType }

constructor TXMLConnectorListType.Create(ANode: TDOMNode);
begin
  inherited Create(aNode,'Connector');
end;

function TXMLConnectorListType.Get_Connector(Index: Integer): TXMLConnectorType;
begin
  Result := TXMLConnectorType(Item[Index]);
end;

{ TXMLConnectorType }

function TXMLConnectorType.Get_Name: AnsiString;
begin
  Result := AnsiString(FindNode('Name').FirstChild.NodeValue);
end;

function TXMLConnectorType.Get_Type_: AnsiString;
begin
  Result := AnsiString(FindNode('Type').FirstChild.NodeValue);
end;

{ TXMLParamListType }

constructor TXMLParamListType.Create(ANode: TDOMNode);
begin
  inherited Create(aNode,'Param');
end;

function TXMLParamListType.Get_Param(Index: Integer): TXMLParamType;
begin
  Result := TXMLParamType(Item[Index]);
end;

{ TXMLParamType }

function TXMLParamType.Get_DefaultKnob: Integer;
var Node : TDOMNode;
begin
  Node := FindNode('DefaultKnob');
  if Node <> nil then
    Result := GetInt(Node.TextContent)
  else
    Result := -1;
end;

function TXMLParamType.Get_ButtonParamIndex: Integer;
var Node : TDOMNode;
begin
  Node := FindNode('ButtonParam');
  if Node <> nil then
    Result := GetInt(Node.TextContent)
  else
    Result := -1;
end;

function TXMLParamType.Get_DefaultValue: Integer;
begin
  Result := GetInt(FindNode('DefaultValue').FirstChild.NodeValue);
end;

function TXMLParamType.Get_Id: Integer;
begin
  Result := GetInt(FindNode('Id').FirstChild.NodeValue);
end;

function TXMLParamType.Get_Name: AnsiString;
begin
  Result := AnsiString(FindNode('Name').FirstChild.NodeValue);
end;

function TXMLParamType.Get_ParamLabel: AnsiString;
var Node : TDomNode;
begin
  Node := FindNode('ParamLabel');
  if assigned(Node) then
    Result := AnsiString(Node.FirstChild.NodeValue)
  else
    Result := '';
end;

{procedure TXMLParamType.Set_DefaultKnob(aValue: integer);
var Node : TDOMElement;
begin
  Node := TDOMElement(FindNode('DefaultKnob'));
  if assigned(Node) then begin
    Node.TextContent := IntToStr(aValue);
  end else begin
    Node := TDOMDocument(OwnerDocument).CreateElement('DefaultKnob');
    AppendChild( Node);
    Node.TextContent := IntToStr(aValue);
  end;
end;}

{ TXMLParamDefListType }

constructor TXMLParamDefListType.Create(ANode: TDOMNode);
begin
  inherited Create(aNode,'ParamDef');
end;

function TXMLParamDefListType.Get_ParamDef(Index: Integer): TXMLParamDefType;
begin
  Result := TXMLParamDefType(Item[Index]);
end;

{ TXMLParamDefType }

function TXMLParamDefType.Get_Comments: AnsiString;
begin
  Result := AnsiString(FindNode('Comments').FirstChild.NodeValue);
end;

function TXMLParamDefType.Get_DefaultValue: Integer;
begin
  Result := GetInt(FindNode('DefaultValue').FirstChild.NodeValue);
end;

function TXMLParamDefType.Get_Definitions: AnsiString;
begin
  Result := AnsiString(FindNode('Definitions').FirstChild.NodeValue);
end;

function TXMLParamDefType.Get_HighValue: Integer;
begin
  Result := GetInt(FindNode('HighValue').FirstChild.NodeValue);
end;

function TXMLParamDefType.Get_Id: Integer;
begin
  Result := GetInt(FindNode('Id').FirstChild.NodeValue);
end;

function TXMLParamDefType.Get_LowValue: Integer;
begin
  Result := GetInt(FindNode('LowValue').FirstChild.NodeValue);
end;

function TXMLParamDefType.Get_ParamType: Integer;
begin
  Result := GetInt(FindNode('ParamType').FirstChild.NodeValue);
end;

function TXMLParamDefType.Get_RangeType: AnsiString;
begin
  Result := AnsiString(FindNode('RangeType').FirstChild.NodeValue);
end;

{ TXMLTCPSettings }

function TXMLTCPSettingsType.Get_IP: string;
begin
  Result := GetAttribute('IP');
end;

function TXMLTCPSettingsType.Get_Port: integer;
begin
  Result := GetInt(GetAttribute('Port'));
end;

procedure TXMLTCPSettingsType.Set_IP(aValue: string);
begin
  SetAttribute('IP', aValue);
end;

procedure TXMLTCPSettingsType.Set_Port(aValue: integer);
begin
  SetAttribute('Port', IntToStr(aValue));
end;

{ TXMLTCPSettingsListType }

constructor TXMLTCPSettingsListType.Create(ANode: TDOMNode);
begin
  inherited Create( aNode,'TCP_Settings');
end;

function TXMLTCPSettingsListType.Get_TCPSettings( Index: Integer): TXMLTCPSettingsType;
begin
  Result := TXMLTCPSettingsType(Item[Index]);
end;

{ TXMLFormSettingsType }

function TXMLFormSettingsType.Get_PosX: integer;
begin
  Result := GetInt(GetAttribute('PosX'));
end;

function TXMLFormSettingsType.Get_PosY: integer;
begin
  Result := GetInt(GetAttribute('PosY'));
end;

function TXMLFormSettingsType.Get_SizeX: integer;
begin
  Result := GetInt(GetAttribute('SizeX'));
end;

function TXMLFormSettingsType.Get_SizeY: integer;
begin
  Result := GetInt(GetAttribute('SizeY'));
end;

function TXMLFormSettingsType.Get_Visible: boolean;
begin
  Result := StrToBool(GetAttribute('Visible'));
end;

procedure TXMLFormSettingsType.Set_PosX(aValue: integer);
begin
  SetAttribute('PosX', IntToStr(aValue));
end;

procedure TXMLFormSettingsType.Set_PosY(aValue: integer);
begin
  SetAttribute('PosY', IntToStr(aValue));
end;

procedure TXMLFormSettingsType.Set_SizeX(aValue: integer);
begin
  SetAttribute('SizeX', IntToStr(aValue));
end;

procedure TXMLFormSettingsType.Set_SizeY(aValue: integer);
begin
  SetAttribute('SizeY', IntToStr(aValue));
end;

procedure TXMLFormSettingsType.Set_Visible(aValue: boolean);
begin
  SetAttribute('Visible', BoolToStr(aValue));
end;

{ TXMLPatchManagerSettingsType }

function TXMLPatchManagerSettingsType.Get_BaseFolder: string;
begin
  Result := GetAttribute('BaseFolder');
end;

function TXMLPatchManagerSettingsType.Get_ExternalSortCol: integer;
begin
  Result := GetInt(GetAttribute('ExternalSortCol'));
end;

function TXMLPatchManagerSettingsType.Get_InternalSortCol: integer;
begin
  Result := GetInt(GetAttribute('InternalSortCol'));
end;

function TXMLPatchManagerSettingsType.Get_SelectedTab: integer;
begin
  Result := GetInt(GetAttribute('SelectedTab'));
end;

procedure TXMLPatchManagerSettingsType.Set_BaseFolder(aValue: string);
begin
  SetAttribute('BaseFolder', aValue);
end;

procedure TXMLPatchManagerSettingsType.Set_ExternalSortCol(aValue: integer);
begin
  SetAttribute('ExternalSortCol', IntToStr(aValue));
end;

procedure TXMLPatchManagerSettingsType.Set_InternalSortCol(aValue: integer);
begin
  SetAttribute('InternalSortCol', IntToStr(aValue));
end;

procedure TXMLPatchManagerSettingsType.Set_SelectedTab(aValue: integer);
begin
  SetAttribute('SelectedTab', IntToStr(aValue));
end;


end.
