unit g2_graph_FMX;

interface
uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.Layouts,
  System.Contnrs, SVGControl, DOM, XMLRead, g2_types, g2_file, g2_usb, math;

type
  TG2GraphModuleFMX = class;

  TCreateModuleFMXEvent = procedure(Sender : TObject; Module : TG2GraphModuleFMX) of Object;

  TG2GraphFMX = class( TG2USB)
  // Contains the control for the VA and FX patching
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    function    CreatePerformance : TG2FilePerformance; override;
  end;

  TG2GraphPerformanceFMX = class( TG2USBPerformance)
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    function    CreateSlot : TG2FileSlot; override;
  end;

  TG2GraphSlotFMX = class( TG2USBSlot)
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    function    CreatePatch : TG2FilePatch; override;
  end;

  TG2GraphPatchFMX = class( TG2USBPatch)
  // This represents the patch
  private
    // Lists for the leds
    FMiniVUList         : TList;
    FLedGroupList       : TList;
    FLed39List          : TList;
    FLed3AList          : TList;
    FVisible            : boolean;
    FSelectedControl    : TControl;
    FSelectedMorphIndex : integer;

    FSVGSkin            : TXMLDocument;
    FLayout             : TLayout;

    FOnCreateModuleFMX : TCreateModuleFMXEvent;

    procedure   SetVisible( aValue : boolean);
    procedure   SetSelectedControl( aValue : TControl);
    procedure   SetSelectedMorphIndex( aValue : integer);
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   Init; override;

    function    CreateModule( aLocation : TLocationType; aModuleIndex : byte; aModuleType : byte): TG2FileModule; override;
    function    CreateCable( aLocation : TLocationType; aColor : byte; aFromModule : byte; aFromConnector : byte; aLinkType : byte; aToModule : byte; aToConnector : byte): TG2FileCable; override;
    function    CreateParameter( aModuleIndex : byte): TG2FileParameter; override;

    procedure   SetMiniVULevel( Index : integer; aValue : byte);
    procedure   SetLedLevel( Index : integer; aValue : byte);
    function    GetMiniVUListCount : integer;
    function    GetLedListCount : integer;
    procedure   SortLeds; override;
    procedure   RemoveFromLedList( aLocation: TLocationType; aModuleIndex : integer);

    property    Visible : boolean read FVisible write SetVisible;
    property    SelectedControl : TControl read FSelectedControl write SetSelectedControl;
    property    SelectedMorphIndex : integer read FSelectedMorphIndex write SetSelectedMorphIndex;
    property    Layout : TLayout read FLayout write FLayout;
    property    SVGSkin : TXMLDocument read FSVGSkin write FSVGSkin;

    property    OnCreateModuleFMX : TCreateModuleFMXEvent read FOnCreateModuleFMX write FOnCreateModuleFMX;
  end;

  TSVGG2Control = class;
  TSVGG2Module = class;

  TG2GraphModuleFMX = class( TG2FileModule)
  private
    FSVGControl : TSVGG2Module;
  protected
    procedure   SetSelected( aValue: boolean); override;
    function    GetNewCol : TBits7; override;
    function    GetNewRow : TBits7; override;
  public
    constructor Create( aPatchPart : TG2FilePatchPart); override;
    constructor CopyCreate( aPatchPart : TG2FilePatchPart; aModule : TG2GraphModuleFMX);
    destructor  Destroy; override;
    function    CreateCopy( aPatchPart : TG2FilePatchPart) : TG2FileModule; override;
    function    CreateParameter: TG2FileParameter; override;
    procedure   ParsePanelData( aParent : TFMXObject; aSkin : TXMLDocument; aModuleType : byte);
    procedure   SetCol( aValue : TBits7); override;
    procedure   SetRow( aValue : TBits7); override;
    procedure   SetModuleColor( aValue : TBits8); override;
    procedure   SetModuleName( aValue : AnsiString); override;

    property SVGControl : TSVGG2Module read FSVGControl write FSVGControl;
  end;

  TG2GraphParameterFMX = class(TG2FileParameter)
  private
    FControlList  : array of TSVGG2Control; // array of controls the parameter is assigned to
  public
    constructor Create( aPatch : TG2FilePatch; aLocation : TLocationType; aModuleIndex : integer; aModule : TG2FileModule);
    destructor  Destroy; override;
    procedure   AssignControl( aControl : TSVGG2Control);
    procedure   DeassignControl( aControl : TSVGG2Control);
    procedure   InvalidateControl; override;
  end;

  TSVGG2Cable = class;

  TG2GraphCableFMX = class( TG2FileCable)
  protected
    FParent         : TFmxObject;
    FPatch          : TG2GraphPatchFMX;
    FModule         : TG2GraphModuleFMX;
    FSVGControl     : TSVGG2Cable;
    FSelected       : boolean;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   InitCable;
    procedure   ConnectorMoved; override;
    procedure   SetParent( aValue : TFmxObject);

    property    GraphControl : TSVGG2Cable read FSVGControl write FSVGControl;
    property    Parent : TFmxObject read FParent write SetParent;
  end;

  TSVGG2ParamLink = class(TSVGGroup)
  private
    FModule : TG2GraphModuleFMX;
    FCodeRef : integer;
    FMasterRef : integer;
    FInfoFunc : integer;
    FTextFunc : integer;
    FDependencies : string;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;

    function    CreateGroup( aNode : TDOMNode; aParent : TSVGGroup; var aSubTreeOwner : TSVGGroup) : TSVGGroup; override;
  end;

  TSVGG2ConnLink = class(TSVGGroup)
  private
    FModule : TG2GraphModuleFMX;
    FCodeRef : integer;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;

    function    CreateGroup( aNode : TDOMNode; aParent : TSVGGroup; var aSubTreeOwner : TSVGGroup) : TSVGGroup; override;
  end;

  TSVGG2Control = class(TSVGGroup)
  private
    FParameter : TG2GraphParameterFMX;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure MouseDown( P : TPointF); virtual;
    procedure MouseMove( P : TPointF; dx, dy : single); virtual;
    procedure MouseUp( P : TPointF); virtual;
  end;

  TSVGHitPath = class(TSVGPath)
  private
    FControl : TSVGG2Control;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;

    property    Control : TSVGG2Control read FControl write FControl;
  end;

  TSVGBtnState = class(TSVGGroup)
  private
    FControl : TSVGG2Control;
    FBtnBackground : TSVGHitPath;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;

    function    CreatePath( aNode : TDOMNode; aParent : TSVGGroup) : TSVGPath; override;
    property    Control : TSVGG2Control read FControl write FControl;
  end;

  TSVGBtnText = class(TSVGG2Control)
  private
    FBtnUp   : TSVGBtnState;
    FBtnDown : TSVGBtnState;

    FValue : single;
  protected
    procedure   SetValue( aValue : single);
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure   MouseDown( P : TPointF); override;

    property    Value : single read FValue write SetValue;
  end;

  TSVGKnob = class(TSVGG2Control)
  private
    FKnobNeedle : TSVGGroup;
    FKnobFace : TSVGHitPath;
    FKnobMorph : TSVGPath;

    FValue : single;

    procedure   SetLayoutAngle( aLayout : TSVGGroup; aAngle : single);
  protected
    procedure   SetValue( aValue : single);
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;

    function    CreatePath( aNode : TDOMNode; aParent : TSVGGroup) : TSVGPath; override;
    function    CreateGroup( aNode : TDOMNode; aParent : TSVGGroup; var aSubTreeOwner : TSVGGroup) : TSVGGroup; override;

    procedure   MouseMove( P : TPointF; dx, dy : single); override;

    property    Value : single read FValue write SetValue;
  end;

  TSVGG2Connector = class(TSVGG2Control)
  private
    FData   : TG2FileConnector;
    FBorder : TSVGHitPath;
  protected
    procedure   SetData( aValue: TG2FileConnector);
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;

    function    CreatePath( aNode : TDOMNode; aParent : TSVGGroup) : TSVGPath; override;

    property    Data : TG2FileConnector read FData write SetData;
  end;

  TNode = class
    x  : single;
    y  : single;
    vx : single;
    vy : single;
  end;

  TSVGG2Cable = class(TControl)
  private
    FP1, FP2 : TPointF;
    FNodeCount : integer;
    FNodes : array of TNode;
    FMargin : single;
    FTimer : TTimer;
    FTimerCount : integer;
    FData : TG2GraphCableFMX;
  protected
    procedure SetPoint1X( Value : single);
    procedure SetPoint1Y( Value : single);
    procedure SetPoint2X( Value : single);
    procedure SetPoint2Y( Value : single);
    procedure IterateCable;
    procedure OnTimer(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure StartTimer;
    procedure InitCable;
  published
    property Point1X : single read FP1.X write SetPoint1X;
    property Point1Y : single read FP1.Y write SetPoint1Y;
    property Point2X : single read FP2.X write SetPoint2X;
    property Point2Y : single read FP2.Y write SetPoint2Y;
  end;

  TSVGG2Module = class(TSVGGroup)
  private
    FSVGSkin : TXMLDocument;
    FData : TG2GraphModuleFMX;
    FPanel : TSVGPath;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;

    function    CreatePath( aNode : TDOMNode; aParent : TSVGGroup) : TSVGPath; override;
    function    CreateGroup( aNode : TDOMNode; aParent : TSVGGroup; var aSubTreeOwner : TSVGGroup) : TSVGGroup; override;

    function    SelectControl( P : TPointF): TSVGG2Control;

    function    GetNewCol: TBits7;
    function    GetNewRow: TBits7;
    procedure   SetModuleColor( aValue : TBits8);

    procedure   SetSelected( aValue : boolean);

    property    Data : TG2GraphModuleFMX read FData write FData;
  end;

function SubTextReplace( aText, aSubText, aReplaceText : string): string;

implementation

//==============================================================================
//
//                                 Utils
//
//==============================================================================

function SubTextReplace( aText, aSubText, aReplaceText : string): string;
var p : integer;
begin
  p := pos( aSubText, aText);
  if p > 0 then begin
    Result := copy( aText, 1, p-1)
            + aReplaceText
            + copy( aText, p + Length(aSubText), Length(aText) - (p + Length(aSubText) - 1));
  end else
    Result := aText;
end;

function NodeID( aNode : TDomNode): string;
var n : TDomNode;
begin
  Result := '';
  if aNode.HasAttributes then begin
    n := aNode.Attributes.GetNamedItem('id');
    if n <> nil then begin
      Result := n.TextContent;
    end;
  end;
end;

function NodeAttribute( aNode : TDomNode; aName : string): string;
var n : TDomNode;
begin
  Result := '';
  if aNode.HasAttributes then begin
    n := aNode.Attributes.GetNamedItem(aName);
    if n <> nil then begin
      Result := n.TextContent;
    end;
  end;
end;

//==============================================================================
//
//                              TG2GraphFMX
//
//==============================================================================

constructor TG2GraphFMX.Create(AOwner: TComponent);
begin
  inherited;
end;

function TG2GraphFMX.CreatePerformance: TG2FilePerformance;
begin
  Result := TG2GraphPerformanceFMX.Create(self);
end;

destructor TG2GraphFMX.Destroy;
begin
  inherited;
end;

//==============================================================================
//
//                          TG2GraphPerformanceFMX
//
//==============================================================================

constructor TG2GraphPerformanceFMX.Create(AOwner: TComponent);
begin
  inherited;

end;

function TG2GraphPerformanceFMX.CreateSlot: TG2FileSlot;
begin
  Result := TG2GraphSlotFMX.Create( self);
end;

destructor TG2GraphPerformanceFMX.Destroy;
begin

  inherited;
end;

//==============================================================================
//
//                             TG2GraphSlotFMX
//
//==============================================================================

constructor TG2GraphSlotFMX.Create(AOwner: TComponent);
begin
  inherited;

end;

function TG2GraphSlotFMX.CreatePatch: TG2FilePatch;
begin
  Result := TG2GraphPatchFMX.Create( self);
end;

destructor TG2GraphSlotFMX.Destroy;
begin

  inherited;
end;

//==============================================================================
//
//                              TG2GraphPatchFMX
//
//==============================================================================


constructor TG2GraphPatchFMX.Create(AOwner: TComponent);
begin
  FMiniVUList := TList.Create;
  FLedGroupList := TList.Create;
  FLed39List := TList.Create;
  FLed3AList := TList.Create;

  FSelectedMorphIndex := 0;

  inherited;
end;

destructor TG2GraphPatchFMX.Destroy;
begin
  FLed3AList.Free;
  FLed39List.Free;
  FLedGroupList.Free;
  FMiniVUList.Free;

  inherited;
end;

procedure TG2GraphPatchFMX.Init;
begin
  inherited;

  FSelectedControl := nil;

  FLed3AList.Clear;
  FLed39List.Clear;
  FLedGroupList.Clear;
  FMiniVUList.Clear;
end;

function TG2GraphPatchFMX.CreateModule(aLocation: TLocationType; aModuleIndex,
  aModuleType: byte): TG2FileModule;
var i : integer;
    Module : TG2GraphModuleFMX;
begin
  // Create a module in a patch file

  Result := nil;
  Module := TG2GraphModuleFMX.Create( PatchPart[ ord(aLocation)]);
  Module.ModuleIndex := aModuleIndex;
  Module.TypeID := aModuleType;

  if assigned( G2) and assigned(G2.FModuleDefList) and assigned(G2.FParamDefList) then begin
    i := 0;
    while (i < G2.FModuleDefList.Count) and ( G2.FModuleDefList.ModuleDef[i].ModuleType <> aModuleType) do
      inc(i);

    if (i < G2.FModuleDefList.Count) then begin

      Module.InitModule( aLocation, G2.FModuleDefList.ModuleDef[i], G2.FParamDefList);

      if (G2.ClientType <> ctVST) then begin
        if aLocation = ltVA then begin
          Module.ParsePanelData( FLayout, FSVGSkin, aModuleType);
        end else
          {if aLocation = ltFX then begin
            Module.Parent := (G2 as TG2Graph).FLayoutFX;
            Module.ParsePanelData;
          end;}
      end;
    end else
      raise Exception.Create('Unknown module type ' + IntToStr( aModuleType));;
  end;
  Module.Location := aLocation;

  if assigned(FOnCreateModuleFMX) then
    FOnCreateModuleFMX( self, Module);

  Result := Module;
end;

function TG2GraphPatchFMX.CreateCable(aLocation: TLocationType; aColor,
  aFromModule, aFromConnector, aLinkType, aToModule,
  aToConnector: byte): TG2FileCable;
var i : integer;
    FromConnKind : TConnectorKind;
    ModuleFrom, ModuleTo : TG2GraphModuleFMX;
    Cable : TG2GraphCableFMX;
    ConnectorFrom, ConnectorTo : TSVGG2Connector;
begin
  // Create a cable connection in a patch file

  Result := nil;

  ModuleFrom := GetModule( ord(aLocation), aFromModule) as TG2GraphModuleFMX;
  if not assigned(ModuleFrom) then
    raise Exception.Create('ModuleIndex ' + IntToStr( aFromModule) + ' not found.');

  ModuleTo := GetModule( ord(aLocation), aToModule) as TG2GraphModuleFMX;
  if not assigned(ModuleTo) then
    raise Exception.Create('ModuleIndex ' + IntToStr( aToModule) + ' not found.');

  Cable               := TG2GraphCableFMX.Create( self);
  Cable.CableColor    := aColor;
  Cable.ModuleFrom    := aFromModule;
  Cable.ConnectorFrom := aFromConnector;
  Cable.LinkType      := aLinkType;
  Cable.ModuleTo      := aToModule;
  Cable.ConnectorTo   := aToConnector;

  // If Linktype is 1 then the first connector is an output, else it's an input (i guess)
  if aLinkType = 1 then
    FromConnKind := ckOutput
  else
    FromConnKind := ckInput;

  if assigned( G2) and ( G2.ClientType <> ctVST) then begin

    // Link connectors to cable
    if FromConnKind = ckInput then
      Cable.FromConnector := ModuleFrom.InConnector[ aFromConnector]
    else
      Cable.FromConnector := ModuleFrom.OutConnector[ aFromConnector];

    Cable.ToConnector := ModuleTo.InConnector[ aToConnector];

    // Add cable to connectors
    Cable.FromConnector.AddCable(Cable);
    Cable.ToConnector.AddCable(Cable);

    if aLocation = ltFX then
      //Cable.Parent := (G2 as TG2Graph).FLayoutFX
    else begin
      Cable.Parent := Layout;
      ConnectorFrom := Cable.FromConnector.GraphControl as TSVGG2Connector;
      ConnectorTo := Cable.ToConnector.GraphControl as TSVGG2Connector;
      if assigned(ConnectorFrom) and assigned(ConnectorTo) then
        Cable.InitCable;
    end;
  end;

  Result := Cable;
end;

function TG2GraphPatchFMX.CreateParameter(aModuleIndex: byte): TG2FileParameter;
begin
  Result := TG2GraphParameterFMX.Create( self, ltPatch, aModuleIndex, nil);
end;

function TG2GraphPatchFMX.GetLedListCount: integer;
begin
  Result := FLed39List.Count;
end;

function TG2GraphPatchFMX.GetMiniVUListCount: integer;
begin
  Result := FLed3AList.Count;
end;

procedure TG2GraphPatchFMX.RemoveFromLedList(aLocation: TLocationType;
  aModuleIndex: integer);
begin

end;

procedure TG2GraphPatchFMX.SetLedLevel(Index: integer; aValue: byte);
begin

end;

procedure TG2GraphPatchFMX.SetMiniVULevel(Index: integer; aValue: byte);
begin

end;

procedure TG2GraphPatchFMX.SetSelectedControl(aValue: TControl);
begin

end;

procedure TG2GraphPatchFMX.SetSelectedMorphIndex(aValue: integer);
begin

end;

procedure TG2GraphPatchFMX.SetVisible(aValue: boolean);
begin

end;

procedure TG2GraphPatchFMX.SortLeds;
begin
  inherited;

end;

//==============================================================================
//
//                          TG2GraphParameterFMX
//
//==============================================================================

constructor TG2GraphParameterFMX.Create(aPatch: TG2FilePatch;
  aLocation: TLocationType; aModuleIndex: integer; aModule: TG2FileModule);
begin
  inherited Create( aPatch, aLocation, aModuleIndex, aModule);

  SetLength(FControlList, 0);
end;

destructor TG2GraphParameterFMX.Destroy;
var i : integer;
begin
  for i := 0 to Length(FControlList) - 1 do
    FControlList[i].FParameter := nil;
  Finalize( FControlList);
  inherited;
end;

procedure TG2GraphParameterFMX.AssignControl(aControl: TSVGG2Control);
var i : integer;
begin
  if not(aControl is TSVGG2Control) then
    raise Exception.Create('Only a TG2GraphControlFMX can be assigned to a parameter.');

  i := 0;
  while (i < Length(FControlList)) and not(FControlList[i] = aControl) do
    inc(i);

  if not(i < Length(FControlList)) then begin
    SetLength(FControlList, i + 1);
    FControlList[i] := aControl as TSVGG2Control;
  end;
end;

procedure TG2GraphParameterFMX.DeassignControl(aControl: TSVGG2Control);
var i, j : integer;
begin
  if not(aControl is TSVGG2Control) then
    raise Exception.Create('Only a TG2GraphControlFMX can be assigned to a parameter.');

  i := 0;
  while (i < Length(FControlList)) and not(FControlList[i] = aControl) do
    inc(i);

  if (i < Length(FControlList)) then begin
    FControlList[i].FParameter := nil;
    for j := i + 1 to Length(FControlList) - 1 do begin
      FControlList[j-1] := FControlList[j];
    end;
    SetLength(FControlList, Length(FControlList) - 1);
  end;
end;

procedure TG2GraphParameterFMX.InvalidateControl;
var i : integer;
begin
  // Update all controls attached to the parameter
  for i := 0 to Length(FControlList) - 1 do begin
    FControlList[i].Repaint;
  end;
end;


//==============================================================================
//
//                               TSVGG2ParamLink
//
//==============================================================================

constructor TSVGG2ParamLink.Create(AOwner: TComponent);
begin
  inherited;
  FModule := nil;
  FCodeRef := 0;
  FMasterRef := 0;
  FInfoFunc := 0;
  FTextFunc := 0;
  FDependencies := '';
end;

function TSVGG2ParamLink.CreateGroup(aNode: TDOMNode; aParent: TSVGGroup;
  var aSubTreeOwner: TSVGGroup): TSVGGroup;
var BtnText : TSVGBtnText;
    SVGAgent : TSVGAgent;
    id, BtnDownId : string;
begin
  id := NodeID( aNode);

  if pos('btnTextUp', id)>0 then begin
    BtnText := TSVGBtnText.Create(self);
    Result := BtnText.FBtnUp;
    aSubTreeOwner := Result;
    aParent.Add(Result);

    SVGAgent := TSVGAgent.Create;
    try
      BtnDownId := SubTextReplace(id, 'Up', 'Down');
      aParent.Add(BtnText.FBtnDown);
      SVGAgent.ParseSVG( aNode.OwnerDocument as TXMLDocument, BtnDownId, BtnText.FBtnDown);
    finally
      SVGAgent.Free;
    end;

  end else begin
    Result := inherited;
  end;
end;

destructor TSVGG2ParamLink.Destroy;
begin
  inherited;
end;

//==============================================================================
//
//                               TSVGG2ConnLink
//
//==============================================================================

constructor TSVGG2ConnLink.Create(AOwner: TComponent);
begin
  inherited;
  FModule := nil;
  FCodeRef := 0;
end;

function TSVGG2ConnLink.CreateGroup(aNode: TDOMNode; aParent: TSVGGroup;
  var aSubTreeOwner: TSVGGroup): TSVGGroup;
var Connector : TSVGG2Connector;
    id : string;
begin
  id := NodeID( aNode);

  if (id = 'connIn_blue') or (id = 'connIn_red') or (id = 'connIn_yellow') or (id = 'connIn_orange') then begin

    Connector := TSVGG2Connector.Create(self);
    Connector.Data := FModule.InConnector[ FCodeRef];
    aSubTreeOwner := Connector;
    aParent.Add(Connector);
    Result := Connector;
  end else

  if (id = 'connOut_blue') or (id = 'connOut_red') or (id = 'connOut_yellow') or (id = 'connOut_orange') then begin
    Connector := TSVGG2Connector.Create(self);
    Connector.Data := FModule.OutConnector[ FCodeRef];
    aSubTreeOwner := Connector;
    aParent.Add(Connector);
    Result := Connector;

  end else begin
    Result := inherited;
  end;
end;

destructor TSVGG2ConnLink.Destroy;
begin
  inherited;
end;


//==============================================================================
//
//                               TSVGG2Control
//
//==============================================================================


constructor TSVGG2Control.Create(AOwner: TComponent);
begin
  inherited;
  FParameter := nil;
end;

destructor TSVGG2Control.Destroy;
begin
  inherited;
end;

procedure TSVGG2Control.MouseDown(P: TPointF);
begin
//
end;

procedure TSVGG2Control.MouseMove(P: TPointF; dx, dy: single);
begin
//
end;

procedure TSVGG2Control.MouseUp(P: TPointF);
begin
//
end;

//==============================================================================
//
//                                TSVGHitPath
//
//==============================================================================

constructor TSVGHitPath.Create(AOwner: TComponent);
begin
  inherited;
  HitTest := True;
  FControl := nil;
end;

destructor TSVGHitPath.Destroy;
begin
  inherited;
end;

//==============================================================================
//
//                                TSVGBtnState
//
//==============================================================================

constructor TSVGBtnState.Create(AOwner: TComponent);
begin
  inherited;
  FControl := AOwner as TSVGG2Control;
  FBtnBackground := nil;
end;

destructor TSVGBtnState.Destroy;
begin
  inherited;
end;

function TSVGBtnState.CreatePath(aNode: TDOMNode; aParent: TSVGGroup): TSVGPath;
var id : string;
begin
  id := NodeID( aNode);


  if pos('_bg', id)>0  then begin
    FBtnBackground := TSVGHitPath.Create(self);
    FBtnBackground.Control := FControl;
    aParent.Add(FBtnBackground);
    Result := FBtnBackground;
  end else
    result := inherited;
end;


//==============================================================================
//
//                                TSVGBtnText
//
//==============================================================================

constructor TSVGBtnText.Create(AOwner: TComponent);
begin
  inherited;
  FBtnUp := TSVGBtnState.Create(self);
  FBtnDown := TSVGBtnState.Create(self);

  FBtnUp.Visible := True;
  FBtnDown.Visible := False;
  FValue := 0;
end;

destructor TSVGBtnText.Destroy;
begin
  inherited;
end;

procedure TSVGBtnText.MouseDown(P: TPointF);
begin
  Value := 1 - Value;
end;

procedure TSVGBtnText.SetValue(aValue: single);
begin
  if aValue  <> FValue then begin
    FValue := aValue;

    if FValue = 0 then begin
      FBtnUp.Visible := True;
      FBtnDown.Visible := False;
    end else begin
      FBtnUp.Visible := False;
      FBtnDown.Visible := True;
    end;

  end;
end;


//==============================================================================
//
//                               TSVGKnob
//
//==============================================================================

constructor TSVGKnob.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TSVGKnob.Destroy;
begin
  inherited;
end;

procedure TSVGKnob.MouseMove(P: TPointF; dx, dy: single);
begin
  inherited;

  Value :=  dx / 100 * 360;
end;

function TSVGKnob.CreateGroup(aNode: TDOMNode; aParent : TSVGGroup;
  var aSubTreeOwner: TSVGGroup): TSVGGroup;
var n : TDOMNode;
    id : string;
begin
  id := NodeID( aNode);

  Result := inherited;

  if (id = 'gknobBig_needle') or (id = 'gknobMedium_needle') or (id = 'gknobSmall_needle') then begin
    FKnobNeedle := Result;
  end;
end;

function TSVGKnob.CreatePath(aNode: TDOMNode; aParent : TSVGGroup): TSVGPath;
var id : string;
begin
  id := NodeID( aNode);

  if (id = 'knobBig_face') or (id = 'knobMedium_face') or (id = 'knobSmall_face') then begin
    FKnobFace := TSVGHitPath.Create(self);
    aParent.Add(FKnobFace);
    FKnobFace.Control := self;
    Result := FKnobFace;
  end else begin
    Result := inherited;
    if (id = 'knobBig_morph') or (id = 'knobMedium_morph') or (id = 'knobSmall_morph') then begin
      FKnobMorph := Result;
    end;
  end;
end;

procedure TSVGKnob.SetLayoutAngle(aLayout: TSVGGroup; aAngle: single);
var MorphPath : string;
begin
  aLayout.Transform(1,1,0,0, aAngle*PI/180, 0,0);

  FKnobMorph.Data.Clear;
  FKnobMorph.Data.MoveTo( PointF(0, 0));
  FKnobMorph.Data.LineTo( PointF(100*sin(aAngle*PI/180), -100*cos(aAngle*PI/180)));
  FKnobMorph.Data.AddArc( PointF(0,0), PointF(100,100), aAngle-90, 45);
  FKnobMorph.Data.ClosePath;
  FKnobMorph.Data.ApplyPathData;

  //Repaint;
end;

procedure TSVGKnob.SetValue(aValue: single);
begin
  if aValue <> FValue then begin
    FValue := aValue;
    SetLayoutAngle( FKnobNeedle, FValue);
  end;
end;

//==============================================================================
//
//                             TSVGG2Connector
//
//==============================================================================

constructor TSVGG2Connector.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TSVGG2Connector.Destroy;
begin
  inherited;
end;

function TSVGG2Connector.CreatePath(aNode: TDOMNode;
  aParent: TSVGGroup): TSVGPath;
var id : string;
begin
  id := NodeID( aNode);

  if pos('_border', id)>0 then begin
    FBorder := TSVGHitPath.Create(self);
    aParent.Add(FBorder);
    FBorder.Control := self;
    Result := FBorder;
  end else begin
    Result := inherited;
  end;
end;

procedure TSVGG2Connector.SetData( aValue : TG2FileConnector);
begin
  FData := aValue;
  FData.GraphControl := self;
end;

//==============================================================================
//
//                             TG2GraphCableFMX
//
//==============================================================================

constructor TG2GraphCableFMX.Create(AOwner: TComponent);
var i : integer;
begin
  if not( AOwner is TG2GraphPatchFMX) then
    raise Exception.Create('Owner must be a patch');

  FPatch := AOwner as TG2GraphPatchFMX;
  FParent := nil;

  FromConnector := nil;
  ToConnector   := nil;

  FSVGControl := TSVGG2Cable.Create(AOwner);
  FSVGControl.FData := self;

  InitCable;
end;

destructor TG2GraphCableFMX.Destroy;
begin
  FSVGControl.Free;

  // Remove the cable from the connectors
  if assigned(FromConnector) then
    FromConnector.DelCable( self);

  if assigned(ToConnector) then
    ToConnector.DelCable( self);

  inherited;
end;

procedure TG2GraphCableFMX.ConnectorMoved;
var ModuleFrom, ModuleTo : TSVGG2Module;
    ConnectorFrom, ConnectorTo : TSVGG2Connector;
    P : TPointF;
begin
  if not(assigned(FromConnector) and (assigned(ToConnector)
      and (assigned(FromConnector.Module)) )) then
    exit;

  ModuleFrom := (FromConnector.Module as TG2GraphModuleFMX).SVGControl;
  ModuleTo := (ToConnector.Module as TG2GraphModuleFMX).SVGControl;

  ConnectorFrom := FromConnector.GraphControl as TSVGG2Connector;
  ConnectorTo := ToConnector.GraphControl as TSVGG2Connector;

  P.X := ConnectorFrom.Position.X + ConnectorFrom.Width/2;
  P.Y := ConnectorFrom.Position.Y + ConnectorFrom.Height/2;
  FSVGControl.Point1X := TransformPoint( P, ConnectorFrom.CTM).X + ModuleFrom.Position.X;
  FSVGControl.Point1Y := TransformPoint( P, ConnectorFrom.CTM).Y + ModuleFrom.Position.Y;

  P.X := ConnectorTo.Position.X + ConnectorTo.Width/2;
  P.Y := ConnectorTo.Position.Y + ConnectorTo.Height/2;
  FSVGControl.Point2X := TransformPoint( P, ConnectorTo.CTM).X + ModuleTo.Position.X;
  FSVGControl.Point2Y := TransformPoint( P, ConnectorTo.CTM).Y + ModuleTo.Position.Y;

  FSVGControl.StartTimer;
end;


procedure TG2GraphCableFMX.InitCable;
var ModuleFrom, ModuleTo : TSVGG2Module;
    ConnectorFrom, ConnectorTo : TSVGG2Connector;
    P : TPointF;
begin
  if not(assigned(FromConnector) and (assigned(ToConnector)
      and (assigned(FromConnector.Module)) )) then
    exit;

  ModuleFrom := (FromConnector.Module as TG2GraphModuleFMX).SVGControl;
  ModuleTo := (ToConnector.Module as TG2GraphModuleFMX).SVGControl;

  ConnectorFrom := FromConnector.GraphControl as TSVGG2Connector;
  ConnectorTo := ToConnector.GraphControl as TSVGG2Connector;

  P.X := ConnectorFrom.Position.X + ConnectorFrom.Width/2;
  P.Y := ConnectorFrom.Position.Y + ConnectorFrom.Height/2;
  FSVGControl.Point1X := TransformPoint( P, ConnectorFrom.CTM).X + ModuleFrom.Position.X;
  FSVGControl.Point1Y := TransformPoint( P, ConnectorFrom.CTM).Y + ModuleFrom.Position.Y;

  P.X := ConnectorTo.Position.X + ConnectorTo.Width/2;
  P.Y := ConnectorTo.Position.Y + ConnectorTo.Height/2;
  FSVGControl.Point2X := TransformPoint( P, ConnectorTo.CTM).X + ModuleTo.Position.X;
  FSVGControl.Point2Y := TransformPoint( P, ConnectorTo.CTM).Y + ModuleTo.Position.Y;

  FSVGControl.InitCable;
end;

procedure TG2GraphCableFMX.SetParent(aValue: TFmxObject);
begin
  FParent := aValue;
  FSVGControl.Parent := FParent;
end;


//==============================================================================
//
//                                TSVGG2Cable
//
//==============================================================================

constructor TSVGG2Cable.Create(AOwner: TComponent);
var i : integer;
begin
  inherited;

  HitTest := False;
  FTimer := TTimer.Create(self);
  FTimer.Interval := 25;

  FMargin := 10;

  FP1.X := Position.X + FMargin;
  FP1.Y := Position.Y + FMargin;

  FP2.X := Position.X + Width - FMargin * 2;
  FP2.Y := Position.Y + FMargin;

  FNodeCount := 10;
  SetLength(FNodes, FNodeCount);
  for i := 0 to FNodeCount - 1 do
    FNodes[i] := TNode.Create;

  FTimer.OnTimer := OnTimer;
  InitCable;
end;

destructor TSVGG2Cable.Destroy;
var i : integer;
begin
  for i := 0 to FNodeCount - 1 do
    FNodes[i].Free;
  Finalize(FNodes);

  FTimer.Enabled := False;
  FTimer.Free;

  inherited;
end;

procedure TSVGG2Cable.InitCable;
var n : integer;
    dx, dy : single;
    min_x, max_x, min_y, max_y : single;
    halfx, maxsag : single;

    function Caterny( x : single): single;
    var g, H : single;
    begin
    { z = -H/g (cosh (gx/H)-1)

      where
      z = vertical distance
      H = Horizontal component of cable force
      g = weight of cable / unit of lenght
      x = horizontal distance

      and
      dz/dx = -sinh(gx/H)
      V = - H sinh(gx/H)

      where
      V = vertical component of cable force
      N = H - gz

      where
      N = tension force in the cable

      Read more: http://wiki.answers.com/Q/What_is_the_formula_for_catenary_calculation#ixzz1iD2bBQpF}

      g := 1;
      H := 1000;
      Result := -H/g *(cosh (g*x/H)-1);
    end;

begin
  min_x := min(FP1.X, FP2.X);
  max_x := max(FP1.X, FP2.X);
  min_y := min(FP1.Y, FP2.Y);
  max_y := max(FP1.Y, FP2.Y);

  n := FNodeCount - 1;

  dx := ( FP2.X - FP1.X) / (n + 1);
  dy := ( FP2.Y - FP1.Y) / (n + 1);

  halfx := ( FP2.X - FP1.X) / 2;
  maxsag := Caterny(-halfx);

  FNodes[n].x := FP2.X;
  FNodes[n].y := FP2.Y;

  dec(n);
  while (n >= 0) do begin

    FNodes[n].x := FP1.X + dx * n;
    FNodes[n].y := FP1.Y + dy * n + Caterny(dx * n - halfx) - maxsag ;
    FNodes[n].vx := 0;
    FNodes[n].vy := 0;

    if FNodes[n].x < min_x then
      min_x := FNodes[n].x;

    if FNodes[n].x > max_x then
      max_x := FNodes[n].x;

    if FNodes[n].y < min_y then
      min_y := FNodes[n].y;

    if FNodes[n].y > max_y then
      max_y := FNodes[n].y;

    dec(n);
  end;
  FNodes[0].x := FP1.X;
  FNodes[0].y := FP1.y;

  Position.X := min_x - FMargin;
  Position.Y := min_y - FMargin;
  Width := max_x - Position.X + FMargin;
  Height := max_y - Position.Y + FMargin;
end;

procedure TSVGG2Cable.IterateCable;
var n : integer;
    min_x, min_y, max_x, max_y : single;
begin
  n := FNodeCount - 1;

  min_x := min(FP1.X, FP2.X);
  max_x := max(FP1.X, FP2.X);
  min_y := min(FP1.Y, FP2.Y);
  max_y := max(FP1.Y, FP2.Y);

  FNodes[n].x := FP2.X;
  FNodes[n].y := FP2.Y;

  dec(n);

  while (n > 0) do begin

  	FNodes[n].vx := FNodes[n].vx + ( FNodes[n + 1].x + FNodes[n - 1].x - FNodes[n].x * 2 ) / TENSION;
		FNodes[n].vy := FNodes[n].vy + ( FNodes[n + 1].y + FNodes[n - 1].y - FNodes[n].y * 2 ) / TENSION;

    FNodes[n].vy := FNodes[n].vy + GRAVITY;

    //-- Reibung
    FNodes[n].vx := FNodes[n].vx * DAMP;
    FNodes[n].vy := FNodes[n].vy * DAMP;

    //-- Addieren der neuen Vektoren
    FNodes[n].x := FNodes[n].x + FNodes[n].vx;
    FNodes[n].y := FNodes[n].y + FNodes[n].vy;

    if FNodes[n].x < min_x then
      min_x := FNodes[n].x;

    if FNodes[n].x > max_x then
      max_x := FNodes[n].x;

    if FNodes[n].y < min_y then
      min_y := FNodes[n].y;

    if FNodes[n].y > max_y then
      max_y := FNodes[n].y;

    dec(n);
  end;
  FNodes[0].x := FP1.X;
  FNodes[0].y := FP1.Y;

  Position.X := min_x - FMargin;
  Position.Y := min_y - FMargin;
  Width := max_x - Position.X + FMargin;
  Height := max_y - Position.Y + FMargin;
end;

procedure TSVGG2Cable.OnTimer(Sender: TObject);
begin
  if FTimerCount <= 0 then
    FTimer.Enabled := False;
  Dec(FTimerCount);
  IterateCable;
  Repaint;
end;

procedure TSVGG2Cable.Paint;
var i : integer;
    Path : TPathData;
    PathPoint : TPathPoint;
    BB : TRectF;
    P1, P2, P3, P4, V, N, G1, G2 : TPointF;
    L, d : single;
begin
  exit;
  d := 1.5;

  Canvas.BeginScene;
  Path := TPathData.Create;
  try

    Canvas.Stroke.Kind := TBrushKind.bkNone;

    Canvas.Fill.Kind := TBrushKind.bkGradient;
    Canvas.Fill.Gradient.Color := claGray;
    Canvas.Fill.Gradient.Color1 := CableColors[ FData.CableColor];

    for i := 1 to FNodeCount - 1 do begin
      V.X := FNodes[i].x - FNodes[i-1].x;
      V.Y := FNodes[i].y - FNodes[i-1].y;
      N.X := V.Y;
      N.Y := -V.X;
      L := sqrt( N.X * N.X + N.Y * N.Y);
      if L <> 0 then begin
        N.X := N.X / L;
        N.Y := N.Y / L;
      end;

      P1.X := FNodes[i-1].x - Position.X + N.X*d;
      P1.Y := FNodes[i-1].y - Position.Y + N.Y*d;
      P2.X := FNodes[i].x - Position.X + N.X*d;
      P2.Y := FNodes[i].y - Position.Y + N.Y*d;
      P3.X := FNodes[i].x - Position.X - N.X*d;
      P3.Y := FNodes[i].y - Position.Y - N.Y*d;
      P4.X := FNodes[i-1].x - Position.X - N.X*d;
      P4.Y := FNodes[i-1].y - Position.Y - N.Y*d;

      BB.Left := min(min(min( p1.X, p2.X), p3.X), p4.X);
      BB.Top := min(min(min( p1.Y, p2.Y), p3.Y), p4.Y);
      BB.Right := max(max(max( p1.X, p2.X), p3.X), p4.X);
      BB.Bottom := max(max(max( p1.Y, p2.Y), p3.Y), p4.Y);

      G1.X := P1.X + (P2.X - P1.X)/2;
      G1.Y := P1.Y + (P2.Y - P1.Y)/2;
      G2.X := P4.X + (P3.X - P4.X)/2;
      G2.Y := P4.Y + (P3.Y - P4.Y)/2;

      G1.X := (G1.X - BB.Left) / (BB.Right - BB.Left);
      G1.Y := (G1.Y - BB.Top) / (BB.Bottom - BB.Top);
      G2.X := (G2.X - BB.Left) / (BB.Right - BB.Left);
      G2.Y := (G2.Y - BB.Top) / (BB.Bottom - BB.Top);

      Canvas.Fill.Gradient.StartPosition.Point := PointF( G1.X, G1.Y);
      Canvas.Fill.Gradient.StopPosition.Point  := PointF( G2.X, G2.Y);

      Path.Clear;
      Path.MoveTo( P1);
      Path.LineTo( P2);
      Path.LineTo( P3);
      Path.LineTo( P4);
      Path.ClosePath;

      Canvas.FillPath( Path, Opacity);
    end;
  finally
    Path.Free;
    Canvas.EndScene;
  end;
end;

procedure TSVGG2Cable.SetPoint1X(Value: single);
begin
  FP1.X := Value;
end;

procedure TSVGG2Cable.SetPoint1Y(Value: single);
begin
  FP1.Y := Value;
end;

procedure TSVGG2Cable.SetPoint2X(Value: single);
begin
  FP2.X := Value;
end;

procedure TSVGG2Cable.SetPoint2Y(Value: single);
begin
  FP2.Y := Value;
end;

procedure TSVGG2Cable.StartTimer;
begin
  IterateCable;
  FTimerCount := 50;
  FTimer.Enabled := True;
end;

//==============================================================================
//
//                             TG2GraphModuleFMX
//
//==============================================================================

constructor TG2GraphModuleFMX.Create(aPatchPart: TG2FilePatchPart);
begin
  inherited Create( aPatchPart);
  FSVGControl := nil;
end;

constructor TG2GraphModuleFMX.CopyCreate(aPatchPart: TG2FilePatchPart;
  aModule: TG2GraphModuleFMX);
begin
  inherited Create( aPatchPart);
  Copy( aModule);
  // TODO
end;

function TG2GraphModuleFMX.CreateCopy(
  aPatchPart: TG2FilePatchPart): TG2FileModule;
begin
  Result := TG2GraphModuleFMX.CopyCreate( aPatchPart, self);
end;

function TG2GraphModuleFMX.CreateParameter: TG2FileParameter;
begin
  Result := TG2GraphParameterFMX.Create( PatchPart.Patch, TLocationType(Location), ModuleIndex, self);
end;

destructor TG2GraphModuleFMX.Destroy;
begin
  inherited;
end;

function TG2GraphModuleFMX.GetNewCol: TBits7;
begin
  if assigned(FSVGControl) then
    Result := FSVGControl.GetNewCol
  else
    Result := 0;
end;

function TG2GraphModuleFMX.GetNewRow: TBits7;
begin
  if assigned(FSVGControl) then
    Result := FSVGControl.GetNewRow
  else
    Result := 0;
end;

procedure TG2GraphModuleFMX.ParsePanelData(aParent: TFMXObject;
  aSkin: TXMLDocument; aModuleType : byte);
var SVGAgent : TSVGAgent;
begin
  if assigned(aSkin) and assigned(aParent) then begin
    SVGAgent := TSVGAgent.Create;
    try
      FSVGControl := TSVGG2Module.Create( self);
      FSVGControl.Data := self;
      FSVGControl.FSVGSkin := aSkin;
      aParent.AddObject( FSVGControl);

      SVGAgent.ParseSVG( aSkin, 'module_' + IntToStr(aModuleType), FSVGControl);
    finally
      SVGAgent.Free;
    end;
  end;
end;

procedure TG2GraphModuleFMX.SetCol(aValue: TBits7);
begin
  if assigned(FSVGControl) then
    FSVGControl.Position.X := aValue * UNITS_COL;
  inherited;
  InvalidateCables;
end;

procedure TG2GraphModuleFMX.SetModuleColor(aValue: TBits8);
begin
  inherited;
  if assigned(FSVGControl) then
    FSVGControl.SetModuleColor( aValue);
end;

procedure TG2GraphModuleFMX.SetModuleName(aValue: AnsiString);
begin
  inherited;
  if assigned(FSVGControl) then;
end;

procedure TG2GraphModuleFMX.SetRow(aValue: TBits7);
begin
  if assigned(FSVGControl) then
    FSVGControl.Position.Y := aValue * UNITS_ROW;
  inherited;
  InvalidateCables;
end;

procedure TG2GraphModuleFMX.SetSelected(aValue: boolean);
begin
  inherited;
  if assigned(FSVGControl) then
    FSVGControl.SetSelected(aValue);
end;

//==============================================================================
//
//                               TSVGG2Module
//
//==============================================================================

constructor TSVGG2Module.Create(AOwner: TComponent);
begin
  inherited;
  Buffered := True;
end;

function TSVGG2Module.CreateGroup(aNode: TDOMNode; aParent : TSVGGroup; var aSubTreeOwner : TSVGGroup): TSVGGroup;
var id, AttributeValue : string;
    ConnLink : TSVGG2ConnLink;
    ParamLink : TSVGG2ParamLink;
begin
  id := NodeID( aNode);

  if pos('_paramlink_', id)>0 then begin

    ParamLink := TSVGG2ParamLink.Create(self);
    aSubTreeOwner := ParamLink;
    aParent.Add(ParamLink);

    ParamLink.FModule := FData;
    AttributeValue := NodeAttribute( aNode, 'nmg2.CodeRef');
    if AttributeValue <> '' then
      ParamLink.FCodeRef := StrToInt(AttributeValue);
    AttributeValue := NodeAttribute( aNode, 'nmg2.InfoFunc');
    if AttributeValue <> '' then
      ParamLink.FInfoFunc := StrToInt(AttributeValue);
    AttributeValue := NodeAttribute( aNode, 'nmg2.TextFunc');
    if AttributeValue <> '' then
      ParamLink.FTextFunc := StrToInt(AttributeValue);
    AttributeValue := NodeAttribute( aNode, 'nmg2.MasterRef');
    if AttributeValue <> '' then
      ParamLink.FMasterRef := StrToInt(AttributeValue);
    AttributeValue := NodeAttribute( aNode, 'nmg2.Dependencies');
    ParamLink.FDependencies := AttributeValue;

    Result := ParamLink;

  end else

  if pos('_connlink_', id)>0 then begin

    ConnLink := TSVGG2ConnLink.Create(self);
    aSubTreeOwner := ConnLink;
    aParent.Add(ConnLink);

    ConnLink.FModule := FData;
    AttributeValue := NodeAttribute( aNode, 'nmg2.CodeRef');
    if AttributeValue <> '' then
      ConnLink.FCodeRef := StrToInt(AttributeValue);

    Result := ConnLink;

  end else

  if (id = 'knobBig') or (id = 'knobMedium') or (id = 'knobSmall') then begin
    Result := TSVGKnob.Create(self);
    aSubTreeOwner := Result;
    aParent.Add(Result);
  end else

    Result := inherited;
end;

function TSVGG2Module.CreatePath(aNode: TDOMNode; aParent : TSVGGroup): TSVGPath;
var n : TDOMNode;
    id : string;
begin
  id := NodeID( aNode);

  Result := inherited;

  if pos('_panel_', id) > 0 then
    FPanel := Result as TSVGPath;
end;

destructor TSVGG2Module.Destroy;
begin
  inherited;
end;

function TSVGG2Module.GetNewCol: TBits7;
begin
  Result := trunc((Position.X) / UNITS_COL);
end;

function TSVGG2Module.GetNewRow: TBits7;
begin
  Result := trunc((Position.Y) / UNITS_ROW);
end;

function TSVGG2Module.SelectControl( P : TPointF): TSVGG2Control;
var SVGObject : TSVGObject;
    SVGHitPath : TSVGHitPath;
begin
  Result := nil;
  SVGObject := ObjectAtPt( P);
  if SVGObject is  TSVGHitPath then begin
    SVGHitPath := SVGObject as TSVGHitPath;
    Result := SVGHitPath.Control;
  end;
end;

procedure TSVGG2Module.SetModuleColor(aValue: TBits8);
var SVGAgent : TSVGAgent;
begin
  if assigned(FSVGSkin) then begin
    SVGAgent := TSVGAgent.Create;
    try
      SVGAgent.ParseSVGPaintServer( FSVGSkin, 'PanelGradient_' + IntToStr(aValue), FPanel.Fill, RectF(0,0,1,1), RectF(0,0,1,1));
    finally
      SVGAgent.Free;
    end;
  end;
end;

procedure TSVGG2Module.SetSelected(aValue: boolean);
begin
  if aValue then
    FPanel.Stroke.Color := claWhite
  else
    FPanel.Stroke.Color := claBlack;
  Repaint;
end;



end.
