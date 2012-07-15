unit G2FMXGraph;

interface

uses
  System.SysUtils, System.Classes, System.UITypes, Types,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Layouts, FMX.Objects,
  G2_Types, G2_File, G2_usb;

const
  TENSION = 10;
	DAMP    = 0.8;
	GRAVITY = 0.04;

type
  TModuleClickEvent = procedure(Sender : TObject; Button: TMouseButton; Shift: TShiftState; X,  Y: Integer; Module : TG2FileModule) of Object;
  TParameterClickEvent = procedure(Sender : TObject; Button: TMouseButton; Shift: TShiftState; X,  Y: Integer; Parameter : TG2FileParameter) of Object;
  TConnectorClickEvent = procedure(Sender : TObject; Button: TMouseButton; Shift: TShiftState; X,  Y: Integer; Parameter : TG2FileConnector) of Object;

  TG2GraphPatch = class;
  TG2FMXPatchArea = class;
  TG2FMXModule = class;
  TG2FMXControl = class;
  TG2FMXConnector = class;
  TG2FMXCable = class;

  TG2Graph = class( TG2USB)
  // Contains the control for the VA and FX patching
  private
    FLayoutVA : TG2FMXPatchArea;
    FLayoutFX : TG2FMXPatchArea;
    FClientType  : TClientType;
    procedure SetClientType( aValue: TClientType);
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    function    CreatePerformance : TG2FilePerformance; override;
    function    GetSelectedPatch : TG2GraphPatch; virtual;
    procedure   SetPatchAreaVA( aValue : TG2FMXPatchArea);
    procedure   SetPatchAreaFX( aValue : TG2FMXPatchArea);
    function    G2MessageDlg( tekst : string; code : integer): integer; override;
    procedure   G2ProcessWindowsMessages;
  published
    property    ClientType : TClientType read FClientType write SetClientType;
    property    PatchAreaVA : TG2FMXPatchArea read FLayoutVA write SetPatchAreaVA;
    property    PatchAreaFX : TG2FMXPatchArea read FLayoutFX write SetPatchAreaFX;
  end;

  TG2GraphSlot = class( TG2USBSlot)
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    function    CreatePatch : TG2FilePatch; override;
  end;

  TG2GraphPerformance = class( TG2USBPerformance)
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    function    CreateSlot : TG2FileSlot; override;
  end;

  TG2GraphPatch = class( TG2USBPatch)
  // This represents the patch
  private
  // Lists for the leds
    FMiniVUList         : TList;
    FLedGroupList       : TList;
    FLed39List          : TList;
    FLed3AList          : TList;
    FVisible            : boolean;
    FSelectedControl    : TG2FMXControl;
    FSelectedMorphIndex : integer;
    procedure   SetVisible( aValue : boolean);
    procedure   SetSelectedControl( aValue : TG2FMXControl);
    procedure   SetSelectedMorphIndex( aValue : integer);
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   Init; override;

    function    CreateModule( aLocation : TLocationType; aModuleIndex : byte; aModuleType : byte): TG2FileModule; override;
    function    CreateCable( aLocation : TLocationType; aColor : byte; aFromModule : byte; aFromConnector : byte; aLinkType : byte; aToModule : byte; aToConnector : byte): TG2FileCable; override;

    procedure   SelectModulesInRect( aLocation : TLocationType; aRect : TRect);
    procedure   MoveOutlines( aLocation : TLocationType; dX, dY: single);
    function    MessMoveModules( aLocation : TLocationType): boolean;

    function    CreateParameter( aModuleIndex : byte): TG2FileParameter; override;
    procedure   SetMiniVULevel( Index : integer; aValue : byte);
    procedure   SetLedLevel( Index : integer; aValue : byte);
    function    GetMiniVUListCount : integer;
    function    GetLedListCount : integer;
    procedure   SortLeds; override;
    procedure   RemoveFromLedList( aLocation: TLocationType; aModuleIndex : integer);

    property    Visible : boolean read FVisible write SetVisible;
    property    SelectedControl : TG2FMXControl read FSelectedControl write SetSelectedControl;
    property    SelectedMorphIndex : integer read FSelectedMorphIndex write SetSelectedMorphIndex;
  end;

  TG2FMXPatchArea = class(TPanel)
  private
    FNewCable : TG2FMXCable;
    FFromConnector : TG2FMXConnector;
  protected
    procedure   MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure   MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure   MouseMove(Shift: TShiftState; X, Y: Single); override;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
  end;

  TG2GraphModule = class( TG2FileModule)
  private
    FOutlineRect    : TRectF;
    FOutlineVisible : boolean;
    FFreePanel      : boolean;
    FPanel          : TG2FMXModule;
  protected
    function    GetParent : TFmxObject;
    procedure   SetParent( aValue : TFmxObject);
    function    GetVisible : boolean;
    procedure   SetVisible( aValue : boolean);
    function    GetScrollPosX : single;
    function    GetScrollPosY : single;
    procedure   SetSelected( aValue: boolean); override;
    function    GetNewCol : TBits7; override;
    function    GetNewRow : TBits7; override;
    function    GetOnModuleClick : TModuleClickEvent;
    procedure   SetOnModuleClick( aValue : TModuleClickEvent);
    function    GetOnParameterClick: TParameterClickEvent;
    procedure   SetOnParameterClick( aValue : TParameterClickEvent);
    function    GetOnConnectorClick: TConnectorClickEvent;
    procedure   SetOnConnectorClick( aValue : TConnectorClickEvent);
  public
    constructor Create( aPatchPart : TG2FilePatchPart); override;
    constructor CopyCreate( aPatchPart : TG2FilePatchPart; aModule : TG2GraphModule);
    destructor  Destroy; override;
    function    CreateCopy( aPatchPart : TG2FilePatchPart) : TG2FileModule; override;
    function    CreateParameter: TG2FileParameter; override;
    procedure   ParsePanelData;
    function    ClientToScreen( p : TPoint):  TPoint;
    procedure   SetCol( aValue : TBits7); override;
    procedure   SetRow( aValue : TBits7); override;
    procedure   SetModuleColor( aValue : TBits8); override;
    procedure   SetModuleName( aValue : AnsiString); override;
  published
    property    Parent: TFmxObject read GetParent write SetParent;
    property    Visible: boolean read GetVisible write SetVisible;
    property    ScrollPosX : single read GetScrollPosX;
    property    ScrollPosY : single read GetScrollPosY;
    property    OnModuleClick : TModuleClickEvent read GetOnModuleClick write SetOnModuleClick;
    property    OnParameterClick : TParameterClickEvent read GetOnParameterClick write SetOnParameterClick;
    property    OnConnectorClick : TConnectorClickEvent read GetOnConnectorClick write SetOnConnectorClick;
  end;

  TG2FMXModule = class( TRectangle)
  // This represents the module
  private
    FData :   TG2GraphModule;
    FOldX,
    FOldY : single;
    FStartX,
    FStartY : single;
    FWasAlreadySelected : boolean;

    FModuleLabel : TLabel;

    FChildControls : TList;

    FOnModuleClick : TModuleClickEvent;
    FOnParameterClick : TParameterClickEvent;
    FOnConnectorClick : TConnectorClickEvent;

    function    GetScrollBarX : single;
    function    GetScrollBarY : single;
    function    GetScrollPosX : single;
    function    GetScrollPosY : single;
    procedure   SetSelected( aValue : boolean);
    function    GetSelected: boolean;
    function    GetLocation : TLocationType;
    function    GetModuleIndex : TBits8;
    function    GetColor : TColor;
  protected
    procedure   MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure   MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure   MouseMove(Shift: TShiftState; X, Y: Single); override;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure   SetCol( aValue : TBits7);
    procedure   SetRow( aValue : TBits7);
    function    GetNewCol : TBits7;
    function    GetNewRow : TBits7;

    function    GetGraphChildControl( ChildControlIndex : integer): TG2FMXControl;
    function    GetChildControlsCount : integer;
    procedure   AddGraphControl( aGraphCtrl : TG2FMXControl);

    procedure   SelectModule;
    procedure   MoveOutline( dX, dY : single);
    procedure   MoveModule;

    property    ScrollBarX : single read GetScrollBarX;
    property    ScrollBarY : single read GetScrollBarY;
    property    ScrollPosX : single read GetScrollPosX;
    property    ScrollPosY : single read GetScrollPosY;
    property    GraphChildControls[ index : integer] : TG2FMXControl read GetGraphChildControl;
    property    ChildControlsCount : integer read GetChildControlsCount;

    function    GetPatch : TG2GraphPatch;
    function    GetControlType( aG2GraphChildControl: TG2FMXControl): string;
    function    NewG2GraphControl( aControlType : string) : TG2FMXControl;

    procedure   ParsePanelData;

  published
    property    OnMouseDown;
    property    OnMouseUp;
    property    OnMouseMove;
    property    OnClick;
    property    OnModuleClick : TModuleClickEvent read FOnModuleClick write FOnModuleClick;
    property    OnParameterClick : TParameterClickEvent read FOnParameterClick write FOnParameterClick;
    property    OnConnectorClick : TConnectorClickEvent read FOnConnectorClick write FOnConnectorClick;
    property    Selected : boolean read GetSelected write SetSelected;
    property    NewRow : TBits7 read GetNewRow;
    property    NewCol : TBits7 read GetNewCol;
    property    Data : TG2GraphModule read FData write FData;
    property    Location    : TLocationType read GetLocation;
    property    ModuleIndex : TBits8 read GetModuleIndex;
    //property    Color : TColor read GetColor;
  end;

  TG2FMXControl = class( TStyledControl)
  // This represents a child graphic control of another graphic control (module)
  protected
    FSelected       : boolean;
    FModule         : TG2FMXModule;
    FParameter      : TG2FileParameter;
    FMouseInput     : boolean;
    FZOrder         : integer;
    FValue,
    FStartValue,
    FLowValue,
    FHighValue      : byte;
    procedure   MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure   MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure   MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure   Select;
    procedure   DeSelect; virtual;
    procedure   SetLowValue( aValue: byte);
    procedure   SetHighValue( aValue: byte);

  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   SetParameter( aParam : TG2FileParameter);
    procedure   SetModule( aValue : TG2FMXModule); virtual;
    procedure   SetValue( aValue : byte);
    function    GetValue : byte;
    procedure   SetParamLabel( aValue : string);
    function    GetParamLabel : string;
    procedure   InitValue( aValue: integer);
    function    CheckValueBounds( aValue : integer): byte;
    procedure   SetMorphValue( aValue: byte);
    function    GetHighValue : byte;
    function    GetLowValue : byte;
    function    GetGraphModule : TG2FMXModule;
    function    GetMorphValue: byte;
    function    HasMorph: boolean;
    function    GetMorph : TMorphParameter;
    function    GetParamIndex : integer;
    procedure   ParsePanelData( fs : TModuleDefStream); virtual;
    function    ParseProperties( fs: TModuleDefStream; aName : string): boolean; virtual;

    property    Parameter   : TG2FileParameter read FParameter write SetParameter;
    property    ParamIndex  : integer read GetParamIndex;
    property    Module      : TG2FMXModule read FModule write SetModule;
    property    MouseInput  : boolean read FMouseInput write FMouseInput;
  published
    property    Value       : byte read GetValue write SetValue;
    property    MorphValue  : byte read GetMorphValue;
    property    LowValue    : byte read GetLowValue write SetLowValue;
    property    HighValue   : byte read GetHighValue write SetHighValue;
    property    ParamLabel  : string read GetParamLabel write SetParamLabel;
    property    OnMouseUp;
    property    OnMouseDown;
    property    OnMouseMove;
  end;

  TG2GraphParameter = class(TG2FileParameter)
  private
    FControlList  : array of TG2FMXControl; // array of controls the parameter is assigned to
  public
    constructor Create( aPatch : TG2FilePatch; aLocation : TLocationType; aModuleIndex : integer; aModule : TG2FileModule);
    destructor  Destroy; override;
    procedure   AssignControl( aControl : TG2FMXControl);
    procedure   DeassignControl( aControl : TG2FMXControl);
  end;

  TG2FMXConnector = class(TG2FMXControl)
  private
    { Private declarations }
    FFillInner, FFillOuter : TBrush;
    FData   : TG2FileConnector;
  protected
    { Protected declarations }
    procedure   MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure   MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure   MouseMove(Shift: TShiftState; X, Y: Single); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure  Paint; override;
    procedure  ParsePanelData(fs: TModuleDefStream);
    function   ParseProperties( fs: TModuleDefStream; aName : string): boolean;
    procedure  SetData(aConnectorData: TG2FileConnector);

    property Data : TG2FileConnector read FData write SetData;
  end;

  TNode = class
    x  : single;
    y  : single;
    vx : single;
    vy : single;
  end;

  TG2FMXCable = class(TStyledControl)
  private
    { Private declarations }
    FP1, FP2 : TPointF;
    FNodeCount : integer;
    FNodes : array of TNode;
    FMargin : single;
    FTimer : TTimer;
    FTimerCount : integer;
  protected
    { Protected declarations }
    procedure SetPoint1X( Value : single);
    procedure SetPoint1Y( Value : single);
    procedure SetPoint2X( Value : single);
    procedure SetPoint2Y( Value : single);
    procedure IterateCable;
    procedure OnTimer(Sender: TObject);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure StartTimer;
    procedure InitCable;
  published
    { Published declarations }
    property Point1X : single read FP1.X write SetPoint1X;
    property Point1Y : single read FP1.Y write SetPoint1Y;
    property Point2X : single read FP2.X write SetPoint2X;
    property Point2Y : single read FP2.Y write SetPoint2Y;
  end;

  TG2GraphCable = class( TG2FileCable)
  protected
    FParent         : TFmxObject;
    FPatch          : TG2GraphPatch;
    FModule         : TG2FMXModule;
    FGraphControl   : TG2FMXCable;
    FSelected       : boolean;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   InitCable;
    procedure   ConnectorMoved; override;
    procedure   SetParent( aValue : TFmxObject);

    property    GraphControl : TG2FMXCable read FGRaphControl write FGRaphControl;
    property    Parent : TFmxObject read FParent write SetParent;
  end;

procedure Register;

implementation
uses
  Math;

{ ==== TG2Graph ================================================================}

constructor TG2Graph.Create(AOwner: TComponent);
begin
  inherited;
  FClientType := ctEditor;
end;

function TG2Graph.CreatePerformance: TG2FilePerformance;
begin
  Result := TG2GraphPerformance.Create(self);
end;

destructor TG2Graph.Destroy;
begin
  inherited;
end;

function TG2Graph.GetSelectedPatch: TG2GraphPatch;
begin
  // Abstract
end;

procedure TG2Graph.SetClientType( aValue: TClientType);
begin
  FClientType := aValue;
end;

procedure TG2Graph.SetPatchAreaVA( aValue : TG2FMXPatchArea);
begin
  FLayoutVA := aValue;
end;

procedure TG2Graph.SetPatchAreaFX( aValue : TG2FMXPatchArea);
begin
  FLayoutFX := aValue;
end;

function TG2Graph.G2MessageDlg( tekst : string; code : integer): integer;
begin
  //
end;

procedure TG2Graph.G2ProcessWindowsMessages;
begin
  Application.ProcessMessages;
end;

// ==== TG2GraphSlot ===========================================================

constructor TG2GraphSlot.Create(AOwner: TComponent);
begin
  inherited;

end;

function TG2GraphSlot.CreatePatch: TG2FilePatch;
begin
  Result := TG2GraphPatch.Create( self);
end;

destructor TG2GraphSlot.Destroy;
begin
  inherited;
end;

// ==== TG2GraphPerformance ====================================================

constructor TG2GraphPerformance.Create(AOwner: TComponent);
begin
  inherited;
end;

function TG2GraphPerformance.CreateSlot: TG2FileSlot;
begin
  Result := TG2GraphSlot.Create( self);
end;

destructor TG2GraphPerformance.Destroy;
begin
  inherited;
end;

// ==== TG2GraphPatch ==========================================================

constructor TG2GraphPatch.Create(AOwner: TComponent);
begin
  FMiniVUList := TList.Create;
  FLedGroupList := TList.Create;
  FLed39List := TList.Create;
  FLed3AList := TList.Create;

  FSelectedMorphIndex := 0;

  inherited;
end;

destructor TG2GraphPatch.Destroy;
var i : integer;
begin
  FLed3AList.Free;
  FLed39List.Free;
  FLedGroupList.Free;
  FMiniVUList.Free;

  inherited;
end;

procedure TG2GraphPatch.Init;
var i : integer;
begin
  inherited;

  FSelectedControl := nil;

  FLed3AList.Clear;
  FLed39List.Clear;
  FLedGroupList.Clear;
  FMiniVUList.Clear;
end;

function TG2GraphPatch.CreateModule( aLocation : TLocationType; aModuleIndex : byte; aModuleType : byte): TG2FileModule;
var i : integer;
    MOdule : TG2GraphModule;
begin
  // Create a module in a patch file

  Result := nil;
  Module := TG2GraphModule.Create( PatchPart[ ord(aLocation)]);
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
          Module.Parent := (G2 as TG2Graph).FLayoutVA;//FG2.FForm; //.ScrollboxVA;
          Module.ParsePanelData;
        end else
          if aLocation = ltFX then begin
            Module.Parent := (G2 as TG2Graph).FLayoutFX;//.ScrollboxFX;
            Module.ParsePanelData;
          end;
      end else
        Module.Parent := nil;

    end else
      raise Exception.Create('Unknown module type ' + IntToStr( aModuleType));;
  end;

  Module.Visible := Visible;
  Module.Location := aLocation;

//  if assigned(FG2USB) and assigned((FG2USB as TG2).FOnCreateModule) then
//    (FG2USB as TG2).FOnCreateModule(self, Module);

  Result := Module;
end;

function TG2GraphPatch.CreateCable( aLocation : TLocationType; aColor : byte; aFromModule : byte; aFromConnector : byte; aLinkType : byte; aToModule : byte; aToConnector : byte): TG2FileCable;
var i : integer;
    FromConnKind : TConnectorKind;
    ModuleFrom, ModuleTo : TG2GraphModule;
    Cable : TG2GraphCable;
    ConnectorFrom, ConnectorTo : TG2FMXConnector;
begin
  // Create a cable connection in a patch file

  Result := nil;

  ModuleFrom := GetModule( ord(aLocation), aFromModule) as TG2GraphModule;
  if not assigned(ModuleFrom) then
    raise Exception.Create('ModuleIndex ' + IntToStr( aFromModule) + ' not found.');

  ModuleTo := GetModule( ord(aLocation), aToModule) as TG2GraphModule;
  if not assigned(ModuleTo) then
    raise Exception.Create('ModuleIndex ' + IntToStr( aToModule) + ' not found.');

  Cable               := TG2GraphCable.Create( self);
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
      Cable.Parent := (G2 as TG2Graph).FLayoutFX
    else
      Cable.Parent := (G2 as TG2Graph).FLayoutVA;

    // Cable needs scrollbar coords
    ConnectorFrom := Cable.FromConnector.GraphControl as TG2FMXConnector;
    ConnectorTo := Cable.ToConnector.GraphControl as TG2FMXConnector;

//    Cable.GraphControl.Point1X := trunc(ModuleFrom.ScrollPosX +  ConnectorFrom.Position.X + ConnectorFrom.Width / 2);
//    Cable.GraphControl.Point1Y := trunc(ModuleTo.ScrollPosY +  ConnectorFrom.Position.Y + ConnectorFrom.Height / 2);
//    Cable.GraphControl.Point2X := Cable.GraphControl.Point1X;
//    Cable.GraphControl.Point2Y := Cable.GraphControl.Point1Y;
//    Cable.GraphControl.InitCable;
    Cable.InitCable;
  end;

  Result := Cable;
end;

procedure TG2GraphPatch.SelectModulesInRect( aLocation : TLocationType; aRect : TRect);
var i, temp : integer;
begin
  if aRect.Left > aRect.Right then begin
    temp := aRect.Left;
    aRect.Left := aRect.Right;
    aRect.Right := temp;
  end;

  if aRect.Top > aRect.Bottom then begin
    temp := aRect.Top;
    aRect.Top := aRect.Bottom;
    aRect.Bottom := temp;
  end;

  UnselectModules( ltFX);
  UnselectModules( ltVA);
  for i := 0 to ModuleList[ ord(aLocation)].Count - 1 do begin
{    if PointInRect( (ModuleList[ ord(aLocation)][i] as TG2GraphModule).FPanel.Left,
                    (ModuleList[ ord(aLocation)][i] as TG2GraphModule).FPanel.Top,
                    aRect) then}
      ModuleList[ ord(aLocation)][i].Selected := True;
  end;
end;

procedure TG2GraphPatch.MoveOutlines( aLocation : TLocationType; dX, dY: single);
var i : integer;
    Module : TG2GraphModule;
begin
  {for i := 0 to PatchPart[ ord(aLocation)].SelectedModuleList.Count - 1 do
    (PatchPart[ ord(aLocation)].SelectedModuleList[i] as TG2GraphModule).FPanel.MoveOutline( dX, dY);}
  for i := 0 to PatchPart[ ord(aLocation)].SelectedModuleList.Count - 1 do begin
    Module := (PatchPart[ ord(aLocation)].SelectedModuleList[i] as TG2GraphModule);
    Module.FPanel.MoveOutline(dX, dY);
  end;

end;

function TG2GraphPatch.MessMoveModules( aLocation : TLocationType): boolean;
var i : integer;
begin
//  Result := inherited MessMoveModules( aLocation);
  for i := 0 to PatchPart[ ord(aLocation)].SelectedModuleList.Count - 1 do
    (PatchPart[ ord(aLocation)].SelectedModuleList[i] as TG2GraphModule).FPanel.MoveModule;
end;

function TG2GraphPatch.CreateParameter( aModuleIndex : byte): TG2FileParameter;
begin
  Result := TG2GraphParameter.Create( self, ltPatch, aModuleIndex, nil);
end;

function TG2GraphPatch.GetLedListCount: integer;
begin
  Result := FLed39List.Count;
end;

function TG2GraphPatch.GetMiniVUListCount: integer;
begin
  Result := FLed3AList.Count;
end;

function CompareLedGreenOrder( Led1 : pointer; Led2 : pointer): integer;
begin
{  if TG2GraphLedGreen(Led1).Module.Location > TG2GraphLedGreen(Led2).Module.Location then
    Result := -1
  else
    if TG2GraphLedGreen(Led1).Module.Location =  TG2GraphLedGreen(Led2).Module.Location then begin
      if TG2GraphLedGreen(Led1).Module.ModuleIndex > TG2GraphLedGreen(Led2).Module.ModuleIndex then
        Result := 1
      else
        if TG2GraphLedGreen(Led1).Module.ModuleIndex = TG2GraphLedGreen(Led2).Module.ModuleIndex then begin
          if TG2GraphLedGreen(Led1).FGroupId > TG2GraphLedGreen(Led2).FGroupID then
            Result := 1
          else
            if TG2GraphLedGreen(Led1).FGroupId = TG2GraphLedGreen(Led2).FGroupID then begin
              if TG2GraphLedGreen(Led1).FCodeRef > TG2GraphLedGreen(Led2).FCodeRef then
                Result := 1
              else
                if TG2GraphLedGreen(Led1).FCodeRef = TG2GraphLedGreen(Led2).FCodeRef then
                  Result := 0
                else
                  Result := -1;
            end else
              Result := -1;
        end else
          Result := -1;
    end else
      Result := 1;}
end;

function CompareMiniVUOrder( Led1, Led2: pointer): integer;
begin
{  if TG2GraphLed(Led1).Module.Location > TG2GraphLed(Led2).Module.Location then
    Result := -1
  else
    if TG2GraphLed(Led1).Module.Location = TG2GraphLed(Led2).Module.Location then begin
      if TG2GraphLed(Led1).Module.ModuleIndex > TG2GraphLed(Led2).Module.ModuleIndex then
        Result := 1
      else
        if TG2GraphLed(Led1).Module.ModuleIndex = TG2GraphLed(Led2).Module.ModuleIndex then begin
          if TG2GraphLed(Led1).FGroupId > TG2GraphLed(Led2).FGroupID then
            Result := 1
          else
            if TG2GraphLed(Led1).FGroupId = TG2GraphLed(Led2).FGroupID then
              Result := 0
            else
              Result := -1;
        end else
          Result := -1;
    end else
      Result := 1;}
end;

procedure TG2GraphPatch.SetLedLevel( Index: integer; aValue: byte);
begin
//  TG2GraphLedGreen(FLed39List[Index]).SetLevel( aValue)
end;

procedure TG2GraphPatch.SetMiniVULevel( Index: integer; aValue: byte);
begin
//  TG2GraphLed(FLed3AList[Index]).SetLevel( aValue)
end;

procedure TG2GraphPatch.SetSelectedControl( aValue: TG2FMXControl);
begin
  if assigned(FSelectedControl) then
    FSelectedControl.DeSelect;

  FSelectedControl := aValue;
  if assigned(FSelectedControl) then
    FSelectedControl.Select;
end;

procedure TG2GraphPatch.SetSelectedMorphIndex( aValue: integer);

  procedure SetControlMorphUpdate;
  var m, l, p, Count : integer;
      Module : TG2FileModule;
      Param : TG2FileParameter;
  begin
    for l := 0 to 1 do begin
      Count := ModuleCount[ l];
      for m := 0 to Count - 1 do begin
        Module := Modules[ l, m];
        if assigned(Module) then begin
          for p := 0 to Module.ParameterCount - 1 do begin
            Param := Module.Parameter[p];
            if assigned(Param) then begin
               if Param.HasMorph then
                 Param.InvalidateControl;
            end;
          end;
        end;
      end;
    end;
  end;

begin
  if aValue <> FSelectedMorphIndex then begin
    FSelectedMorphIndex := aValue;
    //SetControlMorphUpdate;
  end;
end;

procedure TG2GraphPatch.SetVisible( aValue: boolean);
var i, j : integer;
begin
  if aValue <> FVisible then begin
    for i := 0 to 1 do
      for j := 0 to ModuleCount[i] - 1 do
        (ModuleList[i].Items[j] as TG2GraphModule).FPanel.Visible := aValue;
    FVisible := aValue;
  end;
end;

procedure TG2GraphPatch.SortLeds;
var i : integer;
begin
{  // Take leds that are in a led goup with only one led out of the group list
  // and put them in de led list.
  // These leds are addressed in message $39
  // VU-meters and ledgroups with more than 1? led are addressed in message $3A

  FLed39List.Clear;
  FLed3AList.Clear;

  i := 0;
  while i < FLedGroupList.Count do begin

    if TG2GraphLedGroup( FLedGroupList[i]).FLeds.Count = 1 then begin
      FLed39List.Add( TG2GraphLedGroup( FLedGroupList[i]).FLeds[0])
    end else
      FLed3AList.Add( FLedGroupList[i]);

    inc(i);
  end;

  for i := 0 to FMiniVUList.Count - 1 do
    FLed3AList.Add(FMiniVUList[i]);

  FLed3AList.Sort( CompareMiniVUOrder);
  FLed39List.Sort( CompareLedGreenOrder);}
end;

procedure TG2GraphPatch.RemoveFromLedList( aLocation: TLocationType; aModuleIndex : integer);
var i : integer;
begin
{  // When a module is deleted, it's leds must be removed from the lists

  i := 0;
  while (i < FMiniVUList.Count) do begin
    if (TG2GraphLed(FMiniVUList.Items[i]).FModule.ModuleIndex = aModuleIndex) and
       (TG2GraphLed(FMiniVUList.Items[i]).FModule.Location = aLocation) then
      FMiniVUList.Delete(i)
    else
      inc(i);
  end;

  i := 0;
  while (i < FLedGroupList.Count) do begin
    if (TG2GraphLed(FLedGroupList.Items[i]).FModule.ModuleIndex = aModuleIndex) and
       (TG2GraphLed(FLedGroupList.Items[i]).FModule.Location = aLocation) then
      FLedGroupList.Delete(i)
    else
      inc(i);
  end;

  SortLeds;}
end;

// ==== TG2FMXPatchArea ========================================================

constructor TG2FMXPatchArea.Create( AOwner: TComponent);
begin
  inherited;
  FNewCable := nil;
end;

destructor TG2FMXPatchArea.Destroy;
begin
  inherited;
end;

procedure TG2FMXPatchArea.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
end;

procedure TG2FMXPatchArea.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if assigned(FNewCable) then begin
     FreeAndNil(FNewCable);
  end;
  inherited;
end;

procedure TG2FMXPatchArea.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if (ssLeft in Shift) and assigned(FNewCable) then begin
    FNewCable.Point2X := X;
    FNewCable.Point2Y := Y;
    FNewCable.StartTimer;
  end;
end;

// ==== G2GraphModule ==========================================================

constructor TG2GraphModule.Create( aPatchPart : TG2FilePatchPart);
begin
  inherited Create( aPatchPart);
  FPanel := TG2FMXModule.Create( aPatchPart);
  FPanel.FData := self;
  FFreePanel := True;
  FOutlineRect := FPanel.BoundsRect;
  FOutlineVisible := False;
end;

constructor TG2GraphModule.CopyCreate( aPatchPart : TG2FilePatchPart; aModule : TG2GraphModule);
begin
  inherited Create( aPatchPart);
  Copy( aModule);
  FFreePanel := False;
  FPanel := aModule.FPanel;
  FOutlineRect := FPanel.BoundsRect;
  FOutlineVisible := False;
end;

destructor TG2GraphModule.Destroy;
begin
  if FFreePanel then
    FPanel.Free;
  inherited;
end;

function TG2GraphModule.CreateCopy( aPatchPart : TG2FilePatchPart) : TG2FileModule;
begin
  Result := TG2GraphModule.CopyCreate( aPatchPart, self);
end;


function TG2GraphModule.CreateParameter: TG2FileParameter;
begin
  Result := TG2GraphParameter.Create( PatchPart.Patch, TLocationType(Location), ModuleIndex, self);
end;

function TG2GraphModule.GetNewCol: TBits7;
begin
  if assigned(FPanel) then
    Result := FPanel.GetNewCol
  else
    Result := 0;
end;

function TG2GraphModule.GetNewRow: TBits7;
begin
  if assigned(FPanel) then
    Result := FPanel.GetNewRow
  else
    Result := 0;
end;

function TG2GraphModule.GetOnConnectorClick: TConnectorClickEvent;
begin
  if assigned(FPanel) then
    Result := FPanel.OnConnectorClick
  else
    Result := nil;
end;

function TG2GraphModule.GetOnModuleClick: TModuleClickEvent;
begin
  if assigned(FPanel) then
    Result := FPanel.OnModuleClick
  else
    Result := nil;
end;

function TG2GraphModule.GetOnParameterClick: TParameterClickEvent;
begin
  if assigned(FPanel) then
    Result := FPanel.OnParameterClick
  else
    Result := nil;
end;

function TG2GraphModule.GetParent : TFmxObject;
begin
  if assigned(FPanel) then
    Result := FPanel.Parent
  else
    Result := nil;
end;

function TG2GraphModule.GetScrollPosX: single;
begin
  if assigned(FPanel) then
    Result := FPanel.ScrollPosX
  else
    Result := 0;
end;

function TG2GraphModule.GetScrollPosY: single;
begin
  if assigned(FPanel) then
    Result := FPanel.ScrollPosY
  else
    Result := 0;
end;

function TG2GraphModule.GetVisible: boolean;
begin
  if assigned(FPanel) then
    Result := FPanel.Visible
  else
    Result := False;
end;

procedure TG2GraphModule.ParsePanelData;
begin
  if assigned(FPanel) then
    FPanel.ParsePanelData;
end;

procedure TG2GraphModule.SetCol(aValue: TBits7);
begin
  if assigned(FPanel) and (FFreePanel = True) then
    FPanel.SetCol( aValue);
  inherited;
  InvalidateCables;
end;

procedure TG2GraphModule.SetModuleColor( aValue: TBits8);
begin
  inherited;
  if assigned(FPanel) then
    FPanel.Fill.Color := ModuleColors[ aValue] + $FF000000;
end;

procedure TG2GraphModule.SetModuleName(aValue: AnsiString);
begin
  inherited;
  if assigned(FPanel) then
    FPanel.FModuleLabel.Text := aValue;
end;

procedure TG2GraphModule.SetRow(aValue: TBits7);
begin
  if assigned(FPanel) and (FFreePanel = True) then
    FPanel.SetRow( aValue);
  inherited;
  InvalidateCables;
end;

procedure TG2GraphModule.SetOnConnectorClick(aValue: TConnectorClickEvent);
begin
  if assigned(FPanel) then
    FPanel.OnConnectorClick := aValue;
end;

procedure TG2GraphModule.SetOnModuleClick(aValue: TModuleClickEvent);
begin
  if assigned(FPanel) then
    FPanel.OnModuleClick := aValue;
end;

procedure TG2GraphModule.SetOnParameterClick(aValue: TParameterClickEvent);
begin
  if assigned(FPanel) then
    FPanel.OnParameterClick := aValue;
end;

procedure TG2GraphModule.SetParent( aValue : TFmxObject);
begin
  if assigned(FPanel) then
    FPanel.Parent := aValue;
end;

procedure TG2GraphModule.SetSelected( aValue: boolean);
begin
  inherited;
  FPanel.SetSelected( aValue);
end;

procedure TG2GraphModule.SetVisible(aValue: boolean);
begin
  if assigned(FPanel) then
    FPanel.Visible := aValue;
end;

function TG2GraphModule.ClientToScreen(p: TPoint): TPoint;
begin
{  if assigned(FPanel) then
    Result := FPanel.ClientToScreen(p)
  else
    Result := Point(0,0);}
end;


// ==== TG2FMXModule =================================================

constructor TG2FMXModule.Create(AOwner: TComponent);
begin
  inherited Create( AOWner);

  Opacity := 1;
  //CanClip := True;

  FChildControls := TList.Create;

  FOldX := ScrollPosX;
  FOldY := ScrollPosY;

  FWasAlreadySelected := False;

  FModuleLabel := TLabel.Create(self);
  FModuleLabel.Parent := self;
  FMOduleLabel.HitTest := False;
  FModuleLabel.Font.Family := 'Arial';
  FModuleLabel.Font.Size := 10;
  //FModuleLabel.Font.Style := fsBold;
  //FModuleLabel.Font.Color := $00000000;
  FModuleLabel.Position.X := 2;
  FModuleLabel.Position.Y := 2;
  FMOduleLabel.Text := 'Module';

  HitTest := True;


  Width := UNITS_COL;
  Height := UNITS_ROW;
end;

destructor TG2FMXModule.Destroy;
begin

  inherited;
end;

function TG2FMXModule.GetScrollBarX: single;
begin
  if assigned(Parent) and (Parent is TScrollbox) then
    Result := (Parent as TScrollbox).hscrollBar.Value
  else
    Result := 0;
end;

function TG2FMXModule.GetScrollBarY: single;
begin
  if assigned(Parent) and (Parent is TScrollbox) then
    Result := (Parent as TScrollbox).vscrollBar.Value
  else
    Result := 0;
end;

function TG2FMXModule.GetScrollPosX: single;
begin
  if assigned(Parent) and (Parent is TScrollbox) then
    Result := Position.X + (Parent as TScrollbox).hscrollBar.Value
  else
    Result := Position.X;
end;

function TG2FMXModule.GetScrollPosY: single;
begin
  if assigned(Parent) and (Parent is TScrollbox) then
    Result := Position.Y + (Parent as TScrollbox).vscrollBar.Value
  else
    Result := Position.Y;
end;

function TG2FMXModule.GetSelected: boolean;
begin
  Result := FData.Selected;
end;

procedure TG2FMXModule.SetSelected(aValue: boolean);
begin
  FOldX := Left;
  FOldY := Top;

  FData.FOutlineRect := BoundsRect;
end;

procedure TG2FMXModule.SetCol(aValue: TBits7);
begin
  if aValue <> FData.Col then begin
    Position.X := aValue * UNITS_COL - ScrollBarX;
  end;
end;

procedure TG2FMXModule.SetRow(aValue: TBits7);
begin
  if aValue <> FData.Row then begin
    Position.Y := aValue * UNITS_ROW - ScrollbarY;
  end;
end;

procedure TG2FMXModule.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var P : TPoint;
    i : integer;
    Patch : TG2GraphPatch;
begin
  {if assigned( FDropDownList) then begin
    FDropDownList.Free;
    FDropDownList := nil;
  end;}

  Patch := GetPatch;

    case Location of
      ltVA : Patch.UnselectModules(ltFX);
      ltFX : Patch.UnselectModules(ltVA);
    end;

    if not FData.Selected then begin
      if not(ssCtrl in Shift) then
        Patch.UnSelectModules( Location);
      FData.Selected := True;
      FWasAlreadySelected := False;
    end else
      FWasAlreadySelected := True;

    if Location <> Patch.SelectedLocation then
      Patch.SelectedLocation := Location;

    if ssLeft in Shift then begin
      FStartX := X;
      FStartY := Y;
      //Patch.SelectModules;
    end;

  inherited;
end;

procedure TG2FMXModule.MouseMove(Shift: TShiftState; X, Y: Single);
var PatchArea : TG2FMXPatchArea;
    Patch : TG2GraphPatch;
    i : integer;
begin
  if assigned(Parent) then
    PatchArea := Parent as TG2FMXPatchArea;

  Patch := GetPatch;

  if assigned(PatchArea) and assigned(PatchArea.FNewCable) and (ssLeft in Shift) then
    PatchArea.MouseMove(Shift, Position.X + X, Position.Y + Y)
  else begin
    if (ssLeft in Shift) then
      if FWasAlreadySelected then
        Patch.MoveOutlines( Data.Location, X - FStartX, Y - FStartY);

    inherited;
  end;
end;

procedure TG2FMXModule.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var NewCol, NewRow : byte;
    PatchArea : TG2FMXPatchArea;
begin
  if assigned(Parent) then
    PatchArea := Parent as TG2FMXPatchArea;

  if assigned(PatchArea) and assigned(PatchArea.FNewCable) then
    PatchArea.MouseUp(Button, Shift, Position.X + X, Position.Y + Y);

  if assigned( FOnModuleClick) then
    FOnModuleClick( self, Button, Shift, trunc(X), trunc(Y), FData);

  inherited;
end;

procedure TG2FMXModule.SelectModule;
begin
  FOldX := Position.X;
  FOldY := Position.Y;

  FData.FOutlineRect := BoundsRect;
end;

procedure TG2FMXModule.MoveOutline(dX, dY: single);
var i : integer;
    Module : TG2GraphModule;
begin
{  if assigned( Parent) and FData.FOutlineVisible then
    (Parent as TG2GraphScrollbox).DrawOutline(FData.FOutlineRect);

  FData.FOutlineRect.Left := FOldX + dX;
  FData.FOutlineRect.Top := FOldY + dY;
  FData.FOutlineRect.Right := FData.FOutlineRect.Left + Width;
  FData.FOutlineRect.Bottom := FData.FOutlineRect.Top + Height;

  if assigned( Parent) then begin
    (Parent as TG2GraphScrollbox).DrawOutline(FData.FOutlineRect);
    FData.FOutlineVisible := True;
  end;}
  Position.X := Position.X + dX;
  Position.Y := Position.Y + dY;
  for i := 0 to FChildControls.Count - 1 do begin
    if TG2FMXControl(FChildControls[i]) is TG2FMXConnector then begin
      (TG2FMXControl(FChildControls[i]) as TG2FMXConnector).FData.InvalidateCables;
    end;

  end;

end;

procedure TG2FMXModule.MoveModule;
begin
  // TODO
{  if FData.FOutlineVisible then
    if assigned( Parent) then begin
      (Parent as TG2GraphScrollbox).DrawOutline(FData.FOutlineRect);
      FData.FOutlineVisible := False;
    end;}
end;

function TG2FMXModule.GetNewCol: TBits7;
begin
  Result := trunc((ScrollPosX + FData.FOutlineRect.Left - FOldX) / UNITS_COL);
end;

function TG2FMXModule.GetNewRow: TBits7;
begin
  Result := trunc((ScrollPosY + FData.FOutlineRect.Top - FOldY) / UNITS_ROW);
end;

function TG2FMXModule.GetPatch: TG2GraphPatch;
begin
  if not assigned(FData.PatchPart.Patch) then
    raise Exception.Create('Patch not assigned to module.');

  Result := FData.PatchPart.Patch as TG2GraphPatch;
end;

procedure TG2FMXModule.ParsePanelData;
var //MemStream : TMemoryStream;
    ModuleStream : TModuleDefStream;
    CodeRef, Err : integer;
    aPath : string;
    aName, aValue, ControlType, CodeRefStr : AnsiString;
    ChildControl : TG2FMXControl;
    Connector : TG2FMXConnector;
    Param : TG2FileParameter;
    Patch : TG2GraphPatch;
begin
  aPath := ExtractFilePath(ParamStr(0));
  //aPath := GetCurrentDir;
{$IFDEF FPC}
  aPath := aPath + 'Modules/';
{$ELSE}
  aPath := aPath + '\Modules\';
{$ENDIF}

  Patch := GetPatch;

  if FileExists( aPath + string(FData.ModuleFileName) + '.txt') then begin
    //MemStream := TMemoryStream.Create;
    //MemStream.LoadFromFile( aPath + FData.ModuleName + '.txt');
    ModuleStream := TModuleDefStream.Create(aPath + string(FData.ModuleFileName) + '.txt');
    try
      if ModuleStream.ReadConst('<#Module') then begin
        aName := 'Module';
        while (ModuleStream.Position < ModuleStream.Size) and (aName[1] <> '<') do begin
          ModuleStream.ReadSpaces;
          aName := ModuleStream.ReadUntil( [':', #13]);
          if aName[1] <> '<' then begin
            aValue := ModuleStream.ReadUntil( [#13]);

            if aName = 'Height' then
              Height := UNITS_ROW * StrToInt(string(aValue));
          end;
        end;

        while aName[1] = '<' do begin
          ControlType := copy(aName, 3, Length(aName) - 2);
          if ControlType = 'Input' then begin

            CodeRefStr := ModuleStream.PeekValue( 'CodeRef:', [#13, #10, '#'], ['#', '>']);
            val( string(CodeRefStr), CodeRef, Err);
            if Err = 0 then begin
              Connector := TG2FMXConnector.Create(self);
              Connector.Module := self;
              Connector.Data := FData.InConnector[ CodeRef];
              if Connector.Data = nil then
                raise Exception.Create('Data for connector not found...');

              Connector.ParsePanelData( ModuleStream);
              AddGraphControl( Connector);
            end else
              raise Exception.Create('Parse error, module  ' + string(FData.ModuleName) + ' input connector CodeRef not found.' );

          end else
            if ControlType = 'Output' then begin

              CodeRefStr := ModuleStream.PeekValue( 'CodeRef:', [#13, #10, '#'], ['#', '>']);
              val( string(CodeRefStr), CodeRef, Err);
              if Err = 0 then begin
                Connector := TG2FMXConnector.Create(self);
                Connector.Module := self;
                Connector.Data := FData.OutConnector[ CodeRef];
                if Connector.Data = nil then
                  raise Exception.Create('Data for connector not found...');

                Connector.ParsePanelData( ModuleStream);
                AddGraphControl( Connector);
              end else
                raise Exception.Create('Parse error, module  ' + string(FData.ModuleName) + ' output connector CodeRef not found.' );

            end else begin
              ChildControl := NewG2GraphControl(ControlType);
              if ChildControl <> nil then begin

                AddGraphControl( ChildControl);

                if ( ControlType = 'Knob') or
                   ( ControlType = 'ButtonIncDec') or
                   ( ControlType = 'ButtonRadio') or
                   ( ControlType = 'LevelShift') or
                   ( ControlType = 'ButtonFlat') or
                   ( ControlType = 'TextEdit') or
                   ( ControlType = 'PartSelector') or
                   ( ControlType = 'ButtonText') then begin

                  CodeRefStr := ModuleStream.PeekValue( 'CodeRef:', [#13, #10, '#'], ['#', '>']);
                  val( string(CodeRefStr), CodeRef, Err);
                  if Err = 0 then begin
                    if ControlType = 'PartSelector' then begin
                      Param := FData.Mode[ CodeRef];
                    end else begin
                      Param := FData.Parameter[ CodeRef];
                    end;
                    ChildControl.ParsePanelData( ModuleStream);
                    ChildControl.Parameter := Param;
                  end else
                    raise Exception.Create('Parse error, module  ' + string(FData.ModuleName) + ' parameter CodeRef not found.' );

                end else
                  // No parameter associated
                  ChildControl.ParsePanelData( ModuleStream);

              end else begin
                while ( ModuleStream.Position < ModuleStream.Size) and (aName <> '#>') do begin
                  ModuleStream.ReadSpaces;
                  aName := ModuleStream.ReadUntil( [':', #13]);
                  if aName <> '#>' then begin
                    aValue := ModuleStream.ReadUntil( [#13]);
                  end;
                end;
              end;
            end;
          ModuleStream.ReadSpaces;
          aName := ModuleStream.ReadUntil( [':', #13]);
        end;

//        FChildControls.Sort(@CompareZOrder);
      end else
        raise Exception.Create('Unknown file type.');

      Patch.SortLeds;
    finally
      ModuleStream.Free;
    end;
  end;
end;


function TG2FMXModule.GetControlType( aG2GraphChildControl: TG2FMXControl): string;
begin
  if aG2GraphChildControl is TG2FMXControl then
    Result := '';

  if aG2GraphChildControl is TG2FMXConnector then
    Result := 'Connector';

{  if aG2GraphChildControl is TG2GraphLabel then
    Result := 'Label';

  if aG2GraphChildControl is TG2GraphDisplay then
    Result := 'Display';

  if aG2GraphChildControl is TG2GraphGraph then
    Result := 'Graph';

  if aG2GraphChildControl is TG2GraphLedGreen then
    Result := 'Led';

  if aG2GraphChildControl is TG2GraphMiniVU then
    Result := 'MiniVU';

  if aG2GraphChildControl is TG2GraphPartSelector then
    Result := 'PartSelector';

  if aG2GraphChildControl is TG2GraphButtonText then
    Result := 'ButtonText';

  if aG2GraphChildControl is TG2GraphButtonFlat then
    Result := 'ButtonFlat';

  if aG2GraphChildControl is  TG2GraphKnob then
    Result := 'Knob';}

end;

function TG2FMXModule.NewG2GraphControl( aControlType: string): TG2FMXControl;
begin
  Result := nil;

  if (aControlType = 'Line') then begin
    //Result := TG2GraphLine.Create(self);
    Result := TG2FMXControl.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'Text') then begin
    //Result := TG2GraphLabel.Create(self);
    Result := TG2FMXControl.Create(self);
    Result.Module := self;
  end;

  if aControlType = 'Connector' then begin
    Result := TG2FMXConnector.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'TextField') then begin
    //Result := TG2GraphDisplay.Create(self);
    Result := TG2FMXControl.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'Graph') then begin
    //Result := TG2GraphGraph.Create(self);
    Result := TG2FMXControl.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'Led') then begin
    //Result := TG2GraphLedGreen.Create(self);
    Result := TG2FMXControl.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'MiniVU') then begin
    //Result := TG2GraphMiniVU.Create(self);
    Result := TG2FMXControl.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'PartSelector') then begin
    //Result := TG2GraphPartSelector.Create(self);
    Result := TG2FMXControl.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'ButtonText') then begin
    //Result := TG2GraphButtonText.Create(self);
    Result := TG2FMXControl.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'TextEdit') then begin
    //Result := TG2GraphButtonText.Create(self);
    Result := TG2FMXControl.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'ButtonFlat') then begin
    //Result := TG2GraphButtonFlat.Create(self);
    Result := TG2FMXControl.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'LevelShift') then begin
    //Result := TG2GraphButtonFlat.Create(self);
    Result := TG2FMXControl.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'ButtonRadio') then begin
    //Result := TG2GraphButtonRadio.Create(self);
    Result := TG2FMXControl.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'ButtonIncDec') then begin
    //Result := TG2GraphButtonIncDec.Create(self);
    Result := TG2FMXControl.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'Knob') or (aControlType = '') then begin
    //Result := TG2GraphKnob.Create(self);
    Result := TG2FMXControl.Create(self);
    Result.Module := self;
  end;

  //if not assigned( Result)  then
  //  raise Exception.Create('Unknown control type ' + aControlType);
end;


function TG2FMXModule.GetChildControlsCount: integer;
begin
  Result := FChildControls.Count;
end;

function TG2FMXModule.GetGraphChildControl( ChildControlIndex: integer): TG2FMXControl;
begin
  Result := TG2FMXControl( FChildControls[ChildControlIndex]);
end;

function TG2FMXModule.GetColor: TColor;
begin
  Result := FData.Color;
end;

function TG2FMXModule.GetLocation: TLocationType;
begin
  Result := FData.Location;
end;

function TG2FMXModule.GetModuleIndex: TBits8;
begin
  Result := FData.ModuleIndex;
end;

procedure TG2FMXModule.AddGraphControl(aGraphCtrl: TG2FMXControl);
begin
  FChildControls.Add( aGraphCtrl);
end;

// ==== TG2FMXControl ===================================================

constructor TG2FMXControl.Create( AOwner: TComponent);
begin
  inherited;

  FModule := nil;
  FParameter := nil;

  FMouseInput := False;
  FZOrder := 1;
  FSelected := False;

  FValue := 0;
  FLowValue := 0;
  FHighValue := 0;

  HitTest := False;
end;

destructor TG2FMXControl.Destroy;
begin
  SetParameter(nil);
  inherited;
end;

{function TG2FMXControl.GetRelToParentRect : TRect;
begin
  if assigned( FModule) then
    Result := AddRect( BoundsRect, FModule.BoundsRect)
  else
    Result := BoundsRect;
end;}

{function TG2FMXControl.GetScreenCoordsRect: TRect;
begin
  if assigned(FModule) then begin
    Result.Left := FModule.ClientToScreen(Point(Left, Top)).X;
    Result.Right := FModule.ClientToScreen(Point(Left + Width, Top + Height)).X;
    Result.Top := FModule.ClientToScreen(Point(Left, Top)).Y;
    Result.Bottom := FModule.ClientToScreen(Point(Left + Width, Top + Height)).Y;
  end else begin
    Result.Left := Left;
    Result.Right := Left + Width;
    Result.Top := Top;
    Result.Bottom := Top + Height;
  end;
end;}

procedure TG2FMXControl.SetModule( aValue: TG2FMXModule);
begin
  FModule := aValue;
  Parent := FModule;
end;

procedure TG2FMXControl.SetParameter( aParam : TG2FileParameter);
var Param : TG2GraphParameter;
begin
  if assigned(aParam) and not( aParam is TG2FileParameter) then
    raise Exception.Create('Parameter must be of type TG2GraphParameter.');

  Param := aParam as TG2GraphParameter;

  if assigned(FParameter) then begin
    (FParameter as TG2GraphParameter).DeassignControl(self);
    if assigned( Param) then begin
      Param.AssignControl( self);
    end;
    FParameter := Param;
  end else begin
    FParameter := Param;
    if assigned(FParameter) then
      (FParameter as TG2GraphParameter).AssignControl( self);
  end;
end;

function TG2FMXControl.GetGraphModule: TG2FMXModule;
begin
  Result := FModule;
end;

function TG2FMXControl.GetHighValue: byte;
begin
  if assigned( FParameter) then
    Result := FParameter.HighValue
  else
    Result := FHighValue;
end;

function TG2FMXControl.GetLowValue: byte;
begin
  if assigned( FParameter) then
    Result := FParameter.LowValue
  else
    Result := FLowValue;
end;

function TG2FMXControl.HasMorph: boolean;
begin
  if assigned( FParameter) then
    Result := FParameter.HasMorph
  else
    Result := False;
end;

function TG2FMXControl.GetMorph: TMorphParameter;
begin
  if assigned( FParameter) then
    Result := FParameter.GetSelectedMorph
  else
    Result := nil;
end;

function TG2FMXControl.GetMorphValue: byte;
begin
  if assigned( FParameter) then
    Result := FParameter.GetSelectedMorphValue
  else
    Result := 0;
end;

function TG2FMXControl.GetParamIndex: integer;
begin
  if assigned( FParameter) then
    Result := FParameter.ParamIndex
  else
    Result := -1;
end;

function TG2FMXControl.GetParamLabel: string;
begin
  if assigned( FParameter) then
    Result := FParameter.ParamLabel[0]
  else
    Result := '';
end;

function TG2FMXControl.GetValue: byte;
begin
  if assigned( FParameter) then
    Result := FParameter.GetParameterValue
  else
    Result := FValue;
end;

procedure TG2FMXControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: single);
begin
  if ssLeft in Shift then begin
    FStartValue := Value;
    //Select;
  end;
  inherited;
end;

procedure TG2FMXControl.MouseMove(Shift: TShiftState; X, Y: single);
begin
  inherited;
end;

procedure TG2FMXControl.MouseUp(Button: TMouseButton; Shift: TShiftState;  X, Y: single);
begin
  inherited;
end;

procedure TG2FMXControl.ParsePanelData(fs: TModuleDefStream);
var aName, aValue : string;
begin
  while (fs.Position < fs.Size) and (aName <> '#>') do begin
    fs.ReadSpaces;
    aName := fs.ReadUntil([':', #13]);
    if aName <> '#>' then begin
      //aValue := ReadUntil(fs, [#13]);
      if not ParseProperties( fs, aName) then begin
        // Unknown property
        aValue := fs.ReadUntil([#13]);
      end;
    end;
  end;
end;

function TG2FMXControl.ParseProperties( fs: TModuleDefStream; aName : string): boolean;
var aValue : string;
begin
  Result := True;

  if aName = 'XPos' then begin
    aValue := fs.ReadUntil([#13]);
    //Left := FModule.Left + StrToInt( aValue);
    Position.X := StrToInt( aValue);
  end else

  if aName = 'YPos' then begin
    aValue := fs.ReadUntil([#13]);
    //Top  := FModule.Top + StrToInt( aValue);
    Position.Y := StrToInt( aValue);
  end else

  if aName = 'Width' then begin
    aValue := fs.ReadUntil([#13]);
    if aValue[1] = '"' then begin
      fs.Position := fs.Position - Length(aValue) - 1;
      Result := False;
    end else
      Width := StrToInt( aValue);
  end else

  if aName = 'Height' then begin
    aValue := fs.ReadUntil([#13]);
    Height := StrToInt( aValue);
  end else

  if aName = 'ZPos' then begin
    aValue := fs.ReadUntil([#13]);
    FZOrder := StrToInt( aValue);
  end else

    Result := False
end;

function TG2FMXControl.CheckValueBounds( aValue: integer): byte;
begin
  if assigned( FParameter) then begin
    if aValue > FParameter.HighValue then
      Result := FParameter.HighValue
    else
      if aValue < FParameter.LowValue then
        Result := FParameter.LowValue
      else
        Result := aValue;
  end else begin
    if aValue > FHighValue then
      Result := FHighValue
    else
      if aValue < FLowValue then
        Result := FLowValue
      else
        Result := aValue;
  end;
end;

procedure TG2FMXControl.SetParamLabel(aValue: string);
begin
  if assigned( FParameter) then
    FParameter.ParamLabel[0] := aValue
  else begin
  end;
end;


procedure TG2FMXControl.SetValue( aValue: byte);
begin
  if assigned( FParameter) then
    FParameter.SetParameterValue( aValue)
  else begin
    FValue := aValue;

{    if assigned( FOnChange) then
      FOnChange( self);}
  end;
end;

procedure TG2FMXControl.SetLowValue( aValue: byte);
begin
  if assigned( FParameter) then
    FParameter.LowValue := aValue
  else begin
    FLowValue := aValue;
  end;
end;

procedure TG2FMXControl.SetHighValue( aValue: byte);
begin
  if assigned( FParameter) then
    FParameter.HighValue := aValue
  else begin
    FHighValue := aValue;
  end;
end;

procedure TG2FMXControl.InitValue( aValue: integer);
var Rect : TRect;
begin
  if (aValue >= FLowValue) and (aValue <= FHighValue) and ( aValue <> FValue) then begin
    FValue := aValue;
    //Rect := BoundsRect;
    //InvalidateRect( Parent.Handle, @Rect, True);
  end;
end;

procedure TG2FMXControl.SetMorphValue( aValue: byte);
var MorphParameter : TMorphParameter;
    TempValue : integer;
begin
  if assigned( FParameter) then begin
    FParameter.SetSelectedMorphValue( aValue);
  end;
end;

procedure TG2FMXControl.Select;
begin
  FSelected := True;
end;

procedure TG2FMXControl.Deselect;
begin
  FSelected := False;
end;

// ==== TG2GraphParameter ======================================================

constructor TG2GraphParameter.Create( aPatch : TG2FilePatch; aLocation : TLocationType; aModuleIndex : integer; aModule : TG2FileModule);
begin
  inherited Create( aPatch, aLocation, aModuleIndex, aModule);

  SetLength(FControlList, 0);
end;

destructor TG2GraphParameter.Destroy;
var i : integer;
begin
  for i := 0 to Length(FControlList) - 1 do
    FControlList[i].FParameter := nil;
  Finalize( FControlList);
  inherited;
end;

procedure TG2GraphParameter.AssignControl(aControl: TG2FMXControl);
var i : integer;
begin
  if not(aControl is TG2FMXControl) then
    raise Exception.Create('Only a TG2GraphControlFMX can be assigned to a parameter.');

  i := 0;
  while (i < Length(FControlList)) and not(FControlList[i] = aControl) do
    inc(i);

  if not(i < Length(FControlList)) then begin
    SetLength(FControlList, i + 1);
    FControlList[i] := aControl as TG2FMXControl;
    FControlList[i].FParameter := self;
  end;
end;

procedure TG2GraphParameter.DeassignControl(aControl: TG2FMXControl);
var i, j : integer;
begin
  if not(aControl is TG2FMXControl) then
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

// ==== TG2FMXConnector ========================================================

constructor TG2FMXConnector.Create(AOwner: TComponent);
begin
  inherited;
  FFillOuter := TBrush.Create(TBrushKind.bkGradient, $FFBABABA);
  FFillOuter.Gradient.Color := $FFBABABA;
  FFillOuter.Gradient.Color1 := $FFFF0000;
  //FFillOuter.Gradient.Style := FMX.Types.TGradientStyle.gsRadial;
  FFillInner := TBrush.Create(TBrushKind.bkSolid, $FF550000);

  HitTest := True;

  Width := 13;
  Height := 13;
end;

destructor TG2FMXConnector.Destroy;
begin
  FFillInner.Free;
  FFillOuter.Free;
  inherited;
end;

procedure TG2FMXConnector.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var PatchArea : TG2FMXPatchArea;
    Module : TG2FMXModule;
begin
  if (ssLeft in Shift) and assigned(FParent) then begin
    Module := FParent as TG2FMXModule;
    if assigned(Module.Parent) then begin
      PatchArea := Module.Parent as TG2FMXPatchArea;

      PatchArea.FFromConnector := self;

      PatchArea.FNewCable := TG2FMXCable.Create(PatchArea);
      PatchArea.FNewCable.Parent := PatchArea;

      PatchArea.FNewCable.Point1X := Module.Position.X + Position.X + Width / 2;
      PatchArea.FNewCable.Point1Y := Module.Position.Y + Position.Y + Width / 2;
      PatchArea.FNewCable.Point2X := PatchArea.FNewCable.Point1X;
      PatchArea.FNewCable.Point2Y := PatchArea.FNewCable.Point1Y;
      PatchArea.FNewCable.InitCable;
    end;
  end;
  inherited;
end;

procedure TG2FMXConnector.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var PatchArea : TG2FMXPatchArea;
    Module : TG2FMXModule;
begin
  if assigned(FParent) then begin
    Module := FParent as TG2FMXModule;
    if assigned(Module.Parent) then begin
      PatchArea := Module.Parent as TG2FMXPatchArea;

      if PatchArea.FFromConnector = self then begin
        FreeAndNil(PatchArea.FNewCable)
      end else begin
        FModule.FData.PatchPart.Patch.MessAddConnection( FModule.Location, PatchArea.FFromConnector.Data, Data);
        FreeAndNil(PatchArea.FNewCable);
      end;
    end;
  end;
  inherited;
end;

procedure TG2FMXConnector.MouseMove(Shift: TShiftState; X, Y: Single);
var PatchArea : TG2FMXPatchArea;
    Module : TG2FMXModule;
begin
  if (ssLeft in Shift) and assigned(FParent) then begin
    Module := FParent as TG2FMXModule;
    if assigned(Module.Parent) then begin
      PatchArea := Module.Parent as TG2FMXPatchArea;
      PatchArea.MouseMove(Shift, Module.Position.X + Position.X + X, Module.Position.Y + Position.Y + Y);
    end;
  end;
  inherited;
end;


procedure TG2FMXConnector.Paint;
var
  wSize, eSize: Single;
  rInner, rOuter : TRectF;
begin
  if Width < Height then begin
    wSize := Width / 2;
    rOuter.Left := 0;
    rOuter.Right := Width;
    rOuter.Top := Height / 2 - Width / 2;
    rOuter.Bottom := Height / 2 + Width / 2;
  end else begin
    wSize := Height / 2;
    rOuter.Left := Width / 2 - Height / 2;
    rOuter.Right := Width / 2 + Height / 2;
    rOuter.Top := 0;
    rOuter.Bottom := Height;
  end;
  eSize := wSize / 2.4;
  rInner.Left := rOuter.Left + eSize;
  rInner.Top := rOuter.Top + eSize;
  rInner.Width := rOuter.Width - eSize * 2;
  rInner.Height := rOuter.Height - eSize * 2;

  Canvas.Fill.Assign(FFillOuter);
  Canvas.FillEllipse(rOuter, Opacity);
  Canvas.Fill.Assign(FFillInner);
  Canvas.FillEllipse(rInner, Opacity);
end;

procedure TG2FMXConnector.ParsePanelData(fs: TModuleDefStream);
begin
  inherited;

  FData.CalcDefColor;
end;

function TG2FMXConnector.ParseProperties( fs: TModuleDefStream; aName : string): boolean;
var aValue : string;
begin
  Result := True;

  if not inherited ParseProperties( fs, aName) then begin

    if aName = 'CodeRef' then begin
      aValue := fs.ReadUntil([#13]);
      FData.ConnectorIndex := StrToInt(aValue);
    end else

    if aName = 'InfoFunc' then begin
      aValue := fs.ReadUntil([#13]);
      //
    end else

    if aName = 'Type' then begin
      aValue := fs.ReadUntil([#13]);
      if aValue = '"Audio"' then
        FData.ConnectorType := ctAudio
      else
        if aValue = '"Logic"' then
          FData.ConnectorType := ctLogic
        else
          if aValue = '"Control"' then
            FData.ConnectorType := ctControl
          else
            raise Exception.Create('Unknown control type ' + aValue);
    end else

    if aName = 'Bandwidth' then begin
      aValue := fs.ReadUntil([#13]);
      if aValue = '"Static"' then
        FData.Bandwidth := btStatic
      else
        if aValue = '"Dynamic"' then
          FData.BandWidth := btDynamic
        else
          raise Exception.Create('Unknown bandwidth type ' + aValue);
    end else

      Result := False
  end;
end;

procedure TG2FMXConnector.SetData(aConnectorData: TG2FileConnector);
begin
  FData := aConnectorData;
  FData.GraphControl := self;
end;


// ==== TG2GraphCable ==========================================================

constructor TG2GraphCable.Create( AOwner : TComponent);
var i : integer;
begin
  if not( AOwner is TG2GraphPatch) then
    raise Exception.Create('Owner must be a patch');

  FPatch := AOwner as TG2GraphPatch;
  FParent := nil;

  FromConnector := nil;
  ToConnector   := nil;

  FGraphControl := TG2FMXCable.Create(AOwner);

  InitCable;
end;

destructor TG2GraphCable.Destroy;
begin
  FGraphControl.Free;

  // Remove the cable from the connectors
  if assigned(FromConnector) then
    FromConnector.DelCable( self);

  if assigned(ToConnector) then
    ToConnector.DelCable( self);

  inherited;
end;

procedure TG2GraphCable.SetParent(aValue: TFmxObject);
begin
  FParent := aValue;
  FGraphControl.Parent := FParent;
end;

procedure TG2GraphCable.InitCable;
var ModuleFrom, ModuleTo : TG2FMXModule;
    ConnectorFrom, ConnectorTo : TG2FMXConnector;
begin
  if not(assigned(FromConnector) and (assigned(ToConnector)
      and (assigned(FromConnector.Module)) and (assigned((FromConnector.Module as TG2GraphModule).Parent)))) then
    exit;

  ModuleFrom := (FromConnector.Module as TG2GraphModule).FPanel;
  ModuleTo := (ToConnector.Module as TG2GraphModule).FPanel;

  ConnectorFrom := FromConnector.GraphControl as TG2FMXConnector;
  ConnectorTo := ToConnector.GraphControl as TG2FMXConnector;

  FGraphControl.Point1X := ModuleFrom.ScrollPosX + ConnectorFrom.Position.X + ConnectorFrom.Width / 2;
  FGraphControl.Point1Y := ModuleFrom.ScrollPosY + ConnectorFrom.Position.Y + ConnectorFrom.Height / 2;
  FGraphControl.Point2X := ModuleTo.ScrollPosX + ConnectorTo.Position.X + ConnectorTo.Width / 2;
  FGraphControl.Point2Y := ModuleTo.ScrollPosY + ConnectorTo.Position.Y + ConnectorTo.Height / 2;
  FGraphControl.InitCable;
end;

procedure TG2GraphCable.ConnectorMoved;
var ModuleFrom, ModuleTo : TG2FMXModule;
    ConnectorFrom, ConnectorTo : TG2FMXConnector;
begin
  if not(assigned(FromConnector) and (assigned(ToConnector)
      and (assigned(FromConnector.Module)) and (assigned((FromConnector.Module as TG2GraphModule).Parent)))) then
    exit;

  ModuleFrom := (FromConnector.Module as TG2GraphModule).FPanel;
  ModuleTo := (ToConnector.Module as TG2GraphModule).FPanel;

  ConnectorFrom := FromConnector.GraphControl as TG2FMXConnector;
  ConnectorTo := ToConnector.GraphControl as TG2FMXConnector;

  FGraphControl.Point1X := ModuleFrom.ScrollPosX + ConnectorFrom.Position.X + ConnectorFrom.Width / 2;
  FGraphControl.Point1Y := ModuleFrom.ScrollPosY + ConnectorFrom.Position.Y + ConnectorFrom.Height / 2;
  FGraphControl.Point2X := ModuleTo.ScrollPosX + ConnectorTo.Position.X + ConnectorTo.Width / 2;
  FGraphControl.Point2Y := ModuleTo.ScrollPosY + ConnectorTo.Position.Y + ConnectorTo.Height / 2;
  FGraphControl.StartTimer;
end;

// ==== TG2FMXCable ============================================================

constructor TG2FMXCable.Create(AOwner: TComponent);
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

destructor TG2FMXCable.Destroy;
var i : integer;
begin
  for i := 0 to FNodeCount - 1 do
    FNodes[i].Free;
  Finalize(FNodes);

  FTimer.Enabled := False;
  FTimer.Free;

  inherited;
end;

procedure TG2FMXCable.InitCable;
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

procedure TG2FMXCable.IterateCable;
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

procedure TG2FMXCable.OnTimer(Sender: TObject);
begin
  if FTimerCount <= 0 then
    FTimer.Enabled := False;
  Dec(FTimerCount);
  IterateCable;
  Repaint;
end;

procedure TG2FMXCable.StartTimer;
begin
  IterateCable;
  FTimerCount := 50;
  FTimer.Enabled := True;
end;

procedure TG2FMXCable.SetPoint1X( Value : single);
begin
  FP1.X := Value;
end;

procedure TG2FMXCable.SetPoint1Y( Value : single);
begin
  FP1.Y := Value;
end;

procedure TG2FMXCable.SetPoint2X( Value : single);
begin
  FP2.X := Value;
end;

procedure TG2FMXCable.SetPoint2Y( Value : single);
begin
  FP2.Y := Value;
end;

procedure TG2FMXCable.Paint;
var i : integer;
begin
  //Canvas.FillRect(RectF(0, 0, Width, Height), 0, 0, [], 1);
  Canvas.Stroke.Kind := TBrushKind.bkSolid;
  Canvas.StrokeThickness := 2;

  for i := 1 to FNodeCount - 1 do begin
    Canvas.DrawLine(PointF(FNodes[i-1].x - Position.X, FNodes[i-1].y - Position.Y),
                    PointF(FNodes[i].x - Position.X, FNodes[i].y - Position.Y), Opacity);
  end;
end;


procedure Register;
begin
  RegisterComponents('G2 FMX Controls', [TG2FMXPatchArea, TG2FMXControl, TG2FMXCable, TG2FMXConnector]);
end;

end.
