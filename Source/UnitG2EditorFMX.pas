unit UnitG2EditorFMX;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, Math,
  FMX.Layouts, FMX.Objects, FMX.Menus, g2_types, g2_file, FMX.ExtCtrls;

type
  TModuleClickEvent = procedure(Sender : TObject; Button: TMouseButton; Shift: TShiftState; X,  Y: Integer; Module : TG2FileModule) of Object;
  TParameterClickEvent = procedure(Sender : TObject; Button: TMouseButton; Shift: TShiftState; X,  Y: Integer; Parameter : TG2FileParameter) of Object;
  TConnectorClickEvent = procedure(Sender : TObject; Button: TMouseButton; Shift: TShiftState; X,  Y: Integer; Parameter : TG2FileConnector) of Object;

  TG2GraphPatch = class;
  TG2GraphModulePanelFMX = class;
  TG2GraphControlFMX = class;

  TG2Graph = class( TG2File)
  // Contains the control for the VA and FX patching
  private
    //FForm : TForm;
    //FScrollboxVA : TScrollBox;
    //FScrollboxFX : TScrollBox;
    FLayoutVA : TPanel;
    FLayoutFX : TPanel;
    FClientType  : TClientType;
    procedure SetClientType( aValue: TClientType);
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    function    CreatePerformance : TG2FilePerformance; override;
    function    GetSelectedPatch : TG2GraphPatch; virtual;
    procedure   SetScrollboxVA( aValue : TScrollbox);
    procedure   SetScrollboxFX( aValue : TScrollbox);
  published
    //property    ScrollboxVA : TScrollBox read FScrollboxVA write SetScrollboxVA;
    //property    scrollboxFX : TScrollBox read FScrollboxFX write SetScrollboxFX;
    property    ClientType : TClientType read FClientType write SetClientType;
  end;

  TG2GraphSlot = class( TG2FileSlot)
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    function    CreatePatch : TG2FilePatch; override;
  end;

  TG2GraphPerformance = class( TG2FilePerformance)
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    function    CreateSlot : TG2FileSlot; override;
  end;

  TG2GraphPatch = class( TG2FilePatch)
  // This represents the patch
  private
  // Lists for the leds
    FMiniVUList         : TList;
    FLedGroupList       : TList;
    FLed39List          : TList;
    FLed3AList          : TList;
    FVisible            : boolean;
    FSelectedControl    : TG2GraphControlFMX;
    FSelectedMorphIndex : integer;
    procedure   SetVisible( aValue : boolean);
    procedure   SetSelectedControl( aValue : TG2GraphControlFMX);
    procedure   SetSelectedMorphIndex( aValue : integer);
  public
    //FG2 : TG2Graph;
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   Init; override;

    function    CreateModule( aLocation : TLocationType; aModuleIndex : byte; aModuleType : byte): TG2FileModule; override;
    function    CreateCable( aLocation : TLocationType; aColor : byte; aFromModule : byte; aFromConnector : byte; aLinkType : byte; aToModule : byte; aToConnector : byte): TG2FileCable; override;

    {procedure   SelectModules;
    procedure   MoveOutlines( dX, dY : single);
    function    MessMoveModules: boolean; virtual;}
    procedure   SelectModulesInRect( aLocation : TLocationType; aRect : TRect);
    procedure   MoveOutlines( aLocation : TLocationType; dX, dY: single);
    function    MessMoveModules( aLocation : TLocationType): boolean;

    //function    GetG2 : TG2Graph; virtual;
    function    CreateParameter( aModuleIndex : byte): TG2FileParameter; override;
    //procedure   UnselectModules( aLocation : TLocationType);
    procedure   SetMiniVULevel( Index : integer; aValue : byte);
    procedure   SetLedLevel( Index : integer; aValue : byte);
    function    GetMiniVUListCount : integer;
    //function    GetMiniVU( Index : integer): TG2GraphLed;
    function    GetLedListCount : integer;
    //function    GetLed( Index : integer): TG2GraphLed;
    procedure   SortLeds; override;
    procedure   RemoveFromLedList( aLocation: TLocationType; aModuleIndex : integer);

    property    Visible : boolean read FVisible write SetVisible;
    property    SelectedControl : TG2GraphControlFMX read FSelectedControl write SetSelectedControl;
    property    SelectedMorphIndex : integer read FSelectedMorphIndex write SetSelectedMorphIndex;
  end;

  TG2GraphModule = class( TG2FileModule)
  private
    FOutlineRect    : TRectF;
    FOutlineVisible : boolean;
    FFreePanel      : boolean;
    FPanel          : TG2GraphModulePanelFMX;
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

  TG2GraphModulePanelFMX = class( TRectangle)
  // This represents the module
  private
    FData :   TG2GraphModule;
    FOldX,
    FOldY : single;
    FStartX,
    FStartY : single;

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

    function    GetGraphChildControl( ChildControlIndex : integer): TG2GraphControlFMX;
    function    GetChildControlsCount : integer;
    procedure   AddGraphControl( aGraphCtrl : TG2GraphControlFMX);

    procedure   SelectModule;
    procedure   MoveOutline( dX, dY : single);
    procedure   MoveModule;

    property    ScrollBarX : single read GetScrollBarX;
    property    ScrollBarY : single read GetScrollBarY;
    property    ScrollPosX : single read GetScrollPosX;
    property    ScrollPosY : single read GetScrollPosY;
    property    GraphChildControls[ index : integer] : TG2GraphControlFMX read GetGraphChildControl;
    property    ChildControlsCount : integer read GetChildControlsCount;

    function    GetPatch : TG2GraphPatch;
    function    GetControlType( aG2GraphChildControl: TG2GraphControlFMX): string;
    function    NewG2GraphControl( aControlType : string) : TG2GraphControlFMX;

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

  TG2GraphControlFMX = class( TStyledControl)
  // This represents a child graphic control of another graphic control (module)
  protected
    FSelected       : boolean;
    FModule         : TG2GraphModulePanelFMX;
    FParameter      : TG2FileParameter;
    FMouseInput     : boolean;
    FZOrder         : integer;
    FValue,
    FStartValue,
    FLowValue,
    FHighValue      : byte;
    //FOnChange       : TChangeEvent;
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
    //function    GetRelToParentRect : TRect;
    //function    GetScreenCoordsRect : TRect;
    procedure   SetParameter( aParam : TG2FileParameter);
    procedure   SetModule( aValue : TG2GraphModulePanelFMX);
    procedure   SetValue( aValue : byte);
    function    GetValue : byte;
    procedure   SetParamLabel( aValue : string);
    function    GetParamLabel : string;
    procedure   InitValue( aValue: integer);
    function    CheckValueBounds( aValue : integer): byte;
    procedure   SetMorphValue( aValue: byte);
    function    GetHighValue : byte;
    function    GetLowValue : byte;
    function    GetGraphModule : TG2GraphModulePanelFMX;
    function    GetMorphValue: byte;
    function    HasMorph: boolean;
    function    GetMorph : TMorphParameter;
    function    GetParamIndex : integer;
    procedure   ParsePanelData( fs : TModuleDefStream); virtual;
    function    ParseProperties( fs: TModuleDefStream; aName : string): boolean; virtual;

    property    Parameter   : TG2FileParameter read FParameter write SetParameter;
    property    ParamIndex  : integer read GetParamIndex;
    property    Module      : TG2GraphModulePanelFMX read FModule write SetModule;
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
    //property    OnChange    : TChangeEvent read FOnChange write FOnChange;
  end;

  TG2GraphParameter = class(TG2FileParameter)
  private
    FControlList  : array of TG2GraphControlFMX; // array of controls the parameter is assigned to
  public
    constructor Create( aPatch : TG2FilePatch; aLocation : TLocationType; aModuleIndex : integer; aModule : TG2FileModule);
    destructor  Destroy; override;
    procedure   AssignControl( aControl : TG2GraphControlFMX);
    procedure   DeassignControl( aControl : TG2GraphControlFMX);
    //procedure   InvalidateControl; override;
  end;

  TG2GraphConnector = class(TG2GraphControlFMX)
  public
    //FModule : TG2GraphModulePanelFMX;
    FCircle : TCircle;
    FData   : TG2FileConnector;
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure ParsePanelData(fs: TModuleDefStream);
    function ParseProperties( fs: TModuleDefStream; aName : string): boolean;
    procedure SetData(aConnectorData: TG2FileConnector);
  published
    property Data : TG2FileConnector read FData write SetData;
  end;

  TCableElement = class(TShape)
  protected
    procedure   Paint; override;
  public
    x           : single;
    y           : single;
    vx          : single;
    vy          : single;
    RelLeft     : integer; // Relative to Cable
    RelTop      : integer; // Relative to Cable
    p           : array[0..3] of TPoint;
    Color       : TColor;
    ShadowColor : TColor;
    constructor Create( AOwner : TComponent); //override;
    destructor  Destroy; override;
  end;

  TG2GraphCable = class( TG2FileCable)
  // This represents a cable and the methods to manipulate it on the screen
  // Here's where i got the idea for the cables from
  // http://www.charlespetzold.com/blog/2008/11/Simple-Cable-Simulation.html
  // http://lab.andre-michelle.com/cable-interface
  protected
    FParent          : TFmxObject;
    FPatch           : TG2GraphPatch;
    FModule          : TG2GraphModulePanelFMX;
    FNode            : array[ 0.. NCABLE_ELEMENTS] of TCableElement;
    Fx1, Fy1,
    Fx2, Fy2         : integer;
    FSelected        : boolean;
    min_x, min_y, max_x, max_y : integer;
    FLeft : integer;
    FTop : integer;
    FWidth : integer;
    FHeight : integer;
    //FInvalidate : boolean;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   InitCable;
    procedure   PaintElements;
    procedure   ConnectorMoved; override;
    procedure   CableResize( ax1, ay1, ax2, ay2 : integer);
    //procedure   Invalidate;
    function    GetLeft : integer;
    function    GetTop : integer;
    procedure   SetLeft( aValue : integer);
    procedure   SetTop( aValue : integer);
    function    GetScrollBarX : single;
    function    GetScrollBarY : single;
    function    GetBoundsRect: TRect;
    function    GetClientRect : TRect;
    procedure   SetParent( aValue : TFmxObject);

    property    x1 : integer read Fx1 write Fx1;
    property    y1 : integer read Fy1 write Fy1;
    property    x2 : integer read Fx2 write Fx2;
    property    y2 : integer read Fy2 write Fy2;
    property    Left : integer read GetLeft write SetLeft;
    property    Top : integer read GetTop write SetTop;
    property    Width : integer read FWidth write FWidth;
    property    Height : integer read FHeight write FHeight;
    property    ScrollBarX : single read GetScrollBarX;
    property    ScrollBarY : single read GetScrollBarY;
    property    Parent : TFmxObject read FParent write SetParent;
    property    BoundsRect : TRect read GetBoundsRect;
    property    ClientRect : TRect read GetClientRect;
  end;

  TForm1 = class(TForm)
    StyleBook1: TStyleBook;
    MenuBar1: TMenuBar;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    SmallScrollBar1: TSmallScrollBar;
    LayoutZoomFX: TLayout;
    Splitter1: TSplitter;
    ScrollboxVA: TScrollBox;
    LayoutVA: TPanel;
    LayoutZoomVA: TLayout;
    ScrollBoxFX: TScrollBox;
    LayoutFX: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure SmallScrollBar1Change(Sender: TObject);
    procedure Splitter1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FG2 : TG2Graph;
    FPatch : TG2GraphPatch;
    procedure InitPatchLocations;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

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

procedure TG2Graph.SetScrollboxFX(aValue: TScrollbox);
begin
{  FScrollboxFX := aValue;
  if assigned( FScrollboxFX) then begin
  end;}
end;

procedure TG2Graph.SetScrollboxVA(aValue: TScrollbox);
begin
{  FScrollboxVA := aValue;
  if assigned( FScrollboxVA) then begin
  end;}
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
    ConnectorFrom, ConnectorTo : TG2GraphConnector;
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
      //Cable.Parent := FG2.ScrollboxFX
      Cable.Parent := (G2 as TG2Graph).FLayoutFX
    else
      //Cable.Parent := FG2.ScrollboxVA;
      Cable.Parent := (G2 as TG2Graph).FLayoutVA;

    // Cable needs scrollbar coords
    ConnectorFrom := Cable.FromConnector.GraphControl as TG2GraphConnector;
    ConnectorTo := Cable.ToConnector.GraphControl as TG2GraphConnector;

    Cable.x1 := trunc(ModuleFrom.ScrollPosX +  ConnectorFrom.Position.X + ConnectorFrom.Width / 2);
    Cable.y1 := trunc(ModuleTo.ScrollPosY +  ConnectorFrom.Position.Y + ConnectorFrom.Height / 2);
    Cable.x2 := Cable.x1;
    Cable.y2 := Cable.y1;
    Cable.InitCable;
    Cable.ConnectorMoved;
  end;

  Result := Cable;
end;

{procedure TG2GraphPatch.SelectModules;
var i, j : integer;
begin
  for i := 0 to 1 do
    for j := 0 to ModuleCount[i] - 1 do
      if (FModuleList[i].FModules[j] as TG2GraphModule).Selected then
        (FModuleList[i].FModules[j] as TG2GraphModule).FPanel.SelectModule;
end;

procedure TG2GraphPatch.MoveOutlines(dX, dY: single);
var i, j : integer;
begin
  for i := 0 to 1 do
    for j := 0 to ModuleCount[i] - 1 do
      if (FModuleList[i].FModules[j] as TG2GraphModule).Selected then
        (FModuleList[i].FModules[j] as TG2GraphModule).FPanel.MoveOutline( dX, dY);
end;

function TG2GraphPatch.MessMoveModules: boolean;
var i, j : integer;
begin
  for i := 0 to 1 do
    for j := 0 to ModuleCount[i] - 1 do
      if (FModuleList[i].FModules[j] as TG2GraphModule).Selected then
        (FModuleList[i].FModules[j] as TG2GraphModule).FPanel.MoveModule;
end;}

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
begin
  for i := 0 to PatchPart[ ord(aLocation)].SelectedModuleList.Count - 1 do
    (PatchPart[ ord(aLocation)].SelectedModuleList[i] as TG2GraphModule).FPanel.MoveOutline( dX, dY);

end;

function TG2GraphPatch.MessMoveModules( aLocation : TLocationType): boolean;
var i : integer;
begin
//  Result := inherited MessMoveModules( aLocation);
  for i := 0 to PatchPart[ ord(aLocation)].SelectedModuleList.Count - 1 do
    (PatchPart[ ord(aLocation)].SelectedModuleList[i] as TG2GraphModule).FPanel.MoveModule;
end;



{TODO function TG2GraphPatch.GetG2 : TG2Graph;
begin
  Result := nil;
end;}

function TG2GraphPatch.CreateParameter( aModuleIndex : byte): TG2FileParameter;
begin
  Result := TG2GraphParameter.Create( self, ltPatch, aModuleIndex, nil);
end;

{procedure TG2GraphPatch.UnselectModules( aLocation : TLocationType);
var i : integer;
begin
  for i := 0 to ModuleCount[ord(aLocation)] - 1 do
    if (FModuleList[ord(aLocation)].FModules[i] as TG2GraphModule).Selected then
      (FModuleList[ord(aLocation)].FModules[i] as TG2GraphModule).Selected := False;
end;}

{TODO function TG2GraphPatch.GetLed( Index: integer): TG2GraphLed;
begin
  Result := TG2GraphLedGreen(FLed39List.Items[Index])
end;}

function TG2GraphPatch.GetLedListCount: integer;
begin
  Result := FLed39List.Count;
end;

{TODO function TG2GraphPatch.GetMiniVU(Index: integer): TG2GraphLed;
begin
  Result := TG2GraphLed(FLed3AList.Items[Index])
end;}

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

procedure TG2GraphPatch.SetSelectedControl( aValue: TG2GraphControlFMX);
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


// ==== G2GraphModule ==========================================================

constructor TG2GraphModule.Create( aPatchPart : TG2FilePatchPart);
begin
  inherited Create( aPatchPart);
  FPanel := TG2GraphModulePanelFMX.Create( aPatchPart);
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

{constructor TG2GraphModule.Create( AOwner: TComponent);
begin
  inherited Create( AOwner);
  FPanel := TG2GraphModulePanelFMX.Create( AOwner);
  FPanel.FData := self;
  FFreePanel := True;
  FSelected := False;
  FOutlineRect := FPanel.BoundsRect;
  FOutlineVisible := False;
end;

constructor TG2GraphModule.CopyCreate( AOwner: TComponent; aModule : TG2GraphModule);
begin
  inherited Create( AOwner);
  Copy( aModule);
  FFreePanel := False;
  FPanel := aModule.FPanel;
  FSelected := False;
  FOutlineRect := aModule.FOutlineRect;
  FOutlineVisible := False;
end;

destructor TG2GraphModule.Destroy;
begin
  if FFreePanel then
    FPanel.Free;
  inherited;
end;

function TG2GraphModule.CreateCopy(AOwner: TComponent) : TG2FileModule;
begin
  Result := TG2GraphModule.CopyCreate(AOwner, self);
end;}

function TG2GraphModule.CreateParameter: TG2FileParameter;
begin
  //Result := TG2GraphParameter.Create( FPatch, TLocationType(FLocation), FModuleIndex);
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

{procedure TG2GraphModule.Invalidate;
begin
//
end;}

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


// ==== TG2GraphModulePanelFMX =================================================

constructor TG2GraphModulePanelFMX.Create(AOwner: TComponent);
begin
  inherited Create( AOWner);

  Opacity := 1;
  CanClip := True;

  FChildControls := TList.Create;

  FOldX := ScrollPosX;
  FOldY := ScrollPosY;

  FModuleLabel := TLabel.Create(self);
  FModuleLabel.Parent := self;
  FModuleLabel.Font.Family := 'Arial';
  FModuleLabel.Font.Size := 10;
  //FModuleLabel.Font.Style := fsBold;
  //FModuleLabel.Font.Color := $00000000;
  FModuleLabel.Position.X := 2;
  FModuleLabel.Position.Y := 2;
  FMOduleLabel.Text := 'Module';


  Width := UNITS_COL;
  Height := UNITS_ROW;
end;

destructor TG2GraphModulePanelFMX.Destroy;
begin

  inherited;
end;

function TG2GraphModulePanelFMX.GetScrollBarX: single;
begin
  if assigned(Parent) and (Parent is TScrollbox) then
    Result := (Parent as TScrollbox).hscrollBar.Value
  else
    Result := 0;
end;

function TG2GraphModulePanelFMX.GetScrollBarY: single;
begin
  if assigned(Parent) and (Parent is TScrollbox) then
    Result := (Parent as TScrollbox).vscrollBar.Value
  else
    Result := 0;
end;

function TG2GraphModulePanelFMX.GetScrollPosX: single;
begin
  if assigned(Parent) and (Parent is TScrollbox) then
    Result := Position.X + (Parent as TScrollbox).hscrollBar.Value
  else
    Result := Position.X;
end;

function TG2GraphModulePanelFMX.GetScrollPosY: single;
begin
  if assigned(Parent) and (Parent is TScrollbox) then
    Result := Position.Y + (Parent as TScrollbox).vscrollBar.Value
  else
    Result := Position.Y;
end;

function TG2GraphModulePanelFMX.GetSelected: boolean;
begin
  Result := FData.Selected;
end;

procedure TG2GraphModulePanelFMX.SetSelected(aValue: boolean);
begin
  FOldX := Left;
  FOldY := Top;

  FData.FOutlineRect := BoundsRect;
end;

procedure TG2GraphModulePanelFMX.SetCol(aValue: TBits7);
begin
  if aValue <> FData.Col then begin
    Position.X := aValue * UNITS_COL - ScrollBarX;
  end;
end;

procedure TG2GraphModulePanelFMX.SetRow(aValue: TBits7);
begin
  if aValue <> FData.Row then begin
    Position.Y := aValue * UNITS_ROW - ScrollbarY;
  end;
end;

procedure TG2GraphModulePanelFMX.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
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
    end;

    if Location <> Patch.SelectedLocation then
      Patch.SelectedLocation := Location;

    if ssLeft in Shift then begin
      FStartX := X;
      FStartY := Y;
      //Patch.SelectModules;
    end;

  inherited;
end;

procedure TG2GraphModulePanelFMX.MouseMove(Shift: TShiftState; X, Y: Single);
var Patch : TG2GraphPatch;
begin
  // Pass mousemovement to scrollbox when copying a patch
{  if assigned( Parent) and assigned((Parent as TG2GraphScrollbox).CopyPatch) then
    (Parent as TG2GraphScrollbox).MouseMove( Shift, Left + X, Top + Y);}


  if ssLeft in Shift then begin
    Patch := GetPatch;

    Patch.MoveOutlines( Data.Location, X - FStartX, Y - FStartY);
  end;

  inherited;
end;

procedure TG2GraphModulePanelFMX.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var NewCol, NewRow : byte;
    Patch : TG2GraphPatch;
begin
  Patch := GetPatch;

  if (FStartX <> X) or (FStartY <> Y) then
    Patch.MessMoveModules( Data.Location);

  if assigned( FOnModuleClick) then
    FOnModuleClick( self, Button, Shift, trunc(X), trunc(Y), FData);

  inherited;
end;

procedure TG2GraphModulePanelFMX.SelectModule;
begin
  FOldX := Position.X;
  FOldY := Position.Y;

  FData.FOutlineRect := BoundsRect;
end;

procedure TG2GraphModulePanelFMX.MoveOutline(dX, dY: single);
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
end;

procedure TG2GraphModulePanelFMX.MoveModule;
begin
  // TODO
{  if FData.FOutlineVisible then
    if assigned( Parent) then begin
      (Parent as TG2GraphScrollbox).DrawOutline(FData.FOutlineRect);
      FData.FOutlineVisible := False;
    end;}
end;

function TG2GraphModulePanelFMX.GetNewCol: TBits7;
begin
  Result := trunc((ScrollPosX + FData.FOutlineRect.Left - FOldX) / UNITS_COL);
end;

function TG2GraphModulePanelFMX.GetNewRow: TBits7;
begin
  Result := trunc((ScrollPosY + FData.FOutlineRect.Top - FOldY) / UNITS_ROW);
end;

function TG2GraphModulePanelFMX.GetPatch: TG2GraphPatch;
begin
  if not assigned(FData.PatchPart.Patch) then
    raise Exception.Create('Patch not assigned to module.');

  Result := FData.PatchPart.Patch as TG2GraphPatch;
end;

procedure TG2GraphModulePanelFMX.ParsePanelData;
var //MemStream : TMemoryStream;
    ModuleStream : TModuleDefStream;
    CodeRef, Err : integer;
    aPath : string;
    aName, aValue, ControlType, CodeRefStr : AnsiString;
    ChildControl : TG2GraphControlFMX;
    Connector : TG2GraphConnector;
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
              Connector := TG2GraphConnector.Create(self);
              Connector.FModule := self;
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
                Connector := TG2GraphConnector.Create(self);
                Connector.FModule := self;
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

{procedure TG2GraphModulePanelFMX.ParsePanelData;
var MemStream : TMemoryStream;
    i, CodeRef, Err : integer;
    aPath : string;
    aName, aValue, ControlType, CodeRefStr : AnsiString;
    ChildControl : TG2GraphControlFMX;
    Connector : TG2GraphConnector;
    Param : TG2GraphParameter;
    Patch : TG2GraphPatch;
begin
  aPath := ExtractFilePath(ParamStr(0));
  //aPath := GetCurrentDir;
//{$IFDEF FPC}
//  aPath := aPath + 'Modules/';
//{$ELSE}
{  aPath := aPath + '\Modules\';
//{$ENDIF}

{  Patch := GetPatch;

  if FileExists( aPath + FData.ModuleName + '.txt') then begin
    MemStream := TMemoryStream.Create;
    MemStream.LoadFromFile( aPath + FData.ModuleName + '.txt');
    try
      if ReadConst(MemStream, '<#Module') then begin
        aName := 'Module';
        while (MemStream.Position < MemStream.Size) and (aName[1] <> '<') do begin
          ReadSpaces( MemStream);
          aName := ReadUntil(MemStream, [':', #13]);
          if aName[1] <> '<' then begin
            aValue := ReadUntil(MemStream, [#13]);

            if aName = 'Height' then
              Height := UNITS_ROW * StrToInt(aValue);
          end;
        end;

        while aName[1] = '<' do begin
          ControlType := copy(aName, 3, Length(aName) - 2);
          if ControlType = 'Input' then begin

            CodeRefStr := PeekValue( MemStream, 'CodeRef:', [#13, #10, '#'], ['#', '>']);
            val( CodeRefStr, CodeRef, Err);
            if Err = 0 then begin
              Connector := TG2GraphConnector.Create(self);
              Connector.FModule := self;
              Connector.Parent := self;
              Connector.Data := FData.InConnector[ CodeRef];
              if Connector.Data = nil then
                raise Exception.Create('Data for connector not found...');

              Connector.ParsePanelData(MemStream);
              AddGraphControl( Connector);
            end else
              raise Exception.Create('Parse error, module  ' + FData.FModuleName + ' input connector CodeRef not found.' );

          end else
            if ControlType = 'Output' then begin

              CodeRefStr := PeekValue( MemStream, 'CodeRef:', [#13, #10, '#'], ['#', '>']);
              val( CodeRefStr, CodeRef, Err);
              if Err = 0 then begin
                Connector := TG2GraphConnector.Create(self);
                Connector.FModule := self;
                Connector.Parent := self;

                Connector.Data := FData.OutConnector[ CodeRef];
                if Connector.Data = nil then
                  raise Exception.Create('Data for connector not found...');

                Connector.ParsePanelData(MemStream);
                AddGraphControl( Connector);
              end else
                raise Exception.Create('Parse error, module  ' + FData.FModuleName + ' output connector CodeRef not found.' );

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

                  CodeRefStr := PeekValue( MemStream, 'CodeRef:', [#13, #10, '#'], ['#', '>']);
                  val( CodeRefStr, CodeRef, Err);
                  if Err = 0 then begin
                    if ControlType = 'PartSelector' then begin
                      Param := FData.Mode[ CodeRef] as TG2GraphParameter;
                    end else begin
                      Param := FData.Parameter[ CodeRef] as TG2GraphParameter;
                    end;
                    ChildControl.ParsePanelData(MemStream);
                    ChildControl.Parameter := Param;
                  end else
                    raise Exception.Create('Parse error, module  ' + FData.FModuleName + ' parameter CodeRef not found.' );

                end else
                  // No parameter associated
                  ChildControl.ParsePanelData(MemStream);

              end else begin
                while (MemStream.Position < MemStream.Size) and (aName <> '#>') do begin
                  ReadSpaces(MemStream);
                  aName := ReadUntil(MemStream, [':', #13]);
                  if aName <> '#>' then begin
                    aValue := ReadUntil(MemStream, [#13]);
                  end;
                end;
              end;
            end;
          ReadSpaces(MemStream);
          aName := ReadUntil(MemStream, [':', #13]);
        end;

//        FChildControls.Sort(@CompareZOrder);
      end else
        raise Exception.Create('Unknown file type.');

      Patch.SortLeds;
    finally
      //fs.Free;
      MemStream.Free;
    end;
  end;
end;}

function TG2GraphModulePanelFMX.GetControlType( aG2GraphChildControl: TG2GraphControlFMX): string;
begin
  if aG2GraphChildControl is TG2GraphControlFMX then
    Result := '';

  if aG2GraphChildControl is TG2GraphConnector then
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

function TG2GraphModulePanelFMX.NewG2GraphControl( aControlType: string): TG2GraphControlFMX;
begin
  Result := nil;

  if (aControlType = 'Line') then begin
    //Result := TG2GraphLine.Create(self);
    Result := TG2GraphControlFMX.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'Text') then begin
    //Result := TG2GraphLabel.Create(self);
    Result := TG2GraphControlFMX.Create(self);
    Result.Module := self;
  end;

  if aControlType = 'Connector' then begin
    Result := TG2GraphConnector.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'TextField') then begin
    //Result := TG2GraphDisplay.Create(self);
    Result := TG2GraphControlFMX.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'Graph') then begin
    //Result := TG2GraphGraph.Create(self);
    Result := TG2GraphControlFMX.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'Led') then begin
    //Result := TG2GraphLedGreen.Create(self);
    Result := TG2GraphControlFMX.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'MiniVU') then begin
    //Result := TG2GraphMiniVU.Create(self);
    Result := TG2GraphControlFMX.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'PartSelector') then begin
    //Result := TG2GraphPartSelector.Create(self);
    Result := TG2GraphControlFMX.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'ButtonText') then begin
    //Result := TG2GraphButtonText.Create(self);
    Result := TG2GraphControlFMX.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'TextEdit') then begin
    //Result := TG2GraphButtonText.Create(self);
    Result := TG2GraphControlFMX.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'ButtonFlat') then begin
    //Result := TG2GraphButtonFlat.Create(self);
    Result := TG2GraphControlFMX.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'LevelShift') then begin
    //Result := TG2GraphButtonFlat.Create(self);
    Result := TG2GraphControlFMX.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'ButtonRadio') then begin
    //Result := TG2GraphButtonRadio.Create(self);
    Result := TG2GraphControlFMX.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'ButtonIncDec') then begin
    //Result := TG2GraphButtonIncDec.Create(self);
    Result := TG2GraphControlFMX.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'Knob') or (aControlType = '') then begin
    //Result := TG2GraphKnob.Create(self);
    Result := TG2GraphControlFMX.Create(self);
    Result.Module := self;
  end;

  //if not assigned( Result)  then
  //  raise Exception.Create('Unknown control type ' + aControlType);
end;


function TG2GraphModulePanelFMX.GetChildControlsCount: integer;
begin
  Result := FChildControls.Count;
end;

function TG2GraphModulePanelFMX.GetGraphChildControl( ChildControlIndex: integer): TG2GraphControlFMX;
begin
  Result := TG2GraphControlFMX( FChildControls[ChildControlIndex]);
end;

function TG2GraphModulePanelFMX.GetColor: TColor;
begin
  Result := FData.Color;
end;

function TG2GraphModulePanelFMX.GetLocation: TLocationType;
begin
  Result := FData.Location;
end;

function TG2GraphModulePanelFMX.GetModuleIndex: TBits8;
begin
  Result := FData.ModuleIndex;
end;

procedure TG2GraphModulePanelFMX.AddGraphControl(aGraphCtrl: TG2GraphControlFMX);
begin
  FChildControls.Add( aGraphCtrl);
end;

// ==== TG2GraphControlFMX ===================================================

constructor TG2GraphControlFMX.Create( AOwner: TComponent);
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
end;

destructor TG2GraphControlFMX.Destroy;
begin
  SetParameter(nil);
  inherited;
end;

{function TG2GraphControlFMX.GetRelToParentRect : TRect;
begin
  if assigned( FModule) then
    Result := AddRect( BoundsRect, FModule.BoundsRect)
  else
    Result := BoundsRect;
end;}

{function TG2GraphControlFMX.GetScreenCoordsRect: TRect;
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

procedure TG2GraphControlFMX.SetModule( aValue: TG2GraphModulePanelFMX);
begin
  FModule := aValue;
end;

procedure TG2GraphControlFMX.SetParameter( aParam : TG2FileParameter);
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

function TG2GraphControlFMX.GetGraphModule: TG2GraphModulePanelFMX;
begin
  Result := FModule;
end;

function TG2GraphControlFMX.GetHighValue: byte;
begin
  if assigned( FParameter) then
    Result := FParameter.HighValue
  else
    Result := FHighValue;
end;

function TG2GraphControlFMX.GetLowValue: byte;
begin
  if assigned( FParameter) then
    Result := FParameter.LowValue
  else
    Result := FLowValue;
end;

function TG2GraphControlFMX.HasMorph: boolean;
begin
  if assigned( FParameter) then
    Result := FParameter.HasMorph
  else
    Result := False;
end;

function TG2GraphControlFMX.GetMorph: TMorphParameter;
begin
  if assigned( FParameter) then
    Result := FParameter.GetSelectedMorph
  else
    Result := nil;
end;

function TG2GraphControlFMX.GetMorphValue: byte;
begin
  if assigned( FParameter) then
    Result := FParameter.GetSelectedMorphValue
  else
    Result := 0;
end;

function TG2GraphControlFMX.GetParamIndex: integer;
begin
  if assigned( FParameter) then
    Result := FParameter.ParamIndex
  else
    Result := -1;
end;

function TG2GraphControlFMX.GetParamLabel: string;
begin
  if assigned( FParameter) then
    Result := FParameter.ParamLabel[0]
  else
    Result := '';
end;

function TG2GraphControlFMX.GetValue: byte;
begin
  if assigned( FParameter) then
    Result := FParameter.GetParameterValue
  else
    Result := FValue;
end;

procedure TG2GraphControlFMX.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: single);
begin
  if ssLeft in Shift then begin
    FStartValue := Value;
    //Select;
  end;
  inherited;
end;

procedure TG2GraphControlFMX.MouseMove(Shift: TShiftState; X, Y: single);
begin
  inherited;
end;

procedure TG2GraphControlFMX.MouseUp(Button: TMouseButton; Shift: TShiftState;  X, Y: single);
begin
  inherited;
end;

procedure TG2GraphControlFMX.ParsePanelData(fs: TModuleDefStream);
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

function TG2GraphControlFMX.ParseProperties( fs: TModuleDefStream; aName : string): boolean;
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

function TG2GraphControlFMX.CheckValueBounds( aValue: integer): byte;
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

procedure TG2GraphControlFMX.SetParamLabel(aValue: string);
begin
  if assigned( FParameter) then
    FParameter.ParamLabel[0] := aValue
  else begin
  end;
end;


procedure TG2GraphControlFMX.SetValue( aValue: byte);
begin
  if assigned( FParameter) then
    FParameter.SetParameterValue( aValue)
  else begin
    FValue := aValue;

{    if assigned( FOnChange) then
      FOnChange( self);}
  end;
end;

procedure TG2GraphControlFMX.SetLowValue( aValue: byte);
begin
  if assigned( FParameter) then
    FParameter.LowValue := aValue
  else begin
    FLowValue := aValue;
//    inherited Invalidate;
  end;
end;

procedure TG2GraphControlFMX.SetHighValue( aValue: byte);
begin
  if assigned( FParameter) then
    FParameter.HighValue := aValue
  else begin
    FHighValue := aValue;
    //inherited Invalidate;
  end;
end;

procedure TG2GraphControlFMX.InitValue( aValue: integer);
var Rect : TRect;
begin
  if (aValue >= FLowValue) and (aValue <= FHighValue) and ( aValue <> FValue) then begin
    FValue := aValue;
    //Rect := BoundsRect;
    //InvalidateRect( Parent.Handle, @Rect, True);
  end;
end;

procedure TG2GraphControlFMX.SetMorphValue( aValue: byte);
var MorphParameter : TMorphParameter;
    TempValue : integer;
begin
  if assigned( FParameter) then begin
    FParameter.SetSelectedMorphValue( aValue);
    //Update;
  end;
end;

procedure TG2GraphControlFMX.Select;
begin
  FSelected := True;
  //Invalidate;
end;

procedure TG2GraphControlFMX.Deselect;
begin
  FSelected := False;
  //Invalidate;
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

procedure TG2GraphParameter.AssignControl(aControl: TG2GraphControlFMX);
var i : integer;
begin
  if not(aControl is TG2GraphControlFMX) then
    raise Exception.Create('Only a TG2GraphControlFMX can be assigned to a parameter.');

  i := 0;
  while (i < Length(FControlList)) and not(FControlList[i] = aControl) do
    inc(i);

  if not(i < Length(FControlList)) then begin
    SetLength(FControlList, i + 1);
    FControlList[i] := aControl as TG2GraphControlFMX;
    FControlList[i].FParameter := self;
  end;
end;

procedure TG2GraphParameter.DeassignControl(aControl: TG2GraphControlFMX);
var i, j : integer;
begin
  if not(aControl is TG2GraphControlFMX) then
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

{procedure TG2GraphParameter.InvalidateControl;
var i : integer;
begin
  // Update all controls attached to the parameter
  for i := 0 to Length(FControlList) - 1 do
    FControlList[i].Update;
end;}

{ ==== TG2GraphConnector =======================================================}

constructor TG2GraphConnector.Create(AOwner: TComponent);
begin
  inherited;

  Width := 13;
  Height := 13;

  FCircle := TCircle.Create(self);
  FCircle.Parent := self;
  FCircle.Align := TAlignLayout.alClient;
end;

destructor TG2GraphConnector.Destroy;
begin

  inherited;
end;

procedure TG2GraphConnector.ParsePanelData(fs: TModuleDefStream);
begin
  inherited;

  FData.CalcDefColor;
end;

function TG2GraphConnector.ParseProperties( fs: TModuleDefStream; aName : string): boolean;
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

procedure TG2GraphConnector.SetData(aConnectorData: TG2FileConnector);
begin
  FData := aConnectorData;
  //FData.Control := self;
  FData.GraphControl := self;
end;

// ==== TCableElements =========================================================

constructor TCableElement.Create( AOwner : TComponent);
begin
  inherited Create( AOwner);
end;

destructor TCableElement.Destroy;
begin
  inherited;
end;

procedure TCableElement.Paint;
var i : integer;
begin
//  inherited;

  //ExtBitmap.Canvas.Polygon( ep);
  Canvas.Stroke.Color := claBlue;
  Canvas.Stroke.Kind:= tBrushKind.bkSolid;

//  Canvas.BeginScene;
  Canvas.DrawLine( Pointf(p[0].x, p[0].y), Pointf(p[1].x, p[1].y), 1);
  Canvas.DrawLine( Pointf(p[2].x, p[2].y), Pointf(p[3].x, p[3].y), 1);
//  Canvas.EndScene;
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
  for i := 0 to NCABLE_ELEMENTS - 1 do begin
    FNode[i] := TCableElement.Create( nil);
    FNode[i].Parent := FParent;
  end;

  Fx1 := Left;
  Fy1 := Top;
  Fx2 := Left;
  Fy2 := Top;

  InitCable;
end;

destructor TG2GraphCable.Destroy;
var i : integer;
begin
  for i := 0 to NCABLE_ELEMENTS - 1 do
    FNode[i].Free;

  // Remove the cable from the connectors
  if assigned(FromConnector) then
    FromConnector.DelCable( self);

  if assigned(ToConnector) then
    ToConnector.DelCable( self);

  inherited;
end;

function TG2GraphCable.GetBoundsRect: TRect;
begin
  Result.Left := FLeft;
  Result.Top := FTop;
  Result.Right := FLeft + FWidth;
  Result.Bottom := FTop + FHeight;
end;

function TG2GraphCable.GetClientRect: TRect;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Right := FWidth;
  Result.Bottom := FHeight;
end;

function TG2GraphCable.GetLeft: integer;
begin
  Result := FLeft;
end;

procedure TG2GraphCable.SetLeft( aValue: integer);
begin
  FLeft := aValue;
end;

procedure TG2GraphCable.SetParent(aValue: TFmxObject);
var i : integer;
begin
  FParent := aValue;
  for i := 0 to NCABLE_ELEMENTS - 1 do begin
    FNode[i].Parent := FParent;
  end;
end;

function TG2GraphCable.GetTop: integer;
begin
  Result := FTop;
end;

procedure TG2GraphCable.SetTop( aValue: integer);
begin
  FTop := aValue;
end;

function TG2GraphCable.GetScrollBarX: single;
begin
  if FParent <> nil then
    Result := TScrollbox(FParent).HScrollBar.Value
  else
    Result := 0;
end;

function TG2GraphCable.GetScrollBarY: single;
begin
  if FParent <> nil then
    Result := TScrollbox(FParent).VScrollBar.Value
  else
    Result := 0;
end;

procedure Tg2GraphCable.PaintElements;
var i, j, n : integer;
    w : single;
    pt1, pt2, pt3, ptn : TPointF;
    p1, p2 : TPoint;
    pts : array[0..(NCABLE_ELEMENTS*2)-1] of TPoint;
    BitMap : TBitMap;
    Rect : TRect;
    Color : TColor;

    function Darker(c : integer; f : byte): integer;
    var R, G, B : byte;

      function sub( comp : byte): byte;
      begin
        if comp - f > 0 then
          result := comp - f
        else
          result := 0;
      end;

    begin
      R := (c and $000000FF);
      G := (c and $0000FF00) shr 8;
      B := (c and $00FF0000) shr 16;

      result := sub(B) * 65536
              + sub(G) * 256
              + sub(R)
              + $FF000000;
    end;

    function GetG2Color( G2Color : byte): integer;
    begin
      Result := $FFFF0000;
    end;

    function GetUnitNormal(const pt1, pt2 : TPointF): TPointF;
    var
      dx, dy, f: single;
    begin
      dx := (pt2.X - pt1.X);
      dy := (pt2.Y - pt1.Y);

      if (dx = 0) and (dy = 0) then begin
        result.x := 0;
        result.y := 0;
      end else begin
        f := 1 / Hypot(dx, dy);
        dx := dx * f;
        dy := dy * f;
      end;
      Result.X := dy;  //ie perpendicular to
      Result.Y := -dx; //the unit vector
    end;

    function GetPoint( i : integer): TPointF;
    begin
      if i > 0 then begin
        if i < NCABLE_ELEMENTS then begin
          Result.x := FNode[i].x;
          Result.y := FNode[i].y;
        end else begin
          Result.x := FNode[NCABLE_ELEMENTS-1].x;
          Result.y := FNode[NCABLE_ELEMENTS-1].y;
        end;
      end else begin
        Result.x := FNode[0].x;
        Result.y := FNode[0].y;
      end;
    end;

    procedure CalcOulinePoints( aWidth : single);
    begin
      aWidth := aWidth / 2;
      n := 0;
      pt1.x := GetPoint(n + 0).x - Left;
      pt1.y := GetPoint(n + 0).y - Top;
      pt2.x := GetPoint(n + 1).x - Left;
      pt2.y := GetPoint(n + 1).y - Top;
      pt3.x := GetPoint(n + 2).x - Left;
      pt3.y := GetPoint(n + 2).y - Top;
      ptn := GetUnitNormal(pt3, pt1);

      while (n < NCABLE_ELEMENTS - 1) do begin
        pts[n].x := trunc(pt1.x - ptn.x * aWidth);
        pts[n].y := trunc(pt1.y - ptn.y * aWidth);

        inc(n);

        pt1 := pt2;
        pt2 := pt3;
        pt3.x := GetPoint(n + 2).x - Left;
        pt3.y := GetPoint(n + 2).y - Top;
        if n < NCABLE_ELEMENTS - 1 then
          ptn := GetUnitNormal(pt3, pt1);
      end;
      pts[NCABLE_ELEMENTS - 1].x := trunc(pt1.x - ptn.x * aWidth);
      pts[NCABLE_ELEMENTS - 1].y := trunc(pt1.y - ptn.y * aWidth);

      n := NCABLE_ELEMENTS - 1;
      pt1.x := GetPoint(n-0).x - Left;
      pt1.y := GetPoint(n-0).y - Top;
      pt2.x := GetPoint(n-1).x - Left;
      pt2.y := GetPoint(n-1).y - Top;
      pt3.x := GetPoint(n-2).x - Left;
      pt3.y := GetPoint(n-2).y - Top;
      ptn := GetUnitNormal(pt1, pt3);

      while (n > 0) do begin
        pts[(NCABLE_ELEMENTS * 2 - 1) - n].x := trunc(pt1.x + ptn.x * aWidth);
        pts[(NCABLE_ELEMENTS * 2 - 1) - n].y := trunc(pt1.y + ptn.y * aWidth);

        dec(n);

        pt1 := pt2;
        pt2 := pt3;
        pt3.x := GetPoint(n-2).x - Left;
        pt3.y := GetPoint(n-2).y - Top;
        if n > 1 then
          ptn := GetUnitNormal(pt1, pt3);
      end;
      pts[NCABLE_ELEMENTS * 2 - 1].x := trunc(pt1.x + ptn.x * aWidth);
      pts[NCABLE_ELEMENTS * 2 - 1].y := trunc(pt1.y + ptn.y * aWidth);
    end;

begin
  inherited;

  Color := GetG2Color(CableColor);

  // Calc bitmap sizes
  for i := 1 to NCABLE_ELEMENTS - 1 do begin
    FNode[i].RelLeft:= min( trunc(FNode[i].x), trunc(FNode[i-1].x)) - Left - CABLE_CONTROL_MARGIN;
    FNode[i].RelTop := min( trunc(FNode[i].y), trunc(FNode[i-1].y)) - Top - CABLE_CONTROL_MARGIN;
    FNode[i].Position.X := FNode[i].RelLeft + Left;
    FNode[i].Position.Y := FNode[i].RelTop + Top;
    FNode[i].Width  := trunc(abs( FNode[i].x - FNode[i-1].x)) + CABLE_CONTROL_MARGIN * 2;
    FNode[i].Height := trunc(abs( FNode[i].y - FNode[i-1].y)) + CABLE_CONTROL_MARGIN * 2;
    FNode[i].Color := Color;
    FNode[i].ShadowColor := Darker(Color, 80);

    {FNode[i].ClientRect.Left := 0;
    FNode[i].ClientRect.Top := 0;
    FNode[i].ClientRect.Right := FNode[i].Width;
    FNode[i].ClientRect.Bottom := FNode[i].Height;

    Fnode[i].BoundsRect.Left := FNode[i].Left;
    FNode[i].BoundsRect.Top := FNode[i].Top;
    Fnode[i].BoundsRect.Right := FNode[i].Left + FNode[i].Width;
    FNode[i].BoundsRect.Bottom := FNode[i].Top + FNode[i].Height;}
  end;

  CalcOulinePoints(2);

  for n := 1 to NCABLE_ELEMENTS - 1 do begin
    FNode[n].p[0].x := pts[n-1].x - FNode[n].RelLeft;
    FNode[n].p[0].y := pts[n-1].y - FNode[n].RelTop;
    FNode[n].p[1].x := pts[n].x   - FNode[n].RelLeft;
    FNode[n].p[1].y := pts[n].y   - FNode[n].RelTop;

    FNode[n].p[2].x := pts[ NCABLE_ELEMENTS * 2 - (n-1) - 2].x - FNode[n].RelLeft;
    FNode[n].p[2].y := pts[ NCABLE_ELEMENTS * 2 - (n-1) - 2].y - FNode[n].RelTop;
    FNode[n].p[3].x := pts[ NCABLE_ELEMENTS * 2 - (n-1) - 1].x - FNode[n].RelLeft;
    FNode[n].p[3].y := pts[ NCABLE_ELEMENTS * 2 - (n-1) - 1].y - FNode[n].RelTop;
  end;
end;

procedure TG2GraphCable.InitCable;
var n : integer;
    dx, dy : single;
begin
  n := NCABLE_ELEMENTS;

  dx := ( Fx2 - Fx1) / (n + 1);
  dy := ( Fy2 - Fy1) / (n + 1);

  dec(n);
  while (n >= 0) do begin

    FNode[n].x := Fx1 + dx * n;
    FNode[n].y := Fy1 + dy * n;
    FNode[n].vx := 0;
    FNode[n].vy := 0;

    dec(n);
  end;
end;

{procedure TG2GraphCable.Invalidate;
begin
  FInvalidate := True;
end;}

procedure TG2GraphCable.CableResize( ax1, ay1, ax2, ay2 : integer);
var n, min_x, min_y, max_x, max_y : integer;
begin
  n := NCABLE_ELEMENTS - 1;

  Fx1 := ax1;
  Fy1 := ay1;
  Fx2 := ax2;
  Fy2 := ay2;

  min_x := min(Fx1, Fx2);
  max_x := max(Fx1, Fx2);
  min_y := min(Fy1, Fy2);
  max_y := max(Fy1, Fy2);

  FNode[n].x := Fx2;
  FNode[n].y := Fy2;

  dec(n);

  while (n > 0) do begin

  	FNode[n].vx := FNode[n].vx + ( FNode[n + 1].x + FNode[n - 1].x - FNode[n].x * 2 ) / TENSION;
		FNode[n].vy := FNode[n].vy + ( FNode[n + 1].y + FNode[n - 1].y - FNode[n].y * 2 ) / TENSION;

    FNode[n].vy := FNode[n].vy + GRAVITY;

    //-- Reibung
    FNode[n].vx := FNode[n].vx * DAMP;
    FNode[n].vy := FNode[n].vy * DAMP;

    //-- Addieren der neuen Vektoren
    FNode[n].x := FNode[n].x + FNode[n].vx;
    FNode[n].y := FNode[n].y + FNode[n].vy;

    if FNode[n].x < min_x then
      min_x := trunc(FNode[n].x);

    if FNode[n].x > max_x then
      max_x := trunc(FNode[n].x);

    if FNode[n].y < min_y then
      min_y := trunc(FNode[n].y);

    if FNode[n].y > max_y then
      max_y := trunc(FNode[n].y);

    dec(n);
  end;
  FNode[0].x := Fx1;
  FNode[0].y := Fy1;

  Left := min_x - CABLE_CONTROL_MARGIN;
  Top := min_y - CABLE_CONTROL_MARGIN;
  Width := max_x - min_x + CABLE_CONTROL_MARGIN * 2;
  Height := max_y - min_y + CABLE_CONTROL_MARGIN * 2;
end;

procedure TG2GraphCable.ConnectorMoved;
var i : integer;
    Rect : TRect;
    ModuleFrom, ModuleTo : TG2GraphModulePanelFMX;
    ConnectorFrom, ConnectorTo : TG2GraphConnector;
begin
  // TODO : make this better
  if not(assigned(FromConnector) and (assigned(ToConnector)
      and (assigned(FromConnector.Module)) and (assigned((FromConnector.Module as TG2GraphModule).Parent)))) then
    exit;

  ModuleFrom := (FromConnector.Module as TG2GraphModule).FPanel;
  ModuleTo := (ToConnector.Module as TG2GraphModule).FPanel;

  ConnectorFrom := FromConnector.GraphControl as TG2GraphConnector;
  ConnectorTo := ToConnector.GraphControl as TG2GraphConnector;

  for i := 0 to 50 do
    CableResize( trunc(ModuleFrom.ScrollPosX + ConnectorFrom.Position.X + ConnectorFrom.Width / 2),
                 trunc(ModuleFrom.ScrollPosY + ConnectorFrom.Position.Y + ConnectorFrom.Height / 2),
                 trunc(ModuleTo.ScrollPosX + ConnectorTo.Position.X + ConnectorTo.Width / 2),
                 trunc(ModuleTo.ScrollPosY + ConnectorTo.Position.Y + ConnectorTo.Height / 2));
  PaintElements;

//  (FFromConnector.Module as TG2GraphModule).Parent.Invalidate;
end;

// === Form ====================================================================

procedure TForm1.FormCreate(Sender: TObject);
begin
  FG2 := TG2Graph.Create(self);
  FG2.FLayoutVA := LayoutVA;
  FG2.FLayoutFX := LayoutFX;
//  FG2.FForm := self;
//  FG2.ScrollboxVA := sbVA;
//  FG2.scrollboxFX := sbFX;
  FG2.LoadModuleDefs('');
  FPatch := FG2.Performance.Slot[0].Patch as TG2GraphPatch;
  //FPatch := TG2GraphPatch.Create(self);
  //FPatch.FG2 := FG2;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  //FPatch.Free;
  FG2.Free;
end;

procedure TForm1.MenuItem2Click(Sender: TObject);
var aPatch : TG2FilePatch;
    PatchName : string;
    i : integer;
    FileStream : TFileStream;
    aFileName : string;
begin
  if OpenDialog1.Execute then begin
    aFileName := OpenDialog1.FileName;

    FileStream := TFileStream.Create( aFileName, fmOpenRead);
    try
      // Take the patchname from the filename
      aFilename := ExtractFilename( aFileName);

      // Name patch max size = 16, if shorter end with 0
      PatchName := '';
      i := 1;
      while (i<=Length( aFileName)) and (i<=16) and ( aFileName[i] <> '.') do begin
        PatchName := PatchName + aFileName[i];
        inc(i);
      end;

      {aPatch := TG2FilePatch.Create(FG2USB);
      try
        if aPatch.LoadFromFile(FileStream, FG2USB.LogLines.Lines) then
          USBUploadPatch(PatchName, aPatch);
      finally
        aPatch.Free;
      end;}
      FPatch.Init;
      FPatch.LoadFromFile(FileStream, nil);
      FPatch.Visible := True;

      InitPatchLocations;
    finally
      FileStream.Free;
    end;
  end;
end;


procedure TForm1.InitPatchLocations;
var i, max_col, max_row : integer;
begin
  max_col := 0;
  max_row := 0;
  for i := 0 to FPatch.ModuleCount[ord(ltVA)] - 1 do begin
    if FPatch.ModuleList[ord(ltVA)][i].Row > max_row then
      max_row := FPatch.ModuleList[ord(ltVA)][i].Row;

    if FPatch.ModuleList[ord(ltVA)][i].Col > max_col then
      max_col := FPatch.ModuleList[ord(ltVA)][i].Col;
  end;

  LayoutZoomVA.Position.X := 0;
  LayoutZoomVA.Position.Y := 0;
  LayoutVA.Position.X := 0;
  LayoutVA.Position.Y := 0;
  LayoutVA.Width := (max_col + 2) * UNITS_COL;
  LayoutVA.Height := (max_row + 6) * UNITS_ROW;
  LayoutZoomVA.Width := LayoutVa.Width * SmallScrollbar1.Value;
  LayoutZoomVA.Height := LayoutVa.Height * SmallScrollbar1.Value;

  max_col := 0;
  max_row := 0;
  for i := 0 to FPatch.ModuleCount[ord(ltFX)] - 1 do begin
    if FPatch.ModuleList[ord(ltFX)][i].Row > max_row then
      max_row := FPatch.ModuleList[ord(ltFX)][i].Row;

    if FPatch.ModuleList[ord(ltFX)][i].Col > max_col then
      max_col := FPatch.ModuleList[ord(ltFX)][i].Col;
  end;

  LayoutZoomFX.Position.X := 0;
  LayoutZoomFX.Position.Y := 0;
  LayoutFX.Position.X := 0;
  LayoutFX.Position.Y := 0;
  LayoutFX.Width := (max_col + 2) * UNITS_COL;
  LayoutFX.Height := (max_row + 6) * UNITS_ROW;
  LayoutZoomFX.Width := LayoutFX.Width * SmallScrollbar1.Value;
  LayoutZoomFX.Height := LayoutFX.Height * SmallScrollbar1.Value;

end;

procedure TForm1.SmallScrollBar1Change(Sender: TObject);
begin
  LayoutVa.Scale.X := SmallScrollbar1.Value;
  LayoutVa.Scale.Y := SmallScrollbar1.Value;
  LayoutZoomVA.Width := LayoutVa.Width * SmallScrollbar1.Value;
  LayoutZoomVA.Height := LayoutVa.Height * SmallScrollbar1.Value;

  LayoutFX.Scale.X := SmallScrollbar1.Value;
  LayoutFX.Scale.Y := SmallScrollbar1.Value;
  LayoutZoomFX.Width := LayoutVa.Width * SmallScrollbar1.Value;
  LayoutZoomFX.Height := LayoutVa.Height * SmallScrollbar1.Value;

end;

procedure TForm1.Splitter1Click(Sender: TObject);
begin

end;

{ TG2GraphSlot }

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

{ TG2GraphPerformance }

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

end.
