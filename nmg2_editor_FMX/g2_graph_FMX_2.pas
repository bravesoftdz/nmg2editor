unit g2_graph_FMX_2;

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


interface
uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Contnrs, System.UIConsts, System.Math, System.Generics.Defaults,
  System.Generics.Collections,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.Layouts,
  g2_types, g2_file, g2_usb,
  BVE.SVGControl, BVE.SVGXMLWrapperDelphi;

type
  TG2GraphModuleFMX = class;
  TModuleBitmapBuffer = class;

  TCreateModuleFMXEvent = procedure(Sender : TObject; Module : TG2GraphModuleFMX) of Object;

  TG2ParamEvent = (g2pSet, g2pChange);

  TBtnType = (btToggle, btMomentary);

  TG2GraphFMX = class( TG2USB)
  private
    FModulePanels : TObjectList;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    function    CreatePerformance : TG2FilePerformance; override;
    procedure   ParseModulePanels;
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

  TG2MiniVU = class;
  TG2LedGroup = class;
  TG2LedGreen = class;
  TG2Led = class;

  TG2GraphPatchFMX = class( TG2USBPatch)
  // This represents the patch
  private
    // Lists for the leds
    FMiniVUList         : TList<TG2MiniVU>;
    FLedGroupList       : TList<TG2LedGroup>;
    FLed39List          : TList<TG2LedGreen>;
    FLed3AList          : TList<TG2Led>;
    FVisible            : boolean;
    FSelectedControl    : TControl;
    FSelectedMorphIndex : integer;

    FLayout             : TModuleBitmapBuffer;

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

    procedure   SetMiniVULevel( Index : integer; aValue : byte); override;
    procedure   SetLedLevel( Index : integer; aValue : byte); override;
    function    GetMiniVUListCount : integer; override;
    function    GetLedListCount : integer; override;
    procedure   SortLeds; override;
    procedure   RemoveFromLedList( aLocation: TLocationType; aModuleIndex : integer); override;

    property    Visible : boolean read FVisible write SetVisible;
    property    SelectedControl : TControl read FSelectedControl write SetSelectedControl;
    property    SelectedMorphIndex : integer read FSelectedMorphIndex write SetSelectedMorphIndex;
    property    Layout : TModuleBitmapBuffer read FLayout write FLayout;

    property    OnCreateModuleFMX : TCreateModuleFMXEvent read FOnCreateModuleFMX write FOnCreateModuleFMX;
  end;


  TG2Control = class;
  TG2Module = class;

  TG2GraphModuleFMX = class( TG2FileModule)
  private
    FSVGControl : TG2Module;

    function    GetParent: TFMXObject;
    procedure   SetParent(const Value: TFMXObject);
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
    procedure   ParsePanelData;
    procedure   SetCol( aValue : TBits7); override;
    procedure   SetRow( aValue : TBits7); override;
    procedure   SetModuleColor( aValue : TBits8); override;
    procedure   SetModuleName( aValue : AnsiString); override;

    property    SVGControl : TG2Module read FSVGControl write FSVGControl;
    property    Parent: TFMXObject read GetParent write SetParent;
  end;

  TG2GraphParameterFMX = class(TG2FileParameter)
  private
    FControlList  : array of TG2Control; // array of controls the parameter is assigned to
  public
    constructor Create( aPatch : TG2FilePatch; aLocation : TLocationType; aModuleIndex : integer; aModule : TG2FileModule);
    destructor  Destroy; override;
    //procedure   NotifyControls( Event: TG2ParamEvent; Info: NativeInt);
    procedure   AssignControl( aControl : TG2Control);
    procedure   DeassignControl( aControl : TG2Control);
    procedure   InvalidateControl; override;
  end;

  TG2Cable = class;
  TG2CableV2 = class;

  TG2GraphCableFMX = class( TG2FileCable)
  protected
    FParent         : TFmxObject;
    FPatch          : TG2GraphPatchFMX;
    FModule         : TG2GraphModuleFMX;
    FSVGControl     : TG2CableV2;
    FSelected       : boolean;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   InitCable;
    procedure   ConnectorMoved; override;
    procedure   SetParent( aValue : TFmxObject);

    property    GraphControl : TG2CableV2 read FSVGControl write FSVGControl;
    property    Parent : TFmxObject read FParent write SetParent;
  end;

  TG2Shape = class(TControl)
  protected
    function GetControlRect: TRectF;
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;

    property ControlRect : TRectF read GetControlRect;
  end;

  TG2Control = class(TG2Shape)
  private
    FModuleData : TG2GraphModuleFMX;
    FParameter : TG2GraphParameterFMX;
    FID,
    FZOrder,
    FCodeRef,
    FInfoFunc : integer;
    FSelected : boolean;
    FValue : integer;
    FNormValue : single;
  protected
    function GetValue: integer;
    procedure SetValue(const aValue: integer);
    function GetMorphValue: byte;
    procedure SetParamLabel( aIndex : integer; aValue : AnsiString);
    function GetParamLabel( aIndex : integer) : AnsiString;
    procedure SetNormValue(const aValue: single);
    function GetNormValue: single;
    function GetNormParamValue: single;
    procedure SetSelected(const aValue: boolean); virtual;
    procedure SetParameter(const aValue: TG2GraphParameterFMX); virtual;
    procedure SetModuleData(const aValue : TG2GraphModuleFMX); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ParamEvent( Event: TG2ParamEvent; Info: NativeInt); virtual;
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    procedure ParsePanelData( fs : TModuleDefStream); virtual;
    function ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean; virtual;

    procedure Paint; override;

    procedure ProcessMouseDown(Shift: TShiftState; AbsX, AbsY, X, Y: Single); virtual;
    procedure ProcessMouseMove(Shift: TShiftState; AbsX, AbsY, X, Y, dX, dY: Single); virtual;
    procedure ProcessMouseUp(Shift: TShiftState; AbsX, AbsY, X, Y: Single); virtual;

    procedure IncValue;
    procedure DecValue;
    procedure RotateValue;

    property Selected : boolean read FSelected write SetSelected;
    property Parameter : TG2GraphParameterFMX read FParameter write SetParameter;
    property ParamLabel[ Index : integer]: AnsiString read GetParamLabel write SetParamLabel;
    property ModuleData : TG2GraphModuleFMX read FModuleData write SetModuleData;
    property NormValue : single read GetNormValue write SetNormValue;
    property Value : integer read GetValue write SetValue;
    property MorphValue : byte read GetMorphValue;
  end;

  TG2Led = class(TG2Control)
  private
    FGroupId : integer;
    FType    : TLedType;
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetLevel( aValue : byte); virtual;
  end;

  TG2LedGreen = class(TG2Led)
  private
    FCodeRef     : integer;
    FInfoFunc    : integer;
    FLevel       : byte;
    FLedColor    : TColor;
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ParsePanelData(fs: TModuleDefStream); override;
    function ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean; override;

    procedure paint; override;

    procedure SetLevel( aValue : byte); override;
  end;

  TG2LedGroup = class( TG2Led)
  private
    FLeds : TList<TG2LedGreen>;
    FLedOn : integer;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   SetLevel( aValue : byte); override;
  end;

  TG2MiniVU = class( TG2Led)
  private
    FMiniVUWidth,
    FMiniVUHeight : integer;
    FCodeRef      : integer;
    FInfoFunc     : integer;
    FOrientation  : TOrientationType;
    FLevel        : byte;
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Paint; override;

    procedure ParsePanelData( fs : TModuleDefStream); override;
    function ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean; override;

    procedure SetLevel( aValue : byte); override;
  end;

  TG2Connector = class(TG2Control)
  private
    FHoleRect : TRectF;
    FData : TG2FileConnector;
    FTempCable : TG2CableV2;
  protected
    procedure SetData( aValue: TG2FileConnector);
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ProcessMouseDown(Shift: TShiftState; AbsX, AbsY, X, Y: Single); override;
    procedure ProcessMouseMove(Shift: TShiftState; AbsX, AbsY, X, Y, dX, dY: Single); override;
    procedure ProcessMouseUp(Shift: TShiftState; AbsX, AbsY, X, Y: Single); override;

    procedure Paint; override;

    procedure ParsePanelData( fs : TModuleDefStream); override;
    function ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean; override;

    property Data : TG2FileConnector read FData write SetData;
  end;

  TG2Label = class(TG2Control)
  private
    FCaption : string;
    FFont : TFont;
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;

    function ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean; override;

    procedure paint; override;
  end;

  TG2TextField = class(TG2Control)
  private
    FTextFunction : integer;
    FFont : TFont;
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;

    function ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean; override;

    procedure paint; override;
  end;

  TG2Button = class(TG2Control)
  private
    FButtonText : TStringList;
    FBitmapData : TStringList;
    FFont : TFont;
    FOrientation : TOrientationType;
    FButtonCount : integer;
    FButtonWidth : single;
    FButtonHeight : single;
    FImageCount : integer;
    FImageWidth : single;
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;

    function ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean; override;

    procedure paint; override;
  end;

  TG2BtnText = class(TG2Button)
  private
    FButtonTextType : TButtonTextType;
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ParsePanelData(fs: TModuleDefStream); override;
    function ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean; override;

    procedure ProcessMouseUp(Shift: TShiftState; AbsX, AbsY, X, Y: Single); override;

    procedure paint; override;
  end;

  TG2BtnFlat = class(TG2Button)
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ParsePanelData(fs: TModuleDefStream);

    procedure ProcessMouseUp(Shift: TShiftState; AbsX, AbsY, X, Y: Single); override;

    procedure paint; override;
  end;

  TG2BtnRadio = class(TG2Button)
  private
    FRectArray : array of TRectF;
    FUpsideDown : boolean;
    FBtnSelected : integer;
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ParsePanelData(fs: TModuleDefStream); override;
    function ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean; override;

    procedure ProcessMouseUp(Shift: TShiftState; AbsX, AbsY, X, Y: Single); override;

    procedure paint; override;
  end;

  TG2BtnRadioEdit = class(TG2BtnRadio)
  private
    FButtonColumns : integer;
    FButtonRows : integer;
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ParsePanelData(fs: TModuleDefStream); override;
    function ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean; override;
  end;

  TG2BtnIncDec = class(TG2BtnRadio)
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ProcessMouseDown(Shift: TShiftState; AbsX, AbsY, X, Y: Single); override;
    procedure ProcessMouseUp(Shift: TShiftState; AbsX, AbsY, X, Y: Single); override;

    function ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean; override;
  end;

  TG2PartSelector = class( TG2Control)
  private
    FBtnRect,
    FDisplayRect,
    FCollapsedRect,
    FExpandedRect : TRectF;
    FRectArray : array of TRectF;
    FBitmapData : TStringList;
    FImageWidth  : integer;
    FImageCount  : integer;
    FShowOptions : boolean;
    FOptionSelected : integer;
  protected
    procedure DoMouseLeave; override;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure paint; override;

    procedure ParsePanelData( fs : TModuleDefStream); override;
    function ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean; override;

    procedure ProcessMouseMove(Shift: TShiftState; AbsX, AbsY, X, Y, dX, dY: Single); override;
    procedure ProcessMouseUp(Shift: TShiftState; AbsX, AbsY, X, Y: Single); override;
  end;

  TG2KnobButtons = class(TG2Shape)
  private
    FBtnWidth,
    FBtnHeight,
    FBorderWidth : single;
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;

    procedure paint; override;
  end;

  TG2ResetButton = class(TG2Shape)
  private
    FCentered : boolean;
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;

    procedure paint; override;
  end;

  TG2Knob = class(TG2Control)
  private
    FCX, FCY, FR : single;
    FKnobType : TKnobType;
    FKnobBtns : TG2KnobButtons;
    FKnobReset : TG2ResetButton;
    procedure SetKnobType(const Value: TKnobType);
  protected
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;
    procedure SetSelected(const aValue: boolean); override;
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;

    function ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean; override;

    procedure paint; override;

    procedure ProcessMouseMove(Shift: TShiftState; AbsX, AbsY, X, Y, dX, dY: Single); override;
    procedure ProcessMouseUp(Shift: TShiftState; AbsX, AbsY, X, Y: Single); override;

    property KnobType : TKnobType read FKnobType write SetKnobType;
  end;

  TCableStyle = ( csFlat, csGradient);

  TBitmapRect = class
  private
    FRect : TRectF;
    FBitmap : TBitmap;
    FZoom : single;
  protected
    function Getactive: boolean;
    procedure SetActive( aValue : boolean);
    procedure SetZoom( aValue : single);
  public
    constructor Create;
    destructor Destroy; override;

    property Active : boolean read GetActive write Setactive;
    property Rect : TRectF read FRect write FRect;
    property Zoom : single read FZoom write SetZoom;
    property Bitmap : TBitmap read FBitmap write FBitmap;
  end;

  TCableBitmapBuffer = class(TControl)
  private
    FBitmapList : TObjectList;
    Fdx, Fdy : single;
    FCols, FRows : integer;
    FCableList : TCableList;
    FRedrawBuffer : boolean;
    FZoom : single;
    FCableStyle : TCableStyle;
  protected
    procedure SetZoom( aValue : single);
    procedure SetCableStyle( aValue : TCableStyle);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure Clear;
    procedure Resize; override;

    property dX : single read Fdx;
    property dY : single read Fdy;
    property CableList : TCableList read FCableList write FCableList;
    property RedrawBuffer : boolean read FRedrawBuffer write FRedrawBuffer;
    property Zoom : single read FZoom write SetZoom;
    property CableStyle : TCableStyle read FCableStyle write SetCableStyle;
  end;

  TNode = class
    x  : single;
    y  : single;
    vx : single;
    vy : single;
  end;

  TG2Cable = class(TControl)
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
    procedure ClearNodes;
    procedure AddNodes( aNodeCount : integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure PaintBuffer( aBuffer : TCableBitmapBuffer);
    procedure StartTimer;
    procedure InitCable;
  published
    property Point1X : single read FP1.X write SetPoint1X;
    property Point1Y : single read FP1.Y write SetPoint1Y;
    property Point2X : single read FP2.X write SetPoint2X;
    property Point2Y : single read FP2.Y write SetPoint2Y;
  end;

  TG2CableNode = class(TControl)
  private
    x, x2  : single;
    y, y2  : single;
    vx : single;
    vy : single;
    PStart, PEnd : TPointF;
    FCable : TG2CableV2;
    FPolygon : TPolygon;
    FStartNode,
    FEndNode : boolean;
  public
    constructor Create(aCable: TG2CableV2);
    destructor Destroy; override;
    procedure CalcPath;
    procedure Paint; override;
  end;

  TG2CableV2 = class(TComponent)
  private
    FParent : TFmxObject;
    FData : TG2GraphCableFMX;
    FX, FY, FWidth, FHeight : single;
    FP1, FP2 : TPointF;
    FColor : TAlphaColor;
    FNodeCount : integer;
    FNodes : array of TG2CableNode;
    FMargin : single;
    FTimer : TTimer;
    FTimerCount : integer;
  protected
    function GetParent: TFmxObject;
    procedure SetParent(const Value: TFmxObject);
    procedure SetPoint1X(const Value : single);
    procedure SetPoint1Y(const Value : single);
    procedure SetPoint2X(const Value : single);
    procedure SetPoint2Y(const Value : single);
    procedure IterateCable;
    procedure OnTimer(Sender: TObject);
    procedure ClearNodes;
    procedure AddNodes( aNodeCount : integer);
    procedure CalcNodePaths;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure StartTimer;
    procedure InitCable;
  published
    property Parent : TFmxObject read GetParent write SetParent;
    property Point1X : single read FP1.X write SetPoint1X;
    property Point1Y : single read FP1.Y write SetPoint1Y;
    property Point2X : single read FP2.X write SetPoint2X;
    property Point2Y : single read FP2.Y write SetPoint2Y;
  end;

  TG2Module = class(TRectangle)
  private
    FPanel : TSVGPath;
    FModuleData : TG2GraphModuleFMX;
    //FBitmap : TBitmap;
    //FZoom : single;
    //FRedrawBuffer : boolean;

    FOnControlMouseUp: TMouseEvent;
    FOnControlMouseDown: TMouseEvent;
    FOnControlMouseMove: TMouseMoveEvent;

    procedure SetModuleData(const aValue: TG2GraphModuleFMX);
  protected
    procedure   SetZoom( aValue : single);
    procedure   Resize; override;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure  Assign(Source: TPersistent); override;

    procedure   Paint; override;
    procedure   Redraw;

    //procedure   AddGraphControl( aGraphCtrl : TG2Control);
    function    NewG2GraphControl( aControlType : AnsiString) : TG2Control;
    function    GetControlType( aG2GraphChildControl: TG2Control): string;

    procedure   ParsePanelData;
    procedure   ParseDependencies( aControl : TG2Control; aMasterRef : integer; aDependencies : string; aTextFunction : integer);

    function    GetNewCol: TBits7;
    function    GetNewRow: TBits7;
    procedure   SetModuleColor( aValue : TBits8);

    procedure   SetSelected( aValue : boolean);

    property    ModuleData : TG2GraphModuleFMX read FModuleData write SetModuleData;
    //property    Zoom : single read FZoom write SetZoom;

    property OnControlMouseUp: TMouseEvent read FOnControlMouseUp write FOnControlMouseUp;
    property OnControlMouseDown: TMouseEvent read FOnControlMouseDown write FOnControlMouseDown;
    property OnControlMouseMove: TMouseMoveEvent read FOnControlMouseMove write FOnControlMouseMove;
  end;

  TModuleBitmapBuffer = class(TControl)
  private
    FBackGround : TSVGGroup;
    FBGBuffer : TBitmap;
    FModuleList : TModuleList;
    FZoom : single;
    FMaxCol, FMaxRow : integer;

    FOnControlMouseUp: TMouseEvent;
    FOnControlMouseDown: TMouseEvent;
    FOnControlMouseMove: TMouseMoveEvent;

    FOnModuleMouseUp: TMouseEvent;
    FOnModuleMouseDown: TMouseEvent;
    FOnModuleMouseMove: TMouseMoveEvent;
  protected
    function GetMaxCol: integer;
    function GetMaxRow: integer;
    procedure SetZoom( aValue : single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Paint; override;

    procedure CalcBounds;

    property ModuleList : TModuleList read FModuleList write FModuleList;
    property Zoom : single read FZoom write SetZoom;
    property MaxCol : integer read GetMaxCol;
    property MaxRow : integer read GetMaxRow;

    property OnControlMouseUp: TMouseEvent read FOnControlMouseUp write FOnControlMouseUp;
    property OnControlMouseDown: TMouseEvent read FOnControlMouseDown write FOnControlMouseDown;
    property OnControlMouseMove: TMouseMoveEvent read FOnControlMouseMove write FOnControlMouseMove;

    property OnModuleMouseUp: TMouseEvent read FOnModuleMouseUp write FOnModuleMouseUp;
    property OnModuleMouseDown: TMouseEvent read FOnModuleMouseDown write FOnModuleMouseDown;
    property OnModuleMouseMove: TMouseMoveEvent read FOnModuleMouseMove write FOnModuleMouseMove;
  end;

function SubTextReplace( aText, aSubText, aReplaceText : string): string;
function DecodeButtonStateElement( aValue : string; var aName, aState, aSize : string): boolean;
function DecodeModuleControlElement( aValue : string; var aModuleID, aCtrlID, aIndex : integer): boolean;

var
  SVGAgent : TSVGAgent;

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

function ConvertToAlpha( aColor : integer): integer;
begin
  Result := $ff000000
          + (aColor and $000000ff) shl 16
          + (aColor and $0000ff00)
          + (aColor and $00ff0000) shr 16;
end;

function SubtractText( aSubstr, aStr : string): string;
var p : integer;
begin
  p := pos(aSubstr, aStr);
  if p > 0 then
    Result := copy(aStr, p + Length(aSubstr), Length(aStr) - Length(aSubstr))
  else
    Result := aStr;
end;

function GetDelimitedText( aStr : string; aDelimiter : char; var aEndOfStr : boolean): string;
var i : integer;
begin
  Result := '';
  i := 1;
  while (i<=length(aStr)) and (aStr[i]<>aDelimiter) do begin
    Result := Result + aStr[i];
    inc(i);
  end;
  aEndOfStr := not(i<length(aStr));
end;

function ExtractValueStr( var aStr : string; aName : string; aDelimiter : char; var aEndOfStr : boolean; var aValue : string): boolean;
var p : integer;
    temp : string;
begin
  Result := True;
  p := pos(aName, aStr);
  if p = 0 then begin
    Result := False;
    exit;
  end;

  aStr := SubtractText(aName, aStr);
  temp := GetDelimitedText( aStr, aDelimiter, aEndOfStr);

  if not(aEndOfStr) then
    aStr := SubtractText(temp + '_', aStr)
  else
    aStr := SubtractText(temp, aStr);

  aValue := temp;
end;

function ExtractValueInt( var aStr : string; aName : string; aDelimiter : char; var aEndOfStr : boolean; var aValue : integer): boolean;
var p, c : integer;
    temp : string;
begin
  Result := True;
  p := pos(aName, aStr);
  if p = 0 then begin
    Result := False;
    exit;
  end;

  aStr := SubtractText(aName, aStr);
  temp := GetDelimitedText( aStr, aDelimiter, aEndOfStr);

  if not(aEndOfStr) then
    aStr := SubtractText(temp + '_', aStr)
  else
    aStr := SubtractText(temp, aStr);

  val(temp, aValue, c);
  if c>0 then begin
    Result := False;
    exit;
  end;
end;

function DecodeButtonStateElement( aValue : string; var aName, aState, aSize : string): boolean;
var EndOfStr : boolean;
begin
  Result := True;
  aValue := Lowercase(aValue);

  aName := GetDelimitedText(aValue, '_', EndOfStr);
  if not(EndOfStr) then
    aValue := SubtractText(aName + '_', aValue)
  else
    aValue := SubtractText(aName, aValue);

  aState := GetDelimitedText(aValue, '_', EndOfStr);
  if not(EndOfStr) then
    aValue := SubtractText(aState + '_', aValue)
  else
    aValue := SubtractText(aState, aValue);

  aSize := GetDelimitedText(aValue, '_', EndOfStr);
  if not(EndOfStr) then
    aValue := SubtractText(aSize + '_', aValue)
  else
    aValue := SubtractText(aSize, aValue);
end;

function DecodeModuleControlElement( aValue : string; var aModuleID, aCtrlID, aIndex : integer): boolean;
var EndOfStr : boolean;
begin
  Result := True;
  aValue := Lowercase(aValue);

  if not ExtractValueInt(aValue, 'module_', '_', EndOfStr, aModuleID) then begin
    Result := False;
    exit;
  end;

  if not ExtractValueInt(aValue, 'ctrl_', '_', EndOfStr, aCtrlID) then begin
    Result := False;
    exit;
  end;

  if EndOfStr then begin
    aIndex := 0;
    exit;
  end else begin
    if not ExtractValueInt(aValue, 'el_', '_', EndOfStr, aIndex) then begin
      Result := False;
      exit;
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
  FModulePanels := TObjectList.Create(False); // Owner of panels is TG2GraphFMX
end;

destructor TG2GraphFMX.Destroy;
begin
  FModulePanels.Free;
  inherited;
end;

function TG2GraphFMX.CreatePerformance: TG2FilePerformance;
begin
  Result := TG2GraphPerformanceFMX.Create(self);
end;

procedure TG2GraphFMX.ParseModulePanels;
var i : integer;
    id : string;
    SVGModule : TG2Module;
begin
  if assigned(SVGAgent) then begin
    for i := 0 to FModuleDefList.Count - 1 do begin
      id := 'module_' + IntToStr(FModuleDefList.ModuleDef[i].ModuleType);
      SVGModule := TG2Module.Create( self);
      FModulePanels.Add( SVGModule);
    end;
  end;
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
var MiniVUComparison : TComparison<TG2MiniVU>;
    LedGreenComparison : TComparison<TG2LedGreen>;
    LedGroupComparison : TComparison<TG2LedGroup>;
    Led3AComparison : TComparison<TG2Led>;
begin
  MiniVUComparison :=
    function(const Led1, Led2: TG2MiniVU): integer
    begin
      if Led1.ModuleData.Location > Led2.ModuleData.Location then
        Result := -1
      else
        if Led1.ModuleData.Location =Led2.ModuleData.Location then begin
          if Led1.ModuleData.ModuleIndex > Led2.ModuleData.ModuleIndex then
            Result := 1
          else
            if Led1.ModuleData.ModuleIndex = Led2.ModuleData.ModuleIndex then begin
              if Led1.FGroupId > Led2.FGroupID then
                Result := 1
              else
                if Led1.FGroupId = Led2.FGroupID then
                  Result := 0
                else
                  Result := -1;
            end else
              Result := -1;
        end else
          Result := 1;
    end;

  LedGreenComparison :=
    function (const Led1, Led2 : TG2LedGreen): integer
    begin
      if Led1.ModuleData.Location > Led2.ModuleData.Location then
        Result := -1
      else
        if Led1.ModuleData.Location =  Led2.ModuleData.Location then begin
          if Led1.ModuleData.ModuleIndex > Led2.ModuleData.ModuleIndex then
            Result := 1
          else
            if Led1.ModuleData.ModuleIndex = Led2.ModuleData.ModuleIndex then begin
              if Led1.FGroupId > Led2.FGroupID then
                Result := 1
              else
                if Led1.FGroupId = Led2.FGroupID then begin
                  if Led1.FCodeRef > Led2.FCodeRef then
                    Result := 1
                  else
                    if Led1.FCodeRef = Led2.FCodeRef then
                      Result := 0
                    else
                      Result := -1;
                end else
                  Result := -1;
            end else
              Result := -1;
        end else
          Result := 1;
    end;

  LedGroupComparison :=
    function(const Led1, Led2: TG2LedGroup): integer
    begin
      if Led1.ModuleData.Location > Led2.ModuleData.Location then
        Result := -1
      else
        if Led1.ModuleData.Location =Led2.ModuleData.Location then begin
          if Led1.ModuleData.ModuleIndex > Led2.ModuleData.ModuleIndex then
            Result := 1
          else
            if Led1.ModuleData.ModuleIndex = Led2.ModuleData.ModuleIndex then begin
              if Led1.FGroupId > Led2.FGroupID then
                Result := 1
              else
                if Led1.FGroupId = Led2.FGroupID then
                  Result := 0
                else
                  Result := -1;
            end else
              Result := -1;
        end else
          Result := 1;
    end;

  Led3AComparison :=
    function(const Led1, Led2: TG2Led): integer
    begin
      if Led1.ModuleData.Location > Led2.ModuleData.Location then
        Result := -1
      else
        if Led1.ModuleData.Location =Led2.ModuleData.Location then begin
          if Led1.ModuleData.ModuleIndex > Led2.ModuleData.ModuleIndex then
            Result := 1
          else
            if Led1.ModuleData.ModuleIndex = Led2.ModuleData.ModuleIndex then begin
              if Led1.FGroupId > Led2.FGroupID then
                Result := 1
              else
                if Led1.FGroupId = Led2.FGroupID then
                  Result := 0
                else
                  Result := -1;
            end else
              Result := -1;
        end else
          Result := 1;
    end;

  FMiniVUList := TList<TG2MiniVU>.Create(TComparer<TG2MiniVU>.Construct(MiniVUComparison));
  FLedGroupList := TList<TG2LedGroup>.Create(TComparer<TG2LedGroup>.Construct(LedGroupComparison));
  FLed39List := TList<TG2LedGreen>.Create(TComparer<TG2LedGreen>.Construct(LedGreenComparison));
  FLed3AList := TList<TG2Led>.Create(TComparer<TG2Led>.Construct(Led3AComparison));

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

      if (G2.ClientType <> ctVST) and assigned(Layout) then begin
        if aLocation = ltVA then begin
          Module.Parent := Layout;
          Module.FSVGControl.OnControlMouseUp := Layout.OnControlMouseUp;
          Module.FSVGControl.OnControlMouseDown := Layout.OnControlMouseDown;
          Module.FSVGControl.OnControlMouseMove := Layout.OnControlMouseMove;
          Module.FSVGControl.OnMouseUp := Layout.OnModuleMouseUp;
          Module.FSVGControl.OnMouseDown := Layout.OnModuleMouseDown;
          Module.FSVGControl.OnMouseMove := Layout.OnModuleMouseMove;
          Module.ParsePanelData;
          SortLeds;
        end else
          {if aLocation = ltFX then begin
            Module.Parent := (G2 as TG2Graph).FLayoutFX;
            Module.ParsePanelData;
            SortLeds;
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
    ConnectorFrom, ConnectorTo : TG2Connector;
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

  // If Linktype is 1 then the first connector is an output, else it's an input
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
      ConnectorFrom := Cable.FromConnector.GraphControl as TG2Connector;
      ConnectorTo := Cable.ToConnector.GraphControl as TG2Connector;
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

procedure TG2GraphPatchFMX.SetSelectedControl(aValue: TControl);
begin

end;

procedure TG2GraphPatchFMX.SetSelectedMorphIndex(aValue: integer);
begin

end;

procedure TG2GraphPatchFMX.SetVisible(aValue: boolean);
begin

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
var i : integer;
begin
  // When a module is deleted, it's leds must be removed from the lists

  i := 0;
  while (i < FMiniVUList.Count) do begin
     if (FMiniVUList.Items[i].ModuleData.ModuleIndex = aModuleIndex) and
        (FMiniVUList.Items[i].ModuleData.Location = aLocation) then
      FMiniVUList.Delete(i)
    else
      inc(i);
  end;

  i := 0;
  while (i < FLedGroupList.Count) do begin
    if (FLedGroupList.Items[i].ModuleData.ModuleIndex = aModuleIndex) and
       (FLedGroupList.Items[i].ModuleData.Location = aLocation) then
      FLedGroupList.Delete(i)
    else
      inc(i);
  end;

  SortLeds;
end;

procedure TG2GraphPatchFMX.SetLedLevel(Index: integer; aValue: byte);
begin
  FLed39List[Index].SetLevel( aValue)
end;

procedure TG2GraphPatchFMX.SetMiniVULevel(Index: integer; aValue: byte);
begin
  FLed3AList[Index].SetLevel( aValue)
end;

procedure TG2GraphPatchFMX.SortLeds;
var i : integer;
begin
  // Take leds that are in a led goup with only one led out of the group list
  // and put them in de led list.
  // These leds are addressed in message $39
  // VU-meters and ledgroups with more than 1? led are addressed in message $3A

  FLed39List.Clear;
  FLed3AList.Clear;

  i := 0;
  while i < FLedGroupList.Count do begin

    if FLedGroupList[i].FLeds.Count = 1 then begin
      // Add single Leds to the Led39List
      FLed39List.Add( FLedGroupList[i].FLeds[0])
    end else
      // Add grouped Leds to the Led3AList
      FLed3AList.Add( FLedGroupList[i]);

    inc(i);
  end;

  // Add the VU meters to the Led3AList
  for i := 0 to FMiniVUList.Count - 1 do
    FLed3AList.Add( FMiniVUList[i]);

  FLed3AList.Sort;
  FLed39List.Sort;
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

procedure TG2GraphParameterFMX.AssignControl(aControl: TG2Control);
var i : integer;
begin
  if not(aControl is TG2Control) then
    raise Exception.Create('Only a TG2GraphControlFMX can be assigned to a parameter.');

  i := 0;
  while (i < Length(FControlList)) and not(FControlList[i] = aControl) do
    inc(i);

  if not(i < Length(FControlList)) then begin
    SetLength(FControlList, i + 1);
    FControlList[i] := aControl;
  end;
end;

procedure TG2GraphParameterFMX.DeassignControl(aControl: TG2Control);
var i, j : integer;
begin
  if not(aControl is TG2Control) then
    raise Exception.Create('Only a TG2GraphControlFMX can be assigned to a parameter.');

  i := 0;
  while (i < Length(FControlList)) and not(FControlList[i] = aControl) do
    inc(i);

  if (i < Length(FControlList)) then begin
    FControlList[i].Parameter := nil;
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
    (FControlList[i] as TG2Control).ParamEvent( g2pChange, 0);
  end;
end;


//==============================================================================
//
//                               TG2Shape
//
//==============================================================================

constructor TG2Shape.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TG2Shape.Destroy;
begin

  inherited;
end;

function TG2Shape.GetControlRect: TRectF;
begin
  Result := RectF(Position.X, Position.Y, Position.X + Width, Position.Y + Height);
end;

//==============================================================================
//
//                               TG2Control
//
//==============================================================================

constructor TG2Control.Create( AOwner: TComponent);
begin
  inherited;
  FParameter := nil;
  Width := 10;
  Height := 10;
  FID := -1;
  FZOrder := 0;
  FCodeRef := -1;
  FInfoFunc := -1;
  FSelected := False;
  FValue := 0;
  FNormValue := 0;
end;

procedure TG2Control.DecValue;
begin
  if assigned(FParameter) then begin
    if FParameter.GetParameterValue > FParameter.LowValue then
      FParameter.SetParameterValue( FParameter.GetParameterValue - 1)
    else
      FParameter.SetParameterValue( FParameter.LowValue);
    FNormValue := GetNormParamValue;
    FValue := FParameter.GetParameterValue;
    Repaint;
  end;
end;

procedure TG2Control.IncValue;
begin
  if assigned(FParameter) then begin

    if FParameter.GetParameterValue < FParameter.HighValue then
      FParameter.SetParameterValue( FParameter.GetParameterValue + 1)
    else
      FParameter.SetParameterValue( FParameter.HighValue);
    FNormValue := GetNormParamValue;
    FValue := FParameter.GetParameterValue;
    Repaint;
  end;
end;


procedure TG2Control.RotateValue;
begin
  if assigned(FParameter) then begin

    if FParameter.GetParameterValue < FParameter.HighValue then
      FParameter.SetParameterValue( FParameter.GetParameterValue + 1)
    else
      FParameter.SetParameterValue( FParameter.LowValue);
    FNormValue := GetNormParamValue;
    FValue := FParameter.GetParameterValue;
    Repaint;
  end;
end;

destructor TG2Control.Destroy;
begin
  inherited;
end;

procedure TG2Control.Assign(Source: TPersistent);
begin
  inherited;
end;

function TG2Control.GetMorphValue: byte;
begin
  if assigned( FParameter) then
    Result := FParameter.GetSelectedMorphValue
  else
    Result := 0;
end;

function TG2Control.GetNormParamValue: single;
var h, l, v : byte;
begin
  if assigned( FParameter) then begin
    h := FParameter.HighValue;
    l := FParameter.LowValue;
    v := FParameter.GetParameterValue;
    if (h-l) <> 0 then
      Result := (v-l)/(h-l)
    else
      Result := 0;
  end else
    Result := 0;
end;

function TG2Control.GetParamLabel(aIndex: integer): AnsiString;
begin
  if assigned( FParameter) then
    Result := FParameter.ParamLabel[ aIndex]
  else
    Result := '';
end;

function TG2Control.GetNormValue: single;
begin
  Result := FNormValue;
end;

function TG2Control.GetValue: integer;
begin
  Result := FValue;
end;

procedure TG2Control.SetParameter(const aValue: TG2GraphParameterFMX);
begin
  if FParameter <> aValue then begin
    FParameter := aValue;
    if assigned(FParameter) then begin
      FParameter.AssignControl(self);
      ParamEvent( g2pSet, 0);
      NormValue := GetNormParamValue;
      Value := FParameter.GetParameterValue;
    end else begin
      NormValue := 0;
      Value := 0;
    end;
  end;
end;

procedure TG2Control.SetParamLabel(aIndex: integer; aValue: AnsiString);
begin
  if assigned( FParameter) then
    FParameter.ParamLabel[ aIndex] := aValue
  else begin
    //
  end;
end;

procedure TG2Control.SetSelected(const aValue: boolean);
begin
  if FSelected <> aValue then begin
    FSelected := aValue;
  end;
end;

procedure TG2Control.SetNormValue(const aValue: single);
begin
  if aValue <> FNormValue then begin
    FNormValue := aValue;
    if assigned( FParameter) then begin
      FParameter.SetParameterValue(  trunc(FNormValue * (FParameter.HighValue - FParameter.LowValue + 1) + FParameter.LowValue));
      FValue := FParameter.GetParameterValue;
    end;
    Repaint;
  end;
end;

procedure TG2Control.SetValue(const aValue: integer);
begin
  if aValue <> FValue then begin
    FValue := aValue;
    if assigned( FParameter) then begin
      FParameter.SetParameterValue( aValue);
      FNormValue := GetNormValue;
    end;
    Repaint;
  end;
end;

procedure TG2Control.SetModuleData(const aValue : TG2GraphModuleFMX);
begin
  if FModuleData <> aValue then begin
    FModuleData := aValue;

      if assigned(FModuleData) and (FCodeRef <> -1) then begin
        Parameter := FModuleData.Parameter[ FCodeRef] as TG2GraphParameterFMX;
        if assigned(Parameter) and (FInfoFunc <> -1) then begin
          Parameter.InfoFunctionIndex := FInfoFunc;
        end;
    end;
  end;
end;

procedure TG2Control.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
 if (Operation = opRemove) and (AComponent = Parameter) then
    FParameter := nil;
  inherited;
end;

procedure TG2Control.Paint;
var Save: TCanvasSaveState;
begin
  Canvas.BeginScene;
  try
    Save := Canvas.SaveState;
    Canvas.Fill.Kind := TBrushKind.bkNone;
    Canvas.Stroke.Color := claBlue;
    Canvas.DrawRect( BoundsRect, 0, 0, [], AbsoluteOpacity);
  finally
    Canvas.RestoreState(Save);
    Canvas.EndScene;
  end;
end;

procedure TG2Control.ParamEvent(Event: TG2ParamEvent; Info: NativeInt);
begin
  case Event of
    g2pSet:
      begin
        //FValue := GetNormParamValue;
        //Repaint;
      end;
    g2pChange:
      begin
        FNormValue := GetNormParamValue;
        if assigned(FParameter) then
          FValue := FParameter.GetParameterValue;
        Repaint;
      end;
  end;
end;

procedure TG2Control.ParsePanelData(fs: TModuleDefStream);
var aName, aValue : AnsiString;
begin
  while (fs.Position < fs.Size) and (aName <> '#>') do begin
    fs.ReadSpaces;
    aName := fs.ReadUntil( [':', #13]);
    if aName <> '#>' then begin
      //aValue := ReadUntil(fs, [#13]);
      if not ParseProperties( fs, aName) then begin
        // Unknown property
        aValue := fs.ReadUntil( [#13]);
      end;
    end;
  end;
end;

function TG2Control.ParseProperties(fs: TModuleDefStream;
  aName: AnsiString): boolean;
var aValue : AnsiString;
    G2 : TG2File;
    temp : string;
begin
  Result := True;

  if aName = 'ID' then begin
    aValue := fs.ReadUntil( [#13]);
    FID := StrToInt( string(aValue));
  end else

  if aName = 'XPos' then begin
    aValue := fs.ReadUntil( [#13]);
    Position.X := {ModuleData.FSVGControl.Position.X +} StrToInt( string(aValue));
  end else

  if aName = 'YPos' then begin
    aValue := fs.ReadUntil( [#13]);
    Position.Y  := {ModuleData.FSVGControl.Position.Y +} StrToInt( string(aValue));
  end else

  if aName = 'Width' then begin
    aValue := fs.ReadUntil( [#13]);
    if aValue[1] = '"' then begin
      fs.Position := fs.Position - Length(aValue) - 1;
      Result := False;
    end else
      Width := StrToInt( string(aValue));
  end else

  if aName = 'Height' then begin
    aValue := fs.ReadUntil( [#13]);
    Height := StrToInt( string(aValue));
  end else

  if aName = 'ZPos' then begin
    aValue := fs.ReadUntil( [#13]);
    FZOrder := StrToInt( string(aValue));
  end else

  if aName = 'InfoFunc' then begin
    aValue := fs.ReadUntil( [#13]);
    if assigned(FParameter) then
      FParameter.InfoFunctionIndex := StrToInt( string(aValue));

    if assigned(FParameter) and assigned(FParameter.Patch)
       and assigned(FParameter.Patch.G2) then begin
      G2 := FParameter.Patch.G2;
      temp := FParameter.InfoFunction( FParameter.InfoFunctionIndex);
      if pos('?', temp)>0 then
         G2.add_log_line( FParameter.ModuleName + ' ' + FParameter.ParamName + ' ' + IntToStr(FParameter.InfoFunctionIndex) + ' ' + temp, LOGCMD_NUL);
    end;

  end else

    Result := False
end;

procedure TG2Control.ProcessMouseDown(Shift: TShiftState; AbsX, AbsY, X, Y: Single);
begin
 //
end;

procedure TG2Control.ProcessMouseMove(Shift: TShiftState; AbsX, AbsY, X, Y, dX, dY: Single);
begin
 //
end;

procedure TG2Control.ProcessMouseUp(Shift: TShiftState; AbsX, AbsY, X, Y: Single);
begin
 //
end;


//==============================================================================
//
//                                 TG2Led
//
//==============================================================================

constructor TG2Led.Create(AOwner: TComponent);
begin
  inherited;
  HitTest := False;
end;

destructor TG2Led.Destroy;
begin

  inherited;
end;

procedure TG2Led.SetLevel(aValue: byte);
begin
  // Abstract
end;


//==============================================================================
//
//                                TG2LedGreen
//
//==============================================================================

constructor TG2LedGreen.Create(AOwner: TComponent);
begin
  inherited;
  Width := 11;
  Height := 6;
  FLevel := 0;
end;

destructor TG2LedGreen.Destroy;
begin

  inherited;
end;

procedure TG2LedGreen.paint;
var Save: TCanvasSaveState;
begin
  Canvas.BeginScene;
  try
    Save := Canvas.SaveState;
    Canvas.Fill.Kind := TBrushKind.bkSolid;
    if FLevel = 0 then
      Canvas.Fill.Color := claWhite
    else
      Canvas.Fill.Color := claBlue;
    Canvas.Stroke.Kind := TBrushKind.bkSolid;
    Canvas.Stroke.Color := claBlue;

    Canvas.FillRect(BoundsRect, 0, 0, [], AbsoluteOpacity);
    Canvas.DrawRect(BoundsRect, 0, 0, [], AbsoluteOpacity);
  finally
    Canvas.RestoreState(Save);
    Canvas.EndScene;
  end;
end;

procedure TG2LedGreen.ParsePanelData(fs: TModuleDefStream);
var i : integer;
    LedGroup : TG2LedGroup;
    Patch : TG2GraphPatchFMX;
begin
  inherited;

  Patch := ModuleData.PatchPart.Patch as TG2GraphPatchFMX;

  i := 0;
  while (i < Patch.FLedGroupList.Count)
     and not( ( TG2Led( Patch.FLedGroupList[i]).FGroupId = FGroupID)
          and ( TG2Led( Patch.FLedGroupList[i]).ModuleData.ModuleIndex = ModuleData.ModuleIndex)
          and ( TG2Led( Patch.FLedGroupList[i]).ModuleData.Location = ModuleData.Location)) do
    inc(i);

  if not(i < Patch.FLedGroupList.Count) then begin
    // No, add a new group
    LedGroup := TG2LedGroup.Create( ModuleData);
    LedGroup.ModuleData := FModuleData;
    LedGroup.FType := FType;
    LedGroup.FGroupID := FGroupID;
    LedGroup.Parent := ModuleData.FSVGControl;
    Patch.FLedGroupList.Add( LedGroup);
    Patch.FLedGroupList.Sort;
  end else begin
    // Yes, select the existing group
    LedGroup := TG2LedGroup( Patch.FLedGroupList[i]);
  end;
  // Add the led
  LedGroup.FLeds.Add( self);
  LedGroup.FLeds.Sort;
end;

function TG2LedGreen.ParseProperties(fs: TModuleDefStream;
  aName: AnsiString): boolean;
var aValue : AnsiString;
begin
  Result := True;
  if not inherited ParseProperties( fs, aName) then begin

    if aName = 'CodeRef' then begin
      aValue := fs.ReadUntil( [#13]);
      FCodeRef := StrToInt(string(aValue));
    end else

    if aName = 'InfoFunc' then begin
      aValue := fs.ReadUntil( [#13]);
      FInfoFunc := StrToInt(string(aValue));
    end else

    if aName = 'Type' then begin
      aValue := fs.ReadUntil( [#13]);
      if aValue = '"Green"' then begin
        FType := ltGreen;
        Width := 8;
        Height := 8;
      end;
      if aValue = '"Sequencer"' then begin
        FType := ltSequencer;
        Width := 12;
        Height := 7;
      end;
    end else

    if aName = 'GroupId' then begin
      aValue := fs.ReadUntil( [#13]);
      FGroupId := StrToInt(string(aValue));
    end else

      Result := False
  end;
end;

procedure TG2LedGreen.SetLevel(aValue: byte);
begin
  if aValue <> FLevel then begin
    FLevel := aValue;
    Repaint;
  end;
end;

//==============================================================================
//
//                               TG2LedGroup
//
//==============================================================================

constructor TG2LedGroup.Create(AOwner: TComponent);
var LedSeqComparison : TComparison<TG2LedGreen>;
begin
  inherited;

  LedSeqComparison :=
    function (const Led1, Led2 : TG2LedGreen): integer
    begin
      if led1.FCodeRef > led2.FCodeRef then
        Result := 1
      else
        if led1.FCodeRef = led2.FCodeRef then
          Result := 0
        else
          Result := -1;
    end;

  FLeds := TList<TG2LedGreen>.Create(TComparer<TG2LedGreen>.Construct(LedSeqComparison));

  FType := ltSequencer;
  FGroupId := 0;
  FLedOn := 0;
end;

destructor TG2LedGroup.Destroy;
begin
  FLeds.Free;
  inherited;
end;

procedure TG2LedGroup.SetLevel(aValue: byte);
begin
 inherited;

  if FLedOn < FLeds.Count then
    FLeds[ FLedOn].SetLevel( 0);

  if aValue < FLeds.Count then begin
    FLedOn := aValue;
    FLeds[ FLedOn].SetLevel( 1);
  end;
end;


//==============================================================================
//
//                               TG2MiniVU
//
//==============================================================================

constructor TG2MiniVU.Create(AOwner: TComponent);
begin
  inherited;

  FType := ltMiniVU;
  FMiniVUWidth := 8;
  FMiniVUHeight := 16;

  Width := FMiniVUWidth;
  Height := FMiniVUHeight;

  FLevel := 0;
end;

destructor TG2MiniVU.Destroy;
begin

  inherited;
end;

procedure TG2MiniVU.Paint;
var Save: TCanvasSaveState;
    Rect : TRectF;
    i : integer;
    level_green,
    level_yellow,
    level_red : single;
begin
  Canvas.BeginScene;
  try
    Save := Canvas.SaveState;

    level_green := 8 * Height / 16;
    level_yellow := 12 * Height / 16;
    level_red := Height;

    Canvas.Fill.Kind := TBrushKind.bkSolid;

    Rect := BoundsRect;
    Rect.Bottom := BoundsRect.Bottom;
    Rect.Top := BoundsRect.Bottom - level_green;
    Canvas.Fill.Color := claGreen;
    Canvas.FillRect(Rect, 0, 0, [], AbsoluteOpacity);

    Rect.Bottom := BoundsRect.Bottom - level_green;
    Rect.Top := BoundsRect.Bottom - level_yellow;
    Canvas.Fill.Color := claOlive;
    Canvas.FillRect(Rect, 0, 0, [], AbsoluteOpacity);

    Rect.Bottom := BoundsRect.Bottom - level_yellow;
    Rect.Top := BoundsRect.Bottom - Height;
    Canvas.Fill.Color := claMaroon;
    Canvas.FillRect(Rect, 0, 0, [], AbsoluteOpacity);

    Rect := Rect;

    i := 0;
    while (i <= Height) and (i <= FLevel) do begin

      Rect.Top := BoundsRect.Bottom - i - 1;
      Rect.Bottom := BoundsRect.Bottom - i;

      if i > level_yellow then
        Canvas.Fill.Color := claRed
      else
        if i > level_green then
          Canvas.Fill.Color := claYellow
        else
          Canvas.Fill.Color := claLime;

      Canvas.FillRect(Rect, 0, 0, [], AbsoluteOpacity);
      inc(i);
    end;

  finally
    Canvas.RestoreState(Save);
    Canvas.EndScene;
  end;
end;

procedure TG2MiniVU.ParsePanelData(fs: TModuleDefStream);
var Patch : TG2GraphPatchFMX;
begin
  inherited;

  Patch := ModuleData.PatchPart.Patch as TG2GraphPatchFMX;

  Patch.FMiniVUList.Add( self);
  Patch.FMiniVUList.Sort;
end;

function TG2MiniVU.ParseProperties(fs: TModuleDefStream;
  aName: AnsiString): boolean;
var aValue : AnsiString;
begin
  Result := True;
  if not inherited ParseProperties( fs, aName) then begin

    if aName = 'CodeRef' then begin
      aValue := fs.ReadUntil( [#13]);
      FCodeRef := StrToInt(string(aValue));
    end else

    if aName = 'InfoFunc' then begin
      aValue := fs.ReadUntil( [#13]);
      FInfoFunc := StrToInt(string(aValue));
    end else

    if aName = 'Orientation' then begin
      aValue := fs.ReadUntil( [#13]);
      if aValue = '"Vertical"' then begin
        FOrientation := otVertical;
        Width := FMiniVUWidth;
        Height := FMiniVUHeight;
      end;
      if aValue = '"Horizontal"' then begin
        FOrientation := otHorizontal;
        Height := FMiniVUWidth;
        Width := FMiniVUHeight;
      end;
    end else

    if aName = 'GroupId' then begin
      aValue := fs.ReadUntil( [#13]);
      FGroupId := StrToInt(string(aValue));
    end else

      Result := False
  end;
end;

procedure TG2MiniVU.SetLevel(aValue: byte);
begin
  // -40dB 0dB Green  aValue 0..7
  // 0dB 11dB Yellow         9..41/42
  //     >11dB Red           75/76
  if aValue = 0 then
    FLevel := 0
  else
    FLevel := round(FMiniVUHeight*system.math.log10(aValue)*0.5); // Something like this...
  Repaint;
end;

//==============================================================================
//
//                                 TG2Label
//
//==============================================================================

constructor TG2Label.Create(AOwner: TComponent);
begin
  inherited;
  Hittest := False;
  Height := 10;
  Width := 30;
  FCaption := '';
  FFont := TFont.Create;
  FFont.Family := 'Roboto';
end;

destructor TG2Label.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TG2Label.paint;
var Save: TCanvasSaveState;
begin
  Canvas.BeginScene;
  try
    Save := Canvas.SaveState;
    Canvas.Fill.Kind := TBrushKind.bkSolid;
    Canvas.Fill.Color := claBlue;
    Canvas.Stroke.Kind := TBrushKind.bkSolid;
    Canvas.Stroke.Color := claBlue;
    Canvas.Font.Assign(FFont);

    Canvas.FillText( BoundsRect, FCaption, False, AbsoluteOpacity, [], TTextAlign.taLeading);
  finally
    Canvas.RestoreState(Save);
    Canvas.EndScene;
  end;
end;

function TG2Label.ParseProperties(fs: TModuleDefStream;
  aName: AnsiString): boolean;
var aValue : AnsiString;
begin
  Result := True;
  if not inherited ParseProperties( fs, aName) then begin

    if aName = 'FontSize' then begin
      aValue := fs.ReadUntil( [#13]);
      FFont.Size := StrToInt(string(aValue))-2
    end else

    if aName = 'Text' then begin
      aValue := fs.ReadUntil( [#13]);
      FCaption := string(fs.UnQuote(aValue));
    end else

      Result := False
  end;
end;


//==============================================================================
//
//                             TG2TextField
//
//==============================================================================

constructor TG2TextField.Create(AOwner: TComponent);
begin
  inherited;
  Height := 10;
  Width := 30;
  FFont := TFont.Create;
  FFont.Family := 'Roboto';
  FTextFunction := -1;
end;

destructor TG2TextField.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TG2TextField.paint;
var Save: TCanvasSaveState;
begin
  Canvas.BeginScene;
  try
    Save := Canvas.SaveState;

    Canvas.Fill.Kind := TBrushKind.bkSolid;
    Canvas.Fill.Color := claWhite;
    Canvas.Stroke.Kind := TBrushKind.bkSolid;
    Canvas.Stroke.Color := claBlue;
    Canvas.FillRect(BoundsRect, 0, 0, [], AbsoluteOpacity);

    Canvas.Fill.Kind := TBrushKind.bkSolid;
    Canvas.Fill.Color := claBlue;
    Canvas.Stroke.Kind := TBrushKind.bkSolid;
    Canvas.Stroke.Color := claBlue;
    Canvas.Font.Assign(FFont);

    Canvas.FillText( BoundsRect, FParameter.TextFunction, False, AbsoluteOpacity, [], TTextAlign.taCenter);
  finally
    Canvas.RestoreState(Save);
    Canvas.EndScene;
  end;
end;

function TG2TextField.ParseProperties(fs: TModuleDefStream;
  aName: AnsiString): boolean;
var aValue : AnsiString;
    MasterRef, i, value, c : integer;
    G2 : TG2File;
    temp : string;
begin
  Result := True;
  if not inherited ParseProperties( fs, aName) then begin

    if aName = 'MasterRef' then begin
      aValue := fs.ReadUntil( [#13]);
      MasterRef := StrToInt(string(aValue));
      Parameter := ModuleData.Parameter[ MasterRef] as TG2GraphParameterFMX;

    end else

    if aName = 'Text Func' then begin
      aValue := fs.ReadUntil( [#13]);
      FTextFunction := StrToInt(string(aValue));

    end else

    if aName = 'Dependencies' then begin
      fs.ReadConst( '"');
      aValue := fs.ReadUntil( ['"']);

      if assigned(Parameter) then
        ModuleData.FSVGControl.ParseDependencies( self, Parameter.ParamIndex, aValue, FTextFunction);

    end else

      Result := False
  end;
end;

//==============================================================================
//
//                               TG2Button
//
//==============================================================================

constructor TG2Button.Create(AOwner: TComponent);
begin
  inherited;

  FButtonText := TStringList.Create;
  FBitmapData := TStringList.Create;
  FFont := TFont.Create;
  FFont.Family := 'Roboto';
  FFont.Size := 7;

  FOrientation := otHorizontal;

  Width := 13;
  Height := 12;

  FButtonCount := 0;
  FButtonWidth := 0;
  FButtonHeight := 0;

  FImageCount := 0;
  FImageWidth := 0;
end;

destructor TG2Button.Destroy;
begin
  FFont.Free;
  FButtonText.Free;
  FBitmapData.Free;
  inherited;
end;

procedure TG2Button.paint;
var Save: TCanvasSaveState;
    LabelText : string;
begin
  Canvas.BeginScene;
  Save := Canvas.SaveState;
  try
    Canvas.Fill.Kind := TBrushKind.bkSolid;

    if Value = 0 then begin
      Canvas.Fill.Color := claWhite
    end else begin
      Canvas.Fill.Color := claBlue;
    end;

    Canvas.Stroke.Kind := TBrushKind.bkSolid;
    Canvas.Stroke.Color := claBlue;
    Canvas.FillRect(BoundsRect, 0, 0, [], AbsoluteOpacity);
    Canvas.DrawRect(BoundsRect, 0, 0, [], AbsoluteOpacity);

    if FBitmapData.Count > 0 then begin
    end else begin
      LabelText := '';
      if assigned(Parameter) then
        LabelText := Parameter.SelectedButtonText
      else
        if (FButtonText.Count > 0) then begin
          LabelText := string(ParamLabel[0]);
          if LabelText = '' then
            if  NOrmValue < FButtonText.Count then
              LabelText := FButtonText[ Value];
        end;

      if Value = 0 then begin
        Canvas.Fill.Color := claBlue;
        Canvas.Stroke.Color := claBlue;
      end else begin
        Canvas.Fill.Color := claWhite;
        Canvas.Stroke.Color := claWhite;
      end;

      Canvas.Font.Assign(FFont);

      Canvas.FillText( BoundsRect, LabelText, False, AbsoluteOpacity, [], TTextAlign.taCenter);
    end;
  finally
    Canvas.RestoreState(Save);
    Canvas.EndScene;
  end;
end;

function TG2Button.ParseProperties(fs: TModuleDefStream;
  aName: AnsiString): boolean;
var aValue : AnsiString;
begin
  Result := True;

  if not inherited ParseProperties( fs, aName) then begin

    if aName = 'Text' then begin
      fs.ReadConst( '"');
      fs.ReadOptions( FButtonText, [','], ['"']);
    end else

    if aName = 'Image' then begin
      fs.ReadConst( '"');
      fs.ReadOptions( FBitmapData, [':'], ['"']);
    end else

    if aName = 'CodeRef' then begin
      aValue := fs.ReadUntil( [#13]);
      Parameter := ModuleData.Parameter[ StrToInt(string(aValue))] as TG2GraphParameterFMX;
    end else

    if aName = 'InfoFunc' then begin
      aValue := fs.ReadUntil( [#13]);
      //
    end else

    if aName = 'Orientation' then begin
      aValue := fs.ReadUntil( [#13]);
      if aValue = '"Horizontal"' then
        FOrientation := otHorizontal
      else
        FOrientation := otVertical;
    end else

    if aName = 'ButtonCount' then begin
      aValue := fs.ReadUntil( [#13]);
      FButtonCount := StrToInt(string(aValue));
    end else

    if aName = 'ButtonWidth' then begin
      aValue := fs.ReadUntil( [#13]);
      FButtonWidth := StrToInt(string(aValue));
    end else

    {if aName = 'Type' then begin
      aValue := ReadUntil(fs, [#13]);
      //
    end else}

    if aName = 'Style' then begin
      aValue := fs.ReadUntil( [#13]);
      //
    end else

    if aName = 'ImageWidth' then begin
      aValue := fs.ReadUntil( [#13]);
      FImageWidth := StrToInt(string(aValue));
    end else

    if aName = 'ImageCount' then begin
      aValue := fs.ReadUntil( [#13]);
      FImageCount := StrToInt(string(aValue));
    end else

      Result := False

  end;
end;


//==============================================================================
//
//                                 TG2BtnText
//
//==============================================================================

constructor TG2BtnText.Create(AOwner: TComponent);
begin
  inherited;
  FButtonTextType := bttPush;
end;

destructor TG2BtnText.Destroy;
begin

  inherited;
end;

procedure TG2BtnText.paint;
begin
  inherited;

end;

procedure TG2BtnText.ParsePanelData(fs: TModuleDefStream);
begin
  inherited;

  if FBitmapData.Count > 0 then begin
    //FImageList.BitmapWidth := FImageWidth;
    //FImageList.ParseImageData( 1, True)
  end;

  //FParameter.CanChangeLabel := True;
end;

function TG2BtnText.ParseProperties(fs: TModuleDefStream;
  aName: AnsiString): boolean;
var aValue : AnsiString;
begin
  Result := True;

  if not inherited ParseProperties( fs, aName) then begin

    if aName = 'Type' then begin
      aValue := fs.ReadUntil( [#13]);

      {if aValue = '"Push"' then
        FButtonTextType := bttPush
      else
        FButtonTextType := bttNormal;}
      if aValue = '"Push"' then
        FButtonTextType := bttPush
      else
        if aValue = '"Check"' then
          FButtonTextType := bttCheck;

    end else
      Result := False
  end;
end;


procedure TG2BtnText.ProcessMouseUp(Shift: TShiftState; AbsX, AbsY, X, Y: Single);
begin
  if Value = 0 then
    Value := 1
  else
    Value := 0;
end;

//==============================================================================
//
//                                 TG2BtnFlat
//
//==============================================================================

constructor TG2BtnFlat.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TG2BtnFlat.Destroy;
begin
  inherited;
end;

procedure TG2BtnFlat.paint;
var Save: TCanvasSaveState;
begin
  Canvas.BeginScene;
  Save := Canvas.SaveState;
  try
    Canvas.Fill.Kind := TBrushKind.bkSolid;
    Canvas.Fill.Color := claWhite;

    Canvas.Stroke.Kind := TBrushKind.bkSolid;
    Canvas.Stroke.Color := claBlue;
    Canvas.FillRect(BoundsRect, 0, 0, [], AbsoluteOpacity);
    Canvas.DrawRect(BoundsRect, 0, 0, [], AbsoluteOpacity);

    if FBitmapData.Count > 0 then begin
    end else begin
      Canvas.Fill.Kind := TBrushKind.bkSolid;
      Canvas.Fill.Color := claBlue;
      Canvas.Stroke.Kind := TBrushKind.bkSolid;
      Canvas.Stroke.Color := claBlue;
      Canvas.Font.Assign(FFont);

      Canvas.FillText( BoundsRect, FParameter.TextFunction, False, AbsoluteOpacity, [], TTextAlign.taCenter);
    end;
  finally
    Canvas.RestoreState(Save);
    Canvas.EndScene;
  end;
end;

procedure TG2BtnFlat.ParsePanelData(fs: TModuleDefStream);
begin
  inherited;

  if assigned(FParameter) then
    if FImageCount > 0 then begin
      //FImageList.BitmapWidth := FImageWidth;
      //FImageList.ParseImageData( FImageCount, True)
    end;

end;

procedure TG2BtnFlat.ProcessMouseUp(Shift: TShiftState; AbsX, AbsY, X, Y: Single);
begin
  RotateValue;
end;


//==============================================================================
//
//                             TG2BtnRadio
//
//==============================================================================

constructor TG2BtnRadio.Create(AOwner: TComponent);
begin
  inherited;
  FUpsideDown := False;
  SetLength(FRectArray,0);
end;

destructor TG2BtnRadio.Destroy;
begin
  Finalize(FRectArray);
  inherited;
end;

procedure TG2BtnRadio.ProcessMouseUp(Shift: TShiftState; AbsX, AbsY,X, Y: Single);
var i : integer;
begin
  i := 0;
  while (i<FButtonCount) and not(PtInRect(FRectArray[i], PointF(X, Y))) do
    inc(i);

  if (i<FButtonCount) then begin
    FBtnSelected := i;
    Value := i;
  end;
end;

procedure TG2BtnRadio.paint;
var Rect : TRectF;
    i : integer;
    Save: TCanvasSaveState;
    LabelText : string;
begin
  Canvas.BeginScene;
  try
    Save := Canvas.SaveState;

    if FButtonCount > 0 then begin

      for i := 0 to FButtonCount - 1 do begin

        Rect := FRectArray[i];

        Canvas.Fill.Kind := TBrushKind.bkSolid;
        if i = FBtnSelected then
          Canvas.Fill.Color := claBlue
        else
          Canvas.Fill.Color := claWhite;

        Canvas.Stroke.Kind := TBrushKind.bkSolid;
        Canvas.Stroke.Color := claBlue;
        Canvas.FillRect(Rect, 0, 0, [], AbsoluteOpacity);
        Canvas.DrawRect(Rect, 0, 0, [], AbsoluteOpacity);

        if FBitmapData.Count > 0 then begin
        end else begin
          LabelText := '';
          if assigned(Parameter) then
            LabelText := Parameter.ButtonText[i]
          else
            if i < FButtonText.Count then
              LabelText := FButtonText[i];

          if i = FBtnSelected then begin
            Canvas.Fill.Color := claWhite;
            Canvas.Stroke.Color := claWhite;
          end else begin
            Canvas.Fill.Color := claBlue;
            Canvas.Stroke.Color := claBlue;
          end;

          Canvas.Font.Assign(FFont);

          Canvas.FillText( Rect, LabelText, False, AbsoluteOpacity, [], TTextAlign.taCenter);
        end;
      end;
    end;

  finally
    Canvas.RestoreState(Save);
    Canvas.EndScene;
  end;
end;

procedure TG2BtnRadio.ParsePanelData(fs: TModuleDefStream);

    procedure CalcSize;
    var Rect : TRectF;
        i : integer;
    begin
      if FButtonHeight <> 0 then
        Height := FButtonHeight
      else
        FButtonHeight := Height;

      if FButtonWidth <> 0 then
        Width := FButtonWidth
      else
        FButtonWidth := Width;

      if FButtonCount > 0 then begin
        if FOrientation = otHorizontal then begin
          Width := (Width - 1) * FButtonCount + 1;

          FButtonWidth := Width/FButtonCount + 1;
          Rect.Left := 0;
          Rect.Top := 0;
          Rect.Right := FButtonWidth;
          Rect.Bottom := Height;

        end else begin
          Height := (Height -1) * FButtonCount + 1;

          FButtonHeight := (Height/FButtonCount) + 1;
          Rect.Left := 0;
          Rect.Right := Width;
          if FUpsideDown then begin
            Rect.Top := Height - FButtonHeight;
            Rect.Bottom := Height;
          end else begin
            Rect.Top := 0;
            Rect.Bottom := FButtonHeight;
          end;
        end;

        SetLength(FRectArray, FButtonCount);
        for i := 0 to FButtonCount - 1 do begin
           FRectArray[i] := Rect;

           if FOrientation = otHorizontal then begin
            Rect.Left := Rect.Left + FButtonWidth - 1;
            Rect.Right := Rect.Right + FButtonWidth - 1;
          end else begin
            if FUpsideDown then begin
              Rect.Top := Rect.Top - (FButtonHeight - 1);
              Rect.Bottom := Rect.Bottom - (FButtonHeight - 1);
            end else begin
              Rect.Top := Rect.Top + FButtonHeight - 1;
              Rect.Bottom := Rect.Bottom + FButtonHeight - 1;
            end;
          end;
        end;
      end;
    end;

begin
  inherited;

  CalcSize;
  if FBitmapData.Count > 0 then begin
    //FImageList.BitmapWidth := FImageWidth;
    //FImageList.ParseImageData( FButtonCount, True)
  end;
end;

function TG2BtnRadio.ParseProperties(fs: TModuleDefStream;
  aName: AnsiString): boolean;
var aValue : AnsiString;
begin
  Result := True;

  if not inherited ParseProperties( fs, aName) then begin

    if aName = 'Orientation' then begin
      aValue := fs.ReadUntil( [#13]);
      if aValue = '"Horizontal"' then
        FOrientation := otHorizontal
      else
        FOrientation := otVertical;
    end else

    if aName = 'ButtonCount' then begin
      aValue := fs.ReadUntil( [#13]);
      FButtonCount := StrToInt(string(aValue));
    end else

    if aName = 'ButtonWidth' then begin
      aValue := fs.ReadUntil( [#13]);
      FButtonWidth := StrToInt(string(aValue));
    end else

      Result := False
  end;
end;

//==============================================================================
//
//                             TG2BtnRadioEdit
//
//==============================================================================

constructor TG2BtnRadioEdit.Create(AOwner: TComponent);
begin
  inherited;
  FButtonRows := 1;
  FButtonColumns := 1;
end;

destructor TG2BtnRadioEdit.Destroy;
begin

  inherited;
end;

procedure TG2BtnRadioEdit.ParsePanelData(fs: TModuleDefStream);
var Rect : TRectF;
    i, c, r : integer;
begin
  inherited;

  FButtonWidth := Width / FButtonColumns;
  FButtonHeight := Height / FButtonRows;
  Rect.Top := 0;
  Rect.Bottom := FButtonHeight;
  FButtonCount := FButtonColumns * FButtonRows;
  SetLength(FRectArray, FButtonCount);

  i := 0;
  for r := 0 to FButtonRows - 1 do begin
    Rect.Left := 0;
    Rect.Right := FButtonWidth;
    for c := 0 to FButtonColumns - 1 do begin
      FRectArray[i] := Rect;
      inc(i);

      Rect.Left := Rect.Left + FButtonWidth - 1;
      Rect.Right := Rect.Right + FButtonWidth;
    end;
    Rect.Top := Rect.Top + FButtonHeight - 1;
    Rect.Bottom := Rect.Bottom + FButtonHeight;
  end;
end;

function TG2BtnRadioEdit.ParseProperties(fs: TModuleDefStream;
  aName: AnsiString): boolean;
var aValue : AnsiString;
begin
  Result := True;

  if not inherited ParseProperties( fs, aName) then begin
    if aName = 'ButtonColumns' then begin
      aValue := fs.ReadUntil( [#13]);
      FButtonColumns := StrToInt(string(aValue));
      Width := FButtonColumns * 43;
    end else

    if aName = 'ButtonRows' then begin
      aValue := fs.ReadUntil( [#13]);
      FButtonRows := StrToInt(string(aValue));
      Height := FButtonRows * 12;
    end else
      Result := False

  end;
end;

//==============================================================================
//
//                              TG2BtnIncDec
//
//==============================================================================

constructor TG2BtnIncDec.Create(AOwner: TComponent);
begin
  inherited;
  FButtonWidth := 10;
  FButtonHeight := 11;
  FButtonCount := 2;
  FBtnSelected := -1;
end;

destructor TG2BtnIncDec.Destroy;
begin
  inherited;
end;

function TG2BtnIncDec.ParseProperties(fs: TModuleDefStream;
  aName: AnsiString): boolean;
var aValue : AnsiString;
begin
  Result := True;

  if not inherited ParseProperties( fs, aName) then begin

    if aName = 'Type' then begin
      aValue := fs.ReadUntil( [#13]);
      if aValue = '"Left/Right"' then
        FOrientation := otHorizontal
      else
        FOrientation := otVertical;
    end else
      Result := False
  end;
end;

procedure TG2BtnIncDec.ProcessMouseDown(Shift: TShiftState; AbsX, AbsY,X, Y: Single);
var i : integer;
begin
  i := 0;
  while (i<Length(FRectArray)) and not(PtInRect(FRectArray[i], PointF(X,Y))) do
    inc(i);

  if (i<Length(FRectArray)) then begin
    FBtnSelected := i;
  end else
    FBtnSelected := -1;
  Repaint;
end;

procedure TG2BtnIncDec.ProcessMouseUp(Shift: TShiftState; AbsX, AbsY, X, Y: Single);
begin
  if FBtnSelected = 0 then
    DecValue
  else
    if FBtnSelected = 1 then
      IncValue;
  FBtnSelected := -1;
  Repaint;
end;


//==============================================================================
//
//                             TG2PartSelector
//
//==============================================================================

constructor TG2PartSelector.Create(AOwner: TComponent);
begin
  inherited;
  FBitmapData := TStringList.Create;
  FShowOptions := False;
  FOptionSelected := -1;
  SetLength(FRectArray,0);
  Width := trunc(UNITS_ROW * 1.6);
  Height := Width;
end;

destructor TG2PartSelector.Destroy;
begin
  Finalize(FRectArray);
  FBitmapData.Free;
  inherited;
end;

procedure TG2PartSelector.DoMouseLeave;
begin
  if FShowOptions then begin
    FShowOptions := False;
    SetBoundsRect(FCollapsedRect);
    Repaint;
  end;
end;

procedure TG2PartSelector.ProcessMouseMove(Shift: TShiftState; AbsX, AbsY, X, Y, dX, dY: Single);
var i : integer;
begin
  i := 0;
  while (i<Length(FRectArray)) and not(PtInRect(FRectArray[i], PointF(X, Y))) do
    inc(i);

  if (i<Length(FRectArray)) then begin
    if (FOptionSelected <> i) then begin
      FOptionSelected := i;
      Repaint;
    end;
  end else begin
    i := -1;
    if (FOptionSelected <> i) then begin
      FOptionSelected := i;
      Repaint;
    end;
  end;
end;

procedure TG2PartSelector.ProcessMouseUp(Shift: TShiftState; AbsX, AbsY, X, Y: Single);
begin
  if PtInRect(FDisplayRect, PointF(X,Y)) then begin
    FShowOptions := not FShowOptions;
    if FShowOptions then
      SetBoundsRect(FExpandedRect)
    else
      SetBoundsRect(FCollapsedRect);
    Repaint;
  end else begin
    if FOptionSelected <> -1 then begin
      Value := FOptionSelected;
      FShowOptions := False;
      SetBoundsRect(FCollapsedRect);
      Repaint;
    end;
  end;
end;

procedure TG2PartSelector.paint;
var Save: TCanvasSaveState;
    i : integer;
    Rect : TRectF;
begin
  Canvas.BeginScene;
  Save := Canvas.SaveState;
  try
    Canvas.Fill.Kind := TBrushKind.bkSolid;
    Canvas.Fill.Color := claWhite;
    Canvas.Stroke.Kind := TBrushKind.bkSolid;
    Canvas.Stroke.Color := claBlue;
    Canvas.FillRect(FDisplayRect, 0, 0, [], AbsoluteOpacity);
    Canvas.DrawRect(FDisplayRect, 0, 0, [], AbsoluteOpacity);

    Canvas.Stroke.Kind := TBrushKind.bkSolid;
    Canvas.Stroke.Color := claBlue;
    Canvas.FillRect(FBtnRect, 0, 0, [], AbsoluteOpacity);
    Canvas.DrawRect(FBtnRect, 0, 0, [], AbsoluteOpacity);

    if FShowOptions then begin
      for i  := 0 to Length(FRectArray) - 1 do begin
        Rect := FRectArray[i];

        Canvas.Fill.Kind := TBrushKind.bkSolid;
        if i = FOptionSelected then
          Canvas.Fill.Color := claBlue
        else
          Canvas.Fill.Color := claWhite;
        Canvas.FillRect(Rect, 0, 0, [], AbsoluteOpacity);
      end;
    end;

  finally
    Canvas.RestoreState(Save);
    Canvas.EndScene;
  end;
end;

procedure TG2PartSelector.ParsePanelData(fs: TModuleDefStream);
var Rect : TRectF;
    i : integer;
    ImageHeight : single;
begin
  inherited;

  ImageHeight := FBitmapData.Count / (FImageWidth * FImageCount);

  FBtnRect     := RectF( Width-9, 0, Width, Height);
  FDisplayRect := BoundsRect;
  FCollapsedRect := RectF(Position.X, Position.Y, Position.X + Width, Position.Y + Height);
  FExpandedRect := RectF(Position.X, Position.Y, Position.X + System.Math.Max(Width, FImageWidth), Position.Y + Height + ImageHeight * FImageCount );

  SetLength(FRectArray, FImageCount);

  Rect := RectF(0, Height, FImageWidth, Height + ImageHeight);
  for i := 0 to Length(FRectArray) - 1 do begin
    FRectArray[i] := Rect;
    Rect.Top := Rect.Top + ImageHeight;
    Rect.Bottom := Rect.Bottom + ImageHeight;
  end;
end;

function TG2PartSelector.ParseProperties(fs: TModuleDefStream;
  aName: AnsiString): boolean;
var aValue : AnsiString;
begin
  Result := True;

  if not inherited ParseProperties( fs, aName) then begin

    {if aName = 'Text' then begin
      ReadConst( fs, '"');
      ReadOptions( fs, sl, [','], ['"']);
    end else}

    if aName = 'Image' then begin
      fs.ReadConst( '"');
      fs.ReadOptions( FBitmapData, [':'], ['"']);
    end else

    if aName = 'CodeRef' then begin
      aValue := fs.ReadUntil( [#13]);
      FParameter := ModuleData.Mode[ StrToInt(string(aValue))] as TG2GraphParameterFMX;
    end else

    if aName = 'InfoFunc' then begin
      aValue := fs.ReadUntil( [#13]);
      //
    end else

    if aName = 'ImageWidth' then begin
      aValue := fs.ReadUntil( [#13]);
      FImageWidth := StrToInt(string(aValue));
    end else

    if aName = 'ImageCount' then begin
      aValue := fs.ReadUntil( [#13]);
      FImageCount := StrToInt(string(aValue));
    end else

    if aName = 'MenuOffset' then begin
      aValue := fs.ReadUntil( [#13]);
    end else
      Result := False

  end;
end;

//==============================================================================
//
//                           FSVGG2KnobButtons
//
//==============================================================================

constructor TG2KnobButtons.Create(AOwner: TComponent);
begin
  inherited;

  FBtnWidth := 11;
  FBtnHeight := 9;
  FBorderWidth := 1;

  HitTest := False;

  Width := FBtnWidth * 2 - FBorderWidth;
  Height := FBtnHeight;
end;

destructor TG2KnobButtons.Destroy;
begin

  inherited;
end;

procedure TG2KnobButtons.paint;
var bw : single;
    Save: TCanvasSaveState;
begin
  bw := 1;

  Canvas.BeginScene;
  try
    Save := CAnvas.SaveState;
    Canvas.Fill.Kind := TBrushKind.bkSolid;
    Canvas.Fill.Color := claWhite;
    Canvas.Stroke.Kind := TBrushKind.bkNone;

    Canvas.FillRect( BoundsRect, 0, 0, [], AbsoluteOpacity);

    Canvas.Fill.Color := claLightGray;

    Canvas.FillRect(RectF(FBorderWidth, FBorderWidth, FBtnWidth - FBorderWidth, FBtnHeight - FBorderWidth), 0, 0, [], AbsoluteOpacity);
    Canvas.FillRect(RectF(FBtnWidth, FBorderWidth, Width - FBorderWidth, FBtnHeight - FBorderWidth), 0, 0, [], AbsoluteOpacity);
  finally
    Canvas.RestoreState(Save);
    Canvas.EndScene;
  end;
end;

//==============================================================================
//
//                           TG2ResetButton
//
//==============================================================================

constructor TG2ResetButton.Create(AOwner: TComponent);
begin
  inherited;

  HitTest := False;

  Width := 10;
  Height := 4;

  FCentered := True;
end;

destructor TG2ResetButton.Destroy;
begin
  inherited;
end;

procedure TG2ResetButton.paint;
var poly : TPolygon;
    Save: TCanvasSaveState;
begin
  Canvas.BeginScene;
  try
    Save := Canvas.SaveState;

    Canvas.Stroke.Kind := TBrushKind.bkSolid;
    Canvas.Stroke.Color := claBlue;
    Canvas.Fill.Kind := TBrushKind.bkSolid;
    if FCentered then begin
      Canvas.Fill.Color := claBlue;
    end else begin
      Canvas.Fill.Color := claWhite;
    end;


    SetLength(poly, 4);
    poly[0] := PointF(0,0);
    poly[1] := PointF(Width,0);
    poly[2] := PointF(Width/2, Height);
    poly[3] := PointF(0,0);

    Canvas.FillPolygon(poly, AbsoluteOpacity);
    Canvas.DrawPolygon(poly, AbsoluteOpacity);
  finally
    Canvas.RestoreState( Save);
    Canvas.EndScene;
  end;
end;

//==============================================================================
//
//                             TG2Knob
//
//==============================================================================

constructor TG2Knob.Create(AOwner: TComponent);
begin
  inherited;
  HitTest := True;
  FKnobBtns := nil;
  FKnobReset := nil;
  FKnobType := ktNone;
end;

destructor TG2Knob.Destroy;
begin

  inherited;
end;

procedure TG2Knob.DoMouseEnter;
begin
  inherited;
  if assigned(FKnobBtns) then
    FKnobBtns.Visible := True;

end;

procedure TG2Knob.DoMouseLeave;
begin
  inherited;
  if assigned(FKnobBtns) then
    FKnobBtns.Visible := Selected;

end;

procedure TG2Knob.paint;
var Angle : single;
    SliderKnobRect : TRectF;
    f, h, range : single;
    Save: TCanvasSaveState;
begin
  Canvas.BeginScene;
  try
    Save := Canvas.SaveState;

    if (FKnobType = ktSlider) or (FKnobType = ktSeqSlider) then begin
      Canvas.Fill.Kind := TBrushKind.bkSolid;
      Canvas.Fill.Color := claWhite;
      Canvas.Stroke.Color := claBlue;

      Canvas.FillRect( BoundsRect, 0, 0, [], AbsoluteOpacity);
      Canvas.DrawRect( BoundsRect, 0, 0, [], AbsoluteOpacity);

      Canvas.Fill.Color := claBlue;

      f :=  NormValue;

      h := 8;
      range := Height - h;
      SliderKnobRect := RectF( 0, range - f * range, Width, range - f * range + h);

      Canvas.FillRect( SliderKnobRect, 0, 0, [], AbsoluteOpacity);
      Canvas.DrawRect( SliderKnobRect, 0, 0, [], AbsoluteOpacity);

    end else begin
      Canvas.Fill.Kind := TBrushKind.bkSolid;
      Canvas.Fill.Color := claWhite;
      Canvas.Stroke.Color := claBlue;
      Canvas.FillEllipse( RectF(FCX-FR, FCY-FR, FCX+FR, FCY+FR), AbsoluteOpacity);
      Canvas.DrawEllipse( RectF(FCX-FR, FCY-FR, FCX+FR, FCY+FR), AbsoluteOpacity);

      Angle := 2*PI * ( 0.8 * NormValue + 0.1);

      Canvas.DrawLine( PointF(FCX,FCY), PointF(FCX-sin(Angle)*FR, FCY+cos(Angle)*FR), AbsoluteOpacity);

      if assigned(FKnobReset) then begin
        if Value = 64 then
          FKnobReset.FCentered := True
        else
          FKnobReset.FCentered := False;
        FKnobReset.Repaint;
      end;

    end;
  finally
    Canvas.RestoreState(Save);
    Canvas.EndScene;
  end;
end;

function TG2Knob.ParseProperties(fs: TModuleDefStream;
  aName: AnsiString): boolean;
var aValue : AnsiString;
begin
  Result := True;

  if not inherited ParseProperties( fs, aName) then begin

    if aName = 'CodeRef' then begin
      aValue := fs.ReadUntil( [#13]);
      FParameter := FModuleData.Parameter[ StrToInt(string(aValue))] as TG2GraphParameterFMX;
      (FParameter as TG2GraphParameterFMX).AssignControl(self);
    end else

    if aName = 'InfoFunc' then begin
      aValue := fs.ReadUntil( [#13]);
      //
    end else

    if aName = 'Type' then begin
      aValue := fs.ReadUntil( [#13]);
      if aValue = '"Reset"' then begin
        KnobType := ktReset;
      end;
      if aValue = '"Reset/medium"' then begin
        KnobType := ktResetMedium;
      end;
      if aValue = '"Small"' then begin
        KnobType := ktSmall;
      end;
      if aValue = '"Medium"' then begin
        KnobType := ktMedium;
      end;
      if aValue = '"Big"' then begin
        KnobType := ktBig;
      end;
      if aValue = '"Slider"' then begin
        KnobType := ktSlider;
      end;
      if aValue = '"SeqSlider"' then begin
        KnobType := ktSeqSlider;
      end;
    end else
      Result := False
  end;
end;

procedure TG2Knob.ProcessMouseMove(Shift: TShiftState; AbsX, AbsY, X, Y, dX, dY: Single);
var d : single;
begin
  if ssLeft in Shift then begin

    d := dy / 100;
    if NormValue + d > 1 then
      NormValue := 1
    else
      if NormValue + d < 0 then
        NormValue := 0
      else
        NormValue := NormValue + d;
    Repaint;
  end;
end;

procedure TG2Knob.ProcessMouseUp(Shift: TShiftState; AbsX, AbsY, X, Y: Single);
begin
  if assigned(FKnobReset) and PtInRect( FKnobReset.ControlRect, PointF(X, Y)) then
    NormValue := 0.5
  else
    if assigned(FKnobBtns) and PtInRect(FKnobBtns.ControlRect, PointF(X, Y)) then begin
      if X < FKnobBtns.Left + FKnobBtns.Width/2 then
        DecValue
      else
        IncValue;
    end;
end;

procedure TG2Knob.SetKnobType(const Value: TKnobType);

  procedure CreateKnobBtns;
  begin
    FKnobBtns := TG2KnobButtons.Create(self);
    FKnobBtns.Parent := Self;
    FKnobBtns.Position.X := FCX - 10.5;
    FKnobBtns.Position.Y := Height - 9;
    FKnobBtns.OnMouseMove := OnMouseMove;
    FKnobBtns.Visible := False;
  end;

  procedure CreateResetBtn;
  begin
    FKnobReset := TG2ResetButton.Create(self);
    FKnobReset.Parent := Self;
    FKnobReset.Position.X := FCX - 5;
    FKnobReset.Position.Y := 0;
    FKnobReset.OnMouseMove := OnMouseMove;
  end;

begin
  if FKnobType <> Value then begin
    FKnobType := Value;
    case FKnobType of
      ktReset:
        begin
          Width := 18;
          Height := 26;
          FCX := 9;
          FCY := 14;
          FR := 9;
          CreateKnobBtns;
          CreateResetBtn;
        end;
      ktResetMedium:
        begin
          Width := 20;
          Height := 30;
          FCX := 10;
          FCY := 15;
          FR := 10;
          CreateKnobBtns;
          CreateResetBtn;
        end;
      ktSmall:
        begin
          Width := 18;
          Height := 22;
          FCX := 9;
          FCY := 9;
          FR := 9;
          CreateKnobBtns;
        end;
      ktMedium:
        begin
          Width := 20;
          Height := 24;
          FCX := 10;
          FCY := 10;
          FR := 10;
          CreateKnobBtns;
        end;
      ktBig:
        begin
          Width := 22;
          Height := 26;
          FCX := 11;
          FCY := 11;
          FR := 11;
          CreateKnobBtns;
        end;
      ktSlider:
        begin
          Width := 11;
          Height := 45;
        end;
      ktSeqSlider:
        begin
          Width := 11;
          Height := 45;
        end;
    end;
  end;
end;

procedure TG2Knob.SetSelected(const aValue: boolean);
begin
  inherited;
  if assigned(FKnobBtns) then
    FKnobBtns.Visible := Selected;
end;
//==============================================================================
//
//                             TSVGG2Connector
//
//==============================================================================

constructor TG2Connector.Create(AOwner: TComponent);
var hw : single;
begin
  inherited;
  HitTest := True;

  Width := 13;
  Height := 13;

  hw := Width/4;

  FHoleRect := RectF(hw, hw, Width-hw, Height-hw);

  FTempCable := nil;
end;

destructor TG2Connector.Destroy;
begin
  if assigned(FTempCable) then
    FTempCable.Free;

  inherited;
end;

procedure TG2Connector.Paint;
var Save: TCanvasSaveState;
begin
  Canvas.BeginScene;
  try
    Save := Canvas.SaveState;
    Canvas.Fill.Kind := TBrushKind.bkSolid;
    Canvas.Stroke.Kind := TBrushKind.bkSolid;
    Canvas.Fill.Color := claWhite;
    Canvas.Stroke.Color := claBlue;

    if Data.ConnectorKind = ckInput then begin
      Canvas.FillEllipse( BoundsRect, AbsoluteOpacity);
      Canvas.DrawEllipse( BoundsRect, AbsoluteOpacity);
    end else begin
      Canvas.FillRect(BoundsRect, 0, 0, [], AbsoluteOpacity);
      Canvas.DrawRect(BoundsRect, 0, 0, [], AbsoluteOpacity);
    end;

    if Data.CableCount = 0 then begin
      Canvas.Fill.Color := claBlue;
      Canvas.FillEllipse( FHoleRect, AbsoluteOpacity);
      Canvas.DrawEllipse( FHoleRect, AbsoluteOpacity);
    end;

  finally
    Canvas.RestoreState(Save);
    Canvas.EndScene;
  end;
end;

procedure TG2Connector.ParsePanelData(fs: TModuleDefStream);
begin
  inherited;

  FData.CalcDefColor;
end;

function TG2Connector.ParseProperties(fs: TModuleDefStream;
  aName: AnsiString): boolean;
var aValue : AnsiString;
begin
  Result := True;

  if not inherited ParseProperties( fs, aName) then begin

    if aName = 'CodeRef' then begin
      aValue := fs.ReadUntil( [#13]);
      FData.ConnectorIndex := StrToInt(string(aValue));
    end else

    if aName = 'InfoFunc' then begin
      aValue := fs.ReadUntil( [#13]);
      //
    end else

    if aName = 'Type' then begin
      aValue := fs.ReadUntil( [#13]);
      if aValue = '"Audio"' then
        FData.ConnectorType := ctAudio
      else
        if aValue = '"Logic"' then
          FData.ConnectorType := ctLogic
        else
          if aValue = '"Control"' then
            FData.ConnectorType := ctControl
          else
            raise Exception.Create('Unknown control type ' + string(aValue));
    end else

    if aName = 'Bandwidth' then begin
      aValue := fs.ReadUntil( [#13]);
      if aValue = '"Static"' then
        FData.Bandwidth := btStatic
      else
        if aValue = '"Dynamic"' then
          FData.BandWidth := btDynamic
        else
          raise Exception.Create('Unknown bandwidth type ' + string(aValue));
    end else

      Result := False
  end;
end;

procedure TG2Connector.ProcessMouseDown(Shift: TShiftState; AbsX, AbsY, X, Y: Single);
var P : TPointF;
begin
  if assigned(FTempCable) then
    FTempCable.Free;

  if (ssLeft in Shift) then begin

    FTempCable := TG2CableV2.Create( self);
    FTempCable.Parent := Parent;
    FTempCable.FP1.X := AbsX;
    FTempCable.FP1.Y := AbsY;
    FTempCable.FP2.X := AbsX;
    FTempCable.FP2.Y := AbsY;
    FTempCable.InitCable;

  end;
end;

procedure TG2Connector.ProcessMouseMove(Shift: TShiftState; AbsX, AbsY, X, Y, dX,
  dY: Single);
var P : TPointF;
begin
  if (ssLeft in Shift) then begin
    if assigned(FTempCable) then begin
      FTempCable.FP2.X := AbsX;
      FTempCable.FP2.Y := AbsY;
      FTempCable.InitCable;
    end;
  end;
end;

procedure TG2Connector.ProcessMouseUp(Shift: TShiftState; AbsX, AbsY, X, Y: Single);
begin
  if assigned(FTempCable) then begin
    FTempCable.Free;
    FTempCable := nil;
  end;

end;

procedure TG2Connector.SetData(aValue: TG2FileConnector);
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

  FSVGControl := TG2CableV2.Create(AOwner);
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
var ModuleFrom, ModuleTo : TG2Module;
    ConnectorFrom, ConnectorTo : TG2Connector;
    P : TPointF;
begin
  if not(assigned(FromConnector) and (assigned(ToConnector)
      and (assigned(FromConnector.Module)) )) then
    exit;

  ModuleFrom := (FromConnector.Module as TG2GraphModuleFMX).SVGControl;
  ModuleTo := (ToConnector.Module as TG2GraphModuleFMX).SVGControl;

  ConnectorFrom := FromConnector.GraphControl as TG2Connector;
  ConnectorTo := ToConnector.GraphControl as TG2Connector;

  P.X := ConnectorFrom.Position.X + ConnectorFrom.Width/2;
  P.Y := ConnectorFrom.Position.Y + ConnectorFrom.Height/2;
  FSVGControl.Point1X := P.X + ModuleFrom.Position.X;
  FSVGControl.Point1Y := P.Y + ModuleFrom.Position.Y;

  P.X := ConnectorTo.Position.X + ConnectorTo.Width/2;
  P.Y := ConnectorTo.Position.Y + ConnectorTo.Height/2;
  FSVGControl.Point2X := P.X + ModuleTo.Position.X;
  FSVGControl.Point2Y := P.Y + ModuleTo.Position.Y;

  FSVGControl.StartTimer;
end;

procedure TG2GraphCableFMX.InitCable;
var ModuleFrom, ModuleTo : TG2Module;
    ConnectorFrom, ConnectorTo : TG2Connector;
    P : TPointF;
begin
  if not(assigned(FromConnector) and (assigned(ToConnector)
      and (assigned(FromConnector.Module)) )) then
    exit;

  ModuleFrom := (FromConnector.Module as TG2GraphModuleFMX).SVGControl;
  ModuleTo := (ToConnector.Module as TG2GraphModuleFMX).SVGControl;

  ConnectorFrom := FromConnector.GraphControl as TG2Connector;
  ConnectorTo := ToConnector.GraphControl as TG2Connector;

  P.X := ConnectorFrom.Position.X + ConnectorFrom.Width/2;
  P.Y := ConnectorFrom.Position.Y + ConnectorFrom.Height/2;
  FSVGControl.Point1X := P.X + ModuleFrom.Position.X;
  FSVGControl.Point1Y := P.Y + ModuleFrom.Position.Y;

  P.X := ConnectorTo.Position.X + ConnectorTo.Width/2;
  P.Y := ConnectorTo.Position.Y + ConnectorTo.Height/2;
  FSVGControl.Point2X := P.X + ModuleTo.Position.X;
  FSVGControl.Point2Y := P.Y + ModuleTo.Position.Y;

  FSVGControl.InitCable;
end;

procedure TG2GraphCableFMX.SetParent(aValue: TFmxObject);
begin
  FParent := aValue;
  FSVGControl.Parent := FParent;
end;

//==============================================================================
//
//                                TBitmapRect
//
//==============================================================================

constructor TBitmapRect.Create;
begin
  FRect := RectF(0,0,0,0);
  FZoom := 1;
  FBitmap := nil;
end;

destructor TBitmapRect.Destroy;
begin
  if assigned(FBitmap) then
    FBitmap.Free;
  inherited;
end;

function TBitmapRect.Getactive: boolean;
begin
  Result := assigned(FBitmap);
end;

procedure TBitmapRect.SetActive(aValue: boolean);
begin
  if assigned(FBitmap) then begin
    if aValue = False then begin
      FBitmap.Free;
    end;
  end else begin
    if aValue then begin
      FBitmap := TBitmap.Create(trunc(FRect.Width*FZoom) + 1, trunc(FRect.Height*FZoom) + 1);
    end;
  end;
end;

procedure TBitmapRect.SetZoom( aValue : single);
begin
  if aValue <> FZoom then begin
    FZoom := aValue;
    if assigned(FBitmap) then begin
      FBitmap.SetSize(trunc(FRect.Width*FZoom) + 1, trunc(FRect.Height*FZoom) + 1);
    end;
  end;
end;

//==============================================================================
//
//                                TCableBitmapBuffer
//
//==============================================================================

constructor TCableBitmapBuffer.Create(AOwner: TComponent);
begin
  inherited;
  FBitmapList := TObjectList.Create( True);

  Fdx := 100;
  Fdy := 80;
  FZoom := 1;

  FCols := trunc(Width * FZoom / Fdx)+1;
  FRows := trunc(Height * FZoom / Fdy)+1;

  Clear;
  FCableStyle := csFlat;
  //FCableStyle := csGradient;
  FRedrawBuffer := True;

  Hittest := False;
end;

destructor TCableBitmapBuffer.Destroy;
begin
  FBitmapList.Free;
  inherited;
end;

procedure TCableBitmapBuffer.SetCableStyle(aValue: TCableStyle);
begin
  if aValue <> FCableStyle then begin
    FCableStyle := aValue;
    FRedrawBuffer := True;
    Repaint;
  end;
end;

procedure TCableBitmapBuffer.SetZoom(aValue: Single);
var i : integer;
begin
  if aValue <> FZoom then begin
    FZoom := aValue;
    for i := 0 to FBitmapList.Count - 1 do begin
      (FBitmapList[i] as TBitmapRect).Zoom := aValue;
    end;
    FRedrawBuffer := True;
    Repaint;
  end;
end;

procedure TCableBitmapBuffer.Clear;
var i, j : integer;
    BitmapRect : TBitmapRect;
begin
  FBitmapList.Clear;
  for i := 0 to FRows - 1 do
    for j := 0 to FCols - 1 do begin
      BitmapRect := TBitmapRect.Create;
      BitmapRect.Rect := RectF( j*Fdx, i*Fdy, (j+1)*Fdx, (i+1)*Fdy);
      BitmapRect.Zoom := FZoom;
      FBitmapList.Add( BitmapRect);
    end;
end;

procedure TCableBitmapBuffer.Paint;
var i, j, k : integer;
    BitmapRect : TBitmapRect;
    Save: TCanvasSaveState;
begin
  inherited;
  exit;

{  if assigned(FCableList) then begin

    if FRedrawBuffer then begin
      Clear;
      for i := 0 to FCableList.Count - 1 do begin
        (FCableList[i] as TG2GraphCableFMX).FSVGControl.PaintBuffer(self);
      end;
      FRedrawBuffer := False;
    end;

    Canvas.BeginScene;
    try
      Save := Canvas.SaveState;

      Canvas.Stroke.Color := claBlack;
      Canvas.Fill.Color := claRed;

      for i := 0 to FRows - 1 do
        for j := 0 to FCols - 1 do begin
          k := (i * FCols) + j;
          BitmapRect := FBitmapList.Items[k] as TBitmapRect;

          if BitmapRect.Active then begin
            Canvas.DrawRect(RectF(j*Fdx*FZoom,i*Fdy*FZoom,(j+1)*Fdx*FZoom,(i+1)*Fdy*FZoom), 0, 0, [], 1);

            Canvas.DrawBitmap( BitmapRect.Bitmap, RectF(0,0,Fdx*FZoom,Fdy*FZoom), RectF(j*Fdx*FZoom,i*Fdy*FZoom,(j+1)*Fdx*FZoom,(i+1)*Fdy*FZoom), AbsoluteOpacity );
          end;
        end;
    finally
      Canvas.RestoreState(Save);
      Canvas.EndScene;
    end;
  end;}
end;

procedure TCableBitmapBuffer.Resize;
begin
  FCols := trunc(Width  / Fdx)+1;
  FRows := trunc(Height / Fdy)+1;
  Fdx := 100/FZoom;
  Fdy := 80/FZoom;

  FRedrawBuffer := True;

  inherited;
end;

//==============================================================================
//
//                                TG2Cable
//
//==============================================================================

constructor TG2Cable.Create(AOwner: TComponent);
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

  AddNodes(2);

  FTimer.OnTimer := OnTimer;
end;

destructor TG2Cable.Destroy;
begin
  ClearNodes;
  Finalize(FNodes);

  FTimer.Enabled := False;
  FTimer.Free;

  inherited;
end;

procedure TG2Cable.ClearNodes;
var i : integer;
begin
  for i := 0 to FNodeCount - 1 do
    FNodes[i].Free;
  SetLength(FNodes, 0);
end;

procedure TG2Cable.AddNodes( aNodeCount : integer);
var i : integer;
begin
  ClearNodes;
  FNodeCount := aNodeCount;
  SetLength(FNodes, FNodeCount);
  for i := 0 to FNodeCount - 1 do
    FNodes[i] := TNode.Create;
end;

procedure TG2Cable.InitCable;
var n : integer;
    l, dx, dy : single;
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
  min_x := System.Math.Min(FP1.X, FP2.X);
  max_x := System.Math.Max(FP1.X, FP2.X);
  min_y := System.Math.Min(FP1.Y, FP2.Y);
  max_y := System.Math.Max(FP1.Y, FP2.Y);

  l := sqrt((max_x - min_x)*(max_x - min_x) + (max_y - min_y)*(max_y - min_y));
  AddNodes(trunc(l/ 20)+2);

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

procedure TG2Cable.IterateCable;
var n : integer;
    min_x, min_y, max_x, max_y : single;
begin
  n := FNodeCount - 1;

  min_x := System.Math.Min(FP1.X, FP2.X);
  max_x := System.Math.Max(FP1.X, FP2.X);
  min_y := System.Math.Min(FP1.Y, FP2.Y);
  max_y := System.Math.Max(FP1.Y, FP2.Y);

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

procedure TG2Cable.OnTimer(Sender: TObject);
begin
  if FTimerCount <= 0 then
    FTimer.Enabled := False;
  Dec(FTimerCount);
  IterateCable;
  Repaint;
end;

procedure TG2Cable.Paint;
var i : integer;
    Path : TPathData;
    PathPoint : TPathPoint;
    BB : TRectF;
    P1, P2, P3, P4, V, N, G1, G2 : TPointF;
    L, d : single;
    Save: TCanvasSaveState;
begin
  exit;
  d := 1.5;

  Canvas.BeginScene;
  Path := TPathData.Create;
  try
    Save := Canvas.SaveState;

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

      BB.Left := System.Math.Min(System.Math.Min(System.Math.Min( p1.X, p2.X), p3.X), p4.X);
      BB.Top := System.Math.Min(System.Math.Min(System.Math.Min( p1.Y, p2.Y), p3.Y), p4.Y);
      BB.Right := System.Math.Max(System.Math.Max(System.Math.Max( p1.X, p2.X), p3.X), p4.X);
      BB.Bottom := System.Math.Max(System.Math.Max(System.Math.Max( p1.Y, p2.Y), p3.Y), p4.Y);

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
    Canvas.RestoreState(Save);
    Path.Free;
    Canvas.EndScene;
  end;
end;

procedure TG2Cable.PaintBuffer(aBuffer: TCableBitmapBuffer);
var i, j : integer;
    P1, P2, P3, P4, V, N, G1, G2 : TPointF;
    L, d : single;
    R, ElementBoundsRect : TRectF;
    BitmapRect : TBitmapRect;
    Path : TPathData;
    SaveMatrix : TMatrix;
begin
  //d := 1.5;
  d:=1;

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

    P1.X := FNodes[i-1].x + N.X*d;
    P1.Y := FNodes[i-1].y + N.Y*d;
    P2.X := FNodes[i].x + N.X*d;
    P2.Y := FNodes[i].y + N.Y*d;
    P3.X := FNodes[i].x - N.X*d;
    P3.Y := FNodes[i].y - N.Y*d;
    P4.X := FNodes[i-1].x - N.X*d;
    P4.Y := FNodes[i-1].y - N.Y*d;

    // Calc bounding box for segment
    ElementBoundsRect.Left := System.Math.Min(System.Math.Min(System.Math.Min( p1.X, p2.X), p3.X), p4.X);
    ElementBoundsRect.Top := System.Math.Min(System.Math.Min(System.Math.Min( p1.Y, p2.Y), p3.Y), p4.Y);
    ElementBoundsRect.Right := System.Math.Max(System.Math.Max(System.Math.Max( p1.X, p2.X), p3.X), p4.X);
    ElementBoundsRect.Bottom := System.Math.Max(System.Math.Max(System.Math.Max( p1.Y, p2.Y), p3.Y), p4.Y);

    G1.X := P1.X + (P2.X - P1.X)/2;
    G1.Y := P1.Y + (P2.Y - P1.Y)/2;
    G2.X := P4.X + (P3.X - P4.X)/2;
    G2.Y := P4.Y + (P3.Y - P4.Y)/2;

    if (ElementBoundsRect.Right - ElementBoundsRect.Left) <> 0 then begin
      G1.X := (G1.X - ElementBoundsRect.Left) / (ElementBoundsRect.Right - ElementBoundsRect.Left);
      G2.X := (G2.X - ElementBoundsRect.Left) / (ElementBoundsRect.Right - ElementBoundsRect.Left);
    end;

    if (ElementBoundsRect.Bottom - ElementBoundsRect.Top) <> 0 then begin
      G1.Y := (G1.Y - ElementBoundsRect.Top) / (ElementBoundsRect.Bottom - ElementBoundsRect.Top);
      G2.Y := (G2.Y - ElementBoundsRect.Top) / (ElementBoundsRect.Bottom - ElementBoundsRect.Top);
    end;

    // Paint on all overlapping squares
    Path := TPathData.Create;
    try
      Path.Clear;
      Path.MoveTo( P1);
      Path.LineTo( P2);
      Path.LineTo( P3);
      Path.LineTo( P4);
      Path.ClosePath;

      for j := 0 to aBuffer.FBitmapList.Count - 1 do begin
        BitMapRect := aBuffer.FBitmapList.Items[j] as TBitmapRect;
        if BitMapRect.Rect.IntersectsWith( ElementBoundsRect) then begin
          BitMapRect.Active := True;

          BitMapRect.FBitmap.Canvas.BeginScene;
          try
            SaveMatrix := BitMapRect.FBitmap.Canvas.Matrix;

            BitMapRect.FBitmap.Canvas.SetMatrix(
                    CreateTransformationMatrix( aBuffer.Zoom, aBuffer.Zoom, -BitMapRect.Rect.Left, -BitMapRect.Rect.Top, 0, 0, 0));

            if aBuffer.CableStyle = csFlat then begin
              BitMapRect.FBitmap.Canvas.Fill.Color := ConvertToAlpha( CableColors[ FData.CableColor]);
              BitMapRect.FBitmap.Canvas.Stroke.Kind := TBrushKind.bkNone;
            end;

            if aBuffer.CableStyle = csGradient then begin
              BitMapRect.FBitmap.Canvas.Fill.Kind := TBrushKind.bkGradient;
              BitMapRect.FBitmap.Canvas.Fill.Gradient.Color := claBlack;
              BitMapRect.FBitmap.Canvas.Fill.Gradient.Color1 := ConvertToAlpha( CableColors[ FData.CableColor]);
              BitMapRect.FBitmap.Canvas.Fill.Gradient.StartPosition.Point := PointF( G1.X, G1.Y);
              BitMapRect.FBitmap.Canvas.Fill.Gradient.StopPosition.Point  := PointF( G2.X, G2.Y);
              BitMapRect.FBitmap.Canvas.Stroke.Kind := TBrushKind.bkNone;
            end;

            BitMapRect.FBitmap.Canvas.FillPath( Path, 1);
            //BitMapRect.FBitmap.Canvas.DrawPath( Path, 1);

            if (i = 1) then begin
              BitMapRect.FBitmap.Canvas.Fill.Kind := TBrushKind.bkSolid;
              BitMapRect.FBitmap.Canvas.Fill.Color := ConvertToAlpha( CableColors[ FData.CableColor]);
              BitMapRect.FBitmap.Canvas.FillEllipse( RectF(FNodes[i-1].x - 3, FNodes[i-1].y - 3,
                                                           FNodes[i-1].x + 3, FNodes[i-1].y + 3), 1);
            end;

            if (i = FNodeCount-1) then begin
              BitMapRect.FBitmap.Canvas.Fill.Kind := TBrushKind.bkSolid;
              BitMapRect.FBitmap.Canvas.Fill.Color := ConvertToAlpha( CableColors[ FData.CableColor]);
              BitMapRect.FBitmap.Canvas.FillEllipse( RectF(FNodes[i].x - 3, FNodes[i].y - 3,
                                                           FNodes[i].x + 3, FNodes[i].y + 3), 1);
            end;


            BitMapRect.FBitmap.Canvas.SetMatrix(SaveMatrix);
          finally
            BitMapRect.FBitmap.Canvas.EndScene;
          end;
        end;
      end;
    finally
      Path.Free;
    end;
  end;
end;

procedure TG2Cable.SetPoint1X(Value: single);
begin
  FP1.X := Value;
end;

procedure TG2Cable.SetPoint1Y(Value: single);
begin
  FP1.Y := Value;
end;

procedure TG2Cable.SetPoint2X(Value: single);
begin
  FP2.X := Value;
end;

procedure TG2Cable.SetPoint2Y(Value: single);
begin
  FP2.Y := Value;
end;

procedure TG2Cable.StartTimer;
begin
  IterateCable;
  FTimerCount := 50;
  FTimer.Enabled := True;
end;

//==============================================================================
//
//                               TG2CableNode
//
//==============================================================================

constructor TG2CableNode.Create(aCable : TG2CableV2);
begin
  inherited Create(aCable);
  FCable := aCable;

  HitTest := False;

  x := 0;
  y := 0;
  x2 := 0;
  y2 := 0;
  vx := 0;
  vy := 0;

  PStart := PointF(0,0);
  PEnd := PointF(0,0);

  FStartNode := False;
  FEndNode := False;

  SetLength(FPolygon, 5);
end;

destructor TG2CableNode.Destroy;
begin
  Finalize(FPolygon);
  inherited;
end;

procedure TG2CableNode.Paint;
var Save: TCanvasSaveState;
    Rect : TRectF;
begin
  Canvas.BeginScene;
  try
    Save := Canvas.SaveState;
    Canvas.Fill.Kind := TBrushKind.bkSolid;
    Canvas.Fill.Color := FCable.FColor;

    Canvas.Stroke.Kind := TBrushKind.bkSolid;
    Canvas.Stroke.Color := claGreen;

    if FStartNode then begin
      Rect := RectF(PStart.X - 3, PStart.Y - 3, PStart.X + 3, PStart.Y + 3);
      Canvas.FillEllipse( Rect, AbsoluteOpacity);
    end;

    if FEndNode then begin
      Rect := RectF(PEnd.X - 3, PEnd.Y - 3, PEnd.X + 3, PEnd.Y + 3);
      Canvas.FillEllipse( Rect, AbsoluteOpacity);
    end;

    Canvas.FillPolygon( FPolygon, AbsoluteOpacity);
    //Canvas.DrawPolygon( FPolygon, AbsoluteOpacity);

  finally
    Canvas.RestoreState(Save);
    Canvas.EndScene;
  end;
end;

procedure TG2CableNode.CalcPath;
var BB : TRectF;
    P1, P2, P3, P4, V, N, G1, G2 : TPointF;
    L, d : single;
begin

  d := 1;

  // CalcNormal

  V.X := x2 - x;
  V.Y := y2 - y;
  N.X := V.Y;
  N.Y := -V.X;
  L := sqrt( N.X * N.X + N.Y * N.Y);
  if L <> 0 then begin
    N.X := N.X / L;
    N.Y := N.Y / L;
  end;

  // Calc bounding box

  P1.X := x + N.X*d;
  P1.Y := y + N.Y*d;
  P2.X := x2 + N.X*d;
  P2.Y := y2 + N.Y*d;
  P3.X := x2 - N.X*d;
  P3.Y := y2 - N.Y*d;
  P4.X := x - N.X*d;
  P4.Y := y - N.Y*d;

  BB.Left := System.Math.Min(System.Math.Min(System.Math.Min( p1.X, p2.X), p3.X), p4.X);
  BB.Top := System.Math.Min(System.Math.Min(System.Math.Min( p1.Y, p2.Y), p3.Y), p4.Y);
  BB.Right := System.Math.Max(System.Math.Max(System.Math.Max( p1.X, p2.X), p3.X), p4.X);
  BB.Bottom := System.Math.Max(System.Math.Max(System.Math.Max( p1.Y, p2.Y), p3.Y), p4.Y);

  SetBoundsRect(BB);

  PStart.X := x - BB.Left;
  PStart.Y := y - BB.Top;
  PEnd.X := x2 - BB.Left;
  PEnd.Y := y2 - BB.Top;

  P1.X := P1.X - BB.Left;
  P1.Y := P1.Y - BB.Top;
  P2.X := P2.X - BB.Left;
  P2.Y := P2.Y - BB.Top;
  P3.X := P3.X - BB.Left;
  P3.Y := P3.Y - BB.Top;
  P4.X := P4.X - BB.Left;
  P4.Y := P4.Y - BB.Top;

  {G1.X := P1.X + (P2.X - P1.X)/2;
  G1.Y := P1.Y + (P2.Y - P1.Y)/2;
  G2.X := P4.X + (P3.X - P4.X)/2;
  G2.Y := P4.Y + (P3.Y - P4.Y)/2;

  if (Width <> 0) and (Height <> 0) then begin
    G1.X := (G1.X - BB.Left) / (BB.Right - BB.Left);
    G1.Y := (G1.Y - BB.Top) / (BB.Bottom - BB.Top);
    G2.X := (G2.X - BB.Left) / (BB.Right - BB.Left);
    G2.Y := (G2.Y - BB.Top) / (BB.Bottom - BB.Top);
  end;

  Fill.Gradient.StartPosition.Point := PointF( G1.X, G1.Y);
  Fill.Gradient.StopPosition.Point  := PointF( G2.X, G2.Y);}

  FPolygon[0] := P1;
  FPolygon[1] := P2;
  FPolygon[2] := P3;
  FPolygon[3] := P4;
  FPolygon[4] := P1;
end;


//==============================================================================
//
//                               TG2CableV2
//
//==============================================================================

constructor TG2CableV2.Create(AOwner: TComponent);
var i : integer;
begin
  inherited;

  FTimer := TTimer.Create(self);
  FTimer.Interval := 25;

  FMargin := 10;

  FP1.X := FX + FMargin;
  FP1.Y := FY + FMargin;

  FP2.X := FX + FWidth - FMargin * 2;
  FP2.Y := FY + FMargin;

  AddNodes(2);

  FColor := claRed;

  FTimer.OnTimer := OnTimer;
end;

destructor TG2CableV2.Destroy;
begin
  //ClearNodes;
  Finalize(FNodes);

  FTimer.Enabled := False;
  FTimer.Free;

  inherited;
end;

function TG2CableV2.GetParent: TFmxObject;
begin
  Result := FParent;
end;

procedure TG2CableV2.SetParent(const Value: TFmxObject);
var i : integer;
begin
  FParent := Value;
  for i := 0 to FNodeCount - 1 do
    FNodes[i].Parent := Value;
end;

procedure TG2CableV2.AddNodes(aNodeCount: integer);
var i : integer;
begin
  ClearNodes;
  FNodeCount := aNodeCount;
  SetLength(FNodes, FNodeCount);
  for i := 0 to FNodeCount - 1 do begin
    FNodes[i] := TG2CableNode.Create(self);
    FNodes[i].Parent := FParent;
    FNodes[i].FStartNode := i = 0;
    FNodes[i].FEndNode := i = (FNodeCount-1);
  end;
end;

procedure TG2CableV2.ClearNodes;
var i : integer;
begin
  for i := 0 to FNodeCount - 1 do
    FNodes[i].Free;
  FNodeCount := 0;
  SetLength(FNodes, 0);
end;

procedure TG2CableV2.CalcNodePaths;
var i : integer;
begin
  for i := 0 to FNodeCount - 1 do
    FNodes[i].CalcPath;
end;

procedure TG2CableV2.InitCable;
var n : integer;
    l, dx, dy : single;
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
  min_x := System.Math.Min(FP1.X, FP2.X);
  max_x := System.Math.Max(FP1.X, FP2.X);
  min_y := System.Math.Min(FP1.Y, FP2.Y);
  max_y := System.Math.Max(FP1.Y, FP2.Y);

  l := sqrt((max_x - min_x)*(max_x - min_x) + (max_y - min_y)*(max_y - min_y));
  AddNodes(trunc(l/ 20)+2);

  n := FNodeCount - 1;

  dx := ( FP2.X - FP1.X) / FNodeCount;
  dy := ( FP2.Y - FP1.Y) / FNodeCount;

  halfx := ( FP2.X - FP1.X) / 2;
  maxsag := Caterny(-halfx);

  FNodes[n].x2 := FP2.X;
  FNodes[n].y2 := FP2.Y;

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

    if n > 0 then begin
      FNodes[n-1].x2 := FNodes[n].x;
      FNodes[n-1].y2 := FNodes[n].y;
    end;

    dec(n);
  end;
  FNodes[0].x := FP1.X;
  FNodes[0].y := FP1.y;

  FX := min_x - FMargin;
  FY := min_y - FMargin;
  FWidth := max_x - FX + FMargin;
  FHeight := max_y - FY + FMargin;

  CalcNodePaths;
end;

procedure TG2CableV2.IterateCable;
var n : integer;
    min_x, min_y, max_x, max_y : single;
begin

  min_x := System.Math.Min(FP1.X, FP2.X);
  max_x := System.Math.Max(FP1.X, FP2.X);
  min_y := System.Math.Min(FP1.Y, FP2.Y);
  max_y := System.Math.Max(FP1.Y, FP2.Y);

  n := FNodeCount - 1;

  FNodes[n].x2 := FP2.X;
  FNodes[n].y2 := FP2.Y;

  while (n >= 0) do begin

    if n > 0 then begin
    	FNodes[n].vx := FNodes[n].vx + ( FNodes[n].x2 + FNodes[n - 1].x - FNodes[n].x * 2 ) / TENSION;
	  	FNodes[n].vy := FNodes[n].vy + ( FNodes[n].y2 + FNodes[n - 1].y - FNodes[n].y * 2 ) / TENSION;
    end else begin
    	FNodes[n].vx := FNodes[n].vx + ( FNodes[n].x2 + FNodes[n].x - FNodes[n].x * 2 ) / TENSION;
	  	FNodes[n].vy := FNodes[n].vy + ( FNodes[n].y2 + FNodes[n].y - FNodes[n].y * 2 ) / TENSION;
    end;

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

    if n > 0 then begin
      FNodes[n-1].x2 := FNodes[n].x;
      FNodes[n-1].y2 := FNodes[n].y;
    end;

    dec(n);
  end;
  FNodes[0].x := FP1.X;
  FNodes[0].y := FP1.Y;

  FX := min_x - FMargin;
  FY := min_y - FMargin;
  FWidth := max_x - FX + FMargin;
  FHeight := max_y - FY + FMargin;

  CalcNodePaths;
end;

procedure TG2CableV2.OnTimer(Sender: TObject);
begin
  if FTimerCount <= 0 then
    FTimer.Enabled := False;
  Dec(FTimerCount);
  IterateCable;
  //Repaint;
end;

procedure TG2CableV2.SetPoint1X(const Value: single);
begin
  FP1.X := Value;
end;

procedure TG2CableV2.SetPoint1Y(const Value: single);
begin
  FP1.Y := Value;
end;

procedure TG2CableV2.SetPoint2X(const Value: single);
begin
  FP2.X := Value;
end;

procedure TG2CableV2.SetPoint2Y(const Value: single);
begin
  FP2.Y := Value;
end;

procedure TG2CableV2.StartTimer;
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
  FSVGControl := TG2Module.Create(self);
  FSVGControl.ModuleData := self;
end;

constructor TG2GraphModuleFMX.CopyCreate(aPatchPart: TG2FilePatchPart;
  aModule: TG2GraphModuleFMX);
begin
  inherited Create( aPatchPart);
  FSVGControl := TG2Module.Create(self);
  FSVGControl.ModuleData := self;
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

function TG2GraphModuleFMX.GetParent: TFMXObject;
begin
  if assigned(FSVGControl) then
    Result := FSVGControl.Parent
  else
    Result := nil;
end;

procedure TG2GraphModuleFMX.ParsePanelData;
begin
  if assigned(FSVGControl) then
    FSVGControl.ParsePanelData;
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

procedure TG2GraphModuleFMX.SetParent(const Value: TFMXObject);
begin
  if assigned(FSVGControl) then
    FSVGControl.Parent := Value;
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
//                               TG2Module
//
//==============================================================================

constructor TG2Module.Create(AOwner: TComponent);
begin
  inherited;
  //FZoom := 1;
  //FRedrawBuffer := True;
  //FBitmap := TBitmap.Create( trunc(Width) + 1, trunc(Height) + 1);
  Width := UNITS_COL;
  Height := UNITS_ROW;
end;

destructor TG2Module.Destroy;
begin
  //FBitmap.Free;
  inherited;
end;

{procedure TG2Module.AddGraphControl(aGraphCtrl: TG2Control);
begin
  FChildControls.Add( aGraphCtrl);
end;}

procedure TG2Module.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TG2Module then begin
    //FZoom := TG2Module(Source).FZoom;
    //FRedrawBuffer := TG2Module(Source).FRedrawBuffer;
    //FBitmap.Assign( TG2Module(Source).FBitmap);
//    FPanel := FindObject(ID + '_panel') as TSVGPath;
  end;
end;

procedure TG2Module.ParseDependencies( aControl : TG2Control;
   aMasterRef : integer; aDependencies : string; aTextFunction : integer);
var sl : TStringList;
    i, Value, c : integer;
begin
  if not assigned(ModuleData) then
    exit;

  sl := TStringList.Create;
  try
    sl.DelimitedText := aDependencies;

    // Find ref to master parameter
    if aMasterRef >= 0  then begin
      i := 0;
      while (i<sl.Count) and (sl[i]<>IntToStr(aMasterRef)) and (sl[i]<>'s'+IntToStr(aMasterRef)) do
        inc(i);

      if (i<sl.Count) then begin
        // Assign master parameter first
        if (Lowercase(sl[ i][1]) = 's') then begin // One of the static params
          aControl.Parameter := ModuleData.Mode[ aMasterRef] as TG2GraphParameterFMX;
          aControl.Parameter.TextFunctionIndex := aTextFunction;
        end else begin
          aControl.Parameter := ModuleData.Parameter[ aMasterRef] as TG2GraphParameterFMX;
          aControl.Parameter.TextFunctionIndex := aTextFunction;
        end;
      end else begin
        aControl.Parameter := ModuleData.Parameter[ aMasterRef] as TG2GraphParameterFMX;
        aControl.Parameter.TextFunctionIndex := aTextFunction;
      end;
    end;

    if assigned(aControl.Parameter) then begin
      for i := 0 to sl.Count - 1 do begin
        if (length(sl[i])>0) and (Lowercase(sl[i][1]) = 's') then begin
          val(copy(sl[i], 2, Length(sl[i]) - 1), Value, c);
          if c = 0 then begin
            aControl.Parameter.AddTextDependency( ptMode, Value);
            (ModuleData.Mode[ value] as TG2GraphParameterFMX).AssignControl(aControl);
          end;
        end else begin
          val(sl[i], Value, c);
          if c = 0 then begin
            aControl.Parameter.AddTextDependency( ptParam, Value);
            (ModuleData.Parameter[ value] as TG2GraphParameterFMX).AssignControl(aControl);
          end;
        end;
      end;
    end;
  finally
    sl.Free;
  end;
end;

procedure TG2Module.ParsePanelData;
var ModuleStream : TModuleDefStream;
    CodeRef, Err : integer;
    aPath : string;
    aName, aValue, ControlType, CodeRefStr : AnsiString;
    ChildControl : TG2Control;
    Connector : TG2Connector;
    Param : TG2GraphParameterFMX;
    //Patch : TG2GraphPatchFMX;
begin
  aPath := ExtractFilePath(ParamStr(0));
  aPath := aPath + '\Modules\';

  //Patch := GetPatch;

  if FileExists( aPath + string(ModuleData.ModuleFileName) + '.txt') then begin
    ModuleStream := TModuleDefStream.Create(aPath + string(ModuleData.ModuleFileName) + '.txt');
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
              Connector := TG2Connector.Create(self);
              Connector.ModuleData := ModuleData;
              Connector.Data := ModuleData.InConnector[ CodeRef];
              if not assigned(Connector.Data) then
                raise Exception.Create('Data for connector not found...');

              Connector.ParsePanelData( ModuleStream);
              //AddGraphControl( Connector);
              Connector.Parent := self;
              Connector.OnMouseUp := OnControlMouseUp;
              Connector.OnMouseDown := OnControlMouseDown;
              Connector.OnMouseMove := OnControlMouseMove;
            end else
              raise Exception.Create('Parse error, module  ' + string(ModuleData.ModuleName) + ' input connector CodeRef not found.' );

          end else
            if ControlType = 'Output' then begin

              CodeRefStr := ModuleStream.PeekValue( 'CodeRef:', [#13, #10, '#'], ['#', '>']);
              val( string(CodeRefStr), CodeRef, Err);
              if Err = 0 then begin
                Connector := TG2Connector.Create(self);
                Connector.ModuleData := ModuleData;
                Connector.Data := ModuleData.OutConnector[ CodeRef];
                if Connector.Data = nil then
                  raise Exception.Create('Data for connector not found...');

                Connector.ParsePanelData( ModuleStream);
                //AddGraphControl( Connector);
                Connector.Parent := self;
                Connector.OnMouseUp := OnControlMouseUp;
                Connector.OnMouseDown := OnControlMouseDown;
                Connector.OnMouseMove := OnControlMouseMove;
              end else
                raise Exception.Create('Parse error, module  ' + string(ModuleData.ModuleName) + ' output connector CodeRef not found.' );

            end else begin
              ChildControl := NewG2GraphControl(ControlType);
              if ChildControl <> nil then begin

                //AddGraphControl( ChildControl);
                ChildControl.Parent := self;

                if ( ControlType = 'Knob') or
                   ( ControlType = 'ButtonIncDec') or
                   ( ControlType = 'ButtonRadio') or
                   ( ControlType = 'ButtonRadioEdit') or
                   ( ControlType = 'LevelShift') or
                   ( ControlType = 'ButtonFlat') or
                   ( ControlType = 'TextEdit') or
                   ( ControlType = 'PartSelector') or
                   ( ControlType = 'ButtonText') then begin

                  CodeRefStr := ModuleStream.PeekValue( 'CodeRef:', [#13, #10, '#'], ['#', '>']);
                  val( string(CodeRefStr), CodeRef, Err);
                  if Err = 0 then begin
                    if ControlType = 'PartSelector' then begin
                      Param := ModuleData.Mode[ CodeRef] as TG2GraphParameterFMX;
                    end else begin
                      Param := ModuleData.Parameter[ CodeRef] as TG2GraphParameterFMX;
                    end;
                    ChildControl.ParsePanelData( ModuleStream);
                    ChildControl.Parameter := Param;
                  end else
                    raise Exception.Create('Parse error, module  ' + string(ModuleData.ModuleName) + ' parameter CodeRef not found.' );

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

        //FChildControls.Sort(@CompareZOrder);
      end else
        raise Exception.Create('Unknown file type.');

      //Patch.SortLeds;
    finally
      ModuleStream.Free;
    end;
  end;
end;

function TG2Module.GetControlType( aG2GraphChildControl: TG2Control): string;
begin
{  if aG2GraphChildControl is TG2GraphLabel then
    Result := 'Label';

  if aG2GraphChildControl is TG2GraphConnector then
    Result := 'Connector';

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

  if aG2GraphChildControl is TG2GraphTextEdit then
    Result := 'TextEdit';

  if aG2GraphChildControl is TG2GraphButtonFlat then
    Result := 'ButtonFlat';

  if aG2GraphChildControl is TG2GraphLevelShift then
    Result := 'LevelShift';

  if aG2GraphChildControl is  TG2GraphKnob then
    Result := 'Knob';

  if aG2GraphChildControl is  TG2GraphBitmap then
    Result := 'Bitmap'; }
end;

function TG2Module.NewG2GraphControl( aControlType : AnsiString) : TG2Control;
begin
  Result := nil;

  if (aControlType = 'Line') then begin
//    Result := TG2GraphLine.Create(self);
//    Result.Module := self;
    Result := TG2Control.Create(self);
    Result.ModuleData := ModuleData;
  end;

  if (aControlType = 'Text') then begin
    Result := TG2Label.Create(self);
    Result.ModuleData := ModuleData;
  end;

  if aControlType = 'Connector' then begin
    Result := TG2Connector.Create(self);
    Result.ModuleData := ModuleData;
    Result.OnMouseUp := OnControlMouseUp;
    Result.OnMouseDown := OnControlMouseDown;
    Result.OnMouseMove := OnControlMouseMove;
  end;

  if (aControlType = 'TextField') then begin
    Result := TG2TextField.Create(self);
    Result.ModuleData := ModuleData;
  end;

  if (aControlType = 'Graph') then begin
 //   Result := TG2GraphGraph.Create(self);
//    Result.Module := self;
    Result := TG2Control.Create(self);
    Result.ModuleData := ModuleData;
    Result.OnMouseUp := OnControlMouseUp;
    Result.OnMouseDown := OnControlMouseDown;
    Result.OnMouseMove := OnControlMouseMove;
  end;

  if (aControlType = 'Led') then begin
    Result := TG2LedGreen.Create(self);
    Result.ModuleData := ModuleData;
  end;

  if (aControlType = 'MiniVU') then begin
    Result := TG2MiniVU.Create(self);
    Result.ModuleData := ModuleData;
  end;

  if (aControlType = 'PartSelector') then begin
    Result := TG2PartSelector.Create(self);
    Result.ModuleData := ModuleData;
    Result.OnMouseUp := OnControlMouseUp;
    Result.OnMouseDown := OnControlMouseDown;
    Result.OnMouseMove := OnControlMouseMove;
  end;

  if (aControlType = 'ButtonText') then begin
    Result := TG2BtnText.Create(self);
    Result.ModuleData := ModuleData;
    Result.OnMouseUp := OnControlMouseUp;
    Result.OnMouseDown := OnControlMouseDown;
    Result.OnMouseMove := OnControlMouseMove;
  end;

  if (aControlType = 'TextEdit') then begin
    Result := TG2BtnText.Create(self);
    Result.ModuleData := ModuleData;
    Result.OnMouseUp := OnControlMouseUp;
    Result.OnMouseDown := OnControlMouseDown;
    Result.OnMouseMove := OnControlMouseMove;
  end;

  if (aControlType = 'ButtonFlat') then begin
    Result := TG2BtnFlat.Create(self);
    Result.ModuleData := ModuleData;
    Result.OnMouseUp := OnControlMouseUp;
    Result.OnMouseDown := OnControlMouseDown;
    Result.OnMouseMove := OnControlMouseMove;
  end;

  if (aControlType = 'LevelShift') then begin
//    Result := TG2GraphLevelShift.Create(self);
//    Result.Module := self;
    Result := TG2Control.Create(self);
    Result.ModuleData := ModuleData;
    Result.OnMouseUp := OnControlMouseUp;
    Result.OnMouseDown := OnControlMouseDown;
    Result.OnMouseMove := OnControlMouseMove;
  end;

  if (aControlType = 'ButtonRadio') then begin
    Result := TG2BtnRadio.Create(self);
    Result.ModuleData := ModuleData;
    Result.OnMouseUp := OnControlMouseUp;
    Result.OnMouseDown := OnControlMouseDown;
    Result.OnMouseMove := OnControlMouseMove;
  end;

  if (aControlType = 'ButtonRadioEdit') then begin
    Result := TG2BtnRadioEdit.Create(self);
    Result.ModuleData := ModuleData;
    Result.OnMouseUp := OnControlMouseUp;
    Result.OnMouseDown := OnControlMouseDown;
    Result.OnMouseMove := OnControlMouseMove;
  end;

  if (aControlType = 'ButtonIncDec') then begin
    Result := TG2BtnIncDec.Create(self);
    Result.ModuleData := ModuleData;
    Result.OnMouseUp := OnControlMouseUp;
    Result.OnMouseDown := OnControlMouseDown;
    Result.OnMouseMove := OnControlMouseMove;
  end;

  if (aControlType = 'Knob') or (aControlType = '') then begin
    Result := TG2Knob.Create(self);
    Result.ModuleData := ModuleData;
    Result.OnMouseUp := OnControlMouseUp;
    Result.OnMouseDown := OnControlMouseDown;
    Result.OnMouseMove := OnControlMouseMove;
  end;

  if (aControlType = 'Bitmap') then begin
//    Result := TG2GraphBitmap.Create(self);
//    Result.Module := self;
    Result := TG2Control.Create(self);
    Result.ModuleData := ModuleData;
  end;

  //if not assigned( Result)  then
  //  raise Exception.Create('Unknown control type ' + aControlType);
end;


function TG2Module.GetNewCol: TBits7;
begin
  Result := trunc((Position.X) / UNITS_COL);
end;

function TG2Module.GetNewRow: TBits7;
begin
  Result := trunc((Position.Y) / UNITS_ROW);
end;

procedure TG2Module.Paint;
var SaveMatrix : TMatrix;
    Save: TCanvasSaveState;
begin
  inherited;
{  if FRedrawBuffer then begin
    FBitmap.Canvas.BeginScene;
    try
      SaveMatrix := FBitmap.Canvas.Matrix;
      FBitmap.Canvas.SetMatrix( CreateScaleMatrix(FZoom, FZoom));
      //SVGPaint( FBitmap.Canvas);
    finally
      FBitmap.Canvas.SetMatrix(SaveMatrix);
      FRedrawBuffer := False;
      FBitmap.Canvas.EndScene;
    end;
  end;
  Canvas.DrawBitmap( FBitmap, RectF(0, 0, Width, Height), RectF(0, 0, Width, Height), AbsoluteOpacity);
}end;

procedure TG2Module.Redraw;
begin
//  FRedrawBuffer := True;
  Repaint;
end;

procedure TG2Module.SetModuleData(const aValue: TG2GraphModuleFMX);

  procedure SetChildModuleData( aSVGGroup : TSVGGroup);
  var i : integer;
  begin
{    for i := 0 to aSVGGroup.Count - 1 do begin

      if aSVGGroup.Item[i] is TSVGG2Control then begin
        (aSVGGroup.Item[i] as TSVGG2Control).ModuleData := FModuleData;
      end;

      if aSVGGroup.Item[i] is TSVGGroup then
        SetChildModuleData( aSVGGroup.Item[i] as TSVGGroup);
    end;}
  end;

begin
  if FModuleData <> aValue then begin
    FModuleData := aValue;
    //SetChildModuleData(self);
  end;
end;

procedure TG2Module.SetModuleColor(aValue: TBits8);
begin
  if assigned(SVGAgent) then begin
    SVGAgent.ParseSVGPaintServer( 'PanelGradient_' + IntToStr(aValue), FPanel.Fill, RectF(0,0,1,1), RectF(0,0,1,1));
  end;
end;

procedure TG2Module.SetSelected(aValue: boolean);
begin
  if aValue then
    Stroke.Color := claWhite
  else
    Stroke.Color := claBlack;
  Repaint;
end;

procedure TG2Module.SetZoom(aValue: single);
begin
  {if aValue <> FZoom then begin
    FZoom := aValue;
    Position.X := FModuleData.Col * UNITS_COL * FZoom;
    Position.Y := FModuleData.Row * UNITS_ROW * FZoom;

    Width := UNITS_COL * FZoom;
    Height := FModuleData.HeightUnits * UNITS_ROW * FZoom;
    FRedrawBuffer := True;
  end;}
end;

procedure TG2Module.Resize;
begin
  {if (FBitmap.Width <> trunc(Width) + 1) or (FBitmap.Height <> trunc(Height) + 1) then

  FBitmap.SetSize( trunc(Width) + 1, trunc(Height) + 1);
  FRedrawBuffer := True;}
end;

//==============================================================================
//
//                            TModuleBitmapBuffer
//
//==============================================================================

constructor TModuleBitmapBuffer.Create(AOwner: TComponent);
begin
  inherited;

  FBackGround := nil;
  FBGBuffer := TBitmap.Create( UNITS_COL, UNITS_ROW);

  FZoom := 1;

  Hittest := False;
end;

destructor TModuleBitmapBuffer.Destroy;
begin
  FBGBuffer.Free;
  inherited;
end;

procedure TModuleBitmapBuffer.CalcBounds;
begin
  SetBounds(0, 0, (MaxCol + 3) * UNITS_COL, (MaxRow + 6) * UNITS_ROW);
end;

function TModuleBitmapBuffer.GetMaxCol: integer;
var Module : TG2Module;
    i : integer;
begin
  Result := 0;
  if assigned(FModuleList) then begin
    for i := 0 to FModuleList.Count - 1 do begin
      Module := (FModuleList[i] as TG2GraphModuleFMX).FSVGControl;
      if Module.ModuleData.Col > Result then
        Result := Module.ModuleData.Col;
    end;
  end;
end;

function TModuleBitmapBuffer.GetMaxRow: integer;
var Module : TG2Module;
    i : integer;
begin
  Result := 0;
  if assigned(FModuleList) then begin
    for i := 0 to FModuleList.Count - 1 do begin
      Module := (FModuleList[i] as TG2GraphModuleFMX).FSVGControl;
      if Module.ModuleData.Row > Result then
        Result := Module.ModuleData.Row;
    end;
  end;
end;

procedure TModuleBitmapBuffer.Paint;
var i, j, r, c, w, h : integer;
    Save: TCanvasSaveState;
begin
  inherited;
  exit;

  Canvas.BeginScene;
  try
    Save := Canvas.SaveState;

    w := FBGBuffer.Width;
    h := FBGBuffer.Height;
    c := trunc(Width / w) + 1;
    r := trunc(Height / h) + 1;
    for i := 0 to r - 1 do begin
      for j := 0 to c - 1 do begin
        Canvas.DrawBitmap( FBGBuffer, RectF(0, 0, w, h), RectF(j*w, i*h, j*w+w, i*h+h), AbsoluteOpacity);
      end;
    end;
  finally
    Canvas.RestoreState(Save);
    Canvas.EndScene;
  end;

  inherited;
end;

procedure TModuleBitmapBuffer.SetZoom(aValue: single);
var i : integer;
    SVGModule : TG2Module;
    id : string;
    SaveMatrix : TMatrix;
begin
  exit;

  if aValue <> FZoom then begin
    {FZoom := aValue;

    FBGBuffer.SetSize( trunc(UNITS_COL*FZoom) , trunc(UNITS_ROW*FZoom));

    if not(assigned(FBackground)) and assigned(SVGAgent) then begin
      id := 'patchbackground';

      FBackground := TG2Module.Create( self);
      SVGAgent.ParseSVG( id, FBackground, CreateUnityMatrix);
    end;

    FBGBuffer.Canvas.BeginScene;
    try
      SaveMatrix := FBGBuffer.Canvas.Matrix;
      FBGBuffer.Canvas.SetMatrix( CreateScaleMatrix(FZoom, FZoom));
      FBGBuffer.Canvas.Fill.Color := claRed;
      FBackground.SVGPaint(FBGBuffer.Canvas);
    finally
      FBGBuffer.Canvas.SetMatrix(SaveMatrix);
      FBGBuffer.Canvas.EndScene;
    end;

    FMaxCol := 0;
    FMaxRow := 0;
    for i := 0 to FModuleList.Count - 1 do begin
      SVGModule := (FModuleList[i] as TG2GraphModuleFMX).FSVGControl;
      SVGModule.Zoom := FZoom;

      if SVGModule.ModuleData.Row > FMaxRow then
        FMaxRow := SVGModule.ModuleData.Row;

      if SVGModule.ModuleData.Col > FMaxCol then
        FMaxCol := SVGModule.ModuleData.Col;
    end;}
  end;
end;




end.


