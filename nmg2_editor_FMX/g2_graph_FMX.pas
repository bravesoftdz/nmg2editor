unit g2_graph_FMX;

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
  System.Contnrs,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.Layouts,
  g2_types, g2_file, g2_usb,
  BVE.SVGControl, BVE.SVGXMLWrapperDelphi,
  //SVGXMLWrapperNativeXML,
  math;

type
  TG2GraphModuleFMX = class;

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

    FLayout             : TFMXObject;

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
    property    Layout : TFMXObject read FLayout write FLayout;

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
    procedure   ParsePanelData( aParent : TFMXObject; aModuleType : byte);
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
    //procedure   NotifyControls( Event: TG2ParamEvent; Info: NativeInt);
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

  TSVGG2Graphic = class(TSVGGroup)
  private
    FModulePanel : TSVGG2Module;
    FCtrlType : string;
    FCtrlStyle : string;
  public
    constructor Create( AOwner: TComponent; aId : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix); override;
    destructor  Destroy; override;

    function   CreateCopy( AOwner : TComponent; aSVGParent : TSVGGroup): TSVGObject; override;
    procedure  Assign(Source: TPersistent); override;

    function  FindOwnerModule: TSVGG2Module;

    procedure Redraw; virtual;

    property ModulePanel : TSVGG2Module read FModulePanel write FModulePanel;
    property CtrlType : string read FCtrlType write FCtrlType;
    property CtrlStyle : string read FCtrlStyle write FCtrlStyle;
  end;

  TSVGG2ParamLink = class(TSVGG2Graphic)
  private
    FCodeRef : integer;
    FMasterRef : integer;
    FInfoFunc : integer;
    FTextFunc : integer;
    FDependencies : string;
    FCtrlType : string;
    FCtrlStyle : string;
  public
    //constructor Create( AOwner: TComponent); override;
    constructor Create( AOwner: TComponent; aId : string; aSVGParent: TSVGGroup; aCTM, aUserMatrix : TMatrix); override;
    destructor  Destroy; override;

    function   CreateCopy( AOwner : TComponent; aSVGParent : TSVGGroup): TSVGObject; override;
    procedure  Assign(Source: TPersistent); override;

    function  CreateGroup(  AOwner : TComponent; aNode : TSVGNode; aId : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix; var aSubTreeOwner : TSVGGroup) : TSVGGroup; override;
  end;

  TSVGG2ConnLink = class(TSVGG2Graphic)
  private
    FCodeRef : integer;
    FKind : TConnectorKind;
  public
    constructor Create( AOwner: TComponent;  aId : string; aSVGParent: TSVGGroup; aCTM, aUserMatrix : TMatrix); override;
    destructor  Destroy; override;

    function  CreateCopy( AOwner : TComponent; aSVGParent : TSVGGroup): TSVGObject; override;
    procedure Assign(Source: TPersistent); override;

    function  CreateGroup(  AOwner : TComponent; aNode : TSVGNode; aId : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix; var aSubTreeOwner : TSVGGroup) : TSVGGroup; override;
  end;

  TSVGHitPath = class;

  TSVGG2Control = class(TSVGG2Graphic)
  private
    FModuleData : TG2GraphModuleFMX;
    FParameter : TG2GraphParameterFMX;
    FCodeRef : integer;
  protected
    function   GetValue: single;
    procedure  SetParameter(const aValue: TG2GraphParameterFMX); virtual;
    procedure  SetModuleData(const aValue : TG2GraphModuleFMX); virtual;
    procedure  Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure  ParamEvent( Event: TG2ParamEvent; Info: NativeInt); virtual;
  public
    constructor Create( AOwner: TComponent; aId : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix); override;
    destructor Destroy; override;

    function   CreateCopy( AOwner : TComponent; aSVGParent : TSVGGroup): TSVGObject; override;
    procedure  Assign(Source: TPersistent); override;

    procedure MouseDown( P : TPointF; aHitPath : TSVGHitPath); virtual;
    procedure MouseMove( P : TPointF; aHitPath : TSVGHitPath; dx, dy : single); virtual;
    procedure MouseUp( P : TPointF; aHitPath : TSVGHitPath); virtual;
    procedure MouseLeave; virtual;

    property Parameter : TG2GraphParameterFMX read FParameter write SetParameter;
    property ModuleData : TG2GraphModuleFMX read FModuleData write SetModuleData;
  end;

  TSVGG2Connector = class(TSVGG2Control)
  private
    FModuleData : TG2GraphModuleFMX;
    FData   : TG2FileConnector;
    FBorder : TSVGHitPath;
    FKind : TConnectorKind;
    FCodeRef : integer;
  protected
    procedure SetData( aValue: TG2FileConnector);
    procedure SetModuleData(const aValue: TG2GraphModuleFMX); override;
  public
    constructor Create( AOwner: TComponent; aId : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix); override;
    destructor  Destroy; override;

    function   CreateCopy( AOwner : TComponent; aSVGParent : TSVGGroup): TSVGObject; override;
    procedure  Assign(Source: TPersistent); override;

    function   CreatePath(  AOwner : TComponent; aNode : TSVGNode; aId : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix) : TSVGPath; override;

    property Data : TG2FileConnector read FData write SetData;
    property ModuleData : TG2GraphModuleFMX read FModuleData write SetModuleData;
  end;

  TSVGHitPath = class(TSVGPath)
  private
    FControl : TSVGG2Control;
    FBtnIndex : integer;
  public
    constructor Create( AOwner: TComponent); override;
    constructor Create( AOwner: TComponent; aId : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix); override;
    destructor  Destroy; override;

    function   CreateCopy( AOwner : TComponent; aSVGParent : TSVGGroup): TSVGObject; override;
    procedure  Assign(Source: TPersistent); override;

    procedure   MouseDown( P : TPointF); virtual;
    procedure   MouseMove( P : TPointF; dx, dy : single); virtual;
    procedure   MouseUp( P : TPointF); virtual;
    procedure   MouseLeave; virtual;

    property    Control : TSVGG2Control read FControl write FControl;
    property    BtnIndex : integer read FBtnIndex write FBtnIndex;
  end;

  TSVGLabel = class(TSVGSpan)
  private
    FValue : string;
    FTextAlign : TTextAlign;
  protected
    procedure SetValue(const aValue: string);
    procedure SetTextAlign(const aValue: TTextAlign);
    procedure SetSpanText(const aValue: string); override;
  public
    constructor Create( AOwner: TComponent; aId : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix); override;
    destructor  Destroy; override;

    function   CreateCopy( AOwner : TComponent; aSVGParent : TSVGGroup): TSVGObject; override;
    procedure  Assign(Source: TPersistent); override;

    procedure SVGPaint( aCanvas : TCanvas); override;

    property Value : string read FValue write SetValue;
    property TextAlign : TTextAlign read FTextAlign write SetTextAlign;
  end;

  TSVGBtnState = class(TSVGG2Graphic)
  private
    FControl : TSVGG2Control;
    FBtnIndex : integer;
    FBtnBackground : TSVGHitPath;
    FLabel : TSVGLabel;
  public
    //constructor Create( AOwner: TComponent); override;
    constructor Create( AOwner: TComponent; aId : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix); override;
    destructor  Destroy; override;

    function   CreateCopy( AOwner : TComponent; aSVGParent : TSVGGroup): TSVGObject; override;
    procedure  Assign(Source: TPersistent); override;

    function    CreatePath(  AOwner : TComponent; aNode : TSVGNode; aId : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix) : TSVGPath; override;
    function    CreateSpan(  AOwner : TComponent; aNode : TSVGNode; aID : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix) : TSVGSpan; override;

    property    Control : TSVGG2Control read FControl write FControl;
  end;

  TSVGTextField = class(TSVGG2Control)
  private
    FWindow     : TSVGPath;
    FSimpleText : TSVGLabel;
    FValue : string;
  protected
    procedure   SetValue( aValue : string);
    procedure   ParamEvent( Event: TG2ParamEvent; Info: NativeInt); override;
    procedure   SetModuleData(const aValue: TG2GraphModuleFMX); override;
  public
    constructor Create( AOwner: TComponent; aId : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix); override;
    destructor  Destroy; override;

    function   CreateCopy( AOwner : TComponent; aSVGParent : TSVGGroup): TSVGObject; override;
    procedure  Assign(Source: TPersistent); override;

    function    CreateSpan(  AOwner : TComponent; aNode : TSVGNode; aID : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix) : TSVGSpan; override;
    function    CreatePath(  AOwner : TComponent; aNode : TSVGNode; aId : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix) : TSVGPath; override;

    property    Value : string read FValue write SetValue;
  end;

  TSVGBtn = class(TSVGG2Control)
  private
    FValue : single;
    FBtnUp : TSVGBtnState;
    FBtnDown : TSVGBtnState;
    FLabel : TSVGLabel;
    FBtnType : TBtnType;
    FBtnIndex : integer;
    FOnChange : TNotifyEvent;
  protected
    procedure SetValue( aValue : single);
    procedure ParamEvent( Event: TG2ParamEvent; Info: NativeInt); override;
    procedure SetModuleData(const aValue : TG2GraphModuleFMX); override;
  public
    constructor Create( AOwner: TComponent; aId : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix); override;
    destructor  Destroy; override;

    function  CreateCopy( AOwner : TComponent; aSVGParent : TSVGGroup): TSVGObject; override;
    procedure Assign(Source: TPersistent); override;

    procedure Redraw; override;

    function  CreateGroup(  AOwner : TComponent; aNode : TSVGNode; aId : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix; var aSubTreeOwner : TSVGGroup) : TSVGGroup; override;
    function  CreateSpan(  AOwner : TComponent; aNode : TSVGNode; aID : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix) : TSVGSpan; override;

    procedure MouseDown( P : TPointF; aHitPath : TSVGHitPath); override;
    procedure MouseUp( P : TPointF; aHitPath : TSVGHitPath); override;

    property Value : single read FValue write SetValue;
    property BtnType : TBtnType read FBtnType write FBtnType;
    property OnChange : TNotifyEvent read FOnChange write FOnChange;
  end;

  TSVGBtnIncDec = class(TSVGG2Control)
  private
    FValue : byte;
    FBtnDec : TSVGBtn;
    FBtnInc : TSVGBtn;
  protected
    procedure   SetValue( aValue : byte);
    procedure   SetParameter(const Value: TG2GraphParameterFMX); override;
    procedure   SetModuleData(const aValue : TG2GraphModuleFMX); override;
    procedure   ParamEvent( Event: TG2ParamEvent; Info: NativeInt); override;
    procedure   IncBtnChange( Sender : TObject);
    procedure   DecBtnChange( Sender : TObject);
  public
    constructor Create( AOwner: TComponent; aId : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix); override;
    destructor  Destroy; override;

    function  CreateCopy( AOwner : TComponent; aSVGParent : TSVGGroup): TSVGObject; override;
    procedure Assign(Source: TPersistent); override;

    procedure   Redraw; override;

    function    CreateUse(  AOwner : TComponent; aUseNode, aRefNode : TSVGNode; aUseID : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix; var aSubTreeOwner : TSVGGroup) : TSVGGroup; override;

    property    Value : byte read FValue write SetValue;
  end;

  {TSVGBtnFlatOption = class(TSVGG2Graphic)
  private
    FControl : TSVGG2Control;
    FBtnIndex : integer;
    FBtnFace : TSVGHitPath;
    FLabel : TSVGLabel;
  public
    constructor Create( aControl: TSVGG2Control; aBtnIndex : integer; aId : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix);
    destructor  Destroy; override;

    function    CreateSpan( aNode : TSVGNode; aID : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix) : TSVGSpan; override;
    function    CreatePath( aNode : TSVGNode; aId : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix) : TSVGPath; override;
    property    Control : TSVGG2Control read FControl write FControl;
  end;}

  TSVGBtnFlat = class(TSVGG2Control)
  private
    FValue : integer;
    FButtons : TObjectList;
    FOnChange : TNotifyEvent;
  protected
    procedure   SetValue( aValue : integer);
    procedure   ParamEvent( Event: TG2ParamEvent; Info: NativeInt); override;
    procedure   SetModuleData(const aValue: TG2GraphModuleFMX); override;
  public
    constructor Create( AOwner: TComponent; aId : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix); override;
    destructor  Destroy; override;

    function  CreateCopy( AOwner : TComponent; aSVGParent : TSVGGroup): TSVGObject; override;
    procedure Assign(Source: TPersistent); override;

    procedure UpdateParts;
    procedure Redraw; override;

    function  CreateUse(  AOwner : TComponent; aUseNode, aRefNode : TSVGNode; aUseID : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix; var aSubTreeOwner : TSVGGroup) : TSVGGroup; override;

    procedure MouseDown( P : TPointF; aHitPath : TSVGHitPath); override;

    property Value : integer read FValue write SetValue;
    property OnChange : TNotifyEvent read FOnChange write FOnChange;
  end;

  TSVGBtnRadio = class(TSVGG2Control)
  private
    FValue : integer;
    FButtons : TObjectList;
  protected
    procedure SetModuleData(const aValue : TG2GraphModuleFMX); override;
    procedure SetValue( aValue : integer);
    procedure ParamEvent( Event: TG2ParamEvent; Info: NativeInt); override;
    procedure BtnChange( Sender : TObject);
  public
    constructor Create( AOwner: TComponent; aId : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix); override;
    destructor  Destroy; override;

    function  CreateCopy( AOwner : TComponent; aSVGParent : TSVGGroup): TSVGObject; override;
    procedure Assign(Source: TPersistent); override;

    procedure UpdateParts;

    function  CreateUse(  AOwner : TComponent; aUseNode, aRefNode : TSVGNode; aUseID : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix; var aSubTreeOwner : TSVGGroup) : TSVGGroup; override;

    property Value : integer read FValue write SetValue;
  end;

  TSVGKnob = class(TSVGG2Control)
  private
    FKnobNeedle : TSVGGroup;
    FKnobSel : TSVGHitPath;
    FKnobFace : TSVGHitPath;
    FKnobMorph : TSVGPath;
    FKnobBtns : TSVGGroup;
    FBtnReset : TSVGBtn;

    FValue : single;
    FStartPoint : TPointF;
    FStartValue : single;

    procedure SetLayoutAngle( aLayout : TSVGGroup; aAngle : single);
  protected
    procedure SetModuleData(const aValue : TG2GraphModuleFMX); override;
    procedure SetValue( aValue : single);
    procedure ResetBtnChange( Sender : TObject);
    procedure ParamEvent( Event: TG2ParamEvent; Info: NativeInt); override;
  public
    constructor Create( AOwner: TComponent; aId : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix); override;
    destructor  Destroy; override;

    function  CreateCopy( AOwner : TComponent; aSVGParent : TSVGGroup): TSVGObject; override;
    procedure Assign(Source: TPersistent); override;
    procedure InitSVGParentLinkage( aSVGChildObject : TSVGObject); override;

    procedure   UpdateParts;

    function    CreatePath(  AOwner : TComponent; aNode : TSVGNode; aId : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix) : TSVGPath; override;
    function    CreateGroup(  AOwner : TComponent; aNode : TSVGNode; aId : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix; var aSubTreeOwner : TSVGGroup) : TSVGGroup; override;
    function    CreateUse(  AOwner : TComponent; aUseNode, aRefNode : TSVGNode; aUseID : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix; var aSubTreeOwner : TSVGGroup) : TSVGGroup; override;

    procedure   MouseDown( P : TPointF; aHitPath : TSVGHitPath); override;
    procedure   MouseMove( P : TPointF; aHitPath : TSVGHitPath; dx, dy : single); override;
    procedure   MouseUp( P : TPointF; aHitPath : TSVGHitPath); override;
    procedure   MouseLeave; override;

    property    Value : single read FValue write SetValue;
  end;

  TSVGSlider = class(TSVGG2Control)
  private
    FSliderSel : TSVGHitPath;
    FSliderFace : TSVGHitPath;
    FSliderMorph : TSVGPath;
    FSliderBtn : TSVGBtnState;
    FBtnIncDec : TSVGBtnIncDec;

    FValue : single;
    FStartPoint : TPointF;
    FStartValue : single;

    procedure   SetSliderPos( aValue : single);
  protected
    procedure   SetValue( aValue : single);
    procedure   ParamEvent( Event: TG2ParamEvent; Info: NativeInt); override;
  public
    constructor Create( AOwner: TComponent; aId : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix); override;
    destructor  Destroy; override;

    procedure   UpdateParts;

    function    CreatePath(  AOwner : TComponent; aNode : TSVGNode; aId : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix) : TSVGPath; override;
    function    CreateUse(  AOwner : TComponent; aUseNode, aRefNode : TSVGNode; aUseID : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix; var aSubTreeOwner : TSVGGroup) : TSVGGroup; override;

    procedure   MouseDown( P : TPointF; aHitPath : TSVGHitPath); override;
    procedure   MouseMove( P : TPointF; aHitPath : TSVGHitPath; dx, dy : single); override;
    procedure   MouseUp( P : TPointF; aHitPath : TSVGHitPath); override;
    procedure   MouseLeave; override;

    property    Value : single read FValue write SetValue;
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

  TSVGG2Module = class(TSVGGroup)
  private
    FPanel : TSVGPath;
    FModuleData : TG2GraphModuleFMX;
    FBitmap : TBitmap;
    FZoom : single;
    FRedrawBuffer : boolean;
    procedure SetModuleData(const aValue: TG2GraphModuleFMX);
  protected
    procedure   SetZoom( aValue : single);
    procedure   Resize; override;
  public
    constructor Create( AOwner: TComponent); override;
    constructor Create( AOwner: TComponent; aId : string; aSVGParent: TSVGGroup; aCTM, aUserMatrix : TMatrix); override;
    destructor  Destroy; override;

    function   CreateCopy( AOwner : TComponent; aSVGParent : TSVGGroup): TSVGObject; override;
    procedure  Assign(Source: TPersistent); override;
    //procedure  AddSVGChild( aSVGObject : TSVGObject); override;

    procedure   Paint; override;
    procedure   Redraw;

    function    CreateSpan(  AOwner : TComponent; aNode : TSVGNode; aID : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix) : TSVGSpan; override;
    function    CreatePath(  AOwner : TComponent; aNode : TSVGNode; aId : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix) : TSVGPath; override;
    //function    CreateGroup(  AOwner : TComponent; aNode : TSVGNode; aId : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix; var aSubTreeOwner : TSVGGroup) : TSVGGroup; override;
    function    CreateUse(  AOwner : TComponent; aUseNode, aRefNode : TSVGNode; aUseID : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix; var aSubTreeOwner : TSVGGroup) : TSVGGroup; override;

    procedure   ParseDependencies( aControl : TSVGG2Control; aMasterRef : integer; aDependencies : string; aTextFunction : integer);

    function    SelectControl( P : TPointF): TSVGG2Control;
    function    SelectHitpath( P : TPointF): TSVGHitpath;

    function    GetNewCol: TBits7;
    function    GetNewRow: TBits7;
    procedure   SetModuleColor( aValue : TBits8);

    procedure   SetSelected( aValue : boolean);

    property    ModuleData : TG2GraphModuleFMX read FModuleData write SetModuleData;
    property    Zoom : single read FZoom write SetZoom;
  end;

  TModuleBitmapBuffer = class(TControl)
  private
    FBackGround : TSVGGroup;
    FBGBuffer : TBitmap;
    FModuleList : TModuleList;
    FZoom : single;
    FMaxCol, FMaxRow : integer;
  protected
    procedure SetZoom( aValue : single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Paint; override;

    property ModuleList : TModuleList read FModuleList write FModuleList;
    property Zoom : single read FZoom write SetZoom;
    property MaxCol : integer read FMaxCol;
    property MaxRow : integer read FMaxRow;
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
    SVGModule : TSVGG2Module;
begin
  if assigned(SVGAgent) then begin
    for i := 0 to FModuleDefList.Count - 1 do begin
      id := 'module_' + IntToStr(FModuleDefList.ModuleDef[i].ModuleType);
      SVGModule := TSVGG2Module.Create( self, id, nil, CreateUnityMatrix, CreateUnityMatrix);
      SVGAgent.ParseSVG( id, SVGModule, CreateUnityMatrix);
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
          Module.ParsePanelData( FLayout, aModuleType);
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
    FControlList[i] := aControl;
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
    (FControlList[i] as TSVGG2Control).ParamEvent( g2pChange, 0);
  end;
end;

//==============================================================================
//
//                                TSVGG2Graphic
//
//==============================================================================

constructor TSVGG2Graphic.Create( AOwner: TComponent; aId : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix);
begin
  inherited;
  FModulePanel := nil;
end;

destructor TSVGG2Graphic.Destroy;
begin
  inherited;
end;

function TSVGG2Graphic.FindOwnerModule: TSVGG2Module;
var CurSVGParent : TSVGGroup;
begin
  Result := nil;
  CurSVGParent := SVGParent;
  while (CurSVGParent <> nil) and not(CurSVGParent is TSVGG2Module) do
    CurSVGParent := CurSVGParent.SVGParent;

  Result := CurSVGParent as TSVGG2Module;
end;

function TSVGG2Graphic.CreateCopy(AOwner: TComponent; aSVGParent : TSVGGroup): TSVGObject;
begin
  Result := TSVGG2Graphic.Create(AOwner, ID, aSVGParent, CTM, UserMatrix);
  Result.Assign(self);
end;

procedure TSVGG2Graphic.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TSVGG2Graphic then begin
    CtrlType := (Source as TSVGG2Graphic).CtrlType;
    CtrlStyle := (Source as TSVGG2Graphic).CtrlStyle;
    FModulePanel := FindOwnerModule;
  end;
end;

procedure TSVGG2Graphic.Redraw;
var SaveMatrix : TMatrix;
begin
  if assigned(FModulePanel) then begin
    FModulePanel.FBitmap.Canvas.BeginScene;
    try
      SaveMatrix := FModulePanel.FBitmap.Canvas.Matrix;
      FModulePanel.FBitmap.Canvas.SetMatrix( MatrixMultiply(SVGParent.CTM, CreateScaleMatrix(FModulePanel.Zoom, FModulePanel.Zoom)));
      SVGPaint( FModulePanel.FBitmap.Canvas);
    finally
      FModulePanel.FBitmap.Canvas.SetMatrix(SaveMatrix);
      FModulePanel.FBitmap.Canvas.EndScene;
    end;
  end;
end;


//==============================================================================
//
//                               TSVGG2ConnLink
//
//==============================================================================

{constructor TSVGG2ConnLink.Create(AOwner: TComponent);
begin
  inherited;
  FCodeRef := -1;
end;}

constructor TSVGG2ConnLink.Create( AOwner: TComponent;  aId : string; aSVGParent: TSVGGroup; aCTM, aUserMatrix : TMatrix);
begin
  inherited;
  FCodeRef := -1;
end;

function TSVGG2ConnLink.CreateCopy(AOwner: TComponent; aSVGParent : TSVGGroup): TSVGObject;
begin
  Result := TSVGG2ConnLink.Create(AOwner, ID, aSVGParent, CTM, UserMatrix);
  //Result.SVGParent := aSVGParent;
  Result.Assign(self);
end;

procedure TSVGG2ConnLink.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TSVGG2ConnLink then begin
    FCodeRef := (Source as TSVGG2ConnLink).FCodeRef;
  end;
end;

function TSVGG2ConnLink.CreateGroup( AOwner : TComponent; aNode: TSVGNode; aId : string; aSVGParent: TSVGGroup;
  aCTM, aUserMatrix : TMatrix; var aSubTreeOwner: TSVGGroup): TSVGGroup;
var Connector : TSVGG2Connector;
begin
  if (aId = 'conn_in_blue') or (aId = 'conn_in_red') or (aId = 'conn_in_yellow') or (aId = 'conn_in_orange') then begin

    Connector := TSVGG2Connector.Create(self, aId, aSVGParent, aCTM, aUserMatrix);
    Connector.FKind := ckInput;
    Connector.FCodeRef := FCodeRef;
    {if assigned(FModuleData) then
      Connector.Data := FModuleData.InConnector[ FCodeRef];}
    aSubTreeOwner := Connector;
    Result := Connector;
  end else

  if (aId = 'conn_out_blue') or (aId = 'conn_out_red') or (aId = 'conn_out_yellow') or (aId = 'conn_out_orange') then begin
    Connector := TSVGG2Connector.Create(self, aId, aSVGParent, aCTM, aUserMatrix);
    Connector.FKind := ckOutput;
    Connector.FCodeRef := FCodeRef;
    {if assigned(FModuleData) then
      Connector.Data := FModuleData.OutConnector[ FCodeRef];}
    aSubTreeOwner := Connector;
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
//                             TSVGG2Connector
//
//==============================================================================

{constructor TSVGG2Connector.Create(AOwner: TComponent);
begin
  inherited;
end;}

constructor TSVGG2Connector.Create( AOwner: TComponent; aId : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix);
begin
  inherited;
end;

destructor TSVGG2Connector.Destroy;
begin
  inherited;
end;

function TSVGG2Connector.CreateCopy(AOwner: TComponent; aSVGParent : TSVGGroup): TSVGObject;
begin
  Result := TSVGG2Connector.Create(AOwner, ID, aSVGParent, CTM, UserMatrix);
  //Result.SVGParent := aSVGParent;
  Result.Assign(self);
end;

procedure TSVGG2Connector.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TSVGG2Connector then begin
    FKind := (Source as TSVGG2Connector).FKind;
    FCodeRef := (Source as TSVGG2Connector).FCodeRef;
  end;
end;

function TSVGG2Connector.CreatePath( AOwner : TComponent; aNode: TSVGNode; aId : string;
  aSVGParent: TSVGGroup; aCTM, aUserMatrix : TMatrix): TSVGPath;
begin
  if pos('_border', aId)>0 then begin
    FBorder := TSVGHitPath.Create(self, aId, aSVGParent, aCTM, aUserMatrix);
    FBorder.Control := self;
    Result := FBorder;
  end else begin
    Result := inherited;
  end;
end;

procedure TSVGG2Connector.SetModuleData(const aValue: TG2GraphModuleFMX);

  procedure SetChildModuleData( aSVGGroup : TSVGGroup);
  var i : integer;
  begin
    for i := 0 to aSVGGroup.Count - 1 do begin

      if aSVGGroup.Item[i] is TSVGHitPath then begin
        FBorder := (aSVGGroup.Item[i] as TSVGHitPath);
        FBorder.Control := self;
      end;

      if aSVGGroup.Item[i] is TSVGGroup then
        SetChildModuleData( aSVGGroup.Item[i] as TSVGGroup);
    end;
  end;

begin
  if FModuleData <> aValue then begin
    FModuleData := aValue;
    if assigned(FModuleData) then begin

      if SVGParent is TSVGG2ConnLink then begin
        if FKind = ckInput then
          Data := FModuleData.InConnector[ (SVGParent as TSVGG2ConnLink).FCodeRef]
        else
          if FKind = ckOutput then
            Data := FModuleData.OutConnector[ (SVGParent as TSVGG2ConnLink).FCodeRef];
      end;
    end;
    SetChildModuleData( self);
  end;
end;

procedure TSVGG2Connector.SetData( aValue : TG2FileConnector);
begin
  FData := aValue;
  FData.GraphControl := self;
end;

//==============================================================================
//
//                               TSVGG2ParamLink
//
//==============================================================================

constructor TSVGG2ParamLink.Create( AOwner: TComponent; aId : string; aSVGParent: TSVGGroup; aCTM, aUserMatrix : TMatrix);
begin
  inherited;
  FCodeRef := -1;
  FMasterRef := -1;
  FInfoFunc := -1;
  FTextFunc := -1;
  FDependencies := '';
  FCtrlType := '';
  FCtrlStyle := '';
end;

destructor TSVGG2ParamLink.Destroy;
begin
  inherited;
end;

function TSVGG2ParamLink.CreateCopy( AOwner : TComponent; aSVGParent : TSVGGroup): TSVGObject;
begin
  Result := TSVGG2ParamLink.Create(AOwner, ID, aSVGParent, CTM, UserMatrix);
  //Result.SVGParent := aSVGParent;
  Result.Assign(self);
end;

procedure TSVGG2ParamLink.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TSVGG2ParamLink then begin
    FCodeRef := (Source as TSVGG2ParamLink).FCodeRef;
    FMasterRef := (Source as TSVGG2ParamLink).FMasterRef;
    FInfoFunc := (Source as TSVGG2ParamLink).FInfoFunc;
    FTextFunc := (Source as TSVGG2ParamLink).FTextFunc;
    FDependencies := (Source as TSVGG2ParamLink).FDependencies;
    FCtrlType := (Source as TSVGG2ParamLink).FCtrlType;
    FCtrlStyle := (Source as TSVGG2ParamLink).FCtrlStyle;
  end;
end;

function TSVGG2ParamLink.CreateGroup( AOwner : TComponent; aNode: TSVGNode; aId : string; aSVGParent: TSVGGroup;
  aCTM, aUserMatrix : TMatrix; var aSubTreeOwner: TSVGGroup): TSVGGroup;
var TextField : TSVGTextField;
    BtnText : TSVGBtn;
    BtnFlat : TSVGBtnFlat;
    BtnRadio : TSVGBtnRadio;
    BtnIncDec : TSVGBtnIncDec;
    Slider : TSVGSlider;
    Knob : TSVGKnob;
    //BtnCount : integer;
    //AttributeValue : string;
begin
  if FCtrlType = 'textfield' then begin

    TextField := TSVGTextField.Create(self, aId, aSVGParent, aCTM, aUserMatrix);
    TextField.ModulePanel := ModulePanel;
    //ParseDependencies( TextField, FMasterRef, FDependencies, FTextFunc);
    Result := TextField;
    aSubTreeOwner := Result;

  end else

  if FCtrlType = 'btntext' then begin

    BtnText := TSVGBtn.Create(self, aId, aSVGParent, aCTM, aUserMatrix);
    BtnText.ModulePanel := ModulePanel;
    BtnText.FBtnIndex := -1;
    {if assigned(ModuleData) then begin
      BtnText.Parameter := (ModuleData.Parameter[ FCodeRef] as TG2GraphParameterFMX);
      BtnText.Parameter.InfoFunctionIndex := FInfoFunc;
    end;}
    Result := BtnText;
    aSubTreeOwner := Result;

  end else

  if FCtrlType = 'btnflat' then begin

    BtnFlat := TSVGBtnFlat.Create(self, aId, aSVGParent, aCTM, aUserMatrix);
    BtnFlat.ModulePanel := ModulePanel;
    {if assigned(ModuleData) then begin
      BtnFlat.Parameter := (ModuleData.Parameter[ FCodeRef] as TG2GraphParameterFMX);
      BtnFlat.Parameter.InfoFunctionIndex := FInfoFunc;
    end;}
    Result := BtnFlat;
    aSubTreeOwner := Result;

  end else

  if FCtrlType = 'btnradio' then begin

    {BtnCount := 0;
    if aNode.GetAttribute( 'nmg2.ButtonCount', AttributeValue) then
       BtnCount := StrToInt(AttributeValue);}

    BtnRadio := TSVGBtnRadio.Create(self, aId, aSVGParent, aCTM, aUserMatrix);
    BtnRadio.ModulePanel := ModulePanel;
    {if assigned(ModuleData) then begin
      BtnRadio.Parameter := (ModuleData.Parameter[ FCodeRef] as TG2GraphParameterFMX);
      BtnRadio.Parameter.InfoFunctionIndex := FInfoFunc;
    end;}

    Result := BtnRadio;
    aSubTreeOwner := Result;

  end else

  if FCtrlType = 'btnradioedit' then begin

    {BtnCount := 0;
    if aNode.GetAttribute( 'nmg2.ButtonCount', AttributeValue) then
       BtnCount := StrToInt(AttributeValue);}

    BtnRadio := TSVGBtnRadio.Create(self, aId, aSVGParent, aCTM, aUserMatrix);
    BtnRadio.ModulePanel := ModulePanel;
    {if assigned(ModuleData) then begin
      BtnRadio.Parameter := (ModuleData.Parameter[ FCodeRef] as TG2GraphParameterFMX);
      BtnRadio.Parameter.InfoFunctionIndex := FInfoFunc;
    end;}

    Result := BtnRadio;
    aSubTreeOwner := Result;

  end else

  if FCtrlType = 'btnincdec' then begin

    BtnIncDec := TSVGBtnIncDec.Create(self, aId, aSVGParent, aCTM, aUserMatrix);
    BtnIncDec.ModulePanel := ModulePanel;
    {if assigned(ModuleData) then begin
      BtnIncDec.Parameter := (ModuleData.Parameter[ FCodeRef] as TG2GraphParameterFMX);
      BtnIncDec.Parameter.InfoFunctionIndex := FInfoFunc;
    end;}

    Result := BtnIncDec;
    aSubTreeOwner := Result;

  end else

  if FCtrlType = 'knob' then begin

    if FCtrlStyle = 'slider' then begin

      Slider := TSVGSlider.Create(self, aId, aSVGParent, aCTM, aUserMatrix);
      Slider.ModulePanel := ModulePanel;
      {if assigned(ModuleData) then begin
        Slider.Parameter := (ModuleData.Parameter[ FCodeRef] as TG2GraphParameterFMX);
        Slider.Parameter.InfoFunctionIndex := FInfoFunc;
      end;}

      Result := Slider;
      aSubTreeOwner := Result;

    end else
      if (FCtrlStyle = 'big') or
         (FCtrlStyle = 'medium') or
         (FCtrlStyle = 'resetmedium') or
         (FCtrlStyle = 'reset') or
         (FCtrlStyle = 'small') then begin

        Knob := TSVGKnob.Create(self, aId, aSVGParent, aCTM, aUserMatrix);
        Knob.ModulePanel := ModulePanel;
        {if assigned(ModuleData) then begin
          Knob.Parameter := (ModuleData.Parameter[ FCodeRef] as TG2GraphParameterFMX);
          Knob.Parameter.InfoFunctionIndex := FInfoFunc;
        end;}

        Result := Knob;
        aSubTreeOwner := Result;
      end;
  end else
    Result := inherited;
end;

//==============================================================================
//
//                               TSVGG2Control
//
//==============================================================================

constructor TSVGG2Control.Create( AOwner: TComponent; aId : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix);
begin
  inherited;
  FParameter := nil;
end;

destructor TSVGG2Control.Destroy;
begin
  inherited;
end;

function TSVGG2Control.CreateCopy(AOwner: TComponent; aSVGParent : TSVGGroup): TSVGObject;
begin
  Result := TSVGG2Control.Create(AOwner, ID, aSVGParent, CTM, UserMatrix);
  Result.SVGParent := aSVGParent;
  Result.Assign(self);
end;

procedure TSVGG2Control.Assign(Source: TPersistent);
begin
  inherited;
end;

function TSVGG2Control.GetValue: single;
begin
  if (FParameter.HighValue - FParameter.LowValue + 1) <> 0 then
    Result := (FParameter.GetParameterValue - FParameter.LowValue) / (FParameter.HighValue - FParameter.LowValue + 1)
  else
    Result := 0;
end;

procedure TSVGG2Control.MouseDown(P: TPointF; aHitPath : TSVGHitPath);
begin
  Redraw;
  if assigned(FModulePanel) then
    FModulePanel.Repaint;
end;

procedure TSVGG2Control.MouseLeave;
begin
  Redraw;
  if assigned(FModulePanel) then
    //FModule.Repaint;
    FModulePanel.Redraw;
end;

procedure TSVGG2Control.MouseMove(P: TPointF; aHitPath : TSVGHitPath; dx, dy: single);
begin
  Redraw;
  if assigned(FModulePanel) then
    FModulePanel.Repaint;
end;

procedure TSVGG2Control.MouseUp(P: TPointF; aHitPath : TSVGHitPath);
begin
  Redraw;
  if assigned(FModulePanel) then
    FModulePanel.Repaint;
end;

procedure TSVGG2Control.SetParameter(const aValue: TG2GraphParameterFMX);
begin
  if FParameter <> aValue then begin
    FParameter := aValue;
    FParameter.AssignControl(self);
    ParamEvent( g2pSet, 0);
  end;
end;

procedure TSVGG2Control.SetModuleData(const aValue : TG2GraphModuleFMX);
begin
  if FModuleData <> aValue then begin
    FModuleData := aValue;
    if assigned(FModuleData) then begin
      if SVGParent is TSVGG2ParamLink then begin
        Parameter := FModuleData.Parameter[ (SVGParent as TSVGG2ParamLink).FCodeRef] as TG2GraphParameterFMX;
        if assigned(Parameter) then begin
          Parameter.InfoFunctionIndex := (SVGParent as TSVGG2ParamLink).FInfoFunc;
        end;
      end;
    end;
  end;
end;

procedure TSVGG2Control.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
 if (Operation = opRemove) and (AComponent = Parameter) then
    FParameter := nil;
  inherited;
end;

procedure TSVGG2Control.ParamEvent(Event: TG2ParamEvent; Info: NativeInt);
begin
//
end;

//==============================================================================
//
//                                TSVGHitPath
//
//==============================================================================

constructor TSVGHitPath.Create( AOwner: TComponent);
begin
  inherited;
  HitTest := True;
  FControl := nil;
  FBtnIndex := -1;
end;

constructor TSVGHitPath.Create( AOwner: TComponent; aId : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix);
begin
  inherited;
  HitTest := True;
  FControl := nil;
  FBtnIndex := -1;
end;

destructor TSVGHitPath.Destroy;
begin
  inherited;
end;

function TSVGHitPath.CreateCopy( AOwner : TComponent; aSVGParent : TSVGGroup): TSVGObject;
begin
  Result := TSVGHitPath.Create(AOwner);
  Result.SVGParent := aSVGParent;
  Result.Assign(self);
end;

procedure TSVGHitPath.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TSVGHitPath then begin
    HitTest := (Source as TSVGHitPath).HitTest;
    FBtnIndex := (Source as TSVGHitPath).FBtnIndex;
  end;
end;

procedure TSVGHitPath.MouseDown(P: TPointF);
begin
  if assigned(FControl) then
    FControl.MouseDown( P, self);
end;

procedure TSVGHitPath.MouseLeave;
begin
  if assigned(FControl) then
    FControl.MouseLeave;
end;

procedure TSVGHitPath.MouseMove(P: TPointF; dx, dy: single);
begin
  if assigned(FControl) then
    FControl.MouseMove( P, self, dx, dy);
end;

procedure TSVGHitPath.MouseUp(P: TPointF);
begin
  if assigned(FControl) then
    FControl.MouseUp( P, self);
end;

//==============================================================================
//
//                                TSVGBtnState
//
//==============================================================================

{constructor TSVGBtnState.Create( AOwner : TComponent);
begin
  inherited;
  FControl := nil;
  FBtnIndex := 0;
  FBtnBackground := nil;
  FLabel := nil;
end;}

//constructor TSVGBtnState.Create( aControl: TSVGG2Control; aBtnIndex : integer; aId : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix);
constructor TSVGBtnState.Create( AOwner: TComponent; aId : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix);
begin
  inherited Create( AOwner, aId, aSVGParent, aCTM, aUserMatrix);
  FControl := nil;
  FBtnIndex := 0;
  FBtnBackground := nil;
  FLabel := nil;
end;

destructor TSVGBtnState.Destroy;
begin
  inherited;
end;

function TSVGBtnState.CreateCopy( AOwner : TComponent; aSVGParent : TSVGGroup): TSVGObject;
begin
  Result := TSVGBtnState.Create(AOwner, ID, aSVGParent, CTM, UserMatrix);
  //Result.SVGParent := aSVGParent;
  Result.Assign(self);
end;

procedure TSVGBtnState.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TSVGBtnState then begin
     FBtnIndex := (Source as TSVGBtnState).FBtnIndex;
  end;
end;

function TSVGBtnState.CreatePath( AOwner : TComponent; aNode: TSVGNode; aId : string; aSVGParent: TSVGGroup;
  aCTM, aUserMatrix : TMatrix): TSVGPath;
var id : string;
begin
  if pos('_bg', aId)>0  then begin
    FBtnBackground := TSVGHitPath.Create(self, aId, aSVGParent, aCTM, aUserMatrix);
    FBtnBackground.Control := FControl;
    FBtnBackground.BtnIndex := FBtnIndex;
    Result := FBtnBackground;
  end else
    result := inherited;
end;

function TSVGBtnState.CreateSpan( AOwner : TComponent; aNode: TSVGNode; aID: string;
  aSVGParent: TSVGGroup; aCTM, aUserMatrix: TMatrix): TSVGSpan;
begin
  FLabel := TSVGLabel.Create(self, aId, aSVGParent, aCTM, aUserMatrix);
  FLabel.BoundsRect := GetBounds;
  FLabel.TextAlign := TTextAlign.taCenter;
  Result := FLabel;
end;

//==============================================================================
//
//                                TSVGSimpleText
//
//==============================================================================


constructor TSVGLabel.Create( AOwner: TComponent; aId : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix);
begin
  inherited;
  FTextAlign := TTextAlign.taLeading;
  FValue := '';
end;

destructor TSVGLabel.Destroy;
begin
  inherited;
end;

function TSVGLabel.CreateCopy(AOwner: TComponent; aSVGParent: TSVGGroup): TSVGObject;
begin
  Result := TSVGLabel.Create( AOwner, ID, aSVGParent, CTM, UserMatrix);
  Result.Assign( self);
end;

procedure TSVGLabel.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TSVGLabel then begin
    FTextAlign := (Source as TSVGLabel).TextAlign;
    FValue := (Source as TSVGLabel).Value;
    BoundsRect := (Source as TSVGLabel).BoundsRect;
  end;
end;

procedure TSVGLabel.SVGPaint( aCanvas : TCanvas);
var CanvasState : TCanvasSaveState;
    i : integer;
begin
  CanvasState := aCanvas.SaveState;
  try
    aCanvas.Font.Assign(Font);
    aCanvas.FillText(BoundsRect, Value, False, 1.0, [], FTextAlign);

    for i := 0 to Count - 1 do begin
      if Item[i] is TSVGLabel then begin
        Item[i].SVGPaint( aCanvas);
      end;
    end;

  finally
    aCanvas.RestoreState(CanvasState);
  end;
end;

procedure TSVGLabel.SetSpanText(const aValue: string);
var Bitmap : TBitmap;
begin
  if aValue <> Value then begin
    Value := aValue;

    {Bitmap := TBitmap.Create(100,100);
    try
      Bitmap.Canvas.Font.Assign(Font);
      BoundsRect.Width := Bitmap.Canvas.TextWidth(Value);
      BoundsRect.Height := Bitmap.Canvas.TextHeight(Value);
    finally
      Bitmap.Free;
    end;}

  end;
end;

procedure TSVGLabel.SetTextAlign(const aValue: TTextAlign);
begin
  if FTextAlign <> aValue then begin
    FTextAlign := aValue;
    Repaint;
  end;
end;

procedure TSVGLabel.SetValue(const aValue: string);
begin
  if aValue <> FValue then begin
    FValue := aValue;
    Repaint;
  end;
end;

//==============================================================================
//
//                                TSVGTextField
//
//==============================================================================

constructor TSVGTextField.Create(AOwner: TComponent; aId: string;
  aSVGParent: TSVGGroup; aCTM, aUserMatrix: TMatrix);
begin
  inherited;
  FValue := '';
end;

destructor TSVGTextField.Destroy;
begin
  inherited;
end;

function TSVGTextField.CreateCopy(AOwner: TComponent; aSVGParent: TSVGGroup): TSVGObject;
begin
  Result := TSVGTextField.Create( AOwner, ID, aSVGParent, CTM, UserMatrix);
  Result.Assign(self);
end;

procedure TSVGTextField.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TSVGTextField then begin
    Value := (Source as TSVGTextField).Value;
  end;
end;

procedure TSVGTextField.ParamEvent(Event: TG2ParamEvent; Info: NativeInt);
begin
  inherited;
  Value := FParameter.TextFunction;
end;

procedure TSVGTextField.SetModuleData(const aValue: TG2GraphModuleFMX);

  procedure SetChildModuleData( aSVGGroup : TSVGGroup);
  var i : integer;
  begin
    for i := 0 to aSVGGroup.Count - 1 do begin

      if aSVGGroup.Item[i] is TSVGLabel then begin
        FSimpleText := (aSVGGroup.Item[i] as TSVGLabel);
      end;

      if aSVGGroup.Item[i] is TSVGHitPath then begin
        (aSVGGroup.Item[i] as TSVGHitPath).FControl := self;
      end;

      if aSVGGroup.Item[i] is TSVGGroup then
        SetChildModuleData( aSVGGroup.Item[i] as TSVGGroup);
    end;
  end;

begin
  if FModuleData <> aValue then begin
    inherited;
    SetChildModuleData(self);
  end;
end;

procedure TSVGTextField.SetValue(aValue: string);
begin
  if aValue <> FValue then begin
    FValue := aValue;
    if assigned(FSimpleText) then
      FSimpleText.Value := aValue;
    Redraw;
  end;
end;

function TSVGTextField.CreateSpan(  AOwner : TComponent; aNode : TSVGNode; aID : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix) : TSVGSpan;
begin
  if pos('_text', aId)>0 then begin
    FSimpleText := TSVGLabel.Create(self, aId, aSVGParent, aCTM, aUserMatrix);
    FSimpleText.BoundsRect := GetBounds;
    FSimpleText.TextAlign := TTextAlign.taCenter;
    Result := FSimpleText;
  end else begin
    Result := inherited;
  end;
end;

function TSVGTextField.CreatePath( AOwner : TComponent; aNode : TSVGNode; aId : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix) : TSVGPath;
begin
  Result := inherited;

  if pos('_window', aID)>0 then
    FWindow := Result;
end;

//==============================================================================
//
//                                TSVGBtnText
//
//==============================================================================

{constructor TSVGBtn.Create( AOwner: TComponent);
begin
  inherited;
  FBtnType := btToggle;
  FBtnUp := nil;
  FBtnDown := nil;
  FBtnIndex := 0;
  Value := 0.0;
end;}

constructor TSVGBtn.Create( AOwner: TComponent; aId : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix);
begin
  inherited;
  FBtnType := btToggle;
  FBtnUp := nil;
  FBtnDown := nil;
  FBtnIndex := 0;
  Value := 0.0;
end;

destructor TSVGBtn.Destroy;
begin
  inherited;
end;

function TSVGBtn.CreateCopy( AOwner : TComponent; aSVGParent : TSVGGroup): TSVGObject;
begin
  Result := TSVGBtn.Create(AOwner, ID, aSVGParent, CTM, UserMatrix);
  //Result.SVGParent := aSVGParent;
  Result.Assign(self);
end;

procedure TSVGBtn.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TSVGBtn then begin
    FBtnType := (Source as TSVGBtn).FBtnType;
    FBtnIndex := (Source as TSVGBtn).FBtnIndex;
    Value := (Source as TSVGBtn).Value;
  end;
end;

procedure TSVGBtn.MouseDown(P: TPointF; aHitPath : TSVGHitPath);
begin
  case FBtnType of
  btToggle :
    begin
      Value := 1.0 - Value;

      if assigned(Parameter) then
        Parameter.SetParameterValue( trunc(Value));

      if assigned(FOnChange) then
        FOnChange(self);
    end;
  btMomentary :
    begin
      Value := 1.0;

      if assigned(Parameter) then
        Parameter.SetParameterValue( trunc(Value));

      if assigned(FOnChange) then
        FOnChange(self);
    end;
  end;
  inherited;
end;

procedure TSVGBtn.MouseUp( P : TPointF; aHitPath : TSVGHitPath);
begin
  case FBtnType of
  btToggle :
    begin
    end;
  btMomentary :
    begin
      Value := 0.0;
      if assigned(Parameter) then
        Parameter.SetParameterValue( trunc(Value));

      if assigned(FOnChange) then
        FOnChange(self);
    end;
  end;
  inherited;
end;

procedure TSVGBtn.ParamEvent(Event: TG2ParamEvent; Info: NativeInt);
begin
  inherited;
  Value := FParameter.GetParameterValue;
end;

procedure TSVGBtn.Redraw;
begin
  if assigned(FBtnUp) and assigned(FBtnDown) then begin
    FBtnUp.Redraw;
    FBtnDown.Redraw;
  end;
  inherited;
end;

procedure TSVGBtn.SetValue(aValue: single);
begin
  if aValue <> FValue then begin
    FValue := aValue;
    if FValue = 1.0  then begin
      FBtnUp.Visible := False;
      FBtnDown.Visible := True;
    end else begin
      FBtnUp.Visible := True;
      FBtnDown.Visible := False;
    end;
    Redraw;
  end;
end;

procedure TSVGBtn.SetModuleData(const aValue : TG2GraphModuleFMX);

  procedure SetChildModuleData( aSVGGroup : TSVGGroup);
  var i : integer;
  begin
    for i := 0 to aSVGGroup.Count - 1 do begin

      if aSVGGroup.Item[i] is TSVGBtnState then begin
        if (aSVGGroup.Item[i] as TSVGBtnState).FBtnIndex = 0 then begin
          FBtnUp := (aSVGGroup.Item[i] as TSVGBtnState);
          FBtnUp.ModulePanel := ModuleData.FSVGControl;
          FBtnUp.FControl := self;
          FBtnUp.Visible := Value = 0.0;
        end else begin
          FBtnDown := (aSVGGroup.Item[i] as TSVGBtnState);
          FBtnDown.ModulePanel := ModuleData.FSVGControl;
          FBtnDown.FControl := self;
          FBtnDown.Visible := Value = 1.0;
        end;
      end;

      if aSVGGroup.Item[i] is TSVGHitPath then begin
        (aSVGGroup.Item[i] as TSVGHitPath).FControl := self;
      end;

      if aSVGGroup.Item[i] is TSVGGroup then
        SetChildModuleData( aSVGGroup.Item[i] as TSVGGroup);
    end;
  end;

begin
  if FModuleData <> aValue then begin
    inherited;
    SetChildModuleData(self);
  end;
end;

function TSVGBtn.CreateGroup( AOwner : TComponent; aNode: TSVGNode; aId: string;
  aSVGParent: TSVGGroup; aCTM, aUserMatrix: TMatrix;
  var aSubTreeOwner: TSVGGroup): TSVGGroup;
var GraphName, Size, State : string;
    ctrl_type : string;
begin
  if aNode.GetAttribute('nmg2.CtrlType', ctrl_type) then begin
    if ctrl_type = 'btnstate_off' then begin
      FBtnUp := TSVGBtnState.Create(self, aId, aSVGParent, aCTM, aUserMatrix);
      Result := FBtnUp;
      FBtnUp.FControl := self;
      FBtnUp.FBtnIndex := 0;
      FBtnUp.Visible := Value = 0.0;
      FBtnUp.FModulePanel := FModulePanel;
      aSubTreeOwner := Result;
    end else
      if ctrl_type = 'btnstate_on' then begin
        FBtnDown := TSVGBtnState.Create(self, aId, aSVGParent, aCTM, aUserMatrix);
        Result := FBtnDown;
        FBtnDown.FControl := self;
        FBtnDown.FBtnIndex := 1;
        FBtnDown.Visible := Value = 1.0;
        FBtnDown.FModulePanel := FModulePanel;
        aSubTreeOwner := Result;
      end;
  end else
    Result := inherited;
end;

function TSVGBtn.CreateSpan( AOwner : TComponent; aNode: TSVGNode; aID: string;
  aSVGParent: TSVGGroup; aCTM, aUserMatrix: TMatrix): TSVGSpan;
begin
  FLabel := TSVGLabel.Create(self, aId, aSVGParent, aCTM, aUserMatrix);
  FLabel.BoundsRect := GetBounds;
  FLabel.TextAlign := TTextAlign.taCenter;
  Result := FLabel;
end;

//==============================================================================
//
//                               TSVGBtnIncDec
//
//==============================================================================

constructor TSVGBtnIncDec.Create( AOwner: TComponent; aId : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix);
begin
  inherited;
  Value := 0;
end;

destructor TSVGBtnIncDec.Destroy;
begin
  inherited;
end;

function TSVGBtnIncDec.CreateCopy(AOwner: TComponent; aSVGParent: TSVGGroup): TSVGObject;
begin
  Result := TSVGBtnIncDec.Create( AOwner, ID, aSVGParent, CTM, UserMatrix);
  Result.Assign(self);
end;

procedure TSVGBtnIncDec.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TSVGBtnIncDec then begin
    Value := (Source as TSVGBtnIncDec).Value;
  end;
end;

procedure TSVGBtnIncDec.SetParameter(const Value: TG2GraphParameterFMX);
begin
  inherited;
  {if assigned(FBtnDec) then
    FBtnDec.Parameter := Value;
  if assigned(FBtnInc) then
    FBtnInc.Parameter := Value;}
end;

procedure TSVGBtnIncDec.ParamEvent(Event: TG2ParamEvent; Info: NativeInt);
begin
  inherited;
  Value := FParameter.GetParameterValue;
end;

procedure TSVGBtnIncDec.IncBtnChange( Sender : TObject);
begin
  //
end;

procedure TSVGBtnIncDec.DecBtnChange( Sender : TObject);
begin
  //
end;

procedure TSVGBtnIncDec.SetValue( aValue : byte);
begin
  if aValue <> FValue then begin
    FValue := aValue;
  end;
end;

procedure TSVGBtnIncDec.Redraw;
begin
  FBtnDec.Redraw;
  FBtnInc.Redraw;
  inherited;
end;

function TSVGBtnIncDec.CreateUse( AOwner : TComponent; aUseNode, aRefNode : TSVGNode; aUseID : string;
    aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix; var aSubTreeOwner : TSVGGroup) : TSVGGroup;
var ctrl_type : string;
begin
  if aUseNode.GetAttribute('nmg2.CtrlType', ctrl_type) then begin
    if ctrl_type = 'btn_dec' then begin
      FBtnDec := TSVGBtn.Create(self, aUSeId, aSVGParent, aCTM, aUserMatrix);
      FBtnDec.BtnType := btMomentary;
      FBtnDec.FBtnIndex := 0;
      FBtnDec.OnChange := DecBtnChange;
      Result := FBtnDec;
      FBtnDec.FModulePanel := FModulePanel;
      aSubTreeOwner := Result;
    end else
      if ctrl_type = 'btn_inc' then begin
        FBtnInc := TSVGBtn.Create(self, aUseId, aSVGParent, aCTM, aUserMatrix);
        FBtnInc.BtnType := btMomentary;
        FBtnInc.FBtnIndex := 1;
        FBtnInc.OnChange := IncBtnChange;
        Result := FBtnInc;
        FBtnInc.FModulePanel := FModulePanel;
        aSubTreeOwner := Result;
      end;
  end else
    Result := inherited;
end;

procedure TSVGBtnIncDec.SetModuleData(const aValue : TG2GraphModuleFMX);

  procedure SetChildModuleData( aSVGGroup : TSVGGroup);
  var i : integer;
  begin
    for i := 0 to aSVGGroup.Count - 1 do begin

      if aSVGGroup.Item[i] is TSVGBtn then begin

        if (aSVGGroup.Item[i] as TSVGBtn).FBtnIndex = 0 then begin
          FBtnDec := (aSVGGroup.Item[i] as TSVGBtn);
          FBtnDec.OnChange := DecBtnChange;
          FBtnDec.ModulePanel := FModulePanel;
        end else begin
          FBtnInc := (aSVGGroup.Item[i] as TSVGBtn);
          FBtnInc.OnChange := IncBtnChange;
          FBtnInc.ModulePanel := FModulePanel;
        end;
      end;

      if aSVGGroup.Item[i] is TSVGGroup then
        SetChildModuleData( aSVGGroup.Item[i] as TSVGGroup);
    end;
  end;

begin
  if FModuleData <> aValue then begin
    inherited;
    SetChildModuleData(self);
  end;
end;

//==============================================================================
//
//                               TSVGBtnFlat
//
//==============================================================================


constructor TSVGBtnFlat.Create(AOwner: TComponent; aId: string;
  aSVGParent: TSVGGroup; aCTM, aUserMatrix: TMatrix);
begin
  inherited;
  FButtons := TObjectList.Create(False);
  FValue := 0;
end;

destructor TSVGBtnFlat.Destroy;
begin
  FButtons.Free;
  inherited;
end;

function TSVGBtnFlat.CreateCopy(AOwner: TComponent; aSVGParent: TSVGGroup): TSVGObject;
begin
  Result := TSVGBtnFlat.Create( AOwner, ID, aSVGParent, CTM, UserMatrix);
  Result.Assign(self);
end;

procedure TSVGBtnFlat.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TSVGBtnFlat then begin
    Value := (Source as TSVGBtnFlat).Value;
  end;
end;

procedure TSVGBtnFlat.MouseDown(P: TPointF; aHitPath: TSVGHitPath);
var NewValue : byte;
begin
  inherited;

  if Value = FButtons.Count - 1 then
    Value := 0
  else
    Value := Value + 1;

  if assigned(FParameter) then begin
    NewValue := Value;
    if NewValue <> FParameter.GetParameterValue then begin
      FParameter.SetParameterValue(NewValue);
    end;
  end;

  Redraw;
end;

procedure TSVGBtnFlat.ParamEvent(Event: TG2ParamEvent; Info: NativeInt);
begin
  inherited;

  Value := FParameter.GetParameterValue;
end;

procedure TSVGBtnFlat.UpdateParts;
var i : integer;
begin
  if assigned(FParameter) then begin
    {if(FParameter.GetMorphValue(0,0) <> 0) then
      FKnobMorph.Visible := True
    else
      FKnobMorph.Visible := False;}
  end;
  for i := 0 to FButtons.Count - 1 do begin
    (FButtons[i] as TSVGBtnState).Visible := (i = Value);
  end;
end;

procedure TSVGBtnFlat.Redraw;
begin
  inherited;
end;

procedure TSVGBtnFlat.SetModuleData(const aValue: TG2GraphModuleFMX);

  procedure SetChildModuleData( aSVGGroup : TSVGGroup);
  var i : integer;
  begin
    for i := 0 to aSVGGroup.Count - 1 do begin

      if aSVGGroup.Item[i] is TSVGBtnState then begin
        (aSVGGroup.Item[i] as TSVGBtnState).Control := self;
        FButtons.Insert( (aSVGGroup.Item[i] as TSVGBtnState).FBtnIndex, aSVGGroup.Item[i] as TSVGBtnState);
      end;

      if aSVGGroup.Item[i] is TSVGHitPath then begin
        (aSVGGroup.Item[i] as TSVGHitPath).FControl := self;
      end;

      if aSVGGroup.Item[i] is TSVGGroup then
        SetChildModuleData( aSVGGroup.Item[i] as TSVGGroup);
    end;
  end;

begin
  if FModuleData <> aValue then begin
    inherited;
    FButtons.Clear;
    SetChildModuleData(self);
    UpdateParts;
  end;
end;

procedure TSVGBtnFlat.SetValue(aValue: integer);
begin
  if aValue <> FValue then begin
    FValue := aValue;
    UpdateParts;
    Redraw;
  end;
end;

function TSVGBtnFlat.CreateUse( AOwner : TComponent; aUseNode, aRefNode: TSVGNode; aUseID: string;
  aSVGParent: TSVGGroup; aCTM, aUserMatrix: TMatrix;
  var aSubTreeOwner: TSVGGroup): TSVGGroup;
var ctrl_type : string;
    Btn : TSVGBtnState;
begin
  if aUseNode.GetAttribute('nmg2.CtrlType', ctrl_type) then begin
    if ctrl_type = 'option' then begin
      Btn := TSVGBtnState.Create(self, aUseId, aSVGParent, aCTM, aUserMatrix);
      Btn.Control := self;
      Btn.FBtnIndex := FButtons.Count;
      FButtons.Add(Btn);
      Result := Btn;
      aSubTreeOwner := Result;
    end else
      Result := inherited;
  end else
    Result := inherited;
end;

//==============================================================================
//
//                               TSVGBtnRadio
//
//==============================================================================

constructor TSVGBtnRadio.Create( AOwner: TComponent; aId : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix);
begin
  inherited;
  FValue := 0;
  FButtons := TObjectList.Create(False);
end;

destructor TSVGBtnRadio.Destroy;
begin
  FButtons.Free;

  inherited;
end;

function TSVGBtnRadio.CreateCopy(AOwner: TComponent; aSVGParent: TSVGGroup): TSVGObject;
begin
  Result := TSVGBtnRadio.Create( AOwner, ID, aSVGParent, CTM, UserMatrix);
  Result.Assign(self);
end;

procedure TSVGBtnRadio.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TSVGBtnRadio then begin
    Value := (Source as TSVGBtnRadio).Value;
  end;
end;

procedure TSVGBtnRadio.ParamEvent(Event: TG2ParamEvent; Info: NativeInt);
begin
  inherited;

  Value := FParameter.GetParameterValue;
end;


procedure TSVGBtnRadio.BtnChange(Sender: TObject);
var Btn : TSVGBtn;
begin
  if Sender is TSVGBtn then begin
    Btn := Sender as TSVGBtn;
    //Value := Btn.Tag;
    Value := Btn.FBtnIndex;

    if Value <> FParameter.GetParameterValue then begin
      FParameter.SetParameterValue(Value);
    end;
  end;
end;

procedure TSVGBtnRadio.SetValue( aValue : integer);
begin
  if FValue <> aValue then begin
    FValue := aValue;
    UpdateParts;
    Redraw;
  end;
end;

procedure TSVGBtnRadio.UpdateParts;
var i : integer;
begin
  for i := 0 to FButtons.Count-1 do begin
    if i = Value then
      (FButtons[i] as TSVGBtn).Value := 1
    else
      (FButtons[i] as TSVGBtn).Value := 0;
  end;
end;

procedure TSVGBtnRadio.SetModuleData(const aValue: TG2GraphModuleFMX);

  procedure SetChildModuleData( aSVGGroup : TSVGGroup);
  var i : integer;
      Btn : TSVGBtn;
  begin
    for i := 0 to aSVGGroup.Count - 1 do begin

      if aSVGGroup.Item[i] is TSVGBtn then begin
        Btn := (aSVGGroup.Item[i] as TSVGBtn);
        Btn.FBtnType := btToggle;
        Btn.OnChange := BtnChange;
        Btn.ModulePanel := FModulePanel;
        Btn.ModuleData := FModuleData;
        FButtons.Insert( Btn.FBtnIndex, Btn);
      end;

      if aSVGGroup.Item[i] is TSVGGroup then
        SetChildModuleData( aSVGGroup.Item[i] as TSVGGroup);
    end;
  end;

begin
  if FModuleData <> aValue then begin
    inherited;
    FButtons.Clear;
    SetChildModuleData(self);
    UpdateParts;
  end;
end;

function TSVGBtnRadio.CreateUse(  AOwner : TComponent; aUseNode, aRefNode : TSVGNode; aUseID : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix; var aSubTreeOwner : TSVGGroup) : TSVGGroup;
var ctrl_type : string;
    Btn : TSVGBtn;
begin
  if aUseNode.GetAttribute('nmg2.CtrlType', ctrl_type) then begin
    if ctrl_type = 'option' then begin
      Btn := TSVGBtn.Create(self, aUSeId, aSVGParent, aCTM, aUserMatrix);
      Btn.BtnType := btToggle;
      Btn.OnChange := BtnChange;
      Result := Btn;
      //Btn.Tag := FButtons.Count;
      Btn.FBtnIndex := FButtons.Count;
      FButtons.Add(Btn);
      Btn.ModulePanel := FModulePanel;
      aSubTreeOwner := Result;
    end else
      Result := inherited;
  end else
    Result := inherited;
end;


//==============================================================================
//
//                               TSVGKnob
//
//==============================================================================


constructor TSVGKnob.Create( AOwner: TComponent; aId : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix);
begin
  inherited;
  FValue := 0;
end;

destructor TSVGKnob.Destroy;
begin
  inherited;
end;

function TSVGKnob.CreateCopy(AOwner: TComponent; aSVGParent: TSVGGroup): TSVGObject;
begin
  Result := TSVGKnob.Create(AOwner, ID, aSVGParent, CTM, UserMatrix);
  Result.Assign( self);
end;

procedure TSVGKnob.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TSVGKnob then begin
    Value := (Source as TSVGKnob).Value;
  end;
end;

procedure TSVGKnob.InitSVGParentLinkage(aSVGChildObject: TSVGObject);
begin
 if aSVGChildObject is TSVGBtn then begin
    FBtnReset := (aSVGChildObject as TSVGBtn);
    FBtnReset.FBtnType := btToggle;
    FBtnReset.OnChange := ResetBtnChange;
  end;

  if aSVGChildObject is TSVGGroup then begin
    if pos('_needle', aSVGChildObject.ID)>0 then begin
      FKnobNeedle := aSVGChildObject as TSVGGroup;
    end;

    if pos('_btns', aSVGChildObject.ID)>0 then begin
      FKnobBtns := aSVGChildObject as TSVGGroup;
      FKnobBtns.Visible := False;
    end;
  end;

  if aSVGChildObject is TSVGPath then begin
    if pos('_morph', aSVGChildObject.ID)>0 then begin
      FKnobMorph := aSVGChildObject as TSVGPath;
    end;
  end;

  if aSVGChildObject is TSVGHitPath then begin
    (aSVGChildObject as TSVGHitPath).FControl := self;

    if pos('_face', aSVGChildObject.ID)>0 then begin
      FKnobFace := aSVGChildObject as TSVGHitPath;
    end;

    if pos('_sel', aSVGChildObject.ID)>0 then begin
      FKnobSel := aSVGChildObject as TSVGHitPath;
    end;
  end;
end;

procedure TSVGKnob.MouseDown( P : TPointF; aHitPath : TSVGHitPath);
begin
  if aHitPath = FKnobFace then begin
    FStartPoint := P;
    FStartValue := Value;
  end;
  inherited;
end;

procedure TSVGKnob.MouseMove( P: TPointF; aHitPath : TSVGHitPath; dx, dy: single);
var dValue : single;
    NewValue : byte;
begin
  if assigned(FKnobBtns) and (not FKnobBtns.Visible) then begin
    FKnobBtns.Visible := True;
  end;

  if aHitPath = FKnobFace then begin
    dValue := (P.X - FStartPoint.X) / 100;
    if dValue + FStartValue > 1.0 then
      Value := 1.0
    else
      if dValue + FStartValue < 0.0 then
        Value := 0.0
      else
        Value := FStartValue + dValue;

    if assigned(FParameter) then begin
      NewValue := trunc(Value * (FParameter.HighValue - FParameter.LowValue+1) + FParameter.LowValue);
      if NewValue <> FParameter.GetParameterValue then begin
        FParameter.SetParameterValue(NewValue);
      end;
    end;
  end;

  inherited;
end;

procedure TSVGKnob.MouseUp( P : TPointF; aHitPath : TSVGHitPath);
begin
  inherited;
end;

procedure TSVGKnob.MouseLeave;
begin
  FKnobBtns.Visible := False;

  inherited;
end;

procedure TSVGKnob.ResetBtnChange( Sender : TObject);
var  NewValue : byte;
begin
  FStartValue := 0;
  Value := 0.5;

  if assigned(FParameter) then begin
    NewValue := trunc(Value * (FParameter.HighValue - FParameter.LowValue+1) + FParameter.LowValue);
    if NewValue <> FParameter.GetParameterValue then begin
      FParameter.SetParameterValue(NewValue);
    end;
  end;

  Redraw;
end;

procedure TSVGKnob.ParamEvent(Event: TG2ParamEvent; Info: NativeInt);
begin
  inherited;

  if (FParameter.HighValue - FParameter.LowValue+1) <> 0 then
    Value := (FParameter.GetParameterValue - FParameter.LowValue) / (FParameter.HighValue - FParameter.LowValue+1);
end;

procedure TSVGKnob.SetLayoutAngle(aLayout: TSVGGroup; aAngle: single);
var MorphPath : string;
begin
  if assigned(aLayout) then
    aLayout.Transform(1,1,0,0, aAngle*PI/180, 0,0);

  if assigned(FKnobMorph) then begin
    FKnobMorph.Data.Clear;
    FKnobMorph.Data.MoveTo( PointF(0, 0));
    FKnobMorph.Data.LineTo( PointF(10*sin(aAngle*PI/180), -10*cos(aAngle*PI/180)));
    FKnobMorph.Data.AddArc( PointF(0,0), PointF(10,10), aAngle-90, 45);
    FKnobMorph.Data.ClosePath;
    FKnobMorph.Data.ApplyPathData;
  end;
end;

procedure TSVGKnob.UpdateParts;
begin
  SetLayoutAngle( FKnobNeedle, Value * 270 - 135);

  if assigned(FKnobMorph) then
    if assigned(FParameter) and (FParameter.GetMorphValue(0,0) <> 0) then
      FKnobMorph.Visible := True
    else
      FKnobMorph.Visible := False;

  if assigned(FBtnReset) then
    if Value = 0.5 then begin
      FBtnReset.Value := 1.0;
    end else begin
      FBtnReset.Value := 0.0;
    end;
end;

procedure TSVGKnob.SetValue(aValue: single);
begin
  if aValue <> FValue then begin
    FValue := aValue;
    UpdateParts;
    Redraw;
  end;
end;

procedure TSVGKnob.SetModuleData(const aValue: TG2GraphModuleFMX);
begin
  if FModuleData <> aValue then begin
    inherited;
    UpdateParts;
  end;
end;

function TSVGKnob.CreateGroup( AOwner : TComponent; aNode: TSVGNode; aId : string; aSVGParent : TSVGGroup;
  aCTM, aUserMatrix : TMatrix; var aSubTreeOwner: TSVGGroup): TSVGGroup;
var ctrl_type : string;
begin
  Result := inherited;

  if aNode.GetAttribute('nmg2.CtrlType', ctrl_type) then begin
    if ctrl_type = 'needle' then begin
      FKnobNeedle := Result;
      UpdateParts;
    end;
  end;
end;

function TSVGKnob.CreatePath( AOwner : TComponent; aNode: TSVGNode; aId : string; aSVGParent : TSVGGroup;
  aCTM, aUserMatrix : TMatrix): TSVGPath;
var ctrl_type : string;
begin
  if aNode.GetAttribute('nmg2.CtrlType', ctrl_type) then begin
    if ctrl_type = 'selection' then begin
      FKnobSel := TSVGHitPath.Create(self, aId, aSVGParent, aCTM, aUserMatrix);
      FKnobSel.Control := self;
      Result := FKnobSel;
    end else
      if ctrl_type = 'face' then begin
        FKnobFace := TSVGHitPath.Create(self, aId, aSVGParent, aCTM, aUserMatrix);
        FKnobFace.Control := self;
        Result := FKnobFace;
      end else begin
        Result := inherited;
        if ctrl_type = 'morph' then begin
          FKnobMorph := Result;
          UpdateParts;
        end;
      end;
  end else
    Result := inherited;
end;

function TSVGKnob.CreateUse(  AOwner : TComponent; aUseNode, aRefNode : TSVGNode; aUseID : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix; var aSubTreeOwner : TSVGGroup) : TSVGGroup;
var ctrl_type : string;
begin
  if aUseNode.GetAttribute('nmg2.CtrlType', ctrl_type) then begin
    if ctrl_type = 'reset' then begin
      FBtnReset := TSVGBtn.Create(self, aUSeId, aSVGParent, aCTM, aUserMatrix);
      Result := FBtnReset;
      FBtnReset.FModulePanel := FModulePanel;
      FBtnReset.OnChange := ResetBtnChange;
      aSubTreeOwner := Result;
    end else begin
      Result := inherited;
      if ctrl_type = 'buttons' then begin
        FKnobBtns := Result;
        FKnobBtns.Visible := False;
      end;
    end
  end else
    Result := inherited;
end;

//==============================================================================
//
//                               TSVGSlider
//
//==============================================================================

constructor TSVGSlider.Create( AOwner: TComponent; aId : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix);
begin
  inherited;
  FValue := 0;
end;

destructor TSVGSlider.Destroy;
begin
  inherited;
end;

procedure TSVGSlider.MouseDown( P : TPointF; aHitPath : TSVGHitPath);
begin
  if aHitPath = FSliderBtn.FBtnBackground then begin
    FStartPoint := P;
    FStartValue := Value;
  end;

  inherited;
end;

procedure TSVGSlider.MouseMove( P : TPointF; aHitPath : TSVGHitPath; dx, dy : single);
var dValue : single;
    NewValue : byte;
begin
  if aHitPath = FSliderBtn.FBtnBackground then begin

    dValue := (FStartPoint.Y - P.Y) / 100;
    if dValue + FStartValue > 1.0 then
      Value := 1.0
    else
      if dValue + FStartValue < 0.0 then
        Value := 0.0
      else
        Value := FStartValue + dValue;

    NewValue := trunc((Value) * (FParameter.HighValue - FParameter.LowValue+1) + FParameter.LowValue);
    if (NewValue>=FParameter.LowValue) and (NewValue<=FParameter.HighValue) then
      if NewValue <> FParameter.GetParameterValue then begin
        FParameter.SetParameterValue(NewValue);
      end;
  end;

  inherited;
end;

procedure TSVGSlider.MouseUp( P : TPointF; aHitPath : TSVGHitPath);
begin
  inherited;
end;

procedure TSVGSlider.ParamEvent(Event: TG2ParamEvent; Info: NativeInt);
begin
  inherited;

  if (FParameter.HighValue - FParameter.LowValue+1) <> 0 then
    Value := (FParameter.GetParameterValue - FParameter.LowValue) / (FParameter.HighValue - FParameter.LowValue+1);
end;

procedure TSVGSlider.MouseLeave;
begin
  inherited;
end;

procedure TSVGSlider.SetSliderPos( aValue : single);
begin
  if assigned(FSliderBtn) then
    FSliderBtn.Transform(1,1,0,aValue,0,0,0);

  if assigned(FSliderMorph) then begin
    FSliderMorph.Data.Clear;
    //FSliderMorph.Data.MoveTo( PointF(0, 0));
    //FSliderMorph.Data.ClosePath;
    //FSliderMorph.Data.ApplyPathData;
  end;
end;

procedure TSVGSlider.UpdateParts;
begin
  if assigned(FSliderFace) and assigned(FSliderBtn) then
    SetSliderPos( (FSliderFace.Height - FSliderBtn.Height) - (FSliderFace.Height - FSliderBtn.Height) * Value);

  if assigned(FSliderMorph) then
    if assigned(FParameter) and (FParameter.GetMorphValue(0,0) <> 0) then
      FSliderMorph.Visible := True
    else
      FSliderMorph.Visible := False;
end;

procedure TSVGSlider.SetValue( aValue : single);
begin
  if aValue <> FValue then begin
    FValue := aValue;
    UpdateParts;
    Redraw;
  end;
end;

function TSVGSlider.CreatePath( AOwner : TComponent; aNode : TSVGNode; aId : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix) : TSVGPath;
begin
  if (aId = 'slider_face') then begin
    FSliderFace := TSVGHitPath.Create(self, aId, aSVGParent, aCTM, aUserMatrix);
    FSliderFace.Control := self;
    Result := FSliderFace;
  end else begin
    Result := inherited;
    if (aId = 'slider_morph') then begin
      FSliderMorph := Result;
    end;
  end;
end;

function TSVGSlider.CreateUse( AOwner : TComponent; aUseNode, aRefNode : TSVGNode; aUseID : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix; var aSubTreeOwner : TSVGGroup) : TSVGGroup;
var ctrl_type : string;
begin
  if aUseNode.GetAttribute('nmg2.CtrlType', ctrl_type) then begin
    if ctrl_type = 'knob' then begin
      FSliderBtn := TSVGBtnState.Create(self, aUseId, aSVGParent, aCTM, aUserMatrix);
      FSliderBtn.FControl := self;
      FSliderBtn.FBtnIndex := 0;
      FSliderBtn.FModulePanel := FModulePanel;
      Result := FSliderBtn;
      aSubTreeOwner := Result;
    end else
      if ctrl_type = 'btns' then begin
         FBtnIncDec := TSVGBtnIncDec.Create(self, aUseId, aSVGParent, aCTM, aUserMatrix);
         Result := FBtnIncDec;
         FBtnIncDec.FModulePanel := FModulePanel;
         //FBtnIncDec.OnChange := BtnIncDecChange;
         aSubTreeOwner := Result;
      end else
        Result := inherited;
  end else
    Result := inherited;
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
begin
  if assigned(FCableList) then begin

    if FRedrawBuffer then begin
      Clear;
      for i := 0 to FCableList.Count - 1 do begin
        (FCableList[i] as TG2GraphCableFMX).FSVGControl.PaintBuffer(self);
      end;
      FRedrawBuffer := False;
    end;

    Canvas.BeginScene;
    try
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
      Canvas.EndScene;
    end;
  end;
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

  AddNodes(2);

  FTimer.OnTimer := OnTimer;
end;

destructor TSVGG2Cable.Destroy;
begin
  ClearNodes;
  Finalize(FNodes);

  FTimer.Enabled := False;
  FTimer.Free;

  inherited;
end;

procedure TSVGG2Cable.ClearNodes;
var i : integer;
begin
  for i := 0 to FNodeCount - 1 do
    FNodes[i].Free;
  SetLength(FNodes, 0);
end;

procedure TSVGG2Cable.AddNodes( aNodeCount : integer);
var i : integer;
begin
  ClearNodes;
  FNodeCount := aNodeCount;
  SetLength(FNodes, FNodeCount);
  for i := 0 to FNodeCount - 1 do
    FNodes[i] := TNode.Create;
end;

procedure TSVGG2Cable.InitCable;
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
  min_x := min(FP1.X, FP2.X);
  max_x := max(FP1.X, FP2.X);
  min_y := min(FP1.Y, FP2.Y);
  max_y := max(FP1.Y, FP2.Y);

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

procedure TSVGG2Cable.PaintBuffer(aBuffer: TCableBitmapBuffer);
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
    ElementBoundsRect.Left := min(min(min( p1.X, p2.X), p3.X), p4.X);
    ElementBoundsRect.Top := min(min(min( p1.Y, p2.Y), p3.Y), p4.Y);
    ElementBoundsRect.Right := max(max(max( p1.X, p2.X), p3.X), p4.X);
    ElementBoundsRect.Bottom := max(max(max( p1.Y, p2.Y), p3.Y), p4.Y);

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

procedure TG2GraphModuleFMX.ParsePanelData(aParent: TFMXObject; aModuleType : byte);
var id : string;
    i : integer;
    G2 : TG2GraphFMX;
begin
  if assigned(SVGAgent) and assigned(aParent) then begin

    id := 'module_' + IntToStr(aModuleType);

    {FSVGControl := TSVGG2Module.Create( self, id, nil, CreateUnityMatrix, CreateUnityMatrix);
    aParent.AddObject( FSVGControl);

    SVGAgent.ParseSVG( id, FSVGControl, CreateUnityMatrix);
    FSVGControl.ModuleData := self;}

    G2 := PatchPart.Patch.Performance.G2 as TG2GraphFMX;
    i := 0;
    while (i < G2.FModulePanels.Count) and not((G2.FModulePanels[i] as TSVGG2Module).ID = id) do
      inc(i);

    if (i < G2.FModulePanels.Count) then begin
      TSVGObject(FSVGControl):= (G2.FModulePanels[i] as TSVGG2Module).CreateCopy(self, nil);
      FSVGControl.ModuleData := self;
      aParent.AddObject( FSVGControl);
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
  FZoom := 1;
  FRedrawBuffer := True;
  FBitmap := TBitmap.Create( trunc(Width) + 1, trunc(Height) + 1);
end;

constructor TSVGG2Module.Create( AOwner: TComponent; aId : string; aSVGParent: TSVGGroup; aCTM, aUserMatrix : TMatrix);
begin
  inherited;
  FZoom := 1;
  FRedrawBuffer := True;
  FBitmap := TBitmap.Create( trunc(Width) + 1, trunc(Height) + 1);
end;

destructor TSVGG2Module.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

function TSVGG2Module.CreateCopy(AOwner: TComponent; aSVGParent : TSVGGroup): TSVGObject;
begin
  Result := TSVGG2Module.Create(AOwner);
  Result.SVGParent := aSVGParent;
  Result.Assign(self);
end;

{procedure TSVGG2Module.AddSVGChild(aSVGObject: TSVGObject);
begin
  inherited;
end;}

procedure TSVGG2Module.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TSVGG2Module then begin
    FZoom := TSVGG2Module(Source).FZoom;
    FRedrawBuffer := TSVGG2Module(Source).FRedrawBuffer;
    FBitmap.Assign( TSVGG2Module(Source).FBitmap);
    FPanel := FindObject(ID + '_panel') as TSVGPath;
  end;
end;

function TSVGG2Module.CreateUse( AOwner : TComponent; aUseNode, aRefNode : TSVGNode; aUseID : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix; var aSubTreeOwner : TSVGGroup) : TSVGGroup;
var AttributeValue, CodeRef, CtrlType, CtrlStyle : string;
    ConnLink : TSVGG2ConnLink;
    ParamLink : TSVGG2ParamLink;
begin
  if aUseNode.GetAttribute( 'nmg2.CtrlType', CtrlType) then begin

    if CtrlType = 'Connector' then begin

      ConnLink := TSVGG2ConnLink.Create(self, aUseId, aSVGParent, aCTM, aUserMatrix);
      ConnLink.ModulePanel := self;
      if aUseNode.GetAttribute( 'nmg2.CodeRef', AttributeValue) then
        ConnLink.FCodeRef := StrToInt(AttributeValue);

      aSubTreeOwner := ConnLink;
      Result := ConnLink;


    end else

    if (CtrlType = 'textfield')
      or (CtrlType = 'btntext')
      or (CtrlType = 'btnincdec')
      or (CtrlType = 'btnflat')
      or (CtrlType = 'btnradio')
      or (CtrlType = 'knob') then begin

      ParamLink := TSVGG2ParamLink.Create( self, aUseID, aSVGParent, aCTM, aUSerMatrix);
      ParamLink.ModulePanel := self;

      aSubtreeOwner := ParamLink;
      Result := ParamLink;

      if aUseNode.GetAttribute( 'nmg2.CodeRef', AttributeValue) then
        ParamLink.FCodeRef := StrToInt(AttributeValue);

      if aUseNode.GetAttribute( 'nmg2.InfoFunc', AttributeValue) then
        ParamLink.FInfoFunc := StrToInt(AttributeValue);

      if aUseNode.GetAttribute( 'nmg2.TextFunc', AttributeValue) then
        ParamLink.FTextFunc := StrToInt(AttributeValue);

      if aUseNode.GetAttribute( 'nmg2.MasterRef', AttributeValue) then
        ParamLink.FMasterRef := StrToInt(AttributeValue);

      if aUseNode.GetAttribute( 'nmg2.Dependencies', AttributeValue) then
        ParamLink.FDependencies := AttributeValue;

      if aUseNode.GetAttribute( 'nmg2.CtrlType', AttributeValue) then
        ParamLink.FCtrlType := AttributeValue;

      if aUseNode.GetAttribute( 'nmg2.CtrlStyle', AttributeValue) then
        ParamLink.FCtrlStyle := AttributeValue;

    end else
      Result := inherited;

  end else
    Result := inherited;
end;

{function TSVGG2Module.CreateGroup( AOwner : TComponent; aNode: TSVGNode; aId : string; aSVGParent : TSVGGroup;
  aCTM, aUserMatrix : TMatrix; var aSubTreeOwner : TSVGGroup): TSVGGroup;
var AttributeValue : string;
    ConnLink : TSVGG2ConnLink;
    ParamLink : TSVGG2ParamLink;
    TextField : TSVGTextField;
    BtnText : TSVGBtn;
    BtnFlat : TSVGBtnFlat;
    BtnRadio : TSVGBtnRadio;
    BtnIncDec : TSVGBtnIncDec;
    BtnCount : integer;
    Knob : TSVGKnob;
    Slider : TSVGSlider;
begin
  if pos('_paramlink_', aId)>0 then begin

    ParamLink := TSVGG2ParamLink.Create(self, aId, aSVGParent, aCTM, aUserMatrix);
    Result := ParamLink;
    aSubTreeOwner := ParamLink;

    ParamLink.FModuleData := FModuleData;

    if aNode.GetAttribute( 'nmg2.CodeRef', AttributeValue) then
      ParamLink.FCodeRef := StrToInt(AttributeValue);

    if aNode.GetAttribute( 'nmg2.InfoFunc', AttributeValue) then
      ParamLink.FInfoFunc := StrToInt(AttributeValue);

    if aNode.GetAttribute( 'nmg2.TextFunc', AttributeValue) then
      ParamLink.FTextFunc := StrToInt(AttributeValue);

    if aNode.GetAttribute( 'nmg2.MasterRef', AttributeValue) then
      ParamLink.FMasterRef := StrToInt(AttributeValue);

    if aNode.GetAttribute( 'nmg2.Dependencies', AttributeValue) then
      ParamLink.FDependencies := AttributeValue;

    if aNode.GetAttribute( 'nmg2.CtrlType', AttributeValue) then
      ParamLink.FCtrlType := AttributeValue;

    if ParamLink.FCtrlType = 'textfield' then begin

      TextField := TSVGTextField.Create(self, aId, aSVGParent, aCTM, aUserMatrix);
      TextField.FModulePanel := self;
      ParseDependencies( TextField, ParamLink.FMasterRef, ParamLink.FDependencies, ParamLink.FTextFunc);
      Result := TextField;
      aSubTreeOwner := Result;

    end else

    if ParamLink.FCtrlType = 'btntext' then begin

      BtnText := TSVGBtn.Create(self, aId, aSVGParent, aCTM, aUserMatrix);
      BtnText.FModulePanel := self;
      if assigned(ModuleData) then begin
        BtnText.Parameter := (ModuleData.Parameter[ ParamLink.FCodeRef] as TG2GraphParameterFMX);
        BtnText.Parameter.InfoFunctionIndex := ParamLink.FInfoFunc;
      end;
      Result := BtnText;
      aSubTreeOwner := Result;

    end else

    if ParamLink.FCtrlType = 'btnflat' then begin

      BtnFlat := TSVGBtnFlat.Create(self, aId, aSVGParent, aCTM, aUserMatrix);
      BtnFlat.FModulePanel := self;
      if assigned(ModuleData) then begin
        BtnFlat.Parameter := (ModuleData.Parameter[ ParamLink.FCodeRef] as TG2GraphParameterFMX);
        BtnFlat.Parameter.InfoFunctionIndex := ParamLink.FInfoFunc;
      end;
      Result := BtnFlat;
      aSubTreeOwner := Result;

    end else

    if ParamLink.FCtrlType = 'btnradio' then begin

      BtnCount := 0;
      if aNode.GetAttribute( 'nmg2.ButtonCount', AttributeValue) then
         BtnCount := StrToInt(AttributeValue);

      BtnRadio := TSVGBtnRadio.Create(self, aId, aSVGParent, aCTM, aUserMatrix);
      BtnRadio.FModulePanel := self;
      //BtnRadio.BtnCount := BtnCount;
      if assigned(ModuleData) then begin
        BtnRadio.Parameter := (ModuleData.Parameter[ ParamLink.FCodeRef] as TG2GraphParameterFMX);
        BtnRadio.Parameter.InfoFunctionIndex := ParamLink.FInfoFunc;
      end;

      Result := BtnRadio;
      aSubTreeOwner := Result;

    end else

    if ParamLink.FCtrlType = 'btnradioedit' then begin

      BtnCount := 0;
      if aNode.GetAttribute( 'nmg2.ButtonCount', AttributeValue) then
         BtnCount := StrToInt(AttributeValue);

      BtnRadio := TSVGBtnRadio.Create(self, aId, aSVGParent, aCTM, aUserMatrix);
      BtnRadio.FModulePanel := self;
      //BtnRadio.BtnCount := BtnCount;
      if assigned(ModuleData) then begin
        BtnRadio.Parameter := (ModuleData.Parameter[ ParamLink.FCodeRef] as TG2GraphParameterFMX);
        BtnRadio.Parameter.InfoFunctionIndex := ParamLink.FInfoFunc;
      end;

      Result := BtnRadio;
      aSubTreeOwner := Result;

    end else

    if ParamLink.FCtrlType = 'btnincdec' then begin

      BtnIncDec := TSVGBtnIncDec.Create(self, aId, aSVGParent, aCTM, aUserMatrix);
      BtnIncDec.FModulePanel := self;
      if assigned(ModuleData) then begin
        BtnIncDec.Parameter := (ModuleData.Parameter[ ParamLink.FCodeRef] as TG2GraphParameterFMX);
        BtnIncDec.Parameter.InfoFunctionIndex := ParamLink.FInfoFunc;
      end;

      Result := BtnIncDec;
      aSubTreeOwner := Result;

    end else

    if ParamLink.FCtrlType = 'knob' then begin

      if aNode.GetAttribute( 'nmg2.CtrlStyle', AttributeValue) then begin
        if LowerCase(AttributeValue) = 'slider' then begin

          Slider := TSVGSlider.Create(self, aId, aSVGParent, aCTM, aUserMatrix);
          Slider.FModulePanel := self;
          if assigned(ModuleData) then begin
            Slider.Parameter := (ModuleData.Parameter[ ParamLink.FCodeRef] as TG2GraphParameterFMX);
            Slider.Parameter.InfoFunctionIndex := ParamLink.FInfoFunc;
          end;

          Result := Slider;
          aSubTreeOwner := Result;

        end else
          if (LowerCase(AttributeValue) = 'big') or
             (LowerCase(AttributeValue) = 'medium') or
             (LowerCase(AttributeValue) = 'resetmedium') or
             (LowerCase(AttributeValue) = 'reset') or
             (LowerCase(AttributeValue) = 'small') then begin

            Knob := TSVGKnob.Create(self, aId, aSVGParent, aCTM, aUserMatrix);
            Knob.FModulePanel := self;
            if assigned(ModuleData) then begin
              Knob.Parameter := (ModuleData.Parameter[ ParamLink.FCodeRef] as TG2GraphParameterFMX);
              Knob.Parameter.InfoFunctionIndex := ParamLink.FInfoFunc;
            end;

            Result := Knob;
            aSubTreeOwner := Result;
          end;
      end;
    end;

  end else

  if pos('_connlink_', aId)>0 then begin

    ConnLink := TSVGG2ConnLink.Create(self, aId, aSVGParent, aCTM, aUserMatrix);
    aSubTreeOwner := ConnLink;

    //ConnLink.FModuleData := FModuleData;

    if aNode.GetAttribute( 'nmg2.CodeRef', AttributeValue) then
      ConnLink.FCodeRef := StrToInt(AttributeValue);

    Result := ConnLink;

  end else

    Result := inherited;
end;}

procedure TSVGG2Module.ParseDependencies( aControl : TSVGG2Control;
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

function TSVGG2Module.CreateSpan( AOwner : TComponent; aNode : TSVGNode; aID : string; aSVGParent : TSVGGroup; aCTM, aUserMatrix : TMatrix) : TSVGSpan;
var FLabel : TSVGLabel;
begin
  FLabel := TSVGLabel.Create(self, aId, aSVGParent, aCTM, aUserMatrix);
  FLabel.BoundsRect := RectF(0, 0, 80, 10);
  FLabel.TextAlign := TTextAlign.taLeading;
  Result := FLabel;
end;

function TSVGG2Module.CreatePath( AOwner : TComponent; aNode: TSVGNode; aId : string; aSVGParent : TSVGGroup;
  aCTM, aUserMatrix : TMatrix): TSVGPath;
begin
  Result := inherited;

  if pos('_panel', aId) > 0 then
    FPanel := Result as TSVGPath;
end;

function TSVGG2Module.GetNewCol: TBits7;
begin
  Result := trunc((Position.X) / UNITS_COL);
end;

function TSVGG2Module.GetNewRow: TBits7;
begin
  Result := trunc((Position.Y) / UNITS_ROW);
end;

procedure TSVGG2Module.Paint;
var SaveMatrix : TMatrix;
begin
  if FRedrawBuffer then begin
    FBitmap.Canvas.BeginScene;
    try
      SaveMatrix := FBitmap.Canvas.Matrix;
      FBitmap.Canvas.SetMatrix( CreateScaleMatrix(FZoom, FZoom));
      SVGPaint( FBitmap.Canvas);
    finally
      FBitmap.Canvas.SetMatrix(SaveMatrix);
      FRedrawBuffer := False;
      FBitmap.Canvas.EndScene;
    end;
  end;
  Canvas.DrawBitmap( FBitmap, RectF(0, 0, Width, Height), RectF(0, 0, Width, Height), AbsoluteOpacity);
end;

procedure TSVGG2Module.Redraw;
begin
  FRedrawBuffer := True;
  Repaint;
end;

function TSVGG2Module.SelectHitpath( P : TPointF): TSVGHitpath;
var SVGObject : TSVGObject;
begin
  Result := nil;
  SVGObject := ObjectAtPt( PointF(P.X / FZoom, P.Y / FZoom) );
  if SVGObject is  TSVGHitPath then begin
    Result := SVGObject as TSVGHitPath;
  end;
end;

function TSVGG2Module.SelectControl( P : TPointF): TSVGG2Control;
var SVGObject : TSVGObject;
    SVGHitPath : TSVGHitPath;
begin
  Result := nil;
  SVGObject := ObjectAtPt( PointF(P.X / FZoom, P.Y / FZoom) );
  if SVGObject is  TSVGHitPath then begin
    SVGHitPath := SVGObject as TSVGHitPath;
    Result := SVGHitPath.Control;
  end;
end;

procedure TSVGG2Module.SetModuleData(const aValue: TG2GraphModuleFMX);

  procedure SetChildModuleData( aSVGGroup : TSVGGroup);
  var i : integer;
  begin
    for i := 0 to aSVGGroup.Count - 1 do begin

      if aSVGGroup.Item[i] is TSVGG2Control then begin
        (aSVGGroup.Item[i] as TSVGG2Control).ModuleData := FModuleData;
      end;

      if aSVGGroup.Item[i] is TSVGGroup then
        SetChildModuleData( aSVGGroup.Item[i] as TSVGGroup);
    end;
  end;

begin
  if FModuleData <> aValue then begin
    FModuleData := aValue;
    SetChildModuleData(self);
  end;
end;

procedure TSVGG2Module.SetModuleColor(aValue: TBits8);
begin
  if assigned(SVGAgent) then begin
    SVGAgent.ParseSVGPaintServer( 'PanelGradient_' + IntToStr(aValue), FPanel.Fill, RectF(0,0,1,1), RectF(0,0,1,1));
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

procedure TSVGG2Module.SetZoom(aValue: single);
begin
  if aValue <> FZoom then begin
    FZoom := aValue;
    Position.X := FModuleData.Col * UNITS_COL * FZoom;
    Position.Y := FModuleData.Row * UNITS_ROW * FZoom;

    Width := UNITS_COL * FZoom;
    Height := FModuleData.HeightUnits * UNITS_ROW * FZoom;
    FRedrawBuffer := True;
  end;
end;

procedure TSVGG2Module.Resize;
begin
  if (FBitmap.Width <> trunc(Width) + 1) or (FBitmap.Height <> trunc(Height) + 1) then

  FBitmap.SetSize( trunc(Width) + 1, trunc(Height) + 1);
  FRedrawBuffer := True;
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

procedure TModuleBitmapBuffer.Paint;
var i, j, r, c, w, h : integer;
begin
  Canvas.BeginScene;
  try
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
    Canvas.EndScene;
  end;

  inherited;
end;

procedure TModuleBitmapBuffer.SetZoom(aValue: single);
var i : integer;
    SVGModule : TSVGG2Module;
    id : string;
    SaveMatrix : TMatrix;
begin
  if aValue <> FZoom then begin
    FZoom := aValue;

    FBGBuffer.SetSize( trunc(UNITS_COL*FZoom) , trunc(UNITS_ROW*FZoom));

    if not(assigned(FBackground)) and assigned(SVGAgent) then begin
      id := 'patchbackground';

      FBackground := TSVGG2Module.Create( self, id, nil, CreateUnityMatrix, CreateUnityMatrix);
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
    end;
  end;
end;



end.


