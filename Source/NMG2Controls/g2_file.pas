unit g2_file;

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
//
//  This unit contains the G2 classes to read or write G2 objects
//  to filestream or memory stream.
//
//  Parts ported from https://github.com/msg/g2ools
//  Parts used from http://www.iaf.nl/Users/BlueHell/
//
//  ////////////////////////////////////////////////////////////////////////////

// 2011-11-14 bve : Performance chunk, first G2 gives Slotno 1..4, second G2 gives 5..8..?

interface

uses
{$IFDEF VER230}
  System.Contnrs,
{$ELSE}
  Contnrs,
{$ENDIF}
  Windows, Classes, SysUtils, DOM, XMLRead, g2_types, g2_database;

const
  MIDI_BLOCK_SIZE = 479;//419; // The number of whole octets in  a full size MIDI packet

type
  TG2FilePatch = class;
  TG2FilePatchPart = class;
  TCableList = class;
  TModuleParameters = class;
  TParamSet = class;
  TParameterLabels = class;
  TModuleLabels = class;
  TGlobalKnob = class;
  TG2FileSlot = class;
  TG2File = class;
  TG2FileModule = class;
  TG2FileParameter = class;
  TG2FileConnector = class;
  TG2FilePerformance = class;
  TG2FileDataStream = class;

  TCreateModuleEvent = procedure(Sender: TObject; SenderID : integer; Module : TG2FileModule) of object;
  TAddModuleEvent = procedure(Sender: TObject; SenderID : integer; Module : TG2FileModule) of object;
  TDeleteModuleEvent = procedure(Sender : TObject; SenderID : integer; Location: TLocationType; ModuleIndex : integer) of object;
  TAssignKnobEvent = procedure(Sender: TObject; SenderID : integer; Slot : byte; KnobIndex : integer) of object;
  TAssignGlobalKnobEvent = procedure(Sender: TObject; SenderID : integer; KnobIndex : integer) of object;
  TDeassignKnobEvent = procedure(Sender: TObject; SenderID : integer; Slot : byte; KnobIndex : integer) of object;
  TDeassignGlobalKnobEvent = procedure(Sender: TObject; SenderID : integer; KnobIndex : integer) of object;
  TParameterChangeEvent = procedure(Sender: TObject; SenderID : integer; Slot, Variation : byte; Location : TLocationType; ModuleIndex, ParamIndex : byte; aValue : byte) of object;
  TModuleModeChangeEvent = procedure(Sender: TObject; SenderID : integer; Slot : byte; Location : TLocationType; ModuleIndex, ParamIndex : byte; aValue : byte) of object;
  TSelectSlotEvent = procedure(Sender : TObject; SenderID : integer; Slot : integer) of object;

  // Header for VST patch chunk file
  TFXPHeader = record
    chunkMagic  : packed array[0..3] of AnsiChar;        // 'CcnK'
    byteSize    : Cardinal;                              // of this chunk, excl. magic + byteSize
    fxMagic     : packed array[0..3] of AnsiChar;        // 'FxCh', 'FPCh', or 'FBCh'
    version     : Cardinal;
    fxID        : packed array[0..3] of AnsiChar;        // fx unique id
    fxVersion   : Cardinal;
    numPrograms : Cardinal;
    name        : packed array[0..27] of byte;
    chunkSize   : Cardinal;
  end;

  // Header for VST bank chunk file
  TFXBHeader = record
    chunkMagic  : packed array[0..3] of AnsiChar;        // 'CcnK'
    byteSize    : Cardinal;                              // of this chunk, excl. magic + byteSize
    fxMagic     : packed array[0..3] of AnsiChar;        // 'FxCh', 'FPCh', or 'FBCh'
    version     : Cardinal;
    fxID        : packed array[0..3] of AnsiChar;        // fx unique id
    fxVersion   : Cardinal;
    numPrograms : Cardinal;
    future      : packed array[0..127] of byte;
    chunkSize   : Cardinal;
  end;

  // Base class for loading/saving performances and patches from/to stream
  TG2FileDataStream = class( TComponent)
  private
    FChecksum         : Integer; // MIDI checksum septet
    FVersion          : byte;
    FPatchType        : byte;
  protected
    //function GetMaxVariations : byte; virtual; abstract;
    //procedure SetMaxVariations( aValue : byte); virtual; abstract;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    function    LoadFileHeader( aStream : TStream; aChunk : TPatchChunk): boolean;
    procedure   Read( aChunk : TPatchChunk); virtual;
    procedure   Write( aChunk : TPatchChunk; aVariationCount : byte); virtual;

    function    ReadClaviaString( aStream : TStream): AnsiString;
    procedure   WriteClaviaString( aStream : TStream; aValue : AnsiString);

    function    CreateFromMidiStream(const aStream: TStream; aLogLines : TStrings): TMemoryStream;
    procedure   CreateMidiBlock( aBlockNr, aBlockCount, anOffset, aSize: Integer; aChunk : TPatchChunk; aMidiStream : TStream);
    procedure   SaveMidiToStream( const aStream : TStream);
    function    ParseMidiConst  ( const aStream : TStream; aValue: Byte): Byte;
    function    ParseMidiSeptet ( const aStream : TStream): Byte;
    function    ParseMidiInt    ( const aStream : TStream): Integer;
    function    ParseMidiString ( const aStream : TStream): AnsiString;
    procedure   AddMidiByte( aStream : TStream; aByte: Byte);
    procedure   AddMidiInt( aStream : TStream; anInteger: Integer);
    procedure   AddMidiName( aStream : TStream; aName : AnsiString);

    class function LoadFileData( AOwner: TComponent; aStream : TStream; aLogLines : TStrings): TG2FileDataStream; virtual;
    class function LoadFileStream( AOwner: TComponent; aStream : TStream; aChunk : TPatchChunk): TG2FileDataStream; virtual;
    class function LoadMidiData( AOwner : TComponent; aStream: TStream; aLogLines : TStrings): TG2FileDataStream;
    class function LoadMidiStream( AOwner : TComponent; aStream: TStream): TG2FileDataStream; virtual;

    property Version : byte read FVersion write FVersion;
    property PatchType : byte read FPatchType write FPatchType;
    //property MaxVariations : byte read GetMaxVariations write SetMaxVariations;
  end;

  TG2FileModule = class(TComponent)
  private
    // File storage
    FTypeID       : TBits8;
    FModuleIndex  : TBits8;
    FCol          : TBits7;
    FRow          : TBits7;
    FModuleColor  : TBits8;
    FUprate       : TBits1; // processing rate increased due to audio modulation
    FIsLed        : TBits1; // unknown function
    //FUnknown1   : TBits8;
    FUnknown1     : TBits6;
    //FUnknown2   : TBits4;
    FModeCount    : TBits4;
    FModeInfo     : array of TBits6;

    // Interface
    FSelected      : Boolean;
    FSelectedParam : integer;
    FPatchPart     : TG2FilePatchPart;
    FLocation      : TLocationType;
    FModuleName    : AnsiString;
    FNewUprate     : TBits1; // for calculating uprate changes
    FInConnectors  : array of TG2FileConnector;
    FOutConnectors : array of TG2FileConnector;
    FParams        : array of TG2FileParameter;
    FModes         : array of TG2FileParameter;

    function    GetInputConnector( ConnectorIndex : integer): TG2FileConnector;
    function    GetOutputConnector( ConnectorIndex : integer): TG2FileConnector;
    function    GetInputConnectorCount : integer;
    function    GetOutputConnectorCount : integer;
    function    GetParameter( ParamIndex : integer): TG2FileParameter;
    function    GetMode( ModeIndex : integer): TG2FileParameter;
    function    GetParameterCount : integer;
    function    GetModeCount : integer;
    function    GetColor: integer;
    procedure   SetUprate( Value : TBits1);
    procedure   SetModeInfo( aModeIndex : integer; aValue : TBits6);
    function    GetModeInfo( aModeIndex : integer): TBits6;
    function    GetSelectedParam : TG2FileParameter;
    procedure   SetSelectedParam( aValue : TG2FileParameter);
  protected
    procedure   SetCol( Value : TBits7); virtual;
    procedure   SetRow( Value : TBits7); virtual;
    procedure   SetModuleColor( Value : TBits8); virtual;
    procedure   SetModuleName( aValue : AnsiString); virtual;
    procedure   SetSelected( aValue: boolean); virtual;
    function    GetNewCol : TBits7; virtual;
    function    GetNewRow : TBits7; virtual;
    function    GetAssignableKnobCount : integer;
  public
    //constructor Create( AOwner: TComponent); {FMX override;} override;
    constructor Create( aPatchPart : TG2FilePatchPart); virtual;
    destructor  Destroy; override;
    procedure   Init;
    procedure   Read( aChunk : TPatchChunk);
    procedure   Write( aChunk : TPatchChunk);

    procedure   InitModule( aLocation : TLocationType; aModuleDef : TXMLModuleDefType; aParamDefList : TXMLParamDefListType);

    //function    CreateCopy( AOwner : TComponent) : TG2FileModule; virtual;
    function    CreateCopy( aPatchPart : TG2FilePatchPart) : TG2FileModule; virtual;
    procedure   Copy( aModule : TG2FileModule); virtual;

    function    CreateParameter: TG2FileParameter; virtual;
    function    CreateConnector: TG2FileConnector; virtual;
    procedure   InvalidateCables;

    function    GetNextParam : TG2FileParameter;
    function    GetPrevParam : TG2FileParameter;
    procedure   SelectNextParam;
    procedure   SelectPrevParam;

    property    TypeID      : TBits8 read FTypeID write FTypeID;
    property    ModuleIndex : TBits8 read FModuleIndex write FModuleIndex;
    property    Col         : TBits7 read FCol write SetCol;
    property    Row         : TBits7 read FRow write SetRow;
    property    ModuleColor : TBits8 read FModuleColor write SetModuleColor;
    property    Uprate      : TBits1 read FUprate write SetUprate;
    property    IsLed       : TBits1 read FIsLed write FIsLed;
    property    ModeInfo    [ Index : integer]: TBits6 read GetModeInfo write SetModeInfo;
    property    PatchPart : TG2FilePatchPart read FPatchPart write FPatchPart;
    property    ModuleName  : AnsiString read FModuleName write SetModuleName;
    property    InConnector [ Index : integer]: TG2FileConnector read GetInputConnector;
    property    OutConnector[ Index : integer]: TG2FileConnector read GetOutputConnector;
    property    Parameter   [ Index : integer]: TG2FileParameter read GetParameter;
    property    Mode        [ Index : integer]: TG2FileParameter read GetMode;
    property    InConnectorCount : integer read GetInputConnectorCount;
    property    OutConnectorCount : integer read GetOutputConnectorCount;
    property    ParameterCount : integer read GetParameterCount;
    property    ModeCount : integer read GetModeCount;
    property    Location : TLocationType read FLocation write Flocation;
    property    Color : integer read GetColor;
    property    Selected    : boolean read FSelected write SetSelected;
    property    NewRow : TBits7 read GetNewRow;
    property    NewCol : TBits7 read GetNewCol;
    property    NewUprate : TBits1 read FNewUprate write FNewUprate;
    property    SelectedParam : TG2FileParameter read GetSelectedParam write SetSelectedParam;
    property    AssignableKnobCount : integer read GetAssignableKnobCount;
  end;

  TModuleList = class( TObjectList)
  private
    FPatchPart   : TG2FilePatchPart;
    FLocation    : TBits2;
    FModuleCount : TBits8;
    function    GetModule( aIndex : integer) : TG2FileModule;
    procedure   SetModule( aIndex : integer; const aValue : TG2FileModule);
  public
    constructor Create( AOwnsObjects: Boolean; aPatchPart : TG2FilePatchPart);
    constructor CopySelected( AOwnsObjects : boolean; aPatchPart : TG2FilePatchPart; aModuleList : TModuleList);
    destructor  Destroy; override;
    procedure   Init;
    function    FindModule( aModuleIndex : integer) : TG2FileModule;
    function    GetMaxModuleIndex : integer;
    function    GetNoOffModuleType( aModuleType : byte) : integer;
    function    GetSelectedCount: integer;
    procedure   DeleteModule( aModuleIndex : byte);
    function    GetModuleAbove( aModuleIndex : byte) : TG2FileModule;
    function    GetModuleUnder( aModuleIndex : byte) : TG2FileModule;
    function    GetModuleLeft( aModuleIndex : byte) : TG2FileModule;
    function    GetModuleRight( aModuleIndex : byte) : TG2FileModule;
    procedure   Read( aChunk : TPatchChunk);
    procedure   Write( aChunk : TPatchChunk);
    function    AddModule( aValue : TG2FileModule): integer;
    property    Items[ aIndex : integer]: TG2FileModule read GetModule write SetModule; default;
  end;

  TG2FileCable = class
  private
    FCableColor    : TBits3;
    FModuleFrom    : TBits8;
    FConnectorFrom : TBits6;
    FLinkType      : TBits1;
    FModuleTo      : TBits8;
    FConnectorTo   : TBits6;

    FFromConnector,
    FToConnector   : TG2FileConnector;
  public
    constructor Create( AOwner: TComponent); virtual;
    constructor CopyCreate( AOwner: TComponent; aCable : TG2FileCable);
    destructor  Destroy; override;
    procedure   Init;
    procedure   ConnectorMoved; virtual;
    procedure   Read( aChunk : TPatchChunk);
    procedure   Write( aChunk : TPatchChunk);

    property    ModuleFrom : TBits8 read FModuleFrom write FModuleFrom;
    property    ConnectorFrom : TBits6 read FConnectorFrom write FConnectorFrom;
    property    LinkType : TBits1 read FLinkType write FLinkType;
    property    ModuleTo : TBits8 read FModuleTo write FModuleTo;
    property    ConnectorTo : TBits6 read FConnectorTo write FConnectorTo;
    property    FromConnector : TG2FileConnector read FFromConnector write FFromConnector;
    property    ToConnector : TG2FileConnector read FToConnector write FToConnector;
    property    CableColor : Byte read FCableColor write FCableColor;
  end;

  TCableList = class( TObjectList)
  private
    FPatchPart  : TG2FilePatchPart;
    FLocation   : TBits2;
    FUnknown    : TBits12;
    FCableCount : TBits10;
    function    GetCable( aIndex : integer) : TG2FileCable;
    procedure   SetCable( aIndex : integer; const aValue : TG2FileCable);
  public
    constructor Create( AOwnsObjects: Boolean; aPatchPart : TG2FilePatchPart);
    constructor CopySelected( AOwnsObjects: Boolean; aPatchPart : TG2FilePatchPart; aModuleList : TModuleList; aCableList : TCableList);
    destructor  Destroy; override;
    procedure   Init;
    procedure   DeleteCable( aCable : TG2FileCable);
    procedure   Read( aChunk : TPatchChunk);
    procedure   Write( aChunk : TPatchChunk);
    function    FindCable( FromModule : byte; FromConnector : byte; ToModule : byte; ToConnector : byte): TG2FileCable;
    function    AddCable( aValue : TG2FileCable): integer;
    property    Items[ aIndex : integer]: TG2FileCable read GetCable write SetCable; default;
    property    Unknown : TBits12 read FUnknown write FUnknown;
  end;

  TNote = class
  private
    FNote    : TBits7;
    FAttack  : TBits7;
    FRelease : TBits7;
  public
    procedure   Init;
    procedure   Read( aChunk : TPatchChunk);
    procedure   Write( aChunk : TPatchChunk);
  end;

  TCurrentNote = class( TObjectList)
  private
    FLastNote  : TNote;
    FNoteCount : TBits5;
    function    GetNote( aIndex : integer) : TNote;
    procedure   SetNote( aIndex : integer; const aValue : TNote);
  public
    constructor Create( AOwnsObjects : boolean);
    destructor  Destroy; override;
    procedure   Init;
    procedure   Read( aChunk : TPatchChunk);
    procedure   Write( aChunk : TPatchChunk);
    function    AddNote( aValue : TNote): integer;
    property    Items[ aIndex : integer]: TNote read GetNote write SetNote; default;
  end;

  TParams = class
  private
    // Set of parameter values for one module one variation
    FParamSet    : TParamSet;
    FVariation   : TBits8;
    FParamValues : array of TBits7;
    function    GetParamValue( aIndex : integer): TBits7;
    procedure   SetParamValue( aIndex : integer; aValue : TBits7);
  public
    constructor Create( aParamSet : TParamSet);
    constructor Copy( aParamSet : TParamSet; aParams : TParams);
    destructor  Destroy; override;
    procedure   Init;
    procedure   Read( aChunk : TPatchChunk);
    procedure   Write( aChunk : TPatchChunk);

    property    Variation : TBits8 read FVariation write FVariation;
    property    ParamValues[ aIndex : integer]: TBits7 read GetParamValue write SetParamValue;
  end;

  TParamSet = class( TObjectList)
  private
    // Set of parameter values for one module all variations
    // 0 .. 7 : variations 1 .. 8, 8 : init variation
    FModuleIndex      : TBits8;
    FParameterCount   : Integer;
    FModuleParameters : TModuleParameters;
    function    GetVariation( aIndex : integer) : TParams;
    procedure   SetVariation( aIndex : integer; const aValue : TParams);
  public
    constructor Create( AOwnsObjects : boolean; aModuleParameters : TModuleParameters);
    constructor Copy( AOwnsObjects : boolean; aModuleParameters : TModuleParameters; aParamSet : TParamSet);
    destructor  Destroy; override;
    procedure   Init;
    procedure   Read( aChunk : TPatchChunk);
    procedure   Write( aChunk : TPatchChunk; aVariationCount : byte);
    procedure   AddNewVariation;
    procedure   CopyVariation( aFromVariation, aToVariation : byte);
    function    FindVariation( aVariation : integer): TParams;
    function    AddVariation( aValue : TParams): integer;
    property    Items[ aIndex : integer]: TParams read GetVariation write SetVariation; default;
    property    ModuleIndex : TBits8 read FModuleIndex write FModuleIndex;
    property    ParameterCount : Integer read FParameterCount write FParameterCount;
  end;

  TModuleParameters = class( TObjectList)
  private
    FLocation       : Tbits2;
    FParamSetCount  : TBits8;
    FVariationCount : TBits8;
    //FPatch          : TG2FilePatch;
    function    GetParamSet( aIndex : integer) : TParamSet;
    procedure   SetParamSet( aIndex : integer; const aValue : TParamSet);
  public
    constructor Create( AOwnsObjects : boolean{; aPatch : TG2FilePatch});
    constructor CopySelected( AOwnsObjects : boolean; {aPatch : TG2FilePatch;} aModuleList : TModuleList; aModuleParameters : TModuleParameters);
    destructor  Destroy; override;
    procedure   Init;
    procedure   Read( aChunk : TPatchChunk);
    procedure   Write( aChunk : TPatchChunk; aVariationCount : byte);
    procedure   DeleteModule( aModuleIndex : Byte);
    procedure   AddNewVariation;
    procedure   CopyVariation( aFromVariation, aToVariation : byte);
    function    FindParamSet( aModuleIndex : integer): TParamSet;
    function    FindParamValue( aModuleIndex, aVariation, aParamIndex : integer): TBits7;
    function    AddParamSet( aValue : TParamSet): integer;
    property    Items[ aIndex : integer]: TParamSet read GetParamSet write SetParamSet; default;
    property    ParamSetCount : TBits8 read FParamSetCount write FParamSetCount;
    property    VariationCount : TBits8 read FVariationCount write FVariationCount;
  end;

  TMorph = class
  private
    FDials     : array of TBits7;
    FModes     : array of TBits7;
    FMorphName : AnsiString;
    function   GetDial( aIndex : integer): TBits7;
    procedure  SetDial( aIndex : integer; aValue : TBits7);
    function   GetMode( aIndex : integer): TBits7;
    procedure  SetMode( aIndex : integer; aValue : TBits7);
  public
    property   Dials[ Index : integer]: TBits7 read GetDial write SetDial;
    property   Modex[ Index : integer]: TBits7 read GetMode write SetMode;
    property   MorphName : AnsiString read FMorphName write FMorphName;
  end;

  TMorphParameter = class
  private
    FLocation    : TBits2;
    FModuleIndex : TBits8;
    FParamIndex  : TBits7;
    FMorph       : TBits4;
    FRange       : TBits8;
  public
    procedure   Init;
    procedure   Read( aChunk : TPatchChunk);
    procedure   Write( aChunk : TPatchChunk);
    property    Range : TBits8 read FRange write Frange;
  end;

  TMorphParameters = class
  private
    FVariationCount : TBits8;
    Unknown1        : TBits4;
    Unknown2        : array[0..5] of TBits8;
    Unknown3        : TBits4;
    FMorphCount     : TBits4;
    FReserved1      : integer;
    FReserved2      : TBits4;
    FPatch          : TG2FilePatch;
  public
    constructor Create( aPatch : TG2FilePatch);
    destructor  Destroy; override;
    procedure   Init;
    procedure   Read( aChunk : TPatchChunk);
    procedure   Write( aChunk : TPatchChunk; aVariationCount : byte);
    procedure   AddNewVariation;
  end;

  TVariation = class( TObjectList)
  private
    FPatchVol    : TBits7;
    FActiveMuted : TBits7;
    FGlide       : TBits7;
    FGlideTime   : TBits7;
    FBend        : TBits7;
    FSemi        : TBits7;
    FVibrato     : TBits7;
    FCents       : TBits7;
    FRate        : TBits7;
    FArpeggiator : TBits7;
    FArpTime     : TBits7;
    FArpType     : TBits7;
    FOctaveShift : TBits7;
    FSustain     : TBits7;
    FOctaves     : TBits7;
    function    GetMorphParam( aIndex : integer) : TMorphParameter;
    procedure   SetMorphParam( aIndex : integer; const aValue : TMorphParameter);
  public
    constructor Create( AOwnsObjects : boolean);
    destructor  Destroy; override;
    procedure   Copy( aFromVariation : TVariation);

    procedure   AddNewMorphParam( aLocation, aModuleIndex, aParamIndex, aMorphIndex, aRange: byte);
    procedure   DelMorphParam( aLocation, aModuleIndex, aParamIndex, aMorphIndex: byte);
    function    FindMorphParam( aLocation : TLocationType; aModuleIndex, aParamIndex, aMorphIndex, aVariation : byte): TMorphParameter;

    function    AddMorphParam( aValue : TMorphParameter): integer;
    property    Items[ aIndex : integer]: TMorphParameter read GetMorphParam write SetMorphParam; default;
  end;

  TPatchSettings = class
  private
    FLocation       : Tbits2;
    FSectionCnt     : TBits8;
    FVariationCount : TBits8;
    FMorphs         : array[0..NMORPHS-1] of TMorph;
    FVariations     : array of TVariation;
    //FPatch          : TG2FilePatch;
    function    GetVariation( aIndex : integer): TVariation;
    procedure   SetVariation( aIndex : integer; aValue : TVariation);
  public
    constructor Create{( aPatch : TG2FilePatch)};
    destructor  Destroy; override;
    procedure   Init;
    procedure   Read( aChunk : TPatchChunk);
    procedure   Write( aChunk : TPatchChunk; aVariationCount : byte);
    procedure   AddNewVariation;
    property    VariationCount : TBits8 read FVariationCount write FVariationCount;
    property    Variations[ Index : integer]: TVariation read GetVariation write SetVariation;
  end;

  TController = class;

  TKnob = class
  private
    FAssigned    : TBits1;
    FLocation    : TBits2;
    FModuleIndex : TBits8;
    FIsLed       : TBits2;
    FParamIndex  : TBits7;

    FParameter   : TG2FileParameter;
    FKnobIndex   : integer;
    function     GetKnobValue : byte;
    procedure    SetKnobValue( aValue : byte);
    function     GetKnobFloatValue : single;
    procedure    SetKnobFloatValue( aValue : single);
    function     GetKnobHighValue : byte;
    function     GetKnobLowValue : byte;
    function     GetKnobButtonValue : byte;
    procedure    SetKnobButtonValue( aValue : byte);
    function     GetKnobButtonFloatValue : single;
    procedure    SetKnobButtonFloatValue( aValue : single);
  public
    procedure    Init; virtual;
    procedure    Read( aChunk : TPatchChunk); virtual;
    procedure    Write( aChunk : TPatchChunk); virtual;

    property     IsAssigned : TBits1 read FAssigned write FAssigned;
    property     Location : TBits2 read FLocation write FLocation;
    property     ModuleIndex : TBits8 read FModuleIndex write FModuleIndex;
    property     ParamIndex : TBits7 read FParamIndex write FParamIndex;
    property     Parameter : TG2FileParameter read FParameter write FParameter;
    property     KnobValue : byte read GetKnobValue write SetKnobValue;
    property     KnobFloatValue : single read GetKnobFloatValue write SetKnobFloatValue;
    property     KnobButtonValue : byte read GetKnobButtonValue write SetKnobButtonValue;
    property     KnobButtonFloatValue : single read GetKnobButtonFloatValue write SetKnobButtonFloatValue;
    property     KnobHighValue : byte read GetKnobHighValue;
    property     KnobLowValue : byte read GetKnobLowValue;
    property     KnobIndex : integer read FKnobIndex;
  end;

  TKnobList = class( TObjectList)
  private
    FKnobCount : TBits16;
    function    GetKnob( aIndex : integer) : TKnob;
    procedure   SetKnob( aIndex : integer; const aValue : TKnob);
  public
    constructor Create( AOwnsObjects: Boolean);
    destructor  Destroy; override;
    procedure   Init;
    procedure   DeleteModule( aLocation, aModuleIndex : Byte);
    procedure   Read( aChunk : TPatchChunk);
    procedure   Write( aChunk : TPatchChunk);
    function    FindKnob(aLocation : TLocationType; aModuleIndex, aParamIndex : byte): integer;
    function    AddKnob( aValue : TKnob): integer;
    property    Items[ aIndex : integer]: TKnob read GetKnob write SetKnob; default;
  end;

  TController = class
  private
    FMidiCC      : TBits7;
    FLocation    : TBits2;
    FModuleIndex : TBits8;
    FParamIndex  : TBits7;
    FKnob        : TKnob;

    FParameter   : TG2FileParameter;
  public
    procedure Init;
    procedure Read( aChunk : TPatchChunk);
    procedure Write( aChunk : TPatchChunk);
    property  MidiCC : TBits7 read FMidiCC write FMidiCC;
    property  Location : TBits2 read FLocation write FLocation;
    property  ModuleIndex : TBits8 read FModuleIndex write FModuleIndex;
    property  ParamIndex : TBits7 read FParamIndex write FParamIndex;
    property  Parameter : TG2FileParameter read FParameter write FParameter;
    property  Knob : TKnob read FKnob write FKnob;
  end;

  TControllerList = class( TObjectList)
  private
    FControllerCount : TBits7;
    function    GetController( aIndex : integer) : TController;
    procedure   SetController( aIndex : integer; const aValue : TController);
  public
    constructor Create( AOwnsObjects: Boolean);
    destructor  Destroy; override;
    procedure   Init;
    procedure   DeleteModule( aModuleIndex : Byte);
    procedure   Read( aChunk : TPatchChunk);
    procedure   Write( aChunk : TPatchChunk);
    function    FindMidiCC(aLocation : TLocationType; aModuleIndex, aParamIndex : byte): byte;
    function    AddController( aValue : TController): integer;
    property    Items[ aIndex : integer]: TController read Getcontroller write SetController; default;
  end;

  TMorphNames = class
  private
    FLocation   : Tbits2;
    FEntryCount : TBits8;
    FEntry      : TBits8;
    FLength     : TBits8;
    FPatch      : TG2FilePatch;
  public
    constructor Create( aPatch : TG2FilePatch);
    destructor  Destroy; override;
    procedure   Init;
    procedure   Read( aChunk : TPatchChunk);
    procedure   Write( aChunk : TPatchChunk);
  end;

  TParamLabel = class
  private
    FName     : packed array[0..6] of AnsiChar;
    function    GetName : AnsiString;
    procedure   SetName( aValue : AnsiString);
  public
    procedure   Init;
    procedure   Read( aChunk : TPatchChunk);
    procedure   Write( aChunk : TPatchChunk);
    property    ParamLabel : AnsiString read GetName write SetName;
  end;

  TParamLabelParam = class( TObjectList)
  private
    FIsString   : TBits8;
    FParamLen   : TBits8;
    FParamIndex : TBits8;
    function    GetParamLabel( aIndex : integer) : TParamLabel;
    procedure   SetParamLabel( aIndex : integer; const aValue : TParamLabel);
  public
    constructor Create( AOwnsObjects : boolean);
    constructor CopyCreate( AOwnsObjects : boolean; aParamLabelParam : TParamLabelParam);
    destructor  Destroy; override;
    procedure   Init;
    procedure   Read( aChunk : TPatchChunk);
    procedure   Write( aChunk : TPatchChunk);
    function    AddParamLabel( aValue : TParamLabel): integer;

    property    Items[ aIndex : integer]: TParamLabel read GetParamLabel write SetParamLabel; default;
    property    IsString : TBits8 read FIsString write FIsString;
    property    ParamLen : TBits8 read FParamLen write FParamLen;
    property    ParamIndex : TBits8 read FParamIndex write FParamIndex;
  end;

  TParamLabelModule = class( TObjectList)
  private
    FModuleIndex : TBits8;
    FModuleLen   : TBits8;
    function    GetParamLabelParam( aIndex : integer) : TParamLabelParam;
    procedure   SetParamLabelParam( aIndex : integer; const aValue : TParamLabelParam);
  public
    constructor Create( AOwnsObjects: Boolean);
    constructor CopyCreate( AOwnsObjects: Boolean; aParamLabelModule : TParamLabelModule);
    destructor  Destroy; override;
    procedure   Init;
    function    FindParamLabelParam( aParamIndex : integer): TParamLabelParam;
    function    GetModuleLabelsLength: integer;
    procedure   AddParamLabel( aParamIndex : byte; aName : AnsiString);
    procedure   Read( aChunk : TPatchChunk);
    procedure   Write( aChunk : TPatchChunk);
    function    AddParamLabelParam( aValue : TParamLabelParam): integer;

    property    Items[ aIndex : integer]: TParamLabelParam read GetParamLabelParam write SetParamLabelParam; default;
    property    ModuleIndex : TBits8 read FModuleIndex write FModuleIndex;
  end;

  TParameterLabels = class( TObjectList)
  private
    FLocation    : Tbits2;
    FModuleCount : TBits8;
    function    GetParamLabelModule( aIndex : integer) : TParamLabelModule;
    procedure   SetParamLabelModule( aIndex : integer; const aValue : TParamLabelModule);
  public
    constructor Create( AOwnsObjects: Boolean);
    constructor CopySelected( AOwnsObjects: Boolean; aModuleList : TModuleList; aParameterLabels : TParameterLabels);
    destructor  Destroy; override;
    function    FindParamLabelModule( aModuleIndex : integer): TParamLabelModule;
    function    FindParamLabel( aModuleIndex, aParamIndex : byte): AnsiString;
    procedure   AddParamLabel( aModuleIndex, aParamIndex : byte; aName : AnsiString);
    procedure   Init;
    procedure   DeleteParamLabel( aModuleIndex : Byte);
    procedure   Read( aChunk : TPatchChunk);
    procedure   Write( aChunk : TPatchChunk);
    function    AddParamLabelModule( aValue : TParamLabelModule): integer;

    property    Items[ aIndex : integer]: TParamLabelModule read GetParamLabelModule write SetParamLabelModule; default;
  end;

  TModuleLabel = class
  private
    FModuleIndex : TBits8;
    FName        : AnsiString;
  public
    constructor Copy( aModuleLabel : TModuleLabel);
    procedure   Init;
    procedure   Read( aChunk : TPatchChunk);
    procedure   Write( aChunk : TPatchChunk);
    property    ModuleIndex : TBits8 read FModuleIndex write FModuleIndex;
    property    ModuleLabel : AnsiString read FName write FName;
  end;

  TModuleLabels = class( TObjectList)
  private
    FLocation  : TBits2;
    FUnknown   : TBits6;
    FNameCount : TBits8;
    function    GetModuleLabel( aIndex : integer) : TModuleLabel;
    procedure   SetModuleLabel( aIndex : integer; const aValue : TModuleLabel);
  public
    constructor Create( AOwnsObjects: Boolean);
    constructor CopySelected( AOwnsObjects: Boolean; aModuleList : TModuleList; aModuleLabels : TModuleLabels);
    destructor  Destroy; override;
    procedure   Init;
    function    FindModuleLabel( aModuleIndex : byte): AnsiString;
    procedure   DeleteModuleLabel( aModuleIndex : byte);
    procedure   AddNewModuleLabel( aModuleIndex : byte; aName : AnsiString);
    procedure   Read( aChunk : TPatchChunk);
    procedure   Write( aChunk : TPatchChunk);
    function    AddModuleLabel( aValue : TModuleLabel): integer;

    property    Items[ aIndex : integer]: TModuleLabel read GetModuleLabel write SetModuleLabel; default;
    property    Unknown : TBits6 read FUnknown write FUnknown;
  end;

  TPatchDescription = class
  private
    FUnknown1        : array[0..6] of Tbits8;
    FUnknown2        : TBits5;
    FVoiceCount      : TBits5;
    FBarPosition     : TBits14;
    FUnknown3        : TBits3;
    FRedVisible      : TBits1;
    FBlueVisible     : TBits1;
    FYellowVisible   : TBits1;
    FOrangeVisible   : TBits1;
    FGreenVisible    : TBits1;
    FPurpleVisible   : TBits1;
    FWhiteVisible    : TBits1;
    FMonoPoly        : TBits2;
    FActiveVariation : Byte;
    FCategory        : Byte;
    FUnknown4        : TBits4;
  public
    procedure Init;
    procedure Read( aChunk : TPatchChunk);
    procedure Write( aChunk : TPatchChunk);

    property  VoiceCount : TBits5 read FVoiceCount write FVoiceCount;
    property  MonoPoly : Tbits2 read FMonoPoly write FMonoPoly;
    property  ActiveVariation : Byte read FActiveVariation write FActiveVariation;
    property  RedVisible : TBits1 read FRedVisible write FRedVisible;
    property  BlueVisible : TBits1 read FBlueVisible write FBlueVisible;
    property  YellowVisible : TBits1 read FYellowVisible write FYellowVisible;
    property  OrangeVisible : TBits1 read FOrangeVisible write FOrangeVisible;
    property  GreenVisible : TBits1 read FGreenVisible write FGreenVisible;
    property  PurpleVisible : TBits1 read FPurpleVisible write FPurpleVisible;
    property  WhiteVisible : TBits1 read FWhiteVisible write FWhiteVisible;
  end;

  TPatchNotes = class
  private
    FText : packed array of byte;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   Init;
    procedure   Read( aChunk : TPatchChunk);
    procedure   Write( aChunk : TPatchChunk);
  end;

  TG2FilePatchPart = class( TComponent)
  private
    FPatch              : TG2FilePatch;
    FLocation           : TLocationType;
    FModuleList         : TModuleList;
    FCableList          : TCableList;
    FParameterList      : TModuleParameters;
    FModuleLabels       : TModuleLabels;
    FParameterLabels    : TParameterLabels;

    procedure   SetLocation( aValue : TLocationType);
  public
    constructor Create( aPatch : TG2FilePatch);
    constructor CopyModules( aPatch : TG2FilePatch; aSrcePatchPart : TG2FilePatchPart; aModuleList : TModuleList);
    destructor  Destroy; override;
    procedure   Init;
    procedure   DeleteModule( aModuleIndex : integer);
    procedure   AddCable( aCable : TG2FileCable);
    procedure   DeleteCable( aCable : TG2FileCable);
    function    GetMaxModuleIndex : integer;
    function    GetNoOffModuleType( aModuleType : byte): integer;
    function    FindModuleLabel( aModuleIndex : byte): AnsiString;
    function    FindParamValue( aModuleIndex, aVariation, aParamIndex : byte): integer;
    function    FindParamSet( aModuleIndex: byte): TParamSet;
    function    FindModule( aModuleIndex : byte): TG2FileModule;
    function    GetParameterLabel( aModuleIndex, aParamIndex : byte): AnsiString;
    procedure   SetParameterLabel( aModuleIndex, aParamIndex : byte; aValue : AnsiString);
    function    GetModuleLabel( aModuleIndex : byte): AnsiString;
    procedure   SetModuleLabel( aModuleIndex : byte; aValue : AnsiString);
    procedure   AddNewVariation;
    procedure   CopyVariation( aFromVariation, aToVariation : byte);
    function    FindParamLabel( aModuleIndex, aParamIndex : byte): AnsiString;
    procedure   AddParamLabel(aModuleIndex, aParamIndex : byte; aValue : AnsiString);
    procedure   AddNewModuleLabel( aModuleIndex : byte; aValue : AnsiString);
    procedure   SelectModule( aModule : TG2FileModule);
    procedure   DeselectModule( aModule : TG2FileModule);
    function    CreateModule( aModuleIndex : byte; aModuleType : byte): TG2FileModule;
    function    CreateCable( aColor : byte; aFromModule : byte; aFromConnector : byte; aLinkType : byte; aToModule : byte; aToConnector : byte): TG2FileCable; virtual;

    property    Patch : TG2FilePatch read FPatch write FPatch;
    property    Location : TLocationType read FLocation write SetLocation;
    property    ModuleList : TModuleList read FModuleList;
    property    CableList : TCableList read FCableList;
    property    ParameterList : TModuleParameters read FParameterList;
    property    ModuleLabels : TModuleLabels read FModuleLabels;
    property    ParameterLabels : TParameterLabels read FParameterLabels;
  end;

  TG2FilePatch = class( TG2FileDataStream)
  private
    FPatchPart          : array[0..1] of TG2FilePatchPart;
    FSelectedModuleList : TModuleList;
    FPatchSettings      : TPatchSettings;
    FPatchDescription   : TPatchDescription;
    FMorphParameters    : TMorphParameters;
    FKnobList           : TKnobList;
    FControllerList     : TControllerList;
    FMorphNames         : TMorphNames;
    FCurrentNote        : TCurrentNote;
    FPatchNotes         : TPatchNotes;
    // Not found it in the file so far
    FSelectedMorphIndex : integer;
    FSelectedParam      : TG2FileParameter;

    FEditAllVariations  : boolean;

    FG2               : TG2File;
    FPerformance      : TG2FilePerformance;
    FSlot             : TG2FileSlot;

    FParams           : array of TG2FileParameter;

    procedure   SetG2( aValue : TG2File);
    procedure   SetPerformance( aValue : TG2FilePerformance);
    procedure   SetSlot( aValue : TG2FileSlot);
    function    GetPatchPart( aIndex : integer): TG2FilePatchPart;
    function    GetModuleList( aIndex : integer): TModuleList;
    function    GetSelectedModuleList : TModuleList;
    function    GetCableList( aIndex : integer): TCableList;
    function    GetParameterList( aIndex : integer): TModuleParameters;
    function    GetModuleLabels( aIndex : integer): TModuleLabels;
    function    GetParameterLabels( aIndex : integer): TParameterLabels;
    function    GetPatchName : AnsiString;
    procedure   SetPatchName( aValue : AnsiString);
    procedure   SetSelectedParam( aValue : TG2FileParameter); virtual;
    function    GetVariation : byte;
    procedure   SetVariation( aValue : byte);
  protected
    procedure   SetSelectedMorphIndex( aValue : integer); virtual;
  public

    constructor Create( AOwner: TComponent); override;
    constructor CopyModules( AOwner: TComponent; aPatch : TG2FilePatch; aModuleList : TModuleList);
    destructor  Destroy; override;

    procedure   Init; virtual;
    function    ReadChunk( aChunk : TPatchChunk): boolean;
    procedure   Read( aChunk : TPatchChunk); override;
    procedure   Write( aChunk : TPatchChunk; aVariationCount : byte); override;

    function    GetParameterCount: integer;
    function    GetModule( LocationIndex, ModuleIndex: integer): TG2FileModule; //virtual;
    function    GetModuleCount( LocationIndex : integer): integer;
    function    GetParameter( ModuleIndex, ParamIndex : integer): TG2FileParameter;

    procedure   InitParameters;
    procedure   InitNames;
    procedure   InitKnobs;

    function    GetMaxModuleIndex( aLocation : TLocationType) : integer;
    function    GetNoOffModuleType( aLocation : TLocationType; aModuleType : byte) : integer;
    function    GetActiveVariation : byte;

    function    GetModuleName( aLocation : TLocationType; aModuleIndex : byte): AnsiString;

    function    GetParameterValue( aLocation : TLocationType; aModuleIndex : byte; aParamIndex : byte; aVariation : byte): byte;
    procedure   SetParameterValue( aLocation : TLocationType; aModuleIndex : byte; aParamIndex : byte; aVariation : byte; aValue: byte); virtual;
    procedure   InitParameterValue( aLocation : TLocationType; aModuleIndex, aParamIndex, aVariation, aValue : byte); virtual;
    procedure   InitModeValue( aLocation : TLocationType; aModuleIndex, aParamIndex, aValue : byte); virtual;
    function    GetModeValue( aLocation : TLocationType; aModuleIndex : byte; aParamIndex : byte): byte;
    procedure   SetModeValue( aLocation : TLocationType; aModuleIndex : byte; aParamIndex : byte; aValue: byte); virtual;
    procedure   SetMorphValue( aLocation : TLocationType; aModuleIndex : byte; aParamIndex : byte; aMorphIndex : byte; aValue: byte; aVariation : byte); virtual;

    procedure   SaveToFile( aStream : TStream);
    procedure   SaveAsFXP( aStream : TStream);
    function    LoadFromFile( aStream : TStream; aLogLines : TStrings): Boolean;
    class function LoadFileStream( AOwner: TComponent; aStream : TStream; aChunk : TPatchChunk): TG2FileDataStream; override;

    function    CreateModule( aLocation : TLocationType; aModuleIndex : byte; aModuleType : byte): TG2FileModule; virtual;
    function    CreateCable( aLocation : TLocationType; aColor : byte; aFromModule : byte; aFromConnector : byte; aLinkType : byte; aToModule : byte; aToConnector : byte): TG2FileCable; virtual;
    function    CreateParameter( aModuleIndex : byte): TG2FileParameter; virtual;

    procedure   AddModuleToPatch( aLocation : TLocationType; aModule: TG2FileModule);
    procedure   DeleteModuleFromPatch( aLocation : TLocationType; aModule: TG2FileModule);
    procedure   SelectModule( aModule : TG2FileModule);
    procedure   DeselectModule( aModule : TG2FileModule);

    procedure   AddCableToPatch( aLocation : TLocationType; aCable: TG2FileCable);
    procedure   DeleteCableFromPatch( aLocation : TLocationType; aCable: TG2FileCable); virtual;

    function    FindKnob(aLocation : TLocationType; aModuleIndex, aParamIndex : byte): integer;
    function    FindMidiCC(aLocation : TLocationType; aModuleIndex, aParamIndex : byte): byte;

    function    AssignKnobInPatch( aKnobIndex : integer; aLocation : TLocationType; aModuleIndex, aParamIndex : byte): TKnob;
    procedure   DeassignKnobInPatch( aKnobIndex : integer);
    function    GetKnob(KnobIndex: integer): TKnob;

    function    GetMidiCC( aMidiCC : byte): TController;
    function    AssignMidiCCInPatchX(aMidiCC: byte; aLocation : TLocationType; aModuleIndex, aParamIndex : byte): TController;
    procedure   DeassignMidiCCInPatchX(aMidiCC: byte);

    function    GetMorph( aLocation : TLocationType; aModuleIndex, aParamIndex, aMorphIndex, aVariation : byte): TMorphParameter;
    function    HasMorph( aLocation : TLocationType; aModuleIndex, aParamIndex, aVariation : byte): boolean;

    procedure   AddVariation;
    procedure   CopyVariation( aFromVariation, aToVariation : byte);

    function    GetParameterLabel( aLocation : TLocationType; aModuleIndex, aParamIndex : byte): AnsiString;
    procedure   SetParameterLabel( aLocation : TLocationType; aModuleIndex, aParamIndex : byte; aValue : AnsiString);
    function    GetModuleLabel( aLocation : TLocationType; aModuleIndex : byte): AnsiString;
    procedure   SetModuleLabel( aLocation : TLocationType; aModuleIndex : byte; aValue : AnsiString);

    procedure   UnselectModules( aLocation : TLocationType);
    procedure   SelectModuleAbove;
    procedure   SelectModuleUnder;
    procedure   SelectModuleLeft;
    procedure   SelectModuleRight;
    procedure   SelectNextModuleParam;
    procedure   SelectPrevModuleParam;

    function    MessAddModule( aLocation : TLocationType; aModuleType, aCol, aRow: byte): boolean; virtual;
    function    MessCopyModules( aSrcePatch : TG2FilePatchPart; aFromLocation, aToLocation : TLocationType): boolean; virtual;
    // Abstract functions
    function    MessAddConnection( aLocation : TLocationType; aFromConnection, aToConnection : TG2FileConnector): boolean; virtual;
    function    MessDeleteConnection( aLocation : TLocationType; aCable : TG2FileCable): boolean; virtual;
    function    MessDeleteModule( aLocation : TLocationType; aModuleIndex : byte): boolean; virtual;
    function    MessDeleteModules( aLocation : TLocationType): boolean; virtual;
    function    MessMoveModule( aLocation : TLocationType; aModuleIndex, aCol, aRow : byte): boolean; virtual;
    function    MessSetModuleColor( aLocation: TLocationType; aModuleIndex, aColor : byte): boolean; virtual;
    function    MessSetModuleParamLabels( aLocation: TLocationType; aModuleIndex, aParamIndex: byte; aName: AnsiString): boolean; virtual;
    function    MessSetModuleLabel( aLocation : TLocationType; aModuleIndex : byte; aName : AnsiString): boolean; virtual;

    procedure   SortLeds; virtual;

    property    PatchName : AnsiString read GetPatchName write SetPatchName;
    property    ActiveVariation : byte read GetActiveVariation; // TODO
    property    Variation : byte read GetVariation write SetVariation;
    property    SelectedMorphIndex : integer read FSelectedMorphIndex write SetSelectedMorphIndex;

    property    Modules[ LocationIndex, ModuleIndex : integer]: TG2FileModule read GetModule;
    property    ModuleCount[ LocationIndex : integer]: integer read GetModuleCount;
    property    Parameter[ ModuleIndex, ParamIndex : integer]: TG2FileParameter read GetParameter;
    property    ParameterCount : integer read GetParameterCount;
    property    EditAllVariations : boolean read FEditAllVariations write FEditAllVariations;

    property    PatchPart[ Index : integer]: TG2FilePatchPart read GetPatchPart;
    property    ModuleList[ Index : integer]: TModuleList read GetModuleList;
    property    CableList[ Index : integer]: TCableList read GetCableList;
    property    ParameterList[ Index : integer]: TModuleParameters read GetParameterList;
    property    ModuleLabels[ Index : integer]: TModuleLabels read GetModuleLabels;
    property    ParameterLabels[ Index : integer]: TParameterLabels read GetParameterLabels;
    property    PatchSettings : TPatchSettings read FPatchSettings;
    property    KnobList : TKnobList read FKnobList;
    property    PatchDescription : TPatchDescription read FPatchDescription;
    property    SelectedModuleList : TModuleList read GetSelectedModuleList;
    property    SelectedParam : TG2FileParameter read FSelectedParam write SetSelectedParam;

    property    G2 : TG2File read FG2 write SetG2;
    property    Performance : TG2FilePerformance read FPerformance write SetPerformance;
    property    Slot : TG2FileSlot read FSlot write SetSlot;
  end;

  // Interface class between file and application

  TG2FileConnector = class
  private
    FPatch             : TG2FilePatch;
    FLocation          : TLocationType;
    FModule            : TG2FileModule;
    FConnectorKind     : TConnectorKind;
    FConnectorType     : TConnectorType;
    FBandWidth         : TBandWidthType;
    FConnectorDefColor : Byte;
    FConnectorIndex    : Byte;
    FName              : AnsiString;

    FCables            : TObjectList;
    FControl           : TComponent;

    procedure   SetConnectorKind( aValue : TConnectorKind);
    procedure   SetConnectorDefColor( aValue : Byte);
    function    GetConnectorColor : byte;
    function    GetNewConnectorColor: byte;
    function    GetCable( aIndex : integer) : TG2FileCable;
    function    GetCableCount : integer;
    function    GetDefRate: TBits1;
    function    GetRate: TBits1;
    function    GetNewRate: TBits1;
  public
    constructor Create( aPatch : TG2FilePatch; aLocation : TLocationType; aModule : TG2FileModule);
    destructor  Destroy; override;
    procedure   InitConnector( aConnectorIndex : Byte; aConnectorKind : TConnectorKind; ConnectorDef : TXMLConnectorType);
    procedure   AddCable( aCable : TG2FileCable);
    procedure   DelCable( aCable : TG2FileCable);
    procedure   InvalidateCables;
    procedure   CalcDefColor;

    property    Module : TG2FileModule read FModule;
    property    ConnectorKind : TConnectorKind read FConnectorKind write SetConnectorKind;
    property    ConnectorColor : byte read GetConnectorColor;
    property    ConnectorDefColor : byte read FConnectorDefColor write SetConnectorDefColor;
    property    ConnectorIndex : byte read FConnectorIndex write FConnectorIndex;
    property    ConnectorType : TConnectorType read FConnectorType write FConnectorType;
    property    BandWidth : TBandWidthType read FBandWidth write FBandWidth;
    property    Name : AnsiString read FName write FName;
    property    GraphControl : TComponent read FControl write FControl;
    property    Cables[ Index : integer]: TG2FileCable read GetCable;
    property    CableCount : integer read GetCableCount;
    property    NewConnectorColor : Byte read GetNewConnectorColor;
    property    DefRate : TBits1 read GetDefRate;
    property    Rate : TBits1 read GetRate;
    property    NewRate : TBits1 read GetNewRate;
  end;

  // Interface class between file and application

  TG2FileParameter = class
  private
    FPatch             : TG2FilePatch;
    FKnob              : TKnob;
    FGlobalKnob        : TGlobalKnob;
    FController        : TController;
    FLocation          : TLocationType;
    FModuleIndex       : integer;
    FParamType         : TParamType;
    FParamIndex        : integer;
    FLowValue          : byte;
    FHighValue         : byte;
    FDefaultValue      : byte;
    FParamName         : AnsiString;
    FModuleName        : AnsiString;
    FDefaultParamLabel : AnsiString;
    FCanChangeLabel    : boolean;
    FDefaultKnob       : integer; // When a module is assigned to a parameter page
    FButtonParamIndex  : integer; // The parameter that is assigned to the button below the knob on the param page
    FButtonText        : TStrings;
  protected
    procedure   SetSelected( aValue : boolean); virtual;
    function    GetSelected : boolean;
    function    GetButtonText( Index : integer): string;
    procedure   SetButtonText( Index : integer; aValue : string);
    function    GetSelectedButtonText: string;
    function    GetButtonTextCount : integer;
    function    GetButtonParam : TG2FileParameter;
  public
    constructor Create( aPatch : TG2FilePatch; aLocation : TLocationType; aModuleIndex : integer);
    destructor  Destroy; override;
    procedure   InitParam( aModuleName : AnsiString; aParamIndex : byte; aParamType : TParamType; aParamName, aDefaultParamLabel : AnsiString; aLowValue, aHighValue, aDefaultValue : byte; aDefaultKnob, aButtonParamIndex : integer; aButtonText : AnsiString);
    procedure   AssignKnob( aKnobIndex : integer);
    procedure   DeassignKnob( aKnobIndex : integer);
    procedure   AssignGlobalKnob( aPerf : TG2FilePerformance; aSlotIndex : byte; aKnobIndex : integer);
    procedure   DeassignGlobalKnob(  aPerf : TG2FilePerformance; aKnobIndex: integer);
    procedure   AssignController( aMidiCC : byte);
    procedure   DeassignController;
    procedure   SetParameterValue( Value : byte); virtual;
    function    GetParameterValue: byte; virtual;
    procedure   IncValue;
    procedure   DecValue;
    procedure   IncMorphValue;
    procedure   DecMorphValue;
    function    GetParamLabel: AnsiString;
    procedure   SetParamLabel( aValue : AnsiString);
    function    GetSelectedMorph : TMorphParameter;
    function    GetMorph( aMorphIndex, aVariation : integer) : TMorphParameter;
    function    HasMorph: boolean;
    function    GetSelectedMorphValue : byte;
    function    GetMorphValue( aMorphIndex, aVariation : integer) : byte;
    procedure   SetSelectedMorphValue( Value: byte);
    function    TextFunction( aTextFunction: integer; LineNo, TotalLines : integer): string;
    //FMX procedure   AssignControl( aControl : TGraphicControl); virtual; abstract;
    //FMX procedure   DeassignControl( aControl : TGraphicControl); virtual; abstract;
    procedure   InvalidateControl; virtual;

    property    ParamType : TParamType read FParamType write FParamType;
    property    ParamIndex : integer read FParamIndex write FParamIndex;
    property    LowValue : byte read FLowValue write FLowValue;
    property    HighValue : byte read FHighValue write FHighValue;
    property    DefaultValue : byte read FDefaultValue write FDefaultValue;
    property    Location : TLocationType read FLocation write FLocation;
    property    ModuleIndex : integer read FModuleIndex write FModuleIndex;
    property    ParamName : AnsiString read FParamName write FParamName;
    property    ModuleName : AnsiString read FModuleName write FModuleName;
    property    ParamLabel : AnsiString read GetParamLabel write SetParamLabel;
    property    CanChangeLabel : boolean read FCanChangeLabel write FCanChangeLabel;
    property    Selected : boolean read GetSelected write SetSelected;
    property    Knob : TKnob read FKnob write FKnob;
    property    GlobalKnob : TGlobalKnob read FGlobalKnob write FGlobalKnob;
    property    Controller : TController read FController write FController;
    property    ButtonParamIndex : integer read FButtonParamIndex write FButtonParamIndex;
    property    DefaultKnob : integer read FDefaultKnob write FDefaultKnob;
    property    ButtonText[ index : integer]: string read GetButtonText write SetButtonText;
    property    ButtonTextCount : integer read GetButtonTextCount;
    property    SelectedButtonText: string read GetSelectedButtonText;
    property    ButtonParam : TG2FileParameter read GetButtonParam;
  end;

  TG2FileSlot = class( TComponent)
  private
    FUnknown           : array[0..4] of TBits8;
    FEnabled           : TBits8;
    FKeyboard          : TBits8;
    FHold              : TBits8;
    FKeyboardRangeFrom : TBits8;
    FKeyboardRangeTo   : TBits8;
    FSlot              : TBits8;

    FPatchName         : AnsiString;

    FPatch             : TG2FilePatch;
    FPerformance       : TG2FilePerformance;
    FG2                : TG2File;

    function    GetSlotIndex: byte;
    procedure   SetSlotIndex( aSlotIndex : byte);
    procedure   SetEnabled( aValue : TBits8);
    procedure   SetHold( aValue : TBits8);
    procedure   SetKeyboard( aValue : TBits8);
    procedure   SetKeyboardRangeTo( aValue : TBits8);
    procedure   SetKeyboardRangeFrom( aValue : TBits8);
    procedure   SetG2( aValue : TG2File);

    procedure   SetPerformance( aValue : TG2FilePerformance);
    procedure   SetPatch( aValue : TG2FilePatch);
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    function    CreatePatch : TG2FilePatch; virtual;
    procedure   Init; virtual;
    procedure   Read( aChunk : TPatchChunk);
    procedure   Write( aChunk : TPatchChunk);
    function    GetPatch: TG2FilePatch;

    property    SlotIndex : byte read GetSlotIndex write SetSlotIndex;
    property    PatchName : AnsiString read FPatchname write FPatchname;
    property    Enabled : TBits8 read FEnabled write SetEnabled;
    property    Hold : TBits8 read FHold write SetHold;
    property    Keyboard : TBits8 read FKeyboard write SetKeyboard;
    property    Upper : TBits8 read FKeyboardRangeTo write SetKeyboardRangeTo;
    property    Lower : TBits8 read FKeyboardRangeFrom write SetKeyboardRangeFrom;
    property    Patch : TG2FilePatch read FPatch write SetPatch;
    property    Performance : TG2FilePerformance read FPerformance write SetPerformance;
    property    G2 : TG2File read FG2 write SetG2;
  end;

  TGlobalKnob = class( TKnob)
  private
    FSlotIndex  : TBits2;
  public
    procedure   Init; override;
    procedure   Read( aChunk : TPatchChunk); override;
    procedure   Write( aChunk : TPatchChunk); override;
    property    SlotIndex : TBits2 read FSlotIndex write FSlotIndex;
  end;

  TGlobalKnobList = class( TObjectList)
  private
    FKnobCount  : TBits16;
    function    GetGlobalKnob( aIndex : integer) : TGlobalKnob;
    procedure   SetGlobalKnob( aIndex : integer; const aValue : TGlobalKnob);
  public
    constructor Create( AOwnsObjects : boolean);
    destructor  Destroy; override;
    procedure   Init;
    function    FindGlobalKnobIndex( aSlotIndex : byte; aLocation : TLocationType; aModuleIndex, aParamIndex : byte): integer;
    procedure   DeleteModule( aSlotIndex, aLocation, aModuleIndex : Byte);
    procedure   Read( aChunk : TPatchChunk);
    procedure   Write( aChunk : TPatchChunk);
    function    AddGlobalKnob( aValue : TGlobalKnob): integer;
    property    Items[ aIndex : integer]: TGlobalKnob read GetGlobalKnob write SetGlobalKnob; default;
  end;

  TG2FilePerformance = class( TG2FileDataStream)
  private
    FPerformanceName      : AnsiString;

    FUnknown              : array[0..8] of TBits8;
    FSelectedSlot         : TBits2;
    FKeyboardRangeEnabled : TBits8;
    FMasterClock          : TBits8;
    FMasterClockRun       : TBits8;

    FG2                   : TG2File;
    FSlot                 : array[0..NSLOTS-1] of TG2FileSlot;

    FGlobalKnobList       : TGlobalKnobList;

    procedure   SetG2( aValue : TG2File);
    procedure   SetPerformanceName( aValue : AnsiString);
    procedure   SetMasterClock( aValue : TBits8);
    procedure   SetMasterClockRun( aValue : TBits8);
    procedure   SetKeyboardRangeEnabled( aValue : TBits8);
  protected
    function    GetSlot( aSlot : byte): TG2FileSlot;
    procedure   SetSelectedSlotIndex( aValue : TBits2); virtual;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    function    CreateSlot : TG2FileSlot; virtual;
    procedure   Init;
    procedure   InitKnobs;
    procedure   WriteSettings( aChunk : TPatchChunk);
    procedure   Read( aChunk : TPatchChunk); override;
    procedure   Write( aChunk : TPatchChunk; aVariationCount : byte); override;
    procedure   SaveToFile( aStream : TStream);
    procedure   SaveAsFXB( aStream : TStream);
    function    LoadFromFile( aStream : TStream; aLogLines : TStrings): Boolean;
    function    LoadFromFXB( aStream : TStream): Boolean;
    class function LoadFileStream( AOwner: TComponent; aStream : TStream; aChunk : TPatchChunk): TG2FileDataStream; override;

    procedure   DeleteModuleFromPerf( aSlotIndex : Byte; aLocation : TLocationType; aModule: TG2FileModule);
    function    AssignGlobalKnobInPerf( aKnobIndex : integer; aSlotIndex : byte; aLocation : TLocationType; aModuleIndex, aParamIndex : byte): TGlobalKnob;
    procedure   DeassignGlobalKnobInPerf( aKnobIndex : integer);
    function    GetGlobalKnob(KnobIndex: integer): TGlobalKnob;

    property    PerformanceName : AnsiString read FPerformanceName write SetPerformanceName;
    property    SelectedSlot : TBits2 read FSelectedSlot write SetSelectedSlotIndex;
    property    MasterClock : TBits8 read FMasterClock write SetMasterClock;
    property    MasterClockRun : TBits8 read FMasterClockRun write SetMasterClockRun;
    property    KeyboardRangeEnabled : TBits8 read FKeyboardRangeEnabled write SetKeyboardRangeEnabled;
    property    GlobalKnobList : TGlobalKnobList read FGlobalKnobList;
    property    G2 : TG2File read FG2 write SetG2;
    property    Slot[ Index : byte]: TG2FileSlot read GetSlot;
  end;

  TBankItem = class
    Cmd       : byte;
    Mode      : byte;
    Bank      : byte;
    Patch     : byte;
    Category  : byte;
    PatchName : AnsiString;
  end;

  TBankList = class( TObjectList)
  private
    function    GetBankItem( aIndex : integer) : TBankItem;
    procedure   SetBankItem( aIndex : integer; const aValue : TBankItem);
  public
    constructor Create( AOwnsObjects : boolean);
    destructor  Destroy; override;
    function    AddBankItem( aValue : TBankItem): integer;
    property    Items[ aIndex : integer]: TBankItem read GetBankItem write SetBankItem; default;
  end;

  TG2File = class( TComponent)
  private
    FClientType           : TClientType;

    FBankList             : TBankList;

    FOnCreateModule       : TCreateModuleEvent;
    FOnAddModule          : TAddModuleEvent;
    FOnDeleteModule       : TDeleteModuleEvent;
    FOnAssignKnob         : TAssignKnobEvent;
    FOnAssignGlobalKnob   : TAssignGlobalKnobEvent;
    FOnDeassignKnob       : TDeassignKnobEvent;
    FOnDeassignGlobalKnob : TDeassignGlobalKnobEvent;
    FOnParameterChange    : TParameterChangeEvent;
    FOnModuleModeChange   : TModuleModeChangeEvent;
    FOnSelectSlot         : TSelectSlotEvent;

    FPerformance   : TG2FilePerformance;

    FLogLevel    : integer;
    FLogFilePath : string;
    FLogLines    : TStringList;
    FLogLock     : TRTLCriticalSection; // for multi threaded access to log

    FLastError   : string;

    procedure SetPerformance( aValue : TG2FilePerformance);
    procedure SetLogLevel( aValue : integer);
  protected
    function  GetID : integer; virtual;
  public
    // TODO make these private
    FBanks         : TStringList;
    FXMLModuleDefs : TXMLDocument;
    FXMLParamDefs  : TXMLDocument;
    FModuleDefList : TXMLModuleDefListType;
    FParamDefList  : TXMLParamDefListType;

    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;

    function    CreatePerformance : TG2FilePerformance; virtual;

    procedure   LoadModuleDefs( Path : string);
    function    TextFunction( No : integer; Param1, Param2, Param3 : byte): string;

    procedure   add_log_line( tekst: string; log_cmd : integer);
    procedure   dump_buffer( var buffer; max_size : integer);
    procedure   save_log;
    procedure   AssignLog( Lines : TStrings);
    procedure   ClearLog;

    property    BankList : TBankList read FBankList write FBankList;
    property    Performance : TG2FilePerformance read FPerformance write SetPerformance;
  published
    property    ID : integer read GetID;
    property    ClientType : TClientType read FClientType write FClientType;
    property    LogLines : TStringList read FLogLines;
    property    LogLevel : integer read FLogLevel write SetLogLevel;
    property    LastError : string read FLastError write FLastError;
    property    OnCreateModule : TCreateModuleEvent read FOnCreateModule write FOnCreateModule;
    property    OnAddModule : TAddModuleEvent read FOnAddModule write FOnAddModule;
    property    OnDeleteModule : TDeleteModuleEvent read FOnDeleteModule write FOnDeleteModule;
    property    OnAssignKnob : TAssignKnobEvent read FOnAssignKnob write FOnAssignKnob;
    property    OnDeassignKnob : TDeassignKnobEvent read FOnDeassignKnob write FOnDeassignKnob;
    property    OnAssignGlobalKnob : TAssignGlobalKnobEvent read FOnAssignGlobalKnob write FOnAssignGlobalKnob;
    property    OnDeassignGlobalKnob : TDeassignGlobalKnobEvent read FOnDeassignGlobalKnob write FOnDeassignGlobalKnob;
    property    OnParameterChange : TParameterChangeEvent read FOnParameterChange write FOnParameterChange;
    property    OnModuleModeChange : TModuleModeChangeEvent read FOnModuleModeChange write FOnModuleModeChange;
    property    OnSelectSlot : TSelectSlotEvent read FOnSelectSlot write FOnSelectSlot;
  end;

implementation

////////////////////////////////////////////////////////////////////////////////
//  TG2FileModule
////////////////////////////////////////////////////////////////////////////////

constructor TG2FileModule.Create( aPatchPart : TG2FilePatchPart);
begin
  inherited Create( aPatchPart);

  FPatchPart := aPatchPart;

  SetLength(FModeInfo, 0);
  FSelectedParam := -1;
end;

function TG2FileModule.CreateCopy( aPatchPart : TG2FilePatchPart) : TG2FileModule;
begin
  Result := TG2FileModule.Create( aPatchPart);
  Result.Copy( self);
end;

procedure TG2FileModule.Copy( aModule : TG2FileModule);
var i : integer;
begin
  FTypeID       := aModule.FTypeID;
  FModuleIndex  := aModule.FModuleIndex;
  FModuleName   := aModule.FModuleName;
  FCol          := aModule.Col;
  FRow          := aModule.Row;
  FModuleColor  := aModule.ModuleColor;
  FUprate       := aModule.Uprate;
  FIsLed        := aModule.IsLed;
  FUnknown1     := aModule.FUnknown1;
  FModeCount    := aModule.ModeCount;

  SetLength(FModeInfo, FModeCount);
  for i := 0 to Length(FModeInfo) - 1 do
    FModeInfo[i] := aModule.FModeInfo[i];
end;

destructor TG2FileModule.Destroy;
var i : integer;
begin
  if assigned(FPatchPart) then begin
    FPatchPart.DeselectModule( self);

    FPatchPart.DeleteModule( FModuleIndex);
  end;

  for i := 0 to Length(FModes) - 1 do
    FModes[i].Free;
  Finalize(FModes);

  for i := 0 to Length(FParams) - 1 do
    FParams[i].Free;
  Finalize(FParams);

  for i := 0 to Length(FOutConnectors) - 1 do
    FOutConnectors[i].Free;
  Finalize( FOutConnectors);

  for i := 0 to Length(FInConnectors) - 1 do
    FInConnectors[i].Free;
  Finalize( FInConnectors);

  Finalize(FModeInfo);

  inherited;
end;

procedure TG2FileModule.Init;
begin
  TypeID       := 0;
  ModuleIndex  := 0;
  Row          := 0;
  Col          := 0;
  MOduleColor  := 0;
  FUnknown1    := 0;
  FModeCount   := 0;
  SetLength(FModeInfo, 0);

  ModuleName := '';
end;

function TG2FileModule.CreateParameter: TG2FileParameter;
begin
  Result := TG2FileParameter.Create( FPatchPart.FPatch, TLocationType(FLocation), FModuleIndex);
end;

function TG2FileModule.CreateConnector: TG2FileConnector;
begin
  Result := TG2FileConnector.Create( FPatchPart.FPatch, TLocationType(FLocation), self);
end;

procedure TG2FileModule.InitModule( aLocation : TLocationType; aModuleDef: TXMLModuleDefType; aParamDefList : TXMLParamDefListType);
var i : integer;
    aParamDef : TXMLParamDefType;
    aConnectorList : TXMLConnectorListType;
    aParamList : TXMLParamListType;
begin
  FLocation  := aLocation;
  FTypeID    := aModuleDef.ModuleType;
  FUprate    := aModuleDef.Uprate;
  FIsLed     := aModuleDef.IsLed;
  ModuleName := aModuleDef.ShortName;
  FModeCount := 0;

  aConnectorList := aModuleDef.Inputs;
  if assigned(aConnectorList) then
    try
      SetLength( FInConnectors, aConnectorList.Count);
      for i := 0 to aConnectorList.Count - 1 do begin
        FInConnectors[i] := CreateConnector;
        FInConnectors[i].InitConnector( i, ckInput, aConnectorList.Connector[i]);
      end;
    finally
      aConnectorList.Free;
    end;

  aConnectorList := aModuleDef.Outputs;
  if assigned(aConnectorList) then
    try
      SetLength( FOutConnectors, aModuleDef.Outputs.Count);
      for i := 0 to aConnectorList.Count - 1 do begin
        FOutConnectors[i] := CreateConnector;
        FOutConnectors[i].InitConnector( i, ckOutput, aConnectorList.Connector[i]);
      end;
    finally
      aConnectorList.Free;
    end;

  aParamList := aModuleDef.Params;
  if assigned(aParamList) then
    try
      SetLength( FParams, aParamList.Count);
      for i := 0 to aParamList.Count - 1 do begin
        FParams[i] := CreateParameter;
        aParamDef := aParamDefList.ParamDef[ aModuleDef.Params[i].Id];
        FParams[i].InitParam( aModuleDef.ShortName, i, ptParam, aModuleDef.Params[i].Name, aModuleDef.Params[i].ParamLabel,
                              aParamDef.LowValue, aParamDef.HighValue, aParamDef.DefaultValue,
                              aModuleDef.Params[i].DefaultKnob, aModuleDef.Params[i].ButtonParamIndex,
                              aParamDef.ButtonText);
      end;
    finally
      aParamList.Free;
    end;

  aParamList := aModuleDef.Modes;
  if assigned(aParamList) then
    try
      FModeCount  := aModuleDef.Modes.Count;
      SetLength( FModeInfo, FModeCount);
      SetLength( FModes, aParamList.Count);
      for i := 0 to aParamList.Count - 1 do begin
        FModes[i] := CreateParameter;
        aParamDef := aParamDefList.ParamDef[ aModuleDef.Modes[i].Id];
        FModes[i].InitParam( aModuleDef.ShortName, i, ptMode, aModuleDef.Modes[i].Name, aModuleDef.Modes[i].ParamLabel,
                             aParamDef.LowValue, aParamDef.HighValue, aParamDef.DefaultValue,
                             -1, -1,
                              aParamDef.ButtonText);
        FModeInfo[i] := aParamDef.DefaultValue;
      end;
    finally
      aParamList.Free;
    end;
end;

procedure TG2FileModule.InvalidateCables;
var i : integer;
begin
  for i := 0 to Length(FInConnectors) - 1 do
    FInConnectors[i].InvalidateCables;

  for i := 0 to Length(FOutConnectors) - 1 do
    FOutConnectors[i].InvalidateCables;
end;

function TG2FileModule.GetInputConnector( ConnectorIndex: integer): TG2FileConnector;
var i : integer;
begin
  i := 0;
  while ( i < Length(FInConnectors)) and ( ConnectorIndex <> FInConnectors[i].ConnectorIndex) do
    inc(i);

  if ( i < Length(FInConnectors)) then
    Result := FInConnectors[i]
  else
    raise Exception.Create('Input connector with index ' + IntToStr(ConnectorIndex) + ' not found.');
end;

function TG2FileModule.GetInputConnectorCount: integer;
begin
  Result := Length(FInConnectors);
end;

function TG2FileModule.GetOutputConnector( ConnectorIndex: integer): TG2FileConnector;
var i : integer;
begin
  i := 0;
  while ( i < Length(FOutConnectors)) and ( ConnectorIndex <> FOutConnectors[i].ConnectorIndex) do
    inc(i);

  if ( i < Length(FOutConnectors)) then
    Result := FOutConnectors[i]
  else
    raise Exception.Create('Output connector with index ' + IntToStr(ConnectorIndex) + ' not found.');
end;

function TG2FileModule.GetOutputConnectorCount: integer;
begin
  Result := Length(FOutConnectors);
end;

function TG2FileModule.GetMode( ModeIndex: integer): TG2FileParameter;
var i : integer;
begin
  i := 0;
  while ( i < Length(FModes)) and ( ModeIndex <> FModes[i].ParamIndex) do
    inc(i);

  if ( i < Length(FModes)) then
    Result := FModes[i]
  else
    //raise Exception.Create('Mode parameter with index ' + IntToStr(ModeIndex) + ' not found.');
    Result := nil;
end;

function TG2FileModule.GetParameter( ParamIndex: integer): TG2FileParameter;
var i : integer;
begin
  i := 0;
  while ( i < Length(FParams)) and ( ParamIndex <> FParams[i].ParamIndex) do
    inc(i);

  if ( i < Length(FParams)) then
    Result := FParams[i]
  else
    //raise Exception.Create('Parameter with index ' + IntToStr(ParamIndex) + ' not found.');
    Result := nil;
end;

function TG2FileModule.GetModeCount: integer;
begin
  Result := Length( FModeInfo);
end;

function TG2FileModule.GetParameterCount: integer;
begin
  Result := Length( FParams);
end;

procedure TG2FileModule.SetModeInfo( aModeIndex : integer; aValue : TBits6);
begin
  if aModeIndex < Length(FModeInfo) then
    FModeInfo[aModeIndex] := aValue
  else
    raise Exception.Create('Set ModeInfo, index ' + IntToStr(aModeIndex) + ' out of range.');
end;

function TG2FileModule.GetModeInfo( aModeIndex : integer): TBits6;
begin
  if aModeIndex < Length(FModeInfo) then
    Result := FModeInfo[aModeIndex]
  else
    raise Exception.Create('Set ModeInfo, index ' + IntToStr(aModeIndex) + ' out of range.');
end;

function TG2FileModule.GetNewCol : TBits7;
begin
  Result := 0;
end;

function TG2FileModule.GetNewRow : TBits7;
begin
  Result := 0;
end;

function TG2FileModule.GetSelectedParam : TG2FileParameter;
begin
  if FSelectedParam = -1 then
    Result := nil
  else
    Result := FParams[ FSelectedParam];
end;

procedure TG2FileModule.SetSelectedParam( aValue : TG2FileParameter);
var i : integer;
begin
  if Length(FPArams) = 0 then
    exit;

  i := 0;
  while (i < Length(FParams)) and (FParams[i] <> aValue) do
    inc(i);

  if (i < Length(FParams)) then
    FSelectedParam := i;
end;

function TG2FileModule.GetNextParam : TG2FileParameter;
begin
  Result := nil;
  if Length( FParams) = 0 then
    exit;

  inc(FSelectedParam);
  if FSelectedParam >= Length(FParams) then
     FSelectedParam := 0;

  Result := FParams[FSelectedParam];
end;

function TG2FileModule.GetPrevParam : TG2FileParameter;
begin
  Result := nil;
  if Length( FParams) = 0 then
    exit;

  dec(FSelectedParam);
  if FSelectedParam < 0  then
     FSelectedParam := Length(FParams) - 1;

  Result := FParams[FSelectedParam];
end;

procedure TG2FileModule.SelectNextParam;
var Param : TG2FileParameter;
begin
  if FSelectedParam <> -1 then
    FParams[FSelectedParam].Selected := False;
  Param := GetNextParam;
  if assigned(Param) then
    Param.Selected := True
  else
    FSelectedParam := -1;
end;

procedure TG2FileModule.SelectPrevParam;
var Param : TG2FileParameter;
begin
  if FSelectedParam <> -1 then
    FParams[FSelectedParam].Selected := False;
  Param := GetPrevParam;
  if assigned(Param) then
    Param.Selected := True
  else
    FSelectedParam := -1;
end;

procedure TG2FileModule.Read( aChunk : TPatchChunk);
var i : integer;
begin
  // FModuleIndex : TBits8;
  // FRow         : TBits7;
  // FCol         : TBits7;
  // FColor       : TBits8;
  // FUnknown1    : TBits8;
  // FUnknown2    : TBits4; => Seems to be the ModeCount
  // FModeInfo    : array of TBits6;

  //FTypeID      := Chunk.ReadBits(aStream, 8);
  //ModuleIndex  := aChunk.ReadBits( 8);
  Col          := aChunk.ReadBits( 7);
  Row          := aChunk.ReadBits( 7);
  ModuleColor  := aChunk.ReadBits( 8);
  Uprate       := aChunk.ReadBits( 1);
  IsLed        := aChunk.ReadBits( 1);
  FUnknown1    := aChunk.ReadBits( 6);
  FModeCount   := aChunk.ReadBits( 4);

  if assigned(aChunk.FLogLines) then
    aChunk.FLogLines.Add(Format('%3d %3d %3d %3d %3d %3d %3d %3d %3d', [TypeID, ModuleIndex, Row, Col, ModuleColor, Uprate, IsLed, FUnknown1, FModeCount]));

  SetLength(FModeInfo, FModeCount);
  for i := 0 to Length(FModeInfo) - 1 do
    FModeInfo[i] := aChunk.ReadBits( 6);
end;

procedure TG2FileModule.Write( aChunk : TPatchChunk);
var i : integer;
begin
  aChunk.WriteBits(TypeID, 8);
  aChunk.WriteBits(ModuleIndex, 8);
  aChunk.WriteBits(Col, 7);
  aChunk.WriteBits(Row, 7);
  aChunk.WriteBits(ModuleColor, 8);
  aChunk.WriteBits(Uprate, 1);
  aChunk.WriteBits(IsLed, 1);
  aChunk.WriteBits(FUnknown1, 6);
  aChunk.WriteBits(FModeCount, 4);

  for i := 0 to Length(FModeInfo) - 1 do
    aChunk.WriteBits(FModeInfo[i], 6);
end;


procedure TG2FileModule.SetCol(Value: TBits7);
begin
  FCol := Value;
end;

procedure TG2FileModule.SetRow(Value: TBits7);
begin
  FRow := Value;
end;

procedure TG2FileModule.SetModuleColor(Value: TBits8);
begin
  FModuleColor := Value;
 //FMX Invalidate;
end;

procedure TG2FileModule.SetUprate(Value: TBits1);
begin
  FUprate := Value;
//FMX  Invalidate;
end;

procedure TG2FileModule.SetSelected( aValue: Boolean);
begin
  FSelected := aValue;
  // The patch keeps a list of selected modules
  if assigned(FPatchPart) then
    if aValue then
      FPatchPart.SelectModule( self)
    else
      FPatchPart.DeselectModule( self);
end;

procedure TG2FileModule.SetModuleName( aValue: AnsiString);
begin
  FModuleName := aValue;
//FMX  Caption := FModuleName;
//  Invalidate;
end;

function TG2FileModule.GetAssignableKnobCount: integer;
var i : integer;
begin
  Result := 0;
  for i := 0 to Length(FParams) - 1 do
    if FParams[i].FDefaultKnob >= 0 then
      inc(Result);
end;

function TG2FileModule.GetColor: integer;
begin
  if FModuleColor <= 24 then
    Result := ModuleColors[ FModuleColor]
  else
    Result := ModuleColors[ 0];
end;

////////////////////////////////////////////////////////////////////////////////
//  TG2ModuleList
////////////////////////////////////////////////////////////////////////////////

constructor TModuleList.Create( AOwnsObjects : boolean; aPatchPart : TG2FilePatchPart);
begin
  inherited Create( AOwnsObjects);
  FPatchPart := aPatchPart;
end;

constructor TModuleList.CopySelected( AOwnsObjects : boolean; aPatchPart : TG2FilePatchPart; aModuleList : TModuleList);
var Module : TG2FileModule;
    i : integer;
begin
  inherited Create( AOwnsObjects);

  FPatchPart := aPatchPart;
  FModuleCount := aModuleList.GetSelectedCount;

  for i := 0 to aModuleList.Count - 1 do begin
    Module := aModuleList.Items[i];
    if assigned(Module) {and (Module.FSelected)} then begin
      Add(Module.CreateCopy( aPatchPart));
    end;
  end;
end;

function TModuleList.GetModule( aIndex : integer) : TG2FileModule;
begin
  result := inherited Items[aindex] as TG2FileModule;
end;

procedure TModuleList.SetModule( aIndex : integer; const aValue : TG2FileModule);
begin
  inherited Items[aindex] := aValue;
end;

function TModuleList.AddModule( aValue : TG2FileModule): integer;
begin
  Result := inherited Add( aValue);
end;

function TModuleList.GetSelectedCount: integer;
var i : integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    if Items[i].FSelected then
      inc(Result);
end;

destructor TModuleList.Destroy;
begin
  inherited;
end;

procedure TModuleList.Init;
begin
  Clear;

  FLocation       := 0;
  FModuleCount    := 0;
end;

function TModuleList.FindModule( aModuleIndex : integer) : TG2FileModule;
var i : integer;
begin
  i := 0;
  while (i < Count) and (Items[i].ModuleIndex <> aModuleIndex) do
    inc(i);

  if (i < Count) then
    Result := Items[i]
  else
    Result := nil;
end;

procedure TModuleList.DeleteModule( aModuleIndex : byte);
var i : integer;
begin
  i := 0;
  while (i < Count) and (Items[i].ModuleIndex <> aModuleIndex) do
    inc(i);

  if (i < Count) then begin
     Delete(i);
  end;
end;

function TModuleList.GetModuleAbove( aModuleIndex : byte) : TG2FileModule;
var Module : TG2FileModule;
    i : integer;
    dx, dy, ds : single;
begin
  Module := FindModule(aModuleIndex);
  Result := Module;
  if not(assigned(Result)) then
    exit;

  ds := 0;
  for i := 0 to Count - 1 do begin
    if Items[ i].ParameterCount > 0 then begin
      if Items[ i].Row < Module.Row then begin
        dx := Items[i].Col * UNITS_COL - Module.Col * UNITS_COL;
        dy := Items[i].Row * UNITS_ROW - Module.Row * UNITS_ROW;
        if (ds = 0) or (sqrt(dx*dx+dy*dy) < ds) then begin
          Result := Items[i];
          ds := sqrt(dx*dx+dy*dy);
        end;
      end;
    end;
  end;
end;

function TModuleList.GetModuleUnder( aModuleIndex : byte) : TG2FileModule;
var Module : TG2FileModule;
    i : integer;
    dx, dy, ds : single;
begin
  Module := FindModule(aModuleIndex);
  Result := Module;
  if not(assigned(Result)) then
    exit;

  ds := 0;
  for i := 0 to Count - 1 do begin
    if Items[ i].ParameterCount > 0 then begin
      if Items[ i].Row > Module.Row then begin
        dx := Items[i].Col * UNITS_COL - Module.Col * UNITS_COL;
        dy := Items[i].Row * UNITS_ROW - Module.Row * UNITS_ROW;
        if (ds = 0) or (sqrt(dx*dx+dy*dy) < ds) then begin
          Result := Items[i];
          ds := sqrt(dx*dx+dy*dy);
        end;
      end;
    end;
  end;
end;

function TModuleList.GetModuleLeft( aModuleIndex : byte) : TG2FileModule;
var Module : TG2FileModule;
    i : integer;
    dx, dy, ds : single;
begin
  Module := FindModule(aModuleIndex);
  Result := Module;
  if not(assigned(Result)) then
    exit;

  ds := 0;
  for i := 0 to Count - 1 do begin
    if Items[ i].ParameterCount > 0 then begin
      if Items[ i].Col < Module.Col then begin
        dx := Items[i].Col * UNITS_COL - Module.Col * UNITS_COL;
        dy := Items[i].Row * UNITS_ROW - Module.Row * UNITS_ROW;
        if (ds = 0) or (sqrt(dx*dx+dy*dy) < ds) then begin
          Result := Items[i];
          ds := sqrt(dx*dx+dy*dy);
        end;
      end;
    end;
  end;
end;

function TModuleList.GetModuleRight( aModuleIndex : byte) : TG2FileModule;
var Module : TG2FileModule;
    i : integer;
    dx, dy, ds : single;
begin
  Module := FindModule(aModuleIndex);
  Result := Module;
  if not(assigned(Result)) then
    exit;

  ds := 0;
  for i := 0 to Count - 1 do begin
    if Items[ i].ParameterCount > 0 then begin
      if Items[ i].Col > Module.Col then begin
        dx := Items[i].Col * UNITS_COL - Module.Col * UNITS_COL;
        dy := Items[i].Row * UNITS_ROW - Module.Row * UNITS_ROW;
        if (ds = 0) or (sqrt(dx*dx+dy*dy) < ds) then begin
          Result := Items[i];
          ds := sqrt(dx*dx+dy*dy);
        end;
      end;
    end;
  end;
end;

procedure TModuleList.Read( aChunk : TPatchChunk);
var i : integer;
    ModuleIndex, ModuleType : byte;
    Module : TG2FileModule;
begin
  // FLocation       : TBits2;
  // FModuleCount    : TBits8;
  // FModules        : array of TModule;

  FModuleCount    := aChunk.ReadBits( 8);

  if assigned(aChunk.FLogLines) then begin
    aChunk.FLogLines.Add('Module list, count = ' + IntToStr(FModuleCount));
    aChunk.FLogLines.Add('Typ Idx Row Col Clr Upr Isl Un1 Mde');
  end;

  for i := 0 to FModuleCount - 1 do begin
    ModuleType  := aChunk.ReadBits( 8);
    ModuleIndex := aChunk.ReadBits( 8);
    //Module := FPatch.CreateModule( TLocationType(FLocation), ModuleIndex, ModuleType);
    Module := FPatchPart.CreateModule( ModuleIndex, ModuleType);
    Module.Read( aChunk);
    Add(Module);
  end;
end;

procedure TModuleList.Write( aChunk : TPatchChunk);
var i : integer;
begin
  FModuleCount := Count;
  aChunk.WriteBits(FModuleCount, 8);
  for i := 0 to Count - 1 do begin
    Items[i].Write( aChunk);
  end;
end;

function TModuleList.GetMaxModuleIndex : integer;
var i : integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    if Items[i].ModuleIndex > Result then
      Result := Items[i].ModuleIndex;
end;

function TModuleList.GetNoOffModuleType( aModuleType : byte) : integer;
var i : integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    if Items[i].TypeID > aModuleType then
      Result := Result + 1;
end;

////////////////////////////////////////////////////////////////////////////////
//  TG2FileCable
////////////////////////////////////////////////////////////////////////////////

constructor TG2FileCable.Create( AOwner: TComponent);
begin
  inherited Create;

  Init;
end;

constructor TG2FileCable.CopyCreate( AOwner: TComponent; aCable : TG2FileCable);
begin
  inherited Create;

  FCableColor    := aCable.FCableColor;
  FModuleFrom    := aCable.FModuleFrom;
  FConnectorFrom := aCable.FConnectorFrom;
  FLinkType      := aCable.FLinkType;
  FModuleTo      := aCable.FModuleTo;
  FConnectorTo   := aCable.FConnectorTo;
end;

destructor TG2FileCable.Destroy;
begin
  inherited;
end;

procedure TG2FileCable.Init;
begin
  FCableColor    := 0;
  FModuleFrom    := 0;
  FConnectorFrom := 0;
  FLinkType      := 0;
  FModuleTo      := 0;
  FConnectorTo   := 0;
end;

procedure TG2FileCable.Read( aChunk : TPatchChunk);
begin
 // FColor         : TBits3;
 // FModuleFrom    : TBits8;
 // FConnectorFrom : TBits6;
 // FLinkType      : TBits1;
 // FModuleTo      : TBits8;
 // FConnectorTo   : TBits6;

 FCableColor    := aChunk.ReadBits( 3);
 FModuleFrom    := aChunk.ReadBits( 8);
 FConnectorFrom := aChunk.ReadBits( 6);
 FLinkType      := aChunk.ReadBits( 1);
 FModuleTo      := aChunk.ReadBits( 8);
 FConnectorTo   := aChunk.ReadBits( 6);
end;

procedure TG2FileCable.Write( aChunk : TPatchChunk);
begin
  aChunk.WriteBits( FCableColor, 3);
  aChunk.WriteBits( FModuleFrom, 8);
  aChunk.WriteBits( FConnectorFrom, 6);
  aChunk.WriteBits( FLinkType, 1);
  aChunk.WriteBits( FModuleTo, 8);
  aChunk.WriteBits( FConnectorTo, 6);
end;

procedure TG2FileCable.ConnectorMoved;
begin
  // Abstract
end;

////////////////////////////////////////////////////////////////////////////////
//  TG2CableList
////////////////////////////////////////////////////////////////////////////////

constructor TCableList.Create( AOwnsObjects : boolean; aPatchPart : TG2FilePatchPart);
begin
  inherited Create( AOwnsObjects);

  Init;
  FPatchPart := aPatchPart;
end;

constructor TCableList.CopySelected( AOwnsObjects : boolean; aPatchPart : TG2FilePatchPart; aModuleList : TModuleList; aCableList : TCableList);
var i : integer;
    ModuleFrom, ModuleTo : TG2FileModule;
    NumCables : integer;
begin
  inherited Create( AOwnsObjects);

  Init;
  FPatchPart := aPatchPart;

  FLocation   := aCableList.FLocation;
  FUnknown    := aCableList.FUnknown;
  NumCables   := aCableList.Count;

  for i := 0 to NumCables - 1 do begin
    ModuleFrom := aModuleList.FindModule( aCableList.Items[i].FModuleFrom);
    ModuleTo := aModuleList.FindModule( aCableList.Items[i].FModuleTo);

    if assigned(ModuleFrom) and assigned(ModuleTo) {and (ModuleFrom.FSelected) and (ModuleTo.FSelected)} then begin
      Add(TG2FileCable.CopyCreate(nil, aCableList.Items[i]));
    end;
  end;
end;

destructor TCableList.Destroy;
begin
  inherited;
end;

function TCableList.GetCable( aIndex : integer) : TG2FileCable;
begin
  result := inherited Items[aindex] as TG2FileCable;
end;

procedure TCableList.SetCable( aIndex : integer; const aValue : TG2FileCable);
begin
  inherited Items[aindex] := aValue;
end;

function TCableList.AddCable( aValue : TG2FileCable): integer;
begin
  Result := inherited Add( aValue);
end;

procedure TCableList.Init;
begin
  Clear;

  FLocation   := 0;
  FUnknown    := 0;
  FCableCount := 0;
end;

procedure TCableList.DeleteCable( aCable : TG2FileCable);
var i : integer;
begin
  i := 0;
  while (i < Count) and ( aCable <> Items[i]) do
    inc(i);
  if (i < Count) then
    Delete(i);
end;

procedure TCableList.Read( aChunk : TPatchChunk);
var i : integer;
    aColor           : TBits3;
    aModuleFrom      : TBits8;
    aConnectorFrom   : TBits6;
    aLinkType        : TBits1;
    aModuleTo        : TBits8;
    aConnectorTo     : TBits6;
    NumCables        : TBits10;
begin
  // FLocation   : TBits2;
  // FUnknown    : TBits12;
  // FCableCount : TBits10;
  // FCables     : array of TCable;

  FUnknown    := aChunk.ReadBits( 12);
  NumCables   := aChunk.ReadBits( 10);

  if assigned( aChunk.FLogLines) then begin
    aChunk.FLogLines.Add('Num Cables = ' + IntToStr( NumCables));
    aChunk.FLogLines.Add('Col MFr CFr Lkp MTo CTo');
  end;

  for i := 0 to NumCables - 1 do begin
    aColor         := aChunk.ReadBits( 3);
    aModuleFrom    := aChunk.ReadBits( 8);
    aConnectorFrom := aChunk.ReadBits( 6);
    aLinkType      := aChunk.ReadBits( 1);
    aModuleTo      := aChunk.ReadBits( 8);
    aConnectorTo   := aChunk.ReadBits( 6);

    if assigned( aChunk.FLogLines) then
      aChunk.FLogLines.Add( Format('%3d %3d %3d %3d %3d %3d', [aColor, aModuleFrom, aConnectorFrom, aLinkType, aModuleTo, aConnectorTo]));

    //Add(FPatch.CreateCable( TLocationType(FLocation), aColor, aModuleFrom, aConnectorFrom, aLinkType, aModuleTo, aConnectorTo));
    Add(FPatchPart.CreateCable( aColor, aModuleFrom, aConnectorFrom, aLinkType, aModuleTo, aConnectorTo));
  end;
  FCableCount := Count;
end;

procedure TCableList.Write( aChunk : TPatchChunk);
var i : integer;
begin
  aChunk.WriteBits(FUnknown, 12);
  FCableCount := Count;
  aChunk.WriteBits(FCableCount, 10);
  for i := 0 to Count - 1 do begin
    Items[i].Write( aChunk);
  end;
end;

function TCableList.FindCable( FromModule : byte; FromConnector : byte; ToModule : byte; ToConnector : byte): TG2FileCable;
var i : integer;
begin
  i := 0;
  while (i < Count) and not((Items[i].FModuleFrom = FromModule)
                         and (Items[i].FConnectorFrom = FromConnector)
                         and (Items[i].FModuleTo = ToModule)
                         and (Items[i].FConnectorTo = ToConnector)) do
    inc(i);
  if (i < Count) then
    Result := Items[i]
  else
    Result := nil;
end;

////////////////////////////////////////////////////////////////////////////////
//  TNote
////////////////////////////////////////////////////////////////////////////////

procedure TNote.Init;
begin
  FNote    := 64;
  FAttack  := 0;
  FRelease := 0;
end;

procedure TNote.Read( aChunk : TPatchChunk);
begin
  FNote    := aChunk.ReadBits( 7);
  FAttack  := aChunk.ReadBits( 7);
  FRelease := aChunk.ReadBits( 7);
end;

procedure TNote.Write( aChunk : TPatchChunk);
begin
  aChunk.WriteBits(FNote, 7);
  aChunk.WriteBits(FAttack, 7);
  aChunk.WriteBits(FRelease, 7);
end;

////////////////////////////////////////////////////////////////////////////////
//  TCurrentNote
////////////////////////////////////////////////////////////////////////////////

constructor TCurrentNote.Create( AOwnsObjects : boolean);
begin
  inherited;

  FNoteCount := 0;
  FLastNote := TNote.Create;
end;

destructor TCurrentNote.Destroy;
begin
  FNoteCount := 0;
  FLastNote.Free;

  inherited;
end;

function TCurrentNote.GetNote( aIndex : integer) : TNote;
begin
  result := inherited Items[aindex] as TNote;
end;

procedure TCurrentNote.SetNote( aIndex : integer; const aValue : TNote);
begin
  inherited Items[aindex] := aValue;
end;

function TCurrentNote.AddNote( aValue : TNote): integer;
begin
  Result := inherited Add( aValue);
end;

procedure TCurrentNote.Init;
var Note : TNote;
begin
  FLastNote.Init;

  Clear;
  Note := TNote.Create;
  Note.Init;
  Add(Note);
end;

procedure TCurrentNote.Read( aChunk : TPatchChunk);
var i : integer;
    Note : TNote;
begin
  //  FLastNote  : TNote;
  //  FNoteCount : TBits5;
  //  FNotes     : array of TNote;
  clear;

  FLastNote.Read( aChunk);
  FNoteCount := aChunk.ReadBits( 5) + 1; // ?

  if assigned( aChunk.FLogLines) then begin
    aChunk.FLogLines.Add('Num Notes = ' + IntToStr( FNoteCount));
    aChunk.FLogLines.Add('Nte Att Rel');
  end;

  for i := 0 to FNoteCount - 1 do begin
    Note := TNote.Create;
    Note.Read( aChunk);

    if assigned( aChunk.FLogLines) then
      aChunk.FLogLines.Add( Format('%3d %3d %3d', [Note.FNote, Note.FAttack, Note.FRelease]));

    Add(Note);
  end;
end;

procedure TCurrentNote.Write( aChunk : TPatchChunk);
var i : integer;
begin
  FLastNote.Write( aChunk);
  FNoteCount := Count - 1;
  aChunk.WriteBits(FNoteCount, 5);
  for i := 0 to Count - 1 do
    Items[i].Write( aChunk);
end;

////////////////////////////////////////////////////////////////////////////////
//  TParams
////////////////////////////////////////////////////////////////////////////////

constructor TParams.Create(aParamSet : TParamSet);
begin
  FParamSet := aParamSet;
end;

constructor TParams.Copy(aParamSet : TParamSet; aParams : TParams);
var i : integer;
begin
  FVariation := aParams.FVariation;
  SetLength(FParamValues, Length(aParams.FParamValues));
  for i := 0 to Length(aParams.FParamValues) - 1 do
    FParamValues[i] := aParams.FParamValues[i];
end;

procedure TParams.Init;
begin
  SetLength(FParamValues, 0);
  FVariation := 0;
end;

destructor TParams.Destroy;
begin
  Finalize(FParamValues);

  inherited;
end;

function TParams.GetParamValue( aIndex : integer): TBits7;
begin
  if aIndex < Length(FParamValues) then
    Result := FPAramValues[ aIndex]
  else
    raise Exception.Create('Param value index ' + IntToStr( aIndex) + ' out of range.');
end;

procedure TParams.SetParamValue( aIndex : integer; aValue : TBits7);
begin
  if aIndex < Length(FParamValues) then
    FPAramValues[ aIndex] := aValue
  else
    raise Exception.Create('Param value index ' + IntToStr( aIndex) + ' out of range.');
end;

procedure TParams.Read( aChunk : TPatchChunk);
var i : integer;
begin
  // FVariation   : Byte;
  // FParamValues : array of TBits7;
  FVariation := aChunk.ReadBits( 8);
  SetLength(FParamValues, FParamSet.FParameterCount);
  for i := 0 to Length(FParamValues) -1 do
    FParamValues[i] := aChunk.ReadBits( 7);
end;

procedure TParams.Write( aChunk : TPatchChunk);
var i : integer;
begin
  aChunk.WriteBits(FVariation, 8);
  for i := 0 to Length(FParamValues) -1 do
    aChunk.WriteBits(FParamValues[i], 7);
end;


////////////////////////////////////////////////////////////////////////////////
//  TParamSet
////////////////////////////////////////////////////////////////////////////////

constructor TParamSet.Create( AOwnsObjects : boolean; aModuleParameters : TModuleParameters);
begin
  inherited Create( AOwnsObjects);

  FModuleParameters := aModuleParameters;
  Init;
end;

destructor TParamSet.Destroy;
begin
  inherited;
end;

procedure TParamSet.Init;
begin
  Clear;

  FModuleIndex := 0;
  FParameterCount := 0;
end;

function TParamSet.GetVariation( aIndex : integer) : TParams;
begin
  result := inherited Items[aindex] as TParams;
end;

procedure TParamSet.SetVariation( aIndex : integer; const aValue : TParams);
begin
  inherited Items[aindex] := aValue;
end;

function TParamSet.AddVariation( aValue : TParams): integer;
begin
  Result := inherited Add( aValue);
end;

constructor TParamSet.Copy( AOwnsObjects : boolean; aModuleParameters : TModuleParameters; aParamSet : TParamSet);
var i : integer;
begin
  inherited Create( AOwnsObjects);

  FModuleParameters := aModuleParameters;
  FModuleIndex := aParamSet.FModuleIndex;
  FParameterCount := aParamSet.FParameterCount;

  for i := 0 to aParamSet.Count - 1 do
    Add( TParams.Copy(self, aParamSet.Items[i]));
end;

procedure TParamSet.AddNewVariation;
var Params : TParams;
    i : integer;
begin
  Params := TParams.Create(self);
  Params.FVariation := FModuleParameters.FVariationCount - 1; // FModuleParameters.FVariationCount must already be increased
  SetLength(Params.FParamValues, FParameterCount);
  for i := 0 to FParameterCount - 1 do
    Params.FParamValues[i] := 0;
  Add(Params);
end;

procedure TParamSet.CopyVariation( aFromVariation, aToVariation : byte);
var i : integer;
begin
  for i := 0 to Length(Items[aFromVariation].FParamValues) - 1 do
    Items[aToVariation].FParamValues[i] := Items[aFromVariation].FParamValues[i];
end;

function TParamSet.FindVariation( aVariation : integer): TParams;
var i : integer;
begin
  i := 0;
  while (i < Count) and (Items[i].FVariation <> aVariation) do
    inc(i);

  if (i < Count) then
    Result := Items[i]
  else
    Result := nil;
end;

procedure TParamSet.Read( aChunk : TPatchChunk);
var i, j : integer;
    Params : TParams;
    param_str : string;
begin
  // Set of parameter values for one module all variations
  // 0 .. 7 : variations 1 .. 8, 8 : init variation
  // FModuleIndex    : TBits8;
  // FParameterCount : TBits7;
  // FVariations     : array[ 0 .. 8] of TParams;

  FModuleIndex    := aChunk.ReadBits( 8);
  FParameterCount := aChunk.ReadBits( 7);

  if assigned( aChunk.FLogLines) then begin
    aChunk.FLogLines.Add('Module id = ' + IntToStr( FModuleIndex) + ', param cnt = ' + IntToStr(FParameterCount));
    aChunk.FLogLines.Add('Var Params');
  end;

  for i := 0 to FModuleParameters.FVariationCount - 1 do begin
    Params := TParams.Create(self);
    Params.Read( aChunk);


    if assigned( aChunk.FLogLines) then begin
      param_str := '';
      for j := 0 to Length(Params.FParamValues) - 1 do
        param_str := param_str + IntToStr(Params.FParamValues[j]) + ' ';

      aChunk.FLogLines.Add( Format('%3d', [Params.FVariation]) + ' ' + param_str);
    end;

    Add(Params);
  end;
end;

procedure TParamSet.Write( aChunk : TPatchChunk; aVariationCount : byte);
var i : integer;
begin
  aChunk.WriteBits(FModuleIndex, 8);
  aChunk.WriteBits(FParameterCount, 7);
  //for i := 0 to min( Count, FModuleParameters.FPatch.FMaxVariations) - 1 do
  for i := 0 to aVariationCount - 1 do
    Items[i].Write( aChunk);
end;

////////////////////////////////////////////////////////////////////////////////
//  TModuleParameters
////////////////////////////////////////////////////////////////////////////////

constructor TModuleParameters.Create( AOwnsObjects : boolean{; aPatch : TG2FilePatch});
begin
  inherited Create( AOwnsObjects);

  Init;
end;

constructor TModuleParameters.CopySelected( AOwnsObjects : boolean; {aPatch : TG2FilePatch;} aModuleList : TModuleList; aModuleParameters : TModuleParameters);
var NumSets, i : integer;
    Module : TG2FileModule;
begin
  inherited Create( AOwnsObjects);
  Init;

  NumSets         := aModuleParameters.Count;
  FVariationCount := aModuleParameters.FVariationCount;
  for i := 0 to NumSets - 1 do begin
    Module := aModuleList.FindModule( aModuleParameters.Items[ i].ModuleIndex);

    if assigned(Module) {and (Module.FSelected)} then
      Add( TParamSet.Copy( True, self, aModuleParameters.Items[ i]));
  end;
end;

destructor TModuleParameters.Destroy;
begin
  inherited;
end;

procedure TModuleParameters.Init;
begin
  Clear;

  FParamSetCount  := 0;
  FVariationCount := 0;
end;

function TModuleParameters.GetParamSet( aIndex : integer) : TParamSet;
begin
  result := inherited Items[aindex] as TParamSet;
end;

procedure TModuleParameters.SetParamSet( aIndex : integer; const aValue : TParamSet);
begin
  inherited Items[aindex] := aValue;
end;

function TModuleParameters.AddParamSet( aValue : TParamSet): integer;
begin
  Result := inherited Add( aValue);
end;

procedure TModuleParameters.DeleteModule( aModuleIndex : Byte);
var i : integer;
begin
  i := 0;
  while (i < Count) and (Items[i].ModuleIndex <> aModuleIndex) do
    inc(i);

  if (i < Count) then begin
    Delete(i);
    FParamSetCount := Count;
  end;
end;

procedure TModuleParameters.Read( aChunk : TPatchChunk);
var NumSets, i : integer;
    ParamSet : TParamSet;
begin
  //  FLocation  : Tbits2;
  //  FSetCount  : TBits8;
  //  FUnknown   : TBits8  <= this must be variationcount, there seem to be 10 variations in patches that come from usb
  //  FParamSets : array of TParamSet;

  NumSets         := aChunk.ReadBits( 8);
  FVariationCount := aChunk.ReadBits( 8);

  if assigned( aChunk.FLogLines) then begin
    aChunk.FLogLines.Add('Num sets = ' + IntToStr( NumSets) + ', var cnt = ' + IntToStr(FVariationCount));
    aChunk.FLogLines.Add('Var Params');
  end;

  for i := 0 to NumSets - 1 do begin
    ParamSet := TParamSet.Create( True, self);
    ParamSet.Read( achunk);
    Add(ParamSet);
  end;
  FParamSetCount := Count;
end;

procedure TModuleParameters.Write( aChunk : TPatchChunk; aVariationCount : byte);
var i : integer;
begin
  FParamSetCount := Count;
  aChunk.WriteBits(FParamSetCount, 8);
  //aChunk.WriteBits( min( FVariationCount, FPatch.FMaxVariations), 8);
  if FParamSetCount = 0 then
    aChunk.WriteBits( 0, 8)
  else
    aChunk.WriteBits( aVariationCount, 8);
  for i := 0 to Count - 1 do
    Items[i].Write( aChunk, aVariationCount);
end;

procedure TModuleParameters.AddNewVariation;
var i : integer;
begin
  inc(FVariationCount);
  for i := 0 to Count - 1 do
    Items[i].AddNewVariation;
end;

procedure TModuleParameters.CopyVariation( aFromVariation, aToVariation : byte);
var i : integer;
begin
  for i := 0 to Count - 1 do
    Items[i].CopyVariation( aFromVariation, aToVariation);
end;

function TModuleParameters.FindParamSet( aModuleIndex : integer): TParamSet;
var i : integer;
begin
  i := 0;
  while (i < Count) and ( Items[i].ModuleIndex <> aModuleIndex) do
    inc(i);

  if (i < Count) then
    Result := Items[i]
  else
    Result := nil;
end;

function TModuleParameters.FindParamValue( aModuleIndex, aVariation, aParamIndex : integer): TBits7;
var ParamSet : TParamSet;
    Params : TParams;
begin
  ParamSet := FindParamSet( aModuleIndex);
  if assigned(ParamSet) then begin
    Params := ParamSet.FindVariation( aVariation);
    if assigned(Params) then
      Result := Params.FParamValues[ aParamIndex]
    else
      Result := 0;
  end else
    Result := 0;
end;

////////////////////////////////////////////////////////////////////////////////
//  TMorph
////////////////////////////////////////////////////////////////////////////////

function TMorph.GetDial( aIndex : integer): TBits7;
begin
  if aIndex < Length(FDials) then
    Result := FDials[ aIndex]
  else
    raise Exception.Create('Dial index ' + IntToStr( aIndex) + ' out of range.');
end;

procedure TMorph.SetDial( aIndex : integer; aValue : TBits7);
begin
  if aIndex < Length(FDials) then
    FDials[ aIndex] := aValue
  else
    raise Exception.Create('Dial index ' + IntToStr( aIndex) + ' out of range.');
end;

function TMorph.GetMode( aIndex : integer): TBits7;
begin
  if aIndex < Length(FModes) then
    Result := FModes[ aIndex]
  else
    raise Exception.Create('Mode index ' + IntToStr( aIndex) + ' out of range.');
end;

procedure TMorph.SetMode( aIndex : integer; aValue : TBits7);
begin
  if aIndex < Length(FDials) then
    FModes[ aIndex] := aValue
  else
    raise Exception.Create('Mode index ' + IntToStr( aIndex) + ' out of range.');
end;

////////////////////////////////////////////////////////////////////////////////
//  TMorphParameter
////////////////////////////////////////////////////////////////////////////////

procedure TMorphParameter.Init;
begin
  FLocation    := 0;
  FModuleIndex := 0;
  FParamIndex  := 0;
  FMorph       := 0;
  FRange       := 0;
end;

procedure TMorphParameter.Read( aChunk : TPatchChunk);
begin
  // FLocation    : TBits2;
  // FModuleIndex : TBits8;
  // FParamIndex  : TBits7;
  // FMorph       : TBits4;
  // FRange       : TBits8;

  FLocation    := aChunk.ReadBits( 2);
  FModuleIndex := aChunk.ReadBits( 8);
  FParamIndex  := aChunk.ReadBits( 7);
  FMorph       := aChunk.ReadBits( 4);
  FRange       := aChunk.ReadBits( 8);

  if assigned(aChunk.FLogLines) then
    aChunk.FLogLines.Add(Format('%3d %3d %3d %3d %3d', [FLocation, FModuleIndex, FParamIndex, FMorph, FRange]));
end;

procedure TMorphParameter.Write( aChunk : TPatchChunk);
begin
  aChunk.WriteBits(FLocation, 2);
  aChunk.WriteBits(FModuleIndex, 8);
  aChunk.WriteBits(FParamIndex, 7);
  aChunk.WriteBits(FMorph, 4);
  aChunk.WriteBits(FRange, 8);
end;

////////////////////////////////////////////////////////////////////////////////
//  TGMorphParameters
////////////////////////////////////////////////////////////////////////////////

constructor TMorphParameters.Create( aPatch : TG2FilePatch);
begin
  FPatch := aPatch;
end;

destructor TMorphParameters.Destroy;
begin
  inherited;
end;

procedure TMorphParameters.Init;
var i, j : integer;
begin
  // TVariations in TPatchsettings should already be created and initialized

  FVariationCount := Length(FPatch.FPatchSettings.FVariations);
  FMorphCount := NMORPHS;
  FReserved1 := 0;
  FReserved2 := 0;

  for i := 0 to FVariationCount - 1 do begin
    Unknown1   := 0;
    for j := 0 to 5 do
      Unknown2[j] := 0;
    Unknown3   := 0;

    FPatch.FPatchSettings.FVariations[i].Clear;

    FReserved2      := 0;
  end;
end;

procedure TMorphParameters.Read( aChunk : TPatchChunk);
var i, j, BitsLeft : integer;
    Variation : TVariation;
    VariationIndex : integer;
    MorphParamCount : integer;
    MorphParam : TMorphParameter;
begin
  // FVariationCount : TBits8;
  // FMorphCount     : TBits4;
  // FReserved1      : integer;
  // FReserved2      : TBits4;

  FVariationCount := aChunk.ReadBits( 8);
  FMorphCount     := aChunk.ReadBits( 4);
  FReserved1      := aChunk.ReadBits( 20);

  if assigned(aChunk.FLogLines) then begin
    aChunk.FLogLines.Add('Morph param list, varcount : ' + IntToStr(FVariationCount) + ', morphcount : ' + IntToStr(FMorphCount));
  end;

  for i := 0 to FVariationCount - 1 do begin
    VariationIndex := aChunk.ReadBits( 4);

    // These should be fields in TVariation, otherwise there overwritten here for each variation, but maybe they are always 0
    Unknown1 := aChunk.ReadBits( 4);
    for j := 0 to 5 do
      Unknown2[j] := aChunk.ReadBits( 8);
    Unknown3 := aChunk.ReadBits( 4);

    MorphParamCount := aChunk.ReadBits( 8);

    Variation := FPatch.FPatchSettings.FVariations[VariationIndex];

    if assigned(aChunk.FLogLines) then begin
      aChunk.FLogLines.Add('Morph params, variation : ' + IntToStr(VariationIndex) + ', count = ' + IntToStr(MorphParamCount));
      aChunk.FLogLines.Add('Loc Mod Par Mor Rng');
    end;

    for j := 0 to MorphParamCount - 1 do begin
       MorphParam := TMorphParameter.Create;
       MorphParam.Read( aChunk);
       Variation.Add( MorphParam);
    end;

    BitsLeft := aChunk.GetReadBufferBitsLeft;
    if BitsLeft < 4 then
    // At the end of the chunk we need to read the bits that are left over to align to byte
      FReserved2 := aChunk.ReadBits( BitsLeft)
    else
      FReserved2 := aChunk.ReadBits( 4);
  end;
end;

procedure TMorphParameters.Write( aChunk : TPatchChunk; aVariationCount : byte);
var i, j : integer;
    Variation : TVariation;
begin
  //NumVariations := min( FVariationCount, FPatch.FMaxVariations);
  //aChunk.WriteBits(FVariationCount, 8);
  aChunk.WriteBits(aVariationCount, 8);
  aChunk.WriteBits(FMorphCount,     4);
  aChunk.WriteBits(FReserved1,     20);

  for i := 0 to aVariationCount - 1 do begin

    Variation := FPatch.FPatchSettings.FVariations[i];

    aChunk.WriteBits(i, 4);

    aChunk.WriteBits(Unknown1, 4);
    for j := 0 to 5 do
      aChunk.WriteBits(Unknown2[j], 8);
    aChunk.WriteBits(Unknown3, 4);

    aChunk.WriteBits( Variation.Count,  8);
    for j := 0 to Variation.Count - 1 do begin
      Variation.Items[j].Write( aChunk);
    end;

    if i < (aVariationCount - 1) then
      aChunk.WriteBits(FReserved2, 4)
    else
    // At the end of the chunk we need to write the bits that are left over to align to byte
      if aChunk.FBitWriter.GetWriteBufferBitsLeft < 8 then
        // It seems that in case of performances when this is <> 0, the perf. failes to load! This one only took one evening ;)
        aChunk.WriteBits({FReserved2}0, aChunk.FBitWriter.GetWriteBufferBitsLeft);
  end;
end;

procedure TMorphParameters.AddNewVariation;
var Variation : TVariation;
    i, MorphParamCount : integer;
    MorphParam : TMorphParameter;
begin
  inc( FVariationCount);
  Variation := FPatch.FPatchSettings.FVariations[FVariationCount - 1]; // Should already be added

  MorphParamCount := 0; //?
  for i := 0 to MorphParamCount - 1 do begin
    MorphParam := TMorphParameter.Create;
    MorphParam.Init;
    Variation.Add( MorphParam);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//  TVariation
////////////////////////////////////////////////////////////////////////////////

constructor TVariation.Create( AOwnsObjects : boolean);
begin
  inherited;
end;

destructor TVariation.Destroy;
begin
  inherited;
end;

procedure TVariation.Copy( aFromVariation : TVariation);
begin
  FPatchVol    := aFromVariation.FPatchVol;
  FGlide       := aFromVariation.FGlide;
  FGlideTime   := aFromVariation.FGlideTime;
  FBend        := aFromVariation.FBend;
  FSemi        := aFromVariation.FSemi;
  FVibrato     := aFromVariation.FVibrato;
  FCents       := aFromVariation.FCents;
  FRate        := aFromVariation.FRate;
  FArpeggiator := aFromVariation.FArpeggiator;
  FArpTime     := aFromVariation.FArpTime;
  FArpType     := aFromVariation.FArpType;
  FOctaves     := aFromVariation.FOctaves;
  FSustain     := aFromVariation.FSustain;
  FOctaveShift := aFromVariation.FOctaveShift;
end;

function TVariation.GetMorphParam( aIndex : integer) : TMorphParameter;
begin
  result := inherited Items[aindex] as TMorphParameter;
end;

procedure TVariation.SetMorphParam( aIndex : integer; const aValue : TMorphParameter);
begin
  inherited Items[aindex] := aValue;
end;

function TVariation.AddMorphParam( aValue : TMorphParameter): integer;
begin
  Result := inherited Add( aValue);
end;

function TVariation.FindMorphParam( aLocation : TLocationType; aModuleIndex, aParamIndex, aMorphIndex, aVariation : byte): TMorphParameter;
var i : integer;
begin
  Result := nil;
  i := 0;
  while (i < Count) and
    not((Items[i].FLocation = TBits2(aLocation)) and
        (Items[i].FModuleIndex = aModuleIndex) and
        (Items[i].FParamIndex = aParamIndex) and
        (Items[i].FMorph = aMorphIndex)) do
    inc(i);

  if (i < Count) then
    Result := Items[i];
end;

procedure TVariation.AddNewMorphParam( aLocation, aModuleIndex, aParamIndex, aMorphIndex, aRange: byte);
var MorphParam : TMorphParameter;
begin
  MorphParam := TMorphParameter.Create;
  MorphParam.FLocation := aLocation;
  MorphParam.FModuleIndex := aModuleIndex;
  MorphParam.FParamIndex := aParamIndex;
  MorphParam.FMorph := aMorphIndex;
  MorphParam.FRange := aRange;
  Add( MorphParam);
end;

procedure TVariation.DelMorphParam( aLocation, aModuleIndex, aParamIndex, aMorphIndex: byte);
var i: integer;
begin
  i := 0;
  while (i < Count) and not((Items[i].FLocation = aLocation) and
                            (Items[i].FModuleIndex = aModuleIndex) and
                            (Items[i].FParamIndex = aParamIndex) and
                            (Items[i].FMorph = aMorphIndex)) do
    inc(i);

  if (i < Count) then begin
    Delete( i);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//  TPatchSettings
////////////////////////////////////////////////////////////////////////////////

constructor TPatchSettings.Create;
var i : integer;
begin
  // Start to create 9 Variations
  SetLength(FVariations, NVARIATIONS);
  for i := 0 to NVARIATIONS - 1 do
    FVariations[i] := TVariation.Create( True);

  for i := 0 to NMORPHS - 1 do begin
    FMorphs[i] := TMorph.Create;
    SetLength(FMorphs[i].FDials, NVARIATIONS);
    SetLength(FMorphs[i].FModes, NVARIATIONS);
  end;
end;

procedure TPatchSettings.Init;
var i , j : integer;
begin
  FLocation       := 2;
  FSectionCnt     := 7;
  FVariationCount := NVARIATIONS;

  SetLength(FVariations, FVariationCount);

  for i := 0 to FVariationCount - 1 do begin
    for j := 0 to NMORPHS - 1 do
      FMorphs[j].FDials[i] := 0;

    for j := 0 to NMORPHS - 1 do
      FMorphs[j].FModes[i] := 1;
  end;

  for i := 0 to FVariationCount - 1 do begin
    FVariations[i].FPatchVol    := 100;
    FVariations[i].FActiveMuted := 1;
  end;

  for i := 0 to FVariationCount - 1 do begin
    FVariations[i].FGlide      := 0;
    FVariations[i].FGlideTime  := 28;
  end;

  for i := 0 to FVariationCount - 1 do begin
    FVariations[i].FBend       := 1;
    FVariations[i].FSemi       := 1;
  end;

  for i := 0 to FVariationCount - 1 do begin
    FVariations[i].FVibrato     := 0;
    FVariations[i].FCents       := 50;
    FVariations[i].FRate        := 64;
  end;

  for i := 0 to FVariationCount - 1 do begin
    FVariations[i].FArpeggiator := 0;
    FVariations[i].FArpTime     := 3;
    FVariations[i].FArpType     := 0;
    FVariations[i].FOctaves     := 0;
  end;

  for i := 0 to FVariationCount - 1 do begin
    FVariations[i].FOctaveShift := 2;
    FVariations[i].FSustain     := 1;
  end;
end;

destructor TPatchSettings.Destroy;
var i : integer;
begin
  for i := 0 to NMORPHS - 1 do begin
    Finalize(FMorphs[i].FDials);
    Finalize(FMorphs[i].FModes);
    FMorphs[i].Free;
  end;

  for i := 0 to Length(FVariations) -1 do
    FVariations[i].Free;
  Finalize(FVariations);

  inherited;
end;

function TPatchSettings.GetVariation( aIndex : integer): TVariation;
begin
  if aIndex < Length(FVariations) then
    Result := FVariations[ aIndex]
  else
    raise Exception.Create('Variation index ' + IntToStr( aIndex) + ' out of range.');
end;

procedure TPatchSettings.SetVariation( aIndex : integer; aValue : TVariation);
begin
  if aIndex < Length(FVariations) then
    FVariations[ aIndex] := aValue
  else
    raise Exception.Create('Variation index ' + IntToStr( aIndex) + ' out of range.');
end;

procedure TPatchSettings.Read( aChunk : TPatchChunk);
var i, j, variation : integer;
begin
  // FLocation       : Tbits2;
  // FSectionCnt     : TBits8;
  // FVariationCount : TBits8;
  // FMorphs         : array of TMorph;
  // FVariations     : array of TVariation;

  FSectionCnt     := aChunk.ReadBits( 8);
  FVariationCount := aChunk.ReadBits( 8);

  i := Length(FVariations);
  SetLength(FVariations, FVariationCount); // Can be more then 9?
  while (i<FVariationCount) do begin
    FVariations[i] := TVariation.Create(True);  // Create more when necessary (usb)
    inc(i);
  end;

  aChunk.ReadBits( 8); // Sections
  aChunk.ReadBits( 7); // Entries

  for i := 0 to FVariationCount - 1 do begin
    variation := aChunk.ReadBits( 8);
    for j := 0 to NMORPHS - 1 do
      FMorphs[j].FDials[variation] := aChunk.ReadBits( 7);

    for j := 0 to NMORPHS - 1 do
      FMorphs[j].FModes[variation] := aChunk.ReadBits( 7);
  end;

  aChunk.ReadBits( 8); // Sections
  aChunk.ReadBits( 7); // Entries

  for i := 0 to FVariationCount - 1 do begin
    variation := aChunk.ReadBits( 8);
    FVariations[variation].FPatchVol    := aChunk.ReadBits( 7);
    FVariations[variation].FActiveMuted := aChunk.ReadBits( 7);
  end;

  aChunk.ReadBits( 8); // Sections
  aChunk.ReadBits( 7); // Entries

  for i := 0 to FVariationCount - 1 do begin
    variation := aChunk.ReadBits( 8);
    FVariations[variation].FGlide     := aChunk.ReadBits( 7);
    FVariations[variation].FGlideTime := aChunk.ReadBits( 7);
  end;

  aChunk.ReadBits( 8); // Sections
  aChunk.ReadBits( 7); // Entries

  for i := 0 to FVariationCount - 1 do begin
    variation := aChunk.ReadBits( 8);
    FVariations[variation].FBend := aChunk.ReadBits( 7);
    FVariations[variation].FSemi := aChunk.ReadBits( 7);
  end;

  aChunk.ReadBits( 8); // Sections
  aChunk.ReadBits( 7); // Entries

  for i := 0 to FVariationCount - 1 do begin
    variation := aChunk.ReadBits( 8);
    FVariations[variation].FVibrato     := aChunk.ReadBits( 7);
    FVariations[variation].FCents       := aChunk.ReadBits( 7);
    FVariations[variation].FRate        := aChunk.ReadBits( 7);
  end;

  aChunk.ReadBits( 8); // Sections
  aChunk.ReadBits( 7); // Entries

  for i := 0 to FVariationCount - 1 do begin
    variation := aChunk.ReadBits( 8);
    FVariations[variation].FArpeggiator := aChunk.ReadBits( 7);
    FVariations[variation].FArpTime     := aChunk.ReadBits( 7);
    FVariations[variation].FArpType     := aChunk.ReadBits( 7);
    FVariations[variation].FOctaves     := aChunk.ReadBits( 7);
  end;

  aChunk.ReadBits( 8); // Sections
  aChunk.ReadBits( 7); // Entries

  for i := 0 to FVariationCount - 1 do begin
    variation := aChunk.ReadBits( 8);
    FVariations[variation].FOctaveShift := aChunk.ReadBits( 7);
    FVariations[variation].FSustain     := aChunk.ReadBits( 7);
  end;
end;


procedure TPatchSettings.Write( aChunk : TPatchChunk; aVariationCount : byte);
var i , j : integer;
begin
  //NumVariations := min(FVariationCount, FPatch.FMaxVariations);

  aChunk.WriteBits( FSectionCnt, 8);
  aChunk.WriteBits( aVariationCount, 8);

  aChunk.WriteBits(1, 8);
  aChunk.WriteBits(16, 7);

  for i := 0 to aVariationCount - 1 do begin
    aChunk.WriteBits(i, 8);
    for j := 0 to NMORPHS - 1 do
      aChunk.WriteBits(FMorphs[j].FDials[i], 7);

    for j := 0 to NMORPHS - 1 do
      aChunk.WriteBits(FMorphs[j].FModes[i], 7);
  end;

  aChunk.WriteBits(2, 8);
  aChunk.WriteBits(2, 7);

  for i := 0 to aVariationCount - 1 do begin
    aChunk.WriteBits(i, 8);
    aChunk.WriteBits(FVariations[i].FPatchVol, 7);
    aChunk.WriteBits(FVariations[i].FActiveMuted, 7);
  end;

  aChunk.WriteBits(3, 8);
  aChunk.WriteBits(2, 7);

  for i := 0 to aVariationCount - 1 do begin
    aChunk.WriteBits(i, 8);
    aChunk.WriteBits(FVariations[i].FGlide, 7);
    aChunk.WriteBits(FVariations[i].FGlideTime, 7);
  end;

  aChunk.WriteBits(4, 8);
  aChunk.WriteBits(2, 7);

  for i := 0 to aVariationCount - 1 do begin
    aChunk.WriteBits( i, 8);
    aChunk.WriteBits( FVariations[i].FBend, 7);
    aChunk.WriteBits( FVariations[i].FSemi, 7);
  end;

  aChunk.WriteBits( 5, 8);
  aChunk.WriteBits( 3, 7);

  for i := 0 to aVariationCount - 1 do begin
    aChunk.WriteBits( i, 8);
    aChunk.WriteBits( FVariations[i].FVibrato, 7);
    aChunk.WriteBits( FVariations[i].FCents, 7);
    aChunk.WriteBits( FVariations[i].FRate, 7);
  end;

  aChunk.WriteBits( 6, 8);
  aChunk.WriteBits( 4, 7);

  for i := 0 to aVariationCount - 1 do begin
    aChunk.WriteBits( i, 8);
    aChunk.WriteBits( FVariations[i].FArpeggiator, 7);
    aChunk.WriteBits( FVariations[i].FArpTime, 7);
    aChunk.WriteBits( FVariations[i].FArpType, 7);
    aChunk.WriteBits( FVariations[i].FOctaves, 7);
  end;

  aChunk.WriteBits( 7, 8);
  aChunk.WriteBits( 2, 7);

  for i := 0 to aVariationCount - 1 do begin
    aChunk.WriteBits( i, 8);
    aChunk.WriteBits( FVariations[i].FOctaveShift, 7);
    aChunk.WriteBits( FVariations[i].FSustain, 7);
  end;
end;

procedure TPatchSettings.AddNewVariation;
var j : integer;
begin
  inc( FVariationCount);
  SetLength(FVariations, FVariationCount);

  for j := 0 to NMORPHS - 1 do begin
    SetLength( FMorphs[j].FDials, FVariationCount);
    FMorphs[j].FDials[FVariationCount - 1] := 0;
  end;

  for j := 0 to NMORPHS - 1 do begin
    SetLength( FMorphs[j].FModes, FVariationCount);
    FMorphs[j].FModes[FVariationCount - 1] := 1;
  end;

  FVariations[FVariationCount - 1] := TVariation.Create( True);
  FVariations[FVariationCount - 1].FPatchVol    := 100;
  FVariations[FVariationCount - 1].FActiveMuted := 1;
  FVariations[FVariationCount - 1].FGlide       := 0;
  FVariations[FVariationCount - 1].FGlideTime   := 28;
  FVariations[FVariationCount - 1].FBend        := 1;
  FVariations[FVariationCount - 1].FSemi        := 1;
  FVariations[FVariationCount - 1].FVibrato     := 0;
  FVariations[FVariationCount - 1].FCents       := 50;
  FVariations[FVariationCount - 1].FRate        := 64;
  FVariations[FVariationCount - 1].FArpeggiator := 0;
  FVariations[FVariationCount - 1].FArpTime     := 3;
  FVariations[FVariationCount - 1].FArpType     := 0;
  FVariations[FVariationCount - 1].FOctaves     := 0;
  FVariations[FVariationCount - 1].FOctaveShift := 2;
  FVariations[FVariationCount - 1].FSustain     := 1;
end;

////////////////////////////////////////////////////////////////////////////////
//  TKnobAssignment
////////////////////////////////////////////////////////////////////////////////

procedure TKnob.Init;
begin
 FAssigned := 0;
 FLocation := 0;
 FModuleIndex := 0;
 FIsLed := 0;
 FParamIndex := 0;

 FParameter := nil;
end;

procedure TKnob.Read( aChunk : TPatchChunk);
begin
  // Fassigned : TBits1;
  // FLocation : TBits2;
  // FModuleIndex : TBits8;
  // FIsLed : TBits2;
  // FParamIndex: TBits7;


  FAssigned    := aChunk.ReadBits( 1);
  if FAssigned = 1 then begin

    FLocation    := aChunk.ReadBits( 2);
    FModuleIndex := aChunk.ReadBits( 8);
    FIsLed       := aChunk.ReadBits( 2);
    FParamIndex  := aChunk.ReadBits( 7);
  end;
end;

procedure TKnob.Write( aChunk : TPatchChunk);
begin
  aChunk.WriteBits(FAssigned, 1);
  if (FAssigned = 1) then begin
    aChunk.WriteBits(FLocation, 2);
    aChunk.WriteBits(FModuleIndex, 8);
    aChunk.WriteBits(FIsLed, 2);
    aChunk.WriteBits(FParamIndex, 7);
  end;
end;

function TKnob.GetKnobValue : byte;
begin
  if (FAssigned = 1) and assigned(FParameter) then
    Result := FParameter.GetParameterValue
  else
    Result := 0;
end;

procedure TKnob.SetKnobValue( aValue : byte);
begin
  if (FAssigned = 1) and assigned(FParameter) then
    FParameter.SetParameterValue( aValue);
end;

function TKnob.GetKnobButtonValue : byte;
var ButtonParam : TG2FileParameter;
begin
  Result := 0;
  if (FAssigned = 1) and assigned(FParameter) then begin
    ButtonParam := FParameter.ButtonParam;
    if assigned(ButtonParam) then
      Result := ButtonParam.GetParameterValue;
  end;
end;

procedure TKnob.SetKnobButtonValue( aValue : byte);
var ButtonParam : TG2FileParameter;
begin
  if (FAssigned = 1) and assigned(FParameter) then begin
    ButtonParam := FParameter.ButtonParam;
    if assigned(ButtonParam) then
      ButtonParam.SetParameterValue( aValue);
  end;
end;

function TKnob.GetKnobFloatValue : single;
begin
  Result := 0;
  if (FAssigned = 1) and assigned(FParameter) then
    if FParameter.HighValue - FParameter.LowValue <> 0 then
      Result := FParameter.GetParameterValue / (FParameter.HighValue - FParameter.LowValue);
end;

function TKnob.GetKnobButtonFloatValue : single;
var ButtonParam : TG2FileParameter;
begin
  Result := 0;
  if (FAssigned = 1) and assigned(FParameter) then begin
    ButtonParam := FParameter.ButtonParam;
    if assigned(ButtonParam) then begin
      if ButtonParam.HighValue - ButtonParam.LowValue <> 0 then
        Result := ButtonParam.GetParameterValue / (ButtonParam.HighValue - ButtonParam.LowValue);
    end;
  end;
end;

procedure TKnob.SetKnobFloatValue( aValue : single);
begin
  if (FAssigned = 1) and assigned(FParameter) then
    FParameter.SetParameterValue( trunc(aValue * 127));
end;

procedure TKnob.SetKnobButtonFloatValue( aValue : single);
var ButtonParam : TG2FileParameter;
begin
  if (FAssigned = 1) and assigned(FParameter) then begin
    ButtonParam := FParameter.ButtonParam;
    if assigned(ButtonParam) then
      ButtonParam.SetParameterValue( trunc(aValue * 127));
  end;
end;

function TKnob.GetKnobHighValue : byte;
begin
  if (FAssigned = 1) and assigned(FParameter) then
    Result := FParameter.HighValue
  else
    Result := 0;
end;

function TKnob.GetKnobLowValue : byte;
begin
  if (FAssigned = 1) and assigned(FParameter) then
    Result := FParameter.LowValue
  else
    Result := 0;
end;

////////////////////////////////////////////////////////////////////////////////
//  TKnobList
////////////////////////////////////////////////////////////////////////////////

constructor TKnobList.Create( AOwnsObjects: Boolean);
var i : integer;
    Knob : TKnob;
begin
  inherited;

  FKnobCount := 120;
  for i := 0 to FKnobCount - 1 do begin
    Knob := TKnob.Create;
    Knob.FKnobIndex := i;
    Add( Knob);
  end;
end;

destructor TKnobList.Destroy;
begin
  inherited;
end;

function TKnobList.GetKnob( aIndex : integer) : TKnob;
begin
  result := inherited Items[aindex] as TKnob;
end;

procedure TKnobList.SetKnob( aIndex : integer; const aValue : TKnob);
begin
  inherited Items[aindex] := aValue;
end;

function TKnobList.AddKnob( aValue : TKnob): integer;
begin
  Result := inherited Add( aValue);
end;

procedure TKnobList.Init;
var i : integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Init;
end;

procedure TKnobList.DeleteModule( aLocation, aModuleIndex : Byte);
var i : integer;
begin
  for i := 0 to Count - 1 do
    if (Items[i].FModuleIndex = aModuleIndex) and (Items[i].FLocation = aLocation) then
      Items[i].Fassigned := 0;
end;

procedure TKnobList.Read( aChunk : TPatchChunk);
var i : integer;
begin
  FKnobCount := aChunk.ReadBits( 16);

  if assigned(aChunk.FLogLines) then begin
    aChunk.FLogLines.Add('KnobList, count = ' + IntToStr(FKnobCount));
    aChunk.FLogLines.Add('Knb Loc Mod Isl Par');
  end;

  for i := 0 to FKnobCount - 1 do begin
    Items[i].Read( aChunk);

    if assigned( aChunk.FLogLines) and (Items[i].FAssigned = 1) then
      aChunk.FLogLines.Add( Format('%3d %3d %3d %3d %3d', [i, Items[i].FLocation, Items[i].FModuleIndex, Items[i].FIsLed, Items[i].FParamIndex]));
  end;
end;

procedure TKnobList.Write( aChunk : TPatchChunk);
var i : integer;
begin
  FKnobCount := Count;
  aChunk.WriteBits(FKnobCount, 16);
  for i := 0 to FKnobCount - 1 do
    Items[i].Write( aChunk);
end;

function TKnobList.FindKnob(aLocation : TLocationType; aModuleIndex, aParamIndex : byte): integer;
var i : integer;
begin
  i := 0;
  while (i < Count) and not(( Items[i].FAssigned = 1)
                        and ( Items[i].FLocation = TBits2(aLocation))
                        and ( Items[i].FModuleIndex = aModuleIndex)
                        and ( Items[i].FParamIndex = aParamIndex) ) do
    inc(i);

  if (i < Count) then begin
    Result := i
  end else
    Result := -1;
end;

////////////////////////////////////////////////////////////////////////////////
//  TController
////////////////////////////////////////////////////////////////////////////////

procedure TController.Init;
begin
  FMidiCC := 0;
  FLocation := 0;
  FModuleIndex := 0;
  FParamIndex := 0;
  FKnob := nil;
 end;

procedure TController.Read( aChunk : TPatchChunk);
begin
  // FMidiCC : TBits7;
  // FLocation : TBits2;
  // FModuleIndex : TBits8;
  // FParamIndex : TBits7;

  FMidiCC      := aChunk.ReadBits( 7);
  FLocation    := aChunk.ReadBits( 2);
  FModuleIndex := aChunk.ReadBits( 8);
  FParamIndex  := aChunk.ReadBits( 7);
  FKnob := nil;
end;

procedure TController.Write( aChunk : TPatchChunk);
begin
  aChunk.WriteBits(FMidiCC, 7);
  aChunk.WriteBits(FLocation, 2);
  aChunk.WriteBits(FModuleIndex, 8);
  aChunk.WriteBits(FParamIndex, 7);
end;

////////////////////////////////////////////////////////////////////////////////
//  TControllerList
////////////////////////////////////////////////////////////////////////////////

constructor TControllerList.Create( AOwnsObjects: Boolean);
begin
  inherited;
end;

destructor TControllerList.Destroy;
begin
  inherited;
end;

function TControllerList.GetController( aIndex : integer) : TController;
begin
  result := inherited Items[aindex] as TController;
end;

procedure TControllerList.SetController( aIndex : integer; const aValue : TController);
begin
  inherited Items[aindex] := aValue;
end;

function TControllerList.AddController( aValue : TController): integer;
begin
  Result := inherited Add( aValue);
end;

procedure TControllerList.Init;
var Controller : TController;
begin
  Clear;

  Controller := TController.Create;
  Controller.FMidiCC := 7;
  Controller.FLocation := 2;
  Controller.FModuleIndex := 2;
  Controller.FParamIndex := 0;
  Add( Controller);

  Controller := TController.Create;
  Controller.FMidiCC := 17;
  Controller.FLocation := 2;
  Controller.FModuleIndex := 7;
  Controller.FParamIndex := 0;
  Add( Controller);
end;

procedure TControllerList.DeleteModule( aModuleIndex : Byte);
var i : integer;
begin
  i := 0;
  while (i < Count) and (Items[i].FModuleIndex <> aModuleIndex) do
    inc(i);

  if (i < Count) then begin
    Delete(i);
    FControllerCount := Count;
  end;

end;

procedure TControllerList.Read( aChunk : TPatchChunk);
var i : integer;
    Controller : TController;
begin
  // FControllerCount : TBits7;
  Clear;

  FControllerCount := aChunk.ReadBits( 7);

  if assigned(aChunk.FLogLines) then begin
    aChunk.FLogLines.Add('Controller list, count = ' + IntToStr(FControllerCount));
    aChunk.FLogLines.Add('CC# Loc Mod Par');
  end;

  for i := 0 to FControllerCount - 1 do begin
    Controller := TController.Create;
    Controller.Read( aChunk);

    if assigned( aChunk.FLogLines) then
      aChunk.FLogLines.Add( Format('%3d %3d %3d %3d', [Controller.FMidiCC, Controller.FLocation, Controller.FModuleIndex, Controller.FParamIndex]));
    Add( Controller);
  end;
end;

procedure TControllerList.Write( aChunk : TPatchChunk);
var i : integer;
begin
  FControllerCount := Count;
  aChunk.WriteBits(FControllerCount, 7);
  for i := 0 to FControllerCount - 1 do begin
    Items[i].Write(aChunk);
  end;
end;

function TControllerList.FindMidiCC(aLocation : TLocationType; aModuleIndex, aParamIndex : byte): byte;
var i : integer;
begin
  i := 0;
  while (i < Count)
           and not(( Items[i].FLocation = TBits2(aLocation))
               and ( Items[i].FModuleIndex = aModuleIndex)
               and ( Items[i].FParamIndex = aParamIndex)) do
    inc(i);

  if (i < Count) then begin
    Result :=  Items[i].FMidiCC
  end else
    Result := 0;
end;

////////////////////////////////////////////////////////////////////////////////
//  TMorphNames
////////////////////////////////////////////////////////////////////////////////

constructor TMorphNames.Create( aPatch : TG2FilePatch);
begin
  FPatch := aPatch;
end;

destructor TMorphNames.Destroy;
begin
  inherited;
end;

procedure TMorphNames.Init;
var i : integer;
begin
  FLocation   := 2;
  FEntryCount := 1;
  FEntry      := 1;
  FLength     := 80;

  for i := 0 to 7 do
    FPatch.FPatchSettings.FMorphs[i].FMorphName := STD_MORPH_NAMES[i];
end;

procedure TMorphNames.Read( aChunk : TPatchChunk);
var i, j : integer;
    aIndex  : TBits8;
    aLength : TBits8;
    aEntry  : TBits8;
    aName : AnsiString;
    c : byte;
begin
  // FLocation       : Tbits2;
  // FEntryCount     : TBits8;
  // FEntry          : TBits8;
  // FLength         : TBits8;

  FEntryCount     := aChunk.ReadBits( 8);
  FEntry          := aChunk.ReadBits( 8);
  FLength         := aChunk.ReadBits( 8);

  for i := 0 to Length(FPatch.FPatchSettings.FMorphs) - 1 do begin
    aIndex   := aChunk.ReadBits( 8);
    aLength  := aChunk.ReadBits( 8) - 1;
    aEntry   := aChunk.ReadBits( 8);
    aName := '';
    for j := 0 to aLength - 1 do begin
      c := aChunk.ReadBits( 8);
      if c <> 0 then
        aName := aName + AnsiChar(c);
    end;
    FPatch.FPatchSettings.FMorphs[i].FMorphName := aName;
  end;

end;

procedure TMorphNames.Write( aChunk : TPatchChunk);
var i, j : integer;
begin
  aChunk.WriteBits(FEntryCount, 8);
  aChunk.WriteBits(FEntry, 8);
  aChunk.WriteBits(FLength, 8);

  for i := 0 to Length(FPatch.FPatchSettings.FMorphs) - 1 do begin
    aChunk.WriteBits(1, 8);
    aChunk.WriteBits(8, 8);
    aChunk.WriteBits(i + 8, 8);
    for j := 1 to 7 do begin
      if j <= Length(FPatch.FPatchSettings.FMorphs[i].FMorphName) then
        aChunk.WriteBits(ord(FPatch.FPatchSettings.FMorphs[i].FMorphName[j]), 8)
      else
        aChunk.WriteBits(0, 8);
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//  TParamLabel
////////////////////////////////////////////////////////////////////////////////

procedure TParamLabel.Init;
var i : integer;
begin
  for i := 0  to 6 do
    FName[i] := #0;
end;

function TParamLabel.GetName : AnsiString;
var i : integer;
begin
  Result := '';
  i := 0;
  while (i<7) and (FName[i] <> #0) do begin
    Result := Result + FName[i];
    inc(i);
  end;
end;

procedure TParamLabel.SetName( aValue : AnsiString);
var i : integer;
begin
  i := 1;
  while (i<8) and (i <= Length(aValue)) do begin
    FName[i-1] := aValue[i];
    inc(i);
  end;
  while (i<8) do begin
    FName[i-1] := #0;
    inc(i);
  end;
end;

procedure TParamLabel.Read( aChunk : TPatchChunk);
var i : integer;
begin
  for i := 0 to 6 do
    FName[i] := AnsiChar(aChunk.ReadBits( 8));
end;

procedure TParamLabel.Write( aChunk : TPatchChunk);
var i : integer;
begin
  for i := 0 to 6 do
    aChunk.WriteBits(ord(FName[i]), 8);
end;

////////////////////////////////////////////////////////////////////////////////
//  TParamLabelParam
////////////////////////////////////////////////////////////////////////////////

constructor TParamLabelParam.Create( AOwnsObjects : boolean);
begin
  inherited;
end;

constructor TParamLabelParam.CopyCreate( AOwnsObjects : boolean; aParamLabelParam : TParamLabelParam);
var i, j : integer;
    ParamLabel : TParamLabel;
begin
  inherited Create( AOwnsObjects);

  Init;
  FIsString   := aParamLabelParam.FIsString;
  FParamLen   := aParamLabelParam.FParamLen;
  FParamIndex := aParamLabelParam.FParamIndex;

  for i := 0 to aParamLabelParam.Count - 1 do begin
    ParamLabel := TParamLabel.Create;
    for j := 0 to 6 do
      ParamLabel.FName[j] := aParamLabelParam.Items[i].FName[j];
    Add( ParamLabel);
  end;
end;

destructor TParamLabelParam.Destroy;
begin
  inherited;
end;

procedure TParamLabelParam.Init;
begin
  FIsString   := 0;
  FParamLen   := 0;
  FParamIndex := 0;
  Clear;
end;

function TParamLabelParam.GetParamLabel( aIndex : integer) : TParamLabel;
begin
  result := inherited Items[aindex] as TParamLabel;
end;

procedure TParamLabelParam.SetParamLabel( aIndex : integer; const aValue : TParamLabel);
begin
  inherited Items[aindex] := aValue;
end;

function TParamLabelParam.AddParamLabel( aValue : TParamLabel): integer;
begin
  Result := inherited Add( aValue);
end;

procedure TParamLabelParam.Read( aChunk : TPatchChunk);
begin
  // FIsString   : TBits8; 0 Extra parameter value, 1 : Label
  // FParamLen   : TBits8; if label then label length + parameterindex length else 1
  // FParamIndex : TBits8; if label then parameterindex else extra parameter value
  // FParamNames : array of TParamName;
end;

procedure TParamLabelParam.Write( aChunk : TPatchChunk);
var i : integer;
begin
  if FIsString = 1 then
    FParamLen := 1 + Count * 7;

  aChunk.WriteBits(FIsString,   8);
  aChunk.WriteBits(FParamLen,   8);
  aChunk.WriteBits(FParamIndex, 8);

  if FIsString = 1 then
    for i := 0 to Count - 1 do
      Items[i].Write( aChunk);
end;

////////////////////////////////////////////////////////////////////////////////
//  TParamLabelModule
////////////////////////////////////////////////////////////////////////////////

constructor TParamLabelModule.Create( AOwnsObjects : boolean);
begin
  inherited;
end;

constructor TParamLabelModule.CopyCreate( AOwnsObjects : boolean; aParamLabelModule : TParamLabelModule);
var i : integer;
begin
  inherited Create( AOwnsObjects);

  Init;
  FModuleIndex := aParamLabelModule.FModuleIndex;
  FModuleLen   := aParamLabelModule.FModuleLen;
  for i := 0 to aParamLabelModule.Count - 1 do begin
    Add( TParamLabelParam.CopyCreate( True, aParamLabelModule.Items[i]));
  end;
end;

destructor TParamLabelModule.Destroy;
begin
  inherited;
end;

function TParamLabelModule.GetParamLabelParam( aIndex : integer) : TParamLabelParam;
begin
  result := inherited Items[aindex] as TParamLabelParam;
end;

procedure TParamLabelModule.SetParamLabelParam( aIndex : integer; const aValue : TParamLabelParam);
begin
  inherited Items[aindex] := aValue;
end;

function TParamLabelModule.AddParamLabelParam( aValue : TParamLabelParam): integer;
begin
  Result := inherited Add( aValue);
end;

procedure TParamLabelModule.Init;
begin
  FModuleIndex := 0;
  FModuleLen   := 0;
  Clear;
end;

function TParamLabelModule.FindParamLabelParam( aParamIndex : integer): TParamLabelParam;
var i : integer;
begin
  i := 0;
  while (i < Count) and not((Items[i].FIsString = 1) and (Items[i].FParamIndex = aParamIndex)) do
    inc(i);

  if (i < Count) then
    Result := Items[i]
  else
    Result := nil;
end;

function TParamLabelModule.GetModuleLabelsLength: integer;
var i : integer;
begin
  Result := 0;

  for i := 0 to Count - 1 do begin
    if Items[i].FIsString = 1 then begin
      Result := Result + 3 + 7 * Items[i].Count;
    end else
      Result := Result + 3;
  end;
end;

procedure TParamLabelModule.AddParamLabel( aParamIndex : byte; aName : AnsiString);
var ParamLabelParam : TParamLabelParam;
    ParamLabel : TParamLabel;
begin
  ParamLabelParam := FindParamLabelParam( aParamIndex);
  if assigned(ParamLabelParam) and (ParamLabelParam.Count > 0) then // TODO : there could be more than 1 label per paramindex!
    ParamLabelParam.Items[0].SetName( aName)
  else begin
    ParamLabelParam := TParamLabelParam.Create( True);
    ParamLabelParam.FIsString   := 1;
    ParamLabelParam.FParamLen   := 8;
    ParamLabelParam.FParamIndex := aParamIndex;
    ParamLabel := TParamLabel.Create;
    ParamLabel.SetName( aName);
    ParamLabelParam.Add( ParamLabel);
    Add( ParamLabelParam);
    FModuleLen := GetModuleLabelsLength;
  end;
end;

procedure TParamLabelModule.Read( aChunk : TPatchChunk);
var i, j : integer;
    //aParamLen : integer;
    ParamLabelParam : TParamLabelParam;
    ParamLabel : TParamLabel;
    parlabels_str : string;
begin
  // FModuleIndex : TBits8;
  // FModuleLen   : byte;
  // FParams      : array of TParamLabelParam;

  // This section is also used to store som extra params for SeqNote
  // extra editor parameters
  // [0, 1, mag, 0, 1, octave]
  // mag   : 0=3-octaves,1=2-octaves,2=1-octave
  // octave: 0-9 (c0-c9)

  FModuleIndex := aChunk.ReadBits( 8);
  FModuleLen   := aChunk.ReadBits( 8);

  if assigned(aChunk.FLogLines) then begin
    aChunk.FLogLines.Add( Format('%3d %3d', [FModuleIndex, FModuleLen]));
    aChunk.FLogLines.Add( '    Str Len Par Labels');
  end;

  i := FModuleLen;
  while (i > 0) do begin
    ParamLabelParam := TParamLabelParam.Create( True);
    ParamLabelParam.FIsString   := aChunk.ReadBits( 8);
    ParamLabelParam.FParamLen   := aChunk.ReadBits( 8);
    ParamLabelParam.FParamIndex := aChunk.ReadBits( 8);

    if assigned(aChunk.FLogLines) then begin
      parlabels_str := Format('    %3d %3d %3d', [ParamLabelParam.FIsString, ParamLabelParam.FParamLen, ParamLabelParam.FParamIndex]);
    end;

    Add( ParamLabelParam);

    i := i - 3; //bytes

    if ParamLabelParam.FParamLen - 1 > 0 then begin // It's a string
      for j := 0 to ((ParamLabelParam.FParamLen - 1) div 7) - 1 do begin
        ParamLabel := TParamLabel.Create;
        ParamLabel.Read( aChunk);
        ParamLabelParam.Add(ParamLabel);

        i := i - 7; //bytes
      end;
      if assigned(aChunk.FLogLines) then begin
        for j := 0 to ParamLabelParam.Count - 1 do
          parlabels_str := parlabels_str + ' ' + string(ParamLabelParam.Items[j].GetName);
      end;
    end;
    if assigned(aChunk.FLogLines) then
      aChunk.FLogLines.Add( parlabels_str);

  end;
end;


procedure TParamLabelModule.Write( aChunk : TPatchChunk);
var i : integer;
begin
  FModuleLen := GetModuleLabelsLength;

  aChunk.WriteBits(FModuleIndex, 8);
  aChunk.WriteBits(FModuleLen, 8);

  for i := 0 to Count - 1 do
    Items[i].Write( aChunk);
end;

////////////////////////////////////////////////////////////////////////////////
//  TParameterLabels
////////////////////////////////////////////////////////////////////////////////

constructor TParameterLabels.Create( AOwnsObjects : boolean);
begin
  inherited;
end;

constructor TParameterLabels.CopySelected( AOwnsObjects : boolean; aModuleList : TModuleList; aParameterLabels : TParameterLabels);
var i : integer;
    Module : TG2FileModule;
begin
  inherited Create( AOwnsObjects);

  Init;

  FLocation       := aParameterLabels.FLocation;
  FModuleCount    := 0;
  for i := 0 to aParameterLabels.FModuleCount - 1 do begin
    Module := aModuleList.FindModule( aParameterLabels.Items[i].FModuleIndex);

    if assigned(Module) {and (Module.FSelected)} then
      Add( TParamLabelModule.CopyCreate( True, aParameterLabels.Items[i]));
  end;
  FModuleCount := Count;
end;

destructor TParameterLabels.Destroy;
begin
  inherited;
end;

function TParameterLabels.GetParamLabelModule( aIndex : integer) : TParamLabelModule;
begin
  result := inherited Items[aindex] as TParamLabelModule;
end;

procedure TParameterLabels.SetParamLabelModule( aIndex : integer; const aValue : TParamLabelModule);
begin
  inherited Items[aindex] := aValue;
end;

function TParameterLabels.AddParamLabelModule( aValue : TParamLabelModule): integer;
begin
  Result := inherited Add( aValue);
end;

procedure TParameterLabels.Init;
begin
  FLocation       := 0;
  FModuleCount    := 0;
  Clear;
end;

function TParameterLabels.FindParamLabelModule( aModuleIndex : integer): TParamLabelModule;
var i : integer;
begin
  i := 0;
  while (i < Count) and (Items[i].FModuleIndex <> aModuleIndex) do
    inc(i);
  if (i < Count) then
    Result := Items[i]
  else
    Result := nil;
end;

function TParameterLabels.FindParamLabel( aModuleIndex, aParamIndex : byte): AnsiString;
var ParamLabelModule : TParamLabelModule;
    ParamLabelParam : TParamLabelParam;
begin
  ParamLabelModule := FindParamLabelModule( aModuleIndex);
  if assigned(ParamLabelModule) then begin
    ParamLabelParam := ParamLabelModule.FindParamLabelParam( aParamIndex);
    if assigned(ParamLabelParam) and (ParamLabelParam.FIsString = 1) then
      Result := ParamLabelParam.Items[0].GetName // TODO : Could be more than 1 label per paramindex
    else
      Result := '';
  end else
    Result := '';
end;

procedure TParameterLabels.AddParamLabel( aModuleIndex, aParamIndex : byte; aName : AnsiString);
var ParamLabelModule : TParamLabelModule;
begin
  ParamLabelModule := FindParamLabelModule( aModuleIndex);
  if assigned(ParamLabelModule) then
    ParamLabelModule.AddParamLabel( aParamIndex, aName)
  else begin
    ParamLabelModule := TParamLabelModule.Create( True);
    ParamLabelModule.FModuleIndex := aModuleIndex;
    ParamLabelModule.AddParamLabel( aParamIndex, aName);
    Add( ParamLabelModule);
  end;
  FModuleCount := Count;
end;

procedure TParameterLabels.DeleteParamLabel( aModuleIndex : Byte);
var i : integer;
begin
  i := 0;
  while (i < Count) and (Items[i].FModuleIndex <> aModuleIndex) do
    inc(i);

  if (i < Count) then begin
    Delete(i);
    FModuleCount := Count;
  end;
end;

procedure TParameterLabels.Read( aChunk : TPatchChunk);
var i, NumModules : integer;
    ParamLabelModule : TParamLabelModule;
begin
  // FLocation       : Tbits2;
  // FModuleCount    : TBits8
  // FModules        : array of TParamLabelModule;

  NumModules := aChunk.ReadBits( 8);

  if assigned(aChunk.FLogLines) then begin
    aChunk.FLogLines.Add('Param labels, count = ' + IntToStr(NumModules));
    aChunk.FLogLines.Add('Mod Len');
  end;

  for i := 0 to NumModules - 1 do begin
    ParamLabelModule := TParamLabelModule.Create( True);
    ParamLabelModule.Read( aChunk);

    Add( ParamLabelModule);
  end;

  FModuleCount := Count;
end;

procedure TParameterLabels.Write( aChunk : TPatchChunk);
var i : integer;
begin
  FModuleCount := Count;
  aChunk.WriteBits(FModuleCount, 8);
  for i := 0 to Count - 1 do
    Items[i].Write( aChunk);
end;

////////////////////////////////////////////////////////////////////////////////
//  TModuleLabel
////////////////////////////////////////////////////////////////////////////////

constructor TModuleLabel.Copy(aModuleLabel : TModuleLabel);
begin
  FModuleIndex := aModuleLabel.FModuleIndex;
  FName := aModuleLabel.FName;
end;

procedure TModuleLabel.Init;
begin
  FModuleIndex := 0;
  FName        := '';
end;

procedure TModuleLabel.Read( aChunk : TPatchChunk);
begin
  // FModuleIndex : TBits8;
  // FName        : AnsiString;
  FModuleIndex := aChunk.ReadBits( 8);
  FName        := aChunk.ReadName;
end;

procedure TModuleLabel.Write( aChunk : TPatchChunk);
begin
  aChunk.WriteBits(FModuleIndex, 8);
  aChunk.WriteName(FName);
end;

////////////////////////////////////////////////////////////////////////////////
//  TModuleLabels
////////////////////////////////////////////////////////////////////////////////

constructor TModuleLabels.Create( AOwnsObjects: Boolean);
begin
  inherited;
end;

destructor TModuleLabels.Destroy;
begin
  inherited;
end;

constructor TModuleLabels.CopySelected( AOwnsObjects: Boolean; aModuleList : TModuleList; aModuleLabels : TModuleLabels);
var i : integer;
    Module : TG2FileModule;
begin
  inherited Create( AOwnsObjects);

  Init;
  FLocation := aModuleLabels.FLocation;
  FUnknown := aModuleLabels.FUnknown;
  FNameCount := 0;
  for i := 0 to aModuleLabels.FNameCount - 1 do begin
    Module := aModuleList.FindModule( aModuleLabels.Items[i].FModuleIndex);

    if assigned(Module) {and ( Module.FSelected)} then begin
      Add( TModuleLabel.Copy( aModuleLabels.Items[i]));
    end;
  end;
  FNameCount := Count;
end;

function TModuleLabels.GetModuleLabel( aIndex : integer) : TModuleLabel;
begin
  result := inherited Items[aindex] as TModuleLabel;
end;

procedure TModuleLabels.SetModuleLabel( aIndex : integer; const aValue : TModuleLabel);
begin
  inherited Items[aindex] := aValue;
end;

function TModuleLabels.AddModuleLabel( aValue : TModuleLabel): integer;
begin
  Result := inherited Add( aValue);
end;

procedure TModuleLabels.Init;
begin
  FLocation  := 0;
  FUnknown   := 0;
  FNameCount := 0;
  Clear;
end;

function TModuleLabels.FindModuleLabel(aModuleIndex: Byte): AnsiString;
var i : integer;
begin
  i := 0;
  while (i < Count) and (Items[i].FModuleIndex <> aModuleIndex) do
    inc(i);

  if (i < Count) then begin
    Result := Items[i].FName
  end else
    Result := '';
end;

procedure TModuleLabels.AddNewModuleLabel( aModuleIndex : byte; aName : AnsiString);
var i : integer;
    ModuleLabel : TModuleLabel;
begin
  i := 0;
  while (i < Count) and (Items[i].FModuleIndex <> aModuleIndex) do
    inc(i);

  if (i < Count) then begin
    Items[i].FName := aName;
  end else begin
    ModuleLabel:= TModuleLabel.Create;
    ModuleLabel.FModuleIndex := aModuleIndex;
    ModuleLabel.FName := aName;
    Add( ModuleLabel);
  end;
  FNameCount := Count;
end;

procedure TModuleLabels.DeleteModuleLabel( aModuleIndex : Byte);
var i : integer;
begin
  i := 0;
  while (i < Count) and (Items[i].FModuleIndex <> aModuleIndex) do
    inc(i);

  if (i < Count) then begin
    Delete(i);
    FNameCount := Count;
  end;
end;

procedure TModuleLabels.Read( aChunk : TPatchChunk);
var NumNames, i : integer;
    ModuleLabel : TModuleLabel;
begin
  // FLocation    : TBits2;
  // FUnknown     : TBits6;
  // FNameCount   : TBits8
  // FModuleNames : array of TModuleName;

  FUnknown     := aChunk.ReadBits( 6);
  NumNames     := aChunk.ReadBits( 8);

  if assigned(aChunk.FLogLines) then begin
    aChunk.FLogLines.Add('Module labels, count = ' + IntToStr(NumNames));
    aChunk.FLogLines.Add('Mod Label');
  end;

  for i := 0 to NumNames - 1 do begin
    ModuleLabel := TModuleLabel.Create;
    ModuleLabel.Read( aChunk);

    if assigned( aChunk.FLogLines) then
      aChunk.FLogLines.Add( Format('%3d', [ModuleLabel.FModuleIndex]) + ' ' + string(ModuleLabel.FName));

    Add( ModuleLabel);
  end;
  FNameCount := Count;
end;

procedure TModuleLabels.Write( aChunk : TPatchChunk);
var i : integer;
begin
  FNameCount := Count;
  aChunk.WriteBits({FUnknown}0, 6);
  aChunk.WriteBits(FNameCount, 8);
  for i := 0 to Count - 1 do
    Items[i].Write( aChunk);
end;

////////////////////////////////////////////////////////////////////////////////
//  TPatchDescription
////////////////////////////////////////////////////////////////////////////////

procedure TPatchDescription.Init;
var i : integer;
begin
  for i := 0 to 6 do
    FUnknown1[ i]  := 0;

  FUnknown2         := 0;
  FVoiceCount       := 1;
  FBarPosition      := 600;
  FUnknown3         := 2;
  FRedVisible       := 1;
  FBlueVisible      := 1;
  FYellowVisible    := 1;
  FOrangeVisible    := 1;
  FGreenVisible     := 1;
  FPurpleVisible    := 1;
  FWhiteVisible     := 1;
  FMonoPoly         := 1;
  FActiveVariation  := 0;
  FCategory         := 0;
end;

procedure TPatchDescription.Read( aChunk : TPatchChunk);
var i : integer;
begin
  // FUnknown1        : TArray7;
  // FUnknown2        : TBits5;
  // FVoiceCount      : TBits5;
  // FBarPosition     : TBits14;
  // FUnknown3        : TBits3;
  // FRedVisible      : TBits1;
  // FBlueVisible     : TBits1;
  // FYellowVisible   : TBits1;
  // FOrangeVisible   : TBits1;
  // FGreenVisible    : TBits1;
  // FPurpleVisible   : TBits1;
  // FWhiteVisible    : TBits1;
  // FMonoPoly        : TBits2;
  // FActiveVariation : Byte;
  // FCategory        : Byte;
  for i := 0 to 6 do
    FUnknown1[i] := aChunk.ReadBits( 8);
  FUnknown2        := aChunk.ReadBits( 5);
  FVoiceCount      := aChunk.ReadBits( 5);
  FBarPosition     := aChunk.ReadBits( 14);
  FUnknown3        := aChunk.ReadBits( 3);
  FRedVisible      := aChunk.ReadBits( 1);
  FBlueVisible     := aChunk.ReadBits( 1);
  FYellowVisible   := aChunk.ReadBits( 1);
  FOrangeVisible   := aChunk.ReadBits( 1);
  FGreenVisible    := aChunk.ReadBits( 1);
  FPurpleVisible   := aChunk.ReadBits( 1);
  FWhiteVisible    := aChunk.ReadBits( 1);
  FMonoPoly        := aChunk.ReadBits( 2);
  FActiveVariation := aChunk.ReadBits( 8);
  FCategory        := aChunk.ReadBits( 8);
  FUnknown4        := aChunk.ReadBits( 12); // To byte align again
end;

procedure TPatchDescription.Write( aChunk : TPatchChunk);
var i : integer;
begin
  for i := 0 to 6 do
     aChunk.WriteBits({FUnknown1[i]}0, 8);

  aChunk.WriteBits({FUnknown2}0, 5);
  aChunk.WriteBits(FVoiceCount, 5);
  aChunk.WriteBits(FBarPosition, 14);
  aChunk.WriteBits(FUnknown3, 3);
  aChunk.WriteBits(FRedVisible, 1);
  aChunk.WriteBits(FBlueVisible, 1);
  aChunk.WriteBits(FYellowVisible, 1);
  aChunk.WriteBits(FOrangeVisible, 1);
  aChunk.WriteBits(FGreenVisible, 1);
  aChunk.WriteBits(FPurpleVisible, 1);
  aChunk.WriteBits(FWhiteVisible, 1);
  aChunk.WriteBits(FMonoPoly, 2);
  aChunk.WriteBits(FActiveVariation, 8);
  aChunk.WriteBits(FCategory, 8);
  aChunk.WriteBits(FUnknown4, 12); // To byte align again
end;

////////////////////////////////////////////////////////////////////////////////
//  TPatchNotes
////////////////////////////////////////////////////////////////////////////////

constructor TPatchNotes.Create;
begin
  SetLength(FText, 0);
end;

destructor TPatchNotes.Destroy;
begin
  Finalize(FText);

  inherited;
end;

procedure TPatchNotes.Init;
begin
  SetLength(FText, 0);
end;

procedure TPatchNotes.Read( aChunk : TPatchChunk);
begin
  aChunk.FReadBuffer.Read(FText[0], Length(FText));
end;

procedure TPatchNotes.Write( aChunk : TPatchChunk);
begin
  aChunk.FWriteBuffer.Write(FText[0], Length(FText));
end;

////////////////////////////////////////////////////////////////////////////////
//  TG2FilePatchPart
////////////////////////////////////////////////////////////////////////////////

constructor TG2FilePatchPart.Create( aPatch : TG2FilePatch);
begin
  inherited Create( aPatch);

  FPatch := aPatch;
  FModuleList := TModuleList.Create( True, self);
  FCableList := TCableList.Create( True, self);
  FParameterList := TModuleParameters.Create( True{, aPatch});
  FModuleLabels := TModuleLabels.Create( True);
  FParameterLabels := TParameterLabels.Create(True);
end;

constructor TG2FilePatchPart.CopyModules( aPatch : TG2FilePatch; aSrcePatchPart : TG2FilePatchPart; aModuleList : TModuleList);
begin
  inherited Create( aPatch);

  FPatch := aPatch;
  FModuleList := TModuleList.CopySelected( True, self, aModuleList);
  FCableList := TCableList.CopySelected( True, self, aModuleList, aSrcePatchPart.FCableList);
  FParameterList := TModuleParameters.CopySelected( True, {FPatch,} aModuleList, aSrcePatchPart.FParameterList);
  FModuleLabels := TModuleLabels.CopySelected( True, aModuleList, aSrcePatchPart.FModuleLabels);
  FParameterLabels := TParameterLabels.CopySelected( True, aModuleList, aSrcePatchPart.FParameterLabels);
end;

destructor TG2FilePatchPart.Destroy;
begin
  FParameterLabels.Free;
  FModuleLabels.Free;
  FParameterList.Free;
  FCableList.Free;
  FModuleList.Free;
  inherited;
end;

procedure TG2FilePatchPart.Init;
begin
  // TODO : Remove params from Global knob list!

  FModuleList.Init;
  FCableList.Init;
  FParameterList.Init;
  FModuleLabels.Init;
  FParameterLabels.Init;
end;

procedure TG2FilePatchPart.SetLocation( aValue : TLocationType);
begin
  FLocation := aValue;

  FModuleList.FLocation := ord(FLocation);
  FCableList.FLocation   := ord(FLocation);
  FParameterList.FLocation := ord(FLocation);
  FModuleLabels.FLocation := ord(FLocation);
  FParameterLabels.FLocation := ord(FLocation);
end;

procedure TG2FilePatchPart.DeleteModule( aModuleIndex : integer);
begin
  FParameterLabels.DeleteParamLabel( aModuleIndex);
  FModuleLabels.DeleteModuleLabel( aModuleIndex);
  FParameterList.DeleteModule( aModuleIndex);
  FModuleList.DeleteModule( aModuleIndex);
end;

procedure TG2FilePatchPart.AddCable( aCable : TG2FileCable);
begin
  FCableList.AddCable( aCable);
end;

procedure TG2FilePatchPart.DeleteCable( aCable : TG2FileCable);
begin
  FCableList.DeleteCable( aCable);
end;

function TG2FilePatchPart.GetMaxModuleIndex : integer;
begin
  Result := FModuleList.GetMaxModuleIndex;
end;

function TG2FilePatchPart.GetNoOffModuleType( aModuleType : byte): integer;
begin
  Result := FModuleList.GetNoOffModuleType( aModuleType);
end;

function TG2FilePatchPart.FindModuleLabel( aModuleIndex : byte): AnsiString;
begin
  Result := FModuleLabels.FindModuleLabel( aModuleIndex);
end;

function TG2FilePatchPart.FindParamValue( aModuleIndex, aVariation, aParamIndex : byte): integer;
begin
  Result := FParameterList.FindParamValue( aModuleIndex, aVariation, aParamIndex);
end;

function TG2FilePatchPart.FindParamSet( aModuleIndex: byte): TParamSet;
begin
  Result := FParameterList.FindParamSet( aModuleIndex);
end;

function TG2FilePatchPart.FindModule( aModuleIndex : byte): TG2FileModule;
begin
  Result := FModuleList.FindModule( aModuleIndex);
end;

procedure TG2FilePatchPart.AddNewVariation;
begin
  FParameterList.AddNewVariation;
end;

procedure TG2FilePatchPart.CopyVariation( aFromVariation, aToVariation : byte);
begin
  FParameterList.CopyVariation( aFromVariation, aToVariation);
end;

function TG2FilePatchPart.FindParamLabel( aModuleIndex, aParamIndex : byte): AnsiString;
begin
  Result := FParameterLabels.FindParamLabel( aModuleIndex, aParamIndex);
end;

procedure TG2FilePatchPart.AddParamLabel(aModuleIndex, aParamIndex : byte; aValue : AnsiString);
begin
  FParameterLabels.AddParamLabel(aModuleIndex, aParamIndex, aValue);
end;

procedure TG2FilePatchPart.AddNewModuleLabel(aModuleIndex : byte; aValue : AnsiString);
begin
  FModuleLabels.AddNewModuleLabel( aModuleIndex, aValue);
end;

function TG2FilePatchPart.GetParameterLabel( aModuleIndex, aParamIndex : byte): AnsiString;
begin
  Result := FindParamLabel( aModuleIndex, aParamIndex)
end;

procedure TG2FilePatchPart.SetParameterLabel( aModuleIndex, aParamIndex : byte; aValue : AnsiString);
begin
  AddParamLabel(aModuleIndex, aParamIndex, aValue);
end;

function TG2FilePatchPart.GetModuleLabel( aModuleIndex : byte): AnsiString;
var Module : TG2FileModule;
begin
  Result := FindModuleLabel( aModuleIndex);
  if Result = '' then begin
    Module := FindModule( aModuleIndex);
    if Module <> nil then
      Result := Module.ModuleName
    else
      Result := '';
  end;
end;

procedure TG2FilePatchPart.SetModuleLabel( aModuleIndex : byte; aValue : AnsiString);
begin
    AddNewModuleLabel( aModuleIndex, aValue);
end;

procedure TG2FilePatchPart.SelectModule( aModule : TG2FileModule);
begin
  if assigned(FPatch) then
    FPatch.SelectModule( aModule);

end;

procedure TG2FilePatchPart.DeselectModule( aModule : TG2FileModule);
begin
  if assigned(FPatch) then
    FPatch.DeselectModule( aModule);
end;

function TG2FilePatchPart.CreateModule( aModuleIndex : byte; aModuleType : byte): TG2FileModule;
begin
  if assigned(FPatch) then
    Result := FPatch.CreateModule( FLocation, aModuleIndex, aModuleType)
  else
    raise Exception.Create('Patch not assigned to patchpart');
end;

function TG2FilePatchPart.CreateCable( aColor : byte; aFromModule : byte; aFromConnector : byte; aLinkType : byte; aToModule : byte; aToConnector : byte): TG2FileCable;
begin
  if assigned(FPatch) then
    Result := FPatch.CreateCable( FLocation, aColor, aFromModule, aFromConnector, aLinkType, aToModule, aToConnector)
  else
    raise Exception.Create('Patch not assigned to patchpart');
end;

////////////////////////////////////////////////////////////////////////////////
//  TG2FilePatch
////////////////////////////////////////////////////////////////////////////////

constructor TG2FilePatch.Create( AOwner: TComponent);
var i : integer;
begin
  inherited Create( AOwner);

  FG2 := nil;

  //FMaxVariations := 9; // The amount of variations in the patch: file 9, usb 10!

  for i := 0 to 1 do // FX and VA
    FPatchPart[i] := TG2FilePatchPart.Create( Self);

  FCurrentNote := TCurrentNote.Create( True);
  FPatchSettings := TPatchSettings.Create;
  FPatchDescription := TPatchDescription.Create;
  FMorphParameters := TMorphParameters.Create(self);
  FKnobList := TKnobList.Create( True);
  FControllerList := TControllerList.Create( True);
  FMorphNames := TMorphNames.Create(self);
  FPatchNotes := TPatchNotes.Create;

  FSelectedModuleList := TModuleList.Create( False, nil);

  Init;
end;

constructor TG2FilePatch.CopyModules( AOwner: TComponent; aPatch : TG2FilePatch; aModuleList : TModuleList);
var i : integer;
begin
  inherited Create( AOwner);

  //FMaxVariations := aPatch.FMaxVariations;

  FSelectedModuleList := TModuleList.Create( False, nil);

  for i := 0 to 1 do
    FPatchPart[i] := TG2FilePatchPart.CopyModules( self, aPatch.FPatchPart[i],  aModuleList);

  FCurrentNote := TCurrentNote.Create( True);
  FPatchSettings := TPatchSettings.Create;
  FPatchDescription := TPatchDescription.Create;
  FMorphParameters := TMorphParameters.Create(self);
  FKnobList := TKnobList.Create( True);
  FControllerList := TControllerList.Create( True);
  FMorphNames := TMorphNames.Create(self);
  FPatchNotes := TPatchNotes.Create;
end;

destructor TG2FilePatch.Destroy;
var i : integer;
begin
  for i := 0 to Length(FParams) - 1 do
    FParams[i].Free;
  Finalize(FParams);


  FPatchNotes.Free;
  FMorphNames.Free;
  FControllerList.Free;
  FKnobList.Free;
  FPatchDescription.Free;
  FPatchSettings.Free;
  FMorphParameters.Free;
  FCurrentNote.Free;
  for i := 0 to 1 do
    FPatchPart[i].Free;

  FSelectedModuleList.Free;

  inherited;
end;

procedure TG2FilePatch.Init;
var i : integer;
begin
  FVersion   := 23;
  FPatchType := 0;

  FPatchDescription.Init;

  for i := 0 to 1 do begin
    FPatchPart[i].Init;
    FPatchPart[i].Location := TLocationType(i);
  end;

  FCurrentNote.Init;

  FPatchSettings.Init;
  FPatchSettings.FLocation := 2;

  FMorphParameters.Init;

  FKnobList.Init;

  FControllerList.Init;

  FMorphNames.Init;
  FMorphNames.FLocation := 2;

  FPatchNotes.Init;

  for i := 0 to Length(FParams) - 1 do
    FParams[i].Free;
  SetLength(FParams, 0);

  FSelectedModuleList.Clear;

  FEditAllVariations := False;
  FSelectedParam := nil;
end;


procedure TG2FilePatch.SetPatchName( aValue : AnsiString);
begin
  if assigned(FSlot) then
    FSlot.FPatchName := aValue;
end;

procedure TG2FilePatch.SetG2( aValue : TG2File);
begin
  FG2 := aValue;
end;

procedure TG2FilePatch.SetPerformance( aValue : TG2FilePerformance);
begin
  FPerformance := aValue;
end;

procedure TG2FilePatch.SetSlot( aValue : TG2FileSlot);
begin
  FSlot := aValue;
end;

procedure TG2FilePatch.SetSelectedParam(aValue: TG2FileParameter);
var OldSelectedParam : TG2FileParameter;
begin
  OldSelectedParam := SelectedParam;
  FSelectedParam := aValue;
  if assigned(SelectedParam) then
    SelectedParam.InvalidateControl;

  if assigned(OldSelectedParam) then
    OldSelectedParam.InvalidateControl;
end;

procedure TG2FilePatch.InitKnobs;
var i : integer;
    Knob : TKnob;
    Module : TG2FileModule;
    Param : TG2FileParameter;
begin
  for i := 0 to FKnobList.Count - 1 do begin
    Knob := FKnobList.Items[i];
    if Knob.Fassigned = 1 then begin
      case TLocationType(Knob.FLocation) of
      ltPatch : begin
                  Param := Parameter[ Knob.ModuleIndex, Knob.FParamIndex];
                  if assigned(Param) then
                    Param.AssignKnob( i);
                end;
      ltFX,
      ltVA    : begin
                  Module := GetModule( Knob.FLocation, Knob.FModuleIndex);
                  if assigned(Module) then begin
                    Param := Module.Parameter[ Knob.FParamIndex];
                    if assigned(Param) then
                      Param.AssignKnob( i);
                  end;
                end;
      end;
    end;
  end;
end;

procedure TG2FilePatch.InitNames;
var m, l, Count : integer;
    aName : AnsiString;
    Module : TG2FileModule;
begin
  for l := 0 to 1 do begin
    Count := ModuleCount[ l];
    for m := 0 to Count - 1 do begin
      Module := Modules[ l, m];
      if assigned(Module) then begin
        aName := GetModuleName( TLocationType(l), Module.ModuleIndex);
        if aName <> '' then
          Module.FModuleName := aName;
      end;
    end;
  end;
end;

procedure TG2FilePatch.InitParameters;
var i : integer;

  procedure AddParam( ModuleIndex : byte; ModuleName : AnsiString; ParamIndex : byte; ParamName : AnsiString; LowValue, HighValue, DefaultValue: byte);
  begin
    i := Length(FParams);
    SetLength(FParams, i + 1);
    FParams[i] := CreateParameter( ModuleIndex);
    FParams[i].InitParam( ModuleName, ParamIndex, ptParam, ParamName, '', LowValue, HighValue, DefaultValue, -1, -1, '');
  end;

begin
  for i := 0 to Length(FParams) - 1 do
    FParams[i].Free;

  SetLength(FParams, 0);

  AddParam( PATCH_VOLUME,      'Vol',      VOLUME_LEVEL,  'VolLevel',   0, 127, 100);
  AddParam( PATCH_VOLUME,      'Vol',      VOLUME_MUTE,   'VolMute',    0,   1,   0);
  AddParam( PATCH_GLIDE,       'Glide',    GLIDE_TYPE,    'GlideType',  0,   2,   0);
  AddParam( PATCH_GLIDE,       'Glide',    GLIDE_SPEED,   'GlideSpeed', 0, 127,   0);
  AddParam( PATCH_BEND,        'Bend',     BEND_ON_OFF,   'BendOnOff',  0,   1,   0);
  AddParam( PATCH_BEND,        'Bend',     BEND_RANGE,    'BendRange',  0, 127,   0);
  AddParam( PATCH_VIBRATO,     'Vibrato',  VIBRATO_MOD,   'VibrMod',    0,   2,   0);
  AddParam( PATCH_VIBRATO,     'Vibrato',  VIBRATO_DEPTH, 'VibrDepth',  0, 127,   0);
  AddParam( PATCH_VIBRATO,     'Vibrato',  VIBRATO_RATE,  'VibrRate',   0, 127,   0);
  AddParam( PATCH_ARPEGGIATOR, 'Arp',      ARP_ON_OFF,    'ArpOnOff',   0,   1,   0);
  AddParam( PATCH_ARPEGGIATOR, 'Arp',      ARP_SPEED,     'ArpSpeed',   0,   3,   0);
  AddParam( PATCH_ARPEGGIATOR, 'Arp',      ARP_DIRECTION, 'ArpDir',     0,   3,   0);
  AddParam( PATCH_ARPEGGIATOR, 'Arp',      ARP_OCTAVES,   'ArpOct',     0,   3,   0);
  AddParam( PATCH_SUSTAIN,     'Sustain',  SUSTAIN_PEDAL, 'Sustain',    0,   1,   0);
  AddParam( PATCH_SUSTAIN,     'Oct Shft', OCTAVE_SHIFT,  'Oct.Shft',   0,   0,   0);

  AddParam( PATCH_MORPH,       'Morph',    0,             'Wheel',      0,   127,   0);
  AddParam( PATCH_MORPH,       'Morph',    1,             'Vel',        0,   127,   0);
  AddParam( PATCH_MORPH,       'Morph',    2,             'Keyb',       0,   127,   0);
  AddParam( PATCH_MORPH,       'Morph',    3,             'Aft.Tch',    0,   127,   0);
  AddParam( PATCH_MORPH,       'Morph',    4,             'Sust.Pd',    0,   127,   0);
  AddParam( PATCH_MORPH,       'Morph',    5,             'Ctrl.Pd',    0,   127,   0);
  AddParam( PATCH_MORPH,       'Morph',    6,             'P.Stick',    0,   127,   0);
  AddParam( PATCH_MORPH,       'Morph',    7,             'G.Wh 2',     0,   127,   0);

  AddParam( PATCH_MORPH,       'Morph',    8,             'Wheel',      0,   1,   0);
  AddParam( PATCH_MORPH,       'Morph',    9,             'Vel',        0,   1,   0);
  AddParam( PATCH_MORPH,       'Morph',    10,            'Keyb',       0,   1,   0);
  AddParam( PATCH_MORPH,       'Morph',    11,            'Aft.Tch',    0,   1,   0);
  AddParam( PATCH_MORPH,       'Morph',    12,            'Sust.Pd',    0,   1,   0);
  AddParam( PATCH_MORPH,       'Morph',    13,            'Ctrl.Pd',    0,   1,   0);
  AddParam( PATCH_MORPH,       'Morph',    14,            'P.Stick',    0,   1,   0);
  AddParam( PATCH_MORPH,       'Morph',    15,            'G.Wh 2',     0,   1,   0);

  // Module params should already be initialized;
  InitKnobs;
end;

procedure TG2FilePatch.Read( aChunk : TPatchChunk);
var einde : boolean;
begin
  einde := False;
  repeat
    if not ReadChunk( aChunk) then begin
      einde := True;
      raise Exception.Create('Error parsing patch data.');
    end else
      if aChunk.FId = C_PATCH_NOTES then
        einde := true; // assuming this is always the last chunk in a patch

    if not einde then begin
      if aChunk.FStream.Position < aChunk.FStream.Size - 2 then
        aChunk.ReadChunk
      else
        einde := true;
    end;
  until einde;

  InitParameters;
  InitNames;
  SortLeds;
end;

function TG2FilePatch.ReadChunk( aChunk : TPatchChunk): boolean;
var  Location : byte;
begin
  Result := True;
  case aChunk.FId of
  C_PATCH_DESCR :
        begin // Patch description
          FPatchDescription.read( aChunk);
        end ;
  C_MODULE_LIST :
        begin // Module list
          location := aChunk.ReadBits( 2);
          FPatchPart[ location].FModuleList.Read( aChunk);
        end ;
  C_CURRENT_NOTE_2 :
        begin // Current note
          FCurrentNote.Read( aChunk);
        end ;
  C_CABLE_LIST :
        begin // Cable list
          location := aChunk.ReadBits( 2);
          FPatchPart[ location].FCableList.Read( aChunk)
        end ;
  C_PARAM_LIST :
        begin // Parameter list
          location := aChunk.ReadBits( 2);
          case location of
          0..1 : FPatchPart[ location].FParameterList.Read( aChunk);
          2    : FPatchsettings.read( aChunk);
          end;
        end ;
  C_MORPH_PARAM :
        begin // Morph parameter list
          FMorphParameters.Read( aChunk);
        end ;
  C_KNOBS :
        begin // Knob assignments
          FKnobList.Read( aChunk);
        end ;
  C_CONTROLLERS :
        begin // Controller assignments
          FControllerList.Read( aChunk);
        end ;
  C_PARAM_NAMES :
        begin // Parmeter names
          location := aChunk.ReadBits( 2);
          case location of
          0..1 : FPatchPart[ location].FParameterLabels.Read( aChunk);
          2    : FMorphNames.Read( aChunk);
          end;
        end ;
  C_MODULE_NAMES :
        begin // Module names
          location := aChunk.ReadBits( 2);
          FPatchPart[ location].FModuleLabels.Read( aChunk);
        end ;
  C_PATCH_NOTES :
        begin // Patch notes
          SetLength(FPatchNotes.FText, aChunk.FSize);
          FPatchNotes.Read( aChunk);
        end ;
  S_SEL_PARAM_PAGE :
        begin // Sel parameter page
          aChunk.FStream.Position := aChunk.FStream.Position - 1; // This seems to be there only in patches that come from usb
        end;
    else begin
      Result := False;
    end;
  end;
end;

procedure TG2FilePatch.Write( aChunk : TPatchChunk; aVariationCount : byte);
begin
  while FPatchSettings.FVariationCount < aVariationCount do
    AddVariation;

  FPatchDescription.Write( aChunk);
  aChunk.WriteChunk( C_PATCH_DESCR);

  aChunk.WriteBits( LOCATION_VA, 2);
  FPatchPart[ ord(ltVA)].FModuleList.Write( aChunk);
  aChunk.WriteChunk( C_MODULE_LIST);

  aChunk.WriteBits( LOCATION_FX, 2);
  FPatchPart[ ord(ltFX)].FModuleList.Write( aChunk);
  aChunk.WriteChunk( C_MODULE_LIST);

  FCurrentNote.Write( aChunk);
  aChunk.WriteChunk( C_CURRENT_NOTE_2);

  aChunk.WriteBits( LOCATION_VA, 2);
  FPatchPart[ ord(ltVA)].FCableList.Write( aChunk);
  aChunk.WriteChunk( C_CABLE_LIST);

  aChunk.WriteBits( LOCATION_FX, 2);
  FPatchPart[ ord(ltFX)].FCableList.Write( aChunk);
  aChunk.WriteChunk( C_CABLE_LIST);

  aChunk.WriteBits( LOCATION_PATCH_SETTINGS, 2);
  FPatchsettings.Write( aChunk, aVariationCount);
  aChunk.WriteChunk( C_PARAM_LIST);

  aChunk.WriteBits( LOCATION_VA, 2);
  FPatchPart[ ord(ltVA)].FParameterList.Write( aChunk, aVariationCount);
  aChunk.WriteChunk( C_PARAM_LIST);

  aChunk.WriteBits( LOCATION_FX, 2);
  FPatchPart[ ord(ltFX)].FParameterList.Write( aChunk, aVariationCount);
  aChunk.WriteChunk( C_PARAM_LIST);

  FMorphParameters.Write( aChunk, aVariationCount);
  aChunk.WriteChunk( C_MORPH_PARAM);

  FKnobList.Write( aChunk);
  aChunk.WriteChunk( C_KNOBS);

  FControllerList.Write( aChunk);
  aChunk.WriteChunk( C_CONTROLLERS);

  aChunk.WriteBits( LOCATION_PATCH_SETTINGS, 2);
  FMorphNames.Write( aChunk);
  aChunk.WriteChunk( C_PARAM_NAMES);

  aChunk.WriteBits( LOCATION_VA, 2);
  FPatchPart[ ord(ltVA)].FParameterLabels.Write( aChunk);
  aChunk.WriteChunk( C_PARAM_NAMES);

  aChunk.WriteBits( LOCATION_FX, 2);
  FPatchPart[ ord(ltFX)].FParameterLabels.Write( aChunk);
  aChunk.WriteChunk( C_PARAM_NAMES);

  aChunk.WriteBits( LOCATION_VA, 2);
  FPatchPart[ ord(ltVA)].FModuleLabels.Write( aChunk);
  aChunk.WriteChunk( C_MODULE_NAMES);

  aChunk.WriteBits( LOCATION_FX, 2);
  FPatchPart[ ord(ltFX)].FModuleLabels.Write( aChunk);
  aChunk.WriteChunk( C_MODULE_NAMES);

  FPatchNotes.Write( aChunk);
  aChunk.WriteChunk( C_PATCH_NOTES);
end;

function TG2FilePatch.LoadFromFile(aStream : TStream; aLogLines : TStrings): Boolean;
var
  sl : TStringList;
  Chunk : TPatchChunk;
  s  : AnsiString;
  c  : AnsiChar;
  bm, bl : Byte;
  Crc : Word;
begin
  Result := False;

  Chunk := TPatchChunk.Create( aStream);
  sl := TStringList.Create;
  try
    if assigned( aLogLines) then
      Chunk.FLogLines := aLogLines;

    s := '';
    aStream.Position := 0;
    while aStream.Position < aStream.Size do begin
      aStream.Read( c, SizeOf( c));
      case c of
      #$0d :; // just skip it
      #$0a : begin
               sl.Add(string(s));
               s := '';
             end;
      #$00 : Break;
      else
        s := s + c;
      end;
    end;
    if s <> '' then
      sl.Add(string(s));

    Init;
    Chunk.ReadBuffer( 2);
    FVersion := Chunk.ReadBits( 8);
    FPatchType := Chunk.ReadBits( 8);
    Chunk.ReadChunk;
    Read( Chunk);

    aStream.Read(bm, 1);
    aStream.Read(bl, 1);
    Crc := bm * 256 + bl;

    if Crc <> Chunk.FReadCrc then
      raise Exception.Create('Crc error.');

    Result := True;

  finally
    Chunk.Free;
    sl.Free;
  end;
end;

class function TG2FilePatch.LoadFileStream( AOwner: TComponent; aStream : TStream; aChunk : TPatchChunk): TG2FileDataStream;
begin
  Result := TG2FilePatch.Create( AOwner);
  aChunk.ReadChunk;
  Result.Read( aChunk);
end;

procedure TG2FilePatch.SaveToFile( aStream : TStream);
var sl : TStringList;
    i : integer;
    c : AnsiChar;
    s : AnsiString;
    Chunk : TPatchChunk;
begin
  sl := TStringList.Create;
  Chunk := TPatchChunk.Create( aStream);
  try
    sl.Clear;
    sl.Add('Version=Nord Modular G2 File Format 1');
    sl.Add('Type=Patch');
    sl.Add('Version=23');
    sl.Add('Info=BUILD 320');

    aStream.Position := 0;

    for i := 0 to sl.Count - 1 do
    begin
      s := AnsiString(sl[i]);
      aStream.Write( s[1], Length( s));
      c := #$0d;
      aStream.Write( c, SizeOf( c));
      c := #$0a;
      aStream.Write( c, SizeOf( c));
    end;

    c := #0;
    aStream.Write( c, SizeOf( c));

    Chunk.WriteBits(FVersion, 8);
    Chunk.WriteBits(FPatchtype, 8);
    Chunk.Flush;

    //MaxVariations := 9;
    Write( Chunk, 9); // 9 Variations are written to file
    Chunk.WriteCrc(aStream);

  finally
    Chunk.Free;
    sl.Free;
  end;
end;

procedure TG2FilePatch.SaveAsFXP( aStream : TStream);
var FXPHeader : TFXPHeader;
    MemStream : TMemoryStream;
    i : integer;

    function SwapBytes(Value: Cardinal): Cardinal; register;
    asm
      BSWAP  EAX
    end;
{    begin
      // TDOD
    end;}

begin
  MemStream := TMemoryStream.Create;
  try
    SaveToFile(MemStream);

    FXPHeader.chunkMagic  := 'CcnK';
    FXPHeader.byteSize    := SwapBytes(52 + MemStream.Size);
    FXPHeader.fxMagic     := 'FPCh';
    FXPHeader.version     := SwapBytes(1);
    FXPHeader.fxID        := 'NMG2';
    FXPHeader.fxVersion   := SwapBytes(1);
    FXPHeader.numPrograms := SwapBytes(1);
    Fillchar(FXPHeader.Name, 28, #0);
    for i := 1 to Length(PatchName) do
      FXPHeader.name[i] := ord(PatchName[i]);

    FXPHeader.chunkSize   := SwapBytes(MemStream.Size);

    aStream.Write(FXPHeader, SizeOf(FXPHeader));
    aStream.Write(MemStream.Memory^, MemStream.Size);

  finally
    MemStream.Free;
  end;
end;

function TG2FilePatch.CreateModule( aLocation : TLocationType; aModuleIndex : byte; aModuleType : byte): TG2FileModule;
var i : LongWord;
begin
  Result := TG2FileModule.Create( FPatchPart[ ord(aLocation)]);
  Result.FModuleIndex := aModuleIndex;
  Result.FTypeID := aModuleType;

  if assigned( G2) and assigned(G2.FModuleDefList) and assigned(G2.FParamDefList) then begin
    i := 0;
    while (i < G2.FModuleDefList.Count) and ( G2.FModuleDefList.ModuleDef[i].ModuleType <> aModuleType) do
      inc(i);

    if (i < G2.FModuleDefList.Count) then begin
      Result.InitModule( aLocation, G2.FModuleDefList.ModuleDef[i], G2.FParamDefList);
    end else
      raise Exception.Create('Unknown module type ' + IntToStr( aModuleType));;
  end;

  Result.Location := aLocation;

  if assigned(G2) and assigned(G2.OnCreateModule) then
    G2.OnCreateModule(self, G2.ID, Result);
end;

function TG2FilePatch.CreateCable( aLocation : TLocationType; aColor : byte; aFromModule : byte; aFromConnector : byte; aLinkType : byte; aToModule : byte; aToConnector : byte): TG2FileCable;
begin
  Result                := TG2FileCable.Create( self);
  Result.FCableColor    := aColor;
  Result.FModuleFrom    := aFromModule;
  Result.FConnectorFrom := aFromConnector;
  Result.FLinkType      := aLinkType;
  Result.FModuleTo      := aToModule;
  Result.FConnectorTo   := aToConnector;
end;

function TG2FilePatch.CreateParameter( aModuleIndex : byte): TG2FileParameter;
begin
  Result := TG2FileParameter.Create( self, ltPatch, aModuleIndex);
end;

procedure TG2FilePatch.AddModuleToPatch( aLocation : TLocationType; aModule: TG2FileModule);
begin
  FPatchPart[ ord(aLocation)].FModuleList.AddModule( aModule);
  aModule.Selected := True;
end;

procedure TG2FilePatch.DeleteModuleFromPatch( aLocation : TLocationType; aModule: TG2FileModule);
var aModuleIndex : Byte;
begin
  DeselectModule( aModule);

  aModuleIndex := aModule.ModuleIndex;
  FControllerList.DeleteModule( aModuleIndex);
  FKnobList.DeleteModule( aModuleIndex, ord(aLocation));

  if assigned(FPerformance) and assigned(FSlot) then
    FPerformance.DeleteModuleFromPerf( FSlot.SlotIndex, aLocation, aModule);

  FPatchPart[ord(aLocation)].DeleteModule( aModuleIndex);
end;

procedure TG2FilePatch.SelectModule(aModule: TG2FileModule);
begin
  if not assigned(aModule) then
    exit;

  if SelectedModuleList.FindModule( aModule.ModuleIndex) = nil then
    SelectedModuleList.AddModule( aModule);

  if (SelectedModuleList.Count = 1) and (aModule.ParameterCount > 0) then begin
    if (aModule.FSelectedParam = -1) then
      aModule.FSelectedParam := 0;
    if aModule.FSelectedParam >= aModule.ParameterCount then
      aModule.FSelectedParam := 0;

    aModule.Parameter[ aModule.FSelectedParam].Selected := True;
  end;
end;

procedure TG2FilePatch.DeselectModule(aModule: TG2FileModule);
var Param : TG2FileParameter;
begin
  Param := aModule.GetSelectedParam;
  if Param = FSelectedParam then
    FSelectedParam := nil;

  FSelectedModuleList.DeleteModule( aModule.ModuleIndex);
end;

procedure TG2FilePatch.AddCableToPatch( aLocation : TLocationType; aCable: TG2FileCable);
begin
  FPatchPart[ ord(aLocation)].AddCable( aCable);
end;

procedure TG2FilePatch.DeleteCableFromPatch(aLocation : TLocationType; aCable: TG2FileCable);
begin
  FPatchPart[ ord(aLocation)].DeleteCable( aCable);
end;

function TG2FilePatch.FindKnob(aLocation : TLocationType; aModuleIndex, aParamIndex : byte): integer;
begin
  Result := FKnobList.FindKnob( aLocation, aModuleIndex, aParamIndex);
end;

function TG2FilePatch.FindMidiCC(aLocation : TLocationType; aModuleIndex, aParamIndex : byte): byte;
begin
  Result := FControllerList.FindMidiCC( aLocation, aModuleIndex, aParamIndex);
end;

function TG2FilePatch.AssignKnobInPatch(aKnobIndex: integer; aLocation : TLocationType; aModuleIndex, aParamIndex : byte): TKnob;
var Knob : TKnob;
begin
  Result := nil;

  if aKnobIndex < 0 then
    exit;

  if aKnobIndex < FKnobList.Count then begin
    Knob := FKnobList.Items[ aKnobIndex];
    Knob.Fassigned := 1;
    Knob.FLocation := ord(aLocation);
    Knob.FModuleIndex := aModuleIndex;
    Knob.FIsLed := 0;
    Knob.FParamIndex := aParamIndex;

    Result := Knob;
  end else
    raise Exception.Create('Knob index ' + IntToStr(aKnobIndex) + ' out of range.');
end;

function TG2FilePatch.AssignMidiCCInPatchX(aMidiCC: byte; aLocation : TLocationType; aModuleIndex, aParamIndex : byte): TController;
var Controller : TController;
begin
  Result := nil;
  Controller := GetMidiCC( aMidiCC);
  if Controller = nil then begin

    Controller := TController.Create;
    Controller.FLocation := ord(aLocation);
    Controller.FModuleIndex := aModuleIndex;
    Controller.FParamIndex := aParamIndex;
    Controller.FMidiCC := aMidiCC;
    FControllerList.Add( Controller);

    Result := Controller;
  end else begin
    Controller.FLocation := ord(aLocation);
    Controller.FModuleIndex := aModuleIndex;
    Controller.FParamIndex := aParamIndex;

    Result := Controller;
  end;
end;

procedure TG2FilePatch.DeassignKnobInPatch(aKnobIndex: integer);
var Knob : TKnob;
begin
  if aKnobIndex < 0 then
    exit;

  if aKnobIndex < FKnobList.Count then begin
    Knob := FKnobList.Items[ aKnobIndex];
    Knob.Fassigned := 0;
  end else
    raise Exception.Create('Knob index ' + IntToStr(aKnobIndex) + ' out of range.');
end;

procedure TG2FilePatch.DeassignMidiCCInPatchX(aMidiCC: byte);
var i : integer;
begin
  i := 0;
  while (i < FControllerList.Count) and
    not(FControllerList.Items[i].FMidiCC = aMidiCC) do
    inc(i);

  if (i < FControllerList.Count) then
    FControllerList.Delete(i);
end;

function TG2FilePatch.GetMaxModuleIndex( aLocation : TLocationType) : integer;
begin
  Result := FPatchPart[ord(aLocation)].GetMaxModuleIndex;
end;

function TG2FilePatch.GetNoOffModuleType( aLocation : TLocationType; aModuleType : byte) : integer;
begin
  Result := FPatchPart[ord(aLocation)].GetNoOffModuleType( aModuleType);
end;

function TG2FilePatch.GetKnob( KnobIndex: integer): TKnob;
begin
  if (KnobIndex >= 0) and (KnobIndex < FKnobList.Count) then begin
    Result := FKnobList.Items[ KnobIndex]
  end else
    Result := nil;
end;

function TG2FilePatch.GetMidiCC( aMidiCC : byte): TController;
var i : integer;
begin
  Result := nil;
  i := 0;
  while (i < FControllerList.Count) and
    not(FControllerList.Items[i].FMidiCC = aMidiCC) do
    inc(i);

  if (i < FControllerList.Count) then
    Result := FControllerList.Items[i];
end;

function TG2FilePatch.GetModuleName( aLocation : TLocationType; aModuleIndex : byte): AnsiString;
begin
  Result := FPatchPart[ord(aLocation)].FindModuleLabel( aModuleIndex);
end;

function TG2FilePatch.GetParameterValue( aLocation : TLocationType; aModuleIndex : byte; aParamIndex : byte; aVariation : byte): byte;
begin
  Result := 0;

  if ( aLocation = ltFX) or ( aLocation = ltVA) then begin
    Result := FPatchPart[ord(aLocation)].FindParamValue( aModuleIndex, aVariation, aParamIndex);
  end else
    if aLocation = ltPatch then begin

      case aModuleIndex of
        PATCH_MORPH :
          begin
            case aParamIndex of
              0..7  : Result := FPatchSettings.FMorphs[aParamIndex].FDials[ aVariation];
              8..15 : Result := FPatchSettings.FMorphs[aParamIndex - 8].FModes[ aVariation];
            end;
          end;
        PATCH_VOLUME :
          begin
            case aParamIndex of
              VOLUME_LEVEL : Result := FPatchSettings.FVariations[ aVariation].FPatchVol;
              VOLUME_MUTE  : Result := FPatchSettings.FVariations[ aVariation].FActiveMuted;
            end;
          end;
        PATCH_GLIDE :
          begin
            case aParamIndex of
              GLIDE_TYPE  : Result := FPatchSettings.FVariations[ aVariation].FGlide;
              GLIDE_SPEED : Result := FPatchSettings.FVariations[ aVariation].FGlideTime;
            end;
          end;
        PATCH_BEND :
          begin
            case aParamIndex of
              BEND_ON_OFF : Result := FPatchSettings.FVariations[ aVariation].FBend;
              BEND_RANGE  : Result := FPatchSettings.FVariations[ aVariation].FSemi;
            end;
          end;
        PATCH_VIBRATO :
          begin
            case aParamIndex of
              VIBRATO_MOD   : Result := FPatchSettings.FVariations[ aVariation].FVibrato;
              VIBRATO_DEPTH : Result := FPatchSettings.FVariations[ aVariation].FCents;
              VIBRATO_RATE  : Result := FPatchSettings.FVariations[ aVariation].FRate;
            end;
          end;
        PATCH_ARPEGGIATOR :
          begin
            case aParamIndex of
              ARP_ON_OFF    : Result := FPatchSettings.FVariations[ aVariation].FArpeggiator;
              ARP_SPEED     : Result := FPatchSettings.FVariations[ aVariation].FArpTime;
              ARP_DIRECTION : Result := FPatchSettings.FVariations[ aVariation].FArpType;
              ARP_OCTAVES   : Result := FPatchSettings.FVariations[ aVariation].FOctaves;
            end;
          end;
        PATCH_SUSTAIN :
          begin
            case aParamIndex of
              SUSTAIN_PEDAL : Result := FPatchSettings.FVariations[ aVariation].FSustain;
              OCTAVE_SHIFT : Result := FPatchSettings.FVariations[ aVariation].FOctaveShift;
            end;
          end;
      end;
    end;
end;

procedure TG2FilePatch.SetParameterValue( aLocation : TLocationType; aModuleIndex : byte; aParamIndex : byte; aVariation : byte; aValue: byte);
var ParamSet : TParamSet;
    Params : TParams;
begin
  if ( aLocation = ltFX) or ( aLocation = ltVA) then begin
    ParamSet := FPatchPart[ord(aLocation)].FindParamSet( aModuleIndex);
    if assigned(ParamSet) then begin
      Params := ParamSet.FindVariation(aVariation);
      if assigned(Params) then
        Params.FParamValues[ aParamIndex] := aValue;
    end;
  end else
    if aLocation = ltPatch then begin

      case aModuleIndex of
        PATCH_MORPH :
          begin
            case aParamIndex of
              0..7  : FPatchSettings.FMorphs[aParamIndex].FDials[ aVariation] := aValue;
              8..15 : FPatchSettings.FMorphs[aParamIndex - 8].FModes[ aVariation] := aValue;
            end;
          end;
        PATCH_VOLUME :
          begin
            case aParamIndex of
              VOLUME_LEVEL : FPatchSettings.FVariations[ aVariation].FPatchVol := aValue;
              VOLUME_MUTE  : FPatchSettings.FVariations[ aVariation].FActiveMuted := aValue;
            end;
          end;
        PATCH_GLIDE :
          begin
            case aParamIndex of
              GLIDE_TYPE  : FPatchSettings.FVariations[ aVariation].FGlide := aValue;
              GLIDE_SPEED : FPatchSettings.FVariations[ aVariation].FGlideTime := aValue;
            end;
          end;
        PATCH_BEND :
          begin
            case aParamIndex of
              BEND_ON_OFF : FPatchSettings.FVariations[ aVariation].FBend := aValue;
              BEND_RANGE : FPatchSettings.FVariations[ aVariation].FSemi := aValue;
            end;
          end;
        PATCH_VIBRATO :
          begin
            case aParamIndex of
              VIBRATO_MOD   : FPatchSettings.FVariations[ aVariation].FVibrato := aValue;
              VIBRATO_DEPTH : FPatchSettings.FVariations[ aVariation].FCents := aValue;
              VIBRATO_RATE  : FPatchSettings.FVariations[ aVariation].FRate := aValue;
            end;
          end;
        PATCH_ARPEGGIATOR :
          begin
            case aParamIndex of
              ARP_ON_OFF    : FPatchSettings.FVariations[ aVariation].FArpeggiator := aValue;
              ARP_SPEED     : FPatchSettings.FVariations[ aVariation].FArpTime := aValue;
              ARP_DIRECTION : FPatchSettings.FVariations[ aVariation].FArpType := aValue;
              ARP_OCTAVES   : FPatchSettings.FVariations[ aVariation].FOctaves := aValue;
            end;
          end;
        PATCH_SUSTAIN :
          begin
            case aParamIndex of
              SUSTAIN_PEDAL : FPatchSettings.FVariations[ aVariation].FSustain := aValue;
              OCTAVE_SHIFT : FPatchSettings.FVariations[ aVariation].FOctaveShift := aValue;
            end;
          end;
      end;
    end;
end;

procedure TG2FilePatch.InitParameterValue( aLocation : TLocationType; aModuleIndex, aParamIndex, aVariation, aValue : byte);
begin
  //raise Exception.Create('Call of abstract function.');
  SetParameterValue( aLocation, aModuleIndex, aParamIndex, aVariation, aValue);


  if assigned(G2.OnParameterChange) then
    G2.OnParameterChange( self, G2.ID, Slot.SlotIndex, aVariation, aLocation, aModuleIndex, aParamIndex, aValue);
end;

procedure TG2FilePatch.InitModeValue( aLocation : TLocationType; aModuleIndex, aParamIndex, aValue : byte);
begin
  raise Exception.Create('Call of abstract function.');
end;

function TG2FilePatch.GetModeValue( aLocation : TLocationType; aModuleIndex : byte; aParamIndex : byte): byte;
var Module : TG2FileModule;
begin
  Result := 0;

  Module := FPatchPart[ ord(aLocation)].FindModule( aModuleIndex);
  if assigned(Module) then
    if aParamIndex < Module.FModeCount then
      Result := Module.FModeInfo[ aParamIndex];
end;

procedure TG2FilePatch.SetModeValue( aLocation : TLocationType; aModuleIndex : byte; aParamIndex : byte; aValue: byte);
var Module : TG2FileModule;
begin
  Module := FPatchPart[ ord(aLocation)].FindModule( aModuleIndex);
  if assigned(Module) then
    if aParamIndex < Module.FModeCount then
      Module.FModeInfo[ aParamIndex] := aValue;
end;

function TG2FilePatch.HasMorph( aLocation : TLocationType; aModuleIndex, aParamIndex, aVariation : byte): boolean;
var i : integer;
    Variation : TVariation;
begin
  Variation := FPatchSettings.FVariations[aVariation];
  i := 0;
  while (i < Variation.Count) and
    not((Variation.Items[i].FLocation = TBits2(aLocation)) and
        (Variation.Items[i].FModuleIndex = aModuleIndex) and
        (Variation.Items[i].FParamIndex = aParamIndex)) do
    inc(i);

  Result := (i < Variation.Count);
end;

function TG2FilePatch.GetMorph( aLocation : TLocationType; aModuleIndex, aParamIndex, aMorphIndex, aVariation : byte): TMorphParameter;
begin
  Result := FPatchSettings.FVariations[aVariation].FindMorphParam( aLocation, aModuleIndex, aParamIndex, aMorphIndex, aVariation);
end;

procedure TG2FilePatch.SetMorphValue( aLocation : TLocationType; aModuleIndex : byte; aParamIndex : byte; aMorphIndex : byte; aValue: byte; aVariation : byte);
var MorphParameter : TMorphParameter;
begin
  MorphParameter := GetMorph( aLocation, aModuleIndex, aParamIndex, aMorphIndex, aVariation);
  if not assigned(MorphParameter) then begin
    if aValue <> 0 then
      FPatchSettings.FVariations[aVariation].AddNewMorphParam( ord(aLocation), aModuleIndex, aParamIndex, aMorphIndex, aValue);
  end else begin
    if aValue = 0 then
      FPatchSettings.FVariations[aVariation].DelMorphParam( ord(aLocation), aModuleIndex, aParamIndex, aMorphIndex)
    else
      MorphParameter.FRange := aValue;
  end;
end;

procedure TG2FilePatch.SetVariation( aValue : byte);
begin
  FPatchDescription.ActiveVariation := aValue;
end;

function TG2FilePatch.GetVariation : byte;
begin
  Result := FPatchDescription.ActiveVariation;
end;

procedure TG2FilePatch.AddVariation;
begin
  FPatchPart[0].AddNewVariation;
  FPatchPart[1].AddNewVariation;
  FPatchSettings.AddNewVariation;
  FMorphParameters.AddNewVariation;
end;

procedure TG2FilePatch.CopyVariation( aFromVariation, aToVariation : byte);
begin
  FPatchPart[0].CopyVariation( aFromVariation, aToVariation);
  FPatchPart[1].CopyVariation( aFromVariation, aToVariation);
  FPatchSettings.FVariations[ aToVariation].Copy( FPatchSettings.FVariations[ aFromVariation]);
end;

function TG2FilePatch.GetPatchPart( aIndex : integer): TG2FilePatchPart;
begin
  if (aIndex < Length(FPatchPart)) then
    Result := FPatchPart[ aIndex]
  else
    raise Exception.Create('Patch part index out of range.');
end;

function TG2FilePatch.GetModuleList( aIndex : integer): TModuleList;
begin
  if aIndex < Length(FPatchPart) then
    Result := FPatchPart[ aIndex].FModuleList
  else
    raise Exception.Create('Module list index ' + IntToStr( aIndex) + ' out of range.');
end;

function TG2FilePatch.GetSelectedModuleList: TModuleList;
begin
  Result := FSelectedModuleList;
end;

function TG2FilePatch.GetCableList( aIndex : integer): TCableList;
begin
  if aIndex < Length(FPatchPart) then
    Result := FPatchPart[ aIndex].FCableList
  else
    raise Exception.Create('Cable list index ' + IntToStr( aIndex) + ' out of range.');
end;

function TG2FilePatch.GetParameterList( aIndex : integer): TModuleParameters;
begin
  if aIndex < Length(FPatchPart) then
    Result := FPatchPart[ aIndex].FParameterList
  else
    raise Exception.Create('Parameter list index ' + IntToStr( aIndex) + ' out of range.');
end;

function TG2FilePatch.GetModuleLabels( aIndex : integer): TModuleLabels;
begin
  if aIndex < Length(FPatchPart) then
    Result := FPatchPart[ aIndex].FModuleLabels
  else
    raise Exception.Create('ModuleLabels index ' + IntToStr( aIndex) + ' out of range.');
end;

function TG2FilePatch.GetParameterLabels( aIndex : integer): TParameterLabels;
begin
  if aIndex < Length(FPatchPart) then
    Result := FPatchPart[ aIndex].FParameterLabels
  else
    raise Exception.Create('ParameterLabels index ' + IntToStr( aIndex) + ' out of range.');
end;

{procedure TG2FilePatch.SetMaxVariations( aValue : Byte);
begin
  FMaxVariations := aValue;
end;

function TG2FilePatch.GetMaxVariations : Byte;
begin
  Result := FMaxVariations;
end;}

function TG2FilePatch.GetPatchName : AnsiString;
begin
  if assigned(FSlot) then
    Result := FSlot.FPatchName
  else
    Result := '';
end;

function TG2FilePatch.GetModule( LocationIndex, ModuleIndex: integer): TG2FileModule;
begin
  Result := FPatchPart[LocationIndex].FModuleList.FindModule( ModuleIndex);
end;

function TG2FilePatch.GetModuleCount( LocationIndex: integer): integer;
begin
  Result := FPatchPart[LocationIndex].FModuleList.Count;
end;

function TG2FilePatch.GetParameter( ModuleIndex, ParamIndex: integer): TG2FileParameter;
var i : integer;
begin
  i := 0;
  while ( i < Length(FParams)) and not( (ModuleIndex = FParams[i].ModuleIndex)
                                    and (ParamIndex = FParams[i].ParamIndex)) do
    inc(i);

  if ( i < Length(FParams)) then
    Result := FParams[i]
  else
    Result := nil;
    //raise Exception.Create('Parameter with moduleindex ' + IntToStr(ModuleIndex) + ' and paramindex ' + IntToStr(ParamIndex) + ' not found.');
end;

function TG2FilePatch.GetParameterCount: integer;
begin
  Result := Length(FParams);
end;

function TG2FilePatch.GetActiveVariation : byte;
begin
  Result := FPatchDescription.FActiveVariation;
end;


function TG2FilePatch.GetParameterLabel( aLocation : TLocationType; aModuleIndex, aParamIndex : byte): AnsiString;
begin
  if (aLocation = ltFX) or (aLocation = ltVA) then begin
    Result := FPatchPart[ ord(aLocation)].GetParameterLabel( aModuleIndex, aParamIndex)
  end else
    Result := '';
end;

procedure TG2FilePatch.SetParameterLabel( aLocation : TLocationType; aModuleIndex, aParamIndex : byte; aValue : AnsiString);
begin
  if (aLocation = ltFX) or (aLocation = ltVA) then
    FPatchPart[ ord(aLocation)].SetParameterLabel(aModuleIndex, aParamIndex, aValue);
end;

function TG2FilePatch.GetModuleLabel( aLocation : TLocationType; aModuleIndex : byte): AnsiString;
begin
  if (aLocation = ltFX) or (aLocation = ltVA) then begin
    Result := FPatchPart[ ord(aLocation)].GetModuleLabel( aModuleIndex);
  end else
    Result := '';
end;

procedure TG2FilePatch.SetModuleLabel( aLocation : TLocationType; aModuleIndex : byte; aValue : AnsiString);
begin
  if (aLocation = ltFX) or (aLocation = ltVA) then
    FPatchPart[ ord(aLocation)].SetModuleLabel( aModuleIndex, aValue);
end;

procedure TG2FilePatch.UnselectModules( aLocation : TLocationType);
var i : integer;
begin
  for i := 0 to ModuleCount[ord(aLocation)] - 1 do
    if ModuleList[ord(aLocation)].Items[i].Selected then
      ModuleList[ord(aLocation)].Items[i].Selected := False;
end;

procedure TG2FilePatch.SelectModuleAbove;
var Module : TG2FileModule;
begin
  if FSelectedParam = nil then
    exit;

  Module := ModuleList[ ord(FSelectedParam.FLocation)].GetModuleAbove( FSelectedParam.FModuleIndex);
  if assigned(Module) then begin
    UnSelectModules( FSelectedParam.FLocation);
    Module.Selected := True;
  end;
end;

procedure TG2FilePatch.SelectModuleUnder;
var Module : TG2FileModule;
begin
  if FSelectedParam = nil then
    exit;

  Module := ModuleList[ ord(FSelectedParam.FLocation)].GetModuleUnder( FSelectedParam.FModuleIndex);
  if assigned(Module) then begin
    UnSelectModules( FSelectedParam.FLocation);
    Module.Selected := True;
  end;
end;

procedure TG2FilePatch.SelectModuleLeft;
var Module : TG2FileModule;
begin
  if FSelectedParam = nil then
    exit;

  Module := ModuleList[ ord(FSelectedParam.FLocation)].GetModuleLeft( FSelectedParam.FModuleIndex);
  if assigned(Module) then begin
    UnSelectModules( FSelectedParam.FLocation);
    Module.Selected := True;
  end;
end;

procedure TG2FilePatch.SelectModuleRight;
var Module : TG2FileModule;
begin
  if FSelectedParam = nil then
    exit;

  Module := ModuleList[ ord(FSelectedParam.FLocation)].GetModuleRight( FSelectedParam.FModuleIndex);
  if assigned(Module) then begin
    UnSelectModules( FSelectedParam.FLocation);
    Module.Selected := True;
  end;
end;

procedure TG2FilePatch.SelectNextModuleParam;
var Module : TG2FileModule;
begin
  if FSelectedParam = nil then
    exit;

  Module := ModuleList[ ord(FSelectedParam.FLocation)].FindModule( FSelectedParam.FModuleIndex);
  if assigned(Module) then begin
    Module.SelectNextParam;
  end;
end;

procedure TG2FilePatch.SelectPrevModuleParam;
var Module : TG2FileModule;
begin
  if FSelectedParam = nil then
    exit;

  Module := ModuleList[ ord(FSelectedParam.FLocation)].FindModule( FSelectedParam.FModuleIndex);
  if assigned(Module) then begin
    Module.SelectPrevParam;
  end;
end;

function TG2FilePatch.MessAddModule( aLocation : TLocationType; aModuleType, aCol, aRow: byte): boolean;
begin
  Result := False;
  UnselectModules( ltVA);
  UnselectModules( ltFX);
end;

function TG2FilePatch.MessCopyModules( aSrcePatch : TG2FilePatchPart; aFromLocation, aToLocation : TLocationType): boolean;
begin
  Result := False;
  UnselectModules( ltVA);
  UnselectModules( ltFX);
end;

function TG2FilePatch.MessAddConnection( aLocation : TLocationType; aFromConnection, aToConnection : TG2FileConnector): boolean;
begin
  // Abstract
  Result := False;
end;

function TG2FilePatch.MessDeleteConnection( aLocation : TLocationType; aCable : TG2FileCable): boolean;
begin
  // Abstract
  Result := False;
end;

function TG2FilePatch.MessDeleteModule( aLocation : TLocationType; aModuleIndex : byte): boolean;
begin
  // Abstract
  Result := False;
end;

function TG2FilePatch.MessDeleteModules( aLocation : TLocationType): boolean;
begin
  // Abstract
  Result := False;
end;

function TG2FilePatch.MessMoveModule( aLocation : TLocationType; aModuleIndex, aCol, aRow : byte): boolean;
begin
  // Abstract
  Result := False;
end;

function TG2FilePatch.MessSetModuleColor( aLocation: TLocationType; aModuleIndex, aColor : byte): boolean;
begin
  // Abstract
  Result := False;
end;

function TG2FilePatch.MessSetModuleParamLabels( aLocation: TLocationType; aModuleIndex, aParamIndex: byte; aName: AnsiString): boolean;
begin
  // Abstract
  Result := False;
end;

function TG2FilePatch.MessSetModuleLabel( aLocation : TLocationType; aModuleIndex : byte; aName : AnsiString): boolean;
begin
  // Abstract
  Result := False;
end;

procedure TG2FilePatch.SortLeds;
begin
  // Abstract
end;

procedure TG2FilePatch.SetSelectedMorphIndex( aValue : integer);
begin
  FSelectedMorphIndex := aValue;
end;

////////////////////////////////////////////////////////////////////////////////
//  TG2FileConnector
////////////////////////////////////////////////////////////////////////////////

constructor TG2FileConnector.Create( aPatch : TG2FilePatch; aLocation : TLocationType; aModule : TG2FileModule);
begin
  FPatch := aPatch;
  FLocation := aLocation;
  FModule := aModule;

  FConnectorKind := ckInput;
  FConnectorDefColor := COLOR_RED;
  FConnectorIndex := 0;

  FControl := nil;
  FCables := TObjectList.Create( False);
end;

destructor TG2FileConnector.Destroy;
var i : integer;
begin
  // Remove the connector from any cable that isn't freed yet
  for i := 0 to FCables.Count - 1 do begin
    if TG2FileCable(FCables[i]).FFromConnector = self then
      TG2FileCable(FCables[i]).FFromConnector := nil;

    if TG2FileCable(FCables[i]).FToConnector = self then
      TG2FileCable(FCables[i]).FToConnector := nil;
  end;
  FCables.Free;

  inherited;
end;

procedure TG2FileConnector.InitConnector(aConnectorIndex: Byte; aConnectorKind: TConnectorKind; ConnectorDef: TXMLConnectorType);
begin
  FConnectorIndex := aConnectorIndex;
  FConnectorKind := aConnectorKind;
  FName := ConnectorDef.Name;
  if LowerCase(string(ConnectorDef.Type_)) = 'yellow' then begin
    FConnectorType := ctLogic;
    FBandWidth := btStatic;
  end else
    if LowerCase(string(ConnectorDef.Type_)) = 'yellow_orange' then begin
      FConnectorType := ctLogic;
      FBandWidth := btStatic;
    end else
      if LowerCase(string(ConnectorDef.Type_)) = 'blue' then begin
        FConnectorType := ctLogic;
        FBandWidth := btStatic;
      end else
        if LowerCase(string(ConnectorDef.Type_)) = 'blue_red' then begin
          FConnectorType := ctLogic;
          FBandWidth := btStatic;
        end else
          if LowerCase(string(ConnectorDef.Type_)) = 'red' then begin
            FConnectorType := ctLogic;
            FBandWidth := btStatic;
          end else
            raise Exception.Create('Unknown connector type ' + string(ConnectorDef.Type_));
end;

procedure TG2FileConnector.AddCable( aCable: TG2FileCable);
begin
  FCables.Add( aCable);
end;

procedure TG2FileConnector.DelCable(aCable: TG2FileCable);
var i : integer;
begin
  i := 0;
  while ( i < FCables.Count) and (FCables[i] <> aCable) do
    inc(i);

  if ( i < FCables.Count) then
    FCables.Delete(i);

{FMX  if assigned( FModule) and assigned( FModule.Parent) then
    FModule.Parent.Invalidate;}
end;

procedure TG2FileConnector.InvalidateCables;
var i : integer;
begin
  for i := 0 to FCables.Count - 1 do
    TG2FileCable(FCables[i]).ConnectorMoved;
end;

procedure TG2FileConnector.SetConnectorDefColor( aValue: byte);
begin
  FConnectorDefColor := aValue;

{FMX  if assigned(FCOntrol) then
    FControl.Invalidate;}
end;

procedure TG2FileConnector.SetConnectorKind( aValue : TConnectorKind);
begin
  FConnectorKind := aValue;

{FMX  if assigned(FCOntrol) then
    FControl.Invalidate;}
end;

function TG2FileConnector.GetConnectorColor: byte;
begin
  if (FConnectorDefColor = COLOR_BLUE) and (FModule.Uprate = 1) and (FBandWidth = btDynamic) then
    Result := COLOR_RED
  else
    if (FConnectorDefColor = COLOR_YELLOW) and (FModule.Uprate = 1) and (FBandWidth = btDynamic) then
      Result := COLOR_ORANGE
    else
      Result := FConnectorDefColor;
end;

function TG2FileConnector.GetNewConnectorColor: byte;
begin
  if (FConnectorDefColor = COLOR_BLUE) and (FModule.NewUprate = 1) and (FBandWidth = btDynamic) then
    Result := COLOR_RED
  else
    if (FConnectorDefColor = COLOR_YELLOW) and (FModule.NewUprate = 1) and (FBandWidth = btDynamic) then
      Result := COLOR_ORANGE
    else
      Result := FConnectorDefColor;
end;

function TG2FileConnector.GetCable( aIndex : integer) : TG2FileCable;
begin
  if aIndex < FCables.Count then
    Result := FCables[ aIndex] as TG2FileCable
  else
    raise Exception.Create('Cable index ' + IntToStr( aIndex) + ' out of range.');
end;

function TG2FileConnector.GetCableCount : integer;
begin
  Result := FCables.Count;
end;

procedure TG2FileConnector.CalcDefColor;
begin
  if FConnectorType = ctLogic then begin
    ConnectorDefColor := COLOR_YELLOW;
  end else
    if FConnectorType = ctControl then
      ConnectorDefColor := COLOR_BLUE
    else
      if FConnectorType = ctAudio then begin
        if FBandWidth = btStatic then ConnectorDefColor := COLOR_RED;
        if FBandWidth = btDynamic then ConnectorDefColor := COLOR_BLUE;
      end;
end;

function TG2FileConnector.GetDefRate: TBits1;
begin
  Result := 0;
  // Return signal frequency indicator comming from this connector : 0 - 24kHz, 1 - 96kHz
  if FConnectorType = ctLogic then
    Result := 0
  else
    if FConnectorType = ctControl then
      Result := 0
    else
      if FConnectorType = ctAudio then begin
        if FBandWidth = btStatic then Result := 1;
        if FBandWidth = btDynamic then Result := 0;
      end else
        Result := 0;
end;

function TG2FileConnector.GetRate: TBits1;
begin
  if (FModule.Uprate = 1) and (FBandWidth = btDynamic) then
    Result := 1
  else
    Result := GetDefRate;
end;

function TG2FileConnector.GetNewRate: TBits1;
begin
  if (FModule.NewUprate = 1) and (FBandWidth = btDynamic) then
    Result := 1
  else
    Result := GetDefRate;
end;

////////////////////////////////////////////////////////////////////////////////
//  TG2FileParameter
////////////////////////////////////////////////////////////////////////////////

constructor TG2FileParameter.Create( aPatch : TG2FilePatch; aLocation : TLocationType; aModuleIndex : integer);
begin
  inherited Create;

  FPatch := aPatch;
  FLocation := aLocation;
  FModuleIndex := aModuleIndex;
  FKnob := nil;
  FCanChangeLabel := False;
  FDefaultKnob := -1;
  FButtonParamIndex := -1;
  FButtonText := TStringList.Create;
  FButtonText.Delimiter := ';';
end;

destructor TG2FileParameter.Destroy;
begin
  FButtonText.Free;
  inherited;
end;

function TG2FileParameter.GetButtonText(Index: integer): string;
begin
  if Index < FButtonText.Count then
    Result := FButtonText[Index]
  else
    Result := '';
end;

function TG2FileParameter.GetButtonTextCount: integer;
begin
  Result := FButtonText.Count;
end;

function TG2FileParameter.GetMorph( aMorphIndex, aVariation : integer) : TMorphParameter;
begin
  if assigned(FPatch) then
    Result := FPatch.GetMorph( FLocation, FModuleIndex, FParamIndex, aMorphIndex, aVariation)
  else
    Result := nil;
end;

function TG2FileParameter.GetSelectedMorph: TMorphParameter;
begin
  Result := GetMorph( FPatch.FSelectedMorphIndex, FPatch.ActiveVariation);
end;

function TG2FileParameter.HasMorph: boolean;
begin
  if assigned(FPatch) then
    Result := FPatch.HasMorph( FLocation, FModuleIndex, FParamIndex, FPatch.ActiveVariation)
  else
    Result := False;
end;

function TG2FileParameter.GetMorphValue( aMorphIndex, aVariation : integer) : byte;
var MorphParameter : TMorphParameter;
begin
  MorphParameter := GetMorph( aMorphIndex, aVariation);
  if assigned(MorphParameter) then begin
    if MorphParameter.FRange >= 128 then
      Result := GetParameterValue + ( MorphParameter.FRange - 256)
    else
      Result := GetParameterValue + MorphParameter.FRange;

    if Result > FHighValue then
      Result := FHighValue
    else
      if Result < FLowValue then
        Result := FLowValue;

  end else
    Result := 0;
end;

function TG2FileParameter.GetSelectedMorphValue : byte;
begin
  Result := GetMorphValue( FPatch.FSelectedMorphIndex, FPatch.ActiveVariation);
end;

procedure TG2FileParameter.SetSelectedMorphValue( Value: byte);
var Variation, FromVariation, ToVariation : byte;
begin
  if assigned(FPatch) then begin

    if FPatch.EditAllVariations then begin
      FromVariation := 0;
      ToVariation := NVARIATIONS - 1;
    end else begin
      FromVariation := FPatch.ActiveVariation;
      ToVariation := FPatch.ActiveVariation;
    end;

    for Variation := FromVariation to ToVariation do
      FPatch.SetMorphValue( FLocation, FModuleIndex, FParamIndex, FPatch.FSelectedMorphIndex, Value, Variation);
    InvalidateControl;
  end;
end;

function TG2FileParameter.GetParameterValue: byte;
begin
  if assigned(FPatch) then begin
    if FParamType = ptParam then
      Result := FPatch.GetParameterValue( FLocation, FModuleIndex, FParamIndex, FPatch.ActiveVariation)
    else
      Result := FPatch.GetModeValue( FLocation, FModuleIndex, FParamIndex);
  end else
    Result := 0;
end;

procedure TG2FileParameter.SetButtonText(Index: integer; aValue: string);
begin
  if Index < FButtonText.Count then
    FButtonText[Index] := aValue;
end;

procedure TG2FileParameter.SetParameterValue( Value: byte);
var Variation, FromVariation, ToVariation : byte;
begin
  if assigned(FPatch) then begin
    if FParamType = ptParam then begin
      if FPatch.EditAllVariations then begin
        FromVariation := 0;
        ToVariation := NVARIATIONS - 1;
      end else begin
        FromVariation := FPatch.ActiveVariation;
        ToVariation := FPatch.ActiveVariation;
      end;

      for Variation := FromVariation to ToVariation do begin
        FPatch.SetParameterValue( FLocation, FModuleIndex, FParamIndex, Variation, Value)
      end;
    end else
      FPatch.SetModeValue( FLocation, FModuleIndex, FParamIndex, Value);
  end;
end;

procedure TG2FileParameter.IncValue;
var Value : byte;
begin
  Value := GetParameterValue;
  if Value < FHighValue then
    SetParameterValue( Value + 1)
end;

procedure TG2FileParameter.DecValue;
var Value : byte;
begin
  Value := GetParameterValue;
  if Value > FLowValue then
    SetParameterValue( Value - 1)
end;

procedure TG2FileParameter.IncMorphValue;
var Value: byte;
    Range : integer;
    MorphParameter : TMorphParameter;
begin
  Value := GetParameterValue;
  MorphParameter := GetSelectedMorph;
  if not(assigned(MorphParameter)) then
    Range := 0
  else
    Range := MorphParameter.FRange;

  if Range >= 128 then
    Range := Range - 256;

  inc(Range);

  if (Value + Range) <= FHighValue then begin
    if Range < 0 then
      SetSelectedMorphValue( 256 + Range)
    else
      SetSelectedMorphValue( Range);
  end;
end;

procedure TG2FileParameter.DecMorphValue;
var Value: byte;
    Range : integer;
    MorphParameter : TMorphParameter;
begin
  Value := GetParameterValue;
  MorphParameter := GetSelectedMorph;
  if not(assigned(MorphParameter)) then
    Range := 0
  else
    Range := MorphParameter.FRange;

  if Range >= 128 then
    Range := Range - 256;

  dec(Range);

  if (Value + Range) >= FLowValue then begin
    if Range < 0 then
      SetSelectedMorphValue( 256 + Range)
    else
      SetSelectedMorphValue( Range);
  end;
end;

procedure TG2FileParameter.InitParam( aModuleName : AnsiString; aParamIndex : byte; aParamType : TParamType;
                                      aParamName, aDefaultParamLabel : AnsiString;
                                      aLowValue, aHighValue, aDefaultValue : byte;
                                      aDefaultKnob, aButtonParamIndex : integer; aButtonText : AnsiString);
var i : integer;
begin
  FParamType := aParamType;
  FParamIndex := aParamIndex;
  FParamName := aParamName;
  FDefaultParamLabel := aDefaultParamLabel;
  FCanChangeLabel := aDefaultParamLabel <> '';
  FLowValue := aLowValue;
  FHighValue := aHighValue;
  FDefaultValue := aDefaultValue;
  FModuleName := aModuleName;
  FDefaultKnob := aDefaultKnob;
  FButtonParamIndex := aButtonParamIndex;
  FButtonText.DelimitedText := string(aButtonText);

  if assigned(FPatch) then begin
    // Check for associated knob
    i := FPatch.FindKnob( FLocation, FModuleIndex, FParamIndex);

    if i <> -1 then begin
      FKnob := FPatch.FKnobList.Items[i];
      FKnob.FParameter := self;
    end else
      FKnob := nil;
  end;
end;

procedure TG2FileParameter.AssignKnob( aKnobIndex : integer);
begin
  FKnob := FPatch.AssignKnobInPatch( aKnobIndex, FLocation, FModuleIndex, FParamIndex);
  FKnob.FParameter := self;
end;

procedure TG2FileParameter.DeassignKnob( aKnobIndex: integer);
begin
  FPatch.DeassignKnobInPatch( aKnobIndex);
  FKnob := nil;
end;

procedure TG2FileParameter.AssignGlobalKnob( aPerf : TG2FilePerformance; aSlotIndex : byte; aKnobIndex : integer);
begin
  FGlobalKnob := aPerf.AssignGlobalKnobInPerf( aKnobIndex, aSlotIndex, FLocation, FModuleIndex, FParamIndex);
  FGlobalKnob.FParameter := self;
end;

procedure TG2FileParameter.DeassignGlobalKnob( aPerf : TG2FilePerformance; aKnobIndex: integer);
begin
  aPerf.DeassignGlobalKnobInPerf( aKnobIndex);
  FGlobalKnob := nil;
end;

procedure TG2FileParameter.AssignController( aMidiCC : byte);
begin
  FController := FPatch.AssignMidiCCInPatchX( aMidiCC, FLocation, FModuleIndex, FParamIndex);
  FCOntroller.Parameter := self;
end;

procedure TG2FileParameter.DeassignController;
begin
  if assigned(FController) then begin
    FController.Parameter := nil;
    FPatch.DeassignMidiCCInPatchX( FController.MidiCC);
    FController := nil;
  end;
end;

function TG2FileParameter.GetParamLabel: AnsiString;
begin
  if assigned(FPatch) then
    Result := FPatch.GetParameterLabel( FLocation, FModuleIndex, FParamIndex)
  else
    Result := '';
end;

procedure TG2FileParameter.SetParamLabel( aValue : AnsiString);
begin
  if assigned(FPatch) then
    FPatch.SetParameterLabel( FLocation, FModuleIndex, FParamIndex, aValue);
end;

function TG2FileParameter.GetSelected : boolean;
begin
  if assigned(FPatch) then
    Result := FPatch.SelectedParam = self
  else
    Result := False;;
end;

function TG2FileParameter.GetSelectedButtonText: string;
var ParamValue : byte;
begin
  if FButtonText.Count > 0 then begin
    if FButtonText.Count = 1 then begin
      Result := string(ParamLabel);
      if Result = '' then
        Result := FButtonText[0];
    end else begin
      ParamValue := GetParameterValue;
      if ParamValue < FButtonText.Count then
        Result := FButtonText[ParamValue]
      else
        Result := 'Overflow';
    end;
  end else
    Result := '';
end;

function TG2FileParameter.GetButtonParam : TG2FileParameter;
var Module : TG2FileModule;
begin
  if FButtonParamIndex <> -1 then begin
    Module := FPatch.Modules[ ord(FLocation), FModuleIndex];
    if assigned(Module) then
      Result := Module.Parameter[FButtonParamIndex]
    else
      Result := nil;
  end else
    Result := nil;
end;

procedure TG2FileParameter.SetSelected( aValue : boolean);
var Module : TG2FileModule;
begin
  if aValue then begin
    if assigned(FPatch) then begin
      if (FPatch.SelectedParam <> self) then
        FPatch.SelectedParam := self;
      Module := FPatch.ModuleList[ ord( FLocation)].FindModule( FModuleIndex);
      if assigned(Module) then
        Module.SelectedParam := self;
    end;
  end;
end;

function TG2FileParameter.TextFunction( aTextFunction: integer; LineNo, TotalLines : integer): string;
var aValue : byte;
begin
  aValue := GetParameterValue;
  case aTextFunction of
  108  : begin
           case aValue of
           0..15 : Result := IntToStr(aValue + 1);
           16 : Result := 'This';
           17 : Result := 'Slot A';
           18 : Result := 'Slot B';
           19 : Result := 'Slot C';
           20 : Result := 'Slot D';
           end;
         end;
  1000 : Result := string(FModuleName);
  1001 : begin
           case LineNo of
           0 : Result := string(FParamName);
           1 : Result := IntToStr(aValue);
           end;
         end;
  1002 : begin
           Result := IntToStr(FPatch.Slot.SlotIndex + 1) + ':' + string(FModuleName);
         end
  else
    Result := IntToStr(aValue);
  end;
end;

procedure TG2FileParameter.InvalidateControl;
begin
  // Abstract
end;


////////////////////////////////////////////////////////////////////////////////
//  TG2FileSlotSettings
////////////////////////////////////////////////////////////////////////////////

constructor TG2FileSlot.Create( AOwner: TComponent);
begin
  inherited;

  FG2 := nil;
  FPerformance := nil;
  FPatch := CreatePatch;
  FPatch.Slot := self;
  FPatch.G2 := FG2;

  Init;
end;

destructor TG2FileSlot.Destroy;
begin
  inherited;
end;

function TG2FileSlot.CreatePatch : TG2FilePatch;
begin
  Result := TG2FilePatch.Create( self);
end;

procedure TG2FileSlot.Init;
var i : integer;
begin
  for i := 0 to 3 do
    FUnknown[i] := 0;

  FPatchName         := '';
  FEnabled           := 0;
  FKeyboard          := 0;
  FHold              := 0;
  FKeyboardRangeFrom := 0;
  FKeyboardRangeTo   := 0;
  FSlot              := 0;

  FPatch.Init;
end;

procedure TG2FileSlot.SetG2( aValue : TG2File);
begin
  FG2 := aValue;
  if assigned( FPatch) then
    FPatch.G2 := aValue;
end;

procedure TG2FileSlot.SetPerformance( aValue : TG2FilePerformance);
begin
  FPerformance := aValue;
  if assigned( FPatch) then begin
    FPatch.Performance := aValue;
  end;
end;

procedure TG2FileSlot.SetPatch( aValue : TG2FilePatch);
begin
  FPatch := aValue;
  if assigned( FPatch) then begin
    FPatch.G2 := FG2;
  end;
end;

procedure TG2FileSlot.Read( aChunk : TPatchChunk);
begin
  FPatchName         := aChunk.ReadName;
  FEnabled           := aChunk.ReadBits( 8);
  FKeyboard          := aChunk.ReadBits( 8);
  FHold              := aChunk.ReadBits( 8);
  FUnknown[0]        := aChunk.ReadBits( 8);
  FUnknown[1]        := aChunk.ReadBits( 8);
  FKeyboardRangeFrom := aChunk.ReadBits( 8);
  FKeyboardRangeTo   := aChunk.ReadBits( 8);
  //FSlot              := aChunk.ReadBits( 8); // 2011-11-14 bve : First G2 gives 1..4, second G2 gives 5..8..?
  FUnknown[2]        := aChunk.ReadBits( 8);
  FUnknown[3]        := aChunk.ReadBits( 8);
  FUnknown[4]        := aChunk.ReadBits( 8);
end;

procedure TG2FileSlot.Write( aChunk : TPatchChunk);
begin
  aChunk.WriteName( FPatchName);
  aChunk.WriteBits( FEnabled,           8);
  aChunk.WriteBits( FKeyboard,          8);
  aChunk.WriteBits( FHold,              8);
  aChunk.WriteBits( FUnknown[0],        8);
  aChunk.WriteBits( FUnknown[1],        8);
  aChunk.WriteBits( FKeyboardRangeFrom, 8);
  aChunk.WriteBits( FKeyboardRangeTo,   8);
  //aChunk.WriteBits( FSlot,              8);
  aChunk.WriteBits( FUnknown[2],        8);
  aChunk.WriteBits( FUnknown[3],        8);
  aChunk.WriteBits( FUnknown[4],        8);
end;

function TG2FileSlot.GetSlotIndex: byte;
begin
  Result := FSlot - 1;
end;

procedure TG2FileSlot.SetSlotIndex( aSlotIndex : byte);
begin
  FSlot := aSlotIndex + 1;
end;

function TG2FileSlot.GetPatch: TG2FilePatch;
begin
  Result := FPatch;
end;

procedure TG2FileSlot.SetEnabled( aValue : TBits8);
begin
  if aValue > 1 then
    raise Exception.Create('Slot enabled must be 0 or 1.');
  FEnabled := aValue;
end;

procedure TG2FileSlot.SetHold( aValue : TBits8);
begin
  if aValue > 1 then
    raise Exception.Create('Slot hold must be 0 or 1.');
  FHold := aValue;
end;

procedure TG2FileSlot.SetKeyboard( aValue : TBits8);
begin
  if aValue > 1 then
    raise Exception.Create('Slot keyboard must be 0 or 1.');
end;

procedure TG2FileSlot.SetKeyboardRangeTo( aValue : TBits8);
begin
  FKeyboardRangeTo := aValue; // can be from 0 to 255
end;

procedure TG2FileSlot.SetKeyboardRangeFrom( aValue : TBits8);
begin
  FKeyboardRangeFrom := aValue;
end;

////////////////////////////////////////////////////////////////////////////////
//  TGlobalKnob
////////////////////////////////////////////////////////////////////////////////

procedure TGlobalKnob.Init;
begin
 FAssigned := 0;
 FLocation := 0;
 FModuleIndex := 0;
 FIsLed := 0;
 FParamIndex := 0;
 FSlotIndex := 0;

 FParameter := nil;
end;

procedure TGlobalKnob.Read( aChunk : TPatchChunk);
begin
  // Fassigned : TBits1;
  // FLocation : TBits2;
  // FModuleIndex : TBits8;
  // FIsLed : TBits2;
  // FParamIndex: TBits7;
  // FSlotIndex : TBits2

  FAssigned    := aChunk.ReadBits( 1);
  if FAssigned = 1 then begin

    FLocation    := aChunk.ReadBits( 2);
    FModuleIndex := aChunk.ReadBits( 8);
    FIsLed       := aChunk.ReadBits( 2);
    FParamIndex  := aChunk.ReadBits( 7);
    FSlotIndex   := aChunk.ReadBits( 2);
  end;
end;

procedure TGlobalKnob.Write( aChunk : TPatchChunk);
begin
  aChunk.WriteBits(FAssigned, 1);
  if (FAssigned = 1) then begin
    aChunk.WriteBits(FLocation, 2);
    aChunk.WriteBits(FModuleIndex, 8);
    aChunk.WriteBits(FIsLed, 2);
    aChunk.WriteBits(FParamIndex, 7);
    aChunk.WriteBits(FSlotIndex, 2);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//  TGlobalKnobList
////////////////////////////////////////////////////////////////////////////////

constructor TGlobalKnobList.Create( AOwnsObjects : boolean);
var i : integer;
    GlobalKnob : TGlobalKnob;
begin
  FKnobCount := 120;
  for i := 0 to FKnobCount - 1 do begin
    GlobalKnob := TGlobalKnob.Create;
    GlobalKnob.FKnobIndex := i;
    Add( GlobalKnob);
  end;
end;

destructor TGlobalKnobList.Destroy;
begin
  inherited;
end;

function TGlobalKnobList.GetGlobalKnob( aIndex : integer) : TGlobalKnob;
begin
  result := inherited Items[aindex] as TGlobalKnob;
end;

procedure TGlobalKnobList.SetGlobalKnob( aIndex : integer; const aValue : TGlobalKnob);
begin
  inherited Items[aindex] := aValue;
end;

function TGlobalKnobList.AddGlobalKnob( aValue : TGlobalKnob): integer;
begin
  Result := inherited Add( aValue);
end;

procedure TGlobalKnobList.Init;
var i : integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Init;
end;

function TGlobalKnobList.FindGlobalKnobIndex( aSlotIndex : byte; aLocation : TLocationType; aModuleIndex, aParamIndex : byte): integer;
var i : integer;
begin
  i := 0;
  while (i < Count) and not(( Items[i].FAssigned = 1)
                        and ( Items[i].FSlotIndex = aSlotIndex)
                        and ( Items[i].FLocation = TBits2(aLocation))
                        and ( Items[i].FModuleIndex = aModuleIndex)
                        and ( Items[i].FParamIndex = aParamIndex)) do
    inc(i);

  if (i < Count) then begin
    Result := i
  end else
    Result := -1;
end;

procedure TGlobalKnobList.DeleteModule( aSlotIndex, aLocation, aModuleIndex : Byte);
var i : integer;
begin
  for i := 0 to Count - 1 do
    if (Items[i].FModuleIndex = aModuleIndex)  and (Items[i].FLocation = aLocation) and (Items[i].FSlotIndex = aSlotIndex) then
      Items[i].FAssigned := 0;
end;

procedure TGlobalKnobList.Read( aChunk : TPatchChunk);
var i : integer;
begin
  FKnobCount := aChunk.ReadBits( 16);

  if assigned(aChunk.FLogLines) then begin
    aChunk.FLogLines.Add('KnobList, count = ' + IntToStr(FKnobCount));
    aChunk.FLogLines.Add('Knb Loc Mod Isl Par Slt');
  end;

  for i := 0 to FKnobCount - 1 do begin
    Items[i].Read( aChunk);

    if assigned( aChunk.FLogLines) and (Items[i].FAssigned = 1) then
      aChunk.FLogLines.Add( Format('%3d %3d %3d %3d %3d %3d', [i, Items[i].FLocation, Items[i].FModuleIndex, Items[i].FIsLed, Items[i].FParamIndex, Items[i].FSlotIndex]));
  end;
end;

procedure TGlobalKnobList.Write( aChunk : TPatchChunk);
var i : integer;
begin
  aChunk.WriteBits(FKnobCount, 16);
  for i := 0 to FKnobCount - 1 do
    Items[i].Write( aChunk);
end;

////////////////////////////////////////////////////////////////////////////////
//  TG2FilePerformanceSettings
////////////////////////////////////////////////////////////////////////////////

constructor TG2FilePerformance.Create( AOwner: TComponent);
var i : integer;
begin
  inherited;

  FG2 := nil;

  for i := 0 to NSLOTS - 1 do begin
    FSlot[i] := CreateSlot;
    FSlot[i].G2 := FG2;
    FSlot[i].Performance := self;
    FSlot[i].SlotIndex := i;
  end;

  FGlobalKnobList := TGlobalKnobList.Create( True);

  init;
end;

function TG2FilePerformance.CreateSlot : TG2FileSlot;
begin
  Result := TG2FileSlot.Create( self);
end;

function TG2FilePerformance.GetSlot( aSlot : byte): TG2FileSlot;
begin
  Result := FSlot[ aSlot];
end;

procedure TG2FilePerformance.SetG2(aValue: TG2File);
var i : integer;
begin
  FG2 := aValue;
  if assigned(FG2) then begin
    for i := 0 to NSLOTS - 1 do
      FSlot[i].G2 := FG2;
  end;
end;

procedure TG2FilePerformance.Init;
var i : integer;
begin
  FVersion   := 23;
  FPatchType := 1;

  for i := 0 to 8 do
    FUnknown[i] := 0;

  FKeyboardRangeEnabled := 0;
  FMasterClock          := 0;
  FMasterClockRun       := 0;

  for i := 0 to NSLOTS - 1 do begin
    FSlot[i].Init;
    FSlot[i].SlotIndex := i;
  end;

  FGlobalKnobList.Init;
end;

procedure TG2FilePerformance.InitKnobs;
var i : integer;
    Knob : TGlobalKnob;
    Module : TG2FileModule;
    Param : TG2FileParameter;
begin
  for i  := 0 to FGlobalKnobList.Count - 1 do begin
    Knob := FGlobalKnobList.Items[i];
    if Knob.Fassigned = 1 then begin
      case TLocationType(Knob.FLocation) of
      ltPatch : begin
                  Param := FSlot[ Knob.FSlotIndex].GetPatch.Parameter[ Knob.ModuleIndex, Knob.FParamIndex];
                  if assigned(Param) then
                    Param.AssignGlobalKnob( self, Knob.FSlotIndex, i);
                end;
      ltFX,
      ltVA    : begin
                  Module := FSlot[ Knob.FSlotIndex].GetPatch.GetModule( Knob.FLocation, Knob.FModuleIndex);
                  if assigned(Module) then begin
                    Param := Module.Parameter[ Knob.FParamIndex];
                    if assigned(Param) then
                      Param.AssignGlobalKnob( self, Knob.FSlotIndex, i);
                  end;
                end;
      end;
    end;
  end;
end;

destructor TG2FilePerformance.Destroy;
begin
  FGlobalKnobList.Free;

  inherited;
end;

procedure TG2FilePerformance.SetSelectedSlotIndex( aValue : TBits2);
begin
  FSelectedSlot := aValue;
  if assigned(FG2) and assigned(FG2.FOnSelectSlot) then
    FG2.FOnSelectSlot( self, FG2.ID, aValue);
end;

{procedure TG2FilePerformance.SetMaxVariations( aValue : Byte);
var i : integer;
begin
  for i := 0 to NSLOTS-1 do
    FSLot[i].Patch.MaxVariations := aValue;
end;

function TG2FilePerformance.GetMaxVariations : Byte;
begin
  Result := 0
end;}

procedure TG2FilePerformance.SetPerformanceName( aValue : AnsiString);
begin
  if Length( aValue) > 16 then
    raise Exception.Create('Name length must be 16 max.');

  if aValue = '' then
    aValue := 'No name';
  //  raise Exception.Create('Invalid name');

  FPerformanceName := aValue;
end;

procedure TG2FilePerformance.SetMasterClock( aValue : TBits8);
begin
  if (aValue < 30) or (aValue > 240) then
    raise Exception.Create('Master clock must be between 30 and 240.');

  FMasterClock := aValue;
end;

procedure TG2FilePerformance.SetMasterClockRun( aValue : TBits8);
begin
  if (aValue > 1) then
    raise Exception.Create('Master clock run must be 0 or 1.');

  FMasterClockRun := aValue;
end;

procedure TG2FilePerformance.SetKeyboardRangeEnabled( aValue : TBits8);
begin
  if (aValue > 1) then
    raise Exception.Create('Keyboard range enable must be 0 or 1.');

  FKeyboardRangeEnabled := aValue;
end;

procedure TG2FilePerformance.Read( aChunk : TPatchChunk);
var einde : boolean;
    i, PatchCount : integer;
begin
  // FUnknown              : TBits8;
  // FPerformanceName      : 16TBits8 or ends with #0
  // FKeyboardRangeEnabled : TBits7;
  // FMasterClock          : TBits7;
  // FMasterClockRun       : TBits7;
  // FSettings             : array of TPerformanceSetting;

  einde := False;
  repeat
    case aChunk.FId of
      C_PERF_NAME :
        begin
          FPerformanceName := aChunk.ReadName;
        end;
      C_PERF_SETTINGS :
        begin // performance description
          FUnknown[3]           := aChunk.ReadBits( 8);
          FUnknown[4]           := aChunk.ReadBits( 4);
          FSelectedSlot         := aChunk.ReadBits( 2);
          FUnknown[5]           := aChunk.ReadBits( 2);

          FKeyboardRangeEnabled := aChunk.ReadBits( 8);
          FMasterClock          := aChunk.ReadBits( 8);
          FUnknown[6]           := aChunk.ReadBits( 8);
          FMasterClockRun       := aChunk.ReadBits( 8);
          FUnknown[7]           := aChunk.ReadBits( 8);
          FUnknown[8]           := aChunk.ReadBits( 8);

          for i := 0 to NSLOTS - 1 do
            GetSlot( i).Read( aChunk);
          PatchCount := 0;
        end;
      C_PATCH_DESCR :
        begin // Patch description
          GetSlot( PatchCount).GetPatch.Read( aChunk);
          inc(PatchCount);
          //FPatchDescription.read( aChunk);
        end ;
      C_KNOBS_GLOBAL :
        begin // Unknown
          FGlobalKnobList.Read( aChunk);
        end ;
    end;

    if not einde then begin
      if aChunk.FStream.Position < aChunk.FStream.Size - 2 then
        aChunk.ReadChunk
      else
        einde := true;
    end;

  until einde;
  InitKnobs;
end;

procedure TG2FilePerformance.WriteSettings( aChunk : TPatchChunk);
var i : integer;
begin
  aChunk.WriteBits( FUnknown[3],           8);
  aChunk.WriteBits( FUnknown[4],           4);
  aChunk.WriteBits( FSelectedSlot,         2);
  aChunk.WriteBits( FUnknown[5],           2);


  aChunk.WriteBits( FKeyboardRangeEnabled, 8);
  aChunk.WriteBits( FMasterClock,          8);
  aChunk.WriteBits( FUnknown[6],           8);
  aChunk.WriteBits( FMasterClockRun,       8);
  aChunk.WriteBits( FUnknown[7],           8);
  aChunk.WriteBits( FUnknown[8],           8);

  for i := 0 to NSLOTS - 1 do
    GetSlot( i).Write( aChunk);
  aChunk.WriteChunk( C_PERF_SETTINGS);
end;


procedure TG2FilePerformance.Write( aChunk : TPatchChunk; aVariationCount : byte);
var i : integer;
begin
  // Write patch data
  for i := 0 to NSLOTS - 1 do begin
    GetSlot( i).GetPatch.Write( aChunk, aVariationCount);
  end;

  FGlobalKnobList.Write( aChunk);
  aChunk.WriteChunk( C_KNOBS_GLOBAL);
end;

function TG2FilePerformance.LoadFromFile(aStream : TStream; aLogLines : TStrings): Boolean;
var
  sl : TStringList;
  Chunk : TPatchChunk;
  c  : AnsiChar;
  bm, bl : Byte;
  s  : AnsiString;
  Crc : Word;
begin
  Result := False;

  Chunk := TPatchChunk.Create( aStream);
  sl := TStringList.Create;
  try
    if assigned(aLogLines) then
      Chunk.FLogLines := aLogLines;

    s := '';
    aStream.Position := 0;
    while aStream.Position < aStream.Size do begin
      aStream.Read( c, SizeOf( c));
      case c of
      #$0d :; // just skip it
      #$0a : begin
               sl.Add(string(s));
               s := '';
             end;
      #$00 : Break;
      else
        s := s + c;
      end;
    end;
    if s <> '' then
      sl.Add(string(s));

    Init;
    Chunk.ReadBuffer( 2);
    FVersion := Chunk.ReadBits( 8);
    FPatchType := Chunk.ReadBits( 8);
    Chunk.ReadChunk;
    Read( Chunk);

    aStream.Read(bm, 1);
    aStream.Read(bl, 1);
    Crc := bm * 256 + bl;

    if Crc <> Chunk.FReadCrc then
      raise Exception.Create('Crc error.');

    Result := True;

  finally
    Chunk.Free;
    sl.Free;
  end;
end;

class function TG2FilePerformance.LoadFileStream( AOwner : TComponent; aStream : TStream; aChunk : TPatchChunk): TG2FileDataStream;
begin
  Result := TG2FilePerformance.Create( AOwner);
  aChunk.ReadChunk;
  Result.Read( aChunk);
end;

procedure TG2FilePerformance.SaveToFile( aStream : TStream);
var sl : TStringList;
    i : integer;
    c : AnsiChar;
    s : AnsiString;
    Chunk : TPatchChunk;
begin
  sl := TStringList.Create;
  Chunk := TPatchChunk.Create( aStream);
  try
    sl.Clear;
    sl.Add('Version=Nord Modular G2 File Format 1');
    sl.Add('Type=Performance');
    sl.Add('Version=23');
    sl.Add('Info=BUILD 320');

    aStream.Position := 0;

    for i := 0 to sl.Count - 1 do
    begin
      s := AnsiString(sl[i]);
      aStream.Write( s[1], Length( s));
      c := #$0d;
      aStream.Write( c, SizeOf( c));
      c := #$0a;
      aStream.Write( c, SizeOf( c));
    end;

    c := #0;
    aStream.Write( c, SizeOf( c));

    Chunk.WriteBits(FVersion, 8);
    Chunk.WriteBits(FPatchtype, 8);
    Chunk.Flush;

    //MaxVariations := 9;
    WriteSettings(Chunk);
    Write( Chunk, 9); // 9 Variations are written to file
    Chunk.WriteCrc(aStream);

  finally
    Chunk.Free;
    sl.Free;
  end;
end;

procedure TG2FilePerformance.SaveAsFXB( aStream : TStream);
var FXBHeader : TFXBHeader;
    MemStream : TMemoryStream;

    function SwapBytes(Value: Cardinal): Cardinal; register;
    asm
      BSWAP  EAX
    end;
    {begin
      // TDOD
    end;}

begin
  MemStream := TMemoryStream.Create;
  try
    SaveToFile(MemStream);

    FXBHeader.chunkMagic  := 'CcnK';
    FXBHeader.byteSize    := SwapBytes(152 + MemStream.Size);
    FXBHeader.fxMagic     := 'FBCh';
    FXBHeader.version     := SwapBytes(1);
    FXBHeader.fxID        := 'NMG2';
    FXBHeader.fxVersion   := SwapBytes(1);
    FXBHeader.numPrograms := SwapBytes(1);
    Fillchar(FXBHeader.Future, 128, #0);

    FXBHeader.chunkSize   := SwapBytes(MemStream.Size);

    aStream.Write(FXBHeader, SizeOf(FXBHeader));
    aStream.Write(MemStream.Memory^, MemStream.Size);

  finally
    MemStream.Free;
  end;
end;

function TG2FilePerformance.LoadFromFXB( aStream : TStream): boolean;
var FXBHeader : TFXBHeader;
    MemStream : TMemoryStream;

    function SwapBytes(Value: Cardinal): Cardinal; register;
    asm
      BSWAP  EAX
    end;
    {begin
      // TDOD
    end;}

begin
  Result := False;
  MemStream := TMemoryStream.Create;
  try
    aStream.Read( FXBHeader.chunkMagic, SizeOf( FXBHeader.chunkMagic));
    if FXBHeader.chunkMagic <> 'CcnK' then
      raise Exception.Create('Unknown filetype');
    aStream.Read( FXBHeader.byteSize, SizeOf( FXBHeader.byteSize));
    FXBHeader.byteSize := SwapBytes(FXBHeader.byteSize);
    aStream.Read( FXBHeader.fxMagic, SizeOf( FXBHeader.fxMagic));
    if FXBHeader.chunkMagic <> 'FBCh' then
      raise Exception.Create('Unknown filetype');
    aStream.Read( FXBHeader.version, SizeOf( FXBHeader.version));
    FXBHeader.version := SwapBytes(FXBHeader.version);
    aStream.Read( FXBHeader.fxID, SizeOf( FXBHeader.fxID));
    if FXBHeader.fxID <> 'NMG2' then
      raise Exception.Create('Unknown filetype');
    aStream.Read( FXBHeader.fxVersion, SizeOf( FXBHeader.fxVersion));
    FXBHeader.fxVersion := SwapBytes(FXBHeader.fxVersion);
    aStream.Read( FXBHeader.numPrograms, SizeOf( FXBHeader.numPrograms));
    FXBHeader.numPrograms := SwapBytes(FXBHeader.numPrograms);
    aStream.Read( FXBHeader.Future, 128);
    aStream.Read( FXBHeader.chunkSize, SizeOf(FXBHeader.chunkSize));
    FXBHeader.chunkSize := SwapBytes(FXBHeader.chunkSize);

    LoadFromFile( aStream, nil);

    Result := True;
  finally
    MemStream.Free;
  end;
end;

procedure TG2FilePerformance.DeleteModuleFromPerf( aSlotIndex : Byte; aLocation : TLocationType; aModule: TG2FileModule);
var aModuleIndex : Byte;
begin
  aModuleIndex := aModule.ModuleIndex;
  FGlobalKnobList.DeleteModule( aSlotIndex, ord(aLocation), aModuleIndex);
end;

function TG2FilePerformance.AssignGlobalKnobInPerf(aKnobIndex: integer; aSlotIndex: byte; aLocation : TLocationType; aModuleIndex, aParamIndex : byte): TGlobalKnob;
var Knob : TGlobalKnob;
begin
  Result := nil;

  if aKnobIndex < 0 then
    exit;

  if aKnobIndex < FGlobalKnobList.Count then begin
    Knob := FGlobalKnobList.Items[ aKnobIndex];
    Knob.FAssigned := 1;
    Knob.FLocation := ord(aLocation);
    Knob.FModuleIndex := aModuleIndex;
    Knob.FIsLed := 0;
    Knob.FParamIndex := aParamIndex;
    Knob.FSlotIndex := aSlotIndex;

    Result := Knob;
  end else
    raise Exception.Create('Knob index ' + IntToStr(aKnobIndex) + ' out of range.');
end;

procedure TG2FilePerformance.DeassignGlobalKnobInPerf( aKnobIndex: integer);
var Knob : TGlobalKnob;
begin
  if aKnobIndex < 0 then
    exit;

  if aKnobIndex < FGlobalKnobList.Count then begin
    Knob := FGlobalKnobList.Items[ aKnobIndex];
    Knob.FAssigned := 0;
  end else
    raise Exception.Create('Knob index ' + IntToStr(aKnobIndex) + ' out of range.');
end;

function TG2FilePerformance.GetGlobalKnob( KnobIndex: integer): TGlobalKnob;
begin
  if (KnobIndex >= 0) and (KnobIndex < FGlobalKnobList.Count) then begin
    Result := FGlobalKnobList.Items[ KnobIndex]
  end else
    Result := nil;
end;

////////////////////////////////////////////////////////////////////////////////
//  TG2DataStream
////////////////////////////////////////////////////////////////////////////////

constructor TG2FileDataStream.Create( AOwner: TComponent);
begin
  inherited;
end;

destructor  TG2FileDataStream.Destroy;
begin
  inherited;
end;

procedure TG2FileDataStream.Read( aChunk : TPatchChunk);
begin
  // Abstract
end;

procedure TG2FileDataStream.Write( aChunk : TPatchChunk; aVariationCount : byte);
begin
  // Abstract
end;


function TG2FileDataStream.LoadFileHeader( aStream : TStream; aChunk : TPatchChunk): boolean;
var sl : TStringList;
    s : AnsiString;
    c : AnsiChar;
begin
  Result := False;

  sl := TStringList.Create;
  try
    s := '';
    aStream.Position := 0;
    while aStream.Position < aStream.Size do begin
      aStream.Read( c, SizeOf( c));
      case c of
      #$0d :; // just skip it
      #$0a : begin
               sl.Add(string(s));
               s := '';
             end;
      #$00 : Break;
      else
        s := s + c;
      end;
    end;
    if s <> '' then
      sl.Add(string(s));

    aChunk.ReadBuffer( 2);
    FVersion := aChunk.ReadBits( 8);
    FPatchType := aChunk.ReadBits( 8);
  finally
    sl.Free;
  end;
end;

function TG2FileDataStream.ReadClaviaString( aStream : TStream): AnsiString;
var b : byte;
begin
  Result := '';
  aStream.Read(b, 1);
  while (b <> 0) do begin
    Result := Result + AnsiChar(b);
    if Length(Result) = 16 then
      break;
    aStream.Read(b, 1);
  end;
end;

procedure TG2FileDataStream.WriteClaviaString( aStream : TStream; aValue : AnsiString);
var i : integer;
    b : Byte;
begin
  i := 1;
  while (i <= Length(aValue)) and (i<=16) do begin
    if aStream.Write( aValue[i], 1) <> 1 then
      raise Exception.Create('Write error.');
    inc(i);
  end;
  if (i<=16) then begin
    b := 0;
    if aStream.Write(b, 1) <> 1 then
      raise Exception.Create('Write error.');
  end;
end;

function TG2FileDataStream.CreateFromMidiStream(const aStream: TStream; aLogLines : TStrings): TMemoryStream;
var MData, i, SlotIndex, Offs : integer;
    b : byte;
    FName        : AnsiString;
    BlockCount   : integer;
    CurrentBlock : integer;
    BitWriter    : TBitWriter;
    Line         : string;
begin
  BlockCount     := 0;
  CurrentBlock   := 0;
  Offs := 0;

  Result := TMemoryStream.Create;

  BitWriter := TBitWriter.Create;
  try
    aStream.Position := 0;
    while aStream.Position < aStream.Size do Begin
      Line := '';
      FChecksum := 0;
      ParseMidiConst( aStream, $f0);            // Start sysex
      ParseMidiConst( aStream, $33);            // Clavia
      ParseMidiConst( aStream, $7f);            // ?
      ParseMidiConst( aStream, $0a);            // G2

      b := ParseMidiSeptet( aStream);
      case b of
        $20 : PatchType := PATCH_DATA;
        $28 : PatchType := PERF_DATA;
        else
          raise Exception.Create( 'Invalid MIDI, data type changed during load');
      end;

      ParseMidiConst( aStream, $00);            // ?
      SlotIndex := ParseMidiSeptet(aStream);
      ParseMidiConst( aStream, $00);            // ?
      MData := ParseMidiInt( aStream);          // Block Number

      Line := Line + 'Block : ' + IntToStr(MData);

      if MData <> CurrentBlock then
        raise Exception.Create(Format( 'Invalid MIDI, block %d expected but block %d seen', [ CurrentBlock, MData]));

      MData := ParseMidiInt( aStream);          // Block count

      Line := Line + ' Count : ' + IntToStr(MData);

      if BlockCount = 0 then
        BlockCount := MData
      else
        if BlockCount <> MData then
          raise Exception.Create(Format( 'Block count changed during MIDI load (%d -> %d)', [ BlockCount, MData]));


      FName := ParseMidiString( aStream);       // Patch name

      Line := Line + ' Name : ' + string(FName) + ' Data : ';

      MData := ParseMidiInt( aStream);          // Septet count
      for i := 0 to MData - 2 do begin
        aStream.Read( b, 1);
        FChecksum := ( FChecksum + b) and $7f;
        BitWriter.WriteBits( Result, b, 7);
      end;
      aStream.Read( b, 1);
      FChecksum := ( FChecksum + b) and $7f;
      b := b shr (7-BitWriter.GetWriteBufferBitsLeft); {!}
      BitWriter.WriteBits( Result, b, BitWriter.GetWriteBufferBitsLeft);

      for i := Offs to Result.Size - 1 do
        Line := Line + IntToHex(PStaticByteBuffer(Result.Memory)^[i], 2) + ' ';
      Offs := Result.Size;


      if assigned(aLogLines) then
        aLogLines.Add(Line);

      MData := FChecksum;

      if ParseMidiSeptet( aStream) <> MData then // Checksum
        raise Exception.Create(Format( 'Invalid MIDI data, checksum error in block %d', [ CurrentBlock]));

      ParseMidiConst( aStream, $f7);            // End of sysex
      inc( CurrentBlock);
    end;
  finally
    BitWriter.Free;
  end;
end;

procedure TG2FileDataStream.CreateMidiBlock( aBlockNr, aBlockCount, anOffset, aSize: Integer; aChunk : TPatchChunk; aMidiStream : TStream);
var
  i, bits_left : integer;
  b         : Byte;
Begin
  // Writes one MIDI data block to Result

  FChecksum := 0;                                // Cheksum starts at 0
  AddMidiByte( aMidiStream, $f0);                // Header
  AddMidiByte( aMidiStream, $33);                // Header
  AddMidiByte( aMidiStream, $7f);                // Header
  AddMidiByte( aMidiStream, $0a);                // Header
  case PatchType of
    PATCH_DATA : AddMidiByte( aMidiStream, $20);
    PERF_DATA  : AddMidiByte( aMidiStream, $28);
  else
    raise Exception.Create('Unknown data type.');
  end;
  //AddMidiByte( aMidiStream, PatchType);
  AddMidiByte( aMidiStream, $00        );        // ?
  AddMidiByte( aMidiStream, $00        );        // ?
  AddMidiByte( aMidiStream, $00        );        // ?
  AddMidiInt ( aMidiStream, aBlockNr   );        // Block number
  AddMidiInt ( aMidiStream, aBlockCount);        // Block count
  AddMidiName( aMidiStream, 'No name'  );        // Patch name

  aSize := Min( (aChunk.GetReadBufferBitsLeft + 6) div 7, MIDI_BLOCK_SIZE);
  AddMidiInt( aMidiStream, aSize);   // Septet count

  i := 0;
  while i < aSize do begin
    if i = aSize - 1 then begin
      bits_left := aChunk.GetReadBufferBitsLeft mod 8;
      b := aChunk.ReadBits( bits_left);
      b := b shl (7 - bits_left); {! Tricky business}
    end else
      b := aChunk.ReadBits( 7) and $7f;
    AddMidiByte( aMidiStream, b);                // Data septets
    inc(i);
  end;

  b := FChecksum and $7f;
  AddMidiByte( aMidiStream, b);                  // Checksum
  AddMidiByte( aMidiStream, $f7);                // End of sysex
End;

procedure TG2FileDataStream.SaveMidiToStream( const aStream: TStream);
// Saves all data as a set of MIDI blocks to a MIDI file
var
  Offset     : Integer;     // Current septet start offset
  BlockNr    : Integer;     // Current MIDI block number
  BlockCount : Integer;     // Total block count
  Remaining  : Integer;     // Total of remaining septets tp handle
  SaveSize   : Integer;     // Save size for current block, in septets
  FileStream : TMemoryStream;
  Chunk      : TPatchChunk;
  b          : byte;
begin
  FileStream := TMemoryStream.Create;
  Chunk := TPatchChunk.Create( FileStream);
  try
    b := $00;
    FileStream.Write( b, 1);

    Chunk.WriteBits( Version, 8);
    Chunk.WriteBits( Patchtype, 8);
    Chunk.Flush;

    //MaxVariations := 9;

    if PatchType = PERF_DATA then // TODO
      (self as TG2FilePerformance).WriteSettings( Chunk);

    Write( Chunk, 9); // 9 Variations are written to file
    Chunk.WriteCrc( FileStream);

    FileStream.Position := 0;
    Chunk.ReadBuffer( FileStream.Size);
    Remaining  := FileStream.Size;
    //BlockCount := ( DataSize + MIDI_BLOCK_SIZE - 1) div MIDI_BLOCK_SIZE;
    BlockCount :=  (Chunk.GetReadBufferBitsLeft div 7 + MIDI_BLOCK_SIZE - 1) div MIDI_BLOCK_SIZE;
    BlockNr    :=  0;
    Offset     :=  0;
    SaveSize   := Min( Remaining, MIDI_BLOCK_SIZE);
    //while Remaining > 0 do begin
    while Chunk.GetReadBufferBitsLeft > 0 do begin

      CreateMidiBlock( BlockNr, BlockCount, Offset, SaveSize, Chunk, aStream);
      Inc( BlockNr);
      Inc( Offset, MIDI_BLOCK_SIZE);
      Remaining := Remaining - SaveSize;
      SaveSize  := Min( Remaining, MIDI_BLOCK_SIZE);
    end;
  finally
    Chunk.Free;
    FileStream.Free;
  end;
end;

function TG2FileDataStream.ParseMidiConst( const aStream : TStream; aValue: Byte): Byte;
Var
  C : Byte;
Begin
// Reads a constant value from a MIDI file

  aStream.Read(C, 1);
  Result := C;
  if C <> aValue then
    raise Exception.Create(Format( 'Invalid MIDI dump, %.2x expected but %.2x read', [ aValue, C]));
  FChecksum := ( FChecksum + Result) And $7f;
end;

function TG2FileDataStream.ParseMidiInt( const aStream : TStream): Integer;
Begin
// Reads an Integer (0 .. 16383) value from a MIDI file

  Result := 128 * ParseMidiSeptet( aStream);
  Inc( Result, ParseMidiSeptet( aStream));
end;

function TG2FileDataStream.ParseMidiSeptet( const aStream : TStream): Byte;
Var
  C : Byte;
Begin
// Reads an arbitrary data value from a MIDI file

  aStream.Read(C, 1);
  if C > $7f then
    raise Exception.Create(Format( 'Invalid MIDI dump, %.2x is not MIDI data', [ C]));
  Result := C;
  FChecksum := ( FChecksum + Result) And $7f;
end;

function TG2FileDataStream.ParseMidiString( const aStream : TStream): AnsiString;
Var
  C : Byte;
  i : Integer;
Begin
// Reads a String[ 16]\0 from a MIDI file

  Result := '';
  for i := 1 to 16 do begin // Read the 16 characters
    C := parseMidiSeptet( aStream);
    if C <> 0 then // Don't add \0's to pascal string
      Result := Result + AnsiChar( C);
  end;
  ParseMidiConst( aStream, 0); // Read the closing \0
end;

procedure TG2FileDataStream.AddMidiByte( aStream : TStream; aByte: Byte);
begin
// Adds a MIDI byte to aData

  aStream.Write( aByte, 1);
  FChecksum := ( FChecksum + aByte) And $7f;
end;

procedure TG2FileDataStream.AddMidiInt( aStream : TStream; anInteger: Integer);
begin
// Adds an integer (0 .. 16383) to aData

  AddMidiByte( aStream, (anInteger shr 7) and $7f); // Write high septet
  AddMidiByte( aStream, anInteger and $7f);         // then  low  septet
end;

procedure TG2FileDataStream.AddMidiName( aStream : TStream; aName : AnsiString);
var i : Integer;
Begin
  // Adds the patch name as a String[ 16]\0 to aData

  for i := 1 to 16 do                            // Max 16 septets
    if i > Length( aName) then
      AddMidiByte( aStream, 0)
    else
      AddMidiByte( aStream, Byte( aName[ i]) And $7f);
  AddMidiByte( aStream, $00);                    // and finalizing \0
End;


class function TG2FileDataStream.LoadFileData( AOwner : TComponent; aStream: TStream; aLogLines : TStrings): TG2FileDataStream;
var Chunk : TPatchChunk;
    bm, bl : Byte;
    Crc : Word;
    G2FileDataStream : TG2FileDataStream;
begin
  Chunk := TPatchChunk.Create( aStream);
  G2FileDataStream := TG2FileDataStream.Create( AOwner);
  try
    if assigned( aLogLines) then
      Chunk.FLogLines := aLogLines;

    G2FileDataStream.LoadFileHeader( aStream, Chunk);

    if G2FileDataStream.FPatchType = PATCH_DATA then
      Result := TG2FilePatch.LoadFileStream( AOwner, aStream, Chunk)
    else
      Result := TG2FilePerformance.LoadFileStream( AOwner, aStream, Chunk);

    Result.FVersion := G2FileDataStream.FVersion;
    Result.FPatchType := G2FileDataStream.FPatchType;

    aStream.Read(bm, 1);
    aStream.Read(bl, 1);
    Crc := bm * 256 + bl;

    if Crc <> Chunk.FReadCrc then
      raise Exception.Create('Crc error.');

  finally
    G2FileDataStream.Free;
    Chunk.Free;
  end;
end;

class function TG2FileDataStream.LoadMidiData( AOwner : TComponent; aStream: TStream; aLogLines : TStrings): TG2FileDataStream;
var Chunk            : TPatchChunk;
    b                : Byte;
    MemStream        : TMemoryStream;
    G2FileDataStream : TG2FileDataStream;
begin
  G2FileDataStream := TG2FileDataStream.Create( AOwner);
  try
    MemStream := G2FileDataStream.CreateFromMidiStream( aStream, aLogLines);

    MemStream.Position := 0;
    //MemStream.SaveToFile('TestSysEx_converted.bin');
    Chunk := TPatchChunk.Create( MemStream);
    try
      if assigned( aLogLines) then
        Chunk.FLogLines := aLogLines;

      Chunk.ReadBuffer( 3);

      b := Chunk.ReadBits( 8);
      b := Chunk.ReadBits( 8);
      b := Chunk.ReadBits( 8);

      if G2FileDataStream.PatchType = PATCH_DATA then
        Result := TG2FilePatch.LoadFileStream( AOwner, aStream, Chunk)
      else
        Result := TG2FilePerformance.LoadFileStream( AOwner, aStream, Chunk);

      Result.Version := G2FileDataStream.Version;
      Result.PatchType := G2FileDataStream.PatchType;

    finally
      Chunk.Free;
      MemStream.Free;
    end;

  finally
    G2FileDataStream.Free;
  end;
end;

class function TG2FileDataStream.LoadFileStream( AOwner: TComponent; aStream : TStream; aChunk : TPatchChunk): TG2FileDataStream;
begin
  // Abstract
  Result := nil;
end;

class function TG2FileDataStream.LoadMidiStream( AOwner : TComponent; aStream: TStream): TG2FileDataStream;
begin
  // Abstract
  Result := nil;
end;


////////////////////////////////////////////////////////////////////////////////
//  TBankList
////////////////////////////////////////////////////////////////////////////////

constructor TBankList.Create( AOwnsObjects : boolean);
begin
  inherited;
end;

destructor TBankList.Destroy;
begin
  inherited;
end;

function TBankList.GetBankItem( aIndex : integer) : TBankItem;
begin
  result := inherited Items[aindex] as TBankItem;
end;

procedure TBankList.SetBankItem( aIndex : integer; const aValue : TBankItem);
begin
  inherited Items[aindex] := aValue;
end;

function TBankList.AddBankItem( aValue : TBankItem): integer;
begin
  Result := inherited Add( aValue);
end;


////////////////////////////////////////////////////////////////////////////////
//  TG2File
////////////////////////////////////////////////////////////////////////////////

constructor TG2File.Create( AOwner: TComponent);
begin
  inherited;

  InitializeCriticalSection(FLogLock);
  FLogLines := TStringList.Create;
  FLogLevel := 0;

  FBanks := TStringList.Create;
  FXMLModuleDefs := TXMLDocument.Create;
  FXMLParamDefs := TXMLDocument.Create;
  FModuleDefList := nil;
  FParamDefList := nil;

  FPerformance := CreatePerformance;
  FPerformance.G2 := self;

  FBankList := TBankList.Create( True);
end;

destructor TG2File.Destroy;
begin
  FBankList.Free;

  if assigned(FModuleDefList) then
    FreeAndNil(FModuleDefList);

  if assigned( FParamDefList) then
    FreeAndNil(FParamDefList);

  FXMLParamDefs.Free;
  FXMLModuleDefs.Free;

  FBanks.Free;

  EnterCriticalSection(FLogLock);
  try
    FLogLines.Free;
  finally
    LeaveCriticalSection(FLogLock);
  end;
  DeleteCriticalSection(FLogLock);

  inherited;
end;

function TG2File.CreatePerformance : TG2FilePerformance;
begin
  Result := TG2FilePerformance.Create(self);
end;

procedure TG2File.SetPerformance(aValue: TG2FilePerformance);
begin
  FPerformance := aValue;
  if assigned(FPerformance) then
    FPerformance.G2 := self;
end;

procedure TG2File.LoadModuleDefs( Path : string);
var FModuleDefsFileName, FParamDefsFileName : string;
begin
  FLogFilePath := Path;
  FModuleDefsFileName := Path + 'ModuleDef.xml';
  FParamDefsFileName := Path + 'ParamDef.xml';

  if not(FileExists( FModuleDefsFileName)) then
    raise Exception.Create('Module definitions xml file not found : ' + FModuleDefsFileName);

  if not(FileExists( FParamDefsFileName)) then
    raise Exception.Create('Parameter definitions xml file not found : ' + FParamDefsFileName);

  ReadXMLFile( FXMLModuleDefs, FModuleDefsFileName);
  FModuleDefList := TXMLModuleDefListType.Create( FXMLModuleDefs.FirstChild);

  ReadXMLFile( FXMLParamDefs, FParamDefsFileName);
  FParamDefList := TXMLParamDefListType.Create( FXMLParamDefs.FirstChild);
end;

function TG2File.TextFunction( No : integer; Param1, Param2, Param3 : byte): string;
begin
  case No of
  0 : begin
        case Param1 of
        0 : Result := 'Legato';
        1 : Result := 'Mono';
        else
          Result := IntToStr(Param1);
        end;
      end;

  end;
end;

procedure TG2File.SetLogLevel( aValue : integer);
begin
  FLogLevel := aValue;
end;

function TG2File.GetID : integer;
begin
  Result := 0;
end;

procedure TG2File.add_log_line( tekst: string; log_cmd : integer);
begin
  EnterCriticalSection(FLogLock);
  try
    if FLogLevel = 0 then
      exit;

    case log_cmd of
    LOGCMD_NUL: FLogLines.Add(tekst);
    LOGCMD_HDR:
      begin
        FLogLines.Add('');
        FLogLines.Add(stringofchar('=', Length(tekst)));
        FLogLines.Add(tekst);
      end;
    LOGCMD_TAB:
      FLogLines.Add('    ' + tekst);
    LOGCMD_ERR:
      FLogLines.Add('ERROR : ' + tekst);
    end;

  finally
    LeaveCriticalSection(FLogLock);
  end;
end;

procedure TG2File.dump_buffer( var buffer; max_size : integer);
type buf_type = packed array[0..65536 - 1] of byte;
var p, i, c : integer;
    char_line, line : string;
begin
  if FLogLevel = 0 then
    exit;

  EnterCriticalSection(FLogLock);
  try

    c := 0;
    i := 0;
    p := 0;
    line := '';
    char_line := '';;
    while (i<max_size) do begin
      if c < 16 then begin
        line := line + IntToHex(buf_type(buffer)[i], 2) + ' ';
        if buf_type(buffer)[i] >= 32 then
          char_line := char_line + chr(buf_type(buffer)[i])
        else
          char_line := char_line + '.';
        inc(c);
        inc(i);
      end else begin
        FLogLines.Add(IntToHex(p, 6) + ' - ' + line + ' ' + char_line);
        p := i;
        c := 0;
        line := '';
        char_line := '';
      end;
    end;
    if c <> 0 then
      FLogLines.Add(IntToHex(p, 6) + ' - ' + line + stringofchar(' ', 16*3 - Length(line) + 1) + char_line);
  finally
    LeaveCriticalSection(FLogLock);
  end;
end;

procedure TG2File.save_log;
begin
  EnterCriticalSection(FLogLock);
  try
    FLogLines.SaveToFile( FLogFilePath + 'VSTLog_' + IntToStr(GetTickCount) + '.txt'); // TODO
  finally
    LeaveCriticalSection(FLogLock);
  end;
end;

procedure TG2File.AssignLog( Lines : TStrings);
begin
  EnterCriticalSection(FLogLock);
  try
    Lines.Assign(FLogLines);
  finally
    LeaveCriticalSection(FLogLock);
  end;
end;

procedure TG2File.ClearLog;
begin
  EnterCriticalSection(FLogLock);
  try
    FLogLines.Clear;
  finally
    LeaveCriticalSection(FLogLock);
  end;
end;


end.
