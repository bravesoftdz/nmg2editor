unit g2_mess;

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
//  This unit contains the G2 classes to change files through messages
//
//  ////////////////////////////////////////////////////////////////////////////


interface

uses
{$IFDEF VER230}
  System.Contnrs,
{$ELSE}
  Contnrs,
{$ENDIF}
  Windows, Classes, SysUtils, g2_types, g2_database, g2_file;

type
  TMidiCCRecieveEvent = procedure(Sender : TObject; SenderID : integer; MidiCC : byte) of Object;
  TSelectParamPageEvent = procedure( Sender: TObject; SenderID : integer; ParamPage : integer) of object;
  TPatchUpdateEvent = procedure(Sender: TObject; SenderID : integer; PatchIndex : integer) of object;
  TVariationChangeEvent = procedure(Sender: TObject; SenderID : integer; Slot, Variation : integer) of object;
  TSynthSettingsUpdateEvent = procedure( Sender : TObject; SenderID : integer) of Object;
  TStartStopCommunicationEvent = procedure(Sender: TObject; SenderID : integer; Stop : byte) of object;
  TPerfSettingsUpdateEvent = procedure(Sender: TObject; SenderID : integer; PerfMode : boolean) of object;
  TAfterRetrievePatch = procedure(Sender: TObject; SenderID : integer; aSlot, aBank, aPatch : byte) of object;
  TParamChangeMessEvent = procedure(Sender: TObject; SenderID : integer; Slot, Variation : byte; Location : TLocationType; ModuleIndex, ParamIndex : byte; aValue : byte) of object;

  TG2MessPerformance = class;
  TG2MessSlot = class;
  TG2MessPatch = class;

  TG2Message = class( TMemoryStream)
    constructor Create;
    destructor  Destroy; override;
    function    WriteMessage( value : byte): boolean;
    procedure   WriteClaviaString( aValue : AnsiString);
  end;

  TG2SendMessage = class( TG2Message)
  private
    FAddReversed : boolean;
    FOffset : integer;
    function    GetCommand : byte;
    function    GetHasResponse: boolean;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   PrepareForSend;
    procedure   AddToFront( SubMessage : TG2Message);
    procedure   Add( SubMessage : TG2Message);

    property Command : byte read GetCommand;
    property HasResponse : boolean read GetHasResponse;
    property AddReversed : boolean read FAddReversed write FAddReversed; // For undo
    property Offset : integer read FOffset write FOffset;
  end;

  TG2ResponseMessage = class( TG2Message)
  private
    function    GetCommand : byte;
    function    GetIsEmbedded : boolean;
    function    GetIsLedData : boolean;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   CalcCRC;

    property Command : byte read GetCommand;
    property IsEmbedded : boolean read GetIsEmbedded;
    property IsLedData : boolean read GetIsLedData;
  end;

  TNextBankListCmd = record
    Cmd   : Byte;
    Mode  : Byte;
    Bank  : Byte;
    Patch : Byte;
  end;

  TG2Mess = class( TG2File)
  private
    // Synth settings
    FSynthName            : AnsiString;
    FMemoryProtect        : TBits1;
    FMidiChannelA         : TBits8; // 16 : Inactive
    FMidiChannelB         : TBits8; // 16 : Inactive
    FMidiChannelC         : TBits8; // 16 : Inactive
    FMidiChannelD         : TBits8; // 16 : Inactive
    FMidiGlobalChannel    : TBits8; // 16 : Off
    FSysExID              : TBits8; // 16 : All
    FLocalOn              : TBits1;
    FProgramChangeReceive : TBits1;
    FProgramChangeSend    : TBits1;
    FControllersReceive   : TBits1;
    FControllersSend      : TBits1;
    FSendClock            : TBits1;
    FIgnoreExternalClock  : TBits1;
    FTuneCent             : TBits8; // -100..100 (9C..64)
    FGlobalOctaveShiftActive : TBits1;
    FGlobalOctaveShift    : TBits8; // -2..2 (FE..02)
    FTuneSemi             : TBits8; // -6..6
    FPedalPolarity        : TBits1; // Open 0, Closed 1
    FControlPedalGain     : TBits8; // 0..32

    //FInitStep : integer;
    //FInitialized : boolean;
    //FProcessLedData : boolean;

    FOnMidiCCReceive          : TMidiCCRecieveEvent;
    FOnSelectParamPage        : TSelectParamPageEvent;
    FOnPatchUpdate            : TPatchUpdateEvent;
    FOnVariationChange        : TVariationChangeEvent;
    FOnSynthSettingsUpdate    : TSynthSettingsUpdateEvent;
    FOnStartStopCommunication : TStartStopCommunicationEvent;
    FOnPerfSettingsUpdate     : TPerfSettingsUpdateEvent;
    FOnAfterRetrievePatch     : TAfterRetrievePatch;
    FOnParamChangeMessage     : TParamChangeMessEvent;

    FErrorMessage        : boolean;
    FErrorMessageNo      : integer;

    procedure   SetSynthName( aValue : AnsiString);
    procedure   SetMemoryProtect( aValue : TBits1);
    procedure   SetMidiChannelA( aValue : TBits8);
    procedure   SetMidiChannelB( aValue : TBits8);
    procedure   SetMidiChannelC( aValue : TBits8);
    procedure   SetMidiChannelD( aValue : TBits8);
    procedure   SetMidiGlobalChannel( aValue : TBits8);
    procedure   SetSysExID( aValue : TBits8);
    procedure   SetLocalOn( aValue :  TBits1);
    procedure   SetProgramChangeReceive( aValue : TBits1);
    procedure   SetProgramChangeSend( aValue : TBits1);
    procedure   SetControllersReceive( aValue : TBits1);
    procedure   SetControllersSend( aValue : TBits1);
    procedure   SetSendClock( aValue : TBits1);
    procedure   SetIgnoreExternalClock( aValue : TBits1);
    procedure   SetTuneCent( aValue : TBits8);
    procedure   SetGlobalOctaveShiftActive( aValue : TBits1);
    procedure   SetGlobalOctaveShift( aValue : TBits8);
    procedure   SetTuneSemi( aValue : TBits8);
    procedure   SetPedalPolarity( aValue : TBits1);
    procedure   SetControlPedalGain( aValue : TBits8);
  public
    FLogPatchFileChunks  : boolean;
    NextBankListCmd : TNextBankListCmd;

    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;

    function    GetPerformance : TG2MessPerformance;
    function    GetSlot( aSlot : byte) : TG2MessSlot;

    // Client/server communication
    function    CreateInitResponseMessage: TG2ResponseMessage;
    function    CreateVersionCounterResponseMessage( aSlot : byte): TG2ResponseMessage;
    function    CreateOkResponseMessage: TG2ResponseMessage;
    function    CreateSynthSettingsResponseMessage: TG2ResponseMessage;
    function    CreateUnknown1ResponseMessage: TG2ResponseMessage;
    function    CreatePerfSettingsResponseMessage: TG2ResponseMessage;
    function    CreateUnknown2ResponseMessage: TG2ResponseMessage;
    function    CreateGetGlobalKnobsResponseMessage: TG2ResponseMessage;

    function    CreateResponseMessage( ClientMessage : TG2SendMessage; var Initialized : boolean): TG2ResponseMessage;
    function    ProcessResponseMessage( MemStream : TMemoryStream; Param : byte): boolean;
    function    ProcessSendMessage( MemStream : TMemoryStream; SenderID : integer): boolean;

    function    CreateInitMessage: TG2SendMessage;
    function    CreateStartStopCommunicationMessage( Stop : byte): TG2SendMessage;
    function    CreateGetPatchVersionMessage: TG2SendMessage;
    function    CreateGetSynthSettingsMessage: TG2SendMessage;
    procedure   AddSetSynthSettingsMessage( SendMessage : TG2Message);
    function    CreateSetSynthSettingsMessage: TG2SendMessage;
    function    CreateUnknown1Message: TG2SendMessage;
    function    CreateMidiDumpMessage: TG2SendMessage;
    function    CreateListMessage( aMode, aBank, aPatch : byte; names : TStrings): TG2SendMessage;
    function    CreateSetModeMessage( aMode : byte): TG2SendMessage;
    function    CreateNoteMessage( aNote : byte; aOnoff : byte): TG2SendMessage;

    property    SynthName : AnsiString read FSynthName write SetSynthName;
    property    MemoryProtect : TBits1 read FMemoryProtect write SetMemoryProtect;
    property    MidiChannelA : TBits8 read FMidiChannelA write SetMidiChannelA;
    property    MidiChannelB : TBits8 read FMidiChannelB write SetMidiChannelB;
    property    MidiChannelC : TBits8 read FMidiChannelC write SetMidiChannelC;
    property    MidiChannelD : TBits8 read FMidiChannelD write SetMidiChannelD;
    property    MidiGlobalChannel : TBits8 read FMidiGlobalChannel write SetMidiGlobalChannel;
    property    SysExID : TBits8 read FSysExID write SetSysExID;
    property    LocalOn : TBits1 read FLocalOn write SetLocalOn;
    property    ProgramChangeReceive : TBits1 read FProgramChangeReceive write SetProgramChangeReceive;
    property    ProgramChangeSend : TBits1 read FProgramChangeSend write SetProgramChangeSend;
    property    ControllersReceive : TBits1 read FControllersReceive write SetControllersReceive;
    property    ControllersSend : TBits1 read FControllersSend write SetControllersSend;
    property    SendClock : TBits1 read FSendClock write SetSendClock;
    property    IgnoreExternalClock : TBits1 read FIgnoreExternalClock write SetIgnoreExternalClock;
    property    TuneCent : TBits8 read FTuneCent write SetTuneCent;
    property    GlobalOctaveShiftActive : TBits1 read FGlobalOctaveShiftActive write SetGlobalOctaveShiftActive;
    property    GlobalOctaveShift : TBits8 read FGlobalOctaveShift write SetGlobalOctaveShift;
    property    TuneSemi : TBits8 read FTuneSemi write SetTuneSemi;
    property    PedalPolarity : TBits1 read FPedalPolarity write SetPedalPolarity;
    property    ControlPedalGain : TBits8 read FControlPedalGain write SetControlPedalGain;
  published
    property    OnMidiCCReceive : TMidiCCRecieveEvent read FOnMidiCCReceive write FOnMidiCCReceive;
    property    OnSelectParamPage : TSelectParamPageEvent read FOnSelectParamPage write FOnSelectParamPage;
    property    OnPatchUpdate : TPatchUpdateEvent read FOnPatchUpdate write FOnPatchUpdate;
    property    OnVariationChange : TVariationChangeEvent read FOnVariationChange write FOnVariationChange;
    property    OnStartStopCommunication : TStartStopCommunicationEvent read FOnStartStopCommunication write FOnStartStopCommunication;
    property    OnPerfSettingsUpdate : TPerfSettingsUpdateEvent read FOnPerfSettingsUpdate write FOnPerfSettingsUpdate;
    property    OnSynthSettingsUpdate : TSynthSettingsUpdateEvent read FOnSynthSettingsUpdate write FOnSynthSettingsUpdate;
    property    OnAfterRetrievePatch : TAfterRetrievePatch read FOnAfterRetrievePatch write FOnAfterRetrievePatch;
    property    OnParamChangeMessage : TParamChangeMessEvent read FOnParamChangeMessage write FOnParamChangeMessage;
  end;

  TG2MessPerformance = class( TG2FilePerformance)
  protected
    FPerfVersion    : Byte;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   add_log_line(tekst : string; log_cmd : integer);
    function    GetSlot( aSlot : byte) : TG2MessSlot;
    function    ProcessSendMessage( MemStream: TMemoryStream; SenderID : integer): boolean;
    function    CreateGetPerfSettingsMessage: TG2SendMessage;
    function    CreateUnknown2Message: TG2SendMessage;
    function    CreateSelectSlotMessage( aSlot: byte): TG2SendMessage;
    function    CreateRetrieveMessage( aSlot, aBank, aPatch : byte): TG2SendMessage;
    function    CreateStoreMessage( aSlot, aBank, aPatch : byte): TG2SendMessage;
    function    CreateSetPerformanceMessage( aPerfName : AnsiString; aPerf : TG2FilePerformance): TG2SendMessage;
    function    CreateSetPerfSettingsMessage: TG2SendMessage;
    function    CreateSetPerfNameMessage( aPerfName : AnsiString): TG2SendMessage;
    function    CreateGetGlobalKnobsMessage: TG2SendMessage;
  end;

  TG2MessSlot = class( TG2FileSlot)
  protected
    FPatchVersion      : Byte;
    FOnDeleteModule    : TDeleteModuleEvent;
    FOnPatchUpdate     : TPatchUpdateEvent;
    FOnVariationChange : TVariationChangeEvent;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure   add_log_line(tekst : string; log_cmd : integer);
    function    GetPatch : TG2MessPatch;
    function    GetPerformance : TG2MessPerformance;

    function    CreateGetPatchResponseMessage: TG2ResponseMessage;
    function    CreateGetPatchNameResponseMessage: TG2ResponseMessage;
    function    CreateCurrentNoteResponseMessage: TG2ResponseMessage;
    function    CreatePatchTextResponseMessage: TG2ResponseMessage;
    function    CreateResourcesUsedResponseMessage( aLocation : TLocationType): TG2ResponseMessage;
    function    CreateGetSelectedParamResponseMessage: TG2ResponseMessage;
    function    CreateResponseMessage( ClientMessage : TG2SendMessage): TG2ResponseMessage;
    function    CreateOkResponseMessage: TG2ResponseMessage;

    function    CreateGetPatchVersionMessage: TG2SendMessage;
    function    CreatePatchNotesMessage: TG2SendMessage;
    function    CreateSendControllerSnapshotMessage: TG2SendMessage;
    function    CreateResourceTableMessage( aLocation : Byte): TG2SendMessage;
    function    CreateGetPatchNameMessage: TG2SendMessage;
    function    CreateCurrentNoteMessage: TG2SendMessage;
    function    CreateUnknown6Message: TG2SendMessage;
    function    CreateGetSelectedParameterMessage: TG2SendMessage;
    function    CreateSetPatchMessage( aPatchName : AnsiString; aPatch : TG2FilePatch): TG2SendMessage;
    function    CreateGetPatchMessage: TG2SendMessage;
    function    CreateSelectVariationMessage( aVariationIndex: byte): TG2SendMessage;
    function    CreateSetParamMessage( aLocation, aModule, aParam, aValue, aVariation: byte): TG2SendMessage;
    function    CreateSelParamMessage( aLocation, aModule, aParam: integer): TG2SendMessage;
    function    CreateSetMorphMessage( aLocation, aModule, aParam, aMorph, aValue, aNegative, aVariation: byte): TG2SendMessage;
    function    CreateSetModeMessage( aLocation, aModule, aParam, aValue: integer): TG2SendMessage;
    function    CreateCopyVariationMessage( aFromVariation, aToVariation : byte): TG2SendMessage;

    function    ProcessResponseMessage( MemStream : TMemoryStream; Param : byte): boolean; dynamic;
    function    ProcessSendMessage( MemStream : TMemoryStream; SenderID : integer): boolean;  dynamic;

    property    PatchVersion : byte read FPatchVersion write FPatchVersion;
    property    OnDeleteModule : TDeleteModuleEvent read FOnDeleteModule write FOnDeleteModule;
    property    OnPatchUpdate : TPatchUpdateEvent read FOnPatchUpdate write FOnPatchUpdate;
    property    OnVariationChange : TVariationChangeEvent read FOnVariationChange write FOnVariationChange;
  end;

  TG2MessPatch = class( TG2FilePatch)
  private
    FUndoMessage : TG2SendMessage;

    //function    GetG2 : TG2Mess;
    function    GetSlot : TG2MessSlot;
    function    GetPerformance : TG2MessPerformance;

    //procedure   SetVisible( aValue : boolean); virtual;

    function    FindCable( Location : TLocationType; FromModule : byte; FromConnector : byte; ToModule : byte; ToConnector : byte): TG2FileCable;

    procedure   AddNewModuleMessage( SendMessage : TG2SendMessage; aLocation : TLocationType; aNewModuleIndex, aModuleType, aCol, aRow: byte);
    procedure   AddCopyModuleMessage( SendMessage : TG2SendMessage; aLocation : TLocationType; aModule : TG2FileModule);
    procedure   AddCopyModuleParametersMessage( SendMessage : TG2SendMessage; aLocation : TLocationType; aModule : TG2FileModule);
    procedure   AddCopyModuleParamLabelsMessage( SendMessage : TG2SendMessage; aLocation: TLocationType; aModule : TG2FileModule);
    procedure   AddCopyModulesMessage( SendMessage : TG2SendMessage; aSrcePatch : TG2FilePatchPart; aFromLocation, aToLocation : TLocationType; RenumberModules : boolean);
    procedure   AddSetUprateMessage( SendMessage : TG2SendMessage; aModule : TG2FileModule; aUprateValue : byte);
    procedure   AddSetCableColorMessage( SendMessage : TG2SendMessage; aCable : TG2FileCable; aColor : byte);
    procedure   AddMoveModuleMessage( SendMessage : TG2SendMessage; aModule : TG2FileModule; aCol, aRow : byte);
    procedure   AddDeleteModuleMessage( SendMessage : TG2SendMessage; aLocation : TLocationType; aModuleIndex : byte);
    procedure   AddConnectionMessage( SendMessage : TG2SendMessage; aFromConnector, aToConnector : TG2FileConnector);
    procedure   AddDeleteConnectionMessage( SendMessage : TG2SendMessage; aLocation : TLocationType;
                                                   aFromModuleIndex : byte; aFromConnectorKind : TConnectorKind; aFromConnectorIndex : byte;
                                                   aToModuleIndex : byte; aToConnectorKind : TConnectorKind; aToConnectorIndex : byte);
    //procedure   AddDeleteConnectionMessage( SendMessage : TG2SendMessage; aFromConnector, aToConnector : TG2FileConnector);
    procedure   AddDeleteCableMessage( SendMessage : TG2SendMessage; aCable: TG2FileCable);
    procedure   AddAssignKnobMessage( SendMessage : TG2SendMessage; aLocation : TLocationType; aModule, aParam, aKnobIndex: integer);
    procedure   AddDeAssignKnobMessage( SendMessage : TG2SendMessage; aKnobIndex: integer);
    procedure   AddSelectParamPageMessage( SendMessage : TG2SendMessage; aPage: integer);
    procedure   AddAssignMidiCCMessage( SendMessage : TG2SendMessage; aLocation: TLocationType; aModule, aParam, aMidiCC: integer);
    procedure   AddDeassignMidiCCMessage( SendMessage : TG2SendMessage; aMidiCC: integer);
    procedure   AddAssignGlobalKnobMessage( SendMessage : TG2SendMessage; aLocation : TLocationType; aModule, aParam, aKnob: integer);
    procedure   AddSelectGlobalParamPageMessage( SendMessage : TG2SendMessage; aPage : integer);
    procedure   AddDeassignGlobalKnobMessage( SendMessage : TG2SendMessage; aKnob: integer);
    procedure   AddSetPatchDescriptionMessage( SendMessage : TG2SendMessage; FPatchDescription : TPatchDescription);
    procedure   AddSetModuleParamLabelsMessage( SendMessage : TG2SendMessage; aLocation: TLocationType; aModuleIndex, aParamIndex: byte; aName: AnsiString);
    procedure   AddSetModuleLabelMessage( SendMessage : TG2SendMessage; aLocation: TLocationType; aModuleIndex: byte; aName: AnsiString);
    procedure   AddSetModuleColorMessage( SendMessage : TG2SendMessage; aLocation: TLocationType; aModuleIndex, aColor : byte);
    procedure   AddSetMorphMessage( SendMessage : TG2SendMessage; aLocation, aModule, aParam, aMorph, aValue, aNegative, aVariation: byte);
  protected
    FUndoStack   : TList;

    function    CreatePatchMessage: TG2SendMessage;
    procedure   ResetUprateValues( aLocation : TLocationType);
    procedure   CheckUprateChange( aStream: TG2SendMessage; aUprateValue : Byte; aToConnector : TG2FileConnector; aToModule : TG2FileModule);
    function    CreateAddNewModuleMessage( aLocation : TLocationType; aNewModuleIndex, aModuleType, aCol, aRow: byte): TG2SendMessage;
    function    CreateCopyModulesMessage( aSrcePatch : TG2FilePatchPart; aFromLocation, aToLocation : TLocationType; RenumberModules : boolean): TG2SendMessage;
    function    CreateMoveModulesMessage: TG2SendMessage;
    function    CreateSetModuleColorMessage( aLocation: TLocationType; aModuleIndex, aColor : byte): TG2SendMessage;
    function    CreateDeleteModuleMessage( aLocation : TLocationType; aModuleIndex: byte): TG2SendMessage;
    function    CreateDeleteModulesMessage( aLocation : TLocationType): TG2SendMessage;
    function    CreateAddConnectionMessage( aFromConnector, aToConnector : TG2FileConnector): TG2SendMessage;
    function    CreateDeleteConnectionMessage( aCable: TG2FileCable): TG2SendMessage;
    function    CreateAssignKnobMessage( aLocation : TLocationType; aModule, aParam, aKnob: integer): TG2SendMessage;
    function    CreateDeassignKnobMessage( aKnob: integer): TG2SendMessage;
    function    CreateModuleAssignKnobs( aModule : TG2FileModule; aPageIndex : integer): TG2SendMessage;
    function    CreateModuleAssignGlobalKnobs( aModule : TG2FileModule; aPageIndex : integer): TG2SendMessage;
    function    CreateAssignGlobalKnobMessage( aLocation : TLocationType; aModule, aParam, aKnob: integer): TG2SendMessage;
    function    CreateDeassignGlobalKnobMessage( aKnob: integer): TG2SendMessage;
    function    CreateAssignMidiCCMessage( aLocation : TLocationType; aModule, aParam, aMidiCC: integer): TG2SendMessage;
    function    CreateDeassignMidiCCMessage( aMidiCC: integer): TG2SendMessage;
    function    CreateSetPatchDescriptionMessage( FPatchDescription : TPatchDescription): TG2SendMessage;
    function    CreateSetModuleParamLabelsMessage( aLocation : TLocationType; aModuleIndex, aParamIndex : byte; aName : AnsiString): TG2SendMessage;
    function    CreateSetModuleLabelMessage( aLocation : TLocationType; aModuleIndex : byte; aName : AnsiString): TG2SendMessage;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure   add_log_line(tekst : string; log_cmd : integer);

    procedure   PushUndoStack( MemStream : TG2SendMessage);
    function    PopUndoStack : TG2SendMessage;

    function    ProcessMessage( MemStream: TMemoryStream): boolean;

    procedure   SetMiniVULevel( Index : integer; aValue : byte); virtual;
    procedure   SetLedLevel( Index : integer; aValue : byte); virtual;
    function    GetMiniVUListCount : integer; virtual;
    function    GetLedListCount : integer; virtual;
    procedure   RemoveFromLedList( aLocation: TLocationType; aModuleIndex : integer); virtual;
  end;

implementation

////////////////////////////////////////////////////////////////////////////////
//  TG2SendMessage
////////////////////////////////////////////////////////////////////////////////

constructor TG2Message.Create;
begin
  inherited;
end;

destructor TG2Message.Destroy;
begin
  inherited;
end;

function TG2Message.WriteMessage( value : byte): boolean;
begin
  Result := Write( value, 1) = 1;
end;

procedure TG2Message.WriteClaviaString( aValue : AnsiString);
var i : integer;
    b : Byte;
begin
  i := 1;
  while (i <= Length(aValue)) and (i<=16) do begin
    if Write( aValue[i], 1) <> 1 then
      raise Exception.Create('Write error.');
    inc(i);
  end;
  if (i<=16) then begin
    b := 0;
    if Write(b, 1) <> 1 then
      raise Exception.Create('Write error.');
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//  TG2SendMessage
////////////////////////////////////////////////////////////////////////////////

constructor TG2SendMessage.Create;
begin
  inherited;
  FAddReversed := False;
  WriteMessage( 0); // Reserve place for message size
  WriteMessage( 0);
end;

destructor TG2SendMessage.Destroy;
begin
  inherited;
end;

function TG2SendMessage.GetCommand : byte;
begin
  Result := 0;
  if Size < 2 then // Invalid message!
    exit;

  // Get send message command
  Result := 0;
  if PStaticByteBuffer( Memory)^[2] = CMD_INIT then begin
    Result := CMD_INIT;
  end else
    if Size < 4 then begin
      exit; // Invalid message!
    end else
      if (PStaticByteBuffer( Memory)^[3] shr 4) = $02 then
        Result := PStaticByteBuffer( Memory)^[3] and $0F;
end;

function TG2SendMessage.GetHasResponse: boolean;
begin
  Result := False;

  if Size < 3 then // Invalid message!
    exit;

  if PStaticByteBuffer( Memory)^[2] = CMD_INIT then begin
    Result := True;
  end else
    if Size < 4 then begin
      // Invalid message!
      exit;
    end else
      Result := not ((PStaticByteBuffer(Memory)^[3] shr 4) = $03);
end;

procedure TG2SendMessage.PrepareForSend;
var Crc : Word;
    MessageSize : integer;
    b : byte;
begin
  MessageSize := ( Size + 2);
  PStaticByteBuffer( Memory)^[0] := MessageSize div 256;
  PStaticByteBuffer( Memory)^[1] := MessageSize mod 256;

  // Calc CRC
  Crc := 0;
  Position := 2;
  while Position < Size do begin
    Read(b, 1);
    Crc := CrcClavia(Crc, b);
  end;

  // Write CRC
  b := Crc div 256; Write(b, 1);
  b := Crc mod 256; Write(b, 1);
end;

procedure TG2SendMessage.AddToFront( SubMessage: TG2Message);
var OldSize : integer;
begin
  // Destination message holds space for size in front (2 bytes)
  OldSize := Size - FOffset;
  Size := Size + SubMessage.Size;

  move( PStaticByteBuffer(Memory)^[FOffset], PStaticByteBuffer(Memory)^[SubMessage.Size + FOffset], OldSize);
  move( PStaticByteBuffer(SubMessage.Memory)^[0], PStaticByteBuffer(Memory)^[FOffset], SubMessage.Size);
end;

procedure TG2SendMessage.Add( SubMessage : TG2Message);
var OldSize : integer;
begin
  if not FAddReversed then begin
    OldSize := Size;
    Size := Size + SubMessage.Size;
    move( PStaticByteBuffer(SubMessage.Memory)^[0], PStaticByteBuffer(Memory)^[OldSize], SubMessage.Size);
  end else
    AddToFront( SubMessage);
end;

////////////////////////////////////////////////////////////////////////////////
//  TG2ResponseMessage
////////////////////////////////////////////////////////////////////////////////

constructor TG2ResponseMessage.Create;
begin
  inherited;
end;

destructor TG2ResponseMessage.Destroy;
begin
  inherited;
end;

function TG2ResponseMessage.GetCommand: byte;
begin
  Result := 0;
  if Size < 1 then // Invalid message!
    exit;

  // Get the resonse command
  if PStaticByteBuffer( Memory)^[0] = CMD_INIT then
    Result := CMD_INIT
  else
    if Size < 4 then begin // Invalid message!
      exit;
    end else begin
      if (PStaticByteBuffer( Memory)^[0] and $f) = 2 then
        Result := PStaticByteBuffer( Memory)^[2]  // Embedded
      else
        Result := PStaticByteBuffer( Memory)^[1]; // Extended
    end;
end;

function TG2ResponseMessage.GetIsLedData: boolean;
begin
  Result := GetCommand in [$00, $01, $02, $03];
end;

procedure TG2ResponseMessage.CalcCRC;
var crc : word;
    b : byte;
begin
  // Calc CRC
  crc := 0;
  Position := 0;
  while Position < Size do begin
    Read(b, 1);
    crc := CrcClavia(crc, b);
  end;

  // Write CRC
  b := CRC div 256; Write(b, 1);
  b := CRC mod 256; Write(b, 1);

  Position := 0;
end;

function TG2ResponseMessage.GetIsEmbedded : boolean;
begin
  Result := (PStaticByteBuffer(Memory)^[0] and $f) = 2;
end;

////////////////////////////////////////////////////////////////////////////////
//  TG2Mess
////////////////////////////////////////////////////////////////////////////////

constructor TG2Mess.Create( AOwner: TComponent);
begin
  inherited Create( AOwner);

  FSynthName := 'G2 Engine';
  FMemoryProtect := 0;
  FMidiChannelA := 0;
  FMidiChannelB := 1;
  FMidiChannelC := 2;
  FMidiChannelD := 3;
  FMidiGlobalChannel := 15;
  FSysExID := 16;
  FLocalOn := 0;
  FProgramChangeReceive := 1;
  FProgramChangeSend := 0;
  FControllersReceive := 1;
  FControllersSend := 1;
  FSendClock := 0;
  FIgnoreExternalClock := 0;
  FTuneCent := 0;
  FGlobalOctaveShiftActive := 0;
  FGlobalOctaveShift := 0;
  FTuneSemi := 0;
  FPedalPolarity := 0;
  FControlPedalGain := 0;

  FLogPatchFileChunks := False;
end;

destructor TG2Mess.Destroy;
begin
  inherited;
end;

procedure TG2Mess.SetSynthName( aValue : AnsiString);
begin
  if Length(aValue) > 16 then
    raise Exception.Create('Synth name length max 16.');

  if aValue = '' then
    raise Exception.Create('Invalid synth name.');

  FSynthName := aValue;
end;

procedure TG2Mess.SetMemoryProtect( aValue : TBits1);
begin
  if aValue > 1 then
    raise Exception.Create('Memory protect must be 0 or 1.');

  FMemoryProtect := aValue;
end;

procedure TG2Mess.SetMidiChannelA( aValue : TBits8);
begin
  if aValue > 16 then
    raise Exception.Create('Midi channel must be between 0 and 16.');

  FMidiChannelA := aValue;
end;

procedure TG2Mess.SetMidiChannelB( aValue : TBits8);
begin
  if aValue > 16 then
    raise Exception.Create('Midi channel must be between 0 and 16.');

  FMidiChannelB := aValue;
end;

procedure TG2Mess.SetMidiChannelC( aValue : TBits8);
begin
  if aValue > 16 then
    raise Exception.Create('Midi channel must be between 0 and 16.');

  FMidiChannelC := aValue;
end;

procedure TG2Mess.SetMidiChannelD( aValue : TBits8);
begin
  if aValue > 16 then
    raise Exception.Create('Midi channel must be between 0 and 16.');

  FMidiChannelD := aValue;
end;

procedure TG2Mess.SetMidiGlobalChannel( aValue : TBits8);
begin
  if aValue > 16 then
    raise Exception.Create('Global midi channel must be between 0 and 16.');

  FMidiGlobalChannel := aValue;
end;

procedure TG2Mess.SetSysExID( aValue : TBits8);
begin
  if aValue > 16 then
    raise Exception.Create('Sysex ID must be between 0 and 16.');

  FSysExID := aValue;
end;

procedure TG2Mess.SetLocalOn( aValue :  TBits1);
begin
  if aValue > 1 then
    raise Exception.Create('Local on must be 0 or 1.');

  FLocalOn := aValue;
end;

procedure TG2Mess.SetProgramChangeReceive( aValue : TBits1);
begin
  if aValue > 1 then
    raise Exception.Create('Program change receive must be 0 or 1.');

  FProgramChangeReceive := aValue;
end;

procedure TG2Mess.SetProgramChangeSend( aValue : TBits1);
begin
  if aValue > 1 then
    raise Exception.Create('Program change send must be 0 or 1.');

  FProgramChangeSend := aValue;
end;

procedure TG2Mess.SetControllersReceive( aValue : TBits1);
begin
  if aValue > 1 then
    raise Exception.Create('Controllers receive must be 0 or 1.');

  FControllersReceive := aValue;
end;

procedure TG2Mess.SetControllersSend( aValue : TBits1);
begin
  if aValue > 1 then
    raise Exception.Create('Controllers send must be 0 or 1.');

  FControllersSend := aValue;
end;

procedure TG2Mess.SetSendClock( aValue : TBits1);
begin
  if aValue > 1 then
    raise Exception.Create('Send clock must be 0 or 1.');

  FSendClock := aValue;
end;

procedure TG2Mess.SetIgnoreExternalClock( aValue : TBits1);
begin
  if aValue > 1 then
    raise Exception.Create('Ignore external clock must be 0 or 1.');

  FIgnoreExternalClock := aValue;
end;

procedure TG2Mess.SetTuneCent( aValue : TBits8);
begin
  if (aValue > $64) and (aValue < $9c) then
    raise Exception.Create('Tune cent must be between -100 ($9c) and 100 ($64).');
  FTuneCent := aValue;
end;

procedure TG2Mess.SetGlobalOctaveShiftActive( aValue : TBits1);
begin
  if aValue > 1 then
    raise Exception.Create('Global shift active must be 0 or 1.');

  FGlobalOctaveShiftActive := aValue;
end;

procedure TG2Mess.SetGlobalOctaveShift( aValue : TBits8);
begin
 if (aValue > $02) and (aValue < $FE) then
    raise Exception.Create('Global shift active must be between -2 ($FE) or 2 ($02).');

  FGlobalOctaveShift := aValue;
end;

procedure TG2Mess.SetTuneSemi( aValue : TBits8);
begin
  if (aValue > $06) and (aValue < $FA) then
    raise Exception.Create('Tune semi must be between -6 ($FA) and 6 ($06).');

  FTuneSemi := aValue;
end;

procedure TG2Mess.SetPedalPolarity( aValue : TBits1);
begin
  if aValue > 1 then
    raise Exception.Create('Pedal polarity must be 0 or 1.');

  FPedalPolarity := aValue;
end;

procedure TG2Mess.SetControlPedalGain( aValue : TBits8);
begin
  if aValue > 32 then
    raise Exception.Create('Control pedal gain must be between 0 and 32.');
   FControlPedalGain := aValue;
end;

function TG2Mess.GetPerformance: TG2MessPerformance;
begin
  Result := Performance as TG2MessPErformance;
end;

function TG2Mess.GetSlot( aSlot: byte): TG2MessSlot;
begin
  Result := Performance.Slot[ aSlot] as TG2MessSlot;
end;

function TG2Mess.CreateInitResponseMessage: TG2ResponseMessage;
begin
  Result := TG2ResponseMessage.Create;
  Result.WriteMessage( $80);
  // TODO Send rest of message
end;

function TG2Mess.CreateVersionCounterResponseMessage( aSlot : byte): TG2ResponseMessage;
begin
  Result := TG2ResponseMessage.Create;
  Result.WriteMessage( $01);
  Result.WriteMessage( CMD_SYS);
  Result.WriteMessage( $40);
  Result.WriteMessage( $36);
  Result.WriteMessage( aSlot);
  if aSlot < 4 then
   Result.WriteMessage( GetSlot( aSlot).FPatchVersion)
 else
   Result.WriteMessage( GetPerformance.FPerfVersion);
end;

function TG2Mess.CreateOkResponseMessage: TG2ResponseMessage;
begin
  Result := TG2ResponseMessage.Create;
  Result.WriteMessage( $01);
  Result.WriteMessage( CMD_SYS);
  Result.WriteMessage( $00);
  Result.WriteMessage( $7f); // Ok
end;

function TG2Mess.CreateSynthSettingsResponseMessage: TG2ResponseMessage;
begin
  Result := TG2ResponseMessage.Create;

  Result.WriteMessage( $01);
  Result.WriteMessage( CMD_SYS);
  Result.WriteMessage( GetPerformance.FPerfVersion);
  Result.WriteMessage( S_SYNTH_SETTINGS);

  AddSetSynthSettingsMessage( Result);
end;

function TG2Mess.CreateUnknown1ResponseMessage: TG2ResponseMessage;
begin
  Result := TG2ResponseMessage.Create;
  Result.WriteMessage( $01);
  Result.WriteMessage( CMD_SYS);
  Result.WriteMessage( GetPerformance.FPerfVersion);
  Result.WriteMessage( $80);
  // TODO Send rest of message
end;

function TG2Mess.CreatePerfSettingsResponseMessage: TG2ResponseMessage;
var Chunk : TPatchChunk;
begin
  Result := TG2ResponseMessage.Create;
  Result.WriteMessage( $01);
  Result.WriteMessage( CMD_SYS);
  Result.WriteMessage( GetPerformance.FPerfVersion);
  Result.WriteMessage( C_PERF_NAME);
  Result.WriteClaviaString( GetPerformance.PerformanceName);
  Chunk := TPatchChunk.Create(Result);
  try
    GetPerformance.WriteSettings(Chunk);
    //GetPerformance.Write(Chunk, 10);
  finally
    Chunk.Free;
  end;
end;

function TG2Mess.CreateUnknown2ResponseMessage: TG2ResponseMessage;
begin
  Result := TG2ResponseMessage.Create;
  Result.WriteMessage( $01);
  Result.WriteMessage( CMD_SYS);
  Result.WriteMessage( GetPerformance.FPerfVersion);
  Result.WriteMessage( $1e);
  // TODO Send rest of message
end;

function TG2Mess.CreateGetGlobalKnobsResponseMessage: TG2ResponseMessage;
var Chunk : TPatchChunk;
begin
  Result := TG2ResponseMessage.Create;
  Result.WriteMessage( $01);
  Result.WriteMessage( CMD_SYS);
  Result.WriteMessage( GetPerformance.FPerfVersion);
  //Result.WriteMessage( MESS_KNOBS_GLOBAL);

  Chunk := TPatchChunk.Create(Result);
  try
    GetPerformance.GlobalKnobList.Write(Chunk);
    Chunk.WriteChunk( C_KNOBS_GLOBAL);
  finally
    Chunk.Free;
  end;
end;

function TG2MessSlot.CreateGetPatchResponseMessage: TG2ResponseMessage;
var Chunk : TPatchChunk;
begin
  Result := TG2ResponseMessage.Create;
  Result.WriteMessage( $01);
  Result.WriteMessage( SlotIndex + $08 ); //TODO
  Result.WriteMessage( FPatchVersion);

  Chunk := TPatchChunk.Create(Result);
  try
    GetPatch.Write(Chunk, GetPatch.PatchSettings.VariationCount);
  finally
    Chunk.Free;
  end;
end;

function TG2MessSlot.CreateGetPatchNameResponseMessage: TG2ResponseMessage;
begin
  Result := TG2ResponseMessage.Create;
  Result.WriteMessage( $01);
  Result.WriteMessage( SlotIndex + $08);
  Result.WriteMessage( FPatchVersion);
  Result.WriteMessage( R_PATCH_NAME);
  Result.WriteClaviaString( PatchName);
end;

function TG2MessSlot.CreateCurrentNoteResponseMessage: TG2ResponseMessage;
begin
  Result := TG2ResponseMessage.Create;
  Result.WriteMessage( $01);
  Result.WriteMessage( SlotIndex + $08);
  Result.WriteMessage( FPatchVersion);
  Result.WriteMessage( $69);
  // TODO Send rest of message
end;

function TG2MessSlot.CreatePatchTextResponseMessage: TG2ResponseMessage;
begin
  Result := TG2ResponseMessage.Create;
  Result.WriteMessage( $01);
  Result.WriteMessage( SlotIndex + $08);
  Result.WriteMessage( FPatchVersion);
  Result.WriteMessage( $6f);
  // TODO Send rest of message
end;

function TG2MessSlot.CreateResourcesUsedResponseMessage( aLocation : TLocationType): TG2ResponseMessage;
begin
  Result := TG2ResponseMessage.Create;

  Result.WriteMessage( $01);
  Result.WriteMessage( SlotIndex + $08);
  Result.WriteMessage( FPatchVersion);
  Result.WriteMessage( R_RESOURCES_USED);
  Result.WriteMessage( ord(aLocation));
  // TODO Send rest of message
end;

function TG2MessSlot.CreateGetSelectedParamResponseMessage: TG2ResponseMessage;
begin
  Result := TG2ResponseMessage.Create;
  Result.WriteMessage( $01);
  Result.WriteMessage( SlotIndex + $08);
  Result.WriteMessage( FPatchVersion);
  Result.WriteMessage( S_SEL_PARAM);
  // TODO Send rest of message
end;

function TG2MessSlot.CreateOkResponseMessage: TG2ResponseMessage;
begin
  Result := TG2ResponseMessage.Create;
  Result.WriteMessage( $01);
  Result.WriteMessage( SlotIndex + $08);
  Result.WriteMessage( FPatchVersion);
  Result.WriteMessage( $7f); // Ok
end;

function TG2Mess.CreateResponseMessage( ClientMessage : TG2SendMessage; var Initialized : boolean): TG2ResponseMessage;
// Process a message from a client and create a response
var Size : integer;
    Cmd, R, bh, bl, Version, SubCmd, SlotIndex, b : byte;
begin
  Result := nil;

  // Read size
  ClientMessage.Position := 0;
  ClientMessage.Read( bh, 1);
  ClientMessage.Read( bl, 1);
  Size := bh * 256 + bl;

  ClientMessage.Read( R, 1);
  case R of
  $80 : begin
          Result := CreateInitResponseMessage;
        end;
  $0C : begin
          //
        end;
  $01 : begin
          ClientMessage.Read( Cmd, 1);

          case (Cmd and $0f)  of
          CMD_SYS : begin
                      ClientMessage.Read( Version, 1);
                      ClientMessage.Read( SubCmd, 1);
                      case SubCmd of
                        S_SEL_SLOT :
                             begin
                             end;
                        Q_VERSION_CNT :
                             begin
                               ClientMessage.Read( SlotIndex, 1);
                               Result := CreateVersionCounterResponseMessage( SlotIndex);
                             end;
                        S_START_STOP_COM :
                            begin
                              ClientMessage.Read( b, 1);
                              Initialized := b = START_COMM;

                              Result := CreateOkResponseMessage;
                            end;
                        Q_SYNTH_SETTINGS :
                            begin
                              Result := CreateSynthSettingsResponseMessage;
                            end;
                        M_UNKNOWN_1 :
                            begin
                              Result := CreateUnknown1ResponseMessage;
                            end;
                        Q_PERF_SETTINGS :
                            begin
                              Result := CreatePerfSettingsResponseMessage;
                            end;
                        M_UNKNOWN_2 :
                            begin
                              Result := CreateUnknown2ResponseMessage;
                            end;
                        Q_GLOBAL_KNOBS :
                            begin
                              Result := CreateGetGlobalKnobsResponseMessage;
                            end;


                      end;
                    end;
          $08, $09, $0a, $0b :
                begin
                  SlotIndex := (Cmd and $0f) - $08;

                  Result := GetSlot( SlotIndex).CreateResponseMessage( ClientMessage);

                end;
          end;
        end;
  end;
end;

function TG2MessSlot.CreateResponseMessage( ClientMessage : TG2SendMessage): TG2ResponseMessage;
var SubCmd, Version, Location : byte;
begin
  Result := nil;
  ClientMessage.Read( Version, 1);
  ClientMessage.Read( SubCmd, 1);
  case SubCmd of
    Q_PATCH :
      begin
        Result := CreateGetPatchResponseMessage;
      end;
    Q_PATCH_NAME :
      begin
        Result := CreateGetPatchNameResponseMessage;
      end;
    Q_CURRENT_NOTE :
      begin
        Result := CreateCurrentNoteResponseMessage;
      end;
    Q_PATCH_TEXT :
      begin
        Result := CreatePatchTextResponseMessage;
      end;
    Q_RESOURCES_USED :
      begin
        ClientMessage.Read( Location, 1);
        Result := CreateResourcesUsedResponseMessage( TLocationType(Location));
      end;
    M_UNKNOWN_6 :
      begin
        Result := CreateOkResponseMessage;
      end;
    Q_SELECTED_PARAM :
      begin
        Result := CreateGetSelectedParamResponseMessage;
      end;
    {MESS_SET_PARAM :
      begin
        // Client sends parameter change, this is a responseless message so put in parameter update buffer
        ClientMessage.FMessage.Read( Location, 1);
        ClientMessage.FMessage.Read( ModuleIndex, 1);
        ClientMessage.FMessage.Read( Param, 1);
        ClientMessage.FMessage.Read( Value, 1);
        ClientMessage.FMessage.Read( Variation, 1);
        //GetSlot( Slot).AddParamUpdRec( MESS_SET_PARAM, Location, ModuleIndex, Param, 0, Value, 0, Variation, ClientMessage.FSocket.SocketHandle);
        GetSlot( Slot).AddParamUpdRec( MESS_SET_PARAM, Location, ModuleIndex, Param, 0, Value, 0, Variation, ClientMessage.FClientContext);
        Processed := True;
      end;}
  end;

end;

function TG2Mess.ProcessResponseMessage( MemStream : TMemoryStream; Param : byte): boolean;
var aR, aCmd, aVersion, aSubCmd, aSlot, b : byte;
    i : integer;
    Chunk : TPatchChunk;
    aMidiCC : byte;
    aMode, aBank, aPatch, aCategory : integer;
    patch_name, perf_name : AnsiString;
    BankItem : TBankItem;
    BitReader : TBitReader;
begin
  // Return True if message was processed
  Result := False;

  MemStream.Read( aR, 1);
  case aR of
  $80 : begin
          Result := True;
        end;
  $01 : begin
          MemStream.Read( aCmd, 1);
          case aCmd of
          $0c : begin
                  MemStream.Read(aVersion, 1);

                  case aVersion of
                  $40 : begin
                          Result := True;
                          repeat
                            MemStream.Read( aSubCmd, 1);
                            case aSubCmd of
                            $1f : begin
                                    MemStream.Read(b, 1); // Performance version?
                                    GetPerformance.FPerfVersion := b;
                                    i := 0;
                                    while (i < 4) and Result do begin

                                      MemStream.Read( b,     1); // $36
                                      MemStream.Read( aSlot, 1);
                                      MemStream.Read( b,     1);
                                      GetSlot( aSlot).FPatchVersion := b;

                                      inc(i);
                                    end;
                                    break;
                                  end;
                            $36 : begin
                                    MemStream.Read( aSlot, 1);
                                    case aSlot of
                                    $00..$03 :
                                      begin
                                        MemStream.Read( b, 1);
                                        GetSlot( aSlot).FPatchVersion := b;
                                      end;
                                    $04 :
                                      begin
                                        MemStream.Read( b, 1);
                                        GetPerformance.FPerfVersion := b;
                                      end;
                                    end;
                                  end;
                            $38 : begin // TODO : Follows after retrieving a patch, don't know all of it
                                    MemStream.Read( aSlot, 1);
                                    case aSlot of
                                    $00..$03 :
                                      begin
                                        MemStream.Read( b, 1);
                                        GetSlot( aSlot).FPatchVersion := b;
                                      end;
                                    $04 :
                                      begin
                                        MemStream.Read( b, 1);
                                        GetPerformance.FPerfVersion := b;
                                      end;
                                    end;
                                  end;
                              else begin
                                add_log_line('Unknown subcommand ' + IntToHex(aR, 2) + ' ' + IntToHex(aCmd, 2) + ' ' + IntToHex(aVersion, 2) + ' ' + IntToHex(aSubCmd, 2), LOGCMD_ERR);
                                Result := False;
                                break;
                              end;
                            end;
                          until MemStream.Position >= MemStream.Size - 2;
                        end;
                    else begin
                      if aVersion = GetPerformance.FPerfVersion then begin

                          MemStream.Read(aSubCmd, 1);
                          case aSubCmd of
                          S_SYNTH_SETTINGS :
                                begin // synth settings
                                  BitReader := TBitReader.Create;
                                  try
                                    FSynthName := GetPerformance.ReadClaviaString(MemStream);
                                    //while (length(FSynthName) < 16) and (MemStream.Read(b, 1) = 1) and (b <> 0) do begin
                                    //  FSynthName := FSynthName + char(b);
                                    //end;
                                    b := BitReader.ReadBits( MemStream, 8); // $80
                                    b := BitReader.ReadBits( MemStream, 8); // $00
                                    b := BitReader.ReadBits( MemStream, 8); // $00
                                    b := BitReader.ReadBits( MemStream, 8); // $00
                                    FMemoryProtect := BitReader.ReadBits( MemStream, 1);
                                    b := BitReader.ReadBits( MemStream, 7);
                                    FMidiChannelA := BitReader.ReadBits( MemStream, 8);
                                    FMidiChannelB := BitReader.ReadBits( MemStream, 8);
                                    FMidiChannelC := BitReader.ReadBits( MemStream, 8);
                                    FMidiChannelD := BitReader.ReadBits( MemStream, 8);
                                    FMidiGlobalChannel := BitReader.ReadBits( MemStream, 8);
                                    FSysExID := BitReader.ReadBits( MemStream, 8);
                                    FLocalOn := BitReader.ReadBits( MemStream, 1);
                                    b := BitReader.ReadBits( MemStream, 7);
                                    b := BitReader.ReadBits( MemStream, 6);
                                    FProgramChangeReceive := BitReader.ReadBits( MemStream, 1);
                                    FProgramChangeSend := BitReader.ReadBits( MemStream, 1);
                                    b := BitReader.ReadBits( MemStream, 6);
                                    FControllersReceive := BitReader.ReadBits( MemStream, 1);
                                    FControllersSend := BitReader.ReadBits( MemStream, 1);
                                    b := BitReader.ReadBits( MemStream, 1);
                                    FSendClock := BitReader.ReadBits( MemStream, 1);
                                    FIgnoreExternalClock := BitReader.ReadBits( MemStream, 1);
                                    b := BitReader.ReadBits( MemStream, 5);
                                    FTuneCent := BitReader.ReadBits( MemStream, 8);
                                    FGlobalOctaveShiftActive := BitReader.ReadBits( MemStream, 1);
                                    b := BitReader.ReadBits( MemStream, 7);
                                    FGlobalOctaveShift := BitReader.ReadBits( MemStream, 8);
                                    FTuneSemi := BitReader.ReadBits( MemStream, 8);
                                    b := BitReader.ReadBits( MemStream, 8);
                                    FPedalPolarity := BitReader.ReadBits( MemStream, 1);
                                    b := BitReader.ReadBits( MemStream, 7);
                                    FControlPedalGain := BitReader.ReadBits( MemStream, 8);

                                  finally
                                    BitReader.Free;
                                  end;

                                  Result := True;

                                  if assigned(FOnSynthSettingsUpdate) then
                                    FOnSynthSettingsUpdate(self, ID);
                                end;
                          R_LIST_NAMES :
                                begin // List names
                                  for i := 0 to 3 do
                                    MemStream.Read( b, 1); // read 4 unknown bytes

                                  MemStream.Read( aMode, 1);
                                  MemStream.Read( b, 1);   // unknown;
                                  MemStream.Read( aBank, 1);
                                  MemStream.Read( aPatch, 1);
                                  patch_name := '';
                                  while MemStream.Read(b, 1) = 1 do begin
                                    case b of
                                    $01 : begin
                                            i := 1;
                                          end;
                                    $02 : begin
                                            with NextBankListCmd do begin
                                              Cmd   := b;
                                              Mode  := aMode;
                                              Bank  := aBank;
                                              Patch := aPatch;
                                            end;
                                            break;
                                          end;
                                    $03 : begin // next bank
                                            MemStream.Read( aBank, 1);
                                            aPatch := 0;
                                            with NextBankListCmd do begin
                                              Cmd   := b;
                                              Mode  := aMode;
                                              Bank  := aBank;
                                              Patch := aPatch;
                                            end;
                                            break;
                                          end;
                                    $04 : begin // next mode  0 = patch, 1 = perf, 2 = finished
                                            inc( aMode);
                                            aBank := 0;
                                            aPatch := 0;
                                            with NextBankListCmd do begin
                                              Cmd   := b;
                                              Mode  := aMode;
                                              Bank  := aBank;
                                              Patch := aPatch;
                                            end;
                                            break;
                                          end;
                                    $05 : begin;
                                            inc( aPatch);
                                            with NextBankListCmd do begin
                                              Cmd   := b;
                                              Mode  := aMode;
                                              Bank  := aBank;
                                              Patch := aPatch;
                                            end;
                                            break;
                                          end
                                    else
                                      if b = $00 then begin
                                        // End of string
                                        MemStream.Read( aCategory, 1);
                                        FBanks.Add('Category : ' + IntToStr(aCategory) + ' Mode:' + IntToStr( aMode) + ' Bank: ' + IntToStr(aBank)
                                                   + ' Patch: ' + IntTostr( aPatch) + ' ' + string(patch_name));

                                        BankItem := TBankItem.Create;
                                        BankItem.Mode := aMode;
                                        BankItem.Bank := aBank;
                                        BankItem.Patch := aPatch;
                                        BankItem.PatchName := patch_name;
                                        BankItem.Category := aCategory;
                                        if (aMode = 0) and (aBank = 0) and (aPatch = 0) then
                                          BankList.Clear;
                                        BankList.AddBankItem( BankItem);

                                        patch_name := '';
                                        inc(aPatch);
                                      end else begin
                                        patch_name := patch_name + AnsiChar(b);
                                        if length(patch_name) = 16 then begin
                                          // String reached 16 chars
                                          MemStream.Read( aCategory, 1);
                                          FBanks.Add('Category : ' + IntToStr(aCategory) + ' Mode:' + IntToStr( aMode) + ' Bank: ' + IntToStr(aBank)
                                                     + ' Patch: ' + IntTostr( aPatch) + ' ' + string(patch_name));

                                          BankItem := TBankItem.Create;
                                          BankItem.Mode := aMode;
                                          BankItem.Bank := aBank;
                                          BankItem.Patch := aPatch;
                                          BankItem.PatchName := patch_name;
                                          if (aMode = 0) and (aBank = 0) and (aPatch = 0) then
                                            BankList.Clear;
                                          BankItem.Category := aCategory;
                                          BankList.AddBankItem( BankItem);

                                          patch_name := '';
                                          inc(aPatch);
                                        end;
                                      end;
                                    end;
                                  end;
                                  Result := True;
                                end;
                          $1e : begin // unknown_2
                                  Result := True;
                                end;
                          C_PERF_NAME,
                          C_PERF_SETTINGS :
                                begin // Performance settings
                                  // its actially first perf name and then the chunk
                                  perf_name := '';
                                  while (length(perf_name) < 16) and (MemStream.Read(b, 1) = 1) and (b <> 0) do begin
                                    perf_name := perf_name + AnsiChar(b);
                                  end;
                                  GetPerformance.PerformanceName := perf_name;

                                  Chunk := TPatchChunk.Create(MemStream);
                                  try
                                    Chunk.ReadChunk;
                                    // Parse the performance settings
                                    GetPerformance.Read( Chunk);

                                    Result := True;
                                  finally
                                    Chunk.Free;
                                  end;
                                end;
                          C_KNOBS_GLOBAL :
                                begin // Performance settings
                                  MemStream.Position := MemStream.Position - 1;
                                  Chunk := TPatchChunk.Create(MemStream);
                                  try
                                    if assigned(LogLines) then
                                      Chunk.FLogLines := LogLines;

                                    Chunk.ReadChunk;
                                    // Parse the performance settings
                                    GetPerformance.Read( Chunk);

                                    Result := True;
                                  finally
                                    Chunk.Free;
                                  end;
                                end;
                          R_OK : Result := True;
                          R_ERROR :
                                begin
                                  MemStream.Read(b, 1); // error no?
                                  FErrorMessage := True;
                                  FErrorMessageNo := b;

                                  add_log_line('G2 returns error ' + IntToHex(b, 2) + '!', LOGCMD_ERR);

                                  Result := True;
                                end;
                          $80 : begin // Unknown 1
                                  Result := True;
                                end;
                            else begin
                              add_log_line('Unknown subcommand ' + IntToHex(aR, 2) + ' ' + IntToHex(aCmd, 2) + ' ' + IntToHex(aVersion, 2) + ' ' + IntToHex(aSubCmd, 2), LOGCMD_ERR);
                              Result := False;
                            end;
                          end;
                      end else begin
                        add_log_line('Performance version differs ' + IntToHex(aR, 2) + ' ' + IntToHex(aCmd, 2) + ' ' + IntToHex(aVersion, 2), LOGCMD_ERR);
                        Result := False;
                      end;
                    end;
                  end;
                end;
          $00, $08 :
                begin
                  Result := GetSlot(0).ProcessResponseMessage( MemStream, aCmd);
                end;
          $01, $09 :
                begin
                  Result := GetSlot(1).ProcessResponseMessage( MemStream, aCmd);
                end;
          $02, $0a :
                begin
                  Result := GetSlot(2).ProcessResponseMessage( MemStream, aCmd);
                end;
          $03, $0b :
                begin
                  Result := GetSlot(3).ProcessResponseMessage( MemStream, aCmd);
                end;
          R_MIDI_CC :
                begin // Receive Midi info from G2, CC or Program change...
                  MemStream.Read( b, 1);
                  MemStream.Read( b, 1);
                  case b of
                  $80 : begin
                          // Midi CC
                          // 82 01 04 00 80 00 3f 30 40 00 00 00 00 00 00 00  = CC #63
                          MemStream.Read( b, 1);
                          MemStream.Read( aMidiCC, 1);
                          Result := True;

                          if assigned( FOnMidiCCReceive) then
                            FOnMidiCCReceive( self, ID, aMidiCC);
                        end;
                  $38 : begin
                          // Slot patch version changed. Reload slot
                          MemStream.Read( aSlot, 1);
                          Result := True;

                          if assigned( FOnAfterRetrievePatch) then
                            FOnAfterRetrievePatch( self, ID, aSlot, 0, 0);
                        end;
                  end;
                end;
            else begin
              add_log_line('Unknown command ' + IntToHex(aR, 2) + ' ' + IntToHex(aCmd, 2), LOGCMD_ERR);
              Result := False;
            end;
          end;


        {if aCmd in [$00, $01, $02, $03] then
          Result := False
        else
          Result := True;}

        end;
    else begin
      add_log_line('Unknown response byte ' + IntToHex(aR, 2), LOGCMD_ERR);
      Result := False;
    end;
  end;
end;

function TG2Mess.ProcessSendMessage( MemStream: TMemoryStream; SenderID: integer): boolean;
var Cmd, SubCmd, R, b, bh, bl, Stop, Slot, Bank, Patch : byte;
    Size : integer;
begin
  // Return True if processed
  Result := False;

  if (MemStream.Size - MemStream.Position) < 4 then begin
    MemStream.Position := MemStream.Size;
    exit;
  end;

  // Read size
  MemStream.Read( bh, 1);
  MemStream.Read( bl, 1);
  Size := bh * 256 + bl;

  MemStream.Read( R, 1);
  case R of
  $80 : begin
          MemStream.Position := MemStream.Size; // Todo
          Result := True;
        end;
  $01 : begin
          MemStream.Read( Cmd, 1);

          case (Cmd and $0f)  of
          CMD_SYS :
            begin
              MemStream.Read( b, 1); // Version
              MemStream.Read( SubCmd, 1);
              case SubCmd of
                S_SYNTH_SETTINGS :
                  begin
                    Result := True;
                    if assigned(FOnSynthSettingsUpdate) then
                      FOnSynthSettingsUpdate(self, SenderID);
                  end;
                S_SEL_SLOT,
                Q_PERF_SETTINGS,
                C_PERF_SETTINGS :
                  begin
                    MemStream.Position := MemStream.Position - 1;
                    Result := (Performance as TG2MessPerformance).ProcessSendMessage( MemStream, SenderID);
                  end;
                S_START_STOP_COM :
                  begin
                    MemStream.Read( Stop, 1);
                    Result := True;
                    if assigned(FOnStartStopCommunication) then
                      FOnStartStopCommunication( self, SenderID, b);
                  end;
                S_SET_PATCH :
                  begin
                    MemStream.Position := MemStream.Position - 1;
                    Result := (Performance as TG2MessPerformance).ProcessSendMessage( MemStream, SenderID);
                  end;
                S_LOAD :
                  begin
                    MemStream.Read( Slot, 1);
                    MemStream.Read( Bank, 1);
                    MemStream.Read( Patch, 1);
                    Result := True;

                    if assigned( FOnAfterRetrievePatch) then
                      FOnAfterRetrievePatch( self, SenderID, Slot, Bank, Patch);
                  end;
                Q_LIST_NAMES :
                  begin
                    MemStream.Read( b, 1);
                    Result := True;
                  end
                else
                  MemStream.Position := MemStream.Size; // Todo
              end;
            end;
          $08 : begin
                  MemStream.Position := 0;
                  Result := GetSlot(0).ProcessSendMessage( MemStream, SenderID);
                end;
          $09 : begin
                  MemStream.Position := 0;
                  Result := GetSlot(1).ProcessSendMessage( MemStream, SenderID);
                end;
          $0a : begin
                  MemStream.Position := 0;
                  Result := GetSlot(2).ProcessSendMessage( MemStream, SenderID);
                end;
          $0b : begin
                  MemStream.Position := 0;
                  Result := GetSlot(3).ProcessSendMessage( MemStream, SenderID);
                end;
          end;
        end;
  end;
end;

function TG2Mess.CreateInitMessage: TG2SendMessage;
begin
  add_log_line('Init', LOGCMD_HDR);

  Result := TG2SendMessage.Create;
  Result.WriteMessage( CMD_INIT);
end;

function TG2Mess.CreateStartStopCommunicationMessage( Stop : byte): TG2SendMessage;
begin
  if stop = 0 then
    add_log_line('Start communication', LOGCMD_HDR)
  else
    add_log_line('Stop communication', LOGCMD_HDR);

  // Start or stop the message stream comming from the G2

  Result := TG2SendMessage.Create;
  Result.WriteMessage( $01);
  Result.WriteMessage( CMD_REQ + CMD_SYS );
  Result.WriteMessage( $41);
  Result.WriteMessage( S_START_STOP_COM);
  Result.WriteMessage( Stop); //h00 = start, h01 = stop
end;

function TG2Mess.CreateGetPatchVersionMessage: TG2SendMessage;
begin
  add_log_line('Get patch version, slot $04', LOGCMD_HDR);

  Result := TG2SendMessage.Create;
  Result.WriteMessage( $01);
  Result.WriteMessage( CMD_REQ + CMD_SYS );
  Result.WriteMessage( $41);
  Result.WriteMessage( Q_VERSION_CNT);
  Result.WriteMessage( $04);
end;

function TG2Mess.CreateGetSynthSettingsMessage: TG2SendMessage;
begin
  add_log_line('Synth settings', LOGCMD_HDR);

  Result := TG2SendMessage.Create;
  Result.WriteMessage( $01);
  Result.WriteMessage( CMD_REQ + CMD_SYS );
  Result.WriteMessage( $41);
  Result.WriteMessage( Q_SYNTH_SETTINGS);
end;

procedure TG2Mess.AddSetSynthSettingsMessage( SendMessage : TG2Message);
var BitWriter : TBitWriter;
begin
  BitWriter := TBitWriter.Create;
  try
    GetPerformance.WriteClaviaString( SendMessage, FSynthName); // TODO
    SendMessage.WriteMessage( $80);
    SendMessage.WriteMessage( $00);
    SendMessage.WriteMessage( $00);
    SendMessage.WriteMessage( $00);
    BitWriter.WriteBits( SendMessage, FMemoryProtect, 1);
    BitWriter.WriteBits( SendMessage, $00,            7);
    BitWriter.WriteBits( SendMessage, FMidiChannelA, 8);
    BitWriter.WriteBits( SendMessage, FMidiChannelB, 8);
    BitWriter.WriteBits( SendMessage, FMidiChannelC, 8);
    BitWriter.WriteBits( SendMessage, FMidiChannelD, 8);
    BitWriter.WriteBits( SendMessage, FMidiGlobalChannel, 8);
    BitWriter.WriteBits( SendMessage, FSysExID, 8);
    BitWriter.WriteBits( SendMessage, FLocalOn, 1);
    BitWriter.WriteBits( SendMessage, $00,      7);
    BitWriter.WriteBits( SendMessage, $00,                   6);
    BitWriter.WriteBits( SendMessage, FProgramChangeReceive, 1);
    BitWriter.WriteBits( SendMessage, FProgramChangeSend,    1);
    BitWriter.WriteBits( SendMessage, $00,                 6);
    BitWriter.WriteBits( SendMessage, FControllersReceive, 1);
    BitWriter.WriteBits( SendMessage, FControllersSend,    1);
    BitWriter.WriteBits( SendMessage, $00,                  1);
    BitWriter.WriteBits( SendMessage, FSendClock,           1);
    BitWriter.WriteBits( SendMessage, FIgnoreExternalClock, 1);
    BitWriter.WriteBits( SendMessage, $00,                  5);
    BitWriter.WriteBits( SendMessage, FTuneCent, 8);
    BitWriter.WriteBits( SendMessage, FGlobalOctaveShiftActive, 1);
    BitWriter.WriteBits( SendMessage, $00,                      7);
    BitWriter.WriteBits( SendMessage, FGlobalOctaveShift, 8);
    BitWriter.WriteBits( SendMessage, FTuneSemi, 8);
    SendMessage.WriteMessage( $00);
    BitWriter.WriteBits( SendMessage, FPedalPolarity, 1);
    BitWriter.WriteBits( SendMessage, $40,            7);
    BitWriter.WriteBits( SendMessage, FControlPedalGain, 8);
    SendMessage.WriteMessage( $00);
    SendMessage.WriteMessage( $00);
    SendMessage.WriteMessage( $00);
    SendMessage.WriteMessage( $00);
    SendMessage.WriteMessage( $00);
    SendMessage.WriteMessage( $00);
    SendMessage.WriteMessage( $00);
    SendMessage.WriteMessage( $00);
    SendMessage.WriteMessage( $00);
    SendMessage.WriteMessage( $00);
    SendMessage.WriteMessage( $00);
    SendMessage.WriteMessage( $00);
    SendMessage.WriteMessage( $00);
    SendMessage.WriteMessage( $00);
    SendMessage.WriteMessage( $00);
    SendMessage.WriteMessage( $00);
  finally
    BitWriter.Free;
  end;
end;

function TG2Mess.CreateSetSynthSettingsMessage: TG2SendMessage;
begin
  add_log_line('Set synth settings', LOGCMD_HDR);

  Result := TG2SendMessage.Create;
  Result.WriteMessage( $01);
  Result.WriteMessage( CMD_REQ + CMD_SYS );
  Result.WriteMessage( $41);
  Result.WriteMessage( S_SYNTH_SETTINGS);

  AddSetSynthSettingsMessage( Result);
end;

function TG2Mess.CreateUnknown1Message: TG2SendMessage;
begin
  add_log_line('Unknown 1', LOGCMD_HDR);

  Result := TG2SendMessage.Create;
  Result.WriteMessage( $01);
  Result.WriteMessage( CMD_REQ + CMD_SYS );
  Result.WriteMessage( $41);
  Result.WriteMessage( M_UNKNOWN_1);
end;

function TG2Mess.CreateMidiDumpMessage: TG2SendMessage;
begin
  add_log_line('Midi dump', LOGCMD_HDR);

  Result := TG2SendMessage.Create;
  Result.WriteMessage( $01);
  Result.WriteMessage( CMD_REQ + CMD_SYS );
  Result.WriteMessage( $41);
  Result.WriteMessage( S_MIDI_DUMP);
end;


function TG2Mess.CreateListMessage( aMode, aBank, aPatch : byte; names : TStrings): TG2SendMessage;
begin
  add_log_line('List', LOGCMD_HDR);

  Result := TG2SendMessage.Create;
  Result.WriteMessage( $01);
  Result.WriteMessage( CMD_REQ + CMD_SYS );
  Result.WriteMessage( $41);
  Result.WriteMessage( Q_LIST_NAMES);
  Result.WriteMessage( aMode);
  Result.WriteMessage( aBank);
  Result.WriteMessage( aPatch);

{  while FBankListCmd.Mode <> 2 do begin

    Result := TG2SendMessage.Create;
    Result.WriteMessage( $01);
    Result.WriteMessage( CMD_REQ + CMD_SYS );
    Result.WriteMessage( $41);
    Result.WriteMessage( MESS_LIST_PATCHES);
    Result.WriteMessage( FBankListCmd.Mode);
    Result.WriteMessage( FBankListCmd.Bank);
    Result.WriteMessage( FBankListCmd.Patch);
  end;}
end;

function TG2Mess.CreateSetModeMessage( aMode : byte): TG2SendMessage;
begin
  add_log_line('Set mode', LOGCMD_HDR);

  Result := TG2SendMessage.Create;
  Result.WriteMessage( $01);
  Result.WriteMessage( CMD_REQ + CMD_SYS );
  Result.WriteMessage( $41);
  Result.WriteMessage( S_SET_PARAM_MODE);
  Result.WriteMessage( aMode); // $00 perf $01 patch
  Result.WriteMessage( $00);
end;

function TG2Mess.CreateNoteMessage( aNote : byte; aOnoff : byte): TG2SendMessage;
begin
  add_log_line('Play note', LOGCMD_HDR);

  Result := TG2SendMessage.Create;
  Result.WriteMessage( $01);
  Result.WriteMessage( CMD_REQ + CMD_SYS );
  Result.WriteMessage( $41);
  Result.WriteMessage( S_PLAY_NOTE);
  Result.WriteMessage( aOnOff); // $00 on $01 off
  Result.WriteMessage( aNote);
end;

////////////////////////////////////////////////////////////////////////////////
//  TG2MessPerformance
////////////////////////////////////////////////////////////////////////////////

constructor TG2MessPerformance.Create( AOwner: TComponent);
begin
  inherited;

  FPerfVersion := 0;
end;

destructor TG2MessPerformance.Destroy;
begin
  inherited;
end;

function TG2MessPerformance.GetSlot( aSlot : byte): TG2MessSlot;
begin
  Result := Slot[aSlot] as TG2MessSlot;
end;

procedure TG2MessPerformance.add_log_line(tekst : string; log_cmd : integer);
begin
  if assigned(G2) then
    (G2 as TG2Mess).add_log_line( tekst, log_cmd);
end;

function TG2MessPerformance.ProcessSendMessage( MemStream: TMemoryStream; SenderID: integer): boolean;
var SubCmd, aSlot, b : byte;
    i : integer;
    perf_name : AnsiString;
    Chunk : TPatchChunk;
    BitReader : TBitReader;
    Module : TG2FileModule;
    Param : TG2FileParameter;
    GlobalKnob : TGlobalKnob;
    aSlotIndex, aLocation, aModuleIndex, aParamIndex, aKnobIndex, aUnknown : byte;
begin
  // Return True if processed
  Result := False;
  MemStream.Read( SubCmd, 1);
  case SubCmd of
    Q_PERF_SETTINGS :
         begin
           MemStream.Position := MemStream.Size;
           Result := True;

           if assigned((G2 as TG2Mess).FOnPerfSettingsUpdate) then
             (G2 as TG2Mess).FOnPerfSettingsUpdate(self, SenderID, True);
         end;
    S_SEL_SLOT :
         begin
           MemStream.Read( aSlot, 1);
           SetSelectedSlotIndex( aSlot);
           Result := True;
         {  for i := 0 to 3 do
             (Slot[i].Patch as TG2MessPatch).Visible := (i = aSlot);

           if assigned((G2 as TG2Graph).ScrollboxVA)  then
             (G2 as TG2Graph).ScrollboxVA.Invalidate;

           if assigned((G2 as TG2Graph).ScrollboxFX) then
             (G2 as TG2Graph).ScrollboxFX.Invalidate;}

           {if assigned((G2 as TG2Mess).FOnSelectSlot) then
             (G2 as TG2Mess).FOnSelectSlot( self, G2.ID, aSlot);}
         end;
    C_PERF_SETTINGS :
         begin
            Result := True;
            if assigned((G2 as TG2Mess).FOnPerfSettingsUpdate) then
              (G2 as TG2Mess).FOnPerfSettingsUpdate(self, SenderID, True); // Todo : where to find performance mode?
         end;
    S_SET_PATCH :
          begin
            MemStream.Read(b, 1);
            MemStream.Read(b, 1);
            MemStream.Read(b, 1);
            perf_name := '';
            while (length(perf_name) < 16) and (MemStream.Read(b, 1) = 1) and (b <> 0) do begin
              perf_name := perf_name + AnsiChar(b);
            end;
            MemStream.Read(b, 1); // $1a
            MemStream.Read(b, 1); // $29

            if b <> C_PERF_NAME then
              raise Exception.Create('Performance name chunk expected.');

            perf_name := '';
            while (length(perf_name) < 16) and (MemStream.Read(b, 1) = 1) and (b <> 0) do begin
              perf_name := perf_name + AnsiChar(b);
            end;
            PerformanceName := perf_name;

{            for i := 0 to NSLOTS - 1 do
              (Slot[i].Patch as TG2MessPatch).Visible := False;}

            Chunk := TPatchChunk.Create(MemStream);
            try
              if assigned(G2) and (G2 as TG2Mess).FLogPatchFileChunks then
                Chunk.FLogLines := (G2 as TG2Mess).LogLines;

              Chunk.ReadChunk;
              if Chunk.FId = C_PERF_SETTINGS then begin

                Init;
                Read( Chunk); // Parse patch

                Result := True;
              end else
                raise Exception.Create('Performance chunk expected');

            finally
              Chunk.Free;

{              (Slot[ SelectedSlot].Patch as TG2Patch).Visible := True;}
            end;

            // Must be called AFTER moduleindex is known
            for i := 0 to NSLOTS - 1 do begin
              Slot[i].Patch.InitParameters;
              Slot[i].Patch.InitNames;
              (Slot[i].Patch as TG2MessPatch).SortLeds;

              if assigned(G2) then begin
                if assigned((G2 as TG2Mess).FOnPatchUpdate) then
                  (G2 as TG2Mess).FOnPatchUpdate( self, SenderID, i);
              end else
                if assigned((Slot[i] as TG2MessSlot).FOnPatchUpdate) then
                  (Slot[i] as TG2MessSlot).FOnPatchUpdate( self, SenderID, i);
            end;
          end;
    {S_ASS_GLOBAL_KNOB : // Assign knob in global page
            begin
              BitReader := TBitReader.Create;
              try
                aSlotIndex     := BitReader.ReadBits( MemStream, 4);
                aLocation      := BitReader.ReadBits( MemStream, 2);
                aUnknown       := BitReader.ReadBits( MemStream, 2);
                aModuleIndex   := BitReader.ReadBits( MemStream, 8);
                aParamIndex    := BitReader.ReadBits( MemStream, 8);
                aUnknown       := BitReader.ReadBits( MemStream, 8);
                aKnobIndex     := BitReader.ReadBits( MemStream, 8);

                if TLocationType(aLocation) = ltPatch then begin
                  Slot[aSlotIndex].Patch.Parameter[ aModuleIndex, aParamIndex].AssignGlobalKnob( self, aSlotIndex, aKnobIndex);
                end else begin
                  Module := Slot[aSlotIndex].Patch.GetModule( aLocation, aModuleIndex);
                  if assigned( Module) then begin
                    Module.Parameter[ aParamIndex].AssignGlobalKnob( self, aSlotIndex, aKnobIndex);
                  end else
                    add_log_line( 'ModuleIndex ' + IntToStr( aModuleIndex) + ' not found.', LOGCMD_ERR);
                end;

                if assigned(G2.OnAssignGlobalKnob) then
                  G2.OnAssignGlobalKnob(self, G2.ID, aKnobIndex);
              finally
                BitReader.Free;
              end;
            end;
    S_DEASS_GLOB_KNOB : // Deassign global knob
            begin
              Memstream.Read( aUnknown, 1);
              Memstream.Read( aKnobIndex, 1);

              GlobalKnob := GlobalKnobList.Items[ aKnobIndex];

              if GlobalKnob.IsAssigned = 1 then begin
                if GlobalKnob.Location = TBits2(ltPatch) then begin
                  Param := Slot[aSlotIndex].Patch.Parameter[ GlobalKnob.ModuleIndex, GlobalKnob.ParamIndex];
                  if assigned(Param) then
                    Param.DeassignGlobalKnob( self, aKnobIndex);
                end else begin
                  Module := Slot[aSlotIndex].Patch.GetModule( GlobalKnob.Location, GlobalKnob.ModuleIndex);
                  if assigned( Module) then begin
                    Module.Parameter[ GlobalKnob.ParamIndex].DeassignGlobalKnob( self, aKnobIndex);
                  end;
                end;
              end;

              if assigned(G2.OnDeassignGlobalKnob) then
                G2.OnDeassignGlobalKnob(self, G2.ID, aKnobIndex);
            end;
    S_SEL_GLOBAL_PAGE : // Select global parameter page
            begin
              // TODO
            end;}
  end;
end;

function TG2MessPerformance.CreateGetPerfSettingsMessage: TG2SendMessage;
begin
  add_log_line('Performance, settings patch_version ' + IntToStr( FPerfVersion), LOGCMD_HDR);

  Result := TG2SendMessage.Create;
  Result.WriteMessage( $01);
  Result.WriteMessage( CMD_REQ + CMD_SYS );
  Result.WriteMessage( FPerfVersion);
  Result.WriteMessage( Q_PERF_SETTINGS);
end;

function TG2MessPerformance.CreateUnknown2Message: TG2SendMessage;
begin
  add_log_line('Unknown 2, patch_version ' + IntToStr( FPerfVersion), LOGCMD_HDR);

  Result := TG2SendMessage.Create;
  Result.WriteMessage( $01);
  Result.WriteMessage( CMD_REQ + CMD_SYS );
  Result.WriteMessage( FPerfVersion);
  Result.WriteMessage( M_UNKNOWN_2);
end;

function TG2MessPerformance.CreateSelectSlotMessage( aSlot: byte): TG2SendMessage;
begin
  add_log_line('Select slot patch_version ' + IntToStr( FPerfVersion) + ', Slot ' + IntToStr(aSlot), LOGCMD_HDR);

  Result := TG2SendMessage.Create;
  Result.WriteMessage( $01);
  Result.WriteMessage( CMD_REQ + CMD_SYS );
  Result.WriteMessage( FPerfVersion);
  Result.WriteMessage( S_SEL_SLOT);
  Result.WriteMessage( aSlot);
end;

function TG2MessPerformance.CreateRetrieveMessage( aSlot, aBank, aPatch : byte): TG2SendMessage;
begin
  if aSlot = 4 then
    add_log_line('Retrieve performance, slot ' + IntToStr(aSlot) + ', bank ' + IntToStr(aBank) + ', patch ' + IntToStr(aPatch), LOGCMD_HDR)
  else
    add_log_line('Retreve patch, slot ' + IntToStr(aSlot) + ', bank ' + IntToStr(aBank) + ', patch ' + IntToStr(aPatch), LOGCMD_HDR);

  Result := TG2SendMessage.Create;
  Result.WriteMessage( $01);
  Result.WriteMessage( CMD_REQ + CMD_SYS );
  Result.WriteMessage( $41);
  Result.WriteMessage( S_LOAD);
  Result.WriteMessage( aSlot);
  Result.WriteMessage( aBank);
  Result.WriteMessage( aPatch);
end;

function TG2MessPerformance.CreateStoreMessage( aSlot, aBank, aPatch : byte): TG2SendMessage;
begin
  if aSlot = 4 then
    add_log_line('Save performance, slot ' + IntToStr(aSlot) + ', bank ' + IntToStr(aBank) + ', patch ' + IntToStr(aPatch), LOGCMD_HDR)
  else
    add_log_line('Save patch, slot ' + IntToStr(aSlot) + ', bank ' + IntToStr(aBank) + ', patch ' + IntToStr(aPatch), LOGCMD_HDR);

  Result := TG2SendMessage.Create;
  Result.WriteMessage( $01);
  Result.WriteMessage( CMD_REQ + CMD_SYS );
  Result.WriteMessage( $41);
  Result.WriteMessage( S_SAVE);
  Result.WriteMessage( aSlot);
  Result.WriteMessage( aBank);
  Result.WriteMessage( aPatch);
end;

function TG2MessPerformance.CreateSetPerformanceMessage( aPerfName : AnsiString; aPerf : TG2FilePerformance): TG2SendMessage;
var Chunk : TPatchChunk;
    i : integer;
begin
  add_log_line('Upload performance', LOGCMD_HDR);

  Result := TG2SendMessage.Create;
  Result.WriteMessage( $01);
  Result.WriteMessage( CMD_REQ + CMD_SYS);
  Result.WriteMessage( $42); //?
  Result.WriteMessage( S_SET_PATCH);
  Result.WriteMessage( $00);
  Result.WriteMessage( $00);
  Result.WriteMessage( $00);


  Chunk := TPatchChunk.Create(Result);
  try
    if (G2 as TG2Mess).FLogPatchFileChunks then
      Chunk.FLogLines := (G2 as TG2Mess).LogLines;

    Chunk.WriteName( aPerfName);
    Chunk.Flush;

    aPerf.PerformanceName := aPerfName;

    Chunk.WriteBits( $1a,         8);
    Chunk.WriteBits( C_PERF_NAME, 8);
    Chunk.WriteName( aPerfName);
    Chunk.Flush;

    aPerf.WriteSettings( Chunk);

    for i := 0 to NSLOTS - 1 do begin
      while aPerf.Slot[ i].GetPatch.PatchSettings.VariationCount < 10 do
        aPerf.Slot[ i].GetPatch.AddVariation;
      aPerf.Slot[ i].GetPatch.Write( Chunk, 10);
      Chunk.Flush;
    end;

    aPerf.GlobalKnobList.Write( Chunk);
    Chunk.WriteChunk( C_KNOBS_GLOBAL);

  finally
    Chunk.Free;
  end;
end;

function TG2MessPerformance.CreateSetPerfSettingsMessage: TG2SendMessage;
var Chunk : TPatchChunk;
begin
  add_log_line('Upload performance settings', LOGCMD_HDR);

  Result := TG2SendMessage.Create;
  Result.WriteMessage( $01);
  Result.WriteMessage( CMD_REQ + CMD_SYS);
  Result.WriteMessage( FPerfVersion);

  Chunk := TPatchChunk.Create(Result);
  try
    if (G2 as TG2Mess).FLogPatchFileChunks then
      Chunk.FLogLines := (G2 as TG2Mess).LogLines;

    WriteSettings( Chunk);
  finally
    Chunk.Free;
  end;
end;

function TG2MessPerformance.CreateSetPerfNameMessage( aPerfName : AnsiString): TG2SendMessage;
begin
  add_log_line('Performance name', LOGCMD_HDR);

  Result := TG2SendMessage.Create;
  Result.WriteMessage( $01);
  Result.WriteMessage( CMD_REQ + CMD_SYS);
  Result.WriteMessage( FPerfVersion);
  Result.WriteMessage( C_PERF_NAME);
  Result.WriteClaviaString( aPerfName);
end;

function TG2MessPerformance.CreateGetGlobalKnobsMessage: TG2SendMessage;
begin
  add_log_line('Global knobs', LOGCMD_HDR);

  Result := TG2SendMessage.Create;
  Result.WriteMessage( $01);
  Result.WriteMessage( CMD_REQ + CMD_SYS);
  Result.WriteMessage( FPerfVersion);
  Result.WriteMessage( Q_GLOBAL_KNOBS);
end;

////////////////////////////////////////////////////////////////////////////////
//  TG2MessSlot
////////////////////////////////////////////////////////////////////////////////

constructor TG2MessSlot.Create( AOwner: TComponent);
begin
  inherited;
  FPatchVersion := 0;

  Init;
end;

destructor TG2MessSlot.Destroy;
begin
  inherited;
end;

function TG2MessSlot.GetPatch : TG2MessPatch;
begin
  if not assigned(Patch) then
    raise Exception.Create('Patch in slot unassigned!');

  Result := Patch as TG2MessPatch;
end;

function TG2MessSlot.GetPerformance : TG2MessPerformance;
begin
  if not assigned(Performance) then
    raise Exception.Create('Performance not assigned to slot.');

  Result := Performance as TG2MessPerformance;
end;


procedure TG2MessSlot.add_log_line(tekst : string; log_cmd : integer);
begin
  if assigned(G2) then
    (G2 as TG2Mess).add_log_line( tekst, log_cmd);
end;

function TG2MessSlot.ProcessResponseMessage( MemStream: TMemoryStream; Param : byte): boolean;
var aVersion, aSubCmd, aLocation, aModuleIndex, aParamIndex, b, mask,
    aValue, aVariation : byte;
    Chunk : TPatchChunk;
    FPatch : TG2MessPatch;
    i, j : integer;
begin
  Result := False; // Return true if message was processed

  FPatch := GetPatch;

  MemStream.Read(aVersion, 1);

  if aVersion = $40 then begin

    MemStream.Read( aSubCmd, 1);
    case aSubCmd of
    $36 : begin // patch version
            MemStream.Read(b, 1); // Slot?
            Result := MemStream.Read(FPatchVersion, 1) = 1;
          end;
      else begin
         add_log_line('Unknown subcommand ' + IntToHex(aVersion, 2) + ' ' + IntToHex(aSubCmd, 2), LOGCMD_ERR);
         Result := False;
      end;
    end;

  end else
    if aVersion = FPatchVersion then begin

      MemStream.Read( aSubCmd, 1);
      case aSubCmd of
      //h09 : begin // select slot
      //        Result := MemStream.Read(FSelectedSlot, 1) = 1;
      //      end;
      C_PATCH_DESCR :
            begin // patch
              MemStream.Position := MemStream.Position - 1;

              Chunk := TPatchChunk.Create(MemStream);
              try
                if assigned((G2 as TG2Mess).LogLines) then
                  Chunk.FLogLines := (G2 as TG2Mess).LogLines;

                Chunk.ReadChunk;
                if Chunk.FId = $21 then begin
                  MemStream.Read(b, 1);
                  if b = $2d then
                    // Read the extra 2 bytes $2d $00 that comes with patches downloaded with usb (TODO)
                     MemStream.Read(b, 1)
                  else
                    MemStream.Position := MemStream.Position - 1;

                  FPatch.Init;
                  FPatch.Read( Chunk); // Parse patch

                  Result := True;
                end;
                {FPatch.InitParameters;
                FPatch.InitNames;
                FPatch.SortLeds;}

              finally
                Chunk.Free;
              end;
            end;
      R_PATCH_NAME :
            begin // patch name
              PatchName := '';
              while (MemStream.Read(b, 1) = 1) and (length(PatchName) < 16) and (b <> 0) do begin
                PatchName := PatchName + AnsiChar(b);
              end;
              Result := True;
            end;
      S_SEL_PARAM :
            begin // selected parameter
              Result := True;
            end;
      R_VOLUME_DATA :
            begin // Volume indicator data
              i := 0;

              while (i < FPatch.GetMiniVUListCount) do begin
                MemStream.Read(b, 1);
                MemStream.Read(b, 1);
                FPatch.SetMiniVULevel( i, b);
                //log_line(log_lines, 'Update led. ModuleIndex : ' + IntToStr(FG2GraphPatch.GetMiniVU(i).Module.ModuleIndex)
                //                         + ' GroupID : ' + IntToStr(FG2GraphPatch.GetMiniVU(i).FGroupID), LOGCMD_NUL);
                inc(i);
              end;
              Result := True;
            end;
      R_LED_DATA : begin // Led data
              MemStream.Read(aLocation, 1);
              i := 0;
              j := 3;
              mask := 0;
              while (i < FPatch.GetLedListCount) do begin
                if j = 3 then begin
                  MemStream.Read(b, 1);
                  mask := $03;
                  j := 0;
                end else begin
                  mask := mask shl 2;
                  inc(j);
                end;

                FPatch.SetLedLevel( i, (b and mask) shr j);

                inc(i);
              end;
              Result := True;
            end;
      S_SET_PARAM :
            begin // ParamChange
              MemStream.Read( aLocation, 1);
              MemStream.Read( aModuleIndex, 1);
              MemStream.Read( aParamIndex, 1);
              MemStream.Read( aValue, 1);
              MemStream.Read( aVariation, 1);
              Result := True;

              if aModuleIndex <> 0 then
                //FPatch.InitParameterValue(TLocationType(aLocation), aModuleIndex, aParamIndex, aVariation, aValue);
                FPatch.SetParamInPatch(TLocationType(aLocation), aModuleIndex, aParamIndex, aVariation, aValue);
            end;
      C_CURRENT_NOTE_2 :
            begin // Current note
              Result := True;
            end;
      C_PATCH_NOTES :
            begin // Text pad
              Result := True;
            end;

      R_RESOURCES_USED :
            begin // Resources in use
              MemStream.Read(aLocation, 1);
              Result := True;
              {case aLocation of
              $00 : Result := True;
              $01 : Result := True;
              end;
              if Param in [$00, $01, $02, $03] then
                Result := False
              else
                Result := True;}
            end;
      R_OK :
            begin // ok
              Result := True;
            end;
      R_ERROR :
            begin // NOT ok
              MemStream.Read(b, 1); // error no?
              if assigned(G2) then begin
                (G2 as TG2Mess).FErrorMessage := True;
                (G2 as TG2Mess).FErrorMessageNo := b;

                add_log_line('G2 returns error ' + IntToHex(b, 2) + '!', LOGCMD_ERR);
              end;
              Result := True;
            end;
        else begin
          add_log_line('Unknown subcommand ' + IntToHex(aVersion, 2) + ' ' + IntToHex(aSubCmd, 2), LOGCMD_ERR);
          Result := False;
        end;
      end;

    end else begin
      add_log_line('Patch version differs, received version ' + IntToHex(aVersion, 2) + ', current patch version is ' +  IntToHex(FPatchVersion, 2), LOGCMD_ERR);
      Result := False;
    end;

{  if aCmd in [$00, $01, $02, $03] then
    Result := False
  else
    Result := True;}
end;

function TG2MessSlot.ProcessSendMessage( MemStream: TMemoryStream; SenderID : integer): boolean;
var Cmd, SubCmd, aVariation, aLocation, aModuleIndex, aParameterIndex,
    aFromVariation, aToVariation, aValue, b, bh, bl : byte;
    Chunk : TPatchChunk;
    Size : integer;
    Patch : TG2MessPatch;
    Performance : TG2MessPerformance;
begin
  // Return True if proceed
  Result := False;
  Patch := GetPatch;
  Performance := GetPerformance;

  if (MemStream.Size - MemStream.Position) < 6 then begin
    MemStream.Position := MemStream.Size;
    exit;
  end;

  // Read size
  MemStream.Read( bh, 1);
  MemStream.Read( bl, 1);
  Size := bh * 256 + bl;

  MemStream.Read( b, 1); // $01
  MemStream.Read( Cmd, 1);

  case (Cmd and $0f)  of
  CMD_SYS : begin
              MemStream.Position := MemStream.Size; // Todo
              Result := True;
            end;
         else
            begin
              MemStream.Read( b, 1); // Version

              Result := Patch.ProcessMessage( MemStream);

              if not Result then begin
                MemStream.Position := MemStream.Position - 1;

                MemStream.Read( SubCmd, 1);

                case SubCmd of
                S_SET_PATCH :
                      begin
                        MemStream.Read(b, 1);
                        MemStream.Read(b, 1);
                        MemStream.Read(b, 1);
                        PatchName := '';
                        while (length(PatchName) < 16) and (MemStream.Read(b, 1) = 1) and (b <> 0) do begin
                          PatchName := PatchName + AnsiChar(b);
                        end;
                        Patch.PatchName := PatchName;

                        //Patch.Visible := False;
                        Chunk := TPatchChunk.Create(MemStream);
                        try
                          if assigned(G2) and (G2 as TG2Mess).FLogPatchFileChunks then
                            Chunk.FLogLines := (G2 as TG2Mess).LogLines;

                          Chunk.ReadChunk;
                          if Chunk.FId = $21 then begin

                            Patch.Init;
                            Patch.Read( Chunk); // Parse patch

                            Result := True;
                          end;

                        finally
                          Chunk.Free;
                          {if SlotIndex = Performance.Selectedslot then
                            Patch.Visible := True;}
                        end;

                        if assigned(G2) then begin
                          if assigned((G2 as TG2Mess).FOnPatchUpdate) then
                            (G2 as TG2Mess).FOnPatchUpdate( self, SenderID, SlotIndex);
                        end else
                          if assigned( FOnPatchUpdate) then
                            FOnPatchUpdate( self, SenderID, SlotIndex);
                      end;

                Q_PATCH :
                      begin
                        MemStream.Position := MemStream.Size;
                        Result := True;

                        // Must be called AFTER moduleindex is known
                        Patch.InitParameters;
                        Patch.InitNames;
                        Patch.SortLeds;

                        if assigned(G2) then begin
                          if assigned((G2 as TG2Mess).FOnPatchUpdate) then
                            (G2 as TG2Mess).FOnPatchUpdate(self, SenderID, SlotIndex);
                        end else
                          if assigned( FOnPatchUpdate) then
                            FOnPatchUpdate( self, SenderID, SlotIndex);
                      end;
                S_SEL_VARIATION :
                      begin
                        MemStream.Read( aVariation, 1);
                        MemStream.Position := MemStream.Size;
                        Result := True;
                        Patch.Variation := aVariation;

                        if assigned(G2) then begin
                          if assigned((G2 as TG2Mess).FOnvariationChange) then
                            (G2 as TG2Mess).FOnVariationChange(self, SenderID, SlotIndex ,aVariation);
                        end else
                          if assigned( FOnvariationChange) then
                            FOnvariationChange( self, SenderID, SlotIndex ,aVariation);
                      end;
                Q_SELECTED_PARAM :
                      begin
                        MemStream.Position := MemStream.Size; // Todo
                        Result := True;
                      end;
                M_UNKNOWN_6 :
                      begin
                        MemStream.Position := MemStream.Size; // Todo
                        Result := True;
                      end;
                Q_CURRENT_NOTE :
                      begin
                        MemStream.Position := MemStream.Size; // Todo
                        Result := True;
                      end;
                Q_PATCH_NAME :
                      begin
                        MemStream.Position := MemStream.Size; // Todo
                        Result := True;
                      end;
                Q_RESOURCES_USED :
                      begin
                        MemStream.Position := MemStream.Size; // Todo
                        Result := True;
                      end;
                Q_PATCH_TEXT :
                      begin
                        MemStream.Position := MemStream.Size; // Todo
                        Result := True;
                      end;
                S_SET_PARAM :
                      begin
                        MemStream.Read( aLocation, 1);
                        MemStream.Read( aModuleIndex, 1);
                        MemStream.Read( aParameterIndex, 1);
                        MemStream.Read( aValue, 1);
                        MemStream.Read( aVariation, 1);
                        MemStream.Position := MemStream.Size;
                        Result := True;

                        Patch.SetParamInPatch( TLocationType(aLocation), aModuleIndex, aParameterIndex, aVariation, aValue);

                        if assigned(G2) and assigned((G2 as TG2Mess).FOnParamChangeMessage) then
                          (G2 as TG2Mess).FOnParamChangeMessage( self, SenderID, SlotIndex, aVariation, TLocationType(aLocation), aModuleIndex, aParameterIndex, aValue);
                      end;
                S_SET_MODE :
                      begin
                        MemStream.Read( aLocation, 1);
                        MemStream.Read( aModuleIndex, 1);
                        MemStream.Read( aParameterIndex, 1);
                        MemStream.Read( aValue, 1);
                        MemStream.Position := MemStream.Size;
                        Result := True;

                        Patch.InitModeValue( TLocationType(aLocation), aModuleIndex, aParameterIndex, aValue);
                      end;
                S_COPY_VARIATION :
                      begin
                        MemStream.Read( aFromVariation, 1);
                        MemStream.Read( aToVariation, 1);
                        Result := True;
                        Patch.CopyVariation( aFromVariation, aToVariation);
                      end;
                end;
              end;
            end;
  end;
end;


function TG2MessSlot.CreateGetPatchVersionMessage: TG2SendMessage;
begin
  add_log_line('Get patch version, slot ' + IntToStr(SlotIndex), LOGCMD_HDR);

  Result := TG2SendMessage.Create;
  Result.WriteMessage( $01);
  Result.WriteMessage( CMD_REQ + CMD_SYS );
  Result.WriteMessage( $41);
  Result.WriteMessage( Q_VERSION_CNT);
  Result.WriteMessage( SlotIndex);
end;

function TG2MessSlot.CreatePatchNotesMessage: TG2SendMessage;
begin
  add_log_line('Patch notes, slot ' + IntToStr(SlotIndex) + ', patch_version ' + IntToStr(FPatchVersion), LOGCMD_HDR);

  Result := TG2SendMessage.Create;
  Result.WriteMessage( $01);
  Result.WriteMessage( CMD_REQ + CMD_SLOT + SlotIndex );
  Result.WriteMessage( FPatchVersion);
  Result.WriteMessage( Q_PATCH_TEXT);
end;

function TG2MessSlot.CreateSendControllerSnapshotMessage: TG2SendMessage;
begin
  add_log_line('Send controller snapshot', LOGCMD_HDR);

  Result := TG2SendMessage.Create;
  Result.WriteMessage( $01);
  Result.WriteMessage( CMD_REQ + CMD_SLOT + SlotIndex );
  Result.WriteMessage( FPatchVersion);
  Result.WriteMessage( S_CTRL_SNAPSHOT);
end;

function TG2MessSlot.CreateResourceTableMessage( aLocation : Byte): TG2SendMessage;
begin
  add_log_line('Resources used ' + IntToStr(aLocation) + ', slot ' + IntToStr(SlotIndex) + ', patch_version ' + IntToStr(FPatchVersion), LOGCMD_HDR);

  Result := TG2SendMessage.Create;
  Result.WriteMessage( $01);
  Result.WriteMessage( CMD_REQ + CMD_SLOT + SlotIndex );
  Result.WriteMessage( FPatchVersion);
  Result.WriteMessage( Q_RESOURCES_USED);
  Result.WriteMessage( aLocation);
end;

function TG2MessSlot.CreateGetPatchNameMessage: TG2SendMessage;
begin
  add_log_line('Patch name, slot ' + IntToStr(SlotIndex) + ', patch_version ' + IntToStr(FPatchVersion), LOGCMD_HDR);

  Result := TG2SendMessage.Create;
  Result.WriteMessage( $01);
  Result.WriteMessage( CMD_REQ + CMD_SLOT + SlotIndex );
  Result.WriteMessage( FPatchVersion);
  Result.WriteMessage( Q_PATCH_NAME);
end;

function TG2MessSlot.CreateCurrentNoteMessage: TG2SendMessage;
begin
  add_log_line('Current note, slot ' + IntToStr(SlotIndex) + ', patch_version ' + IntToStr(FPatchVersion), LOGCMD_HDR);

  Result := TG2SendMessage.Create;
  Result.WriteMessage( $01);
  Result.WriteMessage( CMD_REQ + CMD_SLOT + SlotIndex );
  Result.WriteMessage( FPatchVersion);
  Result.WriteMessage( Q_CURRENT_NOTE);
end;

function TG2MessSlot.CreateUnknown6Message: TG2SendMessage;
begin
  add_log_line('Unknown 6, slot ' + IntToStr(SlotIndex) + ', patch_version ' + IntToStr(FPatchVersion), LOGCMD_HDR);

  Result := TG2SendMessage.Create;
  Result.WriteMessage( $01);
  Result.WriteMessage( CMD_REQ + CMD_SLOT + SlotIndex );
  Result.WriteMessage( FPatchVersion);
  Result.WriteMessage( M_UNKNOWN_6);
end;

function TG2MessSlot.CreateGetSelectedParameterMessage: TG2SendMessage;
begin
  add_log_line('Get selected parameter, slot ' + IntToStr(SlotIndex) + ', patch_version ' + IntToStr(FPatchVersion), LOGCMD_HDR);

  Result := TG2SendMessage.Create;
  Result.WriteMessage( $01);
  Result.WriteMessage( CMD_REQ + CMD_SLOT + SlotIndex );
  Result.WriteMessage( FPatchVersion);
  Result.WriteMessage( Q_SELECTED_PARAM);
end;

function TG2MessSlot.CreateSetPatchMessage( aPatchName : AnsiString; aPatch : TG2FilePatch): TG2SendMessage;
var Chunk : TPatchChunk;
begin
  add_log_line('Upload, slot ' + IntToStr( SlotIndex), LOGCMD_HDR);

  Result := TG2SendMessage.Create;

  Result.WriteMessage( $01);
  Result.WriteMessage( CMD_REQ + CMD_SLOT + SlotIndex );
  Result.WriteMessage( $53); // ?
  Result.WriteMessage( S_SET_PATCH);
  Result.WriteMessage( $00);
  Result.WriteMessage( $00);
  Result.WriteMessage( $00);

  Chunk := TPatchChunk.Create(Result);
  try
    if (G2 as TG2Mess).FLogPatchFileChunks then
      Chunk.FLogLines := (G2 as TG2Mess).LogLines;

    Chunk.WriteName( aPatchName);
    Chunk.Flush;

    while aPatch.PatchSettings.VariationCount < 10 do
      aPatch.AddVariation;

    aPatch.Write(Chunk, 10);
  finally
    Chunk.Free;
  end;
end;

function TG2MessSlot.CreateGetPatchMessage: TG2SendMessage;
begin
  add_log_line('Download, slot ' + IntToStr(SlotIndex) + ', patch_version ' + IntToStr(FPatchVersion), LOGCMD_HDR);

  Result := TG2SendMessage.Create;
  Result.WriteMessage( $01);
  Result.WriteMessage( CMD_REQ + CMD_SLOT + SlotIndex );
  Result.WriteMessage( FPatchVersion);
  Result.WriteMessage( Q_PATCH);
end;

function TG2MessSlot.CreateSelectVariationMessage( aVariationIndex: byte): TG2SendMessage;
begin
  add_log_line('Select variation ' + IntToStr(aVariationIndex) + ', slot ' + IntToStr(SlotIndex) + ', patch_version ' + IntToStr(FPatchVersion), LOGCMD_HDR);

  Result := TG2SendMessage.Create;
  Result.WriteMessage( $01);
  Result.WriteMessage( CMD_REQ + CMD_SLOT + SlotIndex );
  Result.WriteMessage( FPatchVersion);
  Result.WriteMessage( S_SEL_VARIATION);
  Result.WriteMessage( aVariationIndex);
end;

function TG2MessSlot.CreateSetParamMessage( aLocation, aModule, aParam, aValue, aVariation: byte): TG2SendMessage;
begin
  Result := TG2SendMessage.Create;
  Result.WriteMessage( $01);
  Result.WriteMessage( CMD_NO_RESP + CMD_SLOT + SlotIndex );
  Result.WriteMessage( FPatchVersion);
  Result.WriteMessage( S_SET_PARAM);
  Result.WriteMessage( aLocation);
  Result.WriteMessage( aModule);
  Result.WriteMessage( aParam);
  Result.WriteMessage( aValue);
  Result.WriteMessage( aVariation);
end;

function TG2MessSlot.CreateSelParamMessage( aLocation, aModule, aParam: integer): TG2SendMessage;
begin
  Result := TG2SendMessage.Create;
  Result.WriteMessage( $01);
  Result.WriteMessage( CMD_NO_RESP + CMD_SLOT + SlotIndex );
  Result.WriteMessage( FPatchVersion);
  Result.WriteMessage( S_SEL_PARAM);
  Result.WriteMessage( $00); // Unknown
  Result.WriteMessage( aLocation);
  Result.WriteMessage( aModule);
  Result.WriteMessage( aParam);
end;

function TG2MessSlot.CreateSetMorphMessage( aLocation, aModule, aParam, aMorph, aValue, aNegative, aVariation: byte): TG2SendMessage;
begin
  Result := TG2SendMessage.Create;
  Result.WriteMessage( $01);
  Result.WriteMessage( CMD_NO_RESP + CMD_SLOT + SlotIndex );
  Result.WriteMessage( FPatchVersion);
  Result.WriteMessage( S_SET_MORPH_RANGE);
  Result.WriteMessage( aLocation);
  Result.WriteMessage( aModule);
  Result.WriteMessage( aParam);
  Result.WriteMessage( aMorph);
  Result.WriteMessage( aValue);
  Result.WriteMessage( aNegative);
  Result.WriteMessage( aVariation);
end;

function TG2MessSlot.CreateSetModeMessage( aLocation, aModule, aParam, aValue: integer): TG2SendMessage;
begin
  add_log_line('Set mode, location ' + IntToStr(aLocation) + ', module ' + IntToStr(aModule) + ', param ' + IntToStr(aParam) + ', value ' + IntToStr(aValue)  + ', slot ' + IntToStr(SlotIndex) + ', patch_version ' + IntToStr(FPatchVersion), LOGCMD_HDR);

  Result := TG2SendMessage.Create;
  Result.WriteMessage( $01);
  Result.WriteMessage( CMD_REQ + CMD_SLOT + SlotIndex );
  Result.WriteMessage( FPatchVersion);
  Result.WriteMessage( S_SET_MODE);
  Result.WriteMessage( aLocation);
  Result.WriteMessage( aModule);
  Result.WriteMessage( aParam);
  Result.WriteMessage( aValue);
end;

function TG2MessSlot.CreateCopyVariationMessage( aFromVariation, aToVariation : byte): TG2SendMessage;
begin
  add_log_line('Copy variation, from variation ' + IntToStr(aFromVariation) + ', to variation ' + IntToStr(aToVariation), LOGCMD_HDR);

  Result := TG2SendMessage.Create;
  Result.WriteMessage( $01);
  Result.WriteMessage( CMD_REQ + CMD_SLOT + SlotIndex );
  Result.WriteMessage( FPatchVersion);
  Result.WriteMessage( S_COPY_VARIATION);
  Result.WriteMessage( aFromVariation);
  Result.WriteMessage( aToVariation);
end;

////////////////////////////////////////////////////////////////////////////////
//  TG2MessPatch
////////////////////////////////////////////////////////////////////////////////

constructor TG2MessPatch.Create( AOwner: TComponent);
begin
  inherited;

  FUndoStack := TList.Create;
end;

destructor TG2MessPatch.Destroy;
var i : integer;
begin
  for i := 0 to FUndoStack.Count - 1 do
    TG2SendMessage(FUndoStack[i]).Free;
  FUndoStack.Free;

  inherited;
end;

procedure TG2MessPatch.add_log_line(tekst: string; log_cmd: integer);
begin
  if assigned(G2) then
    (G2 as TG2Mess).add_log_line( tekst, log_cmd);
end;

function TG2MessPatch.GetSlot : TG2MessSlot;
begin
  if not assigned(Slot) then
    raise Exception.Create('Slot not assigned to patch.');

  Result := Slot as TG2MessSlot;
end;

function TG2MessPatch.GetPerformance : TG2MessPerformance;
begin
  if not assigned(Slot) then
    raise Exception.Create('Slot not assigned to patch.');

  Result := (Slot as TG2MessSlot).GetPerformance;
end;

//function TG2MessPatch.GetG2 : TG2Mess;
//begin
//  Result := G2 as TG2Mess;
//end;

procedure TG2MessPatch.PushUndoStack( MemStream : TG2SendMessage);
begin
  if FUndoStack.Count > 50 then begin
    TG2SendMessage(FUndoStack[0]).Free;
    FUndoStack.Delete(0);
  end;
  FUndoStack.Add( MemStream);
end;

function TG2MessPatch.PopUndoStack : TG2SendMessage;
begin
  Result := TG2SendMessage(FUndoStack[FUndoStack.Count - 1]);
  FUndoStack.Delete(FUndoStack.Count - 1);
end;

procedure TG2MessPatch.AddSetUprateMessage( SendMessage : TG2SendMessage; aModule : TG2FileModule; aUprateValue : byte);
var MemStream : TG2Message;
begin
  MemStream := TG2Message.Create;
  try
    MemStream.WriteMessage( S_SET_UPRATE);
    MemStream.WriteMessage( ord(aModule.Location));
    MemStream.WriteMessage( aModule.ModuleIndex);
    MemStream.WriteMessage( aUprateValue);

    SendMessage.Add( MemStream);
  finally
    MemStream.Free;
  end;
end;

procedure TG2MessPatch.AddSetCableColorMessage( SendMessage : TG2SendMessage; aCable : TG2FileCable; aColor : byte);
var MemStream : TG2Message;
begin
  MemStream := TG2Message.Create;
  try
    MemStream.WriteMessage( S_CABLE_COLOR);
    MemStream.WriteMessage( ord(aCable.FromConnector.Module.Location) shl 3 + aColor);
    MemStream.WriteMessage( aCable.FromConnector.Module.ModuleIndex);
    MemStream.WriteMessage( Ord(aCable.FromConnector.ConnectorKind) shl 6 + aCable.FromConnector.ConnectorIndex);
    MemStream.WriteMessage( aCable.ToConnector.Module.ModuleIndex);
    MemStream.WriteMessage( Ord(aCable.ToConnector.ConnectorKind) shl 6 +  aCable.ToConnector.ConnectorIndex);
    SendMessage.Add( MemStream);
  finally
    MemStream.Free;
  end;
end;

procedure TG2MessPatch.AddMoveModuleMessage( SendMessage : TG2SendMessage; aModule : TG2FileModule; aCol, aRow : byte);
var MemStream : TG2Message;
begin
  // Add a move module message
  MemStream := TG2Message.Create;
  try
    MemStream.WriteMessage( S_MOV_MODULE);
    MemStream.WriteMessage( ord(aModule.Location));
    MemStream.WriteMessage( aModule.ModuleIndex);
    MemStream.WriteMessage( aCol);
    MemStream.WriteMessage( aRow);
    SendMessage.Add(MemStream);
  finally
    MemStream.Free;
  end;
end;

procedure TG2MessPatch.AddConnectionMessage( SendMessage : TG2SendMessage; aFromConnector, aToConnector : TG2FileConnector);
var BitWriter : TBitWriter;
    MemStream : TG2Message;
    FromModule, ToModule : TG2FileModule;
    Location : TLocationType;
    TempConnector : TG2FileConnector;
begin
  // The to-connector must be an input, switch if it's an output
  if aToConnector.ConnectorKind = ckOutput then begin
    TempConnector := aToConnector;
    aToConnector := aFromConnector;
    aFromConnector := TempConnector;
  end;

  FromModule := aFromConnector.Module;
  ToModule := aToConnector.Module;
  Location := FromModule.Location;

  MemStream := TG2Message.Create;
  BitWriter := TBitWriter.Create;
  try
    MemStream.WriteMessage( S_ADD_CABLE);

    BitWriter.WriteBits( MemStream, 1, 4); // Unknown
    BitWriter.WriteBits( MemStream, ord(Location), 1);
    BitWriter.WriteBits( MemStream, aFromConnector.ConnectorColor, 3);
    BitWriter.WriteBits( MemStream, FromModule.ModuleIndex, 8);
    BitWriter.WriteBits( MemStream, Ord(aFromConnector.ConnectorKind), 2);
    BitWriter.WriteBits( MemStream, aFromConnector.ConnectorIndex, 6);
    BitWriter.WriteBits( MemStream, ToModule.ModuleIndex, 8);
    BitWriter.WriteBits( MemStream, Ord(aToConnector.ConnectorKind), 2);
    BitWriter.WriteBits( MemStream, aToConnector.ConnectorIndex, 6);

    SendMessage.Add( MemStream);
  finally
    BitWriter.Free;
    MemStream.Free;
  end;
end;

{procedure TG2MessPatch.AddDeleteConnectionMessage( SendMessage : TG2SendMessage; aFromConnector, aToConnector : TG2FileConnector);
var FromModule, ToModule : TG2FileModule;
    Location : TLocationType;
    BitWriter : TBitWriter;
    MemStream : TG2Message;
begin
  MemStream := TG2Message.Create;
  BitWriter := TBitWriter.Create;
  try
    FromModule := aFromConnector.Module;
    ToModule := aToConnector.Module;

    Location := FromModule.Location;

    // Add messages for uprate changes and cable color changes
    //if aFromConnector.ConnectorColor = COLOR_RED then
    //  CheckUprateChange( MemStream, 0, aToConnector, ToModule);

    // Create the delete cable message
    BitWriter.WriteBits( MemStream, S_DEL_CABLE, 8);
    BitWriter.WriteBits( MemStream, 1, 7); // Unknown
    BitWriter.WriteBits( MemStream, ord(Location), 1);
    BitWriter.WriteBits( MemStream, FromModule.ModuleIndex, 8);
    BitWriter.WriteBits( MemStream, Ord(aFromConnector.ConnectorKind), 2);
    BitWriter.WriteBits( MemStream, aFromConnector.ConnectorIndex, 6);
    BitWriter.WriteBits( MemStream, ToModule.ModuleIndex, 8);
    BitWriter.WriteBits( MemStream, Ord(aToConnector.ConnectorKind), 2);
    BitWriter.WriteBits( MemStream, aToConnector.ConnectorIndex, 6);

    SendMessage.Add( MemStream);
  finally
    BitWriter.Free;
    MemStream.Free;
  end;
end;}

procedure TG2MessPatch.AddDeleteConnectionMessage( SendMessage : TG2SendMessage; aLocation : TLocationType;
                                                   aFromModuleIndex : byte; aFromConnectorKind : TConnectorKind; aFromConnectorIndex : byte;
                                                   aToModuleIndex : byte; aToConnectorKind : TConnectorKind; aToConnectorIndex : byte);
var BitWriter : TBitWriter;
    MemStream : TG2Message;
begin
  MemStream := TG2Message.Create;
  BitWriter := TBitWriter.Create;
  try

    // Create the delete cable message
    BitWriter.WriteBits( MemStream, S_DEL_CABLE, 8);
    BitWriter.WriteBits( MemStream, 1, 7); // Unknown
    BitWriter.WriteBits( MemStream, ord(aLocation), 1);
    BitWriter.WriteBits( MemStream, aFromModuleIndex, 8);
    BitWriter.WriteBits( MemStream, Ord(aFromConnectorKind), 2);
    BitWriter.WriteBits( MemStream, aFromConnectorIndex, 6);
    BitWriter.WriteBits( MemStream, aToModuleIndex, 8);
    BitWriter.WriteBits( MemStream, Ord(aToConnectorKind), 2);
    BitWriter.WriteBits( MemStream, aToConnectorIndex, 6);

    SendMessage.Add( MemStream);
  finally
    BitWriter.Free;
    MemStream.Free;
  end;
end;

procedure TG2MessPatch.AddDeleteCableMessage( SendMessage : TG2SendMessage; aCable: TG2FileCable);
begin
  AddDeleteConnectionMessage( SendMessage, aCable.FromConnector.Module.Location,
                              aCable.FromConnector.Module.ModuleIndex,
                              aCable.FromConnector.ConnectorKind,
                              aCable.FromConnector.ConnectorIndex,
                              aCable.ToConnector.Module.ModuleIndex,
                              aCable.ToConnector.ConnectorKind,
                              aCable.ToConnector.ConnectorIndex);
end;

procedure TG2MessPatch.AddAssignKnobMessage( SendMessage : TG2SendMessage; aLocation : TLocationType; aModule, aParam, aKnobIndex: integer);
var BitWriter : TBitWriter;
    MemStream : TG2Message;
begin
  MemStream := TG2Message.Create;
  BitWriter := TBitWriter.Create;
  try
    // Assign the knob
    MemStream.WriteMessage( S_ASSIGN_KNOB);
    BitWriter.WriteBits( MemStream, aModule,        8);
    BitWriter.WriteBits( MemStream, aParam,         8);
    BitWriter.WriteBits( MemStream, ord(aLocation), 2);
    BitWriter.WriteBits( MemStream, $00,            6);
    BitWriter.WriteBits( MemStream, $00,            8);
    BitWriter.WriteBits( MemStream, aKnobIndex,     8);

    SendMessage.Add( MemStream);
  finally
    BitWriter.Free;
    MemStream.Free;
  end;
end;

procedure TG2MessPatch.AddDeAssignKnobMessage( SendMessage : TG2SendMessage; aKnobIndex : integer);
var MemStream : TG2Message;
begin
  MemStream := TG2Message.Create;
  try
    MemStream.WriteMessage( S_DEASSIGN_KNOB);
    MemStream.WriteMessage( $00);
    MemStream.WriteMessage( aKnobIndex);
    SendMessage.Add( MemStream);
  finally
    MemStream.Free;
  end;
end;

procedure TG2MessPatch.AddSelectParamPageMessage( SendMessage : TG2SendMessage; aPage: integer);
var MemStream : TG2Message;
begin
  // Select the page
  MemStream := TG2Message.Create;
  try
    MemStream.WriteMessage( S_SEL_PARAM_PAGE);
    MemStream.WriteMessage( aPage);
    SendMessage.Add( MemStream);
  finally
    MemStream.Free;
  end;
end;

procedure TG2MessPatch.AddAssignMidiCCMessage( SendMessage : TG2SendMessage; aLocation: TLocationType; aModule, aParam, aMidiCC: integer);
var MemStream : TG2Message;
begin
  // assign/deassign cc
  MemStream := TG2Message.Create;
  try
    MemStream.WriteMessage( S_ASSIGN_MIDICC);
    MemStream.WriteMessage( ord(aLocation));
    MemStream.WriteMessage( aModule);
    MemStream.WriteMessage( aParam);
    MemStream.WriteMessage( aMidiCC);
    SendMessage.Add( MemStream);
  finally
    MemStream.Free;
  end;
end;

procedure TG2MessPatch.AddDeassignMidiCCMessage( SendMessage : TG2SendMessage; aMidiCC: integer);
var MemStream : TG2Message;
begin
  MemStream := TG2Message.Create;
  try
    MemStream.WriteMessage( S_DEASSIGN_MIDICC);
    MemStream.WriteMessage( aMidiCC);
    SendMessage.Add( MemStream);
  finally
    MemStream.Free;
  end;
end;

procedure TG2MessPatch.AddAssignGlobalKnobMessage( SendMessage : TG2SendMessage; aLocation : TLocationType; aModule, aParam, aKnob: integer);
var BitWriter : TBitWriter;
    MemStream : TG2Message;
begin
  MemStream := TG2Message.Create;
  BitWriter := TBitWriter.Create;
  try
    // Assign the knob
    MemStream.WriteMessage( S_ASS_GLOBAL_KNOB);
    BitWriter.WriteBits( MemStream, Slot.SlotIndex, 4);
    BitWriter.WriteBits( MemStream, ord(aLocation), 2);
    BitWriter.WriteBits( MemStream, $00,       2);
    BitWriter.WriteBits( MemStream, aModule,   8);
    BitWriter.WriteBits( MemStream, aParam,    8);
    BitWriter.WriteBits( MemStream, $00,       8);
    BitWriter.WriteBits( MemStream, aKnob,     8);
    SendMessage.Add( MemStream);
  finally
    BitWriter.Free;
    MemStream.Free;
  end;
end;

procedure TG2MessPatch.AddDeassignGlobalKnobMessage( SendMessage : TG2SendMessage; aKnob: integer);
var MemStream : TG2Message;
begin
  MemStream := TG2Message.Create;
  try
    MemStream.WriteMessage( S_DEASS_GLOB_KNOB);
    MemStream.WriteMessage( $00);
    MemStream.WriteMessage( aKnob);
    SendMessage.Add( MemStream);
  finally
    MemStream.Free;
  end;
end;

procedure TG2MessPatch.AddSelectGlobalParamPageMessage( SendMessage : TG2SendMessage; aPage : integer);
var MemStream : TG2Message;
begin
  // Select the page
  MemStream := TG2Message.Create;
  try
    MemStream.WriteMessage( S_SEL_GLOBAL_PAGE);
    MemStream.WriteMessage( aPage);
    SendMessage.Add( MemStream);
  finally
    MemStream.Free;
  end;
end;

procedure TG2MessPatch.AddSetPatchDescriptionMessage( SendMessage : TG2SendMessage; FPatchDescription : TPatchDescription);
var Chunk : TPatchChunk;
    MemStream : TG2Message;
begin
  MemStream := TG2Message.Create;
  try
    Chunk := TPatchChunk.Create( MemStream);
    try
      FPatchDescription.Write( Chunk);
      Chunk.WriteChunk( C_PATCH_DESCR);
      SendMessage.Add( MemStream);
    finally
      Chunk.Free;
    end;
  finally
    MemStream.Free;
  end;
end;

procedure TG2MessPatch.AddSetModuleParamLabelsMessage( SendMessage : TG2SendMessage; aLocation: TLocationType; aModuleIndex, aParamIndex: byte; aName: AnsiString);
var MemStream : TG2Message;
    Chunk : TPatchChunk;
    ParamLabelModule, NewParamLabelModule : TParamLabelModule;
    Module : TG2FileModule;
    Parameter : TG2FileParameter;
    p : integer;
begin
  MemStream := TG2Message.Create;
  try
    ParamLabelModule := ParameterLabels[ ord(aLocation)].FindParamLabelModule( aModuleIndex);
    if assigned( ParamLabelModule) then begin
      // Record with Param labels for the module already exists, copy record and change label
      NewParamLabelModule := TParamLabelModule.CopyCreate( True, ParamLabelModule);
      NewParamLabelModule.AddParamLabel( aParamIndex, aName);
    end else begin
      // Create a new record
      NewParamLabelModule := TParamLabelModule.Create( True);
      NewParamLabelModule.ModuleIndex := aModuleIndex;
      Module := Modules[ ord(aLocation), aModuleIndex];
      if not assigned(Module) then
        exit;

      // It seems necessary that the parameter names are in the order of de parameterindex
      // otherwise, sending to G2 goes o.k., but saving and then uploading to the G2 fails!
      for p := 0 to Module.ParameterCount - 1 do begin
        Parameter := Module.Parameter[p];
        if Parameter.CanChangeLabel then
          if Parameter.ParamIndex = p then begin
            if Parameter.ParamIndex = aParamIndex then
              NewParamLabelModule.AddParamLabel( Parameter.ParamIndex, aName)
            else
              NewParamLabelModule.AddParamLabel( Parameter.ParamIndex, Parameter.ParamLabel);
          end;
      end;
    end;

    Chunk := TPatchChunk.Create( MemStream);
    try
      MemStream.WriteMessage( S_SET_PARAM_LABEL);
      MemStream.WriteMessage( ord(aLocation));
      NewParamLabelModule.Write( Chunk);
      Chunk.Flush;
      SendMessage.Add( MemStream);
    finally
      Chunk.Free;
      NewParamLabelModule.Free;
    end;
  finally
    MemStream.Free;
  end;
end;

procedure TG2MessPatch.AddCopyModuleParamLabelsMessage( SendMessage : TG2SendMessage; aLocation: TLocationType; aModule : TG2FileModule);
var MemStream : TG2Message;
    Chunk : TPatchChunk;
    ParamLabelModule, NewParamLabelModule : TParamLabelModule;
begin
  MemStream := TG2Message.Create;
  try
    ParamLabelModule := ParameterLabels[ ord(aLocation)].FindParamLabelModule( aModule.ModuleIndex);
    if assigned( ParamLabelModule) then begin
      // Record with Param labels for the module already exists, copy record and change label
      NewParamLabelModule := TParamLabelModule.CopyCreate( True, ParamLabelModule);
    end else begin
      // Create a new empty record
      NewParamLabelModule := TParamLabelModule.Create( True);
      NewParamLabelModule.ModuleIndex := aModule.ModuleIndex;
    end;

    Chunk := TPatchChunk.Create( MemStream);
    try
      MemStream.WriteMessage( S_SET_PARAM_LABEL);
      MemStream.WriteMessage( ord(aLocation));
      NewParamLabelModule.Write( Chunk);
      Chunk.Flush;
      SendMessage.Add( MemStream);
    finally
      Chunk.Free;
      NewParamLabelModule.Free;
    end;
  finally
    MemStream.Free;
  end;
end;

procedure TG2MessPatch.AddSetModuleLabelMessage( SendMessage : TG2SendMessage; aLocation: TLocationType; aModuleIndex: byte; aName: AnsiString);
var MemStream : TG2Message;
begin
  MemStream := TG2Message.Create;
  try
    MemStream.WriteMessage( S_SET_MODULE_LABEL);
    MemStream.WriteMessage( ord(aLocation));
    MemStream.WriteMessage( aModuleIndex);
    MemStream.WriteClaviaString( aName);
    SendMessage.Add( MemStream);
  finally
    MemStream.Free;
  end;
end;

procedure TG2MessPatch.AddSetModuleColorMessage( SendMessage : TG2SendMessage; aLocation: TLocationType; aModuleIndex, aColor : byte);
var MemStream : TG2Message;
begin
  MemStream := TG2Message.Create;
  try
    MemStream.WriteMessage( S_SET_MODULE_COLOR);
    MemStream.WriteMessage( ord(aLocation));
    MemStream.WriteMessage( aModuleIndex);
    MemStream.WriteMessage( aColor);
    SendMessage.Add( MemStream);
  finally
    MemStream.Free;
  end;
end;

procedure TG2MessPatch.AddSetMorphMessage( SendMessage : TG2SendMessage; aLocation, aModule, aParam, aMorph, aValue, aNegative, aVariation: byte);
var MemStream : TG2Message;
begin
  MemStream := TG2Message.Create;
  try
    MemStream.WriteMessage( S_SET_MORPH_RANGE);
    MemStream.WriteMessage( aLocation);
    MemStream.WriteMessage( aModule);
    MemStream.WriteMessage( aParam);
    MemStream.WriteMessage( aMorph);
    MemStream.WriteMessage( aValue);
    MemStream.WriteMessage( aNegative);
    MemStream.WriteMessage( aVariation);
    SendMessage.Add( MemStream);
  finally
    MemStream.Free;
  end;
end;

procedure TG2MessPatch.AddDeleteModuleMessage( SendMessage : TG2SendMessage; aLocation : TLocationType; aModuleIndex : byte);
var MemStream : TG2Message;
begin
  MemStream := TG2Message.Create;
  try
    MemStream.WriteMessage( S_DEL_MODULE);
    MemStream.WriteMessage( ord(aLocation));
    MemStream.WriteMessage( aModuleIndex);
    SendMessage.Add( MemStream);
  finally
    MemStream.Free;
  end;
end;

procedure TG2MessPatch.AddCopyModuleMessage( SendMessage : TG2SendMessage; aLocation : TLocationType; aModule : TG2FileModule);
var MemStream : TG2Message;
    i : integer;
begin
  MemStream := TG2Message.Create;
  try
    MemStream.WriteMessage( S_ADD_MODULE);
    MemStream.WriteMessage( aModule.TypeID);
    MemStream.WriteMessage( ord(aLocation));
    MemStream.WriteMessage( aModule.ModuleIndex);
    MemStream.WriteMessage( aModule.Col);
    MemStream.WriteMessage( aModule.Row);
    MemStream.WriteMessage( 0);
    MemStream.WriteMessage( aModule.Uprate);
    MemStream.WriteMessage( aModule.IsLed);
    for i := 0 to aModule.ModeCount - 1 do
      MemStream.WriteMessage( aModule.ModeInfo[i]);
    MemStream.WriteClaviaString( aModule.ModuleName);
    SendMessage.Add( MemStream);
  finally
    MemStream.Free;
  end;
end;

procedure TG2MessPatch.AddCopyModuleParametersMessage( SendMessage : TG2SendMessage; aLocation : TLocationType; aModule : TG2FileModule);
var MemStream : TG2Message;
    Chunk : TPatchChunk;
    j, k : integer;
begin
  MemStream := TG2Message.Create;
  try
    Chunk := TPatchChunk.Create( MemStream);
    try
      // Write parameterlist chunk
      Chunk.WriteBits( ord(aLocation), 2);

      if aModule.ParameterCount = 0 then begin
        Chunk.WriteBits( 0,              8); // SetCount
        Chunk.WriteBits( 10,             8); // VariationCount
      end else begin
        Chunk.WriteBits( 1,              8); // SetCount
        Chunk.WriteBits( 10,             8); // VariationCount

        Chunk.WriteBits( aModule.ModuleIndex,    8); // ModuleIndex
        Chunk.WriteBits( aModule.ParameterCount, 7); // ParamCount
        for j := 0 to 9 do begin // Variations
          Chunk.WriteBits( j, 8);

          for k := 0 to aModule.ParameterCount - 1 do begin
            Chunk.WriteBits( aModule.Parameter[k].GetParameterValue, 7);
          end;
        end;
      end;
      Chunk.WriteChunk( $4d);
      Chunk.Flush;
    finally
      Chunk.Free;
    end;
    SendMessage.Add( MemStream);
  finally
    MemStream.Free;
  end;
end;

procedure TG2MessPatch.AddNewModuleMessage( SendMessage : TG2SendMessage; aLocation : TLocationType; aNewModuleIndex, aModuleType, aCol, aRow: byte);
var Chunk : TPatchChunk;
    BitWriter : TBitWriter;
    MemStream : TG2Message;
    ModuleDef : TXMLModuleDefType;
    ParamList : TXMLParamListType;
    ParamDef : TXMLParamDefType;
    ParameterLabels : TParameterLabels;
    i : LongWord;
    j : integer;
begin
  if not assigned(G2.FModuleDefList) then
    raise Exception.Create('Module database not loaded.');

  MemStream := TG2Message.Create;
  BitWriter := TBitWriter.Create;
  try
    // Get the module data
    i := 0;
    while ( i < G2.FModuleDefList.Count) and (G2.FModuleDefList.ModuleDef[i].ModuleType <> aModuleType) do
      inc(i);

    if not( i < G2.FModuleDefList.Count) then
      raise Exception.Create('Module type ' + IntToStr(aModuleType) + ' not found.');

    ModuleDef := G2.FModuleDefList.ModuleDef[ i];

    // Create undo add module message
    AddDeleteModuleMessage( FUndoMessage, aLocation, aNewModuleIndex);

    // Create the add module message
    Chunk := TPatchChunk.Create( MemStream);
    try
      Chunk.WriteBits( S_ADD_MODULE,  8);
      Chunk.WriteBits( aModuleType,      8);
      Chunk.WriteBits( ord(aLocation),   8);
      Chunk.WriteBits( aNewModuleIndex,  8);
      Chunk.WriteBits( aCol,             8);
      Chunk.WriteBits( aRow,             8);
      Chunk.WriteBits( 0,                8);
      Chunk.WriteBits( ModuleDef.Uprate, 8);
      Chunk.WriteBits( ModuleDef.IsLed,  8);

      ParamList := ModuleDef.Modes;
      if assigned(ParamList) then
        try
          for i := 0 to ParamList.Count - 1 do begin
            ParamDef := G2.FParamDefList[ ParamList.Param[i].Id];
            Chunk.WriteBits( ParamDef.DefaultValue, 8);
          end;
        finally
          ParamList.Free;
        end;

      Chunk.WriteName( ModuleDef.ShortName);
      Chunk.Flush;

      // Write empty cablechunk
      Chunk.WriteBits( ord(aLocation),   2);
      Chunk.WriteBits( 0,               12); // Unknown
      Chunk.WriteBits( 0,               10); // CableCount
      Chunk.WriteChunk( $52);

      // Write parameterlist chunk
      {Chunk.WriteBits( ord(aLocation),  2);
      Chunk.WriteBits( 1,               8); // SetCount
      Chunk.WriteBits( 10,              8); // VariationCount, must be 10!

      Chunk.WriteBits( aNewModuleIndex, 8); // ModuleIndex}

      Chunk.WriteBits( ord(aLocation),  2);
      ParamList := ModuleDef.Params;
      if assigned(ParamList) then begin
        Chunk.WriteBits( 1,               8); // SetCount
        Chunk.WriteBits( 10,              8); // VariationCount, must be 10!

        Chunk.WriteBits( aNewModuleIndex, 8); // ModuleIndex
        try
          Chunk.WriteBits( ParamList.Count,  7); // ParamCount
          for i := 0 to 9 do begin // Variations
            Chunk.WriteBits( i, 8);

            for j := 0 to ParamList.Count - 1 do begin
              Chunk.WriteBits( ParamList.Param[j].DefaultValue, 7);
            end;
          end;
        finally
          ParamList.Free;
        end;
      end else begin
        // No parameters
        {Chunk.WriteBits( 0,  7); // ParamCount
        for i := 0 to 9 do // Variations
          Chunk.WriteBits( i, 8);}
        Chunk.WriteBits( 0,               8); // SetCount
        Chunk.WriteBits( 10,              8); // VariationCount, must be 10!
      end;
      Chunk.WriteChunk( $4d);

      // Write paramnames chunk

      if aModuleType = 121 then begin
        // In case of SeqNote, 2 parameters are put in the paramnames chunk:
        // [0, 1, mag, 0, 1, octave]
        // mag: 0=3-octaves,1=2-octaves,2=1-octave
        // octave: 0-9 (c0-c9)

        Chunk.WriteBits( ord(aLocation),  2);
        Chunk.WriteBits( 1,               8); // ModuleCount
        Chunk.WriteBits( aNewModuleIndex, 8); // ModuleIndex
        Chunk.WriteBits( 6,               8); // ModLen

        Chunk.WriteBits( 0,               8); // Magnification
        Chunk.WriteBits( 1,               8);
        Chunk.WriteBits( 1,               8); // 2 - octaves

        Chunk.WriteBits( 0,               8); // Octave
        Chunk.WriteBits( 1,               8);
        Chunk.WriteBits( 5,               8); // c5

        Chunk.WriteChunk( $5b);
      end else begin
        Chunk.WriteBits( ord(aLocation),  2);
        {Chunk.WriteBits( 0,               8); // ModuleCount
        Chunk.WriteChunk( $5b);}

        ParamList := ModuleDef.Params;
        ParameterLabels := TParameterLabels.Create(True);
        if assigned(ParamList) then begin
          try
            for i := 0 to ParamList.Count - 1 do begin
              if ModuleDef.Params[i].ParamLabel <> '' then begin
                ParameterLabels.AddParamLabel( aNewModuleIndex, i,  ModuleDef.Params[i].ParamLabel);
              end;
            end;
            ParameterLabels.Write( Chunk);
          finally
            ParameterLabels.Free;
            ParamList.Free;
          end;
        end else
          Chunk.WriteBits( 0,               8); // ModuleCount

        Chunk.WriteChunk( $5b);
      end;

      // Write modulenames chunk
      Chunk.WriteBits( ord(aLocation), 2);
      Chunk.WriteBits( 0,              6); // Unknown
      Chunk.WriteBits( 1,              8); // NameCount

      Chunk.WriteBits( aNewModuleIndex, 8);
      Chunk.WriteName( ModuleDef.ShortName);
      Chunk.WriteChunk( $5a);

      SendMessage.Add( MemStream);

    finally
      Chunk.Free;
    end;
  finally
    BitWriter.Free;
    MemStream.Free;
  end;
end;

procedure TG2MessPatch.AddCopyModulesMessage( SendMessage : TG2SendMessage; aSrcePatch : TG2FilePatchPart; aFromLocation, aToLocation : TLocationType; RenumberModules : boolean);
var Chunk : TPatchChunk;
    BitWriter : TBitWriter;
    MemStream : TG2Message;
    i, j, k, m, FromModuleIndex : integer;
    Module : TG2FileModule;
    Cable : TG2FileCable;
    RenumberTable : array of integer;

  function GetNewModuleIndex( aOldModuleIndex : integer): integer;
  var i : integer;
  begin
    if not RenumberModules then begin
      // Do not renumber (undo)
      GetNewModuleIndex := aOldModuleIndex;
    end else begin
      i := 0;
      while (i<Length(RenumberTable)) and (aOldModuleIndex <> RenumberTable[i]) do
        inc(i);
      if (i<Length(RenumberTable)) then
        Result := i + FromModuleIndex
      else
        raise Exception.Create('Error renumbering modules.');
    end;
  end;

begin
  // Renumber modules in sourcepatch;
  FromModuleIndex := GetMaxModuleIndex( aToLocation) + 1;

  SetLength( RenumberTable, aSrcePatch.ModuleList.Count);
  for i := 0 to aSrcePatch.ModuleList.Count - 1 do
    RenumberTable[i] := aSrcePatch.ModuleList.Items[i].ModuleIndex;

  MemStream := TG2Message.Create;
  BitWriter := TBitWriter.Create;
  try
    Chunk := TPatchChunk.Create( MemStream);
    try
      for m := 0 to aSrcePatch.ModuleList.Count - 1 do begin

        Module := aSrcePatch.ModuleList[m];

        // Create undo add module message
        AddDeleteModuleMessage( FUndoMessage, aToLocation,  GetNewModuleIndex(Module.ModuleIndex));

        Chunk.WriteBits( S_ADD_MODULE,                          8);
        Chunk.WriteBits( Module.TypeID,                         8);
        Chunk.WriteBits( ord(aToLocation),                      8);
        Chunk.WriteBits( GetNewModuleIndex(Module.ModuleIndex), 8);
        Chunk.WriteBits( Module.Col,                            8);
        Chunk.WriteBits( Module.Row,                            8);
        Chunk.WriteBits( 0,                                     8);
        Chunk.WriteBits( Module.Uprate,                         8); // TODO Recalc uprate
        Chunk.WriteBits( Module.IsLed,                          8);
        for i := 0 to Module.ModeCount - 1 do
          Chunk.WriteBits( Module.ModeInfo[i], 8);
        Chunk.WriteName( Module.ModuleName);
        Chunk.Flush;
      end;

      // Create undo add cables messages

      // Write cablechunk
      Chunk.WriteBits( ord(aToLocation),  2);
      Chunk.WriteBits( aSrcePatch.CableList.Unknown,    12); // Unknown
      Chunk.WriteBits( aSrcePatch.CableList.Count,      10); // CableCount
      for i := 0 to aSrcePatch.CableList.Count - 1 do begin
        Cable := aSrcePatch.CableList[i];

        if Cable.LinkType = 0 then
          AddDeleteConnectionMessage( FUndoMessage,
                                      aToLocation,
                                      GetNewModuleIndex(Cable.ModuleFrom),
                                      ckInput,
                                      Cable.ConnectorFrom,
                                      GetNewModuleIndex(Cable.ModuleTo),
                                      ckInput,
                                      Cable.ConnectorTo)
        else
          AddDeleteConnectionMessage( FUndoMessage,
                                      aToLocation,
                                      GetNewModuleIndex(Cable.ModuleFrom),
                                      ckOutput,
                                      Cable.ConnectorFrom,
                                      GetNewModuleIndex(Cable.ModuleTo),
                                      ckInput,
                                      Cable.ConnectorTo);

        Chunk.WriteBits( Cable.CableColor, 3);
        Chunk.WriteBits( GetNewModuleIndex(Cable.ModuleFrom), 8);
        Chunk.WriteBits( Cable.ConnectorFrom, 6);
        Chunk.WriteBits( Cable.LinkType, 1);
        Chunk.WriteBits( GetNewModuleIndex(Cable.ModuleTo), 8);
        Chunk.WriteBits( Cable.ConnectorTo, 6);
      end;
      Chunk.WriteChunk( $52);

      // Write parameterlist chunk
      Chunk.WriteBits( ord(aToLocation),                         2);
      Chunk.WriteBits( aSrcePatch.ParameterList.Count,          8); // SetCount
      Chunk.WriteBits( aSrcePatch.ParameterList.VariationCount, 8); // VariationCount

      for i := 0 to aSrcePatch.ParameterList.Count - 1 do begin
        Chunk.WriteBits( GetNewModuleIndex( aSrcePatch.ParameterList[i].ModuleIndex), 8); // ModuleIndex
        Chunk.WriteBits( aSrcePatch.ParameterList[i].ParameterCount,                 7); // ParamCount
        for j := 0 to aSrcePatch.ParameterList[i].Count - 1 do begin // Variations
          Chunk.WriteBits( aSrcePatch.ParameterList[i].Items[j].Variation, 8);

          for k := 0 to aSrcePatch.ParameterList[i].ParameterCount - 1 do begin
            Chunk.WriteBits( aSrcePatch.ParameterList[i].Items[j].ParamValues[k], 7);
          end;
        end;
      end;
      Chunk.WriteChunk( $4d);

      // Write paramnames chunk
      Chunk.WriteBits( ord(aToLocation), 2);
      Chunk.WriteBits( aSrcePatch.ParameterLabels.Count, 8); // ModuleCount
      for i := 0 to aSrcePatch.ParameterLabels.Count - 1 do begin
        Chunk.WriteBits( GetNewModuleIndex( aSrcePatch.ParameterLabels[i].ModuleIndex), 8);
        Chunk.WriteBits( aSrcePatch.ParameterLabels[i].GetModuleLabelsLength, 8);
        for j := 0 to aSrcePatch.ParameterLabels[i].Count - 1 do begin
          aSrcePatch.ParameterLabels[i].Items[j].Write( Chunk);
        end;
      end;
      Chunk.WriteChunk( $5b);

      // Write modulenames chunk
      Chunk.WriteBits( ord(aToLocation),                  2);
      Chunk.WriteBits( aSrcePatch.ModuleLabels.Unknown,   6); // Unknown
      Chunk.WriteBits( aSrcePatch.ModuleLabels.Count, 8); // NameCount
      for i := 0 to  aSrcePatch.ModuleLabels.Count - 1 do begin

        Chunk.WriteBits( GetNewModuleIndex( aSrcePatch.ModuleLabels[i].ModuleIndex), 8);
        Chunk.WriteName( aSrcePatch.ModuleLabels[i].ModuleLabel);
      end;
      Chunk.WriteChunk( $5a);

      SendMessage.Add( MemStream);

    finally
      Chunk.Free;
    end;

  finally
    BitWriter.Free;
    MemStream.Free;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// Create patch messages
////////////////////////////////////////////////////////////////////////////////

function TG2MessPatch.CreatePatchMessage: TG2SendMessage;
var Slot : TG2MessSlot;
begin
  Slot := GetSlot;

  // Initialize the undo message
  if assigned(FUndoMessage) then
    FUndoMessage.Free;
  FUndoMessage := TG2SendMessage.Create;
  FUndoMessage.AddReversed := True;
  FUndoMessage.Offset := 5;

  FUndoMessage.WriteMessage( $01);
  FUndoMessage.WriteMessage( CMD_REQ + CMD_SLOT + Slot.SlotIndex);
  FUndoMessage.WriteMessage( Slot.PatchVersion);

  Result := TG2SendMessage.Create;
  Result.WriteMessage( $01);
  Result.WriteMessage( CMD_REQ + CMD_SLOT + Slot.SlotIndex);
  Result.WriteMessage( Slot.PatchVersion);
end;

procedure TG2MessPatch.ResetUprateValues( aLocation : TLocationType);
var i : integer;
    Module : TG2FileModule;
begin
  // Set NewUprate to Uprate
  for i := 0 to PatchPart[ ord(aLocation)].ModuleList.Count - 1 do begin
    Module := PatchPart[ ord(aLocation)].ModuleList[i];
    Module.NewUprate := Module.Uprate;
  end;
end;

procedure TG2MessPatch.CheckUprateChange( aStream : TG2SendMessage; aUprateValue : Byte; aToConnector : TG2FileConnector; aToModule: TG2FileModule);
var i, j : integer;
    Connector : TG2FileConnector;
    Cable : TG2FileCable;
begin
  // This function must be called when a cable is added or deleted
  // aUprateValue is the new uprate value of the from-module

  // This function is called before actual changes in the patch are made, so temporary
  // values are used in module (NewUprate) and connector (NewConnectorColor) to calculte the
  // new state.
  // Before using this function, these temporary values must be reset to the actual values
  // with ResetUprateValues

  // See Chapter 10 in manual.

  // Does the module already have the same uprate value?
  if aToModule.Uprate = aUprateValue then
    exit;

  // Check to module connector color
  if (aUprateValue = 1) and (aToConnector.NewRate = 1) then
    exit;

  if (aUprateValue = 0) and (aToConnector.NewRate = 0) then
    exit;

  // If downrate, downrate the module only if it's not uprated by others cables
  if (aUprateValue = 0) then begin
    // Check if module is uprated by other cables than this one
    i := 0;
    while (i < aToModule.InConnectorCount) do begin

      Connector := aToModule.InConnector[i];
      if Connector <> aToConnector then begin
        j := 0;
        while (j < Connector.CableCount) do begin
          if Connector.Cables[j].FromConnector.NewRate = 1 then
            exit; // Found another cable that uprates this module
          inc(j);
        end;
      end;
      inc(i);
    end;
  end;

  // Write uprate message for module
  aToModule.NewUprate := aUprateValue;
  AddSetUprateMessage( FUndoMessage, aToModule, aToModule.Uprate);
  AddSetUprateMessage( aStream, aToModule, aToModule.NewUprate);

  // Go through all the outgoing connectors and recursively calc uprate changes of connecting modules
  i := 0;
  while ( i < aToModule.OutConnectorCount) do begin

    Connector := aToModule.OutConnector[i];

    j := 0;
    while ( j < Connector.CableCount) do begin

      Cable := Connector.Cables[j];

      if (Cable.FromConnector.NewRate <> Cable.ToConnector.NewRate)
         and (Cable.ToConnector.BandWidth = btDynamic) then begin
        // Write cable change message
        AddSetCableColorMessage( FUndoMessage, Cable, Cable.CableColor);
        AddSetCableColorMessage( aStream, Cable, Connector.NewConnectorColor);

        if Cable.ToConnector.Module <> aToModule then
          CheckUprateChange( aStream, aUprateValue, Cable.ToConnector, Cable.ToConnector.Module);
      end;

      inc(j);
    end;

    inc(i);
  end;
end;

function TG2MessPatch.CreateAddNewModuleMessage( aLocation : TLocationType; aNewModuleIndex, aModuleType, aCol, aRow: byte): TG2SendMessage;
begin
  Result := CreatePatchMessage;
  AddNewModuleMessage( Result, aLocation, aNewModuleIndex, aModuleType, aCol, aRow);
end;

function TG2MessPatch.CreateCopyModulesMessage( aSrcePatch : TG2FilePatchPart; aFromLocation, aToLocation : TLocationType; RenumberModules : boolean): TG2SendMessage;
begin
  Result := CreatePatchMessage;
  AddCopyModulesMessage( Result, aSrcePatch, aFromLocation, aToLocation, RenumberModules);
end;

function TG2MessPatch.CreateDeleteModuleMessage( aLocation : TLocationType; aModuleIndex: byte): TG2SendMessage;
var BitWriter : TBitWriter;
    Module : TG2FileModule;
    Connector : TG2FileConnector;
    Cable : TG2FileCable;
    Param : TG2FileParameter;
    Morph :  TMorphParameter;
    Cables : TList;
    i, j, p, k, v : integer;
    DeleteModuleList : TModuleList;
    UndoPatch : TG2FilePatchPart;
begin
  BitWriter := TBitWriter.Create;
  Cables := TList.Create;
  try
    Result := CreatePatchMessage;

    Module := Modules[ ord(aLocation), aModuleIndex];
    if not assigned(Module) then
      raise Exception.Create('Module ' + IntToStr(aModuleIndex) + ' not found.');

    // Make a list of all cables to be deleted
    for i := 0 to Module.InConnectorCount - 1 do begin
      Connector := Module.InConnector[i];
      for j := 0 to Connector.CableCount - 1 do begin
        if Cables.IndexOf(Connector.Cables[j]) = -1  then
          Cables.Add(Connector.Cables[j]);
      end;
    end;

    for i := 0 to Module.OutConnectorCount - 1 do begin
      Connector := Module.OutConnector[i];
      for j := 0 to Connector.CableCount - 1 do begin
        if Cables.IndexOf(Connector.Cables[j]) = -1  then
          Cables.Add(Connector.Cables[j]);
      end;
    end;

    // Create undo delete module message
    DeleteModuleList := TModuleList.Create( False, nil);
    UndoPatch := TG2FilePatchPart.CopyModules( nil, PatchPart[ ord(aLocation)], DeleteModuleList);
    try
      DeleteModuleList.AddModule( Module);
      AddCopyModulesMessage( FUndoMessage, UndoPatch, DeleteModuleList[0].Location, DeleteModuleList[0].Location, False);
    finally
      UndoPatch.Free;
      DeleteMOduleList.Free;
    end;

    // Add messages for uprate changes and cable color changes
    ResetUprateValues( aLocation);

    // Create undo delete cables message, delete cable messages and uprate and color change messages
    for i := 0 to Cables.Count - 1 do begin
      Cable := TG2FileCable(Cables[i]);
      AddConnectionMessage( FUndoMessage, Cable.FromConnector, Cable.ToConnector);
      AddDeleteCableMessage( Result, Cable);
      CheckUprateChange( Result, Cable.ToConnector.DefRate, Cable.ToConnector, Cable.ToConnector.Module);
    end;

    // Delete controllers, knobs, global knobs, morphs associated with any parameter of the module
    for p := 0 to Module.ParameterCount - 1 do begin
      Param := Module.Parameter[p];

      if assigned(Param.Controller) then begin
        AddAssignMidiCCMessage( FUndoMessage, TLocationType(Param.Controller.Location), Param.Controller.ModuleIndex, Param.Controller.ParamIndex, Param.Controller.MidiCC);
        AddDeassignMidiCCMessage( Result, Param.Controller.MidiCC);
      end;

      if assigned(Param.Knob) then begin
        AddAssignKnobMessage( FUndoMessage, TlocationType(Param.Knob.Location), Param.Knob.ModuleIndex, Param.Knob.ParamIndex, Param.Knob.KnobIndex);
        AddDeassignKnobMessage( Result, Param.Knob.KnobIndex);
      end;

      if assigned(Param.GlobalKnob) then begin
        AddAssignGlobalKnobMessage( FUndoMessage, TLocationType(Param.GlobalKnob.Location), Param.GlobalKnob.ModuleIndex, Param.GlobalKnob.ParamIndex, Param.GlobalKnob.KnobIndex);
        AddDeassignGlobalKnobMessage( Result, Param.GlobalKnob.KnobIndex);
      end;

      for k := 0 to NMORPHS - 1 do begin
        for v := 0 to NVARIATIONS - 1 do begin
          Morph := Param.GetMorph( k, v);
          if assigned(Morph) then begin
            if Morph.Range >= 128 then
              AddSetMorphMessage( FUndoMessage, ord(Param.Location), Param.ModuleIndex, Param.ParamIndex, k, abs(Morph.Range - 256), 1, v)
            else
              AddSetMorphMessage( FUndoMessage, ord(Param.Location), Param.ModuleIndex, Param.ParamIndex, k, Morph.Range, 0, v);
            AddSetMorphMessage( Result, ord(Param.Location), Param.ModuleIndex, Param.ParamIndex, k, 0, 0, v)
          end;
        end;
      end;
    end;

    // Now the delete module message
    AddDeleteModuleMessage( Result, aLocation, aModuleIndex);

    (G2 as TG2Mess).dump_buffer( PStaticByteBuffer(Result.Memory)^[0], Result.Size);

  finally
    Cables.Free;
    BitWriter.Free;
  end;
end;

function TG2MessPatch.CreateDeleteModulesMessage( aLocation : TLocationType): TG2SendMessage;
var BitWriter : TBitWriter;
    m, i, j, p, k, v : integer;
    Module, OtherModule : TG2FileModule;
    Connector : TG2FileConnector;
    Cable : TG2FileCable;
    Param : TG2FileParameter;
    Morph :  TMorphParameter;
    CablesInner, CablesOuter : TList;
begin
  BitWriter := TBitWriter.Create;
  CablesInner := TList.Create;
  CablesOuter := TList.Create;
  try
    Result := CreatePatchMessage;

    // Make a list of all cables to be deleted
    for m := 0 to ModuleCount[ord(aLocation)] - 1 do begin

      Module := ModuleList[ ord(aLocation)].Items[m];
      if Module.Selected then begin

        for i := 0 to Module.InConnectorCount - 1 do begin
          Connector := Module.InConnector[i];
          for j := 0 to Connector.CableCount - 1 do begin

            if Connector.Cables[j].ModuleTo = Module.ModuleIndex then
              OtherModule := Modules[ ord( aLocation), Connector.Cables[j].ModuleFrom]
            else
              OtherModule := Modules[ ord( aLocation), Connector.Cables[j].ModuleTo];

            if assigned(OtherModule) and OtherModule.Selected then begin
              // Both modules are to be deleted
              if CablesInner.IndexOf(Connector.Cables[j]) = -1  then
                CablesInner.Add(Connector.Cables[j]);
            end else
              if CablesOuter.IndexOf(Connector.Cables[j]) = -1  then
                CablesOuter.Add(Connector.Cables[j]);
          end;
        end;

        for i := 0 to Module.OutConnectorCount - 1 do begin
          Connector := Module.OutConnector[i];
          for j := 0 to Connector.CableCount - 1 do begin

            if Connector.Cables[j].ModuleFrom = Module.ModuleIndex then
              OtherModule := Modules[ ord( aLocation), Connector.Cables[j].ModuleTo]
            else
              OtherModule := Modules[ ord( aLocation), Connector.Cables[j].ModuleFrom];

            if assigned(OtherModule) and OtherModule.Selected then begin
              // Both modules are to be deleted
              if CablesInner.IndexOf(Connector.Cables[j]) = -1  then
                CablesInner.Add(Connector.Cables[j]);
            end else
              if CablesOuter.IndexOf(Connector.Cables[j]) = -1  then
                CablesOuter.Add(Connector.Cables[j]);
          end;
        end;
      end;
    end;

    // Add messages for uprate changes and cable color changes
    ResetUprateValues( aLocation);

    // Create undo delete cables message, delete cable messages and uprate and color change messages
    for i := 0 to CablesOuter.Count - 1 do begin
      Cable := TG2FileCable(CablesOuter[i]);
      AddConnectionMessage( FUndoMessage, Cable.FromConnector, Cable.ToConnector);
      AddDeleteCableMessage( Result, Cable);
      CheckUprateChange( Result, Cable.ToConnector.DefRate, Cable.ToConnector, Cable.ToConnector.Module);
    end;

    for i := 0 to CablesInner.Count - 1 do begin
      Cable := TG2FileCable(CablesInner[i]);
      AddConnectionMessage( FUndoMessage, Cable.FromConnector, Cable.ToConnector);
      AddDeleteCableMessage( Result, Cable);
      CheckUprateChange( Result, Cable.ToConnector.DefRate, Cable.ToConnector, Cable.ToConnector.Module);
    end;

    for m := 0 to ModuleCount[ord(aLocation)] - 1 do begin
      Module := ModuleList[ ord(aLocation)].Items[m];
      if Module.Selected then begin
        // Delete and undo delete controllers, knobs, global knobs, morphs associated with any parameter of the module
        for p := 0 to Module.ParameterCount - 1 do begin
          Param := Module.Parameter[p];

          if assigned(Param.Controller) then begin
            AddAssignMidiCCMessage( FUndoMessage, TLocationType(Param.Controller.Location), Param.Controller.ModuleIndex, Param.Controller.ParamIndex, Param.Controller.MidiCC);
            AddDeassignMidiCCMessage( Result, Param.Controller.MidiCC);
          end;

          if assigned(Param.Knob) then begin
            AddAssignKnobMessage( FUndoMessage, TlocationType(Param.Knob.Location), Param.Knob.ModuleIndex, Param.Knob.ParamIndex, Param.Knob.KnobIndex);
            AddDeassignKnobMessage( Result, Param.Knob.KnobIndex);
          end;

          if assigned(Param.GlobalKnob) then begin
            AddAssignGlobalKnobMessage( FUndoMessage, TLocationType(Param.GlobalKnob.Location), Param.GlobalKnob.ModuleIndex, Param.GlobalKnob.ParamIndex, Param.GlobalKnob.KnobIndex);
            AddDeassignGlobalKnobMessage( Result, Param.GlobalKnob.KnobIndex);
          end;

          for k := 0 to NMORPHS - 1 do begin
            for v := 0 to NVARIATIONS - 1 do begin
              Morph := Param.GetMorph( k, v);
              if assigned(Morph) then begin
                if Morph.Range >= 128 then
                  AddSetMorphMessage( FUndoMessage, ord(Param.Location), Param.ModuleIndex, Param.ParamIndex, k, abs(Morph.Range - 256), 1, v)
                else
                  AddSetMorphMessage( FUndoMessage, ord(Param.Location), Param.ModuleIndex, Param.ParamIndex, k, Morph.Range, 0, v);
                AddSetMorphMessage( Result, ord(Param.Location), Param.ModuleIndex, Param.ParamIndex, k, 0, 0, v)
              end;
            end;
          end;
        end;

        // Undo Delete Parameters
        AddCopyModuleParametersMessage( FUndoMessage, aLocation, Module);

        // Undo Delete Parameter labels
        AddCopyModuleParamLabelsMessage( FUndoMessage, aLocation, Module);

        // Undo Delete Module label
        AddSetModuleLabelMessage( FUndoMessage, aLocation, Module.ModuleIndex, Module.ModuleName);

        // Undo Delete module
        AddCopyModuleMessage( FUndoMessage, aLocation, Module);

        (G2 as TG2Mess).add_log_line( 'Undo message:', LOGCMD_NUL);
        (G2 as TG2Mess).dump_buffer( PStaticByteBuffer(FUndoMessage.Memory)^[0], FUndoMessage.Size);

        // Now the delete module message
        AddDeleteModuleMessage( Result, aLocation, Module.ModuleIndex);
      end;
    end;

    (G2 as TG2Mess).dump_buffer( PStaticByteBuffer(Result.Memory)^[0], Result.Size);

  finally
    CablesInner.Free;
    CablesOuter.Free;
    BitWriter.Free;
  end;
end;

function TG2MessPatch.CreateAddConnectionMessage( aFromConnector, aToConnector : TG2FileConnector): TG2SendMessage;
begin
  // Must be in the same location
  if aFromConnector.Module.Location <> aToConnector.Module.Location then
    raise Exception.Create('Modules are in different patch locations.');

  if (aFromConnector.ConnectorKind = ckOutput) and (aToConnector.ConnectorKind = ckOutput) then
    raise Exception.Create('Cannot connect an output to an output.');

  //if (aFromConnector.ConnectorKind = ckInput) and (aToConnector.ConnectorKind = ckInput) then
  //  raise Exception.Create('Cannot connect an input to an input.');

  // Create the add cable message
  Result := CreatePatchMessage;

  AddDeleteConnectionMessage( FUndoMessage,
                              aFromConnector.Module.Location,
                              aFromConnector.Module.ModuleIndex,
                              aFromConnector.ConnectorKind,
                              aFromConnector.ConnectorIndex,
                              aToConnector.Module.ModuleIndex,
                              aToConnector.ConnectorKind,
                              aToConnector.ConnectorIndex);
  AddConnectionMessage( Result, aFromConnector, aToConnector);

  // Add messages for uprate changes and cable color changes
  ResetUprateValues( aFromConnector.Module.Location);
  CheckUprateChange( Result, aFromConnector.Rate, aToConnector, aToConnector.Module);

  (G2 as TG2Mess).dump_buffer( PStaticByteBuffer(Result.Memory)^[0], Result.Size);
end;

function TG2MessPatch.CreateDeleteConnectionMessage( aCable: TG2FileCable): TG2SendMessage;
begin
  Result := CreatePatchMessage;
  AddConnectionMessage( FUndoMessage, aCable.FromConnector, aCable.ToConnector);
  AddDeleteCableMessage( Result, aCable);

  // Add messages for uprate changes and cable color changes
  ResetUprateValues( aCable.FromConnector.Module.Location);
  CheckUprateChange( Result, aCable.ToConnector.DefRate, aCable.ToConnector, aCable.ToConnector.Module);

  (G2 as TG2Mess).dump_buffer( PStaticByteBuffer(Result.Memory)^[0], Result.Size);
end;

function TG2MessPatch.CreateMoveModulesMessage: TG2SendMessage;
var Module : TG2FileModule;
    i, j : integer;
begin
  Result := CreatePatchMessage;

  for i := 0 to 1 do
    for j := 0 to ModuleCount[i] - 1 do begin
      Module := ModuleList[i].Items[j];
      if Module.Selected then begin
        AddMoveModuleMessage( FUndoMessage, Module, Module.Col, Module.Row);
        AddMoveModuleMessage( Result, Module, Module.NewCol, Module.NewRow);
      end;
    end;
end;

function TG2MessPatch.CreateAssignKnobMessage( aLocation : TLocationType; aModule, aParam, aKnob: integer): TG2SendMessage;
var i : integer;
    Knob : TKnob;
begin
  //if aKnob >= FKnobList.FKnobCount then
  if aKnob >= KnobList.Count then
    raise Exception.Create('Knob index ' + IntToStr(aKnob) + ' out of range.');

  // Check if knob is already assigned
  if KnobList.Items[ aKnob].IsAssigned = 1 then
    raise Exception.Create('Knob ' + IntToStr(aKnob) + ' already assigned.');

  Result := CreatePatchMessage;

  // Is the parameter already assigned?
  i := FindKnob( aLocation, aModule, aParam);
  if (i <> -1) then begin
    // Send a deassign message first
    AddAssignKnobMessage( FUndoMessage, aLocation, aModule, aParam, i);
    AddDeassignKnobMessage( Result, i);
  end;

  if i <> aKnob then begin
    // Is the knob already assigned to another parameter?
    Knob := GetKnob( aKnob);
    if assigned(Knob) and (Knob.IsAssigned = 1) then begin
      // Send a deassign message first
      AddAssignKnobMessage( FUndoMessage, TLocationType(Knob.Location), Knob.ModuleIndex, Knob.ParamIndex, aKnob);
      AddDeassignKnobMessage( Result, aKnob);
    end;
  end;

  AddDeassignKnobMessage( FUndoMessage, aKnob);
  AddAssignKnobMessage( Result, aLocation, aModule, aParam, aKnob);

  AddSelectParamPageMessage( Result, aKnob div 8);

  (G2 as TG2MEss).dump_buffer( PStaticByteBuffer(Result.Memory)^[0], Result.Size);
end;

function TG2MessPatch.CreateDeAssignKnobMessage( aKnob: integer): TG2SendMessage;
var Knob : TKnob;
begin
  Result := CreatePatchMessage;

  Knob := GetKnob( aKnob);
  if assigned(Knob) and (Knob.IsAssigned = 1) then begin
    AddAssignKnobMessage( FUndoMessage, TLocationType(Knob.Location), Knob.ModuleIndex, Knob.ParamIndex, aKnob);
  end;

  AddDeAssignKnobMessage( Result, aKnob);

  (G2 as TG2Mess).dump_buffer( PStaticByteBuffer(Result.Memory)^[0], Result.Size);
end;

function TG2MessPatch.CreateModuleAssignKnobs( aModule: TG2FileModule; aPageIndex: integer): TG2SendMessage;
var Knob : TKnob;
    i, KnobCount, NewKnobIndex, CurrentKnobIndex : integer;
begin
  Result := nil;
  KnobCount := aModule.AssignableKnobCount;
  if KnobCount > 0 then begin

    Result := CreatePatchMessage;

    for i := 0 to aModule.ParameterCount - 1 do begin
      NewKnobIndex := aModule.Parameter[i].DefaultKnob;
      if NewKnobIndex >= 0 then begin
        NewKnobIndex := NewKnobIndex + aPageIndex * 8;

        // Is the parameter already assigned to a knob?
        CurrentKnobIndex := FindKnob( aModule.Location, aModule.ModuleIndex, aModule.Parameter[i].ParamIndex);
        if CurrentKnobIndex <> -1 then begin
          // Send a deassign message first
          AddAssignKnobMessage( FUndoMessage, aModule.Location, aModule.ModuleIndex, aModule.Parameter[i].ParamIndex, CurrentKnobIndex);
          AddDeassignKnobMessage( Result, CurrentKnobIndex);
        end;

        // Is another parameter already assigned to this knob?
        if CurrentKnobIndex <> NewKnobIndex then begin
          Knob := GetKnob( NewKnobIndex);
          if assigned(Knob) and (Knob.IsAssigned = 1) then begin
            // Send a deassign message first
            AddAssignKnobMessage( FUndoMessage, TLocationType(Knob.Location), Knob.ModuleIndex, Knob.ParamIndex, NewKnobIndex);
            AddDeassignKnobMessage( Result, NewKnobIndex);
          end;
        end;

        AddDeassignKnobMessage( FUndoMessage, NewKnobIndex);
        AddAssignKnobMessage( Result, aModule.Location, aModule.ModuleIndex, aModule.Parameter[i].ParamIndex, NewKnobIndex);
      end;
    end;
    AddSelectParamPageMessage( Result, aPageIndex);

    (G2 as TG2MEss).dump_buffer( PStaticByteBuffer(Result.Memory)^[0], Result.Size);
  end;
end;

function TG2MessPatch.CreateModuleAssignGlobalKnobs(aModule: TG2FileModule; aPageIndex: integer): TG2SendMessage;
var Knob : TGlobalKnob;
    i, KnobCount, NewKnobIndex, CurrentKnobIndex : integer;
begin
  Result := nil;
  KnobCount := aModule.AssignableKnobCount;
  if KnobCount > 0 then begin

    Result := CreatePatchMessage;

    for i := 0 to aModule.ParameterCount - 1 do begin

      NewKnobIndex := aModule.Parameter[i].DefaultKnob;
      if NewKnobIndex >= 0 then begin

        NewKnobIndex := NewKnobIndex + aPageIndex * 8;

        // Is the parameter already assigned to a knob?
        CurrentKnobIndex := Performance.GlobalKnobList.FindGlobalKnobIndex( GetSlot.SlotIndex, aModule.Location, aModule.ModuleIndex, aModule.Parameter[i].ParamIndex);
        if CurrentKnobIndex <> -1 then begin
          // Send a deassign message first
          AddAssignGlobalKnobMessage( FUndoMessage, aModule.Location, aModule.ModuleIndex, aModule.Parameter[i].ParamIndex, CurrentKnobIndex);
          AddDeassignGlobalKnobMessage( Result, CurrentKnobIndex);
        end;

        if CurrentKnobIndex <> NewKnobIndex then begin
          Knob := Performance.GetGlobalKnob( NewKnobIndex);
          if assigned(Knob) and (Knob.IsAssigned = 1) then begin
            // Send a deassign message first
            AddAssignGlobalKnobMessage( FUndoMessage, TLocationType(Knob.Location), Knob.ModuleIndex, Knob.ParamIndex, NewKnobIndex);
            AddDeassignGlobalKnobMessage( Result, NewKnobIndex);
          end;
        end;
        AddDeassignGlobalKnobMessage( FUndoMessage, NewKnobIndex);
        AddAssignGlobalKnobMessage( Result, aModule.Location, aModule.ModuleIndex, aModule.Parameter[i].ParamIndex, NewKnobIndex);
      end;
    end;
    AddSelectGlobalParamPageMessage( Result, aPageIndex);

    (G2 as TG2MEss).dump_buffer( PStaticByteBuffer(Result.Memory)^[0], Result.Size);
  end;
end;

function TG2MessPatch.CreateAssignMidiCCMessage( aLocation: TLocationType; aModule, aParam, aMidiCC: integer): TG2SendMessage;
var Controller : TController;
begin
  Result := CreatePatchMessage;

  // Is CC already assigned?
  Controller := GetMidiCC( aMidiCC);
  if assigned(Controller) then begin
    AddAssignMidiCCMessage( FUndoMessage, TLocationType(Controller.Location), Controller.ModuleIndex, Controller.ParamIndex, Controller.MidiCC);
    AddDeassignMidiCCMessage( Result, aMidiCC);
  end;
  AddAssignMidiCCMessage( Result, aLocation, aModule, aParam, aMidiCC);
  AddDeassignMidiCCMessage( FUndoMessage, aMidiCC);

  (G2 as TG2Mess).dump_buffer( PStaticByteBuffer(Result.Memory)^[0], Result.Size);
end;

function TG2MessPatch.CreateDeassignMidiCCMessage( aMidiCC: integer): TG2SendMessage;
var Controller : TController;
begin
  Result := CreatePatchMessage;
  Controller := GetMidiCC( aMidiCC);
  if assigned(Controller) then begin
    AddAssignMidiCCMessage( FUndoMessage, TLocationType(Controller.Location), Controller.ModuleIndex, Controller.ParamIndex, Controller.MidiCC);
  end;
  AddDeassignMidiCCMessage( Result, aMidiCC);

  (G2 as TG2Mess).dump_buffer( PStaticByteBuffer(Result.Memory)^[0], Result.Size);
end;

function TG2MessPatch.CreateAssignGlobalKnobMessage( aLocation : TLocationType; aModule, aParam, aKnob: integer): TG2SendMessage;
var Perf : TG2MessPerformance;
    i    : integer;
    Knob : TGlobalKnob;
begin
  // Create the add cable message
  Result := CreatePatchMessage;

  Perf := GetPerformance;

  // Is the parameter already assigned?
  i := Perf.GlobalKnobList.FindGlobalKnobIndex( Slot.SlotIndex, aLocation, aModule, aParam);

  if (i <> -1) then begin
    // Send a deassign message first
    Knob := Perf.GetGlobalKnob(i);
    if assigned(Knob) and (Knob.IsAssigned = 1) then begin
      AddAssignGlobalKnobMessage( FUndoMessage, TLocationType(Knob.Location), Knob.ModuleIndex, Knob.ParamIndex, i);
    end;
    AddDeassignGlobalKnobMessage( Result, i);
  end;

  if i <> aKnob then begin
    // Is the knob already assigned to another parameter?
    Knob := Perf.GetGlobalKnob( aKnob);
    if assigned(Knob) and (Knob.IsAssigned = 1) then begin
      // Send a deassign message first
      AddAssignGlobalKnobMessage( FUndoMessage, TLocationType(Knob.Location), Knob.ModuleIndex, Knob.ParamIndex, aKnob);
      AddDeassignGlobalKnobMessage( Result, aKnob);
    end;
  end;

  AddAssignGlobalKnobMessage( Result, aLocation, aModule, aParam, aKnob);
  AddDeassignGlobalKnobMessage( FUndoMessage, aKnob);
  AddSelectGlobalParamPageMessage( Result, aKnob div 8);

  (G2 as TG2Mess).dump_buffer( PStaticByteBuffer(Result.Memory)^[0], Result.Size);
end;

function TG2MessPatch.CreateDeassignGlobalKnobMessage( aKnob: integer): TG2SendMessage;
var Knob : TGlobalKnob;
    Perf : TG2MessPerformance;
begin
  Result := CreatePatchMessage;

  Perf := GetPerformance;

  Knob := Perf.GetGlobalKnob( aKnob);
  if assigned(Knob) and (Knob.IsAssigned = 1) then begin
    AddAssignGlobalKnobMessage( FUndoMessage, TLocationType(Knob.Location), Knob.ModuleIndex, Knob.ParamIndex, aKnob);
  end;
  AddDeassignGlobalKnobMessage( Result, aKnob);

  (G2 as TG2Mess).dump_buffer( PStaticByteBuffer(Result.Memory)^[0], Result.Size);
end;

function TG2MessPatch.CreateSetPatchDescriptionMessage( FPatchDescription : TPatchDescription): TG2SendMessage;
begin
  Result := CreatePatchMessage;
  AddSetPatchDescriptionMessage( FUndoMessage, PatchDescription);
  AddSetPatchDescriptionMessage( Result, FPatchDescription);

  (G2 as TG2Mess).dump_buffer( PStaticByteBuffer(Result.Memory)^[0], Result.Size);
end;

function TG2MessPatch.CreateSetModuleParamLabelsMessage( aLocation: TLocationType; aModuleIndex, aParamIndex: byte; aName: AnsiString): TG2SendMessage;
begin
  Result := CreatePatchMessage;
  AddSetModuleParamLabelsMessage( FUndoMessage, aLocation, aModuleIndex, aParamIndex, GetParameterLabel(aLocation, aModuleIndex, aParamIndex));
  AddSetModuleParamLabelsMessage( Result, aLocation, aModuleIndex, aParamIndex, aName);

  (G2 as TG2Mess).dump_buffer( PStaticByteBuffer(Result.Memory)^[0], Result.Size);
end;

function TG2MessPatch.CreateSetModuleLabelMessage( aLocation: TLocationType; aModuleIndex: byte; aName: AnsiString): TG2SendMessage;
begin
  // Create the move module message
  Result := CreatePatchMessage;
  AddSetModuleLabelMessage( FUndoMessage, aLocation, aModuleIndex, GetModuleLabel( aLocation, aModuleIndex));
  AddSetModuleLabelMessage( Result, aLocation, aModuleIndex, aName);

  (G2 as TG2Mess).dump_buffer( PStaticByteBuffer(Result.Memory)^[0], Result.Size);
end;

function TG2MessPatch.CreateSetModuleColorMessage( aLocation: TLocationType; aModuleIndex, aColor : byte): TG2SendMessage;
var Module : TG2FileModule;
begin
  // Create the move module message
  Result := CreatePatchMessage;

  Module := Modules[ ord(aLocation), aModuleIndex];
  if assigned( Module) then begin
    AddSetModuleColorMessage( FUndoMessage, aLocation, aModuleIndex, Module.ModuleColor);
  end;
  AddSetModuleColorMessage( Result, aLocation, aModuleIndex, aColor);

  (G2 as TG2Mess).dump_buffer( PStaticByteBuffer(Result.Memory)^[0], Result.Size);
end;

function TG2MessPatch.ProcessMessage( MemStream: TMemoryStream): boolean;
var b, Cmd, aModuleIndex, aModuleType, aUnknown, aParamPage,
    aColor, aFromModuleIndex, aToModuleIndex, aParamIndex, aKnobIndex,
    aFromConnectorIndex, aToConnectorIndex, aVoices, aMonoPoly, aMidiCC, aSlotIndex, aLength,
    aMorphIndex, aVariation, aValue, aNegative : byte;
    aFromConnectorKind, aToConnectorKind : TConnectorKind;
    Knob : TKnob;
    aLocation : TLocationType;
    aLinkType : byte;
    aName : AnsiString;
    Slot : TG2MessSlot;
    Perf : TG2MessPerformance;
    Module : TG2FileModule;
    Cable : TG2FileCable;
    Param : TG2FileParameter;
    ParamLabelModule : TParamLabelModule;
    Controller : TController;
    GlobalKnob : TGlobalKnob;
    Chunk : TPatchChunk;
    BitReader : TBitReader;
    i : integer;
begin
  BitReader := TBitReader.Create;
  try
    Result := True; // Return True if it's a patch command

    Perf := GetPerformance;
    Slot := GetSlot;

    if assigned( FUndoMessage) then begin
      PushUndoStack( FUndoMessage);
      FUndoMessage := nil;
    end;

    repeat
      Cmd := BitReader.ReadBits( MemStream, 8);

      case Cmd of
      S_SET_UPRATE : // Uprate or downrate module
            begin
              aLocation    := TLocationType(BitReader.ReadBits( MemStream, 8));
              aModuleIndex := BitReader.ReadBits( MemStream, 8);
              Module := GetModule( ord(aLocation), aModuleIndex);

              if assigned(Module) then
                Module.Uprate := BitReader.ReadBits( MemStream, 8)
              else
                add_log_line( 'ModuleIndex ' + IntToStr( aModuleIndex) + ' not found.', LOGCMD_ERR);
            end;
      S_ADD_MODULE : // Add a module
            begin
              aModuleType         := BitReader.ReadBits( MemStream, 8);
              aLocation           := TLocationType(BitReader.ReadBits( MemStream, 8));
              aModuleIndex        := BitReader.ReadBits( MemStream, 8);
              Module := CreateModule( aLocation, aModuleIndex, aModuleType);
              Module.Col          := BitReader.ReadBits( MemStream, 8);
              Module.Row          := BitReader.ReadBits( MemStream, 8);
              Module.ModuleColor  := BitReader.ReadBits( MemStream, 8);
              Module.Uprate       := BitReader.ReadBits( MemStream, 8);
              Module.IsLed        := BitReader.ReadBits( MemStream, 8);
              for i := 0 to Module.ModeCount - 1 do
                Module.ModeInfo[i] := BitReader.ReadBits( MemStream, 8);
              Module.ModuleName := '';
              b := BitReader.ReadBits( MemStream, 8);
              while (b <> 0) and (Length(Module.ModuleName)<16) do begin
                Module.ModuleName := Module.ModuleName + AnsiChar(b);
                b := BitReader.ReadBits( MemStream, 8);
              end;
              //Module.Selected := True;
              AddModuleToPatch( aLocation, Module);

              // Must be called AFTER moduleindex is known
              SortLeds;

              if assigned(G2) and assigned(G2.OnAddModule) then
                G2.OnAddModule(self, G2.ID, Module);
            end;
      S_SET_MODULE_COLOR : // Change module color
            begin
              aLocation     := TLocationType(BitReader.ReadBits( MemStream, 8));
              aModuleIndex  := BitReader.ReadBits( MemStream, 8);
              aColor        := BitReader.ReadBits( MemStream, 8);
              Module := GetModule( ord(aLocation), aModuleIndex);

              if assigned( Module) then begin
                Module.ModuleColor := aColor
              end else
                add_log_line( 'ModuleIndex ' + IntToStr( aModuleIndex) + ' not found.', LOGCMD_ERR);
            end;
      S_DEL_MODULE : // Delete module
            begin
              aLocation     := TLocationType(BitReader.ReadBits( MemStream, 8));
              aModuleIndex  := BitReader.ReadBits( MemStream, 8);
              Module := GetModule( ord(aLocation), aModuleIndex);

              if assigned( Module) then begin
                RemoveFromLedList( aLocation, aModuleIndex);
                DeleteModuleFromPatch( aLocation, Module);

                //if assigned(Perf) then begin
                //  Perf.DeleteModuleFromPerf( Slot.SlotIndex, aLocation, Module);
                //end;

                if assigned(G2) and assigned(G2.OnDeleteModule) then
                  G2.OnDeleteModule(self, G2.ID, aLocation, aModuleIndex);

                if assigned(Slot) and assigned(Slot.FOnDeleteModule) then
                  Slot.FOnDeleteModule(self, G2.ID, aLocation, aModuleIndex);
              end else
                add_log_line( 'ModuleIndex ' + IntToStr( aModuleIndex) + ' not found.', LOGCMD_ERR);
            end;
      S_MOV_MODULE : // Move module
            begin
              aLocation     := TLocationType(BitReader.ReadBits( MemStream, 8));
              aModuleIndex  := BitReader.ReadBits( MemStream, 8);
              Module := GetModule( ord(aLocation), aModuleIndex);

              if assigned( Module) then begin
                Module.Col := BitReader.ReadBits( MemStream, 8);
                Module.Row := BitReader.ReadBits( MemStream, 8);
              end else
                add_log_line( 'ModuleIndex ' + IntToStr( aModuleIndex) + ' not found.', LOGCMD_ERR);
            end;
      S_ADD_CABLE : // Add a cable
            begin
              aUnknown            := BitReader.ReadBits( MemStream, 4);
              aLocation           := TLocationType(BitReader.ReadBits( MemStream, 1));
              aColor              := BitReader.ReadBits( MemStream, 3);
              aFromModuleIndex    := BitReader.ReadBits( MemStream, 8);
              aFromConnectorKind  := TConnectorKind(BitReader.ReadBits( MemStream, 2));
              aFromConnectorIndex := BitReader.ReadBits( MemStream, 6);
              aToModuleIndex      := BitReader.ReadBits( MemStream, 8);
              aToConnectorKind    := TConnectorKind(BitReader.ReadBits( MemStream, 2));
              aToConnectorIndex   := BitReader.ReadBits( MemStream, 6);
              if FindCable(aLocation, aFromModuleIndex, aFromConnectorIndex, aToModuleIndex, aToConnectorIndex) = nil then begin
                if aFromConnectorKind = ckInput then
                  aLinkType := 0
                else
                  aLinkType := 1;

                Cable := CreateCable( aLocation,
                                      aColor,
                                      aFromModuleIndex,
                                      aFromConnectorIndex,
                                      aLinkType,
                                      aToModuleIndex,
                                      aToConnectorIndex) as TG2FileCable;
                 AddCableToPatch( aLocation, Cable);

              end else
                add_log_line( 'Cable already exists.', LOGCMD_ERR);
            end;
      S_DEL_CABLE : // Delete a cable
            begin
              aUnknown            := BitReader.ReadBits( MemStream, 7);
              aLocation           := TLocationType(BitReader.ReadBits( MemStream, 1));
              aFromModuleIndex    := BitReader.ReadBits( MemStream, 8);
              aFromConnectorKind  := TConnectorKind(BitReader.ReadBits( MemStream, 2));
              aFromConnectorIndex := BitReader.ReadBits( MemStream, 6);
              aToModuleIndex      := BitReader.ReadBits( MemStream, 8);
              aToConnectorKind    := TConnectorKind(BitReader.ReadBits( MemStream, 2));
              aToConnectorIndex   := BitReader.ReadBits( MemStream, 6);
              Cable := FindCable( aLocation, aFromModuleIndex, aFromConnectorIndex, aToModuleIndex, aToConnectorIndex);
              if assigned(Cable) then begin

                DeleteCableFromPatch( aLocation, Cable);
             end else begin
                // Try the other way around...
                Cable := FindCable( aLocation, aToModuleIndex, aToConnectorIndex, aFromModuleIndex, aFromConnectorIndex);
                if assigned(Cable) then begin
                  DeleteCableFromPatch( aLocation, Cable)
                end else begin
                  add_log_line( ' Delete cable : cable in location ' + IntToStr(ord(aLocation))
                                       + ' from module ' + IntToStr(aFromModuleIndex)
                                       + ' connector ' + IntToStr(aFromConnectorIndex)
                                       + ' to module ' + IntToStr(aToModuleIndex)
                                       + ' connector ' + IntToStr(aToConnectorIndex)
                                       + ' not found.', LOGCMD_ERR);
                end;
              end;
            end;
      S_CABLE_COLOR : // Change cable color
            begin
              aUnknown            := BitReader.ReadBits( MemStream, 4);
              aLocation           := TLocationType(BitReader.ReadBits( MemStream, 1));
              aColor              := BitReader.ReadBits( MemStream, 3);
              aFromModuleIndex    := BitReader.ReadBits( MemStream, 8);
              aFromConnectorKind  := TConnectorKind(BitReader.ReadBits( MemStream, 2));
              aFromConnectorIndex := BitReader.ReadBits( MemStream, 6);
              aToModuleIndex      := BitReader.ReadBits( MemStream, 8);
              aToConnectorKind    := TConnectorKind(BitReader.ReadBits( MemStream, 2));
              aToConnectorIndex   := BitReader.ReadBits( MemStream, 6);
              Cable := FindCable( aLocation, aFromModuleIndex, aFromConnectorIndex, aToModuleIndex, aToConnectorIndex);
              if assigned(Cable) then begin
                Cable.CableColor := aColor;
              end else begin
                Cable := FindCable( aLocation, aToModuleIndex, aToConnectorIndex, aFromModuleIndex, aFromConnectorIndex);
                if assigned(Cable) then begin
                  Cable.CableColor := aColor;
                end else
                  add_log_line( DateTimeToStr(now) + ' Cable color : cable in location ' + IntToStr(integer(aLocation))
                                       + ' from module ' + IntToStr(aFromModuleIndex)
                                       + ' connector ' + IntToStr(aFromConnectorIndex)
                                       + ' to module ' + IntToStr(aToModuleIndex)
                                       + ' connector ' + IntToStr(aToConnectorIndex)
                                       + ' not found.', LOGCMD_ERR);
              end
            end;
      S_ASSIGN_KNOB : // Assign knob
            begin
              aModuleIndex   := BitReader.ReadBits( MemStream, 8);
              aParamIndex    := BitReader.ReadBits( MemStream, 8);
              aLocation      := TLocationType(BitReader.ReadBits( MemStream, 2));
              aUnknown       := BitReader.ReadBits( MemStream, 6);
              aUnknown       := BitReader.ReadBits( MemStream, 8);
              aKnobIndex     := BitReader.ReadBits( MemStream, 8);

              if aLocation = ltPatch then begin
                Parameter[ aModuleIndex, aParamIndex].AssignKnob( aKnobIndex);
              end else begin
                Module := GetModule( ord(aLocation), aModuleIndex);
                if assigned( Module) then begin
                  Module.Parameter[ aParamIndex].AssignKnob( aKnobIndex);
                end else
                  add_log_line( 'ModuleIndex ' + IntToStr( aModuleIndex) + ' not found.', LOGCMD_ERR);
              end;

              if assigned((G2).OnAssignKnob) then
                G2.OnAssignKnob(self, G2.ID, Slot.SlotIndex, aKnobIndex);
            end;
      S_DEASSIGN_KNOB : // Deassign knob
            begin
              aUnknown       := BitReader.ReadBits( MemStream, 8);
              aKnobIndex     := BitReader.ReadBits( MemStream, 8);

              Knob := KnobList.Items[ aKnobIndex];
              if Knob.IsAssigned = 1 then begin
                if Knob.Location = TBits2(ltPatch) then begin
                  Param := Parameter[ Knob.ModuleIndex, Knob.ParamIndex];
                  if assigned(Param) then
                    Param.DeassignKnob( aKnobIndex);
                end else begin
                  Module := GetModule( Knob.Location, Knob.ModuleIndex);
                  if assigned( Module) then begin
                    Module.Parameter[ Knob.ParamIndex].DeassignKnob( aKnobIndex);
                  end;
                end;
              end;

              if assigned(G2.OnDeassignKnob) then
                G2.OnDeassignKnob(self, G2.ID, Slot.SlotIndex, aKnobIndex);
            end;
      S_ASSIGN_MIDICC :
            begin
              aLocation      := TLocationType(BitReader.ReadBits( MemStream, 8));
              aModuleIndex   := BitReader.ReadBits( MemStream, 8);
              aParamIndex    := BitReader.ReadBits( MemStream, 8);
              aMidiCC        := BitReader.ReadBits( MemStream, 8);

              if aLocation = ltPatch then begin
                Parameter[ aModuleIndex, aParamIndex].AssignController( aMidiCC);
              end else begin
                Module := GetModule( ord(aLocation), aModuleIndex);
                if assigned( Module) then begin
                  Module.Parameter[ aParamIndex].AssignController( aMidiCC);
                end else
                  add_log_line( 'ModuleIndex ' + IntToStr( aModuleIndex) + ' not found.', LOGCMD_ERR);
              end;

              //AssignMidiCCInPatch( aMidiCC, TLocationType(aLocation), aModuleIndex, aParamIndex);
            end;
      S_DEASSIGN_MIDICC :
            begin
              aMidiCC        := BitReader.ReadBits( MemStream, 8);

              Controller := GetMidiCC( aMidiCC);
              if assigned(Controller) then begin
                if Controller.Location = TBits2(ltPatch) then begin
                  Param := Parameter[ Controller.ModuleIndex, Controller.ParamIndex];
                  if assigned(Param) then
                    Param.DeassignController;
                end else begin
                  Module := GetModule( Controller.Location, Controller.ModuleIndex);
                  if assigned( Module) then begin
                    Module.Parameter[ Controller.ParamIndex].DeassignController;
                  end;
                end;
                //DeassignMidiCCInPatch( aMidiCC);
              end;
            end;
      S_SEL_PARAM_PAGE : // Select parameter page
            begin
              aParamPage     := BitReader.ReadBits( MemStream, 8);

              if assigned(G2) and assigned((G2 as TG2Mess).FOnSelectParamPage) then
                (G2 as TG2Mess).FOnSelectParamPage(self, G2.ID, aParamPage);
            end;
      S_ASS_GLOBAL_KNOB : // Assign knob in global page
            begin
              aSlotIndex     := BitReader.ReadBits( MemStream, 4);
              aLocation      := TLocationType(BitReader.ReadBits( MemStream, 2));
              aUnknown       := BitReader.ReadBits( MemStream, 2);
              aModuleIndex   := BitReader.ReadBits( MemStream, 8);
              aParamIndex    := BitReader.ReadBits( MemStream, 8);
              aUnknown       := BitReader.ReadBits( MemStream, 8);
              aKnobIndex     := BitReader.ReadBits( MemStream, 8);

              if aLocation = ltPatch then begin
                Parameter[ aModuleIndex, aParamIndex].AssignGlobalKnob( Perf, aSlotIndex, aKnobIndex);
              end else begin
                Module := GetModule( ord(aLocation), aModuleIndex);
                if assigned( Module) then begin
                  Module.Parameter[ aParamIndex].AssignGlobalKnob( Perf, aSlotIndex, aKnobIndex);
                end else
                  add_log_line( 'ModuleIndex ' + IntToStr( aModuleIndex) + ' not found.', LOGCMD_ERR);
              end;

              if assigned(G2.OnAssignGlobalKnob) then
                G2.OnAssignGlobalKnob(self, G2.ID, aKnobIndex);
            end;
      S_DEASS_GLOB_KNOB : // Deassign global knob
            begin
              aUnknown       := BitReader.ReadBits( MemStream, 8);
              aKnobIndex     := BitReader.ReadBits( MemStream, 8);

              GlobalKnob := Perf.GlobalKnobList.Items[ aKnobIndex];

              if GlobalKnob.IsAssigned = 1 then begin
                if GlobalKnob.Location = TBits2(ltPatch) then begin
                  Param := Parameter[ GlobalKnob.ModuleIndex, GlobalKnob.ParamIndex];
                  if assigned(Param) then
                    Param.DeassignGlobalKnob(Perf, aKnobIndex);
                end else begin
                  Module := GetModule( GlobalKnob.Location, GlobalKnob.ModuleIndex);
                  if assigned( Module) then begin
                    Module.Parameter[ GlobalKnob.ParamIndex].DeassignGlobalKnob( Perf, aKnobIndex);
                  end;
                end;
              end;

              if assigned(G2.OnDeassignGlobalKnob) then
                G2.OnDeassignGlobalKnob(self, G2.ID, aKnobIndex);
            end;
      S_SEL_GLOBAL_PAGE : // Select global parameter page
            begin
              // TODO
            end;
      S_SET_PARAM_LABEL :
            begin
              ParamLabelModule := TParamLabelModule.Create( True);
              aLocation        := TLocationType(BitReader.ReadBits( MemStream, 8));
              // A bit dirty
              aModuleIndex     := BitReader.ReadBits( MemStream, 8);
              aLength          := BitReader.ReadBits( MemStream, 8);
              MemStream.Position := MemStream.Position - 2;

              Chunk := TPatchChunk.Create( MemStream);
              try
                Chunk.ReadBuffer( aLength + 2);
                ParamLabelModule.Read(Chunk);
                for i := 0 to ParamLabelModule.Count - 1 do begin
                  SetParameterLabel( aLocation,
                                     ParamLabelModule.ModuleIndex,
                                     ParamLabelModule.Items[i].ParamIndex,
                                     ParamLabelModule.Items[i].Items[0].ParamLabel); // TODO : Could be more than 1 label per parameterindex
                end;
              finally
                Chunk.Free;
                ParamLabelModule.Free;
              end;
            end;
      S_SET_MODULE_LABEL :
            begin
              aLocation    := TLocationType(BitReader.ReadBits( MemStream, 8));
              aModuleIndex := BitReader.ReadBits( MemStream, 8);
              aName := ReadClaviaString(MemStream);
              SetModuleLabel( aLocation, aModuleIndex, aName);
            end;
      S_SET_MORPH_RANGE :
            begin
              aLocation := TLocationType(BitReader.ReadBits( MemStream, 8));
              aModuleIndex := BitReader.ReadBits( MemStream, 8);
              aParamIndex := BitReader.ReadBits( MemStream, 8);
              aMorphIndex := BitReader.ReadBits( MemStream, 8);
              aValue := BitReader.ReadBits( MemStream, 8);
              aNegative := BitReader.ReadBits( MemStream, 8);
              aVariation := BitReader.ReadBits( MemStream, 8);
              if aNegative = 1 then
                SetMorphValue( aLocation, aModuleIndex, aParamIndex, aMorphIndex, aValue - 256, aVariation)
              else
                SetMorphValue( aLocation, aModuleIndex, aParamIndex, aMorphIndex, aValue, aVariation);
            end;
      C_PATCH_DESCR,
      C_MODULE_LIST,
      C_CURRENT_NOTE_2,
      C_CABLE_LIST,
      C_PARAM_LIST,
      C_MORPH_PARAM,
      C_KNOBS,
      C_CONTROLLERS,
      C_PARAM_NAMES,
      C_MODULE_NAMES,
      C_PATCH_NOTES :
            begin // Process chunks in add module message

              MemStream.Position := MemStream.Position - 1;
              Chunk := TPatchChunk.Create( MemStream);
              try
                //Chunk.FLogLines := FG2USB.LogLines.Lines;
                Chunk.ReadChunk;
                Result := ReadChunk( Chunk);
              finally
                Chunk.Free;
              end;

              if assigned(G2) and assigned(Slot) then
                if assigned((G2 as TG2Mess).FOnPatchUpdate) then
                  (G2 as TG2Mess).FOnPatchUpdate( self, G2.ID,  Slot.SlotIndex);
            end;
       else
         Result := False; // not a patch command
      end;
    until (Result = False) or (MemStream.Position >= MemStream.Size - 2);
  finally
    BitReader.Free;
  end;
end;


function TG2MessPatch.GetLedListCount: integer;
begin
  Result := 0;
  // Abstract
end;

function TG2MessPatch.GetMiniVUListCount: integer;
begin
  Result := 0;
  // Abstract
end;

procedure TG2MessPatch.SetLedLevel(Index: integer; aValue: byte);
begin
  // Abstract
end;

procedure TG2MessPatch.SetMiniVULevel(Index: integer; aValue: byte);
begin
  // Abstract
end;

procedure TG2MessPatch.RemoveFromLedList( aLocation: TLocationType; aModuleIndex : integer);
begin
  // Abstract
end;

function TG2MessPatch.FindCable( Location : TLocationType; FromModule : byte; FromConnector : byte; ToModule : byte; ToConnector : byte): TG2FileCable;
begin
  Result := CableList[ ord(Location)].FindCable( FromModule, FromConnector, ToModule, ToConnector) as TG2FileCable;
end;

end.
