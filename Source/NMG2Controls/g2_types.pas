unit g2_types;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

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
//  This unit contains the G2 constants and types and some utility functions.
//
//  ////////////////////////////////////////////////////////////////////////////

interface
uses
  SysUtils,
  Classes,
  StdCtrls,
  math;

const
  VERSION = '0.21';

  NMORPHS = 8;
  NVARIATIONS = 9;
  NSLOTS = 4;

  LOG_PATH = 'C:\Users\Bruno\Documents\Ableton\VST\';

  VENDOR_ID  = $ffc; // clavia, g2
  PRODUCT_ID = 2;

  PATCH_DATA = $00;
  PERF_DATA  = $01;

  LOCATION_FX = 0;
  LOCATION_VA = 1;
  LOCATION_PATCH_SETTINGS = 2;

  CL_CONTROL_HIGHLIGHT   = $00FFFF00;
  CL_SELECTED_PATCH      = $00B6B6B6;
  CL_SELECTED_PARAM      = $00FFFFFF;
  CL_KNOB_MORPH          = $00F4948C;
  CL_KNOB_MORPH_SELECTED = $008582F4;
  CL_DISPLAY_BACKGRND    = $00555555;
  CL_BTN_FACE            = $00D0D0D0;
  CL_BTN_BORDER          = $00404040;
  CL_CLAVIA_RED          = $005A5692;//$006550DC;//$001620D1;
  CL_CLAVIA_BLUE         = $00531F00;

  // Module dimensions
  UNITS_COL = 255;
  UNITS_ROW = 15;

  COLOR_RED    = 0;
  COLOR_BLUE   = 1;
  COLOR_YELLOW = 2;
  COLOR_ORANGE = 3;
  COLOR_GREEN  = 4;
  COLOR_PURPLE = 5;
  COLOR_WHITE  = 6;

  // Cable simulation parameters
  CABLE_CONTROL_MARGIN = 6;
  NCABLE_ELEMENTS      = 16;
  CABLE_THICKNESS      = 2;
  TENSION              = 10;
	DAMP                 = 0.8;
	GRAVITY              = 0.04;

  MAX_BULK_DATA_SIZE      = 4096;
  MAX_INTERRUPT_DATA_SIZE = 4096;

  LOGCMD_NUL = $00;
  LOGCMD_HDR = $01;
  LOGCMD_ERR = $02;
  LOGCMD_TAB = $03;

  CMD_SLOT = $08;
  CMD_SYS  = $0c;
  CMD_INIT = $80;
  CMD_CUST = $0B; // Some custom message i use between server and client

  CMD_REQ  = $20;
  CMD_RESP = $00;
  CMD_NO_RESP = $30;


const
  Q_SYNTH_SETTINGS        = $02;
  S_SYNTH_SETTINGS        = $03;
  R_MIDI_CC               = $04;
  S_SEL_SLOT              = $09;
  S_LOAD                  = $0a;
  S_SAVE                  = $0b;
  S_CLEAR                 = $0c;
  Q_PERF_SETTINGS         = $10;
  C_PERF_SETTINGS         = $11;
  R_LIST_NAMES            = $13;
  Q_LIST_NAMES            = $14;
  S_ASS_GLOBAL_KNOB       = $1c;
  S_DEASS_GLOB_KNOB       = $1d;
  S_SEL_GLOBAL_PAGE       = $1e;
  C_PATCH_DESCR           = $21;
  S_ASSIGN_MIDICC         = $22;
  S_DEASSIGN_MIDICC       = $23;
  S_ASSIGN_KNOB           = $25;
  S_DEASSIGN_KNOB         = $26;
  R_PATCH_NAME            = $27;
  Q_PATCH_NAME            = $28;
  C_PERF_NAME             = $29;
  S_SET_UPRATE            = $2a;
  S_SET_MODE              = $2b;
  S_SEL_PARAM_PAGE        = $2d;
  Q_SELECTED_PARAM        = $2e;
  S_SEL_PARAM             = $2f;
  S_ADD_MODULE            = $30;
  S_SET_MODULE_COLOR      = $31;
  S_DEL_MODULE            = $32;
  S_SET_MODULE_LABEL      = $33;
  S_MOV_MODULE            = $34;
  Q_VERSION_CNT           = $35;
  S_SET_PATCH             = $37;
  R_LED_DATA              = $39;
  R_VOLUME_DATA           = $3a;
  Q_PATCH                 = $3c;
  S_MIDI_DUMP             = $3d;
  S_SET_PARAM_MODE        = $3e;
  S_SET_PARAM             = $40;
  S_SET_PARAM_LABEL       = $42;
  S_SET_MORPH_RANGE       = $43;
  S_COPY_VARIATION        = $44;
  C_MODULE_LIST           = $4a;
  C_PARAM_LIST            = $4d;
  S_ADD_CABLE             = $50;
  S_DEL_CABLE             = $51;
  C_CABLE_LIST            = $52;
  S_CABLE_COLOR           = $54;
  S_CTRL_SNAPSHOT         = $55;
  S_PLAY_NOTE             = $56;
  M_UNKNOWN_2             = $59;
  C_MODULE_NAMES          = $5a;
  C_PARAM_NAMES           = $5b;
  Q_GLOBAL_KNOBS          = $5e;
  C_KNOBS_GLOBAL          = $5f;
  C_CONTROLLERS           = $60;
  C_KNOBS                 = $62;
  C_MORPH_PARAM           = $65;
  Q_CURRENT_NOTE          = $68;
  C_CURRENT_NOTE_2        = $69;
  S_SEL_VARIATION         = $6a;
  Q_PATCH_TEXT            = $6e;
  C_PATCH_NOTES           = $6f;
  M_UNKNOWN_6             = $70;
  Q_RESOURCES_USED        = $71;
  R_RESOURCES_USED        = $72;
  S_START_STOP_COM        = $7d;
  R_ERROR                 = $7e;
  R_OK                    = $7f;
  M_UNKNOWN_1             = $81;

  START_COMM              = $00;
  STOP_COMM               = $01;

  PATCH_ENV               = $04;
    ENV_ATTACK            = $01;
    ENV_DECAY             = $02;
    ENV_SUSTAIN           = $03;
    ENV_RELEASE           = $04;

  PATCH_MORPH             = $01;
  PATCH_VOLUME            = $02;
    VOLUME_LEVEL          = $00;
    VOLUME_MUTE           = $01;
  PATCH_GLIDE             = $03;
    GLIDE_TYPE            = $00;
    GLIDE_SPEED           = $01;
  PATCH_BEND              = $04;
    BEND_ON_OFF           = $00;
    BEND_RANGE            = $01;
  PATCH_VIBRATO           = $05;
    VIBRATO_MOD           = $00;
    VIBRATO_DEPTH         = $01;
    VIBRATO_RATE          = $02;
  PATCH_ARPEGGIATOR       = $06;
    ARP_ON_OFF            = $00;
    ARP_SPEED             = $01;
    ARP_DIRECTION         = $02;
    ARP_OCTAVES           = $03;
  PATCH_SUSTAIN           = $07;
    SUSTAIN_PEDAL         = $01;
    OCTAVE_SHIFT          = $00;

  STD_MORPH_NAMES : array[0..7] of AnsiString = ('Wheel',
                                                 'Vel',
                                                 'Keyb',
                                                 'Aft.Tch',
                                                 'Sust.Pd',
                                                 'Ctrl.Pd',
                                                 'P.Stick',
                                                 'G.Wh 2');

  PARAM_PAGE_NAMES : array[0..4] of string = ('Osc',
                                              'LFO',
                                              'Env',
                                              'Filter',
                                              'Effect');

  CATEGORIES : array[0..15] of string = ('None',
                                         'Acoustic',
                                         'Sequencer',
                                         'Bass',
                                         'Classic',
                                         'Drum',
                                         'Fantasy',
                                         'FX',
                                         'Lead',
                                         'Organ',
                                         'Pad',
                                         'Piano',
                                         'Synth',
                                         'Audio in',
                                         'User1',
                                         'User2');

  MaxPixelCount = 32768;

type
  TMessageDataType = (mdtResponseMessage = 0, mdtSendMessage = 1);
  TParamType = ( ptParam, ptMode);
  TKnobType = ( ktBig, ktMedium, ktSmall, ktExtraSmall, ktReset, ktResetMedium, ktSlider, ktSeqSlider);
  TButtonTextType = (bttNormal, bttPush, bttCheck);
  TOrientationType = ( otHorizontal, otVertical);
  TIconType = (itNone, itUp, itDown, itLeft, itRight, itCheck, itSine, itSaw, itPulse, itTri, itPulse25, itPulse10);
  TBandwidthType = (btStatic, btDynamic);
  TConnectorType = (ctAudio, ctLogic, ctControl);
  TConnectorKind = (ckInput, ckOutput);
  TLedType = (ltSequencer, ltGreen, ltMiniVU);
  TLocationType = (ltFX = 0, ltVA = 1, ltPatch = 2);
  TClientType = (ctEditor, ctVST);

  TByteBuffer = packed array of byte;

  TStaticByteBuffer = packed array[0..MAX_BULK_DATA_SIZE-1] of byte;
  PStaticByteBuffer = ^TStaticByteBuffer;


  TBits16 = word;
  TBits14 = word;
  TBits12 = word;
  TBits10 = word;
  TBits8 = byte;
  TBits7 = byte;
  TBits6 = byte;
  TBits5 = byte;
  TBits4 = byte;
  TBits3 = byte;
  TBits2 = byte;
  TBits1 = byte;

  TEndToken = set of AnsiChar;

  TBitReader = class
    FReadBitPointer : integer;
    FBitBuffer      : byte;
    constructor Create;
    function    ReadBits(aStream : TStream; NoOffBits : byte): Cardinal;
  end;

  TBitWriter = class
    FWriteBitPointer : integer;
    FBitBuffer       : byte;
    constructor Create;
    procedure   WriteBits(aStream : TStream; Value : Cardinal; NoOffBits : byte);
    function    GetWriteBufferBitsLeft : integer;
  end;

  TPatchChunk = class
    FStream      : TStream;
    FId          : byte;
    FSize        : word;
    FLogLines    : TStrings;
    FBitReader   : TBitReader;
    FBitWriter   : TBitWriter;
    FWriteBuffer : TMemoryStream;
    FReadBuffer  : TMemoryStream;
    FWriteCrc    : Word;
    FReadCrc     : Word;
    constructor Create( aStream : TStream);
    destructor  Destroy; override;
    function    PeekID: integer;
    procedure   ReadChunk;
    procedure   ReadBuffer(aSize : integer);
    function    ReadName: AnsiString;
    function    ReadBits(NoOffBits : byte): Cardinal;
    function    GetReadBufferBitsLeft: integer;
    procedure   WriteChunk(aId : byte);
    procedure   WriteBits(Value : Cardinal; NoOffBits : byte);
    procedure   WriteName(Value : AnsiString);
    procedure   WriteCrc(aStream : TStream);
    procedure   Flush;
    procedure   DumpChunkData(aStream : TStream);
  end;

  TModuleDefStream = class( TMemoryStream)
    constructor Create( Filename : string);
    destructor  Destroy; override;
    function    GetNextString( var source : AnsiString; EndTokens : TEndToken): AnsiString;
    function    GetNextByte( var source : AnsiString; EndTokens : TEndToken): Byte;
    function    GetNextInteger( var source : AnsiString; EndTokens : TEndToken): Integer;
    procedure   ReadSpaces;
    function    ReadUntil( EndTokens : TEndToken): AnsiString;
    function    ReadConst( value : AnsiString): boolean;
    function    ReadOptions( sl : TStrings; ListTokens, EndTokens : TEndToken): integer;
    function    PeekValue( Name : AnsiString; ValueEndTokens, EndTokens : TEndToken): AnsiString;
    function    UnQuote( Value : AnsiString): AnsiString;
  end;

function  CrcClavia( Seed: Integer; aVal: Integer): Word;
function  BoolToByte( Value : boolean): byte;
function  BoolToInt( value : boolean): integer;
function  BoolToStr( value : boolean): string;
function  HexToByte( c : char): byte;
function  IntToByte( i : integer): byte;
function  ByteToInt( b : byte): integer;
function  max( v1, v2 : integer): integer;
function  min( v1, v2 : integer): integer;
function  ConvertFwSlahsToBwSlash( filename : string): string;
function  ConvertBwSlahsToFwSlash( filename : string): string;

{$IFNDEF G2_VST}
function  InRange(value: string; min, max: integer): integer;
{$ENDIF}

var
  ModuleColors : array[0..24] of integer = ($00C0C0C0,
                                            $00BABACC, // 1
                                            $00BACCBA, // 2
                                            $00CCBAB0, // 3
                                            $00AACBD0, // 4
                                            $00D4A074, // 5
                                            $007A77E5, // 6 R
                                            $00BDC17B, // 7
                                            $0080B982, // 8
                                            $0048D1E7, // 9
                                            $0062D193, // 10
                                            $007DC7DE, // 11
                                            $00C29A8F, // 12
                                            $00817DBA, // 13
                                            $008D8DCA, // 14
                                            $00A5D1DE, // 15
                                            $009CCF94, // 16
                                            $00C7D669, // 17
                                            $00C8D2A0, // 18
                                            $00D2D2BE, // 19
                                            $00C08C80, // 20
                                            $00C773D6, // 21
                                            $00BE82BE, // 22
                                            $00D2A0CD, // 23
                                            $00D2BED2);// 24

  CableColors : array[0..6] of integer = ($005A5AFF,
                                          $00FF6464,
                                          $0050E6E6,
                                          $0050C0FF,
                                          $0050D250,
                                          $00E600C8,
                                          $00FFFFFF);

implementation

function ConvertFwSlahsToBwSlash( filename : string): string;
var i : integer;
begin
  Result := filename;
  i := pos('/', Result);
  while i > 0 do begin
    Result[i] := '\';
    i := Pos('/', Result);
  end;
end;

function ConvertBwSlahsToFwSlash( filename : string): string;
var i : integer;
begin
  Result := filename;
  i := pos('\', Result);
  while i > 0 do begin
    Result[i] := '/';
    i := Pos('\', Result);
  end;
end;

function CrcClavia( Seed: Integer; aVal: Integer): Word;
var
   i    : Integer;
   aCrc : Integer;
   k    : Integer;
begin
   k    := ((( Seed shr 8) xor aVal) and 255) shl 8;
   aCrc := 0;
   for i := 1 to 8
   do begin
     if ( aCrc xor k) and $8000 <> 0
     then aCrc := ( aCrc shl 1) xor $1021
     else aCrc := aCrc shl 1;
     k := k shl 1;
   end;
   Result := (( Seed shl 8) xor aCrc) and $ffff;
end;

function HexToByte( c : char): byte;
begin
  if c >= 'a' then
    Result := 10 + ord(c) - ord('a')
  else
    Result := ord(c) - ord('0');
end;

function BoolToByte( Value : boolean): byte;
begin
  If Value then
    Result := 1
  else
    Result := 0;
end;

function BoolToInt(value : boolean): integer;
begin
  if value then
    Result := 1
  else
    Result := 0;
end;

function BoolToStr( value : boolean): string;
begin
  if Value then
    Result := 'True'
  else
    Result := 'False';
end;

function IntToByte( i : integer): byte;
begin
  if i < 0 then
    Result := 256 + i
  else
    Result := i;
end;

function ByteToInt( b : byte): integer;
begin
  if b > 127 then
    Result := b - 256
  else
    Result := b;
end;

function InRange(value: string; min, max: integer): integer;
var Code : integer;
begin
  val(Value, Result, Code);
  if Code <> 0 then
    raise Exception.Create('Number required.');

  if (Result < min) or (Result > max)  then
    raise Exception.Create('Value out of range (' + IntToStr(min) + '..' + IntTostr(max) + ').');
end;

function max( v1, v2 : integer): integer;
begin
  if v1 > v2 then
    Result := v1
  else
    Result := v2;
end;

function min( v1, v2 : integer): integer;
begin
  if v1 < v2 then
    Result := v1
  else
    Result := v2;
end;

// ==== TModuleDefStream =======================================================


constructor TModuleDefStream.Create( FileName : string);
begin
  inherited Create;
  LoadFromFile( FileName);
end;

destructor TModuleDefStream.Destroy;
begin
  inherited;
end;

function TModuleDefStream.GetNextString( var source : AnsiString; EndTokens : TEndToken): AnsiString;
begin
  Result := '';
  while (length(source) > 0) and not(source[1] in EndTokens) do begin
    Result := Result + Source[1];
    source := copy(source, 2, Length(source)-1);
  end;
  if (length(source) > 0) then
    source := copy(source, 2, Length(source)-1);
end;

function TModuleDefStream.GetNextByte( var source : AnsiString; EndTokens : TEndToken): Byte;
var value : AnsiString;
begin
  value := '';
  while (length(source) > 0) and not(source[1] in EndTokens) do begin
    value := value + source[1];
    source := copy(source, 2, Length(source)-1);
  end;
  if (length(source) > 0) then
    source := copy(source, 2, Length(source)-1);
  Result := StrToInt(string(value));
end;

function TModuleDefStream.GetNextInteger( var source : AnsiString; EndTokens : TEndToken): Integer;
var value : AnsiString;
begin
  value := '';
  while (length(source) > 0) and not(source[1] in EndTokens) do begin
    value := value + source[1];
    source := copy(source, 2, Length(source)-1);
  end;
  if (length(source) > 0) then
    source := copy(source, 2, Length(source)-1);
  Result := StrToInt(string(value));
end;

procedure TModuleDefStream.ReadSpaces;
var c : AnsiChar;
begin
  while (Position < Size) and (Read( c, 1) = 1) and (c in [' ', #10, #13]) do begin
  end;
  if not(c in [' ', #10, #13]) then
    Position := Position - 1;
end;

function TModuleDefStream.ReadUntil( EndTokens : TEndToken): AnsiString;
var c : AnsiChar;
begin
  Result := '';
  while ( Position < Size) and ( Read( c, 1) = 1) and not(c in EndTokens) do begin
    Result := Result + c;
  end;
end;

function TModuleDefStream.ReadConst( value : AnsiString): boolean;
var c : AnsiChar;
    i : integer;
begin
  Result := false;
  i := 1;
  while ( Position < Size) and (i<=Length(Value)) and ( Read( c, 1) = 1) and (value[i] = c) do begin
    inc(i);
  end;
  Result := True;
end;

function TModuleDefStream.ReadOptions( sl : TStrings; ListTokens, EndTokens : TEndToken): integer;
var c : AnsiChar;
    s : AnsiString;
begin
  while ( Position < Size) and ( Read( c, 1) = 1) and not(c in EndTokens) do begin
    if (c in ListTokens) then begin
      sl.Add(string(s));
      s := '';
    end else
      s := s + c;
  end;
  if s <> '' then
    sl.Add(string(s));
  Result := sl.Count;
end;

function TModuleDefStream.PeekValue( Name : AnsiString; ValueEndTokens, EndTokens : TEndToken): AnsiString;
var oldpos, start : integer;
    c : AnsiChar;
    found : boolean;
begin
  Result := '';
  oldpos := Position;
  try
    Found := False;
    start := Position;
    Read(c, 1);
    while (Position < Size) and not(found) and not( c in EndTokens) do begin
      if c <> Name[Position - start] then begin
        Start := Position;
      end else
        if Position - start = Length(Name) then
          found := True;

      if not(found) then
        Read(c, 1);
    end;

    if Found then begin
      Read(c, 1);
      while ( Position < Size) and not( c in ValueEndTokens) do begin
        Result := Result + c;
        Read(c, 1);
      end;
    end;

  finally
    Position := oldpos;
  end;
end;

function TModuleDefStream.UnQuote( Value : AnsiString): AnsiString;
var i : integer;
begin
  Result := '';
  for i := 1 to Length(Value) do
    if Value[i] <> '"' then
      Result := Result + Value[i];
end;

// ==== TBitReader =============================================================

constructor TBitReader.create;
begin
  FReadBitPointer := 8; // Forces a new read of a new byte in the BitBuffer
  FBitBuffer := 0;
end;

function TBitReader.ReadBits(aStream: TStream; NoOffBits: byte): Cardinal;
var
  Mask: Byte;
  BitBufferSize, BitsLeft : integer;
begin
  // Clavia uses a kind of compression with the patch data

  BitBufferSize := SizeOf(FBitBuffer) * 8;
  BitsLeft := BitBufferSize - FReadBitPointer; // Bits not read in BitBuffer;

  Mask := $FF shr FReadBitPointer;             // Readpointer : msb = 0, lsb = 7 !

  if (NoOffBits - BitsLeft) <= 0 then begin    // The bits left in the buffer can be used

    Result := (FBitBuffer and Mask)            // Clear the bits that are not needed
                  shr (BitsLeft - NoOffBits);  // Move the remaining bits to the right position

    FReadBitPointer := FReadBitPointer + NoOffBits; // New readpointer position

  end else begin                               // Read new data

    NoOffBits := NoOffBits - BitsLeft;
    Result := (FBitBuffer and Mask)            // Clear the bits that are not needed from the old buffer
                               shl NoOffBits;  // Move the remaining bits to the right position

    while (NoOffBits > 0) do begin
      // Read a new byte in the bitbuffer
      if aStream.Read(FBitBuffer, SizeOf(FBitBuffer)) <> SizeOf(FBitBuffer) then
        raise Exception.Create('Read error.');

      if NoOffBits < BitBufferSize then begin
        FReadBitPointer := NoOffBits;
        Result  := Result + FBitBuffer
          shr (BitBufferSize - FReadBitPointer);  // Add the bits from the new bitbuffer}
        NoOffBits := 0;
      end else begin
        FReadBitPointer := 8;
        NoOffBits := NoOffBits - BitBufferSize;
        Result  := Result + FBitBuffer
                                shl (NoOffBits);  // Add the bits from the new bitbuffer}
      end;
    end;
  end;
end;

// ==== TBitWriter =============================================================

constructor TBitWriter.Create;
begin
  FWriteBitPointer := 0;
  FBitBuffer := 0;
end;

procedure TBitWriter.WriteBits(aStream : TStream; Value : Cardinal; NoOffBits : byte);
var
  Mask : Cardinal;
  BitBufferSize, BitsLeft : integer;
begin
  BitBufferSize := SizeOf(FBitBuffer) * 8;

  BitsLeft := BitBufferSize - FWriteBitPointer;

  while NoOffBits > BitsLeft do begin

    FBitBuffer := FBitBuffer + Value shr (NoOffBits - BitsLeft);

    if aStream.Write(FBitBuffer, SizeOf(FBitBuffer)) <> SizeOf(FBitBuffer) then
      raise Exception.Create('Write error.');

    Mask := $FFFFFFFF shr (32 - (NoOffBits - BitsLeft));
    Value := Value and Mask;

    FBitBuffer := 0;
    FWriteBitPointer := 0;

    NoOffBits := NoOffBits - BitsLeft;

    BitsLeft := BitBufferSize - FWriteBitPointer;
  end;

  if NoOffBits = BitsLeft then begin

    FBitBuffer := FBitBuffer + Value;

    if aStream.Write(FBitBuffer, SizeOf(FBitBuffer)) <> SizeOf(FBitBuffer) then
      raise Exception.Create('Write error.');

    FBitBuffer := 0;
    FWriteBitPointer := 0;

  end else begin

    FBitBuffer := FBitBuffer + Value shl (BitsLeft - NoOffBits);
    FWriteBitPointer := FWriteBitPointer + NoOffBits;

  end;
end;

function TBitWriter.GetWriteBufferBitsLeft : integer;
begin
  Result := SizeOf(FBitBuffer) * 8 - FWriteBitPointer;
end;

// ==== TPatchChunk ============================================================

constructor TPatchChunk.Create( aStream : TStream);
begin
  FStream := aStream;
  //aStream.Position := 0;
  FBitReader := TBitReader.Create;
  FBitWriter := TBitWriter.Create;
  FWriteBuffer := TMemoryStream.Create;
  FReadBuffer := TMemoryStream.Create;
  {if aStream.Size > 0 then begin
    ReadBuffer(aStream.Size);
  end;}
  FWriteCrc := 0;
  FReadCrc := 0;
end;

destructor TPatchChunk.Destroy;
begin
  FReadBuffer.Free;
  FWriteBuffer.Free;
  FBitWriter.Free;
  FBitReader.Free;

  inherited;
end;

procedure TPatchChunk.ReadBuffer( aSize : integer);
var i : integer;
    Crc : Word;
begin
  FReadBuffer.Size := aSize;
  if FStream.Read(FReadBuffer.Memory^, aSize) <> aSize then
    raise Exception.Create('Read error');

  for i := 0 to aSize - 1 do
    FReadCrc := CrcClavia( FReadCrc, PStaticByteBuffer(FReadBuffer.Memory)^[i]);

  if assigned(FLogLines) then begin
    // Calculate another CRC for the buffer, to be able to compare chunkdata in logfile
    Crc := 0;
    for i := 0 to aSize - 1 do
      Crc := CrcClavia( Crc, PStaticByteBuffer(FReadBuffer.Memory)^[i]);
    FLogLines.Add('Caluclated Crc : ' + IntToHex( Crc, 4));
    FLogLines.Add('');
  end;

  FReadBuffer.Position := 0;

  FBitReader.FReadBitPointer := 8; // Forces a new read of a new byte in the BitBuffer
end;

function TPatchChunk.PeekID: integer;
var aId : byte;
begin
  Result := -1;
  if FStream.Read(aId, SizeOf(aId)) = SizeOf(aid) then begin
    Result := aId;
    FStream.Position := FStream.Position - 1;
  end;
end;

procedure TPatchChunk.ReadChunk;
var bm, bl : byte;
begin
  if FStream.Read(Fid, SizeOf(Fid)) <> SizeOf(Fid) then
    raise Exception.Create('Error reading chunk.');

  if FStream.Read(bm, SizeOf(bm)) <> SizeOf(bm) then
    raise Exception.Create('Error reading chunk.');

  if FStream.Read(bl, SizeOf(bl)) <> SizeOf(bl) then
    raise Exception.Create('Error reading chunk.');

  FSize := bm * 256 + bl;

  if assigned(FLogLines) then begin
    FLogLines.Add('Chunk id: ' + IntToHex(FId,2) + ', size: ' + IntToStr(FSize));
    DumpChunkData(FStream);
  end;

  // Calc crc
  FReadCrc := CrcClavia( FReadCrc, Fid);
  FReadCrc := CrcClavia( FReadCrc, bm);
  FReadCrc := CrcClavia( FReadCrc, bl);

  ReadBuffer( FSize);
end;

function TPatchChunk.ReadBits(NoOffBits : byte): Cardinal;
begin
  Result := FBitReader.ReadBits(FReadBuffer, NoOffBits);
end;

procedure TPatchChunk.WriteBits(Value : Cardinal; NoOffBits : byte);
begin
  FBitWriter.WriteBits(FWriteBuffer, Value, NoOffBits);
end;

procedure TPatchChunk.WriteChunk(aId : byte);
var BufSize : Word;
    bm, bl : byte;
    i : integer;
begin
  // Write the remaining byte if any
  if FBitWriter.FWriteBitPointer > 0 then
    if FWriteBuffer.Write(FBitWriter.FBitBuffer, 1) <> 1 then
      raise Exception.Create('Write error.');


  FBitWriter.FWriteBitPointer := 0;
  FBitWriter.FBitBuffer := 0;

  FWriteBuffer.Position := 0;
  BufSize := FWriteBuffer.Size;
  bm := Hi(BufSize);
  bl := Lo(BufSize);

  // Calc crc
  FWriteCrc := CrcClavia( FWriteCrc, aId);
  FWriteCrc := CrcClavia( FWriteCrc, bm);
  FWriteCrc := CrcClavia( FWriteCrc, bl);
  for i := 0 to BufSize - 1 do
    FWriteCrc := CrcClavia( FWriteCrc, PStaticByteBuffer(FWriteBuffer.Memory)^[i]);

  if (FStream.Write(aId, 1) <> 1) then
    raise Exception.Create('Write error');

  if (FStream.Write(bm, 1) <> 1) then
    raise Exception.Create('Write error');

  if (FStream.Write(bl, 1) <> 1) then
    raise Exception.Create('Write error');

  if (FStream.Write(FWriteBuffer.Memory^, BufSize) <> BufSize) then
    raise Exception.Create('Write error');

  FWriteBuffer.Clear;
end;

procedure TPatchChunk.Flush;
var BufSize : Word;
    i : integer;
begin
  // Write the remaining byte if any
  if FBitWriter.FWriteBitPointer > 0 then
    if FWriteBuffer.Write(FBitWriter.FBitBuffer, 1) <> 1 then
      raise Exception.Create('Write error.');

  FBitWriter.FWriteBitPointer := 0;
  FBitWriter.FBitBuffer := 0;

  FWriteBuffer.Position := 0;
  BufSize := FWriteBuffer.Size;

  // Calc crc
  for i := 0 to BufSize - 1 do
    FWriteCrc := CrcClavia( FWriteCrc, PStaticByteBuffer(FWriteBuffer.Memory)^[i]);

  if (FStream.Write(FWriteBuffer.Memory^, BufSize) <> BufSize) then
    raise Exception.Create('Write error');

  FWriteBuffer.Clear;
end;

function TPatchChunk.GetReadBufferBitsLeft: integer;
begin
  Result := SizeOf(FBitReader.FBitBuffer) * 8 - FBitReader.FReadBitPointer + (FReadBuffer.Size - FReadBuffer.Position) * 8;
end;

procedure TPatchChunk.WriteCrc(aStream : TStream);
var bm, bl : byte;
begin
  bm := (FWriteCrc shr 8) and $ff;
  aStream.Write( bm, SizeOf( bm));
  bl := FWriteCrc and $ff;
  aStream.Write( bl, SizeOf( bl));
end;

function TPatchChunk.ReadName: AnsiString;
var b : byte;
begin
  Result := '';
  b := ReadBits(8);
  while (b <> 0) do begin
    Result := Result + AnsiChar(b);
    if Length(Result) = 16 then
      break;
    b := ReadBits(8);
  end;
end;

procedure TPatchChunk.WriteName(Value : AnsiString);
var i : integer;
    b : Byte;
begin
  i := 1;
  while (i <= Length(Value)) and (i<=16) do begin
    if FWriteBuffer.Write(Value[i], 1) <> 1 then
      raise Exception.Create('Write error.');
    inc(i);
  end;
  if (i<=16) then begin
    b := 0;
    if FWriteBuffer.Write(b, 1) <> 1 then
      raise Exception.Create('Write error.');
  end;
end;

procedure TPatchChunk.DumpChunkData(aStream : TStream);
var p, i, c, position : integer;
    char_line, line : string;
    b : byte;
begin
  position := aStream.Position;
  try
    c := 0;
    i := 0;
    p := 0;
    line := '';
    char_line := '';;
    while (i<FSize) do begin
      if c < 16 then begin
        aStream.Read(b, 1);
        line := line + IntToHex(b, 2) + ' ';
        if b >= 32 then
          char_line := char_line + chr(b)
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
    aStream.Position := position;
  end;
end;


end.
