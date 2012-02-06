unit g2_usb;

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
//  The libusb-win32 usb driver is needed:
//
//  Download libusb-win32 snapshot from
//    http://sourceforge.net/projects/libusb-win32/files/
//
//  Make a system restore point
//  Install as filter driver on the clavia usb driver
//  Do NOT install as device driver (because it then permanently replaces the clavia driver!)
//
//  This unit contains the USB interface and the client/server functionality
//
//  The usb interface is based on the libusb-win32 (for windows) and the
//  libusb1.0 library for unix
//
//  The client/server is based on the Indy10 library
//
//  Parts ported from https://github.com/msg/g2ools
//
//  ////////////////////////////////////////////////////////////////////////////

interface

uses
{$IFDEF FPC}
  LCLIntf,
  LCLType,
{$ELSE}
  Windows, SyncObjs,
{$ENDIF}
{$IFDEF unix}
  libusb,
{$ELSE}
  LibUSBWin,
{$ENDIF}
  Messages, Forms, Classes, Dialogs, SysUtils, Controls, StdCtrls, ExtCtrls,
  g2_types, g2_file, g2_mess,
  IdCustomTCPServer, idTCPConnection, IdContext, IdTCPServer, idTCPClient,
  idSync, IdYarn, IdThread;

const
{$IFNDEF unix}
  LIBUSB_ERROR_TIMEOUT = -116;
{$ENDIF}
  TIME_OUT             = 0;     // Timeout wait for G2 interrupt message, must be long enough, otherwise there is the risk of losing contact with the G2

  DEFAULT_PORT         = 2501;  // The default tcpip port for the communication
  MAX_READ_BUFFER      = 65536; // tcpip buffer size, long enough to hold the longest message from the G2

type
  TCommand = (
    cmdConnect,
    cmdDisconnect,
    cmdGetPerformance,
    cmdSendPerformance,
    cmdGetGlobalKnobs,
    cmdSendGlobalKnobs,
    cmdGetVariationSettings,
    cmdSendVariationSettings,
    cmdSendMessage);

  // Client identification
  TClient = record
    ID         : integer;     // Unique ID
    ClientType : TClientType; // Editor or VST
  end;

  // The protocol for communication between server and client
  TProtocol = record
    Command     : TCommand;
    Sender      : TClient;
    MessageType : TMessageDataType;
    DataSize    : Integer;
  end;

const
  szProtocol = SizeOf(TProtocol);

type
  TUSBErrorEvent = procedure(Sender: TObject; ErrNo : integer; ErrText, LastCmd : string) of object;
  TUSBActiveChangeEvent = procedure(Sender: TObject; Active : boolean) of object;
  TAfterG2InitEvent = procedure(Sender: TObject) of Object;
  TInitStepEvent = procedure(Sender: TObject) of Object;
  TOnAddClient = procedure(Sender: TObject; ClientIndex : integer) of Object;
  TOnDeleteClient = procedure(Sender: TObject; ClientIndex : integer) of Object;
  TBeforeSendMessage = procedure(Sender: TObject; SenderID: integer; SendMessage : TG2SendMessage) of Object;
  TReceiveResponseMessage = procedure(Sender: TObject; ResponseMessage : TMemoryStream) of Object;

  TClientSendMessage = class;

  // Custom client context for the server
  TClientContext = class(TIdServerContext)
  private
    // Critical section to ensure a single access on the connection at a time
    FClientContextCriticalSection: TCriticalSection;
    FClient: TClient;
  public
    constructor Create(AConnection: TIdTCPConnection; AYarn: TIdYarn; AList: TThreadList = nil); override;
    destructor Destroy; override;
  public
    procedure Lock;
    procedure Unlock;
    procedure ServerSendClientSendMessage( ClientSendMessage : TClientSendMessage);
    procedure ServerSendClientResponseMessage( MemStream : TMemoryStream);
  public
    property Client: TClient read FClient write FClient;
  end;

  TG2USB = class;
  TG2USBPerformance = class;
  TG2USBSlot = class;
  TG2USBPatch = class;

  // Listening thread for a client
  TIdClientReadThread = class(TIdThread)
  protected
    FG2: TG2USB;
    LMessage : TMemoryStream;
    LProtocol: TProtocol;
    procedure Run; override;
  public
    constructor Create(AG2: TG2USB); reintroduce;
    destructor Destroy; override;
    procedure DoClientProcessServerSendMessage;
    procedure DoClientProcessServerResponseMessage;
  End;

  // Thread for receiving USB messages from the G2
  TReceiveMessageThread = class(TThread)
  private
    FG2USB       : TG2USB;
    // Next is all for logging
    FLogMessage  : string;
    FBuffer      : pointer;
    FMaxSize     : integer;
  protected
    procedure   Execute; override;
  public
    constructor Create( CreateSuspended: Boolean; aG2 : TG2USB);
    procedure   ProcessMessage;
    procedure   WriteLog;
    procedure   DumpMessage;
  end;

  // Thread for sending USB messages to the G2
  TSendMessageThread = class( TThread)
  private
    FG2USB       : TG2USB;
    // Next is all for logging
    FLogMessage  : string;
    FBuffer      : pointer;
    FMaxSize     : integer;
  protected
    procedure   Execute; override;
  public
    constructor Create( CreateSuspended: Boolean; aG2 : TG2USB);
    procedure   SendUSBMessage;
    procedure   WriteLog;
    procedure   DumpMessage;
  end;

  // Notification from the server listening thread to the main thread
  TG2ProcessClientSendMessage = class( TIdNotify)
  private
    FG2 : TG2USB;
    FBuffer : TMemoryStream;
    FClient : TClient;
    FClientContext : TClientContext;
  protected
    procedure DoNotify; override;
  end;

  // Record def. for table sending responseless messages
  TParamUpdRec = record
    SubCmd    : byte;
    Location  : byte;
    Module    : byte;
    Param     : byte;
    Morph     : byte;
    Value     : byte;
    Negative  : byte;
    Variation : byte;
    ClientContext : TClientContext;
    Changed   : boolean;
  end;

  // Message object for the send queue
  TClientSendMessage = class
    FMessage        : TG2SendMessage;
    FMessageSender  : TClient;
    FClientContext  : TClientContext; // nil : server, else a client
  end;

  TG2USB = class( TG2Mess)
    private
{$IFDEF unix}
      g2udev : Plibusb_device_handle; //a device handle
      ctx : PPlibusb_context;
{$ELSE}
      bus    : pusb_bus;
      g2dev  : pusb_device;
      g2udev : pusb_dev_handle;
      g2conf : usb_config_descriptor;
      g2intf : usb_interface;
      g2eps  : PArray_usb_endpoint_descriptor;
{$ENDIF}
      g2iin  : Byte;
      g2bin  : Byte;
      g2bout : Byte;

      g_bin_buf : TG2ResponseMessage;
      g_bout_buf : TG2SendMessage;

      // Threadsafe queue for sending USB messages to the G2
      FSendMessageQueue    : TThreadList;
      FSendMessageCount    : integer;

      FInitStep : integer;
      FInitialized : boolean;
      FProcessLedData : boolean;

      FOnUSBError           : TUSBErrorEvent;
      FOnUSBActiveChange    : TUSBActiveChangeEvent;
      FOnNextInitStep       : TInitStepEvent;
      FOnAfterG2Init        : TAfterG2InitEvent;
      FOnAddClient          : TOnAddClient;
      FOnDeleteClient       : TOnDeleteClient;
      FOnBeforeSendMessage  : TBeforeSendMessage;
      FOnReceiveResponseMessage : TReceiveResponseMessage;

      // Vars for communication between server and G2
      FWaitforCmd           : byte;
      FMessageSendStart     : integer;
      FReceiveMessageThread : TReceiveMessageThread;
      FSendMessageThread    : TSendMessageThread;
      FReceiveMessageThreadHandle : THandle;
      FSendMessageThreadHandle : THandle;

      // Vars for communication between server and client
      FHost                 : AnsiString;
      FPort                 : integer;
      FIsServer             : boolean;
      FClientID             : TClient;       // user name & ID

      // Indy server object
      FIdTCPServer          : TIdTCPServer;

      // Indy client object
      FIdTCPClient          : TIdTCPClient;

      // Listening thread for a client
      FidClientReadThread    : TIdClientReadThread;
      FClientCriticalSection : TCriticalSection;

      function    GetUSBActive : boolean;
      procedure   SetUSBActive(const value : boolean);
{$IFDEF unix}
      function    iread(addr: byte; var buffer; size, timeout: longword): integer;
      function    bread(addr: byte; var buffer; size, timeout: longword): integer;
      function    bwrite(addr: byte; var buffer; size, timeout: longword): integer;
{$ELSE}
      function    bwrite(addr : longword; var buffer; size, timeout : longword): integer;
      function    bread(addr : longword; var buffer; size, timeout : longword): integer;
      function    iread(addr : longword; var buffer; size, timeout : longword): integer;
{$ENDIF}
      function    extended_message(var iin_buf : TByteBuffer): integer;
      function    embedded_message(var iin_buf : TByteBuffer): integer;
      function    isextended(buf_in : TByteBuffer): boolean;
      function    isembedded(buf_in : TByteBuffer): boolean;
    protected
      function    GetID : integer; override;
      property    OnNextInitStep : TInitStepEvent read FOnNextInitStep write FOnNextInitStep;
    public
      FLogLedDataMessages  : boolean;
      FErrorMessage        : boolean;
      FErrorMessageNo      : integer;

      constructor Create( AOwner: TComponent); override;
      destructor  Destroy; override;

      function    CreatePerformance: TG2FilePerformance; override;

      function    GetPerformance : TG2USBPerformance;
      function    GetSlot( aSlot : byte) : TG2USBSlot;

      // Initialization USB interface
      function    Init: boolean;
      procedure   Done;

      function    get_error: string;

      procedure   SendOnClientMessage( SendMessage : TMemoryStream);
      function    SendCmdMessage( SendMessage : TG2SendMessage): boolean;

      // Client/server communication

      procedure   IdTCPServerConnect( AContext: TIdContext);
      procedure   IdTCPServerDisconnect( AContext: TIdContext);
      procedure   IdTCPServerExecute( AContext: TIdContext);

      function    ServerProcessClientMessage( ClientMessage : TClientSendMessage): boolean;
      procedure   ServerBroadcastResponseMessage( ResponseMessage : TG2ResponseMessage);
      procedure   ServerBroadcastSendMessage( ClientSendMessage : TClientSendMessage);
      function    ServerProcessClientResponselessMessage( ClientMessage : TClientSendMessage): boolean;

      procedure   ClientProcessServerSendMessage( MemStream : TMemoryStream; SenderID : integer);
      procedure   ClientProcessServerResponseMessage( MemStream : TMemoryStream);
      procedure   ClientSendMessageToServer( MemStream : TMemoryStream);
      procedure   ClientSendConnectedToServer;

      function    GetClientCount: integer;

      procedure   Lock;
      procedure   Unlock;

      // USB communication
      procedure   USBSendMessage;
      procedure   USBSendParamUpd;
      procedure   USBProcessMessage( ResponseMessage : TG2ResponseMessage);
      procedure   USBProcessResponseMessage( ResponseMessage : TG2ResponseMessage);
      procedure   USBProcessSendMessage( ClientSendMessage : TClientSendMessage);

      procedure   USBStartInit;
      procedure   USBInitSeq(Sender: TObject);
      procedure   USBSentStartComm(Sender: TObject);

      procedure   SendInitMessage;
      procedure   SendStartStopCommunicationMessage( Stop : byte);
      procedure   SendGetPatchVersionMessage(var patch_version: byte);
      procedure   SendGetSynthSettingsMessage;
      procedure   SendSetSynthSettingsMessage;
      procedure   SendUnknown1Message;
      procedure   SendDumpMidiMessage;
      procedure   SendListMessage( aMode, aBank, aPatch : byte; names : TStrings);
      procedure   SendSetModeMessage( aMode : byte);
      procedure   SendNoteMessage( aNote : byte; aOnoff : byte);

      property    IdTCPClient : TIdTCPClient read FIdTCPClient;
    published
      property    IsServer : boolean read FIsServer write FIsServer;
      property    Port : integer read FPort write FPort;
      property    Host : AnsiString read FHost write FHost;
      property    USBActive : boolean read GetUSBActive write SetUSBActive;
      property    ProcessLedData : boolean read FProcessLedData write FProcessLedData;
      property    OnUSBError : TUSBErrorEvent read FOnUSBError write FOnUSBError;
      property    OnUSBActiveChange : TUSBActiveChangeEvent read FOnUSBActiveChange write FOnUSBActiveChange;
      property    OnAfterG2Init : TAfterG2InitEvent read FOnAfterG2Init write FOnAfterG2Init;
      property    OnAddClient : TOnAddClient read FOnAddClient write FOnAddClient;
      property    OnDeleteClient : TOnDeleteClient read FOnDeleteClient write FOnDeleteClient;
      property    OnBeforeSendMessage : TBeforeSendMessage read FOnBeforeSendMessage write FOnBeforeSendMessage;
      property    OnReceiveResponseMessage : TReceiveResponseMessage read FOnReceiveResponseMessage write FOnReceiveResponseMessage;
  end;

  TG2USBPerformance = class( TG2MessPerformance)
  private
    FOnNextInitStep     : TInitStepEvent;
    FInitStep           : integer;
    FStartCommAfterInit : boolean;
  protected
    property    OnNextInitStep : TInitStepEvent read FOnNextInitStep write FOnNextInitStep;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;

    function    CreateSlot : TG2FileSlot; override;

    function    GetSlot( aSlot : byte) : TG2USBSlot;

    function    SendCmdMessage( SendMessage : TG2SendMessage): boolean;

    procedure   USBStartInit( aStartCommAfterInit : boolean);
    procedure   USBInitSeq(Sender: TObject);

    procedure   SendGetPerfSettingsMessage;
    procedure   SendUnknown2Message;
    procedure   SendSelectSlotMessage( aSlot: byte);
    procedure   SendRetrieveMessage( aSlot, aBank, aPatch : byte);
    procedure   SendStoreMessage( aSlot, aBank, aPatch : byte);
    procedure   SendSetPerformanceMessage( aPerfName : AnsiString; aPerf : TG2FilePerformance);
    procedure   SendSetPerfSettingsMessage;
    procedure   SendSetPerfNameMessage( aPerfName : AnsiString);
    procedure   SendGetGlobalKnobsMessage;
  end;

  TG2USBSlot = class( TG2MessSlot)
  private
    FParamUpdBuf        : array of TParamUpdRec;
    FParamUpdBufCount   : integer;
    FInitStep           : integer;
    FOnNextInitStep     : TInitStepEvent;
    FStartCommAfterInit : boolean;
  protected
    property    OnNextInitStep : TInitStepEvent read FOnNextInitStep write FOnNextInitStep;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;

    function    CreatePatch : TG2FilePatch; override;

    function    GetPatch : TG2USBPatch;
    function    GetPerformance : TG2USBPerformance;

    function    SendCmdMessage(  SendMessage : TG2SendMessage): boolean;

    procedure   USBStartInit( aStartCommAfterInit : boolean);
    procedure   USBInitSeq(Sender: TObject);

    procedure   SendGetPatchVersionMessage;
    procedure   SendPatchNotesMessage;
    procedure   SendControllerSnapshotMessage;
    procedure   SendResourceTableMessage( aLocation : Byte);
    procedure   SendGetPatchNameMessage;
    procedure   SendCurrentNoteMessage;
    procedure   SendUnknown6Message;
    procedure   SendGetSelectedParameterMessage;
    procedure   SendSetPatchMessage( aPatchName : AnsiString; aPatch : TG2FilePatch);
    procedure   SendGetPatchMessage;
    procedure   SendSelectVariationMessage( aVariationIndex: byte);
    procedure   SendSetParamMessage( aLocation, aModule, aParam, aValue, aVariation: byte);
    procedure   SendSelParamMessage( aLocation, aModule, aParam: integer);
    procedure   SendSetMorphMessage( aLocation, aModule, aParam, aMorph, aValue, aNegative, aVariation: byte);
    procedure   SendSetModeMessage( aLocation, aModule, aParam, aValue: integer);
    procedure   SendCopyVariationMessage( aFromVariation, aToVariation : byte);
    // All responseless messages must be send through the ParamUpdBuf
    procedure   AddParamUpdRec( aSubCmd, aLocation, aModule, aParam, aMorph, aValue, aNegative, aVariation : byte; aClientContext : TClientContext);

    property    ParamUpdBufCount : integer read FParamUpdBufCount;
  end;

  TG2USBPatch = class( TG2MessPatch)
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;

    function    GetPerformance : TG2USBPerformance;

    function    SendCmdMessage( SendMessage : TG2SendMessage): boolean;

    procedure   SendUndoMessage;

    function    MessSetPatchDescription( FPatchDescription : TPatchDescription): boolean;
    function    MessAddModule( aLocation : TLocationType; aModuleType, aCol, aRow: byte): boolean; override;
    function    MessCopyModules( aSrcePatch : TG2FilePatchPart; aFromLocation, aToLocation : TLocationType): boolean; override;
    function    MessAddConnection( aLocation : TLocationType; aFromConnector, aToConnector : TG2FileConnector): boolean; override;
    function    MessDeleteConnection( aLocation : TLocationType; aCable : TG2FileCable): boolean; override;
    function    MessDeleteModule( aLocation : TLocationType; aModuleIndex : byte): boolean; override;
    function    MessDeleteModules( aLocation : TLocationType): boolean; override;
    function    MessMoveModules: boolean; virtual;
    function    MessSetModuleColor( aLocation: TLocationType; aModuleIndex, aColor : byte): boolean; override;
    function    MessAssignKnob(aLocation : TLocationType; aModule, aParam, aKnob: integer): boolean;
    function    MessDeAssignKnob(aKnob: integer): boolean;
    function    MessAssignMidiCC(aLocation: TLocationType; aModule, aParam, aMidiCC: integer): boolean;
    function    MessDeassignMidiCC( aMidiCC: integer): boolean;
    function    MessAssignGlobalKnob( aLocation : TLocationType; aModule, aParam, aKnob: integer): boolean;
    function    MessDeassignGlobalKnob(aKnob: integer): boolean;
    function    MessSetModuleParamLabels( aLocation : TLocationType; aModuleIndex, aParamIndex : byte; aName : AnsiString): boolean; override;
    function    MessSetModuleLabel( aLocation : TLocationType; aModuleIndex : byte; aName : AnsiString): boolean; override;
  end;

implementation

function ProtocolToStream(const AProtocol: TProtocol): TMemoryStream;
begin
  Result := TMemoryStream.Create;
  // set the length of result to the length of the protocol
  Result.Size := szProtocol;
  // move a block of memory from AProtocol to Result
  Move(AProtocol, Result.Memory^, szProtocol);
end;

function StreamToProtocol( MemStream : TMemoryStream): TProtocol;
begin
  if MemStream.Size < szProtocol then
    MemStream.Size := szProtocol;
  Move( MemStream.Memory^, Result, szProtocol);
end;

procedure InitProtocol(var AProtocol: TProtocol);
begin
  FillChar(AProtocol, szProtocol, 0);
end;

////////////////////////////////////////////////////////////////////////////////
//  TG2USB
////////////////////////////////////////////////////////////////////////////////

constructor TG2USB.Create( AOwner: TComponent);
begin
  inherited Create( AOwner);

  FClientCriticalSection := TCriticalSection.Create;

  g_bin_buf := TG2ResponseMessage.Create;
  g_bout_buf := TG2SendMessage.Create;

  FInitialized := False; // G2 Initialization succesfull
  FProcessLedData := False;

  FSendMessageQueue := TThreadList.Create;
  FSendMessageQueue.Duplicates := dupAccept;
  FSendMessageCount := 0;

  FHost := '127.0.0.1'; // TODO: get it from the ini file
  FPort := DEFAULT_PORT;

  FLogLedDataMessages := False;
end;

destructor TG2USB.Destroy;
var i : integer;
    MessageList : TList;
begin
  // Free the send message queue
  MessageList := FSendMessageQueue.LockList;
  try
    for i := 0 to MessageList.Count - 1 do begin
      TClientSendMessage(MessageList[i]).FMessage.Free;
      TClientSendMessage(MessageList[i]).Free;
    end;
    FSendMessageCount := MessageList.Count;
  finally
    FSendMessageQueue.UnlockList;
  end;
  FSendMessageQueue.Free;

  // Free the usb message buffers
  g_bout_buf.Free;
  g_bin_buf.Free;

  FClientCriticalSection.Free;

  inherited;
end;

function TG2USB.get_error: string;
var i : integer;
    err : PAnsiChar;
begin
{$IFDEF unix}
  Result := 'error...';
{$ELSE}
  err := usb_strerror;

  Result := '';
  i := 0;
  while (i<255) and (err[i]<>#0) do begin
    Result := Result + err[i];
    inc(i);
  end;
{$ENDIF}
end;

function TG2USB.GetPerformance: TG2USBPerformance;
begin
  Result := Performance as TG2USBPErformance;
end;

function TG2USB.GetSlot( aSlot: byte): TG2USBSlot;
begin
  Result := Performance.Slot[ aSlot] as TG2USBSlot;
end;
{$IFDEF unix}
function TG2USB.Init : boolean;
var returncode:integer;
    dev : libusb_device;
    devs : PPlibusb_device;
    cnt : integer;
begin
  // Initialization of the USB interface unix

  ctx := nil;

  returncode := libusb_init(@ctx);     //init libusb API
  libusb_set_debug(ctx,3);             //set debug

  cnt := libusb_get_device_list(ctx,@devs);
  g2udev := libusb_open_device_with_vid_pid(ctx,VENDOR_ID,PRODUCT_ID); //hid lib test

  if assigned(g2udev) then begin
    libusb_free_device_list(devs, 1); //free the list, unref the devices in it
    //kernel driver attaching problem
    if (libusb_kernel_driver_active(g2udev, 0) = 1) then  //find out if kernel driver is attached
    begin
      add_log_line('Kernel Driver Active', LOGCMD_HDR);
      if(libusb_detach_kernel_driver(g2udev, 0) = 0) then //detach it
        add_log_line('Kernel Driver Detached!', LOGCMD_HDR);
    end;

    dev := libusb_get_device(g2udev);

    // get 3 endpoints
    g2iin := $81;
    g2bin := $82;
    g2bout := $03;

    //g2udev := usb_open(g2dev);
    //if not Assigned(g2udev) then
    //  raise Exception.Create('Unable to open device.');

    returncode := libusb_claim_interface(g2udev, 0);   //claim usb interface
    if returncode < 0 then
      add_log_line('Claim not possible!', LOGCMD_HDR);
  end;
end;
{$ELSE}
function TG2USB.Init : boolean;
var dev: pusb_device;
begin
  // Initialization of the USB interface windows
  try
    Result := False;

    // Find g2 usb device
    g2dev := nil;
    g2udev := nil;

    // LibUSB-Win32 Initialization
    usb_init;              // Initialize libusb
    usb_find_busses;       // Finds all USB busses on system
    usb_find_devices;      // Find all devices on all USB devices
    bus := usb_get_busses; // Return the list of USB busses found

    while Assigned(bus) do begin
      dev := bus^.devices;
      while Assigned(dev) do begin
        if (dev^.descriptor.idVendor = VENDOR_ID) and (dev^.descriptor.idProduct = PRODUCT_ID) then
          g2dev := dev;
        dev := dev^.next;
      end;
      bus := bus^.next;
    end;

    usb_set_debug(255);

    if g2dev = nil then
      raise Exception.Create('No g2 device found');

    // get 3 endpoints
    g2conf := g2dev^.config[0];
    g2intf := g2conf.iinterface[0];
    g2eps := g2intf.altsetting[0].endpoint;

    g2iin := g2eps[0].bEndpointAddress;
    g2bin := g2eps[1].bEndpointAddress;
    g2bout := g2eps[2].bEndpointAddress;

    g2udev := usb_open(g2dev);
    if not Assigned(g2udev) then
      raise Exception.Create('Unable to open device.');

    if usb_set_configuration(g2udev, g2conf.bConfigurationValue) < 0 then
      raise Exception.Create('Unable to set configuration.');

    if usb_claim_interface(g2udev, 0) < 0 then
      raise Exception.Create('Unable to claim the interface.');

    Result := True;

  except on E:Exception do begin
      MessageDlg( E.Message, mtError, [mbOK], 0);
      Result := False;
    end;
  end;
end;
{$ENDIF}

{$IFDEF unix}
procedure TG2USB.Done;
var returncode:integer;
begin
  // Disconnect from the USB interface unix

  returncode := libusb_release_interface(g2udev, 0); //release the claimed interface
  if(returncode<>0) then
    add_log_line('Cannot Release Interface', LOGCMD_HDR) //result handler
  else
    add_log_line('Released Interface', LOGCMD_HDR);
  if assigned(g2udev) then begin
    // G2 is never attached to the kernel, so following not needed
    //if (libusb_attach_kernel_driver(g2udev, 0) = 0) then //attach kernel again it
    //  add_log_line('Kernel Driver Attached!', LOGCMD_HDR);
    libusb_close(g2udev); //close the device we opened
    g2udev := nil;
  end;
  libusb_exit(ctx);
end;
{$ELSE}
procedure TG2USB.Done;
begin
  // Disconnect from the USB interface windows

  if Assigned(g2udev) then begin
    usb_release_interface(g2udev, 0);
    usb_close(g2udev);
    g2udev := nil;
  end;
end;
{$ENDIF}

{$IFDEF unix}
function TG2USB.iread(addr: byte; var buffer; size, timeout: longword): integer;
var requested : integer;
    bytes_read : longint;
begin
  // Read an interrupt message from USB unix
  Result := libusb_interrupt_transfer(g2udev,addr,PChar(@buffer), size, @bytes_read, timeout);
  if Result >= 0 then
    Result := bytes_read;
end;

function TG2USB.bread(addr: byte; var buffer; size, timeout: longword): integer;
var requested : integer;
    bytes_read : longint;
begin
  // Read a bulk message from USB unix
  Result := libusb_bulk_transfer(g2udev,addr,PChar(@buffer), size, @bytes_read, timeout);

  if Result < 0 then
    add_log_line(get_error, LOGCMD_ERR)
  else
    Result := bytes_read;
end;

function TG2USB.bwrite(addr: byte; var buffer; size, timeout: longword): integer;
var bytes_written : longint;
begin
  // Write a bulk message over USB unix
  try
    Result := libusb_bulk_transfer(g2udev,addr,PChar(@buffer), size, @bytes_written, timeout);
  finally
    if Result < 0 then
      add_log_line(get_error, LOGCMD_ERR)
    else
      Result := size;
  end;
end;

{$ELSE}
function TG2USB.iread(addr: longword; var buffer; size, timeout: longword): integer;
begin
  // Read an interrupt message from USB windows
  Result := usb_interrupt_read(g2udev, addr, buffer, size, timeout);
end;

function TG2USB.bread(addr: longword; var buffer; size, timeout: longword): integer;
begin
  // Read a bulk message from USB windows
  if assigned(g2udev) then
    Result := usb_bulk_read(g2udev, addr, buffer, size, timeout);

  if Result < 0 then
    add_log_line(get_error, LOGCMD_ERR);
end;

function TG2USB.bwrite(addr: longword; var buffer; size, timeout: longword): integer;
begin
  // Write a bulk message over USB windows
  Result := usb_bulk_write(g2udev, addr, buffer, size, timeout);

  if Result < 0 then
    add_log_line(get_error, LOGCMD_ERR);
end;
{$ENDIF}
function TG2USB.extended_message(var iin_buf : TByteBuffer): integer;
var i, retries, bytes_read, buf_position : integer;
    ecrc, acrc : word;
    size : word;
begin
  // Read an G2 extended message from USB, interrupt messages contains length,
  // bulk message contains the data

  Result := -1;

  size := (iin_buf[1] shl 8) or iin_buf[2];

  g_bin_buf.Position := 0;
  g_bin_buf.Size := Size;

  retries := 5; // the message has to return within 5 tries
  bytes_read := 0;
  buf_position := 0;
  while (retries > 0) and (size <> bytes_read) and (bytes_read >= 0) do begin
    bytes_read := bread(g2bin, PStaticByteBuffer(g_bin_buf.Memory)^[buf_position], size, TIME_OUT);
    if bytes_read = LIBUSB_ERROR_TIMEOUT then
      bytes_read := 0 //time out
    else
      buf_position := buf_position + bytes_read;
    dec(retries);
  end;

  if retries = 0 then begin
    add_log_line('Timeout reading extended message.', LOGCMD_ERR);
    exit;
  end;

  if buf_position > 0 then begin

    ecrc := 0; // expected crc
    for i := 0 to size - 2 - 1 do
      ecrc := CrcClavia(ecrc, PStaticByteBuffer(g_bin_buf.Memory)^[i]);

    acrc := PStaticByteBuffer(g_bin_buf.Memory)^[ size-1] // actual crc
          + PStaticByteBuffer(g_bin_buf.Memory)^[ size-2] * 256;

    if ecrc <> acrc then
      add_log_line('Bad crc exp: ' + IntToHex(ecrc,2) + ' act: ' + IntToHex(acrc,2), LOGCMD_ERR);
  end;
  Result := bytes_read;
end;

function TG2USB.embedded_message(var iin_buf : TByteBuffer): integer;
var i, dil : integer;
    ecrc, acrc : word;
begin
  // Read an embedded G2 message, interrupt message contains length and data

  Result := -1;

  g_bin_buf.Position := 0;
  g_bin_buf.Size := Length(iin_buf);
  move( iin_buf[0], g_bin_buf.Memory^, Length(iin_buf));

  dil := PStaticByteBuffer(g_bin_buf.Memory)^[0] shr 4;
  ecrc := 0;
  for i := 1 to dil - 2 do
    ecrc := CrcClavia(ecrc, PStaticByteBuffer(g_bin_buf.Memory)^[i]);

  acrc := PStaticByteBuffer(g_bin_buf.Memory)^[dil-0] // actual crc
        + PStaticByteBuffer(g_bin_buf.Memory)^[dil-1] * 256;

  if ecrc <> acrc then
    add_log_line('Bad crc exp: ' + IntToHex(ecrc,2) + ' act: ' + IntToHex(acrc,2), LOGCMD_ERR);

  Result := g_bin_buf.Size;
end;

function TG2USB.isextended( buf_in : TByteBuffer): boolean;
begin
  Result := buf_in[0] and $f = 1;
end;

function TG2USB.isembedded( buf_in : TByteBuffer): boolean;
begin
  Result := buf_in[0] and $f = 2;
end;

////////////////////////////////////////////////////////////////////////////////
//  TSendMessageThread
//
//  Send messages over USB interface to G2
////////////////////////////////////////////////////////////////////////////////

constructor TSendMessageThread.Create(CreateSuspended: Boolean; aG2: TG2USB);
begin
  FG2USB := aG2;
//  FG2USB.add_log_line('Creating send thread...', LOGCMD_NUL);

  FreeOnTerminate := True;
  inherited Create(CreateSuspended);

{$IFDEF unix}
  Priority := tpNormal;
{$ELSE}
  Priority := tpHigher;
{$ENDIF}
end;

procedure TSendMessageThread.Execute;
var Error : boolean;
begin
  Error := False;
  FG2USB.FWaitForCmd := 0;

  repeat
    // Waiting for a response? No, send a new command
    if (FG2USB.FWaitForCmd = 0) then begin
      // Send updated parameters (no response)
      FG2USB.USBSendParamUpd;
      // Send new message if any
      if (FG2USB.FSendMessageCount > 0) then
        FG2USB.USBSendMessage;
      sleep(1);
    end else
      sleep(10);

    if Error then
      Terminate;

  until Terminated;
end;

procedure TSendMessageThread.SendUSBMessage;
begin
  FG2USB.USBSendMessage;
end;

procedure TSendMessageThread.WriteLog;
begin
  FG2USB.add_log_line( FLogMessage, LOGCMD_ERR);
end;

procedure TSendMessageThread.DumpMessage;
begin
  FG2USB.dump_buffer( FBuffer^, FMaxSize);
end;

////////////////////////////////////////////////////////////////////////////////
//  TReceiveMessageThread
//
//  Receive messages from USB interface
////////////////////////////////////////////////////////////////////////////////

constructor TReceiveMessageThread.Create(CreateSuspended: Boolean; aG2 : TG2USB);
begin
  FG2USB := aG2;
//  FG2USB.add_log_line('Creating thread...', LOGCMD_NUL);

  FreeOnTerminate := True;
  inherited Create(CreateSuspended);

  Priority := tpHigher;
end;

procedure TReceiveMessageThread.Execute;
var iin_buf : TByteBuffer;
    i, bytes_read : integer;
    error : boolean;
    Cmd, b : byte;
begin
  SetLength(iin_buf, 16);

  error := False;

  repeat
    // Is USB Active?
    if Assigned( FG2USB.g2udev) then begin

      // Read the interrupt message
      // Time out must be long enough, otherwise we lose contact with the G2!
      // TIME_OUT = 0 : wait forever
      bytes_read := FG2USB.iread( FG2USB.g2iin, iin_buf[0], 16, TIME_OUT);

      if bytes_read > 0 then begin
        if FG2USB.isextended(iin_buf) then begin
          bytes_read := FG2USB.extended_message(iin_buf);
          Synchronize(ProcessMessage);
        end else
          if FG2USB.isembedded( iin_buf) then begin
            bytes_read := FG2USB.embedded_message(iin_buf);
            Synchronize(ProcessMessage);
          end else begin
            FLogMessage := 'RecThread: Message received is not embedded nor extended.';
            Synchronize( WriteLog);
            Error := True;
          end;
      end else begin
        if bytes_read < 0 then begin
          if bytes_read <> LIBUSB_ERROR_TIMEOUT then begin
            Error := True;
            FLogMessage := 'RecThread: Interrupt read error ' + IntToStr(bytes_read);
            Synchronize( WriteLog);
            FBuffer := FG2USB.g_bout_buf.Memory;
            FMaxSize :=  FG2USB.g_bout_buf.Size;
            Synchronize( DumpMessage);
          end;
        end;
      end;
    end;

    if Error then
      Terminate;

  until Terminated;
end;

procedure TReceiveMessageThread.ProcessMessage;
begin
  FG2USB.USBProcessMessage( FG2USB.g_bin_buf);
end;

procedure TReceiveMessageThread.WriteLog;
begin
  FG2USB.add_log_line( FLogMessage, LOGCMD_NUL);
end;

procedure TReceiveMessageThread.DumpMessage;
begin
  FG2USB.dump_buffer( FBuffer^, FMaxSize);
end;

////////////////////////////////////////////////////////////////////////////////
//  Processing USB Messages
////////////////////////////////////////////////////////////////////////////////

procedure TG2USB.USBSendMessage;
var retries, total_bytes_send, bytes_send, packet : integer;
    crc : word;
    b : byte;
    MessageList : TList;
    ClientSendMessage : TClientSendMessage;
begin
  // Send a message to the G2 over the USB interface

  if FSendMessageCount = 0 then
    exit;

  // Read top message in queue
  MessageList := FSendMessageQueue.LockList;
  try
    ClientSendMessage := TClientSendMessage( MessageList[0]);
    ClientSendMessage.FMessage.PrepareForSend;
    g_bout_buf.Clear;
    g_bout_buf.Write( ClientSendMessage.FMessage.Memory^,
                      ClientSendMessage.FMessage.Size);
  finally
    FSendMessageQueue.UnlockList;
  end;

  // Init expected answer to command
  FWaitForCmd := g_bout_buf.Command;
  FMessageSendStart := GetTickCount;

  if Assigned(g2udev) then begin

    if g_bout_buf.size > 0  then begin
    // Break the message up in packets, but for the G2 the messages aren't very big thus maybe unecessary
      retries := 5;
      total_bytes_send := 0;
      bytes_send := 0;
      while (total_bytes_send < g_bout_buf.size) and ((bytes_send >= 0) or (bytes_send = LIBUSB_ERROR_TIMEOUT)) and (retries > 0) do begin

        if (g_bout_buf.size - total_bytes_send) > MAX_BULK_DATA_SIZE then
          packet := MAX_BULK_DATA_SIZE
        else
          packet := ( g_bout_buf.size - total_bytes_send);

        bytes_send := bwrite( g2bout, PStaticByteBuffer(g_bout_buf.Memory)^[total_bytes_send], packet, TIME_OUT);
        if bytes_send > 0 then
          total_bytes_send := total_bytes_send + bytes_send
        else
          if bytes_send = LIBUSB_ERROR_TIMEOUT then
            dec(retries);
      end;

      if (retries = 0) and (total_bytes_send < g_bout_buf.size) then begin
        add_log_line( 'Timeout sending message', LOGCMD_ERR);
      end;

    end else
      exit; // nothing to send
  end;
end;

procedure TG2USB.USBSendParamUpd;
var i, j : integer;
    SlotIndex , Size : byte;
    Slot : TG2USBSlot;
    crc : word;
    ClientSendMessage : TClientSendMessage;
begin
  // Send all responseless messages with this function

  ClientSendMessage := TClientSendMessage.Create;
  ClientSendMessage.FMessage := g_bout_buf;
  try
    g_bout_buf.Size := 20; // long enough for the longest message

    for SlotIndex := 0 to 3 do begin

      Slot := GetSlot(SlotIndex);

      for i := 0 to Slot.FParamUpdBufCount - 1 do begin
        if Slot.FParamUpdBuf[i].Changed then begin
          case Slot.FParamUpdBuf[i].SubCmd of
            S_SET_PARAM :
              begin
                Size := 13;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 0] := 0;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 1] := Size;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 2] := $01;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 3] := CMD_NO_RESP + CMD_SLOT + SlotIndex;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 4] := Slot.PatchVersion; // Current patch version!
                PStaticByteBuffer(g_bout_buf.Memory)^[ 5] := S_SET_PARAM;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 6] := Slot.FParamUpdBuf[i].Location;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 7] := Slot.FParamUpdBuf[i].Module;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 8] := Slot.FParamUpdBuf[i].Param;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 9] := Slot.FParamUpdBuf[i].Value;
                PStaticByteBuffer(g_bout_buf.Memory)^[10] := Slot.FParamUpdBuf[i].Variation;
              end;
            S_SEL_PARAM :
              begin
                Size := 12;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 0] := 0;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 1] := Size;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 2] := $01;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 3] := CMD_NO_RESP + CMD_SLOT + SlotIndex;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 4] := Slot.PatchVersion; // Current patch version!
                PStaticByteBuffer(g_bout_buf.Memory)^[ 5] := S_SEL_PARAM;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 6] := 00;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 7] := Slot.FParamUpdBuf[i].Location;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 8] := Slot.FParamUpdBuf[i].Module;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 9] := Slot.FParamUpdBuf[i].Param;
              end;
            S_SET_MORPH_RANGE :
              begin
                Size := 15;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 0] := 0;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 1] := Size;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 2] := $01;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 3] := CMD_NO_RESP + CMD_SLOT + SlotIndex;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 4] := Slot.PatchVersion; // Current patch version!
                PStaticByteBuffer(g_bout_buf.Memory)^[ 5] := S_SET_MORPH_RANGE;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 6] := Slot.FParamUpdBuf[i].Location;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 7] := Slot.FParamUpdBuf[i].Module;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 8] := Slot.FParamUpdBuf[i].Param;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 9] := Slot.FParamUpdBuf[i].Morph;
                PStaticByteBuffer(g_bout_buf.Memory)^[10] := Slot.FParamUpdBuf[i].Value;
                PStaticByteBuffer(g_bout_buf.Memory)^[11] := Slot.FParamUpdBuf[i].Negative;
                PStaticByteBuffer(g_bout_buf.Memory)^[12] := Slot.FParamUpdBuf[i].Variation;
              end;
          end;

          // Calc CRC
          crc := 0;
          for j := 2 to Size - 3 do
            crc := CrcClavia(crc, PStaticByteBuffer(g_bout_buf.Memory)^[j]);

          // Write CRC
          PStaticByteBuffer(g_bout_buf.Memory)^[Size - 2] := CRC div 256;
          PStaticByteBuffer(g_bout_buf.Memory)^[Size - 1] := CRC mod 256;;

          // Online?
          if assigned(g2udev) then
            bwrite( g2bout, PStaticByteBuffer(g_bout_buf.Memory)^, Size, TIME_OUT);

          Slot.FParamUpdBuf[i].Changed := False;

          ClientSendMessage.FClientContext := Slot.FParamUpdBuf[i].ClientContext;
          USBProcessSendMessage( ClientSendMessage);
        end;
      end;
    end;
  finally
    ClientSendMessage.Free;
  end;
end;

procedure TG2USB.USBProcessMessage( ResponseMessage : TG2ResponseMessage);
var Cmd : byte;
    MessageList : TList;
    ClientSendMessage : TClientSendMessage;
begin
  // Process a response message received over USB

  Cmd := ResponseMessage.Command;

  USBProcessResponseMessage( ResponseMessage);

  if (FWaitForCmd <> 0) and ((Cmd = FWaitForCmd) or ((Cmd = $04) and (FWaitForCmd = $0c))) then begin
    // Get the send message from the list
    MessageList := FSendMessageQueue.LockList;
    try
      ClientSendMessage := TClientSendMessage( MessageList[0]);
      MessageList.Delete(0);
      FSendMessageCount := MessageList.Count;
    finally
      FSendMessageQueue.UnlockList;
    end;

    USBProcessSendMessage( ClientSendMessage);

    if assigned(FOnReceiveResponseMessage) then
      FOnReceiveResponseMessage( Self, ResponseMessage);

    ClientSendMessage.FMessage.Free;
    ClientSendMessage.Free;
    FWaitForCmd := 0;

    // Send next message in a sequence, if assigned
    if assigned(GetSlot(0)) and assigned(GetSlot(0).FOnNextInitStep)  then
      GetSlot(0).FOnNextInitStep(self)
    else

    if assigned(GetSlot(1)) and assigned(GetSlot(1).FOnNextInitStep)  then
      GetSlot(1).FOnNextInitStep(self)
    else

    if assigned(GetSlot(2)) and assigned(GetSlot(2).FOnNextInitStep)  then
      GetSlot(2).FOnNextInitStep(self)
    else

    if assigned(GetSlot(3)) and assigned(GetSlot(3).FOnNextInitStep)  then
      GetSlot(3).FOnNextInitStep(self)
    else

    if assigned(Performance) and assigned(GetPerformance.FOnNextInitStep) then
      GetPerformance.FOnNextInitStep(self)
    else

    if assigned(FOnNextInitStep) then
      FOnNextInitStep(self);
  end;
end;

procedure TG2USB.USBProcessSendMessage( ClientSendMessage : TClientSendMessage);
begin
  if not((ClientSendMessage.FClientContext = nil) and not(ClientSendMessage.FMessage.HasResponse)) then begin
    // Only forward responseless messages to clients comming from server (to prevent loopbacks)
    ClientSendMessage.FMessage.Position := 0;
    ProcessSendMessage( ClientSendMessage.FMessage, ClientSendMessage.FMessageSender.ID);
  end;

  ServerBroadCastSendMessage( ClientSendMessage);
end;

procedure TG2USB.USBProcessResponseMessage( ResponseMessage : TG2ResponseMessage);
begin
  if not( ResponseMessage.IsLedData) or FLogLedDataMessages then begin
    add_log_line( '', LOGCMD_NUL);
    add_log_line( 'Broadcast : ', LOGCMD_NUL);
    dump_buffer( PStaticByteBuffer( ResponseMessage.Memory)^, ResponseMessage.Size);
  end;

  if ResponseMessage.IsEmbedded then
    ResponseMessage.Position := 1 // Skip first byte if embedded
  else
    ResponseMessage.Position := 0;

  ProcessResponseMessage( ResponseMessage, 0);

  ServerBroadCastResponseMessage( ResponseMessage);
end;

////////////////////////////////////////////////////////////////////////////////
// Communication between Server and client
////////////////////////////////////////////////////////////////////////////////

function TG2USB.GetID : integer;
begin
  Result := FClientID.ID;
end;

procedure TG2USB.IdTCPServerConnect( AContext: TIdContext);
var LClientContext: TClientContext;
begin
  LClientContext := TClientContext(AContext);

  add_log_line( 'Client connect : ' + AContext.Binding.PeerIP
              + ', port : ' + IntToStr( AContext.Binding.PeerPort)
              + ', remote host : ' {+ GStack.WSGetHostByAddr(AContext.Binding.PeerIP)}
              + ', type : ' + IntToStr(ord( LClientContext.Client.ClientType)), LOGCMD_NUL); // <-- here
  if assigned(FOnAddClient) then
    FOnAddClient(self, 0);
end;

procedure TG2USB.IdTCPServerDisconnect( AContext: TIdContext);
begin
  add_log_line( 'Client disconnect : ' + AContext.Binding.PeerIP, LOGCMD_NUL);
  if assigned(FOnDeleteClient) then
    FOnDeleteClient(self, 0);
end;

////////////////////////////////////////////////////////////////////////////////
// Client context for server
////////////////////////////////////////////////////////////////////////////////

constructor TClientContext.Create(AConnection: TIdTCPConnection; AYarn: TIdYarn; AList: TThreadList);
begin
  inherited Create(AConnection, AYarn, AList);
  // create the critical section
  FClientContextCriticalSection := TCriticalSection.Create;
end;

destructor TClientContext.Destroy;
begin
  // free and nil critical section
  FreeAndNil(FClientContextCriticalSection);
  inherited;
end;

procedure TClientContext.Lock;
begin
  if assigned(FClientContextCriticalSection) then
    FClientContextCriticalSection.Enter;
end;

procedure TClientContext.ServerSendClientSendMessage( ClientSendMessage : TClientSendMessage);
var LProtocol: TProtocol;
    LBuffer: TMemoryStream;
begin
  // fill protocol variable with zero's
  FillChar( LProtocol, SizeOf(LProtocol), 0);
  LProtocol.Command := cmdSendMessage;
  LProtocol.Sender := ClientSendMessage.FMessageSender;
  LProtocol.MessageType := mdtSendMessage;
  LProtocol.DataSize := ClientSendMessage.FMessage.Size;

  LBuffer := ProtocolToStream(LProtocol);
  // set the length of the buffer to <size of protocol structure> + <message length>
  LBuffer.Size := szProtocol + LProtocol.DataSize;
  // move message to buffer
  Move( ClientSendMessage.FMessage.Memory^, PStaticByteBuffer(LBuffer.Memory)^[szProtocol], LProtocol.DataSize);
  Lock;
  try
    // write the buffer
    Connection.IOHandler.Write(LBuffer);
  finally
    // unlock client
    Unlock;
    LBuffer.Free;
  end;
end;

procedure TClientContext.ServerSendClientResponseMessage( MemStream : TMemoryStream);
var LProtocol: TProtocol;
    LBuffer: TMemoryStream;
begin
  // fill protocol variable with zero's
  InitProtocol(LProtocol);
  LProtocol.Command := cmdSendMessage;
  LProtocol.Sender := FClient;
  LProtocol.MessageType := mdtResponseMessage;
  LProtocol.DataSize := MemStream.Size;
  //LBuffer := ProtocolToBytes(LProtocol);
  LBuffer := ProtocolToStream(LProtocol);
  // set the length of the buffer to <size of protocol structure> + <message length>
  LBuffer.Size := szProtocol + LProtocol.DataSize;
  // move message to buffer
  Move( MemStream.Memory^, PStaticByteBuffer(LBuffer.Memory)^[szProtocol], LProtocol.DataSize);
  Lock;
  try
    // write the buffer
    Connection.IOHandler.Write(LBuffer);
  finally
    // unlock client
    Unlock;
    LBuffer.Free;
  end;
end;

procedure TClientContext.Unlock;
begin
  if assigned(FClientContextCriticalSection) then
    FClientContextCriticalSection.Leave;
end;

////////////////////////////////////////////////////////////////////////////////
// Server listening thread
//
// Listening en responding to messages received from clients
////////////////////////////////////////////////////////////////////////////////

procedure TG2USB.IdTCPServerExecute( AContext: TIdContext);
var
  LBuffer: TMemoryStream;
  LDataSize: Integer;
  LProtocol: TProtocol;
  LClientContext: TClientContext; // we need to HARD CAST AContext to TClientContext in order to access our custom methods(procedures)
  ClientSendMessage : TClientSendMessage;
  MyNotify: TG2ProcessClientSendMessage;
  MemStream : TMemoryStream;
begin
  // hard cast AContext to TClientContext
  LClientContext := TClientContext(AContext);
  LDataSize := LClientContext.Connection.IOHandler.InputBuffer.Size;
  if LDataSize >= szProtocol then begin
    try
      LBuffer := TMemoryStream.Create;
      LClientContext.Connection.IOHandler.ReadStream( LBuffer, szProtocol);
      LProtocol := StreamToProtocol(LBuffer);
      // check client command and act accordingly
      case LProtocol.Command of
        cmdConnect:
          begin
            add_log_line('<connected> : ' + IntToStr(LProtocol.Sender.ID), LOGCMD_NUL);
            LClientContext.Client := LProtocol.Sender;
          end;
        cmdDisconnect:
          begin
            add_log_line('<disconnected> %s' + IntToStr(LProtocol.Sender.ID), LOGCMD_NUL);
          end;
        cmdSendMessage :
          begin
            MyNotify := TG2ProcessClientSendMessage.Create;
            MyNotify.FG2 := self;
            MyNotify.FBuffer := TMemoryStream.Create;
            LClientContext.Connection.IOHandler.ReadStream(MyNotify.FBuffer, LProtocol.DataSize);
            MyNotify.FClient := LProtocol.Sender;
            MyNotify.FClientContext := LClientContext;
            MyNotify.Notify;
          end;
      end;
    finally
      LBuffer.Free;
    end;
  end else
    sleep(10);
end;

procedure TG2ProcessClientSendMessage.DoNotify;
var ClientSendMessage : TClientSendMessage;
begin
  ClientSendMessage := TClientSendMessage.Create;
  ClientSendMessage.FClientContext := FClientContext;
  ClientSendMessage.FMessageSender := FClient;
  ClientSendMessage.FMessage := TG2SendMessage.Create;
  ClientSendMessage.FMessage.Clear;
  ClientSendMessage.FMessage.Size := FBuffer.Size;
  Move( FBuffer.Memory^, ClientSendMessage.FMessage.Memory^, FBuffer.Size);
  FBuffer.Free;
  FG2.ServerProcessClientMessage( ClientSendMessage);
end;

procedure TG2USB.ServerBroadcastResponseMessage( ResponseMessage : TG2ResponseMessage);
var i : integer;
    LClients: TList;
    LClientContext: TClientContext;
begin
  if not assigned(FIdTCPServer) then
    exit;

  // Send message to the connected clients
  LClients := FIdTCPServer.Contexts.LockList;
  try
    for i := 0 to LClients.Count -1 do begin
      LClientContext := TClientContext(LClients[i]);
      // Don't send led data to vst's
      if not( ResponseMessage.IsLedData and (LClientContext.Client.ClientType = ctVST)) then begin

        LClientContext.Lock;
        try
          LClientContext.ServerSendClientResponseMessage( ResponseMessage);
        finally
          LClientContext.Unlock;
        end;
      end;
    end;
  finally
    // unlock client list
    FIdTCPServer.Contexts.UnlockList;
  end;
end;

procedure TG2USB.ServerBroadcastSendMessage( ClientSendMessage : TClientSendMessage);
var i : integer;
    LClients: TList;
    LClientContext: TClientContext;
begin
  if not assigned(FIdTCPServer) then
    exit;

  // Send message to the connected clients
  LClients := FIdTCPServer.Contexts.LockList;
  try
    for i := 0 to LClients.Count -1 do begin
      LClientContext := TClientContext(LClients[i]);

      LClientContext.Lock;
      try
        LClientContext.ServerSendClientSendMessage( ClientSendMessage);
      finally
        LClientContext.Unlock;
      end;
    end;
  finally
    // unlock client list
    FIdTCPServer.Contexts.UnlockList;
  end;
end;

function TG2USB.GetClientCount: integer;
var LClients: TList;
    i : integer;
begin
  if not assigned(FIdTCPServer) then
    exit;

  Result := 0;
  if assigned(FIdTCPServer) then begin
    LClients := FIdTCPServer.Contexts.LockList;
    try
      for i := 0 to LClients.Count - 1 do
        if TClientContext(LClients[i]).Connection.Connected then
          inc(Result);
    finally
      FIdTCPServer.Contexts.UnlockList;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// Client listening thread
//
// Listening and responding to messages send from server
////////////////////////////////////////////////////////////////////////////////

constructor TIdClientReadThread.Create(AG2: TG2USB);
begin
  inherited Create(False);
  FG2 := AG2;
  FreeOnTerminate := False;
  LMessage := TMemoryStream.Create;
end;

destructor TIdClientReadThread.Destroy;
begin
  LMessage.Free;
  inherited;
end;

procedure TG2USB.Lock;
begin
  FClientCriticalSection.Enter;
end;

procedure TG2USB.Unlock;
begin
  FClientCriticalSection.Leave;
end;

procedure TIdClientReadThread.Run;
var
  LBuffer : TMemoryStream;
  LDataSize: Integer;
begin
  FG2.idTCPClient.IOHandler.CheckForDataOnSource(50);
  if not FG2.idTCPClient.IOHandler.InputBufferIsEmpty then begin
    LBuffer := TMemoryStream.Create;
    try
      LDataSize := FG2.idTCPClient.IOHandler.InputBuffer.Size;

      if LDataSize >= szProtocol then begin
        FG2.Lock;
        try
          FG2.idTCPClient.IOHandler.ReadStream( LBuffer, szProtocol);
        finally
          FG2.Unlock;
        end;

        move( LBuffer.Memory^, LProtocol, szProtocol);
        // check the command, only one at the moment
        case LProtocol.Command of
          cmdSendMessage :
            begin
              LMessage.Clear;
              // Process the message received from the server
              // Synchronize doesn't work with dll's, so a critical section is used (lock)
              FG2.Lock;
              try
                FG2.idTCPClient.IOHandler.ReadStream(LMessage, LProtocol.DataSize);
              finally
                FG2.Unlock;
              end;
              // Don't us the critical section around these procedures, because
              // they can send messages back to the server in the same critical section.
              case LProtocol.MessageType of
                mdtSendMessage     : DoClientProcessServerSendMessage;
                mdtResponseMessage : DoClientProcessServerResponseMessage;
              end;
            end;
        end;
      end;
    finally
      LBuffer.Free;
    end;
  end;
  FG2.idTCPClient.IOHandler.CheckForDisconnect;
end;

procedure TIdClientReadThread.DoClientProcessServerResponseMessage;
begin
  FG2.ClientProcessServerResponseMessage( LMessage)
end;

procedure TIdClientReadThread.DoClientProcessServerSendMessage;
begin
  FG2.ClientProcessServerSendMessage( LMessage, LProtocol.Sender.ID)
end;

procedure TG2USB.ClientProcessServerResponseMessage( MemStream : TMemoryStream);
var Cmd, b : byte;
begin
  // Client receives a response message from server
  MemStream.Position := 0;

  if PStaticByteBuffer(MemStream.Memory)^[0] = CMD_INIT then
    CMD := CMD_INIT
  else
    if (PStaticByteBuffer(MemStream.Memory)^[0] and $f) = 2 then begin
      CMD := PStaticByteBuffer(MemStream.Memory)^[2];  // Embedded
      MemStream.Read(b, 1); // skip first byte;
    end else
      CMD := PStaticByteBuffer(MemStream.Memory)^[1]; // Extended

  //add_log_line( 'Client received message ' + IntToHex( Cmd, 2), LOGCMD_NUL);

  try
    ProcessResponseMessage( MemStream, 0);
  except on E:Exception do
    add_log_line( DateTimeToStr(now) + E.Message, LOGCMD_ERR);
  end;

  // Is this what we've been waiting for?
  if Cmd = FWaitforCmd then begin
    FWaitforCmd := 0;

    if assigned(FOnReceiveResponseMessage) then
      FOnReceiveResponseMessage( Self, MemStream);

    // Still connected?
    if FidTCPClient.Connected then begin

      // Send next message in a sequence, if assigned
      if assigned(GetSlot(0)) and assigned(GetSlot(0).FOnNextInitStep)  then
        GetSlot(0).FOnNextInitStep(self)
      else

      if assigned(GetSlot(1)) and assigned(GetSlot(1).FOnNextInitStep)  then
        GetSlot(1).FOnNextInitStep(self)
      else

      if assigned(GetSlot(2)) and assigned(GetSlot(2).FOnNextInitStep)  then
        GetSlot(2).FOnNextInitStep(self)
      else

      if assigned(GetSlot(3)) and assigned(GetSlot(3).FOnNextInitStep)  then
        GetSlot(3).FOnNextInitStep(self)
      else

      if assigned(Performance) and assigned(GetPerformance.FOnNextInitStep) then
        GetPerformance.FOnNextInitStep(self)
      else

      if assigned(FOnNextInitStep) then
        FOnNextInitStep(self);
    end;
  end;
end;

procedure TG2USB.ClientProcessServerSendMessage( MemStream : TMemoryStream; SenderID : integer);
var Cmd, b : byte;
begin
  // Client receives a send message
  MemStream.Position := 0;
  try
    ProcessSendMessage( MemStream, SenderID);
  except on E:Exception do
    add_log_line( DateTimeToStr(now) + E.Message, LOGCMD_ERR);
  end;
end;

procedure TG2USB.ClientSendConnectedToServer;
var
  LBuffer: TMemoryStream;
  LProtocol: TProtocol;
begin
  InitProtocol(LProtocol);
  LProtocol.Command := cmdConnect;
  LProtocol.Sender := FClientID;
  LBuffer := ProtocolToStream(LProtocol);
  FClientCriticalSection.Enter;
  try
    FidTCPClient.IOHandler.Write(LBuffer);
  finally
    LBuffer.Free;
    FClientCriticalSection.Leave;
  end;
end;

procedure TG2USB.ClientSendMessageToServer( MemStream : TMemoryStream);
var
  LBuffer: TMemoryStream;
  LProtocol: TProtocol;
begin
  // Client sending message to server

  if not( assigned(FIdTCPClient) and FIdTCPClient.Connected) then
    exit;

  InitProtocol(LProtocol);
  LProtocol.Command := cmdSendMessage;
  LProtocol.MessageType := mdtSendMessage;
  LProtocol.Sender := FClientID;
  LProtocol.DataSize := MemStream.Size;
  //LBuffer := ProtocolToBytes(LProtocol);
  LBuffer := ProtocolToStream(LProtocol);
  Lock;
  try
    LBuffer.Size := szProtocol + LProtocol.DataSize;
    Move(PStaticByteBuffer(MemStream.Memory)^[0], PStaticByteBuffer(LBuffer.Memory)^[szProtocol], LProtocol.DataSize);
    FIdTCPClient.IOHandler.Write(LBuffer);
  finally
    LBuffer.Free;
    Unlock;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//                  ACTIVATE/DEACTIVATE COMMUNICATION
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

function TG2USB.GetUSBActive: boolean;
begin
  if FIsServer then
    Result := Assigned(g2udev)
  else
    if assigned(FIdTCPClient) then
      Result := FIdTCPClient.Connected
    else
      Result := False;
end;

procedure TG2USB.SetUSBActive(const Value: boolean);
var Start, Duration : integer;
    ExitCode : DWord;
begin
  if Value then begin

    // Only a server can have an USB connection
    if FIsServer then begin

      add_log_line( 'g2udev handle = ' + IntToHex(integer(g2udev), 4), LOGCMD_NUL);
      if assigned(g2udev) then
        Done;

      if assigned(FIdTCPClient) then begin
        FIdTCPClient.Disconnect;
        FIdTCPClient.Free;
        FIdTCPClient := nil;
      end;

      // Create server object
      FIdTCPServer := TIdTCPServer.Create( Self);
      FIdTCPServer.OnExecute := IdTCPServerExecute;
      FIdTCPServer.Bindings.DefaultPort := FPort;
      // set the context class of the server to our custom TClientContext
      // the server will create automatically our class instance
      // when a client is connected and free it when it disconnects
      FIdTCPServer.ContextClass := TClientContext;
      FIdTCPServer.OnConnect := IdTCPServerConnect;
      FIdTCPServer.OnDisconnect := IdTCPServerDisconnect;
      FIdTCPServer.Active := True;

      add_log_line( 'initializing usb.', LOGCMD_NUL);
      Init;
      add_log_line( 'g2udev handle = ' + IntToHex(integer(g2udev), 4), LOGCMD_NUL);

      // Start the message thread only if there's a USB connection
      if assigned(g2udev) then begin
        add_log_line( 'starting message threads.', LOGCMD_NUL);
        FReceiveMessageThread := TReceiveMessageThread.Create(False, self);
        FReceiveMessageThreadHandle := FReceiveMessageThread.Handle;

        FSendMessageThread := TSendMessageThread.Create(False, self);
        FSendMessageThreadHandle := FSendMessageThread.Handle;
      end;

      if assigned(g2udev) then begin
        if assigned( FOnUSBActiveChange) then
          FOnUSBActiveChange( self, True);

        // Start the G2 initialization for the server
        USBStartInit;
      end;
    end else begin
      // Client connects

      if assigned( FIdTCPServer) then begin
        FIdTCPServer.Active := False;
        FIdTCPServer.Free;
        FIdTCPServer := nil;
      end;

      FClientID.ID := GetTickCount; // set the user ID to connected time
      FClientID.ClientType := ClientType;

      FIdTCPClient := TIdTCPClient.Create( self);

      FIdTCPClient.Port := FPort;
      FIdTCPClient.Host := FHost;
      FIdTCPClient.Connect; // attempt connection


      if FIdTCPClient.Connected then begin
        // if we are connected, create a listener thread instance
        FIdClientReadThread := TIdClientReadThread.Create(self);

        sleep(250); // Wait for listening thread to start

        add_log_line( 'Connect to server ' + FHost + '.', LOGCMD_NUL);
        ClientSendConnectedToServer;

        if assigned( FOnUSBActiveChange) then
          FOnUSBActiveChange( self, True);

        // Start the G2 initialization for the client
        USBStartInit;
      end;
    end;
  end else begin

    // Disconnect
    if assigned(g2udev) then begin
      if FInitialized then
        SendStartStopCommunicationMessage( 1);
      FInitialized := False;
    end;

    if (not FIsServer) and assigned(FIdTCPCLient) and FIdTCPCLient.Connected then begin
      FIdTCPCLient.Disconnect;  // disconnect client
      FIdClientReadThread.WaitFor;
    end;

    // Terminate the USB message thread
    try
      if assigned( FSendMessagethread) then begin
        FSendMessageThread.Terminate;
        WaitForSingleObject( FSendMessageThreadHandle, 3000);
        FSendMessageThread := nil;
      end;
    except on E:Exception do
      add_log_line( E.Message, LOgCMD_ERR);
    end;

    try
      if assigned( FReceiveMessageThread) then begin
        FReceiveMessageThread.Terminate;
      end;
    except on E:Exception do
      add_log_line( E.Message, LOgCMD_ERR);
    end;

    if assigned(g2udev) then begin
      // Clean up usb connection
      Done;
    end;

    if assigned( FReceiveMessageThread) and not(FReceiveMessageThread.Terminated) then begin
      WaitForSingleObject( FReceiveMessageThreadHandle, 3000);
      FReceiveMessageThread := nil;
    end;

    if assigned(FIdTCPServer) then begin
      FIdTCPServer.Active := False;
      FIdTCPServer.Free;
      FIdTCPServer := nil;
    end;

    if assigned(FIdTCPClient) then begin
      FIdTCPCLient.Free;
      FIdTCPCLient := nil;
    end;

    if assigned( FOnUSBActiveChange) then
      FOnUSBActiveChange( self, False);
  end;
end;

function TG2USB.SendCmdMessage( SendMessage : TG2SendMessage): boolean;
var res : integer;
    Start, Duration : integer;
    ClientSendMessage : TClientSendMessage;
    MessageList : TList;
    SendStream : TMemoryStream;
    Cmd : byte;
begin
   // Send message from server to G2 or from client to server

   Result := True;
   FErrorMessage := False;
   FErrorMessageNo := 0;

   add_log_line( '', LOGCMD_NUL);
   add_log_line( 'Send message, size = ' + IntToStr(SendMessage.Size) , LOGCMD_NUL);
   dump_buffer( SendMessage.Memory^, SendMessage.Size);

  if FIsServer then begin

    // Online?
    if assigned(g2udev) then begin

      // Server, send message over USB to G2

      if assigned(FOnBeforeSendMessage) then
        FOnBeforeSendMessage( self, ID, SendMessage);

      add_log_line('Send buffer count was ' + IntTostr(FSendMessageCount), LOGCMD_NUL);

      // Add message to send queue. The message is removed after the message is
      // fully processed in broadcast message by the ReceiveMessage thread.
      ClientSendMessage := TClientSendMessage.Create;
      ClientSendMessage.FMessageSender := FClientID;
      ClientSendMessage.FClientContext := nil;
      ClientSendMessage.FMessage := SendMessage;

      MessageList := FSendMessageQueue.LockList;
      try
        MessageList.Add( ClientSendMessage);
        FSendMessageCount := MessageList.Count;
      finally
        FSendMessageQueue.UnlockList;
      end;

      add_log_line('Send buffer count is ' + IntTostr(FSendMessageCount), LOGCMD_NUL);

    end else begin
      // Not online
      ClientSendMessage := TClientSendMessage.Create;
      ClientSendMessage.FMessageSender := FClientID;
      ClientSendMessage.FClientContext := nil;
      ClientSendMessage.FMessage := SendMessage;
      try
        USBProcessSendMessage( ClientSendMessage);
      finally
        SendMessage.Free;
        ClientSendMessage.Free;
      end;
    end;

  end else begin
    // Client

    if not( assigned(FIdTCPClient) and FIdTCPClient.Connected) then begin
      SendMessage.Free;
      exit;
    end;

    FWaitforCmd := SendMessage.Command;

    // Client, send message to server
    if assigned(FIdTCPClient) and FIdTCPClient.Connected then begin

      if assigned(FOnBeforeSendMessage) then
        FOnBeforeSendMessage( self, ID, SendMessage);

      PStaticByteBuffer( SendMessage.Memory)^[0] := SendMessage.Size div 256;
      PStaticByteBuffer( SendMessage.Memory)^[1] := SendMessage.Size mod 256;
      ClientSendMessageToServer( SendMessage);
    end;

    SendMessage.Free;
  end;
end;

function TG2USB.ServerProcessClientMessage( ClientMessage : TClientSendMessage): boolean;
var ResponseMessage : TG2ResponseMessage;
    MessageList : TList;
begin
  // If it's a message the server can answer, then send the responsemessage here
  // only to the client that asked for it, else send it through to the G2.

  ResponseMessage := CreateResponseMessage( ClientMessage.FMessage);

  if ResponseMessage <> nil then begin

    ResponseMessage.CalcCRC;
    ClientMessage.FClientContext.ServerSendClientResponseMessage(ResponseMessage);
    ResponseMessage.Free;
    ClientMessage.FMessage.Free;
    ClientMessage.Free;
  end else begin
    if assigned(g2udev) then begin
      // Online

      if ClientMessage.FMessage.HasResponse then begin
        MessageList := FSendMessageQueue.LockList;
        try
          // Add message to send queue for G2
          ClientMessage.FMessage.Position := 0;
          MessageList.Add( ClientMessage);
          FSendMessageCount := MessageList.Count;
        finally
          FSendMessageQueue.UnlockList;
        end;
      end else begin
        // Put in parameter update table
        ClientMessage.FMessage.Position := 0;
        ServerProcessClientResponselessMessage( ClientMessage);
      end;

    end else begin
      // Offline
      USBProcessSendMessage( ClientMessage);

      ClientMessage.FMessage.Free;
      ClientMessage.Free;
    end;
  end;
end;

function TG2USB.ServerProcessClientResponselessMessage( ClientMessage : TClientSendMessage): boolean;
var Cmd, SubCmd, aVariation, aLocation, aModuleIndex, aParameterIndex,
    aValue, b, bh, bl : byte;
    Size : integer;
begin
  Result := False;

  if ( ClientMessage.FMessage.Size - ClientMessage.FMessage.Position) < 6 then begin
    ClientMessage.FMessage.Position := ClientMessage.FMessage.Size;
    exit;
  end;

  // Read size
  ClientMessage.FMessage.Read( bh, 1);
  ClientMessage.FMessage.Read( bl, 1);
  Size := bh * 256 + bl;

  ClientMessage.FMessage.Read( b, 1); // $01
  ClientMessage.FMessage.Read( Cmd, 1);

  case (Cmd and $0f)  of
  CMD_SYS : exit;
         else
            begin
              ClientMessage.FMessage.Read( b, 1); // Version
              ClientMessage.FMessage.Read( SubCmd, 1);
              case SubCmd of
              S_SET_PARAM :
                    begin
                      ClientMessage.FMessage.Read( aLocation, 1);
                      ClientMessage.FMessage.Read( aModuleIndex, 1);
                      ClientMessage.FMessage.Read( aParameterIndex, 1);
                      ClientMessage.FMessage.Read( aValue, 1);
                      ClientMessage.FMessage.Read( aVariation, 1);
                      ClientMessage.FMessage.Position := ClientMessage.FMessage.Size;
                      case Cmd and $0f of
                      $08 : GetSlot(0).AddParamUpdRec( SubCmd, aLocation, aModuleIndex, aParameterIndex, 0, aValue, 0, aVariation, ClientMessage.FClientContext);
                      $09 : GetSlot(1).AddParamUpdRec( SubCmd, aLocation, aModuleIndex, aParameterIndex, 0, aValue, 0, aVariation, ClientMessage.FClientContext);
                      $0a : GetSlot(2).AddParamUpdRec( SubCmd, aLocation, aModuleIndex, aParameterIndex, 0, aValue, 0, aVariation, ClientMessage.FClientContext);
                      $0b : GetSlot(3).AddParamUpdRec( SubCmd, aLocation, aModuleIndex, aParameterIndex, 0, aValue, 0, aVariation, ClientMessage.FClientContext);
                      end;
                      Result := True;
                    end;
              end;
            end;
  end;
end;

procedure TG2USB.SendOnClientMessage( SendMessage : TMemoryStream);
begin
  PStaticByteBuffer(SendMessage.Memory)^[0] := SendMessage.Size div 256;
  PStaticByteBuffer(SendMessage.Memory)^[1] := SendMessage.Size mod 256;
  ClientSendMessageToServer( SendMessage);
end;

function TG2USB.CreatePerformance: TG2FilePerformance;
begin
  Result := TG2USBPerformance.Create(self);
end;

procedure TG2USB.USBStartInit;
begin
  FInitStep := 1;
  NextBankListCmd.Mode := 0;
  NextBankListCmd.Bank := 0;
  NextBankListCmd.Patch := 0;

  FOnNextInitStep := USBInitSeq;
  SendInitMessage;
end;

procedure TG2USB.USBInitSeq(Sender: TObject);
var do_next_step : boolean;
begin
  do_next_step := true;
  case FInitStep of
  0 : SendInitMessage;
  1 : SendStartStopCommunicationMessage( STOP_COMM);
  2 : SendGetPatchVersionMessage( GetPerformance.FPerfVersion);
  3 : SendGetSynthSettingsMessage;
  4 : SendUnknown1Message;
  5 : GetPerformance.USBStartInit( False);
  6 : if NextBankListCmd.Mode <> 2 then begin
        SendListMessage( NextBankListCmd.Mode, NextBankListCmd.Bank, NextBankListCmd.Patch, FBanks);
        do_next_step := false;
      end else begin
        SendStartStopCommunicationMessage( START_COMM);
        OnNextInitStep := nil;
        FInitialized := True;
        if assigned(FOnAfterG2Init) then
          FOnAfterG2Init(self);
      end;
  end;
  if do_next_step then
    inc(FInitStep);
end;

procedure TG2USB.USBSentStartComm(Sender: TObject);
begin
  // Usually the last step
  SendStartStopCommunicationMessage( START_COMM);
end;

procedure TG2USB.SendInitMessage;
begin
  SendCmdMessage( CreateInitMessage);
end;

procedure TG2USB.SendStartStopCommunicationMessage( Stop : byte);
begin
  SendCmdMessage( CreateStartStopCommunicationMessage(Stop));
end;

procedure TG2USB.SendGetPatchVersionMessage;
begin
  SendCmdMessage( CreateGetPatchVersionMessage);
end;

procedure TG2USB.SendGetSynthSettingsMessage;
begin
  SendCmdMessage( CreateGetSynthSettingsMessage);
end;

procedure TG2USB.SendSetSynthSettingsMessage;
begin
  SendCmdMessage( CreateSetSynthSettingsMessage);
end;

procedure TG2USB.SendUnknown1Message;
begin
  SendCmdMessage( CreateUnknown1Message);
end;

procedure TG2USB.SendDumpMidiMessage;
begin
  SendCmdMessage( CreateMidiDumpMessage);
end;

procedure TG2USB.SendListMessage( aMode, aBank, aPatch : byte; names : TStrings);
begin
  SendCmdMessage( CreateListMessage( aMode, aBank, aPatch, Names));
end;

procedure TG2USB.SendSetModeMessage( aMode : byte);
begin
  SendCmdMessage( CreateSetModeMessage( aMode));
end;

procedure TG2USB.SendNoteMessage( aNote : byte; aOnoff : byte);
begin
  SendCmdMessage( CreateNoteMessage( aNote, aOnOff));
end;

////////////////////////////////////////////////////////////////////////////////
//  TG2USBPerformance
////////////////////////////////////////////////////////////////////////////////

constructor TG2USBPerformance.Create( AOwner: TComponent);
begin
  inherited;

  FPerfVersion := 0;
  FOnNextInitStep := nil;
end;

destructor TG2USBPerformance.Destroy;
begin
  inherited;
end;

function TG2USBPerformance.CreateSlot: TG2FileSlot;
begin
  Result := TG2USBSlot.Create( self);
end;

function TG2USBPerformance.GetSlot( aSlot : byte): TG2USBSlot;
begin
  Result := Slot[aSlot] as TG2USBSlot;
end;

function TG2USBPerformance.SendCmdMessage( SendMessage : TG2SendMessage): boolean;
begin
  if assigned(G2) then
    Result := (G2 as TG2USB).SendCmdMessage( SendMessage)
  else
    Result := False;
end;

procedure TG2USBPerformance.USBStartInit( aStartCommAfterInit : boolean);
begin
  FInitStep := 1;
  FStartCommAfterInit := aStartCommAfterInit;
  FOnNextInitStep := USBInitSeq;
  SendGetPerfSettingsMessage;
end;

procedure TG2USBPerformance.USBInitSeq(Sender: TObject);
begin
  case FInitStep of
   0 : SendGetPerfSettingsMessage;
   1 : SendUnknown2Message;
   2 : GetSlot(0).USBStartInit( False);
   3 : GetSlot(1).USBStartInit( False);
   4 : GetSlot(2).USBStartInit( False);
   5 : GetSlot(3).USBStartInit( False);
   6 : begin
         SendGetGlobalKnobsMessage;
         if not FStartCommAfterInit then
           OnNextInitStep := nil;
       end;
   7 : begin
         (G2 as TG2USB).SendStartStopCommunicationMessage( START_COMM);
          OnNextInitStep := nil;
       end;
  end;
  inc(FInitStep);
end;

procedure TG2USBPerformance.SendGetPerfSettingsMessage;
begin
  SendCmdMessage( CreateGetPerfSettingsMessage);
end;

procedure TG2USBPerformance.SendUnknown2Message;
begin
  SendCmdMessage( CreateUnknown2Message);
end;

procedure TG2USBPerformance.SendSelectSlotMessage( aSlot: byte);
begin
  SendCmdMessage( CreateSelectSlotMessage( aSlot));
end;

procedure TG2USBPerformance.SendRetrieveMessage( aSlot, aBank, aPatch : byte);
begin
  SendCmdMessage( CreateRetrieveMessage( aSlot, aBank, aPatch));
end;

procedure TG2USBPerformance.SendStoreMessage( aSlot, aBank, aPatch : byte);
begin
  SendCmdMessage( CreateStoreMessage( aSlot, aBank, aPatch));
end;

procedure TG2USBPerformance.SendSetPerformanceMessage( aPerfName : AnsiString; aPerf : TG2FilePerformance);
begin
  SendCmdMessage( CreateSetPerformanceMessage( aPerfName, aPerf));
end;

procedure TG2USBPerformance.SendSetPerfSettingsMessage;
begin
  SendCmdMessage( CreateSetPerfSettingsMessage);
end;

procedure TG2USBPerformance.SendSetPerfNameMessage( aPerfName : AnsiString);
begin
  SendCmdMessage( CreateSetPerfNameMessage( aPerfName));
end;

procedure TG2USBPerformance.SendGetGlobalKnobsMessage;
begin
  SendCmdMessage( CreateGetGlobalKnobsMessage);
end;

////////////////////////////////////////////////////////////////////////////////
//  TG2USBSlot
////////////////////////////////////////////////////////////////////////////////

constructor TG2USBSlot.Create( AOwner: TComponent);
begin
  inherited;

  SetLength( FParamUpdBuf, 100);
  FParamUpdBufCount := 0;
end;

destructor TG2USBSlot.Destroy;
begin
  Finalize( FParamUpdBuf);

  inherited;
end;

function TG2USBSlot.CreatePatch: TG2FilePatch;
begin
  Result := TG2USBPatch.Create( self);
end;

function TG2USBSlot.GetPatch : TG2USBPatch;
begin
  if not assigned(Patch) then
    raise Exception.Create('Patch in slot unassigned!');

  Result := Patch as TG2USBPatch;
end;

function TG2USBSlot.GetPerformance : TG2USBPerformance;
begin
  if not assigned(Performance) then
    raise Exception.Create('Performance not assigned to slot.');

  Result := Performance as TG2USBPerformance;
end;


procedure TG2USBSlot.USBStartInit( aStartCommAfterInit : boolean);
begin
  FInitStep := 1;
  FStartCommAfterInit := aStartCommAfterInit;
  FOnNextInitStep := USBInitSeq;
  SendGetPatchVersionMessage;
end;

procedure TG2USBSlot.USBInitSeq(Sender: TObject);
begin
  case FInitStep of
   0 : SendGetPatchVersionMessage;
   1 : SendGetPatchMessage;
   2 : SendGetPatchNameMessage;
   3 : SendCurrentNoteMessage;
   4 : SendPatchNotesMessage;
   5 : SendResourceTableMessage( LOCATION_VA);
   6 : SendResourceTableMessage( LOCATION_FX);
   7 : SendUnknown6Message;
   8 : begin
         SendGetSelectedParameterMessage;
         if not FStartCommAfterInit then
           OnNextInitStep := nil;
       end;
   9 : begin
         (G2 as TG2USB).SendStartStopCommunicationMessage( START_COMM);
         OnNextInitStep := nil;
       end;
  end;
  inc(FInitStep);
end;

function TG2USBSlot.SendCmdMessage( SendMessage : TG2SendMessage): boolean;
begin
  if assigned(G2) then
    Result := (G2 as TG2USB).SendCmdMessage( SendMessage)
  else
    Result := False;
end;

procedure TG2USBSlot.SendGetPatchVersionMessage;
begin
  SendCmdMessage( CreateGetPatchVersionMessage);
end;

procedure TG2USBSlot.SendPatchNotesMessage;
begin
  SendCmdMessage( CreatePatchNotesMessage);
end;

procedure TG2USBSlot.SendControllerSnapshotMessage;
begin
  SendCmdMessage( CreateSendControllerSnapshotMessage);
end;

procedure TG2USBSlot.SendResourceTableMessage( aLocation : Byte);
begin
  SendCmdMessage( CreateResourceTableMessage( aLocation));
end;

procedure TG2USBSlot.SendGetPatchNameMessage;
begin
  SendCmdMessage( CreateGetPatchNameMessage);
end;

procedure TG2USBSlot.SendCurrentNoteMessage;
begin
  SendCmdMessage( CreateCurrentNoteMessage);
end;

procedure TG2USBSlot.SendUnknown6Message;
begin
  SendCmdMessage( CreateUnknown6Message);
end;

procedure TG2USBSlot.SendGetSelectedParameterMessage;
begin
  SendCmdMessage( CreateGetSelectedParameterMessage);
end;

procedure TG2USBSlot.SendSetPatchMessage( aPatchName : AnsiString; aPatch : TG2FilePatch);
begin
  SendCmdMessage( CreateSetPatchMessage( aPatchName, aPatch));
end;

procedure TG2USBSlot.SendGetPatchMessage;
begin
  SendCmdMessage( CreateGetPatchMessage);
end;

procedure TG2USBSlot.SendSelectVariationMessage( aVariationIndex: byte);
begin
  SendCmdMessage( CreateSelectVariationMessage( aVariationIndex));
end;

procedure TG2USBSlot.AddParamUpdRec( aSubCmd, aLocation, aModule, aParam, aMorph, aValue, aNegative, aVariation : byte; aClientContext : TClientContext);
var i : integer;
    SendStream : TG2SendMessage;
begin
try
  if not assigned(G2) then
    exit;

  // Todo : replace by fast search and add sorted
  i := 0;
  while (i < FParamUpdBufCount) and not(( FParamUpdBuf[i].SubCmd = aSubCmd)
                                    and ( FParamUpdBuf[i].Location = aLocation)
                                    and ( FParamUpdBuf[i].Module = aModule)
                                    and ( FParamUpdBuf[i].Param = aParam)
                                    and ( FParamUpdBuf[i].Morph = aMorph)
                                    and ( FParamUpdBuf[i].Variation = aVariation)
                                    {and ( FParamUpdBuf[i].Sender = aSender)}
                                    and ( FParamUpdBuf[i].ClientContext = aClientContext)) do
    inc(i);

  if not(i < FParamUpdBufCount) then begin
    // Not found, add
    if (FParamUpdBufCount + 1) >= Length( FParamUpdBuf) then
      SetLength( FParamUpdBuf, Length( FParamUpdBuf) + 100); // Increase buffersize

    FParamUpdBuf[ FParamUpdBufCount].SubCmd := aSubCmd;
    FParamUpdBuf[ FParamUpdBufCount].Location := aLocation;
    FParamUpdBuf[ FParamUpdBufCount].Module := aModule;
    FParamUpdBuf[ FParamUpdBufCount].Param := aParam;
    FParamUpdBuf[ FParamUpdBufCount].Morph := aMorph;
    FParamUpdBuf[ FParamUpdBufCount].Value := 128; // Clavia max is 127, so this means "unitialized"
    FParamUpdBuf[ FParamUpdBufCount].Negative := 0;
    FParamUpdBuf[ FParamUpdBufCount].Variation := aVariation;
    //FParamUpdBuf[ FParamUpdBufCount].Sender := aSender;
    FParamUpdBuf[ FParamUpdBufCount].ClientContext := aClientContext;
    inc( FParamUpdBufCount);
  end;

  if ((G2 as TG2USB).FIsServer) then begin
    // Server, mark what has to be updated to the G2
    if (FParamUpdBuf[ i].Value <> aValue) or (FParamUpdBuf[ i].Negative <> aNegative) then begin
      FParamUpdBuf[ i].Value := aValue;
      FParamUpdBuf[ i].Negative := aNegative;
      FParamUpdBuf[ i].Changed := True;
    end;
  end else begin
    // Client, send only if value is changed
    if (FParamUpdBuf[ i].Value <> aValue) or (FParamUpdBuf[ i].Negative <> aNegative) then begin
      FParamUpdBuf[ i].Value := aValue;
      FParamUpdBuf[ i].Negative := aNegative;

      SendStream := nil;
      case aSubCmd of
        S_SEL_PARAM       : SendStream := CreateSelParamMessage( FParamUpdBuf[ i].Location, FParamUpdBuf[ i].Module, FParamUpdBuf[ i].Param);
        S_SET_PARAM       : SendStream := CreateSetParamMessage( FParamUpdBuf[ i].Location, FParamUpdBuf[ i].Module, FParamUpdBuf[ i].Param, FParamUpdBuf[ i].Value, FParamUpdBuf[ i].Variation);
        S_SET_MORPH_RANGE : SendStream := CreateSetMorphMessage( FParamUpdBuf[ i].Location, FParamUpdBuf[ i].Module, FParamUpdBuf[ i].Param, FParamUpdBuf[ i].Morph, FParamUpdBuf[ i].Value, FParamUpdBuf[ i].Negative, FParamUpdBuf[ i].Variation);
      end;

      if assigned(SendStream) then
        (G2 as TG2USB).SendOnClientMessage( SendStream);

    end;
  end;
except on E:Exception do begin
    G2.add_log_line( 'AddParamUpdRec, FParamUpdBufCount ' + IntToStr(FParamUpdBufCount) + ', i = ' + IntToStr(i) + ', Length buf ' + IntToStr(Length(FParamUpdBuf)), LOGCMD_NUL);
    G2.save_log;
  end;
end;

end;

procedure TG2USBSlot.SendSetParamMessage( aLocation, aModule, aParam, aValue, aVariation: byte);
begin
  // This gives eventually access violation errors in ableton vst (could be very many log messages!)
  //add_log_line('Set param, location ' + IntToStr(aLocation) + ', module ' + IntToStr(aModule) + ', param ' + IntToStr(aParam) + ', value ' + IntToStr(aValue) + ', variation ' + IntToStr(aVariation) {+ ', slot ' + IntToStr(SlotIndex) + ', patch_version ' + IntToStr(FPatchVersion)}, LOGCMD_HDR);
try
  AddParamUpdRec( S_SET_PARAM, aLocation, aModule, aParam, 0, aValue, 0, aVariation, nil);
  except on E:Exception do begin
      G2.add_log_line( E.Message + ' USBSetParameter, '// SlotIndex = '+ IntToStr(Int64(SlotIndex))
                       + ', Location = ' + IntToStr(ord(aLocation))
                       + ', ModuleIdnex = ' + IntToStr(aModule)
                       + ', ParamIndex = ' + IntToStr(aParam)
                       + ', Value = ' + IntToStr(aValue)
                       + ', Variation = ' + IntToStr(aVariation), LOGCMD_NUL);
      G2.save_log;
    end;
  end;
end;

procedure TG2USBSlot.SendSelParamMessage( aLocation, aModule, aParam: integer);
var MemStream : TMemoryStream;
begin
  add_log_line('Select param, module ' + IntToStr(aModule) + ', param ' + IntToStr(aParam) + ', slot ' + IntToStr(SlotIndex) + ', patch_version ' + IntToStr(FPatchVersion), LOGCMD_HDR);

  AddParamUpdRec( S_SEL_PARAM, aLocation, aModule, aParam, 0, 0, 0, 0, nil);
end;

procedure TG2USBSlot.SendSetMorphMessage( aLocation, aModule, aParam, aMorph, aValue, aNegative, aVariation: byte);
var MemStream : TMemoryStream;
begin
  add_log_line('Set morph, location ' + IntToStr(aLocation) + ', module ' + IntToStr(aModule) + ', param ' + IntToStr(aParam) + ', morph ' + IntToStr(aMorph) + ', value ' + IntToStr(aValue) + ', negative ' + IntToStr(aNegative) + ', variation ' + IntToStr(aVariation) + ', slot ' + IntToStr(SlotIndex) + ', patch_version ' + IntToStr(FPatchVersion), LOGCMD_HDR);

  AddParamUpdRec( S_SET_MORPH_RANGE, aLocation, aModule, aParam, aMorph, aValue, aNegative, aVariation, nil);
end;

procedure TG2USBSlot.SendSetModeMessage( aLocation, aModule, aParam, aValue: integer);
begin
  SendCmdMessage( CreateSetModeMessage( aLocation, aModule, aParam, aValue));
end;

procedure TG2USBSlot.SendCopyVariationMessage( aFromVariation, aToVariation : byte);
begin
  SendCmdMessage( CreateCopyVariationMessage( aFromVariation, aToVariation));
end;

////////////////////////////////////////////////////////////////////////////////
//  TG2USBPatch
////////////////////////////////////////////////////////////////////////////////

constructor TG2USBPatch.Create( AOwner: TComponent);
begin
  inherited;
end;

destructor TG2USBPatch.Destroy;
begin
  inherited;
end;

function TG2USBPatch.GetPerformance : TG2USBPerformance;
begin
  if not assigned(Slot) then
    raise Exception.Create('Slot not assigned to patch.');

  Result := (Slot as TG2USBSlot).GetPerformance;
end;


function TG2USBPatch.SendCmdMessage( SendMessage : TG2SendMessage): boolean;
begin
  if assigned(G2) then
    Result := (G2 as TG2USB).SendCmdMessage( SendMessage)
  else
    Result := False;
end;

procedure TG2USBPatch.SendUndoMessage;
var MemStream : TG2SendMessage;
begin
  if FUndoStack.Count > 0 then begin
    // Send over usb
    MemStream := PopUndoStack;
    (G2 as TG2USB).SendCmdMessage( MemStream);
  end;
end;

function TG2USBPatch.MessSetPatchDescription( FPatchDescription : TPatchDescription): boolean;
var MemStream : TG2SendMessage;
begin
  MemStream := CreateSetPatchDescriptionMessage( FPatchDescription);
  Result := SendCmdMessage( MemStream);
end;

function TG2USBPatch.MessAddModule( aLocation : TLocationType; aModuleType, aCol, aRow: byte): boolean;
var MemStream : TG2SendMessage;
    aModuleIndex : Byte;
begin
  Result := inherited MessAddModule( aLocation, aModuleType, aCol, aRow);

  // Get a new module index
  aModuleIndex := GetMaxModuleIndex( aLocation) + 1;

  // Send over usb
  MemStream := CreateAddNewModuleMessage( aLocation, aModuleIndex, aModuleType, aCol, ARow);
  Result := SendCmdMessage( MemStream);
end;

function TG2USBPatch.MessCopyModules( aSrcePatch : TG2FilePatchPart; aFromLocation, aToLocation : TLocationType): boolean;
var MemStream : TG2SendMessage;
begin
  Result := inherited MessCopyModules( aSrcePatch, aFromLocation, aToLocation);

  MemStream := CreateCopyModulesMessage( aSrcePatch, aFromLocation, aToLocation, True);
  Result := SendCmdMessage( MemStream);
end;

function TG2USBPatch.MessMoveModules: boolean;
var MemStream : TG2SendMessage;
begin
  MemStream := CreateMoveModulesMessage;
  Result := SendCmdMessage( MemStream);
end;

function TG2USBPatch.MessDeleteModule( aLocation : TLocationType; aModuleIndex : byte): boolean;
var MemStream : TG2SendMessage;
begin
  MemStream := CreateDeleteModuleMessage( aLocation, aModuleIndex);
  Result := SendCmdMessage( MemStream);
end;

function TG2USBPatch.MessDeleteModules( aLocation : TLocationType): boolean;
var MemStream : TG2SendMessage;
begin
  MemStream := CreateDeleteModulesMessage( aLocation);
  Result := SendCmdMessage( MemStream);
end;

function TG2USBPatch.MessSetModuleColor( aLocation: TLocationType; aModuleIndex, aColor : byte): boolean;
var MemStream : TG2SendMessage;
begin
  MemStream := CreateSetModuleColorMessage( aLocation, aModuleIndex, aColor);
  Result := SendCmdMessage( MemStream);
end;

function TG2USBPatch.MessAddConnection( aLocation : TLocationType; aFromConnector, aToConnector : TG2FileConnector): boolean;
var MemStream : TG2SendMessage;
begin
  MemStream := CreateAddConnectionMessage( aFromConnector, aToConnector);
  Result := SendCmdMessage( MemStream);
end;

function TG2USBPatch.MessDeleteConnection( aLocation : TLocationType; aCable: TG2FileCable): boolean;
var MemStream : TG2SendMessage;
begin
  MemStream := CreateDeleteConnectionMessage( aCable);
  Result := SendCmdMessage( MemStream);
end;

function TG2USBPatch.MessAssignKnob(aLocation : TLocationType; aModule, aParam, aKnob: integer): boolean;
var MemStream : TG2SendMessage;
begin
  MemStream := CreateAssignKnobMessage( aLocation, aModule, aParam, aKnob);
  Result := SendCmdMessage( MemStream);
end;

function TG2USBPatch.MessDeassignKnob(aKnob: integer): boolean;
var MemStream : TG2SendMessage;
    Knob : TKnob;
begin
  Knob := GetKnob( aKnob);
  if not assigned(Knob) then
    raise Exception.Create('Knob ' + IntToStr(aKnob) + ' not found.');

  MemStream := CreateDeassignKnobMessage( aKnob);
  Result := SendCmdMessage( MemStream);
end;

function TG2USBPatch.MessAssignMidiCC(aLocation: TLocationType; aModule, aParam, aMidiCC: integer): boolean;
var MemStream : TG2SendMessage;
begin
  MemStream := CreateAssignMidiCCMessage( aLocation, aModule, aParam, aMidiCC);
  Result := SendCmdMessage( MemStream);
end;

function TG2USBPatch.MessDeassignMidiCC( aMidiCC: integer): boolean;
var MemStream : TG2SendMessage;
    Controller : TController;
begin
  Controller := GetMidiCC( aMidiCC);
  if not assigned(Controller) then
    raise Exception.Create('Controller ' + IntToHex(aMidiCC,2) + ' not found.');

  MemStream := CreateDeassignMidiCCMessage( aMidiCC);
  Result := SendCmdMessage( MemStream);
end;

function TG2USBPatch.MessAssignGlobalKnob( aLocation : TLocationType; aModule, aParam, aKnob: integer): boolean;
var MemStream : TG2SendMessage;
begin
  MemStream := CreateAssignGlobalKnobMessage( aLocation, aModule, aParam, aKnob);
  Result := SendCmdMessage( MemStream);
end;

function TG2USBPatch.MessDeassignGlobalKnob(aKnob: integer): boolean;
var MemStream : TG2SendMessage;
    Knob : TGlobalKnob;
    Perf : TG2USBPerformance;
begin
  Perf := GetPerformance;
  if not assigned(Perf) then
    raise Exception.Create('Performance not assigned to patch.');

  Knob := Perf.GetGlobalKnob( aKnob);
  if not assigned(Knob) then
    raise Exception.Create('Knob ' + IntToStr(aKnob) + ' not found.');

  MemStream := CreateDeassignGlobalKnobMessage( aKnob);
  Result := SendCmdMessage( MemStream);
end;

function TG2USBPatch.MessSetModuleParamLabels( aLocation: TLocationType; aModuleIndex, aParamIndex: byte; aName: AnsiString): boolean;
var MemStream : TG2SendMessage;
begin
  MemStream := CreateSetModuleParamLabelsMessage( aLocation, aModuleIndex, aParamIndex, aName);
  Result := SendCmdMessage( MemStream);
end;

function TG2USBPatch.MessSetModuleLabel( aLocation: TLocationType; aModuleIndex: byte; aName: AnsiString): boolean;
var MemStream : TG2SendMessage;
begin
  MemStream := CreateSetModuleLabelMessage( aLocation, aModuleIndex, aName);
  Result := SendCmdMessage( MemStream);
end;

end.