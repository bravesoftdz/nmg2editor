unit UnitG2Editor;

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
//  This unit contains the demo G2 editor
//
//  ////////////////////////////////////////////////////////////////////////////

// Done
// ====
// Select, cut, copy past part of patches
// Undo
// Edit name of modules/parameters
// Choose module color
// Show/hide cables
// Sysex functions
// drag select
// replace socket components by Indy
// assign morphs/patch params to knobs
// short cut keys (keymap + osc map)
// Modules with textedit: initialize parameter label when adding module
// assign volume/patch settings to knob
// Bank/patch manager
// Uprate calculation bug
// Undo delete modules
// Clean up global assign on init/load patch
// ini-file (xml) with some preferences

// TODO List for beta release
// ==========================

// Set parameter functions to g2_file
// Make Set..InPatch functions private
// Auto assign midi
// some of the text functions
// some of the graphic screens
// osc/client server connections screen
// finish basic osc

// TODO List later
// ===============
// Virtual keyboard
// Module text functions
// Module graphic screens
// Multiple g2 support
// Memory used indicator
// Patch mutator
// Patch adjuster

// http://www.delphicorner.f9.co.uk/articles/op1.htm

{$I delphi_version.inc}

interface

uses
{$IFDEF FPC}
  FileUtil, LclIntf,
{$ELSE}
  Windows, XPStyleActnCtrls, ActnMan, ScktComp,
  {$IFDEF G2_VER200_up}
    // Don't know exactly in what version this style was introduced
    Vcl.PlatformDefaultStyleActnCtrls,
  {$ENDIF}
{$ENDIF}
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,  ActnList, ImgList,
  g2_types, g2_database, g2_file, g2_mess, g2_usb, g2_graph, g2_classes,
  graph_util_vcl, Menus, Buttons, DOM, XMLRead, XMLWrite;

type
  TSlotPanel = class(TG2GraphPanel)
  private
    FDisableControls : boolean;
    FSlot            : TG2Slot;
    FlbSlotName      : TG2GraphLabel;
    FePatchName      : TEdit;
    FlbVariation     : TG2GraphLabel;
    FG2btEditAllVars : TG2GraphButtonText;
    FG2rbVariation   : TG2GraphButtonRadio;
    FlbVolume        : TG2GraphLabel;
    FG2kVolume       : TG2GraphKnob;
    FG2btMute        : TG2GraphButtonText;
    FG2dVoices       : TG2GraphDisplay;
    FG2idVoiceMode   : TG2GraphButtonIncDec;
    FG2kMorphArray   : array[0..7] of TG2GraphKnob;
    FG2btMorphArray  : array[0..7] of TG2GraphButtonFlat;
    FpuVariationMenu : TPopupMenu;
    procedure ChangeAlleVariationsClick( Sender: TObject);
  protected
    procedure VariationClick(Sender: TObject);
    procedure VariationMouseUp(Sender: TObject; Button: TMouseButton;  Shift: TShiftState; X, Y: Integer);
    procedure VoiceModeChange(Sender: TObject);
    procedure MorphMouseUp(Sender: TObject; Button: TMouseButton;  Shift: TShiftState; X, Y: Integer);
    procedure SetSlotCaption( aValue : string);
    function  GetSlotCaption: string;
    function  GetSlotIndex: integer;
  public
    constructor Create( AOwner : TComponent; aSlot : TG2Slot);
    destructor Destroy; override;
    procedure  UpdateControls;

    property VariationMenu : TPopupMenu read FpuVariationMenu write FPuVariationMenu;
    property SlotCaption : string read GetSlotCaption write SetSlotCaption;
    property SlotIndex : integer read GetSlotIndex;
  end;

  TfrmG2Main = class(TForm)
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Uploadpatch1: TMenuItem;
    View1: TMenuItem;
    Patchsettings1: TMenuItem;
    cbMode: TCheckBox;
    Splitter1: TSplitter;
    puAddModule: TPopupMenu;
    cbOnline: TCheckBox;
    PerfPanel: TPanel;
    sbFX: TG2GraphScrollBox;
    G2: TG2;
    sbVA: TG2GraphScrollBox;
    cbLogMessages: TCheckBox;
    StartupTimer: TTimer;
    Initpatch1: TMenuItem;
    Parameterpages1: TMenuItem;
    puParamMenu: TPopupMenu;
    Defaultvalue1: TMenuItem;
    miMorphAssign: TMenuItem;
    miEditParamName: TMenuItem;
    N1: TMenuItem;
    miAssignKnob: TMenuItem;
    miAssignGlobalKnob: TMenuItem;
    MIDIController1: TMenuItem;
    N2: TMenuItem;
    Help1: TMenuItem;
    puModuleMenu: TPopupMenu;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    Delete1: TMenuItem;
    puVariationMenu: TPopupMenu;
    Copyto11: TMenuItem;
    Copyto21: TMenuItem;
    Copyto31: TMenuItem;
    Copyto41: TMenuItem;
    Copyto51: TMenuItem;
    Copyto61: TMenuItem;
    Copyto71: TMenuItem;
    Copyto81: TMenuItem;
    N3: TMenuItem;
    Initvariation1: TMenuItem;
    N4: TMenuItem;
    Properties1: TMenuItem;
    Label13: TLabel;
    lbClientsConnected: TLabel;
    miMidiCC: TMenuItem;
    miAssignMidiCC: TMenuItem;
    Deassign1: TMenuItem;
    Loadperformance1: TMenuItem;
    Saveperformance1: TMenuItem;
    Edit1: TMenuItem;
    Copy2: TMenuItem;
    miModuleRename: TMenuItem;
    Undo1: TMenuItem;
    Cut2: TMenuItem;
    Paste2: TMenuItem;
    Delete2: TMenuItem;
    SavePerformanceAsFxb1: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    Exit1: TMenuItem;
    puConnectorMenu: TPopupMenu;
    miDeleteAllCables: TMenuItem;
    miDeleteCable: TMenuItem;
    Editortools1: TMenuItem;
    Comm1: TMenuItem;
    SavePatchAsSysEx: TMenuItem;
    SavePerfAsSysEx: TMenuItem;
    N11: TMenuItem;
    ilModules: TImageList;
    ActionManager1: TActionManager;
    aDelete: TAction;
    aUndo: TAction;
    aRedo: TAction;
    aCut: TAction;
    aCopy: TAction;
    aPaste: TAction;
    aSelectAll: TAction;
    N12: TMenuItem;
    N13: TMenuItem;
    Selectall1: TMenuItem;
    aPatchManager: TAction;
    Patchmanager1: TMenuItem;
    aParameterPages: TAction;
    aSynthSettings: TAction;
    aPerformanceSettings: TAction;
    aPatchSettings: TAction;
    aSettings: TAction;
    aEditTools: TAction;
    aInitPatch: TAction;
    aLoadPatch: TAction;
    aSavePatch: TAction;
    aSavePatchAsFXP: TAction;
    aSavePatchAsSysex: TAction;
    aLoadPerformance: TAction;
    aSavePerformance: TAction;
    aSavePerformanceAsSysEx: TAction;
    aSavePerformanceAsFXB: TAction;
    aLoadIniXML: TAction;
    aSaveIniXML: TAction;
    aExit: TAction;
    Loadini1: TMenuItem;
    Saveini1: TMenuItem;
    N14: TMenuItem;
    ResponseTimer: TTimer;
    Settings2: TMenuItem;
    Settings3: TMenuItem;
    Synthsettings2: TMenuItem;
    Performancesettings2: TMenuItem;
    aMidiDump: TAction;
    aSendControllerSnapshot: TAction;
    Mididump1: TMenuItem;
    Sendcontrolersnapshot1: TMenuItem;
    aViewLog: TAction;
    Viewlog1: TMenuItem;
    N7: TMenuItem;
    Def1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure cbModeClick(Sender: TObject);
    procedure aPatchSettingsExecute(Sender: TObject);
    procedure aViewLogExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure aInitPatchExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cbLogMessagesClick(Sender: TObject);
    procedure G2VariationChange(Sender: TObject; SenderID: Integer; Slot, Variation: Integer);
    procedure G2USBActiveChange(Sender: TObject; Active: Boolean);
    procedure G2PatchUpdate(Sender: TObject; SenderID: Integer; PatchIndex: Integer);
    procedure StartupTimerTimer(Sender: TObject);
    procedure aParameterPagesExecute(Sender: TObject);
    procedure aDownloadPatchExecute(Sender: TObject);
    procedure G2CreateModule(Sender: TObject; SenderID: Integer; Module: TG2FileModule);
    procedure Delete1Click(Sender: TObject);
    procedure sbFXMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure sbVAMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure G2DeassignKnob(Sender: TObject; SenderID: Integer; Slot : byte; KnobIndex: Integer);
    procedure G2AssignKnob(Sender: TObject; SenderID: Integer; Slot : byte; KnobIndex: Integer);
    procedure VariaionCopytoClick(Sender: TObject);
    procedure Properties1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure G2AddClient(Sender: TObject; ClientIndex: Integer);
    procedure G2DeleteClient(Sender: TObject; ClientIndex: Integer);
    procedure G2MidiCCReceive(Sender: TObject; SenderID: Integer; MidiCC: Byte);
    procedure AssignMidiCC( Sender: TObject);
    procedure DeAssignMidiCC( Sender: TObject);
    procedure AssignMorph( Sender: TObject);
    procedure aSynthSettingsExecute(Sender: TObject);
    procedure G2PerfSettingsUpdate(Sender: TObject; SenderID: Integer; PerfMode: Boolean);
    procedure G2SynthSettingsUpdate(Sender: TObject; SenderID: Integer);
    procedure miEditParamNameClick(Sender: TObject);
    procedure miModuleRenameClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miDeleteCablesClick(Sender: TObject);
    procedure miDeleteAllCablesClick(Sender: TObject);

    procedure G2SelectSlot(Sender: TObject; SenderID: Integer; Slot: Integer);
    procedure Splitter1Moved(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure aUndoExecute(Sender: TObject);
    procedure aRedoExecute(Sender: TObject);
    procedure aCutExecute(Sender: TObject);
    procedure aCopyExecute(Sender: TObject);
    procedure aPasteExecute(Sender: TObject);
    procedure aDeleteExecute(Sender: TObject);
    procedure aSelectAllExecute(Sender: TObject);
    procedure G2AfterG2Init(Sender: TObject);
    procedure aPatchManagerExecute(Sender: TObject);
    procedure aPerformanceSettingsExecute(Sender: TObject);
    procedure aSettingsExecute(Sender: TObject);
    procedure aEditToolsExecute(Sender: TObject);
    procedure G2AfterRetrievePatch(Sender: TObject; SenderID: Integer; aSlot,
      aBank, aPatch: Byte);
    procedure aLoadPatchExecute(Sender: TObject);
    procedure aSavePatchExecute(Sender: TObject);
    procedure aSavePatchAsFXPExecute(Sender: TObject);
    procedure aSavePatchAsSysexExecute(Sender: TObject);
    procedure aLoadPerformanceExecute(Sender: TObject);
    procedure aSavePerformanceExecute(Sender: TObject);
    procedure aSavePerformanceAsSysExExecute(Sender: TObject);
    procedure aSavePerformanceAsFXBExecute(Sender: TObject);
    procedure aExitExecute(Sender: TObject);
    procedure cbOnlineClick(Sender: TObject);
    procedure G2BeforeSendMessage(Sender: TObject; SenderID: Integer;
      SendMessage: TG2SendMessage);
    procedure G2ReceiveResponseMessage(Sender: TObject;
      ResponseMessage: TMemoryStream);
    procedure ResponseTimerTimer(Sender: TObject);
    procedure aSendControllerSnapshotExecute(Sender: TObject);
    procedure aMidiDumpExecute(Sender: TObject);
    procedure Def1Click(Sender: TObject);
    procedure aExtractModuleInfoExecute(Sender: TObject);
  private
    { Private declarations }
    procedure DialogKey(var Msg: TWMKey); message CM_DIALOGKEY;
  public
    { Public declarations }
    FDisableControls     : boolean;
    FSlotPanel           : array[0..3] of TSlotPanel;
    FAddPoint            : TPoint;
    FLocation            : TLocationType;
    FOldSplitterPos      : integer;
    FLastReceivedMidiCC  : byte;
    FCopyPatch           : TG2FilePatchPart;
    procedure AddModule( aModuleType : byte);
    procedure DoAddModule( Sender: TObject);
    procedure AssignKnob( Sender: TObject);
    procedure DeAssignKnob( Sender: TObject);
    procedure AssignGlobalKnob( Sender: TObject);
    procedure DeAssignGlobalKnob( Sender: TObject);
    procedure GetPatchversion;
    procedure UpdateControls;
    procedure SetSelectedModuleColor( aColor : byte);
    function  GetPatchWindowHeight: integer;
    procedure LoadImageMap( aBitmap : TBitmap;aCols, aRows: integer; aImageList : TImageList);

    procedure CreateAddModuleMenu;
    procedure CreateParamMenu;

    procedure CopyPatchSelection;
    procedure DeletePatchSelection;
    procedure PastePatchSelection;
    procedure Undo;
    procedure SelectSlot( aSlotIndex : byte);
    procedure SelectVariation( aSlotIndex, aVariationIndex : byte);
    procedure SelectPatchLocation( aLocation : TLocationType);

    procedure SaveIniXML;
    procedure LoadIniXML;

    procedure PatchCtrlMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ModuleClick( Sender : TObject; Button: TMouseButton; Shift: TShiftState; X,  Y: Integer; Module : TG2FileModule);
    procedure ParameterClick( Sender : TObject; Button: TMouseButton; Shift: TShiftState; X,  Y: Integer; Parameter : TG2FileParameter);
    procedure ConnectorClick( Sender : TObject; Button: TMouseButton; Shift: TShiftState; X,  Y: Integer; Connector : TG2FileConnector);
    procedure PanelClick(Sender: TObject);
  end;

  procedure SetFormPosition( aForm : TForm; aLeft, aTop, aWidth, aHeight : integer);

var
  frmG2Main: TfrmG2Main;

implementation

uses UnitLog, UnitPatchSettings, UnitParameterPages, UnitSeqGrid,
  UnitSynthSettings, UnitPerfSettings, UnitEditLabel, UnitSettings,
  UnitEditorTools, UnitPatchManager, UnitModuleDef;

{$IFNDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}


procedure SetFormPosition( aForm : TForm; aLeft, aTop, aWidth, aHeight : integer);
begin
  // Check if form doesn;t fall of the screen
  Screen.Width;

  if aLeft < 0 then
    aForm.Left := 0
  else
    if aLeft > Screen.Width then
      aForm.Left := Screen.Width - 32
    else
      aForm.Left := aLeft;

  if aTop < 0 then
    aForm.Top := 0
  else
    if aTop > Screen.Height then
      aForm.Top := Screen.Height - 32
    else
      aForm.Top := aTop;

  if aWidth > 0 then
    aForm.Width := aWidth;

  if aHeight > 0 then
    aForm.Height := aHeight;
end;

// ==== TSlotPanel =============================================================

constructor TSlotPanel.Create(AOwner: TComponent; aSlot : TG2Slot);
var i : integer;
begin
  inherited Create( AOwner);

  FDisableControls := True;
  FSlot := aSlot;
  Font.Name := 'Arial';

  Height := 33;

  FlbSlotName := TG2GraphLabel.Create(self);
  FlbSlotName.Parent := self;
  FlbSlotName.Font.Size := 12;
  FlbSlotName.Font.Style := [fsbold];
  FlbSlotName.Font.Color := CL_CLAVIA_BLUE;
  FlbSlotName.Caption := Caption;
  FlbSlotName.SetBounds( 15, 8, 11, 19);
  FlbSlotName.OnClick := frmG2Main.PanelClick;

  FePatchName := TEdit.Create(self);
  FePatchName.ReadOnly := True;
  FePatchName.Parent := self;
  FePatchName.SetBounds( 36, 6, 101, 21);

  FlbVariation := TG2GraphLabel.Create(self);
  FlbVariation.Parent := self;
  FlbVariation.Font.Size := 8;
  FlbVariation.Font.Style := [fsBold];
  FlbVariation.Font.Color := CL_CLAVIA_BLUE;
  FlbVariation.Caption := 'Var';
  FlbVariation.SetBounds( 145, 10, 25, 13);
  FlbVariation.OnClick := frmG2Main.PanelClick;

  FG2rbVariation := TG2GraphButtonRadio.Create(self);
  FG2rbVariation.Parent := self;
  FG2rbVariation.SetBounds( 172, 7, 177, 19);
  FG2rbVariation.HightlightColor := CL_CONTROL_HIGHLIGHT;
  FG2rbVariation.Color := clActiveBorder;
  FG2rbVariation.BorderColor := clBtnShadow;
  FG2rbVariation.Bevel := False;
  FG2rbVariation.Orientation := otHorizontal;
  FG2rbVariation.Font.Style := [fsBold];
  for i := 1 to 8 do
    FG2rbVariation.ButtonText.Add( IntToStr(i));
  FG2rbVariation.ButtonCount := 8;
  FG2rbVariation.OnClick := VariationClick;
  FG2rbVariation.OnMouseUp := VariationMouseUp;

  FG2btEditAllVars := TG2GraphButtonText.Create(self);
  FG2btEditAllVars.Parent := Self;
  FG2btEditAllVars.SetBounds( 355, 8, 35, 18);
  FG2btEditAllVars.ParentColor := False;
  FG2btEditAllVars.BorderColor := clBlack;
  FG2btEditAllVars.Color := clBtnFace;
  FG2btEditAllVars.HightlightColor := CL_CONTROL_HIGHLIGHT;
  FG2btEditAllVars.ButtonText.Add('Edit all');
  FG2btEditAllVars.ButtonText.Add('Edit all');
  FG2btEditAllVars.OnClick := ChangeAlleVariationsClick;

  FlbVolume := TG2GraphLabel.Create(self);
  FlbVolume.Parent := self;
  FlbVolume.ParentColor := True;
  FlbVolume.Font.Size := 8;
  FlbVolume.Font.Style := [fsBold];

  FlbVolume.Font.Color := CL_CLAVIA_BLUE;
  FlbVolume.Caption := 'Vol';
  FlbVolume.SetBounds( 396, 10, 36, 13);
  FlbVolume.OnClick := frmG2Main.PanelClick;

  FG2kVolume := TG2GraphKnob.Create(self);
  FG2kVolume.Parent := Self;
  FG2kVolume.Orientation := otHorizontal;
  FG2kVolume.KnobType := ktSlider;
  FG2kVolume.SetBounds( 421, 10, 123, 15);
  FG2kVolume.OnMouseUp := frmG2Main.PatchCtrlMouseUp;

  FG2btMute := TG2GraphButtonText.Create(self);
  FG2btMute.Parent := Self;
  FG2btMute.SetBounds( 548, 8, 37, 18);
  FG2btMute.ParentColor := False;
  FG2btMute.BorderColor := clBlack;
  FG2btMute.Color := clBtnFace;
  FG2btMute.HightlightColor := CL_CONTROL_HIGHLIGHT;
  FG2btMute.ButtonText.Add('Mute');
  FG2btMute.ButtonText.Add('On');
  FG2btMute.OnMouseUp := frmG2Main.PatchCtrlMouseUp;

  FG2dVoices := TG2GraphDisplay.Create(self);
  FG2dVoices.Parent := Self;
  FG2dVoices.SetBounds( 598, 8, 45, 17);
  FG2dVoices.Lines[0] := '0';
  FG2dVoices.Color := CL_DISPLAY_BACKGRND;

  FG2idVoiceMode := TG2GraphButtonIncDec.Create(self);
  FG2idVoiceMode.Parent := Self;
  FG2idVoiceMode.SetBounds( 643, 8, 28, 17);
  FG2idVoiceMode.HighValue := 32;
  FG2idVoiceMode.LowValue := 0;
  FG2idVoiceMode.Orientation := otHorizontal;
  FG2idVoiceMode.Value := 1;
  FG2idVoiceMode.Bevel := True;
  FG2idVoiceMode.ParentColor := False;
  FG2idVoiceMode.BorderColor := clBlack;
  FG2idVoiceMode.Color := clBtnFace;
  FG2idVoiceMode.HightlightColor := CL_CONTROL_HIGHLIGHT;
  FG2idVoiceMode.OnChange := VoiceModeChange;

  for i := 0 to 7 do begin
    FG2kMorphArray[i] := TG2GraphKnob.Create(self);
    FG2kMorphArray[i].Parent := Self;
    FG2kMorphArray[i].KnobType := ktExtraSmall;
    FG2kMorphArray[i].SetBounds( 696 + i * 40, 2, 18, 18);
    FG2kMorphArray[i].Tag := i;
    if i = (FSlot.Patch as TG2Patch).SelectedMorphIndex then
      FG2kMorphArray[i].Color := CL_KNOB_MORPH_SELECTED
    else
      FG2kMorphArray[i].Color := CL_KNOB_MORPH;
    FG2kMorphArray[i].HighValue := 127;
    FG2kMorphArray[i].LowValue := 0;
    FG2kMorphArray[i].Value := 0;
    FG2kMorphArray[i].OnMouseUp := MorphMouseUp;

    FG2btMorphArray[i] := TG2GraphButtonFlat.Create(self);
    FG2btMorphArray[i].Parent := Self;
    FG2btMorphArray[i].SetBounds( 686 + i * 40, 20, 38, 11);
    FG2btMorphArray[i].ParentColor := False;
    FG2btMorphArray[i].Bevel := True;
    FG2btMorphArray[i].Color := clBtnFace;
    FG2btMorphArray[i].Tag := i + 8;
    FG2btMorphArray[i].ButtonText.Add('knob');
    FG2btMorphArray[i].ButtonText.Add(string(STD_MORPH_NAMES[i]));
    FG2btMorphArray[i].ButtonText.Add('');
  end;

  ParentColor := False;
{$IFDEF FPC}
{$ELSE}
  ParentBackground := False;
{$ENDIF}
  Color := CL_CLAVIA_RED;

  FDisableControls := False;
end;

destructor TSlotPanel.Destroy;
begin
  inherited;
end;

function TSlotPanel.GetSlotCaption: string;
begin
  Result := FlbSlotName.Caption;
end;

procedure TSlotPanel.SetSlotCaption( aValue: string);
begin
  FlbSlotName.Caption := aValue;
end;

function TSlotPanel.GetSlotIndex: integer;
begin
  Result := FSlot.SlotIndex;
end;

procedure TSlotPanel.UpdateControls;
var Variation : TVariation;
    i : integer;
begin
  FDisableControls := True;
  try
    FePatchName.Text     := string(FSlot.PatchName);
    FG2rbVariation.InitValue( FSlot.Patch.ActiveVariation);
    Variation            := FSlot.Patch.PatchSettings.Variations[ FSlot.Patch.ActiveVariation];
    if FSlot.Patch.EditAllVariations then
      FG2btEditAllVars.Value := 1
    else
      FG2btEditAllVars.Value := 0;

    FG2btMute.Parameter  := FSlot.Patch.Parameter[ PATCH_VOLUME, VOLUME_MUTE];
    FG2kVolume.Parameter := FSlot.Patch.Parameter[ PATCH_VOLUME, VOLUME_LEVEL];

    FG2idVoiceMode.InitValue( FSlot.Patch.PatchDescription.VoiceCount + 2 - FSlot.Patch.PatchDescription.MonoPoly - 1);
    FG2dVoices.Line[ 0] := FSlot.G2.TextFunction(0, FG2idVoiceMode.Value, 0, 0);
    for i := 0 to 7 do begin
      FG2kMorphArray[i].Parameter := FSlot.Patch.Parameter[ PATCH_MORPH, i];
      FG2btMorphArray[i].Parameter := FSlot.Patch.Parameter[ PATCH_MORPH, 8 + i];
    end;

    frmPatchSettings.UpdateControls( Variation);
    frmParameterPages.UpdateControls;

    Invalidate;
  finally
    FDisableControls := False;
  end;
end;

procedure TSlotPanel.VariationClick(Sender: TObject);
begin
  if FDisableControls then exit;

  frmG2Main.SelectVariation( FSlot.SlotIndex, FG2rbVariation.Value);
end;


procedure TSlotPanel.ChangeAlleVariationsClick(Sender: TObject);
begin
  if FDisableControls then exit;

  with (Sender as TG2GraphButtonText).Parent as TSlotPanel do begin
    FSlot.Patch.EditAllVariations := (Sender as TG2GraphButtonText).Value = 1;
  end;
end;

procedure TSlotPanel.VariationMouseUp(Sender: TObject; Button: TMouseButton;  Shift: TShiftState; X, Y: Integer);
var P : TPoint;
    i : integer;
begin
  GetCursorPos(P);
  if (Button = mbRight) then begin
    for i := 0 to FpuVariationMenu.Items.Count - 1 do begin
      if FpuVariationMenu.Items[i].Tag = FSlot.Patch.ActiveVariation then
        FpuVariationMenu.Items[i].Enabled := False
      else
        FpuVariationMenu.Items[i].Enabled := True;
    end;
    FpuVariationMenu.Popup( P.X, P.Y);
  end;
end;

procedure TfrmG2Main.VariaionCopytoClick(Sender: TObject);
begin
  case (Sender as TMenuItem).Tag of
  0 : G2.SelectedSlot.SendCopyVariationMessage( G2.SelectedPatch.ActiveVariation, 0);
  1 : G2.SelectedSlot.SendCopyVariationMessage( G2.SelectedPatch.ActiveVariation, 1);
  2 : G2.SelectedSlot.SendCopyVariationMessage( G2.SelectedPatch.ActiveVariation, 2);
  3 : G2.SelectedSlot.SendCopyVariationMessage( G2.SelectedPatch.ActiveVariation, 3);
  4 : G2.SelectedSlot.SendCopyVariationMessage( G2.SelectedPatch.ActiveVariation, 4);
  5 : G2.SelectedSlot.SendCopyVariationMessage( G2.SelectedPatch.ActiveVariation, 5);
  6 : G2.SelectedSlot.SendCopyVariationMessage( G2.SelectedPatch.ActiveVariation, 6);
  7 : G2.SelectedSlot.SendCopyVariationMessage( G2.SelectedPatch.ActiveVariation, 7);
  8 : G2.SelectedSlot.SendCopyVariationMessage( G2.SelectedPatch.ActiveVariation, 8);
  end;
end;

procedure TSlotPanel.MorphMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); // TODO
begin
  if Sender is TG2GraphKnob then begin
    FG2kMorphArray[ (FSlot.Patch as TG2Patch).SelectedMorphIndex].Color := CL_KNOB_MORPH;
    (FSlot.Patch as TG2Patch).SelectedMorphIndex := (Sender as TG2GraphKnob).Tag;
    FG2kMorphArray[ (FSlot.Patch as TG2Patch).SelectedMorphIndex].Color := CL_KNOB_MORPH_SELECTED;

    frmG2Main.ParameterClick( Sender, Button, Shift, X, Y, FG2kMorphArray[(FSlot.Patch as TG2Patch).SelectedMorphIndex].Parameter as TG2GraphParameter);
  end;
end;

procedure TSlotPanel.VoiceModeChange(Sender: TObject);
var FPatchDescription : TPatchDescription;
begin
  FPatchDescription := FSlot.Patch.PatchDescription;
  case FG2idVoiceMode.Value of
  0 : begin // Legato
        FPatchDescription.VoiceCount := 1;
        FPatchDescription.MonoPoly := 2;
      end;
  1 : begin // Mono
        FPatchDescription.VoiceCount := 1;
        FPatchDescription.MonoPoly := 1;
      end;
  2..32 :
      begin
        FPatchDescription.VoiceCount := FG2idVoiceMode.Value - 1;
        FPatchDescription.MonoPoly := 0;
      end;
  end;
  (FSlot.Patch as TG2Patch).MessSetPatchDescription( FPatchDescription);
end;

procedure TfrmG2Main.UpdateControls;
var i : integer;
begin
  for i := 0 to 3 do begin
    if assigned(FSlotPanel[i]) then begin

      if G2.SelectedSlotIndex = i then begin
        FSlotPanel[i].Color := CL_CLAVIA_BLUE;
        FSlotPanel[i].FlbSlotName.Font.Color := CL_CLAVIA_RED;
        FSlotPanel[i].FlbVariation.Font.Color := CL_CLAVIA_RED;
        FSlotPanel[i].FlbVolume.Font.Color := CL_CLAVIA_RED;
      end else begin
        if G2.Slot[i].Enabled = 1 then
          FSlotPanel[i].Color := CL_CLAVIA_RED
        else
          FSlotPanel[i].Color := clGray;
        FSlotPanel[i].FlbSlotName.Font.Color := CL_CLAVIA_BLUE;
        FSlotPanel[i].FlbVariation.Font.Color := CL_CLAVIA_BLUE;
        FSlotPanel[i].FlbVolume.Font.Color := CL_CLAVIA_BLUE;
      end;

      FSlotPanel[i].UpdateControls;
    end;
  end;

  if assigned(frmEditorTools) and frmEditorTools.Visible then
    frmEditorTools.UpdateControls;


  if assigned(frmSeqGrid) and frmSeqGrid.Visible then
    frmSeqGrid.Update;
end;

// ==== Main app ===============================================================
// =============================================================================

procedure TfrmG2Main.FormCreate(Sender: TObject);
var ModuleMap : TBitmap;
begin
  FDisableControls := False;
  FOldSplitterPos := Splitter1.Height;

  //FPatchManagerVisible := False;

  FSlotPanel[3] := TSlotPanel.Create( self, G2.SlotD);
  FSlotPanel[3].Parent := self;
  FSlotPanel[3].Align := alTop;
  FSlotPanel[3].VariationMenu := puVariationMenu;
  FSlotPanel[3].SlotCaption := 'D';
  FSlotPanel[3].OnClick := PanelClick;

  FSlotPanel[2] := TSlotPanel.Create( self, G2.SlotC);
  FSlotPanel[2].Parent := self;
  FSlotPanel[2].Align := alTop;
  FSlotPanel[2].VariationMenu := puVariationMenu;
  FSlotPanel[2].SlotCaption := 'C';
  FSlotPanel[2].OnClick := PanelClick;

  FSlotPanel[1] := TSlotPanel.Create( self, G2.SlotB);
  FSlotPanel[1].Parent := self;
  FSlotPanel[1].Align := alTop;
  FSlotPanel[1].VariationMenu := puVariationMenu;
  FSlotPanel[1].SlotCaption := 'B';
  FSlotPanel[1].OnClick := PanelClick;

  FSlotPanel[0] := TSlotPanel.Create( self, G2.SlotA);
  FSlotPanel[0].Parent := self;
  FSlotPanel[0].Align := alTop;
  FSlotPanel[0].VariationMenu := puVariationMenu;
  FSlotPanel[0].SlotCaption := 'A';
  FSlotPanel[0].OnClick := PanelClick;

  PerfPanel.Top := -1;
  PerfPanel.Align := alTop;

  ModuleMap := TBitmap.Create;
  try
    ModuleMap.LoadFromFile('Img\module_map.bmp');
    LoadImageMap( ModuleMap, 10, 22, ilModules);
  finally
    ModuleMap.Free;
  end;

  LoadIniXML;
end;

procedure TfrmG2Main.FormShow(Sender: TObject);
begin
  cbLogMessages.Checked := True;
  cbLogMessagesClick(self);

  StartupTimer.Enabled := True;
end;

procedure TfrmG2Main.StartupTimerTimer(Sender: TObject);
begin
  StartupTimer.Enabled := False;

  // Load module and parameter xml database
  G2.LoadModuleDefs('');
  G2.USBActive := True;
  CreateAddModuleMenu;
  CreateParamMenu;

  UpdateControls;
end;

procedure TfrmG2Main.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if G2.IsServer and (G2.GetClientCount > 0) then
   CanClose := MessageDlg('There are ' + IntToStr( G2.GetClientCount) + ' G2 clients connected to this server, do you really want to close the connection?',
           mtWarning, mbOKCancel, 0) = mrOk;
end;

procedure TfrmG2Main.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveIniXML; // At this moment all the windows still are visible
  G2.USBActive := False;
end;

procedure TfrmG2Main.FormDestroy(Sender: TObject);
begin
  if assigned(FCopyPatch) then
    FCopyPatch.Free;
end;

procedure TfrmG2Main.LoadImageMap(aBitmap: TBitmap; aCols, aRows: integer; aImageList: TImageList);
var SubBitmap : TBitmap;
    i, j : integer;
    Rect, DestRect : TRect;

begin
  SubBitmap := TBitmap.Create;
  SubBitmap.Width := aBitmap.Width div aCols + 1;
  SubBitmap.Height := aBitmap.Height div aRows + 1;
  SubBitmap.PixelFormat := aBitmap.PixelFormat;
  DestRect.Left := 0;
  DestRect.Top := 0;
  DestRect.Right := SubBitmap.Width;
  DestRect.Bottom := SubBitmap.Height;
  aImageList.Masked := False;
  aImageList.Width := SubBitmap.Width;
  aImageList.Height := SubBitmap.Height;
  try
    for i := 0 to aRows - 1 do
      for j := 0 to aCols - 1 do begin
        Rect.Left := j * SubBitmap.Width;
        Rect.Top := i * SubBitmap.Height;
        Rect.Right := Rect.Left + SubBitmap.Width;
        Rect.Bottom := Rect.Top + SubBitmap.Height;

        SubBitmap.Canvas.CopyRect( DestRect, aBitmap.Canvas, Rect);
        aImageList.Add( SubBitmap, nil);
      end;
  finally
    SubBitmap.Free;
  end;
end;

procedure TfrmG2Main.LoadIniXML;
var Doc : TXMLDocument;
    RootNode : TDOMNode;
    TCPSettingsNode : TXMLTCPSettingsType;
    FormSettingsNode : TXMLFormSettingsType;
begin
  if not FileExists('G2_editor_ini.xml') then
    exit;

  Doc := TXMLDocument.Create;
  try
    ReadXMLFile( Doc, 'G2_editor_ini.xml');

    RootNode := Doc.FindNode('G2_Editor_settings');
    if assigned(RootNode) then begin
      TCPSettingsNode := TXMLTCPSettingsType(RootNode.FindNode('TCP_settings'));
      if assigned(TCPSettingsNode) then begin
        G2.Host := TCPSettingsNode.IP;
        G2.Port := TCPSettingsNode.Port;
      end;

      FormSettingsNode := TXMLFormSettingsType(RootNode.FindNode('MainForm'));
      if assigned(FormSettingsNode) then begin
        SetFormPosition( self,
                         FormSettingsNode.PosX,
                         FormSettingsNode.PosY,
                         FormSettingsNode.SizeX,
                         FormSettingsNode.SizeY);
        Visible := True;
      end;
    end;

  finally
    Doc.Free;
  end;
end;

procedure TfrmG2Main.SaveIniXML;
var Doc : TXMLDocument;
    RootNode : TDOMNode;
    TCPSettingsNode : TXMLTCPSettingsType;
    PatchManagerSettingsNode : TXMLPatchManagerSettingsType;
    FormSettingsNode : TXMLFormSettingsType;
begin
  Doc := TXMLDocument.Create;
  try
    if FileExists('G2_editor_ini.xml') then
      ReadXMLFile( Doc, 'G2_editor_ini.xml');

    RootNode := Doc.FindNode('G2_Editor_settings');
    if not assigned(RootNode) then begin
      RootNode := Doc.CreateElement('G2_Editor_settings');
      Doc.AppendChild(RootNode);
    end;

    TCPSettingsNode := TXMLTCPSettingsType(RootNode.FindNode('TCP_settings'));
    if not assigned(TCPSettingsNode) then begin
      TCPSettingsNode := TXMLTCPSettingsType(Doc.CreateElement('TCP_settings'));
      RootNode.AppendChild(TCPSettingsNode);
    end;
    TCPSettingsNode.IP := G2.Host;
    TCPSettingsNode.Port := G2.Port;

    FormSettingsNode := TXMLFormSettingsType(RootNode.FindNode('MainForm'));
    if not assigned(FormSettingsNode) then begin
      FormSettingsNode := TXMLFormSettingsTYpe(Doc.CreateElement('MainForm'));
      RootNode.AppendChild(FormSettingsNode);
    end;
    FormSettingsNode.PosX := Left;
    FormSettingsNode.PosY := Top;
    FormSettingsNode.SizeX := Width;
    FormSettingsNode.SizeY := Height;
    FormSettingsNode.Visible := True;

    PatchManagerSettingsNode := TXMLPatchManagerSettingsType(RootNode.FindNode('PatchManagerSettings'));
    if not assigned(PatchManagerSettingsNode) then begin
      PatchManagerSettingsNode := TXMLPatchManagerSettingsType(Doc.CreateElement('PatchManagerSettings'));
      RootNode.AppendChild(PatchManagerSettingsNode);
    end;
    PatchManagerSettingsNode.BaseFolder := frmSettings.eRootFolder.Text;
    PatchManagerSettingsNode.SelectedTab := frmPatchManager.TabControl1.TabIndex;
    PatchManagerSettingsNode.ExternalSortCol := frmPatchManager.FExternalSortCol;
    PatchManagerSettingsNode.InternalSortCol := frmPatchManager.FInternalSortCol;

    FormSettingsNode := TXMLFormSettingsType(RootNode.FindNode('PatchManagerForm'));
    if not assigned(FormSettingsNode) then begin
      FormSettingsNode := TXMLFormSettingsTYpe(Doc.CreateElement('PatchManagerForm'));
      RootNode.AppendChild(FormSettingsNode);
    end;
    FormSettingsNode.PosX := frmPatchManager.Left;
    FormSettingsNode.PosY := frmPatchManager.Top;
    FormSettingsNode.SizeX := frmPatchManager.Width;
    FormSettingsNode.SizeY := frmPatchManager.Height;
    FormSettingsNode.Visible := frmPatchManager.Visible;

    FormSettingsNode := TXMLFormSettingsType(RootNode.FindNode('SettingsForm'));
    if not assigned(FormSettingsNode) then begin
      FormSettingsNode := TXMLFormSettingsTYpe(Doc.CreateElement('SettingsForm'));
      RootNode.AppendChild(FormSettingsNode);
    end;
    FormSettingsNode.PosX := frmSettings.Left;
    FormSettingsNode.PosY := frmSettings.Top;
    FormSettingsNode.SizeX := frmSettings.Width;
    FormSettingsNode.SizeY := frmSettings.Height;
    FormSettingsNode.Visible := frmSettings.Visible;

    FormSettingsNode := TXMLFormSettingsType(RootNode.FindNode('ParameterPagesForm'));
    if not assigned(FormSettingsNode) then begin
      FormSettingsNode := TXMLFormSettingsTYpe(Doc.CreateElement('ParameterPagesForm'));
      RootNode.AppendChild(FormSettingsNode);
    end;
    FormSettingsNode.PosX := frmParameterPages.Left;
    FormSettingsNode.PosY := frmParameterPages.Top;
    FormSettingsNode.SizeX := frmParameterPages.Width;
    FormSettingsNode.SizeY := frmParameterPages.Height;
    FormSettingsNode.Visible := frmParameterPages.Visible;

    FormSettingsNode := TXMLFormSettingsType(RootNode.FindNode('PatchSettingsForm'));
    if not assigned(FormSettingsNode) then begin
      FormSettingsNode := TXMLFormSettingsTYpe(Doc.CreateElement('PatchSettingsForm'));
      RootNode.AppendChild(FormSettingsNode);
    end;
    FormSettingsNode.PosX := frmPatchSettings.Left;
    FormSettingsNode.PosY := frmPatchSettings.Top;
    FormSettingsNode.SizeX := frmPatchSettings.Width;
    FormSettingsNode.SizeY := frmPatchSettings.Height;
    FormSettingsNode.Visible := frmPatchSettings.Visible;

    FormSettingsNode := TXMLFormSettingsType(RootNode.FindNode('EditorToolsForm'));
    if not assigned(FormSettingsNode) then begin
      FormSettingsNode := TXMLFormSettingsTYpe(Doc.CreateElement('EditorToolsForm'));
      RootNode.AppendChild(FormSettingsNode);
    end;
    FormSettingsNode.PosX := frmEditorTools.Left;
    FormSettingsNode.PosY := frmEditorTools.Top;
    FormSettingsNode.SizeX := frmEditorTools.Width;
    FormSettingsNode.SizeY := frmEditorTools.Height;
    FormSettingsNode.Visible := frmEditorTools.Visible;

    WriteXMLFile( Doc, 'G2_editor_ini.xml');
  finally
    Doc.Free;
  end;
end;

function TfrmG2Main.GetPatchWindowHeight: integer;
var i : integer;
begin
  Result := ClientHeight - PerfPanel.Height;
  for i := 0 to 3 do
    Result := Result - FSlotPanel[i].Height;
end;

procedure TfrmG2Main.DialogKey(var Msg: TWMKey);
begin
  case Msg.CharCode of
    VK_DOWN, VK_UP, VK_RIGHT, VK_LEFT:
      if Assigned(OnKeyDown) then begin
        OnKeyDown(Self, Msg.CharCode, KeyDataToShiftState(Msg.KeyData));
        inherited;
      end
    else
      inherited
  end;
end;

procedure TfrmG2Main.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    ord('1') : SelectVariation( G2.SelectedSlotIndex, 0);
    ord('2') : SelectVariation( G2.SelectedSlotIndex, 1);
    ord('3') : SelectVariation( G2.SelectedSlotIndex, 2);
    ord('4') : SelectVariation( G2.SelectedSlotIndex, 3);
    ord('5') : SelectVariation( G2.SelectedSlotIndex, 4);
    ord('6') : SelectVariation( G2.SelectedSlotIndex, 5);
    ord('7') : SelectVariation( G2.SelectedSlotIndex, 6);
    ord('8') : SelectVariation( G2.SelectedSlotIndex, 7);
    ord('F') : SelectPatchLocation( ltFX);
    ord('V') : SelectPatchLocation( ltVA);
    ord('A') : SelectSlot(0);
    ord('B') : SelectSlot(1);
    ord('C') : SelectSlot(2);
    ord('D') : SelectSlot(3);
    VK_LEFT  : begin
                 if ssShift in Shift then G2.SelectedPatch.SelectModuleLeft;
                 if not(ssShift in Shift) then G2.SelectedPatch.SelectPrevModuleParam;
               end;
    VK_RIGHT : begin
                 if ssShift in Shift then G2.SelectedPatch.SelectModuleRight;
                 if not(ssShift in Shift) then G2.SelectedPatch.SelectNextModuleParam;
               end;
    VK_UP    : begin
                 if ssShift in Shift then G2.SelectedPatch.SelectModuleAbove;
                 if not(ssShift in Shift) and not(ssCtrl in Shift) then if assigned(G2.SelectedPatch.SelectedParam) then G2.SelectedPatch.SelectedParam.IncValue;
                 if ssCtrl in Shift then if assigned(G2.SelectedPatch.SelectedParam) then G2.SelectedPatch.SelectedParam.IncMorphValue;
               end;
    VK_DOWN  : begin
                 if ssShift in Shift then G2.SelectedPatch.SelectModuleUnder;
                 if not(ssShift in Shift) and not(ssCtrl in Shift) then if assigned(G2.SelectedPatch.SelectedParam) then G2.SelectedPatch.SelectedParam.DecValue;
                 if ssCtrl in Shift then if assigned(G2.SelectedPatch.SelectedParam) then G2.SelectedPatch.SelectedParam.DecMorphValue;
               end;
  end;
end;

procedure TfrmG2Main.cbLogMessagesClick(Sender: TObject);
begin
  if cbLogMessages.Checked then
    G2.LogLevel := 1;
end;

procedure TfrmG2Main.cbModeClick(Sender: TObject);
begin
  if FDisableControls then exit;

  if cbMode.Checked then
    G2.SendSetModeMessage(1)  // Performance mode
  else
    G2.SendSetModeMessage(0); // Patch mode

  G2.Performance.USBStartInit( True);

  UpdateControls;
end;

procedure TfrmG2Main.GetPatchversion;
begin
  // Get current patch version from G2
  G2.SelectedSlot.SendGetPatchVersionMessage;
end;

procedure TfrmG2Main.sbFXMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var P : TPoint;
begin
  if assigned( FCopyPatch) and assigned(sbFX.CopyPatch) then begin
    sbFX.SetPositionsInCopyPatch;
    sbFX.CopyPatch := nil;
    sbVA.CopyPatch := nil;
    G2.SelectedPatch.MessCopyModules( FCopyPatch, ltVA, ltFX);
  end;

  if Button = mbRight then begin
    GetCursorPos( P);
    FLocation := ltFX;
    FAddPoint := sbFX.ScreenToClient(P);
    puAddModule.Popup(P.X, P.Y);
  end;
end;

procedure TfrmG2Main.sbVAMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var P : TPoint;
begin
  if assigned( FCopyPatch) and assigned(sbFX.CopyPatch) then begin
    sbVA.SetPositionsInCopyPatch;
    sbFX.CopyPatch := nil;
    sbVA.CopyPatch := nil;
    G2.SelectedPatch.MessCopyModules( FCopyPatch, ltVA, ltVA);
  end;

  if Button = mbRight then begin
    GetCursorPos( P);
    FLocation := ltVA;
    FAddPoint := sbVA.ScreenToClient(P);
    puAddModule.Popup(P.X, P.Y);
  end;
end;

procedure TfrmG2Main.SetSelectedModuleColor(aColor: byte);
var l, m : integer;
    Module : TG2FileModule;
begin
  for l := 0 to 1 do
    for m := 0 to G2.SelectedPatch.ModuleList[ l].Count - 1 do begin
      Module := G2.SelectedPatch.ModuleList[ l].Items[m];
     if Module.Selected then
       G2.SelectedPatch.MessSetModuleColor( TLocationType(l), Module.ModuleIndex, aColor);
    end;
end;

procedure TfrmG2Main.Splitter1Moved(Sender: TObject);
begin
  if FDisableControls then exit;

  FOldSplitterPos := Splitter1.Height;
end;

// ==== File menu ==============================================================

procedure TfrmG2Main.aDownloadPatchExecute(Sender: TObject);
begin
  G2.SelectedSlot.SendGetPatchMessage;
end;

procedure TfrmG2Main.aInitPatchExecute(Sender: TObject);
begin
  // New patch
  G2.SelectedPatch.Init;
  G2.SelectedSlot.SendSetPatchMessage('No name', G2.SelectedPatch);
end;

procedure TfrmG2Main.aLoadPatchExecute(Sender: TObject);
begin
  OpenDialog1.Filter := 'patch files (*.pch2)|*.pch2|sysex files (*.syx)|*.syx|bin files (*.bin)|*.bin';
  if OpenDialog1.Execute then begin
    G2.LoadFileStream( OpenDialog1.FileName);
  end;
end;

procedure TfrmG2Main.aLoadPerformanceExecute(Sender: TObject);
begin
  OpenDialog1.Filter := 'performance files (*.prf2)|*.prf2|sysex files (*.syx)|*.syx|bin files (*.bin)|*.bin';
  if OpenDialog1.Execute then begin
    G2.LoadFileStream( OpenDialog1.FileName);
  end;
end;

procedure TfrmG2Main.aSavePatchAsFXPExecute(Sender: TObject);
var WriteStream : TFileStream;
begin
  SaveDialog1.Filter := 'fxp files (*.fxp)|*.fxp';
  if SaveDialog1.Execute then begin

{$IFDEF FPC}
    if FileExistsUTF8(SaveDialog1.FileName) { *Converted from FileExists*  } then begin
{$ELSE}
    if FileExists(SaveDialog1.FileName) then begin
{$ENDIF}

      if MessageDlg('Are you sure you want to replace the existing file? Because this is demo software there is a change the patch will be corrupted!', mtConfirmation, [mbYes, mbNo], 0) = mrNo then
        exit;
{$IFDEF FPC}
      DeleteFileUTF8(SaveDialog1.FileName); { *Converted from DeleteFile*  }
{$ELSE}
      DeleteFile(SaveDialog1.FileName);
{$ENDIF}
    end;

    WriteStream := TFileStream.Create(SaveDialog1.FileName, fmCreate);
    try
      G2.SelectedPatch.SaveAsFXP(WriteStream);
    finally
      WriteStream.Free;
    end;
  end;
end;

procedure TfrmG2Main.aSavePatchAsSysexExecute(Sender: TObject);
var WriteStream : TFileStream;
begin
  SaveDialog1.Filter := 'sysex files (*.syx)|*.syx|bin files (*.bin)|*.bin';
  if SaveDialog1.Execute then begin
    WriteStream := TFileStream.Create(SaveDialog1.FileName, fmCreate);
    try
      G2.SelectedPatch.SaveMidiToStream(WriteStream);
    finally
      WriteStream.Free;
    end;
  end;
end;

procedure TfrmG2Main.aSavePatchExecute(Sender: TObject);
var WriteStream : TFileStream;
begin
  SaveDialog1.Filter := 'patch files (*.pch2)|*.pch2';
  SaveDialog1.DefaultExt := 'pch2';
  if SaveDialog1.Execute then begin
{$IFDEF FPC}
    if FileExistsUTF8(SaveDialog1.FileName) { *Converted from FileExists*  } then begin
{$ELSE}
    if FileExists(SaveDialog1.FileName) then begin
{$ENDIF}

      if MessageDlg('Are you sure you want to replace the existing file? Because this is demo software there is a change the patch will be corrupted!', mtConfirmation, [mbYes, mbNo], 0) = mrNo then
        exit;
{$IFDEF FPC}
      DeleteFileUTF8(SaveDialog1.FileName); { *Converted from DeleteFile*  }
{$ELSE}
      DeleteFile(SaveDialog1.FileName);
{$ENDIF}
    end;

    WriteStream := TFileStream.Create(SaveDialog1.FileName, fmCreate);
    try
      G2.SelectedPatch.SaveToFile(WriteStream);
    finally
      WriteStream.Free;
    end;
  end;
end;


procedure TfrmG2Main.aSavePerformanceAsFXBExecute(Sender: TObject);
var WriteStream : TFileStream;
begin
  SaveDialog1.Filter := 'fxb files (*.fxb)|*.fxb';
  if SaveDialog1.Execute then begin

{$IFDEF FPC}
    if FileExistsUTF8(SaveDialog1.FileName) { *Converted from FileExists*  } then begin
{$ELSE}
    if FileExists(SaveDialog1.FileName) then begin
{$ENDIF}

      if MessageDlg('Are you sure you want to replace the existing file? Because this is demo software there is a change the patch will be corrupted!', mtConfirmation, [mbYes, mbNo], 0) = mrNo then
        exit;
{$IFDEF FPC}
      DeleteFileUTF8(SaveDialog1.FileName); { *Converted from DeleteFile*  }
{$ELSE}
      DeleteFile(SaveDialog1.FileName);
{$ENDIF}
    end;

    WriteStream := TFileStream.Create(SaveDialog1.FileName, fmCreate);
    try
      G2.Performance.SaveAsFXB(WriteStream);
    finally
      WriteStream.Free;
    end;
  end;
end;

procedure TfrmG2Main.aSavePerformanceAsSysExExecute(Sender: TObject);
var WriteStream : TFileStream;
begin
  SaveDialog1.Filter := 'sysex files (*.syx)|*.syx|bin files (*.bin)|*.bin';
  if SaveDialog1.Execute then begin
    WriteStream := TFileStream.Create(SaveDialog1.FileName, fmCreate);
    try
      G2.Performance.SaveMidiToStream(WriteStream);
    finally
      WriteStream.Free;
    end;
  end;
end;

procedure TfrmG2Main.aSavePerformanceExecute(Sender: TObject);
var WriteStream : TFileStream;
begin
  SaveDialog1.Filter := 'performance files (*.prf2)|*.prf2';
  SaveDialog1.DefaultExt := 'prf2';
  if SaveDialog1.Execute then begin
{$IFDEF FPC}
    if FileExistsUTF8(SaveDialog1.FileName) { *Converted from FileExists*  } then begin
{$ELSE}
    if FileExists(SaveDialog1.FileName) then begin
{$ENDIF}

      if MessageDlg('Are you sure you want to replace the existing file? Because this is demo software there is a change the patch will be corrupted!', mtConfirmation, [mbYes, mbNo], 0) = mrNo then
        exit;

{$IFDEF FPC}
      DeleteFileUTF8(SaveDialog1.FileName); { *Converted from DeleteFile*  }
{$ELSE}
      DeleteFile(SaveDialog1.FileName);
{$ENDIF}
    end;

    WriteStream := TFileStream.Create(SaveDialog1.FileName, fmCreate);
    try
      G2.Performance.SaveToFile(WriteStream);
    finally
      WriteStream.Free;
    end;
  end;
end;

procedure TfrmG2Main.aExitExecute(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmG2Main.aExtractModuleInfoExecute(Sender: TObject);
begin
end;


// ==== Edit menu ==============================================================

procedure TfrmG2Main.aCopyExecute(Sender: TObject);
begin
  CopyPatchSelection;
end;

procedure TfrmG2Main.aCutExecute(Sender: TObject);
begin
  CopyPatchSelection;
  DeletePatchSelection;
end;

procedure TfrmG2Main.aPasteExecute(Sender: TObject);
begin
  PastePatchSelection;
end;

procedure TfrmG2Main.aDeleteExecute(Sender: TObject);
begin
  DeletePatchSelection;
end;

procedure TfrmG2Main.aUndoExecute(Sender: TObject);
begin
  Undo;
end;

procedure TfrmG2Main.aRedoExecute(Sender: TObject);
begin
//
end;

procedure TfrmG2Main.aSelectAllExecute(Sender: TObject);
begin
//
end;

// ==== View menu ==============================================================

procedure TfrmG2Main.aSynthSettingsExecute(Sender: TObject);
begin
  frmSynthSettings.Show;
  //G2.USBSetSynthSettings;
end;

procedure TfrmG2Main.aViewLogExecute(Sender: TObject);
begin
  frmLog.Show;
end;

procedure TfrmG2Main.aParameterPagesExecute(Sender: TObject);
begin
  frmParameterPages.Show;
end;

procedure TfrmG2Main.aPatchManagerExecute(Sender: TObject);
begin
  frmPatchManager.Show;
end;

procedure TfrmG2Main.aPatchSettingsExecute(Sender: TObject);
begin
  frmPatchSettings.Show;
end;

procedure TfrmG2Main.aPerformanceSettingsExecute(Sender: TObject);
begin
  frmPerfSettings.Show;
end;

procedure TfrmG2Main.aSettingsExecute(Sender: TObject);
begin
  frmSettings.Show;
end;

procedure TfrmG2Main.aEditToolsExecute(Sender: TObject);
begin
  frmEditorTools.Show;
end;

// ==== Module menu ============================================================

procedure TfrmG2Main.CreateAddModuleMenu;
var i, j : integer;
    aMenuItem, aSubMenuItem : TMenuItem;
    dummy : integer;
begin
  for i := 1 to 16 do begin

    aMenuItem := TMenuItem.Create( puAddModule);
    case i of
     1 : aMenuItem.Caption := 'In/Out';
     2 : aMenuItem.Caption := 'Note';
     3 : aMenuItem.Caption := 'Osc';
     4 : aMenuItem.Caption := 'LFO';
     5 : aMenuItem.Caption := 'Rnd';
     6 : aMenuItem.Caption := 'Env';
     7 : aMenuItem.Caption := 'Filter';
     8 : aMenuItem.Caption := 'FX';
     9 : aMenuItem.Caption := 'Delay';
    10 : aMenuItem.Caption := 'Shaper';
    11 : aMenuItem.Caption := 'Level';
    12 : aMenuItem.Caption := 'Mixer';
    13 : aMenuItem.Caption := 'Switch';
    14 : aMenuItem.Caption := 'Logic';
    15 : aMenuItem.Caption := 'Seq';
    16 : aMenuItem.Caption := 'MIDI';
    end;

    puAddModule.Items.Add( aMenuItem);

    for j := 0 to G2.FModuleDefList.Count - 1 do begin
      if G2.FModuleDefList.ModuleDef[j].ModuleType = 164 then
        dummy := 1;

      if string(G2.FModuleDefList.ModuleDef[j].Page) = aMenuItem.Caption then begin
        if aMenuItem.ImageIndex = -1 then
          aMenuItem.ImageIndex := G2.FModuleDefList.ModuleDef[j].ModuleType;

        aSubMenuItem := TMenuItem.Create( puAddModule);
        aSubMenuItem.Caption := string(G2.FModuleDefList.ModuleDef[j].ShortName);
        aSubMenuItem.Tag := G2.FModuleDefList.ModuleDef[j].ModuleType;
        aSubMenuItem.ImageIndex := G2.FModuleDefList.ModuleDef[j].ModuleType;
        aSubMenuItem.OnClick := DoAddModule;
        aMenuItem.Add(aSubMenuItem);
      end;
    end;
  end;
end;

procedure TfrmG2Main.ModuleClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,  Y: Integer; Module: TG2FileModule);
var P : TPoint;
begin
  if Button = mbRight then begin
    GetCursorPos(P);
    puModuleMenu.Popup( P.X, P.Y);
    puModuleMenu.Tag := integer(Module);
  end;
end;

procedure TfrmG2Main.miModuleRenameClick(Sender: TObject);
var Module : TG2GraphModule;
begin
  Module := TG2GraphModule(puModuleMenu.Tag);
  frmEditLabel.Left := Module.ClientToScreen(Point(0, 0)).X;
  frmEditLabel.Top := Module.ClientToScreen(Point(0, 0)).Y;
  if frmEditLabel.ShowModal = mrOk then begin
    G2.SelectedPatch.MessSetModuleLabel( Module.Location,
                                         Module.ModuleIndex,
                                         AnsiString(frmEditLabel.eLabel.Text));
  end;
end;

procedure TfrmG2Main.Properties1Click(Sender: TObject);
var Module : TG2GraphModule;
begin
  Module := TG2GraphModule(puModuleMenu.Tag);
  if Module.TypeID = 121 then begin
    frmSeqGrid.SetModule( Module);
    frmSeqGrid.Show;
  end;
end;

procedure TfrmG2Main.Def1Click(Sender: TObject);
var Module : TG2GraphModule;
begin
  Module := TG2GraphModule(puModuleMenu.Tag);
  frmModuleDef.FModuleType := Module.TypeID;
  frmModuleDef.Show;
end;

procedure TfrmG2Main.AddModule( aModuleType : byte);
begin
  G2.SelectedPatch.MessAddModule( FLocation, aModuleType, FAddPoint.X div UNITS_COL, FAddPoint.y div UNITS_ROW );
end;

procedure TfrmG2Main.Delete1Click(Sender: TObject);
var Module : TG2GraphModule;
begin
  Module := TG2GraphModule(puModuleMenu.Tag);

  G2.SelectedPatch.MessDeleteModule( Module.Location, Module.ModuleIndex);
end;

procedure TfrmG2Main.DoAddModule( Sender: TObject);
begin
  with Sender as TMenuItem do
    AddModule( Tag);
end;

// ==== Connector menu =========================================================

procedure TfrmG2Main.ConnectorClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,  Y: Integer; Connector: TG2FileConnector);
var P : TPoint;
    aMenuItem : TMenuItem;
    i : integer;
begin
  if Button = mbRight then begin
    GetCursorPos(P);

    while miDeleteCable.Count > 0 do begin
      miDeleteCable.Items[0].Free;
    end;

    for i := 0 to Connector.CableCount - 1 do begin
      aMenuItem := TMenuItem.Create( puConnectorMenu);
      if Connector.Cables[i].FromConnector = Connector then
        aMenuItem.Caption := 'Cable to ' + string(G2.SelectedPatch.GetModuleLabel( Connector.Module.Location, Connector.Cables[i].ModuleTo))
      else
        aMenuItem.Caption := 'Cable to ' + string(G2.SelectedPatch.GetModuleLabel( Connector.Module.Location, Connector.Cables[i].ModuleFrom));
      aMenuItem.Tag := integer(Connector.Cables[i]);
      aMenuItem.OnClick := miDeleteCablesClick;
      miDeleteCable.Add(aMenuItem);
    end;

    puConnectorMenu.Popup( P.X, P.Y);
    puConnectorMenu.Tag := integer(Connector);
  end;
end;

procedure TfrmG2Main.miDeleteAllCablesClick(Sender: TObject);
var Connector : TG2FileConnector;
   i : integer;
begin
  Connector := TG2FileConnector(puConnectorMenu.Tag);
  for i := 0 to Connector.CableCount - 1 do // TODO : Could be made more efficient (all in one message)
    G2.SelectedPatch.MessDeleteConnection( FLocation, Connector.Cables[0]);
end;

procedure TfrmG2Main.miDeleteCablesClick(Sender: TObject);
begin
  with Sender as TMenuItem do
    G2.SelectedPatch.MessDeleteConnection( FLocation, TG2FileCable(tag));
end;

// ==== Communication menu =====================================================

procedure TfrmG2Main.aSendControllerSnapshotExecute(Sender: TObject);
begin
  G2.SelectedSlot.SendControllerSnapshotMessage;
end;

procedure TfrmG2Main.aMidiDumpExecute(Sender: TObject);
begin
  G2.SendDumpMidiMessage;
end;

// ==== Parameter menu =========================================================

procedure TfrmG2Main.CreateParamMenu;
var i, j, Page, PageColumn, Param : integer;
    aMenuItem, aSubMenuItem : TMenuItem;
begin
  // Assign knobs
  for i := 0 to 14 do begin
    Page := ( i div 3);
    PageColumn := ( i mod 3);

    aMenuItem := TMenuItem.Create( puParamMenu);
    aMenuItem.Caption := 'Page ' + chr(65 + Page) + IntToStr(PageColumn);
    miAssignKnob.Add(aMenuItem);
    for j := 0 to 7 do begin
      aSubMenuItem := TMenuItem.Create( puParamMenu);
      aSubMenuItem.Caption := 'Knob ' + IntToStr(j);
      aSubMenuItem.Tag := i * 8 + j;
      aSubMenuItem.OnClick := AssignKnob;
      aMenuItem.Add(aSubMenuItem);
    end;
  end;
  aMenuItem := TMenuItem.Create( puParamMenu);
  aMenuItem.Caption := '-';
  miAssignKnob.Add(aMenuItem);

  aMenuItem := TMenuItem.Create( puParamMenu);
  aMenuItem.Caption := 'Deassign';
  aMenuItem.OnClick := DeassignKnob;
  miAssignKnob.Add(aMenuItem);

  // Assign global knobs
  for i := 0 to 14 do begin
    Page := ( i div 3);
    PageColumn := ( i mod 3);

    aMenuItem := TMenuItem.Create( puParamMenu);
    aMenuItem.Caption := 'Page ' + chr(65 + Page) + IntToStr(PageColumn);
    miAssignGlobalKnob.Add(aMenuItem);
    for j := 0 to 7 do begin
      aSubMenuItem := TMenuItem.Create( puParamMenu);
      aSubMenuItem.Caption := 'Knob ' + IntToStr(j);
      aSubMenuItem.Tag := i * 8 + j;
      aSubMenuItem.OnClick := AssignGlobalKnob;
      aMenuItem.Add(aSubMenuItem);
    end;
  end;
  aMenuItem := TMenuItem.Create( puParamMenu);
  aMenuItem.Caption := '-';
  miAssignGlobalKnob.Add(aMenuItem);

  aMenuItem := TMenuItem.Create( puParamMenu);
  aMenuItem.Caption := 'Deassign';
  aMenuItem.OnClick := DeassignGlobalKnob;
  miAssignGlobalKnob.Add(aMenuItem);
end;

procedure TfrmG2Main.miEditParamNameClick(Sender: TObject);
var Parameter : TG2FileParameter;
    Rect : TRect;
begin
  Rect := G2.SelectedPatch.SelectedControl.GetScreenCoordsRect;

  frmEditLabel.Left := Rect.Left;
  frmEditLabel.Top := Rect.Top;
  if frmEditLabel.ShowModal = mrOk then begin
    Parameter := G2.SelectedPatch.SelectedControl.Parameter;
    G2.SelectedPatch.MessSetModuleParamLabels( Parameter.Location,
                                               Parameter.ModuleIndex,
                                               Parameter.ParamIndex,
                                               AnsiString(frmEditLabel.eLabel.Text));
  end;
end;

procedure TfrmG2Main.AssignKnob(Sender: TObject);
var Parameter : TG2GraphParameter;
begin
  Parameter := TG2GraphParameter( puParamMenu.Tag);
  G2.SelectedPatch.MessAssignKnob( Parameter.Location, Parameter.ModuleIndex, Parameter.ParamIndex, (Sender as TMenuItem).Tag);
end;

procedure TfrmG2Main.DeAssignKnob(Sender: TObject);
var Parameter : TG2GraphParameter;
    KnobIndex : integer;
begin
  Parameter := TG2GraphParameter( puParamMenu.Tag);
  KnobIndex := G2.SelectedPatch.FindKnob( Parameter.Location, Parameter.ModuleIndex, Parameter.ParamIndex);

  if KnobIndex <> -1 then
    G2.SelectedPatch.MessDeassignKnob( KnobIndex);
end;

procedure TfrmG2Main.AssignGlobalKnob(Sender: TObject);
var Parameter : TG2GraphParameter;
begin
  Parameter := TG2GraphParameter( puParamMenu.Tag);
  G2.SelectedPatch.MessAssignGlobalKnob( Parameter.Location, Parameter.ModuleIndex, Parameter.ParamIndex, (Sender as TMenuItem).Tag);
end;

procedure TfrmG2Main.DeAssignGlobalKnob(Sender: TObject);
var Parameter : TG2GraphParameter;
    KnobIndex : integer;
begin
  Parameter := TG2GraphParameter( puParamMenu.Tag);
  KnobIndex := G2.Performance.GlobalKnobList.FindGlobalKnobIndex( G2.SelectedSlotIndex, Parameter.Location, Parameter.ModuleIndex, Parameter.ParamIndex);

  if KnobIndex <> -1 then
    G2.SelectedPatch.MessDeassignGlobalKnob( KnobIndex);
end;

procedure TfrmG2Main.AssignMidiCC(Sender: TObject);
var Parameter : TG2GraphParameter;
begin
  Parameter := TG2GraphParameter( puParamMenu.Tag);
  G2.SelectedPatch.MessAssignMidiCC( Parameter.Location, Parameter.ModuleIndex, Parameter.ParamIndex, FLastReceivedMidiCC);
end;

procedure TfrmG2Main.DeAssignMidiCC(Sender: TObject);
var Parameter : TG2GraphParameter;
    MidiCC : byte;
begin
  Parameter := TG2GraphParameter( puParamMenu.Tag);
  MidiCC := G2.SelectedPatch.FindMidiCC( Parameter.Location, Parameter.ModuleIndex, Parameter.ParamIndex);
  if MidiCC <> 0 then
    G2.SelectedPatch.MessDeassignMidiCC( MidiCC);
end;

procedure TfrmG2Main.AssignMorph(Sender: TObject);
var Parameter : TG2GraphParameter;
begin
  if Sender is TMenuItem then begin
    if (Sender as TMenuItem).Checked then begin
      // Deassign Morph
      Parameter := TG2GraphParameter( puParamMenu.Tag);
      G2.SelectedPatch.SetMorphValue(  Parameter.Location, Parameter.ModuleIndex, Parameter.ParamIndex, G2.SelectedPatch.SelectedMorphIndex, 0, G2.SelectedPatch.ActiveVariation);
      Parameter.InvalidateControl;
    end else begin
      Parameter := TG2GraphParameter( puParamMenu.Tag);
      G2.SelectedPatch.SetMorphValue(  Parameter.Location, Parameter.ModuleIndex, Parameter.ParamIndex, G2.SelectedPatch.SelectedMorphIndex, 128 - Parameter.GetParameterValue , G2.SelectedPatch.ActiveVariation);
      Parameter.InvalidateControl;
    end;
  end;
end;

procedure TfrmG2Main.ParameterClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,  Y: Integer; Parameter: TG2FileParameter);
var P : TPoint;
    Knob : TKnob;

  procedure SetKnobCheck( index : integer);
  var i, j : integer;
  begin
    for i := 0 to 14 do begin
      for j := 0 to 7 do begin
        if miAssignKnob.Items[i].Items[j].Tag = index then
          miAssignKnob.Items[i].Items[j].Checked := True
        else
          miAssignKnob.Items[i].Items[j].Checked := False;

        Knob := G2.SelectedPatch.GetKnob( i * 8 + j);
        if (Knob <> nil) and (Knob.IsAssigned = 1) then
          miAssignKnob.Items[i].Items[j].Enabled := False
        else
          miAssignKnob.Items[i].Items[j].Enabled := True;
      end;
    end;
  end;

  procedure SetGlobalKnobCheck( index : integer);
  var i, j : integer;
  begin
    for i := 0 to 14 do begin
      for j := 0 to 7 do begin
        if miAssignGlobalKnob.Items[i].Items[j].Tag = index then
          miAssignGlobalKnob.Items[i].Items[j].Checked := True
        else
          miAssignGlobalKnob.Items[i].Items[j].Checked := False;

        Knob := G2.Performance.GetGlobalKnob( i * 8 + j);
        if (Knob <> nil) and (Knob.IsAssigned = 1) then
          miAssignGlobalKnob.Items[i].Items[j].Enabled := False
        else
          miAssignGlobalKnob.Items[i].Items[j].Enabled := True;
      end;
    end;
  end;

  procedure SetMidiCCCheck( MidiCC : byte);
  begin
    miMidiCC.Caption :=  'Midi CC ' + IntToStr(FLastReceivedMidiCC);
    if MidiCC = 0 then begin
      miAssignMidiCC.Checked := False;
      miAssignMidiCC.Caption := 'Assign...';
    end else begin
      miAssignMidiCC.Checked := True;
      miAssignMidiCC.Caption := 'Assign';
    end;
  end;

  procedure SetMorphCheck( MorphAssigned : boolean);
  begin
    miMorphAssign.Checked := MorphAssigned;
  end;

begin
  if not assigned(Parameter) then
    exit;

  if Button = mbRight then begin
    GetCursorPos(P);

    SetKnobCheck( G2.SelectedPatch.FindKnob( Parameter.Location, Parameter.ModuleIndex, Parameter.ParamIndex));
    SetGlobalKnobCheck( G2.Performance.GlobalKnobList.FindGlobalKnobIndex( G2.SelectedSlotIndex, Parameter.Location, Parameter.ModuleIndex, Parameter.ParamIndex));
    SetMidiCCCheck( G2.SelectedPatch.FindMidiCC( Parameter.Location, Parameter.ModuleIndex, Parameter.ParamIndex));
    SetMorphCheck( G2.SelectedPatch.GetMorph( Parameter.Location, Parameter.ModuleIndex, Parameter.ParamIndex, G2.SelectedPatch.SelectedMorphIndex, G2.SelectedPatch.ActiveVariation) <> nil);

    miEditParamName.Enabled := sender is TG2GraphButtonText;

    puParamMenu.Popup( P.X, P.Y);
    puParamMenu.Tag := integer( Parameter);
  end else
    if frmSeqGrid.Visible and ((frmSeqGrid.FModuleIndex = Parameter.ModuleIndex) and
                               (frmSeqGrid.FLocation = Parameter.Location)) then
      frmSeqGrid.Update;
end;

{procedure TfrmG2USBInterface.bNoteClick(Sender: TObject);
begin
  FG2.USBNote($20); // play a test note
end;}
// ==== Functions ==============================================================

procedure TfrmG2Main.CopyPatchSelection;
begin
  if assigned( FCopyPatch) then
    FCopyPatch.Free;

  FCopyPatch := TG2FilePatchPart.CopyModules( G2.SelectedPatch, G2.SelectedPatch.PatchPart[ ord(FLocation)], G2.SelectedPatch.SelectedModuleList);
end;

procedure TfrmG2Main.DeletePatchSelection;
begin
  if G2.SelectedPatch.SelectedModuleList.Count > 0 then
    G2.SelectedPatch.MessDeleteModules( G2.SelectedPatch.SelectedModuleList[0].Location);
end;

procedure TfrmG2Main.PastePatchSelection;
begin
  if assigned( FCopyPatch) then begin
    sbVA.CopyPatch := FCopyPatch;
    sbFX.CopyPatch := FCopyPatch;
  end;
end;

procedure TfrmG2Main.PatchCtrlMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Sender is TG2GraphChildControl then
    ParameterClick( Sender, Button, Shift, X, Y, (Sender as TG2GraphChildControl).Parameter);
end;

procedure TfrmG2Main.Undo;
begin
  G2.SelectedPatch.SendUndoMessage;
end;

procedure TfrmG2Main.SelectSlot( aSlotIndex : byte);
begin
  G2.SelectedSlotIndex := aSlotIndex;
  UpdateControls;
end;

procedure TfrmG2Main.SelectVariation( aSlotIndex, aVariationIndex : byte);
begin
  G2.Slot[ aSlotIndex].SendSelectVariationMessage( aVariationIndex);
end;

procedure TfrmG2Main.SelectPatchLocation(aLocation: TLocationType);
begin
  FDisableControls := True;
  try
    if (aLocation = ltVA) then begin
      if (FLocation = ltFX) then
        sbFX.Height := 10
      else
        sbFX.Height := GetPatchWindowHeight - FOldSplitterPos - Splitter1.Height;
    end else
      if FLocation = ltVA then
        sbFX.Height := GetPatchWindowHeight - Splitter1.Height - 10
      else
        sbFX.Height := GetPatchWindowHeight - FOldSplitterPos - Splitter1.Height;
    FLocation := aLocation;
    Invalidate;
  finally
    FDisableControls := False;
  end;
end;

// ==== Event handling =========================================================

procedure TfrmG2Main.G2BeforeSendMessage(Sender: TObject; SenderID: Integer; SendMessage: TG2SendMessage);
begin
  // Waint for the response
  Screen.Cursor := crHourglass;
  // Set the responsetimer, in case of error
  ResponseTimer.Enabled := True;
end;

procedure TfrmG2Main.G2ReceiveResponseMessage(Sender: TObject; ResponseMessage: TMemoryStream);
begin
  ResponseTimer.Enabled := False;
  Screen.Cursor := crDefault;
end;

procedure TfrmG2Main.ResponseTimerTimer(Sender: TObject);
begin
  // Waited 5s for the response, probably lost contact now
  ResponseTimer.Enabled := False;
  Screen.Cursor := crDefault;
end;


procedure TfrmG2Main.PanelClick(Sender: TObject);
begin
  if FDisableControls then exit;

  if Sender is TSlotPanel then begin
     SelectSlot( (Sender as TSlotPanel).SlotIndex);
    // TODO
    //sbVA.Height := G2.Slot[FSelectedSlot].Patch.FPatchDescription.FBarPosition;
  end else
    if (Sender is TG2GraphLabel) and ((Sender as TG2GraphLabel).Parent is TSlotPanel) then
      SelectSlot( ((Sender as TG2GraphLabel).Parent as TSlotPanel).SlotIndex);

  UpdateControls;
end;

procedure TfrmG2Main.G2PatchUpdate(Sender: TObject; SenderID: Integer; PatchIndex: Integer);
begin
  FSlotPanel[ PatchIndex].UpdateControls;
end;

procedure TfrmG2Main.G2PerfSettingsUpdate(Sender: TObject; SenderID: Integer; PerfMode: Boolean);
begin
  frmPerfSettings.updateDialog;
  UpdateControls;
end;

procedure TfrmG2Main.G2SelectSlot(Sender: TObject; SenderID: Integer; Slot: Integer);
begin
  UpdateControls;
end;

procedure TfrmG2Main.G2SynthSettingsUpdate(Sender: TObject;  SenderID: Integer);
begin
  frmSynthSettings.updateDialog;
end;

procedure TfrmG2Main.G2USBActiveChange(Sender: TObject; Active: Boolean);
begin
  FDisableControls := True;
  try
    cbOnline.Checked := Active;
  finally
    FDisableControls := False;
  end;
end;

procedure TfrmG2Main.cbOnlineClick(Sender: TObject);
begin
  if FDisableControls then
    exit;

  G2.USBActive := cbOnline.Checked;
end;

procedure TfrmG2Main.G2VariationChange(Sender: TObject; SenderID: Integer; Slot, Variation: Integer);
begin
  if G2.SelectedSlotIndex = Slot then begin
    sbVA.Invalidate;
    sbFX.Invalidate;
  end;
  UpdateControls;
end;

procedure TfrmG2Main.G2AddClient(Sender: TObject; ClientIndex: Integer);
begin
  lbClientsConnected.Caption := IntToStr( G2.GetClientCount);
end;

procedure TfrmG2Main.G2AfterG2Init(Sender: TObject);
begin
  UpdateControls;
end;

procedure TfrmG2Main.G2AfterRetrievePatch(Sender: TObject; SenderID: Integer;  aSlot, aBank, aPatch: Byte);
begin
  if aSlot = 4 then
    G2.Performance.USBStartInit( True)
  else
    G2.Slot[ aSlot].USBStartInit( True);
end;

procedure TfrmG2Main.G2DeleteClient(Sender: TObject; ClientIndex: Integer);
begin
  lbClientsConnected.Caption := IntToStr( G2.GetClientCount);
end;

procedure TfrmG2Main.G2MidiCCReceive(Sender: TObject; SenderID: Integer; MidiCC: Byte);
begin
  FLastReceivedMidiCC := MidiCC;
end;

procedure TfrmG2Main.G2AssignKnob(Sender: TObject; SenderID: Integer; Slot : byte; KnobIndex: Integer);
begin
  frmParameterPages.UpdateControls;
end;

procedure TfrmG2Main.G2CreateModule(Sender: TObject; SenderID: Integer; Module: TG2FileModule);
begin
  (Module as TG2GraphModule).OnModuleClick := ModuleClick;
  (Module as TG2GraphModule).OnParameterClick := ParameterClick;
  (Module as TG2GraphModule).OnConnectorClick := ConnectorClick;
  FLocation := Module.Location;
end;

procedure TfrmG2Main.G2DeassignKnob(Sender: TObject; SenderID: Integer; Slot : byte; KnobIndex: Integer);
begin
  frmParameterPages.UpdateControls;
end;

end.
