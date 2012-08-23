unit UnitMidiMapping;

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
//  Unit to map midi to editor UI elements, for use of editor with controler
//
//  ////////////////////////////////////////////////////////////////////////////

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  DOM, XMLRead, XMLWrite,
  MMSystem, MidiType, MidiIn, MidiOut,
  g2_database, g2_midi, Vcl.ComCtrls, JawsCtrls, Vcl.Menus;

type
  TfrmMidiMapping = class(TForm)
    DListView1: DListView;
    puCtrlAssignMidi: TPopupMenu;
    miCtrlMidiAssignment: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FMidiEditorAssignmentList : TMidiEditorAssignmentList;
    FLastMidiEvent   : TMyMidiEvent;
    FCtrlMidiEnabled : boolean;
    FCtrlMidiInput   : TMidiInput;
    FCurrentControl  : TMidiAwareControl;
    FCurrentIndex    : integer;
    procedure   SetCtrlMidiEnabled( aValue : boolean);
  public
    { Public declarations }
    procedure LoadIniXML;

    procedure   OpenCtrlMidi;
    procedure   CloseCtrlMidi;

    procedure   ProcessCtrlMidiMessage;
    procedure   DoCtrlMidiInput(Sender: TObject);

    procedure   PopupMenu( aControl : TMidiAwareControl; aIndex : integer; X, Y : integer);
    procedure   AssignCtrlMidi(Sender: TObject);
    procedure   DeassignCtrlMidi(Sender: TObject);

    procedure   AddMidiEditorAssignment( aControl : TMidiAwareControl;  aMidiChannel, aMidiNote : byte; aMidiCC : byte; aValue : byte);
    procedure   DeleteMidiEditorAssignment( aControl : TMidiAwareControl);

    property    CtrlMidiEnabled : boolean read FCtrlMidiEnabled write SetCtrlMidiEnabled;
    property    CtrlMidiInput : TMidiInput read FCtrlMidiInput;
  end;



var
  frmMidiMapping: TfrmMidiMapping;

implementation

{$R *.dfm}

procedure TfrmMidiMapping.FormCreate(Sender: TObject);
begin
  FCtrlMidiEnabled := False;
  FCtrlMidiInput := TMidiInput.Create(self);

  FMidiEditorAssignmentList := TMidiEditorAssignmentList.Create( True);

  FLastMidiEvent := TMyMidiEvent.Create;

  FCtrlMidiInput.OnMidiInput := DoCtrlMidiInput;
end;

procedure TfrmMidiMapping.FormDestroy(Sender: TObject);
begin
  CloseCtrlMidi;

  FLastMidiEvent.Free;

  FMidiEditorAssignmentList.Free;

  FCtrlMidiInput.Free;
end;

procedure TfrmMidiMapping.LoadIniXML;
var Doc : TXMLDocument;
    RootNode : TDOMNode;
    mi : integer;
    //CtrlMidiSettingsNode : TXMLCtrlMidiSettingsType;
    FormSettingsNode : TXMLFormSettingsType;
begin
  if not FileExists('G2_editor_ini.xml') then
    exit;

  Doc := TXMLDocument.Create;
  try
    ReadXMLFile( Doc, 'G2_editor_ini.xml');

    RootNode := Doc.FindNode('G2_Editor_settings');
    if assigned(RootNode) then begin

      {CtrlMidiSettingsNode := TXMLCtrlMidiSettingsType( RootNode.FindNode('CTRL_MIDI_settings'));
      if assigned( CtrlMidiSettingsNode) then begin
        cbCtrlMidiEnabled.Checked := False;

        mi := 0;
        while (mi<cbCtrlMidiInDevices.Items.Count) and (cbCtrlMidiInDevices.Items[mi] <> CtrlMidiSettingsNode.CtrlMidiInDevice) do
          inc(mi);

        if (mi<cbCtrlMidiInDevices.Items.Count) then begin
          try
            try
              cbCtrlMidiInDevices.ItemIndex := mi;
              frmMidiMapping.CtrlMidiInput.ChangeDevice( cbCtrlMidiInDevices.ItemIndex, False);
              frmMidiMapping.CtrlMidiEnabled := CtrlMidiSettingsNode.CtrlMidiEnabled;
            except on E:Exception do begin
                ShowMessage( E.Message);
                frmMidiMapping.CtrlMidiEnabled := False;
              end;
            end;

          finally
            cbCtrlMidiEnabled.Checked := frmMidiMapping.CtrlMidiEnabled;
          end;
        end else
          frmMidiMapping.CtrlMidiEnabled := False;
      end;}

      FormSettingsNode := TXMLFormSettingsType(RootNode.FindNode('MidiMappingForm'));
      if assigned(FormSettingsNode) then begin
        Left := FormSettingsNode.PosX;
        Top := FormSettingsNode.PosY;
        Width := FormSettingsNode.SizeX;
        Height := FormSettingsNode.SizeY;
        Visible := FormSettingsNode.Visible;
      end;

    end;
  finally
    Doc.Free;
  end;
end;

procedure TfrmMidiMapping.AddMidiEditorAssignment( aControl: TMidiAwareControl; aMidiChannel, aMidiNote, aMidiCC, aValue: byte);
var i : integer;
    MidiEditorAssignment : TMidiEditorAssignment;
begin
  MidiEditorAssignment := FMidiEditorAssignmentList.Find( aControl);
  if not assigned(MidiEditorAssignment) then begin
    MidiEditorAssignment := TMidiEditorAssignment.Create;
    with MidiEditorAssignment do begin
      FChannel  := aMidiChannel;
      FNote     := aMidiNote;
      FCC       := aMidiCC;
      FValue    := aValue;
      FControl  := aControl;
    end;
    FMidiEditorAssignmentList.Add( MidiEditorAssignment);
    aControl.MidiEditorAssignment := MidiEditorAssignment;
  end;
end;

procedure TfrmMidiMapping.DeleteMidiEditorAssignment(aControl: TMidiAwareControl);
var i : integer;
begin
  i := 0;
  while (i<FMidiEditorAssignmentList.Count) and (FMidiEditorAssignmentList.Items[i].FControl <> aControl) do
    inc(i);
  if (i<FMidiEditorAssignmentList.Count) then begin
    FMidiEditorAssignmentList.Items[i].FControl.MidiEditorAssignment := nil;
    FMidiEditorAssignmentList.Delete( i);
  end;
end;

procedure TfrmMidiMapping.OpenCtrlMidi;
begin
  with FCtrlMidiInput do begin
     Open;
     Start;
  end;
end;

procedure TfrmMidiMapping.CloseCtrlMidi;
begin
 with FCtrlMidiInput do begin
   Stop;
   Close;
  end;
end;

procedure TfrmMidiMapping.SetCtrlMidiEnabled(aValue: boolean);
begin
  if aValue then begin
    if FCtrlMidiInput.State = misClosed then
      FCtrlMidiInput.OpenAndStart;
    FCtrlMidiEnabled := aValue;
  end else begin
    FCtrlMidiEnabled := aValue;
    if FCtrlMidiInput.State = misOpen then
      FCtrlMidiInput.StopAndClose;
  end;
end;

procedure TfrmMidiMapping.DoCtrlMidiInput(Sender: TObject);
begin
  ProcessCtrlMidiMessage;
end;

procedure TfrmMidiMapping.PopupMenu( aControl: TMidiAwareControl; aIndex : integer; X, Y: integer);
var i : integer;
    MidiEditorAssignment : TMidiEditorAssignment;
begin
  FCurrentControl := aControl;
  FCurrentIndex := aIndex;

  MidiEditorAssignment := FMidiEditorAssignmentList.Find( aControl);
  if assigned(MidiEditorAssignment) then begin
    miCtrlMidiAssignment.Caption := 'Deassign Ctrl Midi';
    miCtrlMidiAssignment.OnClick := DeassignCtrlMidi;
    puCtrlAssignMidi.Popup( X, Y);
  end else begin
    miCtrlMidiAssignment.Caption := 'Assign Ctrl to ' + IntToHex( FLastMidiEvent.Data1, 2);
    miCtrlMidiAssignment.OnClick := AssignCtrlMidi;
    puCtrlAssignMidi.Popup( X, Y);
  end;
end;

procedure TfrmMidiMapping.AssignCtrlMidi(Sender: TObject);
var MidiMessage, Channel : byte;
begin
  MidiMessage := FLastMidiEvent.MidiMessage shr 4;
  Channel := FLastMidiEvent.MidiMessage and $0F;

  // Is it a note or a cc?
  if MidiMessage = $09 then begin
    // Note
    AddMidiEditorAssignment( FCurrentControl,
                             Channel,
                             FLastMidiEvent.Data1,
                             0,
                             FCurrentIndex);
  end else begin
    if MidiMessage = $0B  then begin
      // CC
      AddMidiEditorAssignment( FCurrentControl,
                               Channel,
                               0,
                               FLastMidiEvent.Data1,
                               FCurrentIndex);
    end;
  end;
end;

procedure TfrmMidiMapping.DeassignCtrlMidi(Sender: TObject);
begin
  DeleteMidiEditorAssignment( FCurrentControl);
end;

procedure TfrmMidiMapping.ProcessCtrlMidiMessage;
var	thisEvent : TMyMidiEvent;
    MidiMessage, Channel : byte;
    i : integer;
begin
  while (FCtrlMidiInput.MessageCount > 0) do begin

    { Get the event as an object }
    thisEvent := FCtrlMidiInput.GetMidiEvent;
    try
      if thisEvent.Sysex = nil then begin
        FLastMidiEvent.MidiMessage := thisEvent.MidiMessage;
        FLastMidiEvent.Data1 := thisEvent.Data1;
        FLastMidiEvent.Data2 := thisEvent.Data2;
        FLastMidiEvent.Time := thisEvent.Time;

        MidiMessage := FLastMidiEvent.MidiMessage shr 4;
        Channel := FLastMidiEvent.MidiMessage and $0F;

        if MidiMessage = $09 then begin
          i := 0;
          while (i<FMidiEditorAssignmentList.Count) do begin
            if FMidiEditorAssignmentList[i].FNote = thisEvent.Data1 then begin
              FMidiEditorAssignmentList[i].FControl.SetValueByMidi(FMidiEditorAssignmentList[i].FValue);
            end;
            inc(i);
          end;

        end else
          if MidiMessage = $0B then begin
            i := 0;
            while (i<FMidiEditorAssignmentList.Count) do begin
              if FMidiEditorAssignmentList[i].FCC = thisEvent.Data1 then begin
                FMidiEditorAssignmentList[i].FControl.SetValueByMidi(thisEvent.Data2);
              end;
              inc(i);
            end;

          end;
      end;
    finally
      thisEvent.Free;
    end;
  end;
end;



end.
