unit g2_midi;

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
//  This unit needs the Delphi MIDI I/O components, download at https://bitbucket.org/h4ndy/midiio-dev/overview
//
//  ////////////////////////////////////////////////////////////////////////////


interface
uses
  Classes, SysUtils,
{$IFNDEF G2_VST}
  MMSystem, MidiType, MidiIn, MidiOut,
{$ENDIF}
  g2_types, g2_file, g2_usb;

const
  CHANNEL_MASK = $f;

type
  TG2Midi = class( TG2USB)
    private
      FMidiEnabled : boolean;
      FSysExStream : TMemoryStream; // Buffer for sysex
{$IFNDEF G2_VST}
      FMidiInput     : TMidiInput;
      FCtrlMidiInput : TMidiInput;
      FMidiOutput    : TMidiOutput;
      FLastMidiEvent : TMyMidiEvent;
    protected
      procedure   SetMidiEnabled( aValue : boolean);
    public
{$ENDIF}
      constructor Create( AOwner: TComponent); override;
      destructor  Destroy; override;
{$IFNDEF G2_VST}
      procedure   OpenMidi;
      procedure   CloseMidi;
      procedure   GetInDevices( sl : TStrings);
      procedure   GetOutDevices( sl : TStrings);
      procedure   ProcessMidiMessage;
      procedure   SysExSend( aSlot: byte);
      procedure   SendMidiShort( aMidiMessage, aData1, aData2: Byte);
      procedure   SendMidiLong( aSysex: Pointer; aMsgLength: Word);
      procedure   DoMidiInput(Sender: TObject);
      procedure   DoCtrlMidiInput(Sender: TObject);
      procedure   SysExSendPatch( aSlot : byte);
      procedure   SysExSendPerformance;
      procedure   SysExAllControllersRequest;
      procedure   SysExPatchRequestBySlot( aSlot : byte);
      procedure   SysExPerformanceRequest;
      procedure   SysExPatchRequestByFileIndex( aBank, aPatch: byte);
      procedure   SysExPerformanceRequestByFileIndex( aBank, aPerf: byte);
   published
      property    MidiEnabled : boolean read FMidiEnabled write SetMidiEnabled;
      property    MidiInput : TMidiInput read FMidiInput;
      property    CtrlMidiInput : TMidiInput read FCtrlMidiInput;
      property    MidiOutput : TMidiOutput read FMidiOutput;
{$ENDIF}
  end;

implementation

constructor TG2Midi.Create(AOwner: TComponent);
begin
  inherited;

  FMidiEnabled := False;
{$IFNDEF G2_VST}
  FMidiInput := TMidiInput.Create(self);
  FCtrlMidiInput := TMidiInput.Create(self);
  FMidiOutput := TMidiOutput.Create(self);
  FSysExStream := TMemoryStream.Create;

  FMidiInput.OnMidiInput := DoMidiInput;
  FCtrlMidiInput.OnMidiInput := DoCtrlMidiInput;
{$ENDIF}
end;

destructor TG2Midi.Destroy;
begin
{$IFNDEF G2_VST}
  CloseMidi;

  FSYsExStream.Free;
  FMidiOutput.Free;
  FCtrlMidiInput.Free;
  FMidiInput.Free;
{$ENDIF}
  inherited;
end;


{$IFNDEF G2_VST}
function MidiToString(MidiEvent: TMyMidiEvent): string;
var channel, parameter, i : integer;
begin
  if MidiEvent.Sysex = nil then begin
    channel := CHANNEL_MASK and MidiEvent.MidiMessage;
    parameter := MidiEvent.MidiMessage - channel;
    Result := IntToHex(parameter, 2)
            + IntToHex(MidiEvent.Data1, 2)
            + IntToHex(MidiEvent.Data2, 2);
  end else begin
    channel := CHANNEL_MASK and ord(MidiEvent.Sysex[2]);
    parameter := ord(MidiEvent.Sysex[5]);
    Result := IntToHex(ord(MidiEvent.Sysex[0]), 2);
    for i := 1 to MidiEvent.SysexLength - 1 do
      Result := Result + IntToHex(ord(MidiEvent.Sysex[i]), 2);
  end;
end;

procedure TG2Midi.SysExAllControllersRequest;
var
  SysEx : packed array of byte;
begin
  SetLength(SysEx, 10);
  SysEx[0] := $F0;
  SysEx[1] := $33;
  SysEx[2] := $7F;
  SysEx[3] := $0A;
  SysEx[4] := $40;
  SysEx[5] := $04;
  SysEx[6] := $F7;

  SendMidiLong(@SysEx[0], 7);
end;

procedure TG2Midi.SysExPatchRequestByFileIndex( aBank, aPatch: byte);
var
  SysEx : packed array of byte;
begin
  if (aBank < 1) or (aBank > 32) then
    raise Exception.Create('Bank must be in the range 1-32.');

  if (aPatch < 1) or (aBank > 128) then
    raise Exception.Create('Patch must be in the range 1-128.');

  SetLength(SysEx, 10);
  SysEx[0] := $F0;
  SysEx[1] := $33;
  SysEx[2] := $7F;
  SysEx[3] := $0A;
  SysEx[4] := $31;
  SysEx[5] := aBank;
  SysEx[6] := aPatch;
  SysEx[7] := $F7;

  SendMidiLong(@SysEx[0], 8);
end;

procedure TG2Midi.SysExPatchRequestBySlot( aSlot: byte);
var
  SysEx : packed array of byte;
begin
  if (aSlot<0) or (aSlot>3) then
    raise Exception.Create('Slot must be in the range 0-3.');

  SetLength(SysEx, 10);
  SysEx[0] := $F0;
  SysEx[1] := $33;
  SysEx[2] := $7F;
  SysEx[3] := $0A;
  SysEx[4] := $30;
  SysEx[5] := aSlot;
  SysEx[6] := $00;
  SysEx[7] := $F7;

  SendMidiLong(@SysEx[0], 8);
end;

procedure TG2Midi.SysExPerformanceRequestByFileIndex( aBank, aPerf : byte);
var
  SysEx : packed array of byte;
begin
  if (aBank < 1) or (aBank > 8) then
    raise Exception.Create('Bank must be in the range 1-8.');

  if (aPerf < 1) or (aPerf > 128) then
    raise Exception.Create('Perf must be in the range 1-128.');

  SetLength(SysEx, 10);
  SysEx[0] := $F0;
  SysEx[1] := $33;
  SysEx[2] := $7F;
  SysEx[3] := $0A;
  SysEx[4] := $39;
  SysEx[5] := aBank;
  SysEx[6] := aPerf;
  SysEx[7] := $F7;

  SendMidiLong(@SysEx[0], 8);
end;

procedure TG2Midi.SysExPerformanceRequest;
var
  SysEx : packed array of byte;
begin
  SetLength(SysEx, 10);

  SysEx[0] := $F0;
  SysEx[1] := $33;
  SysEx[2] := $7F;
  SysEx[3] := $0A;
  SysEx[4] := $38;
  SysEx[5] := $00;
  SysEx[6] := $00;
  SysEx[7] := $F7;

  SendMidiLong(@SysEx[0], 8);
end;

procedure TG2Midi.SendMidiLong( aSysex: Pointer; aMsgLength: Word);
begin
  with FMidiOutput do begin
    PutLong( aSysex, aMsgLength);
    add_log_line( FMidiOutput.ProductName + ' <= ' + 'Sysex', LOGCMD_NUL);
  end;
end;

procedure TG2Midi.SendMidiShort( aMidiMessage, aData1, aData2: Byte);
begin
   with FMidiOutput do begin
     PutShort( aMidiMessage, aData1, aData2);
     add_log_line( FMidiOutput.ProductName + ' <= ' + IntToHex( aMidiMessage, 2)
                                              + ' ' + IntToHex( aData1, 2)
                                              + ' ' + IntToHex( aData2, 2), LOGCMD_NUL);
  end;
end;

procedure TG2Midi.SetMidiEnabled(aValue: boolean);
begin
  if aValue then begin
    if FMidiInput.State = misClosed then
      FMidiInput.OpenAndStart;
    if FMidiOutput.State = mosClosed then
      FMidiOutput.Open;
    if FCtrlMidiInput.State = misClosed then
      FCtrlMidiInput.OpenAndStart;
    FMidiEnabled := aValue;
  end else begin
    FMidiEnabled := aValue;
    if FMidiInput.State = misOpen then
      FMidiInput.StopAndClose;
    if FMidiOutput.State = mosOpen then
      FMidiOutput.Close;
    if FCtrlMidiInput.State = misOpen then
      FCtrlMidiInput.StopAndClose;
  end;
end;

procedure TG2Midi.SysExSend( aSlot: byte);
var
  SysEx : packed array of byte;
  Buffer : packed array[0..3] of Byte;
  i, j, blok, FCheckSum : integer;
  C : Byte;
  line : string;

begin
  SetLength(SysEx, 4096);
  blok := 0;
  i := 0;

  FSysExStream.Position := 0;
  while (FSysExStream.Position < FSysExStream.Size) do begin
    FSysExStream.Read(C, 1);

    if (i<4) then begin
      Buffer[i] := C;
    end else begin
      if (Buffer[0] = $F0) and (Buffer[1] = $33) and (Buffer[2] = $7F) and (Buffer[3] = $0A) then begin
        // Begin nieuw blok

        if blok <> 0 then begin
          // Send Sysex

          if SysEx[4] = $20 then
            SysEx[6] := aSlot;


          FCheckSum := 0;
          line := '';
          for j := 0 to i - 4 - 1 do begin
            line := line + IntToHex(SysEx[j],2);
            if j = i - 4 - 1 - 1 then
              SysEx[j] := FChecksum
            else
              FChecksum := ( FChecksum + SysEx[j]) And $7f;
          end;

          add_log_line( 'Block : ' + IntToStr(blok) + ' ' + Line, LOGCMD_NUL);

          SendMidiLong( @SysEx[0], i-4);
        end;

        inc(blok);
        i := 4;
      end;

      SysEx[i-4] := Buffer[0];
      for j := 0 to 2 do
        Buffer[j] := Buffer[j+1];
      Buffer[3] := C;
    end;

    inc(i);
  end;

  // Rest buffer

  for j := 0 to 3 do begin
    SysEx[i-4] := Buffer[j];
    inc(i);
  end;

  if SysEx[4] = $20 then
    SysEx[6] := aSlot;

  FCheckSum := 0;
  line := '';
  for j := 0 to i - 4 - 1 do begin
    line := line + IntToHex(SysEx[j],2);
    if j = i - 4 - 1 - 1 then
      SysEx[j] := FChecksum
    else
      FChecksum := ( FChecksum + SysEx[j]) And $7f;
  end;

  add_log_line('Block : ' + IntToStr(blok) + ' ' + Line, LOGCMD_NUL);

  SendMidiLong( @SysEx[0], i-4);
end;

procedure TG2Midi.SysExSendPatch( aSlot : byte);
begin
  FSysExStream.Clear;
  GetSlot( aSlot).Patch.SaveMidiToStream( FSysExStream);
  SysExSend( aSlot);
end;

procedure TG2Midi.SysExSendPerformance;
begin
  FSysExStream.Clear;
  Performance.SaveMidiToStream( FSysExStream);
  SysExSend(0);
end;

procedure TG2Midi.DoCtrlMidiInput(Sender: TObject);
var
	thisEvent : TMyMidiEvent;
  midistring : string;
begin
  while (FCtrlMidiInput.MessageCount > 0) do begin

    { Get the event as an object }
    thisEvent := FCtrlMidiInput.GetMidiEvent;
    try
      if thisEvent.Sysex = nil then begin

        midistring := MidiToString(thisEvent);
        add_log_line( midistring, LOGCMD_NUL);

        case thisEvent.MidiMessage of
        143 : SendNoteMessage(thisEvent.Data1, $01);
        144 : case thisEvent.Data2 of
              0 : SendNoteMessage(thisEvent.Data1, $01);
              else
                SendNoteMessage(thisEvent.Data1, $00);
              end;
        end;


      end;
    finally
      thisEvent.Free;
    end;
  end;
end;

procedure TG2Midi.DoMidiInput(Sender: TObject);
begin
  ProcessMidiMessage;
end;

procedure TG2Midi.GetInDevices(sl: TStrings);
var DevMidiIn : TMidiInput;
    lInCaps: TMidiInCaps;
    i : integer;
begin
  DevMidiIn := TMidiInput.Create(self);
  try
    if DevMidiIn.NumDevs > 0 then begin
      for i := 0 To (DevMidiIn.NumDevs-1) do begin
        midiInGetDevCaps(i, @lInCaps, SizeOf(TMidiInCaps));
        sl.Add(lInCaps.szPname)
      end;
    end;

  finally
    DevMidiIn.Free;
  end;
end;

procedure TG2Midi.GetOutDevices(sl: TStrings);
var lOutCaps: TMidiOutCaps;
    DevMidiOut : TMidiOutput;
    i : integer;
begin
  DevMidiOut := TMidiOutput.Create(self);
  try
    if DevMidiOut.NumDevs > 0 then begin
      for i := 0 To (DevMidiOut.NumDevs-1) do begin
        midiOutGetDevCaps(i, @lOutCaps, SizeOf(TMidiOutCaps));
        sl.Add(lOutCaps.szPname);
      end;
    end;
  finally
    DevMidiOut.Free;
  end;
end;

procedure TG2Midi.ProcessMidiMessage;
var
	thisEvent : TMyMidiEvent;
  midistring : string;
  C : Byte;
  block, total, i : integer;
  G2FileDataStream : TG2FileDataStream;
  Lines : TStrings;
begin
  while (FMidiInput.MessageCount > 0) do begin

    { Get the event as an object }
    thisEvent := FMidiInput.GetMidiEvent;
    try
      if thisEvent.Sysex <> nil then begin

        midistring := MidiToString(thisEvent);
        add_log_line( midistring, LOGCMD_NUL);

        if (copy(midistring, 1, 12) = 'F0337F0A2000')   // Patch dump
          or (copy(midistring, 1, 12) = 'F0337F0A2800') // Perf dump
            then begin

          // Welk blok?
          block := ord(thisEvent.Sysex[8])*256 + ord(thisEvent.Sysex[9]);
          total := ord(thisEvent.Sysex[10])*256 + ord(thisEvent.Sysex[11]);

          add_log_line('Receiving sysex patch, block ' + IntToStr(block) + ' of ' + IntToStr(total), LOGCMD_NUL);

          if block = 0 then begin
            // Eerste blok
            FSysExStream.Clear;
          end;

          for i := 0 to thisEvent.SysexLength - 1 do begin
            C := ord(thisEvent.Sysex[i]);
            FSysExStream.Write(C, 1);
          end;

          if block = total - 1 then begin
            // Laatste blok
            FSysExStream.Position := 0;

            Lines := nil;
            if assigned(LogLines) then
              Lines := LogLines;

            //FSysExStream.SaveToFile('TestSysEx.bin');
            G2FileDataStream := TG2FileDataStream.LoadMidiData( self, FSysExStream, Lines);

            if G2FileDataStream is TG2FilePerformance then
              (Performance as TG2USBPerformance).SendSetPerformanceMessage( '', G2FileDataStream as TG2FilePerformance)
            else
              if G2FileDataStream is TG2FilePatch then
                (Performance.Slot[ Performance.SelectedSlot] as TG2USBSlot).SendSetPatchMessage( '', G2FileDataStream as TG2FilePatch)
              else
                raise Exception.Create('Unknown data type');
          end;
        end;
      end;
    finally
      thisEvent.Free;
    end;
  end;
end;

{ Maybe something for later
function TG2Midi.TranslateMidi(MidiEventIn: TMyMidiEvent): integer;
var channel : byte;
    c : integer;
begin
  if MidiEventIn.Sysex <> nil then
    SendMidiLong(MidiEventIn.Sysex, MidiEventIn.SysexLength)
  else
    if not frmG2EngineControl.ProcessCC( MidiEventIn.MidiMessage, MidiEventIn.Data1, MidiEventIn.Data2) then begin
      if frmG2EngineControl.sbPerf.Down then begin
        val(eGlobalChannel.Text, channel, c);
        channel := channel - 1;
      end else
        channel := GetChannel(frmG2EngineControl.CurrentSlot);
      SendMidiShort((MidiEventIn.MidiMessage and $F0) + channel, MidiEventIn.Data1, MidiEventIn.Data2);
    end;
end;}


{procedure TG2Midi.InitMidi;
var lOutCaps: TMidiOutCaps;
    lInCaps: TMidiInCaps;
    i : integer;
    MidiIn : TMidiInput;
    MidiOut : TMidiOutput;
begin
  MidiIn := TMidiInput.Create(self);
  MidiOut := TMidiOutput.Create(self);
  try

    if MidiOut.NumDevs > 0 then begin
      for i := 0 To (MidiOut.NumDevs-1) do begin
        midiOutGetDevCaps(i, @lOutCaps, SizeOf(TMidiOutCaps));

        with lvMidiOut.Items.Add do begin
          Caption:= lOutCaps.szPname;
          Data := TMidiOutput.Create(self);
        end;
      end;
    end;

    if MidiIn.NumDevs > 0 then begin
      for i := 0 To (MidiIn.NumDevs-1) do begin
        midiInGetDevCaps(i, @lInCaps, SizeOf(TMidiInCaps));

        with lvMidiIn.Items.Add do begin
          Caption:= lInCaps.szPname;
          Data := TMidiInput.Create(self);
          TMidiInput(Data).OnMidiInput := MidiInput;
        end;
      end;
    end;

  finally
    MidiOut.Free;
    MidiIn.Free;
  end;
end;}

procedure TG2Midi.OpenMidi;
begin
  with FMidiInput do begin
     Open;
     Start;
  end;

  with FCtrlMidiInput do begin
     Open;
     Start;
  end;

  with FMidiOutput do begin
    Open;
  end;
end;

procedure TG2Midi.CloseMidi;
begin
 with FMidiInput do begin
   Stop;
   Close;
  end;

 with FCtrlMidiInput do begin
   Stop;
   Close;
  end;

  with FMidiOutput do begin
    Close;
  end;
end;
{$ENDIF}

end.
