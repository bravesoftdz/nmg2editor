unit uPlugin;

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
//  This vst was based on Tobybear's template : http://www.tobybear.de/
//
//  Plugin : To avoid access violations, all dynamic vars should be contained
//           in the objects that are created in the dll, because multiple
//           copies of de dll might be instantiated in the host.
//
//  ////////////////////////////////////////////////////////////////////////////

interface
uses Windows, uEditor, DAEffect, DAEffectX,
     DAudioEffect, DAudioEffectX, DVstTemplate, Forms, Messages,
     DVstUtils, Math, DDspUtils, Sysutils, Classes, StdCtrls,
     g2_types, g2_usb, g2_file;

type
   APlugin = class(TVstTemplate)
   public
     FG2 : TG2USB;
     FSelectedSlot : byte;
     FMemStream : TMemoryStream;
     FNumPrograms, FNumParameters,
     FNumGlobalKnobs, FNumVariationParams : integer;
     constructor Create(audioMaster: TAudioMasterCallbackFunc);
     destructor  Destroy; override;
     function    GetLibraryPath: AnsiString;
     procedure   processMIDI(ev: VstMidiEvent); override;
     procedure   initializeParameters; override;
     procedure   parameterChanged(Index: Integer);
     procedure   setParameter(index: Longint; value: Single); override;
     function    getParameter(index: Longint): Single; override;
     function    getChunk(var data: pointer; isPreset: Boolean): longint; override;
     function    setChunk(data: pointer; byteSize: longint; isPreset: Boolean): longint; override;

     procedure   processAudio(const inputs, outputs: TArrayOfSingleArray; sampleframes: Integer); override;
     procedure   editorIdle(Sender: TObject);

     procedure   ClearBuffer;

     procedure   OnParamChange( Sender: TObject; SenderID : integer; Slot, Variation : byte; Location : TLocationType; ModuleIndex, ParamIndex : byte; Value : byte);
     procedure   OnAssignGlobalKnob(Sender: TObject; SenderID : integer; KnobIndex : integer);
     procedure   OnDeassignGlobalKnob(Sender: TObject; SenderID : integer; KnobIndex : integer);
     procedure   OnPatchUpdate(Sender: TObject; SenderID : integer; PatchIndex : integer);
     procedure   OnVariationChange(Sender: TObject; SenderID : integer; Slot, Variation : integer);
   end;

implementation

function APlugin.GetLibraryPath: AnsiString;
var
  Buffer: Array[0..260] of Char;
  i : integer;
begin
  Result := '';
  GetModuleFileName(hInstance, Buffer, Length(Buffer));
  i := 0;
  while (i<=260) and (Buffer[i]<>#0) do begin
    Result := Result + Buffer[i];
    inc(i);
  end;
  if Result <> '' then begin
    while (i > 1) and (Result[i] <> '\') do begin
      dec(i);
    end;
    Result := copy(Result, 1, i);
  end;
end;

constructor APlugin.Create(audioMaster: TAudioMasterCallbackFunc);
begin
  // A G2 object without the vcl graphics is created, because vcl is not
  // thread safe.

  FG2 := TG2USB.Create( nil);
  FG2.IsServer := False;
  FG2.ClientType := ctVST;
  FG2.Port := DEFAULT_PORT;
  FG2.Host := '127.0.0.1';
  FSelectedSlot := 0;

  FG2.OnParameterChange := OnParamChange;
  FG2.OnAssignGlobalKnob := OnAssignGlobalKnob;
  FG2.OnDeassignGlobalKnob := OnDeassignGlobalKnob;
  FG2.OnPatchUpdate := OnPatchUpdate;
  FG2.OnVariationChange := OnVariationChange;

  // define what the plugin "can do"
  // the following canDos are defined:
 { sendVstEvents, sendVstMidiEvent, sendVstTimeInfo,
   receiveVstEvents, receiveVstMidiEvent, receiveVstTimeInfo,
   offline, plugAsChannelInsert, plugAsSend, mixDryWet, noRealTime,
   multipass, metapass, _1in1out, _1in2out, _2in1out, _2in2out,
   _2in4out, _4in2out, _4in4out, _4in8out, _8in4out, _8in8out,
   midiProgramNames, conformsToWindowRules, bypass }
  canDos := [receiveVstEvents, receiveVstMidiEvent, sendVstEvents, sendVstMidiEvent];

  // define some plugin properties
 { prIsSynth, prHasVu, prHasClip, prCanMono,
   prCanReplacing, prProgramsAreChunks,
   prNoSoundinStop, prIsAsync, prExtHasBuffer }
  Properties := [prCanMono, prCanReplacing, prProgramsAreChunks];

  // define number of audio inputs and outputs
  numInputs := 2;
  numOutputs := 2;

  // define number of programs and parameters
  FNumPrograms := 4;
  FNumGlobalKnobs := 120;
  FNumVariationParams := 4;
  FNumParameters := FNumGlobalKnobs + FNumVariationParams;

  FMemStream := TMemoryStream.Create;
  ClearBuffer;

   // set the unique ID for this plugin,
  // change this for every new plugin you make!
  UniqueID      := 'NMG2';

  // more plugin ID settings
  EffectName    := 'G2 Vst';
  ProductName   := 'G2 Vst';
  VendorName    := 'BVerhue';
  VendorVersion := 1;

  // define the delay (in samples) that the plugin produces (if any)
  // this is necessary for latency compensation in the host
  initialDelay := 0;

  // You can replace "TPluginEditorWindow" with "nil" if you do not want an own
  // editor window and use the host's default representation
  inherited Create(audiomaster, FNumPrograms, FNumParameters, TPluginEditorWindow);


{  log.Add('Load module defs from path ' + GetLibraryPath);
  try}
//    FG2.LoadModuleDefs(GetLibraryPath);
{    except on E:Exception do begin
      log.Add(E.Message);
      Log.SaveToFile(LOG_PATH + 'VSTLog.txt');
    end;
  end;}

  //FG2.LogLevel := 1;
  //FG2.USBActive := True;

  // connect event handlers
  OnParameterChange := ParameterChanged;
  OnEditorIdle := EditorIdle;
end;

destructor APlugin.Destroy;
begin
  if FG2.USBActive then
    FG2.USBActive := False;
  FG2.Free;

  FMemStream.Free;

  inherited;
end;

procedure APlugin.ClearBuffer;
var i : integer;
begin
  i := 0;
  FMemStream.Write(i, SizeOf(i));
  FMemStream.Write(i, SizeOf(i));
end;

function APlugin.getChunk(var data: pointer; isPreset: Boolean): longint;
begin
  FG2.add_log_line('getChunk', LOGCMD_NUL);
  if isPreset then begin
    // Save a patch

    FMemStream.Clear;
    FG2.GetSlot( FSelectedSlot).GetPatch.SaveToFile( FMemStream);
    FMemStream.Position := 0;
    data := FMemStream.Memory;
    result := FMemStream.Size;

  end else begin
    // Save a performance

    FMemStream.Clear;
    FG2.Performance.SaveToFile( FMemStream);
    FMemStream.Position := 0;
    data := FMemStream.Memory;
    result := FMemStream.Size;
  end;
end;

function APlugin.setChunk(data: pointer; byteSize: Integer; isPreset: Boolean): longint;
var aPatch : TG2USBPatch;
    aPerf : TG2FilePerformance;
begin
  FG2.add_log_line('setChunk', LOGCMD_NUL);
  try
    if isPreset then begin
      // Load a patch

      FMemStream.Clear;
      FMemStream.Write( data^, byteSize);
      FMemStream.Position := 0;

      aPatch := TG2USBPatch.Create(nil);
      try
        aPatch.LoadFromFile( FMemStream, nil);
        FG2.GetSlot( FSelectedSlot).SendSetPatchMessage('no name', aPatch);
      finally
        aPatch.Free;
      end;

    end else begin
      // Load a performance

      FMemStream.Clear;
      FMemStream.Write( data^, byteSize);
      FMemStream.Position := 0;

      aPerf := TG2FilePerformance.Create( nil);
      try
        if aPerf.LoadFromFile( FMemStream, nil) then
          FG2.GetPerformance.SendSetPerformanceMessage( 'No name', aPerf);
      finally
        aPerf.Free;
      end;
    end;

    ClearBuffer;
  except
    on E:Exception do begin
      FG2.add_log_line( 'SetChunk : ' + E.Message, LOGCMD_NUL);
      FG2.save_log;
    end;
  end;
end;

procedure APlugin.initializeParameters;
var i, j, Page, PageColumn, Param : integer;
    name : AnsiString;
    value : single;
begin
  // initialize your parameters here:
  FG2.LogLevel := 1;
  try
    FG2.LoadModuleDefs(GetLibraryPath);
    FG2.USBActive := True;

    FG2.add_log_line( 'Initialize parameters', LOGCMD_NUL);

    for i := 0 to FNumGlobalKnobs - 1 do begin
      Value := FG2.Performance.GlobalKnobList.Items[i].KnobValue / 127;

      inherited setParameter( i , Value);

      Param := i mod 8;
      PageColumn := (i div 8) mod 3;
      Page := (i div 8) div 3;;

      Name := 'Page ' + chr(65 + Page) + IntToStr(PageColumn) + ' Knob ' + IntTostr(Param);
      j := 1;
      while (j <= Length(Name)) and (j <= 30) do begin
        ParameterProperties[i].name[j-1] := Name[j];
        inc(j);
      end;
      ParameterProperties[i].name[j-1] := #0;
    end;

    for i := 0 to FNumVariationParams - 1 do begin
      Value := FG2.GetSlot(i).GetPatch.ActiveVariation / 7;

      inherited setParameter( i , Value);

      Name := 'Variation slot ' + IntToStr( i + 1);
      j := 1;
      while (j <= Length(Name)) and (j <= 30) do begin
        ParameterProperties[ FNumGlobalKnobs + i].name[j-1] := Name[j];
        inc(j);
      end;
      ParameterProperties[ FNumGlobalKnobs + i].name[j-1] := #0;
    end;
  except on E:Exception do
    begin
      FG2.add_log_line( E.Message, LOGCMD_ERR);
    end;
  end;
end;

// This procedure is for processing VST MIDI input and output.
// MIDI input could be processed as shown in the example below,
// MIDI output can be generated by calling one of the MIDI_*
// procedures (look in DVstTemplate.pas for them).
procedure APlugin.processMIDI(ev: VstMidiEvent);
var time, data1, data2, status, channel: integer;
begin
  // decode MIDI info like channel, status and data bytes
  channel := ev.midiData[0] and $0F;
  status := ev.midiData[0] and $F0;
  data1 := ev.midiData[1] and $7F;
  data2 := ev.midiData[2] and $7F;
  time := ev.deltaFrames;

  // example MIDI code:
  if (status = $90) and (data2 > 0) then begin // "Note On" ?
  // data1 contains note number, data2 contains note velocity
  // send "Note On" back to host (MIDI thru)
    MIDI_NoteOn(channel, data1, data2, time);
  end else
    if ((status = $90) and (data2 = 0)) or (status = $80) then begin // "Note Off" ?
    // data1 contains note number, data2 contains note off velocity
    // send "Note Off" back to host (MIDI thru)
      MIDI_NoteOff(channel, data1, data2, time);
    end;
end;

procedure APlugin.parameterChanged(Index: Integer);
begin
 // This procedure gets called whenever a parameter (index) is
 // changed to a new value. You can then react accordingly.
 // if (Index = 0) then ...
 // else if (Index = 1) then ...
end;

procedure APlugin.editorIdle(Sender: TObject);
begin
 // Here you can directly access the editor form from the plugin.
 // with (Sender as TPluginEditorWindow) do ...
end;

procedure APlugin.processAudio(const inputs, outputs: TArrayOfSingleArray;
  sampleframes: Integer);
var i: integer;
begin
  // This is usually the most important part of your plugin:
  // Here the samples for each input and output channel can be processed
  for i := 0 to sampleframes - 1 do begin
  // in this example, we just pass through the audio data
    outputs[0, i] := inputs[0, i];
    outputs[1, i] := inputs[1, i];
   end;
end;

function APlugin.getParameter(index: Integer): Single;
var Knob : TKnob;
begin
  // Host queries parameter

  //addLogline('getParameter, index = ' + IntToStr(index));

  Result := 0;
  if (index >= 0) and (index < FNumParameters) then begin
    if index < FNumGlobalKnobs then begin
      Knob := FG2.Performance.GlobalKnobList.Items[ index];
      if assigned(Knob) and (Knob.IsAssigned = 1) then
        Result := Knob.KnobValue / 127
      else
        Result := 0;;
    end else
      if index < (FNumGlobalKnobs + FNumVariationParams) then begin
        Result := FG2.GetSlot( index - FNumGlobalKnobs).GetPatch.ActiveVariation / 7;
      end;
  end;

  //  addLogline('  Result =  ' + FloatToStr(Result));
end;

procedure APlugin.setParameter(index: Integer; value: Single);
var Knob : TGlobalKnob;
    Patch : TG2USBPatch;
begin
  try
    // Parameter change from Host

    if (index >= 0) and (index < FNumParameters) then begin
      if index < FNumGlobalKnobs then begin
        Knob := FG2.Performance.GlobalKnobList.Items[ index];
        if assigned(Knob) and (Knob.IsAssigned = 1) then begin
          //Knob.KnobValue := trunc( (Knob.KnobHighValue - Knob.KnobLowValue) * Value);
          //Patch := FG2.Patch[ Knob.SlotIndex];
          //Patch.SetParameterAutomated( TLocationType(Knob.Location), Knob.ModuleIndex, Knob.ParamIndex, Patch.ActiveVariation,
          //                 trunc((Knob.KnobHighValue - Knob.KnobLowValue) * Value));
          FG2.GetSlot( Knob.SlotIndex).SendSetParamMessage( Knob.Location,
                                                            Knob.ModuleIndex, Knob.ParamIndex,
                                                            trunc((Knob.KnobHighValue - Knob.KnobLowValue) * Value),
                                                            FG2.GetSlot( Knob.SlotIndex).GetPatch.ActiveVariation);
          editorNeedsUpdate := True;
        end
      end else
        if index < FNumGlobalKnobs + FNumVariationParams then begin
          // FG2.Patch[ index - FNumGlobalKnobs].SetVariation( trunc(Value * 7));
          FG2.GetSlot( index - FNumGlobalKnobs).SendSelectVariationMessage( trunc(Value * 7));
          editorNeedsUpdate := True;
        end;
    end;
  except on E:Exception do begin
     FG2.add_log_line('SetParameter : ' + E.Message, LOGCMD_NUL);
     FG2.save_log;
    end;
  end;
end;

procedure APlugin.OnAssignGlobalKnob(Sender: TObject; SenderID : integer; KnobIndex: integer);
begin
  FG2.add_log_line('OnAssignKnob', LOGCMD_NUL);
  editorNeedsUpdate := True;
end;

procedure APlugin.OnDeassignGlobalKnob(Sender: TObject; SenderID : integer; KnobIndex: integer);
begin
  FG2.add_log_line('OnAssignKnob', LOGCMD_NUL);
  editorNeedsUpdate := True;
end;

procedure APlugin.OnPatchUpdate(Sender: TObject; SenderID : integer; PatchIndex: integer);
begin
  FG2.add_log_line('OnPatchUpdate', LOGCMD_NUL);
  editorNeedsUpdate := True;
end;

procedure APlugin.OnVariationChange(Sender: TObject; SenderID : integer; Slot, Variation: integer);
begin
  if SenderID = FG2.ID then // Don't process messages that originate from this client
    exit;

  //addLogline('OnVariationChange');
  FG2.add_log_line('OnVariationChange', LOGCMD_NUL);
  try
    //if Assigned(audioMaster) then begin
      //audioMaster(Effect, audioMasterAutomate, FNumGlobalKnobs + Slot, 0, nil, Variation / 7);  // value is in opt
      //FG2.add_log_line('Send audio master ' + IntToStr(FNumGlobalKnobs + Slot) + ', Value ' + FloatToStr(Variation / 7), LOGCMD_NUL);
      //SetParameterAutomated( FNumGlobalKnobs + Slot, Variation / 7);
    //end;
    editorNeedsUpdate := True;
  except on E:Exception do begin
     FG2.add_log_line( 'OnVariationChange : ' + E.Message, LOGCMD_NUL);
     FG2.save_log;
    end;
  end;
end;

procedure APlugin.OnParamChange(Sender: TObject; SenderID : integer; Slot, Variation: byte;  Location: TLocationType; ModuleIndex, ParamIndex, Value: byte);
var KnobIndex : integer;
    Knob : TGlobalKnob;
begin
  if SenderID = FG2.ID then // Don't process messages that originate from this client
    exit;

  try
    KnobIndex := FG2.Performance.GlobalKnobList.FindGlobalKnobIndex( Slot, Location, ModuleIndex, ParamIndex);

    if KnobIndex = -1 then
      exit;

    Knob := FG2.Performance.GetGlobalKnob( KnobIndex);
    if Assigned(audioMaster) and assigned(Knob) then begin
      // Send parameter change to host
      audioMaster(Effect, audioMasterAutomate, KnobIndex, 0, nil, Knob.KnobFloatValue);  // value is in opt
      editorNeedsUpdate := True;
    end;
  except on E:Exception do begin
     FG2.add_log_line( 'OnParamChange : ' + E.Message, LOGCMD_NUL);
     FG2.save_log;
    end;
  end;
end;

end.


