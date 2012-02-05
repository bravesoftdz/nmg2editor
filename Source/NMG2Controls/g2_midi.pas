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
//  Some midi functions
//
//  ////////////////////////////////////////////////////////////////////////////


interface
uses
  Classes, SysUtils, g2_types, g2_file, g2_graph;

implementation

procedure SysExAllControllersRequest;
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

  // TODO SendMidiLong(@SysEx[0], 7);
end;

procedure SysExPatchRequestByFileIndex(Bank, Patch: byte);
var
  SysEx : packed array of byte;
begin
  if (Bank < 1) or (Bank > 32) then
    raise Exception.Create('Bank must be in the range 1-32.');

  if (Patch < 1) or (Bank > 128) then
    raise Exception.Create('Patch must be in the range 1-128.');

  SetLength(SysEx, 10);
  SysEx[0] := $F0;
  SysEx[1] := $33;
  SysEx[2] := $7F;
  SysEx[3] := $0A;
  SysEx[4] := $31;
  SysEx[5] := Bank;
  SysEx[6] := Patch;
  SysEx[7] := $F7;

  // TODO SendMidiLong(@SysEx[0], 8);
end;

procedure SysExPatchRequestBySlot(Slot: byte);
var
  SysEx : packed array of byte;
begin
  if (Slot<1) or (Slot>4) then
    raise Exception.Create('Slot must be in the range 1-4.');

  SetLength(SysEx, 10);
  SysEx[0] := $F0;
  SysEx[1] := $33;
  SysEx[2] := $7F;
  SysEx[3] := $0A;
  SysEx[4] := $30;
  SysEx[5] := Slot - 1;
  SysEx[6] := $00;
  SysEx[7] := $F7;

  // TODO SendMidiLong(@SysEx[0], 8);
end;

procedure SysExPerformanceRequestByFileIndex(Bank, Perf: byte);
var
  SysEx : packed array of byte;
begin
  if (Bank < 1) or (Bank > 8) then
    raise Exception.Create('Bank must be in the range 1-8.');

  if (Perf < 1) or (Perf > 128) then
    raise Exception.Create('Perf must be in the range 1-128.');

  SetLength(SysEx, 10);
  SysEx[0] := $F0;
  SysEx[1] := $33;
  SysEx[2] := $7F;
  SysEx[3] := $0A;
  SysEx[4] := $39;
  SysEx[5] := Bank;
  SysEx[6] := Perf;
  SysEx[7] := $F7;

  // TODO SendMidiLong(@SysEx[0], 8);
end;

procedure SysExPerformanceRequest;
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

  // TODO SendMidiLong(@SysEx[0], 8);
end;



end.
