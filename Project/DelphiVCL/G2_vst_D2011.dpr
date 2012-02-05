{$J-,H+,T-,P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}

library G2_vst_D2011;
// This is the main project, you normally should not change
// anything here! Press Ctrl-F12 to access the main files!

uses
  uPlugin in '..\..\Source\VST\uPlugin.pas',
  uEditor in '..\..\Source\VST\uEditor.pas' {PluginEditorWindow},
  DAEffect;

var Effect : APlugin;
    Oome   : Boolean;

function main(audioMaster: TAudioMasterCallbackFunc): PAEffect; cdecl; export;
begin
  // get vst version
  if audioMaster(nil, audioMasterVersion, 0, 0, nil, 0) = 0 then begin
    Result := nil;
    Exit;
  end;

  Effect := APlugin.Create(audioMaster);
  if not Assigned(Effect) then begin
    Result := nil;
    Exit;
  end;

  if oome then begin
    Effect.Free;
    Result := nil;
    Exit;
  end;

  Result := Effect.Effect;
end;

exports
  Main name 'main';

begin
end.
