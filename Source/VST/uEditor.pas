unit uEditor;

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
//  ////////////////////////////////////////////////////////////////////////////

interface
uses Windows, Forms, DAudioEffectX, Messages, ExtCtrls,
     Classes, Graphics, StdCtrls, Controls, DVstUtils,
     DVstTemplate, DAEffectX,
     g2_types, g2_graph, g2_file, g2_usb, g2_classes;

type
  TPluginEditorWindow = class(TForm)
    Ctrl: TLabel;
    Updater: TTimer;
    Panel2: TPanel;
    Label1: TLabel;
    Label3: TLabel;
    rbVariationA: TG2GraphButtonRadio;
    eNameA: TEdit;
    Panel4: TPanel;
    Label2: TLabel;
    Label4: TLabel;
    rbVariationD: TG2GraphButtonRadio;
    eNameD: TEdit;
    Panel5: TPanel;
    Label5: TLabel;
    Label6: TLabel;
    rbVariationC: TG2GraphButtonRadio;
    eNameC: TEdit;
    Panel6: TPanel;
    Label7: TLabel;
    Label8: TLabel;
    rbVariationB: TG2GraphButtonRadio;
    eNameB: TEdit;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    G2GraphPanel1: TG2GraphPanel;
    skP1: TG2GraphKnob;
    skP2: TG2GraphKnob;
    skP3: TG2GraphKnob;
    skP4: TG2GraphKnob;
    skP5: TG2GraphKnob;
    skP6: TG2GraphKnob;
    skP7: TG2GraphKnob;
    skP8: TG2GraphKnob;
    Disp1A: TG2GraphDisplay;
    Disp2A: TG2GraphDisplay;
    Disp3A: TG2GraphDisplay;
    Disp4A: TG2GraphDisplay;
    Disp5A: TG2GraphDisplay;
    Disp6A: TG2GraphDisplay;
    Disp7A: TG2GraphDisplay;
    Disp8A: TG2GraphDisplay;
    Disp1B: TG2GraphDisplay;
    disp2B: TG2GraphDisplay;
    Disp3B: TG2GraphDisplay;
    Disp4B: TG2GraphDisplay;
    Disp5B: TG2GraphDisplay;
    Disp6B: TG2GraphDisplay;
    Disp7B: TG2GraphDisplay;
    Disp8B: TG2GraphDisplay;
    Button1: TButton;
    G2GraphPanel2: TG2GraphPanel;
    rbPage: TG2GraphButtonRadio;
    rbPageColumn: TG2GraphButtonRadio;
    G2GraphLabel1: TG2GraphLabel;
    Panel1: TPanel;
    lbStatus: TLabel;
    bfP1: TG2GraphButtonFlat;
    bfP2: TG2GraphButtonFlat;
    bfP3: TG2GraphButtonFlat;
    bfP4: TG2GraphButtonFlat;
    bfP5: TG2GraphButtonFlat;
    bfP6: TG2GraphButtonFlat;
    bfP7: TG2GraphButtonFlat;
    bfP8: TG2GraphButtonFlat;
    procedure UpdaterTimer(Sender: TObject);
    procedure rbPageColumnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure rbPageClick(Sender: TObject);
    procedure rbVariationAClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure skPChange(Sender: TObject);
    procedure bfPChange(Sender: TObject);
  private
    FDisableControls : boolean;
    FEffect    : TVstTemplate;
    FKnobArray       : array[0..7] of TG2GraphKnob;
    FDispKnobArray   : array[0..7] of TG2GraphDisplay;
    FDispModuleArray : array[0..7] of TG2GraphDisplay;
    FButtonArray     : array[0..7] of TG2GraphButtonFlat;
    procedure OnEditorOpen(var Msg: TMessage); message WM_EDITOROPEN;
  public
    procedure UpdateControls;
    //procedure ConnectControls;
    function  GetKnobIndexOffset : integer;

    property Effect: TVstTemplate read FEffect write FEffect;
  end;

implementation
uses SysUtils, uPlugin;
{$R *.DFM}

procedure TPluginEditorWindow.Button1Click(Sender: TObject);
begin
  (Effect as APlugin).FG2.save_log;
end;

procedure TPluginEditorWindow.FormCreate(Sender: TObject);
begin
  DoubleBuffered := False;

  FDispModuleArray[0] := Disp1B;
  FDispModuleArray[1] := Disp2B;
  FDispModuleArray[2] := Disp3B;
  FDispModuleArray[3] := Disp4B;
  FDispModuleArray[4] := Disp5B;
  FDispModuleArray[5] := Disp6B;
  FDispModuleArray[6] := Disp7B;
  FDispModuleArray[7] := Disp8B;

  FDispKnobArray[0] := Disp1A;
  FDispKnobArray[1] := Disp2A;
  FDispKnobArray[2] := Disp3A;
  FDispKnobArray[3] := Disp4A;
  FDispKnobArray[4] := Disp5A;
  FDispKnobArray[5] := Disp6A;
  FDispKnobArray[6] := Disp7A;
  FDispKnobArray[7] := Disp8A;

  FKnobArray[0] := skP1;
  FKnobArray[1] := skP2;
  FKnobArray[2] := skP3;
  FKnobArray[3] := skP4;
  FKnobArray[4] := skP5;
  FKnobArray[5] := skP6;
  FKnobArray[6] := skP7;
  FKnobArray[7] := skP8;

  FButtonArray[0] := bfP1;
  FButtonArray[1] := bfP2;
  FButtonArray[2] := bfP3;
  FButtonArray[3] := bfP4;
  FButtonArray[4] := bfP5;
  FButtonArray[5] := bfP6;
  FButtonArray[6] := bfP7;
  FButtonArray[7] := bfP8;
end;

procedure TPluginEditorWindow.UpdaterTimer(Sender: TObject);
begin
 try
   if (not assigned(Effect)) or not(Effect.editorNeedsUpdate) then
     exit;

    Effect.editorNeedsUpdate := false;
    UpdateControls;
  except on E:Exception do begin
     (Effect as APlugin).FG2.add_log_line('Update timer : ' + E.Message, LOGCMD_NUL);
     (Effect as APlugin).FG2.save_log;
    end;
  end;
end;

procedure TPluginEditorWindow.UpdateControls;
var i, j : integer;
    Perf : TG2USBPerformance;
    Knob : TGlobalKnob;
    Module : TG2FileModule;
    Param, ButtonParam : TG2FileParameter;
    Status : string;
begin
  FDisableControls := true;
  (Effect as APlugin).FG2.Lock;
  try
    try
      lbStatus.Caption := (Effect as APlugin).GetStatusText;

      eNameA.Text        := (Effect as APlugin).FG2.GetSlot(0).PatchName;
      rbVariationA.Value := (Effect as APlugin).FG2.GetSlot(0).GetPatch.ActiveVariation;
      eNameB.Text        := (Effect as APlugin).FG2.GetSlot(1).PatchName;
      rbVariationB.Value := (Effect as APlugin).FG2.GetSlot(1).GetPatch.ActiveVariation;
      eNameC.Text        := (Effect as APlugin).FG2.GetSlot(2).PatchName;
      rbVariationC.Value := (Effect as APlugin).FG2.GetSlot(2).GetPatch.ActiveVariation;
      eNameD.Text        := (Effect as APlugin).FG2.GetSlot(3).PatchName;
      rbVariationD.Value := (Effect as APlugin).FG2.GetSlot(3).GetPatch.ActiveVariation;

      Perf := (Effect as APlugin).FG2.GetPerformance;
      for i := 0 to 7 do begin
        Knob := Perf.GetGlobalKnob( GetKnobIndexOffset + i);
        if assigned(Knob) and (Knob.IsAssigned = 1) and assigned(Knob.Parameter) then begin
          Param := Knob.Parameter;
          FKnobArray[i].Value := Knob.KnobValue;
          FDispKnobArray[i].Line[0] := Param.TextFunction(1001, 0, 2);
          FDispKnobArray[i].Line[1] := Param.TextFunction(1001, 1, 2);
          FDispModuleArray[i].Line[0] := Param.TextFunction(1002, 0, 1);
          FButtonArray[i].ButtonText.Clear;
          if assigned(Param.ButtonParam) then begin
            ButtonParam := Param.ButtonParam;
            for j := 0 to ButtonParam.ButtonTextCount - 1 do
              FButtonArray[i].ButtonText.Add( ButtonParam.ButtonText[j]);
            FButtonArray[i].Value := ButtonParam.GetParameterValue;
            FButtonArray[i].LowValue := ButtonParam.LowValue;
            FButtonArray[i].HighValue := ButtonParam.HighValue;
          end;
        end else begin
          FKnobArray[i].Value := 0;
          FDispKnobArray[i].Line[0] := '';
          FDispKnobArray[i].Line[1] := '';
          FDispModuleArray[i].Line[0] := '';
          FButtonArray[i].ButtonText.Clear;
        end;
      end;
    except on E:Exception do begin
       (Effect as APlugin).FG2.add_log_line('UpdateControls : ' + E.Message, LOGCMD_NUL);
       (Effect as APlugin).FG2.save_log;
      end;
    end;
  finally
    (Effect as APlugin).FG2.UnLock;
    FDisableControls := false;
  end;
end;

function TPluginEditorWindow.GetKnobIndexOffset: integer;
begin
  Result := rbPageColumn.Value * 8 + rbPage.Value * 8 * 3;
end;

procedure TPluginEditorWindow.rbPageClick(Sender: TObject);
begin
  Effect.editorNeedsUpdate := True;
end;

procedure TPluginEditorWindow.rbPageColumnClick(Sender: TObject);
begin
  Effect.editorNeedsUpdate := True;
end;

procedure TPluginEditorWindow.rbVariationAClick(Sender: TObject);
begin
  if FDisableControls then
    exit;

  if Sender is TG2GraphButtonRadio then begin
    with Sender as TG2GraphButtonRadio do
      case Tag of
      0 : begin
           (Effect as APlugin).FG2.GetSlot(0).GetPatch.Variation := rbVariationA.Value;
           (Effect as APlugin).setParameterAutomated((Effect as APlugin).FNumGlobalKnobs, rbVariationA.Value / 7);
          end;
      1 : begin
           (Effect as APlugin).FG2.GetSlot(1).GetPatch.Variation := rbVariationB.Value;
           (Effect as APlugin).setParameterAutomated((Effect as APlugin).FNumGlobalKnobs + 1, rbVariationB.Value / 7);
          end;
      2 : begin
           (Effect as APlugin).FG2.GetSlot(2).GetPatch.Variation := rbVariationC.Value;
           (Effect as APlugin).setParameterAutomated((Effect as APlugin).FNumGlobalKnobs + 2, rbVariationC.Value / 7);
          end;
      3 : begin
           (Effect as APlugin).FG2.GetSlot(3).GetPatch.Variation := rbVariationD.Value;
           (Effect as APlugin).setParameterAutomated((Effect as APlugin).FNumGlobalKnobs + 3, rbVariationD.Value / 7);
          end;
      end;

    Effect.editorNeedsUpdate := True;
  end;
end;

procedure TPluginEditorWindow.bfPChange(Sender: TObject);
var KnobIndex : integer;
    Knob : TGlobalKnob;
    G2 : TG2USB;
    ButtonParam : TG2FileParameter;
begin
  if FDisableControls then
    exit;

  G2 := (Effect as APlugin).FG2;

  if Sender is TG2GraphButtonFlat then
    try
      G2.Lock;
      with (Sender) as TG2GraphButtonFlat do begin

        KnobIndex := GetKnobIndexOffset + tag;
        Knob := G2.GetPerformance.GetGlobalKnob( KnobIndex);
        if (not assigned(Knob)) or (Knob.IsAssigned = 0) or (not assigned(Knob.Parameter)) then
          exit;

        ButtonParam := Knob.Parameter.ButtonParam;
        if not assigned(ButtonParam) then
          exit;

        case tag of
        0 : begin
             Knob.KnobButtonValue := Value;
             (Effect as APlugin).setParameterAutomated( KnobIndex + 120, Knob.KnobButtonFloatValue);
            end;
        1 : begin
             Knob.KnobButtonValue := Value;
             (Effect as APlugin).setParameterAutomated( KnobIndex + 120, Knob.KnobButtonFloatValue);
            end;
        2 : begin
             Knob.KnobButtonValue := Value;
             (Effect as APlugin).setParameterAutomated( KnobIndex + 120, Knob.KnobButtonFloatValue);
            end;
        3 : begin
             Knob.KnobButtonValue := Value;
             (Effect as APlugin).setParameterAutomated( KnobIndex + 120, Knob.KnobButtonFloatValue);
            end;
        4 : begin
             Knob.KnobButtonValue := Value;
             (Effect as APlugin).setParameterAutomated( KnobIndex + 120, Knob.KnobButtonFloatValue);
            end;
        5 : begin
             Knob.KnobButtonValue := Value;
             (Effect as APlugin).setParameterAutomated( KnobIndex + 120, Knob.KnobButtonFloatValue);
            end;
        6 : begin
             Knob.KnobButtonValue := Value;
             (Effect as APlugin).setParameterAutomated( KnobIndex + 120, Knob.KnobButtonFloatValue);
            end;
        7 : begin
             Knob.KnobButtonValue := Value;
             (Effect as APlugin).setParameterAutomated( KnobIndex + 120, Knob.KnobButtonFloatValue);
            end;
        end;
      end;
    finally
      G2.Unlock;
    end;
end;

procedure TPluginEditorWindow.skPChange(Sender: TObject);
var KnobIndex : integer;
    Knob : TGlobalKnob;
    G2 : TG2USB;
begin
  if FDisableControls then
    exit;

  G2 := (Effect as APlugin).FG2;

  if Sender is TG2GraphKnob then
    try
      G2.Lock;
      with (Sender) as TG2GraphKnob do begin

        KnobIndex := GetKnobIndexOffset + tag;
        Knob := G2.GetPerformance.GetGlobalKnob( KnobIndex);
        if not assigned(Knob) then
          exit;

        case tag of
        0 : begin
             Knob.KnobValue := Value;
             (Effect as APlugin).setParameterAutomated( KnobIndex, Knob.KnobFloatValue);
            end;
        1 : begin
             Knob.KnobValue := Value;
             (Effect as APlugin).setParameterAutomated( KnobIndex, Knob.KnobFloatValue);
            end;
        2 : begin
             Knob.KnobValue := Value;
             (Effect as APlugin).setParameterAutomated( KnobIndex, Knob.KnobFloatValue);
            end;
        3 : begin
             Knob.KnobValue := Value;
             (Effect as APlugin).setParameterAutomated( KnobIndex, Knob.KnobFloatValue);
            end;
        4 : begin
             Knob.KnobValue := Value;
             (Effect as APlugin).setParameterAutomated( KnobIndex, Knob.KnobFloatValue);
            end;
        5 : begin
             Knob.KnobValue := Value;
             (Effect as APlugin).setParameterAutomated( KnobIndex, Knob.KnobFloatValue);
            end;
        6 : begin
             Knob.KnobValue := Value;
             (Effect as APlugin).setParameterAutomated( KnobIndex, Knob.KnobFloatValue);
            end;
        7 : begin
             Knob.KnobValue := Value;
             (Effect as APlugin).setParameterAutomated( KnobIndex, Knob.KnobFloatValue);
            end;
        end;
      end;
    finally
      G2.Unlock;
    end;
end;

procedure TPluginEditorWindow.OnEditorOpen(var Msg: TMessage);
begin
  try
    Effect := TVstTemplate(Msg.WParam);
    Effect.editorNeedsUpdate := True;
  except on E:Exception do begin
     (Effect as APlugin).FG2.add_log_line('OnEditorOpen : ' + E.Message, LOGCMD_NUL);
     (Effect as APlugin).FG2.save_log;
    end;
  end;
end;

end.

