unit UnitPatchSettings;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFDEF FPC}
{$ELSE}
  Windows,
{$ENDIF}
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, g2_types, g2_file, g2_graph, g2_classes,
  UnitG2Editor;

type
  TfrmPatchSettings = class(TForm)
    G2GraphPanel1: TG2GraphPanel;
    G2GraphLabel1: TG2GraphLabel;
    G2GraphLabel2: TG2GraphLabel;
    G2GraphLabel3: TG2GraphLabel;
    G2GraphLabel4: TG2GraphLabel;
    G2GraphLabel5: TG2GraphLabel;
    G2GraphLabel6: TG2GraphLabel;
    kBendRange: TG2GraphKnob;
    kGlideSpeed: TG2GraphKnob;
    kVibratoDepth: TG2GraphKnob;
    kVibratoRate: TG2GraphKnob;
    obArpeggiatorOnOff: TG2GraphButtonRadio;
    obArpOctaves: TG2GraphButtonRadio;
    obArpSpeed: TG2GraphButtonRadio;
    obBendOnOff: TG2GraphButtonRadio;
    obGlideType: TG2GraphButtonRadio;
    obOctaveShift: TG2GraphButtonRadio;
    obSustainPedalOnOff: TG2GraphButtonRadio;
    obVibratoMod: TG2GraphButtonRadio;
    VibratoRate: TLabel;
    obArpDir: TG2GraphButtonRadio;
    procedure PatchCtrlMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
    //procedure PatchCtrlMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure UpdateControls( Variation : TVariation);
  end;

var
  frmPatchSettings: TfrmPatchSettings;

implementation

{$IFNDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TfrmPatchSettings.PatchCtrlMouseUp(Sender: TObject;  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

//end;

{procedure TfrmPatchSettings.PatchCtrlMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin}
  if Assigned(frmG2Main) and (Sender is TG2GraphChildControl) then
    frmG2Main.ParameterClick( Sender, Button, Shift, X, Y, (Sender as TG2GraphChildControl).Parameter);
end;

procedure TfrmPatchSettings.UpdateControls( Variation : TVariation);
var Patch : TG2Patch;
begin
  Patch := frmG2Main.G2.SelectedPatch;

  obSustainPedalOnOff.Parameter := Patch.Parameter[ PATCH_SUSTAIN, SUSTAIN_PEDAL];
  obOctaveShift.Parameter       := Patch.Parameter[ PATCH_SUSTAIN, OCTAVE_SHIFT];
  obBendOnOff.Parameter         := Patch.Parameter[ PATCH_BEND, BEND_ON_OFF];
  kBendRange.Parameter          := Patch.Parameter[ PATCH_BEND, BEND_RANGE];
  obArpeggiatorOnOff.Parameter  := Patch.Parameter[ PATCH_ARPEGGIATOR, ARP_ON_OFF];
  obArpOctaves.Parameter        := Patch.Parameter[ PATCH_ARPEGGIATOR, ARP_OCTAVES];
  obArpDir.Parameter            := Patch.Parameter[ PATCH_ARPEGGIATOR, ARP_DIRECTION];
  obArpSpeed.Parameter          := Patch.Parameter[ PATCH_ARPEGGIATOR, ARP_SPEED];
  obVibratoMod.Parameter        := Patch.Parameter[ PATCH_ARPEGGIATOR, ARP_SPEED];
  obGlideType.Parameter         := Patch.Parameter[ PATCH_GLIDE, GLIDE_TYPE];
  kGlideSpeed.Parameter         := Patch.Parameter[ PATCH_GLIDE, GLIDE_SPEED];
  kVibratoRate.Parameter        := Patch.Parameter[ PATCH_VIBRATO, VIBRATO_RATE];
  kVibratoDepth.Parameter       := Patch.Parameter[ PATCH_VIBRATO, VIBRATO_DEPTH];
end;

end.