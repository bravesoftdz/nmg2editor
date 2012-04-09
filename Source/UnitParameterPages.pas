unit UnitParameterPages;

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
  Dialogs, G2_Graph, StdCtrls, G2_Types, G2_File, G2_classes, ExtCtrls,
  graph_util_vcl, g2_database, DOM, XMLRead, XMLWrite;

type
  TfrmParameterPages = class(TForm)
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
    Panel1: TPanel;
    obParam: TG2GraphButtonRadio;
    obPage: TG2GraphButtonRadio;
    Disp1B: TG2GraphDisplay;
    disp2B: TG2GraphDisplay;
    Disp3B: TG2GraphDisplay;
    Disp4B: TG2GraphDisplay;
    Disp5B: TG2GraphDisplay;
    Disp6B: TG2GraphDisplay;
    Disp7B: TG2GraphDisplay;
    Disp8B: TG2GraphDisplay;
    btGlobalPages: TG2GraphButtonText;
    Panel2: TPanel;
    bfP1: TG2GraphButtonFlat;
    bfP2: TG2GraphButtonFlat;
    bfP3: TG2GraphButtonFlat;
    bfP4: TG2GraphButtonFlat;
    bfP5: TG2GraphButtonFlat;
    bfP6: TG2GraphButtonFlat;
    bfP7: TG2GraphButtonFlat;
    bfP8: TG2GraphButtonFlat;
    procedure obParamClick(Sender: TObject);
    procedure obPageClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btGlobalPagesChange(Sender: TObject);
  private
    { Private declarations }
    FExtBitmap       : TBitmap;
    FKnobArray       : array[0..7] of TG2GraphKnob;
    FDispKnobArray   : array[0..7] of TG2GraphDisplay;
    FDispModuleArray : array[0..7] of TG2GraphDisplay;
    FButtonArray     : array[0..7] of TG2GraphButtonFlat;
{$IFDEF FPC}
{$ELSE}
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure WMEraseBkGnd(var msg: TWMEraseBkGnd); message WM_ERASEBKGND;
{$ENDIF}
  public
    { Public declarations }
    procedure LoadIniXML;
    procedure UpdateControls;
    procedure UpdateColorScema;
    function GetKnobIndexOffset : integer;
  end;

var
  frmParameterPages: TfrmParameterPages;

implementation
uses
  UnitG2Editor;

{$IFNDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

{ TfrmParameterPages }

procedure TfrmParameterPages.FormCreate(Sender: TObject);
begin
  FExtBitmap := TBitmap.Create;
  FExtBitMap.PixelFormat := pf24Bit;

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

  LoadIniXML;
end;

procedure TfrmParameterPages.FormDestroy(Sender: TObject);
begin
  FExtBitmap.Free;
end;

procedure TfrmParameterPages.LoadIniXML;
var Doc : TXMLDocument;
    RootNode : TDOMNode;
    FormSettingsNode : TXMLFormSettingsType;
begin
  if not FileExists('G2_editor_ini.xml') then
    exit;

  Doc := TXMLDocument.Create;
  try
    ReadXMLFile( Doc, 'G2_editor_ini.xml');

    RootNode := Doc.FindNode('G2_Editor_settings');
    if assigned(RootNode) then begin
      FormSettingsNode := TXMLFormSettingsType(RootNode.FindNode('ParameterPagesForm'));
      if assigned(FormSettingsNode) then begin
        SetFormPosition( self,
                         FormSettingsNode.PosX,
                         FormSettingsNode.PosY,
                         FormSettingsNode.SizeX,
                         FormSettingsNode.SizeY);
        Visible := FormSettingsNode.Visible;
      end;
    end;
  finally
    Doc.Free;
  end;
end;

{$IFDEF FPC}
{$ELSE}
procedure TfrmParameterPages.WMEraseBkGnd(var msg: TWMEraseBkGnd);
begin
  msg.result := 1;
end;

procedure TfrmParameterPages.WMPaint(var Msg: TWMPaint);
var PS        : TPaintStruct;
    i         : integer;
    Rect      : TRect;
    Control   : TG2GraphChildControl;
begin
  BeginPaint(Handle, PS);

  if PS.rcPaint.Right - PS.rcPaint.Left > FExtBitmap.Width then
    FExtBitmap.Width := PS.rcPaint.Right - PS.rcPaint.Left;
  if PS.rcPaint.Bottom - PS.rcPaint.Top > FExtBitmap.Height then
    FExtBitmap.Height := PS.rcPaint.Bottom - PS.rcPaint.Top;

  //PaintBackGround( FExtBitmap.Canvas, PS.rcPaint);
  Rect.Left := 0;
  Rect.Right := PS.rcPaint.Right - PS.rcPaint.Left;
  Rect.Top := 0;
  Rect.Bottom := PS.rcPaint.Bottom - PS.rcPaint.Top;

  FExtBitmap.Canvas.Brush.Color := Color;
  FExtBitmap.Canvas.FillRect( Rect);

  for i := 0 to ControlCount - 1 do begin

    if Controls[i] is TG2GraphChildControl then begin
      Control := Controls[i] as TG2GraphChildControl;
      if RectOverlap( Control.BoundsRect, PS.rcPaint) then
        Control.PaintOn( FExtBitmap.Canvas, PS.rcPaint);
      Msg.Result := 0;
    end;
  end;

  Rect.Left := 0;
  Rect.Top := 0;
  Rect.Right := PS.rcPaint.Right - PS.rcPaint.Left;
  Rect.Bottom := PS.rcPaint.Bottom - PS.rcPaint.Top;

  Canvas.CopyRect( PS.rcPaint, FExtBitmap.Canvas, Rect);

  EndPaint(Handle, PS);
end;
{$ENDIF}

procedure TfrmParameterPages.FormShow(Sender: TObject);
begin
  UpdateControls;
end;

function TfrmParameterPages.GetKnobIndexOffset: integer;
begin
  Result := obPage.Value * 8 + obParam.Value * 8 * 3;
end;

procedure TfrmParameterPages.obPageClick(Sender: TObject);
begin
  UpdateControls;
end;

procedure TfrmParameterPages.obParamClick(Sender: TObject);
begin
  UpdateControls;
end;

procedure TfrmParameterPages.btGlobalPagesChange(Sender: TObject);
begin
  UpdateControls;
end;

procedure TfrmParameterPages.UpdateControls;
var i : integer;
    Perf : TG2Performance;
    Patch : TG2Patch;
    Module : TG2FileModule;
    Knob : TKnob;
    G2 : TG2;
begin
  G2 := frmG2Main.SelectedG2;
  if assigned(G2) then begin

    UpdateColorScema;

    if btGlobalPages.Value = 0 then begin

      Patch := G2.SelectedPatch as TG2Patch;
      for i := 0 to 7 do begin
        Knob := Patch.GetKnob( GetKnobIndexOffset + i);
        FDispModuleArray[i].TextFunction := 1000;
        if assigned(Knob) and (Knob.IsAssigned = 1) then begin
          FKnobArray[i].SetParameter( Knob.Parameter as TG2GraphParameter);
          FDispKnobArray[i].SetParameter( Knob.Parameter as TG2GraphParameter);
          FDispModuleArray[i].SetParameter( Knob.Parameter as TG2GraphParameter);
          if assigned(Knob.Parameter.ButtonParam) then
            FButtonArray[i].SetParameter( Knob.Parameter.ButtonParam);
        end else begin
          FKnobArray[i].SetParameter( nil);
          FDispKnobArray[i].SetParameter( nil);
          FDispModuleArray[i].SetParameter( nil);
          FButtonArray[i].SetParameter( nil);
        end;
      end;

    end else begin

      Perf := G2.Performance;
      for i := 0 to 7 do begin
        Knob := Perf.GetGlobalKnob( GetKnobIndexOffset + i);
        FDispModuleArray[i].TextFunction := 1002;
        if assigned(Knob) and (Knob.IsAssigned = 1) then begin
          FKnobArray[i].SetParameter( Knob.Parameter as TG2GraphParameter);
          FDispKnobArray[i].SetParameter( Knob.Parameter as TG2GraphParameter);
          FDispModuleArray[i].SetParameter( Knob.Parameter as TG2GraphParameter);
          if assigned(Knob.Parameter.ButtonParam) then
            FButtonArray[i].SetParameter( Knob.Parameter.ButtonParam);
        end else begin
          FKnobArray[i].SetParameter( nil);
          FDispKnobArray[i].SetParameter( nil);
          FDispModuleArray[i].SetParameter( nil);
          FButtonArray[i].SetParameter( nil);
        end;
      end;
    end;
  end;
  Invalidate;
end;

procedure TfrmParameterPages.UpdateColorScema;
begin
  btGlobalPages.HightlightColor := G_HighlightColor;
  obParam.HightlightColor := G_HighLightColor;
  obPage.HightlightColor := G_HighlightColor;
end;



end.
