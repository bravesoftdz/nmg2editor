unit UnitG2EditorFMX;

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

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.Contnrs,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.Layouts,
  FMX.Memo, FMX.Ani, FMX.Edit, FMX.Effects, BVE.SVGControl,
  LibUSBWinDyn,
  g2_types, g2_file, g2_usb, g2_graph_FMX_2, FMX.Menus, Xml.xmldom, Xml.XMLIntf,
  BVE.SVGXMLWrapperDelphi, Xml.Win.msxmldom, Xml.XMLDoc, FMX.StdCtrls;

type
  TSVGSelection = class(TLayout)
  private
    FSelList : TObjectList;
  protected
    function  GetActive: boolean;
    procedure SetActive( const aValue : boolean);
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure SelectObject( aSVGModule : TG2Module);
    procedure AddToSelection( aSVGModule : TG2Module);
    procedure UnSelectAll;
    function  Selected( aSVGModule : TG2Module): boolean;

    property Active : boolean read GetActive write SetActive;
  end;

  TfrmSVGTest = class(TForm)
    OpenDialog1: TOpenDialog;
    Layout1: TLayout;
    tbZoom: TTrackBar;
    sb: TScrollBox;
    lSize: TLayout;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    miLoad: TMenuItem;
    miExit: TMenuItem;
    MenuItem4: TMenuItem;
    miCut: TMenuItem;
    miCopy: TMenuItem;
    miPaste: TMenuItem;
    miDelete: TMenuItem;
    Edit3: TEdit;
    Edit4: TEdit;
    TimerZoom: TTimer;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Label4: TLabel;
    eMemory: TEdit;
    XMLDocument1: TXMLDocument;
    TimerStartup: TTimer;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    Profiler: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure tbZoomChange(Sender: TObject);
    procedure sbClick(Sender: TObject);
    procedure miPasteClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miLoadClick(Sender: TObject);
    procedure TimerZoomTimer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure TimerStartupTimer(Sender: TObject);
    procedure SVGAgent1AfterParseNode(Sender: TObject; aNode: TSVGNode);
    procedure SVGAgent1BeforeParseNode(Sender: TObject; aNode: TSVGNode);
    procedure MenuItem3Click(Sender: TObject);
    procedure sbMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure sbMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure sbMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
  private
    { Private declarations }

    FZoom : single;
    FSVGSelection : TSVGSelection;

    FSVGModule : TG2Module;
    FSVGControl,
    FSVGCtrlSelected,
    FSVGCtrlOver : TG2Control;

    //FCableBitmapBuffer : TCableBitmapBuffer;
    FModuleBitmapBuffer : TModuleBitmapBuffer;

    FStartX, FStartY : single;
    procedure ApplicationIdle(Sender: TObject; var Done: Boolean);
    function CreateSVGGroup: TSVGGroup;

    procedure SetSelectedControl(const Value: TG2Control);
    procedure SetSelectedModule(const Value: TG2Module); public
    { Public declarations }
    FG2 : TG2GraphFMX;
    FPatch : TG2GraphPatchFMX;
    procedure CalcLayoutDimensions;
    procedure LoadSkin;
    procedure SetZoom( aZoom : single);

    procedure CreateModuleFMX(Sender : TObject; Module : TG2GraphModuleFMX);

    procedure G2AfterG2Init(Sender: TObject);

    procedure ControlMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure ControlMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure ControlMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);

    procedure ModuleMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure ModuleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure ModuleMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);

    property SelectedControl : TG2Control read FSVGControl write SetSelectedControl;
    property SelectedModule : TG2Module read FSVGModule write SetSelectedModule;

  end;

var
  frmSVGTest: TfrmSVGTest;

implementation

{$R *.fmx}

uses UnitLog;

//==============================================================================
//
//                               Utils
//
//==============================================================================

function RealignChildren( aControl : TFmxObject; dx, dy : single): TRectF;
var i : integer;
    c : TControl;
begin
  Result.Left := 100000;
  Result.Right := 0;
  Result.Top := 100000;
  Result.Bottom := 0;
  for i := 0 to aControl.ChildrenCount - 1 do begin
    if aControl.Children[i] is TControl then begin
      c := (aControl.Children[i] as TControl);
      c.Position.X := c.Position.X + dx;
      c.Position.Y := c.Position.Y + dy;

      if c.Position.X < Result.Left then
        Result.Left := c.Position.X;

      if c.Position.Y < Result.Top then
        Result.Top := c.Position.Y;

      if c.Position.X + c.Width > Result.Right then
        Result.Right := c.Position.X + c.Width;

      if c.Position.Y + c.Height > Result.Bottom then
        Result.Bottom := c.Position.Y + c.Height;
    end;
  end;
end;

function MemoryUsed: cardinal;
var st: TMemoryManagerState;
    sb: TSmallBlockTypeState;
begin
  GetMemoryManagerState(st);
  result := st.TotalAllocatedMediumBlockSize + st.TotalAllocatedLargeBlockSize;
  for sb in st.SmallBlockTypeStates do begin
    result := result + sb.UseableBlockSize * sb.AllocatedBlockCount;
  end;
  frmSVGTest.eMemory.Text := IntToStr(result);
end;

//==============================================================================
//
//                               TfrmSVGTest
//
//==============================================================================

procedure TfrmSVGTest.FormCreate(Sender: TObject);
var i : integer;
    G2DeviceList : TList;
begin
  FormatSettings.DecimalSeparator := '.';
  FZoom := tbZoom.Value;

  //SVGAgent := TG2SVGAgent.Create;

  FSVGSelection := TSVGSelection.Create(self);
  FSVGSelection.Parent := FModuleBitmapBuffer;
  FSVGSelection.Position.X := 0;
  FSVGSelection.Position.Y := 0;
  FSVGSelection.Width := sb.Width / tbZoom.Value;
  FSVGSelection.Height := sb.Height  / tbZoom.Value;
  FSVGSelection.Active := False;

  //FCableBitmapBuffer := TCableBitmapBuffer.Create(self);
  //FCableBitmapBuffer.Parent := lSize;

  //FCableBitmapBuffer.SetBounds(0,0,2000,1500);

  FModuleBitmapBuffer := TModuleBitmapBuffer.Create(self);
  FModuleBitmapBuffer.Parent := lSize;

  FModuleBitmapBuffer.OnControlMouseDown := ControlMouseDown;
  FModuleBitmapBuffer.OnControlMouseMOve := ControlMouseMove;
  FModuleBitmapBuffer.OnControlMouseUp := ControlMouseUp;
  FModuleBitmapBuffer.OnModuleMouseDown := ModuleMouseDown;
  FModuleBitmapBuffer.OnModuleMouseMOve := ModuleMouseMove;
  FModuleBitmapBuffer.OnModuleMouseUp := ModuleMouseUp;

  LoadSkin;

  FG2 := TG2GraphFMX.Create(self);
  FG2.LoadModuleDefs('');
  FG2.IsServer := True;

  FG2.ParseModulePanels;

  FPatch := FG2.Performance.Slot[0].Patch as TG2GraphPatchFMX;
  FPatch.Layout := FModuleBitmapBuffer;
  FPatch.OnCreateModuleFMX := CreateModuleFMX;

  //FCableBitmapBuffer.CableList := FPatch.CableList[1];
  FModuleBitmapBuffer.ModuleList := FPatch.ModuleList[1];

  FG2.OnAfterG2Init := G2AfterG2Init;

  G2DeviceList := TList.Create;
  try
    GetUSBDeviceList( G2DeviceList);

    if G2DeviceList.Count > 0 then begin
      //for i := 0 to G2DeviceList.Count - 1 do begin
      //  AddG2( pusb_device(G2DeviceList[i]));
      //end;
      FG2.G2USBDevice := pusb_device(G2DeviceList[0]);
    end else begin
      //AddG2( nil);
      ShowMessage('No g2 device found')
    end;
  finally
    G2DeviceList.Free;
  end;

  TimerStartup.Enabled := True;
end;

procedure TfrmSVGTest.FormDestroy(Sender: TObject);
begin
  FG2.USBActive := False;
  FG2.Free;
  //SVGAgent.Free;
end;

procedure TfrmSVGTest.ApplicationIdle(Sender: TObject; var Done: Boolean);
begin
  //miPaste.Enabled := Clipboard.HasFormat(cfBitmap);
end;

procedure TfrmSVGTest.miLoadClick(Sender: TObject);
var PatchName : string;
    i : integer;
    FileStream : TFileStream;
    aFileName : string;
begin
  FSVGSelection.UnselectAll;
  FSVGCtrlSelected := nil;
  FSVGCtrlOver := nil;

  SelectedModule :=  nil;
  SelectedControl := nil;

  if OpenDialog1.Execute then begin
    aFileName := OpenDialog1.FileName;

    FileStream := TFileStream.Create( aFileName, fmOpenRead);
    try
      // Take the patchname from the filename
      aFilename := ExtractFilename( aFileName);

      // Name patch max size = 16, if shorter end with 0
      PatchName := '';
      i := 1;
      while (i<=Length( aFileName)) and (i<=16) and ( aFileName[i] <> '.') do begin
        PatchName := PatchName + aFileName[i];
        inc(i);
      end;

      //frmProfiler.Active := True;

      FPatch.Init;
      FPatch.LoadFromFile(FileStream, nil);
      FPatch.InvalidateParameters;

      //frmProfiler.Active := False;

      //FCableBitmapBuffer.BringToFront;
      //FCableBitmapBuffer.RedrawBuffer := True;

      CalcLayoutDimensions;
    finally
      FileStream.Free;
    end;
  end;
  MemoryUsed;
end;

procedure TfrmSVGTest.miPasteClick(Sender: TObject);
begin
  //
end;

procedure TfrmSVGTest.Button1Click(Sender: TObject);
begin
  SetZoom(0.5);
end;

procedure TfrmSVGTest.Button2Click(Sender: TObject);
begin
  SetZoom(1);
end;

procedure TfrmSVGTest.Button3Click(Sender: TObject);
begin
  SetZoom(2);
end;

procedure TfrmSVGTest.CalcLayoutDimensions;
var R : TRectF;
begin
  if assigned(FModuleBitmapBuffer) then begin
    FModuleBitmapBuffer.CaLcBounds;
    lSize.Width := FModuleBitmapBuffer.Width;
    lSize.Height := FModuleBitmapBuffer.Height;
    {R := RealignChildren( FModuleBitmapBuffer, 0, 0);
    FModuleBitmapBuffer.Width := R.Right * tbZoom.Value;
    FModuleBitmapBuffer.Height := R.Bottom * tbZoom.Value;
    lSize.Width := FModuleBitmapBuffer.Width;
    lSize.Height := FModuleBitmapBuffer.Height;
    FModuleBitmapBuffer.Width := lSize.Width;
    FModuleBitmapBuffer.Height := lSize.Height;}
  end;
end;

procedure TfrmSVGTest.SetSelectedControl(const Value: TG2Control);
begin
  if Value <> FSVGControl then begin

    if assigned(FSVGControl) then begin
      FSVGControl.Selected := False;
    end;

    FSVGControl := Value;

    if assigned(FSVGControl) then begin
      FSVGControl.Selected := True;
    end;
  end;
end;

procedure TfrmSVGTest.SetSelectedModule(const Value: TG2Module);
begin
  if Value <> FSVGModule then begin

{    if assigned(FSVGModule) then begin
      FSVGModule.Selected := False;
    end;
}
    FSVGModule := Value;

{    if assigned(FSVGModule) then begin
      FSVGModule.Selected := True;
    end;
}  end;
end;

procedure TfrmSVGTest.SetZoom(aZoom: single);
var mx, my : single;
begin
  // Calc middle of currently visible rectangle of scrollbox

  mx := (sb.ViewportPosition.X + sb.Width/2) / FZoom;
  my := (sb.ViewportPosition.Y + sb.Height/2) / FZoom;

  FZoom := aZoom;

  FModuleBitmapBuffer.Scale.X := 1;
  FModuleBitmapBuffer.Scale.Y := 1;
  FModuleBitmapBuffer.Zoom := FZoom;

  FModuleBitmapBuffer.SetBounds(0,0,
                                (FModuleBitmapBuffer.MaxCol + 3) * UNITS_COL * FZoom,
                                (FModuleBitmapBuffer.MaxRow + 6) * UNITS_ROW * FZoom);
  lSize.Width := FModuleBitmapBuffer.Width;
  lSize.Height := FModuleBitmapBuffer.Height;

  //FCableBitmapBuffer.Scale.X := 1;
  //FCableBitmapBuffer.Scale.Y := 1;
  //FCableBitmapBuffer.Zoom := FZoom;
  //FCableBitmapBuffer.SetBounds(0,0, FModuleBitmapBuffer.Width, FModuleBitmapBuffer.Height);

  // Adjust scrollbox to keep centered on original mid point
  sb.ViewportPosition := PointF( mx * FZoom - (sb.Width/2), my * FZoom - (sb.Height/2));

  MemoryUsed;
end;

procedure TfrmSVGTest.tbZoomChange(Sender: TObject);
var mx, my : single;
begin
 // Calc middle of currently visible rectangle of scrollbox

  mx := (sb.ViewportPosition.X + sb.Width/2) / FZoom;
  my := (sb.ViewportPosition.Y + sb.Height/2) / FZoom;

  FZoom := tbZoom.Value;

  FModuleBitmapBuffer.Scale.X := (FZoom/FModuleBitmapBuffer.Zoom);
  FModuleBitmapBuffer.Scale.Y := (FZoom/FModuleBitmapBuffer.Zoom);

  //FCableBitmapBuffer.Scale.X := (FZoom/FCableBitmapBuffer.Zoom);
  //FCableBitmapBuffer.Scale.Y := (FZoom/FCableBitmapBuffer.Zoom);

  //FCableBitmapBuffer.Scale.X := 1;
  //FCableBitmapBuffer.Scale.Y := 1;
  //FCableBitmapBuffer.Zoom := FZoom;

  lSize.Width := FModuleBitmapBuffer.Width * FZoom;
  lSize.Height := FModuleBitmapBuffer.Height * FZoom;

  // Adjust scrollbox to keep centered on original mid point
  sb.ViewportPosition := PointF( mx * FZoom - (sb.Width/2), my * FZoom - (sb.Height/2));

  MemoryUsed;

  //TimerZoom.Enabled := False;
  //TimerZoom.Enabled := True;
end;

procedure TfrmSVGTest.TimerStartupTimer(Sender: TObject);
begin
  TimerStartup.Enabled := False;
  FG2.USBActive := True;

  MemoryUsed;
end;

procedure TfrmSVGTest.TimerZoomTimer(Sender: TObject);
begin
  exit;
  TimerZoom.Enabled := False;

  FModuleBitmapBuffer.Scale.X := 1;
  FModuleBitmapBuffer.Scale.Y := 1;
  FModuleBitmapBuffer.Zoom := FZoom;

  FModuleBitmapBuffer.Width := (FModuleBitmapBuffer.MaxCol + 3) * UNITS_COL * FZoom;
  FModuleBitmapBuffer.Height := (FModuleBitmapBuffer.MaxRow + 6) * UNITS_ROW * FZoom;
  lSize.Width := FModuleBitmapBuffer.Width;
  lSize.Height := FModuleBitmapBuffer.Height;

  //FCableBitmapBuffer.Scale.X := 1;
  //FCableBitmapBuffer.Scale.Y := 1;
  //FCableBitmapBuffer.Zoom := FZoom;

  MemoryUsed;
end;

procedure TfrmSVGTest.CreateModuleFMX(Sender: TObject;
  Module: TG2GraphModuleFMX);
begin
  {if assigned(Module) and assigned(Module.SVGControl) then begin
     Module.SVGControl.OnMouseDown := ControlMouseDown;
     Module.SVGControl.OnMouseMove := ControlMouseMove;
     Module.SVGControl.OnMouseUp := ControlMouseUp;
  end;}
  CalcLayoutDimensions;
end;

function TfrmSVGTest.CreateSVGGroup: TSVGGroup;
begin
end;

procedure TfrmSVGTest.LoadSkin;
begin
  //SVGAgent.LoadSkin( 'skin\g2_graphics.svg');
  CalcLayoutDimensions;
end;

procedure TfrmSVGTest.MenuItem3Click(Sender: TObject);
begin
  frmLog.Show;
end;

procedure TfrmSVGTest.sbClick(Sender: TObject);
begin
  FSVGSelection.UnSelectAll;
end;

procedure TfrmSVGTest.G2AfterG2Init(Sender: TObject);
begin
  //FCableBitmapBuffer.BringToFront;
  //FCableBitmapBuffer.RedrawBuffer := True;

  CalcLayoutDimensions;
  FPatch.InvalidateParameters;

  MemoryUsed;
end;

procedure TfrmSVGTest.SVGAgent1BeforeParseNode(Sender: TObject;
  aNode: TSVGNode);
begin
  //frmLog.Memo1.Lines.Add(' -> ' + aNode.ElementName);
end;

procedure TfrmSVGTest.SVGAgent1AfterParseNode(Sender: TObject; aNode: TSVGNode);
begin
  //frmLog.Memo1.Lines.Add(' <- ' + aNode.ElementName);
end;



//==============================================================================
//
//                                 Mouse
//
//==============================================================================

procedure TfrmSVGTest.ControlMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var P : TPointF;
begin
  if (Sender is TG2Control) then begin
    SelectedControl := Sender as TG2Control;

    P := SelectedControl.LocalToAbsolute(PointF(X, Y));
    P := FModuleBitmapBuffer.AbsoluteToLocal(P);

    FStartX := P.X;
    FStartY := P.Y;

    SelectedControl.ProcessMouseDown( Shift, P.X, P.Y, X, Y);
  end;
end;

procedure TfrmSVGTest.ModuleMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var P : TPointF;
begin
  if (Sender is TG2Module) then begin

    SelectedModule := Sender as TG2Module;
    SelectedControl := nil;
    P := SelectedModule.LocalToAbsolute(PointF(X, Y));
    P := FModuleBitmapBuffer.AbsoluteToLocal(P);

    FStartX := P.X;
    FStartY := P.Y;
  end;
end;

procedure TfrmSVGTest.sbMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var P: TPointF;
begin
  if Button = TMouseButton.mbLeft then begin

    SelectedControl := nil;
    SelectedModule := nil;

    P := PointF(X, Y);
    P := FModuleBitmapBuffer.AbsoluteToLocal(P);

    FStartX := P.X;
    FStartY := P.Y;
  end;
end;

procedure TfrmSVGTest.ControlMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Single);
var
  P: TPointF;
  dx, dy : single;
begin
  if not(Sender is TControl) then
    exit;

  P := (Sender as TControl).LocalToAbsolute(PointF(X, Y));
  P := FModuleBitmapBuffer.AbsoluteToLocal(P);
  dx := (P.X - FStartX) * FZoom;
  dy := (P.Y - FStartY) * FZoom;
  FStartX := P.X;
  FSTartY := P.Y;

  //if (ssLeft in Shift) then begin

    if FSVGSelection.Active then begin
      FSVGSelection.Position.X := FSVGSelection.Position.X + dx;
      FSVGSelection.Position.Y := FSVGSelection.Position.Y + dy;
    end else
      if assigned(SelectedControl) then begin
        SelectedControl.ProcessMouseMove( Shift, P.X, P.Y, X, Y, dx, dy);
      end else
        if assigned(SelectedModule) then begin
        end;
    //end;
end;

procedure TfrmSVGTest.ModuleMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
var P: TPointF;
    dx, dy : single;
begin
  if not(Sender is TControl) then
    exit;

  P := (Sender as TControl).LocalToAbsolute(PointF(X, Y));
  P := FModuleBitmapBuffer.AbsoluteToLocal(P);
  dx := (P.X - FStartX) * FZoom;
  dy := (P.Y - FStartY) * FZoom;
  FStartX := P.X;
  FSTartY := P.Y;

  if (ssLeft in Shift) then begin
    if FSVGSelection.Active then begin
      FSVGSelection.Position.X := FSVGSelection.Position.X + dx;
      FSVGSelection.Position.Y := FSVGSelection.Position.Y + dy;
    end else
      if assigned(SelectedControl) then begin
        SelectedControl.ProcessMouseMove( Shift, P.X, P.Y, -1, -1, dx, dy);
      end else
        if assigned(SelectedModule) then begin
          if (abs(P.X-FStartX)>0.1) or (abs(P.Y-FStartY)>0.1) then begin
            // Activate selection

            // If the control wasn't selected yet, then select it here
            if not FSVGSelection.Selected(SelectedModule) then begin
              if ssShift in Shift then
                FSVGSelection.AddToSelection(SelectedModule)
              else
                FSVGSelection.SelectObject(SelectedModule);
            end;

            FSVGSelection.Position.X := 0;
            FSVGSelection.Position.Y := 0;
            FSVGSelection.Width := sb.Width / tbZoom.Value;
            FSVGSelection.Height := sb.Height  / tbZoom.Value;
            FSVGSelection.Active := True;
          end;
        end;
  end;
end;


procedure TfrmSVGTest.sbMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
var dx, dy : single;
    P: TPointF;
begin
  P := PointF(X, Y);
  P := FModuleBitmapBuffer.AbsoluteToLocal(P);
  dx := (P.X - FStartX) * FZoom;
  dy := (P.Y - FStartY) * FZoom;
  FStartX := P.X;
  FSTartY := P.Y;

  if (ssLeft in Shift) then begin
    if FSVGSelection.Active then begin
      FSVGSelection.Position.X := FSVGSelection.Position.X + dx;
      FSVGSelection.Position.Y := FSVGSelection.Position.Y + dy;
    end else
      if assigned(SelectedControl) then begin
        SelectedControl.ProcessMouseMove( Shift, P.X, P.Y, -1, -1, dx, dy);
      end;
  end;
end;

procedure TfrmSVGTest.ControlMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var P : TPointF;
begin

  if (Sender is TG2Control) and (Sender = SelectedControl) then begin
    P := SelectedControl.LocalToAbsolute(PointF(X, Y));
    P := FModuleBitmapBuffer.AbsoluteToLocal(P);
    SelectedControl.ProcessMouseUp( Shift, P.X, P.Y, X, Y);
  end else begin
    if FSVGSelection.Active then begin
      // Deactivate the selection
      FSVGSelection.Active := False;
      CalcLayoutDimensions;
    end;
  end;
end;

procedure TfrmSVGTest.ModuleMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  if (Sender is TG2Module) and not(assigned(SelectedControl)) then begin

    // Add the control to the selection
    SelectedModule := Sender as TG2Module;

    if ssShift in Shift then
      FSVGSelection.AddToSelection(SelectedModule)
    else begin
      FSVGSelection.SelectObject(SelectedModule);
    end;
    SelectedModule := nil;
  end else
    if FSVGSelection.Active then begin
      // Deactivate the selection
      FSVGSelection.Active := False;
      CalcLayoutDimensions;
    end;
end;

procedure TfrmSVGTest.sbMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var i : integer;
begin
  if FSVGSelection.Active then begin
    FSVGSelection.Active := False;
    CalcLayoutDimensions;
  end;
end;


//==============================================================================
//
//                              TSelectionLayout
//
//==============================================================================

constructor TSVGSelection.Create(AOwner: TComponent);
begin
  inherited;

  HitTest := False;
  FSelList := TObjectList.Create(False);
end;

destructor TSVGSelection.Destroy;
begin
  FSelList.Free;

  inherited;
end;

procedure TSVGSelection.AddToSelection(aSVGModule: TG2Module);
var i : integer;
begin
{  i := FSelList.IndexOf( aSVGModule);
  if i <> -1 then begin
    FSelList.Delete(i);
    aSVGModule.Data.Selected := False;
  end else begin
    FSelList.Add(aSVGModule);
    aSVGModule.Data.Selected := True;
  end;}
end;

procedure TSVGSelection.UnSelectAll;
var SVGModule : TG2Module;
begin
  while FSelList.Count > 0 do begin
    SVGModule := FSelList[0] as TG2Module;
    SVGModule.ModuleData.Selected := False;
    FSelList.Delete(0);
  end;
end;

function TSVGSelection.Selected(aSVGModule: TG2Module): boolean;
begin
  Result := FSelList.IndexOf( aSVGModule) <> -1;
end;

procedure TSVGSelection.SelectObject(aSVGModule: TG2Module);
var i : integer;
begin
  i := FSelList.IndexOf( aSVGModule);
  if i <> -1 then begin
    UnSelectAll;
  end else begin
    UnSelectall;
    FSelList.Add(aSVGModule);
    aSVGModule.ModuleData.Selected := True;
  end;
end;

function TSVGSelection.GetActive: boolean;
begin
  Result := Visible;
end;

procedure TSVGSelection.SetActive(const aValue: boolean);
var i : integer;
    Control : TControl;
    Image : TImage;
    Bitmap : TBitmap;
    BitmapWidth, BitmapHeight : integer;
begin
  if aValue <> Visible then begin
    if aValue then begin
      DeleteChildren;
      for i := 0 to FSelList.Count - 1 do begin
        Control := (FSelList[i] as TControl);

        BitmapWidth := trunc(Control.Width * frmSVGTest.tbZoom.Value);
        BitmapHeight := trunc(Control.Height * frmSVGTest.tbZoom.Value);
        Bitmap := TBitmap.Create( BitmapWidth, BitmapHeight);
        //Bitmap := Control.MakeScreenshot;
        try
          //Bitmap.Canvas.SetMatrix( CreateScaleMatrix( frmSVGTest.tbZoom.Value, frmSVGTest.tbZoom.Value));

          Control.Scale.X := frmSVGTest.tbZoom.Value;
          Control.Scale.Y := frmSVGTest.tbZoom.Value;
          Control.PaintTo( Bitmap.Canvas, RectF(0, 0, BitmapWidth, BitmapHeight));
          Control.Scale.X := 1;
          Control.Scale.Y := 1;


          Image := TImage.Create( self);
          Image.Bitmap.Assign( Bitmap);
          Image.HitTest := False;
          Image.Position.Assign( Control.Position);
          Image.Width := Control.Width;
          Image.Height := Control.Height;
          Image.Parent := self;
        finally
          BitMap.Free;
        end;
        Control.Visible := False;
      end;
    end else begin
      for i := 0 to FSelList.Count - 1 do begin
        (FSelList[i] as TControl).Position.X := (FSelList[i] as TControl).Position.X + Position.X;
        (FSelList[i] as TControl).Position.Y := (FSelList[i] as TControl).Position.Y + Position.Y;
        (FSelList[i] as TControl).Visible := True;
        (FSelList[i] as TControl).Hittest := True;
      end;
      DeleteChildren;
    end;
    Visible := aValue;
  end;
end;


end.
