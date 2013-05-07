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
  FMX.Memo, FMX.Ani, FMX.Edit, FMX.Effects, SVGControl,
  g2_types, g2_file, g2_usb, g2_graph_FMX, FMX.Menus;

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

    procedure SelectObject( aSVGModule : TSVGG2Module);
    procedure AddToSelection( aSVGModule : TSVGG2Module);
    procedure UnSelectAll;
    function  Selected( aSVGModule : TSVGG2Module): boolean;

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
    procedure FormCreate(Sender: TObject);
    procedure tbZoomChange(Sender: TObject);
    procedure sbClick(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure miPasteClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miLoadClick(Sender: TObject);
    procedure TimerZoomTimer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }

    FZoom : single;
    FSVGSelection : TSVGSelection;
    FSVGModule : TSVGG2Module;
    FSVGHitpath : TSVGHitpath;
    FSVGCtrlSelected,
    FSVGCtrlOver : TSVGG2Control;

    FCableBitmapBuffer : TCableBitmapBuffer;
    FModuleBitmapBuffer : TModuleBitmapBuffer;

    FDX, FDY : single;
    procedure ApplicationIdle(Sender: TObject; var Done: Boolean);
    function CreateSVGGroup: TSVGGroup;
  public
    { Public declarations }
    FG2 : TG2GraphFMX;
    FPatch : TG2GraphPatchFMX;
    procedure CalcLayoutDimensions;
    procedure LoadSkin;
    procedure SetZoom( aZoom : single);

    procedure CreateModuleFMX(Sender : TObject; Module : TG2GraphModuleFMX);

    procedure SVGMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure SVGMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure SVGMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
  end;

var
  frmSVGTest: TfrmSVGTest;

implementation

{$R *.fmx}

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
begin
  FormatSettings.DecimalSeparator := '.';
  FZoom := tbZoom.Value;

  SVGAgent := TG2SVGAgent.Create;

  FSVGSelection := TSVGSelection.Create(self);
  FSVGSelection.Parent := FModuleBitmapBuffer;
  FSVGSelection.Position.X := 0;
  FSVGSelection.Position.Y := 0;
  FSVGSelection.Width := sb.Width / tbZoom.Value;
  FSVGSelection.Height := sb.Height  / tbZoom.Value;
  FSVGSelection.Active := False;

  LoadSkin;

  FG2 := TG2GraphFMX.Create(self);
  FG2.LoadModuleDefs('');
  FG2.IsServer := True;

  FCableBitmapBuffer := TCableBitmapBuffer.Create(self);
  lSize.AddObject(FCableBitmapBuffer);
  FCableBitmapBuffer.SetBounds(0,0,2000,1500);
  FModuleBitmapBuffer := TModuleBitmapBuffer.Create(self);
  lSize.AddObject(FModuleBitmapBuffer);

  FPatch := FG2.Performance.Slot[0].Patch as TG2GraphPatchFMX;
  FPatch.Layout := FModuleBitmapBuffer;
  FPatch.OnCreateModuleFMX := CreateModuleFMX;

  FCableBitmapBuffer.CableList := FPatch.CableList[1];
  FModuleBitmapBuffer.ModuleList := FPatch.ModuleList[1];

  //Application.OnIdle := ApplicationIdle;
  MemoryUsed;
end;

procedure TfrmSVGTest.FormDestroy(Sender: TObject);
begin
  FG2.Free;
  SVGAgent.Free;
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
  FSVGModule :=  nil;
  FSVGCtrlSelected := nil;
  FSVGCtrlOver := nil;

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

      FPatch.Init;
      FPatch.LoadFromFile(FileStream, nil);
      FCableBitmapBuffer.BringToFront;
      FCableBitmapBuffer.RedrawBuffer := True;

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
    {R := RealignChildren( FModuleBitmapBuffer, 0, 0);
    FModuleBitmapBuffer.Width := R.Right * tbZoom.Value;
    FModuleBitmapBuffer.Height := R.Bottom * tbZoom.Value;
    lSize.Width := FModuleBitmapBuffer.Width;
    lSize.Height := FModuleBitmapBuffer.Height;
    FModuleBitmapBuffer.Width := lSize.Width;
    FModuleBitmapBuffer.Height := lSize.Height;}
  end;
end;

procedure TfrmSVGTest.SetZoom(aZoom: single);
var mx, my : single;
begin
  if sb.HScrollBar.Visible then
    mx := (sb.Width/2 + sb.HScrollBar.Value) / FZoom
  else
    mx := (sb.Width/2) / FZoom;

  if sb.VScrollBar.Visible then
    my := (sb.Height/2 + sb.VScrollBar.Value) / FZoom
  else
    my := (sb.Height/2) / FZoom;

  FZoom := aZoom;

  FModuleBitmapBuffer.Scale.X := 1;
  FModuleBitmapBuffer.Scale.Y := 1;
  FModuleBitmapBuffer.Zoom := FZoom;

  //FModuleBitmapBuffer.Width := (FModuleBitmapBuffer.MaxCol + 3) * UNITS_COL * FZoom;
  //FModuleBitmapBuffer.Height := (FModuleBitmapBuffer.MaxRow + 6) * UNITS_ROW * FZoom;
  FModuleBitmapBuffer.SetBounds(0,0,
                                (FModuleBitmapBuffer.MaxCol + 3) * UNITS_COL * FZoom,
                                (FModuleBitmapBuffer.MaxRow + 6) * UNITS_ROW * FZoom);
  lSize.Width := FModuleBitmapBuffer.Width;
  lSize.Height := FModuleBitmapBuffer.Height;

  FCableBitmapBuffer.Scale.X := 1;
  FCableBitmapBuffer.Scale.Y := 1;
  FCableBitmapBuffer.Zoom := FZoom;
  FCableBitmapBuffer.SetBounds(0,0, FModuleBitmapBuffer.Width, FModuleBitmapBuffer.Height);

  sb.HScrollBar.Value := mx * FZoom - (sb.Width/2);
  sb.VScrollBar.Value := my * FZoom - (sb.Height/2);

  MemoryUsed;
end;

procedure TfrmSVGTest.tbZoomChange(Sender: TObject);
var mx, my : single;
begin
  if sb.HScrollBar.Visible then
    mx := (sb.Width/2 + sb.HScrollBar.Value) / FZoom
  else
    mx := (sb.Width/2) / FZoom;

  if sb.VScrollBar.Visible then
    my := (sb.Height/2 + sb.VScrollBar.Value) / FZoom
  else
    my := (sb.Height/2) / FZoom;

  FZoom := tbZoom.Value;

  FModuleBitmapBuffer.Scale.X := (FZoom/FModuleBitmapBuffer.Zoom);
  FModuleBitmapBuffer.Scale.Y := (FZoom/FModuleBitmapBuffer.Zoom);
  FCableBitmapBuffer.Scale.X := (FZoom/FCableBitmapBuffer.Zoom);
  FCableBitmapBuffer.Scale.Y := (FZoom/FCableBitmapBuffer.Zoom);

  lSize.Width := FModuleBitmapBuffer.Width * FZoom;
  lSize.Height := FModuleBitmapBuffer.Height * FZoom;

  sb.HScrollBar.Value := mx * FZoom - (sb.Width/2);
  sb.VScrollBar.Value := my * FZoom - (sb.Height/2);

  TimerZoom.Enabled := False;
  TimerZoom.Enabled := True;
end;

procedure TfrmSVGTest.TimerZoomTimer(Sender: TObject);
begin
  TimerZoom.Enabled := False;

  FModuleBitmapBuffer.Scale.X := 1;
  FModuleBitmapBuffer.Scale.Y := 1;
  FModuleBitmapBuffer.Zoom := FZoom;

  FModuleBitmapBuffer.Width := (FModuleBitmapBuffer.MaxCol + 3) * UNITS_COL * FZoom;
  FModuleBitmapBuffer.Height := (FModuleBitmapBuffer.MaxRow + 6) * UNITS_ROW * FZoom;
  lSize.Width := FModuleBitmapBuffer.Width;
  lSize.Height := FModuleBitmapBuffer.Height;

  FCableBitmapBuffer.Scale.X := 1;
  FCableBitmapBuffer.Scale.Y := 1;
  FCableBitmapBuffer.Zoom := FZoom;

  MemoryUsed;
end;

procedure TfrmSVGTest.CreateModuleFMX(Sender: TObject;
  Module: TG2GraphModuleFMX);
begin
  if assigned(Module) and assigned(Module.SVGControl) then begin
     Module.SVGControl.OnMouseDown := SVGMouseDown;
     Module.SVGControl.OnMouseMove := SVGMouseMove;
     Module.SVGControl.OnMouseUp := SVGMouseUp;
  end;
  CalcLayoutDimensions;
end;

function TfrmSVGTest.CreateSVGGroup: TSVGGroup;
begin
end;

procedure TfrmSVGTest.LoadSkin;
begin
  SVGAgent.LoadSkin( 'skin\g2_graphics.svg');
  CalcLayoutDimensions;
end;

procedure TfrmSVGTest.sbClick(Sender: TObject);
begin
  FSVGSelection.UnSelectAll;
end;

procedure TfrmSVGTest.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var P: TPointF;
begin
  if Button = TMouseButton.mbLeft then begin

    // Get startpoint in lZoom layout

    P := PointF(X, Y);
    P := FModuleBitmapBuffer.AbsoluteToLocal(P);

    FDX := P.X;
    FDY := P.Y;
    FSVGCtrlSelected := nil;
    FSVGModule := nil;
  end;
end;

procedure TfrmSVGTest.SVGMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var Module : TSVGG2Module;
    P : TPointF;
begin
  if (Sender is TSVGG2Module) then begin

    FSVGModule := Sender as TSVGG2Module;
    P := FSVGModule.LocalToAbsolute(PointF(X, Y));
    P := FModuleBitmapBuffer.AbsoluteToLocal(P);

    Edit3.Text := FSVGModule.ID + ' MsDn';
    Edit4.Text := FSVGModule.ID;

    FSVGHitpath := FSVGModule.SelectHitpath( PointF(X , Y));
    if assigned( FSVGHitpath) then begin
      Edit2.Text := FSVGHitpath.ID;
      FSVGHitpath.MouseDown( P);
    end;
  end;
end;

procedure TfrmSVGTest.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
var dx, dy : single;
    P: TPointF;
begin
  if (ssLeft in Shift) then
    if assigned(FSVGHitpath) then begin
      if assigned(FSVGModule) then begin

        P := PointF(X, Y);
        P := FModuleBitmapBuffer.AbsoluteToLocal(P);
        dx := P.X - FDX;
        dy := P.Y - FDY;

        FSVGHitpath.MouseMove( P, dx, dy);
      end;
    end else
      if FSVGSelection.Active then begin

        P := PointF(X, Y);
        P := FModuleBitmapBuffer.AbsoluteToLocal(P);

        FSVGSelection.Position.X := P.X - FDX;
        FSVGSelection.Position.Y := P.Y - FDY;
      end;
end;

procedure TfrmSVGTest.SVGMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Single);
var
  P: TPointF;
  Control : TSVGG2Control;
begin
  if not(Sender is TSVGG2Module) then
    exit;

  FSVGModule := Sender as TSVGG2Module;
  P := FSVGModule.LocalToAbsolute(PointF(X, Y));
  P := FModuleBitmapBuffer.AbsoluteToLocal(P);

  Edit3.Text := FSVGModule.ID + ' MsMv';

  if (ssLeft in Shift) then begin

    if (abs(P.X-FDX)>0.1) or (abs(P.Y-FDY)>0.1) then begin
      if (not assigned(FSVGCtrlSelected)) and (not FSVGSelection.Active) then begin
        // Select something...

        // Check if movement is over a control
        FSVGCtrlSelected := FSVGModule.SelectControl( PointF(X , Y));
        if assigned(FSVGCtrlSelected) then begin
          Edit2.Text := FSVGCtrlSelected.ID;
          // If the parentcontrol isn't currently selected, select it here
          if not FSVGSelection.Selected(FSVGModule) then begin
            if ssShift in Shift then
              FSVGSelection.AddToSelection( FSVGModule)
            else
              FSVGSelection.SelectObject( FSVGModule);
          end;

        end else begin
          // Activate selection

          // If the control wasn't selected yet, then select it here
          if not FSVGSelection.Selected(FSVGModule) then begin
            if ssShift in Shift then
              FSVGSelection.AddToSelection( FSVGModule)
            else
              FSVGSelection.SelectObject( FSVGModule);
          end;

          FSVGSelection.Position.X := 0;
          FSVGSelection.Position.Y := 0;
          FSVGSelection.Width := sb.Width / tbZoom.Value;
          FSVGSelection.Height := sb.Height  / tbZoom.Value;
          FSVGSelection.Active := True;

          FDX := P.X;
          FDY := P.Y;
        end;
      end;
    end;
  end else begin
    Control := FSVGModule.SelectControl( PointF(X , Y));
    if assigned(Control) then begin
      if assigned(FSVGCtrlOver) and (FSVGCtrlOver <> Control) then
        FSVGCtrlOver.MouseLeave;
      FSVGCtrlOver := Control;
      FSVGCtrlOver.MouseMove( P, nil, P.X-FDX, P.Y-FDY);
    end else begin
      if assigned(FSVGCtrlOver) then
        FSVGCtrlOver.MouseLeave;
      FSVGCtrlOver := nil;
    end;
  end;
end;

procedure TfrmSVGTest.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var i : integer;
begin
  if FSVGSelection.Active then begin
    FSVGSelection.Active := False;
    CalcLayoutDimensions;
  end;
end;

procedure TfrmSVGTest.SVGMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var Module : TSVGG2Module;
    P : TPointF;
begin
  // If FSVGControl is assigned, then the last action was a selection movement
  if (Sender is TSVGG2Module) then begin

    Module := Sender as TSVGG2Module;
    P := Module.LocalToAbsolute(PointF(X, Y));
    P := FModuleBitmapBuffer.AbsoluteToLocal(P);

    Edit3.Text := Module.ID + ' MsUp';
    Edit4.Text := FSVGModule.ID;

    if not(assigned(FSVGModule)) then begin
      FSVGModule := Module;
      FSVGCtrlSelected := nil;

      if ssShift in Shift then
        FSVGSelection.AddToSelection( FSVGModule)
      else
        FSVGSelection.SelectObject( FSVGModule);

      FSVGHitpath := FSVGModule.SelectHitpath( PointF(X, Y));
      if assigned(FSVGHitpath) then begin
        FSVGHitpath.MouseUp( P);
      end;


    end else begin

      if assigned(FSVGHitpath) then begin
        FSVGHitpath.MouseUp( P);
      end;

      if not FSVGSelection.Selected(FSVGModule) then begin
        if ssShift in Shift then
          FSVGSelection.AddToSelection( FSVGModule)
        else
          FSVGSelection.SelectObject( FSVGModule);
      end;

    end;
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

procedure TSVGSelection.AddToSelection(aSVGModule: TSVGG2Module);
var i : integer;
begin
  i := FSelList.IndexOf( aSVGModule);
  if i <> -1 then begin
    FSelList.Delete(i);
    aSVGModule.Data.Selected := False;
  end else begin
    FSelList.Add(aSVGModule);
    aSVGModule.Data.Selected := True;
  end;
end;

procedure TSVGSelection.UnSelectAll;
var SVGModule : TSVGG2Module;
begin
  while FSelList.Count > 0 do begin
    SVGModule := FSelList[0] as TSVGG2Module;
    SVGModule.Data.Selected := False;
    FSelList.Delete(0);
  end;
end;

function TSVGSelection.Selected(aSVGModule: TSVGG2Module): boolean;
begin
  Result := FSelList.IndexOf( aSVGModule) <> -1;
end;

procedure TSVGSelection.SelectObject(aSVGModule: TSVGG2Module);
var i : integer;
begin
  i := FSelList.IndexOf( aSVGModule);
  if i <> -1 then begin
    UnSelectAll;
  end else begin
    UnSelectall;
    FSelList.Add(aSVGModule);
    aSVGModule.Data.Selected := True;
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
