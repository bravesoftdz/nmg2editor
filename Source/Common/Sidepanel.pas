unit Sidepanel;

interface

uses
  Winapi.Windows, Winapi.Messages, Vcl.Forms, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.ExtCtrls, Vcl.Graphics;

type
  TSidepanelState = (spsCollapsed, spsExpanded);

  TSidePanelCaption = class(TCustomControl)
  private
    FCaptionHeight : integer;
    FSidepanelState : TSidepanelState;
  protected
    procedure SetSidePanelState( const aSidePanelState : TSidePanelState);
    procedure Paint; override;
  public
    constructor Create(AOwner : TComponent); override;

    property SidePanelState : TSidePanelState read FSidePanelState write SetSidePanelState;
  end;

  TSidePanel = class(TCustomPanel)
  private
    FPanel : TSidePanelCaption;
    FExpandedWidth : integer;
    FDragging : Boolean;
    FLastPos : TPoint;
    FOnCollapse : TNotifyEvent;
    FOnExpand : TNotifyEvent;
  protected
    //procedure Paint; override;
    procedure Resize; override;

    procedure CaptionClick(Sender: TObject);

    function GetCaption : string;
    procedure SetCaption( const aValue : string);
    function GetCaptionHeight : integer;
    procedure SetCaptionHeight( const aValue : integer);
    function GetSidePanelState : TSidePanelState;
    procedure SetSidePanelState( const aValue : TSidePanelState);
    function GetCaptionColor : TColor;
    procedure SetCaptionColor( const aValue : TColor);

    procedure MouseDown(Button: TMouseButton; Shift:
      TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;

  published
    property Caption : string read GetCaption write SetCaption;
    property SidePanelState : TSidePanelState read GetSidePanelState write SetSidePanelState;
    property CaptionHeight : integer read GetCaptionHeight write SetCaptionHeight;
    property CaptionColor : TColor read GetCaptionColor write SetCaptionColor;
    property Color;
    property OnCollapse : TNotifyEvent read FOnCollapse write FOnCollapse;
    property OnExpand : TNotifyEvent read FOnExpand write FOnExpand;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('NM G2', [TSidePanel]);
end;

{ TSidePanel }

constructor TSidePanel.Create(AOwner: TComponent);
begin
  inherited;
  Color := clBtnFace;
  Width := 100;
  Height := 100;
  Align := alLeft;

  FPanel := TSidePanelCaption.Create(self);
  FPanel.Parent := self;
  FPanel.Height := 20;
  FPanel.Align := alTop;
  FPanel.Caption := Caption;
  FPanel.OnClick := CaptionClick;
end;

function TSidePanel.GetCaption: string;
begin
  Result := FPanel.Caption;
end;


procedure TSidePanel.SetCaption(const aValue: string);
begin
  if aValue <> FPanel.Caption then begin
    Fpanel.Caption := aValue;
  end;
end;

function TSidePanel.GetSidePanelState: TSidePanelState;
begin
  Result := FPanel.SidePanelState;
end;

procedure TSidePanel.SetSidePanelState(const aValue: TSidePanelState);
var i : integer;
begin
  if aValue <> FPanel.SidePanelState then begin

    case FPanel.SidepanelState of
      spsExpanded :
        begin
          for i := 0 to ControlCount - 1 do begin
            if Controls[i] is TSidePanelCaption then begin
              //
            end else begin
              Controls[i].Visible := False;
            end;
          end;

          FPanel.SidepanelState := spsCollapsed;
          FExpandedWidth := Width;
          Width := FPanel.FCaptionHeight;

          if assigned(FOnCollapse) then
            FOnCollapse(self);
        end;
      spsCollapsed :
        begin
          for i := ControlCount - 1 downto 0 do begin
            if Controls[i] is TSidePanelCaption then begin
              //
            end else begin
              Controls[i].Visible := True;
            end;
          end;
          Width := FExpandedWidth;
          FPanel.SidepanelState := spsExpanded;
          FPanel.Top := 0;

          if assigned(FOnExpand) then
            FOnExpand(self);
        end;
    end;
  end;
end;

function TSidePanel.GetCaptionHeight: integer;
begin
  Result := FPanel.FCaptionHeight;
end;


procedure TSidePanel.SetCaptionHeight(const aValue: integer);
begin
  if aValue <> FPanel.FCaptionHeight then begin
    FPanel.FCaptionHeight := aValue;
    Invalidate;
  end;
end;

function TSidePanel.GetCaptionColor: TColor;
begin
  Result := FPanel.Color;
end;

procedure TSidePanel.SetCaptionColor(const aValue: TColor);
begin
  if aValue <> FPanel.Color then begin
    FPanel.Color := aVAlue;
  end;
end;

procedure TSidePanel.CaptionClick(Sender: TObject);
begin
   if SidepanelState = spsExpanded then
     SidePanelState := spsCollapsed
   else
     SidePanelState := spsExpanded;
end;


procedure TSidePanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if FPanel.SidePanelState = spsExpanded then begin
    if Align = alLeft then begin
      if (Button = mbLeft) and ((Width - x ) < 10) then begin
        FDragging := True;
        FLastPos := Point(x, y);
        MouseCapture := true;
        Screen.cursor := crSizeWE;
      end else
        inherited;
    end else
      if (Button = mbLeft) and ((Width - x ) < 10) and
        ((Height - y ) < 10) then begin
        FDragging := True;
        FLastPos := Point(x, y);
        MouseCapture := true;
        Screen.cursor := crSizeNWSE;
      end else
        inherited;
  end else
    inherited;
end;

procedure TSidePanel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  r: TRect;
begin
  if FDragging then begin
    r := BoundsRect;
    SetBounds( r.left, r.top, r.right - r.left + X - FlastPos.X,
    r.bottom - r.top + Y - Flastpos.Y );
    FLastPos := Point( x, y );
  end else begin

    inherited;
    Cursor := crDefault;

    if FPanel.SidePanelState = spsExpanded then begin
      if Align = alLeft then begin
        if (Width - x ) < 10 then begin
          Cursor := crSizeWE
        end else
          if ((Width - x ) < 10) and ((Height - y ) < 10) then
            Cursor := crSizeNWSE;
      end;
    end;

  end;
end;

procedure TSidePanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if FDragging then
  begin
    FDragging := False;
    MouseCapture := false;
    Screen.Cursor := crDefault;
  end
  else
    inherited;
end;

{procedure TSidePanel.Paint;
var
  x, y: Integer;
begin
  inherited;

  Canvas.Font.Name := 'Marlett';
  Canvas.Font.Size := 10;
  Canvas.Brush.Style := bsClear;
  x := clientwidth - canvas.textwidth('o');
  y := clientheight - canvas.textheight('o');
  canvas.textout( x, y, 'o' );
end;}

procedure TSidePanel.Resize;
begin
  inherited;
  FPanel.Top := 0;
end;

{ TSidePanelCaption }

constructor TSidePanelCaption.Create(AOwner: TComponent);
begin
  inherited;
  ParentColor := True;
  ParentFont := True;
  Color := clBtnFace;
  Width := 100;
  Height := 100;

  FSidepanelState := spsExpanded;
  FCaptionHeight := 16;
  Height := FCaptionHeight;
  Constraints.MaxHeight := Height;
end;

procedure TSidePanelCaption.Paint;

  procedure DrawArrowLeft( aLeft, aTop, aSize : integer);
  //var Points : array[0..2] of TPoint;
  var old_font : TFont;
  begin
    {Points[0].X := aLeft;         Points[0].Y := aTop + aSize div 2;
    Points[1].X := aLeft + aSize; Points[1].Y := aTop;
    Points[2].X := aLeft + aSize; Points[2].Y := aTop + aSize;
    Canvas.Polygon( Points);}
    old_font := TFont.Create;
    try
      old_font.Assign(Canvas.Font);
      Canvas.Font.Name := 'Marlett';
      Canvas.Font.Size := aSize;
      Canvas.TextOut( aLeft, aTop, '3' );
      Canvas.Font.Assign(old_font);
    finally
      old_font.Free;
    end;
  end;

  procedure DrawArrowRight( aLeft, aTop, aSize : integer);
  //var Points : array[0..2] of TPoint;
  var old_font : TFont;
  begin
    {Points[0].X := aLeft;         Points[0].Y := aTop;
    Points[1].X := aLeft + aSize; Points[1].Y := aTop + aSize div 2;
    Points[2].X := aLeft;         Points[2].Y := aTop + aSize;
    Canvas.Polygon( Points);}
    old_font := TFont.Create;
    try
      old_font.Assign(Canvas.Font);
      Canvas.Font.Name := 'Marlett';
      Canvas.Font.Size := aSize;
      Canvas.TextOut( aLeft, aTop, '4' );
      Canvas.Font.Assign(old_font);
    finally
      old_font.Free;
    end;
  end;

  procedure TextAngle( aLeft, aTop : integer; aAngle : integer; aText : string);
  var old_font : TFont;
      lf : TLogFont;
      tf : TFont;
  begin
    old_font := TFont.Create;
    try
      old_font.Assign(Canvas.Font);
      with Canvas do begin
        //Font.Name := 'Arial';
        //Font.Size := 24;
        tf := TFont.Create;
        try
          tf.Assign(Font) ;
          GetObject(tf.Handle, sizeof(lf), @lf) ;
          lf.lfEscapement := aAngle;
          lf.lfOrientation := aAngle;
          tf.Handle := CreateFontIndirect(lf) ;
          Font.Assign(tf) ;
        finally
          tf.Free;
        end;
        TextOut(aLeft, aTop, aText) ;
      end;
      Canvas.Font.Assign(old_font);
    finally
      old_font.Free;
    end;
  end;

begin
  inherited;

  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := clGray;
  Canvas.Rectangle( ClientRect);

  case FSidepanelState of
    spsCollapsed :
      begin
        //Canvas.Brush.Color := clGray;
        //Canvas.Brush.Style := bsSolid;
        DrawArrowRight( 2, 1, 10);
        Canvas.Brush.Style := bsClear;
        TextAngle( 0, Canvas.TextWidth(Caption) + 4 + 10, 900, Caption);
      end;
    spsExpanded :
      begin
        //Canvas.Brush.Color := clGray;
        //Canvas.Brush.Style := bsSolid;
        DrawArrowLeft( Width - 12, 1, 10);
        Canvas.Brush.Style := bsClear;
        TextAngle( 4, 1, 0, Caption);
      end;
  end;
end;

procedure TSidePanelCaption.SetSidePanelState(
  const aSidePanelState: TSidePanelState);
begin
  if FSidePanelState <> aSidePanelState then begin
    FSidePanelState := aSidePanelState;
    case FSidePanelState of
      spsCollapsed :
        begin
          Constraints.MaxHeight := 0;
          Align := alClient;
        end;
      spsExpanded :
        begin
          Align := alTop;
          Height := FCaptionHeight;
          Constraints.MaxHeight := Height;
        end;
    end;
    Invalidate;
  end;
end;

end.
