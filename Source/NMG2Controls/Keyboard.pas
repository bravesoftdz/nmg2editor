unit Keyboard;

interface

uses
  Windows, System.SysUtils, System.Classes, Vcl.Controls, Graphics;

type
  TKeyboardOrientation = ( kbVertikal, kbHorizontal);

  TKeyboard = class(TGraphicControl)
  private
    { Private declarations }
    FOrientation : TKeyboardOrientation;
    FOctaves : integer;
    FWhiteKeyWidth,
    FWhiteKeyLength,
    FBlackKeyWidth,
    FBlackKeyLength : integer;
    procedure CalcKeyDimensions;
  protected
    { Protected declarations }
    procedure SetOrientation( aValue : TKeyboardOrientation);
    procedure SetOctaves( aValue : integer);
  public
    { Public declarations }
    procedure paint; override;
  published
    { Published declarations }
    constructor Create( AOwner : TComponent);
    property Orientation : TKeyboardOrientation read FOrientation write SetOrientation;
    property Octaves : integer read FOctaves write SetOctaves;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('NM G2', [TKeyboard]);
end;

{ TKeyboard }

procedure TKeyboard.CalcKeyDimensions;
begin
  if FOrientation = kbHorizontal then begin
    FWhiteKeyWidth := ClientWidth div (7*FOctaves);
    FWhiteKeyLength := ClientHeight;
    FBlackKeyLength := round( FWhiteKeyLength * 0.6);
  end else begin
    FWhiteKeyWidth := ClientHeight div (7*FOctaves);
    FWhiteKeyLength := ClientWidth;
    FBlackKeyLength := round( FWhiteKeyLength * 0.6);
  end;
  FBlackKeyWidth := round(FWhiteKeyWidth * 0.7);
end;

constructor TKeyboard.Create(AOwner: TComponent);
begin
  inherited;
  FOctaves := 2;
  FOrientation := kbHorizontal;
end;

procedure TKeyboard.paint;
var i, j : integer;
    Rect : TRect;
begin
  canvas.Brush.Color := clRed;
  canvas.FillRect(ClientRect);

  CalcKeyDimensions;

  if FOrientation = kbHorizontal then begin
    Rect.Top := 0;
    Rect.Left := 0;
    Rect.Bottom := ClientHeight;
    Rect.Right := FWhiteKeyWidth;
  end else begin
    Rect.Top := ClientHeight - FWhiteKeyWidth;
    Rect.Left := 0;
    Rect.Bottom := ClientHeight;
    Rect.Right := ClientWidth;
  end;

  Canvas.Pen.Color := clBlack;

  for i := 0 to FOctaves - 1 do begin
    for j := 0 to 6 do begin
      if FOrientation = kbHorizontal then begin
        Canvas.Brush.Color := clWhite;
        Canvas.FillRect( Rect);
        Canvas.Brush.Color := clBlack;
        Canvas.FrameRect(Rect);
        Rect.Left := Rect.Right;
        Rect.Right := Rect.Right + FWhiteKeyWidth;
      end else begin
        Canvas.Brush.Color := clWhite;
        Canvas.FillRect( Rect);
        Canvas.Brush.Color := clBlack;
        Canvas.FrameRect(Rect);
        Rect.Bottom := Rect.Top;
        Rect.Top := Rect.Top - FWhiteKeyWidth;
      end;
    end;
  end;

  if FOrientation = kbHorizontal then begin
    Rect.Top := 0;
    Rect.Left := FWhiteKeyWidth - FBlackKeyWidth div 2;
    Rect.Bottom := FBlackKeyLength;
    Rect.Right := FWhiteKeyWidth + FBlackKeyWidth div 2;
  end else begin
    Rect.Top := ClientHeight - FWhiteKeyWidth - FBlackKeyWidth div 2;
    Rect.Left := 0;
    Rect.Bottom := ClientHeight - FWhiteKeyWidth + FBlackKeyWidth div 2;
    Rect.Right := FBlackKeyLength;
  end;

  Canvas.Brush.Color := clBlack;
  Canvas.Pen.Color := clBlack;

  for i := 0 to FOctaves - 1 do begin
    for j := 0 to 6 do begin
      if FOrientation = kbHorizontal then begin
        if (j <> 2) and (j<>6) then
          Canvas.FillRect( Rect);
        Rect.Left := Rect.Left + FWhiteKeyWidth;
        Rect.Right := Rect.Right + FWhiteKeyWidth;
      end else begin
        if (j <> 2) and (j<>6) then
          Canvas.FillRect( Rect);
        Rect.Bottom := Rect.Bottom- FWhiteKeyWidth;
        Rect.Top := Rect.Top - FWhiteKeyWidth;
      end;
    end;
  end;

  inherited;
end;

procedure TKeyboard.SetOctaves(aValue: integer);
begin
  FOctaves := aValue;
  Invalidate;
end;

procedure TKeyboard.SetOrientation(aValue: TKeyboardOrientation);
begin
  FOrientation := aValue;
  Invalidate;
end;

end.
