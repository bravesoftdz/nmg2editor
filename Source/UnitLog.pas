unit UnitLog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, G2_Types, g2_mess, G2_USB, g2_classes;

type
  TfrmLog = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    eCommand: TEdit;
    bSendMsg: TButton;
    Panel2: TPanel;
    bRefresh: TButton;
    bClear: TButton;
    procedure bSendMsgClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure bRefreshClick(Sender: TObject);
    procedure bClearClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmLog: TfrmLog;

implementation

uses UnitG2Editor;

{$R *.dfm}

{$if CompilerVersion <= 18.5}
function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean;
begin
  Result := C in CharSet;
end;
{$ifend}


procedure TfrmLog.bClearClick(Sender: TObject);
var G2 : TG2;
begin
  G2 := frmG2Main.SelectedG2;
  if assigned(G2) then begin
    G2.ClearLog;
    G2.AssignLog( Memo1.Lines);
  end;
end;

procedure TfrmLog.bRefreshClick(Sender: TObject);
var G2 : TG2;
begin
  G2 := frmG2Main.SelectedG2;
  if assigned(G2) then begin
    G2.AssignLog( Memo1.Lines);
  end;
end;

procedure TfrmLog.bSendMsgClick(Sender: TObject);
var MemStream : TG2SendMessage;
    G2 : TG2;

    i, j : integer;
    b : byte;

    function GetHexValue(h : char): integer;
    begin
      if CharInSet(h, ['0'..'9']) then
        Result := ord(h) - 48
      else
        Result := ord(h) - 55;
    end;

begin
  // Send a custom message to the G2

  MemStream := TG2SendMessage.Create;

  b := 0;
  i := 1;
  j := 0;
  while (i<= Length(eCommand.Text)) do begin

    if CharInSet(eCommand.Text[i], ['0'..'9']) or CharInSet(eCommand.Text[i], ['a'..'f'])
      or CharInSet(eCommand.Text[i], ['A'..'F']) then begin

      b := b * 16 + GetHexValue(uppercase(eCommand.Text)[i]);
      inc(j);

      if j = 2 then begin
        MemStream.Write(b, 1);
        j := 0;
      end;
    end;

    inc(i);
  end;

  G2 := frmG2Main.SelectedG2;
  if assigned(G2) then begin
    G2.SendCmdMessage( MemStream);
  end;
end;

procedure TfrmLog.FormCreate(Sender: TObject);
begin
  //frmG2Main.G2.LogLines := Memo1.Lines;
end;

procedure TfrmLog.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

procedure TfrmLog.FormShow(Sender: TObject);
var G2 : TG2;
begin
  G2 := frmG2Main.SelectedG2;
  if assigned(G2) then begin
    G2.AssignLog( Memo1.Lines);
  end;
end;

end.
