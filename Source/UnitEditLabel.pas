unit UnitEditLabel;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JawsCtrls;

type
  TfrmEditLabel = class(TForm)
    eLabel: DEdit;
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmEditLabel: TfrmEditLabel;

implementation

{$R *.dfm}

procedure TfrmEditLabel.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    ModalResult := mrOk
  else
    if Key = VK_ESCAPE then
      ModalResult := mrCancel;
end;

end.
