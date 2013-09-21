unit UnitLog;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.Memo,
  FMX.StdCtrls;

type
  TfrmLog = class(TForm)
    Panel1: TPanel;
    Memo1: TMemo;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmLog: TfrmLog;

implementation

{$R *.fmx}

end.
