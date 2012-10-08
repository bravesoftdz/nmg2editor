unit UnitPatchBrowserFilterModules;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.CheckLst,
  g2_database;

type
  TfrmPatchBrowserModuleFilter = class(TForm)
    cblModules: TCheckListBox;
    Panel1: TPanel;
    bOk: TButton;
    bCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FSelectedModules : TStringList;
    procedure UpdateModules( aModuleDefList : TXMLModuleDefListType);
    function ModuleChecked( aModuleID : integer): boolean;
    procedure UpdateSelectedModules;
  end;

var
  frmPatchBrowserModuleFilter: TfrmPatchBrowserModuleFilter;

implementation

{$R *.dfm}

procedure TfrmPatchBrowserModuleFilter.UpdateModules( aModuleDefList : TXMLModuleDefListType);
var m : integer;
    sl : TStringList;
    i : integer;
begin
  sl := TStringList.Create;
  sl.Sorted := True;
  for m := 0 to aModuleDefList.Count - 1 do begin
    i := sl.Add( aModuleDefList.ModuleDef[m].ShortName);
    sl.Objects[i] := pointer(aModuleDefList.ModuleDef[m].ModuleType);
  end;

  for m := 0 to sl.Count - 1 do begin
    i := cblModules.Items.Add(sl[m]);
    cblModules.Items.Objects[i] := sl.Objects[m];
  end;
end;

procedure TfrmPatchBrowserModuleFilter.FormCreate(Sender: TObject);
begin
  FSelectedModules := TStringList.Create;
end;

procedure TfrmPatchBrowserModuleFilter.FormDestroy(Sender: TObject);
begin
  FSelectedModules.Free;
end;

function TfrmPatchBrowserModuleFilter.ModuleChecked( aModuleID : integer): boolean;
var i : integer;
begin
  i := 0;
  while (i<cblModules.Items.Count) and (integer(cblModules.Items.Objects[i]) <> aModuleID) do
    inc(i);

  if (i<cblModules.Items.Count) then begin
    Result := cblModules.Checked[i]
  end else
    Result := False;
end;

procedure TfrmPatchBrowserModuleFilter.UpdateSelectedModules;
var i, m : integer;
begin
  FSelectedModules.Clear;
  for i := 0 to cblModules.Count - 1 do begin
    if cblModules.Checked[i] then begin
      m := FSelectedModules.Add(cblModules.Items[i]);
      FSelectedModules.Objects[m] := cblModules.Items.Objects[i];
    end;
  end;


end;

end.
