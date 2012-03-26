unit UnitPerfSettings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, g2_types, g2_classes;

type
  TfrmPerfSettings = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    ePerfName: TEdit;
    Label1: TLabel;
    eRate: TEdit;
    udRate: TUpDown;
    rbStop: TRadioButton;
    rbRun: TRadioButton;
    Label2: TLabel;
    Label3: TLabel;
    cbEnableA: TCheckBox;
    cbEnableB: TCheckBox;
    cbEnableC: TCheckBox;
    cbEnableD: TCheckBox;
    Label4: TLabel;
    Label5: TLabel;
    cbKeyboardA: TCheckBox;
    cbKeyboardB: TCheckBox;
    cbKeyboardC: TCheckBox;
    cbKeyboardD: TCheckBox;
    cbHoldA: TCheckBox;
    cbHoldB: TCheckBox;
    cbHoldC: TCheckBox;
    cbHoldD: TCheckBox;
    cbKeyboardRange: TCheckBox;
    eLowerA: TEdit;
    eLowerB: TEdit;
    eLowerC: TEdit;
    eLowerD: TEdit;
    eUpperA: TEdit;
    eUpperB: TEdit;
    eUpperC: TEdit;
    eUpperD: TEdit;
    Label6: TLabel;
    Label7: TLabel;
    procedure FormShow(Sender: TObject);
    procedure ePerfNameExit(Sender: TObject);
    procedure PerfChange(Sender: TObject);
    procedure udRateClick(Sender: TObject; Button: TUDBtnType);
  private
    { Private declarations }
    FDisableControls : boolean;
  public
    { Public declarations }
    procedure updateDialog;
  end;

var
  frmPerfSettings: TfrmPerfSettings;

implementation

uses UnitG2Editor;

{$R *.dfm}

{ TfrmPerfSettings }

procedure TfrmPerfSettings.ePerfNameExit(Sender: TObject);
var G2 : TG2;
begin
  if FDisableControls then
    exit;

  G2 := frmG2Main.SelectedG2;
  if not assigned(G2) then
    exit;

  G2.Performance.PerformanceName := AnsiString(ePerfName.Text);
  G2.Performance.SendSetPerfNameMessage( G2.Performance.PerformanceName);
end;

procedure TfrmPerfSettings.FormShow(Sender: TObject);
begin
  updateDialog;
end;

procedure TfrmPerfSettings.PerfChange(Sender: TObject);
var G2 : TG2;
begin
  if FDisableControls then
    exit;

  G2 := frmG2Main.SelectedG2;
  if not assigned(G2) then
    exit;

  G2.Performance.MasterClock := StrToInt(eRate.Text);

  if rbRun.Checked then
    G2.Performance.MasterClockRun := 1
  else
    G2.Performance.MasterClockRun := 0;

  G2.Performance.KeyboardRangeEnabled := BoolToByte(cbKeyBoardRange.Checked);

  G2.Performance.SlotA.Enabled := BoolToByte(cbEnableA.Checked);
  G2.Performance.SlotA.Hold := BoolToByte(cbHoldA.Checked);
  G2.Performance.SlotA.Keyboard := BoolToByte(cbKeyboardA.Checked);
  G2.Performance.SlotA.Upper := StrToInt(eUpperA.Text);
  G2.Performance.SlotA.Lower := StrToInt(eLowerA.Text);

  G2.Performance.SlotB.Enabled := BoolToByte(cbEnableB.Checked);
  G2.Performance.SlotB.Hold := BoolToByte(cbHoldB.Checked);
  G2.Performance.SlotB.Keyboard := BoolToByte(cbKeyboardB.Checked);
  G2.Performance.SlotB.Upper := StrToInt(eUpperB.Text);
  G2.Performance.SlotB.Lower := StrToInt(eLowerB.Text);

  G2.Performance.SlotC.Enabled := BoolToByte(cbEnableC.Checked);
  G2.Performance.SlotC.Hold := BoolToByte(cbHoldC.Checked);
  G2.Performance.SlotC.Keyboard := BoolToByte(cbKeyboardC.Checked);
  G2.Performance.SlotC.Upper := StrToInt(eUpperC.Text);
  G2.Performance.SlotC.Lower := StrToInt(eLowerC.Text);

  G2.Performance.SlotD.Enabled := BoolToByte(cbEnableD.Checked);
  G2.Performance.SlotD.Hold := BoolToByte(cbHoldD.Checked);
  G2.Performance.SlotD.Keyboard := BoolToByte(cbKeyboardD.Checked);
  G2.Performance.SlotD.Upper := StrToInt(eUpperD.Text);
  G2.Performance.SlotD.Lower := StrToInt(eLowerD.Text);

  G2.Performance.SendSetPerfSettingsMessage;
end;

procedure TfrmPerfSettings.udRateClick(Sender: TObject; Button: TUDBtnType);
begin
  eRate.Text := IntToStr(udRate.Position);
  PerfChange(self);
end;

procedure TfrmPerfSettings.updateDialog;
var G2 : TG2;
begin
  G2 := frmG2Main.SelectedG2;
  if not assigned(G2) then
    exit;

  FDisableControls := True;
  try
    ePerfName.Text := string(G2.Performance.PerformanceName);

    eRate.Text := IntToStr(G2.Performance.MasterClock);
    udRate.Position := G2.Performance.MasterClock;
    if G2.Performance.MasterClockRun = 1 then
      rbRun.Checked := True
    else
      rbStop.Checked := True;
    cbKeyboardRange.Checked := G2.Performance.KeyboardRangeEnabled = 1;


    cbEnableA.Checked   := G2.Performance.Slot[0].Enabled = 1;
    cbHoldA.Checked     := G2.Performance.Slot[0].Hold = 1;
    cbKeyboardA.Checked := G2.Performance.Slot[0].Keyboard = 1;
    eLowerA.Text        := IntToStr(G2.Performance.Slot[0].Lower);
    eUpperA.Text        := IntToStr(G2.Performance.Slot[0].Upper);
    eLowerA.Enabled     := cbKeyboardRange.Checked;
    eUpperA.Enabled     := cbKeyboardRange.Checked;

    cbEnableB.Checked   := G2.Performance.Slot[1].Enabled = 1;
    cbHoldB.Checked     := G2.Performance.Slot[1].Hold = 1;
    cbKeyboardB.Checked := G2.Performance.Slot[1].Keyboard = 1;
    eLowerB.Text        := IntToStr(G2.Performance.Slot[1].Lower);
    eUpperB.Text        := IntToStr(G2.Performance.Slot[1].Upper);
    eLowerB.Enabled     := cbKeyboardRange.Checked;
    eUpperB.Enabled     := cbKeyboardRange.Checked;

    cbEnableC.Checked   := G2.Performance.Slot[2].Enabled = 1;
    cbHoldC.Checked     := G2.Performance.Slot[2].Hold = 1;
    cbKeyboardC.Checked := G2.Performance.Slot[2].Keyboard = 1;
    eLowerC.Text        := IntToStr(G2.Performance.Slot[2].Lower);
    eUpperC.Text        := IntToStr(G2.Performance.Slot[2].Upper);
    eLowerC.Enabled     := cbKeyboardRange.Checked;
    eUpperC.Enabled     := cbKeyboardRange.Checked;

    cbEnableD.Checked   := G2.Performance.Slot[3].Enabled = 1;
    cbHoldD.Checked     := G2.Performance.Slot[3].Hold = 1;
    cbKeyboardD.Checked := G2.Performance.Slot[3].Keyboard = 1;
    eLowerD.Text        := IntToStr(G2.Performance.Slot[3].Lower);
    eUpperD.Text        := IntToStr(G2.Performance.Slot[3].Upper);
    eLowerD.Enabled     := cbKeyboardRange.Checked;
    eUpperD.Enabled     := cbKeyboardRange.Checked;

  finally
    FDisableControls := False;
  end;
end;

end.
