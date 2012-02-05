unit UnitPerfSettings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, g2_types;

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
begin
  if FDisableControls then
    exit;

  frmG2Main.G2.Performance.PerformanceName := ePerfName.Text;
  frmG2Main.G2.Performance.SendSetPerfNameMessage( frmG2Main.G2.Performance.PerformanceName);
end;

procedure TfrmPerfSettings.FormShow(Sender: TObject);
begin
  updateDialog;
end;

procedure TfrmPerfSettings.PerfChange(Sender: TObject);
begin
  if FDisableControls then
    exit;

  frmG2Main.G2.Performance.MasterClock := StrToInt(eRate.Text);

  if rbRun.Checked then
    frmG2Main.G2.Performance.MasterClockRun := 1
  else
    frmG2Main.G2.Performance.MasterClockRun := 0;

  frmG2Main.G2.Performance.KeyboardRangeEnabled := BoolToByte(cbKeyBoardRange.Checked);

  frmG2Main.G2.Performance.SlotA.Enabled := BoolToByte(cbEnableA.Checked);
  frmG2Main.G2.Performance.SlotA.Hold := BoolToByte(cbHoldA.Checked);
  frmG2Main.G2.Performance.SlotA.Keyboard := BoolToByte(cbKeyboardA.Checked);
  frmG2Main.G2.Performance.SlotA.Upper := StrToInt(eUpperA.Text);
  frmG2Main.G2.Performance.SlotA.Lower := StrToInt(eLowerA.Text);

  frmG2Main.G2.Performance.SlotB.Enabled := BoolToByte(cbEnableB.Checked);
  frmG2Main.G2.Performance.SlotB.Hold := BoolToByte(cbHoldB.Checked);
  frmG2Main.G2.Performance.SlotB.Keyboard := BoolToByte(cbKeyboardB.Checked);
  frmG2Main.G2.Performance.SlotB.Upper := StrToInt(eUpperB.Text);
  frmG2Main.G2.Performance.SlotB.Lower := StrToInt(eLowerB.Text);

  frmG2Main.G2.Performance.SlotC.Enabled := BoolToByte(cbEnableC.Checked);
  frmG2Main.G2.Performance.SlotC.Hold := BoolToByte(cbHoldC.Checked);
  frmG2Main.G2.Performance.SlotC.Keyboard := BoolToByte(cbKeyboardC.Checked);
  frmG2Main.G2.Performance.SlotC.Upper := StrToInt(eUpperC.Text);
  frmG2Main.G2.Performance.SlotC.Lower := StrToInt(eLowerC.Text);

  frmG2Main.G2.Performance.SlotD.Enabled := BoolToByte(cbEnableD.Checked);
  frmG2Main.G2.Performance.SlotD.Hold := BoolToByte(cbHoldD.Checked);
  frmG2Main.G2.Performance.SlotD.Keyboard := BoolToByte(cbKeyboardD.Checked);
  frmG2Main.G2.Performance.SlotD.Upper := StrToInt(eUpperD.Text);
  frmG2Main.G2.Performance.SlotD.Lower := StrToInt(eLowerD.Text);

  frmG2Main.G2.Performance.SendSetPerfSettingsMessage;
end;

procedure TfrmPerfSettings.udRateClick(Sender: TObject; Button: TUDBtnType);
begin
  eRate.Text := IntToStr(udRate.Position);
  PerfChange(self);
end;

procedure TfrmPerfSettings.updateDialog;
begin
  FDisableControls := True;
  try
    ePerfName.Text := frmG2Main.G2.Performance.PerformanceName;

    eRate.Text := IntToStr(frmG2Main.G2.Performance.MasterClock);
    udRate.Position := frmG2Main.G2.Performance.MasterClock;
    if frmG2Main.G2.Performance.MasterClockRun = 1 then
      rbRun.Checked := True
    else
      rbStop.Checked := True;
    cbKeyboardRange.Checked := frmG2Main.G2.Performance.KeyboardRangeEnabled = 1;


    cbEnableA.Checked   := frmG2Main.G2.Performance.Slot[0].Enabled = 1;
    cbHoldA.Checked     := frmG2Main.G2.Performance.Slot[0].Hold = 1;
    cbKeyboardA.Checked := frmG2Main.G2.Performance.Slot[0].Keyboard = 1;
    eLowerA.Text        := IntToStr(frmG2Main.G2.Performance.Slot[0].Lower);
    eUpperA.Text        := IntToStr(frmG2Main.G2.Performance.Slot[0].Upper);
    eLowerA.Enabled     := cbKeyboardRange.Checked;
    eUpperA.Enabled     := cbKeyboardRange.Checked;

    cbEnableB.Checked   := frmG2Main.G2.Performance.Slot[1].Enabled = 1;
    cbHoldB.Checked     := frmG2Main.G2.Performance.Slot[1].Hold = 1;
    cbKeyboardB.Checked := frmG2Main.G2.Performance.Slot[1].Keyboard = 1;
    eLowerB.Text        := IntToStr(frmG2Main.G2.Performance.Slot[1].Lower);
    eUpperB.Text        := IntToStr(frmG2Main.G2.Performance.Slot[1].Upper);
    eLowerB.Enabled     := cbKeyboardRange.Checked;
    eUpperB.Enabled     := cbKeyboardRange.Checked;

    cbEnableC.Checked   := frmG2Main.G2.Performance.Slot[2].Enabled = 1;
    cbHoldC.Checked     := frmG2Main.G2.Performance.Slot[2].Hold = 1;
    cbKeyboardC.Checked := frmG2Main.G2.Performance.Slot[2].Keyboard = 1;
    eLowerC.Text        := IntToStr(frmG2Main.G2.Performance.Slot[2].Lower);
    eUpperC.Text        := IntToStr(frmG2Main.G2.Performance.Slot[2].Upper);
    eLowerC.Enabled     := cbKeyboardRange.Checked;
    eUpperC.Enabled     := cbKeyboardRange.Checked;

    cbEnableD.Checked   := frmG2Main.G2.Performance.Slot[3].Enabled = 1;
    cbHoldD.Checked     := frmG2Main.G2.Performance.Slot[3].Hold = 1;
    cbKeyboardD.Checked := frmG2Main.G2.Performance.Slot[3].Keyboard = 1;
    eLowerD.Text        := IntToStr(frmG2Main.G2.Performance.Slot[3].Lower);
    eUpperD.Text        := IntToStr(frmG2Main.G2.Performance.Slot[3].Upper);
    eLowerD.Enabled     := cbKeyboardRange.Checked;
    eUpperD.Enabled     := cbKeyboardRange.Checked;

  finally
    FDisableControls := False;
  end;
end;

end.
