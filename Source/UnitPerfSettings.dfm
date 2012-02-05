object frmPerfSettings: TfrmPerfSettings
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Performance Settings'
  ClientHeight = 249
  ClientWidth = 414
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 6
    Width = 27
    Height = 13
    Caption = 'Name'
  end
  object GroupBox1: TGroupBox
    Left = 144
    Top = 8
    Width = 257
    Height = 49
    Caption = 'Master Clock'
    TabOrder = 0
    object Label2: TLabel
      Left = 5
      Top = 19
      Width = 54
      Height = 13
      Caption = 'Rate (BPM)'
    end
    object eRate: TEdit
      Left = 65
      Top = 16
      Width = 41
      Height = 21
      TabOrder = 0
      OnExit = PerfChange
    end
    object udRate: TUpDown
      Left = 109
      Top = 13
      Width = 17
      Height = 25
      Min = 30
      Max = 240
      Position = 30
      TabOrder = 1
      OnClick = udRateClick
    end
    object rbStop: TRadioButton
      Left = 144
      Top = 18
      Width = 41
      Height = 17
      Caption = 'Stop'
      TabOrder = 2
      OnClick = PerfChange
    end
    object rbRun: TRadioButton
      Left = 200
      Top = 18
      Width = 49
      Height = 17
      Caption = 'Run'
      TabOrder = 3
      OnClick = PerfChange
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 56
    Width = 393
    Height = 173
    Caption = 'Slots'
    TabOrder = 1
    object Label3: TLabel
      Left = 24
      Top = 41
      Width = 32
      Height = 13
      Caption = 'Enable'
    end
    object Label4: TLabel
      Left = 100
      Top = 41
      Width = 46
      Height = 13
      Caption = 'Keyboard'
    end
    object Label5: TLabel
      Left = 169
      Top = 41
      Width = 21
      Height = 13
      Caption = 'Hold'
    end
    object Label6: TLabel
      Left = 248
      Top = 41
      Width = 29
      Height = 13
      Caption = 'Lower'
    end
    object Label7: TLabel
      Left = 306
      Top = 41
      Width = 29
      Height = 13
      Caption = 'Upper'
    end
    object cbEnableA: TCheckBox
      Left = 24
      Top = 63
      Width = 49
      Height = 17
      Caption = 'Slot A'
      TabOrder = 0
      OnClick = PerfChange
    end
    object cbEnableB: TCheckBox
      Left = 24
      Top = 90
      Width = 49
      Height = 17
      Caption = 'Slot B'
      TabOrder = 1
      OnClick = PerfChange
    end
    object cbEnableC: TCheckBox
      Left = 24
      Top = 114
      Width = 49
      Height = 17
      Caption = 'Slot C'
      TabOrder = 2
      OnClick = PerfChange
    end
    object cbEnableD: TCheckBox
      Left = 24
      Top = 139
      Width = 57
      Height = 17
      Caption = 'Slot D'
      TabOrder = 3
      OnClick = PerfChange
    end
    object cbKeyboardA: TCheckBox
      Left = 112
      Top = 63
      Width = 30
      Height = 17
      TabOrder = 4
      OnClick = PerfChange
    end
    object cbKeyboardB: TCheckBox
      Left = 112
      Top = 90
      Width = 17
      Height = 17
      TabOrder = 5
      OnClick = PerfChange
    end
    object cbKeyboardC: TCheckBox
      Left = 112
      Top = 114
      Width = 17
      Height = 17
      TabOrder = 6
      OnClick = PerfChange
    end
    object cbKeyboardD: TCheckBox
      Left = 112
      Top = 139
      Width = 19
      Height = 17
      TabOrder = 7
      OnClick = PerfChange
    end
    object cbHoldA: TCheckBox
      Left = 172
      Top = 63
      Width = 25
      Height = 17
      TabOrder = 8
      OnClick = PerfChange
    end
    object cbHoldB: TCheckBox
      Left = 172
      Top = 90
      Width = 25
      Height = 17
      TabOrder = 9
      OnClick = PerfChange
    end
    object cbHoldC: TCheckBox
      Left = 172
      Top = 114
      Width = 25
      Height = 17
      TabOrder = 10
      OnClick = PerfChange
    end
    object cbHoldD: TCheckBox
      Left = 172
      Top = 139
      Width = 30
      Height = 17
      TabOrder = 11
      OnClick = PerfChange
    end
    object cbKeyboardRange: TCheckBox
      Left = 248
      Top = 15
      Width = 97
      Height = 17
      Caption = 'Keyboard range'
      TabOrder = 12
      OnClick = PerfChange
    end
    object eLowerA: TEdit
      Left = 248
      Top = 61
      Width = 38
      Height = 21
      TabOrder = 13
      Text = 'eLowerA'
      OnExit = PerfChange
    end
    object eLowerB: TEdit
      Left = 248
      Top = 88
      Width = 38
      Height = 21
      TabOrder = 14
      Text = 'eLowerB'
      OnExit = PerfChange
    end
    object eLowerC: TEdit
      Left = 248
      Top = 112
      Width = 38
      Height = 21
      TabOrder = 15
      Text = 'eLowerC'
      OnExit = PerfChange
    end
    object eLowerD: TEdit
      Left = 248
      Top = 137
      Width = 38
      Height = 21
      TabOrder = 16
      Text = 'eLowerD'
      OnExit = PerfChange
    end
    object eUpperA: TEdit
      Left = 306
      Top = 61
      Width = 38
      Height = 21
      TabOrder = 17
      Text = 'eUpperA'
      OnExit = PerfChange
    end
    object eUpperB: TEdit
      Left = 306
      Top = 88
      Width = 38
      Height = 21
      TabOrder = 18
      Text = 'eUpperB'
      OnExit = PerfChange
    end
    object eUpperC: TEdit
      Left = 306
      Top = 112
      Width = 38
      Height = 21
      TabOrder = 19
      Text = 'eUpperC'
      OnExit = PerfChange
    end
    object eUpperD: TEdit
      Left = 306
      Top = 137
      Width = 38
      Height = 21
      TabOrder = 20
      Text = 'eUpperD'
      OnExit = PerfChange
    end
  end
  object ePerfName: TEdit
    Left = 16
    Top = 24
    Width = 121
    Height = 21
    TabOrder = 2
    OnExit = ePerfNameExit
  end
end
