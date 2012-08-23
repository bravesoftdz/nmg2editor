object frmMidiMapping: TfrmMidiMapping
  Left = 0
  Top = 0
  Caption = 'NMG2 Editor UI midi mapping'
  ClientHeight = 290
  ClientWidth = 554
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object DListView1: DListView
    Left = 0
    Top = 0
    Width = 554
    Height = 290
    Align = alClient
    Columns = <>
    TabOrder = 0
  end
  object puCtrlAssignMidi: TPopupMenu
    Left = 208
    Top = 32
    object miCtrlMidiAssignment: TMenuItem
      Caption = 'Ctrl midi assign'
    end
  end
end
