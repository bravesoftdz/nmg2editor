object frmEditorTools: TfrmEditorTools
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Editor tools'
  ClientHeight = 227
  ClientWidth = 135
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnKeyUp = FormKeyUp
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object btCablesRed: TG2GraphButtonText
    Left = 7
    Top = 197
    Width = 13
    Height = 14
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    Color = 5921535
    Value = 0
    LowValue = 0
    HighValue = 255
    ParentColor = False
    ParentFont = False
    OnClick = btCablesClick
    HightlightColor = 5921535
    BorderColor = clBlack
    Bevel = False
    Orientation = otHorizontal
    ButtonCount = 0
    ButtonTextType = bttCheckBox
  end
  object btCablesBlue: TG2GraphButtonText
    Left = 21
    Top = 197
    Width = 13
    Height = 14
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    Color = 16737380
    Value = 0
    LowValue = 0
    HighValue = 255
    ParentColor = False
    ParentFont = False
    OnClick = btCablesClick
    HightlightColor = clAqua
    BorderColor = clBlack
    Bevel = False
    Orientation = otHorizontal
    ButtonCount = 0
    ButtonTextType = bttCheckBox
  end
  object btCablesYellow: TG2GraphButtonText
    Left = 35
    Top = 197
    Width = 13
    Height = 14
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    Color = 5301990
    Value = 0
    LowValue = 0
    HighValue = 255
    ParentColor = False
    ParentFont = False
    OnClick = btCablesClick
    HightlightColor = clAqua
    BorderColor = clBlack
    Bevel = False
    Orientation = otHorizontal
    ButtonCount = 0
    ButtonTextType = bttCheckBox
  end
  object btCablesOrange: TG2GraphButtonText
    Left = 49
    Top = 197
    Width = 13
    Height = 14
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    Color = 5292287
    Value = 0
    LowValue = 0
    HighValue = 255
    ParentColor = False
    ParentFont = False
    OnClick = btCablesClick
    HightlightColor = clAqua
    BorderColor = clBlack
    Bevel = False
    Orientation = otHorizontal
    ButtonCount = 0
    ButtonTextType = bttCheckBox
  end
  object btCablesGreen: TG2GraphButtonText
    Left = 63
    Top = 197
    Width = 13
    Height = 14
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    Color = 5296720
    Value = 0
    LowValue = 0
    HighValue = 255
    ParentColor = False
    ParentFont = False
    OnClick = btCablesClick
    HightlightColor = clAqua
    BorderColor = clBlack
    Bevel = False
    Orientation = otHorizontal
    ButtonCount = 0
    ButtonTextType = bttCheckBox
  end
  object btCablesPurple: TG2GraphButtonText
    Left = 77
    Top = 197
    Width = 13
    Height = 14
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    Color = 15073480
    Value = 0
    LowValue = 0
    HighValue = 255
    ParentColor = False
    ParentFont = False
    OnClick = btCablesClick
    HightlightColor = clAqua
    BorderColor = clBlack
    Bevel = False
    Orientation = otHorizontal
    ButtonCount = 0
    ButtonTextType = bttCheckBox
  end
  object btCablesWhite: TG2GraphButtonText
    Left = 92
    Top = 197
    Width = 13
    Height = 14
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    Color = clWhite
    Value = 0
    LowValue = 0
    HighValue = 255
    ParentColor = False
    ParentFont = False
    OnClick = btCablesClick
    HightlightColor = clAqua
    BorderColor = clBlack
    Bevel = False
    Orientation = otHorizontal
    ButtonCount = 0
    ButtonTextType = bttCheckBox
  end
  object Label1: TLabel
    Left = 8
    Top = 176
    Width = 59
    Height = 13
    Caption = 'Show cables'
  end
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 120
    Height = 18
    ParentBackground = False
    TabOrder = 0
    OnClick = PanelClick
  end
  object Panel2: TPanel
    Tag = 6
    Left = 8
    Top = 32
    Width = 25
    Height = 18
    ParentBackground = False
    TabOrder = 1
    OnClick = PanelClick
  end
  object Panel3: TPanel
    Tag = 13
    Left = 39
    Top = 32
    Width = 25
    Height = 18
    ParentBackground = False
    TabOrder = 2
    OnClick = PanelClick
  end
  object Panel4: TPanel
    Tag = 14
    Left = 72
    Top = 32
    Width = 25
    Height = 18
    ParentBackground = False
    TabOrder = 3
    OnClick = PanelClick
  end
  object Panel5: TPanel
    Tag = 1
    Left = 103
    Top = 32
    Width = 25
    Height = 18
    ParentBackground = False
    TabOrder = 4
    OnClick = PanelClick
  end
  object Panel6: TPanel
    Tag = 9
    Left = 8
    Top = 56
    Width = 25
    Height = 18
    ParentBackground = False
    TabOrder = 5
    OnClick = PanelClick
  end
  object Panel7: TPanel
    Tag = 11
    Left = 39
    Top = 56
    Width = 25
    Height = 18
    ParentBackground = False
    TabOrder = 6
    OnClick = PanelClick
  end
  object Panel8: TPanel
    Tag = 15
    Left = 72
    Top = 56
    Width = 25
    Height = 18
    ParentBackground = False
    TabOrder = 7
    OnClick = PanelClick
  end
  object Panel9: TPanel
    Tag = 4
    Left = 103
    Top = 56
    Width = 25
    Height = 18
    ParentBackground = False
    TabOrder = 8
    OnClick = PanelClick
  end
  object Panel10: TPanel
    Tag = 10
    Left = 8
    Top = 80
    Width = 25
    Height = 18
    ParentBackground = False
    TabOrder = 9
    OnClick = PanelClick
  end
  object Panel11: TPanel
    Tag = 8
    Left = 39
    Top = 80
    Width = 25
    Height = 18
    ParentBackground = False
    TabOrder = 10
    OnClick = PanelClick
  end
  object Panel12: TPanel
    Tag = 16
    Left = 72
    Top = 80
    Width = 25
    Height = 18
    ParentBackground = False
    TabOrder = 11
    OnClick = PanelClick
  end
  object Panel13: TPanel
    Tag = 2
    Left = 103
    Top = 80
    Width = 25
    Height = 18
    ParentBackground = False
    TabOrder = 12
    OnClick = PanelClick
  end
  object Panel14: TPanel
    Tag = 17
    Left = 8
    Top = 104
    Width = 25
    Height = 18
    ParentBackground = False
    TabOrder = 13
    OnClick = PanelClick
  end
  object Panel15: TPanel
    Tag = 7
    Left = 39
    Top = 104
    Width = 25
    Height = 18
    ParentBackground = False
    TabOrder = 14
    OnClick = PanelClick
  end
  object Panel16: TPanel
    Tag = 18
    Left = 72
    Top = 104
    Width = 25
    Height = 18
    ParentBackground = False
    TabOrder = 15
    OnClick = PanelClick
  end
  object Panel17: TPanel
    Tag = 19
    Left = 103
    Top = 104
    Width = 25
    Height = 18
    ParentBackground = False
    TabOrder = 16
    OnClick = PanelClick
  end
  object Panel18: TPanel
    Tag = 5
    Left = 8
    Top = 128
    Width = 25
    Height = 18
    ParentBackground = False
    TabOrder = 17
    OnClick = PanelClick
  end
  object Panel19: TPanel
    Tag = 20
    Left = 39
    Top = 128
    Width = 25
    Height = 18
    ParentBackground = False
    TabOrder = 18
    OnClick = PanelClick
  end
  object Panel20: TPanel
    Tag = 12
    Left = 72
    Top = 128
    Width = 25
    Height = 18
    ParentBackground = False
    TabOrder = 19
    OnClick = PanelClick
  end
  object Panel21: TPanel
    Tag = 3
    Left = 103
    Top = 128
    Width = 25
    Height = 18
    ParentBackground = False
    TabOrder = 20
    OnClick = PanelClick
  end
  object Panel22: TPanel
    Tag = 21
    Left = 8
    Top = 152
    Width = 25
    Height = 18
    ParentBackground = False
    TabOrder = 21
    OnClick = PanelClick
  end
  object Panel23: TPanel
    Tag = 22
    Left = 39
    Top = 152
    Width = 25
    Height = 18
    ParentBackground = False
    TabOrder = 22
    OnClick = PanelClick
  end
  object Panel24: TPanel
    Tag = 23
    Left = 72
    Top = 152
    Width = 25
    Height = 18
    ParentBackground = False
    TabOrder = 23
    OnClick = PanelClick
  end
  object Panel25: TPanel
    Tag = 24
    Left = 103
    Top = 152
    Width = 25
    Height = 18
    ParentBackground = False
    TabOrder = 24
    OnClick = PanelClick
  end
end
