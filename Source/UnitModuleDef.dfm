object frmModuleDef: TfrmModuleDef
  Left = 0
  Top = 0
  Caption = 'Module definition'
  ClientHeight = 348
  ClientWidth = 628
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 628
    Height = 41
    Align = alTop
    TabOrder = 0
    ExplicitLeft = 120
    ExplicitTop = 72
    ExplicitWidth = 185
    object Label1: TLabel
      Left = 23
      Top = 14
      Width = 34
      Height = 13
      Caption = 'Module'
    end
    object eModuleName: TEdit
      Left = 77
      Top = 11
      Width = 156
      Height = 21
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 0
    end
  end
  object sgParams: TStringGrid
    Left = 0
    Top = 41
    Width = 628
    Height = 274
    Align = alClient
    DefaultRowHeight = 16
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing]
    TabOrder = 1
    ExplicitLeft = 88
    ExplicitTop = 64
    ExplicitWidth = 320
    ExplicitHeight = 120
  end
  object Panel2: TPanel
    Left = 0
    Top = 315
    Width = 628
    Height = 33
    Align = alBottom
    TabOrder = 2
    ExplicitTop = 272
    ExplicitWidth = 549
    DesignSize = (
      628
      33)
    object Button1: TButton
      Left = 537
      Top = 4
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Update'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
end
