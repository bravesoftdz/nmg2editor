object frmParameterPages: TfrmParameterPages
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Parameter pages'
  ClientHeight = 156
  ClientWidth = 569
  Color = clBtnFace
  Constraints.MaxHeight = 180
  Constraints.MaxWidth = 575
  Constraints.MinHeight = 180
  Constraints.MinWidth = 575
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnKeyUp = FormKeyUp
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object skP1: TG2GraphKnob
    Left = 20
    Top = 59
    Width = 22
    Height = 22
    MidiAware = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    Color = clWhite
    Value = 0
    LowValue = 0
    HighValue = 127
    OnMouseDown = skPMouseDown
    ParentColor = False
    ParentFont = False
    KnobType = ktMedium
    Orientation = otHorizontal
    HightlightColor = clAqua
  end
  object skP2: TG2GraphKnob
    Tag = 1
    Left = 83
    Top = 59
    Width = 22
    Height = 22
    MidiAware = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    Color = clWhite
    Value = 0
    LowValue = 0
    HighValue = 127
    OnMouseDown = skPMouseDown
    ParentColor = False
    ParentFont = False
    KnobType = ktMedium
    Orientation = otHorizontal
    HightlightColor = clAqua
  end
  object skP3: TG2GraphKnob
    Tag = 2
    Left = 147
    Top = 59
    Width = 22
    Height = 22
    MidiAware = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    Color = clWhite
    Value = 0
    LowValue = 0
    HighValue = 127
    OnMouseDown = skPMouseDown
    ParentColor = False
    ParentFont = False
    KnobType = ktMedium
    Orientation = otHorizontal
    HightlightColor = clAqua
  end
  object skP4: TG2GraphKnob
    Tag = 3
    Left = 211
    Top = 59
    Width = 22
    Height = 22
    MidiAware = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    Color = clWhite
    Value = 0
    LowValue = 0
    HighValue = 127
    OnMouseDown = skPMouseDown
    ParentColor = False
    ParentFont = False
    KnobType = ktMedium
    Orientation = otHorizontal
    HightlightColor = clAqua
  end
  object skP5: TG2GraphKnob
    Tag = 4
    Left = 273
    Top = 59
    Width = 22
    Height = 22
    MidiAware = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    Color = clWhite
    Value = 0
    LowValue = 0
    HighValue = 127
    OnMouseDown = skPMouseDown
    ParentColor = False
    ParentFont = False
    KnobType = ktMedium
    Orientation = otHorizontal
    HightlightColor = clAqua
  end
  object skP6: TG2GraphKnob
    Tag = 5
    Left = 336
    Top = 59
    Width = 22
    Height = 22
    MidiAware = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    Color = clWhite
    Value = 0
    LowValue = 0
    HighValue = 127
    OnMouseDown = skPMouseDown
    ParentColor = False
    ParentFont = False
    KnobType = ktMedium
    Orientation = otHorizontal
    HightlightColor = clAqua
  end
  object skP7: TG2GraphKnob
    Tag = 6
    Left = 398
    Top = 59
    Width = 22
    Height = 22
    MidiAware = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    Color = clWhite
    Value = 0
    LowValue = 0
    HighValue = 127
    OnMouseDown = skPMouseDown
    ParentColor = False
    ParentFont = False
    KnobType = ktMedium
    Orientation = otHorizontal
    HightlightColor = clAqua
  end
  object skP8: TG2GraphKnob
    Tag = 7
    Left = 462
    Top = 59
    Width = 22
    Height = 22
    MidiAware = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    Color = clWhite
    Value = 0
    LowValue = 0
    HighValue = 127
    OnMouseDown = skPMouseDown
    ParentColor = False
    ParentFont = False
    KnobType = ktMedium
    Orientation = otHorizontal
    HightlightColor = clAqua
  end
  object Disp1A: TG2GraphDisplay
    Left = 1
    Top = 26
    Width = 60
    Height = 27
    MidiAware = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    Color = 5592405
    Value = 0
    LowValue = 0
    HighValue = 0
    ParentColor = False
    ParentFont = False
    LineCount = 2
    TextFunction = 1001
    DisplayType = 0
  end
  object Disp2A: TG2GraphDisplay
    Left = 65
    Top = 26
    Width = 60
    Height = 27
    MidiAware = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    Color = 5592405
    Value = 0
    LowValue = 0
    HighValue = 0
    ParentColor = False
    ParentFont = False
    LineCount = 2
    TextFunction = 1001
    DisplayType = 0
  end
  object Disp3A: TG2GraphDisplay
    Left = 129
    Top = 26
    Width = 60
    Height = 27
    MidiAware = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    Color = 5592405
    Value = 0
    LowValue = 0
    HighValue = 0
    ParentColor = False
    ParentFont = False
    LineCount = 2
    TextFunction = 1001
    DisplayType = 0
  end
  object Disp4A: TG2GraphDisplay
    Left = 192
    Top = 26
    Width = 60
    Height = 27
    MidiAware = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    Color = 5592405
    Value = 0
    LowValue = 0
    HighValue = 0
    ParentColor = False
    ParentFont = False
    LineCount = 2
    TextFunction = 1001
    DisplayType = 0
  end
  object Disp5A: TG2GraphDisplay
    Left = 255
    Top = 26
    Width = 60
    Height = 27
    MidiAware = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    Color = 5592405
    Value = 0
    LowValue = 0
    HighValue = 0
    ParentColor = False
    ParentFont = False
    LineCount = 2
    TextFunction = 1001
    DisplayType = 0
  end
  object Disp6A: TG2GraphDisplay
    Left = 318
    Top = 26
    Width = 60
    Height = 27
    MidiAware = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    Color = 5592405
    Value = 0
    LowValue = 0
    HighValue = 0
    ParentColor = False
    ParentFont = False
    LineCount = 2
    TextFunction = 1001
    DisplayType = 0
  end
  object Disp7A: TG2GraphDisplay
    Left = 381
    Top = 26
    Width = 60
    Height = 27
    MidiAware = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    Color = 5592405
    Value = 0
    LowValue = 0
    HighValue = 0
    ParentColor = False
    ParentFont = False
    LineCount = 2
    TextFunction = 1001
    DisplayType = 0
  end
  object Disp8A: TG2GraphDisplay
    Left = 444
    Top = 26
    Width = 60
    Height = 27
    MidiAware = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    Color = 5592405
    Value = 0
    LowValue = 0
    HighValue = 0
    ParentColor = False
    ParentFont = False
    LineCount = 2
    TextFunction = 1001
    DisplayType = 0
  end
  object Disp1B: TG2GraphDisplay
    Left = 1
    Top = 3
    Width = 60
    Height = 17
    MidiAware = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    Color = clBtnFace
    Value = 0
    LowValue = 0
    HighValue = 0
    ParentColor = False
    ParentFont = False
    LineCount = 1
    TextFunction = 1000
    DisplayType = 0
  end
  object disp2B: TG2GraphDisplay
    Left = 65
    Top = 3
    Width = 60
    Height = 17
    MidiAware = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    Color = clBtnFace
    Value = 0
    LowValue = 0
    HighValue = 0
    ParentColor = False
    ParentFont = False
    LineCount = 1
    TextFunction = 1000
    DisplayType = 0
  end
  object Disp3B: TG2GraphDisplay
    Left = 129
    Top = 3
    Width = 60
    Height = 17
    MidiAware = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    Color = clBtnFace
    Value = 0
    LowValue = 0
    HighValue = 0
    ParentColor = False
    ParentFont = False
    LineCount = 1
    TextFunction = 1000
    DisplayType = 0
  end
  object Disp4B: TG2GraphDisplay
    Left = 192
    Top = 3
    Width = 60
    Height = 17
    MidiAware = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    Color = clBtnFace
    Value = 0
    LowValue = 0
    HighValue = 0
    ParentColor = False
    ParentFont = False
    LineCount = 1
    TextFunction = 1000
    DisplayType = 0
  end
  object Disp5B: TG2GraphDisplay
    Left = 255
    Top = 3
    Width = 60
    Height = 17
    MidiAware = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    Color = clBtnFace
    Value = 0
    LowValue = 0
    HighValue = 0
    ParentColor = False
    ParentFont = False
    LineCount = 1
    TextFunction = 1000
    DisplayType = 0
  end
  object Disp6B: TG2GraphDisplay
    Left = 318
    Top = 3
    Width = 60
    Height = 17
    MidiAware = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    Color = clBtnFace
    Value = 0
    LowValue = 0
    HighValue = 0
    ParentColor = False
    ParentFont = False
    LineCount = 1
    TextFunction = 1000
    DisplayType = 0
  end
  object Disp7B: TG2GraphDisplay
    Left = 381
    Top = 3
    Width = 60
    Height = 17
    MidiAware = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    Color = clBtnFace
    Value = 0
    LowValue = 0
    HighValue = 0
    ParentColor = False
    ParentFont = False
    LineCount = 1
    TextFunction = 1000
    DisplayType = 0
  end
  object Disp8B: TG2GraphDisplay
    Left = 444
    Top = 3
    Width = 60
    Height = 17
    MidiAware = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    Color = clBtnFace
    Value = 0
    LowValue = 0
    HighValue = 0
    ParentColor = False
    ParentFont = False
    LineCount = 1
    TextFunction = 1000
    DisplayType = 0
  end
  object bfP1: TG2GraphButtonFlat
    Left = 1
    Top = 101
    Width = 60
    Height = 16
    MidiAware = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    Color = 13684944
    Value = 0
    LowValue = 0
    HighValue = 255
    OnMouseDown = bfPMouseDown
    ParentColor = False
    ParentFont = False
    HightlightColor = clAqua
    BorderColor = clBlack
    Bevel = True
    Orientation = otHorizontal
    ButtonCount = 0
  end
  object bfP2: TG2GraphButtonFlat
    Tag = 1
    Left = 65
    Top = 101
    Width = 60
    Height = 16
    MidiAware = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    Color = 13684944
    Value = 0
    LowValue = 0
    HighValue = 255
    OnMouseDown = bfPMouseDown
    ParentColor = False
    ParentFont = False
    HightlightColor = clAqua
    BorderColor = clBlack
    Bevel = True
    Orientation = otHorizontal
    ButtonCount = 0
  end
  object bfP3: TG2GraphButtonFlat
    Tag = 2
    Left = 129
    Top = 101
    Width = 60
    Height = 16
    MidiAware = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    Color = 13684944
    Value = 0
    LowValue = 0
    HighValue = 255
    OnMouseDown = bfPMouseDown
    ParentColor = False
    ParentFont = False
    HightlightColor = clAqua
    BorderColor = clBlack
    Bevel = True
    Orientation = otHorizontal
    ButtonCount = 0
  end
  object bfP4: TG2GraphButtonFlat
    Tag = 3
    Left = 192
    Top = 101
    Width = 60
    Height = 16
    MidiAware = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    Color = 13684944
    Value = 0
    LowValue = 0
    HighValue = 255
    OnMouseDown = bfPMouseDown
    ParentColor = False
    ParentFont = False
    HightlightColor = clAqua
    BorderColor = clBlack
    Bevel = True
    Orientation = otHorizontal
    ButtonCount = 0
  end
  object bfP5: TG2GraphButtonFlat
    Tag = 4
    Left = 255
    Top = 101
    Width = 60
    Height = 16
    MidiAware = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    Color = 13684944
    Value = 0
    LowValue = 0
    HighValue = 255
    OnMouseDown = bfPMouseDown
    ParentColor = False
    ParentFont = False
    HightlightColor = clAqua
    BorderColor = clBlack
    Bevel = True
    Orientation = otHorizontal
    ButtonCount = 0
  end
  object bfP6: TG2GraphButtonFlat
    Tag = 5
    Left = 318
    Top = 101
    Width = 60
    Height = 16
    MidiAware = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    Color = 13684944
    Value = 0
    LowValue = 0
    HighValue = 255
    OnMouseDown = bfPMouseDown
    ParentColor = False
    ParentFont = False
    HightlightColor = clAqua
    BorderColor = clBlack
    Bevel = True
    Orientation = otHorizontal
    ButtonCount = 0
  end
  object bfP7: TG2GraphButtonFlat
    Tag = 6
    Left = 381
    Top = 101
    Width = 60
    Height = 16
    MidiAware = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    Color = 13684944
    Value = 0
    LowValue = 0
    HighValue = 255
    OnMouseDown = bfPMouseDown
    ParentColor = False
    ParentFont = False
    HightlightColor = clAqua
    BorderColor = clBlack
    Bevel = True
    Orientation = otHorizontal
    ButtonCount = 0
  end
  object bfP8: TG2GraphButtonFlat
    Tag = 7
    Left = 444
    Top = 101
    Width = 60
    Height = 16
    MidiAware = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    Color = 13684944
    Value = 0
    LowValue = 0
    HighValue = 255
    OnMouseDown = bfPMouseDown
    ParentColor = False
    ParentFont = False
    HightlightColor = clAqua
    BorderColor = clBlack
    Bevel = True
    Orientation = otHorizontal
    ButtonCount = 0
  end
  object Disp1C: TG2GraphDisplay
    Left = 1
    Top = 83
    Width = 60
    Height = 17
    MidiAware = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    Color = clBtnFace
    Value = 0
    LowValue = 0
    HighValue = 0
    ParentColor = False
    ParentFont = False
    LineCount = 1
    TextFunction = 1001
    DisplayType = 0
  end
  object Disp2C: TG2GraphDisplay
    Left = 65
    Top = 83
    Width = 60
    Height = 17
    MidiAware = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    Color = clBtnFace
    Value = 0
    LowValue = 0
    HighValue = 0
    ParentColor = False
    ParentFont = False
    LineCount = 1
    TextFunction = 1001
    DisplayType = 0
  end
  object Disp3C: TG2GraphDisplay
    Left = 129
    Top = 83
    Width = 60
    Height = 17
    MidiAware = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    Color = clBtnFace
    Value = 0
    LowValue = 0
    HighValue = 0
    ParentColor = False
    ParentFont = False
    LineCount = 1
    TextFunction = 1001
    DisplayType = 0
  end
  object Disp4C: TG2GraphDisplay
    Left = 192
    Top = 83
    Width = 60
    Height = 17
    MidiAware = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    Color = clBtnFace
    Value = 0
    LowValue = 0
    HighValue = 0
    ParentColor = False
    ParentFont = False
    LineCount = 1
    TextFunction = 1001
    DisplayType = 0
  end
  object Disp5C: TG2GraphDisplay
    Left = 255
    Top = 83
    Width = 60
    Height = 17
    MidiAware = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    Color = clBtnFace
    Value = 0
    LowValue = 0
    HighValue = 0
    ParentColor = False
    ParentFont = False
    LineCount = 1
    TextFunction = 1001
    DisplayType = 0
  end
  object Disp6C: TG2GraphDisplay
    Left = 318
    Top = 83
    Width = 60
    Height = 17
    MidiAware = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    Color = clBtnFace
    Value = 0
    LowValue = 0
    HighValue = 0
    ParentColor = False
    ParentFont = False
    LineCount = 1
    TextFunction = 1001
    DisplayType = 0
  end
  object Disp7C: TG2GraphDisplay
    Left = 381
    Top = 83
    Width = 60
    Height = 17
    MidiAware = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    Color = clBtnFace
    Value = 0
    LowValue = 0
    HighValue = 0
    ParentColor = False
    ParentFont = False
    LineCount = 1
    TextFunction = 1001
    DisplayType = 0
  end
  object Disp8C: TG2GraphDisplay
    Left = 444
    Top = 83
    Width = 60
    Height = 17
    MidiAware = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    Color = clBtnFace
    Value = 0
    LowValue = 0
    HighValue = 0
    ParentColor = False
    ParentFont = False
    LineCount = 1
    TextFunction = 1001
    DisplayType = 0
  end
  object pRight: TG2GraphPanel
    Left = 505
    Top = 0
    Width = 64
    Height = 125
    Align = alRight
    BevelOuter = bvNone
    Caption = 'pRight'
    Color = clSilver
    ParentBackground = False
    TabOrder = 0
    ExplicitLeft = 531
    object rbParamPage: TG2GraphButtonRadio
      Left = 7
      Top = 4
      Width = 50
      Height = 116
      MidiAware = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      Color = clActiveBorder
      Value = 0
      LowValue = 0
      HighValue = 4
      OnMouseDown = rbParamPageMouseDown
      OnChange = rbParamPageChange
      ParentColor = False
      ParentFont = False
      ButtonText.Strings = (
        'A - Osc'
        'B - LFO'
        'C - Env'
        'D - Filter'
        'E - Effect')
      HightlightColor = clAqua
      BorderColor = clBlack
      Bevel = False
      Orientation = otVertical
      ButtonCount = 5
    end
  end
  object pBottom: TG2GraphPanel
    Left = 0
    Top = 125
    Width = 569
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    Caption = 'pBottom'
    Color = clSilver
    ParentBackground = False
    TabOrder = 1
    ExplicitWidth = 594
    object rbSlot: TG2GraphButtonRadio
      Left = 4
      Top = 6
      Width = 109
      Height = 21
      MidiAware = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      Color = 13684944
      Value = 0
      LowValue = 0
      HighValue = 3
      OnMouseDown = rbSlotMouseDown
      OnChange = rbSlotChange
      ParentColor = False
      ParentFont = False
      OnClick = rbSlotClick
      ButtonText.Strings = (
        'A'
        'B'
        'C'
        'D')
      HightlightColor = clAqua
      BorderColor = clBlack
      Bevel = False
      Orientation = otHorizontal
      ButtonCount = 4
    end
    object rbVariation: TG2GraphButtonRadio
      Left = 119
      Top = 6
      Width = 217
      Height = 21
      MidiAware = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      Color = 13684944
      Value = 0
      LowValue = 0
      HighValue = 7
      OnMouseDown = rbVariationMouseDown
      ParentColor = False
      ParentFont = False
      OnClick = rbVariationClick
      ButtonText.Strings = (
        '1'
        '2'
        '3'
        '4'
        '5'
        '6'
        '7'
        '8')
      HightlightColor = clAqua
      BorderColor = clBlack
      Bevel = False
      Orientation = otHorizontal
      ButtonCount = 8
    end
    object rbMode: TG2GraphButtonRadio
      Left = 342
      Top = 6
      Width = 141
      Height = 21
      MidiAware = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      Color = 13684944
      Value = 0
      LowValue = 0
      HighValue = 3
      OnMouseDown = rbModeMouseDown
      OnChange = rbModeChange
      ParentColor = False
      ParentFont = False
      OnClick = rbModeClick
      ButtonText.Strings = (
        'Params'
        'Global'
        'Morph'
        'Patch')
      HightlightColor = clAqua
      BorderColor = clBlack
      Bevel = False
      Orientation = otHorizontal
      ButtonCount = 4
    end
    object rbParamColumn: TG2GraphButtonRadio
      Left = 489
      Top = 6
      Width = 73
      Height = 21
      MidiAware = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      Color = clActiveBorder
      Value = 0
      LowValue = 0
      HighValue = 2
      OnMouseDown = rbParamColumnMouseDown
      OnChange = rbParamColumnChange
      ParentColor = False
      ParentFont = False
      ButtonText.Strings = (
        '1'
        '2'
        '3')
      HightlightColor = clAqua
      BorderColor = clBlack
      Bevel = False
      Orientation = otHorizontal
      ButtonCount = 3
    end
  end
end
