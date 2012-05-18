object frmSettings: TfrmSettings
  Left = 0
  Top = 0
  Caption = 'Settings'
  ClientHeight = 262
  ClientWidth = 488
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyUp = FormKeyUp
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 488
    Height = 262
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'TCP-IP'
      object cbIsServer: TCheckBox
        Left = 182
        Top = 22
        Width = 91
        Height = 17
        Caption = 'Is server'
        TabOrder = 0
        OnClick = cbIsServerClick
      end
      object StaticText1: TStaticText
        Left = 16
        Top = 57
        Width = 24
        Height = 17
        Caption = 'Port'
        FocusControl = ePort
        TabOrder = 1
        TabStop = True
      end
      object StaticText2: TStaticText
        Left = 16
        Top = 92
        Width = 26
        Height = 17
        Caption = 'Host'
        FocusControl = eHost
        TabOrder = 3
        TabStop = True
      end
      object StaticText3: TStaticText
        Left = 16
        Top = 123
        Width = 148
        Height = 17
        Caption = 'Timer broadcast led messages'
        FocusControl = eTimerBroadcastLedMessages
        TabOrder = 5
        TabStop = True
      end
      object ePort: DEdit
        Left = 182
        Top = 53
        Width = 121
        Height = 21
        NumbersOnly = True
        TabOrder = 2
        Text = '2501'
      end
      object eHost: DEdit
        Left = 182
        Top = 87
        Width = 121
        Height = 21
        TabOrder = 4
        Text = '127.0.0.1'
      end
      object eTimerBroadcastLedMessages: DEdit
        Left = 182
        Top = 119
        Width = 121
        Height = 21
        TabOrder = 6
        Text = '500'
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Midi'
      ImageIndex = 3
      object cbMidiEnabled: TCheckBox
        Left = 104
        Top = 16
        Width = 97
        Height = 17
        Caption = 'Midi enabled'
        TabOrder = 0
        OnClick = cbMidiEnabledClick
      end
      object cbCtrlMidiEnabled: TCheckBox
        Left = 104
        Top = 124
        Width = 97
        Height = 17
        Caption = 'Ctrl midi enabled'
        TabOrder = 5
        OnClick = cbCtrlMidiEnabledClick
      end
      object StaticText5: TStaticText
        Left = 17
        Top = 92
        Width = 73
        Height = 17
        Caption = 'Sysex midi out'
        TabOrder = 3
        TabStop = True
      end
      object StaticText6: TStaticText
        Left = 17
        Top = 156
        Width = 53
        Height = 17
        Caption = 'Ctrl midi in'
        TabOrder = 6
        TabStop = True
      end
      object StaticText4: TStaticText
        Left = 17
        Top = 52
        Width = 65
        Height = 17
        Caption = 'Sysex midi in'
        TabOrder = 1
        TabStop = True
      end
      object cbMidiOutDevices: DCombobox
        Left = 104
        Top = 88
        Width = 261
        Height = 21
        TabOrder = 4
        OnSelect = cbMidiOutDevicesSelect
      end
      object cbCtrlMidiInDevices: DCombobox
        Left = 104
        Top = 152
        Width = 261
        Height = 21
        TabOrder = 7
        OnSelect = cbCtrlMidiInDevicesSelect
      end
      object cbMidiInDevices: DCombobox
        Left = 104
        Top = 48
        Width = 261
        Height = 21
        TabOrder = 2
        OnSelect = cbMidiInDevicesSelect
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'Editor'
      ImageIndex = 4
      object cbSlotStripColor: TColorBox
        Left = 152
        Top = 80
        Width = 145
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbCustomColors]
        TabOrder = 4
        OnChange = cbSlotStripColorChange
      end
      object cbSlotStripInverseColor: TColorBox
        Left = 152
        Top = 108
        Width = 145
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbCustomColors]
        TabOrder = 6
        OnChange = cbSlotStripInverseColorChange
      end
      object cbSlotStripDisabledColor: TColorBox
        Left = 152
        Top = 136
        Width = 145
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbCustomColors]
        TabOrder = 8
        OnChange = cbSlotStripDisabledColorChange
      end
      object cbHighLightColor: TColorBox
        Left = 152
        Top = 164
        Width = 145
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbCustomColors]
        TabOrder = 10
        OnChange = cbHighLightColorChange
      end
      object cbLogEnabled: TCheckBox
        Left = 152
        Top = 24
        Width = 97
        Height = 17
        Caption = 'Log enabled'
        TabOrder = 0
        OnClick = cbLogEnabledClick
      end
      object cbOnlyTextMenus: TCheckBox
        Left = 152
        Top = 192
        Width = 97
        Height = 17
        Caption = 'Only text menus'
        TabOrder = 11
        OnClick = cbOnlyTextMenusClick
      end
      object StaticText7: TStaticText
        Left = 16
        Top = 54
        Width = 78
        Height = 17
        Caption = 'Cable thickness'
        FocusControl = eCableThickness
        TabOrder = 1
        TabStop = True
      end
      object StaticText8: TStaticText
        Left = 16
        Top = 85
        Width = 72
        Height = 17
        Caption = 'Slot strip color'
        FocusControl = cbSlotStripColor
        TabOrder = 3
        TabStop = True
      end
      object StaticText9: TStaticText
        Left = 16
        Top = 113
        Width = 110
        Height = 17
        Caption = 'Slot strip inverse color'
        FocusControl = cbSlotStripInverseColor
        TabOrder = 5
        TabStop = True
      end
      object StaticText10: TStaticText
        Left = 16
        Top = 141
        Width = 114
        Height = 17
        Caption = 'Slot strip disabled color'
        FocusControl = cbSlotStripDisabledColor
        TabOrder = 7
        TabStop = True
      end
      object StaticText11: TStaticText
        Left = 16
        Top = 169
        Width = 71
        Height = 17
        Caption = 'Highlight color'
        FocusControl = cbHighLightColor
        TabOrder = 9
        TabStop = True
      end
      object eCableThickness: DEdit
        Left = 152
        Top = 50
        Width = 33
        Height = 21
        NumbersOnly = True
        TabOrder = 2
        Text = '2'
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Patch manager'
      ImageIndex = 2
      DesignSize = (
        480
        234)
      object bSelectRootFolder: TButton
        Left = 413
        Top = 22
        Width = 35
        Height = 25
        Anchors = [akTop, akRight]
        Caption = '...'
        TabOrder = 2
        OnClick = bSelectRootFolderClick
      end
      object StaticText12: TStaticText
        Left = 11
        Top = 30
        Width = 68
        Height = 17
        Caption = 'Root folder : '
        FocusControl = eRootFolder
        TabOrder = 0
        TabStop = True
      end
      object eRootFolder: DEdit
        Left = 81
        Top = 26
        Width = 322
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'OSC'
      ImageIndex = 1
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 480
        Height = 59
        Align = alTop
        TabOrder = 0
        object Button2: TButton
          Left = 334
          Top = 11
          Width = 75
          Height = 25
          Caption = 'Activate'
          TabOrder = 4
          OnClick = Button2Click
        end
        object StaticText13: TStaticText
          Left = 19
          Top = 17
          Width = 63
          Height = 17
          Caption = 'OSC Host IP'
          FocusControl = eOSCServerIP
          TabOrder = 0
          TabStop = True
        end
        object StaticText14: TStaticText
          Left = 191
          Top = 17
          Width = 73
          Height = 17
          Caption = 'OSC Host Port'
          FocusControl = eOSCHostPort
          TabOrder = 2
          TabStop = True
        end
        object eOSCServerIP: DEdit
          Left = 90
          Top = 13
          Width = 73
          Height = 21
          TabOrder = 1
          Text = '127.0.0.1'
        end
        object eOSCHostPort: DEdit
          Left = 273
          Top = 13
          Width = 41
          Height = 21
          TabOrder = 3
          Text = '5678'
        end
      end
      object Memo1: TMemo
        Left = 0
        Top = 59
        Width = 480
        Height = 175
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          'Memo1')
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 1
      end
    end
  end
  object IdUDPServer1: TIdUDPServer
    OnStatus = IdUDPServer1Status
    Bindings = <>
    DefaultPort = 5678
    Left = 392
    Top = 176
  end
end
