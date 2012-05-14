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
    ActivePage = TabSheet5
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'TCP-IP'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 221
      object Label3: TLabel
        Left = 16
        Top = 56
        Width = 20
        Height = 13
        Caption = 'Port'
      end
      object Label4: TLabel
        Left = 16
        Top = 91
        Width = 22
        Height = 13
        Caption = 'Host'
      end
      object Label8: TLabel
        Left = 16
        Top = 122
        Width = 144
        Height = 13
        Caption = 'Timer broadcast led messages'
      end
      object ePort: TEdit
        Left = 184
        Top = 53
        Width = 89
        Height = 21
        TabOrder = 0
        Text = '2501'
      end
      object eHost: TEdit
        Left = 184
        Top = 88
        Width = 89
        Height = 21
        TabOrder = 1
        Text = '127.0.0.1'
      end
      object cbIsServer: TCheckBox
        Left = 182
        Top = 22
        Width = 91
        Height = 17
        Caption = 'Is server'
        TabOrder = 2
        OnClick = cbIsServerClick
      end
      object eTimerBroadcastLedMessages: TEdit
        Left = 184
        Top = 119
        Width = 89
        Height = 21
        TabOrder = 3
        Text = '500'
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Midi'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 221
      object Label6: TLabel
        Left = 17
        Top = 91
        Width = 69
        Height = 13
        Caption = 'Sysex midi out'
      end
      object Label7: TLabel
        Left = 17
        Top = 51
        Width = 61
        Height = 13
        Caption = 'Sysex midi in'
      end
      object Label9: TLabel
        Left = 17
        Top = 155
        Width = 49
        Height = 13
        Caption = 'Ctrl midi in'
      end
      object cbMidiEnabled: TCheckBox
        Left = 104
        Top = 16
        Width = 97
        Height = 17
        Caption = 'Midi enabled'
        TabOrder = 0
        OnClick = cbMidiEnabledClick
      end
      object cbMidiInDevices: TComboBox
        Left = 104
        Top = 48
        Width = 261
        Height = 21
        TabOrder = 1
        OnSelect = cbMidiInDevicesSelect
      end
      object cbMidiOutDevices: TComboBox
        Left = 104
        Top = 88
        Width = 261
        Height = 21
        TabOrder = 2
        OnSelect = cbMidiOutDevicesSelect
      end
      object cbCtrlMidiInDevices: TComboBox
        Left = 104
        Top = 152
        Width = 261
        Height = 21
        TabOrder = 3
        OnSelect = cbCtrlMidiInDevicesSelect
      end
      object cbCtrlMidiEnabled: TCheckBox
        Left = 104
        Top = 124
        Width = 97
        Height = 17
        Caption = 'Ctrl midi enabled'
        TabOrder = 4
        OnClick = cbCtrlMidiEnabledClick
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'Editor'
      ImageIndex = 4
      object Label11: TLabel
        Left = 24
        Top = 53
        Width = 74
        Height = 13
        Caption = 'Cable thickness'
      end
      object Label12: TLabel
        Left = 24
        Top = 83
        Width = 68
        Height = 13
        Caption = 'Slot strip color'
      end
      object Label13: TLabel
        Left = 24
        Top = 111
        Width = 106
        Height = 13
        Caption = 'Slot strip inverse color'
      end
      object Label14: TLabel
        Left = 24
        Top = 139
        Width = 110
        Height = 13
        Caption = 'Slot strip disabled color'
      end
      object Label15: TLabel
        Left = 24
        Top = 167
        Width = 67
        Height = 13
        Caption = 'Highlight color'
      end
      object cbSlotStripColor: TColorBox
        Left = 152
        Top = 80
        Width = 145
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbCustomColors]
        TabOrder = 0
        OnChange = cbSlotStripColorChange
      end
      object cbSlotStripInverseColor: TColorBox
        Left = 152
        Top = 108
        Width = 145
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbCustomColors]
        TabOrder = 1
        OnChange = cbSlotStripInverseColorChange
      end
      object cbSlotStripDisabledColor: TColorBox
        Left = 152
        Top = 136
        Width = 145
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbCustomColors]
        TabOrder = 2
        OnChange = cbSlotStripDisabledColorChange
      end
      object cbHighLightColor: TColorBox
        Left = 152
        Top = 164
        Width = 145
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbCustomColors]
        TabOrder = 3
        OnChange = cbHighLightColorChange
      end
      object eCableThickness: TEdit
        Left = 152
        Top = 50
        Width = 49
        Height = 21
        TabOrder = 4
        Text = '2'
      end
      object cbLogEnabled: TCheckBox
        Left = 152
        Top = 24
        Width = 97
        Height = 17
        Caption = 'Log enabled'
        TabOrder = 5
        OnClick = cbLogEnabledClick
      end
      object cbOnlyTextMenus: TCheckBox
        Left = 152
        Top = 192
        Width = 97
        Height = 17
        Caption = 'Only text menus'
        TabOrder = 6
        OnClick = cbOnlyTextMenusClick
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Patch manager'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 221
      DesignSize = (
        480
        234)
      object Label5: TLabel
        Left = 16
        Top = 27
        Width = 64
        Height = 13
        Caption = 'Root folder : '
      end
      object eRootFolder: TEdit
        Left = 85
        Top = 24
        Width = 322
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
      end
      object bSelectRootFolder: TButton
        Left = 413
        Top = 22
        Width = 35
        Height = 25
        Anchors = [akTop, akRight]
        Caption = '...'
        TabOrder = 1
        OnClick = bSelectRootFolderClick
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'OSC'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 221
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 480
        Height = 59
        Align = alTop
        TabOrder = 0
        object Label1: TLabel
          Left = 16
          Top = 16
          Width = 59
          Height = 13
          Caption = 'OSC Host IP'
        end
        object Label2: TLabel
          Left = 184
          Top = 16
          Width = 69
          Height = 13
          Caption = 'OSC Host Port'
        end
        object Button2: TButton
          Left = 334
          Top = 11
          Width = 75
          Height = 25
          Caption = 'Activate'
          TabOrder = 0
          OnClick = Button2Click
        end
        object eOSCServerIP: TEdit
          Left = 88
          Top = 13
          Width = 73
          Height = 21
          TabOrder = 1
          Text = '127.0.0.1'
        end
        object eOSCHostPort: TEdit
          Left = 272
          Top = 13
          Width = 41
          Height = 21
          TabOrder = 2
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
    Left = 416
    Top = 32
  end
end
