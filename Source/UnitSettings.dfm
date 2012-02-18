object frmSettings: TfrmSettings
  Left = 0
  Top = 0
  Caption = 'Settings'
  ClientHeight = 172
  ClientWidth = 488
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 488
    Height = 172
    ActivePage = TabSheet4
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'TCP-IP'
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
      object ePort: TEdit
        Left = 86
        Top = 53
        Width = 91
        Height = 21
        TabOrder = 0
        Text = '2501'
      end
      object eHost: TEdit
        Left = 88
        Top = 88
        Width = 89
        Height = 21
        TabOrder = 1
        Text = '127.0.0.1'
      end
      object cbIsServer: TCheckBox
        Left = 86
        Top = 22
        Width = 91
        Height = 17
        Caption = 'Is server'
        TabOrder = 2
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Midi'
      ImageIndex = 3
      object Label6: TLabel
        Left = 16
        Top = 91
        Width = 37
        Height = 13
        Caption = 'Midi out'
      end
      object Label7: TLabel
        Left = 16
        Top = 51
        Width = 29
        Height = 13
        Caption = 'Midi in'
      end
      object mcbMidiIn: TMidiDeviceComboBox
        Left = 72
        Top = 48
        Width = 185
        Height = 21
        Style = csDropDown
        ItemIndex = 0
        TabOrder = 0
        Text = 'Hercules DJ Console Rmx MIDI'
        OnChange = mcbMidiInChange
        Items.Strings = (
          'Hercules DJ Console Rmx MIDI'
          '2- MIDISPORT 4x4 In A'
          '2- MIDISPORT 4x4 In B'
          '2- MIDISPORT 4x4 In C'
          '2- MIDISPORT 4x4 In D'
          'Pro40 MIDI')
      end
      object mcbMidiOut: TMidiDeviceComboBox
        Left = 72
        Top = 88
        Width = 185
        Height = 21
        Style = csDropDown
        ItemIndex = 0
        TabOrder = 1
        Text = 'Microsoft GS Wavetable Synth'
        OnChange = mcbMidiOutChange
        Items.Strings = (
          'Microsoft GS Wavetable Synth'
          'Hercules DJ Console Rmx MIDI'
          '2- MIDISPORT 4x4 Out A'
          '2- MIDISPORT 4x4 Out C'
          '2- MIDISPORT 4x4 Out B'
          '2- MIDISPORT 4x4 Out D'
          'Pro40 MIDI')
        DeviceType = mdtOutput
      end
      object cbEnableMidi: TCheckBox
        Left = 72
        Top = 16
        Width = 97
        Height = 17
        Caption = 'Enable midi'
        TabOrder = 2
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Patch manager'
      ImageIndex = 2
      DesignSize = (
        480
        144)
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
      object Button1: TButton
        Left = 413
        Top = 22
        Width = 35
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Button1'
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
        Height = 85
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
    Left = 440
    Top = 8
  end
end
