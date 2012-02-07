object frmSettings: TfrmSettings
  Left = 0
  Top = 0
  Caption = 'Settings'
  ClientHeight = 290
  ClientWidth = 554
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
    Width = 554
    Height = 290
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Server'
      object Label3: TLabel
        Left = 16
        Top = 24
        Width = 20
        Height = 13
        Caption = 'Port'
      end
      object Label4: TLabel
        Left = 16
        Top = 51
        Width = 22
        Height = 13
        Caption = 'Host'
      end
      object ePort: TEdit
        Left = 86
        Top = 21
        Width = 91
        Height = 21
        TabOrder = 0
        Text = '2501'
      end
      object eHost: TEdit
        Left = 88
        Top = 48
        Width = 89
        Height = 21
        TabOrder = 1
        Text = '127.0.0.1'
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'OSC'
      ImageIndex = 1
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 546
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
        Width = 546
        Height = 203
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
    object TabSheet3: TTabSheet
      Caption = 'Patch manager'
      ImageIndex = 2
      DesignSize = (
        546
        262)
      object Label5: TLabel
        Left = 16
        Top = 27
        Width = 63
        Height = 13
        Caption = 'Bass folder : '
      end
      object eBassFolder: TEdit
        Left = 85
        Top = 24
        Width = 436
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
      end
    end
  end
  object IdUDPServer1: TIdUDPServer
    OnStatus = IdUDPServer1Status
    Bindings = <>
    DefaultPort = 5678
    Left = 216
    Top = 8
  end
end
