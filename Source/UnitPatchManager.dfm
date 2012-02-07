object frmPatchManager: TfrmPatchManager
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'frmPatchManager'
  ClientHeight = 432
  ClientWidth = 530
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object TabControl1: TTabControl
    Left = 0
    Top = 0
    Width = 530
    Height = 432
    Align = alClient
    TabOrder = 0
    Tabs.Strings = (
      'Disk'
      'Perf.'
      'Patch')
    TabIndex = 0
    OnChange = TabControl1Change
    object Panel1: TPanel
      Left = 4
      Top = 24
      Width = 522
      Height = 53
      Align = alTop
      TabOrder = 0
      DesignSize = (
        522
        53)
      object cbPath: TComboBox
        Left = 12
        Top = 17
        Width = 460
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = 'C:\Users\Bruno\Documents\Nord Modular Patches\G2 Patches'
      end
      object Button1: TButton
        Left = 480
        Top = 15
        Width = 34
        Height = 25
        Action = aSelectFolder
        Anchors = [akTop, akRight]
        TabOrder = 1
      end
    end
    object lvExternal: TListView
      Left = 16
      Top = 83
      Width = 495
      Height = 166
      Columns = <
        item
          Caption = 'File'
          Width = 150
        end
        item
          Caption = 'Date'
          Width = 80
        end
        item
          Caption = 'Path'
          Width = 250
        end>
      ReadOnly = True
      RowSelect = True
      TabOrder = 1
      ViewStyle = vsReport
      OnColumnClick = lvExternalColumnClick
      OnCompare = lvExternalCompare
      OnDblClick = aLoadExecute
      OnKeyUp = lvExternalKeyUp
    end
    object lvInternal: TListView
      Left = 16
      Top = 255
      Width = 495
      Height = 150
      Columns = <
        item
          Caption = 'Name'
          Width = 150
        end
        item
          Caption = 'Category'
          Width = 120
        end
        item
          Caption = 'Slot'
        end>
      TabOrder = 2
      ViewStyle = vsReport
      OnColumnClick = lvInternalColumnClick
      OnCompare = lvInternalCompare
      OnDblClick = aRestoreExecute
      OnKeyUp = lvInternalKeyUp
    end
  end
  object ActionManager1: TActionManager
    Left = 184
    Top = 136
    StyleName = 'XP Style'
    object aReadDir: TAction
      Caption = 'Read dir'
      OnExecute = aReadDirExecute
    end
    object aSearch: TAction
      Caption = 'Search'
      OnExecute = aSearchExecute
    end
    object aLoad: TAction
      Caption = 'Load'
      OnExecute = aLoadExecute
    end
    object aSelectFolder: TAction
      Caption = '...'
      OnExecute = aSelectFolderExecute
    end
    object aShowPerfs: TAction
      Caption = 'Perf.'
      OnExecute = aShowPerfsExecute
    end
    object aShowPatches: TAction
      Caption = 'Patch'
      OnExecute = aShowPatchesExecute
    end
    object aRestore: TAction
      Caption = 'Restore'
      OnExecute = aRestoreExecute
    end
  end
end
