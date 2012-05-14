object frmPatchManager: TfrmPatchManager
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Patch manager'
  ClientHeight = 404
  ClientWidth = 530
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  OnCreate = FormCreate
  OnKeyUp = FormKeyUp
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object TabControl1: TTabControl
    Left = 0
    Top = 0
    Width = 530
    Height = 404
    Align = alClient
    TabOrder = 0
    Tabs.Strings = (
      'Perf. (disk)'
      'Patch (disk)'
      'Perf.'
      'Patch')
    TabIndex = 0
    OnChange = TabControl1Change
    object lvExternalPatch: TListView
      Left = 16
      Top = 152
      Width = 495
      Height = 105
      Columns = <
        item
          Caption = 'Patch file'
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
      TabOrder = 0
      ViewStyle = vsReport
      OnColumnClick = lvExternalPatchColumnClick
      OnCompare = lvExternalCompare
      OnDblClick = aLoadPatchExecute
      OnKeyUp = lvExternalPatchKeyUp
    end
    object lvInternal: TListView
      Left = 16
      Top = 271
      Width = 495
      Height = 117
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
      TabOrder = 1
      ViewStyle = vsReport
      OnColumnClick = lvInternalColumnClick
      OnCompare = lvInternalCompare
      OnDblClick = aRestoreExecute
      OnKeyUp = lvInternalKeyUp
    end
    object lvExternalPerf: TListView
      Left = 16
      Top = 40
      Width = 495
      Height = 106
      Columns = <
        item
          Caption = 'Perf file'
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
      TabOrder = 2
      ViewStyle = vsReport
      OnColumnClick = lvExternalPerfColumnClick
      OnCompare = lvExternalCompare
      OnDblClick = aLoadPerfExecute
      OnKeyUp = lvExternalPerfKeyUp
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
    object aLoadPatch: TAction
      Caption = 'Load patch'
      OnExecute = aLoadPatchExecute
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
    object aReadDirPerf: TAction
      Caption = 'Read dir perf'
    end
    object aLoadPerf: TAction
      Caption = 'Load perf'
      OnExecute = aLoadPerfExecute
    end
  end
end
