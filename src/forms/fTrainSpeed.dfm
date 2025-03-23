object F_TrainSpeed: TF_TrainSpeed
  Left = 0
  Top = 0
  Align = alClient
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'F_TrainSpeed'
  ClientHeight = 163
  ClientWidth = 298
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poDefault
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  TextHeight = 13
  object LV_Speeds: TListView
    Left = 0
    Top = 0
    Width = 298
    Height = 163
    Align = alClient
    Columns = <
      item
        Caption = 'Rychlost'
        Width = 60
      end
      item
        Caption = 'Typ vlaku'
        Width = 100
      end
      item
        Caption = 'P'#345'echodnost HV'
        Width = 100
      end>
    DragMode = dmAutomatic
    MultiSelect = True
    ReadOnly = True
    RowSelect = True
    PopupMenu = PM_Speeds
    TabOrder = 0
    ViewStyle = vsReport
    OnDblClick = LV_SpeedsDblClick
    OnDragDrop = LV_SpeedsDragDrop
    OnDragOver = LV_SpeedsDragOver
    OnKeyDown = LV_SpeedsKeyDown
  end
  object PM_Speeds: TPopupMenu
    OnPopup = PM_SpeedsPopup
    Left = 248
    Top = 104
    object MI_NewRecord: TMenuItem
      Caption = 'Nov'#253' z'#225'znam'
      OnClick = MI_NewRecordClick
    end
    object MI_Delete: TMenuItem
      Caption = 'Smazat'
      OnClick = MI_DeleteClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object MI_Help: TMenuItem
      Caption = 'N'#225'pov'#283'da'
      OnClick = MI_HelpClick
    end
  end
end
