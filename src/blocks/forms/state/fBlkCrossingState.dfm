object F_BlkCrossingState: TF_BlkCrossingState
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Stav p'#345'ejezdu [p'#345'ejezd]'
  ClientHeight = 338
  ClientWidth = 569
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 10
    Top = 31
    Width = 31
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = #352't'#237'tek:'
  end
  object Label6: TLabel
    Left = 291
    Top = 31
    Width = 35
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'V'#253'luka:'
  end
  object Label2: TLabel
    Left = 8
    Top = 8
    Width = 26
    Height = 13
    Caption = 'Stav:'
  end
  object M_Note: TMemo
    Left = 8
    Top = 49
    Width = 275
    Height = 49
    Lines.Strings = (
      'M_Note')
    TabOrder = 1
  end
  object M_Lockout: TMemo
    Left = 289
    Top = 49
    Width = 275
    Height = 49
    Lines.Strings = (
      'M_Lockout')
    TabOrder = 2
  end
  object B_Refresh: TButton
    Left = 411
    Top = 305
    Width = 76
    Height = 24
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Obnovit'
    TabOrder = 9
    OnClick = B_RefreshClick
  end
  object B_Apply: TButton
    Left = 491
    Top = 305
    Width = 73
    Height = 24
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Pou'#382#237't'
    Default = True
    TabOrder = 8
    OnClick = B_ApplyClick
  end
  object CHB_PcEmOpen: TCheckBox
    Left = 8
    Top = 104
    Width = 161
    Height = 17
    Caption = 'Nouzov'#283' otev'#345'en v'#253'prav'#269#237'm'
    TabOrder = 3
  end
  object CHB_PcClosed: TCheckBox
    Left = 8
    Top = 127
    Width = 113
    Height = 17
    Caption = 'Uzav'#345'en v'#253'prav'#269#237'm'
    TabOrder = 4
  end
  object CHB_Zaver: TCheckBox
    Left = 291
    Top = 104
    Width = 49
    Height = 17
    Caption = 'Z'#225'v'#283'r'
    Enabled = False
    TabOrder = 5
  end
  object CHB_Annulation: TCheckBox
    Left = 291
    Top = 127
    Width = 57
    Height = 17
    Caption = 'Anulace'
    Enabled = False
    TabOrder = 6
  end
  object LV_Tracks: TListView
    Left = 8
    Top = 150
    Width = 556
    Height = 150
    Columns = <
      item
        Caption = 'Kolej'
      end
      item
        Caption = 'Stav'
        Width = 150
      end
      item
        Caption = 'Uzav'#345'en'
        Width = 60
      end
      item
        Caption = 'Pozitiva'
        Width = 60
      end
      item
        Caption = 'Anulace'
        Width = 60
      end>
    MultiSelect = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 7
    ViewStyle = vsReport
  end
  object CB_State: TComboBox
    Left = 46
    Top = 8
    Width = 518
    Height = 21
    Style = csDropDownList
    Enabled = False
    TabOrder = 0
    Items.Strings = (
      'disabled'
      'none'
      'open'
      'caution'
      'closed')
  end
end
