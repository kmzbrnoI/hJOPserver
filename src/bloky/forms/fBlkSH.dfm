object F_BlkSH: TF_BlkSH
  Left = 741
  Top = 209
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Sou'#269'tov'#225' hl'#225'ska : [sh]'
  ClientHeight = 369
  ClientWidth = 337
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object L_P02: TLabel
    Left = 8
    Top = 8
    Width = 66
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'N'#225'zev bloku :'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object L_SCom02: TLabel
    Left = 9
    Top = 36
    Width = 11
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'ID'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object L_Usek03: TLabel
    Left = 7
    Top = 59
    Width = 42
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Stanice :'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object E_Name: TEdit
    Left = 120
    Top = 8
    Width = 211
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    MaxLength = 32
    TabOrder = 0
  end
  object B_save_P: TButton
    Left = 248
    Top = 334
    Width = 82
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Pou'#382#237't'
    Default = True
    TabOrder = 4
    OnClick = B_save_PClick
  end
  object B_Storno: TButton
    Left = 152
    Top = 334
    Width = 81
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Storno'
    TabOrder = 5
    OnClick = B_StornoClick
  end
  object SE_ID: TSpinEdit
    Left = 120
    Top = 33
    Width = 211
    Height = 22
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    MaxValue = 2147483647
    MinValue = 1
    TabOrder = 1
    Value = 1
  end
  object LB_Stanice: TListBox
    Left = 120
    Top = 59
    Width = 209
    Height = 46
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ItemHeight = 13
    ParentFont = False
    TabOrder = 2
  end
  object GB_Prejezdy: TGroupBox
    Left = 7
    Top = 110
    Width = 322
    Height = 211
    Caption = ' P'#345'ejezdy '
    TabOrder = 3
    object LV_Prejezdy: TListView
      Left = 1
      Top = 55
      Width = 318
      Height = 114
      Columns = <
        item
          Caption = 'N'#225'zev p'#345'ejezdu'
          Width = 280
        end>
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnChange = LV_PrejezdyChange
    end
    object B_Remove: TButton
      Left = 113
      Top = 16
      Width = 96
      Height = 25
      Caption = 'Smazat p'#345'ejezd'
      TabOrder = 3
      OnClick = B_RemoveClick
    end
    object CB_Prj_Add: TComboBox
      Left = 6
      Top = 177
      Width = 232
      Height = 21
      Style = csDropDownList
      ItemHeight = 0
      TabOrder = 1
    end
    object B_Add: TButton
      Left = 241
      Top = 175
      Width = 75
      Height = 25
      Caption = 'P'#345'idat p'#345'ejezd'
      TabOrder = 2
      OnClick = B_AddClick
    end
  end
end
