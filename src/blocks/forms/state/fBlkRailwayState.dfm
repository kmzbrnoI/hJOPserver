object F_BlkRailwayState: TF_BlkRailwayState
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'F_BlkRailwayState'
  ClientHeight = 185
  ClientWidth = 514
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object L_Usek21: TLabel
    Left = 8
    Top = 8
    Width = 35
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Z'#225'v'#283'r :'
  end
  object L_Usek24: TLabel
    Left = 7
    Top = 33
    Width = 28
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Sm'#283'r:'
  end
  object L_Usek25: TLabel
    Left = 7
    Top = 56
    Width = 37
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = #381#225'dost:'
  end
  object L_Usek20: TLabel
    Left = 7
    Top = 84
    Width = 118
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'P'#345'edpov'#237'dan'#225' souprava:'
  end
  object Label1: TLabel
    Left = 8
    Top = 107
    Width = 50
    Height = 13
    Caption = 'Soupravy:'
  end
  object Label2: TLabel
    Left = 296
    Top = 59
    Width = 50
    Height = 13
    Caption = 'Souprava:'
  end
  object CB_Zadost: TComboBox
    Left = 176
    Top = 58
    Width = 107
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Style = csDropDownList
    TabOrder = 0
    Items.Strings = (
      'Ne'
      'Ano'
      '')
  end
  object CB_Smer: TComboBox
    Left = 176
    Top = 33
    Width = 107
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Style = csDropDownList
    TabOrder = 1
    Items.Strings = (
      'disabled'
      #382#225'dn'#253
      'A->B'
      'B->A')
  end
  object CB_Zaver: TComboBox
    Left = 176
    Top = 8
    Width = 107
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Style = csDropDownList
    TabOrder = 2
    Items.Strings = (
      'Ne'
      'Ano')
  end
  object B_SaveData: TButton
    Left = 210
    Top = 152
    Width = 73
    Height = 24
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Pou'#382#237't'
    Default = True
    TabOrder = 3
    OnClick = B_SaveDataClick
  end
  object B_Obnovit: TButton
    Left = 130
    Top = 152
    Width = 76
    Height = 24
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Obnovit'
    TabOrder = 4
    OnClick = B_ObnovitClick
  end
  object SE_Souprava: TSpinEdit
    Left = 176
    Top = 82
    Width = 107
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 5
    Value = 0
  end
  object E_Soupravy: TEdit
    Left = 8
    Top = 126
    Width = 275
    Height = 21
    ReadOnly = True
    TabOrder = 6
    Text = 'E_Soupravy'
  end
  object B_BP_Enable: TButton
    Left = 296
    Top = 16
    Width = 89
    Height = 25
    Caption = 'Zav'#233'st BP'
    TabOrder = 7
    OnClick = B_BP_EnableClick
  end
  object B_BP_Disable: TButton
    Left = 400
    Top = 16
    Width = 89
    Height = 25
    Caption = 'Zru'#353'it BP'
    TabOrder = 8
    OnClick = B_BP_DisableClick
  end
  object SE_Spr_Add: TSpinEdit
    Left = 296
    Top = 76
    Width = 210
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 9
    Value = 0
  end
  object B_RmSpr: TButton
    Left = 296
    Top = 104
    Width = 108
    Height = 21
    Caption = 'Smazat soupravu'
    TabOrder = 10
    OnClick = B_RmSprClick
  end
  object B_AddSpr: TButton
    Left = 410
    Top = 104
    Width = 96
    Height = 21
    Caption = 'P'#345'idat soupravu'
    TabOrder = 11
    OnClick = B_AddSprClick
  end
end
