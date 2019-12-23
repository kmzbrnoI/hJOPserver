object F_BlkVyhybka: TF_BlkVyhybka
  Left = 613
  Top = 185
  ActiveControl = B_Save
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Editovat data bloku : [blok] (vyhybka)'
  ClientHeight = 329
  ClientWidth = 641
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
  PixelsPerInch = 96
  TextHeight = 13
  object L_Vyh01: TLabel
    Left = 7
    Top = 7
    Width = 37
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'N'#225'zev :'
  end
  object L_Vyh02: TLabel
    Left = 7
    Top = 40
    Width = 17
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'ID :'
  end
  object L_Usek03: TLabel
    Left = 7
    Top = 72
    Width = 42
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Stanice :'
  end
  object E_Nazev: TEdit
    Left = 118
    Top = 7
    Width = 195
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    MaxLength = 32
    TabOrder = 0
  end
  object SE_ID: TSpinEdit
    Left = 118
    Top = 40
    Width = 195
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
  object GB_RCS: TGroupBox
    Left = 7
    Top = 150
    Width = 304
    Height = 172
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' Ovl'#225'd'#225'n'#237' v'#253'hybky - RCS '
    TabOrder = 5
    object L_Vyh05: TLabel
      Left = 151
      Top = 16
      Width = 29
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Modul'
    end
    object L_Vyh06: TLabel
      Left = 8
      Top = 40
      Width = 135
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Nastaven'#237' polohy + (v'#253'stup):'
    end
    object L_Vyh07: TLabel
      Left = 8
      Top = 72
      Width = 132
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Nastaven'#237' polohy - (v'#253'stup):'
    end
    object L_Vyh08: TLabel
      Left = 8
      Top = 104
      Width = 122
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Detekce polohy + (vstup):'
    end
    object L_Vyh09: TLabel
      Left = 8
      Top = 136
      Width = 119
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Detekce polohy - (vstup):'
    end
    object Label1: TLabel
      Left = 230
      Top = 16
      Width = 19
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Port'
    end
    object SE_VystPlus_port: TSpinEdit
      Left = 230
      Top = 40
      Width = 67
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      MaxValue = 0
      MinValue = 0
      TabOrder = 1
      Value = 0
    end
    object SE_VystMinus_port: TSpinEdit
      Left = 230
      Top = 72
      Width = 67
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      MaxValue = 0
      MinValue = 0
      TabOrder = 3
      Value = 0
    end
    object SE_VstPlus_port: TSpinEdit
      Left = 230
      Top = 104
      Width = 67
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      MaxValue = 0
      MinValue = 0
      TabOrder = 5
      Value = 0
    end
    object SE_VstMinus_port: TSpinEdit
      Left = 230
      Top = 136
      Width = 67
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      MaxValue = 0
      MinValue = 0
      TabOrder = 7
      Value = 0
    end
    object SE_VystPlus_module: TSpinEdit
      Left = 151
      Top = 40
      Width = 67
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      MaxValue = 0
      MinValue = 0
      TabOrder = 0
      Value = 1
      OnExit = SE_moduleExit
    end
    object SE_VystMinus_module: TSpinEdit
      Left = 151
      Top = 72
      Width = 67
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      MaxValue = 0
      MinValue = 0
      TabOrder = 2
      Value = 1
      OnExit = SE_moduleExit
    end
    object SE_VstPlus_module: TSpinEdit
      Left = 151
      Top = 104
      Width = 67
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      MaxValue = 0
      MinValue = 0
      TabOrder = 4
      Value = 1
      OnExit = SE_moduleExit
    end
    object SE_VstMinus_module: TSpinEdit
      Left = 151
      Top = 136
      Width = 67
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      MaxValue = 0
      MinValue = 0
      TabOrder = 6
      Value = 1
      OnExit = SE_moduleExit
    end
  end
  object B_Storno: TButton
    Left = 475
    Top = 297
    Width = 74
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Storno'
    TabOrder = 7
    OnClick = B_StornoClick
  end
  object B_Save: TButton
    Left = 554
    Top = 297
    Width = 76
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Pou'#382#237't'
    Default = True
    TabOrder = 6
    OnClick = B_SaveClick
  end
  object LB_Stanice: TListBox
    Left = 118
    Top = 72
    Width = 192
    Height = 46
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    ItemHeight = 13
    TabOrder = 2
  end
  object CHB_Spojka: TCheckBox
    Left = 15
    Top = 125
    Width = 64
    Height = 17
    Caption = 'Spojka'
    TabOrder = 3
    OnClick = CHB_SpojkaClick
  end
  object CB_Spojka: TComboBox
    Left = 118
    Top = 123
    Width = 194
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 4
    OnChange = CB_SpojkaChange
  end
  object GB_Zamek: TGroupBox
    Left = 326
    Top = 8
    Width = 304
    Height = 83
    Caption = ' Z'#225'mek '
    TabOrder = 8
    object Label2: TLabel
      Left = 12
      Top = 46
      Width = 142
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Poloha v'#253'hybky pro zamknut'#237':'
    end
    object CB_Zamek: TComboBox
      Left = 107
      Top = 20
      Width = 189
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
    end
    object CHB_Zamek: TCheckBox
      Left = 12
      Top = 24
      Width = 64
      Height = 17
      Caption = 'Z'#225'mek'
      TabOrder = 1
      OnClick = CHB_ZamekClick
    end
    object CB_Zamek_Poloha: TComboBox
      Left = 184
      Top = 47
      Width = 112
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 2
      Items.Strings = (
        '+'
        '-')
    end
  end
  object GB_Neprofil: TGroupBox
    Left = 326
    Top = 103
    Width = 304
    Height = 138
    Caption = ' Neprofilov'#233' styky '
    TabOrder = 9
    object CHB_npPlus: TCheckBox
      Left = 12
      Top = 24
      Width = 284
      Height = 17
      Caption = 'Kontrolovat volnost '#250'seku pro polohu +'
      TabOrder = 0
      OnClick = CHB_npPlusClick
    end
    object CB_npPlus: TComboBox
      Left = 12
      Top = 47
      Width = 284
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
    end
    object CHB_npMinus: TCheckBox
      Left = 12
      Top = 80
      Width = 284
      Height = 17
      Caption = 'Kontrolovat volnost '#250'seku pro polohu -'
      TabOrder = 2
      OnClick = CHB_npMinusClick
    end
    object CB_npMinus: TComboBox
      Left = 12
      Top = 103
      Width = 284
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 3
    end
  end
end
