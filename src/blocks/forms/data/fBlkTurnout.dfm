object F_BlkTurnout: TF_BlkTurnout
  Left = 613
  Top = 185
  ActiveControl = B_Save
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Upravit blok xxx (v'#253'hybka)'
  ClientHeight = 385
  ClientWidth = 937
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
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
  object E_Name: TEdit
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
    Top = 66
    Width = 304
    Height = 157
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' Ovl'#225'd'#225'n'#237' v'#253'hybky - RCS v'#253'stupy '
    TabOrder = 2
    object L_Vyh05: TLabel
      Left = 151
      Top = 20
      Width = 53
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'RCS modul'
    end
    object L_Vyh06: TLabel
      Left = 8
      Top = 40
      Width = 85
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Poloha + (v'#253'stup):'
    end
    object L_Vyh07: TLabel
      Left = 8
      Top = 72
      Width = 82
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Poloha - (v'#253'stup):'
    end
    object Label1: TLabel
      Left = 230
      Top = 20
      Width = 43
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'RCS port'
    end
    object Label13: TLabel
      Left = 8
      Top = 104
      Width = 61
      Height = 13
      Caption = 'Typ v'#253'stupu:'
    end
    object SE_Out_Plus_port: TSpinEdit
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
    object SE_Out_Minus_port: TSpinEdit
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
    object SE_Out_Plus_module: TSpinEdit
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
    object SE_Out_Minus_module: TSpinEdit
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
    object CB_OutputType: TComboBox
      Left = 8
      Top = 123
      Width = 289
      Height = 21
      Style = csDropDownList
      TabOrder = 4
      Items.Strings = (
        'Kr'#225'tkodob'#283' aktivn'#237' p'#345'i ru'#269'n'#237'm stav'#283'n'#237', trvale aktivn'#237' p'#345'i z'#225'v'#283'ru'
        'V'#382'dy trvale aktivn'#237
        'Aktivn'#237' do dosa'#382'en'#237' koncov'#233' polohy (nebo timeout)')
    end
  end
  object B_Storno: TButton
    Left = 771
    Top = 353
    Width = 74
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Storno'
    TabOrder = 11
    OnClick = B_StornoClick
  end
  object B_Save: TButton
    Left = 854
    Top = 353
    Width = 76
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Pou'#382#237't'
    Default = True
    TabOrder = 10
    OnClick = B_SaveClick
  end
  object GB_Lock: TGroupBox
    Left = 318
    Top = 87
    Width = 304
    Height = 83
    Caption = ' Z'#225'mek '
    TabOrder = 6
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
    object CB_Lock: TComboBox
      Left = 107
      Top = 20
      Width = 189
      Height = 21
      Style = csDropDownList
      TabOrder = 0
    end
    object CHB_Lock: TCheckBox
      Left = 12
      Top = 24
      Width = 64
      Height = 17
      Caption = 'Z'#225'mek'
      TabOrder = 1
      OnClick = CHB_LockClick
    end
    object CB_Lock_Pos: TComboBox
      Left = 184
      Top = 47
      Width = 112
      Height = 21
      Style = csDropDownList
      TabOrder = 2
      Items.Strings = (
        '+'
        '-')
    end
  end
  object GB_Neprofil: TGroupBox
    Left = 318
    Top = 176
    Width = 304
    Height = 138
    Caption = ' Neprofilov'#233' styky '
    TabOrder = 7
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
      TabOrder = 3
    end
  end
  object GB_Coupling: TGroupBox
    Left = 318
    Top = 8
    Width = 304
    Height = 73
    Caption = ' Spojka '
    TabOrder = 5
    object CHB_Coupling: TCheckBox
      Left = 16
      Top = 22
      Width = 64
      Height = 17
      Caption = 'Spojka'
      TabOrder = 0
      OnClick = CHB_CouplingClick
    end
    object CB_Coupling: TComboBox
      Left = 107
      Top = 18
      Width = 187
      Height = 21
      Style = csDropDownList
      TabOrder = 1
    end
    object CHB_Coupling_Common_In: TCheckBox
      Left = 138
      Top = 46
      Width = 95
      Height = 17
      Caption = 'Stejn'#233' vstupy'
      TabOrder = 2
    end
    object CHB_Coupling_Common_Out: TCheckBox
      Left = 16
      Top = 46
      Width = 97
      Height = 17
      Caption = 'Stejn'#233' v'#253'stupy'
      TabOrder = 3
    end
  end
  object GB_Indications: TGroupBox
    Left = 628
    Top = 8
    Width = 302
    Height = 156
    Caption = ' Indikace polohy (v pultu) '
    TabOrder = 8
    object Label3: TLabel
      Left = 7
      Top = 88
      Width = 42
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Poloha +'
    end
    object Label4: TLabel
      Left = 7
      Top = 120
      Width = 39
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Poloha -'
    end
    object Label5: TLabel
      Left = 150
      Top = 64
      Width = 53
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'RCS modul'
    end
    object Label6: TLabel
      Left = 229
      Top = 64
      Width = 43
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'RCS port'
    end
    object SE_Ind_Minus_Module: TSpinEdit
      Left = 150
      Top = 120
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
      OnExit = SE_Ind_Minus_ModuleExit
    end
    object SE_Ind_Plus_Module: TSpinEdit
      Left = 150
      Top = 88
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
      OnExit = SE_Ind_Plus_ModuleExit
    end
    object SE_Ind_Plus_Port: TSpinEdit
      Left = 226
      Top = 88
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
    object SE_Ind_Minus_Port: TSpinEdit
      Left = 226
      Top = 120
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
    object CHB_Indication: TCheckBox
      Left = 7
      Top = 19
      Width = 186
      Height = 17
      Caption = 'Indikovat polohu v'#253'hybky (v pultu)'
      TabOrder = 0
      OnClick = CHB_IndicationClick
    end
    object CHB_Indication_Pst: TCheckBox
      Left = 7
      Top = 42
      Width = 286
      Height = 17
      Caption = 'Indikace aktivn'#237' pouze p'#345'i aktivn'#237'm pomocn'#233'm stav'#283'dle'
      TabOrder = 1
    end
  end
  object GB_Controllers: TGroupBox
    Left = 628
    Top = 170
    Width = 302
    Height = 156
    Caption = ' Voli'#269'e pro p'#345'estaven'#237' (v pultu) '
    TabOrder = 9
    object Label7: TLabel
      Left = 7
      Top = 88
      Width = 42
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Poloha +'
    end
    object Label8: TLabel
      Left = 7
      Top = 120
      Width = 39
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Poloha -'
    end
    object Label9: TLabel
      Left = 150
      Top = 64
      Width = 53
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'RCS modul'
    end
    object Label10: TLabel
      Left = 229
      Top = 64
      Width = 43
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'RCS port'
    end
    object SE_Cont_Minus_Module: TSpinEdit
      Left = 150
      Top = 120
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
      OnExit = SE_Cont_Minus_ModuleExit
    end
    object SE_Cont_Plus_Module: TSpinEdit
      Left = 150
      Top = 88
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
      OnExit = SE_Cont_Plus_ModuleExit
    end
    object SE_Cont_Plus_Port: TSpinEdit
      Left = 226
      Top = 88
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
    object SE_Cont_Minus_Port: TSpinEdit
      Left = 226
      Top = 120
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
    object CHB_Controllers: TCheckBox
      Left = 7
      Top = 19
      Width = 186
      Height = 17
      Caption = 'Povolit voli'#269'e pro nastaven'#237' polohy'
      TabOrder = 0
      OnClick = CHB_ControllersClick
    end
    object CHB_Controllers_Pst: TCheckBox
      Left = 7
      Top = 42
      Width = 286
      Height = 17
      Caption = 'Voli'#269'e aktivn'#237' pouze p'#345'i aktivn'#237'm pomocn'#233'm stav'#283'dle'
      TabOrder = 1
    end
  end
  object CHB_ManAlwaysEm: TCheckBox
    Left = 8
    Top = 359
    Width = 160
    Height = 17
    Caption = 'Ru'#269'n'#237' stav'#283'n'#237' v'#382'dy nouzov'#233
    TabOrder = 4
  end
  object GB_Feedback: TGroupBox
    Left = 8
    Top = 228
    Width = 304
    Height = 125
    Caption = ' Detekce polohy - RCS vstupy '
    TabOrder = 3
    object L_Vyh08: TLabel
      Left = 7
      Top = 64
      Width = 80
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Poloha + (vstup):'
    end
    object L_Vyh09: TLabel
      Left = 7
      Top = 95
      Width = 77
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Poloha - (vstup):'
    end
    object Label11: TLabel
      Left = 150
      Top = 45
      Width = 53
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'RCS modul'
    end
    object Label12: TLabel
      Left = 229
      Top = 45
      Width = 43
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'RCS port'
    end
    object CHB_Feedback: TCheckBox
      Left = 7
      Top = 24
      Width = 153
      Height = 17
      Caption = 'V'#253'hybka m'#225' detekci polohy'
      TabOrder = 0
      OnClick = CHB_FeedbackClick
    end
    object SE_In_Minus_module: TSpinEdit
      Left = 150
      Top = 95
      Width = 67
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      MaxValue = 0
      MinValue = 0
      TabOrder = 3
      Value = 1
      OnExit = SE_moduleExit
    end
    object SE_In_Plus_module: TSpinEdit
      Left = 150
      Top = 64
      Width = 67
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      MaxValue = 0
      MinValue = 0
      TabOrder = 1
      Value = 1
      OnExit = SE_moduleExit
    end
    object SE_In_Plus_port: TSpinEdit
      Left = 229
      Top = 64
      Width = 67
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      MaxValue = 0
      MinValue = 0
      TabOrder = 2
      Value = 0
    end
    object SE_In_Minus_port: TSpinEdit
      Left = 229
      Top = 95
      Width = 67
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      MaxValue = 0
      MinValue = 0
      TabOrder = 4
      Value = 0
    end
  end
end
