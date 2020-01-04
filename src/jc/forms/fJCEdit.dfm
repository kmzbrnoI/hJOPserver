object F_JCEdit: TF_JCEdit
  Left = 505
  Top = 156
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Editace vlakove cesty [nazev]'
  ClientHeight = 470
  ClientWidth = 922
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object L_VC_01: TLabel
    Left = 7
    Top = 7
    Width = 65
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'N'#225'zev cesty :'
  end
  object L_VC_02: TLabel
    Left = 8
    Top = 65
    Width = 56
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'N'#225'v'#283'stidlo :'
  end
  object L_VC_11: TLabel
    Left = 8
    Top = 97
    Width = 52
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Typ cesty :'
  end
  object L_VC_07: TLabel
    Left = 8
    Top = 377
    Width = 81
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Dal'#353#237' n'#225'v'#283'stidlo :'
  end
  object L_VC_10: TLabel
    Left = 7
    Top = 407
    Width = 136
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Rychlost (dal'#353#237' n'#225'v. na st'#367'j) :'
  end
  object L_VC_12: TLabel
    Left = 7
    Top = 439
    Width = 170
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Rychlost (dal'#353#237' n'#225'v. povoluje j'#237'zdu) :'
  end
  object Label3: TLabel
    Left = 8
    Top = 34
    Width = 14
    Height = 13
    Caption = 'ID:'
  end
  object E_Name: TEdit
    Left = 136
    Top = 7
    Width = 201
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    MaxLength = 32
    TabOrder = 0
  end
  object GB_ZaveryVyhybek: TGroupBox
    Left = 345
    Top = 7
    Width = 265
    Height = 261
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' V'#253'hybky '
    TabOrder = 8
    object LV_Vyhybky: TListView
      Left = 2
      Top = 54
      Width = 261
      Height = 134
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alBottom
      Columns = <
        item
          Caption = '#'
          Width = 20
        end
        item
          Caption = 'V'#253'hybka'
          Width = 150
        end
        item
          Alignment = taCenter
          Caption = 'Poloha'
        end>
      GridLines = True
      MultiSelect = True
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnChange = LV_VyhybkyChange
      OnKeyDown = LV_VyhybkyKeyDown
    end
    object CHB_NewZaver: TGroupBox
      Left = 2
      Top = 188
      Width = 261
      Height = 71
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alBottom
      Caption = ' P'#345'idat/upravit v'#253'hybku '
      TabOrder = 2
      object Label10: TLabel
        Left = 8
        Top = 16
        Width = 27
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Blok :'
      end
      object Label11: TLabel
        Left = 8
        Top = 40
        Width = 39
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Poloha :'
      end
      object CB_NewZaverBlok: TComboBox
        Left = 56
        Top = 16
        Width = 145
        Height = 21
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
      end
      object CB_NewZaverPoloha: TComboBox
        Left = 56
        Top = 40
        Width = 145
        Height = 21
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 1
        Text = '+'
        Items.Strings = (
          '+'
          '-')
      end
      object B_Vyh_Add: TButton
        Left = 205
        Top = 16
        Width = 49
        Height = 45
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Ok'
        TabOrder = 2
        OnClick = B_Vyh_AddClick
      end
    end
    object B_Vyh_Del: TButton
      Left = 80
      Top = 17
      Width = 105
      Height = 25
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Smazat v'#253'hybku'
      Enabled = False
      TabOrder = 1
      OnClick = B_Vyh_DelClick
    end
  end
  object GB_Useky: TGroupBox
    Left = 7
    Top = 122
    Width = 329
    Height = 243
    Hint = 'Zadavejte useky od navestidla smerem k poslednimu bloku (vcetne)'
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' '#218'seky '
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
    object CHB_NewBlok: TGroupBox
      Left = 2
      Top = 192
      Width = 325
      Height = 49
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alBottom
      Caption = ' P'#345'idat/upravit '#250'sek '
      TabOrder = 2
      object CB_NewUsek: TComboBox
        Left = 10
        Top = 16
        Width = 250
        Height = 21
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
      end
      object B_Usek_Add: TButton
        Left = 264
        Top = 16
        Width = 53
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Ok'
        TabOrder = 1
        OnClick = B_Usek_AddClick
      end
    end
    object LV_Useky: TListView
      Left = 2
      Top = 53
      Width = 325
      Height = 139
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alBottom
      Columns = <
        item
          Caption = '#'
          Width = 20
        end
        item
          Caption = #218'sek'
          Width = 250
        end>
      GridLines = True
      MultiSelect = True
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnChange = LV_UsekyChange
      OnKeyDown = LV_UsekyKeyDown
    end
    object B_Usek_Del: TButton
      Left = 112
      Top = 18
      Width = 105
      Height = 25
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Smazat '#250'sek'
      Enabled = False
      TabOrder = 1
      OnClick = B_Usek_DelClick
    end
  end
  object B_Save: TButton
    Left = 533
    Top = 440
    Width = 75
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Pou'#382#237't'
    Default = True
    TabOrder = 12
    OnClick = B_SaveClick
  end
  object B_Storno: TButton
    Left = 454
    Top = 440
    Width = 75
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Storno'
    TabOrder = 13
    OnClick = B_StornoClick
  end
  object CB_Navestidlo: TComboBox
    Left = 136
    Top = 65
    Width = 201
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
    OnChange = CB_NavestidloChange
  end
  object CB_Typ: TComboBox
    Left = 136
    Top = 97
    Width = 201
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 3
    OnChange = CB_TypChange
    Items.Strings = (
      'Vlakov'#225' cesta'
      'Posunov'#225' cesta')
  end
  object CB_Dalsi_Nav: TComboBox
    Left = 136
    Top = 377
    Width = 201
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 5
    OnChange = CB_Dalsi_NavChange
  end
  object CB_Rychlost_Volno: TComboBox
    Left = 191
    Top = 439
    Width = 145
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 7
    Items.Strings = (
      '0 km/h'
      '10 km/h'
      '20 km/h'
      '30 km/h'
      '40 km/h'
      '50 km/h'
      '60 km/h'
      '70 km/h'
      '80 km/h'
      '90 km/h'
      '100 km/h')
  end
  object CB_Rychlost_Stuj: TComboBox
    Left = 191
    Top = 407
    Width = 145
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 6
    Items.Strings = (
      '0 km/h'
      '10 km/h'
      '20 km/h'
      '30 km/h'
      '40 km/h'
      '50 km/h'
      '60 km/h'
      '70 km/h'
      '80 km/h'
      '90 km/h'
      '100 km/h')
  end
  object CHB_AutoName: TCheckBox
    Left = 88
    Top = 8
    Width = 43
    Height = 17
    Caption = 'auto'
    Checked = True
    State = cbChecked
    TabOrder = 15
  end
  object GB_trat: TGroupBox
    Left = 345
    Top = 273
    Width = 262
    Height = 105
    Caption = ' N'#225'vaznost na tra'#357' '
    TabOrder = 9
    object Label1: TLabel
      Left = 24
      Top = 40
      Width = 23
      Height = 13
      Caption = 'Tra'#357':'
    end
    object Label2: TLabel
      Left = 24
      Top = 64
      Width = 27
      Height = 13
      Caption = 'Sm'#283'r:'
    end
    object CHB_Trat: TCheckBox
      Left = 16
      Top = 16
      Width = 57
      Height = 17
      Caption = 'Povolit'
      TabOrder = 0
      OnClick = CHB_TratClick
    end
    object CB_TratBlok: TComboBox
      Left = 64
      Top = 39
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
      OnChange = CB_TratBlokChange
    end
    object CB_TratSmer: TComboBox
      Left = 64
      Top = 66
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 2
      Items.Strings = (
        'A --> B'
        'B --> A')
    end
  end
  object SE_ID: TSpinEdit
    Left = 136
    Top = 36
    Width = 201
    Height = 22
    MaxLength = 65536
    MaxValue = 0
    MinValue = 0
    TabOrder = 1
    Value = 0
  end
  object GB_Advanced: TGroupBox
    Left = 617
    Top = 8
    Width = 297
    Height = 389
    Caption = ' Pokro'#269'il'#225' nastaven'#237' (n'#225'pov'#283'da po najet'#237' kurzorem) '
    TabOrder = 14
    object Label4: TLabel
      Left = 8
      Top = 16
      Width = 44
      Height = 13
      Caption = 'P'#345'ejezdy:'
    end
    object Label5: TLabel
      Left = 8
      Top = 112
      Width = 40
      Height = 13
      Caption = 'Odvraty:'
    end
    object Label6: TLabel
      Left = 8
      Top = 207
      Width = 147
      Height = 13
      Caption = 'Bloky, na nich'#382' zav'#233'st redukci:'
    end
    object Label7: TLabel
      Left = 8
      Top = 268
      Width = 35
      Height = 13
      Caption = 'Z'#225'mky:'
    end
    object Label8: TLabel
      Left = 8
      Top = 332
      Width = 71
      Height = 13
      Caption = 'Variantn'#237' body:'
    end
    object M_Prj: TMemo
      Left = 8
      Top = 35
      Width = 281
      Height = 67
      Hint = 
        'Form'#225't: na ka'#382'd'#253' '#345#225'dek jeden p'#345'ejezd: id bloku p'#345'ejezdu, id otev' +
        #237'rac'#237'ho bloku, id 1. uz. bloku., id 2. uz. bloku, ...'
      Lines.Strings = (
        'M_Prj')
      ParentShowHint = False
      ScrollBars = ssVertical
      ShowHint = True
      TabOrder = 0
    end
    object M_Odvraty: TMemo
      Left = 8
      Top = 131
      Width = 281
      Height = 67
      Hint = 
        'Form'#225't: na ka'#382'd'#253' '#345#225'dek jeden odvrat: id bloku odvratu, poloha (+' +
        ',-), id referen'#269'n'#237'ho bloku'
      Lines.Strings = (
        'M_Odvraty')
      ParentShowHint = False
      ScrollBars = ssVertical
      ShowHint = True
      TabOrder = 1
    end
    object E_VB: TEdit
      Left = 8
      Top = 351
      Width = 281
      Height = 21
      Hint = 'Form'#225't: id blok'#367' odd'#283'len'#253'ch '#269#225'rkami'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      Text = 'E_VB'
    end
    object M_Redukce: TMemo
      Left = 8
      Top = 224
      Width = 281
      Height = 36
      Hint = 
        'Form'#225't: na ka'#382'd'#233'm '#345#225'dku jeden blok: id bloku, id referen'#269'n'#237'ho bl' +
        'oku'
      Lines.Strings = (
        'M_Redukce')
      ParentShowHint = False
      ScrollBars = ssVertical
      ShowHint = True
      TabOrder = 3
    end
    object M_Zamky: TMemo
      Left = 8
      Top = 288
      Width = 281
      Height = 36
      Hint = 
        'Form'#225't: na ka'#382'd'#233'm '#345#225'dku jeden blok: id bloku, id referen'#269'n'#237'ho bl' +
        'oku'
      Lines.Strings = (
        'M_Zamky')
      ParentShowHint = False
      ScrollBars = ssVertical
      ShowHint = True
      TabOrder = 4
    end
  end
  object CHB_Advanced: TCheckBox
    Left = 349
    Top = 413
    Width = 164
    Height = 18
    Caption = 'Zobrazit pokro'#269'il'#225' nastaven'#237
    TabOrder = 11
    OnClick = CHB_AdvancedClick
  end
  object CHB_Odbocka: TCheckBox
    Left = 349
    Top = 391
    Width = 97
    Height = 17
    Caption = 'JC do odbo'#269'ky'
    TabOrder = 10
  end
end
