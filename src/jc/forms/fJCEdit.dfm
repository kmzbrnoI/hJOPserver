object F_JCEdit: TF_JCEdit
  Left = 505
  Top = 156
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Editace vlakove cesty [nazev]'
  ClientHeight = 577
  ClientWidth = 976
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
    Width = 53
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'N'#225'v'#283'stidlo:'
  end
  object L_VC_11: TLabel
    Left = 8
    Top = 97
    Width = 49
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Typ cesty:'
  end
  object L_VC_07: TLabel
    Left = 8
    Top = 410
    Width = 81
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Dal'#353#237' n'#225'v'#283'stidlo :'
  end
  object Label3: TLabel
    Left = 8
    Top = 34
    Width = 14
    Height = 13
    Caption = 'ID:'
  end
  object Label9: TLabel
    Left = 9
    Top = 129
    Width = 37
    Height = 13
    Caption = 'N'#225'v'#283'st:'
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
  object GB_Turnouts: TGroupBox
    Left = 345
    Top = 7
    Width = 320
    Height = 261
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' V'#253'hybky '
    TabOrder = 11
    object LV_Turnouts: TListView
      Left = 2
      Top = 15
      Width = 316
      Height = 173
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alClient
      Columns = <
        item
          Caption = '#'
          Width = 20
        end
        item
          Caption = 'ID'
        end
        item
          Caption = 'N'#225'zev'
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
      OnChange = LV_TurnoutsChange
      OnKeyDown = LV_TurnoutsKeyDown
    end
    object GB_Turnout_New: TGroupBox
      Left = 2
      Top = 188
      Width = 316
      Height = 71
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alBottom
      Caption = ' P'#345'idat/upravit v'#253'hybku '
      TabOrder = 1
      object Label10: TLabel
        Left = 8
        Top = 16
        Width = 24
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Blok:'
      end
      object Label11: TLabel
        Left = 8
        Top = 40
        Width = 36
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Poloha:'
      end
      object CB_Turnout: TComboBox
        Left = 56
        Top = 16
        Width = 185
        Height = 21
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Style = csDropDownList
        TabOrder = 0
      end
      object CB_Turnout_Pos: TComboBox
        Left = 56
        Top = 40
        Width = 185
        Height = 21
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 1
        Text = '+'
        Items.Strings = (
          '+'
          '-')
      end
      object B_Turnout_Ok: TButton
        Left = 253
        Top = 40
        Width = 49
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Ok'
        TabOrder = 2
        OnClick = B_Turnout_OkClick
      end
      object B_Turnout_Del: TButton
        Left = 253
        Top = 15
        Width = 49
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Smazat'
        Enabled = False
        TabOrder = 3
        OnClick = B_Turnout_DelClick
      end
    end
  end
  object GB_Tracks: TGroupBox
    Left = 7
    Top = 155
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
    TabOrder = 5
    object GB_Track_New: TGroupBox
      Left = 2
      Top = 192
      Width = 325
      Height = 49
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alBottom
      Caption = ' P'#345'idat/upravit/odstranit '#250'sek '
      TabOrder = 1
      object CB_Track: TComboBox
        Left = 10
        Top = 16
        Width = 183
        Height = 21
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Style = csDropDownList
        TabOrder = 0
      end
      object B_Track_Ok: TButton
        Left = 197
        Top = 16
        Width = 53
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Ok'
        TabOrder = 1
        OnClick = B_Track_OkClick
      end
      object B_Track_Del: TButton
        Left = 254
        Top = 16
        Width = 57
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Smazat'
        Enabled = False
        TabOrder = 2
        OnClick = B_Track_DelClick
      end
    end
    object LV_Tracks: TListView
      Left = 2
      Top = 15
      Width = 325
      Height = 177
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alClient
      Columns = <
        item
          Caption = '#'
          Width = 20
        end
        item
          Caption = 'ID'
        end
        item
          Caption = 'N'#225'zev'
          Width = 210
        end>
      GridLines = True
      MultiSelect = True
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnChange = LV_TracksChange
      OnKeyDown = LV_TracksKeyDown
    end
  end
  object B_Save: TButton
    Left = 590
    Top = 544
    Width = 75
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Pou'#382#237't'
    Default = True
    TabOrder = 13
    OnClick = B_SaveClick
  end
  object B_Storno: TButton
    Left = 511
    Top = 544
    Width = 75
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Storno'
    TabOrder = 14
    OnClick = B_StornoClick
  end
  object CB_Signal: TComboBox
    Left = 136
    Top = 65
    Width = 201
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Style = csDropDownList
    TabOrder = 2
    OnChange = CB_SignalChange
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
    TabOrder = 3
    OnChange = CB_TypChange
    Items.Strings = (
      'Vlakov'#225' cesta'
      'Posunov'#225' cesta')
  end
  object CB_Next_Signal: TComboBox
    Left = 136
    Top = 410
    Width = 201
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Style = csDropDownList
    TabOrder = 6
    OnChange = CB_Next_SignalChange
  end
  object CHB_AutoName: TCheckBox
    Left = 88
    Top = 8
    Width = 43
    Height = 17
    Caption = 'auto'
    Checked = True
    State = cbChecked
    TabOrder = 16
  end
  object GB_Railway: TGroupBox
    Left = 8
    Top = 436
    Width = 328
    Height = 76
    Caption = ' N'#225'vaznost na tra'#357' '
    TabOrder = 7
    object Label1: TLabel
      Left = 111
      Top = 16
      Width = 23
      Height = 13
      Caption = 'Tra'#357':'
    end
    object Label2: TLabel
      Left = 111
      Top = 40
      Width = 27
      Height = 13
      Caption = 'Sm'#283'r:'
    end
    object CHB_Railway: TCheckBox
      Left = 16
      Top = 16
      Width = 57
      Height = 17
      Caption = 'Povolit'
      TabOrder = 0
      OnClick = CHB_RailwayClick
    end
    object CB_Railway: TComboBox
      Left = 151
      Top = 15
      Width = 161
      Height = 21
      Style = csDropDownList
      TabOrder = 1
      OnChange = CB_RailwayChange
    end
    object CB_Railway_Dir: TComboBox
      Left = 151
      Top = 42
      Width = 161
      Height = 21
      Style = csDropDownList
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
    Left = 670
    Top = 8
    Width = 297
    Height = 390
    Caption = ' Pokro'#269'il'#225' nastaven'#237' (n'#225'pov'#283'da po najet'#237' kurzorem) '
    TabOrder = 15
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
    object Label7: TLabel
      Left = 8
      Top = 208
      Width = 35
      Height = 13
      Caption = 'Z'#225'mky:'
    end
    object Label8: TLabel
      Left = 8
      Top = 301
      Width = 71
      Height = 13
      Caption = 'Variantn'#237' body:'
    end
    object Label6: TLabel
      Left = 8
      Top = 352
      Width = 193
      Height = 13
      Caption = 'Zru'#353'it n'#225'v'#283'st p'#345'i obsazen'#237' '#250'seku [0..n-1]:'
    end
    object M_Crossings: TMemo
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
      ScrollBars = ssBoth
      ShowHint = True
      TabOrder = 0
    end
    object M_Refugees: TMemo
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
      Top = 320
      Width = 281
      Height = 21
      Hint = 'Form'#225't: id blok'#367' odd'#283'len'#253'ch '#269#225'rkami'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      Text = 'E_VB'
    end
    object M_Locks: TMemo
      Left = 8
      Top = 227
      Width = 281
      Height = 62
      Hint = 
        'Form'#225't: na ka'#382'd'#233'm '#345#225'dku jeden blok: id bloku, id referen'#269'n'#237'ho bl' +
        'oku'
      Lines.Strings = (
        'M_Zamky')
      ParentShowHint = False
      ScrollBars = ssVertical
      ShowHint = True
      TabOrder = 2
    end
    object SE_SignalFallTrackI: TSpinEdit
      Left = 208
      Top = 352
      Width = 81
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 4
      Value = 0
    end
  end
  object CHB_Advanced: TCheckBox
    Left = 8
    Top = 554
    Width = 164
    Height = 18
    Caption = 'Zobrazit pokro'#269'il'#225' nastaven'#237
    TabOrder = 10
    OnClick = CHB_AdvancedClick
  end
  object CHB_Odbocka: TCheckBox
    Left = 8
    Top = 520
    Width = 97
    Height = 17
    Caption = 'JC do odbo'#269'ky'
    TabOrder = 8
  end
  object CHB_NZV: TCheckBox
    Left = 8
    Top = 537
    Width = 185
    Height = 17
    Caption = 'Nedostate'#269'n'#225' z'#225'brzdn'#225' vzd'#225'lenost'
    TabOrder = 9
  end
  object CB_Signal_Signal: TComboBox
    Left = 136
    Top = 129
    Width = 201
    Height = 21
    Style = csDropDownList
    TabOrder = 4
  end
  object GB_Speeds: TGroupBox
    Left = 346
    Top = 271
    Width = 318
    Height = 268
    Caption = ' Rychlosti '
    TabOrder = 12
    object GB_SpeedsStop: TGroupBox
      Left = 2
      Top = 15
      Width = 314
      Height = 122
      Align = alTop
      Caption = ' Rychlosti p'#345'i dal'#353#237'm n'#225'v'#283'stidle na st'#367'j '
      TabOrder = 0
    end
    object GB_SpeedsGo: TGroupBox
      Left = 2
      Top = 137
      Width = 314
      Height = 129
      Align = alClient
      Caption = ' Rychlosti p'#345'i dal'#353#237'm n'#225'v'#283'stidle povoluj'#237'c'#237'm j'#237'zdu  '
      TabOrder = 1
    end
  end
end
