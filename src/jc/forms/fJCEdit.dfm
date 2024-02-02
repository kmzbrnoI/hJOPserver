object F_JCEdit: TF_JCEdit
  Left = 505
  Top = 156
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'J'#237'zdn'#237' cesta ...'
  ClientHeight = 837
  ClientWidth = 1057
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
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
    Top = 66
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
  object GB_Turnouts: TGroupBox
    Left = 346
    Top = 325
    Width = 329
    Height = 300
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' V'#253'hybky '
    TabOrder = 8
    object LV_Turnouts: TListView
      Left = 2
      Top = 15
      Width = 325
      Height = 212
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alClient
      Columns = <
        item
          Caption = '#'
          Width = 30
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
      Top = 227
      Width = 325
      Height = 71
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alBottom
      Caption = ' P'#345'idat/upravit/smazat v'#253'hybku '
      TabOrder = 1
      object Label10: TLabel
        Left = 8
        Top = 16
        Width = 44
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'V'#253'hybka:'
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
        Width = 201
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
        Width = 201
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
        Left = 269
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
        Left = 269
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
    Left = 346
    Top = 7
    Width = 329
    Height = 314
    Hint = 'Zadavejte useky od navestidla smerem k poslednimu bloku (vcetne)'
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' '#218'seky '
    ParentShowHint = False
    ShowHint = True
    TabOrder = 7
    object GB_Track_New: TGroupBox
      Left = 2
      Top = 239
      Width = 325
      Height = 73
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
        Width = 239
        Height = 21
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Style = csDropDownList
        TabOrder = 0
      end
      object B_Track_Ok: TButton
        Left = 261
        Top = 43
        Width = 57
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
        Left = 261
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
      object CHB_Variant_Point: TCheckBox
        Left = 10
        Top = 42
        Width = 95
        Height = 17
        Caption = 'Variantn'#237' bod'
        TabOrder = 3
      end
    end
    object LV_Tracks: TListView
      Left = 2
      Top = 15
      Width = 325
      Height = 224
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alClient
      Columns = <
        item
          Caption = '#'
          Width = 30
        end
        item
          Caption = 'ID'
        end
        item
          Caption = 'N'#225'zev'
          Width = 180
        end
        item
          Alignment = taCenter
          Caption = 'VB'
          Width = 30
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
    Left = 975
    Top = 806
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
    Left = 896
    Top = 806
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
  object CB_Signal: TComboBox
    Left = 136
    Top = 66
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
  object CHB_AutoName: TCheckBox
    Left = 88
    Top = 8
    Width = 43
    Height = 17
    Caption = 'auto'
    Checked = True
    State = cbChecked
    TabOrder = 14
  end
  object GB_Railway: TGroupBox
    Left = 8
    Top = 553
    Width = 329
    Height = 72
    Caption = ' N'#225'vaznost na tra'#357' '
    TabOrder = 6
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
  object GB_Speeds: TGroupBox
    Left = 8
    Top = 279
    Width = 329
    Height = 268
    Caption = ' Rychlosti '
    TabOrder = 5
    object GB_SpeedsStop: TGroupBox
      Left = 2
      Top = 15
      Width = 325
      Height = 122
      Align = alTop
      Caption = ' Rychlosti p'#345'i dal'#353#237'm n'#225'v'#283'stidle na st'#367'j '
      TabOrder = 0
    end
    object GB_SpeedsGo: TGroupBox
      Left = 2
      Top = 137
      Width = 325
      Height = 129
      Align = alClient
      Caption = ' Rychlosti p'#345'i dal'#353#237'm n'#225'v'#283'stidle povoluj'#237'c'#237'm j'#237'zdu  '
      TabOrder = 1
    end
  end
  object GB_Signal: TGroupBox
    Left = 8
    Top = 123
    Width = 329
    Height = 150
    Caption = ' N'#225'v'#283#353't'#283'n'#237' '
    TabOrder = 4
    object Label9: TLabel
      Left = 12
      Top = 20
      Width = 37
      Height = 13
      Caption = 'N'#225'v'#283'st:'
    end
    object L_VC_07: TLabel
      Left = 12
      Top = 93
      Width = 81
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Dal'#353#237' n'#225'v'#283'stidlo :'
    end
    object Label6: TLabel
      Left = 13
      Top = 119
      Width = 83
      Height = 13
      Caption = 'P'#225'd p'#345'i obsazen'#237':'
    end
    object CB_Signal_Signal: TComboBox
      Left = 129
      Top = 20
      Width = 193
      Height = 21
      Style = csDropDownList
      TabOrder = 0
    end
    object CHB_Odbocka: TCheckBox
      Left = 12
      Top = 47
      Width = 213
      Height = 17
      Caption = 'Sn'#237#382'en'#225' rychlost (odbo'#269'ka) - doln'#237' '#382'lut'#225
      TabOrder = 1
    end
    object CHB_NZV: TCheckBox
      Left = 12
      Top = 66
      Width = 194
      Height = 17
      Caption = 'Nedostate'#269'n'#225' z'#225'brzdn'#225' vzd'#225'lenost'
      TabOrder = 2
    end
    object CB_Next_Signal: TComboBox
      Left = 129
      Top = 93
      Width = 193
      Height = 21
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Style = csDropDownList
      TabOrder = 3
      OnChange = CB_Next_SignalChange
    end
    object CB_Signal_Fall: TComboBox
      Left = 129
      Top = 119
      Width = 193
      Height = 21
      Style = csDropDownList
      TabOrder = 4
    end
  end
  object GB_Locks: TGroupBox
    Left = 685
    Top = 7
    Width = 365
    Height = 188
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' Z'#225'mky '
    TabOrder = 9
    object LV_Locks: TListView
      Left = 2
      Top = 15
      Width = 361
      Height = 100
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
          Width = 100
        end
        item
          Alignment = taCenter
          Caption = 'Ref. ID'
        end
        item
          Caption = 'Referen'#269'n'#237' '#250'sek'
          Width = 100
        end>
      GridLines = True
      MultiSelect = True
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnChange = LV_LocksChange
      OnKeyDown = LV_LocksKeyDown
    end
    object GB_Lock: TGroupBox
      Left = 2
      Top = 115
      Width = 361
      Height = 71
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alBottom
      Caption = ' P'#345'idat/upravit/smazat z'#225'mek '
      TabOrder = 1
      object Label12: TLabel
        Left = 8
        Top = 16
        Width = 36
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Z'#225'mek:'
      end
      object Label13: TLabel
        Left = 8
        Top = 40
        Width = 82
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Referen'#269'n'#237' '#250'sek:'
      end
      object CB_Lock: TComboBox
        Left = 109
        Top = 15
        Width = 180
        Height = 21
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Style = csDropDownList
        TabOrder = 0
      end
      object CB_Lock_Ref: TComboBox
        Left = 109
        Top = 40
        Width = 180
        Height = 21
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Style = csDropDownList
        TabOrder = 1
      end
      object B_Lock_Ok: TButton
        Left = 301
        Top = 40
        Width = 49
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Ok'
        TabOrder = 2
        OnClick = B_Lock_OkClick
      end
      object B_Lock_Del: TButton
        Left = 301
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
        OnClick = B_Lock_DelClick
      end
    end
  end
  object GB_Refugees: TGroupBox
    Left = 685
    Top = 199
    Width = 365
    Height = 261
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' Odvraty '
    TabOrder = 10
    object LV_Refugees: TListView
      Left = 2
      Top = 15
      Width = 361
      Height = 148
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
          Width = 40
        end
        item
          Caption = 'N'#225'zev'
          Width = 80
        end
        item
          Caption = 'Poloha'
        end
        item
          Alignment = taCenter
          Caption = 'Ref. ID'
        end
        item
          Caption = 'Referen'#269'n'#237' '#250'sek'
          Width = 80
        end>
      GridLines = True
      MultiSelect = True
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnChange = LV_RefugeesChange
      OnKeyDown = LV_RefugeesKeyDown
    end
    object GB_Refugee: TGroupBox
      Left = 2
      Top = 163
      Width = 361
      Height = 96
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alBottom
      Caption = ' P'#345'idat/upravit/smazat odvrat '
      TabOrder = 1
      object Label7: TLabel
        Left = 8
        Top = 16
        Width = 35
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Odvrat:'
      end
      object Label14: TLabel
        Left = 8
        Top = 64
        Width = 82
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Referen'#269'n'#237' '#250'sek:'
      end
      object Label15: TLabel
        Left = 9
        Top = 40
        Width = 36
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Poloha:'
      end
      object CB_Refugee: TComboBox
        Left = 109
        Top = 15
        Width = 180
        Height = 21
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Style = csDropDownList
        TabOrder = 0
      end
      object CB_Refugee_Ref: TComboBox
        Left = 109
        Top = 64
        Width = 180
        Height = 21
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Style = csDropDownList
        TabOrder = 2
      end
      object B_Refugee_Ok: TButton
        Left = 301
        Top = 64
        Width = 49
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Ok'
        TabOrder = 3
        OnClick = B_Refugee_OkClick
      end
      object B_Refugee_Del: TButton
        Left = 301
        Top = 39
        Width = 49
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Smazat'
        Enabled = False
        TabOrder = 4
        OnClick = B_Refugee_DelClick
      end
      object CB_Refugee_Pos: TComboBox
        Left = 109
        Top = 40
        Width = 180
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
    end
  end
  object GB_Crossings: TGroupBox
    Left = 7
    Top = 627
    Width = 668
    Height = 202
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' P'#345'ejezdy '
    TabOrder = 11
    object LV_Crossings: TListView
      Left = 2
      Top = 15
      Width = 664
      Height = 94
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
          Width = 90
        end
        item
          Caption = 'Ref. ID'
        end
        item
          Caption = 'Refere'#269'n'#237' '#250'sek'
          Width = 90
        end
        item
          Caption = 'Zav'#237'rac'#237' ID'
          Width = 130
        end
        item
          Caption = 'Zav'#237'rac'#237' '#250'seky'
          Width = 200
        end>
      GridLines = True
      MultiSelect = True
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnChange = LV_CrossingsChange
      OnKeyDown = LV_CrossingsKeyDown
    end
    object GB_Crossing: TGroupBox
      Left = 2
      Top = 109
      Width = 664
      Height = 91
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alBottom
      Caption = ' P'#345'idat/upravit/smazat p'#345'ejezd '
      TabOrder = 1
      object Label5: TLabel
        Left = 8
        Top = 16
        Width = 39
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'P'#345'ejezd:'
      end
      object Label16: TLabel
        Left = 9
        Top = 63
        Width = 82
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Referen'#269'n'#237' '#250'sek:'
      end
      object Label8: TLabel
        Left = 254
        Top = 16
        Width = 99
        Height = 13
        Caption = 'ID zav'#237'rac'#237'ch '#250'sek'#367':'
      end
      object Label17: TLabel
        Left = 254
        Top = 43
        Width = 235
        Height = 13
        Caption = 'Jm'#233'na zav'#237'rac'#237'ch '#250'sek'#367' (pr'#225'zdn'#233' = zav'#237'rat v'#382'dy):'
      end
      object CB_Crossing: TComboBox
        Left = 109
        Top = 16
        Width = 124
        Height = 21
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Style = csDropDownList
        TabOrder = 0
      end
      object CB_Crossing_Ref: TComboBox
        Left = 109
        Top = 63
        Width = 124
        Height = 21
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Style = csDropDownList
        TabOrder = 2
      end
      object B_Crossing_Ok: TButton
        Left = 608
        Top = 61
        Width = 49
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Ok'
        TabOrder = 6
        OnClick = B_Crossing_OkClick
      end
      object B_Crossing_Del: TButton
        Left = 608
        Top = 35
        Width = 49
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Smazat'
        Enabled = False
        TabOrder = 7
        OnClick = B_Crossing_DelClick
      end
      object CHB_Crossing_Closing: TCheckBox
        Left = 9
        Top = 41
        Width = 168
        Height = 17
        Caption = 'P'#345'ejezd zav'#237'ran'#253' j'#237'zdn'#237' cestou'
        TabOrder = 1
        OnClick = CHB_Crossing_ClosingClick
      end
      object E_Crossing_Close_Ids: TEdit
        Left = 359
        Top = 16
        Width = 244
        Height = 21
        TabOrder = 3
        Text = 'E_Crossing_Close_Ids'
      end
      object E_Crossing_Close_Names: TEdit
        Left = 254
        Top = 62
        Width = 349
        Height = 21
        TabOrder = 5
        Text = 'E_Crossing_Close_Names'
      end
      object B_Cros_Names_To_Ids: TButton
        Left = 528
        Top = 40
        Width = 75
        Height = 20
        Caption = 'Jm'#233'na > ID'
        TabOrder = 4
        OnClick = B_Cros_Names_To_IdsClick
      end
    end
  end
end
