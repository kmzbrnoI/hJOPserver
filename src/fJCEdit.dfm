object F_JCEdit: TF_JCEdit
  Left = 505
  Top = 156
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Editace vlakove cesty [nazev]'
  ClientHeight = 450
  ClientWidth = 617
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
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object L_VC_02: TLabel
    Left = 9
    Top = 39
    Width = 56
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'N'#225'v'#283'stidlo :'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object L_VC_11: TLabel
    Left = 9
    Top = 71
    Width = 52
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Typ cesty :'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object L_VC_07: TLabel
    Left = 9
    Top = 103
    Width = 81
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Dal'#353#237' n'#225'v'#283'stidlo :'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object L_VC_10: TLabel
    Left = 8
    Top = 133
    Width = 157
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Rychlost v ceste (dal'#353#237' N = Stuj) :'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object L_VC_12: TLabel
    Left = 8
    Top = 165
    Width = 166
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Rychlost v ceste (dal'#353#237' N = Volno) :'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object E_VCNazev: TEdit
    Left = 136
    Top = 8
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
    Height = 281
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' Z'#225'v'#283'ry v'#253'hybek '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 7
    object LV_Zavery: TListView
      Left = 2
      Top = 48
      Width = 261
      Height = 134
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alBottom
      Columns = <
        item
          Caption = 'Blok'
          Width = 160
        end
        item
          Alignment = taCenter
          Caption = 'Poloha'
          Width = 75
        end>
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      GridLines = True
      ReadOnly = True
      RowSelect = True
      ParentFont = False
      TabOrder = 0
      ViewStyle = vsReport
      OnChange = LV_ZaveryChange
    end
    object CHB_NewZaver: TGroupBox
      Left = 2
      Top = 182
      Width = 261
      Height = 97
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alBottom
      Caption = ' Z'#225'v'#283'r nov'#233' vyh'#253'bky '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      object L_SCom11: TLabel
        Left = 8
        Top = 16
        Width = 27
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Blok :'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object L_SCom12: TLabel
        Left = 8
        Top = 40
        Width = 39
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Poloha :'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
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
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ItemHeight = 0
        ParentFont = False
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
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ItemHeight = 13
        ItemIndex = 0
        ParentFont = False
        TabOrder = 1
        Text = '+'
        Items.Strings = (
          '+'
          '-')
      end
      object B_NewZaverAdd: TButton
        Left = 56
        Top = 64
        Width = 147
        Height = 25
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Pridat nov'#253' z'#225'v'#283'r v'#253'hybky'
        TabOrder = 2
        OnClick = B_NewZaverAddClick
      end
    end
    object B_ZaveryVyhybek_Delete: TButton
      Left = 80
      Top = 16
      Width = 105
      Height = 25
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Smazat z'#225'ver'
      Enabled = False
      TabOrder = 1
      OnClick = B_ZaveryVyhybek_DeleteClick
    end
  end
  object GB_Useky: TGroupBox
    Left = 9
    Top = 190
    Width = 329
    Height = 250
    Hint = 'Zadavejte useky od navestidla smerem k poslednimu bloku (vcetne)'
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' Z'#225'v'#283'ry '#250'sek'#367' '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 6
    object CHB_NewBlok: TGroupBox
      Left = 2
      Top = 195
      Width = 325
      Height = 53
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alBottom
      Caption = ' Nov'#253' '#250'sek '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      object L_SCom14: TLabel
        Left = 8
        Top = 16
        Width = 27
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Blok :'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object CB_NewUsek: TComboBox
        Left = 56
        Top = 16
        Width = 145
        Height = 21
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Style = csDropDownList
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ItemHeight = 0
        ParentFont = False
        TabOrder = 0
      end
      object B_NewUsek: TButton
        Left = 210
        Top = 16
        Width = 107
        Height = 25
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'P'#345'idat nov'#253' '#250'sek'
        TabOrder = 1
        OnClick = B_NewUsekClick
      end
    end
    object LV_Useky: TListView
      Left = 2
      Top = 56
      Width = 325
      Height = 139
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alBottom
      Columns = <
        item
          Caption = 'Blok'
          Width = 250
        end>
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      GridLines = True
      ReadOnly = True
      RowSelect = True
      ParentFont = False
      TabOrder = 0
      ViewStyle = vsReport
      OnChange = LV_UsekyChange
    end
    object B_ZaveryUseku_Delete: TButton
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
      OnClick = B_ZaveryUseku_DeleteClick
    end
  end
  object B_Save: TButton
    Left = 537
    Top = 415
    Width = 75
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Pou'#382#237't'
    TabOrder = 9
    OnClick = B_SaveClick
  end
  object B_Storno: TButton
    Left = 456
    Top = 415
    Width = 75
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Storno'
    TabOrder = 10
    OnClick = B_StornoClick
  end
  object CB_Navestidlo: TComboBox
    Left = 137
    Top = 39
    Width = 201
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Style = csDropDownList
    ItemHeight = 0
    TabOrder = 1
    OnChange = CB_NavestidloChange
  end
  object CB_TypCesty: TComboBox
    Left = 137
    Top = 71
    Width = 201
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
    OnChange = CB_TypCestyChange
    Items.Strings = (
      'Vlakova cesta'
      'Posunova cesta')
  end
  object CB_DalsiNNavaznost: TComboBox
    Left = 137
    Top = 103
    Width = 201
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Style = csDropDownList
    ItemHeight = 0
    TabOrder = 3
  end
  object CB_Rychlost_DalsiN: TComboBox
    Left = 192
    Top = 165
    Width = 145
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 5
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
  object CB_Rychlost_NoDalsiN: TComboBox
    Left = 192
    Top = 133
    Width = 145
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 4
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
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    State = cbChecked
    TabOrder = 11
  end
  object GB_trat: TGroupBox
    Left = 347
    Top = 291
    Width = 262
    Height = 105
    Caption = ' N'#225'vaznost na tra'#357' '
    TabOrder = 8
    object Label1: TLabel
      Left = 24
      Top = 40
      Width = 24
      Height = 13
      Caption = 'Blok:'
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
      ItemHeight = 0
      TabOrder = 1
    end
    object CB_TratSmer: TComboBox
      Left = 64
      Top = 64
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
end