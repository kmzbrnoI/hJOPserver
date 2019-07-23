object F_BlkTU: TF_BlkTU
  Left = 377
  Top = 212
  ActiveControl = B_OK
  BorderStyle = bsToolWindow
  Caption = 'Editovat data bloku : [blok] (tratovy usek)'
  ClientHeight = 489
  ClientWidth = 697
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
  object L_Usek02: TLabel
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
  object L_Usek01: TLabel
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
  object L_Usek15: TLabel
    Left = 8
    Top = 128
    Width = 89
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'D'#233'lka '#250'seku (cm) :'
  end
  object Label1: TLabel
    Left = 8
    Top = 157
    Width = 49
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Zesilova'#269':'
  end
  object L_Usek33: TLabel
    Left = 8
    Top = 182
    Width = 78
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Smy'#269'kov'#253' blok :'
  end
  object L_Trat3: TLabel
    Left = 8
    Top = 207
    Width = 74
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Rychlost v T'#218' :'
  end
  object B_OK: TButton
    Left = 614
    Top = 452
    Width = 75
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Pou'#382#237't'
    Default = True
    TabOrder = 6
    OnClick = B_OKClick
  end
  object B_Storno: TButton
    Left = 535
    Top = 452
    Width = 75
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Storno'
    TabOrder = 7
    OnClick = B_StornoClick
  end
  object SE_ID: TSpinEdit
    Left = 119
    Top = 40
    Width = 193
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
  object E_Nazev: TEdit
    Left = 119
    Top = 7
    Width = 192
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    MaxLength = 32
    TabOrder = 0
  end
  object GB_RCS: TGroupBox
    Left = 7
    Top = 237
    Width = 305
    Height = 129
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' Technologick'#233' vstupy - RCS  '
    TabOrder = 5
    object L_Usek04: TLabel
      Left = 12
      Top = 19
      Width = 54
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = '1. detektor:'
    end
    object Label2: TLabel
      Left = 12
      Top = 45
      Width = 54
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = '2. detektor:'
    end
    object Label3: TLabel
      Left = 12
      Top = 71
      Width = 54
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = '3. detektor:'
    end
    object Label4: TLabel
      Left = 12
      Top = 97
      Width = 54
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = '4. detektor:'
    end
    object SE_Port1: TSpinEdit
      Left = 224
      Top = 19
      Width = 73
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Enabled = False
      MaxValue = 0
      MinValue = 0
      TabOrder = 2
      Value = 0
    end
    object SE_Board1: TSpinEdit
      Left = 136
      Top = 19
      Width = 73
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Enabled = False
      MaxValue = 0
      MinValue = 0
      TabOrder = 1
      Value = 1
      OnExit = SE_RCS_BoardExit
    end
    object CHB_D1: TCheckBox
      Tag = 1
      Left = 100
      Top = 20
      Width = 17
      Height = 17
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Enabled = False
      TabOrder = 0
      OnClick = CHB_D1Click
    end
    object CHB_D2: TCheckBox
      Tag = 2
      Left = 100
      Top = 46
      Width = 17
      Height = 17
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      TabOrder = 3
      OnClick = CHB_D1Click
    end
    object SE_Board2: TSpinEdit
      Left = 136
      Top = 45
      Width = 73
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Enabled = False
      MaxValue = 0
      MinValue = 0
      TabOrder = 4
      Value = 1
      OnExit = SE_RCS_BoardExit
    end
    object SE_Port2: TSpinEdit
      Left = 224
      Top = 45
      Width = 73
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Enabled = False
      MaxValue = 0
      MinValue = 0
      TabOrder = 5
      Value = 0
    end
    object CHB_D3: TCheckBox
      Tag = 3
      Left = 100
      Top = 73
      Width = 17
      Height = 17
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      TabOrder = 6
      OnClick = CHB_D1Click
    end
    object SE_Board3: TSpinEdit
      Left = 136
      Top = 71
      Width = 73
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Enabled = False
      MaxValue = 0
      MinValue = 0
      TabOrder = 7
      Value = 1
      OnExit = SE_RCS_BoardExit
    end
    object SE_Port3: TSpinEdit
      Left = 224
      Top = 71
      Width = 73
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Enabled = False
      MaxValue = 0
      MinValue = 0
      TabOrder = 8
      Value = 0
    end
    object CHB_D4: TCheckBox
      Tag = 4
      Left = 100
      Top = 99
      Width = 17
      Height = 17
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      TabOrder = 9
      OnClick = CHB_D1Click
    end
    object SE_Board4: TSpinEdit
      Left = 136
      Top = 97
      Width = 73
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Enabled = False
      MaxValue = 0
      MinValue = 0
      TabOrder = 10
      Value = 1
      OnExit = SE_RCS_BoardExit
    end
    object SE_Port4: TSpinEdit
      Left = 224
      Top = 97
      Width = 73
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Enabled = False
      MaxValue = 0
      MinValue = 0
      TabOrder = 11
      Value = 0
    end
  end
  object E_Delka: TEdit
    Left = 119
    Top = 128
    Width = 193
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    MaxLength = 6
    NumbersOnly = True
    TabOrder = 3
    Text = '0'
  end
  object LB_Stanice: TListBox
    Left = 119
    Top = 72
    Width = 193
    Height = 46
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    ItemHeight = 13
    TabOrder = 2
  end
  object CB_Zesil: TComboBox
    Left = 119
    Top = 157
    Width = 194
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 4
  end
  object GB_Zastavka: TGroupBox
    Left = 324
    Top = 8
    Width = 365
    Height = 433
    Caption = ' Zast'#225'vka '
    TabOrder = 8
    object Label5: TLabel
      Left = 16
      Top = 40
      Width = 190
      Height = 13
      Caption = 'Pro tyto typy souprav (odd'#283'lujte '#269#225'rkou):'
    end
    object Label6: TLabel
      Left = 16
      Top = 88
      Width = 148
      Height = 13
      Caption = 'Maxim'#225'ln'#237' d'#233'lka soupravy (cm):'
    end
    object Label7: TLabel
      Left = 16
      Top = 120
      Width = 150
      Height = 13
      Caption = #268'as '#269'ek'#225'n'#237' v zast'#225'vce (mm:ss):'
    end
    object CHB_Zastavka_Lichy: TCheckBox
      Left = 16
      Top = 17
      Width = 113
      Height = 17
      Caption = 'Zast'#225'vka lich'#253' sm'#283'r'
      TabOrder = 0
      OnClick = CHB_Zastavka_LichyClick
    end
    object E_Zast_Spr: TEdit
      Left = 16
      Top = 59
      Width = 337
      Height = 21
      TabOrder = 2
      Text = 'E_Zast_Spr'
    end
    object SE_Zast_DelkaSpr: TSpinEdit
      Left = 288
      Top = 91
      Width = 65
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 3
      Value = 0
    end
    object ME_Zast_Delay: TMaskEdit
      Left = 288
      Top = 123
      Width = 65
      Height = 21
      Hint = 'Zadejte aktu'#225'ln'#237' modelov'#253' cas'
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      EditMask = '!90:00;1;_'
      MaxLength = 5
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      Text = '00:00'
    end
    object PC_Zastavka: TPageControl
      Left = 2
      Top = 149
      Width = 361
      Height = 282
      ActivePage = TS_Zast_lichy
      Align = alBottom
      TabOrder = 5
      object TS_Zast_lichy: TTabSheet
        Caption = 'Lich'#253' sm'#283'r'
      end
      object TS_Zast_sudy: TTabSheet
        Caption = 'Sud'#253' sm'#283'r'
        ImageIndex = 1
      end
    end
    object CHB_Zastavka_Sudy: TCheckBox
      Left = 212
      Top = 17
      Width = 113
      Height = 17
      Caption = 'Zast'#225'vka sud'#253' sm'#283'r'
      TabOrder = 1
      OnClick = CHB_Zastavka_LichyClick
    end
  end
  object CHB_SmycBlok: TCheckBox
    Left = 119
    Top = 182
    Width = 17
    Height = 17
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    TabOrder = 9
  end
  object GB_Autoblok: TGroupBox
    Left = 8
    Top = 371
    Width = 304
    Height = 112
    Caption = ' Autoblok '
    TabOrder = 10
    object Label10: TLabel
      Left = 16
      Top = 16
      Width = 196
      Height = 13
      Caption = #218'sek je kryt'#253' n'#225'v'#283'stidlem v lich'#233'm sm'#283'ru:'
    end
    object Label11: TLabel
      Left = 16
      Top = 64
      Width = 197
      Height = 13
      Caption = #218'sek je kryt'#253' n'#225'v'#283'stidlem v sud'#233'm sm'#283'ru:'
    end
    object CHB_NavL: TCheckBox
      Left = 279
      Top = 14
      Width = 17
      Height = 17
      TabOrder = 0
      OnClick = CHB_NavLClick
    end
    object CB_NavL: TComboBox
      Left = 16
      Top = 35
      Width = 280
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
    end
    object CHB_NavS: TCheckBox
      Left = 279
      Top = 62
      Width = 17
      Height = 17
      TabOrder = 2
      OnClick = CHB_NavSClick
    end
    object CB_NavS: TComboBox
      Left = 16
      Top = 83
      Width = 280
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 3
    end
  end
  object CB_Speed: TComboBox
    Left = 119
    Top = 207
    Width = 194
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 11
    Items.Strings = (
      '20 km/h'
      '30 km/h'
      '40 km/h'
      '50 km/h'
      '60 km/h'
      '70 km/h'
      '80 km/h'
      '90 km/h'
      '100 km/h'
      '110 km/h'
      '120 km/h')
  end
end
