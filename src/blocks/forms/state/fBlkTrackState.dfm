object F_BlkTrackState: TF_BlkTrackState
  Left = 967
  Top = 123
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Vlastnosti useku [usek]'
  ClientHeight = 329
  ClientWidth = 585
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
  TextHeight = 13
  object L_Usek21: TLabel
    Left = 8
    Top = 8
    Width = 34
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Z'#225'v'#283'r :'
  end
  object L_Usek25: TLabel
    Left = 7
    Top = 56
    Width = 52
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Konec JC :'
  end
  object L_Usek27: TLabel
    Left = 7
    Top = 91
    Width = 93
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'P'#345'edpov'#237'dan'#253' vlak:'
  end
  object Label1: TLabel
    Left = 305
    Top = 8
    Width = 31
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = #352't'#237'tek:'
  end
  object Label3: TLabel
    Left = 7
    Top = 122
    Width = 77
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'NavCRef po'#269'et:'
  end
  object Label6: TLabel
    Left = 305
    Top = 80
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
    Top = 156
    Width = 75
    Height = 13
    Caption = 'Zesilova'#269' zkrat:'
  end
  object Label7: TLabel
    Left = 8
    Top = 188
    Width = 93
    Height = 13
    Caption = 'Zesilova'#269' nap'#225'jen'#237':'
  end
  object CB_KonecVC: TComboBox
    Left = 176
    Top = 56
    Width = 107
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Style = csDropDownList
    TabOrder = 2
    Items.Strings = (
      'Zadne'
      'Vlakove'
      'Posunove')
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
    TabOrder = 0
    Items.Strings = (
      #381#225'dn'#253
      'Vlakov'#253
      'Posunov'#253
      'Nouzov'#253
      'Stav'#283'c'#237
      'AB')
  end
  object B_Apply: TButton
    Left = 505
    Top = 300
    Width = 73
    Height = 24
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Pou'#382#237't'
    Default = True
    TabOrder = 9
    OnClick = B_ApplyClick
  end
  object B_Refresh: TButton
    Left = 425
    Top = 300
    Width = 76
    Height = 24
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Obnovit'
    TabOrder = 10
    OnClick = B_RefreshClick
  end
  object M_Note: TMemo
    Left = 303
    Top = 26
    Width = 275
    Height = 49
    Lines.Strings = (
      'M_Note')
    TabOrder = 7
  end
  object M_Lockout: TMemo
    Left = 303
    Top = 98
    Width = 275
    Height = 49
    Lines.Strings = (
      'M_Lockout')
    TabOrder = 8
  end
  object SE_Train_Predict: TSpinEdit
    Left = 176
    Top = 88
    Width = 107
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 3
    Value = 0
  end
  object SE_NavJCRef: TSpinEdit
    Left = 176
    Top = 119
    Width = 107
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 4
    Value = 0
  end
  object CB_Booster_Short: TComboBox
    Left = 176
    Top = 153
    Width = 107
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Style = csDropDownList
    TabOrder = 5
    Items.Strings = (
      'Undef'
      'Ano'
      'Ne')
  end
  object CB_Booster_Power: TComboBox
    Left = 176
    Top = 185
    Width = 107
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Style = csDropDownList
    TabOrder = 6
    Items.Strings = (
      'Undef'
      'Ne'
      'Ano')
  end
  object GB_Trains: TGroupBox
    Left = 302
    Top = 153
    Width = 275
    Height = 142
    Caption = ' Vlaky '
    TabOrder = 11
    object LB_Trains: TListBox
      Left = 2
      Top = 44
      Width = 271
      Height = 44
      Align = alBottom
      ItemHeight = 13
      TabOrder = 0
    end
    object B_Train_Delete: TButton
      Left = 56
      Top = 13
      Width = 161
      Height = 25
      Caption = 'Odstranit vlak z '#250'seku'
      TabOrder = 1
      OnClick = B_Train_DeleteClick
    end
    object GB_Train_Add: TGroupBox
      Left = 2
      Top = 88
      Width = 271
      Height = 52
      Align = alBottom
      Caption = ' P'#345'idat vlak '
      TabOrder = 2
      object Label5: TLabel
        Left = 9
        Top = 17
        Width = 58
        Height = 13
        Caption = 'Index vlaku:'
      end
      object SE_Train_Add_Index: TSpinEdit
        Left = 94
        Top = 17
        Width = 80
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 0
        Value = 0
      end
      object B_Train_Add: TButton
        Left = 184
        Top = 17
        Width = 75
        Height = 22
        Caption = 'P'#345'idat'
        TabOrder = 1
        OnClick = B_Train_AddClick
      end
    end
  end
  object CHB_NUZ: TCheckBox
    Left = 8
    Top = 34
    Width = 233
    Height = 17
    Caption = #218'sek ozna'#269'en pro nouzov'#233' uvoln'#283'n'#237' z'#225'v'#283'ru'
    TabOrder = 1
  end
  object CHB_DCC: TCheckBox
    Left = 8
    Top = 219
    Width = 51
    Height = 17
    Caption = 'DCC'
    Enabled = False
    TabOrder = 12
  end
  object CHB_SlowingReady: TCheckBox
    Left = 8
    Top = 242
    Width = 97
    Height = 17
    Caption = 'Slowing Ready'
    Enabled = False
    TabOrder = 13
  end
  object CHB_PathCancelZaver: TCheckBox
    Left = 8
    Top = 265
    Width = 201
    Height = 17
    Caption = 'Odpo'#269'et ru'#353'en'#237' z'#225'v'#283'ru j'#237'zdn'#237' cestou'
    Enabled = False
    TabOrder = 14
  end
end
