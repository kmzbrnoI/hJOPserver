object F_BlkUsek_tech: TF_BlkUsek_tech
  Left = 967
  Top = 123
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Vlastnosti useku [usek]'
  ClientHeight = 345
  ClientWidth = 585
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
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
  object L_Usek24: TLabel
    Left = 7
    Top = 33
    Width = 29
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'NUZ :'
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
    Left = 8
    Top = 82
    Width = 121
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'P'#345'edpov'#237'dan'#225' souprava :'
  end
  object Label1: TLabel
    Left = 306
    Top = 12
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
    Top = 110
    Width = 90
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'SComJCRef po'#269'et:'
  end
  object Label6: TLabel
    Left = 306
    Top = 84
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
    Top = 140
    Width = 75
    Height = 13
    Caption = 'Zesilova'#269' zkrat:'
  end
  object Label7: TLabel
    Left = 8
    Top = 172
    Width = 93
    Height = 13
    Caption = 'Zesilova'#269' nap'#225'jen'#237':'
  end
  object Label4: TLabel
    Left = 304
    Top = 160
    Width = 25
    Height = 13
    Caption = 'DCC:'
  end
  object S_DCC: TShape
    Left = 342
    Top = 160
    Width = 65
    Height = 16
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
    ItemHeight = 13
    TabOrder = 2
    Items.Strings = (
      'Zadne'
      'Vlakove'
      'Posunove')
  end
  object CB_NUZ: TComboBox
    Left = 176
    Top = 33
    Width = 107
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
    Items.Strings = (
      'Ne'
      'Ano')
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
    ItemHeight = 13
    TabOrder = 0
    Items.Strings = (
      #381#225'dn'#253
      'Vlakov'#253
      'Posunov'#253
      'Nouzov'#253
      'Stav'#283'c'#237
      'AB')
  end
  object B_SaveData: TButton
    Left = 505
    Top = 314
    Width = 73
    Height = 24
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Pou'#382#237't'
    Default = True
    TabOrder = 9
    OnClick = B_SaveDataClick
  end
  object B_Obnovit: TButton
    Left = 425
    Top = 314
    Width = 76
    Height = 24
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Obnovit'
    TabOrder = 10
    OnClick = B_ObnovitClick
  end
  object M_Stitek: TMemo
    Left = 304
    Top = 30
    Width = 275
    Height = 49
    Lines.Strings = (
      'M_Stitek')
    TabOrder = 7
  end
  object M_Vyluka: TMemo
    Left = 304
    Top = 102
    Width = 275
    Height = 49
    Lines.Strings = (
      'M_Stitek')
    TabOrder = 8
  end
  object SE_Souprava_Predict: TSpinEdit
    Left = 176
    Top = 82
    Width = 107
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 3
    Value = 0
  end
  object SE_SComJCRef: TSpinEdit
    Left = 176
    Top = 110
    Width = 107
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 4
    Value = 0
  end
  object CB_Zes_Zkrat: TComboBox
    Left = 176
    Top = 137
    Width = 107
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 5
    Items.Strings = (
      'Undef'
      'Ano'
      'Ne')
  end
  object CB_Zes_Napajeni: TComboBox
    Left = 176
    Top = 169
    Width = 107
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 6
    Items.Strings = (
      'Undef'
      'Ne'
      'Ano')
  end
  object GB_Soupravy: TGroupBox
    Left = 8
    Top = 195
    Width = 275
    Height = 142
    Caption = ' Soupravy '
    TabOrder = 11
    object LB_Soupravy: TListBox
      Left = 2
      Top = 44
      Width = 271
      Height = 44
      Align = alBottom
      ItemHeight = 13
      TabOrder = 0
    end
    object B_SprDelete: TButton
      Left = 56
      Top = 13
      Width = 161
      Height = 25
      Caption = 'Odstranit soupravu z '#250'seku'
      TabOrder = 1
      OnClick = B_SprDeleteClick
    end
    object GB_SprAdd: TGroupBox
      Left = 2
      Top = 88
      Width = 271
      Height = 52
      Align = alBottom
      Caption = ' P'#345'idat soupravu '
      TabOrder = 2
      object Label5: TLabel
        Left = 9
        Top = 17
        Width = 75
        Height = 13
        Caption = 'Index soupravy:'
      end
      object SE_SprAdd_Index: TSpinEdit
        Left = 94
        Top = 17
        Width = 80
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 0
        Value = 0
      end
      object B_SprAdd: TButton
        Left = 184
        Top = 17
        Width = 75
        Height = 22
        Caption = 'P'#345'idat'
        TabOrder = 1
        OnClick = B_SprAddClick
      end
    end
  end
end
