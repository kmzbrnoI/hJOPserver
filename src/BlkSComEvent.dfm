object F_BlkSComEvent: TF_BlkSComEvent
  Left = 0
  Top = 0
  Align = alClient
  BorderStyle = bsNone
  Caption = 'F_BlkSComEvent'
  ClientHeight = 385
  ClientWidth = 283
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  ExplicitWidth = 320
  ExplicitHeight = 240
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 247
    Height = 13
    Caption = 'Reagovat na tyto typy souprav: (odd'#283'lujte '#269#225'rkou)'
  end
  object Label2: TLabel
    Left = 8
    Top = 51
    Width = 146
    Height = 13
    Caption = 'Minim'#225'ln'#237' d'#233'lka soupravy (cm):'
  end
  object Label3: TLabel
    Left = 8
    Top = 79
    Width = 150
    Height = 13
    Caption = 'Maxim'#225'ln'#237' d'#233'lka soupravy (cm):'
  end
  object GB_DetekceZastaveni: TGroupBox
    Left = 8
    Top = 106
    Width = 265
    Height = 116
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' Detekce zastaven'#237' pred n'#225'vestidlem '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object Label4: TLabel
      Left = 79
      Top = 18
      Width = 56
      Height = 13
      Caption = #268#225'st '#250'seku:'
    end
    object Label5: TLabel
      Left = 79
      Top = 62
      Width = 14
      Height = 13
      Caption = 'IR:'
    end
    object RB_ZastaveniUsek: TRadioButton
      Left = 15
      Top = 36
      Width = 45
      Height = 17
      Caption = #218'sek'
      TabOrder = 0
      OnClick = RB_ZastaveniUsekClick
    end
    object RB_ZastaveniIR: TRadioButton
      Left = 15
      Top = 80
      Width = 33
      Height = 17
      Caption = 'IR'
      TabOrder = 1
      OnClick = RB_ZastaveniUsekClick
    end
    object CB_ZastavitIR: TComboBox
      Left = 78
      Top = 80
      Width = 169
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
      TabOrder = 2
    end
    object CB_ZastavitUsek: TComboBox
      Left = 79
      Top = 36
      Width = 169
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
      TabOrder = 3
    end
  end
  object GB_DetekceZpomalenii: TGroupBox
    Left = 8
    Top = 226
    Width = 265
    Height = 151
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' Detekce zpomalen'#237' pred n'#225'vestidlem '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object L_SCom14: TLabel
      Left = 140
      Top = 15
      Width = 100
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Zpomalit na rychlost :'
    end
    object Label6: TLabel
      Left = 79
      Top = 102
      Width = 14
      Height = 13
      Caption = 'IR:'
    end
    object Label7: TLabel
      Left = 78
      Top = 58
      Width = 56
      Height = 13
      Caption = #268#225'st '#250'seku:'
    end
    object CB_ZpomalitKmH: TComboBox
      Left = 140
      Top = 32
      Width = 100
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
      ParentFont = False
      TabOrder = 1
      Items.Strings = (
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
    object CHB_Zpomalit: TCheckBox
      Left = 15
      Top = 24
      Width = 121
      Height = 17
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Povolit zpomalov'#225'n'#237
      TabOrder = 0
      OnClick = CHB_ZpomalitClick
    end
    object CB_ZpomalitIR: TComboBox
      Left = 77
      Top = 120
      Width = 169
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
      TabOrder = 2
    end
    object RB_ZpomaleniIR: TRadioButton
      Left = 14
      Top = 120
      Width = 33
      Height = 17
      Caption = 'IR'
      TabOrder = 3
      OnClick = RB_ZpomaleniUsekClick
    end
    object CB_ZpomalitUsek: TComboBox
      Left = 78
      Top = 76
      Width = 169
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
      TabOrder = 4
    end
    object RB_ZpomaleniUsek: TRadioButton
      Left = 14
      Top = 76
      Width = 45
      Height = 17
      Caption = #218'sek'
      TabOrder = 5
      OnClick = RB_ZpomaleniUsekClick
    end
  end
  object E_Spr: TEdit
    Left = 8
    Top = 24
    Width = 265
    Height = 21
    TabOrder = 2
  end
  object SE_MinLength: TSpinEdit
    Left = 176
    Top = 51
    Width = 97
    Height = 22
    MaxValue = 1000
    MinValue = 0
    TabOrder = 3
    Value = 0
  end
  object SE_MaxLength: TSpinEdit
    Left = 176
    Top = 79
    Width = 97
    Height = 22
    MaxValue = 1000
    MinValue = 0
    TabOrder = 4
    Value = 0
  end
end
