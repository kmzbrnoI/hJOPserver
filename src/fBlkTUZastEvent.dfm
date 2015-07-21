object F_BlkTUZastEvent: TF_BlkTUZastEvent
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'F_BlkTUZastEvent'
  ClientHeight = 354
  ClientWidth = 274
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object GB_DetekceZastaveni: TGroupBox
    Left = 7
    Top = 2
    Width = 261
    Height = 150
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' Detekce zastaven'#237'  '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object Label8: TLabel
      Left = 79
      Top = 18
      Width = 56
      Height = 13
      Caption = #268#225'st '#250'seku:'
    end
    object Label9: TLabel
      Left = 79
      Top = 62
      Width = 14
      Height = 13
      Caption = 'IR:'
    end
    object Label1: TLabel
      Left = 15
      Top = 105
      Width = 25
      Height = 13
      Caption = 'Stav:'
    end
    object RB_Zast_Usek: TRadioButton
      Left = 15
      Top = 36
      Width = 45
      Height = 17
      Caption = #218'sek'
      TabOrder = 0
      OnClick = RB_Zast_UsekClick
    end
    object RB_Zast_IR: TRadioButton
      Left = 15
      Top = 80
      Width = 33
      Height = 17
      Caption = 'IR'
      TabOrder = 1
      OnClick = RB_Zast_UsekClick
    end
    object CB_Zast_IR: TComboBox
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
      ItemHeight = 13
      ParentFont = False
      TabOrder = 2
    end
    object CB_Zast_UsekPart: TComboBox
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
      ItemHeight = 13
      ParentFont = False
      TabOrder = 3
      Items.Strings = (
        '1'
        '2'
        '3'
        '4')
    end
    object CB_Zast_Stav: TComboBox
      Left = 15
      Top = 120
      Width = 233
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 4
      Items.Strings = (
        'uvoln'#283'no'
        'obsazeno')
    end
  end
  object GB_DetekceZpomalenii: TGroupBox
    Left = 7
    Top = 156
    Width = 261
    Height = 191
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' Detekce zpomalen'#237'  '
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
    object Label12: TLabel
      Left = 79
      Top = 102
      Width = 14
      Height = 13
      Caption = 'IR:'
    end
    object Label13: TLabel
      Left = 78
      Top = 58
      Width = 56
      Height = 13
      Caption = #268#225'st '#250'seku:'
    end
    object Label2: TLabel
      Left = 15
      Top = 145
      Width = 25
      Height = 13
      Caption = 'Stav:'
    end
    object CB_ZpomalitKmH: TComboBox
      Left = 140
      Top = 32
      Width = 107
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
    object CHB_Zpomal: TCheckBox
      Left = 15
      Top = 23
      Width = 121
      Height = 17
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Povolit zpomalov'#225'n'#237
      TabOrder = 0
      OnClick = CHB_ZpomalClick
    end
    object CB_Zpom_IR: TComboBox
      Left = 77
      Top = 120
      Width = 170
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
      TabOrder = 2
    end
    object RB_Zpom_IR: TRadioButton
      Left = 14
      Top = 120
      Width = 33
      Height = 17
      Caption = 'IR'
      TabOrder = 3
      OnClick = RB_Zpom_UsekClick
    end
    object CB_Zpom_UsekPart: TComboBox
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
      ItemHeight = 13
      ParentFont = False
      TabOrder = 4
      Items.Strings = (
        '1'
        '2'
        '3'
        '4')
    end
    object RB_Zpom_Usek: TRadioButton
      Left = 14
      Top = 76
      Width = 45
      Height = 17
      Caption = #218'sek'
      TabOrder = 5
      OnClick = RB_Zpom_UsekClick
    end
    object CB_Zpom_stav: TComboBox
      Left = 15
      Top = 160
      Width = 232
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 6
      Items.Strings = (
        'uvoln'#283'no'
        'obsazeno')
    end
  end
end
