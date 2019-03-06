object F_BlkSComEvent: TF_BlkSComEvent
  Left = 0
  Top = 0
  Align = alClient
  BorderStyle = bsNone
  Caption = 'F_BlkSComEvent'
  ClientHeight = 302
  ClientWidth = 348
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
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
    Width = 106
    Height = 13
    Caption = 'Min. d'#233'lka soup. (cm):'
  end
  object Label3: TLabel
    Left = 8
    Top = 79
    Width = 110
    Height = 13
    Caption = 'Max. d'#233'lka soup. (cm):'
  end
  object GB_DetekceZastaveni: TGroupBox
    Left = 7
    Top = 106
    Width = 165
    Height = 191
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' Detekce zastaven'#237'  '
    TabOrder = 0
    object P_ZastForm: TPanel
      Left = 2
      Top = 15
      Width = 161
      Height = 174
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
    end
  end
  object GB_DetekceZpomalenii: TGroupBox
    Left = 177
    Top = 50
    Width = 165
    Height = 247
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' Detekce zpomalen'#237'  '
    TabOrder = 1
    object CB_ZpomalitKmH: TComboBox
      Left = 11
      Top = 40
      Width = 141
      Height = 21
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Style = csDropDownList
      ItemHeight = 13
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
      Left = 11
      Top = 19
      Width = 121
      Height = 17
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Zpomalit na rychlost:'
      TabOrder = 0
      OnClick = CHB_ZpomalitClick
    end
    object P_ZpomForm: TPanel
      Left = 2
      Top = 71
      Width = 161
      Height = 174
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
    end
  end
  object E_Spr: TEdit
    Left = 8
    Top = 24
    Width = 334
    Height = 21
    TabOrder = 2
  end
  object SE_MinLength: TSpinEdit
    Left = 120
    Top = 51
    Width = 52
    Height = 22
    MaxValue = 1000
    MinValue = 0
    TabOrder = 3
    Value = 0
  end
  object SE_MaxLength: TSpinEdit
    Left = 120
    Top = 79
    Width = 52
    Height = 22
    MaxValue = 1000
    MinValue = 0
    TabOrder = 4
    Value = 0
  end
end
