object F_BlkSignalEvent: TF_BlkSignalEvent
  Left = 0
  Top = 0
  Align = alClient
  BorderStyle = bsNone
  Caption = 'F_BlkSignalEvent'
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
    Width = 239
    Height = 13
    Caption = 'Reagovat na tyto typy souprav: (regul'#225'rn'#237' v'#253'raz)'
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
    object CHB_Zpomalit: TCheckBox
      Left = 9
      Top = 19
      Width = 152
      Height = 17
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Zpomalit na rychlost [km/h]:'
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
    object SE_Slow_Speed: TSpinEdit
      Left = 9
      Top = 42
      Width = 146
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 1
      Value = 0
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
