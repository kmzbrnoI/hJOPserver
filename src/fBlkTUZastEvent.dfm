object F_BlkTUZastEvent: TF_BlkTUZastEvent
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'F_BlkTUZastEvent'
  ClientHeight = 257
  ClientWidth = 354
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
    Top = 7
    Width = 165
    Height = 191
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
    object P_Stop: TPanel
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
    Left = 183
    Top = 7
    Width = 165
    Height = 242
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
    object CB_ZpomalitKmH: TComboBox
      Left = 15
      Top = 41
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
      Top = 20
      Width = 121
      Height = 17
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Zpomalit na rychlost:'
      TabOrder = 0
      OnClick = CHB_ZpomalClick
    end
    object P_Zpom: TPanel
      Left = 2
      Top = 66
      Width = 161
      Height = 174
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
    end
  end
end
