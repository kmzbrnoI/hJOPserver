object F_HoukEv: TF_HoukEv
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'F_HoukEv'
  ClientHeight = 271
  ClientWidth = 161
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 27
    Height = 13
    Caption = 'Akce:'
  end
  object Label2: TLabel
    Left = 8
    Top = 56
    Width = 38
    Height = 13
    Caption = 'Funkce:'
  end
  object CB_Action: TComboBox
    Left = 8
    Top = 27
    Width = 145
    Height = 21
    Style = csDropDownList
    TabOrder = 0
    Items.Strings = (
      'Nic ned'#283'lat'
      'Kr'#225'tce zapnout funkci'
      'Zapnout funkci'
      'Vypnout funkci')
  end
  object P_Ev: TPanel
    Left = 0
    Top = 107
    Width = 161
    Height = 164
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
  end
  object CB_Func: TComboBox
    Left = 8
    Top = 75
    Width = 145
    Height = 21
    TabOrder = 2
  end
end
