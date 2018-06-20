object F_FuncsSet: TF_FuncsSet
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Nastavit funkci dle v'#253'znamu'
  ClientHeight = 169
  ClientWidth = 257
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 241
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'Nastavte funkci v'#353'em p'#345'evzat'#253'm hnac'#237'm vozidl'#367'm'
  end
  object Label2: TLabel
    Left = 8
    Top = 32
    Width = 76
    Height = 13
    Caption = 'V'#253'znam funkce:'
  end
  object L_Status: TLabel
    Left = 8
    Top = 147
    Width = 112
    Height = 14
    Caption = 'Funkce nastaveny'
    Font.Charset = EASTEUROPE_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object CB_Vyznam: TComboBox
    Left = 8
    Top = 48
    Width = 241
    Height = 21
    ItemHeight = 13
    TabOrder = 0
    Text = 'zvuk'
    Items.Strings = (
      'zvuk')
  end
  object RG_Stav: TRadioGroup
    Left = 8
    Top = 75
    Width = 241
    Height = 57
    Caption = ' Nov'#253' stav funkce '
    ItemIndex = 0
    Items.Strings = (
      'vypnuta'
      'zapnuta')
    TabOrder = 1
  end
  object B_Apply: TButton
    Left = 174
    Top = 138
    Width = 75
    Height = 25
    Caption = 'Nastavit'
    TabOrder = 2
    OnClick = B_ApplyClick
  end
end
