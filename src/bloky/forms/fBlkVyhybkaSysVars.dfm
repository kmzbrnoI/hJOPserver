object F_BlkVyh_tech: TF_BlkVyh_tech
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Technologicke vlastnosti bloku: [blok]'
  ClientHeight = 194
  ClientWidth = 577
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label6: TLabel
    Left = 298
    Top = 84
    Width = 35
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'V'#253'luka:'
  end
  object Label1: TLabel
    Left = 298
    Top = 12
    Width = 31
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = #352't'#237'tek:'
  end
  object L_Usek21: TLabel
    Left = 8
    Top = 24
    Width = 149
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Po'#269'et ud'#283'len'#253'ch nouz. z'#225'v'#283'r'#367':'
  end
  object L_Usek25: TLabel
    Left = 7
    Top = 101
    Width = 147
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'RCS v'#253'stup dr'#382'en (zamknuto):'
  end
  object L_Usek20: TLabel
    Left = 7
    Top = 129
    Width = 64
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Po'#269'et z'#225'mk'#367':'
  end
  object Label2: TLabel
    Left = 8
    Top = 51
    Width = 112
    Height = 13
    Caption = 'Stav'#283'n'#237' do polohy plus:'
  end
  object Label7: TLabel
    Left = 8
    Top = 76
    Width = 120
    Height = 13
    Caption = 'Stav'#283'n'#237' do polohy m'#237'nus:'
  end
  object B_Update: TButton
    Left = 384
    Top = 157
    Width = 92
    Height = 26
    Caption = 'Aktualizovat'
    TabOrder = 0
    OnClick = B_UpdateClick
  end
  object B_Apply: TButton
    Left = 482
    Top = 157
    Width = 89
    Height = 26
    Caption = 'Pou'#382#237't'
    TabOrder = 1
    OnClick = B_ApplyClick
  end
  object M_Vyluka: TMemo
    Left = 296
    Top = 102
    Width = 275
    Height = 49
    Lines.Strings = (
      'M_Stitek')
    TabOrder = 2
  end
  object M_Stitek: TMemo
    Left = 296
    Top = 30
    Width = 275
    Height = 49
    Lines.Strings = (
      'M_Stitek')
    TabOrder = 3
  end
  object CB_Locked: TComboBox
    Left = 176
    Top = 101
    Width = 107
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Style = csDropDownList
    Enabled = False
    ItemHeight = 13
    TabOrder = 4
    Items.Strings = (
      'Ne'
      'Ano')
  end
  object SE_Locks: TSpinEdit
    Left = 176
    Top = 127
    Width = 107
    Height = 22
    MaxValue = 0
    MinValue = 0
    ReadOnly = True
    TabOrder = 5
    Value = 0
  end
  object CB_Stav_Plus: TComboBox
    Left = 176
    Top = 51
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
      'Ne'
      'Ano')
  end
  object CB_Stav_Minus: TComboBox
    Left = 176
    Top = 76
    Width = 107
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 7
    Items.Strings = (
      'Ne'
      'Ano')
  end
  object SE_Zaver: TSpinEdit
    Left = 176
    Top = 24
    Width = 107
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 8
    Value = 0
  end
  object B_Unlock: TButton
    Left = 176
    Top = 155
    Width = 107
    Height = 25
    Caption = 'Odemknout'
    TabOrder = 9
    OnClick = B_UnlockClick
  end
end
