object F_Admin: TF_Admin
  Left = 751
  Top = 268
  AlphaBlendValue = 100
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Testovac'#237' rozhran'#237' programu'
  ClientHeight = 137
  ClientWidth = 417
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object B_InputSim: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'InputSim'
    TabOrder = 0
    OnClick = B_InputSimClick
  end
  object B_Save: TButton
    Left = 337
    Top = 104
    Width = 73
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Ulo'#382'it data'
    TabOrder = 1
    OnClick = B_SaveClick
  end
  object CHB_SimInput: TCheckBox
    Left = 8
    Top = 39
    Width = 121
    Height = 17
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'InputSim automaticky'
    TabOrder = 2
    OnClick = CHB_SimInputClick
  end
  object CHB_SimSoupravaUsek: TCheckBox
    Left = 8
    Top = 56
    Width = 201
    Height = 17
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Simulovat obsazen'#237' '#250'seku soupravou'
    TabOrder = 3
    OnClick = CHB_SimSoupravaUsekClick
  end
  object CHB_JC_Simulator: TCheckBox
    Left = 8
    Top = 74
    Width = 75
    Height = 17
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'JC simul'#225'tor'
    TabOrder = 4
    OnClick = CHB_JC_SimulatorClick
  end
  object CHB_Trat_Sim: TCheckBox
    Left = 8
    Top = 91
    Width = 97
    Height = 17
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Tra'#357' simul'#225'tor'
    TabOrder = 5
    OnClick = CHB_Trat_SimClick
  end
  object CHB_SimVyhybky: TCheckBox
    Left = 8
    Top = 108
    Width = 145
    Height = 17
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Simulovat stav'#283'n'#237' v'#253'hybek'
    TabOrder = 6
    OnClick = CHB_SimVyhybkyClick
  end
end
