object F_Admin: TF_Admin
  Left = 751
  Top = 268
  AlphaBlendValue = 100
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Simulace a diagnostika'
  ClientHeight = 175
  ClientWidth = 417
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
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
    Left = 304
    Top = 143
    Width = 106
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Ulo'#382'it do souboru'
    TabOrder = 10
    OnClick = B_SaveClick
  end
  object CHB_SimInput: TCheckBox
    Left = 8
    Top = 39
    Width = 233
    Height = 17
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Simulovat stav vstup'#367' automaticky (InputSim)'
    TabOrder = 3
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
    TabOrder = 4
    OnClick = CHB_SimSoupravaUsekClick
  end
  object CHB_JC_Simulator: TCheckBox
    Left = 8
    Top = 74
    Width = 202
    Height = 17
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Simulovat pr'#367'jezd vlaku j'#237'zdn'#237' cestou'
    TabOrder = 5
    OnClick = CHB_JC_SimulatorClick
  end
  object CHB_Trat_Sim: TCheckBox
    Left = 8
    Top = 91
    Width = 161
    Height = 17
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Simulovat pr'#367'jezd vlaku trat'#237
    TabOrder = 6
    OnClick = CHB_Trat_SimClick
  end
  object CHB_SimVyhybky: TCheckBox
    Left = 8
    Top = 108
    Width = 249
    Height = 17
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Simulovat koncovou polohu p'#345'i stav'#283'n'#237' v'#253'hybek'
    TabOrder = 7
    OnClick = CHB_SimVyhybkyClick
  end
  object CHB_Zaver: TCheckBox
    Left = 8
    Top = 126
    Width = 201
    Height = 17
    Caption = 'Zobrazovat diagnostick'#233' z'#225'v'#283'ry blok'#367
    TabOrder = 8
    OnClick = CHB_ZaverClick
  end
  object CHB_Show_Block_Id: TCheckBox
    Left = 8
    Top = 144
    Width = 161
    Height = 17
    Caption = 'Zobrazovat v menu ID blok'#367
    TabOrder = 9
    OnClick = CHB_Show_Block_IdClick
  end
  object B_JC_Single_Iter: TButton
    Left = 88
    Top = 8
    Width = 75
    Height = 25
    Caption = 'JC 1 iterace'
    TabOrder = 1
    OnClick = B_JC_Single_IterClick
  end
  object B_Trat_Single_Iter: TButton
    Left = 169
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Tra'#357' 1 iterace'
    TabOrder = 2
    OnClick = B_Trat_Single_IterClick
  end
end
