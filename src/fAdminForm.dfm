object F_Admin: TF_Admin
  Left = 751
  Top = 268
  AlphaBlend = True
  AlphaBlendValue = 200
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Administratorske rozhrani programu'
  ClientHeight = 154
  ClientWidth = 417
  Color = clBlack
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
    Top = 122
    Width = 73
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Ulozit data'
    TabOrder = 1
    OnClick = B_SaveClick
  end
  object CHB_SimInput: TCheckBox
    Left = 8
    Top = 40
    Width = 65
    Height = 17
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'InputSim'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
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
    Caption = 'Simulovat obsazeni useku soupravou'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
  end
  object CHB_SystemStart: TCheckBox
    Left = 8
    Top = 73
    Width = 233
    Height = 17
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Umoznit start systemu pouze se zaplym MTB'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
  end
  object CHB_JC_Simulator: TCheckBox
    Left = 8
    Top = 90
    Width = 75
    Height = 17
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'JC simulator'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
    OnClick = CHB_JC_SimulatorClick
  end
  object CHB_Trat_Sim: TCheckBox
    Left = 8
    Top = 107
    Width = 97
    Height = 17
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Trat simulator'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
    OnClick = CHB_Trat_SimClick
  end
  object CHB_SimVyhybky: TCheckBox
    Left = 8
    Top = 124
    Width = 145
    Height = 17
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Simulace staveni vyhybek'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 7
    OnClick = CHB_SimVyhybkyClick
  end
  object Button1: TButton
    Left = 304
    Top = 72
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 8
  end
end
