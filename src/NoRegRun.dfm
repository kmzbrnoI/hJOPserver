object F_NoRegRun: TF_NoRegRun
  Left = 830
  Top = 67
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'Vitejte v Ridicim programu'
  ClientHeight = 225
  ClientWidth = 353
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object B_Register: TButton
    Left = 112
    Top = 192
    Width = 129
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Zaregistrovat se'
    TabOrder = 1
    OnClick = B_RegisterClick
  end
  object P_1: TPanel
    Left = 8
    Top = 120
    Width = 337
    Height = 65
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    BevelOuter = bvLowered
    Color = clBlack
    TabOrder = 0
    object L_1: TLabel
      Left = 8
      Top = 8
      Width = 321
      Height = 17
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Alignment = taCenter
      AutoSize = False
      Caption = '=====text====='
      Font.Charset = EASTEUROPE_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object B_1: TButton
      Tag = 1
      Left = 8
      Top = 32
      Width = 81
      Height = 25
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = '1'
      TabOrder = 0
      OnClick = B_1Click
    end
    object B_2: TButton
      Tag = 2
      Left = 128
      Top = 32
      Width = 83
      Height = 25
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = '2'
      TabOrder = 1
      OnClick = B_1Click
    end
    object B_3: TButton
      Tag = 3
      Left = 248
      Top = 32
      Width = 83
      Height = 25
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = '3'
      TabOrder = 2
      OnClick = B_1Click
    end
  end
  object ST_1: TStaticText
    Left = 8
    Top = 8
    Width = 337
    Height = 81
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Alignment = taCenter
    AutoSize = False
    Caption = 
      'Tento program je shareware, tzn., '#382'e jeho plne funkcn'#237' demoverzi' +
      ' mu'#382'ete pou'#382#237'vat po dobu 15 dn'#237'. Po uplynut'#237' t'#233'to doby mus'#237'te pr' +
      'ogam bud zaregistrovat, nebo odstranit z poc'#237'tace. Toto demo mu'#382 +
      'ete d'#225'le volne '#353#237'rit. Pro registraci klepnete na tlac'#237'tko Zaregi' +
      'strovat se.'
    Font.Charset = EASTEUROPE_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
  end
  object ST_2: TStaticText
    Left = -1
    Top = 96
    Width = 354
    Height = 17
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Alignment = taCenter
    AutoSize = False
    Caption = 'Toto dialogov'#233' okno se po registraci ji'#382' nebude zobrazovat'
    Font.Charset = EASTEUROPE_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
  end
end
