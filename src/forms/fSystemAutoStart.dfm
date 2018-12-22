object F_AutoStartSystems: TF_AutoStartSystems
  Left = 839
  Top = 713
  BorderIcons = []
  BorderStyle = bsNone
  Caption = '[nil]'
  ClientHeight = 105
  ClientWidth = 345
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object L_Systems1: TLabel
    Left = 8
    Top = 8
    Width = 326
    Height = 22
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Prob'#237'h'#225' automatick'#233' zap'#237'n'#225'n'#237' syst'#233'mu'
    Font.Charset = EASTEUROPE_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object L_Systems2: TLabel
    Left = 48
    Top = 40
    Width = 132
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Syst'#233'my budou zapnuty za :'
  end
  object L_Cas: TLabel
    Left = 192
    Top = 40
    Width = 17
    Height = 22
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = '-1'
    Font.Charset = EASTEUROPE_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object L_Systems3: TLabel
    Left = 224
    Top = 40
    Width = 35
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'sekund'
  end
  object B_Abort: TButton
    Left = 104
    Top = 72
    Width = 121
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Zru'#353'it'
    TabOrder = 0
    OnClick = B_AbortClick
  end
end
