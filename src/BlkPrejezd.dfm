object F_BlkPrejezd: TF_BlkPrejezd
  Left = 741
  Top = 209
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Prejezd : [Prejezd]'
  ClientHeight = 625
  ClientWidth = 354
  Color = clBtnFace
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
  object L_P02: TLabel
    Left = 8
    Top = 8
    Width = 80
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'N'#225'zev prejezdu :'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object L_SCom02: TLabel
    Left = 9
    Top = 36
    Width = 11
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'ID'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object L_Usek03: TLabel
    Left = 7
    Top = 59
    Width = 42
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Stanice :'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object E_Prj_Nazev: TEdit
    Left = 138
    Top = 8
    Width = 209
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    MaxLength = 32
    TabOrder = 0
  end
  object B_save_P: TButton
    Left = 256
    Top = 584
    Width = 82
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Pou'#382#237't'
    Default = True
    TabOrder = 4
    OnClick = B_save_PClick
  end
  object B_Storno: TButton
    Left = 160
    Top = 584
    Width = 81
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Storno'
    TabOrder = 5
    OnClick = B_StornoClick
  end
  object SE_ID: TSpinEdit
    Left = 138
    Top = 33
    Width = 209
    Height = 22
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    MaxValue = 0
    MinValue = 0
    TabOrder = 1
    Value = 0
  end
  object LB_Stanice: TListBox
    Left = 136
    Top = 59
    Width = 209
    Height = 46
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ItemHeight = 13
    ParentFont = False
    TabOrder = 2
  end
  object GB_MTB: TGroupBox
    Left = 16
    Top = 118
    Width = 322
    Height = 447
    Caption = ' MTB '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    object L_P01: TLabel
      Left = 24
      Top = 24
      Width = 32
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Modul:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object GB_Prj_vyst: TGroupBox
      Left = 56
      Top = 68
      Width = 201
      Height = 142
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = ' V'#253'stupy '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      object L_P04: TLabel
        Left = 17
        Top = 28
        Width = 92
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Nouzov'#233' otev'#345'en'#237' :'
      end
      object L_P05: TLabel
        Left = 17
        Top = 63
        Width = 71
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Zavr'#237't prejezd :'
      end
      object L_P06: TLabel
        Left = 17
        Top = 96
        Width = 38
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'V'#253'luka :'
      end
      object SE_vyst_open: TSpinEdit
        Left = 128
        Top = 27
        Width = 49
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        MaxValue = 15
        MinValue = 0
        ParentFont = False
        TabOrder = 0
        Value = 0
      end
      object SE_vyst_close: TSpinEdit
        Left = 128
        Top = 61
        Width = 49
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        MaxValue = 15
        MinValue = 0
        ParentFont = False
        TabOrder = 1
        Value = 0
      end
      object SE_vyst_vyluka: TSpinEdit
        Left = 128
        Top = 96
        Width = 49
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        MaxValue = 15
        MinValue = 0
        ParentFont = False
        TabOrder = 2
        Value = 0
      end
    end
    object GB_Prj_vst: TGroupBox
      Left = 56
      Top = 222
      Width = 201
      Height = 203
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = ' Vstupy '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      object L_P07: TLabel
        Left = 16
        Top = 32
        Width = 47
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Zav'#345'eno :'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object L_P08: TLabel
        Left = 17
        Top = 72
        Width = 51
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Otev'#345'eno :'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object L_P09: TLabel
        Left = 17
        Top = 112
        Width = 47
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'V'#253'straha :'
      end
      object L_P10: TLabel
        Left = 17
        Top = 152
        Width = 45
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Anulace :'
      end
      object SE_vst_close: TSpinEdit
        Left = 128
        Top = 32
        Width = 49
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        MaxValue = 15
        MinValue = 0
        ParentFont = False
        TabOrder = 0
        Value = 0
      end
      object SE_vst_open: TSpinEdit
        Left = 128
        Top = 72
        Width = 49
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        MaxValue = 15
        MinValue = 0
        ParentFont = False
        TabOrder = 1
        Value = 0
      end
      object SE_vst_vystraha: TSpinEdit
        Left = 128
        Top = 112
        Width = 49
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        MaxValue = 15
        MinValue = 0
        ParentFont = False
        TabOrder = 2
        Value = 0
      end
      object SE_vst_anulace: TSpinEdit
        Left = 128
        Top = 152
        Width = 49
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        MaxValue = 15
        MinValue = 0
        ParentFont = False
        TabOrder = 3
        Value = 0
      end
    end
    object SE_MTB: TSpinEdit
      Left = 176
      Top = 24
      Width = 73
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      MaxValue = 191
      MinValue = 1
      ParentFont = False
      TabOrder = 2
      Value = 1
    end
  end
end
