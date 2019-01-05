object F_BlkPrejezd: TF_BlkPrejezd
  Left = 741
  Top = 209
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Prejezd : [Prejezd]'
  ClientHeight = 529
  ClientWidth = 337
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
    Width = 81
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'N'#225'zev p'#345'ejezdu :'
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
    Left = 120
    Top = 8
    Width = 211
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    MaxLength = 32
    TabOrder = 0
  end
  object B_save_P: TButton
    Left = 248
    Top = 494
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
    Left = 152
    Top = 494
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
    Left = 120
    Top = 33
    Width = 211
    Height = 22
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    MaxValue = 2147483647
    MinValue = 1
    TabOrder = 1
    Value = 1
  end
  object LB_Stanice: TListBox
    Left = 120
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
  object GB_RCS: TGroupBox
    Left = 8
    Top = 110
    Width = 321
    Height = 379
    Caption = ' RCS '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    object L_P01: TLabel
      Left = 64
      Top = 16
      Width = 32
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Modul:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object GB_Prj_vyst: TGroupBox
      Left = 64
      Top = 42
      Width = 201
      Height = 103
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
      TabOrder = 1
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
        Width = 72
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Zav'#345#237't prejezd :'
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
    end
    object GB_Prj_vst: TGroupBox
      Left = 64
      Top = 156
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
      TabOrder = 2
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
        Left = 16
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
        Left = 16
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
        Left = 16
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
    object SE_RCS: TSpinEdit
      Left = 184
      Top = 16
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
      TabOrder = 0
      Value = 1
    end
  end
end
