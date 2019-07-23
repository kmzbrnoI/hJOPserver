object F_BlkPrejezd: TF_BlkPrejezd
  Left = 741
  Top = 209
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Prejezd : [Prejezd]'
  ClientHeight = 417
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
  object L_Name: TLabel
    Left = 8
    Top = 8
    Width = 81
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'N'#225'zev p'#345'ejezdu :'
  end
  object L_ID: TLabel
    Left = 9
    Top = 36
    Width = 11
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'ID'
  end
  object L_Station: TLabel
    Left = 7
    Top = 59
    Width = 42
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Stanice :'
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
    Top = 385
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
    Top = 385
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
    ItemHeight = 13
    TabOrder = 2
  end
  object GB_RCS: TGroupBox
    Left = 8
    Top = 110
    Width = 321
    Height = 259
    Caption = ' RCS '
    TabOrder = 3
    object L_P01: TLabel
      Left = 144
      Top = 15
      Width = 29
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Modul'
    end
    object Label1: TLabel
      Left = 222
      Top = 15
      Width = 19
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Port'
    end
    object GB_Prj_vyst: TGroupBox
      Left = 14
      Top = 32
      Width = 291
      Height = 79
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = ' V'#253'stupy '
      TabOrder = 0
      object L_P04: TLabel
        Left = 15
        Top = 20
        Width = 89
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Nouzov'#233' otev'#345'en'#237':'
      end
      object L_P05: TLabel
        Left = 15
        Top = 47
        Width = 69
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Zav'#345#237't prejezd:'
      end
      object SE_vyst_open_port: TSpinEdit
        Left = 208
        Top = 19
        Width = 65
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
      object SE_vyst_close_port: TSpinEdit
        Left = 208
        Top = 45
        Width = 65
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        MaxValue = 0
        MinValue = 0
        TabOrder = 3
        Value = 0
      end
      object SE_vyst_open_board: TSpinEdit
        Left = 128
        Top = 19
        Width = 65
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        MaxValue = 191
        MinValue = 0
        TabOrder = 0
        Value = 0
        OnExit = SE_RCS_boardExit
      end
      object SE_vyst_close_board: TSpinEdit
        Left = 128
        Top = 45
        Width = 65
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        MaxValue = 191
        MinValue = 0
        TabOrder = 2
        Value = 0
        OnExit = SE_RCS_boardExit
      end
    end
    object GB_Prj_vst: TGroupBox
      Left = 14
      Top = 115
      Width = 291
      Height = 134
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = ' Vstupy '
      TabOrder = 1
      object L_P07: TLabel
        Left = 15
        Top = 24
        Width = 44
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Zav'#345'eno:'
      end
      object L_P08: TLabel
        Left = 15
        Top = 50
        Width = 48
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Otev'#345'eno:'
      end
      object L_P09: TLabel
        Left = 15
        Top = 76
        Width = 44
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'V'#253'straha:'
      end
      object L_P10: TLabel
        Left = 15
        Top = 102
        Width = 42
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Anulace:'
      end
      object SE_vst_close_port: TSpinEdit
        Left = 208
        Top = 24
        Width = 65
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
      object SE_vst_open_port: TSpinEdit
        Left = 208
        Top = 50
        Width = 65
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        MaxValue = 0
        MinValue = 0
        TabOrder = 3
        Value = 0
      end
      object SE_vst_vystraha_port: TSpinEdit
        Left = 208
        Top = 76
        Width = 65
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        MaxValue = 0
        MinValue = 0
        TabOrder = 5
        Value = 0
      end
      object SE_vst_anulace_port: TSpinEdit
        Left = 208
        Top = 102
        Width = 65
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        MaxValue = 0
        MinValue = 0
        TabOrder = 7
        Value = 0
      end
      object SE_vst_close_board: TSpinEdit
        Left = 128
        Top = 24
        Width = 65
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        MaxValue = 0
        MinValue = 0
        TabOrder = 0
        Value = 0
        OnExit = SE_RCS_boardExit
      end
      object SE_vst_open_board: TSpinEdit
        Left = 128
        Top = 50
        Width = 65
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        MaxValue = 0
        MinValue = 0
        TabOrder = 2
        Value = 0
        OnExit = SE_RCS_boardExit
      end
      object SE_vst_vystraha_board: TSpinEdit
        Left = 128
        Top = 76
        Width = 65
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        MaxValue = 0
        MinValue = 0
        TabOrder = 4
        Value = 0
        OnExit = SE_RCS_boardExit
      end
      object SE_vst_anulace_board: TSpinEdit
        Left = 128
        Top = 102
        Width = 65
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        MaxValue = 0
        MinValue = 0
        TabOrder = 6
        Value = 0
        OnExit = SE_RCS_boardExit
      end
    end
  end
end
