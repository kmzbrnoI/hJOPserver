object F_BlkCrossing: TF_BlkCrossing
  Left = 741
  Top = 209
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Prejezd : [Prejezd]'
  ClientHeight = 434
  ClientWidth = 651
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
    Left = 7
    Top = 400
    Width = 82
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Pou'#382#237't'
    Default = True
    TabOrder = 5
    OnClick = B_save_PClick
  end
  object B_Storno: TButton
    Left = 93
    Top = 400
    Width = 81
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Storno'
    TabOrder = 6
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
    Height = 285
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
      Height = 106
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = ' V'#253'stupy '
      TabOrder = 0
      object L_P04: TLabel
        Left = 15
        Top = 48
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
        Top = 23
        Width = 69
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Zav'#345#237't prejezd:'
      end
      object Label10: TLabel
        Left = 15
        Top = 74
        Width = 89
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Blokov'#225'n'#237' pozitivy:'
      end
      object SE_vyst_open_port: TSpinEdit
        Left = 208
        Top = 47
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
      end
      object SE_vyst_close_port: TSpinEdit
        Left = 208
        Top = 21
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
      object SE_vyst_open_board: TSpinEdit
        Left = 128
        Top = 47
        Width = 65
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        MaxValue = 191
        MinValue = 0
        TabOrder = 3
        Value = 0
        OnExit = SE_RCS_boardExit
      end
      object SE_vyst_close_board: TSpinEdit
        Left = 128
        Top = 21
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
      object CHB_RCS_NOT: TCheckBox
        Left = 108
        Top = 47
        Width = 16
        Height = 17
        TabOrder = 2
        OnClick = CHB_RCS_NOTClick
      end
      object CHB_RCS_BP: TCheckBox
        Left = 109
        Top = 74
        Width = 16
        Height = 17
        TabOrder = 5
        OnClick = CHB_RCS_BPClick
      end
      object SE_vyst_bp_board: TSpinEdit
        Left = 128
        Top = 73
        Width = 65
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        MaxValue = 191
        MinValue = 0
        TabOrder = 6
        Value = 0
        OnExit = SE_RCS_boardExit
      end
      object SE_vyst_bp_port: TSpinEdit
        Left = 208
        Top = 73
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
    end
    object GB_Prj_vst: TGroupBox
      Left = 14
      Top = 143
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
        TabOrder = 8
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
        TabOrder = 7
        Value = 0
        OnExit = SE_RCS_boardExit
      end
      object CHB_RCS_Anullation: TCheckBox
        Left = 108
        Top = 101
        Width = 16
        Height = 17
        TabOrder = 6
        OnClick = CHB_RCS_AnullationClick
      end
    end
  end
  object GB_JOP_control: TGroupBox
    Left = 336
    Top = 8
    Width = 307
    Height = 417
    Caption = ' P'#345'ejezd '#345#237'zen'#253' pouze z hJOP '
    TabOrder = 4
    object Label2: TLabel
      Left = 16
      Top = 42
      Width = 26
      Height = 13
      Caption = 'Kolej:'
    end
    object CHB_JOP_control: TCheckBox
      Left = 16
      Top = 19
      Width = 161
      Height = 17
      Caption = #218'seky p'#345'ejezdu '#345#237'zen'#233' z hJOP'
      TabOrder = 0
      OnClick = CHB_JOP_controlClick
    end
    object CB_Track: TComboBox
      Left = 48
      Top = 42
      Width = 177
      Height = 21
      Style = csDropDownList
      TabOrder = 1
      OnChange = CB_TrackChange
    end
    object B_Track_Delete: TButton
      Left = 231
      Top = 42
      Width = 59
      Height = 21
      Caption = 'Smazat'
      TabOrder = 2
      OnClick = B_Track_DeleteClick
    end
    object GB_Track: TGroupBox
      Left = 16
      Top = 69
      Width = 273
      Height = 340
      Caption = ' Kolej 1 '
      TabOrder = 3
      object Label3: TLabel
        Left = 16
        Top = 19
        Width = 107
        Height = 13
        Caption = 'Lev'#253' vzdalovac'#237' '#250'sek:'
      end
      object Label4: TLabel
        Left = 16
        Top = 65
        Width = 52
        Height = 13
        Caption = 'Lev'#253' '#250'sek:'
      end
      object Label5: TLabel
        Left = 16
        Top = 111
        Width = 128
        Height = 13
        Caption = 'Anula'#269'n'#237' (prost'#345'edn'#237') '#250'sek:'
      end
      object Label6: TLabel
        Left = 16
        Top = 158
        Width = 56
        Height = 13
        Caption = 'Prav'#253' '#250'sek:'
      end
      object Label7: TLabel
        Left = 16
        Top = 207
        Width = 111
        Height = 13
        Caption = 'Prav'#253' vzdalovac'#237' '#250'sek:'
      end
      object Label8: TLabel
        Left = 16
        Top = 253
        Width = 89
        Height = 13
        Caption = 'Otev'#345#237't p'#345'ejezd p'#345'i:'
      end
      object Label9: TLabel
        Left = 16
        Top = 303
        Width = 141
        Height = 13
        Caption = 'Mezn'#237' anula'#269'n'#237' doba [mm:ss]:'
      end
      object E_Track_Left_Out: TEdit
        Left = 16
        Top = 38
        Width = 241
        Height = 21
        TabOrder = 0
        Text = 'E_Track_Left_Out'
      end
      object E_Track_Left: TEdit
        Left = 16
        Top = 84
        Width = 241
        Height = 21
        TabOrder = 1
        Text = 'E_Track_Left'
      end
      object E_Track_Middle: TEdit
        Left = 16
        Top = 130
        Width = 241
        Height = 21
        TabOrder = 2
        Text = 'Edit2'
      end
      object E_Track_Right: TEdit
        Left = 16
        Top = 176
        Width = 241
        Height = 21
        TabOrder = 3
        Text = 'Edit2'
      end
      object E_Track_Right_Out: TEdit
        Left = 16
        Top = 224
        Width = 241
        Height = 21
        TabOrder = 4
        Text = 'Edit2'
      end
      object ME_Track_Anul_Time: TMaskEdit
        Left = 168
        Top = 304
        Width = 89
        Height = 24
        Hint = 'Zadejte aktu'#225'ln'#237' modelov'#253' cas'
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        EditMask = '!90:00;1;_'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        MaxLength = 5
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 5
        Text = '00:00'
      end
    end
    object CB_Track_Open: TComboBox
      Left = 32
      Top = 341
      Width = 241
      Height = 21
      Style = csDropDownList
      TabOrder = 4
      Items.Strings = (
        'Uvoln'#283'n'#237' prost'#345'edn'#237'ho '#250'seku'
        'Obsazen'#237' t'#345'et'#237'ho '#250'seku'
        'Uvoln'#283'n'#237' vzdalovac'#237'ho '#250'seku')
    end
  end
end
