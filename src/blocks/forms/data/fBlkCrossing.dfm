object F_BlkCrossing: TF_BlkCrossing
  Left = 741
  Top = 209
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Prejezd : [Prejezd]'
  ClientHeight = 554
  ClientWidth = 691
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
    Width = 34
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'N'#225'zev:'
  end
  object L_ID: TLabel
    Left = 9
    Top = 40
    Width = 11
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'ID'
  end
  object Label16: TLabel
    Left = 9
    Top = 72
    Width = 105
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'P'#345'edzv'#225'n'#283'c'#237' doba [s]:'
  end
  object E_Name: TEdit
    Left = 136
    Top = 7
    Width = 225
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
    Top = 520
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
    Top = 520
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
    Left = 136
    Top = 37
    Width = 225
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
  object GB_RCS: TGroupBox
    Left = 8
    Top = 96
    Width = 353
    Height = 419
    Caption = ' RCS '
    TabOrder = 3
    object L_P01: TLabel
      Left = 178
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
      Left = 256
      Top = 15
      Width = 19
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Port'
    end
    object GB_RCS_Out: TGroupBox
      Left = 14
      Top = 32
      Width = 323
      Height = 231
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = ' V'#253'stupy '
      TabOrder = 0
      object L_P04: TLabel
        Left = 10
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
        Left = 10
        Top = 23
        Width = 92
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Uzav'#345'en'#237' / blika'#269'e:'
      end
      object Label10: TLabel
        Left = 10
        Top = 73
        Width = 40
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Pozitiva:'
      end
      object Label12: TLabel
        Left = 10
        Top = 100
        Width = 100
        Height = 13
        Caption = 'Typ v'#253'stupu pozitiva:'
      end
      object Label13: TLabel
        Left = 10
        Top = 126
        Width = 59
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Z'#225'vory dol'#367':'
      end
      object Label14: TLabel
        Left = 10
        Top = 152
        Width = 72
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Z'#225'vory nahoru:'
      end
      object Label15: TLabel
        Left = 10
        Top = 178
        Width = 40
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Zvonek:'
      end
      object SE_out_open_port: TSpinEdit
        Left = 242
        Top = 47
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
      object SE_out_close_port: TSpinEdit
        Left = 242
        Top = 21
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
      end
      object SE_out_open_board: TSpinEdit
        Left = 162
        Top = 47
        Width = 65
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        MaxValue = 191
        MinValue = 0
        TabOrder = 4
        Value = 0
        OnExit = SE_RCS_boardExit
      end
      object SE_out_close_board: TSpinEdit
        Left = 162
        Top = 21
        Width = 65
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        MaxValue = 191
        MinValue = 0
        TabOrder = 1
        Value = 0
        OnExit = SE_RCS_boardExit
      end
      object CHB_RCS_NOT: TCheckBox
        Left = 143
        Top = 47
        Width = 16
        Height = 17
        TabOrder = 3
        OnClick = CHB_RCS_NOTClick
      end
      object CHB_RCS_Positive: TCheckBox
        Left = 143
        Top = 73
        Width = 16
        Height = 17
        TabOrder = 6
        OnClick = CHB_RCS_PositiveClick
      end
      object SE_out_positive_board: TSpinEdit
        Left = 162
        Top = 73
        Width = 65
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        MaxValue = 191
        MinValue = 0
        TabOrder = 7
        Value = 0
        OnExit = SE_RCS_boardExit
      end
      object SE_out_positive_port: TSpinEdit
        Left = 242
        Top = 73
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
      object CHB_RCS_Close: TCheckBox
        Left = 143
        Top = 22
        Width = 15
        Height = 17
        TabOrder = 0
        OnClick = CHB_RCS_CloseClick
      end
      object CB_Positive_Type: TComboBox
        Left = 162
        Top = 100
        Width = 145
        Height = 21
        Style = csDropDownList
        TabOrder = 9
        Items.Strings = (
          'standardn'#237
          'inverzn'#237
          'kmitav'#253)
      end
      object CHB_RCS_Barriers_Down: TCheckBox
        Left = 143
        Top = 126
        Width = 16
        Height = 17
        TabOrder = 10
        OnClick = CHB_RCS_Barriers_DownClick
      end
      object SE_out_barriers_down_board: TSpinEdit
        Left = 162
        Top = 126
        Width = 65
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        MaxValue = 191
        MinValue = 0
        TabOrder = 11
        Value = 0
        OnExit = SE_RCS_boardExit
      end
      object SE_out_barriers_down_port: TSpinEdit
        Left = 242
        Top = 126
        Width = 65
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        MaxValue = 0
        MinValue = 0
        TabOrder = 12
        Value = 0
      end
      object CHB_RCS_Barriers_Up: TCheckBox
        Left = 143
        Top = 152
        Width = 16
        Height = 17
        TabOrder = 13
        OnClick = CHB_RCS_Barriers_UpClick
      end
      object SE_out_barriers_up_board: TSpinEdit
        Left = 162
        Top = 152
        Width = 65
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        MaxValue = 191
        MinValue = 0
        TabOrder = 14
        Value = 0
        OnExit = SE_RCS_boardExit
      end
      object SE_out_barriers_up_port: TSpinEdit
        Left = 242
        Top = 152
        Width = 65
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        MaxValue = 0
        MinValue = 0
        TabOrder = 15
        Value = 0
      end
      object CHB_RCS_Ring: TCheckBox
        Left = 143
        Top = 178
        Width = 16
        Height = 17
        TabOrder = 16
        OnClick = CHB_RCS_RingClick
      end
      object SE_out_ring_board: TSpinEdit
        Left = 162
        Top = 178
        Width = 65
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        MaxValue = 191
        MinValue = 0
        TabOrder = 17
        Value = 0
        OnExit = SE_RCS_boardExit
      end
      object SE_out_ring_port: TSpinEdit
        Left = 242
        Top = 178
        Width = 65
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        MaxValue = 0
        MinValue = 0
        TabOrder = 18
        Value = 0
      end
      object CHB_Ring_Active_Down: TCheckBox
        Left = 13
        Top = 201
        Width = 180
        Height = 17
        Caption = 'Zvonek aktivn'#237' p'#345'i z'#225'vor'#225'ch dole'
        TabOrder = 19
      end
    end
    object GB_RCS_In: TGroupBox
      Left = 14
      Top = 267
      Width = 323
      Height = 134
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = ' Vstupy '
      TabOrder = 1
      object L_P07: TLabel
        Left = 10
        Top = 24
        Width = 109
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Zav'#345'eno / z'#225'vory dole:'
      end
      object L_P08: TLabel
        Left = 10
        Top = 50
        Width = 127
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Otev'#345'eno / z'#225'vory naho'#345'e:'
      end
      object L_P09: TLabel
        Left = 10
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
        Left = 10
        Top = 102
        Width = 42
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Anulace:'
      end
      object SE_in_close_port: TSpinEdit
        Left = 242
        Top = 24
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
      end
      object SE_in_open_port: TSpinEdit
        Left = 242
        Top = 50
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
      object SE_in_caution_port: TSpinEdit
        Left = 242
        Top = 76
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
      object SE_in_annulation_port: TSpinEdit
        Left = 242
        Top = 102
        Width = 65
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        MaxValue = 0
        MinValue = 0
        TabOrder = 11
        Value = 0
      end
      object SE_in_close_board: TSpinEdit
        Left = 162
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
        OnExit = SE_RCS_boardExit
      end
      object SE_in_open_board: TSpinEdit
        Left = 162
        Top = 50
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
      object SE_in_caution_board: TSpinEdit
        Left = 162
        Top = 76
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
      object SE_in_annulation_board: TSpinEdit
        Left = 162
        Top = 102
        Width = 65
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        MaxValue = 0
        MinValue = 0
        TabOrder = 10
        Value = 0
        OnExit = SE_RCS_boardExit
      end
      object CHB_RCS_Anullation: TCheckBox
        Left = 142
        Top = 101
        Width = 16
        Height = 17
        TabOrder = 9
        OnClick = CHB_RCS_AnullationClick
      end
      object CHB_RCS_Closed: TCheckBox
        Left = 142
        Top = 24
        Width = 16
        Height = 17
        TabOrder = 0
        OnClick = CHB_RCS_ClosedClick
      end
      object CHB_RCS_Open: TCheckBox
        Left = 142
        Top = 50
        Width = 16
        Height = 17
        TabOrder = 3
        OnClick = CHB_RCS_OpenClick
      end
      object CHB_RCS_Caution: TCheckBox
        Left = 142
        Top = 76
        Width = 16
        Height = 17
        TabOrder = 6
        OnClick = CHB_RCS_CautionClick
      end
    end
  end
  object GB_JOP_control: TGroupBox
    Left = 376
    Top = 9
    Width = 307
    Height = 472
    Caption = ' Uzav'#237'r'#225'n'#237' p'#345'ejezdu '#345#237'zen'#233' z hJOP '
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
      Height = 388
      Caption = 'check'
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
        Width = 158
        Height = 13
        Caption = 'Otev'#345#237't p'#345'ejezd ve sm'#283'ru L>R p'#345'i:'
      end
      object Label9: TLabel
        Left = 16
        Top = 351
        Width = 141
        Height = 13
        Caption = 'Mezn'#237' anula'#269'n'#237' doba [mm:ss]:'
      end
      object Label11: TLabel
        Left = 16
        Top = 299
        Width = 158
        Height = 13
        Caption = 'Otev'#345#237't p'#345'ejezd ve sm'#283'ru R>L p'#345'i:'
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
        Text = 'E_Track_Middle'
      end
      object E_Track_Right: TEdit
        Left = 16
        Top = 176
        Width = 241
        Height = 21
        TabOrder = 3
        Text = 'E_Track_Right'
      end
      object E_Track_Right_Out: TEdit
        Left = 16
        Top = 224
        Width = 241
        Height = 21
        TabOrder = 4
        Text = 'E_Track_Right_Out'
      end
      object ME_Track_Anul_Time: TMaskEdit
        Left = 168
        Top = 352
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
        TabOrder = 7
        Text = '00:00'
      end
      object CB_Track_Open_RL: TComboBox
        Left = 16
        Top = 318
        Width = 241
        Height = 21
        Style = csDropDownList
        TabOrder = 6
        Items.Strings = (
          'Uvoln'#283'n'#237' prost'#345'edn'#237'ho '#250'seku'
          'Uvoln'#283'n'#237' vzdalovac'#237'ho '#250'seku')
      end
      object CB_Track_Open_LR: TComboBox
        Left = 16
        Top = 272
        Width = 241
        Height = 21
        Style = csDropDownList
        TabOrder = 5
        Items.Strings = (
          'Uvoln'#283'n'#237' prost'#345'edn'#237'ho '#250'seku'
          'Uvoln'#283'n'#237' vzdalovac'#237'ho '#250'seku')
      end
    end
  end
  object SE_Prering_Time: TSpinEdit
    Left = 136
    Top = 69
    Width = 225
    Height = 22
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    MaxValue = 600
    MinValue = 0
    TabOrder = 2
    Value = 1
  end
end
