object F_BlkPst: TF_BlkPst
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'F_BlkPst'
  ClientHeight = 449
  ClientWidth = 990
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 8
    Top = 40
    Width = 15
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'ID:'
  end
  object Label1: TLabel
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
  object E_Name: TEdit
    Left = 120
    Top = 7
    Width = 193
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    MaxLength = 32
    TabOrder = 0
  end
  object SE_ID: TSpinEdit
    Left = 120
    Top = 40
    Width = 193
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
  object B_Storno: TButton
    Left = 94
    Top = 417
    Width = 76
    Height = 24
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Storno'
    TabOrder = 7
    OnClick = B_StornoClick
  end
  object B_Apply: TButton
    Left = 7
    Top = 417
    Width = 74
    Height = 24
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Pou'#382#237't'
    Default = True
    TabOrder = 8
    OnClick = B_ApplyClick
  end
  object GB_RCS: TGroupBox
    Left = 8
    Top = 67
    Width = 303
    Height = 182
    Caption = ' RCS '
    TabOrder = 2
    object Label4: TLabel
      Left = 7
      Top = 104
      Width = 126
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Indikace p'#345'evzet'#237' obsluhy:'
    end
    object Label5: TLabel
      Left = 7
      Top = 40
      Width = 122
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Tla'#269#237'tko p'#345'evzet'#237' obsluhy:'
    end
    object Label6: TLabel
      Left = 229
      Top = 16
      Width = 20
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Port'
    end
    object Label7: TLabel
      Left = 150
      Top = 16
      Width = 28
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Modul'
    end
    object Label8: TLabel
      Left = 7
      Top = 72
      Width = 125
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Tla'#269#237'tko ukon'#269'en'#237' obsluhy:'
    end
    object Label9: TLabel
      Left = 7
      Top = 136
      Width = 50
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Houka'#269'ka:'
    end
    object SE_RCS_Take_Module: TSpinEdit
      Left = 150
      Top = 40
      Width = 67
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      MaxValue = 0
      MinValue = 0
      TabOrder = 0
      Value = 1
    end
    object SE_RCS_Indication_Module: TSpinEdit
      Left = 150
      Top = 104
      Width = 67
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      MaxValue = 0
      MinValue = 0
      TabOrder = 4
      Value = 1
    end
    object SE_RCS_Indication_Port: TSpinEdit
      Left = 226
      Top = 104
      Width = 67
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
    object SE_RCS_Take_Port: TSpinEdit
      Left = 226
      Top = 40
      Width = 67
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
    object SE_RCS_Release_Module: TSpinEdit
      Left = 150
      Top = 72
      Width = 67
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      MaxValue = 0
      MinValue = 0
      TabOrder = 2
      Value = 1
    end
    object SE_RCS_Release_Port: TSpinEdit
      Left = 226
      Top = 72
      Width = 67
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
    object SE_RCS_Horn_Port: TSpinEdit
      Left = 226
      Top = 136
      Width = 67
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
    object SE_RCS_Horn_Module: TSpinEdit
      Left = 150
      Top = 136
      Width = 67
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      MaxValue = 0
      MinValue = 0
      TabOrder = 6
      Value = 1
    end
  end
  object GB_Tracks: TGroupBox
    Left = 317
    Top = 7
    Width = 329
    Height = 225
    Hint = 'Zadavejte useky od navestidla smerem k poslednimu bloku (vcetne)'
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' '#218'seky '
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    object GB_TrackEdit: TGroupBox
      Left = 2
      Top = 174
      Width = 325
      Height = 49
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alBottom
      Caption = ' P'#345'idat/upravit '#250'sek '
      TabOrder = 1
      ExplicitTop = 192
      object CB_Track: TComboBox
        Left = 10
        Top = 16
        Width = 159
        Height = 21
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Style = csDropDownList
        TabOrder = 0
      end
      object B_Track_Ok: TButton
        Left = 173
        Top = 16
        Width = 53
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Ok'
        TabOrder = 1
        OnClick = B_Track_OkClick
      end
      object B_Track_Del: TButton
        Left = 230
        Top = 16
        Width = 89
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Smazat '#250'sek/y'
        Enabled = False
        TabOrder = 2
        OnClick = B_Track_DelClick
      end
    end
    object LV_Tracks: TListView
      Left = 2
      Top = 15
      Width = 325
      Height = 159
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alClient
      Columns = <
        item
          Caption = '#'
          Width = 20
        end
        item
          Caption = 'ID'
        end
        item
          Caption = #218'sek'
          Width = 220
        end>
      GridLines = True
      MultiSelect = True
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnChange = LV_TracksChange
      ExplicitTop = 53
      ExplicitHeight = 139
    end
  end
  object GB_Turnouts: TGroupBox
    Left = 318
    Top = 236
    Width = 328
    Height = 205
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' V'#253'hybky '
    TabOrder = 4
    object LV_Turnouts: TListView
      Left = 2
      Top = 15
      Width = 324
      Height = 137
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alClient
      Columns = <
        item
          Caption = '#'
          Width = 20
        end
        item
          Caption = 'ID'
        end
        item
          Caption = 'V'#253'hybka'
          Width = 220
        end>
      GridLines = True
      MultiSelect = True
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnChange = LV_TurnoutsChange
      ExplicitTop = 52
      ExplicitWidth = 326
      ExplicitHeight = 134
    end
    object GB_TurnoutEdit: TGroupBox
      Left = 2
      Top = 152
      Width = 324
      Height = 51
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alBottom
      Caption = ' P'#345'idat/upravit v'#253'hybku '
      TabOrder = 1
      ExplicitTop = 208
      ExplicitWidth = 326
      object CB_Turnout: TComboBox
        Left = 10
        Top = 16
        Width = 151
        Height = 21
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Style = csDropDownList
        TabOrder = 0
      end
      object B_Turnout_Ok: TButton
        Left = 165
        Top = 15
        Width = 41
        Height = 23
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Ok'
        TabOrder = 1
        OnClick = B_Turnout_OkClick
      end
      object B_Turnout_Del: TButton
        Left = 210
        Top = 15
        Width = 109
        Height = 23
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Smazat v'#253'hybku/y'
        Enabled = False
        TabOrder = 2
        OnClick = B_Turnout_DelClick
      end
    end
  end
  object GB_Refugees: TGroupBox
    Left = 650
    Top = 7
    Width = 329
    Height = 225
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' Odvraty '
    TabOrder = 5
    object LV_Refugees: TListView
      Left = 2
      Top = 15
      Width = 325
      Height = 137
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alClient
      Columns = <
        item
          Caption = '#'
          Width = 20
        end
        item
          Caption = 'ID'
        end
        item
          Caption = 'Odvrat'
          Width = 170
        end
        item
          Alignment = taCenter
          Caption = 'Poloha'
        end>
      GridLines = True
      MultiSelect = True
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnChange = LV_RefugeesChange
      ExplicitTop = 54
      ExplicitWidth = 261
      ExplicitHeight = 134
    end
    object GB_RefEdit: TGroupBox
      Left = 2
      Top = 152
      Width = 325
      Height = 71
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alBottom
      Caption = ' P'#345'idat/upravit odvrat '
      TabOrder = 1
      ExplicitTop = 188
      ExplicitWidth = 261
      object Label10: TLabel
        Left = 8
        Top = 16
        Width = 26
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Blok :'
      end
      object Label11: TLabel
        Left = 8
        Top = 40
        Width = 39
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Poloha :'
      end
      object CB_Ref_Block: TComboBox
        Left = 56
        Top = 16
        Width = 153
        Height = 21
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Style = csDropDownList
        TabOrder = 0
      end
      object CB_Ref_Pos: TComboBox
        Left = 56
        Top = 40
        Width = 153
        Height = 21
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Style = csDropDownList
        TabOrder = 1
        Items.Strings = (
          '+'
          '-')
      end
      object B_Ref_Ok: TButton
        Left = 213
        Top = 39
        Width = 49
        Height = 23
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Ok'
        TabOrder = 2
        OnClick = B_Ref_OkClick
      end
      object B_Ref_Del: TButton
        Left = 213
        Top = 15
        Width = 105
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Smazat odvrat/y'
        Enabled = False
        TabOrder = 3
        OnClick = B_Ref_DelClick
      end
    end
  end
  object GB_Signals: TGroupBox
    Left = 650
    Top = 236
    Width = 329
    Height = 205
    Hint = 'Zadavejte useky od navestidla smerem k poslednimu bloku (vcetne)'
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' N'#225'v'#283'stidla '
    ParentShowHint = False
    ShowHint = True
    TabOrder = 6
    object GB_Signal_Edit: TGroupBox
      Left = 2
      Top = 154
      Width = 325
      Height = 49
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alBottom
      Caption = ' P'#345'idat/upravit n'#225'v'#283'stidlo '
      TabOrder = 1
      ExplicitTop = 192
      object CB_Signal: TComboBox
        Left = 10
        Top = 16
        Width = 185
        Height = 21
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Style = csDropDownList
        TabOrder = 0
      end
      object B_Signal_Ok: TButton
        Left = 199
        Top = 15
        Width = 53
        Height = 23
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Ok'
        TabOrder = 1
        OnClick = B_Signal_OkClick
      end
      object B_Signal_Del: TButton
        Left = 256
        Top = 15
        Width = 62
        Height = 23
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Smazat'
        Enabled = False
        TabOrder = 2
        OnClick = B_Signal_DelClick
      end
    end
    object LV_Signals: TListView
      Left = 2
      Top = 15
      Width = 325
      Height = 139
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alClient
      Columns = <
        item
          Caption = '#'
          Width = 20
        end
        item
          Caption = 'ID'
        end
        item
          Caption = 'N'#225'v'#283'stidlo'
          Width = 220
        end>
      GridLines = True
      MultiSelect = True
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnChange = LV_SignalsChange
      ExplicitTop = 53
    end
  end
end
