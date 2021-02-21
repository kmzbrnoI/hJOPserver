object F_BlkGroupSignal: TF_BlkGroupSignal
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'F_BlkGroupSignal'
  ClientHeight = 297
  ClientWidth = 625
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object L_IR01: TLabel
    Left = 8
    Top = 8
    Width = 37
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'N'#225'zev :'
  end
  object L_IR02: TLabel
    Left = 8
    Top = 40
    Width = 11
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'ID'
  end
  object L_Usek03: TLabel
    Left = 7
    Top = 72
    Width = 42
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Stanice :'
  end
  object E_Name: TEdit
    Left = 120
    Top = 8
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
  object LB_Stations: TListBox
    Left = 119
    Top = 72
    Width = 192
    Height = 46
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    ItemHeight = 13
    TabOrder = 2
  end
  object B_Storno: TButton
    Left = 455
    Top = 261
    Width = 76
    Height = 24
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Storno'
    TabOrder = 6
    OnClick = B_StornoClick
  end
  object B_Apply: TButton
    Left = 544
    Top = 261
    Width = 74
    Height = 24
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Pou'#382#237't'
    Default = True
    TabOrder = 5
    OnClick = B_ApplyClick
  end
  object GB_Signals: TGroupBox
    Left = 318
    Top = 8
    Width = 302
    Height = 238
    Caption = ' N'#225'v'#283'stidla '
    TabOrder = 4
    object LV_Signals: TListView
      Left = 2
      Top = 59
      Width = 298
      Height = 127
      Align = alBottom
      Columns = <
        item
          Caption = 'ID'
        end
        item
          Caption = 'N'#225'zev'
          Width = 200
        end>
      MultiSelect = True
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnChange = LV_SignalsChange
    end
    object B_BlkDelete: TButton
      Left = 88
      Top = 21
      Width = 128
      Height = 25
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Smazat n'#225'v'#283'stidlo'
      Enabled = False
      TabOrder = 1
      OnClick = B_BlkDeleteClick
    end
    object GB_NewSignal: TGroupBox
      Left = 2
      Top = 186
      Width = 298
      Height = 50
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alBottom
      Caption = ' P'#345'idat nov'#233' n'#225'v'#283'stidlo '
      TabOrder = 2
      object B_BlkAdd: TButton
        Left = 227
        Top = 14
        Width = 64
        Height = 25
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'P'#345'idat'
        TabOrder = 1
        OnClick = B_BlkAddClick
      end
      object CB_NewSignal: TComboBox
        Left = 8
        Top = 16
        Width = 206
        Height = 21
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Style = csDropDownList
        TabOrder = 0
      end
    end
  end
  object GB_RCS: TGroupBox
    Left = 7
    Top = 122
    Width = 304
    Height = 147
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' Ovl'#225'd'#225'n'#237' n'#225'v'#283'stidla '
    TabOrder = 3
    object Label3: TLabel
      Left = 131
      Top = 38
      Width = 51
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'RCS modul'
    end
    object Label4: TLabel
      Left = 221
      Top = 38
      Width = 43
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'RCS port'
    end
    object Label5: TLabel
      Left = 7
      Top = 117
      Width = 64
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Typ v'#253'stupu:'
    end
    object Label6: TLabel
      Left = 8
      Top = 56
      Width = 64
      Height = 13
      Caption = 'Prvn'#237' v'#253'stup:'
    end
    object SE_RCSport1: TSpinEdit
      Left = 221
      Top = 55
      Width = 73
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
    object CB_Typ: TComboBox
      Left = 93
      Top = 117
      Width = 201
      Height = 21
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Style = csDropDownList
      TabOrder = 5
      Items.Strings = (
        'S-Com'
        'bin'#225'rn'#237' (0/1)')
    end
    object SE_RCSmodule1: TSpinEdit
      Left = 131
      Top = 55
      Width = 73
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      MaxValue = 0
      MinValue = 0
      TabOrder = 1
      Value = 1
      OnExit = SE_RCSmodule1Exit
    end
    object CHB_RCS_Output: TCheckBox
      Left = 8
      Top = 16
      Width = 201
      Height = 17
      Caption = 'N'#225'v'#283'stidlo m'#225' v'#253'stup na sb'#283'rnici RCS'
      TabOrder = 0
      OnClick = CHB_RCS_OutputClick
    end
    object SE_RCSmodule2: TSpinEdit
      Left = 131
      Top = 86
      Width = 73
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      MaxValue = 0
      MinValue = 0
      TabOrder = 2
      Value = 1
      OnExit = SE_RCSmodule2Exit
    end
    object SE_RCSport2: TSpinEdit
      Left = 221
      Top = 85
      Width = 73
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
    object CHB_RCS_Second_Output: TCheckBox
      Left = 8
      Top = 85
      Width = 81
      Height = 17
      Caption = 'Druh'#253' v'#253'stup:'
      TabOrder = 6
      OnClick = CHB_RCS_Second_OutputClick
    end
  end
end
