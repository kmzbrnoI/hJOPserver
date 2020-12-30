object F_BlkGroupSignal: TF_BlkGroupSignal
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'F_BlkGroupSignal'
  ClientHeight = 409
  ClientWidth = 318
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
    Left = 148
    Top = 378
    Width = 76
    Height = 24
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Storno'
    TabOrder = 3
    OnClick = B_StornoClick
  end
  object B_Apply: TButton
    Left = 237
    Top = 378
    Width = 74
    Height = 24
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Pou'#382#237't'
    Default = True
    TabOrder = 4
    OnClick = B_ApplyClick
  end
  object GB_Signals: TGroupBox
    Left = 8
    Top = 123
    Width = 302
    Height = 238
    Caption = ' N'#225'v'#283'stidla '
    TabOrder = 5
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
      ExplicitTop = -62
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
      ExplicitTop = 217
      ExplicitWidth = 302
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
        ItemHeight = 13
        TabOrder = 0
      end
    end
  end
end
