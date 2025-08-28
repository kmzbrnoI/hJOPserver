object F_BlkGroupSignal: TF_BlkGroupSignal
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'F_BlkGroupSignal'
  ClientHeight = 252
  ClientWidth = 625
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
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
  object B_Storno: TButton
    Left = 85
    Top = 222
    Width = 76
    Height = 24
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Storno'
    TabOrder = 5
    OnClick = B_StornoClick
  end
  object B_Apply: TButton
    Left = 7
    Top = 222
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
    Left = 318
    Top = 8
    Width = 302
    Height = 238
    Caption = ' N'#225'v'#283'stidla '
    TabOrder = 3
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
      OnKeyDown = LV_SignalsKeyDown
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
    Top = 66
    Width = 304
    Height = 135
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' Ovl'#225'd'#225'n'#237' n'#225'v'#283'stidla '
    TabOrder = 2
    object Label3: TLabel
      Left = 170
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
      Left = 234
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
      Left = 8
      Top = 108
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
    object Label1: TLabel
      Left = 106
      Top = 38
      Width = 57
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'RCS syst'#233'm'
    end
    object SE_RCSport1: TSpinEdit
      Left = 234
      Top = 55
      Width = 60
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
    object CB_Typ: TComboBox
      Left = 106
      Top = 108
      Width = 188
      Height = 21
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Style = csDropDownList
      TabOrder = 8
      Items.Strings = (
        'S-Com'
        'bin'#225'rn'#237' (0/1)')
    end
    object SE_RCSmodule1: TSpinEdit
      Left = 170
      Top = 55
      Width = 60
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
      Left = 170
      Top = 82
      Width = 60
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      MaxValue = 0
      MinValue = 0
      TabOrder = 6
      Value = 0
    end
    object SE_RCSport2: TSpinEdit
      Left = 234
      Top = 81
      Width = 60
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
    object CHB_RCS_Second_Output: TCheckBox
      Left = 7
      Top = 82
      Width = 89
      Height = 17
      Caption = 'Druh'#253' v'#253'stup:'
      TabOrder = 4
      OnClick = CHB_RCS_Second_OutputClick
    end
    object SE_RCSsystem1: TSpinEdit
      Left = 106
      Top = 55
      Width = 60
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
    object SE_RCSsystem2: TSpinEdit
      Left = 106
      Top = 82
      Width = 60
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
  end
end
