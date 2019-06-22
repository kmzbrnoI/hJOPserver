object F_MJCEdit: TF_MJCEdit
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Editovat slo'#382'enou j'#237'zdn'#237' cestu [...]'
  ClientHeight = 345
  ClientWidth = 689
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
  object L_VC_01: TLabel
    Left = 7
    Top = 31
    Width = 66
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'N'#225'zev cesty :'
  end
  object Label1: TLabel
    Left = 7
    Top = 6
    Width = 18
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'ID :'
  end
  object CHB_AutoName: TCheckBox
    Left = 88
    Top = 32
    Width = 43
    Height = 17
    Caption = 'auto'
    Checked = True
    State = cbChecked
    TabOrder = 2
  end
  object E_VCNazev: TEdit
    Left = 136
    Top = 32
    Width = 540
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    MaxLength = 32
    TabOrder = 1
  end
  object GB_JCs: TGroupBox
    Left = 7
    Top = 57
    Width = 329
    Height = 250
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' J'#237'zdn'#237' cesty  '
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    object GB_JC_New: TGroupBox
      Left = 2
      Top = 195
      Width = 325
      Height = 53
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alBottom
      Caption = ' Nov'#225' JC  '
      TabOrder = 1
      object CB_JC_Add: TComboBox
        Left = 16
        Top = 16
        Width = 217
        Height = 21
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 0
      end
      object B_JC_Add: TButton
        Left = 245
        Top = 16
        Width = 69
        Height = 25
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'P'#345'idat'
        TabOrder = 1
        OnClick = B_JC_AddClick
      end
    end
    object LV_JCs: TListView
      Left = 2
      Top = 56
      Width = 325
      Height = 139
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alBottom
      Columns = <
        item
          Caption = 'JC'
          Width = 250
        end>
      GridLines = True
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnChange = LV_JCsChange
    end
    object B_JC_Remove: TButton
      Left = 112
      Top = 18
      Width = 105
      Height = 25
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Smazat JC'
      Enabled = False
      TabOrder = 2
      OnClick = B_JC_RemoveClick
    end
  end
  object GB_VB: TGroupBox
    Left = 349
    Top = 57
    Width = 329
    Height = 250
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' Variantn'#237' body '
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
    object GroupBox2: TGroupBox
      Left = 2
      Top = 195
      Width = 325
      Height = 53
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alBottom
      Caption = ' Nov'#253' VB  '
      TabOrder = 1
      object CB_VB_New: TComboBox
        Left = 16
        Top = 16
        Width = 217
        Height = 21
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 0
      end
      object B_VB_New: TButton
        Left = 246
        Top = 16
        Width = 69
        Height = 25
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'P'#345'idat'
        TabOrder = 1
        OnClick = B_VB_NewClick
      end
    end
    object LV_VBs: TListView
      Left = 2
      Top = 56
      Width = 325
      Height = 139
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alBottom
      Columns = <
        item
          Caption = 'Blok'
          Width = 250
        end>
      GridLines = True
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnChange = LV_VBsChange
    end
    object B_VB_Remove: TButton
      Left = 112
      Top = 18
      Width = 105
      Height = 25
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Smazat VB'
      Enabled = False
      TabOrder = 2
      OnClick = B_VB_RemoveClick
    end
  end
  object B_Save: TButton
    Left = 601
    Top = 312
    Width = 75
    Height = 25
    Caption = 'Pou'#382#237't'
    Default = True
    TabOrder = 5
    OnClick = B_SaveClick
  end
  object B_Storno: TButton
    Left = 520
    Top = 312
    Width = 75
    Height = 25
    Caption = 'Storno'
    TabOrder = 6
    OnClick = B_StornoClick
  end
  object SE_ID: TSpinEdit
    Left = 137
    Top = 5
    Width = 539
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 0
    Value = 0
  end
end
