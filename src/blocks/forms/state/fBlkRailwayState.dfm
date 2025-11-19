object F_BlkRailwayState: TF_BlkRailwayState
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'F_BlkRailwayState'
  ClientHeight = 185
  ClientWidth = 514
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object L_Usek24: TLabel
    Left = 7
    Top = 9
    Width = 28
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Sm'#283'r:'
  end
  object L_Usek20: TLabel
    Left = 7
    Top = 84
    Width = 92
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'P'#345'edpov'#237'dan'#253' vlak:'
  end
  object Label1: TLabel
    Left = 8
    Top = 107
    Width = 29
    Height = 13
    Caption = 'Vlaky:'
  end
  object Label2: TLabel
    Left = 296
    Top = 59
    Width = 23
    Height = 13
    Caption = 'Vlak:'
  end
  object Label3: TLabel
    Left = 296
    Top = 8
    Width = 89
    Height = 13
    Caption = 'Blokov'#225' podm'#237'nka:'
  end
  object L_BP: TLabel
    Left = 483
    Top = 9
    Width = 23
    Height = 13
    Alignment = taRightJustify
    Caption = 'L_BP'
  end
  object CB_Direction: TComboBox
    Left = 176
    Top = 9
    Width = 107
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Style = csDropDownList
    TabOrder = 0
    Items.Strings = (
      'disabled'
      #382#225'dn'#253
      'A->B'
      'B->A')
  end
  object B_Apply: TButton
    Left = 434
    Top = 154
    Width = 73
    Height = 24
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Pou'#382#237't'
    Default = True
    TabOrder = 10
    OnClick = B_ApplyClick
  end
  object B_Refresh: TButton
    Left = 354
    Top = 154
    Width = 76
    Height = 24
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Obnovit'
    TabOrder = 11
    OnClick = B_RefreshClick
  end
  object SE_Train: TSpinEdit
    Left = 176
    Top = 82
    Width = 107
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 3
    Value = 0
  end
  object E_Trains: TEdit
    Left = 8
    Top = 126
    Width = 275
    Height = 21
    ReadOnly = True
    TabOrder = 4
    Text = 'E_Trains'
  end
  object B_BP_Enable: TButton
    Left = 296
    Top = 28
    Width = 97
    Height = 25
    Caption = 'Zav'#233'st BP'
    TabOrder = 5
    OnClick = B_BP_EnableClick
  end
  object B_BP_Disable: TButton
    Left = 410
    Top = 28
    Width = 96
    Height = 25
    Caption = 'Zru'#353'it BP'
    TabOrder = 6
    OnClick = B_BP_DisableClick
  end
  object SE_Train_Add: TSpinEdit
    Left = 296
    Top = 76
    Width = 210
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 7
    Value = 0
  end
  object B_Train_Delete: TButton
    Left = 296
    Top = 104
    Width = 108
    Height = 21
    Caption = 'Smazat vlak'
    TabOrder = 8
    OnClick = B_Train_DeleteClick
  end
  object B_Train_Add: TButton
    Left = 410
    Top = 104
    Width = 96
    Height = 21
    Caption = 'P'#345'idat vlak'
    TabOrder = 9
    OnClick = B_Train_AddClick
  end
  object CHB_Zaver: TCheckBox
    Left = 8
    Top = 36
    Width = 50
    Height = 17
    Caption = 'Z'#225'v'#283'r'
    TabOrder = 1
  end
  object CHB_Request: TCheckBox
    Left = 8
    Top = 59
    Width = 145
    Height = 17
    Caption = #381#225'dost o tra'#357'ov'#253' souhlas'
    TabOrder = 2
  end
end
