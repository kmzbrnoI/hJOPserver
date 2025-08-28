object F_Booster_Edit: TF_Booster_Edit
  Left = 918
  Top = 121
  ActiveControl = B_Save
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Zesilovac [zesilovac]'
  ClientHeight = 415
  ClientWidth = 322
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  TextHeight = 13
  object Label4: TLabel
    Left = 9
    Top = 32
    Width = 37
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'N'#225'zev :'
  end
  object Label3: TLabel
    Left = 10
    Top = 7
    Width = 17
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'ID :'
  end
  object Label7: TLabel
    Left = 27
    Top = 71
    Width = 57
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'RCS syst'#233'm'
  end
  object Label10: TLabel
    Left = 120
    Top = 71
    Width = 53
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'RCS modul'
  end
  object Label11: TLabel
    Left = 213
    Top = 71
    Width = 43
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'RCS port'
  end
  object E_Name: TEdit
    Left = 97
    Top = 32
    Width = 218
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    MaxLength = 32
    TabOrder = 1
  end
  object B_Save: TButton
    Left = 237
    Top = 383
    Width = 76
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Pou'#382#237't'
    Default = True
    TabOrder = 5
    OnClick = B_SaveClick
  end
  object GB_Short: TGroupBox
    Left = 9
    Top = 88
    Width = 304
    Height = 89
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' Detekce zkratu '
    TabOrder = 2
    object SE_Short_Port: TSpinEdit
      Left = 204
      Top = 38
      Width = 89
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
    object SE_Short_Module: TSpinEdit
      Left = 111
      Top = 38
      Width = 89
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
    object CHB_Short: TCheckBox
      Left = 8
      Top = 16
      Width = 65
      Height = 17
      Caption = 'Povoleno'
      TabOrder = 0
      OnClick = CHB_ShortClick
    end
    object CHB_Short_Reversed: TCheckBox
      Left = 8
      Top = 65
      Width = 248
      Height = 17
      Caption = 'Reverzn'#237' indikace (neaktivn'#237' port = zkrat)'
      TabOrder = 4
    end
    object SE_Short_System: TSpinEdit
      Left = 18
      Top = 38
      Width = 89
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      MaxValue = 0
      MinValue = 0
      TabOrder = 1
      Value = 1
    end
  end
  object GB_Power: TGroupBox
    Left = 9
    Top = 181
    Width = 304
    Height = 92
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' Detekce v'#253'padku nap'#225'jen'#237' zesilova'#269'e '
    TabOrder = 3
    object SE_Power_Port: TSpinEdit
      Left = 204
      Top = 38
      Width = 89
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
    object SE_Power_Module: TSpinEdit
      Left = 111
      Top = 38
      Width = 89
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
    object CHB_Power: TCheckBox
      Left = 8
      Top = 16
      Width = 65
      Height = 17
      Caption = 'Povoleno'
      TabOrder = 0
      OnClick = CHB_PowerClick
    end
    object CHB_Power_Reversed: TCheckBox
      Left = 8
      Top = 65
      Width = 248
      Height = 17
      Caption = 'Reverzn'#237' indikace (aktivn'#237' port = nap'#225'jen'#237' ok)'
      TabOrder = 4
    end
    object SE_Power_System: TSpinEdit
      Left = 18
      Top = 38
      Width = 89
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      MaxValue = 0
      MinValue = 0
      TabOrder = 1
      Value = 1
    end
  end
  object B_Storno: TButton
    Left = 159
    Top = 383
    Width = 74
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Storno'
    TabOrder = 6
    OnClick = B_StornoClick
  end
  object GB_DCC: TGroupBox
    Left = 9
    Top = 277
    Width = 304
    Height = 92
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' Detekce DCC na vstupu zesilova'#269'e '
    TabOrder = 4
    object SE_DCC_Port: TSpinEdit
      Left = 204
      Top = 38
      Width = 89
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
    object SE_DCC_Module: TSpinEdit
      Left = 111
      Top = 38
      Width = 89
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
    object CHB_DCC: TCheckBox
      Left = 8
      Top = 16
      Width = 65
      Height = 17
      Caption = 'Povoleno'
      TabOrder = 0
      OnClick = CHB_DCCClick
    end
    object CHB_DCC_Reversed: TCheckBox
      Left = 8
      Top = 65
      Width = 248
      Height = 17
      Caption = 'Reverzn'#237' indikace (neaktivn'#237' port = DCC ok)'
      TabOrder = 4
    end
    object SE_DCC_System: TSpinEdit
      Left = 18
      Top = 38
      Width = 89
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      MaxValue = 0
      MinValue = 0
      TabOrder = 1
      Value = 1
    end
  end
  object E_ID: TEdit
    Left = 97
    Top = 7
    Width = 218
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    MaxLength = 32
    TabOrder = 0
  end
end
