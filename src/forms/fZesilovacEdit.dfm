object F_Booster_Edit: TF_Booster_Edit
  Left = 918
  Top = 121
  ActiveControl = B_Save
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Zesilovac [zesilovac]'
  ClientHeight = 462
  ClientWidth = 281
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
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
  object E_Name: TEdit
    Left = 97
    Top = 32
    Width = 177
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    MaxLength = 32
    TabOrder = 1
  end
  object B_Save: TButton
    Left = 198
    Top = 430
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
    Top = 64
    Width = 265
    Height = 113
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' Detekce zkratu '
    TabOrder = 2
    object Label8: TLabel
      Left = 7
      Top = 60
      Width = 49
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'RCS port :'
    end
    object Label7: TLabel
      Left = 7
      Top = 38
      Width = 60
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'RCS deska :'
    end
    object SE_Short_Port: TSpinEdit
      Left = 167
      Top = 64
      Width = 89
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
    object SE_Short_Module: TSpinEdit
      Left = 167
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
      OnExit = SE_RCS_moduleExit
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
    object CHB_short_reversed: TCheckBox
      Left = 8
      Top = 88
      Width = 248
      Height = 17
      Caption = 'Reverzn'#237' indikace (neaktivn'#237' port = zkrat)'
      TabOrder = 3
    end
  end
  object GB_Power: TGroupBox
    Left = 9
    Top = 181
    Width = 265
    Height = 116
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' Detekce v'#253'padku nap'#225'jen'#237' zesilova'#269'e '
    TabOrder = 3
    object Label5: TLabel
      Left = 7
      Top = 60
      Width = 49
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'RCS port :'
    end
    object Label6: TLabel
      Left = 7
      Top = 36
      Width = 60
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'RCS deska :'
    end
    object SE_Power_Port: TSpinEdit
      Left = 167
      Top = 64
      Width = 89
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
    object SE_Power_Module: TSpinEdit
      Left = 167
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
      OnExit = SE_RCS_moduleExit
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
    object CHB_power_reversed: TCheckBox
      Left = 8
      Top = 91
      Width = 248
      Height = 17
      Caption = 'Reverzn'#237' indikace (aktivn'#237' port = nap'#225'jen'#237' ok)'
      TabOrder = 3
    end
  end
  object B_Storno: TButton
    Left = 120
    Top = 430
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
    Top = 301
    Width = 265
    Height = 116
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' Detekce DCC na vstupu zesilova'#269'e '
    TabOrder = 4
    object Label1: TLabel
      Left = 8
      Top = 63
      Width = 49
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'RCS port :'
    end
    object Label2: TLabel
      Left = 7
      Top = 35
      Width = 60
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'RCS deska :'
    end
    object SE_DCC_port: TSpinEdit
      Left = 167
      Top = 63
      Width = 89
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
    object SE_DCC_module: TSpinEdit
      Left = 167
      Top = 37
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
      OnExit = SE_RCS_moduleExit
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
    object CHB_dcc_reversed: TCheckBox
      Left = 8
      Top = 90
      Width = 248
      Height = 17
      Caption = 'Reverzn'#237' indikace (neaktivn'#237' port = DCC ok)'
      TabOrder = 3
    end
  end
  object E_ID: TEdit
    Left = 97
    Top = 7
    Width = 177
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    MaxLength = 32
    TabOrder = 0
  end
end
