object F_ZesilovacEdit: TF_ZesilovacEdit
  Left = 918
  Top = 121
  ActiveControl = B_Save
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Zesilovac [zesilovac]'
  ClientHeight = 473
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
  object L_Nazev: TLabel
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
  object E_Nazev: TEdit
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
  object RG_Typ: TRadioGroup
    Left = 9
    Top = 64
    Width = 265
    Height = 65
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' Typ '
    Items.Strings = (
      'SPAX')
    TabOrder = 2
  end
  object B_Save: TButton
    Left = 198
    Top = 439
    Width = 76
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Pou'#382#237't'
    Default = True
    TabOrder = 6
    OnClick = B_SaveClick
  end
  object GB_Zkrat: TGroupBox
    Left = 9
    Top = 136
    Width = 265
    Height = 100
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' Detekce zkratu '
    TabOrder = 3
    object L_Zkrat_Port: TLabel
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
    object L_Zkrat_module: TLabel
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
    object SE_Zkrat_Port: TSpinEdit
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
    object SE_Zkrat_module: TSpinEdit
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
    object CHB_Zkrat: TCheckBox
      Left = 8
      Top = 16
      Width = 65
      Height = 17
      Caption = 'Povoleno'
      TabOrder = 0
      OnClick = CHB_ZkratClick
    end
  end
  object GB_Napajeni: TGroupBox
    Left = 9
    Top = 240
    Width = 265
    Height = 97
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' Detekce v'#253'padku nap'#225'jen'#237' zesilova'#269'e '
    TabOrder = 4
    object L_Napajeni_Port: TLabel
      Left = 7
      Top = 60
      Width = 49
      Height = 14
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'RCS port :'
    end
    object L_Napajeni_module: TLabel
      Left = 7
      Top = 36
      Width = 60
      Height = 14
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'RCS deska :'
    end
    object SE_Napajeni_port: TSpinEdit
      Left = 167
      Top = 64
      Width = 89
      Height = 23
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      MaxValue = 0
      MinValue = 0
      TabOrder = 2
      Value = 0
    end
    object SE_Napajeni_module: TSpinEdit
      Left = 167
      Top = 38
      Width = 89
      Height = 23
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
    object CHB_Napajeni: TCheckBox
      Left = 8
      Top = 16
      Width = 65
      Height = 17
      Caption = 'Povoleno'
      TabOrder = 0
      OnClick = CHB_NapajeniClick
    end
  end
  object B_Storno: TButton
    Left = 120
    Top = 439
    Width = 74
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Storno'
    TabOrder = 7
    OnClick = B_StornoClick
  end
  object GB_DCC: TGroupBox
    Left = 9
    Top = 341
    Width = 265
    Height = 94
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' Detekce DCC na vstupu zesilova'#269'e '
    TabOrder = 5
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
