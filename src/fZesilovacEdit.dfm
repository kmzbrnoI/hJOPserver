object F_ZesilovacEdit: TF_ZesilovacEdit
  Left = 918
  Top = 121
  ActiveControl = B_Save
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Zesilovac [zesilovac]'
  ClientHeight = 417
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
    Left = 8
    Top = 8
    Width = 37
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'N'#225'zev :'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object E_Nazev: TEdit
    Left = 96
    Top = 8
    Width = 177
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    MaxLength = 32
    TabOrder = 0
  end
  object RG_Typ: TRadioGroup
    Left = 8
    Top = 40
    Width = 265
    Height = 65
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' Typ '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Items.Strings = (
      'SPAX')
    ParentFont = False
    TabOrder = 1
  end
  object B_Save: TButton
    Left = 207
    Top = 386
    Width = 67
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Pou'#382#237't'
    Default = True
    TabOrder = 4
    OnClick = B_SaveClick
  end
  object GB_Zkrat: TGroupBox
    Left = 8
    Top = 112
    Width = 265
    Height = 81
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' Detekce zkratu '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    object L_Zkrat_Port: TLabel
      Left = 8
      Top = 44
      Width = 50
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'MTB port :'
    end
    object L_Zkrat_MTB: TLabel
      Left = 8
      Top = 20
      Width = 61
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'MTB deska :'
    end
    object SE_Zkrat_Port: TSpinEdit
      Left = 168
      Top = 48
      Width = 89
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      MaxValue = 15
      MinValue = 0
      ParentFont = False
      TabOrder = 1
      Value = 0
    end
    object SE_Zkrat_MTB: TSpinEdit
      Left = 168
      Top = 22
      Width = 89
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      MaxValue = 191
      MinValue = 1
      ParentFont = False
      TabOrder = 0
      Value = 1
    end
  end
  object GB_Napajeni: TGroupBox
    Left = 8
    Top = 200
    Width = 265
    Height = 81
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' Detekce nap'#225'jen'#237' '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    object L_Napajeni_Port: TLabel
      Left = 8
      Top = 44
      Width = 50
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'MTB port :'
    end
    object L_Napajeni_MTB: TLabel
      Left = 8
      Top = 20
      Width = 61
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'MTB deska :'
    end
    object SE_Napajeni_Port: TSpinEdit
      Left = 168
      Top = 48
      Width = 89
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      MaxValue = 15
      MinValue = 0
      ParentFont = False
      TabOrder = 1
      Value = 0
    end
    object SE_Napajeni_MTB: TSpinEdit
      Left = 168
      Top = 22
      Width = 89
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      MaxValue = 191
      MinValue = 1
      ParentFont = False
      TabOrder = 0
      Value = 1
    end
  end
  object B_Storno: TButton
    Left = 127
    Top = 386
    Width = 67
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Storno'
    TabOrder = 5
    OnClick = B_StornoClick
  end
  object GB_DCC: TGroupBox
    Left = 9
    Top = 288
    Width = 265
    Height = 94
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' Detekce DCC '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
    object Label1: TLabel
      Left = 7
      Top = 59
      Width = 50
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'MTB port :'
    end
    object Label2: TLabel
      Left = 7
      Top = 35
      Width = 61
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'MTB deska :'
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
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      MaxValue = 15
      MinValue = 0
      ParentFont = False
      TabOrder = 1
      Value = 0
    end
    object SE_DCC_MTB: TSpinEdit
      Left = 167
      Top = 37
      Width = 89
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      MaxValue = 191
      MinValue = 1
      ParentFont = False
      TabOrder = 0
      Value = 1
    end
    object CHB_DCC: TCheckBox
      Left = 8
      Top = 16
      Width = 65
      Height = 17
      Caption = 'Povoleno'
      TabOrder = 2
      OnClick = CHB_DCCClick
    end
  end
end
