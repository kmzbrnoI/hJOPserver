object F_BlkDisconnector: TF_BlkDisconnector
  Left = 869
  Top = 145
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'F_BlkDisconnector'
  ClientHeight = 332
  ClientWidth = 321
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  Position = poScreenCenter
  OnClose = FormClose
  TextHeight = 13
  object Label2: TLabel
    Left = 8
    Top = 33
    Width = 14
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
  object E_name: TEdit
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
    Left = 121
    Top = 33
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
  object GB_RCS: TGroupBox
    Left = 7
    Top = 59
    Width = 307
    Height = 108
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' Ovl'#225'd'#225'n'#237' rozpojova'#269'e  '
    TabOrder = 2
    object Label3: TLabel
      Left = 16
      Top = 16
      Width = 60
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'RCS adresa:'
    end
    object Label4: TLabel
      Left = 16
      Top = 44
      Width = 46
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'RCS port:'
    end
    object Label5: TLabel
      Left = 16
      Top = 72
      Width = 74
      Height = 13
      Caption = 'V'#253'stupn'#237' sign'#225'l:'
    end
    object SE_port: TSpinEdit
      Left = 216
      Top = 43
      Width = 73
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
    object SE_module: TSpinEdit
      Left = 216
      Top = 17
      Width = 73
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      MaxValue = 0
      MinValue = 0
      TabOrder = 0
      Value = 1
      OnExit = SE_moduleExit
    end
    object CB_outputType: TComboBox
      Left = 144
      Top = 70
      Width = 145
      Height = 21
      Style = csDropDownList
      TabOrder = 2
      Items.Strings = (
        'Trvale sepnut'#253
        'Kmit'#225'n'#237' 1 Hz'
        'Kmit'#225'n'#237' 2 Hz'
        'Kmit'#225'n'#237' 3 Hz'
        'Kmit'#225'n'#237' 4 Hz'
        'Kmit'#225'n'#237' 5 Hz'
        'Kmit'#225'n'#237' 10 Hz'
        'Kmit'#225'n'#237' 33 tick/min'
        'Kmit'#225'n'#237' 66 tick/min')
    end
  end
  object B_Storno: TButton
    Left = 151
    Top = 302
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
  object B_Save: TButton
    Left = 240
    Top = 302
    Width = 74
    Height = 24
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Pou'#382#237't'
    Default = True
    TabOrder = 4
    OnClick = B_SaveClick
  end
  object GB_Indications: TGroupBox
    Left = 8
    Top = 172
    Width = 302
    Height = 125
    Caption = ' Voli'#269'e pro ru'#269'n'#237' ovl'#225'd'#225'n'#237' (v pultu)  '
    TabOrder = 3
    object Label6: TLabel
      Left = 7
      Top = 88
      Width = 26
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Voli'#269':'
    end
    object Label8: TLabel
      Left = 150
      Top = 71
      Width = 29
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Modul'
    end
    object Label9: TLabel
      Left = 229
      Top = 71
      Width = 19
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Port'
    end
    object SE_Cont_Module: TSpinEdit
      Left = 150
      Top = 88
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
      OnExit = SE_Cont_ModuleExit
    end
    object SE_Cont_Port: TSpinEdit
      Left = 226
      Top = 88
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
    object CHB_Contoller: TCheckBox
      Left = 7
      Top = 19
      Width = 54
      Height = 17
      Caption = 'Povolit'
      TabOrder = 0
      OnClick = CHB_ContollerClick
    end
    object CHB_Contoller_Pst: TCheckBox
      Left = 7
      Top = 42
      Width = 241
      Height = 17
      Caption = 'Aktivn'#237' pouze p'#345'i aktivn'#237'm pomocn'#233'm stav'#283'dle'
      TabOrder = 1
    end
  end
end
