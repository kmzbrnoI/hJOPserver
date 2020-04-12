object F_BlkVystup: TF_BlkVystup
  Left = 869
  Top = 145
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Editovat data bloku [blok] (Logick'#253' v'#253'stup)'
  ClientHeight = 266
  ClientWidth = 321
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
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
  object E_Nazev: TEdit
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
  object GB_RCS: TGroupBox
    Left = 7
    Top = 132
    Width = 304
    Height = 98
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' RCS '
    TabOrder = 5
    object L_IR04: TLabel
      Left = 16
      Top = 40
      Width = 63
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'RCS adresa :'
    end
    object L_IR05: TLabel
      Left = 16
      Top = 68
      Width = 49
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'RCS port :'
    end
    object SE_port: TSpinEdit
      Left = 216
      Top = 67
      Width = 73
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
    object SE_module: TSpinEdit
      Left = 216
      Top = 41
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
      OnExit = SE_moduleExit
    end
    object CHB_RCS_Enabled: TCheckBox
      Left = 16
      Top = 18
      Width = 169
      Height = 17
      Caption = 'Blok m'#225' v'#253'stup na sb'#283'rnici RCS'
      TabOrder = 0
      OnClick = CHB_RCS_EnabledClick
    end
  end
  object B_Storno: TButton
    Left = 148
    Top = 234
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
  object B_Save: TButton
    Left = 237
    Top = 234
    Width = 74
    Height = 24
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Pou'#382#237't'
    Default = True
    TabOrder = 7
    OnClick = B_SaveClick
  end
  object CHB_Activate_On_Start: TCheckBox
    Left = 8
    Top = 74
    Width = 113
    Height = 17
    Caption = 'Aktivovat po startu'
    TabOrder = 2
  end
  object CHB_Nullable: TCheckBox
    Left = 8
    Top = 97
    Width = 216
    Height = 17
    Caption = 'Po zapnut'#237' automaticky deaktivovat po [s]'
    TabOrder = 3
    OnClick = CHB_NullableClick
  end
  object SE_Null_Time: TSpinEdit
    Left = 237
    Top = 97
    Width = 76
    Height = 22
    MaxValue = 360
    MinValue = 1
    TabOrder = 4
    Value = 0
  end
end
