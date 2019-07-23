object F_BlkVystup: TF_BlkVystup
  Left = 869
  Top = 145
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Editovat data bloku [blok] (Logick'#253' v'#253'stup)'
  ClientHeight = 194
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
    Left = 10
    Top = 66
    Width = 304
    Height = 79
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' RCS '
    TabOrder = 2
    object L_IR04: TLabel
      Left = 16
      Top = 16
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
      Top = 44
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
  end
  object B_Storno: TButton
    Left = 151
    Top = 158
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
  object B_Save: TButton
    Left = 240
    Top = 158
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
end
