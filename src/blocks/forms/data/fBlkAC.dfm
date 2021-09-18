object F_BlkAC: TF_BlkAC
  Left = 869
  Top = 145
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Upravit blok [blok] (AC)'
  ClientHeight = 168
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
  object Label2: TLabel
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
  object Label1: TLabel
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
  object Label4: TLabel
    Left = 7
    Top = 80
    Width = 84
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'P'#345#237'stupov'#253' token:'
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
    Left = 151
    Top = 134
    Width = 76
    Height = 24
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Storno'
    TabOrder = 4
    OnClick = B_StornoClick
  end
  object B_Save: TButton
    Left = 240
    Top = 134
    Width = 74
    Height = 24
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Pou'#382#237't'
    Default = True
    TabOrder = 3
    OnClick = B_SaveClick
  end
  object E_AccessToken: TEdit
    Left = 8
    Top = 98
    Width = 305
    Height = 21
    TabOrder = 2
  end
  object B_GenToken: TButton
    Left = 238
    Top = 75
    Width = 75
    Height = 20
    Caption = 'Vygenerovat'
    TabOrder = 5
    OnClick = B_GenTokenClick
  end
end
