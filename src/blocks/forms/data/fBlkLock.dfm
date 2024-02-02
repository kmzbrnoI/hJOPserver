object F_BlkLock: TF_BlkLock
  Left = 869
  Top = 145
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'F_BlkLock'
  ClientHeight = 110
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
    Top = 78
    Width = 76
    Height = 24
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Storno'
    TabOrder = 2
    OnClick = B_StornoClick
  end
  object B_Save: TButton
    Left = 240
    Top = 78
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
end
