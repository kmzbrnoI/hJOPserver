object F_Tester: TF_Tester
  Left = 849
  Top = 124
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'RCS tester'
  ClientHeight = 354
  ClientWidth = 492
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000088888888888888888888888888888880000
    00000000000000000000000000080F8888888888888888888888888888080F7F
    7F7F7F7F7F7F7F7F7F7F7F7F78080FF7F7F7F7F7700000F7F7F7F7F7F8080F7F
    7F7F7F7F7F88F07F7F7F7F7F78080FF7F7F7F7F7777777F7F7F7F7F7F8080F78
    88888888888888888888888878080FF8FFFFFFFFFFFFFFFFFFFFFFF8F8080F78
    F00000000000FFFFFFFFFFF878080FF8F00808800880FF0000FFFFF8F8080F78
    F00808800880FFFFFFFFFFF878080FF8F00008000080FF0000FFFFF8F8080F78
    F08880111000FFFFFFFFFFF878080FF8F00008999980FFFFFFFFFFF8F8080F78
    F088E8899880FF0000000FF878080FF8F088EE8898E0FFFFFFFFFFF8F8080F78
    F08EEEEE8EE0FF0000000FF878080FF8F00000000000FFFFFFFFFFF8F8080F78
    FFFFFFFFFFFFFFFFFFFFFFF878080FF8888888888888888888888888F8080F7F
    7F7F7F7F7F7F7F7F7F7F7F7F78080FFFFFFFFFFFFFFFFFFFFFFFFFFFFF080000
    00000000000000000000000000080CCCCCCCCCCCCCCCCCCCCCCC070707080000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000FFFF
    FFFFFFFFFFFFFFFFFFFF80000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000001FFFFFFFFFFFFFFFFFFFFFFFF}
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object L_1: TLabel
    Left = 48
    Top = 8
    Width = 63
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Alignment = taCenter
    Caption = 'RCS adresa :'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object GB_vstupy: TGroupBox
    Left = 7
    Top = 49
    Width = 73
    Height = 275
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' Vstupy '
    TabOrder = 0
  end
  object CB_RCSAdr: TComboBox
    Left = 8
    Top = 24
    Width = 153
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
    OnChange = CB_RCSAdrChange
  end
  object CHB_LogZmeny: TCheckBox
    Left = 7
    Top = 330
    Width = 97
    Height = 17
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Logovat zm'#283'ny'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
  end
  object GB_Change: TGroupBox
    Left = 176
    Top = 7
    Width = 305
    Height = 340
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' Zm'#283'ny '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    object LB_Changes: TListBox
      Left = 2
      Top = 15
      Width = 301
      Height = 290
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alTop
      Font.Charset = EASTEUROPE_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      ItemHeight = 14
      ParentFont = False
      TabOrder = 0
    end
    object B_Clear: TButton
      Left = 88
      Top = 314
      Width = 139
      Height = 17
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Smazat'
      TabOrder = 1
      OnClick = B_ClearClick
    end
  end
  object GB_vystupy: TGroupBox
    Left = 91
    Top = 49
    Width = 73
    Height = 275
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' V'#253'stupy '
    TabOrder = 4
  end
end
