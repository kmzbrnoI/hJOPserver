object F_Tester: TF_Tester
  Left = 849
  Top = 124
  BorderIcons = [biSystemMenu]
  Caption = 'RCS tester'
  ClientHeight = 465
  ClientWidth = 360
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
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  TextHeight = 13
  object L_Module: TLabel
    Left = 7
    Top = 40
    Width = 32
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Alignment = taCenter
    Caption = 'Modul:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object L_System: TLabel
    Left = 7
    Top = 13
    Width = 37
    Height = 13
    Caption = 'Syst'#233'm:'
  end
  object GB_Inputs: TGroupBox
    Left = 7
    Top = 65
    Width = 164
    Height = 390
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' Vstupy '
    Color = clBtnFace
    ParentBackground = False
    ParentColor = False
    TabOrder = 2
    object CB_AddrIn: TComboBox
      Left = 2
      Top = 15
      Width = 160
      Height = 21
      Align = alTop
      Style = csDropDownList
      TabOrder = 0
      OnChange = CB_AddrInChange
    end
    object P_Inputs: TPanel
      Left = 2
      Top = 36
      Width = 160
      Height = 352
      Align = alClient
      BevelOuter = bvNone
      ParentBackground = False
      TabOrder = 1
    end
  end
  object CB_Addr: TComboBox
    Left = 104
    Top = 40
    Width = 249
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Style = csDropDownList
    TabOrder = 1
    OnChange = CB_AddrChange
  end
  object GB_Outputs: TGroupBox
    Left = 175
    Top = 65
    Width = 178
    Height = 390
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' V'#253'stupy '
    Color = clBtnFace
    ParentBackground = False
    ParentColor = False
    TabOrder = 3
    object CB_AddrOut: TComboBox
      Left = 2
      Top = 15
      Width = 174
      Height = 21
      Align = alTop
      Style = csDropDownList
      TabOrder = 0
      OnChange = CB_AddrOutChange
    end
    object P_Outputs: TPanel
      Left = 2
      Top = 36
      Width = 174
      Height = 352
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
    end
  end
  object SE_System: TSpinEdit
    Left = 104
    Top = 13
    Width = 248
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 0
    Value = 0
    OnChange = SE_SystemChange
  end
end
