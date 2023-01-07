object F_BlkTurnoutState: TF_BlkTurnoutState
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Stav v'#253'hybky [blok]'
  ClientHeight = 194
  ClientWidth = 577
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label6: TLabel
    Left = 298
    Top = 84
    Width = 35
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'V'#253'luka:'
  end
  object Label1: TLabel
    Left = 298
    Top = 12
    Width = 31
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = #352't'#237'tek:'
  end
  object Label4: TLabel
    Left = 8
    Top = 12
    Width = 149
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Po'#269'et ud'#283'len'#253'ch nouz. z'#225'v'#283'r'#367':'
  end
  object Label3: TLabel
    Left = 7
    Top = 42
    Width = 64
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Po'#269'et z'#225'mk'#367':'
  end
  object B_Refresh: TButton
    Left = 414
    Top = 157
    Width = 76
    Height = 26
    Caption = 'Obnovit'
    TabOrder = 9
    OnClick = B_RefreshClick
  end
  object B_Apply: TButton
    Left = 496
    Top = 157
    Width = 75
    Height = 26
    Caption = 'Pou'#382#237't'
    Default = True
    TabOrder = 8
    OnClick = B_ApplyClick
  end
  object M_Lockout: TMemo
    Left = 296
    Top = 102
    Width = 275
    Height = 49
    Lines.Strings = (
      'M_Lockout')
    TabOrder = 7
  end
  object M_Note: TMemo
    Left = 296
    Top = 30
    Width = 275
    Height = 49
    Lines.Strings = (
      'M_Note')
    TabOrder = 6
  end
  object SE_Locks: TSpinEdit
    Left = 176
    Top = 40
    Width = 107
    Height = 22
    MaxValue = 0
    MinValue = 0
    ReadOnly = True
    TabOrder = 1
    Value = 0
  end
  object SE_Zaver: TSpinEdit
    Left = 176
    Top = 12
    Width = 107
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 0
    Value = 0
  end
  object B_Unlock: TButton
    Left = 176
    Top = 68
    Width = 107
    Height = 25
    Caption = 'Odemknout'
    TabOrder = 2
    OnClick = B_UnlockClick
  end
  object CHB_Moving_Plus: TCheckBox
    Left = 6
    Top = 91
    Width = 130
    Height = 17
    Caption = 'Stav'#283'n'#237' do polohy plus'
    TabOrder = 3
  end
  object CHB_Moving_Minus: TCheckBox
    Left = 6
    Top = 114
    Width = 142
    Height = 17
    Caption = 'Stav'#283'n'#237' do polohy m'#237'nus'
    TabOrder = 4
  end
  object CHB_Locked: TCheckBox
    Left = 6
    Top = 137
    Width = 161
    Height = 17
    Caption = 'RCS v'#253'stup dr'#382'en (zamknuto)'
    Enabled = False
    TabOrder = 5
  end
end
