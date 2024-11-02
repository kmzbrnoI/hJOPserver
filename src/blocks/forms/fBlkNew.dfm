object F_BlkNew: TF_BlkNew
  Left = 487
  Top = 49
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Nov'#253' blok'
  ClientHeight = 329
  ClientWidth = 193
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  Position = poScreenCenter
  TextHeight = 13
  object RG_NewTyp: TRadioGroup
    Left = 7
    Top = 7
    Width = 177
    Height = 286
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' Typ nov'#233'ho bloku  '
    Items.Strings = (
      'Vyh'#253'bka'
      #218'sek'
      'IR'
      'N'#225'v'#283'stidlo'
      'Skupinov'#233' n'#225'v'#283'stidlo'
      'P'#345'ejezd'
      'Tra'#357
      'Z'#225'mek'
      'Rozpojova'#269
      'Tra'#357'ov'#253' '#250'sek'
      'IO'
      'Sou'#269'tov'#225' hl'#225'ska'
      'AC'
      'Pomocn'#233' stav'#283'dlo')
    TabOrder = 0
  end
  object B_Save: TButton
    Left = 111
    Top = 297
    Width = 75
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Vytvo'#345'it'
    Default = True
    TabOrder = 1
    OnClick = B_SaveClick
  end
end
