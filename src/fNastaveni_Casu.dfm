object F_ModCasSet: TF_ModCasSet
  Left = 1015
  Top = 147
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Nastaven'#237' modelov'#233'ho '#269'asu'
  ClientHeight = 145
  ClientWidth = 202
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object L_time_start: TLabel
    Left = 7
    Top = 25
    Width = 21
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = #268'as:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label1: TLabel
    Left = 8
    Top = 64
    Width = 73
    Height = 13
    Caption = 'Zrychlen'#237' '#269'asu:'
  end
  object ME_start_time: TMaskEdit
    Left = 104
    Top = 12
    Width = 89
    Height = 45
    Hint = 'Zadejte aktu'#225'ln'#237' modelov'#253' cas'
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    EditMask = '!90:00;1;_'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -32
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MaxLength = 5
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    Text = '00:00'
    OnKeyPress = ME_start_timeKeyPress
  end
  object B_OK: TButton
    Left = 120
    Top = 114
    Width = 75
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Pou'#382#237't'
    Default = True
    TabOrder = 2
    OnClick = B_OKClick
  end
  object B_Storno: TButton
    Left = 40
    Top = 114
    Width = 75
    Height = 25
    Caption = 'Storno'
    TabOrder = 3
    OnClick = B_StornoClick
  end
  object ME_Nasobic: TMaskEdit
    Left = 104
    Top = 64
    Width = 88
    Height = 45
    EditMask = '!9.9;1;_'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -32
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MaxLength = 3
    ParentFont = False
    TabOrder = 1
    Text = ' . '
    OnKeyPress = ME_start_timeKeyPress
  end
end
