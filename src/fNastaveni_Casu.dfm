object F_ModCasSet: TF_ModCasSet
  Left = 1015
  Top = 147
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Modelov'#253' cas'
  ClientHeight = 234
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
  OnClose = FormClose
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
  object RG_zrychleni: TRadioGroup
    Left = 8
    Top = 64
    Width = 185
    Height = 121
    Hint = 'Zadejte zrychlen'#237' modelov'#233'ho casu'
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' Zrychlen'#237' '#269'asu  '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Items.Strings = (
      '2 x'
      '3 x'
      '4 x'
      '5 x'
      '6 x')
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
  end
  object B_OK: TButton
    Left = 118
    Top = 202
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
    Left = 38
    Top = 202
    Width = 75
    Height = 25
    Caption = 'Storno'
    TabOrder = 3
    OnClick = B_StornoClick
  end
end
