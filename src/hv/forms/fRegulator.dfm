object F_DigiReg: TF_DigiReg
  Left = 599
  Top = 400
  BorderStyle = bsToolWindow
  Caption = 'Regul'#225'tor'
  ClientHeight = 156
  ClientWidth = 401
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label5: TLabel
    Left = 286
    Top = 57
    Width = 33
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Adresa'
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Label6: TLabel
    Left = 286
    Top = 73
    Width = 43
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'P'#345'evzato'
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Label7: TLabel
    Left = 286
    Top = 88
    Width = 34
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Stupe'#328
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Label8: TLabel
    Left = 286
    Top = 105
    Width = 41
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Rychlost'
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object L_address: TLabel
    Left = 349
    Top = 54
    Width = 24
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Alignment = taRightJustify
    Caption = '9999'
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object L_mine: TLabel
    Left = 361
    Top = 71
    Width = 12
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Alignment = taRightJustify
    Caption = 'ne'
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object L_stupen: TLabel
    Left = 361
    Top = 88
    Width = 12
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Alignment = taRightJustify
    Caption = '28'
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object L_speed: TLabel
    Left = 339
    Top = 105
    Width = 18
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Alignment = taRightJustify
    Caption = '120'
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Label1: TLabel
    Left = 361
    Top = 105
    Width = 25
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'km/h'
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object S_Status: TShape
    Left = 16
    Top = 106
    Width = 48
    Height = 18
    Brush.Color = clGray
    Pen.Color = clWhite
    OnMouseUp = S_StatusMouseUp
  end
  object L_ComStatus: TLabel
    Left = 232
    Top = 42
    Width = 75
    Height = 13
    Caption = 'L_ComStatus'
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object Label2: TLabel
    Left = 286
    Top = 122
    Width = 27
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'POM:'
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object L_POM: TLabel
    Left = 370
    Top = 122
    Width = 6
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Alignment = taRightJustify
    Caption = '?'
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object CHB_svetla: TCheckBox
    Left = 138
    Top = 39
    Width = 80
    Height = 21
    Hint = '0'
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'F0 (sv'#283'tla)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = CHB_svetlaClick
  end
  object CHB_f1: TCheckBox
    Tag = 1
    Left = 138
    Top = 57
    Width = 34
    Height = 17
    Hint = '1'
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'F1'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnClick = CHB_svetlaClick
  end
  object CHB_f2: TCheckBox
    Tag = 2
    Left = 138
    Top = 73
    Width = 34
    Height = 17
    Hint = '2'
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'F2'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    OnClick = CHB_svetlaClick
  end
  object CHB_f4: TCheckBox
    Tag = 4
    Left = 138
    Top = 105
    Width = 34
    Height = 17
    Hint = '4'
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'F4'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
    OnClick = CHB_svetlaClick
  end
  object CHB_f3: TCheckBox
    Tag = 3
    Left = 138
    Top = 89
    Width = 34
    Height = 17
    Hint = '3'
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'F3'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    OnClick = CHB_svetlaClick
  end
  object CHB_f5: TCheckBox
    Tag = 5
    Left = 186
    Top = 57
    Width = 32
    Height = 17
    Hint = '5'
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'F5'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
    OnClick = CHB_svetlaClick
  end
  object CHB_f6: TCheckBox
    Tag = 6
    Left = 186
    Top = 73
    Width = 32
    Height = 17
    Hint = '6'
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'F6'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 7
    OnClick = CHB_svetlaClick
  end
  object CHB_f8: TCheckBox
    Tag = 8
    Left = 186
    Top = 105
    Width = 32
    Height = 17
    Hint = '8'
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'F8'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 9
    OnClick = CHB_svetlaClick
  end
  object CHB_f7: TCheckBox
    Tag = 7
    Left = 186
    Top = 89
    Width = 32
    Height = 17
    Hint = '7'
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'F7'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 8
    OnClick = CHB_svetlaClick
  end
  object RG_Smer: TRadioGroup
    Left = 7
    Top = 44
    Width = 65
    Height = 57
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' Sm'#283'r '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Items.Strings = (
      'Vp'#345'ed'
      'Vzad')
    ParentFont = False
    TabOrder = 0
    OnClick = RG_SmerClick
  end
  object B_PrevzitLoko: TButton
    Left = 117
    Top = 126
    Width = 75
    Height = 24
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'P'#345'evz'#237't loko'
    TabOrder = 10
    OnClick = B_PrevzitLokoClick
  end
  object B_OdhlLoko: TButton
    Left = 196
    Top = 126
    Width = 75
    Height = 24
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Odpojit loko'
    Enabled = False
    TabOrder = 11
    OnClick = B_OdhlLokoClick
  end
  object B_STOP: TButton
    Left = 87
    Top = 50
    Width = 39
    Height = 30
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'STOP'
    TabOrder = 12
    OnClick = B_STOPClick
  end
  object CHB_DojezdIgnorate: TCheckBox
    Left = 7
    Top = 129
    Width = 97
    Height = 17
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Tot'#225'ln'#237' '#345#237'zen'#237
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 13
    OnClick = CHB_DojezdIgnorateClick
  end
  object CHB_f9: TCheckBox
    Tag = 9
    Left = 231
    Top = 57
    Width = 32
    Height = 17
    Hint = '5'
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'F9'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 14
    OnClick = CHB_svetlaClick
  end
  object CHB_f10: TCheckBox
    Tag = 10
    Left = 231
    Top = 73
    Width = 40
    Height = 17
    Hint = '6'
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'F10'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 15
    OnClick = CHB_svetlaClick
  end
  object CHB_f12: TCheckBox
    Tag = 12
    Left = 231
    Top = 105
    Width = 40
    Height = 17
    Hint = '8'
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'F12'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 16
    OnClick = CHB_svetlaClick
  end
  object CHB_f11: TCheckBox
    Tag = 11
    Left = 231
    Top = 89
    Width = 40
    Height = 17
    Hint = '7'
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'F11'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 17
    OnClick = CHB_svetlaClick
  end
  object B_Idle: TButton
    Left = 87
    Top = 85
    Width = 39
    Height = 30
    Caption = 'Idle'
    TabOrder = 18
    OnClick = B_IdleClick
  end
  object TB_reg: TTrackBar
    Left = 6
    Top = 7
    Width = 389
    Height = 33
    Hint = 'Urcuje rychlost lokomotivy'
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Align = alCustom
    Max = 28
    ParentShowHint = False
    PageSize = 1
    ShowHint = False
    TabOrder = 19
  end
  object T_Speed: TTimer
    Enabled = False
    Interval = 100
    OnTimer = T_SpeedTimer
    Left = 351
    Top = 12
  end
end
