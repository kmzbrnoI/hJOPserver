object F_Pozadi: TF_Pozadi
  Left = 612
  Top = 148
  AlphaBlend = True
  AlphaBlendValue = 0
  BorderStyle = bsNone
  ClientHeight = 362
  ClientWidth = 530
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefault
  WindowState = wsMaximized
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object T_Ztmavovani: TTimer
    Enabled = False
    Interval = 10
    OnTimer = T_ZtmavovaniTimer
    Left = 8
    Top = 8
  end
end
