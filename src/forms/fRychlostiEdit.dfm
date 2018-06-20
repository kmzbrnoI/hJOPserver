object F_RychlostiEdit: TF_RychlostiEdit
  Left = 977
  Top = 172
  BorderStyle = bsToolWindow
  Caption = 'Editovat stupen [stupen]'
  ClientHeight = 73
  ClientWidth = 233
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 81
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Rychlost (km/h) :'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object SE_Rychlost: TSpinEdit
    Left = 106
    Top = 7
    Width = 121
    Height = 26
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    MaxValue = 0
    MinValue = 0
    TabOrder = 0
    Value = 0
    OnKeyUp = SE_RychlostKeyUp
  end
  object B_Save: TButton
    Left = 152
    Top = 41
    Width = 75
    Height = 26
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Pou'#382#237't'
    TabOrder = 1
    OnClick = B_SaveClick
  end
  object B_Storno: TButton
    Left = 72
    Top = 41
    Width = 75
    Height = 26
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Storno'
    TabOrder = 2
    OnClick = B_StornoClick
  end
end
