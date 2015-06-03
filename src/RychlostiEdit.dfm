object F_RychlostiEdit: TF_RychlostiEdit
  Left = 977
  Top = 172
  BorderStyle = bsToolWindow
  Caption = 'Editovat stupen [stupen]'
  ClientHeight = 90
  ClientWidth = 287
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
  TextHeight = 16
  object Label1: TLabel
    Left = 10
    Top = 10
    Width = 100
    Height = 16
    Caption = 'Rychlost (km/h) :'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object SE_Rychlost: TSpinEdit
    Left = 130
    Top = 8
    Width = 149
    Height = 26
    MaxValue = 0
    MinValue = 0
    TabOrder = 0
    Value = 0
    OnKeyUp = SE_RychlostKeyUp
  end
  object B_Save: TButton
    Left = 187
    Top = 51
    Width = 92
    Height = 31
    Caption = 'Pou'#382#237't'
    TabOrder = 1
    OnClick = B_SaveClick
  end
  object B_Storno: TButton
    Left = 89
    Top = 51
    Width = 92
    Height = 31
    Caption = 'Storno'
    TabOrder = 2
    OnClick = B_StornoClick
  end
end
