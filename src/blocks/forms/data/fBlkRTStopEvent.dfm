object F_BlkRTStopEvent: TF_BlkRTStopEvent
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'F_BlkRTStopEvent'
  ClientHeight = 290
  ClientWidth = 354
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GB_DetekceZastaveni: TGroupBox
    Left = 7
    Top = 7
    Width = 165
    Height = 186
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' Detekce zastaven'#237'  '
    TabOrder = 0
    object P_Stop: TPanel
      Left = 2
      Top = 15
      Width = 161
      Height = 169
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitHeight = 162
    end
  end
  object GB_DetekceZpomalenii: TGroupBox
    Left = 179
    Top = 7
    Width = 165
    Height = 242
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' Detekce zpomalen'#237'  '
    TabOrder = 1
    object CHB_Zpomal: TCheckBox
      Left = 10
      Top = 20
      Width = 148
      Height = 17
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Zpomalit na rychlost [km/h]:'
      TabOrder = 0
      OnClick = CHB_ZpomalClick
    end
    object P_Zpom: TPanel
      Left = 2
      Top = 72
      Width = 161
      Height = 168
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
      ExplicitTop = 104
    end
    object SE_Slow_Speed: TSpinEdit
      Left = 11
      Top = 42
      Width = 146
      Height = 22
      MaxValue = 200
      MinValue = 0
      TabOrder = 1
      Value = 0
    end
  end
end
