object F_HoukEvsUsek: TF_HoukEvsUsek
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Houkac'#237' ud'#225'losti '#250'seku'
  ClientHeight = 369
  ClientWidth = 377
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 131
    Height = 13
    Caption = 'Houkac'#237' ud'#225'losti lich'#253' sm'#283'r:'
  end
  object Label2: TLabel
    Left = 197
    Top = 8
    Width = 133
    Height = 13
    Caption = 'Houkac'#237' ud'#225'losti sud'#253' sm'#283'r:'
  end
  object B_Apply: TButton
    Left = 294
    Top = 336
    Width = 75
    Height = 25
    Caption = 'Pou'#382#237't'
    Default = True
    TabOrder = 2
    OnClick = B_ApplyClick
  end
  object B_Storno: TButton
    Left = 213
    Top = 336
    Width = 75
    Height = 25
    Caption = 'Storno'
    TabOrder = 3
    OnClick = B_StornoClick
  end
  object P_HoukL: TPanel
    Left = 8
    Top = 27
    Width = 172
    Height = 302
    BevelOuter = bvNone
    TabOrder = 0
  end
  object P_HoukS: TPanel
    Left = 197
    Top = 27
    Width = 172
    Height = 302
    BevelOuter = bvNone
    TabOrder = 1
  end
end
