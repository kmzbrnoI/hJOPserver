object F_RV_Pom: TF_RV_Pom
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'POM'
  ClientHeight = 104
  ClientWidth = 210
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
    Width = 17
    Height = 13
    Caption = 'CV:'
  end
  object Label2: TLabel
    Left = 8
    Top = 40
    Width = 45
    Height = 13
    Caption = 'Hodnota:'
  end
  object SE_CV: TSpinEdit
    Left = 81
    Top = 8
    Width = 121
    Height = 22
    MaxValue = 1023
    MinValue = 1
    TabOrder = 0
    Value = 1
    OnKeyPress = SE_ValueKeyPress
  end
  object SE_Value: TSpinEdit
    Left = 80
    Top = 40
    Width = 121
    Height = 22
    MaxValue = 255
    MinValue = 0
    TabOrder = 1
    Value = 0
    OnKeyPress = SE_ValueKeyPress
  end
  object B_OK: TButton
    Left = 127
    Top = 71
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = B_OKClick
  end
  object B_Storno: TButton
    Left = 46
    Top = 71
    Width = 75
    Height = 25
    Caption = 'Storno'
    TabOrder = 3
    OnClick = B_StornoClick
  end
end
