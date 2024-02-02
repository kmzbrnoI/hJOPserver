object F_BlkTrack: TF_BlkTrack
  Left = 377
  Top = 212
  ActiveControl = B_OK
  BorderStyle = bsToolWindow
  Caption = 'F_BlkTrack'
  ClientHeight = 369
  ClientWidth = 409
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  Position = poScreenCenter
  OnClose = FormClose
  TextHeight = 13
  object Label2: TLabel
    Left = 8
    Top = 32
    Width = 14
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'ID:'
  end
  object Label1: TLabel
    Left = 8
    Top = 7
    Width = 34
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'N'#225'zev:'
  end
  object Label3: TLabel
    Left = 8
    Top = 58
    Width = 86
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'D'#233'lka '#250'seku [cm]:'
  end
  object Label5: TLabel
    Left = 8
    Top = 108
    Width = 75
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Smy'#269'kov'#253' blok:'
  end
  object Label4: TLabel
    Left = 8
    Top = 83
    Width = 49
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Zesilova'#269':'
  end
  object Label6: TLabel
    Left = 8
    Top = 129
    Width = 97
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Max. po'#269'et souprav:'
    Color = clBtnFace
    ParentColor = False
  end
  object B_OK: TButton
    Left = 327
    Top = 337
    Width = 75
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Pou'#382#237't'
    Default = True
    TabOrder = 9
    OnClick = B_OKClick
  end
  object B_Storno: TButton
    Left = 248
    Top = 337
    Width = 75
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Storno'
    TabOrder = 10
    OnClick = B_StornoClick
  end
  object SE_ID: TSpinEdit
    Left = 176
    Top = 32
    Width = 226
    Height = 22
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    MaxValue = 2147483647
    MinValue = 1
    TabOrder = 1
    Value = 1
  end
  object E_Name: TEdit
    Left = 176
    Top = 7
    Width = 226
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    MaxLength = 32
    TabOrder = 0
  end
  object GB_RCS: TGroupBox
    Left = 7
    Top = 182
    Width = 395
    Height = 139
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' Detekce obsazen'#237' - vstupy - RCS  '
    TabOrder = 8
    object L_det1: TLabel
      Left = 12
      Top = 32
      Width = 54
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = '0. detektor:'
    end
    object L_det2: TLabel
      Left = 12
      Top = 58
      Width = 54
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = '1. detektor:'
    end
    object L_det3: TLabel
      Left = 12
      Top = 84
      Width = 54
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = '2. detektor:'
    end
    object L_det4: TLabel
      Left = 12
      Top = 110
      Width = 54
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = '3. detektor:'
    end
    object L_P01: TLabel
      Left = 224
      Top = 16
      Width = 29
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Modul'
    end
    object Label7: TLabel
      Left = 312
      Top = 15
      Width = 19
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Port'
    end
    object SE_Port0: TSpinEdit
      Left = 312
      Top = 32
      Width = 73
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Enabled = False
      MaxValue = 0
      MinValue = 0
      TabOrder = 2
      Value = 0
    end
    object SE_Module0: TSpinEdit
      Left = 224
      Top = 32
      Width = 73
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Enabled = False
      MaxValue = 0
      MinValue = 0
      TabOrder = 1
      Value = 1
      OnExit = SE_RCS_BoardExit
    end
    object CHB_D0: TCheckBox
      Left = 188
      Top = 33
      Width = 17
      Height = 17
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      TabOrder = 0
      OnClick = CHB_D0Click
    end
    object CHB_D1: TCheckBox
      Tag = 1
      Left = 188
      Top = 59
      Width = 17
      Height = 17
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      TabOrder = 3
      OnClick = CHB_D0Click
    end
    object SE_Module1: TSpinEdit
      Left = 224
      Top = 58
      Width = 73
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Enabled = False
      MaxValue = 0
      MinValue = 0
      TabOrder = 4
      Value = 1
      OnExit = SE_RCS_BoardExit
    end
    object SE_Port1: TSpinEdit
      Left = 312
      Top = 58
      Width = 73
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Enabled = False
      MaxValue = 0
      MinValue = 0
      TabOrder = 5
      Value = 0
    end
    object CHB_D2: TCheckBox
      Tag = 2
      Left = 188
      Top = 86
      Width = 17
      Height = 17
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      TabOrder = 6
      OnClick = CHB_D0Click
    end
    object SE_Module2: TSpinEdit
      Left = 224
      Top = 84
      Width = 73
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Enabled = False
      MaxValue = 0
      MinValue = 0
      TabOrder = 7
      Value = 1
      OnExit = SE_RCS_BoardExit
    end
    object SE_Port2: TSpinEdit
      Left = 312
      Top = 84
      Width = 73
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Enabled = False
      MaxValue = 0
      MinValue = 0
      TabOrder = 8
      Value = 0
    end
    object CHB_D3: TCheckBox
      Tag = 3
      Left = 188
      Top = 112
      Width = 17
      Height = 17
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      TabOrder = 9
      OnClick = CHB_D0Click
    end
    object SE_Module3: TSpinEdit
      Left = 224
      Top = 110
      Width = 73
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Enabled = False
      MaxValue = 0
      MinValue = 0
      TabOrder = 10
      Value = 1
      OnExit = SE_RCS_BoardExit
    end
    object SE_Port3: TSpinEdit
      Left = 312
      Top = 110
      Width = 73
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Enabled = False
      MaxValue = 0
      MinValue = 0
      TabOrder = 11
      Value = 0
    end
  end
  object E_Length: TEdit
    Left = 176
    Top = 58
    Width = 226
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    MaxLength = 8
    NumbersOnly = True
    TabOrder = 2
    Text = '0'
  end
  object CHB_Loop: TCheckBox
    Left = 176
    Top = 108
    Width = 17
    Height = 17
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    TabOrder = 4
  end
  object CB_Booster: TComboBox
    Left = 176
    Top = 83
    Width = 226
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Style = csDropDownList
    TabOrder = 3
  end
  object SE_Max_Trains: TSpinEdit
    Left = 176
    Top = 129
    Width = 226
    Height = 22
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    MaxValue = 4
    MinValue = 1
    TabOrder = 5
    Value = 1
  end
  object NB_TimeJCZav: TNumberBox
    Left = 296
    Top = 156
    Width = 105
    Height = 21
    Decimal = 1
    Mode = nbmFloat
    TabOrder = 7
  end
  object CHB_TimeJCZav: TCheckBox
    Left = 8
    Top = 156
    Width = 273
    Height = 17
    Caption = 'Prodlou'#382'en'#237' podr'#382'en'#237' z'#225'v'#283'ru p'#345'i rozpadu cest na [s]:'
    TabOrder = 6
    OnClick = CHB_TimeJCZavClick
  end
end
