object F_BlkIO: TF_BlkIO
  Left = 869
  Top = 145
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Editovat data bloku [blok] (IO)'
  ClientHeight = 430
  ClientWidth = 321
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
  object L_IR02: TLabel
    Left = 8
    Top = 40
    Width = 14
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'ID:'
  end
  object L_IR01: TLabel
    Left = 8
    Top = 8
    Width = 34
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'N'#225'zev:'
  end
  object E_Name: TEdit
    Left = 120
    Top = 8
    Width = 193
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    MaxLength = 32
    TabOrder = 0
  end
  object SE_ID: TSpinEdit
    Left = 120
    Top = 40
    Width = 193
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
  object GB_RCS_Input: TGroupBox
    Left = 10
    Top = 148
    Width = 304
    Height = 114
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' RCS vstup '
    TabOrder = 6
    object L_IR04: TLabel
      Left = 16
      Top = 56
      Width = 63
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'RCS adresa :'
    end
    object L_IR05: TLabel
      Left = 16
      Top = 84
      Width = 49
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'RCS port :'
    end
    object SE_RCS_Input_Port: TSpinEdit
      Left = 216
      Top = 83
      Width = 73
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      MaxValue = 0
      MinValue = 0
      TabOrder = 3
      Value = 0
    end
    object SE_RCS_Input_Module: TSpinEdit
      Left = 216
      Top = 57
      Width = 73
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      MaxValue = 0
      MinValue = 0
      TabOrder = 2
      Value = 1
      OnExit = SE_RCS_Input_ModuleExit
    end
    object CHB_RCS_Input: TCheckBox
      Left = 16
      Top = 18
      Width = 188
      Height = 17
      Caption = 'Blok m'#225' vstup na sb'#283'rnici RCS'
      TabOrder = 0
      OnClick = CHB_RCS_InputClick
    end
    object CHB_RCS_Input_Needed: TCheckBox
      Left = 16
      Top = 34
      Width = 220
      Height = 17
      Caption = 'Deaktivovat blok p'#345'i ztr'#225't'#283' RCS vstupu'
      TabOrder = 1
    end
  end
  object B_Storno: TButton
    Left = 151
    Top = 398
    Width = 76
    Height = 24
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Storno'
    TabOrder = 8
    OnClick = B_StornoClick
  end
  object B_Save: TButton
    Left = 240
    Top = 398
    Width = 74
    Height = 24
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Pou'#382#237't'
    Default = True
    TabOrder = 9
    OnClick = B_SaveClick
  end
  object CHB_Activate_On_Start: TCheckBox
    Left = 8
    Top = 74
    Width = 169
    Height = 17
    Caption = 'Aktivovat v'#253'stup po startu'
    TabOrder = 2
  end
  object CHB_Nullable: TCheckBox
    Left = 8
    Top = 97
    Width = 223
    Height = 17
    Caption = 'Po zapnut'#237' automaticky deaktivovat po [s]'
    TabOrder = 3
    OnClick = CHB_NullableClick
  end
  object SE_Null_Time: TSpinEdit
    Left = 237
    Top = 97
    Width = 76
    Height = 22
    MaxValue = 360
    MinValue = 1
    TabOrder = 4
    Value = 1
  end
  object GB_RCS_Output: TGroupBox
    Left = 10
    Top = 266
    Width = 304
    Height = 111
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' RCS v'#253'stup '
    TabOrder = 7
    object Label1: TLabel
      Left = 13
      Top = 56
      Width = 63
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'RCS adresa :'
    end
    object Label2: TLabel
      Left = 13
      Top = 84
      Width = 49
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'RCS port :'
    end
    object SE_RCS_Output_Port: TSpinEdit
      Left = 213
      Top = 83
      Width = 73
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      MaxValue = 0
      MinValue = 0
      TabOrder = 3
      Value = 0
    end
    object SE_RCS_Output_Module: TSpinEdit
      Left = 213
      Top = 57
      Width = 73
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      MaxValue = 0
      MinValue = 0
      TabOrder = 2
      Value = 1
      OnExit = SE_RCS_Output_ModuleExit
    end
    object CHB_RCS_Output: TCheckBox
      Left = 16
      Top = 18
      Width = 185
      Height = 17
      Caption = 'Blok m'#225' v'#253'stup na sb'#283'rnici RCS'
      TabOrder = 0
      OnClick = CHB_RCS_OutputClick
    end
    object CHB_RCS_Output_Needed: TCheckBox
      Left = 16
      Top = 34
      Width = 217
      Height = 17
      Caption = 'Deaktivovat blok p'#345'i ztr'#225't'#283' RCS v'#253'stupu'
      TabOrder = 1
    end
  end
  object CHB_AllowOutChange: TCheckBox
    Left = 8
    Top = 120
    Width = 184
    Height = 17
    Caption = 'Povolit de/aktivaci bloku z panelu'
    TabOrder = 5
  end
end
