object F_RREv: TF_RREv
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'F_RREv'
  ClientHeight = 205
  ClientWidth = 161
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 3
    Width = 62
    Height = 13
    Caption = 'Typ ud'#225'losti:'
  end
  object GB_Time: TGroupBox
    Left = 8
    Top = 90
    Width = 145
    Height = 110
    Caption = ' '#268'as '
    TabOrder = 4
    object Label6: TLabel
      Left = 8
      Top = 16
      Width = 80
      Height = 13
      Caption = 'Form'#225't: mm:ss,z'
    end
    object ME_Time: TMaskEdit
      Left = 8
      Top = 36
      Width = 125
      Height = 21
      Hint = 'Zadejte aktu'#225'ln'#237' modelov'#253' cas'
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      EditMask = '!90:00.0;1;_'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      MaxLength = 7
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Text = '  :  . '
    end
  end
  object GB_Track: TGroupBox
    Left = 8
    Top = 90
    Width = 145
    Height = 110
    Caption = ' '#218'sek '
    TabOrder = 2
    object Label2: TLabel
      Left = 8
      Top = 16
      Width = 26
      Height = 13
      Caption = 'Stav:'
    end
    object Label3: TLabel
      Left = 8
      Top = 62
      Width = 57
      Height = 13
      Caption = #268#225'st '#250'seku:'
    end
    object CB_Track_State: TComboBox
      Left = 8
      Top = 35
      Width = 129
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      Items.Strings = (
        'Uvoln'#283'no'
        'Obsazeno')
    end
    object CB_Track_Part: TComboBox
      Left = 8
      Top = 81
      Width = 129
      Height = 21
      Style = csDropDownList
      TabOrder = 1
      Items.Strings = (
        '0'
        '1'
        '2'
        '3')
    end
  end
  object GB_IR: TGroupBox
    Left = 8
    Top = 90
    Width = 145
    Height = 110
    Caption = ' IR '#269'idlo '
    TabOrder = 3
    object Label4: TLabel
      Left = 8
      Top = 16
      Width = 26
      Height = 13
      Caption = 'Stav:'
    end
    object Label5: TLabel
      Left = 8
      Top = 62
      Width = 27
      Height = 13
      Caption = #268'idlo:'
    end
    object CB_IR_State: TComboBox
      Left = 8
      Top = 35
      Width = 129
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      Items.Strings = (
        'Uvoln'#283'no'
        'Obsazeno')
    end
    object CB_IR: TComboBox
      Left = 8
      Top = 81
      Width = 129
      Height = 21
      Style = csDropDownList
      TabOrder = 1
    end
  end
  object GB_Distance: TGroupBox
    Left = 8
    Top = 90
    Width = 145
    Height = 110
    Caption = ' Vzd'#225'lenost '
    TabOrder = 6
    object Label7: TLabel
      Left = 120
      Top = 28
      Width = 13
      Height = 13
      Caption = 'cm'
    end
    object E_Distance: TEdit
      Left = 13
      Top = 21
      Width = 101
      Height = 21
      NumbersOnly = True
      TabOrder = 0
      Text = 'E_Distance'
    end
  end
  object CB_Type: TComboBox
    Left = 8
    Top = 19
    Width = 145
    Height = 21
    Style = csDropDownList
    TabOrder = 0
    OnChange = CB_TypeChange
    Items.Strings = (
      #218'sek'
      'IR '#269'idlo'
      #268'as'
      'Vzd'#225'lenost')
  end
  object CB_Track: TComboBox
    Left = 8
    Top = 63
    Width = 145
    Height = 21
    Style = csDropDownList
    TabOrder = 1
    OnChange = CB_TrackChange
  end
  object CHB_Track: TCheckBox
    Left = 8
    Top = 46
    Width = 97
    Height = 17
    Caption = 'Odli'#353'n'#253' '#250'sek'
    TabOrder = 5
    OnClick = CHB_TrackClick
  end
end
