object F_UserEdit: TF_UserEdit
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Editovat u'#382'ivatele [user]'
  ClientHeight = 289
  ClientWidth = 617
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnClose = FormClose
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 90
    Height = 13
    Caption = 'U'#382'ivatelsk'#233' jm'#233'no:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 8
    Top = 35
    Width = 69
    Height = 13
    Caption = 'K'#345'estn'#237' jm'#233'no:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 8
    Top = 62
    Width = 41
    Height = 13
    Caption = 'P'#345#237'jmen'#237':'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label6: TLabel
    Left = 8
    Top = 167
    Width = 52
    Height = 13
    Caption = 'Pozn'#225'mka:'
  end
  object E_UserName: TEdit
    Left = 144
    Top = 8
    Width = 121
    Height = 21
    MaxLength = 32
    TabOrder = 0
    Text = 'E_UserName'
  end
  object E_FirstName: TEdit
    Left = 144
    Top = 35
    Width = 121
    Height = 21
    MaxLength = 32
    TabOrder = 1
    Text = 'E_FirstName'
  end
  object E_LastName: TEdit
    Left = 144
    Top = 62
    Width = 121
    Height = 21
    MaxLength = 32
    TabOrder = 2
    Text = 'E_LastName'
  end
  object GB_Password: TGroupBox
    Left = 8
    Top = 89
    Width = 257
    Height = 72
    Caption = ' Heslo '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    object Label4: TLabel
      Left = 16
      Top = 16
      Width = 69
      Height = 13
      Caption = 'Zadejte heslo:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label5: TLabel
      Left = 16
      Top = 43
      Width = 76
      Height = 13
      Caption = 'Potvr'#271'te heslo:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object E_Password1: TEdit
      Left = 120
      Top = 16
      Width = 121
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      MaxLength = 32
      ParentFont = False
      PasswordChar = '*'
      TabOrder = 0
      Text = 'E_Password1'
    end
    object E_Password2: TEdit
      Left = 120
      Top = 43
      Width = 121
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      MaxLength = 32
      ParentFont = False
      PasswordChar = '*'
      TabOrder = 1
      Text = 'E_Password2'
    end
  end
  object CHB_root: TCheckBox
    Left = 8
    Top = 264
    Width = 97
    Height = 17
    Caption = 'Opr'#225'vn'#283'n'#237' root'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
  end
  object B_Apply: TButton
    Left = 534
    Top = 259
    Width = 75
    Height = 25
    Caption = 'Pou'#382#237't'
    Default = True
    TabOrder = 9
    OnClick = B_ApplyClick
  end
  object B_Cancel: TButton
    Left = 453
    Top = 259
    Width = 75
    Height = 25
    Caption = 'Storno'
    TabOrder = 10
    OnClick = B_CancelClick
  end
  object GroupBox1: TGroupBox
    Left = 280
    Top = 8
    Width = 329
    Height = 245
    Caption = ' Opr'#225'vn'#283'n'#237' k '#345#237'zen'#237' oblast'#237' '#345#237'zen'#237
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 8
    object LV_ORs: TListView
      Left = 2
      Top = 15
      Width = 325
      Height = 194
      Align = alTop
      Columns = <
        item
          Caption = 'Id'
        end
        item
          Caption = 'N'#225'zev'
          Width = 110
        end
        item
          Caption = 'Opr'#225'vn'#283'n'#237
          Width = 130
        end>
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      MultiSelect = True
      ReadOnly = True
      RowSelect = True
      ParentFont = False
      TabOrder = 0
      ViewStyle = vsReport
      OnChange = LV_ORsChange
      OnCustomDrawItem = LV_ORsCustomDrawItem
    end
    object CB_Rights: TComboBox
      Left = 80
      Top = 215
      Width = 169
      Height = 21
      Style = csDropDownList
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnChange = CB_RightsChange
      Items.Strings = (
        #382#225'dn'#233' opr'#225'vn'#283'n'#237
        'opr'#225'vn'#283'n'#237' k pozorov'#225'n'#237
        'opr'#225'vn'#283'n'#237' k '#345#237'zen'#237
        'superu'#382'ivatel')
    end
  end
  object CHB_Ban: TCheckBox
    Left = 128
    Top = 264
    Width = 41
    Height = 17
    Caption = 'Ban'
    TabOrder = 6
  end
  object CHB_Reg: TCheckBox
    Left = 196
    Top = 264
    Width = 69
    Height = 17
    Caption = 'Regul'#225'tor'
    TabOrder = 7
  end
  object M_Note: TMemo
    Left = 8
    Top = 187
    Width = 257
    Height = 71
    Lines.Strings = (
      'M_Note')
    ScrollBars = ssVertical
    TabOrder = 4
  end
end
