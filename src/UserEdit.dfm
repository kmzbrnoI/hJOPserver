object F_UserEdit: TF_UserEdit
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Editovat u'#382'ivatele [user]'
  ClientHeight = 237
  ClientWidth = 617
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  PixelsPerInch = 96
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
    Top = 81
    Width = 257
    Height = 80
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
    Top = 168
    Width = 97
    Height = 17
    Caption = 'Opr'#225'vn'#283'n'#237' root'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
  end
  object B_Apply: TButton
    Left = 190
    Top = 204
    Width = 75
    Height = 25
    Caption = 'Pou'#382#237't'
    Default = True
    TabOrder = 5
    OnClick = B_ApplyClick
  end
  object B_Cancel: TButton
    Left = 109
    Top = 204
    Width = 75
    Height = 25
    Caption = 'Storno'
    TabOrder = 6
    OnClick = B_CancelClick
  end
  object GroupBox1: TGroupBox
    Left = 280
    Top = 8
    Width = 329
    Height = 221
    Caption = ' Opr'#225'vn'#283'n'#237' O'#344' '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 7
    object LV_ORs: TListView
      Left = 2
      Top = 15
      Width = 325
      Height = 171
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
          Width = 120
        end>
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ReadOnly = True
      RowSelect = True
      ParentFont = False
      TabOrder = 0
      ViewStyle = vsReport
      OnChange = LV_ORsChange
      OnCustomDrawItem = LV_ORsCustomDrawItem
    end
    object CB_Rights: TComboBox
      Left = 79
      Top = 192
      Width = 169
      Height = 21
      Style = csDropDownList
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ItemHeight = 13
      ParentFont = False
      TabOrder = 1
      OnChange = CB_RightsChange
      Items.Strings = (
        #382#225'dn'#233' opr'#225'vn'#283'n'#237
        'opr'#225'vn'#283'n'#237' ke '#269'ten'#237
        'opr'#225'vn'#283'n'#237' k z'#225'pisu'
        'superuser')
    end
  end
  object CHB_Ban: TCheckBox
    Left = 8
    Top = 191
    Width = 41
    Height = 17
    Caption = 'Ban'
    TabOrder = 8
  end
  object CHB_Reg: TCheckBox
    Left = 8
    Top = 214
    Width = 69
    Height = 17
    Caption = 'Regul'#225'tor'
    TabOrder = 9
  end
end
