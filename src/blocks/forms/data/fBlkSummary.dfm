object F_BlkSummary: TF_BlkSummary
  Left = 741
  Top = 209
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'F_BlkSummary'
  ClientHeight = 328
  ClientWidth = 337
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object Label1: TLabel
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
  object Label2: TLabel
    Left = 9
    Top = 33
    Width = 14
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'ID:'
  end
  object E_Name: TEdit
    Left = 120
    Top = 8
    Width = 211
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    MaxLength = 32
    TabOrder = 0
  end
  object B_save_P: TButton
    Left = 248
    Top = 294
    Width = 82
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Pou'#382#237't'
    Default = True
    TabOrder = 3
    OnClick = B_save_PClick
  end
  object B_Storno: TButton
    Left = 152
    Top = 294
    Width = 81
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Storno'
    TabOrder = 4
    OnClick = B_StornoClick
  end
  object SE_ID: TSpinEdit
    Left = 120
    Top = 33
    Width = 211
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
  object GB_Prejezdy: TGroupBox
    Left = 7
    Top = 70
    Width = 322
    Height = 211
    Caption = ' P'#345'ejezdy '
    TabOrder = 2
    object LV_Crossings: TListView
      Left = 2
      Top = 15
      Width = 318
      Height = 154
      Align = alTop
      Columns = <
        item
          Caption = '#'
          Width = 30
        end
        item
          Caption = 'ID'
        end
        item
          Caption = 'N'#225'zev'
          Width = 200
        end>
      GridLines = True
      MultiSelect = True
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnChange = LV_CrossingsChange
    end
    object CB_Crossing: TComboBox
      Left = 6
      Top = 177
      Width = 178
      Height = 21
      Style = csDropDownList
      TabOrder = 1
    end
    object B_Add: TButton
      Left = 190
      Top = 175
      Width = 60
      Height = 25
      Caption = 'P'#345'idat'
      TabOrder = 2
      OnClick = B_AddClick
    end
    object B_Remove: TButton
      Left = 256
      Top = 175
      Width = 57
      Height = 25
      Caption = 'Smazat'
      TabOrder = 3
      OnClick = B_RemoveClick
    end
  end
end
