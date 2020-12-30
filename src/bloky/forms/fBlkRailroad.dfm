object F_BlkRailroad: TF_BlkRailroad
  Left = 554
  Top = 96
  ActiveControl = B_Save
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Editace trate'
  ClientHeight = 426
  ClientWidth = 706
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object B_Save: TButton
    Left = 624
    Top = 394
    Width = 75
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Pou'#382#237't'
    Default = True
    TabOrder = 3
    OnClick = B_SaveClick
  end
  object B_Storno: TButton
    Left = 547
    Top = 394
    Width = 73
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Storno'
    TabOrder = 4
    OnClick = B_StornoClick
  end
  object GB_UvazkaA: TGroupBox
    Left = 360
    Top = 7
    Width = 337
    Height = 144
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' '#218'vazka bl'#237#382'e po'#269#225'tku trati  '
    TabOrder = 1
    object Label1: TLabel
      Left = 23
      Top = 24
      Width = 66
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'N'#225'zev bloku :'
    end
    object Label2: TLabel
      Left = 23
      Top = 75
      Width = 42
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Stanice :'
    end
    object Label3: TLabel
      Left = 22
      Top = 49
      Width = 11
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'ID'
    end
    object E_UA_name: TEdit
      Left = 104
      Top = 24
      Width = 209
      Height = 21
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      MaxLength = 32
      TabOrder = 0
    end
    object SE_UA_id: TSpinEdit
      Left = 104
      Top = 49
      Width = 209
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
    object LB_UA_St: TListBox
      Left = 104
      Top = 75
      Width = 209
      Height = 46
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      ItemHeight = 13
      TabOrder = 2
    end
  end
  object GB_UvazkaB: TGroupBox
    Left = 360
    Top = 159
    Width = 337
    Height = 144
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' '#218'vazka bl'#237#382'e konci trati  '
    TabOrder = 2
    object Label4: TLabel
      Left = 23
      Top = 24
      Width = 66
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'N'#225'zev bloku :'
    end
    object Label5: TLabel
      Left = 22
      Top = 49
      Width = 11
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'ID'
    end
    object Label6: TLabel
      Left = 23
      Top = 75
      Width = 42
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Stanice :'
    end
    object LB_UB_St: TListBox
      Left = 104
      Top = 75
      Width = 209
      Height = 46
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      ItemHeight = 13
      TabOrder = 2
    end
    object SE_UB_id: TSpinEdit
      Left = 104
      Top = 49
      Width = 209
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
    object E_UB_name: TEdit
      Left = 104
      Top = 24
      Width = 209
      Height = 21
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      MaxLength = 32
      TabOrder = 0
    end
  end
  object GB_Trat: TGroupBox
    Left = 8
    Top = 8
    Width = 337
    Height = 410
    Caption = ' Tra'#357' '
    TabOrder = 0
    object L_Name: TLabel
      Left = 18
      Top = 23
      Width = 57
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'N'#225'zev trati :'
    end
    object L_ID: TLabel
      Left = 18
      Top = 48
      Width = 11
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'ID'
    end
    object Label7: TLabel
      Left = 17
      Top = 76
      Width = 68
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Typ zab. za'#345'. :'
    end
    object Label8: TLabel
      Left = 17
      Top = 101
      Width = 53
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'N'#225'v'#283'stidla:'
    end
    object SE_Trat_ID: TSpinEdit
      Left = 104
      Top = 49
      Width = 216
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
    object E_Trat_Name: TEdit
      Left = 104
      Top = 24
      Width = 216
      Height = 21
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      MaxLength = 32
      TabOrder = 0
    end
    object GB_TratBlk: TGroupBox
      Left = 17
      Top = 132
      Width = 306
      Height = 269
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = ' Tra'#357'ov'#233' bloky (ve sm'#283'ru trati) '
      TabOrder = 4
      object B_Blk_Delete: TButton
        Left = 113
        Top = 21
        Width = 75
        Height = 25
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Smazat T'#218
        Enabled = False
        TabOrder = 1
        OnClick = B_Blk_DeleteClick
      end
      object GB_NewBlk: TGroupBox
        Left = 2
        Top = 217
        Width = 302
        Height = 50
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alBottom
        Caption = ' P'#345'idat nov'#253' tra'#357'ov'#253' '#250'sek  '
        TabOrder = 0
        object B_blk_Add: TButton
          Left = 216
          Top = 14
          Width = 75
          Height = 25
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Caption = 'P'#345'idat T'#218
          TabOrder = 1
          OnClick = B_blk_AddClick
        end
        object CB_NewTratBlok: TComboBox
          Left = 8
          Top = 16
          Width = 193
          Height = 21
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
        end
      end
      object LV_Useky: TListView
        Left = 2
        Top = 55
        Width = 302
        Height = 162
        Align = alBottom
        Columns = <
          item
            Caption = 'ID'
          end
          item
            Caption = 'N'#225'zev'
            Width = 200
          end>
        ReadOnly = True
        RowSelect = True
        TabOrder = 2
        ViewStyle = vsReport
        OnChange = LV_UsekyChange
      end
    end
    object CB_Trat_ZabZar: TComboBox
      Left = 104
      Top = 75
      Width = 216
      Height = 21
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 2
      Items.Strings = (
        'pouze souhlasov'#253' stav'
        'bezsouhlasov'#253' stav s nab'#237'dkou')
    end
    object CB_Navestidla: TComboBox
      Left = 104
      Top = 100
      Width = 216
      Height = 21
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 3
      Items.Strings = (
        'autoblok'
        'hradlo')
    end
  end
end
