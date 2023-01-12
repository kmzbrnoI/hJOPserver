object F_BlkRailway: TF_BlkRailway
  Left = 554
  Top = 96
  ActiveControl = B_Save
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'F_BlkRailway'
  ClientHeight = 341
  ClientWidth = 671
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
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object B_Save: TButton
    Left = 589
    Top = 309
    Width = 75
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Pou'#382#237't'
    Default = True
    TabOrder = 4
    OnClick = B_SaveClick
  end
  object B_Storno: TButton
    Left = 512
    Top = 309
    Width = 73
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Storno'
    TabOrder = 5
    OnClick = B_StornoClick
  end
  object GB_LinkerA: TGroupBox
    Left = 8
    Top = 150
    Width = 337
    Height = 90
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' '#218'vazka bl'#237#382'e po'#269#225'tku trati  '
    TabOrder = 1
    object Label1: TLabel
      Left = 22
      Top = 24
      Width = 71
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'N'#225'zev '#250'vazky:'
    end
    object Label3: TLabel
      Left = 22
      Top = 49
      Width = 14
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'ID:'
    end
    object E_LA_Name: TEdit
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
    object SE_LA_Id: TSpinEdit
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
  end
  object GB_LinkerB: TGroupBox
    Left = 7
    Top = 244
    Width = 337
    Height = 90
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' '#218'vazka bl'#237#382'e konci trati  '
    TabOrder = 2
    object Label4: TLabel
      Left = 22
      Top = 24
      Width = 71
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'N'#225'zev '#250'vazky:'
    end
    object Label5: TLabel
      Left = 22
      Top = 49
      Width = 14
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'ID:'
    end
    object SE_LB_Id: TSpinEdit
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
    object E_LB_Name: TEdit
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
    Height = 137
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
      Width = 14
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'ID:'
    end
    object Label7: TLabel
      Left = 17
      Top = 76
      Width = 65
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Typ zab. za'#345'.:'
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
    object SE_Railway_ID: TSpinEdit
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
    object E_Railway_Name: TEdit
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
    object CB_Type: TComboBox
      Left = 104
      Top = 75
      Width = 216
      Height = 21
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Style = csDropDownList
      TabOrder = 2
      Items.Strings = (
        'pouze souhlasov'#253' stav'
        'bezsouhlasov'#253' stav s nab'#237'dkou')
    end
    object CB_Signals: TComboBox
      Left = 104
      Top = 100
      Width = 216
      Height = 21
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Style = csDropDownList
      TabOrder = 3
      Items.Strings = (
        'autoblok'
        'hradlo')
    end
  end
  object GB_TratBlk: TGroupBox
    Left = 358
    Top = 7
    Width = 306
    Height = 282
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' Tra'#357'ov'#233' '#250'seky (ve sm'#283'ru trati) '
    TabOrder = 3
    object GB_Track: TGroupBox
      Left = 2
      Top = 230
      Width = 302
      Height = 50
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alBottom
      Caption = ' P'#345'idat/upravit/smazat tra'#357'ov'#253' '#250'sek  '
      TabOrder = 0
      object B_Track_Add: TButton
        Left = 176
        Top = 15
        Width = 54
        Height = 23
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'P'#345'idat'
        TabOrder = 1
        OnClick = B_Track_AddClick
      end
      object CB_Track: TComboBox
        Left = 8
        Top = 16
        Width = 164
        Height = 21
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Style = csDropDownList
        TabOrder = 0
      end
      object B_Track_Del: TButton
        Left = 232
        Top = 15
        Width = 64
        Height = 23
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Smazat'
        Enabled = False
        TabOrder = 2
        OnClick = B_Track_DelClick
      end
    end
    object LV_Tracks: TListView
      Left = 2
      Top = 15
      Width = 302
      Height = 215
      Align = alClient
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
          Width = 180
        end>
      DragMode = dmAutomatic
      FullDrag = True
      GridLines = True
      MultiSelect = True
      ReadOnly = True
      RowSelect = True
      TabOrder = 1
      ViewStyle = vsReport
      OnChange = LV_TracksChange
      OnDragDrop = LV_TracksDragDrop
      OnDragOver = LV_TracksDragOver
    end
  end
end
