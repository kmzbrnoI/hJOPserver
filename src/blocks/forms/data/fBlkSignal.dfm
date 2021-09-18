object F_BlkSignal: TF_BlkSignal
  Left = 485
  Top = 168
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Editovat data bloku [blok] (n'#225'v'#283'stidlo)'
  ClientHeight = 346
  ClientWidth = 681
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object L_Name: TLabel
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
  object L_ID: TLabel
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
  object Label1: TLabel
    Left = 7
    Top = 75
    Width = 112
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = #218'sek p'#345'ed n'#225'v'#283'stidlem:'
  end
  object L_Track_Id: TLabel
    Left = 291
    Top = 75
    Width = 20
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Alignment = taRightJustify
    Caption = '[blk]'
  end
  object Label2: TLabel
    Left = 7
    Top = 104
    Width = 170
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Zpo'#382'd'#283'n'#237' p'#225'du n'#225'v'#283'stidla [sekund]:'
  end
  object E_Name: TEdit
    Left = 119
    Top = 7
    Width = 192
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
  object GB_RCS: TGroupBox
    Left = 7
    Top = 131
    Width = 304
    Height = 147
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' Ovl'#225'd'#225'n'#237' n'#225'v'#283'stidla '
    TabOrder = 3
    object Label3: TLabel
      Left = 131
      Top = 38
      Width = 53
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'RCS modul'
    end
    object Label4: TLabel
      Left = 221
      Top = 38
      Width = 43
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'RCS port'
    end
    object Label5: TLabel
      Left = 7
      Top = 117
      Width = 61
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Typ v'#253'stupu:'
    end
    object Label6: TLabel
      Left = 8
      Top = 56
      Width = 62
      Height = 13
      Caption = 'Prvn'#237' v'#253'stup:'
    end
    object SE_RCSport1: TSpinEdit
      Left = 221
      Top = 55
      Width = 73
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      MaxValue = 0
      MinValue = 0
      TabOrder = 4
      Value = 0
    end
    object CB_Typ: TComboBox
      Left = 93
      Top = 117
      Width = 201
      Height = 21
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Style = csDropDownList
      TabOrder = 5
      Items.Strings = (
        'S-Com'
        'bin'#225'rn'#237' (0/1)')
    end
    object SE_RCSmodule1: TSpinEdit
      Left = 131
      Top = 55
      Width = 73
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      MaxValue = 0
      MinValue = 0
      TabOrder = 1
      Value = 1
      OnExit = SE_RCSmodule1Exit
    end
    object CHB_RCS_Output: TCheckBox
      Left = 8
      Top = 16
      Width = 201
      Height = 17
      Caption = 'N'#225'v'#283'stidlo m'#225' v'#253'stup na sb'#283'rnici RCS'
      TabOrder = 0
      OnClick = CHB_RCS_OutputClick
    end
    object SE_RCSmodule2: TSpinEdit
      Left = 131
      Top = 86
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
      OnExit = SE_RCSmodule2Exit
    end
    object SE_RCSport2: TSpinEdit
      Left = 221
      Top = 85
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
    object CHB_RCS_Second_Output: TCheckBox
      Left = 8
      Top = 85
      Width = 81
      Height = 17
      Caption = 'Druh'#253' v'#253'stup:'
      TabOrder = 6
      OnClick = CHB_RCS_Second_OutputClick
    end
  end
  object B_Storno: TButton
    Left = 85
    Top = 312
    Width = 76
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Storno'
    TabOrder = 6
    OnClick = B_StornoClick
  end
  object B_Save: TButton
    Left = 7
    Top = 312
    Width = 74
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Pou'#382#237't'
    Default = True
    TabOrder = 5
    OnClick = B_SaveClick
  end
  object SE_Delay: TSpinEdit
    Left = 224
    Top = 104
    Width = 87
    Height = 22
    MaxValue = 60
    MinValue = 0
    TabOrder = 2
    Value = 0
  end
  object CHB_Locked: TCheckBox
    Left = 8
    Top = 283
    Width = 201
    Height = 17
    Caption = 'N'#225'v'#283'stidlo trvale zamknuto do ST'#366'J'
    TabOrder = 4
  end
  object PC_Events: TPageControl
    Left = 317
    Top = 8
    Width = 356
    Height = 329
    MultiLine = True
    OwnerDraw = True
    TabOrder = 7
    OnDrawTab = PageControlCloseButtonDrawTab
    OnMouseDown = PageControlCloseButtonMouseDown
    OnMouseLeave = PageControlCloseButtonMouseLeave
    OnMouseMove = PageControlCloseButtonMouseMove
    OnMouseUp = PageControlCloseButtonMouseUp
  end
  object BB_Event_Add: TBitBtn
    Left = 652
    Top = 8
    Width = 21
    Height = 21
    Glyph.Data = {
      36030000424D3603000000000000360000002800000010000000100000000100
      1800000000000003000000000000000000000000000000000000FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFBEBEBEBBBBBBBBBBBBECECECFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000
      0000000000AAAAAAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000AAAAAAFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000
      0000000000AAAAAAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000AAAAAAFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000
      0000000000AAAAAAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBEBEBEBBBBBB
      BBBBBBBBBBBBBBBBBBBBBBBB0000000000000000007D7D7DBBBBBBBBBBBBBBBB
      BBBBBBBBBBBBBBECECEC00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000AAAAAA000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000AAAAAA09090900000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000B3B3B3FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000AAAAAAFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000
      0000000000AAAAAAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000AAAAAAFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000
      0000000000AAAAAAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000AAAAAAFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF09090900
      0000000000B3B3B3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
    TabOrder = 8
    OnClick = BB_Event_AddClick
  end
end
