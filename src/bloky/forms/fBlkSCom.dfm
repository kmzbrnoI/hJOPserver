object F_BlkSCom: TF_BlkSCom
  Left = 485
  Top = 168
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Editovat data bloku [blok] (n'#225'v'#283'stidlo)'
  ClientHeight = 369
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
  object L_SCom01: TLabel
    Left = 8
    Top = 8
    Width = 37
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'N'#225'zev :'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object L_SCom02: TLabel
    Left = 8
    Top = 40
    Width = 11
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'ID'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object L_Usek03: TLabel
    Left = 7
    Top = 72
    Width = 42
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Stanice :'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label1: TLabel
    Left = 7
    Top = 131
    Width = 112
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = #218'sek p'#345'ed n'#225'v'#283'stidlem:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object L_UsekID: TLabel
    Left = 291
    Top = 131
    Width = 20
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Alignment = taRightJustify
    Caption = '[blk]'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 7
    Top = 160
    Width = 170
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Zpo'#382'd'#283'n'#237' p'#225'du n'#225'v'#283'stidla [sekund]:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object E_Nazev: TEdit
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
  object GB_MTB: TGroupBox
    Left = 7
    Top = 187
    Width = 304
    Height = 123
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' Ovl'#225'd'#225'n'#237' n'#225'v'#283'stidla '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    object L_SCom05: TLabel
      Left = 7
      Top = 41
      Width = 56
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'RCS modul:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object L_SCom06: TLabel
      Left = 7
      Top = 64
      Width = 46
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'RCS port:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object L_SCom04: TLabel
      Left = 7
      Top = 89
      Width = 61
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Typ v'#253'stupu:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object SE_MTBPort: TSpinEdit
      Left = 223
      Top = 66
      Width = 73
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      MaxValue = 15
      MinValue = 0
      ParentFont = False
      TabOrder = 2
      Value = 0
    end
    object CB_Typ: TComboBox
      Left = 95
      Top = 92
      Width = 201
      Height = 21
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Style = csDropDownList
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ItemHeight = 13
      ParentFont = False
      TabOrder = 3
      Items.Strings = (
        'S-Com'
        'bin'#225'rn'#237' (0/1)')
    end
    object SE_MTBMTB: TSpinEdit
      Left = 223
      Top = 40
      Width = 73
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      MaxValue = 191
      MinValue = 1
      ParentFont = False
      TabOrder = 1
      Value = 1
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
  end
  object B_Storno: TButton
    Left = 85
    Top = 337
    Width = 76
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Storno'
    TabOrder = 7
    OnClick = B_StornoClick
  end
  object B_Save: TButton
    Left = 7
    Top = 337
    Width = 74
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Pou'#382#237't'
    Default = True
    TabOrder = 6
    OnClick = B_SaveClick
  end
  object LB_Stanice: TListBox
    Left = 119
    Top = 72
    Width = 192
    Height = 46
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    ItemHeight = 13
    TabOrder = 2
  end
  object SE_Delay: TSpinEdit
    Left = 224
    Top = 160
    Width = 87
    Height = 22
    MaxValue = 60
    MinValue = 0
    TabOrder = 3
    Value = 0
  end
  object CHB_Zamknuto: TCheckBox
    Left = 8
    Top = 315
    Width = 201
    Height = 17
    Caption = 'N'#225'v'#283'stidlo trvale zamknuto do ST'#366'J'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
  end
  object PC_Events: TPageControl
    Left = 317
    Top = 8
    Width = 356
    Height = 329
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MultiLine = True
    OwnerDraw = True
    ParentFont = False
    TabOrder = 8
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
    DoubleBuffered = True
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
    ParentDoubleBuffered = False
    TabOrder = 9
    OnClick = BB_Event_AddClick
  end
end
