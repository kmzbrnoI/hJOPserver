object F_BlkSignal: TF_BlkSignal
  Left = 485
  Top = 168
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Editovat data bloku [blok] (n'#225'v'#283'stidlo)'
  ClientHeight = 455
  ClientWidth = 681
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
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
    Width = 140
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Zpo'#382'd'#283'n'#237' p'#225'du n'#225'v'#283'stidla [s]:'
  end
  object Label13: TLabel
    Left = 338
    Top = 400
    Width = 121
    Height = 13
    Caption = 'Pro n'#225'v'#283'stidla ve smy'#269'ce'
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
    Left = 8
    Top = 177
    Width = 304
    Height = 136
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' Ovl'#225'd'#225'n'#237' n'#225'v'#283'stidla '
    TabOrder = 6
    object Label3: TLabel
      Left = 169
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
      Left = 233
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
      Left = 8
      Top = 107
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
      Top = 55
      Width = 46
      Height = 13
      Caption = '1. v'#253'stup:'
    end
    object Label11: TLabel
      Left = 105
      Top = 38
      Width = 57
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'RCS syst'#233'm'
    end
    object SE_RCSport1: TSpinEdit
      Left = 233
      Top = 55
      Width = 60
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
    object CB_Typ: TComboBox
      Left = 105
      Top = 107
      Width = 188
      Height = 21
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Style = csDropDownList
      TabOrder = 8
      Items.Strings = (
        'S-COM'
        'bin'#225'rn'#237' (0/1)')
    end
    object SE_RCSmodule1: TSpinEdit
      Left = 169
      Top = 55
      Width = 60
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      MaxValue = 0
      MinValue = 0
      TabOrder = 2
      Value = 1
    end
    object CHB_RCS_Output: TCheckBox
      Left = 8
      Top = 16
      Width = 153
      Height = 17
      Caption = 'N'#225'v'#283'stidlo m'#225' RCS v'#253'stup'
      TabOrder = 0
      OnClick = CHB_RCS_OutputClick
    end
    object SE_RCSmodule2: TSpinEdit
      Left = 169
      Top = 81
      Width = 60
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      MaxValue = 0
      MinValue = 0
      TabOrder = 6
      Value = 1
      OnExit = SE_RCSmodule2Exit
    end
    object SE_RCSport2: TSpinEdit
      Left = 233
      Top = 81
      Width = 60
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      MaxValue = 0
      MinValue = 0
      TabOrder = 7
      Value = 0
    end
    object CHB_RCS_Second_Output: TCheckBox
      Left = 8
      Top = 81
      Width = 73
      Height = 17
      Caption = '2. v'#253'stup:'
      TabOrder = 4
      OnClick = CHB_RCS_Second_OutputClick
    end
    object SE_RCSsystem1: TSpinEdit
      Left = 105
      Top = 55
      Width = 60
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      MaxValue = 0
      MinValue = 0
      TabOrder = 1
      Value = 1
    end
    object SE_RCSsystem2: TSpinEdit
      Left = 105
      Top = 81
      Width = 60
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      MaxValue = 0
      MinValue = 0
      TabOrder = 5
      Value = 1
      OnExit = SE_RCSmodule2Exit
    end
  end
  object B_Storno: TButton
    Left = 520
    Top = 420
    Width = 76
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Storno'
    TabOrder = 12
    OnClick = B_StornoClick
  end
  object B_Save: TButton
    Left = 600
    Top = 420
    Width = 74
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Pou'#382#237't'
    Default = True
    TabOrder = 11
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
    Top = 155
    Width = 201
    Height = 17
    Caption = 'N'#225'v'#283'stidlo trvale zamknuto do ST'#366'J'
    TabOrder = 5
  end
  object PC_Events: TPageControl
    Left = 317
    Top = 8
    Width = 356
    Height = 369
    MultiLine = True
    OwnerDraw = True
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
    TabOrder = 13
    OnClick = BB_Event_AddClick
  end
  object GB_PSt: TGroupBox
    Left = 8
    Top = 318
    Width = 303
    Height = 127
    Caption = ' Pomocn'#233' stav'#283'dlo - indikace, voli'#269'e '
    TabOrder = 7
    object Label7: TLabel
      Left = 7
      Top = 62
      Width = 79
      Height = 26
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Indikace posunu'#13#10'(v'#253'stup):'
    end
    object Label8: TLabel
      Left = 7
      Top = 88
      Width = 61
      Height = 26
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Voli'#269' posunu'#13#10'(vstup):'
    end
    object Label9: TLabel
      Left = 169
      Top = 47
      Width = 53
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'RCS modul'
    end
    object Label10: TLabel
      Left = 233
      Top = 47
      Width = 43
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'RCS port'
    end
    object Label12: TLabel
      Left = 105
      Top = 47
      Width = 57
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'RCS syst'#233'm'
    end
    object CHB_PSt: TCheckBox
      Left = 7
      Top = 19
      Width = 256
      Height = 17
      Caption = 'Indikovat polohu v'#253'hybky (v pultu), umo'#382'nit voli'#269'e'
      TabOrder = 0
      OnClick = CHB_PStClick
    end
    object SE_Cont_Module: TSpinEdit
      Left = 169
      Top = 90
      Width = 60
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      MaxValue = 0
      MinValue = 0
      TabOrder = 5
      Value = 1
    end
    object SE_Ind_Module: TSpinEdit
      Left = 169
      Top = 64
      Width = 60
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      MaxValue = 0
      MinValue = 0
      TabOrder = 2
      Value = 1
    end
    object SE_Ind_Port: TSpinEdit
      Left = 233
      Top = 64
      Width = 60
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
    object SE_Cont_Port: TSpinEdit
      Left = 233
      Top = 90
      Width = 60
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      MaxValue = 0
      MinValue = 0
      TabOrder = 6
      Value = 0
    end
    object SE_Ind_System: TSpinEdit
      Left = 105
      Top = 64
      Width = 60
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      MaxValue = 0
      MinValue = 0
      TabOrder = 1
      Value = 1
    end
    object SE_Cont_System: TSpinEdit
      Left = 105
      Top = 90
      Width = 60
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      MaxValue = 0
      MinValue = 0
      TabOrder = 4
      Value = 1
      OnExit = SE_RCSmodule2Exit
    end
  end
  object CHB_changeTime: TCheckBox
    Left = 8
    Top = 132
    Width = 184
    Height = 17
    Caption = 'Specifick'#253' '#269'as zm'#283'ny n'#225'v'#283'sti [s]:'
    TabOrder = 3
    OnClick = CHB_changeTimeClick
  end
  object NB_ChangeTime: TNumberBox
    Left = 224
    Top = 132
    Width = 87
    Height = 21
    Decimal = 1
    Mode = nbmFloat
    MaxValue = 10.000000000000000000
    TabOrder = 4
  end
  object CHB_ForceDirection: TCheckBox
    Left = 317
    Top = 383
    Width = 228
    Height = 17
    Caption = 'Vnutit sm'#283'r n'#225'v'#283'stidla (pouze pro experty!):'
    TabOrder = 9
    OnClick = CHB_ForceDirectionClick
  end
  object CB_ForceDirection: TComboBox
    Left = 551
    Top = 383
    Width = 122
    Height = 21
    Style = csDropDownList
    TabOrder = 10
    Items.Strings = (
      'lich'#253
      'sud'#253)
  end
end
