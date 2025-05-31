object F_BlkSignalState: TF_BlkSignalState
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Stav n'#225'v'#283'stidla [signal]'
  ClientHeight = 257
  ClientWidth = 753
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 38
    Height = 13
    Caption = 'N'#225'v'#283'st:'
  end
  object Label2: TLabel
    Left = 8
    Top = 40
    Width = 71
    Height = 13
    Caption = 'Za'#269#225'tek cesty:'
  end
  object Label3: TLabel
    Left = 8
    Top = 104
    Width = 32
    Height = 13
    Caption = 'AB JC:'
  end
  object L_ABJC: TLabel
    Left = 333
    Top = 104
    Width = 36
    Height = 13
    Alignment = taRightJustify
    Caption = 'L_ABJC'
  end
  object Label4: TLabel
    Left = 8
    Top = 123
    Width = 33
    Height = 13
    Caption = 'DN JC:'
  end
  object L_DNJC: TLabel
    Left = 332
    Top = 123
    Width = 37
    Height = 13
    Alignment = taRightJustify
    Caption = 'L_DNJC'
  end
  object Label6: TLabel
    Left = 8
    Top = 142
    Width = 45
    Height = 13
    Caption = 'P'#345'ivol JC:'
  end
  object L_PrivolJC: TLabel
    Left = 320
    Top = 142
    Width = 49
    Height = 13
    Alignment = taRightJustify
    Caption = 'L_PrivolJC'
  end
  object L_PN_Start_Time: TLabel
    Left = 287
    Top = 185
    Width = 82
    Height = 13
    Alignment = taRightJustify
    Caption = 'L_PN_Start_Time'
  end
  object CB_Aspect: TComboBox
    Left = 120
    Top = 8
    Width = 249
    Height = 21
    Style = csDropDownList
    TabOrder = 0
    Items.Strings = (
      'changing'
      'disabled'
      'St'#367'j/posun zak'#225'z'#225'n'
      'Volno'
      'V'#253'straha'
      'O'#269'ek'#225'vejte 40 km/h'
      '40 km/h a volno'
      'Sv'#237't'#237' v'#353'e (rezerva)'
      '40 km/h a v'#253'straha'
      '40 km/h a o'#269'ek'#225'vejte 40 km/h'
      'P'#345'ivol'#225'vac'#237' n'#225'v'#283'st'
      'Dovolen zaji'#353't'#283'n'#253' posun'
      'Dovolen nezaji'#353't'#283'n'#253' posun'
      'Opakov'#225'n'#237' n'#225'v'#283'sti volno'
      'Opakov'#225'n'#237' n'#225'v'#283'sti v'#253'straha'
      'N'#225'v'#283'stidlo zhasl'#233
      'Opakov'#225'n'#237' n'#225'v'#283'sti o'#269'ek'#225'vejte 40 km/h'
      'Opakov'#225'n'#237' n'#225'v'#283'sti v'#253'straha a 40 km/h'
      'Opakov'#225'n'#237' n'#225'v'#283'sti o'#269'ek'#225'vejte 40 km/h a 40 km/h')
  end
  object CB_Selected: TComboBox
    Left = 120
    Top = 35
    Width = 249
    Height = 21
    Style = csDropDownList
    TabOrder = 1
    Items.Strings = (
      'ne'
      'vlakov'#225' cesta'
      'posunov'#225' cesta'
      'nouzov'#225' cesta'
      'nouzov'#225' posunov'#225' cesta')
  end
  object CHB_ZAM: TCheckBox
    Left = 8
    Top = 72
    Width = 105
    Height = 17
    Caption = 'Zamknuto do st'#367'j'
    TabOrder = 2
  end
  object CHB_Falling: TCheckBox
    Left = 8
    Top = 161
    Width = 97
    Height = 17
    Caption = 'Pad'#225' do st'#367'j'
    Enabled = False
    TabOrder = 3
  end
  object CHB_PN: TCheckBox
    Left = 8
    Top = 184
    Width = 161
    Height = 17
    Caption = 'P'#345'ivol'#225'vac'#237' n'#225'v'#283'st aktivn'#237' od:'
    Enabled = False
    TabOrder = 4
  end
  object CHB_Autoblok: TCheckBox
    Left = 8
    Top = 207
    Width = 65
    Height = 17
    Caption = 'Autoblok'
    Enabled = False
    TabOrder = 5
  end
  object GB_RNZ: TGroupBox
    Left = 384
    Top = 0
    Width = 361
    Height = 219
    Caption = ' Ru'#353'en'#237' nouzov'#253'ch z'#225'v'#283'r'#367' po nouzov'#233' cest'#283' '
    TabOrder = 6
    object LV_RNZ: TListView
      Left = 2
      Top = 15
      Width = 357
      Height = 202
      Align = alClient
      Columns = <
        item
          Caption = '#'
          Width = 30
        end
        item
          Caption = 'Blok ID'
        end
        item
          Caption = 'Blok'
          Width = 150
        end
        item
          Caption = 'Po'#269'et'
        end>
      MultiSelect = True
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
    end
  end
  object B_Apply: TButton
    Left = 668
    Top = 225
    Width = 75
    Height = 26
    Caption = 'Pou'#382#237't'
    Default = True
    TabOrder = 7
    OnClick = B_ApplyClick
  end
  object B_Refresh: TButton
    Left = 586
    Top = 225
    Width = 76
    Height = 26
    Caption = 'Obnovit'
    TabOrder = 8
    OnClick = B_RefreshClick
  end
  object CHB_Hradlo: TCheckBox
    Left = 8
    Top = 230
    Width = 65
    Height = 17
    Caption = 'Hradlo'
    Enabled = False
    TabOrder = 9
  end
end
