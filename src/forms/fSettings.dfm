object F_Options: TF_Options
  Left = 0
  Top = 184
  ActiveControl = PC_1
  AlphaBlendValue = 200
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Nastaven'#237
  ClientHeight = 529
  ClientWidth = 555
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  Icon.Data = {
    0000010001001010000001002000680400001600000028000000100000002000
    0000010020000000000000040000120B0000120B000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    00000000000000000000B15E2106B5621CBDC57A1FF9B66314EAA35019300000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000BB6A240FBE7127E1E9B244FFF2C34CFFEDB73AFFBB6B1CEE0000
    0000000000000000000000000000000000000000000000000000000000000000
    0000C6792B0AC9812FE0EAB54BFFEDBC4FFFEAB851FFF0C04CFFC77C25FF0000
    000000000000000000000000000000000000000000000000000000000000CF88
    320FD48F3CE0E4AA47FFE2A643FFE2A844FFDC9D3EFFC06E24FFC1742AB60000
    0000000000000000000000000000000000000000000000000000DB9D3B0CDEA4
    45E0DCA042FFD48F35FFD69238FFD08A33FFB76221FFC97E2EE1BD6F270B0000
    00000000000000000000000000000000000000000000ECAB3806E3AB41DDCF8D
    3CFFC87C2DFFC97F2FFFC27328FFB86423FFD6933CE0CA7F2D0A000000000000
    00000000000000000000000000000000000000000000804A4761EBB75CFFAB4E
    08FFB26121FFB3601FFFB76426FFE1A646E0D18C330F00000000000000000000
    0000000000005C30321A6033365C6535384F6A3A3C75C7B7C4FFF0EFF1FFEBBA
    59FFA24304FFAB5B20FFE9B34CE0DD9B390A0000000000000000000000000000
    0000693F4193AF9195FFDFD1D5FFDDD1D3FFDED1D3FFE0D4D3FFD8D0DCFFE6E2
    E3FFE1AE52FFE9AE40DBE2A23B0F00000000000000000000000000000000794E
    5165D6C4C6FFFFFFFFFFF3F0F0FFEFEBEDFFF5F1F3FFD5C8C8FFD1BFBFFFB19E
    B9FFA4684064EDAB34090000000000000000000000000000000000000000A485
    88E5FFFFFFFFC6B5B6FFEAE6E6FFF9F7F8FFF3EDEEFFF9F8F7FFE2DAD9FF683D
    4D5D00000000000000000000000000000000000000000000000000000000E0D3
    D3FFD9C8CAFF6034395D826065B0FFFFFFFFFFFFFFFFFDFCFDFFFAFAF9FF885C
    657000000000000000000000000000000000000000000000000000000000A281
    87FF835C636900000000000000008F6A72A8F5F1F1FFFFFFFFFFFFFFFFFF8A64
    6D82000000000000000000000000000000000000000000000000000000008C66
    6E34000000000000000000000000916D75A8F3EFEFFFFFFFFFFFD9CCCFFF8A65
    6A19000000000000000000000000000000000000000000000000000000000000
    000000000000AE90960BAF929AC7FFFFFFFFFFFFFFFFF2F2F1FF98777C910000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000A78A905CCFBBC0FFE5DDDDF5C1AAB0E39F808867000000000000
    000000000000000000000000000000000000000000000000000000000000FFE0
    0000FFC00000FF800000FF000000FE000000FC010000FC030000C0070000800F
    0000001F0000007F0000007F0000307F0000707F0000C0FF0000C1FF0000}
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object B_pouzit: TButton
    Left = 8
    Top = 496
    Width = 75
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Ulo'#382'it'
    TabOrder = 0
    OnClick = B_pouzitClick
  end
  object B_OK: TButton
    Left = 87
    Top = 496
    Width = 75
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Zav'#345#237't'
    TabOrder = 1
    OnClick = B_OKClick
  end
  object PC_1: TPageControl
    Left = 0
    Top = 0
    Width = 555
    Height = 489
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    ActivePage = TS_Options
    Align = alTop
    MultiLine = True
    TabOrder = 2
    OnChange = PC_1Change
    object TS_Options: TTabSheet
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Konfigurace'
      ImageIndex = 4
      object P_ON_Pozadi: TPanel
        Left = 0
        Top = 0
        Width = 547
        Height = 461
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alClient
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 0
        object GB_ON_1: TGroupBox
          Left = 8
          Top = 8
          Width = 329
          Height = 49
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Caption = ' Glob'#225'ln'#237' konfigura'#269'n'#237' soubor  '
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          object E_dataload: TEdit
            Left = 8
            Top = 16
            Width = 313
            Height = 21
            Hint = 'cesta k souboru klonfigurace'
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            ParentColor = True
            ReadOnly = True
            TabOrder = 0
          end
        end
        object GB_ON_2: TGroupBox
          Left = 8
          Top = 64
          Width = 329
          Height = 289
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Caption = ' V'#353'eobecn'#233' nastaven'#237'  '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          object GB_Log: TGroupBox
            Left = 8
            Top = 16
            Width = 313
            Height = 49
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = ' Logov'#225'n'#237' syst'#233'mu '
            TabOrder = 0
            object CHB_Log_console: TCheckBox
              Left = 14
              Top = 17
              Width = 297
              Height = 17
              Hint = 'Ukl'#225'd'#225'n'#237' logu vyuziti prihlasovani'
              Margins.Left = 2
              Margins.Top = 2
              Margins.Right = 2
              Margins.Bottom = 2
              Caption = 'Logovat konzoli'
              ParentShowHint = False
              ShowHint = True
              TabOrder = 0
            end
          end
          object GB_OnStart: TGroupBox
            Left = 8
            Top = 69
            Width = 313
            Height = 52
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = ' Po startu programu '
            TabOrder = 1
            object CHB_povolit_spusteni: TCheckBox
              Left = 14
              Top = 19
              Width = 297
              Height = 17
              Margins.Left = 2
              Margins.Top = 2
              Margins.Right = 2
              Margins.Bottom = 2
              Caption = 'Automaticky spustit syst'#233'my'
              TabOrder = 0
            end
          end
          object GB_Autosave: TGroupBox
            Left = 8
            Top = 127
            Width = 313
            Height = 72
            Caption = ' Automatick'#233' ulo'#382'en'#237' stavu koleji'#353't'#283' '
            TabOrder = 2
            object Label1: TLabel
              Left = 14
              Top = 40
              Width = 77
              Height = 13
              Caption = 'Perioda (mm:ss):'
            end
            object CHB_Autosave: TCheckBox
              Left = 14
              Top = 16
              Width = 57
              Height = 17
              Caption = 'Povolit'
              TabOrder = 0
              OnClick = CHB_AutosaveClick
            end
            object ME_autosave_period: TMaskEdit
              Left = 112
              Top = 36
              Width = 89
              Height = 24
              Hint = 'Zadejte aktu'#225'ln'#237' modelov'#253' cas'
              Margins.Left = 2
              Margins.Top = 2
              Margins.Right = 2
              Margins.Bottom = 2
              EditMask = '!90:00;1;_'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBlack
              Font.Height = -13
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              MaxLength = 5
              ParentFont = False
              ParentShowHint = False
              ShowHint = True
              TabOrder = 1
              Text = '00:00'
              OnExit = CHB_AutosaveClick
            end
          end
          object GB_Scale: TGroupBox
            Left = 8
            Top = 205
            Width = 313
            Height = 72
            Caption = ' M'#283#345#237'tko koleji'#353't'#283' '
            TabOrder = 3
            object Label2: TLabel
              Left = 14
              Top = 20
              Width = 217
              Height = 13
              Caption = 'Pro v'#253'po'#269'et ujet'#233' vzd'#225'lenosti hnac'#237'ch vozidel'
            end
            object Label3: TLabel
              Left = 17
              Top = 39
              Width = 14
              Height = 20
              Caption = '1:'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBlack
              Font.Height = -16
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
            end
            object E_Scale: TEdit
              Left = 36
              Top = 39
              Width = 55
              Height = 21
              NumbersOnly = True
              TabOrder = 0
              Text = '1'
            end
          end
        end
        object GB_PrijmutaData: TGroupBox
          Left = 344
          Top = 8
          Width = 193
          Height = 161
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Caption = ' Interval hlavn'#237'ho '#269'asova'#269'e '
          TabOrder = 2
          object L_Data1: TLabel
            Left = 8
            Top = 16
            Width = 56
            Height = 13
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'Milisekund :'
          end
          object LB_Timer: TListBox
            Left = 8
            Top = 32
            Width = 177
            Height = 121
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            ItemHeight = 13
            Items.Strings = (
              '10'
              '20'
              '25'
              '50'
              '100'
              '200'
              '250'
              '500'
              '1000')
            TabOrder = 0
            OnClick = LB_TimerClick
          end
        end
      end
    end
    object TS_SS: TTabSheet
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Snadn'#233' spu'#353't'#283'n'#237
      ImageIndex = 5
      object P_SS: TPanel
        Left = 0
        Top = 0
        Width = 547
        Height = 461
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alClient
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 0
        object L_SS_02: TLabel
          Left = 200
          Top = 142
          Width = 161
          Height = 13
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Alignment = taCenter
          AutoSize = False
          Caption = 'Spustit automatick'#253' re'#382'im'
        end
        object L_SS_01: TLabel
          Left = 200
          Top = 94
          Width = 161
          Height = 13
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Alignment = taCenter
          AutoSize = False
          Caption = 'Adresa RCS modulu'
        end
        object CB_SS_AutRezimy: TComboBox
          Left = 200
          Top = 158
          Width = 161
          Height = 21
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Style = csDropDownList
          TabOrder = 0
        end
        object GB_SS_Vystupy: TGroupBox
          Left = 104
          Top = 190
          Width = 161
          Height = 145
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Caption = ' RCS v'#253'stupy '
          TabOrder = 1
          object L_SS_Out_1: TLabel
            Left = 8
            Top = 16
            Width = 58
            Height = 13
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'P'#345'ipraveno :'
          end
          object L_SS_Out_2: TLabel
            Left = 8
            Top = 40
            Width = 51
            Height = 13
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'Spu'#353'teno :'
          end
          object L_SS_Out_3: TLabel
            Left = 8
            Top = 64
            Width = 68
            Height = 13
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'Pozastaveno :'
          end
          object L_SS_Out_4: TLabel
            Left = 8
            Top = 88
            Width = 45
            Height = 13
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'Vypnuto :'
          end
          object L_SS_Out_5: TLabel
            Left = 8
            Top = 112
            Width = 59
            Height = 13
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'Opakov'#225'n'#237' :'
          end
          object SE_SS_Out_Ready: TSpinEdit
            Left = 104
            Top = 16
            Width = 49
            Height = 22
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            MaxValue = 15
            MinValue = 0
            TabOrder = 1
            Value = 0
          end
          object SE_SS_Out_Start: TSpinEdit
            Left = 104
            Top = 40
            Width = 49
            Height = 22
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            MaxValue = 15
            MinValue = 0
            TabOrder = 3
            Value = 0
          end
          object SE_SS_Out_Pause: TSpinEdit
            Left = 104
            Top = 64
            Width = 49
            Height = 22
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            MaxValue = 15
            MinValue = 0
            TabOrder = 5
            Value = 0
          end
          object SE_SS_Out_Stop: TSpinEdit
            Left = 104
            Top = 88
            Width = 49
            Height = 22
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            MaxValue = 15
            MinValue = 0
            TabOrder = 7
            Value = 0
          end
          object CHB_SS_Out_Ready: TCheckBox
            Left = 80
            Top = 16
            Width = 17
            Height = 17
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            TabOrder = 0
            OnClick = CHB_SS_Out_ReadyClick
          end
          object CHB_SS_Out_Start: TCheckBox
            Left = 80
            Top = 40
            Width = 17
            Height = 17
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'CHB_SS_Out_Start'
            TabOrder = 2
            OnClick = CHB_SS_Out_ReadyClick
          end
          object CHB_SS_Out_Pause: TCheckBox
            Left = 80
            Top = 64
            Width = 17
            Height = 17
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            TabOrder = 4
            OnClick = CHB_SS_Out_ReadyClick
          end
          object CHB_SS_Out_Stop: TCheckBox
            Left = 80
            Top = 88
            Width = 17
            Height = 17
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            TabOrder = 6
            OnClick = CHB_SS_Out_ReadyClick
          end
          object CHB_SS_OUT_Opakovani: TCheckBox
            Left = 80
            Top = 112
            Width = 17
            Height = 17
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            TabOrder = 8
            OnClick = CHB_SS_Out_ReadyClick
          end
          object SE_SS_Out_Opakovani: TSpinEdit
            Left = 104
            Top = 112
            Width = 49
            Height = 22
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            MaxValue = 15
            MinValue = 0
            TabOrder = 9
            Value = 0
          end
        end
        object GB_SS_Vstupy: TGroupBox
          Left = 272
          Top = 190
          Width = 161
          Height = 145
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Caption = ' RCS vstupy '
          TabOrder = 2
          object L_SS_In_1: TLabel
            Left = 8
            Top = 16
            Width = 46
            Height = 13
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'Zapnout :'
          end
          object L_SS_In_2: TLabel
            Left = 8
            Top = 40
            Width = 55
            Height = 13
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'Pozastavit :'
          end
          object L_SS_In_3: TLabel
            Left = 8
            Top = 64
            Width = 45
            Height = 13
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'Vypnout :'
          end
          object L_SS_In_4: TLabel
            Left = 8
            Top = 88
            Width = 53
            Height = 13
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'Opakovat :'
          end
          object L_SS_In_5: TLabel
            Left = 8
            Top = 112
            Width = 34
            Height = 13
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'Reset :'
          end
          object SE_SS_In_Start: TSpinEdit
            Left = 104
            Top = 16
            Width = 49
            Height = 22
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            MaxValue = 15
            MinValue = 0
            TabOrder = 1
            Value = 0
          end
          object SE_SS_In_Pause: TSpinEdit
            Left = 104
            Top = 40
            Width = 49
            Height = 22
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            MaxValue = 15
            MinValue = 0
            TabOrder = 3
            Value = 0
          end
          object SE_SS_In_Stop: TSpinEdit
            Left = 104
            Top = 64
            Width = 49
            Height = 22
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            MaxValue = 15
            MinValue = 0
            TabOrder = 5
            Value = 0
          end
          object SE_SS_In_Repeat: TSpinEdit
            Left = 104
            Top = 88
            Width = 49
            Height = 22
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            MaxValue = 15
            MinValue = 0
            TabOrder = 7
            Value = 0
          end
          object CHB_SS_In_Start: TCheckBox
            Left = 80
            Top = 16
            Width = 17
            Height = 17
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            TabOrder = 0
            OnClick = CHB_SS_Out_ReadyClick
          end
          object CHB_SS_In_Pause: TCheckBox
            Left = 80
            Top = 40
            Width = 17
            Height = 17
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            TabOrder = 2
            OnClick = CHB_SS_Out_ReadyClick
          end
          object CHB_SS_In_Stop: TCheckBox
            Left = 80
            Top = 64
            Width = 17
            Height = 17
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            TabOrder = 4
            OnClick = CHB_SS_Out_ReadyClick
          end
          object CHB_SS_In_Repeat: TCheckBox
            Left = 80
            Top = 88
            Width = 17
            Height = 17
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            TabOrder = 6
            OnClick = CHB_SS_Out_ReadyClick
          end
          object CHB_SS_In_Reset: TCheckBox
            Left = 80
            Top = 112
            Width = 17
            Height = 17
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            TabOrder = 8
            OnClick = CHB_SS_Out_ReadyClick
          end
          object SE_SS_In_Reset: TSpinEdit
            Left = 104
            Top = 112
            Width = 49
            Height = 22
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            MaxValue = 15
            MinValue = 0
            TabOrder = 9
            Value = 0
          end
        end
        object B_SS_Save: TButton
          Left = 1112
          Top = 408
          Width = 63
          Height = 25
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Caption = 'Pou'#382#237't'
          TabOrder = 3
          OnClick = B_SS_SaveClick
        end
        object CHB_SS_Enable: TCheckBox
          Left = 252
          Top = 61
          Width = 53
          Height = 17
          Caption = 'Povolit'
          TabOrder = 4
          OnClick = CHB_SS_EnableClick
        end
        object SE_SS_RCSAdr: TSpinEdit
          Left = 204
          Top = 112
          Width = 157
          Height = 22
          MaxValue = 191
          MinValue = 1
          TabOrder = 5
          Value = 1
        end
      end
    end
    object TS_DigiRych: TTabSheet
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Rychlosti'
      ImageIndex = 25
      object LV_DigiRych: TListView
        Left = 0
        Top = 0
        Width = 547
        Height = 461
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alClient
        Color = clWhite
        Columns = <
          item
            Caption = 'Stupe'#328
          end
          item
            Caption = 'Rychlost'
            Width = 100
          end>
        GridLines = True
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnDblClick = LV_DigiRychDblClick
      end
    end
  end
end
