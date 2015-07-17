object F_Options: TF_Options
  Left = 0
  Top = 184
  ActiveControl = PC_1
  AlphaBlendValue = 200
  BorderIcons = [biSystemMenu]
  Caption = 'Nastaven'#237
  ClientHeight = 527
  ClientWidth = 1190
  Color = clBtnFace
  Constraints.MaxHeight = 585
  Constraints.MaxWidth = 1625
  Constraints.MinHeight = 574
  Constraints.MinWidth = 813
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWhite
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000088888888888888888888888888888880000
    00000000000000000000000000080F8888888888888888888888888888080F7F
    7F7F7F7F7F7F7F7F7F7F7F7F78080FF7F7F7F7700000700000700000F8080F7F
    7F7F7F7FFFF07FFFF07FFFF078080FF7F7F7F7777777777777777777F8080F77
    000000007F7F7F770000000078080FF7FFFFFFF0F77007F7FFFFFFF0F8080F77
    FFFFFFF07F7F0F77FFFFFFF078080FF7F000FFF0F77777F7FFFFFFF0F8080F77
    FFFFFFF07F7F7F77FFFFFFF078080FF7F00000F0F77007F7FFFFFFF0F8080F77
    FFFFFFF07F7F0F77FFFFFFF078080FF7F0000FF0F77777F7F00000F0F8080F77
    FFFFFFF07F7F7F77FFFFFFF078080FF7F00000F0F77007F7F000FFF0F8080F77
    FFFFFFF07F7F0F77FFFFFFF078080FF777777777F77777F777777777F8080F7F
    7F7F7F7F7F7F7F7F7F7F7F7F78080FF0000007F7F7F7F7F0000007F7F8080F7F
    7F7F7F7F7F7F7F7F7F7F7F7F78080FFFFFFFFFFFFFFFFFFFFFFFFFFFFF080000
    00000000000000000000000000080CCCCCCCCCCCCCCCCCCCCCCC070707080000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000FFFF
    FFFFFFFFFFFFFFFFFFFF80000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000001FFFFFFFFFFFFFFFFFFFFFFFF}
  Menu = MM_Options
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
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
    Caption = 'OK'
    TabOrder = 1
    OnClick = B_OKClick
  end
  object PC_1: TPageControl
    Left = 0
    Top = 0
    Width = 1190
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
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object P_ON_Pozadi: TPanel
        Left = 0
        Top = 0
        Width = 1182
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
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
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
            Color = clSilver
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            ReadOnly = True
            TabOrder = 0
          end
        end
        object GB_ON_2: TGroupBox
          Left = 8
          Top = 64
          Width = 329
          Height = 345
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Caption = ' Nastaven'#237' syst'#233'mu '
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
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBlack
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              ParentShowHint = False
              ShowHint = True
              TabOrder = 0
            end
          end
          object GB_OnStart: TGroupBox
            Left = 8
            Top = 69
            Width = 313
            Height = 72
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
              Caption = 'Automaticky spustit systemy'
              TabOrder = 0
            end
          end
          object GB_Autosave: TGroupBox
            Left = 8
            Top = 145
            Width = 313
            Height = 72
            Caption = ' Automatick'#233' ulo'#382'en'#237' stavu koleji'#353't'#283' '
            TabOrder = 2
            object Label1: TLabel
              Left = 16
              Top = 40
              Width = 77
              Height = 13
              Caption = 'Perioda (mm:ss):'
            end
            object CHB_Autosave: TCheckBox
              Left = 16
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
        end
        object GB_ModelovyCas: TGroupBox
          Left = 344
          Top = 176
          Width = 193
          Height = 233
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Caption = ' Modelov'#253' cas '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 3
          object L_ModCas_2: TLabel
            Left = 8
            Top = 56
            Width = 69
            Height = 13
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'Sekundy port :'
          end
          object L_ModCas_1: TLabel
            Left = 8
            Top = 16
            Width = 104
            Height = 13
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'MTB adresa v'#253'stupu :'
          end
          object L_ModCas_3: TLabel
            Left = 8
            Top = 88
            Width = 58
            Height = 13
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'Minuty port :'
          end
          object L_ModCas_4: TLabel
            Left = 8
            Top = 120
            Width = 60
            Height = 13
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'Hodiny port :'
          end
          object L_ModCas_5: TLabel
            Left = 8
            Top = 152
            Width = 93
            Height = 13
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'N'#225'sobi'#269' pret'#225#269'en'#237' :'
          end
          object SE_ModCas_Sekundy: TSpinEdit
            Left = 128
            Top = 56
            Width = 57
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
            TabOrder = 0
            Value = 0
          end
          object SE_ModCas_Minuty: TSpinEdit
            Left = 128
            Top = 88
            Width = 57
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
            TabOrder = 1
            Value = 0
          end
          object SE_ModCas_Hodiny: TSpinEdit
            Left = 128
            Top = 120
            Width = 57
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
          object CHB_ModCas_Sekundy: TCheckBox
            Left = 104
            Top = 56
            Width = 17
            Height = 17
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            TabOrder = 3
          end
          object CHB_ModCas_Minuty: TCheckBox
            Left = 104
            Top = 88
            Width = 17
            Height = 17
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            TabOrder = 4
          end
          object CHB_ModCas_Hodiny: TCheckBox
            Left = 104
            Top = 120
            Width = 17
            Height = 17
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            TabOrder = 5
          end
          object B_ModCas_Save: TButton
            Left = 8
            Top = 200
            Width = 65
            Height = 25
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'Pou'#382#237't'
            TabOrder = 6
            OnClick = B_ModCas_SaveClick
          end
          object CB_ModCasPretaceni: TComboBox
            Left = 8
            Top = 168
            Width = 177
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
            TabOrder = 7
            Items.Strings = (
              #381#225'dn'#233' n'#225'soben'#237
              'Hodiny*minuty -> minuty'
              'Hodiny*minuty*sekundy -> sekundy')
          end
          object SE_ModCas_MtbAdr: TSpinEdit
            Left = 7
            Top = 29
            Width = 176
            Height = 22
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            MaxValue = 191
            MinValue = 1
            ParentFont = False
            TabOrder = 8
            Value = 1
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
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
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
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
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
            ParentFont = False
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
      Caption = 'Snadn'#233' spu'#353'ten'#237
      ImageIndex = 5
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object P_SS: TPanel
        Left = 0
        Top = 0
        Width = 1182
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
          Left = 560
          Top = 126
          Width = 161
          Height = 13
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Alignment = taCenter
          AutoSize = False
          Caption = 'Spustit automatick'#253' re'#382'im'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object L_SS_01: TLabel
          Left = 560
          Top = 78
          Width = 161
          Height = 13
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Alignment = taCenter
          AutoSize = False
          Caption = 'Adresa MTB modulu'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object CB_SS_AutRezimy: TComboBox
          Left = 560
          Top = 142
          Width = 161
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
          ItemHeight = 0
          ParentFont = False
          TabOrder = 0
        end
        object GB_SS_Vystupy: TGroupBox
          Left = 464
          Top = 174
          Width = 161
          Height = 145
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Caption = ' MTB v'#253'stupy '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          object L_SS_Out_1: TLabel
            Left = 8
            Top = 16
            Width = 57
            Height = 13
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'Pripraveno :'
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
            Width = 58
            Height = 13
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'Opakovani :'
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
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            MaxValue = 15
            MinValue = 0
            ParentFont = False
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
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            MaxValue = 15
            MinValue = 0
            ParentFont = False
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
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            MaxValue = 15
            MinValue = 0
            ParentFont = False
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
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            MaxValue = 15
            MinValue = 0
            ParentFont = False
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
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            MaxValue = 15
            MinValue = 0
            ParentFont = False
            TabOrder = 9
            Value = 0
          end
        end
        object GB_SS_Vstupy: TGroupBox
          Left = 632
          Top = 174
          Width = 161
          Height = 145
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Caption = ' MTB vstupy '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
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
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
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
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
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
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
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
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
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
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
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
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            MaxValue = 15
            MinValue = 0
            ParentFont = False
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
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            MaxValue = 15
            MinValue = 0
            ParentFont = False
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
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            MaxValue = 15
            MinValue = 0
            ParentFont = False
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
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            MaxValue = 15
            MinValue = 0
            ParentFont = False
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
            Left = 83
            Top = 67
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
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            MaxValue = 15
            MinValue = 0
            ParentFont = False
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
          Left = 612
          Top = 45
          Width = 53
          Height = 17
          Caption = 'Povolit'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 4
          OnClick = CHB_SS_EnableClick
        end
        object SE_SS_MtbAdr: TSpinEdit
          Left = 564
          Top = 96
          Width = 157
          Height = 22
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          MaxValue = 191
          MinValue = 1
          ParentFont = False
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
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object LV_DigiRych: TListView
        Left = 0
        Top = 0
        Width = 1182
        Height = 461
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alClient
        Color = clWhite
        Columns = <
          item
            Caption = 'Stupen'
          end
          item
            Caption = 'Rychlost'
            Width = 100
          end>
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        GridLines = True
        ReadOnly = True
        RowSelect = True
        ParentFont = False
        TabOrder = 0
        ViewStyle = vsReport
        OnDblClick = LV_DigiRychDblClick
      end
    end
    object TS_Centrala: TTabSheet
      Caption = 'Centr'#225'la'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GB_Centrala: TGroupBox
        Left = 522
        Top = 80
        Width = 303
        Height = 288
        TabOrder = 0
        object B_Save: TButton
          Left = 205
          Top = 254
          Width = 80
          Height = 24
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Caption = 'Pou'#382#237't'
          Default = True
          TabOrder = 0
          OnClick = B_SaveClick
        end
        object GB_TrackSystem: TGroupBox
          Left = 19
          Top = 21
          Width = 267
          Height = 219
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Caption = ' Komunikace s centr'#225'lou  '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          object Label3: TLabel
            Left = 13
            Top = 16
            Width = 21
            Height = 13
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'Typ:'
          end
          object Label4: TLabel
            Left = 13
            Top = 46
            Width = 46
            Height = 13
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'Baudrate:'
          end
          object Label5: TLabel
            Left = 13
            Top = 74
            Width = 45
            Height = 13
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'Data-bity:'
          end
          object Label6: TLabel
            Left = 13
            Top = 104
            Width = 44
            Height = 13
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'Stop-bity:'
          end
          object Label7: TLabel
            Left = 13
            Top = 133
            Width = 22
            Height = 13
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'Port:'
          end
          object Label8: TLabel
            Left = 13
            Top = 161
            Width = 43
            Height = 13
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'Loglevel:'
          end
          object Label2: TLabel
            Left = 13
            Top = 189
            Width = 58
            Height = 13
            Caption = #344#237'zen'#237' toku:'
          end
          object CB_TrackSystem: TComboBox
            Left = 104
            Top = 16
            Width = 150
            Height = 21
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Style = csDropDownList
            Enabled = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -9
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ItemHeight = 13
            ParentFont = False
            TabOrder = 0
            Items.Strings = (
              'Intellibox'
              'NanoX (LI100F)')
          end
          object CCB_BaudRate: TComComboBox
            Left = 104
            Top = 46
            Width = 150
            Height = 21
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            ComProperty = cpBaudRate
            Text = 'Custom'
            Style = csDropDownList
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -9
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ItemHeight = 13
            ItemIndex = 0
            ParentFont = False
            TabOrder = 1
          end
          object CCB_DataBits: TComComboBox
            Left = 104
            Top = 74
            Width = 150
            Height = 21
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            ComProperty = cpDataBits
            Text = '5'
            Style = csDropDownList
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -9
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ItemHeight = 13
            ItemIndex = 0
            ParentFont = False
            TabOrder = 2
          end
          object CCB_StopBits: TComComboBox
            Left = 104
            Top = 104
            Width = 150
            Height = 21
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            ComProperty = cpStopBits
            Text = '1'
            Style = csDropDownList
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -9
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ItemHeight = 13
            ItemIndex = 0
            ParentFont = False
            TabOrder = 3
          end
          object CCB_Port: TComComboBox
            Left = 104
            Top = 133
            Width = 150
            Height = 21
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            ComProperty = cpPort
            Text = 'COM1'
            Style = csDropDownList
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -9
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ItemHeight = 13
            ItemIndex = 0
            ParentFont = False
            TabOrder = 4
          end
          object SE_LogLevel: TSpinEdit
            Left = 103
            Top = 161
            Width = 150
            Height = 22
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -9
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            MaxValue = 10
            MinValue = 0
            ParentFont = False
            TabOrder = 5
            Value = 0
          end
          object B_PortRefresh: TButton
            Left = 50
            Top = 136
            Width = 49
            Height = 17
            Caption = 'Refresh'
            TabOrder = 6
            OnClick = B_PortRefreshClick
          end
          object CCB_FC: TComComboBox
            Left = 103
            Top = 189
            Width = 150
            Height = 21
            ComProperty = cpFlowControl
            Text = 'Hardware'
            Style = csDropDownList
            ItemHeight = 13
            ItemIndex = 0
            TabOrder = 7
          end
        end
      end
    end
  end
  object MM_Options: TMainMenu
    Images = F_Main.IL_Menu
    Left = 760
    Top = 608
    object M_log: TMenuItem
      Caption = 'Log'
      object PM_log_delete: TMenuItem
        Caption = 'Smazat soubory log'
        ImageIndex = 42
        OnClick = PM_log_deleteClick
      end
    end
  end
  object OD_Open: TOpenDialog
    FileName = 'konfigurace.ini'
    Filter = 
      'Konfigurace|*.ini|Soupravy|*.ini|Prejezdy|*.ini|Bloky|*.ini|Stan' +
      'ice|*.ini|MTB|*.ini|Automaticke rezimy|*.ini|Zesilovace|*.ini|Tr' +
      'ate|*.ini|Vysvetlivky|*.csv'
    InitialDir = 'D:\Users\Vlak\Delphi'
    Title = 'Nac'#237'st konfiguracn'#237' soubor'
    Left = 824
    Top = 608
  end
  object SD_Save: TSaveDialog
    FileName = 'konfigurace.ini'
    Filter = 
      'Konfigurace|*.ini|Soupravy|*.ini|Prejezdy|*.ini|Bloky|*.ini|Stan' +
      'ice|-.ini|MTB|*.ini|Automaticke rezimy|*.ini|Zesilovace|*.ini|Tr' +
      'ate|*.ini|Vysvetlivky|*.csv'
    InitialDir = 'D:\Users\Vlak\Delphi'
    Options = [ofHideReadOnly, ofExtensionDifferent, ofEnableSizing]
    Title = 'Ulo'#382'it konfiguracn'#237' soubor'
    Left = 792
    Top = 608
  end
end
