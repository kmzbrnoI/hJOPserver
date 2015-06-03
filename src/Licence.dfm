object F_Licence: TF_Licence
  Left = 898
  Top = 202
  BorderIcons = []
  BorderStyle = bsToolWindow
  Caption = 'Licence'
  ClientHeight = 209
  ClientWidth = 297
  Color = clBlack
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
  PixelsPerInch = 96
  TextHeight = 13
  object B_Demo: TButton
    Left = 104
    Top = 176
    Width = 97
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Demo verze'
    TabOrder = 0
    OnClick = B_DemoClick
  end
  object PC_1: TPageControl
    Left = 8
    Top = 8
    Width = 281
    Height = 161
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    ActivePage = TS_Licence
    TabOrder = 1
    object TS_Licence: TTabSheet
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Licence'
      object L_Lic4: TLabel
        Left = 8
        Top = 8
        Width = 72
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Licencn'#237' c'#237'slo :'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object ME_Licence: TMaskEdit
        Left = 8
        Top = 32
        Width = 217
        Height = 40
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        EditMask = '000-000-000-000;1;_'
        Font.Charset = EASTEUROPE_CHARSET
        Font.Color = clWindowText
        Font.Height = -27
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        MaxLength = 15
        ParentFont = False
        TabOrder = 0
        Text = '000-000-000-000'
      end
      object B_Submit: TButton
        Left = 190
        Top = 104
        Width = 75
        Height = 25
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Odeslat'
        Default = True
        TabOrder = 1
        OnClick = B_SubmitClick
      end
    end
    object TS_Spolecnost: TTabSheet
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Multilicence'
      ImageIndex = 1
      object L_Lic1: TLabel
        Left = 8
        Top = 8
        Width = 37
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Jm'#233'no :'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object L_Lic2: TLabel
        Left = 8
        Top = 40
        Width = 46
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Prij'#237'men'#237' :'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object L_Lic3: TLabel
        Left = 8
        Top = 72
        Width = 59
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Spolecnost :'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object B_MutiApply: TButton
        Left = 192
        Top = 104
        Width = 75
        Height = 25
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Pou'#382#237't'
        TabOrder = 0
        OnClick = B_MutiApplyClick
      end
      object E_Multi_UserFName: TEdit
        Left = 104
        Top = 8
        Width = 161
        Height = 21
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        MaxLength = 32
        TabOrder = 1
      end
      object E_Multi_UserSName: TEdit
        Left = 104
        Top = 40
        Width = 161
        Height = 21
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        MaxLength = 32
        TabOrder = 2
      end
      object E_Multi_Company: TEdit
        Left = 104
        Top = 72
        Width = 161
        Height = 21
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        MaxLength = 32
        TabOrder = 3
      end
    end
    object TS_User: TTabSheet
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'U'#382'ivatel'
      ImageIndex = 2
      object L_Lic5: TLabel
        Left = 8
        Top = 8
        Width = 37
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Jm'#233'no :'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object L_Lic6: TLabel
        Left = 8
        Top = 40
        Width = 46
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Prij'#237'men'#237' :'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object E_User_UserFName: TEdit
        Left = 104
        Top = 8
        Width = 161
        Height = 21
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        MaxLength = 32
        TabOrder = 0
      end
      object E_User_UserSName: TEdit
        Left = 104
        Top = 40
        Width = 161
        Height = 21
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        MaxLength = 32
        TabOrder = 1
      end
      object B_UzApply: TButton
        Tag = 1
        Left = 192
        Top = 104
        Width = 75
        Height = 25
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Pou'#382#237't'
        TabOrder = 2
        OnClick = B_UzApplyClick
      end
    end
  end
end
