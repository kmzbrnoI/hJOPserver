object F_AutRezEdit: TF_AutRezEdit
  Left = 170
  Top = 170
  ActiveControl = B_Save
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Automaticky rezim : [AC]'
  ClientHeight = 178
  ClientWidth = 332
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
  object L_AR7: TLabel
    Left = 6
    Top = 8
    Width = 77
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Soubor s kroky :'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object GB_AutRez_Statistika: TGroupBox
    Left = 7
    Top = 53
    Width = 314
    Height = 89
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' Statistika '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object L_AutRez_StSpusteno: TLabel
      Left = 8
      Top = 16
      Width = 54
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Spusteno : '
    end
    object L_AutRez_stDokonceno: TLabel
      Left = 8
      Top = 32
      Width = 65
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Dokonceno : '
    end
    object B_AutRez_StDelete: TButton
      Left = 8
      Top = 56
      Width = 113
      Height = 25
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Sma'#382' statistiku'
      TabOrder = 0
      OnClick = B_AutRez_StDeleteClick
    end
  end
  object B_Save: TButton
    Left = 9
    Top = 146
    Width = 73
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Pou'#382#237't'
    Default = True
    TabOrder = 1
    OnClick = B_SaveClick
  end
  object B_Storno: TButton
    Left = 94
    Top = 146
    Width = 73
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Storno'
    TabOrder = 2
    OnClick = B_StornoClick
  end
  object E_Soubor: TEdit
    Left = 6
    Top = 24
    Width = 233
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Color = clSilver
    ReadOnly = True
    TabOrder = 3
  end
  object B_Prochazet: TButton
    Left = 246
    Top = 24
    Width = 75
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Proch'#225'zet'
    TabOrder = 4
    OnClick = B_ProchazetClick
  end
  object OD_File: TOpenDialog
    Filter = 'Kroky aut rezimu (*.krk)|*.krk'
    FilterIndex = 0
    Title = 'Vyberte soubor'
    Left = 288
    Top = 120
  end
end
