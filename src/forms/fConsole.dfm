object F_Console: TF_Console
  Left = 562
  Top = 381
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  Caption = 'Konzole'
  ClientHeight = 443
  ClientWidth = 570
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnResize = FormResize
  TextHeight = 13
  object M_console: TMemo
    Left = 0
    Top = 0
    Width = 570
    Height = 401
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Align = alTop
    Color = clWhite
    DragCursor = crDefault
    Font.Charset = EASTEUROPE_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Fixedsys'
    Font.Style = []
    ParentFont = False
    PopupMenu = PM_Console
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 2
    ExplicitWidth = 562
  end
  object E_console: TEdit
    Left = 7
    Top = 409
    Width = 490
    Height = 23
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Color = clWhite
    Font.Charset = EASTEUROPE_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Fixedsys'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
  end
  object B_ok: TButton
    Left = 507
    Top = 409
    Width = 57
    Height = 23
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Prove'#271
    Default = True
    TabOrder = 1
    OnClick = B_okClick
  end
  object PM_Console: TPopupMenu
    Left = 8
    Top = 8
    object PM_DeleteConsole: TMenuItem
      Caption = 'Smazat konzoli'
      OnClick = PM_DeleteConsoleClick
    end
  end
end
