object F_Main: TF_Main
  Left = 264
  Top = 316
  Caption = 'hJOPserver'
  ClientHeight = 675
  ClientWidth = 1297
  Color = clBtnFace
  Constraints.MinHeight = 244
  Constraints.MinWidth = 772
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000088888888888888888888888888888880000
    00000000000000000000000000080F8888888888888888888888888888080F7F
    7F7F7F7F7F7F7F7F7F7F7F7F78080FF7F7F7F7F7700000F7F7F7F7F7F8080F7F
    7F7F7F7F7F88F07F7F7F7F7F78080FF7F7F7F7F7777777F7F7F7F7F7F8080F78
    88888888888888888888888878080FF8FFFFFFFFFFFFFFFFFFFFFFF8F8080F78
    F00000000000FFFFFFFFFFF878080FF8F00808800880FF0000FFFFF8F8080F78
    F00808800880FFFFFFFFFFF878080FF8F00008000080FF0000FFFFF8F8080F78
    F08880111000FFFFFFFFFFF878080FF8F00008999980FFFFFFFFFFF8F8080F78
    F088E8899880FF0000000FF878080FF8F088EE8898E0FFFFFFFFFFF8F8080F78
    F08EEEEE8EE0FF0000000FF878080FF8F00000000000FFFFFFFFFFF8F8080F78
    FFFFFFFFFFFFFFFFFFFFFFF878080FF8888888888888888888888888F8080F7F
    7F7F7F7F7F7F7F7F7F7F7F7F78080FFFFFFFFFFFFFFFFFFFFFFFFFFFFF080000
    00000000000000000000000000080CCCCCCCCCCCCCCCCCCCCCCC070707080000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000FFFF
    FFFFFFFFFFFFFFFFFFFF80000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000001FFFFFFFFFFFFFFFFFFFFFFFF}
  Menu = Menu_1
  OldCreateOrder = False
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnPaint = FormPaint
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object P_Pozadi: TPanel
    Left = 0
    Top = 0
    Width = 1297
    Height = 33
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Align = alTop
    BevelOuter = bvNone
    Color = clGray
    ParentBackground = False
    TabOrder = 0
    object P_Date: TPanel
      Left = 888
      Top = 4
      Width = 121
      Height = 25
      Hint = 'Datum'
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      BevelOuter = bvNone
      Caption = '1.1.2000'
      Color = clWhite
      Font.Charset = EASTEUROPE_CHARSET
      Font.Color = clWindowText
      Font.Height = -23
      Font.Name = 'Arial'
      Font.Style = []
      ParentBackground = False
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnDblClick = L_DateDblClick
    end
    object P_Time: TPanel
      Left = 1016
      Top = 4
      Width = 121
      Height = 25
      Hint = 'Cas'
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      BevelOuter = bvNone
      Caption = '0:00:00'
      Color = clWhite
      Font.Charset = EASTEUROPE_CHARSET
      Font.Color = clWindowText
      Font.Height = -23
      Font.Name = 'Arial'
      Font.Style = []
      ParentBackground = False
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnDblClick = L_DateDblClick
    end
    object P_Time_modelovy: TPanel
      Left = 1144
      Top = 4
      Width = 110
      Height = 25
      Hint = 'Modelov'#253' cas'
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      BevelOuter = bvNone
      Caption = '00:00:00'
      Color = clSkyBlue
      Font.Charset = EASTEUROPE_CHARSET
      Font.Color = clRed
      Font.Height = -23
      Font.Name = 'Arial'
      Font.Style = []
      ParentBackground = False
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnDblClick = P_Time_modelovyDblClick
    end
    object P_Zrychleni: TPanel
      Left = 1260
      Top = 4
      Width = 33
      Height = 25
      Hint = 'Prevod modelov'#233'ho casu'
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      BevelOuter = bvNone
      Caption = '1x'
      Color = clSkyBlue
      Font.Charset = EASTEUROPE_CHARSET
      Font.Color = clRed
      Font.Height = -23
      Font.Name = 'Arial'
      Font.Style = []
      ParentBackground = False
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      OnDblClick = P_ZrychleniDblClick
    end
    object P_DCC: TPanel
      Left = 66
      Top = 4
      Width = 50
      Height = 25
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      BevelInner = bvSpace
      BevelOuter = bvNone
      Color = clWhite
      ParentBackground = False
      TabOrder = 4
      object SB_Loconet_Start: TSpeedButton
        Left = 2
        Top = 1
        Width = 23
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Action = A_DCC_Go
        Glyph.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
          0000FF00FF000000000000000000FF00FF00FF00FF00FF00FF00000000000000
          0000FF00FF00FF00FF000000000000000000FF00FF00FF00FF00FF00FF000000
          0000FF00FF0000000000FF00FF0000000000FF00FF0000000000FF00FF00FF00
          FF00FF00FF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
          0000FF00FF0000000000FF00FF0000000000FF00FF0000000000FF00FF00FF00
          FF00FF00FF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
          0000FF00FF0000000000FF00FF0000000000FF00FF0000000000FF00FF00FF00
          FF00FF00FF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
          0000FF00FF000000000000000000FF00FF00FF00FF00FF00FF00000000000000
          0000FF00FF00FF00FF000000000000000000FF00FF00FF00FF00FF00FF000000
          0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
          0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000FF00FF00FF00FF00FF00FF000000
          000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF
          000000FF000000FF000000FF000000000000FF00FF00FF00FF00FF00FF000000
          000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF
          000000FF000000FF000000FF000000000000FF00FF00FF00FF00FF00FF000000
          000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF
          000000FF000000FF000000FF000000000000FF00FF00FF00FF00FF00FF000000
          000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF
          000000FF000000FF000000FF000000000000FF00FF00FF00FF00FF00FF000000
          000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF
          000000FF000000FF000000FF000000000000FF00FF00FF00FF00FF00FF000000
          000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF
          000000FF000000FF000000FF000000000000FF00FF00FF00FF00FF00FF000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000FF00FF00FF00FF00}
        ParentShowHint = False
        ShowHint = True
      end
      object SB_Loconet_Stop: TSpeedButton
        Left = 25
        Top = 1
        Width = 23
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Action = A_DCC_Stop
        Glyph.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
          0000FF00FF000000000000000000FF00FF00FF00FF00FF00FF00000000000000
          0000FF00FF00FF00FF000000000000000000FF00FF00FF00FF00FF00FF000000
          0000FF00FF0000000000FF00FF0000000000FF00FF0000000000FF00FF00FF00
          FF00FF00FF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
          0000FF00FF0000000000FF00FF0000000000FF00FF0000000000FF00FF00FF00
          FF00FF00FF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
          0000FF00FF0000000000FF00FF0000000000FF00FF0000000000FF00FF00FF00
          FF00FF00FF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
          0000FF00FF000000000000000000FF00FF00FF00FF00FF00FF00000000000000
          0000FF00FF00FF00FF000000000000000000FF00FF00FF00FF00FF00FF000000
          0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
          0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000FF00FF00FF00FF00FF00FF000000
          00000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
          FF000000FF000000FF000000FF0000000000FF00FF00FF00FF00FF00FF000000
          00000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
          FF000000FF000000FF000000FF0000000000FF00FF00FF00FF00FF00FF000000
          00000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
          FF000000FF000000FF000000FF0000000000FF00FF00FF00FF00FF00FF000000
          00000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
          FF000000FF000000FF000000FF0000000000FF00FF00FF00FF00FF00FF000000
          00000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
          FF000000FF000000FF000000FF0000000000FF00FF00FF00FF00FF00FF000000
          00000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
          FF000000FF000000FF000000FF0000000000FF00FF00FF00FF00FF00FF000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000FF00FF00FF00FF00}
        ParentShowHint = False
        ShowHint = True
      end
    end
    object P_SystemSet: TPanel
      Left = 8
      Top = 4
      Width = 52
      Height = 25
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      BevelInner = bvSpace
      BevelOuter = bvNone
      Color = clWhite
      ParentBackground = False
      TabOrder = 5
      object SB_SystemStart: TSpeedButton
        Left = 2
        Top = 1
        Width = 24
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Action = A_System_Start
        Glyph.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000000000000000
          0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000000000000000
          000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF
          000000FF000000FF000000FF000000FF00000000000000000000000000000000
          000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF
          000000FF000000FF000000FF000000FF00000000000000000000000000000000
          0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00000000000000000000000000FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        ParentShowHint = False
        ShowHint = True
      end
      object SB_SystemStop: TSpeedButton
        Left = 26
        Top = 1
        Width = 23
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Action = A_System_Stop
        ParentShowHint = False
        ShowHint = True
      end
    end
  end
  object SB1: TStatusBar
    Left = 0
    Top = 655
    Width = 1297
    Height = 20
    Hint = 'Status bar'
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Panels = <
      item
        BiDiMode = bdLeftToRight
        ParentBiDiMode = False
        Text = 'log'
        Width = 700
      end
      item
        Alignment = taCenter
        BiDiMode = bdLeftToRight
        ParentBiDiMode = False
        Text = 'MTB Close'
        Width = 100
      end
      item
        Alignment = taCenter
        Text = 'Centr'#225'la odpojena'
        Width = 100
      end
      item
        Alignment = taCenter
        Text = '--- ; ---'
        Width = 55
      end
      item
        Text = 'Rychlost sbernice : x ms'
        Width = 150
      end
      item
        Text = 'CPU'
        Width = 125
      end>
    ParentShowHint = False
    ShowHint = True
  end
  object PC_1: TPageControl
    Left = 0
    Top = 33
    Width = 1297
    Height = 622
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    ActivePage = TS_FuncsVyznam
    Align = alClient
    MultiLine = True
    TabOrder = 2
    OnChange = PC_1Change
    object TS_Technologie: TTabSheet
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Stav technologie'
      ImageIndex = 15
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GB_Connected_Panels: TGroupBox
        Left = 16
        Top = 206
        Width = 801
        Height = 206
        Caption = ' P'#345'ipojen'#233' panely '
        TabOrder = 0
        object LV_Clients: TListView
          Left = 2
          Top = 15
          Width = 797
          Height = 189
          Align = alClient
          Columns = <
            item
              Caption = 'ID'
            end
            item
              Caption = 'Stav'
            end
            item
              Caption = 'Klient'
            end
            item
              Caption = 'O'#344'1'
            end
            item
              Caption = 'O'#344'2'
            end
            item
              Caption = 'O'#344'3'
            end
            item
              Caption = 'Dal'#353#237' O'#344
            end
            item
              Caption = 'Menu'
            end
            item
              Caption = #352't'#237'tek / v'#253'luka'
            end
            item
              Caption = 'Potvrzovac'#237' sekvence'
            end
            item
              Caption = 'Klient verze protokolu'
            end
            item
              Caption = 'Autorizovan'#253' regul'#225'tor'
            end>
          GridLines = True
          ReadOnly = True
          RowSelect = True
          PopupMenu = PM_Clients
          TabOrder = 0
          ViewStyle = vsReport
          OnCustomDrawItem = LV_ClientsCustomDrawItem
        end
      end
      object GB_technologie_3: TGroupBox
        Left = 16
        Top = 14
        Width = 197
        Height = 186
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = ' Stav technologie '
        ParentShowHint = False
        ShowHint = False
        TabOrder = 1
        object S_MTB_open: TShape
          Left = 16
          Top = 24
          Width = 17
          Height = 9
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Brush.Color = clRed
        end
        object S_MTB_start: TShape
          Left = 16
          Top = 39
          Width = 17
          Height = 9
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Brush.Color = clRed
        end
        object S_Intellibox_connect: TShape
          Left = 16
          Top = 73
          Width = 17
          Height = 9
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Brush.Color = clRed
        end
        object S_Intellibox_go: TShape
          Left = 16
          Top = 92
          Width = 17
          Height = 9
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Brush.Color = clGray
        end
        object S_Server: TShape
          Left = 16
          Top = 140
          Width = 17
          Height = 9
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Brush.Color = clRed
        end
        object L_StavS_1: TLabel
          Left = 40
          Top = 24
          Width = 69
          Height = 13
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Caption = 'MTB otev'#345'eno'
        end
        object L_StavS_2: TLabel
          Tag = 1
          Left = 40
          Top = 39
          Width = 84
          Height = 13
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Caption = 'MTB komunikace'
        end
        object L_StavS_3: TLabel
          Tag = 2
          Left = 40
          Top = 72
          Width = 95
          Height = 13
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Caption = 'P'#345'ipojeno k centr'#225'le'
        end
        object L_StavS_4: TLabel
          Tag = 3
          Left = 40
          Top = 92
          Width = 22
          Height = 13
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Caption = 'DCC'
          ParentShowHint = False
          ShowHint = True
        end
        object L_StavS_6: TLabel
          Tag = 5
          Left = 40
          Top = 140
          Width = 31
          Height = 13
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Caption = 'Server'
        end
        object S_lok_prevzato: TShape
          Left = 16
          Top = 109
          Width = 17
          Height = 9
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Brush.Color = clRed
        end
        object Label1: TLabel
          Tag = 5
          Left = 40
          Top = 109
          Width = 69
          Height = 13
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Caption = 'Loko p'#345'evzato'
        end
        object G_Loko_Prevzato: TGauge
          Left = 114
          Top = 109
          Width = 76
          Height = 14
          ForeColor = clBlue
          MaxValue = 10
          Progress = 0
        end
      end
      object GroupBox1: TGroupBox
        Left = 232
        Top = 14
        Width = 585
        Height = 186
        Caption = ' Log '
        TabOrder = 2
        object LB_Log: TListBox
          Left = 2
          Top = 15
          Width = 581
          Height = 169
          Align = alClient
          ItemHeight = 13
          TabOrder = 0
          OnDblClick = LB_LogDblClick
          OnDrawItem = LB_LogDrawItem
        end
      end
      object GB_Centrala: TGroupBox
        Left = 16
        Top = 418
        Width = 799
        Height = 84
        Caption = ' Centr'#225'la '
        TabOrder = 3
        object Label3: TLabel
          Left = 16
          Top = 24
          Width = 100
          Height = 13
          Caption = 'Verze FW v centr'#225'le:'
        end
        object Label4: TLabel
          Left = 155
          Top = 24
          Width = 54
          Height = 13
          Caption = 'ID centr'#225'ly:'
        end
        object L_CS_FW: TLabel
          Left = 16
          Top = 40
          Width = 43
          Height = 13
          Caption = 'nezn'#225'm'#225
        end
        object L_CS_ID: TLabel
          Left = 155
          Top = 40
          Width = 43
          Height = 13
          Caption = 'nezn'#225'm'#233
        end
        object Label5: TLabel
          Left = 262
          Top = 24
          Width = 42
          Height = 13
          Caption = 'Verze LI:'
        end
        object L_CS_LI_FW: TLabel
          Left = 262
          Top = 40
          Width = 43
          Height = 13
          Caption = 'nezn'#225'm'#225
        end
        object Label6: TLabel
          Left = 404
          Top = 24
          Width = 70
          Height = 13
          Caption = 'Aktualizov'#225'no:'
        end
        object L_CS_UpdateTime: TLabel
          Left = 404
          Top = 43
          Width = 25
          Height = 13
          Caption = 'nikdy'
        end
        object B_CS_Ver_Update: TButton
          Left = 544
          Top = 43
          Width = 75
          Height = 25
          Caption = 'Aktualizovat'
          Enabled = False
          TabOrder = 0
          OnClick = B_CS_Ver_UpdateClick
        end
      end
    end
    object TS_Bloky: TTabSheet
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Bloky'
      ImageIndex = 10
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object LV_Bloky: TListView
        Left = 0
        Top = 40
        Width = 1289
        Height = 554
        Hint = 'Tabulka definovan'#253'ch bloku'
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alClient
        Columns = <
          item
            Caption = 'N'#225'zev'
            Width = 175
          end
          item
            Caption = 'Typ'
            Width = 75
          end
          item
            Alignment = taCenter
            Caption = 'ID'
            Width = 30
          end
          item
            Caption = 'Souprava'
            Width = 75
          end
          item
            Alignment = taCenter
            Caption = 'Stav'
            Width = 125
          end
          item
            Caption = 'Stanice'
            Width = 125
          end
          item
            Caption = 'Stitek'
            Width = 80
          end
          item
            Caption = 'Vyluka'
            Width = 80
          end
          item
            Caption = 'Predpovidana souprava'
            Width = 150
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
        ParentShowHint = False
        PopupMenu = PM_Bloky
        ShowHint = True
        SmallImages = IL_Bloky
        TabOrder = 0
        ViewStyle = vsReport
        OnChange = LV_BlokyChange
        OnCustomDrawItem = LV_BlokyCustomDrawItem
        OnDblClick = LV_BlokyDblClick
      end
      object P_BlkPozadi: TPanel
        Left = 0
        Top = 0
        Width = 1289
        Height = 40
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alTop
        BevelOuter = bvNone
        Color = clGray
        TabOrder = 1
        object P_BlkTlc: TPanel
          Left = 504
          Top = 4
          Width = 217
          Height = 32
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          BevelOuter = bvNone
          Color = clWhite
          TabOrder = 1
          object B_BlkAdd: TButton
            Left = 13
            Top = 3
            Width = 97
            Height = 25
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'Pridat blok'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 0
            OnClick = B_BlkAddClick
          end
          object B_BlkDelete: TButton
            Left = 112
            Top = 3
            Width = 97
            Height = 25
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'Smazat blok'
            Enabled = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 1
            OnClick = B_BlkDeleteClick
          end
        end
        object P_Blk_Ostatni: TPanel
          Left = 992
          Top = 4
          Width = 113
          Height = 32
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          BevelOuter = bvNone
          Color = clWhite
          TabOrder = 2
          object L_BlkPocet: TLabel
            Left = 8
            Top = 10
            Width = 72
            Height = 13
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'Pocet bloku : ?'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
          end
        end
        object P_Blk_Posun: TPanel
          Left = 1112
          Top = 4
          Width = 65
          Height = 32
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          BevelOuter = bvNone
          Color = clWhite
          TabOrder = 3
          object SB_Blk_Up: TSpeedButton
            Left = 8
            Top = 5
            Width = 23
            Height = 22
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Enabled = False
            Glyph.Data = {
              EE000000424DEE0000000000000076000000280000000F0000000F0000000100
              0400000000007800000000000000000000001000000000000000000000000000
              8000008000000080800080000000800080008080000080808000C0C0C0000000
              FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
              8880888888000888888088888800088888808888880008888880888888000888
              8880888888000888888088888800088888808808880008880880880088000880
              0880880008000800088088800000000088808888000000088880888880000088
              888088888800088888808888888888888880}
          end
          object SB_Blk_Down: TSpeedButton
            Left = 35
            Top = 5
            Width = 23
            Height = 22
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Enabled = False
            Glyph.Data = {
              EE000000424DEE0000000000000076000000280000000F0000000F0000000100
              0400000000007800000000000000000000001000000000000000000000000000
              8000008000000080800080000000800080008080000080808000C0C0C0000000
              FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
              8880888888000888888088888000008888808888000000088880888000000000
              8880880008000800088088008800088008808888880008888880888888000888
              8880888888000888888088888800088888808888880008888880888888000888
              888088888800088888808888888888888880}
          end
        end
        object P_Blk_Dataload: TPanel
          Left = 8
          Top = 4
          Width = 212
          Height = 32
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          BevelOuter = bvNone
          Color = clWhite
          TabOrder = 0
          object E_dataload_block: TEdit
            Left = 8
            Top = 6
            Width = 197
            Height = 21
            Hint = 'cesta k souboru bloku'
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
      end
    end
    object TS_HV: TTabSheet
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Hnac'#237' vozidla'
      ImageIndex = 18
      ParentShowHint = False
      ShowHint = True
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object LV_HV: TListView
        Left = 0
        Top = 40
        Width = 1289
        Height = 554
        Hint = 'Tabulka hnacich vozidel'
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alClient
        Columns = <
          item
            Caption = 'Adresa'
          end
          item
            Caption = 'N'#225'zev'
            Width = 130
          end
          item
            Caption = 'Oznacen'#237
            Width = 60
          end
          item
            Caption = 'Majitel'
            Width = 80
          end
          item
            Caption = 'Pozn'#225'mka'
            Width = 125
          end
          item
            Caption = 'T'#345#237'da'
          end
          item
            Alignment = taCenter
            Caption = 'Stanovi'#353'te A'
            Width = 80
          end
          item
            Caption = 'Stanice'
          end
          item
            Caption = 'Rychlost'
          end
          item
            Caption = 'Sm'#283'r'
          end
          item
            Caption = 'F0'
          end
          item
            Caption = 'F1-F4'
          end
          item
            Caption = 'F5-F8'
          end
          item
            Caption = 'F9-F12'
          end
          item
            Caption = 'Status'
          end
          item
            Caption = 'POM'
          end
          item
            Caption = 'Souprava'
          end
          item
            Alignment = taRightJustify
            Caption = 'Najeto vp'#345'ed metr'#367
          end
          item
            Alignment = taRightJustify
            Caption = 'Najeto vp'#345'ed blok'#367
          end
          item
            Alignment = taRightJustify
            Caption = 'Najeto vzad metr'#367
          end
          item
            Alignment = taRightJustify
            Caption = 'Najeto vzad blok'#367
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
        ParentShowHint = False
        PopupMenu = PM_HV
        ShowHint = True
        TabOrder = 0
        ViewStyle = vsReport
        OnChange = LV_HVChange
        OnCustomDrawItem = LV_HVCustomDrawItem
        OnDblClick = LV_HVDblClick
      end
      object P_HV_Pozadi: TPanel
        Left = 0
        Top = 0
        Width = 1289
        Height = 40
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alTop
        BevelOuter = bvNone
        Color = clGray
        TabOrder = 1
        object P_HV_Tlac: TPanel
          Left = 504
          Top = 4
          Width = 217
          Height = 32
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          BevelOuter = bvNone
          Color = clWhite
          TabOrder = 1
          object B_HV_Add: TButton
            Left = 11
            Top = 3
            Width = 97
            Height = 25
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'Pridat HV'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 0
            OnClick = B_HV_AddClick
          end
          object B_HV_Delete: TButton
            Left = 112
            Top = 3
            Width = 97
            Height = 25
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'Smazat HV'
            Enabled = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 1
            OnClick = B_HV_DeleteClick
          end
        end
        object P_HV_Dataload: TPanel
          Left = 8
          Top = 4
          Width = 212
          Height = 32
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          BevelOuter = bvNone
          Color = clWhite
          TabOrder = 0
          object E_dataload_HV: TEdit
            Left = 8
            Top = 6
            Width = 197
            Height = 21
            Hint = 'cesta k souboru bloku'
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
      end
    end
    object TS_Soupravy: TTabSheet
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Soupravy'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object LV_Soupravy: TListView
        Left = 0
        Top = 40
        Width = 1289
        Height = 554
        Hint = 'Tabulka definovan'#253'ch souprav'
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alClient
        Columns = <
          item
            Caption = 'Index'
            Width = 40
          end
          item
            Caption = #268#237'slo'
            Width = 150
          end
          item
            Caption = 'HV1'
            Width = 150
          end
          item
            Caption = 'HV2'
            Width = 80
          end
          item
            Caption = 'Pozn'#225'mka'
            Width = 100
          end
          item
            Caption = #352'ipka'
            Width = 150
          end
          item
            Caption = 'Po'#269'et voz'#367
            Width = 200
          end
          item
            Caption = 'Rychlost'
            Width = 60
          end
          item
            Caption = 'Smer'
            Width = 120
          end
          item
            Caption = 'Stanice'
          end
          item
            Caption = 'Front'
          end
          item
            Caption = 'D'#233'lka'
          end
          item
            Caption = 'Typ'
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
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        ViewStyle = vsReport
        OnChange = LV_SoupravyChange
        OnDblClick = LV_SoupravyDblClick
      end
      object P_Soupravy_pozadi: TPanel
        Left = 0
        Top = 0
        Width = 1289
        Height = 40
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alTop
        BevelOuter = bvNone
        Color = clGray
        TabOrder = 1
        object P_Soupravy_Tlc: TPanel
          Left = 552
          Top = 4
          Width = 113
          Height = 32
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          BevelOuter = bvNone
          Color = clWhite
          TabOrder = 1
          object B_lok_delete: TButton
            Left = 8
            Top = 3
            Width = 97
            Height = 25
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'Smazat soupravu'
            Enabled = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 0
            OnClick = B_lok_deleteClick
          end
        end
        object P_Spr_Dataload: TPanel
          Left = 8
          Top = 4
          Width = 212
          Height = 32
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          BevelOuter = bvNone
          Color = clWhite
          TabOrder = 0
          object E_dataload_soupr: TEdit
            Left = 8
            Top = 6
            Width = 197
            Height = 21
            Hint = 'cesta k souboru souprav'
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
      end
    end
    object TS_Stanice: TTabSheet
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Oblasti '#345#237'zen'#237
      ImageIndex = 16
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object LV_Stanice: TListView
        Left = 0
        Top = 40
        Width = 1289
        Height = 554
        Hint = 'Tabulka definovan'#253'ch stanic'
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alClient
        Columns = <
          item
            Caption = 'Index'
            Width = 40
          end
          item
            Caption = 'N'#225'zev'
            Width = 150
          end
          item
            Caption = 'Zkratka n'#225'zvu'
            Width = 41
          end
          item
            Caption = 'ID'
            Width = 41
          end
          item
            Caption = 'Z'#225'sobn'#237'k'
            Width = 41
          end
          item
            Caption = 'Volba'
            Width = 41
          end
          item
            Caption = 'Osv'#283'tlen'#237
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
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        ViewStyle = vsReport
        OnChange = LV_StaniceChange
      end
      object P_Stanice_Pozadi: TPanel
        Left = 0
        Top = 0
        Width = 1289
        Height = 40
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alTop
        BevelOuter = bvNone
        Color = clGray
        TabOrder = 1
        object P_St_Dataload: TPanel
          Left = 8
          Top = 4
          Width = 313
          Height = 32
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          BevelOuter = bvNone
          Color = clWhite
          TabOrder = 0
          object E_dataload_spnl: TEdit
            Left = 8
            Top = 6
            Width = 197
            Height = 21
            Hint = 'cesta k souboru stanic'
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
          object B_RemoveStack: TButton
            Left = 210
            Top = 6
            Width = 95
            Height = 19
            Caption = 'Smazat z'#225'sobn'#237'k'
            TabOrder = 1
            OnClick = B_RemoveStackClick
          end
        end
      end
    end
    object TS_Zesilovace: TTabSheet
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Zesilova'#269'e'
      ImageIndex = 20
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object LV_Zesilovace: TListView
        Left = 0
        Top = 40
        Width = 1289
        Height = 554
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alClient
        Columns = <
          item
            Caption = 'Index'
            Width = 40
          end
          item
            Caption = 'N'#225'zev'
            Width = 150
          end
          item
            Caption = 'Typ'
            Width = 300
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
        OnChange = LV_ZesilovaceChange
        OnCustomDrawItem = LV_ZesilovaceCustomDrawItem
        OnDblClick = LV_ZesilovaceDblClick
      end
      object P_zes_pozadi: TPanel
        Left = 0
        Top = 0
        Width = 1289
        Height = 40
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alTop
        BevelOuter = bvNone
        Color = clGray
        TabOrder = 1
        object P_Zes_Tlc: TPanel
          Left = 480
          Top = 4
          Width = 217
          Height = 32
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          BevelOuter = bvNone
          Color = clWhite
          TabOrder = 1
          object B_zes_add: TButton
            Left = 8
            Top = 3
            Width = 97
            Height = 25
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'Pridat zesilovac'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 0
            OnClick = B_zes_addClick
          end
          object B_zes_delete: TButton
            Left = 112
            Top = 3
            Width = 97
            Height = 25
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'Smazat zesilovac'
            Enabled = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 1
            OnClick = B_zes_deleteClick
          end
        end
        object P_Zes_Vysvetlivky: TPanel
          Left = 960
          Top = 4
          Width = 217
          Height = 32
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          BevelOuter = bvNone
          Color = clWhite
          TabOrder = 2
          object L_Zes_Napajeni: TLabel
            Left = 8
            Top = 16
            Width = 84
            Height = 13
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'Nen'#237' nap'#225'jeno'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object L_Zes_OK: TLabel
            Left = 104
            Top = 3
            Width = 103
            Height = 13
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'Zesilovac funkcn'#237
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clLime
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object L_Zes_NapajeniL_Zes_Zkrat: TLabel
            Left = 104
            Top = 16
            Width = 109
            Height = 13
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'Zkrat na zesilovaci'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clRed
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object L_Zes_Nedetekovano: TLabel
            Left = 8
            Top = 3
            Width = 57
            Height = 13
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'MTB Stop'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clGray
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
        end
        object P_Zes_Dataload: TPanel
          Left = 8
          Top = 4
          Width = 212
          Height = 32
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          BevelOuter = bvNone
          Color = clWhite
          TabOrder = 0
          object E_dataload_zes: TEdit
            Left = 8
            Top = 6
            Width = 197
            Height = 21
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
      end
    end
    object TS_Aut_Rezimy: TTabSheet
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'AC'
      ParentShowHint = False
      ShowHint = False
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object LV_AC_Db: TListView
        Left = 0
        Top = 40
        Width = 1289
        Height = 145
        Hint = 'Tabulka definovan'#253'ch automatick'#253'ch re'#382'imu'
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alTop
        Columns = <
          item
            Caption = 'Index'
            Width = 40
          end
          item
            Caption = 'N'#225'zev'
            Width = 150
          end
          item
            Alignment = taCenter
            Caption = 'Status'
            Width = 90
          end
          item
            Caption = 'Soubor'
            Width = 500
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
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        ViewStyle = vsReport
        OnChange = LV_AC_DbChange
        OnCustomDrawItem = LV_AC_DbCustomDrawItem
        OnDblClick = LV_AC_DbDblClick
      end
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 1289
        Height = 40
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alTop
        BevelOuter = bvNone
        Color = clGray
        TabOrder = 1
        object P_AC_Dataload: TPanel
          Left = 5
          Top = 4
          Width = 580
          Height = 32
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          BevelOuter = bvNone
          Color = clWhite
          TabOrder = 0
          object SB_AC_Play: TSpeedButton
            Left = 427
            Top = 4
            Width = 29
            Height = 23
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Glyph.Data = {
              36040000424D3604000000000000360000002800000010000000100000000100
              2000000000000004000000000000000000000000000000000000FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF0000FF0000FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF0000FF000000FF0000FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF0000FF000000FF000000FF0000FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF0000FF000000FF000000FF000000FF0000FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF0000FF000000FF000000FF000000FF000000FF
              0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF0000FF000000FF000000FF000000FF000000FF
              000000FF0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF0000FF000000FF000000FF000000FF000000FF
              000000FF000000FF0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF0000FF000000FF000000FF000000FF000000FF
              000000FF0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF0000FF000000FF000000FF000000FF000000FF
              0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF0000FF000000FF000000FF000000FF0000FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF0000FF000000FF000000FF0000FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF0000FF000000FF0000FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF0000FF0000FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
            OnClick = SB_AC_PlayClick
          end
          object SB_AC_Stop: TSpeedButton
            Left = 460
            Top = 4
            Width = 31
            Height = 23
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Glyph.Data = {
              36040000424D3604000000000000360000002800000010000000100000000100
              2000000000000004000000000000000000000000000000000000FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
              FF000000FF000000FF000000FF000000FF00FF00FF00FF00FF00FF00FF00FF00
              FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
              FF000000FF000000FF000000FF000000FF00FF00FF00FF00FF00FF00FF00FF00
              FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
              FF000000FF000000FF000000FF000000FF00FF00FF00FF00FF00FF00FF00FF00
              FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
              FF000000FF000000FF000000FF000000FF00FF00FF00FF00FF00FF00FF00FF00
              FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
              FF000000FF000000FF000000FF000000FF00FF00FF00FF00FF00FF00FF00FF00
              FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
              FF000000FF000000FF000000FF000000FF00FF00FF00FF00FF00FF00FF00FF00
              FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
              FF000000FF000000FF000000FF000000FF00FF00FF00FF00FF00FF00FF00FF00
              FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
              FF000000FF000000FF000000FF000000FF00FF00FF00FF00FF00FF00FF00FF00
              FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
              FF000000FF000000FF000000FF000000FF00FF00FF00FF00FF00FF00FF00FF00
              FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
              FF000000FF000000FF000000FF000000FF00FF00FF00FF00FF00FF00FF00FF00
              FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
              FF000000FF000000FF000000FF000000FF00FF00FF00FF00FF00FF00FF00FF00
              FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
              FF000000FF000000FF000000FF000000FF00FF00FF00FF00FF00FF00FF00FF00
              FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
              FF000000FF000000FF000000FF000000FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
            OnClick = SB_AC_StopClick
          end
          object SB_AC_Pause: TSpeedButton
            Left = 496
            Top = 4
            Width = 32
            Height = 23
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Glyph.Data = {
              36040000424D3604000000000000360000002800000010000000100000000100
              2000000000000004000000000000000000000000000000000000FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF000000FF000000FF000000FF000000FF00FF00FF00FF00FF00
              FF00FF000000FF000000FF000000FF000000FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF000000FF000000FF000000FF000000FF00FF00FF00FF00FF00
              FF00FF000000FF000000FF000000FF000000FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF000000FF000000FF000000FF000000FF00FF00FF00FF00FF00
              FF00FF000000FF000000FF000000FF000000FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF000000FF000000FF000000FF000000FF00FF00FF00FF00FF00
              FF00FF000000FF000000FF000000FF000000FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF000000FF000000FF000000FF000000FF00FF00FF00FF00FF00
              FF00FF000000FF000000FF000000FF000000FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF000000FF000000FF000000FF000000FF00FF00FF00FF00FF00
              FF00FF000000FF000000FF000000FF000000FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF000000FF000000FF000000FF000000FF00FF00FF00FF00FF00
              FF00FF000000FF000000FF000000FF000000FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF000000FF000000FF000000FF000000FF00FF00FF00FF00FF00
              FF00FF000000FF000000FF000000FF000000FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF000000FF000000FF000000FF000000FF00FF00FF00FF00FF00
              FF00FF000000FF000000FF000000FF000000FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF000000FF000000FF000000FF000000FF00FF00FF00FF00FF00
              FF00FF000000FF000000FF000000FF000000FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF000000FF000000FF000000FF000000FF00FF00FF00FF00FF00
              FF00FF000000FF000000FF000000FF000000FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF000000FF000000FF000000FF000000FF00FF00FF00FF00FF00
              FF00FF000000FF000000FF000000FF000000FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF000000FF000000FF000000FF000000FF00FF00FF00FF00FF00
              FF00FF000000FF000000FF000000FF000000FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
            OnClick = SB_AC_PauseClick
          end
          object SB_AC_Repeat: TSpeedButton
            Left = 532
            Top = 4
            Width = 30
            Height = 23
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            GroupIndex = 6328
            Glyph.Data = {
              36040000424D3604000000000000360000002800000010000000100000000100
              2000000000000004000000000000000000000000000000000000FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF0000000000000000000000000000000000FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF000000000000000000000000000000000000000000000000000000
              0000FF00FF000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF0000000000000000000000000000000000000000000000000000000000FF00
              FF00FF00FF00000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
              FF000000000000000000FF00FF00FF00FF000000000000000000FF00FF00FF00
              FF00FF00FF00FF00FF000000000000000000FF00FF00FF00FF00FF00FF000000
              00000000000000000000FF00FF00FF00FF0000000000FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00000000000000000000000000FF00FF00FF00FF000000
              000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF000000000000000000FF00FF00FF00FF000000
              000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF000000000000000000FF00FF00FF00FF000000
              00000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00000000000000000000000000FF00FF00FF00FF00FF00
              FF000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF000000000000000000FF00FF00FF00FF00FF00FF00FF00
              FF0000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
              FF0000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF0000000000000000000000000000000000000000000000
              00000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000000000000000
              0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
            OnClick = SB_AC_RepeatClick
          end
          object E_dataload_AutRez: TEdit
            Left = 8
            Top = 6
            Width = 197
            Height = 21
            Hint = 'cesta k souboru automatick'#253'ch re'#382'imu'
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
          object B_AutRezim_add: TButton
            Left = 210
            Top = 3
            Width = 96
            Height = 25
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'P'#345'idat AC'
            Enabled = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ImageIndex = 44
            ParentFont = False
            TabOrder = 1
            OnClick = B_AutRezim_addClick
          end
          object B_AutRezim_delete: TButton
            Left = 313
            Top = 3
            Width = 97
            Height = 25
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'Smazat AC'
            Enabled = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 2
            OnClick = B_AutRezim_deleteClick
          end
        end
      end
      object LV_AC_Kroky: TListView
        Left = 0
        Top = 185
        Width = 1289
        Height = 409
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alClient
        Columns = <
          item
            Caption = 'Krok'
            Width = 40
          end
          item
            Caption = 'Popis'
            Width = 1000
          end>
        Enabled = False
        GridLines = True
        ReadOnly = True
        RowSelect = True
        TabOrder = 2
        ViewStyle = vsReport
        OnCustomDrawItem = LV_AC_KrokyCustomDrawItem
      end
    end
    object TS_Users: TTabSheet
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'U'#382'ivatel'#233
      ImageIndex = 23
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object LV_Users: TListView
        Left = 0
        Top = 40
        Width = 1289
        Height = 554
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alClient
        Columns = <
          item
            Caption = 'Index'
            Width = 40
          end
          item
            Caption = 'Id'
            Width = 150
          end
          item
            Caption = 'Jm'#233'no + p'#345#237'jmen'#237
            Width = 100
          end
          item
            Alignment = taCenter
            Caption = 'root'
            Width = 60
          end
          item
            Caption = 'Posledn'#237' prihl'#225#353'en'#237
            Width = 125
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
        OnChange = LV_UsersChange
        OnDblClick = LV_UsersDblClick
      end
      object P_Users_pozadi: TPanel
        Left = 0
        Top = 0
        Width = 1289
        Height = 40
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alTop
        BevelOuter = bvNone
        Color = clGray
        TabOrder = 1
        object P_Users_Tlc: TPanel
          Left = 480
          Top = 4
          Width = 217
          Height = 32
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          BevelOuter = bvNone
          Color = clWhite
          TabOrder = 1
          object B_User_Add: TButton
            Left = 8
            Top = 3
            Width = 97
            Height = 25
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'Pridat uzivatele'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 0
            OnClick = B_User_AddClick
          end
          object B_User_Delete: TButton
            Left = 112
            Top = 3
            Width = 97
            Height = 25
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'Smazat uzivatele'
            Enabled = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 1
            OnClick = B_User_DeleteClick
          end
        end
        object P_Users_Dataload: TPanel
          Left = 8
          Top = 4
          Width = 212
          Height = 32
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          BevelOuter = bvNone
          Color = clWhite
          TabOrder = 0
          object E_Dataload_Users: TEdit
            Left = 8
            Top = 6
            Width = 197
            Height = 21
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
      end
    end
    object TS_Stav_MTB: TTabSheet
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Stav MTB'
      ImageIndex = -1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object LV_Stav_MTB: TListView
        Left = 0
        Top = 0
        Width = 1289
        Height = 594
        Hint = 'Stavy vstupu a v'#253'stupu MTB desek'
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alClient
        Columns = <
          item
            Caption = 'Adresa MTB'
            Width = 71
          end
          item
            Caption = 'X'
            Width = 30
          end
          item
            Caption = 'N'#225'zev MTB'
            Width = 125
          end
          item
            Alignment = taCenter
            Caption = 'Typ MTB'
            Width = 70
          end
          item
            Alignment = taCenter
            Caption = 'Vstupy'
            Width = 200
          end
          item
            Alignment = taCenter
            Caption = 'V'#253'stupy'
            Width = 200
          end
          item
            Alignment = taCenter
            Caption = 'Existence'
            Width = 60
          end
          item
            Alignment = taCenter
            Caption = 'Firmware'
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
        ParentShowHint = False
        ShowHint = True
        SmallImages = IL_MTB
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
    object TS_VC: TTabSheet
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'J'#237'zdn'#237' cesty'
      ImageIndex = 24
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object P_VC_Pozadi: TPanel
        Left = 0
        Top = 0
        Width = 1289
        Height = 40
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alTop
        BevelOuter = bvNone
        Color = clGray
        TabOrder = 1
        object P_VC_Dataload: TPanel
          Left = 8
          Top = 4
          Width = 529
          Height = 32
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          BevelOuter = bvNone
          Color = clWhite
          TabOrder = 0
          object E_Dataload_JC: TEdit
            Left = 8
            Top = 6
            Width = 197
            Height = 21
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
          object B_VC_Add: TButton
            Left = 213
            Top = 3
            Width = 97
            Height = 25
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'Pridat cestu'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 1
            OnClick = B_VC_AddClick
          end
          object B_VC_delete: TButton
            Left = 317
            Top = 3
            Width = 97
            Height = 25
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'Smazat cestu'
            Enabled = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 2
            OnClick = B_VC_deleteClick
          end
          object B_JC_Reset: TButton
            Left = 420
            Top = 3
            Width = 97
            Height = 25
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'Zru'#353'it stav'#283'n'#237
            Enabled = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 3
            OnClick = B_JC_ResetClick
          end
        end
        object P_Cesty_Posun: TPanel
          Left = 1112
          Top = 4
          Width = 65
          Height = 32
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          BevelOuter = bvNone
          Color = clWhite
          TabOrder = 1
          object SB_Cesty_Up: TSpeedButton
            Left = 8
            Top = 5
            Width = 23
            Height = 22
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Enabled = False
            Glyph.Data = {
              EE000000424DEE0000000000000076000000280000000F0000000F0000000100
              0400000000007800000000000000000000001000000000000000000000000000
              8000008000000080800080000000800080008080000080808000C0C0C0000000
              FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
              8880888888000888888088888800088888808888880008888880888888000888
              8880888888000888888088888800088888808808880008880880880088000880
              0880880008000800088088800000000088808888000000088880888880000088
              888088888800088888808888888888888880}
            OnClick = SB_Cesty_UpClick
          end
          object SB_Cesty_Down: TSpeedButton
            Left = 35
            Top = 5
            Width = 23
            Height = 22
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Enabled = False
            Glyph.Data = {
              EE000000424DEE0000000000000076000000280000000F0000000F0000000100
              0400000000007800000000000000000000001000000000000000000000000000
              8000008000000080800080000000800080008080000080808000C0C0C0000000
              FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
              8880888888000888888088888000008888808888000000088880888000000000
              8880880008000800088088008800088008808888880008888880888888000888
              8880888888000888888088888800088888808888880008888880888888000888
              888088888800088888808888888888888880}
            OnClick = SB_Cesty_DownClick
          end
        end
      end
      object LV_JC: TListView
        Left = 0
        Top = 40
        Width = 1289
        Height = 554
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alClient
        Columns = <
          item
            Caption = 'Index'
            Width = 40
          end
          item
            Caption = 'N'#225'zev cesty'
            Width = 200
          end
          item
            Caption = 'Rozpad'
          end
          item
            Caption = 'Rozpad ruseni'
          end
          item
            Caption = 'Krok staveni'
          end
          item
            Caption = 'V'#253'choz'#237' n'#225'vestidlo'
            Width = 200
          end
          item
            Caption = 'Vyh'#253'bky v z'#225'veru'
            Width = 300
          end
          item
            Caption = 'Bloky v z'#225'veru'
            Width = 300
          end
          item
            Caption = 'Typ cesty'
            Width = 100
          end
          item
            Caption = 'Dal'#353#237' n'#225'vestidlo'
            Width = 200
          end
          item
            Caption = 'Rychlost DN'
            Width = 55
          end
          item
            Caption = 'Rychlost NoDN'
            Width = 55
          end
          item
            Caption = 'Odvraty'
          end
          item
            Caption = 'P'#345#237'slu'#353'enstv'#237
          end
          item
            Caption = 'Tra'#357
            Width = 100
          end
          item
            Caption = 'P'#345'ejezdy'
          end
          item
            Caption = 'Podm'#237'nky v'#253'hybky'
          end
          item
            Caption = 'Podm'#237'nky z'#225'mky'
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
        OnChange = LV_JCChange
        OnCustomDrawItem = LV_JCCustomDrawItem
        OnDblClick = LV_JCDblClick
      end
    end
    object TS_MultiJC: TTabSheet
      Caption = 'Slo'#382'en'#233' j'#237'zdn'#237' cesty'
      ImageIndex = 12
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Panel6: TPanel
        Left = 0
        Top = 0
        Width = 1289
        Height = 40
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alTop
        BevelOuter = bvNone
        Color = clGray
        TabOrder = 0
        object Panel7: TPanel
          Left = 8
          Top = 4
          Width = 425
          Height = 32
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          BevelOuter = bvNone
          Color = clWhite
          TabOrder = 0
          object E_Dataload_multiJC: TEdit
            Left = 8
            Top = 6
            Width = 197
            Height = 21
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
          object B_mJC_Add: TButton
            Left = 213
            Top = 3
            Width = 97
            Height = 25
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'Pridat cestu'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 1
            OnClick = B_mJC_AddClick
          end
          object B_mJC_Remove: TButton
            Left = 317
            Top = 3
            Width = 97
            Height = 25
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'Smazat cestu'
            Enabled = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 2
            OnClick = B_mJC_RemoveClick
          end
        end
      end
      object LV_MultiJC: TListView
        Left = 0
        Top = 40
        Width = 1289
        Height = 554
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alClient
        Columns = <
          item
            Caption = 'Index'
            Width = 40
          end
          item
            Caption = 'N'#225'zev cesty'
            Width = 200
          end
          item
            Caption = 'Krok'
          end
          item
            Caption = 'J'#237'zdn'#237' cesty'
          end
          item
            Caption = 'Variantn'#237' body'
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
        TabOrder = 1
        ViewStyle = vsReport
        OnChange = LV_MultiJCChange
        OnCustomDrawItem = LV_MultiJCCustomDrawItem
        OnDblClick = LV_MultiJCDblClick
      end
    end
    object TS_FuncsVyznam: TTabSheet
      Caption = 'V'#253'znamy funkc'#237
      ImageIndex = 13
      object M_funcsVyznam: TMemo
        Left = 0
        Top = 40
        Width = 1289
        Height = 554
        Align = alClient
        ScrollBars = ssVertical
        TabOrder = 0
      end
      object P_funcsVyznamBg: TPanel
        Left = 0
        Top = 0
        Width = 1289
        Height = 40
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object B_Change: TButton
          Left = 9
          Top = 6
          Width = 75
          Height = 25
          Caption = 'Ulo'#382'it'
          TabOrder = 0
          OnClick = B_ChangeClick
        end
        object CHB_LoadChanges: TCheckBox
          Left = 96
          Top = 10
          Width = 97
          Height = 17
          Caption = 'Na'#269#237'tat zm'#283'ny'
          Checked = True
          State = cbChecked
          TabOrder = 1
        end
      end
    end
    object TS_log: TTabSheet
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Prim'#225'rn'#237' LOG'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object LV_log: TListView
        Left = 0
        Top = 24
        Width = 1289
        Height = 570
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alClient
        Color = clWhite
        Columns = <
          item
            Caption = 'Cas'
          end
          item
            Caption = 'Zpr'#225'va'
            Width = 850
          end
          item
            Caption = 'V'#253'znam'
            Width = 200
          end
          item
            Caption = 'Error ID'
            Width = 48
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
        OnCustomDrawItem = LV_logCustomDrawItem
      end
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 1289
        Height = 24
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alTop
        BevelOuter = bvNone
        ParentBackground = False
        TabOrder = 1
        object CHB_Mainlog_File: TCheckBox
          Left = 8
          Top = 2
          Width = 126
          Height = 17
          Caption = 'Logovat do souboru'
          TabOrder = 0
        end
        object CHB_mainlog_table: TCheckBox
          Left = 140
          Top = 2
          Width = 122
          Height = 17
          Caption = 'Logovat do tabulky'
          TabOrder = 1
        end
      end
    end
    object TS_Intellibox: TTabSheet
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'LOG komunikace s centr'#225'lou'
      ImageIndex = 14
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object LV_log_lnet: TListView
        Left = 0
        Top = 24
        Width = 1289
        Height = 570
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alClient
        Columns = <
          item
            Caption = 'Cas'
          end
          item
            Caption = 'Level'
            Width = 41
          end
          item
            Caption = 'Zpr'#225'va'
            Width = 813
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
        OnCustomDrawItem = LV_log_lnetCustomDrawItem
        OnDblClick = LV_log_lnetDblClick
      end
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 1289
        Height = 24
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alTop
        BevelOuter = bvNone
        ParentBackground = False
        TabOrder = 1
        object Label2: TLabel
          Left = 299
          Top = 3
          Width = 43
          Height = 13
          Caption = 'Loglevel:'
        end
        object CHB_centrala_table: TCheckBox
          Left = 140
          Top = 2
          Width = 122
          Height = 17
          Caption = 'Logovat do tabulky'
          TabOrder = 0
          OnClick = CHB_centrala_tableClick
        end
        object CHB_centrala_file: TCheckBox
          Left = 8
          Top = 2
          Width = 126
          Height = 17
          Caption = 'Logovat do souboru'
          TabOrder = 1
          OnClick = CHB_centrala_fileClick
        end
        object CB_centrala_loglevel: TComboBox
          Left = 351
          Top = 1
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemHeight = 0
          TabOrder = 2
          OnChange = CB_centrala_loglevelChange
          Items.Strings = (
            '0 - '#382#225'dn'#233' zpr'#225'vy'
            '1 - chyby'
            '2 - p'#345#237'kazy'
            '3 - data'
            '4 - zm'#283'na stav'#367
            '5 - podrobn'#233' informace')
        end
      end
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 250
    OnTimer = Timer1Timer
    Left = 648
    Top = 456
  end
  object Menu_1: TMainMenu
    Images = IL_Menu
    Left = 728
    Top = 456
    object M_System: TMenuItem
      Caption = 'Syst'#233'm'
      object PM_Central_Start: TMenuItem
        Action = A_System_Start
        Caption = 'Central start'
      end
      object PM_Central_Stop: TMenuItem
        Action = A_System_Stop
        Caption = 'Central stop'
      end
      object PM_system_reset: TMenuItem
        Caption = 'Restartovat syst'#233'my'
        Enabled = False
        ImageIndex = 1
        OnClick = PM_system_resetClick
      end
    end
    object MI_PanelServer: TMenuItem
      Caption = 'Panel server'
      object Start1: TMenuItem
        Action = A_PanelServer_Start
      end
      object Stop1: TMenuItem
        Action = A_PanelServer_Stop
      end
    end
    object M_Zarizeni: TMenuItem
      Caption = 'MTB'
      object OtevtMTB1: TMenuItem
        Action = A_MTB_Open
      end
      object ZavtMTB1: TMenuItem
        Action = A_MTB_Close
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object PM_go: TMenuItem
        Action = A_MTB_Go
        Caption = 'Zapnout komunikaci'
      end
      object PM_stop: TMenuItem
        Action = A_MTB_Stop
        Caption = 'Vypnout komunikaci'
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object PM_OptionsMTB: TMenuItem
        Action = A_lib_cfg
        Bitmap.Data = {
          06030000424D060300000000000036000000280000000F0000000F0000000100
          180000000000D002000000000000000000000000000000000000EC00EC010101
          0E0E0E0D0D0D1616161819191213130400000400001213131819191616160D0D
          0D0E0E0E000000000000E000E0121212FDFDFDFAFBFBF0EEEEECE5E5D3D4D42A
          34342A3434D3D4D4ECE5E5F0EEEEFAFBFBFDFDFD0E0E0E000000E000E0111111
          FFFFFFEBE7E72E3939224545612828007D7D007D7D6128282245452E3939EBE7
          E7FFFFFF0E0E0E000000DF00DF161616F6F5F5705F5F24535348FFFF47424200
          ADAD00ADAD47424248FFFF245353705F5FF6F5F5121212000000DF00DF1C1C1C
          E7E9E9161B1B8455554178780FFFFF0BC6C60BC6C60FFFFF417878845555161B
          1BE7E9E9181818000000E000E0181919F9F3F3475454005B5B009B9B2FBFBF67
          36366736362FBFBF009B9B005B5B475454F9F3F3141515000000E000E01B1D1D
          EDE3E30A1E1E00ADAD00CCCC1A98987F51517F51511A989800CCCC00ADAD0A1E
          1EEDE3E3171919000000E100E1131313FFFFFFACB0B04C17172C5D5D23FFFF1A
          89891A898923FFFF2C5D5D4C1717ACB0B0FFFFFF101010000000E200E2111111
          FEFEFEDED3D34763635CFFFF2C828200D5D500D5D52C82825CFFFF476363DED3
          D3FEFEFE0E0E0E000000E100E1111111FEFEFEDAD0D00924241671715F0B0B02
          83830283835E0A0A167171041E1ED3C9C9F7F7F70E0E0E000000E100E1121212
          FEFEFEFAFDFDCCC6C6BAA8A8B2B1B10A35350A3535B2B2B2BAA8A8DED8D8FFFF
          FFFFFFFF161016000000E100E1131313FEFEFEFAFAFAFFFFFFFFFFFFFFFFFFCD
          C3C3CDC3C3FAFCFCFFFFFF7E7E7E282828413C41000300010000E100E1121212
          FBFBFBF7F7F7F7F7F7F7F7F7FAFAFAFFFFFFFFFFFFF3F2F2F8F8F8161716A8A3
          A8001900B300B3000000E000E0131313FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFF433E431F271FC90DC9FF00FF000000EC00EC010101
          0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F150F15050505B600
          B6FF00FFF900F9000000}
        Caption = 'Nastaven'#237
      end
      object PM_MTBDriver_About: TMenuItem
        Caption = 'O driveru'
        ImageIndex = 43
        OnClick = PM_MTBDriver_AboutClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object MI_Libs: TMenuItem
        Caption = 'Knihovny'
        object PM_lib_Simulator: TMenuItem
          Caption = 'Nacist knihovnu Simulator'
          ImageIndex = 13
          OnClick = PM_lib_SimClick
        end
        object PM_lib_MTB: TMenuItem
          Caption = 'Nacist knihovnu MTB'
          ImageIndex = 12
          OnClick = PM_lib_MTBClick
        end
      end
    end
    object M_Centrala: TMenuItem
      Caption = 'Centr'#225'la'
      object PM_Int_connect: TMenuItem
        Action = A_Trk_Connect
      end
      object PM_Int_Disconnect: TMenuItem
        Action = A_Trk_Disconnect
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object PM_Int_run: TMenuItem
        Action = A_DCC_Go
        Caption = 'DCC start'
      end
      object PM_Int_Stop: TMenuItem
        Action = A_DCC_Stop
        Caption = 'DCC stop'
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object PM_AllLokPrevzit: TMenuItem
        Action = A_All_Loko_Prevzit
      end
      object PM_AllLokOdpojit: TMenuItem
        Action = A_All_Loko_Odhlasit
      end
      object N10: TMenuItem
        Caption = '-'
      end
      object PM_FuncsSet: TMenuItem
        Action = A_FuncsSet
      end
    end
    object M_Provoz: TMenuItem
      Caption = 'Provoz'
      object PM_Nastaveni: TMenuItem
        Caption = 'Nastaven'#237
        ImageIndex = 32
        ShortCut = 16463
        OnClick = PM_NastaveniClick
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object PM_Console: TMenuItem
        Caption = 'Konzole'
        OnClick = PM_ConsoleClick
      end
    end
    object M_Reset: TMenuItem
      Caption = 'Reset'
      object PM_ResetV: TMenuItem
        Caption = 'Prestavit vyh'#253'bky do z'#225'kladn'#237' polohy'
        Enabled = False
        ImageIndex = 27
        OnClick = PM_ResetVClick
      end
    end
    object M_Dalsi: TMenuItem
      Caption = 'Dal'#353#237
      object PM_Tester: TMenuItem
        Caption = 'MTB tester'
        Enabled = False
        ImageIndex = 22
        ShortCut = 16468
        OnClick = PM_TesterClick
      end
    end
    object MI_File: TMenuItem
      Caption = 'Soubor'
      object MI_Save_config: TMenuItem
        Caption = 'Ulo'#382'it konfigura'#269'n'#237' data'
        ImageIndex = 21
        OnClick = MI_Save_configClick
      end
      object N9: TMenuItem
        Caption = '-'
      end
      object PM_SaveLayout: TMenuItem
        Action = A_SaveStav
      end
    end
    object M_Zobrazeni: TMenuItem
      Caption = 'Zobrazen'#237
      object PM_SB1: TMenuItem
        AutoCheck = True
        Caption = 'Stavov'#225' li'#353'ta'
        Checked = True
        OnClick = PM_SB1Click
      end
      object PM_SaveFormPos: TMenuItem
        Caption = 'Ulo'#382'it pozice okna'
        OnClick = PM_SaveFormPosClick
      end
    end
    object M_Help: TMenuItem
      Caption = 'N'#225'poveda'
      object PM_Help_RP: TMenuItem
        Caption = 'O programu'
        ImageIndex = 15
        ShortCut = 16496
        OnClick = PM_Help_RPClick
      end
    end
  end
  object AE_1: TApplicationEvents
    OnMessage = AE_1Message
    Left = 872
    Top = 456
  end
  object PM_TI: TPopupMenu
    Left = 824
    Top = 456
    object PM_Open: TMenuItem
      Caption = 'Otevr'#237't'
      Hint = 'Otevr'#237't r'#237'd'#237'c'#237' program'
    end
    object PM_icon_close: TMenuItem
      Caption = 'Skr'#253't ikonu'
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object PM_close: TMenuItem
      Caption = 'Ukoncit program'
      Hint = 'Ukoncit r'#237'd'#237'c'#237' program'
    end
  end
  object T_function: TTimer
    Enabled = False
    Interval = 500
    OnTimer = T_functionTimer
    Left = 600
    Top = 456
  end
  object T_konflikty: TTimer
    Enabled = False
    Interval = 3000
    OnTimer = T_konfliktyTimer
    Left = 544
    Top = 456
  end
  object IL_Zpravy: TImageList
    Height = 14
    Width = 20
    Left = 1032
    Top = 456
    Bitmap = {
      494C010101004801880214000E00FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000500000000E00000001002000000000008011
      00000000000000000000000000000000000000000000FFFFFF00F7FFFF00F7FF
      FF00F7FFFF00F7FFFF00F7FFFF00F7FFFF00F7FFFF00F7FFFF00F7FFFF00F7FF
      FF00F7FFFF00F7FFFF00F7FFFF00F7FFFF00F7FFFF00F7FFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF008484840084848400C6C6
      C600A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
      A500A5A5A500A5A5A500A5A5A500A5A5A500C6C6C6008484840000000000F7FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF008484840084848400A5A5
      A500FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00A5A5A5008484840084848400F7FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF0084848400C6DEC600C6C6
      C60084848400C6DEC600FFFFFF00F7FFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF008484840084848400FFFFFF0084848400F7FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF0084848400C6C6C600FFFF
      FF00C6C6C60084848400C6C6C600FFFFFF00FFFFFF00F7FFFF00C6DEC600FFFF
      FF00FFFFFF00F7FFFF0084848400A5A5A500FFFFFF00F7FFFF0084848400F7FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF0084848400C6C6C600FFFF
      FF00FFFFFF00C6DEC60084848400C6C6C600C6C6C6008484840084848400A5A5
      A500F7FFFF0084848400A5A5A500FFFFFF00FFFFFF00F7FFFF0084848400F7FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF0084848400C6C6C600FFFF
      FF00F7FFFF00FFFFFF00F7FFFF000000000084848400F7FFFF00FFFFFF008484
      840000000000C6C6C600FFFFFF00F7FFFF00FFFFFF00F7FFFF0084848400F7FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF0084848400C6C6C6000000
      0000FFFFFF00FFFFFF00A5A5A50084848400F7FFFF00FFFFFF00FFFFFF00FFFF
      FF00A5A5A50084848400FFFFFF00FFFFFF00FFFFFF00F7FFFF0084848400F7FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF0084848400C6C6C600FFFF
      FF00FFFFFF0084848400848484000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00A5A5A50084848400F7FFFF00FFFFFF00F7FFFF0084848400F7FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF0084848400C6C6C600FFFF
      FF008484840084848400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00C6C6C60084848400F7FFFF00FFFFFF0084848400F7FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF0084848400C6C6C6008484
      8400A5A5A500FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00C6C6C60084848400C6DEC60084848400F7FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF008484840000000000A5A5
      A500FFFFFF00F7FFFF00F7FFFF00F7FFFF00F7FFFF00F7FFFF00F7FFFF00F7FF
      FF00F7FFFF00F7FFFF00F7FFFF00FFFFFF00C6C6C6008484840000000000F7FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A5A5A50084848400C6C6
      C600A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
      A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A50084848400FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      28000000500000000E0000000100010000000000A80000000000000000000000
      000000000000000000000000FFFFFF0080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000010000000000000000000000001800000000000000000000000000000
      0000000000000000020000000000000000000000000000000000000000000000
      8000000000000000000000008000100000000000000000000000000000000000
      0000000000000000000000000000}
  end
  object IL_Menu: TImageList
    Left = 1080
    Top = 456
    Bitmap = {
      494C010135005C019C0210001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      000000000000360000002800000040000000E0000000010020000000000000E0
      0000000000000000000000000000000000000000000010108C0010108C001010
      8C00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000001010940010189C001021AD001010
      9400101094001010940010109400101094001010940008089400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000101094006B7BD6002139C6001021
      B500101094001018A5001031C6001031C6001021AD000810940018188C000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000008109C002121A5007384DE002142
      CE001031BD001839CE001839CE001839CE00314ACE00424ABD0010109C000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000008109C001821A5001842
      CE001842CE001842CE001842CE003152D6007384D6001818A50008089C000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000810A5001829BD002152
      D6002152D6002152D6003963DE007384DE001818AD000810A500000000000000
      AD0010108C000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000810AD00295ADE00295A
      DE00295ADE00426BE700738CDE001821AD000810A5000000000008088C001010
      94001018A5001010940010108C00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000810AD00527BEF00316B
      E7004A73EF007B8CE7001021B5000810AD000000000008109400101094001021
      B5001029C6001021B50010109400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000AD000818B5007384E7006B94
      F7007B94E7001021B5000810B500000000000810940010109C001029B5001031
      C6001031C6001839C60010109400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000810B5000818BD005A6B
      DE001021BD000810B5000000000008109C0010109C001831BD001842CE001842
      CE001842CE005A73DE0008109C00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000810BD000810
      BD000810BD00000000000810A5001010A5001839C600214AD600214AD600214A
      D6002952D6004252BD001010A500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000AD001010A5002142CE002952DE002952DE002952DE003963
      DE002152D6001018AD000810A500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000810AD004252C600527BE7003163E7003163E7006B8CEF007B94
      E7004A73E7002952DE001018AD000810AD000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000810AD00637BDE009CB5F7009CB5F7004A5ACE001018
      B5007384DE005284EF006384E7001018B5000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000818B5000810B5000810B5000810B5000810B5000810
      B5001018B5007384E700394ACE000810B5000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000810B5000810B5000818B500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000008523900085239000852
      3900000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840000848400008484000084
      8400008484000084840000848400008484000084840000848400008484000084
      84000084840084848400C6DEC600000000000000000000000000C6C6C6008484
      8400848484008484000084840000848400008484000084000000840000008400
      0000840000008400000084000000840000000000000000000000C6C6C6008484
      8400848484008484000084840000848400008484000084000000840000008400
      000084000000840000008400000084000000085A3900186B4A00187B5200085A
      3900085A39000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000848400A5A5A500A5A5A500A5A5
      A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
      A500A5A5A50084848400A5A5A5000000000000000000C6C6C60084848400A5A5
      A500FFFFFF00F7CEA500F7CEA500F7CEA500FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00F7CEA500F7CEA500F7CEA5008400000000000000C6C6C60084848400A5A5
      A500FFFFFF00F7CEA500F7CEA500F7CEA500FFFFFF00FFFFFF00FFFFFF00FFFF
      FF000000000000000000F7CEA500840000001063420084BDA50039947300217B
      5A001063420010634200105A42001063420010634200105A4200105A42000000
      0000000000000000000000000000000000008484840084848400C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6DEC600C6DEC600C6DEC600C6DEC600F7FF
      FF00C6DEC600A5A5A50084848400000000000000000084848400A5A5A500A5A5
      A500F7FFFF00A5A5A500A5A5A500A5A5A500FFFFFF00FFFFFF00FFFFFF00FFFF
      FF008484840084848400F7CEA500840000000000000084848400A5A5A500A5A5
      A500F7FFFF00A5A5A500A5A5A500A5A5A500FFFFFF00FFFFFF00FFFFFF000000
      000000000000000000000000000084000000186B4A0029735A008CC6AD00429C
      7B0029846300186B4A00297B5A00399C7300399C730029845A00186B4A000000
      0000000000000000000000000000000000008484840084848400C6DEC600C6DE
      C600C6DEC600C6DEC600C6DEC600C6DEC600C6DEC600C6DEC600F7FFFF00F7FF
      FF00F7FFFF00C6C6C60084848400C6DEC6000000000084848400A5A5A500A5A5
      A500F7CEA500A5A5A500A5A5A500A5A5A500FFFFFF00FFFFFF00FFFFFF00FFFF
      FF008484840084848400F7CEA500840000000000000084848400A5A5A500A5A5
      A500F7CEA500A5A5A500A5A5A500A5A5A500FFFFFF00FFFFFF000084840000FF
      FF00000000000000000000000000840000000000000021735200317B630094C6
      B5004AA58400429473004AA57B004AA57B004AA57B0073BD9C004A947300105A
      4200000000000000000000000000000000008484840084848400C6DEC600C6DE
      C600C6DEC600C6DEC600C6DEC600C6DEC600C6DEC600F7FFFF00F7FFFF00F7FF
      FF00F7FFFF00F7FFFF0084848400A5A5A5000000000084848400A5A5A500A5A5
      A500F7CEA500A5A5A500A5A5A500A5A5A500F7FFFF00F7FFFF00FFFFFF00FFFF
      FF008484840084848400F7CEA500840000000000000084848400A5A5A500A5A5
      A500F7CEA500A5A5A500A5A5A500A5A5A500F7FFFF000084840000FFFF0000FF
      FF0000FFFF0000000000F7CEA500840000000000000000000000297B5A003984
      630052AD8C0052AD8C0052AD8C0052AD8C0084C6AD005A9C8400085A3900186B
      4A00085A3900085A3900000000000000000084848400A5A5A500C6C6C600F7FF
      FF00F7FFFF00F7FFFF00F7FFFF00F7FFFF00F7FFFF00F7FFFF00F7FFFF00F7FF
      FF00F7FFFF00F7FFFF00C6C6C600848484000000000084848400A5A5A500A5A5
      A500F7CEA500F7CEA500F7CEA500F7CEA500F7CEA500F7CEA500F7FFFF00F7FF
      FF00A5A5A50084848400F7CEA500840000000000000084848400A5A5A500A5A5
      A500F7CEA500F7CEA500F7CEA500F7CEA5000084840000FFFF0000FFFF0000FF
      FF000084840084848400F7CEA500840000000000000000000000318463004A9C
      7B0063B59C0063B59C0063B59C008CCEB500639C8C00105A420021845A002994
      6300217B5A00105A4200000000000000000084848400C6C6C600A5A5A500F7FF
      FF00FFFFFF00F7FFFF00F7FFFF00F7FFFF00F7FFFF00F7FFFF00F7FFFF00F7FF
      FF00F7FFFF00F7FFFF00F7FFFF00848484000000000084848400A5A5A500A5A5
      A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
      A500A5A5A500A5A5A500F7CEA500840000000000000084848400A5A5A500A5A5
      A500A5A5A500A5A5A500A5A5A5000084840000FFFF0000FFFF0000FFFF000084
      8400A5A5A500A5A5A500F7CEA500840000000000000000000000398C6B0073BD
      A50073BDA50073BDA5009CCEBD006BA58C0018634A002984630039946B003994
      6B003994730018634A00000000000000000084848400C6C6C600A5A5A500C6C6
      C600FFFFFF00FFFFFF00F7FFFF00F7FFFF00F7FFFF00F7FFFF00F7FFFF00F7FF
      FF00F7FFFF00FFFFFF00C6DEC600848484000000000084848400C6C6C600C6C6
      C600F7FFFF00F7FFFF00F7FFFF00F7CEA500F7CEA500F7CEA500F7CEA500F7CE
      A500F7CEA500A5A5A500F7CEA500848400000000000084848400C6C6C600C6C6
      C600F7FFFF00F7FFFF000084840000FFFF0000FFFF0000FFFF0000848400F7CE
      A500F7CEA500A5A5A500F7CEA500848400000000000000000000429473009CD6
      C60084CEB500A5D6C60073AD9400216B5200398C6B00429C7B00429C7B00429C
      7B007BBDA500216B4A00000000000000000084848400C6C6C600C6C6C600C6C6
      C600A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
      A500A5A5A5008484840084848400C6C6C6000000000084848400C6C6C600C6C6
      C600FFFFFF00FFFFFF00F7FFFF00F7FFFF00F7FFFF00F7CEA500F7CEA500F7CE
      A500F7CEA500A5A5A500F7CEA500848400000000000084848400C6C6C600C6C6
      C600FFFFFF000084840000FFFF0000FFFF0000FFFF0000848400F7CEA500F7CE
      A500F7CEA500A5A5A500F7CEA500848400000000000052AD5200529C7B00A5D6
      C600B5E7D6007BB59C00297B5A004294730052A5840052A5840052A584005AAD
      8C00639C840029735200000000000000000084848400C6DEC600C6DEC600C6DE
      C600C6DEC600C6DEC600C6DEC600C6DEC600C6DEC600C6DEC600C6DEC600C6DE
      C600C6DEC60084848400C6DEC600000000000000000084848400C6C6C600C6C6
      C600FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7FFFF00F7FFFF00F7CEA500F7CE
      A500F7CEA500A5A5A500F7CEA500848400000000000084848400C6C6C600C6C6
      C600FFFFFF000084840000FFFF0000FFFF0000848400F7FFFF00F7CEA500F7CE
      A500F7CEA500A5A5A500F7CEA50084840000000000000000000052A5840052A5
      840084BDA5003184630052A584005AB594005AB594005AB594006BB59C005AAD
      8C0031846300317B5A00000000000000000084848400C6DEC600C6DEC600C6DE
      C600C6DEC600C6DEC600C6DEC600C6DEC600C6DEC600C6DEC600C6DEC600C6DE
      C600C6C6C6008484840000000000000000000000000084848400C6C6C600F7CE
      A500FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7FFFF00F7FF
      FF00F7CEA500A5A5A500F7CEA500848400000000000084848400C6C6C600F7CE
      A500FFFFFF00008484000084840000848400FFFFFF00FFFFFF00F7FFFF00F7FF
      FF00F7CEA500A5A5A500F7CEA5008484000000000000000000000000000052AD
      84004294730073AD94008CCEB5006BBD9C006BBD9C009CD6BD00A5D6C60084C6
      AD0063B59400398C6B00398C630000000000A5A5A500C6DEC600F7FFFF00F7FF
      FF00F7FFFF00F7FFFF00C6DEC600A5A5A500A5A5A500A5A5A500A5A5A500A5A5
      A500A5A5A500C6DEC60000000000000000000000000084848400C6C6C600C6C6
      C600FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7FF
      FF00F7FFFF00A5A5A500F7CEA500848484000000000084848400C6C6C600C6C6
      C600FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7FF
      FF00F7FFFF00A5A5A500F7CEA500848484000000000000000000000000000000
      00000000000042946B0094C6B500C6E7DE00C6E7DE007BB59C00429473009CCE
      BD008CCEBD0073BDA5004A94730042946B00C6C6C600A5A5A500F7FFFF00F7FF
      FF00F7FFFF00C6DEC600A5A5A500C6C6C6000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400F7CEA500C6DE
      C600FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00A5A5A500F7CEA500848484000000000084848400F7CEA500C6DE
      C600FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00A5A5A500F7CEA500848484000000000000000000000000000000
      000000000000429473004A9C73004A9C73004A9C73004A9C73004A9C73004A9C
      7B00A5D6C6009CD6C600A5D6C6004A9C730000000000C6C6C600A5A5A500A5A5
      A500A5A5A500A5A5A500C6C6C600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400C6C6C600C6C6
      C600FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00A5A5A500F7CEA500848484000000000084848400C6C6C600C6C6
      C600FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00A5A5A500F7CEA500848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000004AA5
      7B0052A57B00ADD6C6007BBD9C0052A57B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400848484008484
      8400848484008484840084848400848484008484840084848400848484008484
      8400848484008484840084848400848484000000000084848400848484008484
      8400848484008484840084848400848484008484840084848400848484008484
      8400848484008484840084848400848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000052A5840052A57B0052A58400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A5A5A500A5A5
      A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
      A500A5A5A500A5A5A500A5A5A500000000000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF00000000000084000000840000008400000084
      0000008400000084000000840000008400000084000000840000008400000084
      0000008400000084000000840000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A5A5A500F7CE
      A500F7CEA500F7CEA500F7CEA500F7CEA500F7CEA500F7CEA500F7CEA500F7CE
      A500F7CEA500A5A5A500A5A5A500000000000000FF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF00000000000084000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000840000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A5A5A500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C6DE
      C600C6C6C600F7CEA500A5A5A500000000000000FF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF00000000000084000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000840000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A5A5A500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7FFFF00C6DE
      C600C6DEC600F7CEA500A5A5A500000000000000FF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF00000000000084000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000840000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A5A5A500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7FFFF00C6DE
      C600C6DEC600F7CEA500A5A5A500000000000000FF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF00000000000084000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000840000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A5A5A500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7FF
      FF00C6DEC600F7CEA500A5A5A500000000000000FF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF00000000000084000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000840000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A5A5A500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7FF
      FF00C6DEC600F7CEA500A5A5A500000000000000FF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF00000000000084000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000840000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A5A5A500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7FF
      FF00C6DEC600F7CEA500A5A5A500000000000000FF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF00000000000084000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000840000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A5A5A500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7FF
      FF00C6DEC600F7CEA500A5A5A500000000000000FF0000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFF0000FFFF
      000000000000000000000000FF00000000000084000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFF0000FFFF
      0000000000000000000000840000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A5A5A500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C6DEC600F7CEA500F7FF
      FF00C6DEC600F7CEA500A5A5A500000000000000FF0000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFF0000FFFF
      000000000000000000000000FF00000000000084000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFF0000FFFF
      0000000000000000000000840000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A5A5A500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7CEA500C6C6C600C6C6
      C600C6C6C600A5A5A500C6DEC600000000000000FF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF00000000000084000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000840000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A5A5A500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00A5A5A500F7CEA500F7CE
      A500A5A5A500C6C6C60000000000000000000000FF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF00000000000084000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000840000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A5A5A500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00A5A5A500F7CEA500A5A5
      A500C6C6C6000000000000000000000000000000FF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF00000000000084000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000840000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A5A5A500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7CEA500F7CEA500C6C6
      C600000000000000000000000000000000000000FF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF00000000000084000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000840000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A5A5A500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7CEA500A5A5A5000000
      0000000000000000000000000000000000000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF00000000000084000000840000008400000084
      0000008400000084000000840000008400000084000000840000008400000084
      0000008400000084000000840000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A5A5A500A5A5
      A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF000000FF000000FF000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF000000FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF0000000000000000000000FF000000FF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000FF000000FF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF00000000000000000000000000000000000000000000000000000000000000
      00000000FF000000FF00000000000000000000000000FFFFFF000000FF000000
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF000000FF000000FF00FFFFFF00000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00000000000000000000000000FF000000000000000000
      000000000000FF0000000000000000000000000000000000000000000000FF00
      0000FF000000FF00000000000000000000000000000000000000000000000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      FF000000FF0000000000000000000000000000000000FFFFFF00FFFFFF000000
      FF000000FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      FF000000FF00FFFFFF00FFFFFF00000000000000000000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00000000000000000000000000FF000000000000000000
      000000000000FF0000000000000000000000000000000000000000000000FF00
      00000000000000000000FF000000000000000000000000000000000000000000
      00000000FF000000FF00000000000000000000000000000000000000FF000000
      FF000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF000000FF000000FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000FF000000
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00000000000000000000000000FF000000000000000000
      000000000000FF0000000000000000000000000000000000000000000000FF00
      00000000000000000000FF000000000000000000000000000000000000000000
      0000000000000000FF000000FF0000000000000000000000FF000000FF000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000FF000000FF00FFFFFF00FFFFFF000000FF000000FF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00000000000000000000000000FF000000000000000000
      000000000000FF0000000000000000000000000000000000000000000000FF00
      00000000000000000000FF000000000000000000000000000000000000000000
      000000000000000000000000FF000000FF000000FF000000FF00000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF000000FF000000FF000000FF000000FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00000000000000000000000000FF000000000000000000
      000000000000FF0000000000000000000000FF0000000000000000000000FF00
      0000FF000000FF00000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FF000000FF0000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000FF000000FF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00000000000000000000000000FF00000000000000FF00
      000000000000FF0000000000000000000000FF0000000000000000000000FF00
      00000000000000000000FF000000000000000000000000000000000000000000
      00000000000000000000000000000000FF000000FF0000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000FF000000FF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00000000000000000000000000FF000000FF0000000000
      0000FF000000FF0000000000000000000000FF0000000000000000000000FF00
      00000000000000000000FF000000000000000000000000000000000000000000
      000000000000000000000000FF000000FF000000FF000000FF00000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF000000FF000000FF000000FF000000FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00000000000000000000000000FF000000000000000000
      000000000000FF0000000000000000000000FF0000000000000000000000FF00
      0000FF000000FF00000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF0000000000000000000000FF000000FF000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000FF000000FF00FFFFFF00FFFFFF000000FF000000FF00FFFF
      FF00FFFFFF0000000000FFFFFF00000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00F7FFFF0000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000FF000000FF00000000000000000000000000000000000000FF000000
      FF000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF000000FF000000FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000FF000000
      FF0000000000FFFFFF00FFFFFF00000000000000000000000000FFFFFF000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      FF000000FF0000000000000000000000000000000000FFFFFF00FFFFFF000000
      FF000000FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      FF000000FF00FFFFFF00FFFFFF00000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF008484
      8400000000008400840000000000000000000000000000000000000000000000
      00000000000000000000FF000000FF000000FF000000FF000000FF0000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF00000000000000000000000000000000000000000000000000000000000000
      00000000FF000000FF00000000000000000000000000FFFFFF000000FF000000
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFF
      FF000000FF000000FF00FFFFFF00000000000000000000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      0000A5A5A5000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF0000000000000000000000FF000000FF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFF
      FF00FFFFFF000000FF000000FF00000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF008400
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF000000FF000000FF000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF000000FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FF000000FF
      000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF
      000000FF000000FF0000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000000000000000000000000000000000FF000000FF
      000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF
      000000FF000000FF0000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000000000000000000000000000000000FF000000FF
      000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF
      000000FF000000FF0000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000000000000000000000000000000000FF000000FF
      000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF
      000000FF000000FF0000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000840000000000000000000000000000000000000000000000000000008400
      0000840000000000000000000000000000000000000000000000C6C6C600C6C6
      C600C6C6C60084848400C6C6C600C6C6C600C6C6C600C6C6C60084848400C6C6
      C600C6C6C600C6C6C6000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0084848400848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000840000000000000000000000000000000000000000000000840000008400
      0000000000000000000000000000000000000000000000000000C6C6C600C6C6
      C600C6C6C60084848400C6C6C600C6C6C600C6C6C600C6C6C60084848400C6C6
      C600C6C6C600C6C6C6000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF0000000000FFFF
      FF00FFFFFF000000000084848400848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000840000000000000000000000000000000000000084000000840000000000
      0000000000000000000000000000000000000000000000000000C6C6C6000000
      0000C6C6C60084848400C6C6C6000000000000000000C6C6C60084848400C6C6
      C60000000000C6C6C6000000000000000000FFFFFF0000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      0000000000000000000084848400848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000840000000000000000000000000000008400000084000000000000000000
      0000000000000000000000000000000000000000000000000000C6C6C600C6C6
      C600C6C6C60084848400C6C6C600C6C6C600C6C6C600C6C6C60084848400C6C6
      C600C6C6C600C6C6C6000000000000000000FFFFFF0000000000FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFF
      FF00FFFFFF000000000084848400848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000840000000000000000000000840000008400000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6C6C6000000
      0000C6C6C60084848400C6C6C6000000000000000000C6C6C60084848400C6C6
      C60000000000C6C6C6000000000000000000FFFFFF0000000000FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFF
      FF00FFFFFF000000000084848400848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000840000000000000084000000840000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6C6C600C6C6
      C600C6C6C60084848400C6C6C600C6C6C600C6C6C600C6C6C60084848400C6C6
      C600C6C6C600C6C6C6000000000000000000FFFFFF0000000000FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFF
      FF00FFFFFF000000000084848400848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000840000008400000084000000840000008400000084000000840000000000
      0000000000000000000000000000000000000000000000000000C6C6C6000000
      0000C6C6C60084848400C6C6C6000000000000000000C6C6C60084848400C6C6
      C60000000000C6C6C6000000000000000000FFFFFF0000000000FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFF
      FF00FFFFFF000000000084848400848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000000000000000000000000000000000000000000000000000C6C6C600C6C6
      C600C6C6C60084848400C6C6C600C6C6C600C6C6C600C6C6C60084848400C6C6
      C600C6C6C600C6C6C6000000000000000000FFFFFF0000000000FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFF
      FF00FFFFFF000000000084848400848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000840000000000000000000000000000000000000000000000000000008400
      0000840000000000000000000000000000000000000000000000C6C6C6000000
      0000C6C6C60084848400C6C6C6000000000000000000C6C6C60084848400C6C6
      C60000000000C6C6C6000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0084848400848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000840000000000000000000000000000000000000000000000000000008400
      0000840000000000000000000000000000000000000000000000C6C6C600C6C6
      C600C6C6C60084848400C6C6C600C6C6C600C6C6C600C6C6C60084848400C6C6
      C600C6C6C600C6C6C6000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0084848400848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000840000000000000000000000000000000000000000000000000000008400
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000000000000000000000000000000000000000000000000000C6C6C600C6C6
      C600C6C6C60084848400C6C6C600C6C6C600C6C6C600C6C6C60084848400C6C6
      C600C6C6C600C6C6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000840000008400000084000000840000008400000084000000840000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF00000000000000
      000000000000000000000000000000000000000000000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C6DEC600A5A5A5000000
      0000000084000000000000008400848484008484840000008400000000008484
      840000000000A5A5A500C6DEC600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF00000000000000FF000000
      0000000000000000000000000000000000000000FF00000000000000FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C6C6C600C6DEC600848484000000
      0000848400008484000000000000000000000000000000000000848400000000
      00000000000084848400C6DEC600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF00000000000000FF000000
      0000000000000000000000000000000000000000FF00000000000000FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C6C6C600C6DEC600848484000000
      8400000000008484840084840000000000000000000084840000848400000000
      00000000000084848400C6DEC600000000000000000000000000000000000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF0000000000000000000000000000000000000000000000FF00000000000000
      000000000000000000000000000000000000000000000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C6C6C600C6DEC600848484008484
      8400000000000000000084840000848400008484000084840000000000000000
      00000000000084848400C6DEC600000000000000000000000000000000000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C6C6C600C6DEC600848484008484
      8400000000000000000000000000848400008484000000000000000000000000
      00000000000084848400C6DEC600000000000084840000848400008484000084
      8400008484000084840000848400008484000084840000848400008484000084
      8400008484000084840000848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C6C6C600C6DEC600848484008484
      8400000000000000000000000000000000000000000000000000000000000000
      00000000000084848400C6DEC600000000000084840000848400008484000084
      8400008484000084840000848400008484000084840000848400008484000084
      840000848400008484000084840000000000000000000000FF00000000000000
      000000000000000000000000000000000000000000000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C6C6C600C6DEC600848484000000
      0000000000008484840084840000000000000000000084840000848400000000
      00000000000084848400C6DEC600000000000084840000848400008484000084
      8400008484000084840000848400008484000084840000848400008484000084
      840000848400008484000084840000000000000000000000FF00000000000000
      000000000000000000000000000000000000000000000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C6C6C600C6DEC600848484008484
      84000000000000000000A5A5A500848400008484000084840000000000000000
      00000000000084848400C6DEC600000000000084840000000000000000000084
      8400000000000000000000848400000000000000000000848400000000000000
      000000848400000000000000000000000000000000000000FF00000000000000
      000000000000000000000000000000000000000000000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C6C6C600C6DEC600848484008484
      8400000000000000000000000000A5A5A5008484000000000000000000000000
      00000000000084848400C6DEC600000000000084840000000000000000000084
      8400000000000000000000848400000000000000000000848400000000000000
      000000848400000000000000000000000000000000000000FF00000000000000
      000000000000000000000000000000000000000000000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C6C6C600C6DEC600848484008484
      8400000000000000000000000000000000000000000000000000000000000000
      00000000000084848400C6DEC600000000000084840000848400008484000084
      8400008484000084840000848400008484000084840000848400008484000084
      840000848400008484000084840000000000000000000000FF00000000000000
      000000000000000000000000000000000000000000000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C6C6C600C6DEC600848484000000
      0000000000008484840084840000000000000000000084840000848484000000
      00000000000084848400C6DEC600000000000000000000000000008484000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF00000000000000
      000000000000000000000000000000000000000000000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C6C6C600C6DEC600848484008484
      84000000000084840000F7CEA5008484000084848400F7CEA500848400000000
      00000000000084848400C6DEC600000000000000000000848400000000000084
      8400000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF00000000000000
      000000000000000000000000000000000000000000000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C6C6C600C6DEC600848484008484
      8400000000000000000084840000F7CEA500F7CEA50084000000000000000000
      00000000000084848400C6DEC600000000000000000000848400000000000084
      8400000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF00000000000000
      000000000000000000000000000000000000000000000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C6C6C600C6DEC600848484008484
      8400000000000000000000000000848400008484000000000000000000000000
      00000000000084848400C6DEC600000000000000000000000000008484000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF00000000000000
      000000000000000000000000000000000000000000000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C6C6C600C6DEC600848484008484
      8400848484008484840084848400848484008484840084848400848484008484
      84000000000084848400C6DEC600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840084848400848484008484
      8400848484008484840084848400848484008484840084848400848484008484
      8400848484008484840084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF000000FF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840084848400848484008484
      8400848484000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF000000FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000FF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF000000FF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FFFF0000FF
      FF0000FFFF00000000000000000000000000C6C6C600C6C6C600C6C6C6000000
      0000000000000000000000000000000000008484840084848400848484008484
      8400848484008484840000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FFFF0000FF
      FF0000FFFF00000000000000000000000000C6C6C600C6C6C600C6C6C6000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FFFF0000FF
      FF0000FFFF00000000000000000000000000C6C6C600C6C6C600C6C6C6000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000FF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840084848400848484008484
      8400848484008484840084848400848484008484840000000000000000008484
      84000000000000000000848484000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008484
      84000000000000000000000000000000000000000000000000000000FF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF000000FF00FFFFFF0000000000000000000000000000000000848484008484
      8400848484008484840084848400848484008484840084848400848484008484
      8400848484000000000000000000000000000000000000000000FF000000FF00
      0000FF0000000000000000000000000000000000FF000000FF000000FF000000
      0000000000000000000000000000000000008484840084848400848484008484
      8400848484008484840084848400848484008484840000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000848484008484
      8400848484008484840084848400848484008484840084848400848484008484
      8400848484000000000000000000000000000000000000000000FF000000FF00
      0000FF0000000000000000000000000000000000FF000000FF000000FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF000000FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000FF00FFFF
      FF00000000000000000000000000000000000000000000000000848484008484
      8400848484008484840084848400848484008484840084848400848484008484
      8400848484000000000000000000000000000000000000000000FF000000FF00
      0000FF0000000000000000000000000000000000FF000000FF000000FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF000000FF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840084848400848484008484
      8400848484008484840084848400848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00C6C6C6008484
      8400FFFFFF000000000084848400848484008484840000000000000084000000
      8400000084000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C6C6
      C600F7FFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840084848400848484008484
      8400848484008484840084848400848484008484840084848400848484008484
      84008484840084848400FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000084
      84008484840084848400F7FFFF00000000000000000000000000000000000000
      000000000000000000000000000000000000848400008484000084840000FFFF
      FF00FFFFFF0084848400A5A5A500FFFFFF00FFFFFF00FFFFFF00FFFFFF008484
      840084848400848484008484000000000000000000000000FF000000FF000000
      FF000000FF000000FF000000000000000000000000000000FF000000FF000000
      FF000000FF000000FF000000000000000000000000000000FF000000FF000000
      0000000000000000FF0000000000000000000000FF000000FF00000000000000
      00000000FF000000000000000000000000000000000000000000C6C6C6008484
      840084848400C6C6C600C6C6C600A5A5A5008484840084848400F7FFFF00FFFF
      FF0000000000000000000000000000000000848400008484840084840000FFFF
      FF00FFFFFF008484000084848400FFFFFF00FFFFFF00FFFFFF00000000008484
      0000848484008484840084840000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF0000000000000000000000
      FF00000000000000FF00000000000000FF0000000000000000000000FF000000
      00000000FF0000000000000000000000000000000000F7FFFF0084848400A5A5
      A500A5A5A50084848400C6DEC600C6DEC600C6DEC600C6C6C600C6C6C600A5A5
      A500A5A5A500000000000000000000000000848400008484840084840000F7FF
      FF00F7FFFF008484000084848400F7FFFF00F7FFFF00F7FFFF00F7FFFF008484
      0000848484008484840084840000000000000000000000840000008400000084
      0000008400000084000000000000000000000000000000840000008400000084
      0000008400000084000000000000000000000000000000000000000000000000
      FF00000000000000FF00000000000000FF0000000000000000000000FF000000
      00000000FF0000000000000000000000000000000000C6C6C60084848400C6C6
      C600C6C6C60084848400C6DEC600C6DEC600C6DEC600C6DEC600C6C6C600C6DE
      C600A5A5A500F7FFFF000000000000000000848400008484840084840000F7FF
      FF00F7FFFF008484000084848400F7FFFF00F7FFFF00F7FFFF00F7FFFF008484
      0000848484008484840084840000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF00000000000000FF00000000000000FF0000000000000000000000FF000000
      00000000FF000000000000000000000000000000000084848400C6DEC600C6DE
      C6008484840000008400A5A5A500A5A5A500C6DEC600C6DEC600F7FFFF00C6DE
      C600C6DEC600C6C6C600A5A5A50000000000848400008484000084840000F7FF
      FF00F7FFFF008400000084848400F7FFFF00F7FFFF00F7FFFF00F7FFFF008484
      0000848484008484000084840000FFFFFF000000000000840000008400000084
      000000840000008400000000000000000000000000000000FF000000FF000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      FF00000000000000FF00000000000000FF0000000000000000000000FF000000
      00000000FF000000000000000000000000000000000084848400C6DEC600C6DE
      C600848484000000FF0084848400A5A5A500A5A5A500A5A5A50084848400C6DE
      C600C6DEC600C6DEC600C6C6C600FFFFFF00848400008484000084840000F7FF
      FF00F7FFFF00F7FFFF00F7FFFF00F7FFFF00F7FFFF00F7FFFF00F7FFFF008484
      0000848484008484000084840000848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      0000000000000000FF00000000000000FF0000000000000000000000FF000000
      00000000FF00000000000000000000000000F7FFFF00C6DEC600C6DEC600C6DE
      C60000848400008484000000FF000000FF0084848400C6C6C600848484000084
      8400A5A5A50000000000F7FFFF00C6DEC6008484000084840000848400008484
      0000848400008484000084840000848400008484000084840000848400008484
      0000848484008484000084840000FFFFFF000000000000840000008400000084
      0000008400000084000000000000000000000000000000840000008400000084
      000000840000008400000000000000000000000000000000FF00000000000000
      0000000000000000FF00000000000000FF0000000000000000000000FF000000
      00000000FF000000FF000000000000000000C6C6C600F7FFFF00C6DEC600C6DE
      C600C6DEC6008484840000FFFF0000FFFF000084840000848400848484000084
      840000FFFF00A5A5A500FFFFFF00000000008484000084840000848400008484
      0000848400008484000084840000848400008484000084840000848400008484
      0000848400008484000084840000FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF0000000000000000000000
      0000000000000000FF00000000000000FF0000000000000000000000FF000000
      00000000FF00000000000000FF0000000000A5A5A500F7FFFF00F7FFFF00F7FF
      FF00F7FFFF00C6DEC600C6DEC600A5A5A500A5A5A500C6DEC60000FFFF0000FF
      FF00C6DEC60000FFFF0000FFFF00848484008484000084848400848484008484
      8400848484008484840084848400848400008484000084840000848400008484
      000084840000848400008484000000000000000000000000FF000000FF000000
      FF000000FF000000FF000000000000000000000000000000FF000000FF000000
      FF000000FF000000FF0000000000000000000000FF0000000000000000000000
      0000000000000000FF00000000000000FF0000000000000000000000FF000000
      00000000FF00000000000000FF0000000000A5A5A500F7FFFF00F7FFFF00C6DE
      C600C6C6C600C6DEC600C6DEC600F7FFFF00A5A5A500A5A5A500A5A5A500C6DE
      C600C6DEC60000FFFF0000848400C6C6C600848400008484840084848400C6DE
      C600C6DEC600C6DEC600C6DEC600C6DEC600C6DEC600C6DEC600C6DEC600C6DE
      C600848400008484000084840000FF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF0000000000000000000000
      0000000000000000FF00000000000000FF0000000000000000000000FF000000
      00000000FF00000000000000FF000000000000000000F7FFFF00C6DEC600F7FF
      FF000000000000000000FFFFFF00F7FFFF00C6C6C600C6C6C600C6DEC6000084
      840084848400000000000000000000000000848400008484840084848400C6DE
      C60084000000840000008400000084000000840000008400000084000000C6DE
      C600848400008484000084840000000000000000000000840000008400000084
      000000840000008400000000000000000000000000000000FF000000FF000000
      FF000000FF000000FF0000000000000000000000FF0000000000000000000000
      0000000000000000FF00000000000000FF0000000000000000000000FF000000
      00000000FF00000000000000FF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000848400008484840084848400C6DE
      C600C6DEC600C6DEC600C6DEC600C6DEC600C6DEC600C6DEC600C6DEC600C6DE
      C600848400008484000084840000FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF0000000000000000000000
      FF00000000000000FF00000000000000FF0000000000000000000000FF000000
      00000000FF00000000000000FF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000840000008484840084848400C6DE
      C60084000000840000008400000084000000840000008400000084000000C6DE
      C600848400008484000084000000F7CEA500000000000000FF000000FF000000
      FF000000FF000000FF0000000000000000000000000000840000008400000084
      000000840000008400000000000000000000000000000000FF000000FF000000
      00000000FF000000FF000000FF00000000000000FF000000FF00000000000000
      00000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000840000000000000084000000F7FF
      FF00F7FFFF00F7FFFF00F7FFFF00F7FFFF00F7FFFF00F7FFFF00F7FFFF00F7FF
      FF00840000008400000000000000A5A5A5000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF00000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00848484000000000084848400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF0000FF
      FF000000000000FFFF00000000000000000000FFFF000000000000FFFF000000
      000000FFFF0000FFFF0000FFFF00000000000000000000000000FFFFFF00FFFF
      FF008484840000000000848484000000000000FFFF0000000000848484000000
      000084848400FFFFFF00FFFFFF00000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF0000FF
      FF000000000000FFFF00000000000000000000FFFF000000000000FFFF000000
      000000FFFF0000FFFF0000FFFF00000000000000000000000000FFFFFF00C6C6
      C6000000000000FFFF00848484000000000000FFFF00000000008484840000FF
      FF0000000000C6C6C600FFFFFF00000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000000000000000000000000000848400008484000084
      8400008484000084840000848400008484000084840000848400000000000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF00000000000000000000000000FFFFFF000000
      0000848484008484840000FFFF0000FFFF0000FFFF0000FFFF0000FFFF008484
      84008484840000000000FFFFFF00000000000000000000FFFF0000FFFF000000
      00000000000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000000000000000000000000000848400008484000084
      8400008484000084840000848400008484000084840000848400000000000084
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF008484
      8400000000000000000000FFFF0084848400000000008484840000FFFF000000
      00000000000084848400FFFFFF00000000000000000000FFFF0000FFFF000000
      00000000000000000000000000000000000000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000084
      840000848400000000000000000000000000000000000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      000000FFFF0000FFFF0000FFFF0000000000FFFFFF000000000000FFFF0000FF
      FF0000FFFF0000000000FFFFFF00000000000000000000FFFF0000FFFF0000FF
      FF000000000000000000000000000000000000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000000000000000000000000000000000008484000084
      8400008484000084840000848400008484000000FF00008484000000FF000000
      000000848400008484000000000000000000000000000000FF00000000000000
      FF000000000000000000000000000000FF0000000000000000000000FF000000
      0000000000000000000000000000000000000000000000000000FFFFFF008484
      8400000000000000000000FFFF0084848400000000008484840000FFFF000000
      00000000000084848400FFFFFF00000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000000000000000000000000000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000000000000000000000000000000000000000000084
      8400FFFF0000FFFF0000FFFF0000008484000084840000848400008484000084
      840000000000008484000000000000000000000000000000FF00000000000000
      FF000000000000000000000000000000FF0000000000000000000000FF000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00848484008484840000FFFF0000FFFF0000FFFF0000FFFF0000FFFF008484
      840084848400FFFFFF00FFFFFF00000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000000000000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000000000000000000000000000000000000000000000
      000000848400008484000084840000848400008484000000FF00008484000000
      FF0000848400000000000000000000000000000000000000FF00000000000000
      FF000000FF00000000000000FF00000000000000FF000000FF00000000000000
      FF00000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF000000000000FFFF00848484000000000000FFFF00000000008484840000FF
      FF0000000000FFFFFF00FFFFFF00000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF00000000000000000000000000000000000000
      000000FFFF0000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF008484840000000000848484000000000000FFFF0000000000848484000000
      000084848400FFFFFF00FFFFFF00000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF00000000000000000000FFFF000000
      00000000000000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00848484000000000084848400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF000000000000FFFF0000FFFF0000FF
      FF000000000000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400000000008484
      8400000000008484840084848400848484000000000084848400000000008484
      8400848484008484840084848400000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF00000000000000000000FFFF000000
      00000000000000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400000000008484
      8400000000008484840000000000848484008484840084848400000000008484
      8400000000008484840084848400000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF0000000000000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000000000000000000000
      000000FFFF0000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400000000008484
      8400000000008484840000000000848484008484840084848400000000008484
      8400000000008484840084848400000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00C6DEC600A5A5A5008484840084848400A5A5A500C6DEC600FFFF
      FF0000000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000084840084000000008484000084840084000000008484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000084840084000000008484000084840084000000008484000000
      0000000000000000000000000000000000000000000000000000C6DEC600A5A5
      A500A5A5A500A5A5A500A5A5A500848484008484840084848400848484008484
      8400A5A5A500C6C6C60000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF0084848400FF00000084848400FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000008484
      84000084840000FFFF008484840000848400008484008484840000FFFF000084
      8400848484000000000000000000000000000000000000000000000000008484
      84000084840000FFFF008484840000848400008484008484840000FFFF000084
      84008484840000000000000000000000000000000000FFFFFF00A5A5A500A5A5
      A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A50084848400848484008484
      84008484840084848400FFFFFF0000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FF000000FF000000FF000000FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000848484008484840000FFFF0000FFFF0000FFFF0000FFFF00848484008484
      8400000000000000000000000000000000000000000000000000000000000000
      0000848484008484840000FFFF0000FFFF0000FFFF0000FFFF00848484008484
      84000000000000000000000000000000000000000000FFFFFF00A5A5A500A5A5
      A500A5A5A500A5A5A500A5A5A500A5A5A5008484840084848400C6DEC600C6C6
      C6008484840084848400F7FFFF00000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0084848400FF00000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000008484
      8400008484000084840000848400840000008400000000848400008484000084
      8400848484000000000000000000000000000000000000000000000000008484
      8400008484000084840000848400840000008400000000848400008484000084
      840084848400000000000000000000000000FFFFFF00F7FFFF00A5A5A500A5A5
      A500A5A5A500A5A5A5008484840084848400848484008484840000000000F7FF
      FF008484840084848400C6C6C6000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      00000084840000FFFF000084840084848400848484000084840000FFFF000084
      8400000000000000000000000000000000000000000000000000000000000000
      00000084840000FFFF000084840084848400848484000084840000FFFF000084
      84000000000000000000000000000000000000000000A5A5A500A5A5A500A5A5
      A500A5A5A500F7FFFF00C6DEC600C6DEC600FFFFFF00FFFFFF00848484008484
      84008484840084848400A5A5A5000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FF000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000840000000084840000FFFF00008484000084840000FFFF00008484008400
      0000000000000000000000000000000000000000000000000000000000000000
      0000840000000084840000FFFF00008484000084840000FFFF00008484008400
      00000000000000000000000000000000000000000000A5A5A500A5A5A500A5A5
      A500C6DEC600FFFFFF00A5A5A500A5A5A500A5A5A500FFFFFF0084848400A5A5
      A500A5A5A5008484840084848400FFFFFF0000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FF00000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      00008484840000FFFF000084840000FFFF0000FFFF000084840000FFFF008484
      8400000000000000000000000000000000000000000000000000000000000000
      00008484840000FFFF000084840000FFFF0000FFFF000084840000FFFF008484
      84000000000000000000000000000000000000000000A5A5A500C6C6C600A5A5
      A500F7FFFF00F7FFFF00A5A5A500A5A5A500A5A5A500C6DEC60084848400A5A5
      A500A5A5A500A5A5A500848484000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FF000000FF000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000084840084000000008484000084840084000000008484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000084840084000000008484000084840084000000008484000000
      000000000000000000000000000000000000FFFFFF00A5A5A500C6C6C600A5A5
      A500F7FFFF00F7FFFF00A5A5A500A5A5A500A5A5A500F7FFFF0084848400A5A5
      A500A5A5A500A5A5A500A5A5A5000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF000000FF000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C6DEC600C6C6C600A5A5
      A500C6C6C600F7FFFF00F7FFFF00F7FFFF00FFFFFF00F7FFFF00A5A5A500A5A5
      A500A5A5A500A5A5A500C6C6C6000000000000000000FFFFFF00FFFFFF00FFFF
      FF008484840084848400FFFFFF00FFFFFF0084848400FF000000FF000000FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00A5A5A500C6C6
      C600A5A5A500C6C6C600F7FFFF00F7FFFF00C6DEC600A5A5A500A5A5A500A5A5
      A500A5A5A500A5A5A500F7FFFF000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FF000000FF000000FFFFFF00FFFFFF0084848400FF000000FF000000FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00A5A5A500A5A5
      A500C6C6C600A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
      A500A5A5A500A5A5A500FFFFFF00000000000000000000000000FFFFFF00FFFF
      FF00FF000000FF00000084848400FFFFFF0084848400FF000000FF000000FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00C6DEC600A5A5
      A500A5A5A500C6C6C600C6C6C600A5A5A500A5A5A500A5A5A500A5A5A500A5A5
      A500A5A5A500C6DEC600FFFFFF0000000000000000000000000000000000FFFF
      FF00FFFFFF00FF000000FF000000FF000000FF000000FF000000FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00C6DEC600A5A5A500A5A5A500A5A5A500A5A5A500C6DEC600F7FF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FF000000FF000000FF000000FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF000000000000000000000000000000000000000000FFFFFF000000
      000000000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FF000000FF000000FF000000FF000000FF0000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF000000FF000000FF000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      000000FF000000FF000000FF0000FF00000000FF000000FF000000FF000000FF
      0000000000000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF000000FF00FF0000000000FF000000FF000000FF000000
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FF000000FF
      000000FF0000FF00000000FF000000FF000000FF0000FF00000000FF000000FF
      000000FF000000000000000000000000000000000000000000000000FF000000
      FF000000FF00FF0000000000FF000000FF000000FF00FF0000000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FF000000FF
      000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF
      000000FF000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FF000000FF0000FF00
      000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000FF00
      000000FF000000FF00000000000000000000000000000000FF000000FF00FF00
      00000000FF000000FF000000FF000000FF000000FF000000FF000000FF00FF00
      00000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FF000000FF000000FF
      000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF
      000000FF000000FF00000000000000000000000000000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FF0000FF00000000FF
      000000FF000000FF000000FF000000000000000000000000000000FF000000FF
      0000FF00000000FF00000000000000000000000000000000FF00FF0000000000
      FF000000FF000000FF000000FF000000000000000000000000000000FF000000
      FF00FF0000000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FF000000FF000000FF
      000000FF000000FF000000FF00000000000000FF000000FF000000FF000000FF
      000000FF000000FF00000000000000000000000000000000FF000000FF000000
      FF000000FF000000FF000000FF00000000000000FF000000FF000000FF000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FF000000FF0000FF00
      000000FF000000FF000000FF00000000000000FF000000FF000000FF0000FF00
      000000FF000000FF00000000000000000000000000000000FF000000FF00FF00
      00000000FF000000FF000000FF00000000000000FF000000FF000000FF00FF00
      00000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FF000000FF
      000000FF000000FF000000FF00000000000000FF000000FF000000FF000000FF
      000000FF000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF00000000000000FF000000FF000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FF000000FF
      0000FF00000000FF000000FF00000000000000FF000000FF0000FF00000000FF
      000000FF000000000000000000000000000000000000000000000000FF000000
      FF00FF0000000000FF000000FF00000000000000FF000000FF00FF0000000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      000000FF000000FF000000FF0000FF00000000FF000000FF000000FF000000FF
      0000000000000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF000000FF00FF0000000000FF000000FF000000FF000000
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FF000000FF000000FF000000FF000000FF0000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF000000FF000000FF000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FF000000FF000000FF000000FF000000FF000000FF00000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF000000FF000000FF000000FF000000FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF
      0000000000000000000000000000000000000000000000000000000000000000
      00000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF
      000000FF00000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FF000000FF
      000000000000000000000000000000FF000000FF000000FF0000000000000000
      000000FF000000FF0000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FF00000000
      000000FF000000FF00000000000000FF000000FF00000000000000FF000000FF
      00000000000000FF0000000000000000000000000000000000000000FF000000
      00000000FF000000FF00000000000000FF000000FF00000000000000FF000000
      00000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FF00000000
      000000FF0000000000000000000000FF000000FF00000000000000FF000000FF
      00000000000000FF000000000000000000000000000000000000000000000000
      FF00000000000000FF00000000000000FF00000000000000FF00000000000000
      00000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FF00000000
      000000FF000000FF000000FF000000FF000000FF00000000000000FF000000FF
      00000000000000FF0000000000000000000000000000000000000000FF000000
      00000000FF000000FF00000000000000FF00000000000000FF00000000000000
      0000000000000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FF00000000
      000000FF000000FF00000000000000FF000000FF00000000000000FF000000FF
      00000000000000FF000000000000000000000000000000000000000000000000
      FF00000000000000FF00000000000000FF00000000000000FF00000000000000
      00000000FF00000000000000000000000000000000000000000000FF000000FF
      000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF
      000000FF000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF00000000000000000000000000000000000000000000FF000000FF
      0000000000000000000000FF000000FF000000FF000000FF0000000000000000
      000000FF000000FF0000000000000000000000000000000000000000FF000000
      00000000FF000000000000000000000000000000FF00000000000000FF000000
      0000000000000000FF000000000000000000000000000000000000FF000000FF
      000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF
      000000FF000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF0000000000000000000000000000000000000000000000000000FF
      000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF
      000000FF00000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF00000000000000000000000000000000000000000000FF000000FF
      000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF
      000000FF000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF
      0000000000000000000000000000000000000000000000000000000000000000
      00000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF0000000000000000000000000000000000000000000000000000FF000000FF
      000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF
      000000FF000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      00000000000000FF000000FF000000FF000000FF000000FF000000FF00000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF000000FF000000FF000000FF000000FF000000
      000000000000000000000000000000000000000000000000000000FF000000FF
      000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF
      000000FF000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FF000000FF
      000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF
      000000FF000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FF000000FF
      000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF
      000000FF00000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000FF000000000000000000000000000000000000000000000000000000
      FF0000000000000000000000000000000000000000000000000000FF000000FF
      000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF
      000000FF000000FF00000000000000000000000000000000000000FF000000FF
      000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF
      000000FF00000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF00000000000000000000000000000000000000FF000000
      000000000000000000000000000000000000000000000000000000FF000000FF
      000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF
      000000FF000000FF000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF0000000000000000000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FF000000FF
      000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF
      000000FF000000FF000000000000000000000000000000000000000084000000
      84000000840000008400000084000000FF000000FF0000008400000084000000
      8400000084000000840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF00000000000000000000000000000000000000000000000000000000000000
      FF0000000000000000000000000000000000000000000000000000FF000000FF
      000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF
      000000FF000000FF000000000000000000000000000000000000000084000000
      84000000840000008400000084000000FF000000FF0000008400000084000000
      8400000084000000840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000FF0000000000000000000000000000000000000000000000FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF0000000000000000000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000000000000000000000000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF00000000000000000000000000000000000000FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000084000000
      840000008400000084000000FF000000FF000000FF0000008400000084000000
      8400000084000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000FF000000000000000000000000000000000000000000000000000000
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000084000000
      840000008400000084000000FF000000FF000000FF0000008400000084000000
      8400000084000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000000000000000000000000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000FF0000000000000000000000000000000000000000000000FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF00000000000000000000000000000000000000000000000000000000000000
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000E00000000100010000000000000700000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FFFFFFFFFFFF00000001C000C0000000
      0001800080000000000180008000000000008000800000000000800080000000
      0000800080000000000080008000000000008000800000000000800080000000
      00018000800000000003800080000000000380008000000000FF800080000000
      81FF800080000000FFFF800080000000FFFFFFFFFFFFC00100010001FFFFC001
      7FFD7FFDE7FFC00100010001CFFFC001492549258003C001400540058003C001
      40054005CFFFC00140054005E7FFC00140054005FFFFC0014A854A85FFCFC001
      4A854A85FFE7C0014F854F858003C0037FFD7FFD8003C0076FFD6FFDFFE7C00F
      77FD77FDFFCFC01F00010001FFFFC03FFFFF00000000FFFFFFFF1FF800008001
      FFFF4FF200008001BBE3448200008001BBED73CE00008001BBED408200008001
      BBED7C3E00008001BB63440200008001AB6D7E7E00008001936D440200008001
      BB63799E00008001FF7F408200008001FF7F67E600008001FC1F448200008003
      FFFF1FF800008007FFFF00000000800FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      7FFE7FFE7FFE7FFE000000000000000000000000000000007FFE7FFE7FFE7FFE
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDA37DA37FFFFFFFFD2F7D2F7ADB3ADB3
      D2F7D2F7A9B7A9B7CA77CA77A9B3A9B3CAF7CAF7A5B7A5B7DA23DA23AD13AD13
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8001
      0000DFF7E7E780010000BFFBE7CF800100000001E79F80014040BFFBE73F8001
      4040DFF7E67F80014040FFFFE4FF80014040FFFFE01F800140408CDDE00F8001
      40407B5DE7E7800100007B55E7E7800100007B49E7E7800100008CDDE00F8001
      0001FFFFE01F8001FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBBBBBBBB8001FFFF
      5555555500010001555555550001E00FBBBBBBBB0001E00FFFFFFFFF00010001
      FFFFFFFF00010001BBBBBBBB00010001BBBBBBBB00016DB7BBBBBBBB00016DB7
      BBBBBBBB00010001BBBBBBBB0001DFFFBBBBBBBB0001AFFFBBBBBBBB0001AFFF
      BBBBBBBB0001DFFFBBBBBBBB0001FFFFFFFFFFFFFFFFFFFFFC7F0001FFFFFFFF
      F00F0001FFFF07FFC007FFFFFFFFFBFFC003FEFF820FFDFF8003FC7F820F02FF
      8001FABF820FFF7F0001FEFF820FFFFF0001FEFF820F00010001FFFFFFFFFFFF
      80018003820FFFEF80038003820F005FC0038003820FFFBFC0078003820FFF7F
      F00F8003820F00FFFC7FFFFFFFFFFFFFFFFF8447FFFFFFFFE7FF0001FFFFFFFF
      E1FF000183839B37C00F0021FFFF6AD7800700018383EAD780030001FFFFEAD7
      800100008383EAD780000000FFFFDAD7000400008383BAD300010000FFFF7AD5
      0000000183837AD500000000FFFF7AD58C07000183837AD5FFFF0000FFFF6AD5
      FFFF000083839133FFFF0000FFFFFFFFFFFFFFFF8000FFFFFFFF00018000FFFF
      FFFF000180008003001F000180008003000F0001800080030007FFFF80008003
      0003BFFF800080038001AEDF80008003C001AEDF80008003E001A52F80008003
      F001BFFF80008003FF7F000180008003FFBF000180008003FFBF000180018003
      FFBF000180038003FFDF00018007FFFFFFFFFFFFFDFEFFFFFE7FFE7FF00BF01F
      F00FF00FC003E00FE007E0078001C007E007E00780018003E007E00700210001
      E007E00780010001F00FF00F80000001F00FF00F80010001F00FF00F00010001
      FE7F9E7F80010001BB636DBB80010001BB6BDDBB80018003AB63BDAB8001C007
      936B6D938001E00FBA239DBBF7DBF01FFFFFFFFFFFFFFFFFF83FF83FFFFFFFFF
      E00FE00FFFFFFFFFC007C007FFFFFFFF80038003FFFFFFFF80038003C7FFE3FF
      0001000183FFC1FF00010001119D08F90001000138391C0100010001FC39FE01
      00010001FF9DFFF980038003FFFFFFFF80038003FFFFFFFFC007C007FFFFFFFF
      E00FE00FFFFFFFFFF83FF83FFFFFFFFFFFFFFFFFFFFFFFFFF81FF81FA733A733
      F00FF00FAAEFAAEFE007E007AAEFAAEFC003C003AAEFAAEF80018001A733A733
      80018001BFFFBFFF80018001BFFFBFFF80018001800380038001800180038003
      8001800180038003C003C00380038003E007E00780038003F00FF00F80038003
      F81FF81F80038003FFFFFFFF80038003FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF7FFDFFFFFFFF7FFE3FF9FFFFFFFF3FFC0001FFFFF7EF000000017FFE7BDE
      00003FF93FFC3DBC3FFC7FFD000000007FFEEFEF00000000FFFF77DD3FFC3DBC
      C6C73BB97FFE7BDEDEDB0001FFFFF7EFDEC70001FFFFFFFFDEDB3BB9FFFFFFFF
      DEC777DDFFFFFFFFFFFFEFEFFFFFFFFF00000000000000000000000000000000
      000000000000}
  end
  object SPD_Save: TSavePictureDialog
    Filter = 'Bitmaps (*.bmp)|*.bmp'
    Title = 'Ulozit snimek reliefu'
    Left = 776
    Top = 456
  end
  object IL_Bloky: TImageList
    BkColor = clBlack
    Left = 1176
    Top = 456
    Bitmap = {
      494C01010C007802D8021000100000000000FF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000004000000001002000000000000040
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000059B300000000000059B3000059B3000059B300000000000059B3000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000059B300000000000059B3000059B3000059B300000000000059B3000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000059B300000000000059B3000059B3000059B300000000000059B3000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000059B3000059B300000000000059B300000000000059B3000059B3000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000059B300000000000059B300000000000059B300000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000059B300000000000059B300000000000059B300000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000059B300000000000059B300000000000059B300000000000000
      0000000000000000000000000000000000000000000000000000000080000000
      8000000000000000000000000000FFFFFF00FFFFFF0000000000000000000000
      000000FF000000FF000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000059B3000059B300000000000059B3000059B300000000000000
      0000000000000000000000000000000000000000000000000000000080000000
      8000000000000000000000000000FFFFFF00FFFFFF0000000000000000000000
      000000FF000000FF000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000059B300000000000059B30000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFDBB700FFDBB700FFDBB700FFDB
      B700FFDBB700FFDBB700FFDBB700FFDBB700FFDBB700FFDBB700000000000000
      800000000000FFDBB700FFDBB700FFDBB7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFDBB700FFDBB700FFDBB700FFDB
      B700FFDBB700FFDBB700FFDBB700FFDBB700FFDBB700FFDBB700000080000000
      800000008000FFDBB700FFDBB700FFDBB7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFDBB700FFDBB700FFDBB700FFDB
      B700FFDBB700FFDBB700FFDBB700FFDBB700FFDBB700FFDBB700000000000000
      800000000000FFDBB700FFDBB700FFDBB7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFDBB700FFDBB700FFDBB700FFDB
      B700FFDBB700FFDBB700FFDBB700FFDBB700FFDBB700FFDBB7000000000000FF
      000000000000FFDBB700FFDBB700FFDBB7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFDBB700FFDBB700FFDBB700FFDB
      B700FFDBB700FFDBB700FFDBB700FFDBB700FFDBB700FFDBB70000FF000000FF
      000000FF0000FFDBB700FFDBB700FFDBB7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFDBB700FFDBB700FFDBB700FFDB
      B700FFDBB700FFDBB700FFDBB700FFDBB700FFDBB700FFDBB7000000000000FF
      000000000000FFDBB700FFDBB700FFDBB7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFDBB700FFDBB700FFDBB700FFDB
      B700FFDBB700FFDBB700FFDBB700FFDBB700FFDBB700FFDBB700FFDBB700FFDB
      B700FFDBB700FFDBB700FFDBB700FFDBB7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000FFFF0000FFFF0000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000800080008000
      8000800080008000800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF0000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00000000000000FF000000FF0000000000800080008000
      80008000800080008000000000000000FF000000000000000000000000000000
      00000000000000FF000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000241CED00241CED0000000000241CED00241CED000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000800080008000
      8000800080008000800000000000000000000000000000000000000000000000
      000000FF000000FF000000FF0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000241CED00241CED0000000000241CED00241CED000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FF000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000FF000000FF000000FF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000241CED00241CED00000000000000000000000000241CED00241C
      ED00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000241CED00241CED0000000000241CED00241CED000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000241CED00241CED00241CED00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000241CED00241CED0000000000241CED00241CED000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000FFFF0000FFFF0000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000241CED00241CED00000000000000000000000000241CED00241C
      ED00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FF000000FF000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FFFF000000FF000000FF000000FF0000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FF000000FF00000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FFFF000000FF000000FF000000FF0000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FFFF000000FF000000FF000000FF0000FFFF00000000000000
      00000000000000000000000000000000000000000000FF000000FF000000FF00
      0000FF000000000000000000000000000000FF0000000000000000000000FF00
      0000FF000000FF000000FF000000000000000000000000000000000000000000
      000000000000FF000000FF000000FF000000FF000000FF000000FF0000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FF000000FF000000FF000000FF000000FF000000FF000000FF00
      0000FF000000FF00000000000000000000000000000000000000000000000000
      00000000000000FFFF000000FF000000FF000000FF0000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FF00
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FFFF000000FF000000FF000000FF0000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FF000000FF00000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF0000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FF000000FF000000FF000000FF000000FF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000400000000100010000000000000200000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000F01FFFFFFFFFC001F01FDFF7FFFFE003
      F01F8FE3FFFFF007F01FDFF78003F80FF83FFFFFE00FFC1FF82FCE73E00FFE3F
      F82F8421E38FFF7FF82F8421EFEFFFFFFC6FCE73EFEFFFFF0000FFFFE38FFFFF
      0000FFFFE00FFFFF00008DA9E00FFFFF0000AA8BFFFFFFFF0000AAADFFFFFFFF
      0000AAD9FFFFFFFF0000FFFFFFFFFFFFFFFFE0FFFFFFFF7FFFFFE0FF9F83FF7F
      FFFFE0ED9F81F0078181E0ED9F99F7F78181E0E19F99F7F70000E0ED9F99F497
      8181E0FF9F99F4978181E0FB8799F7F7FFFFE0F58799F007FFFFE0F59F99FF7F
      FDAFE0FB9F99F94FFDAFE0FF9F99FC1FFD9FE0999F99FE3FFDAFE0D78181FC9F
      FD9FE0B78183F9CFFFFFE099FFFFFFFFFFFFFFFFFFFFFFFFF83FFFFFFFFC01FF
      F83FFE7F7FFBFEFFF83FFFBFBFF7FF7FF83F076000000000F83FFFBFEFFDFFEF
      F83FFE7FDFFEFFDFF83FFFFF3FFF003FFEE0FFFFFFFFFFFFFEC0FDBFF6DFFE7F
      FE9FFD7FF6DFFDBFFE3FFCFFF55FFF7FFE7FFCFFF55FFEFF00E0FD7FF39FFDBF
      01C0FDBFF39FFE7FFFFFFFFFFFFFFFFF00000000000000000000000000000000
      000000000000}
  end
  object IL_MTB: TImageList
    BkColor = clBlack
    Height = 14
    Left = 1128
    Top = 456
    Bitmap = {
      494C010106004801880210000E0000000000FF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001C0000000100200000000000001C
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF00000000000000000000840000008400000084
      0000008400000084000000840000008400000084000000840000008400000084
      0000008400000084000000840000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF000000000000FF
      FF0000FFFF0000FFFF0000FFFF00000000000000000000FFFF0000FFFF0000FF
      FF000000000000FFFF0000FFFF00000000000000000000840000008400000000
      0000000000000084000000840000000000000084000000840000008400000000
      0000008400000000000000840000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF000000000000FF
      FF0000FFFF0000FFFF000000000000FFFF0000FFFF000000000000FFFF0000FF
      FF000000000000FFFF0000FFFF00000000000000000000840000000000000084
      0000008400000000000000840000000000000084000000840000000000000000
      0000008400000000000000840000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF00000000000000
      000000FFFF0000FFFF000000000000FFFF0000FFFF000000000000FFFF0000FF
      FF000000000000FFFF0000FFFF00000000000000000000840000000000000084
      0000008400000000000000840000000000000084000000000000008400000000
      0000008400000000000000840000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF000000000000FF
      FF000000000000FFFF000000000000FFFF0000FFFF000000000000FFFF0000FF
      FF000000000000FFFF0000FFFF00000000000000000000840000000000000084
      0000008400000000000000840000000000000084000000000000008400000000
      0000008400000000000000840000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF000000000000FF
      FF000000000000FFFF000000000000FFFF0000FFFF000000000000FFFF0000FF
      FF000000000000FFFF0000FFFF00000000000000000000840000000000000084
      0000008400000000000000840000000000000000000000840000008400000000
      0000008400000000000000840000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF00000000000000
      000000FFFF0000FFFF0000FFFF00000000000000000000FFFF0000FFFF000000
      0000000000000000000000FFFF00000000000000000000840000000000000084
      0000008400000000000000840000000000000084000000840000008400000000
      0000008400000000000000840000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF00000000000000000000840000008400000084
      0000008400000084000000840000008400000084000000840000008400000084
      0000008400000084000000840000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000840000008400000084
      0000008400000084000000840000008400000084000000840000008400000084
      000000840000008400000084000000000000000000000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF0000000000000000000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000000000000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000000000000000000000840000008400000000
      0000000000000084000000840000000000000084000000840000008400000000
      000000840000000000000084000000000000000000000000FF000000FF000000
      00000000FF000000FF000000FF00000000000000FF000000FF00000000000000
      0000000000000000FF000000FF0000000000000000000000FF000000FF000000
      00000000FF000000FF000000FF00000000000000FF000000FF00000000000000
      0000000000000000FF000000FF000000000000000000FF00000000000000FF00
      000000000000FF000000000000000000000000000000FF000000FF0000000000
      000000000000FF000000FF000000000000000000000000840000000000000084
      0000008400000000000000840000000000000084000000840000000000000000
      000000840000000000000084000000000000000000000000FF000000FF000000
      00000000FF000000FF000000FF00000000000000FF000000FF00000000000000
      FF000000FF000000FF000000FF0000000000000000000000FF000000FF000000
      00000000FF000000FF000000FF00000000000000FF000000FF00000000000000
      FF000000FF000000FF000000FF000000000000000000FF00000000000000FF00
      000000000000FF00000000000000FF000000FF000000FF00000000000000FF00
      0000FF00000000000000FF000000000000000000000000840000000000000084
      0000008400000000000000840000000000000084000000000000008400000000
      000000840000000000000084000000000000000000000000FF000000FF000000
      00000000FF000000FF000000FF00000000000000FF000000FF00000000000000
      FF000000FF000000FF000000FF0000000000000000000000FF000000FF000000
      00000000FF000000FF000000FF00000000000000FF000000FF00000000000000
      FF000000FF000000FF000000FF000000000000000000FF000000000000000000
      0000FF000000FF00000000000000FF000000FF000000FF00000000000000FF00
      00000000000000000000FF000000000000000000000000840000000000000084
      0000008400000000000000840000000000000084000000000000008400000000
      000000840000000000000084000000000000000000000000FF000000FF000000
      00000000FF000000FF000000FF00000000000000FF000000FF00000000000000
      FF000000FF000000FF000000FF0000000000000000000000FF000000FF000000
      00000000FF000000FF000000FF00000000000000FF000000FF00000000000000
      FF0000000000000000000000FF000000000000000000FF00000000000000FF00
      000000000000FF0000000000000000000000FF000000FF00000000000000FF00
      0000FF000000FF000000FF000000000000000000000000840000000000000084
      0000008400000000000000840000000000000000000000840000008400000000
      000000840000000000000084000000000000000000000000FF000000FF000000
      00000000FF000000FF000000FF00000000000000FF000000FF00000000000000
      FF000000FF000000FF000000FF0000000000000000000000FF000000FF000000
      00000000FF000000FF000000FF00000000000000FF000000FF00000000000000
      FF0000000000000000000000FF000000000000000000FF00000000000000FF00
      000000000000FF00000000000000FF000000FF000000FF00000000000000FF00
      0000FF000000FF000000FF000000000000000000000000840000000000000084
      0000008400000000000000840000000000000084000000840000008400000000
      000000840000000000000084000000000000000000000000FF00000000000000
      0000000000000000FF000000000000000000000000000000FF00000000000000
      FF000000FF000000FF000000FF0000000000000000000000FF00000000000000
      0000000000000000FF000000000000000000000000000000FF00000000000000
      FF0000000000000000000000FF000000000000000000FF000000000000000000
      0000FF000000FF000000000000000000000000000000FF000000FF0000000000
      000000000000FF000000FF000000000000000000000000840000008400000084
      0000008400000084000000840000008400000084000000840000008400000084
      000000840000008400000084000000000000000000000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF0000000000000000000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000000000000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      28000000400000001C0000000100010000000000E00000000000000000000000
      000000000000000000000000FFFFFF00AAABFFFF00000000AAABFFFF00000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000AAABAAAB00000000AAABAAAB00000000
      AAABAAABFFFFAAABAAABAAABFFFFAAAB00000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      AAABAAABAAABAAABAAABAAABAAABAAAB00000000000000000000000000000000
      000000000000}
  end
  object AL_Main: TActionList
    Images = IL_Menu
    Left = 920
    Top = 456
    object A_MTB_Go: TAction
      Enabled = False
      Hint = 'P'#345'ipojit se k MTB'
      ImageIndex = 4
      ShortCut = 117
      OnExecute = A_MTB_GoExecute
    end
    object A_MTB_Stop: TAction
      Enabled = False
      Hint = 'Odpojit se od MTB'
      ImageIndex = 5
      ShortCut = 118
      OnExecute = A_MTB_StopExecute
    end
    object A_lib_cfg: TAction
      Hint = 'Otev'#345#237't konfiguraci knihovny'
      ImageIndex = 18
      ShortCut = 16452
      OnExecute = A_lib_cfgExecute
    end
    object A_DCC_Go: TAction
      Enabled = False
      Hint = 'Zapnout DCC'
      ImageIndex = 6
      OnExecute = A_DCC_GoExecute
    end
    object A_DCC_Stop: TAction
      Enabled = False
      Hint = 'Vypnout DCC'
      ImageIndex = 7
      OnExecute = A_DCC_StopExecute
    end
    object A_System_Start: TAction
      Hint = 'Central start'
      ImageIndex = 2
      ShortCut = 16455
      OnExecute = A_System_StartExecute
    end
    object A_System_Stop: TAction
      Enabled = False
      Hint = 'Central stop'
      ImageIndex = 3
      ShortCut = 16467
      OnExecute = A_System_StopExecute
    end
    object A_Trk_Connect: TAction
      Caption = 'P'#345'ipojit se'
      ImageIndex = 11
      OnExecute = A_Trk_ConnectExecute
    end
    object A_Trk_Disconnect: TAction
      Caption = 'Odpojit se'
      Enabled = False
      ImageIndex = 10
      OnExecute = A_Trk_DisconnectExecute
    end
    object A_All_Loko_Prevzit: TAction
      Caption = 'P'#345'evz'#237't hnac'#237' vozidla'
      Enabled = False
      ImageIndex = 45
      OnExecute = A_All_Loko_PrevzitExecute
    end
    object A_All_Loko_Odhlasit: TAction
      Caption = 'Odhl'#225'sit hnac'#237' vozidla'
      Enabled = False
      ImageIndex = 44
      OnExecute = A_All_Loko_OdhlasitExecute
    end
    object A_PanelServer_Start: TAction
      Caption = 'Start'
      ImageIndex = 51
      ShortCut = 16496
      OnExecute = A_PanelServer_StartExecute
    end
    object A_PanelServer_Stop: TAction
      Caption = 'Stop'
      Enabled = False
      ImageIndex = 52
      ShortCut = 16497
      OnExecute = A_PanelServer_StopExecute
    end
    object A_MTB_Open: TAction
      Caption = 'Otev'#345#237't MTB'
      ImageIndex = 2
      OnExecute = A_MTB_OpenExecute
    end
    object A_MTB_Close: TAction
      Caption = 'Zav'#345#237't MTB'
      Enabled = False
      ImageIndex = 3
      OnExecute = A_MTB_CloseExecute
    end
    object A_SaveStav: TAction
      Caption = 'Ulo'#382'it stav koleji'#353't'#283
      OnExecute = A_SaveStavExecute
    end
    object A_FuncsSet: TAction
      Caption = 'Nastavit funkci dle v'#253'znamu'
      Enabled = False
      OnExecute = A_FuncsSetExecute
    end
  end
  object IL_Ostatni: TImageList
    BkColor = clBlack
    Height = 12
    Width = 8
    Left = 1224
    Top = 456
    Bitmap = {
      494C010164006001A00208000C0000000000FF10FFFFFFFFFFFFFFFF424D3600
      000000000000360000002800000020000000380100000100200000000000009C
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FF00
      0000FF00000000000000000000000000000000000000000000000000000000FF
      FF0000FFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FF00
      0000FF00000000000000000000000000000000000000000000000000000000FF
      FF0000FFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FF00
      0000FF000000FF0000000000000000000000000000000000000000FFFF0000FF
      FF0000FFFF0000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FF00
      0000FF000000FF0000000000000000000000000000000000000000FFFF0000FF
      FF0000FFFF0000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000FF000000FF00
      0000FF000000FF000000FF000000000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000FF000000FF00
      0000FF000000FF000000FF000000000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF00000000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF00000000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF000000FF0000000000000000000000000000000000000000000000000000FF
      000000FF0000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00000000000000000000000000000000000000000000000000FFFF
      0000FFFF00000000000000000000000000000000000000000000000000000000
      FF000000FF0000000000000000000000000000000000000000000000000000FF
      000000FF0000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00000000000000000000000000000000000000000000000000FFFF
      0000FFFF000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000000000000000000000000000000000FF000000FF
      000000FF000000FF000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000FFFF0000FFFF
      0000FFFF0000FFFF0000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000000000000000000000000000000000FF000000FF
      000000FF000000FF000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000FFFF0000FFFF
      0000FFFF0000FFFF00000000000000000000000000000000FF000000FF000000
      FF000000FF000000FF000000FF00000000000000000000FF000000FF000000FF
      000000FF000000FF000000FF00000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF000000000000000000000000FF000000FF000000
      FF000000FF000000FF000000FF00000000000000000000FF000000FF000000FF
      000000FF000000FF000000FF00000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000000000000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF0000FF000000FF000000FF000000FF
      000000FF000000FF000000FF000000FF0000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF0000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF00000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF0000FF000000FF000000FF000000FF
      000000FF000000FF000000FF000000FF0000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF0000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF00000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FF00
      FF00FF00FF000000000000000000000000000000000000000000000000008484
      8400848484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FF00
      FF00FF00FF000000000000000000000000000000000000000000000000008484
      8400848484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF00FF00FF00
      FF00FF00FF00FF00FF0000000000000000000000000000000000848484008484
      8400848484008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF00FF00FF00
      FF00FF00FF00FF00FF0000000000000000000000000000000000848484008484
      8400848484008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00000000000000000084848400848484008484
      8400848484008484840084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00000000000000000084848400848484008484
      8400848484008484840084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF008484840084848400848484008484
      8400848484008484840084848400848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF008484840084848400848484008484
      8400848484008484840084848400848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF0000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF00000000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF0000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF00000000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF0000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF00000000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF0000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF00000000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF0000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF00000000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF0000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF00000000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF0000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF00000000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF0000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF00000000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF008484840084848400848484008484
      8400848484008484840084848400848484000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF0000FF000000FF000000FF000000FF
      000000FF000000FF000000FF000000FF0000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF008484840084848400848484008484
      8400848484008484840084848400848484000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF0000FF000000FF000000FF000000FF
      000000FF000000FF000000FF000000FF0000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF008484840084848400848484008484
      8400848484008484840084848400848484000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF0000FF000000FF000000FF000000FF
      000000FF000000FF000000FF000000FF0000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF008484840084848400848484008484
      8400848484008484840084848400848484000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF0000FF000000FF000000FF000000FF
      000000FF000000FF000000FF000000FF0000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF008484840084848400848484008484
      8400848484008484840084848400848484000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF0000FF000000FF000000FF000000FF
      000000FF000000FF000000FF000000FF0000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF008484840084848400848484008484
      8400848484008484840084848400848484000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF0000FF000000FF000000FF000000FF
      000000FF000000FF000000FF000000FF0000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF008484840084848400848484008484
      8400848484008484840084848400848484000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF0000FF000000FF000000FF000000FF
      000000FF000000FF000000FF000000FF0000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF008484840084848400848484008484
      8400848484008484840084848400848484000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF0000FF000000FF000000FF000000FF
      000000FF000000FF000000FF000000FF00000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FF00
      0000FF00000000000000000000000000000000000000000000000000000000FF
      FF0000FFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FF00
      0000FF000000FF0000000000000000000000000000000000000000FFFF0000FF
      FF0000FFFF0000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000FF000000FF00
      0000FF000000FF000000FF000000000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000FF000000FF00
      0000FF000000FF000000FF000000000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF00000000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF00000000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF00000000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF00000000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000FF000000FF00
      0000FF000000FF000000FF000000000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000FF000000FF00
      0000FF000000FF000000FF000000000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FF00
      0000FF000000FF0000000000000000000000000000000000000000FFFF0000FF
      FF0000FFFF0000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FF00
      0000FF00000000000000000000000000000000000000000000000000000000FF
      FF0000FFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF000000FF0000000000000000000000000000000000000000000000000000FF
      000000FF0000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00000000000000000000000000000000000000000000000000FFFF
      0000FFFF000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000000000000000000000000000000000FF000000FF
      000000FF000000FF000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000FFFF0000FFFF
      0000FFFF0000FFFF00000000000000000000000000000000FF000000FF000000
      FF000000FF000000FF000000FF00000000000000000000FF000000FF000000FF
      000000FF000000FF000000FF00000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF000000000000000000000000FF000000FF000000
      FF000000FF000000FF000000FF00000000000000000000FF000000FF000000FF
      000000FF000000FF000000FF00000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000000000000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF0000FF000000FF000000FF000000FF
      000000FF000000FF000000FF000000FF0000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF0000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF00000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF0000FF000000FF000000FF000000FF
      000000FF000000FF000000FF000000FF0000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF0000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF00000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF0000FF000000FF000000FF000000FF
      000000FF000000FF000000FF000000FF0000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF0000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF00000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF0000FF000000FF000000FF000000FF
      000000FF000000FF000000FF000000FF0000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF0000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000000000000000FF000000FF000000
      FF000000FF000000FF000000FF00000000000000000000FF000000FF000000FF
      000000FF000000FF000000FF00000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF000000000000000000000000FF000000FF000000
      FF000000FF000000FF000000FF00000000000000000000FF000000FF000000FF
      000000FF000000FF000000FF00000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF00000000000000000000000000000000FF000000
      FF000000FF000000FF000000000000000000000000000000000000FF000000FF
      000000FF000000FF000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000FFFF0000FFFF
      0000FFFF0000FFFF000000000000000000000000000000000000000000000000
      FF000000FF0000000000000000000000000000000000000000000000000000FF
      000000FF0000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00000000000000000000000000000000000000000000000000FFFF
      0000FFFF00000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FF00
      FF00FF00FF000000000000000000000000000000000000000000000000008484
      8400848484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF00FF00FF00
      FF00FF00FF00FF00FF0000000000000000000000000000000000848484008484
      8400848484008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00000000000000000084848400848484008484
      8400848484008484840084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00000000000000000084848400848484008484
      8400848484008484840084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF008484840084848400848484008484
      8400848484008484840084848400848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF008484840084848400848484008484
      8400848484008484840084848400848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF008484840084848400848484008484
      8400848484008484840084848400848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF008484840084848400848484008484
      8400848484008484840084848400848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00000000000000000084848400848484008484
      8400848484008484840084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00000000000000000084848400848484008484
      8400848484008484840084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF00FF00FF00
      FF00FF00FF00FF00FF0000000000000000000000000000000000848484008484
      8400848484008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FF00
      FF00FF00FF000000000000000000000000000000000000000000000000008484
      8400848484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF0000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF00000000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF0000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF00000000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF0000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF00000000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF0000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF00000000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF0000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF00000000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF0000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF00000000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF008484840084848400848484008484
      8400848484008484840084848400848484000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF0000FF000000FF000000FF000000FF
      000000FF000000FF000000FF000000FF0000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF008484840084848400848484008484
      8400848484008484840084848400848484000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF0000FF000000FF000000FF000000FF
      000000FF000000FF000000FF000000FF0000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF008484840084848400848484008484
      8400848484008484840084848400848484000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF0000FF000000FF000000FF000000FF
      000000FF000000FF000000FF000000FF0000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF008484840084848400848484008484
      8400848484008484840084848400848484000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF0000FF000000FF000000FF000000FF
      000000FF000000FF000000FF000000FF0000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF008484840084848400848484008484
      8400848484008484840084848400848484000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF0000FF000000FF000000FF000000FF
      000000FF000000FF000000FF000000FF0000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF008484840084848400848484008484
      8400848484008484840084848400848484000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF0000FF000000FF000000FF000000FF
      000000FF000000FF000000FF000000FF00000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084000000000000000000000000000000
      0000000000000000000000000000000000008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084000000000000000000000000000000
      0000000000000000000000000000000000008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084000000000000000000000000000000
      0000000000000000000000000000000000008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084000000000000000000000000000000
      0000000000000000000000000000000000008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084000000000000000000000000000000
      0000000000000000000000000000000000008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084000000000000000000000000000000
      0000000000000000000000000000000000008400840084008400840084008400
      8400840084008400840084008400840084000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00000000000000000000000000000000000000000000000000FFFF
      0000FFFF0000000000000000000000000000000000000000000000000000FF00
      0000FF00000000000000000000000000000000000000000000000000000000FF
      FF0000FFFF0000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF00000000000000000000FF000000FF000000FF00
      0000FF000000FF000000FF000000000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF00000000000000000000FF000000FF000000FF00
      0000FF000000FF000000FF000000000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF0000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF00000000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF0000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF00000000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF0000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF00000000000000000000FF000000FF000000FF00
      0000FF000000FF000000FF000000000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF00000000000000000000FF000000FF000000FF00
      0000FF000000FF000000FF000000000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000000000000000000000000000000000FFFF
      FF00FFFFFF00000000000000000000000000000000000000000000000000FFFF
      0000FFFF0000000000000000000000000000000000000000000000000000FF00
      0000FF00000000000000000000000000000000000000000000000000000000FF
      FF0000FFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FF00
      FF00FF00FF000000000000000000000000000000000000000000000000008484
      8400848484000000000000000000000000000000000000000000000000000000
      FF000000FF0000000000000000000000000000000000000000000000000000FF
      000000FF000000000000000000000000000000000000FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00000000000000000084848400848484008484
      840084848400848484008484840000000000000000000000FF000000FF000000
      FF000000FF000000FF000000FF00000000000000000000FF000000FF000000FF
      000000FF000000FF000000FF00000000000000000000FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00000000000000000084848400848484008484
      840084848400848484008484840000000000000000000000FF000000FF000000
      FF000000FF000000FF000000FF00000000000000000000FF000000FF000000FF
      000000FF000000FF000000FF000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF008484840084848400848484008484
      8400848484008484840084848400848484000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF0000FF000000FF000000FF000000FF
      000000FF000000FF000000FF000000FF0000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF008484840084848400848484008484
      8400848484008484840084848400848484000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF0000FF000000FF000000FF000000FF
      000000FF000000FF000000FF000000FF000000000000FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00000000000000000084848400848484008484
      840084848400848484008484840000000000000000000000FF000000FF000000
      FF000000FF000000FF000000FF00000000000000000000FF000000FF000000FF
      000000FF000000FF000000FF00000000000000000000FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00000000000000000084848400848484008484
      840084848400848484008484840000000000000000000000FF000000FF000000
      FF000000FF000000FF000000FF00000000000000000000FF000000FF000000FF
      000000FF000000FF000000FF000000000000000000000000000000000000FF00
      FF00FF00FF000000000000000000000000000000000000000000000000008484
      8400848484000000000000000000000000000000000000000000000000000000
      FF000000FF0000000000000000000000000000000000000000000000000000FF
      000000FF00000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400840000000000000000000000
      0000000000000000000000000000840084008400840000000000000000000000
      0000000000000000000000000000840084000000000000000000000000000000
      0000000000000000000000000000000000008400840000000000000000000000
      0000000000000000000000000000840084008400840084008400000000000000
      0000000000000000000084008400840084008400840084008400000000000000
      0000000000000000000084008400840084000000000000000000000000000000
      0000000000000000000000000000000000008400840084008400000000000000
      0000000000000000000084008400840084008400840084008400840084000000
      0000000000008400840084008400840084008400840084008400840084000000
      0000000000008400840084008400840084000000000000000000000000000000
      0000000000000000000000000000000000008400840084008400840084000000
      0000000000008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084000000000000000000000000000000
      0000000000000000000000000000000000008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084000000000000000000000000000000
      0000000000000000000000000000000000008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084000000000000000000000000000000
      0000000000000000000000000000000000008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084000000000000000000000000000000
      0000000000000000000000000000000000008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084000000000000000000000000000000
      0000000000000000000000000000000000008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084000000000000000000000000000000
      0000000000000000000000000000000000008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084000000000000000000000000000000
      0000000000000000000000000000000000008400840084008400840084008400
      8400840084008400840084008400840084000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400840000000000000000000000
      0000000000000000000000000000840084008400840000000000000000000000
      0000000000000000000000000000840084008400840000000000000000000000
      0000000000000000000000000000840084008400840000000000000000000000
      0000000000000000000000000000840084008400840084008400000000000000
      0000000000000000000084008400840084008400840084008400000000000000
      0000000000000000000084008400840084008400840084008400000000000000
      0000000000000000000084008400840084008400840084008400000000000000
      0000000000000000000084008400840084008400840084008400840084000000
      0000000000008400840084008400840084008400840084008400840084000000
      0000000000008400840084008400840084008400840084008400840084000000
      0000000000008400840084008400840084008400840084008400840084000000
      0000000000008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400840000000000000000000000
      0000000000000000000000000000840084008400840000000000000000000000
      0000000000000000000000000000840084000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400840084008400000000000000
      0000000000000000000084008400840084008400840084008400000000000000
      0000000000000000000084008400840084000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400840084008400840084000000
      0000000000008400840084008400840084008400840084008400840084000000
      0000000000008400840084008400840084000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400840084008400
      8400840084008400840084008400840084000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00000000000000000000000000000000000000000000000000FFFF
      0000FFFF0000000000000000000000000000000000000000000000000000FF00
      0000FF00000000000000000000000000000000000000000000000000000000FF
      FF0000FFFF000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000FFFF0000FFFF
      0000FFFF0000FFFF000000000000000000000000000000000000FF000000FF00
      0000FF000000FF0000000000000000000000000000000000000000FFFF0000FF
      FF0000FFFF0000FFFF00000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF00000000000000000000FF000000FF000000FF00
      0000FF000000FF000000FF000000000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF0000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF00000000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF0000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF00000000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FF00
      FF00FF00FF000000000000000000000000000000000000000000000000008484
      8400848484000000000000000000000000000000000000000000000000000000
      FF000000FF0000000000000000000000000000000000000000000000000000FF
      000000FF00000000000000000000000000000000000000000000FF00FF00FF00
      FF00FF00FF00FF00FF0000000000000000000000000000000000848484008484
      84008484840084848400000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000000000000000000000000000000000FF000000FF
      000000FF000000FF0000000000000000000000000000FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00000000000000000084848400848484008484
      840084848400848484008484840000000000000000000000FF000000FF000000
      FF000000FF000000FF000000FF00000000000000000000FF000000FF000000FF
      000000FF000000FF000000FF000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF008484840084848400848484008484
      8400848484008484840084848400848484000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF0000FF000000FF000000FF000000FF
      000000FF000000FF000000FF000000FF0000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF008484840084848400848484008484
      8400848484008484840084848400848484000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF0000FF000000FF000000FF000000FF
      000000FF000000FF000000FF000000FF00000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400840000000000000000000000
      0000000000000000000000000000840084008400840000000000000000000000
      0000000000000000000000000000840084000000000000000000000000000000
      0000000000000000000000000000000000008400840000000000000000000000
      0000000000000000000000000000840084008400840000000000000000000000
      0000000000000000000000000000840084008400840000000000000000000000
      0000000000000000000000000000840084000000000000000000000000000000
      0000000000000000000000000000000000008400840000000000000000000000
      0000000000000000000000000000840084008400840000000000000000000000
      0000000000000000000000000000840084008400840000000000000000000000
      0000000000000000000000000000840084000000000000000000000000000000
      0000000000000000000000000000000000008400840000000000000000000000
      0000000000000000000000000000840084008400840084008400000000000000
      0000000000000000000084008400840084008400840084008400000000000000
      0000000000000000000084008400840084000000000000000000000000000000
      0000000000000000000000000000000000008400840084008400000000000000
      0000000000000000000084008400840084008400840084008400000000000000
      0000000000000000000084008400840084008400840084008400000000000000
      0000000000000000000084008400840084000000000000000000000000000000
      0000000000000000000000000000000000008400840084008400000000000000
      0000000000000000000084008400840084008400840084008400000000000000
      0000000000000000000084008400840084008400840084008400000000000000
      0000000000000000000084008400840084000000000000000000000000000000
      0000000000000000000000000000000000008400840084008400000000000000
      0000000000000000000084008400840084008400840084008400840084000000
      0000000000008400840084008400840084008400840084008400840084000000
      0000000000008400840084008400840084000000000000000000000000000000
      0000000000000000000000000000000000008400840084008400840084000000
      0000000000008400840084008400840084008400840084008400840084000000
      0000000000008400840084008400840084008400840084008400840084000000
      0000000000008400840084008400840084000000000000000000000000000000
      0000000000000000000000000000000000008400840084008400840084000000
      0000000000008400840084008400840084008400840084008400840084000000
      0000000000008400840084008400840084008400840084008400840084000000
      0000000000008400840084008400840084000000000000000000000000000000
      0000000000000000000000000000000000008400840084008400840084000000
      0000000000008400840084008400840084000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400840000000000000000000000
      0000000000000000000000000000840084008400840000000000000000000000
      0000000000000000000000000000840084008400840000000000000000000000
      0000000000000000000000000000840084008400840000000000000000000000
      0000000000000000000000000000840084008400840000000000000000000000
      0000000000000000000000000000840084008400840000000000000000000000
      0000000000000000000000000000840084008400840000000000000000000000
      0000000000000000000000000000840084008400840000000000000000000000
      0000000000000000000000000000840084008400840000000000000000000000
      0000000000000000000000000000840084008400840000000000000000000000
      0000000000000000000000000000840084008400840000000000000000000000
      0000000000000000000000000000840084008400840000000000000000000000
      0000000000000000000000000000840084008400840084008400000000000000
      0000000000000000000084008400840084008400840084008400000000000000
      0000000000000000000084008400840084008400840084008400000000000000
      0000000000000000000084008400840084008400840084008400000000000000
      0000000000000000000084008400840084008400840084008400000000000000
      0000000000000000000084008400840084008400840084008400000000000000
      0000000000000000000084008400840084008400840084008400000000000000
      0000000000000000000084008400840084008400840084008400000000000000
      0000000000000000000084008400840084008400840084008400000000000000
      0000000000000000000084008400840084008400840084008400000000000000
      0000000000000000000084008400840084008400840084008400000000000000
      0000000000000000000084008400840084008400840084008400000000000000
      0000000000000000000084008400840084008400840084008400840084000000
      0000000000008400840084008400840084008400840084008400840084000000
      0000000000008400840084008400840084008400840084008400840084000000
      0000000000008400840084008400840084008400840084008400840084000000
      0000000000008400840084008400840084008400840084008400840084000000
      0000000000008400840084008400840084008400840084008400840084000000
      0000000000008400840084008400840084008400840084008400840084000000
      0000000000008400840084008400840084008400840084008400840084000000
      0000000000008400840084008400840084008400840084008400840084000000
      0000000000008400840084008400840084008400840084008400840084000000
      0000000000008400840084008400840084008400840084008400840084000000
      0000000000008400840084008400840084008400840084008400840084000000
      0000000000008400840084008400840084000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400840000000000000000000000
      0000000000000000000000000000840084008400840000000000000000000000
      0000000000000000000000000000840084000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400840000000000000000000000
      0000000000000000000000000000840084008400840000000000000000000000
      0000000000000000000000000000840084000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400840000000000000000000000
      0000000000000000000000000000840084008400840000000000000000000000
      0000000000000000000000000000840084000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400840084008400000000000000
      0000000000000000000084008400840084008400840084008400000000000000
      0000000000000000000084008400840084000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400840084008400000000000000
      0000000000000000000084008400840084008400840084008400000000000000
      0000000000000000000084008400840084000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400840084008400000000000000
      0000000000000000000084008400840084008400840084008400000000000000
      0000000000000000000084008400840084000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400840084008400840084000000
      0000000000008400840084008400840084008400840084008400840084000000
      0000000000008400840084008400840084000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400840084008400840084000000
      0000000000008400840084008400840084008400840084008400840084000000
      0000000000008400840084008400840084000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400840084008400840084000000
      0000000000008400840084008400840084008400840084008400840084000000
      000000000000840084008400840084008400000000000000000000000000FFFF
      FF00FFFFFF00000000000000000000000000000000000000000000000000FFFF
      0000FFFF0000000000000000000000000000000000000000000000000000FF00
      0000FF00000000000000000000000000000000000000000000000000000000FF
      FF0000FFFF00000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00000000000000000000000000000000000000000000000000FFFF
      0000FFFF0000000000000000000000000000000000000000000000000000FF00
      0000FF00000000000000000000000000000000000000000000000000000000FF
      FF0000FFFF00000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00000000000000000000000000000000000000000000000000FFFF
      0000FFFF0000000000000000000000000000000000000000000000000000FF00
      0000FF00000000000000000000000000000000000000000000000000000000FF
      FF0000FFFF000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000FFFF0000FFFF
      0000FFFF0000FFFF000000000000000000000000000000000000FF000000FF00
      0000FF000000FF0000000000000000000000000000000000000000FFFF0000FF
      FF0000FFFF0000FFFF0000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000FFFF0000FFFF
      0000FFFF0000FFFF000000000000000000000000000000000000FF000000FF00
      0000FF000000FF0000000000000000000000000000000000000000FFFF0000FF
      FF0000FFFF0000FFFF0000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000FFFF0000FFFF
      0000FFFF0000FFFF000000000000000000000000000000000000FF000000FF00
      0000FF000000FF0000000000000000000000000000000000000000FFFF0000FF
      FF0000FFFF0000FFFF00000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF00000000000000000000FF000000FF000000FF00
      0000FF000000FF000000FF000000000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF00000000000000000000FF000000FF000000FF00
      0000FF000000FF000000FF000000000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF00000000000000000000FF000000FF000000FF00
      0000FF000000FF000000FF000000000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF0000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF00000000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF0000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF00000000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF0000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF00000000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF00000000000000000000000000FF00
      FF00FF00FF000000000000000000000000000000000000000000000000008484
      8400848484000000000000000000000000000000000000000000000000000000
      FF000000FF0000000000000000000000000000000000000000000000000000FF
      000000FF0000000000000000000000000000000000000000000000000000FF00
      FF00FF00FF000000000000000000000000000000000000000000000000008484
      8400848484000000000000000000000000000000000000000000000000000000
      FF000000FF0000000000000000000000000000000000000000000000000000FF
      000000FF0000000000000000000000000000000000000000000000000000FF00
      FF00FF00FF000000000000000000000000000000000000000000000000008484
      8400848484000000000000000000000000000000000000000000000000000000
      FF000000FF0000000000000000000000000000000000000000000000000000FF
      000000FF00000000000000000000000000000000000000000000FF00FF00FF00
      FF00FF00FF00FF00FF0000000000000000000000000000000000848484008484
      84008484840084848400000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000000000000000000000000000000000FF000000FF
      000000FF000000FF000000000000000000000000000000000000FF00FF00FF00
      FF00FF00FF00FF00FF0000000000000000000000000000000000848484008484
      84008484840084848400000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000000000000000000000000000000000FF000000FF
      000000FF000000FF000000000000000000000000000000000000FF00FF00FF00
      FF00FF00FF00FF00FF0000000000000000000000000000000000848484008484
      84008484840084848400000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000000000000000000000000000000000FF000000FF
      000000FF000000FF0000000000000000000000000000FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00000000000000000084848400848484008484
      840084848400848484008484840000000000000000000000FF000000FF000000
      FF000000FF000000FF000000FF00000000000000000000FF000000FF000000FF
      000000FF000000FF000000FF00000000000000000000FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00000000000000000084848400848484008484
      840084848400848484008484840000000000000000000000FF000000FF000000
      FF000000FF000000FF000000FF00000000000000000000FF000000FF000000FF
      000000FF000000FF000000FF00000000000000000000FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00000000000000000084848400848484008484
      840084848400848484008484840000000000000000000000FF000000FF000000
      FF000000FF000000FF000000FF00000000000000000000FF000000FF000000FF
      000000FF000000FF000000FF000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF008484840084848400848484008484
      8400848484008484840084848400848484000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF0000FF000000FF000000FF000000FF
      000000FF000000FF000000FF000000FF0000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF008484840084848400848484008484
      8400848484008484840084848400848484000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF0000FF000000FF000000FF000000FF
      000000FF000000FF000000FF000000FF0000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF008484840084848400848484008484
      8400848484008484840084848400848484000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF0000FF000000FF000000FF000000FF
      000000FF000000FF000000FF000000FF0000424D3E000000000000003E000000
      2800000020000000380100000100010000000000E00400000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FFFFFFFFFFFFFFFFE7E7FFE7E7E7FFE7C3C3FFC3C3C3FFC38181FF818181FF81
      0000FF000000FF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE7E7E7E7E7E7E7E7
      C3C3C3C3C3C3C3C381818181818181810000000000000000FFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFF00E7E7FF00E7E7FF00C3C3FF00C3C3FF008181FF008181
      FF000000FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000000000000000
      000000000000000000000000000000000000000000000000FFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFF000000000000000000000000000000000000000000000000
      0000000000000000FFFFFFFFFFFFFFFFE7E7FFE7C3C3FFC38181FF818181FF81
      0000FF000000FF000000FF000000FF008181FF818181FF81C3C3FFC3E7E7FFE7
      E7E7E7E7C3C3C3C3818181818181818100000000000000000000000000000000
      8181818181818181C3C3C3C3E7E7E7E7FFFFE7E7FFFFC3C3FFFF8181FFFF8181
      FFFF0000FFFF0000FF000000FF000000FF008181FF008181FF00C3C3FF00E7E7
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000000000000000
      00000000000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFF000000000000000000000000000000000000000000000000
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FF000000FF00
      0000FF000000FF000000FF000000FF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFF000000000000000000000000000000000000000000000000
      FFFFFFFFFFFFFFFFFFE7FFFFFF81FFFFFF81FFFFFF00FFFFFF000000FF810000
      FF810000FFE70000FFFF0000FFFF0000FFFFFFFFFFFFFFFFE7E7E7E781818181
      8181818100000000000000008181818181818181E7E7E7E7FFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFE7E7E7E78181818181818181000000000000000081818181
      81818181E7E7E7E7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7E7EFF7E3C3CFF3C
      1818FF180000FF000000FF000000FF000000FF000000FF000000FF000000FF00
      FFFFFFFFFFFFFFFF7E7E7E7E3C3C3C3C18181818000000000000000000000000
      00000000000000000000000000000000FFFFFFFFFFFFFFFFFFFF7E7EFFFF3C3C
      FFFF1818FFFF0000FFFF0000FFE70000FFC30000FF810000FF000000FF000000
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE7E7E7E7
      C3C3C3C3818181810000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFE7E7E7E7C3C3C3C3818181810000000000000000
      FFFFFFFFFFFFFFFFFFFFFFFF7E7EFF7E7E7EFF7E7E7EFF7E3C3CFF3C3C3CFF3C
      3C3CFF3C1818FF181818FF181818FF18FFFFFFFFFFFFFFFFFFFFFFFF7E7E7E7E
      7E7E7E7E7E7E7E7E3C3C3C3C3C3C3C3C3C3C3C3C181818181818181818181818
      FFE7FFFFFFE7FFFFFFE7FFFFFFC37E7EFFC37E7EFFC37E7EFF813C3CFF813C3C
      FF813C3CFF001818FF001818FF001818E7E7E7E7E7E7E7E7E7E7E7E7C3C3C3C3
      C3C3C3C3C3C3C3C3818181818181818181818181000000000000000000000000
      E7E7E7E7E7E7E7E7E7E7E7E7C3C3C3C3C3C3C3C3C3C3C3C38181818181818181
      8181818100000000000000000000000000000000000000000000000000000000
      000000000000}
  end
  object PM_HV: TPopupMenu
    OnPopup = PM_HVPopup
    Left = 336
    Top = 456
    object PM_Regulator: TMenuItem
      Caption = 'Regul'#225'tor'
      OnClick = PM_RegulatorClick
    end
    object N6: TMenuItem
      Caption = '-'
    end
    object PM_Properties: TMenuItem
      Caption = 'Vlastnosti'
      OnClick = PM_PropertiesClick
    end
  end
  object PM_Bloky: TPopupMenu
    OnPopup = PM_BlokyPopup
    Left = 280
    Top = 456
    object MI_TechProp: TMenuItem
      Caption = 'Technologick'#233' vlastnosti'
      OnClick = MI_TechPropClick
    end
    object MenuItem2: TMenuItem
      Caption = '-'
    end
    object MI_Prop: TMenuItem
      Caption = 'Vlastnosti'
      OnClick = MI_PropClick
    end
  end
  object PM_Clients: TPopupMenu
    OnPopup = PM_ClientsPopup
    Left = 408
    Top = 456
    object MI_Disconnect: TMenuItem
      Caption = 'Odpojit'
      OnClick = MI_DisconnectClick
    end
  end
end
