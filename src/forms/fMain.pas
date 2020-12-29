unit fMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Menus, ImgList, Buttons, ComCtrls, Trakce,
  inifiles, ActnList, AppEvnts, adCpuUsage, cpuLoad, ExtDlgs, Gauges, StrUtils,
  ComObj, TechnologieTrakce, BoosterDb;

const
 _SB_LOG = 0;
 _SB_RCS = 1;
 _SB_TRAKCE_STAV = 2;
 _SB_TRAKCE_LIB = 3;
 _SB_SBERNICE = 4;
 _SB_PROC = 5;

 _INIDATA_FN = 'inidata.ini';

 _TABLE_COLOR_GREEN = $E0FFE0;
 _TABLE_COLOR_GRAY = $DDDDDD;
 _TABLE_COLOR_RED = $E0E0FF;
 _TABLE_COLOR_YELLOW = $A0FFFF;
 _TABLE_COLOR_BLUE = $FFE0E0;
 _TABLE_COLOR_WHITE = $FFFFFF;
 _TABLE_COLOR_PINKY = $E2B6FF;

 _LV_CLIENTS_COL_STATE = 0;
 _LV_CLIENTS_COL_CLIENT = 1;
 _LV_CLIENTS_COL_PING = 2;
 _LV_CLIENTS_COL_OR1 = 3;
 _LV_CLIENTS_COL_OR2 = 4;
 _LV_CLIENTS_COL_OR3 = 5;
 _LV_CLIENTS_COL_OR_NEXT = 6;
 _LV_CLIENTS_COL_MENU = 7;
 _LV_CLIENTS_COL_STIT = 8;
 _LV_CLIENTS_COL_RIZ = 9;
 _LV_CLIENTS_COL_PROTOCOL = 10;
 _LV_CLIENTS_COL_REGULATOR = 11;
 _LV_CLIENTS_COL_SH = 12;

type

  TF_Main = class(TForm)
    T_Main: TTimer;
    Menu_1: TMainMenu;
    MI_RCS: TMenuItem;
    MI_RCS_Go: TMenuItem;
    MI_RCS_Stop: TMenuItem;
    MI_RCS_Options: TMenuItem;
    MI_Provoz: TMenuItem;
    PM_Nastaveni: TMenuItem;
    PM_ResetV: TMenuItem;
    SB1: TStatusBar;
    N1: TMenuItem;
    PM_Tester: TMenuItem;
    PM_TI: TPopupMenu;
    PM_Open: TMenuItem;
    PM_close: TMenuItem;
    N3: TMenuItem;
    PM_icon_close: TMenuItem;
    M_Help: TMenuItem;
    PM_Help_RP: TMenuItem;
    MI_Centrala: TMenuItem;
    MI_DCC_on: TMenuItem;
    MI_DCC_Off: TMenuItem;
    MI_Trk_Disconnect: TMenuItem;
    N4: TMenuItem;
    MI_Trk_connect: TMenuItem;
    M_Zobrazeni: TMenuItem;
    PM_SB1: TMenuItem;
    T_GUI_refresh: TTimer;
    MI_System: TMenuItem;
    PM_Central_Start: TMenuItem;
    PM_Central_Stop: TMenuItem;
    N5: TMenuItem;
    T_clear_log_msg: TTimer;
    P_Pozadi: TPanel;
    P_Date: TPanel;
    P_Time: TPanel;
    P_Time_modelovy: TPanel;
    P_Zrychleni: TPanel;
    IL_Menu: TImageList;
    SPD_Save: TSavePictureDialog;
    MI_Loco_Acquire: TMenuItem;
    MI_Loco_Release: TMenuItem;
    P_DCC: TPanel;
    SB_Loconet_Start: TSpeedButton;
    SB_Loconet_Stop: TSpeedButton;
    P_SystemSet: TPanel;
    SB_SystemStart: TSpeedButton;
    SB_SystemStop: TSpeedButton;
    MI_RCS_Libs: TMenuItem;
    N2: TMenuItem;
    PM_SaveFormPos: TMenuItem;
    IL_Bloky: TImageList;
    IL_RCS: TImageList;
    N7: TMenuItem;
    PM_Console: TMenuItem;
    AL_Main: TActionList;
    A_RCS_Go: TAction;
    A_RCS_Stop: TAction;
    A_RCS_lib_cfg: TAction;
    A_DCC_Go: TAction;
    A_DCC_Stop: TAction;
    A_System_Start: TAction;
    A_System_Stop: TAction;
    A_Trk_Connect: TAction;
    A_Trk_Disconnect: TAction;
    A_Locos_Acquire: TAction;
    A_Locos_Release: TAction;
    MI_PanelServer: TMenuItem;
    A_PanelServer_Start: TAction;
    A_PanelServer_Stop: TAction;
    Start1: TMenuItem;
    Stop1: TMenuItem;
    A_RCS_Open: TAction;
    A_RCS_Close: TAction;
    MI_RCS_Open: TMenuItem;
    MI_RCS_Close: TMenuItem;
    N8: TMenuItem;
    PC_1: TPageControl;
    TS_Technologie: TTabSheet;
    TS_Bloky: TTabSheet;
    LV_Bloky: TListView;
    P_BlkPozadi: TPanel;
    P_Blk_Ostatni: TPanel;
    L_BlkPocet: TLabel;
    P_Blk_Left: TPanel;
    E_dataload_block: TEdit;
    TS_HV: TTabSheet;
    LV_HV: TListView;
    P_HV_Pozadi: TPanel;
    P_HV_Left: TPanel;
    E_dataload_HV_dir: TEdit;
    TS_Soupravy: TTabSheet;
    LV_Soupravy: TListView;
    P_Soupravy_pozadi: TPanel;
    P_Spr_Left: TPanel;
    E_dataload_soupr: TEdit;
    TS_Stanice: TTabSheet;
    LV_Stanice: TListView;
    P_Stanice_Pozadi: TPanel;
    P_St_Left: TPanel;
    E_dataload_spnl: TEdit;
    TS_Zesilovace: TTabSheet;
    LV_Zesilovace: TListView;
    P_zes_pozadi: TPanel;
    P_Zes_Right: TPanel;
    L_Zes_Napajeni: TLabel;
    L_Zes_OK: TLabel;
    L_Zes_NapajeniL_Zes_Zkrat: TLabel;
    L_Zes_Nedetekovano: TLabel;
    P_Zes_Left: TPanel;
    E_dataload_zes: TEdit;
    TS_Users: TTabSheet;
    LV_Users: TListView;
    P_Users_pozadi: TPanel;
    P_Users_Left: TPanel;
    E_dataload_users: TEdit;
    TS_Stav_RCS: TTabSheet;
    LV_Stav_RCS: TListView;
    TS_VC: TTabSheet;
    P_VC_Pozadi: TPanel;
    P_VC_Left: TPanel;
    E_Dataload_JC: TEdit;
    LV_JC: TListView;
    TS_log: TTabSheet;
    LV_log: TListView;
    TS_Intellibox: TTabSheet;
    LV_log_lnet: TListView;
    GB_Connected_Panels: TGroupBox;
    LV_Clients: TListView;
    GB_stav_technologie: TGroupBox;
    S_RCS_open: TShape;
    S_RCS_start: TShape;
    S_Trakce_Connected: TShape;
    S_DCC: TShape;
    S_Server: TShape;
    L_StavS_1: TLabel;
    L_StavS_2: TLabel;
    L_StavS_3: TLabel;
    L_StavS_4: TLabel;
    L_StavS_6: TLabel;
    GB_Log: TGroupBox;
    LB_Log: TListBox;
    MI_File: TMenuItem;
    MI_Save_config: TMenuItem;
    S_locos_acquired: TShape;
    Label1: TLabel;
    PM_HV: TPopupMenu;
    PM_Properties: TMenuItem;
    PM_Regulator: TMenuItem;
    N6: TMenuItem;
    Panel2: TPanel;
    CHB_Mainlog_File: TCheckBox;
    CHB_mainlog_table: TCheckBox;
    Panel3: TPanel;
    Label2: TLabel;
    CB_centrala_loglevel_file: TComboBox;
    N9: TMenuItem;
    PM_SaveLayout: TMenuItem;
    A_SaveStav: TAction;
    PM_Bloky: TPopupMenu;
    MI_TechProp: TMenuItem;
    MenuItem2: TMenuItem;
    MI_Prop: TMenuItem;
    B_VC_Add: TButton;
    B_VC_delete: TButton;
    B_JC_Reset: TButton;
    B_RemoveStack: TButton;
    TS_MultiJC: TTabSheet;
    Panel6: TPanel;
    P_MJC_Left: TPanel;
    E_Dataload_multiJC: TEdit;
    LV_MultiJC: TListView;
    B_mJC_Add: TButton;
    B_mJC_Remove: TButton;
    PM_Clients: TPopupMenu;
    MI_Disconnect: TMenuItem;
    G_locos_acquired: TGauge;
    N10: TMenuItem;
    MI_Trk_Func_Vyzn: TMenuItem;
    A_FuncsSet: TAction;
    TS_FuncsVyznam: TTabSheet;
    M_funcsVyznam: TMemo;
    P_funcsVyznamBg: TPanel;
    B_Change: TButton;
    CHB_LoadChanges: TCheckBox;
    AE_Main: TApplicationEvents;
    SD_HV_Stats: TSaveDialog;
    CB_centrala_loglevel_table: TComboBox;
    Label7: TLabel;
    S_PTServer: TShape;
    L_PTServer: TLabel;
    MI_PT: TMenuItem;
    MI_Start: TMenuItem;
    MI_Stop: TMenuItem;
    A_PT_Start: TAction;
    A_PT_Stop: TAction;
    MI_Houk: TMenuItem;
    MI_RCS_Update: TMenuItem;
    TS_AB: TTabSheet;
    Panel4: TPanel;
    LV_AB: TListView;
    P_AB_Left: TPanel;
    B_AB_Delete: TButton;
    B_BlkAdd: TButton;
    B_BlkDelete: TButton;
    B_HV_Add: TButton;
    B_HV_Delete: TButton;
    B_lok_delete: TButton;
    B_HVStats_Export: TButton;
    B_HVStats_Clear: TButton;
    B_zes_add: TButton;
    B_zes_delete: TButton;
    B_User_Add: TButton;
    B_User_Delete: TButton;
    E_dataload_HV_state: TEdit;
    E_dataload_users_stat: TEdit;
    P_dataload_rcs: TPanel;
    CHB_RCS_Show_Only_Active: TCheckBox;
    N11: TMenuItem;
    CHB_rcslog: TCheckBox;
    N12: TMenuItem;
    MI_Trk_Options: TMenuItem;
    N13: TMenuItem;
    MI_Trk_Libs: TMenuItem;
    MI_Trk_Update: TMenuItem;
    A_Trk_Lib_Cfg: TAction;
    A_Turnoff_Functions: TAction;
    CHB_Log_Auth: TCheckBox;
    procedure T_MainTimer(Sender: TObject);
    procedure PM_NastaveniClick(Sender: TObject);
    procedure PM_ResetVClick(Sender: TObject);
    procedure MI_RCS_libClick(Sender: TObject);
    procedure MI_Trk_libClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure AE_1Message(var Msg: tagMSG;
      var Handled: Boolean);
    procedure PM_TesterClick(Sender: TObject);
    procedure PM_Help_RPClick(Sender: TObject);
    procedure PM_SB1Click(Sender: TObject);
    procedure T_GUI_refreshTimer(Sender: TObject);
    procedure T_clear_log_msgTimer(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure L_DateDblClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PM_SaveFormPosClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure PM_ConsoleClick(Sender: TObject);
    procedure A_RCS_GoExecute(Sender: TObject);
    procedure A_RCS_StopExecute(Sender: TObject);
    procedure A_RCS_lib_cfgExecute(Sender: TObject);
    procedure A_DCC_GoExecute(Sender: TObject);
    procedure A_DCC_StopExecute(Sender: TObject);
    procedure A_System_StartExecute(Sender: TObject);
    procedure A_System_StopExecute(Sender: TObject);
    procedure A_Trk_ConnectExecute(Sender: TObject);
    procedure A_Trk_DisconnectExecute(Sender: TObject);
    procedure A_Locos_AcquireExecute(Sender: TObject);
    procedure A_Locos_ReleaseExecute(Sender: TObject);
    procedure A_PanelServer_StartExecute(Sender: TObject);
    procedure A_PanelServer_StopExecute(Sender: TObject);
    procedure A_RCS_OpenExecute(Sender: TObject);
    procedure A_RCS_CloseExecute(Sender: TObject);
    procedure LV_ClientsCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure PC_1Change(Sender: TObject);
    procedure LV_ZesilovaceDblClick(Sender: TObject);
    procedure B_zes_addClick(Sender: TObject);
    procedure B_zes_deleteClick(Sender: TObject);
    procedure LV_ZesilovaceCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure LV_HVChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure LV_HVDblClick(Sender: TObject);
    procedure B_HV_AddClick(Sender: TObject);
    procedure B_HV_DeleteClick(Sender: TObject);
    procedure LV_BlokyCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure LV_BlokyDblClick(Sender: TObject);
    procedure B_BlkAddClick(Sender: TObject);
    procedure LV_BlokyChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure B_BlkDeleteClick(Sender: TObject);
    procedure B_VC_AddClick(Sender: TObject);
    procedure LV_JCDblClick(Sender: TObject);
    procedure B_VC_deleteClick(Sender: TObject);
    procedure LV_logCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure LV_log_lnetCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure LV_ZesilovaceChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure LV_JCChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure MI_Save_configClick(Sender: TObject);
    procedure LB_LogDblClick(Sender: TObject);
    procedure LV_SoupravyChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure B_lok_deleteClick(Sender: TObject);
    procedure LV_log_lnetDblClick(Sender: TObject);
    procedure LV_HVCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure PM_PropertiesClick(Sender: TObject);
    procedure PM_RegulatorClick(Sender: TObject);
    procedure PM_HVPopup(Sender: TObject);
    procedure LV_JCCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure CB_centrala_loglevel_fileChange(Sender: TObject);
    procedure LB_LogDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure B_User_AddClick(Sender: TObject);
    procedure LV_UsersChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure LV_UsersDblClick(Sender: TObject);
    procedure B_User_DeleteClick(Sender: TObject);
    procedure A_SaveStavExecute(Sender: TObject);
    procedure PM_BlokyPopup(Sender: TObject);
    procedure MI_PropClick(Sender: TObject);
    procedure MI_TechPropClick(Sender: TObject);
    procedure B_JC_ResetClick(Sender: TObject);
    procedure P_Time_modelovyDblClick(Sender: TObject);
    procedure P_ZrychleniDblClick(Sender: TObject);
    procedure LV_StaniceChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure B_RemoveStackClick(Sender: TObject);
    procedure LV_MultiJCCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure LV_MultiJCChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure B_mJC_AddClick(Sender: TObject);
    procedure B_mJC_RemoveClick(Sender: TObject);
    procedure LV_MultiJCDblClick(Sender: TObject);
    procedure PM_ClientsPopup(Sender: TObject);
    procedure MI_DisconnectClick(Sender: TObject);
    procedure A_FuncsSetExecute(Sender: TObject);
    procedure B_ChangeClick(Sender: TObject);
    procedure LV_BlokyKeyPress(Sender: TObject; var Key: Char);
    procedure LV_JCKeyPress(Sender: TObject; var Key: Char);
    procedure LV_MultiJCKeyPress(Sender: TObject; var Key: Char);
    procedure LV_UsersKeyPress(Sender: TObject; var Key: Char);
    procedure LV_ZesilovaceKeyPress(Sender: TObject; var Key: Char);
    procedure LV_HVKeyPress(Sender: TObject; var Key: Char);
    procedure B_ClearStatsClick(Sender: TObject);
    procedure B_HVStats_ExportClick(Sender: TObject);
    procedure CB_centrala_loglevel_tableChange(Sender: TObject);
    procedure A_PT_StartExecute(Sender: TObject);
    procedure A_PT_StopExecute(Sender: TObject);
    procedure LV_Stav_RCSCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure MI_HoukClick(Sender: TObject);
    procedure MI_RCS_UpdateClick(Sender: TObject);
    procedure B_AB_DeleteClick(Sender: TObject);
    procedure LV_ABChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure CHB_RCS_Show_Only_ActiveClick(Sender: TObject);
    procedure CHB_rcslogClick(Sender: TObject);
    procedure A_Trk_Lib_CfgExecute(Sender: TObject);
    procedure MI_Trk_UpdateClick(Sender: TObject);
    procedure A_Turnoff_FunctionsExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CHB_Log_AuthClick(Sender: TObject);
  private
    KomunikaceGo: TdateTime;
    call_method: TNotifyEvent;
    mCpuLoad: TCpuLoad;

    procedure UpdateCallMethod();
    procedure OnFuncsVyznamChange(Sender: TObject);

    procedure WMPowerBroadcast(var Msg: TMessage); message WM_POWERBROADCAST;
    procedure WMQueryEndSession(var Msg: TWMQueryEndSession); message WM_QUERYENDSESSION;
    procedure WMEndSession(var Msg: TWMEndSession); message WM_ENDSESSION;

  public
    KomunikacePocitani: Shortint;
    CloseMessage: Boolean; // jestli se ptat uzivatele na ukonceni SW
    NUZClose: Boolean; // flag hard ukonceni SW bez kontroly pripojeni k systemum a zobrazeni dialogu
    sb1Log: Boolean;

    procedure CloseForm();
    procedure RepaintObjects();
    procedure LoadIniLibData();
    procedure DetekujAutSpusteniSystemu();
    procedure OnStart();
    procedure SaveFormPosition();
    procedure VypisDatumCas();
    procedure LogStatus(str: string);
    procedure DisableRemoveButtons();
    procedure SetCallMethod(Method: TNotifyEvent);
    procedure UpdateSystemButtons();
    procedure CheckNasobicWidth();
    function SoupravySelectedCount(): Integer;
    function LVSelectedTexts(LV: TListView; single: string; multiple: string): string;

    // RCS
    procedure OnRCSStart(Sender: TObject);
    procedure OnRCSScanned(Sender: TObject);
    procedure OnRCSStop(Sender: TObject);
    procedure OnRCSOpen(Sender: TObject);
    procedure OnRCSClose(Sender: TObject);
    procedure OnRCSErrOpen(Sender: TObject; errMsg: string);
    procedure OnRCSErrClose(Sender: TObject; errMsg: string);
    procedure OnRCSErrStart(Sender: TObject; errMsg: string);
    procedure OnRCSErrStop(Sender: TObject; errMsg: string);
    procedure OnRCSReady(Sender: TObject; ready: Boolean);

    procedure UpdateRCSLibsList();

    // Trakce
    procedure OnTrkBeforeOpen(Sender: TObject);
    procedure OnTrkAfterOpen(Sender: TObject);
    procedure OnTrkBeforeClose(Sender: TObject);
    procedure OnTrkAfterClose(Sender: TObject);
    procedure OnTrkReady(Sender: TObject; ready: Boolean);
    procedure OnTrkErrOpen(Sender: TObject; errMsg: string);
    procedure OnTrkStatusChange(Sender: TObject; trkStatus: TTrkStatus);

    procedure UpdateTrkLibsList();
    procedure OnDCCGoError(Sender: TObject; Data: Pointer);
    procedure OnDCCStopError(Sender: TObject; Data: Pointer);

    procedure OnTrkAllAcquired(Sender: TObject);
    procedure OnTrkAcquireError(Sender: TObject);
    procedure OnTrkAllReleased(Sender: TObject);
    procedure OnTrkLocoAcquired(Sender: TObject);
    procedure OnTrkLocoReleased(Sender: TObject);
    procedure OnTrkAllFunctionTurnedOff(Sender: TObject; data: Pointer);

  end;//public

 TSystemStatus = (null, starting, stopping);                                    // stav startovani / vypinani systemu
 TSystem=class
   Status: TSystemStatus;                                                        // aktualni stav systemu
 end;

var
  F_Main: TF_Main;
  SystemData: TSystem;

implementation

uses fTester, fSettings, fNastaveni_Casu, fSplash, fHoukEvsUsek, DataJC,
     fAbout, Verze, fSystemInfo, fBlkUsek, fBlkVyhybka, fAdminForm, Simulation,
     fRegulator, fBlkSH, fSystemAutoStart, fBlkUsekSysVars, GetSystems,
     TechnologieRCS, TechnologieJC, FileSystem, fConsole, TOblsRizeni, BlockDb,
     Block, BlockTrack, BlockTurnout, BlockSignal, BlockIR, TOblRizeni,
     SnadnSpusteni, BlockSummary, BlockCrossing, TJCDatabase, Logging,
     TCPServerOR, DataBloky, DataHV, DataRCS, DataORs, DataZesilovac,
     fBlkNew, fHVEdit, fJCEdit, fZesilovacEdit, THVDatabase, fBlkIR, fBlkPrejezd,
     fBlkNav, fBlkTrat, BlockLinker, TrainDb, DataSpr, DataUsers, fUserEdit, UserDb,
     fBlkVyhybkaSysVars, fBlkTratSysVars, BlockRailway, ModelovyCas, fBlkZamek,
     BlockLock, DataMultiJC, TMultiJCDatabase, fMJCEdit, BlockDisconnector,
     fBlkRozp, fFuncsSet, FunkceVyznam, fBlkTU, RCSdebugger, Booster, DataAB,
     AppEv, fBlkIO, BlockIO, TCPServerPT, RCSErrors, TechnologieAB,
     Diagnostics, BlockAC, fBlkAC;

{$R *.dfm}

////////////////////////////////////////////////////////////////////////////////
// RCS BEGIN
////////////////////////////////////////////////////////////////////////////////

procedure TF_Main.MI_RCS_libClick(Sender: TObject);
var fn: string;
 begin
  fn := StringReplace(TMenuItem(Sender).Caption, '&', '', [rfReplaceAll]);

  Screen.Cursor := crHourGlass;
  writelog('RCS -> ' + fn, WR_RCS);
  try
    RCSi.LoadLib(RCSi.libDir + '\' + fn);
    Self.LogStatus('RCS: načteno ' + fn);
  except
    on E: Exception do
     begin
      Screen.Cursor := crDefault;
      Application.MessageBox(PChar('Nelze načíst knihovnu ' + fn + ':'+#13#10+
          E.Message), 'Nelze načíst knihovnu', MB_OK OR MB_ICONWARNING);
      Exit();
     end;
  end;
  RCSTableData.LoadToTable(not Self.CHB_RCS_Show_Only_Active.Checked);
  Screen.Cursor := crDefault;
 end;

procedure TF_Main.UpdateRCSLibsList();
var SR: TSearchRec;
    item: TMenuItem;

    procedure AddLib(name: string);
    begin
     item := TMenuItem.Create(Self.MI_RCS_Libs);
     item.Caption := name;
     item.OnClick := Self.MI_RCS_libClick;
     Self.MI_RCS_Libs.Add(item);
    end;
 begin
  Self.MI_RCS_Libs.Clear();

  if (FindFirst(RCSi.libDir+'\*.dll', faAnyFile, SR) = 0) then
   begin
    if ((SR.Attr AND faDirectory) = 0) then
      AddLib(SR.Name);

    while (FindNext(SR) = 0) do
      if ((SR.Attr AND faDirectory) = 0) then
        AddLib(SR.Name);

    SysUtils.FindClose(SR);
   end;
end;

procedure TF_Main.A_RCS_lib_cfgExecute(Sender: TObject);
begin
 if (not RCSi.HasDialog()) then
  begin
   Application.MessageBox('Aktuální knihovna nemá konfigurační okno.', 'Info', MB_OK OR MB_ICONINFORMATION);
   Exit();
  end;

 Screen.Cursor := crHourGlass;
 try
   RCSi.ShowConfigDialog();
 except
   on E: Exception do
    begin
     Screen.Cursor := crDefault;
     Application.MessageBox(PChar('Nelze zobrazit konfigurační dialog RCS : ' + E.Message),
                            'Varování', MB_OK OR MB_ICONWARNING);
     Exit();
    end;
 end;
 Screen.Cursor := crDefault;
 writelog('Zobrazen ConfigDialog knihovny', WR_RCS);
end;

procedure TF_Main.A_RCS_CloseExecute(Sender: TObject);
begin
 if ((SystemData.Status = stopping) and (not RCSi.NoExOpened)) then
  begin
   Self.LogStatus('System: stop OK');
   SystemData.Status := null;
   Self.UpdateSystemButtons();
   Exit();
  end;

 F_Main.S_RCS_open.Brush.Color := clBlue;
 Self.LogStatus('RCS: uzavírám zařízení...');

 writelog('----- RCS CLOSING -----', WR_RCS);

 with (F_Main) do
  begin
   A_RCS_Open.Enabled      := false;
   A_RCS_Close.Enabled     := false;
   SB1.Panels.Items[_SB_RCS].Text := 'Zavírám RCS...';

   A_System_Start.Enabled := false;
   A_System_Stop.Enabled := false;
  end;//with F_Main do

 try
   RCSi.Close();
 except
   on E: ERCSNotOpened do
     Self.OnRCSErrClose(Self, 'RCS není otevřeno, nelze jej proto zavřít!');
   on E: ERCSScanningNotFinished do
     Self.OnRCSErrClose(Self, 'RCS nelze uzavřít před sokončneíms kenování modulů!');
   on E: Exception do
     Self.OnRCSErrClose(Self, 'Nastala kritická chyba : '+E.Message);
 end;
end;

procedure TF_Main.A_RCS_GoExecute(Sender: TObject);
begin
 if ((SystemData.Status = starting) and (RCSi.NoExStarted)) then
  begin
   Self.A_Trk_ConnectExecute(nil);
   Exit();
  end;

  with (F_Main) do
   begin
    A_RCS_Go.Enabled     := false;
    A_RCS_Stop.Enabled   := false;
    A_RCS_Close.Enabled  := false;

    A_System_Start.Enabled := false;
    A_System_Stop.Enabled := false;

    SB1.Panels.Items[_SB_RCS].Text := 'Spouštím RCS...';
   end;//with F_Main do

  Self.LogStatus('RCS: Spouštím komunikaci...');
  F_Main.S_RCS_Start.Brush.Color   := clBlue;

  writelog('----- RCS STARTING -----', WR_RCS);

  try
    RCSi.Start();
  except
   on E: ERCSAlreadyStarted do
     Self.OnRCSErrStart(Self, 'Komunikace již probíhá!');
   on E: ERCSFirmwareTooLow do
     Self.OnRCSErrStart(Self, 'Firmware RCS-USB modulu je starý, nelze se připojit k takto starému FW!');
   on E: ERCSNoModules do
     Self.OnRCSErrStart(Self, 'Na sběrnici nebyl nalezen žádný RCS modul, nelze spustit komunikaci!');
   on E: ERCSNotOpened do
     Self.OnRCSErrStart(Self, 'Nepřipojeno k RCS-USB, připojte se nejdříve k RCS-USB!');
   on E: ERCSScanningNotFinished do
     Self.OnRCSErrStart(Self, 'Neproběhl sken modulů, vyčkejte na dokončení skenu modulů!');
   on E: Exception do
     Self.OnRCSErrStart(Self, 'Nastala kritická chyba : '+E.Message);
  end;
end;

procedure TF_Main.A_RCS_OpenExecute(Sender: TObject);
begin
 if ((SystemData.Status = starting) and (RCSi.NoExOpened)) then
  begin
   Self.A_RCS_GoExecute(nil);
   Exit();
  end;

 with (F_Main) do
  begin
   A_RCS_Open.Enabled     := false;
   A_RCS_Close.Enabled    := false;

   SB1.Panels.Items[_SB_RCS].Text := 'Otevírám RCS...';

   A_System_Start.Enabled := false;
   A_System_Stop.Enabled := false;
  end;//with F_Main do

 Self.LogStatus('RCS: Otevírám zařízení, hledám moduly...');
 F_Main.S_RCS_open.Brush.Color   := clBlue;

 writelog('----- RCS OPENING -----', WR_RCS);

 try
   RCSi.Open();
 except
  on E: ERCSAlreadyOpened do
    Self.OnRCSErrOpen(Self, 'RCS je již otevřeno!');
  on E: ERCSCannotOpenPort do
    Self.OnRCSErrOpen(Self, 'Nepodařilo se otevřít USB port, otevřete konfigurační okno RCS driveru a zkontrolujte, že je vybrán správný port!');
  on E: Exception do
    Self.OnRCSErrOpen(Self, 'Nastala kritická chyba : '+E.Message);
 end;
end;

procedure TF_Main.A_RCS_StopExecute(Sender: TObject);
begin
 if ((SystemData.Status = stopping) and (not RCSi.NoExStarted)) then
  begin
   F_Main.A_RCS_CloseExecute(nil);
   Exit();
  end;

 F_Main.S_RCS_Start.Brush.Color := clGray;
 Self.LogStatus('RCS: zastavuji komunikaci...');

 writelog('----- RCS STOPPING -----', WR_RCS);

 with (F_Main) do
  begin
   A_RCS_Go.Enabled      := false;
   A_RCS_Stop.Enabled    := false;
   SB1.Panels.Items[_SB_RCS].Text := 'Zastavuji RCS...';

   A_System_Start.Enabled := false;
   A_System_Stop.Enabled := false;
  end;//with F_Main do

  try
    RCSi.Stop();
  except
   on E: ERCSNotStarted do
     Self.OnRCSErrStop(Self, 'RCS komunikace není spuštěna, nelze ji proto zastavit!');
   on E: Exception do
     Self.OnRCSErrStop(Self, 'Nastala kritická chyba : '+E.Message);
  end;
end;

//--- events from RCS lib begin ---
procedure TF_Main.OnRCSStart(Sender: TObject);
begin
  with (F_Main) do
   begin
    A_RCS_Go.Enabled     := false;
    A_RCS_Stop.Enabled   := true;

    PM_Tester.Enabled    := true;
    PM_ResetV.Enabled    := true;

    SB1.Panels.Items[_SB_RCS].Text := 'RCS spuštěno';
    UpdateSystemButtons();
   end;//with F_Main do

  writelog('----- RCS START OK -----', WR_RCS);

  Self.LogStatus('RCS: komunikace spuštěna, čekám na první sken všech modulů...');
  RCSTableData.UpdateTable();
end;

procedure TF_Main.OnRCSScanned(Sender: TObject);
begin
  F_Main.S_RCS_Start.Brush.Color := clLime;
  RCSTableData.UpdateTable();

  writelog('----- RCS SCANNED -----', WR_RCS);
  Self.LogStatus('RCS: moduly naskenovány');

  // inicialziace osvetleni
  ORs.InitOsv();

  if (SystemData.Status = starting) then
   Self.A_Trk_ConnectExecute(nil);
end;

procedure TF_Main.OnRCSStop(Sender: TObject);
begin
  if (Blocks.enabled) then
   begin
    Blocks.Disable();
    Trains.ClearPOdj();
   end;

  ModCas.started := false;
  Self.UpdateSystemButtons();

  if (F_Tester.Showing) then F_Tester.Close();

  F_Main.S_RCS_Start.Brush.Color := clRed;

  with (F_Main) do
   begin
    A_RCS_Go.Enabled      := true;
    A_RCS_Stop.Enabled    := false;
    A_RCS_Close.Enabled   := true;

    PM_ResetV.Enabled     := false;
    PM_Tester.Enabled     := false;

    SB1.Panels.Items[_SB_RCS].Text := 'RCS otevřeno';
   end;//with F_Main do


  writelog('----- RCS STOP OK -----', WR_RCS);

  Self.LogStatus('RCS: komunikace zastavena');

  RCSTableData.UpdateTable();

  if ((F_Main.Showing) and (F_Main.PC_1.ActivePage = F_Main.TS_Bloky)) then
    BlokyTableData.UpdateTable();

  ORTCPServer.BroadcastBottomError('Výpadek systému RCS!', 'TECHNOLOGIE');

  if (SystemData.Status = stopping) then
   Self.A_RCS_CloseExecute(nil);
end;

procedure TF_Main.OnRCSOpen(Sender: TObject);
var i: Integer;
    str: string;
begin
 Self.A_RCS_Open.Enabled     := false;
 Self.A_RCS_Close.Enabled    := true;
 Self.A_RCS_Go.Enabled       := true;
 Self.A_RCS_Stop.Enabled     := false;
 Self.MI_RCS_Libs.Enabled    := false;
 Self.UpdateSystemButtons();

 F_Main.S_RCS_open.Brush.Color := clLime;

 try
   writelog('----- RCS OPEN OK : '+IntToStr(RCSi.GetModuleCount)+' modules -----', WR_RCS);
 except
   writelog('----- RCS OPEN OK : unknown amount of modules -----', WR_RCS);
 end;

 Self.LogStatus('RCS: otevřeno');
 SB1.Panels.Items[_SB_RCS].Text := 'RCS otevřeno';

 F_Tester.AfterRCSOpen();

 RCSTableData.LoadToTable(not Self.CHB_RCS_Show_Only_Active.Checked);

 if (SystemData.Status = starting) then
  begin
   // scan, jestli nahodou nechybi RCS desky
   str := '';
   for i := 0 to RCSi.maxModuleAddr+1 do
    if ((RCSi.GetNeeded(i)) and (not RCSi.IsModule(i))) then
     begin
      if (Length(str) > 0) then str := str + ', ';
      str := str + IntToStr(i);
     end;
   if (str <> '') then
    begin
     writelog('Chybí RCS moduly '+str, WR_RCS);
     Self.LogStatus('WARN: Chybí RCS moduly '+str);
    end;

   Self.A_RCS_GoExecute(nil);
  end;
end;

procedure TF_Main.OnRCSClose(Sender: TObject);
begin
 Self.A_RCS_Go.Enabled    := false;
 Self.A_RCS_Stop.Enabled  := false;
 Self.A_RCS_Close.Enabled := false;
 Self.A_RCS_Open.Enabled  := true;
 Self.MI_RCS_Libs.Enabled := true;
 Self.UpdateSystemButtons();

 // may happen when RCS USB disconnects
 if (Blocks.enabled) then
  begin
   Blocks.Disable();
   Trains.ClearPOdj();
  end;
 Trains.StopAllTrains();

 F_Main.S_RCS_open.Brush.Color  := clRed;
 F_Main.S_RCS_Start.Brush.Color := clRed;

 writelog('----- RCS CLOSE OK -----', WR_RCS);

 Self.LogStatus('RCS: uzavřeno');
 SB1.Panels.Items[_SB_RCS].Text := 'RCS zavřeno';

 ORTCPServer.BroadcastBottomError('Výpadek systému RCS!', 'TECHNOLOGIE');

 if (SystemData.Status = stopping) then
  begin
   Self.LogStatus('System: stop OK');
   SystemData.Status := null;
   Self.UpdateSystemButtons();
  end;

 RCSTableData.UpdateTable();
end;

procedure TF_Main.OnRCSErrOpen(Sender: TObject; errMsg: string);
begin
 Self.A_RCS_Go.Enabled    := false;
 Self.A_RCS_Stop.Enabled  := false;
 Self.A_RCS_Open.Enabled  := true;
 Self.UpdateSystemButtons();

 F_Main.S_RCS_open.Brush.Color := clRed;

 SystemData.Status := TSystemStatus.null;

 Self.LogStatus('ERR: RCS OPEN FAIL: '+errMsg);
 writelog('----- RCS OPEN FAIL - '+errMsg+' -----', WR_ERROR);
 SB1.Panels.Items[_SB_RCS].Text := 'RCS zavřeno';

 Application.MessageBox(PChar('Při otevírání RCS nastala chyba:'+#13#10+errMsg), 'Chyba', MB_OK OR MB_ICONWARNING);
end;

procedure TF_Main.OnRCSErrClose(Sender: TObject; errMsg: string);
begin
 A_RCS_Go.Enabled    := false;
 A_RCS_Stop.Enabled  := false;
 A_RCS_Open.Enabled  := true;

 F_Main.S_RCS_open.Brush.Color := clRed;

 SystemData.Status := null;

 Self.LogStatus('ERR: RCS CLOSE FAIL: '+errMsg);
 SB1.Panels.Items[_SB_RCS].Text := 'RCS zavřeno';

 Application.MessageBox(PChar('Při uzavírání RCS nastala chyba:'+#13#10+errMsg),'Chyba', MB_OK OR MB_ICONWARNING);
 writelog('----- RCS CLOSE FAIL - '+errMsg+' -----', WR_ERROR);
end;

procedure TF_Main.OnRCSErrStart(Sender: TObject; errMsg: string);
begin
  A_RCS_Close.Enabled := true;
  Self.UpdateSystemButtons();
  A_RCS_Go.Enabled := true;

  SB1.Panels.Items[_SB_RCS].Text := 'RCS otevřeno';
  S_RCS_Start.Brush.Color := clRed;

  SystemData.Status := TSystemStatus.null;

  Self.LogStatus('ERR: RCS START FAIL: '+errMsg);
  writelog('----- RCS START FAIL - '+errMsg+' -----', WR_ERROR);

  Application.MessageBox(PChar('Při zapínání komunikace nastala chyba:'+#13#10+errMsg), 'Chyba', MB_OK OR MB_ICONWARNING);
end;

procedure TF_Main.OnRCSErrStop(Sender: TObject; errMsg: string);
begin
  A_RCS_Open.Enabled := true;
  A_RCS_Close.Enabled := true;
  A_RCS_Go.Enabled := true;

  SB1.Panels.Items[_SB_RCS].Text := 'RCS otevřeno';
  S_RCS_Start.Brush.Color := clRed;

  SystemData.Status := null;

  Self.LogStatus('ERR: RCS STOP FAIL: '+errMsg);

  Application.MessageBox(PChar('Při vypínání komunikace nastala chyba:'+#13#10+errMsg+#13#10), 'Chyba', MB_OK OR MB_ICONWARNING);
  writelog('----- RCS STOP FAIL - '+errMsg+' -----', WR_ERROR);
end;

procedure TF_Main.OnRCSReady(Sender: TObject; ready: Boolean);
var started, opened: Boolean;
begin
 try
   started := RCSi.Started;
   opened := RCSi.Opened;
 except
   on E: Exception do
    begin
     started := false;
     opened := false;
     AppEvents.LogException(E, 'OnRCSReady');
    end;
 end;

 Self.A_RCS_Open.Enabled  := ready and (not opened);
 Self.A_RCS_Close.Enabled := ready and opened;
 Self.A_RCS_Go.Enabled    := ready and opened and (not started);
 Self.A_RCS_Stop.Enabled  := ready and started;

 try
   if ((ready) and (diag.simInputs) and (RCSi.simulation)) then
     RCSi.InputSim();
 except
   on E: Exception do
     writelog('Nelze provést inputSim : ' + E.Message, WR_ERROR);
 end;
end;

//--- events from RCS lib end ---

////////////////////////////////////////////////////////////////////////////////
// RCS END
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
// TRAKCE BEGIN
////////////////////////////////////////////////////////////////////////////////

procedure TF_Main.MI_Trk_libClick(Sender: TObject);
var fn: string;
 begin
  fn := StringReplace(TMenuItem(Sender).Caption, '&', '', [rfReplaceAll]);

  Screen.Cursor := crHourGlass;
  TrakceI.Log(llInfo, 'Změna knihovny -> ' + fn);
  Self.SB1.Panels.Items[_SB_TRAKCE_LIB].Text := '-';
  try
    TrakceI.LoadLib(TrakceI.libDir + '\' + fn);
    Self.LogStatus('Trakce: načteno ' + fn);
    Self.SB1.Panels.Items[_SB_TRAKCE_LIB].Text := ExtractFileName(TrakceI.Lib);
  except
    on E: Exception do
     begin
      Screen.Cursor := crDefault;
      Application.MessageBox(PChar('Nelze načíst knihovnu ' + fn + ':'+#13#10+
          E.Message), 'Nelze načíst knihovnu', MB_OK OR MB_ICONWARNING);
      Exit();
     end;
  end;
  Screen.Cursor := crDefault;
 end;

procedure TF_Main.UpdateTrkLibsList();
var SR: TSearchRec;
    item: TMenuItem;

    procedure AddLib(name: string);
    begin
     item := TMenuItem.Create(Self.MI_Trk_Libs);
     item.Caption := name;
     item.OnClick := Self.MI_Trk_libClick;
     Self.MI_Trk_Libs.Add(item);
    end;
 begin
  Self.MI_Trk_Libs.Clear();

  if (FindFirst(TrakceI.libDir+'\*.dll', faAnyFile, SR) = 0) then
   begin
    if ((SR.Attr AND faDirectory) = 0) then
      AddLib(SR.Name);

    while (FindNext(SR) = 0) do
      if ((SR.Attr AND faDirectory) = 0) then
        AddLib(SR.Name);

    SysUtils.FindClose(SR);
   end;
end;

procedure TF_Main.A_Trk_Lib_CfgExecute(Sender: TObject);
begin
 if (not TrakceI.HasDialog()) then
  begin
   Application.MessageBox('Aktuální knihovna nemá konfigurační okno.', 'Info', MB_OK OR MB_ICONINFORMATION);
   Exit();
  end;

 Screen.Cursor := crHourGlass;
 try
   TrakceI.ShowConfigDialog();
 except
   on E: Exception do
    begin
     Screen.Cursor := crDefault;
     Application.MessageBox(PChar('Nelze zobrazit konfigurační dialog: ' + E.Message),
                            'Varování', MB_OK OR MB_ICONWARNING);
     Exit();
    end;
 end;
 Screen.Cursor := crDefault;
end;

procedure TF_Main.MI_Trk_UpdateClick(Sender: TObject);
begin
 try
   Self.UpdateTrkLibsList();
   Application.MessageBox('Seznam knihoven úspěšně aktualizován.', 'Info', MB_OK OR MB_ICONINFORMATION);
 except
   on E: Exception do
     Application.MessageBox(PChar('Seznam knihoven se nepodařilo aktualizovat:'+#13#10 + E.Message),
        'Chyba', MB_OK OR MB_ICONWARNING);
 end;
end;

procedure TF_Main.A_Trk_ConnectExecute(Sender: TObject);
begin
 if ((SystemData.Status = starting) and (TrakceI.ConnectedSafe())) then
  begin
   Self.A_DCC_GoExecute(Self);
   Exit();
  end;

 try
   TrakceI.Connect();
 except
   on E: Exception do
    begin
     if (TrakceI.opening) then
      begin
       Self.OnTrkErrOpen(Self, E.Message);
       Self.OnTrkAfterClose(Self);
      end;
    end;
 end;

 Application.ProcessMessages();
end;

procedure TF_Main.A_Trk_DisconnectExecute(Sender: TObject);
begin
 if ((SystemData.Status = stopping) and (not TrakceI.ConnectedSafe())) then
  begin
   F_Main.A_RCS_StopExecute(nil);
   Exit();
  end;

 try
   TrakceI.Disconnect();
 except
   on E: Exception do
    begin
     TrakceI.Log(llErrors, 'CLOSE: error: ' + E.Message);
     Application.MessageBox(PChar('Chyba pri uzavírání komunikace s centrálou:'+#13#10+E.Message+#13#10+'Více informací naleznete v logu.'),
                            'Chyba', MB_OK OR MB_ICONERROR);
    end;
 end;

 Application.ProcessMessages();
end;

procedure TF_Main.A_Locos_AcquireExecute(Sender: TObject);
var addr: Cardinal;
begin
 F_Main.LogStatus('Loko: přebírám...');
 F_Main.S_locos_acquired.Brush.Color := clBlue;
 F_Main.G_locos_acquired.ForeColor := clBlue;

 F_Main.G_locos_acquired.MaxValue := 0;
 for addr := 0 to THVDatabase._MAX_ADDR-1 do
   if (HVDb[addr] <> nil) and (HVDb[addr].ShouldAcquire()) then
     F_Main.G_locos_acquired.MaxValue := F_Main.G_locos_acquired.MaxValue + 1;
 if (F_Main.G_locos_acquired.MaxValue = 0) then
   F_Main.G_locos_acquired.MaxValue := 1;

 HVDb.TrakceAcquireAllUsed(Self.OnTrkAllAcquired, Self.OnTrkAcquireError, Self.OnTrkLocoAcquired);
end;

procedure TF_Main.A_Locos_ReleaseExecute(Sender: TObject);
begin
 F_Main.LogStatus('Loko: odhlašuji...');
 F_Main.S_locos_acquired.Brush.Color := clBlue;
 F_Main.G_locos_acquired.ForeColor := clBlue;
 HVDb.TrakceReleaseAllUsed(Self.OnTrkAllReleased, Self.OnTrkLocoReleased);
end;

procedure TF_Main.OnTrkAllAcquired(Sender: TObject);
begin
 F_Main.LogStatus('Loko: všechna loko převzata');

 Self.S_locos_acquired.Brush.Color := clLime;
 Self.A_Locos_Acquire.Enabled := false;
 Self.A_Locos_Release.Enabled := true;

 Self.G_locos_acquired.Progress := Self.G_locos_acquired.MaxValue;
 Self.G_locos_acquired.ForeColor := clLime;

 if (SystemData.Status = starting) then
   F_Main.A_PanelServer_StartExecute(nil);
end;

procedure TF_Main.OnTrkAcquireError(Sender: TObject);
begin
 Self.G_locos_acquired.ForeColor := clRed;
 Self.S_locos_acquired.Brush.Color := clRed;
 Self.A_Locos_Acquire.Enabled := true;

 if (SystemData.Status = TSystemStatus.starting) then
  begin
   SystemData.Status := TSystemStatus.null;
   F_Main.A_System_Start.Enabled := true;
   F_Main.A_System_Stop.Enabled  := true;
  end;

 Application.MessageBox('Nepodařilo se převzít všechny lokomotivy, více informací v logu.', 'Chyba', MB_OK OR MB_ICONWARNING);
end;

procedure TF_Main.OnTrkAllReleased(Sender: TObject);
begin
 F_Main.LogStatus('Loko: všechna loko odhlášena');

 Self.S_locos_acquired.Brush.Color := clRed;

 Self.A_Locos_Acquire.Enabled  := true;
 Self.A_Locos_Release.Enabled := false;

 Self.G_locos_acquired.Progress  := 0;
 Self.G_locos_acquired.ForeColor := clBlue;

 if (SystemData.Status = stopping) then
   F_Main.SetCallMethod(F_Main.A_Trk_DisconnectExecute);
end;

procedure TF_Main.OnTrkLocoAcquired(Sender: TObject);
begin
 Self.G_locos_acquired.Progress := Self.G_locos_acquired.Progress + 1;
end;

procedure TF_Main.OnTrkLocoReleased(Sender: TObject);
begin
 Self.G_locos_acquired.Progress := Self.G_locos_acquired.Progress - 1;
end;

procedure TF_Main.A_DCC_GoExecute(Sender: TObject);
begin
 if ((SystemData.Status = starting) and (TrakceI.TrackStatusSafe() = TTrkStatus.tsOn)) then
  begin
   Self.A_Locos_AcquireExecute(Self);
   Exit();
  end;

 Self.LogStatus('DCC: zapínám');

 try
   TrakceI.SetTrackStatus(tsOn, TTrakce.Callback(), TTrakce.Callback(Self.OnDCCGoError));
 except
   on E: Exception do
    begin
     SystemData.Status := null;
     Application.MessageBox(PChar('Chyba při DCC GO:'+#13#10+E.Message), 'Chyba', MB_OK OR MB_ICONERROR);
     Self.LogStatus('DCC: START: ERR '+E.Message);
    end;
 end;
end;

procedure TF_Main.A_DCC_StopExecute(Sender: TObject);
begin
  Self.LogStatus('DCC: vypínám');

  try
    TrakceI.SetTrackStatus(tsOff, TTrakce.Callback(), TTrakce.Callback(Self.OnDCCStopError));
  except
    on E: Exception do
     begin
      Application.MessageBox(PChar('Chyba při DCC STOP:'+#13#10+E.Message),'Chyba', MB_OK OR MB_ICONERROR);
      Self.LogStatus('DCC: STOP: ERR '+E.Message);
     end;
  end;
end;

procedure TF_Main.A_FuncsSetExecute(Sender: TObject);
begin
 F_FuncsSet.Show();
end;

procedure TF_Main.OnDCCGoError(Sender: TObject; Data: Pointer);
begin
 SystemData.Status := TSystemStatus.null;
 Self.UpdateSystemButtons();
 Self.A_DCC_Go.Enabled       := true;
 Self.A_DCC_Stop.Enabled     := true;
 Self.S_DCC.Brush.Color  := clGray;
 Self.LogStatus('DCC: START: ERR: cenrála neodpověděla na příkaz');
 Application.MessageBox('Centrála neodpověděla na příkaz DCC START', 'Varování', MB_OK OR MB_ICONWARNING);
end;

procedure TF_Main.OnDCCStopError(Sender: TObject; Data: Pointer);
begin
 Self.LogStatus('DCC: STOP: ERR: cenrála neodpověděla na příkaz');
 Self.UpdateSystemButtons();
 Self.A_DCC_Go.Enabled       := true;
 Self.A_DCC_Stop.Enabled     := true;
 Self.S_DCC.Brush.Color  := clGray;
 Application.MessageBox('Centrála neodpověděla na příkaz DCC STOP', 'Varování', MB_OK OR MB_ICONWARNING);
end;

procedure TF_Main.OnTrkBeforeOpen(Sender: TObject);
begin
 Self.A_Trk_Connect.Enabled := false;
 Self.A_Trk_Disconnect.Enabled := false;
 Self.A_System_Start.Enabled := false;
 Self.A_System_Stop.Enabled := false;
 Self.SB1.Panels.Items[_SB_TRAKCE_STAV].Text := 'Připojování...';
 Self.S_Trakce_Connected.Brush.Color := clBlue;
 Self.LogStatus('Centrála: připojování...');
 Self.B_HV_Add.Enabled := false;
 Self.B_HV_Delete.Enabled := false;
 Self.MI_Trk_Libs.Enabled := false;
 Application.ProcessMessages();
end;

procedure TF_Main.OnTrkAfterOpen(Sender: TObject);
begin
 Self.A_Trk_Connect.Enabled := false;
 Self.A_Trk_Disconnect.Enabled := true;
 Self.SB1.Panels.Items[_SB_TRAKCE_STAV].Text := 'Centrála připojena';
 Self.LogStatus('Centrála: připojeno');
 Self.S_Trakce_Connected.Brush.Color := clLime;
 Self.A_Locos_Acquire.Enabled := true;
 Self.UpdateSystemButtons();
 Self.A_FuncsSet.Enabled := true;

 Application.ProcessMessages();
end;

procedure TF_Main.OnTrkBeforeClose(Sender: TObject);
begin
 Self.A_Trk_Connect.Enabled := false;
 Self.A_Trk_Disconnect.Enabled := false;
 Self.A_System_Start.Enabled := false;
 Self.A_System_Stop.Enabled := false;
 Self.SB1.Panels.Items[_SB_TRAKCE_STAV].Text := 'Odpojování...';
 Self.LogStatus('Centrála: odpojování...');
 Self.S_Trakce_Connected.Brush.Color := clBlue;
 Self.G_locos_acquired.Progress := 0;
 Self.S_locos_acquired.Brush.Color := clRed;
 Application.ProcessMessages();
end;

procedure TF_Main.OnTrkAfterClose(Sender: TObject);
begin
 Self.A_Trk_Connect.Enabled := true;
 Self.A_Trk_Disconnect.Enabled := false;
 Self.SB1.Panels.Items[_SB_TRAKCE_STAV].Text := 'Centrála odpojena';
 Self.LogStatus('Centrála: odpojena');
 Self.S_Trakce_Connected.Brush.Color := clRed;
 Self.A_Locos_Acquire.Enabled := false;
 Self.A_Locos_Release.Enabled := false;
 Self.B_HV_Add.Enabled := true;
 Self.S_locos_acquired.Brush.Color := clRed;
 Self.G_locos_acquired.Progress := 0;
 Self.MI_Trk_Libs.Enabled := true;
 Self.UpdateSystemButtons();

 RegCollector.CloseAll();
 HVTableData.LoadToTable();

 Self.S_DCC.Brush.Color := clGray;
 Self.A_DCC_Go.Enabled   := false;
 Self.A_DCC_Stop.Enabled := false;
 Self.A_FuncsSet.Enabled := false;
 if (F_FuncsSet.Showing) then F_FuncsSet.Close();

 HVDb.CSReset();
 Application.ProcessMessages();

 // no clinets shoudl be connected
 // when disconnect called with clients connected, fatal error happened
 ORTCPServer.BroadcastBottomError('Výpadek systému řízení trakce, ztráta kontroly nad jízdou!', 'TECHNOLOGIE');

 if (SystemData.Status = stopping) then
   Self.A_RCS_StopExecute(Self);
end;

procedure TF_Main.OnTrkReady(Sender: TObject; ready: Boolean);
begin
 Self.A_Trk_Connect.Enabled := ready and (not TrakceI.ConnectedSafe());
end;

procedure TF_Main.OnTrkErrOpen(Sender: TObject; errMsg: string);
begin
 if (SystemData.Status = TSystemStatus.starting) then
  begin
   SystemData.Status := TSystemStatus.null;
   F_Main.A_System_Start.Enabled := true;
   F_Main.A_System_Stop.Enabled := true;
  end;

 TrakceI.opening := false;
 Self.LogStatus('ERR: Trakce OPEN FAIL: '+errMsg);
 Application.MessageBox(PChar('Při otevírání Trakce nastala chyba:'+#13#10+errMsg), 'Chyba', MB_OK OR MB_ICONWARNING);
end;

procedure TF_Main.CB_centrala_loglevel_fileChange(Sender: TObject);
begin
 TrakceI.logLevelFile := TTrkLogLevel(Self.CB_centrala_loglevel_file.ItemIndex);
end;

procedure TF_Main.CB_centrala_loglevel_tableChange(Sender: TObject);
begin
 TrakceI.logLevelTable := TTrkLogLevel(Self.CB_centrala_loglevel_table.ItemIndex);
end;

procedure TF_Main.OnTrkStatusChange(Sender: TObject; trkStatus: TTrkStatus);
begin
 if (trkStatus = TTrkStatus.tsOn) then
  begin
   //je DCC
   TrakceI.DCCGoTime := Now;
   F_Main.S_DCC.Brush.Color := clLime;
   Self.LogStatus('DCC: go');

   if (TrakceI.ConnectedSafe()) then
    begin
     Self.A_DCC_Go.Enabled := false;
     Self.A_DCC_Stop.Enabled := true;
    end;

   if ((SystemData.Status = starting) and (TrakceI.ConnectedSafe())) then
     Self.A_Locos_AcquireExecute(nil);

   ORTCPServer.DCCStart();
  end else begin

   ORs.BroadcastPlaySound(_SND_CHYBA, false, TORControlRights.write);

   //neni DCC
   Self.S_DCC.Brush.Color := clRed;
   Self.LogStatus('DCC: stop');

   if (TrakceI.ConnectedSafe()) then
    begin
     Self.A_DCC_Go.Enabled := true;
     Self.A_DCC_Stop.Enabled := false;
    end;
   if ((SystemData.Status = starting) and (TrakceI.ConnectedSafe())) then
     Self.A_DCC_GoExecute(Self);

   ORTCPServer.DCCStop();
  end;//else state
end;

procedure TF_Main.A_Turnoff_FunctionsExecute(Sender: TObject);
begin
 Self.LogStatus('Vypínám zvuky hnacích vozidel...');
 Application.ProcessMessages();
 TrakceI.TurnOffSound(TTrakce.Callback(Self.OnTrkAllFunctionTurnedOff),
                      TTrakce.Callback(Self.OnTrkAllFunctionTurnedOff));
end;

procedure TF_Main.OnTrkAllFunctionTurnedOff(Sender: TObject; data: Pointer);
begin
 Self.LogStatus('Zvuky všech hnacích vozidel vypnuty');
 Application.ProcessMessages();
 Self.A_Locos_ReleaseExecute(Self);
end;

////////////////////////////////////////////////////////////////////////////////
// TRAKCE END
////////////////////////////////////////////////////////////////////////////////

procedure TF_Main.T_MainTimer(Sender: TObject);
 begin
  try
    SS.Update();
    DetekujAutSpusteniSystemu;
    Blocks.Update();
    VypisDatumCas();
    ModCas.Update();
    JCDb.Update();
    MultiJCDb.Update();
    Boosters.Update();
    ORs.Update();
    UpdateCallMethod();
    RCSd.Update();
    TrakceI.Update();
    ABlist.Update();
  except
   on E: Exception do
    begin
     if (not log_err_flag) then
       AppEvents.LogException(E, 'Main timer exception');
    end;
  end;
 end;

procedure TF_Main.PM_TesterClick(Sender: TObject);
 begin
  F_Tester.Show;
 end;

procedure TF_Main.P_Time_modelovyDblClick(Sender: TObject);
begin
 ModCas.started := not ModCas.started;
end;

procedure TF_Main.P_ZrychleniDblClick(Sender: TObject);
begin
 F_ModCasSet.OpenForm();
end;

procedure TF_Main.PM_NastaveniClick(Sender: TObject);
 begin
  F_Options.Show;
 end;

procedure TF_Main.PM_PropertiesClick(Sender: TObject);
begin
 if (LV_HV.Selected <> nil) then
   F_HVEdit.OpenForm(HVDB[Integer(LV_HV.Selected.Data)]);
end;

procedure TF_Main.PC_1Change(Sender: TObject);
begin
 Self.DisableRemoveButtons();

 if (PC_1.ActivePage = TS_VC)         then JCTableData.UpdateTable;
 if (PC_1.ActivePage = TS_MultiJC)    then MultiJCTableData.UpdateTable;
 if (PC_1.ActivePage = TS_Users)      then UsersTableData.UpdateTable;
 if (PC_1.ActivePage = TS_Bloky)      then BlokyTableData.UpdateTable();
 if (PC_1.ActivePage = TS_Zesilovace) then ZesTableData.UpdateTable();
 if (PC_1.ActivePage = TS_Soupravy)   then TrainTableData.UpdateTable();
 if (PC_1.ActivePage = F_Main.TS_HV)  then HVTableData.UpdateTable();
 if (PC_1.ActivePage = TS_Stanice)    then ORsTableData.UpdateTable(true);
 if (PC_1.ActivePage = TS_Technologie) then ORTCPServer.GUIRefreshTable(); 
end;

procedure TF_Main.PM_BlokyPopup(Sender: TObject);
var i: Integer;
begin
 if (Self.LV_Bloky.Selected = nil) then
  begin
   for i := 0 to (Sender as TPopUpMenu).Items.Count-1 do
    (Sender as TPopUpMenu).Items.Items[i].Enabled := false;
  end else begin
   for i := 0 to (Sender as TPopUpMenu).Items.Count-1 do
    (Sender as TPopUpMenu).Items.Items[i].Enabled := true;
  end;
end;

procedure TF_Main.PM_ResetVClick(Sender: TObject);
 begin
  if (Application.MessageBox('Pozor: tato operace zaráz přestaví všechny výhybky na kolejišti, '+
                             'což může způsobit přetížení napájecích zdrojů. Chcete skutečně pokračovat?', 'Otázka',
                             MB_YESNO OR MB_ICONWARNING OR MB_DEFBUTTON2) = mrYes) then
   begin
    Blocks.ZakladniPolohaVyhybek();
    writelog('Vyhýbky přestaveny do základní polohy', WR_MESSAGE);
    Application.MessageBox('Výhybky přestaveny do záklaních poloh.', 'Informace', MB_OK OR MB_ICONINFORMATION);
   end;
 end;

procedure TF_Main.PM_RegulatorClick(Sender: TObject);
begin
 if (Self.LV_HV.Selected = nil) then Exit;

 if (TrakceI.ConnectedSafe()) then
  begin
   try
    RegCollector.Open(HVDb[StrToInt(Self.LV_HV.Selected.Caption)]);
   except
    on E: Exception do
      Application.MessageBox(PChar(E.Message), 'Varování', MB_OK OR MB_ICONWARNING);
   end;
  end;//if
end;

procedure TF_Main.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var ci: TCloseInfo;
 begin
  if (Self.NUZClose) then
   begin
    CanClose := true;
    Exit();
   end;
  ci := GetFunctions.CanClose();
  if (Integer(ci) > 0) then CanClose := false;

  case (ci) of
    TCloseInfo.ci_system_changing : begin
      writelog('Pokus o zavření okna při zapínání nebo vypínání systémů', WR_ERROR);
      Application.MessageBox(PChar('Technologie právě zapíná nebo vypíná systémy, aplikaci nelze momentálně zavřít.'+
              #13#10+'Nouzové ukončení programu lze provést spuštěním příkazu "app-exit" v konzoli')
              , 'Nelze ukončit program', MB_OK OR MB_ICONWARNING);
    end;

    TCloseInfo.ci_system_started : begin
      writelog('Pokus o zavření okna bez ukončení komunikace se systémy', WR_ERROR);
      if (Application.MessageBox('Program není odpojen od systémů, odpojit od systémů?',
        'Nelze ukončit program', MB_YESNO OR MB_ICONWARNING) = mrYes) then
          F_Main.A_System_StopExecute(Self);
    end;

    TCloseInfo.ci_rcs : begin
      writelog('Pokus o zavření okna bez uzavření RCS', WR_ERROR);
      if (Application.MessageBox('Program není odpojen od RCS, odpojit?',
          'Nelze ukončit program', MB_YESNO OR MB_ICONWARNING) = mrYes) then
       begin
        try
          if (RCSi.Started) then RCSi.Stop()
          else if (RCSi.Opened) then RCSi.Close();
        except
          on E: Exception do
            Application.MessageBox(PChar('Nastala výjimka : ' + E.Message), 'Chyba', MB_OK OR MB_ICONERROR);
        end;
       end;
    end;

    TCloseInfo.ci_server : begin
      writelog('Pokus o zavření okna bez vypnutí panel serveru', WR_ERROR);
      if (Application.MessageBox('PanelServer stále běží, vypnout?',
          'Nelze ukončit program', MB_YESNO OR MB_ICONWARNING) = mrYes) then
       ORTCPServer.Stop();
    end;

    TCloseInfo.ci_trakce : begin
      writelog('Pokus o zavření okna bez odpojení od centrály', WR_ERROR);
      if (Application.MessageBox('Program není odpojen od centrály, odpojit?',
          'Nelze ukončit program', MB_YESNO OR MB_ICONWARNING) = mrYes) then
        TrakceI.Disconnect();
    end;

    TCloseInfo.ci_yes : begin
      if (Self.CloseMessage) then
       begin
        CanClose := (Application.Messagebox('Opravdu chcete ukončit program?', 'hJOPserver',
            MB_YESNO OR MB_ICONQUESTION OR MB_DEFBUTTON2) = mrYES);
       end else begin
        CanClose := true;
       end;
    end;

  end;//case
 end;

procedure TF_Main.FormCreate(Sender: TObject);
begin
 Self.CloseMessage := true;
 Self.mCpuLoad := TCPuLoad.Create();

 JCTableData := TJCTableData.Create(Self.LV_JC);
 ABTableData := TABTableData.Create(Self.LV_AB);
 UsersTableData := TUsersTableData.Create(Self.LV_Users);
 RCSTableData := TRCSTableData.Create(Self.LV_Stav_RCS);
 TrainTableData := TTrainTableData.Create(Self.LV_Soupravy);
 HVTableData := THVTableData.Create(Self.LV_HV);
 ZesTableData := TZesTableData.Create(Self.LV_Zesilovace);
 ORsTableData := TORsTableData.Create(Self.LV_Stanice);
 MultiJCTableData := TMultiJCTableData.Create(Self.LV_MultiJC);

 // assign RCS events:
 RCSi.AfterOpen  := Self.OnRCSOpen;
 RCSi.AfterClose := Self.OnRCSClose;
 RCSi.AfterStart := Self.OnRCSStart;
 RCSi.AfterStop  := Self.OnRCSStop;
 RCSi.OnScanned  := Self.OnRCSScanned;
 RCSi.OnReady    := Self.ONRCSReady;

 // assign Trakce events:
 TrakceI.BeforeOpen := Self.OnTrkBeforeOpen;
 TrakceI.AfterOpen := Self.OnTrkAfterOpen;
 TrakceI.BeforeClose := Self.OnTrkBeforeClose;
 TrakceI.AfterClose := Self.OnTrkAfterClose;
 TrakceI.OnReady := Self.OnTrkReady;
 TrakceI.OnTrackStatusChanged := Self.OnTrkStatusChange;
 TrakceI.OnOpenError := Self.OnTrkErrOpen;

 TrakceI.LogObj := Self.LV_log_lnet;

 FuncsFyznam.OnChange := Self.OnFuncsVyznamChange;

 Self.LoadIniLibData();

 F_Main.Caption := 'hJOPserver – v'+NactiVerzi(Application.ExeName)+' (build '+GetLastBuildDate+')';
 F_Main.SB1.Panels.Items[_SB_RCS].Text := 'RCS zavřeno';
end;

procedure TF_Main.FormDestroy(Sender: TObject);
begin
 Self.mCpuLoad.Free();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_Main.AE_1Message(var Msg: tagMSG; var Handled: Boolean);
begin
 Handled := false;

 if (Msg.Message = runningMsg) then
  begin
   Application.Restore;
   SetForeGroundWindow(F_Main.Handle);
   Handled := true;
  end;

 // STISK KLAVESY
 case (msg.message) of
   WM_KEYDOWN: begin
       Handled := false;
       RegCollector.KeyPress(msg.wParam, Handled);
       if (Handled) then Exit;

       case (msg.wParam) of
         VK_F9: begin
            try
              RCSi.HideConfigDialog();
            except
              on E: Exception do
                Application.MessageBox(PChar('Nelze skrýt konfigurační dialog RCS : ' + E.Message), 'Varování', MB_OK OR MB_ICONWARNING);
            end;
         end;

         VK_ESCAPE: if (F_About.Showing) then F_About.Close;

         VK_F4: begin
            // zobrazeni debug okna
            F_Admin.Show();
            Handled := true;
         end;
        end;//case
   end;
 end;
end;

procedure TF_Main.WMPowerBroadcast(var Msg: TMessage);
begin
 case (msg.WParam) of
    PBT_APMQUERYSUSPEND: begin
       msg.Result := BROADCAST_QUERY_DENY;
     end;

    PBT_APMSUSPEND: begin
       // windows is going to sleep -> disconnect all devices
       if (TrakceI.ConnectedSafe()) then
        begin
         ORTCPServer.Stop();
         try
           TrakceI.EmergencyStop();
           TrakceI.Disconnect();
         except

         end;
        end;

       try
         if (RCSi.Started) then RCSi.Stop();
         if (RCSi.Opened) then RCSi.Close();
       except

       end;
     end;

 end;//case
end;

procedure TF_Main.WMQueryEndSession(var Msg: TWMQueryEndSession);
begin
 if (GetFunctions.CanClose() <> ci_yes) then
  begin
   Msg.Result := 0;
 end else begin
   Msg.Result := 1;
   Self.CloseMessage := false;
   Self.NUZClose := true;
 end;
 inherited;
end;

procedure TF_Main.WMEndSession(var Msg: TWMEndSession);
begin
 if (Msg.EndSession = True) then
  begin
   if (TrakceI.ConnectedSafe()) then
    begin
     ORTCPServer.Stop();
     try
       TrakceI.EmergencyStop();
       TrakceI.Disconnect();
     except

     end;
    end;

   try
     if (RCSi.Started) then RCSi.Stop();
     if (RCSi.Opened) then RCSi.Close();
   except

   end;

   Self.CloseMessage := false;
   Self.NUZClose := true;
   F_Main.Close();
  end;
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_Main.A_PanelServer_StartExecute(Sender: TObject);
begin
 if ((SystemData.Status = starting) and (not Blocks.enabled)) then
   Blocks.Enable();

 try
   ORTCPServer.Start();
   Self.A_PanelServer_Start.Enabled := false;
   Self.A_PanelServer_Stop.Enabled  := true;
   Self.UpdateSystemButtons();
 except
   on E : Exception do
    begin
     SystemData.Status := TSystemStatus.null;
     Self.UpdateSystemButtons();
     Application.MessageBox(PChar('Chyba při zapínání serveru:'+#13#10+E.Message), 'Chyba', MB_OK OR MB_ICONWARNING);
     Exit();
    end;
 end;
end;

procedure TF_Main.A_PanelServer_StopExecute(Sender: TObject);
begin
 ORTCPServer.Stop();

 Self.A_PanelServer_Start.Enabled := true;
 Self.A_PanelServer_Stop.Enabled  := false;
 Self.UpdateSystemButtons();
end;

procedure TF_Main.A_PT_StartExecute(Sender: TObject);
begin
 if (SystemData.Status = TSystemStatus.starting) then
   Self.LogStatus('PT server: spouštění...');

 try
   PtServer.Start();
   if (SystemData.Status = TSystemStatus.starting) then
    begin
     Self.LogStatus('PT server: spuštěn');
     Self.LogStatus('System: start OK');
    end;
 except
   on E: Exception do
     Application.MessageBox(PChar('Nelze nastartovat PT server:'+#13#10+E.Message), 'Chyba', MB_OK OR MB_ICONWARNING);
 end;

 if (SystemData.Status = TSystemStatus.starting) then
   SystemData.Status := null;
 Self.UpdateSystemButtons();
end;

procedure TF_Main.A_PT_StopExecute(Sender: TObject);
begin
 if (SystemData.Status = TSystemStatus.stopping) then
   Self.LogStatus('PT server: vypínání...');

 try
   PtServer.Stop();
 except
   on E: Exception do
     Application.MessageBox(PChar('Nelze zastavit PT server:'+#13#10+E.Message), 'Chyba', MB_OK OR MB_ICONWARNING);
 end;

 Self.UpdateSystemButtons();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_Main.A_SaveStavExecute(Sender: TObject);
var ini: TMemIniFile;
begin
  try
    // ukladani stavu bloku: ulozime do docasneho souboru a az pak prepiseme stavajici konfigurak
    Blocks.SaveStatToFile(Blocks.fstatus+'_');

    if (FileExists(Blocks.fstatus)) then
      DeleteFile(Blocks.fstatus);
    MoveFile(PChar(Blocks.fstatus+'_'), PChar(Blocks.fstatus));
    DeleteFile(Blocks.fstatus+'_');
  except
    on E: Exception do
      AppEvents.LogException(E, 'Blocks.SaveStatToFile');
  end;

  try
    HVDb.SaveState(F_Main.E_dataload_HV_state.Text+'_');

    if (FileExists(F_Main.E_dataload_HV_state.Text)) then
      DeleteFile(F_Main.E_dataload_HV_state.Text);
    MoveFile(PChar(F_Main.E_dataload_HV_state.Text+'_'), PChar(F_Main.E_dataload_HV_state.Text));
    DeleteFile(F_Main.E_dataload_HV_state.Text+'_');
  except
    on E: Exception do
      AppEvents.LogException(E, 'HvDb.SaveToDir');
  end;

  try
    Trains.SaveData(F_Main.E_dataload_soupr.Text+'_');

    if (FileExists(F_Main.E_dataload_soupr.Text)) then
      DeleteFile(F_Main.E_dataload_soupr.Text);
    MoveFile(PChar(F_Main.E_dataload_soupr.Text+'_'), PChar(F_Main.E_dataload_soupr.Text));
    DeleteFile(F_Main.E_dataload_soupr.Text+'_');
  except
    on E: Exception do
      AppEvents.LogException(E, 'Trains.SaveData');
  end;

  try
    F_Main.SaveFormPosition;
    FormData.SaveFormData(FormData.aFile);
  except
    on E: Exception do
      AppEvents.LogException(E, 'Save form position');
  end;

  try
    ORs.SaveStatus(ORs.status_filename);
  except
    on E: Exception do
      AppEvents.LogException(E, 'Save OR status');
  end;

  try
    ini := TMeminifile.Create(_INIDATA_FN, TEncoding.UTF8);
    try
      try
        RCSi.SaveToFile(ini);
      except
        on E: Exception do
          AppEvents.LogException(E, 'Save RCS');
      end;

      try
        TrakceI.SaveToFile(ini);
      except
        on E: Exception do
          AppEvents.LogException(E, 'Save Trakce');
      end;

      ini.WriteBool('Log', 'main-file', Self.CHB_Mainlog_File.Checked);
      ini.WriteBool('Log', 'main-table', Self.CHB_Mainlog_Table.Checked);
      ini.WriteBool('Log', 'rcs', Self.CHB_rcslog.Checked);
      ini.WriteBool('Log', 'auth', Self.CHB_Log_Auth.Checked);

    finally
      ini.UpdateFile();
      ini.Free();
    end;
  except
    on E: Exception do
      AppEvents.LogException(E, 'Save cfg');
  end;

  try
    ini := TMemIniFile.Create(ExtractRelativePath(ExtractFilePath(Application.ExeName), F_Options.E_dataload.Text), TEncoding.UTF8);
    try
      ModCas.SaveData(ini);
      ini.WriteString('funcsVyznam', 'funcsVyznam', FuncsFyznam.GetFuncsVyznam());
      ini.WriteBool('RCS', 'ShowOnlyActive', Self.CHB_RCS_Show_Only_Active.Checked);
    finally
      ini.UpdateFile();
      ini.Free();
    end;
  except
    on E: Exception do
      AppEvents.LogException(E, 'Save cfg');
  end;
end;

procedure TF_Main.A_System_StartExecute(Sender: TObject);
begin
 Self.LB_Log.Items.Insert(0, '--------------------------------------------------------------------------------');

 if (not RCSi.ready) then
  begin
   Application.MessageBox(PChar('Systém nelze spustit, RCS není připraveno k zapnutí systému'+#13#10+'Možné příčiny:'+#13#10+' - nenačtena validní knihovna'), 'Nelze spustit', MB_OK OR MB_ICONWARNING);
   Self.LogStatus('ERR: Systém nelze spustit, RCS není připraveno k zapnutí systému');
   Exit();
  end;
 if (not TrakceI.ready) then
  begin
   Application.MessageBox(PChar('Systém nelze spustit, Trakce není připravena k zapnutí systému'+#13#10+'Možné příčiny:'+#13#10+' - nenačtena validní knihovna'), 'Nelze spustit', MB_OK OR MB_ICONWARNING);
   Self.LogStatus('ERR: Systém nelze spustit, Trakce není připravena k zapnutí systému');
   Exit();
  end;

 Self.LogStatus('Zapínám systémy...');
 SystemData.Status := starting;
 Self.A_System_Start.Enabled := false;
 Self.A_RCS_OpenExecute(nil);
end;

procedure TF_Main.A_System_StopExecute(Sender: TObject);
begin
 Self.A_System_Stop.Enabled := false;

 Self.LB_Log.Items.Insert(0, '--------------------------------------------------------------------------------');
 Self.LogStatus('Vypínám systémy...');
 SystemData.Status := stopping;

 Self.LogStatus('Zastavuji všechny Trains...');
 Trains.StopAllTrains();

 Application.ProcessMessages();

 if (PtServer.openned) then Self.A_PT_StopExecute(nil);

 Self.LogStatus('Odpojuji panely...');
 ORs.DisconnectPanels();
 Self.A_PanelServer_StopExecute(nil);

 JCDb.RusAllJC();
 Blocks.Disable();
 Trains.ClearPOdj();
 Blocks.Reset();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_Main.SetCallMethod(Method: TNotifyEvent);
begin
 while (Assigned(Self.call_method)) do
  begin
   Application.ProcessMessages();
   sleep(1);
  end;
 Self.call_method := method;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_Main.B_AB_DeleteClick(Sender: TObject);
var blk: TBlk;
    jc: TJC;
begin
 jc := ABlist[Self.LV_AB.ItemIndex];
 if ((Self.LV_AB.Selected <> nil) and (Application.MessageBox(PChar('Opravdu smazat jízdní cestu '+jc.name+'?'), 'Opravdu?', MB_YESNO OR MB_ICONQUESTION) = mrYes)) then
  begin
   try
     Blocks.GetBlkByID(jc.data.NavestidloBlok, blk);
     if ((blk <> nil) and (Blk.typ = btSignal) and (TBlkSignal(blk).ABJC = jc)) then
      begin
       TBlkSignal(blk).ABJC := nil;
       if (ABlist.Contains(jc)) then
         ABlist.Remove(ABlist[Self.LV_AB.ItemIndex]);
      end else
       ABlist.Remove(ABlist[Self.LV_AB.ItemIndex]);
   except
    on E: Exception do
      Application.MessageBox(PChar('Chyba při mazání:'+#13#10+E.Message), 'Chyba', MB_OK OR MB_ICONERROR);
   end;
  end;
end;

procedure TF_Main.B_BlkAddClick(Sender: TObject);
begin
 F_BlkNew.OpenForm;
end;

procedure TF_Main.B_BlkDeleteClick(Sender: TObject);
var pozice: Integer;
 begin
  Pozice := LV_Bloky.ItemIndex;

  Beep;
  if Application.MessageBox(PChar('Opravdu chcete smazazat blok '+Blocks.GetBlkIndexName(pozice)+'?'),
                            'Mazání bloku', MB_YESNO OR MB_ICONQUESTION OR MB_DEFBUTTON2) = mrYes then
   begin
    try
      Blocks.Delete(pozice);
    except
      on E: Exception do
        Application.MessageBox(PChar('Chyba:'+#13#10+E.Message), 'Chyba', MB_OK OR MB_ICONWARNING);
    end;
   end;//if MesageBox
end;

procedure TF_Main.B_ChangeClick(Sender: TObject);
var data: string;
    i: Integer;
begin
 data := '';
 for i := 0 to Self.M_funcsVyznam.Lines.Count-1 do
   if (Self.M_funcsVyznam.Lines[i] <> '') then
     data := data + '{' + Self.M_funcsVyznam.Lines[i] + '};';
 FuncsFyznam.ParseWholeList(data);
 ORTCPServer.BroadcastFuncsVyznam();
end;

procedure TF_Main.B_ClearStatsClick(Sender: TObject);
begin
 if (Application.MessageBox('Opravdu smazat najeté bloky a kilometry všech hnacích vozidel?',
                            'Opravdu?', MB_YESNO OR MB_ICONQUESTION OR MB_DEFBUTTON2) = mrYes) then
   HVDb.ClearAllStatistics();
end;

procedure TF_Main.OnFuncsVyznamChange(Sender: TObject);
var vyzn: TFuncVyznam;
    strs: TStrings;
begin
 if (not Self.CHB_LoadChanges.Checked) then Exit();
 Self.M_funcsVyznam.Clear();
 for vyzn in FuncsFyznam.Items do
   Self.M_funcsVyznam.Lines.Add(vyzn.GetPanelStr());

 strs := TStringList.Create();
 try
   for vyzn in FuncsFyznam.Items do
     strs.Add(vyzn.popis);
   F_FuncsSet.UpdateFuncsList(strs);
 finally
   strs.Free();
 end;
end;

procedure TF_Main.B_HVStats_ExportClick(Sender: TObject);
var fn: string;
begin
 if (Self.SD_HV_Stats.Execute(Self.Handle)) then
  begin
   try
    if (RightStr(Self.SD_HV_Stats.FileName, 4) <> '.csv') then
      fn := Self.SD_HV_Stats.FileName + '.csv'
    else
      fn := Self.SD_HV_Stats.FileName;
    HVDb.ExportStatistics(fn);
   except
    on E: Exception do
      Application.MessageBox(PChar('Nelze exportovat'+#13#10+E.Message), 'Chyba', MB_OK OR MB_ICONERROR);
   end;
  end;
end;

procedure TF_Main.B_HV_AddClick(Sender: TObject);
begin
 F_HVEdit.NewHV();
end;

procedure TF_Main.B_HV_DeleteClick(Sender: TObject);
var hvs: string;
    LI: TListItem;
    i: Integer;
    addr: Word;
begin
 if (Self.LV_HV.Selected = nil) then Exit();

 hvs := Self.LVSelectedTexts(Self.LV_HV, 'HV', 'HV');

 if (Application.MessageBox(PChar('Opravdu smazat '+hvs+'?'), '?', MB_YESNO OR MB_ICONQUESTION) = mrYes) then
  begin
   for i := Self.LV_HV.Items.Count-1 downto 0 do
    begin
     LI := Self.LV_HV.Items[i];
     if (LI.Selected) then
      begin
       addr := Integer(LI.Data);
       try
         HVDb.Remove(addr);
       except
         on E: Exception do
          begin
           Application.MessageBox(PChar('Mazání HV '+IntToStr(addr)+' se nezdařilo:'+#13#10+E.Message),
                                  'Chyba', MB_OK OR MB_ICONWARNING);
           Exit();
          end;
       end;
      end;
    end;
  end;
end;

procedure TF_Main.B_JC_ResetClick(Sender: TObject);
var JC: TJC;
begin
 if (Self.LV_JC.Selected = nil) then Exit();

 JC := JCDb.GetJCByIndex(Self.LV_JC.ItemIndex);
 if (JC.staveni) then
   JC.CancelStaveni('Nouzové rušení stavění JC', true);
end;

procedure TF_Main.B_lok_deleteClick(Sender: TObject);
var sprs: string;
    LI: TListItem;
    i: Integer;
begin
 if (Self.LV_Soupravy.Selected = nil) then Exit();
 if (not Assigned(Trains[Self.LV_Soupravy.ItemIndex])) then Exit();

 sprs := Self.LVSelectedTexts(Self.LV_Soupravy, 'soupravu', 'soupravy');

 if (Application.MessageBox(PChar('Opravdu smazat '+sprs+'?'), '?', MB_YESNO OR MB_ICONQUESTION) = mrYes) then
  begin
   for i := Self.LV_Soupravy.Items.Count-1 downto 0 do
    begin
     LI := Self.LV_Soupravy.Items[i];
     if ((LI.Selected) and (LI.Caption <> '')) then
       Trains.Remove(LI.Index);
    end;
  end;
end;

procedure TF_Main.B_mJC_AddClick(Sender: TObject);
begin
  if ((Self.LV_MultiJC.Selected <> nil) and
      (Application.MessageBox(PChar('Chcete použít složenou JC ' + MultiJCDb[Self.LV_MultiJC.ItemIndex].nazev + ' jako šablonu pro vytvoření nové složené JC?'),
                              'Nová složená JC', MB_YESNO OR MB_ICONQUESTION OR MB_DEFBUTTON1) = mrYes)) then
    F_MJCEdit.NewMJC(MultiJCDb[Self.LV_MultiJC.ItemIndex])
  else
    F_MJCEdit.NewMJC(nil);
end;

procedure TF_Main.B_mJC_RemoveClick(Sender: TObject);
var mjcs: string;
    LI: TListItem;
    i: Integer;
begin
 if (Self.LV_MultiJC.Selected = nil) then Exit();

 mjcs := Self.LVSelectedTexts(Self.LV_MultiJC, 'cestu', 'cesty');

 if (Application.MessageBox(PChar('Opravdu smazat '+mjcs+'?'), '?', MB_YESNO OR MB_ICONQUESTION) = mrYes) then
  begin
   for i := Self.LV_MultiJC.Items.Count-1 downto 0 do
    begin
     LI := Self.LV_MultiJC.Items[i];
     if (LI.Selected) then
       MultiJCDb.Remove(LI.Index);
    end;
  end;
end;

procedure TF_Main.B_RemoveStackClick(Sender: TObject);
var OblR: TOR;
begin
 if (Self.LV_Stanice.Selected = nil) then Exit();
 OblR := ORs[Self.LV_Stanice.ItemIndex];
 if (Application.MessageBox(PChar('Opravdu smazat zásobník jízdních cest stanice '+OblR.Name+' ?'),
                            'Opravdu?', MB_YESNO OR MB_ICONQUESTION) = mrYes) then
   OblR.stack.ClearStack();
end;

procedure TF_Main.B_User_AddClick(Sender: TObject);
begin
 F_UserEdit.NewUser();
end;

procedure TF_Main.B_User_DeleteClick(Sender: TObject);
begin
 if (Application.MessageBox(PChar('Opravdu smazat uživatele '+Self.LV_Users.Selected.SubItems[0]+' ?'),
                            'Opravdu?', MB_YESNO OR MB_ICONQUESTION) = mrYes) then
  begin
   UsrDB.RemoveUser(Self.LV_Users.ItemIndex);
   Self.B_User_Delete.Enabled := false;
  end;
end;

procedure TF_Main.B_VC_AddClick(Sender: TObject);
begin
  if ((Self.LV_JC.Selected <> nil) and
      (Application.MessageBox(PChar('Chcete použít JC ' + JCDb[Self.LV_JC.ItemIndex].name + ' jako šablonu pro vytvoření nové JC?'),
                              'Nová JC', MB_YESNO OR MB_ICONQUESTION OR MB_DEFBUTTON1) = mrYes)) then
    F_JCEdit.NewJC(Self.LV_JC.ItemIndex)
  else
    F_JCEdit.NewJC(-1);
end;

procedure TF_Main.B_VC_deleteClick(Sender: TObject);
var jcs: string;
    LI: TListItem;
    i: Integer;
    jc: TJC;
begin
 if (Self.LV_JC.Selected = nil) then Exit();

 jcs := Self.LVSelectedTexts(Self.LV_JC, 'cestu', 'cesty');

 if (Application.MessageBox(PChar('Opravdu smazat '+jcs+'?'), '?', MB_YESNO OR MB_ICONQUESTION) = mrYes) then
  begin
   for i := Self.LV_JC.Items.Count-1 downto 0 do
    begin
     LI := Self.LV_JC.Items[i];
     if (LI.Selected) then
      begin
       try
         jc := JCDb.GetJCByIndex(LI.Index);
         if (ABlist.Contains(jc)) then
           ABlist.Remove(jc);
         JCDb.RemoveJC(LI.Index);
       except
         on E: Exception do
          begin
           Application.MessageBox(PChar('Nelze smazat JC'+#13#10+E.Message), 'Chyba', MB_OK OR MB_ICONERROR);
           Exit();
          end;
       end;
      end;
    end;
  end;
end;

procedure TF_Main.B_zes_addClick(Sender: TObject);
begin
  F_ZesilovacEdit.NewZes();
end;

procedure TF_Main.B_zes_deleteClick(Sender: TObject);
var pozice: integer;
begin
 Pozice := LV_Zesilovace.ItemIndex;
 Beep;
 if Application.MessageBox(PChar('Opravdu chcete smazat zesilovač '+Boosters.sorted[Pozice].name+'?'),
                           'Mazání zesilovace', MB_YESNO OR MB_ICONQUESTION OR MB_DEFBUTTON2) = mrYes then
  begin
   Boosters.Remove(Boosters.sorted[Pozice].id);
   LV_Zesilovace.Items.Delete(Pozice);
  end;//if MessageBox
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_Main.PM_Help_RPClick(Sender: TObject);
 begin
  F_About.ShowModal;
 end;

procedure TF_Main.PM_HVPopup(Sender: TObject);
var i: Integer;
begin
 if (Self.LV_HV.Selected = nil) then
  begin
   for i := 0 to (Sender as TPopUpMenu).Items.Count-1 do
    (Sender as TPopUpMenu).Items.Items[i].Enabled := false;
  end else begin
   for i := 0 to (Sender as TPopUpMenu).Items.Count-1 do
    (Sender as TPopUpMenu).Items.Items[i].Enabled := true;
  end;
end;

procedure TF_Main.PM_SB1Click(Sender: TObject);
begin
 if PM_SB1.Checked then
  begin
   SB1.Visible:=true;
   writelog('Zobrazeno SB1', WR_MESSAGE);
  end else begin
   SB1.Visible:=false;
   writelog('Skryto SB1', WR_MESSAGE);
  end;
end;

procedure TF_Main.T_GUI_refreshTimer(Sender: TObject);
begin
 try
   mCpuLoad.Refresh();

   // update tables
   if (Self.Showing) then
    begin
     if (F_Main.PC_1.ActivePage = F_Main.TS_Bloky) then BlokyTableData.UpdateTable();
     if (F_Main.PC_1.ActivePage = F_Main.TS_Soupravy) then TrainTableData.UpdateTable();
     if (F_Main.PC_1.ActivePage = F_Main.TS_Zesilovace) then ZesTableData.UpdateTable();
     if (F_Main.PC_1.ActivePage = F_Main.TS_HV) then HVTableData.UpdateTable();
     if (F_Main.PC_1.ActivePage = F_Main.TS_VC) then JCTableData.UpdateTable();
     if (F_Main.PC_1.ActivePage = F_Main.TS_MultiJC) then MultiJCTableData.UpdateTable();
     if (F_Main.PC_1.ActivePage = F_Main.TS_Stanice) then ORsTableData.UpdateTable();
     if (F_Main.PC_1.ActivePage = F_Main.TS_Technologie) then ORTCPServer.GUIRefreshFromQueue();

     ABTableData.Update();
    end;

    HVDb.UpdateTokenTimeout();
    Config.UpdateAutosave();
  except
   on E: Exception do
    begin
     if (not log_err_flag) then
       AppEvents.LogException(E, 'Function timer exception');
    end;
  end;
end;

procedure TF_Main.T_clear_log_msgTimer(Sender: TObject);
begin
 if (Self.sb1Log) then                                              //zapis do SB1 - cekani 0,5 s
  begin
   F_Main.SB1.Panels.Items[_SB_LOG].Text := '';
   Self.sb1Log := false;
  end;
end;

procedure TF_Main.CHB_Log_AuthClick(Sender: TObject);
begin
 Logging.auth_logging := Self.CHB_Log_Auth.Checked;
end;

procedure TF_Main.CHB_rcslogClick(Sender: TObject);
begin
 RCSi.log := Self.CHB_rcslog.Checked;
end;

procedure TF_Main.CHB_RCS_Show_Only_ActiveClick(Sender: TObject);
begin
 if (RCSi.Lib <> '') then
   RCSTableData.LoadToTable(not Self.CHB_RCS_Show_Only_Active.Checked);
end;

procedure TF_Main.CloseForm();
 begin
  WriteLog('########## Probíhá ukončování hJOPserver ##########', WR_MESSAGE);

  Self.T_Main.Enabled := false;
  Self.T_GUI_refresh.Enabled := false;
  self.T_clear_log_msg.Enabled := false;
  JCSimulator.timer.Enabled := false;
  TratSimulator.timer.Enabled := false;
  VyhSimulator.timer.Enabled := false;

  Self.A_SaveStavExecute(Self);
  TrakceI.LogObj := nil;

  WriteLog('###############################################', WR_MESSAGE);
 end;

procedure TF_Main.RepaintObjects();
 begin
  SB1.Panels.Items[0].Width:=F_Main.ClientWidth-SB1.Panels.Items[1].Width-SB1.Panels.Items[2].Width-
  SB1.Panels.Items[3].Width-SB1.Panels.Items[4].Width-SB1.Panels.Items[5].Width;
  P_Zrychleni.Left:=F_Main.ClientWidth-P_Zrychleni.Width-5;
  P_Time_modelovy.Left:=P_Zrychleni.Left-P_Time_modelovy.Width-5;
  P_Time.Left:=P_Time_modelovy.Left-P_Time.Width-5;
  P_Date.Left:=P_Time.Left-P_Date.Width-5;

  GB_Connected_Panels.Height := TS_Technologie.Height - GB_Connected_Panels.Top - 10;

  GB_Connected_Panels.Width := TS_Technologie.ClientWidth - 2*GB_Connected_Panels.Left;
  GB_Log.Width := TS_Technologie.Width - GB_Log.Left - GB_stav_technologie.Left;
end;

procedure TF_Main.FormResize(Sender: TObject);
 begin
  RepaintObjects;
 end;

procedure TF_Main.FormShow(Sender: TObject);
begin
 Self.RepaintObjects();
end;

procedure TF_Main.L_DateDblClick(Sender: TObject);
begin
 Application.Messagebox('Datum a čas lze nastavit v operačním systému','Informace', MB_ICONINFORMATION OR MB_OK OR MB_DEFBUTTON1);
end;

procedure TF_Main.MI_DisconnectClick(Sender: TObject);
begin
 if (ORTCPServer.GetClient(F_Main.LV_Clients.ItemIndex) <> nil) then
  begin
   try
     ORTCPServer.DisconnectClient(ORTCPServer.GetClient(Self.LV_Clients.ItemIndex).conn);
   except
     on E: Exception do
       Application.MessageBox(PChar('Výjimka při odpojování - '+e.Message), 'Chyba', MB_OK OR MB_ICONWARNING);
   end;
  end;
end;

procedure TF_Main.MI_HoukClick(Sender: TObject);
var Blk: TBlk;
begin
 if (Self.LV_Bloky.Selected = nil) then Exit();
 if (Blocks.GetBlkByIndex(Self.LV_Bloky.ItemIndex, Blk) <> 0) then Exit();
 if ((Blk.typ <> btTrack) and (Blk.typ <> btRT)) then Exit();

 F_HoukEvsUsek.Open(TBlkTrack(Blk));
end;

procedure TF_Main.MI_PropClick(Sender: TObject);
begin
 if (Self.LV_Bloky.Selected <> nil) then
  Self.LV_BlokyDblClick(Self.LV_Bloky);
end;

procedure TF_Main.MI_RCS_UpdateClick(Sender: TObject);
begin
 try
   Self.UpdateRCSLibsList();
   Application.MessageBox('Seznam knihoven úspěšně aktualizován.', 'Info', MB_OK OR MB_ICONINFORMATION);
 except
   on E: Exception do
     Application.MessageBox(PChar('Seznam knihoven se nepodařilo aktualizovat:'+#13#10 + E.Message),
        'Chyba', MB_OK OR MB_ICONWARNING);
 end;
end;

procedure TF_Main.MI_Save_configClick(Sender: TObject);
var inidata: TMemIniFIle;
begin
  Application.ProcessMessages();
  Screen.Cursor := crHourGlass;

  try
    inidata := TMeminifile.Create(_INIDATA_FN, TEncoding.UTF8);
    try
      Config.CompleteSaveToFile(inidata);
    finally
      inidata.UpdateFile();
      inidata.Free();
    end;
  except
    on E: Exception do
     begin
      AppEvents.LogException(E, 'TF_Main.MI_Save_configClick');
      Application.MessageBox(PChar('Výjimka: '+E.Message), 'Výjimka', MB_OK OR MB_ICONERROR);
     end;
  end;

 Screen.Cursor := crDefault;
end;

procedure TF_Main.MI_TechPropClick(Sender: TObject);
var Blk: TBlk;
 begin
  if (LV_Bloky.Selected = nil) then Exit;
  if (Blocks.GetBlkByIndex(Self.LV_Bloky.ItemIndex, Blk) <> 0) then Exit;

  case (Blk.typ) of
   btTurnout: F_BlkVyh_tech.OpenForm(Blk as TBlkTurnout);
   btTrack, btRT :
                  F_BlkUsek_tech.OpenForm(Blk as TBlkTrack);
   btIR      : ;
   btSignal  : ;
   btCrossing : ;
   btRailway    : F_BlkTrat_tech.OpenForm(Blk as TBlkRailway);
   btLinker  : ;
  end;//case
end;

procedure TF_Main.LB_LogDblClick(Sender: TObject);
begin
 if (Application.MessageBox('Smazat obsah seznamu?', 'Smazat?', MB_YESNO OR MB_ICONQUESTION OR MB_DEFBUTTON2) = mrYes) then
   Self.LB_Log.Clear();
end;

procedure TF_Main.LB_LogDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
 with Control as TListBox do
  begin
   if (Copy(Items[Index], 12, 3) = 'ERR') then begin
      Canvas.Brush.Color := $AAAAFF;
      Canvas.Font.Color := clWhite;
   end else if (Copy(Items[Index], 12, 4) = 'WARN') then begin
      Canvas.Brush.Color := $AAFFFF;
      Canvas.Font.Color := clBlack;
   end else begin
      Canvas.Brush.Color := Color;
      Canvas.Font.Color := Font.Color;
   end;

     Canvas.FillRect(Rect);
     Canvas.TextRect(Rect, Rect.Left + 2, Rect.Top, Items[Index]);
  end;
end;

procedure TF_Main.LoadIniLibData();
var inidata: TMemIniFile;
 begin
  inidata := TMeminifile.Create(_INIDATA_FN, TEncoding.UTF8);
  try
    Self.CHB_Mainlog_File.Checked := inidata.ReadBool('Log', 'main-file', true);
    Self.CHB_Mainlog_Table.Checked := inidata.ReadBool('Log', 'main-table', true);
    Self.CHB_rcslog.Checked := inidata.ReadBool('Log', 'rcs', false);
    Self.CHB_Log_Auth.Checked := inidata.ReadBool('Log', 'auth', false);
    RCSi.log := Self.CHB_rcslog.Checked;
    Logging.auth_logging := Self.CHB_Log_Auth.Checked;
  finally
    inidata.Free();
  end;
 end;

procedure TF_Main.DetekujAutSpusteniSystemu();
 begin
  if (KomunikacePocitani <> 0) then
   begin
    if (not GetFunctions.GetSystemStart) then
     begin
      F_AutoStartSystems.L_Cas.Caption := FormatDateTime('ss', Now-KomunikaceGo);
      if (not F_AutoStartSystems.Showing) then
       begin
        WriteLog('Probiha automaticke pripojovani k systemum - t=6s', WR_MESSAGE);
        F_AutoStartSystems.Show;
       end;
      if (KomunikacePocitani = 1) then
       begin
        KomunikaceGo := Now + EncodeTime(0,0,6,0);
        KomunikacePocitani := 2;
       end else begin
        if (Round((Now - KomunikaceGo) * 24 * 3600) = 0) then
         begin
          WriteLog('Automaticke pripojovani k systemum - t=0 - zapinam systemy', WR_MESSAGE);
          F_AutoStartSystems.Close;
          KomunikacePocitani := 0;          
          F_Main.A_System_StartExecute(nil);
         end;
       end;//else not KomunikacePocitani
     end;//if (not GetFunctions.GetSystemStart) and ...
   end;//if KomunikacePocitani <> -1
 end;

procedure TF_Main.VypisDatumCas();
 begin
  P_Date.Caption := FormatDateTime('dd. mm. yyyy', Now);
  P_Time.Caption := FormatDateTime('hh:mm:ss', Now);
 end;

procedure TF_Main.OnStart();
 begin
  mCpuLoad.DrawCPUGauge();

  writelog('Spuštěn hJOPserver v'+NactiVerzi(application.ExeName), WR_MESSAGE);
  writelog('----------------------------------------------------------------', WR_MESSAGE);

  if (not Self.CloseMessage) then
   begin
    Self.Close();
    Exit();
   end;

  F_splash.PB_Prubeh.Position := F_splash.PB_Prubeh.Max;
  F_Splash.AddStav('Téměř spuštěno...');

  BlokyTableData.LoadTable();
  JCTableData.LoadToTable();
  RCSTableData.LoadToTable(not Self.CHB_RCS_Show_Only_Active.Checked);
  UsersTableData.LoadToTable();
  ORsTableData.LoadToTable();

  Self.PC_1.ActivePage := TS_Technologie;

  ORTCPServer.GUIInitTable();
  ModCas.UpdateGUIColors();

  Self.Visible := true;

  Self.T_Main.Enabled := true;
  Self.T_GUI_refresh.Enabled := true;
  Self.T_clear_log_msg.Enabled := true;

  Self.UpdateRCSLibsList();
  Self.UpdateTrkLibsList();

  Self.CB_centrala_loglevel_file.ItemIndex := Integer(TrakceI.logLevelFile);
  Self.CB_centrala_loglevel_table.ItemIndex := Integer(TrakceI.logLevelTable);

  if (TrakceI.Lib = '') then
    F_Main.SB1.Panels.Items[_SB_TRAKCE_LIB].Text := '-'
  else
    F_Main.SB1.Panels.Items[_SB_TRAKCE_LIB].Text := ExtractFileName(TrakceI.Lib);

  try
    if ((RCSi.ready) and (diag.simInputs) and (RCSi.simulation)) then
      RCSi.InputSim();
  except
    on E: Exception do
      writelog('Nelze provést inputSim : ' + E.Message, WR_ERROR);
  end;

  Self.S_PTServer.Visible := (GlobalConfig.ptAutoStart);
  Self.L_PTServer.Visible := (GlobalConfig.ptAutoStart);

  if (not Self.CloseMessage) then
   begin
    Self.Close();
    Exit();
   end;
 end;

procedure TF_Main.FormClose(Sender: TObject; var Action: TCloseAction);
 begin
  F_SystemInfo.OpenForm('Probíhá ukládání dat...');
  Application.ProcessMessages;
  CloseForm;
 end;

procedure TF_Main.SaveFormPosition();
var ini: TMemIniFile;
 begin
  ini := TMemIniFile.Create(F_Options.E_dataload.Text, TEncoding.UTF8);
  try
    case F_Main.WindowState of
     wsNormal   : ini.WriteInteger('Application', 'WState', 1);
     wsMaximized: ini.WriteInteger('Application', 'WState', 0);
    end;//case
    ini.WriteInteger('Application', 'Left', F_Main.Left);
    ini.WriteInteger('Application', 'Top', F_Main.Top);
    ini.WriteInteger('Application', 'Heigth', F_Main.Height);
    ini.WriteInteger('Application', 'Width', F_Main.Width);
  finally
    ini.UpdateFile();
    ini.Free();
  end;
 end;

procedure TF_Main.PM_SaveFormPosClick(Sender: TObject);
 begin
  F_Main.SaveFormPosition;
 end;

procedure TF_Main.FormPaint(Sender: TObject);
begin
 mCpuLoad.ResizeCPUGauge();
end;

procedure TF_Main.PM_ClientsPopup(Sender: TObject);
var i: Integer;
begin
 for i := 0 to F_Main.PM_Clients.Items.Count-1 do
  F_Main.PM_Clients.Items[i].Enabled := (F_Main.LV_Clients.Selected <> nil) and (ORTCPServer.GetClient(F_Main.LV_Clients.ItemIndex) <> nil);
end;

procedure TF_Main.PM_ConsoleClick(Sender: TObject);
 begin
  F_Console.Show;
 end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_Main.LV_JCChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
 B_VC_delete.Enabled := (LV_JC.Selected <> nil);

 if (LV_JC.Selected <> nil) then
   B_JC_Reset.Enabled := JCDb.GetJCByIndex(LV_JC.ItemIndex).staveni
 else
   B_JC_Reset.Enabled := false;
end;

procedure TF_Main.LV_JCCustomDrawItem(Sender: TCustomListView; Item: TListItem;
  State: TCustomDrawState; var DefaultDraw: Boolean);
begin
 if (Item.SubItems.Count >= 4) then
  begin
   if (Item.SubItems[3] <> '0') then
     Self.LV_JC.Canvas.Brush.Color := _TABLE_COLOR_YELLOW
   else if (Item.SubItems[1] = '-6') then
     Self.LV_JC.Canvas.Brush.Color := clAqua
   else if (Item.SubItems[1] <> '-5') then
     Self.LV_JC.Canvas.Brush.Color := _TABLE_COLOR_GREEN;
  end;
end;

procedure TF_Main.LV_JCDblClick(Sender: TObject);
begin
  if (LV_JC.Selected <> nil) then
    F_JCEdit.EditJC(LV_JC.ItemIndex);
end;

procedure TF_Main.LV_JCKeyPress(Sender: TObject; var Key: Char);
begin
 if (Key = #13) then Self.LV_JCDblClick(LV_Bloky);
end;

procedure TF_Main.LV_logCustomDrawItem(Sender: TCustomListView; Item: TListItem;
  State: TCustomDrawState; var DefaultDraw: Boolean);
begin
   LV_log.Canvas.Brush.Color := TColor(LV_log.Items[Item.Index].Data);
end;

procedure TF_Main.LV_log_lnetCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
 if (Item.SubItems.Count < 2) then Exit();

 case (StrToIntDef(Item.SubItems[0],0)) of
  1:(Sender as TCustomListView).Canvas.Brush.Color := _TABLE_COLOR_RED;
  2:(Sender as TCustomListView).Canvas.Brush.Color := _TABLE_COLOR_YELLOW;
  4,5: begin
     (Sender as TCustomListView).Canvas.Brush.Color := _TABLE_COLOR_GRAY;
     if (LeftStr(Item.SubItems[1], 3) = 'GET') then
       (Sender as TCustomListView).Canvas.Brush.Color := _TABLE_COLOR_BLUE;
     if (LeftStr(Item.SubItems[1], 3) = 'PUT') then
       (Sender as TCustomListView).Canvas.Brush.Color := _TABLE_COLOR_GREEN;
    end;//case 2
  3,6:(Sender as TCustomListView).Canvas.Brush.Color := _TABLE_COLOR_WHITE;
 end;//case
end;

procedure TF_Main.LV_log_lnetDblClick(Sender: TObject);
begin
 Self.LV_log_lnet.Clear();
end;

procedure TF_Main.LV_MultiJCChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
 Self.B_mJC_Remove.Enabled := (Self.LV_MultiJC.Selected <> nil);

 if (Self.LV_MultiJC.SelCount > 1) then
   Self.B_mJC_Remove.Caption := 'Smazat cesty'
 else
   Self.B_mJC_Remove.Caption := 'Smazat cestu';
end;

procedure TF_Main.LV_MultiJCCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
 if (Item.SubItems.Count >= 4) then
  begin
   if (Item.SubItems[1] <> '-1') then
     Self.LV_MultiJC.Canvas.Brush.Color := _TABLE_COLOR_YELLOW
   else
     Self.LV_MultiJC.Canvas.Brush.Color := _TABLE_COLOR_WHITE;
  end;
end;

procedure TF_Main.LV_MultiJCDblClick(Sender: TObject);
begin
 if (Self.LV_MultiJC.Selected <> nil) then
   F_MJCEdit.EditMJC(MultiJCDb[Self.LV_MultiJC.ItemIndex]);
end;

procedure TF_Main.LV_MultiJCKeyPress(Sender: TObject; var Key: Char);
begin
 if (Key = #13) then Self.LV_MultiJCDblClick(LV_Bloky);
end;

procedure TF_Main.LV_SoupravyChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
 Self.B_lok_delete.Enabled := (Self.SoupravySelectedCount() > 0);

 if (Self.SoupravySelectedCount() > 1) then
   Self.B_lok_delete.Caption := 'Smazat soupravy'
 else
   Self.B_lok_delete.Caption := 'Smazat soupravu';
end;

procedure TF_Main.LV_StaniceChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var OblR: TOR;
begin
 if (Self.LV_Stanice.Selected <> nil) then
  begin
   OblR := ORs[Self.LV_Stanice.ItemIndex];
   if (OblR.stack.Count > 0) then Self.B_RemoveStack.Enabled := true else Self.B_RemoveStack.Enabled := false;
  end else begin
   Self.B_RemoveStack.Enabled := false;
  end;
end;

procedure TF_Main.LV_Stav_RCSCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
 if ((Item.SubItems.Count > 5) and (Item.SubItems[5] = 'Fail')) then
   Self.LV_Stav_RCS.Canvas.Brush.Color := _TABLE_COLOR_RED
 else
   Self.LV_Stav_RCS.Canvas.Brush.Color := _TABLE_COLOR_WHITE;
end;

procedure TF_Main.LV_UsersChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
 Self.B_User_Delete.Enabled := Self.LV_Users.Selected <> nil;
end;

procedure TF_Main.LV_UsersDblClick(Sender: TObject);
begin
 if (Self.LV_Users.Selected <> nil) then
  F_UserEdit.OpenForm(UsrDB.GetUser(Self.LV_Users.ItemIndex));
end;

procedure TF_Main.LV_UsersKeyPress(Sender: TObject; var Key: Char);
begin
 if (Key = #13) then Self.LV_UsersDblClick(LV_Bloky);
end;

procedure TF_Main.LV_ZesilovaceChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
 B_zes_delete.Enabled := LV_Zesilovace.Selected <> nil;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_Main.LogStatus(str: string);
begin
 if (Assigned(Self.LB_Log)) then
  begin
   if (Self.LB_Log.Items.Count > 100) then Self.LB_Log.Clear();   
   Self.LB_Log.Items.Insert(0, FormatDateTime('hh:nn:ss', Now)+ ' : ' + str);
  end;
 writeLog(str, WR_SYSTEM);
end;

procedure TF_Main.LV_ABChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
 Self.B_AB_Delete.Enabled := (Self.LV_AB.Selected <> nil);
end;

procedure TF_Main.LV_BlokyChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  B_BlkDelete.Enabled := (LV_Bloky.Selected <> nil);
end;

procedure TF_Main.LV_BlokyCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
var Blk: TBlk;
 begin
  if (Blocks.GetBlkByIndex(Item.Index, Blk) <> 0) then Exit;

  case (Blk.typ) of
   btTurnout: begin
    case ((Blk as TBlkTurnout).position) of
     TTurnoutPosition.disabled : LV_Bloky.Canvas.Brush.Color := _TABLE_COLOR_GRAY;
     TTurnoutPosition.none     : LV_Bloky.Canvas.Brush.Color := _TABLE_COLOR_YELLOW;
     TTurnoutPosition.plus     : LV_Bloky.Canvas.Brush.Color := _TABLE_COLOR_GREEN;
     TTurnoutPosition.minus    : LV_Bloky.Canvas.Brush.Color := _TABLE_COLOR_GREEN;
     TTurnoutPosition.both     : LV_Bloky.Canvas.Brush.Color := _TABLE_COLOR_RED;
    end;
   end;

  //////////////////////
   btTrack, btRT : begin
    case ((Blk as TBlkTrack).occupied) of
     TTrackState.disabled : LV_Bloky.Canvas.Brush.Color := _TABLE_COLOR_GRAY;
     TTrackState.none     : LV_Bloky.Canvas.Brush.Color := _TABLE_COLOR_YELLOW;
     TTrackState.free : LV_Bloky.Canvas.Brush.Color := _TABLE_COLOR_GREEN;
     TTrackState.occupied : LV_Bloky.Canvas.Brush.Color := _TABLE_COLOR_PINKY;
    end;
   end;

  //////////////////////
   btIR: begin
    case ((Blk as TBlkIR).occupied) of
     TIROccupationState.disabled : LV_Bloky.Canvas.Brush.Color := _TABLE_COLOR_GRAY;
     TIROccupationState.none     : LV_Bloky.Canvas.Brush.Color := _TABLE_COLOR_YELLOW;
     TIROccupationState.free     : LV_Bloky.Canvas.Brush.Color := _TABLE_COLOR_GREEN;
     TIROccupationState.occupied : LV_Bloky.Canvas.Brush.Color := _TABLE_COLOR_PINKY;
    end;
   end;

  //////////////////////
   btSignal: begin
    if ((Blk as TBlkSignal).signal < ncStuj) then
     LV_Bloky.Canvas.Brush.Color := _TABLE_COLOR_GRAY  // disabled
    else if ((Blk as TBlkSignal).signal = ncStuj) then
     LV_Bloky.Canvas.Brush.Color := _TABLE_COLOR_GREEN
    else
     LV_Bloky.Canvas.Brush.Color := _TABLE_COLOR_PINKY;
   end;

  //////////////////////
   btCrossing: begin
    case ((Blk as TBlkCrossing).state) of
     TBlkCrossingBasicState.disabled : LV_Bloky.Canvas.Brush.Color := _TABLE_COLOR_GRAY;
     TBlkCrossingBasicState.none     : LV_Bloky.Canvas.Brush.Color := _TABLE_COLOR_YELLOW;
     TBlkCrossingBasicState.open     : LV_Bloky.Canvas.Brush.Color := _TABLE_COLOR_GREEN;
     TBlkCrossingBasicState.caution  : LV_Bloky.Canvas.Brush.Color := _TABLE_COLOR_PINKY;
     TBlkCrossingBasicState.closed   : LV_Bloky.Canvas.Brush.Color := _TABLE_COLOR_PINKY;
    end;
   end;

  //////////////////////
   btLinker: begin
    if (not (Blk as TBlkLinker).enabled) then
     LV_Bloky.Canvas.Brush.Color := _TABLE_COLOR_GRAY
    else
     LV_Bloky.Canvas.Brush.Color := _TABLE_COLOR_WHITE;
   end;

  //////////////////////
   btRailway: begin
    if ((Blk as TBlkRailway).state.direction = TRailwayDirection.disabled) then
     LV_Bloky.Canvas.Brush.Color := _TABLE_COLOR_GRAY
    else
     LV_Bloky.Canvas.Brush.Color := _TABLE_COLOR_WHITE;
   end;

  //////////////////////
   btLock: begin
    if ((Blk as TBlkLock).state.enabled) then
     begin
      if ((Blk as TBlkLock).keyReleased) then
       LV_Bloky.Canvas.Brush.Color := _TABLE_COLOR_PINKY
      else
       LV_Bloky.Canvas.Brush.Color := _TABLE_COLOR_WHITE;
     end else
       LV_Bloky.Canvas.Brush.Color := _TABLE_COLOR_GRAY
   end;

  //////////////////////
   btDisconnector: begin
    case ((Blk as TBlkDisconnector).state) of
      TBlkDiscBasicState.disabled     : LV_Bloky.Canvas.Brush.Color := _TABLE_COLOR_GRAY;
      TBlkDiscBasicState.not_selected : LV_Bloky.Canvas.Brush.Color := _TABLE_COLOR_GREEN;
      TBlkDiscBasicState.mounting     : LV_Bloky.Canvas.Brush.Color := _TABLE_COLOR_PINKY;
      TBlkDiscBasicState.active       : LV_Bloky.Canvas.Brush.Color := _TABLE_COLOR_PINKY;
    end;
   end;

  //////////////////////
  btIO: begin
    if (TBlkIO(Blk).enabled) then
     begin
      if ((TBlkIO(Blk).activeOutput) or (TBlkIO(Blk).activeInput)) then
        LV_Bloky.Canvas.Brush.Color := _TABLE_COLOR_YELLOW
      else if (TBlkAC(Blk).clientConnected) then
        LV_Bloky.Canvas.Brush.Color := _TABLE_COLOR_WHITE
     end else
      LV_Bloky.Canvas.Brush.Color := _TABLE_COLOR_GRAY;
  end;

  //////////////////////
  btSummary: begin
    case ((Blk as TBlkSummary).enabled) of
      false : LV_Bloky.Canvas.Brush.Color := _TABLE_COLOR_GRAY;
      true  : LV_Bloky.Canvas.Brush.Color := _TABLE_COLOR_GREEN;
    end;
  end;

  //////////////////////
  btAC: begin
    if (TBlkAC(Blk).enabled) then
     begin
      if (not TBlkAC(Blk).stopped) then
        LV_Bloky.Canvas.Brush.Color := _TABLE_COLOR_YELLOW
      else if (TBlkAC(Blk).clientConnected) then
        LV_Bloky.Canvas.Brush.Color := _TABLE_COLOR_GREEN
      else
        LV_Bloky.Canvas.Brush.Color := _TABLE_COLOR_WHITE;
     end else
      LV_Bloky.Canvas.Brush.Color := _TABLE_COLOR_GRAY;
  end;

  end;//case
end;

procedure TF_Main.LV_BlokyDblClick(Sender: TObject);
var Blk: TBlk;
 begin
  if (LV_Bloky.Selected = nil) then Exit;
  if (Blocks.GetBlkByIndex(Self.LV_Bloky.ItemIndex, Blk) <> 0) then Exit;

  case (Blk.typ) of
   btTurnout: F_BlkVyhybka.OpenForm(Self.LV_Bloky.ItemIndex);
   btTrack: F_BlkUsek.OpenForm(Self.LV_Bloky.ItemIndex);
   btIR: F_BlkIR.OpenForm(Self.LV_Bloky.ItemIndex);
   btSignal: F_BlkNav.OpenForm(Self.LV_Bloky.ItemIndex);
   btCrossing: F_BlkPrejezd.OpenForm(Self.LV_Bloky.ItemIndex);
   btRailway, btLinker: F_BlkTrat.OpenForm(Self.LV_Bloky.ItemIndex);
   btLock: F_BlkZamek.OpenForm(Self.LV_Bloky.ItemIndex);
   btDisconnector: F_BlkRozp.OpenForm(Self.LV_Bloky.ItemIndex);
   btRT: F_BlkTU.OpenForm(Self.LV_Bloky.ItemIndex);
   btIO: F_BlkIO.OpenForm(Self.LV_Bloky.ItemIndex);
   btSummary: F_BlkSH.OpenForm(Self.LV_Bloky.ItemIndex);
   btAC: F_BlkAC.OpenForm(Self.LV_Bloky.ItemIndex);
  end;//case
end;

procedure TF_Main.LV_BlokyKeyPress(Sender: TObject; var Key: Char);
begin
 if (Key = #13) then Self.LV_BlokyDblClick(LV_Bloky);
end;

procedure TF_Main.LV_ClientsCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
 if ((Item.SubItems[_LV_CLIENTS_COL_STATE] = 'uzavřeno') or (Item.SubItems[_LV_CLIENTS_COL_STATE] = 'odpojen')) then
   Self.LV_Clients.Canvas.Brush.Color := _TABLE_COLOR_WHITE
 else if ((Item.SubItems[_LV_CLIENTS_COL_STATE] = 'otevírání') or (Item.SubItems[_LV_CLIENTS_COL_STATE] = 'handshake')) then
   Self.LV_Clients.Canvas.Brush.Color := _TABLE_COLOR_GRAY
 else if (Item.SubItems[_LV_CLIENTS_COL_STATE] = 'otevřeno') then begin
   if (Item.SubItems[_LV_CLIENTS_COL_PING] = 'unreachable') then
     Self.LV_Clients.Canvas.Brush.Color := _TABLE_COLOR_RED
   else if (Item.SubItems[_LV_CLIENTS_COL_PING] = '?') then
     Self.LV_Clients.Canvas.Brush.Color := _TABLE_COLOR_YELLOW
   else
     Self.LV_Clients.Canvas.Brush.Color := _TABLE_COLOR_GREEN;
 end else
   Self.LV_Clients.Canvas.Brush.Color := _TABLE_COLOR_YELLOW;
end;

procedure TF_Main.LV_HVChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
 B_HV_Delete.Enabled := (LV_HV.Selected <> nil) and (not TrakceI.ConnectedSafe());
end;

procedure TF_Main.LV_HVCustomDrawItem(Sender: TCustomListView; Item: TListItem;
  State: TCustomDrawState; var DefaultDraw: Boolean);
begin
 if (Item.SubItems.Count > 18) then
  begin
   if ((Item.SubItems[17] = 'COM ERROR!') or (Item.SubItems[18] = 'error')) then
     (Sender as TCustomListView).Canvas.Brush.Color := _TABLE_COLOR_RED
   else if (Item.SubItems[17] = 'PC') then
     (Sender as TCustomListView).Canvas.Brush.Color := _TABLE_COLOR_GREEN
   else if ((Item.SubItems[17] = 'ukradeno') or (Item.SubItems[18] = 'progr')) then
     (Sender as TCustomListView).Canvas.Brush.Color := _TABLE_COLOR_YELLOW;
  end;
end;

procedure TF_Main.LV_HVDblClick(Sender: TObject);
begin
 if (LV_HV.Selected = nil) then Exit();

 if (TrakceI.ConnectedSafe()) then
  begin
   try
    RegCollector.Open(HVDb[StrToInt(Self.LV_HV.Selected.Caption)]);
   except
    on E: Exception do
      Application.MessageBox(PChar(E.Message), 'Varování', MB_OK OR MB_ICONWARNING);
   end;
  end else begin
   F_HVEdit.OpenForm(HVDB[Integer(LV_HV.Selected.Data)]);
  end;
end;

procedure TF_Main.LV_HVKeyPress(Sender: TObject; var Key: Char);
begin
 if (Key = #13) then Self.LV_HVDblClick(LV_Bloky);
end;

procedure TF_Main.LV_ZesilovaceCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
 if ((not RCSi.NoExStarted()) or (not Boosters.sorted[Item.Index].rcsPresent)) then
  begin
   LV_Zesilovace.Canvas.Brush.Color := _TABLE_COLOR_GRAY;
  end else begin
   if (Boosters.sorted[Item.Index].napajeni = TBoosterSignal.ok) then
    begin
     if (Boosters.sorted[Item.Index].Zkrat = TBoosterSignal.ok) then
      begin
       LV_Zesilovace.Canvas.Brush.Color := _TABLE_COLOR_GREEN;
      end else begin
       LV_Zesilovace.Canvas.Brush.Color := _TABLE_COLOR_RED;
      end;
    end else begin
     LV_Zesilovace.Canvas.Brush.Color := _TABLE_COLOR_BLUE;
    end;
  end;//if not Zarizeni.Start
end;

procedure TF_Main.LV_ZesilovaceDblClick(Sender: TObject);
begin
 if (LV_Zesilovace.Selected <> nil) then
   F_ZesilovacEdit.OpenForm(Boosters.sorted[LV_Zesilovace.ItemIndex]);
end;

procedure TF_Main.LV_ZesilovaceKeyPress(Sender: TObject; var Key: Char);
begin
 if (Key = #13) then Self.LV_ZesilovaceDblClick(LV_Bloky);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_Main.DisableRemoveButtons();
 begin
  B_BlkDelete.Enabled := false;
  B_HV_Delete.Enabled := false;
  B_lok_delete.Enabled := false;
  B_zes_delete.Enabled := false;
  B_User_Delete.Enabled := false;
  B_VC_delete.Enabled := false;
  B_JC_Reset.Enabled := false;
  B_RemoveStack.Enabled := false;
  B_mJC_Remove.Enabled := false;
 end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_Main.UpdateCallMethod();
var ev: TNotifyEvent;
begin
  if (Assigned(Self.call_method)) then
   begin
    // toto poradi musi byt zachovano !
    // volani eventu totiz muze zpusobit Application.ProcessMessages
    ev := Self.call_method;
    Self.call_method := nil;
    ev(self);
   end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_Main.UpdateSystemButtons();
begin
 Self.A_System_Start.Enabled := ((not RCSi.Started) or (not TrakceI.ConnectedSafe())
    or (Self.A_Locos_Acquire.Enabled) or (not ORTCPServer.openned) or (not Blocks.enabled) or
       ((GlobalConfig.ptAutoStart) and (not PtServer.openned)));
 Self.A_System_Stop.Enabled := (RCSi.Opened) or (TrakceI.ConnectedSafe())
    or (ORTCPServer.openned) or (PtServer.openned);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_Main.CheckNasobicWidth();
const _SMALL = 33;
      _LARGE = 50;
begin
 if ((Length(Self.P_Zrychleni.Caption) > 2) and (Self.P_Zrychleni.Width <= _SMALL)) then
  begin
   Self.P_Zrychleni.Width := _LARGE;
   Self.FormResize(Self);
 end else if ((Length(Self.P_Zrychleni.Caption) = 2) and (Self.P_Zrychleni.Width = _LARGE)) then
  begin
   Self.P_Zrychleni.Width := _SMALL;
   Self.FormResize(Self);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TF_Main.SoupravySelectedCount(): Integer;
var LI: TListItem;
begin
 Result := 0;
 for LI in Self.LV_Soupravy.Items do
   if ((LI.Selected) and (LI.Caption <> '')) then
     Inc(Result);
end;

////////////////////////////////////////////////////////////////////////////////

function TF_Main.LVSelectedTexts(LV: TListView; single: string; multiple: string): string;
var LI: TListItem;
    count: Integer;
begin
 Result := '';
 count := 0;
 for LI in LV.Items do
  begin
   if (LI.Selected) then
    begin
     Result := Result + LI.Caption + ', ';
     Inc(count);
    end;
  end;
 Result := LeftStr(Result, Length(Result)-2);

 if (count = 1) then
   Result := single + ' ' + Result
 else
   Result := multiple + ' ' + Result;
end;

////////////////////////////////////////////////////////////////////////////////

initialization
  SystemData := TSystem.Create();

finalization
  FreeAndNil(SystemData);

end.//unit
