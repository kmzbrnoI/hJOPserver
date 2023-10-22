unit fMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Menus, ImgList, Buttons, ComCtrls, Trakce,
  inifiles, ActnList, AppEvnts, cpuLoad, ExtDlgs, Gauges, StrUtils,
  ComObj, TechnologieTrakce, BoosterDb, System.Actions, System.ImageList,
  Vcl.Mask, Vcl.Samples.Spin;

const
  _SB_LOG = 0;
  _SB_RCS = 1;
  _SB_TRAKCE_STAV = 2;
  _SB_TRAKCE_LIB = 3;
  _SB_RCS_LIB = 4;
  _SB_PROC = 5;

  _INIDATA_FN = 'inidata.ini';

  _TABLE_COLOR_GREEN = $E0FFE0;
  _TABLE_COLOR_GRAY = $DDDDDD;
  _TABLE_COLOR_RED = $E0E0FF;
  _TABLE_COLOR_YELLOW = $D0FFFF;
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

  TAutostartState = (asDisabled, asEnabled, asWaiting);

  TF_Main = class(TForm)
    T_Main: TTimer;
    Menu_1: TMainMenu;
    MI_RCS: TMenuItem;
    MI_RCS_Go: TMenuItem;
    MI_RCS_Stop: TMenuItem;
    MI_RCS_Options: TMenuItem;
    MI_Provoz: TMenuItem;
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
    LV_Blocks: TListView;
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
    P_log: TPanel;
    Panel3: TPanel;
    Label2: TLabel;
    CB_centrala_loglevel_file: TComboBox;
    N9: TMenuItem;
    PM_SaveLayout: TMenuItem;
    A_SaveStav: TAction;
    PM_Bloky: TPopupMenu;
    MI_BlockState: TMenuItem;
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
    CHB_log_rcs: TCheckBox;
    N12: TMenuItem;
    MI_Trk_Options: TMenuItem;
    N13: TMenuItem;
    MI_Trk_Libs: TMenuItem;
    MI_Trk_Update: TMenuItem;
    A_Trk_Lib_Cfg: TAction;
    A_Turnoff_Functions: TAction;
    CHB_log_auth: TCheckBox;
    Label3: TLabel;
    CB_global_loglevel_file: TComboBox;
    Label4: TLabel;
    CB_global_loglevel_table: TComboBox;
    MI_SimulationDiagnostics: TMenuItem;
    TS_Config: TTabSheet;
    P_ConfigHeader: TPanel;
    E_configFilename: TEdit;
    P_Config: TPanel;
    CHB_Log_console: TCheckBox;
    CHB_autostart: TCheckBox;
    GB_Autosave: TGroupBox;
    Label5: TLabel;
    CHB_Autosave: TCheckBox;
    ME_autosave_period: TMaskEdit;
    GB_Scale: TGroupBox;
    Label6: TLabel;
    Label8: TLabel;
    E_Scale: TEdit;
    CB_MainTimerInterval: TComboBox;
    Label9: TLabel;
    GB_Speeds: TGroupBox;
    LV_DigiRych: TListView;
    B_ConfigApply: TButton;
    GB_Times: TGroupBox;
    Label10: TLabel;
    SE_timeRC: TSpinEdit;
    Label11: TLabel;
    SE_timeRCVC: TSpinEdit;
    Label12: TLabel;
    SE_timeRCPC: TSpinEdit;
    Label13: TLabel;
    SE_timeNUZ: TSpinEdit;
    Label14: TLabel;
    SE_jcMaxMovingTurnouts: TSpinEdit;
    procedure T_MainTimer(Sender: TObject);
    procedure PM_ResetVClick(Sender: TObject);
    procedure MI_RCS_libClick(Sender: TObject);
    procedure MI_Trk_libClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure AE_1Message(var Msg: tagMSG; var Handled: Boolean);
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
    procedure LV_ClientsCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
      var DefaultDraw: Boolean);
    procedure PC_1Change(Sender: TObject);
    procedure LV_ZesilovaceDblClick(Sender: TObject);
    procedure B_zes_addClick(Sender: TObject);
    procedure B_zes_deleteClick(Sender: TObject);
    procedure LV_ZesilovaceCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
      var DefaultDraw: Boolean);
    procedure LV_HVChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure LV_HVDblClick(Sender: TObject);
    procedure B_HV_AddClick(Sender: TObject);
    procedure B_HV_DeleteClick(Sender: TObject);
    procedure LV_BlocksCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
      var DefaultDraw: Boolean);
    procedure LV_BlocksDblClick(Sender: TObject);
    procedure B_BlkAddClick(Sender: TObject);
    procedure LV_BlocksChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure B_BlkDeleteClick(Sender: TObject);
    procedure B_VC_AddClick(Sender: TObject);
    procedure LV_JCDblClick(Sender: TObject);
    procedure B_VC_deleteClick(Sender: TObject);
    procedure LV_logCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
      var DefaultDraw: Boolean);
    procedure LV_log_lnetCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
      var DefaultDraw: Boolean);
    procedure LV_ZesilovaceChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure LV_JCChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure MI_Save_configClick(Sender: TObject);
    procedure LB_LogDblClick(Sender: TObject);
    procedure LV_SoupravyChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure B_lok_deleteClick(Sender: TObject);
    procedure LV_log_lnetDblClick(Sender: TObject);
    procedure LV_HVCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
      var DefaultDraw: Boolean);
    procedure PM_PropertiesClick(Sender: TObject);
    procedure PM_RegulatorClick(Sender: TObject);
    procedure PM_HVPopup(Sender: TObject);
    procedure LV_JCCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
      var DefaultDraw: Boolean);
    procedure CB_centrala_loglevel_fileChange(Sender: TObject);
    procedure LB_LogDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure B_User_AddClick(Sender: TObject);
    procedure LV_UsersChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure LV_UsersDblClick(Sender: TObject);
    procedure B_User_DeleteClick(Sender: TObject);
    procedure A_SaveStavExecute(Sender: TObject);
    procedure PM_BlokyPopup(Sender: TObject);
    procedure MI_PropClick(Sender: TObject);
    procedure MI_BlockStateClick(Sender: TObject);
    procedure B_JC_ResetClick(Sender: TObject);
    procedure P_Time_modelovyDblClick(Sender: TObject);
    procedure P_ZrychleniDblClick(Sender: TObject);
    procedure LV_StaniceChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure B_RemoveStackClick(Sender: TObject);
    procedure LV_MultiJCCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
      var DefaultDraw: Boolean);
    procedure LV_MultiJCChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure B_mJC_AddClick(Sender: TObject);
    procedure B_mJC_RemoveClick(Sender: TObject);
    procedure LV_MultiJCDblClick(Sender: TObject);
    procedure PM_ClientsPopup(Sender: TObject);
    procedure MI_DisconnectClick(Sender: TObject);
    procedure A_FuncsSetExecute(Sender: TObject);
    procedure B_ChangeClick(Sender: TObject);
    procedure LV_BlocksKeyPress(Sender: TObject; var Key: Char);
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
    procedure LV_Stav_RCSCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
      var DefaultDraw: Boolean);
    procedure MI_HoukClick(Sender: TObject);
    procedure MI_RCS_UpdateClick(Sender: TObject);
    procedure B_AB_DeleteClick(Sender: TObject);
    procedure LV_ABChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure CHB_RCS_Show_Only_ActiveClick(Sender: TObject);
    procedure CHB_log_rcsClick(Sender: TObject);
    procedure A_Trk_Lib_CfgExecute(Sender: TObject);
    procedure MI_Trk_UpdateClick(Sender: TObject);
    procedure A_Turnoff_FunctionsExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CHB_log_authClick(Sender: TObject);
    procedure LV_logDblClick(Sender: TObject);
    procedure LV_SoupravyCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure MI_SimulationDiagnosticsClick(Sender: TObject);
    procedure B_ConfigApplyClick(Sender: TObject);
    procedure LV_DigiRychDblClick(Sender: TObject);
  private
    call_method: TNotifyEvent;
    mCpuLoad: TCpuLoad;

    procedure UpdateCallMethod();
    procedure OnFuncNameChange(Sender: TObject);

    procedure WMPowerBroadcast(var Msg: TMessage); message WM_POWERBROADCAST;
    procedure WMQueryEndSession(var Msg: TWMQueryEndSession); message WM_QUERYENDSESSION;
    procedure WMEndSession(var Msg: TWMEndSession); message WM_ENDSESSION;

  public
    autostart: record
      goTime: TDateTime;
      state: TAutostartState;
    end;

    CloseMessage: Boolean; // jestli se ptat uzivatele na ukonceni SW
    NUZClose: Boolean; // flag hard ukonceni SW bez kontroly pripojeni k systemum a zobrazeni dialogu
    sb1Log: Boolean;

    procedure CloseForm();
    procedure RepaintObjects();
    procedure LoadIniLibData();
    procedure AutostartUpdate();
    procedure OnStart();
    procedure ShowDateTime();
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
    procedure OnDCCGoOk(Sender: TObject; Data: Pointer);
    procedure OnDCCGoError(Sender: TObject; Data: Pointer);
    procedure OnDCCStopOk(Sender: TObject; Data: Pointer);
    procedure OnDCCStopError(Sender: TObject; Data: Pointer);

    procedure OnTrkAllAcquired(Sender: TObject);
    procedure OnTrkAcquireError(Sender: TObject);
    procedure OnTrkAllReleased(Sender: TObject);
    procedure OnTrkLocoAcquired(Sender: TObject);
    procedure OnTrkLocoReleased(Sender: TObject);
    procedure OnTrkAllFunctionTurnedOff(Sender: TObject; Data: Pointer);

    // GlobalConfig
    procedure FillGlobalConfig();

  end; // public

  TSystemStatus = (null, starting, stopping); // stav startovani / vypinani systemu

  TSystem = class
    Status: TSystemStatus; // aktualni stav systemu
  end;

var
  F_Main: TF_Main;
  SystemData: TSystem;

implementation

uses fTester, fNastaveni_Casu, fSplash, fHoukEvsUsek, DataJC,
  fAbout, version, fSystemInfo, fBlkTrack, fBlkTurnout, fAdminForm, Simulation,
  fRegulator, fBlkSummary, fSystemAutoStart, fBlkTrackState, GetSystems,
  TechnologieRCS, TechnologieJC, Config, fConsole, AreaDb, BlockDb,
  Block, BlockTrack, BlockTurnout, BlockSignal, BlockIR, Area,
  BlockSummary, BlockCrossing, TJCDatabase, Logging,
  TCPServerPanel, DataBloky, DataHV, DataRCS, DataORs, DataZesilovac,
  fBlkNew, fHVEdit, fJCEdit, fZesilovacEdit, THVDatabase, fBlkIR, fBlkCrossing,
  fBlkSignal, fBlkRailway, BlockLinker, TrainDb, DataTrains, DataUsers, fUserEdit, UserDb,
  fBlkTurnoutState, fBlkRailwayState, BlockRailway, ModelovyCas, fBlkLock,
  BlockLock, DataMultiJC, TMultiJCDatabase, fMJCEdit, BlockDisconnector,
  fBlkDisconnector, fFuncsSet, FunkceVyznam, fBlkRT, RCSdebugger, Booster, DataAB,
  AppEv, fBlkIO, BlockIO, TCPServerPT, RCSErrors, TechnologieAB, fBlkCrossingState,
  Diagnostics, BlockAC, fBlkAC, fBlkGroupSignal, fBlkPst, BlockPst, fBlkSignalState,
  fRychlostiEdit;

{$R *.dfm}
/// /////////////////////////////////////////////////////////////////////////////
// RCS BEGIN
/// /////////////////////////////////////////////////////////////////////////////

procedure TF_Main.MI_RCS_libClick(Sender: TObject);
var fn: string;
begin
  fn := StringReplace(TMenuItem(Sender).Caption, '&', '', [rfReplaceAll]);

  Screen.Cursor := crHourGlass;
  Log('RCS -> ' + fn, llInfo, lsRCS);
  Self.SB1.Panels.Items[_SB_RCS_LIB].Text := '-';
  try
    RCSi.LoadLib(RCSi.libDir + '\' + fn);
    Self.LogStatus('RCS: načteno ' + fn);
    Self.SB1.Panels.Items[_SB_RCS_LIB].Text := ExtractFileName(RCSi.Lib);
  except
    on E: Exception do
    begin
      Screen.Cursor := crDefault;
      Application.MessageBox(PChar('Nelze načíst knihovnu ' + fn + ':' + #13#10 + E.Message), 'Nelze načíst knihovnu',
        MB_OK OR MB_ICONWARNING);
      Exit();
    end;
  end;
  RCSTableData.LoadToTable(not Self.CHB_RCS_Show_Only_Active.Checked);
  Screen.Cursor := crDefault;
end;

procedure TF_Main.UpdateRCSLibsList();
var SR: TSearchRec;
  Item: TMenuItem;

  procedure AddLib(name: string);
  begin
    Item := TMenuItem.Create(Self.MI_RCS_Libs);
    Item.Caption := name;
    Item.OnClick := Self.MI_RCS_libClick;
    Self.MI_RCS_Libs.Add(Item);
  end;

begin
  Self.MI_RCS_Libs.Clear();

  if (FindFirst(RCSi.libDir + '\*.dll', faAnyFile, SR) = 0) then
  begin
    if ((SR.Attr AND faDirectory) = 0) then
      AddLib(SR.name);

    while (FindNext(SR) = 0) do
      if ((SR.Attr AND faDirectory) = 0) then
        AddLib(SR.name);

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
      Application.MessageBox(PChar('Nelze zobrazit konfigurační dialog RCS : ' + E.Message), 'Varování',
        MB_OK OR MB_ICONWARNING);
      Exit();
    end;
  end;
  Screen.Cursor := crDefault;
  Log('Zobrazen ConfigDialog knihovny', llInfo, lsRCS);
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

  Self.S_RCS_open.Brush.Color := clBlue;
  Self.LogStatus('RCS: uzavírám zařízení...');

  Log('----- RCS CLOSING -----', llInfo, lsRCS);

  with (F_Main) do
  begin
    A_RCS_Open.Enabled := false;
    A_RCS_Close.Enabled := false;
    SB1.Panels.Items[_SB_RCS].Text := 'Zavírám RCS...';

    A_System_Start.Enabled := false;
    A_System_Stop.Enabled := false;
  end; // with F_Main do

  try
    RCSi.Close();
  except
    on E: ERCSNotOpened do
      Self.OnRCSErrClose(Self, 'RCS není otevřeno, nelze jej proto zavřít!');
    on E: ERCSScanningNotFinished do
      Self.OnRCSErrClose(Self, 'RCS nelze uzavřít před sokončneíms kenování modulů!');
    on E: Exception do
      Self.OnRCSErrClose(Self, 'Nastala kritická chyba : ' + E.Message);
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
    A_RCS_Go.Enabled := false;
    A_RCS_Stop.Enabled := false;
    A_RCS_Close.Enabled := false;

    A_System_Start.Enabled := false;
    A_System_Stop.Enabled := false;

    SB1.Panels.Items[_SB_RCS].Text := 'Spouštím RCS...';
  end; // with F_Main do

  Self.LogStatus('RCS: Spouštím komunikaci...');
  Self.S_RCS_start.Brush.Color := clBlue;

  Log('----- RCS STARTING -----', llInfo, lsRCS);

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
      Self.OnRCSErrStart(Self, 'Nastala kritická chyba : ' + E.Message);
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
    A_RCS_Open.Enabled := false;
    A_RCS_Close.Enabled := false;

    SB1.Panels.Items[_SB_RCS].Text := 'Otevírám RCS...';

    A_System_Start.Enabled := false;
    A_System_Stop.Enabled := false;
  end; // with F_Main do

  Self.LogStatus('RCS: Otevírám zařízení, hledám moduly...');
  Self.S_RCS_open.Brush.Color := clBlue;

  Log('----- RCS OPENING -----', llInfo, lsRCS);

  try
    RCSi.logActionInProgress := true;
    RCSi.Open();
  except
    on E: ERCSAlreadyOpened do
      Self.OnRCSErrOpen(Self, 'RCS je již otevřeno!');
    on E: ERCSCannotOpenPort do
      Self.OnRCSErrOpen(Self,
        'Nepodařilo se otevřít USB port, otevřete konfigurační okno RCS driveru a zkontrolujte, že je vybrán správný port!');
    on E: Exception do
      Self.OnRCSErrOpen(Self, 'Nastala kritická chyba : ' + E.Message);
  end;
end;

procedure TF_Main.A_RCS_StopExecute(Sender: TObject);
begin
  if ((SystemData.Status = stopping) and (not RCSi.NoExStarted)) then
  begin
    Self.A_RCS_CloseExecute(nil);
    Exit();
  end;

  Self.S_RCS_start.Brush.Color := clGray;
  Self.LogStatus('RCS: zastavuji komunikaci...');

  Log('----- RCS STOPPING -----', llInfo, lsRCS);

  with (F_Main) do
  begin
    A_RCS_Go.Enabled := false;
    A_RCS_Stop.Enabled := false;
    SB1.Panels.Items[_SB_RCS].Text := 'Zastavuji RCS...';

    A_System_Start.Enabled := false;
    A_System_Stop.Enabled := false;
  end; // with F_Main do

  try
    RCSi.logActionInProgress := true;
    RCSi.Stop();
  except
    on E: ERCSNotStarted do
      Self.OnRCSErrStop(Self, 'RCS komunikace není spuštěna, nelze ji proto zastavit!');
    on E: Exception do
      Self.OnRCSErrStop(Self, 'Nastala kritická chyba : ' + E.Message);
  end;
end;

// --- events from RCS lib begin ---
procedure TF_Main.OnRCSStart(Sender: TObject);
begin
  RCSi.logActionInProgress := false;

  with (F_Main) do
  begin
    A_RCS_Go.Enabled := false;
    A_RCS_Stop.Enabled := true;

    PM_Tester.Enabled := true;
    PM_ResetV.Enabled := true;

    SB1.Panels.Items[_SB_RCS].Text := 'RCS spuštěno';
    UpdateSystemButtons();
  end; // with F_Main do

  Log('----- RCS START OK -----', llInfo, lsRCS);

  Self.LogStatus('RCS: komunikace spuštěna, čekám na první sken všech modulů...');
  RCSTableData.UpdateTable();
end;

procedure TF_Main.OnRCSScanned(Sender: TObject);
begin
  Self.S_RCS_start.Brush.Color := clLime;
  RCSTableData.UpdateTable();

  Log('----- RCS SCANNED -----', llInfo, lsRCS);
  Self.LogStatus('RCS: moduly naskenovány');

  // inicialziace osvetleni
  Areas.InitOsv();

  if (SystemData.Status = starting) then
    Self.A_Trk_ConnectExecute(nil);
end;

procedure TF_Main.OnRCSStop(Sender: TObject);
begin
  RCSi.logActionInProgress := false;

  if (Blocks.Enabled) then
  begin
    Blocks.Disable();
    Trains.ClearPOdj();
  end;

  ModCas.started := false;
  Self.UpdateSystemButtons();

  if (F_Tester.Showing) then
    F_Tester.Close();

  Self.S_RCS_start.Brush.Color := clRed;

  with (Self) do
  begin
    A_RCS_Go.Enabled := true;
    A_RCS_Stop.Enabled := false;
    A_RCS_Close.Enabled := true;

    PM_ResetV.Enabled := false;
    PM_Tester.Enabled := false;

    SB1.Panels.Items[_SB_RCS].Text := 'RCS otevřeno';
  end; // with F_Main do

  Log('----- RCS STOP OK -----', llInfo, lsRCS);

  Self.LogStatus('RCS: komunikace zastavena');

  RCSTableData.UpdateTable();

  if ((Self.Showing) and (Self.PC_1.ActivePage = F_Main.TS_Bloky)) then
    BlocksTablePainter.UpdateTable();

  PanelServer.BroadcastBottomError('Výpadek systému RCS!', 'TECHNOLOGIE');
  Trains.StopAllTrains();

  if (SystemData.Status = stopping) then
    Self.A_RCS_CloseExecute(nil);
end;

procedure TF_Main.OnRCSOpen(Sender: TObject);
var i: Integer;
  str: string;
begin
  RCSi.logActionInProgress := false;

  Self.A_RCS_Open.Enabled := false;
  Self.A_RCS_Close.Enabled := true;
  Self.A_RCS_Go.Enabled := true;
  Self.A_RCS_Stop.Enabled := false;
  Self.MI_RCS_Libs.Enabled := false;
  Self.UpdateSystemButtons();

  Self.S_RCS_open.Brush.Color := clLime;

  try
    Log('----- RCS OPEN OK : ' + IntToStr(RCSi.GetModuleCount) + ' modules -----', llInfo, lsRCS);
  except
    Log('----- RCS OPEN OK : unknown amount of modules -----', llWarning, lsRCS);
  end;

  Self.LogStatus('RCS: otevřeno');
  SB1.Panels.Items[_SB_RCS].Text := 'RCS otevřeno';

  F_Tester.AfterRCSOpen();

  RCSTableData.LoadToTable(not Self.CHB_RCS_Show_Only_Active.Checked);

  if (SystemData.Status = starting) then
  begin
    // scan, jestli nahodou nechybi RCS desky
    str := '';
    for i := 0 to RCSi.maxModuleAddr + 1 do
      if ((RCSi.GetNeeded(i)) and (not RCSi.IsModule(i))) then
      begin
        if (Length(str) > 0) then
          str := str + ', ';
        str := str + IntToStr(i);
      end;
    if (str <> '') then
    begin
      Log('Chybí RCS moduly ' + str, llWarning, lsRCS);
      Self.LogStatus('WARN: Chybí RCS moduly ' + str);
    end;

    Self.A_RCS_GoExecute(nil);
  end;
end;

procedure TF_Main.OnRCSClose(Sender: TObject);
begin
  RCSi.logActionInProgress := false;

  Self.A_RCS_Go.Enabled := false;
  Self.A_RCS_Stop.Enabled := false;
  Self.A_RCS_Close.Enabled := false;
  Self.A_RCS_Open.Enabled := true;
  Self.MI_RCS_Libs.Enabled := true;
  Self.UpdateSystemButtons();

  // may happen when RCS USB disconnects
  if (Blocks.Enabled) then
  begin
    Blocks.Disable();
    Trains.ClearPOdj();
  end;
  Trains.StopAllTrains();

  Self.S_RCS_open.Brush.Color := clRed;
  Self.S_RCS_start.Brush.Color := clRed;

  Log('----- RCS CLOSE OK -----', llInfo, lsRCS);

  Self.LogStatus('RCS: uzavřeno');
  SB1.Panels.Items[_SB_RCS].Text := 'RCS zavřeno';

  PanelServer.BroadcastBottomError('Výpadek systému RCS!', 'TECHNOLOGIE');
  Trains.StopAllTrains();

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
  RCSi.logActionInProgress := false;

  Self.A_RCS_Go.Enabled := false;
  Self.A_RCS_Stop.Enabled := false;
  Self.A_RCS_Open.Enabled := true;
  Self.UpdateSystemButtons();

  Self.S_RCS_open.Brush.Color := clRed;

  SystemData.Status := TSystemStatus.null;

  Self.LogStatus('ERR: RCS OPEN FAIL: ' + errMsg);
  Log('----- RCS OPEN FAIL - ' + errMsg + ' -----', llError, lsRCS);
  SB1.Panels.Items[_SB_RCS].Text := 'RCS zavřeno';

  Application.MessageBox(PChar('Při otevírání RCS nastala chyba:' + #13#10 + errMsg), 'Chyba', MB_OK OR MB_ICONWARNING);
end;

procedure TF_Main.OnRCSErrClose(Sender: TObject; errMsg: string);
begin
  RCSi.logActionInProgress := false;

  A_RCS_Go.Enabled := false;
  A_RCS_Stop.Enabled := false;
  A_RCS_Open.Enabled := true;

  Self.S_RCS_open.Brush.Color := clRed;

  SystemData.Status := null;

  Self.LogStatus('ERR: RCS CLOSE FAIL: ' + errMsg);
  SB1.Panels.Items[_SB_RCS].Text := 'RCS zavřeno';

  Application.MessageBox(PChar('Při uzavírání RCS nastala chyba:' + #13#10 + errMsg), 'Chyba', MB_OK OR MB_ICONWARNING);
  Log('----- RCS CLOSE FAIL - ' + errMsg + ' -----', llError, lsRCS);
end;

procedure TF_Main.OnRCSErrStart(Sender: TObject; errMsg: string);
begin
  RCSi.logActionInProgress := false;

  A_RCS_Close.Enabled := true;
  Self.UpdateSystemButtons();
  A_RCS_Go.Enabled := true;

  SB1.Panels.Items[_SB_RCS].Text := 'RCS otevřeno';
  S_RCS_start.Brush.Color := clRed;

  SystemData.Status := TSystemStatus.null;

  Self.LogStatus('ERR: RCS START FAIL: ' + errMsg);
  Log('----- RCS START FAIL - ' + errMsg + ' -----', llError, lsRCS);

  Application.MessageBox(PChar('Při zapínání komunikace nastala chyba:' + #13#10 + errMsg), 'Chyba',
    MB_OK OR MB_ICONWARNING);
end;

procedure TF_Main.OnRCSErrStop(Sender: TObject; errMsg: string);
begin
  RCSi.logActionInProgress := false;

  A_RCS_Open.Enabled := true;
  A_RCS_Close.Enabled := true;
  A_RCS_Go.Enabled := true;

  SB1.Panels.Items[_SB_RCS].Text := 'RCS otevřeno';
  S_RCS_start.Brush.Color := clRed;

  SystemData.Status := null;

  Self.LogStatus('ERR: RCS STOP FAIL: ' + errMsg);

  Application.MessageBox(PChar('Při vypínání komunikace nastala chyba:' + #13#10 + errMsg + #13#10), 'Chyba',
    MB_OK OR MB_ICONWARNING);
  Log('----- RCS STOP FAIL - ' + errMsg + ' -----', llError, lsRCS);
end;

procedure TF_Main.OnRCSReady(Sender: TObject; ready: Boolean);
var started, opened: Boolean;
begin
  try
    started := RCSi.started;
    opened := RCSi.opened;
  except
    on E: Exception do
    begin
      started := false;
      opened := false;
      AppEvents.LogException(E, 'OnRCSReady');
    end;
  end;

  Self.A_RCS_Open.Enabled := ready and (not opened);
  Self.A_RCS_Close.Enabled := ready and opened;
  Self.A_RCS_Go.Enabled := ready and opened and (not started);
  Self.A_RCS_Stop.Enabled := ready and started;

  try
    if ((ready) and (diag.simInputs) and (RCSi.Simulation)) then
      RCSi.InputSim();
  except
    on E: Exception do
      Log('Nelze provést inputSim : ' + E.Message, llError, lsRCS);
  end;
end;

// --- events from RCS lib end ---

/// /////////////////////////////////////////////////////////////////////////////
// RCS END
/// /////////////////////////////////////////////////////////////////////////////

/// /////////////////////////////////////////////////////////////////////////////
// TRAKCE BEGIN
/// /////////////////////////////////////////////////////////////////////////////

procedure TF_Main.MI_Trk_libClick(Sender: TObject);
var fn: string;
begin
  fn := StringReplace(TMenuItem(Sender).Caption, '&', '', [rfReplaceAll]);

  Screen.Cursor := crHourGlass;
  TrakceI.Log(TTrkLogLevel.llInfo, 'Změna knihovny -> ' + fn);
  Self.SB1.Panels.Items[_SB_TRAKCE_LIB].Text := '-';
  try
    TrakceI.LoadLib(TrakceI.libDir + '\' + fn);
    Self.LogStatus('Trakce: načteno ' + fn);
    Self.SB1.Panels.Items[_SB_TRAKCE_LIB].Text := ExtractFileName(TrakceI.Lib);
  except
    on E: Exception do
    begin
      Screen.Cursor := crDefault;
      Application.MessageBox(PChar('Nelze načíst knihovnu ' + fn + ':' + #13#10 + E.Message), 'Nelze načíst knihovnu',
        MB_OK OR MB_ICONWARNING);
      Exit();
    end;
  end;
  Screen.Cursor := crDefault;
end;

procedure TF_Main.UpdateTrkLibsList();
var SR: TSearchRec;
  Item: TMenuItem;

  procedure AddLib(name: string);
  begin
    Item := TMenuItem.Create(Self.MI_Trk_Libs);
    Item.Caption := name;
    Item.OnClick := Self.MI_Trk_libClick;
    Self.MI_Trk_Libs.Add(Item);
  end;

begin
  Self.MI_Trk_Libs.Clear();

  if (FindFirst(TrakceI.libDir + '\*.dll', faAnyFile, SR) = 0) then
  begin
    if ((SR.Attr AND faDirectory) = 0) then
      AddLib(SR.name);

    while (FindNext(SR) = 0) do
      if ((SR.Attr AND faDirectory) = 0) then
        AddLib(SR.name);

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
      Application.MessageBox(PChar('Nelze zobrazit konfigurační dialog: ' + E.Message), 'Varování',
        MB_OK OR MB_ICONWARNING);
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
      Application.MessageBox(PChar('Seznam knihoven se nepodařilo aktualizovat:' + #13#10 + E.Message), 'Chyba',
        MB_OK OR MB_ICONWARNING);
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
    Self.A_RCS_StopExecute(nil);
    Exit();
  end;

  try
    TrakceI.Disconnect();
  except
    on E: Exception do
    begin
      TrakceI.Log(llErrors, 'CLOSE: error: ' + E.Message);
      Application.MessageBox(PChar('Chyba pri uzavírání komunikace s centrálou:' + #13#10 + E.Message + #13#10 +
        'Více informací naleznete v logu.'), 'Chyba', MB_OK OR MB_ICONERROR);
    end;
  end;

  Application.ProcessMessages();
end;

procedure TF_Main.A_Locos_AcquireExecute(Sender: TObject);
begin
  Self.LogStatus('Loko: přebírám...');
  Self.S_locos_acquired.Brush.Color := clBlue;
  Self.G_locos_acquired.ForeColor := clBlue;

  Self.G_locos_acquired.MaxValue := 0;
  for var addr := 0 to THVDatabase._MAX_ADDR - 1 do
    if (HVDb[addr] <> nil) and (HVDb[addr].ShouldAcquire()) then
      Self.G_locos_acquired.MaxValue := Self.G_locos_acquired.MaxValue + 1;
  if (Self.G_locos_acquired.MaxValue = 0) then
    Self.G_locos_acquired.MaxValue := 1;

  HVDb.TrakceAcquireAllUsed(Self.OnTrkAllAcquired, Self.OnTrkAcquireError, Self.OnTrkLocoAcquired);
end;

procedure TF_Main.A_Locos_ReleaseExecute(Sender: TObject);
begin
  Self.LogStatus('Loko: odhlašuji...');
  Self.S_locos_acquired.Brush.Color := clBlue;
  Self.G_locos_acquired.ForeColor := clBlue;
  HVDb.TrakceReleaseAllUsed(Self.OnTrkAllReleased, Self.OnTrkLocoReleased);
end;

procedure TF_Main.OnTrkAllAcquired(Sender: TObject);
begin
  Self.LogStatus('Loko: všechna loko převzata');

  Self.S_locos_acquired.Brush.Color := clLime;
  Self.A_Locos_Acquire.Enabled := false;
  Self.A_Locos_Release.Enabled := true;

  Self.G_locos_acquired.Progress := Self.G_locos_acquired.MaxValue;
  Self.G_locos_acquired.ForeColor := clLime;

  if (SystemData.Status = starting) then
    Self.A_PanelServer_StartExecute(nil);
end;

procedure TF_Main.OnTrkAcquireError(Sender: TObject);
begin
  Self.G_locos_acquired.ForeColor := clRed;
  Self.S_locos_acquired.Brush.Color := clRed;
  Self.A_Locos_Acquire.Enabled := true;

  if (SystemData.Status = TSystemStatus.starting) then
  begin
    SystemData.Status := TSystemStatus.null;
    Self.A_System_Start.Enabled := true;
    Self.A_System_Stop.Enabled := true;
  end;

  Application.MessageBox('Nepodařilo se převzít všechny lokomotivy, více informací v logu.', 'Chyba',
    MB_OK OR MB_ICONWARNING);
end;

procedure TF_Main.OnTrkAllReleased(Sender: TObject);
begin
  Self.LogStatus('Loko: všechna loko odhlášena');

  Self.S_locos_acquired.Brush.Color := clRed;

  Self.A_Locos_Acquire.Enabled := true;
  Self.A_Locos_Release.Enabled := false;

  Self.G_locos_acquired.Progress := 0;
  Self.G_locos_acquired.ForeColor := clBlue;

  if (SystemData.Status = stopping) then
    Self.SetCallMethod(F_Main.A_Trk_DisconnectExecute);
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
    TrakceI.SetTrackStatus(tsOn, TTrakce.Callback(Self.OnDCCGoOk), TTrakce.Callback(Self.OnDCCGoError));
  except
    on E: Exception do
    begin
      SystemData.Status := null;
      Application.MessageBox(PChar('Chyba při DCC GO:' + #13#10 + E.Message), 'Chyba', MB_OK OR MB_ICONERROR);
      Self.LogStatus('DCC: START: ERR ' + E.Message);
    end;
  end;
end;

procedure TF_Main.A_DCC_StopExecute(Sender: TObject);
begin
  Self.LogStatus('DCC: vypínám');

  try
    TrakceI.SetTrackStatus(tsOff, TTrakce.Callback(Self.OnDCCStopOk), TTrakce.Callback(Self.OnDCCStopError));
  except
    on E: Exception do
    begin
      Application.MessageBox(PChar('Chyba při DCC STOP:' + #13#10 + E.Message), 'Chyba', MB_OK OR MB_ICONERROR);
      Self.LogStatus('DCC: STOP: ERR ' + E.Message);
    end;
  end;
end;

procedure TF_Main.A_FuncsSetExecute(Sender: TObject);
begin
  F_FuncsSet.Show();
end;

procedure TF_Main.OnDCCGoOk(Sender: TObject; Data: Pointer);
begin
  TrakceI.emergency := False;
end;

procedure TF_Main.OnDCCGoError(Sender: TObject; Data: Pointer);
begin
  SystemData.Status := TSystemStatus.null;
  Self.UpdateSystemButtons();
  Self.A_DCC_Go.Enabled := true;
  Self.A_DCC_Stop.Enabled := true;
  Self.S_DCC.Brush.Color := clGray;
  Self.LogStatus('DCC: START: ERR: cenrála neodpověděla na příkaz');
  Application.MessageBox('Centrála neodpověděla na příkaz DCC START', 'Varování', MB_OK OR MB_ICONWARNING);
end;

procedure TF_Main.OnDCCStopOk(Sender: TObject; Data: Pointer);
begin
  TrakceI.emergency := False;
end;

procedure TF_Main.OnDCCStopError(Sender: TObject; Data: Pointer);
begin
  TrakceI.emergency := True;
  Self.LogStatus('DCC: STOP: ERR: centrála neodpověděla na příkaz');
  Self.UpdateSystemButtons();
  Self.A_DCC_Go.Enabled := true;
  Self.A_DCC_Stop.Enabled := true;
  Self.S_DCC.Brush.Color := clGray;
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
  Self.A_DCC_Go.Enabled := false;
  Self.A_DCC_Stop.Enabled := false;
  Self.A_FuncsSet.Enabled := false;
  if (F_FuncsSet.Showing) then
    F_FuncsSet.Close();

  HVDb.CSReset();
  Application.ProcessMessages();

  // no clinets shoudl be connected
  // when disconnect called with clients connected, fatal error happened
  PanelServer.BroadcastBottomError('Výpadek systému řízení trakce, ztráta kontroly nad jízdou!', 'TECHNOLOGIE');

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
    Self.A_System_Start.Enabled := true;
    Self.A_System_Stop.Enabled := true;
  end;

  TrakceI.opening := false;
  Self.LogStatus('ERR: Trakce OPEN FAIL: ' + errMsg);
  Application.MessageBox(PChar('Při otevírání Trakce nastala chyba:' + #13#10 + errMsg), 'Chyba',
    MB_OK OR MB_ICONWARNING);
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
    // je DCC
    TrakceI.DCCGoTime := Now;
    Self.S_DCC.Brush.Color := clLime;
    Self.LogStatus('DCC: go');

    if (TrakceI.ConnectedSafe()) then
    begin
      Self.A_DCC_Go.Enabled := false;
      Self.A_DCC_Stop.Enabled := true;
    end;

    if ((SystemData.Status = starting) and (TrakceI.ConnectedSafe())) then
      Self.A_Locos_AcquireExecute(nil);

    PanelServer.DCCStart();
  end else begin

    Areas.BroadcastPlaySound(_SND_ERROR, false, TAreaRights.write);

    // neni DCC
    Self.S_DCC.Brush.Color := clRed;
    Self.LogStatus('DCC: stop');

    if (TrakceI.ConnectedSafe()) then
    begin
      Self.A_DCC_Go.Enabled := true;
      Self.A_DCC_Stop.Enabled := false;
    end;
    if ((SystemData.Status = starting) and (TrakceI.ConnectedSafe())) then
      Self.A_DCC_GoExecute(Self);

    PanelServer.DCCStop();
  end; // else state
end;

procedure TF_Main.A_Turnoff_FunctionsExecute(Sender: TObject);
begin
  Self.LogStatus('Vypínám zvuky hnacích vozidel...');
  Application.ProcessMessages();
  TrakceI.TurnOffSound(TTrakce.Callback(Self.OnTrkAllFunctionTurnedOff),
    TTrakce.Callback(Self.OnTrkAllFunctionTurnedOff));
end;

procedure TF_Main.OnTrkAllFunctionTurnedOff(Sender: TObject; Data: Pointer);
begin
  Self.LogStatus('Zvuky všech hnacích vozidel vypnuty');
  Application.ProcessMessages();
  Self.A_Locos_ReleaseExecute(Self);
end;

/// /////////////////////////////////////////////////////////////////////////////
// TRAKCE END
/// /////////////////////////////////////////////////////////////////////////////

procedure TF_Main.T_MainTimer(Sender: TObject);
begin
  try
    AutostartUpdate();
    Blocks.Update();
    ShowDateTime();
    ModCas.Update();
    JCDb.Update();
    MultiJCDb.Update();
    Boosters.Update();
    Areas.Update();
    UpdateCallMethod();
    RCSd.Update();
    TrakceI.Update();
    ABlist.Update();
    trains.UpdateTraveled(Self.T_Main.Interval);
  except
    on E: Exception do
    begin
      if (not log_last_error) then
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

procedure TF_Main.PM_PropertiesClick(Sender: TObject);
begin
  if (LV_HV.Selected <> nil) then
    F_HVEdit.OpenForm(HVDb[StrToInt(LV_HV.Selected.Caption)]);
end;

procedure TF_Main.PC_1Change(Sender: TObject);
begin
  Self.DisableRemoveButtons();

  if (PC_1.ActivePage = TS_VC) then
    JCTableData.UpdateTable;
  if (PC_1.ActivePage = TS_MultiJC) then
    MultiJCTableData.UpdateTable;
  if (PC_1.ActivePage = TS_Users) then
    UsersTableData.UpdateTable;
  if (PC_1.ActivePage = TS_Bloky) then
    BlocksTablePainter.UpdateTable();
  if (PC_1.ActivePage = TS_Zesilovace) then
    ZesTableData.UpdateTable();
  if (PC_1.ActivePage = TS_Soupravy) then
    TrainTableData.UpdateTable();
  if (PC_1.ActivePage = Self.TS_HV) then
    HVTableData.UpdateTable();
  if (PC_1.ActivePage = TS_Stanice) then
    ORsTableData.UpdateTable(true);
  if (PC_1.ActivePage = TS_Technologie) then
    PanelServer.GUIRefreshTable();
end;

procedure TF_Main.PM_BlokyPopup(Sender: TObject);
begin
  if (Self.LV_Blocks.Selected = nil) then
  begin
    for var item in (Sender as TPopupMenu).Items do
      item.Enabled := false;
  end else begin
    var blk := Blocks.GetBlkByIndex(Self.LV_Blocks.ItemIndex);
    if (blk = nil) then
      Exit();

    Self.MI_BlockState.Enabled := (blk <> nil) and ((blk.typ = btTurnout) or (blk.typ = btTrack) or (blk.typ = btRT) or (blk.typ = btCrossing) or (blk.typ = btSignal) or (blk.typ = btRailway));
    Self.MI_Houk.Enabled := (blk <> nil) and ((blk.typ = btTrack) or (blk.typ = btRT));
    Self.MI_Prop.Enabled := true;
  end;
end;

procedure TF_Main.PM_ResetVClick(Sender: TObject);
begin
  if (Application.MessageBox('Pozor: tato operace zaráz přestaví všechny výhybky na kolejišti, ' +
    'což může způsobit přetížení napájecích zdrojů. Chcete skutečně pokračovat?', 'Otázka',
    MB_YESNO OR MB_ICONWARNING OR MB_DEFBUTTON2) = mrYes) then
  begin
    Blocks.MoveTurnoutBasicPosition();
    Log('Vyhýbky přestaveny do základní polohy', llInfo);
    Application.MessageBox('Výhybky přestaveny do záklaních poloh.', 'Informace', MB_OK OR MB_ICONINFORMATION);
  end;
end;

procedure TF_Main.PM_RegulatorClick(Sender: TObject);
begin
  if (Self.LV_HV.Selected = nil) then
    Exit();

  if (TrakceI.ConnectedSafe()) then
  begin
    try
      RegCollector.Open(HVDb[StrToInt(Self.LV_HV.Selected.Caption)]);
    except
      on E: Exception do
        Application.MessageBox(PChar(E.Message), 'Varování', MB_OK OR MB_ICONWARNING);
    end;
  end; // if
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
  if (Integer(ci) > 0) then
    CanClose := false;

  case (ci) of
    TCloseInfo.ci_system_changing:
      begin
        Log('Pokus o zavření okna při zapínání nebo vypínání systémů', llWarning);
        Application.MessageBox(PChar('Technologie právě zapíná nebo vypíná systémy, aplikaci nelze momentálně zavřít.' +
          #13#10 + 'Nouzové ukončení programu lze provést spuštěním příkazu "app-exit" v konzoli'),
          'Nelze ukončit program', MB_OK OR MB_ICONWARNING);
      end;

    TCloseInfo.ci_system_started:
      begin
        Log('Pokus o zavření okna bez ukončení komunikace se systémy', llWarning);
        if (Application.MessageBox('Program není odpojen od systémů, odpojit od systémů?', 'Nelze ukončit program',
          MB_YESNO OR MB_ICONWARNING) = mrYes) then
          Self.A_System_StopExecute(Self);
      end;

    TCloseInfo.ci_rcs:
      begin
        Log('Pokus o zavření okna bez uzavření RCS', llWarning);
        if (Application.MessageBox('Program není odpojen od RCS, odpojit?', 'Nelze ukončit program',
          MB_YESNO OR MB_ICONWARNING) = mrYes) then
        begin
          try
            if (RCSi.started) then
              RCSi.Stop()
            else if (RCSi.opened) then
              RCSi.Close();
          except
            on E: Exception do
              Application.MessageBox(PChar('Nastala výjimka : ' + E.Message), 'Chyba', MB_OK OR MB_ICONERROR);
          end;
        end;
      end;

    TCloseInfo.ci_server:
      begin
        Log('Pokus o zavření okna bez vypnutí panel serveru', llWarning);
        if (Application.MessageBox('PanelServer stále běží, vypnout?', 'Nelze ukončit program',
          MB_YESNO OR MB_ICONWARNING) = mrYes) then
          PanelServer.Stop();
      end;

    TCloseInfo.ci_trakce:
      begin
        Log('Pokus o zavření okna bez odpojení od centrály', llWarning);
        if (Application.MessageBox('Program není odpojen od centrály, odpojit?', 'Nelze ukončit program',
          MB_YESNO OR MB_ICONWARNING) = mrYes) then
          TrakceI.Disconnect();
      end;

    TCloseInfo.ci_yes:
      begin
        if (Self.CloseMessage) then
        begin
          CanClose := (Application.MessageBox('Opravdu chcete ukončit program?', 'hJOPserver',
            MB_YESNO OR MB_ICONQUESTION OR MB_DEFBUTTON2) = mrYes);
        end else begin
          CanClose := true;
        end;
      end;

  end; // case
end;

procedure TF_Main.FormCreate(Sender: TObject);
begin
  Self.CloseMessage := true;
  Self.mCpuLoad := TCpuLoad.Create();

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
  RCSi.AfterOpen := Self.OnRCSOpen;
  RCSi.AfterClose := Self.OnRCSClose;
  RCSi.AfterStart := Self.OnRCSStart;
  RCSi.AfterStop := Self.OnRCSStop;
  RCSi.OnScanned := Self.OnRCSScanned;
  RCSi.OnReady := Self.OnRCSReady;

  // assign Trakce events:
  TrakceI.BeforeOpen := Self.OnTrkBeforeOpen;
  TrakceI.AfterOpen := Self.OnTrkAfterOpen;
  TrakceI.BeforeClose := Self.OnTrkBeforeClose;
  TrakceI.AfterClose := Self.OnTrkAfterClose;
  TrakceI.OnReady := Self.OnTrkReady;
  TrakceI.OnTrackStatusChanged := Self.OnTrkStatusChange;
  TrakceI.OnOpenError := Self.OnTrkErrOpen;

  TrakceI.LogObj := Self.LV_log_lnet;

  FuncNames.OnChange := Self.OnFuncNameChange;

  Self.LoadIniLibData();

  Self.Caption := 'hJOPserver – v' + VersionStr(Application.ExeName) +
    ' (build ' + FormatDateTime('dd.mm.yyyy', BuildDateTime()) + ')';
  Self.SB1.Panels.Items[_SB_RCS].Text := 'RCS zavřeno';
end;

procedure TF_Main.FormDestroy(Sender: TObject);
begin
  Self.mCpuLoad.Free();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_Main.AE_1Message(var Msg: tagMSG; var Handled: Boolean);
begin
  Handled := false;

  // STISK KLAVESY
  case (Msg.Message) of
    WM_KEYDOWN:
      begin
        Handled := false;
        RegCollector.KeyPress(Msg.wParam, Handled);
        if (Handled) then
          Exit();

        case (Msg.wParam) of
          VK_F9:
            begin
              try
                RCSi.HideConfigDialog();
              except
                on E: Exception do
                  Application.MessageBox(PChar('Nelze skrýt konfigurační dialog RCS : ' + E.Message), 'Varování',
                    MB_OK OR MB_ICONWARNING);
              end;
            end;

          VK_ESCAPE:
            if (F_About.Showing) then
              F_About.Close;
        end; // case
      end;
  end;
end;

procedure TF_Main.WMPowerBroadcast(var Msg: TMessage);
begin
  case (Msg.wParam) of
    PBT_APMQUERYSUSPEND:
      begin
        Msg.Result := BROADCAST_QUERY_DENY;
      end;

    PBT_APMSUSPEND:
      begin
        // windows is going to sleep -> disconnect all devices
        if (TrakceI.ConnectedSafe()) then
        begin
          PanelServer.Stop();
          try
            TrakceI.EmergencyStop();
            TrakceI.Disconnect();
          except

          end;
        end;

        try
          if (RCSi.started) then
            RCSi.Stop();
          if (RCSi.opened) then
            RCSi.Close();
        except

        end;
      end;

  end; // case
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
  if (Msg.EndSession = true) then
  begin
    if (TrakceI.ConnectedSafe()) then
    begin
      PanelServer.Stop();
      try
        TrakceI.EmergencyStop();
        TrakceI.Disconnect();
      except

      end;
    end;

    try
      if (RCSi.started) then
        RCSi.Stop();
      if (RCSi.opened) then
        RCSi.Close();
    except

    end;

    Self.CloseMessage := false;
    Self.NUZClose := true;
    Self.Close();
  end;
  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_Main.A_PanelServer_StartExecute(Sender: TObject);
begin
  if ((SystemData.Status = starting) and (not Blocks.Enabled)) then
  begin
    try
      Blocks.Enable();
    except
      on E: Exception do
      begin
        SystemData.Status := TSystemStatus.null;
        Self.UpdateSystemButtons();
        Application.MessageBox(PChar('Chyba při aktivaci bloků:' + #13#10 + E.Message), 'Chyba', MB_OK OR MB_ICONWARNING);
        Exit();
      end;
    end;
  end;

  try
    PanelServer.Start();
    Self.A_PanelServer_Start.Enabled := false;
    Self.A_PanelServer_Stop.Enabled := true;
    Self.UpdateSystemButtons();
  except
    on E: Exception do
    begin
      SystemData.Status := TSystemStatus.null;
      Self.UpdateSystemButtons();
      Application.MessageBox(PChar('Chyba při zapínání panelServeru:' + #13#10 + E.Message), 'Chyba',
        MB_OK OR MB_ICONWARNING);
      Exit();
    end;
  end;
end;

procedure TF_Main.A_PanelServer_StopExecute(Sender: TObject);
begin
  PanelServer.Stop();

  Self.A_PanelServer_Start.Enabled := true;
  Self.A_PanelServer_Stop.Enabled := false;
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
      Application.MessageBox(PChar('Nelze nastartovat PT server:' + #13#10 + E.Message), 'Chyba',
        MB_OK OR MB_ICONWARNING);
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
      Application.MessageBox(PChar('Nelze zastavit PT server:' + #13#10 + E.Message), 'Chyba', MB_OK OR MB_ICONWARNING);
  end;

  Self.UpdateSystemButtons();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_Main.A_SaveStavExecute(Sender: TObject);
var ini: TMemIniFile;
begin
  try
    // ukladani stavu bloku: ulozime do docasneho souboru a az pak prepiseme stavajici konfigurak
    Blocks.SaveStatToFile(Blocks.fstatus + '_');

    if (FileExists(Blocks.fstatus)) then
      DeleteFile(Blocks.fstatus);
    MoveFile(PChar(Blocks.fstatus + '_'), PChar(Blocks.fstatus));
    DeleteFile(Blocks.fstatus + '_');
  except
    on E: Exception do
      AppEvents.LogException(E, 'Blocks.SaveStatToFile');
  end;

  try
    HVDb.SaveState(Self.E_dataload_HV_state.Text + '_');

    if (FileExists(Self.E_dataload_HV_state.Text)) then
      DeleteFile(Self.E_dataload_HV_state.Text);
    MoveFile(PChar(Self.E_dataload_HV_state.Text + '_'), PChar(Self.E_dataload_HV_state.Text));
    DeleteFile(Self.E_dataload_HV_state.Text + '_');
  except
    on E: Exception do
      AppEvents.LogException(E, 'HvDb.SaveToDir');
  end;

  try
    Trains.SaveData(Self.E_dataload_soupr.Text + '_');

    if (FileExists(Self.E_dataload_soupr.Text)) then
      DeleteFile(Self.E_dataload_soupr.Text);
    MoveFile(PChar(Self.E_dataload_soupr.Text + '_'), PChar(Self.E_dataload_soupr.Text));
    DeleteFile(Self.E_dataload_soupr.Text + '_');
  except
    on E: Exception do
      AppEvents.LogException(E, 'Trains.SaveData');
  end;

  try
    FormData.SaveFormData(FormData.aFile);
  except
    on E: Exception do
      AppEvents.LogException(E, 'Save form position');
  end;

  try
    Areas.SaveState(Areas.status_filename);
  except
    on E: Exception do
      AppEvents.LogException(E, 'Save OR status');
  end;

  try
    ini := TMemIniFile.Create(_INIDATA_FN, TEncoding.UTF8);
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

      ini.WriteInteger(_INIDATA_PATHS_LOG_SECTION, 'main-file-loglevel', Self.CB_global_loglevel_file.ItemIndex);
      ini.WriteInteger(_INIDATA_PATHS_LOG_SECTION, 'main-table-loglevel', Self.CB_global_loglevel_table.ItemIndex);
      ini.WriteBool(_INIDATA_PATHS_LOG_SECTION, 'rcs', Self.CHB_log_rcs.Checked);
      ini.WriteBool(_INIDATA_PATHS_LOG_SECTION, 'auth', Self.CHB_log_auth.Checked);

      ini.UpdateFile();
    finally
      ini.Free();
    end;
  except
    on E: Exception do
      AppEvents.LogException(E, 'Save cfg');
  end;

  try
    ini := TMemIniFile.Create(ExtractRelativePath(ExtractFilePath(Application.ExeName), Self.E_configFilename.Text),
      TEncoding.UTF8);
    try
      ModCas.SaveData(ini);
      ini.WriteString('funcsVyznam', 'funcsVyznam', FuncNames.AllNames());
      ini.WriteBool('RCS', 'ShowOnlyActive', Self.CHB_RCS_Show_Only_Active.Checked);
      ini.UpdateFile();
    finally
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
    Application.MessageBox(PChar('Systém nelze spustit, RCS není připraveno k zapnutí systému' + #13#10 +
      'Možné příčiny:' + #13#10 + ' - nenačtena validní knihovna'), 'Nelze spustit', MB_OK OR MB_ICONWARNING);
    Self.LogStatus('ERR: Systém nelze spustit, RCS není připraveno k zapnutí systému');
    Exit();
  end;
  if (not TrakceI.ready) then
  begin
    Application.MessageBox(PChar('Systém nelze spustit, Trakce není připravena k zapnutí systému' + #13#10 +
      'Možné příčiny:' + #13#10 + ' - nenačtena validní knihovna'), 'Nelze spustit', MB_OK OR MB_ICONWARNING);
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

  Self.LogStatus('Zastavuji všechny vlaky...');
  Trains.StopAllTrains();

  Application.ProcessMessages();

  if (PtServer.openned) then
    Self.A_PT_StopExecute(nil);

  Self.LogStatus('Odpojuji panely...');
  Areas.DisconnectPanels();
  Self.A_PanelServer_StopExecute(nil);

  JCDb.CancelAll();
  Blocks.Disable();
  Trains.ClearPOdj();
  Blocks.Reset();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_Main.SetCallMethod(Method: TNotifyEvent);
begin
  while (Assigned(Self.call_method)) do
  begin
    Application.ProcessMessages();
    sleep(1);
  end;
  Self.call_method := Method;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_Main.B_AB_DeleteClick(Sender: TObject);
var jc: TJC;
begin
  jc := ABlist[Self.LV_AB.ItemIndex];
  if ((Self.LV_AB.Selected <> nil) and (Application.MessageBox(PChar('Opravdu smazat jízdní cestu ' + jc.name + '?'),
    'Opravdu?', MB_YESNO OR MB_ICONQUESTION) = mrYes)) then
  begin
    try
      var signal := Blocks.GetBlkSignalByID(jc.Data.signalId);
      if ((signal <> nil) and (signal.ABJC = jc)) then
      begin
        signal.ABJC := nil;
        if (ABlist.Contains(jc)) then
          ABlist.Remove(ABlist[Self.LV_AB.ItemIndex]);
      end
      else
        ABlist.Remove(ABlist[Self.LV_AB.ItemIndex]);
    except
      on E: Exception do
        Application.MessageBox(PChar('Chyba při mazání:' + #13#10 + E.Message), 'Chyba', MB_OK OR MB_ICONERROR);
    end;
  end;
end;

procedure TF_Main.B_BlkAddClick(Sender: TObject);
begin
  F_BlkNew.OpenForm;
end;

procedure TF_Main.B_BlkDeleteClick(Sender: TObject);
begin
  var i := LV_Blocks.ItemIndex;

  Beep;
  if Application.MessageBox(PChar('Opravdu chcete smazazat blok ' + Blocks.GetBlkIndexName(i) + '?'), 'Mazání bloku',
    MB_YESNO OR MB_ICONQUESTION OR MB_DEFBUTTON2) = mrYes then
  begin
    try
      Blocks.Delete(i);
    except
      on E: Exception do
        Application.MessageBox(PChar('Chyba:' + #13#10 + E.Message), 'Chyba', MB_OK OR MB_ICONWARNING);
    end;
  end; // if MesageBox
end;

procedure TF_Main.B_ChangeClick(Sender: TObject);
var Data: string;
begin
  Data := '';
  for var i := 0 to Self.M_funcsVyznam.Lines.Count - 1 do
    if (Self.M_funcsVyznam.Lines[i] <> '') then
      Data := Data + '{' + Self.M_funcsVyznam.Lines[i] + '};';
  FuncNames.ParseWholeList(Data);
  PanelServer.BroadcastFuncsDescription();
end;

procedure TF_Main.B_ClearStatsClick(Sender: TObject);
begin
  if (Application.MessageBox('Opravdu smazat najeté bloky a kilometry všech hnacích vozidel?', 'Opravdu?',
    MB_YESNO OR MB_ICONQUESTION OR MB_DEFBUTTON2) = mrYes) then
    HVDb.ClearAllStatistics();
end;

procedure TF_Main.OnFuncNameChange(Sender: TObject);
begin
  if (not Self.CHB_LoadChanges.Checked) then
    Exit();
  Self.M_funcsVyznam.Clear();
  for var name in FuncNames.Items do
    Self.M_funcsVyznam.Lines.Add(name.GetPanelStr());

  var strs := TStringList.Create();
  try
    for var name in FuncNames.Items do
      strs.Add(name.name);
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
        Application.MessageBox(PChar('Nelze exportovat' + #13#10 + E.Message), 'Chyba', MB_OK OR MB_ICONERROR);
    end;
  end;
end;

procedure TF_Main.B_HV_AddClick(Sender: TObject);
begin
  F_HVEdit.NewHV();
end;

procedure TF_Main.B_HV_DeleteClick(Sender: TObject);
begin
  if (Self.LV_HV.Selected = nil) then
    Exit();

  var hvs := Self.LVSelectedTexts(Self.LV_HV, 'HV', 'HV');

  if (Application.MessageBox(PChar('Opravdu smazat ' + hvs + '?'), '?', MB_YESNO OR MB_ICONQUESTION) = mrYes) then
  begin
    for var i := Self.LV_HV.Items.Count - 1 downto 0 do
    begin
      var LI: TListItem := Self.LV_HV.Items[i];
      if (LI.Selected) then
      begin
        var addr := StrToInt(LI.Caption);
        try
          HVDb.Remove(addr);
        except
          on E: Exception do
          begin
            Application.MessageBox(PChar('Mazání HV ' + IntToStr(addr) + ' se nezdařilo:' + #13#10 + E.Message),
              'Chyba', MB_OK OR MB_ICONWARNING);
            Exit();
          end;
        end;
      end;
    end;
  end;
end;

procedure TF_Main.B_JC_ResetClick(Sender: TObject);
var jc: TJC;
begin
  if (Self.LV_JC.Selected = nil) then
    Exit();

  jc := JCDb.GetJCByIndex(Self.LV_JC.ItemIndex);
  if (jc.activating) then
    jc.CancelActivating('Nouzové rušení stavění JC');
end;

procedure TF_Main.B_lok_deleteClick(Sender: TObject);
var sprs: string;
  LI: TListItem;
  i: Integer;
begin
  if (Self.LV_Soupravy.Selected = nil) then
    Exit();
  if (not Assigned(Trains[Self.LV_Soupravy.ItemIndex])) then
    Exit();

  sprs := Self.LVSelectedTexts(Self.LV_Soupravy, 'soupravu', 'soupravy');

  if (Application.MessageBox(PChar('Opravdu smazat ' + sprs + '?'), '?', MB_YESNO OR MB_ICONQUESTION) = mrYes) then
  begin
    for i := Self.LV_Soupravy.Items.Count - 1 downto 0 do
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
    (Application.MessageBox(PChar('Chcete použít složenou JC ' + MultiJCDb[Self.LV_MultiJC.ItemIndex].name +
    ' jako šablonu pro vytvoření nové složené JC?'), 'Nová složená JC', MB_YESNO OR MB_ICONQUESTION OR MB_DEFBUTTON1)
    = mrYes)) then
    F_MJCEdit.NewMJC(MultiJCDb[Self.LV_MultiJC.ItemIndex])
  else
    F_MJCEdit.NewMJC(nil);
end;

procedure TF_Main.B_mJC_RemoveClick(Sender: TObject);
var mjcs: string;
  LI: TListItem;
  i: Integer;
begin
  if (Self.LV_MultiJC.Selected = nil) then
    Exit();

  mjcs := Self.LVSelectedTexts(Self.LV_MultiJC, 'cestu', 'cesty');

  if (Application.MessageBox(PChar('Opravdu smazat ' + mjcs + '?'), '?', MB_YESNO OR MB_ICONQUESTION) = mrYes) then
  begin
    for i := Self.LV_MultiJC.Items.Count - 1 downto 0 do
    begin
      LI := Self.LV_MultiJC.Items[i];
      if (LI.Selected) then
        MultiJCDb.Remove(LI.Index);
    end;
  end;
end;

procedure TF_Main.B_RemoveStackClick(Sender: TObject);
var Area: TArea;
begin
  if (Self.LV_Stanice.Selected = nil) then
    Exit();
  Area := Areas[Self.LV_Stanice.ItemIndex];
  if (Application.MessageBox(PChar('Opravdu smazat zásobník jízdních cest stanice ' + Area.name + ' ?'), 'Opravdu?',
    MB_YESNO OR MB_ICONQUESTION) = mrYes) then
    Area.stack.Clear();
end;

procedure TF_Main.B_User_AddClick(Sender: TObject);
begin
  F_UserEdit.NewUser();
end;

procedure TF_Main.B_User_DeleteClick(Sender: TObject);
begin
  if (Application.MessageBox(PChar('Opravdu smazat uživatele ' + Self.LV_Users.Selected.SubItems[0] + ' ?'), 'Opravdu?',
    MB_YESNO OR MB_ICONQUESTION) = mrYes) then
  begin
    UsrDB.RemoveUser(Self.LV_Users.ItemIndex);
    Self.B_User_Delete.Enabled := false;
  end;
end;

procedure TF_Main.B_VC_AddClick(Sender: TObject);
begin
  if ((Self.LV_JC.Selected <> nil) and
    (Application.MessageBox(PChar('Chcete použít JC ' + JCDb[Self.LV_JC.ItemIndex].name +
    ' jako šablonu pro vytvoření nové JC?'), 'Nová JC', MB_YESNO OR MB_ICONQUESTION OR MB_DEFBUTTON1) = mrYes)) then
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
  if (Self.LV_JC.Selected = nil) then
    Exit();

  jcs := Self.LVSelectedTexts(Self.LV_JC, 'cestu', 'cesty');

  if (Application.MessageBox(PChar('Opravdu smazat ' + jcs + '?'), '?', MB_YESNO OR MB_ICONQUESTION) = mrYes) then
  begin
    for i := Self.LV_JC.Items.Count - 1 downto 0 do
    begin
      LI := Self.LV_JC.Items[i];
      if (LI.Selected) then
      begin
        try
          jc := JCDb.GetJCByIndex(LI.Index);
          if (ABlist.Contains(jc)) then
            ABlist.Remove(jc);
          JCDb.Remove(LI.Index);
        except
          on E: Exception do
          begin
            Application.MessageBox(PChar('Nelze smazat JC' + #13#10 + E.Message), 'Chyba', MB_OK OR MB_ICONERROR);
            Exit();
          end;
        end;
      end;
    end;
  end;
end;

procedure TF_Main.B_zes_addClick(Sender: TObject);
begin
  F_Booster_Edit.NewBooster();
end;

procedure TF_Main.B_zes_deleteClick(Sender: TObject);
var pozice: Integer;
begin
  pozice := LV_Zesilovace.ItemIndex;
  Beep;
  if Application.MessageBox(PChar('Opravdu chcete smazat zesilovač ' + Boosters.sorted[pozice].name + '?'),
    'Mazání zesilovace', MB_YESNO OR MB_ICONQUESTION OR MB_DEFBUTTON2) = mrYes then
  begin
    Boosters.Remove(Boosters.sorted[pozice].id);
    LV_Zesilovace.Items.Delete(pozice);
  end; // if MessageBox
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_Main.PM_Help_RPClick(Sender: TObject);
begin
  F_About.ShowModal;
end;

procedure TF_Main.PM_HVPopup(Sender: TObject);
var i: Integer;
begin
  if (Self.LV_HV.Selected = nil) then
  begin
    for i := 0 to (Sender as TPopupMenu).Items.Count - 1 do
      (Sender as TPopupMenu).Items.Items[i].Enabled := false;
  end else begin
    for i := 0 to (Sender as TPopupMenu).Items.Count - 1 do
      (Sender as TPopupMenu).Items.Items[i].Enabled := true;
  end;
end;

procedure TF_Main.PM_SB1Click(Sender: TObject);
begin
  SB1.Visible := PM_SB1.Checked;
  if (PM_SB1.Checked) then
    Log('Zobrazeno SB1', llInfo)
  else
    Log('Skryto SB1', llInfo);
end;

procedure TF_Main.T_GUI_refreshTimer(Sender: TObject);
begin
  try
    mCpuLoad.RefreshCPUGauge();

    // update tables
    if (Self.Showing) then
    begin
      if (Self.PC_1.ActivePage = Self.TS_Bloky) then
        BlocksTablePainter.UpdateTable();
      if (Self.PC_1.ActivePage = Self.TS_Soupravy) then
        TrainTableData.UpdateTable();
      if (Self.PC_1.ActivePage = Self.TS_Zesilovace) then
        ZesTableData.UpdateTable();
      if (Self.PC_1.ActivePage = Self.TS_HV) then
        HVTableData.UpdateTable();
      if (Self.PC_1.ActivePage = Self.TS_VC) then
        JCTableData.UpdateTable();
      if (Self.PC_1.ActivePage = Self.TS_MultiJC) then
        MultiJCTableData.UpdateTable();
      if (Self.PC_1.ActivePage = Self.TS_Stanice) then
        ORsTableData.UpdateTable();
      if (Self.PC_1.ActivePage = Self.TS_Technologie) then
        PanelServer.GUIRefreshFromQueue();

      ABTableData.Update();
    end;

    HVDb.UpdateTokenTimeout();
    Config.UpdateAutosave();
  except
    on E: Exception do
    begin
      if (not log_last_error) then
        AppEvents.LogException(E, 'Function timer exception');
    end;
  end;
end;

procedure TF_Main.T_clear_log_msgTimer(Sender: TObject);
begin
  if (Self.sb1Log) then
  begin
    Self.SB1.Panels.Items[_SB_LOG].Text := '';
    Self.sb1Log := false;
  end;
end;

procedure TF_Main.CHB_log_authClick(Sender: TObject);
begin
  Logging.auth_logging := Self.CHB_Log_Auth.Checked;
end;

procedure TF_Main.CHB_log_rcsClick(Sender: TObject);
begin
  RCSi.Log := Self.CHB_log_rcs.Checked;
end;

procedure TF_Main.CHB_RCS_Show_Only_ActiveClick(Sender: TObject);
begin
  if (RCSi.Lib <> '') then
    RCSTableData.LoadToTable(not Self.CHB_RCS_Show_Only_Active.Checked);
end;

procedure TF_Main.CloseForm();
begin
  Log('########## Probíhá ukončování hJOPserver ##########', llInfo);

  Self.T_Main.Enabled := false;
  Self.T_GUI_refresh.Enabled := false;
  Self.T_clear_log_msg.Enabled := false;
  JCSimulator.timer.Enabled := false;
  RailwaySimulator.timer.Enabled := false;
  TurnoutSimulator.timer.Enabled := false;

  Self.A_SaveStavExecute(Self);
  TrakceI.LogObj := nil;

  Log('###############################################', llInfo);
end;

procedure TF_Main.RepaintObjects();
begin
  Self.SB1.Panels.Items[0].Width := Self.ClientWidth - Self.SB1.Panels.Items[1].Width - Self.SB1.Panels.Items[2].Width -
    Self.SB1.Panels.Items[3].Width - Self.SB1.Panels.Items[4].Width - Self.SB1.Panels.Items[5].Width;
  Self.P_Zrychleni.Left := Self.ClientWidth - Self.P_Zrychleni.Width - 5;
  Self.P_Time_modelovy.Left := Self.P_Zrychleni.Left - Self.P_Time_modelovy.Width - 5;
  Self.P_Time.Left := Self.P_Time_modelovy.Left - Self.P_Time.Width - 5;
  Self.P_Date.Left := Self.P_Time.Left - Self.P_Date.Width - 5;

  Self.GB_Connected_Panels.Height := Self.TS_Technologie.Height - Self.GB_Connected_Panels.Top - 10;

  Self.GB_Connected_Panels.Width := Self.TS_Technologie.ClientWidth - 2*Self.GB_Connected_Panels.Left;
  Self.GB_Log.Width := Self.TS_Technologie.Width - Self.GB_Log.Left - Self.GB_stav_technologie.Left;

  Self.P_Config.Top := ((Self.TS_Config.Height-Self.P_ConfigHeader.Height) div 2) - (Self.P_Config.Height div 2) + Self.P_ConfigHeader.Height;
  Self.P_Config.Left := (Self.TS_Config.Width div 2) - (Self.P_Config.Width div 2);
end;

procedure TF_Main.FormResize(Sender: TObject);
begin
  Self.RepaintObjects();
end;

procedure TF_Main.FormShow(Sender: TObject);
begin
  Self.RepaintObjects();
end;

procedure TF_Main.L_DateDblClick(Sender: TObject);
begin
  Application.MessageBox('Datum a čas lze nastavit v operačním systému', 'Informace', MB_ICONINFORMATION OR MB_OK OR
    MB_DEFBUTTON1);
end;

procedure TF_Main.MI_DisconnectClick(Sender: TObject);
begin
  if (PanelServer.GetClient(Self.LV_Clients.ItemIndex) <> nil) then
  begin
    try
      PanelServer.DisconnectClient(PanelServer.GetClient(Self.LV_Clients.ItemIndex).connection);
    except
      on E: Exception do
        Application.MessageBox(PChar('Výjimka při odpojování - ' + E.Message), 'Chyba', MB_OK OR MB_ICONWARNING);
    end;
  end;
end;

procedure TF_Main.MI_HoukClick(Sender: TObject);
begin
  if (Self.LV_Blocks.Selected = nil) then
    Exit();
  var blk := Blocks.GetBlkByIndex(Self.LV_Blocks.ItemIndex);
  if (blk = nil) then
    Exit();
  if ((blk.typ <> btTrack) and (blk.typ <> btRT)) then
    Exit();

  F_HoukEvsUsek.Open(TBlkTrack(blk));
end;

procedure TF_Main.MI_PropClick(Sender: TObject);
begin
  if (Self.LV_Blocks.Selected <> nil) then
    Self.LV_BlocksDblClick(Self.LV_Blocks);
end;

procedure TF_Main.MI_RCS_UpdateClick(Sender: TObject);
begin
  try
    Self.UpdateRCSLibsList();
    Application.MessageBox('Seznam knihoven úspěšně aktualizován.', 'Info', MB_OK OR MB_ICONINFORMATION);
  except
    on E: Exception do
      Application.MessageBox(PChar('Seznam knihoven se nepodařilo aktualizovat:' + #13#10 + E.Message), 'Chyba',
        MB_OK OR MB_ICONWARNING);
  end;
end;

procedure TF_Main.MI_Save_configClick(Sender: TObject);
var inidata: TMemIniFile;
begin
  Application.ProcessMessages();
  Screen.Cursor := crHourGlass;

  try
    inidata := TMemIniFile.Create(_INIDATA_FN, TEncoding.UTF8);
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
      Application.MessageBox(PChar('Výjimka: ' + E.Message), 'Výjimka', MB_OK OR MB_ICONERROR);
    end;
  end;

  Screen.Cursor := crDefault;
end;

procedure TF_Main.MI_SimulationDiagnosticsClick(Sender: TObject);
begin
  F_Admin.Show();
end;

procedure TF_Main.MI_BlockStateClick(Sender: TObject);
var blk: TBlk;
begin
  if (LV_Blocks.Selected = nil) then
    Exit();
  blk := Blocks.GetBlkByIndex(Self.LV_Blocks.ItemIndex);
  if (blk = nil) then
    Exit();

  case (blk.typ) of
    btTurnout:
      F_BlkTurnoutState.Open(blk as TBlkTurnout);
    btTrack, btRT:
      F_BlkTrackState.Open(blk as TBlkTrack);
    btRailway:
      F_BlkRailwayState.Open(blk as TBlkRailway);
    btCrossing:
      F_BlkCrossingState.Open(blk as TBlkCrossing);
    btSignal:
      F_BlkSignalState.Open(blk as TBlkSignal);
  end;
end;

procedure TF_Main.LB_LogDblClick(Sender: TObject);
begin
  if (Application.MessageBox('Smazat obsah seznamu?', 'Smazat?', MB_YESNO OR MB_ICONQUESTION OR MB_DEFBUTTON2) = mrYes)
  then
    Self.LB_Log.Clear();
end;

procedure TF_Main.LB_LogDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  with Control as TListBox do
  begin
    if (Copy(Items[Index], 12, 3) = 'ERR') then
    begin
      Canvas.Brush.Color := $AAAAFF;
      Canvas.Font.Color := clWhite;
    end else if (Copy(Items[Index], 12, 4) = 'WARN') then
    begin
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
  inidata := TMemIniFile.Create(_INIDATA_FN, TEncoding.UTF8);
  try
    Self.CB_global_loglevel_file.ItemIndex := inidata.ReadInteger(_INIDATA_PATHS_LOG_SECTION, 'main-file-loglevel', 3);
    Self.CB_global_loglevel_table.ItemIndex := inidata.ReadInteger(_INIDATA_PATHS_LOG_SECTION, 'main-table-loglevel', 3);
    Self.CHB_log_rcs.Checked := inidata.ReadBool(_INIDATA_PATHS_LOG_SECTION, 'rcs', false);
    Self.CHB_log_auth.Checked := inidata.ReadBool(_INIDATA_PATHS_LOG_SECTION, 'auth', false);

    RCSi.Log := Self.CHB_log_rcs.Checked;
    Logging.auth_logging := Self.CHB_Log_Auth.Checked;
  finally
    inidata.Free();
  end;
end;

procedure TF_Main.AutostartUpdate();
begin
  if (Self.autostart.state = asDisabled) then
    Exit();
  if (GetFunctions.GetSystemStart()) then
  begin
    Self.autostart.state := asDisabled;
    Exit();
  end;

  F_AutoStartSystems.L_Cas.Caption := FormatDateTime('ss', Now - Self.autostart.goTime);
  if (Self.autostart.state = asEnabled) then
  begin
    Log('Probiha automaticke pripojovani k systemum - t=6s', llInfo);
    F_AutoStartSystems.Show();
    Self.autostart.goTime := Now + EncodeTime(0, 0, 6, 0);
    Self.autostart.state := asWaiting;
  end else if (Self.autostart.state = asWaiting) then begin           
    if (Round((Now - Self.autostart.goTime) * 24 * 3600) = 0) then
    begin
      Log('Automaticke pripojovani k systemum - t=0 - zapinam systemy', llInfo);
      F_AutoStartSystems.Close();
      Self.autostart.state := asDisabled;
      F_Main.A_System_StartExecute(nil);
    end;
  end;
end;

procedure TF_Main.ShowDateTime();
begin
  P_Date.Caption := FormatDateTime('dd. mm. yyyy', Now);
  P_Time.Caption := FormatDateTime('hh:mm:ss', Now);
end;

procedure TF_Main.OnStart();
begin
  mCpuLoad.CreateCPUGauge();

  Log('Spuštěn hJOPserver v' + VersionStr(Application.ExeName), llInfo);
  Log('----------------------------------------------------------------', llInfo);

  if (not Self.CloseMessage) then
  begin
    Self.Close();
    Exit();
  end;

  F_splash.PB_Prubeh.Position := F_splash.PB_Prubeh.Max;
  F_splash.AddStav('Téměř spuštěno...');

  BlocksTablePainter.LoadTable();
  JCTableData.LoadToTable();
  RCSTableData.LoadToTable(not Self.CHB_RCS_Show_Only_Active.Checked);
  UsersTableData.LoadToTable();
  ORsTableData.LoadToTable();

  Self.PC_1.ActivePage := TS_Technologie;

  PanelServer.GUIInitTable();
  ModCas.UpdateGUIColors();
  Self.FillGlobalConfig();

  Self.Visible := true;

  Self.T_Main.Enabled := true;
  Self.T_GUI_refresh.Enabled := true;
  Self.T_clear_log_msg.Enabled := true;

  Self.UpdateRCSLibsList();
  Self.UpdateTrkLibsList();

  Self.CB_centrala_loglevel_file.ItemIndex := Integer(TrakceI.logLevelFile);
  Self.CB_centrala_loglevel_table.ItemIndex := Integer(TrakceI.logLevelTable);

  if (TrakceI.Lib = '') then
    Self.SB1.Panels.Items[_SB_TRAKCE_LIB].Text := '-'
  else
    Self.SB1.Panels.Items[_SB_TRAKCE_LIB].Text := ExtractFileName(TrakceI.Lib);

  if (RCSi.Lib = '') then
    Self.SB1.Panels.Items[_SB_RCS_LIB].Text := '-'
  else
    Self.SB1.Panels.Items[_SB_RCS_LIB].Text := ExtractFileName(RCSi.Lib);

  try
    if ((RCSi.ready) and (diag.simInputs) and (RCSi.Simulation)) then
      RCSi.InputSim();
  except
    on E: Exception do
      Log('Nelze provést inputSim : ' + E.Message, llInfo, lsRCS);
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

procedure TF_Main.PM_SaveFormPosClick(Sender: TObject);
begin
  try
    FormData.SaveFormData(FormData.aFile);
  except
    on E: Exception do
      Application.MessageBox(PChar('Výjimka:'+#13#10+E.Message), 'Chyba při ukládání dat', MB_OK OR MB_ICONWARNING);
  end;
end;

procedure TF_Main.FormPaint(Sender: TObject);
begin
  mCpuLoad.ResizeCPUGauge();
end;

procedure TF_Main.PM_ClientsPopup(Sender: TObject);
begin
  for var item in Self.PM_Clients.Items do
    item.Enabled := (Self.LV_Clients.Selected <> nil) and (PanelServer.GetClient(Self.LV_Clients.ItemIndex) <> nil);
end;

procedure TF_Main.PM_ConsoleClick(Sender: TObject);
begin
  F_Console.Show;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_Main.LV_JCChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  B_VC_delete.Enabled := (LV_JC.Selected <> nil);

  if (LV_JC.Selected <> nil) then
    B_JC_Reset.Enabled := JCDb.GetJCByIndex(LV_JC.ItemIndex).activating
  else
    B_JC_Reset.Enabled := false;
end;

procedure TF_Main.LV_JCCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  var DefaultDraw: Boolean);
begin
  if (Item.SubItems.Count >= 4) then
  begin
    if (Item.SubItems[3] <> '0') then
      Self.LV_JC.Canvas.Brush.Color := _TABLE_COLOR_YELLOW
    else if (Item.SubItems[1] = IntToStr(_JC_DESTROY_NC)) then
      Self.LV_JC.Canvas.Brush.Color := _TABLE_COLOR_BLUE
    else if (Item.SubItems[1] <> IntToStr(_JC_DESTROY_NONE)) then
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
  if (Key = #13) then
    Self.LV_JCDblClick(LV_Blocks);
end;

procedure TF_Main.LV_logCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  var DefaultDraw: Boolean);
begin
  LV_log.Canvas.Brush.Color := TColor(LV_log.Items[Item.Index].Data);
end;

procedure TF_Main.LV_logDblClick(Sender: TObject);
begin
  Self.LV_log.Clear();
end;

procedure TF_Main.LV_log_lnetCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  var DefaultDraw: Boolean);
begin
  if (Item.SubItems.Count < 2) then
    Exit();

  case (StrToIntDef(Item.SubItems[0], 0)) of
    1:
      (Sender as TCustomListView).Canvas.Brush.Color := _TABLE_COLOR_RED;
    2:
      (Sender as TCustomListView).Canvas.Brush.Color := _TABLE_COLOR_YELLOW;
    4, 5:
      begin
        (Sender as TCustomListView).Canvas.Brush.Color := _TABLE_COLOR_GRAY;
        if (LeftStr(Item.SubItems[1], 3) = 'GET') then
          (Sender as TCustomListView).Canvas.Brush.Color := _TABLE_COLOR_BLUE;
        if (LeftStr(Item.SubItems[1], 3) = 'PUT') then
          (Sender as TCustomListView).Canvas.Brush.Color := _TABLE_COLOR_GREEN;
      end; // case 2
    3, 6:
      (Sender as TCustomListView).Canvas.Brush.Color := _TABLE_COLOR_WHITE;
end; // case
end;

procedure TF_Main.LV_log_lnetDblClick(Sender: TObject);
begin
  Self.LV_log_lnet.Clear();
end;

procedure TF_Main.LV_MultiJCChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  Self.B_mJC_Remove.Enabled := (Self.LV_MultiJC.Selected <> nil);

  if (Self.LV_MultiJC.SelCount > 1) then
    Self.B_mJC_Remove.Caption := 'Smazat cesty'
  else
    Self.B_mJC_Remove.Caption := 'Smazat cestu';
end;

procedure TF_Main.LV_MultiJCCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  var DefaultDraw: Boolean);
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
  if (Key = #13) then
    Self.LV_MultiJCDblClick(LV_Blocks);
end;

procedure TF_Main.LV_SoupravyChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  Self.B_lok_delete.Enabled := (Self.SoupravySelectedCount() > 0);

  if (Self.SoupravySelectedCount() > 1) then
    Self.B_lok_delete.Caption := 'Smazat soupravy'
  else
    Self.B_lok_delete.Caption := 'Smazat soupravu';
end;

procedure TF_Main.LV_SoupravyCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if ((Item.Index >= Trains.count) or (Trains[Item.Index] = nil)) then
    Exit();

  if (Trains[Item.Index].emergencyStopped) then
    (Sender as TCustomListView).Canvas.Brush.Color := _TABLE_COLOR_RED
  else if (Trains[Item.Index].IsSpeedOverride) then
    (Sender as TCustomListView).Canvas.Brush.Color := _TABLE_COLOR_YELLOW;
end;

procedure TF_Main.LV_StaniceChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  if (Self.LV_Stanice.Selected <> nil) then
  begin
    var area := Areas[Self.LV_Stanice.ItemIndex];
    Self.B_RemoveStack.Enabled := (area.stack.Count > 0);
  end else begin
    Self.B_RemoveStack.Enabled := false;
  end;
end;

procedure TF_Main.LV_Stav_RCSCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  var DefaultDraw: Boolean);
begin
  if ((Item.SubItems.Count > 5) and ((Item.SubItems[5] = 'Fail') or (Item.SubItems[5] = 'Error'))) then
    Self.LV_Stav_RCS.Canvas.Brush.Color := _TABLE_COLOR_RED
  else if ((Item.SubItems.Count > 5) and (Item.SubItems[5] = 'Warning')) then
    Self.LV_Stav_RCS.Canvas.Brush.Color := _TABLE_COLOR_YELLOW
  else
    Self.LV_Stav_RCS.Canvas.Brush.Color := _TABLE_COLOR_WHITE;
end;

procedure TF_Main.LV_UsersChange(Sender: TObject; Item: TListItem; Change: TItemChange);
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
  if (Key = #13) then
    Self.LV_UsersDblClick(LV_Blocks);
end;

procedure TF_Main.LV_ZesilovaceChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  B_zes_delete.Enabled := LV_Zesilovace.Selected <> nil;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_Main.LogStatus(str: string);
begin
  if (Assigned(Self.LB_Log)) then
  begin
    if (Self.LB_Log.Items.Count > 100) then
      Self.LB_Log.Clear();
    Self.LB_Log.Items.Insert(0, FormatDateTime('hh:nn:ss', Now) + ' : ' + str);
  end;
  Log(str, llInfo, lsSystem);
end;

procedure TF_Main.LV_ABChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  Self.B_AB_Delete.Enabled := (Self.LV_AB.Selected <> nil);
end;

procedure TF_Main.LV_BlocksChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  B_BlkDelete.Enabled := (LV_Blocks.Selected <> nil);
end;

procedure TF_Main.LV_BlocksCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  var DefaultDraw: Boolean);
var blk: TBlk;
begin
  blk := Blocks.GetBlkByIndex(Item.Index);
  if (blk = nil) then
    Exit();

  case (blk.typ) of
    btTurnout:
      begin
        case ((blk as TBlkTurnout).Position) of
          TTurnoutPosition.disabled:
            LV_Blocks.Canvas.Brush.Color := _TABLE_COLOR_GRAY;
          TTurnoutPosition.none:
            LV_Blocks.Canvas.Brush.Color := _TABLE_COLOR_YELLOW;
          TTurnoutPosition.plus:
            LV_Blocks.Canvas.Brush.Color := _TABLE_COLOR_GREEN;
          TTurnoutPosition.minus:
            LV_Blocks.Canvas.Brush.Color := _TABLE_COLOR_GREEN;
          TTurnoutPosition.both:
            LV_Blocks.Canvas.Brush.Color := _TABLE_COLOR_RED;
        end;
      end;

    /// ///////////////////
    btTrack, btRT:
      begin
        case ((blk as TBlkTrack).occupied) of
          TTrackState.disabled:
            LV_Blocks.Canvas.Brush.Color := _TABLE_COLOR_GRAY;
          TTrackState.none:
            LV_Blocks.Canvas.Brush.Color := _TABLE_COLOR_YELLOW;
          TTrackState.Free:
            LV_Blocks.Canvas.Brush.Color := _TABLE_COLOR_GREEN;
          TTrackState.occupied:
            LV_Blocks.Canvas.Brush.Color := _TABLE_COLOR_PINKY;
        end;
      end;

    /// ///////////////////
    btIR:
      begin
        case ((blk as TBlkIR).occupied) of
          TIROccupationState.disabled:
            LV_Blocks.Canvas.Brush.Color := _TABLE_COLOR_GRAY;
          TIROccupationState.none:
            LV_Blocks.Canvas.Brush.Color := _TABLE_COLOR_YELLOW;
          TIROccupationState.Free:
            LV_Blocks.Canvas.Brush.Color := _TABLE_COLOR_GREEN;
          TIROccupationState.occupied:
            LV_Blocks.Canvas.Brush.Color := _TABLE_COLOR_PINKY;
        end;
      end;

    /// ///////////////////
    btSignal, btGroupSignal:
      begin
        if ((blk as TBlkSignal).signal < ncStuj) then
          LV_Blocks.Canvas.Brush.Color := _TABLE_COLOR_GRAY // disabled
        else if ((blk as TBlkSignal).signal = ncStuj) then
          LV_Blocks.Canvas.Brush.Color := _TABLE_COLOR_GREEN
        else
          LV_Blocks.Canvas.Brush.Color := _TABLE_COLOR_PINKY;
      end;

    /// ///////////////////
    btCrossing:
      begin
        case ((blk as TBlkCrossing).state) of
          TBlkCrossingBasicState.disabled:
            LV_Blocks.Canvas.Brush.Color := _TABLE_COLOR_GRAY;
          TBlkCrossingBasicState.unknown:
            LV_Blocks.Canvas.Brush.Color := _TABLE_COLOR_YELLOW;
          TBlkCrossingBasicState.error:
            LV_Blocks.Canvas.Brush.Color := _TABLE_COLOR_RED;
          TBlkCrossingBasicState.open:
            LV_Blocks.Canvas.Brush.Color := _TABLE_COLOR_GREEN;
          TBlkCrossingBasicState.caution, TBlkCrossingBasicState.closed:
            LV_Blocks.Canvas.Brush.Color := _TABLE_COLOR_PINKY;
        else
          LV_Blocks.Canvas.Brush.Color := _TABLE_COLOR_WHITE;
        end;
      end;

    /// ///////////////////
    btLinker:
      begin
        if (not(blk as TBlkLinker).Enabled) then
          LV_Blocks.Canvas.Brush.Color := _TABLE_COLOR_GRAY
        else
          LV_Blocks.Canvas.Brush.Color := _TABLE_COLOR_WHITE;
      end;

    /// ///////////////////
    btRailway:
      begin
        if ((blk as TBlkRailway).State.direction = TRailwayDirection.disabled) then
          LV_Blocks.Canvas.Brush.Color := _TABLE_COLOR_GRAY
        else
          LV_Blocks.Canvas.Brush.Color := _TABLE_COLOR_WHITE;
      end;

    /// ///////////////////
    btLock:
      begin
        if ((blk as TBlkLock).State.Enabled) then
        begin
          if ((blk as TBlkLock).keyReleased) then
            LV_Blocks.Canvas.Brush.Color := _TABLE_COLOR_PINKY
          else
            LV_Blocks.Canvas.Brush.Color := _TABLE_COLOR_WHITE;
        end
        else
          LV_Blocks.Canvas.Brush.Color := _TABLE_COLOR_GRAY
      end;

    /// ///////////////////
    btDisconnector:
      begin
        case ((blk as TBlkDisconnector).State) of
          TBlkDiscBasicState.disabled:
            LV_Blocks.Canvas.Brush.Color := _TABLE_COLOR_GRAY;
          TBlkDiscBasicState.inactive:
            LV_Blocks.Canvas.Brush.Color := _TABLE_COLOR_GREEN;
          TBlkDiscBasicState.active, TBlkDiscBasicState.activeInfinite:
            LV_Blocks.Canvas.Brush.Color := _TABLE_COLOR_PINKY;
        end;
      end;

    /// ///////////////////
    btIO:
      begin
        if (TBlkIO(blk).Enabled) then
        begin
          if ((TBlkIO(blk).activeOutput) or (TBlkIO(blk).activeInput)) then
            LV_Blocks.Canvas.Brush.Color := _TABLE_COLOR_YELLOW
          else if (TBlkAC(blk).clientConnected) then
            LV_Blocks.Canvas.Brush.Color := _TABLE_COLOR_WHITE
        end
        else
          LV_Blocks.Canvas.Brush.Color := _TABLE_COLOR_GRAY;
      end;

    /// ///////////////////
    btSummary:
      begin
        case ((blk as TBlkSummary).Enabled) of
          false:
            LV_Blocks.Canvas.Brush.Color := _TABLE_COLOR_GRAY;
          true:
            LV_Blocks.Canvas.Brush.Color := _TABLE_COLOR_GREEN;
        end;
      end;

    /// ///////////////////
    btAC:
      begin
        if (TBlkAC(blk).Enabled) then
        begin
          if (not TBlkAC(blk).stopped) then
            LV_Blocks.Canvas.Brush.Color := _TABLE_COLOR_YELLOW
          else if (TBlkAC(blk).clientConnected) then
            LV_Blocks.Canvas.Brush.Color := _TABLE_COLOR_GREEN
          else
            LV_Blocks.Canvas.Brush.Color := _TABLE_COLOR_WHITE;
        end
        else
          LV_Blocks.Canvas.Brush.Color := _TABLE_COLOR_GRAY;
      end;

    /// ///////////////////
    btPst:
      begin
        case (TBlkPst(blk).status) of
          pstDisabled: LV_Blocks.Canvas.Brush.Color := _TABLE_COLOR_GRAY;
          pstOff: LV_Blocks.Canvas.Brush.Color := _TABLE_COLOR_GREEN;
          pstTakeReady: LV_Blocks.Canvas.Brush.Color := _TABLE_COLOR_YELLOW;
          pstRefuging: LV_Blocks.Canvas.Brush.Color := _TABLE_COLOR_RED;
          pstActive: LV_Blocks.Canvas.Brush.Color := _TABLE_COLOR_BLUE;
        end;
      end;
  end; // case
end;

procedure TF_Main.LV_BlocksDblClick(Sender: TObject);
var blk: TBlk;
begin
  if (LV_Blocks.Selected = nil) then
    Exit();
  blk := Blocks.GetBlkByIndex(Self.LV_Blocks.ItemIndex);
  if (blk = nil) then
    Exit();

  try
    case (blk.typ) of
      btTurnout:
        F_BlkTurnout.EditBlock(Self.LV_Blocks.ItemIndex);
      btTrack:
        F_BlkTrack.EditBlock(Self.LV_Blocks.ItemIndex);
      btIR:
        F_BlkIR.EditBlock(Self.LV_Blocks.ItemIndex);
      btSignal:
        F_BlkSignal.EditBlock(Self.LV_Blocks.ItemIndex);
      btCrossing:
        F_BlkCrossing.EditBlock(Self.LV_Blocks.ItemIndex);
      btRailway, btLinker:
        F_BlkRailway.EditBlock(Self.LV_Blocks.ItemIndex);
      btLock:
        F_BlkLock.EditBlock(Self.LV_Blocks.ItemIndex);
      btDisconnector:
        F_BlkDisconnector.EditBlock(Self.LV_Blocks.ItemIndex);
      btRT:
        F_BlkRT.EditBlock(Self.LV_Blocks.ItemIndex);
      btIO:
        F_BlkIO.EditBlock(Self.LV_Blocks.ItemIndex);
      btSummary:
        F_BlkSummary.EditBlock(Self.LV_Blocks.ItemIndex);
      btAC:
        F_BlkAC.EditBlock(Self.LV_Blocks.ItemIndex);
      btGroupSignal:
        F_BlkGroupSignal.EditBlock(Self.LV_Blocks.ItemIndex);
      btPst:
        F_BlkPst.EditBlock(Self.LV_Blocks.ItemIndex);
    end;
  except
    on E:Exception do
      Application.MessageBox(PChar(E.Message), 'Chyba', MB_OK OR MB_ICONERROR);
  end;
end;

procedure TF_Main.LV_BlocksKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) then
    Self.LV_BlocksDblClick(LV_Blocks);
end;

procedure TF_Main.LV_ClientsCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  var DefaultDraw: Boolean);
begin
  if ((Item.SubItems[_LV_CLIENTS_COL_STATE] = 'uzavřeno') or (Item.SubItems[_LV_CLIENTS_COL_STATE] = 'odpojen')) then
    Self.LV_Clients.Canvas.Brush.Color := _TABLE_COLOR_WHITE
  else if ((Item.SubItems[_LV_CLIENTS_COL_STATE] = 'otevírání') or (Item.SubItems[_LV_CLIENTS_COL_STATE] = 'handshake'))
  then
    Self.LV_Clients.Canvas.Brush.Color := _TABLE_COLOR_GRAY
  else if (Item.SubItems[_LV_CLIENTS_COL_STATE] = 'otevřeno') then
  begin
    if (Item.SubItems[_LV_CLIENTS_COL_PING] = 'unreachable') then
      Self.LV_Clients.Canvas.Brush.Color := _TABLE_COLOR_RED
    else if (Item.SubItems[_LV_CLIENTS_COL_PING] = '?') then
      Self.LV_Clients.Canvas.Brush.Color := _TABLE_COLOR_YELLOW
    else
      Self.LV_Clients.Canvas.Brush.Color := _TABLE_COLOR_GREEN;
  end
  else
    Self.LV_Clients.Canvas.Brush.Color := _TABLE_COLOR_YELLOW;
end;

procedure TF_Main.LV_HVChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  B_HV_Delete.Enabled := (LV_HV.Selected <> nil) and (not TrakceI.ConnectedSafe());
end;

procedure TF_Main.LV_HVCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  var DefaultDraw: Boolean);
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
  if (LV_HV.Selected = nil) then
    Exit();

  if (TrakceI.ConnectedSafe()) then
  begin
    try
      RegCollector.Open(HVDb[StrToInt(Self.LV_HV.Selected.Caption)]);
    except
      on E: Exception do
        Application.MessageBox(PChar(E.Message), 'Varování', MB_OK OR MB_ICONWARNING);
    end;
  end else begin
    F_HVEdit.OpenForm(HVDb[StrToInt(LV_HV.Selected.Caption)]);
  end;
end;

procedure TF_Main.LV_HVKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) then
    Self.LV_HVDblClick(LV_Blocks);
end;

procedure TF_Main.LV_ZesilovaceCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  var DefaultDraw: Boolean);
begin
  if ((not RCSi.NoExStarted()) or (not Boosters.sorted[Item.Index].rcsPresent)) then
  begin
    LV_Zesilovace.Canvas.Brush.Color := _TABLE_COLOR_GRAY;
  end else begin
    if (Boosters.sorted[Item.Index].power = TBoosterSignal.ok) then
    begin
      if (Boosters.sorted[Item.Index].overload = TBoosterSignal.ok) then
      begin
        LV_Zesilovace.Canvas.Brush.Color := _TABLE_COLOR_GREEN;
      end else begin
        LV_Zesilovace.Canvas.Brush.Color := _TABLE_COLOR_RED;
      end;
    end else begin
      LV_Zesilovace.Canvas.Brush.Color := _TABLE_COLOR_BLUE;
    end;
  end; // if not Zarizeni.Start
end;

procedure TF_Main.LV_ZesilovaceDblClick(Sender: TObject);
begin
  if (LV_Zesilovace.Selected <> nil) then
    F_Booster_Edit.EditBooster(Boosters.sorted[LV_Zesilovace.ItemIndex]);
end;

procedure TF_Main.LV_ZesilovaceKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) then
    Self.LV_ZesilovaceDblClick(LV_Blocks);
end;

/// /////////////////////////////////////////////////////////////////////////////

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

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_Main.UpdateCallMethod();
var ev: TNotifyEvent;
begin
  if (Assigned(Self.call_method)) then
  begin
    // toto poradi musi byt zachovano !
    // volani eventu totiz muze zpusobit Application.ProcessMessages
    ev := Self.call_method;
    Self.call_method := nil;
    ev(Self);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_Main.UpdateSystemButtons();
begin
  Self.A_System_Start.Enabled := ((not RCSi.NoExStarted) or (not TrakceI.ConnectedSafe()) or (Self.A_Locos_Acquire.Enabled)
    or (not PanelServer.openned) or (not Blocks.Enabled) or ((GlobalConfig.ptAutoStart) and (not PtServer.openned)));
  Self.A_System_Stop.Enabled := (RCSi.NoExOpened) or (TrakceI.ConnectedSafe()) or (PanelServer.openned) or
    (PtServer.openned);
end;

/// /////////////////////////////////////////////////////////////////////////////

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

/// /////////////////////////////////////////////////////////////////////////////

function TF_Main.SoupravySelectedCount(): Integer;
var LI: TListItem;
begin
  Result := 0;
  for LI in Self.LV_Soupravy.Items do
    if ((LI.Selected) and (LI.Caption <> '')) then
      Inc(Result);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TF_Main.LVSelectedTexts(LV: TListView; single: string; multiple: string): string;
var count: Integer;
begin
  Result := '';
  count := 0;
  for var LI: TListItem in LV.Items do
  begin
    if (LI.Selected) then
    begin
      Result := Result + LI.Caption + ', ';
      Inc(count);
    end;
  end;
  Result := LeftStr(Result, Length(Result) - 2);

  if (count = 1) then
    Result := single + ' ' + Result
  else
    Result := multiple + ' ' + Result;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_Main.FillGlobalConfig();
begin
  Self.ME_autosave_period.Text := FormatDateTime('nn:ss', GlobalConfig.autosave_period);
  Self.CHB_Autosave.Checked := GlobalConfig.autosave;
  Self.E_Scale.Text := IntToStr(GlobalConfig.scale);
  Self.CHB_autostart.Checked := GlobalConfig.autostart;
  Self.CHB_Log_console.Checked := GlobalConfig.consoleLog;

  case (Self.T_Main.Interval) of
    25: Self.CB_MainTimerInterval.ItemIndex := 0;
    50: Self.CB_MainTimerInterval.ItemIndex := 1;
    100: Self.CB_MainTimerInterval.ItemIndex := 2;
    200: Self.CB_MainTimerInterval.ItemIndex := 3;
    250: Self.CB_MainTimerInterval.ItemIndex := 4;
    500: Self.CB_MainTimerInterval.ItemIndex := 5;
  else
    Self.CB_MainTimerInterval.ItemIndex := -1;
  end;

  Self.SE_timeRC.Value := GlobalConfig.times.rcFree;
  Self.SE_timeRCVC.Value := GlobalConfig.times.rcVcOccupied;
  Self.SE_timeRCPC.Value := GlobalConfig.times.rcPcOccupied;
  Self.SE_timeNUZ.Value := GlobalConfig.times.nuz;

  Self.SE_jcMaxMovingTurnouts.Value := GlobalConfig.jcMaxMovingTurnouts;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_Main.B_ConfigApplyClick(Sender: TObject);
begin
  try
    GlobalConfig.scale := StrToInt(Self.E_Scale.Text);
  except
    on E: Exception do
    begin
      Application.MessageBox(PChar('Nepodařilo se načíst měřítko:' + #13#10 + E.Message), 'Chyba',
        MB_OK OR MB_ICONWARNING);
      Exit();
    end;
  end;

  GlobalConfig.autosave := Self.CHB_Autosave.Checked;
  GlobalConfig.autostart := Self.CHB_autostart.Checked;
  GlobalConfig.consoleLog := Self.CHB_Log_console.Checked;

  if (Self.CHB_Autosave.Checked) then
  begin
    try
      GlobalConfig.autosave_period := EncodeTime(0, StrToInt(LeftStr(Self.ME_autosave_period.Text, 2)),
        StrToInt(Copy(Self.ME_autosave_period.Text, 4, 2)), 0);
    except
      GlobalConfig.autosave := False;
      Application.MessageBox('Nepodařilo se načíst čas automatického uložení', 'Chyba', MB_OK OR MB_ICONERROR);
    end;
  end;

  case (Self.CB_MainTimerInterval.ItemIndex) of
    0: Self.T_Main.Interval := 25;
    1: Self.T_Main.Interval := 50;
    2: Self.T_Main.Interval := 100;
    3: Self.T_Main.Interval := 200;
    4: Self.T_Main.Interval := 250;
    5: Self.T_Main.Interval := 500;
  end;
  Log('Primární smyčka nastavena na ' + IntToStr(Self.T_Main.Interval) + ' ms', TLogLevel.llInfo);

  GlobalConfig.times.rcFree := Self.SE_timeRC.Value;
  GlobalConfig.times.rcVcOccupied := Self.SE_timeRCVC.Value;
  GlobalConfig.times.rcPcOccupied := Self.SE_timeRCPC.Value;
  GlobalConfig.times.nuz := Self.SE_timeNUZ.Value;

  GlobalConfig.jcMaxMovingTurnouts := Self.SE_jcMaxMovingTurnouts.Value;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_Main.LV_DigiRychDblClick(Sender: TObject);
begin
  if (Self.LV_DigiRych.Selected <> nil) then
    F_RychlostiEdit.OpenForm(Self.LV_DigiRych.ItemIndex);
end;

/// /////////////////////////////////////////////////////////////////////////////

initialization

SystemData := TSystem.Create();

finalization

FreeAndNil(SystemData);

end.// unit
