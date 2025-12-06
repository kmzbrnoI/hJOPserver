unit fMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Menus, ImgList, Buttons, ComCtrls, TrakceIFace,
  inifiles, ActnList, AppEvnts, cpuLoad, ExtDlgs, Gauges, StrUtils, RCSsc,
  ComObj, BoosterDb, System.Actions, System.ImageList, Logging,
  Vcl.Mask, Vcl.Samples.Spin, Generics.Collections, Vcl.NumberBox;

const
  _INIDATA_FN = 'inidata.ini';

  _TABLE_COLOR_GREEN = $E0FFE0;
  _TABLE_COLOR_GRAY = $DDDDDD;
  _TABLE_COLOR_RED = $E0E0FF;
  _TABLE_COLOR_YELLOW = $D0FFFF;
  _TABLE_COLOR_BLUE = $FFE0E0;
  _TABLE_COLOR_WHITE = $FFFFFF;
  _TABLE_COLOR_PINKY = $E2B6FF;

type

  TAutostartState = (asDisabled, asEnabled, asWaiting);

  TRCSMI = record
    MI_Root: TMenuItem;
    MI_Open: TMenuItem;
    MI_Close: TMenuItem;
    MI_Start: TMenuItem;
    MI_Stop: TMenuItem;
    MI_Settings: TMenuItem;
    MI_NoLib: TMenuItem;
    MI_Libs: TList<TMenuItem>;
  end;

  TF_Main = class(TForm)
    T_Main: TTimer;
    Menu_1: TMainMenu;
    MI_RCS: TMenuItem;
    MI_RCSs_Go: TMenuItem;
    MI_RCSs_Stop: TMenuItem;
    MI_Provoz: TMenuItem;
    PM_ResetV: TMenuItem;
    SB1: TStatusBar;
    N1: TMenuItem;
    PM_Tester: TMenuItem;
    M_Help: TMenuItem;
    PM_Help_RP: TMenuItem;
    MI_Trakce: TMenuItem;
    MI_DCC_on: TMenuItem;
    MI_DCC_Off: TMenuItem;
    MI_Trk_Disconnect: TMenuItem;
    N4: TMenuItem;
    MI_Trk_connect: TMenuItem;
    T_GUI_refresh: TTimer;
    MI_Application: TMenuItem;
    PM_Central_Start: TMenuItem;
    PM_Central_Stop: TMenuItem;
    N5: TMenuItem;
    P_Pozadi: TPanel;
    P_Date: TPanel;
    P_Time: TPanel;
    P_Time_modelovy: TPanel;
    P_Zrychleni: TPanel;
    IL_Menu: TImageList;
    MI_Loco_Acquire: TMenuItem;
    MI_Loco_Release: TMenuItem;
    P_DCC: TPanel;
    P_SystemSet: TPanel;
    SB_SystemStart: TSpeedButton;
    SB_SystemStop: TSpeedButton;
    PM_SaveFormPos: TMenuItem;
    IL_Bloky: TImageList;
    IL_RCS: TImageList;
    PM_Console: TMenuItem;
    AL_Main: TActionList;
    A_RCSs_Go: TAction;
    A_RCSs_Stop: TAction;
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
    A_RCSs_Open: TAction;
    A_RCSs_Close: TAction;
    MI_RCSs_Open: TMenuItem;
    MI_RCSs_Close: TMenuItem;
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
    TS_RV: TTabSheet;
    LV_Vehicles: TListView;
    P_RV_Pozadi: TPanel;
    P_RV_Left: TPanel;
    E_dataload_RV_dir: TEdit;
    TS_Trains: TTabSheet;
    LV_Trains: TListView;
    P_Trains_Bg: TPanel;
    P_Trains_Left: TPanel;
    E_dataload_soupr: TEdit;
    TS_Areas: TTabSheet;
    LV_Areas: TListView;
    P_Areas_Bg: TPanel;
    P_St_Left: TPanel;
    E_dataload_spnl: TEdit;
    TS_Zesilovace: TTabSheet;
    LV_Boosters: TListView;
    P_bst_bg: TPanel;
    P_Zes_Right: TPanel;
    L_Zes_Napajeni: TLabel;
    L_Zes_OK: TLabel;
    L_Zes_NapajeniL_Zes_Zkrat: TLabel;
    L_Zes_Nedetekovano: TLabel;
    P_Zes_Left: TPanel;
    E_dataload_zes: TEdit;
    TS_Users: TTabSheet;
    LV_Users: TListView;
    P_Users_Bg: TPanel;
    P_Users_Left: TPanel;
    E_dataload_users: TEdit;
    TS_RCS0: TTabSheet;
    LV_RCS0_State: TListView;
    TS_VC: TTabSheet;
    P_JC_Bg: TPanel;
    P_JC_Left: TPanel;
    E_Dataload_JC: TEdit;
    LV_JC: TListView;
    TS_log: TTabSheet;
    LV_log: TListView;
    GB_Connected_Panels: TGroupBox;
    LV_Clients: TListView;
    GB_stav_technologie: TGroupBox;
    S_RCS0_open: TShape;
    S_RCS0_started: TShape;
    S_Trakce_Connected: TShape;
    S_DCC: TShape;
    S_Server: TShape;
    L_RCS_open: TLabel;
    L_RCS_started: TLabel;
    L_StavS_3: TLabel;
    L_StavS_4: TLabel;
    L_StavS_6: TLabel;
    GB_Log: TGroupBox;
    LB_BriefLog: TListBox;
    MI_Save_config: TMenuItem;
    S_locos_acquired: TShape;
    Label1: TLabel;
    PM_RV: TPopupMenu;
    PM_Loco_Edit: TMenuItem;
    PM_Loco_Reg: TMenuItem;
    N6: TMenuItem;
    P_log: TPanel;
    PM_SaveLayout: TMenuItem;
    A_SaveStav: TAction;
    PM_Bloky: TPopupMenu;
    MI_Block_State: TMenuItem;
    MenuItem2: TMenuItem;
    MI_Block_Edit: TMenuItem;
    B_JC_Add: TButton;
    B_JC_delete: TButton;
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
    B_FuncChange: TButton;
    CHB_LoadChanges: TCheckBox;
    AE_Main: TApplicationEvents;
    SD_RV_Stats: TSaveDialog;
    S_PTServer: TShape;
    L_PTServer: TLabel;
    MI_PT: TMenuItem;
    MI_Start: TMenuItem;
    MI_Stop: TMenuItem;
    A_PT_Start: TAction;
    A_PT_Stop: TAction;
    MI_Block_Houk: TMenuItem;
    MI_RCSs_Update: TMenuItem;
    TS_AB: TTabSheet;
    Panel4: TPanel;
    LV_AB: TListView;
    P_AB_Left: TPanel;
    B_AB_Delete: TButton;
    B_BlkAdd: TButton;
    B_BlkDelete: TButton;
    B_RV_Add: TButton;
    B_RV_Delete: TButton;
    B_train_delete: TButton;
    B_RVStats_Export: TButton;
    B_RVStats_Clear: TButton;
    B_zes_add: TButton;
    B_zes_delete: TButton;
    B_User_Add: TButton;
    B_User_Delete: TButton;
    E_dataload_RV_state: TEdit;
    E_dataload_users_stat: TEdit;
    P_dataload_rcs: TPanel;
    CHB_RCS0_Show_Only_Active: TCheckBox;
    N11: TMenuItem;
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
    B_FuncUpdate: TButton;
    NB_TimeJCZav: TNumberBox;
    Label15: TLabel;
    MI_Block_Delete: TMenuItem;
    PM_Loco_Delete: TMenuItem;
    MI_Loco_Tacho_Reset: TMenuItem;
    S_RCS1_open: TShape;
    S_RCS1_started: TShape;
    S_RCS2_open: TShape;
    S_RCS2_started: TShape;
    S_RCS3_open: TShape;
    S_RCS3_started: TShape;
    TS_RCS1: TTabSheet;
    TS_RCS2: TTabSheet;
    TS_RCS3: TTabSheet;
    LV_RCS1_State: TListView;
    Panel1: TPanel;
    CHB_RCS1_Show_Only_Active: TCheckBox;
    Panel2: TPanel;
    CHB_RCS2_Show_Only_Active: TCheckBox;
    Panel5: TPanel;
    CHB_RCS3_Show_Only_Active: TCheckBox;
    LV_RCS2_State: TListView;
    LV_RCS3_State: TListView;
    CHB_RCS0_Log: TCheckBox;
    CHB_RCS1_Log: TCheckBox;
    CHB_RCS2_Log: TCheckBox;
    CHB_RCS3_Log: TCheckBox;
    MI_SaveVehicles: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    MI_ExitApp: TMenuItem;
    L_TrkState: TLabel;
    procedure T_MainTimer(Sender: TObject);
    procedure PM_ResetVClick(Sender: TObject);
    procedure MI_Trk_libClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure AE_1Message(var Msg: tagMSG; var Handled: Boolean);
    procedure PM_TesterClick(Sender: TObject);
    procedure PM_Help_RPClick(Sender: TObject);
    procedure T_GUI_refreshTimer(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure L_DateDblClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PM_SaveFormPosClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure PM_ConsoleClick(Sender: TObject);
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
    procedure LV_ClientsCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
      var DefaultDraw: Boolean);
    procedure PC_1Change(Sender: TObject);
    procedure LV_BoostersDblClick(Sender: TObject);
    procedure B_zes_addClick(Sender: TObject);
    procedure B_zes_deleteClick(Sender: TObject);
    procedure LV_BoostersCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
      var DefaultDraw: Boolean);
    procedure LV_VehiclesChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure LV_VehiclesDblClick(Sender: TObject);
    procedure B_RV_AddClick(Sender: TObject);
    procedure B_RV_DeleteClick(Sender: TObject);
    procedure LV_BlocksCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
      var DefaultDraw: Boolean);
    procedure LV_BlocksDblClick(Sender: TObject);
    procedure B_BlkAddClick(Sender: TObject);
    procedure LV_BlocksChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure B_BlkDeleteClick(Sender: TObject);
    procedure B_JC_AddClick(Sender: TObject);
    procedure LV_JCDblClick(Sender: TObject);
    procedure B_JC_deleteClick(Sender: TObject);
    procedure LV_logCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
      var DefaultDraw: Boolean);
    procedure LV_log_lnetCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
      var DefaultDraw: Boolean);
    procedure LV_BoostersChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure LV_JCChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure MI_Save_configClick(Sender: TObject);
    procedure LB_BriefLogDblClick(Sender: TObject);
    procedure LV_TrainsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure B_train_deleteClick(Sender: TObject);
    procedure LV_VehiclesCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
      var DefaultDraw: Boolean);
    procedure PM_Loco_EditClick(Sender: TObject);
    procedure PM_Loco_RegClick(Sender: TObject);
    procedure PM_RVPopup(Sender: TObject);
    procedure LV_JCCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
      var DefaultDraw: Boolean);
    procedure LB_BriefLogDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure B_User_AddClick(Sender: TObject);
    procedure LV_UsersChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure LV_UsersDblClick(Sender: TObject);
    procedure B_User_DeleteClick(Sender: TObject);
    procedure A_SaveStavExecute(Sender: TObject);
    procedure PM_BlokyPopup(Sender: TObject);
    procedure MI_Block_EditClick(Sender: TObject);
    procedure MI_Block_StateClick(Sender: TObject);
    procedure B_JC_ResetClick(Sender: TObject);
    procedure P_Time_modelovyDblClick(Sender: TObject);
    procedure P_ZrychleniDblClick(Sender: TObject);
    procedure LV_AreasChange(Sender: TObject; Item: TListItem; Change: TItemChange);
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
    procedure B_FuncChangeClick(Sender: TObject);
    procedure LV_BlocksKeyPress(Sender: TObject; var Key: Char);
    procedure LV_JCKeyPress(Sender: TObject; var Key: Char);
    procedure LV_MultiJCKeyPress(Sender: TObject; var Key: Char);
    procedure LV_UsersKeyPress(Sender: TObject; var Key: Char);
    procedure LV_BoostersKeyPress(Sender: TObject; var Key: Char);
    procedure LV_VehiclesKeyPress(Sender: TObject; var Key: Char);
    procedure B_ClearStatsClick(Sender: TObject);
    procedure B_RVStats_ExportClick(Sender: TObject);
    procedure A_PT_StartExecute(Sender: TObject);
    procedure A_PT_StopExecute(Sender: TObject);
    procedure LV_RCS0_StateCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
      var DefaultDraw: Boolean);
    procedure MI_Block_HoukClick(Sender: TObject);
    procedure MI_RCSs_UpdateClick(Sender: TObject);
    procedure B_AB_DeleteClick(Sender: TObject);
    procedure LV_ABChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure CHB_RCS0_Show_Only_ActiveClick(Sender: TObject);
    procedure A_Trk_Lib_CfgExecute(Sender: TObject);
    procedure MI_Trk_UpdateClick(Sender: TObject);
    procedure A_Turnoff_FunctionsExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CHB_log_authClick(Sender: TObject);
    procedure LV_logDblClick(Sender: TObject);
    procedure LV_TrainsCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure MI_SimulationDiagnosticsClick(Sender: TObject);
    procedure B_ConfigApplyClick(Sender: TObject);
    procedure LV_DigiRychDblClick(Sender: TObject);
    procedure B_FuncUpdateClick(Sender: TObject);
    procedure LV_BlocksKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure LV_VehiclesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure LV_TrainsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure LV_BoostersKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure LV_UsersKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure LV_JCKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure LV_MultiJCKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure LV_ABKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MI_Block_DeleteClick(Sender: TObject);
    procedure PM_Loco_DeleteClick(Sender: TObject);
    procedure MI_Loco_Tacho_ResetClick(Sender: TObject);
    procedure A_RCSs_OpenExecute(Sender: TObject);
    procedure A_RCSs_CloseExecute(Sender: TObject);
    procedure A_RCSs_GoExecute(Sender: TObject);
    procedure A_RCSs_StopExecute(Sender: TObject);
    procedure CHB_RCS0_LogClick(Sender: TObject);
    procedure MI_SaveVehiclesClick(Sender: TObject);
    procedure MI_ExitAppClick(Sender: TObject);

  public const
    _SB_LOG = 0;
    _SB_TRAKCE_STAV = 1;
    _SB_TRAKCE_LIB = 2;
    _SB_PROC = 3;

    _SB_LOG_SHOW_TIME_MS = 3000;

    _LV_CLIENTS_COL_STATE = 0;
    _LV_CLIENTS_COL_CLIENT = 1;
    _LV_CLIENTS_COL_PROTOCOL = 2;
    _LV_CLIENTS_COL_APP = 3;
    _LV_CLIENTS_COL_PING = 4;
    _LV_CLIENTS_COL_OR1 = 5;
    _LV_CLIENTS_COL_OR2 = 6;
    _LV_CLIENTS_COL_OR3 = 7;
    _LV_CLIENTS_COL_OR_NEXT = 8;
    _LV_CLIENTS_COL_REGULATOR = 9;
    _LV_CLIENTS_COL_SH = 10;
    _LV_CLIENTS_COL_DCC = 11;

  private
    call_method: TNotifyEvent;
    mCpuLoad: TCpuLoad;
    MI_RCSs: array [0..TRCSs._RCSS_MAX] of TRCSMI;
    S_RCS_Open: array [0..TRCSs._RCSS_MAX] of TShape;
    S_RCS_Started: array [0..TRCSs._RCSS_MAX] of TShape;
    LV_RCSs_State: array [0..TRCSs._RCSS_MAX] of TListView;
    mRCSGUIInitialized: Boolean;
    mSb1Log: Boolean;
    mSb1LogHideTime: TDateTime;

    procedure UpdateCallMethod();
    procedure OnFuncNameChange(Sender: TObject);
    procedure UpdateFuncMemo();

    procedure RCSInit();
    procedure RCSGUIUpdateAll();

    procedure CreateMIRCSs();
    procedure RCSMIUpdateRoot();
    procedure RCSMIUpdateI(i: Integer);
    procedure MI_RCS_Settings_Click(Sender: TObject);
    procedure MI_RCS_Lib_Click(Sender: TObject);
    procedure MI_RCS_UnloadLib_Click(Sender: TObject);
    procedure MI_RCS_Open_Click(Sender: TObject);
    procedure MI_RCS_Close_Click(Sender: TObject);
    procedure MI_RCS_Start_Click(Sender: TObject);
    procedure MI_RCS_Stop_Click(Sender: TObject);

    procedure RCSShapesInit();
    procedure RCSShapesUpdate();
    procedure RCSSLVStateInit();
    procedure RCSStopSafeReaction();

    procedure WMPowerBroadcast(var Msg: TMessage); message WM_POWERBROADCAST;
    procedure WMQueryEndSession(var Msg: TWMQueryEndSession); message WM_QUERYENDSESSION;
    procedure WMEndSession(var Msg: TWMEndSession); message WM_ENDSESSION;

    procedure SB1LogUpdate();

  public
    CHB_RCSs_Show_Only_Active: array [0..TRCSs._RCSS_MAX] of TCheckBox;
    CHB_RCSs_Log: array [0..TRCSs._RCSS_MAX] of TCheckBox;

    autostart: record
      goTime: TDateTime;
      state: TAutostartState;
    end;

    CloseMessage: Boolean; // jestli se ptat uzivatele na ukonceni SW
    NUZClose: Boolean; // flag hard ukonceni SW bez kontroly pripojeni k systemum a zobrazeni dialogu

    procedure CloseForm();
    procedure RepaintObjects();
    procedure LoadIniLibData();
    procedure AutostartUpdate();
    procedure OnStart();
    procedure ShowDateTime();
    procedure LogBrief(str: string; level: TLogLevel);
    procedure DisableRemoveButtons();
    procedure SetCallMethod(Method: TNotifyEvent);
    procedure UpdateSystemButtons();
    procedure CheckNasobicWidth();
    function TrainsSelectedCount(): Integer;
    function LVSelectedTexts(LV: TListView; single: string; multiple: string): string;
    procedure PanelServerStartingStarted();
    procedure SB1Log(msg: string);

    // RCS
    procedure OnRCSBeforeStart(Sender: TObject);
    procedure OnRCSStart(Sender: TObject);
    procedure OnRCSScanned(Sender: TObject);
    procedure OnRCSBeforeStop(Sender: TObject);
    procedure OnRCSStop(Sender: TObject);
    procedure OnRCSBeforeOpen(Sender: TObject);
    procedure OnRCSOpen(Sender: TObject);
    procedure OnRCSBeforeClose(Sender: TObject);
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
    procedure OnTrkEmergencyChanged(Sender: TObject);

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

uses fTester, fModelTimeSet, fSplash, fHoukEvsUsek, DataJC, ownConvert,
  fAbout, version, fSystemInfo, fBlkTrack, fBlkTurnout, fAdminForm, Simulation,
  fRegulator, fBlkSummary, fSystemAutoStart, fBlkTrackState, GetSystems,
  RCSc, TechnologieJC, Config, fConsole, AreaDb, BlockDb, ownGuiUtils,
  Block, BlockTrack, BlockTurnout, BlockSignal, BlockIR, Area, RCSIFace,
  BlockSummary, BlockCrossing, TJCDatabase, TrakceC, IfThenElse,
  TCPServerPanel, DataBloky, DataRV, DataRCS, DataORs, DataZesilovac,
  fBlkNew, fVehicleEdit, fJCEdit, fZesilovacEdit, TRVDatabase, fBlkIR, fBlkCrossing,
  fBlkSignal, fBlkRailway, BlockLinker, TrainDb, DataTrains, DataUsers, fUserEdit, UserDb,
  fBlkTurnoutState, fBlkRailwayState, BlockRailway, TimeModel, fBlkLock,
  BlockLock, DataMultiJC, TMultiJCDatabase, fMJCEdit, BlockDisconnector,
  fBlkDisconnector, fFuncsSet, FunkceVyznam, fBlkRT, RCSdebugger, Booster, DataAB,
  AppEv, fBlkIO, BlockIO, TCPServerPT, RCSErrors, TechnologieAB, fBlkCrossingState,
  Diagnostics, BlockAC, fBlkAC, fBlkGroupSignal, fBlkPst, BlockPst, fBlkSignalState,
  fRychlostiEdit, TRailVehicle;

{$R *.dfm}

/// /////////////////////////////////////////////////////////////////////////////
// RCS BEGIN
/// /////////////////////////////////////////////////////////////////////////////

procedure TF_Main.RCSInit();
begin
  Self.RCSShapesInit();
  Self.mRCSGUIInitialized := True;

  Self.CreateMIRCSs();
  Self.UpdateRCSLibsList();
  Self.RCSGUIUpdateAll();

  try
    if (diag.simInputs) then
      Simulation.InputSim();
  except
    on E: Exception do
      logging.Log('Nelze provést inputSim : ' + E.Message, llError, lsRCS, True);
  end;
end;

procedure TF_Main.CreateMIRCSs();
begin
  for var i: Integer := 0 to RCSs._RCSS_MAX do
  begin
    Self.MI_RCSs[i].MI_Root := TMenuItem.Create(Self.MI_RCS);
    Self.MI_RCSs[i].MI_Root.Caption := 'RCS '+IntToStr(i);
    Self.MI_RCS.Insert(6+i, Self.MI_RCSs[i].MI_Root);

    Self.MI_RCSs[i].MI_Open := TMenuItem.Create(Self.MI_RCSs[i].MI_Root);
    Self.MI_RCSs[i].MI_Open.Caption := 'Otevřít zařízení';
    Self.MI_RCSs[i].MI_Open.ImageIndex := 2;
    Self.MI_RCSs[i].MI_Open.Tag := i;
    Self.MI_RCSs[i].MI_Open.OnClick := Self.MI_RCS_Open_Click;
    Self.MI_RCSs[i].MI_Root.Add(Self.MI_RCSs[i].MI_Open);

    Self.MI_RCSs[i].MI_Close := TMenuItem.Create(Self.MI_RCSs[i].MI_Root);
    Self.MI_RCSs[i].MI_Close.Caption := 'Zavřít zařízení';
    Self.MI_RCSs[i].MI_Close.ImageIndex := 3;
    Self.MI_RCSs[i].MI_Close.Tag := i;
    Self.MI_RCSs[i].MI_Close.OnClick := Self.MI_RCS_Close_Click;
    Self.MI_RCSs[i].MI_Root.Add(Self.MI_RCSs[i].MI_Close);

    Self.MI_RCSs[i].MI_Root.Add(ownGuiUtils.MenuItemSeparator(Self.MI_RCSs[i].MI_Root));

    Self.MI_RCSs[i].MI_Start := TMenuItem.Create(Self.MI_RCSs[i].MI_Root);
    Self.MI_RCSs[i].MI_Start.Caption := 'Spustit komunikaci';
    Self.MI_RCSs[i].MI_Start.ImageIndex := 4;
    Self.MI_RCSs[i].MI_Start.Tag := i;
    Self.MI_RCSs[i].MI_Start.OnClick := Self.MI_RCS_Start_Click;
    Self.MI_RCSs[i].MI_Root.Add(Self.MI_RCSs[i].MI_Start);

    Self.MI_RCSs[i].MI_Stop := TMenuItem.Create(Self.MI_RCSs[i].MI_Root);
    Self.MI_RCSs[i].MI_Stop.Caption := 'Zastavit komunikaci';
    Self.MI_RCSs[i].MI_Stop.ImageIndex := 5;
    Self.MI_RCSs[i].MI_Stop.Tag := i;
    Self.MI_RCSs[i].MI_Stop.OnClick := Self.MI_RCS_Stop_Click;
    Self.MI_RCSs[i].MI_Root.Add(Self.MI_RCSs[i].MI_Stop);

    Self.MI_RCSs[i].MI_Root.Add(ownGuiUtils.MenuItemSeparator(Self.MI_RCSs[i].MI_Root));

    Self.MI_RCSs[i].MI_Settings := TMenuItem.Create(Self.MI_RCSs[i].MI_Root);
    Self.MI_RCSs[i].MI_Settings.Caption := 'Nastavení';
    Self.MI_RCSs[i].MI_Settings.ImageIndex := 18;
    Self.MI_RCSs[i].MI_Settings.Tag := i;
    Self.MI_RCSs[i].MI_Settings.OnClick := Self.MI_RCS_Settings_Click;
    if (i = 0) then
      Self.MI_RCSs[i].MI_Settings.ShortCut := ShortCut(Word('D'), [ssCtrl]);
    Self.MI_RCSs[i].MI_Root.Add(Self.MI_RCSs[i].MI_Settings);

    Self.MI_RCSs[i].MI_Root.Add(ownGuiUtils.MenuItemSeparator(Self.MI_RCSs[i].MI_Root));

    Self.MI_RCSs[i].MI_NoLib := TMenuItem.Create(Self.MI_RCSs[i].MI_Root);
    Self.MI_RCSs[i].MI_NoLib.Caption := 'Žádná knihovna';
    Self.MI_RCSs[i].MI_NoLib.RadioItem := True;
    Self.MI_RCSs[i].MI_NoLib.OnClick := Self.MI_RCS_UnloadLib_Click;
    Self.MI_RCSs[i].MI_NoLib.Tag := i;
    Self.MI_RCSs[i].MI_Root.Add(Self.MI_RCSs[i].MI_NoLib);

    Self.MI_RCSs[i].MI_Libs := TList<TMenuItem>.Create();
  end;
end;

procedure TF_Main.RCSGUIUpdateAll();
begin
  if (not Self.mRCSGUIInitialized) then
    Exit();

  for var i: Integer := 0 to RCSs._RCSS_MAX do
    Self.RCSMIUpdateI(i);
  Self.RCSMIUpdateRoot();
  Self.RCSShapesUpdate();

  Self.PM_Tester.Enabled := (RCSs.AnyRCSState(rsStartedNotScanned) or RCSs.AnyRCSState(rsStartedScanned));
  Self.PM_ResetV.Enabled := Self.PM_Tester.Enabled;
  Self.TS_RCS0.TabVisible := (RCSs[0].lib <> '');
  Self.TS_RCS1.TabVisible := (RCSs[1].lib <> '');
  Self.TS_RCS2.TabVisible := (RCSs[2].lib <> '');
  Self.TS_RCS3.TabVisible := (RCSs[3].lib <> '');
end;

procedure TF_Main.RCSMIUpdateRoot();
var anyOpenEnabled: Boolean;
    anyCloseEnabled: Boolean;
    anyStartEnabled: Boolean;
    anyStopEnabled: Boolean;
begin
  anyOpenEnabled := False;
  anyCloseEnabled := False;
  anyStartEnabled := False;
  anyStopEnabled := False;

  for var i: Integer := 0 to RCSs._RCSS_MAX do
  begin
    if (Self.MI_RCSs[i].MI_Open.Enabled) then
      anyOpenEnabled := True;
    if (Self.MI_RCSs[i].MI_Close.Enabled) then
      anyCloseEnabled := True;
    if (Self.MI_RCSs[i].MI_Start.Enabled) then
      anyStartEnabled := True;
    if (Self.MI_RCSs[i].MI_Stop.Enabled) then
      anyStopEnabled := True;
  end;

  Self.A_RCSs_Open.Enabled := anyOpenEnabled;
  Self.A_RCSs_Close.Enabled := anyCloseEnabled;
  Self.A_RCSs_Go.Enabled := ((anyStartEnabled) and (not anyOpenEnabled));
  Self.A_RCSs_Stop.Enabled := anyStopEnabled;
end;

procedure TF_Main.RCSMIUpdateI(i: Integer);
begin
  Self.MI_RCSs[i].MI_Open.Enabled := ((RCSs[i].state = TRCSState.rsClosed) and (RCSs[i].ready));
  Self.MI_RCSs[i].MI_Close.Enabled := (RCSs[i].state = TRCSState.rsOpenStopped);
  Self.MI_RCSs[i].MI_Start.Enabled := (RCSs[i].state = TRCSState.rsOpenStopped);
  Self.MI_RCSs[i].MI_Stop.Enabled := ((RCSs[i].state = TRCSState.rsStartedScanned) or (RCSs[i].state = TRCSState.rsStartedNotScanned));
  Self.MI_RCSs[i].MI_Settings.Enabled := RCSs[i].libLoaded;
  Self.MI_RCSs[i].MI_NoLib.Enabled := (RCSs[i].state = TRCSState.rsClosed);

  var anyLoaded: Boolean := False;
  for var libmi: TMenuItem in Self.MI_RCSs[i].MI_Libs do
  begin
    libmi.Enabled := (RCSs[i].state = TRCSState.rsClosed);
    const libFn: string = StringReplace(libmi.Caption, '&', '', [rfReplaceAll]);
    libmi.Checked := (libFn = ExtractFileName(RCSs[i].lib));
    if (libmi.Checked) then
      anyLoaded := True;
  end;
  if (not anyLoaded) then
    Self.MI_RCSs[i].MI_NoLib.Checked := True;
end;

procedure TF_Main.MI_RCS_Lib_Click(Sender: TObject);
begin
  const rcsi: Integer = TMenuItem(Sender).Tag;
  const fn: string = StringReplace(TMenuItem(Sender).Caption, '&', '', [rfReplaceAll]);
  var rcs: TRCS := RCSs[rcsi];

  Screen.Cursor := crHourGlass;
  rcs.Log('-> ' + fn, TLogLevel.llInfo);

  try
    RCSs.LoadLib(rcsi, fn);
    rcs.Log('RCS'+IntToStr(rcsi)+': načteno ' + fn, TLogLevel.llInfo, True);
  except
    on E: Exception do
    begin
      StrMessageBox('Nelze načíst knihovnu ' + rcs.libDir + '\' + fn + ':' + #13#10 + E.Message, 'Nelze načíst knihovnu',
        MB_OK OR MB_ICONWARNING);
    end;
  end;

  RCSTableData[rcsi].LoadToTable(not Self.CHB_RCSs_Show_Only_Active[rcsi].Checked);
  Self.RCSGUIUpdateAll();
  Screen.Cursor := crDefault;
end;

procedure TF_Main.MI_RCS_UnloadLib_Click(Sender: TObject);
begin
  const rcsi: Integer = TMenuItem(Sender).Tag;
  var rcs: TRCS := RCSs[rcsi];

  if (not rcs.libLoaded) then
    Exit(); // already not loaded

  Screen.Cursor := crHourGlass;

  try
    rcs.UnloadLib();
    rcs.Log('načtena žádná knihovna', TLogLevel.llInfo, True);
  except
    on E: Exception do
    begin
      Screen.Cursor := crDefault;
      StrMessageBox('Nelze odnačíst knihovnu:' + #13#10 + E.Message, 'Nelze odnačíst knihovnu', MB_OK OR MB_ICONWARNING);
      Exit();
    end;
  end;

  RCSTableData[rcsi].LoadToTable(not Self.CHB_RCSs_Show_Only_Active[rcsi].Checked);
  Self.RCSGUIUpdateAll();
  Screen.Cursor := crDefault;
end;

procedure TF_Main.UpdateRCSLibsList();
  procedure AddLib(systemI: Integer; name: string);
  begin
    var item := TMenuItem.Create(Self.MI_RCSs[systemI].MI_Root);
    item.Caption := name;
    item.RadioItem := True;
    item.Tag := systemI;
    item.OnClick := Self.MI_RCS_Lib_Click;
    Self.MI_RCSs[systemI].MI_Root.Add(item);
    Self.MI_RCSs[systemI].MI_Libs.Add(item)
  end;

begin
  for var i: Integer := 0 to RCSs._RCSS_MAX do
  begin
    for var libmi: TMenuItem in Self.MI_RCSs[i].MI_Libs do
    begin
      Self.MI_RCSs[i].MI_Root.Remove(libmi);
      libmi.Free();
    end;
    Self.MI_RCSs[i].MI_Libs.Clear();

    var sr: TSearchRec;
    if (FindFirst(RCSs[i].libDir + '\*.dll', faAnyFile, sr) = 0) then
    begin
      if ((sr.Attr AND faDirectory) = 0) then
        AddLib(i, sr.name);

      while (FindNext(sr) = 0) do
        if ((sr.Attr AND faDirectory) = 0) then
          AddLib(i, sr.name);

      SysUtils.FindClose(sr);
    end;
  end;


  Self.RCSGUIUpdateAll();
end;

procedure TF_Main.MI_RCS_Settings_Click(Sender: TObject);
begin
  const rcsi: Integer = TMenuItem(Sender).Tag;

  if (not RCSs[rcsi].libLoaded) then
  begin
    StrMessageBox('RCS knihovna není načtena!', 'Info', MB_OK OR MB_ICONINFORMATION);
    Exit();
  end;

  if (not RCSs[rcsi].HasDialog()) then
  begin
    StrMessageBox('Používaná knihovna nemá konfigurační okno.', 'Info', MB_OK OR MB_ICONINFORMATION);
    Exit();
  end;

  Screen.Cursor := crHourGlass;
  try
    RCSs[rcsi].ShowConfigDialog();
  except
    on E: Exception do
    begin
      Screen.Cursor := crDefault;
      StrMessageBox('Nelze zobrazit konfigurační dialog RCS'+IntToStr(rcsi)+': ' + E.Message, 'Varování',
        MB_OK OR MB_ICONWARNING);
      Exit();
    end;
  end;
  Screen.Cursor := crDefault;
  RCSs[rcsi].Log('Zobrazen ConfigDialog', TLogLevel.llInfo);
end;

procedure TF_Main.A_RCSs_OpenExecute(Sender: TObject);
begin
  for var i: Integer := 0 to RCSs._RCSS_MAX do
  begin
    if ((RCSs[i].libLoaded) and (not RCSs[i].ready)) then
    begin
      StrMessageBox('RCS'+IntToStr(i)+' není připraveno k zapnutí systému' + #13#10 +
        'Možné příčiny:' + #13#10 + ' - nenačtena platná knihovna', 'Nelze spustit', MB_OK OR MB_ICONWARNING);
      Exit();
    end;
  end;
  if (not RCSs.AnyLibLoaded()) then
  begin
    StrMessageBox('Nanačtena žádná RCS knihovna', 'Nelze spustit', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  for var i: Integer := 0 to RCSs._RCSS_MAX do
    if ((RCSs[i].libLoaded) and (RCSs[i].state = rsClosed)) then
      Self.MI_RCS_Open_Click(Self.MI_RCSs[i].MI_Open);
end;

procedure TF_Main.A_RCSs_StopExecute(Sender: TObject);
begin
  for var i: Integer := 0 to RCSs._RCSS_MAX do
    if ((RCSs[i].state = rsStartedNotScanned) or (RCSs[i].state = rsStartedScanned)) then
      Self.MI_RCS_Stop_Click(Self.MI_RCSs[i].MI_Stop);
end;

procedure TF_Main.MI_RCS_Open_Click(Sender: TObject);
begin
  const rcsi: Integer = TMenuItem(Sender).Tag;
  var rcs: TRCS := RCSs[rcsi];

  if ((SystemData.Status = starting) and (rcs.Opened())) then
  begin
    Self.MI_RCS_Start_Click(Self.MI_RCSs[rcsi].MI_Start);
    Exit();
  end;

  try
    rcs.Open();
  except
    on E: ERCSAlreadyOpened do
      Self.OnRCSErrOpen(rcs, 'RCS je již otevřeno!');
    on E: ERCSCannotOpenPort do
      Self.OnRCSErrOpen(rcs,
        'Nepodařilo se otevřít USB port, otevřete konfigurační okno RCS driveru a zkontrolujte, že je vybrán správný port!');
    on E: Exception do
      Self.OnRCSErrOpen(rcs, 'Nastala kritická chyba : ' + E.Message);
  end;
end;

procedure TF_Main.A_RCSs_CloseExecute(Sender: TObject);
begin
  for var i: Integer := 0 to RCSs._RCSS_MAX do
    if (RCSs[i].state = rsOpenStopped) then
      Self.MI_RCS_Close_Click(Self.MI_RCSs[i].MI_Close);
end;

procedure TF_Main.MI_RCS_Close_Click(Sender: TObject);
begin
  const rcsi: Integer = TMenuItem(Sender).Tag;
  var rcs: TRCS := RCSs[rcsi];

  if ((SystemData.Status = stopping) and (RCSs.AllRCSsState(rsClosed))) then
  begin
    logging.Log('System: stop OK', TLogLevel.llInfo, lsSystem, True);
    SystemData.Status := null;
    Self.UpdateSystemButtons();
    Exit();
  end;

  if (rcs.state = rsClosed) then
    Exit();

  try
    rcs.Close();
  except
    on E: ERCSNotOpened do
      Self.OnRCSErrClose(rcs, 'RCS není otevřeno, nelze jej proto zavřít!');
    on E: ERCSScanningNotFinished do
      Self.OnRCSErrClose(rcs, 'RCS nelze uzavřít před sokončneíms kenování modulů!');
    on E: Exception do
      Self.OnRCSErrClose(rcs, 'Nastala kritická chyba : ' + E.Message);
  end;
end;

procedure TF_Main.A_RCSs_GoExecute(Sender: TObject);
begin
  for var i: Integer := 0 to RCSs._RCSS_MAX do
    if (RCSs[i].state = rsOpenStopped) then
      Self.MI_RCS_Start_Click(Self.MI_RCSs[i].MI_Start);
end;

procedure TF_Main.MI_RCS_Start_Click(Sender: TObject);
begin
  const rcsi: Integer = TMenuItem(Sender).Tag;
  var rcs: TRCS := RCSs[rcsi];

  var allStaredScanned := True;
  for var i: Integer := 0 to RCSs._RCSS_MAX do
    if ((RCSs[i].state <> rsStartedScanned) and (RCSs[i].libLoaded)) then
      allStaredScanned := False;

  if ((SystemData.Status = starting) and (allStaredScanned)) then
  begin
    Self.A_Trk_ConnectExecute(nil);
    Exit();
  end;
  if ((SystemData.Status = starting) and (rcs.Started())) then
    Exit();

  try
    rcs.Start();
  except
    on E: ERCSAlreadyStarted do
      Self.OnRCSErrStart(rcs, 'Komunikace již probíhá!');
    on E: ERCSFirmwareTooLow do
      Self.OnRCSErrStart(rcs, 'Firmware RCS-USB modulu je starý, nelze se připojit k takto starému FW!');
    on E: ERCSNoModules do
      Self.OnRCSErrStart(rcs, 'Na sběrnici nebyl nalezen žádný RCS modul, nelze spustit komunikaci!');
    on E: ERCSNotOpened do
      Self.OnRCSErrStart(rcs, 'Nepřipojeno k RCS-USB, připojte se nejdříve k RCS-USB!');
    on E: ERCSScanningNotFinished do
      Self.OnRCSErrStart(rcs, 'Neproběhl sken modulů, vyčkejte na dokončení skenu modulů!');
    on E: Exception do
      Self.OnRCSErrStart(rcs, 'Nastala kritická chyba : ' + E.Message);
  end;
end;

procedure TF_Main.MI_RCS_Stop_Click(Sender: TObject);
begin
  const rcsi: Integer = TMenuItem(Sender).Tag;
  var rcs: TRCS := RCSs[rcsi];

  if ((SystemData.Status = stopping) and (not rcs.Started())) then
  begin
    Self.MI_RCS_Close_Click(Self.MI_RCSs[rcsi].MI_Close);
    Exit();
  end;

  try
    rcs.Stop();
  except
    on E: ERCSNotStarted do
      Self.OnRCSErrStop(rcs, 'RCS komunikace není spuštěna, nelze ji proto zastavit!');
    on E: Exception do
      Self.OnRCSErrStop(rcs, 'Nastala kritická chyba : ' + E.Message);
  end;
end;

procedure TF_Main.RCSShapesInit();
begin
  Self.S_RCS_Open[0] := Self.S_RCS0_open;
  Self.S_RCS_Open[1] := Self.S_RCS1_open;
  Self.S_RCS_Open[2] := Self.S_RCS2_open;
  Self.S_RCS_Open[3] := Self.S_RCS3_open;

  Self.S_RCS_Started[0] := Self.S_RCS0_started;
  Self.S_RCS_Started[1] := Self.S_RCS1_started;
  Self.S_RCS_Started[2] := Self.S_RCS2_started;
  Self.S_RCS_Started[3] := Self.S_RCS3_started;
end;

procedure TF_Main.RCSShapesUpdate();
const SPACE_PX: Integer = 5;
begin
  if (not Self.mRCSGUIInitialized) then
    Exit();

  var left: Integer := Self.S_RCS0_open.Left;
  for var i:Integer := 0 to RCSs._RCSS_MAX do
  begin
    Self.S_RCS_Open[i].Visible := (RCSs[i].lib <> ''); // true if lib unsuccessfully loaded
    Self.S_RCS_Open[i].Left := left;
    Self.S_RCS_Started[i].Visible := Self.S_RCS_Open[i].Visible;
    Self.S_RCS_Started[i].Left := Self.S_RCS_Open[i].Left;

    if (Self.S_RCS_Open[i].Visible) then
      left := left + Self.S_RCS0_open.Width + SPACE_PX;

    case (RCSs[i].state) of
      rsClosed: begin
        Self.S_RCS_Open[i].Brush.Color := clRed;
        Self.S_RCS_Started[i].Brush.Color := clRed;
      end;
      rsOpening, rsClosing: begin
        Self.S_RCS_Open[i].Brush.Color := clBlue;
        Self.S_RCS_Started[i].Brush.Color := clRed;
      end;
      rsOpenStopped: begin
        Self.S_RCS_Open[i].Brush.Color := clLime;
        Self.S_RCS_Started[i].Brush.Color := clRed;
      end;
      rsStarting, rsStopping, rsStartedNotScanned: begin
        Self.S_RCS_Open[i].Brush.Color := clLime;
        Self.S_RCS_Started[i].Brush.Color := clBlue;
      end;
      rsStartedScanned: begin
        Self.S_RCS_Open[i].Brush.Color := clLime;
        Self.S_RCS_Started[i].Brush.Color := clLime;
      end;
    end;
  end;

  Self.L_RCS_open.Left := left;
  Self.L_RCS_started.Left := left;
end;

procedure TF_Main.RCSSLVStateInit();
begin
  Self.LV_RCSs_State[0] := Self.LV_RCS0_State;
  Self.LV_RCSs_State[1] := Self.LV_RCS1_State;
  Self.LV_RCSs_State[2] := Self.LV_RCS2_State;
  Self.LV_RCSs_State[3] := Self.LV_RCS3_State;

  Self.CHB_RCSs_Show_Only_Active[0] := Self.CHB_RCS0_Show_Only_Active;
  Self.CHB_RCSs_Show_Only_Active[1] := Self.CHB_RCS1_Show_Only_Active;
  Self.CHB_RCSs_Show_Only_Active[2] := Self.CHB_RCS2_Show_Only_Active;
  Self.CHB_RCSs_Show_Only_Active[3] := Self.CHB_RCS3_Show_Only_Active;

  Self.CHB_RCSs_Log[0] := Self.CHB_RCS0_Log;
  Self.CHB_RCSs_Log[1] := Self.CHB_RCS1_Log;
  Self.CHB_RCSs_Log[2] := Self.CHB_RCS2_Log;
  Self.CHB_RCSs_Log[3] := Self.CHB_RCS3_Log;
end;

// --- events from RCS lib begin ---

procedure TF_Main.OnRCSBeforeOpen(Sender: TObject);
begin
  var rcs: TRCS := TRCS(Sender);

  Self.RCSGUIUpdateAll();
  Self.UpdateSystemButtons();

  rcs.Log('Otevírám zařízení, hledám moduly...', TLogLevel.llInfo, True);
end;

procedure TF_Main.OnRCSOpen(Sender: TObject);
begin
  var rcs: TRCS := TRCS(Sender);

  Self.RCSGUIUpdateAll();
  Self.UpdateSystemButtons();

  try
    rcs.Log('Otevřeno, modulů: ' + IntToStr(rcs.GetModuleCount()), TLogLevel.llInfo, True);
  except
    rcs.Log('Otevřeno, neznámý počet modulů!', TLogLevel.llWarning, True);
  end;

  try
    rcs.Log('Verze zařízení: ' + rcs.GetDeviceVersion(), TLogLevel.llInfo);
  except

  end;

  F_Tester.AfterRCSOpen(rcs.systemI);
  RCSTableData[rcs.systemI].LoadToTable(not Self.CHB_RCSs_Show_Only_Active[rcs.systemI].Checked);

  if (SystemData.Status = starting) then
  begin
    // any RCS modules missing?
    var str: string := '';
    for var i := 0 to rcs.maxModuleAddr + 1 do
    begin
      if ((rcs.GetNeeded(i)) and (not rcs.IsModule(i))) then
      begin
        if (Length(str) > 0) then
          str := str + ', ';
        str := str + IntToStr(i);
      end;
    end;
    if (str <> '') then
      rcs.Log('Chybí moduly ' + str, TLogLevel.llWarning, True);

    Self.MI_RCS_Start_Click(Self.MI_RCSs[rcs.systemI].MI_Start);
  end;
end;

procedure TF_Main.OnRCSBeforeClose(Sender: TObject);
begin
  var rcs: TRCS := TRCS(Sender);

  Self.RCSGUIUpdateAll();
  Self.UpdateSystemButtons();

  rcs.Log('Uzavírám zařízení...', TLogLevel.llInfo, True);
end;

procedure TF_Main.OnRCSClose(Sender: TObject);
begin
  var rcs: TRCS := TRCS(Sender);

  Self.RCSGUIUpdateAll();
  Self.UpdateSystemButtons();

  // may happen when RCS USB disconnects
  if (not RCSs.AnyRCSStateGTE(TRCSState.rsOpenStopped)) then
    Self.RCSStopSafeReaction();

  rcs.Log('Uzavřeno', TLogLevel.llInfo, True);

  PanelServer.BroadcastBottomError('Odpojeno od RCS'+IntToStr(rcs.systemI)+'!', 'TECHNOLOGIE');

  if ((SystemData.Status = stopping) and (RCSs.AllRCSsState(rsClosed))) then
  begin
    logging.Log('System: stop OK', TLogLevel.llInfo, lsSystem, True);
    SystemData.Status := null;
    Self.UpdateSystemButtons();
  end;

  RCSTableData[rcs.systemI].UpdateTable();
end;

procedure TF_Main.RCSStopSafeReaction();
begin
  if (Blocks.Enabled) then
  begin
    Blocks.Disable();
    Trains.ClearPOdj();
    Self.A_SaveStavExecute(Self);
  end;
  Trains.StopAllTrains();
end;

procedure TF_Main.OnRCSErrOpen(Sender: TObject; errMsg: string);
begin
  var rcs: TRCS := TRCS(Sender);

  Self.RCSGUIUpdateAll();
  Self.UpdateSystemButtons();

  SystemData.Status := TSystemStatus.null;

  rcs.Log('Chyba otevírání: ' + errMsg, TLogLevel.llError, True);
end;

procedure TF_Main.OnRCSErrClose(Sender: TObject; errMsg: string);
begin
  var rcs: TRCS := TRCS(Sender);

  Self.RCSGUIUpdateAll();
  Self.UpdateSystemButtons();

  SystemData.Status := null;

  rcs.Log('Chyba uzavírání: ' + errMsg, TLogLevel.llError, True);
end;

procedure TF_Main.OnRCSBeforeStart(Sender: TObject);
begin
  var rcs: TRCS := TRCS(Sender);

  Self.RCSGUIUpdateAll();
  Self.UpdateSystemButtons();

  rcs.Log('Spouštím komunikaci...', TLogLevel.llInfo, True);
end;

procedure TF_Main.OnRCSStart(Sender: TObject);
begin
  var rcs: TRCS := TRCS(Sender);

  Self.RCSGUIUpdateAll();
  Self.UpdateSystemButtons();

  rcs.Log('Komunikace spuštěna, čekám na první sken všech modulů...', TLogLevel.llInfo, True);

  RCSTableData[rcs.systemI].UpdateTable();
end;

procedure TF_Main.OnRCSScanned(Sender: TObject);
begin
  var rcs: TRCS := TRCS(Sender);

  Self.RCSGUIUpdateAll();
  RCSTableData[rcs.systemI].UpdateTable();

  rcs.Log('Moduly naskenovány', TLogLevel.llInfo, True);

  var allStaredScanned := True;
  for var i: Integer := 0 to RCSs._RCSS_MAX do
    if ((RCSs[i].state <> rsStartedScanned) and (RCSs[i].libLoaded)) then
      allStaredScanned := False;

  if (allStaredScanned) then
    Areas.InitLights();

  if ((SystemData.Status = starting) and (allStaredScanned)) then
    Self.A_Trk_ConnectExecute(nil);
end;

procedure TF_Main.OnRCSBeforeStop(Sender: TObject);
begin
  var rcs: TRCS := TRCS(Sender);

  Self.RCSGUIUpdateAll();
  Self.UpdateSystemButtons();

  rcs.Log('Zastavuji komunikaci...', TLogLevel.llInfo, True);
end;

procedure TF_Main.OnRCSStop(Sender: TObject);
begin
  var rcs: TRCS := TRCS(Sender);

  if (not RCSs.AnyRCSStateGTE(TRCSState.rsStartedNotScanned)) then
    Self.RCSStopSafeReaction();

  modelTime.started := false;

  Self.RCSGUIUpdateAll();
  Self.UpdateSystemButtons();

  if ((F_Tester.Showing) and (not RCSs.AnyRCSState(rsStartedScanned)) and (not RCSs.AnyRCSState(rsStartedNotScanned))) then
    F_Tester.Close();

  rcs.Log('Komunikace zastavena', TLogLevel.llInfo, True);
  RCSTableData[rcs.systemI].UpdateTable();

  if ((Self.Showing) and (Self.PC_1.ActivePage = F_Main.TS_Bloky)) then
    BlocksTablePainter.UpdateTable();

  PanelServer.BroadcastBottomError('Ztráta komunikace RCS'+IntToStr(rcs.systemI)+'!', 'TECHNOLOGIE');

  if (SystemData.Status = stopping) then
    Self.MI_RCS_Close_Click(Self.MI_RCSs[rcs.systemI].MI_Close);
end;

procedure TF_Main.OnRCSErrStart(Sender: TObject; errMsg: string);
begin
  var rcs: TRCS := TRCS(Sender);

  Self.RCSGUIUpdateAll();
  Self.UpdateSystemButtons();

  SystemData.Status := TSystemStatus.null;

  rcs.Log('Chyba spouštění komunikace: ' + errMsg, TLogLevel.llError, True);
end;

procedure TF_Main.OnRCSErrStop(Sender: TObject; errMsg: string);
begin
  var rcs: TRCS := TRCS(Sender);

  Self.RCSGUIUpdateAll();

  SystemData.Status := TSystemStatus.null;

  rcs.Log('Chyba zastavování komunikace: ' + errMsg, TLogLevel.llError, True);
end;

procedure TF_Main.OnRCSReady(Sender: TObject; ready: Boolean);
begin
  var rcs: TRCS := TRCS(Sender);

  Self.RCSGUIUpdateAll();

  try
    if ((ready) and (diag.simInputs) and (rcs.simulation)) then
      Simulation.InputSim();
  except
    on E: Exception do
      rcs.Log('Nelze provést inputSim: ' + E.Message, llError);
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
  trakce.Log('Změna knihovny -> ' + fn, TLogLevel.llInfo);
  Self.SB1.Panels.Items[_SB_TRAKCE_LIB].Text := '-';
  try
    trakce.LoadLib(trakce.libDir + '\' + fn);
    logging.Log('Trakce: načteno ' + fn, TLogLevel.llInfo, lsTrakce, True);
    Self.SB1.Panels.Items[_SB_TRAKCE_LIB].Text := ExtractFileName(trakce.Lib);
  except
    on E: Exception do
    begin
      Screen.Cursor := crDefault;
      StrMessageBox('Nelze načíst knihovnu ' + fn + ':' + #13#10 + E.Message, 'Nelze načíst knihovnu',
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

  if (FindFirst(trakce.libDir + '\*.dll', faAnyFile, SR) = 0) then
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
  if (not trakce.HasDialog()) then
  begin
    StrMessageBox('Aktuální knihovna nemá konfigurační okno.', 'Info', MB_OK OR MB_ICONINFORMATION);
    Exit();
  end;

  Screen.Cursor := crHourGlass;
  try
    trakce.ShowConfigDialog();
  except
    on E: Exception do
    begin
      Screen.Cursor := crDefault;
      StrMessageBox('Nelze zobrazit konfigurační dialog: ' + E.Message, 'Varování',
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
    StrMessageBox('Seznam knihoven úspěšně aktualizován.', 'Info', MB_OK OR MB_ICONINFORMATION);
  except
    on E: Exception do
      ErrorMessageBox('Seznam knihoven se nepodařilo aktualizovat:', E.Message, 'Chyba',
        MB_OK OR MB_ICONWARNING);
  end;
end;

procedure TF_Main.A_Trk_ConnectExecute(Sender: TObject);
begin
  if ((SystemData.Status = starting) and (trakce.ConnectedSafe())) then
  begin
    Self.A_DCC_GoExecute(Self);
    Exit();
  end;

  try
    trakce.Connect();
  except
    on E: Exception do
    begin
      if (trakce.opening) then
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
  if ((SystemData.Status = stopping) and (not trakce.ConnectedSafe())) then
  begin
    for var i: Integer := 0 to RCSs._RCSS_MAX do
      if ((RCSs[i].ready) and (SystemData.Status = stopping)) then // stopping could be stopped after 0nd RCS stop&close
        Self.MI_RCS_Stop_Click(Self.MI_RCSs[i].MI_Stop);
    Exit();
  end;

  try
    trakce.Disconnect();
  except
    on E: Exception do
    begin
      trakce.Log('CLOSE: error: ' + E.Message, TLogLevel.llError);
      StrMessageBox('Chyba pri uzavírání komunikace s centrálou:' + #13#10 + E.Message + #13#10 +
        'Více informací naleznete v logu.', 'Chyba', MB_OK OR MB_ICONERROR);
    end;
  end;

  Application.ProcessMessages();
end;

procedure TF_Main.A_Locos_AcquireExecute(Sender: TObject);
begin
  logging.Log('Vozidla: přebírám...', TLogLevel.llInfo, lsTrakce, True);
  Self.S_locos_acquired.Brush.Color := clBlue;
  Self.G_locos_acquired.ForeColor := clBlue;

  Self.G_locos_acquired.MaxValue := 0;
  for var addr := 0 to TRVDatabase._MAX_ADDR - 1 do
    if (RVDb[addr] <> nil) and (RVDb[addr].ShouldAcquire()) then
      Self.G_locos_acquired.MaxValue := Self.G_locos_acquired.MaxValue + 1;
  if (Self.G_locos_acquired.MaxValue = 0) then
    Self.G_locos_acquired.MaxValue := 1;

  RVDb.TrakceAcquireAllUsed(Self.OnTrkAllAcquired, Self.OnTrkAcquireError, Self.OnTrkLocoAcquired);
end;

procedure TF_Main.A_Locos_ReleaseExecute(Sender: TObject);
begin
  logging.Log('Vozidla: odhlašuji...', TLogLevel.llInfo, lsTrakce, True);
  Self.S_locos_acquired.Brush.Color := clBlue;
  Self.G_locos_acquired.ForeColor := clBlue;
  RVDb.TrakceReleaseAllUsed(Self.OnTrkAllReleased, Self.OnTrkLocoReleased);
end;

procedure TF_Main.OnTrkAllAcquired(Sender: TObject);
begin
  logging.Log('Vozidla: všechna vozidla převzata', TLogLevel.llInfo, lsTrakce, True);

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

  StrMessageBox('Nepodařilo se převzít všechna vozidla, více informací v logu.', 'Chyba',
    MB_OK OR MB_ICONWARNING);
end;

procedure TF_Main.OnTrkAllReleased(Sender: TObject);
begin
  logging.Log('Vozidla: všechna vozidla odhlášena', TLogLevel.llInfo, lsTrakce, True);

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
  if ((SystemData.Status = starting) and (trakce.TrackStatusSafe() = TTrkStatus.tsOn)) then
  begin
    Self.A_Locos_AcquireExecute(Self);
    Exit();
  end;

  logging.Log('DCC: zapínám', TLogLevel.llInfo, lsTrakce, True);

  try
    trakce.SetTrackStatus(tsOn, TTrakce.Callback(Self.OnDCCGoOk), TTrakce.Callback(Self.OnDCCGoError));
  except
    on E: Exception do
    begin
      SystemData.Status := null;
      logging.Log('DCC: START: ERR ' + E.Message, TLogLevel.llError, lsTrakce, True);
      ExceptionMessageBox('Chyba při DCC GO:', E);
    end;
  end;
end;

procedure TF_Main.A_DCC_StopExecute(Sender: TObject);
begin
  logging.Log('DCC: vypínám', TLogLevel.llInfo, lsTrakce, True);

  try
    trakce.SetTrackStatus(tsOff, TTrakce.Callback(Self.OnDCCStopOk), TTrakce.Callback(Self.OnDCCStopError));
  except
    on E: Exception do
    begin
      logging.Log('DCC: STOP: ERR ' + E.Message, TLogLevel.llError, lsTrakce, True);
      ExceptionMessageBox('Chyba při DCC STOP:', E);
    end;
  end;
end;

procedure TF_Main.A_FuncsSetExecute(Sender: TObject);
begin
  F_FuncsSet.Show();
end;

procedure TF_Main.OnDCCGoOk(Sender: TObject; Data: Pointer);
begin
  trakce.emergency := False;
end;

procedure TF_Main.OnDCCGoError(Sender: TObject; Data: Pointer);
begin
  SystemData.Status := TSystemStatus.null;
  Self.UpdateSystemButtons();
  Self.A_DCC_Go.Enabled := true;
  Self.A_DCC_Stop.Enabled := true;
  Self.S_DCC.Brush.Color := clGray;
  logging.Log('DCC: START: ERR: centrála neodpověděla na příkaz', TLogLevel.llError, lsTrakce);
  StrMessageBox('Centrála neodpověděla na příkaz DCC START', 'Varování', MB_OK OR MB_ICONWARNING);
end;

procedure TF_Main.OnDCCStopOk(Sender: TObject; Data: Pointer);
begin
  trakce.emergency := False;
end;

procedure TF_Main.OnDCCStopError(Sender: TObject; Data: Pointer);
begin
  trakce.emergency := True;
  logging.Log('DCC: STOP: ERR: centrála neodpověděla na příkaz', TLogLevel.llError, lsTrakce, True);
  Self.UpdateSystemButtons();
  Self.A_DCC_Go.Enabled := true;
  Self.A_DCC_Stop.Enabled := true;
  Self.S_DCC.Brush.Color := clGray;
  StrMessageBox('Centrála neodpověděla na příkaz DCC STOP', 'Varování', MB_OK OR MB_ICONWARNING);
end;

procedure TF_Main.OnTrkBeforeOpen(Sender: TObject);
begin
  Self.A_Trk_Connect.Enabled := false;
  Self.A_Trk_Disconnect.Enabled := false;
  Self.A_System_Start.Enabled := false;
  Self.A_System_Stop.Enabled := false;
  Self.SB1.Panels.Items[_SB_TRAKCE_STAV].Text := 'Připojování...';
  Self.S_Trakce_Connected.Brush.Color := clBlue;
  logging.Log('Centrála: připojování...', TLogLevel.llInfo, lsTrakce, True);
  Self.B_RV_Add.Enabled := false;
  Self.B_RV_Delete.Enabled := false;
  Self.MI_Trk_Libs.Enabled := false;
  Application.ProcessMessages();
end;

procedure TF_Main.OnTrkAfterOpen(Sender: TObject);
begin
  Self.A_Trk_Connect.Enabled := false;
  Self.A_Trk_Disconnect.Enabled := true;
  Self.SB1.Panels.Items[_SB_TRAKCE_STAV].Text := 'Trakce připojena';
  logging.Log('Centrála: připojeno', TLogLevel.llInfo, lsTrakce, True);
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
  logging.Log('Centrála: odpojování...', TLogLevel.llInfo, lsTrakce, True);
  Self.S_Trakce_Connected.Brush.Color := clBlue;
  Self.G_locos_acquired.Progress := 0;
  Self.S_locos_acquired.Brush.Color := clRed;
  Application.ProcessMessages();
end;

procedure TF_Main.OnTrkAfterClose(Sender: TObject);
begin
  Self.A_Trk_Connect.Enabled := true;
  Self.A_Trk_Disconnect.Enabled := false;
  Self.SB1.Panels.Items[_SB_TRAKCE_STAV].Text := 'Trakce odpojena';
  logging.Log('Centrála: odpojena', TLogLevel.llInfo, lsTrakce, True);
  Self.S_Trakce_Connected.Brush.Color := clRed;
  Self.A_Locos_Acquire.Enabled := false;
  Self.A_Locos_Release.Enabled := false;
  Self.B_RV_Add.Enabled := true;
  Self.S_locos_acquired.Brush.Color := clRed;
  Self.G_locos_acquired.Progress := 0;
  Self.MI_Trk_Libs.Enabled := true;
  Self.UpdateSystemButtons();

  RegCollector.CloseAll();
  RVTableData.LoadToTable();

  Self.S_DCC.Brush.Color := clGray;
  Self.A_DCC_Go.Enabled := false;
  Self.A_DCC_Stop.Enabled := false;
  Self.A_FuncsSet.Enabled := false;
  if (F_FuncsSet.Showing) then
    F_FuncsSet.Close();

  RVDb.CSReset();
  Application.ProcessMessages();

  // no clinets shoudl be connected
  // when disconnect called with clients connected, fatal error happened
  PanelServer.BroadcastBottomError('Výpadek trakce, ztráta kontroly nad jízdou!', 'TECHNOLOGIE');

  if (SystemData.Status = stopping) then
    for var i: Integer := 0 to RCSs._RCSS_MAX do
      if ((RCSs[i].ready) and (SystemData.Status = stopping)) then // stopping could be stopped after 0nd RCS stop&close
        Self.MI_RCS_Stop_Click(Self.MI_RCSs[i].MI_Stop);
end;

procedure TF_Main.OnTrkReady(Sender: TObject; ready: Boolean);
begin
  Self.A_Trk_Connect.Enabled := ready and (not trakce.ConnectedSafe());
end;

procedure TF_Main.OnTrkErrOpen(Sender: TObject; errMsg: string);
begin
  if (SystemData.Status = TSystemStatus.starting) then
  begin
    SystemData.Status := TSystemStatus.null;
    Self.A_System_Start.Enabled := true;
    Self.A_System_Stop.Enabled := true;
  end;

  trakce.opening := false;
  logging.Log('Trakce OPEN FAIL: ' + errMsg, TLogLevel.llError, lsTrakce, True);
  ErrorMessageBox('Při otevírání Trakce nastala chyba:', errMsg);
end;

procedure TF_Main.OnTrkStatusChange(Sender: TObject; trkStatus: TTrkStatus);
begin
  if (trkStatus = TTrkStatus.tsOn) then
  begin
    // je DCC
    trakce.DCCGoTime := Now;
    Self.S_DCC.Brush.Color := clLime;
    logging.Log('DCC: go', TLogLevel.llInfo, lsTrakce, True);

    if (trakce.ConnectedSafe()) then
    begin
      Self.A_DCC_Go.Enabled := false;
      Self.A_DCC_Stop.Enabled := true;
    end;

    if ((SystemData.Status = starting) and (trakce.ConnectedSafe())) then
      Self.A_Locos_AcquireExecute(nil);

    PanelServer.DCCStart();
  end else begin

    Areas.BroadcastPlaySound(_SND_ERROR, false, TAreaRights.write);

    // neni DCC
    Self.S_DCC.Brush.Color := clRed;
    logging.Log('DCC: stop', TLogLevel.llInfo, lsTrakce, True);

    if (trakce.ConnectedSafe()) then
    begin
      Self.A_DCC_Go.Enabled := true;
      Self.A_DCC_Stop.Enabled := false;
    end;

    if ((SystemData.Status = starting) and (trakce.ConnectedSafe())) then
      Self.A_DCC_GoExecute(Self);

    PanelServer.DCCStop();
  end; // else state

  Simulation.DccChangedBoosterSim(trkStatus = TTrkStatus.tsOn);
end;

procedure TF_Main.OnTrkEmergencyChanged(Sender: TObject);
begin
  if (trakce.emergency) then
  begin
    Self.LogBrief('Trakce: NOUZOVÝ STAV', TLogLevel.llError);
    Self.L_TrkState.Font.Style := [TFontStyle.fsBold];
    Self.L_TrkState.Font.Color := clRed;
    Self.L_TrkState.Caption := 'Stav: NOUZE';
  end else begin
    Self.LogBrief('Trakce: nouzový stav pominul', TLogLevel.llWarning);
    Self.L_TrkState.Font.Style := [];
    Self.L_TrkState.Font.Color := clBlack;
    Self.L_TrkState.Caption := 'Stav: ok';
  end;
end;

procedure TF_Main.A_Turnoff_FunctionsExecute(Sender: TObject);
begin
  Logging.Log('Vypínám zvuky vozidel...', TLogLevel.llInfo, lsTrakce, True);
  Application.ProcessMessages();
  trakce.TurnOffSound(TTrakce.Callback(Self.OnTrkAllFunctionTurnedOff),
    TTrakce.Callback(Self.OnTrkAllFunctionTurnedOff));
end;

procedure TF_Main.OnTrkAllFunctionTurnedOff(Sender: TObject; Data: Pointer);
begin
  logging.Log('Zvuky všech vozidel vypnuty', TLogLevel.llInfo, lsTrakce, True);
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
    modelTime.Update();
    JCDb.Update();
    MultiJCDb.Update();
    Boosters.Update();
    Areas.Update();
    UpdateCallMethod();
    RCSd.Update();
    trakce.Update();
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
  modelTime.started := not modelTime.started;
end;

procedure TF_Main.P_ZrychleniDblClick(Sender: TObject);
begin
  F_ModelTimeSet.OpenForm();
end;

procedure TF_Main.PM_Loco_DeleteClick(Sender: TObject);
begin
  if (Self.B_RV_Delete.Enabled) then
    Self.B_RV_DeleteClick(Self)
end;

procedure TF_Main.PM_Loco_EditClick(Sender: TObject);
begin
  if (Self.LV_Vehicles.Selected <> nil) then
    F_RVEdit.EditVehicle(RVDb[StrToInt(LV_Vehicles.Selected.Caption)]);
end;

procedure TF_Main.MI_Loco_Tacho_ResetClick(Sender: TObject);
begin
  if (Self.LV_Vehicles.Selected = nil) then
    Exit();
  var vehicle: TRV := RVDb[StrToInt(LV_Vehicles.Selected.Caption)];

  if (StrMessageBox('Opravdu vyresetovat ujetou dráhu vozidla '+IntToStr(vehicle.addr)+'?', 'Otázka', MB_YESNO OR MB_ICONQUESTION) = mrYes) then
  begin
    vehicle.ResetStats();
    RVTableData.UpdateLine(vehicle);
  end;
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
  if (PC_1.ActivePage = TS_Trains) then
    TrainTableData.UpdateTable();
  if (PC_1.ActivePage = Self.TS_RV) then
    RVTableData.UpdateTable();
  if (PC_1.ActivePage = TS_Areas) then
    ORsTableData.UpdateTable(true);
  if (PC_1.ActivePage = TS_Technologie) then
    PanelServer.GUIRefreshTable();
end;

procedure TF_Main.PM_BlokyPopup(Sender: TObject);
begin
  for var item in (Sender as TPopupMenu).Items do
    item.Enabled := ((Self.LV_Blocks.Selected <> nil) and (Blocks.GetBlkByIndex(Self.LV_Blocks.ItemIndex) <> nil));

  var blk := Blocks.GetBlkByIndex(Self.LV_Blocks.ItemIndex);
  Self.MI_Block_State.Enabled := (blk <> nil) and ((blk.typ = btTurnout) or (blk.typ = btTrack) or (blk.typ = btRT) or (blk.typ = btCrossing) or (blk.typ = btSignal) or (blk.typ = btRailway));
  Self.MI_Block_Houk.Visible := (blk <> nil) and ((blk.typ = btTrack) or (blk.typ = btRT));
end;

procedure TF_Main.PM_ResetVClick(Sender: TObject);
begin
  if (StrMessageBox('Pozor: tato operace zaráz přestaví potenciálně všechny výhybky na kolejišti, ' +
    'což může způsobit přetížení napájecích zdrojů. Chcete skutečně pokračovat?', 'Otázka',
    MB_YESNO OR MB_ICONWARNING OR MB_DEFBUTTON2) = mrYes) then
  begin
    Blocks.MoveTurnoutBasicPosition();
    logging.Log('Vyhýbky přestaveny do základní polohy', TLogLevel.llInfo, lsAny, True);
    StrMessageBox('Výhybky přestaveny do základních poloh.', 'Informace', MB_OK OR MB_ICONINFORMATION);
  end;
end;

procedure TF_Main.PM_Loco_RegClick(Sender: TObject);
begin
  if (Self.LV_Vehicles.Selected = nil) then
    Exit();

  if (trakce.ConnectedSafe()) then
  begin
    try
      RegCollector.Open(RVDb[StrToInt(Self.LV_Vehicles.Selected.Caption)]);
    except
      on E: Exception do
        StrMessageBox(E.Message, 'Varování', MB_OK OR MB_ICONWARNING);
    end;
  end else begin
    StrMessageBox('Nelze otevřít regulátor, hJOPserver není připojen k trakci!', 'Nelze otevřít regulátor', MB_OK OR MB_ICONWARNING);
  end;
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
        StrMessageBox('Technologie právě zapíná nebo vypíná systémy, aplikaci nelze momentálně zavřít.' +
          #13#10 + 'Nouzové ukončení programu lze provést spuštěním příkazu "app-exit" v konzoli',
          'Nelze ukončit program', MB_OK OR MB_ICONWARNING);
      end;

    TCloseInfo.ci_system_started:
      begin
        Log('Pokus o zavření okna bez ukončení komunikace se systémy', llWarning);
        if (StrMessageBox('Program není odpojen od systémů, odpojit od systémů?', 'Nelze ukončit program',
          MB_YESNO OR MB_ICONWARNING) = mrYes) then
          Self.A_System_StopExecute(Self);
      end;

    TCloseInfo.ci_rcs:
      begin
        Log('Pokus o zavření okna bez uzavření RCS', llWarning);
        if (StrMessageBox('Program není odpojen od RCS, odpojit?', 'Nelze ukončit program',
          MB_YESNO OR MB_ICONWARNING) = mrYes) then
          Self.A_System_StopExecute(Self);
      end;

    TCloseInfo.ci_server:
      begin
        Log('Pokus o zavření okna bez vypnutí panel serveru', llWarning);
        if (StrMessageBox('PanelServer stále běží, vypnout?', 'Nelze ukončit program',
          MB_YESNO OR MB_ICONWARNING) = mrYes) then
          PanelServer.Stop();
      end;

    TCloseInfo.ci_trakce:
      begin
        Log('Pokus o zavření okna bez odpojení od centrály', llWarning);
        if (StrMessageBox('Program není odpojen od centrály, odpojit?', 'Nelze ukončit program',
          MB_YESNO OR MB_ICONWARNING) = mrYes) then
          trakce.Disconnect();
      end;

    TCloseInfo.ci_yes:
      begin
        if (Self.CloseMessage) then
        begin
          CanClose := (StrMessageBox('Opravdu chcete ukončit hJOPserver?', 'hJOPserver',
            MB_YESNO OR MB_ICONQUESTION OR MB_DEFBUTTON2) = mrYes);
        end else begin
          CanClose := true;
        end;
      end;

  end; // case
end;

procedure TF_Main.FormCreate(Sender: TObject);
begin
  Self.mRCSGUIInitialized := False;
  Self.CloseMessage := true;
  Self.mCpuLoad := TCpuLoad.Create();

  Self.RCSSLVStateInit();

  JCTableData := TJCTableData.Create(Self.LV_JC);
  ABTableData := TABTableData.Create(Self.LV_AB);
  UsersTableData := TUsersTableData.Create(Self.LV_Users);
  for var rcsi := 0 to RCSs._RCSS_MAX do
    RCSTableData[rcsi] := TRCSTableData.Create(Self.LV_RCSs_State[rcsi], RCSs[rcsi]);
  TrainTableData := TTrainTableData.Create(Self.LV_Trains);
  RVTableData := TRVTableData.Create(Self.LV_Vehicles);
  ZesTableData := TZesTableData.Create(Self.LV_Boosters);
  ORsTableData := TORsTableData.Create(Self.LV_Areas);
  MultiJCTableData := TMultiJCTableData.Create(Self.LV_MultiJC);

  // assign RCS events:
  for var i: Integer := 0 to RCSs._RCSS_MAX do
  begin
    RCSs[i].BeforeOpen := Self.OnRCSBeforeOpen;
    RCSs[i].AfterOpen := Self.OnRCSOpen;
    RCSs[i].BeforeClose := Self.OnRCSBeforeClose;
    RCSs[i].AfterClose := Self.OnRCSClose;
    RCSs[i].BeforeStart := Self.OnRCSBeforeStart;
    RCSs[i].AfterStart := Self.OnRCSStart;
    RCSs[i].BeforeStop := Self.OnRCSBeforeStop;
    RCSs[i].AfterStop := Self.OnRCSStop;
    RCSs[i].OnScanned := Self.OnRCSScanned;
    RCSs[i].OnReady := Self.OnRCSReady;
  end;

  // assign Trakce events:
  trakce.BeforeOpen := Self.OnTrkBeforeOpen;
  trakce.AfterOpen := Self.OnTrkAfterOpen;
  trakce.BeforeClose := Self.OnTrkBeforeClose;
  trakce.AfterClose := Self.OnTrkAfterClose;
  trakce.OnReady := Self.OnTrkReady;
  trakce.OnTrackStatusChanged := Self.OnTrkStatusChange;
  trakce.OnEmergencyChanged := Self.OnTrkEmergencyChanged;
  trakce.OnOpenError := Self.OnTrkErrOpen;

  FuncNames.OnChange := Self.OnFuncNameChange;

  Self.LoadIniLibData();

  Self.Caption := 'hJOPserver – ' + StandardVersionBuildStr();
end;

procedure TF_Main.FormDestroy(Sender: TObject);
begin
  Self.mCpuLoad.Free();

  for var i: Integer := 0 to RCSs._RCSS_MAX do
    Self.MI_RCSs[i].MI_Libs.Free();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_Main.AE_1Message(var Msg: tagMSG; var Handled: Boolean);
begin
  Handled := false;

  case (Msg.Message) of
    WM_KEYDOWN: // key press
      begin
        Handled := false;
        RegCollector.KeyPress(Msg.wParam, Handled);
        // if (Handled) then
        //  Exit();
        // continue with the rest of handling...
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
        if (trakce.ConnectedSafe()) then
        begin
          PanelServer.Stop();
          try
            trakce.EmergencyStop();
            trakce.Disconnect();
          except

          end;
        end;

        RCSs.EmergencyCloseAll();
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
    if (trakce.ConnectedSafe()) then
    begin
      PanelServer.Stop();
      try
        trakce.EmergencyStop();
        trakce.Disconnect();
      except

      end;
    end;

    RCSs.EmergencyCloseAll();

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
        ExceptionMessageBox('Chyba při aktivaci bloků:', E);
        Exit();
      end;
    end;
  end;

  if ((SystemData.status = starting) and (PanelServer.openned)) then
  begin
    Self.PanelServerStartingStarted();
    Exit();
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
      ExceptionMessageBox('Chyba při zapínání panelServeru:', E);
      Exit();
    end;
  end;

  if (SystemData.status = starting) then
    Self.PanelServerStartingStarted();
end;

procedure TF_Main.PanelServerStartingStarted();
begin
  if (PtServer.autoStart) then
  begin
    Self.A_PT_StartExecute(Self)
  end else begin
    logging.Log('System: start OK', TLogLevel.llInfo, lsSystem, True);
    SystemData.status := null;
    Self.UpdateSystemButtons();
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
  try
    PtServer.Start();
    if (SystemData.Status = TSystemStatus.starting) then
      logging.Log('System: start OK', TLogLevel.llInfo, lsSystem, True);
  except
    on E: Exception do
      ExceptionMessageBox('Nelze nastartovat PT server:', E);
  end;

  if (SystemData.Status = TSystemStatus.starting) then
    SystemData.Status := null;
  Self.UpdateSystemButtons();
end;

procedure TF_Main.A_PT_StopExecute(Sender: TObject);
begin
  try
    PtServer.Stop();
  except
    on E: Exception do
      ExceptionMessageBox('Nelze zastavit PT server:', E);
  end;

  Self.UpdateSystemButtons();
end;


/// /////////////////////////////////////////////////////////////////////////////

procedure TF_Main.A_SaveStavExecute(Sender: TObject);
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
    RVDb.SaveState(Self.E_dataload_RV_state.Text + '_');

    if (FileExists(Self.E_dataload_RV_state.Text)) then
      DeleteFile(Self.E_dataload_RV_state.Text);
    MoveFile(PChar(Self.E_dataload_RV_state.Text + '_'), PChar(Self.E_dataload_RV_state.Text));
    DeleteFile(Self.E_dataload_RV_state.Text + '_');
  except
    on E: Exception do
      AppEvents.LogException(E, 'RVDb.SaveToDir');
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
    Config.SaveIniDataFile();
    GlobalConfig.SaveToFile(Self.E_configFilename.Text); // save funcsVyznam, ShowOnlyActive
  except
    on E: Exception do
      AppEvents.LogException(E, 'Save cfg');
  end;

  if (Sender <> nil) then
    Logging.log('Uložen stav kolejiště', TLogLevel.llInfo, lsData, True);
end;

procedure TF_Main.A_System_StartExecute(Sender: TObject);
begin
  Self.LB_BriefLog.Items.Insert(0, '--------------------------------------------------------------------------------');

  for var i: Integer := 0 to RCSs._RCSS_MAX do
  begin
    if ((RCSs[i].libLoaded) and (not RCSs[i].ready)) then
    begin
      logging.Log('Systém nelze spustit, RCS není připraveno k zapnutí systému', TLogLevel.llError, lsSystem, True);
      StrMessageBox('Systém nelze spustit, RCS'+IntToStr(i)+' není připraveno k zapnutí systému' + #13#10 +
        'Možné příčiny:' + #13#10 + ' - nenačtena platná knihovna', 'Nelze spustit', MB_OK OR MB_ICONWARNING);
      Exit();
    end;
  end;
  if (not RCSs.AnyLibLoaded()) then
  begin
    logging.Log('Systém nelze spustit, nanačtena žádná RCS knihovna', TLogLevel.llError, lsSystem, True);
    StrMessageBox('Systém nelze spustit, nanačtena žádná RCS knihovna', 'Nelze spustit', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  if (not trakce.ready) then
  begin
    logging.Log('Systém nelze spustit, Trakce není připravena k zapnutí systému', TLogLevel.llError, lsSystem, True);
    StrMessageBox('Systém nelze spustit, Trakce není připravena k zapnutí systému' + #13#10 +
      'Možné příčiny:' + #13#10 + ' - nenačtena platná knihovna', 'Nelze spustit', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (AppEvents.lastException <> nil) then
  begin
    logging.Log('Systém nelze spustit, AppEvents hlásí výjimku', TLogLevel.llError, lsSystem, True);
    StrMessageBox('Systém nelze spustit, v minulosti nastala kritická výjimka!', 'Nelze spustit', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  logging.Log('Zapínám systémy...', TLogLevel.llInfo, lsSystem, True);
  SystemData.Status := starting;
  Self.A_System_Start.Enabled := false;

  for var i: Integer := 0 to RCSs._RCSS_MAX do
    if ((RCSs[i].ready) and (SystemData.Status = TSystemStatus.starting)) then // otevreni prvniho RCS muze zpusobit dokonceni inicializace, v takovem pripade neotrvirat dalsi
      Self.MI_RCS_Open_Click(Self.MI_RCSs[i].MI_Open);
end;

procedure TF_Main.A_System_StopExecute(Sender: TObject);
begin
  var response: Integer := StrMessageBox('Opravdu zastavit server a odpojit se od všech systémů?', 'Otázka', MB_YESNO OR MB_ICONQUESTION);
  if (response <> mrYes) then
    Exit();

  Self.A_System_Stop.Enabled := false;

  Self.LB_BriefLog.Items.Insert(0, '--------------------------------------------------------------------------------');
  logging.Log('Vypínám systémy...', TLogLevel.llInfo, lsSystem, True);
  SystemData.Status := stopping;

  logging.Log('Zastavuji všechny vlaky...', TLogLevel.llInfo, lsSystem, True);
  Trains.StopAllTrains();

  Application.ProcessMessages();

  if (PtServer.openned) then
    Self.A_PT_StopExecute(nil);

  logging.Log('Odpojuji panely...', TLogLevel.llInfo, lsSystem, True);
  Areas.DisconnectPanels();
  Self.A_PanelServer_StopExecute(nil);

  JCDb.CancelAll();
  Blocks.Disable();
  Trains.ClearPOdj();
  Blocks.Reset();
  Self.A_SaveStavExecute(Self);
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
  if ((Self.LV_AB.Selected <> nil) and (StrMessageBox('Opravdu smazat jízdní cestu ' + jc.name + '?',
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
        ExceptionMessageBox('Chyba při mazání:', E);
    end;
  end;
end;

procedure TF_Main.B_BlkAddClick(Sender: TObject);
begin
  F_BlkNew.OpenForm();
end;

procedure TF_Main.B_BlkDeleteClick(Sender: TObject);
begin
  var i := LV_Blocks.ItemIndex;

  Beep;
  if (StrMessageBox('Opravdu chcete smazazat blok ' + Blocks.GetBlkIndexName(i) + '?', 'Mazání bloku',
      MB_YESNO OR MB_ICONQUESTION OR MB_DEFBUTTON2) = mrYes) then
  begin
    try
      Blocks.Delete(i);
    except
      on E: Exception do
        ExceptionMessageBox(E);
    end;
  end; // if MesageBox
end;

procedure TF_Main.B_FuncChangeClick(Sender: TObject);
begin
  Self.B_FuncChange.Enabled := False;
  FuncNames.Clear();

  try
    try
      for var line: string in Self.M_funcsVyznam.Lines do
        if (line <> '') then
          FuncNames.Add(line);
    except
      on E:Exception do
      begin
        ExceptionMessageBox('Nepodařilo se přidat!', E);
        Exit();
      end;
    end;

    StrMessageBox('Úspěšně uloženo', 'OK', MB_OK OR MB_ICONINFORMATION);
  finally
    Self.B_FuncChange.Enabled := True;
    PanelServer.BroadcastFuncsDescription();
    Self.OnFuncNameChange(Self);
  end;
end;

procedure TF_Main.B_FuncUpdateClick(Sender: TObject);
begin
  try
    Self.B_FuncUpdate.Enabled := False;
    Self.UpdateFuncMemo();
    StrMessageBox('Úspěšně aktualizováno', 'OK', MB_OK OR MB_ICONINFORMATION);
  except
    on E:Exception do
      ExceptionMessageBox(E);
  end;

  Self.B_FuncUpdate.Enabled := True;
end;

procedure TF_Main.UpdateFuncMemo();
begin
  var all: TList<TPair<string, TRVFuncType>> := FuncNames.All();
  try
    Self.M_funcsVyznam.Clear();
    for var entry in all do
      Self.M_funcsVyznam.Lines.Add(entry.Key + ':' + TRV.RVFuncTypeToChar(entry.Value));
  finally
    all.Free();
  end;
end;

procedure TF_Main.B_ClearStatsClick(Sender: TObject);
begin
  if (StrMessageBox('Opravdu smazat najeté bloky a kilometry všech vozidel?', 'Opravdu?',
    MB_YESNO OR MB_ICONQUESTION OR MB_DEFBUTTON2) = mrYes) then
    RVDb.ClearAllStatistics();
end;

procedure TF_Main.OnFuncNameChange(Sender: TObject);
begin
  var all: TList<TPair<string, TRVFuncType>> := FuncNames.All();
  var strs := TStringList.Create();
  try
    for var entry in all do
      strs.Add(entry.Key);
    F_FuncsSet.UpdateFuncsList(strs);
  finally
    all.Free();
    strs.Free();
  end;

  if ((Self.CHB_LoadChanges.Checked) and (Self.B_FuncChange.Enabled)) then
    Self.UpdateFuncMemo();
end;

procedure TF_Main.B_RVStats_ExportClick(Sender: TObject);
var fn: string;
begin
  if (Self.SD_RV_Stats.Execute(Self.Handle)) then
  begin
    try
      if (RightStr(Self.SD_RV_Stats.FileName, 4) <> '.csv') then
        fn := Self.SD_RV_Stats.FileName + '.csv'
      else
        fn := Self.SD_RV_Stats.FileName;
      RVDb.ExportStatistics(fn);
    except
      on E: Exception do
        ExceptionMessageBox('Nelze exportovat', E);
    end;
  end;
end;

procedure TF_Main.B_RV_AddClick(Sender: TObject);
begin
  F_RVEdit.NewVehicle();
end;

procedure TF_Main.B_RV_DeleteClick(Sender: TObject);
begin
  if (Self.LV_Vehicles.Selected = nil) then
    Exit();

  var vehicles := Self.LVSelectedTexts(Self.LV_Vehicles, 'vozidlo', 'vozidla');
  var response: Integer := StrMessageBox('Opravdu smazat ' + vehicles + '?', '?', MB_YESNO OR MB_ICONQUESTION OR MB_DEFBUTTON2);
  if (response = mrYes) then
  begin
    for var i := Self.LV_Vehicles.Items.Count - 1 downto 0 do
    begin
      var LI: TListItem := Self.LV_Vehicles.Items[i];
      if (LI.Selected) then
      begin
        var addr := StrToInt(LI.Caption);
        try
          RVDb.Remove(addr);
        except
          on E: Exception do
          begin
            ExceptionMessageBox('Mazání vozidla ' + IntToStr(addr) + ' se nezdařilo:', E);
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

procedure TF_Main.B_train_deleteClick(Sender: TObject);
begin
  if (Self.LV_Trains.Selected = nil) then
    Exit();
  if (not Assigned(Trains[Self.LV_Trains.ItemIndex])) then
    Exit();

  var sprs: string := Self.LVSelectedTexts(Self.LV_Trains, 'vlak', 'vlaky');
  var response: Integer := StrMessageBox('Opravdu smazat ' + sprs + '?', '?', MB_YESNO OR MB_ICONQUESTION OR MB_DEFBUTTON2);
  if (response = mrYes) then
  begin
    for var i: Integer := Self.LV_Trains.Items.Count - 1 downto 0 do
    begin
      var LI: TListItem := Self.LV_Trains.Items[i];
      if ((LI.Selected) and (LI.Caption <> '')) then
        Trains.Remove(LI.Index);
    end;
  end;
end;

procedure TF_Main.B_mJC_AddClick(Sender: TObject);
begin
  if ((Self.LV_MultiJC.Selected <> nil) and
    (StrMessageBox('Chcete použít složenou JC ' + MultiJCDb[Self.LV_MultiJC.ItemIndex].name +
    ' jako šablonu pro vytvoření nové složené JC?', 'Nová složená JC', MB_YESNO OR MB_ICONQUESTION OR MB_DEFBUTTON1)
    = mrYes)) then
    F_MJCEdit.NewMJC(MultiJCDb[Self.LV_MultiJC.ItemIndex])
  else
    F_MJCEdit.NewMJC(nil);
end;

procedure TF_Main.B_mJC_RemoveClick(Sender: TObject);
begin
  if (Self.LV_MultiJC.Selected = nil) then
    Exit();

  var mjcs: string := Self.LVSelectedTexts(Self.LV_MultiJC, 'cestu', 'cesty');
  var response: Integer := StrMessageBox('Opravdu smazat ' + mjcs + '?', '?', MB_YESNO OR MB_ICONQUESTION OR MB_DEFBUTTON2);
  if (response = mrYes) then
  begin
    for var i: Integer := Self.LV_MultiJC.Items.Count - 1 downto 0 do
    begin
      var LI: TListItem := Self.LV_MultiJC.Items[i];
      if (LI.Selected) then
        MultiJCDb.Remove(LI.Index);
    end;
  end;
end;

procedure TF_Main.B_RemoveStackClick(Sender: TObject);
var area: TArea;
begin
  if (Self.LV_Areas.Selected = nil) then
    Exit();
  area := Areas[Self.LV_Areas.ItemIndex];
  if (StrMessageBox('Opravdu smazat zásobník jízdních cest dopravny ' + Area.name + ' ?', 'Opravdu?',
    MB_YESNO OR MB_ICONQUESTION) = mrYes) then
    Area.stack.Clear();
end;

procedure TF_Main.B_User_AddClick(Sender: TObject);
begin
  F_UserEdit.NewUser();
end;

procedure TF_Main.B_User_DeleteClick(Sender: TObject);
begin
  var response: Integer := StrMessageBox('Opravdu smazat uživatele ' + Self.LV_Users.Selected.SubItems[0] + ' ?', 'Opravdu?',
    MB_YESNO OR MB_ICONQUESTION OR MB_DEFBUTTON2);
  if (response = mrYes) then
  begin
    UsrDB.RemoveUser(Self.LV_Users.ItemIndex);
    Self.B_User_Delete.Enabled := false;
  end;
end;

procedure TF_Main.B_JC_AddClick(Sender: TObject);
begin
  if ((Self.LV_JC.Selected <> nil) and
    (StrMessageBox('Chcete použít JC ' + JCDb[Self.LV_JC.ItemIndex].name +
    ' jako šablonu pro vytvoření nové JC?', 'Nová JC', MB_YESNO OR MB_ICONQUESTION OR MB_DEFBUTTON1) = mrYes)) then
    F_JCEdit.NewJC(Self.LV_JC.ItemIndex)
  else
    F_JCEdit.NewJC(-1);
end;

procedure TF_Main.B_JC_deleteClick(Sender: TObject);
begin
  if (Self.LV_JC.Selected = nil) then
    Exit();

  var jcs: string := Self.LVSelectedTexts(Self.LV_JC, 'cestu', 'cesty');
  var response: Integer := StrMessageBox('Opravdu smazat ' + jcs + '?', '?', MB_YESNO OR MB_ICONQUESTION OR MB_DEFBUTTON2);
  if (response = mrYes) then
  begin
    for var i: Integer := Self.LV_JC.Items.Count - 1 downto 0 do
    begin
      var LI: TListItem := Self.LV_JC.Items[i];
      if (LI.Selected) then
      begin
        try
          var jc: TJC := JCDb.GetJCByIndex(LI.Index);
          if (ABlist.Contains(jc)) then
            ABlist.Remove(jc);
          JCDb.Remove(LI.Index);
        except
          on E: Exception do
          begin
            ExceptionMessageBox('Nelze smazat JC', E);
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
  pozice := LV_Boosters.ItemIndex;
  Beep;
  if (StrMessageBox('Opravdu chcete smazat zesilovač ' + Boosters.sorted[pozice].name + '?',
    'Mazání zesilovace', MB_YESNO OR MB_ICONQUESTION OR MB_DEFBUTTON2) = mrYes) then
  begin
    Boosters.Remove(Boosters.sorted[pozice].id);
    LV_Boosters.Items.Delete(pozice);
  end; // if MessageBox
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_Main.PM_Help_RPClick(Sender: TObject);
begin
  F_About.ShowModal;
end;

procedure TF_Main.PM_RVPopup(Sender: TObject);
var i: Integer;
begin
  if (Self.LV_Vehicles.Selected = nil) then
  begin
    for i := 0 to (Sender as TPopupMenu).Items.Count - 1 do
      (Sender as TPopupMenu).Items.Items[i].Enabled := false;
  end else begin
    for i := 0 to (Sender as TPopupMenu).Items.Count - 1 do
      (Sender as TPopupMenu).Items.Items[i].Enabled := true;
  end;
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
      if (Self.PC_1.ActivePage = Self.TS_Trains) then
        TrainTableData.UpdateTable();
      if (Self.PC_1.ActivePage = Self.TS_Zesilovace) then
        ZesTableData.UpdateTable();
      if (Self.PC_1.ActivePage = Self.TS_RV) then
        RVTableData.UpdateTable();
      if (Self.PC_1.ActivePage = Self.TS_VC) then
        JCTableData.UpdateTable();
      if (Self.PC_1.ActivePage = Self.TS_MultiJC) then
        MultiJCTableData.UpdateTable();
      if (Self.PC_1.ActivePage = Self.TS_Areas) then
        ORsTableData.UpdateTable();
      if (Self.PC_1.ActivePage = Self.TS_Technologie) then
        PanelServer.GUIRefreshFromQueue();

      ABTableData.Update();
    end;

    RVDb.UpdateTokenTimeout();
    Config.UpdateAutosave();
    Self.SB1LogUpdate();
  except
    on E: Exception do
    begin
      if (not log_last_error) then
        AppEvents.LogException(E, 'Function timer exception');
    end;
  end;
end;

procedure TF_Main.SB1Log(msg: string);
begin
  Self.SB1.Panels.Items[_SB_LOG].Text := msg;
  Self.mSb1LogHideTime := Now + EncodeTime(0, 0, _SB_LOG_SHOW_TIME_MS div 1000, _SB_LOG_SHOW_TIME_MS mod 1000);
  Self.mSb1Log := True;
end;

procedure TF_Main.SB1LogUpdate();
begin
  if ((Self.mSb1Log) and (Now > Self.mSb1LogHideTime)) then
  begin
    Self.SB1.Panels.Items[_SB_LOG].Text := '';
    Self.mSb1Log := False;
  end;
end;

procedure TF_Main.CHB_log_authClick(Sender: TObject);
begin
  Logging.auth_logging := Self.CHB_Log_Auth.Checked;
end;

procedure TF_Main.CHB_RCS0_LogClick(Sender: TObject);
begin
  var rcsi: Integer := TCheckBox(Sender).Tag;
  RCSs[rcsi].logEnabled := TCheckBox(Sender).Checked;
end;

procedure TF_Main.CHB_RCS0_Show_Only_ActiveClick(Sender: TObject);
begin
  var rcsi: Integer := TCheckBox(Sender).Tag;
  if ((rcsi >= 0) and (rcsi < RCSs._RCSS_COUNT) and (RCSs[rcsi].Lib <> '')) then
    RCSTableData[rcsi].LoadToTable(not Self.CHB_RCSs_Show_Only_Active[rcsi].Checked);
end;

procedure TF_Main.CloseForm();
begin
  logging.Log('########## Probíhá ukončování hJOPserver ##########', TLogLevel.llInfo);

  Self.T_Main.Enabled := false;
  Self.T_GUI_refresh.Enabled := false;
  JCSimulator.timer.Enabled := false;
  RailwaySimulator.timer.Enabled := false;
  TurnoutSimulator.timer.Enabled := false;

  Self.A_SaveStavExecute(Self);

  logging.Log('###############################################', TLogLevel.llInfo);
end;

procedure TF_Main.RepaintObjects();
begin
  var allButFirstWidth: Integer := 0;
  for var i: Integer := 1 to Self.SB1.Panels.Count-1 do
    allButFirstWidth := allButFirstWidth + Self.SB1.Panels[i].Width;

  Self.SB1.Panels.Items[0].Width := Self.ClientWidth - allButFirstWidth;
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
  StrMessageBox('Datum a čas lze nastavit v operačním systému', 'Informace', MB_ICONINFORMATION OR MB_OK);
end;

procedure TF_Main.MI_Block_DeleteClick(Sender: TObject);
begin
  if (Self.B_BlkDelete.Enabled) then
    Self.B_BlkDeleteClick(Self);
end;

procedure TF_Main.MI_DisconnectClick(Sender: TObject);
begin
  if (PanelServer.GetClient(Self.LV_Clients.ItemIndex) <> nil) then
  begin
    try
      PanelServer.DisconnectClient(PanelServer.GetClient(Self.LV_Clients.ItemIndex).connection);
    except
      on E: Exception do
        ExceptionMessageBox('Výjimka při odpojování', E);
    end;
  end;
end;

procedure TF_Main.MI_ExitAppClick(Sender: TObject);
begin
  Self.Close();
end;

procedure TF_Main.MI_Block_HoukClick(Sender: TObject);
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

procedure TF_Main.MI_Block_EditClick(Sender: TObject);
begin
  if (Self.LV_Blocks.Selected <> nil) then
    Self.LV_BlocksDblClick(Self.LV_Blocks);
end;

procedure TF_Main.MI_RCSs_UpdateClick(Sender: TObject);
begin
  try
    Self.UpdateRCSLibsList();
    StrMessageBox('Seznam knihoven úspěšně aktualizován.', 'Informace', MB_OK OR MB_ICONINFORMATION);
  except
    on E: Exception do
      ExceptionMessageBox('Seznam knihoven se nepodařilo aktualizovat:', E);
  end;
end;

procedure TF_Main.MI_SaveVehiclesClick(Sender: TObject);
begin
  var response := StrMessageBox('Ukládání vozidel probíhá automaticky, manuální ukládání je potřeba pouze ve výjimečných situacích. Pokračovat?',
    'Otázka', MB_YESNO OR MB_ICONQUESTION OR MB_DEFBUTTON2);
  if (response <> mrYes) then
    Exit();

  Screen.Cursor := crHourGlass;
  try
    RVDb.SaveData(Self.E_dataload_RV_dir.Text);
    RVDb.SaveState(Self.E_dataload_RV_state.Text);
  except
    on e: Exception do
      AppEvents.LogException(e);
  end;
  Logging.log('Uložena všechna vozidla', TLogLevel.llInfo, lsData, True);
  Screen.Cursor := crDefault;
end;

procedure TF_Main.MI_Save_configClick(Sender: TObject);
begin
  Application.ProcessMessages();
  Screen.Cursor := crHourGlass;

  try
    Config.CompleteSaveToFile();
  except
    on E: Exception do
    begin
      AppEvents.LogException(E, 'TF_Main.MI_Save_configClick');
      ExceptionMessageBox('Výjimka: ', E);
    end;
  end;

  Screen.Cursor := crDefault;
end;

procedure TF_Main.MI_SimulationDiagnosticsClick(Sender: TObject);
begin
  F_Admin.Show();
end;

procedure TF_Main.MI_Block_StateClick(Sender: TObject);
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

procedure TF_Main.LB_BriefLogDblClick(Sender: TObject);
begin
  if (StrMessageBox('Smazat obsah stručného logu?', 'Smazat?', MB_YESNO OR MB_ICONQUESTION OR MB_DEFBUTTON2) = mrYes) then
    Self.LB_BriefLog.Clear();
end;

procedure TF_Main.LB_BriefLogDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
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
    for var i: Integer := 0 to RCSs._RCSS_MAX do
    begin
      var log: Boolean := inidata.ReadBool(_INIDATA_PATHS_LOG_SECTION, 'rcs'+IntToStr(i), false);
      Self.CHB_RCSs_Log[i].Checked := log;
      RCSs[i].logEnabled := log;
    end;
    Self.CHB_log_auth.Checked := inidata.ReadBool(_INIDATA_PATHS_LOG_SECTION, 'auth', false);

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
    Log('Probiha automaticke pripojovani k systemum - t=6s', TLogLevel.llInfo);
    F_AutoStartSystems.Show();
    Self.autostart.goTime := Now + EncodeTime(0, 0, 6, 0);
    Self.autostart.state := asWaiting;
  end else if (Self.autostart.state = asWaiting) then begin           
    if (Round((Now - Self.autostart.goTime) * 24 * 3600) = 0) then
    begin
      Log('Automaticke pripojovani k systemum - t=0 - zapinam systemy', TLogLevel.llInfo);
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

  if (not Self.CloseMessage) then
  begin
    Self.Close();
    Exit();
  end;

  F_splash.PB_Prubeh.Position := F_splash.PB_Prubeh.Max;
  F_splash.AddStav('Téměř spuštěno...');

  BlocksTablePainter.LoadTable();
  JCTableData.LoadToTable();
  for var rcsi: Integer := 0 to RCSs._RCSS_MAX do
    RCSTableData[rcsi].LoadToTable(not Self.CHB_RCSs_Show_Only_Active[rcsi].Checked);
  UsersTableData.LoadToTable();
  ORsTableData.LoadToTable();

  Self.PC_1.ActivePage := TS_Technologie;

  PanelServer.GUIInitTable();
  modelTime.UpdateGUIColors();
  Self.FillGlobalConfig();

  // RCS
  Self.RCSInit();

  // Trakce
  Self.UpdateTrkLibsList();

  if (trakce.Lib = '') then
    Self.SB1.Panels.Items[_SB_TRAKCE_LIB].Text := '-'
  else
    Self.SB1.Panels.Items[_SB_TRAKCE_LIB].Text := ExtractFileName(trakce.Lib);

  // PT Server
  Self.S_PTServer.Visible := (PtServer.autoStart);
  Self.L_PTServer.Visible := (PtServer.autoStart);

  // --- FINISHED ---
  Self.Visible := true;

  Self.T_Main.Enabled := true;
  Self.T_GUI_refresh.Enabled := true;

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
    Logging.log('Uloženy pozice oken', TLogLevel.llInfo, lsData, True);
  except
    on E: Exception do
      ExceptionMessageBox('Výjimka:', E);
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
  Self.B_JC_delete.Enabled := (LV_JC.Selected <> nil);

  if (LV_JC.Selected <> nil) then
    Self.B_JC_Reset.Enabled := JCDb.GetJCByIndex(LV_JC.ItemIndex).activating
  else
    Self.B_JC_Reset.Enabled := false;
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

procedure TF_Main.LV_JCKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((Key = VK_DELETE) and (Self.B_JC_delete.Enabled)) then
    Self.B_JC_deleteClick(Self);
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
  var response: Integer := StrMessageBox('Opravdu smazat tabulku logu?', '?', MB_YESNO OR MB_ICONQUESTION OR MB_DEFBUTTON2);
  if (response = mrYes) then
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
    if (Item.SubItems[1] <> '0') then
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

procedure TF_Main.LV_MultiJCKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((Key = VK_DELETE) and (Self.B_mJC_Remove.Enabled)) then
    Self.B_mJC_RemoveClick(Self);
end;

procedure TF_Main.LV_MultiJCKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) then
    Self.LV_MultiJCDblClick(LV_Blocks);
end;

procedure TF_Main.LV_TrainsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  Self.B_train_delete.Enabled := (Self.TrainsSelectedCount() > 0);

  if (Self.TrainsSelectedCount() > 1) then
    Self.B_train_delete.Caption := 'Smazat vlaky'
  else
    Self.B_train_delete.Caption := 'Smazat vlak';
end;

procedure TF_Main.LV_TrainsCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if ((Item.Index >= Trains.count) or (Trains[Item.Index] = nil)) then
    Exit();

  if (Trains[Item.Index].emergencyStopped) then
    (Sender as TCustomListView).Canvas.Brush.Color := _TABLE_COLOR_RED
  else if (Trains[Item.Index].IsSpeedOverride) then
    (Sender as TCustomListView).Canvas.Brush.Color := _TABLE_COLOR_YELLOW;
end;

procedure TF_Main.LV_TrainsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((Key = VK_DELETE) and (Self.B_train_delete.Enabled)) then
    Self.B_train_deleteClick(Self);
end;

procedure TF_Main.LV_AreasChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  if (Self.LV_Areas.Selected <> nil) then
  begin
    var area := Areas[Self.LV_Areas.ItemIndex];
    Self.B_RemoveStack.Enabled := (area.stack.Count > 0);
  end else begin
    Self.B_RemoveStack.Enabled := false;
  end;
end;

procedure TF_Main.LV_RCS0_StateCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  var DefaultDraw: Boolean);
begin
  if ((Item.SubItems.Count > 5) and ((Item.SubItems[5] = 'Fail') or (Item.SubItems[5] = 'Error'))) then
    Sender.Canvas.Brush.Color := _TABLE_COLOR_RED
  else if ((Item.SubItems.Count > 5) and (Item.SubItems[5] = 'Warning')) then
    Sender.Canvas.Brush.Color := _TABLE_COLOR_YELLOW
  else
    Sender.Canvas.Brush.Color := _TABLE_COLOR_WHITE;
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

procedure TF_Main.LV_UsersKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((Key = VK_DELETE) and (Self.B_User_Delete.Enabled)) then
    Self.B_User_DeleteClick(Self);
end;

procedure TF_Main.LV_UsersKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) then
    Self.LV_UsersDblClick(LV_Blocks);
end;

procedure TF_Main.LV_BoostersChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  B_zes_delete.Enabled := LV_Boosters.Selected <> nil;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_Main.LogBrief(str: string; level: TLogLevel);
begin
  if (not Assigned(Self.LB_BriefLog)) then
    Exit();

  if (Self.LB_BriefLog.Items.Count > 100) then
    Self.LB_BriefLog.Clear();

  var prefix := '';
  case (level) of
    TLogLevel.llError: prefix := 'ERR: ';
    TLogLevel.llWarning: prefix := 'WARN: ';
  end;

  Self.LB_BriefLog.Items.Insert(0, FormatDateTime('hh:nn:ss', Now) + ' : ' + prefix + str);
end;

procedure TF_Main.LV_ABChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  Self.B_AB_Delete.Enabled := (Self.LV_AB.Selected <> nil);
end;

procedure TF_Main.LV_ABKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((Key = VK_DELETE) and (Self.B_AB_Delete.Enabled)) then
    Self.B_AB_DeleteClick(Self);
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
      ExceptionMessageBox(E);
  end;
end;

procedure TF_Main.LV_BlocksKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((Key = VK_DELETE) and (Self.B_BlkDelete.Enabled)) then
    Self.B_BlkDeleteClick(Self);
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

procedure TF_Main.LV_VehiclesChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  Self.B_RV_Delete.Enabled := (LV_Vehicles.Selected <> nil) and (not trakce.ConnectedSafe());
end;

procedure TF_Main.LV_VehiclesCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
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

procedure TF_Main.LV_VehiclesDblClick(Sender: TObject);
begin
  if (LV_Vehicles.Selected = nil) then
    Exit();

  if (trakce.ConnectedSafe()) then
  begin
    try
      RegCollector.Open(RVDb[StrToInt(Self.LV_Vehicles.Selected.Caption)]);
    except
      on E: Exception do
        ExceptionMessageBox(E);
    end;
  end else begin
    F_RVEdit.EditVehicle(RVDb[StrToInt(LV_Vehicles.Selected.Caption)]);
  end;
end;

procedure TF_Main.LV_VehiclesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((Key = VK_DELETE) and (Self.B_RV_Delete.Enabled)) then
    Self.B_RV_DeleteClick(Self);
end;

procedure TF_Main.LV_VehiclesKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) then
    Self.LV_VehiclesDblClick(LV_Blocks);
end;

procedure TF_Main.LV_BoostersCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  var DefaultDraw: Boolean);
begin
  if (not Boosters.sorted[Item.Index].rcsPresent) then
  begin
    LV_Boosters.Canvas.Brush.Color := _TABLE_COLOR_GRAY;
  end else begin
    if (Boosters.sorted[Item.Index].power = TBoosterSignal.ok) then
    begin
      if (Boosters.sorted[Item.Index].overload = TBoosterSignal.ok) then
      begin
        LV_Boosters.Canvas.Brush.Color := _TABLE_COLOR_GREEN;
      end else begin
        LV_Boosters.Canvas.Brush.Color := _TABLE_COLOR_RED;
      end;
    end else begin
      LV_Boosters.Canvas.Brush.Color := _TABLE_COLOR_BLUE;
    end;
  end; // if not Zarizeni.Start
end;

procedure TF_Main.LV_BoostersDblClick(Sender: TObject);
begin
  if (LV_Boosters.Selected <> nil) then
    F_Booster_Edit.EditBooster(Boosters.sorted[LV_Boosters.ItemIndex]);
end;

procedure TF_Main.LV_BoostersKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((Key = VK_DELETE) and (Self.B_zes_delete.Enabled)) then
    Self.B_zes_deleteClick(Self);
end;

procedure TF_Main.LV_BoostersKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) then
    Self.LV_BoostersDblClick(LV_Blocks);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_Main.DisableRemoveButtons();
begin
  Self.B_BlkDelete.Enabled := false;
  Self.B_RV_Delete.Enabled := false;
  Self.B_train_delete.Enabled := false;
  Self.B_zes_delete.Enabled := false;
  Self.B_User_Delete.Enabled := false;
  Self.B_JC_delete.Enabled := false;
  Self.B_JC_Reset.Enabled := false;
  Self.B_RemoveStack.Enabled := false;
  Self.B_mJC_Remove.Enabled := false;
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
  Self.A_System_Start.Enabled := ((not RCSs.AllActiveRCSsStateGTE(rsStartedNotScanned)) or (not trakce.ConnectedSafe()) or (Self.A_Locos_Acquire.Enabled)
    or (not PanelServer.openned) or (not Blocks.Enabled) or ((PtServer.autoStart) and (not PtServer.openned))) and (not RCSs.IsStateActionInProgress());
  Self.A_System_Stop.Enabled := (((RCSs.AnyRCSStateGTE(rsOpenStopped)) or (trakce.ConnectedSafe()) or (PanelServer.openned) or
    (PtServer.openned)) and (not RCSs.IsStateActionInProgress()));
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

function TF_Main.TrainsSelectedCount(): Integer;
begin
  Result := 0;
  for var LI: TListItem in Self.LV_Trains.Items do
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
  Self.NB_TimeJCZav.Text := TimeToSecTenths(GlobalConfig.times.jcReleaseZaver);

  Self.SE_jcMaxMovingTurnouts.Value := GlobalConfig.jcMaxMovingTurnouts;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_Main.B_ConfigApplyClick(Sender: TObject);
begin
  try
    try
      GlobalConfig.scale := StrToInt(Self.E_Scale.Text);
    except
      on E: Exception do
      begin
        ExceptionMessageBox('Nepodařilo se načíst měřítko:', E);
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
        ErrorMessageBox('Nepodařilo se načíst čas automatického uložení');
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
    GlobalConfig.times.jcReleaseZaver := SecTenthsToTime(Self.NB_TimeJCZav.Text);

    GlobalConfig.jcMaxMovingTurnouts := Self.SE_jcMaxMovingTurnouts.Value;
  except
    on E: Exception do
    begin
      ExceptionMessageBox(E);
      Exit();
    end;
  end;
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
