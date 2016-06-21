unit fMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Outputdriver, ExtCtrls, StdCtrls, Menus, ImgList, Buttons, ComCtrls,
  inifiles, ActnList, AppEvnts, Mask, ScktComp, ToolWin, adCpuUsage,
  ExtDlgs,  Gauges, Registry, StrUtils, fLicence, mmsystem, Grids, Spin, ValEdit,
  DateUtils, ShellApi, ActiveX, ShlObj, ComObj, RPConst,
  TrakceGUI, Trakce, BoosterDb, CPort;

type

  TF_Main = class(TForm)
    Timer1: TTimer;
    Menu_1: TMainMenu;
    M_Zarizeni: TMenuItem;
    PM_go: TMenuItem;
    PM_stop: TMenuItem;
    PM_OptionsMTB: TMenuItem;
    M_Provoz: TMenuItem;
    PM_Nastaveni: TMenuItem;
    M_Reset: TMenuItem;
    PM_ResetV: TMenuItem;
    SB1: TStatusBar;
    N1: TMenuItem;
    M_Dalsi: TMenuItem;
    PM_Tester: TMenuItem;
    PM_TI: TPopupMenu;
    PM_Open: TMenuItem;
    PM_close: TMenuItem;
    N3: TMenuItem;
    PM_icon_close: TMenuItem;
    M_Help: TMenuItem;
    PM_Help_RP: TMenuItem;
    M_Centrala: TMenuItem;
    PM_Int_run: TMenuItem;
    PM_Int_Stop: TMenuItem;
    PM_Int_Disconnect: TMenuItem;
    N4: TMenuItem;
    PM_Int_connect: TMenuItem;
    M_Zobrazeni: TMenuItem;
    PM_SB1: TMenuItem;
    T_function: TTimer;
    M_System: TMenuItem;
    PM_Central_Start: TMenuItem;
    PM_Central_Stop: TMenuItem;
    N5: TMenuItem;
    T_konflikty: TTimer;
    PM_system_reset: TMenuItem;
    P_Pozadi: TPanel;
    P_Date: TPanel;
    P_Time: TPanel;
    P_Time_modelovy: TPanel;
    P_Zrychleni: TPanel;
    PM_MTBDriver_About: TMenuItem;
    IL_Menu: TImageList;
    SPD_Save: TSavePictureDialog;
    PM_AllLokPrevzit: TMenuItem;
    PM_AllLokOdpojit: TMenuItem;
    P_DCC: TPanel;
    SB_Loconet_Start: TSpeedButton;
    SB_Loconet_Stop: TSpeedButton;
    P_SystemSet: TPanel;
    SB_SystemStart: TSpeedButton;
    SB_SystemStop: TSpeedButton;
    MI_Libs: TMenuItem;
    PM_lib_Simulator: TMenuItem;
    PM_lib_MTB: TMenuItem;
    N2: TMenuItem;
    PM_SaveFormPos: TMenuItem;
    IL_Bloky: TImageList;
    IL_MTB: TImageList;
    N7: TMenuItem;
    PM_Console: TMenuItem;
    AL_Main: TActionList;
    A_MTB_Go: TAction;
    A_MTB_Stop: TAction;
    A_lib_cfg: TAction;
    A_DCC_Go: TAction;
    A_DCC_Stop: TAction;
    A_System_Start: TAction;
    A_System_Stop: TAction;
    A_Trk_Connect: TAction;
    A_Trk_Disconnect: TAction;
    A_All_Loko_Prevzit: TAction;
    A_All_Loko_Odhlasit: TAction;
    MI_PanelServer: TMenuItem;
    A_PanelServer_Start: TAction;
    A_PanelServer_Stop: TAction;
    Start1: TMenuItem;
    Stop1: TMenuItem;
    A_MTB_Open: TAction;
    A_MTB_Close: TAction;
    OtevtMTB1: TMenuItem;
    ZavtMTB1: TMenuItem;
    N8: TMenuItem;
    PC_1: TPageControl;
    TS_Technologie: TTabSheet;
    TS_Bloky: TTabSheet;
    LV_Bloky: TListView;
    P_BlkPozadi: TPanel;
    P_BlkTlc: TPanel;
    B_BlkAdd: TButton;
    B_BlkDelete: TButton;
    P_Blk_Ostatni: TPanel;
    L_BlkPocet: TLabel;
    P_Blk_Dataload: TPanel;
    E_dataload_block: TEdit;
    TS_HV: TTabSheet;
    LV_HV: TListView;
    P_HV_Pozadi: TPanel;
    P_HV_Tlac: TPanel;
    B_HV_Add: TButton;
    B_HV_Delete: TButton;
    P_HV_Dataload: TPanel;
    E_dataload_HV: TEdit;
    TS_Soupravy: TTabSheet;
    LV_Soupravy: TListView;
    P_Soupravy_pozadi: TPanel;
    P_Soupravy_Tlc: TPanel;
    B_lok_delete: TButton;
    P_Spr_Dataload: TPanel;
    E_dataload_soupr: TEdit;
    TS_Stanice: TTabSheet;
    LV_Stanice: TListView;
    P_Stanice_Pozadi: TPanel;
    P_St_Dataload: TPanel;
    E_dataload_spnl: TEdit;
    TS_Zesilovace: TTabSheet;
    LV_Zesilovace: TListView;
    P_zes_pozadi: TPanel;
    P_Zes_Tlc: TPanel;
    B_zes_add: TButton;
    B_zes_delete: TButton;
    P_Zes_Vysvetlivky: TPanel;
    L_Zes_Napajeni: TLabel;
    L_Zes_OK: TLabel;
    L_Zes_NapajeniL_Zes_Zkrat: TLabel;
    L_Zes_Nedetekovano: TLabel;
    P_Zes_Dataload: TPanel;
    E_dataload_zes: TEdit;
    TS_Aut_Rezimy: TTabSheet;
    LV_AC_Db: TListView;
    Panel1: TPanel;
    P_AC_Dataload: TPanel;
    E_dataload_AutRez: TEdit;
    TS_Users: TTabSheet;
    LV_Users: TListView;
    P_Users_pozadi: TPanel;
    P_Users_Tlc: TPanel;
    B_User_Add: TButton;
    B_User_Delete: TButton;
    P_Users_Dataload: TPanel;
    E_Dataload_Users: TEdit;
    TS_Stav_MTB: TTabSheet;
    LV_Stav_MTB: TListView;
    TS_VC: TTabSheet;
    P_VC_Pozadi: TPanel;
    P_VC_Dataload: TPanel;
    E_Dataload_JC: TEdit;
    LV_JC: TListView;
    TS_log: TTabSheet;
    LV_log: TListView;
    TS_Intellibox: TTabSheet;
    LV_log_lnet: TListView;
    GB_Connected_Panels: TGroupBox;
    LV_Clients: TListView;
    GB_technologie_3: TGroupBox;
    S_MTB_open: TShape;
    S_MTB_start: TShape;
    S_Intellibox_connect: TShape;
    S_Intellibox_go: TShape;
    S_Server: TShape;
    L_StavS_1: TLabel;
    L_StavS_2: TLabel;
    L_StavS_3: TLabel;
    L_StavS_4: TLabel;
    L_StavS_6: TLabel;
    GroupBox1: TGroupBox;
    LB_Log: TListBox;
    MI_File: TMenuItem;
    MI_Save_config: TMenuItem;
    S_lok_prevzato: TShape;
    Label1: TLabel;
    PM_HV: TPopupMenu;
    PM_Properties: TMenuItem;
    PM_Regulator: TMenuItem;
    N6: TMenuItem;
    Panel2: TPanel;
    CHB_Mainlog_File: TCheckBox;
    CHB_mainlog_table: TCheckBox;
    Panel3: TPanel;
    CHB_centrala_table: TCheckBox;
    CHB_centrala_file: TCheckBox;
    Label2: TLabel;
    CB_centrala_loglevel: TComboBox;
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
    Panel7: TPanel;
    E_Dataload_multiJC: TEdit;
    LV_MultiJC: TListView;
    GB_Centrala: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    L_CS_FW: TLabel;
    L_CS_ID: TLabel;
    B_CS_Ver_Update: TButton;
    Label5: TLabel;
    L_CS_LI_FW: TLabel;
    Label6: TLabel;
    L_CS_UpdateTime: TLabel;
    B_mJC_Add: TButton;
    B_mJC_Remove: TButton;
    LV_AC_Kroky: TListView;
    B_AutRezim_add: TButton;
    B_AutRezim_delete: TButton;
    SB_AC_Play: TSpeedButton;
    SB_AC_Stop: TSpeedButton;
    SB_AC_Pause: TSpeedButton;
    SB_AC_Repeat: TSpeedButton;
    PM_Clients: TPopupMenu;
    MI_Disconnect: TMenuItem;
    G_Loko_Prevzato: TGauge;
    N10: TMenuItem;
    PM_FuncsSet: TMenuItem;
    A_FuncsSet: TAction;
    TS_FuncsVyznam: TTabSheet;
    M_funcsVyznam: TMemo;
    P_funcsVyznamBg: TPanel;
    B_Change: TButton;
    CHB_LoadChanges: TCheckBox;
    AE_Main: TApplicationEvents;
    P_HV_Stats: TPanel;
    B_HVStats_Clear: TButton;
    B_HVStats_Export: TButton;
    SD_HV_Stats: TSaveDialog;
    procedure Timer1Timer(Sender: TObject);
    procedure PM_NastaveniClick(Sender: TObject);
    procedure PM_ResetVClick(Sender: TObject);
    procedure PM_lib_MTBClick(Sender: TObject);
    procedure PM_lib_SimClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure AE_1Message(var Msg: tagMSG;
      var Handled: Boolean);
    procedure PM_TesterClick(Sender: TObject);
    procedure PM_Help_RPClick(Sender: TObject);
    procedure PM_SB1Click(Sender: TObject);
    procedure T_functionTimer(Sender: TObject);
    procedure T_konfliktyTimer(Sender: TObject);
    procedure PM_system_resetClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure L_DateDblClick(Sender: TObject);
    procedure PM_MTBDriver_AboutClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PM_SaveFormPosClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure PM_ConsoleClick(Sender: TObject);
    procedure A_MTB_GoExecute(Sender: TObject);
    procedure A_MTB_StopExecute(Sender: TObject);
    procedure A_lib_cfgExecute(Sender: TObject);
    procedure A_DCC_GoExecute(Sender: TObject);
    procedure A_DCC_StopExecute(Sender: TObject);
    procedure A_System_StartExecute(Sender: TObject);
    procedure A_System_StopExecute(Sender: TObject);
    procedure A_Trk_ConnectExecute(Sender: TObject);
    procedure A_Trk_DisconnectExecute(Sender: TObject);
    procedure A_All_Loko_PrevzitExecute(Sender: TObject);
    procedure A_All_Loko_OdhlasitExecute(Sender: TObject);
    procedure A_PanelServer_StartExecute(Sender: TObject);
    procedure A_PanelServer_StopExecute(Sender: TObject);
    procedure A_MTB_OpenExecute(Sender: TObject);
    procedure A_MTB_CloseExecute(Sender: TObject);
    procedure LV_ClientsCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure PC_1Change(Sender: TObject);
    procedure LV_AC_DbChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure B_AutRezim_deleteClick(Sender: TObject);
    procedure B_AutRezim_addClick(Sender: TObject);
    procedure LV_AC_DbDblClick(Sender: TObject);
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
    procedure CB_centrala_loglevelChange(Sender: TObject);
    procedure CHB_centrala_tableClick(Sender: TObject);
    procedure CHB_centrala_fileClick(Sender: TObject);
    procedure LV_SoupravyDblClick(Sender: TObject);
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
    procedure B_CS_Ver_UpdateClick(Sender: TObject);
    procedure LV_MultiJCChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure B_mJC_AddClick(Sender: TObject);
    procedure B_mJC_RemoveClick(Sender: TObject);
    procedure LV_MultiJCDblClick(Sender: TObject);
    procedure LV_AC_DbCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure SB_AC_PlayClick(Sender: TObject);
    procedure SB_AC_StopClick(Sender: TObject);
    procedure SB_AC_PauseClick(Sender: TObject);
    procedure SB_AC_RepeatClick(Sender: TObject);
    procedure LV_AC_KrokyCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
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
  private
    KomunikaceGo:TdateTime;
    call_method:TNotifyEvent;

    procedure UpdateCallMethod();
    procedure LoadACKroky();
    procedure OnFuncsVyznamChange(Sender:TObject);

    procedure WMPowerBroadcast(var Msg: TMessage); message WM_POWERBROADCAST;
    procedure WMQueryEndSession(var Msg: TWMQueryEndSession); message WM_QUERYENDSESSION;
    procedure WMEndSession(var Msg: TWMEndSession); message WM_ENDSESSION;

  public
    KomunikacePocitani:Shortint;

    procedure CreateSystem;                                                     // inicializace SW
    procedure CreateClasses;                                                    // vytvoreni objektu
    procedure FreeVars;                                                         // zniceni objektu
    procedure SetStartVars;                                                     // inicializace promennych

    procedure CloseForm;                                                        // ukonceni aplikace
    procedure RepaintObjects;                                                   // prekresleni objektu podle rozmeru okna
    procedure LoadIniLibData;                                                   // nacteni ini_lib dat
    procedure DetekujAutSpusteniSystemu;                                        // detekuje aut. spusteni systemu po zapnuti programu
    procedure OnStart;                                                          // spusti se pri startu SW
    procedure SaveFormPosition;                                                 // ulozi pozici a stav F_Main

    procedure VypisDatumCas;                                                    // aktualizuje datum a cas ve F_Main

    procedure LogStatus(str:string);                                            // vypise info do LB_Log
    procedure DisableRemoveButtons();                                           // znemozni pouziti mazacich tlacitek, typicky se vola po startu systemu

    // MTB events:
    procedure OnMTBStart(Sender:TObject);
    procedure OnMTBStop(Sender:TObject);
    procedure OnMTBOpen(Sender:TObject);
    procedure OnMTBClose(Sender:TObject);
    procedure OnMTBErrOpen(Sender:TObject; errValue: word; errAddr: byte; errMsg:string);
    procedure OnMTBErrClose(Sender:TObject; errValue: word; errAddr: byte; errMsg:string);
    procedure OnMTBErrStart(Sender:TObject; errValue: word; errAddr: byte; errMsg:string);
    procedure OnMTBErrStop(Sender:TObject; errValue: word; errAddr: byte; errMsg:string);
    procedure OnMTBReady(Sender:TObject; ready:boolean);

    // centrala events:
    procedure OnCentralaDCCChange(Sender:TObject; state:boolean);
    procedure OnDCCGoError(Sender:TObject; Data:Pointer);
    procedure OnDCCStopError(Sender:TObject; Data:Pointer);

    procedure SetCallMethod(Method:TNotifyEvent);
    procedure OnSoundDisabled(Sender:TObject);
  end;//public

 TVytizeni=class                                                                // vytizeni procesoru programem
  Gauge:TGauge;                                                                   // objekt ve F_Main, co ktereho se kresli vytizeni
  GraphPos:Integer;                                                               // pozice v grafu procesoru
  LPa,LPb,LPc:Int64;                                                              // cteni procesoru
   procedure DetekujVytizeniProcesoru;                                            // vykresli vytizeni procesoru
   procedure DrawCPUGauge;                                                        // vytvori objekt Gauge a umisti ho na spravne misto
   procedure ResizeCPUGauge;                                                      // meni pozici Gauge pri zmene velikosti okna
 end;

 TReset=class                                                                   // reset SW
   procedure ZakladniPolohaVyhybek;                                                // prestavit vyhybky do zakladni polohy
 end;

 TStav=record                                                                   //stav systemu
  xTime:string;                                                                   // aktualni cas
  xDate:string;                                                                   // aktualni datum
 end;

TLogData=class
  function CreateLogDirectories:boolean;                                        // vytvori slozky, od kterych se ukladaji soubory log
end;

TSystemStatus = (null, starting, stopping);                                     // stav startovani / vypinani systemu
TSystem=class
  Status:TSystemStatus;                                                         // aktualni stav systemu
 end;

var
  F_Main: TF_Main;

  ResetData:TReset;                                                             // reset
  LogData:TLogData;                                                             // logovani
  OPData:TStav;                                                                 // aktualni datum a cas
  Vytizeni:TVytizeni;                                                           // zobrazeni vytizceni procesoru
  SystemData:TSystem;                                                           // zapinani / vypinani systemu
  TrkSystem:TTrkGUI;                                                            // trakce

  ini_lib:TMemInifile;                                                          // objekt pro pristup k ini_lib souboru
  Log:boolean;                                                                  // flag logovani do tabulky ve F_Main

  CloseMessage:Boolean;                                                         // flag ptain se uzivatele na ukonceni SW
  NUZClose:Boolean;                                                             // flag hard ukonceni SW bez kontroly pripojeni k systemum a zobrazeni dialogu

implementation

//deklarace ostatnich unit
uses fTester, fSettings, fNastaveni_Casu, fSplash,
     fAbout, Verze,
     fLoginPozadi, fSystemInfo,
     fBlkUsek, fBlkVyhybka, fAdminForm,
     fRegulator,
     fSystemAutoStart, fBlkUsekSysVars, GetSystems, Prevody,
     TechnologieMTB, TechnologieJC, FileSystem, fConsole,
     TOblsRizeni, TBloky, TBlok, TBlokUsek, TBlokVyhybka, TBlokSCom,
     TBlokIR, TOblRizeni, AC, SnadnSpusteni,
     TBlokPrejezd, TJCDatabase, Logging, TCPServerOR, DataAC, DataJC,
     DataBloky, DataHV, DataMTB, DataORs, DataZesilovac, fACEdit, fBlkNew, fHVEdit,
     fJCEdit, fZesilovacEdit, THVDatabase, fBlkIR, fBlkPrejezd, fBlkSCom, fBlkTrat,
     TBLokUvazka, SprDb, DataSpr, DataUsers, fUserEdit, UserDb,
     fBlkVyhybkaSysVars, fBlkTratSysVars, TBlokTrat, ModelovyCas, fBlkZamek,
     TBlokZamek, DataMultiJC, TMultiJCDatabase, fMJCEdit, ACDatabase,
     TBlokRozp, fBlkRozp, fFuncsSet, FunkceVyznam, fBlkTU, MTBdebugger, Booster,
     AppEv;

//function ShutdownBlockReasonCreate(hWnd: HWND; Reason: LPCWSTR): Bool; stdcall; external user32;
//function ShutdownBlockReasonDestroy(hWnd: HWND): Bool; stdcall; external user32;

{$R *.dfm}

procedure TF_Main.FormCreate(Sender: TObject);
 begin
  //vse presunuto do Form1.CreateSystem kvuli splash oknu
 end;//procedure

procedure TF_Main.PM_lib_MTBClick(Sender: TObject);
 begin
  Screen.Cursor := crHourGlass;
  writelog('MTB -> mtb.dll', WR_MTB);
  try
    MTB.LoadLib('mtb.dll');
  except
    on E:Exception do
     begin
      Screen.Cursor := crDefault;
      Application.MessageBox(PChar('Nelze naèíst knihovnu mtb.dll:'+#13#10+E.Message), 'Nelze naèíst knihovnu', MB_OK OR MB_ICONWARNING);
      AppEvents.LogException(E, 'Nelze naèíst knihovnu mtb.dll');
      Exit();
     end;
  end;
  Self.LogStatus('MTB: naèteno mtb.dll');
  Screen.Cursor := crDefault;
 end;

procedure TF_Main.PM_lib_SimClick(Sender: TObject);
 begin
  Screen.Cursor := crHourGlass;
  writelog('MTB -> simulator.dll', WR_MTB);
  try
    MTB.LoadLib('simulator.dll');
  except
    on E:Exception do
     begin
      Screen.Cursor := crDefault;
      Application.MessageBox(PChar('Nelze naèíst knihovnu simulator.dll:'+#13#10+E.Message), 'Nelze naèíst knihovnu', MB_OK OR MB_ICONWARNING);
      AppEvents.LogException(E, 'Nelze naèíst knihovnu simulator.dll');
      Exit();
     end;
  end;
  Self.LogStatus('MTB: naèteno simulator.dll');
  Screen.Cursor := crDefault;
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
   F_HVEdit.OpenForm(HVDB.HVozidla[Integer(LV_HV.Selected.Data^)]);
end;

procedure TF_Main.PC_1Change(Sender: TObject);
begin
 Self.DisableRemoveButtons();

 if (PC_1.ActivePage = TS_VC)         then JCTableData.UpdateTable;
 if (PC_1.ActivePage = TS_MultiJC)    then MultiJCTableData.UpdateTable;
 if (PC_1.ActivePage = TS_Users)      then UsersTableData.UpdateTable;
 if (PC_1.ActivePage = TS_Bloky)      then BlokyTableData.UpdateTable();
 if (PC_1.ActivePage = TS_Zesilovace) then ZesTableData.LoadToTable();
 if (PC_1.ActivePage = TS_Soupravy)   then SprTableData.UpdateTable();
 if (PC_1.ActivePage = TS_Aut_Rezimy) then ACTAbleData.UpdateTable();
 if (PC_1.ActivePage = F_Main.TS_HV)  then HVTableData.UpdateTable();
 if (PC_1.ActivePage = TS_Stanice)    then ORsTableData.UpdateTable(true);
end;

procedure TF_Main.PM_BlokyPopup(Sender: TObject);
var i:Integer;
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

procedure TF_Main.Timer1Timer(Sender: TObject);
 begin
  try
    ACDb.Update();
    SS.Update();
    DetekujAutSpusteniSystemu;
    Blky.Update();
    VypisDatumCas();
    ModCas.Update();
    JCDb.Update();
    MultiJCDb.Update();
    BoostersDb.Update();
    ORs.Update();
    UpdateCallMethod();
    MTBd.Update();
  except
   on E: Exception do
    begin
     if (not log_err_flag) then
       AppEvents.LogException(E, 'Main timer exception');
    end;
  end;
 end;//procedure

procedure TF_Main.PM_ResetVClick(Sender: TObject);
 begin
  ResetData.ZakladniPolohaVyhybek;
 end;

procedure TF_Main.PM_RegulatorClick(Sender: TObject);
var ret:Integer;
begin
 if (Self.LV_HV.Selected = nil) then Exit; 

 if (TrkSystem.openned) then
  begin
   try
    ret := RegCollector.Open(HVDb.HVozidla[StrToInt(Self.LV_HV.Selected.Caption)]);
    if (ret = 1) then
      Application.MessageBox('Dosáhli jste maximálního poètu otevøených regulátorù!', 'Varování', MB_OK OR MB_ICONWARNING);
   except

   end;
  end;//if
end;//procedure

procedure TF_Main.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var ci:TCloseInfo;
 begin
  if (NUZClose) then
   begin
    CanClose := true;
    Exit();
   end;
  ci := GetFunctions.CanClose();
  if (Integer(ci) > 0) then CanClose := false;

  case (ci) of
    TCloseInfo.ci_system_changing : begin
      writelog('Pokus o zavøení okna pøi zapínání nebo vypínání systémù', WR_ERROR);
      Application.MessageBox(PChar('Technologie právì zapíná nebo vypíná systémy, aplikaci nelze momentálnì zavøít.'+
              #13#10+'Nouzové ukonèení programu lze provést spuštìním pøíkazu "app-exit" v konzoli')
              , 'Nelze ukonèit program', MB_OK OR MB_ICONWARNING);
    end;

    TCloseInfo.ci_system_started : begin
      writelog('Pokus o zavøení okna bez ukonèení komunikace se systémy', WR_ERROR);
      if (Application.MessageBox('Program není odpojen od systémù, odpojit od systémù?',
        'Nelze ukonèit program', MB_YESNO OR MB_ICONWARNING) = mrYes) then
          F_Main.A_System_StopExecute(Self);
    end;

    TCloseInfo.ci_mtb : begin
      writelog('Pokus o zavøení okna bez uzavøení MTB', WR_ERROR);
      if (Application.MessageBox('Program není odpojen od MTB, odpojit?',
          'Nelze ukonèit program', MB_YESNO OR MB_ICONWARNING) = mrYes) then
       begin
        if (MTB.Start) then MTB.Stop()
        else if (MTB.Openned) then MTB.Close();
       end;
    end;

    TCloseInfo.ci_server : begin
      writelog('Pokus o zavøení okna bez vypnutí panel serveru', WR_ERROR);
      if (Application.MessageBox('PanelServer stále bìží, vypnout?',
          'Nelze ukonèit program', MB_YESNO OR MB_ICONWARNING) = mrYes) then
       ORTCPServer.Stop();
    end;

    TCloseInfo.ci_trakce : begin
      writelog('Pokus o zavøení okna bez odpojení od centrály', WR_ERROR);
      if (Application.MessageBox('Program není odpojen od centrály, odpojit?',
          'Nelze ukonèit program', MB_YESNO OR MB_ICONWARNING) = mrYes) then
        TrkSystem.Close();
    end;

    TCloseInfo.ci_yes : begin
      if (CloseMessage) then
       begin
        CanClose := (Application.Messagebox('Opravdu chcete ukonèit program?', 'hJOPserver',
            MB_YESNO OR MB_ICONQUESTION OR MB_DEFBUTTON2) = mrYES);
       end else begin//CloseMessage
        CloseMessage := true;
        CanClose     := true;
       end;//else CloseMessage
    end;

  end;//case
 end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TF_Main.AE_1Message(var Msg: tagMSG; var Handled: Boolean);
begin
 Handled := false;

 if (Msg.Message = MyMsg) then
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
         VK_F9:MTB.HideCfgDialog();
         VK_ESCAPE:if (F_About.Showing) then F_About.Close;
         VK_F4:begin
           // zobrazeni debug okna
           F_Admin.Show();
           Handled := true;
         end;
        end;//case
   end;
 end;
end;//procedure

procedure TF_Main.WMPowerBroadcast(var Msg: TMessage);
begin
 case (msg.WParam) of
    PBT_APMQUERYSUSPEND: begin
       msg.Result := BROADCAST_QUERY_DENY;
     end;

    PBT_APMSUSPEND: begin
       // windows is going to sleep -> disconnect all devices
       if (TrkSystem.openned) then
        begin
         ORTCPServer.Stop();
         try
           TrkSystem.EmergencyStop();
           TrkSystem.FastResetLoko();
           TrkSystem.Close(true);
         except

         end;
        end;
       if (MTB.Start) then MTB.Stop();
       if (MTB.Openned) then MTB.Close();
     end;

 end;//case
end;//procedure

procedure TF_Main.WMQueryEndSession(var Msg: TWMQueryEndSession);
begin
 if (GetFunctions.CanClose() <> ci_yes) then
  begin
   // disallow Windows from shutting down
//   ShutdownBlockReasonDestroy(Application.MainForm.Handle);
//   ShutdownBlockReasonCreate(Application.MainForm.Handle, 'hJOPserver není odpojen od systémù');
   Msg.Result := 0;
 end else begin
//   ShutdownBlockReasonDestroy(Application.MainForm.Handle);
   Msg.Result := 1;
   CloseMessage := false;
   NUZClose     := true;
 end;
 inherited;
end;

procedure TF_Main.WMEndSession(var Msg: TWMEndSession);
begin
 if (Msg.EndSession = True) then
  begin
   if (TrkSystem.openned) then
    begin
     ORTCPServer.Stop();
     try
       TrkSystem.EmergencyStop();
       TrkSystem.FastResetLoko();
       TrkSystem.Close(true);
     except

     end;
    end;
   if (MTB.Start) then MTB.Stop();
   if (MTB.Openned) then MTB.Close();

   CloseMessage := false;
   NUZClose     := true;
   F_Main.Close();
  end;
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_Main.A_All_Loko_OdhlasitExecute(Sender: TObject);
begin
 F_Main.LogStatus('Loko: odhlašuji...');
 Application.ProcessMessages();
 F_Main.S_lok_prevzato.Brush.Color := clBlue;
 TrkSystem.OdhlasitAll;
end;

procedure TF_Main.A_All_Loko_PrevzitExecute(Sender: TObject);
begin
 F_Main.LogStatus('Loko: pøebírám...');
 Application.ProcessMessages();
 F_Main.S_lok_prevzato.Brush.Color := clBlue;
 TrkSystem.PrevzitAll;
end;//procedure

procedure TF_Main.A_DCC_GoExecute(Sender: TObject);   //DCC go
var return:Integer;
begin
  Self.LogStatus('DCC: zapínám');
  TrkSystem.callback_err := TTrakce.GenerateCallback(Self.OnDCCGoError);
  return := TrkSystem.CentralStart();
  if (return <> 0) then
   begin
    Application.MessageBox(PChar('Chyba pri DCC GO: chyba '+IntToStr(return)),'Chyba',MB_OK OR MB_ICONERROR);
    Self.LogStatus('DCC: START: ERR '+IntToStr(return));
   end;
end;//procedure

procedure TF_Main.A_DCC_StopExecute(Sender: TObject); //DCC stop
var return:Integer;
begin
  Self.LogStatus('DCC: vypínám');
  TrkSystem.callback_err := TTrakce.GenerateCallback(Self.OnDCCStopError);
  return := TrkSystem.CentralStop();
  if (return <> 0) then
   begin
    Application.MessageBox(PChar('Chyba pri DCC STOP: chyba '+IntToStr(return)),'Chyba',MB_OK OR MB_ICONERROR);
    Self.LogStatus('DCC: STOP: ERR '+IntToStr(return));
   end;
end;

procedure TF_Main.A_FuncsSetExecute(Sender: TObject);
begin
 F_FuncsSet.Show();
end;//procedure

procedure TF_Main.OnDCCGoError(Sender:TObject; Data:Pointer);
begin
 SystemData.Status := TSystemStatus.null;
 F_Main.A_System_Start.Enabled := true;
 F_Main.A_System_Stop.Enabled  := true;
 F_Main.A_DCC_Go.Enabled       := true;
 F_Main.A_DCC_Stop.Enabled     := true;
 F_Main.S_Intellibox_go.Brush.Color  := clGray;
 Self.LogStatus('DCC: START: ERR: cenrála neodpovìdìla na pøíkaz');
 Application.MessageBox('Centrála neodpovìdìla na pøíkaz DCC START', 'Varování', MB_OK OR MB_ICONWARNING);
end;//procedure

procedure TF_Main.OnDCCStopError(Sender:TObject; Data:Pointer);
begin
 Self.LogStatus('DCC: STOP: ERR: cenrála neodpovìdìla na pøíkaz');
 F_Main.A_System_Start.Enabled := true;
 F_Main.A_System_Stop.Enabled  := true;
 F_Main.A_DCC_Go.Enabled       := true;
 F_Main.A_DCC_Stop.Enabled     := true;
 F_Main.S_Intellibox_go.Brush.Color  := clGray;
 Application.MessageBox('Centrála neodpovìdìla na pøíkaz DCC STOP', 'Varování', MB_OK OR MB_ICONWARNING);
end;//procedure


procedure TF_Main.A_lib_cfgExecute(Sender: TObject);
begin
 MTB.ShowCfgDialog();
 writelog('Zobrazen ConfigDialog knihovny',WR_MTB);
end;

procedure TF_Main.A_MTB_CloseExecute(Sender: TObject);
begin
 if ((SystemData.Status = stopping) and (not MTB.Openned)) then
  begin
   Self.LogStatus('System: stop OK');
   SystemData.Status := null;
   Self.A_System_Start.Enabled := true;
   Exit();
  end;

 F_Main.S_MTB_open.Brush.Color := clBlue;
 Self.LogStatus('MTB: uzavírám zaøízení...');

 writelog('----- MTB CLOSING -----',WR_MTB);

 with (F_Main) do
  begin
   A_MTB_Open.Enabled      := false;
   A_MTB_Close.Enabled     := false;
   SB1.Panels.Items[_SB_MTB].Text := 'MTB closing...';
  end;//with F_Main do

 try
   MTB.Close();
 except
   on E:Exception do
    begin
     AppEvents.LogException(E, '----- MTB CLOSE FAIL -----');
     Self.LogStatus('MTB: CLOSE FAIL - '+E.Message);
     Application.MessageBox(PChar('Chyba pøi uzavírání MTB - '+E.Message),'Chyba',MB_OK OR MB_ICONERROR);
    end;
 end;
end;

procedure TF_Main.A_MTB_GoExecute(Sender: TObject);
begin
 if ((SystemData.Status = starting) and (MTB.Start)) then
  begin
   Self.A_Trk_ConnectExecute(nil);
   Exit();
  end;

  with (F_Main) do
   begin
    A_MTB_Go.Enabled     := false;
    A_MTB_Stop.Enabled   := false;
    A_MTB_Close.Enabled  := false;

    SB1.Panels.Items[_SB_MTB].Text := 'MTB starting...';
   end;//with F_Main do

  Self.LogStatus('MTB: Spouštím komunikaci...');
  F_Main.S_MTB_Start.Brush.Color   := clBlue;

  writelog('----- MTB STARTING -----',WR_MTB);

  try
    MTB.Go();
  except
   on E:Exception do
    begin
     AppEvents.LogException(E, '----- MTB START FAIL -----');
     Self.LogStatus('MTB: START FAIL - '+E.Message);
     if (SystemData.Status = starting) then
      begin
       SystemData.Status := TSystemStatus.null;
       Self.A_System_Start.Enabled := true;
      end;
     Application.MessageBox(PChar('Chyba pri zapinani komunikace'+#13#10+E.Message), 'Chyba', MB_OK OR MB_ICONERROR);
    end;
  end;
end;

procedure TF_Main.A_MTB_OpenExecute(Sender: TObject);
begin
 if ((SystemData.Status = starting) and (MTB.Openned)) then
  begin
   Self.A_MTB_GoExecute(nil);
   Exit();
  end;

 with (F_Main) do
  begin
   A_MTB_Open.Enabled     := false;
   A_MTB_Close.Enabled    := false;

   SB1.Panels.Items[_SB_MTB].Text := 'MTB opening...';
  end;//with F_Main do

 Self.LogStatus('MTB: Otevírám zaøízení, hledám moduly...');
 F_Main.S_MTB_open.Brush.Color   := clBlue;

 writelog('----- MTB OPENING -----',WR_MTB);

 try
   MTB.Open();
 except
  on E:Exception do
   begin
    AppEvents.LogException(E, '----- MTB OPEN FAIL -----');
    Self.LogStatus('MTB: OPEN FAIL - '+E.Message);
    if (SystemData.Status = starting) then
     begin
      SystemData.Status := TSystemStatus.null;
      Self.A_System_Start.Enabled := true;
     end;
    Application.MessageBox(PChar('Chyba pri otevírání MTB'+#13#10+E.Message),'Chyba', MB_OK OR MB_ICONERROR);
   end;
 end;
end;//procedure

procedure TF_Main.A_MTB_StopExecute(Sender: TObject);
begin
 ACDb.StopAllACs();

 if ((SystemData.Status = stopping) and (not MTB.Start)) then
  begin
   F_Main.A_MTB_CloseExecute(nil);
   Exit();
  end;

 F_Main.S_MTB_Start.Brush.Color := clGray;
 Self.LogStatus('MTB: zastavuji komunikaci...');

 writelog('----- MTB STOPPING -----',WR_MTB);

 with (F_Main) do
  begin
   A_MTB_Go.Enabled      := false;
   A_MTB_Stop.Enabled    := false;
   SB1.Panels.Items[_SB_MTB].Text := 'MTB stopping...';
  end;//with F_Main do

  try
    MTB.Stop();
  except
   on E:Exception do
    begin
     AppEvents.LogException(E, '----- MTB STOP FAIL -----');
     Self.LogStatus('MTB: STOP ERR - '+E.Message);
     Application.MessageBox(PChar('Chyba pri vypinani komunikace'+#13#10+E.Message),'Chyba',MB_OK OR MB_ICONERROR);
    end;
  end;
end;

procedure TF_Main.A_PanelServer_StartExecute(Sender: TObject);
begin
 if ((SystemData.Status = starting) and (not Blky.enabled)) then Blky.Enable();

 try
   ORTCPServer.Start();
 except
   on E : Exception do
    begin
     Application.MessageBox(PChar('Chyba pøi zapínání serveru - '+#13#10+E.Message), 'Chyba', MB_OK OR MB_ICONWARNING);
     Exit();
    end;
 end;

 Self.A_PanelServer_Start.Enabled := false;
 Self.A_PanelServer_Stop.Enabled  := true;
end;

procedure TF_Main.A_PanelServer_StopExecute(Sender: TObject);
begin
 ORTCPServer.Stop();

 Self.A_PanelServer_Start.Enabled := true;
 Self.A_PanelServer_Stop.Enabled  := false;
end;

////////////////////////////////////////////////////////////////////////////////

//--- events from MTB lib begin ---
procedure TF_Main.OnMTBStart(Sender:TObject);
begin
  with (F_Main) do
   begin
    A_MTB_Go.Enabled     := false;
    A_MTB_Stop.Enabled   := true;

    PM_Tester.Enabled    := true;
    PM_ResetV.Enabled    := true;

    SB1.Panels.Items[_SB_MTB].Text := 'MTB started';
   end;//with F_Main do

  writelog('----- MTB START OK -----',WR_MTB);

  Self.LogStatus('MTB: komunikace spuštìna');
  F_Main.S_MTB_Start.Brush.Color     := clLime;
  MTBTableData.UpdateTable();

  if (F_Admin.CHB_SystemStart.Checked) then
    Blky.Enable();

  // aktualizace ovladaich prvku AC
  if (F_Main.LV_AC_Db.Selected <> nil) then
    F_Main.LV_AC_DbChange(F_Main.LV_AC_Db, F_Main.LV_AC_Db.Selected, TItemChange.ctText);

  // inicialziace osvetleni
  ORs.InitOsv();

  if (SystemData.Status = starting) then
   Self.A_Trk_ConnectExecute(nil);
end;//procedure

procedure TF_Main.OnMTBStop(Sender:TObject);
begin
  if (F_Admin.CHB_SystemStart.Checked) then
   begin
    // debug mod
    JCDB.RusAllJC();
    ORs.DisconnectPanels();
   end;//if AdminVstup
  if (Blky.enabled) then Blky.Disable();

  ModCas.started := false;

  //vynulovani RunErroru
  Konfigurace.ini := TMemIniFile.Create(F_Options.E_dataload.Text);
  Konfigurace.ini.WriteInteger('SystemCfg', 'RunError',0);
  Konfigurace.ini.UpdateFile;
  Konfigurace.ini.Free;

  if (F_Tester.Showing) then F_Tester.Close();  

  F_Main.S_MTB_Start.Brush.Color := clRed;

  with (F_Main) do
   begin
    A_MTB_Go.Enabled      := true;
    A_MTB_Stop.Enabled    := false;
    A_MTB_Close.Enabled   := true;

    PM_ResetV.Enabled     := false;
    PM_Tester.Enabled     := false;

    SB1.Panels.Items[_SB_MTB].Text := 'MTB openned';
   end;//with F_Main do


  writelog('----- MTB STOP OK -----',WR_MTB);

  Self.LogStatus('MTB: komunikace zastavena');

  MTBTableData.UpdateTable();

  if ((F_Main.Showing) and (F_Main.PC_1.ActivePage = F_Main.TS_Bloky)) then BlokyTableData.UpdateTable;

  if (SystemData.Status = stopping) then
   Self.A_MTB_CloseExecute(nil);
end;//procedure

procedure TF_Main.OnMTBOpen(Sender:TObject);
var i:Integer;
    str:string;
begin
 Self.A_MTB_Open.Enabled     := false;
 Self.A_MTB_Close.Enabled    := true;
 Self.A_MTB_Go.Enabled       := true;
 Self.A_MTB_Stop.Enabled     := false;
 Self.MI_Libs.Enabled        := false;

 F_Main.S_MTB_open.Brush.Color := clLime;

 MTBTableData.LoadToTable;

 writelog('----- MTB OPEN OK : '+IntToStr(MTB.count)+' modules -----', WR_MTB);

 Self.LogStatus('MTB: otevøeno');
 SB1.Panels.Items[_SB_MTB].Text := 'MTB openned';

 F_Tester.AfterMTBOpen();

 MTBTableData.UpdateTable();

 if (SystemData.Status = starting) then
  begin
   // scan, jestli nahodou nechybi MTB desky
   str := '';
   for i := 0 to MTB._MAX_MTB-1 do
    if ((MTB.GetNeeded(i)) and (not MTB.IsModule(i))) then
     begin
      if (Length(str) > 0) then str := str + ', ';
      str := str + IntToStr(i);
     end;
   if (str <> '') then
    begin
     writelog('Chybí MTB moduly '+str, WR_MTB, 1);
     Self.LogStatus('WARN: Chybí MTB moduly '+str);
{    // chybeni MTB modulu pouze zalogovano, preskakujeme
     if (Application.MessageBox(PChar('Chybí MTB moduly '+str+#13#10+'Chcete pokraèovat?'), 'Varování', MB_YESNO OR MB_ICONWARNING) = mrNo) then
      begin
       Self.LogStatus('Chybí MTB moduly - storno');
       SystemData.Status := null;
       Exit();
      end;
     Self.LogStatus('Chybí MTB moduly - pokraèuji'); }
    end;

   Self.A_MTB_GoExecute(nil);
  end;
end;//procedure

procedure TF_Main.OnMTBClose(Sender:TObject);
begin
 Self.A_MTB_Go.Enabled    := false;
 Self.A_MTB_Stop.Enabled  := false;
 Self.A_MTB_Close.Enabled := false;
 Self.A_MTB_Open.Enabled  := true;
 Self.MI_Libs.Enabled     := true;

 F_Main.S_MTB_open.Brush.Color := clRed;

 writelog('----- MTB CLOSE OK -----',WR_MTB);

 Self.LogStatus('MTB: uzavøeno');
 SB1.Panels.Items[_SB_MTB].Text := 'MTB closed';

 if (SystemData.Status = stopping) then
  begin
   Self.LogStatus('System: stop OK');
   SystemData.Status := null;
   Self.A_System_Start.Enabled := true;
  end;

 MTBTableData.UpdateTable();
end;//procedure

procedure TF_Main.OnMTBErrOpen(Sender:TObject; errValue: word; errAddr: byte; errMsg:string);
begin
 Self.A_MTB_Go.Enabled    := false;
 Self.A_MTB_Stop.Enabled  := false;
 Self.A_MTB_Open.Enabled  := true;

 Self.A_System_Start.Enabled := true;

 F_Main.S_MTB_open.Brush.Color := clRed;

 SystemData.Status := null;

 Self.LogStatus('ERR: MTB: '+errMsg);
 SB1.Panels.Items[_SB_MTB].Text := 'MTB closed';

 Application.MessageBox(PChar('Pri otevírání MTB nastala chyba '+#13#10+errMsg+#13#10+'Chybovy kod: '+IntToStr(errValue)+':'+IntToStr(errAddr)), 'Chyba', MB_OK OR MB_ICONERROR);
 writelog('----- MTB OPEN DRIVER ERROR - '+errMsg+' - errValue='+IntToStr(errValue)+' -----', WR_ERROR, 21);
end;//procedure

procedure TF_Main.OnMTBErrClose(Sender:TObject; errValue: word; errAddr: byte; errMsg:string);
begin
 Self.A_MTB_Go.Enabled    := false;
 Self.A_MTB_Stop.Enabled  := false;
 Self.A_MTB_Open.Enabled  := true;

 F_Main.S_MTB_open.Brush.Color := clRed;

 SystemData.Status := null;

 Self.LogStatus('ERR: MTB: '+errMsg);
 SB1.Panels.Items[_SB_MTB].Text := 'MTB closed';

 Application.MessageBox(PChar('Pri uzavírání MTB nastala chyba '+#13#10+errMsg+#13#10+'Chybovy kod: '+IntToStr(errValue)+':'+IntToStr(errAddr)),'Chyba',MB_OK OR MB_ICONERROR);
 writelog('----- MTB CLOSE MTB DRIVER ERROR - '+errMsg+' - errValue='+IntToStr(errValue)+' -----', WR_ERROR, 21);
end;//procedure

procedure TF_Main.OnMTBErrStart(Sender:TObject; errValue: word; errAddr: byte; errMsg:string);
begin
  with (F_Main) do
   begin
    A_MTB_Go.Enabled     := true;
    SB1.Panels.Items[_SB_MTB].Text := 'MTB openned';
   end;//with F_Main do

  Self.A_System_Start.Enabled := true;
  F_Main.S_MTB_Start.Brush.Color   := clRed;

  SystemData.Status := null;

  //defaultni hodnota padu
  Konfigurace.ini := TMemIniFile.Create(F_Options.E_dataload.Text);
  Konfigurace.ini.WriteInteger('SystemCfg','RunError',0);
  Konfigurace.ini.UpdateFile;
  Konfigurace.ini.Free;

  Self.LogStatus('ERR: MTB: '+errMsg);

  Application.MessageBox(PChar('Pri zapínání komunikace nastala chyba:'+#13#10+errMsg+#13#10+'Chybovy kod: '+IntToStr(errValue)+':'+IntToStr(errAddr)), 'Chyba', MB_OK OR MB_ICONERROR);
  writelog('----- MTB START DRIVER ERROR - '+errMsg+' - errValue='+IntToStr(errValue)+' -----',WR_ERROR,21);
end;//procedure

procedure TF_Main.OnMTBErrStop(Sender:TObject; errValue: word; errAddr: byte; errMsg:string);
begin
  with (F_Main) do
   begin
    A_MTB_Go.Enabled     := true;
    SB1.Panels.Items[_SB_MTB].Text := 'MTB openned';
   end;//with F_Main do

  SystemData.Status := null;

  F_Main.S_MTB_Start.Brush.Color   := clRed;

  F_Main.A_MTB_Open.Enabled  := true;
  F_Main.A_MTB_Close.Enabled := true;

  Self.LogStatus('ERR: MTB: '+errMsg);

  Application.MessageBox(PChar('Pri vypínání komunikace nastala chyba '+#13#10+errMsg+#13#10+'Chybovy kod: '+IntToStr(errValue)+':'+IntToStr(errAddr)),'Chyba',MB_OK OR MB_ICONERROR);
  writelog('----- MTB STOP DRIVER ERROR - '+errMsg+' - errValue='+IntToStr(errValue)+' -----',WR_ERROR,21);
end;//procedure

procedure TF_Main.OnMTBReady(Sender:TObject; ready:boolean);
begin
 if (ready) then
  begin
   Self.A_MTB_Open.Enabled  := (not MTB.Openned);
   Self.A_MTB_Close.Enabled := MTB.Openned;
   Self.A_MTB_Go.Enabled    := (MTB.Openned) and (not MTB.Start);
   Self.A_MTB_Stop.Enabled  := MTB.Start;
  end else begin
   Self.A_MTB_Open.Enabled  := false;
   Self.A_MTB_Close.Enabled := false;
   Self.A_MTB_Go.Enabled    := false;
   Self.A_MTB_Stop.Enabled  := false;
  end;
end;

//--- events from MTB lib end ---

////////////////////////////////////////////////////////////////////////////////

procedure TF_Main.A_SaveStavExecute(Sender: TObject);
begin
  try
    // ukladani stavu bloku: ulozime do docasneho souboru a az pak prepiseme stavajici konfigurak
    Blky.SaveStatToFile(Blky.fstatus+'_');

    DeleteFile(Blky.fstatus);
    MoveFile(PChar(Blky.fstatus+'_'), PChar(Blky.fstatus));
    DeleteFile(Blky.fstatus+'_');
  except
    on E:Exception do
      AppEvents.LogException(E, 'Blky.SaveStatToFile');
  end;

  try
    HVDb.SaveToDir('lok');    // tady se ulozi predevsim stavy funkci
  except
    on E:Exception do
      AppEvents.LogException(E, 'HvDb.SaveToDir');
  end;

  try
    Soupravy.SaveData(F_Main.E_dataload_soupr.Text+'_');

    DeleteFile(F_Main.E_dataload_soupr.Text);
    MoveFile(PChar(F_Main.E_dataload_soupr.Text+'_'), PChar(F_Main.E_dataload_soupr.Text));
    DeleteFile(F_Main.E_dataload_soupr.Text+'_');
  except
    on E:Exception do
      AppEvents.LogException(E, 'Soupravy.SaveData');
  end;

  try
    F_Main.SaveFormPosition;
    FormData.SaveFormData(FormData.aFile);
  except
    on E:Exception do
      AppEvents.LogException(E, 'Save form position');
  end;

  try
    ORs.SaveStatus(ORs.status_filename);
  except
    on E:Exception do
      AppEvents.LogException(E, 'Save OR status');
  end;

  ini_lib.WriteBool('Log','main-file', Self.CHB_Mainlog_File.Checked);
  ini_lib.WriteBool('Log','main-table', Self.CHB_Mainlog_Table.Checked);

  try
    Konfigurace.ini := TMemIniFile.Create(ExtractRelativePath(ExtractFilePath(Application.ExeName), F_Options.E_dataload.Text));
    ModCas.SaveData(Konfigurace.ini);
    Konfigurace.ini.WriteBool('SystemCfg', 'FirstStart', false);
    Konfigurace.ini.WriteString('funcsVyznam', 'funcsVyznam', FuncsFyznam.GetFuncsVyznam());
    Konfigurace.ini.UpdateFile();
    Konfigurace.ini.Free();
  except
    on E:Exception do
      AppEvents.LogException(E, 'Save cfg');
  end;
end;

procedure TF_Main.A_System_StartExecute(Sender: TObject);      //system start
begin
 Self.LB_Log.Items.Insert(0, '--------------------------------------------------------------------------------');

 if (not MTB.ready) then
  begin
   Application.MessageBox(PChar('Systém nelze spustit, MTB není pøipraveno k zapnutí systému'+#13#10+'Možné pøíèiny:'+#13#10+' - nenaètena validní knihovna'), 'Nelze spustit', MB_OK OR MB_ICONWARNING);
   Self.LogStatus('ERR: Systém nelze spustit, MTB není pøipraveno k zapnutí systému');
   Exit();
  end;

 Self.LogStatus('Zapínám systémy...');
 SystemData.Status := starting;
 Self.A_System_Start.Enabled := false;
 Self.A_MTB_OpenExecute(nil);
end;//procedure

procedure TF_Main.A_System_StopExecute(Sender: TObject);       //system stop
begin
 Self.A_System_Stop.Enabled := false;

 Self.LB_Log.Items.Insert(0, '--------------------------------------------------------------------------------');
 Self.LogStatus('Vypínám systémy...');
 SystemData.Status := stopping;

 Self.LogStatus('Zastavuji všechny soupravy...');
 Soupravy.StopAllSpr();

 Application.ProcessMessages();

 Self.LogStatus('Odpojuji panely...');
 ORs.DisconnectPanels();

 Self.A_PanelServer_StopExecute(nil);

 JCDb.RusAllJC();
 Blky.Disable();
 Blky.Reset();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_Main.SetCallMethod(Method:TNotifyEvent);
begin
 while (Assigned(Self.call_method)) do
  begin
   Application.ProcessMessages();
   sleep(1);
  end;
 Self.call_method := method;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TF_Main.A_Trk_ConnectExecute(Sender: TObject);
var return:Integer;
    err:string;
begin
 return := TrkSystem.Open;
 if (return <> 0) then
  begin
   case (return) of
    3: err := 'nelze otevøít COM port, více informací v LOGu';
   else
    err := 'neznámá chyba';
   end;

   Application.MessageBox(PChar('Chyba pri otevírání komunikace s centrálou: chyba '+IntToStr(return)+#13#10+err),'Chyba',MB_OK OR MB_ICONERROR);

   F_Main.A_Trk_Connect.Enabled          := true;
   F_Main.SB1.Panels.Items[_SB_INT].Text := 'Odpojeno';
   F_Main.S_Intellibox_connect.Brush.Color := clRed;

   Exit;
  end;
 Application.ProcessMessages;
end;//procedure

procedure TF_Main.A_Trk_DisconnectExecute(Sender: TObject);
var return:Integer;
    err:string;
begin
 return := TrkSystem.Close;
 if (return <> 0) then
  begin
   case (return) of
    3: err := 'nelze zavøít COM port, více informací v LOGu';
   else
    err := 'neznámá chyba';
   end;

   Application.MessageBox(PChar('Chyba pri uzavírání komunikace s centrálou: chyba '+IntToStr(return)+#13#10+err),'Chyba',MB_OK OR MB_ICONERROR);
   Exit;
  end;
 Application.ProcessMessages;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_Main.B_AutRezim_addClick(Sender: TObject);
begin
 F_AutRezEdit.NewAutRezCreate;
end;

procedure TF_Main.B_AutRezim_deleteClick(Sender: TObject);
begin
 if Application.MessageBox(PChar('Opravdu smazat AC '+ACDb.ACs[Self.LV_AC_Db.ItemIndex].name+'?'), 'Mazání AC', MB_YESNO OR MB_ICONQUESTION OR MB_DEFBUTTON2) = mrYes then
   ACDb.RemoveAC(Self.LV_AC_Db.ItemIndex);
end;

procedure TF_Main.B_BlkAddClick(Sender: TObject);
begin
 F_BlkNew.OpenForm;
end;

procedure TF_Main.B_BlkDeleteClick(Sender: TObject);
var pozice:Integer;
 begin
  Pozice := LV_Bloky.ItemIndex;

  Beep;
  if Application.MessageBox(PChar('Opravdu chcete smazazat blok '+Blky.GetBlkIndexName(pozice)+'?'),'Mazání bloku', MB_YESNO OR MB_ICONQUESTION OR MB_DEFBUTTON2) = mrYes then
   begin
    try
      Blky.Delete(pozice);
    except
      on E:Exception do
        Application.MessageBox(PChar('Chyba:'+#13#10+E.Message), 'Chyba', MB_OK OR MB_ICONWARNING);
    end;
   end;//if MesageBox
end;

// ulozeni zmen fyznamu funkci z Mema do struktury programu
procedure TF_Main.B_ChangeClick(Sender: TObject);
var data:string;
    i:Integer;
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
 if (Application.MessageBox('Opravdu smazat najeté bloky a kilometry všech hnacích vozidel?', 'Opravdu?', MB_YESNO OR MB_ICONQUESTION OR MB_DEFBUTTON2) = mrYes) then
   HVDb.ClearAllStatistics();
end;

procedure TF_Main.OnFuncsVyznamChange(Sender:TObject);
var i:Integer;
begin
 if (not Self.CHB_LoadChanges.Checked) then Exit();
 Self.M_funcsVyznam.Clear();
 for i := 0 to FuncsFyznam.Items.Count-1 do
   Self.M_funcsVyznam.Lines.Add(FuncsFyznam.Items[i]);
 F_FuncsSet.UpdateFuncsList(FuncsFyznam.Items);
end;//procedure

procedure TF_Main.B_CS_Ver_UpdateClick(Sender: TObject);
begin
 TrkSystem.GetCSVersion();
 TrkSystem.GetLIVersion();
end;

procedure TF_Main.B_HVStats_ExportClick(Sender: TObject);
var fn:string;
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
    on E:Exception do
      Application.MessageBox(PChar('Nelze exporotvat'+#13#10+E.Message), 'Chyba', MB_OK OR MB_ICONERROR);
   end;
  end;
end;

procedure TF_Main.B_HV_AddClick(Sender: TObject);
begin
 F_HVEdit.NewHV();
end;

procedure TF_Main.B_HV_DeleteClick(Sender: TObject);
var addr:Word;
    return:Integer;
begin
 if (Self.LV_HV.Selected = nil) then Exit(); 

 addr := Integer(LV_HV.Selected.Data^);

 if Application.MessageBox(PChar('Opravdu chcete smazat HV '+IntToStr(addr)+'?'),'Mazání HV', MB_YESNO OR MB_ICONQUESTION OR MB_DEFBUTTON2) = mrYes then
  begin
   return := HVDb.Remove(addr);
   if (return <> 0) then
    begin
     case (return) of
       1: Application.MessageBox('Operace se nezdaøila - HV neexistuje', 'Chyba', MB_OK OR MB_ICONWARNING);
       2: Application.MessageBox(PCHar('Operace se nezdaøila - HV je pøiøazeno soupravì '+Soupravy.GetSprNameByIndex(HVDb.HVozidla[addr].Stav.souprava)), 'Chyba', MB_OK OR MB_ICONWARNING);
     else
      Application.MessageBox(PChar('Operace se nezdaøila - chyba '+IntToStr(return)), 'Chyba', MB_OK OR MB_ICONWARNING);
     end;

     Exit;
    end;
  end;
end;

procedure TF_Main.B_JC_ResetClick(Sender: TObject);
var JC:TJC;
begin
 if (Self.LV_JC.Selected = nil) then Exit();

 JC := JCDb.GetJCByIndex(Self.LV_JC.ItemIndex);
 if (JC.staveni) then
   JC.CancelStaveni('Nouzové rušení stavìní JC', true);
end;

procedure TF_Main.B_lok_deleteClick(Sender: TObject);
begin
 if (not Assigned(Soupravy.soupravy[Self.LV_Soupravy.ItemIndex])) then Exit();

 if (Application.MessageBox(PChar('Opravdu smazat soupravu '+Soupravy.soupravy[Self.LV_Soupravy.ItemIndex].nazev+'?'), '?', MB_YESNO OR MB_ICONQUESTION) = mrYes) then
  Soupravy.RemoveSpr(Self.LV_Soupravy.ItemIndex);
end;

procedure TF_Main.B_mJC_AddClick(Sender: TObject);
begin
 F_MJCEdit.OpenForm(nil);
end;

procedure TF_Main.B_mJC_RemoveClick(Sender: TObject);
begin
 if ((Self.LV_MultiJC.Selected <> nil) and (Application.MessageBox(PChar('Opravdu smazat složenou jízdní cestu '+MultiJCDb.GetJCByIndex(Self.LV_MultiJC.ItemIndex).Nazev), 'Opravdu?', MB_YESNO OR MB_ICONQUESTION) = mrYes)) then
   MultiJCDb.Remove(Self.LV_MultiJC.ItemIndex);
end;

procedure TF_Main.B_RemoveStackClick(Sender: TObject);
var OblR:TOR;
begin
 if (Self.LV_Stanice.Selected = nil) then Exit();
 ORs.GetORByIndex(Self.LV_Stanice.ItemIndex, OblR);
 if (Application.MessageBox(PChar('Opravdu smazat zásobník jízdních cest stanice '+OblR.Name+' ?'), 'Opravdu?', MB_YESNO OR MB_ICONQUESTION) = mrYes) then
   OblR.stack.ClearStack();
end;

procedure TF_Main.B_User_AddClick(Sender: TObject);
begin
 F_UserEdit.NewUser();
end;

procedure TF_Main.B_User_DeleteClick(Sender: TObject);
begin
 if (Application.MessageBox(PChar('Opravdu smazat uživatele '+Self.LV_Users.Selected.SubItems.Strings[0]+' ?'), 'Opravdu?', MB_YESNO OR MB_ICONQUESTION) = mrYes) then
  begin
   UsrDB.RemoveUser(Self.LV_Users.ItemIndex);
   Self.B_User_Delete.Enabled := false;
  end;
end;//procedure

procedure TF_Main.B_VC_AddClick(Sender: TObject);
begin
  F_JCEdit.NewVCCreate();
end;

procedure TF_Main.B_VC_deleteClick(Sender: TObject);
begin
 if (Application.MessageBox(PChar('Opravdu chcete smazat jízdní cestu '+JCDb.GetJCByIndex(LV_JC.ItemIndex).nazev+' ?'),'Mazání jízdní cesty', MB_YESNO OR MB_ICONQUESTION OR MB_DEFBUTTON2) = mrYes) then
  begin
   try
     JCDb.RemoveJC(LV_JC.ItemIndex);
   except
     on E:Exception do
       Application.MessageBox(PChar('Nelze smazat JC'+#13#10+E.Message), 'Chyba', MB_OK OR MB_ICONERROR);
   end;
  end;
end;

procedure TF_Main.B_zes_addClick(Sender: TObject);
begin
  F_ZesilovacEdit.NewZes();
end;

procedure TF_Main.B_zes_deleteClick(Sender: TObject);
var pozice,return:integer;
begin
 Pozice := LV_Zesilovace.ItemIndex;
 Beep;
 if Application.MessageBox(PChar('Opravdu chcete smazat zesilovac '+BoostersDb.GetBooster(pozice).bSettings.Name+'?'),'Mazání zesilovace', MB_YESNO OR MB_ICONQUESTION OR MB_DEFBUTTON2) = mrYes then
  begin
   return := BoostersDb.RemoveBooster(pozice);
   if (return <> 0) then
    begin
     Application.MessageBox(PChar('Chyba pri mazani zesilovace - chyba '+IntToStr(return)),'Chyba',MB_OK OR MB_ICONSTOP);
     Exit;
    end;

   LV_Zesilovace.Items.Delete(Pozice);
  end;//if MessageBox
end;

//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TF_Main.PM_Help_RPClick(Sender: TObject);
 begin
  F_About.ShowModal;
 end;

procedure TF_Main.PM_HVPopup(Sender: TObject);
var i:Integer;
begin
 if (Self.LV_HV.Selected = nil) then
  begin
   for i := 0 to (Sender as TPopUpMenu).Items.Count-1 do
    (Sender as TPopUpMenu).Items.Items[i].Enabled := false;
  end else begin
   for i := 0 to (Sender as TPopUpMenu).Items.Count-1 do
    (Sender as TPopUpMenu).Items.Items[i].Enabled := true;
  end;
end;//procedure

procedure TReset.ZakladniPolohaVyhybek;
var i:Integer;
    Blk:TBlk;
 begin
  for i := 0 to Blky.Cnt-1 do
   begin
    Blky.GetBlkByIndex(i,Blk);
    if (Blk.GetGlobalSettings().typ <> _BLK_VYH) then continue;
    (Blk as TBlkVyhybka).SetPoloha(plus);
   end;//for cyklus
  writelog('Vyhýbky pøestaveny do základní polohy', WR_MESSAGE);
 end;//procedure

procedure TF_Main.PM_SB1Click(Sender: TObject);
begin
 if PM_SB1.Checked then
  begin
   SB1.Visible:=true;
   writelog('Zobrazeno SB1',WR_MESSAGE);
  end else begin
   SB1.Visible:=false;
   writelog('Skryto SB1',WR_MESSAGE);
  end;
end;

procedure TF_Main.T_functionTimer(Sender: TObject);
begin
 try
   Vytizeni.DetekujVytizeniProcesoru;

   // update tables
   if (Self.Showing) then
    begin
     if (F_Main.PC_1.ActivePage = F_Main.TS_Aut_Rezimy) then ACTableData.UpdateTable();
     if (F_Main.PC_1.ActivePage = F_Main.TS_Bloky) then BlokyTableData.UpdateTable();
     if (F_Main.PC_1.ActivePage = F_Main.TS_Soupravy) then SprTableData.UpdateTable();
     if (F_Main.PC_1.ActivePage = F_Main.TS_Zesilovace) then F_Main.LV_Zesilovace.Repaint;
     if (F_Main.PC_1.ActivePage = F_Main.TS_HV) then HVTableData.UpdateTable();
     if (F_Main.PC_1.ActivePage = F_Main.TS_VC) then JCTableData.UpdateTable();
     if (F_Main.PC_1.ActivePage = F_Main.TS_MultiJC) then MultiJCTableData.UpdateTable();
     if (F_Main.PC_1.ActivePage = F_Main.TS_Stanice) then ORsTableData.UpdateTable();
    end;

    HVDb.UpdateTokenTimeout();
    Data.UpdateAutosave();
  except
   on E: Exception do
    begin
     if (not log_err_flag) then
       AppEvents.LogException(E, 'Function timer exception');
    end;
  end;
end;//procedure

procedure TF_Main.T_konfliktyTimer(Sender: TObject);
begin
 if (log) then                                              //zapis do SB1 - cekani 0,5 s
  begin
   F_Main.SB1.Panels.Items[_SB_LOG].Text:='';
   log:=false;
  end;
end;//procedure

procedure TF_Main.FreeVars;
 begin
  try
    MTB.Free;
  except
    on E:Exception do
      AppEvents.LogException(E, 'MTB.Free');
  end;

  ResetData.Free;

  if (Assigned(TrkSystem)) then FreeAndNil(TrkSystem);

  Lic.Free;

  Vytizeni.Free;
  LogData.Free;

  SystemData.Free;
  GetFunctions.Free;
  PrevodySoustav.Free;

  ini_lib.UpdateFile;
  FreeAndNil(ini_lib);

  BoostersDb.Free;

  ORs.Free();
  Blky.Free();
 end;

procedure TF_Main.CB_centrala_loglevelChange(Sender: TObject);
begin
 TrkSystem.loglevel := Self.CB_centrala_loglevel.ItemIndex;
end;

procedure TF_Main.CHB_centrala_fileClick(Sender: TObject);
begin
 TrkSystem.logfile := Self.CHB_centrala_file.Checked;
end;

procedure TF_Main.CHB_centrala_tableClick(Sender: TObject);
begin
 TrkSystem.logtable := Self.CHB_centrala_table.Checked;
end;

procedure TF_Main.CloseForm;
 begin
  WriteLog('########## Probíhá ukonèování hJOPserver ##########',WR_MESSAGE);

  Self.Timer1.Enabled         := false;
  Self.T_function.Enabled     := false;
  self.T_konflikty.Enabled    := false;
  JCSimulator.timer.Enabled   := false;
  TratSimulator.timer.Enabled := false;
  VyhSimulator.timer.Enabled  := false;

  Self.A_SaveStavExecute(Self);

  try
    FreeVars();
  except
    on E:Exception do
      AppEvents.LogException(E, 'FreeVars');
  end;
  WriteLog('###############################################',WR_MESSAGE);
 end;

procedure TF_Main.PM_system_resetClick(Sender: TObject);
begin
 Self.A_System_StartExecute(Self);
 Self.A_System_StopExecute(Self);
end;

function TLogData.CreateLogDirectories:boolean;
 begin
  Result := true;

  if not DirectoryExists('log\') then
    if not CreateDir('log\') then
     begin
      raise Exception.Create('Nelze vytvoøit složku log');
      Result := false;
     end;

  if not DirectoryExists('log\program') then
    if not CreateDir('log\program') then
     begin
      raise Exception.Create('Nelze vytvoøit složku log\program');
      Result := false;
     end;

  if not DirectoryExists('log\lnet') then
    if not CreateDir('log\lnet') then
     begin
      writelog('ERR: Nelze vytvoøit složku log\lnet', WR_ERROR);
      Result := false;
     end;
 end;

procedure TF_Main.CreateClasses;
 begin
  ini_lib        := TMeminifile.Create('inidata.ini');
  MTB            := TMTB.Create();
  ResetData      := TReset.Create;
  Lic            := TLicence.Create;
  Vytizeni       := TVytizeni.Create;
  LogData        := TLogData.Create;
  SystemData     := TSystem.Create;
  GetFunctions   := TGetFunctions.Create;
  PrevodySoustav := TPrevody.Create;
  BoostersDb     := TBoosterDb.Create();

  ACTableData    := TACTableData.Create(Self.LV_AC_Db);
  JCTableData    := TJCTableData.Create(Self.LV_JC);
  UsersTableData := TUsersTableData.Create(Self.LV_Users);
  MTBTableData   := TMTBTableData.Create(Self.LV_Stav_MTB);
  SprTableData   := TSprTableData.Create(Self.LV_Soupravy);
  HVTableData    := THVTableData.Create(Self.LV_HV);
  ZesTableData   := TZesTableData.Create(Self.LV_Zesilovace);
  ORsTableData   := TORsTableData.Create(Self.LV_Stanice);
  MultiJCTableData := TMultiJCTableData.Create(Self.LV_MultiJC);

  ORs := TORs.Create();
  Blky := TBlky.Create();
 end;

procedure TF_Main.RepaintObjects;
 begin
  SB1.Top:=F_Main.ClientWidth-SB1.Width;
  SB1.Panels.Items[0].Width:=F_Main.ClientWidth-SB1.Panels.Items[1].Width-SB1.Panels.Items[2].Width-
  SB1.Panels.Items[3].Width-SB1.Panels.Items[4].Width-SB1.Panels.Items[5].Width;
  P_Zrychleni.Left:=F_Main.ClientWidth-P_Zrychleni.Width-5;
  P_Time_modelovy.Left:=P_Zrychleni.Left-P_Time_modelovy.Width-5;
  P_Time.Left:=P_Time_modelovy.Left-P_Time.Width-5;
  P_Date.Left:=P_Time.Left-P_Date.Width-5;

  P_Soupravy_Tlc.Left  := (PC_1.Width div 2)-(P_Soupravy_Tlc.Width div 2);
  P_Zes_Tlc.Left       := (PC_1.Width div 2)-(P_Zes_Tlc.Width div 2);
  P_Users_Tlc.Left     := (PC_1.Width div 2)-(P_Users_Tlc.Width div 2);
  P_BlkTlc.Left        := (PC_1.Width div 2)-(P_BlkTlc.Width div 2);
  P_HV_Tlac.Left       := (PC_1.Width div 2)-(P_HV_Tlac.Width div 2);

  P_Zes_Vysvetlivky.Left := PC_1.Width - P_Zes_Vysvetlivky.Width-15;
  P_HV_Stats.Left        := PC_1.Width - P_HV_Stats.Width-15;
  P_Blk_Ostatni.Left     := PC_1.Width - P_Blk_Ostatni.Width - 15;

  GB_Centrala.Top := TS_Technologie.ClientHeight - GB_Centrala.Height - 10;
  GB_Connected_Panels.Height := GB_Centrala.Top - GB_Connected_Panels.Top - 10;
end;//procedure

procedure TF_Main.FormResize(Sender: TObject);
 begin
  RepaintObjects;
 end;//procedure

procedure TF_Main.L_DateDblClick(Sender: TObject);
begin
 Application.Messagebox('Datum a èas lze nastavit v operaèním systému','Informace',MB_ICONINFORMATION OR MB_OK OR MB_DEFBUTTON1);
end;

procedure TF_Main.MI_DisconnectClick(Sender: TObject);
begin
 if (ORTCPServer.GetClient(F_Main.LV_Clients.ItemIndex) <> nil) then
  begin
   try
     ORTCPServer.DisconnectClient(ORTCPServer.GetClient(Self.LV_Clients.ItemIndex).conn);
   except
     on E:Exception do
       Application.MessageBox(PChar('Výjimka pøi odpojování - '+e.Message), 'Chyba', MB_OK OR MB_ICONWARNING);
   end;
  end;
end;

procedure TF_Main.MI_PropClick(Sender: TObject);
begin
 if (Self.LV_Bloky.Selected <> nil) then
  Self.LV_BlokyDblClick(Self.LV_Bloky);
end;

procedure TF_Main.MI_Save_configClick(Sender: TObject);
begin
  Application.ProcessMessages;
  Screen.Cursor := crHourGlass;

  Data.CompleteSaveToFile;

  Screen.Cursor := crDefault;
end;

procedure TF_Main.MI_TechPropClick(Sender: TObject);
var Blk:TBlk;
 begin
  if (LV_Bloky.Selected = nil) then Exit;
  if (Blky.GetBlkByIndex(Self.LV_Bloky.ItemIndex,Blk) <> 0) then Exit;

  case (Blk.GetGlobalSettings.typ) of
   _BLK_VYH     : F_BlkVyh_tech.OpenForm(Blk as TBlkVyhybka);
   _BLK_USEK, _BLK_TU :
                  F_BlkUsek_tech.OpenForm(Blk as TBlkUsek);
   _BLK_IR      : ;
   _BLK_SCOM    : ;
   _BLK_PREJEZD : ;
   _BLK_TRAT    : F_BlkTrat_tech.OpenForm(Blk as TBlkTrat);
   _BLK_UVAZKA  : ;
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

procedure TF_Main.LoadIniLibData;
 begin
  Self.CHB_Mainlog_File.Checked  := ini_lib.ReadBool('Log','main-file', true);
  Self.CHB_Mainlog_Table.Checked := ini_lib.ReadBool('Log','main-table', true);
 end;//procedure

procedure TF_Main.SetStartVars;
 begin
  CloseMessage := true;
 end;//procedure

procedure TVytizeni.DetekujVytizeniProcesoru;
 begin
  CollectCPUData;
  Vytizeni.Gauge.Progress := Round(GetCPUUsage(GetCPUCount-1)*100);
 end;

procedure TF_Main.PM_MTBDriver_AboutClick(Sender: TObject);
 begin
  MTB.ShowAboutDialog();
 end;//procedure

procedure TF_Main.DetekujAutSpusteniSystemu;
 begin
  if (KomunikacePocitani <> 0) then
   begin
    if (not GetFunctions.GetSystemStart) then
     begin
      F_AutoStartSystems.L_Cas.Caption := FormatDateTime('ss',Now-KomunikaceGo);     
      if (not F_AutoStartSystems.Showing) then
       begin
        WriteLog('Probiha automaticke pripojovani k systemum - t=6s',WR_MESSAGE);
        F_AutoStartSystems.Show;
       end;
      if (KomunikacePocitani = 1) then
       begin
        KomunikaceGo := Now + EncodeTime(0,0,6,0);
        KomunikacePocitani := 2;
       end else begin
        if (Round((Now - KomunikaceGo) * 24 * 3600) = 0) then
         begin
          WriteLog('Automaticke pripojovani k systemum - t=0 - zapinam systemy',WR_MESSAGE);
          F_AutoStartSystems.Close;
          KomunikacePocitani := 0;          
          F_Main.A_System_StartExecute(nil);
         end;
       end;//else not KomunikacePocitani
     end;//if (not GetFunctions.GetSystemStart) and ...
   end;//if KomunikacePocitani <> -1
 end;//procedure

procedure TF_Main.VypisDatumCas;
 begin
  DateTimeToString(OPData.xTime, 'hh:mm:ss', Time);                //cas
  DateTimeToString(OPData.xDate,'dd.mm.yyyy', Now);
  P_Date.Caption := OPData.xDate;
  P_Time.Caption := OPData.xTime;
 end;//procedure

procedure TF_Main.SB_AC_PauseClick(Sender: TObject);
begin
 if (Self.LV_AC_Db.Selected <> nil) then
  begin
   ACDb.ACs[Self.LV_AC_Db.ItemIndex].Pause();
   Self.LV_AC_DbChange(Self.LV_AC_Db, Self.LV_AC_Db.Selected, TItemChange.ctText);
  end;
end;

procedure TF_Main.SB_AC_PlayClick(Sender: TObject);
begin
 if (Self.LV_AC_Db.Selected <> nil) then
  begin
   ACDb.ACs[Self.LV_AC_Db.ItemIndex].Start();
   Self.LV_AC_DbChange(Self.LV_AC_Db, Self.LV_AC_Db.Selected, TItemChange.ctText);
  end;
end;

procedure TF_Main.SB_AC_RepeatClick(Sender: TObject);
var new:boolean;
begin
 if (Self.LV_AC_Db.Selected <> nil) then
  begin
   new := not Self.SB_AC_Repeat.Down;
   Self.SB_AC_Repeat.Down := new;
   Self.SB_AC_Repeat.AllowAllUp := not new;
   ACDb.ACs[Self.LV_AC_Db.ItemIndex].repeating := Self.SB_AC_Repeat.Down;
  end;
end;

procedure TF_Main.SB_AC_StopClick(Sender: TObject);
begin
 if (Self.LV_AC_Db.Selected <> nil) then
  begin
   ACDb.ACs[Self.LV_AC_Db.ItemIndex].Stop();
   Self.LV_AC_DbChange(Self.LV_AC_Db, Self.LV_AC_Db.Selected, TItemChange.ctText);
  end;
end;

procedure TF_Main.OnStart;
 begin
  Vytizeni.DrawCPUGauge;

  writelog('Spuštìn hJOPserver v'+NactiVerzi(application.ExeName)+_VERSION_PR,0,0);
  writelog('----------------------------------------------------------------',WR_MESSAGE);

  if (not CloseMessage) then F_Main.Close;

  F_splash.PB_Prubeh.Position := F_splash.PB_Prubeh.Max;
  F_Splash.AddStav('Temer spusteno...');

  BlokyTableData.LoadTable();
  JCTableData.LoadToTable();
  MTBTableData.LoadToTable();
  UsersTableData.LoadToTable();
  ORsTableData.LoadToTable();

  Self.PC_1.ActivePage := TS_Technologie;

  ORTCPServer.GUIInitTable();

  F_Main.Visible := true;
  F_Licence.ZkontrolujStartLic;

  F_Main.Timer1.Enabled := true;
  F_Main.T_function.Enabled := true;
  F_Main.T_konflikty.Enabled := true;

  if (not CloseMessage) then
   begin
    F_Main.Close;
    Exit;
   end;//if not CloseMessage
 end;//procedure

procedure TF_Main.FormClose(Sender: TObject; var Action: TCloseAction);
 begin
  F_SystemInfo.OpenForm('Probíhá ukládání dat...');
  Application.ProcessMessages;
  CloseForm;
 end;//procedure

procedure TF_Main.SaveFormPosition;
 begin
  Konfigurace.ini := TMemIniFile.Create(F_Options.E_dataload.Text);
  case F_Main.WindowState of
   wsNormal   : Konfigurace.ini.WriteInteger('Application', 'WState',1);
   wsMaximized: Konfigurace.ini.WriteInteger('Application', 'WState',0);
  end;//case
  Konfigurace.ini.WriteInteger('Application', 'Left',F_Main.Left);
  Konfigurace.ini.WriteInteger('Application', 'Top',F_Main.Top);
  Konfigurace.ini.WriteInteger('Application', 'Heigth',F_Main.Height);
  Konfigurace.ini.WriteInteger('Application', 'Width',F_Main.Width);
  Konfigurace.ini.UpdateFile;
  Konfigurace.ini.Free;
 end;//procedure

procedure TF_Main.PM_SaveFormPosClick(Sender: TObject);
 begin
  F_Main.SaveFormPosition;
 end;

procedure TF_Main.CreateSystem;
 begin
  Randomize;

  F_splash.AddStav('Vytváøím datové struktury');
  CreateClasses;
  F_splash.AddStav('Naèítám ini_lib data');
  LoadIniLibData;
  F_splash.AddStav('Vytváøím složky logù');

  try
    LogData.CreateLogDirectories;
  except
    on e:Exception do
      AppEvents.LogException(E);
  end;

  QueryPerformanceFrequency(Vytizeni.LPc);
  SetStartVars;

  //assign events
  MTB.OnOpen  := Self.OnMTBOpen;
  MTB.OnClose := Self.OnMTBClose;
  MTB.OnStart := Self.OnMTBStart;
  MTB.OnStop  := Self.OnMTBStop;

  MTB.OnErrOpen  := Self.OnMTBErrOpen;
  MTB.OnErrClose := Self.OnMTBErrClose;
  MTB.OnErrStart := Self.OnMTBErrStart;
  MTB.OnErrStop  := Self.OnMTBErrStop;

  MTB.OnReady    := Self.ONMTBReady;

  FuncsFyznam.OnChange := Self.OnFuncsVyznamChange;

  F_Main.Caption := 'hJOPserver         v'+NactiVerzi(Application.ExeName)+_VERSION_PR+' (build '+GetLastBuildDate+')';
  F_Main.SB1.Panels.Items[_SB_MTB].Text := 'MTB close';
  RepaintObjects;
 end;//procedure

procedure TVytizeni.DrawCPUGauge;
var cyklus:Integer;
 begin
  Gauge := TGauge.Create(F_Main.SB1);
  Gauge.Parent := F_Main.SB1;
  Gauge.Visible := true;
  Gauge.Left := 0;
  for cyklus := 0 to _SB_PROC-1 do
   begin
    Gauge.Left := Gauge.Left+F_Main.SB1.Panels.Items[cyklus].Width;
   end;//for cyklus
  Gauge.Left := Gauge.Left + 30;
  Gauge.Top := 3;
  Gauge.Height := 16;
  Gauge.Width := F_Main.SB1.Panels.Items[_SB_PROC].Width-30;
  Gauge.Color := clWhite;
  Gauge.ForeColor := clLime;
 end;//procedure

procedure TF_Main.FormPaint(Sender: TObject);
 begin
  Vytizeni.ResizeCPUGauge;
 end;//procedure

procedure TVytizeni.ResizeCPUGauge;
var cyklus,Zleva:Integer;
 begin
  Gauge.Parent  := F_Main.SB1;
  Zleva := 0;
  for cyklus := 0 to _SB_PROC-1 do
   begin
    Zleva := Zleva + F_Main.SB1.Panels.Items[cyklus].Width;
   end;//for cyklus
  Zleva := Zleva + 30;
  Gauge.Left := Zleva;
 end;//procedure

procedure TF_Main.PM_ClientsPopup(Sender: TObject);
var i:Integer;
begin
 for i := 0 to F_Main.PM_Clients.Items.Count-1 do
  F_Main.PM_Clients.Items.Items[i].Enabled := (F_Main.LV_Clients.Selected <> nil) and (ORTCPServer.GetClient(F_Main.LV_Clients.ItemIndex) <> nil);
end;//procedure

procedure TF_Main.PM_ConsoleClick(Sender: TObject);
 begin
  F_Console.Show;
 end;//procedure

////////////////////////////////////////////////////////////////////////////////
//centrala events:

procedure TF_Main.OnCentralaDCCChange(Sender:TObject; state:boolean);
begin
 Blky.SetDCC(state);

 if (state) then
  begin
   //je DCC
   F_Main.S_Intellibox_go.Brush.Color  := clLime;
   Self.LogStatus('DCC: go');

   if (TrkSystem.openned) then
    begin
     F_Main.A_DCC_Go.Enabled   := false;
     F_Main.A_DCC_Stop.Enabled := true;
    end;

   if ((SystemData.Status = starting) and (TrkSystem.isInitOk)) then
     F_Main.A_All_Loko_PrevzitExecute(nil);

   ORTCPServer.DCCStart();
  end else begin

   ORs.BroadcastBottomError('Výpadek signálu DCC !', 'TECHNOLOGIE', TORControlRights.write);

   //neni DCC
   F_Main.S_Intellibox_go.Brush.Color  := clRed;
   Self.LogStatus('DCC: stop');

   if (TrkSystem.openned) then
    begin
     F_Main.A_DCC_Go.Enabled   := true;
     F_Main.A_DCC_Stop.Enabled := false;
    end;

   ORTCPServer.DCCStop();
  end;//else state

end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TF_Main.LV_JCChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  if (LV_JC.Selected <> nil) then
   begin
    B_VC_delete.Enabled := true;

    if (JCDb.GetJCByIndex(LV_JC.ItemIndex).staveni) then
      B_JC_Reset.Enabled := true
    else
      B_JC_Reset.Enabled := false;
   end else begin
    B_VC_delete.Enabled   := false;
    B_JC_Reset.Enabled    := false;
   end;
end;

procedure TF_Main.LV_JCCustomDrawItem(Sender: TCustomListView; Item: TListItem;
  State: TCustomDrawState; var DefaultDraw: Boolean);
begin
 if (Item.SubItems.Count >= 4) then
  begin
   if (Item.SubItems.Strings[3] <> '0') then
     Self.LV_JC.Canvas.Brush.Color := $AAFFFF
   else if (Item.SubItems.Strings[1] = '-6') then
     Self.LV_JC.Canvas.Brush.Color := clAqua
   else if (Item.SubItems.Strings[1] <> '-5') then
     Self.LV_JC.Canvas.Brush.Color := $AAFFAA;
  end;
end;

procedure TF_Main.LV_JCDblClick(Sender: TObject);
begin
  if (LV_JC.Selected <> nil) then
    F_JCEdit.OpenForm(LV_JC.ItemIndex);
end;

procedure TF_Main.LV_JCKeyPress(Sender: TObject; var Key: Char);
begin
 if (Key = #13) then Self.LV_JCDblClick(LV_Bloky);
end;

procedure TF_Main.LV_logCustomDrawItem(Sender: TCustomListView; Item: TListItem;
  State: TCustomDrawState; var DefaultDraw: Boolean);
begin
   LV_log.Canvas.Brush.Color := TColor(LV_log.Items.Item[Item.Index].Data);
end;

procedure TF_Main.LV_log_lnetCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
 if (Item.SubItems.Count < 2) then Exit();

 case (StrToIntDef(Item.SubItems.Strings[0],0)) of
  1:(Sender as TCustomListView).Canvas.Brush.Color := $AAAAFF;
  2:begin
     (Sender as TCustomListView).Canvas.Brush.Color := $EEEEEE;
     if (LeftStr(Item.SubItems.Strings[1], 3) = 'GET') then (Sender as TCustomListView).Canvas.Brush.Color := $FFD0D0;
     if (LeftStr(Item.SubItems.Strings[1], 3) = 'PUT') then (Sender as TCustomListView).Canvas.Brush.Color := $D0FFD0;
     if (LeftStr(Item.SubItems.Strings[1], 3) = 'ERR') then (Sender as TCustomListView).Canvas.Brush.Color := $AAAAFF;
     if (LeftStr(Item.SubItems.Strings[1], 4) = 'WARN') then (Sender as TCustomListView).Canvas.Brush.Color := $AAFFFF;
    end;//case 2
  3:(Sender as TCustomListView).Canvas.Brush.Color := clWhite;
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
end;

procedure TF_Main.LV_MultiJCCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
 if (Item.SubItems.Count >= 4) then
  begin
   if (Item.SubItems.Strings[1] <> '-1') then
     Self.LV_MultiJC.Canvas.Brush.Color := $AAFFFF
   else
     Self.LV_MultiJC.Canvas.Brush.Color := clWhite;
  end;
end;

procedure TF_Main.LV_MultiJCDblClick(Sender: TObject);
begin
 if (Self.LV_MultiJC.Selected <> nil) then
   F_MJCEdit.OpenForm(MultiJCDb.GetJCByIndex(Self.LV_MultiJC.ItemIndex));
end;

procedure TF_Main.LV_MultiJCKeyPress(Sender: TObject; var Key: Char);
begin
 if (Key = #13) then Self.LV_MultiJCDblClick(LV_Bloky);
end;

procedure TF_Main.LV_SoupravyChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
 if ((Self.LV_Soupravy.Selected <> nil) and (Self.LV_Soupravy.Selected.Caption <> '')) then
  Self.B_lok_delete.Enabled := true
 else
  Self.B_lok_delete.Enabled := false;
end;

procedure TF_Main.LV_SoupravyDblClick(Sender: TObject);
begin
 if (Self.LV_Soupravy.Selected <> nil) then
  Soupravy.soupravy[Self.LV_Soupravy.ItemIndex].VezmiVlak();
end;

procedure TF_Main.LV_StaniceChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var OblR:TOR;
begin
 if (Self.LV_Stanice.Selected <> nil) then
  begin
   ORs.GetORByIndex(Self.LV_Stanice.ItemIndex, OblR);
   if (OblR.stack.Count > 0) then Self.B_RemoveStack.Enabled := true else Self.B_RemoveStack.Enabled := false;
  end else begin
   Self.B_RemoveStack.Enabled := false;
  end;
end;

procedure TF_Main.LV_UsersChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
 if (Self.LV_Users.Selected <> nil) then
  Self.B_User_Delete.Enabled := true
 else
  Self.B_User_Delete.Enabled := false;
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
  if (LV_Zesilovace.Selected <> nil) then
   begin
    B_zes_delete.Enabled := true;
   end else begin
    B_zes_delete.Enabled := false;
   end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TF_Main.LogStatus(str:string);
begin
 if (Assigned(Self.LB_Log)) then
  begin
   if (Self.LB_Log.Items.Count > 100) then Self.LB_Log.Clear();   
   Self.LB_Log.Items.Insert(0, FormatDateTime('hh:nn:ss', Now)+ ' : ' + str);
  end;
 writeLog(str, WR_SYSTEM, 0);
end;

procedure TF_Main.LV_AC_DbChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
 if (LV_AC_Db.Selected <> nil) then
  begin
   B_AutRezim_delete.Enabled := not (ACDb.ACs[Self.LV_AC_Db.ItemIndex].running);
   if (ACDb.ACs[Self.LV_AC_Db.ItemIndex].running) then
    begin
     Self.SB_AC_Play.Enabled   := not ACDb.ACs[Self.LV_AC_Db.ItemIndex].running;
     Self.SB_AC_Stop.Enabled   := true;
     Self.SB_AC_Pause.Enabled  := ACDb.ACs[Self.LV_AC_Db.ItemIndex].running;
     Self.SB_AC_Repeat.Enabled := true;
     Self.SB_AC_Repeat.Down    := ACDb.ACs[Self.LV_AC_Db.ItemIndex].repeating;
    end else begin
     Self.SB_AC_Play.Enabled   := ACDb.ACs[Self.LV_AC_Db.ItemIndex].ready;
     Self.SB_AC_Stop.Enabled   := (ACDb.ACs[Self.LV_AC_Db.ItemIndex].ACKrok > -1);
     Self.SB_AC_Pause.Enabled  := false;
     Self.SB_AC_Repeat.Enabled := (ACDb.ACs[Self.LV_AC_Db.ItemIndex].ACKrok > -1);
    end;
  end else begin
   B_AutRezim_delete.Enabled := false;
   Self.SB_AC_Play.Enabled   := false;
   Self.SB_AC_Stop.Enabled   := false;
   Self.SB_AC_Pause.Enabled  := false;
   Self.SB_AC_Repeat.Enabled := false;
  end;

 Self.LoadACKroky();
end;

procedure TF_Main.LV_AC_DbCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
 if (Item.SubItems.Strings[1] = 'running') then
   Self.LV_AC_Db.Canvas.Brush.Color := $FFFFAA
 else if (Item.SubItems.Strings[1] = 'ready') then
   Self.LV_AC_Db.Canvas.Brush.Color := $AAFFAA
 else
   Self.LV_AC_Db.Canvas.Brush.Color := $FFFFFF;
end;

procedure TF_Main.LV_AC_DbDblClick(Sender: TObject);
begin
 if (LV_AC_Db.Selected <> nil) then
   F_AutRezEdit.OpenForm(LV_AC_Db.ItemIndex);
end;

procedure TF_Main.LV_AC_KrokyCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
 if ((Self.LV_AC_Db.Selected = nil) or (ACDb.ACs[Self.LV_AC_Db.ItemIndex].ACKrok <> Item.Index)) then
   Self.LV_AC_Kroky.Canvas.Brush.Color := $FFFFFF
 else
   Self.LV_AC_Kroky.Canvas.Brush.Color := $AAFFFF;
end;

procedure TF_Main.LV_BlokyChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  B_BlkDelete.Enabled := (LV_Bloky.Selected <> nil);
end;

procedure TF_Main.LV_BlokyCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
var Blk:TBlk;
 begin
  if (Blky.GetBlkByIndex(Item.Index,Blk) <> 0) then Exit;

  case (Blk.GetGlobalSettings().typ) of
   _BLK_VYH:begin
    case ((Blk as TBlkVyhybka).Poloha) of
     TVyhPoloha.disabled : LV_Bloky.Canvas.Brush.Color := $CCCCCC;
     TVyhPoloha.none     : LV_Bloky.Canvas.Brush.Color := $E0FFEE;
     TVyhPoloha.plus     : LV_Bloky.Canvas.Brush.Color := $E0FFE0;
     TVyhPoloha.minus    : LV_Bloky.Canvas.Brush.Color := $E0FFE0;
     TVyhPoloha.both     : LV_Bloky.Canvas.Brush.Color := $0000FF;
    end;//case poloha
   end;//_BLK_VYH

  //////////////////////
   _BLK_USEK, _BLK_TU:begin
    case ((Blk as TBlkUsek).Obsazeno) of
     TUsekStav.disabled : LV_Bloky.Canvas.Brush.Color := $CCCCCC;
     TUsekStav.none     : LV_Bloky.Canvas.Brush.Color := $E0FFEE;
     TUsekStav.uvolneno : LV_Bloky.Canvas.Brush.Color := $E0FFE0;
     TUsekStav.obsazeno : LV_Bloky.Canvas.Brush.Color := $66CCFF;
    end;//case Obsazeno
   end;//_BLK_VYH

  //////////////////////
   _BLK_IR:begin
    case ((Blk as TBlkIR).Stav) of
     TIRStav.disabled : LV_Bloky.Canvas.Brush.Color := $CCCCCC;
     TIRStav.none     : LV_Bloky.Canvas.Brush.Color := $E0FFEE;
     TIRStav.uvolneno : LV_Bloky.Canvas.Brush.Color := $E0FFE0;
     TIRStav.obsazeno : LV_Bloky.Canvas.Brush.Color := $66CCFF;
    end;//case Obsazeno
   end;//_BLK_VYH

  //////////////////////
   _BLK_SCOM:begin
    if ((Blk as TBlkSCom).Navest < 0) then
     LV_Bloky.Canvas.Brush.Color := $CCCCCC  // disabled
    else
     LV_Bloky.Canvas.Brush.Color := $FFFFFF;
   end;//_BLK_VYH

  //////////////////////
   _BLK_PREJEZD:begin
    case ((Blk as TBlkPrejezd).Stav.basicStav) of
     TBlkPrjBasicStav.disabled : LV_Bloky.Canvas.Brush.Color := $CCCCCC;
     TBlkPrjBasicStav.none     : LV_Bloky.Canvas.Brush.Color := $66CCFF;
     TBlkPrjBasicStav.otevreno : LV_Bloky.Canvas.Brush.Color := $E0FFE0;
     TBlkPrjBasicStav.vystraha : LV_Bloky.Canvas.Brush.Color := $E0FFE0;
     TBlkPrjBasicStav.uzavreno : LV_Bloky.Canvas.Brush.Color := $E0FFE0;
     TBlkPrjBasicStav.anulace  : LV_Bloky.Canvas.Brush.Color := $FFFFFF;
    end;//case Obsazeno
   end;//_BLK_VYH

  //////////////////////
   _BLK_UVAZKA:begin
    if (not (Blk as TBlkUvazka).enabled) then
     LV_Bloky.Canvas.Brush.Color := $CCCCCC
    else
     LV_Bloky.Canvas.Brush.Color := $FFFFFF;
   end;

  //////////////////////
   _BLK_TRAT:begin
    if ((Blk as TBlkTrat).stav.smer = TTratSmer.disabled) then
     LV_Bloky.Canvas.Brush.Color := $CCCCCC
    else
     LV_Bloky.Canvas.Brush.Color := $FFFFFF;
   end;

  //////////////////////
   _BLK_ZAMEK:begin
    if ((Blk as TBlkZamek).Stav.enabled) then
     begin
      if ((Blk as TBlkZamek).klicUvolnen) then
       LV_Bloky.Canvas.Brush.Color := $AAAAFF
      else
       LV_Bloky.Canvas.Brush.Color := $FFFFFF;
     end else
       LV_Bloky.Canvas.Brush.Color := $CCCCCC
   end;

  //////////////////////
   _BLK_ROZP:begin
    case ((Blk as TBlkRozp).status) of
      TRozpStatus.disabled     : LV_Bloky.Canvas.Brush.Color := $CCCCCC;
      TRozpStatus.not_selected : LV_Bloky.Canvas.Brush.Color := $FFFFFF;
      TRozpStatus.mounting     : LV_Bloky.Canvas.Brush.Color := $FFAAAA;
      TRozpStatus.active       : LV_Bloky.Canvas.Brush.Color := $AAAAFF;
    end;
   end;

  end;//case
end;

procedure TF_Main.LV_BlokyDblClick(Sender: TObject);
var Blk:TBlk;
 begin
  if (LV_Bloky.Selected = nil) then Exit;
  if (Blky.GetBlkByIndex(Self.LV_Bloky.ItemIndex,Blk) <> 0) then Exit;

  case (Blk.GetGlobalSettings.typ) of
   _BLK_VYH     : F_BlkVyhybka.OpenForm(Self.LV_Bloky.ItemIndex);
   _BLK_USEK    : F_BlkUsek.OpenForm(Self.LV_Bloky.ItemIndex);
   _BLK_IR      : F_BlkIR.OpenForm(Self.LV_Bloky.ItemIndex);
   _BLK_SCOM    : F_BlkSCom.OpenForm(Self.LV_Bloky.ItemIndex);
   _BLK_PREJEZD : F_BlkPrejezd.OpenForm(Self.LV_Bloky.ItemIndex);
   _BLK_TRAT, _BLK_UVAZKA : F_BlkTrat.OpenForm(Self.LV_Bloky.ItemIndex);
   _BLK_ZAMEK   : F_BlkZamek.OpenForm(Self.LV_Bloky.ItemIndex);
   _BLK_ROZP    : F_BlkRozp.OpenForm(Self.LV_Bloky.ItemIndex);
   _BLK_TU      : F_BlkTU.OpenForm(Self.LV_Bloky.ItemIndex);
  end;//case
end;

procedure TF_Main.LV_BlokyKeyPress(Sender: TObject; var Key: Char);
begin
 if (Key = #13) then Self.LV_BlokyDblClick(LV_Bloky);
end;

procedure TF_Main.LV_ClientsCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
 if ((Item.SubItems.Strings[0] = 'uzavøeno') or (Item.SubItems.Strings[0] = 'odpojen')) then
   Self.LV_Clients.Canvas.Brush.Color := clWhite
 else if ((Item.SubItems.Strings[0] = 'otevírání') or (Item.SubItems.Strings[0] = 'handshake'))then
   Self.LV_Clients.Canvas.Brush.Color := $CCCCCC
 else if (Item.SubItems.Strings[0] = 'otevøeno') then
   Self.LV_Clients.Canvas.Brush.Color := $E0FFE0
 else
   Self.LV_Clients.Canvas.Brush.Color := $66CCFF;
end;

procedure TF_Main.LV_HVChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
 if ((LV_HV.Selected <> nil) and (not TrkSystem.openned)) then
  begin
   B_HV_Delete.Enabled := true;
  end else begin
   B_HV_Delete.Enabled := false;
  end;//else LV_HV.Selected <> nil
end;

procedure TF_Main.LV_HVCustomDrawItem(Sender: TCustomListView; Item: TListItem;
  State: TCustomDrawState; var DefaultDraw: Boolean);
begin
 if (Item.SubItems.Count > 15) then
  begin
   if ((Item.SubItems.Strings[15] = 'COM ERROR!') or (Item.SubItems.Strings[16] = 'error')) then (Sender as TCustomListView).Canvas.Brush.Color := $AAAAFF;
   if (Item.SubItems.Strings[15] = 'PC') then (Sender as TCustomListView).Canvas.Brush.Color := $AAFFAA;
   if ((Item.SubItems.Strings[15] = 'ukradeno') or (Item.SubItems.Strings[16] = 'progr')) then (Sender as TCustomListView).Canvas.Brush.Color := $AAFFFF;
  end;
end;

procedure TF_Main.LV_HVDblClick(Sender: TObject);
var ret:Integer;
begin
 if (LV_HV.Selected = nil) then Exit();

 if (TrkSystem.openned) then
  begin
   try
    ret := RegCollector.Open(HVDb.HVozidla[StrToInt(Self.LV_HV.Selected.Caption)]);
    if (ret = 1) then
      Application.MessageBox('Dosáhli jste maximálního poètu otevøených regulátorù!', 'Varování', MB_OK OR MB_ICONWARNING);
   except

   end;
  end else begin
   F_HVEdit.OpenForm(HVDB.HVozidla[Integer(LV_HV.Selected.Data^)]);
  end;
end;

procedure TF_Main.LV_HVKeyPress(Sender: TObject; var Key: Char);
begin
 if (Key = #13) then Self.LV_HVDblClick(LV_Bloky);
end;

procedure TF_Main.LV_ZesilovaceCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
 if ((not MTB.Start) or (not BoostersDb.GetBooster(Item.Index).defined)) then
  begin
   LV_Zesilovace.Canvas.Brush.Color := $CCCCCC;
  end else begin
   if (BoostersDb.GetBooster(Item.Index).napajeni = TBoosterSignal.ok) then
    begin
     if (BoostersDb.GetBooster(Item.Index).Zkrat = TBoosterSignal.ok) then
      begin
       LV_Zesilovace.Canvas.Brush.Color := $AAFFAA;
      end else begin
       LV_Zesilovace.Canvas.Brush.Color := $AAAAFF;
      end;
    end else begin
     LV_Zesilovace.Canvas.Brush.Color := $FFAAAA;
    end;
  end;//if not Zarizeni.Start
end;

procedure TF_Main.LV_ZesilovaceDblClick(Sender: TObject);
begin
 if (LV_Zesilovace.Selected <> nil) then
   F_ZesilovacEdit.OpenForm(BoostersDb.GetBooster(LV_Zesilovace.ItemIndex));
end;

procedure TF_Main.LV_ZesilovaceKeyPress(Sender: TObject; var Key: Char);
begin
 if (Key = #13) then Self.LV_ZesilovaceDblClick(LV_Bloky);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_Main.DisableRemoveButtons;
 begin
  B_BlkDelete.Enabled       := false;
  B_HV_Delete.Enabled       := false;
  B_lok_delete.Enabled      := false;
  B_zes_delete.Enabled      := false;
  B_User_Delete.Enabled     := false;
  B_AutRezim_delete.Enabled := false;
  B_VC_delete.Enabled       := false;
  B_JC_Reset.Enabled        := false;
  B_RemoveStack.Enabled     := false;
  B_mJC_Remove.Enabled      := false;

  Self.SB_AC_Play.Enabled   := false;
  Self.SB_AC_Stop.Enabled   := false;
  Self.SB_AC_Pause.Enabled  := false;
  Self.SB_AC_Repeat.Enabled := false;
 end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TF_Main.UpdateCallMethod();
var ev:TNotifyEvent;
begin
  if (Assigned(Self.call_method)) then
   begin
    // toto poradi musi byt zachovano !
    // volani eventu totiz muze zpuosbit Application.ProcessMessages
    ev := Self.call_method;
    Self.call_method := nil;
    ev(self);
   end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_Main.LoadACKroky();
var i:Integer;
    AC:TAC;
    LI:TListItem;
begin
 Self.LV_AC_Kroky.Clear();
 Self.LV_AC_Kroky.Enabled := (Self.LV_AC_Db.Selected <> nil);
 if (Self.LV_AC_Db.Selected = nil) then Exit();
 AC := ACDb.ACs[Self.LV_AC_Db.ItemIndex];

 for i := 0 to AC.kroky.Count-1 do
  begin
   LI := Self.LV_AC_Kroky.Items.Add;
   LI.Caption := IntToStr(i+1);

   if  (AC.kroky[i].command = _AC_CMDTYPE_END) then LI.SubItems.Add('----- Ukonceni AC -----');
   if  (AC.kroky[i].command = _AC_CMDTYPE_JC) then LI.SubItems.Add('Vlakova cesta '+JCDb.GetJCByID(AC.kroky[i].Params[0]).Nazev);
   if ((AC.kroky[i].command = _AC_CMDTYPE_USEK) and (AC.kroky[i].Params[1] = 1)) then LI.SubItems.Add('Cekani na obsazeni useku '+Blky.GetBlkName(AC.kroky[i].Params[0]));
   if ((AC.kroky[i].command = _AC_CMDTYPE_USEK) and (AC.kroky[i].Params[1] = 0)) then LI.SubItems.Add('Cekani na uvolneni useku '+Blky.GetBlkName(AC.kroky[i].Params[0]));
   if  (AC.kroky[i].command = _AC_CMDTYPE_OSV) then LI.SubItems.Add('Zmena osvetleni ve stanici '+ORs.GetORNameByIndex(AC.kroky[i].Params[0]));
   if  (AC.kroky[i].command = _AC_CMDTYPE_TRAT) then LI.SubItems.Add('Nastaveni smeru trati '+Blky.GetBlkName(AC.kroky[i].Params[0]));
   if  (AC.kroky[i].command = _AC_CMDTYPE_DELAY) then LI.SubItems.Add('Cekani '+IntToStr(AC.kroky[i].Params[0])+' sekund');
   if  (AC.kroky[i].command = _AC_CMDTYPE_NAV) then LI.SubItems.Add('Kontrola stavu navestidla '+Blky.GetBlkName(AC.kroky[i].Params[0])+'; navest:'+IntToStr(AC.kroky[i].Params[1]));
  end;//for i
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TF_Main.OnSoundDisabled(Sender:TObject);
begin
 Self.A_All_Loko_OdhlasitExecute(Self);
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit
