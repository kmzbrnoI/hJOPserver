unit FileSystem;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, IniFiles, ComCtrls, ExtCtrls, StdCtrls, StrUtils,
  fMain, Trakce, TrakceGUI, XpressNET, CPort, AC, TJCDatabase;

const
  _INIDATA_PATHS_DATA_SECTION = 'PathsData';
  _INIDATA_PATHS_STATE_SECTION = 'PathsState';

type
  EFileNotFound = class(Exception);

  TData=class
   autosave:boolean;
   autosave_period:TTime;
   autosave_next:TDateTime;

    procedure CompleteSaveToFile;
    procedure CompleteLoadFromFile;
    procedure UpdateAutosave();
  end;

  TKonfigurace=class
    procedure LoadCfgFromFile(IniLoad:string);
    procedure SaveCfgToFile(IniSave:string);
  end;

  TFormData=class
   aFile:String;
    procedure LoadFormData(IniLoad:string);
    procedure SaveFormData(IniSave:string);
  end;

var
  Data:TData;
  Konfigurace:TKonfigurace;
  FormData:TFormData;


implementation

uses fSettings, fSplash, fAdminForm, GetSystems, Prevody,
     TechnologieRCS, Verze, fHVEdit, TechnologieJC, fConsole, TOblsRizeni, TBloky,
     TBlok, TBlokUsek, TBlokVyhybka, TBlokNav, TBlokIR, TOblRizeni, BoosterDb,
     Booster, SnadnSpusteni, TBlokPrejezd, THVDatabase, TCPServerPT,
     Logging, TCPServerOR, SprDb, UserDb, ModelovyCas, TMultiJCDatabase,
     DataBloky, ACDatabase, FunkceVyznam, UDPDiscover, appEv;

procedure TData.CompleteLoadFromFile;
var read,read2:string;
 begin
  F_Splash.AddStav('Naèítám konfiguraci');
  read := ini_lib.ReadString(_INIDATA_PATHS_DATA_SECTION, 'konfigurace', 'data\konfigurace.ini');
  try
    Konfigurace.LoadCfgFromFile(read);
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;

  F_Splash.AddStav('Naèítám uživatele');
  try
    UsrDB.LoadAll(
      ini_lib.ReadString(_INIDATA_PATHS_DATA_SECTION, 'users', 'data\users.ini'),
      ini_lib.ReadString(_INIDATA_PATHS_STATE_SECTION, 'users', 'stav\users.ini')
    );
  except
    on E:Exception do
      AppEvents.LogException(E, E.Message);
  end;

  F_Splash.AddStav('Naèítám stanice (soubor *.spnl)');
  read  := ini_lib.ReadString(_INIDATA_PATHS_DATA_SECTION, 'spnl', 'data\stanice.spnl');
  read2 := ini_lib.ReadString(_INIDATA_PATHS_STATE_SECTION, 'or', 'stav\or.ini');
  try
    ORs.LoadData(read, read2);
  except
    on E:EFileNotFound do
      writelog(E.Message, WR_ERROR);
    on E:Exception do
      AppEvents.LogException(E);
  end;
  F_Main.E_dataload_spnl.Text := read;

  F_Splash.AddStav('Naèítám hnací vozidla');
  F_Main.E_dataload_HV_dir.Text := ini_lib.ReadString(_INIDATA_PATHS_DATA_SECTION, 'lok', 'lok');
  F_Main.E_dataload_HV_state.Text := ini_lib.ReadString(_INIDATA_PATHS_STATE_SECTION, 'lok', 'stav\lok.ini');
  writelog('Naèítám hnací vozidla - '+F_Main.E_dataload_HV_dir.Text+'\*', WR_DATA);
  try
    HVDb.LoadFromDir(F_Main.E_dataload_HV_dir.Text, F_Main.E_dataload_HV_state.Text);
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;
  writelog('Naèteno '+IntToStr(HVDb.cnt)+' hnacích vozidel',WR_DATA);

  F_Splash.AddStav('Naèítám RCS');
  writelog('Nacitam RCS...', WR_DATA);
  try
    RCSi.LoadFromFile(ini_lib);
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;
  writelog('RCS nacteno',WR_DATA);

  F_Splash.AddStav('Naèítám databázi zesilovaèù');
  read := ini_lib.ReadString(_INIDATA_PATHS_DATA_SECTION, 'zesilovace', 'data\zesilovace.ini');
  try
    Boosters.LoadFromFile(read);
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;
  F_Main.E_dataload_zes.Text := ExtractRelativePath(ExtractFilePath(Application.ExeName),read);

  F_Splash.AddStav('Naèítám soupravy');
  read := ini_lib.ReadString(_INIDATA_PATHS_STATE_SECTION, 'soupravy', 'stav\soupravy.ini');
  try
    Soupravy.LoadData(read);
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;
  F_Main.E_dataload_soupr.Text := ExtractRelativePath(ExtractFilePath(Application.ExeName), read);

  //nacitani bloku
  F_Splash.AddStav('Naèítám databázi blokù');
  read := ini_lib.ReadString(_INIDATA_PATHS_DATA_SECTION, 'bloky', 'data\bloky.ini');
  read2 := ini_lib.ReadString(_INIDATA_PATHS_STATE_SECTION, 'bloky', 'stav\bloky.ini');
  try
    Blky.LoadFromFile(read, F_Main.E_dataload_spnl.Text, read2);
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;
  F_Main.E_dataload_block.Text := read;
  BlokyTableData := TBlokyTableData.Create(F_Main.LV_Bloky);

  Soupravy.UpdateFront();

  F_Splash.AddStav('Naèítám databázi jizdních cest');
  read := ini_lib.ReadString(_INIDATA_PATHS_DATA_SECTION, 'JC', 'data\JC.ini');
  try
    JCDb.LoadData(read);
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;

  F_Splash.AddStav('Naèítám databázi složených jizdních cest');
  read := ini_lib.ReadString(_INIDATA_PATHS_DATA_SECTION, 'mJC', 'data\mJC.ini');
  try
    MultiJCDb.LoadData(read);
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;
  F_Main.E_Dataload_multiJC.Text := MultiJCDb.filename;

  F_Splash.AddStav('Naèítám databázi automatických režimù');
  read := ini_lib.ReadString(_INIDATA_PATHS_DATA_SECTION, 'AC', 'AC');
  read2 := ini_lib.ReadString(_INIDATA_PATHS_STATE_SECTION, 'AC', 'stav\AC.ini');
  try
    ACDb.LoadFromDir(read);
    ACDb.LoadStatFromFile(read2);
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;
  F_Main.E_dataload_AC.Text := ExtractRelativePath(ExtractFilePath(Application.ExeName), ACDb.dirname);

  F_Splash.AddStav('Naèítám databázi FormData');
  read := ini_lib.ReadString(_INIDATA_PATHS_STATE_SECTION, 'forms', 'stav\forms.ini');
  try
    FormData.LoadFormData(read);
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;

  F_Splash.AddStav('Naèítám vedlejší databáze');
  TrkSystem.LoadSpeedTable('data\rychlosti.csv',F_Options.LV_DigiRych);
  try
    F_Admin.LoadData;
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;
 end;

procedure TData.CompleteSaveToFile;
var tmpStr:string;
 begin
  ini_lib.EraseSection(_INIDATA_PATHS_DATA_SECTION);
  ini_lib.EraseSection(_INIDATA_PATHS_STATE_SECTION);
  WriteLog('Probíha kompletní ukládání dat', WR_DATA);

  try
    Blky.SaveToFile(F_Main.E_dataload_block.Text);
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;

  try
    RCSi.SaveToFile(ini_lib);
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;

  try
    Konfigurace.SaveCfgToFile(F_Options.E_dataload.Text);
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;

  try
    ACDb.SaveStatToFile(ACDb.statfilename);
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;

  try
    Boosters.SaveToFile(F_Main.E_dataload_zes.Text);
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;

  try
    JCDb.SaveData(JCDb.filename);
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;

  try
    MultiJCDb.SaveData(MultiJCDb.filename);
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;

  try
    Soupravy.SaveData(F_Main.E_dataload_soupr.Text);
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;

  try
    FormData.SaveFormData(FormData.aFile);
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;

  try
    UsrDB.SaveData(F_Main.E_Dataload_Users.Text);
    UsrDB.SaveStat(F_Main.E_dataload_users_stat.Text);
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;

  try
    HVDb.SaveData(F_Main.E_dataload_HV_dir.Text);
    HVDb.SaveState(F_Main.E_dataload_HV_state.Text);
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;

  try
    TrkSystem.SaveSpeedTable('data/rychlosti.csv');
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;

  try
    F_Admin.SaveData;
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;

  WriteLog('Kompletni ukladani dat dokonceno',WR_DATA);

  try
    ini_lib.WriteString(_INIDATA_PATHS_DATA_SECTION, 'spnl', F_Main.E_dataload_spnl.Text);
    ini_lib.WriteString(_INIDATA_PATHS_DATA_SECTION, 'bloky', Blky.blky_file);
    ini_lib.WriteString(_INIDATA_PATHS_STATE_SECTION, 'bloky', Blky.fstatus);
    ini_lib.WriteString(_INIDATA_PATHS_DATA_SECTION, 'zesilovace', F_Main.E_dataload_zes.Text);
    ini_lib.WriteString(_INIDATA_PATHS_DATA_SECTION, 'JC', JCDb.filename);
    ini_lib.WriteString(_INIDATA_PATHS_DATA_SECTION, 'mJC', MultiJCDb.filename);
    ini_lib.WriteString(_INIDATA_PATHS_STATE_SECTION, 'soupravy', F_Main.E_dataload_soupr.Text);
    ini_lib.WriteString(_INIDATA_PATHS_DATA_SECTION, 'users', UsrDB.filenameData);
    ini_lib.WriteString(_INIDATA_PATHS_STATE_SECTION, 'users', UsrDB.filenameStat);
    ini_lib.WriteString(_INIDATA_PATHS_DATA_SECTION, 'AC', ACDb.dirname);
    ini_lib.WriteString(_INIDATA_PATHS_STATE_SECTION, 'AC', ACDb.statfilename);
    ini_lib.WriteString(_INIDATA_PATHS_DATA_SECTION, 'lok', F_Main.E_dataload_HV_dir.Text);
    ini_lib.WriteString(_INIDATA_PATHS_STATE_SECTION, 'lok', F_Main.E_dataload_HV_state.Text);

    if (ORs.status_filename = '') then
      tmpStr := 'stav\or_stav.ini'
    else
      tmpStr := ORs.status_filename;

    ini_lib.WriteString(_INIDATA_PATHS_STATE_SECTION, 'or', tmpStr);
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;

  ini_lib.UpdateFile();
 end;

////////////////////////////////////////////////////////////////////////////////

procedure TKonfigurace.LoadCfgFromFile(IniLoad:string);
var str:string;
    system:Ttrk_system;
    ini:TMemIniFile;
 begin
  writelog('Naèítám konfiguraci - '+IniLoad,WR_DATA);
  F_Options.E_dataload.Text := IniLoad;
  ini := TMemIniFile.Create(IniLoad, TEncoding.UTF8);
  try
    //nastaveni defaultnich cest pro Open/Save dialogy
    F_Options.OD_Open.InitialDir := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))+'data\';
    F_Options.SD_Save.InitialDir := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))+'data\';

    //nacitani dat o programu - left, top...
    case ini.ReadInteger('Application','WState',0) of
     0:begin
        F_Main.WindowState := wsMaximized;
       end;
     1:begin
        F_Main.Left   := ini.ReadInteger('Application', 'Left',200);
        F_Main.Top    := ini.ReadInteger('Application', 'Top',200);
        F_Main.Height := ini.ReadInteger('Application', 'Heigth',500);
        F_Main.Width  := ini.ReadInteger('Application', 'Width',1125);
        F_Main.WindowState := wsNormal;
       end;
    end;//case

    //nacitani dat o konzoli
    F_Options.CHB_Log_console.Checked := ini_lib.ReadBool('Log','Log_console',true);

    str := ini.ReadString('Centrala', 'system', 'xpressnet');
    if (str = 'simulator') then
     system := TRS_Simulator
    else
     system := TRS_XpressNet;

    TrkSystem := TTrkGUI.Create(
     system,
     F_Main.LV_log_lnet,
     TTrkLogLevel(ini.ReadInteger('Centrala', 'logfile', 2)),
     TTrkLogLevel(ini.ReadInteger('Centrala', 'logtable', 2))
    );

    TrkSystem.BaudRate := TBaudRate(ini.ReadInteger('Centrala','BaudRate',0));
    TrkSystem.StopBits := TStopBits(ini.ReadInteger('Centrala','StopBits',0));
    TrkSystem.DataBits := TDataBits(ini.ReadInteger('Centrala','DataBits',0));
    TrkSystem.FlowControl := TFlowControl(ini.ReadInteger('Centrala', 'FlowControl', 0));
    TrkSystem.COM := ini.ReadString('Centrala','COM','COM1');

    F_Main.CB_centrala_loglevel_file.ItemIndex  := Integer(TrkSystem.logfile);
    F_Main.CB_centrala_loglevel_table.ItemIndex := Integer(TrkSystem.logtable);

    //nactitani dalsich dat
    F_Options.LB_Timer.ItemIndex := ini.ReadInteger('SystemCfg','TimerInterval',4);
    F_Options.LB_TimerClick(Self);
    F_Options.CHB_povolit_spusteni.Checked := ini.ReadBool('SystemCfg','AutSpusteni',false);
    if (F_Options.CHB_povolit_spusteni.Checked) then F_Main.KomunikacePocitani := 1;

    //nacitani modeloveho casu
    ModCas.LoadData(ini);

    SS.LoadData(ini);

    ORTCPServer.port := ini.ReadInteger('PanelServer', 'port', _PANEL_DEFAULT_PORT);

    try
      PtServer.port := ini.ReadInteger('PTServer', 'port', _PT_DEFAULT_PORT);
    except
      on E:EPTActive do
        writeLog('PT ERR: '+E.Message, WR_PT);
    end;

    PtServer.compact := ini.ReadBool('PTServer', 'compact', _PT_COMPACT_RESPONSE);

    // autosave

    Data.autosave := ini.ReadBool('autosave', 'enabled', true);
    str := ini.ReadString('autosave', 'period', '00:30');
    Data.autosave_period := EncodeTime(0, StrToIntDef(LeftStr(str, 2), 1), StrToIntDef(Copy(str, 4, 2), 0), 0);
    if (data.autosave) then
      Data.autosave_next := Now + Data.autosave_period;

    // nacteni vyznamu funkci
    FuncsFyznam.ParseWholeList(ini.ReadString('funcsVyznam', 'funcsVyznam', ''));
    F_Main.CHB_RCS_Show_Only_Active.Checked := ini.ReadBool('RCS', 'ShowOnlyActive', false);

    // nacteni UDP discovery
    UDPdisc := TUDPDiscover.Create(_DISC_DEFAULT_PORT,
         ini.ReadString('PanelServer', 'nazev', ''),
         ini.ReadString('PanelServer', 'popis', ''));

    writelog('Konfigurace naètena', WR_DATA);
  finally
    ini.Free();
  end;
 end;

procedure TKonfigurace.SaveCfgToFile(IniSave:string);
var ini:TMemIniFile;
 begin
  ini_lib.WriteString(_INIDATA_PATHS_DATA_SECTION, 'konfigurace', IniSave);
  ini := TMemIniFile.Create(IniSave, TEncoding.UTF8);

  try
    ModCas.SaveData(ini);

    ini.WriteInteger('SystemCfg', 'TimerInterval', F_Options.LB_Timer.ItemIndex);
    ini.WriteBool('SystemCfg', 'AutSpusteni', F_Options.CHB_povolit_spusteni.Checked);

    ini.WriteInteger('Centrala', 'BaudRate', Integer(TrkSystem.BaudRate));
    ini.WriteInteger('Centrala', 'StopBits', Integer(TrkSystem.StopBits));
    ini.WriteInteger('Centrala', 'DataBits', Integer(TrkSystem.DataBits));
    ini.WriteInteger('Centrala', 'FlowControl', Integer(TrkSystem.FlowControl));
    ini.Writestring('Centrala', 'COM', TrkSystem.COM);

    if (TrkSystem.TrkSystem = TRS_LocoNET) then
      ini.WriteString('Centrala', 'system', 'loconet')
    else if (TrkSystem.TrkSystem = TRS_Simulator) then
      ini.WriteString('Centrala', 'system', 'simulator')
    else
      ini.WriteString('Centrala', 'system', 'xpressnet');

    ini.WriteInteger('Centrala', 'logtable', Integer(TrkSystem.logtable));
    ini.WriteInteger('Centrala', 'logfile', Integer(TrkSystem.logfile));

    ini_lib.WriteBool('Log', 'Log_console', F_Options.CHB_Log_console.Checked);
    ini.WriteInteger('AdminData', 'FormLeft', F_Admin.Left);
    ini.WriteInteger('AdminData', 'FormTop', F_Admin.Top);

    ini.WriteInteger('PanelServer', 'port', ORTCPServer.port);
    ini.WriteString('PanelServer', 'nazev', UDPdisc.name);
    ini.WriteString('PanelServer', 'popis', UDPdisc.description);

    ini.WriteInteger('PTServer', 'port', PtServer.port);
    ini.WriteBool('PTServer', 'compact', PtServer.compact);

    SS.SaveData(ini);

    // autosave
    ini.WriteBool('autosave', 'enabled', Data.autosave);
    ini.WriteString('autosave', 'period', FormatDateTime('nn:ss', Data.autosave_period));

    // ulozeni vyznamu funkci
    ini.WriteString('funcsVyznam', 'funcsVyznam', FuncsFyznam.GetFuncsVyznam());
    ini.WriteBool('RCS', 'ShowOnlyActive', F_Main.CHB_RCS_Show_Only_Active.Checked);
  finally
    ini.UpdateFile();
    ini.Free();
  end;
 end;

////////////////////////////////////////////////////////////////////////////////

procedure TFormData.LoadFormData(IniLoad:String);
var j:Integer;
    objs:TStrings;
    obj, rawObj:String;
    aComponent:TComponent;
    ini:TMemIniFile;
 begin
  writelog('Naèítám FormData - '+IniLoad,WR_DATA);

  ini := TMemIniFile.Create(IniLoad, TEncoding.UTF8);
  FormData.aFile := IniLoad;
  objs := TStringList.Create();
  try
    ini.GetStrings(objs); // "[LV_xy]" (must cut "[]")
    for obj in objs do
     begin
      j := 0;
      rawObj := Copy(obj, 2, Length(obj)-2);
      while (ini.ReadInteger(rawObj, IntToStr(j), -1) > -1) do
       begin
        aComponent := F_Options.FindComponent(rawObj);
        if (AComponent = nil) then aComponent := F_Main.FindComponent(rawObj);

        if (aComponent <> nil) then
          if (j < (aComponent as TListView).Columns.Count) then
            (aComponent as TListView).Columns.Items[j].Width := ini.ReadInteger(rawObj, IntToStr(j),0);

        j := j + 1;
       end;//while
     end;//for i
  finally
    objs.Free();
    ini.Free();
  end;
  writelog('FormData úspìšnì naètena', WR_DATA);
 end;

procedure TFormData.SaveFormData(IniSave:String);
var i, j:Integer;
    ini:TMemIniFile;
 begin
  DeleteFile(IniSave);
  ini := TMemIniFile.Create(IniSave, TEncoding.UTF8);

  try
    for i := 0 to F_Options.ComponentCount-1 do
      if (F_Options.Components[i].ClassType = TListView) then
        for j := 0 to (F_Options.Components[i] as TListView).Columns.Count-1 do
          ini.WriteInteger(F_Options.Components[i].Name, IntToStr(j), (F_Options.Components[i] as TListView).Columns.Items[j].Width);

    for i := 0 to F_Main.ComponentCount-1 do
      if (F_Main.Components[i].ClassType = TListView) then
        for j := 0 to (F_Main.Components[i] as TListView).Columns.Count-1 do
          ini.WriteInteger(F_Main.Components[i].Name, IntToStr(j), (F_Main.Components[i] as TListView).Columns.Items[j].Width);
  finally
    ini.UpdateFile();
    ini.Free();
  end;
 end;

////////////////////////////////////////////////////////////////////////////////

// autosave se stara a prubezne ukladani stavu kolejiste do souboru (nikoliv cele konfigurace!)
procedure TData.UpdateAutosave();
begin
 if ((Self.autosave) and (Now > Self.autosave_next)) then
  begin
   try
     F_Main.A_SaveStavExecute(Self);
   except
     on E:Exception do
       AppEvents.LogException(E, 'Vyjimka pri ukladani stavu kolejiste');
   end;
   Self.autosave_next := Now + Self.autosave_period;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

//************KONEC UNITY************

initialization
  Data                := TData.Create;
  Konfigurace         := TKonfigurace.Create;
  FormData            := TFormData.Create;

finalization
  FreeAndNil(Data);
  FreeAndNil(Konfigurace);
  FreeAndNil(FormData);


end.//unit

