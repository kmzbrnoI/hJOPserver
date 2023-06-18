unit FileSystem;

interface

uses
  SysUtils, Classes, Forms, IniFiles, ComCtrls, StrUtils, TechnologieRCS;

const
  _INIDATA_PATHS_DATA_SECTION = 'PathsData';
  _INIDATA_PATHS_STATE_SECTION = 'PathsState';
  _INIDATA_PATHS_LOG_SECTION = 'Log';

type
  EFileNotFound = class(Exception);

  TConfig = class
    autosave_next: TDateTime;

    procedure CreateCfgDirs();
    procedure CompleteLoadFromFile(inidata: TMemIniFile);
    procedure CompleteSaveToFile(inidata: TMemIniFile);
    procedure UpdateAutosave();
  end;

  TGlobalConfig = class
    autosave: Boolean;
    autosave_period: TTime;
    scale: Cardinal;
    ptAutoStart: Boolean;

    procedure LoadCfgFromFile(filename: string);
    procedure LoadCfgPtServer(ini: TMemIniFile);
    procedure SaveCfgToFile(filename: string);
  end;

  TFormData = class
    aFile: String;
    procedure LoadFormData(filename: string);
    procedure SaveFormData(filename: string);
  end;

  function RCSFromIni(ini: TMemIniFile; section: string; key: string; oldboard: string = ''; oldport: string = ''): TRCSAddr;

var
  Config: TConfig;
  GlobalConfig: TGlobalConfig;
  FormData: TFormData;

implementation

uses fSettings, fSplash, fAdminForm, GetSystems, Diagnostics, fMain,
  AreaDb, BlockDb, BoosterDb, SnadnSpusteni, THVDatabase,
  TCPServerPT, Logging, TCPServerPanel, TrainDb, UserDb, ModelovyCas, TMultiJCDatabase,
  DataBloky, FunkceVyznam, UDPDiscover, appEv, Trakce, fTester,
  TechnologieTrakce, TJCDatabase;

procedure TConfig.CreateCfgDirs();
begin
  try
    CreateDir('data');
    CreateDir('lok');
    CreateDir('stav');
  except
    on e: Exception do
      AppEvents.LogException(e);
  end;
end;

procedure TConfig.CompleteLoadFromFile(inidata: TMemIniFile);
var read, read2: string;
begin
  F_Splash.AddStav('Načítám konfiguraci...');

  if (not inidata.SectionExists(_INIDATA_PATHS_DATA_SECTION)) then
    Logging.log(_INIDATA_FN + ': neexistuje sekce ' + _INIDATA_PATHS_DATA_SECTION, llWarning);
  if (not inidata.SectionExists(_INIDATA_PATHS_STATE_SECTION)) then
    Logging.log(_INIDATA_FN + ': neexistuje sekce ' + _INIDATA_PATHS_STATE_SECTION, llWarning);

  read := inidata.ReadString(_INIDATA_PATHS_DATA_SECTION, 'konfigurace', 'data\konfigurace.ini');
  try
    GlobalConfig.LoadCfgFromFile(read);
  except
    on e: Exception do
      AppEvents.LogException(e);
  end;

  F_Options.CHB_Log_console.Checked := inidata.ReadBool(_INIDATA_PATHS_LOG_SECTION, 'console', true);

  F_Splash.AddStav('Načítám uživatele...');
  try
    UsrDB.LoadAll(inidata.ReadString(_INIDATA_PATHS_DATA_SECTION, 'users', 'data\users.ini'),
      inidata.ReadString(_INIDATA_PATHS_STATE_SECTION, 'users', 'stav\users.ini'));
  except
    on e: Exception do
      AppEvents.LogException(e, e.Message);
  end;

  F_Splash.AddStav('Načítám stanice (soubor *.spnl)...');
  read := inidata.ReadString(_INIDATA_PATHS_DATA_SECTION, 'spnl', 'data\stanice.spnl');
  read2 := inidata.ReadString(_INIDATA_PATHS_STATE_SECTION, 'or', 'stav\or.ini');
  try
    Areas.LoadData(read, read2);
  except
    on e: EFileNotFound do
      Log(e.Message, llError, lsData);
    on e: Exception do
      AppEvents.LogException(e);
  end;
  F_Main.E_dataload_spnl.Text := read;

  F_Splash.AddStav('Načítám hnací vozidla...');
  F_Main.E_dataload_HV_dir.Text := inidata.ReadString(_INIDATA_PATHS_DATA_SECTION, 'lok', 'lok');
  F_Main.E_dataload_HV_state.Text := inidata.ReadString(_INIDATA_PATHS_STATE_SECTION, 'lok', 'stav\lok.ini');
  Log('Načítám hnací vozidla - ' + F_Main.E_dataload_HV_dir.Text + '\*', TLogLevel.llInfo, lsData);
  try
    HVDb.LoadFromDir(F_Main.E_dataload_HV_dir.Text, F_Main.E_dataload_HV_state.Text);
  except
    on e: Exception do
      AppEvents.LogException(e);
  end;
  Log('Načteno ' + IntToStr(HVDb.cnt) + ' hnacích vozidel', TLogLevel.llInfo, lsData);

  F_Splash.AddStav('Načítám RCS...');
  Log('Načítám RCS...', TLogLevel.llInfo, lsData);
  try
    RCSi.LoadFromFile(inidata);
  except
    on e: Exception do
      AppEvents.LogException(e);
  end;
  Log('RCS načteno', TLogLevel.llInfo, lsData);

  F_Splash.AddStav('Načítám trakci...');
  Log('Načítám trakci...', TLogLevel.llInfo, lsData);
  try
    TrakceI.LoadFromFile(inidata);
  except
    on e: Exception do
      AppEvents.LogException(e);
  end;
  Log('Trakce načtena', TLogLevel.llInfo, lsData);

  F_Splash.AddStav('Načítám databázi zesilovačů...');
  read := inidata.ReadString(_INIDATA_PATHS_DATA_SECTION, 'zesilovace', 'data\zesilovace.ini');
  try
    Boosters.LoadFromFile(read);
  except
    on e: Exception do
      AppEvents.LogException(e);
  end;
  F_Main.E_dataload_zes.Text := ExtractRelativePath(ExtractFilePath(Application.ExeName), read);

  F_Splash.AddStav('Načítám soupravy...');
  read := inidata.ReadString(_INIDATA_PATHS_STATE_SECTION, 'soupravy', 'stav\soupravy.ini');
  try
    Trains.LoadData(read);
  except
    on e: Exception do
      AppEvents.LogException(e);
  end;
  F_Main.E_dataload_soupr.Text := ExtractRelativePath(ExtractFilePath(Application.ExeName), read);

  F_Splash.AddStav('Načítám databázi bloků...');
  read := inidata.ReadString(_INIDATA_PATHS_DATA_SECTION, 'bloky', 'data\bloky.ini');
  read2 := inidata.ReadString(_INIDATA_PATHS_STATE_SECTION, 'bloky', 'stav\bloky.ini');
  try
    Blocks.LoadFromFile(read, F_Main.E_dataload_spnl.Text, read2);
  except
    on e: Exception do
      AppEvents.LogException(e);
  end;
  F_Main.E_dataload_block.Text := read;
  BlocksTablePainter := TBlocksTablePainter.Create(F_Main.LV_Blocks);

  Trains.UpdateFront();

  F_Splash.AddStav('Načítám databázi jizdních cest...');
  read := inidata.ReadString(_INIDATA_PATHS_DATA_SECTION, 'JC', 'data\JC.ini');
  try
    JCDb.LoadData(read);
  except
    on e: Exception do
      AppEvents.LogException(e);
  end;

  F_Splash.AddStav('Načítám databázi složených jizdních cest...');
  read := inidata.ReadString(_INIDATA_PATHS_DATA_SECTION, 'mJC', 'data\mJC.ini');
  try
    MultiJCDb.LoadData(read);
  except
    on e: Exception do
      AppEvents.LogException(e);
  end;
  F_Main.E_Dataload_multiJC.Text := MultiJCDb.filename;

  F_Splash.AddStav('Načítám databázi FormData...');
  read := inidata.ReadString(_INIDATA_PATHS_STATE_SECTION, 'forms', 'stav\forms.ini');
  try
    FormData.LoadFormData(read);
  except
    on e: Exception do
      AppEvents.LogException(e);
  end;

  F_Splash.AddStav('Načítám vedlejší databáze...');
  TrakceI.LoadSpeedTable('data\rychlosti.csv', F_Options.LV_DigiRych);
end;

procedure TConfig.CompleteSaveToFile(inidata: TMemIniFile);
begin
  inidata.EraseSection(_INIDATA_PATHS_DATA_SECTION);
  inidata.EraseSection(_INIDATA_PATHS_STATE_SECTION);
  Log('Probíha kompletní ukládání dat', TLogLevel.llInfo, lsData);

  try
    Blocks.SaveToFile(F_Main.E_dataload_block.Text);
  except
    on e: Exception do
      AppEvents.LogException(e);
  end;

  try
    RCSi.SaveToFile(inidata);
  except
    on e: Exception do
      AppEvents.LogException(e);
  end;

  try
    TrakceI.SaveToFile(inidata);
  except
    on e: Exception do
      AppEvents.LogException(e);
  end;

  try
    GlobalConfig.SaveCfgToFile(F_Options.E_dataload.Text);
  except
    on e: Exception do
      AppEvents.LogException(e);
  end;

  try
    Boosters.SaveToFile(F_Main.E_dataload_zes.Text);
  except
    on e: Exception do
      AppEvents.LogException(e);
  end;

  try
    JCDb.SaveData(JCDb.filename);
  except
    on e: Exception do
      AppEvents.LogException(e);
  end;

  try
    MultiJCDb.SaveData(MultiJCDb.filename);
  except
    on e: Exception do
      AppEvents.LogException(e);
  end;

  try
    Trains.SaveData(F_Main.E_dataload_soupr.Text);
  except
    on e: Exception do
      AppEvents.LogException(e);
  end;

  try
    FormData.SaveFormData(FormData.aFile);
  except
    on e: Exception do
      AppEvents.LogException(e);
  end;

  try
    UsrDB.SaveData(F_Main.E_Dataload_Users.Text);
    UsrDB.SaveStat(F_Main.E_dataload_users_stat.Text);
  except
    on e: Exception do
      AppEvents.LogException(e);
  end;

  try
    HVDb.SaveData(F_Main.E_dataload_HV_dir.Text);
    HVDb.SaveState(F_Main.E_dataload_HV_state.Text);
  except
    on e: Exception do
      AppEvents.LogException(e);
  end;

  try
    TrakceI.SaveSpeedTable('data/rychlosti.csv');
  except
    on e: Exception do
      AppEvents.LogException(e);
  end;

  try
    F_Admin.B_SaveClick(Self);
  except
    on e: Exception do
      AppEvents.LogException(e);
  end;

  Log('Kompletní ukládání dat dokončeno', TLogLevel.llInfo, lsData);

  try
    inidata.WriteString(_INIDATA_PATHS_DATA_SECTION, 'konfigurace', F_Options.E_dataload.Text);
    inidata.WriteString(_INIDATA_PATHS_DATA_SECTION, 'spnl', F_Main.E_dataload_spnl.Text);
    inidata.WriteString(_INIDATA_PATHS_DATA_SECTION, 'bloky', Blocks.filename);
    inidata.WriteString(_INIDATA_PATHS_STATE_SECTION, 'bloky', Blocks.fstatus);
    inidata.WriteString(_INIDATA_PATHS_DATA_SECTION, 'zesilovace', F_Main.E_dataload_zes.Text);
    inidata.WriteString(_INIDATA_PATHS_DATA_SECTION, 'JC', JCDb.filename);
    inidata.WriteString(_INIDATA_PATHS_DATA_SECTION, 'mJC', MultiJCDb.filename);
    inidata.WriteString(_INIDATA_PATHS_STATE_SECTION, 'soupravy', F_Main.E_dataload_soupr.Text);
    inidata.WriteString(_INIDATA_PATHS_DATA_SECTION, 'users', UsrDB.filenameData);
    inidata.WriteString(_INIDATA_PATHS_STATE_SECTION, 'users', UsrDB.filenameStat);
    inidata.WriteString(_INIDATA_PATHS_DATA_SECTION, 'lok', F_Main.E_dataload_HV_dir.Text);
    inidata.WriteString(_INIDATA_PATHS_STATE_SECTION, 'lok', F_Main.E_dataload_HV_state.Text);
    inidata.WriteBool(_INIDATA_PATHS_LOG_SECTION, 'console', F_Options.CHB_Log_console.Checked);

    var tmpStr: string;
    if (Areas.status_filename = '') then
      tmpStr := 'stav\or_stav.ini'
    else
      tmpStr := Areas.status_filename;

    inidata.WriteString(_INIDATA_PATHS_STATE_SECTION, 'or', tmpStr);
  except
    on e: Exception do
      AppEvents.LogException(e);
  end;

  inidata.UpdateFile();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TGlobalConfig.LoadCfgFromFile(filename: string);
var str: string;
  ini: TMemIniFile;
begin
  Log('Načítám globální konfiguraci: ' + filename, TLogLevel.llInfo, lsData);
  F_Options.E_dataload.Text := filename;

  ini := TMemIniFile.Create(filename, TEncoding.UTF8);
  try
    var interval := ini.ReadInteger('SystemCfg', 'mainTimerIntervalMs', 100);
    case (interval) of
      25: F_Options.LB_Timer.ItemIndex := 0;
      50: F_Options.LB_Timer.ItemIndex := 1;
      100: F_Options.LB_Timer.ItemIndex := 2;
      200: F_Options.LB_Timer.ItemIndex := 3;
      250: F_Options.LB_Timer.ItemIndex := 4;
      500: F_Options.LB_Timer.ItemIndex := 5;
    else
      F_Options.LB_Timer.ItemIndex := 2;
    end;
    F_Options.LB_TimerClick(Self);

    F_Options.CHB_povolit_spusteni.Checked := ini.ReadBool('SystemCfg', 'AutSpusteni', false);
    if (F_Options.CHB_povolit_spusteni.Checked) then
      F_Main.autostart.state := asEnabled;
    GlobalConfig.scale := ini.ReadInteger('SystemCfg', 'scale', 120);

    ModCas.LoadData(ini);
    SS.LoadData(ini);
    FuncNames.ParseWholeList(ini.ReadString('funcsVyznam', 'funcsVyznam', ''));
    F_Main.CHB_RCS_Show_Only_Active.Checked := ini.ReadBool('RCS', 'ShowOnlyActive', false);

    PanelServer.port := ini.ReadInteger('PanelServer', 'port', _DEFAULT_PORT);

    Self.LoadCfgPtServer(ini);

    // autosave
    GlobalConfig.autosave := ini.ReadBool('autosave', 'enabled', true);
    str := ini.ReadString('autosave', 'period', '00:30');
    GlobalConfig.autosave_period := EncodeTime(0, StrToIntDef(LeftStr(str, 2), 1), StrToIntDef(Copy(str, 4, 2), 0), 0);
    if (GlobalConfig.autosave) then
      Config.autosave_next := Now + GlobalConfig.autosave_period;

    // UDP discovery
    UDPdisc := TUDPDiscover.Create(_DISC_DEFAULT_PORT, ini.ReadString('PanelServer', 'nazev', ''),
      ini.ReadString('PanelServer', 'popis', ''));

    try
      diag.LoadData(ini, 'AdminData');
      F_Admin.LoadData(ini);
    except
      on e: Exception do
        AppEvents.LogException(e);
    end;

    Log('Globální konfigurace načtena', TLogLevel.llInfo, lsData);
  finally
    ini.Free();
  end;
end;

procedure TGlobalConfig.LoadCfgPtServer(ini: TMemIniFile);
var strs: TStringList;
  key, str: string;
const _SECTION = 'PTServer';
begin
  try
    PtServer.port := ini.ReadInteger(_SECTION, 'port', _PT_DEFAULT_PORT);
  except
    on e: EPTActive do
      Log('PT ERR: ' + e.Message, llError, lsPT);
  end;

  PtServer.compact := ini.ReadBool(_SECTION, 'compact', _PT_COMPACT_RESPONSE);
  Self.ptAutoStart := ini.ReadBool(_SECTION, 'autoStart', false);

  strs := TStringList.Create();
  try
    ini.ReadSection(_SECTION + 'StaticTokens', strs);
    for key in strs do
    begin
      try
        str := ini.ReadString(_SECTION + 'StaticTokens', key, '');
        if (str <> '') then
          PtServer.AccessTokenAdd(key, str);
      except
        on e: Exception do
          Log('PTserver: nepodařilo se přidat token ' + str + ' (' + e.Message + ')', TLogLevel.llError, lsPT);
      end;
    end;
  finally
    strs.Free();
  end;
end;

procedure TGlobalConfig.SaveCfgToFile(filename: string);
var ini: TMemIniFile;
begin
  ini := TMemIniFile.Create(filename, TEncoding.UTF8);

  try
    ModCas.SaveData(ini);

    ini.EraseSection('SystemCfg');
    ini.WriteInteger('SystemCfg', 'mainTimerIntervalMs', F_Main.T_Main.Interval);
    ini.WriteBool('SystemCfg', 'AutSpusteni', F_Options.CHB_povolit_spusteni.Checked);
    ini.WriteInteger('SystemCfg', 'scale', GlobalConfig.scale);

    SS.SaveData(ini);

    ini.EraseSection('autosave');
    ini.WriteBool('autosave', 'enabled', GlobalConfig.autosave);
    ini.WriteString('autosave', 'period', FormatDateTime('nn:ss', GlobalConfig.autosave_period));

    ini.WriteString('funcsVyznam', 'funcsVyznam', FuncNames.AllNames());
    ini.WriteBool('RCS', 'ShowOnlyActive', F_Main.CHB_RCS_Show_Only_Active.Checked);
  finally
    ini.UpdateFile();
    ini.Free();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TFormData.LoadFormData(filename: String);
var objs: TStrings;
  ini: TMemIniFile;
begin
  Log('Načítám FormData: ' + filename, TLogLevel.llInfo, lsData);

  ini := TMemIniFile.Create(filename, TEncoding.UTF8);
  FormData.aFile := filename;
  objs := TStringList.Create();
  try
    ini.ReadSections(objs); // "[LV_xy]" (must cut "[]")
    for var obj in objs do
    begin
      var aComponent := Application.FindComponent(obj);
      if (aComponent = nil) then
        aComponent := F_Options.FindComponent(obj);
      if (aComponent = nil) then
        aComponent := F_Main.FindComponent(obj);

      if (aComponent <> nil) then
      begin
        if (aComponent is TForm) then
        begin
          var form: TForm := (aComponent as TForm);
          if ((ini.ReadBool(obj, 'maximized', false)) and (TBorderIcon.biMaximize in form.BorderIcons)) then
          begin
            form.WindowState := TWindowState.wsMaximized;
          end else begin
            form.WindowState := wsNormal;
            form.Left := ini.ReadInteger(obj, 'Left', (aComponent as TForm).Left);
            form.Top := ini.ReadInteger(obj, 'Top', (aComponent as TForm).Top);
            form.Width := ini.ReadInteger(obj, 'Width', (aComponent as TForm).Width);
            form.Height := ini.ReadInteger(obj, 'Height', (aComponent as TForm).Height);
          end;
        end;

        if (aComponent is TListView) then
        begin
          var j := 0;
          while (ini.ReadInteger(obj, IntToStr(j), -1) > -1) do
          begin
            if (j < (aComponent as TListView).Columns.Count) then
              (aComponent as TListView).Columns.Items[j].Width := ini.ReadInteger(obj, IntToStr(j), 0);
            j := j + 1;
          end;
        end; // while
      end;
    end; // for i

  finally
    objs.Free();
    ini.Free();
  end;
  Log('FormData úspěšně načtena', TLogLevel.llInfo, lsData);
end;

procedure TFormData.SaveFormData(filename: String);
var ini: TMemIniFile;
begin
  DeleteFile(filename);
  ini := TMemIniFile.Create(filename, TEncoding.UTF8);

  try
    for var i := 0 to F_Options.ComponentCount - 1 do
      if (F_Options.Components[i] is TListView) then
        for var j := 0 to (F_Options.Components[i] as TListView).Columns.Count - 1 do
          ini.WriteInteger(F_Options.Components[i].Name, IntToStr(j), (F_Options.Components[i] as TListView)
            .Columns.Items[j].Width);

    for var i := 0 to F_Main.ComponentCount - 1 do
      if (F_Main.Components[i] is TListView) then
        for var j := 0 to (F_Main.Components[i] as TListView).Columns.Count - 1 do
          ini.WriteInteger(F_Main.Components[i].Name, IntToStr(j), (F_Main.Components[i] as TListView)
            .Columns.Items[j].Width);

    for var i: Integer := 0 to Application.ComponentCount - 1 do
    begin
      if (Application.Components[i] is TForm) then
      begin
        var form: TForm := (Application.Components[i] as TForm);
        if ((form.BorderStyle = bsSizeable) and (form.WindowState <> TWindowState.wsMaximized)) then
        begin
          ini.WriteInteger(form.Name, 'Width', form.Width);
          ini.WriteInteger(form.Name, 'Height', form.Height);
        end;
        if (TBorderIcon.biMaximize in form.BorderIcons) then
        begin
          if (form.WindowState = TWindowState.wsMaximized) then
            ini.WriteBool(Application.Components[i].Name, 'maximized', true);
        end;
        if ((form = F_Main) and (form.WindowState <> TWindowState.wsMaximized)) then
        begin
          ini.WriteInteger(form.Name, 'Left', form.Left);
          ini.WriteInteger(form.Name, 'Top', form.Top);
        end;
      end;
    end;
  finally
    ini.UpdateFile();
    ini.Free();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

// autosave se stara a prubezne ukladani stavu kolejiste do souboru (nikoliv cele konfigurace!)
procedure TConfig.UpdateAutosave();
begin
  if ((GlobalConfig.autosave) and (Now > Self.autosave_next)) then
  begin
    try
      F_Main.A_SaveStavExecute(Self);
    except
      on e: Exception do
        AppEvents.LogException(e, 'Vyjimka pri ukladani stavu kolejiste');
    end;
    Self.autosave_next := Now + GlobalConfig.autosave_period;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function RCSFromIni(ini: TMemIniFile; section: string; key: string; oldboard: string = ''; oldport: string = ''): TRCSAddr;
begin
  var both := ini.ReadString(section, key, '');
  if (both <> '') then
  begin
    Result.Load(both);
  end else begin
    if ((oldboard = '') or (oldport = '')) then
      raise Exception.Create('Unable to load old RCS!');
    Result.board := ini.ReadInteger(section, oldboard, 0);
    Result.port := ini.ReadInteger(section, oldport, 0);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

initialization

GlobalConfig := TGlobalConfig.Create();
Config := TConfig.Create();
FormData := TFormData.Create();

finalization

FreeAndNil(FormData);
FreeAndNil(Config);
FreeAndNil(GlobalConfig);

end.// unit
