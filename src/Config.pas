unit Config;

interface

uses
  SysUtils, Classes, Forms, IniFiles, ComCtrls, StrUtils, RCSc, RCSsc, Spin,
  StdCtrls, Generics.Collections;

const
  _INIDATA_PATHS_DATA_SECTION = 'PathsData';
  _INIDATA_PATHS_STATE_SECTION = 'PathsState';
  _INIDATA_PATHS_LOG_SECTION = 'Log';

type
  EFileNotFound = class(Exception);

  TGlobalConfig = class
  const
    _TIME_DEFAULT_RC: Integer = 3; // seconds
    _TIME_DEFAULT_RC_VC: Integer = 20; // seconds
    _TIME_DEFAULT_RC_PC: Integer = 10; // seconds
    _TIME_DEFAULT_NUZ: Integer = 20; // seconds
    _TIME_DEFAULT_JC_RELEASE_ZAVER: string = '1.0'; // seconds
    _JC_DEFAULT_MAX_MOVING_TURNOUTS: Integer = 4;

  public
    autosave: Boolean;
    autosave_period: TTime;
    scale: Cardinal;
    autosave_next: TDateTime;
    autostart: Boolean;
    consoleLog: Boolean;
    jcMaxMovingTurnouts: Cardinal;

    times: record // in seconds
      rcFree: Cardinal;
      rcVcOccupied: Cardinal;
      rcPcOccupied: Cardinal;
      nuz: Cardinal;
      jcReleaseZaver: TTime;
    end;

    procedure LoadFromFile(filename: string);
    procedure SaveToFile(filename: string);
  end;

  TFormData = class
    aFile: String;
    procedure LoadFormData(filename: string);
    procedure SaveFormData(filename: string);
  end;

  function RCSFromIni(ini: TMemIniFile; section: string; key: string; oldboard: string = ''; oldport: string = ''): TRCSAddr;
  function RCSOptionalFromIni(ini: TMemIniFile; section: string; key: string; oldboard: string = ''; oldport: string = ''): TRCSAddrOptional;
  function RCSOptionalFromUI(enabled: TCheckBox; board: TSpinEdit; port: TSpinEdit; rcs: TList<TRCSAddr> = nil): TRCSAddrOptional;
  procedure RCSOptionalToUI(const addr: TRCSAddrOptional; var enabled: TCheckBox; var board: TSpinEdit; var port: TSpinEdit);

  function RCSsFromIni(ini: TMemIniFile; section: string; key: string; oldboard: string = ''; oldport: string = ''): TRCSsAddr;
  function RCSsOptionalFromIni(ini: TMemIniFile; section: string; key: string; oldboard: string = ''; oldport: string = ''): TRCSsAddrOptional;
  function RCSsOptionalFromUI(enabled: TCheckBox; system: TSpinEdit; module: TSpinEdit; port: TSpinEdit; rcs: TList<TRCSsAddr> = nil): TRCSsAddrOptional;
  procedure RCSsOptionalToUI(const addr: TRCSsAddrOptional; var enabled: TCheckBox; var system: TSpinEdit; var module: TSpinEdit; var port: TSpinEdit);

  procedure CreateCfgDirs();
  procedure CompleteLoadFromFile(inidata: TMemIniFile);
  procedure CompleteSaveToFile(inidata: TMemIniFile);
  procedure UpdateAutosave();

var
  GlobalConfig: TGlobalConfig;
  FormData: TFormData;

implementation

uses fSplash, fAdminForm, GetSystems, Diagnostics, fMain, ownConvert,
  AreaDb, BlockDb, BoosterDb, THVDatabase,
  TCPServerPT, Logging, TCPServerPanel, TrainDb, UserDb, TimeModel, TMultiJCDatabase,
  DataBloky, FunkceVyznam, UDPDiscover, appEv, fTester, TrakceC, TJCDatabase;

procedure CreateCfgDirs();
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

procedure CompleteLoadFromFile(inidata: TMemIniFile);
var read, read2: string;
begin
  F_Splash.AddStav('Načítám konfiguraci...');

  if (not inidata.SectionExists(_INIDATA_PATHS_DATA_SECTION)) then
    Logging.log(_INIDATA_FN + ': neexistuje sekce ' + _INIDATA_PATHS_DATA_SECTION, llWarning);
  if (not inidata.SectionExists(_INIDATA_PATHS_STATE_SECTION)) then
    Logging.log(_INIDATA_FN + ': neexistuje sekce ' + _INIDATA_PATHS_STATE_SECTION, llWarning);

  read := inidata.ReadString(_INIDATA_PATHS_DATA_SECTION, 'konfigurace', 'data\konfigurace.ini');
  try
    GlobalConfig.LoadFromFile(read);
  except
    on e: Exception do
      AppEvents.LogException(e);
  end;

  GlobalConfig.consoleLog := inidata.ReadBool(_INIDATA_PATHS_LOG_SECTION, 'console', true);

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
    RCSs.LoadFromFile(inidata);
  except
    on e: Exception do
      AppEvents.LogException(e);
  end;
  Log('RCS načteno', TLogLevel.llInfo, lsData);

  F_Splash.AddStav('Načítám trakci...');
  Log('Načítám trakci...', TLogLevel.llInfo, lsData);
  try
    trakce.LoadFromFile(inidata);
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
  trakce.LoadSpeedTable('data\rychlosti.csv', F_Main.LV_DigiRych);
end;

procedure CompleteSaveToFile(inidata: TMemIniFile);
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
    RCSs.SaveToFile(inidata);
    RCSs.SaveAllConfigs();
  except
    on e: Exception do
      AppEvents.LogException(e);
  end;

  try
    trakce.SaveToFile(inidata);
  except
    on e: Exception do
      AppEvents.LogException(e);
  end;

  try
    GlobalConfig.SaveToFile(F_Main.E_configFilename.Text);
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
    trakce.SaveSpeedTable('data/rychlosti.csv');
  except
    on e: Exception do
      AppEvents.LogException(e);
  end;

  try
    F_Admin.B_SaveClick(nil);
  except
    on e: Exception do
      AppEvents.LogException(e);
  end;

  Log('Kompletní ukládání dat dokončeno', TLogLevel.llInfo, lsData, True);

  try
    inidata.WriteString(_INIDATA_PATHS_DATA_SECTION, 'konfigurace', F_Main.E_configFilename.Text);
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
    inidata.WriteBool(_INIDATA_PATHS_LOG_SECTION, 'console', GlobalConfig.consoleLog);

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

procedure TGlobalConfig.LoadFromFile(filename: string);
var ini: TMemIniFile;
begin
  Log('Načítám globální konfiguraci: ' + filename, TLogLevel.llInfo, lsData);
  F_Main.E_configFilename.Text := filename;

  ini := TMemIniFile.Create(filename, TEncoding.UTF8);
  try
    F_Main.T_Main.Interval := ini.ReadInteger('SystemCfg', 'mainTimerIntervalMs', 100);
    Self.autostart := ini.ReadBool('SystemCfg', 'AutSpusteni', false);
    if (Self.autostart) then
      F_Main.autostart.state := asEnabled;
    Self.scale := ini.ReadInteger('SystemCfg', 'scale', 120);

    modelTime.LoadData(ini);
    FuncNames.Clear();
    FuncNames.Add(ini.ReadString('funcsVyznam', 'funcsVyznam', ''));

    for var rcsi: Integer := 0 to RCSs._RCSS_MAX do
      F_Main.CHB_RCSs_Show_Only_Active[rcsi].Checked := ini.ReadBool('RCS'+IntToStr(rcsi), 'ShowOnlyActive', false);

    try
      PanelServer.LoadConfig(ini);
    except
      on E:Exception do
        AppEvents.LogException(E, 'PanelServer.LoadConfig');
    end;

    try
      PtServer.LoadConfig(ini);
    except
      on E:Exception do
        AppEvents.LogException(E, 'PtServer.LoadConfig');
    end;

    // autosave
    begin
      Self.autosave := ini.ReadBool('autosave', 'enabled', true);
      var str := ini.ReadString('autosave', 'period', '00:30');
      Self.autosave_period := EncodeTime(0, StrToIntDef(LeftStr(str, 2), 1), StrToIntDef(Copy(str, 4, 2), 0), 0);
      if (Self.autosave) then
        Self.autosave_next := Now + Self.autosave_period;
    end;

    try
      UDPdisc.LoadConfig(ini);
    except
      on E:Exception do
        AppEvents.LogException(E, 'UDPdisc.LoadConfig');
    end;

    try
      diag.LoadData(ini, 'AdminData');
      F_Admin.LoadData(ini);
    except
      on e: Exception do
        AppEvents.LogException(e);
    end;

    Self.times.rcFree := ini.ReadInteger('times', 'rcFree', _TIME_DEFAULT_RC);
    Self.times.rcVcOccupied := ini.ReadInteger('times', 'rcVcOccupied', _TIME_DEFAULT_RC_VC);
    Self.times.rcPcOccupied := ini.ReadInteger('times', 'rcPcOccupied', _TIME_DEFAULT_RC_PC);
    Self.times.nuz := ini.ReadInteger('times', 'nuz', _TIME_DEFAULT_NUZ);
    Self.times.jcReleaseZaver := SecTenthsToTime(ini.ReadString('times', 'jcReleaseZaver', _TIME_DEFAULT_JC_RELEASE_ZAVER));

    Self.jcMaxMovingTurnouts := ini.ReadInteger('JC', 'maxMovingTurnouts', _JC_DEFAULT_MAX_MOVING_TURNOUTS);
    if (Self.jcMaxMovingTurnouts < 1) then
      Self.jcMaxMovingTurnouts := 1;

    Log('Globální konfigurace načtena', TLogLevel.llInfo, lsData);
  finally
    ini.Free();
  end;
end;

procedure TGlobalConfig.SaveToFile(filename: string);
var ini: TMemIniFile;
begin
  ini := TMemIniFile.Create(filename, TEncoding.UTF8);

  try
    PanelServer.SaveConfig(ini);
    modelTime.SaveData(ini);
    UDPdisc.SaveConfig(ini);
    PtServer.SaveConfig(ini);

    ini.EraseSection('SystemCfg');
    ini.WriteInteger('SystemCfg', 'mainTimerIntervalMs', F_Main.T_Main.Interval);
    ini.WriteBool('SystemCfg', 'AutSpusteni', Self.autostart);
    ini.WriteInteger('SystemCfg', 'scale', Self.scale);

    ini.EraseSection('autosave');
    ini.WriteBool('autosave', 'enabled', Self.autosave);
    ini.WriteString('autosave', 'period', FormatDateTime('nn:ss', Self.autosave_period));

    ini.WriteString('funcsVyznam', 'funcsVyznam', FuncNames.PanelStr());

    ini.EraseSection('RCS'); // old section
    for var rcsi: Integer := 0 to RCSs._RCSS_MAX do
      ini.WriteBool('RCS'+IntToStr(rcsi), 'ShowOnlyActive', F_Main.CHB_RCSs_Show_Only_Active[rcsi].Checked);

    ini.WriteInteger('times', 'rcFree', Self.times.rcFree);
    ini.WriteInteger('times', 'rcVcOccupied', Self.times.rcVcOccupied);
    ini.WriteInteger('times', 'rcPcOccupied', Self.times.rcPcOccupied);
    ini.WriteInteger('times', 'nuz', Self.times.nuz);
    ini.WriteString('times', 'jcReleaseZaver', TimeToSecTenths(Self.times.jcReleaseZaver));

    ini.WriteInteger('JC', 'maxMovingTurnouts', Self.jcMaxMovingTurnouts);
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
procedure UpdateAutosave();
begin
  if ((GlobalConfig.autosave) and (Now > GlobalConfig.autosave_next)) then
  begin
    try
      F_Main.A_SaveStavExecute(nil);
    except
      on e: Exception do
        AppEvents.LogException(e, 'Vyjimka pri ukladani stavu kolejiste');
    end;
    GlobalConfig.autosave_next := Now + GlobalConfig.autosave_period;
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
    Result.module := ini.ReadInteger(section, oldboard, 0);
    Result.port := ini.ReadInteger(section, oldport, 0);
  end;
end;

function RCSOptionalFromIni(ini: TMemIniFile; section: string; key: string; oldboard: string; oldport: string): TRCSAddrOptional;
begin
  if ((ini.ReadString(section, key, '') = '') and (ini.ReadString(section, oldboard, '') = '')
      and (ini.ReadString(section, oldport, '') = '')) then
  begin
    Result.enabled := False;
    Result.addr.module := 0;
    Result.addr.port := 0;
    Exit();
  end;

  Result.enabled := True;
  Result.addr := RCSFromIni(ini, section, key, oldboard, oldport);
end;

function RCSOptionalFromUI(enabled: TCheckBox; board: TSpinEdit; port: TSpinEdit; rcs: TList<TRCSAddr>): TRCSAddrOptional;
begin
  Result.enabled := enabled.Checked;
  if (Result.enabled) then
  begin
    Result.addr.module := board.Value;
    Result.addr.port := port.Value;
    if (rcs <> nil) then
      rcs.Add(Result.addr);
  end else begin
    Result.addr.module := 0;
    Result.addr.port := 0;
  end;
end;

procedure RCSOptionalToUI(const addr: TRCSAddrOptional; var enabled: TCheckBox; var board: TSpinEdit; var port: TSpinEdit);
begin
  enabled.Checked := addr.enabled;
  if (Assigned(enabled.OnClick)) then
    enabled.OnClick(enabled);
  if (addr.addr.module > Cardinal(board.MaxValue)) then
    board.MaxValue := 0;
  if (addr.enabled) then
  begin
    board.Value := addr.addr.module;
    port.Value := addr.addr.port;
  end;
end;

function RCSsFromIni(ini: TMemIniFile; section: string; key: string; oldboard: string = ''; oldport: string = ''): TRCSsAddr;
begin
  var triplet := ini.ReadString(section, key, '');
  if (triplet <> '') then
  begin
    Result.Load(triplet);
  end else begin
    if ((oldboard = '') or (oldport = '')) then
      raise Exception.Create('Unable to load old RCS!');
    Result.system := 0;
    Result.module := ini.ReadInteger(section, oldboard, 0);
    Result.port := ini.ReadInteger(section, oldport, 0);
  end;
end;

function RCSsOptionalFromIni(ini: TMemIniFile; section: string; key: string; oldboard: string = ''; oldport: string = ''): TRCSsAddrOptional;
begin
  if ((ini.ReadString(section, key, '') = '') and (ini.ReadString(section, oldboard, '') = '')
      and (ini.ReadString(section, oldport, '') = '')) then
  begin
    Result.enabled := False;
    Result.addr.system := 0;
    Result.addr.module := 0;
    Result.addr.port := 0;
    Exit();
  end;

  Result.enabled := True;
  Result.addr := RCSsFromIni(ini, section, key, oldboard, oldport);
end;

function RCSsOptionalFromUI(enabled: TCheckBox; system: TSpinEdit; module: TSpinEdit; port: TSpinEdit; rcs: TList<TRCSsAddr> = nil): TRCSsAddrOptional;
begin
  Result.enabled := enabled.Checked;
  if (Result.enabled) then
  begin
    Result.addr.system := system.Value;
    Result.addr.module := module.Value;
    Result.addr.port := port.Value;
    if (rcs <> nil) then
      rcs.Add(Result.addr);
  end else begin
    Result.addr.system := 0;
    Result.addr.module := 0;
    Result.addr.port := 0;
  end;
end;

procedure RCSsOptionalToUI(const addr: TRCSsAddrOptional; var enabled: TCheckBox; var system: TSpinEdit; var module: TSpinEdit; var port: TSpinEdit);
begin
  enabled.Checked := addr.enabled;
  if (Assigned(enabled.OnClick)) then
    enabled.OnClick(enabled);
  if (addr.enabled) then
  begin
    system.Value := addr.addr.system;
    module.Value := addr.addr.module;
    port.Value := addr.addr.port;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

initialization

GlobalConfig := TGlobalConfig.Create();
FormData := TFormData.Create();

finalization

FreeAndNil(FormData);
FreeAndNil(GlobalConfig);

end.// unit
