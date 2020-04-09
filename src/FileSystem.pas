unit FileSystem;

interface

uses
  SysUtils, Classes, Forms, IniFiles, ComCtrls, StrUtils;

const
  _INIDATA_PATHS_DATA_SECTION = 'PathsData';
  _INIDATA_PATHS_STATE_SECTION = 'PathsState';

type
  EFileNotFound = class(Exception);

  TConfig=class
   autosave_next:TDateTime;

    procedure CreateCfgDirs();
    procedure CompleteLoadFromFile(inidata: TMemIniFile);
    procedure CompleteSaveToFile(inidata: TMemIniFile);
    procedure UpdateAutosave();
  end;

  TGlobalConfig=class
   autosave:boolean;
   autosave_period:TTime;
   scale:Cardinal;
   ptAutoStart:Boolean;

    procedure LoadCfgFromFile(filename: string);
    procedure LoadCfgPtServer(ini: TMemIniFile);
    procedure SaveCfgToFile(filename: string);
  end;

  TFormData=class
   aFile:String;
    procedure LoadFormData(filename:string);
    procedure SaveFormData(filename:string);
  end;

var
  Config:TConfig;
  GlobalConfig:TGlobalConfig;
  FormData:TFormData;

implementation

uses fSettings, fSplash, fAdminForm, GetSystems, Prevody, Diagnostics, fMain,
     TechnologieRCS, TOblsRizeni, TBloky, BoosterDb, SnadnSpusteni, THVDatabase,
     TCPServerPT, Logging, TCPServerOR, SprDb, UserDb, ModelovyCas, TMultiJCDatabase,
     DataBloky, ACDatabase, FunkceVyznam, UDPDiscover, appEv, Trakce,
     TechnologieTrakce, TJCDatabase;

procedure TConfig.CreateCfgDirs();
begin
 try
   CreateDir('data');
   CreateDir('lok');
   CreateDir('stav');
 except
   on e:Exception do
     AppEvents.LogException(E);
 end;
end;

procedure TConfig.CompleteLoadFromFile(inidata: TMemIniFile);
var read,read2:string;
 begin
  F_Splash.AddStav('Načítám konfiguraci...');

  read := inidata.ReadString(_INIDATA_PATHS_DATA_SECTION, 'konfigurace', 'data\konfigurace.ini');
  try
    GlobalConfig.LoadCfgFromFile(read);
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;

  F_Options.CHB_Log_console.Checked := inidata.ReadBool('Log', 'console', true);

  F_Splash.AddStav('Načítám uživatele...');
  try
    UsrDB.LoadAll(
      inidata.ReadString(_INIDATA_PATHS_DATA_SECTION, 'users', 'data\users.ini'),
      inidata.ReadString(_INIDATA_PATHS_STATE_SECTION, 'users', 'stav\users.ini')
    );
  except
    on E:Exception do
      AppEvents.LogException(E, E.Message);
  end;

  F_Splash.AddStav('Načítám stanice (soubor *.spnl)...');
  read  := inidata.ReadString(_INIDATA_PATHS_DATA_SECTION, 'spnl', 'data\stanice.spnl');
  read2 := inidata.ReadString(_INIDATA_PATHS_STATE_SECTION, 'or', 'stav\or.ini');
  try
    ORs.LoadData(read, read2);
  except
    on E:EFileNotFound do
      writelog(E.Message, WR_ERROR);
    on E:Exception do
      AppEvents.LogException(E);
  end;
  F_Main.E_dataload_spnl.Text := read;

  F_Splash.AddStav('Načítám hnací vozidla...');
  F_Main.E_dataload_HV_dir.Text := inidata.ReadString(_INIDATA_PATHS_DATA_SECTION, 'lok', 'lok');
  F_Main.E_dataload_HV_state.Text := inidata.ReadString(_INIDATA_PATHS_STATE_SECTION, 'lok', 'stav\lok.ini');
  writelog('Načítám hnací vozidla - '+F_Main.E_dataload_HV_dir.Text+'\*', WR_DATA);
  try
    HVDb.LoadFromDir(F_Main.E_dataload_HV_dir.Text, F_Main.E_dataload_HV_state.Text);
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;
  writelog('Načteno '+IntToStr(HVDb.cnt)+' hnacích vozidel',WR_DATA);

  F_Splash.AddStav('Načítám RCS...');
  writelog('Načítám RCS...', WR_DATA);
  try
    RCSi.LoadFromFile(inidata);
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;
  writelog('RCS načteno', WR_DATA);

  F_Splash.AddStav('Načítám trakci...');
  writelog('Načítám trakci...', WR_DATA);
  try
    TrakceI.LoadFromFile(inidata);
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;
  writelog('Trakce načtena', WR_DATA);

  F_Splash.AddStav('Načítám databázi zesilovačů...');
  read := inidata.ReadString(_INIDATA_PATHS_DATA_SECTION, 'zesilovace', 'data\zesilovace.ini');
  try
    Boosters.LoadFromFile(read);
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;
  F_Main.E_dataload_zes.Text := ExtractRelativePath(ExtractFilePath(Application.ExeName),read);

  F_Splash.AddStav('Načítám soupravy...');
  read := inidata.ReadString(_INIDATA_PATHS_STATE_SECTION, 'soupravy', 'stav\soupravy.ini');
  try
    Soupravy.LoadData(read);
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;
  F_Main.E_dataload_soupr.Text := ExtractRelativePath(ExtractFilePath(Application.ExeName), read);

  F_Splash.AddStav('Načítám databázi bloků...');
  read := inidata.ReadString(_INIDATA_PATHS_DATA_SECTION, 'bloky', 'data\bloky.ini');
  read2 := inidata.ReadString(_INIDATA_PATHS_STATE_SECTION, 'bloky', 'stav\bloky.ini');
  try
    Blky.LoadFromFile(read, F_Main.E_dataload_spnl.Text, read2);
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;
  F_Main.E_dataload_block.Text := read;
  BlokyTableData := TBlokyTableData.Create(F_Main.LV_Bloky);

  Soupravy.UpdateFront();

  F_Splash.AddStav('Načítám databázi jizdních cest...');
  read := inidata.ReadString(_INIDATA_PATHS_DATA_SECTION, 'JC', 'data\JC.ini');
  try
    JCDb.LoadData(read);
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;

  F_Splash.AddStav('Načítám databázi složených jizdních cest...');
  read := inidata.ReadString(_INIDATA_PATHS_DATA_SECTION, 'mJC', 'data\mJC.ini');
  try
    MultiJCDb.LoadData(read);
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;
  F_Main.E_Dataload_multiJC.Text := MultiJCDb.filename;

  F_Splash.AddStav('Načítám databázi automatických režimů...');
  read := inidata.ReadString(_INIDATA_PATHS_DATA_SECTION, 'AC', 'AC');
  read2 := inidata.ReadString(_INIDATA_PATHS_STATE_SECTION, 'AC', 'stav\AC.ini');
  try
    ACDb.LoadFromDir(read);
    ACDb.LoadStatFromFile(read2);
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;
  F_Main.E_dataload_AC.Text := ExtractRelativePath(ExtractFilePath(Application.ExeName), ACDb.dirname);

  F_Splash.AddStav('Načítám databázi FormData...');
  read := inidata.ReadString(_INIDATA_PATHS_STATE_SECTION, 'forms', 'stav\forms.ini');
  try
    FormData.LoadFormData(read);
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;

  F_Splash.AddStav('Načítám vedlejší databáze...');
  TrakceI.LoadSpeedTable('data\rychlosti.csv',F_Options.LV_DigiRych);
 end;

procedure TConfig.CompleteSaveToFile(inidata: TMemIniFile);
var tmpStr:string;
 begin
  inidata.EraseSection(_INIDATA_PATHS_DATA_SECTION);
  inidata.EraseSection(_INIDATA_PATHS_STATE_SECTION);
  WriteLog('Probíha kompletní ukládání dat', WR_DATA);

  try
    Blky.SaveToFile(F_Main.E_dataload_block.Text);
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;

  try
    RCSi.SaveToFile(inidata);
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;

  try
    TrakceI.SaveToFile(inidata);
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;

  try
    GlobalConfig.SaveCfgToFile(F_Options.E_dataload.Text);
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
    TrakceI.SaveSpeedTable('data/rychlosti.csv');
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;

  try
    F_Admin.B_SaveClick(Self);
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;

  WriteLog('Kompletní ukládání dat dokončeno',WR_DATA);

  try
    inidata.WriteString(_INIDATA_PATHS_DATA_SECTION, 'konfigurace', F_Options.E_dataload.Text);
    inidata.WriteString(_INIDATA_PATHS_DATA_SECTION, 'spnl', F_Main.E_dataload_spnl.Text);
    inidata.WriteString(_INIDATA_PATHS_DATA_SECTION, 'bloky', Blky.blky_file);
    inidata.WriteString(_INIDATA_PATHS_STATE_SECTION, 'bloky', Blky.fstatus);
    inidata.WriteString(_INIDATA_PATHS_DATA_SECTION, 'zesilovace', F_Main.E_dataload_zes.Text);
    inidata.WriteString(_INIDATA_PATHS_DATA_SECTION, 'JC', JCDb.filename);
    inidata.WriteString(_INIDATA_PATHS_DATA_SECTION, 'mJC', MultiJCDb.filename);
    inidata.WriteString(_INIDATA_PATHS_STATE_SECTION, 'soupravy', F_Main.E_dataload_soupr.Text);
    inidata.WriteString(_INIDATA_PATHS_DATA_SECTION, 'users', UsrDB.filenameData);
    inidata.WriteString(_INIDATA_PATHS_STATE_SECTION, 'users', UsrDB.filenameStat);
    inidata.WriteString(_INIDATA_PATHS_DATA_SECTION, 'AC', ACDb.dirname);
    inidata.WriteString(_INIDATA_PATHS_STATE_SECTION, 'AC', ACDb.statfilename);
    inidata.WriteString(_INIDATA_PATHS_DATA_SECTION, 'lok', F_Main.E_dataload_HV_dir.Text);
    inidata.WriteString(_INIDATA_PATHS_STATE_SECTION, 'lok', F_Main.E_dataload_HV_state.Text);
    inidata.WriteBool('Log', 'console', F_Options.CHB_Log_console.Checked);

    if (ORs.status_filename = '') then
      tmpStr := 'stav\or_stav.ini'
    else
      tmpStr := ORs.status_filename;

    inidata.WriteString(_INIDATA_PATHS_STATE_SECTION, 'or', tmpStr);
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;

  inidata.UpdateFile();
 end;

////////////////////////////////////////////////////////////////////////////////

procedure TGlobalConfig.LoadCfgFromFile(filename: string);
var str:string;
    ini:TMemIniFile;
 begin
  writelog('Načítám globální konfiguraci: '+filename, WR_DATA);
  F_Options.E_dataload.Text := filename;

  ini := TMemIniFile.Create(filename, TEncoding.UTF8);
  try
    case ini.ReadInteger('Application','WState',0) of
     0:begin
        F_Main.WindowState := wsMaximized;
       end;
     1:begin
        F_Main.Left   := ini.ReadInteger('Application', 'Left', 200);
        F_Main.Top    := ini.ReadInteger('Application', 'Top', 200);
        F_Main.Height := ini.ReadInteger('Application', 'Heigth', 500);
        F_Main.Width  := ini.ReadInteger('Application', 'Width', 1125);
        F_Main.WindowState := wsNormal;
       end;
    end;//case

    F_Options.LB_Timer.ItemIndex := ini.ReadInteger('SystemCfg', 'TimerInterval', 4);
    F_Options.LB_TimerClick(Self);
    F_Options.CHB_povolit_spusteni.Checked := ini.ReadBool('SystemCfg', 'AutSpusteni', false);
    if (F_Options.CHB_povolit_spusteni.Checked) then
      F_Main.KomunikacePocitani := 1;
    GlobalConfig.scale := ini.ReadInteger('SystemCfg', 'scale', 120);

    ModCas.LoadData(ini);
    SS.LoadData(ini);
    FuncsFyznam.ParseWholeList(ini.ReadString('funcsVyznam', 'funcsVyznam', ''));
    F_Main.CHB_RCS_Show_Only_Active.Checked := ini.ReadBool('RCS', 'ShowOnlyActive', false);

    ORTCPServer.port := ini.ReadInteger('PanelServer', 'port', _PANEL_DEFAULT_PORT);

    Self.LoadCfgPtServer(ini);

    // autosave
    GlobalConfig.autosave := ini.ReadBool('autosave', 'enabled', true);
    str := ini.ReadString('autosave', 'period', '00:30');
    GlobalConfig.autosave_period := EncodeTime(0, StrToIntDef(LeftStr(str, 2), 1), StrToIntDef(Copy(str, 4, 2), 0), 0);
    if (GlobalConfig.autosave) then
      Config.autosave_next := Now + GlobalConfig.autosave_period;

    // UDP discovery
    UDPdisc := TUDPDiscover.Create(_DISC_DEFAULT_PORT,
         ini.ReadString('PanelServer', 'nazev', ''),
         ini.ReadString('PanelServer', 'popis', ''));

    try
      diag.LoadData(ini, 'AdminData');
      F_Admin.LoadData(ini);
    except
      on E:Exception do
        AppEvents.LogException(E);
    end;

    writelog('Globální konfigurace načtena', WR_DATA);
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
    on E:EPTActive do
      writeLog('PT ERR: '+E.Message, WR_PT);
  end;

  PtServer.compact := ini.ReadBool(_SECTION, 'compact', _PT_COMPACT_RESPONSE);
  Self.ptAutoStart := ini.ReadBool(_SECTION, 'autoStart', false);

  strs := TStringList.Create();
  try
    ini.ReadSection(_SECTION+'StaticTokens', strs);
    for key in strs do
     begin
      try
        str := ini.ReadString(_SECTION+'StaticTokens', key, '');
        if (str <> '') then
          PtServer.AccessTokenAdd(key, str);
      except
        on E:Exception do
          writelog('PTserver: nepodařilo se přidat token '+str+' ('+E.Message+')', WR_ERROR);
      end;
     end;
  finally
    strs.Free();
  end;
 end;

procedure TGlobalConfig.SaveCfgToFile(filename:string);
var ini:TMemIniFile;
 begin
  ini := TMemIniFile.Create(filename, TEncoding.UTF8);

  try
    ModCas.SaveData(ini);

    ini.WriteInteger('SystemCfg', 'TimerInterval', F_Options.LB_Timer.ItemIndex);
    ini.WriteBool('SystemCfg', 'AutSpusteni', F_Options.CHB_povolit_spusteni.Checked);
    ini.WriteInteger('SystemCfg', 'scale', GlobalConfig.scale);

    SS.SaveData(ini);

    ini.WriteBool('autosave', 'enabled', GlobalConfig.autosave);
    ini.WriteString('autosave', 'period', FormatDateTime('nn:ss', GlobalConfig.autosave_period));

    ini.WriteString('funcsVyznam', 'funcsVyznam', FuncsFyznam.GetFuncsVyznam());
    ini.WriteBool('RCS', 'ShowOnlyActive', F_Main.CHB_RCS_Show_Only_Active.Checked);
  finally
    ini.UpdateFile();
    ini.Free();
  end;
 end;

////////////////////////////////////////////////////////////////////////////////

procedure TFormData.LoadFormData(filename:String);
var j:Integer;
    objs:TStrings;
    obj, rawObj:String;
    aComponent:TComponent;
    ini:TMemIniFile;
 begin
  writelog('Načítám FormData: '+filename, WR_DATA);

  ini := TMemIniFile.Create(filename, TEncoding.UTF8);
  FormData.aFile := filename;
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
  writelog('FormData úspěšně načtena', WR_DATA);
 end;

procedure TFormData.SaveFormData(filename:String);
var i, j:Integer;
    ini:TMemIniFile;
 begin
  DeleteFile(filename);
  ini := TMemIniFile.Create(filename, TEncoding.UTF8);

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
procedure TConfig.UpdateAutosave();
begin
 if ((GlobalConfig.autosave) and (Now > Self.autosave_next)) then
  begin
   try
     F_Main.A_SaveStavExecute(Self);
   except
     on E:Exception do
       AppEvents.LogException(E, 'Vyjimka pri ukladani stavu kolejiste');
   end;
   Self.autosave_next := Now + GlobalConfig.autosave_period;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

initialization
  GlobalConfig := TGlobalConfig.Create();
  Config := TConfig.Create();
  FormData := TFormData.Create();

finalization
  FreeAndNil(FormData);
  FreeAndNil(Config);
  FreeAndNil(GlobalConfig);

end.//unit

