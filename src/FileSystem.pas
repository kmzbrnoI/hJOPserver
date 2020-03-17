﻿unit FileSystem;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, IniFiles, ComCtrls, ExtCtrls, StdCtrls, StrUtils,
  fMain, Trakce, TechnologieTrakce, TJCDatabase;

const
  _INIDATA_PATHS_DATA_SECTION = 'PathsData';
  _INIDATA_PATHS_STATE_SECTION = 'PathsState';

type
  EFileNotFound = class(Exception);

  TData=class
   autosave:boolean;
   autosave_period:TTime;
   autosave_next:TDateTime;

    procedure CreateCfgDirs();
    procedure CompleteLoadFromFile(inidata: TMemIniFile);
    procedure CompleteSaveToFile(inidata: TMemIniFile);
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

uses fSettings, fSplash, fAdminForm, GetSystems, Prevody, Diagnostics,
     TechnologieRCS, TOblsRizeni, TBloky, BoosterDb, SnadnSpusteni, THVDatabase,
     TCPServerPT, Logging, TCPServerOR, SprDb, UserDb, ModelovyCas, TMultiJCDatabase,
     DataBloky, ACDatabase, FunkceVyznam, UDPDiscover, appEv;

procedure TData.CreateCfgDirs();
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

procedure TData.CompleteLoadFromFile(inidata: TMemIniFile);
var read,read2:string;
 begin
  F_Splash.AddStav('Načítám konfiguraci...');
  read := inidata.ReadString(_INIDATA_PATHS_DATA_SECTION, 'konfigurace', 'data\konfigurace.ini');
  try
    Konfigurace.LoadCfgFromFile(read);
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

  //nacitani bloku
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

procedure TData.CompleteSaveToFile(inidata: TMemIniFile);
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

  WriteLog('Kompletni ukladani dat dokonceno',WR_DATA);

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

procedure TKonfigurace.LoadCfgFromFile(IniLoad:string);
var str:string;
    ini:TMemIniFile;
 begin
  writelog('Načítám konfiguraci - '+IniLoad, WR_DATA);
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

    try
      diag.LoadData(ini, 'AdminData');
      F_Admin.LoadData(ini);
    except
      on E:Exception do
        AppEvents.LogException(E);
    end;

    writelog('Konfigurace načtena', WR_DATA);
  finally
    ini.Free();
  end;
 end;

procedure TKonfigurace.SaveCfgToFile(IniSave:string);
var ini:TMemIniFile;
 begin
  ini := TMemIniFile.Create(IniSave, TEncoding.UTF8);

  try
    ModCas.SaveData(ini);

    ini.WriteInteger('SystemCfg', 'TimerInterval', F_Options.LB_Timer.ItemIndex);
    ini.WriteBool('SystemCfg', 'AutSpusteni', F_Options.CHB_povolit_spusteni.Checked);

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
  writelog('Načítám FormData - '+IniLoad,WR_DATA);

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
  writelog('FormData úspěšně načtena', WR_DATA);
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

