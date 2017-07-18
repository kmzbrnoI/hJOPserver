unit FileSystem;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, IniFiles, ComCtrls, ExtCtrls, StdCtrls, StrUtils,
  fMain, Trakce, TrakceGUI, XpressNET, CPort, AC, TJCDatabase;

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
 ini:TMemIniFile;
  procedure LoadCfgFromFile(IniLoad:string);
  procedure SaveCfgToFile(IniSave:string);
end;

TFormData=class
 aFile:String;
 ini:TMemIniFile;
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
     TBlok, TBlokUsek, TBlokVyhybka, TBlokSCom, TBlokIR, TOblRizeni, BoosterDb,
     Booster, SnadnSpusteni, TBlokPrejezd, THVDatabase, TCPServerPT,
     Logging, TCPServerOR, SprDb, UserDb, ModelovyCas, TMultiJCDatabase,
     DataBloky, ACDatabase, FunkceVyznam, UDPDiscover, appEv;

procedure TData.CompleteLoadFromFile;
var read,read2:string;
 begin
  DateTimeToString(OPData.xDate, 'dd.mm.yyyy', Now);

  F_Splash.AddStav('Naèítám konfiguraci');
  read := ini_lib.ReadString('NacteniDat','Konfigurace', 'data\Konfigurace.ini');
  try
    Konfigurace.LoadCfgFromFile(read);
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;

  F_Splash.AddStav('Naèítám uživatele');
  read := ini_lib.ReadString('NacteniDat','users', 'data\users.ini');
  try
    UsrDB.LoadFile(read);
  except
    on E:Exception do
      AppEvents.LogException(E, E.Message);
  end;
  F_Main.E_Dataload_Users.Text := read;

  F_Splash.AddStav('Naèítám stanice (soubor *.spnl)');
  read  := ini_lib.ReadString('NacteniDat', 'spnl', 'data\stanice.spnl');
  read2 := ini_lib.ReadString('NacteniDat', 'or_stat', 'data\or_stat.ini');
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
  HVDb.LoadFromDir('lok');

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
  read := ini_lib.ReadString('NacteniDat','Zesilovace', 'data\Zesilovace.ini');
  try
    Boosters.LoadFromFile(read);
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;
  F_Main.E_dataload_zes.Text := ExtractRelativePath(ExtractFilePath(Application.ExeName),read);

  F_Splash.AddStav('Naèítám soupravy');
  read := ini_lib.ReadString('NacteniDat','soupravy', 'data\soupravy.ini');
  try
    Soupravy.LoadData(read);
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;
  F_Main.E_dataload_soupr.Text := ExtractRelativePath(ExtractFilePath(Application.ExeName), read);

  //nacitani bloku
  F_Splash.AddStav('Naèítám databázi blokù');
  read := ini_lib.ReadString('NacteniDat', 'Bloky', 'data\bloky.ini');
  read2 := ini_lib.ReadString('NacteniDat', 'Bloky_stat', 'data\bloky_stat.ini');
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
  read := ini_lib.ReadString('NacteniDat','JC', 'data\JC.ini');
  try
    JCDb.LoadData(read);
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;

  F_Splash.AddStav('Naèítám databázi složených jizdních cest');
  read := ini_lib.ReadString('NacteniDat', 'mJC', 'data\mJC.ini');
  try
    MultiJCDb.LoadData(read);
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;
  F_Main.E_Dataload_multiJC.Text := MultiJCDb.filename;

  F_Splash.AddStav('Naèítám databázi automatických režimù');
  read := ini_lib.ReadString('NacteniDat', 'AC', 'AC');
  read2 := ini_lib.ReadString('NacteniDat', 'AC_stat', 'data\AC_stat.ini');
  try
    ACDb.LoadFromDir(read);
    ACDb.LoadStatFromFile(read2);
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;
  F_Main.E_dataload_AC.Text := ExtractRelativePath(ExtractFilePath(Application.ExeName), ACDb.dirname);

  F_Splash.AddStav('Naèítám databázi FormData');
  read := ini_lib.ReadString('NacteniDat','FormData', 'data\FormData.ini');
  try
    FormData.LoadFormData(read);
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;

  F_Splash.AddStav('Naèítám vedlejší databáze');
  TrkSystem.LoadSpeedTable('data\Rychlosti.csv',F_Options.LV_DigiRych);
  try
    F_Admin.LoadData;
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;
 end;//procedure

procedure TData.CompleteSaveToFile;
var tmpStr:string;
 begin
  ini_lib.EraseSection('NacteniDat');
  WriteLog('Probiha kompletni ukladani dat',WR_DATA);

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
    UsrDB.SaveFile(F_Main.E_Dataload_Users.Text);
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;

  try
    HVDb.SaveToDir('lok');
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;

  try
    TrkSystem.SaveSpeedTable(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))+'data/Rychlosti.csv');
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
    ini_lib.WriteString('NacteniDat', 'spnl', ExtractRelativePath(ExtractFilePath(Application.ExeName), F_Main.E_dataload_spnl.Text));
    ini_lib.WriteString('NacteniDat', 'Bloky', ExtractRelativePath(ExtractFilePath(Application.ExeName), Blky.blky_file));
    ini_lib.WriteString('NacteniDat', 'Bloky_stat', ExtractRelativePath(ExtractFilePath(Application.ExeName), Blky.fstatus));
    ini_lib.WriteString('NacteniDat', 'Zesilovace', ExtractRelativePath(ExtractFilePath(Application.ExeName), F_Main.E_dataload_zes.Text));
    ini_lib.WriteString('NacteniDat', 'JC', ExtractRelativePath(ExtractFilePath(Application.ExeName), JCDb.filename));
    ini_lib.WriteString('NacteniDat', 'mJC', ExtractRelativePath(ExtractFilePath(Application.ExeName), MultiJCDb.filename));
    ini_lib.WriteString('NacteniDat', 'soupravy', ExtractRelativePath(ExtractFilePath(Application.ExeName), F_Main.E_dataload_soupr.Text));
    ini_lib.WriteString('NacteniDat', 'users', ExtractRelativePath(ExtractFilePath(Application.ExeName), F_Main.E_Dataload_Users.Text));
    ini_lib.WriteString('NacteniDat', 'AC', ExtractRelativePath(ExtractFilePath(Application.ExeName), ACDb.dirname));
    ini_lib.WriteString('NacteniDat', 'AC_stat', ExtractRelativePath(ExtractFilePath(Application.ExeName), ACDb.statfilename));

    if (ORs.status_filename = '') then
      tmpStr := 'data\or_stat.ini'
    else
      tmpStr := ORs.status_filename;

    ini_lib.WriteString('NacteniDat', 'or_stat', ExtractRelativePath(ExtractFilePath(Application.ExeName), tmpStr));
  except
    on E:Exception do
      AppEvents.LogException(E);
  end;

  ini_lib.UpdateFile();
 end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TKonfigurace.LoadCfgFromFile(IniLoad:string);
var str:string;
 begin
   writelog('Naèítám konfiguraci - '+IniLoad,WR_DATA);
   Konfigurace.ini := TMemIniFile.Create(IniLoad, TEncoding.UTF8);
   F_Options.E_dataload.Text:=IniLoad;

   //zacatek nacitani dat do programu

   //nastaveni defaultnich cest pro Open/Save dialogy
   F_Options.OD_Open.InitialDir := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))+'data\';
   F_Options.SD_Save.InitialDir := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))+'data\';

   //nacitani dat o programu - left, top...
   case Konfigurace.ini.ReadInteger('Application','WState',0) of
    0:begin
       F_Main.WindowState := wsMaximized;
      end;
    1:begin
       F_Main.Left   := Konfigurace.ini.ReadInteger('Application', 'Left',200);
       F_Main.Top    := Konfigurace.ini.ReadInteger('Application', 'Top',200);
       F_Main.Height := Konfigurace.ini.ReadInteger('Application', 'Heigth',500);
       F_Main.Width  := Konfigurace.ini.ReadInteger('Application', 'Width',1125);
       F_Main.WindowState := wsNormal;
      end;
   end;//case

   //nacitani dat o konzoli
   F_Options.CHB_Log_console.Checked := ini_lib.ReadBool('Log','Log_console',true);

   //zjisteni, jestli byl naposled program ukoncen nasilim
   Konfigurace.ini.WriteInteger('SystemCfg','RunError',0);
   Konfigurace.ini.UpdateFile;

   TrkSystem := TTrkGUI.Create(Ttrk_system.TRS_XpressNET, F_Main.LV_log_lnet, TTrkLogLevel(ini.ReadInteger('Centrala', 'logfile', 2)), TTrkLogLevel(ini.ReadInteger('Centrala', 'logtable', 2)));

   TrkSystem.BaudRate := TBaudRate(ini.ReadInteger('Centrala','BaudRate',0));
   TrkSystem.StopBits := TStopBits(ini.ReadInteger('Centrala','StopBits',0));
   TrkSystem.DataBits := TDataBits(ini.ReadInteger('Centrala','DataBits',0));
   TrkSystem.FlowControl := TFlowControl(ini.ReadInteger('Centrala', 'FlowControl', 0));
   TrkSystem.COM := ini.ReadString('Centrala','COM','COM1');

   F_Main.CB_centrala_loglevel_file.ItemIndex  := Integer(TrkSystem.logfile);
   F_Main.CB_centrala_loglevel_table.ItemIndex := Integer(TrkSystem.logtable);

   //nactitani dalsich dat
   F_Options.LB_Timer.ItemIndex             := ini.ReadInteger('SystemCfg','TimerInterval',4);
   F_Options.LB_TimerClick(Self);
   F_Options.CHB_povolit_spusteni.Checked   := ini.ReadBool('SystemCfg','AutSpusteni',false);
   if (F_Options.CHB_povolit_spusteni.Checked) then F_Main.KomunikacePocitani := 1;

   //nacitani modeloveho casu
   ModCas.LoadData(Konfigurace.ini);

   SS.LoadData(Konfigurace.ini);

   ORTCPServer.port := ini.ReadInteger('PanelServer', 'port', _PANEL_DEFAULT_PORT);

   try
     PtServer.port := ini.ReadInteger('PTServer', 'port', _PT_DEFAULT_PORT);
   except
     on E:EPTActive do
       writeLog('PT ERR: '+E.Message, WR_PT);
   end;

   PtServer.compact := ini.ReadBool('PTServer', 'compact', _PT_COMPACT_RESPONSE);

   // autosave

   Data.autosave        := ini.ReadBool('autosave', 'enabled', true);
   str := ini.ReadString('autosave', 'period', '00:30');
   Data.autosave_period := EncodeTime(0, StrToIntDef(LeftStr(str, 2), 1), StrToIntDef(Copy(str, 4, 2), 0), 0);
   if (data.autosave) then
     Data.autosave_next := Now + Data.autosave_period;

   // nacteni vyznamu funkci
   FuncsFyznam.ParseWholeList(ini.ReadString('funcsVyznam', 'funcsVyznam', ''));

   // nacteni UDP discovery
   UDPdisc := TUDPDiscover.Create(_DISC_DEFAULT_PORT,
        ini.ReadString('PanelServer', 'nazev', ''),
        ini.ReadString('PanelServer', 'popis', ''));

   writelog('Konfigurace naètena', WR_DATA);
 end;//procedure

procedure TKonfigurace.SaveCfgToFile(IniSave:string);
 begin
  ini_lib.WriteString('NacteniDat','Konfigurace',IniSave); 
  Konfigurace.ini := TMemIniFile.Create(IniSave, TEncoding.UTF8);

  //ukladani modeloveho casu
  ModCas.SaveData(Konfigurace.ini);

  ini.WriteInteger('SystemCfg','TimerInterval',F_Options.LB_Timer.ItemIndex);
  ini.WriteBool('SystemCfg','AutSpusteni',F_Options.CHB_povolit_spusteni.Checked);

  ini.WriteInteger('Centrala','BaudRate', Integer(TrkSystem.BaudRate));
  ini.WriteInteger('Centrala','StopBits', Integer(TrkSystem.StopBits));
  ini.WriteInteger('Centrala','DataBits', Integer(TrkSystem.DataBits));
  ini.WriteInteger('Centrala','FlowControl', Integer(TrkSystem.FlowControl));
  ini.Writestring('Centrala','COM', TrkSystem.COM);

  ini.WriteInteger('Centrala', 'logtable', Integer(TrkSystem.logtable));
  ini.WriteInteger('Centrala', 'logfile', Integer(TrkSystem.logfile));

  ini_lib.WriteBool('Log','Log_console',F_Options.CHB_Log_console.Checked);
  ini.WriteInteger('AdminData','FormLeft',F_Admin.Left);
  ini.WriteInteger('AdminData','FormTop',F_Admin.Top);

  ini.WriteInteger('PanelServer', 'port', ORTCPServer.port);
  ini.WriteString('PanelServer', 'nazev', UDPdisc.name);
  ini.WriteString('PanelServer', 'popis', UDPdisc.description);

  ini.WriteInteger('PTServer', 'port', PtServer.port);
  ini.WriteBool('PTServer', 'compact', PtServer.compact);

  SS.SaveData(Konfigurace.ini);

  // autosave
  ini.WriteBool('autosave', 'enabled', Data.autosave);
  ini.WriteString('autosave', 'period', FormatDateTime('nn:ss', Data.autosave_period));

  // ulozeni vyznamu funkci
  ini.WriteString('funcsVyznam', 'funcsVyznam', FuncsFyznam.GetFuncsVyznam());

  Konfigurace.ini.UpdateFile;
  Konfigurace.ini.Free;
 end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TFormData.LoadFormData(IniLoad:String);
var cyklus,cyklus2:Integer;
    Polozky:TStrings;
    Polozka:String;
    aComponent:TComponent;
 begin
  writelog('Naèítám FormData - '+IniLoad,WR_DATA);

  FormData.ini := TMemIniFile.Create(IniLoad, TEncoding.UTF8);
  FormData.aFile := IniLoad;
  Polozky := TStringList.Create;

  FormData.ini.GetStrings(Polozky);
  if (Polozky.Count = 0) then
   begin
    writelog('FormData upozorneni - pocet polozek = 0',WR_DATA);
    Exit;
   end;//if Polozky.Count = 0
  for cyklus := 0 to Polozky.Count-1 do
   begin
    cyklus2 := 0;
    while (FormData.ini.ReadInteger(RightStr(LeftStr(Polozky[cyklus],Length(Polozky[cyklus])-1),Length(Polozky[cyklus])-2),IntToStr(cyklus2),-1) > -1) do
     begin
      Polozka := RightStr(LeftStr(Polozky[cyklus],Length(Polozky[cyklus])-1),Length(Polozky[cyklus])-2);
      aComponent := F_Options.FindComponent(Polozka);
      if (AComponent = nil) then aComponent := F_Main.FindComponent(Polozka);

      if (aComponent <> nil) then
       begin
        if (cyklus2 < (aComponent as TListView).Columns.Count) then
         begin
          (aComponent as TListView).Columns.Items[cyklus2].Width := FormData.ini.ReadInteger(Polozka,IntToStr(cyklus2),0);
         end;
       end;//if (aComponent <> nil)
      cyklus2 := cyklus2 + 1;
     end;//while
   end;//for cyklus

  Polozky.Free;
  FormData.ini.Free;
  writelog('FormData uspesne nactena',WR_DATA);
 end;//procedure

procedure TFormData.SaveFormData(IniSave:String);
var cyklus,cyklus2:Integer;
 begin
  DeleteFile(IniSave);
  FormData.ini := TMemIniFile.Create(IniSave, TEncoding.UTF8);

  for cyklus := 0 to F_Options.ComponentCount-1 do
    if (F_Options.Components[cyklus].ClassType = TListView) then
      for cyklus2 := 0 to (F_Options.Components[cyklus] as TListView).Columns.Count-1 do
        FormData.ini.WriteInteger(F_Options.Components[cyklus].Name,IntToStr(cyklus2),(F_Options.Components[cyklus] as TListView).Columns.Items[cyklus2].Width);

  for cyklus := 0 to F_Main.ComponentCount-1 do
    if (F_Main.Components[cyklus].ClassType = TListView) then
      for cyklus2 := 0 to (F_Main.Components[cyklus] as TListView).Columns.Count-1 do
        FormData.ini.WriteInteger(F_Main.Components[cyklus].Name,IntToStr(cyklus2),(F_Main.Components[cyklus] as TListView).Columns.Items[cyklus2].Width);

  FormData.ini.UpdateFile;
  FormData.ini.Free;
 end;//procedure

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
end;//procedure

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

