unit FileSystem;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, IniFiles, ComCtrls, ExtCtrls, StdCtrls,Mask, Gauges, StrUtils, Registry, fLicence, Grids, jpeg, ShellAPI,
  fMain, RPConst, Trakce, TrakceGUI, XpressNET, CPort, AC, TJCDatabase;

type

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
     TechnologieMTB, Verze, fHVEdit, TechnologieJC, fConsole, TOblsRizeni, TBloky,
     TBlok, TBlokUsek, TBlokVyhybka, TBlokSCom, TBlokIR, TOblRizeni, BoosterDb,
     Booster, SnadnSpusteni, TBlokPrejezd, THVDatabase,
     Logging, TCPServerOR, SprDb, UserDb, ModelovyCas, TMultiJCDatabase,
     DataBloky, ACDatabase, FunkceVyznam;

procedure TData.CompleteLoadFromFile;
var return:Byte;
    read,read2:string;
 begin
  DateTimeToString(OPData.xDate, 'dd.mm.yyyy', Now);

  try
    if (not DirectoryExists('data')) then
      CreateDir('data');
  except
    writelog('Nelze vytvorit slozku data', WR_DATA, 1);
  end;

  F_Splash.AddStav('Naèítám konfiguraci');
  read := ini_lib.ReadString('NacteniDat','Konfigurace', 'data\Konfigurace.ini');
  Konfigurace.LoadCfgFromFile(read);

  F_Splash.AddStav('Naèítám uživatele');
  read := ini_lib.ReadString('NacteniDat','users', 'data\users.ini');
  UsrDB.LoadFile(read);
  F_Main.E_Dataload_Users.Text := read;

  F_Splash.AddStav('Naèítám stanice (soubor *.spnl)');
  read := ini_lib.ReadString('NacteniDat','spnl', 'data\stanice.spnl');
  ORs.LoadData(read);
  F_Main.E_dataload_spnl.Text := read;

  F_Splash.AddStav('Naèítám hnací vozidla');
  HVDb.LoadFromDir('lok');

  F_Splash.AddStav('Naèítám MTB');
  writelog('Nacitam MTB...',WR_DATA);
  read := ini_lib.ReadString('NacteniDat','MTBData', 'data\MTB.ini');
  return := MTB.LoadFromFile(read);

  if (return <> 0) then  //overeni chyby pri nacitani
   begin
    writelog('MTB LOAD ERROR : '+IntToStr(return),WR_DATA);
    Application.MessageBox(PChar('Chyba pri nacitani MTB - chyba '+IntToStr(return)),'Chyba',MB_OK OR MB_ICONERROR);
   end;
  writelog('MTB nacteno',WR_DATA);

  F_Splash.AddStav('Naèítám soupravy');
  read := ini_lib.ReadString('NacteniDat','soupravy', 'data\soupravy.ini');
  Soupravy.LoadData(read);
  F_Main.E_dataload_soupr.Text := ExtractRelativePath(ExtractFilePath(Application.ExeName), read);

  //nacitani bloku
  F_Splash.AddStav('Naèítám databázi blokù');
  read := ini_lib.ReadString('NacteniDat', 'Bloky', 'data\bloky.ini');
  read2 := ini_lib.ReadString('NacteniDat', 'Bloky_stat', 'data\bloky_stat.ini');
  Blky.LoadFromFile(read, F_Main.E_dataload_spnl.Text, read2);
  F_Main.E_dataload_block.Text := read;
  BlokyTableData := TBlokyTableData.Create(F_Main.LV_Bloky);

  Soupravy.UpdateFront();

  F_Splash.AddStav('Naèítám databázi zesilovaèù');
  read := ini_lib.ReadString('NacteniDat','Zesilovace', 'data\Zesilovace.ini');
  BoostersDb.LoadFromFile(read);
  F_Main.E_dataload_zes.Text := ExtractRelativePath(ExtractFilePath(Application.ExeName),read);

  F_Splash.AddStav('Naèítám databázi jizdních cest');
  read := ini_lib.ReadString('NacteniDat','JC', 'data\JC.ini');
  JCDb.LoadData(read);

  F_Splash.AddStav('Naèítám databázi složených jizdních cest');
  read := ini_lib.ReadString('NacteniDat', 'mJC', 'data\mJC.ini');
  MultiJCDb.LoadData(read);
  F_Main.E_Dataload_multiJC.Text := MultiJCDb.filename;

  F_Splash.AddStav('Naèítám databázi automatických režimù');
  read := ini_lib.ReadString('NacteniDat','AutRezimy', 'data\AutRezimy.ini');
  ACDb.LoadFromFile(read);
  F_Main.E_dataload_AutRez.Text := ExtractRelativePath(ExtractFilePath(Application.ExeName),read);

  F_Splash.AddStav('Naèítám databázi FormData');
  read := ini_lib.ReadString('NacteniDat','FormData', 'data\FormData.ini');
  FormData.LoadFormData(read);

  F_Splash.AddStav('Naèítám vedlejší databáze');
  TrkSystem.LoadSpeedTable('data\Rychlosti.csv',F_Options.LV_DigiRych);
  F_Admin.LoadData;
 end;//procedure

procedure TData.CompleteSaveToFile;
var return:Integer;
 begin
  ini_lib.EraseSection('NacteniDat');
  WriteLog('Probiha kompletni ukladani dat',WR_DATA);

  Blky.SaveToFile(F_Main.E_dataload_block.Text);

  return := MTB.SaveToFile(MTB.filename);
  if (return <> 0) then  //overeni chyby pri nacitani
   begin
    writelog('MTB SAVE ERROR : '+IntToStr(return),WR_DATA);
    Application.MessageBox(PChar('Chyba pri ukladani MTB - chyba '+IntToStr(return)),'Chyba',MB_OK OR MB_ICONERROR);
   end else begin
    writelog('MTB SAVE OK',WR_DATA);
   end;//else return <> 0

  Konfigurace.SaveCfgToFile(F_Options.E_dataload.Text);
  ACDb.SaveToFile(ACDb.filename);
  BoostersDb.SaveToFile(F_Main.E_dataload_zes.Text);
  JCDb.SaveData(JCDb.filename);
  MultiJCDb.SaveData(MultiJCDb.filename);
  Soupravy.SaveData(F_Main.E_dataload_soupr.Text);
  FormData.SaveFormData(FormData.aFile);
  UsrDB.SaveFile(F_Main.E_Dataload_Users.Text);
  HVDb.SaveToDir('lok');
  TrkSystem.SaveSpeedTable(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))+'data/Rychlosti.csv');
  F_Admin.SaveData;
  WriteLog('Kompletni ukladani dat dokonceno',WR_DATA);

  ini_lib.WriteString('NacteniDat', 'spnl', ExtractRelativePath(ExtractFilePath(Application.ExeName), F_Main.E_dataload_spnl.Text));
  ini_lib.WriteString('NacteniDat', 'Bloky', ExtractRelativePath(ExtractFilePath(Application.ExeName), Blky.blky_file));
  ini_lib.WriteString('NacteniDat', 'Bloky_stat', ExtractRelativePath(ExtractFilePath(Application.ExeName), Blky.fstatus));
  ini_lib.WriteString('NacteniDat', 'MTBData', ExtractRelativePath(ExtractFilePath(Application.ExeName), MTB.filename));
  ini_lib.WriteString('NacteniDat', 'Zesilovace', ExtractRelativePath(ExtractFilePath(Application.ExeName), F_Main.E_dataload_zes.Text));
  ini_lib.WriteString('NacteniDat', 'JC', ExtractRelativePath(ExtractFilePath(Application.ExeName), JCDb.filename));
  ini_lib.WriteString('NacteniDat', 'mJC', ExtractRelativePath(ExtractFilePath(Application.ExeName), MultiJCDb.filename));
  ini_lib.WriteString('NacteniDat', 'soupravy', ExtractRelativePath(ExtractFilePath(Application.ExeName), F_Main.E_dataload_soupr.Text));
  ini_lib.WriteString('NacteniDat', 'users', ExtractRelativePath(ExtractFilePath(Application.ExeName), F_Main.E_Dataload_Users.Text));
 end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TKonfigurace.LoadCfgFromFile(IniLoad:string);
var str:string;
 begin
   writelog('Naèítám konfiguraci - '+IniLoad,WR_DATA);
   Konfigurace.ini := TMemIniFile.Create(IniLoad);
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
   if ini.ReadInteger('SystemCfg', 'RunError',0) = 1 then ShowErrorMessage := true else ShowErrorMessage := false;
   Konfigurace.ini.WriteInteger('SystemCfg','RunError',0);
   Konfigurace.ini.UpdateFile;

   TrkSystem := TTrkGUI.Create(Ttrk_system.TRS_XpressNET, F_Main.LV_log_lnet, ini.ReadInteger('Centrala', 'loglevel', 2), ini.ReadBool('Centrala', 'logfile', true), ini.ReadBool('Centrala', 'logtable', true));

   TrkSystem.BaudRate := TBaudRate(ini.ReadInteger('Centrala','BaudRate',0));
   TrkSystem.StopBits := TStopBits(ini.ReadInteger('Centrala','StopBits',0));
   TrkSystem.DataBits := TDataBits(ini.ReadInteger('Centrala','DataBits',0));
   TrkSystem.FlowControl := TFlowControl(ini.ReadInteger('Centrala', 'FlowControl', 0));
   TrkSystem.COM := ini.ReadString('Centrala','COM','COM1');

   F_Main.CB_centrala_loglevel.ItemIndex := TrkSystem.loglevel;
   F_Main.CHB_centrala_table.Checked     := TrkSystem.logtable;
   F_Main.CHB_centrala_file.Checked      := TrkSystem.logfile;

   //nactitani dalsich dat
   F_Options.LB_Timer.ItemIndex             := ini.ReadInteger('SystemCfg','TimerInterval',1);
   F_Options.LB_TimerClick(Self);
   F_Options.CHB_povolit_spusteni.Checked   := ini.ReadBool('SystemCfg','AutSpusteni',false);
   if (F_Options.CHB_povolit_spusteni.Checked) then F_Main.KomunikacePocitani := 1;

   //nacitani modeloveho casu
   ModCas.LoadData(Konfigurace.ini);

   SS.LoadData(Konfigurace.ini);

   ORTCPServer.port := ini.ReadInteger('PanelServer', 'port', _PANEL_DEFAULT_PORT);

   // autosave

   Data.autosave        := ini.ReadBool('autosave', 'enabled', true);
   str := ini.ReadString('autosave', 'period', '00:30');
   Data.autosave_period := EncodeTime(0, StrToIntDef(LeftStr(str, 2), 1), StrToIntDef(Copy(str, 4, 2), 0), 0);
   if (data.autosave) then
     Data.autosave_next := Now + Data.autosave_period;

   // nacteni vyznamu funkci
   FuncsFyznam.ParseWholeList(ini.ReadString('funcsVyznam', 'funcsVyznam', ''));

   writelog('Konfigurace naètena',WR_DATA);
 end;//procedure

procedure TKonfigurace.SaveCfgToFile(IniSave:string);
 begin
  ini_lib.WriteString('NacteniDat','Konfigurace',IniSave); 
  Konfigurace.ini := TMemIniFile.Create(IniSave);

  //ukladani modeloveho casu
  ModCas.SaveData(Konfigurace.ini);

  ini.WriteInteger('SystemCfg','TimerInterval',F_Options.LB_Timer.ItemIndex);
  ini.WriteBool('SystemCfg','AutSpusteni',F_Options.CHB_povolit_spusteni.Checked);

  ini.WriteInteger('Centrala','BaudRate', Integer(TrkSystem.BaudRate));
  ini.WriteInteger('Centrala','StopBits', Integer(TrkSystem.StopBits));
  ini.WriteInteger('Centrala','DataBits', Integer(TrkSystem.DataBits));
  ini.WriteInteger('Centrala','FlowControl', Integer(TrkSystem.FlowControl));
  ini.Writestring('Centrala','COM', TrkSystem.COM);

  ini.WriteInteger('Centrala', 'loglevel', TrkSystem.loglevel);
  ini.WriteBool('Centrala', 'logtable', TrkSystem.logtable);
  ini.WriteBool('Centrala', 'logfile', TrkSystem.logfile);

  ini_lib.WriteBool('Log','Log_console',F_Options.CHB_Log_console.Checked);
  ini.WriteInteger('AdminData','FormLeft',F_Admin.Left);
  ini.WriteInteger('AdminData','FormTop',F_Admin.Top);

  ini.WriteInteger('PanelServer', 'port', ORTCPServer.port);

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

  FormData.ini := TMemIniFile.Create(IniLoad);
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
  FormData.ini := TMemIniFile.Create(IniSave);

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
       writelog('Vyjimka pri ukladani stavu kolejiste - '+E.Message, WR_ERROR);
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

