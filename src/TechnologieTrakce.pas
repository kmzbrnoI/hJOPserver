﻿unit TechnologieTrakce;

{
 Trida TTrakce zpristupnuje rizeni trakce zbytku programu. Rizeni trakce je
 reseno dynamicky linkovanou knihovnou.
}

interface

uses
  SysUtils, Classes, StrUtils, CPort, Trakce, ComCtrls, Graphics, Forms, Windows,
  THnaciVozidlo, Generics.Collections, Contnrs, IniFiles;

const
  // maximalni rychlost pro rychlostni tabulku
  _MAX_SPEED = 28;

  // tato rychlostni tabulka je pouzita v pripade, kdy selze nacitani
  // souboru s rychlostni tabulkou:
  _DEFAULT_SPEED_TABLE : array [0.._MAX_SPEED] of Cardinal = (
    0,
    1,
    2,
    4,
    6,
    8,
    10,
    12,
    15,
    17,
    20,
    22,
    25,
    30,
    35,
    40,
    45,
    50,
    55,
    60,
    65,
    70,
    75,
    80,
    85,
    90,
    100,
    110,
    120
  );

  _MAX_LOGTABLE_ITEMS = 500;
  _LOG_PATH = 'log\trakce';

type
  TReadyEvent = procedure (Sender:TObject; ready:boolean) of object;
  TErrorEvent = procedure (Sender:TObject; errMsg:string) of object;
  TGetSpInfoEvent = procedure(Sender: TObject; Slot:TTrkLocoInfo; var handled:boolean) of object;
  TGetFInfoEvent = procedure(Sender: TObject; Addr:Integer; func:TFunkce; var handled:boolean) of object;

  // passed as a callaback parameter when programming POM
  TPOMCallback = record
    locoAddr:Word;
    toProgram:TList<THVPomCV>;
    index:Integer; // index of currently programmed CV in list 'toProgram'
    callback_ok, callback_err:TCommandCallback;
    new:TPomStatus;
    // TODO pokud dojde pri programovani POMu k chybe, je do HV.Slot.pom ulozeno TPomStatus.error
  end;

  {
   Pri prebirani lokomotivy se do callbacku jednotlivych cinnosti ukladaji tato data:
    prebirani lokomotivy =
      1) prevzit loko z centraly
      2) nastavit funkce na pozadovane hodnoty
      3) naprogarmovat POM
      4) zavolat OK callback
        - Pokud v libovolne casti procesu nastane chyba, je vyvolan Error callback.
        - Chyba pri nastavovani funkci je oznamena pouze jako WARNING (error callback neni volan).
  }
  TAcquireCallback = record
    addr:Word;                                                                  // adresa lokomotivy
    callback_ok, callback_err:TCommandCallback;                                 // globalni callbacky
  end;

  // pri hromadnem nastavovani funkci dle vyznamu (napr. vypnout vsechna "svetla") se do callbacku predavaji tato data
  TFuncsCallback = record
    addr:Word;                                                                  // adresa lokomotivy
    vyznam:string;                                                              // vyznam funkce, kterou nastavujeme
    state:boolean;                                                              // novy stav funkce
    callback_ok, callback_err:TCommandCallback;                                 // globalni callbacky
  end;

  // reprezenatce jedne funkci sady
  TFuncCBSada = record
    func:array[0..15] of boolean;                                               // funkce v sade
    index:Integer;                                                              // index sady (pro xpressnet: 0:F0-F4, 1:F5-F8, 2:F9-12)
  end;

  // pri programovani funkci si udrzuji nasledujici data (v callback datech jednotlivych procesu)
  TFuncCallback = record
    addr:Word;                                                                  // adresa lokomotivy
    callback_ok, callback_err:TCommandCallback;                                 // globalni callbacky
    sady:TList<TFuncCBSada>;                                                    // seznam jednotlivych sad, kazda sada obsahuje funkce k naprogramovani
  end;

  // zaznam toggleQueue
  THVFunc = record
    HV:THV;
    fIndex:Cardinal;
    time:TDateTime;
  end;

  TTrakce = class(TTrakceIFace)
   private const
     _DEF_LOGLEVEL = TTrkLogLevel.llInfo;
     _DEFAULT_LIB = 'xpressnet.dll';
     _INIFILE_SECTNAME = 'XN';


   private
     fLibDir:string;
     aReady:boolean;
     SpeedTable:array [0.._MAX_SPEED] of Cardinal;
     turnoff_callback:TNotifyEvent;                                             // callback volany po prikazu TurnOff
                                                                                //   Prikaz TurnOff zapina F0 a vypina vsechny vyssi funkce
                                                                                //   pouziva se pri vypinani systemu (pro vypnuti otravnych zvuku zvukovych dekoderu)

     eOnReady : TReadyEvent;
     eOnOpenError : TErrorEvent;

     mLogLevelFile : TTrkLogLevel;
     mLogLevelTable: TTrkLogLevel;
                                                                                // pri .Open se nastavuje na false
     procedure LoadLib(filename:string);

     procedure SetLoglevelFile(ll:TTrkLogLevel);
     procedure SetLoglevelTable(ll:TTrkLogLevel);

     //////////////////////////////////////

     procedure TrkLog(Sender:TObject; lvl:TTrkLogLevel; msg:string);            // logovaci event z .Trakce

     procedure NouzReleaseLoko();
//     procedure LokComErr(Sender:TObject; addr:Integer);                         // event oznamujici chybu komunikace s danou lokomotivou (je volan paralelne s error callback eventem, ale jen pro urcite prikazy - pro prikazy tykajici se rizeni konkretni lokomotivy).
//     procedure LokComOK(Sender:TObject; addr:Integer);                          // event potvrzujici komunikaci s danym HV (je volan pokazde, pokud centrala odpovi na prikaz o nasatveni dat HV)

     // eventy z komunikace s centralou pri prebirani a odhlasovani HV (tj. prebirani a odhlasovani VSECH LOKO)
//     procedure PrebiraniUpdateOK(Sender:TObject; Data:Pointer);                 // loko uspesne prevzato
//     procedure PrebiraniUpdateErr(Sender:TObject; Data:Pointer);                // prevzeti se nazdarilo (napr. centrala neodpovedela na prikaz o prevzeti, na POM ...)
//     procedure OdhlasovaniUpdateOK(Sender:TObject; Data:Pointer);               // loko uspesne uvolneno
//     procedure OdhlasovaniUpdateErr(Sender:TObject; Data:Pointer);              // uvolneni loko se nezdarilo (napr. centrala nedopovedela na POM, ...)

//     procedure AllPrevzato();                                                   // je volana, pokud jsou vsechny loko prevzaty (primarni vyuziti = interakce s GUI)
//     procedure AllOdhlaseno();                                                  // je volana, pokud jsou vsechny loko odhlaseny (primarni vyuziti = interakce s GUI)

     procedure TurnOffFunctions_cmdOK(Sender:TObject; Data:Pointer);            // OK callback pro prikaz TurnOff

     // eventy spojene se zapisem jednotlivych POM:
     procedure POMCvWroteOK(Sender:TObject; Data:Pointer);
     procedure POMCvWroteErr(Sender:TObject; Data:Pointer);

     // chyba pri nouzovem uvolnovani HV
     //   Nouzzove jsou uvolnena takove HV, ktera jsou pri BeforeClose jeste prevzata.
     procedure NouzReleaseCallbackErr(Sender:TObject; Data:Pointer);

//     procedure LoksSetFuncOK(Sender:TObject; Data:Pointer);
//     procedure LoksSetFuncErr(Sender:TObject; Data:Pointer);

     procedure LoadSpeedTableToTable(var LVRych:TListView);
//     procedure UpdateSpeedDir(HV:THV; Sender:TObject; speed:boolean; dir:boolean);

     procedure CheckToggleQueue();
     procedure FlushToggleQueue();
     procedure ProcessHVFunc(hvFunc:THVFunc);
     class function HVFunc(HV:THV; fIndex:Cardinal; time:TDateTime):THVFunc;

   public


     {
      Pozn.
        Vsechny funkce spojene s nastavovanim dat HV maji parametr Sender
        kam je vhodne dat bud konkretni regulator, nebo OR v pripade
          regulatoru klienta.
        Informace o zmene dat HV je pak volana do vsech systemu mimo Senderu.
        Tedy napriklad, pokud je loko otevrene v 5 regulatorech a jeste na serveru
          a dojde ke zmene rychlosti v OR1, je infroamce o zmene rychlosti
          odeslana do OR2, OR3, OR4, OR5 a regulatoru na serveru, nikoliv
          vsak do OR1 (tomu prijde napriklad OK, ci error callback)
     }

    DCCGoTime:TDateTime;
    toggleQueue:TQueue<THVFunc>;
    LogObj:TListView;

     constructor Create();
     destructor Destroy(); override;

     procedure Log(loglevel:TTrkLogLevel; msg:string);

     procedure LoadFromFile(ini:TMemIniFile);
     procedure SaveToFile(ini:TMemIniFile);

     procedure LokFuncToggle(Sender:TObject; HV:THV; fIndex:Cardinal);

     procedure LoadSpeedTable(filename:string;var LVRych:TListView);
     procedure SaveSpeedTable(filename:string);

     function SpeedStep(kmph:Cardinal):Cardinal;
     function GetStepSpeed(step:byte):Integer;
     function SetStepSpeed(step:byte; sp:Integer):Byte;

     procedure LoksSetFunc(vyznam:string; state:boolean);
     procedure POMWriteCVs(Sender:TObject; HV:THV; list:TList<THVPomCV>; new:TPomStatus; ok: TCb; err: TCb);

     procedure AcquireAllLocos();
     procedure ReleaseAllLocos();
     procedure FastResetLocos();

     procedure TurnOffFunctions(callback:TNotifyEvent);
     procedure Update();

     function NearestLowerSpeed(speed:Cardinal):Cardinal;

     property OnReady:TReadyEvent read eOnReady write eOnReady;
     property OnOpenError:TErrorEvent read eOnOpenError write eOnOpenError;

     property logLevelFile: TTrkLogLevel read mLogLevelFile write SetLoglevelFile;
     property logLevelTable: TTrkLogLevel read mLogLevelTable write SetLoglevelTable;

     property ready:boolean read aready;
     property libDir:string read fLibDir;

  end;//TTrkGUI

var
  TrakceI: TTrakce;

////////////////////////////////////////////////////////////////////////////////

implementation

uses fMain, fSettings, TechnologieRCS, fRegulator, SprDb, Souprava,
    GetSystems, THVDatabase, fAdminForm, DataHV, Prevody, TBloky, RegulatorTCP,
    TCPServerOR, fFuncsSet;

////////////////////////////////////////////////////////////////////////////////

constructor TTrakce.Create();
begin
 inherited;

 Self.mLogLevelFile := _DEF_LOGLEVEL;
 Self.mLogLevelTable := _DEF_LOGLEVEL;
 Self.LogObj := nil;
 Self.aReady := false;

 Self.Log(llInfo, 'BEGIN '+
                  'loglevel_file='+LogLevelToString(Self.logLevelFile)+
                  ', loglevel_table='+LogLevelToString(Self.logLevelTable)
 );

 Self.turnoff_callback := nil;
 Self.DCCGoTime := Now;

 Self.toggleQueue := TQueue<THVFunc>.Create();

 TTrakceIFace(Self).OnLog := Self.TrkLog;
end;

destructor TTrakce.Destroy();
begin
 try
   Self.Log(llInfo, 'END');
 except

 end;

 Self.FlushToggleQueue();
 FreeAndNil(Self.toggleQueue);

 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrakce.LoadLib(filename:string);
var str, tmp, libName:string;
begin
 libName := ExtractFileName(filename);

 if (not FileExists(filename)) then
   raise Exception.Create('Library file not found, not loading');

 if (Self.ready) then
  begin
   Self.aReady := false;
   if (Assigned(Self.OnReady)) then Self.OnReady(Self, Self.aReady);
  end;

 TTrakceIFace(Self).LoadLib(filename);

 Log(llInfo, 'Načtena knihovna '+ libName);
 F_Main.SB1.Panels.Items[3].Text := libName;

 if (Self.unbound.Count = 0) then
  begin
   Self.aReady := true;
   if (Assigned(Self.OnReady)) then Self.OnReady(Self, Self.aReady);
  end else begin
   str := '';
   for tmp in Self.unbound do
     str := str + tmp + ', ';
   str := LeftStr(str, Length(str)-2);
   F_Main.LogStatus('ERR: Trakce: nepodařilo se svázat následující funkce : ' + str);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrakce.Log(logLevel:TTrkLogLevel; msg:string);
var LV_Log:TListItem;
    f:TextFile;
    xDate, xTime:string;
    output:string;
    b:Byte;
 begin
  if ((logLevel > Self.logLevelFile) and (logLevel > Self.logLevelTable)) then Exit();

  DateTimeToString(xDate, 'yy_mm_dd', Now);
  DateTimeToString(xTime, 'hh:mm:ss,zzz', Now);

  if ((logLevel <= Self.logLevelTable) and (Self.LogObj <> nil)) then
   begin
    if (Self.LogObj.Items.Count > _MAX_LOGTABLE_ITEMS) then
      Self.LogObj.Clear();

    try
      LV_Log := Self.LogObj.Items.Insert(0);
      LV_Log.Caption := xTime;
      LV_Log.SubItems.Add(IntToStr(Integer(logLevel)));
      LV_Log.SubItems.Add(msg);
    except

    end;
   end;

  if (logLevel <= Self.logLevelFile) then
   begin
    try
      AssignFile(f, _LOG_PATH+'\'+xDate+'.log');
      if (FileExists(_LOG_PATH+'\'+xDate+'.log')) then
        Append(f)
      else
        Rewrite(f);

      output := xTime + ' [' +IntToStr(Integer(logLevel))+'] ' + msg + #13#10;
      for b in TEncoding.UTF8.GetBytes(output) do
        Write(f, AnsiChar(b));

      CloseFile(f);
    except

    end;
   end;
end;

procedure TTrakce.TrkLog(Sender:TObject; lvl:TTrkLogLevel; msg:string);
begin
 Self.Log(lvl, msg);
 if ((Self.opening) and (lvl = llErrors) and (Assigned(Self.OnOpenError))) then
  begin
   Self.opening := false;
   Self.OnOpenError(Self, msg);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrakce.LoadFromFile(ini:TMemIniFile);
var lib:string;
begin
  fLibDir := ini.ReadString(_INIFILE_SECTNAME, 'dir', '.');
  lib := ini.ReadString(_INIFILE_SECTNAME, 'lib', _DEFAULT_LIB);
  Self.mLogLevelFile := TTrkLogLevel(ini.ReadInteger(_INIFILE_SECTNAME, 'loglevelFile', Integer(_DEF_LOGLEVEL)));
  Self.mLogLevelTable := TTrkLogLevel(ini.ReadInteger(_INIFILE_SECTNAME, 'loglevelTable', Integer(_DEF_LOGLEVEL)));

  try
    Self.LoadLib(fLibDir + '\' + lib);
  except
    on E:Exception do
      Self.Log(llErrors, 'Nelze načíst knihovnu ' + fLibDir + '\' + lib + ', ' + E.Message);
  end;
end;

procedure TTrakce.SaveToFile(ini:TMemIniFile);
begin
  if (Self.Lib <> '') then
    ini.WriteString(_INIFILE_SECTNAME, 'lib', ExtractFileName(Self.Lib));
  ini.WriteInteger(_INIFILE_SECTNAME, 'loglevelFile', Integer(Self.logLevelFile));
  ini.WriteInteger(_INIFILE_SECTNAME, 'loglevelTable', Integer(Self.logLevelTable));
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrakce.LokFuncToggle(Sender:TObject; HV:THV; fIndex:Cardinal);
begin
 HV.SetSingleFunc(fIndex, true, TrakceI.Callback(), TrakceI.Callback(), Sender);
 Self.toggleQueue.Enqueue(HVFunc(HV, fIndex, Now+EncodeTime(0, 0, 0, 500)));
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrakce.LoksSetFunc(vyznam:string; state:boolean);
var addr, i:Integer;
    cb:Pointer;
begin
{ for addr := 0 to _MAX_ADDR-1 do
  begin
   if ((HVDb[addr] = nil) or (not HVDb[addr].acquired)) then continue;

   for i := 0 to _HV_FUNC_MAX do
    if ((HVDb[addr].Data.funcVyznam[i] = vyznam) and (HVDb[addr].slotFunkce[i] <> state)) then
      begin
       HVDb.HVozidla[addr].Stav.funkce[i] := state;

       GetMem(cb, sizeof(TFuncsCallback));
       TFuncsCallback(cb^).callback_ok  := Self.Trakce.callback_ok;
       TFuncsCallback(cb^).callback_err := Self.Trakce.callback_err;
       TFuncsCallback(cb^).addr         := addr;
       TFuncsCallback(cb^).vyznam       := vyznam;
       TFuncsCallback(cb^).state        := state;

       Self.callback_ok  := TTrakce.GenerateCallback(Self.LoksSetFuncOK, cb);
       Self.callback_err := TTrakce.GenerateCallback(Self.LoksSetFuncErr, cb);

       try
         Self.LokSetFunc(Self, HVDb.HVozidla[addr], HVDb.HVozidla[addr].Stav.funkce);
       except
         Self.LoksSetFuncErr(Self, cb);
       end;

       Exit();
      end;//if vyznam = vyznam
  end;//for i

 if (Assigned(Self.Trakce.callback_ok.callback)) then Self.Trakce.callback_ok.callback(Self, Self.Trakce.callback_ok.data); }
end;

////////////////////////////////////////////////////////////////////////////////
// Speed table

procedure TTrakce.LoadSpeedTable(filename:string;var LVRych:TListView);
var i, j:Integer;
    myFile:TextFile;
    speed:Integer;
 begin
  try
    AssignFile(myFile, filename);
    Reset(myFile);
  except
    Log(llErrors, 'Chyba pri nacitani souboru s rychlostmi - nepodarilo se pristoupit k souboru! Pouzivam vychozi rychlostni tabulku.');

    // nacteme vychozi rychlostni tabulku
    for i := 0 to _MAX_SPEED do
      Self.SpeedTable[i] := _DEFAULT_SPEED_TABLE[i];
    Self.LoadSpeedTableToTable(LVRych);

    Exit();
  end;

  for i := 0 to _MAX_SPEED do
   begin
    if (Eof(myFile)) then
     begin
      Log(llErrors, 'Chyba pri nacitani souboru s rychlostmi - prilis malo radku! Doplnuji vychozi rychlostni tabulkou.');
      CloseFile(myFile);
      for j := i to _MAX_SPEED do
        Self.SpeedTable[j] := _DEFAULT_SPEED_TABLE[j];
      Self.LoadSpeedTableToTable(LVRych);
      Exit();
     end else begin
      try
        ReadLn(myFile, speed);
        Self.SpeedTable[i] := speed;
      except
        on E:Exception do
         begin
          Log(llErrors, 'Soubor s rychlostmi, řádek ' + IntToStr(i+1) + ': ' + E.Message);
          Self.SpeedTable[i] := _DEFAULT_SPEED_TABLE[i];
         end;
      end;
     end;
   end;//while

  CloseFile(myFile);
  Self.LoadSpeedTableToTable(LVRych);
end;

procedure TTrakce.LoadSpeedTableToTable(var LVRych:TListView);
var i:Integer;
    LI:TListItem;
begin
 LVrych.Clear();

 for i := 0 to _MAX_SPEED do
  begin
   LI := LVRych.Items.Add;
   LI.Caption := IntToStr(i);
   LI.SubItems.Add(IntToStr(Self.SpeedTable[i])+' km/h');
  end;
end;

procedure TTrakce.SaveSpeedTable(filename:string);
var i:Integer;
    myFile:TextFile;
 begin
  try
    AssignFile(myFile, filename);
    Rewrite(myFile);
  except
    Log(llErrors, 'Chyba pri ukladani souboru s rychlostmi - nepodarilo se pristoupit k souboru !');
    Exit();
  end;

  for i := 0 to _MAX_SPEED do
    WriteLn(myFile, IntToStr(Self.SpeedTable[i]));

  CloseFile(myFile);
end;

function TTrakce.SpeedStep(kmph:Cardinal):Cardinal;
var i:Integer;
begin
 for i := 0 to  _MAX_SPEED do
  if (Self.SpeedTable[i] = kmph) then
    Exit(i);
 Exit(1); // v pripade nenalezeni rychlosti vraci nouzovy STOP
end;//fucntion

function TTrakce.GetStepSpeed(step:byte):Integer;
begin
 if (step >  _MAX_SPEED) then Exit(-1);
 Result := Self.SpeedTable[step];
end;

function TTrakce.SetStepSpeed(step:byte;sp:Integer):Byte;
begin
 if (step >  _MAX_SPEED) then Exit(1);
 Self.SpeedTable[step] := sp;
 Result := 0;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrakce.AcquireAllLocos();
var i:Integer;
    data:Pointer;
    k_prevzeti:Integer;
begin
{ k_prevzeti := 0;
 for i := 0 to _MAX_ADDR-1 do
  begin
   if (HVDb.HVozidla[i] = nil) then continue;
   if ((HVDb.HVozidla[i].Slot.Prevzato) and (HVDb.HVozidla[i].Slot.pom = pc)) then continue;
   if (HVDb.HVozidla[i].Stav.souprava > -1) then Inc(k_prevzeti);
  end;

 F_Main.G_Loko_Prevzato.MaxValue  := k_prevzeti;
 F_Main.G_Loko_Prevzato.Progress  := 0;
 F_Main.G_Loko_Prevzato.ForeColor := clBlue;

 for i := 0 to _MAX_ADDR-1 do
  begin
   if (HVDb.HVozidla[i] = nil) then continue;
   if ((HVDb.HVozidla[i].Slot.Prevzato) and (HVDb.HVozidla[i].Slot.pom = pc)) then continue;
   if (HVDb.HVozidla[i].Stav.souprava > -1) then
    begin
     GetMem(data, sizeof(integer));
     Integer(data^) := i;
     Self.callback_err := TTrakce.GenerateCallback(Self.PrebiraniUpdateErr, data);
     Self.callback_ok  := TTrakce.GenerateCallback(Self.PrebiraniUpdateOK, data);
     try
       Self.PrevzitLoko(HVDb.HVozidla[i]);
     except
       Self.PrebiraniUpdateErr(self, data);
     end;
     Exit();
    end;
  end;

 F_Main.LogStatus('Loko: žádné loko k převzetí');
 F_Main.G_Loko_Prevzato.MaxValue := 1;
 F_Main.G_Loko_Prevzato.Progress := 1;
 Self.AllPrevzato(); }
end;

procedure TTrakce.ReleaseAllLocos();
var i:Integer;
    data:Pointer;
    k_odhlaseni:Integer;
begin
{ k_odhlaseni := 0;
 for i := 0 to _MAX_ADDR-1 do
  begin
   if (HVDb.HVozidla[i] = nil) then continue;
   if (HVDb.HVozidla[i].Slot.Prevzato) then Inc(k_odhlaseni);
  end;

 F_Main.G_Loko_Prevzato.MaxValue  := k_odhlaseni;
 F_Main.G_Loko_Prevzato.Progress  := F_Main.G_Loko_Prevzato.MaxValue;
 F_Main.G_Loko_Prevzato.ForeColor := clBlue;

 for i := 0 to _MAX_ADDR-1 do
  begin
   if (HVDb.HVozidla[i] = nil) then continue;
   if (HVDb.HVozidla[i].Slot.Prevzato) then
    begin
     GetMem(data, sizeof(integer));
     Integer(data^) := i;
     Self.callback_err := TTrakce.GenerateCallback(Self.OdhlasovaniUpdateErr, data);
     Self.callback_ok  := TTrakce.GenerateCallback(Self.OdhlasovaniUpdateOK, data);
     Self.OdhlasitLoko(HVDb.HVozidla[i]);
     Exit();
    end;
  end;

 F_Main.LogStatus('Loko: žádné loko k odhlášení');
 F_Main.G_Loko_Prevzato.MaxValue := 1;
 F_Main.G_Loko_Prevzato.Progress := 0;
 Self.AllOdhlaseno(); }
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrakce.FastResetLocos();
var i:Integer;
begin
 for i := 0 to _MAX_ADDR-1 do
  begin
   if (HVDb[i] = nil) then continue;
   if (HVDb[i].acquired) then
    begin
     HVDb[i].stav.acquired := false;
     HVDb[i].stav.pom := TPomStatus.released;
    end;
  end;
{ Self.AllOdhlaseno(); }
end;

////////////////////////////////////////////////////////////////////////////////

//event s TTrakce, ktery se zavola pri uspesnem pripojeni ci odhlaseni loko
// v DATA jsou ulozena data callbacku, ktery prislusi prikazu pro prevzeti
{ procedure TTrakce.ConnectChange(Sender: TObject; addr:Integer; code:TConnect_code; data:Pointer);
var data2:Pointer;
    reg:THVRegulator;
begin
 // existuje u nas HV vubec ?
 if (HVDb.HVozidla[addr] = nil) then Exit;

 //nastavit vlastnosti
 case (code) of
   TConnect_code.TC_Connected:begin

     // 1) aktualizace slotu
     HVDb.HVozidla[addr].Slot := Self.Trakce.Slot;
     HVDb.HVozidla[addr].Slot.prevzato_full := false;
     HVDb.HVozidla[addr].Slot.Prevzato := true;
     HVDb.HVozidla[addr].Slot.stolen   := false;
     Self.TrkLog(self, tllCommand, 'GET LOCO DATA: loko '+HVDb.HVozidla[addr].data.Nazev+' ('+IntToSTr(addr)+')');
     HVDb.HVozidla[addr].changed := true;

     // 2) priprava POM (POM zatim neprogramujeme, jen si pripravime flag)
     HVDb.HVozidla[addr].Slot.pom := progr;

     // 3) pokracujeme v prebirani, dalsi faze je ziskani stavu funkci 13-28

     // 4) aktualni stav zpropagujeme do celeho programu
     if (HVDb.HVozidla[addr].Stav.souprava > -1) then
       Blky.ChangeUsekWithSpr(HVDb.HVozidla[addr].Stav.souprava);

     RegCollector.ConnectChange(addr);
     HVDb.HVozidla[addr].UpdateRuc();

     // odesleme do regulatoru info o uspesne autorizaci
     // to je dobre tehdy, kdyz je loko prebirano z centraly
     if (HVDb.HVozidla[addr].ruc) then
       for reg in HVDb.HVozidla[addr].Stav.regulators do
         ORTCPServer.SendLn(reg.conn, '-;LOK;'+IntToStr(addr)+';AUTH;total;{'+HVDb.HVozidla[addr].GetPanelLokString()+'}{') // TODO: remove {
     else
       for reg in HVDb.HVozidla[addr].Stav.regulators do
         ORTCPServer.SendLn(reg.conn, '-;LOK;'+IntToStr(addr)+';AUTH;ok;{'+HVDb.HVozidla[addr].GetPanelLokString()+'}{'); // TODO: remove {

     if (data <> nil) then
      begin
       data2 := data;
      end else begin
       GetMem(data2, sizeof(TPrevzitCallback));
       TPrevzitCallback(data2^).addr := addr;
       TPrevzitCallback(data2^).callback_ok  := TTrakce.GenerateCallback(nil);
       TPrevzitCallback(data2^).callback_err := TTrakce.GenerateCallback(nil);
      end;

     // 5) nacteme stav funkci 13-28
     Self.callback_ok  := TTrakce.GenerateCallback(Self.PrevzatoFunc1328OK, data2);
     Self.callback_err := TTrakce.GenerateCallback(Self.PrevzatoErr, data2);
     Self.Trakce.LokGetFunctions(addr, 13);
   end;//TC_Connected

   TConnect_code.TC_Unavailable:begin
     // tato funkce neni na XpressNETu podporovana, proto neni dodelana
     RegCollector.ConnectChange(addr);
   end;

   TConnect_code.TC_Disconnected:begin
     HVDb.HVozidla[addr].Slot.Prevzato  := false;
   end;//TC_Connected

   TConnect_code.TC_Stolen:begin
     if (not HVDb.HVozidla[addr].Slot.prevzato) then Exit();    // tato situace muze nastat, kdyz odhlasime HV a pak si ho vezme Rocomouse
                                                                // odhlaseni HV totiz fakticky nerekne centrale, ze ji odhlasujeme

     HVDb.HVozidla[addr].Slot.Prevzato  := false;
     HVDb.HVozidla[addr].Slot.stolen    := true;
     RegCollector.Stolen(addr);

     TCPRegulator.LokStolen(HVDb.HVozidla[addr]);

     if (HVDb.HVozidla[addr].Stav.souprava > -1) then
       Blky.ChangeUsekWithSpr(HVDb.HVozidla[addr].Stav.souprava);

     HVDb.HVozidla[addr].UpdateRuc();

     // zapiseme POM rucniho rizeni
     Self.POMWriteCVs(Self, HVDb.HVozidla[addr], HVDb.HVozidla[addr].Data.POMrelease, TPomStatus.released);
   end;//TC_Connected

 end;//case

 HVDb.HVozidla[addr].changed := true;
end;                                        }

procedure TTrakce.SetLoglevelFile(ll:TTrkLogLevel);
begin
 Self.mLogLevelFile := ll;
 Log(llCommands, 'NEW loglevel_file = '+LogLevelToString(ll));

 if ((ll > llNo) and (not DirectoryExists(_LOG_PATH))) then
   if (not SysUtils.ForceDirectories(ExpandFileName(_LOG_PATH))) then
     Log(llErrors, 'Nelze vytvořit složku '+_LOG_PATH);
end;

procedure TTrakce.SetLoglevelTable(ll:TTrkLogLevel);
begin
 Self.mLogLevelTable := ll;
 Log(llCommands, 'NEW loglevel_table = '+LogLevelToString(ll));
end;

////////////////////////////////////////////////////////////////////////////////
// eventy pri prebirani vsech LOKO:

{procedure TTrakce.PrebiraniUpdateOK(Sender:TObject; Data:Pointer);
var i:Integer;
begin
 F_Main.G_Loko_Prevzato.Progress := F_Main.G_Loko_Prevzato.Progress + 1;
 F_Main.G_Loko_Prevzato.ForeColor := clBlue;

 for i := Integer(data^) to _MAX_ADDR-1 do
  begin
   if ((HVDb.HVozidla[i] = nil) or ((HVDb.HVozidla[i].Slot.Prevzato) and ((HVDb.HVozidla[i].Slot.pom = released) or (HVDb.HVozidla[i].Slot.pom = pc)))) then continue;
   if (HVDb.HVozidla[i].Stav.souprava > -1) then
    begin
     Integer(data^) := i;
     Self.callback_err := TTrakce.GenerateCallback(Self.PrebiraniUpdateErr, data);
     Self.callback_ok  := TTrakce.GenerateCallback(Self.PrebiraniUpdateOK, data);
     try
       Self.PrevzitLoko(HVDb.HVozidla[i]);
     except
       Self.PrebiraniUpdateErr(self, data);
     end;
     Exit();
    end;
  end;

 // zadne dalsi loko k prevzeti
 FreeMem(data);
 Application.ProcessMessages();
 F_Main.LogStatus('Loko: všechna loko převzata');
 Self.AllPrevzato();
end;

procedure TTrakce.PrebiraniUpdateErr(Sender:TObject; Data:Pointer);
begin
 Self.Log(tllError, 'ERR: LOKO '+ IntToStr(Integer(data^)) + ' se nepodařilo převzít');
 F_Main.LogStatus('LOKO: loko '+ IntToStr(Integer(data^)) + ' se nepodařilo převzít');

 F_Main.G_Loko_Prevzato.ForeColor := clRed;

 F_Main.S_lok_prevzato.Brush.Color  := clRed; 
 F_Main.A_All_Loko_Prevzit.Enabled  := true;
 
 if (SystemData.Status = TSystemStatus.starting) then
  begin
   SystemData.Status := TSystemStatus.null;
   F_Main.A_System_Start.Enabled := true;
   F_Main.A_System_Stop.Enabled  := true;
  end;

 Application.MessageBox(PChar('LOKO '+ IntToStr(Integer(data^)) + ' se nepodařilo převzít'), 'Chyba', MB_OK OR MB_ICONWARNING);
 FreeMem(data);
end;

////////////////////////////////////////////////////////////////////////////////
// eventy pri odhlasovani vech LOKO:

procedure TTrakce.OdhlasovaniUpdateOK(Sender:TObject; Data:Pointer);
var i:Integer;
begin
 F_Main.G_Loko_Prevzato.Progress := F_Main.G_Loko_Prevzato.Progress - 1;
 F_Main.G_Loko_Prevzato.ForeColor := clBlue;

 // loko odhlaseno -> najdeme dalsi k odhlaseni a naprogramujeme POM
 for i := Integer(data^) to _MAX_ADDR-1 do
  begin
   if (HVDb.HVozidla[i] = nil) then continue;
   if (HVDb.HVozidla[i].Slot.Prevzato) then
    begin
     Integer(data^) := i;
     Self.callback_err := TTrakce.GenerateCallback(Self.OdhlasovaniUpdateErr, data);
     Self.callback_ok  := TTrakce.GenerateCallback(Self.OdhlasovaniUpdateOK, data);
     try
       Self.OdhlasitLoko(HVDb.HVozidla[i]);
     except
       on E:Exception do
        begin
         Self.callback_err := TTrakce.GenerateCallback(nil);
         Self.callback_ok  := TTrakce.GenerateCallback(nil);
         FreeMem(data);
         F_Main.LogStatus('Výjimka: ' + E.Message);
        end;
     end;
     Exit();
    end;
  end;

 // zadne dalsi loko k odhlaseni
 FreeMem(data);
 F_Main.LogStatus('Loko: všechna loko odhlášena');
 Application.ProcessMessages();
 Self.AllOdhlaseno();
end;

procedure TTrakce.OdhlasovaniUpdateErr(Sender:TObject; Data:Pointer);
begin
 // pokud behem odhlasovani loko nastane chyba, nahlasime ji, ale loko povazujeme za odhlasene
 Self.Log(tllError, 'WARN: Loko '+IntToStr(Integer(data^))+ ' se nepodařilo odhlásit');
 F_Main.LogStatus('WARN: Loko '+IntToStr(Integer(data^))+ ' se nepodařilo odhlásit');
 F_Main.G_Loko_Prevzato.ForeColor := clRed;
 HVDb.HVozidla[Integer(data^)].Slot.prevzato := false;
 HVDb.HVozidla[Integer(data^)].Slot.prevzato_full := false;
 Self.OdhlasovaniUpdateOK(Self, data);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrakce.AllPrevzato();
begin
 F_Main.S_lok_prevzato.Brush.Color := clLime;

 F_Main.A_All_Loko_Prevzit.Enabled  := false;
 F_Main.A_All_Loko_Odhlasit.Enabled := true;

 F_Main.G_Loko_Prevzato.Progress  := HVDb.cnt;
 F_Main.G_Loko_Prevzato.ForeColor := clLime;

 if (SystemData.Status = starting) then
   F_Main.A_PanelServer_StartExecute(nil);
end;

procedure TTrakce.AllOdhlaseno();
begin
 F_Main.S_lok_prevzato.Brush.Color := clRed;

 F_Main.A_All_Loko_Prevzit.Enabled  := true;
 F_Main.A_All_Loko_Odhlasit.Enabled := false;

 F_Main.G_Loko_Prevzato.Progress  := 0;
 F_Main.G_Loko_Prevzato.ForeColor := clBlue;

 if (SystemData.Status = stopping) then
   F_Main.SetCallMethod(F_Main.A_Trk_DisconnectExecute);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrakce.LokComErr(Sender:TObject; addr:Integer);
begin
 if (not Assigned(HVDb.HVozidla[addr])) then Exit();

 if (not HVDb.HVozidla[addr].Slot.com_err) then
  begin
   HVDb.HVozidla[addr].Slot.com_err := true;
   HVDb.HVozidla[addr].changed := true;
  end;
end;

procedure TTrakce.LokComOK(Sender:TObject; addr:Integer);
begin
 if (not Assigned(HVDb.HVozidla[addr])) then Exit();

 if (HVDb.HVozidla[addr].Slot.com_err) then
  begin
   HVDb.HVozidla[addr].Slot.com_err := false;
   HVDb.HVozidla[addr].changed := true;
  end;
end;        }

////////////////////////////////////////////////////////////////////////////////

// tato funkce je volana pri vypnuti systemu / vypne u vsech hnacich vozidel zvuk
// zvuk si ale zapamatuje jako zaply pro pristi nabeh systemu
procedure TTrakce.TurnOffFunctions(callback:TNotifyEvent);
var i, j:Integer;
    func:Integer;
    newfuncs:TFunkce;
    addr:Pointer;
begin
 if (Assigned(Self.turnoff_callback)) then Exit();
 Self.turnoff_callback := callback;

 F_Main.LogStatus('Vypínám zvuky hnacích vozidel...');
 Application.ProcessMessages();

 for i := 0 to _MAX_ADDR-1 do
  begin
   if ((HVDb[i] <> nil) and (HVDb[i].acquired) and (not HVDb[i].stolen)) then
    begin
     func := -1;
     for j := 0 to _HV_FUNC_MAX do
      if ((HVDb[i].Data.funcVyznam[j] = 'zvuk') and (HVDb[i].slotFunkce[j])) then
       begin
        func := j;
        break;
       end;
     if (func = -1) then continue;

     GetMem(addr, 3);
     Integer(addr^) := i;

     newfuncs := HVDb.HVozidla[i].Stav.funkce;
     newfuncs[func] := false;

{     Self.callback_err := Trakce.GenerateCallback(Self.TurnOffFunctions_cmdOK, addr);
     Self.callback_ok  := Trakce.GenerateCallback(Self.TurnOffFunctions_cmdOK, addr);

     try
       Self.LokSetFunc(Self, HVDb.HVozidla[i], newfuncs);
       HVDb.HVozidla[i].Stav.funkce[func] := true;
     except
       Self.TurnOffFunctions_cmdOK(Self, addr);
     end; }

     Exit();
    end;
  end;//for i

 // no loco
 if Assigned(Self.turnoff_callback) then
  begin
   Self.turnoff_callback(Self);
   Self.turnoff_callback := nil;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrakce.TurnOffFunctions_cmdOK(Sender:TObject; Data:Pointer);
var i, j:Integer;
    func:Integer;
    newfuncs:TFunkce;
begin
 for i := Integer(data^)+1 to _MAX_ADDR-1 do
  begin
   if ((HVDb[i] <> nil) and (HVDb[i].acquired) and (not HVDb[i].stolen)) then
    begin
     func := -1;
     for j := 0 to _HV_FUNC_MAX do
      if ((HVDb[i].Data.funcVyznam[j] = 'zvuk') and (HVDb[i].slotFunkce[j])) then
       begin
        func := j;
        break;
       end;
     if (func = -1) then continue;

     Integer(data^) := i;

     newfuncs := HVDb.HVozidla[i].Stav.funkce;
     newfuncs[func] := false;

{     Self.callback_err := Trakce.GenerateCallback(Self.TurnOffFunctions_cmdOK, data);
     Self.callback_ok  := Trakce.GenerateCallback(Self.TurnOffFunctions_cmdOK, data);

     Self.LokSetFunc(Self, HVDb.HVozidla[i], newfuncs);
     HVDb.HVozidla[i].Stav.funkce[func] := true; }

     Exit();
    end;
  end;//for i

 // no further loco
 F_Main.LogStatus('Zvuky všech hnacích vozidel vypnuty');
 Application.ProcessMessages();

 FreeMem(data);
 if Assigned(Self.turnoff_callback) then
  begin
   Self.turnoff_callback(Self);
   Self.turnoff_callback := nil;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

// zapsat seznam vsech cv v listu
procedure TTrakce.POMWriteCVs(Sender:TObject; HV:THV; list:TList<THVPomCV>; new:TPomStatus; ok: TCb; err: TCb);
var data:Pointer;
begin
 // vytvorime si callback
{ GetMem(data, sizeof(TPOMCallback));
 TPOMCallback(data^).addr := HV.adresa;
 TPOMCallback(data^).list := list;
 TPOMCallback(data^).callback_ok  := Self.Trakce.callback_ok;
 TPOMCallback(data^).callback_err := Self.Trakce.callback_err;
 TPOMCallback(data^).index := 0;
 TPOMCallback(data^).new   := new;

 HV.Slot.pom := progr;
 HV.changed  := true;

 if (list.Count < 1) then
  begin
   Self.callback_err := TTrakce.GenerateCallback(nil);
   Self.callback_ok  := TTrakce.GenerateCallback(nil);
   Self.POMCvWroteOK(Self, data);
  end else begin
   // callback pro jednotlive pom
   Self.callback_err := TTrakce.GenerateCallback(Self.POMCvWroteErr, data);
   Self.callback_ok  := TTrakce.GenerateCallback(Self.POMCvWroteOK, data);

   try
     Self.POMWriteCV(Sender, HV, list[0].cv, list[0].data);
   except
     Self.POMCvWroteErr(Self, data);
   end;
  end; }
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrakce.POMCvWroteOK(Sender:TObject; Data:Pointer);
begin
{ if (TPOMCallback(data^).index >= (TPOMCallback(data^).list.Count-1)) then
  begin
   // posledni data -> zavolame OK event

   if (Assigned(HVDb.HVozidla[TPOMCallback(data^).addr])) then
    begin
     HVDb.HVozidla[TPOMCallback(data^).addr].Slot.pom := TPOMCallback(data^).new;
     HVDb.HVozidla[TPOMCallback(data^).addr].changed := true;
     RegCollector.ConnectChange(TPOMCallback(data^).addr);
    end;

   if (Assigned(TPOMCallback(data^).callback_ok.callback)) then
     TPOMCallback(data^).callback_ok.callback(Self, TPOMCallback(data^).callback_ok.data);
   FreeMem(data);
  end else begin
   // odesleme dalsi data
   TPOMCallback(data^).index := TPOMCallback(data^).index+1;

   Self.callback_err := TTrakce.GenerateCallback(Self.POMCvWroteErr, data);
   Self.callback_ok  := TTrakce.GenerateCallback(Self.POMCvWroteOK, data);
   Self.POMWriteCV(Sender, HVDB.HVozidla[TPOMCallback(data^).addr], TPOMCallback(data^).list[TPOMCallback(data^).index].cv,
                   TPOMCallback(data^).list[TPOMCallback(data^).index].data);
  end;// else konec dat }
end;

// pokud pri POMu nastane chyba, zavolame Error callback a ukoncime programovani
procedure TTrakce.POMCvWroteErr(Sender:TObject; Data:Pointer);
begin
{ if (Assigned(HVDb.HVozidla[TPOMCallback(data^).addr])) then
  begin
   HVDb.HVozidla[TPOMCallback(data^).addr].Slot.pom := TPomStatus.error;
   HVDb.HVozidla[TPOMCallback(data^).addr].changed  := true;
   RegCollector.ConnectChange(TPOMCallback(data^).addr);
  end;

 if (Assigned(TPOMCallback(data^).callback_err.callback)) then
  TPOMCallback(data^).callback_err.callback(Self, TPOMCallback(data^).callback_err.data);
 FreeMem(data); }
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrakce.NouzReleaseCallbackErr(Sender:TObject; Data:Pointer);
begin
{ HVDb.HVozidla[Integer(data^)].Slot.prevzato      := false;
 HVDb.HVozidla[Integer(data^)].Slot.prevzato_full := false; }
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrakce.NouzReleaseLoko();
var i:Integer;
    data:Pointer;
begin
{ GetMem(data, sizeof(integer));
 for i := 0 to _MAX_ADDR-1 do
  begin
   if (HVDb.HVozidla[i] = nil) then continue;

   try
     if (HVDb.HVozidla[i].Slot.prevzato) then
      begin
       Integer(data^) := i;
       Self.callback_err := TTrakce.GenerateCallback(Self.NouzReleaseCallbackErr, data);
       try
         Self.OdhlasitLoko(HVDb.HVozidla[i]);
       except
         on E:Exception do
           FreeMem(data);
       end;
      end;
     while (HVDb.HVozidla[i].Slot.prevzato) do
      begin
       sleep(1);
       Application.ProcessMessages;
      end;
   except

   end;
  end;
 FreeMem(data);             }
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrakce.CheckToggleQueue();
begin
 if (Self.toggleQueue.Count = 0) then Exit();

 if (Now >= Self.toggleQueue.Peek.time) then
  begin
   try
     Self.ProcessHVFunc(Self.toggleQueue.Dequeue());
   except

   end;
  end;
end;

procedure TTrakce.FlushToggleQueue();
begin
 while (Self.toggleQueue.Count > 0) do
  begin
   try
     Self.ProcessHVFunc(Self.toggleQueue.Dequeue());
   except

   end;
  end;
end;

procedure TTrakce.ProcessHVFunc(hvFunc:THVFunc);
var funkce:TFunkce;
begin
 if (hvFunc.HV = nil) then Exit();

{ funkce := hvFunc.HV.Slot.funkce;
 funkce[hvFunc.fIndex] := false;
 Self.LokSetFunc(Self, hvFunc.HV, funkce); }
end;

////////////////////////////////////////////////////////////////////////////////
///
class function TTrakce.HVFunc(HV:THV; fIndex:Cardinal; time:TDateTime):THVFunc;
begin
 Result.HV := HV;
 Result.fIndex := fIndex;
 Result.time := time;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrakce.Update();
begin
 Self.CheckToggleQueue();
end;

////////////////////////////////////////////////////////////////////////////////

function TTrakce.NearestLowerSpeed(speed:Cardinal):Cardinal;
var stupen:Integer;
begin
 for stupen := _MAX_SPEED downto 0 do
   if (Self.SpeedTable[stupen] <= speed) then
     Exit(Self.SpeedTable[stupen]);
 Result := 0;
end;

////////////////////////////////////////////////////////////////////////////////

initialization
  TrakceI := TTrakce.Create();
finalization
  TrakceI.Free();

end.//unit
