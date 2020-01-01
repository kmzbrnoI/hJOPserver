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

  // passed as a parameter to callback when programming POM
  TPOMCallback = record
    locoAddr:Word;
    toProgram:TList<THVPomCV>;
    index:Integer; // index of currently programmed CV in list 'toProgram'
    callback_ok, callback_err:TCommandCallback;
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
  TSetDescFuncsCallback = record
    addr:Word;
    description:string;
    state:boolean;
    callback_ok, callback_err:TCommandCallback;
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

     procedure TrkLog(Sender:TObject; lvl:TTrkLogLevel; msg:string);
     procedure TrkLocoStolen(Sender: TObject; addr: Word);

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

     procedure LoksSetFuncOK(Sender:TObject; Data:Pointer);
     procedure LoksSetFuncErr(Sender:TObject; Data:Pointer);

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

     procedure LoksSetFunc(description:string; state:boolean; ok: TCb; err: TCb);
     procedure POMWriteCVs(addr: Word; toProgram: TList<THVPomCV>; ok: TCb; err: TCb);

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
 TTrakceIFace(Self).OnLocoStolen := Self.TrkLocoStolen;
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

procedure TTrakce.TrkLocoStolen(Sender: TObject; addr: Word);
begin
 if (HVDb[addr] <> nil) then
   HVDb[addr].TrakceStolen();
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
// LoksSetFunc
////////////////////////////////////////////////////////////////////////////////

procedure TTrakce.LoksSetFunc(description:string; state:boolean; ok: TCb; err: TCb);
var cb: ^TSetDescFuncsCallback;
begin
 GetMem(cb, sizeof(TSetDescFuncsCallback));
 cb^.callback_ok := ok;
 cb^.callback_err := err;
 cb^.addr := 0;
 cb^.description := description;
 cb^.state := state;

 Self.LoksSetFuncOK(Self, cb);
end;

procedure TTrakce.LoksSetFuncOK(Sender:TObject; Data:Pointer);
var addr: Word;
    cb: ^TSetDescFuncsCallback;
begin
 cb := Data;
 addr := cb^.addr;

 if (not Self.ConnectedSafe()) then
   Exit();

 while ((addr < _MAX_ADDR) and ((HVDb[addr] = nil) or (not HVDb[addr].acquired) or
        (not HVDb[addr].funcDict.ContainsKey(cb^.description)) or
        (HVDb[addr].slotFunkce[HVDb[addr].funcDict[cb^.description]] = cb^.state))) do
   Inc(addr);

 if (addr = _MAX_ADDR) then
  begin
   if (Assigned(cb^.callback_ok.callback)) then
     cb^.callback_ok.callback(Self, cb^.callback_ok.data);
   FreeMem(cb);
   Exit();
  end;

 cb^.addr := addr+1;
 try
   HVDb[addr].SetSingleFunc(HVDb[addr].funcDict[cb^.description], cb^.state,
                            TTrakce.Callback(Self.LoksSetFuncOK, cb),
                            TTrakce.Callback(Self.LoksSetFuncErr, cb));
 except
   Self.LoksSetFuncErr(Self, cb);
 end;
end;

procedure TTrakce.LoksSetFuncErr(Sender:TObject; Data:Pointer);
var cb: ^TSetDescFuncsCallback;
begin
 cb := Data;
 if (Assigned(cb^.callback_err.callback)) then
   cb^.callback_err.callback(Self, cb^.callback_err.data);
 FreeMem(cb);
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

{procedure TTrakce.AcquireAllLocos();
var i:Integer;
    data:Pointer;
    k_prevzeti:Integer;
begin}
{ k_prevzeti := 0;
 for i := 0 to _MAX_ADDR-1 do
  begin
   if (HVDb[i] = nil) then continue;
   if ((HVDb[i].Slot.Prevzato) and (HVDb[i].Slot.pom = pc)) then continue;
   if (HVDb[i].Stav.souprava > -1) then Inc(k_prevzeti);
  end;

 F_Main.G_Loko_Prevzato.MaxValue  := k_prevzeti;
 F_Main.G_Loko_Prevzato.Progress  := 0;
 F_Main.G_Loko_Prevzato.ForeColor := clBlue;

 F_Main.G_Loko_Prevzato.MaxValue := 1;
 F_Main.G_Loko_Prevzato.Progress := 1;

procedure TTrakce.ReleaseAllLocos();
var i:Integer;
    data:Pointer;
    k_odhlaseni:Integer;
begin}
{ k_odhlaseni := 0;
 for i := 0 to _MAX_ADDR-1 do
  begin
   if (HVDb[i] = nil) then continue;
   if (HVDb[i].Slot.Prevzato) then Inc(k_odhlaseni);
  end;

 F_Main.G_Loko_Prevzato.MaxValue  := k_odhlaseni;
 F_Main.G_Loko_Prevzato.Progress  := F_Main.G_Loko_Prevzato.MaxValue;
 F_Main.G_Loko_Prevzato.ForeColor := clBlue;

 F_Main.LogStatus('Loko: žádné loko k odhlášení');
 F_Main.G_Loko_Prevzato.MaxValue := 1;
 F_Main.G_Loko_Prevzato.Progress := 0;
 Self.AllOdhlaseno(); }

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

     newfuncs := HVDb[i].Stav.funkce;
     newfuncs[func] := false;

{     Self.callback_err := Trakce.GenerateCallback(Self.TurnOffFunctions_cmdOK, addr);
     Self.callback_ok  := Trakce.GenerateCallback(Self.TurnOffFunctions_cmdOK, addr);

     try
       Self.LokSetFunc(Self, HVDb[i], newfuncs);
       HVDb[i].Stav.funkce[func] := true;
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

     newfuncs := HVDb[i].Stav.funkce;
     newfuncs[func] := false;

{     Self.callback_err := Trakce.GenerateCallback(Self.TurnOffFunctions_cmdOK, data);
     Self.callback_ok  := Trakce.GenerateCallback(Self.TurnOffFunctions_cmdOK, data);

     Self.LokSetFunc(Self, HVDb[i], newfuncs);
     HVDb[i].Stav.funkce[func] := true; }

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
// POM
////////////////////////////////////////////////////////////////////////////////

procedure TTrakce.POMWriteCVs(addr: Word; toProgram: TList<THVPomCV>; ok: TCb; err: TCb);
var data: ^TPomCallback;
begin
 if (toProgram.Count = 0) then
  begin
   if (Assigned(ok.callback)) then
     ok.callback(Self, ok.data);
   Exit();
  end;

 GetMem(data, sizeof(TPOMCallback));
 data^.locoAddr := addr;
 data^.toProgram := toProgram;
 data^.callback_ok := ok;
 data^.callback_err := err;
 data^.index := 0;

 try
   Self.POMWriteCV(addr, toProgram[0].cv, toProgram[0].data,
                   TTrakce.Callback(Self.POMCvWroteOK, data),
                   TTrakce.Callback(Self.POMCvWroteErr, data));
 except
   Self.POMCvWroteErr(Self, data);
 end;
end;

procedure TTrakce.POMCvWroteOK(Sender:TObject; Data:Pointer);
var pomData: ^TPomCallback;
begin
 pomData := Data;

 if (pomData.index >= (pomData.toProgram.Count-1)) then
  begin
   // last POM
   if (Assigned(pomData.callback_ok.callback)) then
     pomData.callback_ok.callback(Self, pomData.callback_ok.data);
   FreeMem(data);
  end else begin
   // send next data
   pomData.index := pomData.index+1;

   Self.POMWriteCV(pomData.locoAddr, pomData.toProgram[pomData.index].cv,
                   pomData.toProgram[pomData.index].Data,
                   TTrakce.Callback(Self.POMCvWroteOK, data),
                   TTrakce.Callback(Self.POMCvWroteErr, data));
  end;
end;

procedure TTrakce.POMCvWroteErr(Sender:TObject; Data:Pointer);
var pomData: ^TPomCallback;
begin
 pomData := Data;
 if (Assigned(pomData.callback_err.callback)) then
   pomData.callback_err.callback(Self, pomData.callback_err.data);
 FreeMem(data);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrakce.NouzReleaseCallbackErr(Sender:TObject; Data:Pointer);
begin
{ HVDb[Integer(data^)].Slot.prevzato      := false;
 HVDb[Integer(data^)].Slot.prevzato_full := false; }
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrakce.NouzReleaseLoko();
var i:Integer;
    data:Pointer;
begin
{ GetMem(data, sizeof(integer));
 for i := 0 to _MAX_ADDR-1 do
  begin
   if (HVDb[i] = nil) then continue;

   try
     if (HVDb[i].Slot.prevzato) then
      begin
       Integer(data^) := i;
       Self.callback_err := TTrakce.GenerateCallback(Self.NouzReleaseCallbackErr, data);
       try
         Self.OdhlasitLoko(HVDb[i]);
       except
         on E:Exception do
           FreeMem(data);
       end;
      end;
     while (HVDb[i].Slot.prevzato) do
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
