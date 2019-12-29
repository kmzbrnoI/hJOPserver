unit TechnologieTrakce;

{
 Trida TTrakce zpristupnuje rizeni trakce zbytku programu. Rizeni trakce je
 reseno dynamicky linkovanou knihovnou.
}

interface

uses
  SysUtils, Classes, StrUtils, CPort, Trakce, ComCtrls, Graphics, Forms, Windows,
  THnaciVozidlo, Generics.Collections, Contnrs;

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
  TLogEvent = procedure(Sender:TObject; lvl:TTrkLogLevel; msg:string; var handled:boolean) of object;
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

   private
     LogObj:TListView;
     aReady:boolean;
     SpeedTable:array [0.._MAX_SPEED] of Cardinal;
     turnoff_callback:TNotifyEvent;                                             // callback volany po prikazu TurnOff
                                                                                //   Prikaz TurnOff zapina F0 a vypina vsechny vyssi funkce
                                                                                //   pouziva se pri vypinani systemu (pro vypnuti otravnych zvuku zvukovych dekoderu)

     //events definition
     FOnLog: TLogEvent;                                                         // log event

     flogfile : TTrkLogLevel;
     flogtable: TTrkLogLevel;
     finitok  : boolean;                                                        // je true, pokud po otevreni seriaku centrala zakomujikovala (tj. odpovedela na prikaz)
                                                                                // pri .Open se nastavuje na false
     procedure LoadLib(filename:string);

     //////////////////////////////////////

     procedure TrkLog(Sender:TObject; lvl:TTrkLogLevel; msg:string);            // logovaci event z .Trakce

     procedure NouzReleaseLoko();
//     procedure LokComErr(Sender:TObject; addr:Integer);                         // event oznamujici chybu komunikace s danou lokomotivou (je volan paralelne s error callback eventem, ale jen pro urcite prikazy - pro prikazy tykajici se rizeni konkretni lokomotivy).
//     procedure LokComOK(Sender:TObject; addr:Integer);                          // event potvrzujici komunikaci s danym HV (je volan pokazde, pokud centrala odpovi na prikaz o nasatveni dat HV)
                                                                                // je protipol predchoziho eventu

     // eventy z .Trakce pri zapinani systemu (tj. odpoved na priklad GET-STATUS)
//     procedure InitStatErr(Sender:TObject; Data:Pointer);
//    procedure InitStatOK(Sender:TObject; Data:Pointer);

     // event z .Tracke pri ozivovani systemu po neuspesnem prikazu GET-STATUS
     // oziveni - prikaz DCC STOP; tyto eventy tedy odpovidaji inicializacnimu prikazu STOP
//     procedure InitStopErr(Sender:TObject; Data:Pointer);
//     procedure InitStopOK(Sender:TObject; Data:Pointer);

     // Obecne nastaveni callback komunikacnich eventu
     // Tyto funkce nastavi callback eventy do .Trakce
//     procedure SetCallbackErr(callback_err:TCommandCallback);
//     procedure SetCallbackOK(callback_ok:TCommandCallback);

     // eventy z komunikace s centralou pri prebirani a odhlasovani HV (tj. prebirani a odhlasovani VSECH LOKO)
//     procedure PrebiraniUpdateOK(Sender:TObject; Data:Pointer);                 // loko uspesne prevzato
//     procedure PrebiraniUpdateErr(Sender:TObject; Data:Pointer);                // prevzeti se nazdarilo (napr. centrala neodpovedela na prikaz o prevzeti, na POM ...)
//     procedure OdhlasovaniUpdateOK(Sender:TObject; Data:Pointer);               // loko uspesne uvolneno
//     procedure OdhlasovaniUpdateErr(Sender:TObject; Data:Pointer);              // uvolneni loko se nezdarilo (napr. centrala nedopovedela na POM, ...)

     // eventy spojene s jednotlivymi fazemi prebirani HV
//     procedure AcquireErr(Sender:TObject; Data:Pointer);

//     procedure AcquiredFunc1328(Sender:TObject; Data:Pointer);
//     procedure AcquiredDirection(Sender:TObject; Data:Pointer);
//     procedure AcquiredPOM(Sender:TObject; Data:Pointer);
//     procedure AcquiredFunc(Sender:TObject; Data:Pointer);
//     procedure AcquireFuncErr(Sender:TObject; Data:Pointer);

     // eventy spojene s jedntlivymi fazemi odhlasovani loko:
//     procedure Released(Sender:TObject; Data:Pointer);
//     procedure ReleaseErr(Sender:TObject; Data:Pointer);
//     procedure ReleasePOMOK(Sender:TObject; Data:Pointer);
//     procedure ReleasePOMErr(Sender:TObject; Data:Pointer);

     // callbacky spojene s nastavovanim funkci:
     // Funkce nastavujeme po jednotlivych sadach.
     // K nastaveni dalsi sady dojde az po uspesnem nastaveni sady predchozi - tj. prichodu prikazu OK z centraly, resp. zavolani OK callbacku
//     procedure FuncOK(Sender:TObject; Data:Pointer);
//     procedure FuncErr(Sender:TObject; Data:Pointer);

//     procedure AllPrevzato();                                                   // je volana, pokud jsou vsechny loko prevzaty (primarni vyuziti = interakce s GUI)
//     procedure AllOdhlaseno();                                                  // je volana, pokud jsou vsechny loko odhlaseny (primarni vyuziti = interakce s GUI)

     procedure OnTrackStatusChange(Sender: TObject);                            // event volany z .Trakce pri zmene TrackStatus (napr. CENTRAL-STOP, SERVICE, ...)
                                                                                // novy status je k dispozici v .Trakce.TrackStatus

     procedure TurnOffFunctions_cmdOK(Sender:TObject; Data:Pointer);            // OK callback pro prikaz TurnOff

     // eventy spojene se zapisem jednotlivych POM:
     procedure POMCvWroteOK(Sender:TObject; Data:Pointer);
     procedure POMCvWroteErr(Sender:TObject; Data:Pointer);

     // chyba pri nouzovem uvolnovani HV
     //   Nouzzove jsou uvolnena takove HV, ktera jsou pri BeforeClose jeste prevzata.
     procedure NouzReleaseCallbackErr(Sender:TObject; Data:Pointer);

//     procedure LoksSetFuncOK(Sender:TObject; Data:Pointer);
//     procedure LoksSetFuncErr(Sender:TObject; Data:Pointer);

     procedure SetLoglevelFile(ll:TTrkLogLevel);
     procedure SetLoglevelTable(ll:TTrkLogLevel);

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

     constructor Create();
     destructor Destroy(); override;

     procedure Log(loglevel:TTrkLogLevel; msg:string);


     procedure LokFuncToggle(Sender:TObject; HV:THV; fIndex:Cardinal);

     procedure LoadSpeedTable(filename:string;var LVRych:TListView);
     procedure SaveSpeedTable(filename:string);

     function SpeedStep(kmph:Cardinal):Cardinal;
     function GetStepSpeed(step:byte):Integer;
     function SetStepSpeed(step:byte; sp:Integer):Byte;

//     procedure LocoAcquire(HV:THV);
//     procedure LocoRelease(HV:THV);

     procedure LoksSetFunc(vyznam:string; state:boolean);
     procedure POMWriteCVs(Sender:TObject; HV:THV; list:TList<THVPomCV>; new:TPomStatus);

     procedure AcquireAllLocos();
     procedure ReleaseAllLocos();
     procedure FastResetLocos();

     procedure TurnOffFunctions(callback:TNotifyEvent);
     procedure Update();

     function NearestLowerSpeed(speed:Cardinal):Cardinal;

     property OnLog: TLogEvent read FOnLog write FOnLog;
     property logfile:TTrkLogLevel read flogfile write SetLoglevelFile;
     property logtable:TTrkLogLevel read flogtable write SetLoglevelTable;
     property ready:boolean read aready;

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

 Self.flogfile  := _DEF_LOGLEVEL;
 Self.flogtable := _DEF_LOGLEVEL;
 Self.LogObj := nil;
 Self.aReady := false;

 Self.Log(llInfo, 'BEGIN '+
                  'loglevel_file='+LogLevelToString(Self.logfile)+
                  ', loglevel_table='+LogLevelToString(Self.logtable)
 );

 Self.turnoff_callback := nil;
 Self.DCCGoTime := Now;

 Self.toggleQueue := TQueue<THVFunc>.Create();

 // TODO: assign events
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
   Self.aReady := false;

 TTrakceIFace(Self).LoadLib(filename);

 Log(llInfo, 'Načtena knihovna '+ libName);
 F_Main.SB1.Panels.Items[3].Text := libName;

 if (Self.unbound.Count = 0) then
  begin
   Self.aReady := true;
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
  if ((logLevel > Self.logfile) and (logLevel > Self.logtable)) then Exit;

  DateTimeToString(xDate, 'yy_mm_dd', Now);
  DateTimeToString(xTime, 'hh:mm:ss,zzz', Now);

  if (Self.LogObj.Items.Count > _MAX_LOGTABLE_ITEMS) then
    Self.LogObj.Clear();

  if (logLevel <= Self.logtable) then
   begin
    try
      LV_Log := Self.LogObj.Items.Insert(0);
      LV_Log.Caption := xTime;
      LV_Log.SubItems.Add(IntToStr(Integer(logLevel)));
      LV_Log.SubItems.Add(msg);
    except

    end;
   end;//if Self.logtable

  if (logLevel <= Self.logfile) then
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
var handled:boolean;
begin
 handled := false;
 if (Assigned(Self.FOnLog)) then Self.FOnLog(self, lvl, msg, handled);

 if (handled) then Exit;
 Self.Log(lvl, msg);
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

{ procedure TTrakce.LocoAcquire(HV:THV);
var cb:Pointer;
begin
 if (not Self.openned) then
  begin
   Self.Log(tllError, 'ERR: COM not openned');
   raise ENotOpenned.Create('COM not openned');
  end;

 Self.TrkLog(self, tllCommand, 'PUT: LOK-2-MYCONTROL: '+HV.data.Nazev+' ('+IntToStr(HV.Adresa)+')');
 HV.Slot.prevzato_full := false;
 HV.RecordUseNow();

 GetMem(cb, sizeof(TPrevzitCallback));
 TPrevzitCallback(cb^).callback_ok  := Self.Trakce.callback_ok;
 TPrevzitCallback(cb^).callback_err := Self.Trakce.callback_err;
 TPrevzitCallback(cb^).addr         := HV.adresa;

 if (not HV.Slot.prevzato) then
  begin
   // ok callback neni potreba, protoze se vola ConnectChange
   Self.callback_ok  := TTrakce.GenerateCallback(nil, cb);    // cb tu ale presto musi byt (je potreba v ConnectChange)
   Self.callback_err := TTrakce.GenerateCallback(Self.PrevzatoErr, cb);
   Self.Trakce.Lok2MyControl(HV.Adresa);
  end else begin
   Self.callback_err := TTrakce.GenerateCallback(nil);
   Self.callback_ok  := TTrakce.GenerateCallback(nil);
   Self.ConnectChange(Self, HV.adresa, Tconnect_code.TC_Connected, cb);
  end;
end;

procedure TTrakce.LocoRelease(HV:THV);
var cb:Pointer;
begin
 if (not Self.openned) then
  begin
   Self.Log(tllError, 'ERR: COM not openned');
   raise ENotOpenned.Create('COM not openned');
  end;

 Self.TrkLog(self, tllCommand, 'PUT: LOK-FROM-MYCONTROL: '+HV.data.Nazev+' ('+IntToStr(HV.Adresa)+')');

 GetMem(cb, sizeof(TPrevzitCallback));
 TPrevzitCallback(cb^).callback_ok  := Self.Trakce.callback_ok;
 TPrevzitCallback(cb^).callback_err := Self.Trakce.callback_err;
 TPrevzitCallback(cb^).addr         := HV.adresa;

 Self.callback_ok  := TTrakce.GenerateCallback(Self.OdhlasenoPOMOK, cb);
 Self.callback_err := TTrakce.GenerateCallback(Self.OdhlasenoPOMErr, cb);

 // nenastavovat HV.ruc, POM si tady delame sami !!
 HV.Stav.ruc := false;
 HV.RecordUseNow();

 if (HV.Slot.pom <> TPomStatus.released) then
   Self.POMWriteCVs(Self, HV, HV.Data.POMrelease, TPomStatus.released)
 else
   Self.OdhlasenoPOMOK(Self, cb);
end; }

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
 Self.flogfile := ll;
 Log(llCommands, 'NEW loglevel_file = '+LogLevelToString(ll));
end;

procedure TTrakce.SetLoglevelTable(ll:TTrkLogLevel);
begin
 Self.flogtable := ll;
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
// callback funkce pri prebirani jednoho HV a pri programovani POM (pri prebirani):

procedure TTrakce.PrevzatoErr(Sender:TObject; Data:Pointer);
begin
 // loko se nepodarilo prevzit -> zavolat error callback
 if (Assigned(TPrevzitCallback(data^).callback_err.callback)) then
   TPrevzitCallback(data^).callback_err.callback(Self, TPrevzitCallback(data^).callback_err.data);
 FreeMem(data);
end;

procedure TTrakce.PrevzatoPOMOK(Sender:TObject; Data:Pointer);
begin
 // HV konecne kompletne prevzato
 HVDb.HVozidla[TPrevzitCallback(data^).addr].Slot.prevzato_full := true;

 // volame OK callback
 if (Assigned(TPrevzitCallback(data^).callback_ok.callback)) then
   TPrevzitCallback(data^).callback_ok.callback(Self, TPrevzitCallback(data^).callback_ok.data);
 FreeMem(data);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrakce.PrevzatoFunc1328OK(Sender:TObject; Data:Pointer);
var i:Integer;
    HV:THV;
    smer:Integer;
begin
 HV := HVDb.HVozidla[TPrevzitCallback(data^).addr];

 // nacetli jsme stav funkci 13-28 -> stav ulozime do slotu
 for i := 13 to 28 do
   HV.Slot.funkce[i] := Self.Trakce.Slot.funkce[i];

 // pokud ma souprava jasne dany smer, nastavime ho
 // podminka na sipky je tu kvuli prebirani z RUCniho rizeni z XpressNETu
 if ((HV.Stav.souprava > -1) and
     (Soupravy[HV.Stav.souprava].sdata.smer_L xor Soupravy[HV.Stav.souprava].sdata.smer_S)) then
  begin
   // souprava ma zadany prave jeden smer
   smer := (Integer(Soupravy[HV.Stav.souprava].smer) xor Integer(HV.Stav.StanovisteA));
   if ((smer = HV.Slot.smer) and (HV.Slot.speed = 0)) then
    begin
     // smer ok
     Self.PrevzatoSmerOK(Sender, Data);
     Exit();
    end else begin
     // smer nok -> aktualizovat smer
     Self.callback_ok  := TTrakce.GenerateCallback(Self.PrevzatoSmerOK, data);
     Self.callback_err := TTrakce.GenerateCallback(Self.PrevzatoErr, data);

     try
       Self.LokSetDirectSpeed(nil, HV, 0, smer);
     except
       Self.PrevzatoErr(Self, data);
     end;
    end;
  end else
   Self.PrevzatoSmerOK(Sender, Data);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrakce.PrevzatoSmerOK(Sender:TObject; Data:Pointer);
begin
 // nastavime funkce tak, jak je chceme my
 Self.callback_ok  := TTrakce.GenerateCallback(Self.PrevzatoFuncOK, data);
 Self.callback_err := TTrakce.GenerateCallback(Self.PrevzatoFuncErr, data);

 try
   Self.LokSetFunc(nil, HVDb.HVozidla[TPrevzitCallback(data^).addr], HVDb.HVozidla[TPrevzitCallback(data^).addr].Stav.funkce);
 except
   Self.PrevzatoFuncErr(Self, data);
 end;
end;

////////////////////////////////////////////////////////////////////////////////
// callback funkce pri odhlasovani hnaciho vozidla a pri programovani POM pri odhlasovani:

procedure TTrakce.OdhlasenoOK(Sender:TObject; Data:Pointer);
begin
 // loko odhlaseno a POM nastaveno -> volame OK callback
 if (Assigned(TPrevzitCallback(data^).callback_ok.callback)) then
   TPrevzitCallback(data^).callback_ok.callback(Self, TPrevzitCallback(data^).callback_ok.data);
 FreeMem(data);
end;

procedure TTrakce.OdhlasenoErr(Sender:TObject; Data:Pointer);
begin
 // POM nastaveno, ale loko neodhlaseno -> volame error callback
 if (Assigned(TPrevzitCallback(data^).callback_err.callback)) then
   TPrevzitCallback(data^).callback_err.callback(Self, TPrevzitCallback(data^).callback_err.data);
 FreeMem(data);
end;

procedure TTrakce.OdhlasenoPOMOK(Sender:TObject; Data:Pointer);
begin
 // release POM uspesne naprogramovano -> odhlasit loko
 if (Assigned(HVDb.HVozidla[TPrevzitCallback(data^).addr])) then
  begin
   HVDb.HVozidla[TPrevzitCallback(data^).addr].changed := true;
   RegCollector.ConnectChange(TPrevzitCallback(data^).addr);
  end;

 Self.callback_ok  := TTrakce.GenerateCallback(Self.OdhlasenoOK, data);
 Self.callback_err := TTrakce.GenerateCallback(Self.OdhlasenoErr, data);
 Self.Trakce.LokFromMyControl(TPrevzitCallback(data^).addr);
end;

procedure TTrakce.OdhlasenoPOMErr(Sender:TObject; Data:Pointer);
begin
 // release POM error -> zavolat error callback
 if (Assigned(TPrevzitCallback(data^).callback_err.callback)) then
   TPrevzitCallback(data^).callback_err.callback(Self, TPrevzitCallback(data^).callback_err.data);
 FreeMem(data);
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

procedure TTrakce.OnTrackStatusChange(Sender: TObject);
begin
 if (Self.TrackStatus() = tsOn) then
   Self.DCCGoTime := Now;
 F_Main.OnCentralaDCCChange(Self, Self.TrackStatus() = tsOn); // TODO: do more nicely
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
procedure TTrakce.POMWriteCVs(Sender:TObject; HV:THV; list:TList<THVPomCV>; new:TPomStatus);
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
// callbacky pri nastavovani funkci hnacicho vozidel (F0-Fn)
// data jsou TFuncCallback

{procedure TTrakce.FuncOK(Sender:TObject; Data:Pointer);
var i:Integer;
    func:Byte;
    sada:Integer;
    s:string;
begin }
{ if (TFuncCallback(data^).sady.Count < 1) then
  begin
   // vsechny sady nastaveny
   TFuncCallback(data^).sady.Free();
   if (Assigned(TFuncCallback(data^).callback_ok.callback)) then
      TFuncCallback(data^).callback_ok.callback(Self, TFuncCallback(data^).callback_ok.data);
   FreeMem(data);
  end else begin
   // nastavit dalsi sadu

   // vypocet func bytu pro jednotlive sady separatne:
   sada := TFuncCallback(data^).sady[0].index;
   case (sada) of
    0:begin
       for i := 0 to 4 do s := s + IntToStr(PrevodySoustav.BoolToInt(TFuncCallback(data^).sady[0].func[i]));
       Self.TrkLog(self, tllCommand, 'PUT: LOK FUNC 0-4: '+HVDb.HVozidla[TFuncCallback(data^).addr].Data.Nazev+' ('+IntToStr(TFuncCallback(data^).addr)+') : '+s);

       func := 0;
       for i := 0 to 3 do if (TFuncCallback(data^).sady[0].func[i+1]) then func := func or (1 shl (i+1));
       if (TFuncCallback(data^).sady[0].func[0]) then func := func or 1;
    end;

    1: begin
       for i := 0 to 3 do s := s + IntToStr(PrevodySoustav.BoolToInt(TFuncCallback(data^).sady[0].func[i]));
       Self.TrkLog(self, tllCommand, 'PUT: LOK FUNC 5-8: '+HVDb.HVozidla[TFuncCallback(data^).addr].Data.Nazev+' ('+IntToStr(TFuncCallback(data^).addr)+') : '+s);

       func := 0;
       for i := 0 to 3 do if (TFuncCallback(data^).sady[0].func[i]) then func := func or (1 shl i);
    end;

    2: begin
       for i := 0 to 3 do s := s + IntToStr(PrevodySoustav.BoolToInt(TFuncCallback(data^).sady[0].func[i]));
       Self.TrkLog(self, tllCommand, 'PUT: LOK FUNC 9-12: '+HVDb.HVozidla[TFuncCallback(data^).addr].Data.Nazev+' ('+IntToStr(TFuncCallback(data^).addr)+') : '+s);

       func := 0;
       for i := 0 to 3 do if (TFuncCallback(data^).sady[0].func[i]) then func := func or (1 shl i);
    end;

    3: begin
       for i := 0 to 7 do s := s + IntToStr(PrevodySoustav.BoolToInt(TFuncCallback(data^).sady[0].func[i]));
       Self.TrkLog(self, tllCommand, 'PUT: LOK FUNC 13-20: '+HVDb.HVozidla[TFuncCallback(data^).addr].Data.Nazev+' ('+IntToStr(TFuncCallback(data^).addr)+') : '+s);

       func := 0;
       for i := 0 to 7 do if (TFuncCallback(data^).sady[0].func[i]) then func := func or (1 shl i);
    end;

    4: begin
       for i := 0 to 7 do s := s + IntToStr(PrevodySoustav.BoolToInt(TFuncCallback(data^).sady[0].func[i]));
       Self.TrkLog(self, tllCommand, 'PUT: LOK FUNC 21-28: '+HVDb.HVozidla[TFuncCallback(data^).addr].Data.Nazev+' ('+IntToStr(TFuncCallback(data^).addr)+') : '+s);

       func := 0;
       for i := 0 to 7 do if (TFuncCallback(data^).sady[0].func[i]) then func := func or (1 shl i);
    end;

   else
    // neznama sada -> konec
    TFuncCallback(data^).sady.Free();
    if (Assigned(TFuncCallback(data^).callback_ok.callback)) then
       TFuncCallback(data^).callback_ok.callback(Self, TFuncCallback(data^).callback_ok.data);
    FreeMem(data);
    Exit();
   end;

   TFuncCallback(data^).sady.Delete(0);     // prvni sada zpracovana
   Self.callback_ok  := TTrakce.GenerateCallback(Self.FuncOK, data);
   Self.callback_err := TTrakce.GenerateCallback(Self.FuncErr, data);

   try
     Self.Trakce.LokSetFunc(TFuncCallback(data^).addr, sada, func);
   except
     Self.FuncErr(Self, data);
   end;
  end;          }
//end;

{procedure TTrakce.FuncErr(Sender:TObject; Data:Pointer);
begin
 // chyba pri nastavovani funkci -> zavolame error callback
 TFuncCallback(data^).sady.Free();
 if (Assigned(TFuncCallback(data^).callback_err.callback)) then
    TFuncCallback(data^).callback_err.callback(Self, TFuncCallback(data^).callback_err.data);
 FreeMem(data);
end;}

////////////////////////////////////////////////////////////////////////////////
// callbacky pri nastavovani funkci hnaciho vozidla pri prebirani:

{procedure TTrakce.PrevzatoFuncOK(Sender:TObject; Data:Pointer);
begin
 // HV prevzato a funkce nastaveny -> nastavit POM
 Self.callback_ok  := TTrakce.GenerateCallback(Self.PrevzatoPOMOK, data);
 Self.callback_err := TTrakce.GenerateCallback(Self.PrevzatoErr, data);

 if ((RegCollector.IsLoko(HVDb.HVozidla[TPrevzitCallback(data^).addr])) or (HVDb.HVozidla[TPrevzitCallback(data^).addr].ruc)) then
  begin
   // rucni rizeni
   HVDb.HVozidla[TPrevzitCallback(data^).addr].Stav.ruc := true;
   Self.POMWriteCVs(Self, HVDb.HVozidla[TPrevzitCallback(data^).addr], HVDb.HVozidla[TPrevzitCallback(data^).addr].Data.POMrelease, TPomStatus.released);
  end else begin
   // rizeni automatem
   HVDb.HVozidla[TPrevzitCallback(data^).addr].Stav.ruc := false;
   Self.POMWriteCVs(Self, HVDb.HVozidla[TPrevzitCallback(data^).addr], HVDb.HVozidla[TPrevzitCallback(data^).addr].Data.POMtake, TPomStatus.pc);
  end;
end;

procedure TTrakce.PrevzatoFuncErr(Sender:TObject; Data:Pointer);
begin
 // loko prevzato, ale funkce se nepodarilo nastavit -> error callback
 Self.Log(tllWarning, 'WARN: LOKO '+ IntToStr(TPrevzitCallback(data^).addr) + ' se nepodařilo nastavit funkce');
 F_Main.LogStatus('WARN: loko '+ IntToStr(TPrevzitCallback(data^).addr) + ' se nepodařilo nastavit funkce');
 Self.PrevzatoFuncOK(Self, data);
end;}

////////////////////////////////////////////////////////////////////////////////
// callbacky hromadneho nastavovani funkci dle vyznamu:
//    napr. zapni "zvuk" vsech hnacich vozidel

{procedure TTrakce.LoksSetFuncOK(Sender:TObject; Data:Pointer);
var addr, i:Integer;
begin
 for addr := TFuncsCallback(data^).addr+1 to _MAX_ADDR-1 do
  begin
   if ((HVDb.HVozidla[addr] = nil) or (not HVDb.HVozidla[addr].Slot.prevzato)) then continue;

   for i := 0 to _HV_FUNC_MAX do
    if ((HVDb.HVozidla[addr].Data.funcVyznam[i] = TFuncsCallback(data^).vyznam) and (HVDb.HVozidla[addr].Slot.funkce[i] <> TFuncsCallback(data^).state)) then
      begin
       HVDb.HVozidla[addr].Stav.funkce[i] := TFuncsCallback(data^).state;
       TFuncsCallback(data^).addr := addr;

       Self.callback_ok  := TTrakce.GenerateCallback(Self.LoksSetFuncOK, data);
       Self.callback_err := TTrakce.GenerateCallback(Self.LoksSetFuncErr, data);

       try
         Self.LokSetFunc(Self, HVDb.HVozidla[addr], HVDb.HVozidla[addr].Stav.funkce);
       except
         Self.LoksSetFuncErr(Self, data);
       end;

       Exit();
      end;//if vyznam = vyznam
  end;//for i

 if (Assigned(TFuncsCallback(data^).callback_ok.callback)) then TFuncsCallback(data^).callback_ok.callback(Self, TFuncsCallback(data^).callback_ok.data);
 FreeMem(data);
end;

procedure TTrakce.LoksSetFuncErr(Sender:TObject; Data:Pointer);
begin
 // sem lze pridat oznameni chyby
 Self.LoksSetFuncOK(Sender, Data);
end;}

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
