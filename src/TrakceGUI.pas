unit TrakceGUI;

{
 Trida TTrkGUI vytvari pomyslne 'graficke rozhrani' tridy TTrakce pro zbytek
 programu. Ve skutecnosti se nejedna o GUI, ale o funkce tridy TTrakce upravene
 tak, aby sly v programu pouzit co nejsnaze.

 Zapouzdreni abstraktni tridy TTrakce, resp. konkretni tridy TXPressNet (ktera
 ji dedi) spociva v implemenatci callbackovych funkci v tride TTrkGUI, ktera
 napriklad chyby promita primo do realneho GUI aplikace.
}

interface

uses
  SysUtils, Classes, StrUtils, CPort, Trakce, ComCtrls, Graphics, Forms, Windows,
  THnaciVozidlo, Generics.Collections;

const
  // maximalni rychlost pro rychlostni tabulku
  _MAX_SPEED = 28;

type
  TLogEvent = procedure(Sender:TObject; lvl:Integer; msg:string; var handled:boolean) of object;
  TGetSpInfoEvent = procedure(Sender: TObject; Slot:TSlot; var handled:boolean) of object;
  TGetFInfoEvent = procedure(Sender: TObject; Addr:Integer; func:TFunkce; var handled:boolean) of object;

  ENotOpenned = class(Exception);

  // pri programovani POMu se do callbacku predavaji tato data
  TPOMCallback = record
    addr:Word;                                                                  // adresa programovane lokomotivy
    list:TList<THVPomCV>;                                                       // kompletni nemenny seznam CV k programovani
    index:Integer;                                                              // index prave progamovaneho CV
    callback_ok, callback_err:TCommandCallback;                                 // callbacky pri kompletnim ukonceni programovani, resp. vyskytu chyby v libovolnem kroku
    new:TPomStatus;                                                             // status, jaky ma nabyt POM hnaciho vozidla po ukonceni programovaani (zpravidla PC, nebo RELEASED)
                                                                                // pokud dojde pri programovani POMu k chybe, je do HV.Slot.pom ulozeno TPomStatus.error
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
  TPrevzitCallback = record
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

  // ------- trida TTrkGUI --------
  TTrkGUI = class
   private const
     _DEF_LOGLEVEL = 2;                                                         // default loglevel

     _DEF_BAUDRATE    = br9600;                                                 // default serial baudrate
     _DEF_STOPBITS    = sbOneStopBit;                                           // default serial stopbits
     _DEF_DATABITS    = dbEight;                                                // default serial databits
     _DEF_FLOWCONTROL = fcHardware;                                             // default serial flow control

   private
     Trakce:TTrakce;                                                            // reference na tridu TTrakce
                                                                                //   Ttrakce je abstarktni, zde se ve skutecnosti nachazi konkretni trida protokolu (zatim jen TXpressNET, v pripade potreby napriklad i TLocoNET)
     LogObj:TListView;                                                          // Logovaci objekt - obvykle obsahuje refenreci na logovaci tabulku ve F_Main (je inicializovano v kontruktoru)

     fTrkSystem:Ttrk_system;                                                    // aktualni trakcni system; koresponduje se vytvorenou instanci v .Trakce

     SpeedTable:array [0.._MAX_SPEED] of Integer;                               // rychlostni tabulka
                                                                                //   index = jizdni stupen, .SpeedTable[index] = rychlost v km/h
                                                                                //   napr. SpeedTable[15] = 40 <-> rychlostni stupen 15: 40 km/h

     turnoff_callback:TNotifyEvent;                                             // callback volany po prikazu TurnOff
                                                                                //   Prikaz TurnOff zapina F0 a vypina vsechny vyssi funkce
                                                                                //   pouziva se pri vypinani systemu (pro vypnuti otravnych zvuku zvukovych dekoderu)

     //events definition
     FOnLog: TLogEvent;                                                         // log event

     floglevel: Byte;                                                           // log level
     flogfile : boolean;                                                        // logovat do souboru ?
     flogtable: boolean;                                                        // logovat do tabulky (.LogObj) ?
     finitok  : boolean;                                                        // je true, pokud po otevreni seriaku centrala zakomujikovala (tj. odpovedela na prikaz)
                                                                                // pri .Open se nastavuje na false
     {
      Info: Samotna komponenta serioveho portu je ulozena v .Trakce.ComPort.CPort.
      Tato komponenta je pri uzavrenem seriaku znicena.
      Pred otevrenim je volna konstruktor, po uzavreni destruktor!
     }

     CPortData : record                                                         // data pro nastaveni serioveho portu
      br:TBaudRate;                                                             // baudrate
      com:string;                                                               // com port ve formatu "COM1", "COM8" ...
      sb:TStopBits;                                                             // stop bits
      db:TDataBits;                                                             // data bits
      FlowControl:TFlowControl;
     end;

     function GetOpenned():boolean;                                             // je seriak otevreny ?

     procedure TrkLog(Sender:TObject; lvl:Integer; msg:string);                 // logovaci event z .Trakce
     procedure WriteLog(lvl:Integer; msg:string);                               // zaloguje data (at uz do souboru, ci do tabulky)

     // eventy serioveho portu:
     procedure OnComError(Sender: TObject; Errors: TComErrors);
     procedure OnComException(Sender: TObject;
        TComException: TComExceptions; ComportMessage: string; WinError: Int64;
        WinMessage: string);
     procedure OnComWriteError(Sender:TObject);                                 // tento event je volan z .Trakce pri vyvolani vyjimky pri zapisu do ComPortu
                                                                                // to nastava napriklad, pokud ComPort najednou zmizi z pocitace (nekdo odpoji USB)
     function GettSpeed(kmph:Integer):Integer;                                  // vrati rchlostni stupen pri zadane rychlosti v km/h

     // eventy z komponenty seriaku:
     procedure BeforeOpen(Sender:TObject);
     procedure AfterOpen(Sender:TObject);
     procedure BeforeClose(Sender:TObject);
     procedure AfterClose(Sender:TObject);

     procedure NouzReleaseLoko();

     procedure ConnectChange(Sender: TObject; addr:Integer; code:TConnect_code;
        data:Pointer);                                                          // event volany z .Trace pri zmene majitele lokomotivy (loko prevzato, ukradeno, nedostupne, ...)
     procedure LokComErr(Sender:TObject; addr:Integer);                         // event oznamujici chybu komunikace s danou lokomotivou (je volan paralelne s error callback eventem, ale jen pro urcite prikazy - pro prikazy tykajici se rizeni konkretni lokomotivy).
     procedure LokComOK(Sender:TObject; addr:Integer);                          // event potvrzujici komunikaci s danym HV (je volan pokazde, pokud centrala odpovi na prikaz o nasatveni dat HV)
                                                                                // je protipol predchoziho eventu

     procedure SetLogLevel(level:Byte);                                         // nastavit loglevel

     // eventy z .Trakce pri zapinani systemu (tj. odpoved na priklad GET-STATUS)
     procedure InitStatErr(Sender:TObject; Data:Pointer);
     procedure InitStatOK(Sender:TObject; Data:Pointer);

     // event z .Tracke pri ozivovani systemu po neuspesnem prikazu GET-STATUS
     // oziveni - prikaz DCC STOP; tyto eventy tedy odpovidaji inicializacnimu prikazu STOP
     procedure InitStopErr(Sender:TObject; Data:Pointer);
     procedure InitStopOK(Sender:TObject; Data:Pointer);

     // Obecne nastaveni callback komunikacnich eventu
     // Tyto funkce nastavi callback eventy do .Trakce
     procedure SetCallbackErr(callback_err:TCommandCallback);
     procedure SetCallbackOK(callback_ok:TCommandCallback);     

     // eventy z komunikace s centralou pri prebirani a odhlasovani HV (tj. prebirani a odhlasovani VSECH LOKO)
     procedure PrebiraniUpdateOK(Sender:TObject; Data:Pointer);                 // loko uspesne prevzato
     procedure PrebiraniUpdateErr(Sender:TObject; Data:Pointer);                // prevzeti se nazdarilo (napr. centrala neodpovedela na prikaz o prevzeti, na POM ...)
     procedure OdhlasovaniUpdateOK(Sender:TObject; Data:Pointer);               // loko uspesne uvolneno
     procedure OdhlasovaniUpdateErr(Sender:TObject; Data:Pointer);              // uvolneni loko se nezdarilo (napr. centrala nedopovedela na POM, ...)

     // eventy spojene s jednotlivymi fazemi prebirani HV
     procedure PrevzatoErr(Sender:TObject; Data:Pointer);                       // centala nedopovedela na prikaz o prevzeti
                                                                                // (to, ze centrala odpovedela, ale HV neni dostupne, je signalizovano eventem .ConnectChange)
     procedure PrevzatoFunc1328OK(Sender:TObject; Data:Pointer);                // funkce 13-28 byly uspesne nacteny
     procedure PrevzatoPOMOK(Sender:TObject; Data:Pointer);                     // POM vsech CV uspesne dokoncen
     procedure PrevzatoFuncOK(Sender:TObject; Data:Pointer);                    // nastavovani vsech funkci uspesne dokonceno
     procedure PrevzatoFuncErr(Sender:TObject; Data:Pointer);                   // centrala neodpovedela na prikaz o nastaveni funkci

     // eventy spojene s jedntlivymi fazemi odhlasovani loko:
     procedure OdhlasenoOK(Sender:TObject; Data:Pointer);                       // loko odhlaseno centrala odpovedela na prikaz o uvolneni
     procedure OdhlasenoErr(Sender:TObject; Data:Pointer);                      // centrala neodpovedela na prikaz o uvolneni
     procedure OdhlasenoPOMOK(Sender:TObject; Data:Pointer);                    // kompletni Release POM hotov
     procedure OdhlasenoPOMErr(Sender:TObject; Data:Pointer);                   // centrala neodpovedela na Release POM

     // callbacky spojene s nastavovanim funkci:
     // Funkce nastavujeme po jednotlivych sadach.
     // K nastaveni dalsi sady dojde az po uspesnem nastaveni sady predchozi - tj. prichodu prikazu OK z centraly, resp. zavolani OK callbacku
     procedure FuncOK(Sender:TObject; Data:Pointer);
     procedure FuncErr(Sender:TObject; Data:Pointer);

     procedure AllPrevzato();                                                   // je volana, pokud jsou vsechny loko prevzaty (primarni vyuziti = interakce s GUI)
     procedure AllOdhlaseno();                                                  // je volana, pokud jsou vsechny loko odhlaseny (primarni vyuziti = interakce s GUI)

     procedure OnTrackStatusChange(Sender: TObject);                            // event volany z .Trakce pri zmene TrackStatus (napr. CENTRAL-STOP, SERVICE, ...)
                                                                                // novy status je k dispozici v .Trakce.TrackStatus

     function GetTrkStatus():Ttrk_status;                                       // zjistit TrackStatus s .Trakce

     procedure TurnOffFunctions_cmdOK(Sender:TObject; Data:Pointer);            // OK callback pro prikaz TurnOff

     procedure GotCSVersion(Sender:TObject; version:TCSVersion);                // Callback z .Trakce volany pri prichodu prikazu informujicim o verzi v FW v centrale
     procedure GotLIVersion(Sender:TObject; version:TLIVersion);                // Callback z .Trakce volany pri prichodu prikazu informujicim o verzi v LI

     // eventy spojene se zapisem jednotlivych POM:
     procedure POMCvWroteOK(Sender:TObject; Data:Pointer);
     procedure POMCvWroteErr(Sender:TObject; Data:Pointer);

     // chyba pri nouzovem uvolnovani HV
     //   Nouzzove jsou uvolnena takove HV, ktera jsou pri BeforeClose jeste prevzata.
     procedure NouzReleaseCallbackErr(Sender:TObject; Data:Pointer);

     // callback pri zjistovani verze centraly
     procedure GotCSVersionOK(Sender:TObject; Data:Pointer);
     procedure GotCSVersionErr(Sender:TObject; Data:Pointer);

     // callback pri zjistovani verze LI
     procedure GotLIVersionOK(Sender:TObject; Data:Pointer);
     procedure GotLIVersionErr(Sender:TObject; Data:Pointer);

     procedure LoksSetFuncOK(Sender:TObject; Data:Pointer);
     procedure LoksSetFuncErr(Sender:TObject; Data:Pointer);

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

     constructor Create(TrkSystem:Ttrk_system; LogObj:TListView; loglevel:Integer = _DEF_LOGLEVEL; logfile:boolean = true; logtable:boolean = true);
     destructor Destroy(); override;

     // otevrit a zavrit spojeni s centralou:
     function Open():Byte;
     function Close(force:boolean = false):Byte;

     // zakladni prikazy do centraly:
     function CentralStart():Byte;                                              // TRACK ON
     function CentralStop():Byte;                                               // TRACK OFF
     function LokSetSpeed(Sender:TObject; HV:THV; speed:Integer; dir:Integer = -1):Byte;
                                                                                // nastaveni rychlosti daneho HV, rychlost v km/h
     function LokSetDirectSpeed(Sender:TObject; HV:THV; speed:Integer; dir:Integer = -1):Byte;
                                                                                // nastaveni rychlosti daneho HV, rychlost se zadava primo ve stupnich
     function LokSetFunc(Sender:TObject; HV:THV; funkce:TFunkce; force:boolean = false):Byte;
                                                                                // nastaveni funkce HV; force nastavi vsechny funkce v parametru bez ohledu na rozdilnost od aktualniho stavu
     procedure LoksSetFunc(vyznam:string; state:boolean);                       // nastavi funkci s danym vyznamem u vsech prevzatych hnacich vozidel na hodnotu state
     function LokGetSpSteps(HV:THV):Byte;                                       // vysle pozadavek na zjisteni informaci o lokomotive
     function EmergencyStop():Byte;                                             // nouzove zastaveni vsech lokomotiv, ktere centrala zna (centrala, nikoliv pocitac!)
     function EmergencyStopLoko(Sender:TObject; HV:THV):Byte;                   // nouzove zastaveni konkretniho HV
     function EmergencyStopAddr(addr:Integer):Byte;                             // nouzove zastaveni konkretni adresy lokmotivy

     procedure GetCSVersion();                                                  // zjistit verzi FW v centrale
     procedure GetLIVersion();                                                  // zjistit verzi SW a HW LI

     procedure InitSystems();                                                   // odesle centrale povel TRACK-STATUS a doufa v odpoved
                                                                                // je volano pri pripojeni k centrale jako overeni funkcni komunikace

     // nacitani a ukladani rychlostni tabulky z a do souboru
     function LoadSpeedTable(filename:string;var LVRych:TListView):Byte;
     procedure SaveSpeedTable(filename:string);

     function GetStepSpeed(step:byte):Integer;                                  // vraci rychlosti prislusneho jizdniho stupne
     function SetSpetSpeed(step:byte;sp:Integer):Byte;                          // nastavi rychlost prislusneho jizdniho stupne

     procedure PrevzitLoko(HV:THV);                                             // prevzit dane HV (tj. z centraly, nastavit funkce, POM)
     procedure OdhlasitLoko(HV:THV);                                            // uvolnit loko (tj. RELEASE POM, odhlasit)

     function POMWriteCV(Sender:TObject; HV:THV; cv:Word; data:byte):Integer;   // zapsat 1 CV POMem, v praxi nevyuzivano
     procedure POMWriteCVs(Sender:TObject; HV:THV; list:TList<THVPomCV>; new:TPomStatus);
                                                                                // zapsat seznam CV POMem

     procedure PrevzitAll();                                                    // prevzit vsechna HV na soupravach
     procedure OdhlasitAll();                                                   // odhlasit vsechna prevzata HV
     procedure FastResetLoko();                                                 // resetuje lokomotivy do odhlaseneho stavu, rychly reset

     procedure TurnOffFunctions(callback:TNotifyEvent);                         // vypnout funkce vyssi, nez F0; F0 zapnout

     // aktualni data serioveho portu
     property BaudRate:TBaudRate read CPortData.br write CPortData.br;
     property COM:string read CPortData.com write CPortData.com;
     property StopBits:TStopBits read CPortData.sb write CPortData.sb;
     property DataBits:TDataBits read CPortData.db write CPortData.db;
     property FlowControl:TFlowCOntrol read CPortData.FlowControl write CPortData.FlowControl;

     property openned:boolean read GetOpenned;                                  // otevreny seriovy port
     property TrkSystem:Ttrk_system read fTrkSystem;                            // aktualni TrkSystem (XpressNET, LocoNET, ...)
     property isInitOk:boolean read finitok;                                    // komunikuje cetrala?, resp. odpovedel na prikaz STATUS
     property status:Ttrk_status read GetTrkStatus;                             // aktualni STATUS centraly

     { !!! DULEZITE:
       pri volani libovolne funkce (i zvnejsku) je mozne do techto properties
       nastavit Callback eventy, ktere budou zavolany pri vykonani prikazu.
       V praxi:
        .callback_err := TTrakce.GenerateCallback(Self.chyba)
        .callback_ok  := TTrakce.GenerateCallback(Self.uspech)
        .SetRych(...)
       Vzdy je zavolan prave jeden z callbacku !
        OK callback je volan kdyz centrala odpovi na prikaz "OK",
        Error callback je volany, kdyz centrala odpovi chybou,
          nebo na prikaz opakovane neodpovi.
     }
     property callback_err:TCommandCallback write SetCallbackErr;
     property callback_ok:TCommandCallback write SetCallbackOK;

     // vnejsi eventy:
     property OnLog: TLogEvent read FOnLog write FOnLog;
     property loglevel: Byte read floglevel write SetLogLevel;
     property logfile:boolean read flogfile write flogfile;
     property logtable:boolean read flogtable write flogtable;

   protected
  end;//TTrkGUI

////////////////////////////////////////////////////////////////////////////////

implementation

uses fMain, fSettings, RPConst, XpressNET, TechnologieMTB, fRegulator,
    GetSystems, THVDatabase, fAdminForm, DataHV,
    Prevody, TBloky, RegulatorTCP, TCPServerOR, fFuncsSet;

////////////////////////////////////////////////////////////////////////////////

constructor TTrkGUI.Create(TrkSystem:Ttrk_system; LogObj:TListView; loglevel:Integer = _DEF_LOGLEVEL; logfile:boolean = true; logtable:boolean = true);
begin
 inherited Create;

 case TrkSystem of
  TRS_LocoNET   : ;
  TRS_XpressNET : Self.Trakce := TXpressNET.Create();
 end;//case

 Self.floglevel := loglevel;
 Self.flogfile  := logfile;
 Self.flogtable := logtable;

 Self.fTrkSystem := TrkSystem;

 Self.Trakce.OnLog                := Self.TrkLog;
 Self.Trakce.OnConnectChange      := Self.ConnectChange;
 Self.Trakce.OnLokComError        := Self.LokComErr;
 Self.Trakce.OnLokComOK           := Self.LokComOK;
 Self.Trakce.OnTrackStatusChange  := Self.OnTrackStatusChange;
 Self.Trakce.OnComError           := Self.OnComWriteError;

 Self.LogObj := LogObj;

 Self.CPortData.br          := _DEF_BAUDRATE;
 Self.CPortData.sb          := _DEF_STOPBITS;
 Self.CPortData.db          := _DEF_DATABITS;
 Self.CPortData.FlowControl := _DEF_FLOWCONTROL;

 Self.turnoff_callback := nil;
 Self.DCCGoTime := Now;

 Self.WriteLog(2, 'BEGIN loglevel='+IntToStr(Self.loglevel));
end;//ctor

destructor TTrkGUI.Destroy();
begin
 Self.WriteLog(2, 'END');
 FreeAndNil(Self.Trakce);
 inherited Destroy;
end;//dotr

////////////////////////////////////////////////////////////////////////////////

function TTrkGUI.GetOpenned():boolean;
begin
 if (not Assigned(Self.Trakce.ComPort.CPort)) then Exit(false);
 Result := Self.Trakce.ComPort.CPort.Connected;
end;//function

function TTrkGUI.Open():Byte;
begin
 Self.finitok := false;

 if ((SystemData.Status = starting) and (Self.openned)) then
  begin
   Self.InitSystems();
   Exit(0);
  end;

 Self.TrkLog(self,2,'OPENING port='+Self.CPortData.com+' br='+BaudRateToStr(Self.CPortData.br)+' sb='+StopBitsToStr(Self.CPortData.sb)+' db='+DataBitsToStr(Self.CPortData.db)+' fc='+FlowControlToStr(Self.CPortData.FlowControl));

 if ((Assigned(Self.Trakce.ComPort.CPort)) and (Self.Trakce.ComPort.CPort.Connected)) then
  begin
   Self.TrkLog(self, 1, 'OPEN: already connected');
   Exit(0);
  end;

 if (not Assigned(Self.Trakce.ComPort.CPort)) then
   Self.Trakce.ComPort.CPort := TComPort.Create(nil);

 Self.Trakce.ComPort.CPort.OnError       := Self.OnComError;
 Self.Trakce.ComPort.CPort.OnException   := Self.OnComException;
 Self.Trakce.ComPort.CPort.OnBeforeOpen  := Self.BeforeOpen;
 Self.Trakce.ComPort.CPort.OnAfterOpen   := Self.AfterOpen;
 Self.Trakce.ComPort.CPort.OnBeforeClose := Self.BeforeClose;
 Self.Trakce.ComPort.CPort.OnAfterClose  := Self.AfterClose;
 Self.Trakce.ComPort.CPort.Port        := Self.CPortData.com;
 Self.Trakce.ComPort.CPort.BaudRate    := Self.CPortData.br;
 Self.Trakce.ComPort.CPort.DataBits    := Self.CPortData.db;
 Self.Trakce.ComPort.CPort.StopBits    := Self.CPortData.sb;
 Self.Trakce.ComPort.CPort.FlowControl.FlowControl := Self.CPortData.FlowControl;

 try
  Self.Trakce.ComPort.CPort.Open;
 except
  on E : Exception do
   begin
    Self.Trakce.ComPort.CPort.Close;
    Self.TrkLog(self, 1, 'OPEN ERR: com port object error : '+E.Message);
    Self.AfterClose(self);

    if (SystemData.Status = TSystemStatus.starting) then
     begin
      SystemData.Status := TSystemStatus.null;
      F_Main.A_System_Start.Enabled := true;
      F_Main.A_System_Stop.Enabled := true;
     end;

    Exit(3);
   end;
 end;

 Result := 0;
end;//function

function TTrkGUI.Close(force:boolean = false):Byte;
var i:Integer;
begin
 if ((SystemData.Status = stopping) and (not Self.openned)) then
  begin
   F_Main.A_MTB_StopExecute(nil);
   Exit(0);
  end;

 Self.TrkLog(self, 2, 'CLOSING...');

 if (not Self.openned) then
  begin
   Self.TrkLog(self, 1, 'CLOSE: already disconnected');
   Exit(0);
  end;

 for i := 0 to _MAX_ADDR-1 do
   if (HVDb.HVozidla[i] <> nil) then
     HVDb.HVozidla[i].Slot.stolen := false;

 if (not force) then Self.NouzReleaseLoko();
 
 try
  Self.Trakce.ComPort.CPort.Close();
 except
  on E : Exception do
    Self.TrkLog(self, 1, 'CLOSE ERR: com port object error : '+E.Message);
 end;

 try
   FreeAndNil(Self.Trakce.ComPort.CPort);
 except
   Self.AfterClose(self);
 end;


 Result := 0;
end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TTrkGUI.TrkLog(Sender:TObject; lvl:Integer; msg:string);
var handled:boolean;
begin
 handled := false;
 if (Assigned(Self.FOnLog)) then Self.FOnLog(self, lvl, msg, handled);

 if (handled) then Exit;
 Self.WriteLog(lvl, msg);
end;//procedure

procedure TTrkGUI.WriteLog(lvl:Integer; msg:string);
var LV_Log:TListItem;
    f:TextFile;
    xDate, xTime:string;
 begin
  if (lvl > Self.floglevel) then Exit;

  DateTimeToString(xDate, 'yy_mm_dd', Now);
  DateTimeToString(xTime, 'hh:mm:ss,zzz', Now);

  if (Self.LogObj.Items.Count > 500) then
    Self.LogObj.Clear();

  if (Self.logtable) then
   begin
    try
      LV_Log := Self.LogObj.Items.Insert(0);
      LV_Log.Caption := xTime;
      LV_Log.SubItems.Add(IntToStr(lvl));
      LV_Log.SubItems.Add(msg);
    except

    end;
   end;//if Self.logtable

  if (Self.logfile) then
   begin
    try
      AssignFile(f, 'log\lnet\'+xDate+'.log');
      if FileExists('log\lnet\'+xDate+'.log') then
        Append(f)
      else
        Rewrite(f);

      Writeln(f, xTime+' '+IntToStr(lvl)+': '+msg);

      CloseFile(f);
    except

    end;
   end;
end;//prrocedure

////////////////////////////////////////////////////////////////////////////////

function TTrkGUI.CentralStart():Byte;
begin
 if (not Assigned(Self.Trakce)) then Exit(1);
 if (not Self.openned) then Exit(2);

 Self.TrkLog(self,2,'PUT: CENTRAL START');
 Self.Trakce.TrackStatus := TS_ON;

 Result := 0;
end;//function

function TTrkGUI.CentralStop():Byte;
begin
 if (not Assigned(Self.Trakce)) then Exit(1);
 if (not Self.openned) then Exit(2);

 Self.TrkLog(self,2,'PUT: CENTRAL STOP');
 Self.Trakce.TrackStatus := TS_OFF;

 Result := 0;
end;//function

////////////////////////////////////////////////////////////////////////////////

function TTrkGUI.LokSetSpeed(Sender:TObject; HV:THV; speed:Integer; dir:Integer = -1):Byte;    //rychlost se zadava v km/h
begin
 if (not Self.openned) then Exit(1);
 if (HV = nil) then Exit(2);
 if ((dir > 1) or (dir < -1)) then Exit(4);

 if ((HV.Slot.speed = Self.GettSpeed(speed)) and (HV.Slot.Smer = dir)) then
  begin
   Self.TrkLog(self, 5, 'PUT: IGNORE LOK SPEED: '+HV.data.Nazev+' ('+IntToStr(HV.Adresa)+'); sp='+IntToStr(speed)+' skmph; dir='+IntToStr(dir));
   Result := 4;
   Exit;
  end;

 if (dir = -1) then dir := HV.Slot.Smer;

 HV.Slot.speed := Self.GettSpeed(speed);
 HV.Slot.Smer  := dir;

 TCPRegulator.LokUpdateSpeed(HV, Sender);
 RegCollector.UpdateElements(Sender, HV.Slot.adresa);
 HV.changed := true;

 Self.TrkLog(self,2,'PUT: LOK SPEED: '+HV.data.Nazev+' ('+IntToStr(HV.Adresa)+'); sp='+IntToSTr(speed)+' kmph = '+IntToSTr(Self.GettSpeed(speed))+' steps; dir='+IntToStr(dir));
 Self.Trakce.LokSetSpeed(HV.Adresa,Self.GettSpeed(speed),dir);
 Result := 0;
end;//function

function TTrkGUI.LokSetDirectSpeed(Sender:TObject; HV:THV; speed:Integer; dir:Integer = -1):Byte;    //rychlost se zadava v steps
begin
 if (not Self.openned) then Exit(1);
 if (HV = nil) then Exit(2);
 if ((dir > 1) or (dir < -1)) then Exit(4);

 if ((HV.Slot.speed = speed) and (HV.Slot.Smer = dir)) then
  begin
   Self.TrkLog(self, 5, 'PUT: IGNORE LOK SPEED: '+HV.data.Nazev+' ('+IntToStr(HV.Adresa)+'); sp='+IntToStr(speed)+' steps; dir='+IntToStr(dir));
   Exit(4);
  end;

 if (dir = -1) then dir := HV.Slot.Smer;

 HV.Slot.speed := speed;
 HV.Slot.Smer  := dir;

 TCPRegulator.LokUpdateSpeed(HV, Sender);
 RegCollector.UpdateElements(Sender, HV.Slot.adresa);
 HV.changed := true;

 Self.TrkLog(self,2,'PUT: LOK SPEED: '+HV.data.Nazev+' ('+IntToStr(HV.Adresa)+'); sp='+IntToStr(speed)+' steps; dir='+IntToStr(dir));

 Self.Trakce.LokSetSpeed(HV.Adresa,speed,dir);

 Result := 0;
end;//function

// nastaveni funkci lokomotiv:
//  vypocteme sady a samotne funkce nechame nastavit TTrkGUI.FuncOK(...)
//  jednotlive sady se pak nastavuji postupne (po prijeti OK callbacku)
function TTrkGUI.LokSetFunc(Sender:TObject; HV:THV; funkce:TFunkce; force:boolean = false):Byte;
const
    _SADY_CNT = 5;

var i:Cardinal;
    sady_change:array [0.._SADY_CNT-1] of boolean;
    fc:Pointer;
    sada:TFuncCBSada;

begin
 if (not Self.openned) then Exit(1);
 if (HV = nil) then Exit(2);

 for i := 0 to _SADY_CNT-1 do sady_change[i] := false;

 for i := 0 to _HV_FUNC_MAX do
  begin
   if ((funkce[i] <> HV.Slot.funkce[i]) or (force)) then
    begin
     case i of
      0..4   : sady_change[0] := true;
      5..8   : sady_change[1] := true;
      9..12  : sady_change[2] := true;
      13..20 : sady_change[3] := true;
      21..28 : sady_change[4] := true;
     end;//case
     HV.Slot.funkce[i] := funkce[i];
     HV.Stav.funkce[i] := funkce[i];
    end;
  end;

 GetMem(fc, sizeof(TFuncCallback));
 TFuncCallback(fc^).sady := TList<TFuncCBSada>.Create();
 TFuncCallback(fc^).addr := HV.adresa;
 TFuncCallback(fc^).callback_ok  := Self.Trakce.callback_ok;
 TFuncCallback(fc^).callback_err := Self.Trakce.callback_err;

 Self.callback_err := TTrakce.GenerateCallback(nil);
 Self.callback_ok  := TTrakce.GenerateCallback(nil);

 if (sady_change[0]) then
  begin
   sada.index := 0;
   for i := 0 to 4 do sada.func[i] := funkce[i];
   TFuncCallback(fc^).sady.Add(sada);
  end;
 if (sady_change[1]) then
  begin
   sada.index := 1;
   for i := 0 to 3 do sada.func[i] := funkce[i+5];
   TFuncCallback(fc^).sady.Add(sada);
  end;
 if (sady_change[2]) then
  begin
   sada.index := 2;
   for i := 0 to 3 do sada.func[i] := funkce[i+9];
   TFuncCallback(fc^).sady.Add(sada);
  end;
 if (sady_change[3]) then
  begin
   sada.index := 3;
   for i := 0 to 7 do sada.func[i] := funkce[i+13];
   TFuncCallback(fc^).sady.Add(sada);
  end;
 if (sady_change[4]) then
  begin
   sada.index := 4;
   for i := 0 to 7 do sada.func[i] := funkce[i+21];
   TFuncCallback(fc^).sady.Add(sada);
  end;

 if ((sady_change[0]) or (sady_change[1]) or (sady_change[2]) or (sady_change[3]) or (sady_change[4])) then
  begin
   HV.Slot.funkce := funkce;
   TCPRegulator.LokUpdateFunc(HV, Sender);
   RegCollector.UpdateElements(Sender, HV.Slot.adresa);
   HV.changed := true;

   Self.FuncOK(Self, fc);
  end else begin
   if (Assigned(TFuncCallback(fc^).callback_ok.callback)) then
    TFuncCallback(fc^).callback_ok.callback(Self, TFuncCallback(fc^).callback_ok.data);
   FreeMem(fc);
  end;

 Result := 0;
end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TTrkGUI.LoksSetFunc(vyznam:string; state:boolean);
var addr, i:Integer;
    cb:Pointer;
begin
 for addr := 0 to _MAX_ADDR-1 do
  begin
   if ((HVDb.HVozidla[addr] = nil) or (not HVDb.HVozidla[addr].Slot.prevzato)) then continue;

   for i := 0 to _HV_FUNC_MAX do
    if ((HVDb.HVozidla[addr].Data.funcVyznam[i] = vyznam) and (HVDb.HVozidla[addr].Slot.funkce[i] <> state)) then
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
       Self.LokSetFunc(Self, HVDb.HVozidla[addr], HVDb.HVozidla[addr].Stav.funkce);

       Exit();
      end;//if vyznam = vyznam
  end;//for i

 if (Assigned(Self.Trakce.callback_ok.callback)) then Self.Trakce.callback_ok.callback(Self, Self.Trakce.callback_ok.data);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TTrkGUI.LoadSpeedTable(filename:string;var LVRych:TListView):Byte;
var i, j:Integer;
    LI:TListItem;
    myFile:TextFile;
 begin
  try
    AssignFile(myFile, filename);
    Reset(myFile);
  except
    Self.TrkLog(self,1,'Chyba pri nacitani souboru s rychlostmi - nepodarilo se pristoupit k souboru !');
    Exit(1);
  end;

  for i := 0 to _MAX_SPEED do
   begin
    if (Eof(myFile)) then
     begin
      Self.TrkLog(self,1,'Chyba pri nacitani souboru s rychlostmi - prilis malo radku');
      CloseFile(myFile);
      for j := i to _MAX_SPEED do Self.SpeedTable[j] := 0;
      Exit(2);
     end else begin
      ReadLn(myFile, Self.SpeedTable[i]);
     end;

    LI := LVRych.Items.Add;
    LI.Caption := IntToStr(i);
    LI.SubItems.Add(IntToStr(Self.SpeedTable[i])+' km/h');
   end;//while

  CloseFile(myFile);
  Result := 0;
end;//function

procedure TTrkGUI.SaveSpeedTable(filename:string);
var i:Integer;
    myFile:TextFile;
 begin
  try
    AssignFile(myFile, filename);
    Rewrite(myFile);
  except
    Self.TrkLog(self,1,'Chyba pri ukladani souboru s rychlostmi - nepodarilo se pristoupit k souboru !');
    Exit;
  end;

  for i := 0 to _MAX_SPEED do WriteLn(myFile, IntToStr(Self.SpeedTable[i]));

  CloseFile(myFile);
end;//procedure

// vrati jizdni stupen prislusny k dane rychlosti v km/h
function TTrkGUI.GettSpeed(kmph:Integer):Integer;
var i:Integer;
begin
 for i := 0 to  _MAX_SPEED do
  if (Self.SpeedTable[i] = kmph) then
    Exit(i);
 Exit(1); // v pripade nenalezeni rychlosti vraci nouzovy STOP
end;//fucntion

procedure TTrkGUI.BeforeOpen(Sender:TObject);
begin
 F_Main.A_Trk_Connect.Enabled       := false;
 F_Main.A_Trk_Disconnect.Enabled    := false;
 F_Main.SB1.Panels.Items[_SB_INT].Text := 'Pøipojování...';
 F_Main.S_Intellibox_connect.Brush.Color := clBlue;
 F_Main.LogStatus('Centrála: pøipojování...');
 F_Main.B_HV_Add.Enabled    := false;
 F_Main.B_HV_Delete.Enabled := false;
 Application.ProcessMessages();
end;//procedure

procedure TTrkGUI.AfterOpen(Sender:TObject);
begin
 Self.Trakce.AfterOpen();
 Self.TrkLog(self, 2, 'OPEN OK');
 F_Main.A_Trk_Connect.Enabled       := false;
 F_Main.A_Trk_Disconnect.Enabled    := true;
 F_Main.SB1.Panels.Items[_SB_INT].Text := 'Centrála pøipojena';
 F_Main.LogStatus('Centrála: pøipojeno');
 F_Main.S_Intellibox_connect.Brush.Color := clLime;
 F_Main.A_All_Loko_Prevzit.Enabled := true;
 F_Main.B_CS_Ver_Update.Enabled    := true;

 F_Main.A_DCC_Go.Enabled   := true;
 F_Main.A_DCC_Stop.Enabled := true;
 F_Main.A_FuncsSet.Enabled := true;

 Application.ProcessMessages();

 // na STATUS se ptam vzdy
 Self.InitSystems();
end;//procedure

procedure TTrkGUI.BeforeClose(Sender:TObject);
begin
 F_Main.A_Trk_Connect.Enabled       := false;
 F_Main.A_Trk_Disconnect.Enabled    := false;
 F_Main.SB1.Panels.Items[_SB_INT].Text := 'Odpojování...';
 F_Main.LogStatus('Centrála: odpojování...');
 F_Main.S_Intellibox_connect.Brush.Color := clBlue;
 Application.ProcessMessages();
 Self.Trakce.BeforeClose();   // smaze buffer historie
end;//procedure

procedure TTrkGUI.AfterClose(Sender:TObject);
var addr:Integer;
begin
 Self.TrkLog(self,2,'CLOSE OK');
 F_Main.A_Trk_Connect.Enabled       := true;
 F_Main.A_Trk_Disconnect.Enabled    := false;
 F_Main.SB1.Panels.Items[_SB_INT].Text := 'Centrála odpojena';
 F_Main.LogStatus('Centrála: odpojena');
 F_Main.S_Intellibox_connect.Brush.Color := clRed;
 F_Main.A_All_Loko_Prevzit.Enabled  := false;
 F_Main.A_All_Loko_Odhlasit.Enabled := false;
 F_Main.B_HV_Add.Enabled            := true;
 F_Main.B_CS_Ver_Update.Enabled     := false;

 // zavrit vsechny regulatory
 RegCollector.CloseAll();

 HVTableData.LoadToTable();

 F_Main.S_Intellibox_go.Brush.Color := clGray;
 F_Main.A_DCC_Go.Enabled   := false;
 F_Main.A_DCC_Stop.Enabled := false;
 F_Main.A_FuncsSet.Enabled := false;
 if (F_FuncsSet.Showing) then F_FuncsSet.Close();
 

 for addr := 0 to _MAX_ADDR-1 do
  if (HVDb.HVozidla[addr] <> nil) then HVDb.HVozidla[addr].Slot.pom := TPomStatus.released;  

 Application.ProcessMessages();

 if (SystemData.Status = stopping) then
   F_Main.A_MTB_StopExecute(nil);
end;//procedure

function TTrkGUI.GetStepSpeed(step:byte):Integer;
begin
 if (step >  _MAX_SPEED) then Exit(-1);
 Result := Self.SpeedTable[step];
end;//function

function TTrkGUI.SetSpetSpeed(step:byte;sp:Integer):Byte;
begin
 if (step >  _MAX_SPEED) then Exit(1);
 Self.SpeedTable[step] := sp;
 Result := 0;
end;//function

function TTrkGUI.EmergencyStop():Byte;
begin
 if (not Self.openned) then Exit(1);
 Self.TrkLog(self,2,'PUT: EMERGENCY STOP');
 Self.Trakce.EmergencyStop();
 Result := 0;
end;//function

function TTrkGUI.EmergencyStopAddr(addr:Integer):Byte;
begin
 if (not Self.openned) then Exit(1);
 if (addr < 0) then Exit(2);
 if (addr > 9999) then Exit(3);

 Self.TrkLog(self,2,'PUT: EMERGENCY STOP LOKO '+IntToStr(addr));
 Self.Trakce.LokEmergencyStop(addr);

 Result := 0;
end;//function

function TTrkGUI.EmergencyStopLoko(Sender:TObject; HV:THV):Byte;
begin
 if (not Self.openned) then Exit(1);
 if (HV = nil) then Exit(2);

 Self.TrkLog(self,2,'PUT: EMERGENCY STOP HV '+HV.data.Nazev+' = '+IntToStr(HV.Adresa));
 Self.Trakce.LokEmergencyStop(HV.Adresa);

 HV.Slot.speed := 0;

 TCPRegulator.LokUpdateSpeed(HV);
 RegCollector.UpdateElements(Sender, HV.Slot.adresa);
 HV.changed := true;

 Result := 0;
end;//fucnction

function TTrkGUI.LokGetSpSteps(HV:THV):Byte;
begin
 if (not Self.openned) then Exit(1);
 if (HV = nil) then Exit(2);

 Self.TrkLog(self,2,'PUT: GET HV INFO '+HV.data.Nazev+' = '+IntToStr(HV.Adresa));
 Self.Trakce.LokGetInfo(HV.Adresa);

 Result := 0;
end;//function

////////////////////////////////////////////////////////////////////////////////

// prevzit loko do kontroly systemu:
//  pri prebirani LOKO si ulozime vnejsi callbacky, abychom mohli na prevzeti aplikovat vlastni callbacky
//  tyto callbacky pak zajisti nastaveni POM podle toho, jestli je loko otevrene v nejakem regulatoru
procedure TTrkGUI.PrevzitLoko(HV:THV);
var cb:Pointer;
begin
 if (not Self.openned) then
  begin
   Self.WriteLog(1, 'ERR: COM not openned');
   raise ENotOpenned.Create('COM not openned');
  end;

 Self.TrkLog(self, 2, 'PUT: LOK-2-MYCONTROL: '+HV.data.Nazev+' ('+IntToStr(HV.Adresa)+')');
 HV.Slot.prevzato_full := false;

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
end;//function

// odhlasovani loko =
//    1) POM release
//    2) odhlasit loko
procedure TTrkGUI.OdhlasitLoko(HV:THV);
var cb:Pointer;
begin
 if (not Self.openned) then
  begin
   Self.WriteLog(1, 'ERR: COM not openned');
   raise ENotOpenned.Create('COM not openned');
  end;

 Self.TrkLog(self, 2, 'PUT: LOK-FROM-MYCONTROL: '+HV.data.Nazev+' ('+IntToStr(HV.Adresa)+')');

 GetMem(cb, sizeof(TPrevzitCallback));
 TPrevzitCallback(cb^).callback_ok  := Self.Trakce.callback_ok;
 TPrevzitCallback(cb^).callback_err := Self.Trakce.callback_err;
 TPrevzitCallback(cb^).addr         := HV.adresa;

 Self.callback_ok  := TTrakce.GenerateCallback(Self.OdhlasenoPOMOK, cb);
 Self.callback_err := TTrakce.GenerateCallback(Self.OdhlasenoPOMErr, cb);

 // nenastavovat HV.ruc, POM si tady delame sami !!
 HV.Stav.ruc := false;

 Self.POMWriteCVs(Self, HV, HV.Data.POMrelease, TPomStatus.released);
end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TTrkGUI.PrevzitAll();
var i:Integer;
    data:Pointer;
    k_prevzeti:Integer;
begin
 k_prevzeti := 0;
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

 F_Main.LogStatus('Loko: žádné loko k pøevzetí');
 F_Main.G_Loko_Prevzato.MaxValue := 1;
 F_Main.G_Loko_Prevzato.Progress := 1;
 Self.AllPrevzato();
end;//procedure

procedure TTrkGUI.OdhlasitAll();
var i:Integer;
    data:Pointer;
    k_odhlaseni:Integer;
begin
 k_odhlaseni := 0;
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
 Self.AllOdhlaseno();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

//event s TTrakce, ktery se zavola pri uspesnem pripojeni ci odhlaseni loko
// v DATA jsou ulozena data callbacku, ktery prislusi prikazu pro prevzeti
procedure TTrkGUI.ConnectChange(Sender: TObject; addr:Integer; code:TConnect_code; data:Pointer);
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
     Self.TrkLog(self,2,'GET LOCO DATA: loko '+HVDb.HVozidla[addr].data.Nazev+' ('+IntToSTr(addr)+')');
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
         ORTCPServer.SendLn(reg.conn, '-;LOK;'+IntToStr(addr)+';AUTH;total;{'+HVDb.HVozidla[addr].GetPanelLokString()+'}')
     else
       for reg in HVDb.HVozidla[addr].Stav.regulators do
         ORTCPServer.SendLn(reg.conn, '-;LOK;'+IntToStr(addr)+';AUTH;ok;{'+HVDb.HVozidla[addr].GetPanelLokString()+'}');

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
end;//procedure

procedure TTrkGUI.SetLogLevel(level:Byte);
begin
 Self.floglevel := level;
 Self.WriteLog(2, 'NEW loglevel = '+IntToStr(level));
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TTrkGUI.InitSystems();
begin
 F_Main.LogStatus('Centrála: testuji komunikaci - vysílám povel STATUS');

 Self.callback_err := TTrakce.GenerateCallback(Self.InitStatErr);
 Self.callback_ok  := TTrakce.GenerateCallback(Self.InitStatOK);
 Self.Trakce.GetTrackStatus();
end;//procedure

procedure TTrkGUI.InitStatErr(Sender:TObject; Data:Pointer);
begin
 F_Main.LogStatus('WARN: Centrála: neodpovìdìla na pøíkaz STATUS, zkouším pøíkaz STOP...');

 Self.callback_err := TTrakce.GenerateCallback(Self.InitStopErr);
 Self.callback_ok  := TTrakce.GenerateCallback(Self.InitStopOK);
 Self.CentralStop();
end;//procedure

procedure TTrkGUI.InitStatOK(Sender:TObject; Data:Pointer);
begin
 Self.finitok := true;
 F_Main.LogStatus('Centrála: komunikuje');

 F_Main.LogStatus('Zjišuji verzi FW v centrále...');
 Self.callback_ok  := TTrakce.GenerateCallback(Self.GotCSVersionOK);
 Self.callback_err := TTrakce.GenerateCallback(Self.GotCSVersionErr);
 Self.GetCSVersion();
end;//procedure

procedure TTrkGUI.InitStopErr(Sender:TObject; Data:Pointer);
begin
 Self.finitok := false;
 F_Main.LogStatus('ERR: Centrála: neodpovìdìla na pøíkaz STOP !');
 SystemData.Status := TSystemStatus.null;
 F_Main.A_System_Start.Enabled := true;
 F_Main.A_System_Stop.Enabled := true;
 Application.MessageBox('Centrála neodpovìdìla na pøíkaz STATUS a STOP', 'Nelze pokraèovat', MB_OK OR MB_ICONWARNING);
end;//procedure

procedure TTrkGUI.InitStopOK(Sender:TObject; Data:Pointer);
begin
 Self.finitok := true;
 F_Main.LogStatus('Centrála: komunikuje');

 Self.GetCSVersion();
 Self.GetLIVersion();

 if (SystemData.Status = starting) then
  begin
   // poslali jsme STOP -> je jasne, ze musime DCC opet zapnout
   F_Main.A_DCC_GoExecute(self)
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TTrkGUI.SetCallbackErr(callback_err:TCommandCallback);
begin
 Self.Trakce.callback_err := callback_err;
end;//procedure

procedure TTrkGUI.SetCallbackOK(callback_ok:TCommandCallback);     
begin
 Self.Trakce.callback_ok := callback_ok;
end;//procedure

////////////////////////////////////////////////////////////////////////////////
// eventy pri prebirani vsech LOKO:

procedure TTrkGUI.PrebiraniUpdateOK(Sender:TObject; Data:Pointer);
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
 F_Main.LogStatus('Loko: všechna loko pøevzata');
 Self.AllPrevzato();
end;//procedure

procedure TTrkGUI.PrebiraniUpdateErr(Sender:TObject; Data:Pointer);
begin
 Self.WriteLog(1, 'ERR: LOKO '+ IntToStr(Integer(data^)) + ' se nepodaøilo pøevzít');
 F_Main.LogStatus('LOKO: loko '+ IntToStr(Integer(data^)) + ' se nepodaøilo pøevzít');

 F_Main.G_Loko_Prevzato.ForeColor := clRed;

 F_Main.S_lok_prevzato.Brush.Color  := clRed; 
 F_Main.A_All_Loko_Prevzit.Enabled  := true;
 
 if (SystemData.Status = TSystemStatus.starting) then
  begin
   SystemData.Status := TSystemStatus.null;
   F_Main.A_System_Start.Enabled := true;
   F_Main.A_System_Stop.Enabled  := true;
  end;

 Application.MessageBox(PChar('LOKO '+ IntToStr(Integer(data^)) + ' se nepodaøilo pøevzít'), 'Chyba', MB_OK OR MB_ICONWARNING);
 FreeMem(data);
end;//procedure

////////////////////////////////////////////////////////////////////////////////
// eventy pri odhlasovani vech LOKO:

procedure TTrkGUI.OdhlasovaniUpdateOK(Sender:TObject; Data:Pointer);
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
     Self.OdhlasitLoko(HVDb.HVozidla[i]);
     Exit();
    end;
  end;

 // zadne dalsi loko k odhlaseni
 FreeMem(data);
 F_Main.LogStatus('Loko: všechna loko odhlášena');
 Application.ProcessMessages();
 Self.AllOdhlaseno();
end;//procedure

procedure TTrkGUI.OdhlasovaniUpdateErr(Sender:TObject; Data:Pointer);
begin
 // pokud behem odhlasovani loko nastane chyba, nahlasime ji, ale loko povazujeme za odhlasene
 Self.WriteLog(1, 'WARN: Loko '+IntToStr(Integer(data^))+ ' se nepodaøilo odhlásit');
 F_Main.LogStatus('WARN: Loko '+IntToStr(Integer(data^))+ ' se nepodaøilo odhlásit');
 F_Main.G_Loko_Prevzato.ForeColor := clRed;
 HVDb.HVozidla[Integer(data^)].Slot.prevzato := false;
 HVDb.HVozidla[Integer(data^)].Slot.prevzato_full := false;
 Self.OdhlasovaniUpdateOK(Self, data);
end;//procedure

////////////////////////////////////////////////////////////////////////////////
// callback funkce pri prebirani jednoho HV a pri programovani POM (pri prebirani):

procedure TTrkGUI.PrevzatoErr(Sender:TObject; Data:Pointer);
begin
 // loko se nepodarilo prevzit -> zavolat error callback
 if (Assigned(TPrevzitCallback(data^).callback_err.callback)) then
   TPrevzitCallback(data^).callback_err.callback(Self, TPrevzitCallback(data^).callback_err.data);
 FreeMem(data);
end;//procedure

procedure TTrkGUI.PrevzatoPOMOK(Sender:TObject; Data:Pointer);
begin
 // HV konecne kompletne prevzato
 HVDb.HVozidla[TPrevzitCallback(data^).addr].Slot.prevzato_full := true;

 // volame OK callback
 if (Assigned(TPrevzitCallback(data^).callback_ok.callback)) then
   TPrevzitCallback(data^).callback_ok.callback(Self, TPrevzitCallback(data^).callback_ok.data);
 FreeMem(data);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TTrkGUI.PrevzatoFunc1328OK(Sender:TObject; Data:Pointer);
var i:Integer;
begin
 // nacetli jsme stav funkci 13-28 -> stav ulozime do slotu
 for i := 13 to 28 do
   HVDb.HVozidla[TPrevzitCallback(data^).addr].Slot.funkce[i] := Self.Trakce.Slot.funkce[i];

 //nastavime funkce tak, jak je chceme my
 Self.callback_ok  := TTrakce.GenerateCallback(Self.PrevzatoFuncOK, data);
 Self.callback_err := TTrakce.GenerateCallback(Self.PrevzatoFuncErr, data);
 Self.LokSetFunc(nil, HVDb.HVozidla[TPrevzitCallback(data^).addr], HVDb.HVozidla[TPrevzitCallback(data^).addr].Stav.funkce);
end;

////////////////////////////////////////////////////////////////////////////////
// callback funkce pri odhlasovani hnaciho vozidla a pri programovani POM pri odhlasovani:

procedure TTrkGUI.OdhlasenoOK(Sender:TObject; Data:Pointer);
begin
 // loko odhlaseno a POM nastaveno -> volame OK callback
 if (Assigned(TPrevzitCallback(data^).callback_ok.callback)) then
   TPrevzitCallback(data^).callback_ok.callback(Self, TPrevzitCallback(data^).callback_ok.data);
 FreeMem(data);
end;//procedure

procedure TTrkGUI.OdhlasenoErr(Sender:TObject; Data:Pointer);
begin
 // POM nastaveno, ale loko neodhlaseno -> volame error callback
 if (Assigned(TPrevzitCallback(data^).callback_err.callback)) then
   TPrevzitCallback(data^).callback_err.callback(Self, TPrevzitCallback(data^).callback_err.data);
 FreeMem(data);
end;//procedure

procedure TTrkGUI.OdhlasenoPOMOK(Sender:TObject; Data:Pointer);
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
end;//procedure

procedure TTrkGUI.OdhlasenoPOMErr(Sender:TObject; Data:Pointer);
begin
 // release POM error -> zavolat error callback
 if (Assigned(TPrevzitCallback(data^).callback_err.callback)) then
   TPrevzitCallback(data^).callback_err.callback(Self, TPrevzitCallback(data^).callback_err.data);
 FreeMem(data);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TTrkGUI.AllPrevzato();
begin
 F_Main.S_lok_prevzato.Brush.Color := clLime;

 F_Main.A_All_Loko_Prevzit.Enabled  := false;
 F_Main.A_All_Loko_Odhlasit.Enabled := true;

 F_Main.G_Loko_Prevzato.Progress  := HVDb.cnt;
 F_Main.G_Loko_Prevzato.ForeColor := clLime;

 if (SystemData.Status = starting) then
   F_Main.A_PanelServer_StartExecute(nil);
end;//procedure

procedure TTrkGUI.AllOdhlaseno();
begin
 F_Main.S_lok_prevzato.Brush.Color := clRed;

 F_Main.A_All_Loko_Prevzit.Enabled  := true;
 F_Main.A_All_Loko_Odhlasit.Enabled := false;

 F_Main.G_Loko_Prevzato.Progress  := 0;
 F_Main.G_Loko_Prevzato.ForeColor := clBlue;

 if (SystemData.Status = stopping) then
   F_Main.SetCallMethod(F_Main.A_Trk_DisconnectExecute);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TTrkGUI.LokComErr(Sender:TObject; addr:Integer);
begin
 if (not Assigned(HVDb.HVozidla[addr])) then Exit();

 if (not HVDb.HVozidla[addr].Slot.com_err) then
  begin
   HVDb.HVozidla[addr].Slot.com_err := true;
   HVDb.HVozidla[addr].changed := true;
  end;
end;//procedure

procedure TTrkGUI.LokComOK(Sender:TObject; addr:Integer);
begin
 if (not Assigned(HVDb.HVozidla[addr])) then Exit();

 if (HVDb.HVozidla[addr].Slot.com_err) then
  begin
   HVDb.HVozidla[addr].Slot.com_err := false;
   HVDb.HVozidla[addr].changed := true;
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TTrkGUI.OnComError(Sender: TObject; Errors: TComErrors);
begin
 Self.WriteLog(1, 'ERR: COM PORT ERROR');
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TTrkGUI.OnComException(Sender: TObject;
  TComException: TComExceptions; ComportMessage: string; WinError: Int64;
  WinMessage: string);
begin
 Self.WriteLog(1, 'ERR: COM PORT EXCEPTION: '+ComportMessage+'; '+WinMessage);
 raise Exception.Create(ComportMessage);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TTrkGUI.OnTrackStatusChange(Sender: TObject);
begin
 if (Self.Trakce.TrackStatus = Ttrk_status.TS_ON) then Self.DCCGoTime := Now;
 F_Main.OnCentralaDCCChange(Self, Self.Trakce.TrackStatus = Ttrk_status.TS_ON);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TTrkGUI.GetTrkStatus():Ttrk_status;
begin
 Result := Self.Trakce.TrackStatus;
end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TTrkGUI.OnComWriteError(Sender:TObject);
begin
 if (Self.openned) then Self.Close();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

// tato funkce je volana pri vypnuti systemu / vypne u vsech hnacich vozidel zvuk
// zvuk si ale zapamatuje jako zaply pro pristi nabeh systemu
procedure TTrkGUI.TurnOffFunctions(callback:TNotifyEvent);
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
   if ((HVDb.HVozidla[i] <> nil) and (HVDb.HVozidla[i].Slot.prevzato) and (not HVDb.HVozidla[i].Slot.stolen)) then
    begin
     func := -1;
     for j := 0 to _HV_FUNC_MAX do
      if ((HVDb.HVozidla[i].Data.funcVyznam[j] = 'zvuk') and (HVDb.HVozidla[i].Slot.funkce[j])) then
       begin
        func := j;
        break;
       end;
     if (func = -1) then continue;

     GetMem(addr, 3);
     Integer(addr^) := i;

     newfuncs := HVDb.HVozidla[i].Stav.funkce;
     newfuncs[func] := false;

     Self.callback_err := Trakce.GenerateCallback(Self.TurnOffFunctions_cmdOK, addr);
     Self.callback_ok  := Trakce.GenerateCallback(Self.TurnOffFunctions_cmdOK, addr);

     Self.LokSetFunc(Self, HVDb.HVozidla[i], newfuncs);
     HVDb.HVozidla[i].Stav.funkce[func] := true;

     Exit();
    end;
  end;//for i

 // no loco
 if Assigned(Self.turnoff_callback) then
  begin
   Self.turnoff_callback(Self);
   Self.turnoff_callback := nil;
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TTrkGUI.TurnOffFunctions_cmdOK(Sender:TObject; Data:Pointer);
var i, j:Integer;
    func:Integer;
    newfuncs:TFunkce;
begin
 for i := Integer(data^)+1 to _MAX_ADDR-1 do
  begin
   if ((HVDb.HVozidla[i] <> nil) and (HVDb.HVozidla[i].Slot.prevzato) and (not HVDb.HVozidla[i].Slot.stolen)) then
    begin
     func := -1;
     for j := 0 to _HV_FUNC_MAX do
      if ((HVDb.HVozidla[i].Data.funcVyznam[j] = 'zvuk') and (HVDb.HVozidla[i].Slot.funkce[j])) then
       begin
        func := j;
        break;
       end;
     if (func = -1) then continue;

     Integer(data^) := i;

     newfuncs := HVDb.HVozidla[i].Stav.funkce;
     newfuncs[func] := false;

     Self.callback_err := Trakce.GenerateCallback(Self.TurnOffFunctions_cmdOK, data);
     Self.callback_ok  := Trakce.GenerateCallback(Self.TurnOffFunctions_cmdOK, data);

     Self.LokSetFunc(Self, HVDb.HVozidla[i], newfuncs);
     HVDb.HVozidla[i].Stav.funkce[func] := true;

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
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TTrkGUI.GotCSVersion(Sender:TObject; version:TCSVersion);
begin
 F_Main.L_CS_FW.Caption := IntToStr(version.major) + '.' + IntToStr(version.minor);
 F_Main.L_CS_ID.Caption := IntToStr(version.id);
 F_Main.L_CS_UpdateTime.Caption := FormatDateTime('dd.mm.yyyy hh:nn:ss', Now);
 F_Main.LogStatus('FW v centrále: '+IntToStr(version.major) + '.' + IntToStr(version.minor) + ', id: '+IntToStr(version.id));
end;//procedure

procedure TTrkGUI.GotLIVersion(Sender:TObject; version:TLIVersion);
begin
 F_Main.L_CS_LI_FW.Caption := 'HW: ' + IntToStr(version.hw_major) + '.' + IntToStr(version.hw_minor) +
                              ', SW: ' + IntToStr(version.sw_major) + '.' + IntToStr(version.sw_minor);
 F_Main.L_CS_UpdateTime.Caption := FormatDateTime('dd.mm.yyyy hh:nn:ss', Now);
 F_Main.LogStatus('FW v LI: '+F_Main.L_CS_LI_FW.Caption);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TTrkGUI.GetCSVersion();
begin
 Self.Trakce.GetCSVersion(Self.GotCSVersion);
end;//procedure

procedure TTrkGUI.GetLIVersion();
begin
 Self.Trakce.GetLIVersion(Self.GotLIVersion);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TTrkGUI.POMWriteCV(Sender:TObject; HV:THV; cv:Word; data:byte):Integer;
begin
 if (not Self.openned) then Exit(1);
 if (HV = nil) then Exit(2);

 Self.TrkLog(self, 2, 'PUT: POM '+HV.data.Nazev+' ('+IntToStr(HV.Adresa)+') : '+IntToStr(cv)+':'+IntToStr(data));
 Self.Trakce.POMWriteCV(HV.adresa, cv, data);
 Result := 0;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

// zapsat seznam vsech cv v listu
procedure TTrkGUI.POMWriteCVs(Sender:TObject; HV:THV; list:TList<THVPomCV>; new:TPomStatus);
var data:Pointer;
begin
 // vytvorime si callback
 GetMem(data, sizeof(TPOMCallback));
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
   Self.POMWriteCV(Sender, HV, list[0].cv, list[0].data);
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TTrkGUI.POMCvWroteOK(Sender:TObject; Data:Pointer);
begin
 if (TPOMCallback(data^).index >= (TPOMCallback(data^).list.Count-1)) then
  begin
   // posledni data -> zavolame OK event

   if (Assigned(HVDb.HVozidla[TPOMCallback(data^).addr])) then
    begin
     HVDb.HVozidla[TPOMCallback(data^).addr].Slot.pom := TPOMCallback(data^).new;
     HVDb.HVozidla[TPOMCallback(data^).addr].changed := true;
     RegCollector.ConnectChange(TPOMCallback(data^).addr);
//     HVDb.HVozidla[TPOMCallback(data^).addr].UpdateRuc(); zakomentovano - resi se pri RUC hanciho vozidla
    end;

   if (Assigned(TPOMCallback(data^).callback_ok.callback)) then TPOMCallback(data^).callback_ok.callback(Self, TPOMCallback(data^).callback_ok.data);
   FreeMem(data);
  end else begin
   // odesleme dalsi data
   TPOMCallback(data^).index := TPOMCallback(data^).index+1;

   Self.callback_err := TTrakce.GenerateCallback(Self.POMCvWroteErr, data);
   Self.callback_ok  := TTrakce.GenerateCallback(Self.POMCvWroteOK, data);
   Self.POMWriteCV(Sender, HVDB.HVozidla[TPOMCallback(data^).addr], TPOMCallback(data^).list[TPOMCallback(data^).index].cv, TPOMCallback(data^).list[TPOMCallback(data^).index].data);
  end;// else konec dat
end;//procedure

// pokud pri POMu nastane chyba, zavolame Error callback a ukoncime programovani
procedure TTrkGUI.POMCvWroteErr(Sender:TObject; Data:Pointer);
begin
 if (Assigned(HVDb.HVozidla[TPOMCallback(data^).addr])) then
  begin
   HVDb.HVozidla[TPOMCallback(data^).addr].Slot.pom := TPomStatus.error;
   HVDb.HVozidla[TPOMCallback(data^).addr].changed  := true;
   RegCollector.ConnectChange(TPOMCallback(data^).addr);
//   HVDb.HVozidla[TPOMCallback(data^).addr].UpdateRuc(); resi se pri RUC hnaciho vozidla
  end;

 if (Assigned(TPOMCallback(data^).callback_err.callback)) then
  TPOMCallback(data^).callback_err.callback(Self, TPOMCallback(data^).callback_err.data);
 FreeMem(data);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TTrkGUI.NouzReleaseCallbackErr(Sender:TObject; Data:Pointer);
begin
 HVDb.HVozidla[Integer(data^)].Slot.prevzato      := false;
 HVDb.HVozidla[Integer(data^)].Slot.prevzato_full := false;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TTrkGUI.GotCSVersionOK(Sender:TObject; Data:Pointer);
begin
 Self.callback_ok  := TTrakce.GenerateCallback(Self.GotLIVersionOK);
 Self.callback_err := TTrakce.GenerateCallback(Self.GotLIVersionErr);
 Self.GetLIVersion();
end;//procedure

procedure TTrkGUI.GotCSVersionErr(Sender:TObject; Data:Pointer);
begin
 F_Main.LogStatus('WARN: Centrála nepodvìdìla na požadavek o verzi centrály, pokraèuji...');
 Self.GotCSVersionOK(Self, data);
end;//procedure

procedure TTrkGUI.GotLIVersionOK(Sender:TObject; Data:Pointer);
begin
 if (SystemData.Status = starting) then
  begin
   if (Self.Trakce.TrackStatus <> Ttrk_status.TS_ON) then
     F_Main.A_DCC_GoExecute(self)
    else
     F_Main.A_All_Loko_PrevzitExecute(nil);
  end;
end;//procedure

procedure TTrkGUI.GotLIVersionErr(Sender:TObject; Data:Pointer);
begin
 F_Main.LogStatus('WARN: Centrála nepodvìdìla na požadavek o verzi LI, pokraèuji...');
 Self.GotLIVersionOK(Self, data);
end;//procedure

////////////////////////////////////////////////////////////////////////////////
// callbacky pri nastavovani funkci hnacicho vozidel (F0-Fn)
// data jsou TFuncCallback

procedure TTrkGUI.FuncOK(Sender:TObject; Data:Pointer);
var i:Integer;
    func:Byte;
    sada:Integer;
    s:string;
begin
 if (TFuncCallback(data^).sady.Count < 1) then
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
       Self.TrkLog(self, 2, 'PUT: LOK FUNC 0-4: '+HVDb.HVozidla[TFuncCallback(data^).addr].Data.Nazev+' ('+IntToStr(TFuncCallback(data^).addr)+') : '+s);

       func := 0;
       for i := 0 to 3 do if (TFuncCallback(data^).sady[0].func[i+1]) then func := func or (1 shl (i+1));
       if (TFuncCallback(data^).sady[0].func[0]) then func := func or 1;
    end;

    1: begin
       for i := 0 to 3 do s := s + IntToStr(PrevodySoustav.BoolToInt(TFuncCallback(data^).sady[0].func[i]));
       Self.TrkLog(self, 2, 'PUT: LOK FUNC 5-8: '+HVDb.HVozidla[TFuncCallback(data^).addr].Data.Nazev+' ('+IntToStr(TFuncCallback(data^).addr)+') : '+s);

       func := 0;
       for i := 0 to 3 do if (TFuncCallback(data^).sady[0].func[i]) then func := func or (1 shl i);
    end;

    2: begin
       for i := 0 to 3 do s := s + IntToStr(PrevodySoustav.BoolToInt(TFuncCallback(data^).sady[0].func[i]));
       Self.TrkLog(self, 2, 'PUT: LOK FUNC 9-12: '+HVDb.HVozidla[TFuncCallback(data^).addr].Data.Nazev+' ('+IntToStr(TFuncCallback(data^).addr)+') : '+s);

       func := 0;
       for i := 0 to 3 do if (TFuncCallback(data^).sady[0].func[i]) then func := func or (1 shl i);
    end;

    3: begin
       for i := 0 to 7 do s := s + IntToStr(PrevodySoustav.BoolToInt(TFuncCallback(data^).sady[0].func[i]));
       Self.TrkLog(self, 2, 'PUT: LOK FUNC 13-20: '+HVDb.HVozidla[TFuncCallback(data^).addr].Data.Nazev+' ('+IntToStr(TFuncCallback(data^).addr)+') : '+s);

       func := 0;
       for i := 0 to 7 do if (TFuncCallback(data^).sady[0].func[i]) then func := func or (1 shl i);
    end;

    4: begin
       for i := 0 to 7 do s := s + IntToStr(PrevodySoustav.BoolToInt(TFuncCallback(data^).sady[0].func[i]));
       Self.TrkLog(self, 2, 'PUT: LOK FUNC 21-28: '+HVDb.HVozidla[TFuncCallback(data^).addr].Data.Nazev+' ('+IntToStr(TFuncCallback(data^).addr)+') : '+s);

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
   Self.Trakce.LokSetFunc(TFuncCallback(data^).addr, sada, func);
  end;
end;//procedure

procedure TTrkGUI.FuncErr(Sender:TObject; Data:Pointer);
begin
 // chyba pri nastavovani funkci -> zavolame error callback
 TFuncCallback(data^).sady.Free();
 if (Assigned(TFuncCallback(data^).callback_err.callback)) then
    TFuncCallback(data^).callback_err.callback(Self, TFuncCallback(data^).callback_err.data);
 FreeMem(data);
end;//procedure

////////////////////////////////////////////////////////////////////////////////
// callbacky pri nastavovani funkci hnaciho vozidla pri prebirani:

procedure TTrkGUI.PrevzatoFuncOK(Sender:TObject; Data:Pointer);
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
end;//procedure

procedure TTrkGUI.PrevzatoFuncErr(Sender:TObject; Data:Pointer);
begin
 // loko prevzato, ale funkce se nepodarilo nastavit -> error callback
 Self.WriteLog(1, 'WARN: LOKO '+ IntToStr(TPrevzitCallback(data^).addr) + ' se nepodaøilo nastavit funkce');
 F_Main.LogStatus('WARN: loko '+ IntToStr(TPrevzitCallback(data^).addr) + ' se nepodaøilo nastavit funkce');
 Self.PrevzatoFuncOK(Self, data);
end;//procedure

////////////////////////////////////////////////////////////////////////////////
// callbacky hromadneho nastavovani funkci dle vyznamu:
//    napr. zapni "zvuk" vsech hnacich vozidel

procedure TTrkGUI.LoksSetFuncOK(Sender:TObject; Data:Pointer);
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
       Self.LokSetFunc(Self, HVDb.HVozidla[addr], HVDb.HVozidla[addr].Stav.funkce);

       Exit();
      end;//if vyznam = vyznam
  end;//for i

 if (Assigned(TFuncsCallback(data^).callback_ok.callback)) then TFuncsCallback(data^).callback_ok.callback(Self, TFuncsCallback(data^).callback_ok.data);
 FreeMem(data);
end;//procedure

procedure TTrkGUI.LoksSetFuncErr(Sender:TObject; Data:Pointer);
begin
 // sem lze pridat oznameni chyby
 Self.LoksSetFuncOK(Sender, Data);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TTrkGUI.FastResetLoko();
var i:Integer;
begin
 for i := 0 to _MAX_ADDR-1 do
  begin
   if (HVDb.HVozidla[i] = nil) then continue;
   if (HVDb.HVozidla[i].Slot.Prevzato) then
    begin
     HVDb.HVozidla[i].Slot.Prevzato := false;
     HVDb.HVozidla[i].Slot.pom := TPomStatus.released;
    end;
  end;//for i
 Self.AllOdhlaseno();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrkGUI.NouzReleaseLoko();
var i:Integer;
    data:Pointer;
begin
 GetMem(data, sizeof(integer));
 for i := 0 to _MAX_ADDR-1 do
  begin
   if (HVDb.HVozidla[i] = nil) then continue;

   try
     if (HVDb.HVozidla[i].Slot.prevzato) then
      begin
       Integer(data^) := i;
       Self.callback_err := TTrakce.GenerateCallback(Self.NouzReleaseCallbackErr, data);
       Self.OdhlasitLoko(HVDb.HVozidla[i]);
      end;
     while (HVDb.HVozidla[i].Slot.prevzato) do
      begin
       sleep(1);
       Application.ProcessMessages;
      end;
   except

   end;
  end;
 FreeMem(data);
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit
