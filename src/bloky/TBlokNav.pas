unit TBlokNav;

//definice a obsluha technologickeho bloku Navestidlo

interface

uses IniFiles, TBlok, Menus, TOblsRizeni, SysUtils, Classes, rrEvent,
      TechnologieJC, IdContext, Generics.Collections, THnaciVozidlo,
      TOblRizeni, StrUtils, JsonDataObjects, TechnologieRCS, Train, JclPCRE;

type
 TBlkNavVolba = (none = 0, VC = 1, PC = 2, NC = 3, PP = 4);
 TBlkNavOutputType = (scom = 0, binary = 1);
 TBlkNavSymbol = (unknown = -1, hlavni = 0, seradovaci = 1);
 TBlkNavCode = (
   ncChanging = -2,
   ncDisabled = -1,
   ncStuj = 0,
   ncVolno = 1,
   ncVystraha = 2,
   ncOcek40 = 3,
   ncVolno40 = 4,
   ncVse = 5,
   ncVystraha40 = 6,
   nc40Ocek40 = 7,
   ncPrivol = 8,
   ncPosunZaj = 9,
   ncPosunNezaj = 10,
   ncOpakVolno = 11,
   ncOpakVystraha = 12,
   ncZhasnuto = 13,
   ncOpakOcek40 = 14,
   ncOpakVystraha40 = 15,
   ncOpak40Ocek40 = 16
 );

 ENoEvents = class(Exception);

 // zastaovoaci a zpomalovaci udalost pro jeden typ soupravy a jeden rozsah delek soupravy
 TBlkNavTrainEvent = class
  train_typ_re: TJclRegEx;                        // regexp matchujici typ soupravy
  delka:record                                  // tato udalost je brana v potaz, pokud ma souprava delku > min && < max
    min:Integer;
    max:Integer;
  end;

  zastaveni: TRREv;                             // zastavovaci udalost
  zpomaleni: record                             // zpomalovaci udalost
    enabled: boolean;                             // povolena zpomalovaci udalost?
    speed: Integer;                               // rychlost z km/h (40, 50, 60...)
    ev: TRREv;                                    // udalost
  end;

   constructor Create(); overload;
   constructor Create(str: string; old: boolean); overload;
   destructor Destroy(); override;

   procedure Parse(str:string; old:boolean);
   class function ParseTrainTypes(types: string): string;
   function ToFileStr(short:boolean = false): string;
   class function ParseOldRychEvent(str:string): TRREv;
 end;

 // vlastnosti bloku Nav, ktere se ukladaji do databaze bloku
 TBlkNavSettings = record
  RCSAddrs:TRCSAddrs;                            // ve skutecnosti je signifikantni jen jedna adresa - na indexu [0], coz je vystup pro rizeni navestidla
  OutputType:TBlkNavOutputType;                  // typ vystupu: binarni/SCom
  events:TObjectList<TBlkNavTrainEvent>;         // tady jsou ulozena veskera zastavovani a zpomalovani; zastaveni na indexu 0 je vzdy primarni
                                                 // program si pamatuje vice zastavovacich a zpomalovaich udalosti pro ruzne typy a delky soupravy
  ZpozdeniPadu:Integer;                          // zpozdeni padu navestidla v sekundach (standartne 0)
  zamknuto:boolean;                              // jestli je navestidlo trvale zamknuto na STUJ (hodi se napr. u navestidel a konci kusych koleji)
 end;

 // stav bloku Nav
 TBlkNavStav = record
  ZacatekVolba:TBlkNavVolba;                     // zacatek volby jidni cesty
  ZacatekAB:Boolean;                             // jestli je zacatek volby JC v rezimu AB
  Navest: TBlkNavCode;                           // aktualni navest dle kodu SCom; pokud je vypla komunikace, -1
  cilova_navest: TBlkNavCode;                    // navest, ktera ma byt nastavena
  navest_old: TBlkNavCode;                       // behem staveni obsahuje byvalou navest
  ABJC:TJC;                                      // odkaz na automaticky stavenou JC
  ZAM:Boolean;                                   // navestidlo zamkle z panelu
  dn_jc_ref,privol_ref:TJC;                      // reference na aktualni JC na navestidle (resp. NC)

  padani:boolean;                                // zde je true, pokud navestidlo pada do STUJ (ma zpozdeny pad) a jeste nespadlo
  padani_start:TDateTime;                        // start padani

  privol_start:TDateTime;                        // start privolavaci navesti (privolavacka sviti pouze omezeny cas a pak se vypne)
  privol_timer_id:Integer;                       // id timeru ukonceni privolavacky v panelu, ze ktreho byla JC postavena
  autoblok:boolean;

  toRnz:TDictionary<Integer, Cardinal>;          // seznam bloku k RNZ spolu s pocty ruseni, ktere je treba udelat
  RCtimer:Integer;                               // id timeru, ktery se prave ted pouziva pro ruseni JC
  changeCallbackOk, changeCallbackErr: TNotifyEvent; // notifikace o nastaveni polohy navestidla
  changeEnd: TTime;
 end;

 // vlastnosti navestidla ziskane ze souboru .spnl (od reliefu, resp. z Mergeru)
 TBlkNavRel = record
  SymbolType:TBlkNavSymbol;                      // typ navestidla
  UsekID:Integer;                                // ID useku pred navestidlem
  smer:THVStanoviste;                            // smer navetidla (lichy X sudy)
 end;

 // Blok Nav (Navestidlo)
 TBlkNav = class(TBlk)
  const
   //defaultni stav
   _def_Nav_stav:TBlkNavStav = (
     ZacatekVolba : none;
     ZacatekAB : false;
     Navest : ncDisabled;
     ABJC : nil;
     ZAM : false;
     dn_jc_ref : nil;
     privol_ref : nil;
     privol_timer_id : 0;
     autoblok : false;
     RCtimer : -1;
     changeCallbackOk: nil;
     changeCallbackErr: nil;
   );

   // privolavaci navest sviti jednu minitu a 30 sekund
   _PRIVOL_MIN = 1;
   _PRIVOL_SEC = 30;

   _NAV_DEFAULT_DELAY = 2;
   _NAV_CHANGE_DELAY_MSEC = 1000;
   _NAV_CHANGE_SHORT_DELAY_MSEC = 200;

  private
   NavSettings: TBlkNavSettings;
   NavStav: TBlkNavStav;
   NavRel: TBlkNavRel;

   fUsekPred:TBlk;
   lastEvIndex:Integer;

    function IsEnabled(): Boolean;
    function RCinProgress():boolean;

    procedure mSetNavest(navest: TBlkNavCode);

    function GetAB():boolean;
    procedure SetAB(ab:boolean);
    procedure SetABJC(ab:TJC);

    procedure SetZacatekVolba(typ:TBlkNavVolba);
    procedure SetZAM(zam:boolean);

    // obsluha polozek v menu panelu
    procedure MenuVCStartClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuVCStopClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuPCStartClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuPCStopClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuSTUJClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuDNClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuRCClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuABStartClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuABStopClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuLockClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuUnlockClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuPNStartClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuPNStopClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuPPStartClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuPPStopClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuPPNClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuRNZClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuKCDKClick(SenderPnl:TIdContext; SenderOR:TObject);

    // DEBUG menu:
    procedure MenuAdminStopIR(SenderPnl:TIdContext; SenderOR:TObject; enabled:boolean);

    procedure UpdatePadani();
    procedure UpdatePrivol();
    procedure UpdateNavestSet();
    procedure OnNavestSetOk();
    procedure OnNavestSetError();

    procedure PrivolDKClick(SenderPnl:TIDContext; SenderOR:TObject; Button:TPanelButton);
    procedure PrivokDKPotvrSekv(Sender:TIdContext; success:boolean);
    procedure RNZPotvrSekv(Sender:TIdContext; success:boolean);

    procedure SetUsekPredID(new_id:Integer);

    function CurrentEventIndex():Integer;

    function GetUsekPred():TBlk;
    function CanIDoRNZ():boolean;

    procedure UnregisterAllEvents();
    function IsChanging():boolean;
    function GetCilovaNavest(): TBlkNavCode;

  public
    constructor Create(index:Integer);
    destructor Destroy(); override;

    function IsPovolovaciNavest(jctype:TJCType = TJCType.vlak):boolean; overload;
    function IsOpakVystraha(): Boolean;
    class function IsPovolovaciNavest(Navest: TBlkNavCode; jctype:TJCType = TJCType.vlak):boolean; overload;

    //load/save data
    procedure LoadData(ini_tech:TMemIniFile;const section:string;ini_rel,ini_stat:TMemIniFile); override;
    procedure SaveData(ini_tech:TMemIniFile;const section:string); override;
    procedure SaveStatus(ini_stat:TMemIniFile;const section:string); override;

    //enable or disable symbol on relief
    procedure Enable(); override;
    procedure Disable(); override;
    function UsesRCS(addr: TRCSAddr; portType: TRCSIOType): Boolean; override;

    //update states
    procedure Update(); override;
    procedure Change(now:boolean = false); override;

    procedure JCZrusNavest();   // zahrnuje cas na pad navesti
    procedure SetNavest(navest: TBlkNavCode; changeCallbackOk, changeCallbackErr: TNotifyEvent);

    //----- Nav own functions -----

    function GetSettings():TBlkNavSettings;
    procedure SetSettings(data:TBlkNavSettings);

    procedure UpdateRychlostTrain(force:boolean = false);
    procedure AddBlkToRnz(blkId:Integer; change:boolean = true);
    procedure RemoveBlkFromRnz(blkId:Integer);
    procedure RCtimerTimeout();
    function FourtyKmph(): Boolean;
    class function AddOpak(navest: TBlkNavCode): TBlkNavCode;

    function GetTrain(usek:TBlk = nil): TTrain;
    procedure PropagatePOdjToTrat();

    class function NavestToString(navest: TBlkNavCode): string;

    property SymbolType:TBlkNavSymbol read NavRel.SymbolType;
    property UsekID:Integer read NavRel.UsekID write SetUsekPredID;
    property Smer:THVStanoviste read NavRel.smer write NavRel.smer;

    //stavove promenne
    property navest: TBlkNavCode read NavStav.Navest write mSetNavest;
    property cilovaNavest: TBlkNavCode read GetCilovaNavest;
    property ZacatekVolba:TBlkNavVolba read NavStav.ZacatekVolba write SetZacatekVolba;
    property ZacatekAB:Boolean read NavStav.ZacatekAB;
    property AB:boolean read GetAB write SetAB;
    property ABJC:TJC read NavStav.ABJC write SetABJC;
    property ZAM:boolean read NavStav.ZAM write SetZAM;
    property Lichy:THVStanoviste read NavRel.Smer;
    property DNjc:TJC read NavStav.dn_jc_ref write NavStav.dn_jc_ref;
    property privol:TJC read NavStav.privol_ref write NavStav.privol_ref;
    property UsekPred:TBlk read GetUsekPred;
    property autoblok:boolean read NavStav.autoblok write NavStav.autoblok;
    property canRNZ:boolean read CanIDoRNZ;
    property RCtimer:Integer read NavStav.RCtimer write NavStav.RCtimer;
    property changing:Boolean read IsChanging;
    property enabled: Boolean read IsEnabled;

    //GUI:

    procedure PanelMenuClick(SenderPnl:TIdContext; SenderOR:TObject; item:string; itemindex:Integer); override;
    function ShowPanelMenu(SenderPnl:TIdContext; SenderOR:TObject; rights:TORCOntrolRights):string; override;
    procedure PanelClick(SenderPnl:TIdCOntext; SenderOR:TObject; Button:TPanelButton; rights:TORCOntrolRights; params:string = ''); override;
    function PanelStateString():string; override;

    //PT:

    procedure GetPtData(json:TJsonObject; includeState:boolean); override;
    procedure GetPtState(json:TJsonObject); override;

 end;//class TBlkNav

//format dat Navu v souboru *.ini:
//  ev=udalosti zastavovani a zpomalovani
//  OutType=typ vystupu (scom, binarni)
//  zamknuti=zamknuti navestidla trvale do STUJ

// format ev: (ev1)(ev2)(ev3)
// format ev1: RychEvent-zastaveni|RychEvent-zpomaleni|re:train_typ_regexp|min_delka|max_delka
//      train_typ, min_delka a max_delka jsou u eventu 0 (globalniho eventu) vynechany
//      vsechny dalsi eventy jsou specificke -> vyse zminene informace v nich jsou ulozeny

//format RychEvent data: textove ulozeny 1 radek, kde jsou data oddelena ";"
//   typ_zastaveni(0=usek;1=ir);
//    pro usek nasleduje: usekid;usekpart;speed;
//    pro ir nasleduje: irid;speed;

////////////////////////////////////////////////////////////////////////////////

implementation

uses TBloky, TBlokUsek, TJCDatabase, TCPServerOR, Graphics,
     GetSystems, Logging, TrainDb, TBlokIR, Zasobnik, ownStrUtils,
     TBlokTratUsek, TBlokTrat, TBlokVyhybka, TBlokZamek, TechnologieAB,
     predvidanyOdjezd, ownConvert;

constructor TBlkNav.Create(index:Integer);
begin
 inherited Create(index);

 Self.GlobalSettings.typ := btNav;
 Self.NavStav := Self._def_nav_stav;
 Self.NavStav.toRnz := TDictionary<Integer, Cardinal>.Create();
 Self.NavSettings.events := TObjectList<TBlkNavTrainEvent>.Create();
 Self.fUsekPred := nil;
end;

destructor TBlkNav.Destroy();
begin
 Self.NavStav.toRnz.Free();
 Self.NavSettings.events.Free();

 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkNav.LoadData(ini_tech:TMemIniFile;const section:string;ini_rel,ini_stat:TMemIniFile);
var strs: TStrings;
    str: string;
    i: Integer;
begin
 inherited LoadData(ini_tech, section, ini_rel, ini_stat);

 Self.NavSettings.RCSAddrs := Self.LoadRCS(ini_tech, section);

 Self.NavSettings.zamknuto := ini_tech.ReadBool(section, 'zamknuti', false);

 Self.NavSettings.OutputType := TBlkNavOutputType(ini_tech.ReadInteger(section, 'OutType', 0));
 Self.NavSettings.ZpozdeniPadu := ini_tech.ReadInteger(section, 'zpoz', _NAV_DEFAULT_DELAY);

 strs := Self.LoadORs(ini_rel, 'N');
 try
   if (strs.Count >= 3) then
    begin
     Self.NavRel.SymbolType := TBlkNavSymbol(StrToInt(strs[1]));

     // 0 = navestidlo v lichem smeru. 1 = navestidlo v sudem smeru
     if (strs[2] = '0') then
       Self.NavRel.smer := THVStanoviste.lichy
     else
       Self.NavRel.smer := THVStanoviste.sudy;
     Self.NavRel.UsekID := StrToInt(strs[3]);
    end else begin
     Self.NavRel.SymbolType := TBlkNavSymbol.unknown;
     Self.NavRel.smer := THVStanoviste.lichy;
     Self.NavRel.UsekID := -1;
    end;
 finally
   strs.Free();
 end;

 // Nacitani zastavovacich a zpomaovacich udalosti
 // toto musi byt az po nacteni spnl
 Self.NavSettings.events.Clear();

 str := ini_tech.ReadString(section, 'ev', '');
 if (str <> '') then
  begin
   // 1) stary zpusob nacitani zastavovacich udalosti (vsechny udalosti ve starem
   //    formatu na jednom radku). Tohle je tady hlavne kvuli zpetne kompatibilite.
   strs := TStringList.Create();
   try
     ExtractStrings(['(', ')'], [], PChar(str), strs);
     for str in strs do
      begin
       try
         Self.NavSettings.events.Add(TBlkNavTrainEvent.Create(str, true));
       except

       end;
      end;
   finally
     strs.Free();
   end;

  end else begin
   // 2) novy zpusob nacitani zastavovacich udalosti
   //    kazda udalost na samostatnem radku ev1=... ev2=... ...
   i := 0;
   str := ini_tech.ReadString(section, 'ev'+IntToStr(i), '');
   while (str <> '') do
    begin
     try
       Self.NavSettings.events.Add(TBlkNavTrainEvent.Create(str, false));
     except

     end;
     Inc(i);
     str := ini_tech.ReadString(section, 'ev'+IntToStr(i), '');
    end;
  end;

 PushRCSToOR(Self.ORsRef, Self.NavSettings.RCSAddrs);
end;

procedure TBlkNav.SaveData(ini_tech:TMemIniFile;const section:string);
var i:Integer;
begin
 inherited SaveData(ini_tech, section);

 Self.SaveRCS(ini_tech, section, Self.NavSettings.RCSAddrs);

 for i := 0 to Self.NavSettings.events.Count-1 do
   ini_tech.WriteString(section, 'ev'+IntToStr(i), Self.NavSettings.events[i].ToFileStr(i = 0));

 if (Self.NavSettings.RCSAddrs.Count > 0) then
   ini_tech.WriteInteger(section, 'OutType', Integer(Self.NavSettings.OutputType));

 if (Self.NavSettings.ZpozdeniPadu <> _NAV_DEFAULT_DELAY) then
   ini_tech.WriteInteger(section, 'zpoz', Self.NavSettings.ZpozdeniPadu);

 if (Self.NavSettings.zamknuto) then
   ini_tech.WriteBool(section, 'zamknuti', Self.NavSettings.zamknuto);
end;

procedure TBlkNav.SaveStatus(ini_stat:TMemIniFile;const section:string);
begin

end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkNav.Enable();
var rcsaddr: TRCSAddr;
    enable: Boolean;
begin
 if (Self.Navest <> ncDisabled) then
   Exit(); // skip already enabled block

 enable := true;
 try
   for rcsaddr in Self.NavSettings.RCSAddrs do
     if (not RCSi.IsNonFailedModule(rcsaddr.board)) then
       enable := false;
 except
   enable := false;
 end;

 if (enable) then
  begin
   Self.NavStav.Navest := ncStuj;
   Self.NavStav.navest_old := ncStuj;
  end;

 Self.NavStav.toRnz.Clear();
 Self.UnregisterAllEvents();
 Self.Change();
end;

procedure TBlkNav.Disable();
begin
 Self.NavStav.Navest := ncDisabled;
 Self.NavStav.navest_old := ncDisabled;
 Self.NavStav.ZacatekVolba := TBlkNavVolba.none;
 Self.AB := false;
 Self.NavStav.ZAM  := false;
 Self.NavStav.toRnz.Clear();
 Self.NavStav.RCtimer := -1;
 Self.UnregisterAllEvents();
 Self.Change(true);
end;

function TBlkNav.UsesRCS(addr: TRCSAddr; portType: TRCSIOType): Boolean;
begin
 Result := ((portType = TRCSIOType.output) and (Self.NavSettings.RCSAddrs.Contains(addr)));
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkNav.Update();
begin
 Self.UpdateNavestSet();
 Self.UpdatePadani();
 Self.UpdateRychlostTrain();

 if (Self.Navest = ncPrivol) then
   Self.UpdatePrivol();

 if (Self.NavSettings.RCSAddrs.Count > 0) then
  begin
   if (RCSi.IsNonFailedModule(Self.NavSettings.RCSAddrs[0].board)) then
    begin
     if (Self.NavStav.Navest = ncDisabled) then
      begin
       Self.NavStav.Navest := ncStuj;
       Self.Change();
      end;
    end else begin
     if (Self.changing) then
       Self.OnNavestSetError();

     if (Self.NavStav.Navest >= ncStuj) then
      begin
       Self.NavStav.Navest := ncDisabled;
       JCDb.RusJC(Self);
       Self.Change();
      end;
    end;
  end;

 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkNav.Change(now:boolean = false);
begin
 // zmenu navesti propagujeme do prilehle trati, kde by mohlo dojit ke zmene
 // navesti autobloku
 if ((Self.UsekPred <> nil) and (Self.UsekPred.typ = btTU) and (TBlkTU(Self.UsekPred).InTrat > -1)) then
   Self.UsekPred.Change();

 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkNav.GetSettings():TBlkNavSettings;
begin
 Result := Self.NavSettings;
end;

procedure TBlkNav.SetSettings(data:TBlkNavSettings);
begin
 if (Self.NavSettings.events <> data.events) then
   Self.NavSettings.events.Free();

 if (Self.NavSettings.RCSAddrs <> data.RCSAddrs) then
   Self.NavSettings.RCSAddrs.Free();

 Self.NavSettings := data;
 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////
//nastavovani stavovych promennych:

procedure TBlkNav.SetNavest(navest: TBlkNavCode; changeCallbackOk, changeCallbackErr: TNotifyEvent);
var oblr:TOR;
    traini: Integer;
begin
 if ((Self.NavStav.Navest = ncDisabled) or (Self.NavSettings.zamknuto)) then
  begin
   if (Assigned(changeCallbackErr)) then
     changeCallbackErr(Self);
   Exit();
  end;

 if ((navest = ncPrivol) or (navest = ncStuj)) then
  begin
   // prodlouzeni nebo zruseni privolavaci navesti -> zrusit odpocet v panelu
   if (Self.NavStav.privol_timer_id > 0) then
     for oblr in Self.ORsRef do
      begin
       oblr.BroadcastGlobalData('INFO-TIMER-RM;'+IntToStr(Self.NavStav.privol_timer_id));
       oblr.TimerCnt := oblr.TimerCnt - 1;
      end;
   Self.NavStav.privol_timer_id := 0;
  end;

 if (navest = ncPrivol) then
   Self.NavStav.privol_start := Now;

 if ((Self.NavStav.Navest = navest) or ((Self.changing) and (Self.NavStav.cilova_navest = navest))) then
  begin
   if (Assigned(changeCallbackOk)) then
     changeCallbackOk(Self);
   Exit();
  end;

 // nastaveni vystupu
 try
   if (Self.NavSettings.RCSAddrs.Count > 0) then
    begin
     if (Self.NavSettings.OutputType = scom) then
      begin
       //scom
       RCSi.SetOutputs(Self.NavSettings.RCSAddrs, Integer(navest));
      end else begin
       //binary
       case (navest) of
        ncStuj, ncVse, ncPrivol, ncZhasnuto:
            RCSi.SetOutputs(Self.NavSettings.RCSAddrs, 0);
       else
        RCSi.SetOutputs(Self.NavSettings.RCSAddrs, 1);
       end;
      end;//else
    end;
 except
   if (Assigned(changeCallbackErr)) then
     changeCallbackErr(Self);
   Exit();
 end;

 // ruseni nouzove jizdni cesty pri padu navestidla do STUJ
 if (navest = ncStuj) then
  begin
   if ((Self.UsekPred <> nil) and ((Self.UsekPred.typ = btUsek) or
       (Self.UsekPred.typ = btTU)) and ((Self.UsekPred as TBlkUsek).NavJCRef.Contains(Self))) then
    (Self.UsekPred as TBlkUsek).NavJCRef.Remove(Self);

   if (Assigned(Self.privol)) then
    begin
     Self.privol.RusJCWithoutBlk();
     Self.privol := nil;
    end;
  end;

 if ((Self.Navest = ncPrivol) and (navest = ncStuj)) then
  begin
   // STUJ po privolavacce -> vypnout zvukovou vyzvu
   for oblr in Self.ORsRef do
     oblr.PrivolavackaBlkCnt := oblr.PrivolavackaBlkCnt - 1;
  end;

 if (not Self.changing) then
   Self.NavStav.navest_old := Self.Navest;
 Self.NavStav.changeCallbackOk := changeCallbackOk;
 Self.NavStav.changeCallbackErr := changeCallbackErr;
 Self.NavStav.Navest := ncChanging;
 Self.NavStav.cilova_navest := navest;

 if (Self.NavSettings.RCSAddrs.Count > 0) then
   Self.NavStav.changeEnd := Now + EncodeTime(0, 0, _NAV_CHANGE_DELAY_MSEC div 1000, _NAV_CHANGE_DELAY_MSEC mod 1000)
 else
   Self.NavStav.changeEnd := Now + EncodeTime(0, 0, _NAV_CHANGE_SHORT_DELAY_MSEC div 1000, _NAV_CHANGE_SHORT_DELAY_MSEC mod 1000);

 if (not TBlkNav.IsPovolovaciNavest(Self.NavStav.cilova_navest)) then // zastavujeme ihned
   Self.UpdateRychlostTrain(true);

 if (Self.UsekPred <> nil) then
  begin
   for traini in TBlkUsek(Self.UsekPred).trains do
     Trains[traini].OnPredictedSignalChange();
   if (TBlkUsek(Self.UsekPred).trainPredict <> nil) then
     TBlkUsek(Self.UsekPred).trainPredict.OnPredictedSignalChange();
  end;

 Self.Change();
end;

procedure TBlkNav.mSetNavest(navest: TBlkNavCode);
begin
 Self.SetNavest(navest, TNotifyEvent(nil), TNotifyEvent(nil));
end;

procedure TBlkNav.OnNavestSetOk();
var tmp:TNotifyEvent;
    oblr:TOR;
begin
 Self.NavStav.Navest := Self.NavStav.cilova_navest;

 if (Self.NavStav.cilova_navest = ncPrivol) then
  begin
   // nova navest je privolavacka -> zapnout zvukovou vyzvu
   for oblr in Self.ORsRef do
     oblr.PrivolavackaBlkCnt := oblr.PrivolavackaBlkCnt + 1;
  end;

 if (Self.autoblok) then
  begin
   if (TBlkTU(Self.UsekPred).nextTU <> nil) then TBlkTU(Self.UsekPred).nextTU.Change();
   if (TBlkTU(Self.UsekPred).Trat <> nil) then TBlkTrat(TBlkTU(Self.UsekPred).Trat).UpdateTrainPredict();
  end;

 if (Assigned(Self.NavStav.changeCallbackOk)) then
  begin
   tmp := Self.NavStav.changeCallbackOk; // may set new callback in event
   Self.NavStav.changeCallbackOk := nil;
   tmp(Self);
  end;

 Self.UpdateRychlostTrain(true);
 JCDb.CheckNNavaznost(Self);
 Self.Change();
end;

procedure TBlkNav.OnNavestSetError();
var tmp:TNotifyEvent;
begin
 // Tato funkce zatim neni moc vyuzivana, jedna se o pripravu do budoucna, kdy
 // by melo navestidlo kontrolu navesti (vstup do hJOP).

 if (Assigned(Self.NavStav.changeCallbackErr)) then
  begin
   tmp := Self.NavStav.changeCallbackErr; // may set new callback in event
   Self.NavStav.changeCallbackErr := nil;
   tmp(Self);
  end;

 // Jak nastavit aktualni navest?
 Self.NavStav.Navest := ncStuj;

 Self.Change();
end;

procedure TBlkNav.UpdateNavestSet();
begin
 if (not Self.changing) then Exit();
 if (Self.NavStav.changeEnd <= Now) then
   Self.OnNavestSetOk();
end;

function TBlkNav.GetAB():boolean;
begin
 Result := (Self.NavStav.ABJC <> nil);
end;

procedure TBlkNav.SetABJC(ab:TJC);
begin
 if ((ab <> nil) and (Self.ABJC <> nil)) then
   raise EInvalidOperation.Create('Cannot change AB JC, can only enable/disable AB JC!');

 if ((Self.ABJC <> nil) and (ab = nil)) then begin
   try
     if (Assigned(ABlist)) then
       ABlist.Remove(Self.ABJC);
   except
     on E:EABJCNotInList do
       asm nop; end; // ignore exception
   end;
   Self.NavStav.ABJC := nil;
   Self.Change();
 end else if ((Self.ABJC = nil) and (ab <> nil)) then begin
   try
     ABlist.Add(ab);
   except
     on E:EABJCAlreadyInList do
       asm nop; end; // ignore exception
   end;
   Self.NavStav.ABJC := ab;
   Self.Change();
 end;
end;

procedure TBlkNav.SetAB(ab:boolean);
begin
 if (ab) then
   raise EInvalidOperation.Create('You can only enable AB via SetABJC!');

 if (Self.AB and (not ab)) then
   Self.ABJC := nil;
end;

procedure TBlkNav.SetZacatekVolba(typ:TBlkNavVolba);
begin
 if (Self.NavStav.ZacatekVolba = typ) then Exit();
 Self.NavStav.ZacatekVolba := typ;
 if (typ = TBlkNavVolba.none) then
   Self.NavStav.ZacatekAB := false;
 Self.Change();
end;

procedure TBlkNav.SetZAM(zam:boolean);
begin
 if (Self.NavStav.ZAM = zam) then Exit();
 Self.NavStav.ZAM := zam;

 if (Self.DNjc <> nil) then
  begin
   if (zam) then
    begin
     if (Self.DNjc.RozpadBlok <= 0) then
      begin
       Self.DNjc.RozpadBlok := -2;
       Self.DNjc.STUJ();
      end;
    end else begin
     if ((Self.DNjc.RozpadBlok = -2) and (not Self.RCinProgress())) then
      begin
       Self.DNjc.RozpadBlok := -1;
       Self.DNjc.DN();
      end;
    end;
   Blky.TrainPrediction(Self);
  end;

 if ((Self.autoblok) and (not zam) and (Self.UsekPred <> nil) and (TBlkTU(Self.UsekPred).Trat <> nil)) then
   TBlkTrat(TBlkTU(Self.UsekPred).Trat).ChangeUseky();

 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////
//gui: menu
//dynamicke funkce

procedure TBlkNav.MenuVCStartClick(SenderPnl:TIdContext; SenderOR:TObject);
var Blk:TBlk;
begin
 if (Self.NavRel.SymbolType = TBlkNavSymbol.seradovaci) then Exit();
 if ((SenderOR as TOR).stack.volba = PV) then
   if (((Self.DNjc <> nil) and (Self.DNjc.RozpadRuseniBlok < 1)) or
       (JCDb.FindOnlyStaveniJC(Self.id) <> nil)) then Exit;

 Blk := Blky.GeTBlkNavZacatekVolba((SenderOR as TOR).id);
 if (Blk <> nil) then
  begin
   (Blk as TBlkNav).ZacatekVolba := TBlkNavVolba.none;
   TOR(SenderOR).ClearVb(); // smazeme dosavadni seznam variantnich bodu
  end;
 Self.ZacatekVolba := TBlkNavVolba.VC;
 Self.NavStav.ZacatekAB := false;
end;

procedure TBlkNav.MenuVCStopClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 Self.ZacatekVolba := TBlkNavVolba.none;
end;

procedure TBlkNav.MenuPCStartClick(SenderPnl:TIdContext; SenderOR:TObject);
var Blk:TBlk;
begin
 if ((SenderOR as TOR).stack.volba = PV) then
   if (((Self.DNjc <> nil) and (Self.DNjc.RozpadRuseniBlok < 1)) or
       (JCDb.FindOnlyStaveniJC(Self.id) <> nil)) then Exit;

 Blk := BLky.GeTBlkNavZacatekVolba((SenderOR as TOR).id);
 if (Blk <> nil) then (Blk as TBlkNav).ZacatekVolba := TBlkNavVolba.none;
 Self.ZacatekVolba := TBlkNavVolba.PC;
 Self.NavStav.ZacatekAB := false;
end;

procedure TBlkNav.MenuPCStopClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 Self.ZacatekVolba := TBlkNavVolba.none;
end;

procedure TBlkNav.MenuSTUJClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 // poradi musi byt zachovano !
 Self.Navest := ncStuj;
 if (Self.DNjc = nil) then Exit();

 Self.DNjc.STUJ();
 Blky.TrainPrediction(Self);
end;

procedure TBlkNav.MenuDNClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 if (Self.DNjc = nil) then Exit();

 if (Self.RCinProgress()) then
  begin
   TOR(SenderOR).StopMereniCasu(Self.NavStav.RCtimer);
   Self.NavStav.RCtimer := -1;
  end;

 Self.DNjc.DN();
 Blky.TrainPrediction(Self);
end;

procedure TBlkNav.MenuRCClick(SenderPnl:TIdContext; SenderOR:TObject);
var JC:TJC;
    Blk:TBlk;
begin
 if ((Self.DNjc = nil) or (Self.RCinProgress())) then Exit;

 JC := Self.DNjc;

 Blk := Self.UsekPred;
 if ((Blk = nil) or ((Blk.typ <> btUsek) and (Blk.typ <> btTU))) then
  begin
   // pokud blok pred JC neni -> 30 sekund
   Self.NavStav.RCtimer := (SenderOR as TOR).AddMereniCasu(JC.RusJC, EncodeTime(0, 0, 30, 0));
  end else begin
   if ((Blk as TBlkUsek).Obsazeno = TUsekStav.uvolneno) then
    begin
     // pokud neni blok pred JC obsazen -> 2 sekundy
     Self.NavStav.RCtimer := (SenderOR as TOR).AddMereniCasu(JC.RusJC, EncodeTime(0, 0, 2, 0));
    end else begin
     // pokud je obsazen, zalezi na typu jizdni cesty
     case (JC.data.TypCesty) of
      TJCType.vlak  : Self.NavStav.RCtimer := (SenderOR as TOR).AddMereniCasu(JC.RusJC, EncodeTime(0, 0, 15, 0));   // vlakova cesta : 20 sekund
      TJCType.posun : Self.NavStav.RCtimer := (SenderOR as TOR).AddMereniCasu(JC.RusJC, EncodeTime(0, 0,  5, 0));   // posunova cesta: 10 sekund
     else
      Self.NavStav.RCtimer := (SenderOR as TOR).AddMereniCasu(JC.RusJC, EncodeTime(0, 1, 0, 0));                   // nejaka divna cesta: 1 minuta
     end;
    end;
  end;

 Self.AB := false;
 JC.RusJCWithoutBlk();
 Blky.TrainPrediction(Self);
end;

procedure TBlkNav.MenuABStartClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 if (Self.DNjc <> nil) then
   Self.ABJC := Self.DNjc
 else begin
   Self.MenuVCStartClick(SenderPnl, SenderOR);
   Self.NavStav.ZacatekAB := true;
 end;
end;

procedure TBlkNav.MenuABStopClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 if (Self.ABJC <> nil) then
   Self.ABJC := nil
 else
   Self.MenuVCStopClick(SenderPnl, SenderOR);
end;

procedure TBlkNav.MenuLockClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 Self.ZAM := true;
 Self.Navest := ncStuj;
end;

procedure TBlkNav.MenuUnlockClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 Self.ZAM := false;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkNav.MenuPNStartClick(SenderPnl:TIdContext; SenderOR:TObject);
var Blk:TBlk;
    oblr:TOR;
begin
 if (Self.NavRel.SymbolType = TBlkNavSymbol.seradovaci) then Exit;

 Blk := Blky.GeTBlkNavZacatekVolba((SenderOR as TOR).id);
 if (Blk <> nil) then (Blk as TBlkNav).ZacatekVolba := TBlkNavVolba.none;
 Self.ZacatekVolba := TBlkNavVolba.NC;

 for oblr in Self.OblsRizeni do
   oblr.ORDKClickServer(Self.PrivolDKClick);
end;

procedure TBlkNav.MenuPNStopClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 Self.ZacatekVolba := TBlkNavVolba.none;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkNav.MenuPPStartClick(SenderPnl:TIdContext; SenderOR:TObject);
var Blk:TBlk;
begin
 if ((SenderOR as TOR).stack.volba = PV) then
   if ((Self.Navest > ncStuj) or (JCDb.FindJC(Self.id, false) <> nil)) then Exit;

 Blk := Blky.GeTBlkNavZacatekVolba((SenderOR as TOR).id);
 if (Blk <> nil) then (Blk as TBlkNav).ZacatekVolba := TBlkNavVolba.none;
 Self.ZacatekVolba := TBlkNavVolba.PP;
end;

procedure TBlkNav.MenuPPStopClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 Self.ZacatekVolba := TBlkNavVolba.none;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkNav.MenuPPNClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 ORTCPServer.Potvr(SenderPnl, Self.PrivokDKPotvrSekv, SenderOR as TOR, 'Prodloužení doby přivolávací návěsti', TBlky.GetBlksList(Self), nil);
end;

procedure TBlkNav.MenuRNZClick(SenderPnl:TIdContext; SenderOR:TObject);
var podminky:TList<TPSPodminka>;
    blkId:Integer;
    blk:TBlk;
begin
 podminky := TList<TPSPodminka>.Create();

 for blkId in Self.NavStav.toRnz.Keys do
  begin
   Blky.GetBlkByID(blkId, Blk);
   if (blk <> nil) then
     podminky.Add(TOR.GetPSPodminka(blk, 'Rušení NZ'));
  end;

 ORTCPServer.Potvr(SenderPnl, Self.RNZPotvrSekv, SenderOR as TOR, 'Zrušení nouzových závěrů po nouzové cestě', TBlky.GetBlksList(Self), podminky);
end;

procedure TBlkNav.MenuKCDKClick(SenderPnl:TIdContext; SenderOR:TObject);
var oblr:TOR;
begin
 if (Self.ZacatekVolba = TBlkNavVolba.NC) then
  begin
   for oblr in Self.OblsRizeni do
     oblr.ORDKClickClient();
   ORTCPServer.Potvr(SenderPnl, Self.PrivokDKPotvrSekv, SenderOR as TOR, 'Zapnutí přivolávací návěsti',
                     TBlky.GetBlksList(Self), nil);
  end;
end;

procedure TBlkNav.MenuAdminStopIR(SenderPnl:TIdContext; SenderOR:TObject; enabled:boolean);
var Blk:TBlk;
begin
 try
   if (Self.NavSettings.events[0].zastaveni.typ = TRREvType.rrtIR) then
    begin
     Blky.GetBlkByID(Self.NavSettings.events[0].zastaveni.data.irId, Blk);
     if ((Blk = nil) or (Blk.typ <> btIR)) then Exit();
     if (enabled) then
       RCSi.SetInput(TBlkIR(Blk).GetSettings().RCSAddrs[0].board, TBlkIR(Blk).GetSettings().RCSAddrs[0].port, 1)
     else
       RCSi.SetInput(TBlkIR(Blk).GetSettings().RCSAddrs[0].board, TBlkIR(Blk).GetSettings().RCSAddrs[0].port, 0);
    end;
 except
   ORTCPServer.BottomError(SenderPnl, 'Nepodařilo se nastavit stav IR čidla!', TOR(SenderOR).ShortName, 'SIMULACE');
 end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkNav.PanelClick(SenderPnl:TIdCOntext; SenderOR:TObject; Button:TPanelButton; rights:TORCOntrolRights; params:string = '');
begin
 case (Button) of
  F2: ORTCPServer.Menu(SenderPnl, Self, (SenderOR as TOR), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));

  ENTER: begin
    if (((((Self.DNjc = nil) or (Self.DNjc.RozpadRuseniBlok >= 1)) and
           (JCDb.FindOnlyStaveniJC(Self.id) = nil) and (Self.Navest <> ncPrivol) and (JCDb.IsAnyVCAvailable(Self) and (Self.enabled)))
         or (TOR(SenderOR).stack.volba = VZ)) and (JCDb.IsAnyVC(Self))) then begin
      if ((not Self.NavSettings.zamknuto) and (not Self.autoblok)) then Self.MenuVCStartClick(SenderPnl, SenderOR);
    end else
      ORTCPServer.Menu(SenderPnl, Self, (SenderOR as TOR), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));
  end;

  F1: begin
    if (((((Self.DNjc = nil) or (Self.DNjc.RozpadRuseniBlok >= 1)) and
           (JCDb.FindOnlyStaveniJC(Self.id) = nil) and (Self.Navest <> ncPrivol) and (JCDb.IsAnyPCAvailable(Self)) and (Self.enabled))
         or ((SenderOR as TOR).stack.volba = VZ)) and (JCDb.IsAnyPC(Self))) then begin
      if ((not Self.NavSettings.zamknuto) and (not Self.autoblok)) then Self.MenuPCStartClick(SenderPnl, SenderOR);
    end else
      ORTCPServer.Menu(SenderPnl, Self, (SenderOR as TOR), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));
  end;
 end;//case
end;

////////////////////////////////////////////////////////////////////////////////

//toto se zavola pri kliku na jakoukoliv itemu menu tohoto bloku
procedure TBlkNav.PanelMenuClick(SenderPnl:TIdContext; SenderOR:TObject; item:string; itemindex:Integer);
begin
 if      (item = 'VC>')  then Self.MenuVCStartClick(SenderPnl, SenderOR)
 else if (item = 'VC<')  then Self.MenuVCStopClick (SenderPnl, SenderOR)
 else if (item = 'PC>')  then Self.MenuPCStartClick(SenderPnl, SenderOR)
 else if (item = 'PC<')  then Self.MenuPCStopClick (SenderPnl, SenderOR)
 else if (item = 'STUJ') then Self.MenuSTUJClick   (SenderPnl, SenderOR)
 else if (item = 'DN')   then Self.MenuDNClick     (SenderPnl, SenderOR)
 else if (item = 'RC')   then Self.MenuRCClick     (SenderPnl, SenderOR)
 else if (item = 'AB>')  then Self.MenuABStartClick(SenderPnl, SenderOR)
 else if (item = 'AB<')  then Self.MenuABStopClick (SenderPnl, SenderOR)
 else if (item = 'ZAM>') then Self.MenuLockClick   (SenderPnl, SenderOR)
 else if (item = 'ZAM<') then Self.MenuUnlockClick (SenderPnl, SenderOR)
 else if (item = 'PN>')  then Self.MenuPNStartClick(SenderPnl, SenderOR)
 else if (item = 'PN<')  then Self.MenuPNStopClick (SenderPnl, SenderOR)
 else if (item = 'PP>')  then Self.MenuPPStartClick(SenderPnl, SenderOR)
 else if (item = 'PP<')  then Self.MenuPPStopClick (SenderPnl, SenderOR)
 else if (item = 'PPN')  then Self.MenuPPNClick    (SenderPnl, SenderOR)
 else if (item = 'RNZ')  then Self.MenuRNZClick    (SenderPnl, SenderOR)
 else if (item = 'IR>')  then Self.MenuAdminStopIR (SenderPnl, SenderOR, true)
 else if (item = 'IR<')  then Self.MenuAdminStopIR (SenderPnl, SenderOR, false)
 else if (item = 'KC')   then Self.MenuKCDKClick   (SenderPnl, SenderOR);
end;

////////////////////////////////////////////////////////////////////////////////

//vytvoreni menu pro konkretni s-com:
function TBlkNav.ShowPanelMenu(SenderPnl:TIdContext; SenderOR:TObject; rights:TORCOntrolRights):string;
var Blk:TBlk;
begin
 Result := inherited;

 // pokud je navestidlo trvale zamkle, neumoznime zadne volby
 if (Self.NavSettings.zamknuto) then Exit();

 if (((((Self.DNjc = nil) or (Self.DNjc.RozpadRuseniBlok >= 1)) and
        (JCDb.FindOnlyStaveniJC(Self.id) = nil) and (Self.Navest <> ncPrivol) and (not Self.AB))
      or ((SenderOR as TOR).stack.volba = VZ)) and
     (not Self.autoblok)) then
  begin
    case (Self.NavStav.ZacatekVolba) of
     TBlkNavVolba.VC :
       if (Self.ZacatekAB) then
         Result := Result + 'AB<,'
       else
         Result := Result + 'VC<,';
     TBlkNavVolba.PC : Result := Result + 'PC<,';
     TBlkNavVolba.NC : Result := Result + 'PN<,';
     TBlkNavVolba.PP : Result := Result + 'PP<,';
    else
      //2 = VC, 3= PC
      if (Self.NavRel.SymbolType = TBlkNavSymbol.hlavni) then
       begin
        if (((JCDb.IsAnyVCAvailable(Self)) and (Self.enabled)) or ((SenderOR as TOR).stack.volba = VZ)) then // i kdyz neni zadna VC, schvalne umoznime PN
         begin
          Result := Result + 'VC>,';
          if (Self.DNjc = nil) then
            Result := Result + 'AB>,';
         end;
        Result := Result + '!PN>,';
       end;
      if (JCDb.IsAnyPC(Self)) then
       begin
        if (((JCDb.IsAnyPCAvailable(Self)) and (Self.enabled)) or ((SenderOR as TOR).stack.volba = VZ)) then
          Result := Result + 'PC>,';
        Result := Result + 'PP>,';
       end;
    end;// else ZacatekVolba <> none ...

    Result := Result + '-,';
  end;

 if ((Self.Navest > ncStuj) and (not Self.autoblok)) then
   Result := Result + 'STUJ,';

 if (Self.Navest = ncPrivol) then
   Result := Result + '!PPN,';

 if (Self.DNjc <> nil) then
  begin
   // bud je cesta primo postavena, nebo je zrusena, ale podminky jsou vyhovujici pro DN
   // plati jen pro postavenou JC
   if ((not Self.ZAM) and (Self.Navest = ncStuj) and (Self.DNjc.CanDN())) then
     Result := Result + 'DN,';

   if (((Self.Navest > ncStuj) or (Self.DNjc.CanDN()) or (Self.DNjc.RozpadBlok < 1))
       and (not Self.RCinProgress())) then
    begin
     Result := Result + 'RC,';

     // AB lze jen u vlakove cesty
     if ((Self.DNjc.data.TypCesty = TJCType.vlak) and (not Self.AB)) then
       Result := Result + 'AB>,';
    end;
 end;

 // AB lze jen u vlakove cesty
 if (Self.AB) then
   Result := Result + 'AB<,';

 if (Self.NavStav.ZAM) then
   Result := Result + 'ZAM<,'
 else
   Result := Result + 'ZAM>,';

 if ((Self.Navest <> ncPrivol) and (Self.CanIDoRNZ)) then
  Result := Result + '!RNZ,';

 // DEBUG: jednoduche nastaveni IR pri knihovne simulator
 if (RCSi.simulation) then
  begin
   if ((Self.NavSettings.events.Count > 0) and (Self.NavSettings.events[0].zastaveni.typ = TRREvType.rrtIR)) then
    begin
     Blky.GetBlkByID(Self.NavSettings.events[0].zastaveni.data.irId, Blk);
     if ((Blk <> nil) and (Blk.typ = btIR)) then
      begin
       case (TBlkIR(Blk).Stav) of
         TIRStav.uvolneno : Result := Result + '-,*IR>,';
         TIRStav.obsazeno : Result := Result + '-,*IR<,';
       end;//case
      end;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

class function TBlkNav.NavestToString(navest: TBlkNavCode):string;
begin
  case (navest) of
   ncChanging: Result  := 'stavění...';
   ncDisabled: Result  := 'disabled';
   ncStuj: Result  := 'stůj/posun zakázán';
   ncVolno: Result  := 'volno';
   ncVystraha: Result  := 'výstraha';
   ncOcek40: Result  := 'očekávejte 40 km/h';
   ncVolno40: Result  := '40 km/h a volno';
   ncVse: Result  := 'svítí vše (Rezerva)';
   ncVystraha40:Result  := '40 km/h a výstraha';
   nc40Ocek40: Result  := '40 km/h a očekávejte 40 km/h';
   ncPrivol: Result  := 'přivolávací návěst';
   ncPosunZaj: Result  := 'dovolen zajištěný posun';
   ncPosunNezaj: Result := 'dovolen nezajištěný posun';
   ncOpakVolno:Result := 'opakování návěsti volno';
   ncOpakVystraha:Result := 'opakování návěsti výstraha';
   ncZhasnuto:Result := 'návěstidlo zhaslé';
   ncOpakOcek40:Result := 'opakování návěsti očekávejte 40 km/h';
   ncOpakVystraha40:Result := 'opakování návěsti výstraha a 40 km/h';
   ncOpak40Ocek40:Result := '40 km/h a opakování návěsti očekávejte 40 km/h';
  else
    Result := 'Jiná návěst';
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkNav.JCZrusNavest();
begin
 if (Self.NavSettings.ZpozdeniPadu > 0) then
  begin
   Self.NavStav.padani       := true;
   Self.NavStav.padani_start := Now;
   writelog('Návěstidlo '+Self.GlobalSettings.name+': spoždění pádu '+IntToStr(Self.NavSettings.ZpozdeniPadu)+' s', WR_VC);
  end else begin
   Self.Navest := ncStuj;
  end;

 Self.UpdateRychlostTrain(true);
end;

procedure TBlkNav.UpdatePadani();
begin
 if (not Self.NavStav.padani) then Exit();

 if (Self.NavStav.padani_start + EncodeTime(0, Self.NavSettings.ZpozdeniPadu div 60, Self.NavSettings.ZpozdeniPadu mod 60, 0) < Now) then
  begin
   Self.Navest := ncStuj;
   Self.NavStav.padani := false;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

// aktualizace rychlosti souprav před návěstidlem
// pozor na padání !
// force nucene zastavi vlak, resp. nastavi jeho rychlost
//  metoda je volana s force v pripade, kdy dochazi k prime zmene navesti od uzivatele (STUJ, DN, RC)
procedure TBlkNav.UpdateRychlostTrain(force:boolean = false);
var Usek, Nav:TBlk;
    train:TTrain;
    navEv:TBlkNavTrainEvent;
    i:Integer;
    trat:TBlkTrat;
begin
 if (Self.NavSettings.events.Count = 0) then Exit();
 Usek := Self.UsekPred;
 if (Self.NavRel.SymbolType = TBlkNavSymbol.seradovaci) then Exit();          // pokud jsem posunove navestidlo, koncim funkci
 if ((Usek = nil) or ((Usek.typ <> btUsek) and (Usek.typ <> btTU))) then Exit();    // pokud pred navestidlem neni usek, koncim funkci

 // pokud na useku prede mnou neni souprava, koncim funkci
 if (not (Usek as TBlkUsek).IsTrain()) then
  begin
   // tady musi dojit ke zruseni registrace eventu, kdyby nedoslo, muze se stat,
   // ze za nejakou dobu budou splneny podminky, pro overovani eventu, ale
   // event bude porad bezet -> pokud je casovy, okamzite byse spustil
   if ((Self.lastEvIndex >= 0) and (Self.lastEvIndex < Self.NavSettings.events.Count)) then
     if (Self.NavSettings.events[Self.lastEvIndex].zastaveni.enabled) then
       Self.NavSettings.events[Self.lastEvIndex].zastaveni.Unregister();
   Exit();
  end;

 // pokud souprava svym predkem neni na bloku pred navestidlem, koncim funkci
 train := Self.GetTrain(Usek);
 if (train.front <> Usek) then
  begin
   // tady musime zrusit registraci eventu, viz vyse
   if ((Self.lastEvIndex >= 0) and (Self.lastEvIndex < Self.NavSettings.events.Count)) then
     if (Self.NavSettings.events[Self.lastEvIndex].zastaveni.enabled) then
       Self.NavSettings.events[Self.lastEvIndex].zastaveni.Unregister();
   Exit();
  end;

 if ((Usek.typ = btTU) and (TBlkTU(Usek).Trat <> nil)) then
  begin
   trat := TBlkTrat(TBlkTU(Usek).Trat);

   // Ignoruji krajni navestidla trati, ktera jsou proti smeru trati
   if ((trat.Smer = TTratSmer.AtoB) and (Self = trat.navLichy)) then
     Exit();
   if ((trat.Smer = TTratSmer.BtoA) and (Self = trat.navSudy)) then
     Exit();

   // Vsechna navestidla autobloku proti smeru trati se ignoruji (zejmena v kontetu zmeny smeru soupravy)
   if ((Self.autoblok) and (TBlkTrat(TBlkTU(Usek).Trat).Smer = TTratSmer.AtoB) and (Self.Smer = THVStanoviste.sudy)) then
     Exit();
   if ((Self.autoblok) and (TBlkTrat(TBlkTU(Usek).Trat).Smer = TTratSmer.BtoA) and (Self.Smer = THVStanoviste.lichy)) then
     Exit();
  end;

 // zjisteni aktualni udalosti podle typu a delky soupravy
 i := Self.CurrentEventIndex();
 navEv := Self.NavSettings.events[i];

 if (i <> Self.lastEvIndex) then
  begin
   // Z nejakeho duvodu reagujeme na novou udalost -> vypnout starou udalost
   if ((Self.lastEvIndex >= 0) and (Self.lastEvIndex < Self.NavSettings.events.Count)) then
    begin
     if (Self.NavSettings.events[Self.lastEvIndex].zastaveni.enabled) then
       Self.NavSettings.events[Self.lastEvIndex].zastaveni.Unregister();

     if ((Self.NavSettings.events[Self.lastEvIndex].zpomaleni.enabled) and
         (Self.NavSettings.events[Self.lastEvIndex].zpomaleni.ev.enabled)) then
       Self.NavSettings.events[Self.lastEvIndex].zpomaleni.ev.Unregister();
    end;

   Self.lastEvIndex := i;
  end;

 ///////////////////////////////////////////////////

 // ZPOMALOVANI
 if ((navEv.zpomaleni.enabled) and (train.wantedSpeed > navEv.zpomaleni.speed) and
     ((Usek as TBlkUsek).zpomalovani_ready) and
     ((not Self.IsPovolovaciNavest()) or (train.IsPOdj(Usek))) and
     (train.direction = Self.NavRel.smer)) then
  begin
   if (not navEv.zpomaleni.ev.enabled) then
     navEv.zpomaleni.ev.Register();

   if (navEv.zpomaleni.ev.IsTriggerred(Usek, true)) then
    begin
     navEv.zpomaleni.ev.Unregister();
     train.speed := navEv.zpomaleni.speed;
     (Usek as TBlkUsek).zpomalovani_ready := false;
    end;
  end else begin
   if ((navEv.zpomaleni.enabled) and (navEv.zpomaleni.ev.enabled)) then
     navEv.zpomaleni.ev.Unregister();
  end;

 ///////////////////////////////////////////////////

 // ZASTAVOVANI, resp. nastavovani rychlosti prislusne JC
 if (not navEv.zastaveni.enabled) then
   navEv.zastaveni.Register();

 if ((navEv.zastaveni.IsTriggerred(Usek, true)) or (force)) then       // podminka IsRychEvent take resi to, ze usek musi byt obsazeny (tudiz resi vypadek useku)
  begin
   // event se odregistruje automaticky pri zmene

   if ((train.IsPOdj(Usek)) and (train.direction = Self.NavRel.smer)) then
    begin
     // predvidany odjezd neuplynul -> zastavit soupravu
     if (train.wantedSpeed <> 0) then
       train.SetRychlostSmer(0, Self.NavRel.smer);

     // souprava je na zastavovaci udalosti -> zacit pocitat cas
     if (not train.GetPOdj(Usek).origin_set) then
      begin
       train.GetPOdj(Usek).RecordOriginNow();
       TBlkUsek(Usek).PropagatePOdjToTrat();
       Usek.Change();
      end;

     Exit();
    end;

   if ((Assigned(Self.DNjc)) and (Self.DNjc.data.TypCesty = TJCType.vlak)) then
    begin
     // je JC -> je postaveno?
     if ((Self.IsPovolovaciNavest()) and (not Self.NavStav.padani)) then
      begin
       // je postaveno -> zkontrolujeme, jestli budeme na konci zastavovat
       if ((train.wantedSpeed > 0) and (train.direction <> Self.NavRel.smer)) then Exit(); // pokud jede souprava opacnym smerem, kaslu na ni

       case (Self.DNjc.data.DalsiNavaznost) of
         TJCNextNavType.blok: begin
           Blky.GetBlkByID(Self.DNjc.data.DalsiNavestidlo, nav);

           if ((nav <> nil) and (nav.typ = btNav) and (TBlkNav(nav).IsPovolovaciNavest()) and
               (not train.IsPOdj(Self.DNjc.lastUsek))) then
            begin
              // na konci JC budeme stat
              if ((train.wantedSpeed <> Self.DNjc.data.RychlostDalsiN*10) or (train.direction <> Self.NavRel.smer)) then
                train.SetRychlostSmer(Self.DNjc.data.RychlostDalsiN*10, Self.NavRel.smer);
            end else begin
              // na konci JC jedeme dal
              if ((train.wantedSpeed <> Self.DNjc.data.RychlostNoDalsiN*10) or (train.direction <> Self.NavRel.smer)) then
                train.SetRychlostSmer(Self.DNjc.data.RychlostNoDalsiN*10, Self.NavRel.smer);
            end;
         end;

         TJCNextNavType.trat: begin
           if ((train.wantedSpeed <> Self.DNjc.data.RychlostDalsiN*10) or (train.direction <> Self.NavRel.smer)) then
             train.SetRychlostSmer(Self.DNjc.data.RychlostDalsiN*10, Self.NavRel.smer);
         end;

         TJCNextNavType.zadna: begin
           if ((train.wantedSpeed <> Self.DNjc.data.RychlostNoDalsiN*10) or (train.direction <> Self.NavRel.smer)) then
             train.SetRychlostSmer(Self.DNjc.data.RychlostNoDalsiN*10, Self.NavRel.smer);
          end;
       end;

       // kontrola prehravani stanicniho hlaseni
       train.CheckSh(Self);
      end else begin
       // neni povolovaci navest -> zastavit LOKO
       if ((train.direction = Self.NavRel.smer) and (train.wantedSpeed <> 0)) then
         train.SetRychlostSmer(0, Self.NavRel.smer);
      end;
    end else begin
     // nenalezena jizdni cesta -> muze se jednat o navestidlo v autobloku
     if (train.direction = Self.NavRel.smer) then
      begin
       if ((Self.IsPovolovaciNavest()) and (not Self.NavStav.padani) and
           (Self.UsekPred.typ = btTU) and (TBlkTU(Self.UsekPred).InTrat > -1)) then
        begin
         if (Cardinal(train.wantedSpeed) <> TBlkTU(Self.UsekPred).Speed(train)) then
           train.SetRychlostSmer(TBlkTU(Self.UsekPred).Speed(train), Self.NavRel.smer)
        end else begin
         //  neni povolovaci navest -> zastavit
         if (train.wantedSpeed <> 0) then
           train.SetRychlostSmer(0, Self.NavRel.smer);
        end;
      end;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// Vraci udalost, na kterou by se melo reagovat podle aktualniho stavu kolejiste.

function TBlkNav.CurrentEventIndex():Integer;
var i:Integer;
    train:TTrain;
    Usek:TBlk;
    event: TBlkNavTrainEvent;
begin
 if (Self.NavSettings.events.Count = 0) then
   raise ENoEvents.Create('No current events!');

 Usek := Self.UsekPred;
 if (not (Usek as TBlkUsek).IsTrain()) then
  begin
   // na bloku neni zadna souprava
   Result := 0;
  end else begin
   train := Self.GetTrain(Usek);

   // hledame takovy event, ktery odpovida nasi souprave
   for i := 0 to Self.NavSettings.events.Count-1 do
    begin
     event := Self.NavSettings.events[i];
     if ((train.length >= event.delka.min) and (train.length <= event.delka.max) and (event.train_typ_re.Match(train.typ))) then
       Exit(i);
    end;

   // pokud jsme event odpovidajici parametrum soupravy nenasli, vyhodnocujeme globalni event
   Result := 0;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkNav.IsPovolovaciNavest(jctype:TJCType = TJCType.vlak):boolean;
begin
 if ((Self.Navest = ncChanging) and (TBlkNav.IsPovolovaciNavest(Self.NavStav.cilova_navest, jctype))) then
   // navest se meni na nejakou povolovaci -> ridim se jeste tou starou
   Result := TBlkNav.IsPovolovaciNavest(Self.NavStav.navest_old, jctype)
 else
   Result := TBlkNav.IsPovolovaciNavest(Self.Navest, jctype);
end;

function TBlkNav.IsOpakVystraha(): Boolean;
begin
 Result := (Self.cilovaNavest = ncOpakVystraha) or (Self.cilovaNavest = ncOpakVystraha40);
end;

class function TBlkNav.IsPovolovaciNavest(Navest: TBlkNavCode; jctype:TJCType = TJCType.vlak):boolean;
begin
 if (jcType = TJCType.vlak) then
  begin
   case (navest) of
     ncVolno, ncVystraha, ncOcek40, ncVolno40, ncVystraha40, nc40Ocek40,
     ncOpakVolno, ncOpakVystraha, ncOpakOcek40, ncOpakVystraha40, ncOpak40Ocek40: Result := true;
   else
    Result := false;
   end;
  end else if (jcType = TJCType.posun) then
    Result := (navest = ncPosunZaj) or (navest = ncPosunNezaj)
  else
    Result := false;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkNav.UpdatePrivol();
var oblr:TOR;
begin
 if ((Self.NavStav.privol_start+EncodeTime(0, _PRIVOL_MIN, _PRIVOL_SEC, 0) < Now+EncodeTime(0, 0, 30, 0)) and
     (Self.NavStav.privol_timer_id = 0)) then
  begin
   // oznameni o brzkem ukonceni privolavaci navesti
   Self.NavStav.privol_timer_id := Random(65536)+1;
   for oblr in Self.ORsRef do
    begin
     oblr.BroadcastGlobalData('INFO-TIMER;'+IntToStr(Self.NavStav.privol_timer_id)+
                              ';0;30;PN '+Self.GlobalSettings.name);
     oblr.TimerCnt := oblr.TimerCnt + 1;
    end;
  end;

 if (Self.NavStav.privol_start+EncodeTime(0, _PRIVOL_MIN, _PRIVOL_SEC, 0) < Now) then
  begin
   // pad privolavaci navesti
   Self.Navest := ncStuj;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

// privolavaci navest bez podpory zabezpecovaciho zarizeni
procedure TBlkNav.PrivolDKClick(SenderPnl:TIDContext; SenderOR:TObject; Button:TPanelButton);
var oblr:TOR;
begin
 if (Button = ENTER) then
  begin
   for oblr in Self.OblsRizeni do
     oblr.ORDKClickClient();
   ORTCPServer.Potvr(SenderPnl, Self.PrivokDKPotvrSekv, SenderOR as TOR, 'Zapnutí přivolávací návěsti',
                     TBlky.GetBlksList(Self), nil);
  end else begin
   if (Button = TPanelButton.F2) then
     ORTCPServer.Menu(SenderPnl, Self, TOR(SenderOR), '$'+TOR(SenderOR).Name + ',-,' + 'KC');
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkNav.PrivokDKPotvrSekv(Sender:TIdContext; success:boolean);
begin
 if (success) then
  begin
   Self.NavStav.ZacatekVolba := TBlkNavVolba.none;
   Self.Navest := ncPrivol;
  end else begin
   self.ZacatekVolba := TBlkNavVolba.none;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkNav.RNZPotvrSekv(Sender:TIdContext; success:boolean);
var blkId:Integer;
    blk:TBlk;
    toRNZ:TDictionary<Integer, Cardinal>;
begin
 if (not success) then Exit();

 // nejdriv uvolnime toRNZ -- abychom jej nemazali v DecreaseNouzZaver
 toRNZ := Self.NavStav.toRnz;
 Self.NavStav.toRnz := TDictionary<Integer, Cardinal>.Create();

 for blkId in toRNZ.Keys do
  begin
   Blky.GetBlkByID(blkId, blk);
   if (blk = nil) then continue;   

   case (blk.typ) of
    btVyhybka: begin
       if (TBlkVyhybka(blk).vyhZaver) then
         TBlkVyhybka(blk).DecreaseNouzZaver(toRnz[blkId]);
    end;

    btZamek: begin
       if (TBlkZamek(blk).nouzZaver) then
         TBlkZamek(blk).DecreaseNouzZaver(toRnz[blkId]);
    end;
   end;
  end;

 toRNZ.Free();
 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkNav.SetUsekPredID(new_id:Integer);
begin
 if (Self.NavRel.UsekID = new_id) then Exit();
 Self.NavRel.UsekID := new_id;
 if (new_id = -1) then Self.autoblok := false;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkNav.GetUsekPred():TBlk;
begin
 if (((Self.fUsekPred = nil) and (Self.UsekID <> -1)) or ((Self.fUsekPred <> nil) and (Self.UsekID <> Self.fUsekPred.id))) then
   Blky.GetBlkByID(Self.UsekID, Self.fUsekPred);
 Result := Self.fUsekPred;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkNav.AddBlkToRnz(blkId:Integer; change:boolean = true);
begin
 if (Self.NavStav.toRnz.ContainsKey(blkId)) then
   Self.NavStav.toRnz[blkId] := Self.NavStav.toRnz[blkId] + 1
 else
   Self.NavStav.toRnz.Add(blkId, 1);

 if ((Self.NavStav.toRnz.Count = 1) and (Self.NavStav.toRnz[blkId] = 1) and
     (change)) then
   Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkNav.CanIDoRNZ():boolean;
begin
 Result := Self.NavStav.toRnz.Count > 0;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkNav.RemoveBlkFromRnz(blkId:Integer);
begin
 if (Self.NavStav.toRnz.ContainsKey(blkId)) then
  begin
   Self.NavStav.toRnz.Remove(blkId);
   if (Self.NavStav.toRnz.Count = 0) then
     Self.Change();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkNav.UnregisterAllEvents();
var ev:TBlkNavTrainEvent;
begin
 for ev in Self.NavSettings.events do
  begin
   ev.zastaveni.Unregister();
   if ((ev.zpomaleni.enabled) and (Assigned(ev.zpomaleni.ev))) then
     ev.zpomaleni.ev.Unregister();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkNav.GetPtData(json:TJsonObject; includeState:boolean);
begin
 inherited;

 if (includeState) then
   Self.GetPtState(json['blokStav']);
end;

procedure TBlkNav.GetPtState(json:TJsonObject);
begin
 json['navest'] := Self.Navest;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkNav.GetTrain(usek:TBlk = nil): TTrain;
begin
 if (usek = nil) then
   Blky.GetBlkByID(Self.UsekID, usek);

 if (Self.Smer = THVStanoviste.lichy) then
   Result := TBlkUsek(usek).trainSudy
 else
   Result := TBlkUsek(usek).trainL;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkNav.RCinProgress():boolean;
begin
 Result := (Self.NavStav.RCtimer > -1);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkNav.RCtimerTimeout();
begin
 Self.NavStav.RCtimer := -1;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkNav.PropagatePOdjToTrat();
var train: TTrain;
    trat:TBlk;
    podj:TPOdj;
begin
 train := Self.GetTrain();
 if (train = nil) then
  begin
   train := TBlkUsek(Self.UsekPred).trainPredict;
   if (train = nil) then Exit();
  end;

 if (Self.DNjc = nil) then Exit();
 if (Self.DNjc.data.Trat = -1) then Exit();
 Blky.GetBlkByID(Self.DNjc.data.Trat, trat);
 if (TBlkTrat(trat).trainPredict = nil) then Exit();
 if (TBlkTrat(trat).trainPredict.train <> train) then Exit();

 if (train.IsPOdj(Self.UsekPred)) then
  begin
   podj := train.GetPOdj(Self.UsekPred);
   if (not podj.IsDepSet) then Exit();
   if ((TBlkTrat(trat).trainPredict.IsTimeDefined) and (TBlkTrat(trat).trainPredict.time = podj.DepTime())) then Exit();

   TBlkTrat(trat).trainPredict.predict := true;
   TBlkTrat(trat).trainPredict.time := podj.DepTime();
   TBlkTrat(trat).Change();
  end else if ((TBlkTrat(trat).trainPredict.predict) and
               ((not train.IsPOdj(Self.UsekPred)) or (not train.GetPOdj(Self.UsekPred).IsDepSet()))) then begin
   TBlkTrat(trat).trainPredict.predict := false;
   TBlkTrat(trat).trainPredict.UndefTime();
   TBlkTrat(trat).Change();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkNav.IsChanging():boolean;
begin
 Result := (Self.Navest = ncChanging);
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkNav.PanelStateString():string;
var fg, bg, okoli: TColor;
begin
 Result := inherited;

 case (Self.ZacatekVolba) of
  TBlkNavVolba.none : okoli := clBlack;
  TBlkNavVolba.VC   : okoli := clGreen;
  TBlkNavVolba.PC   : okoli := clWhite;
  TBlkNavVolba.NC,
  TBlkNavVolba.PP   : okoli := clTeal;
 else
  okoli := clBlack;
 end;

 bg := okoli;
 fg := $A0A0A0;

 if (Self.ZAM) then
   case (Self.SymbolType) of
    TBlkNavSymbol.hlavni : fg := clRed;
    TBlkNavSymbol.seradovaci : fg := clBlue;
   end;
 if (Self.canRNZ) then
   fg := clTeal;

 case (Self.Navest) of
  ncStuj: begin end;
  ncChanging, ncZhasnuto: begin
    fg := clBlack;
    bg := $A0A0A0;
  end;
  ncVolno, ncVystraha, ncOcek40, ncVolno40, ncVystraha40, nc40Ocek40,
  ncOpakVolno, ncOpakVystraha, ncOpakOcek40, ncOpakVystraha40, ncOpak40Ocek40: fg := clLime;
  ncPrivol, ncPosunZaj, ncPosunNezaj: fg := clWhite;
  ncVse: fg := clYellow;
 else
  if (fg = $A0A0A0) then
    fg := clBlack;
  bg := clFuchsia;
 end;

 Result := Result + ownConvert.ColorToStr(fg) + ';' +
                    ownConvert.ColorToStr(bg) + ';' +
                    IntToStr(ownConvert.BoolToInt(Self.Navest = ncPrivol)) + ';' +
                    IntToStr(ownConvert.BoolToInt(Self.AB)) + ';' +
                    ownConvert.ColorToStr(okoli);
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkNav.FourtyKmph(): Boolean;
begin
 Result := (Self.cilovaNavest = ncVolno40) or (Self.cilovaNavest = ncVystraha40) or
           (Self.cilovaNavest = nc40Ocek40) or (Self.cilovaNavest = ncOpakVystraha40) or
           (Self.cilovaNavest = ncOpak40Ocek40);
end;

class function TBlkNav.AddOpak(navest: TBlkNavCode): TBlkNavCode;
begin
 case (navest) of
  ncVolno: Result := ncOpakVolno;
  ncVystraha: Result := ncOpakVystraha;
  ncOcek40: Result := ncOpakOcek40;
  ncVystraha40: Result := ncOpakVystraha40;
  nc40Ocek40: Result := ncOpak40Ocek40;
 else
  Result := navest;
 end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkNav.GetCilovaNavest(): TBlkNavCode;
begin
 if (Self.changing) then
   Result := Self.NavStav.cilova_navest
 else
   Result := Self.NavStav.Navest;
end;

////////////////////////////////////////////////////////////////////////////////

constructor TBlkNavTrainEvent.Create();
begin
 inherited;

 Self.train_typ_re := TJclRegEx.Create();
 Self.train_typ_re.Compile('.*', false);
 Self.zastaveni := nil;
 Self.zpomaleni.ev := nil;
end;

constructor TBlkNavTrainEvent.Create(str: string; old: boolean);
begin
 Self.Create();
 Self.Parse(str, old);
end;

destructor TBlkNavTrainEvent.Destroy();
begin
 Self.train_typ_re.Free();
 if (Assigned(Self.zastaveni)) then
   Self.zastaveni.Free();
 if (Assigned(Self.zpomaleni.ev)) then
   Self.zpomaleni.ev.Free();

 inherited;
end;

// format ev1: RychEvent-zastaveni|RychEvent-zpomaleni|re:train_typ_regexp|min_delka|max_delka
procedure TBlkNavTrainEvent.Parse(str:string; old:boolean);
var sl, sl2:TStrings;
begin
 sl := TStringList.Create();
 sl2 := TStringList.Create();
 ExtractStringsEx(['|'], [], PChar(str), sl);

 Self.train_typ_re.Compile('.*', false);
 try
   Self.delka.min := -1;
   Self.delka.max := -1;

   if (old) then
     Self.zastaveni := Self.ParseOldRychEvent(sl[0])
   else
     Self.zastaveni := TRREv.Create(sl[0]);

   if ((sl.Count > 1) and (sl[1] <> '') and (LeftStr(sl[1], 2) <> '-1')) then
    begin
     ExtractStringsEx([';'], [], sl[1], sl2);

     Self.zpomaleni.enabled := true;
     if (old) then
       Self.zpomaleni.ev := Self.ParseOldRychEvent(sl[1])
     else
       Self.zpomaleni.ev := TRREv.Create(sl2[0]);

     Self.zpomaleni.speed := StrToInt(sl2[sl2.Count-1]);
    end else begin
     Self.zpomaleni.enabled := false;
     Self.zpomaleni.ev := nil;
    end;

   if (sl.Count > 2) then
    begin
     Self.train_typ_re.Compile(ParseTrainTypes(sl[2]), false);
     Self.delka.min := StrToIntDef(sl[3], -1);
     Self.delka.max := StrToIntDef(sl[4], -1);
    end;
 finally
   sl.Free();
   sl2.Free();
 end;
end;

class function TBlkNavTrainEvent.ParseTrainTypes(types: string): string;
var strs: TSTrings;
    str: string;
begin
 if ((types = '') or (types = ';')) then
   Result := '^.*$'
 else if (StartsText('re:', types)) then
  begin
   if (types = 're:') then
     Result := '^.*$'
   else
     Result := RightStr(types, Length(types)-3);
  end else begin
   // backward-compatibility: manually create regexp
   Result := '^(';
   strs := TStringList.Create();
   try
     ExtractStringsEx([';'], [' '], types, strs);
     for str in strs do
       Result := Result + str + '|';
   finally
     strs.Free();
   end;
   Result[Length(Result)] := ')';
   Result := Result + '$';
 end;
end;

function TBlkNavTrainEvent.ToFileStr(short:boolean = false): string;
begin
 Result := '{' + Self.zastaveni.GetDefStr() + '}|';

 if (Self.zpomaleni.enabled) then
   Result := Result + '{{' + Self.zpomaleni.ev.GetDefStr() + '};' +
              IntToStr(Self.zpomaleni.speed) + '}';
 Result := Result + '|';

 if (not short) then
  begin
   Result := Result + '{re:' + Self.train_typ_re.Pattern + '}|' +
             IntToStr(Self.delka.min) + '|' +
             IntToStr(Self.delka.max);
  end;
end;

//ziskavani zpomalovacich a zastavovaich dat ze souboru (parsing dat)
//format RychEvent data: textove ulozeny 1 radek, kde jsou data oddelena ";"
// : typ_zastaveni(0=usek;1=ir);
//    pro usek nasleduje: usekid;usekpart;speed;
//    pro ir nasleduje: irid;speed;
class function TBlkNavTrainEvent.ParseOldRychEvent(str:string): TRREv;
var data:TStrings;
    rrData:TRREvData;
begin
 data := TStringList.Create();

 try
   ExtractStringsEx([';'], [], str, data);

   case (data[0][1]) of
    '0':begin
      // usek
      rrData.typ := TRREvType.rrtUsek;
      rrData.usekState := true;
      rrData.usekPart := StrToInt(data[2]);
    end;//case 0

    '1':begin
      // ir
      rrData.typ := TRREvType.rrtIR;
      rrData.irId := StrToInt(data[1]);
      rrData.irState := true;
    end;//case 1
   end;//case

   Result := TRREv.Create(rrData);
 finally
   data.Free();
 end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkNav.IsEnabled(): Boolean;
begin
 Result := (Self.navest <> TBlkNavCode.ncDisabled);
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit
