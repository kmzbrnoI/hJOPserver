unit TBlokNav;

{ SIGNAL technological block definition. }

interface

uses IniFiles, TBlok, Menus, TOblsRizeni, SysUtils, Classes, rrEvent,
      TechnologieJC, IdContext, Generics.Collections, THnaciVozidlo,
      TOblRizeni, StrUtils, JsonDataObjects, TechnologieRCS, Train, JclPCRE;

type
 TBlkSignalSelection = (none = 0, VC = 1, PC = 2, NC = 3, PP = 4);
 TBlkSignalOutputType = (scom = 0, binary = 1);
 TBlkSignalSymbol = (unknown = -1, main = 0, shunting = 1);
 TBlkSignalCode = (
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

 // zastavovaci a zpomalovaci udalost pro jeden typ soupravy a jeden rozsah delek soupravy
 TBlkSignalTrainEvent = class
  train_typ_re: TJclRegEx;                       // regexp matchujici typ soupravy
  length: record                                  // tato udalost je brana v potaz, pokud ma souprava delku > min && < max
    min: Integer;
    max: Integer;
  end;

  stop: TRREv;                                   // zastavovaci udalost
  slow: record                                   // zpomalovaci udalost
    enabled: Boolean;                             // povolena zpomalovaci udalost?
    speed: Integer;                               // rychlost z km/h (40, 50, 60...)
    ev: TRREv;                                    // udalost
  end;

   constructor Create(); overload;
   constructor Create(str: string; old: Boolean); overload;
   destructor Destroy(); override;

   procedure Parse(str: string; old: Boolean);
   class function ParseTrainTypes(types: string): string;
   function ToFileStr(short: Boolean = false): string;
   class function ParseOldRychEvent(str: string): TRREv;
 end;

 TBlkSignalSettings = record
  RCSAddrs: TRCSAddrs;
  outputType: TBlkSignalOutputType;
  events: TObjectList<TBlkSignalTrainEvent>;      // tady jsou ulozena veskera zastavovani a zpomalovani; zastaveni na indexu 0 je vzdy primarni
                                                  // program si pamatuje vice zastavovacich a zpomalovaich udalosti pro ruzne typy a delky soupravy
  fallDelay: Integer;                             // zpozdeni padu navestidla v sekundach (standartne 0)
  locked: Boolean;                                // jestli je navestidlo trvale zamknuto na STUJ (hodi se napr. u navestidel a konci kusych koleji)
 end;

 TBlkSignalState = record
  signal, signalOld: TBlkSignalCode;
  selected: TBlkSignalSelection;                  // zacatek volby jidni cesty
  beginAB: Boolean;                               // jestli je zacatek volby JC v rezimu AB
  targetSignal: TBlkSignalCode;                   // navest, ktera ma byt nastavena
  ABJC: TJC;                                      // odkaz na automaticky stavenou JC
  ZAM: Boolean;                                   // navestidlo zamkle z panelu
  dnJC, privolJC: TJC;                            // reference na aktualni JC na navestidle (resp. NC)

  falling: Boolean;                               // zde je true, pokud navestidlo pada do STUJ (ma zpozdeny pad) a jeste nespadlo
  fallingStart: TDateTime;

  privolStart: TDateTime;                         // start privolavaci navesti (privolavacka sviti pouze omezeny cas a pak se vypne)
  privolTimerId: Integer;                         // id timeru ukonceni privolavacky v panelu, ze ktreho byla JC postavena
  autoblok: Boolean;

  toRnz: TDictionary<Integer, Cardinal>;          // seznam bloku k RNZ spolu s pocty ruseni, ktere je treba udelat
  RCtimer: Integer;                               // id timeru, ktery se prave ted pouziva pro ruseni JC
  changeCallbackOk, changeCallbackErr: TNotifyEvent; // notifikace o nastaveni polohy navestidla
  changeEnd: TTime;
 end;

 TBlkSignalSpnl = record
  symbolType: TBlkSignalSymbol;
  trackId: Integer;                                // ID useku pred navestidlem
  direction: THVStanoviste;
 end;

 TBlkSignal = class(TBlk)
  const
   _def_Nav_stav: TBlkSignalState = (
     signal : ncDisabled;
     selected : none;
     beginAB : false;
     targetSignal : ncDisabled;
     ABJC : nil;
     ZAM : false;
     dnJC : nil;
     privolJC : nil;
     privolTimerId : 0;
     autoblok : false;
     RCtimer : -1;
     changeCallbackOk: nil;
     changeCallbackErr: nil;
   );

   // doba sviceni privolavaci navesti
   _PRIVOL_MIN = 1;
   _PRIVOL_SEC = 30;

   _SIG_DEFAULT_DELAY = 2;
   _SIG_CHANGE_DELAY_MSEC = 1000;
   _SIG_CHANGE_SHORT_DELAY_MSEC = 200;

  private
   m_settings: TBlkSignalSettings;
   m_state: TBlkSignalState;
   m_spnl: TBlkSignalSpnl;

   m_trackId: TBlk;
   m_lastEvIndex: Integer;

    function IsEnabled(): Boolean;
    function RCinProgress(): Boolean;

    procedure mSetSignal(signal: TBlkSignalCode);

    function GetAB(): Boolean;
    procedure SetAB(ab: Boolean);
    procedure SetABJC(ab: TJC);

    procedure SetSelected(typ: TBlkSignalSelection);
    procedure SetZAM(zam: Boolean);

    procedure MenuVCStartClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuVCStopClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuPCStartClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuPCStopClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuSTUJClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuDNClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuRCClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuABStartClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuABStopClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuLockClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuUnlockClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuPNStartClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuPNStopClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuPPStartClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuPPStopClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuPPNClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuRNZClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuKCDKClick(SenderPnl: TIdContext; SenderOR: TObject);

    procedure MenuAdminStopIR(SenderPnl: TIdContext; SenderOR: TObject; enabled: Boolean);

    procedure UpdateFalling();
    procedure UpdatePrivol();
    procedure UpdateSignalSet();
    procedure OnSignalSetOk();
    procedure OnSignalSetError();

    procedure PrivolDKClick(SenderPnl: TIDContext; SenderOR: TObject; Button: TPanelButton);
    procedure PrivokDKPotvrSekv(Sender: TIdContext; success: Boolean);
    procedure RNZPotvrSekv(Sender: TIdContext; success: Boolean);

    function GetTrackId(): TBlk;
    procedure SetTrackId(new_id: Integer);

    function CurrentEventIndex(): Integer;
    function CanIDoRNZ(): Boolean;

    procedure UnregisterAllEvents();
    function IsChanging(): Boolean;
    function GetTargetSignal(): TBlkSignalCode;

  public
    constructor Create(index: Integer);
    destructor Destroy(); override;

    function IsGoSignal(jctype: TJCType = TJCType.vlak): Boolean; overload;
    function IsOpakVystraha(): Boolean;
    class function IsGoSignal(Navest: TBlkSignalCode; jctype: TJCType = TJCType.vlak): Boolean; overload;

    procedure LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile); override;
    procedure SaveData(ini_tech: TMemIniFile; const section: string); override;

    procedure Enable(); override;
    procedure Disable(); override;
    function UsesRCS(addr: TRCSAddr; portType: TRCSIOType): Boolean; override;

    procedure Update(); override;
    procedure Change(now: Boolean = false); override;

    procedure JCZrusNavest();   // zahrnuje cas na pad navesti
    procedure SetSignal(navest: TBlkSignalCode; changeCallbackOk, changeCallbackErr: TNotifyEvent);

    //----- Signal specific functions -----

    function GetSettings(): TBlkSignalSettings;
    procedure SetSettings(data: TBlkSignalSettings);

    procedure UpdateRychlostTrain(force: Boolean = false);
    procedure AddBlkToRnz(blkId: Integer; change: Boolean = true);
    procedure RemoveBlkFromRnz(blkId: Integer);
    procedure RCtimerTimeout();
    function FourtyKmph(): Boolean;
    class function AddOpak(navest: TBlkSignalCode): TBlkSignalCode;

    function GetTrain(usek: TBlk = nil): TTrain;
    procedure PropagatePOdjToTrat();

    class function SignalToString(navest: TBlkSignalCode): string;

    property symbolType: TBlkSignalSymbol read m_spnl.symbolType;
    property trackId: Integer read m_spnl.trackId write SetTrackId;
    property direction: THVStanoviste read m_spnl.direction write m_spnl.direction;

    property signal: TBlkSignalCode read m_state.signal write mSetSignal;
    property targetSignal: TBlkSignalCode read GetTargetSignal;
    property selected: TBlkSignalSelection read m_state.selected write SetSelected;
    property beginAB: Boolean read m_state.beginAB;
    property AB: Boolean read GetAB write SetAB;
    property ABJC: TJC read m_state.ABJC write SetABJC;
    property ZAM: Boolean read m_state.ZAM write SetZAM;
    property lichy: THVStanoviste read m_spnl.direction;
    property DNjc: TJC read m_state.dnJC write m_state.dnJC;
    property privol: TJC read m_state.privolJC write m_state.privolJC;
    property track: TBlk read GetTrackId;
    property autoblok: Boolean read m_state.autoblok write m_state.autoblok;
    property canRNZ: Boolean read CanIDoRNZ;
    property RCtimer: Integer read m_state.RCtimer write m_state.RCtimer;
    property changing: Boolean read IsChanging;
    property enabled: Boolean read IsEnabled;

    procedure PanelMenuClick(SenderPnl: TIdContext; SenderOR: TObject; item: string; itemindex: Integer); override;
    function ShowPanelMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TORCOntrolRights): string; override;
    procedure PanelClick(SenderPnl: TIdCOntext; SenderOR: TObject; Button: TPanelButton; rights: TORCOntrolRights; params: string = ''); override;
    function PanelStateString(): string; override;

    procedure GetPtData(json: TJsonObject; includeState: Boolean); override;
    procedure GetPtState(json: TJsonObject); override;

 end;

//format dat Navu v souboru *.ini:
//  ev=udalosti zastavovani a zpomalovani
//  OutType=typ vystupu (scom, binarni)
//  zamknuti=zamknuti navestidla trvale do STUJ

// format ev: (ev1)(ev2)(ev3)
// format ev1: RychEvent-zastaveni|RychEvent-zpomaleni|re: train_typ_regexp|min_delka|max_delka
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

constructor TBlkSignal.Create(index: Integer);
begin
 inherited Create(index);

 Self.GlobalSettings.typ := btSignal;
 Self.m_state := Self._def_nav_stav;
 Self.m_state.toRnz := TDictionary<Integer, Cardinal>.Create();
 Self.m_settings.events := TObjectList<TBlkSignalTrainEvent>.Create();
 Self.m_trackId := nil;
end;

destructor TBlkSignal.Destroy();
begin
 Self.m_state.toRnz.Free();
 Self.m_settings.events.Free();

 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSignal.LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile);
var strs: TStrings;
    str: string;
    i: Integer;
begin
 inherited LoadData(ini_tech, section, ini_rel, ini_stat);

 Self.m_settings.RCSAddrs := Self.LoadRCS(ini_tech, section);

 Self.m_settings.locked := ini_tech.ReadBool(section, 'zamknuti', false);

 Self.m_settings.OutputType := TBlkSignalOutputType(ini_tech.ReadInteger(section, 'OutType', 0));
 Self.m_settings.fallDelay := ini_tech.ReadInteger(section, 'zpoz', _SIG_DEFAULT_DELAY);

 strs := Self.LoadORs(ini_rel, 'N');
 try
   if (strs.Count >= 3) then
    begin
     Self.m_spnl.symbolType := TBlkSignalSymbol(StrToInt(strs[1]));

     // 0 = navestidlo v lichem smeru. 1 = navestidlo v sudem smeru
     if (strs[2] = '0') then
       Self.m_spnl.direction := THVStanoviste.lichy
     else
       Self.m_spnl.direction := THVStanoviste.sudy;
     Self.m_spnl.trackId := StrToInt(strs[3]);
    end else begin
     Self.m_spnl.symbolType := TBlkSignalSymbol.unknown;
     Self.m_spnl.direction := THVStanoviste.lichy;
     Self.m_spnl.trackId := -1;
    end;
 finally
   strs.Free();
 end;

 // Nacitani zastavovacich a zpomaovacich udalosti
 // toto musi byt az po nacteni spnl
 Self.m_settings.events.Clear();

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
         Self.m_settings.events.Add(TBlkSignalTrainEvent.Create(str, true));
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
       Self.m_settings.events.Add(TBlkSignalTrainEvent.Create(str, false));
     except

     end;
     Inc(i);
     str := ini_tech.ReadString(section, 'ev'+IntToStr(i), '');
    end;
  end;

 PushRCSToOR(Self.ORsRef, Self.m_settings.RCSAddrs);
end;

procedure TBlkSignal.SaveData(ini_tech: TMemIniFile; const section: string);
var i: Integer;
begin
 inherited SaveData(ini_tech, section);

 Self.SaveRCS(ini_tech, section, Self.m_settings.RCSAddrs);

 for i := 0 to Self.m_settings.events.Count-1 do
   ini_tech.WriteString(section, 'ev'+IntToStr(i), Self.m_settings.events[i].ToFileStr(i = 0));

 if (Self.m_settings.RCSAddrs.Count > 0) then
   ini_tech.WriteInteger(section, 'OutType', Integer(Self.m_settings.OutputType));

 if (Self.m_settings.fallDelay <> _SIG_DEFAULT_DELAY) then
   ini_tech.WriteInteger(section, 'zpoz', Self.m_settings.fallDelay);

 if (Self.m_settings.locked) then
   ini_tech.WriteBool(section, 'zamknuti', Self.m_settings.locked);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSignal.Enable();
var rcsaddr: TRCSAddr;
    enable: Boolean;
begin
 if (Self.signal <> ncDisabled) then
   Exit(); // skip already enabled block

 enable := true;
 try
   for rcsaddr in Self.m_settings.RCSAddrs do
     if (not RCSi.IsNonFailedModule(rcsaddr.board)) then
       enable := false;
 except
   enable := false;
 end;

 if (enable) then
  begin
   Self.m_state.signal := ncStuj;
   Self.m_state.signalOld := ncStuj;
  end;

 Self.m_state.toRnz.Clear();
 Self.UnregisterAllEvents();
 Self.Change();
end;

procedure TBlkSignal.Disable();
begin
 Self.m_state.signal := ncDisabled;
 Self.m_state.signalOld := ncDisabled;
 Self.m_state.selected := TBlkSignalSelection.none;
 Self.AB := false;
 Self.m_state.ZAM  := false;
 Self.m_state.toRnz.Clear();
 Self.m_state.RCtimer := -1;
 Self.UnregisterAllEvents();
 Self.Change(true);
end;

function TBlkSignal.UsesRCS(addr: TRCSAddr; portType: TRCSIOType): Boolean;
begin
 Result := ((portType = TRCSIOType.output) and (Self.m_settings.RCSAddrs.Contains(addr)));
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSignal.Update();
begin
 Self.UpdateSignalSet();
 Self.UpdateFalling();
 Self.UpdateRychlostTrain();

 if (Self.signal = ncPrivol) then
   Self.UpdatePrivol();

 if (Self.m_settings.RCSAddrs.Count > 0) then
  begin
   if (RCSi.IsNonFailedModule(Self.m_settings.RCSAddrs[0].board)) then
    begin
     if (Self.m_state.signal = ncDisabled) then
      begin
       Self.m_state.signal := ncStuj;
       Self.Change();
      end;
    end else begin
     if (Self.changing) then
       Self.OnSignalSetError();

     if (Self.m_state.signal >= ncStuj) then
      begin
       Self.m_state.signal := ncDisabled;
       JCDb.RusJC(Self);
       Self.Change();
      end;
    end;
  end;

 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSignal.Change(now: Boolean = false);
begin
 // zmenu navesti propagujeme do prilehle trati, kde by mohlo dojit ke zmene
 // navesti autobloku
 if ((Self.track <> nil) and (Self.track.typ = btTU) and (TBlkTU(Self.track).InTrat > -1)) then
   Self.track.Change();

 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkSignal.GetSettings(): TBlkSignalSettings;
begin
 Result := Self.m_settings;
end;

procedure TBlkSignal.SetSettings(data: TBlkSignalSettings);
begin
 if (Self.m_settings.events <> data.events) then
   Self.m_settings.events.Free();

 if (Self.m_settings.RCSAddrs <> data.RCSAddrs) then
   Self.m_settings.RCSAddrs.Free();

 Self.m_settings := data;
 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////
//nastavovani stavovych promennych:

procedure TBlkSignal.SetSignal(navest: TBlkSignalCode; changeCallbackOk, changeCallbackErr: TNotifyEvent);
var oblr: TOR;
    traini: Integer;
begin
 if ((Self.m_state.signal = ncDisabled) or (Self.m_settings.locked)) then
  begin
   if (Assigned(changeCallbackErr)) then
     changeCallbackErr(Self);
   Exit();
  end;

 if ((navest = ncPrivol) or (navest = ncStuj)) then
  begin
   // prodlouzeni nebo zruseni privolavaci navesti -> zrusit odpocet v panelu
   if (Self.m_state.privolTimerId > 0) then
     for oblr in Self.ORsRef do
      begin
       oblr.BroadcastGlobalData('INFO-TIMER-RM;'+IntToStr(Self.m_state.privolTimerId));
       oblr.TimerCnt := oblr.TimerCnt - 1;
      end;
   Self.m_state.privolTimerId := 0;
  end;

 if (navest = ncPrivol) then
   Self.m_state.privolStart := Now;

 if ((Self.m_state.signal = navest) or ((Self.changing) and (Self.m_state.targetSignal = navest))) then
  begin
   if (Assigned(changeCallbackOk)) then
     changeCallbackOk(Self);
   Exit();
  end;

 // nastaveni vystupu
 try
   if (Self.m_settings.RCSAddrs.Count > 0) then
    begin
     if (Self.m_settings.OutputType = scom) then
      begin
       //scom
       RCSi.SetOutputs(Self.m_settings.RCSAddrs, Integer(navest));
      end else begin
       //binary
       case (navest) of
        ncStuj, ncVse, ncPrivol, ncZhasnuto:
            RCSi.SetOutputs(Self.m_settings.RCSAddrs, 0);
       else
        RCSi.SetOutputs(Self.m_settings.RCSAddrs, 1);
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
   if ((Self.track <> nil) and ((Self.track.typ = btUsek) or
       (Self.track.typ = btTU)) and ((Self.track as TBlkUsek).signalJCRef.Contains(Self))) then
    (Self.track as TBlkUsek).signalJCRef.Remove(Self);

   if (Assigned(Self.privol)) then
    begin
     Self.privol.RusJCWithoutBlk();
     Self.privol := nil;
    end;
  end;

 if ((Self.signal = ncPrivol) and (navest = ncStuj)) then
  begin
   // STUJ po privolavacce -> vypnout zvukovou vyzvu
   for oblr in Self.ORsRef do
     oblr.PrivolavackaBlkCnt := oblr.PrivolavackaBlkCnt - 1;
  end;

 if (not Self.changing) then
   Self.m_state.signalOld := Self.signal;
 Self.m_state.changeCallbackOk := changeCallbackOk;
 Self.m_state.changeCallbackErr := changeCallbackErr;
 Self.m_state.signal := ncChanging;
 Self.m_state.targetSignal := navest;

 if (Self.m_settings.RCSAddrs.Count > 0) then
   Self.m_state.changeEnd := Now + EncodeTime(0, 0, _SIG_CHANGE_DELAY_MSEC div 1000, _SIG_CHANGE_DELAY_MSEC mod 1000)
 else
   Self.m_state.changeEnd := Now + EncodeTime(0, 0, _SIG_CHANGE_SHORT_DELAY_MSEC div 1000, _SIG_CHANGE_SHORT_DELAY_MSEC mod 1000);

 if (not TBlkSignal.IsGoSignal(Self.m_state.targetSignal)) then // zastavujeme ihned
   Self.UpdateRychlostTrain(true);

 if (Self.track <> nil) then
  begin
   for traini in TBlkUsek(Self.track).trains do
     Trains[traini].OnPredictedSignalChange();
   if (TBlkUsek(Self.track).trainPredict <> nil) then
     TBlkUsek(Self.track).trainPredict.OnPredictedSignalChange();
  end;

 Self.Change();
end;

procedure TBlkSignal.mSetSignal(signal: TBlkSignalCode);
begin
 Self.SetSignal(signal, TNotifyEvent(nil), TNotifyEvent(nil));
end;

procedure TBlkSignal.OnSignalSetOk();
var tmp: TNotifyEvent;
    oblr: TOR;
begin
 Self.m_state.signal := Self.m_state.targetSignal;

 if (Self.m_state.targetSignal = ncPrivol) then
  begin
   // nova navest je privolavacka -> zapnout zvukovou vyzvu
   for oblr in Self.ORsRef do
     oblr.PrivolavackaBlkCnt := oblr.PrivolavackaBlkCnt + 1;
  end;

 if (Self.autoblok) then
  begin
   if (TBlkTU(Self.track).nextTU <> nil) then TBlkTU(Self.track).nextTU.Change();
   if (TBlkTU(Self.track).Trat <> nil) then TBlkTrat(TBlkTU(Self.track).Trat).UpdateTrainPredict();
  end;

 if (Assigned(Self.m_state.changeCallbackOk)) then
  begin
   tmp := Self.m_state.changeCallbackOk; // may set new callback in event
   Self.m_state.changeCallbackOk := nil;
   tmp(Self);
  end;

 Self.UpdateRychlostTrain(true);
 JCDb.CheckNNavaznost(Self);
 Self.Change();
end;

procedure TBlkSignal.OnSignalSetError();
var tmp: TNotifyEvent;
begin
 // Tato funkce zatim neni moc vyuzivana, jedna se o pripravu do budoucna, kdy
 // by melo navestidlo kontrolu navesti (vstup do hJOP).

 if (Assigned(Self.m_state.changeCallbackErr)) then
  begin
   tmp := Self.m_state.changeCallbackErr; // may set new callback in event
   Self.m_state.changeCallbackErr := nil;
   tmp(Self);
  end;

 // Jak nastavit aktualni navest?
 Self.m_state.signal := ncStuj;

 Self.Change();
end;

procedure TBlkSignal.UpdateSignalSet();
begin
 if (not Self.changing) then Exit();
 if (Self.m_state.changeEnd <= Now) then
   Self.OnSignalSetOk();
end;

function TBlkSignal.GetAB(): Boolean;
begin
 Result := (Self.m_state.ABJC <> nil);
end;

procedure TBlkSignal.SetABJC(ab: TJC);
begin
 if ((ab <> nil) and (Self.ABJC <> nil)) then
   raise EInvalidOperation.Create('Cannot change AB JC, can only enable/disable AB JC!');

 if ((Self.ABJC <> nil) and (ab = nil)) then begin
   try
     if (Assigned(ABlist)) then
       ABlist.Remove(Self.ABJC);
   except
     on E: EABJCNotInList do
       asm nop; end; // ignore exception
   end;
   Self.m_state.ABJC := nil;
   Self.Change();
 end else if ((Self.ABJC = nil) and (ab <> nil)) then begin
   try
     ABlist.Add(ab);
   except
     on E: EABJCAlreadyInList do
       asm nop; end; // ignore exception
   end;
   Self.m_state.ABJC := ab;
   Self.Change();
 end;
end;

procedure TBlkSignal.SetAB(ab: Boolean);
begin
 if (ab) then
   raise EInvalidOperation.Create('You can only enable AB via SetABJC!');

 if (Self.AB and (not ab)) then
   Self.ABJC := nil;
end;

procedure TBlkSignal.SetSelected(typ: TBlkSignalSelection);
begin
 if (Self.m_state.selected = typ) then Exit();
 Self.m_state.selected := typ;
 if (typ = TBlkSignalSelection.none) then
   Self.m_state.beginAB := false;
 Self.Change();
end;

procedure TBlkSignal.SetZAM(zam: Boolean);
begin
 if (Self.m_state.ZAM = zam) then Exit();
 Self.m_state.ZAM := zam;

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

 if ((Self.autoblok) and (not zam) and (Self.track <> nil) and (TBlkTU(Self.track).Trat <> nil)) then
   TBlkTrat(TBlkTU(Self.track).Trat).ChangeUseky();

 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////
//gui: menu
//dynamicke funkce

procedure TBlkSignal.MenuVCStartClick(SenderPnl: TIdContext; SenderOR: TObject);
var Blk: TBlk;
begin
 if (Self.m_spnl.symbolType = TBlkSignalSymbol.shunting) then Exit();
 if ((SenderOR as TOR).stack.volba = PV) then
   if (((Self.DNjc <> nil) and (Self.DNjc.RozpadRuseniBlok < 1)) or
       (JCDb.FindOnlyStaveniJC(Self.id) <> nil)) then Exit;

 Blk := Blky.GeTBlkSignalSelected((SenderOR as TOR).id);
 if (Blk <> nil) then
  begin
   (Blk as TBlkSignal).selected := TBlkSignalSelection.none;
   TOR(SenderOR).ClearVb(); // smazeme dosavadni seznam variantnich bodu
  end;
 Self.selected := TBlkSignalSelection.VC;
 Self.m_state.beginAB := false;
end;

procedure TBlkSignal.MenuVCStopClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 Self.selected := TBlkSignalSelection.none;
end;

procedure TBlkSignal.MenuPCStartClick(SenderPnl: TIdContext; SenderOR: TObject);
var Blk: TBlk;
begin
 if ((SenderOR as TOR).stack.volba = PV) then
   if (((Self.DNjc <> nil) and (Self.DNjc.RozpadRuseniBlok < 1)) or
       (JCDb.FindOnlyStaveniJC(Self.id) <> nil)) then Exit;

 Blk := BLky.GeTBlkSignalSelected((SenderOR as TOR).id);
 if (Blk <> nil) then (Blk as TBlkSignal).selected := TBlkSignalSelection.none;
 Self.selected := TBlkSignalSelection.PC;
 Self.m_state.beginAB := false;
end;

procedure TBlkSignal.MenuPCStopClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 Self.selected := TBlkSignalSelection.none;
end;

procedure TBlkSignal.MenuSTUJClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 // poradi musi byt zachovano !
 Self.signal := ncStuj;
 if (Self.DNjc = nil) then Exit();

 Self.DNjc.STUJ();
 Blky.TrainPrediction(Self);
end;

procedure TBlkSignal.MenuDNClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 if (Self.DNjc = nil) then Exit();

 if (Self.RCinProgress()) then
  begin
   TOR(SenderOR).StopMereniCasu(Self.m_state.RCtimer);
   Self.m_state.RCtimer := -1;
  end;

 Self.DNjc.DN();
 Blky.TrainPrediction(Self);
end;

procedure TBlkSignal.MenuRCClick(SenderPnl: TIdContext; SenderOR: TObject);
var JC: TJC;
    Blk: TBlk;
begin
 if ((Self.DNjc = nil) or (Self.RCinProgress())) then Exit;

 JC := Self.DNjc;

 Blk := Self.track;
 if ((Blk = nil) or ((Blk.typ <> btUsek) and (Blk.typ <> btTU))) then
  begin
   // pokud blok pred JC neni -> 30 sekund
   Self.m_state.RCtimer := (SenderOR as TOR).AddMereniCasu(JC.RusJC, EncodeTime(0, 0, 30, 0));
  end else begin
   if ((Blk as TBlkUsek).Obsazeno = TUsekStav.uvolneno) then
    begin
     // pokud neni blok pred JC obsazen -> 2 sekundy
     Self.m_state.RCtimer := (SenderOR as TOR).AddMereniCasu(JC.RusJC, EncodeTime(0, 0, 2, 0));
    end else begin
     // pokud je obsazen, zalezi na typu jizdni cesty
     case (JC.typ) of
      TJCType.vlak  : Self.m_state.RCtimer := (SenderOR as TOR).AddMereniCasu(JC.RusJC, EncodeTime(0, 0, 15, 0));   // vlakova cesta : 20 sekund
      TJCType.posun : Self.m_state.RCtimer := (SenderOR as TOR).AddMereniCasu(JC.RusJC, EncodeTime(0, 0,  5, 0));   // posunova cesta: 10 sekund
     else
      Self.m_state.RCtimer := (SenderOR as TOR).AddMereniCasu(JC.RusJC, EncodeTime(0, 1, 0, 0));                   // nejaka divna cesta: 1 minuta
     end;
    end;
  end;

 Self.AB := false;
 JC.RusJCWithoutBlk();
 Blky.TrainPrediction(Self);
end;

procedure TBlkSignal.MenuABStartClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 if (Self.DNjc <> nil) then
   Self.ABJC := Self.DNjc
 else begin
   Self.MenuVCStartClick(SenderPnl, SenderOR);
   Self.m_state.beginAB := true;
 end;
end;

procedure TBlkSignal.MenuABStopClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 if (Self.ABJC <> nil) then
   Self.ABJC := nil
 else
   Self.MenuVCStopClick(SenderPnl, SenderOR);
end;

procedure TBlkSignal.MenuLockClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 Self.ZAM := true;
 Self.signal := ncStuj;
end;

procedure TBlkSignal.MenuUnlockClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 Self.ZAM := false;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSignal.MenuPNStartClick(SenderPnl: TIdContext; SenderOR: TObject);
var Blk: TBlk;
    oblr: TOR;
begin
 if (Self.m_spnl.symbolType = TBlkSignalSymbol.shunting) then Exit;

 Blk := Blky.GeTBlkSignalSelected((SenderOR as TOR).id);
 if (Blk <> nil) then (Blk as TBlkSignal).selected := TBlkSignalSelection.none;
 Self.selected := TBlkSignalSelection.NC;

 for oblr in Self.stations do
   oblr.ORDKClickServer(Self.PrivolDKClick);
end;

procedure TBlkSignal.MenuPNStopClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 Self.selected := TBlkSignalSelection.none;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSignal.MenuPPStartClick(SenderPnl: TIdContext; SenderOR: TObject);
var Blk: TBlk;
begin
 if ((SenderOR as TOR).stack.volba = PV) then
   if ((Self.signal > ncStuj) or (JCDb.FindJC(Self.id, false) <> nil)) then Exit;

 Blk := Blky.GeTBlkSignalSelected((SenderOR as TOR).id);
 if (Blk <> nil) then (Blk as TBlkSignal).selected := TBlkSignalSelection.none;
 Self.selected := TBlkSignalSelection.PP;
end;

procedure TBlkSignal.MenuPPStopClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 Self.selected := TBlkSignalSelection.none;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSignal.MenuPPNClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 ORTCPServer.Potvr(SenderPnl, Self.PrivokDKPotvrSekv, SenderOR as TOR, 'Prodloužení doby přivolávací návěsti', TBlky.GetBlksList(Self), nil);
end;

procedure TBlkSignal.MenuRNZClick(SenderPnl: TIdContext; SenderOR: TObject);
var podminky: TList<TPSPodminka>;
    blkId: Integer;
    blk: TBlk;
begin
 podminky := TList<TPSPodminka>.Create();

 for blkId in Self.m_state.toRnz.Keys do
  begin
   Blky.GetBlkByID(blkId, Blk);
   if (blk <> nil) then
     podminky.Add(TOR.GetPSPodminka(blk, 'Rušení NZ'));
  end;

 ORTCPServer.Potvr(SenderPnl, Self.RNZPotvrSekv, SenderOR as TOR, 'Zrušení nouzových závěrů po nouzové cestě', TBlky.GetBlksList(Self), podminky);
end;

procedure TBlkSignal.MenuKCDKClick(SenderPnl: TIdContext; SenderOR: TObject);
var oblr: TOR;
begin
 if (Self.selected = TBlkSignalSelection.NC) then
  begin
   for oblr in Self.stations do
     oblr.ORDKClickClient();
   ORTCPServer.Potvr(SenderPnl, Self.PrivokDKPotvrSekv, SenderOR as TOR, 'Zapnutí přivolávací návěsti',
                     TBlky.GetBlksList(Self), nil);
  end;
end;

procedure TBlkSignal.MenuAdminStopIR(SenderPnl: TIdContext; SenderOR: TObject; enabled: Boolean);
var Blk: TBlk;
begin
 try
   if (Self.m_settings.events[0].stop.typ = TRREvType.rrtIR) then
    begin
     Blky.GetBlkByID(Self.m_settings.events[0].stop.data.irId, Blk);
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

procedure TBlkSignal.PanelClick(SenderPnl: TIdCOntext; SenderOR: TObject; Button: TPanelButton; rights: TORCOntrolRights; params: string = '');
begin
 case (Button) of
  F2: ORTCPServer.Menu(SenderPnl, Self, (SenderOR as TOR), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));

  ENTER: begin
    if (((((Self.DNjc = nil) or (Self.DNjc.RozpadRuseniBlok >= 1)) and
           (JCDb.FindOnlyStaveniJC(Self.id) = nil) and (Self.signal <> ncPrivol) and (JCDb.IsAnyVCAvailable(Self) and (Self.enabled)))
         or (TOR(SenderOR).stack.volba = VZ)) and (JCDb.IsAnyVC(Self))) then begin
      if ((not Self.m_settings.locked) and (not Self.autoblok)) then Self.MenuVCStartClick(SenderPnl, SenderOR);
    end else
      ORTCPServer.Menu(SenderPnl, Self, (SenderOR as TOR), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));
  end;

  F1: begin
    if (((((Self.DNjc = nil) or (Self.DNjc.RozpadRuseniBlok >= 1)) and
           (JCDb.FindOnlyStaveniJC(Self.id) = nil) and (Self.signal <> ncPrivol) and (JCDb.IsAnyPCAvailable(Self)) and (Self.enabled))
         or ((SenderOR as TOR).stack.volba = VZ)) and (JCDb.IsAnyPC(Self))) then begin
      if ((not Self.m_settings.locked) and (not Self.autoblok)) then Self.MenuPCStartClick(SenderPnl, SenderOR);
    end else
      ORTCPServer.Menu(SenderPnl, Self, (SenderOR as TOR), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));
  end;
 end;//case
end;

////////////////////////////////////////////////////////////////////////////////

//toto se zavola pri kliku na jakoukoliv itemu menu tohoto bloku
procedure TBlkSignal.PanelMenuClick(SenderPnl: TIdContext; SenderOR: TObject; item: string; itemindex: Integer);
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
function TBlkSignal.ShowPanelMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TORCOntrolRights): string;
var Blk: TBlk;
begin
 Result := inherited;

 // pokud je navestidlo trvale zamkle, neumoznime zadne volby
 if (Self.m_settings.locked) then Exit();

 if (((((Self.DNjc = nil) or (Self.DNjc.RozpadRuseniBlok >= 1)) and
        (JCDb.FindOnlyStaveniJC(Self.id) = nil) and (Self.signal <> ncPrivol) and (not Self.AB))
      or ((SenderOR as TOR).stack.volba = VZ)) and
     (not Self.autoblok)) then
  begin
    case (Self.m_state.selected) of
     TBlkSignalSelection.VC :
       if (Self.beginAB) then
         Result := Result + 'AB<,'
       else
         Result := Result + 'VC<,';
     TBlkSignalSelection.PC : Result := Result + 'PC<,';
     TBlkSignalSelection.NC : Result := Result + 'PN<,';
     TBlkSignalSelection.PP : Result := Result + 'PP<,';
    else
      //2 = VC, 3= PC
      if (Self.m_spnl.symbolType = TBlkSignalSymbol.main) then
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
    end;// else selected <> none ...

    Result := Result + '-,';
  end;

 if ((Self.signal > ncStuj) and (not Self.autoblok)) then
   Result := Result + 'STUJ,';

 if (Self.signal = ncPrivol) then
   Result := Result + '!PPN,';

 if (Self.DNjc <> nil) then
  begin
   // bud je cesta primo postavena, nebo je zrusena, ale podminky jsou vyhovujici pro DN
   // plati jen pro postavenou JC
   if ((not Self.ZAM) and (Self.signal = ncStuj) and (Self.DNjc.CanDN())) then
     Result := Result + 'DN,';

   if (((Self.signal > ncStuj) or (Self.DNjc.CanDN()) or (Self.DNjc.RozpadBlok < 1))
       and (not Self.RCinProgress())) then
    begin
     Result := Result + 'RC,';

     // AB lze jen u vlakove cesty
     if ((Self.DNjc.typ = TJCType.vlak) and (not Self.AB)) then
       Result := Result + 'AB>,';
    end;
 end;

 // AB lze jen u vlakove cesty
 if (Self.AB) then
   Result := Result + 'AB<,';

 if (Self.m_state.ZAM) then
   Result := Result + 'ZAM<,'
 else
   Result := Result + 'ZAM>,';

 if ((Self.signal <> ncPrivol) and (Self.CanIDoRNZ)) then
  Result := Result + '!RNZ,';

 // DEBUG: jednoduche nastaveni IR pri knihovne simulator
 if (RCSi.simulation) then
  begin
   if ((Self.m_settings.events.Count > 0) and (Self.m_settings.events[0].stop.typ = TRREvType.rrtIR)) then
    begin
     Blky.GetBlkByID(Self.m_settings.events[0].stop.data.irId, Blk);
     if ((Blk <> nil) and (Blk.typ = btIR)) then
      begin
       case (TBlkIR(Blk).occupied) of
         TIROccupationState.free     : Result := Result + '-,*IR>,';
         TIROccupationState.occupied : Result := Result + '-,*IR<,';
       end;//case
      end;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

class function TBlkSignal.SignalToString(navest: TBlkSignalCode): string;
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
   ncVystraha40: Result  := '40 km/h a výstraha';
   nc40Ocek40: Result  := '40 km/h a očekávejte 40 km/h';
   ncPrivol: Result  := 'přivolávací návěst';
   ncPosunZaj: Result  := 'dovolen zajištěný posun';
   ncPosunNezaj: Result := 'dovolen nezajištěný posun';
   ncOpakVolno: Result := 'opakování návěsti volno';
   ncOpakVystraha: Result := 'opakování návěsti výstraha';
   ncZhasnuto: Result := 'návěstidlo zhaslé';
   ncOpakOcek40: Result := 'opakování návěsti očekávejte 40 km/h';
   ncOpakVystraha40: Result := 'opakování návěsti výstraha a 40 km/h';
   ncOpak40Ocek40: Result := '40 km/h a opakování návěsti očekávejte 40 km/h';
  else
    Result := 'Jiná návěst';
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSignal.JCZrusNavest();
begin
 if (Self.m_settings.fallDelay > 0) then
  begin
   Self.m_state.falling := true;
   Self.m_state.fallingStart := Now;
   writelog('Návěstidlo '+Self.GlobalSettings.name+': spoždění pádu '+IntToStr(Self.m_settings.fallDelay)+' s', WR_VC);
  end else begin
   Self.signal := ncStuj;
  end;

 Self.UpdateRychlostTrain(true);
end;

procedure TBlkSignal.UpdateFalling();
begin
 if (not Self.m_state.falling) then Exit();

 if (Self.m_state.fallingStart + EncodeTime(0, Self.m_settings.fallDelay div 60, Self.m_settings.fallDelay mod 60, 0) < Now) then
  begin
   Self.signal := ncStuj;
   Self.m_state.falling := false;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

// aktualizace rychlosti souprav před návěstidlem
// pozor na padání !
// force nucene zastavi vlak, resp. nastavi jeho rychlost
//  metoda je volana s force v pripade, kdy dochazi k prime zmene navesti od uzivatele (STUJ, DN, RC)
procedure TBlkSignal.UpdateRychlostTrain(force: Boolean = false);
var Usek, signal: TBlk;
    train: TTrain;
    signalEv: TBlkSignalTrainEvent;
    i: Integer;
    trat: TBlkTrat;
begin
 if (Self.m_settings.events.Count = 0) then Exit();
 Usek := Self.track;
 if (Self.m_spnl.symbolType = TBlkSignalSymbol.shunting) then Exit();          // pokud jsem posunove navestidlo, koncim funkci
 if ((Usek = nil) or ((Usek.typ <> btUsek) and (Usek.typ <> btTU))) then Exit();    // pokud pred navestidlem neni usek, koncim funkci

 // pokud na useku prede mnou neni souprava, koncim funkci
 if (not (Usek as TBlkUsek).IsTrain()) then
  begin
   // tady musi dojit ke zruseni registrace eventu, kdyby nedoslo, muze se stat,
   // ze za nejakou dobu budou splneny podminky, pro overovani eventu, ale
   // event bude porad bezet -> pokud je casovy, okamzite byse spustil
   if ((Self.m_lastEvIndex >= 0) and (Self.m_lastEvIndex < Self.m_settings.events.Count)) then
     if (Self.m_settings.events[Self.m_lastEvIndex].stop.enabled) then
       Self.m_settings.events[Self.m_lastEvIndex].stop.Unregister();
   Exit();
  end;

 // pokud souprava svym predkem neni na bloku pred navestidlem, koncim funkci
 train := Self.GetTrain(Usek);
 if (train.front <> Usek) then
  begin
   // tady musime zrusit registraci eventu, viz vyse
   if ((Self.m_lastEvIndex >= 0) and (Self.m_lastEvIndex < Self.m_settings.events.Count)) then
     if (Self.m_settings.events[Self.m_lastEvIndex].stop.enabled) then
       Self.m_settings.events[Self.m_lastEvIndex].stop.Unregister();
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
   if ((Self.autoblok) and (TBlkTrat(TBlkTU(Usek).Trat).Smer = TTratSmer.AtoB) and (Self.direction = THVStanoviste.sudy)) then
     Exit();
   if ((Self.autoblok) and (TBlkTrat(TBlkTU(Usek).Trat).Smer = TTratSmer.BtoA) and (Self.direction = THVStanoviste.lichy)) then
     Exit();
  end;

 // zjisteni aktualni udalosti podle typu a delky soupravy
 i := Self.CurrentEventIndex();
 signalEv := Self.m_settings.events[i];

 if (i <> Self.m_lastEvIndex) then
  begin
   // Z nejakeho duvodu reagujeme na novou udalost -> vypnout starou udalost
   if ((Self.m_lastEvIndex >= 0) and (Self.m_lastEvIndex < Self.m_settings.events.Count)) then
    begin
     if (Self.m_settings.events[Self.m_lastEvIndex].stop.enabled) then
       Self.m_settings.events[Self.m_lastEvIndex].stop.Unregister();

     if ((Self.m_settings.events[Self.m_lastEvIndex].slow.enabled) and
         (Self.m_settings.events[Self.m_lastEvIndex].slow.ev.enabled)) then
       Self.m_settings.events[Self.m_lastEvIndex].slow.ev.Unregister();
    end;

   Self.m_lastEvIndex := i;
  end;

 ///////////////////////////////////////////////////

 // ZPOMALOVANI
 if ((signalEv.slow.enabled) and (train.wantedSpeed > signalEv.slow.speed) and
     ((Usek as TBlkUsek).zpomalovani_ready) and
     ((not Self.IsGoSignal()) or (train.IsPOdj(Usek))) and
     (train.direction = Self.m_spnl.direction)) then
  begin
   if (not signalEv.slow.ev.enabled) then
     signalEv.slow.ev.Register();

   if (signalEv.slow.ev.IsTriggerred(Usek, true)) then
    begin
     signalEv.slow.ev.Unregister();
     train.speed := signalEv.slow.speed;
     (Usek as TBlkUsek).zpomalovani_ready := false;
    end;
  end else begin
   if ((signalEv.slow.enabled) and (signalEv.slow.ev.enabled)) then
     signalEv.slow.ev.Unregister();
  end;

 ///////////////////////////////////////////////////

 // ZASTAVOVANI, resp. nastavovani rychlosti prislusne JC
 if (not signalEv.stop.enabled) then
   signalEv.stop.Register();

 if ((signalEv.stop.IsTriggerred(Usek, true)) or (force)) then       // podminka IsRychEvent take resi to, ze usek musi byt obsazeny (tudiz resi vypadek useku)
  begin
   // event se odregistruje automaticky pri zmene

   if ((train.IsPOdj(Usek)) and (train.direction = Self.m_spnl.direction)) then
    begin
     // predvidany odjezd neuplynul -> zastavit soupravu
     if (train.wantedSpeed <> 0) then
       train.SetSpeedDirection(0, Self.m_spnl.direction);

     // souprava je na zastavovaci udalosti -> zacit pocitat cas
     if (not train.GetPOdj(Usek).origin_set) then
      begin
       train.GetPOdj(Usek).RecordOriginNow();
       TBlkUsek(Usek).PropagatePOdjToTrat();
       Usek.Change();
      end;

     Exit();
    end;

   if ((Assigned(Self.DNjc)) and (Self.DNjc.typ = TJCType.vlak)) then
    begin
     // je JC -> je postaveno?
     if ((Self.IsGoSignal()) and (not Self.m_state.falling)) then
      begin
       // je postaveno -> zkontrolujeme, jestli budeme na konci zastavovat
       if ((train.wantedSpeed > 0) and (train.direction <> Self.m_spnl.direction)) then Exit(); // pokud jede souprava opacnym smerem, kaslu na ni

       case (Self.DNjc.data.DalsiNavaznost) of
         TJCNextNavType.blok: begin
           Blky.GetBlkByID(Self.DNjc.data.DalsiNavestidlo, signal);

           if ((signal <> nil) and (signal.typ = btSignal) and (TBlkSignal(signal).IsGoSignal()) and
               (not train.IsPOdj(Self.DNjc.lastUsek))) then
            begin
              // na konci JC budeme stat
              if ((train.wantedSpeed <> Self.DNjc.data.speedGo) or (train.direction <> Self.m_spnl.direction)) then
                train.SetSpeedDirection(Self.DNjc.data.speedGo, Self.m_spnl.direction);
            end else begin
              // na konci JC jedeme dal
              if ((train.wantedSpeed <> Self.DNjc.data.speedStop) or (train.direction <> Self.m_spnl.direction)) then
                train.SetSpeedDirection(Self.DNjc.data.speedStop, Self.m_spnl.direction);
            end;
         end;

         TJCNextNavType.trat: begin
           if ((train.wantedSpeed <> Self.DNjc.data.speedGo) or (train.direction <> Self.m_spnl.direction)) then
             train.SetSpeedDirection(Self.DNjc.data.speedGo, Self.m_spnl.direction);
         end;

         TJCNextNavType.zadna: begin
           if ((train.wantedSpeed <> Self.DNjc.data.speedStop) or (train.direction <> Self.m_spnl.direction)) then
             train.SetSpeedDirection(Self.DNjc.data.speedStop, Self.m_spnl.direction);
          end;
       end;

       // kontrola prehravani stanicniho hlaseni
       train.CheckSh(Self);
      end else begin
       // neni povolovaci navest -> zastavit LOKO
       if ((train.direction = Self.m_spnl.direction) and (train.wantedSpeed <> 0)) then
         train.SetSpeedDirection(0, Self.m_spnl.direction);
      end;
    end else begin
     // nenalezena jizdni cesta -> muze se jednat o navestidlo v autobloku
     if (train.direction = Self.m_spnl.direction) then
      begin
       if ((Self.IsGoSignal()) and (not Self.m_state.falling) and
           (Self.track.typ = btTU) and (TBlkTU(Self.track).InTrat > -1)) then
        begin
         if (Cardinal(train.wantedSpeed) <> TBlkTU(Self.track).Speed(train)) then
           train.SetSpeedDirection(TBlkTU(Self.track).Speed(train), Self.m_spnl.direction)
        end else begin
         //  neni povolovaci navest -> zastavit
         if (train.wantedSpeed <> 0) then
           train.SetSpeedDirection(0, Self.m_spnl.direction);
        end;
      end;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// Vraci udalost, na kterou by se melo reagovat podle aktualniho stavu kolejiste.

function TBlkSignal.CurrentEventIndex(): Integer;
var i: Integer;
    train: TTrain;
    Usek: TBlk;
    event: TBlkSignalTrainEvent;
begin
 if (Self.m_settings.events.Count = 0) then
   raise ENoEvents.Create('No current events!');

 Usek := Self.track;
 if (not (Usek as TBlkUsek).IsTrain()) then
  begin
   // na bloku neni zadna souprava
   Result := 0;
  end else begin
   train := Self.GetTrain(Usek);

   // hledame takovy event, ktery odpovida nasi souprave
   for i := 0 to Self.m_settings.events.Count-1 do
    begin
     event := Self.m_settings.events[i];
     if ((train.length >= event.length.min) and (train.length <= event.length.max) and (event.train_typ_re.Match(train.typ))) then
       Exit(i);
    end;

   // pokud jsme event odpovidajici parametrum soupravy nenasli, vyhodnocujeme globalni event
   Result := 0;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkSignal.IsGoSignal(jctype: TJCType = TJCType.vlak): Boolean;
begin
 if ((Self.signal = ncChanging) and (TBlkSignal.IsGoSignal(Self.m_state.targetSignal, jctype))) then
   // navest se meni na nejakou povolovaci -> ridim se jeste tou starou
   Result := TBlkSignal.IsGoSignal(Self.m_state.signalOld, jctype)
 else
   Result := TBlkSignal.IsGoSignal(Self.signal, jctype);
end;

function TBlkSignal.IsOpakVystraha(): Boolean;
begin
 Result := (Self.targetSignal = ncOpakVystraha) or (Self.targetSignal = ncOpakVystraha40);
end;

class function TBlkSignal.IsGoSignal(Navest: TBlkSignalCode; jctype: TJCType = TJCType.vlak): Boolean;
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

procedure TBlkSignal.UpdatePrivol();
var oblr: TOR;
begin
 if ((Self.m_state.privolStart+EncodeTime(0, _PRIVOL_MIN, _PRIVOL_SEC, 0) < Now+EncodeTime(0, 0, 30, 0)) and
     (Self.m_state.privolTimerId = 0)) then
  begin
   // oznameni o brzkem ukonceni privolavaci navesti
   Self.m_state.privolTimerId := Random(65536)+1;
   for oblr in Self.ORsRef do
    begin
     oblr.BroadcastGlobalData('INFO-TIMER;'+IntToStr(Self.m_state.privolTimerId)+
                              ';0;30; PN '+Self.GlobalSettings.name);
     oblr.TimerCnt := oblr.TimerCnt + 1;
    end;
  end;

 if (Self.m_state.privolStart+EncodeTime(0, _PRIVOL_MIN, _PRIVOL_SEC, 0) < Now) then
  begin
   // pad privolavaci navesti
   Self.signal := ncStuj;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

// privolavaci navest bez podpory zabezpecovaciho zarizeni
procedure TBlkSignal.PrivolDKClick(SenderPnl: TIDContext; SenderOR: TObject; Button: TPanelButton);
var oblr: TOR;
begin
 if (Button = ENTER) then
  begin
   for oblr in Self.stations do
     oblr.ORDKClickClient();
   ORTCPServer.Potvr(SenderPnl, Self.PrivokDKPotvrSekv, SenderOR as TOR, 'Zapnutí přivolávací návěsti',
                     TBlky.GetBlksList(Self), nil);
  end else begin
   if (Button = TPanelButton.F2) then
     ORTCPServer.Menu(SenderPnl, Self, TOR(SenderOR), '$'+TOR(SenderOR).Name + ',-,' + 'KC');
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSignal.PrivokDKPotvrSekv(Sender: TIdContext; success: Boolean);
begin
 if (success) then
  begin
   Self.m_state.selected := TBlkSignalSelection.none;
   Self.signal := ncPrivol;
  end else begin
   self.selected := TBlkSignalSelection.none;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSignal.RNZPotvrSekv(Sender: TIdContext; success: Boolean);
var blkId: Integer;
    blk: TBlk;
    toRNZ: TDictionary<Integer, Cardinal>;
begin
 if (not success) then Exit();

 // nejdriv uvolnime toRNZ -- abychom jej nemazali v DecreaseNouzZaver
 toRNZ := Self.m_state.toRnz;
 Self.m_state.toRnz := TDictionary<Integer, Cardinal>.Create();

 for blkId in toRNZ.Keys do
  begin
   Blky.GetBlkByID(blkId, blk);
   if (blk = nil) then continue;   

   case (blk.typ) of
    btTurnout: begin
       if (TBlkTurnout(blk).emLock) then
         TBlkTurnout(blk).DecreaseEmergencyLock(toRnz[blkId]);
    end;

    btLock: begin
       if (TBlkLock(blk).emLock) then
         TBlkLock(blk).DecreaseNouzZaver(toRnz[blkId]);
    end;
   end;
  end;

 toRNZ.Free();
 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSignal.SetTrackId(new_id: Integer);
begin
 if (Self.m_spnl.trackId = new_id) then Exit();
 Self.m_spnl.trackId := new_id;
 if (new_id = -1) then Self.autoblok := false;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkSignal.GetTrackId(): TBlk;
begin
 if (((Self.m_trackId = nil) and (Self.trackId <> -1)) or ((Self.m_trackId <> nil) and (Self.trackId <> Self.m_trackId.id))) then
   Blky.GetBlkByID(Self.trackId, Self.m_trackId);
 Result := Self.m_trackId;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSignal.AddBlkToRnz(blkId: Integer; change: Boolean = true);
begin
 if (Self.m_state.toRnz.ContainsKey(blkId)) then
   Self.m_state.toRnz[blkId] := Self.m_state.toRnz[blkId] + 1
 else
   Self.m_state.toRnz.Add(blkId, 1);

 if ((Self.m_state.toRnz.Count = 1) and (Self.m_state.toRnz[blkId] = 1) and
     (change)) then
   Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkSignal.CanIDoRNZ(): Boolean;
begin
 Result := Self.m_state.toRnz.Count > 0;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSignal.RemoveBlkFromRnz(blkId: Integer);
begin
 if (Self.m_state.toRnz.ContainsKey(blkId)) then
  begin
   Self.m_state.toRnz.Remove(blkId);
   if (Self.m_state.toRnz.Count = 0) then
     Self.Change();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSignal.UnregisterAllEvents();
var ev: TBlkSignalTrainEvent;
begin
 for ev in Self.m_settings.events do
  begin
   ev.stop.Unregister();
   if ((ev.slow.enabled) and (Assigned(ev.slow.ev))) then
     ev.slow.ev.Unregister();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSignal.GetPtData(json: TJsonObject; includeState: Boolean);
begin
 inherited;

 json['symbolType'] := Integer(Self.m_spnl.symbolType);
 json['track'] := Self.m_spnl.trackId;
 json['direction'] := Integer(Self.m_spnl.direction);

 if (includeState) then
   Self.GetPtState(json['blockState']);
end;

procedure TBlkSignal.GetPtState(json: TJsonObject);
begin
 json['signal'] := Self.signal;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkSignal.GetTrain(usek: TBlk = nil): TTrain;
begin
 if (usek = nil) then
   Blky.GetBlkByID(Self.trackId, usek);

 if (Self.direction = THVStanoviste.lichy) then
   Result := TBlkUsek(usek).trainSudy
 else
   Result := TBlkUsek(usek).trainL;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkSignal.RCinProgress(): Boolean;
begin
 Result := (Self.m_state.RCtimer > -1);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSignal.RCtimerTimeout();
begin
 Self.m_state.RCtimer := -1;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSignal.PropagatePOdjToTrat();
var train: TTrain;
    trat: TBlk;
    podj: TPOdj;
begin
 train := Self.GetTrain();
 if (train = nil) then
  begin
   train := TBlkUsek(Self.track).trainPredict;
   if (train = nil) then Exit();
  end;

 if (Self.DNjc = nil) then Exit();
 if (Self.DNjc.data.Trat = -1) then Exit();
 Blky.GetBlkByID(Self.DNjc.data.Trat, trat);
 if (TBlkTrat(trat).trainPredict = nil) then Exit();
 if (TBlkTrat(trat).trainPredict.train <> train) then Exit();

 if (train.IsPOdj(Self.track)) then
  begin
   podj := train.GetPOdj(Self.track);
   if (not podj.IsDepSet) then Exit();
   if ((TBlkTrat(trat).trainPredict.IsTimeDefined) and (TBlkTrat(trat).trainPredict.time = podj.DepTime())) then Exit();

   TBlkTrat(trat).trainPredict.predict := true;
   TBlkTrat(trat).trainPredict.time := podj.DepTime();
   TBlkTrat(trat).Change();
  end else if ((TBlkTrat(trat).trainPredict.predict) and
               ((not train.IsPOdj(Self.track)) or (not train.GetPOdj(Self.track).IsDepSet()))) then begin
   TBlkTrat(trat).trainPredict.predict := false;
   TBlkTrat(trat).trainPredict.UndefTime();
   TBlkTrat(trat).Change();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkSignal.IsChanging(): Boolean;
begin
 Result := (Self.signal = ncChanging);
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkSignal.PanelStateString(): string;
var fg, bg, okoli: TColor;
begin
 Result := inherited;

 case (Self.selected) of
  TBlkSignalSelection.none : okoli := clBlack;
  TBlkSignalSelection.VC   : okoli := clGreen;
  TBlkSignalSelection.PC   : okoli := clWhite;
  TBlkSignalSelection.NC,
  TBlkSignalSelection.PP   : okoli := clTeal;
 else
  okoli := clBlack;
 end;

 bg := okoli;
 fg := $A0A0A0;

 if (Self.ZAM) then
   case (Self.symbolType) of
    TBlkSignalSymbol.main : fg := clRed;
    TBlkSignalSymbol.shunting : fg := clBlue;
   end;
 if (Self.canRNZ) then
   fg := clTeal;

 case (Self.signal) of
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
                    IntToStr(ownConvert.BoolToInt(Self.signal = ncPrivol)) + ';' +
                    IntToStr(ownConvert.BoolToInt(Self.AB)) + ';' +
                    ownConvert.ColorToStr(okoli);
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkSignal.FourtyKmph(): Boolean;
begin
 Result := (Self.targetSignal = ncVolno40) or (Self.targetSignal = ncVystraha40) or
           (Self.targetSignal = nc40Ocek40) or (Self.targetSignal = ncOpakVystraha40) or
           (Self.targetSignal = ncOpak40Ocek40);
end;

class function TBlkSignal.AddOpak(navest: TBlkSignalCode): TBlkSignalCode;
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

function TBlkSignal.GetTargetSignal(): TBlkSignalCode;
begin
 if (Self.changing) then
   Result := Self.m_state.targetSignal
 else
   Result := Self.m_state.signal;
end;

////////////////////////////////////////////////////////////////////////////////

constructor TBlkSignalTrainEvent.Create();
begin
 inherited;

 Self.train_typ_re := TJclRegEx.Create();
 Self.train_typ_re.Compile('.*', false);
 Self.stop := nil;
 Self.slow.ev := nil;
end;

constructor TBlkSignalTrainEvent.Create(str: string; old: Boolean);
begin
 Self.Create();
 Self.Parse(str, old);
end;

destructor TBlkSignalTrainEvent.Destroy();
begin
 Self.train_typ_re.Free();
 if (Assigned(Self.stop)) then
   Self.stop.Free();
 if (Assigned(Self.slow.ev)) then
   Self.slow.ev.Free();

 inherited;
end;

// format ev1: RychEvent.stop|RychEvent-slow|re:train_typ_regexp|min_delka|max_delka
procedure TBlkSignalTrainEvent.Parse(str: string; old: Boolean);
var sl, sl2: TStrings;
begin
 sl := TStringList.Create();
 sl2 := TStringList.Create();
 ExtractStringsEx(['|'], [], PChar(str), sl);

 Self.train_typ_re.Compile('.*', false);
 try
   Self.length.min := -1;
   Self.length.max := -1;

   if (old) then
     Self.stop := Self.ParseOldRychEvent(sl[0])
   else
     Self.stop := TRREv.Create(sl[0]);

   if ((sl.Count > 1) and (sl[1] <> '') and (LeftStr(sl[1], 2) <> '-1')) then
    begin
     ExtractStringsEx([';'], [], sl[1], sl2);

     Self.slow.enabled := true;
     if (old) then
       Self.slow.ev := Self.ParseOldRychEvent(sl[1])
     else
       Self.slow.ev := TRREv.Create(sl2[0]);

     Self.slow.speed := StrToInt(sl2[sl2.Count-1]);
    end else begin
     Self.slow.enabled := false;
     Self.slow.ev := nil;
    end;

   if (sl.Count > 2) then
    begin
     Self.train_typ_re.Compile(ParseTrainTypes(sl[2]), false);
     Self.length.min := StrToIntDef(sl[3], -1);
     Self.length.max := StrToIntDef(sl[4], -1);
    end;
 finally
   sl.Free();
   sl2.Free();
 end;
end;

class function TBlkSignalTrainEvent.ParseTrainTypes(types: string): string;
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
     Result := RightStr(types, System.Length(types)-3);
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
   Result[System.Length(Result)] := ')';
   Result := Result + '$';
 end;
end;

function TBlkSignalTrainEvent.ToFileStr(short: Boolean = false): string;
begin
 Result := '{' + Self.stop.GetDefStr() + '}|';

 if (Self.slow.enabled) then
   Result := Result + '{{' + Self.slow.ev.GetDefStr() + '};' +
              IntToStr(Self.slow.speed) + '}';
 Result := Result + '|';

 if (not short) then
  begin
   Result := Result + '{re:' + Self.train_typ_re.Pattern + '}|' +
             IntToStr(Self.length.min) + '|' +
             IntToStr(Self.length.max);
  end;
end;

//ziskavani zpomalovacich a zastavovaich dat ze souboru (parsing dat)
//format RychEvent data: textove ulozeny 1 radek, kde jsou data oddelena ";"
// : typ.stop(0=usek;1=ir);
//    pro usek nasleduje: usekid;usekpart;speed;
//    pro ir nasleduje: irid;speed;
class function TBlkSignalTrainEvent.ParseOldRychEvent(str: string): TRREv;
var data: TStrings;
    rrData: TRREvData;
begin
 data := TStringList.Create();

 try
   ExtractStringsEx([';'], [], str, data);

   case (data[0][1]) of
    '0': begin
      // usek
      rrData.typ := TRREvType.rrtUsek;
      rrData.usekState := true;
      rrData.usekPart := StrToInt(data[2]);
    end;//case 0

    '1': begin
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

function TBlkSignal.IsEnabled(): Boolean;
begin
 Result := (Self.signal <> TBlkSignalCode.ncDisabled);
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit
