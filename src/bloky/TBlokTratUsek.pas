unit TBlokTratUsek;

// Definice a obsluha technologickeho bloku Tratovy usek
// Tratovy usek dedi z Useku
// TU = Tratovy usek

{
Jak to funguje:
 - Trat je rozdelena na sekce, kazda sekce obsahuje 1..n useku.
 - Kazda sekce je v jednom smeru kryta prave jednim navestidlem,
   sekce tedy nabyva vyznamu useku autobloku.
 - Pokud mam napriklad trat bez autobloku o 4 usecich, jedna se o jednu
   sekci se 4-mi useky.
 - Sekce jsou smerove zavisle.
 - Sekce jakozto takova neni reprezntovana zadnou specialni tridou,
   je reprezentovana TU, ktery je kryty navestidlem. Takovy TU se nazyva
   "Section Master" \property sectMaster
 - Prvni TU trati v danem smeru je pro tyto ucely vzdy povazovan za Section
   Master (tedy je povazovan za kryty navestidlem).
 - Kazdy TU ma odkaz na sveho Section Master.
 - Kady Section Master ma odkaz na vsechny TU, ktere spadaji do jeho sekce.
 - Nastavovani kryciho navestidla sekce je zodpovednost pouze Section Master.
 - Kazdy TU ma odkaz na vedlejsi TU (sTU, lTU), lTU vzdy bliz zacatku trati.
 - prevTU a nextTU jsou vedlejsi tratove useky v zavislosti na aktualnim
   smeru trati.
 - Nastavovani vazeb TU probiha po nacteni config souboru a pri jakekoliv
   zmene trati. Behem provozu neni doporuceno menit trate.
 - navKryci je odkaz na kryci navestidlo tratoveho useku podle aktualniho
   smeru trati.
 - Rychlost soupravy v trati se meni az za 2 iterace Update od vkroceni
   soupravy do TU. To z toho duvodu, ze souprava mohla zastavit pred navestidlem
   (takovou soupravu nechceme rozjizdet).
   sprRychUpdateIter
 - Pozor na rozdil mezi sectObsazeno a sectReady, sectReady zahrnuje i poruchu
 blokove podminky a tak je pro vetsinu pripadu vhodnejsi.
 - Blokovou podminku rusi TU, nikoliv trat.

--------------------------------------------------------------------------------
}

interface

uses TBlokUsek, Classes, TBlok, IniFiles, SysUtils, IdContext, rrEvent,
      Generics.Collections, TOblRizeni, THnaciVozidlo, Train, JclPCRE;

type
 TBlkTUZastEvents = class                                                       // cidla zastavky v jednom smeru
  zastaveni: TRREv;                                                               // zastavovaci udalost
  zpomaleni: record                                                               // zpomalovaci udalost
    enabled: Boolean;                                                               // povolena zpomalovaci udalost?
    speed: Integer;                                                                 // rychlost z km/h (40, 50, 60...)
    ev: TRREv;                                                                      // udalost
  end;

   constructor Create(); overload;
   constructor Create(ini_tech: TMemIniFile; const section: string; const prefix: string); overload;
   destructor Destroy(); override;

   procedure LoadFromFile(ini_tech: TMemIniFile; const section: string; const prefix: string);
   procedure SaveToFile(ini_tech: TMemIniFile; const section: string; const prefix: string);
 end;

 TBlkTUZastavka = class                                                         // zastavka na TU
  ev_lichy: TBlkTUZastEvents;                                                     // odkaz na zastavovaci a zpomalovaci event v lichem smeru
  ev_sudy: TBlkTUZastEvents;                                                      // odkaz na zastavovaci a zpomalovaci event v sudem smeru
  spr_typ_re: TJclRegEx;                                                          // regexp matchujici typ soupravy
  max_delka: Integer;                                                             // maximalni delka soupravy pro zastaveni v zastavce
  delay: TTime;                                                                   // cas cekani v zastavce

   constructor Create(); overload;
   constructor Create(ini_tech: TMemIniFile; const section: string); overload;
   destructor Destroy(); override;

   procedure LoadFromFile(ini_tech: TMemIniFile; const section: string);
   procedure SaveToFile(ini_tech: TMemIniFile; const section: string);
 end;

 TBlkTUSettings = record                                                        // nastaveni TU
   zastavka: TBlkTUZastavka;                                                      // odkaz na zastavku
   navLid, navSid: Integer;                                                        // odkaz na kryci navestidlo TU v lichem smeru a kryci navestidlo v sudem smeru
                                                                                  // obsahuje id bloku navestidla, pokud neni TU kryty v danem smeru, obsahuje -1
                                                                                  // vyuzivano pro autoblok
   rychlosti: TDictionary<Cardinal, Cardinal>;                                     // rychlost v tratovem useku pro danou tridu prechodnosti; trida prechodnosti 0 je fallback v pripade neexistence zaznamu
 end;

 TBlkTUStav = record                                                            // stav tratoveho useku
  inTrat: Integer;                                                                // tady je ulozeno id bloku trati, v jake se blok nachazi; pokud se nenachazi v trati -> -1

  zast_stopped: Boolean;                                                          // jakmile zastavim soupravu v zastavce, nastavim sem true; pokud souprava jede, je zde false
  zast_run_time: TDateTime;                                                       // tady je ulozen cas, kdy se ma souprava ze zastavky rozjet
  zast_rych: Integer;                                                             // tady si pamatuji, jakou rychlost mela souprava puvodne (mela by to byt tratova, toto je tu pro rozsireni zastavek nejen do trati)
  zast_enabled: Boolean;                                                          // zastavku lze z panelu zapnout a vypnout (v zakladnim stavu je zapla)
  zast_passed: Boolean;                                                           // tady je ulozeno true, pokud souprava zastavku jiz projela
  zast_zpom_ready: Boolean;                                                       // jestli je TU pripraveny ke zpomalovani soupravy v zastavce
  zast_sound_step: Cardinal;                                                      // krok prehravani zvuku
                                                                                    // 0 = pripraveno, 1 = prehrana pistalka vypravciho, 2 = prehrano zavreni dveri, 3 = prehrana houkacka

  bpInBlk: Boolean;                                                               // jestli je v useku zavedena blokova podminka
                                                                                  // bpInBlk = kontroluji obsazeni bloku, pri uvolneni useku bez predani dale vyhlasit poruchu BP
  poruchaBP: Boolean;                                                             // jestli nastala porucha blokove podminky
  sprRychUpdateIter: Integer;                                                      // pocet zbyvajicich iteraci do nastaveni rychlost soupravy
 end;


 // technologicky blok Tratoveho useku
 TBlkTU = class(TBlkUsek)
  private const
   _def_tu_stav: TBlkTUStav = (                                                  // zakladni stav TU
    inTrat: -1;
    zast_stopped : false;
    zast_enabled : true;
    zast_zpom_ready : false;
    zast_sound_step : 0;
    poruchaBP : false;
    sprRychUpdateIter : 0;
   );

  private
   TUSettings: TBlkTUSettings;
   fTUStav: TBlkTUStav;

   fNavKryci, fTrat: TBlk;                                                      // odkaz na kryci navestidla v lichem a sudem smeru
                                                                                // pro pristup k temto blokum pouzivat property bez f, toto jsou pouze pomocne promenne!

    procedure ZastUpdate();                                                     // technologie zastavky (rizeni zastavovani z rozjizdeni vlaku)
    procedure ZastRunTrain();                                                   // zastavit vlak v zastavce
    procedure ZastStopTrain();                                                  // rozjet vlak ze zastavky

    // kliky v menu TU:
    procedure MenuZastClick(SenderPnl: TIdContext; SenderOR: TObject; new_state: Boolean);
    procedure MenuJEDLokClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuRBPClick(SenderPnl: TIdContext; SenderOR: TObject);

    procedure UpdateBP();                                                       // technologie blokove podminky, resi veskere predavani souprav mezi TU a sekcemi TU

    function GetTrat(): TBlk;                                                   // vrati blok trati, ve kterem je TU, viz property \Trat
    function GetNavKryci(): TBlk;                                               // vrati kryci navestidlo TU, jinak nil (pokud blok neni v aktualnim smeru trati kryty zadnym navestidlem)
    function GetNavKryciL(): TBlk;
    function GetNavKryciS(): TBlk;
    function GetTratReady(): Boolean;                                           // vrati, jestli je trat zpusobila prace: jestli existuje a ma smer AtoB nebo BtoA, viz property \tratSmer
    function GetPrevTU(): TBlkTU;                                               // vrati predchozi TU v zavislosti na smeru trati, pokud smer neni AtoB nebo BtoA, vrati nil, viz property \prevTU
    function GetNextTU(): TBlkTU;                                               // vrati dalsi TU v zavislosti na smeru trati, pokud smer neni AtoB nebo BtoA, vrati nil, viz property \nextTU
    function GetSectObsazeno(): TUsekStav;                                      // vrati stav obsazeni cele sekce, mozne volat pouze u Section Masteru
                                                                                // POZOR: tato metoda, resp property \sectObsazeno by mela byt pouzivana VELMI OPATRNE, casto je vhodnejsi property \sectReady, ktera zahrnuje i poruchu BP
    function GetSectMaster(): TBlkTU;                                           // vrati sectMaster kazdeho TU, pokud je TU sam sobe sectMaster, obsahuje referenci na self, viz property \sectMaster
    function GetNextNav(): TBlk;                                                // vrati dalsi navestidlo v trati, v krajnim pripade az hranicni navestidlo cele trati podle aktualniho smeru trati, viz property \nextNav
    function GetSectReady(): Boolean;                                           // vrati, zda-li je sekce pripravena pro vjezd vlaku do ni, viz property \sectReady, mozne volat pouze u sectMaster

    procedure PanelPotvrSekvRBP(Sender: TIdContext; success: Boolean);          // callback potvrzovaci sekvence RBP

    procedure UpdateNavest();                                                   // aktualizuje navest krycich navestidel
    procedure UpdateTrainRych();                                                  // aktualizuje rychlost soupravy v TU (pocita s \sprRychUpdateIter)

    procedure SetRychUpdate(state: Boolean);                                     // nastavi \sprRychUpdateIter
    function GetRychUpdate: Boolean;                                             // vrati, jestli bezi odpocet \sprRychUpdateIter

    function GetReady(): Boolean;                                                // jestli je usek pripraveny na vjeti soupravy

    function IsZastavka(): Boolean;
    function IsZastavkaLichy(): Boolean;
    function IsZastavkaSudy(): Boolean;

    procedure SetPoruchaBP(state: Boolean);                                      // nastavi stav poruchy blokove podminky
    procedure AddTrain(spr: Integer);

  public
   lTU, sTU: TBlkTU;                                                            // reference na tratovy usek blize zacatku trati (lTU) a TU blize konci trati (sTU), tyto refence nastavuje trat pri inicializaci, nebo zmene konfigurace trati

   lsectMaster: TBlkTU;                                                          // sectMaster pro lichy smer trati
   lsectUseky: TList<TBlkTU>;                                                    // pokud jsem sectMaster, zde jsou ulozeny useky me sekce v lichem smeru; pokud nejsem sectMaster, je tento senzam prazdny

   ssectMaster: TBlkTU;                                                          // sectMaster pro sudy smer trati
   ssectUseky: TList<TBlkTU>;                                                    // pokud jsem sectMaster, zde jsou ulozeny useky me sekce v sudem smeru; pokud nejsem sectMaster, je tento senzam prazdny

    constructor Create(index: Integer);
    destructor Destroy(); override;

    function GetSettings(): TBlkTUSettings; overload;
    procedure SetSettings(data: TBlkTUSettings); overload;

    //load/save data
    procedure LoadData(ini_tech: TMemIniFile; const section : string; ini_rel, ini_stat: TMemIniFile); override;
    procedure SaveData(ini_tech: TMemIniFile; const section: string); override;
    procedure SaveStatus(ini_stat: TMemIniFile; const section: string); override;

    procedure Enable(); override;
    procedure Disable(); override;

    procedure Update(); override;
    procedure Change(now: Boolean = false); override;
    procedure ChangeFromTrat();                                                 // aktualizace TU z trati, vola se zejemna pri zmene smeru a jeho ucel je nastavit navestidla autobloku podle smeru trati

    function ShowPanelMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TORCOntrolRights): string; override;
    procedure PanelClick(SenderPnl: TIdContext; SenderOR: TObject; Button: TPanelButton; rights: TORCOntrolRights; params: string = ''); override;
    procedure PanelMenuClick(SenderPnl: TIdContext; SenderOR: TObject; item: string; itemindex: Integer); override;

    procedure CreateNavRefs();                                                  // navestidlum autobloku nastavi UsekPred a smer
    procedure RemoveTURefs();                                                   // zrusi UsekPred navetidlum autobloku

    procedure UvolnenoZJC();                                                    // obsah useku (ne nutne souprava!) byl prevzat z krajniho useku trati jizdni cestou
                                                                                // tato metoda ma smysl pouze pro krajni TU trati a resi radne odstraneni obsahu useku z trati

    procedure AddTrainL(index: Integer); override;
    procedure AddTrainS(index: Integer); override;
    procedure RemoveTrains(); override;
    procedure RemoveTrain(index: Integer); override;

    function Speed(HV: THV): Cardinal; overload;
    function Speed(spr: TTrain): Cardinal; overload;

    // pro vyznam properties viz hlavicky getteru a setteru
    property TUStav: TBlkTUStav read fTUStav;
    property InTrat: Integer read fTUStav.InTrat write fTUStav.InTrat;
    property bpInBlk: Boolean read fTUStav.bpInBlk write fTUStav.bpInBlk;
    property sectObsazeno: TUsekStav read GetSectObsazeno;
    property sectReady: Boolean read GetSectReady;
    property rychUpdate: Boolean read GetRychUpdate write SetRychUpdate;

    property trat: TBlk read GetTrat;
    property navKryci: TBlk read GetNavKryci;
    property navKryciL: TBlk read GetNavKryciL;
    property navKryciS: TBlk read GetNavKryciS;
    property tratReady: Boolean read GetTratReady;

    property prevTU: TBlkTU read GetPrevTU;
    property nextTU: TBlkTU read GetNextTU;
    property sectMaster: TBlkTU read GetSectMaster;
    property nextNav: TBlk read GetNextNav;
    property ready: Boolean read GetReady;
    property poruchaBP: Boolean read fTUStav.poruchaBP write SetPoruchaBP;
    property zastavka: Boolean read IsZastavka;
    property zastavkaLichy: Boolean read IsZastavkaLichy;
    property zastavkaSudy: Boolean read IsZastavkaSudy;

 end;//TBlkUsek


implementation

uses TrainDb, TBloky, TCPServerOR, TBlockRailway, TBlockSignal, TJCDatabase,
     logging, TechnologieJC, ownStrUtils, THVDatabase;

////////////////////////////////////////////////////////////////////////////////

constructor TBlkTU.Create(index: Integer);
begin
 inherited Create(index);

 Self.m_globSettings.typ := btTU;
 Self.fTUStav := _def_tu_stav;

 Self.fTrat := nil;
 Self.fNavKryci := nil;
 Self.lsectMaster := nil;
 Self.lsectUseky := TList<TBlkTU>.Create();
 Self.ssectMaster := nil;
 Self.ssectUseky := TList<TBlkTU>.Create();
 Self.bpInBlk := false;
 Self.TUSettings.rychlosti := TDictionary<Cardinal, Cardinal>.Create();
 Self.TUSettings.zastavka := nil;
end;

destructor TBlkTU.Destroy();
begin
 Self.lsectUseky.Free();
 Self.ssectUseky.Free();
 Self.TUSettings.zastavka.Free();
 Self.TUSettings.rychlosti.Free();

 inherited;
end;//dtor

////////////////////////////////////////////////////////////////////////////////
// nacte konfiguracni data ze souboru

procedure TBlkTU.LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile);
var str: string;
    strs, strs2: TStrings;
begin
 inherited LoadData(ini_tech, section, ini_rel, ini_stat);

 Self.TUSettings.navLid := ini_tech.ReadInteger(section, 'navL', -1);
 Self.TUSettings.navSid := ini_tech.ReadInteger(section, 'navS', -1);

 Self.TUSettings.rychlosti.Clear();
 strs := TStringList.Create();
 strs2 := TStringList.Create();
 try
   str := ini_tech.ReadString(section, 'rychlosti', '');
   if (str = '') then
    begin
     Self.TUSettings.rychlosti.Add(0, ini_tech.ReadInteger(section, 'rychlost', 0));
    end else begin
     ExtractStringsEx([','], [], str, strs);
     for str in strs do
      begin
       strs2.Clear();
       ExtractStringsEx([':'], [], str, strs2);
       if (strs2.Count = 2) then
         Self.TUSettings.rychlosti.AddOrSetValue(StrToInt(strs2[0]), StrToInt(strs2[1]));
      end;
    end;
 finally
   strs.Free();
   strs2.Free();
 end;

 Self.bpInBlk := ini_stat.ReadBool(section, 'bpInBlk', false);

 if ((not Self.TUSettings.rychlosti.ContainsKey(0)) or (Self.TUSettings.rychlosti[0] < 10)) then
   writelog('WARNING: traťový úsek '+Self.name + ' ('+IntToStr(Self.id)+') nemá korektně zadanou traťovou rychlost', WR_ERROR);

 if ((ini_tech.ReadString(section, 'zast_ev_lichy_zast', '') <> '') or
     (ini_tech.ReadString(section, 'zast_ev_sudy_zast', '') <> '')) then
   Self.TUSettings.zastavka := TBlkTUZastavka.Create(ini_tech, section)
 else
   if (Assigned(Self.TUSettings.zastavka)) then
     Self.TUSettings.zastavka.Free();
end;

// ulozi konfiguracni data do souboru
procedure TBlkTU.SaveData(ini_tech: TMemIniFile; const section: string);
var str: string;
    j: Cardinal;
    speeds: TList<Cardinal>;
begin
 inherited SaveData(ini_tech, section);

 if (Self.TUSettings.navLid <> -1) then
   ini_tech.WriteInteger(section, 'navL', Self.TUSettings.navLid);

 if (Self.TUSettings.navSid <> -1) then
   ini_tech.WriteInteger(section, 'navS', Self.TUSettings.navSid);

 speeds := TList<Cardinal>.Create(Self.TUSettings.rychlosti.Keys);
 try
   speeds.Sort();
   str := '';
   for j in speeds do
     str := str + IntToStr(j) + ':' + IntToStr(Self.TUSettings.rychlosti[j]) + ',';
   ini_tech.WriteString(section, 'rychlosti', str);
 finally
   speeds.Free();
 end;

 if (Self.zastavka) then
   Self.TUSettings.zastavka.SaveToFile(ini_tech, section);
end;

// ulozi stavova data do souboru
procedure TBlkTU.SaveStatus(ini_stat: TMemIniFile; const section: string);
begin
 inherited SaveStatus(ini_stat, section);

 if (Self.bpInBlk) then
   ini_stat.WriteBool(section, 'bpInBlk', Self.bpInBlk);
end;

////////////////////////////////////////////////////////////////////////////////
// operace s nastavenim TU

function TBlkTU.GetSettings(): TBlkTUSettings;
begin
 Result := Self.TUSettings;
end;

procedure TBlkTU.SetSettings(data: TBlkTUSettings);
begin
 if (Self.TUSettings.zastavka <> data.zastavka) and (Assigned(Self.TUSettings.zastavka)) then
   Self.TUSettings.zastavka.Free();

 if (Self.TUSettings.rychlosti <> data.rychlosti) then
   Self.TUSettings.rychlosti.Free();

 Self.TUSettings := data;
 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTU.Enable();
var blk: TBlkSignal;
begin
 inherited;
 Self.fTUStav.poruchaBP := false;

 // Aktiaovovat navestidla rucne, aby se rovnou nastavily navesti v trati
 Blky.GetBlkByID(Self.TUSettings.navLid, TBlk(Blk));
 if ((blk <> nil) and (blk.typ = btSignal)) then
   blk.Enable();

 Blky.GetBlkByID(Self.TUSettings.navSid, TBlk(Blk));
 if ((blk <> nil) and (blk.typ = btSignal)) then
   blk.Enable();

 Self.UpdateNavest();
end;

procedure TBlkTU.Disable();
begin
 Self.fTUStav.poruchaBP := false;
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////
// aktualizace stavu bloku

procedure TBlkTU.Update();
begin
 inherited;

 if ((Self.InTrat > -1) and (Self.Stav.Stav = TUsekStav.obsazeno) and (Self.IsTrain()) and (Self.zastavka)) then
   Self.ZastUpdate();

 Self.UpdateTrainRych();
end;

////////////////////////////////////////////////////////////////////////////////
// zmena stavu bloku

procedure TBlkTU.Change(now: Boolean = false);
begin
 inherited;

 // aktualizovat predavani blokove podminky
 Self.UpdateBP();

 // UpdateNavest musi byt volano i u non-master bloku, zajistuje totiz i zhasinani
 //  navestidel v druhem smeru (v druhem smeru muzou byt sectMaster uplne jinak)
 Self.UpdateNavest();

 if (Self.Trat <> nil) then
  begin
   Self.Trat.Change();

   // propagace zmen k masterTU sekce:
   if ((Self.sectMaster <> nil) and (Self.sectMaster <> Self)) then Self.sectMaster.Change();

   // propagace zmen do vedlejsi sekce (napriklad kvuli navesti)
   if ((Self.sectMaster = Self) and (Self.prevTU <> nil) and
       (Self.prevTU.sectMaster <> nil)) then Self.prevTU.sectMaster.Change();
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// aktualizace stavu zastavky

procedure TBlkTU.ZastUpdate();
begin
 if (not Self.TUStav.zast_stopped) then
  begin
   // cekam na obsazeni IR
   if ((not Self.TUStav.zast_enabled) or (Self.TUStav.zast_passed) or
      (Self.train.length > Self.TUSettings.Zastavka.max_delka) or (Self.train.front <> self)) then Exit();

   // kontrola spravneho smeru
   if (((Self.train.direction = THVSTanoviste.lichy) and (not Self.zastavkaLichy)) or
       ((Self.train.direction = THVSTanoviste.sudy) and (not Self.zastavkaSudy))) then Exit();

   // kontrola typu soupravy:
   if (not Self.TUSettings.zastavka.spr_typ_re.Match(Self.train.typ)) then
     Exit();

   // zpomalovani pred zastavkou:
   if (Self.fTUStav.zast_zpom_ready) then
    begin
     case (Self.train.direction) of
      THVSTanoviste.lichy : begin
        if (Self.TUSettings.zastavka.ev_lichy.zpomaleni.enabled) then
         begin
          if (not Self.TUSettings.zastavka.ev_lichy.zpomaleni.ev.enabled) then
            Self.TUSettings.zastavka.ev_lichy.zpomaleni.ev.Register();

          if ((Self.TUSettings.zastavka.ev_lichy.zpomaleni.enabled) and
              (Self.train.wantedSpeed > Self.TUSettings.zastavka.ev_lichy.zpomaleni.speed) and
              (Self.TUSettings.zastavka.ev_lichy.zpomaleni.ev.IsTriggerred(Self, true))) then
           begin
            Self.train.speed := Self.TUSettings.zastavka.ev_lichy.zpomaleni.speed;
            Self.fTUStav.zast_zpom_ready := false;
            Self.rychUpdate := false;
            Self.TUSettings.zastavka.ev_lichy.zpomaleni.ev.Unregister();
           end;
         end;
      end;

      THVSTanoviste.sudy  : begin
        if (Self.TUSettings.zastavka.ev_sudy.zpomaleni.enabled) then
         begin
          if (not Self.TUSettings.zastavka.ev_sudy.zpomaleni.ev.enabled) then
            Self.TUSettings.zastavka.ev_sudy.zpomaleni.ev.Register();

          if ((Self.train.wantedSpeed > Self.TUSettings.zastavka.ev_sudy.zpomaleni.speed) and
              (Self.TUSettings.zastavka.ev_sudy.zpomaleni.ev.IsTriggerred(Self, true))) then
           begin
            Self.train.speed := Self.TUSettings.zastavka.ev_sudy.zpomaleni.speed;
            Self.fTUStav.zast_zpom_ready := false;
            Self.rychUpdate := false;
            Self.TUSettings.zastavka.ev_sudy.zpomaleni.ev.Unregister();
           end;
         end;
      end;
     end;//case
    end;


   // zastavovani v zastavce
   case (Self.train.direction) of
    THVSTanoviste.lichy : begin
      if (not Self.TUSettings.zastavka.ev_lichy.zastaveni.enabled) then
        Self.TUSettings.zastavka.ev_lichy.zastaveni.Register();

      if (Self.TUSettings.zastavka.ev_lichy.zastaveni.IsTriggerred(Self, true)) then
       begin
        Self.ZastStopTrain();
        Self.rychUpdate := false;
        Self.TUSettings.zastavka.ev_lichy.zastaveni.Unregister();
       end;
    end;

    THVSTanoviste.sudy  : begin
      if (not Self.TUSettings.zastavka.ev_sudy.zastaveni.enabled) then
        Self.TUSettings.zastavka.ev_sudy.zastaveni.Register();

      if (Self.TUSettings.zastavka.ev_sudy.zastaveni.IsTriggerred(Self, true)) then
       begin
        Self.ZastStopTrain();
        Self.rychUpdate := false;
        Self.TUSettings.zastavka.ev_sudy.zastaveni.Unregister();
       end;
    end;
   end;//case
  end else begin

   // osetreni rozjeti vlaku z nejakeho pochybneho duvodu
   //  pokud se souprava rozjede, koncim zastavku
   if (Self.train.wantedSpeed <> 0) then
    begin
     Self.fTUStav.zast_stopped := false;
     Self.Change();  // change je dulezite volat kvuli menu
    end;

   // prehravani zvuku pri rozjezdu
   case (Self.TUStav.zast_sound_step) of
    0: begin
      if (Now >= Self.TUStav.zast_run_time - EncodeTime(0, 0, 4, 0)) then
       begin
        Self.fTUStav.zast_sound_step := 1;
        Self.train.ToggleHouk('trubka vlakvedoucího');
       end;
    end;

    1: begin
      if (Now >= Self.TUStav.zast_run_time - EncodeTime(0, 0, 2, 0)) then
       begin
        Self.fTUStav.zast_sound_step := 2;
        Self.train.ToggleHouk('zavření dveří');
       end;
    end;
   end;

   // cekam na timeout na rozjeti vlaku
   if (Now > Self.TUStav.zast_run_time) then
    begin
     Self.train.ToggleHouk('houkačka krátká');
     Self.ZastRunTrain();
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTU.ZastStopTrain();
begin
 Self.fTUStav.zast_stopped  := true;
 Self.fTUStav.zast_sound_step := 0;
 Self.fTUStav.zast_run_time := Now+Self.TUSettings.Zastavka.delay;

 try
   Self.fTUStav.zast_rych := Self.train.speed;
   Self.train.speed := 0;
   Self.train.SetSpeedBuffer(@Self.fTUStav.zast_rych);
 except

 end;

 Self.Change();     // change je dulezite volat kvuli menu
end;

procedure TBlkTU.ZastRunTrain();
begin
 Self.fTUStav.zast_stopped := false;
 Self.fTUStav.zast_passed  := true;
 Self.fTUStav.zast_sound_step := 0;

 try
   Self.train.SetSpeedBuffer(nil);
   Self.train.speed := Self.TUStav.zast_rych;
 except

 end;

 Self.Change();     // change je dulezite volat kvuli menu
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTU.ShowPanelMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TORCOntrolRights): string;
begin
 Result := inherited;

 // zastavka
 if ((Self.InTrat > -1) and (Self.zastavka)) then
  begin
   Result := Result + '-,';
   if (not Self.TUStav.zast_stopped) then
    begin
     // pokud neni v zastavce zastavena souprava, lze zastavku vypinat a zapinat
     case (Self.TUStav.zast_enabled) of
      false : Result := Result + 'ZAST>,';
      true  : Result := Result + 'ZAST<,';
     end;//case
    end else begin
     // pokud v zastavce osuprava stoji, lze ji rozjet
     Result := Result + 'JEĎ vlak,';
    end;
  end;

 if (Self.poruchaBP) then
   Result := Result + '!RBP,';
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTU.PanelClick(SenderPnl: TIdContext; SenderOR: TObject ; Button: TPanelButton; rights: TORCOntrolRights; params: string = '');
var Blk: TBlk;
begin
 case (Button) of
  F2: ORTCPServer.Menu(SenderPnl, Self, (SenderOR as TOR), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));

  ENTER : begin
    if (not Self.MenuKCClick(SenderPnl, SenderOR)) then
    if (not Self.PresunLok(SenderPnl, SenderOR, 0)) then // predpokladame, ze TU muze mit max. 1 soupravu
      ORTCPServer.Menu(SenderPnl, Self, (SenderOR as TOR), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));
  end;

  F1: begin
    Blk := Blky.GetBlkSignalSelected((SenderOR as TOR).id);
    if (Blk = nil) then
      ORTCPServer.Menu(SenderPnl, Self, (SenderOR as TOR), Self.ShowPanelMenu(SenderPnl, SenderOR, rights))
    else
      Self.MenuVBClick(SenderPnl, SenderOR);
  end;
 end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTU.AddTrainL(index: Integer);
begin
 inherited;
 Self.AddTrain(index);
end;

procedure TBlkTU.AddTrainS(index: Integer);
begin
 inherited;
 Self.AddTrain(index);
end;

procedure TBlkTU.AddTrain(spr: Integer);
begin
 if ((Self.zastavka) and (not Self.fTUStav.zast_zpom_ready)) then
   Self.fTUStav.zast_zpom_ready := true;

 // Zmena smeru soupravy muze nastat na zacatku i konci trati
 // tak, aby souprava byla vzdy rizeni spravnymi nasvestidly.
 // Souprava ve smeru A-->B vzdy jeden v lichem smeru

 if ((Self.Trat <> nil) and (TBlkRailway(Self.Trat).GetSettings().trackIds.Count > 0)) then
  begin
   if ((Self.id = TBlkRailway(Self.Trat).GetSettings().trackIds[0])) then
    begin
     if (TBlkRailway(Self.Trat).direction = TRailwayDirection.AtoB) then begin // vjizdim do trati
       if (Self.train.direction <> THVStanoviste.lichy) then
         Self.train.ChangeDirection();
     end else if (TBlkRailway(Self.Trat).direction = TRailwayDirection.BtoA) then begin // vjizdim do posledniho useku ve smeru trati
       if (Self.train.direction <> TBlkSignal(TBlkRailway(Self.Trat).signalA).direction) then
         Self.train.ChangeDirection();
     end;
    end;
   if ((Self.id = TBlkRailway(Self.Trat).GetSettings().trackIds[TBlkRailway(Self.Trat).GetSettings().trackIds.Count-1])) then
    begin
     if (TBlkRailway(Self.Trat).direction = TRailwayDirection.BtoA) then begin // vjizdim do trati
       if ((Self.train.direction <> THVStanoviste.sudy) and (TBlkRailway(Self.Trat).GetSettings().trackIds.Count > 0)) then
         Self.train.ChangeDirection();
     end else if (TBlkRailway(Self.Trat).direction = TRailwayDirection.AtoB) then begin // vjizdim do posledniho useku ve smeru trati
       if (Self.train.direction <> TBlkSignal(TBlkRailway(Self.Trat).signalB).direction) then
         Self.train.ChangeDirection();
     end;
    end;
  end;

 // kontrola zmeny OR trati, ve ktere jen jeden blok
 if (((Self.Trat as TBlkRailway).direction >= TRailwayDirection.AtoB) and (Self.prevTU = nil) and (Self.nextTU = nil)) then
   TBlkRailway(Self.Trat).TrainChangeOR(Self.train);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTU.RemoveTrains();
var spr: Integer;
begin
 for spr in Self.trains do
   Self.RemoveTrain(spr);
end;

procedure TBlkTU.RemoveTrain(index: Integer);
var old_spr: TTrain;
    trat: TBlkRailway;
begin
 old_spr := Self.train;

 inherited;

 if (Self.fTUStav.zast_stopped) then
  begin
   // vlak, ktery oupsti TU a mel by stat v zastavce, je vracen do stavu, kdy se mu nastavuje rychlost
   // toto je pojistka, ke ktere by teoreticky nikdy nemelo dojit
   old_spr.SetSpeedBuffer(nil);
   Self.fTUStav.zast_stopped := false;
  end;

 Self.fTUStav.zast_passed := false;
 Self.fTUStav.zast_zpom_ready := false;

 if (Self.zastavkaLichy) then
  begin
   Self.TUSettings.zastavka.ev_lichy.zastaveni.Unregister();
   if (Self.TUSettings.zastavka.ev_lichy.zpomaleni.enabled) then
     Self.TUSettings.zastavka.ev_lichy.zpomaleni.ev.Unregister();
  end;
 if (Self.zastavkaSudy) then
  begin
   Self.TUSettings.zastavka.ev_sudy.zastaveni.Unregister();
   if (Self.TUSettings.zastavka.ev_sudy.zpomaleni.enabled) then
     Self.TUSettings.zastavka.ev_sudy.zpomaleni.ev.Unregister();
  end;

 // souprava uvolnena z useku, mozna bude nutne ji uvolnit z cele trati
 if (Self.Trat <> nil) then
  begin
   trat := TBlkRailway(Self.Trat);

   // souprava vyjela z trate -> odstranit z trate
   if (not trat.IsTrainInAnyTU(old_spr)) then
     trat.RemoveTrain(old_spr);

   // zavolame uvolneni posledniho TU z jizdni cesty
   Self.UvolnenoZJC();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTU.MenuZastClick(SenderPnl: TIdContext; SenderOR: TObject; new_state: Boolean);
begin
 if (not Self.TUStav.zast_stopped) then
   Self.fTUStav.zast_enabled := new_state;
end;

procedure TBlkTU.MenuJEDLokClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 if (Self.TUStav.zast_stopped) then
   Self.ZastRunTrain();
end;

procedure TBlkTU.MenuRBPClick(SenderPnl: TIdContext; SenderOR: TObject);
var podm: TPSPodminky;
begin
 podm := TPSPodminky.Create();
 if (Self.IsTrain()) then
  begin
   podm.Add(TOR.GetPSPodminka(Self, 'Smazání soupravy '+Self.train.name+' z úseku'));
   if ((Self.Trat <> nil) and (not TBlkRailway(Self.Trat).IsTrainInMoreTUs(Self.train))) then
     podm.Add(TOR.GetPSPodminka(Self.Trat, 'Smazání soupravy '+Self.train.name+' z tratě'));
   if (Blky.GetBlkWithTrain(Self.train).Count = 1) then
     podm.Add(TOR.GetPSPodminka(Self, 'Smazání soupravy '+Self.train.name+' z kolejiště'));
  end;

 ORTCPServer.Potvr(SenderPnl, Self.PanelPotvrSekvRBP, SenderOR as TOR,
                   'Zrušení poruchy blokové podmínky', TBlky.GetBlksList(Self), podm);
end;

procedure TBlkTU.PanelPotvrSekvRBP(Sender: TIdContext; success: Boolean);
var old_train: Integer;
    blks: TList<TObject>;
begin
 if (success) then
  begin
   old_train := Self.trainI;
   Self.bpInBlk   := false;
   Self.poruchaBP := false;
   if (Self.IsTrain()) then
    begin
     Self.RemoveTrains();
     blks := Blky.GetBlkWithTrain(TrainDb.Trains[old_train]);
     if (blks.Count = 0) then TrainDb.Trains.Remove(old_train);
     blks.Free();
    end;

   if (Self.Trat <> nil) then Self.Trat.Change();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTU.PanelMenuClick(SenderPnl: TIdContext; SenderOR: TObject; item: string; itemindex: Integer);
begin
 if (item = 'JEĎ vlak')   then Self.MenuJEDLokClick(SenderPnl, SenderOR)
 else if (item = 'ZAST>') then Self.MenuZastClick(SenderPnl, SenderOR, true)
 else if (item = 'ZAST<') then Self.MenuZastClick(SenderPnl, SenderOR, false)
 else if (item = 'RBP') then Self.MenuRBPClick(SenderPnl, SenderOR)
 else inherited;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTU.GetTrat(): TBlk;
begin
 if (((Self.fTrat = nil) and (Self.TUStav.inTrat <> -1)) or ((Self.fTrat <> nil) and (Self.fTrat.id <> Self.TUStav.inTrat))) then
   Blky.GetBlkByID(Self.TUStav.inTrat, Self.fTrat);
 Result := Self.fTrat;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTU.GetNavKryci(): TBlk;
var navPrevID: Integer;
begin
 if ((Self.Trat = nil) or ((TBlkRailway(Self.Trat).direction <> TRailwayDirection.AtoB) and (TBlkRailway(Self.Trat).direction <> TRailwayDirection.BtoA))) then Exit(nil);

 case (TBlkRailway(Self.Trat).direction) of
   TRailwayDirection.AtoB : navPrevID := Self.TUSettings.navLid;
   TRailwayDirection.BtoA : navPrevID := Self.TUSettings.navSid;
 else
  navPrevID := -1;
 end;

 if (((Self.fNavKryci = nil) and (navPrevID <> -1)) or ((Self.fNavKryci <> nil) and (Self.fNavKryci.id <> navPrevID))) then
   Blky.GetBlkByID(navPrevID, Self.fNavKryci);
 Result := Self.fNavKryci;
end;

function TBlkTU.GetNavKryciL(): TBlk;
begin
 Blky.GetBlkByID(Self.TUSettings.navLid, Result);
end;

function TBlkTU.GetNavKryciS(): TBlk;
begin
 Blky.GetBlkByID(Self.TUSettings.navSid, Result);
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTU.GetTratReady(): Boolean;
begin
 Result := ((Self.Trat <> nil) and ((TBlkRailway(Self.Trat).direction = TRailwayDirection.AtoB) or (TBlkRailway(Self.Trat).direction = TRailwayDirection.BtoA)));
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTU.GetPrevTU(): TBlkTU;
begin
 if (not Self.tratReady) then Exit(nil);

 case (TBlkRailway(Self.Trat).direction) of
   TRailwayDirection.AtoB : Result := Self.lTU;
   TRailwayDirection.BtoA : Result := Self.sTU;
 else
  Result := nil;
 end;
end;

function TBlkTU.GetNextTU(): TBlkTU;
begin
 if (not Self.tratReady) then Exit(nil);

 case (TBlkRailway(Self.Trat).direction) of
   TRailwayDirection.AtoB : Result := Self.sTU;
   TRailwayDirection.BtoA : Result := Self.lTU;
 else
  Result := nil;
 end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTU.UpdateBP();
begin
 if ((not Self.tratReady) or (not TBlkRailway(Self.Trat).BP)) then Exit();

 if ((Self.prevTU = nil) and (Self.Obsazeno = TUsekStav.obsazeno)) then
  begin
   // nastala aktivace blokove podminky prvniho bloku trati
   Self.bpInBlk := true;
  end;

 // predavani soupravy z predchoziho TU do meho TU
 if ((Self.prevTU <> nil) and (Self.Obsazeno = TUsekStav.obsazeno) and
     (Self.prevTU.Obsazeno = TUsekStav.obsazeno) and
     ((Self.navKryci = nil) or (TBlkSignal(Self.navKryci).IsGoSignal()))) then
  begin
   // nastala aktivace blokove podminky
   Self.bpInBlk := true;

   if ((not Self.IsTrain()) and (Self.prevTU.IsTrain())) then
    begin
     // mezi useky je potreba predat soupravu
     Self.prevTU.train.front := Self;
     Self.AddTrainL(Self.prevTU.trainI);
     Self.zpomalovani_ready := true;
     Self.houk_ev_enabled := true;
     Self.rychUpdate := true;

     if (Self.nextTU = nil) then
      begin
       // souprava vstoupila do posledniho bloku trati
       // zmena stanic soupravy a hnacich vozidel v ni
       TBlkRailway(Self.Trat).TrainChangeOR(Self.train);
      end;
    end;//if predavam soupravu
  end;

 // uvolnovani soupravy z TU (pokud je jiz predana do dalsiho TU)
 if ((Self.bpInBlk) and (Self.nextTU <> nil) and (Self.nextTU.train = Self.train) and
     (Self.Obsazeno = TusekStav.Uvolneno) and (Self.nextTU.Obsazeno = TUsekStav.obsazeno)) then
  begin
   Self.bpInBlk := false;
   Self.RemoveTrains();
   if (not TBlkRailway(Self.Trat).occupied) then TBlkRailway(Self.Trat).BP := false;
  end;

 // kontrola poruchy blokove podminky
 if (Self.bpInBlk) then
  begin
   if ((Self.Obsazeno = TUsekStav.uvolneno) and (not Self.poruchaBP) and
       ((Self.Zaver = TZaver.no) or (Self.Zaver = TZaver.ab))) then
     Self.poruchaBP := true;
   if ((Self.Obsazeno = TUsekStav.obsazeno) and (Self.poruchaBP)) then
     Self.poruchaBP := false;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTU.GetSectObsazeno(): TUsekStav;
var blk: TBlkTU;
    sectUseky: TList<TBlkTU>;
begin
 if (Self.Trat = nil) then Exit(TusekStav.none);
 if (Self.Obsazeno <= TUsekStav.none) then Exit(TUsekStav.Obsazeno);

 // pozadavek na obsazenost sekce muze prijit i kdyz trat nema smer,
 //  typicky kdyz se stavi JC do bezsouhlasove trati s automatickou
 //  zmenou souhlasu
 // -> pro krajni useky trti vracime obsazenost prvni sekce

 case (TBlkRailway(Self.Trat).direction) of
  TRailwayDirection.AtoB : sectUseky := Self.lsectUseky;
  TRailwayDirection.BtoA : sectUseky := Self.ssectUseky;
  TRailwayDirection.no : begin
         if (Self.sTU = nil) then
          sectUseky := Self.ssectUseky
         else begin
           if (Self.lTU = nil) then
            sectUseky := Self.lsectUseky
           else
            Exit(TUsekStav.none);
         end;
  end;
 else
  Exit(TUsekStav.none);
 end;

 for blk in sectUseky do
  if (blk.Obsazeno <> TUsekStav.uvolneno) then Exit(blk.Obsazeno);
 Exit(TUsekStav.uvolneno);
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTU.GetSectMaster(): TBlkTU;
begin
 if (Self.Trat = nil) then Exit(nil);
 case (TBlkRailway(Self.Trat).direction) of
  TRailwayDirection.AtoB : Result := Self.lsectMaster;
  TRailwayDirection.BtoA : Result := Self.ssectMaster;
 else
  Result := nil;
 end;
end;

////////////////////////////////////////////////////////////////////////////////
// vrati dalsi navestidlo v trati (pokud ma trat smer)
// pokud neni dalsi navestidlo autobloku, vrati hranicni navestidlo trati

function TBlkTU.GetNextNav(): TBlk;
var blk: TBLkTU;
begin
 if (Self.Trat = nil) or (TBlkRailway(Self.Trat).direction = TRailwayDirection.no) then Exit(nil);

 blk := Self.nextTU;
 while ((blk <> nil) and (blk.sectMaster <> blk)) do
   blk := blk.nextTU;

 if (blk <> nil) then
   Exit(blk.navKryci)
 else begin
  case (TBlkRailway(Self.Trat).direction) of
    TRailwayDirection.AtoB : Exit(TBlkRailway(Self.Trat).signalB);
    TRailwayDirection.BtoA : Exit(TBlkRailway(Self.Trat).signalA);
  end;
 end;

 Result := nil;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTU.CreateNavRefs();
var Blk: TBlk;
begin
 Blky.GetBlkByID(Self.TUSettings.navLid, Blk);
 if ((Blk <> nil) and (Blk.typ = btSignal) and (Self.lTU <> nil)) then
  begin
   TBlkSignal(Blk).trackId := Self.lTU.id;
   TBlkSignal(Blk).direction := THVStanoviste.lichy;
   TBlkSignal(Blk).autoblok := true;
  end;

 Blky.GetBlkByID(Self.TUSettings.navSid, Blk);
 if ((Blk <> nil) and (Blk.typ = btSignal) and (Self.sTU <> nil)) then
  begin
   TBlkSignal(Blk).trackId := Self.sTU.id;
   TBlkSignal(Blk).direction := THVStanoviste.sudy;
   TBlkSignal(Blk).autoblok := true;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTU.RemoveTURefs();
var Blk: TBlk;
begin
 Self.lTU         := nil;
 Self.sTU         := nil;
 Self.lsectMaster := nil;
 Self.lsectUseky.Clear();
 Self.ssectMaster := nil;
 Self.ssectUseky.Clear();
 Self.bpInBlk     := false;
 Self.InTrat      := -1;

 Blky.GetBlkByID(Self.TUSettings.navLid, Blk);
 if ((Blk <> nil) and (Blk.typ = btSignal)) then TBlkSignal(Blk).trackId := -1;
 Blky.GetBlkByID(Self.TUSettings.navSid, Blk);
 if ((Blk <> nil) and (Blk.typ = btSignal)) then TBlkSignal(Blk).trackId := -1;
end;

////////////////////////////////////////////////////////////////////////////////
// Tato metoda nastavuje kryci navestidlo sekce a zaroven kontroluje
// zruseni navesti do trati v pripade nahleho obsazeni prvni sekce trati.

procedure TBlkTU.UpdateNavest();
var Blk: TBlk;
    jc: TJC;
begin
 // kontrola zruseni navesti jizdni cesty pri obsazeni sekce trati:
 // tato metoda je volana vzdy pouze u sectMastera (tj. krajniho bloku sekce)
 if (Self.Trat = nil) then Exit();

 // NASTAVOVANI NAVESTI AUTOBLOKU:

 // nejprve zhasneme navestidla v nespravnem smeru
 if ((TBlkRailway(Self.Trat).direction = TRailwayDirection.AtoB) or (TBlkRailway(Self.Trat).direction = TRailwayDirection.no)) then
  begin
   if (Self.TUSettings.navSid > -1) then
    begin
     Blky.GetBlkByID(Self.TUSettings.navSid, Blk);
     if (Blk <> nil) then
       TBlkSignal(Blk).signal := TBlkSignalCode(TBlkRailway(Self.Trat).SignalCounterDirection());
    end;
  end;

 if ((TBlkRailway(Self.Trat).direction = TRailwayDirection.BtoA) or (TBlkRailway(Self.Trat).direction = TRailwayDirection.no)) then
  begin
   if (Self.TUSettings.navLid > -1) then
    begin
     Blky.GetBlkByID(Self.TUSettings.navLid, Blk);
     if (Blk <> nil) then
       TBlkSignal(Blk).signal := TBlkSignalCode(TBlkRailway(Self.Trat).SignalCounterDirection());
    end;
  end;

  if (TBlkRailway(Self.Trat).direction = TRailwayDirection.no) then
    Exit();

 // zrusit jizdni cestu muzeme pouze u sekce na kraji trati (v trati se rusi
 //   navest autobloku)
 if ((Self.prevTU = nil) and (Self.sectObsazeno = TUsekStav.obsazeno)
     and (TBlkRailway(Self.Trat).Zaver)) then
  begin
   jc := JCDb.FindPostavenaJCWithUsek(Self.id);
   if ((jc <> nil) and (not jc.waitForLastUsekOrTratObsaz) and (jc.stav.RozpadBlok < jc.data.Useky.Count-1)) then
     JCDb.RusJC(Self);
  end;

 // nastavime kryci navestidlo
 if ((Self.navKryci <> nil) and (not TBlkSignal(Self.navKryci).ZAM) and
     (TBlkSignal(Self.navKryci).signal >= ncStuj)) then
  begin
   if (not Self.sectReady) then
    begin
     // sekce obsazena -> navestidlo na STUJ
     TBlkSignal(Self.navKryci).signal := ncStuj
    end else begin
     // sekce uvolnena -> hledame dalsi navestidlo
     case (TBlkRailway(Self.Trat).signals) of
       TRailwaySignals.hradlo: TBlkSignal(Self.navKryci).signal := ncVolno;
       TRailwaySignals.autoblok: begin
         if ((Self.nextNav = nil) or (not TBlkSignal(Self.nextNav).IsGoSignal()) or (TBlkSignal(Self.nextNav).IsOpakVystraha())) then
           TBlkSignal(Self.navKryci).signal := ncVystraha
         else if ((TBlkSignal(Self.nextNav).FourtyKmph()) or (TBlkSignal(Self.nextNav).signal = ncOpakOcek40)) then
           TBlkSignal(Self.navKryci).signal := ncOcek40
         else
           TBlkSignal(Self.navKryci).signal := ncVolno;
       end;
     end;
    end;
  end;

end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTU.ChangeFromTrat();
begin
 Self.UpdateNavest();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTU.UpdateTrainRych();
begin
 if (Self.fTUStav.sprRychUpdateIter > 0) then
  begin
   Dec(Self.fTUStav.sprRychUpdateIter);
   if (Self.fTUStav.sprRychUpdateIter = 0) then
    begin
     if ((Self.IsTrain()) and (Self.zpomalovani_ready) and
        (Self.train.wantedSpeed > 0) and
        (Cardinal(Self.train.wantedSpeed) <> Self.Speed(Self.train))) then
       Self.train.speed := Self.Speed(Self.train);
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTU.GetRychUpdate: Boolean;
begin
 Result := (Self.fTUStav.sprRychUpdateIter > 0);
end;

procedure TBlkTU.SetRychUpdate(state: Boolean);
begin
 if ((state) and (Self.fTUStav.sprRychUpdateIter = 0)) then Self.fTUStav.sprRychUpdateIter := 2;
 if ((not state) and (Self.fTUStav.sprRychUpdateIter > 0)) then Self.fTUStav.sprRychUpdateIter := 0;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTU.UvolnenoZJC();
var railway: TBlkRailway;
begin
 Self.fTUStav.zast_stopped := false;
 Self.fTUStav.zast_passed  := false;

 if (Self.Trat = nil) then Exit();
 railway := TBlkRailway(Self.Trat);

 // zrusime potencialni poruchu blokove podminky a blokovou podminku
 Self.bpInBlk   := false;
 Self.poruchaBP := false;

 if (((railway.GetSettings().rType = TRailwayType.request))
     and (not railway.Zaver) and (not railway.occupied) and (not railway.RBPCan) and
         (railway.state.trains.Count = 0) and (not railway.emLock)) then
  railway.direction := TRailwayDirection.no;

 // pokud je trat uplne volna, zrusime blokovou podminku
 if (not railway.occupied) then railway.BP := false;

 railway.UpdateTrainPredict();
 railway.Change();
end;

////////////////////////////////////////////////////////////////////////////////
// komtrola volnosti sekce pro prijezd soupravy: musi byt splneno
//  1) zadny usek sekce neni obsazen
//  2) vsechny useky sekce jsou bez soupravy

function TBlkTU.GetSectReady(): Boolean;
var blk: TBlkTU;
    sectUseky: TList<TBlkTU>;
begin
 if ((Self.Trat = nil) or (Self.Obsazeno <= TUsekStav.none)) then Exit(false);

 case (TBlkRailway(Self.Trat).direction) of
  TRailwayDirection.AtoB : sectUseky := Self.lsectUseky;
  TRailwayDirection.BtoA : sectUseky := Self.ssectUseky;
  TRailwayDirection.no : begin
         if (Self.sTU = nil) then
          sectUseky := Self.ssectUseky
         else begin
           if (Self.lTU = nil) then
            sectUseky := Self.lsectUseky
           else
            Exit(false);
         end;
  end;
 else
  Exit(false);
 end;

 { zaver u prvniho bloku trati nekontrolujeme, protoze tuto metodu vyuziva JC
   pri staveni, ktera na prvni usek dava nouzovy zaver pri staveni }
 for blk in sectUseky do
   if ((blk.Obsazeno <> TUsekStav.uvolneno) or (blk.IsTrain())
    or (blk.poruchaBP)) then Exit(false);
 Result := true;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTU.GetReady(): Boolean;
begin
 Result := ((Self.Obsazeno = TUsekStav.uvolneno) and (not Self.IsTrain())
  and (not Self.poruchaBP));
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTU.SetPoruchaBP(state: Boolean);
begin
 if (Self.poruchaBP <> state) then
  begin
   Self.fTUStav.poruchaBP := state;
   Self.Change();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTU.Speed(HV: THV): Cardinal;
begin
 if (Self.TUSettings.rychlosti.ContainsKey(HV.data.transience)) then
   Result := Self.TUSettings.rychlosti[HV.data.transience]
 else if (Self.TUSettings.rychlosti.ContainsKey(0)) then
   Result := Self.TUSettings.rychlosti[0]
 else
   Result := 0;
end;

function TBlkTU.Speed(spr: TTrain): Cardinal;
var addr: Word;
    minSpeed: Cardinal;
begin
 if (spr.HVs.Count = 0) then
   Exit(0);

 minSpeed := Self.Speed(HVDb[spr.HVs[0]]);
 for addr in spr.HVs do
   if (Self.Speed(HVDb[addr]) < minSpeed) then
     minSpeed := Self.Speed(HVDb[addr]);
 Result := minSpeed;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTU.IsZastavka(): Boolean;
begin
 Result := (Self.TUSettings.zastavka <> nil);
end;

function TBlkTU.IsZastavkaLichy(): Boolean;
begin
 Result := Self.zastavka and Assigned(Self.TUSettings.zastavka.ev_lichy);
end;

function TBlkTU.IsZastavkaSudy(): Boolean;
begin
 Result := Self.zastavka and Assigned(Self.TUSettings.zastavka.ev_sudy);
end;

////////////////////////////////////////////////////////////////////////////////

constructor TBlkTUZastavka.Create();
begin
 inherited;

 Self.spr_typ_re := TJclRegEx.Create();
 Self.spr_typ_re.Compile('.*', false);
 Self.ev_lichy := nil;
 Self.ev_sudy := nil;
end;

constructor TBlkTUZastavka.Create(ini_tech: TMemIniFile; const section: string);
begin
 Self.Create();
 Self.LoadFromFile(ini_tech, section);
end;

destructor TBlkTUZastavka.Destroy();
begin
 Self.spr_typ_re.Free();
 if (Assigned(Self.ev_lichy)) then
   Self.ev_lichy.Free();
 if (Assigned(Self.ev_sudy)) then
   Self.ev_sudy.Free();

 inherited;
end;

procedure TBlkTUZastavka.LoadFromFile(ini_tech: TMemIniFile; const section: string);
var str: string;
begin
 str := ini_tech.ReadString(section, 'zast_ev_lichy_zast', '');
 try
   if (str <> '') then
     Self.ev_lichy := TBlkTUZastEvents.Create(ini_tech, section, 'zast_ev_lichy')
   else
     if (Assigned(Self.ev_lichy)) then
       Self.ev_lichy.Free();
 except
   Self.ev_lichy := nil;
 end;

 try
   str := ini_tech.ReadString(section, 'zast_ev_sudy_zast', '');
   if (str <> '') then
     Self.ev_sudy := TBlkTUZastEvents.Create(ini_tech, section, 'zast_ev_sudy')
   else
     if (Assigned(Self.ev_sudy)) then
       Self.ev_sudy.Free();
 except
   Self.ev_sudy := nil;
 end;

 Self.max_delka := ini_tech.ReadInteger(section, 'zast_max_delka', 0);
 Self.delay := StrToTime(ini_tech.ReadString(section, 'zast_delay', '00:20'));
 Self.spr_typ_re.Compile(TBlkSignalTrainEvent.ParseTrainTypes(ini_tech.ReadString(section, 'zast_soupravy', '')), false);
end;

procedure TBlkTUZastavka.SaveToFile(ini_tech: TMemIniFile; const section: string);
begin
 ini_tech.WriteInteger(section, 'zast_max_delka', Self.max_delka);
 ini_tech.WriteString(section, 'zast_delay', TimeToStr(Self.delay));
 ini_tech.WriteString(section, 'zast_soupravy', 're:'+Self.spr_typ_re.Pattern);
 if (Self.ev_lichy <> nil) then
   Self.ev_lichy.SaveToFile(ini_tech, section, 'zast_ev_lichy');
 if (Self.ev_sudy <> nil) then
   Self.ev_sudy.SaveToFile(ini_tech, section, 'zast_ev_sudy');
end;

////////////////////////////////////////////////////////////////////////////////

constructor TBlkTUZastEvents.Create();
begin
 inherited;

 Self.zastaveni := nil;
 Self.zpomaleni.ev := nil;
end;

constructor TBlkTUZastEvents.Create(ini_tech: TMemIniFile; const section: string; const prefix: string);
begin
 Self.Create();
 Self.LoadFromFile(ini_tech, section, prefix);
end;

destructor TBlkTUZastEvents.Destroy();
begin
 if (Assigned(Self.zastaveni)) then
   Self.zastaveni.Free();
 if (Assigned(Self.zpomaleni.ev)) then
   Self.zpomaleni.ev.Free();

 inherited;
end;

procedure TBlkTUZastEvents.LoadFromFile(ini_tech: TMemIniFile; const section: string; const prefix: string);
var str: string;
begin
 Self.zastaveni := TRREv.Create(ini_tech.ReadString(section, prefix+'_zast', ''));

 str := ini_tech.ReadString(section, prefix+'_zpom_ev', '');
 Self.zpomaleni.enabled := (str <> '');
 if (Self.zpomaleni.enabled) then
  begin
   Self.zpomaleni.ev := TRREv.Create(str);
   Self.zpomaleni.speed := ini_tech.ReadInteger(section, prefix+'_zpom_sp', 40);
  end;
end;

procedure TBlkTUZastEvents.SaveToFile(ini_tech: TMemIniFile; const section: string; const prefix: string);
begin
 ini_tech.WriteString(section, prefix+'_zast', Self.zastaveni.GetDefStr());
 if (Self.zpomaleni.enabled) then
  begin
   ini_tech.WriteString(section, prefix+'_zpom_ev', Self.zpomaleni.ev.GetDefStr());
   ini_tech.WriteInteger(section, prefix+'_zpom_sp', Self.zpomaleni.speed);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

end.//unitq
