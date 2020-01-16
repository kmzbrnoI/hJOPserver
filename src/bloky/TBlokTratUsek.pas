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
      Generics.Collections, TOblRizeni;

type
 TBlkTUZastEvents = record                                                      // cidla zastavky v jednom smeru
  enabled: boolean;                                                               // jestli je zastavka v danem smeru povolena
  zastaveni: TRREv;                                                               // zastavovaci udalost
  zpomaleni: record                                                               // zpomalovaci udalost
    enabled: boolean;                                                               // povolena zpomalovaci udalost?
    speed: Integer;                                                                 // rychlost z km/h (40, 50, 60...)
    ev: TRREv;                                                                      // udalost
  end;
 end;

 TBlkTUZastavka = record                                                        // zastavka na TU
  ev_lichy:TBlkTUZastEvents;                                                      // odkaz na zastavovaci a zpomalovaci event v lichem smeru
  ev_sudy:TBlkTUZastEvents;                                                       // odkaz na zastavovaci a zpomalovaci event v sudem smeru
  soupravy:TStrings;                                                              // typy souprav, pro ktere je zastavka
  max_delka:Integer;                                                              // maximalni delka soupravy pro zastaveni v zastavce
  delay:TTime;                                                                    // cas cekani v zastavce
 end;

 TBlkTUSettings = record                                                        // nastaveni TU
   zastavka:TBlkTUZastavka;                                                       // odkaz na zastavku
   navLid,navSid:Integer;                                                         // odkaz na kryci navestidlo TU v lichem smeru a kryci navestidlo v sudem smeru
                                                                                  // obsahuje id bloku navestidla, pokud neni TU kryty v danem smeru, obsahuje -1
                                                                                  // vyuzivano pro autoblok
   rychlost:Integer;                                                              // rychlost v tratovem useku
 end;

 TBlkTUStav = record                                                            // stav tratoveho useku
  inTrat:Integer;                                                                 // tady je ulozeno id bloku trati, v jake se blok nachazi; pokud se nenachazi v trati -> -1

  zast_stopped:boolean;                                                           // jakmile zastavim soupravu v zastavce, nastavim sem true; pokud souprava jede, je zde false
  zast_run_time:TDateTime;                                                        // tady je ulozen cas, kdy se ma souprava ze zastavky rozjet
  zast_rych:Integer;                                                              // tady si pamatuji, jakou rychlost mela souprava puvodne (mela by to byt tratova, toto je tu pro rozsireni zastavek nejen do trati)
  zast_enabled:boolean;                                                           // zastavku lze z panelu zapnout a vypnout (v zakladnim stavu je zapla)
  zast_passed:boolean;                                                            // tady je ulozeno true, pokud souprava zastavku jiz projela
  zast_zpom_ready:boolean;                                                        // jestli je TU pripraveny ke zpomalovani soupravy v zastavce
  zast_sound_step:Cardinal;                                                       // krok prehravani zvuku
                                                                                    // 0 = pripraveno, 1 = prehrana pistalka vypravciho, 2 = prehrano zavreni dveri, 3 = prehrana houkacka

  bpInBlk:boolean;                                                                // jestli je v useku zavedena blokova podminka
                                                                                  // bpInBlk = kontroluji obsazeni bloku, pri uvolneni useku bez predani dale vyhlasit poruchu BP
  poruchaBP:boolean;                                                              // jestli nastala porucha blokove podminky
  sprRychUpdateIter:Integer;                                                      // pocet zbyvajicich iteraci do nastaveni rychlost soupravy
 end;


 // technologicky blok Tratoveho useku
 TBlkTU = class(TBlkUsek)
  private const
   _def_tu_stav:TBlkTUStav = (                                                  // zakladni stav TU
    inTrat: -1;
    zast_stopped : false;
    zast_enabled : true;
    zast_zpom_ready : false;
    zast_sound_step : 0;
    poruchaBP : false;
    sprRychUpdateIter : 0;
   );

   _def_tu_zastavka:TBlkTUZastavka = (                                          // zakladni stav zastavky
    soupravy : nil;
    max_delka : 0;
   );

  private
   TUSettings:TBlkTUSettings;
   fTUStav:TBlkTUStav;

   fNavKryci, fTrat : TBlk;                                                     // odkaz na kryci navestidla v lichem a sudem smeru
                                                                                // pro pristup k temto blokum pouzivat property bez f, toto jsou pouze pomocne promenne!

    procedure ZastUpdate();                                                     // technologie zastavky (rizeni zastavovani z rozjizdeni vlaku)
    procedure ZastRunTrain();                                                   // zastavit vlak v zastavce
    procedure ZastStopTrain();                                                  // rozjet vlak ze zastavky

    // kliky v menu TU:
    procedure MenuZastClick(SenderPnl:TIdContext; SenderOR:TObject; new_state:boolean);
    procedure MenuJEDLokClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuRBPClick(SenderPnl:TIdContext; SenderOR:TObject);

    procedure UpdateBP();                                                       // technologie blokove podminky, resi veskere predavani souprav mezi TU a sekcemi TU

    function GetTrat():TBlk;                                                    // vrati blok trati, ve kterem je TU, viz property \Trat
    function GetNavKryci():TBlk;                                                // vrati kryci navestidlo TU, jinak nil (pokud blok neni v aktualnim smeru trati kryty zadnym navestidlem)
    function GetNavKryciL():TBlk;
    function GetNavKryciS():TBlk;
    function GetTratReady():boolean;                                            // vrati, jestli je trat zpusobila prace: jestli existuje a ma smer AtoB nebo BtoA, viz property \tratSmer
    function GetPrevTU():TBlkTU;                                                // vrati predchozi TU v zavislosti na smeru trati, pokud smer neni AtoB nebo BtoA, vrati nil, viz property \prevTU
    function GetNextTU():TBlkTU;                                                // vrati dalsi TU v zavislosti na smeru trati, pokud smer neni AtoB nebo BtoA, vrati nil, viz property \nextTU
    function GetSectObsazeno():TUsekStav;                                       // vrati stav obsazeni cele sekce, mozne volat pouze u Section Masteru
                                                                                // POZOR: tato metoda, resp property \sectObsazeno by mela byt pouzivana VELMI OPATRNE, casto je vhodnejsi property \sectReady, ktera zahrnuje i poruchu BP
    function GetSectMaster():TBlkTU;                                            // vrati sectMaster kazdeho TU, pokud je TU sam sobe sectMaster, obsahuje referenci na self, viz property \sectMaster
    function GetNextNav():TBlk;                                                 // vrati dalsi navestidlo v trati, v krajnim pripade az hranicni navestidlo cele trati podle aktualniho smeru trati, viz property \nextNav
    function GetSectReady():boolean;                                            // vrati, zda-li je sekce pripravena pro vjezd vlaku do ni, viz property \sectReady, mozne volat pouze u sectMaster

    procedure PanelPotvrSekvRBP(Sender:TIdContext; success:boolean);            // callback potvrzovaci sekvence RBP

    procedure UpdateNavest();                                                   // aktualizuje navest krycich navestidel
    procedure UpdateSprRych();                                                  // aktualizuje rychlost soupravy v TU (pocita s \sprRychUpdateIter)

    procedure SetRychUpdate(state:boolean);                                     // nastavi \sprRychUpdateIter
    function GetRychUpdate:boolean;                                             // vrati, jestli bezi odpocet \sprRychUpdateIter

    function GetReady():boolean;                                                // jestli je usek pripraveny na vjeti soupravy

    procedure SetPoruchaBP(state:boolean);                                      // nastavi stav poruchy blokove podminky
    procedure AddSouprava(spr:Integer);

  public
   lTU, sTU: TBlkTU;                                                            // reference na tratovy usek blize zacatku trati (lTU) a TU blize konci trati (sTU), tyto refence nastavuje trat pri inicializaci, nebo zmene konfigurace trati

   lsectMaster:TBlkTU;                                                          // sectMaster pro lichy smer trati
   lsectUseky:TList<TBlkTU>;                                                    // pokud jsem sectMaster, zde jsou ulozeny useky me sekce v lichem smeru; pokud nejsem sectMaster, je tento senzam prazdny

   ssectMaster:TBlkTU;                                                          // sectMaster pro sudy smer trati
   ssectUseky:TList<TBlkTU>;                                                    // pokud jsem sectMaster, zde jsou ulozeny useky me sekce v sudem smeru; pokud nejsem sectMaster, je tento senzam prazdny

    constructor Create(index:Integer);
    destructor Destroy(); override;

    function GetSettings():TBlkTUSettings; overload;
    procedure SetSettings(data:TBlkTUSettings); overload;

    //load/save data
    procedure LoadData(ini_tech:TMemIniFile;const section:string;ini_rel,ini_stat:TMemIniFile); override;
    procedure SaveData(ini_tech:TMemIniFile;const section:string); override;
    procedure SaveStatus(ini_stat:TMemIniFile;const section:string); override;

    procedure Enable(); override;
    procedure Disable(); override;

    procedure Update(); override;
    procedure Change(now:boolean = false); override;
    procedure ChangeFromTrat();                                                 // aktualizace TU z trati, vola se zejemna pri zmene smeru a jeho ucel je nastavit navestidla autobloku podle smeru trati

    function ShowPanelMenu(SenderPnl:TIdContext; SenderOR:TObject; rights:TORCOntrolRights):string; override;
    procedure PanelClick(SenderPnl:TIdContext; SenderOR:TObject; Button:TPanelButton; rights:TORCOntrolRights; params:string = ''); override;
    procedure PanelMenuClick(SenderPnl:TIdContext; SenderOR:TObject; item:string; itemindex:Integer); override;

    procedure CreateNavRefs();                                                  // navestidlum autobloku nastavi UsekPred a smer
    procedure RemoveTURefs();                                                   // zrusi UsekPred navetidlum autobloku

    procedure UvolnenoZJC();                                                    // obsah useku (ne nutne souprava!) byl prevzat z krajniho useku trati jizdni cestou
                                                                                // tato metoda ma smysl pouze pro krajni TU trati a resi radne odstraneni obsahu useku z trati

    procedure AddSoupravaL(index:Integer); override;
    procedure AddSoupravaS(index:Integer); override;
    procedure RemoveSoupravy(); override;
    procedure RemoveSouprava(index:Integer); override;

    // pro vyznam properties viz hlavicky getteru a setteru
    property TUStav:TBlkTUStav read fTUStav;
    property InTrat:Integer read fTUStav.InTrat write fTUStav.InTrat;
    property bpInBlk:boolean read fTUStav.bpInBlk write fTUStav.bpInBlk;
    property sectObsazeno:TUsekStav read GetSectObsazeno;
    property sectReady:boolean read GetSectReady;
    property rychUpdate:boolean read GetRychUpdate write SetRychUpdate;

    property Trat:TBlk read GetTrat;
    property navKryci:TBlk read GetNavKryci;
    property navKryciL:TBlk read GetNavKryciL;
    property navKryciS:TBlk read GetNavKryciS;
    property tratReady:boolean read GetTratReady;

    property prevTU:TBlkTU read GetPrevTU;
    property nextTU:TBlkTU read GetNextTU;
    property sectMaster:TBlkTU read GetSectMaster;
    property nextNav:TBlk read GetNextNav;
    property ready:boolean read GetReady;
    property poruchaBP:boolean read fTUStav.poruchaBP write SetPoruchaBP;

 end;//TBlkUsek


implementation

uses SprDb, TBloky, TCPServerOR, TBlokTrat, TBlokNav, TJCDatabase, Prevody,
     logging, THnaciVozidlo;

////////////////////////////////////////////////////////////////////////////////

constructor TBlkTU.Create(index:Integer);
begin
 inherited Create(index);

 Self.GlobalSettings.typ := _BLK_TU;
 Self.fTUStav := _def_tu_stav;

 Self.fTrat        := nil;
 Self.fNavKryci    := nil;
 Self.lsectMaster  := nil;
 Self.lsectUseky   := TList<TBlkTU>.Create();
 Self.ssectMaster  := nil;
 Self.ssectUseky   := TList<TBlkTU>.Create();
 Self.bpInBlk      := false;

 Self.TUSettings.zastavka.ev_lichy.zastaveni := nil;
 Self.TUSettings.zastavka.ev_lichy.zpomaleni.ev := nil;
 Self.TUSettings.zastavka.ev_sudy.zastaveni := nil;
 Self.TUSettings.zastavka.ev_sudy.zpomaleni.ev := nil;
end;//ctor

destructor TBlkTU.Destroy();
begin
 Self.lsectUseky.Free();
 Self.ssectUseky.Free();
 Self.TUSettings.Zastavka.soupravy.Free();

 if (Assigned(Self.TUSettings.zastavka.ev_lichy.zastaveni)) then
   Self.TUSettings.zastavka.ev_lichy.zastaveni.Free();
 if (Assigned(Self.TUSettings.zastavka.ev_lichy.zpomaleni.ev)) then
   Self.TUSettings.zastavka.ev_lichy.zpomaleni.ev.Free();

 if (Assigned(Self.TUSettings.zastavka.ev_sudy.zastaveni)) then
   Self.TUSettings.zastavka.ev_sudy.zastaveni.Free();
 if (Assigned(Self.TUSettings.zastavka.ev_sudy.zpomaleni.ev)) then
   Self.TUSettings.zastavka.ev_sudy.zpomaleni.ev.Free();

 inherited;
end;//dtor

////////////////////////////////////////////////////////////////////////////////
// nacte konfiguracni data ze souboru

procedure TBlkTU.LoadData(ini_tech:TMemIniFile;const section:string;ini_rel,ini_stat:TMemIniFile);
var zastLichy, zastSudy, str:string;
begin
 inherited LoadData(ini_tech, section, ini_rel, ini_stat);

 Self.TUSettings.navLid := ini_tech.ReadInteger(section, 'navL', -1);
 Self.TUSettings.navSid := ini_tech.ReadInteger(section, 'navS', -1);

 Self.TUSettings.rychlost := ini_tech.ReadInteger(section, 'rychlost', -1);
 Self.bpInBlk := ini_stat.ReadBool(section, 'bpInBlk', false);

 if (Self.TUSettings.rychlost < 10) then
   writelog('WARNING: traťový úsek '+Self.name + ' ('+IntToStr(Self.id)+') nemá korektně zadanou traťovou rychlost', WR_ERROR);

 // nacitani zastavky
 if (Assigned(Self.TUSettings.Zastavka.soupravy)) then Self.TUSettings.Zastavka.soupravy.Free();
 Self.TUSettings.Zastavka := _def_tu_zastavka;
 Self.TUSettings.Zastavka.soupravy := TStringList.Create();

 zastLichy := ini_tech.ReadString(section, 'zast_ev_lichy_zast', '');
 Self.TUsettings.Zastavka.ev_lichy.enabled := (zastLichy <> '');

 zastSudy := ini_tech.ReadString(section, 'zast_ev_sudy_zast', '');
 Self.TUsettings.Zastavka.ev_sudy.enabled := (zastSudy <> '');

 Self.TUsettings.Zastavka.max_delka := ini_tech.ReadInteger(section, 'zast_max_delka', 0);
 Self.TUsettings.Zastavka.delay     := StrToTime(ini_tech.ReadString(section, 'zast_delay', '00:20'));

 Self.TUsettings.Zastavka.soupravy.Clear();
 ExtractStrings([';'],[],PChar(ini_tech.ReadString(section, 'zast_soupravy', '')), Self.TUsettings.Zastavka.soupravy);

 // zastavka v lichem smeru
 if (Self.TUsettings.Zastavka.ev_lichy.enabled) then
  begin
   try
     Self.TUsettings.Zastavka.ev_lichy.zastaveni := TRREv.Create(zastLichy);

     str := ini_tech.ReadString(section, 'zast_ev_lichy_zpom_ev', '');
     Self.TUsettings.Zastavka.ev_lichy.zpomaleni.enabled := (str <> '');
     if (Self.TUsettings.Zastavka.ev_lichy.zpomaleni.enabled) then
      begin
       Self.TUsettings.Zastavka.ev_lichy.zpomaleni.ev := TRREv.Create(str);
       Self.TUsettings.Zastavka.ev_lichy.zpomaleni.speed := ini_tech.ReadInteger(section, 'zast_ev_lichy_zpom_sp', 40);
      end;
   except
     Self.TUsettings.Zastavka.ev_lichy.enabled := false;
   end;
  end;

 // zastavka v sudem smeru
 if (Self.TUsettings.Zastavka.ev_sudy.enabled) then
  begin
   try
     Self.TUsettings.Zastavka.ev_sudy.zastaveni := TRREv.Create(zastSudy);

     str := ini_tech.ReadString(section, 'zast_ev_sudy_zpom_ev', '');
     Self.TUsettings.Zastavka.ev_sudy.zpomaleni.enabled := (str <> '');
     if (Self.TUsettings.Zastavka.ev_sudy.zpomaleni.enabled) then
      begin
       Self.TUsettings.Zastavka.ev_sudy.zpomaleni.ev := TRREv.Create(str);
       Self.TUsettings.Zastavka.ev_sudy.zpomaleni.speed := ini_tech.ReadInteger(section, 'zast_ev_sudy_zpom_sp', 40);
      end;
   except
     Self.TUsettings.Zastavka.ev_sudy.enabled := false;
   end;
  end;
end;

// ulozi konfiguracni data do souboru
procedure TBlkTU.SaveData(ini_tech:TMemIniFile;const section:string);
var str:string;
    i:Integer;
begin
 inherited SaveData(ini_tech, section);

 if (Self.TUSettings.navLid <> -1) then
   ini_tech.WriteInteger(section, 'navL', Self.TUSettings.navLid);

 if (Self.TUSettings.navSid <> -1) then
   ini_tech.WriteInteger(section, 'navS', Self.TUSettings.navSid);

 ini_tech.WriteInteger(section, 'rychlost', Self.TUSettings.rychlost);

 // ukladani zastavky
 if (Self.TUsettings.Zastavka.ev_lichy.enabled) then
  begin
   ini_tech.WriteString(section, 'zast_ev_lichy_zast', Self.TUsettings.Zastavka.ev_lichy.zastaveni.GetDefStr());
   if (Self.TUsettings.Zastavka.ev_lichy.zpomaleni.enabled) then
    begin
     ini_tech.WriteString(section, 'zast_ev_lichy_zpom_ev', Self.TUsettings.Zastavka.ev_lichy.zpomaleni.ev.GetDefStr());
     ini_tech.WriteInteger(section, 'zast_ev_lichy_zpom_sp', Self.TUsettings.Zastavka.ev_lichy.zpomaleni.speed);
    end;
  end;

 if (Self.TUsettings.Zastavka.ev_sudy.enabled) then
  begin
   ini_tech.WriteString(section, 'zast_ev_sudy_zast', Self.TUsettings.Zastavka.ev_sudy.zastaveni.GetDefStr());
   if (Self.TUsettings.Zastavka.ev_sudy.zpomaleni.enabled) then
    begin
     ini_tech.WriteString(section, 'zast_ev_sudy_zpom_ev', Self.TUsettings.Zastavka.ev_sudy.zpomaleni.ev.GetDefStr());
     ini_tech.WriteInteger(section, 'zast_ev_sudy_zpom_sp', Self.TUsettings.Zastavka.ev_sudy.zpomaleni.speed);
    end;
  end;

 if ((Self.TUsettings.Zastavka.ev_lichy.enabled) or ((Self.TUsettings.Zastavka.ev_sudy.enabled))) then
  begin
   ini_tech.WriteInteger(section, 'zast_max_delka', Self.TUsettings.Zastavka.max_delka);
   ini_tech.WriteString(section, 'zast_delay', TimeToStr(Self.TUsettings.Zastavka.delay));

   str := '';
   for i := 0 to Self.TUsettings.Zastavka.soupravy.Count-1 do
    str := str + Self.TUsettings.Zastavka.soupravy[i] + ';';
   ini_tech.WriteString(section, 'zast_soupravy', str);
  end;
end;

// ulozi stavova data do souboru
procedure TBlkTU.SaveStatus(ini_stat:TMemIniFile;const section:string);
begin
 inherited SaveStatus(ini_stat, section);

 if (Self.bpInBlk) then
   ini_stat.WriteBool(section, 'bpInBlk', Self.bpInBlk);
end;

////////////////////////////////////////////////////////////////////////////////
// operace s nastavenim TU

function TBlkTU.GetSettings():TBlkTUSettings;
begin
 Result := Self.TUSettings;
end;

procedure TBlkTU.SetSettings(data:TBlkTUSettings);
begin
 if (Self.TUSettings.zastavka.ev_lichy.zastaveni <> data.zastavka.ev_lichy.zastaveni) then
   Self.TUSettings.zastavka.ev_lichy.zastaveni.Free();

 if (Self.TUSettings.zastavka.ev_sudy.zastaveni <> data.zastavka.ev_sudy.zastaveni) then
   Self.TUSettings.zastavka.ev_sudy.zastaveni.Free();

 if ((Assigned(Self.TUSettings.zastavka.ev_lichy.zpomaleni.ev)) and
     (Self.TUSettings.zastavka.ev_lichy.zpomaleni.ev <> data.zastavka.ev_lichy.zpomaleni.ev)) then
   Self.TUSettings.zastavka.ev_lichy.zpomaleni.ev.Free();

 if ((Assigned(Self.TUSettings.zastavka.ev_sudy.zpomaleni.ev)) and
     (Self.TUSettings.zastavka.ev_sudy.zpomaleni.ev <> data.zastavka.ev_sudy.zpomaleni.ev)) then
   Self.TUSettings.zastavka.ev_sudy.zpomaleni.ev.Free();

 if (Self.TUSettings.Zastavka.soupravy <> data.Zastavka.soupravy) then
  Self.TUSettings.Zastavka.soupravy.Free();

 Self.TUSettings := data;
 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTU.Enable();
var blk:TBlkNav;
begin
 inherited;
 Self.fTUStav.poruchaBP := false;

 // Aktiaovovat navestidla rucne, aby se rovnou nastavily navesti v trati
 Blky.GetBlkByID(Self.TUSettings.navLid, TBlk(Blk));
 if ((blk <> nil) and (blk.typ = _BLK_NAV)) then
   blk.Enable();

 Blky.GetBlkByID(Self.TUSettings.navSid, TBlk(Blk));
 if ((blk <> nil) and (blk.typ = _BLK_NAV)) then
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

 if ((Self.InTrat > -1) and (Self.Stav.Stav = TUsekStav.obsazeno) and (Self.IsSouprava()) and
     ((Self.TUSettings.Zastavka.ev_lichy.enabled) or (Self.TUSettings.Zastavka.ev_sudy.enabled))) then
   Self.ZastUpdate();

 Self.UpdateSprRych();
end;

////////////////////////////////////////////////////////////////////////////////
// zmena stavu bloku

procedure TBlkTU.Change(now:boolean = false);
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
var i:Integer;
    found:boolean;
begin
 if (not Self.TUStav.zast_stopped) then
  begin
   // cekam na obsazeni IR
   if ((not Self.TUStav.zast_enabled) or (Self.TUStav.zast_passed) or
      (Soupravy[Self.Souprava].delka > Self.TUSettings.Zastavka.max_delka) or (Soupravy[Self.Souprava].front <> self)) then Exit();

   // kontrola spravneho smeru
   if (((Soupravy[Self.Souprava].smer = THVSTanoviste.lichy) and (not Self.TUSettings.zastavka.ev_lichy.enabled)) or
       ((Soupravy[Self.Souprava].smer = THVSTanoviste.sudy) and (not Self.TUSettings.zastavka.ev_sudy.enabled))) then Exit();

   // kontrola typu soupravy:
   found := false;
   for i := 0 to Self.TUSettings.Zastavka.soupravy.Count-1 do
    begin
     if (Self.TUSettings.Zastavka.soupravy[i] = Soupravy[Self.Souprava].typ) then
      begin
       found := true;
       break;
      end;
    end;

   if (not found) then Exit();

   // zpomalovani pred zastavkou:
   if (Self.fTUStav.zast_zpom_ready) then
    begin
     case (Soupravy[Self.Souprava].smer) of
      THVSTanoviste.lichy : begin
        if (Self.TUSettings.zastavka.ev_lichy.zpomaleni.enabled) then
         begin
          if (not Self.TUSettings.zastavka.ev_lichy.zpomaleni.ev.enabled) then
            Self.TUSettings.zastavka.ev_lichy.zpomaleni.ev.Register();

          if ((Self.TUSettings.zastavka.ev_lichy.zpomaleni.enabled) and
              (Soupravy[Self.Souprava].chtenaRychlost > Self.TUSettings.zastavka.ev_lichy.zpomaleni.speed) and
              (Self.TUSettings.zastavka.ev_lichy.zpomaleni.ev.IsTriggerred(Self, true))) then
           begin
            Soupravy[Self.Souprava].rychlost := Self.TUSettings.zastavka.ev_lichy.zpomaleni.speed;
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

          if ((Soupravy[Self.Souprava].chtenaRychlost > Self.TUSettings.zastavka.ev_sudy.zpomaleni.speed) and
              (Self.TUSettings.zastavka.ev_lichy.zpomaleni.ev.IsTriggerred(Self, true))) then
           begin
            Soupravy[Self.Souprava].rychlost := Self.TUSettings.zastavka.ev_sudy.zpomaleni.speed;
            Self.fTUStav.zast_zpom_ready := false;
            Self.rychUpdate := false;
            Self.TUSettings.zastavka.ev_sudy.zpomaleni.ev.Unregister();
           end;
         end;
      end;
     end;//case
    end;


   // zastavovani v zastavce
   case (Soupravy[Self.Souprava].smer) of
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
   if (Soupravy[Self.Souprava].chtenaRychlost <> 0) then
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
        Soupravy[Self.Souprava].ToggleHouk('trubka vlakvedoucího');
       end;
    end;

    1: begin
      if (Now >= Self.TUStav.zast_run_time - EncodeTime(0, 0, 2, 0)) then
       begin
        Self.fTUStav.zast_sound_step := 2;
        Soupravy[Self.Souprava].ToggleHouk('zavření dveří');
       end;
    end;
   end;

   // cekam na timeout na rozjeti vlaku
   if (Now > Self.TUStav.zast_run_time) then
    begin
     Soupravy[Self.Souprava].ToggleHouk('houkačka krátká');
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
   Self.fTUStav.zast_rych := Soupravy[Self.Souprava].rychlost;
   Soupravy[Self.Souprava].rychlost := 0;
   Soupravy[Self.Souprava].SetSpeedBuffer(@Self.fTUStav.zast_rych);
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
   Soupravy[Self.Souprava].SetSpeedBuffer(nil);
   Soupravy[Self.Souprava].rychlost := Self.TUStav.zast_rych;
 except

 end;

 Self.Change();     // change je dulezite volat kvuli menu
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTU.ShowPanelMenu(SenderPnl:TIdContext; SenderOR:TObject; rights:TORCOntrolRights):string;
begin
 Result := inherited;

 // zastavka
 if ((Self.InTrat > -1) and
     ((Self.TUSettings.Zastavka.ev_lichy.enabled) or (Self.TUSettings.Zastavka.ev_sudy.enabled))) then
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

procedure TBlkTU.PanelClick(SenderPnl:TIdContext; SenderOR:TObject ;Button:TPanelButton; rights:TORCOntrolRights; params:string = '');
var Blk:TBlk;
begin
 if (Self.Stav.Stav <= TUsekStav.none) then Exit();

 case (Button) of
  F2: ORTCPServer.Menu(SenderPnl, Self, (SenderOR as TOR), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));

  ENTER : begin
    if (not Self.MenuKCClick(SenderPnl, SenderOR)) then
    if (not Self.PresunLok(SenderPnl, SenderOR, 0)) then // predpokladame, ze TU muze mit max. 1 soupravu
      ORTCPServer.Menu(SenderPnl, Self, (SenderOR as TOR), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));
  end;

  F1: begin
    Blk := Blky.GetBlkNavZacatekVolba((SenderOR as TOR).id);
    if (Blk = nil) then
      ORTCPServer.Menu(SenderPnl, Self, (SenderOR as TOR), Self.ShowPanelMenu(SenderPnl, SenderOR, rights))
    else
      Self.MenuVBClick(SenderPnl, SenderOR);
  end;
 end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTU.AddSoupravaL(index: Integer);
begin
 inherited;
 Self.AddSouprava(index);
end;

procedure TBlkTU.AddSoupravaS(index: Integer);
begin
 inherited;
 Self.AddSouprava(index);
end;

procedure TBlkTU.AddSouprava(spr:Integer);
begin
 if (((Self.TUSettings.zastavka.ev_lichy.enabled) or (Self.TUSettings.zastavka.ev_sudy.enabled)) and
     (not Self.fTUStav.zast_zpom_ready)) then Self.fTUStav.zast_zpom_ready := true;

 // Zmena smeru soupravy muze nastat na zacatku i konci trati
 // tak, aby souprava byla vzdy rizeni spravnymi nasvestidly.
 // Souprava ve smeru A-->B vzdy jeden v lichem smeru

 if ((Self.Trat <> nil) and (TBlkTrat(Self.Trat).GetSettings().Useky.Count > 0)) then
  begin
   if ((Self.id = TBlkTrat(Self.Trat).GetSettings().Useky[0])) then
    begin
     if (TBlkTrat(Self.Trat).Smer = TTratSmer.AtoB) then begin // vjizdim do trati
       if (Soupravy[Self.Souprava].smer <> THVStanoviste.lichy) then
         Soupravy[Self.Souprava].ChangeSmer();
     end else if (TBlkTrat(Self.Trat).Smer = TTratSmer.BtoA) then begin // vjizdim do posledniho useku ve smeru trati
       if (Soupravy[Self.Souprava].smer <> TBlkNav(TBlkTrat(Self.Trat).navLichy).Smer) then
         Soupravy[Self.Souprava].ChangeSmer();
     end;
    end;
   if ((Self.id = TBlkTrat(Self.Trat).GetSettings().Useky[TBlkTrat(Self.Trat).GetSettings().Useky.Count-1])) then
    begin
     if (TBlkTrat(Self.Trat).Smer = TTratSmer.BtoA) then begin // vjizdim do trati
       if ((Soupravy[Self.Souprava].smer <> THVStanoviste.sudy) and (TBlkTrat(Self.Trat).GetSettings().Useky.Count > 0)) then
         Soupravy[Self.Souprava].ChangeSmer();
     end else if (TBlkTrat(Self.Trat).Smer = TTratSmer.AtoB) then begin // vjizdim do posledniho useku ve smeru trati
       if (Soupravy[Self.Souprava].smer <> TBlkNav(TBlkTrat(Self.Trat).navSudy).Smer) then
         Soupravy[Self.Souprava].ChangeSmer();
     end;
    end;
  end;

 // kontrola zmeny OR trati, ve ktere jen jeden blok
 if (((Self.Trat as TBlkTrat).Smer >= TTratSmer.AtoB) and (Self.prevTU = nil) and (Self.nextTU = nil)) then
   TBlkTrat(Self.Trat).SprChangeOR(Self.Souprava);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTU.RemoveSoupravy();
var spr:Integer;
begin
 for spr in Self.Soupravs do
   Self.RemoveSouprava(spr);
end;

procedure TBlkTU.RemoveSouprava(index:Integer);
var old_spr:Integer;
    trat:TBlkTrat;
begin
 old_spr := Self.Souprava;

 inherited;

 if (Self.fTUStav.zast_stopped) then
  begin
   // vlak, ktery oupsti TU a mel by stat v zastavce, je vracen do stavu, kdy se mu nastavuje rychlost
   // toto je pojistka, ke ktere by teoreticky nikdy nemelo dojit
   Soupravy[old_spr].SetSpeedBuffer(nil);
   Self.fTUStav.zast_stopped := false;
  end;

 Self.fTUStav.zast_passed     := false;
 Self.fTUStav.zast_zpom_ready := false;

 if (Self.TUSettings.zastavka.ev_lichy.enabled) then
   Self.TUSettings.zastavka.ev_lichy.zastaveni.Unregister();
 if (Self.TUSettings.zastavka.ev_sudy.enabled) then
   Self.TUSettings.zastavka.ev_sudy.zastaveni.Unregister();
 if (Self.TUSettings.zastavka.ev_lichy.zpomaleni.enabled) then
   Self.TUSettings.zastavka.ev_lichy.zpomaleni.ev.Unregister();
 if (Self.TUSettings.zastavka.ev_sudy.zpomaleni.enabled) then
   Self.TUSettings.zastavka.ev_sudy.zpomaleni.ev.Unregister();

 // souprava uvolnena z useku, mozna bude nutne ji uvolnit z cele trati
 if (Self.Trat <> nil) then
  begin
   trat := TBlkTrat(Self.Trat);

   // souprava vyjela z trate -> odstranit z trate
   if (not trat.IsSprInAnyTU(old_spr)) then
     trat.RemoveSpr(old_spr);

   // zavolame uvolneni posledniho TU z jizdni cesty
   Self.UvolnenoZJC();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTU.MenuZastClick(SenderPnl:TIdContext; SenderOR:TObject; new_state:boolean);
begin
 if (not Self.TUStav.zast_stopped) then
   Self.fTUStav.zast_enabled := new_state;
end;

procedure TBlkTU.MenuJEDLokClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 if (Self.TUStav.zast_stopped) then
   Self.ZastRunTrain();
end;

procedure TBlkTU.MenuRBPClick(SenderPnl:TIdContext; SenderOR:TObject);
var podm:TPSPodminky;
begin
 podm := TPSPodminky.Create();
 if (Self.IsSouprava()) then
  begin
   podm.Add(TOR.GetPSPodminka(Self, 'Smazání soupravy '+Soupravy[Self.Souprava].nazev+' z úseku'));
   if ((Self.Trat <> nil) and (not TBlkTrat(Self.Trat).IsSprInMoreTUs(Self.Souprava))) then
     podm.Add(TOR.GetPSPodminka(Self.Trat, 'Smazání soupravy '+Soupravy[Self.Souprava].nazev+' z tratě'));
   if (Blky.GetBlkWithSpr(Self.Souprava).Count = 1) then
     podm.Add(TOR.GetPSPodminka(Self, 'Smazání soupravy '+Soupravy[Self.Souprava].nazev+' z kolejiště'));
  end;

 ORTCPServer.Potvr(SenderPnl, Self.PanelPotvrSekvRBP, SenderOR as TOR,
                   'Zrušení poruchy blokové podmínky', TBlky.GetBlksList(Self), podm);
end;

procedure TBlkTU.PanelPotvrSekvRBP(Sender:TIdContext; success:boolean);
var old_spr:Integer;
    blks:TList<TObject>;
begin
 if (success) then
  begin
   old_spr := Self.Souprava;
   Self.bpInBlk   := false;
   Self.poruchaBP := false;
   if (Self.IsSouprava()) then
    begin
     Self.RemoveSoupravy();
     blks := Blky.GetBlkWithSpr(old_spr);
     if (blks.Count = 0) then Soupravy.RemoveSpr(old_spr);
     blks.Free();
    end;

   if (Self.Trat <> nil) then Self.Trat.Change();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTU.PanelMenuClick(SenderPnl:TIdContext; SenderOR:TObject; item:string; itemindex:Integer);
begin
 if (Self.Stav.Stav <= TUsekStav.none) then Exit();

 if (item = 'JEĎ vlak')   then Self.MenuJEDLokClick(SenderPnl, SenderOR)
 else if (item = 'ZAST>') then Self.MenuZastClick(SenderPnl, SenderOR, true)
 else if (item = 'ZAST<') then Self.MenuZastClick(SenderPnl, SenderOR, false)
 else if (item = 'RBP') then Self.MenuRBPClick(SenderPnl, SenderOR)
 else inherited;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTU.GetTrat():TBlk;
begin
 if (((Self.fTrat = nil) and (Self.TUStav.inTrat <> -1)) or ((Self.fTrat <> nil) and (Self.fTrat.id <> Self.TUStav.inTrat))) then
   Blky.GetBlkByID(Self.TUStav.inTrat, Self.fTrat);
 Result := Self.fTrat;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTU.GetNavKryci():TBlk;
var navPrevID:Integer;
begin
 if ((Self.Trat = nil) or ((TBlkTrat(Self.Trat).Smer <> TTratSmer.AtoB) and (TBlkTrat(Self.Trat).Smer <> TTratSmer.BtoA))) then Exit(nil);

 case (TBlkTrat(Self.Trat).Smer) of
   TTratSmer.AtoB : navPrevID := Self.TUSettings.navLid;
   TTratSmer.BtoA : navPrevID := Self.TUSettings.navSid;
 else
  navPrevID := -1;
 end;

 if (((Self.fNavKryci = nil) and (navPrevID <> -1)) or ((Self.fNavKryci <> nil) and (Self.fNavKryci.id <> navPrevID))) then
   Blky.GetBlkByID(navPrevID, Self.fNavKryci);
 Result := Self.fNavKryci;
end;

function TBlkTU.GetNavKryciL():TBlk;
begin
 Blky.GetBlkByID(Self.TUSettings.navLid, Result);
end;

function TBlkTU.GetNavKryciS():TBlk;
begin
 Blky.GetBlkByID(Self.TUSettings.navSid, Result);
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTU.GetTratReady():boolean;
begin
 Result := ((Self.Trat <> nil) and ((TBlkTrat(Self.Trat).Smer = TTratSmer.AtoB) or (TBlkTrat(Self.Trat).Smer = TTratSmer.BtoA)));
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTU.GetPrevTU():TBlkTU;
begin
 if (not Self.tratReady) then Exit(nil);

 case (TBlkTrat(Self.Trat).Smer) of
   TTratSmer.AtoB : Result := Self.lTU;
   TTratSmer.BtoA : Result := Self.sTU;
 else
  Result := nil;
 end;
end;

function TBlkTU.GetNextTU():TBlkTU;
begin
 if (not Self.tratReady) then Exit(nil);

 case (TBlkTrat(Self.Trat).Smer) of
   TTratSmer.AtoB : Result := Self.sTU;
   TTratSmer.BtoA : Result := Self.lTU;
 else
  Result := nil;
 end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTU.UpdateBP();
begin
 if ((not Self.tratReady) or (not TBlkTrat(Self.Trat).BP)) then Exit();

 if ((Self.prevTU = nil) and (Self.Obsazeno = TUsekStav.obsazeno)) then
  begin
   // nastala aktivace blokove podminky prvniho bloku trati
   Self.bpInBlk := true;
  end;

 // predavani soupravy z predchoziho TU do meho TU
 if ((Self.prevTU <> nil) and (Self.Obsazeno = TUsekStav.obsazeno) and
     (Self.prevTU.Obsazeno = TUsekStav.obsazeno) and
     ((Self.navKryci = nil) or (TBlkNav(Self.navKryci).Navest > 0))) then
  begin
   // nastala aktivace blokove podminky
   Self.bpInBlk := true;

   if ((not Self.IsSouprava()) and (Self.prevTU.IsSouprava())) then
    begin
     // mezi useky je potreba predat soupravu
     Soupravy[Self.prevTU.Souprava].front := Self;
     Self.AddSoupravaL(Self.prevTU.Souprava);
     Self.zpomalovani_ready := true;
     Self.houk_ev_enabled := true;
     Self.rychUpdate := true;

     if (Self.nextTU = nil) then
      begin
       // souprava vstoupila do posledniho bloku trati
       // zmena stanic soupravy a hnacich vozidel v ni
       TBlkTrat(Self.Trat).SprChangeOR(Self.Souprava);
      end;
    end;//if predavam soupravu
  end;

 // uvolnovani soupravy z TU (pokud je jiz predana do dalsiho TU)
 if ((Self.bpInBlk) and (Self.nextTU <> nil) and (Self.nextTU.Souprava = Self.Souprava) and
     (Self.Obsazeno = TusekStav.Uvolneno) and (Self.nextTU.Obsazeno = TUsekStav.obsazeno)) then
  begin
   Self.bpInBlk  := false;
   Self.RemoveSoupravy();
   if (not TBlkTrat(Self.Trat).Obsazeno) then TBlkTrat(Self.Trat).BP := false;   
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

function TBlkTU.GetSectObsazeno():TUsekStav;
var blk:TBlkTU;
    sectUseky:TList<TBlkTU>;
begin
 if (Self.Trat = nil) then Exit(TusekStav.none);
 if (Self.Obsazeno <= TUsekStav.none) then Exit(TUsekStav.Obsazeno);

 // pozadavek na obsazenost sekce muze prijit i kdyz trat nema smer,
 //  typicky kdyz se stavi JC do bezsouhlasove trati s automatickou
 //  zmenou souhlasu
 // -> pro krajni useky trti vracime obsazenost prvni sekce

 case (TBlkTrat(Self.Trat).Smer) of
  TTratSmer.AtoB : sectUseky := Self.lsectUseky;
  TTratSmer.BtoA : sectUseky := Self.ssectUseky;
  TTratSmer.zadny : begin
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

function TBlkTU.GetSectMaster():TBlkTU;
begin
 if (Self.Trat = nil) then Exit(nil);
 case (TBlkTrat(Self.Trat).Smer) of
  TTratSmer.AtoB : Result := Self.lsectMaster;
  TTratSmer.BtoA : Result := Self.ssectMaster;
 else
  Result := nil;
 end;
end;

////////////////////////////////////////////////////////////////////////////////
// vrati dalsi navestidlo v trati (pokud ma trat smer)
// pokud neni dalsi navestidlo autobloku, vrati hranicni navestidlo trati

function TBlkTU.GetNextNav():TBlk;
var blk:TBLkTU;
begin
 if (Self.Trat = nil) or (TBlkTrat(Self.Trat).smer = TTratSmer.zadny) then Exit(nil);

 blk := Self.nextTU;
 while ((blk <> nil) and (blk.sectMaster <> blk)) do
   blk := blk.nextTU;

 if (blk <> nil) then
   Exit(blk.navKryci)
 else begin
  case (TBlkTrat(Self.Trat).Smer) of
    TTratSmer.AtoB : Exit(TBlkTrat(Self.Trat).navSudy);
    TTratSmer.BtoA : Exit(TBlkTrat(Self.Trat).navLichy);
  end;
 end;

 Result := nil;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTU.CreateNavRefs();
var Blk:TBlk;
begin
 Blky.GetBlkByID(Self.TUSettings.navLid, Blk);
 if ((Blk <> nil) and (Blk.typ = _BLK_NAV) and (Self.lTU <> nil)) then
  begin
   TBlkNav(Blk).UsekID := Self.lTU.id;
   TBlkNav(Blk).Smer := THVStanoviste.lichy;
   TBlkNav(Blk).autoblok := true;
  end;

 Blky.GetBlkByID(Self.TUSettings.navSid, Blk);
 if ((Blk <> nil) and (Blk.typ = _BLK_NAV) and (Self.sTU <> nil)) then
  begin
   TBlkNav(Blk).UsekID := Self.sTU.id;
   TBlkNav(Blk).Smer := THVStanoviste.sudy;
   TBlkNav(Blk).autoblok := true;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTU.RemoveTURefs();
var Blk:TBlk;
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
 if ((Blk <> nil) and (Blk.typ = _BLK_NAV)) then TBlkNav(Blk).UsekID := -1;
 Blky.GetBlkByID(Self.TUSettings.navSid, Blk);
 if ((Blk <> nil) and (Blk.typ = _BLK_NAV)) then TBlkNav(Blk).UsekID := -1;
end;

////////////////////////////////////////////////////////////////////////////////
// Tato metoda nastavuje kryci navestidlo sekce a zaroven kontroluje
// zruseni navesti do trati v pripade nahleho obsazeni prvni sekce trati.

procedure TBlkTU.UpdateNavest();
var
    Blk:TBlk;
begin
 // kontrola zruseni navesti jizdni cesty pri obsazeni sekce trati:
 // tato metoda je volana vzdy pouze u sectMastera (tj. krajniho bloku sekce)
 if (Self.Trat = nil) then Exit();

 // NASTAVOVANI NAVESTI AUTOBLOKU:

 // nejprve zhasneme navestidla v nespravnem smeru
 if ((TBlkTrat(Self.Trat).Smer = TTratSmer.AtoB) or (TBlkTrat(Self.Trat).Smer = TTratSmer.zadny)) then
  begin
   if (Self.TUSettings.navSid > -1) then
    begin
     Blky.GetBlkByID(Self.TUSettings.navSid, Blk);
     if (Blk <> nil) then
       TBlkNav(Blk).Navest := TBlkTrat(Self.Trat).NavestProtismer();
    end;
  end;

 if ((TBlkTrat(Self.Trat).Smer = TTratSmer.BtoA) or (TBlkTrat(Self.Trat).Smer = TTratSmer.zadny)) then
  begin
   if (Self.TUSettings.navLid > -1) then
    begin
     Blky.GetBlkByID(Self.TUSettings.navLid, Blk);
     if (Blk <> nil) then
       TBlkNav(Blk).Navest := TBlkTrat(Self.Trat).NavestProtismer();
    end;
  end;

  if (TBlkTrat(Self.Trat).Smer = TTratSmer.zadny) then
    Exit();

 // zrusit jizdni cestu muzeme pouze u sekce na kraji trati (v trati se rusi
 //   navest autobloku)
 if ((Self.prevTU = nil) and (Self.sectObsazeno = TUsekStav.obsazeno)
     and (TBlkTrat(Self.Trat).Zaver)) then
   JCDb.RusJC(Self);

 // nastavime kryci navestidlo
 if ((Self.navKryci <> nil) and (not TBlkNav(Self.navKryci).ZAM) and
     (TBlkNav(Self.navKryci).Navest >= 0)) then
  begin
   if (not Self.sectReady) then
    begin
     // sekce obsazena -> navestidlo na STUJ
     TBlkNav(Self.navKryci).Navest := TBlkNav._NAV_STUJ
    end else begin
     // sekce uvolnena -> hledame dalsi navestidlo
     if ((Self.nextNav = nil) or (not TBlkNav(Self.nextNav).IsPovolovaciNavest())) then
       TBlkNav(Self.navKryci).Navest := TBlkNav._NAV_VYSTRAHA
      else
       TBlkNav(Self.navKryci).Navest := TBlkNav._NAV_VOLNO;
    end;
  end;

end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTU.ChangeFromTrat();
begin
 Self.UpdateNavest();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTU.UpdateSprRych();
begin
 if (Self.fTUStav.sprRychUpdateIter > 0) then
  begin
   Dec(Self.fTUStav.sprRychUpdateIter);
   if (Self.fTUStav.sprRychUpdateIter = 0) then
    begin
     if ((Self.IsSouprava()) and (Self.zpomalovani_ready) and
        (Soupravy[Self.Souprava].chtenaRychlost > 0) and
        (Soupravy[Self.Souprava].chtenaRychlost <> Self.TUSettings.rychlost)) then
       Soupravy[Self.Souprava].rychlost := Self.TUSettings.rychlost;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTU.GetRychUpdate:boolean;
begin
 Result := (Self.fTUStav.sprRychUpdateIter > 0);
end;

procedure TBlkTU.SetRychUpdate(state:boolean);
begin
 if ((state) and (Self.fTUStav.sprRychUpdateIter = 0)) then Self.fTUStav.sprRychUpdateIter := 2;
 if ((not state) and (Self.fTUStav.sprRychUpdateIter > 0)) then Self.fTUStav.sprRychUpdateIter := 0;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTU.UvolnenoZJC();
var trat:TBlkTrat;
begin
 Self.fTUStav.zast_stopped := false;
 Self.fTUStav.zast_passed  := false;

 if (Self.Trat = nil) then Exit();
 trat := TBlkTrat(Self.Trat);

 // zrusime potencialni poruchu blokove podminky a blokovou podminku
 Self.bpInBlk   := false;
 Self.poruchaBP := false;

 if (((trat.GetSettings().zabzar = TTratZZ.nabidka))
     and (not trat.Zaver) and (not trat.Obsazeno) and (not trat.RBPCan) and (Trat.stav.soupravy.Count = 0) and (not trat.nouzZaver)) then
  trat.Smer := TTratSmer.zadny;

 // pokud je trat uplne volna, zrusime blokovou podminku
 if (not trat.Obsazeno) then trat.BP := false;

 trat.UpdateSprPredict();
 trat.Change();
end;

////////////////////////////////////////////////////////////////////////////////
// komtrola volnosti sekce pro prijezd soupravy: musi byt splneno
//  1) zadny usek sekce neni obsazen
//  2) vsechny useky sekce jsou bez soupravy

function TBlkTU.GetSectReady():boolean;
var blk:TBlkTU;
    sectUseky:TList<TBlkTU>;
begin
 if ((Self.Trat = nil) or (Self.Obsazeno <= TUsekStav.none)) then Exit(false);

 case (TBlkTrat(Self.Trat).Smer) of
  TTratSmer.AtoB : sectUseky := Self.lsectUseky;
  TTratSmer.BtoA : sectUseky := Self.ssectUseky;
  TTratSmer.zadny : begin
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
   if ((blk.Obsazeno <> TUsekStav.uvolneno) or (blk.IsSouprava())
    or (blk.poruchaBP)) then Exit(false);
 Result := true;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTU.GetReady():boolean;
begin
 Result := ((Self.Obsazeno = TUsekStav.uvolneno) and (not Self.IsSouprava())
  and (not Self.poruchaBP));
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTU.SetPoruchaBP(state:boolean);
begin
 if (Self.poruchaBP <> state) then
  begin
   Self.fTUStav.poruchaBP := state;
   Self.Change();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit
