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
Format dat zastavky v souboru bloku:
  zast=IR_lichy|IR_sudy|max_delka_soupravy|delay_time|spr1;spr2;...
  pokud je zast prazdny string, zastavka je disabled
}

{
 TODO: zastavka na stejny IR, jako navestidlo autobloku
}

interface

uses TBlokUsek, Classes, TBlok, IniFiles, SysUtils, IdContext, RPConst,
      Generics.Collections;

type
 TBlkTUZastavka = record                                                        // zastavka na TU
  enabled:boolean;                                                                // existuje v useku zastavka?
  IR_lichy:Integer;                                                               // odkaz na zastavovaci IR v lichem smeru
  IR_sudy:Integer;                                                                // odkaz na zastavovaci IR v sudem smeru
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
  zast_passed:boolean;                                                            // atdy je ulozeno true, pokud souprava zastavku jiz projela

  bpInBlk:boolean;                                                                // jestli je v useku zavedena blokova podminka
                                                                                  // bpInBlk = kontroluji obsazeni bloku, pri uvolneni useku bez predani dale vyhlasit poruchu BP
  sprRychUpdateIter:Integer;                                                      // pocet zbyvajicich iteraci do nastaveni rychlost soupravy
 end;


 // technologicky blok Tratoveho useku
 TBlkTU = class(TBlkUsek)
  private const
   _def_tu_stav:TBlkTUStav = (                                                  // zakladni stav TU
    inTrat: -1;
    zast_stopped : false;
    zast_enabled : true;
    sprRychUpdateIter : 0;
   );

   _def_tu_zastavka:TBlkTUZastavka = (                                          // zakladni stav zastavky
    enabled : false;
    IR_lichy : -1;
    IR_sudy : -1;
    soupravy : nil;
    max_delka : 0;
   );

  private
   TUSettings:TBlkTUSettings;
   fTUStav:TBlkTUStav;

   fZastIRLichy, fZastIRSudy,                                                   // odkaz na IR zastavky v lichem a sudem smeru
   fNavKryci, fTrat : TBlk;                                                     // odkaz na kryci navestidla v lichem a sudem smeru
                                                                                // pro pristup k temto blokum pouzivat property bez f, toto jsou pouze pomocne promenne!

    function GetZastIRLichy():TBlk;                                             // vrati ZastIRLichy, vyuzito u property \ZastIRLichy
    function GetZastIRSudy():TBlk;                                              // vrati ZastIRSudy, vyuzito u property \ZastIRSudy

    procedure ZastUpdate();                                                     // technologie zastavky (rizeni zastavovani z rozjizdeni vlaku)
    procedure ZastRunTrain();                                                   // zastavit vlak v zastavce
    procedure ZastStopTrain();                                                  // rozjet vlak ze zastavky

    // kliky v menu TU:
    procedure MenuZastClick(SenderPnl:TIdContext; SenderOR:TObject; new_state:boolean);
    procedure MenuJEDLokClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuRBPClick(SenderPnl:TIdContext; SenderOR:TObject);

    function GetUsekSpr:Integer;                                                // vrati soupravu na TU, vyuzito u property \Souprava

    procedure UpdateBP();                                                       // technologie blokove podminky, resi veskere predavani souprav mezi TU a sekcemi TU

    function GetTrat():TBlk;                                                    // vrati blok trati, ve kterem je TU, viz property \Trat
    function GetNavKryci():TBlk;                                                // vrati kryci navestidlo TU, jinak nil (pokud blok neni v aktualnim smeru trati kryty zadnym navestidlem)
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

    property zastIRlichy:TBlk read GetZastIRLichy;                              // IR zastavky v lichem smeru
    property zastIRsudy:TBlk read GetZastIRSudy;                                // IR zastavky v sudem smeru

  protected
    procedure SetUsekSpr(spr:Integer); override;                                // nastaven soupravy useku, kvuli warningum kompilatoru presunuto do protected (v bazove tride je protected)

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

    procedure Update(); override;
    procedure Change(now:boolean = false); override;
    procedure ChangeFromTrat();                                                 // aktualizace TU z trati, vola se zejemna pri zmene smeru a jeho ucel je nastavit navestidla autobloku podle smeru trati

    function ShowPanelMenu(SenderPnl:TIdContext; SenderOR:TObject; rights:TORCOntrolRights):string; override;
    procedure PanelClick(SenderPnl:TIdContext; SenderOR:TObject; Button:TPanelButton; rights:TORCOntrolRights); override;
    procedure PanelMenuClick(SenderPnl:TIdContext; SenderOR:TObject; item:string); override;

    procedure CreateSComRefs();                                                 // navestidlum autobloku nastavi UsekPred a smer
    procedure RemoveTURefs();                                                   // zrusi UsekPred navetidlum autobloku

    procedure UvolnenoZJC();                                                    // obsah useku (ne nutne souprava!) byl prevzat z krajniho useku trati jizdni cestou
                                                                                // tato metoda ma smysl pouze pro krajni TU trati a resi radne odstraneni obsahu useku z trati

    // pro vyznam properties viz hlavicky getteru a setteru
    property TUStav:TBlkTUStav read fTUStav;
    property Souprava:Integer read GetUsekSpr write SetUsekSpr;
    property InTrat:Integer read fTUStav.InTrat write fTUStav.InTrat;
    property bpInBlk:boolean read fTUStav.bpInBlk write fTUStav.bpInBlk;
    property sectObsazeno:TUsekStav read GetSectObsazeno;
    property sectReady:boolean read GetSectReady;
    property rychUpdate:boolean read GetRychUpdate write SetRychUpdate;

    property Trat:TBlk read GetTrat;
    property navKryci:TBlk read GetNavKryci;
    property tratReady:boolean read GetTratReady;

    property prevTU:TBlkTU read GetPrevTU;
    property nextTU:TBlkTU read GetNextTU;
    property sectMaster:TBlkTU read GetSectMaster;
    property nextNav:TBlk read GetNextNav;

 end;//TBlkUsek


implementation

uses SprDb, TBloky, TBlokIR, TCPServerOR, TOblRizeni, TBlokTrat, TBlokSCom,
      TJCDatabase;

////////////////////////////////////////////////////////////////////////////////

constructor TBlkTU.Create(index:Integer);
begin
 inherited Create(index);

 Self.GlobalSettings.typ := _BLK_TU;
 Self.fTUStav := _def_tu_stav;

 Self.fZastIRLichy := nil;
 Self.fZastIRSUdy  := nil;
 Self.fTrat        := nil;
 Self.fNavKryci    := nil;
 Self.lsectMaster  := nil;
 Self.lsectUseky   := TList<TBlkTU>.Create();
 Self.ssectMaster  := nil;
 Self.ssectUseky   := TList<TBlkTU>.Create();
 Self.bpInBlk      := false;
end;//ctor

destructor TBlkTU.Destroy();
begin
 Self.lsectUseky.Free();
 Self.ssectUseky.Free();
 Self.TUSettings.Zastavka.soupravy.Free();
 inherited Destroy();
end;//dtor

////////////////////////////////////////////////////////////////////////////////
// nacte konfiguracni data ze souboru

procedure TBlkTU.LoadData(ini_tech:TMemIniFile;const section:string;ini_rel,ini_stat:TMemIniFile);
var str:TStrings;
begin
 inherited LoadData(ini_tech, section, ini_rel, ini_stat);

 Self.TUSettings.navLid := ini_tech.ReadInteger(section, 'navL', -1);
 Self.TUSettings.navSid := ini_tech.ReadInteger(section, 'navS', -1);

 Self.TUSettings.rychlost := ini_tech.ReadInteger(section, 'rychlost', -1);
 Self.bpInBlk := ini_stat.ReadBool(section, 'bpInBlk', false);

 str := TStringList.Create();
 ExtractStrings(['|'],[], PChar(ini_tech.ReadString(section, 'zast', '')), str);

 // nacitani zastavky
 if (Assigned(Self.TUSettings.Zastavka.soupravy)) then Self.TUSettings.Zastavka.soupravy.Free();
 Self.TUSettings.Zastavka := _def_tu_zastavka;
 Self.TUSettings.Zastavka.soupravy := TStringList.Create();

 if (str.Count > 0) then
  begin
   try
    Self.TUsettings.Zastavka.enabled   := true;
    Self.TUsettings.Zastavka.IR_lichy  := StrToInt(str[0]);
    Self.TUsettings.Zastavka.IR_sudy   := StrToInt(str[1]);
    Self.TUsettings.Zastavka.max_delka := StrToInt(str[2]);
    Self.TUsettings.Zastavka.delay     := StrToTime(str[3]);
    Self.TUsettings.Zastavka.soupravy.Clear();
    ExtractStrings([';'],[],PChar(str[4]), Self.TUsettings.Zastavka.soupravy);
   except
    Self.TUsettings.Zastavka := _def_tu_zastavka;
   end;
  end;

 str.Free();
end;//procedure

// ulozi konfiguracni data do souboru
procedure TBlkTU.SaveData(ini_tech:TMemIniFile;const section:string);
var str:string;
    i:Integer;
begin
 inherited SaveData(ini_tech, section);

 ini_tech.WriteInteger(section, 'navL', Self.TUSettings.navLid);
 ini_tech.WriteInteger(section, 'navS', Self.TUSettings.navSid);

 ini_tech.WriteInteger(section, 'rychlost', Self.TUSettings.rychlost);

 // ukladani zastavky
 if (Self.TUsettings.Zastavka.enabled) then
  begin
   with (Self.TUsettings.Zastavka) do
    begin
     str := IntToStr(IR_lichy) + '|' + IntToStr(IR_sudy) + '|' + IntToStr(max_delka) + '|' + TimeToStr(delay) + '|';
     for i := 0 to soupravy.Count-1 do
      str := str + soupravy[i] + ';';
    end;

   ini_tech.WriteString(section, 'zast', str);
  end else begin
   ini_tech.WriteString(section, 'zast', '');
  end;

end;//procedure

// ulozi stavova data do souboru
procedure TBlkTU.SaveStatus(ini_stat:TMemIniFile;const section:string);
begin
 inherited;
 ini_stat.WriteBool(section, 'bpInBlk', Self.bpInBlk);
end;

////////////////////////////////////////////////////////////////////////////////
// operace s nastavenim TU

function TBlkTU.GetSettings():TBlkTUSettings;
begin
 Result := Self.TUSettings;
end;//function

procedure TBlkTU.SetSettings(data:TBlkTUSettings);
begin
 if (Self.TUSettings.Zastavka.soupravy <> data.Zastavka.soupravy) then
  Self.TUSettings.Zastavka.soupravy.Free();

 Self.TUSettings := data;
 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////
// aktualizace stavu bloku

procedure TBlkTU.Update();
begin
 inherited;

 if ((Self.InTrat > -1) and (Self.Stav.Stav = TUsekStav.obsazeno) and (Self.Souprava > -1) and (Self.TUSettings.Zastavka.enabled)) then
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
      (Soupravy.soupravy[Self.Souprava].delka > Self.TUSettings.Zastavka.max_delka) or (Soupravy.soupravy[Self.Souprava].front <> self)) then Exit();

   // kontrola typu soupravy:
   found := false;
   for i := 0 to Self.TUSettings.Zastavka.soupravy.Count-1 do
    begin
     if (Self.TUSettings.Zastavka.soupravy[i] = Soupravy.soupravy[Self.Souprava].typ) then
      begin
       found := true;
       break;
      end;
    end;

   if (not found) then Exit();

   case (Soupravy.soupravy[Self.Souprava].smer) of
    THVSTanoviste.lichy : if ((Assigned(Self.zastIRlichy)) and ((Self.zastIRlichy as TBlkIR).Stav = TIRStav.obsazeno)) then
                              Self.ZastStopTrain();
    THVSTanoviste.sudy  : if ((Assigned(Self.zastIRsudy)) and ((Self.zastIRsudy as TBlkIR).Stav = TIRStav.obsazeno)) then
                              Self.ZastStopTrain();
   end;//case
  end else begin
   // osetreni rozjeti vlaku z nejakeho pochybneho duvodu
   //  pokud se souprava rozjede, koncim zastavku
   if (Soupravy.soupravy[Self.Souprava].rychlost <> 0) then
    begin
     Self.fTUStav.zast_stopped := false;
     Self.Change();  // change je dulezite volat kvuli menu
    end;

   // cekam na timeout na rozjeti vlaku
   if (Now > Self.TUStav.zast_run_time) then
    Self.ZastRunTrain();
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////
// zastavky:

function TBlkTU.GetZastIRLichy():TBlk;
begin
 if (((Self.fZastIRLichy = nil) and (Self.TUSettings.Zastavka.IR_lichy <> -1)) or ((Self.fZastIRLichy <> nil) and (Self.fZastIRLichy.GetGlobalSettings.id <> Self.TUSettings.Zastavka.IR_lichy))) then
   Blky.GetBlkByID(Self.TUSettings.Zastavka.IR_lichy, Self.fZastIRLichy);
 Result := Self.fZastIRLichy;
end;//function

function TBlkTU.GetZastIRSudy():TBlk;
begin
 if (((Self.fZastIRSudy = nil) and (Self.TUSettings.Zastavka.IR_sudy <> -1)) or ((Self.fZastIRSudy <> nil) and (Self.fZastIRSudy.GetGlobalSettings.id <> Self.TUSettings.Zastavka.IR_sudy))) then
   Blky.GetBlkByID(Self.TUSettings.Zastavka.IR_sudy, Self.fZastIRSudy);
 Result := Self.fZastIRSudy;
end;//function


////////////////////////////////////////////////////////////////////////////////

procedure TBlkTU.ZastStopTrain();
begin
 Self.fTUStav.zast_stopped  := true;
 Self.fTUStav.zast_run_time := Now+Self.TUSettings.Zastavka.delay;

 try
   Self.fTUStav.zast_rych := Soupravy.soupravy[Self.Souprava].rychlost;
   Soupravy.soupravy[Self.Souprava].rychlost := 0;
 except

 end;

 Self.Change();     // change je dulezite volat kvuli menu
end;//procedure

procedure TBlkTU.ZastRunTrain();
begin
 Self.fTUStav.zast_stopped := false;
 Self.fTUStav.zast_passed  := true;

 try
   Soupravy.soupravy[Self.Souprava].rychlost := Self.TUStav.zast_rych;
 except

 end;

 Self.Change();     // change je dulezite volat kvuli menu
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TBlkTU.ShowPanelMenu(SenderPnl:TIdContext; SenderOR:TObject; rights:TORCOntrolRights):string;
begin
 Result := inherited;

 // zastavka
 if ((Self.TUSettings.Zastavka.enabled) and (Self.InTrat > -1)) then
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
     Result := Result + 'JEÏ vlak,';
    end;
  end;

 if ((Self.Zaver = TJCType.nouz) and (Self.Souprava > -1)) then
   Result := Result + '!RBP,';
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTU.PanelClick(SenderPnl:TIdContext; SenderOR:TObject ;Button:TPanelButton; rights:TORCOntrolRights);
begin
 if (Self.Stav.Stav <= TUsekStav.none) then Exit();

 case (Button) of
  right,F2: ORTCPServer.Menu(SenderPnl, Self, (SenderOR as TOR), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));
  left    : if (not Self.MenuKCClick(SenderPnl, SenderOR)) then
              if (not Self.PresunLok(SenderPnl, SenderOR)) then
                ORTCPServer.Menu(SenderPnl, Self, (SenderOR as TOR), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));
  middle  : Self.MenuVBClick(SenderPnl, SenderOR);
  F3: Self.ShowPanelSpr(SenderPnl, SenderOR, rights);
 end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTU.SetUsekSpr(spr:Integer);
var old_spr:Integer;
    trat:TBlkTrat;
begin
 if (spr = Self.Souprava) then Exit();
 old_spr := Self.Souprava;
 inherited;

 if (spr = -1) then
  begin
   Self.fTUStav.zast_stopped := false;
   Self.fTUStav.zast_passed  := false;

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
end;

function TBlkTU.GetUsekSpr:Integer;
begin
 Result := Self.Stav.Spr;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTU.MenuZastClick(SenderPnl:TIdContext; SenderOR:TObject; new_state:boolean);
begin
 if (not Self.TUStav.zast_stopped) then
   Self.fTUStav.zast_enabled := new_state;
end;//procedure

procedure TBlkTU.MenuJEDLokClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 if (Self.TUStav.zast_stopped) then
   Self.ZastRunTrain();
end;//procedure

procedure TBlkTU.MenuRBPClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 ORTCPServer.Potvr(SenderPnl, Self.PanelPotvrSekvRBP, SenderOR as TOR, 'Zrušení poruchy blokové podmínky', TBlky.GetBlksList(Self), nil);
end;

procedure TBlkTU.PanelPotvrSekvRBP(Sender:TIdContext; success:boolean);
var old_spr:Integer;
    blks:TList<TObject>;
begin
 if (success) then
  begin
   old_spr := Self.Souprava;
   Self.bpInBlk := false;
   Self.Zaver   := TJCType.no;
   if (Self.Souprava > -1) then
    begin
     Self.Souprava := -1;
     blks := Blky.GetBlkWithSpr(old_spr);
     if (blks.Count = 0) then Soupravy.RemoveSpr(old_spr);
     blks.Free();
    end;

   if (Self.Trat <> nil) then Self.Trat.Change();
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTU.PanelMenuClick(SenderPnl:TIdContext; SenderOR:TObject; item:string);
begin
 if (Self.Stav.Stav <= TUsekStav.none) then Exit();

 if (item = 'JEÏ vlak')   then Self.MenuJEDLokClick(SenderPnl, SenderOR)
 else if (item = 'ZAST>') then Self.MenuZastClick(SenderPnl, SenderOR, true)
 else if (item = 'ZAST<') then Self.MenuZastClick(SenderPnl, SenderOR, false)
 else if (item = 'RBP') then Self.MenuRBPClick(SenderPnl, SenderOR)
 else inherited;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TBlkTU.GetTrat():TBlk;
begin
 if (((Self.fTrat = nil) and (Self.TUStav.inTrat <> -1)) or ((Self.fTrat <> nil) and (Self.fTrat.GetGlobalSettings.id <> Self.TUStav.inTrat))) then
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

 if (((Self.fNavKryci = nil) and (navPrevID <> -1)) or ((Self.fNavKryci <> nil) and (Self.fNavKryci.GetGlobalSettings.id <> navPrevID))) then
   Blky.GetBlkByID(navPrevID, Self.fNavKryci);
 Result := Self.fNavKryci;
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
     ((Self.navKryci = nil) or (TBlkSCom(Self.navKryci).Navest > 0))) then
  begin
   // nastala aktivace blokove podminky
   Self.bpInBlk := true;

   if ((Self.Souprava = -1) and (Self.prevTU.Souprava > -1)) then
    begin
     // mezi useky je potreba predat soupravu
     Soupravy.soupravy[Self.prevTU.Souprava].front := Self;
     Self.Souprava := Self.prevTU.Souprava;
     Self.zpomalovani_ready := true;
     Self.rychUpdate := true;

     if (Self.nextTU = nil) then
      begin
       // souprava vstoupila do posledniho bloku trati
       // zmena stanic soupravy a hnacich vozidel v ni
       TBlkTrat(Self.Trat).SprChangeOR(Self.Souprava);

       // je nutne zmenit smer soupravy?
       if (((Assigned(TBlkTrat(Self.Trat).navLichy)) and (Assigned(TBlkTrat(Self.Trat).navSudy))) and
           (TBlkSCom(TBlkTrat(Self.Trat).navLichy).Smer = TBlkSCom(TBlkTrat(Self.Trat).navSudy).Smer)) then
        begin
         // navestidla na koncich trati jsou ve stejnem smeru -> zmenit smer soupravy, hnacich vozidel v ni a sipek
         Soupravy.soupravy[Self.Souprava].ChangeSmer();
        end;
      end;
    end;//if predavam soupravu
  end;

 // uvolnovani soupravy z TU (pokud je jiz predana do dalsiho TU)
 if ((Self.bpInBlk) and (Self.nextTU <> nil) and (Self.nextTU.Souprava = Self.Souprava) and
     (Self.Obsazeno = TusekStav.Uvolneno) and (Self.nextTU.Obsazeno = TUsekStav.obsazeno)) then
  begin
   Self.bpInBlk  := false;
   Self.Souprava := -1;
   if (not TBlkTrat(Self.Trat).Obsazeno) then TBlkTrat(Self.Trat).BP := false;   
  end;

 // kontrola poruchy blokove podminky
 if ((Self.bpInBlk)) then
  begin
   if ((Self.Obsazeno = TUsekStav.uvolneno) and (Self.Zaver = TJCType.no)) then
     Self.Zaver := TJCType.nouz;
   if ((Self.Obsazeno = TUsekStav.obsazeno) and (Self.Zaver = TJCType.nouz)) then
     Self.Zaver := TJCType.no;
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

 blk := Self;
 while ((blk <> nil) and (blk.sectMaster <> nil)) do
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

procedure TBlkTU.CreateSComRefs();
var Blk:TBlk;
begin
 Blky.GetBlkByID(Self.TUSettings.navLid, Blk);
 if ((Blk <> nil) and (Blk.GetGlobalSettings.typ = _BLK_SCOM) and (Self.lTU <> nil)) then
  begin
   TBlkSCom(Blk).UsekID := Self.lTU.GetGlobalSettings.id;
   TBlkSCom(Blk).Smer   := THVStanoviste.lichy;
  end;

 Blky.GetBlkByID(Self.TUSettings.navSid, Blk);
 if ((Blk <> nil) and (Blk.GetGlobalSettings.typ = _BLK_SCOM) and (Self.sTU <> nil)) then
  begin
   TBlkSCom(Blk).UsekID := Self.sTU.GetGlobalSettings.id;
   TBlkSCom(Blk).Smer   := THVStanoviste.sudy;
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
 if ((Blk <> nil) and (Blk.GetGlobalSettings.typ = _BLK_SCOM)) then TBlkSCom(Blk).UsekID := -1;
 Blky.GetBlkByID(Self.TUSettings.navSid, Blk);
 if ((Blk <> nil) and (Blk.GetGlobalSettings.typ = _BLK_SCOM)) then TBlkSCom(Blk).UsekID := -1;
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
 case (TBlkTrat(Self.Trat).Smer) of
   TTratSmer.AtoB  : begin
    if (Self.TUSettings.navSid > -1) then
     begin
      Blky.GetBlkByID(Self.TUSettings.navSid, Blk);
      if (Blk <> nil) then TBlkSCom(Blk).Navest := 13;
     end;
   end;

   TTratSmer.BtoA  : begin
    if (Self.TUSettings.navLid > -1) then
     begin
      Blky.GetBlkByID(Self.TUSettings.navLid, Blk);
      if (Blk <> nil) then TBlkSCom(Blk).Navest := 13;
     end;
   end;

   TTratSmer.zadny : begin
    if (Self.TUSettings.navSid > -1) then
     begin
      Blky.GetBlkByID(Self.TUSettings.navSid, Blk);
      if (Blk <> nil) then TBlkSCom(Blk).Navest := 13;
     end;
    if (Self.TUSettings.navLid > -1) then
     begin
      Blky.GetBlkByID(Self.TUSettings.navLid, Blk);
      if (Blk <> nil) then TBlkSCom(Blk).Navest := 13;
     end;
    Exit();
   end;
  end;//case

 // zrusit jizdni cestu muzeme pouze u sekce na kraji trati (v trati se rusi
 //   navest autobloku)
 if ((Self.prevTU = nil) and (Self.sectObsazeno = TUsekStav.obsazeno)
     and (TBlkTrat(Self.Trat).Zaver)) then
   JCDb.RusJC(Self);

  // nastavime kryci navestidlo
  if (Self.navKryci <> nil) then
   begin
    if (not Self.sectReady) then
     begin
      // sekce obsazena -> navetidlo na STUJ
      TBlkSCom(Self.navKryci).Navest := 0
     end else begin
      // sekce uvolnena -> hledame dalsi navestidlo
      if ((Self.nextNav = nil) or (TBlkSCom(Self.nextNav).Navest = 0)) then
        TBlkSCom(Self.navKryci).Navest := 2
      else
        TBlkSCom(Self.navKryci).Navest := 1;
     end;
   end;

end;//procedure

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
     if ((Self.Souprava > -1) and (Self.zpomalovani_ready) and
        (Soupravy.soupravy[Self.Souprava].rychlost > 0) and
        (Soupravy.soupravy[Self.Souprava].rychlost <> Self.TUSettings.rychlost)) then
       Soupravy.soupravy[Self.Souprava].rychlost := Self.TUSettings.rychlost;
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

 // zrusime potencialni nouzovy zaver
 Self.bpInBlk := false;
 Self.Zaver   := TJCType.no;

 if (((trat.GetSettings().zabzar = TTratZZ.nabidka))
     and (not trat.Zaver) and (not trat.Obsazeno) and (not trat.RBPCan) and (Trat.stav.soupravy.cnt = 0) and (not trat.nouzZaver)) then
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
   if ((blk.Obsazeno <> TUsekStav.uvolneno) or (blk.Souprava > -1)
    or ((blk.Zaver <> TJCType.no) and (blk.prevTU <> nil))) then Exit(false);
 Result := true;
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit
