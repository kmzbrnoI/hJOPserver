unit TechnologieJC;

{
  Kompletni technologie jizdnich cest.

  Tento soubor implementuje tridu TJC, ktera reprezentuje jednu jizdni cestu.
  Jizdni cesta se stara o vse od udrzovani vsech jejich udaju, kterymi je dana
  v zaverove tabulce, pres jeji staveni, kontrolu podminek, zobrazovani
  potvrzovacich sekvenci pri staveni az po spravne ruseni jizdni cesty.
}

{
  Co je to BARIERA JIZDNI CESTY?
  > Bariera jizdni cesty je prekazka branici jejimu postaveni, ktere se lze
  > zbavit napr. jen pouhym potvrzenim (napr. jizdni cesta pres blok se stitkem),
  > potvrzenim pres potvrzovaci sekvenci, nebo se ji nelze zbavit vubec a jizdni
  > cestu jendnoduse nelze postavit.

  Technologie jizdnch cest rozeznava nekolik druhu berier:
   1) KRITICKE BARIERY
      jsou takove bariery, ktere dispecer nemuze odstranit (v ceste napriklad
      chybi tecnologicky blok).
      > Kriticka bariera se pozna tak, ze \CriticalBariera vrati true.
   2) STANDARDNI BARIERY
      jsou takove bariery, ktere se odstrani "samy" - napriklad usek pod
      zaverem, obsazney usek.
      > Standardni bariera typicky neni kriticka, ani neni varovna, tudiz
      > se pozna tak, ze nesplnuje podminky kriticke ani varovne bariery.
   3) VAROVNE BARIERY
      jsou takove bariery, ktere primo nebrani jizdni ceste ve staveni, ale je
      potreba si je uvedmit a potvrdit je (napr. na useku je stitek, ci vyluka).
      Tyto bariery je vzdy nutne potvrdit upozorneni v levem dolnim rohu panelu,
      nektere z nich mohou vyzadovat i potvrzeni potvrzovaci sekvenci.
      > Varovna bariera se pozna tak, ze \WarningBariera vrati true
      > Bariera nutna potvrzeni potvrzovaci sekvenci se pozna tak, ze
      >  \PotvrSekvBariera vrati true.
}

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, Logging,
  Dialogs, Menus, Buttons, ComCtrls, fMain, TBloky, TBlok, IbUtils, Train,
  IniFiles, IdContext, TBlokTrat, Generics.Collections, UPO, TBlokVyhybka,
  TOblRizeni, changeEvent, changeEventCaller, JsonDataObjects, PTUtils;

const
  _JC_INITPOTVR_TIMEOUT_SEC = 60;                                               // timeout UPO a potvrzeni na zacatku staveni JC
  _JC_TIMEOUT_SEC = 30;                                                         // timeout pro staveni jizdni cesty (vlakove i posunove v sekundach)
  _JC_PRJ_TIMEOUT_SEC = 50;                                                     // timeout pri staveni JC pro zavirani prejezdu v ceste
  _NC_TIMEOUT_MIN = 1;                                                          // timeout pro staveni nouzove cesty (vlakove i posunove) v minutach
  _JC_MAX_VYH_STAVENI = 4;                                                      // kolik vyhybek se muze stavit zaroven v JC

  _KROK_DEFAULT = 0;
  _KROK_KRIT_BARIERY = 1;
  _KROK_POTVR_BARIERY = 5;
  _KROK_POTVR_SEKV = 6;

  _JC_KROK_INIT = 10;
  _JC_KROK_CEKANI_VYHYBKA_POLOHA = 11;
  _JC_KROK_ZAVRIT_PREJEZDY = 12;
  _JC_KROK_CEKANI_PREJEZDY = 13;
  _JC_KROK_FINALNI_ZAVER = 14;
  _JC_KROK_CEKANI_NAVESTIDLO = 15;
  _JC_KROK_FINISH = 16;
  _JC_KROK_CEKANI_POSLEDNI_USEK = 20;

  _NC_KROK_INIT = 100;
  _NC_KROK_BARIERA_UPDATE = 101;
  _NC_KROK_BARIERY_POTVRZENY = 102;
  _NC_KROK_CEKANI_NAVESTIDLO = 103;
  _NC_KROK_FINISH = 104;

type
  TJCType = (vlak = 1, posun = 2, nouz = 3);
  TJCNextNavType = (zadna = 0, trat = 1, blok = 2);

  // jedna bariera ve staveni jizdni cesty:
  TJCBariera = record
   typ:Integer;                                                                 // typ bariery, odkazuje na konstanty _JCB_*, viz nize
   blok:TBlk;                                                                   // blok, na ktery se bariera vztahuje; nektere bariery nemusi byt prirazeny bloku, platnost tohoto parametru je potreba overit pro kazdou barieru samostatne
   param:Integer;                                                               // parametr bariery, typicky napr. ID bloku, ktery neexistuje; v takovem pripade samozrejme nemuze existovat \blok
  end;
  TJCBariery = TList<TJCBariera>;                                               // seznam barier; typicky seznam barie branici staveni jizdni cesty

  // zaver vyhybky v jizdni ceste
  TJCVyhZaver=record
   Blok:Integer;                                                                // odkaz na blok (ID bloku)
   Poloha:TVyhPoloha;                                                           // chtena poloha vyhybky
  end;

  // zaver odvratove vyhybky v jizdni ceste
  TJCOdvratZaver=record
   Blok:Integer;                                                                // odkaz na blok (ID bloku)
   Poloha:TVyhPoloha;                                                           // chtena poloha vyhybky
   ref_blk:Integer;                                                             // blok, pri jehoz zruseni redukce (typicky usek a uvolneni zaveru) dojde i k uvolneni zaveru odvratove vyhybky
  end;

  // bloky v JC, ketre jsou navazany na konkretni useky v ramci JC
  TJCRefZaver=record
   Blok:Integer;                                                                // odkaz na blok ID
   ref_blk:Integer;                                                             // blok, pri jehoz uvolneni zaveru dojde ke zruseni redukce \Blok
  end;

  // prejezd v jizdni ceste
  TJCPrjZaver=record
   Prejezd:Integer;                                                             // odkaz na ID bloku prejezdu
   uzaviraci:TList<Integer>;                                                    // uzaviraci bloky (ID) prejezdu
                                                                                // pokud se prejezd nezavira, je seznam prazdny
   oteviraci:Integer;                                                           // oteviraci blok (ID) prejezdu
                                                                                // pokud se prejezd nezavira, je nedefinovany
  end;

  ///////////////////////////////////////////////////////////////////////////

  // staveni jizdni cesty:
  //    staveni jizdni cesty probiha krokove, viz \UpdateStaveni
  TJCStaveni = record
   krok: Integer;                                                               // aktualni krok staveni jizdni cesty
   timeOut: TDateTime;                                                          // cas, pri jehoz prekroceni dojde k timeoutu JC
   senderOR: TObject;                                                           // oblast rizeni, ktera vyvolala staveni JC, do teto OR jsou typicky odesilany notifikacni a chybove hlasky (napr. upozorneni vlevo dole panelu, potvrzovaci sekvence)
   senderPnl: TIdContext;                                                       // konkretni panel, kery vyvolal staveni JC
   rozpadBlok,                                                                  // index useku, na ktery ma vkrocit souprava
   rozpadRuseniBlok: Integer;                                                   // index useku, ze ktereho ma vystoupit souprava
                                                                                  // index je index v seznamu useku, tedy napr. 0 =  0. usek v jizdni ceste
                                                                                  // -6 = postavena nouzova cesta, -5 = cesta neni postavena, -2 = navestidlo na STUJ, -1 = usek pred navestidlem, 0..n = useky JC
   from_stack: TObject;                                                         // odkaz na zasobnik, ze ktereho proehlo staveni JC
   nc: boolean;                                                                 // flag staveni nouzove cesty (vlakovou i posunovou)
   ncBariery: TJCBariery;                                                       // aktualni seznam barier pro potvrzovaci sekvenci pri staveni nouzove cesty
   ncBarieryCntLast: Integer;                                                   // posledni pocet barier ve staveni nouzove cesty
   nextVyhybka: Integer;                                                        // vyhybka, ktera se ma stavit jako dalsi
                                                                                // po postaveni vsechn vyhybek plynule prechazi do indexu seznamu odvratu
   ab: Boolean;                                                                 // po postaveni JC automaticky zavest AB
   prjWasClosed: Boolean;                                                       // jiz byl vydan povel k zavreni prejezdu
   lastUsekOrTratObsaz: Boolean;                                                // je obsazen posledni usek JC, nestavit navestidlo a nezavirat prejezdy
  end;

  // vlastnosti jizdni cesty nemenici se se stavem:
  TJCprop = record
   name: string;
   id: Integer;
   typ: TJCType;

   navestidloBlok: Integer;
   dalsiNavaznost: TJCNextNavType;                                              // typ dalsi navaznosti
   dalsiNavestidlo: Integer;                                                    // ID bloku dalsiho navestidla

   vyhybky: TList<TJCVyhZaver>;
   useky: TList<Integer>;
   odvraty: TList<TJCOdvratZaver>;
   prejezdy: TList<TJCPrjZaver>;
   zamky: TList<TJCRefZaver>;
   vb: TList<Integer>;                                                          // seznam variantnich bodu JC - obashuje postupne ID bloku typu usek

   trat: Integer;                                                               // ID trati, na kterou JC navazuje; pokud JC nenavazuje na trat, je \Trat = -1
   tratSmer: TtratSmer;
   speedGo, speedStop: Cardinal;                                                // rychlost v JC pri dalsim navestidle navestici dovolujici a NEdovolujici navest
   odbocka: Boolean;
   nzv: Boolean;                                                                // nedostatecna zabrzdna vzdalenost
  end;

  ENavChanged = procedure(Sender:TObject; origNav:TBlk) of object;

  ///////////////////////////////////////////////////////////////////////////

  TJC=class
   public const

    // bariery ve staveni jizdni cesty:

    _JCB_OK                      = 0;
    _JCB_STAVENI                 = 1;
    _JCB_BLOK_DISABLED           = 2;
    _JCB_BLOK_NOT_EXIST          = 3;
    _JCB_BLOK_NOT_TYP            = 4;
    _JCB_PRIVOLAVACKA            = 5;

    _JCB_NAV_NOT_USEK            = 10;

    _JCB_USEK_OBSAZENO           = 20;
    _JCB_USEK_ZAVER              = 21;
    _JCB_USEK_VYLUKA             = 22;
    _JCB_USEK_SOUPRAVA           = 23;
    _JCB_USEK_STITEK             = 24;
    _JCB_USEK_AB                 = 25;
    _JCB_USEK_LAST_OBSAZENO      = 26;  // povoleno postaveni JC

    _JCB_VYHYBKA_KONC_POLOHA     = 30;
    _JCB_VYHYBKA_VYLUKA          = 31;
    _JCB_VYHYBKA_STITEK          = 32;
    _JCB_VYHYBKA_ZAMCENA         = 33;
    _JCB_VYHYBKA_NOUZ_ZAVER      = 34;
    _JCB_VYHYBKA_NESPAVNA_POLOHA = 35;

    _JCB_PREJEZD_NOUZOVE_OTEVREN = 40;
    _JCB_PREJEZD_PORUCHA         = 41;
    _JCB_PREJEZD_STITEK          = 42;
    _JCB_PREJEZD_NEUZAVREN       = 43;

    _JCB_ODVRAT_ZAMCENA          = 60;
    _JCB_ODVRAT_OBSAZENA         = 61;
    _JCB_ODVRAT_KONC_POLOHA      = 62;

    _JCB_TRAT_NEPRIPRAVENA       = 69;
    _JCB_TRAT_ZAK                = 70;
    _JCB_TRAT_ZAVER              = 71;
    _JCB_TRAT_OBSAZENO           = 72;  // povoleno postaveni JC
    _JCB_TRAT_ZADOST             = 73;
    _JCB_TRAT_NESOUHLAS          = 74;
    _JCB_TRAT_NO_BP              = 75;
    _JCB_TRAT_NOT_ZAK            = 76;
    _JCB_TRAT_STITEK             = 77;
    _JCB_TRAT_NEPRENOS           = 78;
    _JCB_TRAT_PRENOS_NAKONEC     = 79;

    _JCB_ZAMEK_NEUZAMCEN         = 80;
    _JCB_ZAMEK_NOUZ_ZAVER        = 81;

    _JCB_HV_RUC                  = 100;
    _JCB_HV_NOT_ALL_RUC          = 101;

    _JCB_SPR_SMER                = 120;


   private const
    _def_jc_staveni : TJCStaveni = (
     Krok : _KROK_DEFAULT;
     RozpadBlok : -5;
     RozpadRuseniBlok : -5;
     ab: false;
     prjWasClosed: false;
    );

   private
     fproperties: TJCprop;
     fstaveni: TJCStaveni;
     fOnIdChanged: TNotifyEvent;
     fOnNavChanged: ENavChanged;

      procedure SetInitKrok();
      procedure SetProperties(prop:TJCProp);

      procedure RusZacatekJC();
      procedure RusKonecJC();
      procedure RusVBJC();
      procedure PredejDataDalsimuBloku();                                       // predani dat dalsimu useku v jizdni ceste
      procedure CheckSmyckaBlok(blk:TBlk);                                      // kontroluje zmenu smeru soupravy a hnacich vozidel pri vkroceni do smyckove bloku, tato kontrola probiha pouze pri vkroceni do posledniho bloku JC

      function GetStaveni():boolean;
      function GetPostaveno():boolean;

      procedure PS_vylCallback(Sender:TIdContext; success:boolean);             // callback potvrzovaci sekvence na vyluku
      procedure UPO_OKCallback(Sender:TObject);                                 // callback potvrzeni upozorneni
      procedure UPO_EscCallback(Sender:TObject);                                // callback zamitnuti upozorneni
      procedure NC_PS_Callback(Sender:TIdContext; success:boolean);             // callback potvrzovaci sekvence nouzove cesty

      procedure UsekClosePrj(Sender:TObject; data:integer);                     // zavre prejezd pri vkroceni na dany usek, odkaz na tuto metodu je posilan usekum, ktere ji pri obsazeni vyvolaji

      procedure SetRozpadBlok(RozpadBlok:Integer);
      procedure SetRozpadRuseniBlok(RozpadRuseniBlok:Integer);
      procedure SetKrok(Krok:Integer);
      procedure CritBarieraEsc(Sender:TObject);

      // callbacky ne/nastevni polohy vyhybek:
      procedure VyhNeprestavenaJCPC(Sender:TObject; error: TVyhSetError);
      procedure VyhNeprestavenaNC(Sender:TObject; error: TVyhSetError);
      procedure VyhPrestavenaNC(Sender:TObject);
      procedure VyhPrestavenaJCPC(Sender:TObject);
      procedure NavNepostaveno(Sender:TObject);

      procedure KontrolaPodminekVCPC(var bariery:TList<TJCBariera>);            // kontrola podminek vlakovych a posunovych cest
      procedure KontrolaPodminekNC(var bariery:TList<TJCBariera>);              // kontrola podminek nouzovych cest
      procedure PodminkyNCStaveni(var bariery:TList<TJCBariera>);

      function BarieryNCToPotvr(bariery:TJCBariery):TPSPodminky;                // seznam barier nouzve cesty prevede na potvrzovaci sekvence pro klienta

      function GetTrain(nav:TBlk = nil; usek:TBlk = nil): TTrain; // vraci cislo soupravy na useku pred navestidlem

      function GetAB():boolean;
      function PorusenaKritickaPodminka():boolean;
      function GetNav():TBlk;
      function GetWaitForLastUsekOrTratObsaz():Boolean;
      function GetLastUsek(): TBlk;

      procedure BarieraToJson(const bariera: TJCBariera; result: TJsonObject);
      procedure Log(msg: string; typ: Integer = WR_VC);

   public

     index:Integer;                                                             // index v tabulce jizdni cest ve F_Main
     changed:boolean;                                                           // JC zmenana -> akualizuje se v tabulce ve F_Main

      class function JCBariera(typ:Integer; Blok:TBlk = nil; param:Integer = 0):TJCBariera;
                                                                                // jednoduche genreovani berier jako navratove funkce teto funkce
      function JCBarieraToMessage(Bariera:TJCBariera):TUPOItem;                 // prevod bariery na spravu upozorneni vlevo dole
      class function CriticalBariera(typ:Integer):boolean;                      // je bariera kriticka?
      class function PotvrSekvBariera(typ:Integer):boolean;                     // je bariera hodna potvrzovaci sekvence?
      function WarningBariera(typ:Integer):boolean;                             // je bariera hodna zobrazeni upozorneni?

      class function PotvrSekvBarieraToReason(typ:Integer):string;

      constructor Create(); overload;
      constructor Create(data:TJCprop); overload;
      destructor Destroy(); override;

      procedure NastavNav();                                                    // nastavi pozadovanu navest pri postaveni JC
      procedure RusJC(Sender:TObject = nil);                                    // rusi vlakovou cestu
      procedure RusJCWithoutBlk();                                              // rusi vlakovou cestu bez zruseni zaveru useku
      procedure UsekyRusJC();                                                   // kontroluje projizdeni soupravy useky a rusi jejich zavery
      procedure UsekyRusNC();                                                   // rusi poruchu BP trati, ze ktere odjizdi souprava v ramci nouzove jizdni cesty
      procedure NeprofilObsaz();                                                // volano pri obsazeni kontrolvoaneho neprofiloveho useku

      procedure UpdateStaveni();                                                // prubezne stavi JC, meni kroky
      procedure UpdateTimeOut();                                                // kontroluje TimeOut staveni JC
      procedure CancelStaveni(reason:string = ''; stack_remove:boolean = false);// zrusi staveni a oduvodneni zaloguje a zobrazi dispecerovi

      procedure LoadData(ini:TMemIniFile; section:string);
      procedure SaveData(ini:TMemIniFile; section:string);

      function StavJC(SenderPnl:TIdContext; SenderOR:TObject; bariery_out:TJCBariery;
          from_stack:TObject = nil; nc:boolean = false; fromAB:boolean = false;
          abAfter:Boolean = false):Integer; overload;
      function StavJC(SenderPnl:TIdContext; SenderOR:TObject;
          from_stack:TObject = nil; nc:boolean = false; fromAB:boolean = false;
          abAfter:Boolean = false):Integer; overload;


      function CanDN():boolean;                                                 // true = je mozno DN; tato funkce kontroluje, jestli je mozne znovupostavit cestu i kdyz byla fakticky zrusena = musi zkontrolovat vsechny podminky
      procedure DN();                                                           // DN nastavi zavery vsech bloku na validni a rozsviti navestidlo
      procedure STUJ();

      function KontrolaPodminek(NC:boolean = false):TJCBariery;
      function IsAnyVyhMinus():boolean;
      procedure ClientDisconnect(AContext: TIDContext);

      procedure GetPtData(json:TJsonObject; includeStaveni:boolean);
      procedure GetPtStaveni(json:TJsonObject);
      procedure PostPtStav(reqJson:TJsonObject; respJson:TJsonObject);

      property data: TJCprop read fproperties write SetProperties;
      property stav: TJCStaveni read fstaveni;

      property name: String read fproperties.name;
      property id: Integer read fproperties.id write fproperties.id;
      property typ: TJCType read fproperties.typ;

      property staveni: Boolean read GetStaveni;
      property postaveno: Boolean read GetPostaveno;                             // true pokud je postavena navest
      property AB: Boolean read GetAB;
      property waitForLastUsekOrTratObsaz: Boolean read GetWaitForLastUsekOrTratObsaz;
      property lastUsek: TBlk read GetLastUsek;

      property rozpadBlok: Integer read fstaveni.rozpadBlok write SetRozpadBlok;
      property rozpadRuseniBlok: Integer read fstaveni.rozpadRuseniBlok write SetRozpadRuseniBlok;
      property krok: Integer read fstaveni.krok write SetKrok;
      property navestidlo: TBlk read GetNav;

      property OnIdChanged: TNotifyEvent read fOnIdChanged write fOnIdChanged;
      property OnNavChanged: ENavChanged read fOnNavChanged write fOnNavChanged;
  end;

implementation

uses GetSystems, TechnologieRCS, THnaciVozidlo, TBlokNav, TBlokUsek, TOblsRizeni,
     TBlokPrejezd, TJCDatabase, TCPServerOR, TrainDb, timeHelper,
     THVDatabase, Zasobnik, TBlokUvazka, TBlokZamek, TBlokTratUsek;

////////////////////////////////////////////////////////////////////////////////

constructor TJC.Create();
begin
 inherited;

 Self.fproperties.id := -1;
 Self.changed  := true;
 Self.fstaveni := _def_jc_staveni;
 Self.fstaveni.ncBariery := TList<TJCBariera>.Create();

 Self.fproperties.zamky := TList<TJCRefZaver>.Create();
 Self.fproperties.vb    := TList<Integer>.Create();

 Self.fproperties.vyhybky  := TList<TJCVyhZaver>.Create();
 Self.fproperties.useky    := TList<Integer>.Create();
 Self.fproperties.odvraty  := TList<TJCOdvratZaver>.Create();
 Self.fproperties.prejezdy := TList<TJCPrjZaver>.Create();
end;//ctor

constructor TJC.Create(data:TJCprop);
begin
 inherited Create();

 Self.fproperties := data;
 Self.fstaveni := _def_jc_staveni;
 if (not Assigned(Self.fstaveni.ncBariery)) then Self.fstaveni.ncBariery := TList<TJCBariera>.Create();

 if (not Assigned(Self.fproperties.zamky))    then Self.fproperties.zamky := TList<TJCRefZaver>.Create();
 if (not Assigned(Self.fproperties.vb))       then Self.fproperties.vb := TList<Integer>.Create();
 if (not Assigned(Self.fproperties.odvraty))  then Self.fproperties.odvraty := TList<TJCOdvratZaver>.Create();
 if (not Assigned(Self.fproperties.prejezdy)) then Self.fproperties.prejezdy := TList<TJCPrjZaver>.Create();
 if (not Assigned(Self.fproperties.vyhybky))  then Self.fproperties.vyhybky := TList<TJCVyhZaver>.Create();
 if (not Assigned(Self.fproperties.useky))    then Self.fproperties.useky := TList<Integer>.Create();
end;//ctor

destructor TJC.Destroy();
var i:Integer;
begin
 if (Assigned(Self.fstaveni.ncBariery)) then FreeAndNil(Self.fstaveni.ncBariery);
 if (Assigned(Self.fproperties.zamky)) then FreeAndNil(Self.fproperties.zamky);
 if (Assigned(Self.fproperties.vb)) then  Self.fproperties.vb.Free();

 if (Assigned(Self.fproperties.vyhybky))  then Self.fproperties.vyhybky.Free();
 if (Assigned(Self.fproperties.useky))    then Self.fproperties.useky.Free();
 if (Assigned(Self.fproperties.odvraty))  then Self.fproperties.odvraty.Free();
 for i := 0 to Self.fproperties.prejezdy.Count-1 do Self.fproperties.prejezdy[i].uzaviraci.Free();
 if (Assigned(Self.fproperties.prejezdy)) then Self.fproperties.prejezdy.Free();

 inherited;
end;//ctor

////////////////////////////////////////////////////////////////////////////////

// kontroluje podminky pro staveni konkretni jizdni cesty
// vraci List prblemu (tzv. bariery), ktere definuji to, proc jizdni cestu nelze postavit (tedy vraci vsechny nesplnene podminky)
// tzv. kriticke bariery jsou vzdy na zacatu Listu
function TJC.KontrolaPodminek(NC:boolean = false):TJCBariery;
var i:Integer;
    Blk,blk2:TBlk;
    privol:TBlksList;
    vyhZaver:TJCVyhZaver;
    usekZaver:Integer;
    refZaver:TJCRefZaver;
    prjZaver:TJCPrjZaver;
    odvratZaver:TJCOdvratZaver;
begin
  Result := TList<TJCBariera>.Create();

  if (Self.staveni) then
    Result.Add(Self.JCBariera(_JCB_STAVENI));

  // kontrola useku navestidla:
  if (Blky.GetBlkByID(Self.fproperties.navestidloBlok, Blk) <> 0) then
   begin
    // blok navestidla neexistuje
    Result.Add(Self.JCBariera(_JCB_BLOK_NOT_EXIST, nil, Self.fproperties.navestidloBlok));
    Exit;
   end;

  if (Self.lastUsek = nil) then
   begin
    // neexistuje ani jeden usek nebo je posledni usek nevalidni
    Result.Add(Self.JCBariera(_JCB_BLOK_NOT_EXIST, nil, 0));
    Exit();
   end;

  if (Blk.typ <> btNav) then
   begin
    // blok navestidla neni typu navestidlo
    Result.Add(Self.JCBariera(_JCB_BLOK_NOT_TYP, Blk, Self.fproperties.navestidloBlok));
    Exit;
   end;

  if ((Blk as TBlkNav).UsekPred = nil) then
   begin
    // blok navestidla pred sebou nema zadny usek
    Result.Add(Self.JCBariera(_JCB_NAV_NOT_USEK, Blk, Self.fproperties.navestidloBlok));
    Exit;
   end;

  // vyhybky:
  // kontrolujeme, jestli vyhybky existuji a jestli jsou to vyhybky
  for vyhZaver in Self.fproperties.vyhybky do
   begin
    if (Blky.GetBlkByID(vyhZaver.Blok, Blk) <> 0) then
     begin
      Result.Add(Self.JCBariera(_JCB_BLOK_NOT_EXIST, nil, vyhZaver.Blok));
      Exit;
     end;

    if (Blk.typ <> btVyhybka) then
     begin
      Result.Add(Self.JCBariera(_JCB_BLOK_NOT_TYP, Blk, vyhZaver.Blok));
      Exit;
     end;
   end;

  // useky:
  for usekZaver in Self.fproperties.useky do
   begin
    // zkontrolujeme, jestli useky existuji a jestli jsou to useky
    if (Blky.GetBlkByID(usekZaver, Blk) <> 0) then
     begin
      Result.Add(Self.JCBariera(_JCB_BLOK_NOT_EXIST, nil, usekZaver));
      Exit;
     end;

    if ((Blk.typ <> btUsek) and (Blk.typ <> btTU)) then
     begin
      Result.Add(Self.JCBariera(_JCB_BLOK_NOT_TYP, Blk, usekZaver));
      Exit;
     end;
   end;

  // kontrola prejezdu
  for prjZaver in Self.fproperties.prejezdy do
   begin
    // kontrola existence bloku prejezdu
    if (Blky.GetBlkByID(prjZaver.Prejezd, blk) <> 0) then
     begin
      Result.Insert(0, Self.JCBariera(_JCB_BLOK_NOT_EXIST, nil, prjZaver.Prejezd));
      Exit;
     end;

    // kontrola typu bloku prejezdu
    if (blk.typ <> btPrejezd) then
     begin
      Result.Insert(0, Self.JCBariera(_JCB_BLOK_NOT_TYP, blk, prjZaver.Prejezd));
      Exit;
     end;

    // pokud se ma prejezd zavirat
    if (prjZaver.uzaviraci.Count > 0) then
     begin
      // kontrola existence oteviraciho bloku
      if (Blky.GetBlkByID(prjZaver.oteviraci, blk2) <> 0) then
       begin
        Result.Insert(0, Self.JCBariera(_JCB_BLOK_NOT_EXIST, blk, prjZaver.oteviraci));
        Exit;
       end;

      // kontrola typu oteviraciho bloku
      if ((blk2.typ <> btUsek) and (blk2.typ <> btTU)) then
       begin
        Result.Insert(0, Self.JCBariera(_JCB_BLOK_NOT_TYP, blk, prjZaver.oteviraci));
        Exit;
       end;

      // kontrola existence uzaviracich bloku a jejich typu
      for usekZaver in prjZaver.uzaviraci do
       begin
        if (Blky.GetBlkByID(usekZaver, blk2) <> 0) then
         begin
          Result.Insert(0, Self.JCBariera(_JCB_BLOK_NOT_EXIST, blk, usekZaver));
          Exit;
         end;
        if ((blk2.typ <> btUsek) and (blk2.typ <> btTU)) then
         begin
          Result.Insert(0, Self.JCBariera(_JCB_BLOK_NOT_TYP, blk, usekZaver));
          Exit;
         end;
       end;
     end;
   end;

  // kontrola odvratu
  for odvratZaver in Self.fproperties.odvraty do
   begin
    if (Blky.GetBlkByID(odvratZaver.ref_blk, blk) <> 0) then
     begin
      Result.Insert(0, Self.JCBariera(_JCB_BLOK_NOT_EXIST, nil, odvratZaver.ref_blk));
      Exit;
     end;
    if ((blk.typ <> btUsek) and (blk.typ <> btTU)) then
     begin
      Result.Insert(0, Self.JCBariera(_JCB_BLOK_NOT_TYP, blk, odvratZaver.ref_blk));
      Exit;
     end;
    if (Blky.GetBlkByID(odvratZaver.Blok, blk) <> 0) then
     begin
      Result.Insert(0, Self.JCBariera(_JCB_BLOK_NOT_EXIST, nil, odvratZaver.Blok));
      Exit;
     end;
    if (blk.typ <> btVyhybka) then
     begin
      Result.Insert(0, Self.JCBariera(_JCB_BLOK_NOT_TYP, blk, odvratZaver.Blok));
      Exit;
     end;
   end;

  // trat
  if (Self.fproperties.Trat > -1) then
   begin
    if (Self.lastUsek.typ <> btTU) then
     begin
      Result.Add(Self.JCBariera(_JCB_BLOK_NOT_TYP, Self.lastUsek, Self.lastUsek.id));
      Exit;
     end;
    if (Blky.GetBlkByID(Self.fproperties.Trat, blk) <> 0) then
     begin
      Result.Insert(0, Self.JCBariera(_JCB_BLOK_NOT_EXIST, nil, Self.fproperties.Trat));
      Exit;
     end;
    if (blk.typ <> btTrat) then
     begin
      Result.Insert(0, Self.JCBariera(_JCB_BLOK_NOT_TYP, blk, Self.fproperties.Trat));
      Exit;
     end;
   end;

  // kontrola podminkovych bloku zamku
  for refZaver in Self.fproperties.zamky do
   begin
    if (Blky.GetBlkByID(refZaver.Blok, blk) <> 0) then
     begin
      Result.Insert(0, Self.JCBariera(_JCB_BLOK_NOT_EXIST, nil, refZaver.Blok));
      Exit;
     end;
    if (blk.typ <> btZamek) then
     begin
      Result.Insert(0, Self.JCBariera(_JCB_BLOK_NOT_TYP, blk, blk.id));
      Exit;
     end;
    if (Blky.GetBlkByID(refZaver.ref_blk, blk) <> 0) then
     begin
      Result.Insert(0, Self.JCBariera(_JCB_BLOK_NOT_EXIST, nil, refZaver.ref_blk));
      Exit;
     end;
    if ((blk.typ <> btUsek) and (blk.typ <> btTU)) then
     begin
      Result.Insert(0, Self.JCBariera(_JCB_BLOK_NOT_TYP, blk, blk.id));
      Exit;
     end;
   end;//for i

 if (NC) then
  Self.KontrolaPodminekNC(Result)
 else
  Self.KontrolaPodminekVCPC(Result);

 // kontrola zaplych privolavacich navesti
 privol := Blky.GetNavPrivol(Self.fstaveni.senderOR as TOR);

 for i := 0 to privol.Count-1 do
   Result.Add(Self.JCBariera(_JCB_PRIVOLAVACKA, privol[i] as TBlk, (privol[i] as TBlk).id));

 if (Assigned(privol)) then privol.Free();
end;

////////////////////////////////////////////////////////////////////////////////
// kontrola podminek vlakove a posunove cesty

procedure TJC.KontrolaPodminekVCPC(var bariery:TList<TJCBariera>);
var i, usek, cnt, addr:Integer;
    Blk,blk2:TBlk;
    glob:TBlkSettings;
    flag, cont:boolean;
    train:TTrain;
    prjZaver:TJCPrjZaver;
    vyhZaver:TJCVyhZaver;
    odvratZaver:TJCOdvratZaver;
    refZaver:TJCRefZaver;
begin
  // navestidlo
  Blky.GetBlkByID(Self.fproperties.navestidloBlok, Blk);
  if (not (Blk as TBlkNav).enabled) then
    bariery.Add(Self.JCBariera(_JCB_BLOK_DISABLED, Blk, Blk.id));

  // kontrola useku:
  if (Self.fproperties.Trat > -1) then
    cnt := Self.fproperties.useky.Count-1
  else
    cnt := Self.fproperties.useky.Count;

  for i := 0 to cnt-1 do
   begin
    Blky.GetBlkByID(Self.fproperties.useky[i], Blk);
    glob := Blk.GetGlobalSettings();

    if ((Blk as TBlkUsek).Stav.Stav = TUsekStav.disabled) then
      bariery.Add(Self.JCBariera(_JCB_BLOK_DISABLED, Blk, Blk.id));

    // obsazenost
    if ((i <> Self.fproperties.useky.Count-1) or (Self.typ <> TJCType.posun)) then
     begin
      // kontrola disabled jiz probehla
      if ((Blk as TBlkUsek).Obsazeno <> TUsekStav.uvolneno) then
       begin
        if ((i = Self.fproperties.useky.Count-1) and (Self.fproperties.useky.Count > 1)) then
          bariery.Add(Self.JCBariera(_JCB_USEK_LAST_OBSAZENO, Blk, Blk.id))
        else
          bariery.Add(Self.JCBariera(_JCB_USEK_OBSAZENO, Blk, Blk.id));
       end else begin
        // souprava
        if ((Blk as TBlkUsek).IsTrain()) then
          bariery.Add(Self.JCBariera(_JCB_USEK_SOUPRAVA, Blk, Blk.id));
       end;
     end;//if

    // zaver
    if ((Blk as TBlkUsek).Zaver <> TZaver.no) then
     begin
      if ((Blk as TBlkUsek).Zaver = TZaver.ab) then
        bariery.Add(Self.JCBariera(_JCB_USEK_AB, Blk, Blk.id))
      else
        bariery.Add(Self.JCBariera(_JCB_USEK_ZAVER, Blk, Blk.id));
     end;

    // vyluka
    if ((Blk as TBlkUsek).Vyluka <> '') then
      bariery.Add(Self.JCBariera(_JCB_USEK_VYLUKA, blk, blk.id));

    // stitek
    if ((Blk as TBlkUsek).Stitek <> '') then
      bariery.Add(Self.JCBariera(_JCB_USEK_STITEK, blk, blk.id));
   end;//for i

  // kontrola vyhybek:
  for vyhZaver in Self.fproperties.vyhybky do
   begin
    Blky.GetBlkByID(vyhZaver.Blok, Blk);
    glob := Blk.GetGlobalSettings();

    if ((Blk as TBlkVyhybka).Stav.poloha = TVyhPoloha.disabled) then
      bariery.Add(Self.JCBariera(_JCB_BLOK_DISABLED, Blk, Blk.id));

    // kontrola neprofilovych useku vyhybek pro polohu +
    if ((vyhZaver.Poloha = TVyhPoloha.plus) and (TBlkVyhybka(Blk).npBlokPlus <> nil) and
        (TBlkUsek(TBlkVyhybka(Blk).npBlokPlus).Obsazeno = TUsekStav.disabled)) then
      bariery.Add(Self.JCBariera(_JCB_BLOK_DISABLED, TBlkVyhybka(Blk).npBlokPlus,
          TBlkVyhybka(Blk).npBlokPlus.id));

    // kontrola neprofilovych useku vyhybek pro polohu -
    if ((vyhZaver.Poloha = TVyhPoloha.minus) and (TBlkVyhybka(Blk).npBlokMinus <> nil) and
        (TBlkUsek(TBlkVyhybka(Blk).npBlokMinus).Obsazeno = TUsekStav.disabled)) then
      bariery.Add(Self.JCBariera(_JCB_BLOK_DISABLED, TBlkVyhybka(Blk).npBlokMinus,
          TBlkVyhybka(Blk).npBlokMinus.id));

    // kontrola koncove polohy:
    if (((Blk as TBlkVyhybka).poloha = TVyhPoloha.none) or ((Blk as TBlkVyhybka).poloha = TVyhPoloha.both)) then
      bariery.Add(Self.JCBariera(_JCB_VYHYBKA_KONC_POLOHA, Blk, Blk.id));

    // zaver nema smysl kontrolovat - zaver vyhybek je prakticky zaver useku
    // proto ho staci zkontrolovat jen u useku

    // kontrola vyluky vyhybky:
    if ((Blk as TBlkVyhybka).Vyluka <> '') then
      bariery.Add(Self.JCBariera(_JCB_VYHYBKA_VYLUKA, Blk, Blk.id));

    // kontrola stitku vyhybky:
    if ((Blk as TBlkVyhybka).Stitek <> '') then
      bariery.Add(Self.JCBariera(_JCB_VYHYBKA_STITEK, Blk, Blk.id));

    // kontrola nouzoveho zaveru a redukce menu:
    if ((Blk as TBlkVyhybka).Poloha <> vyhZaver.Poloha) then
     begin
      if ((Blk as TBlkVyhybka).vyhZaver) then
        bariery.Add(Self.JCBariera(_JCB_VYHYBKA_NOUZ_ZAVER, Blk, Blk.id))
      else if (TBlkVyhybka(Blk).outputLocked) then
        bariery.Add(Self.JCBariera(_JCB_VYHYBKA_ZAMCENA, Blk, Blk.id));
     end;

    // kontrola spojky
    Blky.GetBlkByID((Blk as TBlkVyhybka).GetSettings.spojka, Blk2);
    // pokud nemam ja polohu, predpokladam, ze spojka bude muset byt prestavena -> musi byt volna, bez zaveru, ...
    // kontrolovat zaver z useku neni potreba - pokud je problem se zaverem, vyvstane uz na useku JC, jinak je vyhybka v poloze, ktere zaver nevadi
    if ((blk2 <> nil) and ((Blk as TBlkVyhybka).Poloha <> vyhZaver.Poloha)) then
     begin
      if ((Blk2 as TBlkVyhybka).vyhZaver) then
        bariery.Add(Self.JCBariera(_JCB_VYHYBKA_NOUZ_ZAVER, Blk2, Blk2.id))
      else if (TBlkVyhybka(Blk2).outputLocked) then
        bariery.Add(Self.JCBariera(_JCB_VYHYBKA_ZAMCENA, Blk2, Blk.id));

      if ((Blk2 as TBlkVyhybka).Obsazeno = TUsekStav.obsazeno) then
        bariery.Add(Self.JCBariera(_JCB_USEK_OBSAZENO, Blk2, Blk2.id));
     end;

    // kontrola neprofiloveho styku pro polohu +
    if ((vyhZaver.Poloha = TVyhPoloha.plus) and (TBlkVyhybka(Blk).npBlokPlus <> nil) and
        (TBlkUsek(TBlkVyhybka(Blk).npBlokPlus).Obsazeno <> TUsekStav.uvolneno)) then
      bariery.Add(Self.JCBariera(_JCB_USEK_OBSAZENO, TBlkVyhybka(Blk).npBlokPlus,
          TBlkVyhybka(Blk).npBlokPlus.id));

    // kontrola neprofiloveho styku pro polohu -
    if ((vyhZaver.Poloha = TVyhPoloha.minus) and (TBlkVyhybka(Blk).npBlokMinus <> nil) and
        (TBlkUsek(TBlkVyhybka(Blk).npBlokMinus).Obsazeno <> TUsekStav.uvolneno)) then
      bariery.Add(Self.JCBariera(_JCB_USEK_OBSAZENO, TBlkVyhybka(Blk).npBlokMinus,
          TBlkVyhybka(Blk).npBlokMinus.id));
   end;//for i

  // kontrola prejezdu
  for prjZaver in Self.fproperties.prejezdy do
   begin
    Blky.GetBlkByID(prjZaver.Prejezd, Blk);

    // blok disabled
    if ((Blk as TBlkPrejezd).Stav.basicStav = TBlkPrjBasicStav.disabled) then
      bariery.Add(Self.JCBariera(_JCB_BLOK_DISABLED, Blk, Blk.id));

    if ((Blk as TBlkPrejezd).Stav.basicStav <> TBlkPrjBasicStav.none) then
     begin
      if ((Blk as TBlkPrejezd).Stav.PC_NOT) then
        bariery.Add(Self.JCBariera(_JCB_PREJEZD_NOUZOVE_OTEVREN, blk, prjZaver.Prejezd));
     end else
      bariery.Add(Self.JCBariera(_JCB_PREJEZD_PORUCHA, blk, prjZaver.Prejezd));

    // kontrola stitku prejezdu:
    if ((Blk as TBlkPrejezd).Stitek <> '') then
      bariery.Add(Self.JCBariera(_JCB_PREJEZD_STITEK, Blk, Blk.id));
   end;//for i

  // kontrola odvratu
  for odvratZaver in Self.fproperties.odvraty do
   begin
    Blky.GetBlkByID(odvratZaver.Blok, Blk);
    glob := Blk.GetGlobalSettings();

    if ((Blk as TBlkVyhybka).Stav.poloha = TVyhPoloha.disabled) then
      bariery.Add(Self.JCBariera(_JCB_BLOK_DISABLED, Blk, Blk.id));

    // kontrola koncove polohy:
    if (((Blk as TBlkVyhybka).poloha = TVyhPoloha.none) or ((Blk as TBlkVyhybka).poloha = TVyhPoloha.both)) then
      bariery.Add(Self.JCBariera(_JCB_VYHYBKA_KONC_POLOHA, Blk, Blk.id));

    // kontrola vyluky vyhybky:
    if ((Blk as TBlkVyhybka).Vyluka <> '') then
      bariery.Add(Self.JCBariera(_JCB_VYHYBKA_VYLUKA, Blk, Blk.id));

    // kontrola stitku vyhybky:
    if ((Blk as TBlkVyhybka).Stitek <> '') then
      bariery.Add(Self.JCBariera(_JCB_VYHYBKA_STITEK, Blk, Blk.id));

    if ((Blk as TBlkVyhybka).poloha <> odvratZaver.Poloha) then
     begin
      if ((Blk as TBlkVyhybka).vyhZaver) then
        bariery.Add(Self.JCBariera(_JCB_VYHYBKA_NOUZ_ZAVER, Blk, Blk.id))

      else if ((Blk as TBlkVyhybka).outputLocked) then
        bariery.Add(Self.JCBariera(_JCB_ODVRAT_ZAMCENA, blk, odvratZaver.Blok));

      if ((Blk as TBlkVyhybka).Obsazeno = TUsekStav.obsazeno) then
        bariery.Add(Self.JCBariera(_JCB_ODVRAT_OBSAZENA, blk, odvratZaver.Blok));
     end;//if poloha <> Poloha

    // kontrola spojky odvratu
    Blky.GetBlkByID((Blk as TBlkVyhybka).GetSettings.spojka, Blk2);
    if (Blk2 <> nil) then
     begin
      // kontrola vyluky vyhybky:
      if ((Blk2 as TBlkVyhybka).Vyluka <> '') then
        bariery.Add(Self.JCBariera(_JCB_VYHYBKA_VYLUKA, Blk2, Blk2.id));

      // kontrola stitku vyhybky:
      if ((Blk2 as TBlkVyhybka).Stitek <> '') then
        bariery.Add(Self.JCBariera(_JCB_VYHYBKA_STITEK, Blk2, Blk2.id));

      // kontrola zamceni odvratu
      if ((Blk as TBlkVyhybka).Poloha <> odvratZaver.Poloha) then
       begin
        if ((Blk2 as TBlkVyhybka).Zaver > TZaver.no) then
         begin
          if ((Blk2 as TBlkVyhybka).Zaver = TZaver.ab) then
            bariery.Add(Self.JCBariera(_JCB_USEK_AB, Blk2, Blk2.id))
          else
            bariery.Add(Self.JCBariera(_JCB_USEK_ZAVER, Blk2, Blk2.id));
         end;

        if ((Blk2 as TBlkVyhybka).vyhZaver) then
          bariery.Add(Self.JCBariera(_JCB_VYHYBKA_NOUZ_ZAVER, Blk2, Blk2.id))
        else if ((Blk2 as TBlkVyhybka).outputLocked) then
          bariery.Add(Self.JCBariera(_JCB_VYHYBKA_ZAMCENA, Blk2, Blk2.id));

        if ((Blk2 as TBlkVyhybka).Obsazeno = TUsekStav.obsazeno) then
          bariery.Add(Self.JCBariera(_JCB_USEK_OBSAZENO, Blk2, Blk2.id));
       end;
     end;
   end;

  // kontrola trati
  if (Self.fproperties.Trat > -1) then
   begin
    Blky.GetBlkByID(Self.fproperties.Trat, Blk);
    glob := Blk.GetGlobalSettings();

    if ((Blk as TBlkTrat).stav.smer = TTratSmer.disabled) then
      bariery.Add(Self.JCBariera(_JCB_BLOK_DISABLED, Blk, Blk.id));

    cont := true;
    if ((blk as TBlkTrat).ZAK) then
      bariery.Add(Self.JCBariera(_JCB_TRAT_ZAK, blk, Self.fproperties.Trat));
    if ((blk as TBlkTrat).Zadost) then
      bariery.Add(Self.JCBariera(_JCB_TRAT_ZADOST, blk, Self.fproperties.Trat));
    if (((TBlkTrat(blk).Zaver) or (TBlkTrat(blk).nouzZaver)) and (Self.fproperties.TratSmer <> TBlkTrat(blk).Smer)) then
     begin
      bariery.Add(Self.JCBariera(_JCB_TRAT_NESOUHLAS, blk, Self.fproperties.Trat));
      cont := false;
     end;
    if ((cont) and ((blk as TBlkTrat).Zaver)) then
      bariery.Add(Self.JCBariera(_JCB_TRAT_ZAVER, blk, Self.fproperties.Trat));

    if (cont) and ((not TBlkTrat(blk).SameUserControlsBothUvazka()) or ((blk as TBlkTrat).nouzZaver)) then
      if ((((blk as TBlkTrat).GetSettings().zabzar = TTratZZ.souhlas) or ((blk as TBlkTrat).GetSettings().zabzar = TTratZZ.nabidka))
          and (Self.fproperties.TratSmer <> (blk as TBlkTrat).Smer)) then
       begin
        bariery.Add(Self.JCBariera(_JCB_TRAT_NESOUHLAS, blk, Self.fproperties.Trat));
        cont := false;
       end;

    if ((cont) and (Self.fproperties.TratSmer <> (blk as TBlkTrat).Smer)) then
     begin
      // trat beze smeru, do ktere bude dle predchozi podminky povoleno vjet -> trat s automatickou zmenou souhlasu
      // -> kontrola volnosti vsech useku trati (protoze nastane zmena smeru)
      if (not TBlkTrat(Blk).ready) then
       begin
        bariery.Add(Self.JCBariera(_JCB_TRAT_NESOUHLAS, blk, Self.fproperties.Trat));
        cont := false;
       end;
     end;

    if ((cont) and (Self.typ = TJCType.vlak)) then
     begin
      if (TBlkTU(Self.lastUsek).sectObsazeno = TUsekStav.obsazeno) then
       begin
        Blky.GetBlkByID(Self.fproperties.Trat, Blk);
        bariery.Add(Self.JCBariera(_JCB_TRAT_OBSAZENO, blk, Self.fproperties.Trat));
       end else if (not TBlkTU(Self.lastUsek).sectReady) then begin
        Blky.GetBlkByID(Self.fproperties.Trat, Blk);
        bariery.Add(Self.JCBariera(_JCB_TRAT_NEPRIPRAVENA, blk, Self.fproperties.Trat));
       end;
     end;

    Blky.GetBlkByID(Self.fproperties.Trat, Blk);

    // kontrola stitku uvazky v nasi OR:
    if ((TBlkUvazka(TBlkTrat(Blk).uvazkaA).OblsRizeni.Count > 0) and
        (TBlkUvazka(TBlkTrat(Blk).uvazkaA).OblsRizeni[0] = Self.fstaveni.senderOR) and
        (TBlkUvazka(TBlkTrat(Blk).uvazkaA).Stitek <> '')) then
      bariery.Add(Self.JCBariera(_JCB_TRAT_STITEK, TBlkUvazka(TBlkTrat(Blk).uvazkaA),
          TBlkUvazka(TBlkTrat(Blk).uvazkaA).id));

    if ((TBlkUvazka(TBlkTrat(Blk).uvazkaB).OblsRizeni.Count > 0) and
        (TBlkUvazka(TBlkTrat(Blk).uvazkaB).OblsRizeni[0] = Self.fstaveni.senderOR) and
        (TBlkUvazka(TBlkTrat(Blk).uvazkaB).Stitek <> '')) then
      bariery.Add(Self.JCBariera(_JCB_TRAT_STITEK, TBlkUvazka(TBlkTrat(Blk).uvazkaB),
          TBlkUvazka(TBlkTrat(Blk).uvazkaB).id));

    // stitky a vyluky na tratovych usecich
    for usek in TBlkTrat(Blk).GetSettings().useky do
     begin
      Blky.GetBlkByID(usek, Blk2);

      // vyluka
      if (TBlkUsek(Blk2).Vyluka <> '') then
        bariery.Add(Self.JCBariera(_JCB_USEK_VYLUKA, blk2, blk2.id));

      // stitek
      if (TBlkUsek(Blk2).Stitek <> '') then
        bariery.Add(Self.JCBariera(_JCB_USEK_STITEK, blk2, blk2.id));
     end;
   end;

  // kontrola uzamceni podminkovych zamku:
  for refZaver in Self.fproperties.zamky do
   begin
    Blky.GetBlkByID(refZaver.Blok, Blk);
    glob := Blk.GetGlobalSettings();

    // kontrola uzamceni
    if ((Blk as TBlkZamek).klicUvolnen) then
      bariery.Add(Self.JCBariera(_JCB_ZAMEK_NEUZAMCEN, blk, blk.id));
   end;//for i

 // kontrola ukradene loko v souprave pred navestidlem
 Blky.GetBlkByID(Self.fproperties.navestidloBlok, Blk2);
 Blk := (Blk2 as TBlkNav).UsekPred;

 if ((Blk as TBlkUsek).IsTrain()) then
  begin
   flag := false;
   train := Self.GetTrain(Blk2, Blk);

   // kontrola rucniho rizeni lokomotiv
   if (Self.typ = TJCType.vlak) then
     for addr in train.HVs do
       if ((HVDb[addr].stolen) or (HVDb[addr].ruc)) then
        begin
         bariery.Add(Self.JCBariera(_JCB_HV_RUC, nil, addr));
         flag := true;
        end;

   // pokud jsou jen nektere lokomotivy rizene rucne
   if (flag) then
     for addr in train.HVs do
       if ((not HVDb[addr].stolen) and (not HVDb[addr].ruc)) then
        begin
         bariery.Add(Self.JCBariera(_JCB_HV_NOT_ALL_RUC));
         break;
        end;

   // kontrola smeru soupravy
   if (Self.typ = TJCType.vlak) then
    begin
     if (train.sdata.dir_L or train.sdata.dir_S) then
       if (((TBlkNav(Blk2).Smer = THVStanoviste.lichy) and (not train.sdata.dir_L)) or
           ((TBlkNav(Blk2).Smer = THVStanoviste.sudy) and (not train.sdata.dir_S))) then
         bariery.Add(Self.JCBariera(_JCB_SPR_SMER, nil, train.index));
    end;

  end;
end;

////////////////////////////////////////////////////////////////////////////////
// kontrola podminek nouzove cesty:

procedure TJC.KontrolaPodminekNC(var bariery:TList<TJCBariera>);
var i, usek, cnt:Integer;
    Blk,blk2:TBlk;
    glob:TBlkSettings;
    vyhZaver:TJCVyhZaver;
    prjZaver:TJCPrjZaver;
    odvratZaver:TJCOdvratZaver;
begin
  {
    nouzovou cestu nelze postavit pres:
     1) useky se zaverem
     2) vyhybky s nouzovym zaverem
    jinak lze vsechy bariery prekonat
  }

  // useky:
  if (Self.fproperties.Trat > -1) then
    cnt := Self.fproperties.useky.Count-1
  else
    cnt := Self.fproperties.useky.Count;

  for i := 0 to cnt-1 do
   begin
    Blky.GetBlkByID(Self.fproperties.useky[i], Blk);
    glob := Blk.GetGlobalSettings();

    // zaver
    if ((Blk as TBlkUsek).Zaver <> TZaver.no) then
     begin
      if ((Blk as TBlkUsek).Zaver = TZaver.ab) then
        bariery.Add(Self.JCBariera(_JCB_USEK_AB, Blk, Blk.id))
      else
        bariery.Add(Self.JCBariera(_JCB_USEK_ZAVER, Blk, Blk.id));
     end;

    // vyluka
    if ((Blk as TBlkUsek).Vyluka <> '') then
      bariery.Add(Self.JCBariera(_JCB_USEK_VYLUKA, blk, blk.id));

    // stitek
    if ((Blk as TBlkUsek).Stitek <> '') then
      bariery.Add(Self.JCBariera(_JCB_USEK_STITEK, blk, blk.id));
   end;//for i

  // kontrola vyhybek:
  for vyhZaver in Self.fproperties.vyhybky do
   begin
    Blky.GetBlkByID(vyhZaver.Blok, Blk);
    glob := Blk.GetGlobalSettings();

    // kontrola vyluky vyhybky:
    if ((Blk as TBlkVyhybka).Vyluka <> '') then
      bariery.Add(Self.JCBariera(_JCB_VYHYBKA_VYLUKA, Blk, Blk.id));

    // kontrola stitku vyhybky:
    if ((Blk as TBlkVyhybka).Stitek <> '') then
      bariery.Add(Self.JCBariera(_JCB_VYHYBKA_STITEK, Blk, Blk.id));

    // kontrola nouzoveho zaveru a redukce menu:
    if ((Blk as TBlkVyhybka).Poloha <> vyhZaver.Poloha) then
     begin
      if ((Blk as TBlkVyhybka).vyhZaver) then
        bariery.Add(Self.JCBariera(_JCB_VYHYBKA_NOUZ_ZAVER, Blk, Blk.id))
      else if (TBlkVyhybka(Blk).outputLocked) then
        bariery.Add(Self.JCBariera(_JCB_VYHYBKA_ZAMCENA, Blk, Blk.id));
     end;

    // kontrola spojky
    Blky.GetBlkByID((Blk as TBlkVyhybka).GetSettings.spojka, Blk2);
    // pokud nemam ja polohu, prespokladam, ze spojka bude muset byt prestavena -> musi byt volna, bez zaveru, ...
    // kontrolovat zaver z useku eni potreba - pokud je problem se zaverem, vyvstane uz na useku JC, jinak je vyhybka v poloze, ktere zaver nevadi
    if ((blk2 <> nil) and ((Blk as TBlkVyhybka).Poloha <> vyhZaver.Poloha)) then
     begin
      if ((Blk2 as TBlkVyhybka).vyhZaver) then
        bariery.Add(Self.JCBariera(_JCB_VYHYBKA_NOUZ_ZAVER, Blk2, Blk2.id))
      else if ((Blk2 as TBlkVyhybka).outputLocked) then
        bariery.Add(Self.JCBariera(_JCB_VYHYBKA_ZAMCENA, Blk2, Blk2.id));
     end;
   end;//for i

  // kontrola prejezdu
  for prjZaver in Self.fproperties.prejezdy do
   begin
    Blky.GetBlkByID(prjZaver.Prejezd, Blk);
    // kontrola stitku prejezdu:
    if ((Blk as TBlkPrejezd).Stitek <> '') then
      bariery.Add(Self.JCBariera(_JCB_PREJEZD_STITEK, Blk, Blk.id));
   end;//for i

  // kontrola odvratu
  for odvratZaver in Self.fproperties.odvraty do
   begin
    Blky.GetBlkByID(odvratZaver.Blok, Blk);
    glob := Blk.GetGlobalSettings();

    // kontrola vyluky vyhybky:
    if ((Blk as TBlkVyhybka).Vyluka <> '') then
      bariery.Add(Self.JCBariera(_JCB_VYHYBKA_VYLUKA, Blk, Blk.id));

    // kontrola stitku vyhybky:
    if ((Blk as TBlkVyhybka).Stitek <> '') then
      bariery.Add(Self.JCBariera(_JCB_VYHYBKA_STITEK, Blk, Blk.id));

    if ((Blk as TBlkVyhybka).poloha <> odvratZaver.Poloha) then
     begin
      if ((Blk as TBlkVyhybka).vyhZaver) then
        bariery.Add(Self.JCBariera(_JCB_VYHYBKA_NOUZ_ZAVER, Blk, Blk.id))

      else if (((Blk as TBlkVyhybka).Zaver <> TZaver.no) or ((Blk as TBlkVyhybka).outputLocked)) then
        bariery.Add(Self.JCBariera(_JCB_ODVRAT_ZAMCENA, blk, odvratZaver.Blok));
     end;//if poloha <> Poloha

    // kontrola spojky odvratu
    Blky.GetBlkByID((Blk as TBlkVyhybka).GetSettings.spojka, Blk2);
    if (blk2 <> nil) then
     begin
      // kontrola vyluky vyhybky:
      if ((Blk2 as TBlkVyhybka).Vyluka <> '') then
        bariery.Add(Self.JCBariera(_JCB_VYHYBKA_VYLUKA, Blk2, Blk2.id));

      // kontrola stitku vyhybky:
      if ((Blk2 as TBlkVyhybka).Stitek <> '') then
        bariery.Add(Self.JCBariera(_JCB_VYHYBKA_STITEK, Blk2, Blk2.id));

      // kontrola zamceni odvratu
      if ((Blk as TBlkVyhybka).Poloha <> odvratZaver.Poloha) then
       begin
        if ((Blk2 as TBlkVyhybka).Zaver > TZaver.no) then
         begin
          if ((Blk2 as TBlkVyhybka).Zaver = TZaver.ab) then
            bariery.Add(Self.JCBariera(_JCB_USEK_AB, Blk2, Blk2.id))
          else
            bariery.Add(Self.JCBariera(_JCB_USEK_ZAVER, Blk2, Blk2.id));
         end;

        if ((Blk2 as TBlkVyhybka).vyhZaver) then
          bariery.Add(Self.JCBariera(_JCB_VYHYBKA_NOUZ_ZAVER, Blk2, Blk2.id))
        else if ((Blk2 as TBlkVyhybka).outputLocked) then
          bariery.Add(Self.JCBariera(_JCB_VYHYBKA_ZAMCENA, Blk2, Blk2.id))
       end;
     end;
   end;//for i

  // kontrola trati
  if (Self.fproperties.Trat > -1) then
   begin
    Blky.GetBlkByID(Self.fproperties.Trat, Blk);

    // stitky a vyluky na tratovych usecich
    for usek in TBlkTrat(Blk).GetSettings().useky do
     begin
      Blky.GetBlkByID(usek, Blk2);

      // vyluka
      if (TBlkUsek(Blk2).Vyluka <> '') then
        bariery.Add(Self.JCBariera(_JCB_USEK_VYLUKA, blk2, blk2.id));

      // stitek
      if (TBlkUsek(Blk2).Stitek <> '') then
        bariery.Add(Self.JCBariera(_JCB_USEK_STITEK, blk2, blk2.id));
     end;

    // kontrola stitku uvazky v nasi OR:
    if ((TBlkUvazka(TBlkTrat(Blk).uvazkaA).OblsRizeni.Count > 0) and
        (TBlkUvazka(TBlkTrat(Blk).uvazkaA).OblsRizeni[0] = Self.fstaveni.senderOR) and
        (TBlkUvazka(TBlkTrat(Blk).uvazkaA).Stitek <> '')) then
      bariery.Add(Self.JCBariera(_JCB_TRAT_STITEK, TBlkUvazka(TBlkTrat(Blk).uvazkaA),
          TBlkUvazka(TBlkTrat(Blk).uvazkaA).id));

    if ((TBlkUvazka(TBlkTrat(Blk).uvazkaB).OblsRizeni.Count > 0) and
        (TBlkUvazka(TBlkTrat(Blk).uvazkaB).OblsRizeni[0] = Self.fstaveni.senderOR) and
        (TBlkUvazka(TBlkTrat(Blk).uvazkaB).Stitek <> '')) then
      bariery.Add(Self.JCBariera(_JCB_TRAT_STITEK, TBlkUvazka(TBlkTrat(Blk).uvazkaB),
          TBlkUvazka(TBlkTrat(Blk).uvazkaB).id));
   end;

end;

////////////////////////////////////////////////////////////////////////////////

procedure TJC.Log(msg: string; typ: Integer = WR_VC);
begin
 writelog('JC '+Self.name+': '+msg, typ);
end;

////////////////////////////////////////////////////////////////////////////////

// stavi konkretni jizdni cestu
// tato fce ma za ukol zkontrolovat vstupni podminky jizdni cesty
// tato funkce jeste nic nenastavuje!
function TJC.StavJC(SenderPnl:TIdContext; SenderOR:TObject; bariery_out:TJCBariery;
                    from_stack:TObject = nil; nc:boolean = false; fromAB:boolean = false;
                    abAfter: Boolean = false):Integer;
var i:Integer;
    bariery:TJCBariery;
    bariera:TJCBariera;
    critical:boolean;
    upo:TUPOItems;
    item:TUPOItem;
 begin
  Self.fstaveni.timeOut := Now + EncodeTime(0, _JC_INITPOTVR_TIMEOUT_SEC div 60, _JC_INITPOTVR_TIMEOUT_SEC mod 60, 0);

  Self.fstaveni.from_stack := from_stack;
  Self.fstaveni.senderOR := SenderOR;
  Self.fstaveni.senderPnl := SenderPnl;
  Self.fstaveni.nc := nc;
  Self.fstaveni.ab := (abAfter) and (Self.typ = TJCType.vlak);
  Self.fstaveni.prjWasClosed := false;
  Self.fstaveni.lastUsekOrTratObsaz := false;

  Self.Log('Požadavek na stavění, kontroluji podmínky');

  bariery := Self.KontrolaPodminek(Self.fstaveni.nc);
  upo := TList<TUPOItem>.Create;
  try
    // ignorujeme AB zaver pokud je staveno z AB seznamu
    if (fromAB) then
      for i := bariery.Count-1 downto 0 do
        if (bariery[i].typ = _JCB_USEK_AB) then
          bariery.Delete(i);

    // existuji kriticke bariery?
    critical := false;
    for bariera in bariery do
     begin
      if ((bariera.typ = _JCB_USEK_LAST_OBSAZENO) or (bariera.typ = _JCB_TRAT_OBSAZENO)) then
        Self.fstaveni.lastUsekOrTratObsaz := true;

      if ((Self.CriticalBariera(bariera.typ)) or (not Self.WarningBariera(bariera.typ))) then
       begin
        critical := true;
        upo.Add(Self.JCBarieraToMessage(bariera));
       end;
     end;

    if (critical) then
     begin
      // kriticke bariey existuji -> oznamim je
      Self.Log('Celkem '+IntToStr(bariery.Count)+' bariér, ukončuji stavění');
      if (SenderPnl <> nil) then
       begin
        Self.krok := _KROK_KRIT_BARIERY;
        ORTCPServer.UPO(Self.fstaveni.senderPnl, upo, true, nil, Self.CritBarieraEsc, Self);
       end;
      Exit(1);
     end else begin
      // bariery k potvrzeni
      if (((bariery.Count > 0) or ((nc) and (from_stack <> nil))) and (SenderPnl <> nil)) then
       begin
        Self.Log('Celkem '+IntToStr(bariery.Count)+' warning bariér, žádám potvrzení...');
        for i := 0 to bariery.Count-1 do
         upo.Add(Self.JCBarieraToMessage(bariery[i]));

        // pokud se jedna o NC ze zasobniku, zobrazuji jeste upozorneni na NC
        if ((nc) and (from_stack <> nil)) then
         begin
          item[0] := GetUPOLine('Pozor !', taCenter, clYellow, $A0A0A0);
          item[1] := GetUPOLine('Stavění nouzové cesty.');
          item[2] := GetUPOLine('');
          upo.Add(item);
         end;

        ORTCPServer.UPO(Self.fstaveni.senderPnl, upo, false, Self.UPO_OKCallback, Self.UPO_EscCallback, Self);
        Self.krok := _KROK_POTVR_BARIERY;
        Exit(0);
       end;
     end;

    // v jzdni ceste nejsou zadne bariery -> stavim
    Self.Log('Žádné bariéry, stavím');
    Self.SetInitKrok();
  finally
    if (bariery_out <> nil) then
      bariery_out.AddRange(bariery);
    bariery.Free();
    upo.Free();
  end;

  Result := 0;
 end;

function TJC.StavJC(SenderPnl:TIdContext; SenderOR:TObject;
  from_stack:TObject = nil; nc:boolean = false; fromAB:boolean = false;
  abAfter:Boolean = false):Integer;
begin
 Result := Self.StavJC(SenderPnl, SenderOR, nil, from_stack, nc, fromAB, abAfter);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TJC.PS_vylCallback(Sender:TIdContext; success:boolean);
var
    bariery:TJCBariery;
    critical:boolean;
    i:Integer;
begin
 // pro potvrzovaci sekvenci vyluky by mel byt krok '6'
 if (Self.krok <> _KROK_POTVR_SEKV) then Exit;

 if (not success) then
  begin
   Self.CancelStaveni('');
   Exit();
  end;

 // znovu zkontrolujeme bariery (behem potvrzovani se mohly vyskytnout)
 bariery := Self.KontrolaPodminek(Self.fstaveni.nc);

 // existuji kriticke bariery?
 critical := false;
 for i := 0 to bariery.Count-1 do
  if ((bariery[i].typ <> _JCB_STAVENI) and ((Self.CriticalBariera(bariery[i].typ)) or (not Self.WarningBariera(bariery[i].typ)))) then
   begin
    critical := true;
    break;
   end;

 // behem potvrzovani se mohly vyskytnout
 if (critical) then
  begin
   Self.CancelStaveni('Nelze postavit - kritické bariéry');
   if (Self.fstaveni.senderPnl <> nil) and (Self.fstaveni.senderOR <> nil) then
     ORTCPServer.BottomError(Self.fstaveni.senderPnl, 'Nelze postavit '+Self.name+' - kritické bariéry',
        (Self.fstaveni.senderOR as TOR).ShortName, 'TECHNOLOGIE');
   bariery.Free();
   Exit();
  end;

 Self.Log('Krok 2 : povrzovaci sekvence OK');
 Self.SetInitKrok();
end;

////////////////////////////////////////////////////////////////////////////////
// callbacky z upozornovacich barier:

procedure TJC.UPO_OKCallback(Sender:TObject);
var
    bariery:TJCBariery;
    critical:boolean;
    i:Integer;
    nav:TBlk;
    podm:TList<TPSPodminka>;
begin
 if (Self.krok <> _KROK_POTVR_BARIERY) then Exit();

 Self.Log('Krok 1 : upozornění schválena, kontroluji znovu bariéry');

 // znovu zkontrolujeme bariery (behem potvrzovani se mohly vyskytnout)
 bariery := Self.KontrolaPodminek(Self.fstaveni.nc);

 // existuji kriticke bariery?
 critical := false;
 for i := 0 to bariery.Count-1 do
  if ((bariery[i].typ <> _JCB_STAVENI) and ((Self.CriticalBariera(bariery[i].typ)) or (not Self.WarningBariera(bariery[i].typ)))) then
   begin
    critical := true;
    break;
   end;

 // behem potvrzovani se mohly vyskytnout
 if (critical) then
  begin
   Self.CancelStaveni('Nelze postavit - kritické bariéry');
   if (Self.fstaveni.senderPnl <> nil) and (Self.fstaveni.senderOR <> nil) then
     ORTCPServer.BottomError(Self.fstaveni.senderPnl, 'Nelze postavit '+Self.name+' - kritické bariéry',
        (Self.fstaveni.senderOR as TOR).ShortName, 'TECHNOLOGIE');
   bariery.Free();
   Exit();
  end;

 // existuji bariery na potvrzeni potvrzovaci sekvenci ?
 podm := TList<TPSPodminka>.Create;
 for i := 0 to bariery.Count-1 do
  begin
   if (Self.PotvrSekvBariera(bariery[i].typ)) then
     podm.Add(TOR.GetPSPodminka(bariery[i].blok, TJC.PotvrSekvBarieraToReason(bariery[i].typ)));
  end;//for i

 if (podm.Count > 0) then
  begin
   // ano, takoveto bariery existuji -> potvrzovaci sekvence
   Self.Log('Bariéry s potvrzovací sekvencí, žádám potvrzení...');
   Blky.GetBlkByID(Self.fproperties.navestidloBlok, nav);

   if (Self.fstaveni.senderPnl <> nil) and (Self.fstaveni.senderOR <> nil) then
     ORTCPServer.Potvr(Self.fstaveni.senderPnl, Self.PS_vylCallback, (Self.fstaveni.senderOR as TOR),
        'Jízdní cesta s potvrzením', TBlky.GetBlksList(nav, Self.lastUsek), podm);

   Self.krok := _KROK_POTVR_SEKV;
  end else begin
   // ne, takoveto bariery neexistuji -> stavim jizdni cestu
   Self.SetInitKrok();
  end;
end;//proceudre

procedure TJC.UPO_EscCallback(Sender:TObject);
begin
 if (Self.krok = _KROK_POTVR_BARIERY) then
  begin
   Self.CancelStaveni();
   Self.krok := _KROK_DEFAULT;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

// jakmile je zavolano StavJC(), tato funkce se stara o to, aby staveni doslo az do konce
// kontroluje prubezne podminky apod.
procedure TJC.UpdateStaveni();
var i,j:Integer;
    blk:TBlk;
    aZaver:TJCType;
    neprofil:TBlkUsek;
    uzavren,anyUzavren:boolean;
    str:string;
    npCall:^TNPCallerData;
    count:Integer;
    stavim:Cardinal;
    bariery:TList<TJCBariera>;
    bariera:TJCBariera;
    nextVyhybka:Integer;
    uzavBlok:Integer;
    usekZaver:Integer;
    prjZaver:TJCPrjZaver;
    vyhZaver:TJCVyhZaver;
    odvratZaver:TJCOdvratZaver;
    refZaver:TJCRefZaver;
    vyhybka:TBlkVyhybka;
    usek, nextUsek:TBlkUsek;
    zamek:TBlkZamek;
    prejezd:TBlkPrejezd;
    navestidlo:TBlkNav;
    trat:TBlkTrat;
    oblr:TOR;
    tuAdd:TBlkTU;
    train: TTrain;
    chEv: TChangeEvent;
    remEvDataPtr: ^TRemoveEventData;
 begin
  if ((not Self.Staveni) and (Self.krok <> _JC_KROK_CEKANI_POSLEDNI_USEK)) then Exit;

  Blky.GetBlkByID(Self.fproperties.navestidloBlok, TBlk(navestidlo));

  //////////////////////////////////////////////////////////////////////////////
  // staveni vlakovych a posunovych cest:

  case (Self.krok) of
   _JC_KROK_INIT:begin
      // nejprve priradime uvolneni zaveru posledniho bloku uvolneni zaveru predposledniho bloku
      if (Self.fproperties.useky.Count > 1) then
       begin
        Blky.GetBlkByID(Self.fproperties.useky[Self.fproperties.useky.Count-2], TBlk(usek));
        usek.AddChangeEvent(usek.EventsOnZaverReleaseOrAB,
          CreateChangeEvent(ceCaller.CopyUsekZaver, Self.lastUsek.id));

        for i := 0 to Self.fproperties.useky.Count-2 do
         begin
          Blky.GetBlkByID(Self.fproperties.useky[i], TBlk(usek));
          Blky.GetBlkByID(Self.fproperties.useky[i+1], TBlk(nextUsek));

          if (usek.Stav.stanicni_kolej) then
           begin
            chEv := CreateChangeEvent(ceCaller.CopyUsekZaver, usek.id);
            TBlk.AddChangeEvent(nextUsek.EventsOnZaverReleaseOrAB, chEv);
            GetMem(remEvDataPtr, SizeOf(TRemoveEventData));
            remEvDataPtr^ := TRemoveEventData.Create(nextUsek.EventsOnZaverReleaseOrAB, chEv);
            TBlk.AddChangeEvent(usek.EventsOnZaverReleaseOrAB, CreateChangeEvent(ceCaller.RemoveEvent, Integer(remEvDataPtr)));
           end;
         end;
       end;

      Self.Log('Useky: nastavuji staveci zavery');
      for usekZaver in Self.fproperties.useky do
       begin
        Blky.GetBlkByID(usekZaver, TBlk(usek));
        usek.Zaver := TZaver.staveni;
       end;//for cyklus

      Self.Log('Vyhybky: zamykam do pozadovanych poloh');
      Self.fstaveni.nextVyhybka := -1;
      stavim := 0;
      nextVyhybka := -1;
      for i := 0 to Self.fproperties.vyhybky.Count-1 do
       begin
        vyhZaver := Self.fproperties.vyhybky[i];

        Blky.GetBlkByID(Self.fproperties.vyhybky[i].Blok, TBlk(vyhybka));
        if (vyhybka.Poloha <> TVyhPoloha(vyhZaver.Poloha)) then
         begin
          if (stavim >= _JC_MAX_VYH_STAVENI) then
           begin
            if (nextVyhybka = -1) then
              nextVyhybka := i;
            continue;
           end;
          Inc(stavim);
         end;

        // Warning: this may call callback directly
        // Callback for just-locking turnout will have no effect due to nextVyhybka = -1
        vyhybka.SetPoloha(TVyhPoloha(vyhZaver.Poloha),
                          true, false, Self.VyhPrestavenaJCPC, Self.VyhNeprestavenaJCPC);
       end;

      for i := 0 to Self.fproperties.odvraty.Count-1 do
       begin
        odvratZaver := Self.fproperties.odvraty[i];

        // nastaveni odvratu
        Blky.GetBlkByID(odvratZaver.Blok, TBlk(vyhybka));
        if (vyhybka.Poloha <> TVyhPoloha(odvratZaver.Poloha)) then
         begin
          if (stavim >= _JC_MAX_VYH_STAVENI) then
           begin
            if (nextVyhybka = -1) then
              nextVyhybka := i;
            continue;
           end;
          Inc(stavim);
         end;

        vyhybka.IntentionalLock();

        // pridani zruseni redukce
        Blky.GetBlkByID(odvratZaver.ref_blk, TBlk(usek));
        usek.AddChangeEvent(usek.EventsOnZaverReleaseOrAB,
          CreateChangeEvent(ceCaller.NullVyhybkaMenuReduction, odvratZaver.Blok));

        // Warning: this may call callback directly
        // Callback for just-locking turnout will have no effect due to nextVyhybka = -1
        vyhybka.SetPoloha(TVyhPoloha(odvratZaver.Poloha),
                          true, false, Self.VyhPrestavenaJCPC, Self.VyhNeprestavenaJCPC);
       end;

      Self.fstaveni.nextVyhybka := nextVyhybka;

      Self.Log('Zamky: nastavuji zavery');
      for refZaver in Self.fproperties.zamky do
       begin
        Blky.GetBlkByID(refZaver.ref_blk, TBlk(usek));
        usek.AddChangeEvent(usek.EventsOnZaverReleaseOrAB,
          CreateChangeEvent(ceCaller.NullZamekZaver, refZaver.Blok));

        // nastaveni zaveru zamku
        Blky.GetBlkByID(refZaver.Blok, TBlk(zamek));
        zamek.Zaver := true;
       end;

      Self.krok := _JC_KROK_CEKANI_VYHYBKA_POLOHA;
      Self.Log('Vyhybky: poloha: detekce');
     end;//case 0


   _JC_KROK_CEKANI_VYHYBKA_POLOHA:begin
      for vyhZaver in Self.fproperties.vyhybky do
       begin
        Blky.GetBlkByID(vyhZaver.Blok, TBlk(vyhybka));
        if (vyhybka.Poloha <> vyhZaver.Poloha) then
          Exit;
       end;
      for odvratZaver in Self.fproperties.odvraty do
       begin
        Blky.GetBlkByID(odvratZaver.Blok, TBlk(vyhybka));
        if (vyhybka.Poloha <> odvratZaver.Poloha) then
          Exit;
       end;

      Self.Log('Krok 11 : vyhybky: poloha: OK');
      Self.fstaveni.nextVyhybka := -1;

      Self.Log('Krok 11: useky: nastavuji nouzovy zaver');
      for usekZaver in Self.fproperties.useky do
       begin
        Blky.GetBlkByID(usekZaver, TBlk(usek));
        usek.Zaver := TZaver.nouz;
       end;

      Self.Log('Krok 11: useky: kontroluji volnost useku s neprofilovymi styky, zapevnuji neprofilove useky');
      for vyhZaver in Self.fproperties.vyhybky do
       begin
        neprofil := nil;
        Blky.GetBlkByID(vyhZaver.Blok, TBlk(vyhybka));

        if ((vyhZaver.Poloha = TVyhPoloha.plus) and (vyhybka.npBlokPlus <> nil)) then
          neprofil := TBlkUsek(vyhybka.npBlokPlus)
        else if ((vyhZaver.Poloha = TVyhPoloha.minus) and (vyhybka.npBlokMinus <> nil)) then
          neprofil := TBlkUsek(vyhybka.npBlokMinus);

        if (neprofil <> nil) then
         begin
          if (neprofil.Obsazeno <> TUsekStav.uvolneno) then
           begin
            if (Self.fstaveni.senderPnl <> nil) and (Self.fstaveni.senderOR <> nil) then
              ORTCPServer.BottomError(Self.fstaveni.senderPnl, 'Neuvolněn ' + neprofil.name,
                  (Self.fstaveni.senderOR as TOR).ShortName, 'TECHNOLOGIE');
            Self.Log('Krok 14 : Neprofilovy usek '+neprofil.name+' neuvolnen!');
            Self.CancelStaveni();
            Exit();
           end;

          neprofil.AddNeprofilJC(Self.fproperties.id);

          Blky.GetBlkByID(vyhybka.UsekID, TBlk(usek));

          npCall := GetMemory(SizeOf(TNPCallerData));
          npCall.usekId := neprofil.id;
          npCall.jcId   := Self.fproperties.id;
          usek.AddChangeEvent(usek.EventsOnZaverReleaseOrAB,
              CreateChangeEvent(ceCaller.RemoveUsekNeprofil, Integer(npCall)));
         end;
       end;

      if ((navestidlo.ZAM) or (Self.fstaveni.lastUsekOrTratObsaz)) then
        Self.krok := _JC_KROK_FINALNI_ZAVER
      else
        Self.krok := _JC_KROK_ZAVRIT_PREJEZDY;
     end;//case 1


   _JC_KROK_ZAVRIT_PREJEZDY:begin
       // prejezdy
       Self.fstaveni.prjWasClosed := true;
       anyUzavren := false;
       for i := 0 to Self.fproperties.prejezdy.Count-1 do
        begin
         prjZaver := Self.fproperties.prejezdy[i];
         if (prjZaver.uzaviraci.Count = 0) then
           continue;

         Blky.GetBlkByID(prjZaver.Prejezd, TBlk(prejezd));
         uzavren := false;

         // prejezd uzavirame jen v pripade, ze nejaky z jeho aktivacnich bloku je obsazen
         // v pripade posunove cesty uzavirame vzdy

         if (Self.typ = TJCType.posun) then
          begin
           // posunova cesta:
           Self.Log('Krok 12 : prejezd '+prejezd.name+' - uzaviram');

           prejezd.Zaver := true;

           // pridani zruseni redukce, tim se prejezd automaticky otevre po zruseni zaveru bloku pod nim
           Blky.GetBlkByID(prjZaver.oteviraci, TBlk(usek));
           usek.AddChangeEvent(usek.EventsOnZaverReleaseOrAB,
             CreateChangeEvent(ceCaller.NullPrejezdZaver, prjZaver.Prejezd));

           uzavren := true;
           anyUzavren := true;
          end else begin

           // vlakova cesta:
           for uzavBlok in prjZaver.uzaviraci do
            begin
             Blky.GetBlkByID(uzavBlok, TBlk(usek));
             if (usek.Obsazeno = TusekStav.obsazeno) then
              begin
               Self.Log('Krok 12 : prejezd '+prejezd.name+' - aktivacni usek '+usek.name+' obsazen - uzaviram');

               prejezd.Zaver := true;

               // pridani zruseni redukce, tim se prejezd automaticky otevre po zruseni zaveru bloku pod nim
               Blky.GetBlkByID(prjZaver.oteviraci, TBlk(usek));
               usek.AddChangeEvent(usek.EventsOnZaverReleaseOrAB,
                 CreateChangeEvent(ceCaller.NullPrejezdZaver, prjZaver.Prejezd));

               uzavren := true;
               anyUzavren := true;
               break;
              end;
            end;//for j
          end;// else posunova cesta

         if (not uzavren) then
          begin
           // prejezd neuzaviram -> pridam pozadavek na zavreni pri obsazeni do vsech aktivacnich useku
           for uzavBlok in prjZaver.uzaviraci do
            begin
             Blky.GetBlkByID(uzavBlok, TBlk(usek));
             if (not usek.EventsOnObsaz.Contains(CreateChangeEvent(Self.UsekClosePrj, i))) then
               usek.AddChangeEvent(usek.EventsOnObsaz, CreateChangeEvent(Self.UsekClosePrj, i));
            end;

           Self.Log('Krok 12 : prejezd '+prejezd.name+' - zadny aktivacni usek neobsazen - nechavam otevreny');
          end;
        end;//for i

      if (anyUzavren) then
       begin
        Self.krok := _JC_KROK_CEKANI_PREJEZDY;
        Self.fstaveni.timeOut := Now + EncodeTime(0, _JC_PRJ_TIMEOUT_SEC div 60, _JC_PRJ_TIMEOUT_SEC mod 60, 0);
       end else
        Self.krok := _JC_KROK_FINALNI_ZAVER;

     end;


   _JC_KROK_CEKANI_PREJEZDY:begin
       // kontrola stavu prejezdu
       for prjZaver in Self.fproperties.prejezdy do
        begin
         if (prjZaver.uzaviraci.Count = 0) then
           continue;

         Blky.GetBlkByID(prjZaver.Prejezd, TBlk(prejezd));

         if (prejezd.Stav.basicStav <> TBlkPrjBasicStav.uzavreno) then Exit();
         Self.Log('Krok 13 : prejezd '+prejezd.name+' uzavren');
        end;//for i

      Self.krok := _JC_KROK_FINALNI_ZAVER;
     end;


   _JC_KROK_FINALNI_ZAVER:begin
      Self.Log('Krok 14 : useky: nastavit validni zaver');

      aZaver := Self.typ;

      for i := 0 to Self.fproperties.useky.Count-1 do
       begin
        usekZaver := Self.fproperties.useky[i];
        Blky.GetBlkByID(usekZaver, TBlk(usek));
        usek.Zaver := TZaver(aZaver);
       end;

      navestidlo.DNjc := Self;

      if (Self.PorusenaKritickaPodminka()) then
       begin
        // Nepostavit navestidlo!
        Self.krok := _JC_KROK_FINISH;
        Exit();
       end;

      if ((navestidlo.ZAM) or (Self.fstaveni.lastUsekOrTratObsaz)) then
       begin
        Self.Log('Krok 14 : navestidlo: nestavim');
        Self.krok := _JC_KROK_FINISH;
       end else begin
        Self.Log('Krok 14 : navestidlo: stavim...');
        Self.NastavNav();
        Self.krok := _JC_KROK_CEKANI_NAVESTIDLO;
       end;
   end;// case 14

   _JC_KROK_CEKANI_NAVESTIDLO:begin
     if (navestidlo.navest > ncStuj) then
      begin
       Self.Log('Krok 15 : navestidlo postaveno');
       Self.krok := _JC_KROK_FINISH;
      end;
   end;

   _JC_KROK_FINISH:begin
      Self.RusZacatekJC();
      Self.RusVBJC();
      Self.RusKonecJC();

      // nastavit front blok soupravy
      usek := navestidlo.UsekPred as TBlkUsek;
      if (usek.IsTrain()) then
        Self.GetTrain(Navestidlo, usek).front := usek;

      if (not usek.NavJCRef.Contains(Navestidlo)) then
        usek.NavJCRef.Add(Navestidlo);

      navestidlo.DNjc := Self;

      if (Self.fstaveni.lastUsekOrTratObsaz) then
        Self.krok := _JC_KROK_CEKANI_POSLEDNI_USEK
      else
        Self.krok := _KROK_DEFAULT;

      // kdyby nastala nize chyba, musi byt moznost JC smazat ze zasobniku
      if (Self.fstaveni.from_stack <> nil) then
        (Self.fstaveni.from_stack as TORStack).firstEnabled := true;

      // Kontrola kritickych podminek.
      // (behem staveni mohla nastat zmena)
      if (Self.PorusenaKritickaPodminka()) then
       begin
        if (navestidlo.Navest <> ncStuj) then
          navestidlo.Navest := ncStuj;
        if (Self.fstaveni.senderPnl <> nil) and (Self.fstaveni.senderOR <> nil) then
          ORTCPServer.BottomError(Self.fstaveni.senderPnl, 'Podmínky pro JC nesplněny!',
            (Self.fstaveni.senderOR as TOR).ShortName, 'TECHNOLOGIE');
        Self.Log('Krok 16 : Podmínky pro JC nesplněny!');
        Exit();
       end;

      // trat
      // zruseni redukce posledniho bloku jizdni cesty je navazano na zruseni zaveru trati
      // -> jakmile dojde ke zruseni zaveru posledniho bloku, dojde ke zruseni zaveru trati
      if (Self.fproperties.Trat > -1) then
       begin
        Blky.GetBlkByID(Self.fproperties.Trat, TBlk(trat));

        if (Self.typ = TJCType.vlak) then trat.Zaver := true;

        // posledni blok posunove cesty je trat = posun mezi dopravnami -> zavedeme zakaz odjezdu do trati
        if (Self.typ = TJCType.posun) then
         begin
          case (Self.fproperties.TratSmer) of
           TTratSmer.AtoB : TBlkUvazka(trat.uvazkaA).ZAK := true;
           TTratSmer.BtoA : TBlkUvazka(trat.uvazkaB).ZAK := true;
          end;
         end;

        trat.Smer := Self.fproperties.TratSmer;

        // zruseni zaveru posledniho bloku JC priradime zruseni zaveru trati
        Self.lastUsek.AddChangeEvent(TBlkUsek(Self.lastUsek).EventsOnZaverReleaseOrAB,
          CreateChangeEvent(ceCaller.NullTratZaver, Self.fproperties.Trat));
       end;

      if ((navestidlo.ZAM) or (Self.fstaveni.lastUsekOrTratObsaz)) then
        Self.rozpadBlok := -2
      else
        Self.rozpadBlok := -1;
      Self.rozpadRuseniBlok := -2;

      if (Self.typ = TJCType.vlak) then
        Blky.TrainPrediction(Navestidlo);

      // pokud je cesta ze zasobniku, smazeme ji odtam
      if (Self.fstaveni.from_stack <> nil) then
       begin
        (Self.fstaveni.from_stack as TORStack).RemoveJC(Self);
        Self.fstaveni.from_stack := nil;
       end;

      navestidlo.PropagatePOdjToTrat();

      if ((Self.fstaveni.ab) and (not navestidlo.AB)) then
        navestidlo.ABJC := Self;

      Self.Log('Postavena JC '+Self.name);
   end;

   _JC_KROK_CEKANI_POSLEDNI_USEK: begin
     blk := Self.lastUsek;

     if (Self.fproperties.Trat > -1) then
      begin
       if (TBlkTU(blk).sectReady) then
        begin
         Self.fstaveni.lastUsekOrTratObsaz := false;
         Self.DN();
        end;
      end else begin
       if ((TBlkUsek(blk).Obsazeno = TUsekStav.uvolneno) and (not TBlkUsek(blk).IsTrain)) then
        begin
         Self.fstaveni.lastUsekOrTratObsaz := false;
         Self.DN();
        end;
      end;
   end;

   ///////////////////////////////////////////////////////////////////////////
   // staveni nouzovych cest:

   _NC_KROK_INIT:begin
    // vsem usekum nastavime staveci zaver:
    Self.Log('Krok 100: useky: nastavuji staveci zavery');
    for usekZaver in Self.fproperties.useky do
     begin
      Blky.GetBlkByID(usekZaver, TBlk(usek));
      usek.Zaver := TZaver.staveni;
     end;//for cyklus

    // nastavit nouzovy zaver uvazky
    if (Self.fproperties.Trat > -1) then
     begin
      Self.Log('Krok 100: trat: nastavuji nouzovy zaver uvazky');
      Blky.GetBlkByID(Self.fproperties.Trat, TBlk(trat));

      // najdeme si uvazku, ktera je v OR navestidla a te nastavime nouzovy zaver
      if ((trat.uvazkaA as TBlkUvazka).OblsRizeni.Count > 0) then
       begin
        for oblr in navestidlo.OblsRizeni do
          if ((trat.uvazkaA as TBlkUvazka).OblsRizeni[0] = oblr) then
             (trat.uvazkaA as TBlkUvazka).nouzZaver := true;

        for oblr in navestidlo.OblsRizeni do
          if ((trat.uvazkaB as TBlkUvazka).OblsRizeni[0] = oblr) then
             (trat.uvazkaB as TBlkUvazka).nouzZaver := true;
       end;
     end;

    // nastavit vyhybky do pozadovanych poloh:
    Self.Log('Krok 100: vyhybky: nastavuji do pozadovanych poloh');

    Self.fstaveni.nextVyhybka := 0;

    while ((Self.fstaveni.nextVyhybka <> -1) and (Self.fstaveni.nextVyhybka < _JC_MAX_VYH_STAVENI) and
           (Self.fstaveni.nextVyhybka < Self.fproperties.vyhybky.Count)) do
     begin
      vyhZaver := Self.fproperties.vyhybky[Self.fstaveni.nextVyhybka];
      Blky.GetBlkByID(vyhZaver.Blok, TBlk(vyhybka));

      Inc(Self.fstaveni.nextVyhybka);
      vyhybka.SetPoloha(TVyhPoloha(vyhZaver.Poloha), // this call could increase nextVyhybka directly! or even set nextVyhybka = -1
                        true, false, Self.VyhPrestavenaNC, Self.VyhNeprestavenaNC);
     end;

    // For simplicity solve odvrat just in callback
    // This may be a little bit slower, but will generally work fine

    Self.Log('Krok 100: prejezdy: uzaviram');
    for prjZaver in Self.fproperties.prejezdy do
     begin
      if (prjZaver.uzaviraci.Count = 0) then
        continue;

      Blky.GetBlkByID(prjZaver.Prejezd, TBlk(prejezd));
      if (not prejezd.NOtevreni) then
        prejezd.UZ := true;
     end;

    // nastavit nouzovy zaver zamkum
    for refZaver in Self.fproperties.zamky do
     begin
      Blky.GetBlkByID(refZaver.Blok, TBlk(zamek));
      zamek.nouzZaver := true;
      navestidlo.AddBlkToRnz(zamek.id, false);
     end;

    Self.fstaveni.ncBarieryCntLast := -1;   // tady je potreba mit cislo < 0

    Self.krok := _NC_KROK_BARIERA_UPDATE;
   end;//case 100

   _NC_KROK_BARIERA_UPDATE:begin
    // prubezne kontroluji podminky a zobrazuji potvrzovaci sekvenci

    // zjistime aktualni bariery:
    Self.fstaveni.ncBariery.Clear();
    Self.PodminkyNCStaveni(Self.fstaveni.ncBariery);

    // kontrolujeme rozdilnost seznamu:
    if (Self.fstaveni.ncBariery.Count <> Self.fstaveni.ncBarieryCntLast) then
     begin
      Self.Log('Krok 101: zmena potvr., odesilam aktualni seznam');
      if (Self.typ = TJCType.vlak) then
        str := 'Zapnutí přivolávací návěsti'
      else
        str := 'Nouzová posunová cesta';

      if (Self.fstaveni.senderPnl <> nil) and (Self.fstaveni.senderOR <> nil) then
        ORTCPServer.Potvr(Self.fstaveni.senderPnl, Self.NC_PS_Callback, Self.fstaveni.senderOR as TOR,
          str, TBlky.GetBlksList(Navestidlo, lastUsek), Self.BarieryNCToPotvr(Self.fstaveni.ncBariery));
     end;
    Self.fstaveni.ncBarieryCntLast := Self.fstaveni.ncBariery.Count;

    // nastavovani smeru trati:
    if (Self.fproperties.Trat > -1) then
     begin
      Blky.GetBlkByID(Self.fproperties.Trat, TBlk(trat));

      // pokud v trati neni zavedena blokova podminka, zavedeme ji
      if ((Self.typ = TJCType.vlak) and (trat.Smer = Self.data.TratSmer) and (not trat.BP)) then
        trat.BP := true;

      // posledni blok posunove cesty je trat = posun mezi dopravnami -> zavedeme zakaz odjezdu do trati
      if ((Self.typ = TJCType.posun) and (trat.Smer = Self.fproperties.TratSmer)) then
       begin
        case (Self.fproperties.TratSmer) of
         TTratSmer.AtoB : if (not TBlkUvazka(trat.uvazkaA).ZAK) then TBlkUvazka(trat.uvazkaA).ZAK := true;
         TTratSmer.BtoA : if (not TBlkUvazka(trat.uvazkaB).ZAK) then TBlkUvazka(trat.uvazkaB).ZAK := true;
        end;
       end;
     end;
   end;

   _NC_KROK_BARIERY_POTVRZENY:begin
    // potrvzovaci sekvence potvrzena -> stavim navestidlo, ...

    Self.fstaveni.nextVyhybka := -1;
    Self.Log('Krok 102: useky: rusim zavery');
    for usekZaver in Self.fproperties.useky do
     begin
      Blky.GetBlkByID(usekZaver, TBlk(usek));
      usek.Zaver := TZaver.no;
     end;//for cyklus

    navestidlo.privol := Self;

    // i pokud je navetidlo ve STUJ, nastavuji navest (to je spravne chovani podle JOP)
    if ((Self.typ = TJCType.vlak) and (navestidlo.enabled)) then
     begin
      Self.NastavNav();
      Self.Log('Krok 102 : navestidlo: nastavuji na privolavaci navest...');
      Self.krok := _NC_KROK_CEKANI_NAVESTIDLO;
     end else
      Self.krok := _NC_KROK_FINISH;
   end;

   _NC_KROK_CEKANI_NAVESTIDLO:begin
     if (navestidlo.Navest = ncPrivol) then
      begin
       Self.Log('Krok 103 : navestidlo postaveno');
       Self.krok := _NC_KROK_FINISH;
      end;
   end;

   _NC_KROK_FINISH:begin
    Self.RusZacatekJC();
    Self.RusVBJC();
    Self.RusKonecJC();

    Self.krok := _KROK_DEFAULT;

    // pokud je cesta ze zasobniku, smazeme ji odtam
    if (Self.fstaveni.from_stack <> nil) then
     begin
      (Self.fstaveni.from_stack as TORStack).RemoveJC(Self);
      Self.fstaveni.from_stack := nil;
     end;

    // presun soupravy z useku pred navestidlem do posledniho useku JC

    // Presun probehne za techto podminek:
    //  a) Bud privolavame do stanice = na dopravni kolej
    //  b) Nebo privolavame do trate, ktera MUSI byt ve spravnem smeru a MUSI v ni byt zavedena blokova podminka

    if (Self.typ = TJCType.vlak) then
     begin
      usek := navestidlo.UsekPred as TBlkUsek;
      train := Self.GetTrain(Navestidlo, usek);

      // a)
      if ((lastUsek.typ = btUsek) and (TBlkUsek(Self.lastUsek).Stav.stanicni_kolej) and
          (not TBlkusek(Self.lastUsek).TrainsFull())) then
       begin
        if (usek.IsTrain()) then
         begin
          if ((usek.typ = btTU) and (TBlkTU(usek).InTrat > -1)) then
           begin
            Blky.GetBlkByID((usek as TBlkTU).InTrat, TBlk(trat));
            trat.RemoveTrain(train);
           end;

          // na dopravni kolej vlozime soupravu blize vjezdovemu navestidlu
          if (navestidlo.Smer = THVStanoviste.lichy) then
            TBlkUsek(Self.lastUsek).AddTrainL(train)
          else
            TBlkUsek(Self.lastUsek).AddTrainS(train);

          usek.RemoveTrain(train);
         end;
        Self.fstaveni.rozpadBlok := -6;
       end;

      // b)
      if ((lastUsek.typ = btTU) and ((lastUsek as TBlkTU).InTrat > -1)) then
        Blky.GetBlkByID((lastUsek as TBlkTU).InTrat, TBlk(trat))
      else
        trat := nil;

      if ((trat <> nil) and (usek.IsTrain()) and (lastUsek.typ = btTU) and
          ((lastUsek as TBlkTU).InTrat = Self.data.Trat)) then
       begin
        tuAdd := nil;

        if (trat.vyluka) then
         begin
          // Pridat soupravu do posledniho bloku trati
          if ((trat.stav.trains.Count = 0) and ((trat.GetLastUsek(Self.data.TratSmer) as TBlkTU).Zaver = TZaver.no)) then
           begin
            tuAdd := (trat.GetLastUsek(Self.data.TratSmer) as TBlkTU);
            trat.TrainChangeOR(train, Self.data.TratSmer);
            if (trat.ChangesTrainDir()) then
              train.ChangeSmer();
          end;
         end else begin
          if ((not TBlkUsek(Self.lastUsek).IsTrain()) and (trat.BP) and (trat.Smer = Self.data.TratSmer)) then
           begin
            // Pridat soupravu do prvniho bloku trati
            tuAdd := (lastUsek as TBlkTU);
            tuAdd.poruchaBP := true;
           end;
         end;

        if (tuAdd <> nil) then
         begin
          trat.AddTrain(TBlkTratTrain.Create(train.index));
          tuAdd.AddTrainL(train); // tady je jedno jestli zavolat L nebo S
                                   // v trati muze byt na jednom useku vzdy jen jedna souprava
                                   // kontrolovano vyse
          trat.Change();
          usek.RemoveTrain(train);
         end;
       end;
     end;//if typ = vlak

    Self.Log('Postavena NC '+Self.name);
   end;//case 102
   end;//case
 end;

////////////////////////////////////////////////////////////////////////////////

// je volana, pokud behem staveni dojde k vyjimce
// napriklad pri kontrole obsazenosti useku v JC apod.
procedure TJC.CancelStaveni(reason: string = ''; stack_remove:boolean = false);
var usekZaver:Integer;
    usek:TBlkUsek;
begin
 if (reason <> '') then
  begin
   if (Self.fstaveni.senderPnl <> nil) then
     ORTCPServer.SendInfoMsg(Self.fstaveni.senderPnl, reason);
   Self.Log('Nelze postavit - '+reason);
  end;

 case (Self.krok) of
    _NC_KROK_BARIERA_UPDATE:begin
      if (Self.fstaveni.senderPnl <> nil) then
        ORTCPServer.PotvrClose(Self.fstaveni.senderPnl, reason);
    end   
 end;//case Self.krok

 // staveci zavery jsou zruseny, ostatni zavery zustavaji (lze je vyNUZovat)
 for usekZaver in Self.data.useky do
  begin
   Blky.GetBlkByID(usekZaver, TBlk(usek));
   if (usek.Zaver = TZaver.staveni) then
      usek.Zaver := no;
  end;

 Self.fstaveni.nextVyhybka := -1;
 Self.krok := _KROK_DEFAULT;
 Self.fstaveni.nc := false;
 Self.fstaveni.ab := false;
 Self.fstaveni.prjWasClosed := false;
 Self.RusZacatekJC();
 Self.RusVBJC();
 Self.RusKonecJC();
 if (Self.fstaveni.senderPnl <> nil) then
   ORTCPServer.CancelUPO(Self.fstaveni.senderPnl, Self);
 if (Self.fstaveni.from_stack <> nil) then
    if (stack_remove) then (Self.fstaveni.from_stack as TORStack).RemoveJC(Self)
  else
   if (Self.fstaveni.senderOR <> nil) then
     (Self.fstaveni.senderOR as TOR).BroadcastData('ZAS;FIRST;1');

 Self.fstaveni.from_stack := nil;
end;

////////////////////////////////////////////////////////////////////////////////

//rusi zacatek jizdni cesty
procedure TJC.RusZacatekJC();
var Blk:TBlk;
 begin
  Blky.GetBlkByID(Self.fproperties.navestidloBlok, Blk);
  if (Blk = nil) then Exit;
  if (Blk.typ <> btNav) then Exit;
  if ((Blk as TBlkNav).ZacatekVolba = TBlkNavVolba.none) then Exit;

  (Blk as TBlkNav).ZacatekVolba := TBlkNavVolba.none;
  if ((Blk as TBlkNav).DNjc = Self) then
    (Blk as TBlkNav).DNjc := nil;

  Self.Log('Zrusen zacatek staveni VC na bloku '+Blk.name);
 end;

//rusi konec jizdni cesty
procedure TJC.RusKonecJC();
 begin
  if (Self.lastUsek <> nil) then
    TBlkUsek(Self.lastUsek).KonecJC := no;
 end;

procedure TJC.RusVBJC();
var Blk:TBlk;
    vb:Integer;
begin
 for vb in Self.data.vb do
  begin
   Blky.GetBlkByID(vb, Blk);
   if ((Blk <> nil) and ((Blk.typ = btUsek) or (Blk.typ = btTU))) then
     (Blk as TBLkUsek).KonecJC := TZaver.no;
  end; 
end;

////////////////////////////////////////////////////////////////////////////////

//ruseni jizdni cesty
procedure TJC.RusJC(Sender:TObject = nil);
var usekZaver: Integer;
    usek: TBlkUsek;
    nav: TBlkNav;
 begin
  Self.RusJCWithoutBlk();

  Blky.GetBlkByID(Self.fproperties.navestidloBlok, TBlk(nav));
  nav.DNjc := nil;
  nav.RCtimerTimeout();

  for usekZaver in Self.fproperties.useky do
   begin
    Blky.GetBlkByID(usekZaver, TBlk(usek));
    usek.Zaver := no;
   end;

  // zaver trati se rusi automaticky uvolnenim zaveru posledniho bloku pred trati

  Self.Log('Zrusena');
 end;

//ruseni jizdni cesty bez ruseni zaveru bloku
procedure TJC.RusJCWithoutBlk();
var Nav:TBlk;
 begin
  Blky.GetBlkByID(Self.fproperties.navestidloBlok, Nav);
  Self.Log('Probiha ruseni navesti');

  if (((Nav as TBlkNav).DNjc = self) and ((Nav as TBlkNav).Navest > ncStuj)) then
   begin
    (Nav as TBlkNav).Navest := ncStuj;
    if ((Nav as TBlkNav).AB) then
     begin
      (Nav as TBlkNav).AB := false; // automaticky zrusi AB
      if (Self.fstaveni.senderPnl <> nil) then
        ORTCPServer.BottomError(Self.fstaveni.senderPnl, 'Zrušena AB '+Nav.name,
          (Self.fstaveni.senderOR as TOR).ShortName, 'TECHNOLOGIE');
     end;
   end;

  Self.krok := _KROK_DEFAULT;
  Self.rozpadBlok := -5;
  Self.rozpadRuseniBlok := -5;
 end;

////////////////////////////////////////////////////////////////////////////////

//RozpadBlok = blok index, kam by mela souprava vjet
//RozpadRuseniBlok = blok index, kde je posledni detekovany vagon soupravy
procedure TJC.usekyRusJC();
var Nav:TBlkNav;
    Blk:TBlk;
    Usek,DalsiUsek:TBlkUsek;
    i:Integer;
    train: TTrain;
begin
 Blky.GetBlkByID(Self.fproperties.navestidloBlok, TBlk(Nav));

 // kontrola obsazenosti useku pred navestidlem
 Usek := Nav.UsekPred as TBlkUsek;
 if ((Self.rozpadBlok = -1) and ((Usek.Obsazeno <> TUsekStav.uvolneno) or
     (Usek.GetSettings.RCSAddrs.Count = 0))) then
  begin
   Self.rozpadBlok       := 0;
   Self.rozpadRuseniBlok := -1;
  end;

 // uvolneni prvniho useku pred navestidlem v posunove ceste je signalem pro zhasnuti navestidla
 if ((Usek.GetSettings().RCSAddrs.Count > 0) and (Usek.Obsazeno = TUsekStav.uvolneno) and
     (Nav.Navest <> ncStuj) and (Self.rozpadRuseniBlok = -1) and (Self.typ = TJCType.posun) and
     (Self.rozpadBlok >= 1)) then
  begin
   Self.Log('Uvolnen usek '+Usek.name+' : navestidlo '+ Nav.name+' nastaveno na STUJ');
   Nav.JCZrusNavest();
  end;


 for i := Self.rozpadBlok to Self.fproperties.useky.Count-1 do
  begin
   if (i < 0) then continue;    // i = -1 kdyz se kontroluje blok pred navestidlem, -2 pokud je navestidlo na STUJ, nebo zamkle

   Blky.GetBlkByID(Self.fproperties.useky[i], TBlk(Usek));

   // druha cast podminky je tu pro pripad, kdy by byl na konci posunove cesty obsazeny usek
   if ((Usek.Obsazeno = obsazeno) and ((i < Self.fproperties.useky.Count-1) or (Self.rozpadBlok > Self.fproperties.useky.Count-2) or (Self.typ <> TJCType.posun))) then
    begin
     if (i = Self.rozpadBlok) then
      begin
       //pokud se tento usek rovna RozpadBloku
       Usek.Zaver := TZaver.nouz;

       if (Self.typ = TJCType.vlak) then
        begin
         //posuneme soupravu o blok dal
         Self.PredejDataDalsimuBloku();
        end;//if (Self.typ = 0)

       // obsazeni prvniho useku
       // pozor: toto musi byt na tomto miste kvuli nastavovani Souprava.front
       if ((i = 0) and (Nav.Navest <> ncStuj) and (Self.rozpadBlok = 0)) then
        begin
         // navestidlo pri obsazeni prvniho useku rusime v pripade, ze se jedna o VC
         if (Self.typ = TJCType.vlak) then
          begin
           Self.Log('Obsazen usek '+Usek.name+' : navestidlo '+Nav.name+' nastaveno na STUJ');
           Nav.JCZrusNavest();

           // aktualizace casu odjezdu v trati
           if (Self.fproperties.Trat > -1) then
            begin
             Blky.GetBlkByID(Self.fproperties.Trat, Blk);
             if (TBlkTrat(Blk).TrainPredict <> nil) then
              begin
               TBlkTrat(Blk).TrainPredict.time := timeHelper.hJOPnow();
               TBlkTrat(Blk).TrainPredict.predict := false;
               TBlkTrat(Blk).Change();
              end;
            end;
          end;
        end;

       Self.rozpadBlok := Self.rozpadBlok + 1;

       // pokud jsme v predposlednim useku a posledni je nedetekovany, posuneme RozpadBlok jeste o jeden usek, aby se cesta mohla zrusit
       if (i = Self.fproperties.useky.Count-2) then
        begin
         if (TBlkUsek(Self.lastUsek).GetSettings().RCSAddrs.Count = 0) then
           Self.rozpadBlok := Self.rozpadBlok + 1;
        end;

       if ((i = Self.fproperties.useky.Count-1) and (Self.fproperties.Trat > -1)) then
        begin
         // posledni usek JC obsazen -> trat
         Blky.GetBlkByID(Self.fproperties.Trat, Blk);

         if (Self.typ = TJCType.vlak) then
          begin
           (Blk as TBlkTrat).BP := true;
           if (Usek.IsTrain()) then
            begin
             if (((Blk as TBlkTrat).TrainPredict <> nil) and
                 ((Blk as TBlkTrat).TrainPredict.train = Usek.train)) then
               (Blk as TBlkTrat).AddTrain((Blk as TBlkTrat).trainPredict)
             else
               (Blk as TBlkTrat).AddTrain(TBlkTratTrain.Create(Usek.TrainI));
            end;
          end;
         (Blk as TBlkTrat).Zaver := false;

         // nastavime rychlost souprave
         if (Self.typ = TJCType.vlak) then
           TBlkTU(Usek).rychUpdate := true;
        end;


      end else begin //if Self.rozpadBlok = 0
       if (Usek.Zaver > TZaver.no) then
        begin
         //pokud jsme na jinem useku, nez RozpadBlok
         if ((Nav.Navest > ncStuj) and (Nav.DNjc = Self)) then
          begin
           if (Self.fstaveni.senderPnl <> nil) and (Self.fstaveni.senderOR <> nil) then
             ORTCPServer.BottomError(Self.fstaveni.senderPnl, 'Chyba povolovací návěsti '+Nav.name,
                (Self.fstaveni.senderOR as TOR).ShortName, 'TECHNOLOGIE');
           Self.RusJCWithoutBlk();
          end;

         // v trati zaver nerusime, nesmime tam dat ani nouzovy, ani zadny zaver
         if ((i <> Self.fproperties.useky.Count-1) or (Self.fproperties.Trat = -1)) then
           Usek.Zaver := TZaver.nouz;
        end;
      end;
    end;


   // kontrola zruseni jizdni cesty vlivem vynuzovani bloku
   if ((i = Self.rozpadBlok) and ((Usek.Zaver = TZaver.no))) then
    begin
     // pokud usek, na ktery se chystam vkrocit, nema zaver, je neco divne -> zrusit JC (predevsim kvuli predavani loko, ktere by mohlo narusit dalsi JC)
     Self.RusJCWithoutBlk();
     Exit();
    end;

  end;//for i

  // jizdni cesta konci uvolnenim predposledniho useku

  // mensitko je dulezite a ma smysl !
  //  kdyby tam bylo <=, mohl by se rozpadnout jediny usek, na kterem je souprava tim, ze se odobsadi
  if ((Self.rozpadRuseniBlok >= 0) and (Self.rozpadRuseniBlok < Self.rozpadBlok-1)) then
   begin
    //ziskani dotazovaneho useku
    Blky.GetBlkByID(Self.fproperties.useky[Self.rozpadRuseniBlok], TBlk(Usek));

    if (Self.rozpadRuseniBlok+1 < Self.fproperties.useky.Count) then
      Blky.GetBlkByID(Self.fproperties.useky[Self.rozpadRuseniBlok+1], TBlk(DalsiUsek))
    else
      DalsiUsek := nil;

    if ((Usek.Zaver = TZaver.nouz) and (Usek.Obsazeno = uvolneno) and
        ((DalsiUsek = nil) or (DalsiUsek.Obsazeno = TUsekStav.obsazeno) or
        (DalsiUsek.GetSettings.RCSAddrs.Count = 0))) then
     begin
      // cesta se rozpada...
      if (Self.AB) then
        Usek.Zaver := TZaver.AB
      else
        Usek.Zaver := TZaver.no;

      Self.rozpadRuseniBlok := Self.rozpadRuseniBlok + 1;

      if ((Self.typ = TJCType.vlak) and (Usek.IsTrain())) then
       begin
        Self.Log('Smazana souprava '+Usek.Train.name+' z bloku '+Usek.name, WR_SPRPREDAT);
        Usek.RemoveTrains();
       end;
     end;//if Self.rozpadBlok >= 1
   end;//if (cyklus2 = Self.rozpadRuseniBlok)

  // tady se resi pripad, kdy stanicni kolej zustane obsazena (protoze tam stoji vagony),
  // ale souprava se z ni musi odstanit uvolnenim prvniho bloku JC
  if ((Self.rozpadRuseniBlok = -1) and (Self.rozpadBlok > 0)) then
   begin
    Blky.GetBlkByID(Self.fproperties.useky[0], TBlk(Usek));

    if (Self.fproperties.useky.Count > 1) then
      Blky.GetBlkByID(Self.fproperties.useky[1], TBlk(DalsiUsek))
    else
      DalsiUsek := nil;

    if ((Usek.Zaver = TZaver.nouz) and (Usek.Obsazeno = uvolneno) and
        ((DalsiUsek = nil) or (DalsiUsek.Obsazeno = TUsekStav.obsazeno) or
        (DalsiUsek.GetSettings.RCSAddrs.Count = 0))) then
     begin
      // uvolneni prvniho useku v posunove ceste je signalem pro zhasnuti navestidla
      if ((Nav.Navest <> ncStuj) and (Self.typ = TJCType.posun)) then
       begin
        Self.Log('Uvolnen usek '+Usek.name+' : navestidlo '+Nav.name+' nastaveno na STUJ');
        Nav.JCZrusNavest();
       end;

      if (Self.AB) then
        TBlkUsek(Usek).Zaver := TZaver.AB
      else
        TBlkUsek(Usek).Zaver := TZaver.no;

      Self.rozpadRuseniBlok := 1;

      if ((Self.typ = TJCType.vlak) and (Usek.IsTrain())) then
       begin
        // mazani soupravy z useku pred navestidlem
        Blk := TBlkNav(Nav).UsekPred;
        train := Self.GetTrain(Nav, Blk);
        if (train = TBlkUsek(Usek).train) then
         begin
          Self.Log('Smazana souprava '+train.name+' z bloku '+Blk.name, WR_SPRPREDAT);
          (Blk as TBlkUsek).RemoveTrain(train);
         end;

        Self.Log('Smazana souprava '+train.name+' z bloku '+Usek.name, WR_SPRPREDAT);
        Usek.RemoveTrains();
       end;
     end;
   end;

  // mazani soupravy z useku pred navestidlem
  if ((Self.rozpadBlok > 0) and (Self.rozpadRuseniBlok = -1)) then
   begin
    Usek := Nav.UsekPred as TBlkUsek;
    if ((Usek.Obsazeno = TUsekStav.uvolneno) and (Usek.GetSettings.RCSAddrs.Count > 0)) then
     begin
      if (Usek.IsTrain() and (Self.typ = TJCType.vlak)) then
       begin
        train := Self.GetTrain(nav, Usek);
        Usek.RemoveTrain(train);
        Self.Log('Smazana souprava '+train.name+' z bloku '+Usek.name, WR_SPRPREDAT);
       end;

      Self.rozpadRuseniBlok := 0;

      if ((Usek.typ = btTU) and (TBlkTU(Usek).Trat <> nil) and (TBlkTU(Usek).bpInBlk)) then
        TBlkTU(Usek).UvolnenoZJC();
     end;
   end;

  Usek := Nav.UsekPred as TBlkUsek;
  if ((Self.rozpadBlok = 0) and (Self.rozpadRuseniBlok = -1) and
      (TBlkUsek(Usek).Obsazeno <> TUsekStav.obsazeno)) then
   begin
    // usek pred navestidlem se opet uvolnil
    Self.rozpadBlok := -1;
    Self.rozpadRuseniBlok := -2;
   end;


  // tahleta silenost za OR je tu pro pripad, kdy JC ma jen jeden usek (to se stava napriklad na smyckach)
  if ((Self.rozpadRuseniBlok = Self.fproperties.useky.Count-1) and (Self.fproperties.useky.Count > 1))
      or ((Self.fproperties.useky.Count = 1) and (Self.rozpadBlok = 1)) then
   begin
    // vsechny useky az na posledni jsou uvolneny -> rusime JC

    // tady by teoreticky melo prijit ruseni zaveru posledniho bloku, ale to neni poteba,
    // protoze zaver tohoto bloku je primo navazny na zaver predposledniho bloku pres redukce
    // to je napriklad kvuli tratim, ci z toho duvodu, ze na stanicnich kolejich nejde dat NUZ

    // pozor ale na JC, ktere maji jen jeden usek a ten je stanicni koleji:
    if (Self.fproperties.useky.Count = 1) then
     begin
      Blky.GetBlkByID(Self.fproperties.useky[0], TBlk(Usek));

      if (Self.AB) then
        Usek.Zaver := TZaver.AB
      else
        Usek.Zaver := TZaver.no;

      Usek := Nav.UsekPred as TBlkUsek;
      train := Self.GetTrain(Nav, Usek);

      // pokud ma cesta jen jeden usek, odstranime soupravu z useku pred navestidlem:
      if ((Self.typ = TJCType.vlak) and (train <> nil)) then
       begin
        Usek.RemoveTrain(train);
        Self.Log('Smazana souprava '+train.name+' z bloku '+Usek.name, WR_SPRPREDAT);
       end;

      if ((Usek.typ = btTU) and (TBlkTU(Usek).Trat <> nil) and (TBlkTU(Usek).bpInBlk)) then
        TBlkTU(Usek).UvolnenoZJC();
     end;

    Self.rozpadBlok       := -5;
    Self.rozpadRuseniBlok := -5;
    Self.Log('Ruseni: rozpad cesty vlakem');
    if (Nav.DNjc = Self) then
     begin
      if (Nav.Navest > ncStuj) then      // tato situace opravdu muze nastat - predstavte si posunovou cestu s jednim usekem vychazejici z nedetek koleje
        Nav.JCZrusNavest();
      Nav.DNjc := nil;
     end;
   end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TJC.NeprofilObsaz();
var Nav:TBlk;
begin
 if (Self.staveni) then
  begin
   Self.CancelStaveni('Nelze postavit - obsazen neprofilový úsek');
  end else begin
   Blky.GetBlkByID(Self.fproperties.navestidloBlok, Nav);
   if (((Nav as TBlkNav).Navest > ncStuj) and ((Nav as TBlkNav).DNjc = Self)) then
    begin
     if (Self.fstaveni.senderPnl <> nil) and (Self.fstaveni.senderOR <> nil) then
       ORTCPServer.BottomError(Self.fstaveni.senderPnl, 'Chyba povolovací návěsti '+Nav.name,
            (Self.fstaveni.senderOR as TOR).ShortName, 'TECHNOLOGIE');
     Self.RusJCWithoutBlk();
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TJC.usekyRusNC();
var TU, first : TBlkUsek;
    nav : TBlkNav;
begin
 Blky.GetBlkByID(Self.fproperties.navestidloBlok, TBlk(nav));
 TU := TBlkTU((nav as TBlkNav).UsekPred);
 Blky.GetBlkByID(Self.fproperties.useky[0], TBlk(first));

 if ((first.Obsazeno = TUsekStav.obsazeno) and (TU.Obsazeno = TUsekStav.uvolneno)
    and (not TU.IsTrain())) then
  begin
   if (TBlkTU(TU).bpInBlk) then
     TBlkTU(TU).UvolnenoZJC();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

//preda soupravu v jizdni ceste dalsimu bloku v poradi
procedure TJC.PredejDataDalsimuBloku();
var UsekActual,UsekDalsi,Nav:TBlk;
    train: TTrain;
 begin
  if (Self.rozpadBlok = 0) then
   begin
    Blky.GetBlkByID(Self.fproperties.navestidloBlok, Nav);
    UsekActual := (Nav as TBlkNav).UsekPred;
    train := Self.GetTrain(Nav, UsekActual);
    if ((UsekActual as TBlkUsek).IsTrain()) then
      if (train.front <> UsekActual) then
         Exit();
   end else begin
    Blky.GetBlkByID(Self.fproperties.useky[Self.rozpadBlok-1], UsekActual);
    train := TBlkUsek(UsekActual).train;
   end;

  Blky.GetBlkByID(Self.fproperties.useky[Self.rozpadBlok], UsekDalsi);
  if (not (UsekActual as TBlkUsek).IsTrain()) then Exit;

  (UsekDalsi as TBlkUsek).zpomalovani_ready := true;
  (UsekDalsi as TBlkUsek).AddTrainL(train);
  (UsekDalsi as TBlkUsek).train.front := UsekDalsi;
  (UsekDalsi as TBlkUsek).houk_ev_enabled := true;
  Self.Log('Predana souprava '+(UsekDalsi as TBlkUsek).train.name+
      ' z bloku '+UsekActual.name+' do bloku '+UsekDalsi.name, WR_SPRPREDAT);

  Self.CheckSmyckaBlok(UsekDalsi);
 end;

procedure TJC.CheckSmyckaBlok(blk:TBlk);
var oblr:TOR;
begin
 if (((Blk as TBlkUsek).GetSettings().SmcUsek) and ((Blk as TBlkUsek).IsTrain())) then
  begin
   // kontrola zmeny vychozi a cilove stanice
   for oblr in blk.OblsRizeni do
    begin
     if (oblr = (Blk as TBlkUsek).train.stationTo) then
      begin
       (Blk as TBlkUsek).train.InterChangeStanice(false);
       break;
      end;
    end;

   (Blk as TBlkUsek).train.ChangeSmer();
   Self.Log('Obsazen smyckovy usek '+Blk.name+ ' - menim smer loko v souprave '+
      (Blk as TBlkUsek).train.name, WR_SPRPREDAT);
  end;//if
end;

////////////////////////////////////////////////////////////////////////////////

//nastavi navestidlo JC na pozadovanou navest
procedure TJC.NastavNav();
var Nav,DalsiNav:TBlkNav;
    Navest: TBlkNavCode;
 begin
  Blky.GetBlkByID(Self.fproperties.navestidloBlok, TBlk(Nav));

  Navest := ncStuj;

  if ((Self.fstaveni.nc) and (Self.typ = TJCType.vlak)) then
   begin
    // nouzova cesta
    Navest := ncPrivol;
   end else begin

    case (Self.typ) of
     TJCType.posun : begin
      // posunova cesta
      Navest := ncPosunZaj;
     end;//case posun

     TJcType.vlak : begin
      Blky.GetBlkByID(Self.fproperties.dalsiNavestidlo, TBlk(DalsiNav));
      if ((Self.fproperties.dalsiNavaznost = TJCNextNavType.zadna) or
          (Self.fproperties.dalsiNavaznost = TJCNextNavType.trat) or
          ((Self.fproperties.dalsiNavaznost = TJCNextNavType.blok) and
           ((DalsiNav <> nil) and (DalsiNav.IsPovolovaciNavest()) and (not DalsiNav.IsOpakVystraha())))) then
       begin
        // na dalsim navestidle je navest povolujici jizdu (vyjma PN)
        if (Self.data.odbocka) then
         begin
          if ((Self.fproperties.dalsiNavaznost = TJCNextNavType.blok) and
              ((DalsiNav.Navest = ncOpakOcek40) or (DalsiNav.FourtyKmph()))) then
            Navest := nc40Ocek40
          else
            Navest := ncVolno40;
         end else begin
          if ((Self.fproperties.dalsiNavaznost = TJCNextNavType.blok) and
              ((DalsiNav.Navest = ncOpakOcek40) or (DalsiNav.FourtyKmph()))) then
            Navest := ncOcek40
          else
            Navest := ncVolno;
         end;

       end else begin
        // na dalsim navestidle je STUJ nebo opakoveni navesti vystraha (to je pro nas jako stuj)

        if (Self.data.odbocka) then
          Navest := ncVystraha40
        else
          Navest := ncVystraha;
       end;

      if ((Self.fproperties.nzv) and (Navest <> ncVolno)) then
        Navest := TBlkNav.AddOpak(Navest);
     end;//case vlak

     end;//case
   end;// else nouzova cesta

  Nav.SetNavest(Navest, TNotifyEvent(nil), Self.NavNepostaveno);
 end;

////////////////////////////////////////////////////////////////////////////////

procedure TJC.LoadData(ini:TMemIniFile; section:string);
var sl,sl2:TStrings;
    i,j, cnt:Integer;
    vyhZaver:TJCVyhZaver;
    odvrat:TJCOdvratZaver;
    ref:TJCRefZaver;
    prj:TJCPrjZaver;
    sect_size:Integer;
begin
 Self.fproperties.name := ini.ReadString(section, 'nazev', section);
 Self.fproperties.id := StrToInt(section);
 Self.fproperties.navestidloBlok := ini.ReadInteger(section, 'nav', -1);
 Self.fproperties.typ := TJCType(ini.ReadInteger(section, 'typ', -1));
 Self.fproperties.dalsiNavaznost := TJCNextNavType(ini.ReadInteger(section, 'dalsiNTyp', 0));
 Self.fproperties.dalsiNavestidlo := ini.ReadInteger(section, 'dalsiN', 0);
 Self.fproperties.speedGo := ini.ReadInteger(section, 'rychDalsiN', 0);
 Self.fproperties.speedStop := ini.ReadInteger(section, 'rychNoDalsiN', 0);
 Self.fproperties.Trat := ini.ReadInteger(section, 'trat', -1);
 Self.fproperties.TratSmer := TTratSmer(ini.ReadInteger(section, 'tratSmer', 0));

 // nacteni zaveru useku:
 sl  := TStringList.Create();
 sl2 := TStringList.Create();

 try
   ExtractStrings([';', ',', '|', '-', '('], [')'], PChar(ini.ReadString(section, 'useky', '')), sl);
   Self.fproperties.useky.Count := sl.Count;
   for i := 0 to Self.fproperties.useky.Count-1 do
     Self.fproperties.useky[i] := StrToInt(sl[i]);

   // nacteni zaveru vyhybek:
   sl.Clear();
   ExtractStrings([';', ',', '|', '-', '(', ')'], [], PChar(ini.ReadString(section, 'vyhybky', '')), sl);
   sect_size := 2;
   cnt := (sl.Count div sect_size);
   Self.fproperties.vyhybky.Clear();
   for i := 0 to cnt-1 do
    begin
     vyhZaver.Blok := StrToInt(sl[i*sect_size]);
     vyhZaver.Poloha := TVyhPoloha(StrToInt(sl[(i*sect_size)+1]));
     Self.fproperties.vyhybky.Add(vyhZaver);
    end;//for i

   // nacteni odvratu:
   sl.Clear();
   ExtractStrings([';', ',', '|', '-', '(', ')'], [], PChar(ini.ReadString(section, 'odvraty', '')), sl);
   sect_size := 3;
   cnt := (sl.Count div sect_size);
   Self.fproperties.odvraty.Clear();
   for i := 0 to cnt-1 do
    begin
     odvrat.Blok := StrToInt(sl[i*sect_size]);
     odvrat.Poloha := TVyhPoloha(StrToInt(sl[(i*sect_size)+1]));
     odvrat.ref_blk := StrToInt(sl[(i*sect_size)+2]);
     Self.fproperties.odvraty.Add(odvrat);
    end;//for i

   //format dat prejezdu:
   // (...),(...),(...) jsou jednotlive prejezdy
   // konkretni popis toho, co ma byt na miste tecek:
   //  (prj_blk_id,otevreni_blk,uzavreni_blk_1,uzavreni_blk_2,uzavreni_blk_3,..)

   // nacteni prejezdu
   sl.Clear();
   ExtractStrings(['(', ')'], [], PChar(ini.ReadString(section, 'prj', '')), sl);
   for i := 0 to sl.Count-1 do
    begin
     sl2.Clear();
     ExtractStrings([';', ',', '|', '-'], [], PChar(sl[i]), sl2);

     prj.Prejezd := StrToInt(sl2[0]);
     if (sl2.Count > 1) then
       prj.oteviraci := StrToInt(sl2[1])
     else
       prj.oteviraci := -1;

     prj.uzaviraci := TList<Integer>.Create();
     for j := 2 to sl2.Count-1 do
       prj.uzaviraci.Add(StrToInt(sl2[j]));

     Self.fproperties.prejezdy.Add(prj);
    end;//for i

   // nacteni podminek zamku:
   sl.Clear();
   ExtractStrings(['(', ')'], [], PChar(ini.ReadString(section, 'podm-zamky', '')), sl);
   Self.fproperties.zamky.Clear();
   for i := 0 to sl.Count-1 do
    begin
     sl2.Clear();
     ExtractStrings([';', ',', '|', '-'], [], PChar(sl[i]), sl2);

     ref.Blok    := StrToInt(sl2[0]);
     ref.ref_blk := StrToInt(sl2[1]);
     Self.fproperties.zamky.Add(ref);
    end;//for i

   // nacteni variantnich bodu
   sl.Clear();
   ExtractStrings([';', ',', '|', '-', '(', ')'], [], PChar(ini.ReadString(section, 'vb', '')), sl);
   for i := 0 to sl.Count-1 do
     Self.fproperties.vb.Add(StrToInt(sl[i]));

 finally
   sl.Free();
   sl2.Free();
 end;

 if (ini.ValueExists(section, 'odbocka')) then
   Self.fproperties.odbocka := ini.ReadBool(section, 'odbocka', false)
 else
   Self.fproperties.odbocka := Self.IsAnyVyhMinus();

 Self.fproperties.nzv := ini.ReadBool(section, 'nzv', false)
end;

procedure TJC.SaveData(ini:TMemIniFile; section:string);
var line:string;
    i,j:Integer;
begin
 ini.WriteString (section, 'nazev', Self.fproperties.name);
 ini.WriteInteger(section, 'nav', Self.fproperties.navestidloBlok);
 ini.WriteInteger(section, 'typ', Integer(Self.fproperties.typ));
 if (Self.fproperties.dalsiNavaznost <> TJCNextNavType.zadna) then
   ini.WriteInteger(section, 'dalsiNTyp', Integer(Self.fproperties.dalsiNavaznost));
 if (Self.fproperties.dalsiNavaznost = TJCNextNavType.blok) then
   ini.WriteInteger(section, 'dalsiN', Self.fproperties.dalsiNavestidlo);
 ini.WriteInteger(section, 'rychDalsiN', Self.fproperties.speedGo);
 ini.WriteInteger(section, 'rychNoDalsiN', Self.fproperties.speedStop);

 if (Self.fproperties.odbocka = Self.IsAnyVyhMinus) then
   ini.DeleteKey(section, 'odbocka')
 else
   ini.WriteBool(section, 'odbocka', Self.fproperties.odbocka);

 if (not Self.fproperties.nzv) then
   ini.DeleteKey(section, 'nzv')
 else
   ini.WriteBool(section, 'nzv', true);

 if (Self.fproperties.Trat > -1) then
  begin
   ini.WriteInteger(section, 'trat', Self.fproperties.Trat);
   ini.WriteInteger(section, 'tratSmer', Integer(Self.fproperties.TratSmer));
  end;

 // useky
 line := '';
 for i := 0 to Self.fproperties.useky.Count-1 do
   line := line + IntToStr(Self.fproperties.useky[i]) + ',';
 if (line <> '') then
   ini.WriteString(section, 'useky', line);

 // vyhybky
 line := '';
 for i := 0 to Self.fproperties.vyhybky.Count-1 do
   line := line + '(' + IntToStr(Self.fproperties.vyhybky[i].Blok) + ',' + IntToStr(Integer(Self.fproperties.vyhybky[i].Poloha)) + ')';
 if (line <> '') then
   ini.WriteString(section, 'vyhybky', line);

 // odvraty
 line := '';
 for i := 0 to Self.fproperties.odvraty.Count-1 do
   line := line + '(' + IntToStr(Self.fproperties.odvraty[i].Blok) + ',' + IntToStr(Integer(Self.fproperties.odvraty[i].Poloha)) + ',' + IntToStr(Self.fproperties.odvraty[i].ref_blk)+ ')';
 if (line <> '') then
   ini.WriteString(section, 'odvraty', line);

 // prejezdy
 line := '';
 for i := 0 to Self.fproperties.prejezdy.Count-1 do
  begin
   line := line + '(' + IntToStr(Self.fproperties.prejezdy[i].Prejezd);

   if (Self.fproperties.prejezdy[i].uzaviraci.Count > 0) then
    begin
     line := line + ',' + IntToStr(Self.fproperties.prejezdy[i].oteviraci)+ ',';
     for j := 0 to Self.fproperties.prejezdy[i].uzaviraci.Count-1 do
       line := line + IntToStr(Self.fproperties.prejezdy[i].uzaviraci[j]) + ',';
    end;

   if (line[Length(line)] = ',') then
     line[Length(line)] := ')'
   else
     line := line + ')';
  end;
 if (line <> '') then
   ini.WriteString(section, 'prj', line);

 // zamky
 line := '';
 for i := 0 to Self.fproperties.zamky.Count-1 do
   line := line + '(' + IntToStr(Self.fproperties.zamky[i].Blok) + ';' + IntToStr(Self.fproperties.zamky[i].ref_blk) + ')';
 if (line <> '') then
   ini.WriteString(section, 'podm-zamky', line);

 // variantni body
 line := '';
 for i := 0 to Self.fproperties.vb.Count-1 do
   line := line + IntToStr(Self.fproperties.vb[i]) + ';';
 if (line <> '') then
   ini.WriteString(section, 'vb', line);
end;

////////////////////////////////////////////////////////////////////////////////

// timeout staveni JC
procedure TJC.UpdateTimeOut();
var prejezd:TBlkPrejezd;
    prjZaver:TJCPrjZaver;
begin
 // na nouzovou cestu se nevztahuje timeout
 if (not Self.Staveni) then Exit;

 if (Now > Self.fstaveni.timeOut) then
  begin
   case (Self.krok) of
    _KROK_POTVR_SEKV: begin
     if (Self.fstaveni.senderPnl <> nil) and (Self.fstaveni.senderOR <> nil) then
       ORTCPServer.PotvrClose(Self.fstaveni.senderPnl);
    end;
    _JC_KROK_CEKANI_PREJEZDY: begin
      // prejezd(y) neuzavren
      for prjZaver in Self.fproperties.prejezdy do
       begin
        Blky.GetBlkByID(prjZaver.Prejezd, TBlk(prejezd));
        if (prejezd.Stav.basicStav <> TBlkPrjBasicStav.uzavreno) then
          if (Self.fstaveni.senderPnl <> nil) and (Self.fstaveni.senderOR <> nil) then
            ORTCPServer.BottomError(Self.fstaveni.senderPnl, 'Neuzavřen '+prejezd.name,
              (Self.fstaveni.senderOR as TOR).ShortName, 'TECHNOLOGIE');
       end;//for i
    end;//case 13

   else
     if (Self.fstaveni.senderPnl <> nil) and (Self.fstaveni.senderOR <> nil) then
       ORTCPServer.BottomError(Self.fstaveni.senderPnl, 'Timeout '+Self.name,
         (Self.fstaveni.senderOR as TOR).ShortName, 'TECHNOLOGIE');
   end;//else case

   //timeout
   Self.CancelStaveni('Překročení času stavění JC', true);    // toto je docasne reseni: cestu vymazeme ze zasobniku
  end;//if timeout
end;

////////////////////////////////////////////////////////////////////////////////

function TJC.GetStaveni():boolean;
begin
 Result := ((Self.krok > _KROK_DEFAULT) and (Self.krok <> _JC_KROK_CEKANI_POSLEDNI_USEK));
end;

function TJC.GetPostaveno():boolean;
begin
 Result := (Self.fstaveni.rozpadBlok > -5);
end;

////////////////////////////////////////////////////////////////////////////////

// true = je mozno DN
//tato funkce kontroluje, jestli je mozne znovupostavit cestu i kdyz byla fakticky zrusena = musi zkontrolovat vsechny podminky
function TJC.CanDN():boolean;
var i:Integer;
    train: TTrain;
    usekZaver: Integer;
    vyhZaver: TJCVyhZaver;
    odvratZaver: TJCOdvratZaver;
    prjZaver: TJCPrjZaver;
    refZaver: TJCRefZaver;
    usek: TBlkUsek;
    vyhybka: TBlkVyhybka;
    prejezd: TBlkPrejezd;
    trat: TBlkTrat;
    zamek: TBlkZamek;
begin
 // index soupravy na useku pred navestidlem
 train := Self.GetTrain();

 // zkontrolujeme zavery bloku
 // JC NELZE obnovit z useku, na kterych uplne spadl zaver (do zadneho zaveru)
 // porusily by se reference na redukce menu
 for i := 0 to Self.fproperties.useky.Count-1 do
  begin
   usekZaver := Self.fproperties.useky[i];
   Blky.GetBlkByID(usekZaver, TBlk(usek));
   if ((usek.Zaver = TZaver.no) or (usek.Zaver = TZaver.staveni) or (usek.NUZ) or
      ((usek.Obsazeno <> TUsekStav.uvolneno) and
       ((Self.typ = TJCType.vlak) or (i <> Self.fproperties.useky.Count-1)))) then Exit(false);

   // na usecich v ceste je dovoleno mit soupravu pred navestidlem, v takovem
   // pripade ji DN z useku v ceste smaze

   if (Self.typ = TJCType.vlak) then
    begin
     if (train = nil) then
      begin
       // pred navestidlem neni souprava -> na usecich nesmi byt zadna souprava
       if (usek.IsTrain()) then Exit(false);
      end else begin
       // pred navestidlem je souprava -> na usecich smi byt jen stejna souprava
       // jako pred navestidlem
       if ((usek.IsTrain()) and
           ((usek.trains.Count > 1) or (usek.train <> train))) then
         Exit(false);
      end;
    end;
  end;//for i

 // zkontrolujeme polohu vyhybek
 for vyhZaver in Self.fproperties.vyhybky do
  begin
   Blky.GetBlkByID(vyhZaver.Blok, TBlk(vyhybka));
   if (vyhybka.Poloha <> vyhZaver.Poloha) then Exit(false);

   // kontrola neprofiloveho styku pro polohu +
   if ((vyhZaver.Poloha = TVyhPoloha.plus) and (vyhybka.npBlokPlus <> nil) and
       (TBlkUsek(vyhybka.npBlokPlus).Obsazeno <> TUsekStav.uvolneno)) then
     Exit(false);

   // kontrola neprofiloveho styku pro polohu -
   if ((vyhZaver.Poloha = TVyhPoloha.minus) and (vyhybka.npBlokMinus <> nil) and
       (TBlkUsek(vyhybka.npBlokMinus).Obsazeno <> TUsekStav.uvolneno)) then
     Exit(false);
  end;//for i

 // zkontrolujeme polohu odvratu
 for odvratZaver in Self.fproperties.odvraty do
  begin
   Blky.GetBlkByID(odvratZaver.Blok, TBlk(vyhybka));
   if (vyhybka.Poloha <> odvratZaver.Poloha) then Exit(false);
  end;//for i

 // zkontrolujeme poruchy prejezdu
 //  prejezdy, na kterych je zaver, by taky mely byt uzavrene
 for prjZaver in Self.fproperties.prejezdy do
  begin
   Blky.GetBlkByID(prjZaver.Prejezd, TBlk(prejezd));
   if ((prejezd.Stav.basicStav = TBlkPrjBasicStav.none) or
      (prejezd.Stav.basicStav = TBlkPrjBasicStav.disabled)) then Exit(false);
   if ((prejezd.Zaver) and (prejezd.Stav.basicStav <> TBlkPrjBasicStav.uzavreno)) then Exit(false);
  end;//for i

 //zkontrolujeme trat
 if (Self.fproperties.Trat > -1) then
  begin
   Blky.GetBlkByID(Self.fproperties.Trat, TBlk(trat));
   if (trat.Zadost) then Exit(false);
   if ((((not (TBlkTU(Self.lastUsek).sectReady)) or (trat.ZAK)) and (Self.typ = TJCType.vlak)) or
       (trat.RBPCan) or (trat.Smer <> Self.fproperties.TratSmer)) then
     Exit(false);
  end;

  // kontrola uzamceni zamku:
  for refZaver in Self.fproperties.zamky do
   begin
    Blky.GetBlkByID(refZaver.Blok, TBlk(zamek));

    // kontrola uzamceni
    if (zamek.klicUvolnen) then
      Exit(false);
   end;//for i

 Result := true;
end;

// DN provede zbytek staveni JC (prejezdy, finalizace)
// tato procedura predpoklada, ze podminky pro DN jsou splneny
procedure TJC.DN();
begin
 Self.Log('DN');
 Self.fstaveni.timeOut := Now + EncodeTime(0, _JC_TIMEOUT_SEC div 60, _JC_TIMEOUT_SEC mod 60, 0);

 if (Self.fstaveni.prjWasClosed) then
   Self.krok := _JC_KROK_FINALNI_ZAVER
 else
   Self.krok := _JC_KROK_ZAVRIT_PREJEZDY;
end;

////////////////////////////////////////////////////////////////////////////////

// volano z navestidla pri STUJ
// nevolat nidky jindy !
procedure TJC.STUJ();
begin
 Self.rozpadBlok := -2;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TJC.UsekClosePrj(Sender:TObject; data:Integer);
var blkId:Integer;
    prejezd:TBlkPrejezd;
    usek:TBlkUsek;
begin
 if ((Self.postaveno) or (Self.staveni)) then
  begin
   // zavrit prejezd
   Blky.GetBlkByID(Self.fproperties.prejezdy[data].Prejezd, TBlk(prejezd));
   prejezd.Zaver := true;
   Self.Log('Obsazen '+TBlkUsek(Sender).name+' - uzaviram prejezd '+prejezd.name);

   // prejezd se uzavira -> po uvolneni zaveru bloku pod prejezdem prejezd opet otevrit
   Blky.GetBlkByID(Self.fproperties.prejezdy[data].oteviraci, TBlk(usek));
   usek.AddChangeEvent(usek.EventsOnZaverReleaseOrAB,
     CreateChangeEvent(ceCaller.NullPrejezdZaver, Self.fproperties.prejezdy[data].Prejezd));
  end;

 for blkId in Self.fproperties.prejezdy[data].uzaviraci do
  begin
   Blky.GetBlkByID(blkId, TBlk(usek));
   usek.RemoveChangeEvent(usek.EventsOnObsaz, CreateChangeEvent(Self.UsekClosePrj, data));
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TJC.SetRozpadBlok(RozpadBlok:Integer);
begin
 Self.fstaveni.rozpadBlok := RozpadBlok;
 Self.changed := true;
end;

procedure TJC.SetRozpadRuseniBlok(RozpadRuseniBlok:Integer);
begin
 Self.fstaveni.rozpadRuseniBlok := RozpadRuseniBlok;
 Self.changed := true;
end;

procedure TJC.SetKrok(Krok:Integer);
begin
 Self.fstaveni.krok := Krok;
 Self.changed := true;
end;

////////////////////////////////////////////////////////////////////////////////

class function TJC.JCBariera(typ:Integer; Blok:TBlk = nil; param:Integer = 0):TJCBariera;
begin
 Result.typ   := typ;
 Result.blok  := Blok;
 Result.param := param;
end;

////////////////////////////////////////////////////////////////////////////////

function TJC.JCBarieraToMessage(Bariera:TJCBariera):TUPOItem;
var i:Integer;
    lines:TStrings;
    canZAK:boolean;
    Blk:TBlk;
begin
 for i := 0 to _UPO_LINES-1 do
  begin
   Result[i].str := '';
   Result[i].fg  := clNone;
   Result[i].bg  := clNone;
  end;


 case (Bariera.typ) of
  _JCB_BLOK_DISABLED, _JCB_BLOK_NOT_TYP, _JCB_NAV_NOT_USEK, _JCB_BLOK_NOT_EXIST,
  _JCB_USEK_OBSAZENO, _JCB_USEK_ZAVER, _JCB_USEK_AB, _JCB_USEK_SOUPRAVA,
  _JCB_VYHYBKA_KONC_POLOHA, _JCB_VYHYBKA_ZAMCENA, _JCB_VYHYBKA_NOUZ_ZAVER,
  _JCB_PREJEZD_NOUZOVE_OTEVREN, _JCB_PREJEZD_PORUCHA,
  _JCB_ODVRAT_ZAMCENA, _JCB_ODVRAT_OBSAZENA, _JCB_ODVRAT_KONC_POLOHA,
  _JCB_TRAT_ZAVER, _JCB_TRAT_NEPRIPRAVENA, _JCB_TRAT_ZADOST, _JCB_TRAT_NESOUHLAS,
  _JCB_ZAMEK_NEUZAMCEN, _JCB_VYHYBKA_NESPAVNA_POLOHA:
  begin
    Result[0] := GetUPOLine('NEPŘÍPUSTNÉ', taCenter, clRed, clWhite);
    if (Assigned(Bariera.blok)) then
      Result[2] := GetUPOLine(Bariera.blok.name)
    else
      Result[2] := GetUPOLine('ID ' + IntToStr(bariera.param));
  end;
 end;//case


 case (Bariera.typ) of
  _JCB_OK                      : Result[0] := GetUPOLine('OK', taCenter, clBlue, $A0A0A0);
  _JCB_STAVENI                 : Result[0] := GetUPOLine('Již se staví', taCenter, clBlue, $A0A0A0);

  _JCB_BLOK_DISABLED           : Result[1] := GetUPOLine('Blok neaktivní');
  _JCB_BLOK_NOT_EXIST          : Result[1] := GetUPOLine('Blok neexistuje');
  _JCB_BLOK_NOT_TYP            : Result[1] := GetUPOLine('Blok není správného typu');

  _JCB_NAV_NOT_USEK            : Result[1] := GetUPOLine('Není úsek před návěstidlem');

  _JCB_USEK_OBSAZENO           : Result[1] := GetUPOLine('Úsek obsazen');
  _JCB_USEK_ZAVER              : Result[1] := GetUPOLine('Úsek zapevněn');
  _JCB_USEK_SOUPRAVA           : Result[1] := GetUPOLine('Souprava');
  _JCB_USEK_AB                 : Result[1] := GetUPOLine('Blokováno automatickou JC');

  _JCB_VYHYBKA_KONC_POLOHA     : Result[1] := GetUPOLine('Není koncová poloha');
  _JCB_VYHYBKA_ZAMCENA         : Result[1] := GetUPOLine('Zamčena');
  _JCB_VYHYBKA_NOUZ_ZAVER      : Result[1] := GetUPOLine('Nouzový závěr');
  _JCB_VYHYBKA_NESPAVNA_POLOHA : Result[1] := GetUPOLine('Nesprávná poloha');

  _JCB_PREJEZD_NOUZOVE_OTEVREN : Result[1] := GetUPOLine('Nouzově otevřen');
  _JCB_PREJEZD_PORUCHA         : Result[1] := GetUPOLine('Poruchový stav');

  _JCB_ODVRAT_ZAMCENA          : Result[1] := GetUPOLine('Zamčena');
  _JCB_ODVRAT_OBSAZENA         : Result[1] := GetUPOLine('Obsazena');
  _JCB_ODVRAT_KONC_POLOHA      : Result[1] := GetUPOLine('Není koncová poloha');

  _JCB_TRAT_ZAVER              : Result[1] := GetUPOLine('Závěr');
  _JCB_TRAT_ZADOST             : Result[1] := GetUPOLine('Probíhá žádost');
  _JCB_TRAT_NESOUHLAS          : Result[1] := GetUPOLine('Nesouhlas');
  _JCB_TRAT_NEPRIPRAVENA       : Result[1] := GetUPOLine('Nepovoluje odjezd');
  _JCB_TRAT_OBSAZENO : begin
    Result[0] := GetUPOLine('NEBUDE POVOLUJÍCÍ NÁVĚST', taCenter, clBlack, clYellow);
    Result[1] := GetUPOLine('Trať obsazena');
    Result[2] := GetUPOLine(Bariera.blok.name);
  end;

  _JCB_ZAMEK_NEUZAMCEN         : Result[1] := GetUPOLine('Neuzamčen');
  _JCB_ZAMEK_NOUZ_ZAVER        : Result[1] := GetUPOLine('Není nouzový závěr');

  _JCB_USEK_VYLUKA : begin
    Result[0] := GetUPOLine('VÝLUKA '+Bariera.blok.name, taCenter, clBlack, clOlive);
    lines := GetLines((Bariera.blok as TBlkUsek).Vyluka, _UPO_LINE_LEN);
    try
      Result[1] := GetUPOLine(lines[0], taLeftJustify, clYellow, $A0A0A0);
      if (lines.Count > 1) then
        Result[2] := GetUPOLine(lines[1], taLeftJustify, clYellow, $A0A0A0);
    finally
      lines.Free();
    end;
  end;

  _JCB_USEK_STITEK : begin
    Result[0] := GetUPOLine('ŠTÍTEK '+Bariera.blok.name, taCenter, clBlack, clTeal);
    lines := GetLines((Bariera.blok as TBlkUsek).Stitek, _UPO_LINE_LEN);
    try
      Result[1] := GetUPOLine(lines[0], taLeftJustify, clYellow, $A0A0A0);
      if (lines.Count > 1) then
        Result[2] := GetUPOLine(lines[1], taLeftJustify, clYellow, $A0A0A0);
    finally
      lines.Free();
    end;
  end;

  _JCB_VYHYBKA_VYLUKA : begin
    Result[0] := GetUPOLine('VÝLUKA '+Bariera.blok.name, taCenter, clBlack, clOlive);
    lines := GetLines((Bariera.blok as TBlkVyhybka).Vyluka, _UPO_LINE_LEN);
    try
      Result[1] := GetUPOLine(lines[0], taLeftJustify, clYellow, $A0A0A0);
      if (lines.Count > 1) then
        Result[2] := GetUPOLine(lines[1], taLeftJustify, clYellow, $A0A0A0);
    finally
      lines.Free();
    end;
  end;

  _JCB_VYHYBKA_STITEK : begin
    Result[0] := GetUPOLine('ŠTÍTEK '+Bariera.blok.name, taCenter, clBlack, clTeal);
    lines := GetLines((Bariera.blok as TBlkVyhybka).Stitek, _UPO_LINE_LEN);
    try
      Result[1] := GetUPOLine(lines[0], taLeftJustify, clYellow, $A0A0A0);
      if (lines.Count > 1) then
        Result[2] := GetUPOLine(lines[1], taLeftJustify, clYellow, $A0A0A0);
    finally
      lines.Free();
    end;
  end;

  _JCB_PREJEZD_STITEK : begin
    Result[0] := GetUPOLine('ŠTÍTEK '+Bariera.blok.name, taCenter, clBlack, clTeal);
    lines := GetLines((Bariera.blok as TBlkPrejezd).Stitek, _UPO_LINE_LEN);
    try
      Result[1] := GetUPOLine(lines[0], taLeftJustify, clYellow, $A0A0A0);
      if (lines.Count > 1) then
        Result[2] := GetUPOLine(lines[1], taLeftJustify, clYellow, $A0A0A0);
    finally
      lines.Free();
    end;
  end;

  _JCB_USEK_LAST_OBSAZENO : begin
    Result[0] := GetUPOLine('NEBUDE POVOLUJÍCÍ NÁVĚST', taCenter, clBlack, clYellow);
    Result[1] := GetUPOLine('Kolejový úsek obsazen');
    Result[2] := GetUPOLine(Bariera.blok.name);
  end;

  _JCB_PRIVOLAVACKA : begin
    Result[0] := GetUPOLine('POZOR !', taCenter, clYellow, $A0A0A0);
    Result[1] := GetUPOLine('Svítí přivolávací návěst');
    Result[2] := GetUPOLine(Bariera.blok.name);
  end;

  _JCB_HV_RUC : begin
    Result[0] := GetUPOLine('POZOR !', taCenter, clYellow, $A0A0A0);
    Result[1] := GetUPOLine('Hnací vozidlo v ručním řízení');
    Result[2] := GetUPOLine(IntToStr(Bariera.param) + ' : ' + HVDb[Bariera.param].Data.nazev);
  end;

  _JCB_HV_NOT_ALL_RUC : begin
    Result[0] := GetUPOLine('POZOR !', taCenter, clYellow, $A0A0A0);
    Result[1] := GetUPOLine('Ne všechna HV v ručním řízení');
    Result[2] := GetUPOLine('');
  end;

  _JCB_TRAT_ZAK : begin
    Blky.GetBlkByID(Self.fproperties.Trat, Blk);
    case (Self.fproperties.TratSmer) of
      TTratSmer.AtoB : canZAK := TBlkUvazka(TBlkTrat(Blk).uvazkaA).ZAK;
      TTratSmer.BtoA : canZAK := TBlkUvazka(TBlkTrat(Blk).uvazkaB).ZAK;
    else
     canZAK := true;
    end;

    if ((Self.typ = TJCType.posun) and (canZAK)) then
     begin
      Result[0] := GetUPOLine('ZAVEDEN ZÁKAZ ODJEZDU', taCenter, clRed, clWhite);
      Result[1] := GetUPOLine(Bariera.blok.name);
      Result[2] := GetUPOLine('');
     end else begin
      Result[0] := GetUPOLine('NEPŘÍPUSTNÉ', taCenter, clRed, clWhite);
      Result[1] := GetUPOLine('Zákaz odjezdu');
      if (Assigned(Bariera.blok)) then
        Result[2] := GetUPOLine(Bariera.blok.name)
      else
        Result[2] := GetUPOLine('ID ' + IntToStr(bariera.param));
     end;
  end;

  _JCB_TRAT_STITEK : begin
    Result[0] := GetUPOLine('ŠTÍTEK '+Bariera.blok.name, taCenter, clBlack, clTeal);
    lines := GetLines((Bariera.blok as TBlkUvazka).Stitek, _UPO_LINE_LEN);
    Result[1] := GetUPOLine(lines[0], taLeftJustify, clYellow, $A0A0A0);
    if (lines.Count > 1) then
      Result[2] := GetUPOLine(lines[1], taLeftJustify, clYellow, $A0A0A0);
    lines.Free();
  end;

  _JCB_SPR_SMER : begin
    Result[0] := GetUPOLine('POZOR !', taCenter, clYellow, $A0A0A0);
    Result[1] := GetUPOLine('Jízda proti směru soupravy');
    Result[2] := GetUPOLine('Soprava ' + Trains[Bariera.param].name);
  end;

 else
  Result[0] := GetUPOLine('Neznámá bariéra ve stavění JC', taCenter, clRed, clWhite);
 end;
end;

////////////////////////////////////////////////////////////////////////////////

// vraci true, pokud je zadana bariera tzv. kriticka, jinak false
// kriticka bariera je takova bariera, jejiz odstraneni neni bezny uzivatel schopen
//   napr. absence existence urcitych bloku v jizdni ceste apod.
class function TJC.CriticalBariera(typ:Integer):boolean;
begin
 case (typ) of
  _JCB_STAVENI, _JCB_BLOK_DISABLED, _JCB_BLOK_NOT_EXIST, _JCB_BLOK_NOT_TYP :
            Result := true;
 else
  Result := false;
 end;
end;

////////////////////////////////////////////////////////////////////////////////

function TJC.WarningBariera(typ:Integer):boolean;
var Blk:TBlk;
begin
 case (typ) of
  _JCB_TRAT_ZAK: begin
      Blky.GetBlkByID(Self.fproperties.Trat, Blk);
      case (Self.fproperties.TratSmer) of
        TTratSmer.AtoB : Result := (Self.typ = TJCType.posun) and (TBlkUvazka(TBlkTrat(Blk).uvazkaA).ZAK);
        TTratSmer.BtoA : Result := (Self.typ = TJCType.posun) and (TBlkUvazka(TBlkTrat(Blk).uvazkaB).ZAK);
      else
        Result := false;
      end;
  end;
  _JCB_USEK_STITEK, _JCB_USEK_VYLUKA, _JCB_VYHYBKA_STITEK, _JCB_VYHYBKA_VYLUKA, _JCB_PREJEZD_STITEK,
  _JCB_PRIVOLAVACKA, _JCB_HV_RUC, _JCB_HV_NOT_ALL_RUC, _JCB_SPR_SMER, _JCB_TRAT_STITEK,
  _JCB_USEK_LAST_OBSAZENO, _JCB_TRAT_OBSAZENO:
            Result := true;
 else
  Result := false;
 end;
end;

////////////////////////////////////////////////////////////////////////////////

class function TJC.PotvrSekvBariera(typ:Integer):boolean;
begin
 case (typ) of
  _JCB_VYHYBKA_VYLUKA, _JCB_USEK_VYLUKA, _JCB_TRAT_ZAK: Result := true;
 else
  Result := false;
 end;
end;

////////////////////////////////////////////////////////////////////////////////

class function TJC.PotvrSekvBarieraToReason(typ:Integer):string;
begin
 case (typ) of
  _JCB_VYHYBKA_VYLUKA : Result := 'Výluka výhybkového bloku';
  _JCB_USEK_VYLUKA    : Result := 'Výluka kolejového úseku';
  _JCB_TRAT_ZAK       : Result := 'Zákaz odjezdu na trať';
 else
  Result := '';
 end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TJC.CritBarieraEsc(Sender:TObject);
begin
 Self.CancelStaveni('', true);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TJC.VyhPrestavenaJCPC(Sender:TObject);
var i:Integer;
    Blk:TBlk;
    odvrat:Integer;
    usek: TBlkUsek;
begin
 { Pozor: muze se stat, ze nektera z vyhybek, ktere jeste nejsou prestavovany,
   je behem staveni JC prestavena externim zdrojem. Je treba na to pamatovat.

   Pozor: i ty vyhybky, ktere pri staveni nebyly explicitne zamknuty, se samy
   zamknou pri udeleni zaveru na usek. Nelze tedy vyhybky rozlisovat podle
   zamknuti.

   Pozor: tato funkce muze volat sama sebe rekurzivne skrze callback.
 }

 if (Self.fstaveni.nextVyhybka < 0) then Exit();

 if (Self.fstaveni.nextVyhybka < Self.fproperties.vyhybky.Count) then
  begin
   // stavim dalsi vyhybku
   for i := Self.fstaveni.nextVyhybka to Self.fproperties.vyhybky.Count-1 do
    begin
     Blky.GetBlkByID(Self.fproperties.vyhybky[i].Blok, Blk);
     if ((Blk as TBlkVyhybka).Poloha <> TVyhPoloha(Self.fproperties.vyhybky[i].Poloha)) then
      begin
       Self.fstaveni.nextVyhybka := i+1;
       (Blk as TBlkVyhybka).SetPoloha(TVyhPoloha(Self.fproperties.vyhybky[i].Poloha),
                                      true, false, Self.VyhPrestavenaJCPC, Self.VyhNeprestavenaJCPC);
       Exit();
      end;
    end;

   // sem se skoci, pokud vsechny zbyvajici vyhybky byly ve spravne poloze
   Self.fstaveni.nextVyhybka := Self.fproperties.vyhybky.Count;
  end;

 if (Self.fstaveni.nextVyhybka < Self.fproperties.vyhybky.Count+Self.fproperties.odvraty.Count) then
  begin
   // stavim dalsi odvrat
   odvrat := Self.fstaveni.nextVyhybka - Self.fproperties.vyhybky.Count;
   for i := odvrat to Self.fproperties.odvraty.Count-1 do
    begin
     // nastaveni odvratu
     Blky.GetBlkByID(Self.fproperties.odvraty[i].Blok, Blk);
     if ((Blk as TBlkVyhybka).Poloha <> TVyhPoloha(Self.fproperties.odvraty[i].Poloha)) then
      begin
       TBlkVyhybka(Blk).IntentionalLock();

       Blky.GetBlkByID(Self.fproperties.odvraty[i].ref_blk, TBlk(usek));
       usek.AddChangeEvent(usek.EventsOnZaverReleaseOrAB,
         CreateChangeEvent(ceCaller.NullVyhybkaMenuReduction, Self.fproperties.odvraty[i].Blok));

       Self.fstaveni.nextVyhybka := i+Self.fproperties.vyhybky.Count+1;
       TBlkVyhybka(Blk).SetPoloha(TVyhPoloha(Self.fproperties.odvraty[i].Poloha),
                                  true, false, Self.VyhPrestavenaJCPC, Self.VyhNeprestavenaJCPC);
       Exit();
      end;
    end;

   // sem se skoci, pokud vsechny zbyvajici odvraty byly ve spravne poloze
   Self.fstaveni.nextVyhybka := -1;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TJC.VyhNeprestavenaJCPC(Sender:TObject; error: TVyhSetError);
begin
 if (not Self.staveni) then Exit();

 if (Self.fstaveni.senderPnl <> nil) and (Self.fstaveni.senderOR <> nil) then
   ORTCPServer.BottomError(Self.fstaveni.senderPnl,
     'Nepřestavena '+(Sender as TBlkVyhybka).name + ': ' + TBlkVyhybka.SetErrorToMsg(error),
     (Self.fstaveni.senderOR as TOR).ShortName, 'TECHNOLOGIE');
 Self.CancelStaveni('', true);
 Self.RusJC();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TJC.VyhNeprestavenaNC(Sender:TObject; error: TVyhSetError);
begin
 Self.VyhPrestavenaNC(Sender);
end;

procedure TJC.VyhPrestavenaNC(Sender:TObject);
var Navestidlo, spojka:TBlk;
    Blk:TBlk;
    odvrat:Integer;
begin
 if ((Self.fstaveni.krok <> _NC_KROK_INIT) and (Self.fstaveni.krok <> _NC_KROK_BARIERA_UPDATE)) then Exit();

 TBlkVyhybka(Sender).vyhZaver := true;

 Blky.GetBlkByID(Self.fproperties.navestidloBlok, Navestidlo);
 TBlkNav(Navestidlo).AddBlkToRnz(TBlk(Sender).id, false);

 if (TBlkVyhybka(Sender).GetSettings().spojka > -1) then
  begin
   Blky.GetBlkByID(TBlkVyhybka(Sender).GetSettings().spojka, spojka);
   TBlkVyhybka(spojka).vyhZaver := true;
   TBlkNav(Navestidlo).AddBlkToRnz(TBlkVyhybka(Sender).GetSettings().spojka, false);
  end;

 // staveni dalsich vyhybek

 if (Self.fstaveni.nextVyhybka < 0) then Exit();

 if (Self.fstaveni.nextVyhybka < Self.fproperties.vyhybky.Count) then
  begin
   // stavim dalsi vyhybku
   // Tady staci postavit jen jednu vyhybku, protoze jeji uzamceni opet zavola
   // tuto udalost.

   Blky.GetBlkByID(Self.fproperties.vyhybky[Self.fstaveni.nextVyhybka].Blok, Blk);
   Inc(Self.fstaveni.nextVyhybka);

   (Blk as TBlkVyhybka).SetPoloha(TVyhPoloha(Self.fproperties.vyhybky[Self.fstaveni.nextVyhybka-1].Poloha),
                                  true, false, Self.VyhPrestavenaNC, Self.VyhNeprestavenaNC); // may call callback directly!
  end else if ((Self.fstaveni.nextVyhybka >= Self.fproperties.vyhybky.Count) and
      (Self.fstaveni.nextVyhybka < Self.fproperties.vyhybky.Count+Self.fproperties.odvraty.Count)) then begin
   // nastaveni odvratu
   // Tady staci postavit jen jednu vyhybku, protoze jeji uzamceni opet zavola
   // tuto udalost.

   odvrat := Self.fstaveni.nextVyhybka - Self.fproperties.vyhybky.Count;

   Blky.GetBlkByID(Self.fproperties.odvraty[odvrat].Blok, Blk);
   Inc(Self.fstaveni.nextVyhybka);

   TBlkVyhybka(Blk).SetPoloha(TVyhPoloha(Self.fproperties.odvraty[odvrat].Poloha),
                              true, false, Self.VyhPrestavenaNC, Self.VyhNeprestavenaNC); // may call callback directly!
  end else if (Self.fstaveni.nextVyhybka = Self.fproperties.vyhybky.Count+Self.fproperties.odvraty.Count) then
    Self.fstaveni.nextVyhybka := -1;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TJC.NavNepostaveno(Sender:TObject);
var nav:TBlk;
begin
 if (not Self.staveni) then Exit();
 Blky.GetBlkByID(Self.fproperties.navestidloBlok, nav);

 if (Self.fstaveni.senderPnl <> nil) and (Self.fstaveni.senderOR <> nil) then
   ORTCPServer.BottomError(Self.fstaveni.senderPnl, 'Návěstidlo '+nav.name + ' nepostaveno',
     (Self.fstaveni.senderOR as TOR).ShortName, 'TECHNOLOGIE');
 Self.CancelStaveni('', true);
end;

////////////////////////////////////////////////////////////////////////////////
// generuje podminky branici postaveni nouzove posunove ceste
//  tyto podminky jsou prubezne zobrazovany dispecerovi v potvrzovaci sekvenci

procedure TJC.PodminkyNCStaveni(var bariery:TList<TJCBariera>);
var i:Integer;
    Blk,blk2:TBlk;
    glob:TBlkSettings;
    usek, lastUsek:TBlkUsek;
    trat:TBlkTrat;
begin
  // kontrola navestidla
  Blky.GetBlkByID(Self.fproperties.navestidloBlok, Blk);
  if (not (Blk as TBlkNav).enabled) then
    bariery.Add(Self.JCBariera(_JCB_BLOK_DISABLED, Blk, Blk.id));

  // kontrola useku
  for i := 0 to Self.fproperties.useky.Count-1 do
   begin
    Blky.GetBlkByID(Self.fproperties.useky[i], Blk);
    glob := Blk.GetGlobalSettings();

    // disabled
    if ((Blk as TBlkUsek).Obsazeno = TUsekStav.disabled) then
      bariery.Add(Self.JCBariera(_JCB_BLOK_DISABLED, Blk, Blk.id))

    else if ((i <> Self.fproperties.useky.Count-1) or (Self.typ <> TJCType.posun)) then
     begin
      if ((Blk as TBlkUsek).Obsazeno <> TUsekStav.uvolneno) then
        bariery.Add(Self.JCBariera(_JCB_USEK_OBSAZENO, Blk, Blk.id));
     end;//if

    // souprava
    if (((Blk as TBlkUsek).IsTrain()) and (Self.typ = TJCType.vlak)) then
      bariery.Add(Self.JCBariera(_JCB_USEK_SOUPRAVA, Blk, Blk.id));
   end;//for i

  // kontrola vyhybek:
  for i := 0 to Self.fproperties.vyhybky.Count-1 do
   begin
    Blky.GetBlkByID(Self.fproperties.vyhybky[i].Blok, Blk);
    glob := Blk.GetGlobalSettings();

    // kontrola polohy:
    if ((Blk as TBlkVyhybka).poloha <> Self.fproperties.vyhybky[i].Poloha) then
      bariery.Add(Self.JCBariera(_JCB_VYHYBKA_KONC_POLOHA, Blk, Blk.id));

    // kontrola nouzoveho zaveru:
    if (not (Blk as TBlkVyhybka).vyhZaver) then
      bariery.Add(Self.JCBariera(_JCB_VYHYBKA_NOUZ_ZAVER, Blk, Blk.id));

    // kontrola spojky
    Blky.GetBlkByID((Blk as TBlkVyhybka).GetSettings.spojka, Blk2);
    if ((blk2 <> nil) and ((Blk as TBlkVyhybka).Poloha <> Self.fproperties.vyhybky[i].Poloha)) then
     begin
      if (not (Blk2 as TBlkVyhybka).vyhZaver) then
        bariery.Add(Self.JCBariera(_JCB_VYHYBKA_NOUZ_ZAVER, Blk2, Blk2.id));

      if ((Blk2 as TBlkVyhybka).Obsazeno = TUsekStav.obsazeno) then
        bariery.Add(Self.JCBariera(_JCB_USEK_OBSAZENO, Blk2, Blk2.id));
     end;

    // kontrola neprofiloveho styku pro polohu +
    if ((Self.fproperties.vyhybky[i].Poloha = TVyhPoloha.plus) and (TBlkVyhybka(Blk).npBlokPlus <> nil)) then
     begin
      if (TBlkUsek(TBlkVyhybka(Blk).npBlokPlus).Obsazeno = TUsekStav.disabled) then
        bariery.Add(Self.JCBariera(_JCB_BLOK_DISABLED, TBlkVyhybka(Blk).npBlokPlus,
            TBlkVyhybka(Blk).npBlokPlus.id))
      else
        if (TBlkUsek(TBlkVyhybka(Blk).npBlokPlus).Obsazeno <> TUsekStav.uvolneno) then
          bariery.Add(Self.JCBariera(_JCB_USEK_OBSAZENO, TBlkVyhybka(Blk).npBlokPlus,
              TBlkVyhybka(Blk).npBlokPlus.id));
     end;

    // kontrola neprofiloveho styku pro polohu -
    if ((Self.fproperties.vyhybky[i].Poloha = TVyhPoloha.minus) and (TBlkVyhybka(Blk).npBlokMinus <> nil)) then
     begin
      if (TBlkUsek(TBlkVyhybka(Blk).npBlokMinus).Obsazeno = TUsekStav.disabled) then
        bariery.Add(Self.JCBariera(_JCB_BLOK_DISABLED, TBlkVyhybka(Blk).npBlokMinus,
            TBlkVyhybka(Blk).npBlokMinus.id))
      else
        if (TBlkUsek(TBlkVyhybka(Blk).npBlokMinus).Obsazeno <> TUsekStav.uvolneno) then
          bariery.Add(Self.JCBariera(_JCB_USEK_OBSAZENO, TBlkVyhybka(Blk).npBlokMinus,
              TBlkVyhybka(Blk).npBlokMinus.id));
     end;
   end;//for i

  // kontrola prejezdu
  for i := 0 to Self.fproperties.prejezdy.Count-1 do
   begin
    Blky.GetBlkByID(Self.fproperties.prejezdy[i].Prejezd, Blk);

    if ((Blk as TBlkPrejezd).Stav.basicStav <> TBlkPrjBasicStav.none) then
     begin
      if ((Blk as TBlkPrejezd).Stav.PC_NOT) then
       begin
        bariery.Add(Self.JCBariera(_JCB_PREJEZD_NOUZOVE_OTEVREN, blk, Self.fproperties.prejezdy[i].Prejezd));
       end else begin
        if ((Blk as TBlkPrejezd).Stav.basicStav <> TBlkPrjBasicStav.uzavreno) then
          bariery.Add(Self.JCBariera(_JCB_PREJEZD_NEUZAVREN, blk, Self.fproperties.prejezdy[i].Prejezd));
       end;
     end else
       bariery.Add(Self.JCBariera(_JCB_PREJEZD_PORUCHA, blk, Self.fproperties.prejezdy[i].Prejezd));
   end;//for i

  // kontrola odvratu
  for i := 0 to Self.fproperties.odvraty.Count-1 do
   begin
    Blky.GetBlkByID(Self.fproperties.odvraty[i].Blok, Blk);
    glob := Blk.GetGlobalSettings();

    // kontrola polohy:
    if ((Blk as TBlkVyhybka).poloha <> Self.fproperties.odvraty[i].Poloha) then
      bariery.Add(Self.JCBariera(_JCB_VYHYBKA_KONC_POLOHA, Blk, Blk.id));

    // kontrola nouzoveho zaveru:
    if (not (Blk as TBlkVyhybka).vyhZaver) then
      bariery.Add(Self.JCBariera(_JCB_VYHYBKA_NOUZ_ZAVER, Blk, Blk.id));

    // kontrola spojky odvratu
    Blky.GetBlkByID((Blk as TBlkVyhybka).GetSettings.spojka, Blk2);
    if (blk2 <> nil) then
     begin
      // kontrola spravneho uzamceni odvratu
      if ((Blk as TBlkVyhybka).Poloha <> Self.fproperties.odvraty[i].Poloha) then
        if (not (Blk2 as TBlkVyhybka).vyhZaver) then
          bariery.Add(Self.JCBariera(_JCB_VYHYBKA_NOUZ_ZAVER, Blk2, Blk2.id));
     end;
   end;//for i

  if (Self.fproperties.Trat > -1) then
   begin
    if (Self.typ = TJCType.vlak) then
     begin
      Blky.GetBlkByID(Self.fproperties.useky[Self.fproperties.useky.Count-1], Blk);
      if (not TBlkTU(blk).sectReady) then
       begin
        Blky.GetBlkByID(Self.fproperties.Trat, Blk);
        bariery.Add(Self.JCBariera(_JCB_TRAT_NEPRIPRAVENA, blk, Self.fproperties.Trat));
       end;
     end;

    Blky.GetBlkByID(Self.fproperties.Trat, TBlk(trat));
    glob := trat.GetGlobalSettings();

    if ((trat.ZAK) and (Self.typ = TJCType.vlak)) then
      bariery.Add(Self.JCBariera(_JCB_TRAT_ZAK, blk, Self.fproperties.Trat));
    if ((not trat.ZAK) and (Self.typ = TJCType.posun)) then
      bariery.Add(Self.JCBariera(_JCB_TRAT_NOT_ZAK, blk, Self.fproperties.Trat));
    if (trat.Zaver) then
      bariery.Add(Self.JCBariera(_JCB_TRAT_ZAVER, blk, Self.fproperties.Trat));
    if (trat.Zadost) then
      bariery.Add(Self.JCBariera(_JCB_TRAT_ZADOST, blk, Self.fproperties.Trat));
    if (Self.fproperties.TratSmer <> trat.Smer) then
      bariery.Add(Self.JCBariera(_JCB_TRAT_NESOUHLAS, blk, Self.fproperties.Trat));
    if ((not trat.BP) and (Self.typ = TJCType.vlak)) then
      bariery.Add(Self.JCBariera(_JCB_TRAT_NO_BP, blk, Self.fproperties.Trat));

    usek := (Self.navestidlo as TBlkNav).UsekPred as TBlkUsek;
    lastUsek := TBlkUsek(Self.lastUsek);

    if ((usek.IsTrain) and (lastUsek.typ = btTU) and ((lastUsek as TBlkTU).InTrat = Self.data.Trat)) then
     begin
      if (trat.vyluka) then
       begin
        if ((trat.stav.trains.Count > 0) or ((trat.GetLastUsek(Self.data.TratSmer) as TBlkTU).Zaver <> TZaver.no)) then
          bariery.Add(Self.JCBariera(_JCB_TRAT_NEPRENOS, trat, Self.fproperties.Trat))
        else
          bariery.Add(Self.JCBariera(_JCB_TRAT_PRENOS_NAKONEC, trat, Self.fproperties.Trat));
       end else begin
        if ((lastUsek.IsTrain()) or (not trat.BP) or (trat.Smer <> Self.data.TratSmer)) then
          bariery.Add(Self.JCBariera(_JCB_TRAT_NEPRENOS, trat, Self.fproperties.Trat));
       end;
     end;
   end;

  // kontrola uzamceni zamku:
  for i := 0 to Self.fproperties.zamky.Count-1 do
   begin
    Blky.GetBlkByID(Self.fproperties.zamky[i].Blok, Blk);
    glob := Blk.GetGlobalSettings();

    // kontrola uzamceni
    if ((Blk as TBlkZamek).klicUvolnen) then
      bariery.Add(Self.JCBariera(_JCB_ZAMEK_NEUZAMCEN, blk, blk.id));

    // kontrola uzamceni
    if (not (Blk as TBlkZamek).nouzZaver) then
      bariery.Add(Self.JCBariera(_JCB_ZAMEK_NOUZ_ZAVER, blk, blk.id));
   end;//for i
end;

////////////////////////////////////////////////////////////////////////////////

procedure TJC.NC_PS_Callback(Sender:TIdContext; success:boolean);
var i:Integer;
    blk:TBlk;
begin
 if (success) then
  begin
   if (Self.krok = _NC_KROK_BARIERA_UPDATE) then
     Self.krok := _NC_KROK_BARIERY_POTVRZENY;
  end else begin
   Self.CancelStaveni();

   // aktualizace stavu navestidla (zobrazeni RNZ)
   Blky.GetBlkByID(Self.fproperties.navestidloBlok, Blk);
   Blk.Change();

   for i := 0 to Self.fproperties.useky.Count-1 do
    begin
     Blky.GetBlkByID(Self.fproperties.useky[i], Blk);
     (Blk as TBlkUsek).Zaver := TZaver.no;
    end;//for cyklus
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TJC.BarieryNCToPotvr(bariery:TJCBariery):TPSPodminky;
var i:Integer;
begin
 Result := TList<TPSPodminka>.Create();

 for i := 0 to bariery.Count-1 do
  begin
   case (bariery[i].typ) of
    _JCB_BLOK_DISABLED           : Result.Add(TOR.GetPSPodminka(bariery[i].blok, 'Blok neaktivní'));

    _JCB_USEK_OBSAZENO           : Result.Add(TOR.GetPSPodminka(bariery[i].blok, 'Úsek obsazen'));
    _JCB_USEK_SOUPRAVA           : Result.Add(TOR.GetPSPodminka(bariery[i].blok, 'Úsek obsahuje soupravu'));

    _JCB_PREJEZD_NOUZOVE_OTEVREN : Result.Add(TOR.GetPSPodminka(bariery[i].blok, 'Nouzově otevřen'));
    _JCB_PREJEZD_PORUCHA         : Result.Add(TOR.GetPSPodminka(bariery[i].blok, 'Porucha'));
    _JCB_PREJEZD_NEUZAVREN       : Result.Add(TOR.GetPSPodminka(bariery[i].blok, 'Neuzavřen'));

    _JCB_VYHYBKA_KONC_POLOHA     : Result.Add(TOR.GetPSPodminka(bariery[i].blok, 'Není správná poloha'));
    _JCB_VYHYBKA_NOUZ_ZAVER      : Result.Add(TOR.GetPSPodminka(bariery[i].blok, 'Není zaveden nouzový závěr'));
    _JCB_VYHYBKA_NESPAVNA_POLOHA : Result.Add(TOR.GetPSPodminka(bariery[i].blok, 'Není správná poloha'));

    _JCB_TRAT_ZAK                : Result.Add(TOR.GetPSPodminka(bariery[i].blok, 'Zákaz odjezdu'));
    _JCB_TRAT_NOT_ZAK            : Result.Add(TOR.GetPSPodminka(bariery[i].blok, 'Nezaveden zákaz odjezdu'));
    _JCB_TRAT_ZAVER              : Result.Add(TOR.GetPSPodminka(bariery[i].blok, 'Závěr'));
    _JCB_TRAT_NEPRIPRAVENA       : Result.Add(TOR.GetPSPodminka(bariery[i].blok, 'Nepovoluje odjezd'));
    _JCB_TRAT_ZADOST             : Result.Add(TOR.GetPSPodminka(bariery[i].blok, 'Probíhá žádost'));
    _JCB_TRAT_NESOUHLAS          : Result.Add(TOR.GetPSPodminka(bariery[i].blok, 'Nesouhlas'));
    _JCB_TRAT_NO_BP              : Result.Add(TOR.GetPSPodminka(bariery[i].blok, 'Bloková podmínka nezavedena'));
    _JCB_TRAT_NEPRENOS           : Result.Add(TOR.GetPSPodminka(bariery[i].blok, 'Nedojde k přenosu čísla vlaku'));
    _JCB_TRAT_PRENOS_NAKONEC     : Result.Add(TOR.GetPSPodminka(bariery[i].blok, 'Vlak bude přenesen až na konec trati'));

    _JCB_ZAMEK_NEUZAMCEN         : Result.Add(TOR.GetPSPodminka(bariery[i].blok, 'Neuzamčen'));
    _JCB_ZAMEK_NOUZ_ZAVER        : Result.Add(TOR.GetPSPodminka(bariery[i].blok, 'Není zaveden nouzový závěr'));
   end;//case bariera typ
  end;//for i
end;

////////////////////////////////////////////////////////////////////////////////

procedure TJC.SetProperties(prop:TJCProp);
var id_changed:boolean;
    nav_changed:boolean;
    orig_nav:TBlk;
begin
 id_changed := ((Self.id <> prop.id) and (Self.id <> -1));
 nav_changed := (Self.data.navestidloBlok <> prop.navestidloBlok);
 Blky.GetBlkByID(Self.data.navestidloBlok, orig_nav);
 Self.fproperties := prop;
 if (id_Changed) then
  begin
   // sem se skoci, pokud je potreba preskladat JC, protoze doslo ke zmene ID
   // pri vytvareni novych JC se sem neskace
   if (Assigned(Self.OnIdChanged)) then
     Self.OnIdChanged(Self);
  end;

 if (nav_changed) then
   if (Assigned(Self.OnNavChanged)) then
     Self.OnNavChanged(Self, orig_nav);
end;

////////////////////////////////////////////////////////////////////////////////

function TJC.GetTrain(nav:TBlk = nil; usek:TBlk = nil): TTrain;
begin
 if (nav = nil) then
   Blky.GetBlkByID(Self.fproperties.navestidloBlok, nav);

 Result := TBlkNav(nav).GetTrain(usek);
end;

////////////////////////////////////////////////////////////////////////////////

function TJC.GetAB():boolean;
var Blk:TBlk;
begin
 Blky.GetBlkByID(Self.fproperties.navestidloBlok, Blk);
 Result := ((Blk <> nil) and (Blk.typ = btNav) and (TBlkNav(Blk).ABJC = Self));
end;

////////////////////////////////////////////////////////////////////////////////

function TJC.IsAnyVyhMinus():boolean;
var vyh:TJCVyhZaver;
begin
 for vyh in Self.fproperties.vyhybky do
   if (vyh.Poloha = TVyhPoloha.minus) then
     Exit(true);
 Result := false;
end;

////////////////////////////////////////////////////////////////////////////////

function TJC.PorusenaKritickaPodminka():boolean;
var bariery:TJCBariery;
    bariera:TJCBariera;
    Navestidlo:TBlk;
begin
  Result := false;
  Blky.GetBlkByID(Self.fproperties.navestidloBlok, Navestidlo);
  bariery := TJCBariery.Create();
  try
    Self.KontrolaPodminekVCPC(bariery);
    for bariera in bariery do
     begin
      case (bariera.typ) of
        _JCB_BLOK_DISABLED, _JCB_BLOK_NOT_EXIST, _JCB_BLOK_NOT_TYP,
        _JCB_NAV_NOT_USEK, _JCB_USEK_OBSAZENO, _JCB_USEK_SOUPRAVA, _JCB_USEK_AB,
        _JCB_VYHYBKA_KONC_POLOHA, _JCB_VYHYBKA_NESPAVNA_POLOHA, _JCB_PREJEZD_NOUZOVE_OTEVREN,
        _JCB_PREJEZD_PORUCHA, _JCB_ODVRAT_KONC_POLOHA, _JCB_TRAT_NEPRIPRAVENA,
        _JCB_TRAT_ZADOST, _JCB_TRAT_NESOUHLAS, _JCB_TRAT_NO_BP, _JCB_ZAMEK_NEUZAMCEN:
          Exit(true);

        _JCB_TRAT_ZAK: Exit(Self.typ = TJCType.vlak);
      end;
     end;
  finally
    bariery.Free();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TJC.GetNav():TBlk;
begin
 Blky.GetBlkByID(Self.fproperties.navestidloBlok, Result);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TJC.ClientDisconnect(AContext: TIDContext);
begin
 if (Self.fstaveni.senderPnl = AContext) then
   Self.fstaveni.senderPnl := nil;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TJC.SetInitKrok();
begin
 if (Self.fstaveni.nc) then
  begin
   Self.krok := _NC_KROK_INIT;
   Self.fstaveni.timeOut := Now + EncodeTime(0, _NC_TIMEOUT_MIN, 0, 0);
  end else begin
   Self.krok := _JC_KROK_INIT;
   Self.fstaveni.timeOut := Now + EncodeTime(0, _JC_TIMEOUT_SEC div 60, _JC_TIMEOUT_SEC mod 60, 0);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TJC.GetWaitForLastUsekOrTratObsaz():Boolean;
begin
 Result := (Self.krok = _JC_KROK_CEKANI_POSLEDNI_USEK);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TJC.GetPtData(json:TJsonObject; includeStaveni:boolean);
var vyhZaver: TJCVyhZaver;
    odvratZaver: TJCOdvratZaver;
    newObj: TJsonObject;
    usek: Integer;
    prjZaver: TJCPrjZaver;
    refZaver: TJCRefZaver;
begin
 json['name'] := Self.fproperties.name;
 json['id'] := Self.fproperties.id;
 json['navId'] := Self.fproperties.navestidloBlok;
 case (Self.typ) of
  TJCType.vlak: json['typ'] := 'VC';
  TJCType.posun: json['typ'] := 'PC';
 end;
 case (Self.fproperties.dalsiNavaznost) of
  TJCNextNavType.zadna: json['dalsiNav'] := '-';
  TJCNextNavType.trat: json['dalsiNav'] := 'trat';
  TJCNextNavType.blok: begin
    json['dalsiNav'] := 'blok';
    json['dalsiNavId'] := Self.fproperties.dalsiNavestidlo;
  end;
 end;

 for vyhZaver in Self.fproperties.vyhybky do
  begin
   newObj := json.A['vyhybky'].AddObject();
   newObj['blok'] := vyhZaver.Blok;
   case (vyhZaver.Poloha) of
    TVyhPoloha.plus: newObj['poloha'] := '+';
    TVyhPoloha.minus: newObj['poloha'] := '-';
   end;
  end;

 for usek in Self.fproperties.useky do
   json.A['useky'].Add(usek);

 for odvratZaver in Self.fproperties.odvraty do
  begin
   newObj := json.A['odvraty'].AddObject();
   newObj['blok'] := odvratZaver.Blok;
   case (odvratZaver.Poloha) of
    TVyhPoloha.plus: newObj['poloha'] := '+';
    TVyhPoloha.minus: newObj['poloha'] := '-';
   end;
   newObj['refBlk'] := odvratZaver.ref_blk;
  end;

 for prjZaver in Self.fproperties.prejezdy do
  begin
   newObj := json.A['prejezdy'].AddObject();
   newObj['prejezd'] := prjZaver.Prejezd;
   newObj['oteviraci'] := prjZaver.oteviraci;
   for usek in prjZaver.uzaviraci do
     newObj.A['uzaviraci'].Add(usek);
  end;

 for refZaver in Self.fproperties.zamky do
  begin
   newObj := json.A['zamky'].AddObject();
   newObj['zamek'] := refZaver.Blok;
   newObj['refUsek'] := refZaver.ref_blk;
  end;

 for usek in Self.fproperties.vb do
   json.A['vb'].Add(usek);

 if (Self.fproperties.Trat <> -1) then
  begin
   json['trat'] := Self.fproperties.Trat;
   json['tratSmer'] := Integer(Self.fproperties.TratSmer);
  end;

 json['speedGo'] := Self.fproperties.speedGo;
 json['speedStop'] := Self.fproperties.speedStop;
 json['odbocka'] := Self.fproperties.odbocka;

 if (includeStaveni) then
   Self.GetPtStaveni(json['staveni']);
end;

procedure TJC.GetPtStaveni(json:TJsonObject);
begin
 json['staveni'] := Self.staveni;
 json['postaveno'] := Self.postaveno;
 json['krok'] := Self.fstaveni.krok;
 json['rozpadBlok'] := Self.fstaveni.rozpadBlok;
 json['rozpadRuseniBlok'] := Self.fstaveni.rozpadRuseniBlok;
 json['ab'] := Self.AB;
end;

procedure TJC.PostPtStav(reqJson:TJsonObject; respJson:TJsonObject);
var bariery: TJCBariery;
    bariera: TJCBariera;
    ok: Integer;
    ab: Boolean;
begin
 if ((Self.navestidlo = nil) or (TBlkNav(Self.navestidlo).OblsRizeni.Count = 0)) then
  begin
   PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '400', 'Návěstidlo není v OŘ');
   Exit();
  end;

 ab := (reqJson.Contains('ab') and reqJson.B['ab']);

 bariery := TJCBariery.Create();
 try
   ok := Self.StavJC(nil, TBlkNav(Self.navestidlo).OblsRizeni[0], bariery, nil, false, false, ab);
   respJson['success'] := (ok = 0);
   for bariera in bariery do
     Self.BarieraToJson(bariera, respJson.A['bariery'].AddObject());
 finally
   bariery.Free();
 end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TJC.BarieraToJson(const bariera: TJCBariera; result: TJsonObject);
var upoItem: TUPOItem;
begin
 result['typ'] := bariera.typ;
 if (bariera.blok <> nil) then
   result['blok'] := bariera.blok.id;
 result['param'] := bariera.param;
 if (CriticalBariera(bariera.typ)) then
   result['type'] := 'critical'
 else if (WarningBariera(bariera.typ)) then
   result['type'] := 'warning'
 else
   result['type'] := 'standard';

 upoItem := JCBarieraToMessage(bariera);
 result['description'] := upoItem[0].str + ' ' + upoItem[1].str + ' ' + upoItem[2].str;
end;

function TJC.GetLastUsek(): TBlk;
begin
 if (Self.data.useky.Count = 0) then
   Exit(nil);
 Blky.GetBlkByID(Self.data.useky[Self.data.useky.Count-1], Result);
 if (Result.typ <> btUsek) and (Result.typ <> btTU) then
   Result := nil;
end;

end.//unit
