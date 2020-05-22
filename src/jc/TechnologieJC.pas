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
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, Buttons, ComCtrls, fMain, TBloky, TBlok, IbUtils, Souprava,
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
   Krok:Integer;                                                                // aktualni krok staveni jizdni cesty
   TimeOut:TDateTime;                                                           // cas, pri jehoz prekroceni dojde k timeoutu JC
   SenderOR:TObject;                                                            // oblast rizeni, ktera vyvolala staveni JC, do teto OR jsou typicky odesilany notifikacni a chybove hlasky (napr. upozorneni vlevo dole panelu, potvrzovaci sekvence)
   SenderPnl:TIdContext;                                                        // konkretni panel, kery vyvolal staveni JC
   RozpadBlok,                                                                  // index useku, na ktery ma vkrocit souprava
   RozpadRuseniBlok:Integer;                                                    // index useku, ze ktereho ma vystoupit souprava
                                                                                  // index je index v seznamu useku, tedy napr. 0 =  0. usek v jizdni ceste
                                                                                  // -6 = postavena nouzova cesta, -5 = cesta neni postavena, -2 = navestidlo na STUJ, -1 = usek pred navestidlem, 0..n = useky JC
   from_stack:TObject;                                                          // odkaz na zasobnik, ze ktereho proehlo staveni JC
   nc:boolean;                                                                  // flag staveni nouzove cesty (vlakovou i posunovou)
   ncBariery:TJCBariery;                                                        // aktualni seznam barier pro potvrzovaci sekvenci pri staveni nouzove cesty
   ncBarieryCntLast:Integer;                                                    // posledni pocet barier ve staveni nouzove cesty
   nextVyhybka:Integer;                                                         // vyhybka, ktera se ma stavit jako dalsi
                                                                                // po postaveni vsechn vyhybek plynule prechazi do indexu seznamu odvratu
   ab:Boolean;                                                                  // po postaveni JC automaticky zavest AB
   prjWasClosed:Boolean;                                                        // jiz byl vydan povel k zavreni prejezdu
   lastUsekOrTratObsaz:Boolean;                                                 // je obsazen posledni usek JC, nestavit navestidlo a nezavirat prejezdy
  end;

  // vlastnosti jizdni cesty nemenici se se stavem:
  TJCprop = record
   Nazev:string;                                                                // nazev JC
   id:Integer;                                                                  // id jizdni cesty
   NavestidloBlok:Integer;                                                      // ID navestidla, od ktereho JC zacina
   TypCesty:TJCType;                                                            // typ JC (vlakova, posunova)
   DalsiNavaznost:TJCNextNavType;                                                         // typ dalsi navaznosti
   DalsiNavestidlo:Integer;                                                     // ID bloku dalsiho navestidla
   Vyhybky  : TList<TJCVyhZaver>;
   Useky    : TList<Integer>;
   Odvraty  : TList<TJCOdvratZaver>;
   Prejezdy : TList<TJCPrjZaver>;
   zamky    : TList<TJCRefZaver>;                                               // zamky, ktere musi byt uzamcene
   vb:TList<Integer>;                                                           // seznam variantnich bodu JC - obashuje postupne ID bloku typu usek

   Trat:Integer;                                                                // ID trati, na kterou JC navazuje; pokud JC nenavazuje na trat, je \Trat = -1
   TratSmer:TtratSmer;                                                          // pozadovany smer navazujici trate
   RychlostNoDalsiN,RychlostDalsiN:Byte;                                        // rychlost v JC pri dalsim navestidle navestici NEdovolujici navest, rychlost v JC pri dalsim navestidle navesticim dovolujici navest
   odbocka:Boolean;                                                             // jestli cesta vede do odbocky
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
      procedure VyhNeprestavenaJCPC(Sender:TObject);
      procedure VyhNeprestavenaNC(Sender:TObject);
      procedure VyhPrestavenaNC(Sender:TObject);
      procedure VyhPrestavenaJCPC(Sender:TObject);
      procedure NavNepostaveno(Sender:TObject);

      procedure KontrolaPodminekVCPC(var bariery:TList<TJCBariera>);            // kontrola podminek vlakovych a posunovych cest
      procedure KontrolaPodminekNC(var bariery:TList<TJCBariera>);              // kontrola podminek nouzovych cest
      procedure PodminkyNCStaveni(var bariery:TList<TJCBariera>);

      function BarieryNCToPotvr(bariery:TJCBariery):TPSPodminky;                // seznam barier nouzve cesty prevede na potvrzovaci sekvence pro klienta

      function GetSouprava(nav:TBlk = nil; usek:TBlk = nil): TSouprava; // vraci cislo soupravy na useku pred navestidlem

      function GetAB():boolean;
      function PorusenaKritickaPodminka():boolean;
      function GetNav():TBlk;
      function GetWaitForLastUsekOrTratObsaz():Boolean;

      procedure BarieraToJson(const bariera: TJCBariera; result: TJsonObject);

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

      property data:TJCprop read fproperties write SetProperties;
      property stav:TJCStaveni read fstaveni;
      property staveni:boolean read GetStaveni;
      property nazev:string read fproperties.Nazev;
      property postaveno:boolean read GetPostaveno;                             // true pokud je postavena navest
      property id:Integer read fproperties.id write fproperties.id;
      property AB:boolean read GetAB;
      property waitForLastUsekOrTratObsaz:boolean read GetWaitForLastUsekOrTratObsaz;

      property RozpadBlok:Integer read fstaveni.RozpadBlok write SetRozpadBlok;
      property RozpadRuseniBlok:Integer read fstaveni.RozpadRuseniBlok write SetRozpadRuseniBlok;
      property Krok:Integer read fstaveni.Krok write SetKrok;
      property navestidlo:TBlk read GetNav;

      property OnIdChanged: TNotifyEvent read fOnIdChanged write fOnIdChanged;
      property OnNavChanged: ENavChanged read fOnNavChanged write fOnNavChanged;
  end;

implementation

uses GetSystems, TechnologieRCS, THnaciVozidlo, TBlokNav, TBlokUsek, TOblsRizeni,
     TBlokPrejezd, TJCDatabase, Logging, TCPServerOR, SprDb, timeHelper,
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

 Self.fproperties.Vyhybky  := TList<TJCVyhZaver>.Create();
 Self.fproperties.Useky    := TList<Integer>.Create();
 Self.fproperties.Odvraty  := TList<TJCOdvratZaver>.Create();
 Self.fproperties.Prejezdy := TList<TJCPrjZaver>.Create();
end;//ctor

constructor TJC.Create(data:TJCprop);
begin
 inherited Create();

 Self.fproperties := data;
 Self.fstaveni := _def_jc_staveni;
 if (not Assigned(Self.fstaveni.ncBariery)) then Self.fstaveni.ncBariery := TList<TJCBariera>.Create();

 if (not Assigned(Self.fproperties.zamky))    then Self.fproperties.zamky := TList<TJCRefZaver>.Create();
 if (not Assigned(Self.fproperties.vb))       then Self.fproperties.vb := TList<Integer>.Create();
 if (not Assigned(Self.fproperties.Odvraty))  then Self.fproperties.Odvraty := TList<TJCOdvratZaver>.Create();
 if (not Assigned(Self.fproperties.Prejezdy)) then Self.fproperties.Prejezdy := TList<TJCPrjZaver>.Create();
 if (not Assigned(Self.fproperties.Vyhybky))  then Self.fproperties.Vyhybky := TList<TJCVyhZaver>.Create();
 if (not Assigned(Self.fproperties.Useky))    then Self.fproperties.Useky := TList<Integer>.Create();
end;//ctor

destructor TJC.Destroy();
var i:Integer;
begin
 if (Assigned(Self.fstaveni.ncBariery)) then FreeAndNil(Self.fstaveni.ncBariery);
 if (Assigned(Self.fproperties.zamky)) then FreeAndNil(Self.fproperties.zamky);
 if (Assigned(Self.fproperties.vb)) then  Self.fproperties.vb.Free();

 if (Assigned(Self.fproperties.Vyhybky))  then Self.fproperties.Vyhybky.Free();
 if (Assigned(Self.fproperties.Useky))    then Self.fproperties.Useky.Free();
 if (Assigned(Self.fproperties.Odvraty))  then Self.fproperties.Odvraty.Free();
 for i := 0 to Self.fproperties.Prejezdy.Count-1 do Self.fproperties.Prejezdy[i].uzaviraci.Free();
 if (Assigned(Self.fproperties.Prejezdy)) then Self.fproperties.Prejezdy.Free();

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
  if (Blky.GetBlkByID(Self.fproperties.NavestidloBlok, Blk) <> 0) then
   begin
    // blok navestidla neexistuje
    Result.Add(Self.JCBariera(_JCB_BLOK_NOT_EXIST, nil, Self.fproperties.NavestidloBlok));
    Exit;
   end;

  if (Blk.typ <> _BLK_NAV) then
   begin
    // blok navestidla neni typu navestidlo
    Result.Add(Self.JCBariera(_JCB_BLOK_NOT_TYP, Blk, Self.fproperties.NavestidloBlok));
    Exit;
   end;

  // blok disabled
  if ((Blk as TBlkNav).Navest < 0) then
   begin
    Result.Add(Self.JCBariera(_JCB_BLOK_DISABLED, Blk, Blk.id));
    Exit;
   end;

  if ((Blk as TBlkNav).UsekPred = nil) then
   begin
    // blok navestidla pred sebou nema zadny usek
    Result.Add(Self.JCBariera(_JCB_NAV_NOT_USEK, Blk, Self.fproperties.NavestidloBlok));
    Exit;
   end;

  // vyhybky:
  // kontrolujeme, jestli vyhybky existuji a jestli jsou to vyhybky
  for vyhZaver in Self.fproperties.Vyhybky do
   begin
    if (Blky.GetBlkByID(vyhZaver.Blok, Blk) <> 0) then
     begin
      Result.Add(Self.JCBariera(_JCB_BLOK_NOT_EXIST, nil, vyhZaver.Blok));
      Exit;
     end;//if

    if (Blk.typ <> _BLK_VYH) then
     begin
      Result.Add(Self.JCBariera(_JCB_BLOK_NOT_TYP, Blk, vyhZaver.Blok));
      Exit;
     end;

    // blok disabled
    if ((Blk as TBlkVyhybka).Stav.poloha = TVyhPoloha.disabled) then
     begin
      Result.Add(Self.JCBariera(_JCB_BLOK_DISABLED, Blk, Blk.id));
      Exit;
     end;

    // kontrola neprofilovych useku vyhybek pro polohu +
    if ((vyhZaver.Poloha = TVyhPoloha.plus) and (TBlkVyhybka(Blk).npBlokPlus <> nil) and
        (TBlkUsek(TBlkVyhybka(Blk).npBlokPlus).Obsazeno = TUsekStav.disabled)) then
     begin
      Result.Add(Self.JCBariera(_JCB_BLOK_DISABLED, TBlkVyhybka(Blk).npBlokPlus,
          TBlkVyhybka(Blk).npBlokPlus.id));
      Exit;
     end;

    // kontrola neprofilovych useku vyhybek pro polohu -
    if ((vyhZaver.Poloha = TVyhPoloha.minus) and (TBlkVyhybka(Blk).npBlokMinus <> nil) and
        (TBlkUsek(TBlkVyhybka(Blk).npBlokMinus).Obsazeno = TUsekStav.disabled)) then
     begin
      Result.Add(Self.JCBariera(_JCB_BLOK_DISABLED, TBlkVyhybka(Blk).npBlokMinus,
          TBlkVyhybka(Blk).npBlokMinus.id));
      Exit;
     end;
   end;//for i

  // useky:
  for usekZaver in Self.fproperties.Useky do
   begin
    // zkontrolujeme, jestli useky existuji a jestli jsou to useky
    if (Blky.GetBlkByID(usekZaver, Blk) <> 0) then
     begin
      Result.Add(Self.JCBariera(_JCB_BLOK_NOT_EXIST, nil, usekZaver));
      Exit;
     end;//if

    if ((Blk.typ <> _BLK_USEK) and (Blk.typ <> _BLK_TU)) then
     begin
      Result.Add(Self.JCBariera(_JCB_BLOK_NOT_TYP, Blk, usekZaver));
      Exit;
     end;

    // blok disabled
    if ((Blk as TBlkUsek).Stav.Stav = TUsekStav.disabled) then
     begin
      Result.Add(Self.JCBariera(_JCB_BLOK_DISABLED, Blk, Blk.id));
      Exit;
     end;
   end;//for i

  // kontrola prejezdu
  for prjZaver in Self.fproperties.Prejezdy do
   begin
    // kontrola existence bloku prejezdu
    if (Blky.GetBlkByID(prjZaver.Prejezd, blk) <> 0) then
     begin
      Result.Insert(0, Self.JCBariera(_JCB_BLOK_NOT_EXIST, nil, prjZaver.Prejezd));
      Exit;
     end;

    // kontrola typu bloku prejezdu
    if (blk.typ <> _BLK_PREJEZD) then
     begin
      Result.Insert(0, Self.JCBariera(_JCB_BLOK_NOT_TYP, blk, prjZaver.Prejezd));
      Exit;
     end;

    // blok disabled
    if ((Blk as TBlkPrejezd).Stav.basicStav = TBlkPrjBasicStav.disabled) then
     begin
      Result.Add(Self.JCBariera(_JCB_BLOK_DISABLED, Blk, Blk.id));
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
      if ((blk2.typ <> _BLK_USEK) and (blk2.typ <> _BLK_TU)) then
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
        if ((blk2.typ <> _BLK_USEK) and (blk2.typ <> _BLK_TU)) then
         begin
          Result.Insert(0, Self.JCBariera(_JCB_BLOK_NOT_TYP, blk, usekZaver));
          Exit;
         end;
       end;//for j
     end;
   end;//for i

  // kontrola odvratu
  for odvratZaver in Self.fproperties.Odvraty do
   begin
    if (Blky.GetBlkByID(odvratZaver.ref_blk, blk) <> 0) then
     begin
      Result.Insert(0, Self.JCBariera(_JCB_BLOK_NOT_EXIST, nil, odvratZaver.ref_blk));
      Exit;
     end;
    if ((blk.typ <> _BLK_USEK) and (blk.typ <> _BLK_TU)) then
     begin
      Result.Insert(0, Self.JCBariera(_JCB_BLOK_NOT_TYP, blk, odvratZaver.ref_blk));
      Exit;
     end;
    if (Blky.GetBlkByID(odvratZaver.Blok, blk) <> 0) then
     begin
      Result.Insert(0, Self.JCBariera(_JCB_BLOK_NOT_EXIST, nil, odvratZaver.Blok));
      Exit;
     end;
    if (blk.typ <> _BLK_VYH) then
     begin
      Result.Insert(0, Self.JCBariera(_JCB_BLOK_NOT_TYP, blk, odvratZaver.Blok));
      Exit;
     end;
    // blok disabled
    if ((Blk as TBlkVyhybka).Stav.poloha = TVyhPoloha.disabled) then
     begin
      Result.Add(Self.JCBariera(_JCB_BLOK_DISABLED, Blk, Blk.id));
      Exit;
     end;
   end;//for i

  // trat
  if (Self.fproperties.Trat > -1) then
   begin
    Blky.GetBlkByID(Self.fproperties.Useky[Self.fproperties.Useky.Count-1], blk);
    if (Blk.typ <> _BLK_TU) then
     begin
      Result.Add(Self.JCBariera(_JCB_BLOK_NOT_TYP, Blk, Self.fproperties.Useky[Self.fproperties.Useky.Count-1]));
      Exit;
     end;
    if (Blky.GetBlkByID(Self.fproperties.Trat, blk) <> 0) then
     begin
      Result.Insert(0, Self.JCBariera(_JCB_BLOK_NOT_EXIST, nil, Self.fproperties.Trat));
      Exit;
     end;
    if (blk.typ <> _BLK_TRAT) then
     begin
      Result.Insert(0, Self.JCBariera(_JCB_BLOK_NOT_TYP, blk, Self.fproperties.Trat));
      Exit;
     end;
    // blok disabled
    if ((Blk as TBlkTrat).stav.smer = TTratSmer.disabled) then
     begin
      Result.Add(Self.JCBariera(_JCB_BLOK_DISABLED, Blk, Blk.id));
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
    if (blk.typ <> _BLK_ZAMEK) then
     begin
      Result.Insert(0, Self.JCBariera(_JCB_BLOK_NOT_TYP, blk, blk.id));
      Exit;
     end;
    if (Blky.GetBlkByID(refZaver.ref_blk, blk) <> 0) then
     begin
      Result.Insert(0, Self.JCBariera(_JCB_BLOK_NOT_EXIST, nil, refZaver.ref_blk));
      Exit;
     end;
    if ((blk.typ <> _BLK_USEK) and (blk.typ <> _BLK_TU)) then
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
 privol := Blky.GetNavPrivol(Self.fstaveni.SenderOR as TOR);

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
    spr:TSouprava;
    prjZaver:TJCPrjZaver;
    vyhZaver:TJCVyhZaver;
    odvratZaver:TJCOdvratZaver;
    refZaver:TJCRefZaver;
begin
  // useky:
  if (Self.fproperties.Trat > -1) then
    cnt := Self.fproperties.Useky.Count-1
  else
    cnt := Self.fproperties.Useky.Count;

  for i := 0 to cnt-1 do
   begin
    Blky.GetBlkByID(Self.fproperties.Useky[i], Blk);
    glob := Blk.GetGlobalSettings();

    // obsazenost
    if ((i <> Self.fproperties.Useky.Count-1) or (Self.fproperties.TypCesty <> TJCType.posun)) then
     begin
      // kontrola disabled jiz probehla
      if ((Blk as TBlkUsek).Obsazeno <> TUsekStav.uvolneno) then
       begin
        if ((i = Self.fproperties.Useky.Count-1) and (Self.fproperties.Useky.Count > 1)) then
          bariery.Add(Self.JCBariera(_JCB_USEK_LAST_OBSAZENO, Blk, Blk.id))
        else
          bariery.Add(Self.JCBariera(_JCB_USEK_OBSAZENO, Blk, Blk.id));
       end else begin
        // souprava
        if ((Blk as TBlkUsek).IsSouprava()) then
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
  for vyhZaver in Self.fproperties.Vyhybky do
   begin
    Blky.GetBlkByID(vyhZaver.Blok, Blk);
    glob := Blk.GetGlobalSettings();

    // kontrola koncove polohy:
    if ((Integer((Blk as TBlkVyhybka).poloha) < 0) or (Integer((Blk as TBlkVyhybka).poloha) > 1)) then
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
     begin
      bariery.Add(Self.JCBariera(_JCB_USEK_OBSAZENO, TBlkVyhybka(Blk).npBlokPlus,
          TBlkVyhybka(Blk).npBlokPlus.id));
      Exit;
     end;

    // kontrola neprofiloveho styku pro polohu -
    if ((vyhZaver.Poloha = TVyhPoloha.minus) and (TBlkVyhybka(Blk).npBlokMinus <> nil) and
        (TBlkUsek(TBlkVyhybka(Blk).npBlokMinus).Obsazeno <> TUsekStav.uvolneno)) then
     begin
      bariery.Add(Self.JCBariera(_JCB_USEK_OBSAZENO, TBlkVyhybka(Blk).npBlokMinus,
          TBlkVyhybka(Blk).npBlokMinus.id));
      Exit;
     end;
   end;//for i

  // kontrola prejezdu
  for prjZaver in Self.fproperties.Prejezdy do
   begin
    Blky.GetBlkByID(prjZaver.Prejezd, Blk);
    if ((Blk as TBlkPrejezd).Stav.basicStav <> TBlkPrjBasicStav.none) then
     begin
      if ((Blk as TBlkPrejezd).Stav.PC_NOT) then
        bariery.Add(Self.JCBariera(_JCB_PREJEZD_NOUZOVE_OTEVREN, blk, prjZaver.Prejezd));
     end else begin
      bariery.Add(Self.JCBariera(_JCB_PREJEZD_PORUCHA, blk, prjZaver.Prejezd));
     end;//else NouzoveOtevreni

    // kontrola stitku prejezdu:
    if ((Blk as TBlkPrejezd).Stitek <> '') then
      bariery.Add(Self.JCBariera(_JCB_PREJEZD_STITEK, Blk, Blk.id));
   end;//for i

  // kontrola odvratu
  for odvratZaver in Self.fproperties.Odvraty do
   begin
    Blky.GetBlkByID(odvratZaver.Blok, Blk);
    glob := Blk.GetGlobalSettings();

    // kontrola koncove polohy:
    if ((Integer((Blk as TBlkVyhybka).poloha) < 0) or (Integer((Blk as TBlkVyhybka).poloha) > 1)) then
      bariery.Add(Self.JCBariera(_JCB_ODVRAT_KONC_POLOHA, blk, odvratZaver.Blok));

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
   end;//for i

  // kontrola trati
  if (Self.fproperties.Trat > -1) then
   begin
    Blky.GetBlkByID(Self.fproperties.Trat, Blk);
    glob := Blk.GetGlobalSettings();

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

    if ((cont) and (Self.fproperties.TypCesty = TJCType.vlak)) then
     begin
      Blky.GetBlkByID(Self.fproperties.Useky[Self.fproperties.Useky.Count-1], Blk);
      if (TBlkTU(blk).sectObsazeno = TUsekStav.obsazeno) then
       begin
        Blky.GetBlkByID(Self.fproperties.Trat, Blk);
        bariery.Add(Self.JCBariera(_JCB_TRAT_OBSAZENO, blk, Self.fproperties.Trat));
       end else if (not TBlkTU(blk).sectReady) then begin
        Blky.GetBlkByID(Self.fproperties.Trat, Blk);
        bariery.Add(Self.JCBariera(_JCB_TRAT_NEPRIPRAVENA, blk, Self.fproperties.Trat));
       end;
     end;

    Blky.GetBlkByID(Self.fproperties.Trat, Blk);

    // kontrola stitku uvazky v nasi OR:
    if ((TBlkUvazka(TBlkTrat(Blk).uvazkaA).OblsRizeni.Count > 0) and
        (TBlkUvazka(TBlkTrat(Blk).uvazkaA).OblsRizeni[0] = Self.fstaveni.SenderOR) and
        (TBlkUvazka(TBlkTrat(Blk).uvazkaA).Stitek <> '')) then
      bariery.Add(Self.JCBariera(_JCB_TRAT_STITEK, TBlkUvazka(TBlkTrat(Blk).uvazkaA),
          TBlkUvazka(TBlkTrat(Blk).uvazkaA).id));

    if ((TBlkUvazka(TBlkTrat(Blk).uvazkaB).OblsRizeni.Count > 0) and
        (TBlkUvazka(TBlkTrat(Blk).uvazkaB).OblsRizeni[0] = Self.fstaveni.SenderOR) and
        (TBlkUvazka(TBlkTrat(Blk).uvazkaB).Stitek <> '')) then
      bariery.Add(Self.JCBariera(_JCB_TRAT_STITEK, TBlkUvazka(TBlkTrat(Blk).uvazkaB),
          TBlkUvazka(TBlkTrat(Blk).uvazkaB).id));

    // stitky a vyluky na tratovych usecich
    for usek in TBlkTrat(Blk).GetSettings().Useky do
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
 Blky.GetBlkByID(Self.fproperties.NavestidloBlok, Blk2);
 Blk := (Blk2 as TBlkNav).UsekPred;

 if ((Blk as TBlkUsek).IsSouprava()) then
  begin
   flag := false;
   spr := Self.GetSouprava(Blk2, Blk);

   // kontrola rucniho rizeni lokomotiv
   if (Self.fproperties.TypCesty = TJCType.vlak) then
     for addr in spr.HVs do
       if ((HVDb[addr].stolen) or (HVDb[addr].ruc)) then
        begin
         bariery.Add(Self.JCBariera(_JCB_HV_RUC, nil, addr));
         flag := true;
        end;

   // pokud jsou jen nektere lokomotivy rizene rucne
   if (flag) then
     for addr in spr.HVs do
       if ((not HVDb[addr].stolen) and (not HVDb[addr].ruc)) then
        begin
         bariery.Add(Self.JCBariera(_JCB_HV_NOT_ALL_RUC));
         break;
        end;

   // kontrola smeru soupravy
   if (Self.fproperties.TypCesty = TJCType.vlak) then
    begin
     if (spr.sdata.dir_L or spr.sdata.dir_S) then
       if (((TBlkNav(Blk2).Smer = THVStanoviste.lichy) and (not spr.sdata.dir_L)) or
           ((TBlkNav(Blk2).Smer = THVStanoviste.sudy) and (not spr.sdata.dir_S))) then
         bariery.Add(Self.JCBariera(_JCB_SPR_SMER, nil, spr.index));
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
    cnt := Self.fproperties.Useky.Count-1
  else
    cnt := Self.fproperties.Useky.Count;

  for i := 0 to cnt-1 do
   begin
    Blky.GetBlkByID(Self.fproperties.Useky[i], Blk);
    glob := Blk.GetGlobalSettings();

    // zaver

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
  for vyhZaver in Self.fproperties.Vyhybky do
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
  for prjZaver in Self.fproperties.Prejezdy do
   begin
    Blky.GetBlkByID(prjZaver.Prejezd, Blk);
    // kontrola stitku prejezdu:
    if ((Blk as TBlkPrejezd).Stitek <> '') then
      bariery.Add(Self.JCBariera(_JCB_PREJEZD_STITEK, Blk, Blk.id));
   end;//for i

  // kontrola odvratu
  for odvratZaver in Self.fproperties.Odvraty do
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
    for usek in TBlkTrat(Blk).GetSettings().Useky do
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
        (TBlkUvazka(TBlkTrat(Blk).uvazkaA).OblsRizeni[0] = Self.fstaveni.SenderOR) and
        (TBlkUvazka(TBlkTrat(Blk).uvazkaA).Stitek <> '')) then
      bariery.Add(Self.JCBariera(_JCB_TRAT_STITEK, TBlkUvazka(TBlkTrat(Blk).uvazkaA),
          TBlkUvazka(TBlkTrat(Blk).uvazkaA).id));

    if ((TBlkUvazka(TBlkTrat(Blk).uvazkaB).OblsRizeni.Count > 0) and
        (TBlkUvazka(TBlkTrat(Blk).uvazkaB).OblsRizeni[0] = Self.fstaveni.SenderOR) and
        (TBlkUvazka(TBlkTrat(Blk).uvazkaB).Stitek <> '')) then
      bariery.Add(Self.JCBariera(_JCB_TRAT_STITEK, TBlkUvazka(TBlkTrat(Blk).uvazkaB),
          TBlkUvazka(TBlkTrat(Blk).uvazkaB).id));
   end;

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
  // timeout:
  if (nc) then
   Self.fstaveni.TimeOut := Now + EncodeTime(0, _NC_TIMEOUT_MIN, 0, 0)
  else
   Self.fstaveni.TimeOut := Now + EncodeTime(0, 0, _JC_TIMEOUT_SEC, 0);

  Self.fstaveni.from_stack := from_stack;
  Self.fstaveni.SenderOR := SenderOR;
  Self.fstaveni.SenderPnl := SenderPnl;
  Self.fstaveni.nc := nc;
  Self.fstaveni.ab := (abAfter) and (Self.fproperties.TypCesty = TJCType.vlak);
  Self.fstaveni.prjWasClosed := false;
  Self.fstaveni.lastUsekOrTratObsaz := false;

  writelog('JC '+Self.Nazev+' - požadavek na stavění, kontroluji podmínky', WR_VC);

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
      writelog('JC '+Self.Nazev+' : celkem '+IntToStr(bariery.Count)+' bariér, ukončuji stavění', WR_VC);
      if (SenderPnl <> nil) then
       begin
        Self.Krok := _KROK_KRIT_BARIERY;
        ORTCPServer.UPO(Self.fstaveni.SenderPnl, upo, true, nil, Self.CritBarieraEsc, Self);
       end;
      Exit(1);
     end else begin
      // bariery k potvrzeni
      if (((bariery.Count > 0) or ((nc) and (from_stack <> nil))) and (SenderPnl <> nil)) then
       begin
        writelog('JC '+Self.Nazev+' : celkem '+IntToStr(bariery.Count)+' warning bariér, žádám potvrzení...', WR_VC);
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

        ORTCPServer.UPO(Self.fstaveni.SenderPnl, upo, false, Self.UPO_OKCallback, Self.UPO_EscCallback, Self);
        Self.Krok := _KROK_POTVR_BARIERY;
        Exit(0);
       end;
     end;

    // v jzdni ceste nejsou zadne bariery -> stavim
    writelog('JC '+Self.Nazev+' : žádné bariéry, stavím', WR_VC);
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
 if (Self.Krok <> _KROK_POTVR_SEKV) then Exit;

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
   if (Self.fstaveni.SenderPnl <> nil) and (Self.fstaveni.SenderOR <> nil) then
     ORTCPServer.BottomError(Self.fstaveni.SenderPnl, 'Nelze postavit '+Self.nazev+' - kritické bariéry',
        (Self.fstaveni.SenderOR as TOR).ShortName, 'TECHNOLOGIE');
   bariery.Free();
   Exit();
  end;

 writelog('JC '+Self.Nazev+' : krok 2 : povrzovaci sekvence OK', WR_VC);
 Self.SetInitKrok();
end;

////////////////////////////////////////////////////////////////////////////////
// callbacky z upozornovacich barier:

procedure TJC.UPO_OKCallback(Sender:TObject);
var
    bariery:TJCBariery;
    critical:boolean;
    i:Integer;
    nav, usek:TBlk;
    podm:TList<TPSPodminka>;
begin
 if (Self.Krok <> _KROK_POTVR_BARIERY) then Exit();

 writelog('JC '+Self.Nazev+' : krok 1 : upozornění schválena, kontroluji znovu bariéry', WR_VC);

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
   if (Self.fstaveni.SenderPnl <> nil) and (Self.fstaveni.SenderOR <> nil) then
     ORTCPServer.BottomError(Self.fstaveni.SenderPnl, 'Nelze postavit '+Self.nazev+' - kritické bariéry',
        (Self.fstaveni.SenderOR as TOR).ShortName, 'TECHNOLOGIE');
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
   writelog('JC '+Self.Nazev+' : bariéry s potvrzovací sekvencí, žádám potvrzení...', WR_VC);
   Blky.GetBlkByID(Self.fproperties.NavestidloBlok, nav);
   Blky.GetBlkByID(Self.fproperties.Useky[Self.fproperties.Useky.Count-1], usek);

   if (Self.fstaveni.SenderPnl <> nil) and (Self.fstaveni.SenderOR <> nil) then
     ORTCPServer.Potvr(Self.fstaveni.SenderPnl, Self.PS_vylCallback, (Self.fstaveni.SenderOR as TOR),
        'Jízdní cesta s potvrzením', TBlky.GetBlksList(nav, usek), podm);

   Self.Krok := _KROK_POTVR_SEKV;
  end else begin
   // ne, takoveto bariery neexistuji -> stavim jizdni cestu
   Self.SetInitKrok();
  end;
end;//proceudre

procedure TJC.UPO_EscCallback(Sender:TObject);
begin
 if (Self.Krok = _KROK_POTVR_BARIERY) then
  begin
   Self.CancelStaveni();
   Self.Krok := _KROK_DEFAULT;
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
    usek, lastUsek:TBlkUsek;
    zamek:TBlkZamek;
    prejezd:TBlkPrejezd;
    navestidlo:TBlkNav;
    trat:TBlkTrat;
    tu:TBlkTU;
    oblr:TOR;
    tuAdd:TBlkTU;
    spr: TSouprava;
 begin
  if ((not Self.Staveni) and (Self.Krok <> _JC_KROK_CEKANI_POSLEDNI_USEK)) then Exit;

  Blky.GetBlkByID(Self.fproperties.NavestidloBlok, TBlk(navestidlo));

  //////////////////////////////////////////////////////////////////////////////
  // staveni vlakovych a posunovych cest:

  case (Self.Krok) of
   _JC_KROK_INIT:begin
      // nejprve priradime uvolneni zaveru posledniho bloku uvolneni zaveru predposledniho bloku
      if (Self.fproperties.Useky.Count > 1) then
       begin
        Blky.GetBlkByID(Self.fproperties.Useky[Self.fproperties.Useky.Count-2], TBlk(usek));
        usek.AddChangeEvent(usek.EventsOnZaverReleaseOrAB,
          CreateChangeEvent(ceCaller.CopyUsekZaver, Self.fproperties.Useky[Self.fproperties.Useky.Count-1]));
       end;

      writelog('Krok 10: useky: nastavuji staveci zavery', WR_VC);
      for usekZaver in Self.fproperties.Useky do
       begin
        Blky.GetBlkByID(usekZaver, TBlk(usek));
        usek.Zaver := TZaver.staveni;
       end;//for cyklus

      writelog('Krok 10 : vyhybky: zamykam do pozadovanych poloh', WR_VC);
      Self.fstaveni.nextVyhybka := -1;
      stavim := 0;
      nextVyhybka := -1;
      for i := 0 to Self.fproperties.Vyhybky.Count-1 do
       begin
        vyhZaver := Self.fproperties.Vyhybky[i];

        Blky.GetBlkByID(Self.fproperties.Vyhybky[i].Blok, TBlk(vyhybka));
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

        vyhybka.SetPoloha(TVyhPoloha(vyhZaver.Poloha),
                          true, false, Self.VyhPrestavenaJCPC, Self.VyhNeprestavenaJCPC);
       end;

      for i := 0 to Self.fproperties.Odvraty.Count-1 do
       begin
        odvratZaver := Self.fproperties.Odvraty[i];

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

        vyhybka.SetPoloha(TVyhPoloha(odvratZaver.Poloha),
                          true, false, Self.VyhPrestavenaJCPC, Self.VyhNeprestavenaJCPC);
       end;

      Self.fstaveni.nextVyhybka := nextVyhybka;

      writelog('Krok 10 : zamky: nastavuji zavery', WR_VC);
      for refZaver in Self.fproperties.zamky do
       begin
        Blky.GetBlkByID(refZaver.ref_blk, TBlk(usek));
        usek.AddChangeEvent(usek.EventsOnZaverReleaseOrAB,
          CreateChangeEvent(ceCaller.NullZamekZaver, refZaver.Blok));

        // nastaveni zaveru zamku
        Blky.GetBlkByID(refZaver.Blok, TBlk(zamek));
        zamek.Zaver := true;
       end;

      Self.Krok := _JC_KROK_CEKANI_VYHYBKA_POLOHA;
      writelog('Krok 11 : vyhybky: poloha: detekce',WR_VC);
     end;//case 0


   _JC_KROK_CEKANI_VYHYBKA_POLOHA:begin
      for vyhZaver in Self.fproperties.Vyhybky do
       begin
        Blky.GetBlkByID(vyhZaver.Blok, TBlk(vyhybka));
        if (vyhybka.Poloha <> vyhZaver.Poloha) then
          Exit;
       end;
      for odvratZaver in Self.fproperties.Odvraty do
       begin
        Blky.GetBlkByID(odvratZaver.Blok, TBlk(vyhybka));
        if (vyhybka.Poloha <> odvratZaver.Poloha) then
          Exit;
       end;

      writelog('Krok 11 : vyhybky: poloha: OK', WR_VC);
      Self.fstaveni.nextVyhybka := -1;

      writelog('Krok 11: useky: nastavuji nouzovy zaver', WR_VC);
      for usekZaver in Self.fproperties.Useky do
       begin
        Blky.GetBlkByID(usekZaver, TBlk(usek));
        usek.Zaver := TZaver.nouz;
       end;

      writelog('Krok 11: useky: kontroluji volnost useku s neprofilovymi styky, zapevnuji neprofilove useky', WR_VC);
      for vyhZaver in Self.fproperties.Vyhybky do
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
            if (Self.fstaveni.SenderPnl <> nil) and (Self.fstaveni.SenderOR <> nil) then
              ORTCPServer.BottomError(Self.fstaveni.SenderPnl, 'Neuvolněn ' + neprofil.name,
                  (Self.fstaveni.SenderOR as TOR).ShortName, 'TECHNOLOGIE');
            writelog('Krok 14 : Neprofilovy usek '+neprofil.name+' neuvolnen!', WR_VC);
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
        Self.Krok := _JC_KROK_FINALNI_ZAVER
      else
        Self.Krok := _JC_KROK_ZAVRIT_PREJEZDY;
     end;//case 1


   _JC_KROK_ZAVRIT_PREJEZDY:begin
       // prejezdy
       Self.fstaveni.prjWasClosed := true;
       anyUzavren := false;
       for i := 0 to Self.fproperties.Prejezdy.Count-1 do
        begin
         prjZaver := Self.fproperties.Prejezdy[i];
         if (prjZaver.uzaviraci.Count = 0) then
           continue;

         Blky.GetBlkByID(prjZaver.Prejezd, TBlk(prejezd));
         uzavren := false;

         // prejezd uzavirame jen v pripade, ze nejaky z jeho aktivacnich bloku je obsazen
         // v pripade posunove cesty uzavirame vzdy

         if (Self.fproperties.TypCesty = TJCType.posun) then
          begin
           // posunova cesta:
           writelog('Krok 12 : prejezd '+prejezd.name+' - uzaviram', WR_VC);

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
               writelog('Krok 12 : prejezd '+prejezd.name+' - aktivacni usek '+usek.name+' obsazen - uzaviram', WR_VC);

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

           writelog('Krok 12 : prejezd '+prejezd.name+' - zadny aktivacni usek neobsazen - nechavam otevreny', WR_VC);
          end;
        end;//for i

      if (anyUzavren) then
       begin
        Self.Krok := _JC_KROK_CEKANI_PREJEZDY;
        Self.fstaveni.TimeOut := Now + EncodeTime(0, _JC_PRJ_TIMEOUT_SEC div 60, _JC_PRJ_TIMEOUT_SEC mod 60, 0);
       end else
        Self.Krok := _JC_KROK_FINALNI_ZAVER;

     end;


   _JC_KROK_CEKANI_PREJEZDY:begin
       // kontrola stavu prejezdu
       for prjZaver in Self.fproperties.Prejezdy do
        begin
         if (prjZaver.uzaviraci.Count = 0) then
           continue;

         Blky.GetBlkByID(prjZaver.Prejezd, TBlk(prejezd));

         if (prejezd.Stav.basicStav <> TBlkPrjBasicStav.uzavreno) then Exit();
         writelog('Krok 13 : prejezd '+prejezd.name+' uzavren', WR_VC);
        end;//for i

      Self.Krok := _JC_KROK_FINALNI_ZAVER;
     end;


   _JC_KROK_FINALNI_ZAVER:begin
      writelog('Krok 14 : useky: nastavit validni zaver', WR_VC);

      aZaver := Self.fproperties.TypCesty;

      for i := 0 to Self.fproperties.Useky.Count-1 do
       begin
        usekZaver := Self.fproperties.Useky[i];
        Blky.GetBlkByID(usekZaver, TBlk(usek));
        usek.Zaver := TZaver(aZaver);
       end;

      navestidlo.DNjc := Self;

      if (Self.PorusenaKritickaPodminka()) then
       begin
        // Nepostavit navestidlo!
        Self.Krok := _JC_KROK_FINISH;
        Exit();
       end;

      if ((navestidlo.ZAM) or (Self.fstaveni.lastUsekOrTratObsaz)) then
       begin
        writelog('Krok 14 : navestidlo: nestavim',WR_VC);
        Self.Krok := _JC_KROK_FINISH;
       end else begin
        writelog('Krok 14 : navestidlo: stavim...', WR_VC);
        Self.NastavNav();
        Self.Krok := _JC_KROK_CEKANI_NAVESTIDLO;
       end;
   end;// case 14

   _JC_KROK_CEKANI_NAVESTIDLO:begin
     if (navestidlo.Navest > TBlkNav._NAV_STUJ) then
      begin
       writelog('Krok 15 : navestidlo postaveno', WR_VC);
       Self.Krok := _JC_KROK_FINISH;
      end;
   end;

   _JC_KROK_FINISH:begin
      Self.RusZacatekJC();
      Self.RusVBJC();
      Self.RusKonecJC();

      // nastavit front blok soupravy
      usek := navestidlo.UsekPred as TBlkUsek;
      if (usek.IsSouprava()) then
        Self.GetSouprava(Navestidlo, usek).front := usek;

      if (not usek.NavJCRef.Contains(Navestidlo)) then
        usek.NavJCRef.Add(Navestidlo);

      navestidlo.DNjc := Self;

      if (Self.fstaveni.lastUsekOrTratObsaz) then
        Self.Krok := _JC_KROK_CEKANI_POSLEDNI_USEK
      else
        Self.Krok := _KROK_DEFAULT;

      // kdyby nastala nize chyba, musi byt moznost JC smazat ze zasobniku
      if (Self.fstaveni.from_stack <> nil) then
        (Self.fstaveni.from_stack as TORStack).firstEnabled := true;

      // Kontrola kritickych podminek.
      // (behem staveni mohla nastat zmena)
      if (Self.PorusenaKritickaPodminka()) then
       begin
        if (navestidlo.Navest <> TBlkNav._NAV_STUJ) then
          navestidlo.Navest := TBlkNav._NAV_STUJ;
        if (Self.fstaveni.SenderPnl <> nil) and (Self.fstaveni.SenderOR <> nil) then
          ORTCPServer.BottomError(Self.fstaveni.SenderPnl, 'Podmínky pro JC nesplněny!',
            (Self.fstaveni.SenderOR as TOR).ShortName, 'TECHNOLOGIE');
        writelog('Krok 16 : Podmínky pro JC nesplněny!', WR_VC);
        Exit();
       end;

      Blky.GetBlkByID(Self.fproperties.Useky[Self.fproperties.Useky.Count-1], TBlk(lastUsek));

      // trat
      // zruseni redukce posledniho bloku jizdni cesty je navazano na zruseni zaveru trati
      // -> jakmile dojde ke zruseni zaveru posledniho bloku, dojde ke zruseni zaveru trati
      if (Self.fproperties.Trat > -1) then
       begin
        Blky.GetBlkByID(Self.fproperties.Trat, TBlk(trat));

        if (Self.fproperties.TypCesty = TJCType.vlak) then trat.Zaver := true;

        // posledni blok posunove cesty je trat = posun mezi dopravnami -> zavedeme zakaz odjezdu do trati
        if (Self.fproperties.TypCesty = TJCType.posun) then
         begin
          case (Self.fproperties.TratSmer) of
           TTratSmer.AtoB : TBlkUvazka(trat.uvazkaA).ZAK := true;
           TTratSmer.BtoA : TBlkUvazka(trat.uvazkaB).ZAK := true;
          end;
         end;

        trat.Smer := Self.fproperties.TratSmer;

        // zruseni zaveru posledniho bloku JC priradime zruseni zaveru trati
        lastUsek.AddChangeEvent(lastUsek.EventsOnZaverReleaseOrAB,
          CreateChangeEvent(ceCaller.NullTratZaver, Self.fproperties.Trat));
       end;

      if ((navestidlo.ZAM) or (Self.fstaveni.lastUsekOrTratObsaz)) then
        Self.RozpadBlok := -2
      else
        Self.RozpadBlok := -1;
      Self.RozpadRuseniBlok := -2;

      if (Self.data.TypCesty = TJCType.vlak) then
        Blky.SprPrediction(Navestidlo);

      // pokud je cesta ze zasobniku, smazeme ji odtam
      if (Self.fstaveni.from_stack <> nil) then
       begin
        (Self.fstaveni.from_stack as TORStack).RemoveJC(Self);
        Self.fstaveni.from_stack := nil;
       end;

      navestidlo.PropagatePOdjToTrat();

      if ((Self.fstaveni.ab) and (not navestidlo.AB)) then
        navestidlo.ABJC := Self;

      writelog('Postavena JC '+Self.Nazev, WR_VC);
   end;

   _JC_KROK_CEKANI_POSLEDNI_USEK: begin
     Blky.GetBlkByID(Self.fproperties.Useky[Self.fproperties.Useky.Count-1], blk);

     if (Self.fproperties.Trat > -1) then
      begin
       if (TBlkTU(blk).sectReady) then
        begin
         Self.fstaveni.lastUsekOrTratObsaz := false;
         Self.DN();
        end;
      end else begin
       if ((TBlkUsek(blk).Obsazeno = TUsekStav.uvolneno) and (not TBlkUsek(blk).IsSouprava)) then
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
    writelog('Krok 100: useky: nastavuji staveci zavery', WR_VC);
    for usekZaver in Self.fproperties.Useky do
     begin
      Blky.GetBlkByID(usekZaver, TBlk(usek));
      usek.Zaver := TZaver.staveni;
     end;//for cyklus

    // nastavit nouzovy zaver uvazky
    if (Self.fproperties.Trat > -1) then
     begin
      writelog('Krok 100: trat: nastavuji nouzovy zaver uvazky', WR_VC);
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
    writelog('Krok 100: vyhybky: nastavuji do pozadovanych poloh', WR_VC);

    Self.fstaveni.nextVyhybka := -1;
    stavim := 0;
    nextVyhybka := -1;
    for i := 0 to Self.fproperties.Vyhybky.Count-1 do
     begin
      vyhZaver := Self.fproperties.Vyhybky[i];
      Blky.GetBlkByID(vyhZaver.Blok, TBlk(vyhybka));
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

      vyhybka.SetPoloha(TVyhPoloha(vyhZaver.Poloha),
                        true, false, Self.VyhPrestavenaNC, Self.VyhNeprestavenaNC);
     end;

    for i := 0 to Self.fproperties.Odvraty.Count-1 do
     begin
      // nastaveni odvratu
      odvratZaver := Self.fproperties.Odvraty[i];
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

      vyhybka.SetPoloha(TVyhPoloha(odvratZaver.Poloha),
                        true, false, Self.VyhPrestavenaNC, Self.VyhNeprestavenaNC);
     end;

    Self.fstaveni.nextVyhybka := nextVyhybka;

    writelog('Krok 100: prejezdy: uzaviram', WR_VC);
    for prjZaver in Self.fproperties.Prejezdy do
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

    Self.Krok := _NC_KROK_BARIERA_UPDATE;
   end;//case 100

   _NC_KROK_BARIERA_UPDATE:begin
    // prubezne kontroluji podminky a zobrazuji potvrzovaci sekvenci

    // zjistime aktualni bariery:
    Self.fstaveni.ncBariery.Clear();
    Self.PodminkyNCStaveni(Self.fstaveni.ncBariery);

    // kontrolujeme rozdilnost seznamu:
    if (Self.fstaveni.ncBariery.Count <> Self.fstaveni.ncBarieryCntLast) then
     begin
      Blky.GetBlkByID(Self.fproperties.Useky[Self.fproperties.Useky.Count-1], TBlk(lastUsek));
      writelog('Krok 101: zmena potvr., odesilam aktualni seznam', WR_VC);
      if (Self.fproperties.TypCesty = TJCType.vlak) then
        str := 'Zapnutí přivolávací návěsti'
      else
        str := 'Nouzová posunová cesta';

      if (Self.fstaveni.SenderPnl <> nil) and (Self.fstaveni.SenderOR <> nil) then
        ORTCPServer.Potvr(Self.fstaveni.SenderPnl, Self.NC_PS_Callback, Self.fstaveni.SenderOR as TOR,
          str, TBlky.GetBlksList(Navestidlo, lastUsek), Self.BarieryNCToPotvr(Self.fstaveni.ncBariery));
     end;
    Self.fstaveni.ncBarieryCntLast := Self.fstaveni.ncBariery.Count;

    // nastavovani smeru trati:
    if (Self.fproperties.Trat > -1) then
     begin
      Blky.GetBlkByID(Self.fproperties.Trat, TBlk(trat));
      Blky.GetBlkByID(Self.fproperties.Useky[Self.fproperties.Useky.Count-1], TBlk(tu));

      // pokud v trati neni zavedena blokova podminka, zavedeme ji
      if ((Self.fproperties.TypCesty = TJCType.vlak) and (trat.Smer = Self.data.TratSmer) and (not trat.BP)) then
        trat.BP := true;

      // posledni blok posunove cesty je trat = posun mezi dopravnami -> zavedeme zakaz odjezdu do trati
      if ((Self.fproperties.TypCesty = TJCType.posun) and (trat.Smer = Self.fproperties.TratSmer)) then
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
    writelog('Krok 102: useky: rusim zavery', WR_VC);
    for usekZaver in Self.fproperties.Useky do
     begin
      Blky.GetBlkByID(usekZaver, TBlk(usek));
      usek.Zaver := TZaver.no;
     end;//for cyklus

    navestidlo.privol := Self;

    // i pokud je navetidlo ve STUJ, nastavuji navest (to je spravne chovani podle JOP)
    if (Self.fproperties.TypCesty = TJCType.vlak) then
     begin
      Self.NastavNav();
      writelog('Krok 102 : navestidlo: nastavuji na privolavaci navest...', WR_VC);
      Self.Krok := _NC_KROK_CEKANI_NAVESTIDLO;
     end else
      Self.Krok := _NC_KROK_FINISH;
   end;

   _NC_KROK_CEKANI_NAVESTIDLO:begin
     if (navestidlo.Navest = TBlkNav._NAV_PRIVOL) then
      begin
       writelog('Krok 103 : navestidlo postaveno', WR_VC);
       Self.Krok := _NC_KROK_FINISH;
      end;
   end;

   _NC_KROK_FINISH:begin
    Self.RusZacatekJC();
    Self.RusVBJC();
    Self.RusKonecJC();

    Self.Krok := _KROK_DEFAULT;

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

    if (Self.fproperties.TypCesty = TJCType.vlak) then
     begin
      usek := navestidlo.UsekPred as TBlkUsek;
      Blky.GetBlkByID(Self.fproperties.Useky[Self.fproperties.Useky.Count-1], TBlk(lastUsek));
      spr := Self.GetSouprava(Navestidlo, usek);

      // a)
      if ((lastUsek.typ = _BLK_USEK) and (lastUsek.Stav.stanicni_kolej) and
          (not lastUsek.SoupravyFull())) then
       begin
        if (usek.IsSouprava()) then
         begin
          if ((usek.typ = _BLK_TU) and (TBlkTU(usek).InTrat > -1)) then
           begin
            Blky.GetBlkByID((usek as TBlkTU).InTrat, TBlk(trat));
            trat.RemoveSpr(spr);
           end;

          // na dopravni kolej vlozime soupravu blize vjezdovemu navestidlu
          if (navestidlo.Smer = THVStanoviste.lichy) then
            lastUsek.AddSoupravaL(spr)
          else
            lastUsek.AddSoupravaS(spr);

          usek.RemoveSouprava(spr);
         end;
        Self.fstaveni.RozpadBlok := -6;
       end;

      // b)
      if ((lastUsek.typ = _BLK_TU) and ((lastUsek as TBlkTU).InTrat > -1)) then
        Blky.GetBlkByID((lastUsek as TBlkTU).InTrat, TBlk(trat))
      else
        trat := nil;

      if ((trat <> nil) and (usek.IsSouprava()) and (lastUsek.typ = _BLK_TU) and
          ((lastUsek as TBlkTU).InTrat = Self.data.Trat)) then
       begin
        tuAdd := nil;

        if (trat.vyluka) then
         begin
          // Pridat soupravu do posledniho bloku trati
          if ((trat.stav.soupravy.Count = 0) and ((trat.GetLastUsek(Self.data.TratSmer) as TBlkTU).Zaver = TZaver.no)) then
           begin
            tuAdd := (trat.GetLastUsek(Self.data.TratSmer) as TBlkTU);
            trat.SprChangeOR(spr, Self.data.TratSmer);
            if (trat.ChangesSprDir()) then
              spr.ChangeSmer();
          end;
         end else begin
          if ((not lastUsek.IsSouprava()) and (trat.BP) and (trat.Smer = Self.data.TratSmer)) then
           begin
            // Pridat soupravu do prvniho bloku trati
            tuAdd := (lastUsek as TBlkTU);
            tuAdd.poruchaBP := true;
           end;
         end;

        if (tuAdd <> nil) then
         begin
          trat.AddSpr(TBlkTratSouprava.Create(spr.index));
          tuAdd.AddSoupravaL(spr); // tady je jedno jestli zavolat L nebo S
                                   // v trati muze byt na jednom useku vzdy jen jedna souprava
                                   // kontrolovano vyse
          trat.Change();
          usek.RemoveSouprava(spr);
         end;
       end;
     end;//if typcesty = vlak

    writelog('Postavena NC '+Self.Nazev, WR_VC);
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
   if (Self.fstaveni.SenderPnl <> nil) then
     ORTCPServer.SendInfoMsg(Self.fstaveni.SenderPnl, reason);
   writelog('Nelze postavit JC '+Self.Nazev+' - '+reason, WR_VC);
  end;

 case (Self.Krok) of
    _NC_KROK_BARIERA_UPDATE:begin
      if (Self.fstaveni.SenderPnl <> nil) then
        ORTCPServer.PotvrClose(Self.fstaveni.SenderPnl, reason);
    end   
 end;//case Self.Krok

 // staveci zavery jsou zruseny, ostatni zavery zustavaji (lze je vyNUZovat)
 for usekZaver in Self.data.Useky do
  begin
   Blky.GetBlkByID(usekZaver, TBlk(usek));
   if (usek.Zaver = TZaver.staveni) then
      usek.Zaver := no;
  end;

 Self.fstaveni.nextVyhybka := -1;
 Self.Krok := _KROK_DEFAULT;
 Self.fstaveni.nc := false;
 Self.fstaveni.ab := false;
 Self.fstaveni.prjWasClosed := false;
 Self.RusZacatekJC();
 Self.RusVBJC();
 Self.RusKonecJC();
 if (Self.fstaveni.SenderPnl <> nil) then
   ORTCPServer.CancelUPO(Self.fstaveni.SenderPnl, Self);
 if (Self.fstaveni.from_stack <> nil) then
    if (stack_remove) then (Self.fstaveni.from_stack as TORStack).RemoveJC(Self)
  else
   if (Self.fstaveni.SenderOR <> nil) then
     (Self.fstaveni.SenderOR as TOR).BroadcastData('ZAS;FIRST;1');

 Self.fstaveni.from_stack := nil;
end;

////////////////////////////////////////////////////////////////////////////////

//rusi zacatek jizdni cesty
procedure TJC.RusZacatekJC();
var Blk:TBlk;
 begin
  Blky.GetBlkByID(Self.fproperties.NavestidloBlok, Blk);
  if (Blk = nil) then Exit;
  if (Blk.typ <> _BLK_NAV) then Exit;
  if ((Blk as TBlkNav).ZacatekVolba = TBlkNavVolba.none) then Exit;

  (Blk as TBlkNav).ZacatekVolba := TBlkNavVolba.none;
  if ((Blk as TBlkNav).DNjc = Self) then
    (Blk as TBlkNav).DNjc := nil;

  writelog('Zrusen zacatek staveni VC na bloku '+Blk.name,WR_VC);
 end;

//rusi konec jizdni cesty
procedure TJC.RusKonecJC();
var usek:TBlkUsek;
 begin
  Blky.GetBlkByID(Self.fproperties.Useky[Self.fproperties.Useky.Count-1], TBlk(usek));
  if (usek = nil) then Exit;
  usek.KonecJC := no;
 end;

procedure TJC.RusVBJC();
var Blk:TBlk;
    vb:Integer;
begin
 for vb in Self.data.vb do
  begin
   Blky.GetBlkByID(vb, Blk);
   if ((Blk <> nil) and ((Blk.typ = _BLK_USEK) or (Blk.typ = _BLK_TU))) then
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

  Blky.GetBlkByID(Self.fproperties.NavestidloBlok, TBlk(nav));
  nav.DNjc := nil;
  nav.RCtimerTimeout();

  for usekZaver in Self.fproperties.Useky do
   begin
    Blky.GetBlkByID(usekZaver, TBlk(usek));
    usek.Zaver := no;
   end;

  // zaver trati se rusi automaticky uvolnenim zaveru posledniho bloku pred trati

  writelog('Zrusena JC '+Self.Nazev, WR_VC);
 end;

//ruseni jizdni cesty bez ruseni zaveru bloku
procedure TJC.RusJCWithoutBlk();
var Nav:TBlk;
 begin
  Blky.GetBlkByID(Self.fproperties.NavestidloBlok, Nav);
  writelog('Probiha ruseni navesti JC '+Self.Nazev, WR_VC);

  if (((Nav as TBlkNav).DNjc = self) and ((Nav as TBlkNav).Navest > 0)) then
   begin
    (Nav as TBlkNav).Navest := TBlkNav._NAV_STUJ;
    if ((Nav as TBlkNav).AB) then
     begin
      (Nav as TBlkNav).AB := false; // automaticky zrusi AB
      if (Self.fstaveni.SenderPnl <> nil) then
        ORTCPServer.BottomError(Self.fstaveni.SenderPnl, 'Zrušena AB '+Nav.name,
          (Self.fstaveni.SenderOR as TOR).ShortName, 'TECHNOLOGIE');
     end;
   end;

  Self.Krok := _KROK_DEFAULT;
  Self.RozpadBlok := -5;
  Self.RozpadRuseniBlok := -5;
 end;

////////////////////////////////////////////////////////////////////////////////

//RozpadBlok = blok index, kam by mela souprava vjet
//RozpadRuseniBlok = blok index, kde je posledni detekovany vagon soupravy
procedure TJC.UsekyRusJC();
var Nav:TBlkNav;
    Blk:TBlk;
    Usek,DalsiUsek:TBlkUsek;
    i:Integer;
    spr: TSouprava;
begin
 Blky.GetBlkByID(Self.fproperties.NavestidloBlok, TBlk(Nav));

 // kontrola obsazenosti useku pred navestidlem
 Usek := Nav.UsekPred as TBlkUsek;
 if ((Self.RozpadBlok = -1) and ((Usek.Obsazeno = TUsekStav.obsazeno) or
     (Usek.GetSettings.RCSAddrs.Count = 0))) then
  begin
   Self.RozpadBlok       := 0;
   Self.RozpadRuseniBlok := -1;
  end;

 // uvolneni prvniho useku pred navestidlem v posunove ceste je signalem pro zhasnuti navestidla
 if ((Usek.GetSettings().RCSAddrs.Count > 0) and (Usek.Obsazeno = TUsekStav.uvolneno) and
     (Nav.Navest <> 0) and (Self.RozpadRuseniBlok = -1) and (Self.data.TypCesty = TJCType.posun) and
     (Self.RozpadBlok >= 1)) then
  begin
   writelog('JC '+Self.Nazev+': Uvolnen usek '+Usek.name+' : navestidlo '+
     Nav.name+' nastaveno na STUJ',WR_VC);
   Nav.JCZrusNavest();
  end;


 for i := Self.RozpadBlok to Self.fproperties.Useky.Count-1 do
  begin
   if (i < 0) then continue;    // i = -1 kdyz se kontroluje blok pred navestidlem, -2 pokud je navestidlo na STUJ, nebo zamkle

   Blky.GetBlkByID(Self.fproperties.Useky[i], TBlk(Usek));

   // druha cast podminky je tu pro pripad, kdy by byl na konci posunove cesty obsazeny usek
   if ((Usek.Obsazeno = obsazeno) and ((i < Self.fproperties.Useky.Count-1) or (Self.RozpadBlok > Self.fproperties.Useky.Count-2) or (Self.fproperties.TypCesty <> TJCType.posun))) then
    begin
     if (i = Self.RozpadBlok) then
      begin
       //pokud se tento usek rovna RozpadBloku
       Usek.Zaver := TZaver.nouz;

       if (Self.fproperties.TypCesty = TJCType.vlak) then
        begin
         //posuneme soupravu o blok dal
         Self.PredejDataDalsimuBloku();
        end;//if (Self.TypCesty = 0)

       // obsazeni prvniho useku
       // pozor: toto musi byt na tomto miste kvuli nastavovani Souprava.front
       if ((i = 0) and (Nav.Navest <> 0) and (Self.RozpadBlok = 0)) then
        begin
         // navestidlo pri obsazeni prvniho useku rusime v pripade, ze se jedna o VC
         if (Self.data.TypCesty = TJCType.vlak) then
          begin
           writelog('JC '+Self.Nazev+': Obsazen usek '+Usek.name+' : navestidlo '+Nav.name+' nastaveno na STUJ',WR_VC);
           Nav.JCZrusNavest();

           // aktualizace casu odjezdu v trati
           if (Self.fproperties.Trat > -1) then
            begin
             Blky.GetBlkByID(Self.fproperties.Trat, Blk);
             if (TBlkTrat(Blk).SprPredict <> nil) then
              begin
               TBlkTrat(Blk).SprPredict.time := timeHelper.hJOPnow();
               TBlkTrat(Blk).SprPredict.predict := false;
               TBlkTrat(Blk).Change();
              end;
            end;
          end;
        end;

       Self.RozpadBlok := Self.RozpadBlok + 1;

       // pokud jsme v predposlednim useku a posledni je nedetekovany, posuneme RozpadBlok jeste o jeden usek, aby se cesta mohla zrusit
       if (i = Self.fproperties.Useky.Count-2) then
        begin
         Blky.GetBlkByID(Self.fproperties.Useky[Self.fproperties.Useky.Count-1], Blk);
         if ((Blk as TBLkUsek).GetSettings().RCSAddrs.Count = 0) then
           Self.RozpadBlok := Self.RozpadBlok + 1;
        end;

       if ((i = Self.fproperties.Useky.Count-1) and (Self.fproperties.Trat > -1)) then
        begin
         // posledni usek JC obsazen -> trat
         Blky.GetBlkByID(Self.fproperties.Trat, Blk);

         if (Self.fproperties.TypCesty = TJCType.vlak) then
          begin
           (Blk as TBlkTrat).BP := true;
           if (Usek.IsSouprava()) then
            begin
             if (((Blk as TBlkTrat).SprPredict <> nil) and
                 ((Blk as TBlkTrat).SprPredict.souprava = Usek.Souprava)) then
               (Blk as TBlkTrat).AddSpr((Blk as TBlkTrat).SprPredict)
             else
               (Blk as TBlkTrat).AddSpr(TBlkTratSouprava.Create(Usek.SoupravaI));
            end;
          end;
         (Blk as TBlkTrat).Zaver := false;

         // nastavime rychlost souprave
         if (Self.fproperties.TypCesty = TJCType.vlak) then
           TBlkTU(Usek).rychUpdate := true;
        end;


      end else begin//if Self.RozpadBlok = 0
       if (Integer(Usek.Zaver) > 0) then
        begin
         //pokud jsme na jinem useku, nez RozpadBlok
         if ((Nav.Navest > 0) and (Nav.DNjc = Self)) then
          begin
           if (Self.fstaveni.SenderPnl <> nil) and (Self.fstaveni.SenderOR <> nil) then
             ORTCPServer.BottomError(Self.fstaveni.SenderPnl, 'Chyba povolovací návěsti '+Nav.name,
                (Self.fstaveni.SenderOR as TOR).ShortName, 'TECHNOLOGIE');
           Self.RusJCWithoutBlk();
          end;

         // v trati zaver nerusime, nesmime tam dat ani nouzovy, ani zadny zaver
         if ((i <> Self.fproperties.Useky.Count-1) or (Self.fproperties.Trat = -1)) then
           Usek.Zaver := TZaver.nouz;
        end;
      end;
    end;


   // kontrola zruseni jizdni cesty vlivem vynuzovani bloku
   if ((i = Self.RozpadBlok) and ((Usek.Zaver = TZaver.no))) then
    begin
     // pokud usek, na ktery se chystam vkrocit, nema zaver, je neco divne -> zrusit JC (predevsim kvuli predavani loko, ktere by mohlo narusit dalsi JC)
     Self.RusJCWithoutBlk();
     Exit();
    end;

  end;//for i

  // jizdni cesta konci uvolnenim predposledniho useku

  // mensitko je dulezite a ma smysl !
  //  kdyby tam bylo <=, mohl by se rozpadnout jediny usek, na kterem je souprava tim, ze se odobsadi
  if ((Self.RozpadRuseniBlok >= 0) and (Self.RozpadRuseniBlok < Self.RozpadBlok-1)) then
   begin
    //ziskani dotazovaneho useku
    Blky.GetBlkByID(Self.fproperties.Useky[Self.RozpadRuseniBlok], TBlk(Usek));

    if (Self.RozpadRuseniBlok+1 < Self.fproperties.Useky.Count) then
      Blky.GetBlkByID(Self.fproperties.Useky[Self.RozpadRuseniBlok+1], TBlk(DalsiUsek))
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

      Self.RozpadRuseniBlok := Self.RozpadRuseniBlok + 1;

      if ((Self.fproperties.TypCesty = TJCType.vlak) and (Usek.IsSouprava())) then
       begin
        writelog('JC '+Self.nazev+': smazana souprava '+Usek.Souprava.name+
          ' z bloku '+Usek.name, WR_SPRPREDAT);
        Usek.RemoveSoupravy();
       end;
     end;//if Self.RozpadBlok >= 1
   end;//if (cyklus2 = Self.RozpadRuseniBlok)

  // tady se resi pripad, kdy stanicni kolej zustane obsazena (protoze tam stoji vagony),
  // ale souprava se z ni musi odstanit uvolnenim prvniho bloku JC
  if ((Self.RozpadRuseniBlok = -1) and (Self.fproperties.Useky.Count > 0) and (Self.RozpadBlok > 0)) then
   begin
    Blky.GetBlkByID(Self.fproperties.Useky[0], TBlk(Usek));

    if (Self.fproperties.Useky.Count > 1) then
      Blky.GetBlkByID(Self.fproperties.Useky[1], TBlk(DalsiUsek))
    else
      DalsiUsek := nil;

    if ((Usek.Zaver = TZaver.nouz) and (Usek.Obsazeno = uvolneno) and
        ((DalsiUsek = nil) or (DalsiUsek.Obsazeno = TUsekStav.obsazeno) or
        (DalsiUsek.GetSettings.RCSAddrs.Count = 0))) then
     begin
      // uvolneni prvniho useku v posunove ceste je signalem pro zhasnuti navestidla
      if ((Nav.Navest <> 0) and (Self.data.TypCesty = TJCType.posun)) then
       begin
        writelog('JC '+Self.Nazev+': Uvolnen usek '+Usek.name+
          ' : navestidlo '+Nav.name+' nastaveno na STUJ',WR_VC);
        Nav.JCZrusNavest();
       end;

      if (Self.AB) then
        TBlkUsek(Usek).Zaver := TZaver.AB
      else
        TBlkUsek(Usek).Zaver := TZaver.no;

      Self.RozpadRuseniBlok := 1;

      if ((Self.fproperties.TypCesty = TJCType.vlak) and (Usek.IsSouprava())) then
       begin
        // mazani soupravy z useku pred navestidlem
        Blk := TBlkNav(Nav).UsekPred;
        spr := Self.GetSouprava(Nav, Blk);
        if (spr = TBlkUsek(Usek).Souprava) then
         begin
          writelog('JC '+Self.nazev+': smazana souprava '+spr.name+' z bloku '+Blk.name, WR_SPRPREDAT);
          (Blk as TBlkUsek).RemoveSouprava(spr);
         end;

        writelog('JC '+Self.nazev+': smazana souprava '+spr.name+' z bloku '+Usek.name, WR_SPRPREDAT);
        Usek.RemoveSoupravy();
       end;
     end;
   end;

  // mazani soupravy z useku pred navestidlem
  if ((Self.RozpadBlok > 0) and (Self.RozpadRuseniBlok = -1)) then
   begin
    Usek := Nav.UsekPred as TBlkUsek;
    if ((Usek.Obsazeno = TUsekStav.uvolneno) and (Usek.GetSettings.RCSAddrs.Count > 0)) then
     begin
      if (Usek.IsSouprava() and (Self.fproperties.TypCesty = TJCType.vlak)) then
       begin
        spr := Self.GetSouprava(nav, Usek);
        Usek.RemoveSouprava(spr);
        writelog('JC '+Self.nazev+': smazana souprava '+spr.name+' z bloku '+Usek.name, WR_SPRPREDAT);
       end;

      Self.RozpadRuseniBlok := 0;

      if ((Usek.typ = _BLK_TU) and (TBlkTU(Usek).Trat <> nil) and (TBlkTU(Usek).bpInBlk)) then
        TBlkTU(Usek).UvolnenoZJC();
     end;
   end;

  Usek := Nav.UsekPred as TBlkUsek;
  if ((Self.RozpadBlok = 0) and (Self.RozpadRuseniBlok = -1) and
      (TBlkUsek(Usek).Obsazeno <> TUsekStav.obsazeno)) then
   begin
    // usek pred navestidlem se opet uvolnil
    Self.RozpadBlok := -1;
    Self.RozpadRuseniBlok := -2;
   end;


  // tahleta silenost za OR je tu pro pripad, kdy JC ma jen jeden usek (to se stava napriklad na smyckach)
  if ((Self.RozpadRuseniBlok = Self.fproperties.Useky.Count-1) and (Self.fproperties.Useky.Count > 1))
      or ((Self.fproperties.Useky.Count = 1) and (Self.RozpadBlok = 1)) then
   begin
    // vsechny useky az na posledni jsou uvolneny -> rusime JC

    // tady by teoreticky melo prijit ruseni zaveru posledniho bloku, ale to neni poteba,
    // protoze zaver tohoto bloku je primo navazny na zaver predposledniho bloku pres redukce
    // to je napriklad kvuli tratim, ci z toho duvodu, ze na stanicnich kolejich nejde dat NUZ

    // pozor ale na JC, ktere maji jen jeden usek a ten je stanicni koleji:
    if (Self.fproperties.Useky.Count = 1) then
     begin
      Blky.GetBlkByID(Self.fproperties.Useky[0], TBlk(Usek));

      if (Self.AB) then
        Usek.Zaver := TZaver.AB
      else
        Usek.Zaver := TZaver.no;

      Usek := Nav.UsekPred as TBlkUsek;
      spr := Self.GetSouprava(Nav, Usek);

      // pokud ma cesta jen jeden usek, odstranime soupravu z useku pred navestidlem:
      if ((Self.fproperties.TypCesty = TJCType.vlak) and (spr <> nil)) then
       begin
        Usek.RemoveSouprava(spr);
        writelog('JC '+Self.nazev+': smazana souprava '+spr.name+' z bloku '+Usek.name, WR_SPRPREDAT);
       end;

      if ((Usek.typ = _BLK_TU) and (TBlkTU(Usek).Trat <> nil) and (TBlkTU(Usek).bpInBlk)) then
        TBlkTU(Usek).UvolnenoZJC();
     end;

    Self.RozpadBlok       := -5;
    Self.RozpadRuseniBlok := -5;
    writelog('JC '+Self.nazev+' - ruseni: rozpad cesty vlakem', WR_VC);
    if (Nav.DNjc = Self) then
     begin
      if (Nav.Navest > 0) then      // tato situace opravdu muze nastat - predstavte si posunovou cestu s jednim usekem vychazejici z nedetek koleje
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
   Blky.GetBlkByID(Self.fproperties.NavestidloBlok, Nav);
   if (((Nav as TBlkNav).Navest > 0) and ((Nav as TBlkNav).DNjc = Self)) then
    begin
     if (Self.fstaveni.SenderPnl <> nil) and (Self.fstaveni.SenderOR <> nil) then
       ORTCPServer.BottomError(Self.fstaveni.SenderPnl, 'Chyba povolovací návěsti '+Nav.name,
            (Self.fstaveni.SenderOR as TOR).ShortName, 'TECHNOLOGIE');
     Self.RusJCWithoutBlk();
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TJC.UsekyRusNC();
var TU, first : TBlkUsek;
    nav : TBlkNav;
begin
 Blky.GetBlkByID(Self.fproperties.NavestidloBlok, TBlk(nav));
 TU := TBlkTU((nav as TBlkNav).UsekPred);
 Blky.GetBlkByID(Self.fproperties.Useky[0], TBlk(first));

 if ((first.Obsazeno = TUsekStav.obsazeno) and (TU.Obsazeno = TUsekStav.uvolneno)
    and (not TU.IsSouprava())) then
  begin
   if (TBlkTU(TU).bpInBlk) then
     TBlkTU(TU).UvolnenoZJC();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

//preda soupravu v jizdni ceste dalsimu bloku v poradi
procedure TJC.PredejDataDalsimuBloku();
var UsekActual,UsekDalsi,Nav:TBlk;
    spr: TSouprava;
 begin
  if (Self.RozpadBlok = 0) then
   begin
    Blky.GetBlkByID(Self.fproperties.NavestidloBlok, Nav);
    UsekActual := (Nav as TBlkNav).UsekPred;
    spr := Self.GetSouprava(Nav, UsekActual);
    if ((UsekActual as TBlkUsek).IsSouprava()) then
      if (spr.front <> UsekActual) then
         Exit();
   end else begin
    Blky.GetBlkByID(Self.fproperties.Useky[Self.RozpadBlok-1], UsekActual);
    spr := TBlkUsek(UsekActual).Souprava;
   end;

  Blky.GetBlkByID(Self.fproperties.Useky[Self.RozpadBlok], UsekDalsi);
  if (not (UsekActual as TBlkUsek).IsSouprava()) then Exit;

  (UsekDalsi as TBlkUsek).zpomalovani_ready := true;
  (UsekDalsi as TBlkUsek).AddSoupravaL(spr);
  (UsekDalsi as TBlkUsek).Souprava.front := UsekDalsi;
  (UsekDalsi as TBlkUsek).houk_ev_enabled := true;
  writelog('JC '+Self.nazev+': predana souprava '+(UsekDalsi as TBlkUsek).Souprava.name+
      ' z bloku '+UsekActual.name+' do bloku '+UsekDalsi.name, WR_SPRPREDAT);

  Self.CheckSmyckaBlok(UsekDalsi);
 end;

procedure TJC.CheckSmyckaBlok(blk:TBlk);
var oblr:TOR;
begin
 if (((Blk as TBlkUsek).GetSettings().SmcUsek) and ((Blk as TBlkUsek).IsSouprava())) then
  begin
   // kontrola zmeny vychozi a cilove stanice
   for oblr in blk.OblsRizeni do
    begin
     if (oblr = (Blk as TBlkUsek).Souprava.stationTo) then
      begin
       (Blk as TBlkUsek).Souprava.InterChangeStanice(false);
       break;
      end;
    end;

   (Blk as TBlkUsek).Souprava.ChangeSmer();
   writelog('Obsazen smyckovy usek '+Blk.name+ ' - menim smer loko v souprave '+
      (Blk as TBlkUsek).Souprava.name, WR_SPRPREDAT);
  end;//if
end;

////////////////////////////////////////////////////////////////////////////////

//nastavi navestidlo JC na pozadovanou navest
procedure TJC.NastavNav();
var Nav,DalsiNav:TBlkNav;
    Navest:Integer;
 begin
  Blky.GetBlkByID(Self.fproperties.NavestidloBlok, TBlk(Nav));

  Navest := TBlkNav._NAV_STUJ;

  if ((Self.fstaveni.nc) and (Self.fproperties.TypCesty = TJCType.vlak)) then
   begin
    // nouzova cesta
    Navest := TBlkNav._NAV_PRIVOL;
   end else begin

    case (Self.fproperties.TypCesty) of
     TJCType.posun : begin
      // posunova cesta
      Navest := TBlkNav._NAV_POSUN_ZAJ;
     end;//case posun

     TJcType.vlak : begin
      Blky.GetBlkByID(Self.fproperties.DalsiNavestidlo, TBlk(DalsiNav));
      if ((Self.fproperties.DalsiNavaznost = TJCNextNavType.zadna) or
          (Self.fproperties.DalsiNavaznost = TJCNextNavType.trat) or
          ((Self.fproperties.DalsiNavaznost = TJCNextNavType.blok) and
           ((DalsiNav <> nil) and (DalsiNav.IsPovolovaciNavest())))) then
       begin
        // na dalsim navestidle lze jet
        if (Self.data.odbocka) then begin
          if ((Self.fproperties.DalsiNavaznost = TJCNextNavType.blok) and
              ((DalsiNav.Navest = TBlkNav._NAV_VYSTRAHA_40) or
               ((DalsiNav.Navest = TBlkNav._NAV_40_OCEK_40)) or
               (DalsiNav.Navest = TBlkNav._NAV_VOLNO_40))) then
            Navest := TBlkNav._NAV_40_OCEK_40
          else
            Navest := TBlkNav._NAV_VOLNO_40;
        end else begin
          if ((Self.fproperties.DalsiNavaznost = TJCNextNavType.blok) and
              ((DalsiNav.Navest = TBlkNav._NAV_VYSTRAHA_40) or
               ((DalsiNav.Navest = TBlkNav._NAV_40_OCEK_40)) or
               (DalsiNav.Navest = TBlkNav._NAV_VOLNO_40))) then
            Navest := TBlkNav._NAV_OCEK_40
          else
            Navest := TBlkNav._NAV_VOLNO;
        end;

       end else begin
        // na dalsim navestidle je na STUJ

        if (Self.data.odbocka) then
          Navest := TBlkNav._NAV_VYSTRAHA_40
        else
          Navest := TBlkNav._NAV_VYSTRAHA;
       end;
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
 Self.fproperties.Nazev := ini.ReadString(section, 'nazev', section);
 Self.fproperties.id := StrToInt(section);
 Self.fproperties.NavestidloBlok := ini.ReadInteger(section, 'nav', -1);
 Self.fproperties.TypCesty := TJCType(ini.ReadInteger(section, 'typ', -1));
 Self.fproperties.DalsiNavaznost := TJCNextNavType(ini.ReadInteger(section, 'dalsiNTyp', 0));
 Self.fproperties.DalsiNavestidlo := ini.ReadInteger(section, 'dalsiN', 0);
 Self.fproperties.RychlostDalsiN := ini.ReadInteger(section, 'rychDalsiN', 0);
 Self.fproperties.RychlostNoDalsiN := ini.ReadInteger(section, 'rychNoDalsiN', 0);
 Self.fproperties.Trat := ini.ReadInteger(section, 'trat', -1);
 Self.fproperties.TratSmer := TTratSmer(ini.ReadInteger(section, 'tratSmer', 0));

 // nacteni zaveru useku:
 sl  := TStringList.Create();
 sl2 := TStringList.Create();

 try
   ExtractStrings([';', ',', '|', '-', '('], [')'], PChar(ini.ReadString(section, 'useky', '')), sl);
   Self.fproperties.Useky.Count := sl.Count;
   for i := 0 to Self.fproperties.Useky.Count-1 do
     Self.fproperties.Useky[i] := StrToInt(sl[i]);

   // nacteni zaveru vyhybek:
   sl.Clear();
   ExtractStrings([';', ',', '|', '-', '(', ')'], [], PChar(ini.ReadString(section, 'vyhybky', '')), sl);
   sect_size := 2;
   cnt := (sl.Count div sect_size);
   Self.fproperties.Vyhybky.Clear();
   for i := 0 to cnt-1 do
    begin
     vyhZaver.Blok := StrToInt(sl[i*sect_size]);
     vyhZaver.Poloha := TVyhPoloha(StrToInt(sl[(i*sect_size)+1]));
     Self.fproperties.Vyhybky.Add(vyhZaver);
    end;//for i

   // nacteni odvratu:
   sl.Clear();
   ExtractStrings([';', ',', '|', '-', '(', ')'], [], PChar(ini.ReadString(section, 'odvraty', '')), sl);
   sect_size := 3;
   cnt := (sl.Count div sect_size);
   Self.fproperties.Odvraty.Clear();
   for i := 0 to cnt-1 do
    begin
     odvrat.Blok := StrToInt(sl[i*sect_size]);
     odvrat.Poloha := TVyhPoloha(StrToInt(sl[(i*sect_size)+1]));
     odvrat.ref_blk := StrToInt(sl[(i*sect_size)+2]);
     Self.fproperties.Odvraty.Add(odvrat);
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

     Self.fproperties.Prejezdy.Add(prj);
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

end;

procedure TJC.SaveData(ini:TMemIniFile; section:string);
var line:string;
    i,j:Integer;
begin
 ini.WriteString (section, 'nazev', Self.fproperties.Nazev);
 ini.WriteInteger(section, 'nav', Self.fproperties.NavestidloBlok);
 ini.WriteInteger(section, 'typ', Integer(Self.fproperties.TypCesty));
 if (Self.fproperties.DalsiNavaznost <> TJCNextNavType.zadna) then
   ini.WriteInteger(section, 'dalsiNTyp', Integer(Self.fproperties.DalsiNavaznost));
 if (Self.fproperties.DalsiNavaznost = TJCNextNavType.blok) then
   ini.WriteInteger(section, 'dalsiN', Self.fproperties.DalsiNavestidlo);
 ini.WriteInteger(section, 'rychDalsiN', Self.fproperties.RychlostDalsiN);
 ini.WriteInteger(section, 'rychNoDalsiN', Self.fproperties.RychlostNoDalsiN);

 if (Self.fproperties.odbocka = Self.IsAnyVyhMinus) then
   ini.DeleteKey(section, 'odbocka')
 else
   ini.WriteBool(section, 'odbocka', Self.fproperties.odbocka);

 if (Self.fproperties.Trat > -1) then
  begin
   ini.WriteInteger(section, 'trat', Self.fproperties.Trat);
   ini.WriteInteger(section, 'tratSmer', Integer(Self.fproperties.TratSmer));
  end;

 // useky
 line := '';
 for i := 0 to Self.fproperties.Useky.Count-1 do
   line := line + IntToStr(Self.fproperties.Useky[i]) + ',';
 if (line <> '') then
   ini.WriteString(section, 'useky', line);

 // vyhybky
 line := '';
 for i := 0 to Self.fproperties.Vyhybky.Count-1 do
   line := line + '(' + IntToStr(Self.fproperties.Vyhybky[i].Blok) + ',' + IntToStr(Integer(Self.fproperties.Vyhybky[i].Poloha)) + ')';
 if (line <> '') then
   ini.WriteString(section, 'vyhybky', line);

 // odvraty
 line := '';
 for i := 0 to Self.fproperties.Odvraty.Count-1 do
   line := line + '(' + IntToStr(Self.fproperties.Odvraty[i].Blok) + ',' + IntToStr(Integer(Self.fproperties.Odvraty[i].Poloha)) + ',' + IntToStr(Self.fproperties.Odvraty[i].ref_blk)+ ')';
 if (line <> '') then
   ini.WriteString(section, 'odvraty', line);

 // prejezdy
 line := '';
 for i := 0 to Self.fproperties.Prejezdy.Count-1 do
  begin
   line := line + '(' + IntToStr(Self.fproperties.Prejezdy[i].Prejezd);

   if (Self.fproperties.Prejezdy[i].uzaviraci.Count > 0) then
    begin
     line := line + ',' + IntToStr(Self.fproperties.Prejezdy[i].oteviraci)+ ',';
     for j := 0 to Self.fproperties.Prejezdy[i].uzaviraci.Count-1 do
       line := line + IntToStr(Self.fproperties.Prejezdy[i].uzaviraci[j]) + ',';
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

 if (Now > Self.fstaveni.TimeOut) then
  begin
   case (Self.Krok) of
    _JC_KROK_CEKANI_PREJEZDY:begin
      // prejezd(y) neuzavren
      for prjZaver in Self.fproperties.Prejezdy do
       begin
        Blky.GetBlkByID(prjZaver.Prejezd, TBlk(prejezd));
        if (prejezd.Stav.basicStav <> TBlkPrjBasicStav.uzavreno) then
          if (Self.fstaveni.SenderPnl <> nil) and (Self.fstaveni.SenderOR <> nil) then
            ORTCPServer.BottomError(Self.fstaveni.SenderPnl, 'Neuzavřen '+prejezd.name,
              (Self.fstaveni.SenderOR as TOR).ShortName, 'TECHNOLOGIE');
       end;//for i
    end;//case 13

   else
     if (Self.fstaveni.SenderPnl <> nil) and (Self.fstaveni.SenderOR <> nil) then
       ORTCPServer.BottomError(Self.fstaveni.SenderPnl, 'Timeout '+Self.nazev,
         (Self.fstaveni.SenderOR as TOR).ShortName, 'TECHNOLOGIE');
   end;//else case

   //timeout
   Self.CancelStaveni('Překročení času stavění JC', true);    // toto je docasne reseni: cestu vymazeme ze zasobniku
  end;//if timeout
end;

////////////////////////////////////////////////////////////////////////////////

function TJC.GetStaveni():boolean;
begin
 Result := ((Self.Krok > _KROK_DEFAULT) and (Self.Krok <> _JC_KROK_CEKANI_POSLEDNI_USEK));
end;

function TJC.GetPostaveno():boolean;
begin
 Result := (Self.fstaveni.RozpadBlok > -5);
end;

////////////////////////////////////////////////////////////////////////////////

// true = je mozno DN
//tato funkce kontroluje, jestli je mozne znovupostavit cestu i kdyz byla fakticky zrusena = musi zkontrolovat vsechny podminky
function TJC.CanDN():boolean;
var i:Integer;
    spr: TSouprava;
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
 spr := Self.GetSouprava();

 // zkontrolujeme zavery bloku
 // JC NELZE obnovit z useku, na kterych uplne spadl zaver (do zadneho zaveru)
 // porusily by se reference na redukce menu
 for i := 0 to Self.fproperties.Useky.Count-1 do
  begin
   usekZaver := Self.fproperties.Useky[i];
   Blky.GetBlkByID(usekZaver, TBlk(usek));
   if ((usek.Zaver = TZaver.no) or (usek.Zaver = TZaver.staveni) or (usek.NUZ) or
      ((usek.Obsazeno <> TUsekStav.uvolneno) and
       ((Self.fproperties.TypCesty = TJCType.vlak) or (i <> Self.fproperties.Useky.Count-1)))) then Exit(false);

   // na usecich v ceste je dovoleno mit soupravu pred navestidlem, v takovem
   // pripade ji DN z useku v ceste smaze

   if (Self.fproperties.TypCesty = TJCType.vlak) then
    begin
     if (spr = nil) then
      begin
       // pred navestidlem neni souprava -> na usecich nesmi byt zadna souprava
       if (usek.IsSouprava()) then Exit(false);
      end else begin
       // pred navestidlem je souprava -> na usecich smi byt jen stejna souprava
       // jako pred navestidlem
       if ((usek.IsSouprava()) and
           ((usek.Soupravs.Count > 1) or (usek.Souprava <> spr))) then
         Exit(false);
      end;
    end;
  end;//for i

 // zkontrolujeme polohu vyhybek
 for vyhZaver in Self.fproperties.Vyhybky do
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
 for odvratZaver in Self.fproperties.Odvraty do
  begin
   Blky.GetBlkByID(odvratZaver.Blok, TBlk(vyhybka));
   if (vyhybka.Poloha <> odvratZaver.Poloha) then Exit(false);
  end;//for i

 // zkontrolujeme poruchy prejezdu
 //  prejezdy, na kterych je zaver, by taky mely byt uzavrene
 for prjZaver in Self.fproperties.Prejezdy do
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
   Blky.GetBlkByID(Self.fproperties.Useky[Self.fproperties.Useky.Count-1], TBlk(usek));
   if (trat.Zadost) then Exit(false);
   if ((((not (TBlkTU(usek).sectReady)) or (trat.ZAK)) and (Self.fproperties.TypCesty = TJCType.vlak)) or
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
 writelog('DN JC '+Self.nazev, WR_VC);
 Self.fstaveni.TimeOut := Now + EncodeTime(0, 0, _JC_TIMEOUT_SEC, 0);

 if (Self.fstaveni.prjWasClosed) then
   Self.Krok := _JC_KROK_FINALNI_ZAVER
 else
   Self.Krok := _JC_KROK_ZAVRIT_PREJEZDY;
end;

////////////////////////////////////////////////////////////////////////////////

// volano z navestidla pri STUJ
// nevolat nidky jindy !
procedure TJC.STUJ();
begin
 Self.RozpadBlok := -2;
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
   Blky.GetBlkByID(Self.fproperties.Prejezdy[data].Prejezd, TBlk(prejezd));
   prejezd.Zaver := true;
   writelog('JC '+Self.nazev+': obsazen '+TBlkUsek(Sender).name+
      ' - uzaviram prejezd '+prejezd.name, WR_VC);

   // prejezd se uzavira -> po uvolneni zaveru bloku pod prejezdem prejezd opet otevrit
   Blky.GetBlkByID(Self.fproperties.Prejezdy[data].oteviraci, TBlk(usek));
   usek.AddChangeEvent(usek.EventsOnZaverReleaseOrAB,
     CreateChangeEvent(ceCaller.NullPrejezdZaver, Self.fproperties.Prejezdy[data].Prejezd));
  end;

 for blkId in Self.fproperties.Prejezdy[data].uzaviraci do
  begin
   Blky.GetBlkByID(blkId, TBlk(usek));
   usek.RemoveChangeEvent(usek.EventsOnObsaz, CreateChangeEvent(Self.UsekClosePrj, data));
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TJC.SetRozpadBlok(RozpadBlok:Integer);
begin
 Self.fstaveni.RozpadBlok := RozpadBlok;
 Self.changed := true;
end;

procedure TJC.SetRozpadRuseniBlok(RozpadRuseniBlok:Integer);
begin
 Self.fstaveni.RozpadRuseniBlok := RozpadRuseniBlok;
 Self.changed := true;
end;

procedure TJC.SetKrok(Krok:Integer);
begin
 Self.fstaveni.Krok := Krok;
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
    Result[2] := GetUPOLine(IntToStr(Bariera.param) + ' : ' + HVDb[Bariera.param].Data.Nazev);
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

    if ((Self.fproperties.TypCesty = TJCType.posun) and (canZAK)) then
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
    Result[2] := GetUPOLine('Soprava ' + Soupravy[Bariera.param].name);
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
        TTratSmer.AtoB : Result := (Self.fproperties.TypCesty = TJCType.posun) and (TBlkUvazka(TBlkTrat(Blk).uvazkaA).ZAK);
        TTratSmer.BtoA : Result := (Self.fproperties.TypCesty = TJCType.posun) and (TBlkUvazka(TBlkTrat(Blk).uvazkaB).ZAK);
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
 }

 if (Self.fstaveni.nextVyhybka < 0) then Exit();

 if (Self.fstaveni.nextVyhybka < Self.fproperties.Vyhybky.Count) then
  begin
   // stavim dalsi vyhybku
   for i := Self.fstaveni.nextVyhybka to Self.fproperties.Vyhybky.Count-1 do
    begin
     Blky.GetBlkByID(Self.fproperties.Vyhybky[i].Blok, Blk);
     if ((Blk as TBlkVyhybka).Poloha <> TVyhPoloha(Self.fproperties.Vyhybky[i].Poloha)) then
      begin
       Self.fstaveni.nextVyhybka := i+1;
       (Blk as TBlkVyhybka).SetPoloha(TVyhPoloha(Self.fproperties.Vyhybky[i].Poloha),
                                      true, false, Self.VyhPrestavenaJCPC, Self.VyhNeprestavenaJCPC);
       Exit();
      end;
    end;

   // sem se skoci, pokud vsechny zbyvajici vyhybky byly ve spravne poloze
   Self.fstaveni.nextVyhybka := Self.fproperties.Vyhybky.Count;
  end;

 if (Self.fstaveni.nextVyhybka < Self.fproperties.Vyhybky.Count+Self.fproperties.Odvraty.Count) then
  begin
   // stavim dalsi odvrat
   odvrat := Self.fstaveni.nextVyhybka - Self.fproperties.Vyhybky.Count;
   for i := odvrat to Self.fproperties.Odvraty.Count-1 do
    begin
     // nastaveni odvratu
     Blky.GetBlkByID(Self.fproperties.Odvraty[i].Blok, Blk);
     if ((Blk as TBlkVyhybka).Poloha <> TVyhPoloha(Self.fproperties.Odvraty[i].Poloha)) then
      begin
       TBlkVyhybka(Blk).IntentionalLock();

       Blky.GetBlkByID(Self.fproperties.Odvraty[i].ref_blk, TBlk(usek));
       usek.AddChangeEvent(usek.EventsOnZaverReleaseOrAB,
         CreateChangeEvent(ceCaller.NullVyhybkaMenuReduction, Self.fproperties.Odvraty[i].Blok));

       Self.fstaveni.nextVyhybka := i+Self.fproperties.Vyhybky.Count+1;
       TBlkVyhybka(Blk).SetPoloha(TVyhPoloha(Self.fproperties.Odvraty[i].Poloha),
                                  true, false, Self.VyhPrestavenaJCPC, Self.VyhNeprestavenaJCPC);
       Exit();
      end;
    end;

   // sem se skoci, pokud vsechny zbyvajici odvraty byly ve spravne poloze
   Self.fstaveni.nextVyhybka := -1;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TJC.VyhNeprestavenaJCPC(Sender:TObject);
begin
 if (not Self.staveni) then Exit();

 if (Self.fstaveni.SenderPnl <> nil) and (Self.fstaveni.SenderOR <> nil) then
   ORTCPServer.BottomError(Self.fstaveni.SenderPnl, 'Nepřestavena '+(Sender as TBlkVyhybka).name,
     (Self.fstaveni.SenderOR as TOR).ShortName, 'TECHNOLOGIE');
 Self.CancelStaveni('', true);
 Self.RusJC();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TJC.VyhNeprestavenaNC(Sender:TObject);
begin
 Self.VyhPrestavenaNC(Sender);
end;

procedure TJC.VyhPrestavenaNC(Sender:TObject);
var Navestidlo, spojka:TBlk;
    Blk:TBlk;
    odvrat:Integer;
begin
 if ((Self.fstaveni.Krok <> 100) and (Self.fstaveni.Krok <> 101)) then Exit();

 TBlkVyhybka(Sender).vyhZaver := true;

 Blky.GetBlkByID(Self.fproperties.NavestidloBlok, Navestidlo);
 TBlkNav(Navestidlo).AddBlkToRnz(TBlk(Sender).id, false);

 if (TBlkVyhybka(Sender).GetSettings().spojka > -1) then
  begin
   Blky.GetBlkByID(TBlkVyhybka(Sender).GetSettings().spojka, spojka);
   TBlkVyhybka(spojka).vyhZaver := true;
   TBlkNav(Navestidlo).AddBlkToRnz(TBlkVyhybka(Sender).GetSettings().spojka, false);
  end;

 // staveni dalsich vyhybek

 if (Self.fstaveni.nextVyhybka < 0) then Exit();

 if (Self.fstaveni.nextVyhybka < Self.fproperties.Vyhybky.Count) then
  begin
   // stavim dalsi vyhybku
   // Tady staci postavit jen jednu vyhybku, protoze jeji uzamceni opet zavola
   // tuto udalost.

   Blky.GetBlkByID(Self.fproperties.Vyhybky[Self.fstaveni.nextVyhybka].Blok, Blk);
   Inc(Self.fstaveni.nextVyhybka);
   (Blk as TBlkVyhybka).SetPoloha(TVyhPoloha(Self.fproperties.Vyhybky[Self.fstaveni.nextVyhybka-1].Poloha),
                                  true, false, Self.VyhPrestavenaNC, Self.VyhNeprestavenaNC);
  end else begin
   if (Self.fstaveni.nextVyhybka < Self.fproperties.Vyhybky.Count+Self.fproperties.Odvraty.Count) then
    begin
     // nastaveni odvratu
     // Tady staci postavit jen jednu vyhybku, protoze jeji uzamceni opet zavola
     // tuto udalost.

     odvrat := Self.fstaveni.nextVyhybka - Self.fproperties.Vyhybky.Count;

     Blky.GetBlkByID(Self.fproperties.Odvraty[odvrat].Blok, Blk);
     Inc(Self.fstaveni.nextVyhybka);
     TBlkVyhybka(Blk).SetPoloha(TVyhPoloha(Self.fproperties.Odvraty[odvrat].Poloha),
                                true, false, Self.VyhPrestavenaNC, Self.VyhNeprestavenaNC);
    end;
  end;

 if (Self.fstaveni.nextVyhybka = Self.fproperties.Vyhybky.Count+Self.fproperties.Odvraty.Count) then
   Self.fstaveni.nextVyhybka := -1;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TJC.NavNepostaveno(Sender:TObject);
var nav:TBlk;
begin
 if (not Self.staveni) then Exit();
 Blky.GetBlkByID(Self.fproperties.NavestidloBlok, nav);

 if (Self.fstaveni.SenderPnl <> nil) and (Self.fstaveni.SenderOR <> nil) then
   ORTCPServer.BottomError(Self.fstaveni.SenderPnl, 'Návěstidlo '+nav.name + ' nepostaveno',
     (Self.fstaveni.SenderOR as TOR).ShortName, 'TECHNOLOGIE');
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
  // useky:
  for i := 0 to Self.fproperties.Useky.Count-1 do
   begin
    Blky.GetBlkByID(Self.fproperties.Useky[i], Blk);
    glob := Blk.GetGlobalSettings();

    // disabled
    if ((Blk as TBlkUsek).Obsazeno = TUsekStav.disabled) then
      bariery.Add(Self.JCBariera(_JCB_BLOK_DISABLED, Blk, Blk.id));

    // obsazenost
    if ((i <> Self.fproperties.Useky.Count-1) or (Self.fproperties.TypCesty <> TJCType.posun)) then
     begin
      if ((Blk as TBlkUsek).Obsazeno <> TUsekStav.uvolneno) then
        bariery.Add(Self.JCBariera(_JCB_USEK_OBSAZENO, Blk, Blk.id));
     end;//if

    // souprava
    if (((Blk as TBlkUsek).IsSouprava()) and (Self.fproperties.TypCesty = TJCType.vlak)) then
      bariery.Add(Self.JCBariera(_JCB_USEK_SOUPRAVA, Blk, Blk.id));
   end;//for i

  // kontrola vyhybek:
  for i := 0 to Self.fproperties.Vyhybky.Count-1 do
   begin
    Blky.GetBlkByID(Self.fproperties.Vyhybky[i].Blok, Blk);
    glob := Blk.GetGlobalSettings();

    // kontrola polohy:
    if ((Blk as TBlkVyhybka).poloha <> Self.fproperties.Vyhybky[i].Poloha) then
      bariery.Add(Self.JCBariera(_JCB_VYHYBKA_KONC_POLOHA, Blk, Blk.id));

    // kontrola nouzoveho zaveru:
    if (not (Blk as TBlkVyhybka).vyhZaver) then
      bariery.Add(Self.JCBariera(_JCB_VYHYBKA_NOUZ_ZAVER, Blk, Blk.id));

    // kontrola spojky
    Blky.GetBlkByID((Blk as TBlkVyhybka).GetSettings.spojka, Blk2);
    if ((blk2 <> nil) and ((Blk as TBlkVyhybka).Poloha <> Self.fproperties.Vyhybky[i].Poloha)) then
     begin
      if (not (Blk2 as TBlkVyhybka).vyhZaver) then
        bariery.Add(Self.JCBariera(_JCB_VYHYBKA_NOUZ_ZAVER, Blk2, Blk2.id));

      if ((Blk2 as TBlkVyhybka).Obsazeno = TUsekStav.obsazeno) then
        bariery.Add(Self.JCBariera(_JCB_USEK_OBSAZENO, Blk2, Blk2.id));
     end;

    // kontrola neprofiloveho styku pro polohu +
    if ((Self.fproperties.Vyhybky[i].Poloha = TVyhPoloha.plus) and (TBlkVyhybka(Blk).npBlokPlus <> nil)) then
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
    if ((Self.fproperties.Vyhybky[i].Poloha = TVyhPoloha.minus) and (TBlkVyhybka(Blk).npBlokMinus <> nil)) then
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
  for i := 0 to Self.fproperties.Prejezdy.Count-1 do
   begin
    Blky.GetBlkByID(Self.fproperties.Prejezdy[i].Prejezd, Blk);

    if ((Blk as TBlkPrejezd).Stav.basicStav <> TBlkPrjBasicStav.none) then
     begin
      if ((Blk as TBlkPrejezd).Stav.PC_NOT) then
       begin
        bariery.Add(Self.JCBariera(_JCB_PREJEZD_NOUZOVE_OTEVREN, blk, Self.fproperties.Prejezdy[i].Prejezd));
       end else begin
        if ((Blk as TBlkPrejezd).Stav.basicStav <> TBlkPrjBasicStav.uzavreno) then
          bariery.Add(Self.JCBariera(_JCB_PREJEZD_NEUZAVREN, blk, Self.fproperties.Prejezdy[i].Prejezd));
       end;
     end else begin
      bariery.Add(Self.JCBariera(_JCB_PREJEZD_PORUCHA, blk, Self.fproperties.Prejezdy[i].Prejezd));
     end;//else NouzoveOtevreni
   end;//for i

  // kontrola odvratu
  for i := 0 to Self.fproperties.Odvraty.Count-1 do
   begin
    Blky.GetBlkByID(Self.fproperties.Odvraty[i].Blok, Blk);
    glob := Blk.GetGlobalSettings();

    // kontrola polohy:
    if ((Blk as TBlkVyhybka).poloha <> Self.fproperties.Odvraty[i].Poloha) then
      bariery.Add(Self.JCBariera(_JCB_VYHYBKA_KONC_POLOHA, Blk, Blk.id));

    // kontrola nouzoveho zaveru:
    if (not (Blk as TBlkVyhybka).vyhZaver) then
      bariery.Add(Self.JCBariera(_JCB_VYHYBKA_NOUZ_ZAVER, Blk, Blk.id));

    // kontrola spojky odvratu
    Blky.GetBlkByID((Blk as TBlkVyhybka).GetSettings.spojka, Blk2);
    if (blk2 <> nil) then
     begin
      // kontrola spravneho uzamceni odvratu
      if ((Blk as TBlkVyhybka).Poloha <> Self.fproperties.Odvraty[i].Poloha) then
        if (not (Blk2 as TBlkVyhybka).vyhZaver) then
          bariery.Add(Self.JCBariera(_JCB_VYHYBKA_NOUZ_ZAVER, Blk2, Blk2.id));
     end;
   end;//for i

  if (Self.fproperties.Trat > -1) then
   begin
    if (Self.fproperties.TypCesty = TJCType.vlak) then
     begin
      Blky.GetBlkByID(Self.fproperties.Useky[Self.fproperties.Useky.Count-1], Blk);
      if (not TBlkTU(blk).sectReady) then
       begin
        Blky.GetBlkByID(Self.fproperties.Trat, Blk);
        bariery.Add(Self.JCBariera(_JCB_TRAT_NEPRIPRAVENA, blk, Self.fproperties.Trat));
       end;
     end;

    Blky.GetBlkByID(Self.fproperties.Trat, TBlk(trat));
    glob := trat.GetGlobalSettings();

    if ((trat.ZAK) and (Self.fproperties.TypCesty = TJCType.vlak)) then
      bariery.Add(Self.JCBariera(_JCB_TRAT_ZAK, blk, Self.fproperties.Trat));
    if ((not trat.ZAK) and (Self.fproperties.TypCesty = TJCType.posun)) then
      bariery.Add(Self.JCBariera(_JCB_TRAT_NOT_ZAK, blk, Self.fproperties.Trat));
    if (trat.Zaver) then
      bariery.Add(Self.JCBariera(_JCB_TRAT_ZAVER, blk, Self.fproperties.Trat));
    if (trat.Zadost) then
      bariery.Add(Self.JCBariera(_JCB_TRAT_ZADOST, blk, Self.fproperties.Trat));
    if (Self.fproperties.TratSmer <> trat.Smer) then
      bariery.Add(Self.JCBariera(_JCB_TRAT_NESOUHLAS, blk, Self.fproperties.Trat));
    if ((not trat.BP) and (Self.fproperties.TypCesty = TJCType.vlak)) then
      bariery.Add(Self.JCBariera(_JCB_TRAT_NO_BP, blk, Self.fproperties.Trat));

    usek := (Self.navestidlo as TBlkNav).UsekPred as TBlkUsek;
    Blky.GetBlkByID(Self.data.Useky[Self.data.Useky.Count-1], TBlk(lastUsek));

    if ((usek.IsSouprava) and (lastUsek.typ = _BLK_TU) and ((lastUsek as TBlkTU).InTrat = Self.data.Trat)) then
     begin
      if (trat.vyluka) then
       begin
        if ((trat.stav.soupravy.Count > 0) or ((trat.GetLastUsek(Self.data.TratSmer) as TBlkTU).Zaver <> TZaver.no)) then
          bariery.Add(Self.JCBariera(_JCB_TRAT_NEPRENOS, trat, Self.fproperties.Trat))
        else
          bariery.Add(Self.JCBariera(_JCB_TRAT_PRENOS_NAKONEC, trat, Self.fproperties.Trat));
       end else begin
        if ((lastUsek.IsSouprava()) or (not trat.BP) or (trat.Smer <> Self.data.TratSmer)) then
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
   if (Self.Krok = _NC_KROK_BARIERA_UPDATE) then
     Self.Krok := _NC_KROK_BARIERY_POTVRZENY;
  end else begin
   Self.CancelStaveni();

   // aktualizace stavu navestidla (zobrazeni RNZ)
   Blky.GetBlkByID(Self.fproperties.NavestidloBlok, Blk);
   Blk.Change();

   for i := 0 to Self.fproperties.Useky.Count-1 do
    begin
     Blky.GetBlkByID(Self.fproperties.Useky[i], Blk);
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
 nav_changed := (Self.data.NavestidloBlok <> prop.NavestidloBlok);
 Blky.GetBlkByID(Self.data.NavestidloBlok, orig_nav);
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

function TJC.GetSouprava(nav:TBlk = nil; usek:TBlk = nil): TSouprava;
begin
 if (nav = nil) then
   Blky.GetBlkByID(Self.fproperties.NavestidloBlok, nav);

 Result := TBlkNav(nav).GetSouprava(usek);
end;

////////////////////////////////////////////////////////////////////////////////

function TJC.GetAB():boolean;
var Blk:TBlk;
begin
 Blky.GetBlkByID(Self.fproperties.NavestidloBlok, Blk);
 Result := ((Blk <> nil) and (Blk.typ = _BLK_NAV) and (TBlkNav(Blk).ABJC = Self));
end;

////////////////////////////////////////////////////////////////////////////////

function TJC.IsAnyVyhMinus():boolean;
var vyh:TJCVyhZaver;
begin
 for vyh in Self.fproperties.Vyhybky do
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
  Blky.GetBlkByID(Self.fproperties.NavestidloBlok, Navestidlo);
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

        _JCB_TRAT_ZAK: Exit(Self.fproperties.TypCesty = TJCType.vlak);
      end;
     end;
  finally
    bariery.Free();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TJC.GetNav():TBlk;
begin
 Blky.GetBlkByID(Self.fproperties.NavestidloBlok, Result);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TJC.ClientDisconnect(AContext: TIDContext);
begin
 if (Self.fstaveni.SenderPnl = AContext) then
   Self.fstaveni.SenderPnl := nil;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TJC.SetInitKrok();
begin
 if (Self.fstaveni.nc) then
   Self.Krok := _NC_KROK_INIT
 else
   Self.Krok := _JC_KROK_INIT;
end;

////////////////////////////////////////////////////////////////////////////////

function TJC.GetWaitForLastUsekOrTratObsaz():Boolean;
begin
 Result := (Self.Krok = _JC_KROK_CEKANI_POSLEDNI_USEK);
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
 json['nazev'] := Self.fproperties.Nazev;
 json['id'] := Self.fproperties.id;
 json['navId'] := Self.fproperties.NavestidloBlok;
 case (Self.fproperties.TypCesty) of
  TJCType.vlak: json['typ'] := 'VC';
  TJCType.posun: json['typ'] := 'PC';
 end;
 case (Self.fproperties.DalsiNavaznost) of
  TJCNextNavType.zadna: json['dalsiNav'] := '-';
  TJCNextNavType.trat: json['dalsiNav'] := 'trat';
  TJCNextNavType.blok: begin
    json['dalsiNav'] := 'blok';
    json['dalsiNavId'] := Self.fproperties.DalsiNavestidlo;
  end;
 end;

 for vyhZaver in Self.fproperties.Vyhybky do
  begin
   newObj := json.A['vyhybky'].AddObject();
   newObj['blok'] := vyhZaver.Blok;
   case (vyhZaver.Poloha) of
    TVyhPoloha.plus: newObj['poloha'] := '+';
    TVyhPoloha.minus: newObj['poloha'] := '-';
   end;
  end;

 for usek in Self.fproperties.Useky do
   json.A['useky'].Add(usek);

 for odvratZaver in Self.fproperties.Odvraty do
  begin
   newObj := json.A['odvraty'].AddObject();
   newObj['blok'] := odvratZaver.Blok;
   case (odvratZaver.Poloha) of
    TVyhPoloha.plus: newObj['poloha'] := '+';
    TVyhPoloha.minus: newObj['poloha'] := '-';
   end;
   newObj['refBlk'] := odvratZaver.ref_blk;
  end;

 for prjZaver in Self.fproperties.Prejezdy do
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

 json['rychlostDalsiN'] := Self.fproperties.RychlostDalsiN;
 json['rychlostNoDalsiN'] := Self.fproperties.RychlostNoDalsiN;
 json['odbocka'] := Self.fproperties.odbocka;

 if (includeStaveni) then
   Self.GetPtStaveni(json['staveni']);
end;

procedure TJC.GetPtStaveni(json:TJsonObject);
begin
 json['staveni'] := Self.staveni;
 json['postaveno'] := Self.postaveno;
 json['krok'] := Self.fstaveni.Krok;
 json['rozpadBlok'] := Self.fstaveni.RozpadBlok;
 json['rozpadRuseniBlok'] := Self.fstaveni.RozpadRuseniBlok;
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

end.//unit
