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
  Dialogs, Menus, Buttons, ComCtrls, fMain, BlockDb, Block, IbUtils, Train,
  IniFiles, IdContext, BlockRailway, Generics.Collections, UPO, BlockTurnout,
  Area, changeEvent, changeEventCaller, JsonDataObjects, PTUtils;

const
  _JC_INITPOTVR_TIMEOUT_SEC = 60;                                               // timeout UPO a potvrzeni na zacatku staveni JC
  _JC_TIMEOUT_SEC = 30;                                                         // timeout pro staveni jizdni cesty (vlakove i posunove v sekundach)
  _JC_PRJ_TIMEOUT_SEC = 50;                                                     // timeout pri staveni JC pro zavirani prejezdu v ceste
  _NC_TIMEOUT_MIN = 1;                                                          // timeout pro staveni nouzove cesty (vlakove i posunove) v minutach
  _JC_MAX_VYH_STAVENI = 4;                                                      // kolik vyhybek se muze stavit zaroven v JC

  _STEP_DEFAULT = 0;
  _STEP_KRIT_BARIERY = 1;
  _STEP_POTVR_BARIERY = 5;
  _STEP_POTVR_SEKV = 6;

  _JC_STEP_INIT = 10;
  _JC_STEP_CEKANI_VYHYBKA_POLOHA = 11;
  _JC_STEP_ZAVRIT_PREJEZDY = 12;
  _JC_STEP_CEKANI_PREJEZDY = 13;
  _JC_STEP_FINALNI_ZAVER = 14;
  _JC_STEP_CEKANI_NAVESTIDLO = 15;
  _JC_STEP_FINISH = 16;
  _JC_STEP_CEKANI_POSLEDNI_USEK = 20;

  _NC_STEP_INIT = 100;
  _NC_STEP_BARIERA_UPDATE = 101;
  _NC_STEP_BARIERY_POTVRZENY = 102;
  _NC_STEP_CEKANI_NAVESTIDLO = 103;
  _NC_STEP_FINISH = 104;

  _JC_DESTROY_NC = -6;
  _JC_DESTROY_NONE = -5;
  _JC_DESTROY_SIGNAL_STUJ = -2;
  _JC_DESTROY_SIGNAL_TRACK = -1;

type
  TJCType = (train = 1, shunt = 2, emergency = 3);
  TJCNextSignalType = (no = 0, railway = 1, signal = 2);

  // jedna bariera ve staveni jizdni cesty:
  TJCBarrier = record
   typ: Integer;  // typ bariery, odkazuje na konstanty _JCB_*, viz nize
   block: TBlk;   // blok, na ktery se bariera vztahuje; nektere bariery nemusi byt prirazeny bloku, platnost tohoto parametru je potreba overit pro kazdou barieru samostatne
   param: Integer;  // parametr bariery, typicky napr. ID bloku, ktery neexistuje; v takovem pripade samozrejme nemuze existovat \blok
  end;
  TJCBarriers = TList<TJCBarrier>;

  // zaver vyhybky v jizdni ceste
  TJCTurnoutZav = record
   block: Integer;
   position: TTurnoutPosition;
  end;

  // zaver odvratove vyhybky v jizdni ceste
  TJCRefugeeZav = record
   block: Integer;
   position: TTurnoutPosition;
   ref_blk: Integer;  // blok, pri jehoz zruseni redukce (typicky usek a uvolneni zaveru) dojde i k uvolneni zaveru odvratove vyhybky
  end;

  // bloky v JC, ketre jsou navazany na konkretni useky v ramci JC
  TJCRefZav = record
   block: Integer;
   ref_blk: Integer;  // blok, pri jehoz uvolneni zaveru dojde ke zruseni redukce \Blok
  end;

  // prejezd v jizdni ceste
  TJCCrossingZav = record
   crossingId: Integer;
   closeTracks: TList<Integer>;  // pokud se prejezd nezavira, je seznam prazdny
   openTrack: Integer;   // pokud se prejezd nezavira, je nedefinovany
  end;

  ///////////////////////////////////////////////////////////////////////////

  TJCdata = record
   name: string;
   id: Integer;
   typ: TJCType;

   signalId: Integer;
   nextSignalType: TJCNextSignalType;
   nextSignalId: Integer;

   turnouts: TList<TJCTurnoutZav>;
   tracks: TList<Integer>;
   refuges: TList<TJCRefugeeZav>;  // odvraty
   crossings: TList<TJCCrossingZav>;
   locks: TList<TJCRefZav>;
   vb: TList<Integer>;  // seznam variantnich bodu JC - obashuje postupne ID bloku typu usek

   railwayId: Integer;
   railwayDir: TRailwayDirection;
   speedGo, speedStop: Integer;  // rychlost v JC pri dalsim navestidle navestici dovolujici a NEdovolujici navest
   turn: Boolean;  // jc od odbocky (40 km/h)
   nzv: Boolean;   // nedostatecna zabrzdna vzdalenost
   signalFallTrackI: Cardinal;
  end;

  // staveni jizdni cesty:
  //    staveni jizdni cesty probiha krokove, viz \UpdateStaveni
  TJCstate = record
   step: Integer;                                                               // aktualni krok staveni jizdni cesty
   timeOut: TDateTime;                                                          // cas, pri jehoz prekroceni dojde k timeoutu JC
   senderOR: TObject;                                                           // oblast rizeni, ktera vyvolala staveni JC, do teto OR jsou typicky odesilany notifikacni a chybove hlasky (napr. upozorneni vlevo dole panelu, potvrzovaci sekvence)
   senderPnl: TIdContext;                                                       // konkretni panel, kery vyvolal staveni JC
   destroyBlock,                                                                // index useku, na ktery ma vkrocit souprava
   destroyEndBlock: Integer;                                                    // index useku, ze ktereho ma vystoupit souprava
                                                                                  // index je index v seznamu useku, tedy napr. 0 =  0. usek v jizdni ceste; + specialni hodnoty _JC_DESTROY*
   from_stack: TObject;                                                         // odkaz na zasobnik, ze ktereho proehlo staveni JC
   nc: Boolean;                                                                 // flag staveni nouzove cesty (vlakovou i posunovou)
   ncBariery: TJCBarriers;                                                       // aktualni seznam barier pro potvrzovaci sekvenci pri staveni nouzove cesty
   ncBarieryCntLast: Integer;                                                   // posledni pocet barier ve staveni nouzove cesty
   nextTurnout: Integer;                                                        // vyhybka, ktera se ma stavit jako dalsi
                                                                                // po postaveni vsechn vyhybek plynule prechazi do indexu seznamu odvratu
   ab: Boolean;                                                                 // po postaveni JC automaticky zavest AB
   crossingWasClosed: Boolean;                                                  // jiz byl vydan povel k zavreni prejezdu
   lastTrackOrRailwayOccupied: Boolean;                                         // je obsazen posledni usek JC, nestavit navestidlo a nezavirat prejezdy
  end;

  ENavChanged = procedure(Sender: TObject; origNav: TBlk) of object;

  ///////////////////////////////////////////////////////////////////////////

  TJC = class
   public const

    // bariery ve staveni jizdni cesty:

    _JCB_OK                      = 0;
    _JCB_STAVENI                 = 1;
    _JCB_BLOK_DISABLED           = 2;
    _JCB_BLOK_NOT_EXIST          = 3;
    _JCB_BLOK_NOT_TYP            = 4;
    _JCB_PRIVOLAVACKA            = 5;

    _JCB_NAV_NOT_USEK            = 10;
    _JCB_NAV_NAVEST              = 11;

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
    _def_jc_staveni : TJCstate = (
      step : _STEP_DEFAULT;
      destroyBlock : _JC_DESTROY_NONE;
      destroyEndBlock : _JC_DESTROY_NONE;
      ab: false;
      crossingWasClosed: false;
    );

   private
     m_data: TJCdata;
     m_state: TJCstate;
     fOnIdChanged: TNotifyEvent;
     fOnSignalChanged: ENavChanged;

      procedure SetInitStep();
      procedure SetData(prop: TJCdata);

      procedure CancelSignalBegin();
      procedure CancelTrackEnd();
      procedure CancelVBs();
      procedure MoveTrainToNextTrack();
      procedure CheckLoopBlock(blk: TBlk); // kontroluje zmenu smeru soupravy a hnacich vozidel pri vkroceni do smyckove bloku,
                                           // tato kontrola probiha pouze pri vkroceni do posledniho bloku JC
      function IsActivating(): Boolean;
      function IsActive(): Boolean;

      procedure PS_vylCallback(Sender: TIdContext; success: Boolean);           // callback potvrzovaci sekvence na vyluku
      procedure UPO_OKCallback(Sender: TObject);                                // callback potvrzeni upozorneni
      procedure UPO_EscCallback(Sender: TObject);                               // callback zamitnuti upozorneni
      procedure NC_PS_Callback(Sender: TIdContext; success: Boolean);           // callback potvrzovaci sekvence nouzove cesty

      procedure TrackCloseCrossing(Sender: TObject; data: integer);  // zavre prejezd pri vkroceni na dany usek, odkaz na tuto metodu je posilan usekum, ktere ji pri obsazeni vyvolaji

      procedure SetDestroyBlock(destroyBlock: Integer);
      procedure SetDestroyEndBlock(destroyEndBlock: Integer);
      procedure SetStep(step: Integer);
      procedure CritBarieraEsc(Sender: TObject);

      // callbacky ne/nastaveni polohy vyhybek:
      procedure TurnoutErrJCPC(Sender: TObject; error: TTurnoutSetError);
      procedure TurnoutErrNC(Sender: TObject; error: TTurnoutSetError);
      procedure TurnoutMovedNC(Sender: TObject);
      procedure TurnoutMovedJCPC(Sender: TObject);
      procedure SignalError(Sender: TObject);

      procedure BarriersVCPC(var barriers: TList<TJCBarrier>);
      procedure BarriersNC(var barriers: TList<TJCBarrier>);
      procedure BarriersNCToAccept(var bariery: TList<TJCBarrier>);

      function BarriersToPotvrSekv(barriers: TJCBarriers): TConfSeqItems; // seznam barier nouzve cesty prevede na potvrzovaci sekvence pro klienta

      function GetTrain(nav: TBlk = nil; usek: TBlk = nil): TTrain; // vraci cislo soupravy na useku pred navestidlem

      function GetAB(): Boolean;
      function IsCriticalBarrier(): Boolean;
      function GetSignal(): TBlk;
      function GetWaitFroLastTrackOrRailwayOccupied(): Boolean;
      function GetLastTrack(): TBlk;

      procedure BarrierToJson(const bariera: TJCBarrier; result: TJsonObject);
      procedure Log(msg: string; typ: Integer = WR_VC);

   public

     index: Integer;                                                             // index v tabulce jizdni cest ve F_Main
     changed: Boolean;                                                           // JC zmenana -> akualizuje se v tabulce ve F_Main

      class function JCBarrier(typ: Integer; block: TBlk = nil; param: Integer = 0): TJCBarrier;
                                                                                // jednoduche genreovani berier jako navratove funkce teto funkce
      function JCBarrierToMessage(Bariera: TJCBarrier): TUPOItem;               // prevod bariery na spravu upozorneni vlevo dole
      class function CriticalBarrier(typ: Integer): Boolean;                    // je bariera kriticka?
      class function PotvrSekvBarrier(typ: Integer): Boolean;                   // je bariera hodna potvrzovaci sekvence?
      function WarningBarrier(typ: Integer): Boolean;                           // je bariera hodna zobrazeni upozorneni?

      class function PotvrSekvBarrierToReason(typ: Integer): string;

      constructor Create(); overload;
      constructor Create(data: TJCdata); overload;
      destructor Destroy(); override;

      procedure SetSignalSignal();
      procedure Cancel(Sender: TObject = nil);
      procedure CancelWithoutTrackRelease();
      procedure DynamicCanceling();  // kontroluje projizdeni soupravy useky a rusi jejich zavery
      procedure DynamicCancelingNC();  // rusi poruchu BP trati, ze ktere odjizdi souprava v ramci nouzove jizdni cesty
      procedure NonProfileOccupied();  // volano pri obsazeni kontrolvoaneho neprofiloveho useku

      procedure UpdateActivating();
      procedure UpdateTimeOut();
      procedure CancelActivating(reason: string = ''; stack_remove: Boolean = false); // zrusi staveni a oduvodneni zaloguje a zobrazi dispecerovi

      procedure LoadData(ini: TMemIniFile; section: string);
      procedure SaveData(ini: TMemIniFile; section: string);

      function Activate(SenderPnl: TIdContext; SenderOR: TObject; bariery_out: TJCBarriers;
          from_stack: TObject = nil; nc: Boolean = false; fromAB: Boolean = false;
          abAfter: Boolean = false): Integer; overload;
      function Activate(SenderPnl: TIdContext; SenderOR: TObject;
          from_stack: TObject = nil; nc: Boolean = false; fromAB: Boolean = false;
          abAfter: Boolean = false): Integer; overload;


      function CanDN(): Boolean;                                                 // true = je mozno DN; tato funkce kontroluje, jestli je mozne znovupostavit cestu i kdyz byla fakticky zrusena = musi zkontrolovat vsechny podminky
      procedure DN();                                                           // DN nastavi zavery vsech bloku na validni a rozsviti navestidlo
      procedure STUJ();

      function Barriers(NC: Boolean = false): TJCBarriers;
      function IsAnyTurnoutMinus(): Boolean;
      procedure ClientDisconnect(AContext: TIDContext);

      procedure GetPtData(json: TJsonObject; includeStaveni: Boolean);
      procedure GetPtState(json: TJsonObject);
      procedure PostPtStav(reqJson: TJsonObject; respJson: TJsonObject);

      property data: TJCdata read m_data write SetData;
      property state: TJCstate read m_state;

      property name: String read m_data.name;
      property id: Integer read m_data.id write m_data.id;
      property typ: TJCType read m_data.typ;

      property activating: Boolean read IsActivating;
      property active: Boolean read IsActive;                             // true pokud je postavena navest
      property AB: Boolean read GetAB;
      property waitForLastTrackOrRailwayOccupy: Boolean read GetWaitFroLastTrackOrRailwayOccupied;
      property lastTrack: TBlk read GetLastTrack;

      property destroyBlock: Integer read m_state.destroyBlock write SetDestroyBlock;
      property destroyEndBlock: Integer read m_state.destroyEndBlock write SetDestroyEndBlock;
      property step: Integer read m_state.step write SetStep;
      property signal: TBlk read GetSignal;

      property OnIdChanged: TNotifyEvent read fOnIdChanged write fOnIdChanged;
      property OnSignalChanged: ENavChanged read fOnSignalChanged write fOnSignalChanged;
  end;

implementation

uses GetSystems, TechnologieRCS, THnaciVozidlo, BlockSignal, BlockTrack, AreaDb,
     BlockCrossing, TJCDatabase, TCPServerPanel, TrainDb, timeHelper,
     THVDatabase, AreaStack, BlockLinker, BlockLock, BlockRailwayTrack;

////////////////////////////////////////////////////////////////////////////////

constructor TJC.Create();
begin
 inherited;

 Self.m_data.id := -1;
 Self.changed  := true;
 Self.m_state := _def_jc_staveni;
 Self.m_state.ncBariery := TList<TJCBarrier>.Create();

 Self.m_data.locks := TList<TJCRefZav>.Create();
 Self.m_data.vb := TList<Integer>.Create();

 Self.m_data.turnouts := TList<TJCTurnoutZav>.Create();
 Self.m_data.tracks := TList<Integer>.Create();
 Self.m_data.refuges := TList<TJCRefugeeZav>.Create();
 Self.m_data.crossings := TList<TJCCrossingZav>.Create();
end;

constructor TJC.Create(data: TJCdata);
begin
 inherited Create();

 Self.m_data := data;
 Self.m_state := _def_jc_staveni;
 if (not Assigned(Self.m_state.ncBariery)) then Self.m_state.ncBariery := TList<TJCBarrier>.Create();

 if (not Assigned(Self.m_data.locks)) then Self.m_data.locks := TList<TJCRefZav>.Create();
 if (not Assigned(Self.m_data.vb)) then Self.m_data.vb := TList<Integer>.Create();
 if (not Assigned(Self.m_data.refuges)) then Self.m_data.refuges := TList<TJCRefugeeZav>.Create();
 if (not Assigned(Self.m_data.crossings)) then Self.m_data.crossings := TList<TJCCrossingZav>.Create();
 if (not Assigned(Self.m_data.turnouts)) then Self.m_data.turnouts := TList<TJCTurnoutZav>.Create();
 if (not Assigned(Self.m_data.tracks)) then Self.m_data.tracks := TList<Integer>.Create();
end;

destructor TJC.Destroy();
var i: Integer;
begin
 if (Assigned(Self.m_state.ncBariery)) then FreeAndNil(Self.m_state.ncBariery);
 if (Assigned(Self.m_data.locks)) then FreeAndNil(Self.m_data.locks);
 if (Assigned(Self.m_data.vb)) then  Self.m_data.vb.Free();

 if (Assigned(Self.m_data.turnouts))  then Self.m_data.turnouts.Free();
 if (Assigned(Self.m_data.tracks))    then Self.m_data.tracks.Free();
 if (Assigned(Self.m_data.refuges))  then Self.m_data.refuges.Free();
 for i := 0 to Self.m_data.crossings.Count-1 do Self.m_data.crossings[i].closeTracks.Free();
 if (Assigned(Self.m_data.crossings)) then Self.m_data.crossings.Free();

 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

// kontroluje podminky pro staveni konkretni jizdni cesty
// vraci List prblemu (tzv. bariery), ktere definuji to, proc jizdni cestu nelze postavit (tedy vraci vsechny nesplnene podminky)
// tzv. kriticke bariery jsou vzdy na zacatu Listu
function TJC.Barriers(NC: Boolean = false): TJCBarriers;
var i: Integer;
    Blk, blk2: TBlk;
    privol: TBlksList;
    turnoutZav: TJCTurnoutZav;
    trackZav: Integer;
    refZaver: TJCRefZav;
    signalZav: TJCCrossingZav;
    refugeeZav: TJCRefugeeZav;
begin
  Result := TList<TJCBarrier>.Create();

  if (Self.activating) then
    Result.Add(Self.JCBarrier(_JCB_STAVENI));

  // kontrola useku navestidla:
  if (Blocks.GetBlkByID(Self.m_data.signalId, Blk) <> 0) then
   begin
    // blok navestidla neexistuje
    Result.Add(Self.JCBarrier(_JCB_BLOK_NOT_EXIST, nil, Self.m_data.signalId));
    Exit();
   end;

  if (Self.lastTrack = nil) then
   begin
    // neexistuje ani jeden usek nebo je posledni usek nevalidni
    Result.Add(Self.JCBarrier(_JCB_BLOK_NOT_EXIST, nil, 0));
    Exit();
   end;

  if (Blk.typ <> btSignal) then
   begin
    // blok navestidla neni typu navestidlo
    Result.Add(Self.JCBarrier(_JCB_BLOK_NOT_TYP, Blk, Self.m_data.signalId));
    Exit();
   end;

  if ((Blk as TBlkSignal).track = nil) then
   begin
    // blok navestidla pred sebou nema zadny usek
    Result.Add(Self.JCBarrier(_JCB_NAV_NOT_USEK, Blk, Self.m_data.signalId));
    Exit();
   end;

  // vyhybky:
  // kontrolujeme, jestli vyhybky existuji a jestli jsou to vyhybky
  for turnoutZav in Self.m_data.turnouts do
   begin
    if (Blocks.GetBlkByID(turnoutZav.block, Blk) <> 0) then
     begin
      Result.Add(Self.JCBarrier(_JCB_BLOK_NOT_EXIST, nil, turnoutZav.block));
      Exit();
     end;

    if (Blk.typ <> btTurnout) then
     begin
      Result.Add(Self.JCBarrier(_JCB_BLOK_NOT_TYP, Blk, turnoutZav.block));
      Exit();
     end;
   end;

  // useky:
  for trackZav in Self.m_data.tracks do
   begin
    // zkontrolujeme, jestli useky existuji a jestli jsou to useky
    if (Blocks.GetBlkByID(trackZav, Blk) <> 0) then
     begin
      Result.Add(Self.JCBarrier(_JCB_BLOK_NOT_EXIST, nil, trackZav));
      Exit();
     end;

    if ((Blk.typ <> btTrack) and (Blk.typ <> btRT)) then
     begin
      Result.Add(Self.JCBarrier(_JCB_BLOK_NOT_TYP, Blk, trackZav));
      Exit();
     end;
   end;

  // kontrola prejezdu
  for signalZav in Self.m_data.crossings do
   begin
    // kontrola existence bloku prejezdu
    if (Blocks.GetBlkByID(signalZav.crossingId, blk) <> 0) then
     begin
      Result.Insert(0, Self.JCBarrier(_JCB_BLOK_NOT_EXIST, nil, signalZav.crossingId));
      Exit();
     end;

    // kontrola typu bloku prejezdu
    if (blk.typ <> btCrossing) then
     begin
      Result.Insert(0, Self.JCBarrier(_JCB_BLOK_NOT_TYP, blk, signalZav.crossingId));
      Exit();
     end;

    // pokud se ma prejezd zavirat
    if (signalZav.closeTracks.Count > 0) then
     begin
      // kontrola existence oteviraciho bloku
      if (Blocks.GetBlkByID(signalZav.openTrack, blk2) <> 0) then
       begin
        Result.Insert(0, Self.JCBarrier(_JCB_BLOK_NOT_EXIST, blk, signalZav.openTrack));
        Exit();
       end;

      // kontrola typu oteviraciho bloku
      if ((blk2.typ <> btTrack) and (blk2.typ <> btRT)) then
       begin
        Result.Insert(0, Self.JCBarrier(_JCB_BLOK_NOT_TYP, blk, signalZav.openTrack));
        Exit();
       end;

      // kontrola existence uzaviracich bloku a jejich typu
      for trackZav in signalZav.closeTracks do
       begin
        if (Blocks.GetBlkByID(trackZav, blk2) <> 0) then
         begin
          Result.Insert(0, Self.JCBarrier(_JCB_BLOK_NOT_EXIST, blk, trackZav));
          Exit();
         end;
        if ((blk2.typ <> btTrack) and (blk2.typ <> btRT)) then
         begin
          Result.Insert(0, Self.JCBarrier(_JCB_BLOK_NOT_TYP, blk, trackZav));
          Exit();
         end;
       end;
     end;
   end;

  // kontrola odvratu
  for refugeeZav in Self.m_data.refuges do
   begin
    if (Blocks.GetBlkByID(refugeeZav.ref_blk, blk) <> 0) then
     begin
      Result.Insert(0, Self.JCBarrier(_JCB_BLOK_NOT_EXIST, nil, refugeeZav.ref_blk));
      Exit();
     end;
    if ((blk.typ <> btTrack) and (blk.typ <> btRT)) then
     begin
      Result.Insert(0, Self.JCBarrier(_JCB_BLOK_NOT_TYP, blk, refugeeZav.ref_blk));
      Exit();
     end;
    if (Blocks.GetBlkByID(refugeeZav.block, blk) <> 0) then
     begin
      Result.Insert(0, Self.JCBarrier(_JCB_BLOK_NOT_EXIST, nil, refugeeZav.block));
      Exit();
     end;
    if (blk.typ <> btTurnout) then
     begin
      Result.Insert(0, Self.JCBarrier(_JCB_BLOK_NOT_TYP, blk, refugeeZav.block));
      Exit();
     end;
   end;

  // trat
  if (Self.m_data.railwayId > -1) then
   begin
    if (Self.lastTrack.typ <> btRT) then
     begin
      Result.Add(Self.JCBarrier(_JCB_BLOK_NOT_TYP, Self.lastTrack, Self.lastTrack.id));
      Exit();
     end;
    if (Blocks.GetBlkByID(Self.m_data.railwayId, blk) <> 0) then
     begin
      Result.Insert(0, Self.JCBarrier(_JCB_BLOK_NOT_EXIST, nil, Self.m_data.railwayId));
      Exit();
     end;
    if (blk.typ <> btRailway) then
     begin
      Result.Insert(0, Self.JCBarrier(_JCB_BLOK_NOT_TYP, blk, Self.m_data.railwayId));
      Exit();
     end;
   end;

  // kontrola podminkovych bloku zamku
  for refZaver in Self.m_data.locks do
   begin
    if (Blocks.GetBlkByID(refZaver.block, blk) <> 0) then
     begin
      Result.Insert(0, Self.JCBarrier(_JCB_BLOK_NOT_EXIST, nil, refZaver.block));
      Exit();
     end;
    if (blk.typ <> btLock) then
     begin
      Result.Insert(0, Self.JCBarrier(_JCB_BLOK_NOT_TYP, blk, blk.id));
      Exit();
     end;
    if (Blocks.GetBlkByID(refZaver.ref_blk, blk) <> 0) then
     begin
      Result.Insert(0, Self.JCBarrier(_JCB_BLOK_NOT_EXIST, nil, refZaver.ref_blk));
      Exit();
     end;
    if ((blk.typ <> btTrack) and (blk.typ <> btRT)) then
     begin
      Result.Insert(0, Self.JCBarrier(_JCB_BLOK_NOT_TYP, blk, blk.id));
      Exit();
     end;
   end;//for i

 if (NC) then
  Self.BarriersNC(Result)
 else
  Self.BarriersVCPC(Result);

 // kontrola zaplych privolavacich navesti
 privol := Blocks.GetNavPrivol(Self.m_state.senderOR as TArea);

 for i := 0 to privol.Count-1 do
   Result.Add(Self.JCBarrier(_JCB_PRIVOLAVACKA, privol[i] as TBlk, (privol[i] as TBlk).id));

 if (Assigned(privol)) then privol.Free();
end;

////////////////////////////////////////////////////////////////////////////////
// kontrola podminek vlakove a posunove cesty

procedure TJC.BarriersVCPC(var barriers: TList<TJCBarrier>);
var i, track, cnt, addr: Integer;
    Blk, blk2: TBlk;
    glob: TBlkSettings;
    flag, cont: Boolean;
    train: TTrain;
    crossingZav: TJCCrossingZav;
    turnoutZav: TJCTurnoutZav;
    refugeeZav: TJCRefugeeZav;
    refZaver: TJCRefZav;
begin
  // navestidlo
  Blocks.GetBlkByID(Self.m_data.signalId, Blk);
  if (not (Blk as TBlkSignal).enabled) then
    barriers.Add(Self.JCBarrier(_JCB_BLOK_DISABLED, Blk, Blk.id));

  if (TBlkSignal(Blk).signal <> ncStuj) then
    barriers.Add(Self.JCBarrier(_JCB_NAV_NAVEST, Blk, Blk.id));

  // kontrola useku:
  if (Self.m_data.railwayId > -1) then
    cnt := Self.m_data.tracks.Count-1
  else
    cnt := Self.m_data.tracks.Count;

  for i := 0 to cnt-1 do
   begin
    Blocks.GetBlkByID(Self.m_data.tracks[i], Blk);
    glob := Blk.GetGlobalSettings();

    if ((Blk as TBlkTrack).occupied = TTrackState.disabled) then
      barriers.Add(Self.JCBarrier(_JCB_BLOK_DISABLED, Blk, Blk.id));

    // obsazenost
    if ((i <> Self.m_data.tracks.Count-1) or (Self.typ <> TJCType.shunt)) then
     begin
      // kontrola disabled jiz probehla
      if ((Blk as TBlkTrack).occupied <> TTrackState.free) then
       begin
        if ((i = Self.m_data.tracks.Count-1) and (Self.m_data.tracks.Count > 1)) then
          barriers.Add(Self.JCBarrier(_JCB_USEK_LAST_OBSAZENO, Blk, Blk.id))
        else
          barriers.Add(Self.JCBarrier(_JCB_USEK_OBSAZENO, Blk, Blk.id));
       end else begin
        // souprava
        if ((Blk as TBlkTrack).IsTrain()) then
          barriers.Add(Self.JCBarrier(_JCB_USEK_SOUPRAVA, Blk, Blk.id));
       end;
     end;//if

    // zaver
    if ((Blk as TBlkTrack).Zaver <> TZaver.no) then
     begin
      if ((Blk as TBlkTrack).Zaver = TZaver.ab) then
        barriers.Add(Self.JCBarrier(_JCB_USEK_AB, Blk, Blk.id))
      else
        barriers.Add(Self.JCBarrier(_JCB_USEK_ZAVER, Blk, Blk.id));
     end;

    // vyluka
    if ((Blk as TBlkTrack).lockout <> '') then
      barriers.Add(Self.JCBarrier(_JCB_USEK_VYLUKA, blk, blk.id));

    // stitek
    if ((Blk as TBlkTrack).note <> '') then
      barriers.Add(Self.JCBarrier(_JCB_USEK_STITEK, blk, blk.id));
   end;//for i

  // kontrola vyhybek:
  for turnoutZav in Self.m_data.turnouts do
   begin
    Blocks.GetBlkByID(turnoutZav.block, Blk);
    glob := Blk.GetGlobalSettings();

    if (TBlkTurnout(Blk).position = TTurnoutPosition.disabled) then
      barriers.Add(Self.JCBarrier(_JCB_BLOK_DISABLED, Blk, Blk.id));

    // kontrola neprofilovych useku vyhybek pro polohu +
    if ((turnoutZav.position = TTurnoutPosition.plus) and (TBlkTurnout(Blk).npBlokPlus <> nil) and
        (TBlkTrack(TBlkTurnout(Blk).npBlokPlus).occupied = TTrackState.disabled)) then
      barriers.Add(Self.JCBarrier(_JCB_BLOK_DISABLED, TBlkTurnout(Blk).npBlokPlus,
          TBlkTurnout(Blk).npBlokPlus.id));

    // kontrola neprofilovych useku vyhybek pro polohu -
    if ((turnoutZav.position = TTurnoutPosition.minus) and (TBlkTurnout(Blk).npBlokMinus <> nil) and
        (TBlkTrack(TBlkTurnout(Blk).npBlokMinus).occupied = TTrackState.disabled)) then
      barriers.Add(Self.JCBarrier(_JCB_BLOK_DISABLED, TBlkTurnout(Blk).npBlokMinus,
          TBlkTurnout(Blk).npBlokMinus.id));

    // kontrola koncove polohy:
    if ((TBlkTurnout(Blk).position = TTurnoutPosition.none) or (TBlkTurnout(Blk).position = TTurnoutPosition.both)) then
      barriers.Add(Self.JCBarrier(_JCB_VYHYBKA_KONC_POLOHA, Blk, Blk.id));

    // zaver nema smysl kontrolovat - zaver vyhybek je prakticky zaver useku
    // proto ho staci zkontrolovat jen u useku

    // kontrola vyluky vyhybky:
    if (TBlkTurnout(Blk).lockout <> '') then
      barriers.Add(Self.JCBarrier(_JCB_VYHYBKA_VYLUKA, Blk, Blk.id));

    // kontrola stitku vyhybky:
    if (TBlkTurnout(Blk).note <> '') then
      barriers.Add(Self.JCBarrier(_JCB_VYHYBKA_STITEK, Blk, Blk.id));

    // kontrola nouzoveho zaveru a redukce menu:
    if (TBlkTurnout(Blk).position <> turnoutZav.position) then
     begin
      if (TBlkTurnout(Blk).emLock) then
        barriers.Add(Self.JCBarrier(_JCB_VYHYBKA_NOUZ_ZAVER, Blk, Blk.id))
      else if (TBlkTurnout(Blk).outputLocked) then
        barriers.Add(Self.JCBarrier(_JCB_VYHYBKA_ZAMCENA, Blk, Blk.id));
     end;

    // kontrola spojky
    Blocks.GetBlkByID(TBlkTurnout(Blk).GetSettings.coupling, Blk2);
    // pokud nemam ja polohu, predpokladam, ze spojka bude muset byt prestavena -> musi byt volna, bez zaveru, ...
    // kontrolovat zaver z useku neni potreba - pokud je problem se zaverem, vyvstane uz na useku JC, jinak je vyhybka v poloze, ktere zaver nevadi
    if ((blk2 <> nil) and (TBlkTurnout(Blk).position <> turnoutZav.position)) then
     begin
      if (TBlkTurnout(Blk2).emLock) then
        barriers.Add(Self.JCBarrier(_JCB_VYHYBKA_NOUZ_ZAVER, Blk2, Blk2.id))
      else if (TBlkTurnout(Blk2).outputLocked) then
        barriers.Add(Self.JCBarrier(_JCB_VYHYBKA_ZAMCENA, Blk2, Blk.id));

      if (TBlkTurnout(Blk2).occupied = TTrackState.occupied) then
        barriers.Add(Self.JCBarrier(_JCB_USEK_OBSAZENO, Blk2, Blk2.id));
     end;

    // kontrola neprofiloveho styku pro polohu +
    if ((turnoutZav.position = TTurnoutPosition.plus) and (TBlkTurnout(Blk).npBlokPlus <> nil) and
        (TBlkTrack(TBlkTurnout(Blk).npBlokPlus).occupied <> TTrackState.free)) then
      barriers.Add(Self.JCBarrier(_JCB_USEK_OBSAZENO, TBlkTurnout(Blk).npBlokPlus,
          TBlkTurnout(Blk).npBlokPlus.id));

    // kontrola neprofiloveho styku pro polohu -
    if ((turnoutZav.position = TTurnoutPosition.minus) and (TBlkTurnout(Blk).npBlokMinus <> nil) and
        (TBlkTrack(TBlkTurnout(Blk).npBlokMinus).occupied <> TTrackState.free)) then
      barriers.Add(Self.JCBarrier(_JCB_USEK_OBSAZENO, TBlkTurnout(Blk).npBlokMinus,
          TBlkTurnout(Blk).npBlokMinus.id));
   end;//for i

  // kontrola prejezdu
  for crossingZav in Self.m_data.crossings do
   begin
    Blocks.GetBlkByID(crossingZav.crossingId, Blk);

    // blok disabled
    if ((Blk as TBlkCrossing).state = TBlkCrossingBasicState.disabled) then
      barriers.Add(Self.JCBarrier(_JCB_BLOK_DISABLED, Blk, Blk.id));

    if ((Blk as TBlkCrossing).state <> TBlkCrossingBasicState.none) then
     begin
      if ((Blk as TBlkCrossing).pcEmOpen) then
        barriers.Add(Self.JCBarrier(_JCB_PREJEZD_NOUZOVE_OTEVREN, blk, crossingZav.crossingId));
     end else
      barriers.Add(Self.JCBarrier(_JCB_PREJEZD_PORUCHA, blk, crossingZav.crossingId));

    // kontrola stitku prejezdu:
    if ((Blk as TBlkCrossing).note <> '') then
      barriers.Add(Self.JCBarrier(_JCB_PREJEZD_STITEK, Blk, Blk.id));
   end;//for i

  // kontrola odvratu
  for refugeeZav in Self.m_data.refuges do
   begin
    Blocks.GetBlkByID(refugeeZav.block, Blk);
    glob := Blk.GetGlobalSettings();

    if (TBlkTurnout(Blk).position = TTurnoutPosition.disabled) then
      barriers.Add(Self.JCBarrier(_JCB_BLOK_DISABLED, Blk, Blk.id));

    // kontrola koncove polohy:
    if ((TBlkTurnout(Blk).position = TTurnoutPosition.none) or (TBlkTurnout(Blk).position = TTurnoutPosition.both)) then
      barriers.Add(Self.JCBarrier(_JCB_VYHYBKA_KONC_POLOHA, Blk, Blk.id));

    // kontrola vyluky vyhybky:
    if (TBlkTurnout(Blk).lockout <> '') then
      barriers.Add(Self.JCBarrier(_JCB_VYHYBKA_VYLUKA, Blk, Blk.id));

    // kontrola stitku vyhybky:
    if (TBlkTurnout(Blk).note <> '') then
      barriers.Add(Self.JCBarrier(_JCB_VYHYBKA_STITEK, Blk, Blk.id));

    if (TBlkTurnout(Blk).position <> refugeeZav.position) then
     begin
      if (TBlkTurnout(Blk).emLock) then
        barriers.Add(Self.JCBarrier(_JCB_VYHYBKA_NOUZ_ZAVER, Blk, Blk.id))

      else if (TBlkTurnout(Blk).outputLocked) then
        barriers.Add(Self.JCBarrier(_JCB_ODVRAT_ZAMCENA, blk, refugeeZav.block));

      if (TBlkTurnout(Blk).occupied = TTrackState.occupied) then
        barriers.Add(Self.JCBarrier(_JCB_ODVRAT_OBSAZENA, blk, refugeeZav.block));
     end;//if poloha <> Poloha

    // kontrola spojky odvratu
    Blocks.GetBlkByID(TBlkTurnout(Blk).GetSettings.coupling, Blk2);
    if (Blk2 <> nil) then
     begin
      // kontrola vyluky vyhybky:
      if (TBlkTurnout(Blk2).lockout <> '') then
        barriers.Add(Self.JCBarrier(_JCB_VYHYBKA_VYLUKA, Blk2, Blk2.id));

      // kontrola stitku vyhybky:
      if (TBlkTurnout(Blk2).note <> '') then
        barriers.Add(Self.JCBarrier(_JCB_VYHYBKA_STITEK, Blk2, Blk2.id));

      // kontrola zamceni odvratu
      if (TBlkTurnout(Blk).position <> refugeeZav.position) then
       begin
        if (TBlkTurnout(Blk2).zaver > TZaver.no) then
         begin
          if (TBlkTurnout(Blk2).zaver = TZaver.ab) then
            barriers.Add(Self.JCBarrier(_JCB_USEK_AB, Blk2, Blk2.id))
          else
            barriers.Add(Self.JCBarrier(_JCB_USEK_ZAVER, Blk2, Blk2.id));
         end;

        if (TBlkTurnout(Blk2).emLock) then
          barriers.Add(Self.JCBarrier(_JCB_VYHYBKA_NOUZ_ZAVER, Blk2, Blk2.id))
        else if (TBlkTurnout(Blk2).outputLocked) then
          barriers.Add(Self.JCBarrier(_JCB_VYHYBKA_ZAMCENA, Blk2, Blk2.id));

        if (TBlkTurnout(Blk2).occupied = TTrackState.occupied) then
          barriers.Add(Self.JCBarrier(_JCB_USEK_OBSAZENO, Blk2, Blk2.id));
       end;
     end;
   end;

  // kontrola trati
  if (Self.m_data.railwayId > -1) then
   begin
    Blocks.GetBlkByID(Self.m_data.railwayId, Blk);
    glob := Blk.GetGlobalSettings();

    if ((Blk as TBlkRailway).direction = TRailwayDirection.disabled) then
      barriers.Add(Self.JCBarrier(_JCB_BLOK_DISABLED, Blk, Blk.id));

    cont := true;
    if ((blk as TBlkRailway).departureForbidden) then
      barriers.Add(Self.JCBarrier(_JCB_TRAT_ZAK, blk, Self.m_data.railwayId));
    if ((blk as TBlkRailway).request) then
      barriers.Add(Self.JCBarrier(_JCB_TRAT_ZADOST, blk, Self.m_data.railwayId));
    if (((TBlkRailway(blk).Zaver) or (TBlkRailway(blk).emLock)) and (Self.m_data.railwayDir <> TBlkRailway(blk).direction)) then
     begin
      barriers.Add(Self.JCBarrier(_JCB_TRAT_NESOUHLAS, blk, Self.m_data.railwayId));
      cont := false;
     end;
    if ((cont) and ((blk as TBlkRailway).Zaver)) then
      barriers.Add(Self.JCBarrier(_JCB_TRAT_ZAVER, blk, Self.m_data.railwayId));

    if (cont) and ((not TBlkRailway(blk).SameUserBothLinkers()) or ((blk as TBlkRailway).emLock)) then
      if ((((blk as TBlkRailway).GetSettings().rType = TRailwayType.permanent) or ((blk as TBlkRailway).GetSettings().rType = TRailwayType.request))
          and (Self.m_data.railwayDir <> (blk as TBlkRailway).direction)) then
       begin
        barriers.Add(Self.JCBarrier(_JCB_TRAT_NESOUHLAS, blk, Self.m_data.railwayId));
        cont := false;
       end;

    if ((cont) and (Self.m_data.railwayDir <> (blk as TBlkRailway).direction)) then
     begin
      // trat beze smeru, do ktere bude dle predchozi podminky povoleno vjet -> trat s automatickou zmenou souhlasu
      // -> kontrola volnosti vsech useku trati (protoze nastane zmena smeru)
      if (not TBlkRailway(Blk).ready) then
       begin
        barriers.Add(Self.JCBarrier(_JCB_TRAT_NESOUHLAS, blk, Self.m_data.railwayId));
        cont := false;
       end;
     end;

    if ((cont) and (Self.typ = TJCType.train)) then
     begin
      if (TBlkRT(Self.lastTrack).sectOccupied = TTrackState.occupied) then
       begin
        Blocks.GetBlkByID(Self.m_data.railwayId, Blk);
        barriers.Add(Self.JCBarrier(_JCB_TRAT_OBSAZENO, blk, Self.m_data.railwayId));
       end else if (not TBlkRT(Self.lastTrack).sectReady) then begin
        Blocks.GetBlkByID(Self.m_data.railwayId, Blk);
        barriers.Add(Self.JCBarrier(_JCB_TRAT_NEPRIPRAVENA, blk, Self.m_data.railwayId));
       end;
     end;

    Blocks.GetBlkByID(Self.m_data.railwayId, Blk);

    // kontrola stitku uvazky v nasi OR:
    if ((TBlkLinker(TBlkRailway(Blk).linkerA).areas.Count > 0) and
        (TBlkLinker(TBlkRailway(Blk).linkerA).areas[0] = Self.m_state.senderOR) and
        (TBlkLinker(TBlkRailway(Blk).linkerA).note <> '')) then
      barriers.Add(Self.JCBarrier(_JCB_TRAT_STITEK, TBlkLinker(TBlkRailway(Blk).linkerA),
          TBlkLinker(TBlkRailway(Blk).linkerA).id));

    if ((TBlkLinker(TBlkRailway(Blk).linkerB).areas.Count > 0) and
        (TBlkLinker(TBlkRailway(Blk).linkerB).areas[0] = Self.m_state.senderOR) and
        (TBlkLinker(TBlkRailway(Blk).linkerB).note <> '')) then
      barriers.Add(Self.JCBarrier(_JCB_TRAT_STITEK, TBlkLinker(TBlkRailway(Blk).linkerB),
          TBlkLinker(TBlkRailway(Blk).linkerB).id));

    // stitky a vyluky na tratovych usecich
    for track in TBlkRailway(Blk).GetSettings().trackIds do
     begin
      Blocks.GetBlkByID(track, Blk2);

      // vyluka
      if (TBlkTrack(Blk2).lockout <> '') then
        barriers.Add(Self.JCBarrier(_JCB_USEK_VYLUKA, blk2, blk2.id));

      // stitek
      if (TBlkTrack(Blk2).note <> '') then
        barriers.Add(Self.JCBarrier(_JCB_USEK_STITEK, blk2, blk2.id));
     end;
   end;

  // kontrola uzamceni podminkovych zamku:
  for refZaver in Self.m_data.locks do
   begin
    Blocks.GetBlkByID(refZaver.block, Blk);
    glob := Blk.GetGlobalSettings();

    // kontrola uzamceni
    if ((Blk as TBlkLock).keyReleased) then
      barriers.Add(Self.JCBarrier(_JCB_ZAMEK_NEUZAMCEN, blk, blk.id));
   end;//for i

 // kontrola ukradene loko v souprave pred navestidlem
 Blocks.GetBlkByID(Self.m_data.signalId, Blk2);
 Blk := (Blk2 as TBlkSignal).track;

 if ((Blk as TBlkTrack).IsTrain()) then
  begin
   flag := false;
   train := Self.GetTrain(Blk2, Blk);

   // kontrola rucniho rizeni lokomotiv
   if (Self.typ = TJCType.train) then
     for addr in train.HVs do
       if ((HVDb[addr].data.typ <> THVType.car) and ((HVDb[addr].stolen) or (HVDb[addr].ruc))) then
        begin
         barriers.Add(Self.JCBarrier(_JCB_HV_RUC, nil, addr));
         flag := true;
        end;

   // pokud jsou jen nektere lokomotivy rizene rucne
   if (flag) then
     for addr in train.HVs do
       if ((HVDb[addr].data.typ <> THVType.car) and (not HVDb[addr].stolen) and (not HVDb[addr].ruc)) then
        begin
         barriers.Add(Self.JCBarrier(_JCB_HV_NOT_ALL_RUC));
         break;
        end;

   // kontrola smeru soupravy
   if (Self.typ = TJCType.train) then
    begin
     if (train.sdata.dir_L or train.sdata.dir_S) then
       if (((TBlkSignal(Blk2).direction = THVSite.odd) and (not train.sdata.dir_L)) or
           ((TBlkSignal(Blk2).direction = THVSite.even) and (not train.sdata.dir_S))) then
         barriers.Add(Self.JCBarrier(_JCB_SPR_SMER, nil, train.index));
    end;

  end;
end;

////////////////////////////////////////////////////////////////////////////////
// kontrola podminek nouzove cesty:

procedure TJC.BarriersNC(var barriers: TList<TJCBarrier>);
var i, usek, cnt: Integer;
    Blk, blk2: TBlk;
    glob: TBlkSettings;
    turnoutZav: TJCTurnoutZav;
    crossingZav: TJCCrossingZav;
    refugeeZav: TJCRefugeeZav;
begin
  { nouzovou cestu nelze postavit pres:
     1) useky se zaverem
     2) vyhybky s nouzovym zaverem
    jinak lze vsechy bariery prekonat
  }

  // useky:
  if (Self.m_data.railwayId > -1) then
    cnt := Self.m_data.tracks.Count-1
  else
    cnt := Self.m_data.tracks.Count;

  for i := 0 to cnt-1 do
   begin
    Blocks.GetBlkByID(Self.m_data.tracks[i], Blk);
    glob := Blk.GetGlobalSettings();

    // zaver
    if ((Blk as TBlkTrack).Zaver <> TZaver.no) then
     begin
      if ((Blk as TBlkTrack).Zaver = TZaver.ab) then
        barriers.Add(Self.JCBarrier(_JCB_USEK_AB, Blk, Blk.id))
      else
        barriers.Add(Self.JCBarrier(_JCB_USEK_ZAVER, Blk, Blk.id));
     end;

    // vyluka
    if ((Blk as TBlkTrack).lockout <> '') then
      barriers.Add(Self.JCBarrier(_JCB_USEK_VYLUKA, blk, blk.id));

    // stitek
    if ((Blk as TBlkTrack).note <> '') then
      barriers.Add(Self.JCBarrier(_JCB_USEK_STITEK, blk, blk.id));
   end;//for i

  // kontrola vyhybek:
  for turnoutZav in Self.m_data.turnouts do
   begin
    Blocks.GetBlkByID(turnoutZav.block, Blk);
    glob := Blk.GetGlobalSettings();

    // kontrola vyluky vyhybky:
    if (TBlkTurnout(Blk).lockout <> '') then
      barriers.Add(Self.JCBarrier(_JCB_VYHYBKA_VYLUKA, Blk, Blk.id));

    // kontrola stitku vyhybky:
    if (TBlkTurnout(Blk).note <> '') then
      barriers.Add(Self.JCBarrier(_JCB_VYHYBKA_STITEK, Blk, Blk.id));

    // kontrola nouzoveho zaveru a redukce menu:
    if (TBlkTurnout(Blk).position <> turnoutZav.position) then
     begin
      if (TBlkTurnout(Blk).emLock) then
        barriers.Add(Self.JCBarrier(_JCB_VYHYBKA_NOUZ_ZAVER, Blk, Blk.id))
      else if (TBlkTurnout(Blk).outputLocked) then
        barriers.Add(Self.JCBarrier(_JCB_VYHYBKA_ZAMCENA, Blk, Blk.id));
     end;

    // kontrola spojky
    Blocks.GetBlkByID(TBlkTurnout(Blk).GetSettings.coupling, Blk2);
    // pokud nemam ja polohu, prespokladam, ze spojka bude muset byt prestavena -> musi byt volna, bez zaveru, ...
    // kontrolovat zaver z useku eni potreba - pokud je problem se zaverem, vyvstane uz na useku JC, jinak je vyhybka v poloze, ktere zaver nevadi
    if ((blk2 <> nil) and (TBlkTurnout(Blk).position <> turnoutZav.position)) then
     begin
      if (TBlkTurnout(Blk2).emLock) then
        barriers.Add(Self.JCBarrier(_JCB_VYHYBKA_NOUZ_ZAVER, Blk2, Blk2.id))
      else if (TBlkTurnout(Blk2).outputLocked) then
        barriers.Add(Self.JCBarrier(_JCB_VYHYBKA_ZAMCENA, Blk2, Blk2.id));
     end;
   end;//for i

  // kontrola prejezdu
  for crossingZav in Self.m_data.crossings do
   begin
    Blocks.GetBlkByID(crossingZav.crossingId, Blk);
    // kontrola stitku prejezdu:
    if ((Blk as TBlkCrossing).note <> '') then
      barriers.Add(Self.JCBarrier(_JCB_PREJEZD_STITEK, Blk, Blk.id));
   end;//for i

  // kontrola odvratu
  for refugeeZav in Self.m_data.refuges do
   begin
    Blocks.GetBlkByID(refugeeZav.block, Blk);
    glob := Blk.GetGlobalSettings();

    // kontrola vyluky vyhybky:
    if (TBlkTurnout(Blk).lockout <> '') then
      barriers.Add(Self.JCBarrier(_JCB_VYHYBKA_VYLUKA, Blk, Blk.id));

    // kontrola stitku vyhybky:
    if (TBlkTurnout(Blk).note <> '') then
      barriers.Add(Self.JCBarrier(_JCB_VYHYBKA_STITEK, Blk, Blk.id));

    if (TBlkTurnout(Blk).position <> refugeeZav.position) then
     begin
      if (TBlkTurnout(Blk).emLock) then
        barriers.Add(Self.JCBarrier(_JCB_VYHYBKA_NOUZ_ZAVER, Blk, Blk.id))

      else if ((TBlkTurnout(Blk).Zaver <> TZaver.no) or (TBlkTurnout(Blk).outputLocked)) then
        barriers.Add(Self.JCBarrier(_JCB_ODVRAT_ZAMCENA, blk, refugeeZav.block));
     end;//if poloha <> Poloha

    // kontrola spojky odvratu
    Blocks.GetBlkByID(TBlkTurnout(Blk).GetSettings.coupling, Blk2);
    if (blk2 <> nil) then
     begin
      // kontrola vyluky vyhybky:
      if (TBlkTurnout(Blk2).lockout <> '') then
        barriers.Add(Self.JCBarrier(_JCB_VYHYBKA_VYLUKA, Blk2, Blk2.id));

      // kontrola stitku vyhybky:
      if (TBlkTurnout(Blk2).note <> '') then
        barriers.Add(Self.JCBarrier(_JCB_VYHYBKA_STITEK, Blk2, Blk2.id));

      // kontrola zamceni odvratu
      if (TBlkTurnout(Blk).position <> refugeeZav.position) then
       begin
        if (TBlkTurnout(Blk2).zaver > TZaver.no) then
         begin
          if (TBlkTurnout(Blk2).zaver = TZaver.ab) then
            barriers.Add(Self.JCBarrier(_JCB_USEK_AB, Blk2, Blk2.id))
          else
            barriers.Add(Self.JCBarrier(_JCB_USEK_ZAVER, Blk2, Blk2.id));
         end;

        if (TBlkTurnout(Blk2).emLock) then
          barriers.Add(Self.JCBarrier(_JCB_VYHYBKA_NOUZ_ZAVER, Blk2, Blk2.id))
        else if (TBlkTurnout(Blk2).outputLocked) then
          barriers.Add(Self.JCBarrier(_JCB_VYHYBKA_ZAMCENA, Blk2, Blk2.id))
       end;
     end;
   end;//for i

  // kontrola trati
  if (Self.m_data.railwayId > -1) then
   begin
    Blocks.GetBlkByID(Self.m_data.railwayId, Blk);

    // stitky a vyluky na tratovych usecich
    for usek in TBlkRailway(Blk).GetSettings().trackIds do
     begin
      Blocks.GetBlkByID(usek, Blk2);

      // vyluka
      if (TBlkTrack(Blk2).lockout <> '') then
        barriers.Add(Self.JCBarrier(_JCB_USEK_VYLUKA, blk2, blk2.id));

      // stitek
      if (TBlkTrack(Blk2).note <> '') then
        barriers.Add(Self.JCBarrier(_JCB_USEK_STITEK, blk2, blk2.id));
     end;

    // kontrola stitku uvazky v nasi OR:
    if ((TBlkLinker(TBlkRailway(Blk).linkerA).areas.Count > 0) and
        (TBlkLinker(TBlkRailway(Blk).linkerA).areas[0] = Self.m_state.senderOR) and
        (TBlkLinker(TBlkRailway(Blk).linkerA).note <> '')) then
      barriers.Add(Self.JCBarrier(_JCB_TRAT_STITEK, TBlkLinker(TBlkRailway(Blk).linkerA),
          TBlkLinker(TBlkRailway(Blk).linkerA).id));

    if ((TBlkLinker(TBlkRailway(Blk).linkerB).areas.Count > 0) and
        (TBlkLinker(TBlkRailway(Blk).linkerB).areas[0] = Self.m_state.senderOR) and
        (TBlkLinker(TBlkRailway(Blk).linkerB).note <> '')) then
      barriers.Add(Self.JCBarrier(_JCB_TRAT_STITEK, TBlkLinker(TBlkRailway(Blk).linkerB),
          TBlkLinker(TBlkRailway(Blk).linkerB).id));
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
function TJC.Activate(SenderPnl: TIdContext; SenderOR: TObject; bariery_out: TJCBarriers;
                      from_stack: TObject = nil; nc: Boolean = false; fromAB: Boolean = false;
                      abAfter: Boolean = false): Integer;
var i: Integer;
    barriers: TJCBarriers;
    barrier: TJCBarrier;
    critical: Boolean;
    upo: TUPOItems;
    item: TUPOItem;
 begin
  Self.m_state.timeOut := Now + EncodeTime(0, _JC_INITPOTVR_TIMEOUT_SEC div 60, _JC_INITPOTVR_TIMEOUT_SEC mod 60, 0);

  Self.m_state.from_stack := from_stack;
  Self.m_state.senderOR := SenderOR;
  Self.m_state.senderPnl := SenderPnl;
  Self.m_state.nc := nc;
  Self.m_state.ab := (abAfter) and (Self.typ = TJCType.train);
  Self.m_state.crossingWasClosed := false;
  Self.m_state.lastTrackOrRailwayOccupied := false;

  Self.Log('Požadavek na stavění, kontroluji podmínky');

  barriers := Self.Barriers(Self.m_state.nc);
  upo := TList<TUPOItem>.Create;
  try
    // ignorujeme AB zaver pokud je staveno z AB seznamu
    if (fromAB) then
      for i := barriers.Count-1 downto 0 do
        if (barriers[i].typ = _JCB_USEK_AB) then
          barriers.Delete(i);

    // existuji kriticke bariery?
    critical := false;
    for barrier in barriers do
     begin
      if ((barrier.typ = _JCB_USEK_LAST_OBSAZENO) or (barrier.typ = _JCB_TRAT_OBSAZENO)) then
        Self.m_state.lastTrackOrRailwayOccupied := true;

      if ((Self.CriticalBarrier(barrier.typ)) or (not Self.WarningBarrier(barrier.typ))) then
       begin
        critical := true;
        upo.Add(Self.JCBarrierToMessage(barrier));
       end;
     end;

    if (critical) then
     begin
      // kriticke bariey existuji -> oznamim je
      Self.Log('Celkem '+IntToStr(barriers.Count)+' bariér, ukončuji stavění');
      if (SenderPnl <> nil) then
       begin
        Self.step := _STEP_KRIT_BARIERY;
        PanelServer.UPO(Self.m_state.senderPnl, upo, true, nil, Self.CritBarieraEsc, Self);
       end;
      Exit(1);
     end else begin
      // bariery k potvrzeni
      if (((barriers.Count > 0) or ((nc) and (from_stack <> nil))) and (SenderPnl <> nil)) then
       begin
        Self.Log('Celkem '+IntToStr(barriers.Count)+' warning bariér, žádám potvrzení...');
        for i := 0 to barriers.Count-1 do
         upo.Add(Self.JCBarrierToMessage(barriers[i]));

        // pokud se jedna o NC ze zasobniku, zobrazuji jeste upozorneni na NC
        if ((nc) and (from_stack <> nil)) then
         begin
          item[0] := GetUPOLine('Pozor !', taCenter, clYellow, $A0A0A0);
          item[1] := GetUPOLine('Stavění nouzové cesty.');
          item[2] := GetUPOLine('');
          upo.Add(item);
         end;

        PanelServer.UPO(Self.m_state.senderPnl, upo, false, Self.UPO_OKCallback, Self.UPO_EscCallback, Self);
        Self.step := _STEP_POTVR_BARIERY;
        Exit(0);
       end;
     end;

    // v jzdni ceste nejsou zadne bariery -> stavim
    Self.Log('Žádné bariéry, stavím');
    Self.SetInitStep();
  finally
    if (bariery_out <> nil) then
      bariery_out.AddRange(barriers);
    barriers.Free();
    upo.Free();
  end;

  Result := 0;
 end;

function TJC.Activate(SenderPnl: TIdContext; SenderOR: TObject;
  from_stack: TObject = nil; nc: Boolean = false; fromAB: Boolean = false;
  abAfter: Boolean = false): Integer;
begin
 Result := Self.Activate(SenderPnl, SenderOR, nil, from_stack, nc, fromAB, abAfter);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TJC.PS_vylCallback(Sender: TIdContext; success: Boolean);
var barriers: TJCBarriers;
    critical: Boolean;
    i: Integer;
begin
 // pro potvrzovaci sekvenci vyluky by mel byt krok '6'
 if (Self.step <> _STEP_POTVR_SEKV) then Exit();

 if (not success) then
  begin
   Self.CancelActivating('');
   Exit();
  end;

 // znovu zkontrolujeme bariery (behem potvrzovani se mohly vyskytnout)
 barriers := Self.Barriers(Self.m_state.nc);

 // existuji kriticke bariery?
 critical := false;
 for i := 0 to barriers.Count-1 do
  if ((barriers[i].typ <> _JCB_STAVENI) and ((Self.CriticalBarrier(barriers[i].typ)) or (not Self.WarningBarrier(barriers[i].typ)))) then
   begin
    critical := true;
    break;
   end;

 // behem potvrzovani se mohly vyskytnout
 if (critical) then
  begin
   Self.CancelActivating('Nelze postavit - kritické bariéry');
   if (Self.m_state.senderPnl <> nil) and (Self.m_state.senderOR <> nil) then
     PanelServer.BottomError(Self.m_state.senderPnl, 'Nelze postavit '+Self.name+' - kritické bariéry',
        (Self.m_state.senderOR as TArea).ShortName, 'TECHNOLOGIE');
   barriers.Free();
   Exit();
  end;

 Self.Log('Krok 2 : povrzovaci sekvence OK');
 Self.SetInitStep();
end;

////////////////////////////////////////////////////////////////////////////////
// callbacky z upozornovacich barier:

procedure TJC.UPO_OKCallback(Sender: TObject);
var barriers: TJCBarriers;
    critical: Boolean;
    i: Integer;
    signal: TBlk;
    conditions: TList<TConfSeqItem>;
begin
 if (Self.step <> _STEP_POTVR_BARIERY) then Exit();

 Self.Log('Krok 1 : upozornění schválena, kontroluji znovu bariéry');

 // znovu zkontrolujeme bariery (behem potvrzovani se mohly vyskytnout)
 barriers := Self.Barriers(Self.m_state.nc);

 // existuji kriticke bariery?
 critical := false;
 for i := 0 to barriers.Count-1 do
  if ((barriers[i].typ <> _JCB_STAVENI) and ((Self.CriticalBarrier(barriers[i].typ)) or (not Self.WarningBarrier(barriers[i].typ)))) then
   begin
    critical := true;
    break;
   end;

 // behem potvrzovani se mohly vyskytnout
 if (critical) then
  begin
   Self.CancelActivating('Nelze postavit - kritické bariéry');
   if (Self.m_state.senderPnl <> nil) and (Self.m_state.senderOR <> nil) then
     PanelServer.BottomError(Self.m_state.senderPnl, 'Nelze postavit '+Self.name+' - kritické bariéry',
        (Self.m_state.senderOR as TArea).ShortName, 'TECHNOLOGIE');
   barriers.Free();
   Exit();
  end;

 // existuji bariery na potvrzeni potvrzovaci sekvenci ?
 conditions := TList<TConfSeqItem>.Create;
 for i := 0 to barriers.Count-1 do
  begin
   if (Self.PotvrSekvBarrier(barriers[i].typ)) then
     conditions.Add(TArea.GetPSPodminka(barriers[i].block, TJC.PotvrSekvBarrierToReason(barriers[i].typ)));
  end;//for i

 if (conditions.Count > 0) then
  begin
   // ano, takoveto bariery existuji -> potvrzovaci sekvence
   Self.Log('Bariéry s potvrzovací sekvencí, žádám potvrzení...');
   Blocks.GetBlkByID(Self.m_data.signalId, signal);

   if (Self.m_state.senderPnl <> nil) and (Self.m_state.senderOR <> nil) then
     PanelServer.ConfirmationSequence(Self.m_state.senderPnl, Self.PS_vylCallback, (Self.m_state.senderOR as TArea),
        'Jízdní cesta s potvrzením', TBlocks.GetBlksList(signal, Self.lastTrack), conditions);

   Self.step := _STEP_POTVR_SEKV;
  end else begin
   // ne, takoveto bariery neexistuji -> stavim jizdni cestu
   Self.SetInitStep();
  end;
end;//proceudre

procedure TJC.UPO_EscCallback(Sender: TObject);
begin
 if (Self.step = _STEP_POTVR_BARIERY) then
  begin
   Self.CancelActivating();
   Self.step := _STEP_DEFAULT;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

// jakmile je zavolano StavJC(), tato funkce se stara o to, aby staveni doslo az do konce
// kontroluje prubezne podminky apod.
procedure TJC.UpdateActivating();
var i, j: Integer;
    blk: TBlk;
    aZaver: TJCType;
    neprofil: TBlkTrack;
    closed, anyClosed: Boolean;
    str: string;
    npCall:^TNPCallerData;
    count: Integer;
    stavim: Cardinal;
    barriers: TList<TJCBarrier>;
    barrier: TJCBarrier;
    nextTurnout: Integer;
    uzavBlok: Integer;
    trackZav: Integer;
    crossingZav: TJCCrossingZav;
    turnoutZav: TJCTurnoutZav;
    refugeeZav: TJCRefugeeZav;
    refZav: TJCRefZav;
    turnout: TBlkTurnout;
    usek, nextUsek: TBlkTrack;
    lock: TBlkLock;
    crossing: TBlkCrossing;
    signal: TBlkSignal;
    railway: TBlkRailway;
    area: TArea;
    tuAdd: TBlkRT;
    train: TTrain;
    chEv: TChangeEvent;
    remEvDataPtr: ^TRemoveEventData;
 begin
  if ((not Self.activating) and (Self.step <> _JC_STEP_CEKANI_POSLEDNI_USEK)) then Exit();

  Blocks.GetBlkByID(Self.m_data.signalId, TBlk(signal));

  //////////////////////////////////////////////////////////////////////////////
  // staveni vlakovych a posunovych cest:

  case (Self.step) of
   _JC_STEP_INIT: begin
      // nejprve priradime uvolneni zaveru posledniho bloku uvolneni zaveru predposledniho bloku
      if (Self.m_data.tracks.Count > 1) then
       begin
        Blocks.GetBlkByID(Self.m_data.tracks[Self.m_data.tracks.Count-2], TBlk(usek));
        usek.AddChangeEvent(usek.eventsOnZaverReleaseOrAB,
          CreateChangeEvent(ceCaller.CopyUsekZaver, Self.lastTrack.id));

        for i := 0 to Self.m_data.tracks.Count-2 do
         begin
          Blocks.GetBlkByID(Self.m_data.tracks[i], TBlk(usek));
          Blocks.GetBlkByID(Self.m_data.tracks[i+1], TBlk(nextUsek));

          if (usek.spnl.stationTrack) then
           begin
            chEv := CreateChangeEvent(ceCaller.CopyUsekZaver, usek.id);
            TBlk.AddChangeEvent(nextUsek.eventsOnZaverReleaseOrAB, chEv);
            GetMem(remEvDataPtr, SizeOf(TRemoveEventData));
            remEvDataPtr^ := TRemoveEventData.Create(nextUsek.eventsOnZaverReleaseOrAB, chEv);
            TBlk.AddChangeEvent(usek.eventsOnZaverReleaseOrAB, CreateChangeEvent(ceCaller.RemoveEvent, Integer(remEvDataPtr)));
           end;
         end;
       end;

      Self.Log('Useky: nastavuji staveci zavery');
      for trackZav in Self.m_data.tracks do
       begin
        Blocks.GetBlkByID(trackZav, TBlk(usek));
        usek.Zaver := TZaver.staveni;
       end;//for cyklus

      Self.Log('Vyhybky: zamykam do pozadovanych poloh');
      Self.m_state.nextTurnout := -1;
      stavim := 0;
      nextTurnout := -1;
      for i := 0 to Self.m_data.turnouts.Count-1 do
       begin
        turnoutZav := Self.m_data.turnouts[i];

        Blocks.GetBlkByID(Self.m_data.turnouts[i].block, TBlk(turnout));
        if (turnout.position <> TTurnoutPosition(turnoutZav.position)) then
         begin
          if (stavim >= _JC_MAX_VYH_STAVENI) then
           begin
            if (nextTurnout = -1) then
              nextTurnout := i;
            continue;
           end;
          Inc(stavim);
         end;

        // Warning: this may call callback directly
        // Callback for just-locking turnout will have no effect due to nextVyhybka = -1
        turnout.SetPosition(TTurnoutPosition(turnoutZav.position),
                            true, false, Self.TurnoutMovedJCPC, Self.TurnoutErrJCPC);
       end;

      for i := 0 to Self.m_data.refuges.Count-1 do
       begin
        refugeeZav := Self.m_data.refuges[i];

        // nastaveni odvratu
        Blocks.GetBlkByID(refugeeZav.block, TBlk(turnout));
        if (turnout.position <> TTurnoutPosition(refugeeZav.position)) then
         begin
          if (stavim >= _JC_MAX_VYH_STAVENI) then
           begin
            if (nextTurnout = -1) then
              nextTurnout := i;
            continue;
           end;
          Inc(stavim);
         end;

        turnout.IntentionalLock();

        // pridani zruseni redukce
        Blocks.GetBlkByID(refugeeZav.ref_blk, TBlk(usek));
        usek.AddChangeEvent(usek.eventsOnZaverReleaseOrAB,
          CreateChangeEvent(ceCaller.NullVyhybkaMenuReduction, refugeeZav.block));

        // Warning: this may call callback directly
        // Callback for just-locking turnout will have no effect due to nextVyhybka = -1
        turnout.SetPosition(TTurnoutPosition(refugeeZav.position),
                            true, false, Self.TurnoutMovedJCPC, Self.TurnoutErrJCPC);
       end;

      Self.m_state.nextTurnout := nextTurnout;

      Self.Log('Zamky: nastavuji zavery');
      for refZav in Self.m_data.locks do
       begin
        Blocks.GetBlkByID(refZav.ref_blk, TBlk(usek));
        usek.AddChangeEvent(usek.eventsOnZaverReleaseOrAB,
          CreateChangeEvent(ceCaller.NullZamekZaver, refZav.block));

        // nastaveni zaveru zamku
        Blocks.GetBlkByID(refZav.block, TBlk(lock));
        lock.Zaver := true;
       end;

      Self.step := _JC_STEP_CEKANI_VYHYBKA_POLOHA;
      Self.Log('Vyhybky: poloha: detekce');
     end;//case 0


   _JC_STEP_CEKANI_VYHYBKA_POLOHA: begin
      for turnoutZav in Self.m_data.turnouts do
       begin
        Blocks.GetBlkByID(turnoutZav.block, TBlk(turnout));
        if (turnout.position <> turnoutZav.position) then
          Exit();
       end;
      for refugeeZav in Self.m_data.refuges do
       begin
        Blocks.GetBlkByID(refugeeZav.block, TBlk(turnout));
        if (turnout.position <> refugeeZav.position) then
          Exit();
       end;

      Self.Log('Krok 11 : vyhybky: poloha: OK');
      Self.m_state.nextTurnout := -1;

      Self.Log('Krok 11: useky: nastavuji nouzovy zaver');
      for trackZav in Self.m_data.tracks do
       begin
        Blocks.GetBlkByID(trackZav, TBlk(usek));
        usek.Zaver := TZaver.nouz;
       end;

      Self.Log('Krok 11: useky: kontroluji volnost useku s neprofilovymi styky, zapevnuji neprofilove useky');
      for turnoutZav in Self.m_data.turnouts do
       begin
        neprofil := nil;
        Blocks.GetBlkByID(turnoutZav.block, TBlk(turnout));

        if ((turnoutZav.position = TTurnoutPosition.plus) and (turnout.npBlokPlus <> nil)) then
          neprofil := TBlkTrack(turnout.npBlokPlus)
        else if ((turnoutZav.position = TTurnoutPosition.minus) and (turnout.npBlokMinus <> nil)) then
          neprofil := TBlkTrack(turnout.npBlokMinus);

        if (neprofil <> nil) then
         begin
          if (neprofil.occupied <> TTrackState.free) then
           begin
            if (Self.m_state.senderPnl <> nil) and (Self.m_state.senderOR <> nil) then
              PanelServer.BottomError(Self.m_state.senderPnl, 'Neuvolněn ' + neprofil.name,
                  (Self.m_state.senderOR as TArea).ShortName, 'TECHNOLOGIE');
            Self.Log('Krok 14 : Neprofilovy usek '+neprofil.name+' neuvolnen!');
            Self.CancelActivating();
            Exit();
           end;

          neprofil.AddNeprofilJC(Self.m_data.id);

          Blocks.GetBlkByID(turnout.trackID, TBlk(usek));

          npCall := GetMemory(SizeOf(TNPCallerData));
          npCall.usekId := neprofil.id;
          npCall.jcId   := Self.m_data.id;
          usek.AddChangeEvent(usek.eventsOnZaverReleaseOrAB,
              CreateChangeEvent(ceCaller.RemoveUsekNeprofil, Integer(npCall)));
         end;
       end;

      if ((signal.ZAM) or (Self.m_state.lastTrackOrRailwayOccupied)) then
        Self.step := _JC_STEP_FINALNI_ZAVER
      else
        Self.step := _JC_STEP_ZAVRIT_PREJEZDY;
     end;//case 1


   _JC_STEP_ZAVRIT_PREJEZDY: begin
       // prejezdy
       Self.m_state.crossingWasClosed := true;
       anyClosed := false;
       for i := 0 to Self.m_data.crossings.Count-1 do
        begin
         crossingZav := Self.m_data.crossings[i];
         if (crossingZav.closeTracks.Count = 0) then
           continue;

         Blocks.GetBlkByID(crossingZav.crossingId, TBlk(crossing));
         closed := false;

         // prejezd uzavirame jen v pripade, ze nejaky z jeho aktivacnich bloku je obsazen
         // v pripade posunove cesty uzavirame vzdy

         if (Self.typ = TJCType.shunt) then
          begin
           // posunova cesta:
           Self.Log('Krok 12 : prejezd '+crossing.name+' - uzaviram');

           crossing.Zaver := true;

           // pridani zruseni redukce, tim se prejezd automaticky otevre po zruseni zaveru bloku pod nim
           Blocks.GetBlkByID(crossingZav.openTrack, TBlk(usek));
           usek.AddChangeEvent(usek.eventsOnZaverReleaseOrAB,
             CreateChangeEvent(ceCaller.NullPrejezdZaver, crossingZav.crossingId));

           closed := true;
           anyClosed := true;
          end else begin

           // vlakova cesta:
           for uzavBlok in crossingZav.closeTracks do
            begin
             Blocks.GetBlkByID(uzavBlok, TBlk(usek));
             if (usek.occupied = TTrackState.occupied) then
              begin
               Self.Log('Krok 12 : prejezd '+crossing.name+' - aktivacni usek '+usek.name+' obsazen - uzaviram');

               crossing.Zaver := true;

               // pridani zruseni redukce, tim se prejezd automaticky otevre po zruseni zaveru bloku pod nim
               Blocks.GetBlkByID(crossingZav.openTrack, TBlk(usek));
               usek.AddChangeEvent(usek.eventsOnZaverReleaseOrAB,
                 CreateChangeEvent(ceCaller.NullPrejezdZaver, crossingZav.crossingId));

               closed := true;
               anyClosed := true;
               break;
              end;
            end;//for j
          end;// else posunova cesta

         if (not closed) then
          begin
           // prejezd neuzaviram -> pridam pozadavek na zavreni pri obsazeni do vsech aktivacnich useku
           for uzavBlok in crossingZav.closeTracks do
            begin
             Blocks.GetBlkByID(uzavBlok, TBlk(usek));
             if (not usek.eventsOnOccupy.Contains(CreateChangeEvent(Self.TrackCloseCrossing, i))) then
               usek.AddChangeEvent(usek.eventsOnOccupy, CreateChangeEvent(Self.TrackCloseCrossing, i));
            end;

           Self.Log('Krok 12 : prejezd '+crossing.name+' - zadny aktivacni usek neobsazen - nechavam otevreny');
          end;
        end;//for i

      if (anyClosed) then
       begin
        Self.step := _JC_STEP_CEKANI_PREJEZDY;
        Self.m_state.timeOut := Now + EncodeTime(0, _JC_PRJ_TIMEOUT_SEC div 60, _JC_PRJ_TIMEOUT_SEC mod 60, 0);
       end else
        Self.step := _JC_STEP_FINALNI_ZAVER;

     end;


   _JC_STEP_CEKANI_PREJEZDY: begin
       // kontrola stavu prejezdu
       for crossingZav in Self.m_data.crossings do
        begin
         if (crossingZav.closeTracks.Count = 0) then
           continue;

         Blocks.GetBlkByID(crossingZav.crossingId, TBlk(crossing));

         if (crossing.state <> TBlkCrossingBasicState.closed) then Exit();
         Self.Log('Krok 13 : prejezd '+crossing.name+' uzavren');
        end;//for i

      Self.step := _JC_STEP_FINALNI_ZAVER;
     end;


   _JC_STEP_FINALNI_ZAVER: begin
      Self.Log('Krok 14 : useky: nastavit validni zaver');

      aZaver := Self.typ;

      for i := 0 to Self.m_data.tracks.Count-1 do
       begin
        trackZav := Self.m_data.tracks[i];
        Blocks.GetBlkByID(trackZav, TBlk(usek));
        usek.Zaver := TZaver(aZaver);
       end;

      signal.DNjc := Self;

      if (Self.IsCriticalBarrier()) then
       begin
        // Nepostavit navestidlo!
        Self.step := _JC_STEP_FINISH;
        Exit();
       end;

      if ((signal.ZAM) or (Self.m_state.lastTrackOrRailwayOccupied)) then
       begin
        Self.Log('Krok 14 : navestidlo: nestavim');
        Self.step := _JC_STEP_FINISH;
       end else begin
        Self.Log('Krok 14 : navestidlo: stavim...');
        Self.SetSignalSignal();
        Self.step := _JC_STEP_CEKANI_NAVESTIDLO;
       end;
   end;// case 14

   _JC_STEP_CEKANI_NAVESTIDLO: begin
     if (signal.signal > ncStuj) then
      begin
       Self.Log('Krok 15 : navestidlo postaveno');
       Self.step := _JC_STEP_FINISH;
      end;
   end;

   _JC_STEP_FINISH: begin
      Self.CancelSignalBegin();
      Self.CancelVBs();
      Self.CancelTrackEnd();

      // nastavit front blok soupravy
      usek := signal.track as TBlkTrack;
      if (usek.IsTrain()) then
        Self.GetTrain(signal, usek).front := usek;

      if (not usek.signalJCRef.Contains(signal)) then
        usek.signalJCRef.Add(signal);

      signal.DNjc := Self;

      if (Self.m_state.lastTrackOrRailwayOccupied) then
        Self.step := _JC_STEP_CEKANI_POSLEDNI_USEK
      else
        Self.step := _STEP_DEFAULT;

      // kdyby nastala nize chyba, musi byt moznost JC smazat ze zasobniku
      if (Self.m_state.from_stack <> nil) then
        (Self.m_state.from_stack as TORStack).firstEnabled := true;

      // Kontrola kritickych podminek.
      // (behem staveni mohla nastat zmena)
      if (Self.IsCriticalBarrier()) then
       begin
        if (signal.signal <> ncStuj) then
          signal.signal := ncStuj;
        if (Self.m_state.senderPnl <> nil) and (Self.m_state.senderOR <> nil) then
          PanelServer.BottomError(Self.m_state.senderPnl, 'Podmínky pro JC nesplněny!',
            (Self.m_state.senderOR as TArea).ShortName, 'TECHNOLOGIE');
        Self.Log('Krok 16 : Podmínky pro JC nesplněny!');
        Exit();
       end;

      // trat
      // zruseni redukce posledniho bloku jizdni cesty je navazano na zruseni zaveru trati
      // -> jakmile dojde ke zruseni zaveru posledniho bloku, dojde ke zruseni zaveru trati
      if (Self.m_data.railwayId > -1) then
       begin
        Blocks.GetBlkByID(Self.m_data.railwayId, TBlk(railway));

        if (Self.typ = TJCType.train) then railway.Zaver := true;

        // posledni blok posunove cesty je trat = posun mezi dopravnami -> zavedeme zakaz odjezdu do trati
        if (Self.typ = TJCType.shunt) then
         begin
          case (Self.m_data.railwayDir) of
           TRailwayDirection.AtoB : TBlkLinker(railway.linkerA).departureForbidden := true;
           TRailwayDirection.BtoA : TBlkLinker(railway.linkerB).departureForbidden := true;
          end;
         end;

        railway.direction := Self.m_data.railwayDir;

        // zruseni zaveru posledniho bloku JC priradime zruseni zaveru trati
        Self.lastTrack.AddChangeEvent(TBlkTrack(Self.lastTrack).eventsOnZaverReleaseOrAB,
          CreateChangeEvent(ceCaller.NullTratZaver, Self.m_data.railwayId));
       end;

      if ((signal.ZAM) or (Self.m_state.lastTrackOrRailwayOccupied)) then
        Self.destroyBlock := _JC_DESTROY_SIGNAL_STUJ
      else
        Self.destroyBlock := _JC_DESTROY_SIGNAL_TRACK;
      Self.destroyEndBlock := _JC_DESTROY_SIGNAL_STUJ;

      if (Self.typ = TJCType.train) then
        Blocks.TrainPrediction(signal);

      // pokud je cesta ze zasobniku, smazeme ji odtam
      if (Self.m_state.from_stack <> nil) then
       begin
        (Self.m_state.from_stack as TORStack).RemoveJC(Self);
        Self.m_state.from_stack := nil;
       end;

      signal.PropagatePOdjToTrat();

      if ((Self.m_state.ab) and (not signal.AB)) then
        signal.ABJC := Self;

      Self.Log('Postavena JC '+Self.name);
   end;

   _JC_STEP_CEKANI_POSLEDNI_USEK: begin
     blk := Self.lastTrack;

     if (Self.m_data.railwayId > -1) then
      begin
       if (TBlkRT(blk).sectReady) then
        begin
         Self.m_state.lastTrackOrRailwayOccupied := false;
         Self.DN();
        end;
      end else begin
       if ((TBlkTrack(blk).occupied = TTrackState.free) and (not TBlkTrack(blk).IsTrain)) then
        begin
         Self.m_state.lastTrackOrRailwayOccupied := false;
         Self.DN();
        end;
      end;
   end;

   ///////////////////////////////////////////////////////////////////////////
   // staveni nouzovych cest:

   _NC_STEP_INIT: begin
    // vsem usekum nastavime staveci zaver:
    Self.Log('Krok 100: useky: nastavuji staveci zavery');
    for trackZav in Self.m_data.tracks do
     begin
      Blocks.GetBlkByID(trackZav, TBlk(usek));
      usek.Zaver := TZaver.staveni;
     end;//for cyklus

    // nastavit nouzovy zaver uvazky
    if (Self.m_data.railwayId > -1) then
     begin
      Self.Log('Krok 100: trat: nastavuji nouzovy zaver uvazky');
      Blocks.GetBlkByID(Self.m_data.railwayId, TBlk(railway));

      // najdeme si uvazku, ktera je v OR navestidla a te nastavime nouzovy zaver
      if ((railway.linkerA as TBlkLinker).areas.Count > 0) then
       begin
        for area in signal.areas do
          if ((railway.linkerA as TBlkLinker).areas[0] = area) then
             (railway.linkerA as TBlkLinker).emLock := true;

        for area in signal.areas do
          if ((railway.linkerB as TBlkLinker).areas[0] = area) then
             (railway.linkerB as TBlkLinker).emLock := true;
       end;
     end;

    // nastavit vyhybky do pozadovanych poloh:
    Self.Log('Krok 100: vyhybky: nastavuji do pozadovanych poloh');

    Self.m_state.nextTurnout := 0;

    while ((Self.m_state.nextTurnout <> -1) and (Self.m_state.nextTurnout < _JC_MAX_VYH_STAVENI) and
           (Self.m_state.nextTurnout < Self.m_data.turnouts.Count)) do
     begin
      turnoutZav := Self.m_data.turnouts[Self.m_state.nextTurnout];
      Blocks.GetBlkByID(turnoutZav.block, TBlk(turnout));

      Inc(Self.m_state.nextTurnout);
      turnout.SetPosition(TTurnoutPosition(turnoutZav.position), // this call could increase nextVyhybka directly! or even set nextVyhybka = -1
                          true, false, Self.TurnoutMovedNC, Self.TurnoutErrNC);
     end;

    // For simplicity solve odvrat just in callback
    // This may be a little bit slower, but will generally work fine

    Self.Log('Krok 100: prejezdy: uzaviram');
    for crossingZav in Self.m_data.crossings do
     begin
      if (crossingZav.closeTracks.Count = 0) then
        continue;

      Blocks.GetBlkByID(crossingZav.crossingId, TBlk(crossing));
      if (not crossing.pcEmOpen) then
        crossing.pcClosed := true;
     end;

    // nastavit nouzovy zaver zamkum
    for refZav in Self.m_data.locks do
     begin
      Blocks.GetBlkByID(refZav.block, TBlk(lock));
      lock.emLock := true;
      signal.AddBlkToRnz(lock.id, false);
     end;

    Self.m_state.ncBarieryCntLast := -1;   // tady je potreba mit cislo < 0

    Self.step := _NC_STEP_BARIERA_UPDATE;
   end;//case 100

   _NC_STEP_BARIERA_UPDATE: begin
    // prubezne kontroluji podminky a zobrazuji potvrzovaci sekvenci

    // zjistime aktualni bariery:
    Self.m_state.ncBariery.Clear();
    Self.BarriersNCToAccept(Self.m_state.ncBariery);

    // kontrolujeme rozdilnost seznamu:
    if (Self.m_state.ncBariery.Count <> Self.m_state.ncBarieryCntLast) then
     begin
      Self.Log('Krok 101: zmena potvr., odesilam aktualni seznam');
      if (Self.typ = TJCType.train) then
        str := 'Zapnutí přivolávací návěsti'
      else
        str := 'Nouzová posunová cesta';

      if (Self.m_state.senderPnl <> nil) and (Self.m_state.senderOR <> nil) then
        PanelServer.ConfirmationSequence(Self.m_state.senderPnl, Self.NC_PS_Callback, Self.m_state.senderOR as TArea,
          str, TBlocks.GetBlksList(signal, lastTrack), Self.BarriersToPotvrSekv(Self.m_state.ncBariery));
     end;
    Self.m_state.ncBarieryCntLast := Self.m_state.ncBariery.Count;

    // nastavovani smeru trati:
    if (Self.m_data.railwayId > -1) then
     begin
      Blocks.GetBlkByID(Self.m_data.railwayId, TBlk(railway));

      // pokud v trati neni zavedena blokova podminka, zavedeme ji
      if ((Self.typ = TJCType.train) and (railway.direction = Self.data.railwayDir) and (not railway.BP)) then
        railway.BP := true;

      // posledni blok posunove cesty je trat = posun mezi dopravnami -> zavedeme zakaz odjezdu do trati
      if ((Self.typ = TJCType.shunt) and (railway.direction = Self.m_data.railwayDir)) then
       begin
        case (Self.m_data.railwayDir) of
         TRailwayDirection.AtoB : if (not TBlkLinker(railway.linkerA).departureForbidden) then TBlkLinker(railway.linkerA).departureForbidden := true;
         TRailwayDirection.BtoA : if (not TBlkLinker(railway.linkerB).departureForbidden) then TBlkLinker(railway.linkerB).departureForbidden := true;
        end;
       end;
     end;
   end;

   _NC_STEP_BARIERY_POTVRZENY: begin
    // potrvzovaci sekvence potvrzena -> stavim navestidlo, ...

    Self.m_state.nextTurnout := -1;
    Self.Log('Krok 102: useky: rusim zavery');
    for trackZav in Self.m_data.tracks do
     begin
      Blocks.GetBlkByID(trackZav, TBlk(usek));
      usek.Zaver := TZaver.no;
     end;//for cyklus

    signal.privol := Self;

    // i pokud je navetidlo ve STUJ, nastavuji navest (to je spravne chovani podle JOP)
    if ((Self.typ = TJCType.train) and (signal.enabled)) then
     begin
      Self.SetSignalSignal();
      Self.Log('Krok 102 : navestidlo: nastavuji na privolavaci navest...');
      Self.step := _NC_STEP_CEKANI_NAVESTIDLO;
     end else
      Self.step := _NC_STEP_FINISH;
   end;

   _NC_STEP_CEKANI_NAVESTIDLO: begin
     if (signal.signal = ncPrivol) then
      begin
       Self.Log('Krok 103 : navestidlo postaveno');
       Self.step := _NC_STEP_FINISH;
      end;
   end;

   _NC_STEP_FINISH: begin
    Self.CancelSignalBegin();
    Self.CancelVBs();
    Self.CancelTrackEnd();

    Self.step := _STEP_DEFAULT;

    // pokud je cesta ze zasobniku, smazeme ji odtam
    if (Self.m_state.from_stack <> nil) then
     begin
      (Self.m_state.from_stack as TORStack).RemoveJC(Self);
      Self.m_state.from_stack := nil;
     end;

    // presun soupravy z useku pred navestidlem do posledniho useku JC

    // Presun probehne za techto podminek:
    //  a) Bud privolavame do stanice = na dopravni kolej
    //  b) Nebo privolavame do trate, ktera MUSI byt ve spravnem smeru a MUSI v ni byt zavedena blokova podminka

    if (Self.typ = TJCType.train) then
     begin
      usek := signal.track as TBlkTrack;
      train := Self.GetTrain(signal, usek);

      // a)
      if ((lastTrack.typ = btTrack) and (TBlkTrack(Self.lastTrack).spnl.stationTrack) and
          (not TBlkTrack(Self.lastTrack).TrainsFull())) then
       begin
        if (usek.IsTrain()) then
         begin
          if ((usek.typ = btRT) and (TBlkRT(usek).inRailway > -1)) then
           begin
            Blocks.GetBlkByID((usek as TBlkRT).inRailway, TBlk(railway));
            railway.RemoveTrain(train);
           end;

          // na dopravni kolej vlozime soupravu blize vjezdovemu navestidlu
          if (signal.direction = THVSite.odd) then
            TBlkTrack(Self.lastTrack).AddTrainL(train)
          else
            TBlkTrack(Self.lastTrack).AddTrainS(train);

          usek.RemoveTrain(train);
          train.front := Self.lastTrack;
         end;
        Self.m_state.destroyBlock := _JC_DESTROY_NC;
       end;

      // b)
      if ((lastTrack.typ = btRT) and ((lastTrack as TBlkRT).inRailway > -1)) then
        Blocks.GetBlkByID((lastTrack as TBlkRT).inRailway, TBlk(railway))
      else
        railway := nil;

      if ((railway <> nil) and (usek.IsTrain()) and (lastTrack.typ = btRT) and
          ((lastTrack as TBlkRT).inRailway = Self.data.railwayId)) then
       begin
        tuAdd := nil;

        if (railway.lockout) then
         begin
          // Pridat soupravu do posledniho bloku trati
          if ((railway.state.trains.Count = 0) and ((railway.GetLastTrack(Self.data.railwayDir) as TBlkRT).zaver = TZaver.no)) then
           begin
            tuAdd := (railway.GetLastTrack(Self.data.railwayDir) as TBlkRT);
            railway.TrainChangeOR(train, Self.data.railwayDir);
            if (railway.ChangesTrainDir()) then
              train.ChangeDirection();
          end;
         end else begin
          if ((not TBlkTrack(Self.lastTrack).IsTrain()) and (railway.BP) and (railway.direction = Self.data.railwayDir)) then
           begin
            // Pridat soupravu do prvniho bloku trati
            tuAdd := (lastTrack as TBlkRT);
            tuAdd.bpError := true;
           end;
         end;

        if (tuAdd <> nil) then
         begin
          railway.AddTrain(TBlkRailwayTrain.Create(train.index));
          tuAdd.AddTrainL(train); // tady je jedno jestli zavolat L nebo S
                                   // v trati muze byt na jednom useku vzdy jen jedna souprava
                                   // kontrolovano vyse
          railway.Change();
          usek.RemoveTrain(train);
          train.front := tuAdd;
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
procedure TJC.CancelActivating(reason: string = ''; stack_remove: Boolean = false);
var trackZaver: Integer;
    track: TBlkTrack;
begin
 if (reason <> '') then
  begin
   if (Self.m_state.senderPnl <> nil) then
     PanelServer.SendInfoMsg(Self.m_state.senderPnl, reason);
   Self.Log('Nelze postavit - '+reason);
  end;

 case (Self.step) of
    _NC_STEP_BARIERA_UPDATE: begin
      if (Self.m_state.senderPnl <> nil) then
        PanelServer.CSClose(Self.m_state.senderPnl, reason);
    end   
 end;//case Self.krok

 // staveci zavery jsou zruseny, ostatni zavery zustavaji (lze je vyNUZovat)
 for trackZaver in Self.data.tracks do
  begin
   Blocks.GetBlkByID(trackZaver, TBlk(track));
   if (track.Zaver = TZaver.staveni) then
      track.Zaver := TZaver.no;
  end;

 Self.m_state.nextTurnout := -1;
 Self.step := _STEP_DEFAULT;
 Self.m_state.nc := false;
 Self.m_state.ab := false;
 Self.m_state.crossingWasClosed := false;
 Self.CancelSignalBegin();
 Self.CancelVBs();
 Self.CancelTrackEnd();
 if (Self.m_state.senderPnl <> nil) then
   PanelServer.CancelUPO(Self.m_state.senderPnl, Self);
 if (Self.m_state.from_stack <> nil) then
    if (stack_remove) then (Self.m_state.from_stack as TORStack).RemoveJC(Self)
  else
   if (Self.m_state.senderOR <> nil) then
     (Self.m_state.senderOR as TArea).BroadcastData('ZAS;FIRST;1');

 Self.m_state.from_stack := nil;
end;

////////////////////////////////////////////////////////////////////////////////

//rusi zacatek jizdni cesty
procedure TJC.CancelSignalBegin();
var Blk: TBlk;
 begin
  Blocks.GetBlkByID(Self.m_data.signalId, Blk);
  if (Blk = nil) then Exit();
  if (Blk.typ <> btSignal) then Exit();
  if ((Blk as TBlkSignal).selected = TBlkSignalSelection.none) then Exit();

  (Blk as TBlkSignal).selected := TBlkSignalSelection.none;
  if ((Blk as TBlkSignal).DNjc = Self) then
    (Blk as TBlkSignal).DNjc := nil;

  Self.Log('Zrusen zacatek staveni VC na bloku '+Blk.name);
 end;

//rusi konec jizdni cesty
procedure TJC.CancelTrackEnd();
 begin
  if (Self.lastTrack <> nil) then
    TBlkTrack(Self.lastTrack).jcEnd := TZaver.no;
 end;

procedure TJC.CancelVBs();
var Blk: TBlk;
    vb: Integer;
begin
 for vb in Self.data.vb do
  begin
   Blocks.GetBlkByID(vb, Blk);
   if ((Blk <> nil) and ((Blk.typ = btTrack) or (Blk.typ = btRT))) then
     (Blk as TBlkTrack).jcEnd := TZaver.no;
  end; 
end;

////////////////////////////////////////////////////////////////////////////////

//ruseni jizdni cesty
procedure TJC.Cancel(Sender: TObject = nil);
var trackZaver: Integer;
    track: TBlkTrack;
    signal: TBlkSignal;
 begin
  Self.CancelWithoutTrackRelease();

  Blocks.GetBlkByID(Self.m_data.signalId, TBlk(signal));
  signal.DNjc := nil;
  signal.RCtimerTimeout();

  for trackZaver in Self.m_data.tracks do
   begin
    Blocks.GetBlkByID(trackZaver, TBlk(track));
    track.Zaver := TZaver.no;
   end;

  // zaver trati se rusi automaticky uvolnenim zaveru posledniho bloku pred trati

  Self.Log('Zrusena');
 end;

//ruseni jizdni cesty bez ruseni zaveru bloku
procedure TJC.CancelWithoutTrackRelease();
var signal: TBlk;
 begin
  Blocks.GetBlkByID(Self.m_data.signalId, signal);
  Self.Log('Probiha ruseni navesti');

  if (((signal as TBlkSignal).DNjc = self) and ((signal as TBlkSignal).signal > ncStuj)) then
   begin
    (signal as TBlkSignal).signal := ncStuj;
    if ((signal as TBlkSignal).AB) then
     begin
      (signal as TBlkSignal).AB := false; // automaticky zrusi AB
      if (Self.m_state.senderPnl <> nil) then
        PanelServer.BottomError(Self.m_state.senderPnl, 'Zrušena AB '+signal.name,
          (Self.m_state.senderOR as TArea).ShortName, 'TECHNOLOGIE');
     end;
   end;

  Self.step := _STEP_DEFAULT;
  Self.destroyBlock := _JC_DESTROY_NONE;
  Self.destroyEndBlock := _JC_DESTROY_NONE;
 end;

////////////////////////////////////////////////////////////////////////////////

//RozpadBlok = blok index, kam by mela souprava vjet
//RozpadRuseniBlok = blok index, kde je posledni detekovany vagon soupravy
procedure TJC.DynamicCanceling();
var signal: TBlkSignal;
    blk: TBlk;
    track, nextTrack: TBlkTrack;
    i: Integer;
    train: TTrain;
begin
 Blocks.GetBlkByID(Self.m_data.signalId, TBlk(signal));

 // kontrola obsazenosti useku pred navestidlem
 track := signal.track as TBlkTrack;
 if ((Self.destroyBlock = _JC_DESTROY_SIGNAL_TRACK) and ((track.occupied <> TTrackState.free) or
     (track.GetSettings.RCSAddrs.Count = 0))) then
  begin
   Self.destroyBlock := 0;
   Self.destroyEndBlock := _JC_DESTROY_SIGNAL_TRACK;
  end;

 // uvolneni prvniho useku pred navestidlem v posunove ceste je signalem pro zhasnuti navestidla
 if ((track.GetSettings().RCSAddrs.Count > 0) and (track.occupied = TTrackState.free) and
     (signal.signal <> ncStuj) and (Self.destroyEndBlock = _JC_DESTROY_SIGNAL_TRACK) and (Self.typ = TJCType.shunt) and
     (Self.destroyBlock >= 1)) then
  begin
   Self.Log('Uvolnen usek '+track.name+' : navestidlo '+ signal.name+' nastaveno na STUJ');
   signal.JCCancelSignal();
  end;


 for i := Self.destroyBlock to Self.m_data.tracks.Count-1 do
  begin
   if (i < 0) then continue;    // i = -1 kdyz se kontroluje blok pred navestidlem, -2 pokud je navestidlo na STUJ, nebo zamkle

   Blocks.GetBlkByID(Self.m_data.tracks[i], TBlk(track));

   // druha cast podminky je tu pro pripad, kdy by byl na konci posunove cesty obsazeny usek
   if ((track.occupied = occupied) and
       ((i < Self.m_data.tracks.Count-1) or (Self.destroyBlock > Self.m_data.tracks.Count-2) or (Self.typ <> TJCType.shunt))) then
    begin
     if (i = Self.destroyBlock) then
      begin
       track.Zaver := TZaver.nouz;

       if (Self.typ = TJCType.train) then
         Self.MoveTrainToNextTrack();

       // obsazeni useku rusiciho navest (obvykle 0. usek, u skupinoveho navestidla byva jiny)
       // pozor: toto musi byt na tomto miste kvuli nastavovani Souprava.front
       if ((i = Self.m_data.signalFallTrackI) and (signal.signal <> ncStuj) and (Self.typ = TJCType.train)) then
        begin
         // navestidlo pri obsazeni useku rusime jen v pripade, ze se jedna o VC
         Self.Log('Obsazen usek '+track.name+' : navestidlo '+signal.name+' nastaveno na STUJ');
         signal.JCCancelSignal();

         // aktualizace casu odjezdu v trati
         if (Self.m_data.railwayId > -1) then
          begin
           Blocks.GetBlkByID(Self.m_data.railwayId, blk);
           if (TBlkRailway(blk).TrainPredict <> nil) then
            begin
             TBlkRailway(blk).TrainPredict.time := timeHelper.hJOPnow();
             TBlkRailway(blk).TrainPredict.predict := false;
             TBlkRailway(blk).Change();
            end;
          end;
        end;

       Self.destroyBlock := Self.destroyBlock + 1;

       // pokud jsme v predposlednim useku a posledni je nedetekovany, posuneme RozpadBlok jeste o jeden usek, aby se cesta mohla zrusit
       if (i = Self.m_data.tracks.Count-2) then
         if (TBlkTrack(Self.lastTrack).GetSettings().RCSAddrs.Count = 0) then
           Self.destroyBlock := Self.destroyBlock + 1;

       if ((i = Self.m_data.tracks.Count-1) and (Self.m_data.railwayId > -1)) then
        begin
         // posledni usek JC obsazen -> trat
         Blocks.GetBlkByID(Self.m_data.railwayId, blk);

         if (Self.typ = TJCType.train) then
          begin
           (blk as TBlkRailway).BP := true;
           if (track.IsTrain()) then
            begin
             if (((blk as TBlkRailway).TrainPredict <> nil) and
                 ((blk as TBlkRailway).TrainPredict.train = track.train)) then
               (blk as TBlkRailway).AddTrain((blk as TBlkRailway).trainPredict)
             else
               (blk as TBlkRailway).AddTrain(TBlkRailwayTrain.Create(track.TrainI));
            end;
          end;
         (blk as TBlkRailway).Zaver := false;

         // nastavime rychlost souprave
         if (Self.typ = TJCType.train) then
           TBlkRT(track).speedUpdate := true;
        end;


      end else begin //if Self.rozpadBlok = 0
       if (track.Zaver > TZaver.no) then
        begin
         // pokud jsme na jinem useku, nez RozpadBlok
         if ((signal.targetSignal > ncStuj) and (signal.DNjc = Self)) then
          begin
           if (Self.m_state.senderPnl <> nil) and (Self.m_state.senderOR <> nil) then
             PanelServer.BottomError(Self.m_state.senderPnl, 'Chyba povolovací návěsti '+signal.name,
                (Self.m_state.senderOR as TArea).ShortName, 'TECHNOLOGIE');
           Self.CancelWithoutTrackRelease();
          end;

         // v trati zaver nerusime, nesmime tam dat ani nouzovy, ani zadny zaver
         if ((i <> Self.m_data.tracks.Count-1) or (Self.m_data.railwayId = -1)) then
           track.Zaver := TZaver.nouz;
        end;
      end;
    end;


   // kontrola zruseni jizdni cesty vlivem vynuzovani bloku
   if ((i = Self.destroyBlock) and ((track.Zaver = TZaver.no))) then
    begin
     // pokud usek, na ktery se chystam vkrocit, nema zaver, je neco divne -> zrusit JC (predevsim kvuli predavani loko, ktere by mohlo narusit dalsi JC)
     Self.CancelWithoutTrackRelease();
     Exit();
    end;

  end;//for i

  // jizdni cesta konci uvolnenim predposledniho useku

  // mensitko je dulezite a ma smysl !
  //  kdyby tam bylo <=, mohl by se rozpadnout jediny usek, na kterem je souprava tim, ze se odobsadi
  if ((Self.destroyEndBlock >= 0) and (Self.destroyEndBlock < Self.destroyBlock-1)) then
   begin
    //ziskani dotazovaneho useku
    Blocks.GetBlkByID(Self.m_data.tracks[Self.destroyEndBlock], TBlk(track));

    if (Self.destroyEndBlock+1 < Self.m_data.tracks.Count) then
      Blocks.GetBlkByID(Self.m_data.tracks[Self.destroyEndBlock+1], TBlk(nextTrack))
    else
      nextTrack := nil;

    if ((track.Zaver = TZaver.nouz) and (track.occupied = TTrackState.free) and
        ((nextTrack = nil) or (nextTrack.occupied = TTrackState.occupied) or
        (nextTrack.GetSettings.RCSAddrs.Count = 0))) then
     begin
      // cesta se rozpada...
      if (Self.AB) then
        track.Zaver := TZaver.AB
      else
        track.Zaver := TZaver.no;

      Self.destroyEndBlock := Self.destroyEndBlock + 1;

      if ((Self.typ = TJCType.train) and (track.IsTrain())) then
       begin
        Self.Log('Smazana souprava '+track.Train.name+' z bloku '+track.name, WR_SPRPREDAT);
        track.RemoveTrains();
       end;
     end;//if Self.rozpadBlok >= 1
   end;//if (cyklus2 = Self.rozpadRuseniBlok)

  // tady se resi pripad, kdy stanicni kolej zustane obsazena (protoze tam stoji vagony),
  // ale souprava se z ni musi odstranit uvolnenim prvniho bloku JC
  if ((Self.destroyEndBlock = _JC_DESTROY_SIGNAL_TRACK) and (Self.destroyBlock > 0)) then
   begin
    Blocks.GetBlkByID(Self.m_data.tracks[0], TBlk(track));

    if (Self.m_data.tracks.Count > 1) then
      Blocks.GetBlkByID(Self.m_data.tracks[1], TBlk(nextTrack))
    else
      nextTrack := nil;

    if ((track.Zaver = TZaver.nouz) and (track.occupied = TTrackState.free) and
        ((nextTrack = nil) or (nextTrack.occupied = TTrackState.occupied) or
        (nextTrack.GetSettings.RCSAddrs.Count = 0))) then
     begin
      // uvolneni prvniho useku v posunove ceste je signalem pro zhasnuti navestidla
      if ((signal.signal <> ncStuj) and (Self.typ = TJCType.shunt)) then
       begin
        Self.Log('Uvolnen usek '+track.name+' : navestidlo '+signal.name+' nastaveno na STUJ');
        signal.JCCancelSignal();
       end;

      if (Self.AB) then
        TBlkTrack(track).Zaver := TZaver.AB
      else
        TBlkTrack(track).Zaver := TZaver.no;

      Self.destroyEndBlock := 1;

      if ((Self.typ = TJCType.train) and (track.IsTrain())) then
       begin
        // mazani soupravy z useku pred navestidlem
        blk := TBlkSignal(signal).track;
        train := Self.GetTrain(signal, blk);
        if (train = TBlkTrack(track).train) then
         begin
          Self.Log('Smazana souprava '+train.name+' z bloku '+blk.name, WR_SPRPREDAT);
          (blk as TBlkTrack).RemoveTrain(train);
         end;

        Self.Log('Smazana souprava '+train.name+' z bloku '+track.name, WR_SPRPREDAT);
        track.RemoveTrains();
       end;
     end;
   end;

  // mazani soupravy z useku pred navestidlem
  if ((Self.destroyBlock > 0) and (Self.destroyEndBlock = _JC_DESTROY_SIGNAL_TRACK)) then
   begin
    track := signal.track as TBlkTrack;
    if ((track.occupied = TTrackState.free) and (track.GetSettings.RCSAddrs.Count > 0)) then
     begin
      if (track.IsTrain() and (Self.typ = TJCType.train)) then
       begin
        train := Self.GetTrain(signal, track);
        track.RemoveTrain(train);
        Self.Log('Smazana souprava '+train.name+' z bloku '+track.name, WR_SPRPREDAT);
       end;

      Self.destroyEndBlock := 0;

      if ((track.typ = btRT) and (TBlkRT(track).railway <> nil) and (TBlkRT(track).bpInBlk)) then
        TBlkRT(track).ReleasedFromJC();
     end;
   end;

  track := signal.track as TBlkTrack;
  if ((Self.destroyBlock = 0) and (Self.destroyEndBlock = _JC_DESTROY_SIGNAL_TRACK) and
      (TBlkTrack(track).occupied <> TTrackState.occupied)) then
   begin
    // usek pred navestidlem se opet uvolnil
    Self.destroyBlock := _JC_DESTROY_SIGNAL_TRACK;
    Self.destroyEndBlock := _JC_DESTROY_SIGNAL_STUJ;
   end;


  // tahleta silenost za OR je tu pro pripad, kdy JC ma jen jeden usek (to se stava napriklad na smyckach)
  if ((Self.destroyEndBlock = Self.m_data.tracks.Count-1) and (Self.m_data.tracks.Count > 1))
      or ((Self.m_data.tracks.Count = 1) and (Self.destroyBlock = 1)) then
   begin
    // vsechny useky az na posledni jsou uvolneny -> rusime JC

    // tady by teoreticky melo prijit ruseni zaveru posledniho bloku, ale to neni poteba,
    // protoze zaver tohoto bloku je primo navazny na zaver predposledniho bloku pres redukce
    // to je napriklad kvuli tratim, ci z toho duvodu, ze na stanicnich kolejich nejde dat NUZ

    // pozor ale na JC, ktere maji jen jeden usek a ten je stanicni koleji:
    if (Self.m_data.tracks.Count = 1) then
     begin
      Blocks.GetBlkByID(Self.m_data.tracks[0], TBlk(track));

      if (Self.AB) then
        track.Zaver := TZaver.AB
      else
        track.Zaver := TZaver.no;

      track := signal.track as TBlkTrack;
      train := Self.GetTrain(signal, track);

      // pokud ma cesta jen jeden usek, odstranime soupravu z useku pred navestidlem:
      if ((Self.typ = TJCType.train) and (train <> nil)) then
       begin
        track.RemoveTrain(train);
        Self.Log('Smazana souprava '+train.name+' z bloku '+track.name, WR_SPRPREDAT);
       end;

      if ((track.typ = btRT) and (TBlkRT(track).railway <> nil) and (TBlkRT(track).bpInBlk)) then
        TBlkRT(track).ReleasedFromJC();
     end;

    Self.destroyBlock := _JC_DESTROY_NONE;
    Self.destroyEndBlock := _JC_DESTROY_NONE;
    Self.Log('Ruseni: rozpad cesty vlakem');
    if (signal.DNjc = Self) then
     begin
      if (signal.signal > ncStuj) then      // tato situace opravdu muze nastat - predstavte si posunovou cestu s jednim usekem vychazejici z nedetek koleje
        signal.JCCancelSignal();
      signal.DNjc := nil;
     end;
   end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TJC.NonProfileOccupied();
var signal: TBlk;
begin
 if (Self.activating) then
  begin
   Self.CancelActivating('Nelze postavit - obsazen neprofilový úsek');
  end else begin
   Blocks.GetBlkByID(Self.m_data.signalId, signal);
   if (((signal as TBlkSignal).signal > ncStuj) and ((signal as TBlkSignal).DNjc = Self)) then
    begin
     if (Self.m_state.senderPnl <> nil) and (Self.m_state.senderOR <> nil) then
       PanelServer.BottomError(Self.m_state.senderPnl, 'Chyba povolovací návěsti '+signal.name,
            (Self.m_state.senderOR as TArea).ShortName, 'TECHNOLOGIE');
     Self.CancelWithoutTrackRelease();
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TJC.DynamicCancelingNC();
var railwayTrack, first : TBlkTrack;
    signal : TBlkSignal;
begin
 Blocks.GetBlkByID(Self.m_data.signalId, TBlk(signal));
 railwayTrack := TBlkRT((signal as TBlkSignal).track);
 Blocks.GetBlkByID(Self.m_data.tracks[0], TBlk(first));

 if ((first.occupied = TTrackState.occupied) and (railwayTrack.occupied = TTrackState.free)
    and (not railwayTrack.IsTrain())) then
  begin
   if (TBlkRT(railwayTrack).bpInBlk) then
     TBlkRT(railwayTrack).ReleasedFromJC();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

//preda soupravu v jizdni ceste dalsimu bloku v poradi
procedure TJC.MoveTrainToNextTrack();
var trackActual, trackNext, signal: TBlk;
    train: TTrain;
 begin
  if (Self.destroyBlock = 0) then
   begin
    Blocks.GetBlkByID(Self.m_data.signalId, signal);
    trackActual := (signal as TBlkSignal).track;
    train := Self.GetTrain(signal, trackActual);
    if ((trackActual as TBlkTrack).IsTrain()) then
      if (train.front <> trackActual) then
         Exit();
   end else begin
    Blocks.GetBlkByID(Self.m_data.tracks[Self.destroyBlock-1], trackActual);
    train := TBlkTrack(trackActual).train;
   end;

  Blocks.GetBlkByID(Self.m_data.tracks[Self.destroyBlock], trackNext);
  if (not (trackActual as TBlkTrack).IsTrain()) then Exit();

  (trackNext as TBlkTrack).slowingReady := true;
  (trackNext as TBlkTrack).AddTrainL(train);
  (trackNext as TBlkTrack).train.front := trackNext;
  (trackNext as TBlkTrack).houkEvEnabled := true;
  Self.Log('Predana souprava '+(trackNext as TBlkTrack).train.name+
      ' z bloku '+trackActual.name+' do bloku '+trackNext.name, WR_SPRPREDAT);

  Self.CheckLoopBlock(trackNext);
 end;

procedure TJC.CheckLoopBlock(blk: TBlk);
var area: TArea;
begin
 if (((Blk as TBlkTrack).GetSettings().loop) and ((Blk as TBlkTrack).IsTrain())) then
  begin
   // kontrola zmeny vychozi a cilove stanice
   for area in blk.areas do
    begin
     if (area = (Blk as TBlkTrack).train.stationTo) then
      begin
       (Blk as TBlkTrack).train.InterChangeStanice(false);
       break;
      end;
    end;

   (Blk as TBlkTrack).train.ChangeDirection();
   Self.Log('Obsazen smyckovy usek '+Blk.name+ ' - menim smer loko v souprave '+
      (Blk as TBlkTrack).train.name, WR_SPRPREDAT);
  end;//if
end;

////////////////////////////////////////////////////////////////////////////////

//nastavi navestidlo JC na pozadovanou navest
procedure TJC.SetSignalSignal();
var sugnal, nextSignal: TBlkSignal;
    code: TBlkSignalCode;
 begin
  Blocks.GetBlkByID(Self.m_data.signalId, TBlk(sugnal));

  code := ncStuj;

  if ((Self.m_state.nc) and (Self.typ = TJCType.train)) then
   begin
    // nouzova cesta
    code := ncPrivol;
   end else begin

    case (Self.typ) of
     TJCType.shunt : begin
      // posunova cesta
      code := ncPosunZaj;
     end;//case posun

     TJcType.train : begin
      Blocks.GetBlkByID(Self.m_data.nextSignalId, TBlk(nextSignal));
      if ((Self.m_data.nextSignalType = TJCNextSignalType.no) or
          (Self.m_data.nextSignalType = TJCNextSignalType.railway) or
          ((Self.m_data.nextSignalType = TJCNextSignalType.signal) and
           ((nextSignal <> nil) and (nextSignal.IsGoSignal()) and (not nextSignal.IsOpakVystraha())))) then
       begin
        // na dalsim navestidle je navest povolujici jizdu (vyjma PN)
        if (Self.data.turn) then
         begin
          if ((Self.m_data.nextSignalType = TJCNextSignalType.signal) and
              ((nextSignal.signal = ncOpakOcek40) or (nextSignal.FourtyKmph()))) then
            code := nc40Ocek40
          else
            code := ncVolno40;
         end else begin
          if ((Self.m_data.nextSignalType = TJCNextSignalType.signal) and
              ((nextSignal.signal = ncOpakOcek40) or (nextSignal.FourtyKmph()))) then
            code := ncOcek40
          else
            code := ncVolno;
         end;

       end else begin
        // na dalsim navestidle je STUJ nebo opakoveni navesti vystraha (to je pro nas jako stuj)

        if (Self.data.turn) then
          code := ncVystraha40
        else
          code := ncVystraha;
       end;

      if ((Self.m_data.nzv) and (code <> ncVolno)) then
        code := TBlkSignal.AddOpak(code);
     end;//case vlak

     end;//case
   end;// else nouzova cesta

  sugnal.SetSignal(code, TNotifyEvent(nil), Self.SignalError);
 end;

////////////////////////////////////////////////////////////////////////////////

procedure TJC.LoadData(ini: TMemIniFile; section: string);
var sl, sl2: TStrings;
    i, j, cnt: Integer;
    turnoutZav: TJCTurnoutZav;
    refugeeZav: TJCRefugeeZav;
    refZav: TJCRefZav;
    crossingZav: TJCCrossingZav;
    sect_size: Integer;
begin
 Self.m_data.name := ini.ReadString(section, 'nazev', section);
 Self.m_data.id := StrToInt(section);
 Self.m_data.signalId := ini.ReadInteger(section, 'nav', -1);
 Self.m_data.typ := TJCType(ini.ReadInteger(section, 'typ', -1));
 Self.m_data.nextSignalType := TJCNextSignalType(ini.ReadInteger(section, 'dalsiNTyp', 0));
 Self.m_data.nextSignalId := ini.ReadInteger(section, 'dalsiN', 0);
 Self.m_data.speedGo := ini.ReadInteger(section, 'rychDalsiN', 0)*10;
 Self.m_data.speedStop := ini.ReadInteger(section, 'rychNoDalsiN', 0)*10;
 Self.m_data.railwayId := ini.ReadInteger(section, 'trat', -1);
 Self.m_data.railwayDir := TRailwayDirection(ini.ReadInteger(section, 'tratSmer', 0));

 // nacteni zaveru useku:
 sl := TStringList.Create();
 sl2 := TStringList.Create();
 try
   ExtractStrings([';', ',', '|', '-', '('], [')'], PChar(ini.ReadString(section, 'useky', '')), sl);
   Self.m_data.tracks.Count := sl.Count;
   for i := 0 to Self.m_data.tracks.Count-1 do
     Self.m_data.tracks[i] := StrToInt(sl[i]);

   // nacteni zaveru vyhybek:
   sl.Clear();
   ExtractStrings([';', ',', '|', '-', '(', ')'], [], PChar(ini.ReadString(section, 'vyhybky', '')), sl);
   sect_size := 2;
   cnt := (sl.Count div sect_size);
   Self.m_data.turnouts.Clear();
   for i := 0 to cnt-1 do
    begin
     turnoutZav.block := StrToInt(sl[i*sect_size]);
     turnoutZav.position := TTurnoutPosition(StrToInt(sl[(i*sect_size)+1]));
     Self.m_data.turnouts.Add(turnoutZav);
    end;//for i

   // nacteni odvratu:
   sl.Clear();
   ExtractStrings([';', ',', '|', '-', '(', ')'], [], PChar(ini.ReadString(section, 'odvraty', '')), sl);
   sect_size := 3;
   cnt := (sl.Count div sect_size);
   Self.m_data.refuges.Clear();
   for i := 0 to cnt-1 do
    begin
     refugeeZav.block := StrToInt(sl[i*sect_size]);
     refugeeZav.position := TTurnoutPosition(StrToInt(sl[(i*sect_size)+1]));
     refugeeZav.ref_blk := StrToInt(sl[(i*sect_size)+2]);
     Self.m_data.refuges.Add(refugeeZav);
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

     crossingZav.crossingId := StrToInt(sl2[0]);
     if (sl2.Count > 1) then
       crossingZav.openTrack := StrToInt(sl2[1])
     else
       crossingZav.openTrack := -1;

     crossingZav.closeTracks := TList<Integer>.Create();
     for j := 2 to sl2.Count-1 do
       crossingZav.closeTracks.Add(StrToInt(sl2[j]));

     Self.m_data.crossings.Add(crossingZav);
    end;//for i

   // nacteni podminek zamku:
   sl.Clear();
   ExtractStrings(['(', ')'], [], PChar(ini.ReadString(section, 'podm-zamky', '')), sl);
   Self.m_data.locks.Clear();
   for i := 0 to sl.Count-1 do
    begin
     sl2.Clear();
     ExtractStrings([';', ',', '|', '-'], [], PChar(sl[i]), sl2);

     refZav.block := StrToInt(sl2[0]);
     refZav.ref_blk := StrToInt(sl2[1]);
     Self.m_data.locks.Add(refZav);
    end;//for i

   // nacteni variantnich bodu
   sl.Clear();
   ExtractStrings([';', ',', '|', '-', '(', ')'], [], PChar(ini.ReadString(section, 'vb', '')), sl);
   for i := 0 to sl.Count-1 do
     Self.m_data.vb.Add(StrToInt(sl[i]));

 finally
   sl.Free();
   sl2.Free();
 end;

 if (ini.ValueExists(section, 'odbocka')) then
   Self.m_data.turn := ini.ReadBool(section, 'odbocka', false)
 else
   Self.m_data.turn := Self.IsAnyTurnoutMinus();

 Self.m_data.nzv := ini.ReadBool(section, 'nzv', false);
 Self.m_data.signalFallTrackI := ini.ReadInteger(section, 'rusNavestUsek', 0);
end;

procedure TJC.SaveData(ini: TMemIniFile; section: string);
var line: string;
    i, j: Integer;
begin
 ini.WriteString (section, 'nazev', Self.m_data.name);
 ini.WriteInteger(section, 'nav', Self.m_data.signalId);
 ini.WriteInteger(section, 'typ', Integer(Self.m_data.typ));
 if (Self.m_data.nextSignalType <> TJCNextSignalType.no) then
   ini.WriteInteger(section, 'dalsiNTyp', Integer(Self.m_data.nextSignalType));
 if (Self.m_data.nextSignalType = TJCNextSignalType.signal) then
   ini.WriteInteger(section, 'dalsiN', Self.m_data.nextSignalId);
 ini.WriteInteger(section, 'rychDalsiN', Self.m_data.speedGo div 10);
 ini.WriteInteger(section, 'rychNoDalsiN', Self.m_data.speedStop div 10);

 if (Self.m_data.turn = Self.IsAnyTurnoutMinus) then
   ini.DeleteKey(section, 'odbocka')
 else
   ini.WriteBool(section, 'odbocka', Self.m_data.turn);

 if (not Self.m_data.nzv) then
   ini.DeleteKey(section, 'nzv')
 else
   ini.WriteBool(section, 'nzv', true);

 if (Self.m_data.signalFallTrackI = 0) then
   ini.DeleteKey(section, 'rusNavestUsek')
 else
   ini.WriteInteger(section, 'rusNavestUsek', Self.m_data.signalFallTrackI);

 if (Self.m_data.railwayId > -1) then
  begin
   ini.WriteInteger(section, 'trat', Self.m_data.railwayId);
   ini.WriteInteger(section, 'tratSmer', Integer(Self.m_data.railwayDir));
  end;

 // useky
 line := '';
 for i := 0 to Self.m_data.tracks.Count-1 do
   line := line + IntToStr(Self.m_data.tracks[i]) + ',';
 if (line <> '') then
   ini.WriteString(section, 'useky', line);

 // vyhybky
 line := '';
 for i := 0 to Self.m_data.turnouts.Count-1 do
   line := line + '(' + IntToStr(Self.m_data.turnouts[i].block) + ',' + IntToStr(Integer(Self.m_data.turnouts[i].position)) + ')';
 if (line <> '') then
   ini.WriteString(section, 'vyhybky', line);

 // odvraty
 line := '';
 for i := 0 to Self.m_data.refuges.Count-1 do
   line := line + '(' + IntToStr(Self.m_data.refuges[i].block) + ',' + IntToStr(Integer(Self.m_data.refuges[i].position)) + ',' + IntToStr(Self.m_data.refuges[i].ref_blk)+ ')';
 if (line <> '') then
   ini.WriteString(section, 'odvraty', line);

 // prejezdy
 line := '';
 for i := 0 to Self.m_data.crossings.Count-1 do
  begin
   line := line + '(' + IntToStr(Self.m_data.crossings[i].crossingId);

   if (Self.m_data.crossings[i].closeTracks.Count > 0) then
    begin
     line := line + ',' + IntToStr(Self.m_data.crossings[i].openTrack)+ ',';
     for j := 0 to Self.m_data.crossings[i].closeTracks.Count-1 do
       line := line + IntToStr(Self.m_data.crossings[i].closeTracks[j]) + ',';
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
 for i := 0 to Self.m_data.locks.Count-1 do
   line := line + '(' + IntToStr(Self.m_data.locks[i].block) + ';' + IntToStr(Self.m_data.locks[i].ref_blk) + ')';
 if (line <> '') then
   ini.WriteString(section, 'podm-zamky', line);

 // variantni body
 line := '';
 for i := 0 to Self.m_data.vb.Count-1 do
   line := line + IntToStr(Self.m_data.vb[i]) + ';';
 if (line <> '') then
   ini.WriteString(section, 'vb', line);
end;

////////////////////////////////////////////////////////////////////////////////

// timeout staveni JC
procedure TJC.UpdateTimeOut();
var crossing: TBlkCrossing;
    crossingZav: TJCCrossingZav;
begin
 // na nouzovou cestu se nevztahuje timeout
 if (not Self.activating) then Exit();

 if (Now > Self.m_state.timeOut) then
  begin
   case (Self.step) of
    _STEP_POTVR_SEKV: begin
     if (Self.m_state.senderPnl <> nil) and (Self.m_state.senderOR <> nil) then
       PanelServer.CSClose(Self.m_state.senderPnl);
    end;
    _JC_STEP_CEKANI_PREJEZDY: begin
      // prejezd(y) neuzavren
      for crossingZav in Self.m_data.crossings do
       begin
        Blocks.GetBlkByID(crossingZav.crossingId, TBlk(crossing));
        if (crossing.state <> TBlkCrossingBasicState.closed) then
          if (Self.m_state.senderPnl <> nil) and (Self.m_state.senderOR <> nil) then
            PanelServer.BottomError(Self.m_state.senderPnl, 'Neuzavřen '+crossing.name,
              (Self.m_state.senderOR as TArea).ShortName, 'TECHNOLOGIE');
       end;//for i
    end;//case 13

   else
     if (Self.m_state.senderPnl <> nil) and (Self.m_state.senderOR <> nil) then
       PanelServer.BottomError(Self.m_state.senderPnl, 'Timeout '+Self.name,
         (Self.m_state.senderOR as TArea).ShortName, 'TECHNOLOGIE');
   end;//else case

   //timeout
   Self.CancelActivating('Překročení času stavění JC', true);    // toto je docasne reseni: cestu vymazeme ze zasobniku
  end;//if timeout
end;

////////////////////////////////////////////////////////////////////////////////

function TJC.IsActivating(): Boolean;
begin
 Result := ((Self.step > _STEP_DEFAULT) and (Self.step <> _JC_STEP_CEKANI_POSLEDNI_USEK));
end;

function TJC.IsActive(): Boolean;
begin
 Result := (Self.m_state.destroyBlock > _JC_DESTROY_NONE);
end;

////////////////////////////////////////////////////////////////////////////////

// true = je mozno DN
//tato funkce kontroluje, jestli je mozne znovupostavit cestu i kdyz byla fakticky zrusena = musi zkontrolovat vsechny podminky
function TJC.CanDN(): Boolean;
var i: Integer;
    train: TTrain;
    trackZaver: Integer;
    vyhZaver: TJCTurnoutZav;
    odvratZaver: TJCRefugeeZav;
    crossingZaver: TJCCrossingZav;
    refZaver: TJCRefZav;
    track: TBlkTrack;
    turnout: TBlkTurnout;
    crossing: TBlkCrossing;
    railway: TBlkRailway;
    lock: TBlkLock;
begin
 // index soupravy na useku pred navestidlem
 train := Self.GetTrain();

 // zkontrolujeme zavery bloku
 // JC NELZE obnovit z useku, na kterych uplne spadl zaver (do zadneho zaveru)
 // porusily by se reference na redukce menu
 for i := 0 to Self.m_data.tracks.Count-1 do
  begin
   trackZaver := Self.m_data.tracks[i];
   Blocks.GetBlkByID(trackZaver, TBlk(track));
   if ((track.Zaver = TZaver.no) or (track.Zaver = TZaver.staveni) or (track.NUZ) or
      ((track.occupied <> TTrackState.free) and
       ((Self.typ = TJCType.train) or (i <> Self.m_data.tracks.Count-1)))) then Exit(false);

   // na usecich v ceste je dovoleno mit soupravu pred navestidlem, v takovem
   // pripade ji DN z useku v ceste smaze

   if (Self.typ = TJCType.train) then
    begin
     if (train = nil) then
      begin
       // pred navestidlem neni souprava -> na usecich nesmi byt zadna souprava
       if (track.IsTrain()) then Exit(false);
      end else begin
       // pred navestidlem je souprava -> na usecich smi byt jen stejna souprava
       // jako pred navestidlem
       if ((track.IsTrain()) and
           ((track.trains.Count > 1) or (track.train <> train))) then
         Exit(false);
      end;
    end;
  end;//for i

 // zkontrolujeme polohu vyhybek
 for vyhZaver in Self.m_data.turnouts do
  begin
   Blocks.GetBlkByID(vyhZaver.block, TBlk(turnout));
   if (turnout.position <> vyhZaver.position) then Exit(false);

   // kontrola neprofiloveho styku pro polohu +
   if ((vyhZaver.position = TTurnoutPosition.plus) and (turnout.npBlokPlus <> nil) and
       (TBlkTrack(turnout.npBlokPlus).occupied <> TTrackState.free)) then
     Exit(false);

   // kontrola neprofiloveho styku pro polohu -
   if ((vyhZaver.position = TTurnoutPosition.minus) and (turnout.npBlokMinus <> nil) and
       (TBlkTrack(turnout.npBlokMinus).occupied <> TTrackState.free)) then
     Exit(false);
  end;//for i

 // zkontrolujeme polohu odvratu
 for odvratZaver in Self.m_data.refuges do
  begin
   Blocks.GetBlkByID(odvratZaver.block, TBlk(turnout));
   if (turnout.position <> odvratZaver.position) then Exit(false);
  end;//for i

 // zkontrolujeme poruchy prejezdu
 //  prejezdy, na kterych je zaver, by taky mely byt uzavrene
 for crossingZaver in Self.m_data.crossings do
  begin
   Blocks.GetBlkByID(crossingZaver.crossingId, TBlk(crossing));
   if ((crossing.state = TBlkCrossingBasicState.none) or
      (crossing.state = TBlkCrossingBasicState.disabled)) then Exit(false);
   if ((crossing.Zaver) and (crossing.state <> TBlkCrossingBasicState.closed)) then Exit(false);
  end;//for i

 //zkontrolujeme trat
 if (Self.m_data.railwayId > -1) then
  begin
   Blocks.GetBlkByID(Self.m_data.railwayId, TBlk(railway));
   if (railway.request) then Exit(false);
   if ((((not (TBlkRT(Self.lastTrack).sectReady)) or (railway.departureForbidden)) and (Self.typ = TJCType.train)) or
       (railway.RBPCan) or (railway.direction <> Self.m_data.railwayDir)) then
     Exit(false);
  end;

  // kontrola uzamceni zamku:
  for refZaver in Self.m_data.locks do
   begin
    Blocks.GetBlkByID(refZaver.block, TBlk(lock));

    // kontrola uzamceni
    if (lock.keyReleased) then
      Exit(false);
   end;//for i

 Result := true;
end;

// DN provede zbytek staveni JC (prejezdy, finalizace)
// tato procedura predpoklada, ze podminky pro DN jsou splneny
procedure TJC.DN();
begin
 Self.Log('DN');
 Self.m_state.timeOut := Now + EncodeTime(0, _JC_TIMEOUT_SEC div 60, _JC_TIMEOUT_SEC mod 60, 0);

 if (Self.m_state.crossingWasClosed) then
   Self.step := _JC_STEP_FINALNI_ZAVER
 else
   Self.step := _JC_STEP_ZAVRIT_PREJEZDY;
end;

////////////////////////////////////////////////////////////////////////////////

// volano z navestidla pri STUJ
// nevolat nidky jindy !
procedure TJC.STUJ();
begin
 Self.destroyBlock := _JC_DESTROY_SIGNAL_STUJ;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TJC.TrackCloseCrossing(Sender: TObject; data: Integer);
var blkId: Integer;
    crossing: TBlkCrossing;
    track: TBlkTrack;
begin
 if ((Self.active) or (Self.activating)) then
  begin
   // zavrit prejezd
   Blocks.GetBlkByID(Self.m_data.crossings[data].crossingId, TBlk(crossing));
   crossing.Zaver := true;
   Self.Log('Obsazen '+TBlkTrack(Sender).name+' - uzaviram prejezd '+crossing.name);

   // prejezd se uzavira -> po uvolneni zaveru bloku pod prejezdem prejezd opet otevrit
   Blocks.GetBlkByID(Self.m_data.crossings[data].openTrack, TBlk(track));
   track.AddChangeEvent(track.eventsOnZaverReleaseOrAB,
     CreateChangeEvent(ceCaller.NullPrejezdZaver, Self.m_data.crossings[data].crossingId));
  end;

 for blkId in Self.m_data.crossings[data].closeTracks do
  begin
   Blocks.GetBlkByID(blkId, TBlk(track));
   track.RemoveChangeEvent(track.eventsOnOccupy, CreateChangeEvent(Self.TrackCloseCrossing, data));
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TJC.SetDestroyBlock(destroyBlock: Integer);
begin
 Self.m_state.destroyBlock := destroyBlock;
 Self.changed := true;
end;

procedure TJC.SetDestroyEndBlock(destroyEndBlock: Integer);
begin
 Self.m_state.destroyEndBlock := destroyEndBlock;
 Self.changed := true;
end;

procedure TJC.SetStep(step: Integer);
begin
 Self.m_state.step := step;
 Self.changed := true;
end;

////////////////////////////////////////////////////////////////////////////////

class function TJC.JCBarrier(typ: Integer; block: TBlk = nil; param: Integer = 0): TJCBarrier;
begin
 Result.typ   := typ;
 Result.block  := block;
 Result.param := param;
end;

////////////////////////////////////////////////////////////////////////////////

function TJC.JCBarrierToMessage(Bariera: TJCBarrier): TUPOItem;
var i: Integer;
    lines: TStrings;
    canZAK: Boolean;
    Blk: TBlk;
begin
 for i := 0 to _UPO_LINES-1 do
  begin
   Result[i].str := '';
   Result[i].fg  := clNone;
   Result[i].bg  := clNone;
  end;


 case (Bariera.typ) of
  _JCB_BLOK_DISABLED, _JCB_BLOK_NOT_TYP, _JCB_NAV_NOT_USEK, _JCB_NAV_NAVEST, _JCB_BLOK_NOT_EXIST,
  _JCB_USEK_OBSAZENO, _JCB_USEK_ZAVER, _JCB_USEK_AB, _JCB_USEK_SOUPRAVA,
  _JCB_VYHYBKA_KONC_POLOHA, _JCB_VYHYBKA_ZAMCENA, _JCB_VYHYBKA_NOUZ_ZAVER,
  _JCB_PREJEZD_NOUZOVE_OTEVREN, _JCB_PREJEZD_PORUCHA,
  _JCB_ODVRAT_ZAMCENA, _JCB_ODVRAT_OBSAZENA, _JCB_ODVRAT_KONC_POLOHA,
  _JCB_TRAT_ZAVER, _JCB_TRAT_NEPRIPRAVENA, _JCB_TRAT_ZADOST, _JCB_TRAT_NESOUHLAS,
  _JCB_ZAMEK_NEUZAMCEN, _JCB_VYHYBKA_NESPAVNA_POLOHA:
  begin
    Result[0] := GetUPOLine('NEPŘÍPUSTNÉ', taCenter, clRed, clWhite);
    if (Assigned(Bariera.block)) then
      Result[2] := GetUPOLine(Bariera.block.name)
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
  _JCB_NAV_NAVEST              : Result[1] := GetUPOLine('Není základní návěst');

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
    Result[2] := GetUPOLine(Bariera.block.name);
  end;

  _JCB_ZAMEK_NEUZAMCEN         : Result[1] := GetUPOLine('Neuzamčen');
  _JCB_ZAMEK_NOUZ_ZAVER        : Result[1] := GetUPOLine('Není nouzový závěr');

  _JCB_USEK_VYLUKA : begin
    Result[0] := GetUPOLine('VÝLUKA '+Bariera.block.name, taCenter, clBlack, clOlive);
    lines := GetLines((Bariera.block as TBlkTrack).lockout, _UPO_LINE_LEN);
    try
      Result[1] := GetUPOLine(lines[0], taLeftJustify, clYellow, $A0A0A0);
      if (lines.Count > 1) then
        Result[2] := GetUPOLine(lines[1], taLeftJustify, clYellow, $A0A0A0);
    finally
      lines.Free();
    end;
  end;

  _JCB_USEK_STITEK : begin
    Result[0] := GetUPOLine('ŠTÍTEK '+Bariera.block.name, taCenter, clBlack, clTeal);
    lines := GetLines(TBlkTrack(Bariera.block).note, _UPO_LINE_LEN);
    try
      Result[1] := GetUPOLine(lines[0], taLeftJustify, clYellow, $A0A0A0);
      if (lines.Count > 1) then
        Result[2] := GetUPOLine(lines[1], taLeftJustify, clYellow, $A0A0A0);
    finally
      lines.Free();
    end;
  end;

  _JCB_VYHYBKA_VYLUKA : begin
    Result[0] := GetUPOLine('VÝLUKA '+Bariera.block.name, taCenter, clBlack, clOlive);
    lines := GetLines(TBlkTurnout(Bariera.block).lockout, _UPO_LINE_LEN);
    try
      Result[1] := GetUPOLine(lines[0], taLeftJustify, clYellow, $A0A0A0);
      if (lines.Count > 1) then
        Result[2] := GetUPOLine(lines[1], taLeftJustify, clYellow, $A0A0A0);
    finally
      lines.Free();
    end;
  end;

  _JCB_VYHYBKA_STITEK : begin
    Result[0] := GetUPOLine('ŠTÍTEK '+Bariera.block.name, taCenter, clBlack, clTeal);
    lines := GetLines(TBlkTurnout(Bariera.block).note, _UPO_LINE_LEN);
    try
      Result[1] := GetUPOLine(lines[0], taLeftJustify, clYellow, $A0A0A0);
      if (lines.Count > 1) then
        Result[2] := GetUPOLine(lines[1], taLeftJustify, clYellow, $A0A0A0);
    finally
      lines.Free();
    end;
  end;

  _JCB_PREJEZD_STITEK : begin
    Result[0] := GetUPOLine('ŠTÍTEK '+Bariera.block.name, taCenter, clBlack, clTeal);
    lines := GetLines(TBlkCrossing(Bariera.block).note, _UPO_LINE_LEN);
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
    Result[2] := GetUPOLine(Bariera.block.name);
  end;

  _JCB_PRIVOLAVACKA : begin
    Result[0] := GetUPOLine('POZOR !', taCenter, clYellow, $A0A0A0);
    Result[1] := GetUPOLine('Svítí přivolávací návěst');
    Result[2] := GetUPOLine(Bariera.block.name);
  end;

  _JCB_HV_RUC : begin
    Result[0] := GetUPOLine('POZOR !', taCenter, clYellow, $A0A0A0);
    Result[1] := GetUPOLine('Hnací vozidlo v ručním řízení');
    Result[2] := GetUPOLine(IntToStr(Bariera.param) + ' : ' + HVDb[Bariera.param].name);
  end;

  _JCB_HV_NOT_ALL_RUC : begin
    Result[0] := GetUPOLine('POZOR !', taCenter, clYellow, $A0A0A0);
    Result[1] := GetUPOLine('Ne všechna HV v ručním řízení');
    Result[2] := GetUPOLine('');
  end;

  _JCB_TRAT_ZAK : begin
    Blocks.GetBlkByID(Self.m_data.railwayId, Blk);
    case (Self.m_data.railwayDir) of
      TRailwayDirection.AtoB : canZAK := TBlkLinker(TBlkRailway(Blk).linkerA).departureForbidden;
      TRailwayDirection.BtoA : canZAK := TBlkLinker(TBlkRailway(Blk).linkerB).departureForbidden;
    else
     canZAK := true;
    end;

    if ((Self.typ = TJCType.shunt) and (canZAK)) then
     begin
      Result[0] := GetUPOLine('ZAVEDEN ZÁKAZ ODJEZDU', taCenter, clRed, clWhite);
      Result[1] := GetUPOLine(Bariera.block.name);
      Result[2] := GetUPOLine('');
     end else begin
      Result[0] := GetUPOLine('NEPŘÍPUSTNÉ', taCenter, clRed, clWhite);
      Result[1] := GetUPOLine('Zákaz odjezdu');
      if (Assigned(Bariera.block)) then
        Result[2] := GetUPOLine(Bariera.block.name)
      else
        Result[2] := GetUPOLine('ID ' + IntToStr(bariera.param));
     end;
  end;

  _JCB_TRAT_STITEK : begin
    Result[0] := GetUPOLine('ŠTÍTEK '+Bariera.block.name, taCenter, clBlack, clTeal);
    lines := GetLines((Bariera.block as TBlkLinker).note, _UPO_LINE_LEN);
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
class function TJC.CriticalBarrier(typ: Integer): Boolean;
begin
 case (typ) of
  _JCB_STAVENI, _JCB_BLOK_DISABLED, _JCB_BLOK_NOT_EXIST, _JCB_BLOK_NOT_TYP :
            Result := true;
 else
  Result := false;
 end;
end;

////////////////////////////////////////////////////////////////////////////////

function TJC.WarningBarrier(typ: Integer): Boolean;
var Blk: TBlk;
begin
 case (typ) of
  _JCB_TRAT_ZAK: begin
      Blocks.GetBlkByID(Self.m_data.railwayId, Blk);
      case (Self.m_data.railwayDir) of
        TRailwayDirection.AtoB : Result := (Self.typ = TJCType.shunt) and (TBlkLinker(TBlkRailway(Blk).linkerA).departureForbidden);
        TRailwayDirection.BtoA : Result := (Self.typ = TJCType.shunt) and (TBlkLinker(TBlkRailway(Blk).linkerB).departureForbidden);
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

class function TJC.PotvrSekvBarrier(typ: Integer): Boolean;
begin
 case (typ) of
  _JCB_VYHYBKA_VYLUKA, _JCB_USEK_VYLUKA, _JCB_TRAT_ZAK: Result := true;
 else
  Result := false;
 end;
end;

////////////////////////////////////////////////////////////////////////////////

class function TJC.PotvrSekvBarrierToReason(typ: Integer): string;
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

procedure TJC.CritBarieraEsc(Sender: TObject);
begin
 Self.CancelActivating('', true);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TJC.TurnoutMovedJCPC(Sender: TObject);
var i: Integer;
    Blk: TBlk;
    refugeeId: Integer;
    track: TBlkTrack;
begin
 { Pozor: muze se stat, ze nektera z vyhybek, ktere jeste nejsou prestavovany,
   je behem staveni JC prestavena externim zdrojem. Je treba na to pamatovat.

   Pozor: i ty vyhybky, ktere pri staveni nebyly explicitne zamknuty, se samy
   zamknou pri udeleni zaveru na usek. Nelze tedy vyhybky rozlisovat podle
   zamknuti.

   Pozor: tato funkce muze volat sama sebe rekurzivne skrze callback.
 }

 if (Self.m_state.nextTurnout < 0) then Exit();

 if (Self.m_state.nextTurnout < Self.m_data.turnouts.Count) then
  begin
   // stavim dalsi vyhybku
   for i := Self.m_state.nextTurnout to Self.m_data.turnouts.Count-1 do
    begin
     Blocks.GetBlkByID(Self.m_data.turnouts[i].block, Blk);
     if (TBlkTurnout(Blk).position <> TTurnoutPosition(Self.m_data.turnouts[i].position)) then
      begin
       Self.m_state.nextTurnout := i+1;
       TBlkTurnout(Blk).SetPosition(TTurnoutPosition(Self.m_data.turnouts[i].position),
                                    true, false, Self.TurnoutMovedJCPC, Self.TurnoutErrJCPC);
       Exit();
      end;
    end;

   // sem se skoci, pokud vsechny zbyvajici vyhybky byly ve spravne poloze
   Self.m_state.nextTurnout := Self.m_data.turnouts.Count;
  end;

 if (Self.m_state.nextTurnout < Self.m_data.turnouts.Count+Self.m_data.refuges.Count) then
  begin
   // stavim dalsi odvrat
   refugeeId := Self.m_state.nextTurnout - Self.m_data.turnouts.Count;
   for i := refugeeId to Self.m_data.refuges.Count-1 do
    begin
     // nastaveni odvratu
     Blocks.GetBlkByID(Self.m_data.refuges[i].block, Blk);
     if (TBlkTurnout(Blk).position <> TTurnoutPosition(Self.m_data.refuges[i].position)) then
      begin
       TBlkTurnout(Blk).IntentionalLock();

       Blocks.GetBlkByID(Self.m_data.refuges[i].ref_blk, TBlk(track));
       track.AddChangeEvent(track.eventsOnZaverReleaseOrAB,
         CreateChangeEvent(ceCaller.NullVyhybkaMenuReduction, Self.m_data.refuges[i].block));

       Self.m_state.nextTurnout := i+Self.m_data.turnouts.Count+1;
       TBlkTurnout(Blk).SetPosition(TTurnoutPosition(Self.m_data.refuges[i].position),
                                    true, false, Self.TurnoutMovedJCPC, Self.TurnoutErrJCPC);
       Exit();
      end;
    end;

   // sem se skoci, pokud vsechny zbyvajici odvraty byly ve spravne poloze
   Self.m_state.nextTurnout := -1;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TJC.TurnoutErrJCPC(Sender: TObject; error: TTurnoutSetError);
begin
 if (not Self.activating) then Exit();

 if (Self.m_state.senderPnl <> nil) and (Self.m_state.senderOR <> nil) then
   PanelServer.BottomError(Self.m_state.senderPnl,
     'Nepřestavena '+(Sender as TBlkTurnout).name + ': ' + TBlkTurnout.SetErrorToMsg(error),
     (Self.m_state.senderOR as TArea).ShortName, 'TECHNOLOGIE');
 Self.CancelActivating('', true);
 Self.Cancel();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TJC.TurnoutErrNC(Sender: TObject; error: TTurnoutSetError);
begin
 Self.TurnoutMovedNC(Sender);
end;

procedure TJC.TurnoutMovedNC(Sender: TObject);
var signal, coupling: TBlk;
    Blk: TBlk;
    refugeeId: Integer;
begin
 if ((Self.m_state.step <> _NC_STEP_INIT) and (Self.m_state.step <> _NC_STEP_BARIERA_UPDATE)) then Exit();

 TBlkTurnout(Sender).emLock := true;

 Blocks.GetBlkByID(Self.m_data.signalId, signal);
 TBlkSignal(signal).AddBlkToRnz(TBlk(Sender).id, false);

 if (TBlkTurnout(Sender).GetSettings().coupling > -1) then
  begin
   Blocks.GetBlkByID(TBlkTurnout(Sender).GetSettings().coupling, coupling);
   TBlkTurnout(coupling).emLock := true;
   TBlkSignal(signal).AddBlkToRnz(TBlkTurnout(Sender).GetSettings().coupling, false);
  end;

 // staveni dalsich vyhybek

 if (Self.m_state.nextTurnout < 0) then Exit();

 if (Self.m_state.nextTurnout < Self.m_data.turnouts.Count) then
  begin
   // stavim dalsi vyhybku
   // Tady staci postavit jen jednu vyhybku, protoze jeji uzamceni opet zavola
   // tuto udalost.

   Blocks.GetBlkByID(Self.m_data.turnouts[Self.m_state.nextTurnout].block, Blk);
   Inc(Self.m_state.nextTurnout);

   TBlkTurnout(Blk).SetPosition(TTurnoutPosition(Self.m_data.turnouts[Self.m_state.nextTurnout-1].position),
                                true, false, Self.TurnoutMovedNC, Self.TurnoutErrNC); // may call callback directly!
  end else if ((Self.m_state.nextTurnout >= Self.m_data.turnouts.Count) and
      (Self.m_state.nextTurnout < Self.m_data.turnouts.Count+Self.m_data.refuges.Count)) then begin
   // nastaveni odvratu
   // Tady staci postavit jen jednu vyhybku, protoze jeji uzamceni opet zavola
   // tuto udalost.

   refugeeId := Self.m_state.nextTurnout - Self.m_data.turnouts.Count;

   Blocks.GetBlkByID(Self.m_data.refuges[refugeeId].block, Blk);
   Inc(Self.m_state.nextTurnout);

   TBlkTurnout(Blk).SetPosition(TTurnoutPosition(Self.m_data.refuges[refugeeId].position),
                                true, false, Self.TurnoutMovedNC, Self.TurnoutErrNC); // may call callback directly!
  end else if (Self.m_state.nextTurnout = Self.m_data.turnouts.Count+Self.m_data.refuges.Count) then
    Self.m_state.nextTurnout := -1;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TJC.SignalError(Sender: TObject);
var signal: TBlk;
begin
 if (not Self.activating) then Exit();
 Blocks.GetBlkByID(Self.m_data.signalId, signal);

 if (Self.m_state.senderPnl <> nil) and (Self.m_state.senderOR <> nil) then
   PanelServer.BottomError(Self.m_state.senderPnl, 'Návěstidlo '+signal.name + ' nepostaveno',
     (Self.m_state.senderOR as TArea).ShortName, 'TECHNOLOGIE');
 Self.CancelActivating('', true);
end;

////////////////////////////////////////////////////////////////////////////////
// generuje podminky branici postaveni nouzove posunove ceste
//  tyto podminky jsou prubezne zobrazovany dispecerovi v potvrzovaci sekvenci

procedure TJC.BarriersNCToAccept(var bariery: TList<TJCBarrier>);
var i: Integer;
    Blk, blk2: TBlk;
    glob: TBlkSettings;
    track, lastTrack: TBlkTrack;
    railway: TBlkRailway;
    crossingZav: TJCCrossingZav;
    trackId: Integer;
    turnoutZav: TJCTurnoutZav;
    refugeeZav: TJCRefugeeZav;
    refZav: TJCRefZav;
begin
  // kontrola navestidla
  Blocks.GetBlkByID(Self.m_data.signalId, Blk);
  if (not (Blk as TBlkSignal).enabled) then
    bariery.Add(Self.JCBarrier(_JCB_BLOK_DISABLED, Blk, Blk.id));

  // kontrola useku
  for i := 0 to Self.m_data.tracks.Count-1 do
   begin
    trackId := Self.m_data.tracks[i];
    Blocks.GetBlkByID(trackId, Blk);
    glob := Blk.GetGlobalSettings();

    // disabled
    if ((Blk as TBlkTrack).occupied = TTrackState.disabled) then
      bariery.Add(Self.JCBarrier(_JCB_BLOK_DISABLED, Blk, Blk.id))

    else if ((i <> Self.m_data.tracks.Count-1) or (Self.typ <> TJCType.shunt)) then
     begin
      if ((Blk as TBlkTrack).occupied <> TTrackState.free) then
        bariery.Add(Self.JCBarrier(_JCB_USEK_OBSAZENO, Blk, Blk.id));
     end;

    // souprava
    if (((Blk as TBlkTrack).IsTrain()) and (Self.typ = TJCType.train)) then
      bariery.Add(Self.JCBarrier(_JCB_USEK_SOUPRAVA, Blk, Blk.id));
   end;

  // kontrola vyhybek:
  for turnoutZav in Self.m_data.turnouts do
   begin
    Blocks.GetBlkByID(turnoutZav.block, Blk);
    glob := Blk.GetGlobalSettings();

    // kontrola polohy:
    if (TBlkTurnout(Blk).position <> turnoutZav.position) then
      bariery.Add(Self.JCBarrier(_JCB_VYHYBKA_KONC_POLOHA, Blk, Blk.id));

    // kontrola nouzoveho zaveru:
    if (not TBlkTurnout(Blk).emLock) then
      bariery.Add(Self.JCBarrier(_JCB_VYHYBKA_NOUZ_ZAVER, Blk, Blk.id));

    // kontrola spojky
    Blocks.GetBlkByID(TBlkTurnout(Blk).GetSettings.coupling, Blk2);
    if ((blk2 <> nil) and (TBlkTurnout(Blk).position <> turnoutZav.position)) then
     begin
      if (not TBlkTurnout(Blk2).emLock) then
        bariery.Add(Self.JCBarrier(_JCB_VYHYBKA_NOUZ_ZAVER, Blk2, Blk2.id));

      if (TBlkTurnout(Blk2).occupied = TTrackState.occupied) then
        bariery.Add(Self.JCBarrier(_JCB_USEK_OBSAZENO, Blk2, Blk2.id));
     end;

    // kontrola neprofiloveho styku pro polohu +
    if ((turnoutZav.position = TTurnoutPosition.plus) and (TBlkTurnout(Blk).npBlokPlus <> nil)) then
     begin
      if (TBlkTrack(TBlkTurnout(Blk).npBlokPlus).occupied = TTrackState.disabled) then
        bariery.Add(Self.JCBarrier(_JCB_BLOK_DISABLED, TBlkTurnout(Blk).npBlokPlus,
            TBlkTurnout(Blk).npBlokPlus.id))
      else
        if (TBlkTrack(TBlkTurnout(Blk).npBlokPlus).occupied <> TTrackState.free) then
          bariery.Add(Self.JCBarrier(_JCB_USEK_OBSAZENO, TBlkTurnout(Blk).npBlokPlus,
              TBlkTurnout(Blk).npBlokPlus.id));
     end;

    // kontrola neprofiloveho styku pro polohu -
    if ((turnoutZav.position = TTurnoutPosition.minus) and (TBlkTurnout(Blk).npBlokMinus <> nil)) then
     begin
      if (TBlkTrack(TBlkTurnout(Blk).npBlokMinus).occupied = TTrackState.disabled) then
        bariery.Add(Self.JCBarrier(_JCB_BLOK_DISABLED, TBlkTurnout(Blk).npBlokMinus,
            TBlkTurnout(Blk).npBlokMinus.id))
      else
        if (TBlkTrack(TBlkTurnout(Blk).npBlokMinus).occupied <> TTrackState.free) then
          bariery.Add(Self.JCBarrier(_JCB_USEK_OBSAZENO, TBlkTurnout(Blk).npBlokMinus,
              TBlkTurnout(Blk).npBlokMinus.id));
     end;
   end;

  // kontrola prejezdu
  for crossingZav in Self.m_data.crossings do
   begin
    Blocks.GetBlkByID(crossingZav.crossingId, Blk);

    if ((Blk as TBlkCrossing).state <> TBlkCrossingBasicState.none) then
     begin
      if ((Blk as TBlkCrossing).pcEmOpen) then
       begin
        bariery.Add(Self.JCBarrier(_JCB_PREJEZD_NOUZOVE_OTEVREN, blk, crossingZav.crossingId));
       end else begin
        if ((Blk as TBlkCrossing).state <> TBlkCrossingBasicState.closed) then
          bariery.Add(Self.JCBarrier(_JCB_PREJEZD_NEUZAVREN, blk, crossingZav.crossingId));
       end;
     end else
       bariery.Add(Self.JCBarrier(_JCB_PREJEZD_PORUCHA, blk, crossingZav.crossingId));
   end;

  // kontrola odvratu
  for refugeeZav in Self.m_data.refuges do
   begin
    Blocks.GetBlkByID(refugeeZav.block, Blk);
    glob := Blk.GetGlobalSettings();

    // kontrola polohy:
    if (TBlkTurnout(Blk).position <> refugeeZav.position) then
      bariery.Add(Self.JCBarrier(_JCB_VYHYBKA_KONC_POLOHA, Blk, Blk.id));

    // kontrola nouzoveho zaveru:
    if (not TBlkTurnout(Blk).emLock) then
      bariery.Add(Self.JCBarrier(_JCB_VYHYBKA_NOUZ_ZAVER, Blk, Blk.id));

    // kontrola spojky odvratu
    Blocks.GetBlkByID(TBlkTurnout(Blk).GetSettings.coupling, Blk2);
    if (blk2 <> nil) then
     begin
      // kontrola spravneho uzamceni odvratu
      if (TBlkTurnout(Blk).position <> refugeeZav.position) then
        if (not TBlkTurnout(Blk2).emLock) then
          bariery.Add(Self.JCBarrier(_JCB_VYHYBKA_NOUZ_ZAVER, Blk2, Blk2.id));
     end;
   end;

  if (Self.m_data.railwayId > -1) then
   begin
    if (Self.typ = TJCType.train) then
     begin
      Blocks.GetBlkByID(Self.m_data.tracks[Self.m_data.tracks.Count-1], Blk);
      if (not TBlkRT(blk).sectReady) then
       begin
        Blocks.GetBlkByID(Self.m_data.railwayId, Blk);
        bariery.Add(Self.JCBarrier(_JCB_TRAT_NEPRIPRAVENA, blk, Self.m_data.railwayId));
       end;
     end;

    Blocks.GetBlkByID(Self.m_data.railwayId, TBlk(railway));
    glob := railway.GetGlobalSettings();

    if ((railway.departureForbidden) and (Self.typ = TJCType.train)) then
      bariery.Add(Self.JCBarrier(_JCB_TRAT_ZAK, blk, Self.m_data.railwayId));
    if ((not railway.departureForbidden) and (Self.typ = TJCType.shunt)) then
      bariery.Add(Self.JCBarrier(_JCB_TRAT_NOT_ZAK, blk, Self.m_data.railwayId));
    if (railway.Zaver) then
      bariery.Add(Self.JCBarrier(_JCB_TRAT_ZAVER, blk, Self.m_data.railwayId));
    if (railway.request) then
      bariery.Add(Self.JCBarrier(_JCB_TRAT_ZADOST, blk, Self.m_data.railwayId));
    if (Self.m_data.railwayDir <> railway.direction) then
      bariery.Add(Self.JCBarrier(_JCB_TRAT_NESOUHLAS, blk, Self.m_data.railwayId));
    if ((not railway.BP) and (Self.typ = TJCType.train)) then
      bariery.Add(Self.JCBarrier(_JCB_TRAT_NO_BP, blk, Self.m_data.railwayId));

    track := (Self.signal as TBlkSignal).track as TBlkTrack;
    lastTrack := TBlkTrack(Self.lastTrack);

    if ((track.IsTrain) and (lastTrack.typ = btRT) and ((lastTrack as TBlkRT).inRailway = Self.data.railwayId)) then
     begin
      if (railway.lockout) then
       begin
        if ((railway.state.trains.Count > 0) or ((railway.GetLastTrack(Self.data.railwayDir) as TBlkRT).Zaver <> TZaver.no)) then
          bariery.Add(Self.JCBarrier(_JCB_TRAT_NEPRENOS, railway, Self.m_data.railwayId))
        else
          bariery.Add(Self.JCBarrier(_JCB_TRAT_PRENOS_NAKONEC, railway, Self.m_data.railwayId));
       end else begin
        if ((lastTrack.IsTrain()) or (not railway.BP) or (railway.direction <> Self.data.railwayDir)) then
          bariery.Add(Self.JCBarrier(_JCB_TRAT_NEPRENOS, railway, Self.m_data.railwayId));
       end;
     end;
   end;

  // kontrola uzamceni zamku:
  for refZav in Self.m_data.locks do
   begin
    Blocks.GetBlkByID(refZav.block, Blk);
    glob := Blk.GetGlobalSettings();

    // kontrola uzamceni
    if ((Blk as TBlkLock).keyReleased) then
      bariery.Add(Self.JCBarrier(_JCB_ZAMEK_NEUZAMCEN, blk, blk.id));

    // kontrola uzamceni
    if (not (Blk as TBlkLock).emLock) then
      bariery.Add(Self.JCBarrier(_JCB_ZAMEK_NOUZ_ZAVER, blk, blk.id));
   end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TJC.NC_PS_Callback(Sender: TIdContext; success: Boolean);
var trackId: Integer;
    blk: TBlk;
begin
 if (success) then
  begin
   if (Self.step = _NC_STEP_BARIERA_UPDATE) then
     Self.step := _NC_STEP_BARIERY_POTVRZENY;
  end else begin
   Self.CancelActivating();

   // aktualizace stavu navestidla (zobrazeni RNZ)
   Blocks.GetBlkByID(Self.m_data.signalId, Blk);
   Blk.Change();

   for trackId in Self.m_data.tracks do
    begin
     Blocks.GetBlkByID(trackId, Blk);
     (Blk as TBlkTrack).Zaver := TZaver.no;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TJC.BarriersToPotvrSekv(barriers: TJCBarriers): TConfSeqItems;
var i: Integer;
begin
 Result := TList<TConfSeqItem>.Create();

 for i := 0 to barriers.Count-1 do
  begin
   case (barriers[i].typ) of
    _JCB_BLOK_DISABLED           : Result.Add(TArea.GetPSPodminka(barriers[i].block, 'Blok neaktivní'));

    _JCB_USEK_OBSAZENO           : Result.Add(TArea.GetPSPodminka(barriers[i].block, 'Úsek obsazen'));
    _JCB_USEK_SOUPRAVA           : Result.Add(TArea.GetPSPodminka(barriers[i].block, 'Úsek obsahuje soupravu'));

    _JCB_PREJEZD_NOUZOVE_OTEVREN : Result.Add(TArea.GetPSPodminka(barriers[i].block, 'Nouzově otevřen'));
    _JCB_PREJEZD_PORUCHA         : Result.Add(TArea.GetPSPodminka(barriers[i].block, 'Porucha'));
    _JCB_PREJEZD_NEUZAVREN       : Result.Add(TArea.GetPSPodminka(barriers[i].block, 'Neuzavřen'));

    _JCB_VYHYBKA_KONC_POLOHA     : Result.Add(TArea.GetPSPodminka(barriers[i].block, 'Není správná poloha'));
    _JCB_VYHYBKA_NOUZ_ZAVER      : Result.Add(TArea.GetPSPodminka(barriers[i].block, 'Není zaveden nouzový závěr'));
    _JCB_VYHYBKA_NESPAVNA_POLOHA : Result.Add(TArea.GetPSPodminka(barriers[i].block, 'Není správná poloha'));

    _JCB_TRAT_ZAK                : Result.Add(TArea.GetPSPodminka(barriers[i].block, 'Zákaz odjezdu'));
    _JCB_TRAT_NOT_ZAK            : Result.Add(TArea.GetPSPodminka(barriers[i].block, 'Nezaveden zákaz odjezdu'));
    _JCB_TRAT_ZAVER              : Result.Add(TArea.GetPSPodminka(barriers[i].block, 'Závěr'));
    _JCB_TRAT_NEPRIPRAVENA       : Result.Add(TArea.GetPSPodminka(barriers[i].block, 'Nepovoluje odjezd'));
    _JCB_TRAT_ZADOST             : Result.Add(TArea.GetPSPodminka(barriers[i].block, 'Probíhá žádost'));
    _JCB_TRAT_NESOUHLAS          : Result.Add(TArea.GetPSPodminka(barriers[i].block, 'Nesouhlas'));
    _JCB_TRAT_NO_BP              : Result.Add(TArea.GetPSPodminka(barriers[i].block, 'Bloková podmínka nezavedena'));
    _JCB_TRAT_NEPRENOS           : Result.Add(TArea.GetPSPodminka(barriers[i].block, 'Nedojde k přenosu čísla vlaku'));
    _JCB_TRAT_PRENOS_NAKONEC     : Result.Add(TArea.GetPSPodminka(barriers[i].block, 'Vlak bude přenesen až na konec trati'));

    _JCB_ZAMEK_NEUZAMCEN         : Result.Add(TArea.GetPSPodminka(barriers[i].block, 'Neuzamčen'));
    _JCB_ZAMEK_NOUZ_ZAVER        : Result.Add(TArea.GetPSPodminka(barriers[i].block, 'Není zaveden nouzový závěr'));
   end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TJC.SetData(prop: TJCdata);
var id_changed: Boolean;
    signal_changed: Boolean;
    orig_signal: TBlk;
begin
 id_changed := ((Self.id <> prop.id) and (Self.id <> -1));
 signal_changed := (Self.data.signalId <> prop.signalId);
 Blocks.GetBlkByID(Self.data.signalId, orig_signal);
 Self.m_data := prop;
 if (id_Changed) then
  begin
   // sem se skoci, pokud je potreba preskladat JC, protoze doslo ke zmene ID
   // pri vytvareni novych JC se sem neskace
   if (Assigned(Self.OnIdChanged)) then
     Self.OnIdChanged(Self);
  end;

 if (signal_changed) then
   if (Assigned(Self.OnSignalChanged)) then
     Self.OnSignalChanged(Self, orig_signal);
end;

////////////////////////////////////////////////////////////////////////////////

function TJC.GetTrain(nav: TBlk = nil; usek: TBlk = nil): TTrain;
begin
 if (nav = nil) then
   Blocks.GetBlkByID(Self.m_data.signalId, nav);

 Result := TBlkSignal(nav).GetTrain(usek);
end;

////////////////////////////////////////////////////////////////////////////////

function TJC.GetAB(): Boolean;
var Blk: TBlk;
begin
 Blocks.GetBlkByID(Self.m_data.signalId, Blk);
 Result := ((Blk <> nil) and (Blk.typ = btSignal) and (TBlkSignal(Blk).ABJC = Self));
end;

////////////////////////////////////////////////////////////////////////////////

function TJC.IsAnyTurnoutMinus(): Boolean;
var turnout: TJCTurnoutZav;
begin
 for turnout in Self.m_data.turnouts do
   if (turnout.position = TTurnoutPosition.minus) then
     Exit(true);
 Result := false;
end;

////////////////////////////////////////////////////////////////////////////////

function TJC.IsCriticalBarrier(): Boolean;
var barriers: TJCBarriers;
    barrier: TJCBarrier;
    signal: TBlk;
begin
  Result := false;
  Blocks.GetBlkByID(Self.m_data.signalId, signal);
  barriers := TJCBarriers.Create();
  try
    Self.BarriersVCPC(barriers);
    for barrier in barriers do
     begin
      case (barrier.typ) of
        _JCB_BLOK_DISABLED, _JCB_BLOK_NOT_EXIST, _JCB_BLOK_NOT_TYP,
        _JCB_NAV_NOT_USEK, _JCB_USEK_OBSAZENO, _JCB_USEK_SOUPRAVA, _JCB_USEK_AB,
        _JCB_VYHYBKA_KONC_POLOHA, _JCB_VYHYBKA_NESPAVNA_POLOHA, _JCB_PREJEZD_NOUZOVE_OTEVREN,
        _JCB_PREJEZD_PORUCHA, _JCB_ODVRAT_KONC_POLOHA, _JCB_TRAT_NEPRIPRAVENA,
        _JCB_TRAT_ZADOST, _JCB_TRAT_NESOUHLAS, _JCB_TRAT_NO_BP, _JCB_ZAMEK_NEUZAMCEN:
          Exit(true);

        _JCB_TRAT_ZAK: Exit(Self.typ = TJCType.train);
      end;
     end;
  finally
    barriers.Free();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TJC.GetSignal(): TBlk;
begin
 Blocks.GetBlkByID(Self.m_data.signalId, Result);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TJC.ClientDisconnect(AContext: TIDContext);
begin
 if (Self.m_state.senderPnl = AContext) then
   Self.m_state.senderPnl := nil;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TJC.SetInitStep();
begin
 if (Self.m_state.nc) then
  begin
   Self.step := _NC_STEP_INIT;
   Self.m_state.timeOut := Now + EncodeTime(0, _NC_TIMEOUT_MIN, 0, 0);
  end else begin
   Self.step := _JC_STEP_INIT;
   Self.m_state.timeOut := Now + EncodeTime(0, _JC_TIMEOUT_SEC div 60, _JC_TIMEOUT_SEC mod 60, 0);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TJC.GetWaitFroLastTrackOrRailwayOccupied(): Boolean;
begin
 Result := (Self.step = _JC_STEP_CEKANI_POSLEDNI_USEK);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TJC.GetPtData(json: TJsonObject; includeStaveni: Boolean);
var turnoutZav: TJCTurnoutZav;
    refugeeZav: TJCRefugeeZav;
    newObj: TJsonObject;
    usek: Integer;
    crossingZav: TJCCrossingZav;
    refZav: TJCRefZav;
begin
 json['name'] := Self.m_data.name;
 json['id'] := Self.m_data.id;
 json['signalId'] := Self.m_data.signalId;
 case (Self.typ) of
  TJCType.train: json['type'] := 'VC';
  TJCType.shunt: json['type'] := 'PC';
 end;
 case (Self.m_data.nextSignalType) of
  TJCNextSignalType.no: json['nextSignal'] := '-';
  TJCNextSignalType.railway: json['nextSignal'] := 'trat';
  TJCNextSignalType.signal: begin
    json['nextSignal'] := 'block';
    json['nextSignalId'] := Self.m_data.nextSignalId;
  end;
 end;

 for turnoutZav in Self.m_data.turnouts do
  begin
   newObj := json.A['turnouts'].AddObject();
   newObj['block'] := turnoutZav.block;
   case (turnoutZav.position) of
    TTurnoutPosition.plus: newObj['position'] := '+';
    TTurnoutPosition.minus: newObj['position'] := '-';
   end;
  end;

 for usek in Self.m_data.tracks do
   json.A['tracks'].Add(usek);

 for refugeeZav in Self.m_data.refuges do
  begin
   newObj := json.A['refuges'].AddObject();
   newObj['block'] := refugeeZav.block;
   case (refugeeZav.position) of
    TTurnoutPosition.plus: newObj['position'] := '+';
    TTurnoutPosition.minus: newObj['position'] := '-';
   end;
   newObj['refBlock'] := refugeeZav.ref_blk;
  end;

 for crossingZav in Self.m_data.crossings do
  begin
   newObj := json.A['crossings'].AddObject();
   newObj['crossing'] := crossingZav.crossingId;
   newObj['open'] := crossingZav.openTrack;
   for usek in crossingZav.closeTracks do
     newObj.A['close'].Add(usek);
  end;

 for refZav in Self.m_data.locks do
  begin
   newObj := json.A['locks'].AddObject();
   newObj['lock'] := refZav.block;
   newObj['refTrack'] := refZav.ref_blk;
  end;

 for usek in Self.m_data.vb do
   json.A['vb'].Add(usek);

 if (Self.m_data.railwayId <> -1) then
  begin
   json['railway'] := Self.m_data.railwayId;
   json['railwayDir'] := Integer(Self.m_data.railwayDir);
  end;

 json['speedGo'] := Self.m_data.speedGo;
 json['speedStop'] := Self.m_data.speedStop;
 json['turn'] := Self.m_data.turn;

 if (includeStaveni) then
   Self.GetPtState(json['state']);
end;

procedure TJC.GetPtState(json: TJsonObject);
begin
 json['activating'] := Self.activating;
 json['active'] := Self.active;
 json['step'] := Self.m_state.step;
 json['destroyBlock'] := Self.m_state.destroyBlock;
 json['destroyEndBlock'] := Self.m_state.destroyEndBlock;
 json['ab'] := Self.AB;
end;

procedure TJC.PostPtStav(reqJson: TJsonObject; respJson: TJsonObject);
var barriers: TJCBarriers;
    barrier: TJCBarrier;
    ok: Integer;
    ab: Boolean;
begin
 if ((Self.signal = nil) or (TBlkSignal(Self.signal).areas.Count = 0)) then
  begin
   PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '400', 'Návěstidlo není v OŘ');
   Exit();
  end;

 ab := (reqJson.Contains('ab') and reqJson.B['ab']);

 barriers := TJCBarriers.Create();
 try
   ok := Self.Activate(nil, TBlkSignal(Self.signal).areas[0], barriers, nil, false, false, ab);
   respJson['success'] := (ok = 0);
   for barrier in barriers do
     Self.BarrierToJson(barrier, respJson.A['bariery'].AddObject());
 finally
   barriers.Free();
 end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TJC.BarrierToJson(const bariera: TJCBarrier; result: TJsonObject);
var upoItem: TUPOItem;
begin
 result['typ'] := bariera.typ;
 if (bariera.block <> nil) then
   result['blok'] := bariera.block.id;
 result['param'] := bariera.param;
 if (CriticalBarrier(bariera.typ)) then
   result['type'] := 'critical'
 else if (WarningBarrier(bariera.typ)) then
   result['type'] := 'warning'
 else
   result['type'] := 'standard';

 upoItem := JCBarrierToMessage(bariera);
 result['description'] := upoItem[0].str + ' ' + upoItem[1].str + ' ' + upoItem[2].str;
end;

function TJC.GetLastTrack(): TBlk;
begin
 if (Self.data.tracks.Count = 0) then
   Exit(nil);
 Blocks.GetBlkByID(Self.data.tracks[Self.data.tracks.Count-1], Result);
 if (Result.typ <> btTrack) and (Result.typ <> btRT) then
   Result := nil;
end;

end.//unit
