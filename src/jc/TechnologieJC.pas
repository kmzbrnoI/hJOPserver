﻿unit TechnologieJC;

{
  Kompletni technologie jizdnich cest.

  Tento soubor implementuje tridu TJC, ktera reprezentuje jednu jizdni cestu.
  Jizdni cesta se stara o vse od udrzovani vsech jejich udaju, kterymi je dana
  v zaverove tabulce, pres jeji staveni, kontrolu podminek, zobrazovani
  potvrzovacich sekvenci pri staveni az po spravne ruseni jizdni cesty.
}

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, Logging,
  Dialogs, Menus, Buttons, ComCtrls, fMain, BlockDb, Block, Train,
  IniFiles, IdContext, BlockRailway, Generics.Collections, UPO, BlockTurnout,
  Area, changeEvent, changeEventCaller, JsonDataObjects, PTUtils, JCBarriers;

const
  _JC_INITPOTVR_TIMEOUT_SEC = 60; // timeout UPO a potvrzeni na zacatku staveni JC
  _JC_TIMEOUT_SEC = 30; // timeout pro staveni jizdni cesty (vlakove i posunove v sekundach)
  _JC_PRJ_TIMEOUT_SEC = 50; // timeout pri staveni JC pro zavirani prejezdu v ceste
  _NC_TIMEOUT_MIN = 1; // timeout pro staveni nouzove cesty (vlakove i posunove) v minutach
  _JC_MAX_VYH_STAVENI = 4; // kolik vyhybek se muze stavit zaroven v JC

  _JC_DESTROY_NC = -6;
  _JC_DESTROY_NONE = -5;
  _JC_DESTROY_SIGNAL_STUJ = -2;
  _JC_DESTROY_SIGNAL_TRACK = -1;

type
  TJCType = (train = 1, shunt = 2, emergency = 3);
  TJCNextSignalType = (no = 0, railway = 1, signal = 2);

  JCStep = (
    stepDefault = 0,
    stepCritBarriers = 1,
    stepConfBarriers = 5,
    stepConfSeq = 6,

    stepJcInit = 10,
    stepJcTurnoutsMoving = 11,
    stepJcCloseCross = 12,
    stepJcWaitCross = 13,
    stepJcFinalZaver = 14,
    stepJcSignalWait = 15,
    stepJcFinish = 16,
    stepJcLastTrackWait = 20,

    stepNcInit = 100,
    stepNcBarrierUpdate = 101,
    stepNcBarrierConfirmed = 102,
    stepNcSignalWait = 103,
    stepNcFinish = 104
  );


  // zaver vyhybky v jizdni ceste
  TJCTurnoutZav = record
    Block: Integer;
    position: TTurnoutPosition;
  end;

  // zaver odvratove vyhybky v jizdni ceste
  TJCRefugeeZav = record
    Block: Integer;
    position: TTurnoutPosition;
    // blok, pri jehoz zruseni redukce (typicky usek a uvolneni zaveru) dojde i k uvolneni zaveru odvratove vyhybky
    ref_blk: Integer;
  end;

  // bloky v JC, ketre jsou navazany na konkretni useky v ramci JC
  TJCRefZav = record
    Block: Integer;
    ref_blk: Integer; // blok, pri jehoz uvolneni zaveru dojde ke zruseni redukce \Blok
  end;

  // prejezd v jizdni ceste
  TJCCrossingZav = record
    crossingId: Integer;
    closeTracks: TList<Integer>; // pokud se prejezd nezavira, je seznam prazdny
    openTrack: Integer; // pokud se prejezd nezavira, je nedefinovany
  end;

  /// ////////////////////////////////////////////////////////////////////////

  TJCdata = record
    name: string;
    id: Integer;
    typ: TJCType;

    signalId: Integer;
    signalCode: Integer; // effective only for shunting jc
    nextSignalType: TJCNextSignalType;
    nextSignalId: Integer;

    turnouts: TList<TJCTurnoutZav>;
    tracks: TList<Integer>;
    refuges: TList<TJCRefugeeZav>; // odvraty
    crossings: TList<TJCCrossingZav>;
    locks: TList<TJCRefZav>;
    vb: TList<Integer>; // seznam variantnich bodu JC - obashuje postupne ID bloku typu usek

    railwayId: Integer;
    railwayDir: TRailwayDirection;
    speedGo, speedStop: Integer; // rychlost v JC pri dalsim navestidle navestici dovolujici a NEdovolujici navest
    turn: Boolean; // jc od odbocky (40 km/h)
    nzv: Boolean; // nedostatecna zabrzdna vzdalenost
    signalFallTrackI: Cardinal;
  end;

  // staveni jizdni cesty:
  // staveni jizdni cesty probiha krokove, viz \UpdateStaveni
  TJCstate = record
    step: JCStep; // aktualni krok staveni jizdni cesty
    timeOut: TDateTime; // cas, pri jehoz prekroceni dojde k timeoutu JC
    // oblast rizeni, ktera vyvolala staveni JC, do teto OR jsou typicky odesilany notifikacni a chybove hlasky (napr. upozorneni vlevo dole panelu, potvrzovaci sekvence)
    senderOR: TObject;
    senderPnl: TIdContext; // konkretni panel, kery vyvolal staveni JC
    destroyBlock, // index useku, na ktery ma vkrocit souprava
    destroyEndBlock: Integer; // index useku, ze ktereho ma vystoupit souprava
    // index je index v seznamu useku, tedy napr. 0 =  0. usek v jizdni ceste; + specialni hodnoty _JC_DESTROY*
    from_stack: TObject; // odkaz na zasobnik, ze ktereho proehlo staveni JC
    nc: Boolean; // flag staveni nouzove cesty (vlakovou i posunovou)
    ncBariery: TJCBarriers; // aktualni seznam barier pro potvrzovaci sekvenci pri staveni nouzove cesty
    ncBarieryCntLast: Integer; // posledni pocet barier ve staveni nouzove cesty
    nextTurnout: Integer; // vyhybka, ktera se ma stavit jako dalsi
    // po postaveni vsechn vyhybek plynule prechazi do indexu seznamu odvratu
    ab: Boolean; // po postaveni JC automaticky zavest AB
    crossingWasClosed: Boolean; // jiz byl vydan povel k zavreni prejezdu
    lastTrackOrRailwayOccupied: Boolean; // je obsazen posledni usek JC, nestavit navestidlo a nezavirat prejezdy
  end;

  ENavChanged = procedure(Sender: TObject; origNav: TBlk) of object;

  /// ////////////////////////////////////////////////////////////////////////

  TJC = class
  private const
    _def_jc_staveni: TJCstate = (step: stepDefault; destroyBlock: _JC_DESTROY_NONE; destroyEndBlock: _JC_DESTROY_NONE;
      ab: false; crossingWasClosed: false;);

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
    // kontroluje zmenu smeru soupravy a hnacich vozidel pri vkroceni do smyckove bloku,
    // tato kontrola probiha pouze pri vkroceni do posledniho bloku JC
    procedure CheckLoopBlock(blk: TBlk);
    function IsActivating(): Boolean;
    function IsActive(): Boolean;

    procedure PS_vylCallback(Sender: TIdContext; success: Boolean); // callback potvrzovaci sekvence na vyluku
    procedure UPO_OKCallback(Sender: TObject); // callback potvrzeni upozorneni
    procedure UPO_EscCallback(Sender: TObject); // callback zamitnuti upozorneni
    procedure NC_PS_Callback(Sender: TIdContext; success: Boolean); // callback potvrzovaci sekvence nouzove cesty

    // zavre prejezd pri vkroceni na dany usek, odkaz na tuto metodu je posilan usekum, ktere ji pri obsazeni vyvolaji
    procedure TrackCloseCrossing(Sender: TObject; data: Integer);

    procedure SetDestroyBlock(destroyBlock: Integer);
    procedure SetDestroyEndBlock(destroyEndBlock: Integer);
    procedure SetStep(step: JCStep);
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

    function GetTrain(signal: TBlk = nil; track: TBlk = nil): TTrain; // vraci cislo soupravy na useku pred navestidlem

    function GetAB(): Boolean;
    function IsCriticalBarrier(): Boolean;
    function GetSignal(): TBlk;
    function GetWaitFroLastTrackOrRailwayOccupied(): Boolean;
    function GetLastTrack(): TBlk;
    function PSts(): TList<TBlk>;

    procedure Log(msg: string; typ: LogType = ltJC);

  public

    index: Integer; // index v tabulce jizdni cest ve F_Main
    changed: Boolean; // JC zmenana -> akualizuje se v tabulce ve F_Main

    constructor Create(); overload;
    constructor Create(data: TJCdata); overload;
    destructor Destroy(); override;

    procedure SetSignalSignal();
    procedure Cancel(Sender: TObject = nil);
    procedure CancelWithoutTrackRelease();
    procedure DynamicCanceling(); // kontroluje projizdeni soupravy useky a rusi jejich zavery
    procedure DynamicCancelingNC(); // rusi poruchu BP trati, ze ktere odjizdi souprava v ramci nouzove jizdni cesty
    procedure NonProfileOccupied(); // volano pri obsazeni kontrolvoaneho neprofiloveho useku

    procedure UpdateActivating();
    procedure UpdateTimeOut();
    // zrusi staveni a oduvodneni zaloguje a zobrazi dispecerovi
    procedure CancelActivating(reason: string = ''; stack_remove: Boolean = false);

    procedure LoadData(ini: TMemIniFile; section: string);
    procedure SaveData(ini: TMemIniFile; section: string);

    function Activate(senderPnl: TIdContext; senderOR: TObject; bariery_out: TJCBarriers; from_stack: TObject = nil;
      nc: Boolean = false; fromAB: Boolean = false; abAfter: Boolean = false): Integer; overload;
    function Activate(senderPnl: TIdContext; senderOR: TObject; from_stack: TObject = nil; nc: Boolean = false;
      fromAB: Boolean = false; abAfter: Boolean = false): Integer; overload;

    function CanDN(): Boolean;
    // true = je mozno DN; tato funkce kontroluje, jestli je mozne znovupostavit cestu i kdyz byla fakticky zrusena = musi zkontrolovat vsechny podminky
    procedure DN(); // DN nastavi zavery vsech bloku na validni a rozsviti navestidlo
    procedure STUJ();

    function barriers(nc: Boolean = false): TJCBarriers;
    function IsAnyTurnoutMinus(): Boolean;
    procedure ClientDisconnect(AContext: TIdContext);

    procedure GetPtData(json: TJsonObject; includeStaveni: Boolean);
    procedure GetPtState(json: TJsonObject);
    procedure PostPtActivate(reqJson: TJsonObject; respJson: TJsonObject);

    property data: TJCdata read m_data write SetData;
    property state: TJCstate read m_state;

    property name: String read m_data.name;
    property id: Integer read m_data.id write m_data.id;
    property typ: TJCType read m_data.typ;

    property activating: Boolean read IsActivating;
    property active: Boolean read IsActive; // true pokud je postavena navest
    property ab: Boolean read GetAB;
    property waitForLastTrackOrRailwayOccupy: Boolean read GetWaitFroLastTrackOrRailwayOccupied;
    property lastTrack: TBlk read GetLastTrack;

    property destroyBlock: Integer read m_state.destroyBlock write SetDestroyBlock;
    property destroyEndBlock: Integer read m_state.destroyEndBlock write SetDestroyEndBlock;
    property step: JCStep read m_state.step write SetStep;
    property signal: TBlk read GetSignal;

    property OnIdChanged: TNotifyEvent read fOnIdChanged write fOnIdChanged;
    property OnSignalChanged: ENavChanged read fOnSignalChanged write fOnSignalChanged;
  end;

implementation

uses GetSystems, TechnologieRCS, THnaciVozidlo, BlockSignal, BlockTrack, AreaDb,
  BlockCrossing, TJCDatabase, TCPServerPanel, TrainDb, timeHelper, ownConvert,
  THVDatabase, AreaStack, BlockLinker, BlockLock, BlockRailwayTrack, BlockDisconnector,
  BlockPSt;

/// /////////////////////////////////////////////////////////////////////////////

constructor TJC.Create();
begin
  inherited;

  Self.m_data.id := -1;
  Self.changed := true;
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
  if (not Assigned(Self.m_state.ncBariery)) then
    Self.m_state.ncBariery := TList<TJCBarrier>.Create();

  if (not Assigned(Self.m_data.locks)) then
    Self.m_data.locks := TList<TJCRefZav>.Create();
  if (not Assigned(Self.m_data.vb)) then
    Self.m_data.vb := TList<Integer>.Create();
  if (not Assigned(Self.m_data.refuges)) then
    Self.m_data.refuges := TList<TJCRefugeeZav>.Create();
  if (not Assigned(Self.m_data.crossings)) then
    Self.m_data.crossings := TList<TJCCrossingZav>.Create();
  if (not Assigned(Self.m_data.turnouts)) then
    Self.m_data.turnouts := TList<TJCTurnoutZav>.Create();
  if (not Assigned(Self.m_data.tracks)) then
    Self.m_data.tracks := TList<Integer>.Create();
end;

destructor TJC.Destroy();
begin
  if (Assigned(Self.m_state.ncBariery)) then
    FreeAndNil(Self.m_state.ncBariery);
  if (Assigned(Self.m_data.locks)) then
    FreeAndNil(Self.m_data.locks);
  if (Assigned(Self.m_data.vb)) then
    Self.m_data.vb.Free();

  if (Assigned(Self.m_data.turnouts)) then
    Self.m_data.turnouts.Free();
  if (Assigned(Self.m_data.tracks)) then
    Self.m_data.tracks.Free();
  if (Assigned(Self.m_data.refuges)) then
    Self.m_data.refuges.Free();
  for var i := 0 to Self.m_data.crossings.Count - 1 do
    Self.m_data.crossings[i].closeTracks.Free();
  if (Assigned(Self.m_data.crossings)) then
    Self.m_data.crossings.Free();

  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

// kontroluje podminky pro staveni konkretni jizdni cesty
// vraci List prblemu (tzv. bariery), ktere definuji to, proc jizdni cestu nelze postavit (tedy vraci vsechny nesplnene podminky)
// tzv. kriticke bariery jsou vzdy na zacatu Listu
function TJC.barriers(nc: Boolean = false): TJCBarriers;
begin
  result := TList<TJCBarrier>.Create();

  if (Self.activating) then
    result.Add(JCBarrier(barProcessing));

  // signal
  begin
    var signal: TBlk;

    if (Blocks.GetBlkByID(Self.m_data.signalId, signal) <> 0) then
    begin
      result.Add(JCBarrier(barBlockNotExists, nil, Self.m_data.signalId));
      Exit();
    end;

    if (Self.lastTrack = nil) then
    begin
      result.Add(JCBarrier(barBlockNotExists, nil, 0));
      Exit();
    end;

    if (signal.typ <> btSignal) then
    begin
      result.Add(JCBarrier(barBlockWrongType, signal));
      Exit();
    end;

    if ((signal as TBlkSignal).track = nil) then
    begin
      result.Add(JCBarrier(barSignalNoTrack, signal));
      Exit();
    end;
  end;

  // turnouts
  for var turnoutZav: TJCTurnoutZav in Self.m_data.turnouts do
  begin
    var turnout: TBlk;
    if (Blocks.GetBlkByID(turnoutZav.Block, turnout) <> 0) then
    begin
      result.Add(JCBarrier(barBlockNotExists, nil, turnoutZav.Block));
      Exit();
    end;

    if (turnout.typ <> btTurnout) then
    begin
      result.Add(JCBarrier(barBlockWrongType, turnout));
      Exit();
    end;
  end;

  // tracks
  for var trackZav: Integer in Self.m_data.tracks do
  begin
    var track: TBlk;
    if (Blocks.GetBlkByID(trackZav, track) <> 0) then
    begin
      result.Add(JCBarrier(barBlockNotExists, nil, trackZav));
      Exit();
    end;

    if ((track.typ <> btTrack) and (track.typ <> btRT)) then
    begin
      result.Add(JCBarrier(barBlockWrongType, track));
      Exit();
    end;
  end;

  // crossings
  for var crossingZav: TJCCrossingZav in Self.m_data.crossings do
  begin
    var crossing: TBlk;
    if (Blocks.GetBlkByID(crossingZav.crossingId, crossing) <> 0) then
    begin
      result.Insert(0, JCBarrier(barBlockNotExists, nil, crossingZav.crossingId));
      Exit();
    end;

    if (crossing.typ <> btCrossing) then
    begin
      result.Insert(0, JCBarrier(barBlockWrongType, crossing));
      Exit();
    end;

    // if track should be closed by path
    if (crossingZav.closeTracks.Count > 0) then
    begin
      var openTrack: TBlk;
      if (Blocks.GetBlkByID(crossingZav.openTrack, openTrack) <> 0) then
      begin
        result.Insert(0, JCBarrier(barBlockNotExists, openTrack));
        Exit();
      end;

      if ((openTrack.typ <> btTrack) and (openTrack.typ <> btRT)) then
      begin
        result.Insert(0, JCBarrier(barBlockWrongType, openTrack));
        Exit();
      end;

      for var trackZav: Integer in crossingZav.closeTracks do
      begin
        var closeTrack: TBlk;
        if (Blocks.GetBlkByID(trackZav, closeTrack) <> 0) then
        begin
          result.Insert(0, JCBarrier(barBlockNotExists, crossing));
          Exit();
        end;
        if ((closeTrack.typ <> btTrack) and (closeTrack.typ <> btRT)) then
        begin
          result.Insert(0, JCBarrier(barBlockWrongType, crossing));
          Exit();
        end;
      end;
    end;
  end;

  // refugees
  for var refugeeZav: TJCRefugeeZav in Self.m_data.refuges do
  begin
    var refugeeRef: TBlk;
    if (Blocks.GetBlkByID(refugeeZav.ref_blk, refugeeRef) <> 0) then
    begin
      result.Insert(0, JCBarrier(barBlockNotExists, nil, refugeeZav.ref_blk));
      Exit();
    end;
    if ((refugeeRef.typ <> btTrack) and (refugeeRef.typ <> btRT)) then
    begin
      result.Insert(0, JCBarrier(barBlockWrongType, refugeeRef));
      Exit();
    end;
    var refugee: TBlk;
    if (Blocks.GetBlkByID(refugeeZav.Block, refugee) <> 0) then
    begin
      result.Insert(0, JCBarrier(barBlockNotExists, nil, refugeeZav.Block));
      Exit();
    end;
    if (refugee.typ <> btTurnout) then
    begin
      result.Insert(0, JCBarrier(barBlockWrongType, refugee));
      Exit();
    end;
  end;

  // railway
  if (Self.m_data.railwayId > -1) then
  begin
    var railway: TBlk;
    if (Self.lastTrack.typ <> btRT) then
    begin
      result.Add(JCBarrier(barBlockWrongType, Self.lastTrack));
      Exit();
    end;
    if (Blocks.GetBlkByID(Self.m_data.railwayId, railway) <> 0) then
    begin
      result.Insert(0, JCBarrier(barBlockNotExists, nil, Self.m_data.railwayId));
      Exit();
    end;
    if (railway.typ <> btRailway) then
    begin
      result.Insert(0, JCBarrier(barBlockWrongType, railway));
      Exit();
    end;
  end;

  // locks
  for var refZaver: TJCRefZav in Self.m_data.locks do
  begin
    var lock: TBlk;
    if (Blocks.GetBlkByID(refZaver.Block, lock) <> 0) then
    begin
      result.Insert(0, JCBarrier(barBlockNotExists, nil, refZaver.Block));
      Exit();
    end;
    if (lock.typ <> btLock) then
    begin
      result.Insert(0, JCBarrier(barBlockWrongType, lock));
      Exit();
    end;
    var lockRef: TBlk;
    if (Blocks.GetBlkByID(refZaver.ref_blk, lockRef) <> 0) then
    begin
      result.Insert(0, JCBarrier(barBlockNotExists, nil, refZaver.ref_blk));
      Exit();
    end;
    if ((lockRef.typ <> btTrack) and (lockRef.typ <> btRT)) then
    begin
      result.Insert(0, JCBarrier(barBlockWrongType, lockRef));
      Exit();
    end;
  end;

  if (nc) then
    Self.BarriersNC(result)
  else
    Self.BarriersVCPC(result);

  var privol: TBlksList := Blocks.GetNavPrivol(Self.m_state.senderOR as TArea);

  for var i: Integer := 0 to privol.Count - 1 do
    result.Add(JCBarrier(barPrivol, privol[i] as TBlk, (privol[i] as TBlk).id));

  if (Assigned(privol)) then
    privol.Free();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TJC.BarriersVCPC(var barriers: TList<TJCBarrier>);
begin
  // signal
  var signal: TBlkSignal := TBlkSignal(Blocks.GetBlkByID(Self.m_data.signalId));
  if (not signal.enabled) then
    barriers.Add(JCBarrier(barBlockDisabled, signal));

  if (signal.signal <> ncStuj) then
    barriers.Add(JCBarrier(barSignalActive, signal));

  // tracks
  var tracksCount: Integer;
  if (Self.m_data.railwayId > -1) then
    tracksCount := Self.m_data.tracks.Count-1
  else
    tracksCount := Self.m_data.tracks.Count;

  for var i: Integer := 0 to tracksCount-1 do
  begin
    var track: TBlkTrack := TBlkTrack(Blocks.GetBlkByID(Self.m_data.tracks[i]));

    if (track.occupied = TTrackState.disabled) then
      barriers.Add(JCBarrier(barBlockDisabled, track));

    // occupancy
    if ((i <> Self.m_data.tracks.Count-1) or (Self.typ <> TJCType.shunt)) then
    begin
      if (track.occupied <> TTrackState.Free) then
      begin
        if ((i = Self.m_data.tracks.Count - 1) and (Self.m_data.tracks.Count > 1)) then
          barriers.Add(JCBarrier(barTrackLastOccupied, track))
        else
          barriers.Add(JCBarrier(barTrackOccupied, track));
      end else begin
        if (track.IsTrain()) then
          barriers.Add(JCBarrier(barTrackTrain, track));
      end;
    end;

    if (track.Zaver <> TZaver.no) then
    begin
      if (track.Zaver = TZaver.ab) then
        barriers.Add(JCBarrier(barTrackAB, track))
      else
        barriers.Add(JCBarrier(barTrackZaver, track));
    end;

    if (track.lockout <> '') then
      barriers.Add(JCBarrier(barBlockLockout, track));

    if (track.note <> '') then
      barriers.Add(JCBarrier(barBlockNote, track));

    if (track.PstIs()) then
      barriers.Add(JCBarrier(barTrackPSt, track));
  end;

  // turnouts
  for var turnoutZav: TJCTurnoutZav in Self.m_data.turnouts do
  begin
    var turnout: TBlkTurnout := TBlkturnout(Blocks.GetBlkByID(turnoutZav.Block));

    if (turnout.position = TTurnoutPosition.disabled) then
      barriers.Add(JCBarrier(barBlockDisabled, turnout));

    if ((turnoutZav.position = TTurnoutPosition.plus) and (turnout.npBlokPlus <> nil) and
      (TBlkTrack(turnout.npBlokPlus).occupied = TTrackState.disabled)) then
      barriers.Add(JCBarrier(barBlockDisabled, turnout.npBlokPlus));

    if ((turnoutZav.position = TTurnoutPosition.minus) and (turnout.npBlokMinus <> nil) and
      (TBlkTrack(turnout.npBlokMinus).occupied = TTrackState.disabled)) then
      barriers.Add(JCBarrier(barBlockDisabled, turnout.npBlokMinus));

    if ((turnout.position = TTurnoutPosition.none) or (turnout.position = TTurnoutPosition.both)) then
      barriers.Add(JCBarrier(barTurnoutNoPos, turnout));

    // we don't need to check 'zaver' because is was checked on tracks

    if (turnout.lockout <> '') then
      barriers.Add(JCBarrier(barBlockLockout, turnout));

    if (turnout.note <> '') then
      barriers.Add(JCBarrier(barBlockNote, turnout));

    if (turnout.PstIs()) then
      barriers.Add(JCBarrier(barTurnoutPst, turnout));

    if (turnout.position <> turnoutZav.position) then
    begin
      if (turnout.emLock) then
        barriers.Add(JCBarrier(barTurnoutEmLock, turnout))
      else if (turnout.outputLocked) then
        barriers.Add(JCBarrier(barTurnoutLocked, turnout));
    end;

    // coupling
    var coupling: TBlkTurnout := TBlkTurnout(Blocks.GetBlkByID(turnout.GetSettings.coupling));
    if ((coupling <> nil) and (turnout.position <> turnoutZav.position)) then
    begin
      if (coupling.emLock) then
        barriers.Add(JCBarrier(barTurnoutEmLock, coupling))
      else if (coupling.outputLocked) then
        barriers.Add(JCBarrier(barTurnoutLocked, coupling));

      if (coupling.occupied = TTrackState.occupied) then
        barriers.Add(JCBarrier(barTrackOccupied, coupling));
    end;

    if ((coupling <> nil) and (coupling.PstIs())) then
      barriers.Add(JCBarrier(barTurnoutPst, coupling));

    if ((turnoutZav.position = TTurnoutPosition.plus) and (turnout.npBlokPlus <> nil) and
      (TBlkTrack(turnout.npBlokPlus).occupied <> TTrackState.Free)) then
      barriers.Add(JCBarrier(barTrackOccupied, turnout.npBlokPlus));

    if ((turnoutZav.position = TTurnoutPosition.minus) and (turnout.npBlokMinus <> nil) and
      (TBlkTrack(turnout.npBlokMinus).occupied <> TTrackState.Free)) then
      barriers.Add(JCBarrier(barTrackOccupied, turnout.npBlokMinus));
  end;

  // crossings
  for var crossingZav: TJCCrossingZav in Self.m_data.crossings do
  begin
    var crossing: TBlkCrossing := TBlkCrossing(Blocks.GetBlkByID(crossingZav.crossingId));

    if (crossing.state = TBlkCrossingBasicState.disabled) then
      barriers.Add(JCBarrier(barBlockDisabled, crossing));

    if (crossing.state <> TBlkCrossingBasicState.none) then
    begin
      if (crossing.pcEmOpen) then
        barriers.Add(JCBarrier(barCrosEmOpen, crossing));
    end else
      barriers.Add(JCBarrier(barCrosError, crossing));

    if (crossing.note <> '') then
      barriers.Add(JCBarrier(barBlockNote, crossing));
  end;

  // refugees
  for var refugeeZav: TJCRefugeeZav in Self.m_data.refuges do
  begin
    var refugee: TBlkTurnout := TBlkTurnout(Blocks.GetBlkByID(refugeeZav.Block));

    if (refugee.position = TTurnoutPosition.disabled) then
      barriers.Add(JCBarrier(barBlockDisabled, refugee));

    if ((refugee.position = TTurnoutPosition.none) or (refugee.position = TTurnoutPosition.both)) then
      barriers.Add(JCBarrier(barTurnoutNoPos, refugee));

    if (refugee.lockout <> '') then
      barriers.Add(JCBarrier(barBlockLockout, refugee));

    if (refugee.note <> '') then
      barriers.Add(JCBarrier(barBlockNote, refugee));

    if (refugee.position <> refugeeZav.position) then
    begin
      if (refugee.emLock) then
        barriers.Add(JCBarrier(barTurnoutEmLock, refugee))

      else if (refugee.outputLocked) then
        barriers.Add(JCBarrier(barRefugeeLocked, refugee));

      if (refugee.occupied = TTrackState.occupied) then
        barriers.Add(JCBarrier(barRefugeeOccupied, refugee));
    end;

    if (refugee.PstIs()) then
      barriers.Add(JCBarrier(barRefugeePst, refugee));

    // refugee's coupling
    var coupling: TBlkTurnout := TBlkTurnout(Blocks.GetBlkByID(refugee.GetSettings.coupling));
    if (coupling <> nil) then
    begin
      if (coupling.lockout <> '') then
        barriers.Add(JCBarrier(barBlockLockout, coupling));

      if (coupling.note <> '') then
        barriers.Add(JCBarrier(barBlockNote, coupling));

      if (coupling.PstIs()) then
        barriers.Add(JCBarrier(barRefugeePst, coupling));

      if (refugee.position <> refugeeZav.position) then
      begin
        if (TBlkTurnout(coupling).Zaver > TZaver.no) then
        begin
          if (TBlkTurnout(coupling).Zaver = TZaver.ab) then
            barriers.Add(JCBarrier(barTrackAB, coupling))
          else
            barriers.Add(JCBarrier(barTrackZaver, coupling));
        end;

        if (TBlkTurnout(coupling).emLock) then
          barriers.Add(JCBarrier(barTurnoutEmLock, coupling))
        else if (TBlkTurnout(coupling).outputLocked) then
          barriers.Add(JCBarrier(barTurnoutLocked, coupling));

        if (TBlkTurnout(coupling).occupied = TTrackState.occupied) then
          barriers.Add(JCBarrier(barTrackOccupied, coupling));
      end;
    end;
  end;

  // railway
  if (Self.m_data.railwayId > -1) then
  begin
    var railway: TBlkRailway := TBlkRailway(Blocks.GetBlkByID(Self.m_data.railwayId));

    if (railway.direction = TRailwayDirection.disabled) then
      barriers.Add(JCBarrier(barBlockDisabled, railway));

    var cont: Boolean := true;
    if (railway.departureForbidden) then
    begin
      var warn: Boolean := false;
      case (railway.direction) of
        TRailwayDirection.AtoB:
          warn := (Self.typ = TJCType.shunt) and (TBlkLinker(railway.linkerA).departureForbidden) and
                  (not TBlkLinker(railway.linkerB).departureForbidden);
        TRailwayDirection.BtoA:
          warn := (Self.typ = TJCType.shunt) and (TBlkLinker(railway.linkerB).departureForbidden) and
                  (not (TBlkLinker(railway.linkerA).departureForbidden));
      end;

      if (warn) then
        barriers.Add(JCBarrier(barRailwayZAKPC, railway))
      else
        barriers.Add(JCBarrier(barRailwayZAKVC, railway));
    end;
    if (railway.request) then
      barriers.Add(JCBarrier(barRailwayRequesting, railway));
    if (((TBlkRailway(railway).Zaver) or (TBlkRailway(railway).emLock)) and
      (Self.m_data.railwayDir <> TBlkRailway(railway).direction)) then
    begin
      barriers.Add(JCBarrier(barRailwayWrongDir, railway));
      cont := false;
    end;
    if ((cont) and (railway.Zaver)) then
      barriers.Add(JCBarrier(barRailwayZaver, railway));

    if (cont) and ((not railway.SameUserBothLinkers()) or (railway.emLock)) then
      if (((railway.GetSettings().rType = TRailwayType.permanent) or
        (railway.GetSettings().rType = TRailwayType.request)) and
        (Self.m_data.railwayDir <> railway.direction)) then
      begin
        barriers.Add(JCBarrier(barRailwayWrongDir, railway));
        cont := false;
      end;

    if ((cont) and (Self.m_data.railwayDir <> railway.direction)) then
    begin
      // trat beze smeru, do ktere bude dle predchozi podminky povoleno vjet -> trat s automatickou zmenou souhlasu
      // -> kontrola volnosti vsech useku trati (protoze nastane zmena smeru)
      if (not railway.ready) then
      begin
        barriers.Add(JCBarrier(barRailwayWrongDir, railway));
        cont := false;
      end;
    end;

    if ((cont) and (Self.typ = TJCType.Train)) then
    begin
      if (TBlkRT(Self.lastTrack).sectOccupied = TTrackState.occupied) then
        barriers.Add(JCBarrier(barRailwayOccupied, railway))
      else if (not TBlkRT(Self.lastTrack).sectReady) then
        barriers.Add(JCBarrier(barRailwayNotReady, railway));
    end;

    // kontrola stitku uvazky v nasi OR:
    if ((TBlkLinker(railway.linkerA).areas.Count > 0) and
      (TBlkLinker(railway.linkerA).areas[0] = Self.m_state.senderOR) and
      (TBlkLinker(railway.linkerA).note <> '')) then
      barriers.Add(JCBarrier(barBlockNote, TBlkLinker(railway.linkerA)));

    if ((TBlkLinker(railway.linkerB).areas.Count > 0) and
      (TBlkLinker(railway.linkerB).areas[0] = Self.m_state.senderOR) and
      (TBlkLinker(railway.linkerB).note <> '')) then
      barriers.Add(JCBarrier(barBlockNote, TBlkLinker(railway.linkerB)));

    // stitky a vyluky na tratovych usecich
    for var trackId: Integer in railway.GetSettings().trackIds do
    begin
      var track: TBlkTrack := TBlkTrack(Blocks.GetBlkByID(trackId));

      if (track.lockout <> '') then
        barriers.Add(JCBarrier(barBlockLockout, track));
      if (track.note <> '') then
        barriers.Add(JCBarrier(barBlockNote, track));
    end;
  end;

  // locks
  for var refZaver: TJCRefZav in Self.m_data.locks do
  begin
    var lock: TBlkLock := TBlkLock(Blocks.GetBlkByID(refZaver.Block));
    if (lock.keyReleased) then
      barriers.Add(JCBarrier(barLockNotLocked, lock));
  end;

  // stolen engine
  var signalTrack: TBlkTrack;
  signalTrack := TBlkTrack(signal.track);

  if (signalTrack.IsTrain()) then
  begin
    var someHVsRuc := false;
    var train: TTrain := Self.GetTrain(signal, signalTrack);

    // manual-controlled engline
    if (Self.typ = TJCType.Train) then
      for var addr: Integer in train.HVs do
        if ((HVDb[addr].data.typ <> THVType.car) and ((HVDb[addr].stolen) or (HVDb[addr].ruc))) then
        begin
          barriers.Add(JCBarrier(barHVManual, nil, addr));
          someHVsRuc := true;
        end;

    // only some manual-controlled englines
    if (someHVsRuc) then
      for var addr: Integer in train.HVs do
        if ((HVDb[addr].data.typ <> THVType.car) and (not HVDb[addr].stolen) and (not HVDb[addr].ruc)) then
        begin
          barriers.Add(JCBarrier(barHVNotAllManual));
          break;
        end;

    // direction of a train
    if (Self.typ = TJCType.Train) then
    begin
      if (Train.sdata.dir_L or Train.sdata.dir_S) then
        if (((signal.direction = THVSite.odd) and (not Train.sdata.dir_L)) or
          ((signal.direction = THVSite.even) and (not Train.sdata.dir_S))) then
          barriers.Add(JCBarrier(barTrainWrongDir, nil, train.index));
    end;

  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TJC.BarriersNC(var barriers: TList<TJCBarrier>);
begin
  { nouzovou cestu nelze postavit pres:
    1) useky se zaverem
    2) vyhybky s nouzovym zaverem
    jinak lze vsechy bariery prekonat
  }

  // tracks
  var tracksCount: Integer;
  if (Self.m_data.railwayId > -1) then
    tracksCount := Self.m_data.tracks.Count - 1
  else
    tracksCount := Self.m_data.tracks.Count;

  for var i: Integer := 0 to tracksCount-1 do
  begin
    var track: TBlkTrack := TBlkTrack(Blocks.GetBlkByID(Self.m_data.tracks[i]));

    if (track.Zaver <> TZaver.no) then
    begin
      if (track.Zaver = TZaver.ab) then
        barriers.Add(JCBarrier(barTrackAB, track))
      else
        barriers.Add(JCBarrier(barTrackZaver, track));
    end;

    if (track.lockout <> '') then
      barriers.Add(JCBarrier(barBlockLockout, track));

    if (track.note <> '') then
      barriers.Add(JCBarrier(barBlockNote, track));

    if (track.PstIs()) then
      barriers.Add(JCBarrier(barTrackPSt, track));
  end;

  // turnouts
  for var turnoutZav: TJCTurnoutZav in Self.m_data.turnouts do
  begin
    var turnout: TBlkTurnout := TBlkTurnout(Blocks.GetBlkByID(turnoutZav.Block));

    if (turnout.lockout <> '') then
      barriers.Add(JCBarrier(barBlockLockout, turnout));

    if (turnout.note <> '') then
      barriers.Add(JCBarrier(barBlockNote, turnout));

    if (turnout.PstIs()) then
      barriers.Add(JCBarrier(barTurnoutPst, turnout));

    if (turnout.position <> turnoutZav.position) then
    begin
      if (turnout.emLock) then
        barriers.Add(JCBarrier(barTurnoutEmLock, turnout))
      else if (turnout.outputLocked) then
        barriers.Add(JCBarrier(barTurnoutLocked, turnout));
    end;

    // turnout's refugee
    var coupling: TBlkTurnout := TBlkTurnout(Blocks.GetBlkByID(turnout.GetSettings.coupling));
    // pokud nemam ja polohu, predpokladam, ze spojka bude muset byt prestavena -> musi byt volna, bez zaveru, ...
    // kontrolovat zaver z useku eni potreba - pokud je problem se zaverem, vyvstane uz na useku JC, jinak je vyhybka v poloze, ktere zaver nevadi
    if ((coupling <> nil) and (turnout.position <> turnoutZav.position)) then
    begin
      if (TBlkTurnout(coupling).emLock) then
        barriers.Add(JCBarrier(barTurnoutEmLock, coupling))
      else if (TBlkTurnout(coupling).outputLocked) then
        barriers.Add(JCBarrier(barTurnoutLocked, coupling));
    end;

    if ((coupling <> nil) and (coupling.PstIs())) then
      barriers.Add(JCBarrier(barTurnoutPst, coupling));
  end;

  // crossings
  for var crossingZav: TJCCrossingZav in Self.m_data.crossings do
  begin
    var crossing: TBlkCrossing := TBlkCrossing(Blocks.GetBlkByID(crossingZav.crossingId));
    if (crossing.note <> '') then
      barriers.Add(JCBarrier(barBlockNote, crossing));
  end;

  // refugees
  for var refugeeZav: TJCRefugeeZav in Self.m_data.refuges do
  begin
    var refugee: TBlkTurnout := TBlkTurnout(Blocks.GetBlkByID(refugeeZav.Block));

    if (refugee.lockout <> '') then
      barriers.Add(JCBarrier(barBlockLockout, refugee));

    if (refugee.note <> '') then
      barriers.Add(JCBarrier(barBlockNote, refugee));

    if (refugee.position <> refugeeZav.position) then
    begin
      if (refugee.emLock) then
        barriers.Add(JCBarrier(barTurnoutEmLock, refugee))

      else if ((refugee.Zaver <> TZaver.no) or (refugee.outputLocked)) then
        barriers.Add(JCBarrier(barRefugeeLocked, refugee));
    end;

    if (refugee.PstIs()) then
      barriers.Add(JCBarrier(barRefugeePst, refugee));

    // refugee's coupling
    var coupling: TBlkTurnout := TBlkTurnout(Blocks.GetBlkByID(refugee.GetSettings.coupling));
    if (coupling <> nil) then
    begin
      if (coupling.lockout <> '') then
        barriers.Add(JCBarrier(barBlockLockout, coupling));

      if (coupling.note <> '') then
        barriers.Add(JCBarrier(barBlockNote, coupling));

      if (coupling.PstIs()) then
        barriers.Add(JCBarrier(barRefugeePst, coupling));

      if (refugee.position <> refugeeZav.position) then
      begin
        if (coupling.Zaver > TZaver.no) then
        begin
          if (coupling.Zaver = TZaver.ab) then
            barriers.Add(JCBarrier(barTrackAB, coupling))
          else
            barriers.Add(JCBarrier(barTrackZaver, coupling));
        end;

        if (coupling.emLock) then
          barriers.Add(JCBarrier(barTurnoutEmLock, coupling))
        else if (coupling.outputLocked) then
          barriers.Add(JCBarrier(barTurnoutLocked, coupling))
      end;
    end;
  end;

  // railway
  if (Self.m_data.railwayId > -1) then
  begin
    var railway: TBlkRailway := TBlkRailway(Blocks.GetBlkByID(Self.m_data.railwayId));

    for var trackId: Integer in railway.GetSettings().trackIds do
    begin
      var track: TBlkTrack := TBlkTrack(Blocks.GetBlkByID(trackId));

      if (track.lockout <> '') then
        barriers.Add(JCBarrier(barBlockLockout, track));
      if (track.note <> '') then
        barriers.Add(JCBarrier(barBlockNote, track));
    end;

    // kontrola stitku uvazky v nasi OR:
    if ((TBlkLinker(railway.linkerA).areas.Count > 0) and
      (TBlkLinker(railway.linkerA).areas[0] = Self.m_state.senderOR) and
      (TBlkLinker(railway.linkerA).note <> '')) then
      barriers.Add(JCBarrier(barBlockNote, TBlkLinker(railway.linkerA)));

    if ((TBlkLinker(railway.linkerB).areas.Count > 0) and
      (TBlkLinker(railway.linkerB).areas[0] = Self.m_state.senderOR) and
      (TBlkLinker(railway.linkerB).note <> '')) then
      barriers.Add(JCBarrier(barBlockNote, TBlkLinker(railway.linkerB)));
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TJC.Log(msg: string; typ: LogType = ltJC);
begin
  Logging.Log('JC ' + Self.name + ': ' + msg, typ);
end;

/// /////////////////////////////////////////////////////////////////////////////

// stavi konkretni jizdni cestu
// tato fce ma za ukol zkontrolovat vstupni podminky jizdni cesty
// tato funkce jeste nic nenastavuje!
function TJC.Activate(senderPnl: TIdContext; senderOR: TObject; bariery_out: TJCBarriers; from_stack: TObject = nil;
  nc: Boolean = false; fromAB: Boolean = false; abAfter: Boolean = false): Integer;
begin
  Self.m_state.timeOut := Now + EncodeTime(0, _JC_INITPOTVR_TIMEOUT_SEC div 60, _JC_INITPOTVR_TIMEOUT_SEC mod 60, 0);

  Self.m_state.from_stack := from_stack;
  Self.m_state.senderOR := senderOR;
  Self.m_state.senderPnl := senderPnl;
  Self.m_state.nc := nc;
  Self.m_state.ab := (abAfter) and (Self.typ = TJCType.Train);
  Self.m_state.crossingWasClosed := false;
  Self.m_state.lastTrackOrRailwayOccupied := false;

  Self.Log('Požadavek na stavění, kontroluji podmínky');

  var barriers: TJCBarriers := Self.barriers(Self.m_state.nc);
  var UPO: TUPOItems := TList<TUPOItem>.Create;
  try
    // ignorujeme AB zaver pokud je staveno z AB seznamu
    if (fromAB) then
      for var i: Integer := barriers.Count - 1 downto 0 do
        if (barriers[i].typ = barTrackAB) then
          barriers.Delete(i);

    // existuji kriticke bariery?
    var critical: Boolean := false;
    for var barrier: TJCBarrier in barriers do
    begin
      if ((barrier.typ = barTrackLastOccupied) or (barrier.typ = barRailwayOccupied)) then
        Self.m_state.lastTrackOrRailwayOccupied := true;

      if ((JCBarriers.CriticalBarrier(barrier.typ)) or (not JCBarriers.JCWarningBarrier(barrier.typ))) then
      begin
        critical := true;
        UPO.Add(JCBarrierToMessage(barrier));
      end;
    end;

    if (critical) then
    begin
      // kriticke bariey existuji -> oznamim je
      Self.Log('Celkem ' + IntToStr(barriers.Count) + ' bariér, ukončuji stavění');
      if (senderPnl <> nil) then
      begin
        Self.step := stepCritBarriers;
        PanelServer.UPO(Self.m_state.senderPnl, UPO, true, nil, Self.CritBarieraEsc, Self);
      end;
      Exit(1);
    end else begin
      // bariery k potvrzeni
      if (((barriers.Count > 0) or ((nc) and (from_stack <> nil))) and (senderPnl <> nil)) then
      begin
        Self.Log('Celkem ' + IntToStr(barriers.Count) + ' warning bariér, žádám potvrzení...');
        for var i: Integer := 0 to barriers.Count - 1 do
          UPO.Add(JCBarrierToMessage(barriers[i]));

        // pokud se jedna o NC ze zasobniku, zobrazuji jeste upozorneni na NC
        if ((nc) and (from_stack <> nil)) then
        begin
          var item: TUPOItem;
          item[0] := GetUPOLine('Pozor !', taCenter, clYellow, $A0A0A0);
          item[1] := GetUPOLine('Stavění nouzové cesty.');
          item[2] := GetUPOLine('');
          UPO.Add(item);
        end;

        PanelServer.UPO(Self.m_state.senderPnl, UPO, false, Self.UPO_OKCallback, Self.UPO_EscCallback, Self);
        Self.step := stepConfBarriers;
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
    UPO.Free();
  end;

  result := 0;
end;

function TJC.Activate(senderPnl: TIdContext; senderOR: TObject; from_stack: TObject = nil; nc: Boolean = false;
  fromAB: Boolean = false; abAfter: Boolean = false): Integer;
begin
  result := Self.Activate(senderPnl, senderOR, nil, from_stack, nc, fromAB, abAfter);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TJC.PS_vylCallback(Sender: TIdContext; success: Boolean);
begin
  // pro potvrzovaci sekvenci vyluky by mel byt krok '6'
  if (Self.step <> stepConfSeq) then
    Exit();

  if (not success) then
  begin
    Self.CancelActivating('');
    Exit();
  end;

  // znovu zkontrolujeme bariery (behem potvrzovani se mohly vyskytnout)
  var barriers: TJCBarriers := Self.barriers(Self.m_state.nc);

  try
    // existuji kriticke bariery?
    var critical: Boolean := false;
    for var barrier in barriers do
      if ((barrier.typ <> barProcessing) and ((JCBarriers.CriticalBarrier(barrier.typ)) or
        (not JCBarriers.JCWarningBarrier(barrier.typ)))) then
      begin
        critical := true;
        break;
      end;

    // behem potvrzovani se mohly vyskytnout
    if (critical) then
    begin
      Self.CancelActivating('Nelze postavit - kritické bariéry');
      if (Self.m_state.senderPnl <> nil) and (Self.m_state.senderOR <> nil) then
        PanelServer.BottomError(Self.m_state.senderPnl, 'Nelze postavit ' + Self.name + ' - kritické bariéry',
          (Self.m_state.senderOR as TArea).ShortName, 'TECHNOLOGIE');
      barriers.Free();
      Exit();
    end;

    Self.Log('Krok 2 : povrzovaci sekvence OK');
    Self.SetInitStep();
  finally
    barriers.Free();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////
// callbacky z upozornovacich barier:

procedure TJC.UPO_OKCallback(Sender: TObject);
begin
  if (Self.step <> stepConfBarriers) then
    Exit();

  Self.Log('Krok 1 : upozornění schválena, kontroluji znovu bariéry');

  // znovu zkontrolujeme bariery (behem potvrzovani se mohly vyskytnout)
  var barriers: TJCBarriers := Self.barriers(Self.m_state.nc);
  try
    // existuji kriticke bariery?
    var critical: Boolean := false;
    for var barrier in barriers do
      if ((barrier.typ <> barProcessing) and ((JCBarriers.CriticalBarrier(barrier.typ)) or
        (not JCBarriers.JCWarningBarrier(barrier.typ)))) then
      begin
        critical := true;
        break;
      end;

    // behem potvrzovani se mohly vyskytnout
    if (critical) then
    begin
      Self.CancelActivating('Nelze postavit - kritické bariéry');
      if (Self.m_state.senderPnl <> nil) and (Self.m_state.senderOR <> nil) then
        PanelServer.BottomError(Self.m_state.senderPnl, 'Nelze postavit ' + Self.name + ' - kritické bariéry',
          (Self.m_state.senderOR as TArea).ShortName, 'TECHNOLOGIE');
      barriers.Free();
      Exit();
    end;

    // existuji bariery na potvrzeni potvrzovaci sekvenci ?
    var conditions: TList<TConfSeqItem> := TList<TConfSeqItem>.Create;
    for var barrier in barriers do
    begin
      if (JCBarriers.IsCSBarrier(barrier.typ)) then
        conditions.Add(TArea.GetCSCondition(barrier.Block, JCBarriers.BarrierGetCSNote(barrier.typ)));
    end;

    if (conditions.Count > 0) then
    begin
      // ano, takoveto bariery existuji -> potvrzovaci sekvence
      Self.Log('Bariéry s potvrzovací sekvencí, žádám potvrzení...');

      if (Self.m_state.senderPnl <> nil) and (Self.m_state.senderOR <> nil) then
        PanelServer.ConfirmationSequence(Self.m_state.senderPnl, Self.PS_vylCallback, (Self.m_state.senderOR as TArea),
          'Jízdní cesta s potvrzením', TBlocks.GetBlksList(Self.signal, Self.lastTrack), conditions);

      Self.step := stepConfSeq;
    end else begin
      // ne, takoveto bariery neexistuji -> stavim jizdni cestu
      Self.SetInitStep();
    end;

  finally
    barriers.Free();
  end;
end;

procedure TJC.UPO_EscCallback(Sender: TObject);
begin
  if (Self.step = stepConfBarriers) then
  begin
    Self.CancelActivating();
    Self.step := stepDefault;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

// jakmile je zavolano StavJC(), tato funkce se stara o to, aby staveni doslo az do konce
// kontroluje prubezne podminky apod.
procedure TJC.UpdateActivating();
var
  npCall: ^TNPCallerData;
  remEvDataPtr: ^TRemoveEventData;
begin
  if ((not Self.activating) and (Self.step <> stepJcLastTrackWait)) then
    Exit();

  var signal: TBlkSignal := TBlkSignal(Self.signal);

  /// ///////////////////////////////////////////////////////////////////////////
  // staveni vlakovych a posunovych cest:

  case (Self.step) of
    stepJcInit:
      begin
        // nejprve priradime uvolneni zaveru posledniho bloku uvolneni zaveru predposledniho bloku
        if (Self.m_data.tracks.Count > 1) then
        begin
          var oneButLastTrack: TBlkTrack := TBlkTrack(Blocks.GetBlkByID(Self.m_data.tracks[Self.m_data.tracks.Count-2]));
          oneButLastTrack.AddChangeEvent(
            oneButLastTrack.eventsOnZaverReleaseOrAB,
            CreateChangeEvent(ceCaller.CopyUsekZaver, Self.lastTrack.id)
          );

          for var i: Integer := 0 to Self.m_data.tracks.Count - 2 do
          begin
            var track: TBlkTrack := TBlkTrack(Blocks.GetBlkByID(Self.m_data.tracks[i]));
            var nextTrack: TBlkTrack := TBlkTrack(Blocks.GetBlkByID(Self.m_data.tracks[i+1]));

            if (track.spnl.stationTrack) then
            begin
              var chEv: TChangeEvent := CreateChangeEvent(ceCaller.CopyUsekZaver, track.id);
              TBlk.AddChangeEvent(nextTrack.eventsOnZaverReleaseOrAB, chEv);
              GetMem(remEvDataPtr, SizeOf(TRemoveEventData));
              remEvDataPtr^ := TRemoveEventData.Create(nextTrack.eventsOnZaverReleaseOrAB, chEv);
              TBlk.AddChangeEvent(track.eventsOnZaverReleaseOrAB, CreateChangeEvent(ceCaller.RemoveEvent,
                Integer(remEvDataPtr)));
            end;
          end;
        end;

        Self.Log('Useky: nastavuji staveci zavery');
        for var trackZav: Integer in Self.m_data.tracks do
        begin
          var track: TBlkTrack := TBlkTrack(Blocks.GetBlkByID(trackZav));
          track.zaver := TZaver.staveni;
        end;

        Self.Log('Vyhybky: zamykam do pozadovanych poloh');
        Self.m_state.nextTurnout := -1;
        var stavim: Cardinal := 0;
        var nextTurnout: Integer := -1;
        for var i: Integer := 0 to Self.m_data.turnouts.Count - 1 do
        begin
          var turnoutZav: TJCTurnoutZav := Self.m_data.turnouts[i];
          var turnout: TBlkTurnout := TBlkTurnout(Blocks.GetBlkByID(Self.m_data.turnouts[i].Block));

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
          turnout.SetPosition(TTurnoutPosition(turnoutZav.position), true, false, Self.TurnoutMovedJCPC,
            Self.TurnoutErrJCPC);
        end;

        for var i: Integer := 0 to Self.m_data.refuges.Count - 1 do
        begin
          var refugeeZav: TJCRefugeeZav := Self.m_data.refuges[i];
          var refugee: TBlkTurnout := TBlkTurnout(Blocks.GetBlkByID(refugeeZav.Block));

          // nastaveni odvratu
          if (refugee.position <> TTurnoutPosition(refugeeZav.position)) then
          begin
            if (stavim >= _JC_MAX_VYH_STAVENI) then
            begin
              if (nextTurnout = -1) then
                nextTurnout := i;
              continue;
            end;
            Inc(stavim);
          end;

          refugee.IntentionalLock();

          // pridani zruseni redukce
          var track: TBlkTrack := TBlkTrack(Blocks.GetBlkByID(refugeeZav.ref_blk));
          track.AddChangeEvent(track.eventsOnZaverReleaseOrAB, CreateChangeEvent(ceCaller.NullVyhybkaMenuReduction,
            refugeeZav.Block));

          // Warning: this may call callback directly
          // Callback for just-locking turnout will have no effect due to nextVyhybka = -1
          refugee.SetPosition(TTurnoutPosition(refugeeZav.position), true, false, Self.TurnoutMovedJCPC,
            Self.TurnoutErrJCPC);
        end;

        Self.m_state.nextTurnout := nextTurnout;

        Self.Log('Zamky: nastavuji zavery');
        for var refZav: TJCRefZav in Self.m_data.locks do
        begin
          var refTrack: TBlkTrack := TBlkTrack(Blocks.GetBlkByID(refZav.ref_blk));
          refTrack.AddChangeEvent(refTrack.eventsOnZaverReleaseOrAB, CreateChangeEvent(ceCaller.NullZamekZaver, refZav.Block));

          // nastaveni zaveru zamku
          var lock: TBlkLock := TBlkLock(Blocks.GetBlkByID(refZav.Block));
          lock.zaver := true;
        end;

        Self.step := stepJcTurnoutsMoving;
        Self.Log('Vyhybky: poloha: detekce');
      end; // case 0

    stepJcTurnoutsMoving:
      begin
        for var turnoutZav: TJCturnoutZav in Self.m_data.turnouts do
        begin
          var turnout: TBlkTurnout := TBlkTurnout(Blocks.GetBlkByID(turnoutZav.Block));
          if (turnout.position <> turnoutZav.position) then
            Exit();
        end;
        for var refugeeZav: TJCRefugeeZav in Self.m_data.refuges do
        begin
          var turnout: TBlkTurnout := TBlkTurnout(Blocks.GetBlkByID(refugeeZav.Block));
          if (turnout.position <> refugeeZav.position) then
            Exit();
        end;

        Self.Log('Krok 11 : vyhybky: poloha: OK');
        Self.m_state.nextTurnout := -1;

        Self.Log('Krok 11: useky: nastavuji nouzovy zaver');
        for var trackZav: Integer in Self.m_data.tracks do
        begin
          var track: TBlkTrack := TBlkTrack(Blocks.GetBlkByID(trackZav));
          track.zaver := TZaver.nouz;
        end;

        Self.Log('Krok 11: useky: kontroluji volnost useku s neprofilovymi styky, zapevnuji neprofilove useky');
        for var turnoutZav: TJCTurnoutZav in Self.m_data.turnouts do
        begin
          var neprofil: TBlkTrack := nil;
          var turnout: TBlkTurnout := TBlkTurnout(Blocks.GetBlkByID(turnoutZav.Block));

          if ((turnoutZav.position = TTurnoutPosition.plus) and (turnout.npBlokPlus <> nil)) then
            neprofil := TBlkTrack(turnout.npBlokPlus)
          else if ((turnoutZav.position = TTurnoutPosition.minus) and (turnout.npBlokMinus <> nil)) then
            neprofil := TBlkTrack(turnout.npBlokMinus);

          if (neprofil <> nil) then
          begin
            if (neprofil.occupied <> TTrackState.Free) then
            begin
              if (Self.m_state.senderPnl <> nil) and (Self.m_state.senderOR <> nil) then
                PanelServer.BottomError(Self.m_state.senderPnl, 'Neuvolněn ' + neprofil.name,
                  (Self.m_state.senderOR as TArea).ShortName, 'TECHNOLOGIE');
              Self.Log('Krok 14 : Neprofilovy usek ' + neprofil.name + ' neuvolnen!');
              Self.CancelActivating();
              Exit();
            end;

            neprofil.AddNeprofilJC(Self.m_data.id);

            var turnoutTrack: TBlkTrack := TBlkTrack(Blocks.GetBlkByID(turnout.trackID));

            npCall := GetMemory(SizeOf(TNPCallerData));
            npCall.usekId := neprofil.id;
            npCall.jcId := Self.m_data.id;
            turnoutTrack.AddChangeEvent(turnoutTrack.eventsOnZaverReleaseOrAB, CreateChangeEvent(ceCaller.RemoveUsekNeprofil,
              Integer(npCall)));
          end;
        end;

        if ((signal.ZAM) or (Self.m_state.lastTrackOrRailwayOccupied)) then
          Self.step := stepJcFinalZaver
        else
          Self.step := stepJcCloseCross;
      end; // case 1

    stepJcCloseCross:
      begin
        // crossings
        Self.m_state.crossingWasClosed := true;
        var anyClosed: Boolean := false;
        for var i: Integer := 0 to Self.m_data.crossings.Count - 1 do
        begin
          var crossingZav: TJCCrossingZav := Self.m_data.crossings[i];
          if (crossingZav.closeTracks.Count = 0) then
            continue;

          var crossing: TBlkCrossing := TBlkCrossing(Blocks.GetBlkByID(crossingZav.crossingId));
          var closed: Boolean := false;

          // prejezd uzavirame jen v pripade, ze nejaky z jeho aktivacnich bloku je obsazen
          // v pripade posunove cesty uzavirame vzdy

          if (Self.typ = TJCType.shunt) then
          begin
            // posunova cesta:
            Self.Log('Krok 12 : prejezd ' + crossing.name + ' - uzaviram');

            crossing.zaver := true;

            // pridani zruseni redukce, tim se prejezd automaticky otevre po zruseni zaveru bloku pod nim
            var openTrack: TBlkTrack := TBlkTrack(Blocks.GetBlkByID(crossingZav.openTrack));
            openTrack.AddChangeEvent(
              openTrack.eventsOnZaverReleaseOrAB,
              CreateChangeEvent(ceCaller.NullPrejezdZaver, crossingZav.crossingId)
            );

            closed := true;
            anyClosed := true;
          end else begin

            // vlakova cesta:
            for var closeTrackId: Integer in crossingZav.closeTracks do
            begin
              var closeTrack: TBlkTrack := TBlkTrack(Blocks.GetBlkByID(closeTrackId));
              if (closeTrack.occupied = TTrackState.occupied) then
              begin
                Self.Log('Krok 12 : prejezd ' + crossing.name + ' - aktivacni usek ' + closeTrack.name +
                  ' obsazen - uzaviram');

                crossing.zaver := true;

                // pridani zruseni redukce, tim se prejezd automaticky otevre po zruseni zaveru bloku pod nim
                var openTrack: TBlkTrack := TBlkTrack(Blocks.GetBlkByID(crossingZav.openTrack));
                openTrack.AddChangeEvent(
                  openTrack.eventsOnZaverReleaseOrAB,
                  CreateChangeEvent(ceCaller.NullPrejezdZaver, crossingZav.crossingId)
                );

                closed := true;
                anyClosed := true;
                break;
              end;
            end; // for j
          end; // else posunova cesta

          if (not closed) then
          begin
            // prejezd neuzaviram -> pridam pozadavek na zavreni pri obsazeni do vsech aktivacnich useku
            for var closeTrackId: Integer in crossingZav.closeTracks do
            begin
              var closeTrack: TBlkTrack := TBlkTrack(Blocks.GetBlkByID(closeTrackId));
              if (not closeTrack.eventsOnOccupy.Contains(CreateChangeEvent(Self.TrackCloseCrossing, i))) then
                closeTrack.AddChangeEvent(closeTrack.eventsOnOccupy, CreateChangeEvent(Self.TrackCloseCrossing, i));
            end;

            Self.Log('Krok 12 : prejezd ' + crossing.name + ' - zadny aktivacni usek neobsazen - nechavam otevreny');
          end;
        end; // for i

        if (anyClosed) then
        begin
          Self.step := stepJcWaitCross;
          Self.m_state.timeOut := Now + EncodeTime(0, _JC_PRJ_TIMEOUT_SEC div 60, _JC_PRJ_TIMEOUT_SEC mod 60, 0);
        end
        else
          Self.step := stepJcFinalZaver;

      end;

    stepJcWaitCross:
      begin
        // kontrola stavu prejezdu
        for var crossingZav: TJCCrossingZav in Self.m_data.crossings do
        begin
          if (crossingZav.closeTracks.Count = 0) then
            continue;

          var crossing: TBlkCrossing := TBlkCrossing(Blocks.GetBlkByID(crossingZav.crossingId));

          if (crossing.state <> TBlkCrossingBasicState.closed) then
            Exit();
          Self.Log('Krok 13 : prejezd ' + crossing.name + ' uzavren');
        end; // for i

        Self.step := stepJcFinalZaver;
      end;

    stepJcFinalZaver:
      begin
        Self.Log('Krok 14 : useky: nastavit validni zaver');

        for var i: Integer := 0 to Self.m_data.tracks.Count - 1 do
        begin
          var trackId: Integer := Self.m_data.tracks[i];
          var track: TBlkTrack := TBlkTrack(Blocks.GetBlkByID(trackId));
          track.zaver := TZaver(Self.typ);
        end;

        signal.DNjc := Self;

        if (Self.IsCriticalBarrier()) then
        begin
          // Nepostavit navestidlo!
          Self.step := stepJcFinish;
          Exit();
        end;

        if ((signal.ZAM) or (Self.m_state.lastTrackOrRailwayOccupied)) then
        begin
          Self.Log('Krok 14 : navestidlo: nestavim');
          Self.step := stepJcFinish;
        end else begin
          Self.Log('Krok 14 : navestidlo: stavim...');
          Self.SetSignalSignal();
          Self.step := stepJcSignalWait;
        end;
      end; // case 14

    stepJcSignalWait:
      begin
        if (signal.signal > ncStuj) then
        begin
          Self.Log('Krok 15 : navestidlo postaveno');
          Self.step := stepJcFinish;
        end;
      end;

    stepJcFinish:
      begin
        Self.CancelSignalBegin();
        Self.CancelVBs();
        Self.CancelTrackEnd();

        // nastavit front blok soupravy
        var signalTrack: TBlkTrack := signal.track as TBlkTrack;
        if (signalTrack.IsTrain()) then
          Self.GetTrain(signal, signalTrack).front := signalTrack;

        if (not signalTrack.signalJCRef.Contains(signal)) then
          signalTrack.signalJCRef.Add(signal);

        signal.DNjc := Self;

        if (Self.m_state.lastTrackOrRailwayOccupied) then
          Self.step := stepJcLastTrackWait
        else
          Self.step := stepDefault;

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
          var railway: TBlkRailway := TBlkRailway(Blocks.GetBlkByID(Self.m_data.railwayId));

          if (Self.typ = TJCType.Train) then
            railway.zaver := true;

          // posledni blok posunove cesty je trat = posun mezi dopravnami -> zavedeme zakaz odjezdu do trati
          if (Self.typ = TJCType.shunt) then
          begin
            case (Self.m_data.railwayDir) of
              TRailwayDirection.AtoB:
                TBlkLinker(railway.linkerA).departureForbidden := true;
              TRailwayDirection.BtoA:
                TBlkLinker(railway.linkerB).departureForbidden := true;
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

        if (Self.typ = TJCType.Train) then
          Blocks.TrainPrediction(signal);

        // pokud je cesta ze zasobniku, smazeme ji odtam
        if (Self.m_state.from_stack <> nil) then
        begin
          (Self.m_state.from_stack as TORStack).RemoveJC(Self);
          Self.m_state.from_stack := nil;
        end;

        signal.PropagatePOdjToRailway();

        if ((Self.m_state.ab) and (not signal.ab)) then
          signal.ABJC := Self;

        Self.Log('Postavena JC ' + Self.name);
      end;

    stepJcLastTrackWait:
      begin
        var lastTrack: TBlkRT := TBlkRT(Self.lastTrack);

        if (Self.m_data.railwayId > -1) then
        begin
          if (lastTrack.sectReady) then
          begin
            Self.m_state.lastTrackOrRailwayOccupied := false;
            Self.DN();
          end;
        end else begin
          if ((lastTrack.occupied = TTrackState.Free) and (not lastTrack.IsTrain)) then
          begin
            Self.m_state.lastTrackOrRailwayOccupied := false;
            Self.DN();
          end;
        end;
      end;

    /// ////////////////////////////////////////////////////////////////////////
    // staveni nouzovych cest:

    stepNcInit:
      begin
        // vsem usekum nastavime staveci zaver:
        Self.Log('Krok 100: useky: nastavuji staveci zavery');
        for var trackZav: Integer in Self.m_data.tracks do
        begin
          var track: TBlkTrack := TBlkTrack(Blocks.GetBlkByID(trackZav));
          track.zaver := TZaver.staveni;
        end;

        // nastavit nouzovy zaver uvazky
        if (Self.m_data.railwayId > -1) then
        begin
          Self.Log('Krok 100: trat: nastavuji nouzovy zaver uvazky');
          var railway: TBlkRailway := TBlkRailway(Blocks.GetBlkByID(Self.m_data.railwayId));

          // najdeme si uvazku, ktera je v OR navestidla a te nastavime nouzovy zaver
          if ((railway.linkerA as TBlkLinker).areas.Count > 0) then
          begin
            for var area: TArea in signal.areas do
              if ((railway.linkerA as TBlkLinker).areas[0] = area) then
                (railway.linkerA as TBlkLinker).emLock := true;

            for var area: TArea in signal.areas do
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
          var turnoutZav: TJCTurnoutZav := Self.m_data.turnouts[Self.m_state.nextTurnout];
          var turnout: TBlkTurnout := TBlkTurnout(Blocks.GetBlkByID(turnoutZav.Block));

          Inc(Self.m_state.nextTurnout);
          turnout.SetPosition(TTurnoutPosition(turnoutZav.position),
            // this call could increase nextVyhybka directly! or even set nextVyhybka = -1
            true, false, Self.TurnoutMovedNC, Self.TurnoutErrNC);
        end;

        // For simplicity solve odvrat just in callback
        // This may be a little bit slower, but will generally work fine

        Self.Log('Krok 100: prejezdy: uzaviram');
        for var crossingZav: TJCCrossingZav in Self.m_data.crossings do
        begin
          if (crossingZav.closeTracks.Count = 0) then
            continue;

          var crossing: TBlkCrossing := TBlkCrossing(Blocks.GetBlkByID(crossingZav.crossingId));
          if (not crossing.pcEmOpen) then
            crossing.pcClosed := true;
        end;

        // nastavit nouzovy zaver zamkum
        for var refZav: TJCRefZav in Self.m_data.locks do
        begin
          var lock: TBlkLock := TBlkLock(Blocks.GetBlkByID(refZav.Block));
          lock.emLock := true;
          signal.AddBlkToRnz(lock.id, false);
        end;

        // nastavit nouzovy zaver PSt
        var psts := Self.PSts();
        try
          for var pst in psts do
          begin
            TBlkPst(pst).emLock := true;
            signal.AddBlkToRnz(pst.id, false);
          end;
        finally
          psts.Free();
        end;

        Self.m_state.ncBarieryCntLast := -1; // tady je potreba mit cislo < 0

        Self.step := stepNcBarrierUpdate;
      end; // case 100

    stepNcBarrierUpdate:
      begin
        // prubezne kontroluji podminky a zobrazuji potvrzovaci sekvenci

        // zjistime aktualni bariery:
        Self.m_state.ncBariery.Clear();
        Self.BarriersNCToAccept(Self.m_state.ncBariery);

        // kontrolujeme rozdilnost seznamu:
        if (Self.m_state.ncBariery.Count <> Self.m_state.ncBarieryCntLast) then
        begin
          Self.Log('Krok 101: zmena potvr., odesilam aktualni seznam');
          var str: string;
          if (Self.typ = TJCType.Train) then
            str := 'Zapnutí přivolávací návěsti'
          else
            str := 'Nouzová posunová cesta';

          if (Self.m_state.senderPnl <> nil) and (Self.m_state.senderOR <> nil) then
            PanelServer.ConfirmationSequence(Self.m_state.senderPnl, Self.NC_PS_Callback,
              Self.m_state.senderOR as TArea, str, TBlocks.GetBlksList(signal, lastTrack),
              JCBarriers.BarriersToConfSeq(Self.m_state.ncBariery));
        end;
        Self.m_state.ncBarieryCntLast := Self.m_state.ncBariery.Count;

        // nastavovani smeru trati:
        if (Self.m_data.railwayId > -1) then
        begin
          var railway: TBlkRailway := TBlkRailway(Blocks.GetBlkByID(Self.m_data.railwayId));

          // pokud v trati neni zavedena blokova podminka, zavedeme ji
          if ((Self.typ = TJCType.Train) and (railway.direction = Self.data.railwayDir) and (not railway.BP)) then
            railway.BP := true;

          // posledni blok posunove cesty je trat = posun mezi dopravnami -> zavedeme zakaz odjezdu do trati
          if ((Self.typ = TJCType.shunt) and (railway.direction = Self.m_data.railwayDir)) then
          begin
            case (Self.m_data.railwayDir) of
              TRailwayDirection.AtoB:
                if (not TBlkLinker(railway.linkerA).departureForbidden) then
                  TBlkLinker(railway.linkerA).departureForbidden := true;
              TRailwayDirection.BtoA:
                if (not TBlkLinker(railway.linkerB).departureForbidden) then
                  TBlkLinker(railway.linkerB).departureForbidden := true;
            end;
          end;
        end;
      end;

    stepNcBarrierConfirmed:
      begin
        // potrvzovaci sekvence potvrzena -> stavim navestidlo, ...

        Self.m_state.nextTurnout := -1;
        Self.Log('Krok 102: useky: rusim zavery');
        for var trackZav: Integer in Self.m_data.tracks do
        begin
          var track: TBlkTrack := TBlkTrack(Blocks.GetBlkByID(trackZav));
          track.zaver := TZaver.no;
        end;

        signal.privol := Self;

        // i pokud je navetidlo ve STUJ, nastavuji navest (to je spravne chovani podle JOP)
        if ((Self.typ = TJCType.Train) and (signal.enabled)) then
        begin
          Self.SetSignalSignal();
          Self.Log('Krok 102 : navestidlo: nastavuji na privolavaci navest...');
          Self.step := stepNcSignalWait;
        end
        else
          Self.step := stepNcFinish;
      end;

    stepNcSignalWait:
      begin
        if (signal.signal = ncPrivol) then
        begin
          Self.Log('Krok 103 : navestidlo postaveno');
          Self.step := stepNcFinish;
        end;
      end;

    stepNcFinish:
      begin
        Self.CancelSignalBegin();
        Self.CancelVBs();
        Self.CancelTrackEnd();

        Self.step := stepDefault;

        // pokud je cesta ze zasobniku, smazeme ji odtam
        if (Self.m_state.from_stack <> nil) then
        begin
          (Self.m_state.from_stack as TORStack).RemoveJC(Self);
          Self.m_state.from_stack := nil;
        end;

        // presun soupravy z useku pred navestidlem do posledniho useku JC

        // Presun probehne za techto podminek:
        // a) Bud privolavame do stanice = na dopravni kolej
        // b) Nebo privolavame do trate, ktera MUSI byt ve spravnem smeru a MUSI v ni byt zavedena blokova podminka

        if (Self.typ = TJCType.Train) then
        begin
          var signalTrack: TBlkTrack := signal.track as TBlkTrack;
          var train: TTrain := Self.GetTrain(signal, signalTrack);

          // a)
          if ((lastTrack.typ = btTrack) and (TBlkTrack(Self.lastTrack).spnl.stationTrack) and
            (not TBlkTrack(Self.lastTrack).TrainsFull())) then
          begin
            if (signalTrack.IsTrain()) then
            begin
              if ((signalTrack.typ = btRT) and (TBlkRT(signalTrack).inRailway > -1)) then
              begin
                var railway: TBlkRailway := TBlkRailway(Blocks.GetBlkByID((signalTrack as TBlkRT).inRailway));
                railway.RemoveTrain(train);
              end;

              // na dopravni kolej vlozime soupravu blize vjezdovemu navestidlu
              if (signal.direction = THVSite.odd) then
                TBlkTrack(Self.lastTrack).AddTrainL(Train)
              else
                TBlkTrack(Self.lastTrack).AddTrainS(Train);

              signalTrack.RemoveTrain(Train);
              train.front := Self.lastTrack;
            end;
            Self.m_state.destroyBlock := _JC_DESTROY_NC;
          end;

          // b)
          var railway: TBlkRailway;
          if ((lastTrack.typ = btRT) and ((lastTrack as TBlkRT).inRailway > -1)) then
            railway := TBlkRailway(Blocks.GetBlkByID((lastTrack as TBlkRT).inRailway))
          else
            railway := nil;

          if ((railway <> nil) and (signalTrack.IsTrain()) and (lastTrack.typ = btRT) and
            ((lastTrack as TBlkRT).inRailway = Self.data.railwayId)) then
          begin
            var rtAdd: TBlkRT := nil;

            if (railway.lockout) then
            begin
              // Pridat soupravu do posledniho bloku trati
              if ((railway.state.trains.Count = 0) and ((railway.GetLastTrack(Self.data.railwayDir) as TBlkRT)
                .Zaver = TZaver.no)) then
              begin
                rtAdd := (railway.GetLastTrack(Self.data.railwayDir) as TBlkRT);
                railway.TrainChangeOR(Train, Self.data.railwayDir);
                if (railway.ChangesTrainDir()) then
                  train.ChangeDirection();
              end;
            end else begin
              if ((not TBlkTrack(Self.lastTrack).IsTrain()) and (railway.BP) and
                (railway.direction = Self.data.railwayDir)) then
              begin
                // Pridat soupravu do prvniho bloku trati
                rtAdd := (lastTrack as TBlkRT);
                rtAdd.bpError := true;
              end;
            end;

            if (rtAdd <> nil) then
            begin
              railway.AddTrain(TBlkRailwayTrain.Create(Train.index));
              rtAdd.AddTrainL(Train); // tady je jedno jestli zavolat L nebo S
              // v trati muze byt na jednom useku vzdy jen jedna souprava
              // kontrolovano vyse
              railway.Change();
              signalTrack.RemoveTrain(train);
              train.front := rtAdd;
            end;
          end;
        end; // if typ = vlak

        Self.Log('Postavena NC ' + Self.name);
      end; // case 102
  end; // case
end;

/// /////////////////////////////////////////////////////////////////////////////

// je volana, pokud behem staveni dojde k vyjimce
// napriklad pri kontrole obsazenosti useku v JC apod.
procedure TJC.CancelActivating(reason: string = ''; stack_remove: Boolean = false);
begin
  if (reason <> '') then
  begin
    if (Self.m_state.senderPnl <> nil) then
      PanelServer.SendInfoMsg(Self.m_state.senderPnl, reason);
    Self.Log('Nelze postavit - ' + reason);
  end;

  case (Self.step) of
    stepNcBarrierUpdate:
      begin
        if (Self.m_state.senderPnl <> nil) then
          PanelServer.CSClose(Self.m_state.senderPnl, reason);
      end
  end;

  // staveci zavery jsou zruseny, ostatni zavery zustavaji (lze je vyNUZovat)
  for var trackZaver: Integer in Self.data.tracks do
  begin
    var track: TBlkTrack := TBlkTrack(Blocks.GetBlkByID(trackZaver));
    if (track.Zaver = TZaver.staveni) then
      track.Zaver := TZaver.no;
  end;

  Self.m_state.nextTurnout := -1;
  Self.step := stepDefault;
  Self.m_state.nc := false;
  Self.m_state.ab := false;
  Self.m_state.crossingWasClosed := false;
  Self.CancelSignalBegin();
  Self.CancelVBs();
  Self.CancelTrackEnd();
  if (Self.m_state.senderPnl <> nil) then
    PanelServer.CancelUPO(Self.m_state.senderPnl, Self);
  if (Self.m_state.from_stack <> nil) then
    if (stack_remove) then
      (Self.m_state.from_stack as TORStack).RemoveJC(Self)
    else if (Self.m_state.senderOR <> nil) then
      (Self.m_state.senderOR as TArea).BroadcastData('ZAS;FIRST;1');

  Self.m_state.from_stack := nil;
end;

/// /////////////////////////////////////////////////////////////////////////////

// rusi zacatek jizdni cesty
procedure TJC.CancelSignalBegin();
begin
  if (Self.signal = nil) then
    Exit();
  if (Self.signal.typ <> btSignal) then
    Exit();
  if ((Self.signal as TBlkSignal).selected = TBlkSignalSelection.none) then
    Exit();

  (Self.signal as TBlkSignal).selected := TBlkSignalSelection.none;
  if ((Self.signal as TBlkSignal).DNjc = Self) then
    (Self.signal as TBlkSignal).DNjc := nil;

  Self.Log('Zrusen zacatek staveni VC na bloku ' + Self.signal.name);
end;

// rusi konec jizdni cesty
procedure TJC.CancelTrackEnd();
begin
  if (Self.lastTrack <> nil) then
    TBlkTrack(Self.lastTrack).jcEnd := TZaver.no;
end;

procedure TJC.CancelVBs();
begin
  for var vb: Integer in Self.data.vb do
  begin
    var blk: TBlk := Blocks.GetBlkByID(vb);
    if ((blk <> nil) and ((blk.typ = btTrack) or (blk.typ = btRT))) then
      (blk as TBlkTrack).jcEnd := TZaver.no;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TJC.Cancel(Sender: TObject = nil);
begin
  Self.CancelWithoutTrackRelease();

  (Self.signal as TBlkSignal).DNjc := nil;
  (Self.signal as TBlkSignal).RCtimerTimeout();

  for var trackZaver: Integer in Self.m_data.tracks do
  begin
    var track: TBlkTrack := TBlkTrack(Blocks.GetBlkByID(trackZaver));
    track.Zaver := TZaver.no;
  end;

  // zaver trati se rusi automaticky uvolnenim zaveru posledniho bloku pred trati

  Self.Log('Zrusena');
end;

// ruseni jizdni cesty bez ruseni zaveru bloku
procedure TJC.CancelWithoutTrackRelease();
begin
  var signal: TBlkSignal := TBlkSignal(Self.signal);
  Self.Log('Probiha ruseni navesti');

  if ((signal.DNjc = Self) and (signal.signal > ncStuj)) then
  begin
    signal.signal := ncStuj;
    if (signal.ab) then
    begin
      signal.ab := false; // automaticky zrusi AB
      if (Self.m_state.senderPnl <> nil) then
        PanelServer.BottomError(Self.m_state.senderPnl, 'Zrušena AB ' + signal.name,
          (Self.m_state.senderOR as TArea).ShortName, 'TECHNOLOGIE');
    end;
  end;

  Self.step := stepDefault;
  Self.destroyBlock := _JC_DESTROY_NONE;
  Self.destroyEndBlock := _JC_DESTROY_NONE;
end;

/// /////////////////////////////////////////////////////////////////////////////

// RozpadBlok = blok index, kam by mela souprava vjet
// RozpadRuseniBlok = blok index, kde je posledni detekovany vagon soupravy
procedure TJC.DynamicCanceling();
begin
  var signal: TBlkSignal := TBlkSignal(Self.signal);

  // kontrola obsazenosti useku pred navestidlem
  var signalTrack: TBlkTrack := signal.track as TBlkTrack;
  if ((Self.destroyBlock = _JC_DESTROY_SIGNAL_TRACK) and ((signalTrack.occupied <> TTrackState.Free) or
    (signalTrack.GetSettings.RCSAddrs.Count = 0))) then
  begin
    Self.destroyBlock := 0;
    Self.destroyEndBlock := _JC_DESTROY_SIGNAL_TRACK;
  end;

  // uvolneni prvniho useku pred navestidlem v posunove ceste je signalem pro zhasnuti navestidla
  if ((signalTrack.GetSettings().RCSAddrs.Count > 0) and (signalTrack.occupied = TTrackState.Free) and (signal.signal <> ncStuj) and
    (Self.destroyEndBlock = _JC_DESTROY_SIGNAL_TRACK) and (Self.typ = TJCType.shunt) and (Self.destroyBlock >= 1)) then
  begin
    Self.Log('Uvolnen usek ' + signalTrack.name + ' : navestidlo ' + signal.name + ' nastaveno na STUJ');
    signal.JCCancelSignal();
  end;

  for var i: Integer := Self.destroyBlock to Self.m_data.tracks.Count - 1 do
  begin
    if (i < 0) then
      continue; // i = -1 kdyz se kontroluje blok pred navestidlem, -2 pokud je navestidlo na STUJ, nebo zamkle

    var track: TBlkTrack := TBlkTrack(Blocks.GetBlkByID(Self.m_data.tracks[i]));

    // druha cast podminky je tu pro pripad, kdy by byl na konci posunove cesty obsazeny usek
    if ((track.occupied = occupied) and ((i < Self.m_data.tracks.Count - 1) or
      (Self.destroyBlock > Self.m_data.tracks.Count - 2) or (Self.typ <> TJCType.shunt))) then
    begin
      if (i = Self.destroyBlock) then
      begin
        track.Zaver := TZaver.nouz;

        if (Self.typ = TJCType.Train) then
          Self.MoveTrainToNextTrack();

        // obsazeni useku rusiciho navest (obvykle 0. usek, u skupinoveho navestidla byva jiny)
        // pozor: toto musi byt na tomto miste kvuli nastavovani Souprava.front
        if ((i = Self.m_data.signalFallTrackI) and (signal.signal <> ncStuj) and (Self.typ = TJCType.Train)) then
        begin
          // navestidlo pri obsazeni useku rusime jen v pripade, ze se jedna o VC
          Self.Log('Obsazen usek ' + track.name + ' : navestidlo ' + signal.name + ' nastaveno na STUJ');
          signal.JCCancelSignal();

          // aktualizace casu odjezdu v trati
          if (Self.m_data.railwayId > -1) then
          begin
            var railway: TBlkRailway := TBlkRailway(Blocks.GetBlkByID(Self.m_data.railwayId));
            if (railway.TrainPredict <> nil) then
            begin
              railway.TrainPredict.time := timeHelper.hJOPnow();
              railway.TrainPredict.predict := false;
              railway.Change();
            end;
          end;
        end;

        Self.destroyBlock := Self.destroyBlock + 1;

        // pokud jsme v predposlednim useku a posledni je nedetekovany, posuneme RozpadBlok jeste o jeden usek, aby se cesta mohla zrusit
        if (i = Self.m_data.tracks.Count - 2) then
          if (TBlkTrack(Self.lastTrack).GetSettings().RCSAddrs.Count = 0) then
            Self.destroyBlock := Self.destroyBlock + 1;

        if ((i = Self.m_data.tracks.Count - 1) and (Self.m_data.railwayId > -1)) then
        begin
          // posledni usek JC obsazen -> trat
          var railway: TBlkRailway := TBlkRailway(Blocks.GetBlkByID(Self.m_data.railwayId));

          if (Self.typ = TJCType.Train) then
          begin
            railway.BP := true;
            if (track.IsTrain()) then
            begin
              if ((railway.TrainPredict <> nil) and (railway.TrainPredict.Train = track.Train))
              then
                railway.AddTrain(railway.TrainPredict)
              else
                railway.AddTrain(TBlkRailwayTrain.Create(track.TrainI));
            end;
          end;
          railway.Zaver := false;

          // nastavime rychlost souprave
          if (Self.typ = TJCType.Train) then
            TBlkRT(track).speedUpdate := true;
        end;

      end else begin // if Self.rozpadBlok = 0
        if (track.Zaver > TZaver.no) then
        begin
          // pokud jsme na jinem useku, nez RozpadBlok
          if ((signal.targetSignal > ncStuj) and (signal.DNjc = Self)) then
          begin
            if (Self.m_state.senderPnl <> nil) and (Self.m_state.senderOR <> nil) then
              PanelServer.BottomError(Self.m_state.senderPnl, 'Chyba povolovací návěsti ' + signal.name,
                (Self.m_state.senderOR as TArea).ShortName, 'TECHNOLOGIE');
            Self.CancelWithoutTrackRelease();
          end;

          // v trati zaver nerusime, nesmime tam dat ani nouzovy, ani zadny zaver
          if ((i <> Self.m_data.tracks.Count - 1) or (Self.m_data.railwayId = -1)) then
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

  end;

  // jizdni cesta konci uvolnenim predposledniho useku

  // mensitko je dulezite a ma smysl !
  // kdyby tam bylo <=, mohl by se rozpadnout jediny usek, na kterem je souprava tim, ze se odobsadi
  if ((Self.destroyEndBlock >= 0) and (Self.destroyEndBlock < Self.destroyBlock - 1)) then
  begin
    // ziskani dotazovaneho useku
    var track: TBlkTrack := TBlkTrack(Blocks.GetBlkByID(Self.m_data.tracks[Self.destroyEndBlock]));

    var nextTrack: TBlkTrack;
    if (Self.destroyEndBlock + 1 < Self.m_data.tracks.Count) then
      Blocks.GetBlkByID(Self.m_data.tracks[Self.destroyEndBlock + 1], TBlk(nextTrack))
    else
      nextTrack := nil;

    if ((track.Zaver = TZaver.nouz) and (track.occupied = TTrackState.Free) and
      ((nextTrack = nil) or (nextTrack.occupied = TTrackState.occupied) or (nextTrack.GetSettings.RCSAddrs.Count = 0)))
    then
    begin
      // cesta se rozpada...
      if (Self.ab) then
        track.Zaver := TZaver.ab
      else
        track.Zaver := TZaver.no;

      Self.destroyEndBlock := Self.destroyEndBlock + 1;

      if ((Self.typ = TJCType.Train) and (track.IsTrain())) then
      begin
        Self.Log('Smazana souprava ' + track.Train.name + ' z bloku ' + track.name, ltTrainMove);
        track.RemoveTrains();
      end;
    end; // if Self.rozpadBlok >= 1
  end; // if (cyklus2 = Self.rozpadRuseniBlok)

  // tady se resi pripad, kdy stanicni kolej zustane obsazena (protoze tam stoji vagony),
  // ale souprava se z ni musi odstranit uvolnenim prvniho bloku JC
  if ((Self.destroyEndBlock = _JC_DESTROY_SIGNAL_TRACK) and (Self.destroyBlock > 0)) then
  begin
    var track: TBlkTrack := TBlkTrack(Blocks.GetBlkByID(Self.m_data.tracks[0]));
    var nextTrack: TBlkTrack;

    if (Self.m_data.tracks.Count > 1) then
      Blocks.GetBlkByID(Self.m_data.tracks[1], TBlk(nextTrack))
    else
      nextTrack := nil;

    if ((track.Zaver = TZaver.nouz) and (track.occupied = TTrackState.Free) and
      ((nextTrack = nil) or (nextTrack.occupied = TTrackState.occupied) or (nextTrack.GetSettings.RCSAddrs.Count = 0)))
    then
    begin
      // uvolneni prvniho useku v posunove ceste je signalem pro zhasnuti navestidla
      if ((signal.signal <> ncStuj) and (Self.typ = TJCType.shunt)) then
      begin
        Self.Log('Uvolnen usek ' + track.name + ' : navestidlo ' + signal.name + ' nastaveno na STUJ');
        signal.JCCancelSignal();
      end;

      if (Self.ab) then
        TBlkTrack(track).Zaver := TZaver.ab
      else
        TBlkTrack(track).Zaver := TZaver.no;

      Self.destroyEndBlock := 1;

      if ((Self.typ = TJCType.Train) and (track.IsTrain())) then
      begin
        // mazani soupravy z useku pred navestidlem
        var train: TTrain := Self.GetTrain(signal, signalTrack);
        if (train = TBlkTrack(track).Train) then
        begin
          Self.Log('Smazana souprava ' + train.name + ' z bloku ' + signalTrack.name, ltTrainMove);
          (signalTrack as TBlkTrack).RemoveTrain(train);
        end;

        Self.Log('Smazana souprava ' + train.name + ' z bloku ' + signalTrack.name, ltTrainMove);
        track.RemoveTrains();
      end;
    end;
  end;

  // mazani soupravy z useku pred navestidlem
  if ((Self.destroyBlock > 0) and (Self.destroyEndBlock = _JC_DESTROY_SIGNAL_TRACK)) then
  begin
    if ((signalTrack.occupied = TTrackState.Free) and (signalTrack.GetSettings.RCSAddrs.Count > 0)) then
    begin
      if (signalTrack.IsTrain() and (Self.typ = TJCType.Train)) then
      begin
        var train: TTrain := Self.GetTrain(signal, signalTrack);
        signalTrack.RemoveTrain(train);
        Self.Log('Smazana souprava ' + train.name + ' z bloku ' + signalTrack.name, ltTrainMove);
      end;

      Self.destroyEndBlock := 0;

      if ((signalTrack.typ = btRT) and (TBlkRT(signalTrack).railway <> nil) and (TBlkRT(signalTrack).bpInBlk)) then
        TBlkRT(signalTrack).ReleasedFromJC();
    end;
  end;

  if ((Self.destroyBlock = 0) and (Self.destroyEndBlock = _JC_DESTROY_SIGNAL_TRACK) and
    (TBlkTrack(signalTrack).occupied <> TTrackState.occupied)) then
  begin
    // usek pred navestidlem se opet uvolnil
    Self.destroyBlock := _JC_DESTROY_SIGNAL_TRACK;
    Self.destroyEndBlock := _JC_DESTROY_SIGNAL_STUJ;
  end;

  // tahleta silenost za OR je tu pro pripad, kdy JC ma jen jeden usek (to se stava napriklad na smyckach)
  if ((Self.destroyEndBlock = Self.m_data.tracks.Count - 1) and (Self.m_data.tracks.Count > 1)) or
    ((Self.m_data.tracks.Count = 1) and (Self.destroyBlock = 1)) then
  begin
    // vsechny useky az na posledni jsou uvolneny -> rusime JC

    // tady by teoreticky melo prijit ruseni zaveru posledniho bloku, ale to neni poteba,
    // protoze zaver tohoto bloku je primo navazny na zaver predposledniho bloku pres redukce
    // to je napriklad kvuli tratim, ci z toho duvodu, ze na stanicnich kolejich nejde dat NUZ

    // pozor ale na JC, ktere maji jen jeden usek a ten je stanicni koleji:
    if (Self.m_data.tracks.Count = 1) then
    begin
      var track: TBlkTrack := TBlkTrack(Blocks.GetBlkByID(Self.m_data.tracks[0]));

      if (Self.ab) then
        track.Zaver := TZaver.ab
      else
        track.Zaver := TZaver.no;

      var train: TTrain := Self.GetTrain(signal, signalTrack);

      // pokud ma cesta jen jeden usek, odstranime soupravu z useku pred navestidlem:
      if ((Self.typ = TJCType.Train) and (train <> nil)) then
      begin
        signalTrack.RemoveTrain(train);
        Self.Log('Smazana souprava ' + Train.name + ' z bloku ' + signalTrack.name, ltTrainMove);
      end;

      if ((signalTrack.typ = btRT) and (TBlkRT(signalTrack).railway <> nil) and (TBlkRT(signalTrack).bpInBlk)) then
        TBlkRT(signalTrack).ReleasedFromJC();
    end;

    if ((Self.lastTrack.typ = TBlkType.btRT) and (TBlkTrack(Self.lastTrack).IsTrain()) and (Self.typ = TJCType.Train))
    then
      TBlkTrack(Self.lastTrack).Train.UpdateRailwaySpeed();

    Self.destroyBlock := _JC_DESTROY_NONE;
    Self.destroyEndBlock := _JC_DESTROY_NONE;
    Self.Log('Ruseni: rozpad cesty vlakem');
    if (signal.DNjc = Self) then
    begin
      if (signal.signal > ncStuj) then
      // tato situace opravdu muze nastat - predstavte si posunovou cestu s jednim usekem vychazejici z nedetek koleje
        signal.JCCancelSignal();
      signal.DNjc := nil;
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TJC.NonProfileOccupied();
begin
  if (Self.activating) then
  begin
    Self.CancelActivating('Nelze postavit - obsazen neprofilový úsek');
  end else begin
    var signal: TBlkSignal := Self.signal as TBlkSignal;
    if ((signal.signal > ncStuj) and (signal.DNjc = Self)) then
    begin
      if (Self.m_state.senderPnl <> nil) and (Self.m_state.senderOR <> nil) then
        PanelServer.BottomError(Self.m_state.senderPnl, 'Chyba povolovací návěsti ' + signal.name,
          (Self.m_state.senderOR as TArea).ShortName, 'TECHNOLOGIE');
      Self.CancelWithoutTrackRelease();
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TJC.DynamicCancelingNC();
var railwayTrack, first: TBlkTrack;
begin
  railwayTrack := TBlkRT((Self.signal as TBlkSignal).track);
  first := TBlkTrack(Blocks.GetBlkByID(Self.m_data.tracks[0]));

  if ((first.occupied = TTrackState.occupied) and (railwayTrack.occupied = TTrackState.Free) and
    (not railwayTrack.IsTrain())) then
  begin
    if (TBlkRT(railwayTrack).bpInBlk) then
      TBlkRT(railwayTrack).ReleasedFromJC();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

// preda soupravu v jizdni ceste dalsimu bloku v poradi
procedure TJC.MoveTrainToNextTrack();
var trackActual, trackNext: TBlk;
  train: TTrain;
begin
  if (Self.destroyBlock = 0) then
  begin
    trackActual := (Self.signal as TBlkSignal).track;
    Train := Self.GetTrain(Self.signal, trackActual);
    if ((trackActual as TBlkTrack).IsTrain()) then
      if (Train.front <> trackActual) then
        Exit();
  end else begin
    Blocks.GetBlkByID(Self.m_data.tracks[Self.destroyBlock - 1], trackActual);
    Train := TBlkTrack(trackActual).Train;
  end;

  Blocks.GetBlkByID(Self.m_data.tracks[Self.destroyBlock], trackNext);
  if (not(trackActual as TBlkTrack).IsTrain()) then
    Exit();

  (trackNext as TBlkTrack).slowingReady := true;
  (trackNext as TBlkTrack).AddTrainL(Train);
  (trackNext as TBlkTrack).Train.front := trackNext;
  (trackNext as TBlkTrack).houkEvEnabled := true;
  Self.Log('Predana souprava ' + (trackNext as TBlkTrack).Train.name + ' z bloku ' + trackActual.name + ' do bloku ' +
    trackNext.name, ltTrainMove);

  Self.CheckLoopBlock(trackNext);
end;

procedure TJC.CheckLoopBlock(blk: TBlk);
begin
  if (((blk as TBlkTrack).GetSettings().loop) and ((blk as TBlkTrack).IsTrain())) then
  begin
    // kontrola zmeny vychozi a cilove stanice
    for var area: TArea in blk.areas do
    begin
      if (area = (blk as TBlkTrack).Train.areaTo) then
      begin
        (blk as TBlkTrack).Train.InterChangeArea(false);
        break;
      end;
    end;

    (blk as TBlkTrack).Train.ChangeDirection();
    Self.Log('Obsazen smyckovy usek ' + blk.name + ' - menim smer loko v souprave ' + (blk as TBlkTrack).Train.name,
      ltTrainMove);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

// nastavi navestidlo JC na pozadovanou navest
procedure TJC.SetSignalSignal();
var code: TBlkSignalCode;
begin
  code := ncStuj;

  if ((Self.m_state.nc) and (Self.typ = TJCType.Train)) then
  begin
    // nouzova cesta
    code := ncPrivol;
  end else begin

    case (Self.typ) of
      TJCType.shunt:
        begin
          code := TBlkSignalCode(Self.m_data.signalCode);
        end; // case shunt

      TJCType.train:
        begin
          var nextSignal: TBlkSignal;
          Blocks.GetBlkByID(Self.m_data.nextSignalId, TBlk(nextSignal));

          if ((Self.m_data.nextSignalType = TJCNextSignalType.no) or
            (Self.m_data.nextSignalType = TJCNextSignalType.railway) or
            ((Self.m_data.nextSignalType = TJCNextSignalType.signal) and ((nextSignal <> nil) and
            (nextSignal.IsGoSignal()) and (not nextSignal.IsOpakVystraha())))) then
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
        end; // case vlak

    end; // case
  end; // else emergency

  (Self.signal as TBlkSignal).SetSignal(code, TNotifyEvent(nil), Self.SignalError);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TJC.LoadData(ini: TMemIniFile; section: string);
var sl: TStrings;
begin
  Self.m_data.name := ini.ReadString(section, 'nazev', section);
  Self.m_data.id := StrToInt(section);
  Self.m_data.signalId := ini.ReadInteger(section, 'nav', -1);
  Self.m_data.typ := TJCType(ini.ReadInteger(section, 'typ', -1));
  Self.m_data.nextSignalType := TJCNextSignalType(ini.ReadInteger(section, 'dalsiNTyp', 0));
  Self.m_data.nextSignalId := ini.ReadInteger(section, 'dalsiN', 0);
  Self.m_data.railwayId := ini.ReadInteger(section, 'trat', -1);
  Self.m_data.railwayDir := TRailwayDirection(ini.ReadInteger(section, 'tratSmer', 0));

  if (Self.m_data.typ = TJCType.shunt) then
   Self.m_data.signalCode := ini.ReadInteger(section, 'navest', Integer(ncPosunZaj))
  else
   Self.m_data.signalCode := Integer(ncDisabled);

  Self.m_data.speedGo := ini.ReadInteger(section, 'rychDalsiN', 0);
  if (Self.m_data.speedGo < 10) then
    Self.m_data.speedGo := Self.m_data.speedGo*10; //  backward compatibility

  Self.m_data.speedStop := ini.ReadInteger(section, 'rychNoDalsiN', 0);
  if (Self.m_data.speedStop < 10) then
    Self.m_data.speedStop := Self.m_data.speedStop*10; //  backward compatibility

  sl := TStringList.Create();
  try
    // tracks
    ExtractStrings([';', ',', '|', '-', '('], [')'], PChar(ini.ReadString(section, 'useky', '')), sl);
    Self.m_data.tracks.Count := sl.Count;
    for var i: Integer := 0 to Self.m_data.tracks.Count - 1 do
      Self.m_data.tracks[i] := StrToInt(sl[i]);

    // turnouts
    begin
      sl.Clear();
      ExtractStrings([';', ',', '|', '-', '(', ')'], [], PChar(ini.ReadString(section, 'vyhybky', '')), sl);
      var sect_size: Integer := 2;
      var cnt: Integer := (sl.Count div sect_size);
      Self.m_data.turnouts.Clear();
      for var i: Integer := 0 to cnt - 1 do
      begin
        var turnoutZav: TJCTurnoutZav;
        turnoutZav.Block := StrToInt(sl[i * sect_size]);
        turnoutZav.position := TTurnoutPosition(StrToInt(sl[(i * sect_size) + 1]));
        Self.m_data.turnouts.Add(turnoutZav);
      end;
    end;

    // refugees
    begin
      sl.Clear();
      ExtractStrings([';', ',', '|', '-', '(', ')'], [], PChar(ini.ReadString(section, 'odvraty', '')), sl);
      var sect_size: Integer := 3;
      var cnt: Integer := (sl.Count div sect_size);
      Self.m_data.refuges.Clear();
      for var i: Integer := 0 to cnt - 1 do
      begin
        var refugeeZav: TJCRefugeeZav;
        refugeeZav.Block := StrToInt(sl[i * sect_size]);
        refugeeZav.position := TTurnoutPosition(StrToInt(sl[(i * sect_size) + 1]));
        refugeeZav.ref_blk := StrToInt(sl[(i * sect_size) + 2]);
        Self.m_data.refuges.Add(refugeeZav);
      end;
    end;

    // format dat prejezdu:
    // (...),(...),(...) jsou jednotlive prejezdy
    // konkretni popis toho, co ma byt na miste tecek:
    // (prj_blk_id,otevreni_blk,uzavreni_blk_1,uzavreni_blk_2,uzavreni_blk_3,..)

    // crossings
    begin
      var crossingStrs: TStrings := TStringList.Create();
      try
        sl.Clear();
        ExtractStrings(['(', ')'], [], PChar(ini.ReadString(section, 'prj', '')), sl);
        for var i: Integer := 0 to sl.Count - 1 do
        begin
          crossingStrs.Clear();
          ExtractStrings([';', ',', '|', '-'], [], PChar(sl[i]), crossingStrs);

          var crossingZav: TJCCrossingZav;
          crossingZav.crossingId := StrToInt(crossingStrs[0]);
          if (crossingStrs.Count > 1) then
            crossingZav.openTrack := StrToInt(crossingStrs[1])
          else
            crossingZav.openTrack := -1;

          crossingZav.closeTracks := TList<Integer>.Create();
          for var j: Integer := 2 to crossingStrs.Count - 1 do
            crossingZav.closeTracks.Add(StrToInt(crossingStrs[j]));

          Self.m_data.crossings.Add(crossingZav);
        end;
      finally
        crossingStrs.Free();
      end;
    end;

    // locks
    begin
      var lockStrs: TStrings := TStringList.Create();
      try
        sl.Clear();
        ExtractStrings(['(', ')'], [], PChar(ini.ReadString(section, 'podm-zamky', '')), sl);
        Self.m_data.locks.Clear();
        for var i: Integer := 0 to sl.Count - 1 do
        begin
          lockStrs.Clear();
          ExtractStrings([';', ',', '|', '-'], [], PChar(sl[i]), lockStrs);

          var refZav: TJCRefZav;
          refZav.Block := StrToInt(lockStrs[0]);
          refZav.ref_blk := StrToInt(lockStrs[1]);
          Self.m_data.locks.Add(refZav);
        end;
      finally
        lockStrs.Free();
      end;
    end;

    // variant points
    begin
      sl.Clear();
      ExtractStrings([';', ',', '|', '-', '(', ')'], [], PChar(ini.ReadString(section, 'vb', '')), sl);
      for var i: Integer := 0 to sl.Count - 1 do
        Self.m_data.vb.Add(StrToInt(sl[i]));
    end;

  finally
    sl.Free();
  end;

  if (ini.ValueExists(section, 'odbocka')) then
    Self.m_data.turn := ini.ReadBool(section, 'odbocka', false)
  else
    Self.m_data.turn := Self.IsAnyTurnoutMinus();

  Self.m_data.nzv := ini.ReadBool(section, 'nzv', false);
  Self.m_data.signalFallTrackI := ini.ReadInteger(section, 'rusNavestUsek', 0);
end;

procedure TJC.SaveData(ini: TMemIniFile; section: string);
begin
  ini.WriteString(section, 'nazev', Self.m_data.name);
  ini.WriteInteger(section, 'nav', Self.m_data.signalId);
  ini.WriteInteger(section, 'typ', Integer(Self.m_data.typ));
  if (Self.m_data.nextSignalType <> TJCNextSignalType.no) then
    ini.WriteInteger(section, 'dalsiNTyp', Integer(Self.m_data.nextSignalType));
  if (Self.m_data.nextSignalType = TJCNextSignalType.signal) then
    ini.WriteInteger(section, 'dalsiN', Self.m_data.nextSignalId);
  ini.WriteInteger(section, 'rychDalsiN', Self.m_data.speedGo);
  ini.WriteInteger(section, 'rychNoDalsiN', Self.m_data.speedStop);

  if ((Self.m_data.typ = TJCType.shunt) and (Self.m_data.signalCode <> Integer(ncPosunZaj))) then
    ini.WriteInteger(section, 'navest', Integer(Self.m_data.signalCode))
  else
    ini.DeleteKey(section, 'navest');

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

  // tracks
  var tracksStr: string := SerializeIntList(Self.m_data.tracks);
  if (tracksStr <> '') then
    ini.WriteString(section, 'useky', tracksStr);

  // turnouts
  var turnoutsStr := '';
  for var i: Integer := 0 to Self.m_data.turnouts.Count - 1 do
    turnoutsStr := turnoutsStr + '(' + IntToStr(Self.m_data.turnouts[i].Block) + ',' +
      IntToStr(Integer(Self.m_data.turnouts[i].position)) + ')';
  if (turnoutsStr <> '') then
    ini.WriteString(section, 'vyhybky', turnoutsStr);

  // refugees
  var refugeesStr: string := '';
  for var i: Integer := 0 to Self.m_data.refuges.Count - 1 do
    refugeesStr := refugeesStr + '(' + IntToStr(Self.m_data.refuges[i].Block) + ',' +
      IntToStr(Integer(Self.m_data.refuges[i].position)) + ',' + IntToStr(Self.m_data.refuges[i].ref_blk) + ')';
  if (refugeesStr <> '') then
    ini.WriteString(section, 'odvraty', refugeesStr);

  // crossings
  var crossingsStr: string := '';
  for var i: Integer := 0 to Self.m_data.crossings.Count - 1 do
  begin
    crossingsStr := crossingsStr + '(' + IntToStr(Self.m_data.crossings[i].crossingId);

    if (Self.m_data.crossings[i].closeTracks.Count > 0) then
    begin
      crossingsStr := crossingsStr + ',' + IntToStr(Self.m_data.crossings[i].openTrack) + ',';
      for var j: Integer := 0 to Self.m_data.crossings[i].closeTracks.Count - 1 do
        crossingsStr := crossingsStr + IntToStr(Self.m_data.crossings[i].closeTracks[j]) + ',';
    end;

    if (crossingsStr[Length(crossingsStr)] = ',') then
      crossingsStr[Length(crossingsStr)] := ')'
    else
      crossingsStr := crossingsStr + ')';
  end;
  if (crossingsStr <> '') then
    ini.WriteString(section, 'prj', crossingsStr);

  // locks
  var locksStr: string := '';
  for var i: Integer := 0 to Self.m_data.locks.Count - 1 do
    locksStr := locksStr + '(' + IntToStr(Self.m_data.locks[i].Block) + ';' + IntToStr(Self.m_data.locks[i].ref_blk) + ')';
  if (locksStr <> '') then
    ini.WriteString(section, 'podm-zamky', locksStr);

  // Variant points
  var vpsStr: string := SerializeIntList(Self.m_data.vb);
  if (vpsStr <> '') then
    ini.WriteString(section, 'vb', vpsStr);
end;

/// /////////////////////////////////////////////////////////////////////////////

// timeout staveni JC
procedure TJC.UpdateTimeOut();
begin
  // na nouzovou cestu se nevztahuje timeout
  if (not Self.activating) then
    Exit();

  if (Now > Self.m_state.timeOut) then
  begin
    case (Self.step) of
      stepConfSeq:
        begin
          if (Self.m_state.senderPnl <> nil) and (Self.m_state.senderOR <> nil) then
            PanelServer.CSClose(Self.m_state.senderPnl);
        end;
      stepJcWaitCross:
        begin
          // prejezd(y) neuzavren
          for var crossingZav: TJCCrossingZav in Self.m_data.crossings do
          begin
            var crossing := TBlkCrossing(Blocks.GetBlkByID(crossingZav.crossingId));
            if (crossing.state <> TBlkCrossingBasicState.closed) then
              if (Self.m_state.senderPnl <> nil) and (Self.m_state.senderOR <> nil) then
                PanelServer.BottomError(Self.m_state.senderPnl, 'Neuzavřen ' + crossing.name,
                  (Self.m_state.senderOR as TArea).ShortName, 'TECHNOLOGIE');
          end;
        end;

    else
      if (Self.m_state.senderPnl <> nil) and (Self.m_state.senderOR <> nil) then
        PanelServer.BottomError(Self.m_state.senderPnl, 'Timeout ' + Self.name,
          (Self.m_state.senderOR as TArea).ShortName, 'TECHNOLOGIE');
    end; // else case

    // timeout
    Self.CancelActivating('Překročení času stavění JC', true); // toto je docasne reseni: cestu vymazeme ze zasobniku
  end; // if timeout
end;

/// /////////////////////////////////////////////////////////////////////////////

function TJC.IsActivating(): Boolean;
begin
  result := ((Self.step > stepDefault) and (Self.step <> stepJcLastTrackWait));
end;

function TJC.IsActive(): Boolean;
begin
  result := (Self.m_state.destroyBlock > _JC_DESTROY_NONE);
end;

/// /////////////////////////////////////////////////////////////////////////////

// true = je mozno DN
// tato funkce kontroluje, jestli je mozne znovupostavit cestu i kdyz byla fakticky zrusena = musi zkontrolovat vsechny podminky
function TJC.CanDN(): Boolean;
var train: TTrain;
begin
  // index soupravy na useku pred navestidlem
  train := Self.GetTrain();

  // zkontrolujeme zavery bloku
  // JC NELZE obnovit z useku, na kterych uplne spadl zaver (do zadneho zaveru)
  // porusily by se reference na redukce menu
  for var i: Integer := 0 to Self.m_data.tracks.Count - 1 do
  begin
    var trackZaver: Integer := Self.m_data.tracks[i];
    var track: TBlkTrack;
    Blocks.GetBlkByID(trackZaver, TBlk(track));
    if ((track.Zaver = TZaver.no) or (track.Zaver = TZaver.staveni) or (track.NUZ) or
      ((track.occupied <> TTrackState.Free) and ((Self.typ = TJCType.Train) or (i <> Self.m_data.tracks.Count - 1))))
    then
      Exit(false);

    // na usecich v ceste je dovoleno mit soupravu pred navestidlem, v takovem
    // pripade ji DN z useku v ceste smaze

    if (Self.typ = TJCType.Train) then
    begin
      if (Train = nil) then
      begin
        // pred navestidlem neni souprava -> na usecich nesmi byt zadna souprava
        if (track.IsTrain()) then
          Exit(false);
      end else begin
        // pred navestidlem je souprava -> na usecich smi byt jen stejna souprava
        // jako pred navestidlem
        if ((track.IsTrain()) and ((track.trains.Count > 1) or (track.Train <> Train))) then
          Exit(false);
      end;
    end;
  end; // for i

  // zkontrolujeme polohu vyhybek
  for var turnoutZav: TJCTurnoutZav in Self.m_data.turnouts do
  begin
    var turnout: TBlkTurnout;
    Blocks.GetBlkByID(turnoutZav.Block, TBlk(turnout));
    if (turnout.position <> turnoutZav.position) then
      Exit(false);

    // kontrola neprofiloveho styku pro polohu +
    if ((turnoutZav.position = TTurnoutPosition.plus) and (turnout.npBlokPlus <> nil) and
      (TBlkTrack(turnout.npBlokPlus).occupied <> TTrackState.Free)) then
      Exit(false);

    // kontrola neprofiloveho styku pro polohu -
    if ((turnoutZav.position = TTurnoutPosition.minus) and (turnout.npBlokMinus <> nil) and
      (TBlkTrack(turnout.npBlokMinus).occupied <> TTrackState.Free)) then
      Exit(false);
  end;

  // zkontrolujeme polohu odvratu
  for var refugeeZav: TJCRefugeeZav in Self.m_data.refuges do
  begin
    var turnout: TBlkTurnout;
    Blocks.GetBlkByID(refugeeZav.Block, TBlk(turnout));
    if (turnout.position <> refugeeZav.position) then
      Exit(false);
  end;

  // zkontrolujeme poruchy prejezdu
  // prejezdy, na kterych je zaver, by taky mely byt uzavrene
  for var crossingZav: TJCCrossingZav in Self.m_data.crossings do
  begin
    var crossing: TBlkCrossing;
    Blocks.GetBlkByID(crossingZav.crossingId, TBlk(crossing));
    if ((crossing.state = TBlkCrossingBasicState.none) or (crossing.state = TBlkCrossingBasicState.disabled)) then
      Exit(false);
    if ((crossing.Zaver) and (crossing.state <> TBlkCrossingBasicState.closed)) then
      Exit(false);
  end; // for i

  // zkontrolujeme trat
  if (Self.m_data.railwayId > -1) then
  begin
    var railway: TBlkRailway;
    Blocks.GetBlkByID(Self.m_data.railwayId, TBlk(railway));
    if (railway.request) then
      Exit(false);
    if ((((not(TBlkRT(Self.lastTrack).sectReady)) or (railway.departureForbidden)) and (Self.typ = TJCType.Train)) or
      (railway.RBPCan) or (railway.direction <> Self.m_data.railwayDir)) then
      Exit(false);
  end;

  // kontrola uzamceni zamku:
  for var refZav: TJCRefZav in Self.m_data.locks do
  begin
    var lock: TBlkLock;
    Blocks.GetBlkByID(refZav.Block, TBlk(lock));

    // kontrola uzamceni
    if (lock.keyReleased) then
      Exit(false);
  end;

  result := true;
end;

// DN provede zbytek staveni JC (prejezdy, finalizace)
// tato procedura predpoklada, ze podminky pro DN jsou splneny
procedure TJC.DN();
begin
  Self.Log('DN');
  Self.m_state.timeOut := Now + EncodeTime(0, _JC_TIMEOUT_SEC div 60, _JC_TIMEOUT_SEC mod 60, 0);

  if (Self.m_state.crossingWasClosed) then
    Self.step := stepJcFinalZaver
  else
    Self.step := stepJcCloseCross;
end;

/// /////////////////////////////////////////////////////////////////////////////

// volano z navestidla pri STUJ
// nevolat nidky jindy !
procedure TJC.STUJ();
begin
  Self.destroyBlock := _JC_DESTROY_SIGNAL_STUJ;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TJC.TrackCloseCrossing(Sender: TObject; data: Integer);
begin
  if ((Self.active) or (Self.activating)) then
  begin
    // zavrit prejezd
    var crossing: TBlkCrossing;
    Blocks.GetBlkByID(Self.m_data.crossings[data].crossingId, TBlk(crossing));
    crossing.Zaver := true;
    Self.Log('Obsazen ' + TBlkTrack(Sender).name + ' - uzaviram prejezd ' + crossing.name);

    // prejezd se uzavira -> po uvolneni zaveru bloku pod prejezdem prejezd opet otevrit
    var track: TBlkTrack;
    Blocks.GetBlkByID(Self.m_data.crossings[data].openTrack, TBlk(track));
    track.AddChangeEvent(track.eventsOnZaverReleaseOrAB, CreateChangeEvent(ceCaller.NullPrejezdZaver,
      Self.m_data.crossings[data].crossingId));
  end;

  for var blkId: Integer in Self.m_data.crossings[data].closeTracks do
  begin
    var track: TBlkTrack;
    Blocks.GetBlkByID(blkId, TBlk(track));
    track.RemoveChangeEvent(track.eventsOnOccupy, CreateChangeEvent(Self.TrackCloseCrossing, data));
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

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

procedure TJC.SetStep(step: JCStep);
begin
  Self.m_state.step := step;
  Self.changed := true;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TJC.CritBarieraEsc(Sender: TObject);
begin
  Self.CancelActivating('', true);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TJC.TurnoutMovedJCPC(Sender: TObject);
begin
  { Pozor: muze se stat, ze nektera z vyhybek, ktere jeste nejsou prestavovany,
    je behem staveni JC prestavena externim zdrojem. Je treba na to pamatovat.

    Pozor: i ty vyhybky, ktere pri staveni nebyly explicitne zamknuty, se samy
    zamknou pri udeleni zaveru na usek. Nelze tedy vyhybky rozlisovat podle
    zamknuti.

    Pozor: tato funkce muze volat sama sebe rekurzivne skrze callback.
  }

  if (Self.m_state.nextTurnout < 0) then
    Exit();

  if (Self.m_state.nextTurnout < Self.m_data.turnouts.Count) then
  begin
    // stavim dalsi vyhybku
    for var i: Integer := Self.m_state.nextTurnout to Self.m_data.turnouts.Count - 1 do
    begin
      var blk: TBlk;
      Blocks.GetBlkByID(Self.m_data.turnouts[i].Block, blk);
      if (TBlkTurnout(blk).position <> TTurnoutPosition(Self.m_data.turnouts[i].position)) then
      begin
        Self.m_state.nextTurnout := i + 1;
        TBlkTurnout(blk).SetPosition(TTurnoutPosition(Self.m_data.turnouts[i].position), true, false,
          Self.TurnoutMovedJCPC, Self.TurnoutErrJCPC);
        Exit();
      end;
    end;

    // sem se skoci, pokud vsechny zbyvajici vyhybky byly ve spravne poloze
    Self.m_state.nextTurnout := Self.m_data.turnouts.Count;
  end;

  if (Self.m_state.nextTurnout < Self.m_data.turnouts.Count + Self.m_data.refuges.Count) then
  begin
    // stavim dalsi odvrat
    var refugeeId: Integer := Self.m_state.nextTurnout - Self.m_data.turnouts.Count;
    for var i: Integer := refugeeId to Self.m_data.refuges.Count - 1 do
    begin
      // nastaveni odvratu
      var refugee: TBlkTurnout;
      Blocks.GetBlkByID(Self.m_data.refuges[i].Block, TBlk(refugee));
      if (refugee.position <> TTurnoutPosition(Self.m_data.refuges[i].position)) then
      begin
        refugee.IntentionalLock();

        var track: TBlkTrack;
        Blocks.GetBlkByID(Self.m_data.refuges[i].ref_blk, TBlk(track));
        track.AddChangeEvent(track.eventsOnZaverReleaseOrAB, CreateChangeEvent(ceCaller.NullVyhybkaMenuReduction,
          Self.m_data.refuges[i].Block));

        Self.m_state.nextTurnout := i + Self.m_data.turnouts.Count + 1;
        refugee.SetPosition(TTurnoutPosition(Self.m_data.refuges[i].position), true, false,
          Self.TurnoutMovedJCPC, Self.TurnoutErrJCPC);
        Exit();
      end;
    end;

    // sem se skoci, pokud vsechny zbyvajici odvraty byly ve spravne poloze
    Self.m_state.nextTurnout := -1;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TJC.TurnoutErrJCPC(Sender: TObject; error: TTurnoutSetError);
begin
  if (not Self.activating) then
    Exit();

  if (Self.m_state.senderPnl <> nil) and (Self.m_state.senderOR <> nil) then
    PanelServer.BottomError(Self.m_state.senderPnl, 'Nepřestavena ' + (Sender as TBlkTurnout).name + ': ' +
      TBlkTurnout.SetErrorToMsg(error), (Self.m_state.senderOR as TArea).ShortName, 'TECHNOLOGIE');
  Self.CancelActivating('', true);
  Self.Cancel();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TJC.TurnoutErrNC(Sender: TObject; error: TTurnoutSetError);
begin
  Self.TurnoutMovedNC(Sender);
end;

procedure TJC.TurnoutMovedNC(Sender: TObject);
begin
  if ((Self.m_state.step <> stepNcInit) and (Self.m_state.step <> stepNcBarrierUpdate)) then
    Exit();

  TBlkTurnout(Sender).emLock := true;

  var signal := TBlkSignal(Blocks.GetBlkByID(Self.m_data.signalId));
  signal.AddBlkToRnz(TBlk(Sender).id, false);

  if (TBlkTurnout(Sender).GetSettings().coupling > -1) then
  begin
    var coupling := TBlkTurnout(Blocks.GetBlkByID(TBlkTurnout(Sender).GetSettings().coupling));
    coupling.emLock := true;
    signal.AddBlkToRnz(TBlkTurnout(Sender).GetSettings().coupling, false);
  end;

  // staveni dalsich vyhybek

  if (Self.m_state.nextTurnout < 0) then
    Exit();

  if (Self.m_state.nextTurnout < Self.m_data.turnouts.Count) then
  begin
    // stavim dalsi vyhybku
    // Tady staci postavit jen jednu vyhybku, protoze jeji uzamceni opet zavola
    // tuto udalost.

    var nextTurnout := TBlkTurnout(Blocks.GetBlkByID(Self.m_data.turnouts[Self.m_state.nextTurnout].Block));
    Inc(Self.m_state.nextTurnout);

    nextTurnout.SetPosition(TTurnoutPosition(Self.m_data.turnouts[Self.m_state.nextTurnout - 1].position), true,
      false, Self.TurnoutMovedNC, Self.TurnoutErrNC); // may call callback directly!
  end else if ((Self.m_state.nextTurnout >= Self.m_data.turnouts.Count) and
    (Self.m_state.nextTurnout < Self.m_data.turnouts.Count + Self.m_data.refuges.Count)) then
  begin
    // nastaveni odvratu
    // Tady staci postavit jen jednu vyhybku, protoze jeji uzamceni opet zavola
    // tuto udalost.

    var refugeeId := Self.m_state.nextTurnout - Self.m_data.turnouts.Count;
    var refugee := TBlkTurnout(Blocks.GetBlkByID(Self.m_data.refuges[refugeeId].Block));
    Inc(Self.m_state.nextTurnout);

    refugee.SetPosition(TTurnoutPosition(Self.m_data.refuges[refugeeId].position), true, false,
      Self.TurnoutMovedNC, Self.TurnoutErrNC); // may call callback directly!
  end else if (Self.m_state.nextTurnout = Self.m_data.turnouts.Count + Self.m_data.refuges.Count) then
    Self.m_state.nextTurnout := -1;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TJC.SignalError(Sender: TObject);
begin
  if (not Self.activating) then
    Exit();
  var signal := TBlkSignal(Blocks.GetBlkByID(Self.m_data.signalId));

  if (Self.m_state.senderPnl <> nil) and (Self.m_state.senderOR <> nil) then
    PanelServer.BottomError(Self.m_state.senderPnl, 'Návěstidlo ' + signal.name + ' nepostaveno',
      (Self.m_state.senderOR as TArea).ShortName, 'TECHNOLOGIE');
  Self.CancelActivating('', true);
end;

/// /////////////////////////////////////////////////////////////////////////////
// generuje podminky branici postaveni nouzove posunove ceste
// tyto podminky jsou prubezne zobrazovany dispecerovi v potvrzovaci sekvenci

procedure TJC.BarriersNCToAccept(var bariery: TList<TJCBarrier>);
begin
  // signal
  var signal := Blocks.GetBlkByID(Self.m_data.signalId);
  if (not(signal as TBlkSignal).enabled) then
    bariery.Add(JCBarrier(barBlockDisabled, signal));

  // tracks
  for var i := 0 to Self.m_data.tracks.Count - 1 do
  begin
    var trackID := Self.m_data.tracks[i];
    var track := TBlkTrack(Blocks.GetBlkByID(trackID));
    var glob := track.GetGlobalSettings();

    if (track.occupied = TTrackState.disabled) then
      bariery.Add(JCBarrier(barBlockDisabled, track))

    else if ((i <> Self.m_data.tracks.Count - 1) or (Self.typ <> TJCType.shunt)) then
    begin
      if (track.occupied <> TTrackState.Free) then
        bariery.Add(JCBarrier(barTrackOccupied, track));
    end;

    if ((track.IsTrain()) and (Self.typ = TJCType.Train)) then
      bariery.Add(JCBarrier(barTrackTrain, track));
  end;

  // turnouts
  for var turnoutZav in Self.m_data.turnouts do
  begin
    var turnout: TBlkTurnout := TBlkTurnout(Blocks.GetBlkByID(turnoutZav.Block));
    var glob := turnout.GetGlobalSettings();

    if (turnout.position <> turnoutZav.position) then
      bariery.Add(JCBarrier(barTurnoutNoPos, turnout));

    if (not turnout.emLock) then
      bariery.Add(JCBarrier(barTurnoutEmLock, turnout));

    var coupling := TBlkTurnout(Blocks.GetBlkByID(turnout.GetSettings.coupling));
    if ((coupling <> nil) and (turnout.position <> turnoutZav.position)) then
    begin
      if (not coupling.emLock) then
        bariery.Add(JCBarrier(barTurnoutEmLock, coupling));

      if (coupling.occupied = TTrackState.occupied) then
        bariery.Add(JCBarrier(barTrackOccupied, coupling));
    end;

    if ((turnoutZav.position = TTurnoutPosition.plus) and (turnout.npBlokPlus <> nil)) then
    begin
      if (TBlkTrack(turnout.npBlokPlus).occupied = TTrackState.disabled) then
        bariery.Add(JCBarrier(barBlockDisabled, turnout.npBlokPlus))
      else if (TBlkTrack(turnout.npBlokPlus).occupied <> TTrackState.Free) then
        bariery.Add(JCBarrier(barTrackOccupied, turnout.npBlokPlus));
    end;

    if ((turnoutZav.position = TTurnoutPosition.minus) and (turnout.npBlokMinus <> nil)) then
    begin
      if (TBlkTrack(turnout.npBlokMinus).occupied = TTrackState.disabled) then
        bariery.Add(JCBarrier(barBlockDisabled, turnout.npBlokMinus))
      else if (TBlkTrack(turnout.npBlokMinus).occupied <> TTrackState.Free) then
        bariery.Add(JCBarrier(barTrackOccupied, turnout.npBlokMinus));
    end;
  end;

  // crossings
  for var crossingZav in Self.m_data.crossings do
  begin
    var crossing := TBlkCrossing(Blocks.GetBlkByID(crossingZav.crossingId));

    if (crossing.state <> TBlkCrossingBasicState.none) then
    begin
      if (crossing.pcEmOpen) then
      begin
        bariery.Add(JCBarrier(barCrosEmOpen, crossing));
      end else begin
        if (crossing.state <> TBlkCrossingBasicState.closed) then
          bariery.Add(JCBarrier(barCrosNotClosed, crossing));
      end;
    end
    else
      bariery.Add(JCBarrier(barCrosError, crossing));
  end;

  // refugees
  for var refugeeZav in Self.m_data.refuges do
  begin
    var refugee := TBlkTurnout(Blocks.GetBlkByID(refugeeZav.Block));

    if (refugee.position <> refugeeZav.position) then
      bariery.Add(JCBarrier(barTurnoutNoPos, refugee));

    if (not refugee.emLock) then
      bariery.Add(JCBarrier(barTurnoutEmLock, refugee));

    var coupling := TBlkTurnout(Blocks.GetBlkByID(refugee.GetSettings.coupling));
    if (coupling <> nil) then
    begin
      if (refugee.position <> refugeeZav.position) then
        if (not coupling.emLock) then
          bariery.Add(JCBarrier(barTurnoutEmLock, coupling));
    end;
  end;

  if (Self.m_data.railwayId > -1) then
  begin
    if (Self.typ = TJCType.Train) then
    begin
      var lastTrack := TBlkRT(Blocks.GetBlkByID(Self.m_data.tracks[Self.m_data.tracks.Count - 1]));
      if (not lastTrack.sectReady) then
      begin
        var railway := TBlkRailway(Blocks.GetBlkByID(Self.m_data.railwayId));
        bariery.Add(JCBarrier(barRailwayNotReady, railway));
      end;
    end;

    begin
      var railway := TBlkRailway(Blocks.GetBlkByID(Self.m_data.railwayId));

      if ((railway.departureForbidden) and (Self.typ = TJCType.Train)) then
        bariery.Add(JCBarrier(barRailwayZAKVC, railway));
      if ((not railway.departureForbidden) and (Self.typ = TJCType.shunt)) then
        bariery.Add(JCBarrier(barRailwayNoZAK, railway));
      if (railway.Zaver) then
        bariery.Add(JCBarrier(barRailwayZaver, railway));
      if (railway.request) then
        bariery.Add(JCBarrier(barRailwayRequesting, railway));
      if (Self.m_data.railwayDir <> railway.direction) then
        bariery.Add(JCBarrier(barRailwayWrongDir, railway));
      if ((not railway.BP) and (Self.typ = TJCType.Train)) then
        bariery.Add(JCBarrier(barRailwayNoBp, railway));

      var track := TBlkTrack((Self.signal as TBlkSignal).track);
      var lastTrack := TBlkTrack(Self.lastTrack);

      if ((track.IsTrain) and (lastTrack.typ = btRT) and ((lastTrack as TBlkRT).inRailway = Self.data.railwayId)) then
      begin
        if (railway.lockout) then
        begin
          if ((railway.state.trains.Count > 0) or ((railway.GetLastTrack(Self.data.railwayDir) as TBlkRT).Zaver <>
            TZaver.no)) then
            bariery.Add(JCBarrier(barRailwayNoTrainMove, railway))
          else
            bariery.Add(JCBarrier(barRailwayMoveEnd, railway));
        end else begin
          if ((lastTrack.IsTrain()) or (not railway.BP) or (railway.direction <> Self.data.railwayDir)) then
            bariery.Add(JCBarrier(barRailwayNoTrainMove, railway));
        end;
      end;
    end;
  end;

  // locks
  for var refZav in Self.m_data.locks do
  begin
    var lock := TBlkLock(Blocks.GetBlkByID(refZav.Block));

    if (lock.keyReleased) then
      bariery.Add(JCBarrier(barLockNotLocked, lock));

    if (lock.emLock) then
      bariery.Add(JCBarrier(barLockEmLock, lock));
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TJC.NC_PS_Callback(Sender: TIdContext; success: Boolean);
var trackID: Integer;
  blk: TBlk;
begin
  if (success) then
  begin
    if (Self.step = stepNcBarrierUpdate) then
      Self.step := stepNcBarrierConfirmed;
  end else begin
    Self.CancelActivating();

    // aktualizace stavu navestidla (zobrazeni RNZ)
    Blocks.GetBlkByID(Self.m_data.signalId, blk);
    blk.Change();

    for trackID in Self.m_data.tracks do
    begin
      Blocks.GetBlkByID(trackID, blk);
      (blk as TBlkTrack).Zaver := TZaver.no;
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TJC.SetData(prop: TJCdata);
var id_changed: Boolean;
  signal_changed: Boolean;
  orig_signal: TBlk;
begin
  id_changed := ((Self.id <> prop.id) and (Self.id <> -1));
  signal_changed := (Self.data.signalId <> prop.signalId);
  Blocks.GetBlkByID(Self.data.signalId, orig_signal);
  Self.m_data := prop;
  if (id_changed) then
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

/// /////////////////////////////////////////////////////////////////////////////

function TJC.GetTrain(signal: TBlk = nil; track: TBlk = nil): TTrain;
begin
  if (signal = nil) then
    Blocks.GetBlkByID(Self.m_data.signalId, signal);

  result := TBlkSignal(signal).GetTrain(track);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TJC.GetAB(): Boolean;
var blk: TBlk;
begin
  Blocks.GetBlkByID(Self.m_data.signalId, blk);
  result := ((blk <> nil) and (blk.typ = btSignal) and (TBlkSignal(blk).ABJC = Self));
end;

/// /////////////////////////////////////////////////////////////////////////////

function TJC.IsAnyTurnoutMinus(): Boolean;
var turnout: TJCTurnoutZav;
begin
  for turnout in Self.m_data.turnouts do
    if (turnout.position = TTurnoutPosition.minus) then
      Exit(true);
  result := false;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TJC.IsCriticalBarrier(): Boolean;
var barriers: TJCBarriers;
  barrier: TJCBarrier;
  signal: TBlk;
begin
  result := false;
  Blocks.GetBlkByID(Self.m_data.signalId, signal);
  barriers := TJCBarriers.Create();
  try
    Self.BarriersVCPC(barriers);
    for barrier in barriers do
    begin
      case (barrier.typ) of
        barBlockDisabled, barBlockNotExists, barBlockWrongType, barSignalNoTrack, barTrackOccupied,
          barTrackTrain, barTrackAB, barTurnoutNoPos, barTurnoutWrongPos,
          barCrosEmOpen, barCrosError, barRefugeeNoPosition, barRailwayNotReady,
          barRailwayRequesting, barRailwayWrongDir, barRailwayNoBp, barLockNotLocked,
          barTrackPSt, barTurnoutPst, barRefugeePst, barRailwayZAKVC:
          Exit(true);
      end;
    end;
  finally
    barriers.Free();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TJC.GetSignal(): TBlk;
begin
  Blocks.GetBlkByID(Self.m_data.signalId, result);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TJC.ClientDisconnect(AContext: TIdContext);
begin
  if (Self.m_state.senderPnl = AContext) then
    Self.m_state.senderPnl := nil;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TJC.SetInitStep();
begin
  if (Self.m_state.nc) then
  begin
    Self.step := stepNcInit;
    Self.m_state.timeOut := Now + EncodeTime(0, _NC_TIMEOUT_MIN, 0, 0);
  end else begin
    Self.step := stepJcInit;
    Self.m_state.timeOut := Now + EncodeTime(0, _JC_TIMEOUT_SEC div 60, _JC_TIMEOUT_SEC mod 60, 0);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TJC.GetWaitFroLastTrackOrRailwayOccupied(): Boolean;
begin
  result := (Self.step = stepJcLastTrackWait);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TJC.GetPtData(json: TJsonObject; includeStaveni: Boolean);
begin
  json['name'] := Self.m_data.name;
  json['id'] := Self.m_data.id;
  json['signalId'] := Self.m_data.signalId;
  case (Self.typ) of
    TJCType.Train:
      json['type'] := 'VC';
    TJCType.shunt:
      json['type'] := 'PC';
  end;
  case (Self.m_data.nextSignalType) of
    TJCNextSignalType.no:
      json['nextSignal'] := '-';
    TJCNextSignalType.railway:
      json['nextSignal'] := 'trat';
    TJCNextSignalType.signal:
      begin
        json['nextSignal'] := 'block';
        json['nextSignalId'] := Self.m_data.nextSignalId;
      end;
  end;

  for var turnoutZav in Self.m_data.turnouts do
  begin
    var newObj := json.A['turnouts'].AddObject();
    newObj['block'] := turnoutZav.Block;
    case (turnoutZav.position) of
      TTurnoutPosition.plus:
        newObj['position'] := '+';
      TTurnoutPosition.minus:
        newObj['position'] := '-';
    end;
  end;

  for var trackId in Self.m_data.tracks do
    json.A['tracks'].Add(trackId);

  for var refugeeZav in Self.m_data.refuges do
  begin
    var newObj := json.A['refuges'].AddObject();
    newObj['block'] := refugeeZav.Block;
    case (refugeeZav.position) of
      TTurnoutPosition.plus:
        newObj['position'] := '+';
      TTurnoutPosition.minus:
        newObj['position'] := '-';
    end;
    newObj['refBlock'] := refugeeZav.ref_blk;
  end;

  for var crossingZav in Self.m_data.crossings do
  begin
    var newObj := json.A['crossings'].AddObject();
    newObj['crossing'] := crossingZav.crossingId;
    newObj['open'] := crossingZav.openTrack;
    for var trackId in crossingZav.closeTracks do
      newObj.A['close'].Add(trackId);
  end;

  for var refZav in Self.m_data.locks do
  begin
    var newObj := json.A['locks'].AddObject();
    newObj['lock'] := refZav.Block;
    newObj['refTrack'] := refZav.ref_blk;
  end;

  for var trackId in Self.m_data.vb do
    json.A['vb'].Add(trackId);

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
  json['ab'] := Self.ab;
end;

procedure TJC.PostPtActivate(reqJson: TJsonObject; respJson: TJsonObject);
begin
  if ((Self.signal = nil) or (TBlkSignal(Self.signal).areas.Count = 0)) then
  begin
    PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '400', 'Návěstidlo není v OŘ');
    Exit();
  end;

  var ab := (reqJson.Contains('ab') and reqJson.B['ab']);

  var barriers := TJCBarriers.Create();
  try
    var ok := Self.Activate(nil, TBlkSignal(Self.signal).areas[0], barriers, nil, false, false, ab);
    respJson['success'] := (ok = 0);
    for var barrier in barriers do
      JCBarriers.BarrierToJson(barrier, respJson.A['barriers'].AddObject());
  finally
    barriers.Free();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TJC.GetLastTrack(): TBlk;
begin
  if (Self.data.tracks.Count = 0) then
    Exit(nil);
  Blocks.GetBlkByID(Self.data.tracks[Self.data.tracks.Count - 1], result);
  if (result.typ <> btTrack) and (result.typ <> btRT) then
    result := nil;
end;

function TJC.PSts(): TList<TBlk>;
begin
  Result := TList<TBlk>.Create();
  try
    for var blk: TBlk in Blocks do
    begin
      if (blk.typ = btPst) then
      begin
        for var trackId in Self.m_data.tracks do
        begin
          if (TBlkPst(blk).GetSettings().tracks.Contains(trackId)) then
          begin
            Result.Add(blk);
            Break;
          end;
        end;
      end;
    end;
  except
    Result.Free();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
