unit TechnologieJC;

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
  Dialogs, Menus, Buttons, ComCtrls, fMain, Block, Train, BlockTrack,
  IniFiles, IdContext, BlockRailway, Generics.Collections, UPO, BlockTurnout,
  Area, changeEvent, changeEventCaller, JsonDataObjects, PTUtils, JCBarriers,
  TrainSpeed, Math;

const
  _JC_INITPOTVR_TIMEOUT_SEC = 60; // timeout UPO a potvrzeni na zacatku staveni JC
  _JC_TIMEOUT_SEC = 30; // timeout pro staveni jizdni cesty (vlakove i posunove v sekundach)
  _JC_PRJ_TIMEOUT_SEC = 50; // timeout pri staveni JC pro zavirani prejezdu v ceste
  _NC_TIMEOUT_MIN = 1; // timeout pro staveni nouzove cesty (vlakove i posunove) v minutach

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


  // zaver vyhybky v JC
  TJCTurnoutZav = record
    block: Integer;
    position: TTurnoutPosition;
  end;

  // zaver odvratove vyhybky v JC
  TJCRefugeeZav = record
    block: Integer;
    position: TTurnoutPosition;
    ref_blk: Integer; // id bloku, pri jehoz zruseni zaveru dojde i k uvolneni zaveru odvratove vyhybky
  end;

  // bloky v JC, ketre jsou navazany na konkretni useky v ramci JC (napr. zamky)
  TJCRefZav = record
    block: Integer;
    ref_blk: Integer; // id bloku, pri jehoz uvolneni zaveru dojde ke zruseni
  end;

  // prejezd v JC
  TJCCrossingZav = record
    crossingId: Integer;
    closeTracks: TList<Integer>; // pokud se prejezd nezavira, je seznam prazdny
    openTrack: Integer; // pokud se prejezd nezavira, je -1
  end;

  /// ////////////////////////////////////////////////////////////////////////

  TJCdata = record
    name: string;
    id: Integer;
    typ: TJCType;
    emOnly: Boolean; // emergency-only path

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
    permNotes: TList<string>;

    railwayId: Integer;
    railwayDir: TRailwayDirection;
    speedsGo, speedsStop: TList<TTrainSpeed>;
    turn: Boolean; // jc od odbocky (navesteni 40 km/h)
    nzv: Boolean; // nedostatecna zabrzdna vzdalenost
    signalFallTrackI: Cardinal;
    loopTrackI: Integer; // index of loop track; -1 if no loop
  end;

  // staveni jizdni cesty:
  // staveni jizdni cesty probiha krokove, viz \UpdateStaveni
  TJCstate = record
    step: JCStep; // aktualni krok staveni jizdni cesty
    timeOut: TDateTime; // cas, pri jehoz prekroceni dojde k timeoutu JC
    // oblast rizeni, ktera vyvolala staveni JC, do teto OR jsou typicky odesilany notifikacni a chybove hlasky (napr. upozorneni vlevo dole panelu, potvrzovaci sekvence)
    senderOR: TObject;
    senderPnl: TIdContext; // konkretni panel, kery vyvolal staveni JC
    destroyBlock, // index useku, na ktery ma vkrocit vlak
    destroyEndBlock: Integer; // index useku, ze ktereho ma vystoupit vlak
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
    crossingsToClose: TList<Boolean>; // seznam prejezdu k zavreni pri aktivaci
    RCtimer: Integer; // id timeru, ktery se prave ted pouziva pro ruseni JC; -1 pokud se JC nerusi, jinak se prave ted rusi
    RCtimerArea: TArea; // oblast rizeni, ze ktere bylo spusteno mereni casu ruseni JC
    RClongTime: Boolean; // jestli se bude cesta rusit dlouhym nebo kratkym casem
    occupyStateWhenCancellingStarted: TList<TTrackState>;
  end;

  ENavChanged = procedure(Sender: TObject; origNav: TBlk) of object;

  /// ////////////////////////////////////////////////////////////////////////

  TJC = class
  private const
    _def_jc_state: TJCstate = (step: stepDefault; destroyBlock: _JC_DESTROY_NONE; destroyEndBlock: _JC_DESTROY_NONE;
      ab: false; crossingWasClosed: false; crossingsToClose: nil; RCtimer: -1; RCtimerArea: nil; occupyStateWhenCancellingStarted: nil);

  private
    m_data: TJCdata;
    m_state: TJCstate;
    fOnIdChanged: TNotifyEvent;
    fOnSignalChanged: ENavChanged;

    procedure SetInitStep();
    procedure SetData(prop: TJCdata);

    procedure CancelVBs();
    procedure MoveTrainToNextTrack();
    // kontroluje zmenu smeru vlaku a vozidel pri vkroceni do smyckove bloku,
    // tato kontrola probiha pouze pri vkroceni do posledniho bloku JC
    procedure CheckLoopBlock(blk: TBlk);
    function IsActivating(): Boolean;
    function IsActive(): Boolean;
    function IsNCActive(): Boolean;
    function IsCancelling(): Boolean;

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

    procedure BarriersVCPC(var barriers: TJCBarriers);
    procedure BarriersNC(var barriers: TJCBarriers);
    procedure BarriersNCToAccept(var bariery: TJCBarriers);

    function GetTrain(signal: TBlk = nil; track: TBlk = nil): TTrain; // vraci vlak na useku pred navestidlem

    function GetAB(): Boolean;
    function IsCriticalBarrier(): Boolean;
    function GetSignal(): TBlk;
    function GetWaitFroLastTrackOrRailwayOccupied(): Boolean;
    function GetLastTrack(): TBlkTrack;
    function PSts(): TList<TBlk>;

    procedure Log(msg: string; level: TLogLevel = llInfo; source: TLogSource = lsJC);
    procedure LogStep(msg: string; level: TLogLevel = llInfo; source: TLogSource = lsJC);

    procedure DetermineCrossingsToClose(var toClose: TList<Boolean>);
    procedure TrackCancelZaver(track: TBlkTrack);

    function CancelTimeSec(): Cardinal;
    procedure EmergencyStopTrainInPath();
    procedure EmergencyStopFrontTrainsInTrack(track: TBlkTrack);

  public

    index: Integer; // index v tabulce jizdni cest ve F_Main
    changed: Boolean; // JC zmenana -> akualizuje se v tabulce ve F_Main

    constructor Create(); overload;
    constructor Create(data: TJCdata); overload;
    destructor Destroy(); override;
    procedure Update();

    procedure SetSignalSignal();
    procedure Cancel(Sender: TObject = nil);
    procedure CancelWithoutTrackRelease(showError: Boolean = False);
    procedure EmergencyCancelActivePath();
    procedure CancelOrStop();
    procedure DynamicCancelling(); // kontroluje projizdeni vlaku useky a rusi jejich zavery
    procedure DynamicCancellingNC(); // rusi poruchu BP trati, ze ktere odjizdi vlak v ramci nouzove jizdni cesty
    procedure NonProfileOccupied(); // volano pri obsazeni kontrolvoaneho neprofiloveho useku

    procedure StartCancelling(senderArea: TArea);
    procedure StopCancelling();
    procedure CheckCancellingTracks();

    procedure UpdateActivating();
    procedure UpdateTimeOut();
    // zrusi staveni a oduvodneni zaloguje a zobrazi dispecerovi
    procedure CancelActivating(reason: string = ''; stackToPV: Boolean = True);
    procedure CancelSignalBegin();
    procedure CancelTrackEnd();

    procedure LoadData(ini: TMemIniFile; section: string);
    procedure SaveData(ini: TMemIniFile; section: string);

    procedure Activate(senderPnl: TIdContext; senderOR: TObject; bariery_out: TJCBarriers; from_stack: TObject = nil;
      nc: Boolean = false; fromAB: Boolean = false; abAfter: Boolean = false; ignoreWarn: Boolean = False); overload;
    procedure Activate(senderPnl: TIdContext; senderOR: TObject; from_stack: TObject = nil; nc: Boolean = false;
      fromAB: Boolean = false; abAfter: Boolean = false; ignoreWarn: Boolean = False); overload;

    function CanDN(): Boolean;
    // true = je mozno DN; tato funkce kontroluje, jestli je mozne znovupostavit cestu i kdyz byla fakticky zrusena = musi zkontrolovat vsechny podminky
    procedure DN(senderPnl: TIdContext; senderOR: TObject); // DN nastavi zavery vsech bloku na validni a rozsviti navestidlo
    procedure STUJ();

    function Barriers(nc: Boolean = false): TJCBarriers;
    function IsAnyTurnoutMinus(): Boolean;
    procedure ClientDisconnect(AContext: TIdContext);

    procedure GetPtData(json: TJsonObject; includeStaveni: Boolean);
    procedure GetPtState(json: TJsonObject);
    procedure PostPtActivate(reqJson: TJsonObject; respJson: TJsonObject);

    function ContainsTrackInclRailway(blockid: Integer): Boolean;
    function ContainsLock(blockid: Integer): Boolean;
    function ContainsTurnout(blockid: Integer): Boolean;
    function ContainsRailway(blockid: Integer): Boolean;
    function ContainsCrossing(blockid: Integer): Boolean;

    procedure EmergencyStopTrainInVC();

    property data: TJCdata read m_data write SetData;
    property state: TJCstate read m_state;

    property name: String read m_data.name;
    property id: Integer read m_data.id write m_data.id;
    property typ: TJCType read m_data.typ;
    property emOnly: Boolean read m_data.emOnly;

    property activating: Boolean read IsActivating;
    property cancelling: Boolean read IsCancelling;
    property active: Boolean read IsActive; // true pokud je postavena navest
    property ncActive: Boolean read IsNCActive;
    property ab: Boolean read GetAB;
    property waitForLastTrackOrRailwayOccupy: Boolean read GetWaitFroLastTrackOrRailwayOccupied;
    property lastTrack: TBlkTrack read GetLastTrack;

    property destroyBlock: Integer read m_state.destroyBlock write SetDestroyBlock;
    property destroyEndBlock: Integer read m_state.destroyEndBlock write SetDestroyEndBlock;
    property step: JCStep read m_state.step write SetStep;
    property signal: TBlk read GetSignal;

    property OnIdChanged: TNotifyEvent read fOnIdChanged write fOnIdChanged;
    property OnSignalChanged: ENavChanged read fOnSignalChanged write fOnSignalChanged;
  end;

  TJCContains = function(jc:TJC; blockid: Integer): Boolean;

  function NewJCData(): TJCdata;
  procedure FreeJCData(jcdata: TJCData);

  function ContainsTrackInclRailway(jc: TJC; blockid: Integer): Boolean;
  function ContainsLock(jc: TJC; blockid: Integer): Boolean;
  function ContainsTurnout(jc: TJC; blockid: Integer): Boolean;
  function ContainsRailway(jc: TJC; blockid: Integer): Boolean;
  function ContainsCrossing(jc: TJC; blockid: Integer): Boolean;

implementation

uses GetSystems, RCSc, TRailVehicle, BlockSignal, AreaDb, PanelConnData,
  BlockCrossing, TJCDatabase, TCPServerPanel, TrainDb, timeHelper, ownConvert,
  TRVDatabase, AreaStack, BlockLinker, BlockLock, BlockRailwayTrack, BlockDisconnector,
  BlockPSt, appEv, ConfSeq, BlockDb, Config, colorHelper;

/// /////////////////////////////////////////////////////////////////////////////

constructor TJC.Create();
begin
  inherited;

  Self.m_data.id := -1;
  Self.changed := true;
  Self.m_state := _def_jc_state;
  Self.m_state.ncBariery := TJCBarriers.Create();
  Self.m_state.crossingsToClose := TList<Boolean>.Create();
  Self.m_state.occupyStateWhenCancellingStarted := TList<TTrackState>.Create();

  Self.m_data := NewJCData();
end;

constructor TJC.Create(data: TJCdata);
begin
  inherited Create();

  Self.m_data := data;
  Self.m_state := _def_jc_state;
  if (not Assigned(Self.m_state.ncBariery)) then
    Self.m_state.ncBariery := TJCBarriers.Create();
  if (not Assigned(Self.m_state.crossingsToClose)) then
    Self.m_state.crossingsToClose := TList<Boolean>.Create();
  if (not Assigned(Self.m_state.occupyStateWhenCancellingStarted)) then
    Self.m_state.occupyStateWhenCancellingStarted := TList<TTrackState>.Create();
end;

destructor TJC.Destroy();
begin
  if (Assigned(Self.m_state.ncBariery)) then
    FreeAndNil(Self.m_state.ncBariery);
  if (Assigned(Self.m_state.crossingsToClose)) then
    FreeAndNil(Self.m_state.crossingsToClose);
  if (Assigned(Self.m_state.occupyStateWhenCancellingStarted)) then
    FreeAndNil(Self.m_state.occupyStateWhenCancellingStarted);

  FreeJCData(Self.m_data);

  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

function NewJCData(): TJCdata;
begin
  Result.turnouts := TList<TJCTurnoutZav>.Create();
  Result.tracks := TList<Integer>.Create();
  Result.refuges := TList<TJCRefugeeZav>.Create();
  Result.crossings := TList<TJCCrossingZav>.Create();
  Result.locks := TList<TJCRefZav>.Create();
  Result.vb := TList<Integer>.Create();
  Result.permNotes := TList<string>.Create();
  Result.speedsGo := TList<TTrainSpeed>.Create();
  Result.speedsStop := TList<TTrainSpeed>.Create();
end;

procedure FreeJCData(jcdata: TJCData);
begin
  if (Assigned(jcdata.turnouts)) then
    jcdata.turnouts.Free();
  if (Assigned(jcdata.tracks)) then
    jcdata.tracks.Free();
  if (Assigned(jcdata.refuges)) then
    jcdata.refuges.Free();
  if (Assigned(jcdata.crossings)) then
  begin
    for var crossing in jcdata.crossings do
      crossing.closeTracks.Free();
    jcdata.crossings.Free();
  end;
  if (Assigned(jcdata.locks)) then
    FreeAndNil(jcdata.locks);
  if (Assigned(jcdata.vb)) then
    jcdata.vb.Free();
  if (Assigned(jcdata.permNotes)) then
    FreeAndNil(jcdata.permNotes);
  if (Assigned(jcdata.speedsGo)) then
    jcdata.speedsGo.Free();
  if (Assigned(jcdata.speedsStop)) then
    jcdata.speedsStop.Free();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TJC.Update();
begin
  try
    if (Self.state.destroyBlock > _JC_DESTROY_NONE) then
      Self.DynamicCancelling()
    else if (Self.state.destroyBlock = _JC_DESTROY_NC) then
      Self.DynamicCancellingNC()
    else if (Self.cancelling) then
      Self.CheckCancellingTracks();

    if ((Self.activating) or (Self.step = stepJcLastTrackWait)) then
    begin
      Self.UpdateActivating();
      Self.UpdateTimeOut();
    end;
  except
    on E: Exception do
    begin
      if (not log_last_error) then
        AppEvents.LogException(E, 'JC ' + Self.name + ' update error');
      if (Self.activating) then
        Self.CancelActivating('Výjimka')
      else
        Self.EmergencyCancelActivePath();
    end;
  end; // except
end;

/// /////////////////////////////////////////////////////////////////////////////

// kontroluje podminky pro staveni konkretni jizdni cesty
// vraci List prblemu (tzv. bariery), ktere definuji to, proc jizdni cestu nelze postavit (tedy vraci vsechny nesplnene podminky)
// tzv. kriticke bariery jsou vzdy na zacatu Listu
function TJC.barriers(nc: Boolean = false): TJCBarriers;
begin
  Result := TJCBarriers.Create();

  if (Self.activating) then
    Result.Add(TJCBarProcessing.Create());

  // signal
  begin
    var signal: TBlkSignal := Blocks.GetBlkSignalByID(Self.m_data.signalId);

    if (signal = nil) then
    begin
      Result.Add(TJCBarBlockNotExists.Create(Self.m_data.signalId));
      Exit();
    end;

    if (Self.lastTrack = nil) then
    begin
      Result.Add(TJCBarBlockNotExists.Create(0));
      Exit();
    end;

    if (TBlkSignal(signal).track = nil) then
    begin
      Result.Add(TJCBarSignalNoTrack.Create(signal));
      Exit();
    end;
  end;

  // turnouts
  for var turnoutZav: TJCTurnoutZav in Self.m_data.turnouts do
  begin
    var turnout: TBlkTurnout := Blocks.GetBlkTurnoutByID(turnoutZav.block);
    if (turnout = nil) then
    begin
      Result.Add(TJCBarBlockNotExists.Create(turnoutZav.block));
      Exit();
    end;
  end;

  // tracks
  for var trackZav: Integer in Self.m_data.tracks do
  begin
    var track: TBlkTrack := Blocks.GetBlkTrackOrRTByID(trackZav);
    if (track = nil) then
    begin
      Result.Add(TJCBarBlockNotExists.Create(trackZav));
      Exit();
    end;
  end;

  // crossings
  for var crossingZav: TJCCrossingZav in Self.m_data.crossings do
  begin
    var crossing: TBlkCrossing := Blocks.GetBlkCrossingByID(crossingZav.crossingId);
    if (crossing = nil) then
    begin
      Result.Insert(0, TJCBarBlockNotExists.Create(crossingZav.crossingId));
      Exit();
    end;

    // if track should be closed by path
    if (crossingZav.closeTracks.Count > 0) then
    begin
      var openTrack: TBlkTrack := Blocks.GetBlkTrackOrRTByID(crossingZav.openTrack);
      if (openTrack = nil) then
      begin
        Result.Insert(0, TJCBarBlockNotExists.Create(crossingZav.openTrack));
        Exit();
      end;

      for var trackZav: Integer in crossingZav.closeTracks do
      begin
        var closeTrack: TBlkTrack := Blocks.GetBlkTrackOrRTByID(trackZav);
        if (closeTrack = nil) then
        begin
          Result.Insert(0, TJCBarBlockNotExists.Create(trackZav));
          Exit();
        end;
      end;
    end;
  end;

  // refugees
  for var refugeeZav: TJCRefugeeZav in Self.m_data.refuges do
  begin
    var refugeeRef: TBlk := Blocks.GetBlkTrackOrRTByID(refugeeZav.ref_blk);
    if (refugeeRef = nil) then
    begin
      Result.Insert(0, TJCBarBlockNotExists.Create(refugeeZav.ref_blk));
      Exit();
    end;

    var refugee: TBlkTurnout := Blocks.GetBlkTurnoutByID(refugeeZav.block);
    if (refugee = nil) then
    begin
      Result.Insert(0, TJCBarBlockNotExists.Create(refugeeZav.block));
      Exit();
    end;
  end;

  // railway
  if (Self.m_data.railwayId > -1) then
  begin
    if (Self.lastTrack.typ <> btRT) then
    begin
      Result.Add(TJCBarBlockWrongType.Create(Self.lastTrack));
      Exit();
    end;

    var railway: TBlkRailway := Blocks.GetBlkRailwayByID(Self.m_data.railwayId);
    if (railway = nil) then
    begin
      Result.Insert(0, TJCBarBlockNotExists.Create(Self.m_data.railwayId));
      Exit();
    end;
  end;

  // locks
  for var refZaver: TJCRefZav in Self.m_data.locks do
  begin
    var lock: TBlkLock := Blocks.GetBlkLockByID(refZaver.block);
    if (lock = nil) then
    begin
      Result.Insert(0, TJCBarBlockNotExists.Create(refZaver.block));
      Exit();
    end;

    var lockRef: TBlkTrack := Blocks.GetBlkTrackOrRTByID(refZaver.ref_blk);
    if (lockRef = nil) then
    begin
      Result.Insert(0, TJCBarBlockNotExists.Create(refZaver.ref_blk));
      Exit();
    end;
  end;

  for var note: string in Self.m_data.permNotes do
    Result.Add(TJCBarGeneralNote.Create(note));


  if (nc) then
    Self.BarriersNC(Result)
  else
    Self.BarriersVCPC(Result);

  var privol: TBlksList := Blocks.PNSignals(Self.m_state.senderOR as TArea);
  try
    for var i: Integer := 0 to privol.Count - 1 do
      Result.Add(TJCBarPrivol.Create(privol[i] as TBlk));
  finally
    privol.Free();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TJC.BarriersVCPC(var barriers: TJCBarriers);
begin
  // signal
  var signal: TBlkSignal := TBlkSignal(Blocks.GetBlkByID(Self.m_data.signalId));
  if (not signal.enabled) then
    barriers.Add(TJCBarBlockDisabled.Create(signal));

  if ((signal.targetSignal <> ncStuj) or (signal.signal <> ncStuj)) then
    barriers.Add(TJCBarSignalActive.Create(signal));

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
      barriers.Add(TJCBarBlockDisabled.Create(track));

    // occupancy
    if ((i <> Self.m_data.tracks.Count-1) or (Self.typ <> TJCType.shunt)) then
    begin
      if (track.occupied <> TTrackState.Free) then
      begin
        if ((i = Self.m_data.tracks.Count - 1) and (Self.m_data.tracks.Count > 1)) then
          barriers.Add(TJCBarTrackLastOccupied.Create(track))
        else
          barriers.Add(TJCBarTrackOccupied.Create(track));
      end else begin
        if (track.IsTrain()) then
          barriers.Add(TJCBarTrackTrain.Create(track));
      end;
    end;

    if (track.Zaver <> TZaver.no) then
    begin
      if (track.Zaver = TZaver.ab) then
        barriers.Add(TJCBarTrackAB.Create(track))
      else
        barriers.Add(TJCBarTrackZaver.Create(track));
    end;

    if (track.lockout <> '') then
      barriers.Add(TJCBarBlockLockout.Create(track));

    if (track.note <> '') then
      barriers.Add(TJCBarBlockNote.Create(track));

    if ((track.PstIs()) and ((i < Self.m_data.tracks.Count-1) or (Self.typ <> TJCType.shunt))) then
      barriers.Add(TJCBarBlockPSt.Create(track));
  end;

  // turnouts
  for var turnoutZav: TJCTurnoutZav in Self.m_data.turnouts do
  begin
    var turnout: TBlkTurnout := TBlkturnout(Blocks.GetBlkByID(turnoutZav.block));

    if (turnout.position = TTurnoutPosition.disabled) then
      barriers.Add(TJCBarBlockDisabled.Create(turnout));

    if ((turnoutZav.position = TTurnoutPosition.plus) and (turnout.npBlokPlus <> nil) and
      (TBlkTrack(turnout.npBlokPlus).occupied = TTrackState.disabled)) then
      barriers.Add(TJCBarBlockDisabled.Create(turnout.npBlokPlus));

    if ((turnoutZav.position = TTurnoutPosition.minus) and (turnout.npBlokMinus <> nil) and
      (TBlkTrack(turnout.npBlokMinus).occupied = TTrackState.disabled)) then
      barriers.Add(TJCBarBlockDisabled.Create(turnout.npBlokMinus));

    if ((turnout.position = TTurnoutPosition.none) or (turnout.position = TTurnoutPosition.both)) then
      barriers.Add(TJCBarTurnoutNoPos.Create(turnout));

    // we don't need to check 'zaver' because is was checked on tracks

    if (turnout.lockout <> '') then
      barriers.Add(TJCBarBlockLockout.Create(turnout));

    if (turnout.note <> '') then
      barriers.Add(TJCBarBlockNote.Create(turnout));

    if (turnout.PstIs()) then
      barriers.Add(TJCBarBlockPst.Create(turnout));

    if (turnout.position <> turnoutZav.position) then
    begin
      if (turnout.emLock) then
        barriers.Add(TJCBarTurnoutEmLock.Create(turnout))
      else if (turnout.outputLocked) then
        barriers.Add(TJCBarTurnoutLocked.Create(turnout));
    end;

    // coupling
    var coupling: TBlkTurnout := TBlkTurnout(Blocks.GetBlkByID(turnout.GetSettings.coupling));
    if (coupling <> nil) then
    begin
      if (turnout.position <> turnoutZav.position) then
      begin
        if (coupling.occupied = TTrackState.occupied) then
          barriers.Add(TJCBarTrackOccupied.Create(coupling.parent))
        else if (coupling.emLock) then
          barriers.Add(TJCBarTurnoutEmLock.Create(coupling))
        else if (coupling.outputLocked) then
          barriers.Add(TJCBarTurnoutLocked.Create(coupling));

        if (coupling.PstIs()) then
          barriers.Add(TJCBarBlockPst.Create(coupling));
      end;

      if (coupling.lockout <> '') then
        barriers.Add(TJCBarBlockLockout.Create(coupling));
      if (coupling.note <> '') then
        barriers.Add(TJCBarBlockNote.Create(coupling));
    end;

    if ((turnoutZav.position = TTurnoutPosition.plus) and (turnout.npBlokPlus <> nil) and
      (TBlkTrack(turnout.npBlokPlus).occupied <> TTrackState.Free)) then
      barriers.Add(TJCBarTrackOccupied.Create(turnout.npBlokPlus));

    if ((turnoutZav.position = TTurnoutPosition.minus) and (turnout.npBlokMinus <> nil) and
      (TBlkTrack(turnout.npBlokMinus).occupied <> TTrackState.Free)) then
      barriers.Add(TJCBarTrackOccupied.Create(turnout.npBlokMinus));
  end;

  // crossings
  for var crossingZav: TJCCrossingZav in Self.m_data.crossings do
  begin
    var crossing: TBlkCrossing := TBlkCrossing(Blocks.GetBlkByID(crossingZav.crossingId));

    if (crossing.state = TBlkCrossingBasicState.disabled) then
      barriers.Add(TJCBarBlockDisabled.Create(crossing));

    if (crossing.state <> TBlkCrossingBasicState.error) then
    begin
      if (crossing.pcEmOpen) then
        barriers.Add(TJCBarCrossingEmergencyOpened.Create(crossing));
    end else
      barriers.Add(TJCBarCrossingError.Create(crossing));

    if (crossing.note <> '') then
      barriers.Add(TJCBarBlockNote.Create(crossing));
  end;

  // refugees
  for var refugeeZav: TJCRefugeeZav in Self.m_data.refuges do
  begin
    var refugee: TBlkTurnout := TBlkTurnout(Blocks.GetBlkByID(refugeeZav.block));

    if (refugee.position = TTurnoutPosition.disabled) then
      barriers.Add(TJCBarBlockDisabled.Create(refugee));

    if ((refugee.position = TTurnoutPosition.none) or (refugee.position = TTurnoutPosition.both)) then
      barriers.Add(TJCBarTurnoutNoPos.Create(refugee));

    if (refugee.lockout <> '') then
      barriers.Add(TJCBarBlockLockout.Create(refugee));

    if (refugee.note <> '') then
      barriers.Add(TJCBarBlockNote.Create(refugee));

    if (refugee.position <> refugeeZav.position) then
    begin
      if (refugee.emLock) then
        barriers.Add(TJCBarTurnoutEmLock.Create(refugee))

      else if (refugee.outputLocked) then
        barriers.Add(TJCBarTurnoutLocked.Create(refugee));

      if (refugee.occupied = TTrackState.occupied) then
        barriers.Add(TJCBarTurnoutOccupied.Create(refugee));
    end;

    if (refugee.PstIs()) then
      barriers.Add(TJCBarBlockPst.Create(refugee));

    // refugee's coupling
    var coupling: TBlkTurnout := TBlkTurnout(Blocks.GetBlkByID(refugee.GetSettings.coupling));
    if (coupling <> nil) then
    begin
      if (coupling.lockout <> '') then
        barriers.Add(TJCBarBlockLockout.Create(coupling));

      if (coupling.note <> '') then
        barriers.Add(TJCBarBlockNote.Create(coupling));

      if (coupling.PstIs()) then
        barriers.Add(TJCBarBlockPst.Create(coupling));

      if (refugee.position <> refugeeZav.position) then
      begin
        if (TBlkTurnout(coupling).Zaver > TZaver.no) then
        begin
          if (TBlkTurnout(coupling).Zaver = TZaver.ab) then
            barriers.Add(TJCBarTrackAB.Create(coupling))
          else
            barriers.Add(TJCBarTrackZaver.Create(coupling));
        end;

        if (TBlkTurnout(coupling).emLock) then
          barriers.Add(TJCBarTurnoutEmLock.Create(coupling))
        else if (TBlkTurnout(coupling).outputLocked) then
          barriers.Add(TJCBarTurnoutLocked.Create(coupling));

        if (TBlkTurnout(coupling).occupied = TTrackState.occupied) then
          barriers.Add(TJCBarTrackOccupied.Create(coupling));
      end;
    end;
  end;

  // railway
  if (Self.m_data.railwayId > -1) then
  begin
    var railway: TBlkRailway := TBlkRailway(Blocks.GetBlkByID(Self.m_data.railwayId));

    if (railway.direction = TRailwayDirection.disabled) then
      barriers.Add(TJCBarBlockDisabled.Create(railway));

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
        barriers.Add(TJCBarRailwayZAKPC.Create(railway))
      else
        barriers.Add(TJCBarRailwayZAKVC.Create(railway));
    end;
    if (railway.request) then
      barriers.Add(TJCBarRailwayRequesting.Create(railway));
    if (((TBlkRailway(railway).Zaver) or (TBlkRailway(railway).emLock)) and
      (Self.m_data.railwayDir <> TBlkRailway(railway).direction)) then
    begin
      barriers.Add(TJCBarRailwayWrongDir.Create(railway));
      cont := false;
    end;
    if ((cont) and (railway.Zaver)) then
      barriers.Add(TJCBarRailwayZaver.Create(railway));

    if (cont) and ((not railway.SameUserBothLinkers()) or (railway.emLock)) then
      if (((railway.GetSettings().rType = TRailwayType.permanent) or
        (railway.GetSettings().rType = TRailwayType.request)) and
        (Self.m_data.railwayDir <> railway.direction)) then
      begin
        barriers.Add(TJCBarRailwayWrongDir.Create(railway));
        cont := false;
      end;

    if ((cont) and (Self.m_data.railwayDir <> railway.direction)) then
    begin
      // trat beze smeru, do ktere bude dle predchozi podminky povoleno vjet -> trat s automatickou zmenou souhlasu
      // -> kontrola volnosti vsech useku trati (protoze nastane zmena smeru)
      if (not railway.ready) then
      begin
        barriers.Add(TJCBarRailwayWrongDir.Create(railway));
        cont := false;
      end;
    end;

    if ((cont) and (Self.typ = TJCType.Train)) then
    begin
      if (TBlkRT(Self.lastTrack).sectOccupied = TTrackState.occupied) then
        barriers.Add(TJCBarRailwayOccupied.Create(railway))
      else if (not TBlkRT(Self.lastTrack).sectReady) then
        barriers.Add(TJCBarRailwayNotReady.Create(railway));
    end;

    // kontrola stitku uvazky v nasi OR:
    if ((TBlkLinker(railway.linkerA).areas.Count > 0) and
      (TBlkLinker(railway.linkerA).areas[0] = Self.m_state.senderOR) and
      (TBlkLinker(railway.linkerA).note <> '')) then
      barriers.Add(TJCBarBlockNote.Create(railway.linkerA));

    if ((TBlkLinker(railway.linkerB).areas.Count > 0) and
      (TBlkLinker(railway.linkerB).areas[0] = Self.m_state.senderOR) and
      (TBlkLinker(railway.linkerB).note <> '')) then
      barriers.Add(TJCBarBlockNote.Create(railway.linkerB));

    // stitky a vyluky na tratovych usecich
    for var trackId: Integer in railway.GetSettings().trackIds do
    begin
      var track: TBlkTrack := TBlkTrack(Blocks.GetBlkByID(trackId));

      if (track.lockout <> '') then
        barriers.Add(TJCBarBlockLockout.Create(track));
      if (track.note <> '') then
        barriers.Add(TJCBarBlockNote.Create(track));
    end;
  end;

  // locks
  for var refZaver: TJCRefZav in Self.m_data.locks do
  begin
    var lock: TBlkLock := TBlkLock(Blocks.GetBlkByID(refZaver.block));
    if (lock.keyReleased) then
      barriers.Add(TJCBarLockNotLocked.Create(lock));
  end;

  // stolen engine
  var signalTrack: TBlkTrack;
  signalTrack := TBlkTrack(signal.track);

  if (signalTrack.IsTrain()) then
  begin
    var anyEngineManual := false;
    var train: TTrain := Self.GetTrain(signal, signalTrack);

    // manual-controlled engline
    if (Self.typ = TJCType.Train) then
    begin
      for var addr: Integer in train.vehicles do
      begin
        if ((RVDb[addr].data.typ <> TRVType.car) and ((RVDb[addr].stolen) or (RVDb[addr].manual))) then
        begin
          barriers.Add(TJCBarRVManual.Create(addr));
          anyEngineManual := true;
        end;
      end;
    end;

    // only some manual-controlled englines
    if (anyEngineManual) then
    begin
      for var addr: Integer in train.vehicles do
      begin
        if ((RVDb[addr].data.typ <> TRVType.car) and (not RVDb[addr].stolen) and (not RVDb[addr].manual)) then
        begin
          barriers.Add(TJCBarRVNotAllManual.Create(train.index));
          break;
        end;
      end;
    end;

    // direction of a train
    if (Self.typ = TJCType.Train) then
    begin
      if (train.sdata.dir_L or train.sdata.dir_S) then
        if (((signal.direction = TRVSite.odd) and (not train.sdata.dir_L)) or
          ((signal.direction = TRVSite.even) and (not train.sdata.dir_S))) then
          barriers.Add(TJCBarTrainWrongDir.Create(train.index));

      if (train.front <> signalTrack) then
        barriers.Add(TJCBarTrainNotFront.Create(train.index))
    end;

    for var addr: Integer in train.vehicles do
      for var i: Integer := 0 to _RV_FUNC_MAX do
        if (((RVDb[addr].data.funcDescription[i].Contains('posun')) or (RVDb[addr].data.funcDescription[i].Contains('Posun'))) and
            (RVDb[addr].stateFunctions[i])) then
          barriers.Add(TJCBarRVFuncActive.Create(addr, i));
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TJC.BarriersNC(var barriers: TJCBarriers);
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
        barriers.Add(TJCBarTrackAB.Create(track))
      else
        barriers.Add(TJCBarTrackZaver.Create(track));
    end;

    if (track.lockout <> '') then
      barriers.Add(TJCBarBlockLockout.Create(track));

    if (track.note <> '') then
      barriers.Add(TJCBarBlockNote.Create(track));

    if (track.PstIs()) then
      barriers.Add(TJCBarBlockPSt.Create(track));
  end;

  // turnouts
  for var turnoutZav: TJCTurnoutZav in Self.m_data.turnouts do
  begin
    var turnout: TBlkTurnout := TBlkTurnout(Blocks.GetBlkByID(turnoutZav.block));

    if (turnout.lockout <> '') then
      barriers.Add(TJCBarBlockLockout.Create(turnout));

    if (turnout.note <> '') then
      barriers.Add(TJCBarBlockNote.Create(turnout));

    if (turnout.PstIs()) then
      barriers.Add(TJCBarBlockPst.Create(turnout));

    if (turnout.position <> turnoutZav.position) then
    begin
      if (turnout.emLock) then
        barriers.Add(TJCBarTurnoutEmLock.Create(turnout))
      else if (turnout.outputLocked) then
        barriers.Add(TJCBarTurnoutLocked.Create(turnout));
    end;

    // turnout's refugee
    var coupling: TBlkTurnout := TBlkTurnout(Blocks.GetBlkByID(turnout.GetSettings.coupling));
    // pokud nemam ja polohu, predpokladam, ze spojka bude muset byt prestavena -> musi byt volna, bez zaveru, ...
    // kontrolovat zaver z useku eni potreba - pokud je problem se zaverem, vyvstane uz na useku JC, jinak je vyhybka v poloze, ktere zaver nevadi
    if (coupling <> nil) then
    begin
      if (turnout.position <> turnoutZav.position) then
      begin
        if (TBlkTurnout(coupling).emLock) then
          barriers.Add(TJCBarTurnoutEmLock.Create(coupling))
        else if (TBlkTurnout(coupling).outputLocked) then
          barriers.Add(TJCBarTurnoutLocked.Create(coupling));
      end;

      if (coupling.lockout <> '') then
        barriers.Add(TJCBarBlockLockout.Create(coupling));
      if (coupling.note <> '') then
        barriers.Add(TJCBarBlockNote.Create(coupling));
      if (coupling.PstIs()) then
        barriers.Add(TJCBarBlockPst.Create(coupling));
    end;
  end;

  // crossings
  for var crossingZav: TJCCrossingZav in Self.m_data.crossings do
  begin
    var crossing: TBlkCrossing := TBlkCrossing(Blocks.GetBlkByID(crossingZav.crossingId));
    if (crossing.note <> '') then
      barriers.Add(TJCBarBlockNote.Create(crossing));
  end;

  // refugees
  for var refugeeZav: TJCRefugeeZav in Self.m_data.refuges do
  begin
    var refugee: TBlkTurnout := TBlkTurnout(Blocks.GetBlkByID(refugeeZav.block));

    if (refugee.lockout <> '') then
      barriers.Add(TJCBarBlockLockout.Create(refugee));

    if (refugee.note <> '') then
      barriers.Add(TJCBarBlockNote.Create(refugee));

    if (refugee.position <> refugeeZav.position) then
    begin
      if (refugee.emLock) then
        barriers.Add(TJCBarTurnoutEmLock.Create(refugee))

      else if ((refugee.Zaver <> TZaver.no) or (refugee.outputLocked)) then
        barriers.Add(TJCBarTurnoutLocked.Create(refugee));
    end;

    if (refugee.PstIs()) then
      barriers.Add(TJCBarBlockPst.Create(refugee));

    // refugee's coupling
    var coupling: TBlkTurnout := TBlkTurnout(Blocks.GetBlkByID(refugee.GetSettings.coupling));
    if (coupling <> nil) then
    begin
      if (coupling.lockout <> '') then
        barriers.Add(TJCBarBlockLockout.Create(coupling));

      if (coupling.note <> '') then
        barriers.Add(TJCBarBlockNote.Create(coupling));

      if (coupling.PstIs()) then
        barriers.Add(TJCBarBlockPst.Create(coupling));

      if (refugee.position <> refugeeZav.position) then
      begin
        if (coupling.Zaver > TZaver.no) then
        begin
          if (coupling.Zaver = TZaver.ab) then
            barriers.Add(TJCBarTrackAB.Create(coupling))
          else
            barriers.Add(TJCBarTrackZaver.Create(coupling));
        end;

        if (coupling.emLock) then
          barriers.Add(TJCBarTurnoutEmLock.Create(coupling))
        else if (coupling.outputLocked) then
          barriers.Add(TJCBarTurnoutLocked.Create(coupling));
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
        barriers.Add(TJCBarBlockLockout.Create(track));
      if (track.note <> '') then
        barriers.Add(TJCBarBlockNote.Create(track));
    end;

    // kontrola stitku uvazky v nasi OR:
    if ((TBlkLinker(railway.linkerA).areas.Count > 0) and
      (TBlkLinker(railway.linkerA).areas[0] = Self.m_state.senderOR) and
      (TBlkLinker(railway.linkerA).note <> '')) then
      barriers.Add(TJCBarBlockNote.Create(railway.linkerA));

    if ((TBlkLinker(railway.linkerB).areas.Count > 0) and
      (TBlkLinker(railway.linkerB).areas[0] = Self.m_state.senderOR) and
      (TBlkLinker(railway.linkerB).note <> '')) then
      barriers.Add(TJCBarBlockNote.Create(railway.linkerB));
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TJC.Log(msg: string; level: TLogLevel; source: TLogSource);
begin
  Logging.Log('JC ' + Self.name + ': ' + msg, level, source);
end;

procedure TJC.LogStep(msg: string; level: TLogLevel; source: TLogSource);
begin
  Self.Log('krok '+IntToStr(Integer(Self.step))+': '+msg, level, source);
end;

/// /////////////////////////////////////////////////////////////////////////////

// stavi konkretni jizdni cestu
// tato fce ma za ukol zkontrolovat vstupni podminky jizdni cesty
// tato funkce jeste nic nenastavuje!
procedure TJC.Activate(senderPnl: TIdContext; senderOR: TObject; bariery_out: TJCBarriers; from_stack: TObject;
  nc: Boolean; fromAB: Boolean; abAfter: Boolean; ignoreWarn: Boolean);
begin
  Self.m_state.timeOut := Now + EncodeTimeSec(_JC_INITPOTVR_TIMEOUT_SEC);

  Self.m_state.from_stack := from_stack;
  Self.m_state.senderOR := senderOR;
  Self.m_state.senderPnl := senderPnl;
  Self.m_state.nc := nc;
  Self.m_state.ab := (abAfter) and (Self.typ = TJCType.Train);
  Self.m_state.crossingWasClosed := false;
  Self.m_state.lastTrackOrRailwayOccupied := false;
  Self.m_state.RCtimer := -1;
  Self.m_state.RCtimerArea := nil;
  Self.m_state.RClongTime := False; // default = short time

  Self.Log('Požadavek na stavění, kontroluji podmínky');

  var barriers: TJCBarriers := Self.barriers(Self.m_state.nc);
  var UPO: TUPOItems := TList<TUPOItem>.Create;
  try
    // ignorujeme AB zaver pokud je staveno z AB seznamu
    if (fromAB) then
      for var i: Integer := barriers.Count - 1 downto 0 do
        if (barriers[i].ClassType = TJCBarTrackAB) then
          barriers.Delete(i);

    // existuji kriticke bariery?
    var critical: Boolean := false;
    for var barrier: TJCBarrier in barriers do
    begin
      if ((barrier.ClassType = TJCBarTrackLastOccupied) or (barrier.ClassType = TJCBarRailwayOccupied)) then
        Self.m_state.lastTrackOrRailwayOccupied := true;

      if (not barrier.CanContinueByConfirm()) then
      begin
        critical := true;
        UPO.Add(barrier.ToUPO());
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
    end else begin
      // bariery k potvrzeni
      // kdyz je neni komu oznamit, cesta se rovnou stavi
      if (((barriers.Count > 0) or ((nc) and (from_stack <> nil))) and (senderPnl <> nil) and (not ignoreWarn)) then
      begin
        Self.Log('Celkem ' + IntToStr(barriers.Count) + ' warning bariér, žádám potvrzení...');
        for var i: Integer := 0 to barriers.Count - 1 do
          UPO.Add(barriers[i].ToUPO());

        // pokud se jedna o NC ze zasobniku, zobrazuji jeste upozorneni na NC
        if ((nc) and (from_stack <> nil)) then
        begin
          var item: TUPOItem;
          item[0] := GetUPOLine('Pozor !', taCenter, TJopColor.yellow, TJopColor.grayDark);
          item[1] := GetUPOLine('Stavění nouzové cesty.');
          item[2] := GetUPOLine('');
          UPO.Add(item);
        end;

        PanelServer.UPO(Self.m_state.senderPnl, UPO, false, Self.UPO_OKCallback, Self.UPO_EscCallback, Self);
        Self.step := stepConfBarriers;
      end else begin
        // v jzdni ceste nejsou zadne bariery -> stavim
        if (barriers.Count = 0) then
          Self.Log('Žádné bariéry, stavím')
        else if (ignoreWarn) then
          Self.Log('Jsou warning bariéry, ale je ignoreWarn -> stavím')
        else if (senderPnl = nil) then
          Self.Log('Jsou warning bariéry, ale senderPnl=nil -> stavím');

        Self.SetInitStep();
      end;
    end;
  except
    on E:Exception do
    begin
      Self.CancelActivating('Výjimka při stavění');
      AppEvents.LogException(E, 'JC.Activate');
    end;
  end;

  if (bariery_out <> nil) then
  begin
    bariery_out.AddRange(barriers);
    barriers.OwnsObjects := False; // data moved to bariery_out
  end;
  barriers.Free();
  UPO.Free();
end;

procedure TJC.Activate(senderPnl: TIdContext; senderOR: TObject; from_stack: TObject; nc: Boolean;
  fromAB: Boolean; abAfter: Boolean; ignoreWarn: Boolean);
begin
  Self.Activate(senderPnl, senderOR, nil, from_stack, nc, fromAB, abAfter, ignoreWarn);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TJC.PS_vylCallback(Sender: TIdContext; success: Boolean);
begin
  // pro potvrzovaci sekvenci vyluky by mel byt krok '6'
  if (Self.step <> stepConfSeq) then
    Exit();

  if (not success) then
  begin
    Self.CancelActivating('', False);
    Exit();
  end;

  // znovu zkontrolujeme bariery (behem potvrzovani se mohly vyskytnout)
  var barriers: TJCBarriers := Self.barriers(Self.m_state.nc);

  try
    // existuji kriticke bariery?
    // behem potvrzovani se mohly vyskytnout
    if (not CanContinueByConfirmButProcessing(barriers)) then
    begin
      Self.CancelActivating('Nelze postavit - kritické bariéry');
      if (Self.m_state.senderPnl <> nil) and (Self.m_state.senderOR <> nil) then
        PanelServer.BottomError(Self.m_state.senderPnl, 'Nelze postavit ' + Self.name + ' - kritické bariéry',
          (Self.m_state.senderOR as TArea).ShortName, 'TECHNOLOGIE');
      Exit(); // will call barriers.Free() in finally block
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

  // znovu zkontrolujeme bariery (behem potvrzovani se mohly zmenit)
  var barriers: TJCBarriers := Self.barriers(Self.m_state.nc);
  try
    // existuji kriticke bariery?
    // behem potvrzovani se mohly vyskytnout
    if (not JCBarriers.CanContinueByConfirmButProcessing(barriers)) then
    begin
      Self.CancelActivating('Nelze postavit - kritické bariéry');
      if (Self.m_state.senderPnl <> nil) and (Self.m_state.senderOR <> nil) then
        PanelServer.BottomError(Self.m_state.senderPnl, 'Nelze postavit ' + Self.name + ' - kritické bariéry',
          (Self.m_state.senderOR as TArea).ShortName, 'TECHNOLOGIE');
      Exit();
    end;

    // existuji bariery na potvrzeni potvrzovaci sekvenci ?
    var csItems: TList<TConfSeqItem> := TList<TConfSeqItem>.Create();
    try
      for var barrier in barriers do
        if ((barrier.IsRisky()) and (barrier.InheritsFrom(TJCBlockBarrier))) then
          csItems.Add(CSItem((barrier as TJCBlockBarrier).block, barrier.RiskyNote()));

      if (csItems.Count > 0) then
      begin
        // ano, takoveto bariery existuji -> potvrzovaci sekvence
        Self.Log('Bariéry s potvrzovací sekvencí, žádám potvrzení...');

        if (Self.m_state.senderPnl <> nil) and (Self.m_state.senderOR <> nil) then
          PanelServer.ConfirmationSequence(Self.m_state.senderPnl, Self.PS_vylCallback, (Self.m_state.senderOR as TArea),
            'Jízdní cesta s potvrzením', GetObjsList(Self.signal, Self.lastTrack), csItems, True, False);

        Self.step := stepConfSeq;
      end else begin
        // ne, takoveto bariery neexistuji -> stavim jizdni cestu
        Self.SetInitStep();
      end;
    finally
      csItems.Free();
    end;

  finally
    barriers.Free();
  end;
end;

procedure TJC.UPO_EscCallback(Sender: TObject);
begin
  if (Self.step = stepConfBarriers) then
  begin
    Self.Log('UPO odmítnuto');
    Self.CancelActivating('', False);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

// jakmile je zavolano Activate, tato funkce se stara o to, aby staveni doslo az do konce.
// Stavi vyhybky, zavira prejezdy atd.
procedure TJC.UpdateActivating();
var
  npCall: ^TNPCallerData;
begin
  if ((not Self.activating) and (Self.step <> stepJcLastTrackWait)) then
    Exit();

  var signal: TBlkSignal := TBlkSignal(Self.signal);

  /// ///////////////////////////////////////////////////////////////////////////
  // staveni vlakovych a posunovych cest:

  case (Self.step) of
    stepJcInit:
      begin
        // U cest do trati uvolneni zaveru posledniho useku zpusobi uvolneni zaveru predposledniho bloku
        // To je hlavne pro to, aby sel zrusit zaver posledniho useku pomoci NUZ
        // U stanicnich koleji se zaver musi zrusit explicitnim zavedenim NUZ na stanicni koleji.
        if ((Self.m_data.tracks.Count > 1) and (Self.lastTrack.typ = TBlkType.btRT)) then
        begin
          var oneButLastTrack: TBlkTrack := TBlkTrack(Blocks.GetBlkByID(Self.m_data.tracks[Self.m_data.tracks.Count-2]));
          oneButLastTrack.AddChangeEvent(
            oneButLastTrack.eventsOnZaverReleaseOrAB,
            CreateChangeEventInt(ceCaller.CopyTrackZaver, Self.lastTrack.id)
          );
        end;

        Self.LogStep('Useky: nastavuji staveci zavery');
        for var trackZav: Integer in Self.m_data.tracks do
        begin
          var track: TBlkTrack := TBlkTrack(Blocks.GetBlkByID(trackZav));
          track.zaver := TZaver.staveni;
        end;

        Self.LogStep('Vyhybky: zamykam do pozadovanych poloh');
        Self.m_state.nextTurnout := -1;
        var stavim: Cardinal := 0;
        var nextTurnout: Integer := -1;
        for var i: Integer := 0 to Self.m_data.turnouts.Count - 1 do
        begin
          var turnoutZav: TJCTurnoutZav := Self.m_data.turnouts[i];
          var turnout: TBlkTurnout := TBlkTurnout(Blocks.GetBlkByID(Self.m_data.turnouts[i].block));

          if (turnout.position <> TTurnoutPosition(turnoutZav.position)) then
          begin
            if (stavim >= GlobalConfig.jcMaxMovingTurnouts) then
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
          var refugee: TBlkTurnout := TBlkTurnout(Blocks.GetBlkByID(refugeeZav.block));

          // nastaveni odvratu
          if (refugee.position <> TTurnoutPosition(refugeeZav.position)) then
          begin
            if (stavim >= GlobalConfig.jcMaxMovingTurnouts) then
            begin
              if (nextTurnout = -1) then
                nextTurnout := i;
              continue;
            end;
            Inc(stavim);
          end;

          refugee.IntentionalLock();

          // zaver odvratu se rusi pri ruseni zaveru referencniho bloku
          var track: TBlkTrack := TBlkTrack(Blocks.GetBlkByID(refugeeZav.ref_blk));
          track.AddChangeEvent(track.eventsOnZaverReleaseOrAB, CreateChangeEventInt(ceCaller.TurnoutUnlock, refugeeZav.block));

          // Warning: this may call callback directly
          // Callback for just-locking turnout will have no effect due to nextVyhybka = -1
          refugee.SetPosition(TTurnoutPosition(refugeeZav.position), true, false, Self.TurnoutMovedJCPC,
            Self.TurnoutErrJCPC);
        end;

        Self.m_state.nextTurnout := nextTurnout;

        Self.LogStep('Zamky: nastavuji zavery');
        for var refZav: TJCRefZav in Self.m_data.locks do
        begin
          var refTrack: TBlkTrack := TBlkTrack(Blocks.GetBlkByID(refZav.ref_blk));
          refTrack.AddChangeEvent(refTrack.eventsOnZaverReleaseOrAB, CreateChangeEventInt(ceCaller.LockCancelZaver, refZav.block));

          // nastaveni zaveru zamku
          var lock: TBlkLock := TBlkLock(Blocks.GetBlkByID(refZav.block));
          lock.zaver := true;
        end;

        // trat
        if (Self.m_data.railwayId > -1) then
        begin
          Self.LogStep('Nastavuji zaver trati');
          var railway: TBlkRailway := TBlkRailway(Blocks.GetBlkByID(Self.m_data.railwayId));

          if (railway.zaver) then
          begin
            Self.CancelActivating('Nesouhlas');
            Exit();
          end;
          railway.zaver := true;
          railway.direction := Self.m_data.railwayDir;

          // zruseni zaveru posledniho bloku JC zpusobi zruseni zaveru trati
          Self.lastTrack.AddChangeEvent(Self.lastTrack.eventsOnZaverReleaseOrAB,
            CreateChangeEventInt(ceCaller.RailwayCancelZaver, Self.m_data.railwayId));
        end;

        Self.step := stepJcTurnoutsMoving;
        Self.LogStep('Vyhybky: poloha: detekce');
      end; // case 0

    stepJcTurnoutsMoving:
      begin
        for var turnoutZav: TJCturnoutZav in Self.m_data.turnouts do
        begin
          var turnout: TBlkTurnout := TBlkTurnout(Blocks.GetBlkByID(turnoutZav.block));
          if (turnout.position <> turnoutZav.position) then
            Exit();
        end;
        for var refugeeZav: TJCRefugeeZav in Self.m_data.refuges do
        begin
          var turnout: TBlkTurnout := TBlkTurnout(Blocks.GetBlkByID(refugeeZav.block));
          if (turnout.position <> refugeeZav.position) then
            Exit();
        end;

        Self.LogStep('Vyhybky: poloha: OK');
        Self.m_state.nextTurnout := -1;

        Self.LogStep('Useky: nastavuji nouzovy zaver');
        for var trackZav: Integer in Self.m_data.tracks do
        begin
          var track: TBlkTrack := TBlkTrack(Blocks.GetBlkByID(trackZav));
          track.zaver := TZaver.nouz;
        end;

        Self.LogStep('Useky: kontroluji volnost useku s neprofilovymi styky, zapevnuji neprofilove useky');
        for var turnoutZav: TJCTurnoutZav in Self.m_data.turnouts do
        begin
          var neprofil: TBlkTrack := nil;
          var turnout: TBlkTurnout := TBlkTurnout(Blocks.GetBlkByID(turnoutZav.block));

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
              Self.LogStep('Krok 14 : Neprofilovy usek ' + neprofil.name + ' neuvolnen!');
              Self.CancelActivating();
              Exit();
            end;

            neprofil.AddNeprofilJC(Self.m_data.id);

            var turnoutTrack: TBlkTrack := TBlkTrack(Blocks.GetBlkByID(turnout.trackID));

            npCall := GetMemory(SizeOf(TNPCallerData));
            npCall^.usekId := neprofil.id;
            npCall^.jcId := Self.m_data.id;
            turnoutTrack.AddChangeEvent(turnoutTrack.eventsOnZaverReleaseOrAB, CreateChangeEvent(ceCaller.RemoveUsekNeprofil, npCall));
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
        Self.DetermineCrossingsToClose(Self.m_state.crossingsToClose);

        for var i: Integer := 0 to Self.m_data.crossings.Count - 1 do
        begin
          var crossingZav: TJCCrossingZav := Self.m_data.crossings[i];
          if (crossingZav.openTrack = -1) then
            continue;

          var crossing: TBlkCrossing := TBlkCrossing(Blocks.GetBlkByID(crossingZav.crossingId));

          if (Self.m_state.crossingsToClose[i]) then
          begin
            Self.LogStep('Prejezd ' + crossing.name + ' - uzaviram');
            crossing.zaver := true;

            // uvolnit zaver prejezdu pri zruseni zaveru (nebo nastaveni na AB zaver) referencniho useku prejezdu
            var openTrack: TBlkTrack := TBlkTrack(Blocks.GetBlkByID(crossingZav.openTrack));
            openTrack.AddChangeEvent(
              openTrack.eventsOnZaverReleaseOrAB,
              CreateChangeEventInt(ceCaller.CrossingCancelZaver, crossingZav.crossingId)
            );

            anyClosed := true;
          end else begin
            Self.LogStep('Prejezd ' + crossing.name + ' - zadny aktivacni usek neobsazen - nechavam otevreny');

            // prejezd neuzaviram -> pridam pozadavek na zavreni pri obsazeni do vsech aktivacnich useku
            for var closeTrackId: Integer in crossingZav.closeTracks do
            begin
              var closeTrack: TBlkTrack := TBlkTrack(Blocks.GetBlkByID(closeTrackId));
              if (not closeTrack.eventsOnOccupy.Contains(CreateChangeEventInt(Self.TrackCloseCrossing, i))) then
                closeTrack.AddChangeEvent(closeTrack.eventsOnOccupy, CreateChangeEventInt(Self.TrackCloseCrossing, i));
            end;
          end;
        end;

        if (anyClosed) then
        begin
          Self.step := stepJcWaitCross;
          Self.m_state.timeOut := Now + EncodeTimeSec(_JC_PRJ_TIMEOUT_SEC);
        end
        else
          Self.step := stepJcFinalZaver;
      end;

    stepJcWaitCross:
      begin
        // kontrola stavu prejezdu
        for var i: Integer := 0 to Self.m_data.crossings.Count-1 do
        begin
          if (not Self.m_state.crossingsToClose[i]) then
            continue;

          var crossing: TBlkCrossing := TBlkCrossing(Blocks.GetBlkByID(Self.m_data.crossings[i].crossingId));
          if (not crossing.safelyClosed) then
            Exit();
        end;

        Self.LogStep('Vsechny pozadovane prejezdy uzavreny');
        Self.step := stepJcFinalZaver;
      end;

    stepJcFinalZaver:
      begin
        Self.LogStep('Useky: nastavit validni zaver');

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
          Self.LogStep('Navestidlo: nestavim');
          Self.step := stepJcFinish;
        end else begin
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
        end else if ((signal.targetSignal = ncStuj) and (signal.signal = ncStuj)) then
        begin
          // Nekdo ihned pri staveni navestidla dal STUJ
          Self.Log('Krok 15 : navestidlo NEpostaveno');
          Self.step := stepJcFinish;
        end;
      end;

    stepJcFinish:
      begin
        Self.CancelSignalBegin();
        Self.CancelVBs();
        Self.CancelTrackEnd();

        // nastavit front blok vlaku
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
          if (signal.targetSignal > ncStuj) then
            signal.signal := ncStuj;
          // Send to all areas because DN could be from any source (last track freed etc.)
          Self.signal.BottomErrorBroadcast('Podmínky pro '+Self.name+' nesplněny!', 'TECHNOLOGIE');
          Self.LogStep('Podmínky pro JC nesplněny!');
          if (Self.m_state.from_stack <> nil) then
          begin
            (Self.m_state.from_stack as TORStack).mode := TORStackMode.PV;
            Self.m_state.from_stack := nil;
          end;
          Exit();
        end;

        if ((Self.m_data.railwayId > -1) and (Self.typ = TJCType.shunt)) then
        begin
          var railway: TBlkRailway := TBlkRailway(Blocks.GetBlkByID(Self.m_data.railwayId));
          case (Self.m_data.railwayDir) of
            TRailwayDirection.AtoB:
              TBlkLinker(railway.linkerA).departureForbidden := true;
            TRailwayDirection.BtoA:
              TBlkLinker(railway.linkerB).departureForbidden := true;
          end;
        end;

        if ((signal.targetSignal = ncStuj) or (signal.ZAM) or (Self.m_state.lastTrackOrRailwayOccupied)) then
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

        Self.LogStep('Postavena JC ' + Self.name);
      end;

    stepJcLastTrackWait:
      begin
        var lastTrack: TBlkRT := TBlkRT(Self.lastTrack);

        if (Self.m_data.railwayId > -1) then
        begin
          if (lastTrack.sectReady) then
          begin
            Self.m_state.lastTrackOrRailwayOccupied := false;
            Self.DN(nil, nil);
          end;
        end else begin
          if ((lastTrack.occupied = TTrackState.Free) and (not lastTrack.IsTrain)) then
          begin
            Self.m_state.lastTrackOrRailwayOccupied := false;
            Self.DN(nil, nil);
          end;
        end;
      end;

    /// ////////////////////////////////////////////////////////////////////////
    // staveni nouzovych cest:

    stepNcInit:
      begin
        // vsem usekum nastavime staveci zaver:
        Self.LogStep('Useky: nastavuji staveci zavery');
        for var trackZav: Integer in Self.m_data.tracks do
        begin
          var track: TBlkTrack := TBlkTrack(Blocks.GetBlkByID(trackZav));
          track.zaver := TZaver.staveni;
        end;

        // nastavit nouzovy zaver uvazky
        if (Self.m_data.railwayId > -1) then
        begin
          Self.LogStep('Trat: nastavuji nouzovy zaver uvazky');
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
        Self.LogStep('Vyhybky: nastavuji do pozadovanych poloh');

        Self.m_state.nextTurnout := 0;

        while ((Self.m_state.nextTurnout <> -1) and (Self.m_state.nextTurnout < Integer(GlobalConfig.jcMaxMovingTurnouts)) and
          (Self.m_state.nextTurnout < Self.m_data.turnouts.Count)) do
        begin
          var turnoutZav: TJCTurnoutZav := Self.m_data.turnouts[Self.m_state.nextTurnout];
          var turnout: TBlkTurnout := TBlkTurnout(Blocks.GetBlkByID(turnoutZav.block));

          Inc(Self.m_state.nextTurnout);
          turnout.SetPosition(TTurnoutPosition(turnoutZav.position),
            // this call could increase nextVyhybka directly! or even set nextVyhybka = -1
            true, false, Self.TurnoutMovedNC, Self.TurnoutErrNC);
        end;

        // For simplicity solve odvrat just in callback
        // This may be a little bit slower, but will generally work fine

        Self.LogStep('Prejezdy: uzaviram');
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
          var lock: TBlkLock := TBlkLock(Blocks.GetBlkByID(refZav.block));
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
      end;

    stepNcBarrierUpdate:
      begin
        // prubezne kontroluji podminky a zobrazuji potvrzovaci sekvenci

        // zjistime aktualni bariery:
        Self.m_state.ncBariery.Clear();
        Self.BarriersNCToAccept(Self.m_state.ncBariery);

        // kontrolujeme rozdilnost seznamu:
        if (Self.m_state.ncBariery.Count <> Self.m_state.ncBarieryCntLast) then
        begin
          Self.LogStep('Zmena potvr., odesilam aktualni seznam');
          var str: string;
          if (Self.typ = TJCType.Train) then
            str := 'Zapnutí přivolávací návěsti'
          else
            str := 'Nouzová posunová cesta';

          if (Self.m_state.senderPnl <> nil) and (Self.m_state.senderOR <> nil) then
            PanelServer.ConfirmationSequence(Self.m_state.senderPnl, Self.NC_PS_Callback,
              Self.m_state.senderOR as TArea, str, GetObjsList(signal, lastTrack),
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
        Self.LogStep('Useky: rusim zavery');
        for var trackZav: Integer in Self.m_data.tracks do
        begin
          var track: TBlkTrack := TBlkTrack(Blocks.GetBlkByID(trackZav));
          track.zaver := TZaver.no;
        end;

        // STUJ nelze udelat na PN nouzovych posunovych cest
        if (Self.typ = TJCType.train) then
          signal.privol := Self;

        // i pokud je navetidlo ve STUJ, nastavuji navest (to je spravne chovani podle JOP)
        if ((Self.typ = TJCType.Train) and (signal.enabled)) then
        begin
          Self.SetSignalSignal();
          Self.step := stepNcSignalWait;
        end
        else
          Self.step := stepNcFinish;
      end;

    stepNcSignalWait:
      begin
        if (signal.signal = ncPrivol) then
        begin
          Self.LogStep('Navestidlo postaveno');
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

        // presun vlaku z useku pred navestidlem do posledniho useku JC

        // Presun probehne za techto podminek:
        // a) Bud privolavame do stanice = na dopravni kolej
        // b) Nebo privolavame do trate, ktera MUSI byt ve spravnem smeru a MUSI v ni byt zavedena blokova podminka

        if (Self.typ = TJCType.Train) then
        begin
          var signalTrack: TBlkTrack := signal.track as TBlkTrack;
          var train: TTrain := Self.GetTrain(signal, signalTrack);

          Self.m_state.destroyBlock := _JC_DESTROY_NC;

          // a)
          if ((lastTrack.typ = btTrack) and (Self.lastTrack.spnl.stationTrack) and
            (not Self.lastTrack.TrainsFull())) then
          begin
            if (signalTrack.IsTrain()) then
            begin
              if ((signalTrack.typ = btRT) and (TBlkRT(signalTrack).inRailway > -1)) then
              begin
                var railway: TBlkRailway := TBlkRailway(Blocks.GetBlkByID((signalTrack as TBlkRT).inRailway));
                railway.RemoveTrain(train);
              end;

              // na dopravni kolej vlozime vlak blize vjezdovemu navestidlu
              if (signal.direction = TRVSite.odd) then
                Self.lastTrack.AddTrainL(Train)
              else
                Self.lastTrack.AddTrainS(Train);

              signalTrack.RemoveTrain(Train);
              train.front := Self.lastTrack;
            end;
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
              // Pridat vlak do posledniho bloku trati
              if ((railway.state.trains.Count = 0) and ((railway.GetLastTrack(Self.data.railwayDir) as TBlkRT)
                .Zaver = TZaver.no)) then
              begin
                rtAdd := (railway.GetLastTrack(Self.data.railwayDir) as TBlkRT);
                railway.TrainChangeOR(Train, Self.data.railwayDir);
                if (railway.ChangesTrainDir()) then
                  train.ChangeDirection();
              end;
            end else begin
              if ((not Self.lastTrack.IsTrain()) and (railway.BP) and
                (railway.direction = Self.data.railwayDir)) then
              begin
                // Pridat vlak do prvniho bloku trati
                rtAdd := (lastTrack as TBlkRT);
                rtAdd.bpError := true;
              end;
            end;

            if (rtAdd <> nil) then
            begin
              railway.AddTrain(TBlkRailwayTrain.Create(Train.index));
              rtAdd.AddTrainL(Train); // tady je jedno jestli zavolat L nebo S
              // v trati muze byt na jednom useku vzdy jen jeden vlak
              // kontrolovano vyse
              railway.Change();
              signalTrack.RemoveTrain(train);
              train.front := rtAdd;
            end;
          end;
        end; // if typ = vlak

        Self.LogStep('Postavena NC ' + Self.name);
      end;
  end; // case
end;

/// /////////////////////////////////////////////////////////////////////////////

// je volana, pokud behem staveni dojde k vyjimce
// napriklad pri kontrole obsazenosti useku v JC apod.
procedure TJC.CancelActivating(reason: string = ''; stackToPV: Boolean = True);
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
      end;

    stepConfBarriers, stepCritBarriers:
      begin
        if (Self.m_state.senderPnl <> nil) then
          PanelServer.CancelUPO(Self.m_state.senderPnl, Self);
      end;
  end;

  // staveci zavery jsou zruseny, ostatni zavery zustavaji (lze je vyNUZovat)
  for var trackZaver: Integer in Self.data.tracks do
  begin
    var track: TBlkTrack := Blocks.GetBlkTrackOrRTByID(trackZaver);
    if ((track <> nil) and (track.Zaver = TZaver.staveni)) then // opravdu muze byt nil - kdyz usek v ceste neexistuje
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
  if (Self.m_state.from_stack <> nil) then
  begin
    if (stackToPV) then
      (Self.m_state.from_stack as TORStack).mode := TORStackMode.PV;
    (Self.m_state.from_stack as TORStack).firstEnabled := True;
  end;

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
    Self.lastTrack.jcEnd := TZaver.no;
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

procedure TJC.StartCancelling(senderArea: TArea);
begin
  if (Self.cancelling) then
    Exit();

  Self.m_state.occupyStateWhenCancellingStarted.Clear();
  for var trackId: Integer in Self.m_data.tracks do
    Self.m_state.occupyStateWhenCancellingStarted.Add(Blocks.GetBlkTrackOrRTByID(trackId).occupied);

  Self.m_state.RCtimerArea := senderArea;
  Self.m_state.RCtimer := senderArea.AddCountdown(Self.Cancel, EncodeTimeSec(Self.CancelTimeSec()));

  if ((Assigned(Self.signal)) and (Self.signal.typ = TBlkType.btSignal)) then
    TBlkSignal(Self.signal).ab := False;
  Self.CancelWithoutTrackRelease();
  if ((Assigned(Self.signal)) and (Self.signal.typ = TBlkType.btSignal)) then
    Blocks.TrainPrediction(Self.signal as TBlkSignal);
  Self.Log('Spuštěn odpočet času do definitivního zrušení cesty.');
end;

procedure TJC.StopCancelling();
begin
  if (not Self.cancelling) then
    Exit();
  if (Self.m_state.RCtimerArea <> nil) then
    Self.m_state.RCtimerArea.RemoveCountdown(Self.m_state.RCtimer);
  Self.m_state.RCtimer := -1;
  Self.m_state.RCtimerArea := nil;
  Self.Log('Zastaveno rušení cesty.');
end;

procedure TJC.Cancel(Sender: TObject = nil);
begin
  if ((Self.m_state.RCtimer > -1) and (Self.m_state.RCtimerArea <> nil)) then
    if (Self.m_state.RCtimerArea.IsCountdown(Self.m_state.RCtimer)) then
      Self.m_state.RCtimerArea.RemoveCountdown(Self.m_state.RCtimer);

  Self.m_state.occupyStateWhenCancellingStarted.Clear();
  Self.m_state.RCtimer := -1;
  Self.m_state.RCtimerArea := nil;

  Self.CancelWithoutTrackRelease();

  if ((Self.signal as TBlkSignal).dnJC = Self) then
    (Self.signal as TBlkSignal).DNjc := nil;

  for var trackZaver: Integer in Self.m_data.tracks do
  begin
    var track: TBlkTrack := TBlkTrack(Blocks.GetBlkByID(trackZaver));
    track.zaver := TZaver.no;
  end;

  // Ostatni zavery (trate, prejezdy, odvraty, zamky, ...) se zrusit automaticky pri zruseni zaveru
  // referencniho useku.

  Self.Log('Zrušena');
end;

// ruseni jizdni cesty bez ruseni zaveru bloku
procedure TJC.CancelWithoutTrackRelease(showError: Boolean = False);
begin
  Self.Log('CancelWithoutTrackRelease');

  if ((Self.signal <> nil) and (Self.signal.typ = btSignal) and (TBlkSignal(Self.signal).targetSignal > ncStuj) and (TBlkSignal(Self.signal).DNjc = Self)) then
  begin
    var signal: TBlkSignal := TBlkSignal(Self.signal);
    signal.signal := ncStuj;
    if (showError) then
      signal.BottomErrorBroadcast('Chyba povolovací návěsti ' + signal.name, 'TECHNOLOGIE');
    if (signal.ab) then
    begin
      signal.ab := false; // automaticky zrusi AB
      signal.BottomErrorBroadcast('Zrušena AB ' + signal.name, 'TECHNOLOGIE');
    end;
  end;

  Self.step := stepDefault;
  Self.destroyBlock := _JC_DESTROY_NONE;
  Self.destroyEndBlock := _JC_DESTROY_NONE;
end;

procedure TJC.EmergencyCancelActivePath();
begin
  Self.EmergencyStopTrainInVC();
  Self.CancelWithoutTrackRelease(True);
end;

procedure TJC.CancelOrStop();
begin
  if ((Self.signal <> nil) and (Self.signal.typ = TBlkType.btSignal) and (TBlkSignal(Self.signal).DNjc = Self) and
      ((TBlkSignal(Self.signal).IsGoSignal(TJCType.train) or (TBlkSignal(Self.signal).IsGoSignal(TJCType.shunt))) or (TBlkSignal(Self.signal).ZAM) or
      (Self.waitForLastTrackOrRailwayOccupy))) then
  begin
    Self.EmergencyCancelActivePath();
  end else begin
    Self.EmergencyStopTrainInVC();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

// RozpadBlok = blok index, kam by mel vlak vjet
// RozpadRuseniBlok = blok index, kde je posledni detekovany vagon vlaku
procedure TJC.DynamicCancelling();
begin
  var signal: TBlkSignal := TBlkSignal(Self.signal);

  // kontrola obsazenosti useku pred navestidlem
  var signalTrack: TBlkTrack := signal.track as TBlkTrack;
  if ((Self.destroyBlock = _JC_DESTROY_SIGNAL_TRACK) and ((signalTrack.occupied <> TTrackState.Free) or
    (not signalTrack.occupAvailable))) then
  begin
    Self.destroyBlock := 0;
    Self.destroyEndBlock := _JC_DESTROY_SIGNAL_TRACK;
    Self.m_state.RClongTime := True; // obsazeno -> rusime odted vzdy dlouhym casem
  end;

  // uvolneni prvniho useku pred navestidlem v posunove ceste je signalem pro zhasnuti navestidla
  if ((signalTrack.occupAvailable) and (signalTrack.occupied = TTrackState.Free) and (signal.targetSignal <> ncStuj) and
    (Self.destroyEndBlock = _JC_DESTROY_SIGNAL_TRACK) and (Self.typ = TJCType.shunt) and (Self.destroyBlock >= 1)) then
  begin
    Self.Log('Uvolnen usek ' + signalTrack.name + ' : navestidlo ' + signal.name + ' nastaveno na STUJ');
    signal.JCCancelSignal();
  end;

  // destroyBlock = -1 kdyz se kontroluje blok pred navestidlem, -2 pokud je navestidlo na STUJ, nebo zamkle
  for var i: Integer := Max(Self.destroyBlock, 0) to Self.m_data.tracks.Count - 1 do
  begin
    var track: TBlkTrack := TBlkTrack(Blocks.GetBlkByID(Self.m_data.tracks[i]));

    // Obsazeni posledniho useku v posunove ceste nekontrolujeme do momentu, dokud neni na tomto
    // useku destroyBlock. Tzn. pokud je posledni usek PC obsazeny (coz muze byt), nebereme toto
    // v potaz.
    if ((track.occupied = occupied) and
        ((Self.typ <> TJCType.shunt) or (i < Self.m_data.tracks.Count - 1) or (Self.destroyBlock >= Self.m_data.tracks.Count - 1))) then
    begin
      if (i = Self.destroyBlock) then
      begin
        // obsadil se usek, ktery jsme ocekavali
        track.Zaver := TZaver.nouz;

        if (Self.typ = TJCType.Train) then
          Self.MoveTrainToNextTrack();

        // obsazeni useku rusiciho navest (obvykle 0. usek, u skupinoveho navestidla byva jiny)
        // pozor: toto musi byt na tomto miste kvuli nastavovani train.front
        if ((i = Integer(Self.m_data.signalFallTrackI)) and (signal.targetSignal <> ncStuj) and (Self.typ = TJCType.Train)) then
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

        // pokud jsme v predposlednim useku a posledni je nedetekovany, posuneme destroyBlock jeste o jeden usek, aby se cesta mohla zrusit
        if (i = Self.m_data.tracks.Count - 2) then
          if (not Self.lastTrack.occupAvailable) then
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

          // nastavime rychlost vlaku
          if (Self.typ = TJCType.Train) then
            TBlkRT(track).speedUpdate := true;
        end;

      end else begin // if (i = Self.destroyBlock)
        // obsadil se usek, ktery jsme NEocekavali

        if ((track.zaver > TZaver.no) and ((i <> Self.m_data.tracks.Count-1) or (not Self.waitForLastTrackOrRailwayOccupy))) then
        begin
          // pokud jsme na jinem useku, nez RozpadBlok
          Self.CancelOrStop();

          // v trati zaver nerusime, nesmime tam dat ani nouzovy, ani zadny zaver
          if ((i <> Self.m_data.tracks.Count - 1) or (Self.m_data.railwayId = -1)) then
            track.zaver := TZaver.nouz;
        end;
      end;
    end;

    // kontrola zruseni jizdni cesty vlivem vynuzovani bloku
    if ((i = Self.destroyBlock) and (track.Zaver = TZaver.no)) then
    begin
      // pokud usek, na ktery se chystam vkrocit, nema zaver, je neco divne -> zrusit JC (predevsim kvuli predavani loko, ktere by mohlo narusit dalsi JC)
      Self.EmergencyCancelActivePath();
      Exit();
    end;

  end;

  // jizdni cesta konci uvolnenim predposledniho useku

  // mensitko je dulezite a ma smysl !
  // kdyby tam bylo <=, mohl by se rozpadnout jediny usek, na kterem je vlak tim, ze se odobsadi
  if ((Self.destroyEndBlock >= 0) and (Self.destroyEndBlock < Self.destroyBlock - 1)) then
  begin
    // ziskani dotazovaneho useku
    var track: TBlkTrack := TBlkTrack(Blocks.GetBlkByID(Self.m_data.tracks[Self.destroyEndBlock]));

    var nextTrack: TBlkTrack;
    if (Self.destroyEndBlock + 1 < Self.m_data.tracks.Count) then
      nextTrack := Blocks.GetBlkTrackOrRTByID(Self.m_data.tracks[Self.destroyEndBlock + 1])
    else
      nextTrack := nil;

    if ((track.Zaver = TZaver.nouz) and (track.occupied = TTrackState.Free) and
      ((nextTrack = nil) or (nextTrack.occupied = TTrackState.occupied) or (not nextTrack.occupAvailable)))
    then
    begin
      // cesta se rozpada...
      Self.TrackCancelZaver(track);
      Self.destroyEndBlock := Self.destroyEndBlock + 1;

      if ((Self.typ = TJCType.Train) and (track.IsTrain())) then
      begin
        var train := track.Train;
        track.RemoveTrains();
        Self.Log('Smazan vlak ' + train.name + ' z bloku ' + track.name, llInfo);
        if (Self.lastTrack.typ = TBlkType.btRT) then
          train.UpdateRailwaySpeed();
      end;
    end; // if Self.rozpadBlok >= 1
  end; // if (cyklus2 = Self.rozpadRuseniBlok)

  // tady se resi pripad, kdy stanicni kolej zustane obsazena (protoze tam stoji vagony),
  // ale vlak se z ni musi odstranit uvolnenim prvniho useku JC
  if ((Self.destroyEndBlock = _JC_DESTROY_SIGNAL_TRACK) and (Self.destroyBlock > 0)) then
  begin
    var track: TBlkTrack := TBlkTrack(Blocks.GetBlkByID(Self.m_data.tracks[0]));
    var nextTrack: TBlkTrack;

    if (Self.m_data.tracks.Count > 1) then
      nextTrack := Blocks.GetBlkTrackOrRTByID(Self.m_data.tracks[1])
    else
      nextTrack := nil;

    if ((track.Zaver = TZaver.nouz) and (track.occupied = TTrackState.Free) and
      ((nextTrack = nil) or (nextTrack.occupied = TTrackState.occupied) or (not nextTrack.occupAvailable)))
    then
    begin
      // uvolneni prvniho useku v posunove ceste je signalem pro zhasnuti navestidla
      if ((signal.targetSignal <> ncStuj) and (Self.typ = TJCType.shunt)) then
      begin
        Self.Log('Uvolnen usek ' + track.name + ' : navestidlo ' + signal.name + ' nastaveno na STUJ');
        signal.JCCancelSignal();
      end;

      Self.TrackCancelZaver(track as TBlkTrack);
      Self.destroyEndBlock := 1;

      if ((Self.typ = TJCType.Train) and (track.IsTrain())) then
      begin
        // mazani vlaku z useku pred navestidlem
        var train: TTrain := Self.GetTrain(signal, signalTrack);
        if (train = TBlkTrack(track).Train) then
        begin
          Self.Log('Smazan vlak ' + train.name + ' z bloku ' + signalTrack.name, llInfo);
          (signalTrack as TBlkTrack).RemoveTrain(train);
        end;

        Self.Log('Smazan vlak ' + train.name + ' z bloku ' + signalTrack.name, llInfo);
        track.RemoveTrains();
      end;
    end;
  end;

  // mazani vlaku z useku pred navestidlem
  if ((Self.destroyBlock > 0) and (Self.destroyEndBlock = _JC_DESTROY_SIGNAL_TRACK)) then
  begin
    if ((signalTrack.occupied = TTrackState.Free) and (signalTrack.occupAvailable)) then
    begin
      if (signalTrack.IsTrain() and (Self.typ = TJCType.Train)) then
      begin
        var train: TTrain := Self.GetTrain(signal, signalTrack);
        signalTrack.RemoveTrain(train);
        Self.Log('Smazan vlak ' + train.name + ' z bloku ' + signalTrack.name, llInfo);
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

  // Cast podminky za logickym OR je tu pro pripad, kdy JC ma jen jeden usek (to se stava napriklad na smyckach)
  if ((Self.destroyEndBlock = Self.m_data.tracks.Count - 1) and (Self.m_data.tracks.Count > 1)) or
    ((Self.m_data.tracks.Count = 1) and (Self.destroyBlock = 1)) then
  begin
    // vsechny useky az na posledni jsou uvolneny -> rusime JC
    // Zaver posledniho useku rusime vzdy, byt u cest do trati existuje vazba "uvolneni zaveru predposledniho useku rusi zaver posledniho useku".
    // Toto je z toho duvodu, aby i na poslednim useku JC doslo k pocitani casu uvolneni zaveru a to od momentu, kdy se rozpadne jizdni cesta.
    Self.TrackCancelZaver(Self.lastTrack);

    // pokud ma cesta jen jeden usek, odstranime vlak z useku pred navestidlem:
    if (Self.m_data.tracks.Count = 1) then
    begin
      var train: TTrain := Self.GetTrain(signal, signalTrack);
      if ((Self.typ = TJCType.Train) and (train <> nil)) then
      begin
        signalTrack.RemoveTrain(train);
        Self.Log('Smazan vlak ' + train.name + ' z bloku ' + signalTrack.name, llInfo);
      end;

      if ((signalTrack.typ = btRT) and (TBlkRT(signalTrack).railway <> nil) and (TBlkRT(signalTrack).bpInBlk)) then
        TBlkRT(signalTrack).ReleasedFromJC();
    end;

    Self.destroyBlock := _JC_DESTROY_NONE;
    Self.destroyEndBlock := _JC_DESTROY_NONE;
    Self.Log('Ruseni: rozpad cesty vlakem');
    if (signal.DNjc = Self) then
    begin
      // tato situace opravdu muze nastat - posunova cesta s jednim usekem vychazejici z koleje bez indikace volnosti
      if (signal.targetSignal > ncStuj) then
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
    Self.EmergencyCancelActivePath();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TJC.DynamicCancellingNC();
var signalTrack, first: TBlkTrack;
begin
  signalTrack := TBlkRT((Self.signal as TBlkSignal).track);
  first := TBlkTrack(Blocks.GetBlkByID(Self.m_data.tracks[0]));

  if ((first.occupied = TTrackState.occupied) and (signalTrack.occupied = TTrackState.Free) and
    (not signalTrack.IsTrain())) then
  begin
    if ((signalTrack.typ = btRT) and (TBlkRT(signalTrack).railway <> nil) and (TBlkRT(signalTrack).bpInBlk)) then
      TBlkRT(signalTrack).ReleasedFromJC();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

// preda vlak v jizdni ceste dalsimu bloku v poradi
procedure TJC.MoveTrainToNextTrack();
var trackActual, trackNext: TBlkTrack;
  train: TTrain;
begin
  if (Self.destroyBlock = 0) then
  begin
    trackActual := TBlkTrack(TBlkSignal(Self.signal).track);
    Train := Self.GetTrain(Self.signal, trackActual);
    if ((trackActual as TBlkTrack).IsTrain()) then
      if (Train.front <> trackActual) then
        Exit();
  end else begin
    trackActual := Blocks.GetBlkTrackOrRTByID(Self.m_data.tracks[Self.destroyBlock - 1]);
    train := TBlkTrack(trackActual).Train;
  end;

  trackNext := Blocks.GetBlkTrackOrRTByID(Self.m_data.tracks[Self.destroyBlock]);
  if (not trackActual.IsTrain()) then
    Exit();

  trackNext.slowingReady := true;
  trackNext.AddTrainL(Train);
  trackNext.Train.front := trackNext;
  trackNext.houkEvEnabled := true;
  Self.Log('Predan vlak ' + trackNext.Train.name + ' z bloku ' + trackActual.name + ' do bloku ' + trackNext.name, llInfo);

  Self.CheckLoopBlock(trackNext);
end;

procedure TJC.CheckLoopBlock(blk: TBlk);
begin
  var track: TBlkTrack := (blk as TBlkTrack);

  if ((Self.m_data.loopTrackI > -1) and (Self.m_data.loopTrackI < Self.m_data.tracks.Count) and
      (track.id = Self.m_data.tracks[Self.m_data.loopTrackI]) and (track.IsTrain())) then
  begin
    // prohodit vychozi a cilovou stanici
    for var area: TArea in blk.areas do
    begin
      if (area = track.Train.areaTo) then
      begin
        track.Train.InterChangeArea(false);
        break;
      end;
    end;

    track.Train.ChangeDirection();
    Self.Log('Obsazen smyckovy usek ' + blk.name + ' - menim smer loko ve vlaku ' + track.Train.name, llInfo);
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
          var nextSignal: TBlkSignal := Blocks.GetBlkSignalByID(Self.m_data.nextSignalId);

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

  Self.LogStep('Návěstidlo '+Self.signal.name+': nastavuji návěst '+TBlkSignal.SignalToString(code)+' ...');
  (Self.signal as TBlkSignal).SetSignal(code, TNotifyEvent(nil), Self.SignalError);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TJC.LoadData(ini: TMemIniFile; section: string);
begin
  Self.m_data.name := ini.ReadString(section, 'nazev', section);
  Self.m_data.id := StrToInt(section);
  Self.m_data.signalId := ini.ReadInteger(section, 'nav', -1);
  Self.m_data.typ := TJCType(ini.ReadInteger(section, 'typ', -1));
  Self.m_data.emOnly := ini.ReadBool(section, 'pouzeNouzova', False);
  Self.m_data.nextSignalType := TJCNextSignalType(ini.ReadInteger(section, 'dalsiNTyp', 0));
  Self.m_data.nextSignalId := ini.ReadInteger(section, 'dalsiN', 0);
  Self.m_data.railwayId := ini.ReadInteger(section, 'trat', -1);
  Self.m_data.railwayDir := TRailwayDirection(ini.ReadInteger(section, 'tratSmer', 0));

  if (Self.m_data.typ = TJCType.shunt) then
   Self.m_data.signalCode := ini.ReadInteger(section, 'navest', Integer(ncPosunZaj))
  else
   Self.m_data.signalCode := Integer(ncDisabled);

  try
    Self.m_data.speedsGo := TTrainSpeed.IniLoad(ini.ReadString(section, 'rychDalsiN', ''));
    Self.m_data.speedsStop := TTrainSpeed.IniLoad(ini.ReadString(section, 'rychNoDalsiN', ''));
  except
    on E:Exception do
    begin
      Self.m_data.speedsGo.Clear();
      Self.m_data.speedsStop.Clear();
      Self.Log('Nelze nacist rychlosti: '+E.Message, llError, lsData);
    end;
  end;

  var sl: TStrings := TStringList.Create();
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
        turnoutZav.block := StrToInt(sl[i * sect_size]);
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
        refugeeZav.block := StrToInt(sl[i * sect_size]);
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
          refZav.block := StrToInt(lockStrs[0]);
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
  Self.m_data.loopTrackI := ini.ReadInteger(section, 'smycUsek', -1);

  begin
    Self.m_data.permNotes.Clear();
    var str: string := ini.ReadString(section, 'stit0', '');
    var i: Integer := 0;
    while (str <> '') do
    begin
      Self.m_data.permNotes.Add(str);
      Inc(i);
      str := ini.ReadString(section, 'stit'+IntToStr(i), '');
    end;
  end;
end;

procedure TJC.SaveData(ini: TMemIniFile; section: string);
begin
  ini.WriteString(section, 'nazev', Self.m_data.name);
  ini.WriteInteger(section, 'nav', Self.m_data.signalId);
  ini.WriteInteger(section, 'typ', Integer(Self.m_data.typ));
  if (Self.m_data.emOnly) then
    ini.WriteBool(section, 'pouzeNouzova', True);
  if (Self.m_data.nextSignalType <> TJCNextSignalType.no) then
    ini.WriteInteger(section, 'dalsiNTyp', Integer(Self.m_data.nextSignalType));
  if (Self.m_data.nextSignalType = TJCNextSignalType.signal) then
    ini.WriteInteger(section, 'dalsiN', Self.m_data.nextSignalId);

  var speedsGo: string := TTrainSpeed.IniStr(Self.m_data.speedsGo);
  if (speedsGo <> '') then
    ini.WriteString(section, 'rychDalsiN', speedsGo);

  var speedsStop: string := TTrainSpeed.IniStr(Self.m_data.speedsStop);
  if (speedsStop <> '') then
    ini.WriteString(section, 'rychNoDalsiN', speedsStop);

  if ((Self.m_data.typ = TJCType.shunt) and (Self.m_data.signalCode <> Integer(ncPosunZaj))) then
    ini.WriteInteger(section, 'navest', Integer(Self.m_data.signalCode));

  if (Self.m_data.turn <> Self.IsAnyTurnoutMinus) then
    ini.WriteBool(section, 'odbocka', Self.m_data.turn);

  if (Self.m_data.nzv) then
    ini.WriteBool(section, 'nzv', true);

  if (Self.m_data.signalFallTrackI > 0) then
    ini.WriteInteger(section, 'rusNavestUsek', Self.m_data.signalFallTrackI);

  if (Self.m_data.loopTrackI > -1) then
    ini.WriteInteger(section, 'smycUsek', Self.m_data.loopTrackI);

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
  for var turnoutZav: TJCTurnoutZav in Self.m_data.turnouts do
    turnoutsStr := turnoutsStr + '(' + IntToStr(turnoutZav.block) + ',' + IntToStr(Integer(turnoutZav.position)) + ')';
  if (turnoutsStr <> '') then
    ini.WriteString(section, 'vyhybky', turnoutsStr);

  // refugees
  var refugeesStr: string := '';
  for var refugeeZav: TJCRefugeeZav in Self.m_data.refuges do
    refugeesStr := refugeesStr + '(' + IntToStr(refugeeZav.block) + ',' +
      IntToStr(Integer(refugeeZav.position)) + ',' + IntToStr(refugeeZav.ref_blk) + ')';
  if (refugeesStr <> '') then
    ini.WriteString(section, 'odvraty', refugeesStr);

  // crossings
  var crossingsStr: string := '';
  for var crossingZav: TJCCrossingZav in Self.m_data.crossings do
  begin
    crossingsStr := crossingsStr + '(' + IntToStr(crossingZav.crossingId);
    if (crossingZav.openTrack > -1) then
      crossingsStr := crossingsStr + ',' + IntToStr(crossingZav.openTrack) + ',';

    if (crossingZav.closeTracks.Count > 0) then
      for var closeTrackId: Integer in crossingZav.closeTracks do
        crossingsStr := crossingsStr + IntToStr(closeTrackId) + ',';

    if (crossingsStr[Length(crossingsStr)] = ',') then
      crossingsStr[Length(crossingsStr)] := ')'
    else
      crossingsStr := crossingsStr + ')';
  end;
  if (crossingsStr <> '') then
    ini.WriteString(section, 'prj', crossingsStr);

  // locks
  var locksStr: string := '';
  for var lockZav: TJCRefZav in Self.m_data.locks do
    locksStr := locksStr + '(' + IntToStr(lockZav.block) + ';' + IntToStr(lockZav.ref_blk) + ')';
  if (locksStr <> '') then
    ini.WriteString(section, 'podm-zamky', locksStr);

  // Variant points
  var vpsStr: string := SerializeIntList(Self.m_data.vb);
  if (vpsStr <> '') then
    ini.WriteString(section, 'vb', vpsStr);

  for var i: Integer := 0 to Self.m_data.permNotes.Count-1 do
    ini.WriteString(section, 'stit'+IntToStr(i), Self.m_data.permNotes[i]);
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
            PanelServer.CSClose(Self.m_state.senderPnl, 'Timeout');
        end;
      stepJcWaitCross:
        begin
          // prejezd(y) neuzavren
          for var crossingZav: TJCCrossingZav in Self.m_data.crossings do
          begin
            var crossing := TBlkCrossing(Blocks.GetBlkByID(crossingZav.crossingId));
            if (not crossing.safelyClosed) then
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
    Self.CancelActivating('Překročení času stavění JC');
  end; // if timeout
end;

/// /////////////////////////////////////////////////////////////////////////////

function TJC.IsActivating(): Boolean;
begin
  Result := ((Self.step > stepDefault) and (Self.step <> stepJcLastTrackWait));
end;

function TJC.IsActive(): Boolean;
begin
  Result := (Self.m_state.destroyBlock > _JC_DESTROY_NONE);
end;

function TJC.IsNCActive(): Boolean;
begin
  Result := (Self.m_state.destroyBlock = _JC_DESTROY_NC);
end;

function TJC.IsCancelling(): Boolean;
begin
  Result := (Self.m_state.RCtimer > -1);
end;

/// /////////////////////////////////////////////////////////////////////////////

// true = je mozno DN
// tato funkce kontroluje, jestli je mozne znovupostavit cestu i kdyz byla fakticky zrusena = musi zkontrolovat vsechny podminky
function TJC.CanDN(): Boolean;
var train: TTrain;
begin
  // index vlaku na useku pred navestidlem
  train := Self.GetTrain();

  // zkontrolujeme zavery bloku
  // JC NELZE obnovit, pokud na nekterych usecich uz vubec neni zaver
  // mohlo totiz dojit napriklad k uvolneni zaveru odvratu
  for var i: Integer := 0 to Self.m_data.tracks.Count - 1 do
  begin
    var trackZaver: Integer := Self.m_data.tracks[i];
    var track: TBlkTrack := Blocks.GetBlkTrackOrRTByID(trackZaver);
    if ((track.Zaver = TZaver.no) or (track.Zaver = TZaver.staveni) or (track.NUZ) or
      ((track.occupied <> TTrackState.Free) and ((Self.typ = TJCType.Train) or (i <> Self.m_data.tracks.Count - 1))))
    then
      Exit(false);

    // na usecich v ceste je dovoleno mit vlak pred navestidlem, v takovem
    // pripade ji DN z useku v ceste smaze

    if (Self.typ = TJCType.Train) then
    begin
      if (Train = nil) then
      begin
        // pred navestidlem neni vlak -> na usecich nesmi byt zadny vlak
        if (track.IsTrain()) then
          Exit(false);
      end else begin
        // pred navestidlem je vlak -> na usecich smi byt jen stejny vlak
        // jako pred navestidlem
        if ((track.IsTrain()) and ((track.trains.Count > 1) or (track.Train <> Train))) then
          Exit(false);
      end;
    end;
  end; // for i

  // zkontrolujeme polohu vyhybek
  for var turnoutZav: TJCTurnoutZav in Self.m_data.turnouts do
  begin
    var turnout: TBlkTurnout := Blocks.GetBlkTurnoutByID(turnoutZav.block);
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
    var turnout: TBlkTurnout := Blocks.GetBlkTurnoutByID(refugeeZav.block);
    if (turnout.position <> refugeeZav.position) then
      Exit(false);
  end;

  // zkontrolujeme poruchy prejezdu
  // prejezdy, na kterych je zaver, by taky mely byt uzavrene
  for var crossingZav: TJCCrossingZav in Self.m_data.crossings do
  begin
    var crossing: TBlkCrossing := Blocks.GetBlkCrossingByID(crossingZav.crossingId);
    if ((crossing.state = TBlkCrossingBasicState.error) or (crossing.state = TBlkCrossingBasicState.disabled)) then
      Exit(false);
    if ((crossing.Zaver) and (not crossing.safelyClosed)) then
      Exit(false);
  end; // for i

  // zkontrolujeme trat
  if (Self.m_data.railwayId > -1) then
  begin
    var railway: TBlkRailway := Blocks.GetBlkRailwayByID(Self.m_data.railwayId);
    if (railway.request) then
      Exit(false);
    if ((((not(TBlkRT(Self.lastTrack).sectReady)) or (railway.departureForbidden)) and (Self.typ = TJCType.Train)) or
      (railway.RBPCan) or (railway.direction <> Self.m_data.railwayDir)) then
      Exit(false);
  end;

  // kontrola uzamceni zamku:
  for var refZav: TJCRefZav in Self.m_data.locks do
  begin
    var lock: TBlkLock := Blocks.GetBlkLockByID(refZav.block);

    // kontrola uzamceni
    if (lock.keyReleased) then
      Exit(false);
  end;

  Result := true;
end;

// DN provede zbytek staveni JC (prejezdy, finalizace)
// tato procedura predpoklada, ze podminky pro DN jsou splneny
procedure TJC.DN(senderPnl: TIdContext; senderOR: TObject);
begin
  Self.Log('DN');
  if (senderOR <> nil) then
    Self.m_state.senderOR := senderOR;
  if (senderPnl <> nil) then
    Self.m_state.senderPnl := senderPnl;
  Self.m_state.timeOut := Now + EncodeTimeSec(_JC_TIMEOUT_SEC);

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
    var crossing: TBlkCrossing := Blocks.GetBlkCrossingByID(Self.m_data.crossings[data].crossingId);
    crossing.Zaver := true;
    Self.Log('Obsazen ' + TBlkTrack(Sender).name + ' - uzaviram prejezd ' + crossing.name);

    // prejezd se uzavira -> po uvolneni zaveru bloku pod prejezdem prejezd opet otevrit
    var track: TBlkTrack := Blocks.GetBlkTrackOrRTByID(Self.m_data.crossings[data].openTrack);
    track.AddChangeEvent(track.eventsOnZaverReleaseOrAB, CreateChangeEventInt(ceCaller.CrossingCancelZaver,
      Self.m_data.crossings[data].crossingId));
  end;

  for var blkId: Integer in Self.m_data.crossings[data].closeTracks do
  begin
    var track: TBlkTrack := Blocks.GetBlkTrackOrRTByID(blkId);
    track.RemoveChangeEvent(track.eventsOnOccupy, CreateChangeEventInt(Self.TrackCloseCrossing, data));
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
  Self.CancelActivating('', False);
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
      var turnout: TBlkTurnout := Blocks.GetBlkTurnoutByID(Self.m_data.turnouts[i].block);
      if (turnout.position <> TTurnoutPosition(Self.m_data.turnouts[i].position)) then
      begin
        Self.m_state.nextTurnout := i + 1;
        turnout.SetPosition(TTurnoutPosition(Self.m_data.turnouts[i].position), true, false,
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
      var refugee: TBlkTurnout := Blocks.GetBlkTurnoutByID(Self.m_data.refuges[i].block);
      if (refugee.position <> TTurnoutPosition(Self.m_data.refuges[i].position)) then
      begin
        refugee.IntentionalLock();

        var track: TBlkTrack := Blocks.GetBlkTrackOrRTByID(Self.m_data.refuges[i].ref_blk);
        track.AddChangeEvent(track.eventsOnZaverReleaseOrAB, CreateChangeEventInt(ceCaller.TurnoutUnlock,
          Self.m_data.refuges[i].block));

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
  Self.CancelActivating();
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

    var nextTurnout := TBlkTurnout(Blocks.GetBlkByID(Self.m_data.turnouts[Self.m_state.nextTurnout].block));
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
    var refugee := TBlkTurnout(Blocks.GetBlkByID(Self.m_data.refuges[refugeeId].block));
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
  Self.CancelActivating();
end;

/// /////////////////////////////////////////////////////////////////////////////
// generuje podminky branici postaveni nouzove posunove ceste
// tyto podminky jsou prubezne zobrazovany dispecerovi v potvrzovaci sekvenci

procedure TJC.BarriersNCToAccept(var bariery: TJCBarriers);
begin
  // signal
  var signal := Blocks.GetBlkByID(Self.m_data.signalId);
  if (not(signal as TBlkSignal).enabled) then
    bariery.Add(TJCBarBlockDisabled.Create(signal));

  // tracks
  for var i := 0 to Self.m_data.tracks.Count - 1 do
  begin
    var trackID := Self.m_data.tracks[i];
    var track := TBlkTrack(Blocks.GetBlkByID(trackID));
    var glob := track.GetGlobalSettings();

    if (track.occupied = TTrackState.disabled) then
      bariery.Add(TJCBarBlockDisabled.Create(track))

    else if ((i <> Self.m_data.tracks.Count - 1) or (Self.typ <> TJCType.shunt)) then
    begin
      if (track.occupied <> TTrackState.Free) then
        bariery.Add(TJCBarTrackOccupied.Create(track));
    end;

    if ((track.IsTrain()) and (Self.typ = TJCType.Train)) then
      bariery.Add(TJCBarTrackTrain.Create(track));
  end;

  // turnouts
  for var turnoutZav in Self.m_data.turnouts do
  begin
    var turnout: TBlkTurnout := TBlkTurnout(Blocks.GetBlkByID(turnoutZav.block));
    var glob := turnout.GetGlobalSettings();

    if (turnout.position <> turnoutZav.position) then
      bariery.Add(TJCBarTurnoutWrongPos.Create(turnout));

    if (not turnout.emLock) then
      bariery.Add(TJCBarTurnoutEmLock.Create(turnout));

    var coupling := TBlkTurnout(Blocks.GetBlkByID(turnout.GetSettings.coupling));
    if ((coupling <> nil) and (turnout.position <> turnoutZav.position)) then
    begin
      if (not coupling.emLock) then
        bariery.Add(TJCBarTurnoutEmLock.Create(coupling));

      if (coupling.occupied = TTrackState.occupied) then
        bariery.Add(TJCBarTrackOccupied.Create(coupling));
    end;

    if ((turnoutZav.position = TTurnoutPosition.plus) and (turnout.npBlokPlus <> nil)) then
    begin
      if (TBlkTrack(turnout.npBlokPlus).occupied = TTrackState.disabled) then
        bariery.Add(TJCBarBlockDisabled.Create(turnout.npBlokPlus))
      else if (TBlkTrack(turnout.npBlokPlus).occupied <> TTrackState.Free) then
        bariery.Add(TJCBarTrackOccupied.Create(turnout.npBlokPlus));
    end;

    if ((turnoutZav.position = TTurnoutPosition.minus) and (turnout.npBlokMinus <> nil)) then
    begin
      if (TBlkTrack(turnout.npBlokMinus).occupied = TTrackState.disabled) then
        bariery.Add(TJCBarBlockDisabled.Create(turnout.npBlokMinus))
      else if (TBlkTrack(turnout.npBlokMinus).occupied <> TTrackState.Free) then
        bariery.Add(TJCBarTrackOccupied.Create(turnout.npBlokMinus));
    end;
  end;

  // crossings
  for var crossingZav in Self.m_data.crossings do
  begin
    var crossing := TBlkCrossing(Blocks.GetBlkByID(crossingZav.crossingId));

    if (crossing.state <> TBlkCrossingBasicState.error) then
    begin
      if (crossing.pcEmOpen) then
      begin
        bariery.Add(TJCBarCrossingEmergencyOpened.Create(crossing));
      end else begin
        if ((crossingZav.closeTracks.Count > 0) and (not crossing.safelyClosed)) then
          bariery.Add(TJCBarCrossingNotClosed.Create(crossing));
      end;
    end
    else
      bariery.Add(TJCBarCrossingError.Create(crossing));
  end;

  // refugees
  for var refugeeZav in Self.m_data.refuges do
  begin
    var refugee := TBlkTurnout(Blocks.GetBlkByID(refugeeZav.block));

    if (refugee.position <> refugeeZav.position) then
      bariery.Add(TJCBarTurnoutWrongPos.Create(refugee));

    if (not refugee.emLock) then
      bariery.Add(TJCBarTurnoutEmLock.Create(refugee));

    var coupling := TBlkTurnout(Blocks.GetBlkByID(refugee.GetSettings.coupling));
    if (coupling <> nil) then
    begin
      if (refugee.position <> refugeeZav.position) then
        if (not coupling.emLock) then
          bariery.Add(TJCBarTurnoutEmLock.Create(coupling));
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
        bariery.Add(TJCBarRailwayNotReady.Create(railway));
      end;
    end;

    begin
      var railway := TBlkRailway(Blocks.GetBlkByID(Self.m_data.railwayId));

      if ((railway.departureForbidden) and (Self.typ = TJCType.Train)) then
        bariery.Add(TJCBarRailwayZAKVC.Create(railway));
      if ((not railway.departureForbidden) and (Self.typ = TJCType.shunt)) then
        bariery.Add(TJCBarRailwayNoZAK.Create(railway));
      if (railway.Zaver) then
        bariery.Add(TJCBarRailwayZaver.Create(railway));
      if (railway.request) then
        bariery.Add(TJCBarRailwayRequesting.Create(railway));
      if (Self.m_data.railwayDir <> railway.direction) then
        bariery.Add(TJCBarRailwayWrongDir.Create(railway));
      if ((not railway.BP) and (Self.typ = TJCType.Train)) then
        bariery.Add(TJCBarRailwayNoBp.Create(railway));

      var track := TBlkTrack((Self.signal as TBlkSignal).track);

      if ((track.IsTrain) and (Self.lastTrack.typ = btRT) and ((Self.lastTrack as TBlkRT).inRailway = Self.data.railwayId)) then
      begin
        if (railway.lockout) then
        begin
          if ((railway.state.trains.Count > 0) or ((railway.GetLastTrack(Self.data.railwayDir) as TBlkRT).Zaver <>
            TZaver.no)) then
            bariery.Add(TJCBarRailwayNoTrainMove.Create(railway))
          else
            bariery.Add(TJCBarRailwayMoveToEnd.Create(railway));
        end else begin
          if ((Self.lastTrack.IsTrain()) or (not railway.BP) or (railway.direction <> Self.data.railwayDir)) then
            bariery.Add(TJCBarRailwayNoTrainMove.Create(railway));
        end;
      end;
    end;
  end;

  // locks
  for var refZav in Self.m_data.locks do
  begin
    var lock := TBlkLock(Blocks.GetBlkByID(refZav.block));

    if (lock.keyReleased) then
      bariery.Add(TJCBarLockNotLocked.Create(lock));

    if (not lock.emLock) then
      bariery.Add(TJCBarLockEmLock.Create(lock));
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TJC.NC_PS_Callback(Sender: TIdContext; success: Boolean);
begin
  if (success) then
  begin
    if (Self.step = stepNcBarrierUpdate) then
      Self.step := stepNcBarrierConfirmed;
  end else begin
    Self.CancelActivating('', False);

    // aktualizace stavu navestidla (zobrazeni RNZ)
    var blk := Blocks.GetBlkByID(Self.m_data.signalId);
    blk.Change();

    for var trackID in Self.m_data.tracks do
    begin
      var track := Blocks.GetBlkTrackOrRTByID(trackID);
      track.Zaver := TZaver.no;
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TJC.SetData(prop: TJCdata);
begin
  if (Self.m_data.turnouts <> prop.turnouts) then
    Self.m_data.turnouts.Free();
  if (Self.m_data.tracks <> prop.tracks) then
    Self.m_data.tracks.Free();
  if (Self.m_data.refuges <> prop.refuges) then
    Self.m_data.refuges.Free();
  if (Self.m_data.crossings <> prop.crossings) then
    Self.m_data.crossings.Free();
  if (Self.m_data.locks <> prop.locks) then
    Self.m_data.locks.Free();
  if (Self.m_data.vb <> prop.vb) then
    Self.m_data.vb.Free();
  if (Self.m_data.permNotes <> prop.permNotes) then
    Self.m_data.permNotes.Free();
  if (Self.m_data.speedsGo <> prop.speedsGo) then
    Self.m_data.speedsGo.Free();
  if (Self.m_data.speedsStop <> prop.speedsStop) then
    Self.m_data.speedsStop.Free();

  var id_changed := ((Self.id <> prop.id) and (Self.id <> -1));
  var signal_changed := (Self.data.signalId <> prop.signalId);
  var orig_signal := Blocks.GetBlkSignalByID(Self.data.signalId);
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
    signal := Blocks.GetBlkSignalByID(Self.m_data.signalId);

  Result := TBlkSignal(signal).GetTrain(track);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TJC.GetAB(): Boolean;
begin
  var signal := Blocks.GetBlkSignalByID(Self.m_data.signalId);
  Result := ((signal <> nil) and (signal.ABJC = Self));
end;

/// /////////////////////////////////////////////////////////////////////////////

function TJC.IsAnyTurnoutMinus(): Boolean;
begin
  for var turnout: TJCTurnoutZav in Self.m_data.turnouts do
    if (turnout.position = TTurnoutPosition.minus) then
      Exit(true);
  Result := false;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TJC.IsCriticalBarrier(): Boolean;
begin
  Result := False;
  var barriers := TJCBarriers.Create();
  try
    Self.BarriersVCPC(barriers);
    for var barrier in barriers do
      if (barrier.MustPassForDN()) then
        Exit(True);
  finally
    barriers.Free();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TJC.GetSignal(): TBlk;
begin
  Result := Blocks.GetBlkSignalByID(Self.m_data.signalId);
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
    Self.m_state.timeOut := Now + EncodeTimeSec(_JC_TIMEOUT_SEC);
    if ((Self.state.from_stack = nil) and (Self.state.senderPnl <> nil)) then
      TPanelConnData(Self.state.senderPnl.Data).lastActivatedPath := Self;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TJC.GetWaitFroLastTrackOrRailwayOccupied(): Boolean;
begin
  Result := (Self.step = stepJcLastTrackWait);
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
  json['signalFallTrackI'] := Self.m_data.signalFallTrackI;

  for var turnoutZav in Self.m_data.turnouts do
  begin
    var newObj := json.A['turnouts'].AddObject();
    newObj['block'] := turnoutZav.block;
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
    newObj['block'] := refugeeZav.block;
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
    newObj['lock'] := refZav.block;
    newObj['refTrack'] := refZav.ref_blk;
  end;

  for var trackId in Self.m_data.vb do
    json.A['vb'].Add(trackId);

  if (Self.m_data.railwayId <> -1) then
  begin
    json['railway'] := Self.m_data.railwayId;
    json['railwayDir'] := Integer(Self.m_data.railwayDir);
  end;

  TTrainSpeed.GetPtData(Self.m_data.speedsGo, json['speedsGo']);
  TTrainSpeed.GetPtData(Self.m_data.speedsStop, json['speedsStop']);
  json['turn'] := Self.m_data.turn;

  json['loopTrackI'] := Self.m_data.loopTrackI;

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
    PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, 400, 'Návěstidlo není v OŘ');
    Exit();
  end;

  var ab := (reqJson.Contains('ab') and reqJson.B['ab']);

  var barriers := TJCBarriers.Create();
  try
    var wasActivating := Self.activating;
    Self.Activate(nil, TBlkSignal(Self.signal).areas[0], barriers, nil, false, false, ab);
    respJson['success'] := (not wasActivating) and (Self.activating);
    for var barrier in barriers do
      barrier.ToJson(respJson.A['barriers'].AddObject());
  finally
    barriers.Free();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TJC.GetLastTrack(): TBlkTrack;
begin
  if (Self.data.tracks.Count = 0) then
    Exit(nil);
  Result := Blocks.GetBlkTrackOrRTByID(Self.data.tracks[Self.data.tracks.Count - 1]);
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
    raise;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TJC.DetermineCrossingsToClose(var toClose: TList<Boolean>);
begin
  toClose.Clear();

  for var crossingZav: TJCCrossingZav in Self.m_data.crossings do
  begin
    if (crossingZav.openTrack = -1) then
    begin
      toClose.Add(False);
      continue;
    end;

    // prejezd uzavirame jen v pripade, ze nejaky z jeho aktivacnich bloku je obsazen
    // v pripade posunove cesty uzavirame vzdy
    if ((Self.typ = TJCType.shunt) or (crossingZav.closeTracks.Count = 0)) then
    begin
      toClose.Add(True);
    end else begin
      // vlakova cesta:
      var anyOccupied: Boolean := false;
      for var closeTrackId: Integer in crossingZav.closeTracks do
        if (TBlkTrack(Blocks.GetBlkByID(closeTrackId)).occupied = TTrackState.occupied) then
          anyOccupied := true;

      toClose.Add(anyOccupied);
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TJC.ContainsTrackInclRailway(blockid: Integer): Boolean;
begin
  for var _trackId: Integer in Self.data.tracks do
    if (_trackId = blockid) then
      Exit(True);

  // Also tracks in neighbouringRailway
  if (Self.data.railwayId > -1) then
  begin
    var railway: TBlk := Blocks.GetBlkByID(Self.data.railwayId);
    if ((railway <> nil) and (railway.typ = btRailway)) then
    begin
      for var _trackId: Integer in TBlkRailway(railway).GetSettings().trackIds do
      begin
        var railwayTrack: TBlk := Blocks.GetBlkByID(_trackId);
        if ((railwayTrack <> nil) and (railwayTrack.typ = btRT)) then
          if ((TBlkRT(railwayTrack).signalCover = nil) and (railwayTrack.id = blockid)) then
            Exit(True);
      end;
    end;
  end;

  Result := False;
end;

function TJC.ContainsLock(blockid: Integer): Boolean;
begin
  for var lockZav: TJCRefZav in Self.data.locks do
    if (lockZav.block = blockid) then
      Exit(True);
  Result := False;
end;

function TJC.ContainsTurnout(blockid: Integer): Boolean;
begin
  var turnout: TBlkTurnout := TBlkTurnout(Blocks.GetBlkByID(blockid));

  for var turnoutZav: TJCTurnoutZav in Self.data.turnouts do
    if (turnoutZav.block = turnout.id) then
      Exit(True);

  for var refugeeZav: TJCRefugeeZav in Self.data.refuges do
    if (refugeeZav.block = turnout.id) then
      Exit(True);

  if ((turnout <> nil) and (turnout.lock <> nil)) then
    for var refZav: TJCRefZav in Self.data.locks do
      if (refZav.block = turnout.lock.id) then
        Exit(True);

  Result := False;
end;

function TJC.ContainsRailway(blockid: Integer): Boolean;
begin
  Result := (Self.data.railwayId = blockid);
end;

function TJC.ContainsCrossing(blockid: Integer): Boolean;
begin
  for var crossingZav: TJCCrossingZav in Self.data.crossings do
    if (crossingZav.crossingId = blockid) then
      Exit(True);
  Result := False;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TJC.TrackCancelZaver(track: TBlkTrack);
begin
  if (Self.ab) then
    track.SetZaverWithPathTimer(TZaver.ab)
  else
    track.SetZaverWithPathTimer(TZaver.no);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TJC.CancelTimeSec(): Cardinal;
begin
  Result := Max(GlobalConfig.times.rcVcOccupied, GlobalConfig.times.rcPcOccupied); // vychozi hodnota (maximalni cas)

  if (Self.m_state.RClongTime) then
  begin
    case (Self.typ) of
      TJCType.Train: Result := GlobalConfig.times.rcVcOccupied;
      TJCType.shunt: Result := GlobalConfig.times.rcPcOccupied;
    end;
  end else begin
    Result := GlobalConfig.times.rcFree;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////
// When tracks are occupied or zaver is lost during cancelling,
// cancelling is stopped and dnJC := nil (-> NUZ required for further cancelling).

procedure TJC.CheckCancellingTracks();
begin
  var violated: Boolean := False;

  if (Self.m_data.tracks.Count <> Self.m_state.occupyStateWhenCancellingStarted.Count) then
    raise Exception.Create('Self.m_data.tracks.Count <> Self.m_state.occupyStateWhenCancellingStarted.Count');

  for var i: Integer := 0 to Self.m_data.tracks.Count - 1 do
  begin
    var trackId: Integer := Self.m_data.tracks[i];
    var track: TBlkTrack := TBlkTrack(Blocks.GetBlkByID(trackId));

    if ((track <> nil) and ((track.typ = TBlkType.btTrack) or (track.typ = TBlkType.btRT))) then
    begin
      if ((track.occupied = TTrackState.occupied) and (Self.m_state.occupyStateWhenCancellingStarted[i] = TTrackState.free) and
          (not ((Self.typ = TJCType.shunt) and (i = Self.m_data.tracks.Count-1)))) then
        violated := True;
      if (track.zaver = TZaver.no) then
        violated := True;
    end;
  end;

  if (violated) then
  begin
    Self.Log('Obsazen úsek nebo zrušen závěr úseku, zastavuji rušení cesty!');
    Self.StopCancelling();
    if ((Self.signal <> nil) and (Self.signal.typ = TBlkType.btSignal) and (TBlkSignal(Self.signal).dnJC = Self)) then
      TBlkSignal(Self.signal).dnJC := nil;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function ContainsTrackInclRailway(jc: TJC; blockid: Integer): Boolean;
begin
  Result := jc.ContainsTrackInclRailway(blockid);
end;

function ContainsLock(jc: TJC; blockid: Integer): Boolean;
begin
  Result := jc.ContainsLock(blockid);
end;

function ContainsTurnout(jc: TJC; blockid: Integer): Boolean;
begin
  Result := jc.ContainsTurnout(blockid);
end;

function ContainsRailway(jc: TJC; blockid: Integer): Boolean;
begin
  Result := jc.ContainsRailway(blockid);
end;

function ContainsCrossing(jc: TJC; blockid: Integer): Boolean;
begin
  Result := jc.ContainsCrossing(blockid);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TJC.EmergencyStopTrainInVC();
begin
  if ((Self.typ = TJCType.train) and (not Self.m_state.nc)) then
    Self.EmergencyStopTrainInPath();
end;

procedure TJC.EmergencyStopTrainInPath();
begin
  for var trackZav: Integer in Self.m_data.tracks do
  begin
    var track: TBlkTrack := Blocks.GetBlkTrackOrRTByID(trackZav);
    if (track <> nil) then
      Self.EmergencyStopFrontTrainsInTrack(track);
  end;

  if ((Self.signal <> nil) and (Self.signal.typ = TBlkType.btSignal)) then
  begin
    var signalTrack: TBlk := TBlkSignal(Self.signal).track;
    if ((signalTrack.typ = btTrack) or (signalTrack.typ = btRT)) then
      Self.EmergencyStopFrontTrainsInTrack(TBlkTrack(signalTrack));
  end;
end;

procedure TJC.EmergencyStopFrontTrainsInTrack(track: TBlkTrack);
begin
  for var trainI: Integer in track.trains do
  begin
    if ((trains[trainI].speed > 0) and (trains[trainI].front = track)) then
    begin
      trains[trainI].EmergencyStop();
      Self.Log('Narušení JC - nouzově zastaven vlak ' + trains[trainI].name + '!', TLogLevel.llWarning);
      Self.signal.BottomErrorBroadcast('Narušení JC - nouzově zastaven vlak ' + trains[trainI].name + '!', 'TECHNOLOGIE');
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
