unit BlockTrack;

{ TRACK technological block definition. }

interface

uses IniFiles, Block, Menus, AreaDb, SysUtils, Classes, Booster, houkEvent,
  IdContext, Generics.Collections, JsonDataObjects, Area, Train,
  announcement, changeEvent, predvidanyOdjezd, TechnologieRCS;

type
  TTrackState = (disabled = -5, none = -1, free = 0, occupied = 1);

  ETrainFull = class(Exception);
  ETrainNotExists = class(Exception);
  EMultipleTrains = class(Exception);
  EDuplicitTrains = class(Exception);
  ERunningTrain = class(Exception);

  TBlkTrackSettings = record
    RCSAddrs: TRCSAddrs;
    lenght: double; // in meters
    loop: Boolean;
    boosterId: string;
    houkEvL: TObjectList<THoukEv>; // seznam houkacich udalosti pro lichy smer
    houkEvS: TObjectList<THoukEv>; // seznam houkacich udalosti pro sudy smer
    maxTrains: Cardinal;
  end;

  TSectionsState = TList<TTrackState>;

  TBlkTrackState = record
    occupied, occupiedOld: TTrackState;
    sectionsOccupied: TSectionsState;
    zaver: TZaver;
    NUZ: Boolean;
    jcEnd: TZaver;
    note, lockout: string;
    signalJCRef: TList<TBlk>; // navestidla, ze kterych je z tohoto bloku postavena JC
    trainPredict: Integer; // souprava, ktera je na tomto bloku predpovidana

    shortCircuit: TBoosterSignal;
    power: TBoosterSignal;
    DCC: Boolean; // stav DCC na useku: kdyz je kontrola na SPAXu, beru SPAX, jinak se bere stav z centraly

    trainMoving: Integer;
    // index soupravy, ktera se presouva, v ramci lokalniho seznamu souprav na useku; zadny presun = -1

    slowingReady: Boolean;
    // pri predani soupravy do tohoto useku z trati, ci z jizdni cesty, je tento flag nastaven na true
    // to znamena, ze je souprava pripravena ke zpomalovani; navetidlo umozni zpomaleni jen, pokud je tento flag na true
    // po zpomaleni si navestidlo flag zrusi

    // ztrata soupravy na useku se pozna tak, ze blok po urcity cas obsahuje soupravu, ale neni obsazen
    trainLost: Boolean;
    // to je nutne pro predavani souprav mezi bloky v ramci JC (usek se uvolni, ale souprava se jeste nestihne predat
    // pro reseni timeoutu jsou tyto 2 promenne
    trainLostTime: Integer;
    shortCircSenseTime: TDateTime; // cas, kdy ma dojit k detekci zkratu
    currentHoukEv: Integer; // index aktualni houkaci udalosti
    neprofilJCcheck: TList<Integer>; // seznam id jizdnich cest, ktere na usek uvalily podminku neprofiloveho styku
    trains: TList<Integer>; // seznam souprav na useku ve smeru L --> S
    psts: TList<TBlk>;
  end;

  TBlkTrackSpnl = record
    stationTrack: Boolean;
    trackName: string;
    trainPos: Boolean;
  end;

  TBlkTrack = class(TBlk)
  const
    _def_track_state: TBlkTrackState = (occupied: disabled; occupiedOld: disabled; zaver: no; NUZ: false; jcEnd: no;
      note: ''; lockout: ''; trainPredict: - 1; shortCircuit: TBoosterSignal.undef; power: TBoosterSignal.undef;
      DCC: false; trainMoving: - 1; slowingReady: false; trainLost: false; currentHoukEv: - 1;);

    _DEFAULT_MAX_TRAINS = 1;

  private
    m_state: TBlkTrackState;
    m_spnl: TBlkTrackSpnl;
    last_zes_zkrat: TBoosterSignal; // poziva se na pamatovani posledniho stavu zkratu zesilovace pri vypnuti DCC

    procedure SetNUZ(NUZ: Boolean);
    procedure SetZaver(zaver: TZaver);
    procedure SetNote(note: string);
    procedure mSetLockout(lockout: string);
    function GetTrainPredict(): TTrain;
    procedure SetTrainPredict(Train: TTrain);
    procedure SetJCend(jcEnd: TZaver);
    procedure SetTrainMoving(moving: Integer);

    procedure SetPower(state: TBoosterSignal);
    procedure SetShortCircuit(state: TBoosterSignal);
    procedure SetDCC(state: Boolean);

    procedure XTakeTrainOk(Sender: TObject; Data: Pointer);
    procedure XTakeTrainErr(Sender: TObject; Data: Pointer);

    procedure MenuNewTrainClick(SenderPnl: TIdContext; SenderOR: TObject; itemindex: Integer);
    procedure MenuEditTrainClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuInfoTrainClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuDeleteTrainClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuUVOLTrainClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuVEZMITrainClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuXVEZMITrainClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuRegVEZMITrainClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuRUCTrainClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuMAUSTrainClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuSTOPTrainOnClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuSTOPTrainOffClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuJedTrainClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuStitClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuVylClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuNUZStartClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuNUZStopClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuPRESUNTrainClick(SenderPnl: TIdContext; SenderOR: TObject; new_state: Boolean);
    procedure MenuHLASENIOdjezdClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuHLASENIPrijezdClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuHLASENIPrujezdClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuVLOZTrainClick(SenderPnl: TIdContext; SenderOR: TObject; itemindex: Integer);
    procedure MenuPOdjClick(SenderPnl: TIdContext; SenderOR: TObject);

    procedure PotvrDeleteTrain(Sender: TIdContext; success: Boolean);
    procedure PotvrUvolTrain(Sender: TIdContext; success: Boolean);
    procedure PotvrRegVezmiTrain(Sender: TIdContext; success: Boolean);
    procedure PotvrSTOPTrainOff(Sender: TObject);

    procedure MenuObsazClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuUvolClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuDetClick(SenderPnl: TIdContext; SenderOR: TObject; id: Integer; state: Boolean);

    procedure ORVylukaNull(Sender: TIdContext; success: Boolean);

    procedure LoadHoukEventToList(list: TList<THoukEv>; ini_tech: TMemIniFile; section: string; prefix: string);
    procedure CheckHoukEv();

    procedure CheckPOdjChanged();

    function GetHoukList(): TList<THoukEv>;
    function GetHoukEvEnabled(): Boolean;
    procedure SetHoukEvEnabled(state: Boolean);

    function GetAnnouncementTrain(trainLocalIndex: Integer): TAnnTrain;

    procedure NonProfileOccupy();

    function GetTrainI(): Integer;
    function GetTrain(): TTrain;
    function GetTrainIL(): Integer;
    function GetTrainL(): TTrain;
    function GetTrainIS(): Integer;
    function GetTrainS(): TTrain;

    procedure ShowProperMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TAreaRights; params: string);
    function CanTrainSpeedInsert(index: Integer): Boolean;
    function IsStujForTrain(Train: TTrain): Boolean;
    function RealBoosterShortCircuit(): TBoosterSignal;
    function CanBeNextVB(blocks: TList<TBlk>): Boolean;
    function CanBeKC(blocks: TList<TBlk>): Boolean;

    function GetSignalL(): TBlk;
    function GetSignalS(): TBlk;

    procedure PstCheckActive();

  protected
    m_settings: TBlkTrackSettings;

    function MenuVBClick(SenderPnl: TIdContext; SenderOR: TObject): Boolean;
    function MenuKCClick(SenderPnl: TIdContext; SenderOR: TObject): Boolean;
    function MoveTrain(SenderPnl: TIdContext; SenderOR: TObject; trainLocalIndex: Integer): Boolean;

  public

    eventsOnOccupy: TChangeEvents;
    eventsOnFree: TChangeEvents;
    eventsOnZaverReleaseOrAB: TChangeEvents;

    constructor Create(index: Integer);
    destructor Destroy(); override;

    procedure LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile); override;
    procedure SaveData(ini_tech: TMemIniFile; const section: string); override;
    procedure SaveState(ini_stat: TMemIniFile; const section: string); override;

    procedure Enable(); override;
    procedure Disable(); override;
    procedure Reset(); override;
    function UsesRCS(addr: TRCSAddr; portType: TRCSIOType): Boolean; override;

    procedure Update(); override;
    procedure Freeze(); override;
    procedure UnFreeze(); override;

    // ----- Track specific functions -----

    function GetSettings(): TBlkTrackSettings;
    procedure SetSettings(Data: TBlkTrackSettings);

    procedure OnBoosterChange();

    procedure SetLockout(Sender: TIdContext; lockout: string);

    procedure AddNeprofilJC(id: Integer);
    procedure RemoveNeprofilJC(id: Integer);
    function IsNeprofilJC(): Boolean;

    function IsTrain(): Boolean; overload;
    function IsTrain(index: Integer): Boolean; overload;
    function IsTrain(Train: TTrain): Boolean; overload;
    procedure AddTrainL(index: Integer); overload; virtual;
    procedure AddTrainL(Train: TTrain); overload; virtual;
    procedure AddTrainS(index: Integer); overload; virtual;
    procedure AddTrainS(Train: TTrain); overload; virtual;
    procedure AddTrain(localTrainIndex: Integer; Train: Integer); overload;
    procedure AddTrain(localTrainIndex: Integer; Train: TTrain); overload;
    procedure RemoveTrains(); virtual;
    procedure RemoveTrain(index: Integer); overload; virtual;
    procedure RemoveTrain(Train: TTrain); overload; virtual;
    function TrainsFull(): Boolean;
    function CanStandTrain(): Boolean;

    function IsTrainMoving(): Boolean;
    procedure ClearPOdj();
    procedure PropagatePOdjToRailway();

    procedure MenuSOUPRAVA(SenderPnl: TIdContext; SenderOR: TObject; trainLocalI: Integer);

    procedure PstAdd(pst: TBlk);
    procedure PstRemove(pst: TBlk);
    function PstIsActive(): Boolean;
    function PstIs(): Boolean;

    property state: TBlkTrackState read m_state;
    property spnl: TBlkTrackSpnl read m_spnl;

    property occupied: TTrackState read m_state.occupied;
    property NUZ: Boolean read m_state.NUZ write SetNUZ;
    property zaver: TZaver read m_state.zaver write SetZaver;
    property note: string read m_state.note write SetNote;
    property lockout: string read m_state.lockout write mSetLockout;
    property trainPredict: TTrain read GetTrainPredict write SetTrainPredict;
    property jcEnd: TZaver read m_state.jcEnd write SetJCend;
    property signalJCRef: TList<TBlk> read m_state.signalJCRef write m_state.signalJCRef;
    property sectionsState: TList<TTrackState> read m_state.sectionsOccupied;
    property signalL: TBlk read GetSignalL; // warning: slow getter!
    property signalS: TBlk read GetSignalS; // warning: slow getter!

    property trainI: Integer read GetTrainI;
    property train: TTrain read GetTrain;
    property trainIL: Integer read GetTrainIL;
    property trainL: TTrain read GetTrainL;
    property trainIS: Integer read GetTrainIS;
    property trainSudy: TTrain read GetTrainS;
    property trains: TList<Integer> read m_state.trains;

    property shortCircuit: TBoosterSignal read m_state.shortCircuit write SetShortCircuit;
    property power: TBoosterSignal read m_state.power write SetPower;
    property DCC: Boolean read m_state.DCC write SetDCC;

    property trainMoving: Integer read m_state.trainMoving write SetTrainMoving;
    property slowingReady: Boolean read m_state.slowingReady write m_state.slowingReady;
    property houkEvEnabled: Boolean read GetHoukEvEnabled write SetHoukEvEnabled;

    // GUI:

    procedure PanelMenuClick(SenderPnl: TIdContext; SenderOR: TObject; item: string; itemindex: Integer); override;

    function ShowPanelMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TAreaRights): string; override;
    procedure PanelClick(SenderPnl: TIdContext; SenderOR: TObject; Button: TPanelButton; rights: TAreaRights;
      params: string = ''); override;
    procedure POdjChanged(trainId: Integer; var podj: TPOdj);
    function PanelStateString(): string; override;
    function AcceptsMenuClick(SenderPnl: TIdContext; SenderOR: TObject; rights: TAreaRights; item: string): Boolean; override;

    // PT:

    procedure GetPtData(json: TJsonObject; includeState: Boolean); override;
    procedure GetPtState(json: TJsonObject); override;
    procedure PutPtState(reqJson: TJsonObject; respJson: TJsonObject); override;

  end;

  /// /////////////////////////////////////////////////////////////////////////////

implementation

uses GetSystems, BlockDb, BlockSignal, Logging, RCS, ownStrUtils, Diagnostics,
  TJCDatabase, fMain, TCPServerPanel, BlockRailway, TrainDb, THVDatabase, Math,
  Trakce, THnaciVozidlo, BlockRailwayTrack, BoosterDb, appEv, StrUtils, UPO,
  announcementHelper, TechnologieJC, PTUtils, RegulatorTCP, TCPAreasRef, ConfSeq,
  Graphics, ownConvert, TechnologieTrakce, TMultiJCDatabase, BlockPst, IfThenElse;

constructor TBlkTrack.Create(index: Integer);
begin
  inherited Create(index);

  Self.m_globSettings.typ := btTrack;
  Self.m_state := _def_track_state;

  Self.eventsOnOccupy := TChangeEvents.Create();
  Self.eventsOnFree := TChangeEvents.Create();
  Self.eventsOnZaverReleaseOrAB := TChangeEvents.Create();

  Self.m_settings.houkEvL := TObjectList<THoukEv>.Create();
  Self.m_settings.houkEvS := TObjectList<THoukEv>.Create();

  Self.m_settings.maxTrains := _DEFAULT_MAX_TRAINS;

  Self.m_state.neprofilJCcheck := TList<Integer>.Create();
  Self.m_state.trains := TList<Integer>.Create();
  Self.m_state.signalJCRef := TList<TBlk>.Create();
  Self.m_state.sectionsOccupied := TList<TTrackState>.Create();
  Self.m_state.psts := TList<TBlk>.Create();
end;

destructor TBlkTrack.Destroy();
begin
  if (Assigned(Self.m_settings.houkEvL)) then
    Self.m_settings.houkEvL.free();

  if (Assigned(Self.m_settings.houkEvS)) then
    Self.m_settings.houkEvS.free();

  Self.eventsOnOccupy.free();
  Self.eventsOnFree.free();
  Self.eventsOnZaverReleaseOrAB.free();

  Self.m_state.neprofilJCcheck.free();
  Self.m_state.trains.free();
  Self.m_state.signalJCRef.free();
  Self.m_state.sectionsOccupied.free();
  Self.m_state.psts.Free();

  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkTrack.LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile);
begin
  inherited LoadData(ini_tech, section, ini_rel, ini_stat);

  Self.m_settings.RCSAddrs := Self.LoadRCS(ini_tech, section);
  Self.m_settings.lenght := ini_tech.ReadFloat(section, 'delka', 0);
  Self.m_settings.boosterId := ini_tech.ReadString(section, 'zesil', '');
  Self.m_settings.loop := ini_tech.ReadBool(section, 'smc', false);
  Self.m_settings.maxTrains := ini_tech.ReadInteger(section, 'maxSpr', _DEFAULT_MAX_TRAINS);

  if (Boosters[Self.m_settings.boosterId] = nil) then
    Log('Blok ' + Self.name + ' (' + IntToStr(Self.id) + ') nemá návaznost na validní zesilovač',
      llWarning, lsData);

  Self.m_state.note := ini_stat.ReadString(section, 'stit', '');
  Self.m_state.lockout := ini_stat.ReadString(section, 'vyl', '');

  var strs: TStrings := TStringList.Create();
  try
    Self.m_state.trains.Clear();
    ExtractStringsEx([','], [], ini_stat.ReadString(section, 'spr', ''), strs);
    for var s: string in strs do
    begin
      var trainIndex: Integer := TrainDb.trains.GetTrainIndexByName(s);
      if (trainIndex > -1) then
        Self.m_state.trains.Add(trainIndex)
      else
        Log('Souprava ' + s + ' na bloku ' + Self.name + ' neexistuje, mažu soupravu', llWarning, lsData);
    end;

    // houkaci udalosti
    try
      Self.LoadHoukEventToList(Self.m_settings.houkEvL, ini_tech, section, 'houkL');
    except
      Log('Nepodařilo se načíst houkací události L bloku ' + Self.name, llWarning, lsData);
    end;

    try
      Self.LoadHoukEventToList(Self.m_settings.houkEvS, ini_tech, section, 'houkS');
    except
      Log('Nepodařilo se načíst houkací události S bloku ' + Self.name, llWarning, lsData);
    end;
  finally
    strs.free();
  end;

  strs := Self.LoadAreas(ini_rel, 'U');
  try
    if (strs.Count >= 2) then
    begin
      Self.m_spnl.stationTrack := (strs[1] = '1');
      if (strs.Count >= 3) then
        Self.m_spnl.trackName := strs[2]
      else
        Self.m_spnl.trackName := '';
    end;

    if (strs.Count >= 4) then
      Self.m_spnl.trainPos := (strs[3] = '1');

    if ((not Self.m_spnl.stationTrack) and (Self.m_settings.maxTrains <> 1)) then
      Self.m_settings.maxTrains := 1;
  finally
    strs.free();
  end;

  Self.RCSRegister(Self.m_settings.RCSAddrs);
end;

procedure TBlkTrack.SaveData(ini_tech: TMemIniFile; const section: string);
begin
  inherited SaveData(ini_tech, section);

  Self.SaveRCS(ini_tech, section, Self.m_settings.RCSAddrs);
  ini_tech.WriteFloat(section, 'delka', Self.m_settings.lenght);
  ini_tech.WriteString(section, 'zesil', Self.m_settings.boosterId);

  if (Self.m_settings.maxTrains <> _DEFAULT_MAX_TRAINS) then
    ini_tech.WriteInteger(section, 'maxSpr', Self.m_settings.maxTrains);

  if (Self.m_settings.loop) then
    ini_tech.WriteBool(section, 'smc', Self.m_settings.loop);

  if (Assigned(Self.m_settings.houkEvL)) then
  begin
    for var i: Integer := 0 to Self.m_settings.houkEvL.Count - 1 do
    begin
      try
        ini_tech.WriteString(section, 'houkL' + IntToStr(i), Self.m_settings.houkEvL[i].GetDefString());
      except
        on E: Exception do
          AppEvents.LogException(E, 'Ukladani houkaci udalosti bloku ' + Self.name);
      end;
    end;
  end;

  if (Assigned(Self.m_settings.houkEvS)) then
  begin
    for var i: Integer := 0 to Self.m_settings.houkEvS.Count - 1 do
    begin
      try
        ini_tech.WriteString(section, 'houkS' + IntToStr(i), Self.m_settings.houkEvS[i].GetDefString());
      except
        on E: Exception do
          AppEvents.LogException(E, 'Ukladani houkaci udalosti bloku ' + Self.name);
      end;
    end;
  end;
end;

procedure TBlkTrack.SaveState(ini_stat: TMemIniFile; const section: string);
begin
  if (Self.m_state.note <> '') then
    ini_stat.WriteString(section, 'stit', Self.m_state.note);

  if (Self.m_state.lockout <> '') then
    ini_stat.WriteString(section, 'vyl', Self.m_state.lockout);

  if (Self.IsTrain()) then
  begin
    var str: string := '';
    for var trainI: Integer in Self.trains do
      str := str + TrainDb.trains[trainI].name + ',';
    ini_stat.WriteString(section, 'spr', str);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkTrack.Enable();
var enable: Boolean;
begin
  enable := true;
  try
    for var rcsaddr: TRCSAddr in Self.m_settings.RCSAddrs do
      if (not RCSi.IsNonFailedModule(rcsaddr.board)) then
        enable := false;
  except
    enable := false;
  end;

  if (enable) then
  begin
    Self.m_state.occupied := none;
    Self.m_state.occupiedOld := none;
  end;

  Self.OnBoosterChange();
  Self.m_state.sectionsOccupied.Clear();
  Self.m_state.neprofilJCcheck.Clear();
  Self.m_state.psts.Clear();

  Self.Update();
  // change event will be called in Update();
end;

procedure TBlkTrack.Disable();
begin
  inherited;

  if (Self.m_state.shortCircuit = TBoosterSignal.error) then
    for var area: TArea in Self.areas do
      Area.shortCircBlkCnt := Area.shortCircBlkCnt - 1;

  Self.m_state.occupied := disabled;
  Self.m_state.occupiedOld := disabled;
  Self.m_state.NUZ := false;
  Self.m_state.trainPredict := -1;
  Self.m_state.jcEnd := TZaver.no;
  Self.m_state.slowingReady := false;
  Self.houkEvEnabled := false;
  Self.m_state.shortCircuit := TBoosterSignal.undef;
  Self.m_state.power := TBoosterSignal.undef;
  Self.m_state.DCC := false;
  Self.m_state.sectionsOccupied.Clear();
  Self.m_state.neprofilJCcheck.Clear();
  Self.m_state.psts.Clear();

  Self.Change(true);
end;

procedure TBlkTrack.Reset();
begin
  Self.eventsOnOccupy.Clear();
  Self.eventsOnFree.Clear();
  Self.eventsOnZaverReleaseOrAB.Clear();
  Self.zaver := TZaver.no;
  Self.m_state.psts.Clear();
end;

function TBlkTrack.UsesRCS(addr: TRCSAddr; portType: TRCSIOType): Boolean;
begin
  Result := ((portType = TRCSIOType.input) and (Self.m_settings.RCSAddrs.Contains(addr)));
end;

/// /////////////////////////////////////////////////////////////////////////////

// update all local variables
procedure TBlkTrack.Update();
begin
  inherited Update();

  if (((Self.shortCircuit = TBoosterSignal.error) or (Self.power = TBoosterSignal.error)) and (not Self.frozen)) then
  begin
    Self.Freeze();
    Exit();
  end;

  if (Self.frozen) then
  begin
    if ((Self.shortCircuit <> TBoosterSignal.error) and (Self.RealBoosterShortCircuit() = TBoosterSignal.error) and
      (Self.power = TBoosterSignal.ok) and (Now > Self.state.shortCircSenseTime) and (Self.DCC)) then
      Self.shortCircuit := TBoosterSignal.error;

    if (((not Self.DCC) or (Self.power = TBoosterSignal.error) or (Now < Self.state.shortCircSenseTime)) and
      (Self.shortCircuit = TBoosterSignal.error)) then
      Self.shortCircuit := TBoosterSignal.ok;

    if ((Self.DCC) and (Self.shortCircuit <> TBoosterSignal.error) and (Self.power <> TBoosterSignal.error) and
      (Now > Self.state.shortCircSenseTime)) then
      Self.UnFreeze();
  end;

  if (Self.m_state.sectionsOccupied.Count <> Self.m_settings.RCSAddrs.Count) then
  begin
    Self.m_state.sectionsOccupied.Clear();
    for var i: Integer := 0 to Self.m_settings.RCSAddrs.Count - 1 do
      Self.m_state.sectionsOccupied.Add(TTrackState.none);
  end;

  if (not Self.frozen) then
    Self.m_state.occupied := TTrackState.free; // must be here to update booster state

  for var i: Integer := 0 to Self.m_settings.RCSAddrs.Count - 1 do
  begin
    var state: TRCSInputState;
    try
      state := RCSi.GetInput(Self.m_settings.RCSAddrs[i]);
    except
      state := failure;
    end;

    case (state) of
      isOn:
        if (not Self.frozen) then
          Self.m_state.sectionsOccupied[i] := TTrackState.occupied;
      isOff:
        if ((not Self.frozen) or (Self.m_state.sectionsOccupied[i] = TTrackState.disabled)) then
          Self.m_state.sectionsOccupied[i] := TTrackState.free;
      failure, notYetScanned, unavailableModule, unavailablePort:
        begin
          Self.m_state.sectionsOccupied[i] := TTrackState.disabled;
          Self.m_state.occupied := TTrackState.disabled;
        end;
    end;
  end;

  if (Self.m_state.occupied <> TTrackState.disabled) then
    for var trackState: TTrackState in Self.m_state.sectionsOccupied do
      if (trackState = TTrackState.occupied) then
        Self.m_state.occupied := TTrackState.occupied;

  // reseni vypadku soupravy
  // pad soupravy z bloku az po urcitem case - aby se jizdni ceste nechal cas na zpracovani pohybu soupravy
  if (Self.m_state.trainLost) then
  begin
    if (not Self.IsTrain()) then
    begin
      Self.m_state.trainLost := false;
      Exit();
    end;

    Inc(Self.m_state.trainLostTime);
    if (Self.m_state.trainLostTime > 3) then
    begin
      Self.m_state.trainLost := false;

      // informace o vypadku soupravy probiha jen ve stanicnich kolejich a v trati
      if ((Self.typ = btRT) or (Self.spnl.stationTrack)) then
        Self.BottomErrorBroadcast('Ztráta soupravy v úseku ' + Self.name, 'TECHNOLOGIE');
      if (Self.m_state.zaver <> TZaver.no) then
        Self.m_state.zaver := TZaver.nouz;
    end; // if train_vypadek_time > 3
  end; // if train_vypadek

  // OnChange
  if (Self.m_state.occupied <> Self.m_state.occupiedOld) then
  begin
    if (Self.m_state.occupied = TTrackState.disabled) then
    begin
      Self.m_state.occupied := disabled;
      Self.m_state.occupiedOld := Self.m_state.occupied;
      JCDb.Cancel(Self);

      // zastavime soupravy na useku
      for var train: Integer in Self.trains do
        TrainDb.trains[train].speed := 0;

      Self.Change(true);
    end;

    if (Self.m_state.occupiedOld = TTrackState.disabled) then
    begin
      // Wake-up from disabled
      Self.OnBoosterChange();
    end;

    // kontrola udalosti obsazeni
    if (Self.m_state.occupied = TTrackState.occupied) then
    begin
      Self.NonProfileOccupy();
      Self.CallChangeEvents(Self.eventsOnOccupy);
    end else if (Self.m_state.occupied = TTrackState.free) then
      Self.CallChangeEvents(Self.eventsOnFree);

    if (Self.IsTrain()) then
    begin
      // souprava
      if ((Self.m_state.occupied = TTrackState.free) and (Self.m_state.occupiedOld = TTrackState.occupied)) then
      begin
        Self.m_state.trainLost := true;
        Self.m_state.trainLostTime := 0;
      end;
    end; // if Train <> -1

    Self.m_state.occupiedOld := Self.m_state.occupied;
    Self.Change();
  end;

  // reseni zruseni PRESUN soupravy, ktera jede
  if ((Self.IsTrainMoving()) and ((not Self.IsTrain(Self.trains[Self.trainMoving])) or
    ((TrainDb.trains[Self.trains[Self.trainMoving]].wantedSpeed > 0) and (not TrainDb.trains[Self.trains[Self.trainMoving]].emergencyStopped)))) then
    Self.trainMoving := -1;

  // pousteni houkani na houkaci udalosti
  if (Self.m_state.currentHoukEv > -1) then
    Self.CheckHoukEv();

  // kontrola zmeny barev vlivem uplynuti casu predvidaneho odjezdu
  Self.CheckPOdjChanged();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkTrack.SetNUZ(NUZ: Boolean);
begin
  if (Self.m_state.NUZ = NUZ) then
    Exit();

  if (Self.m_state.NUZ) and (not NUZ) then
  begin
    for var area: TArea in Self.m_areas do
      if (Area.NUZblkCnt > 0) then
        Area.NUZblkCnt := Area.NUZblkCnt - 1;
  end else begin
    if ((not Self.m_state.NUZ) and (NUZ)) then
      for var area: TArea in Self.m_areas do
        Area.NUZblkCnt := Area.NUZblkCnt + 1;
  end;

  Self.m_state.NUZ := NUZ;
  Self.Change();
end;

procedure TBlkTrack.SetZaver(zaver: TZaver);
var old: TZaver;
begin
  if (zaver = Self.zaver) then
    Exit();

  if ((Self.m_state.zaver > TZaver.no) and ((zaver = TZaver.no) or (zaver = TZaver.ab))) then
    Self.NUZ := false;

  old := Self.zaver;
  Self.m_state.zaver := zaver;
  Self.m_state.trainPredict := -1;

  if ((old > TZaver.no) and (zaver = TZaver.no)) then
    Self.CallChangeEvents(Self.eventsOnZaverReleaseOrAB)
  else if ((old <> TZaver.no) and (old <> TZaver.ab) and (zaver = TZaver.ab)) then
    Self.CallChangeEvents(Self.eventsOnZaverReleaseOrAB);

  // staveci zavery se do panelu neposilaji, protoze jsou mi k nicemu
  if ((Self.zaver <> TZaver.staveni) or (old <> TZaver.no) or (diag.showZaver)) then
    Self.Change();
end;

procedure TBlkTrack.SetNote(note: string);
begin
  Self.m_state.note := note;
  Self.Change();
end;

procedure TBlkTrack.mSetLockout(lockout: string);
begin
  Self.m_state.lockout := lockout;
  Self.Change();
end;

procedure TBlkTrack.ORVylukaNull(Sender: TIdContext; success: Boolean);
begin
  if (success) then
    Self.lockout := '';
end;

procedure TBlkTrack.SetLockout(Sender: TIdContext; lockout: string);
begin
  if ((Self.m_state.lockout <> '') and (lockout = '')) then
  begin
    PanelServer.ConfirmationSequence(Sender, Self.ORVylukaNull, Self.m_areas[0], 'Zrušení výluky',
      GetObjsList(Self), nil);
  end else begin
    Self.lockout := lockout;
  end;
end;

function TBlkTrack.GetTrainPredict(): TTrain;
begin
  if (Self.m_state.trainPredict = -1) then
    Exit(nil);
  Result := TrainDb.trains[Self.m_state.trainPredict];
end;

procedure TBlkTrack.SetTrainPredict(Train: TTrain);
var old: Integer;
begin
  old := Self.m_state.trainPredict;
  if (Train = nil) then
    Self.m_state.trainPredict := -1
  else
    Self.m_state.trainPredict := Train.index;

  if ((Train = nil) and (old > -1)) then
  begin
    // odstranit predvidany odjezd mazane predpovidane soupravy
    if (TrainDb.trains[old].IsPOdj(Self)) then
      TrainDb.trains[old].RemovePOdj(Self);
  end;

  Self.Change();
end;

procedure TBlkTrack.SetJCend(jcEnd: TZaver);
begin
  Self.m_state.jcEnd := jcEnd;
  Self.Change();
end;

procedure TBlkTrack.OnBoosterChange();
begin
  if (Boosters.ContainsKey(Self.m_settings.boosterId)) then
  begin
    Self.DCC := (Boosters[Self.m_settings.boosterId].DCC = TBoosterSignal.ok);
    Self.power := Boosters[Self.m_settings.boosterId].power;
    Self.shortCircuit := Boosters[Self.m_settings.boosterId].overload;
  end else begin
    Self.DCC := (TrakceI.TrackStatusSafe() = TTrkStatus.tsOn);
    Self.power := TBoosterSignal.undef;
    Self.shortCircuit := TBoosterSignal.undef;
  end;
end;

procedure TBlkTrack.SetShortCircuit(state: TBoosterSignal);
begin
  if (Self.frozen) then
    Self.last_zes_zkrat := state;

  if (Self.m_state.shortCircuit = state) then
    Exit();

  if ((state = TBoosterSignal.error) and (not Self.frozen) and ((not Self.DCC) or (Self.power = TBoosterSignal.error) or
    (Now < Self.state.shortCircSenseTime))) then
  begin
    if (Self.shortCircuit <> TBoosterSignal.ok) then
    begin
      Self.m_state.shortCircuit := TBoosterSignal.ok;
      Self.Change();
    end;
    Self.Freeze();
    Exit();
  end;

  if (state = TBoosterSignal.error) then
  begin
    // do OR oznamime, ze nastal zkrat, pak se prehraje zvuk v klientech...
    for var area: TArea in Self.m_areas do
      Area.shortCircBlkCnt := Area.shortCircBlkCnt + 1;
  end else begin
    if (Self.m_state.shortCircuit = TBoosterSignal.error) then
      for var area: TArea in Self.m_areas do
        Area.shortCircBlkCnt := Area.shortCircBlkCnt - 1;
  end;

  Self.m_state.shortCircuit := state;
  Self.Change();
end;

procedure TBlkTrack.SetPower(state: TBoosterSignal);
begin
  if (Self.m_state.power = state) then
    Exit();

  Self.m_state.power := state;

  if (state = TBoosterSignal.ok) then
    Self.m_state.shortCircSenseTime := Now + EncodeTime(0, 0, 1, 0);

  Self.Change();
end;

procedure TBlkTrack.SetDCC(state: Boolean);
begin
  if (state = Self.state.DCC) then
    Exit();

  // doslo ke zmene DCC
  Self.m_state.DCC := state;
  if (state) then
    Self.m_state.shortCircSenseTime := Now + EncodeTime(0, 0, 1, 0)
  else
    Self.Freeze();

  Self.Change();
end;

procedure TBlkTrack.SetTrainMoving(moving: Integer);
begin
  if (Self.m_state.trainMoving <> moving) then
  begin
    Self.m_state.trainMoving := moving;
    Self.Change();
  end;
end;

procedure TBlkTrack.Freeze();
begin
  if (Self.frozen) then
    Exit();

  inherited;
  Self.last_zes_zkrat := Self.shortCircuit;
end;

procedure TBlkTrack.UnFreeze();
begin
  if (not Self.frozen) then
    Exit();

  inherited;
  if (Self.shortCircuit <> Self.last_zes_zkrat) then
  begin
    Self.shortCircuit := Self.last_zes_zkrat;
    Self.Change();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkTrack.GetSettings(): TBlkTrackSettings;
begin
  Result := Self.m_settings;
end;

procedure TBlkTrack.SetSettings(Data: TBlkTrackSettings);
begin
  if (Self.m_settings.houkEvL <> Data.houkEvL) then
    Self.m_settings.houkEvL.free();

  if (Self.m_settings.houkEvS <> Data.houkEvS) then
    Self.m_settings.houkEvS.free();

  if (Self.m_settings.RCSAddrs <> Data.RCSAddrs) then
    Self.m_settings.RCSAddrs.free();

  Self.m_settings := Data;

  if (not Self.spnl.stationTrack) then
    Self.m_settings.maxTrains := 1;

  Self.Change();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkTrack.MenuNewTrainClick(SenderPnl: TIdContext; SenderOR: TObject; itemindex: Integer);
begin
  // nejdrive posleme aktualni seznam hnacich vozidel
  (SenderOR as TArea).PanelHVList(SenderPnl);

  // pak posleme pozadavek na editaci hnaciho vozidla
  (SenderOR as TArea).BlkNewTrain(Self, SenderPnl, (itemindex - 2) div 2);
end;

procedure TBlkTrack.MenuVLOZTrainClick(SenderPnl: TIdContext; SenderOR: TObject; itemindex: Integer);
begin
  Self.MoveTrain(SenderPnl, SenderOR, (itemindex - 2) div 2);
end;

procedure TBlkTrack.MenuEditTrainClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  if ((TPanelConnData(SenderPnl.Data).train_menu_index < 0) or (TPanelConnData(SenderPnl.Data).train_menu_index >=
    Self.trains.Count)) then
    Exit();

  // nejdrive posleme aktualni senam hnacich vozidel
  (SenderOR as TArea).PanelHVList(SenderPnl);

  // pak posleme pozadavek na editaci hnaciho vozidla
  (SenderOR as TArea).BlkEditTrain(Self, SenderPnl,
    TrainDb.trains[Self.trains[TPanelConnData(SenderPnl.Data).train_menu_index]]);
end;

procedure TBlkTrack.MenuInfoTrainClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  var train: TTrain := nil;
  if ((TPanelConnData(SenderPnl.Data).train_menu_index >= 0) and (TPanelConnData(SenderPnl.Data).train_menu_index < Self.trains.Count)) then
    train := TrainDb.trains[Self.trains[TPanelConnData(SenderPnl.Data).train_menu_index]]
  else if ((TPanelConnData(SenderPnl.Data).train_menu_index = 0) and (Self.trainPredict <> nil)) then
    train := Self.trainPredict;

  if (train = nil) then
    Exit();

  var csItems := train.InfoWindowItems();
  try
    PanelServer.InfoWindow(SenderPnl, nil, TArea(SenderOR), 'Vlak ' + train.name, GetObjsList(Self), csItems, True, False);
  finally
    csItems.Free();
  end;
end;

procedure TBlkTrack.MenuDeleteTrainClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  if ((TPanelConnData(SenderPnl.Data).train_menu_index < 0) or (TPanelConnData(SenderPnl.Data).train_menu_index >=
    Self.trains.Count)) then
    Exit();

  var csItems := TList<TConfSeqItem>.Create();
  try
    for var blk in Blocks.GetBlkWithTrain(TrainDb.trains[Self.trains[TPanelConnData(SenderPnl.Data).train_menu_index]]) do
      csItems.Add(CSItem(blk, 'Smazání soupravy z úseku'));
    PanelServer.ConfirmationSequence(SenderPnl, Self.PotvrDeleteTrain, SenderOR as TArea,
      'Smazání soupravy ' + TrainDb.trains[Self.trains[TPanelConnData(SenderPnl.Data).train_menu_index]].name,
      GetObjsList(Self), csItems, true, false);
  finally
    csItems.Free();
  end;
end;

procedure TBlkTrack.PotvrDeleteTrain(Sender: TIdContext; success: Boolean);
begin
  if ((TPanelConnData(Sender.Data).train_menu_index < 0) or (TPanelConnData(Sender.Data).train_menu_index >=
    Self.trains.Count)) then
    Exit();

  if (success) then
  begin
    if (Self.m_state.trainMoving = TPanelConnData(Sender.Data).train_menu_index) then
      Self.m_state.trainMoving := -1;
    TrainDb.trains.Remove(Self.trains[TPanelConnData(Sender.Data).train_menu_index]);
  end;
end;

procedure TBlkTrack.PotvrUvolTrain(Sender: TIdContext; success: Boolean);
begin
  if ((TPanelConnData(Sender.Data).train_menu_index < 0) or (TPanelConnData(Sender.Data).train_menu_index >=
    Self.trains.Count)) then
    Exit();

  if (not success) then
    Exit();

  if (Blocks.GetBlkWithTrain(TrainDb.trains[Self.trains[TPanelConnData(Sender.Data).train_menu_index]]).Count = 1) then
  begin
    TrainDb.trains.Remove(Self.trains[TPanelConnData(Sender.Data).train_menu_index]);
    PanelServer.SendInfoMsg(Sender, 'Souprava odstraněna');
  end else begin
    Self.RemoveTrain(Self.trains[TPanelConnData(Sender.Data).train_menu_index]);
  end;
end;

procedure TBlkTrack.MenuUVOLTrainClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  if ((TPanelConnData(SenderPnl.Data).train_menu_index < 0) or (TPanelConnData(SenderPnl.Data).train_menu_index >=
    Self.trains.Count)) then
    Exit();

  PanelServer.ConfirmationSequence(SenderPnl, Self.PotvrUvolTrain, SenderOR as TArea,
    'Uvolnění soupravy ' + TrainDb.trains[Self.trains[TPanelConnData(SenderPnl.Data).train_menu_index]].name +
    ' z bloku', GetObjsList(Self), nil);
end;

procedure TBlkTrack.MenuVEZMITrainClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  if ((TPanelConnData(SenderPnl.Data).train_menu_index < 0) or (TPanelConnData(SenderPnl.Data).train_menu_index >=
    Self.trains.Count)) then
    Exit();

  var train: TTrain := TrainDb.trains[Self.trains[TPanelConnData(SenderPnl.Data).train_menu_index]];

  if (train.stolen) then
  begin
    // Prevzit soupravu, ktera byla ukradena.
    Self.MenuXVEZMITrainClick(SenderPnl, SenderOR);
  end else begin
    if (train.IsAnyLokoInRegulator()) then
    begin
      // Nasilim prevzit lokomotivy z regulatoru.
      Self.MenuRegVEZMITrainClick(SenderPnl, SenderOR);
    end;
  end;
end;

procedure TBlkTrack.XTakeTrainOk(Sender: TObject; Data: Pointer);
begin
  PanelServer.SendInfoMsg(TIdContext(Data), 'Vlak převzat');
end;

procedure TBlkTrack.XTakeTrainErr(Sender: TObject; Data: Pointer);
begin
  PanelServer.BottomError(TIdContext(Data), 'Vlak se nepodařilo převzít', '', 'TECHNOLOGIE');
end;

procedure TBlkTrack.MenuXVEZMITrainClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  if ((TPanelConnData(SenderPnl.Data).train_menu_index < 0) or (TPanelConnData(SenderPnl.Data).train_menu_index >=
    Self.trains.Count)) then
    Exit();

  var train: TTrain := TrainDb.trains[Self.trains[TPanelConnData(SenderPnl.Data).train_menu_index]];
  train.Acquire(TTrakce.Callback(Self.XTakeTrainOk, SenderPnl), TTrakce.Callback(Self.XTakeTrainErr, SenderPnl));
end;

procedure TBlkTrack.MenuRegVEZMITrainClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  if ((TPanelConnData(SenderPnl.Data).train_menu_index < 0) or (TPanelConnData(SenderPnl.Data).train_menu_index >=
    Self.trains.Count)) then
    Exit();
  var train: TTrain := TrainDb.trains[Self.trains[TPanelConnData(SenderPnl.Data).train_menu_index]];

  var csItems: TConfSeqItems := TConfSeqItems.Create();
  try
    for var hvaddr: Integer in Train.HVs do
      if (HVDb[hvaddr] <> nil) then
        csItems.Add(CSItem(HVDb[hvaddr].NiceName(), 'Násilné převzetí řízení'));

    PanelServer.ConfirmationSequence(SenderPnl, Self.PotvrRegVezmiTrain, SenderOR as TArea,
      'Nouzové převzetí hnacích vozidel do automatického řízení', GetObjsList(Self), csItems, True, False);
  finally
    csItems.Free();
  end;
end;

procedure TBlkTrack.PotvrRegVezmiTrain(Sender: TIdContext; success: Boolean);
begin
  if ((TPanelConnData(Sender.Data).train_menu_index < 0) or (TPanelConnData(Sender.Data).train_menu_index >=
    Self.trains.Count) or (not success)) then
    Exit();
  var train: TTrain := TrainDb.trains[Self.trains[TPanelConnData(Sender.Data).train_menu_index]];

  try
    train.ForceRemoveAllRegulators();
    PanelServer.SendInfoMsg(Sender, 'Vlak převzat');
  except
    on E: Exception do
      PanelServer.BottomError(Sender, 'Vlak se nepodařilo převzít', TArea(Sender).ShortName, 'TECHNOLOGIE');
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkTrack.MenuStitClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  PanelServer.note(SenderPnl, Self, Self.state.note);
end;

procedure TBlkTrack.MenuVylClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  PanelServer.Lockut(SenderPnl, Self, Self.state.lockout);
end;

// pokud volba nebyla uspesna, vraci false a v tom pripade je vyvolano menu
function TBlkTrack.MenuKCClick(SenderPnl: TIdContext; SenderOR: TObject): Boolean;
begin
  if ((Self.m_state.jcEnd <> TZaver.no) and (not TPanelConnData(SenderPnl.Data).pathBlocks.Contains(Self))) then
  begin
    PanelServer.SendInfoMsg(SenderPnl, 'Probíhá volba');
    Exit(true);
  end;

  if (TPanelConnData(SenderPnl.Data).pathBlocks.Count < 1) then // no start selected
    Exit(false);
  if (not TPanelConnData(SenderPnl.Data).PathIsStartSignal()) then
    Exit(false);

  var signal := TBlkSignal(TPanelConnData(SenderPnl.Data).pathBlocks[0]);

  case (signal.selected) of
    TBlkSignalSelection.VC:
      Self.m_state.jcEnd := TZaver.vlak;
    TBlkSignalSelection.PC:
      Self.m_state.jcEnd := TZaver.posun;
    TBlkSignalSelection.NC, TBlkSignalSelection.PP:
      Self.m_state.jcEnd := TZaver.nouz;
  end; // case

  TPanelConnData(SenderPnl.Data).pathBlocks.Add(Self);

  try
    JCDb.ActivateJC(TPanelConnData(SenderPnl.Data).pathBlocks, SenderPnl, SenderOR, signal.beginAB);
  except
    on E: Exception do
    begin
      AppEvents.LogException(E, 'TBlkTrack.MenuKCClick - JCDb.ActivateJC');
      TPanelConnData(SenderPnl.Data).DeleteAndHideLastPathBlock();
      PanelServer.BottomError(SenderPnl, 'Vnitřní chyba serveru', TArea(SenderOR).shortName, 'TECHNOLOGIE')
    end;
  end;

  Self.Change();
  Result := true;
end;

function TBlkTrack.MenuVBClick(SenderPnl: TIdContext; SenderOR: TObject): Boolean;
begin
  if (Self.m_state.jcEnd <> TZaver.no) then
  begin
    PanelServer.SendInfoMsg(SenderPnl, 'Probíhá volba');
    Exit(true);
  end;

  if (not Self.CanBeNextVB(TPanelConnData(SenderPnl.Data).pathBlocks)) then
  begin
    PanelServer.SendInfoMsg(SenderPnl, 'Není variantním bodem žádné JC');
    Exit(true);
  end;

  var startSignal := TBlkSignal(TPanelConnData(SenderPnl.Data).pathBlocks[0]);

  case (startSignal.selected) of
    TBlkSignalSelection.VC:
      Self.m_state.jcEnd := TZaver.vlak;
    TBlkSignalSelection.PC:
      Self.m_state.jcEnd := TZaver.posun;
    TBlkSignalSelection.NC:
      Self.m_state.jcEnd := TZaver.nouz;
    TBlkSignalSelection.PP:
      Self.m_state.jcEnd := TZaver.nouz;
  end;

  TPanelConnData(SenderPnl.Data).pathBlocks.Add(Self);
  Self.Change();
  Result := true;
end;

procedure TBlkTrack.MenuNUZStartClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  Self.NUZ := true;
end;

procedure TBlkTrack.MenuNUZStopClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  Self.NUZ := false;
end;

procedure TBlkTrack.MenuPRESUNTrainClick(SenderPnl: TIdContext; SenderOR: TObject; new_state: Boolean);
begin
  if ((TPanelConnData(SenderPnl.Data).train_menu_index < 0) or (TPanelConnData(SenderPnl.Data).train_menu_index >=
    Self.trains.Count)) then
    Exit();

  if (new_state) then
  begin
    var blk: TBlk := Blocks.GetBlkTrackTrainMoving((SenderOR as TArea).id);
    if (blk <> nil) then
      (blk as TBlkTrack).trainMoving := -1;
    Self.trainMoving := TPanelConnData(SenderPnl.Data).train_menu_index;
  end else begin
    Self.trainMoving := -1;
  end;
end;

procedure TBlkTrack.MenuRUCTrainClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  if ((TPanelConnData(SenderPnl.Data).train_menu_index < 0) or (TPanelConnData(SenderPnl.Data).train_menu_index >=
    Self.trains.Count)) then
    Exit();

  var str: string := (SenderOR as TArea).id + ';LOK-TOKEN;OK;';
  for var addr: Integer in TrainDb.trains[Self.trains[TPanelConnData(SenderPnl.Data).train_menu_index]].HVs do
  begin
    var HV: THV := HVDb[addr];
    str := str + '[' + IntToStr(HV.addr) + '|' + HV.GetToken() + ']';
  end;

  PanelServer.SendLn(SenderPnl, str);
end;

procedure TBlkTrack.MenuMAUSTrainClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  if ((TPanelConnData(SenderPnl.Data).train_menu_index < 0) or (TPanelConnData(SenderPnl.Data).train_menu_index >=
    Self.trains.Count)) then
    Exit();

  var str: string := (SenderOR as TArea).id + ';MAUS;{';
  for var addr: Integer in TrainDb.trains[Self.trains[TPanelConnData(SenderPnl.Data).train_menu_index]].HVs do
  begin
    var HV: THV := HVDb[addr];
    str := str + IntToStr(HV.addr) + '|';
  end; // for i
  str := str + '}';

  PanelServer.SendLn(SenderPnl, str);
end;

procedure TBlkTrack.MenuSTOPTrainOnClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  if ((TPanelConnData(SenderPnl.Data).train_menu_index < 0) or
      (TPanelConnData(SenderPnl.Data).train_menu_index >= Self.trains.Count)) then
    Exit();

  var train: TTrain := TrainDb.trains[Self.trains[TPanelConnData(SenderPnl.Data).train_menu_index]];
  train.EmergencyStop();

  if (train.IsAnyHVRuc()) then
    train.RucUPO(SenderPnl);
end;

procedure TBlkTrack.MenuSTOPTrainOffClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  if ((TPanelConnData(SenderPnl.Data).train_menu_index < 0) or (TPanelConnData(SenderPnl.Data).train_menu_index >=
    Self.trains.Count)) then
    Exit();

  var train: TTrain := TrainDb.trains[Self.trains[TPanelConnData(SenderPnl.Data).train_menu_index]];
  var UPO := train.RucBarriers();
  try
    if (train.wantedSpeed > 0) then
    begin
      var item: TUPOItem;
      item[0] := GetUPOLine('Pozor !', taCenter, clBlack, clYellow);
      item[1] := GetUPOLine('Souprava bude uvedena do pohybu!');
      item[2] := GetUPOLine(train.name);
      UPO.Add(item);
    end;

    if (UPO.Count > 0) then
      PanelServer.UPO(SenderPnl, UPO, false, Self.PotvrSTOPTrainOff, nil, SenderOR)
    else
      train.EmergencyStopRelease();
  finally
    UPO.Free();
  end;
end;

procedure TBlkTrack.PotvrSTOPTrainOff(Sender: TObject);
begin
  var SenderPnl: TIdContext := TIdContext(Sender);
  if ((TPanelConnData(SenderPnl.Data).train_menu_index < 0) or (TPanelConnData(SenderPnl.Data).train_menu_index >=
    Self.trains.Count)) then
    Exit();

  var train: TTrain := TrainDb.trains[Self.trains[TPanelConnData(SenderPnl.Data).train_menu_index]];
  train.EmergencyStopRelease();
end;

procedure TBlkTrack.MenuJedTrainClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  if ((TPanelConnData(SenderPnl.Data).train_menu_index < 0) or
      (TPanelConnData(SenderPnl.Data).train_menu_index >= Self.trains.Count)) then
    Exit();

  var train: TTrain := TrainDb.trains[Self.trains[TPanelConnData(SenderPnl.Data).train_menu_index]];
  try
    train.DisableSpeedOverride();
  except
    on E:ENotOverriden do
      PanelServer.BottomError(SenderPnl, 'Nelze rozjet nezastavený vlak!', TArea(SenderOR).ShortName, 'Technologie');
  end;
end;

procedure TBlkTrack.MenuObsazClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  try
    for var rcsaddr: TRCSAddr in Self.m_settings.RCSAddrs do
      RCSi.SetInput(rcsaddr, 1);
  except
    PanelServer.BottomError(SenderPnl, 'Simulace nepovolila nastavení RCS vstupů!', TArea(SenderOR).ShortName,
      'SIMULACE');
  end;
end;

procedure TBlkTrack.MenuUvolClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  try
    for var rcsaddr: TRCSAddr in Self.m_settings.RCSAddrs do
      RCSi.SetInput(rcsaddr, 0);
  except
    PanelServer.BottomError(SenderPnl, 'Simulace nepovolila nastavení RCS vstupů!', TArea(SenderOR).ShortName,
      'SIMULACE');
  end;
end;

procedure TBlkTrack.MenuDetClick(SenderPnl: TIdContext; SenderOR: TObject; id: Integer; state: Boolean);
begin
  if ((id < 0) or (id >= Self.m_settings.RCSAddrs.Count)) then
    Exit();

  try
    RCSi.SetInput(Self.m_settings.RCSAddrs[id], ownConvert.BoolToInt(state));
  except
    PanelServer.BottomError(SenderPnl, 'Simulace nepovolila nastavení RCS vstupů!', TArea(SenderOR).ShortName,
      'SIMULACE');
  end;
end;

procedure TBlkTrack.MenuHLASENIOdjezdClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  if ((TPanelConnData(SenderPnl.Data).train_menu_index < 0) or (TPanelConnData(SenderPnl.Data).train_menu_index >=
    Self.trains.Count)) then
    Exit();

  try
    if (not Assigned(TArea(SenderOR).announcement)) then
      Exit();
    TArea(SenderOR).announcement.Departure(Self.GetAnnouncementTrain(TPanelConnData(SenderPnl.Data).train_menu_index));
  except
    on E: Exception do
    begin
      Log('Nepodařilo se spustit staniční hlášení : ' + E.Message, llError);
      PanelServer.BottomError(SenderPnl, 'Nepodařilo se spustit staniční hlášení!', TArea(SenderOR).ShortName,
        'TECHNOLOGIE');
    end;
  end;
end;

procedure TBlkTrack.MenuHLASENIPrijezdClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  if ((TPanelConnData(SenderPnl.Data).train_menu_index < 0) or (TPanelConnData(SenderPnl.Data).train_menu_index >=
    Self.trains.Count)) then
    Exit();

  try
    if (not Assigned(TArea(SenderOR).announcement)) then
      Exit();

    var annTrain: TAnnTrain := Self.GetAnnouncementTrain(TPanelConnData(SenderPnl.Data).train_menu_index);
    var blk: TBlkTrack := announcementHelper.CanPlayArrival(TrainDb.trains[Self.trains[TPanelConnData(SenderPnl.Data).train_menu_index]
      ], TArea(SenderOR)).stationTrack;
    if (blk = nil) then
      Exit();

    annTrain.track := blk.spnl.trackName;
    TArea(SenderOR).announcement.Arrival(annTrain);
  except
    on E: Exception do
    begin
      Log('Nepodařilo se spustit staniční hlášení : ' + E.Message, llError);
      PanelServer.BottomError(SenderPnl, 'Nepodařilo se spustit staniční hlášení!', TArea(SenderOR).ShortName,
        'TECHNOLOGIE');
    end;
  end;
end;

procedure TBlkTrack.MenuHLASENIPrujezdClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  if ((TPanelConnData(SenderPnl.Data).train_menu_index < 0) or (TPanelConnData(SenderPnl.Data).train_menu_index >=
    Self.trains.Count)) then
    Exit();

  try
    if (not Assigned(TArea(SenderOR).announcement)) then
      Exit();

    var annTrain: TAnnTrain := Self.GetAnnouncementTrain(TPanelConnData(SenderPnl.Data).train_menu_index);
    var blk: TBlkTrack := announcementHelper.CanPlayArrival(TrainDb.trains[Self.trains[TPanelConnData(SenderPnl.Data).train_menu_index]
      ], TArea(SenderOR)).stationTrack;

    if (blk <> nil) then
      annTrain.track := blk.spnl.trackName;
    TArea(SenderOR).announcement.Transit(annTrain);
  except
    on E: Exception do
    begin
      Log('Nepodařilo se spustit staniční hlášení : ' + E.Message, llError);
      PanelServer.BottomError(SenderPnl, 'Nepodařilo se spustit staniční hlášení!', TArea(SenderOR).ShortName,
        'TECHNOLOGIE');
    end;
  end;
end;

procedure TBlkTrack.MenuPOdjClick(SenderPnl: TIdContext; SenderOR: TObject);
var train: TTrain;
begin
  if ((TPanelConnData(SenderPnl.Data).train_menu_index >= 0) and
      (TPanelConnData(SenderPnl.Data).train_menu_index < Self.trains.Count)) then
  begin
    train := TrainDb.trains[Self.trains[TPanelConnData(SenderPnl.Data).train_menu_index]];
  end else begin
    if (Self.trainPredict = nil) then
      Exit();
    train := Self.trainPredict;
  end;

  if (Train.IsPOdj(Self)) then
    PanelServer.podj(SenderPnl, Self, Train.index, train.GetPOdj(Self))
  else
    PanelServer.podj(SenderPnl, Self, Train.index, nil);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkTrack.MenuSOUPRAVA(SenderPnl: TIdContext; SenderOR: TObject; trainLocalI: Integer);
var menu: string;
begin
  TPanelConnData(SenderPnl.Data).train_menu_index := trainLocalI;

  var train: TTrain := TrainDb.trains[Self.trains[trainLocalI]];
  menu := '$' + Self.m_globSettings.name + ',';
  menu := menu + '$Souprava ' + train.name + ',-,';
  menu := menu + train.Menu(SenderPnl, SenderOR, Self, trainLocalI);

  PanelServer.menu(SenderPnl, Self, (SenderOR as TArea), menu);
end;

/// /////////////////////////////////////////////////////////////////////////////

// vytvoreni menu pro potreby konkretniho bloku:
function TBlkTrack.ShowPanelMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TAreaRights): string;
var addStr: string;
begin
  Result := inherited;

  if (Blocks.GetBlkTrackTrainMoving((SenderOR as TArea).id) <> nil) then
    addStr := 'VLOŽ vlak,'
  else
    addStr := 'NOVÝ vlak,';

  if (Self.TrainsFull() and (Self.trains.Count = 1)) then
  begin
    TPanelConnData(SenderPnl.Data).train_menu_index := 0;
    var train: TTrain := TrainDb.trains[Self.trains[0]];
    Result := Result + train.Menu(SenderPnl, SenderOR, Self, 0) + '-,';
  end else begin
    var canAdd := ((Self.CanStandTrain()) and (((not Self.TrainsFull()) and ((Self.m_state.occupied = TTrackState.occupied)
      or (Self.m_settings.RCSAddrs.Count = 0))) or // novy vlak
      (addStr = 'VLOŽ vlak,') // presun vlaku
      ));

    if (canAdd) then
      Result := Result + addStr;

    for var train: Integer in Self.trains do
    begin
      Result := Result + TrainDb.trains[Train].name + ',';
      if (canAdd) then
        Result := Result + addStr;
    end;

    if ((canAdd) or (Self.trains.Count > 0)) then
      Result := Result + '-,';
  end;

  if ((Self.trainPredict <> nil) and (Self.spnl.stationTrack)) then
    Result := Result + 'INFO vlak,PODJ,-,';

  Result := Result + 'STIT,VYL,';

  if (Self.m_state.NUZ) then
    Result := Result + '-,NUZ<,';

  if ((((not(SenderOR as TArea).NUZtimer) and (Integer(Self.m_state.zaver) > 0) and (Self.m_state.zaver <> TZaver.ab)
    and (Self.m_state.zaver <> TZaver.staveni) and (Self.typ = btTrack)) or
    (rights >= superuser)) and (not Self.m_state.NUZ)) then
    Result := Result + '-,NUZ>,';

  // 11 = KC
  if (TPanelConnData(SenderPnl.Data).PathIsStartSignal()) then
  begin
    if ((Self.CanBeKC(TPanelConnData(SenderPnl.Data).pathBlocks)) or Self.CanBeNextVB(TPanelConnData(SenderPnl.Data).pathBlocks)) then
      Result := Result + '-,';
    if (Self.CanBeKC(TPanelConnData(SenderPnl.Data).pathBlocks)) then
      Result := Result + 'KC,';
    if (Self.CanBeNextVB(TPanelConnData(SenderPnl.Data).pathBlocks)) then
      Result := Result + 'VB,';
  end;

  // pokud mame knihovnu simulator, muzeme ridit stav useku
  // DEBUG nastroj
  if (RCSi.simulation) then
  begin
    Result := Result + '-,';

    for var m_state: TTrackState in Self.sectionsState do
    begin
      if (m_state = TTrackState.free) then
      begin
        Result := Result + '*OBSAZ,';
        break;
      end;
    end;

    for var m_state: TTrackState in Self.sectionsState do
    begin
      if (m_state = TTrackState.occupied) then
      begin
        Result := Result + '*UVOL,';
        break;
      end;
    end;

    if (Self.sectionsState.Count > 1) then
    begin
      for var i: Integer := 0 to Self.sectionsState.Count - 1 do
      begin
        case (Self.sectionsState[i]) of
          TTrackState.free:
            Result := Result + '*DET' + IntToStr(i + 1) + '>,';
          TTrackState.occupied:
            Result := Result + '*DET' + IntToStr(i + 1) + '<,';
        else
          Result := Result + '*DET' + IntToStr(i + 1) + '>,';
          Result := Result + '*DET' + IntToStr(i + 1) + '<,';
        end;
      end;
    end;
  end; // if RCSi.lib = 2
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkTrack.PanelClick(SenderPnl: TIdContext; SenderOR: TObject; Button: TPanelButton; rights: TAreaRights;
  params: string = '');
begin
  case (Button) of
    F2:
      Self.ShowProperMenu(SenderPnl, (SenderOR as TArea), rights, params);

    ENTER:
      begin
        var handled := Self.MenuKCClick(SenderPnl, SenderOR);
        if ((not handled) and ((Self.m_settings.maxTrains = 1) or (Self.trains.Count = 0))) then
          handled := Self.MoveTrain(SenderPnl, SenderOR, 0);
        if (not handled) then
          Self.ShowProperMenu(SenderPnl, (SenderOR as TArea), rights, params);
      end;

    F1:
      begin
        var handled := Self.MenuVBClick(SenderPnl, SenderOR);
        if (not handled) then
          Self.ShowProperMenu(SenderPnl, (SenderOR as TArea), rights, params)
      end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

// toto se zavola pri kliku na jakoukoliv itemu menu tohoto bloku
procedure TBlkTrack.PanelMenuClick(SenderPnl: TIdContext; SenderOR: TObject; item: string; itemindex: Integer);
begin
  if (item = 'NOVÝ vlak') then
    Self.MenuNewTrainClick(SenderPnl, SenderOR, itemindex)
  else if (item = 'VLOŽ vlak') then
    Self.MenuVLOZTrainClick(SenderPnl, SenderOR, itemindex)
  else if (item = 'EDIT vlak') then
    Self.MenuEditTrainClick(SenderPnl, SenderOR)
  else if (item = 'INFO vlak') then
    Self.MenuInfoTrainClick(SenderPnl, SenderOR)
  else if (item = 'ZRUŠ vlak') then
    Self.MenuDeleteTrainClick(SenderPnl, SenderOR)
  else if (item = 'UVOL vlak') then
    Self.MenuUVOLTrainClick(SenderPnl, SenderOR)
  else if (item = 'VEZMI vlak') then
    Self.MenuVEZMITrainClick(SenderPnl, SenderOR)
  else if (item = 'PŘESUŇ vlak>') then
    Self.MenuPRESUNTrainClick(SenderPnl, SenderOR, true)
  else if (item = 'PŘESUŇ vlak<') then
    Self.MenuPRESUNTrainClick(SenderPnl, SenderOR, false)
  else if (item = 'RUČ vlak') then
    Self.MenuRUCTrainClick(SenderPnl, SenderOR)
  else if (item = 'MAUS vlak') then
    Self.MenuMAUSTrainClick(SenderPnl, SenderOR)
  else if (item = 'STOP vlak>') then
    Self.MenuSTOPTrainOnClick(SenderPnl, SenderOR)
  else if (item = 'STOP vlak<') then
    Self.MenuSTOPTrainOffClick(SenderPnl, SenderOR)
  else if (item = 'JEĎ vlak') then
    Self.MenuJEDTrainClick(SenderPnl, SenderOR)
  else if (item = 'STIT') then
    Self.MenuStitClick(SenderPnl, SenderOR)
  else if (item = 'VYL') then
    Self.MenuVylClick(SenderPnl, SenderOR)
  else if (item = 'KC') then
    Self.MenuKCClick(SenderPnl, SenderOR)
  else if (item = 'VB') then
    Self.MenuVBClick(SenderPnl, SenderOR)
  else if (item = 'NUZ>') then
    Self.MenuNUZStartClick(SenderPnl, SenderOR)
  else if (item = 'NUZ<') then
    Self.MenuNUZStopClick(SenderPnl, SenderOR)
  else if (item = 'OBSAZ') then
    Self.MenuObsazClick(SenderPnl, SenderOR)
  else if (item = 'UVOL') then
    Self.MenuUvolClick(SenderPnl, SenderOR)
  else if (item = 'HLÁŠENÍ odjezd') then
    Self.MenuHLASENIOdjezdClick(SenderPnl, SenderOR)
  else if (item = 'HLÁŠENÍ příjezd') then
    Self.MenuHLASENIPrijezdClick(SenderPnl, SenderOR)
  else if (item = 'HLÁŠENÍ průjezd') then
    Self.MenuHLASENIPrujezdClick(SenderPnl, SenderOR)
  else if (item = 'PODJ') then
    Self.MenuPOdjClick(SenderPnl, SenderOR)
  else if (LeftStr(item, 3) = 'DET') then
  begin
    Self.MenuDetClick(SenderPnl, SenderOR, StrToInt(item[4]) - 1, item[5] = '>');
  end else begin
    // cislo soupravy
    for var i: Integer := 0 to Self.trains.Count - 1 do
      if (item = TrainDb.trains[Self.trains[i]].name) then
      begin
        Self.MenuSOUPRAVA(SenderPnl, SenderOR, i);
        break;
      end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

// vraci true, pokud volba vyvolala nejaky efekt (false pokud se ma zobrazit menu)
function TBlkTrack.MoveTrain(SenderPnl: TIdContext; SenderOR: TObject; trainLocalIndex: Integer): Boolean;
var blk: TBlk;
begin
  blk := Blocks.GetBlkTrackTrainMoving((SenderOR as TArea).id);
  if (blk = nil) then
    Exit(false);

  if (not Self.CanStandTrain()) then
  begin
    PanelServer.SendInfoMsg(SenderPnl, 'Loko lze přesunout pouze na staniční kolej!');
    Exit(true);
  end;

  if ((Self.TrainsFull()) and (blk <> Self)) then
  begin
    PanelServer.SendInfoMsg(SenderPnl, 'Do úseku se již nevejde další souprava!');
    Exit(true);
  end;

  if ((Self.zaver > TZaver.no) and (Self.zaver <> TZaver.posun)) then
  begin
    PanelServer.SendInfoMsg(SenderPnl, 'Nelze přesunout na úsek se závěrem!');
    Exit(true);
  end;

  if (not Self.CanTrainSpeedInsert(trainLocalIndex)) then
  begin
    PanelServer.SendInfoMsg(SenderPnl, 'Nelze vložit soupravu před jedoucí soupravu!');
    Exit(true);
  end;

  var train: TTrain := TrainDb.trains[TBlkTrack(blk).trains[TBlkTrack(blk).trainMoving]];

  if (blk = Self) then
  begin
    Self.m_state.trains.Insert(trainLocalIndex, Train.index);

    if (trainLocalIndex <= Self.trainMoving) then
      Self.m_state.trains.Delete(Self.trainMoving + 1)
    else
      Self.m_state.trains.Delete(Self.trainMoving);

    Self.m_state.trainMoving := -1;
    Self.Change();
  end else begin

    try
      Self.AddTrain(trainLocalIndex, Train);
      (blk as TBlkTrack).RemoveTrain(Train);
    except
      on E: Exception do
      begin
        PanelServer.SendInfoMsg(SenderPnl, E.Message);
        Exit(true);
      end;
    end;
  end;

  if (train.station <> TArea(SenderOR)) then
    train.station := TArea(SenderOR);

  PanelServer.SendInfoMsg(SenderPnl, 'Souprava ' + Train.name + ' přesunuta na ' + Self.m_globSettings.name + '.');

  if (Blocks.GetBlkWithTrain(Train).Count = 1) then
  begin
    train.front := Self;
    train.speed := 0; // train could be moving from emergency stop to e.g. middle of track occupied from both sides with trains -> must stop
    for var signal in Self.signalJCRef do
      TBlkSignal(signal).UpdateTrainSpeed(true); // if any active signal has affect to train speed, let it affect
  end;

  for var signal: TBlk in (blk as TBlkTrack).signalJCRef do
  begin
    Blocks.TrainPrediction(signal as TBlkSignal);
    TBlkSignal(signal).UpdateTrainSpeed(true);
  end;

  if (blk <> Self) then
    for var signal: TBlk in Self.signalJCRef do
      Blocks.TrainPrediction(signal as TBlkSignal);

  Result := true;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkTrack.GetPtData(json: TJsonObject; includeState: Boolean);
begin
  inherited;

  TBlk.RCSstoJSON(Self.m_settings.RCSAddrs, json.A['rcs']);

  json['length'] := Self.m_settings.lenght;
  if (Self.m_settings.loop) then
    json['loop'] := Self.m_settings.loop;
  json['booster'] := Self.m_settings.boosterId;
  json['maxTrains'] := Self.m_settings.maxTrains;
  json['stationTrack'] := Self.spnl.stationTrack;
  if (Self.spnl.trackName <> '') then
    json['trackName'] := Self.spnl.trackName;

  if (includeState) then
    Self.GetPtState(json['blockState']);
end;

procedure TBlkTrack.GetPtState(json: TJsonObject);
begin
  case (Self.occupied) of
    TTrackState.disabled:
      json['state'] := 'off';
    TTrackState.none:
      json['state'] := 'none';
    TTrackState.free:
      json['state'] := 'free';
    TTrackState.occupied:
      json['state'] := 'occupied';
  end;

  for var m_state: TTrackState in Self.sectionsState do
  begin
    case (m_state) of
      TTrackState.disabled:
        json.A['sections'].Add('off');
      TTrackState.none:
        json.A['sections'].Add('none');
      TTrackState.free:
        json.A['sections'].Add('free');
      TTrackState.occupied:
        json.A['sections'].Add('occupied');
    end;
  end;

  json['power'] := (Self.power = TBoosterSignal.ok);
  json['shortCircuit'] := (Self.shortCircuit = TBoosterSignal.error);
  json['dcc'] := Self.DCC;

  if (Self.note <> '') then
    json['note'] := Self.note;
  if (Self.lockout <> '') then
    json['lockout'] := Self.lockout;

  for var train: Integer in Self.trains do
    json.A['trains'].Add(TrainDb.trains[Train].name);

  json['lock'] := Integer(Self.state.zaver);
  if (Self.m_state.trainPredict > -1) then
    json['trainPredict'] := TrainDb.trains[Self.m_state.trainPredict].name;
  if (Self.m_state.NUZ) then
    json['nuz'] := true;
  if (Self.m_state.jcEnd <> TZaver.no) then
    json['endJC'] := Integer(Self.m_state.jcEnd);
end;

procedure TBlkTrack.PutPtState(reqJson: TJsonObject; respJson: TJsonObject);
begin
  if (reqJson.Contains('trains')) then
  begin
    if (Cardinal(reqJson.A['trains'].Count) > Self.m_settings.maxTrains) then
    begin
      PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '400', 'Bad Request',
        'Nelze pridat vice souprav, nez je limit useku');
      inherited;
      Exit();
    end;

    Self.RemoveTrains();
    for var trainStr: string in reqJson.A['trains'] do
    begin
      var train: Integer := TrainDb.trains.GetTrainIndexByName(trainStr);
      if (Train > -1) then
      begin
        try
          Self.AddTrainS(Train)
        except
          on E: Exception do
          begin
            PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '400', 'Bad Request', E.Message);
          end;

        end;
      end
      else
        PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '400', 'Bad Request',
          'Souprava ' + trainStr + ' neexistuje, ignoruji.');
    end;
  end;

  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkTrack.LoadHoukEventToList(list: TList<THoukEv>; ini_tech: TMemIniFile; section: string; prefix: string);
begin
  try
    var i: Integer := 0;
    list.Clear();
    var data: string := ini_tech.ReadString(section, prefix + IntToStr(i), '');
    while (data <> '') do
    begin
      try
        list.Add(THoukEv.Create(data));
      except
        Log('Nepodarilo se nacist houkaci udalost ' + Data + ' bloku ' + Self.name, llError);
        Exit();
      end;

      Inc(i);
      Data := ini_tech.ReadString(section, prefix + IntToStr(i), '');
    end;
  except

  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkTrack.CheckHoukEv();
begin
  if (Self.m_state.currentHoukEv < 0) then
    Exit();

  if (not Self.IsTrain()) then
  begin
    Self.houkEvEnabled := false;
    Exit();
  end;

  var list: TList<THoukEv> := Self.GetHoukList();
  if (list = nil) then
  begin
    Self.houkEvEnabled := false;
    Exit();
  end;

  if (Self.m_state.currentHoukEv >= list.Count) then
  begin
    Self.houkEvEnabled := false;
    Exit();
  end;

  // kontrola udalosti
  if (list[Self.m_state.currentHoukEv].CheckTriggerred(Self)) then
  begin
    // udalost aktivovana, zpracovana a automaticky odregistrovana -> presun na dalsi udalost
    Inc(Self.m_state.currentHoukEv);
    if (Self.m_state.currentHoukEv >= list.Count) then
      Self.houkEvEnabled := false
    else
      list[Self.m_state.currentHoukEv].Register(Self.trainL.index);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkTrack.GetHoukEvEnabled(): Boolean;
begin
  Result := (Self.m_state.currentHoukEv > -1);
end;

procedure TBlkTrack.SetHoukEvEnabled(state: Boolean);
begin
  if (state) then
  begin
    if (not Self.IsTrain()) then
      Exit();
    if (Self.GetHoukList().Count = 0) then
      Exit();

    // aktivace prvni houkaci udalosti
    Self.m_state.currentHoukEv := 0;
    Self.GetHoukList()[0].Register(Self.trainL.index);
  end else begin
    Self.m_state.currentHoukEv := -1;

    for var houkEv: THoukEv in Self.m_settings.houkEvL do
      houkEv.Unregister();

    for var houkEv: THoukEv in Self.m_settings.houkEvS do
      houkEv.Unregister();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkTrack.GetHoukList(): TList<THoukEv>;
begin
  if (not Self.IsTrain()) then
    Exit(nil);

  if (Self.trainL.direction = THVSite.odd) then
    Result := Self.m_settings.houkEvL
  else
    Result := Self.m_settings.houkEvS;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkTrack.GetAnnouncementTrain(trainLocalIndex: Integer): TAnnTrain;
begin
  if ((trainLocalIndex < 0) or (trainLocalIndex >= Self.trains.Count)) then
    Exit();

  Result.name := TrainDb.trains[Self.trains[trainLocalIndex]].name;
  Result.typ := TrainDb.trains[Self.trains[trainLocalIndex]].typ;
  Result.track := Self.spnl.trackName;
  Result.fromAreaId := TArea(TrainDb.trains[Self.trains[trainLocalIndex]].areaFrom).id;
  Result.toAreaId := TArea(TrainDb.trains[Self.trains[trainLocalIndex]].areaTo).id;

  Result.timeArrive := 0;

  if ((TrainDb.trains[Self.trains[trainLocalIndex]].IsPOdj(Self)) and
    (TrainDb.trains[Self.trains[trainLocalIndex]].GetPOdj(Self).abs_enabled)) then
    Result.timeDepart := TrainDb.trains[Self.trains[trainLocalIndex]].GetPOdj(Self).abs
  else
    Result.timeDepart := 0;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkTrack.AddNeprofilJC(id: Integer);
begin
  if (Self.m_state.neprofilJCcheck.Contains(id)) then
    Exit();

  Self.m_state.neprofilJCcheck.Add(id);
  if (Self.m_state.neprofilJCcheck.Count = 1) then
    Self.Change();
end;

procedure TBlkTrack.RemoveNeprofilJC(id: Integer);
begin
  if (not Self.m_state.neprofilJCcheck.Contains(id)) then
    Exit();

  Self.m_state.neprofilJCcheck.Remove(id);
  if (Self.m_state.neprofilJCcheck.Count = 0) then
    Self.Change();
end;

function TBlkTrack.IsNeprofilJC(): Boolean;
begin
  Result := (Self.m_state.neprofilJCcheck.Count > 0);
end;

procedure TBlkTrack.NonProfileOccupy();
begin
  for var jcid: Integer in Self.m_state.neprofilJCcheck do
  begin
    var jc: TJC := JCDb.GetJCByID(jcid);
    if (jc <> nil) then
      jc.NonProfileOccupied();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkTrack.GetTrainIL(): Integer;
begin
  if (Self.m_state.trains.Count < 1) then
    Result := -1
  else
    Result := Self.m_state.trains[0];
end;

function TBlkTrack.GetTrainL(): TTrain;
begin
  if (Self.GetTrainIL() = -1) then
    Result := nil
  else
    Result := TrainDb.trains[Self.GetTrainIL()];
end;

function TBlkTrack.GetTrainIS(): Integer;
begin
  if (Self.m_state.trains.Count < 1) then
    Result := -1
  else
    Result := Self.m_state.trains[Self.m_state.trains.Count - 1];
end;

function TBlkTrack.GetTrainS(): TTrain;
begin
  if (Self.GetTrainIS() = -1) then
    Result := nil
  else
    Result := TrainDb.trains[Self.GetTrainIS()];
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkTrack.IsTrain(): Boolean;
begin
  Result := (Self.m_state.trains.Count > 0);
end;

function TBlkTrack.IsTrain(index: Integer): Boolean;
begin
  Result := Self.m_state.trains.Contains(index);
end;

function TBlkTrack.IsTrain(Train: TTrain): Boolean;
begin
  Result := Self.IsTrain(Train.index);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkTrack.AddTrainL(index: Integer);
begin
  if (Self.TrainsFull()) then
    raise ETrainFull.Create('Do bloku ' + Self.name + ' se uz nevejde dalsi souprava!');
  if (Self.trains.Contains(index)) then
    raise EDuplicitTrains.Create('Nelze pridat jednu soupravu na jeden blok vicekrat!');

  Self.m_state.trains.Insert(0, index);
  Self.m_state.trainPredict := -1;
  Self.Change();
end;

procedure TBlkTrack.AddTrainS(index: Integer);
begin
  if (Self.TrainsFull()) then
    raise ETrainFull.Create('Do bloku ' + Self.name + ' se uz nevejde dalsi souprava!');
  if (Self.trains.Contains(index)) then
    raise EDuplicitTrains.Create('Nelze pridat jednu soupravu na jeden blok vicekrat!');

  Self.m_state.trains.Add(index);
  Self.m_state.trainPredict := -1;
  Self.Change();
end;

procedure TBlkTrack.AddTrainL(Train: TTrain);
begin
  Self.AddTrainL(Train.index);
end;

procedure TBlkTrack.AddTrainS(Train: TTrain);
begin
  Self.AddTrainS(Train.index);
end;

procedure TBlkTrack.AddTrain(localTrainIndex: Integer; Train: Integer);
begin
  if (Self.TrainsFull()) then
    raise ETrainFull.Create('Do bloku ' + Self.name + ' se uz nevejde dalsi souprava!');
  if (Self.trains.Contains(Train)) then
    raise EDuplicitTrains.Create('Nelze pridat jednu soupravu na jeden blok vicekrat!');
  if (not Self.CanTrainSpeedInsert(localTrainIndex)) then
    raise ERunningTrain.Create('Nelze vložit soupravu před jedoucí soupravu!');

  Self.m_state.trains.Insert(localTrainIndex, Train);
  Self.m_state.trainPredict := -1;
  Self.Change();
end;

procedure TBlkTrack.AddTrain(localTrainIndex: Integer; Train: TTrain);
begin
  Self.AddTrain(localTrainIndex, Train.index);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkTrack.RemoveTrains();
var Train: Integer;
begin
  for Train in Self.trains do
    if (TrainDb.trains[Train].IsPOdj(Self)) then
      TrainDb.trains[Train].RemovePOdj(Self);

  Self.m_state.trains.Clear();
  Self.m_state.trainMoving := -1;
  Self.m_state.slowingReady := false;
  Self.houkEvEnabled := false;
  Self.Change();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkTrack.RemoveTrain(index: Integer);
begin
  if (Self.m_state.trains.Contains(index)) then
  begin
    if ((Self.IsTrainMoving) and (Self.trains[Self.trainMoving] = index)) then
      Self.m_state.trainMoving := -1;

    Self.m_state.trains.Remove(index);

    // odstranit predvidany odjezd z aktualniho useku
    if (TrainDb.trains[index].IsPOdj(Self)) then
      TrainDb.trains[index].RemovePOdj(Self);

    Self.Change();
  end else begin
    raise ETrainNotExists.Create('Souprava ' + IntToStr(index) + ' neexistuje na useku ' + Self.name);
  end;

  if (not Self.IsTrain()) then
  begin
    Self.m_state.slowingReady := false;
    Self.houkEvEnabled := false;
  end;
end;

procedure TBlkTrack.RemoveTrain(Train: TTrain);
begin
  Self.RemoveTrain(Train.index);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkTrack.TrainsFull(): Boolean;
begin
  Result := (Cardinal(Self.m_state.trains.Count) >= Self.m_settings.maxTrains);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkTrack.IsTrainMoving(): Boolean;
begin
  Result := (Self.state.trainMoving > -1);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkTrack.GetTrainI(): Integer;
begin
  if (Self.trains.Count = 0) then
    Exit(-1)
  else if (Self.trains.Count = 1) then
    Exit(Self.trains[0])
  else
    raise EMultipleTrains.Create('Usek ' + Self.name +
      ' obsahuje vice souprav, nelze se proto ptat jen na jednu soupravu!');
end;

function TBlkTrack.GetTrain(): TTrain;
begin
  if (Self.GetTrainI() = -1) then
    Result := nil
  else
    Result := TrainDb.trains[Self.GetTrainI()];
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkTrack.ShowProperMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TAreaRights; params: string);
begin
  if (params <> '') then
  begin
    var i: Integer := StrToIntDef(params, -1);
    if ((i <> -1) and (i >= 0) and (i < Self.trains.Count)) then
      Self.MenuSOUPRAVA(SenderPnl, (SenderOR as TArea), i)
    else
      PanelServer.menu(SenderPnl, Self, (SenderOR as TArea), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));
  end
  else
    PanelServer.menu(SenderPnl, Self, (SenderOR as TArea), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));
end;

/// /////////////////////////////////////////////////////////////////////////////

// vraci true prave tehdy, kdyz lze vlozit soupravu na pozici index
// kontroluje, zda-li se nenazime vlozit pred soupravu v pohybu
function TBlkTrack.CanTrainSpeedInsert(index: Integer): Boolean;
begin
  Result := not ((Self.trains.Count > 0) and (((index = 0) and (TrainDb.trains[Self.trains[index]].wantedSpeed > 0) and
    (TrainDb.trains[Self.trains[index]].direction = THVSite.even)) or ((index = Self.trains.Count) and
    (TrainDb.trains[Self.trains[index - 1]].wantedSpeed > 0) and
    (TrainDb.trains[Self.trains[index - 1]].direction = THVSite.odd))));
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkTrack.POdjChanged(trainId: Integer; var podj: TPOdj);
var Train: Integer;
  was: Boolean;
  nav: TBlk;
  jc: TJC;
begin
  if ((not Self.trains.Contains(trainId)) and (trainId <> Self.m_state.trainPredict)) then
    raise Exception.Create('Souprava již není na úseku!');

  was := TrainDb.trains[trainId].IsPOdj(Self);

  for Train in Self.trains do
    if (Train = trainId) then
      podj.RecordOriginNow();

  TrainDb.trains[trainId].AddOrUpdatePOdj(Self, podj);

  if ((was) and (not TrainDb.trains[trainId].IsPOdj(Self))) then
  begin
    // PODJ bylo odstraneno -> rozjet soupravu pred navestidlem i kdyz neni na zastavovaci udalosti
    // aktualizaci rychlosti pro vsechny signalJCRef bychom nemeli nic pokazit
    for nav in Self.signalJCRef do
      TBlkSignal(nav).UpdateTrainSpeed(true);
  end;

  // Pri zruseni / zavedei PODJ aktualizovat rychlsot loko, ktera prijizdi,
  // protoze muze dojit ke zmene rychlosti
  jc := JCDb.FindActiveJCWithTrack(Self.id);
  if (jc <> nil) then
    TBlkSignal(jc.signal).UpdateTrainSpeed(true);

  Self.PropagatePOdjToRailway();
  Self.Change();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkTrack.CheckPOdjChanged();
var shouldChange: Boolean;
begin
  shouldChange := false;

  for var trainI: Integer in Self.trains do
  begin
    var train: TTrain := TrainDb.trains[trainI];

    if ((Train.IsPOdj(Self)) and (Train.GetPOdj(Self).changed)) then
    begin
      var podj: TPOdj := Train.GetPOdj(Self);

      // prehravani zvukove vystrahy
      if ((not shouldChange) and (Self.IsStujForTrain(Train))) then
      begin
        if ((podj.phase_old = ppPreparing) and (podj.GetPhase() = ppGoingToLeave)) then
          for var area: TArea in Self.m_areas do
            Area.BlkPlaySound(Self, TAreaRights.write, _SND_JC_CALL)

        else if ((podj.phase_old = ppGoingToLeave) and (podj.GetPhase() = ppSoundLeave)) then
          for var area: TArea in Self.m_areas do
            Area.BlkPlaySound(Self, TAreaRights.write, _SND_JC_NO);
      end;

      if (train.GetPOdj(Self).DepRealDelta() < 0) then
      begin
        train.RemovePOdj(Self);

        // tvrda aktualizace rychlosti soupravy
        if ((Train = Self.trainL) or (Train = Self.trainSudy)) then
        begin
          for var signal: TBlk in Self.signalJCRef do
            if (((TBlkSignal(signal).direction = THVSite.even) and (Train = Self.trainL)) or
              ((TBlkSignal(signal).direction = THVSite.odd) and (Train = Self.trainSudy))) then
              TBlkSignal(signal).UpdateTrainSpeed(true);
        end;
      end
      else
        Train.GetPOdj(Self).changed := false;

      shouldChange := true;
    end;
  end;

  if (Self.trainPredict <> nil) then
  begin
    var train: TTrain := Self.trainPredict;
    if ((train.IsPOdj(Self)) and (train.GetPOdj(Self).changed)) then
    begin
      if (train.GetPOdj(Self).DepRealDelta() < 0) then
        train.RemovePOdj(Self)
      else
        train.GetPOdj(Self).changed := false;

      shouldChange := true;
    end;
  end;

  if (shouldChange) then
    Self.Change();
end;

/// /////////////////////////////////////////////////////////////////////////////

// predvidane odjezdy se mazou pres bloky, aby bloky zavolaly Change().
procedure TBlkTrack.ClearPOdj();
var change: Boolean;
begin
  change := false;

  for var train: Integer in Self.trains do
  begin
    if (TrainDb.trains[Train].IsPOdj(Self)) then
    begin
      TrainDb.trains[Train].RemovePOdj(Self);
      change := true;
    end;
  end;

  if ((Self.trainPredict <> nil) and (Self.trainPredict.IsPOdj(Self))) then
  begin
    Self.trainPredict.RemovePOdj(Self);
    change := true;
  end;

  if (change) then
    Self.Change();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkTrack.PropagatePOdjToRailway();
begin
  for var signal: TBlk in Self.signalJCRef do
    TBlkSignal(signal).PropagatePOdjToRailway();
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkTrack.IsStujForTrain(Train: TTrain): Boolean;
begin
  if (not Self.trains.Contains(Train.index)) then
    Exit(false);
  if (Self.signalJCRef.Count = 0) then
    Exit(true);

  if (Self.signalJCRef.Count = 1) then
  begin
    if (not TBlkSignal(Self.signalJCRef[0]).IsGoSignal()) then
      Exit(true);
    if (TBlkSignal(Self.signalJCRef[0]).direction = THVSite.odd) then
      Exit(Train <> Self.trainSudy)
    else
      Exit(Train <> Self.trainL);
  end;

  if (Self.signalJCRef.Count = 2) then
  begin
    if ((Self.trains.Count = 1) and (not TBlkSignal(Self.signalJCRef[0]).IsGoSignal()) and
      (not TBlkSignal(Self.signalJCRef[1]).IsGoSignal())) then
      Exit(true);
    if ((Self.trains.Count >= 2) and ((not TBlkSignal(Self.signalJCRef[0]).IsGoSignal()) or
      (not TBlkSignal(Self.signalJCRef[1]).IsGoSignal()))) then
      Exit(true);
    if ((Self.trains.Count > 2) and (Self.trainL <> Train) and (Self.trainSudy <> Train)) then
      Exit(true);
  end;

  Result := false;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkTrack.PanelStateString(): string;
var fg, bg, nebarVetve, sfg, sbg: TColor;
begin
  // Pro blok trati se vola take
  Result := IntToStr(Integer(btTrack)) + ';' + IntToStr(Self.id) + ';';;

  nebarVetve := $A0A0A0;

  // --- Foreground ---

  case (Self.occupied) of
    TTrackState.disabled:
      fg := clFuchsia;
    TTrackState.none, TTrackState.free:
      fg := ite(Self.PstIs(), clBlue, $A0A0A0);
    TTrackState.occupied:
      fg := clRed;
  else
    fg := clFuchsia;
  end;

  if (Self.PstIs()) then
    nebarVetve := clBlue;

  // zobrazeni zakazu odjezdu do trati
  if ((fg = $A0A0A0) and (Self.typ = btRT) and (TBlkRT(Self).inRailway > -1)) then
  begin
    var railway := Blocks.GetBlkRailwayByID(TBlkRT(Self).inRailway);
    if (railway <> nil) then
      if (railway.departureForbidden) then
        fg := clBlue;
  end;

  // neprofilove deleni v useku
  if ((fg = $A0A0A0) and (Self.IsNeprofilJC())) then
    fg := clYellow;

  // zaver
  if (((Self.occupied) = TTrackState.free) and (Self.typ = btTrack) and (Self.GetSettings().RCSAddrs.Count > 0)) then
  begin
    case (Self.zaver) of
      TZaver.vlak:
        fg := clLime;
      TZaver.posun:
        fg := clWhite;
      TZaver.nouz:
        fg := clAqua;
      TZaver.ab:
        if (diag.showZaver) then
          fg := $707070
        else
          fg := $A0A0A0;
      TZaver.staveni:
        if (diag.showZaver) then
          fg := clBlue;
    end; // case
  end;

  // porucha BP v trati
  if ((Self.typ = btRT) and (TBlkRT(Self).bpError)) then
    fg := clAqua;

  if (fg = clYellow) then
    nebarVetve := clYellow;

  Result := Result + ownConvert.ColorToStr(fg) + ';';

  // --- Background ---

  bg := clBlack;
  if (Self.note <> '') then
    bg := clTeal;
  if (Self.lockout <> '') then
    bg := clOlive;

  if (not Self.DCC) then
    bg := clMaroon;
  if (Self.shortCircuit = TBoosterSignal.error) then
    bg := clFuchsia;
  if ((Self.power <> TBoosterSignal.ok) or (Self.shortCircuit = TBoosterSignal.undef)) then
    bg := clBlue;

  Result := Result + ownConvert.ColorToStr(bg) + ';';

  Result := Result + IntToStr(ownConvert.BoolToInt(Self.NUZ)) + ';' + IntToStr(Integer(Self.jcEnd)) + ';' +
    ownConvert.ColorToStr(nebarVetve) + ';';

  // seznam souprav
  Result := Result + '{';
  for var i: Integer := 0 to Self.trains.Count - 1 do
  begin
    var trainI: Integer := Self.trains[i];
    var train := TrainDb.trains[trainI];

    sfg := fg;
    sbg := bg;

    if (Self.occupied = TTrackState.free) then
      sfg := clAqua;

    Result := Result + '(' + train.name + ';' +
      IntToStr(ownConvert.BoolToInt(train.sdata.dir_L)) +
      IntToStr(ownConvert.BoolToInt(train.sdata.dir_S)) + ';';

    if ((train.sdata.note <> '') or (train.HasAnyHVNote())) then
      sbg := clTeal;

    if (train.areaTo = train.station) then
      sbg := clSilver;

    // predvidany odjezd
    if (train.IsPOdj(Self)) then
      predvidanyOdjezd.GetPOdjColors(train.GetPOdj(Self), sfg, sbg);

    Result := Result + ownConvert.ColorToStr(sfg) + ';' + ownConvert.ColorToStr(sbg) + ';';

    if (Self.trainMoving = i) then
      Result := Result + ownConvert.ColorToStr(clYellow) + ';'
    else
      Result := Result + '-;';

    Result := Result + IntToStr(ownConvert.BoolToInt(train.emergencyStopped)) + ';';
    Result := Result + ')';
  end;

  // predpovidana souprava
  if (Self.trainPredict <> nil) then
  begin
    // predvidany odjezd
    sfg := fg;
    sbg := bg;

    if ((Self.trainPredict.sdata.note <> '') or (Self.trainPredict.HasAnyHVNote())) then
      sbg := clTeal;

    // Do not show end station as train text is not contrast compared to background

    if (Self.trainPredict.IsPOdj(Self)) then
      predvidanyOdjezd.GetPOdjColors(Self.trainPredict.GetPOdj(Self), sfg, sbg);

    Result := Result + '(' + Self.trainPredict.name + ';' + '00;' + ownConvert.ColorToStr(sfg) + ';' +
      ownConvert.ColorToStr(sbg) + ';)';
  end;

  Result := Result + '}';
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkTrack.AcceptsMenuClick(SenderPnl: TIdContext; SenderOR: TObject; rights: TAreaRights; item: string): Boolean;
begin
  var panelData := TPanelConnData(SenderPnl.Data);
  if ((panelData.train_menu_index >= 0) and (panelData.train_menu_index < Self.trains.Count)) then
  begin
    var train: TTrain := TrainDb.trains[Self.trains[panelData.train_menu_index]];
    var trainmenu: string := train.Menu(SenderPnl, SenderOR, Self, panelData.train_menu_index);
    if (trainmenu.Contains(item)) then
      Exit(true);
  end;

  var menu: string := Self.ShowPanelMenu(SenderPnl, SenderOR, rights);
  Result := menu.Contains(item); // default = accept only those items persent in menu now
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkTrack.RealBoosterShortCircuit(): TBoosterSignal;
begin
  if (Boosters.ContainsKey(Self.m_settings.boosterId)) then
  begin
    Result := Boosters[Self.m_settings.boosterId].overload;
  end else begin
    Result := TBoosterSignal.ok;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkTrack.CanStandTrain(): Boolean;
begin
  Result := (Self.spnl.stationTrack or Self.spnl.trainPos);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkTrack.CanBeNextVB(blocks: TList<TBlk>): Boolean;
begin
  if (blocks.Contains(Self)) then
    Exit(false);

  var vbs_plus_me := TList<TBlk>.Create();
  try
    vbs_plus_me.AddRange(blocks);
    vbs_plus_me.Add(Self);

    Result := ((JCDb.IsAnyJCWithPrefix(vbs_plus_me)) or (MultiJCDb.IsAnyMJCWithPrefix(vbs_plus_me)));
  finally
    vbs_plus_me.free();
  end;
end;

function TBlkTrack.CanBeKC(blocks: TList<TBlk>): Boolean;
begin
  var withMe := TList<TBlk>.Create();
  try
    withMe.AddRange(blocks);
    withMe.Add(Self);

    Result := ((JCDb.FindJC(withMe) <> nil) or (MultiJCDb.Find(withMe) <> nil));
  finally
    withMe.Free();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkTrack.GetSignalL(): TBlk;
begin
  for var blk: TBlk in Blocks do
    if ((blk.typ = btSignal) and (TBlkSignal(blk).trackId = Self.id) and (TBlkSignal(blk).direction = THVSite.odd)) then
      Exit(blk);
  Result := nil;
end;

function TBlkTrack.GetSignalS(): TBlk;
begin
  for var blk: TBlk in Blocks do
    if ((blk.typ = btSignal) and (TBlkSignal(blk).trackId = Self.id) and (TBlkSignal(blk).direction = THVSite.even))
    then
      Exit(blk);
  Result := nil;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkTrack.PstAdd(pst: TBlk);
begin
  if (Self.m_state.psts.Contains(pst)) then
    Exit();

  Self.m_state.psts.Add(pst);
  Self.Change();
end;

procedure TBlkTrack.PstRemove(pst: TBlk);
begin
  if (not Self.m_state.psts.Contains(pst)) then
    Exit();

  Self.m_state.psts.Remove(pst);
  Self.Change();
end;

function TBlkTrack.PstIsActive(): Boolean;
begin
  Self.PstCheckActive();
  for var blk: TBlk in Self.m_state.psts do
    if (TBlkPst(blk).status = pstActive) then
      Exit(true);
  Result := false;
end;

function TBlkTrack.PstIs(): Boolean;
begin
  Self.PstCheckActive();
  Result := (Self.m_state.psts.Count > 0);
end;

procedure TBlkTrack.PstCheckActive();
begin
  for var i := Self.m_state.psts.Count-1 downto 0 do
    if (TBlkPst(Self.m_state.psts[i]).status <= pstOff) then
      Self.PstRemove(self.m_state.psts[i]);
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
