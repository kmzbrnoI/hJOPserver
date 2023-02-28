unit BlockRailway;

{
  RAILWAY technological block definition (trať).

  Tento blok se na panelu nevyskytuje - slouzi pouze jako rodic dvou uvazek.
  U bloku trati je zajisteno, ze existuji a jsou typu TBlkLinker.
  Bloky, ktere tomuto nevyhovuji, jsou po startu odstraneny.
}

interface

uses IniFiles, Block, Menus, AreaDb, SysUtils, Classes, JsonDataObjects,
  Generics.Collections, Train, BlockRailwayTrack;

type
  TRailwayType = (permanent = 0, request = 2);
  TRailwayDirection = (disabled = -1, no = 0, AtoB = 1, BtoA = 2);
  TRailwaySignals = (autoblok = 0, hradlo = 1);

  TBlkRailwaySettings = record
    linkerA, linkerB: Integer;
    rType: TRailwayType;
    signals: TRailwaySignals;
    trackIds: TList<Integer>;
  end;

  TBlkRailwayEFull = class(Exception);
  TBlkRailwayETimeNotDefined = class(Exception);

  TBlkRailwayTrain = class
  private
    mTime: TTime;
    mTimeDefined: Boolean;

    function GetTime(): TTime;
    procedure SetTime(time: TTime);
    function GeTTrain(): TTrain;

  public
    traini: Integer; // index of a train
    predict: Boolean;

    constructor Create(Train: Integer); overload;
    constructor Create(Train: Integer; time: TTime; predict: Boolean = false); overload;
    function IsTimeDefined(): Boolean;
    procedure UndefTime();
    property time: TTime read GetTime write SetTime;
    property Train: TTrain read GeTTrain;

    function SerializeForPanel(railway: TBlk; trainPredict: Boolean = false): string;
  end;

  TBlkRailwayState = record
    zaver: Boolean;
    direction: TRailwayDirection;
    request: Boolean;
    trains: TObjectList<TBlkRailwayTrain>;
    trainPredict: TBlkRailwayTrain;
    BP: Boolean;
  end;

  TTrainTrack = record
    railwayIndex: Integer;
    track: TBlk;
  end;

  TBlkRailway = class(TBlk)
  const
    _def_railway_state: TBlkRailwayState = (zaver: false; direction: disabled; request: false; trainPredict: nil;
      BP: false;);

  private
    m_settings: TBlkRailwaySettings;
    m_state: TBlkRailwayState;
    file_direction: TRailwayDirection;
    // tady si ukladame reference na skutecne bloky, ktere si vytvarime az pri prvnim pristupu k uvazce pres \uvazkaA a \uvazkaB
    m_linkerA, m_linkerB: TBlk;
    // fNavLichy je navestidlo u stanice blize pocatku trati, fNavSudy navestidlo u stanice blize konce trati
    m_signalA, m_signalB: TBlk; // analogicky funguji krajni navestidla trati, viz \navLichy a \navSudy
    m_tracks: TList<TBlkRT>;

    function GetLinkerA(): TBlk;
    function GetLinkerB(): TBlk;

    function GetOccupied(): Boolean;
    function GetDepartureForbidden(): Boolean;
    function GetRBP(): Boolean;
    function GetEmergencyLock(): Boolean;

    procedure SetDirection(direction: TRailwayDirection);
    procedure SetZaver(zaver: Boolean);
    procedure SetRequest(request: Boolean);
    procedure SetTrainPredict(train: TBlkRailwayTrain);

    procedure SetBP(state: Boolean);

    function GetSignalA(): TBlk;
    function GetSignalB(): TBlk;

    // zkontroluje existenci vsech bloku, ktere maji v trati byt; nevalidni bloky z trati smaze a provede o tom zapis do LOGu
    procedure CheckTUExist();
    // inicializuje tratove useky - oznami jim, ze jsou v trati a provede mnoho dalsich veci, viz telo metody
    procedure InitTUs();
    // resetuje stav tratoveho useku; tratovy usek zapomene, ze je v nejake trati a stane se neutralnim tratovym usekem, ktery nic nevi
    procedure ResetTUs();

    // vrati, jestli jsou vsechny tratove useky pripraveny pro vjezd soupravy, pouziva se pri zjistovani toho, jestli je mozne obratit smer trati
    function GetReady(): Boolean;
    function GetTrainIndex(Train: TTrain): Integer;
    function TrainTUsCount(Train: TTrain): Integer;

    function GetLastTrack(): TBlk; overload;
    function GetLockout(): Boolean;
    function IsFree(): Boolean; // free = able to change railway direction (definition from "releovka")

  public
    constructor Create(index: Integer);
    destructor Destroy(); override;

    procedure LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile); override;
    procedure SaveData(ini_tech: TMemIniFile; const section: string); override;
    procedure SaveState(ini_stat: TMemIniFile; const section: string); override;

    procedure Enable(); override;
    procedure Disable(); override;
    procedure Reset(); override;
    procedure AfterLoad(); override;

    procedure Change(now: Boolean = false); override;
    procedure ChangeFromLinker(Sender: TBlk);
    procedure ChangeTracks();

    // ----- Railway specific functions -----

    function GetSettings(): TBlkRailwaySettings;
    procedure SetSettings(data: TBlkRailwaySettings);

    function IsFirstLinker(uv: TBlk): Boolean;
    procedure TrainChangeOR(Train: TTrain); overload;
    procedure TrainChangeOR(Train: TTrain; smer: TRailwayDirection); overload;

    procedure AddTrain(Train: TTrain); overload;
    procedure AddTrain(Train: TBlkRailwayTrain); overload;
    function GetTrainsList(separator: Char): string;
    procedure RemoveTrain(Train: TTrain);

    function IsTrain(Train: TTrain; predict: Boolean = true): Boolean;
    function IsTrainInAnyTU(Train: TTrain): Boolean;
    function IsTrainInMoreTUs(Train: TTrain): Boolean;

    procedure CallChangeToTU();
    procedure UpdateTrainPredict(call_prediction: Boolean = true);
    function SignalCounterDirection(): Integer;

    function SameUserBothLinkers(): Boolean; // vraci true prave tehdy, kdyz obe uvazky kontrlu stejny uzivatel
    // kdyz je true, do trati neni potreba zadat

    function ChangesTrainDir(): Boolean; // vraci true prave tehdy, kdyz se v trati meni smer soupravy
    function GetTrainTrack(Train: TTrain): TBlk;
    function GetLastTrack(smer: TRailwayDirection): TBlk; overload;
    function HasAutoblokSignal(blk: TBlk): Boolean;

    procedure GetPtData(json: TJsonObject; includeState: Boolean); override;
    procedure GetPtState(json: TJsonObject); override;

    procedure RecalcTracks();

    property linkerA: TBlk read GetLinkerA; // blok uvazky blize zacatku trati
    property linkerB: TBlk read GetLinkerB; // blok uvazky blize konci trati
    property RBPCan: Boolean read GetRBP;
    // vraci, jestli v trati doslo k poruse uplne blokove podminky, resp. jesli je mozno ji zrusit

    property state: TBlkRailwayState read m_state;
    property occupied: Boolean read GetOccupied;
    property direction: TRailwayDirection read m_state.direction write SetDirection;
    property zaver: Boolean read m_state.zaver write SetZaver;
    property departureForbidden: Boolean read GetDepartureForbidden;
    property emLock: Boolean read GetEmergencyLock;
    property request: Boolean read m_state.request write SetRequest;
    property BP: Boolean read m_state.BP write SetBP;
    property trainPredict: TBlkRailwayTrain read m_state.trainPredict write SetTrainPredict;
    property lastTrack: TBlk read GetLastTrack;
    property lockout: Boolean read GetLockout;
    property tracks: TList<TBlkRT> read m_tracks;
    property railwayFree: Boolean read IsFree;

    // vrati hranicni navestidla
    property signalA: TBlk read GetSignalA; // hranicni navestidlo trati blize zacatku trati
    property signalB: TBlk read GetSignalB; // hranicni navestidlo trati blize konci trati

    property ready: Boolean read GetReady;
    // jsou vsechny tratove useky "ready"? typicky se pouziva jako flag moznosti zmeny smeru trati
    property rType: TRailwayType read m_settings.rType;
    property signals: TRailwaySignals read m_settings.signals;

  end;

  /// /////////////////////////////////////////////////////////////////////////////

implementation

uses GetSystems, TechnologieRCS, BlockDb, Area, BlockSignal, Logging,
  TJCDatabase, fMain, TCPServerPanel, BlockTrack, BlockLinker, TrainDb, THVDatabase,
  appEv, timeHelper, ownConvert, Graphics;

constructor TBlkRailway.Create(index: Integer);
begin
  inherited;

  Self.m_globSettings.typ := btRailway;
  Self.m_state := _def_railway_state;

  Self.m_linkerA := nil;
  Self.m_linkerB := nil;

  Self.m_signalA := nil;
  Self.m_signalB := nil;

  Self.m_settings.trackIds := TList<Integer>.Create();
  Self.m_state.trains := TObjectList<TBlkRailwayTrain>.Create();
  Self.m_tracks := TList<TBlkRT>.Create();
end;

destructor TBlkRailway.Destroy();
begin
  Self.m_state.trains.Free();
  Self.m_settings.trackIds.Free();
  Self.m_tracks.Free();

  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkRailway.LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile);
var index: Integer;
begin
  inherited LoadData(ini_tech, section, ini_rel, ini_stat);

  Self.m_settings.linkerA := ini_tech.ReadInteger(section, 'uvazkaA', -1);
  Self.m_settings.linkerB := ini_tech.ReadInteger(section, 'uvazkaB', -1);
  index := ini_tech.ReadInteger(section, 'zabzar', 0);
  if (index = 1) then
    index := 2;
  Self.m_settings.rType := TRailwayType(index);
  Self.m_settings.signals := TRailwaySignals(ini_tech.ReadInteger(section, 'navestidla', 0));

  Self.file_direction := TRailwayDirection(ini_stat.ReadInteger(section, 'smer', 1));
  Self.m_state.BP := ini_stat.ReadBool(section, 'BP', false);

  var strs: TStrings := TStringList.Create();
  try
    ExtractStrings([',', ';'], [], PChar(ini_stat.ReadString(section, 'spr', '')), strs);
    Self.m_state.trains.Clear();
    for var str in strs do
    begin
      index := trains.GetTrainIndexByName(str);
      if (index > -1) then
        Self.m_state.trains.Add(TBlkRailwayTrain.Create(index));
    end;
  finally
    strs.Free();
  end;

  strs := TStringList.Create();
  try
    ExtractStrings([';', ','], [], PChar(ini_tech.ReadString(section, 'useky', '')), strs);
    Self.m_settings.trackIds.Clear();
    for var str in strs do
    begin
      try
        Self.m_settings.trackIds.Add(StrToInt(str));
      except

      end;
    end;
  finally
    strs.Free();
  end;
end;

procedure TBlkRailway.SaveData(ini_tech: TMemIniFile; const section: string);
begin
  inherited SaveData(ini_tech, section);

  ini_tech.WriteInteger(section, 'uvazkaA', Self.m_settings.linkerA);
  ini_tech.WriteInteger(section, 'uvazkaB', Self.m_settings.linkerB);
  ini_tech.WriteInteger(section, 'zabzar', Integer(Self.m_settings.rType));
  ini_tech.WriteInteger(section, 'navestidla', Integer(Self.m_settings.signals));
  ini_tech.WriteString(section, 'useky', SerializeIntList(Self.m_settings.trackIds));
end;

procedure TBlkRailway.SaveState(ini_stat: TMemIniFile; const section: string);
begin
  ini_stat.WriteInteger(section, 'smer', Integer(Self.file_direction));

  if (Self.m_state.BP) then
    ini_stat.WriteBool(section, 'BP', Self.m_state.BP);

  var str: string := '';
  for var rTrain: TBlkRailwayTrain in Self.m_state.trains do
    str := str + rTrain.Train.name + ';';

  if (str <> '') then
    ini_stat.WriteString(section, 'spr', str);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkRailway.Enable();
begin
  Self.m_state.direction := Self.file_direction;
  Self.Change();
end;

procedure TBlkRailway.Disable();
begin
  if (Self.direction <> TRailwayDirection.disabled) then
    Self.file_direction := Self.direction;
  Self.trainPredict := nil;
  Self.m_state.direction := TRailwayDirection.disabled;
  Self.Change(true);
end;

procedure TBlkRailway.Reset();
begin
  Self.zaver := false;
  Self.request := false;
  Self.trainPredict := nil;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkRailway.AfterLoad();
begin
  Self.RecalcTracks();
  Self.CheckTUExist();
  Self.InitTUs();

  if (Self.linkerA = nil) then
    Self.Log('Není návaznost na úvazku A', llError);
  if (Self.linkerB = nil) then
    Self.Log('Není návaznost na úvazku B', llError);
end;

// change je vyvolano i pri zmene obsazenosti jakehokoliv useku v trati
procedure TBlkRailway.Change(now: Boolean = false);
begin
  inherited Change(now);

  if ((Self.request) and (Self.occupied)) then
    Self.request := false;

  (Self.linkerA as TBlkLinker).ChangeFromTrat();
  (Self.linkerB as TBlkLinker).ChangeFromTrat();

  inherited Update();
end;

procedure TBlkRailway.ChangeFromLinker(Sender: TBlk);
begin
  if (Sender = Self.linkerA) then
    (Self.linkerB as TBlkLinker).ChangeFromTrat();
  if (Sender = Self.linkerB) then
    (Self.linkerA as TBlkLinker).ChangeFromTrat();

  if ((Self.request) and (Self.occupied)) then
    Self.request := false;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkRailway.ChangeTracks();
begin
  for var blkRT: TBlkRT in Self.tracks do
    blkRT.Change();
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkRailway.GetOccupied(): Boolean;
begin
  for var blkRT: TBlkRT in Self.tracks do
    if (blkRT.occupied = TTrackState.occupied) then
      Exit(true);
  Result := false;
end;

function TBlkRailway.GetDepartureForbidden(): Boolean;
begin
  if ((Self.linkerA = nil) or (Self.linkerB = nil)) then
    Exit(false);
  if (((Self.linkerA as TBlkLinker).departureForbidden) or ((Self.linkerB as TBlkLinker).departureForbidden)) then
    Result := true
  else
    Result := false;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkRailway.SetDirection(direction: TRailwayDirection);
begin
  Self.m_state.direction := direction;

  // zrusime blokovou podminku
  Self.BP := false;

  Self.Change();
  Self.CallChangeToTU();
end;

procedure TBlkRailway.SetZaver(zaver: Boolean);
begin
  if (Self.m_state.zaver <> zaver) then
  begin
    Self.m_state.zaver := zaver;
    Self.trainPredict := nil;
    Self.Change();
  end else begin
    Self.trainPredict := nil;
  end;
end;

procedure TBlkRailway.SetRequest(request: Boolean);
begin
  if (Self.request = request) then
    Exit();

  // tady se resi prehravani zvuku
  try
    var linker: TBLkLinker := nil;
    if ((Self.m_linkerA as TBlkLinker).request) then
      linker := (Self.m_linkerB as TBlkLinker)
    else if ((Self.m_linkerB as TBlkLinker).request) then
      linker := (Self.m_linkerA as TBlkLinker);

    if ((linker <> nil) and (request <> Self.m_state.request)) then
    begin
      if (request) then
      begin
        for var area: TArea in linker.areas do
          Area.railwayReqBlkCnt := Area.railwayReqBlkCnt + 1;
      end else begin
        for var area: TArea in linker.areas do
          Area.railwayReqBlkCnt := Area.railwayReqBlkCnt - 1;
      end;
    end;
  except
    on E: Exception do
      AppEvents.LogException(E, 'SetRequest');
  end;

  Self.m_state.request := request;
  Self.Change();
end;

procedure TBlkRailway.SetTrainPredict(Train: TBlkRailwayTrain);
begin
  if (Self.m_state.trainPredict = Train) then
    Exit();

  if (Self.m_state.trainPredict <> nil) then
    FreeAndNil(Self.m_state.trainPredict);

  if (Train <> nil) then
    Self.m_state.trainPredict := Train;

  Self.Change();
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkRailway.GetSettings(): TBlkRailwaySettings;
begin
  Result := Self.m_settings;
end;

procedure TBlkRailway.SetSettings(data: TBlkRailwaySettings);
begin
  Self.ResetTUs();
  if (data.trackIds <> Self.m_settings.trackIds) then
    Self.m_settings.trackIds.Free();

  Self.m_settings := data;

  if (not Assigned(data.trackIds)) then
    data.trackIds := TList<Integer>.Create();
  Self.RecalcTracks();
  Self.CheckTUExist();
  Self.InitTUs();

  // zrusim uvazku, aby se prepocitala
  Self.m_linkerA := nil;
  Self.m_linkerB := nil;

  Self.Change();
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkRailway.IsFirstLinker(uv: TBlk): Boolean;
begin
  if (uv = Self.linkerA) then
    Result := true
  else
    Result := false;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkRailway.GetLinkerA(): TBlk;
begin
  if (Self.m_linkerA = nil) then
    Self.m_linkerA := Blocks.GetBlkLinkerByID(Self.m_settings.linkerA);
  Result := Self.m_linkerA;
end;

function TBlkRailway.GetLinkerB(): TBlk;
begin
  if (Self.m_linkerB = nil) then
    Self.m_linkerB := Blocks.GetBlkLinkerByID(Self.m_settings.linkerB);
  Result := Self.m_linkerB;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkRailway.GetRBP(): Boolean;
begin
  for var blkRT: TBlkRT in Self.tracks do
    if (blkRT.bpError) then
      Exit(true);
  Result := false;
end;

/// /////////////////////////////////////////////////////////////////////////////
// zavedeni / zruseni blokove podminky
// zavest blokovou podminky lze vzdy, zrusit ji lze jen tehdy, kdyz
// na zadnem tratovem useku neni blokova podminka

procedure TBlkRailway.SetBP(state: Boolean);
begin
  if (Self.BP = state) then
    Exit();

  if (state) then
  begin
    Self.m_state.BP := true;
  end else begin
    for var blkRT: TBlkRT in Self.tracks do
      if (blkRT.bpInBlk) then
        Exit();
    Self.m_state.BP := false;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkRailway.AddTrain(Train: TTrain);
begin
  Self.AddTrain(TBlkRailwayTrain.Create(Train.index, timeHelper.hJOPnow()));
end;

procedure TBlkRailway.AddTrain(Train: TBlkRailwayTrain);
begin
  Self.m_state.trains.Add(Train);
  if (Train <> Self.trainPredict) then
    Self.trainPredict := nil // will also Free
  else
    Self.m_state.trainPredict := nil; // will not Free

  if (not Train.IsTimeDefined()) then
    Train.time := timeHelper.hJOPnow();

  Log('Trať ' + Self.m_globSettings.name + ' : přidána souprava ' + Train.Train.name, llInfo, lsTrainMove);

  Self.Change();
end;

procedure TBlkRailway.RemoveTrain(Train: TTrain);
var toChange: Boolean;
begin
  toChange := false;

  if ((Self.trainPredict <> nil) and (Self.trainPredict.Train = Train)) then
  begin
    Self.trainPredict := nil;
    toChange := true;
  end;

  if (Self.IsTrain(Train)) then
  begin
    Self.m_state.trains.Delete(Self.GetTrainIndex(Train));
    Log('Trať ' + Self.m_globSettings.name + ' : smazána souprava ' + Train.name, llInfo, lsTrainMove);
    toChange := true;
  end;

  if (toChange) then
    Self.Change();
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkRailway.GetTrainsList(separator: Char): string;
begin
  Result := '';

  for var train: TBlkRailwayTrain in Self.m_state.trains do
    Result := Result + train.SerializeForPanel(Self) + separator;

  if (Self.trainPredict <> nil) then
    Result := Result + Self.trainPredict.SerializeForPanel(Self, true) + separator;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkRailway.TrainChangeOR(Train: TTrain);
begin
  Self.TrainChangeOR(Train, Self.direction);
end;

procedure TBlkRailway.TrainChangeOR(Train: TTrain; smer: TRailwayDirection);
begin
  case (smer) of
    TRailwayDirection.AtoB:
      begin
        if ((Self.linkerB as TBlkLinker).areas.Count > 0) then
          Train.station := (Self.linkerB as TBlkLinker).areas[0]
        else
          Train.station := nil;
      end; // AtoB
    TRailwayDirection.BtoA:
      begin
        if ((Self.linkerA as TBlkLinker).areas.Count > 0) then
          Train.station := (Self.linkerA as TBlkLinker).areas[0]
        else
          Train.station := nil;
      end; // BtoA
  end; // case

  Log('Trať ' + Self.m_globSettings.name + ' : souprava ' + Train.name + ' : stanice změněna na ' +
    (Train.station as TArea).name, llInfo, lsTrainMove);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkRailway.GetEmergencyLock(): Boolean;
begin
  Result := (Self.linkerA as TBlkLinker).emLock or (Self.linkerB as TBlkLinker).emLock;
end;

/// /////////////////////////////////////////////////////////////////////////////

// vrati hranicni navestidlo trati na jejim zacatku
// hranicni navestidlo musi
// - byt ve stejne OR, jako uvazka
// - mit blok_pred_id hranicni blok trati
// - nebyl navestidlo autobloku druheho useku trati
function TBlkRailway.GetSignalA(): TBlk;
var blkRT: TBlkRT;
begin
  if ((Self.tracks.Count = 0) or (Self.linkerA = nil) or (Self.linkerA.areas.Count = 0)) then
    Exit(nil);

  if ((Self.m_signalA = nil) or ((Self.m_signalA as TBlkSignal).trackId <> Self.tracks[0].id)) then
  begin
    if (Self.tracks.Count > 1) then
      blkRT := Self.tracks[1]
    else
      blkRT := nil;

    for var blk: TBlk in Blocks do
    begin
      if (blk.typ <> btSignal) then
        continue;
      if ((TBlkSignal(blk).trackId = Self.tracks[0].id) and (blk.areas.Count > 0) and (blk.areas[0] = Self.linkerA.areas[0]) and
        ((blkRT = nil) or (blk.id <> blkRT.GetSettings.signalLid))) then
      begin
        Self.m_signalA := blk;
        break;
      end;
    end;
  end;

  Result := Self.m_signalA;
end;

// vrati hranicni navestidlo trati na jejim konci
function TBlkRailway.GetSignalB(): TBlk;
var blkRT: TBlkRT;
begin
  if ((Self.tracks.Count = 0) or (Self.linkerB = nil) or (Self.linkerB.areas.Count = 0)) then
    Exit(nil);

  if ((Self.m_signalB = nil) or ((Self.m_signalB as TBlkSignal).trackId <> Self.tracks[Self.tracks.Count - 1].id)) then
  begin
    if (Self.tracks.Count > 1) then
      blkRT := Self.tracks[Self.tracks.Count - 2]
    else
      blkRT := nil;

    for var blk: TBlk in Blocks do
    begin
      if (blk.typ <> btSignal) then
        continue;
      if ((TBlkSignal(blk).trackId = Self.tracks[Self.tracks.Count - 1].id) and (blk.areas.Count > 0) and (blk.areas[0] = Self.linkerB.areas[0])
        and ((blkRT = nil) or (blk.id <> blkRT.GetSettings.signalSid))) then
      begin
        Self.m_signalB := blk;
        break;
      end;
    end;
  end;

  Result := Self.m_signalB;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkRailway.IsTrain(Train: TTrain; predict: Boolean = true): Boolean;
begin
  Result := ((Self.GetTrainIndex(Train) > -1) or ((predict) and (Self.trainPredict <> nil) and
    (Self.trainPredict.Train = Train)));
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkRailway.GetTrainIndex(Train: TTrain): Integer;
begin
  for var i: Integer := 0 to Self.m_state.trains.Count - 1 do
    if (Self.m_state.trains[i].Train = Train) then
      Exit(i);
  Exit(-1);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkRailway.CheckTUExist();
begin
  for var i: Integer := Self.m_settings.trackIds.Count - 1 downto 0 do
  begin
    var track := Blocks.GetBlkByID(Self.m_settings.trackIds[i]);
    if ((track = nil) or (track.typ <> btRT)) then
    begin
      Self.Log('Obsahuje referenci na TU ID ' + IntToStr(Self.m_settings.trackIds[i]) +
        ', tento blok ale bud neexistuje, nebo neni typu TU, odstranuji referenci', llError);
      Self.m_settings.trackIds.Delete(i);
      continue;
    end;
    if ((TBlkRT(track).inRailway <> -1) and (TBlkRT(track).inRailway <> Self.id)) then
    begin
      Self.Log('TU ID ' + IntToStr(Self.m_settings.trackIds[i]) + ' jiz referuje na trat ID ' +
        IntToStr(TBlkRT(track).inRailway) + ', odstranuji referenci', llError);
      Self.m_settings.trackIds.Delete(i);
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkRailway.TrainTUsCount(Train: TTrain): Integer;
begin
  Result := 0;
  for var blkRT: TBlkRT in Self.tracks do
    if (blkRT.IsTrain(Train)) then
      Inc(Result);
end;

function TBlkRailway.IsTrainInAnyTU(Train: TTrain): Boolean;
begin
  Result := (Self.TrainTUsCount(Train) > 0);
end;

function TBlkRailway.IsTrainInMoreTUs(Train: TTrain): Boolean;
begin
  Result := (Self.TrainTUsCount(Train) > 1);
end;

/// /////////////////////////////////////////////////////////////////////////////
// vytvoreni navaznosti mezi tratovymi useky, sekcemi tratovych useku a
// navestidly autobloku

procedure TBlkRailway.InitTUs();
var
  lRT, sRT: TBlkRT;
  blk, sMaster: TBlkRT;
begin
  // 1) nejprve vytvorime navaznosti mezi tratovymi useky:
  // Kazdemu TU rekneme, jaky TU je vedle neho v lichem smeru (bliz zacatku trati)
  // a jaky TU je vedle enho v sudem smeru (bliz konci trati).
  // Krajni TU maji referenci na dalsi TU "nil".

  if (Self.tracks.Count = 0) then
    Exit();

  lRT := nil;
  blk := Self.tracks[0];
  if (Self.tracks.Count > 1) then
    sRT := Self.tracks[1]
  else
    sRT := nil;

  for var i: Integer := 0 to Self.tracks.Count - 2 do
  begin
    blk.lRT := lRT;
    blk.sRT := sRT;
    lRT := blk;
    blk := sRT;
    if (i < Self.tracks.Count - 2) then
      sRT := Self.tracks[i + 2];
  end;

  // posledni TU:
  blk.lRT := lRT;
  blk.sRT := nil;

  /// //////////////////////////////////////////////////////////////////////////
  // 2) Kazdemu TU priradime jeho Section Master a Section Masteru priradime
  // jeho useky.

  // a) v lichem smeru: jdeme od zacatku trati ke konci
  sMaster := Self.tracks[0];
  var tracks: TList<TBlkRT> := sMaster.lsectTracks;
  for blk in Self.tracks do
  begin
    // useku take priradime, ze je v nasi trati
    (blk as TBlkRT).inRailway := Self.id;

    if (blk.GetSettings().signalLid <> -1) then
    begin
      sMaster := blk;
      tracks := sMaster.lsectTracks;
    end;
    blk.lsectMaster := sMaster;
    tracks.Add(blk);
  end;

  // b) v sudem smeru: jdeme od konce trati k zacatku
  sMaster := Self.tracks[Self.tracks.Count - 1];
  tracks := sMaster.ssectTracks;
  for var i: Integer := Self.tracks.Count - 1 downto 0 do
  begin
    blk := Self.tracks[i];
    if (blk.GetSettings().signalSid <> -1) then
    begin
      sMaster := blk;
      tracks := sMaster.ssectTracks;
    end;
    blk.ssectMaster := sMaster;
    tracks.Add(blk);
  end;

  /// //////////////////////////////////////////////////////////////////////////
  // 3) inicializujeme navaznosti navestidel

  for blk in Self.tracks do
    blk.CreateNavRefs();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkRailway.ResetTUs();
begin
  for var blkRT: TBlkRT in Self.tracks do
    blkRT.RemoveTURefs();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkRailway.CallChangeToTU();
begin
  for var blkRT: TBlkRT in Self.tracks do
    blkRT.ChangeFromRailway();
end;

/// /////////////////////////////////////////////////////////////////////////////
// aktualizace predpovidane soupravy na posledni usek trati
// volano pri uvolneni posledniho useku trati, nebo RBP

procedure TBlkRailway.UpdateTrainPredict(call_prediction: Boolean = true);
begin
  if ((Self.direction <> TRailwayDirection.AtoB) and (Self.direction <> TRailwayDirection.BtoA)) then
    Exit();
  if (Self.tracks.Count = 0) then
    Exit();

  case (Self.direction) of
    TRailwayDirection.AtoB:
      begin
        var last: TBlkRT := Self.tracks[Self.tracks.Count - 1];
        last.trainPredict := nil;
        if (last.IsTrain()) then
          Exit();
        for var i: Integer := Self.tracks.Count - 2 downto 0 do
        begin
          var blk: TBlkRT := Self.tracks[i];
          if (blk.IsTrain()) then
          begin
            last.trainPredict := blk.Train;
            break;
          end;
          if (blk.trainPredict <> nil) then
          begin
            last.trainPredict := blk.trainPredict;
            break;
          end;
          if ((blk.signalCover <> nil) and (TBlkSignal(blk.signalCover).signal = ncStuj)) then
          begin
            Blocks.TrainPrediction(Self.signalB as TBlkSignal);
            Exit();
          end;
        end;

        if ((last.trainPredict = nil) and (Self.trainPredict <> nil)) then
          last.trainPredict := Self.trainPredict.Train;
        if ((call_prediction) and (Self.signalB <> nil)) then
          Blocks.TrainPrediction(Self.signalB as TBlkSignal);
      end;

    TRailwayDirection.BtoA:
      begin
        var last: TBlkRT := Self.tracks[0];
        last.trainPredict := nil;
        if (last.IsTrain()) then
          Exit();
        for var i: Integer := 1 to Self.tracks.Count - 1 do
        begin
          var blk: TBlkRT := Self.tracks[i];
          if (blk.IsTrain()) then
          begin
            last.trainPredict := blk.Train;
            break;
          end;
          if (blk.trainPredict <> nil) then
          begin
            last.trainPredict := blk.trainPredict;
            break;
          end;
          if ((blk.signalCover <> nil) and (TBlkSignal(blk.signalCover).signal = ncStuj)) then
          begin
            Blocks.TrainPrediction(Self.signalA as TBlkSignal);
            Exit();
          end;
        end;

        if ((last.trainPredict = nil) and (Self.trainPredict <> nil)) then
          last.trainPredict := Self.trainPredict.Train;
        if ((call_prediction) and (Self.signalA <> nil)) then
          Blocks.TrainPrediction(Self.signalA as TBlkSignal);
      end;
  end; // case
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkRailway.GetReady(): Boolean;
begin
  for var blkId: Integer in Self.m_settings.trackIds do
  begin
    var blkRT: TBlkRT := Blocks.GetBlkRTByID(blkId);
    if ((blkRT = nil) or (not blkRT.ready)) then
      Exit(false);
  end;
  Result := true;
end;

/// /////////////////////////////////////////////////////////////////////////////
// Vraci true prave tehdy, pokud je trat na obou koncich rizena stejnym uzivatelem.
// Mazerne nenavazujeme rizeni obou koncu na konkretniho uzivatele, napriklad
// kvuli staveni ze zasobniku.

function TBlkRailway.SameUserBothLinkers(): Boolean;
begin
  if ((not Assigned(Self.linkerA)) or (not Assigned(Self.linkerB))) then
    Exit(false);
  if ((TBlkLinker(Self.linkerA).areas.Count <> 1) or (TBlkLinker(Self.linkerB).areas.Count <> 1)) then
    Exit(false);

  for var first: TAreaPanel in TBlkLinker(Self.linkerA).areas[0].Connected do
    if (IsWritable(first)) then
      for var second: TAreaPanel in TBlkLinker(Self.linkerB).areas[0].Connected do
        if ((first.user = second.user) and (IsWritable(second))) then
          Exit(true);

  Result := false;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkRailway.ChangesTrainDir(): Boolean;
begin
  Result := (Assigned(Self.signalA)) and (Assigned(Self.signalB)) and
    (TBlkSignal(Self.signalA).direction = TBlkSignal(Self.signalB).direction);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkRailway.GetTrainTrack(Train: TTrain): TBlk;
begin
  for var blkRT: TBlkRT in Self.tracks do
    if (blkRT.Train = Train) then
      Exit(blkRT);
  Result := nil;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkRailway.GetLastTrack(): TBlk;
begin
  Result := Self.GetLastTrack(Self.direction);
end;

function TBlkRailway.GetLastTrack(smer: TRailwayDirection): TBlk;
begin
  if (Self.tracks.Count < 1) then
    raise Exception.Create('Trať nemá žádný úsek!');

  if (smer = TRailwayDirection.AtoB) then
    Result := Self.tracks[Self.tracks.Count - 1]
  else if (smer = TRailwayDirection.BtoA) then
    Result := Self.tracks[0]
  else
    raise Exception.Create('Trať nemá žádný směr!');
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkRailway.GetLockout(): Boolean;
begin
  for var blkRT: TBlkTrack in Self.tracks do
    if (blkRT.lockout <> '') then
      Exit(true);
  Result := false;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkRailway.SignalCounterDirection(): Integer;
begin
  case (Self.signals) of
    TRailwaySignals.autoblok:
      Result := Integer(ncZhasnuto);
    TRailwaySignals.hradlo:
      Result := Integer(ncStuj);
  else
    Result := Integer(ncZhasnuto);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkRailway.HasAutoblokSignal(blk: TBlk): Boolean;
begin
  for var blkRT: TBlkRT in Self.tracks do
    if ((blk = blkRT.signalCoverL) or (blk = blkRT.signalCoverS)) then
      Exit(true);
  Result := false;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkRailway.GetPtData(json: TJsonObject; includeState: Boolean);
begin
  inherited;

  json['linkerA'] := Self.m_settings.linkerA;
  json['linkerB'] := Self.m_settings.linkerB;
  json['zabzar'] := Integer(Self.m_settings.rType);
  json['signals'] := Integer(Self.m_settings.signals);
  for var trackId: Integer in Self.m_settings.trackIds do
    json.A['tracks'].Add(trackId);

  if (includeState) then
    Self.GetPtState(json['blockState']);
end;

procedure TBlkRailway.GetPtState(json: TJsonObject);
begin
  json['lock'] := Self.zaver;
  json['direction'] := Integer(Self.direction);
  json['request'] := Self.request;

  for var train: TBlkRailwayTrain in Self.m_state.trains do
    if (not train.predict) then
      json.A['trains'].Add(train.Train.name);
  if (Self.m_state.trainPredict <> nil) then
    json['trainPredict'] := Self.m_state.trainPredict.Train;

  json['BP'] := Self.m_state.BP;
  json['free'] := Self.railwayFree;
  json['rbpcan'] := Self.RBPCan;
  json['occupied'] := Self.occupied;
  json['departureForbidden'] := Self.departureForbidden;
  json['emLock'] := Self.emLock;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkRailway.RecalcTracks();
begin
  Self.m_tracks.Clear();
  for var blkId: Integer in Self.m_settings.trackIds do
  begin
    var rt: TBlkRT := Blocks.GetBlkRTByID(blkId);
    if (rt <> nil) then
      Self.m_tracks.Add(rt);
  end;
end;

function TBlkRailway.IsFree(): Boolean;
begin
  Result := ((not Self.request) and (not Self.RBPCan) and (not Self.emLock) and
             (not Self.occupied) and (not Self.zaver) and (not Self.departureForbidden));
end;

/// /////////////////////////////////////////////////////////////////////////////
/// /////////////////////////////////////////////////////////////////////////////
// TBlkTraTTrain

constructor TBlkRailwayTrain.Create(Train: Integer);
begin
  inherited Create();
  Self.traini := Train;
  Self.mTimeDefined := false;
  Self.predict := false;
end;

constructor TBlkRailwayTrain.Create(Train: Integer; time: TTime; predict: Boolean = false);
begin
  inherited Create();
  Self.traini := Train;
  Self.time := time;
  Self.predict := predict;
end;

function TBlkRailwayTrain.GetTime(): TTime;
begin
  if (not Self.IsTimeDefined()) then
    raise TBlkRailwayETimeNotDefined.Create('Time not defined!');
  Result := Self.mTime;
end;

procedure TBlkRailwayTrain.SetTime(time: TTime);
begin
  Self.mTimeDefined := true;
  Self.mTime := time;
end;

function TBlkRailwayTrain.IsTimeDefined(): Boolean;
begin
  Result := Self.mTimeDefined;
end;

procedure TBlkRailwayTrain.UndefTime();
begin
  Self.mTimeDefined := false;
end;

function TBlkRailwayTrain.SerializeForPanel(railway: TBlk; trainPredict: Boolean = false): string;
var
  fg: TColor;
  bg: TColor;
begin
  fg := clWhite;
  bg := clBlack;

  // Pozor, souprava muze byt ve vice usecich a mit poruchu BP jen v jednom z nich
  var bpError := false;
  for var blkRT: TBlkRT in TBlkRailway(railway).tracks do
    if (blkRT.Train = Self.Train) and (blkRT.bpError) then
      bpError := true;

  var blk := Self.Train.front as TBlk;
  var stopsInHalt := ((blk <> nil) and (blk.typ = btRT) and (TBlkRT(blk).rtState.stopStopped));

  if (trainPredict) then
    fg := clYellow
  else if (bpError) then
    fg := clAqua
  else if ((Self.Train.speed = 0) and (not stopsInHalt)) then
    fg := clRed;

  if ((Self.Train.HasAnyHVNote()) or (Self.Train.sdata.note <> '')) then
    bg := clTeal;

  Result := Self.Train.name + '|';
  Result := Result + ownConvert.ColorToStr(fg) + '|';
  Result := Result + ownConvert.ColorToStr(bg) + '|';

  if (Self.mTimeDefined) then
    Result := Result + FormatDateTime('nn', Self.mTime);
  Result := Result + '|';
  if (Self.predict) then
    Result := Result + ownConvert.ColorToStr(clYellow) + '|'
  else
    Result := Result + ownConvert.ColorToStr(clAqua) + '|';

  Result := Result + '{';
  for var addr: Integer in Self.Train.HVs do
    Result := Result + HVDb[addr].name + '|';
  Result := Result + '}|';

  if (not trainPredict) then
    Result := Result + IntToStr(ownConvert.BoolToInt(Self.Train.emergencyStopped))
  else
    Result := Result + '-'
end;

function TBlkRailwayTrain.GeTTrain(): TTrain;
begin
  if (Self.traini = -1) then
    Exit(nil);
  Result := trains[Self.traini];
end;

/// /////////////////////////////////////////////////////////////////////////////

end.// unit
