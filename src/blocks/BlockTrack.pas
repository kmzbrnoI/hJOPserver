unit BlockTrack;

{ TRACK technological block definition. }

interface

uses IniFiles, Block, Menus, TOblsRizeni, SysUtils, Classes, Booster, houkEvent,
     IdContext, Generics.Collections, JsonDataObjects, Area, Train,
     stanicniHlaseni, changeEvent, predvidanyOdjezd, TechnologieRCS;

type
 TTrackState  = (disabled = -5, none = -1, free = 0, occupied = 1);

 ETrainFull = class(Exception);
 ETrainNotExists = class(Exception);
 EMultipleTrains = class(Exception);
 EDuplicitTrains = class(Exception);
 ERunningTrain   = class(Exception);

 TBlkTrackSettings = record
  RCSAddrs: TRCSAddrs;
  lenght: double;          // in meters
  loop: Boolean;
  boosterId: string;
  houkEvL: TObjectList<THoukEv>;  //seznam houkacich udalosti pro lichy smer
  houkEvS: TObjectList<THoukEv>;  //seznam houkacich udalosti pro sudy smer
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
  signalJCRef: TList<TBlk>;  // navestidla, ze kterych je z tohoto bloku postavena JC
  trainPredict: Integer;     // souprava, ktera je na tomto bloku predpovidana

  shortCircuit: TBoosterSignal;
  power: TBoosterSignal;
  DCC: Boolean;              // stav DCC na useku: kdyz je kontrola na SPAXu, beru SPAX, jinak se bere stav z centraly

  trainMoving: Integer;      // index soupravy, ktera se presouva, v ramci lokalniho seznamu souprav na useku; zadny presun = -1

  slowingReady: Boolean;     // pri predani soupravy do tohoto useku z trati, ci z jizdni cesty, je tento flag nastaven na true
                               // to znamena, ze je souprava pripravena ke zpomalovani; navetidlo umozni zpomaleni jen, pokud je tento flag na true
                               // po zpomaleni si navestidlo flag zrusi

  trainLost: Boolean;        // ztrata soupravy na useku se pozna tak, ze blok po urcity cas obsahuje soupravu, ale neni obsazen
  trainLostTime: Integer;    //    to je nutne pro predavani souprav mezi bloky v ramci JC (usek se uvolni, ale souprava se jeste nestihne predat
                             // pro reseni timeoutu jsou tyto 2 promenne
  shortCircSenseTime: TDateTime; // cas, kdy ma dojit k detekci zkratu
  currentHoukEv: Integer;    // index aktualni houkaci udalosti
  neprofilJCcheck: TList<Integer>; // seznam id jizdnich cest, ktere na usek uvalily podminku neprofiloveho styku
  trains: TList<Integer>;        // seznam souprav na useku ve smeru L --> S
 end;

 TBlkTrackSpnl = record
  stationTrack: Boolean;
  trackName: string;
  trainPos: Boolean;
 end;

 TBlkTrack = class(TBlk)
  const
   _def_track_state: TBlkTrackState = (
    occupied: disabled;
    occupiedOld: disabled;
    zaver: no;
    NUZ: false;
    jcEnd: no;
    note: '';
    lockout: '';
    trainPredict: -1;
    shortCircuit: TBoosterSignal.undef;
    power: TBoosterSignal.undef;
    DCC: false;
    trainMoving: -1;
    slowingReady: false;
    trainLost: false;
    currentHoukEv: -1;
   );

   _DEFAULT_MAX_TRAINS = 1;

  private
   m_state: TBlkTrackState;
   m_spnl: TBlkTrackSpnl;
   last_zes_zkrat: TBoosterSignal;  //poziva se na pamatovani posledniho stavu zkratu zesilovace pri vypnuti DCC

    procedure SetNUZ(nuz: Boolean);
    procedure SetZaver(Zaver: TZaver);
    procedure SetNote(note: string);
    procedure mSetLockout(lockout: string);
    function GetTrainPredict(): TTrain;
    procedure SetTrainPredict(train: TTrain);
    procedure SetJCend(jcEnd: TZaver);
    procedure SetTrainMoving(moving: Integer);

    procedure SetPower(state: TBoosterSignal);
    procedure SetShortCircuit(state: TBoosterSignal);
    procedure SetDCC(state: Boolean);

    procedure XTakeTrainOk(Sender: TObject; Data: Pointer);
    procedure XTakeTrainErr(Sender: TObject; Data: Pointer);

    procedure MenuNewLokClick(SenderPnl: TIdContext; SenderOR: TObject; itemindex: Integer);
    procedure MenuEditLokClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuDeleteLokClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuUVOLLokClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuVEZMILokClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuXVEZMILokClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuRegVEZMILokClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuRUCLokClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuMAUSLokClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuStitClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuVylClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuNUZStartClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuNUZStopClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuPRESUNLokClick(SenderPnl: TIdContext; SenderOR: TObject; new_state: Boolean);
    procedure MenuHLASENIOdjezdClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuHLASENIPrijezdClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuHLASENIPrujezdClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuVLOZLokClick(SenderPnl: TIdContext; SenderOR: TObject; itemindex: Integer);
    procedure MenuPOdjClick(SenderPnl: TIdContext; SenderOR: TObject);

    procedure PotvrDeleteLok(Sender: TIdContext; success: Boolean);
    procedure PotvrUvolLok(Sender: TIdContext; success: Boolean);
    procedure PotvrRegVezmiLok(Sender: TIdContext; success: Boolean);

    procedure MenuObsazClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuUvolClick(SenderPnl: TIdContext; SenderOR: TObject);

    procedure ORVylukaNull(Sender: TIdContext; success: Boolean);

    procedure LoadHoukEventToList(list: TList<THoukEv>; ini_tech: TMemIniFile; section: string; prefix: string);
    procedure CheckHoukEv();

    procedure CheckPOdjChanged();

    function GetHoukList(): TList<THoukEv>;
    function GetHoukEvEnabled(): Boolean;
    procedure SetHoukEvEnabled(state: Boolean);

    function GetSHTrain(trainLocalIndex: Integer): TSHTrain;

    procedure NeprofilObsaz();

    function GetTrainI(): Integer;
    function GetTrain(): TTrain;
    function GetTrainIL(): Integer;
    function GetTrainL(): TTrain;
    function GetTrainIS(): Integer;
    function GetTrainS(): TTrain;

    procedure ShowProperMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TAreaRights; params: string);
    function CanTrainSpeedInsert(index: Integer): Boolean;
    function IsStujForTrain(train: TTrain): Boolean;
    function RealBoosterShortCircuit(): TBoosterSignal;
    function CanStandTrain(): Boolean;
    function CanBeNextVB(vbs: TList<TObject>; start: TBlk): Boolean;
    function CanBeKC(vbs: TList<TObject>; start: TBlk): Boolean;

    function GetNavL(): TBlk;
    function GetNavS(): TBlk;

  protected
   m_settings: TBlkTrackSettings;

    procedure MenuVBClick(SenderPnl: TIdContext; SenderOR: TObject);
    function MenuKCClick(SenderPnl: TIdContext; SenderOR: TObject): Boolean;
    function MoveLok(SenderPnl: TIdContext; SenderOR: TObject; trainLocalIndex: Integer): Boolean;

  public

    eventsOnOccupy: TChangeEvents;
    eventsOnFree: TChangeEvents;
    eventsOnZaverReleaseOrAB: TChangeEvents;

    constructor Create(index: Integer);
    destructor Destroy(); override;

    procedure LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile); override;
    procedure SaveData(ini_tech: TMemIniFile; const section: string); override;
    procedure SaveStatus(ini_stat: TMemIniFile; const section: string); override;

    procedure Enable(); override;
    procedure Disable(); override;
    procedure Reset(); override;
    function UsesRCS(addr: TRCSAddr; portType: TRCSIOType): Boolean; override;

    procedure Update(); override;
    procedure Freeze(); override;
    procedure UnFreeze(); override;

    //----- Track specific functions -----

    function GetSettings(): TBlkTrackSettings;
    procedure SetSettings(data: TBlkTrackSettings);

    procedure OnBoosterChange();

    procedure SetLockout(Sender: TIDCOntext; lockout: string);

    procedure AddNeprofilJC(id: Integer);
    procedure RemoveNeprofilJC(id: Integer);
    function IsNeprofilJC(): Boolean;

    function IsTrain(): Boolean; overload;
    function IsTrain(index: Integer): Boolean; overload;
    function IsTrain(train: TTrain): Boolean; overload;
    procedure AddTrainL(index: Integer); overload; virtual;
    procedure AddTrainL(train: TTrain); overload; virtual;
    procedure AddTrainS(index: Integer); overload; virtual;
    procedure AddTrainS(train: TTrain); overload; virtual;
    procedure AddTrain(localTrainIndex: Integer; train: Integer); overload;
    procedure AddTrain(localTrainIndex: Integer; train: TTrain); overload;
    procedure RemoveTrains(); virtual;
    procedure RemoveTrain(index: Integer); overload; virtual;
    procedure RemoveTrain(train: TTrain); overload; virtual;
    function TrainsFull(): Boolean;

    function IsTrainMoving(): Boolean;
    procedure ClearPOdj();
    procedure PropagatePOdjToRailway();

    procedure MenuSOUPRAVA(SenderPnl: TIdContext; SenderOR: TObject; trainLocalI: Integer);

    property state: TBlkTrackState read m_state;
    property spnl: TBlkTrackSpnl read m_spnl;

    property occupied: TTrackState read m_state.occupied;
    property NUZ: Boolean read m_state.NUZ write SetNUZ;
    property zaver: TZaver read m_state.Zaver write SetZaver;
    property note: string read m_state.note write SetNote;
    property lockout: string read m_state.lockout write mSetLockout;
    property trainPredict: TTrain read GetTrainPredict write SetTrainPredict;
    property jcEnd: TZaver read m_state.jcEnd write SetJCend;
    property signalJCRef: TList<TBlk> read m_state.signalJCRef write m_state.signalJCRef;
    property sectionsState: TList<TTrackState> read m_state.sectionsOccupied;
    property signalL: TBlk read GetNavL;  // warning: slow getter!
    property signalS: TBLk read GetNavS;  // warning: slow getter!

    property trainI: Integer read GeTTrainI;
    property train: TTrain read GeTTrain;
    property trainIL: Integer read GeTTrainIL;
    property trainL: TTrain read GeTTrainL;
    property trainIS: Integer read GeTTrainIS;
    property trainSudy: TTrain read GeTTrainS;
    property trains: TList<Integer> read m_state.trains;

    property shortCircuit: TBoosterSignal read m_state.shortCircuit write SetShortCircuit;
    property power: TBoosterSignal read m_state.power write SetPower;
    property DCC: Boolean read m_state.DCC write SetDCC;

    property trainMoving: Integer read m_state.trainMoving write SetTrainMoving;
    property slowingReady: Boolean read m_state.slowingReady write m_state.slowingReady;
    property houkEvEnabled: Boolean read GetHoukEvEnabled write SetHoukEvEnabled;

    //GUI:

    procedure PanelMenuClick(SenderPnl: TIdContext; SenderOR: TObject; item: string; itemindex: Integer); override;

    function ShowPanelMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TAreaRights): string; override;
    procedure PanelClick(SenderPnl: TIdContext; SenderOR: TObject; Button: TPanelButton; rights: TAreaRights; params: string = ''); override;
    procedure POdjChanged(trainId: Integer; var podj: TPOdj);
    function GetTrainMenu(SenderPnl: TIdContext; SenderOR: TObject; trainLocalI: Integer): string;
    function PanelStateString(): string; override;

    //PT:

    procedure GetPtData(json: TJsonObject; includeState: Boolean); override;
    procedure GetPtState(json: TJsonObject); override;
    procedure PutPtState(reqJson: TJsonObject; respJson: TJsonObject); override;

 end;

////////////////////////////////////////////////////////////////////////////////

implementation

uses GetSystems, BlockDb, BlockSignal, Logging, RCS, ownStrUtils, Diagnostics,
    TJCDatabase, fMain, TCPServerOR, BlockRailway, TrainDb, THVDatabase, Math,
    Trakce, THnaciVozidlo, BlockRailwayTrack, BoosterDb, appEv,
    stanicniHlaseniHelper, TechnologieJC, PTUtils, RegulatorTCP, TCPORsRef,
    Graphics, ownConvert, TechnologieTrakce, TMultiJCDatabase;

constructor TBlkTrack.Create(index: Integer);
begin
 inherited Create(index);

 Self.m_globSettings.typ := btTrack;
 Self.m_state := _def_track_state;

 Self.eventsOnOccupy := TChangeEvents.Create();
 Self.eventsOnFree  := TChangeEvents.Create();
 Self.eventsOnZaverReleaseOrAB := TChangeEvents.Create();

 Self.m_settings.houkEvL := TObjectList<THoukEv>.Create();
 Self.m_settings.houkEvS := TObjectList<THoukEv>.Create();

 Self.m_settings.maxTrains := _DEFAULT_MAX_TRAINS;

 Self.m_state.neprofilJCcheck := TList<Integer>.Create();
 Self.m_state.trains := TList<Integer>.Create();
 Self.m_state.signalJCRef := TList<TBlk>.Create();
 Self.m_state.sectionsOccupied := TList<TTrackState>.Create();
end;

destructor TBlkTrack.Destroy();
begin
 if (Assigned(Self.m_settings.houkEvL)) then
   Self.m_settings.houkEvL.Free();

 if (Assigned(Self.m_settings.houkEvS)) then
   Self.m_settings.houkEvS.Free();

 Self.eventsOnOccupy.Free();
 Self.eventsOnFree.Free();
 Self.eventsOnZaverReleaseOrAB.Free();

 Self.m_state.neprofilJCcheck.Free();
 Self.m_state.trains.Free();
 Self.m_state.signalJCRef.Free();
 Self.m_state.sectionsOccupied.Free();

 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrack.LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile);
var strs: TStrings;
    s: string;
    trainIndex: Integer;
begin
 inherited LoadData(ini_tech, section, ini_rel, ini_stat);

 Self.m_settings.RCSAddrs := Self.LoadRCS(ini_tech, section);
 Self.m_settings.lenght := ini_tech.ReadFloat(section,'delka',0);
 Self.m_settings.boosterId := ini_tech.ReadString(section,'zesil','');
 Self.m_settings.loop := ini_tech.ReadBool(section, 'smc', false);
 Self.m_settings.maxTrains := ini_tech.ReadInteger(section, 'maxSpr', _DEFAULT_MAX_TRAINS);

 if (Boosters[Self.m_settings.boosterId] = nil) then
   writelog('WARNING: Blok '+Self.name + ' ('+IntToStr(Self.id)+
            ') nemá návaznost na validní zesilovač', WR_ERROR);

 Self.m_state.note := ini_stat.ReadString(section, 'stit', '');
 Self.m_state.lockout := ini_stat.ReadString(section, 'vyl' , '');

 strs := TStringList.Create();
 try
   Self.m_state.trains.Clear();
   ExtractStringsEx([','], [], ini_stat.ReadString(section, 'spr' , ''), strs);
   for s in strs do
    begin
     trainIndex := TrainDb.Trains.GetTrainIndexByName(s);
     if (trainIndex > -1) then
       Self.m_state.trains.Add(trainIndex)
     else
       writelog('WARNING: souprava '+s+' na bloku '+Self.name+' neexistuje, mažu soupravu', WR_DATA);
    end;

   // houkaci udalosti
   try
     Self.LoadHoukEventToList(Self.m_settings.houkEvL, ini_tech, section, 'houkL');
   except
     writelog('Nepodařilo se načíst houkací události L bloku ' + Self.name, WR_ERROR);
   end;

   try
     Self.LoadHoukEventToList(Self.m_settings.houkEvS, ini_tech, section, 'houkS');
   except
     writelog('Nepodařilo se načíst houkací události S bloku ' + Self.name, WR_ERROR);
   end;
 finally
   strs.Free();
 end;

 strs := Self.LoadORs(ini_rel, 'U');
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
   strs.Free();
 end;

 PushRCSToArea(Self.m_areas, Self.m_settings.RCSAddrs);
end;

procedure TBlkTrack.SaveData(ini_tech: TMemIniFile; const section: string);
var i: Integer;
begin
 inherited SaveData(ini_tech, section);

 Self.SaveRCS(ini_tech, section, Self.m_settings.RCSAddrs);
 ini_tech.WriteFloat(section,'delka', Self.m_settings.lenght);
 ini_tech.WriteString(section,'zesil', Self.m_settings.boosterId);

 if (Self.m_settings.maxTrains <> _DEFAULT_MAX_TRAINS) then
   ini_tech.WriteInteger(section, 'maxSpr', Self.m_settings.maxTrains);

 if (Self.m_settings.loop) then
   ini_tech.WriteBool(section, 'smc', Self.m_settings.loop);

 if (Assigned(Self.m_settings.houkEvL)) then
  begin
   for i := 0 to Self.m_settings.houkEvL.Count-1 do
    begin
     try
       ini_tech.WriteString(section, 'houkL'+IntToStr(i), Self.m_settings.houkEvL[i].GetDefString());
     except
       on E: Exception do
         AppEvents.LogException(E, 'Ukladani houkaci udalosti bloku ' + Self.name);
     end;
    end;
  end;

 if (Assigned(Self.m_settings.houkEvS)) then
  begin
   for i := 0 to Self.m_settings.houkEvS.Count-1 do
    begin
     try
       ini_tech.WriteString(section, 'houkS'+IntToStr(i), Self.m_settings.houkEvS[i].GetDefString());
     except
       on E: Exception do
         AppEvents.LogException(E, 'Ukladani houkaci udalosti bloku ' + Self.name);
     end;
    end;
  end;
end;

procedure TBlkTrack.SaveStatus(ini_stat: TMemIniFile; const section: string);
var str: string;
    traini: Integer;
begin
 if (Self.m_state.note <> '') then
   ini_stat.WriteString(section, 'stit', Self.m_state.note);

 if (Self.m_state.lockout <> '') then
   ini_stat.WriteString(section, 'vyl' , Self.m_state.lockout);

 if (Self.IsTrain()) then
  begin
   str := '';
   for traini in Self.trains do
     str := str + TrainDb.Trains[traini].name + ',';
   ini_stat.WriteString(section, 'spr' , str);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrack.Enable();
var rcsaddr: TRCSAddr;
    enable: Boolean;
begin
 enable := true;
 try
   for rcsaddr in Self.m_settings.RCSAddrs do
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

 Self.Update();
 //change event will be called in Update();
end;

procedure TBlkTrack.Disable();
var area: TArea;
begin
 inherited;

 if (Self.m_state.shortCircuit = TBoosterSignal.error) then
   for area in Self.areas do
     area.shortCircBlkCnt := area.shortCircBlkCnt - 1;

 Self.m_state.occupied := disabled;
 Self.m_state.occupiedOld := disabled;
 Self.m_state.NUZ := false;
 Self.m_state.TrainPredict := -1;
 Self.m_state.jcEnd := TZaver.no;
 Self.m_state.slowingReady := false;
 Self.houkEvEnabled := false;
 Self.m_state.shortCircuit := TBoosterSignal.undef;
 Self.m_state.power := TBoosterSignal.undef;
 Self.m_state.DCC := false;
 Self.m_state.sectionsOccupied.Clear();

 Self.m_state.neprofilJCcheck.Clear();

 Self.Change(true);
end;

procedure TBlkTrack.Reset();
begin
 Self.eventsOnOccupy.Clear();
 Self.eventsOnFree.Clear();
 Self.eventsOnZaverReleaseOrAB.Clear();
 Self.Zaver := TZaver.no;
end;

function TBlkTrack.UsesRCS(addr: TRCSAddr; portType: TRCSIOType): Boolean;
begin
 Result := ((portType = TRCSIOType.input) and (Self.m_settings.RCSAddrs.Contains(addr)));
end;

////////////////////////////////////////////////////////////////////////////////

//update all local variables
procedure TBlkTrack.Update();
var i, train: Integer;
    state: TRCSInputState;
    trackState: TTrackState;
    area: TArea;
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

   if (((not Self.DCC) or (Self.power = TBoosterSignal.error)
        or (Now < Self.state.shortCircSenseTime))
       and (Self.shortCircuit = TBoosterSignal.error)) then
     Self.shortCircuit := TBoosterSignal.ok;

   if ((Self.DCC) and (Self.shortCircuit <> TBoosterSignal.error) and (Self.power <> TBoosterSignal.error) and
       (Now > Self.state.shortCircSenseTime)) then
     Self.UnFreeze();
   Exit();
  end;

 if (Self.m_state.sectionsOccupied.Count <> Self.m_settings.RCSAddrs.Count) then
  begin
   Self.m_state.sectionsOccupied.Clear();
   for i := 0 to Self.m_settings.RCSAddrs.Count-1 do
     Self.m_state.sectionsOccupied.Add(TTrackState.none);
  end;

 Self.m_state.occupied := TTrackState.free; // must be here to update booster state

 for i := 0 to Self.m_settings.RCSAddrs.Count-1 do
  begin
   try
     state := RCSi.GetInput(Self.m_settings.RCSAddrs[i]);
   except
     state := failure;
   end;

   case (state) of
    isOn  : Self.m_state.sectionsOccupied[i] := TTrackState.occupied;
    isOff : Self.m_state.sectionsOccupied[i] := TTrackState.free;
    failure, notYetScanned, unavailableModule, unavailablePort: begin
        Self.m_state.sectionsOccupied[i] := TTrackState.disabled;
        Self.m_state.occupied := TTrackState.disabled;
      end;
   end;
  end;

 if (Self.m_state.occupied <> TTrackState.disabled) then
   for trackState in Self.m_state.sectionsOccupied do
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
       for area in Self.areas do
         area.BlkWriteError(Self, 'Ztráta soupravy v úseku '+Self.name, 'TECHNOLOGIE');
     if (Self.m_state.zaver <> TZaver.no) then Self.m_state.Zaver := TZaver.nouz;
    end;//if train_vypadek_time > 3
  end;//if train_vypadek

 // OnChange
 if (Self.m_state.occupied <> Self.m_state.occupiedOld) then
  begin
   if (Self.m_state.occupied = TTrackState.disabled) then
    begin
     Self.m_state.occupied := disabled;
     Self.m_state.occupiedOld := Self.m_state.occupied;
     JCDb.Cancel(Self);

     // zastavime soupravy na useku
     for train in Self.trains do
       TrainDb.Trains[train].speed := 0;

     Self.Change(true);
    end;

   if (Self.m_state.occupiedOld = TTrackState.disabled) then
    begin
     // Wake-up from disabled
     Self.OnBoosterChange();
    end;

   // kontrola udalosti obsazeni
   if (Self.m_state.occupied = TTrackState.occupied) then begin
     Self.NeprofilObsaz();
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
    end;//if Train <> -1

   Self.m_state.occupiedOld := Self.m_state.occupied;
   Self.Change();
  end;

 // reseni zruseni PRESUN soupravy, ktera jede
 if ((Self.IsTrainMoving()) and ((not Self.IsTrain(Self.trains[Self.trainMoving])) or
     (TrainDb.Trains[Self.trains[Self.trainMoving]].wantedSpeed > 0))) then
   Self.trainMoving := -1;

 // pousteni houkani na houkaci udalosti
 if (Self.m_state.currentHoukEv > -1) then
   Self.CheckHoukEv();

 // kontrola zmeny barev vlivem uplynuti casu predvidaneho odjezdu
 Self.CheckPOdjChanged();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrack.SetNUZ(nuz: Boolean);
var area: TArea;
begin
 if (Self.m_state.NUZ = nuz) then Exit();

 if (Self.m_state.NUZ) and (not nuz) then
  begin
   for area in Self.m_areas do
     if (area.NUZblkCnt > 0) then
       area.NUZblkCnt := area.NUZblkCnt - 1;
  end else begin
    if ((not Self.m_state.NUZ) and (nuz)) then
      for area in Self.m_areas do
        area.NUZblkCnt := area.NUZblkCnt + 1;
  end;

 Self.m_state.NUZ := nuz;
 Self.Change();
end;

procedure TBlkTrack.SetZaver(Zaver: TZaver);
var old: TZaver;
begin
 if (Zaver = Self.Zaver) then Exit();

 if ((Self.m_state.Zaver > TZaver.no) and ((Zaver = TZaver.no) or (Zaver = TZaver.ab))) then
   Self.NUZ := false;

 old := Self.Zaver;
 Self.m_state.Zaver := Zaver;
 Self.m_state.TrainPredict := -1;

 if ((old > TZaver.no) and (zaver = TZaver.no)) then
   Self.CallChangeEvents(Self.eventsOnZaverReleaseOrAB)
 else if ((old <> TZaver.no) and (old <> TZaver.ab) and (zaver = TZaver.ab)) then
   Self.CallChangeEvents(Self.eventsOnZaverReleaseOrAB);

 // staveci zavery se do panelu neposilaji, protoze jsou mi k nicemu
 if ((Self.Zaver <> TZaver.staveni) or (old <> TZaver.no) or (diag.showZaver)) then
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

procedure TBlkTrack.SetLockout(Sender: TIDCOntext; lockout: string);
begin
 if ((self.m_state.lockout <> '') and (lockout = '')) then
  begin
   ORTCPServer.Potvr(Sender, Self.ORVylukaNull, Self.m_areas[0], 'Zrušení výluky', TBlocks.GetBlksList(Self), nil);
  end else begin
   Self.lockout := lockout;
  end;
end;

function TBlkTrack.GetTrainPredict(): TTrain;
begin
 if (Self.m_state.TrainPredict = -1) then
   Exit(nil);
 Result := TrainDb.Trains[Self.m_state.TrainPredict];
end;

procedure TBlkTrack.SetTrainPredict(train: TTrain);
var old: Integer;
begin
 old := Self.m_state.TrainPredict;
 if (train = nil) then
   Self.m_state.TrainPredict := -1
 else
   Self.m_state.TrainPredict := train.index;

 if ((train = nil) and (old > -1)) then
  begin
   // odstranit predvidany odjezd mazane predpovidane soupravy
   if (TrainDb.Trains[old].IsPOdj(Self)) then
     TrainDb.Trains[old].RemovePOdj(Self);
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
var area: TArea;
begin
 if (Self.frozen) then
   Self.last_zes_zkrat := state;

 if (Self.m_state.shortCircuit = state) then
   Exit();

 if ((state = TBoosterSignal.error) and (not Self.frozen) and
     ((not Self.DCC) or (Self.power = TBoosterSignal.error) or
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
   for area in Self.m_areas do
    area.shortCircBlkCnt := area.shortCircBlkCnt + 1;
  end else begin
   if (Self.m_state.shortCircuit = TBoosterSignal.error) then
     for area in Self.m_areas do
       area.shortCircBlkCnt := area.shortCircBlkCnt - 1;
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
   Self.m_state.shortCircSenseTime := Now+EncodeTime(0, 0, 1, 0);

 Self.Change();
end;

procedure TBlkTrack.SetDCC(state: Boolean);
begin
 if (state = Self.state.DCC) then
   Exit();

 // doslo ke zmene DCC
 Self.m_state.DCC := state;
 if (state) then
   Self.m_state.shortCircSenseTime := Now+EncodeTime(0, 0, 1, 0)
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
 if (Self.frozen) then Exit();

 inherited;
 Self.last_zes_zkrat := Self.shortCircuit;
end;

procedure TBlkTrack.UnFreeze();
begin
 if (not Self.frozen) then Exit();

 inherited;
 if (Self.shortCircuit <> Self.last_zes_zkrat) then
  begin
   Self.shortCircuit := Self.last_zes_zkrat;
   Self.Change();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrack.GetSettings(): TBlkTrackSettings;
begin
 Result := Self.m_settings;
end;

procedure TBlkTrack.SetSettings(data: TBlkTrackSettings);
begin
 if (Self.m_settings.houkEvL <> data.houkEvL) then
   Self.m_settings.houkEvL.Free();

 if (Self.m_settings.houkEvS <> data.houkEvS) then
   Self.m_settings.houkEvS.Free();

 if (Self.m_settings.RCSAddrs <> data.RCSAddrs) then
   Self.m_settings.RCSAddrs.Free();

 Self.m_settings := data;

 if (not Self.spnl.stationTrack) then
   Self.m_settings.maxTrains := 1;

 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrack.MenuNewLokClick(SenderPnl: TIdContext; SenderOR: TObject; itemindex: Integer);
begin
 // nejdrive posleme aktualni seznam hnacich vozidel
 (SenderOR as TArea).PanelHVList(SenderPnl);

 // pak posleme pozadavek na editaci hnaciho vozidla
 (SenderOR as TArea).BlkNewTrain(Self, SenderPnl, (itemindex-2) div 2);
end;

procedure TBlkTrack.MenuVLOZLokClick(SenderPnl: TIdContext; SenderOR: TObject; itemindex: Integer);
begin
 Self.MoveLok(SenderPnl, SenderOR, (itemindex-2) div 2);
end;

procedure TBlkTrack.MenuEditLokClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 if ((TTCPORsRef(SenderPnl.Data).train_menu_index < 0) or
     (TTCPORsRef(SenderPnl.Data).train_menu_index >= Self.trains.Count)) then Exit();

 // nejdrive posleme aktualni senam hnacich vozidel
 (SenderOR as TArea).PanelHVList(SenderPnl);

 // pak posleme pozadavek na editaci hnaciho vozidla
 (SenderOR as TArea).BlkEditTrain(Self, SenderPnl, TrainDb.Trains[Self.trains[TTCPORsRef(SenderPnl.Data).train_menu_index]]);
end;

procedure TBlkTrack.MenuDeleteLokClick(SenderPnl: TIdContext; SenderOR: TObject);
var podm: TConfSeqItems;
    blk: TObject;
begin
 if ((TTCPORsRef(SenderPnl.Data).train_menu_index < 0) or
     (TTCPORsRef(SenderPnl.Data).train_menu_index >= Self.trains.Count)) then Exit();

 podm := TConfSeqItems.Create();
 for blk in Blocks.GetBlkWithTrain(TrainDb.Trains[Self.trains[TTCPORsRef(SenderPnl.Data).train_menu_index]]) do
   podm.Add(TArea.GetPSPodminka(blk, 'Smazání soupravy z úseku'));
 ORTCPServer.Potvr(SenderPnl, Self.PotvrDeleteLok, SenderOR as TArea,
   'Smazání soupravy '+TrainDb.Trains[Self.trains[TTCPORsRef(SenderPnl.Data).train_menu_index]].name,
   TBlocks.GetBlksList(Self), podm);
end;

procedure TBlkTrack.PotvrDeleteLok(Sender: TIdContext; success: Boolean);
begin
 if ((TTCPORsRef(Sender.Data).train_menu_index < 0) or
     (TTCPORsRef(Sender.Data).train_menu_index >= Self.trains.Count)) then Exit();

 if (success) then
  begin
   if (Self.m_state.trainMoving = TTCPORsRef(Sender.Data).train_menu_index) then
     Self.m_state.trainMoving := -1;
   TrainDb.Trains.Remove(Self.trains[TTCPORsRef(Sender.Data).train_menu_index]);
  end;
end;

procedure TBlkTrack.PotvrUvolLok(Sender: TIdContext; success: Boolean);
begin
 if ((TTCPORsRef(Sender.Data).train_menu_index < 0) or
     (TTCPORsRef(Sender.Data).train_menu_index >= Self.trains.Count)) then Exit();

 if (not success) then Exit();

 if (Blocks.GetBlkWithTrain(TrainDb.Trains[Self.trains[TTCPORsRef(Sender.Data).train_menu_index]]).Count = 1) then
  begin
   TrainDb.Trains.Remove(Self.trains[TTCPORsRef(Sender.Data).train_menu_index]);
   ORTCPServer.SendInfoMsg(Sender, 'Souprava odstraněna');
  end else begin
   Self.RemoveTrain(Self.trains[TTCPORsRef(Sender.Data).train_menu_index]);
  end;
end;

procedure TBlkTrack.MenuUVOLLokClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 if ((TTCPORsRef(SenderPnl.Data).train_menu_index < 0) or
     (TTCPORsRef(SenderPnl.Data).train_menu_index >= Self.trains.Count)) then Exit();

 ORTCPServer.Potvr(SenderPnl, Self.PotvrUvolLok, SenderOR as TArea,
  'Uvolnění soupravy '+TrainDb.Trains[Self.trains[TTCPORsRef(SenderPnl.Data).train_menu_index]].name+' z bloku',
  TBlocks.GetBlksList(Self), nil);
end;

procedure TBlkTrack.MenuVEZMILokClick(SenderPnl: TIdContext; SenderOR: TObject);
var train: TTrain;
begin
 if ((TTCPORsRef(SenderPnl.Data).train_menu_index < 0) or
     (TTCPORsRef(SenderPnl.Data).train_menu_index >= Self.trains.Count)) then Exit();

 train := TrainDb.Trains[Self.trains[TTCPORsRef(SenderPnl.Data).train_menu_index]];

 if (train.stolen) then
  begin
   // Prevzit soupravu, ktera byla ukradena.
   Self.MenuXVEZMILokClick(SenderPnl, SenderOR);
  end else begin
   if (train.IsAnyLokoInRegulator()) then
    begin
     // Nasilim prevzit lokomotivy z regulatoru.
     Self.MenuRegVEZMILokClick(SenderPnl, SenderOR);
    end;
  end;
end;

procedure TBlkTrack.XTakeTrainOk(Sender: TObject; Data: Pointer);
begin
 ORTCPServer.SendInfoMsg(TIdContext(Data), 'Vlak převzat');
end;

procedure TBlkTrack.XTakeTrainErr(Sender: TObject; Data: Pointer);
begin
 ORTCPServer.BottomError(TIdContext(Data), 'Vlak se nepodařilo převzít', '', 'TECHNOLOGIE');
end;

procedure TBlkTrack.MenuXVEZMILokClick(SenderPnl: TIdContext; SenderOR: TObject);
var train: TTrain;
begin
 if ((TTCPORsRef(SenderPnl.Data).train_menu_index < 0) or
     (TTCPORsRef(SenderPnl.Data).train_menu_index >= Self.trains.Count)) then Exit();

 train := TrainDb.Trains[Self.trains[TTCPORsRef(SenderPnl.Data).train_menu_index]];
 train.Acquire(TTrakce.Callback(Self.XTakeTrainOk, SenderPnl), TTrakce.Callback(Self.XTakeTrainErr, SenderPnl));
end;

procedure TBlkTrack.MenuRegVEZMILokClick(SenderPnl: TIdContext; SenderOR: TObject);
var podm: TConfSeqItems;
    train: TTrain;
    hvaddr: Integer;
begin
 if ((TTCPORsRef(SenderPnl.Data).train_menu_index < 0) or
     (TTCPORsRef(SenderPnl.Data).train_menu_index >= Self.trains.Count)) then Exit();
 train := TrainDb.Trains[Self.trains[TTCPORsRef(SenderPnl.Data).train_menu_index]];

 podm := TConfSeqItems.Create();
 for hvaddr in train.HVs do
   if (HVDb[hvaddr] <> nil) then
     podm.Add(TArea.GetPSPodminka(HVDb[hvaddr].NiceName(), 'Násilné převzetí řízení'));

 ORTCPServer.Potvr(SenderPnl, Self.PotvrRegVezmiLok, SenderOR as TArea,
  'Nouzové převzetí hnacích vozidel do automatického řízení',
  TBlocks.GetBlksList(Self), podm);
end;

procedure TBlkTrack.PotvrRegVezmiLok(Sender: TIdContext; success: Boolean);
var train: TTrain;
begin
 if ((TTCPORsRef(Sender.Data).train_menu_index < 0) or
     (TTCPORsRef(Sender.Data).train_menu_index >= Self.trains.Count) or (not success)) then Exit();
 train := TrainDb.Trains[Self.trains[TTCPORsRef(Sender.Data).train_menu_index]];

 try
   train.ForceRemoveAllRegulators();
   ORTCPServer.SendInfoMsg(Sender, 'Vlak převzat');
 except
  on E: Exception do
    ORTCPServer.BottomError(Sender, 'Vlak se nepodařilo převzít', TArea(Sender).ShortName, 'TECHNOLOGIE');
 end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrack.MenuStitClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 ORTCPServer.Stitek(SenderPnl, Self, Self.state.note);
end;

procedure TBlkTrack.MenuVylClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 ORTCPServer.Vyluka(SenderPnl, Self, Self.state.lockout);
end;

// pokud volba nebyla uspesna, vraci false a v tom pripade je vyvolano menu
function TBlkTrack.MenuKCClick(SenderPnl: TIdContext; SenderOR: TObject): Boolean;
var signal: TBlkSignal;
begin
 if ((Self.m_state.jcEnd <> TZaver.no) and (not (SenderOR as TArea).vb.Contains(Self))) then
  begin
   ORTCPServer.SendInfoMsg(SenderPnl, 'Probíhá volba');
   Exit(true);
  end;

 if ((SenderOR as TArea).vb.Contains(Self)) then (SenderOR as TArea).vb.Remove(self);

 signal := Blocks.GeTBlkSignalSelected((SenderOR as TArea).id) as TBlkSignal;
 if (signal = nil) then Exit(false);

 case (signal.selected) of
  TBlkSignalSelection.VC : Self.m_state.jcEnd := TZaver.vlak;
  TBlkSignalSelection.PC : Self.m_state.jcEnd := TZaver.posun;
  TBlkSignalSelection.NC, TBlkSignalSelection.PP
                   : Self.m_state.jcEnd := TZaver.nouz;
 end;//case

 JCDb.ActivateJC(signal, Self, SenderPnl, SenderOR, signal.beginAB);

 Self.Change();
 Result := true;
end;

procedure TBlkTrack.MenuVBClick(SenderPnl: TIdContext; SenderOR: TObject);
var Blk: TBlk;
begin
 if (Self.m_state.jcEnd <> TZaver.no) then
  begin
   ORTCPServer.SendInfoMsg(SenderPnl, 'Probíhá volba');
   Exit();
  end;

 Blk := Blocks.GeTBlkSignalSelected((SenderOR as TArea).id);
 if (Blk = nil) then Exit();

 if (not Self.CanBeNextVB((SenderOR as TArea).vb, blk)) then
  begin
   ORTCPServer.SendInfoMsg(SenderPnl, 'Není variantním bodem žádné JC');
   Exit();
  end;

 case ((Blk as TBlkSignal).selected) of
  TBlkSignalSelection.VC : Self.m_state.jcEnd := TZaver.vlak;
  TBlkSignalSelection.PC : Self.m_state.jcEnd := TZaver.posun;
  TBlkSignalSelection.NC : Self.m_state.jcEnd := TZaver.nouz;
  TBlkSignalSelection.PP : Self.m_state.jcEnd := TZaver.nouz;
 end;

 (SenderOR as TArea).vb.Add(Self);

 Self.Change();
end;

procedure TBlkTrack.MenuNUZStartClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 Self.NUZ := true;
end;

procedure TBlkTrack.MenuNUZStopClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 Self.NUZ := false;
end;

procedure TBlkTrack.MenuPRESUNLokClick(SenderPnl: TIdContext; SenderOR: TObject; new_state: Boolean);
var Blk: TBlk;
begin
 if ((TTCPORsRef(SenderPnl.Data).train_menu_index < 0) or
     (TTCPORsRef(SenderPnl.Data).train_menu_index >= Self.trains.Count)) then Exit();

 if (new_state) then
  begin
   if (TrainDb.Trains[Self.trains[TTCPORsRef(SenderPnl.Data).train_menu_index]].station <> SenderOR) then
    begin
     ORTCPServer.SendInfoMsg(SenderPnl, 'Loko se nenachází ve vaši oblasti řízení');
     Exit();
    end;

   Blk := Blocks.GetBlkUsekVlakPresun((SenderOR as TArea).id);
   if (Blk <> nil) then (Blk as TBlkTrack).trainMoving := -1;
   Self.trainMoving := TTCPORsRef(SenderPnl.Data).train_menu_index;
  end else begin
   Self.trainMoving := -1;
  end;
end;

procedure TBlkTrack.MenuRUClokClick(SenderPnl: TIdContext; SenderOR: TObject);
var addr: Integer;
    str: string;
    HV: THV;
begin
 if ((TTCPORsRef(SenderPnl.Data).train_menu_index < 0) or
     (TTCPORsRef(SenderPnl.Data).train_menu_index >= Self.trains.Count)) then Exit();

 str := (SenderOR as TArea).id + ';LOK-TOKEN;OK;';
 for addr in TrainDb.Trains[Self.trains[TTCPORsRef(SenderPnl.Data).train_menu_index]].HVs do
  begin
   HV := HVDb[addr];
   str := str + '[' + IntToStr(HV.addr) + '|' + HV.GetToken() + ']';
  end;//for i

 ORTCPServer.SendLn(SenderPnl, str);
end;

procedure TBlkTrack.MenuMAUSlokClick(SenderPnl: TIdContext; SenderOR: TObject);
var addr: Integer;
    str: string;
    HV: THV;
begin
 if ((TTCPORsRef(SenderPnl.Data).train_menu_index < 0) or
     (TTCPORsRef(SenderPnl.Data).train_menu_index >= Self.trains.Count)) then Exit();

 str := (SenderOR as TArea).id + ';MAUS;{';
 for addr in TrainDb.Trains[Self.trains[TTCPORsRef(SenderPnl.Data).train_menu_index]].HVs do
  begin
   HV := HVDb[addr];
   str := str + IntToStr(HV.addr) + '|';
  end;//for i
 str := str + '}';

 ORTCPServer.SendLn(SenderPnl, str);
end;

procedure TBlkTrack.MenuObsazClick(SenderPnl: TIdContext; SenderOR: TObject);
var rcsaddr: TRCSAddr;
begin
 try
   for rcsaddr in Self.m_settings.RCSAddrs do
     RCSi.SetInput(rcsaddr.board, rcsaddr.port, 1);
 except
   ORTCPServer.BottomError(SenderPnl, 'Simulace nepovolila nastavení RCS vstupů!', TArea(SenderOR).ShortName, 'SIMULACE');
 end;
end;

procedure TBlkTrack.MenuUvolClick(SenderPnl: TIdContext; SenderOR: TObject);
var rcsaddr: TRCSAddr;
begin
 try
   for rcsaddr in Self.m_settings.RCSAddrs do
     RCSi.SetInput(rcsaddr.board, rcsaddr.port, 0);
 except
   ORTCPServer.BottomError(SenderPnl, 'Simulace nepovolila nastavení RCS vstupů!', TArea(SenderOR).ShortName, 'SIMULACE');
 end;
end;

procedure TBlkTrack.MenuHLASENIOdjezdClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 if ((TTCPORsRef(SenderPnl.Data).train_menu_index < 0) or
     (TTCPORsRef(SenderPnl.Data).train_menu_index >= Self.trains.Count)) then Exit();

 try
   if (not Assigned(TArea(SenderOR).announcement)) then Exit();
   TArea(SenderOR).announcement.Odjede(Self.GetSHTrain(TTCPORsRef(SenderPnl.Data).train_menu_index));
 except
   on E: Exception do
    begin
     writelog('Nepodařilo se spustit staniční hlášení : ' + E.Message, WR_ERROR);
     ORTCPServer.BottomError(SenderPnl, 'Nepodařilo se spustit staniční hlášení!', TArea(SenderOR).ShortName, 'TECHNOLOGIE');
    end;
 end;
end;

procedure TBlkTrack.MenuHLASENIPrijezdClick(SenderPnl: TIdContext; SenderOR: TObject);
var shTrain: TSHTrain;
    blk: TBlkTrack;
begin
 if ((TTCPORsRef(SenderPnl.Data).train_menu_index < 0) or
     (TTCPORsRef(SenderPnl.Data).train_menu_index >= Self.trains.Count)) then Exit();

 try
   if (not Assigned(TArea(SenderOR).announcement)) then Exit();

   shTrain := Self.GetSHTrain(TTCPORsRef(SenderPnl.Data).train_menu_index);
   blk := stanicniHlaseniHelper.CanPlayPrijezdSH(
      TrainDb.Trains[Self.trains[TTCPORsRef(SenderPnl.Data).train_menu_index]],
      TArea(SenderOR)).stanicniKolej;
   if (blk = nil) then Exit();

   shTrain.kolej := blk.spnl.trackName;
   TArea(SenderOR).announcement.Prijede(shTrain);
 except
   on E: Exception do
    begin
     writelog('Nepodařilo se spustit staniční hlášení : ' + E.Message, WR_ERROR);
     ORTCPServer.BottomError(SenderPnl, 'Nepodařilo se spustit staniční hlášení!', TArea(SenderOR).ShortName, 'TECHNOLOGIE');
    end;
 end;
end;

procedure TBlkTrack.MenuHLASENIPrujezdClick(SenderPnl: TIdContext; SenderOR: TObject);
var shTrain: TSHTrain;
    blk: TBlkTrack;
begin
 if ((TTCPORsRef(SenderPnl.Data).train_menu_index < 0) or
     (TTCPORsRef(SenderPnl.Data).train_menu_index >= Self.trains.Count)) then Exit();

 try
   if (not Assigned(TArea(SenderOR).announcement)) then Exit();

   shTrain := Self.GetSHTrain(TTCPORsRef(SenderPnl.Data).train_menu_index);
   blk := stanicniHlaseniHelper.CanPlayPrijezdSH(
      TrainDb.Trains[Self.trains[TTCPORsRef(SenderPnl.Data).train_menu_index]],
      TArea(SenderOR)).stanicniKolej;

   if (blk <> nil) then
     shTrain.kolej := blk.spnl.trackName;
   TArea(SenderOR).announcement.Projede(shTrain);
 except
   on E: Exception do
    begin
     writelog('Nepodařilo se spustit staniční hlášení : ' + E.Message, WR_ERROR);
     ORTCPServer.BottomError(SenderPnl, 'Nepodařilo se spustit staniční hlášení!', TArea(SenderOR).ShortName, 'TECHNOLOGIE');
    end;
 end;
end;

procedure TBlkTrack.MenuPOdjClick(SenderPnl: TIdContext; SenderOR: TObject);
var train: TTrain;
begin
 if ((TTCPORsRef(SenderPnl.Data).train_menu_index >= 0) and
     (TTCPORsRef(SenderPnl.Data).train_menu_index < Self.trains.Count)) then
  begin
   train := TrainDb.Trains[Self.trains[TTCPORsRef(SenderPnl.Data).train_menu_index]];
  end else begin
   if (Self.TrainPredict = nil) then Exit();
   train := Self.TrainPredict;
  end;

 if (train.IsPOdj(Self)) then
   ORTCPServer.POdj(SenderPnl, Self, train.index, train.GetPOdj(Self))
 else
   ORTCPServer.POdj(SenderPnl, Self, train.index, nil);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrack.MenuSOUPRAVA(SenderPnl: TIdContext; SenderOR: TObject; trainLocalI: Integer);
var menu: string;
begin
 TTCPORsRef(SenderPnl.Data).train_menu_index := trainLocalI;

 menu := '$'+Self.m_globSettings.name+',';
 menu := menu + '$Souprava ' + TrainDb.Trains[Self.trains[trainLocalI]].name + ',-,';
 menu := menu + Self.GetTrainMenu(SenderPnl, SenderOr, trainLocalI);

 ORTCPServer.Menu(SenderPnl, Self, (SenderOR as TArea), menu);
end;

////////////////////////////////////////////////////////////////////////////////

//vytvoreni menu pro potreby konkretniho bloku:
function TBlkTrack.ShowPanelMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TAreaRights): string;
var Blk: TBlk;
    train: Integer;
    canAdd: Boolean;
    addStr: string;
    m_state: TTrackState;
begin
 Result := inherited;

 if (Blocks.GetBlkUsekVlakPresun((SenderOR as TArea).id) <> nil) then
   addStr := 'VLOŽ vlak,'
 else
   addStr := 'NOVÝ vlak,';

 if (Self.TrainsFull() and (Self.trains.Count = 1)) then begin
   TTCPORsRef(SenderPnl.Data).train_menu_index := 0;
   Result := Result + Self.GetTrainMenu(SenderPnl, SenderOR, 0) + '-,';
 end else begin
   canAdd := ((Self.CanStandTrain()) and
              (( (not Self.TrainsFull()) and ((Self.m_state.occupied = TTrackState.occupied) or (Self.m_settings.RCSAddrs.Count = 0)) ) or // novy vlak
               ( addStr = 'VLOŽ vlak,' ) // presun vlaku
              ));

   if (canAdd) then
     Result := Result + addStr;

   for train in Self.trains do
    begin
     Result := Result + TrainDb.Trains[train].name + ',';
     if (canAdd) then Result := Result + addStr;
    end;

   if ((canAdd) or (Self.trains.Count > 0)) then
     Result := Result + '-,';
 end;

 if ((Self.TrainPredict <> nil) and (Self.spnl.stationTrack)) then
   Result := Result + 'PODJ,-,';

 Result := Result + 'STIT,VYL,';

 if (Self.m_state.NUZ) then
   Result := Result + '-,NUZ<,';

 if ((((not (SenderOR as TArea).NUZtimer) and (Integer(Self.m_state.Zaver) > 0) and (Self.m_state.Zaver <> TZaver.ab) and
      (Self.m_state.Zaver <> TZaver.staveni) and (Self.typ = btTrack) and
      (not Self.spnl.stationTrack)) or (rights >= superuser)) and
      (not Self.m_state.NUZ)) then
   Result := Result + '-,NUZ>,';

 //11 = KC
 Blk := Blocks.GeTBlkSignalSelected((SenderOR as TArea).id);
 if (Blk <> nil) then
  begin
   if ((Self.CanBeKC((SenderOR as TArea).vb, blk)) or Self.CanBeNextVB((SenderOR as TArea).vb, blk)) then
     Result := Result + '-,';
   if (Self.CanBeKC((SenderOR as TArea).vb, blk)) then
     Result := Result + 'KC,';
   if (Self.CanBeNextVB((SenderOR as TArea).vb, blk)) then
     Result := Result + 'VB,';
  end;

 // pokud mame knihovnu simulator, muzeme ridit stav useku
 //  DEBUG nastroj
 if (RCSi.simulation) then
  begin
   Result := Result + '-,';

   for m_state in Self.sectionsState do
    if (m_state = TTrackState.free) then
     begin
      Result := Result + '*OBSAZ,';
      break;
     end;

   for m_state in Self.sectionsState do
    if (m_state = TTrackState.occupied) then
     begin
      Result := Result + '*UVOL,';
      break;
     end;
  end;//if RCSi.lib = 2
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrack.GetTrainMenu(SenderPnl: TIdContext; SenderOR: TObject; trainLocalI: Integer): string;
var train: TTrain;
    shPlay: stanicniHlaseniHelper.TSHToPlay;
    train_count: Integer;
begin
 train := TrainDb.Trains[Self.trains[trainLocalI]];
 train_count := Blocks.GetBlkWithTrain(TrainDb.Trains[Self.trains[trainLocalI]]).Count;

 if (Self.CanStandTrain()) then
   Result := Result + 'EDIT vlak,';
 if ((Self.CanStandTrain()) or (train_count <= 1)) then
   Result := Result + '!ZRUŠ vlak,';
 if (train_count > 1) then
   Result := Result + '!UVOL vlak,';

 if (train.HVs.Count > 0) then
  begin
   Result := Result + 'RUČ vlak,';
   if (TTCPORsRef(SenderPnl.Data).maus) then Result := Result + 'MAUS vlak,';
  end;

 if (Self.trainMoving = trainLocalI) then
  Result := Result + 'PŘESUŇ vlak<,'
 else if ((not Self.IsTrainMoving()) and (train.wantedSpeed = 0) and (train.station = SenderOR)) then
   Result := Result + 'PŘESUŇ vlak>,';

 if (train.stolen) then
   Result := Result + 'VEZMI vlak,'
 else begin
   if (train.IsAnyLokoInRegulator()) then
     Result := Result + '!VEZMI vlak,';
 end;

 if (Self.CanStandTrain()) then
   Result := Result + 'PODJ,';

 if ((Assigned(TArea(SenderOR).announcement)) and (TArea(SenderOR).announcement.available) and
     (train.stationFrom <> nil) and (train.stationTo <> nil) and (train.typ <> '')) then
  begin
   if ((Self.spnl.stationTrack) and (train.announcement)) then
     Result := Result + 'HLÁŠENÍ odjezd,';

   try
     shPlay := stanicniHlaseniHelper.CanPlayPrijezdSH(train, TArea(SenderOR));
   except
     on E: Exception do
       AppEvents.LogException(E, 'CanPlayPrijezdSH');
   end;

   if ((shPlay.stanicniKolej <> nil) and ((shPlay.trat = nil) or (train.IsPOdj(shPlay.stanicniKolej)))) then
     Result := Result + 'HLÁŠENÍ příjezd,'
   else if (shPlay.trat <> nil) then
     Result := Result + 'HLÁŠENÍ průjezd,';
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrack.PanelClick(SenderPnl: TIdContext; SenderOR: TObject ; Button: TPanelButton; rights: TAreaRights; params: string = '');
var Blk: TBlk;
begin
 case (Button) of
  F2: Self.ShowProperMenu(SenderPnl, (SenderOR as TArea), rights, params);

  ENTER: begin
    if (not Self.MenuKCClick(SenderPnl, SenderOR)) then
      if (((Self.m_settings.maxTrains <> 1) and (Self.trains.Count > 0)) or (not Self.MoveLok(SenderPnl, SenderOR, 0))) then
        Self.ShowProperMenu(SenderPnl, (SenderOR as TArea), rights, params);
  end;

  F1: begin
    Blk := Blocks.GeTBlkSignalSelected((SenderOR as TArea).id);
    if (Blk = nil) then
      Self.ShowProperMenu(SenderPnl, (SenderOR as TArea), rights, params)
    else
      Self.MenuVBClick(SenderPnl, SenderOR);
  end;
 end;
end;

////////////////////////////////////////////////////////////////////////////////

//toto se zavola pri kliku na jakoukoliv itemu menu tohoto bloku
procedure TBlkTrack.PanelMenuClick(SenderPnl: TIdContext; SenderOR: TObject; item: string; itemindex: Integer);
var i: Integer;
begin
 if (item = 'NOVÝ vlak')           then Self.MenuNewLokClick(SenderPnl, SenderOR, itemindex)
 else if (item = 'VLOŽ vlak')      then Self.MenuVLOZLokClick(SenderPnl, SenderOR, itemindex)
 else if (item = 'EDIT vlak')      then Self.MenuEditLokClick(SenderPnl, SenderOR)
 else if (item = 'ZRUŠ vlak')      then Self.MenuDeleteLokClick(SenderPnl, SenderOR)
 else if (item = 'UVOL vlak')      then Self.MenuUVOLLokClick(SenderPnl, SenderOR)
 else if (item = 'VEZMI vlak')     then Self.MenuVEZMILokClick(SenderPnl, SenderOR)
 else if (item = 'PŘESUŇ vlak>')   then Self.MenuPRESUNLokClick(SenderPnl, SenderOR, true)
 else if (item = 'PŘESUŇ vlak<')   then Self.MenuPRESUNLokClick(SenderPnl, SenderOR, false)
 else if (item = 'RUČ vlak')       then Self.MenuRUCLokClick(SenderPnl, SenderOR)
 else if (item = 'MAUS vlak')      then Self.MenuMAUSLokClick(SenderPnl, SenderOR)
 else if (item = 'STIT')           then Self.MenuStitClick(SenderPnl, SenderOR)
 else if (item = 'VYL')            then Self.MenuVylClick(SenderPnl, SenderOR)
 else if (item = 'KC')             then Self.MenuKCClick(SenderPnl, SenderOR)
 else if (item = 'VB')             then Self.MenuVBClick(SenderPnl, SenderOR)
 else if (item = 'NUZ>')           then Self.MenuNUZStartClick(SenderPnl, SenderOR)
 else if (item = 'NUZ<')           then Self.MenuNUZStopClick(SenderPnl, SenderOR)
 else if (item = 'OBSAZ')          then Self.MenuObsazClick(SenderPnl, SenderOR)
 else if (item = 'UVOL')           then Self.MenuUvolClick(SenderPnl, SenderOR)
 else if (item = 'HLÁŠENÍ odjezd') then Self.MenuHLASENIOdjezdClick(SenderPnl, SenderOR)
 else if (item = 'HLÁŠENÍ příjezd')then Self.MenuHLASENIPrijezdClick(SenderPnl, SenderOR)
 else if (item = 'HLÁŠENÍ průjezd')then Self.MenuHLASENIPrujezdClick(SenderPnl, SenderOR)
 else if (item = 'PODJ')           then Self.MenuPOdjClick(SenderPnl, SenderOR)
 else begin
  // cislo soupravy
  for i := 0 to Self.trains.Count-1 do
    if (item = TrainDb.Trains[Self.trains[i]].name) then
     begin
      Self.MenuSOUPRAVA(SenderPnl, SenderOR, i);
      break;
     end;
 end;
end;

////////////////////////////////////////////////////////////////////////////////

// vraci true, pokud volba vyvolala nejaky efekt (false pokud se ma zobrazit menu)
function TBlkTrack.MoveLok(SenderPnl: TIdContext; SenderOR: TObject; trainLocalIndex: Integer): Boolean;
var Blk, signal: TBlk;
    train: TTrain;
begin
 Blk := Blocks.GetBlkUsekVlakPresun((SenderOR as TArea).id);
 if (Blk = nil) then Exit(false);

 if (not Self.CanStandTrain()) then
  begin
   ORTCPServer.SendInfoMsg(SenderPnl, 'Loko lze přesunout pouze na staniční kolej!');
   Exit(true);
  end;

 if ((Self.TrainsFull()) and (Blk <> Self)) then
  begin
   ORTCPServer.SendInfoMsg(SenderPnl, 'Do úseku se již nevejde další souprava!');
   Exit(true);
  end;

 if ((Self.Zaver > TZaver.no) and (Self.Zaver <> TZaver.posun)) then
  begin
   ORTCPServer.SendInfoMsg(SenderPnl, 'Nelze přesunout na úsek se závěrem!');
   Exit(true);
  end;

 if (not Self.CanTrainSpeedInsert(trainLocalIndex)) then
  begin
   ORTCPServer.SendInfoMsg(SenderPnl, 'Nelze vložit soupravu před jedoucí soupravu!');
   Exit(true);
  end;

 train := TrainDb.Trains[TBlkTrack(Blk).trains[TBlkTrack(Blk).trainMoving]];

 if (Blk = Self) then
  begin
   Self.m_state.trains.Insert(trainLocalIndex, train.index);

   if (trainLocalIndex <= Self.trainMoving) then
     Self.m_state.trains.Delete(Self.trainMoving+1)
   else
     Self.m_state.trains.Delete(Self.trainMoving);

   Self.m_state.trainMoving := -1;
   Self.Change();
  end else begin

   try
     Self.AddTrain(trainLocalIndex, train);
     (Blk as TBlkTrack).RemoveTrain(train);
   except
     on E: Exception do
      begin
       ORTCPServer.SendInfoMsg(SenderPnl, E.Message);
       Exit(true);
      end;
   end;
  end;

 ORTCPServer.SendInfoMsg(SenderPnl, 'Souprava '+train.name+' přesunuta na '+Self.m_globSettings.name+'.');

 if (Blocks.GetBlkWithTrain(train).Count = 1) then
   train.front := Self;

 for signal in (Blk as TBlkTrack).signalJCRef do
   Blocks.TrainPrediction(signal);

 if (Blk <> Self) then
   for signal in Self.signalJCRef do
     Blocks.TrainPrediction(signal);

 Result := true;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrack.GetPtData(json: TJsonObject; includeState: Boolean);
begin
 inherited;

 TBlk.RCSstoJSON(Self.m_settings.RCSAddrs, json.A['rcs']);

 json['length'] := Self.m_settings.lenght;
 if (Self.m_settings.loop) then json['loop'] := Self.m_settings.loop;
 json['booster'] := Self.m_settings.boosterId;
 json['maxTrains'] := Self.m_settings.maxTrains;
 json['stationTrack'] := Self.spnl.stationTrack;
 if (Self.spnl.trackName <> '') then
   json['trackName'] := Self.spnl.trackName;

 if (includeState) then
   Self.GetPtState(json['blockState']);
end;

procedure TBlkTrack.GetPtState(json: TJsonObject);
var train: Integer;
    m_state: TTrackState;
begin
 case (Self.occupied) of
  TTrackState.disabled : json['state'] := 'off';
  TTrackState.none     : json['state'] := 'none';
  TTrackState.free : json['state'] := 'free';
  TTrackState.occupied : json['state'] := 'occupied';
 end;

 for m_state in Self.sectionsState do
  begin
   case (m_state) of
    TTrackState.disabled : json.A['sections'].Add('off');
    TTrackState.none     : json.A['sections'].Add('none');
    TTrackState.free : json.A['sections'].Add('free');
    TTrackState.occupied : json.A['sections'].Add('occupied');
   end;
  end;

 json['power'] := (Self.power = TBoosterSignal.ok);
 json['shortCircuit'] := (Self.shortCircuit = TBoosterSignal.error);
 json['dcc'] := Self.DCC;

 if (Self.note <> '') then json['note'] := Self.note;
 if (Self.lockout <> '') then json['lockout'] := Self.lockout;

 for train in Self.trains do
   json.A['trains'].Add(TrainDb.Trains[train].name);

 json['lock'] := Integer(Self.state.Zaver);
 if (Self.m_state.TrainPredict > -1) then
   json['trainPredict'] := TrainDb.Trains[Self.m_state.TrainPredict].name;
 if (Self.m_state.NUZ) then
   json['nuz'] := true;
 if (Self.m_state.jcEnd <> TZaver.no) then
   json['endJC'] := Integer(Self.m_state.jcEnd);
end;

procedure TBlkTrack.PutPtState(reqJson: TJsonObject; respJson: TJsonObject);
var trainStr: string;
    train: Integer;
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
   for trainStr in reqJson.A['trains'] do
    begin
     train := TrainDb.Trains.GetTrainIndexByName(trainStr);
     if (train > -1) then
      begin
       try
         Self.AddTrainS(train)
       except
        on E: Exception do
         begin
          PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '400', 'Bad Request',
                                E.Message);
         end;

       end;
      end else
       PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '400', 'Bad Request',
                             'Souprava ' + trainStr + ' neexistuje, ignoruji.');
    end;
  end;

 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrack.LoadHoukEventToList(list: TList<THoukEv>; ini_tech: TMemIniFile; section: string; prefix: string);
var i: Integer;
    data: string;
begin
 try
   i := 0;
   list.Clear();
   data := ini_tech.ReadString(section, prefix+IntToStr(i), '');
   while (data <> '') do
    begin
     try
       list.Add(THoukEv.Create(data));
     except
       writelog('Nepodarilo se nacist houkaci udalost ' + data + ' bloku ' +
                  Self.name, WR_ERROR);
       Exit();
     end;

     Inc(i);
     data := ini_tech.ReadString(section, prefix+IntToStr(i), '');
    end;
 except

 end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrack.CheckHoukEv();
var list: TList<THoukEv>;
begin
 if (Self.m_state.currentHoukEv < 0) then Exit();

 if (not Self.IsTrain()) then
  begin
   Self.houkEvEnabled := false;
   Exit();
  end;

 list := Self.GetHoukList();
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
     list[Self.m_state.currentHoukEv].Register();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrack.GetHoukEvEnabled(): Boolean;
begin
 Result := (Self.m_state.currentHoukEv > -1);
end;

procedure TBlkTrack.SetHoukEvEnabled(state: Boolean);
var houkEv: THoukEv;
begin
 if (state) then
  begin
   if (not Self.IsTrain()) then Exit();
   if (Self.GetHoukList().Count = 0) then Exit();

   // aktivace prvni houkaci udalosti
   Self.m_state.currentHoukEv := 0;
   Self.GetHoukList()[0].Register();
  end else begin
   Self.m_state.currentHoukEv := -1;

   for houkEv in Self.m_settings.houkEvL do
     houkEv.Unregister();

   for houkEv in Self.m_settings.houkEvS do
     houkEv.Unregister();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrack.GetHoukList(): TList<THoukEv>;
begin
 if (not Self.IsTrain()) then Exit(nil);

 if (Self.TrainL.direction = THVStanoviste.lichy) then
   Result := Self.m_settings.houkEvL
 else
   Result := Self.m_settings.houkEvS;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrack.GetSHTrain(trainLocalIndex: Integer): TSHTrain;
begin
 if ((trainLocalIndex < 0) or (trainLocalIndex >= Self.trains.Count)) then Exit();

 Result.cislo := TrainDb.Trains[Self.trains[trainLocalIndex]].name;
 Result.typ := TrainDb.Trains[Self.trains[trainLocalIndex]].typ;
 Result.kolej := Self.spnl.trackName;
 Result.fromORid := TArea(TrainDb.Trains[Self.trains[trainLocalIndex]].stationFrom).id;
 Result.toORid := TArea(TrainDb.Trains[Self.trains[trainLocalIndex]].stationTo).id;

 Result.timeArrive := 0;

 if ((TrainDb.Trains[Self.trains[trainLocalIndex]].IsPOdj(Self)) and
     (TrainDb.Trains[Self.trains[trainLocalIndex]].GetPOdj(Self).abs_enabled)) then
   Result.timeDepart := TrainDb.Trains[Self.trains[trainLocalIndex]].GetPOdj(Self).abs
 else
   Result.timeDepart := 0;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrack.AddNeprofilJC(id: Integer);
begin
 if (Self.m_state.neprofilJCcheck.Contains(id)) then Exit();

 Self.m_state.neprofilJCcheck.Add(id);
 if (Self.m_state.neprofilJCcheck.Count = 1) then
   Self.Change();
end;

procedure TBlkTrack.RemoveNeprofilJC(id: Integer);
begin
 if (not Self.m_state.neprofilJCcheck.Contains(id)) then Exit();

 Self.m_state.neprofilJCcheck.Remove(id);
 if (Self.m_state.neprofilJCcheck.Count = 0) then
   Self.Change();
end;

function TBlkTrack.IsNeprofilJC(): Boolean;
begin
 Result := (Self.m_state.neprofilJCcheck.Count > 0);
end;

procedure TBlkTrack.NeprofilObsaz();
var jcid: Integer;
    jc: TJC;
begin
 for jcid in Self.m_state.neprofilJCcheck do
  begin
   jc := JCDb.GetJCByID(jcid);
   if (jc <> nil) then
     jc.NonProfileOccupied();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrack.GeTTrainIL(): Integer;
begin
 if (Self.m_state.trains.Count < 1) then
   Result := -1
 else
   Result := Self.m_state.trains[0];
end;

function TBlkTrack.GeTTrainL(): TTrain;
begin
 if (Self.GeTTrainIL() = -1) then
   Result := nil
 else
   Result := TrainDb.Trains[Self.GeTTrainIL()];
end;

function TBlkTrack.GeTTrainIS(): Integer;
begin
 if (Self.m_state.trains.Count < 1) then
   Result := -1
 else
   Result := Self.m_state.trains[Self.m_state.trains.Count-1];
end;

function TBlkTrack.GeTTrainS(): TTrain;
begin
 if (Self.GeTTrainIS() = -1) then
   Result := nil
 else
   Result := TrainDb.Trains[Self.GeTTrainIS()];
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrack.IsTrain(): Boolean;
begin
 Result := (Self.m_state.trains.Count > 0);
end;

function TBlkTrack.IsTrain(index: Integer): Boolean;
begin
 Result := Self.m_state.trains.Contains(index);
end;

function TBlkTrack.IsTrain(train: TTrain): Boolean;
begin
 Result := Self.IsTrain(train.index);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrack.AddTrainL(index: Integer);
begin
 if (Self.TrainsFull()) then
   raise ETrainFull.Create('Do bloku ' + Self.name + ' se uz nevejde dalsi souprava!');
 if (Self.trains.Contains(index)) then
   raise EDuplicitTrains.Create('Nelze pridat jednu soupravu na jeden blok vicekrat!');

 Self.m_state.trains.Insert(0, index);
 Self.m_state.TrainPredict := -1;
 Self.Change();
end;

procedure TBlkTrack.AddTrainS(index: Integer);
begin
 if (Self.TrainsFull()) then
   raise ETrainFull.Create('Do bloku ' + Self.name + ' se uz nevejde dalsi souprava!');
 if (Self.trains.Contains(index)) then
   raise EDuplicitTrains.Create('Nelze pridat jednu soupravu na jeden blok vicekrat!');

 Self.m_state.trains.Add(index);
 Self.m_state.TrainPredict := -1;
 Self.Change();
end;

procedure TBlkTrack.AddTrainL(train: TTrain);
begin
 Self.AddTrainL(train.index);
end;

procedure TBlkTrack.AddTrainS(train: TTrain);
begin
 Self.AddTrainS(train.index);
end;

procedure TBlkTrack.AddTrain(localTrainIndex: Integer; train: Integer);
begin
 if (Self.TrainsFull()) then
   raise ETrainFull.Create('Do bloku ' + Self.name + ' se uz nevejde dalsi souprava!');
 if (Self.trains.Contains(train)) then
   raise EDuplicitTrains.Create('Nelze pridat jednu soupravu na jeden blok vicekrat!');
 if (not Self.CanTrainSpeedInsert(localTrainIndex)) then
   raise ERunningTrain.Create('Nelze vložit soupravu před jedoucí soupravu!');

 Self.m_state.trains.Insert(localTrainIndex, train);
 Self.m_state.TrainPredict := -1;
 Self.Change();
end;

procedure TBlkTrack.AddTrain(localTrainIndex: Integer; train: TTrain);
begin
 Self.AddTrain(localTrainIndex, train.index);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrack.RemoveTrains();
var train: Integer;
begin
 for train in Self.trains do
   if (TrainDb.Trains[train].IsPOdj(Self)) then
     TrainDb.Trains[train].RemovePOdj(Self);

 Self.m_state.trains.Clear();
 Self.m_state.trainMoving := -1;
 Self.m_state.slowingReady := false;
 Self.houkEvEnabled := false;
 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrack.RemoveTrain(index: Integer);
begin
 if (Self.m_state.trains.Contains(index)) then
  begin
   if ((Self.IstrainMoving) and (Self.trains[Self.trainMoving] = index)) then
     Self.m_state.trainMoving := -1;

   Self.m_state.trains.Remove(index);

   // odstranit predvidany odjezd z aktualniho useku
   if (TrainDb.Trains[index].IsPOdj(Self)) then
     TrainDb.Trains[index].RemovePOdj(Self);

   Self.Change();
  end else begin
   raise ETrainNotExists.Create('Souprava ' + IntToStr(index) +
     ' neexistuje na useku ' + Self.name);
  end;

 if (not Self.IsTrain()) then
  begin
   Self.m_state.slowingReady := false;
   Self.houkEvEnabled := false;
  end;
end;

procedure TBlkTrack.RemoveTrain(train: TTrain);
begin
 Self.RemoveTrain(train.index);
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrack.TrainsFull(): Boolean;
begin
 Result := (Cardinal(Self.m_state.trains.Count) >= Self.m_settings.maxTrains);
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrack.IstrainMoving(): Boolean;
begin
 Result := (Self.state.trainMoving > -1);
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrack.GeTTrainI(): Integer;
begin
 if (Self.trains.Count = 0) then Exit(-1)
 else if (Self.trains.Count = 1) then Exit(Self.trains[0])
 else raise EMultipleTrains.Create('Usek ' + Self.name +
   ' obsahuje vice souprav, nelze se proto ptat jen na jednu soupravu!');
end;

function TBlkTrack.GeTTrain(): TTrain;
begin
 if (Self.GeTTrainI() = -1) then
   Result := nil
 else
   Result := TrainDb.Trains[Self.GeTTrainI()];
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrack.ShowProperMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TAreaRights; params: string);
var i: Integer;
begin
 if (params <> '') then begin
   i := StrToIntDef(params, -1);
   if ((i <> -1) and (i >= 0) and (i < Self.trains.Count)) then
     Self.MenuSOUPRAVA(SenderPnl, (SenderOR as TArea), i)
   else
     ORTCPServer.Menu(SenderPnl, Self, (SenderOR as TArea), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));
 end else
   ORTCPServer.Menu(SenderPnl, Self, (SenderOR as TArea), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));
end;

////////////////////////////////////////////////////////////////////////////////

// vraci true prave tehdy, kdyz lze vlozit soupravu na pozici index
// kontroluje, zda-li se nenazime vlozit pred soupravu v pohybu
function TBlkTrack.CanTrainSpeedInsert(index: Integer): Boolean;
begin
 Result := not ((Self.trains.Count > 0) and
                (((index = 0) and (TrainDb.Trains[Self.trains[index]].wantedSpeed > 0) and
                  (TrainDb.Trains[Self.trains[index]].direction = THVStanoviste.sudy)) or
                 ((index = Self.trains.Count) and (TrainDb.Trains[Self.trains[index-1]].wantedSpeed > 0) and
                  (TrainDb.Trains[Self.trains[index-1]].direction = THVStanoviste.lichy))));
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrack.POdjChanged(trainId: Integer; var podj: TPOdj);
var train: Integer;
    was: Boolean;
    nav: TBlk;
    jc: TJC;
begin
 if ((not Self.trains.Contains(trainId)) and (trainId <> Self.m_state.TrainPredict)) then
   raise Exception.Create('Souprava již není na úseku!');

 was := TrainDb.Trains[trainId].IsPOdj(Self);

 for train in Self.trains do
   if (train = trainId) then
     podj.RecordOriginNow();

 TrainDb.Trains[trainId].AddOrUpdatePOdj(Self, podj);

 if ((was) and (not TrainDb.Trains[trainId].IsPOdj(Self))) then
  begin
   // PODJ bylo odstraneno -> rozjet soupravu pred navestidlem i kdyz neni na zastavovaci udalosti
   // aktualizaci rychlosti pro vsechny signalJCRef bychom nemeli nic pokazit
   for nav in Self.signalJCRef do
     TBlkSignal(nav).UpdateRychlostTrain(true);
  end;

 // Pri zruseni / zavedei PODJ aktualizovat rychlsot loko, ktera prijizdi,
 // protoze muze dojit ke zmene rychlosti
 jc := JCDb.FindActiveJCWithTrack(Self.id);
  if (jc <> nil) then
    TBlkSignal(jc.signal).UpdateRychlostTrain(true);

 Self.PropagatePOdjToRailway();
 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrack.CheckPOdjChanged();
var traini: Integer;
    shouldChange: Boolean;
    podj: TPOdj;
    signal: TBlk;
    area: TArea;
    train: TTrain;
begin
 shouldChange := false;

 for traini in Self.trains do
  begin
   train := TrainDb.Trains[traini];

   if ((train.IsPOdj(Self)) and (train.GetPOdj(Self).changed)) then
    begin
     podj := train.GetPOdj(Self);

     // prehravani zvukove vystrahy
     if ((not shouldChange) and (Self.IsStujForTrain(train))) then
      begin
        if ((podj.phase_old = ppPreparing) and (podj.GetPhase() = ppGoingToLeave)) then
          for area in Self.m_areas do
            area.BlkPlaySound(Self, TAreaRights.write, _SND_STAVENI_VYZVA)

        else if ((podj.phase_old = ppGoingToLeave) and (podj.GetPhase() = ppSoundLeave)) then
          for area in Self.m_areas do
            area.BlkPlaySound(Self, TAreaRights.write, _SND_NENI_JC);
      end;

     if (train.GetPOdj(Self).DepRealDelta() < 0) then
      begin
       train.RemovePOdj(Self);

       // tvrda aktualizace rychlosti soupravy
       if ((train = Self.trainL) or (train = Self.trainSudy)) then
        begin
         for signal in Self.signalJCRef do
           if (((TBlkSignal(signal).direction = THVStanoviste.sudy) and (train = Self.trainL)) or
               ((TBlkSignal(signal).direction = THVStanoviste.lichy) and (train = Self.trainSudy))) then
             TBlkSignal(signal).UpdateRychlostTrain(true);
        end;
      end else
       train.GetPOdj(Self).changed := false;

     shouldChange := true;
    end;
  end;

 if (Self.TrainPredict <> nil) then
  begin
   train := Self.TrainPredict;
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

////////////////////////////////////////////////////////////////////////////////

// predvidane odjezdy se mazou pres bloky, aby bloky zavolaly Change().
procedure TBlkTrack.ClearPOdj();
var train: Integer;
    change: Boolean;
begin
 change := false;

 for train in Self.trains do
  begin
   if (TrainDb.Trains[train].IsPOdj(Self)) then
    begin
     TrainDb.Trains[train].RemovePOdj(Self);
     change := true;
    end;
  end;

 if ((Self.TrainPredict <> nil) and (Self.TrainPredict.IsPOdj(Self))) then
  begin
   Self.TrainPredict.RemovePOdj(Self);
   change := true;
  end;

 if (change) then
   Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrack.PropagatePOdjToRailway();
var signal: TBlk;
begin
 for signal in Self.signalJCRef do
   TBlkSignal(signal).PropagatePOdjToTrat();
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrack.IsStujForTrain(train: TTrain): Boolean;
begin
 if (not Self.trains.Contains(train.index)) then Exit(false);
 if (Self.signalJCRef.Count = 0) then Exit(true);

 if (Self.signalJCRef.Count = 1) then
  begin
   if (not TBlkSignal(Self.signalJCRef[0]).IsGoSignal()) then Exit(true);
   if (TBlkSignal(Self.signalJCRef[0]).direction = THvStanoviste.lichy) then
     Exit(train <> Self.trainSudy)
   else
     Exit(train <> Self.trainL);
  end;

 if (Self.signalJCRef.Count = 2) then
  begin
   if ((Self.trains.Count = 1) and (not TBlkSignal(Self.signalJCRef[0]).IsGoSignal()) and
        (not TBlkSignal(Self.signalJCRef[1]).IsGoSignal())) then Exit(true);
   if ((Self.trains.Count >= 2) and ((not TBlkSignal(Self.signalJCRef[0]).IsGoSignal()) or
        (not TBlkSignal(Self.signalJCRef[1]).IsGoSignal()))) then Exit(true);
   if ((Self.trains.Count > 2) and (Self.trainL <> train) and (Self.trainSudy <> train)) then Exit(true);
 end;

 Result := false;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrack.PanelStateString(): string;
var fg, bg, nebarVetve, sfg, sbg: TColor;
    Blk: TBlk;
    traini, i: Integer;
begin
 // Pro blok trati se vola take
 Result := IntToStr(Integer(btTrack))+';'+IntToStr(Self.id)+';';;

 nebarVetve := $A0A0A0;

 // --- Popredi ---

 case (Self.occupied) of
  TTrackState.disabled : fg := clFuchsia;
  TTrackState.none     : fg := $A0A0A0;
  TTrackState.free : fg := $A0A0A0;
  TTrackState.occupied : fg := clRed;
 else
  fg := clFuchsia;
 end;

 // zobrazeni zakazu odjezdu do trati
 if ((fg = $A0A0A0) and (Self.typ = btRT) and (TBlkRT(Self).inRailway > -1)) then
  begin
   Blocks.GetBlkByID(TBlkRT(Self).inRailway, Blk);
   if ((Blk <> nil) and (Blk.typ = btRailway)) then
     if ((Blk as TBlkRailway).departureForbidden) then
       fg := clBlue;
  end;

 // neprofilove deleni v useku
 if ((fg = $A0A0A0) and (Self.IsNeprofilJC())) then
   fg := clYellow;

 // zaver
 if (((Self.occupied) = TTrackState.free) and (Self.typ = btTrack) and
     (Self.GetSettings().RCSAddrs.Count > 0)) then
  begin
   case (Self.Zaver) of
    TZaver.vlak   : fg := clLime;
    TZaver.posun  : fg := clWhite;
    TZaver.nouz   : fg := clAqua;
    TZaver.ab     : if (diag.showZaver) then fg := $707070 else fg := $A0A0A0;
    TZaver.staveni: if (diag.showZaver) then fg := clBlue;
   end;//case
  end;

 // porucha BP v trati
 if ((Self.typ = btRT) and (TBlkRT(Self).bpError)) then fg := clAqua;

 if (fg = clYellow) then
   nebarVetve := clYellow;

 Result := Result + ownConvert.ColorToStr(fg) + ';';

 // --- Pozadi ---

 bg := clBlack;
 if (Self.note <> '') then bg := clTeal;
 if (Self.lockout <> '') then bg := clOlive;

 if (not Self.DCC) then bg := clMaroon;
 if (Self.shortCircuit = TBoosterSignal.error) then bg := clFuchsia;
 if ((Self.power <> TBoosterSignal.ok) or
    (Self.shortCircuit = TBoosterSignal.undef)) then bg := clBlue;

 Result := Result + ownConvert.ColorToStr(bg) + ';';

 Result := Result + IntToStr(ownConvert.BoolToInt(Self.NUZ)) + ';' +
                    IntToStr(Integer(Self.jcEnd)) + ';' +
                    ownConvert.ColorToStr(nebarVetve) + ';';

 // seznam souprav
 Result := Result + '{';
 for i := 0 to Self.trains.Count-1 do
  begin
   traini := Self.trains[i];
   sfg := fg;
   sbg := bg;

   if (Self.occupied = TTrackState.free) then
     sfg := clAqua;

   Result := Result + '(' + TrainDb.Trains[traini].name + ';' +
                            IntToStr(ownConvert.BoolToInt(TrainDb.Trains[traini].sdata.dir_L)) +
                            IntToStr(ownConvert.BoolToInt(TrainDb.Trains[traini].sdata.dir_S)) + ';';

   if ((TrainDb.Trains[traini].stationTo = TrainDb.Trains[traini].station) and (sbg = clBlack)) then
     sbg := clSilver;

   // predvidany odjezd
   if (TrainDb.Trains[traini].IsPOdj(Self)) then
     predvidanyOdjezd.GetPOdjColors(TrainDb.Trains[traini].GetPOdj(Self), sfg, sbg);

   Result := Result + ownConvert.ColorToStr(sfg) + ';' +
                      ownConvert.ColorToStr(sbg) + ';';

   if (Self.trainMoving = i) then
    Result := Result + ownConvert.ColorToStr(clYellow) + ';';

   Result := Result + ')';
  end;

 // predpovidana souprava
 if (Self.TrainPredict <> nil) then
  begin
   // predvidany odjezd
   sfg := fg;
   sbg := bg;

   if (Self.TrainPredict.IsPOdj(Self)) then
     predvidanyOdjezd.GetPOdjColors(Self.TrainPredict.GetPOdj(Self), sfg, sbg);

   Result := Result + '(' + Self.TrainPredict.name + ';' +
              '00;' +
              ownConvert.ColorToStr(sfg) + ';' +
              ownConvert.ColorToStr(sbg) + ';)';
  end;

 Result := Result + '}';
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrack.RealBoosterShortCircuit(): TBoosterSignal;
begin
 if (Boosters.ContainsKey(Self.m_settings.boosterId)) then
  begin
   Result := Boosters[Self.m_settings.boosterId].overload;
  end else begin
   Result := TBoosterSignal.ok;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrack.CanStandTrain(): Boolean;
begin
 Result := (Self.spnl.stationTrack or Self.spnl.trainPos);
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrack.CanBeNextVB(vbs: TList<TObject>; start: TBlk): Boolean;
var vbs_plus_me: TList<TObject>;
begin
 if (vbs.Contains(Self)) then
   Exit(false);

 vbs_plus_me := TList<TObject>.Create();

 try
   vbs_plus_me.AddRange(vbs);
   vbs_plus_me.Add(Self);

   Result := ((JCDb.IsAnyJCWithPrefix(start as TBlkSignal, vbs_plus_me)) or
              (MultiJCDb.IsAnyMJCWithPrefix(start as TBlkSignal, vbs_plus_me)));
 finally
   vbs_plus_me.Free();
 end;
end;

function TBlkTrack.CanBeKC(vbs: TList<TObject>; start: TBlk): Boolean;
begin
 Result := ((JCDb.FindJC(start as TBlkSignal, vbs, Self) <> nil) or
            (MultiJCDb.Find(start as TBlkSignal, vbs, Self) <> nil));
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrack.GetNavL(): TBlk;
var blk: TBlk;
begin
 for blk in Blocks do
   if ((blk.typ = btSignal) and (TBlkSignal(blk).trackId = Self.id) and (TBlkSignal(blk).direction = THVStanoviste.lichy)) then
     Exit(blk);
 Result := nil;
end;

function TBlkTrack.GetNavS(): TBlk;
var blk: TBlk;
begin
 for blk in Blocks do
   if ((blk.typ = btSignal) and (TBlkSignal(blk).trackId = Self.id) and (TBlkSignal(blk).direction = THVStanoviste.sudy)) then
     Exit(blk);
 Result := nil;
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit

