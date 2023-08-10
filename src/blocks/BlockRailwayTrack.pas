unit BlockRailwayTrack;

{ RAILWAY TRACK technological block definition. }

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

uses BlockTrack, Classes, Block, IniFiles, SysUtils, IdContext, rrEvent,
  Generics.Collections, Area, THnaciVozidlo, Train, RegularExpressions,
  TrainSpeed;

type
  TBlkRTStopEvents = class // zastavka v jednom smeru
    stop: TRREv;

    slow: record
      enabled: Boolean;
      speed: Integer;
      ev: TRREv;
    end;

    constructor Create(); overload;
    constructor Create(ini_tech: TMemIniFile; const section: string; const prefix: string); overload;
    destructor Destroy(); override;

    procedure LoadFromFile(ini_tech: TMemIniFile; const section: string; const prefix: string);
    procedure SaveToFile(ini_tech: TMemIniFile; const section: string; const prefix: string);
  end;

  TBlkRTStop = class // zastavka na TU
    evL: TBlkRTStopEvents;
    evS: TBlkRTStopEvents;
    trainType: string;
    maxLength: Integer;
    delay: TTime;

    constructor Create(); overload;
    constructor Create(ini_tech: TMemIniFile; const section: string); overload;
    destructor Destroy(); override;

    procedure LoadFromFile(ini_tech: TMemIniFile; const section: string);
    procedure SaveToFile(ini_tech: TMemIniFile; const section: string);
  end;

  TBlkRTSettings = record
    stop: TBlkRTStop;
    // obsahuje id bloku navestidla, pokud neni TU kryty v danem smeru, obsahuje -1
    // vyuzivano pro autoblok
    signalLid, signalSid: Integer; // odkaz na kryci navestidlo TU v lichem smeru a kryci navestidlo v sudem smeru
    speedsL, speedsS: TList<TTrainSpeed>; // when speedS is empty, speedL is considered also for even direction
  end;

  TBlkRTState = record
    inRailway: Integer; // tady je ulozeno id bloku trati, v jake se blok nachazi; pokud se nenachazi v trati -> -1

    stopStopped: Boolean; // jakmile zastavim soupravu v zastavce, nastavim sem true; pokud souprava jede, je zde false
    stopRunTime: TDateTime; // tady je ulozen cas, kdy se ma souprava ze zastavky rozjet
    // tady si pamatuji, jakou rychlost mela souprava puvodne (mela by to byt tratova, toto je tu pro rozsireni zastavek nejen do trati)
    stopEnabled: Boolean; // zastavku lze z panelu zapnout a vypnout (v zakladnim stavu je zapla)
    stopPassed: Boolean; // tady je ulozeno true, pokud souprava zastavku jiz projela
    stopSlowReady: Boolean; // jestli je TU pripraveny ke zpomalovani soupravy v zastavce
    stopSoundStep: Cardinal; // krok prehravani zvuku
    // 0 = pripraveno, 1 = prehrana pistalka vypravciho, 2 = prehrano zavreni dveri, 3 = prehrana houkacka

    // bpInBlk = kontroluji obsazeni bloku, pri uvolneni useku bez predani dale vyhlasit poruchu BP
    bpInBlk: Boolean; // jestli je v useku zavedena blokova podminka
    bpError: Boolean; // jestli nastala porucha blokove podminky
    trainSpeedUpdateIter: Integer; // pocet zbyvajicich iteraci do nastaveni rychlost soupravy
    updateSignals: Boolean; // whether to update signals (used for init update)
  end;

  TBlkRT = class(TBlkTrack)
  private const
    _def_rt_stav: TBlkRTState = (inRailway: - 1; stopStopped: false; stopEnabled: true; stopSlowReady: false;
      stopSoundStep: 0; bpError: false; trainSpeedUpdateIter: 0; updateSignals: false; );

  private
    m_rtSettings: TBlkRTSettings;
    m_rtState: TBlkRTState;

    m_signalCovering, m_railway: TBlk; // odkaz na kryci navestidla v lichem a sudem smeru
    // pro pristup k temto blokum pouzivat property bez f, toto jsou pouze pomocne promenne!

    procedure StopUpdate();
    procedure StopRunTrain();
    procedure StopStopTrain();

    procedure MenuZastClick(SenderPnl: TIdContext; SenderOR: TObject; new_state: Boolean);
    procedure MenuJEDTrainClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuRBPClick(SenderPnl: TIdContext; SenderOR: TObject);

    procedure UpdateBP(); // technologie blokove podminky, resi veskere predavani souprav mezi TU a sekcemi TU

    function GetRailway(): TBlk;
    function GetSignalCover(): TBlk;
    // vrati kryci navestidlo TU, jinak nil (pokud blok neni v aktualnim smeru trati kryty zadnym navestidlem)
    function GetSignalCoverL(): TBlk;
    function GetSignalCoverS(): TBlk;
    // vrati, jestli je trat zpusobila prace: jestli existuje a ma smer AtoB nebo BtoA, viz property \tratSmer
    function GetRailwayReady(): Boolean;
    // vrati predchozi TU v zavislosti na smeru trati, pokud smer neni AtoB nebo BtoA, vrati nil, viz property \prevTU
    function GetPrevRT(): TBlkRT;
    // vrati dalsi TU v zavislosti na smeru trati, pokud smer neni AtoB nebo BtoA, vrati nil, viz property \nextTU
    function GetNextRT(): TBlkRT;
    function GetSectOccupy(): TTrackState; // vrati stav obsazeni cele sekce, mozne volat pouze u Section Masteru
                                           // POZOR: tato metoda, resp property \sectObsazeno by mela byt pouzivana VELMI OPATRNE, casto je vhodnejsi property \sectReady, ktera zahrnuje i poruchu BP
    // vrati sectMaster kazdeho TU, pokud je TU sam sobe sectMaster, obsahuje referenci na self, viz property \sectMaster
    function GetSectMaster(): TBlkRT;
    // vrati dalsi navestidlo v trati, v krajnim pripade az hranicni navestidlo cele trati podle aktualniho smeru trati, viz property \nextNav
    function GetNextSignal(): TBlk;
    // vrati, zda-li je sekce pripravena pro vjezd vlaku do ni, viz property \sectReady, mozne volat pouze u sectMaster
    function GetSectReady(): Boolean;

    procedure PanelPotvrSekvRBP(Sender: TIdContext; success: Boolean); // callback potvrzovaci sekvence RBP

    procedure UpdateSignals(); // aktualizuje navest krycich navestidel
    procedure UpdateTrainSpeed(); // aktualizuje rychlost soupravy v TU (pocita s \sprRychUpdateIter)

    procedure SetSpeedUpdate(state: Boolean); // nastavi \sprRychUpdateIter
    function GetSpeedUpdate(): Boolean; // vrati, jestli bezi odpocet \sprRychUpdateIter

    function GetReady(): Boolean; // jestli je usek pripraveny na vjeti soupravy
    function IsStopSlowedDown(): Boolean;

    function mIsStop(): Boolean;
    function mIsStopL(): Boolean;
    function mIsStopS(): Boolean;

    procedure SetBPError(state: Boolean); // nastavi stav poruchy blokove podminky
    procedure AddTrain(spr: Integer);

  public
    // reference na tratovy usek blize zacatku trati (lTU) a TU blize konci trati (sTU), tyto refence nastavuje trat pri inicializaci, nebo zmene konfigurace trati
    lRT, sRT: TBlkRT;

    lsectMaster: TBlkRT; // sectMaster pro lichy smer trati
    // pokud jsem sectMaster, zde jsou ulozeny useky me sekce v lichem smeru; pokud nejsem sectMaster, je tento senzam prazdny
    lsectTracks: TList<TBlkRT>;

    ssectMaster: TBlkRT; // sectMaster pro sudy smer trati
    // pokud jsem sectMaster, zde jsou ulozeny useky me sekce v sudem smeru; pokud nejsem sectMaster, je tento senzam prazdny
    ssectTracks: TList<TBlkRT>;

    constructor Create(index: Integer);
    destructor Destroy(); override;

    function GetSettings(): TBlkRTSettings; overload;
    procedure SetSettings(data: TBlkRTSettings); overload;
    procedure SetGlobalSettings(data: TBlkSettings); override;

    procedure LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile); override;
    procedure SaveData(ini_tech: TMemIniFile; const section: string); override;
    procedure SaveState(ini_stat: TMemIniFile; const section: string); override;

    procedure Enable(); override;
    procedure Disable(); override;

    procedure Update(); override;
    procedure Change(now: Boolean = false); override;
    // aktualizace TU z trati, vola se zejemna pri zmene smeru a jeho ucel je nastavit navestidla autobloku podle smeru trati
    procedure ChangeFromRailway();

    function ShowPanelMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TAreaRights): string; override;
    procedure PanelClick(SenderPnl: TIdContext; SenderOR: TObject; Button: TPanelButton; rights: TAreaRights;
      params: string = ''); override;
    procedure PanelMenuClick(SenderPnl: TIdContext; SenderOR: TObject; item: string; itemindex: Integer); override;

    procedure CreateNavRefs(); // navestidlum autobloku nastavi UsekPred a smer
    procedure RemoveTURefs(); // zrusi UsekPred navetidlum autobloku

    // tato metoda ma smysl pouze pro krajni TU trati a resi radne odstraneni obsahu useku z trati
    procedure ReleasedFromJC(); // obsah useku (ne nutne souprava!) byl prevzat z krajniho useku trati jizdni cestou

    procedure AddTrainL(index: Integer); override;
    procedure AddTrainS(index: Integer); override;
    procedure RemoveTrains(); override;
    procedure RemoveTrain(index: Integer); override;

    function speed(train: TTrain): Cardinal;

    // pro vyznam properties viz hlavicky getteru a setteru
    property rtState: TBlkRTState read m_rtState;
    property inRailway: Integer read m_rtState.inRailway write m_rtState.inRailway;
    property bpInBlk: Boolean read m_rtState.bpInBlk write m_rtState.bpInBlk;
    property sectOccupied: TTrackState read GetSectOccupy;
    property sectReady: Boolean read GetSectReady;
    property speedUpdate: Boolean read GetSpeedUpdate write SetSpeedUpdate;

    property railway: TBlk read GetRailway;
    property signalCover: TBlk read GetSignalCover;
    property signalCoverL: TBlk read GetSignalCoverL;
    property signalCoverS: TBlk read GetSignalCoverS;
    property railwayReady: Boolean read GetRailwayReady;

    property prevRT: TBlkRT read GetPrevRT;
    property nextRT: TBlkRT read GetNextRT;
    property sectMaster: TBlkRT read GetSectMaster;
    property nextSignal: TBlk read GetNextSignal;
    property ready: Boolean read GetReady;
    property bpError: Boolean read m_rtState.bpError write SetBPError;
    property isStop: Boolean read mIsStop;
    property stopL: Boolean read mIsStopL;
    property stopS: Boolean read mIsStopS;
    property stopSlowedDown: Boolean read IsStopSlowedDown;

  end;

implementation

uses TrainDb, BlockDb, TCPServerPanel, BlockRailway, BlockSignal, TJCDatabase,
  logging, TechnologieJC, ownStrUtils, THVDatabase, IfThenElse, ConfSeq, ownConvert;

/// /////////////////////////////////////////////////////////////////////////////

constructor TBlkRT.Create(index: Integer);
begin
  inherited Create(index);

  Self.m_globSettings.typ := btRT;
  Self.m_rtState := _def_rt_stav;

  Self.m_railway := nil;
  Self.m_signalCovering := nil;
  Self.lsectMaster := nil;
  Self.lsectTracks := TList<TBlkRT>.Create();
  Self.ssectMaster := nil;
  Self.ssectTracks := TList<TBlkRT>.Create();
  Self.bpInBlk := false;
  Self.m_rtSettings.speedsL := TList<TTrainSpeed>.Create();
  Self.m_rtSettings.speedsS := TList<TTrainSpeed>.Create();
  Self.m_rtSettings.stop := nil;
end;

destructor TBlkRT.Destroy();
begin
  if (Assigned(Blocks)) then
    for var blk: TBlk in Blocks do
      if (blk.typ = TBlkType.btRailway) then
        TBlkRailway(blk).RecalcTracks();

  Self.lsectTracks.Free();
  Self.ssectTracks.Free();
  Self.m_rtSettings.stop.Free();
  Self.m_rtSettings.speedsL.Free();
  Self.m_rtSettings.speedsS.Free();

  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkRT.LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile);
begin
  inherited LoadData(ini_tech, section, ini_rel, ini_stat);

  Self.m_rtSettings.signalLid := ini_tech.ReadInteger(section, 'navL', -1);
  Self.m_rtSettings.signalSid := ini_tech.ReadInteger(section, 'navS', -1);

  Self.m_rtSettings.speedsL.Clear();
  Self.m_rtSettings.speedsS.Clear();

  var rychlostiL := ini_tech.ReadString(section, 'rychlostiL', '');
  var rychlostiS := ini_tech.ReadString(section, 'rychlostiS', '');
  if (rychlostiL = '') then
  begin
    // backward-compatible mode (trans1:speed1,trans2:speed2)
    rychlostiL := ini_tech.ReadString(section, 'rychlosti', '');
    if (rychlostiL = '') then // even mode backward-compatible mode (single speed)
      rychlostiL := ini_tech.ReadString(section, 'rychlost', '');
  end;

  try
    Self.m_rtSettings.speedsL := TTrainSpeed.IniLoad(rychlostiL);
    Self.m_rtSettings.speedsS := TTrainSpeed.IniLoad(rychlostiS);
  except
    on E:Exception do
    begin
      Self.m_rtSettings.speedsL.Clear();
      Self.m_rtSettings.speedsS.Clear();
      Self.Log('Nelze nacist rychlosti: '+E.Message, llError, lsData);
    end;
  end;

  Self.bpInBlk := ini_stat.ReadBool(section, 'bpInBlk', false);

  if (Self.m_rtSettings.speedsL.Count = 0) then
    Self.Log('Nenačtena traťová rychlost', llError, lsData);

  if ((ini_tech.ReadString(section, 'zast_ev_lichy_zast', '') <> '') or
    (ini_tech.ReadString(section, 'zast_ev_sudy_zast', '') <> '')) then
    Self.m_rtSettings.stop := TBlkRTStop.Create(ini_tech, section)
  else if (Assigned(Self.m_rtSettings.stop)) then
    Self.m_rtSettings.stop.Free();
end;

procedure TBlkRT.SaveData(ini_tech: TMemIniFile; const section: string);
begin
  inherited SaveData(ini_tech, section);

  if (Self.m_rtSettings.signalLid <> -1) then
    ini_tech.WriteInteger(section, 'navL', Self.m_rtSettings.signalLid);

  if (Self.m_rtSettings.signalSid <> -1) then
    ini_tech.WriteInteger(section, 'navS', Self.m_rtSettings.signalSid);

  ini_tech.WriteString(section, 'rychlostiL', TTrainSpeed.IniStr(Self.m_rtSettings.speedsL));
  if (Self.m_rtSettings.speedsS.Count > 0) then
    ini_tech.WriteString(section, 'rychlostiS', TTrainSpeed.IniStr(Self.m_rtSettings.speedsS));

  if (Self.isStop) then
    Self.m_rtSettings.stop.SaveToFile(ini_tech, section);
end;

procedure TBlkRT.SaveState(ini_stat: TMemIniFile; const section: string);
begin
  inherited SaveState(ini_stat, section);

  if (Self.bpInBlk) then
    ini_stat.WriteBool(section, 'bpInBlk', Self.bpInBlk);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkRT.GetSettings(): TBlkRTSettings;
begin
  Result := Self.m_rtSettings;
end;

procedure TBlkRT.SetSettings(data: TBlkRTSettings);
begin
  if (Self.m_rtSettings.stop <> data.stop) and (Assigned(Self.m_rtSettings.stop)) then
    Self.m_rtSettings.stop.Free();

  if (Self.m_rtSettings.speedsL <> data.speedsL) then
    Self.m_rtSettings.speedsL.Free();
  if (Self.m_rtSettings.speedsS <> data.speedsS) then
    Self.m_rtSettings.speedsS.Free();

  Self.m_rtSettings := data;
  Self.Change();
end;

procedure TBlkRT.SetGlobalSettings(data: TBlkSettings);
var blk: TBlk;
begin
  inherited;

  // id can change -> recalc new railway
  for blk in Blocks do
    if (blk.typ = TBlkType.btRailway) then
      TBlkRailway(blk).RecalcTracks();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkRT.Enable();
begin
  inherited;
  Self.m_rtState.bpError := false;

  // Aktiaovovat navestidla rucne, aby se rovnou nastavily navesti v trati
  begin
    var signal: TBlkSignal := Blocks.GetBlkSignalByID(Self.m_rtSettings.signalLid);
    if (signal <> nil) then
      signal.Enable();
  end;

  begin
    var signal: TBlkSignal := Blocks.GetBlkSignalByID(Self.m_rtSettings.signalSid);
    if (signal <> nil) then
      signal.Enable();
  end;

  // do not call UpdateSignals directly, wait for first Update to have all other blocks (railway, signals) initialized
  Self.m_rtState.updateSignals := true;
end;

procedure TBlkRT.Disable();
begin
  Self.m_rtState.bpError := false;
  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkRT.Update();
begin
  inherited;

  if ((Self.inRailway > -1) and (Self.occupied = TTrackState.occupied) and (Self.IsTrain()) and (Self.isStop)) then
    Self.StopUpdate();

  if (Self.m_rtState.updateSignals) then
  begin
    Self.m_rtState.updateSignals := false;
    Self.UpdateSignals();
  end;

  Self.UpdateTrainSpeed();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkRT.Change(now: Boolean = false);
begin
  inherited;

  // aktualizovat predavani blokove podminky
  Self.UpdateBP();

  // UpdateNavest musi byt volano i u non-master bloku, zajistuje totiz i zhasinani
  // navestidel v druhem smeru (v druhem smeru muzou byt sectMaster uplne jinak)
  Self.UpdateSignals();

  if (Self.railway <> nil) then
  begin
    Self.railway.Change();

    // propagace zmen k masterTU sekce:
    if ((Self.sectMaster <> nil) and (Self.sectMaster <> Self)) then
      Self.sectMaster.Change();

    // propagace zmen do vedlejsi sekce (napriklad kvuli navesti)
    if ((Self.sectMaster = Self) and (Self.prevRT <> nil) and (Self.prevRT.sectMaster <> nil)) then
      Self.prevRT.sectMaster.Change();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////
// aktualizace stavu zastavky

procedure TBlkRT.StopUpdate();
begin
  if (not Self.rtState.stopStopped) then
  begin
    if ((not Self.rtState.stopEnabled) or (Self.rtState.stopPassed) or
      (Self.train.length > Self.m_rtSettings.stop.maxLength) or (Self.train.front <> Self)) then
      Exit();

    // kontrola spravneho smeru
    if (((Self.train.direction = THVSite.odd) and (not Self.stopL)) or ((Self.train.direction = THVSite.even) and
      (not Self.stopS))) then
      Exit();

    // kontrola typu soupravy
    if (not TRegEx.IsMatch(Self.train.typ, Self.m_rtSettings.stop.trainType)) then
      Exit();

    // zpomalovani pred zastavkou:
    if (Self.m_rtState.stopSlowReady) then
    begin
      case (Self.train.direction) of
        THVSite.odd:
          begin
            if (Self.m_rtSettings.stop.evL.slow.enabled) then
            begin
              if (not Self.m_rtSettings.stop.evL.slow.ev.enabled) then
                Self.m_rtSettings.stop.evL.slow.ev.Register(Self.train.index);

              if ((Self.m_rtSettings.stop.evL.slow.enabled) and
                (Self.train.wantedSpeed > Self.m_rtSettings.stop.evL.slow.speed) and
                (Self.m_rtSettings.stop.evL.slow.ev.IsTriggerred(Self, true))) then
              begin
                Self.train.speed := Self.m_rtSettings.stop.evL.slow.speed;
                Self.m_rtState.stopSlowReady := false;
                Self.speedUpdate := false;
                Self.m_rtSettings.stop.evL.slow.ev.Unregister();
              end;
            end;
          end;

        THVSite.even:
          begin
            if (Self.m_rtSettings.stop.evS.slow.enabled) then
            begin
              if (not Self.m_rtSettings.stop.evS.slow.ev.enabled) then
                Self.m_rtSettings.stop.evS.slow.ev.Register(Self.train.index);

              if ((Self.train.wantedSpeed > Self.m_rtSettings.stop.evS.slow.speed) and
                (Self.m_rtSettings.stop.evS.slow.ev.IsTriggerred(Self, true))) then
              begin
                Self.train.speed := Self.m_rtSettings.stop.evS.slow.speed;
                Self.m_rtState.stopSlowReady := false;
                Self.speedUpdate := false;
                Self.m_rtSettings.stop.evS.slow.ev.Unregister();
              end;
            end;
          end;
      end; // case
    end;

    if ((Self.occupied <> TTrackState.occupied) or (not Self.IsTrain())) then
      Exit();

    // zastavovani v zastavce
    case (Self.train.direction) of
      THVSite.odd:
        begin
          if (not Self.m_rtSettings.stop.evL.stop.enabled) then
            Self.m_rtSettings.stop.evL.stop.Register(Self.train.index);

          if (Self.m_rtSettings.stop.evL.stop.IsTriggerred(Self, true)) then
          begin
            Self.StopStopTrain();
            Self.speedUpdate := false;
            Self.m_rtSettings.stop.evL.stop.Unregister();
          end;
        end;

      THVSite.even:
        begin
          if (not Self.m_rtSettings.stop.evS.stop.enabled) then
            Self.m_rtSettings.stop.evS.stop.Register(Self.train.index);

          if (Self.m_rtSettings.stop.evS.stop.IsTriggerred(Self, true)) then
          begin
            Self.StopStopTrain();
            Self.speedUpdate := false;
            Self.m_rtSettings.stop.evS.stop.Unregister();
          end;
        end;
    end; // case
  end else begin

    // osetreni rozjeti vlaku z nejakeho pochybneho duvodu
    // pokud se souprava rozjede, koncim zastavku
    if ((Self.train.speed <> 0) or (not Self.train.IsSpeedOverride())) then
    begin
      Self.m_rtState.stopStopped := false;
      Self.Change(); // change je dulezite volat kvuli menu
    end;

    // prehravani zvuku pri rozjezdu
    case (Self.rtState.stopSoundStep) of
      0:
        begin
          if (now >= Self.rtState.stopRunTime - EncodeTime(0, 0, 4, 0)) then
          begin
            Self.m_rtState.stopSoundStep := 1;
            Self.train.ToggleHouk('trubka vlakvedoucího');
          end;
        end;

      1:
        begin
          if (now >= Self.rtState.stopRunTime - EncodeTime(0, 0, 2, 0)) then
          begin
            Self.m_rtState.stopSoundStep := 2;
            Self.train.ToggleHouk('zavření dveří');
          end;
        end;
    end;

    // cekam na timeout na rozjeti vlaku
    if (now > Self.rtState.stopRunTime) then
    begin
      Self.train.ToggleHouk('houkačka krátká');
      Self.StopRunTrain();
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkRT.StopStopTrain();
begin
  Self.m_rtState.stopStopped := true;
  Self.m_rtState.stopSoundStep := 0;
  Self.m_rtState.stopRunTime := now + Self.m_rtSettings.stop.delay;

  try
    Self.train.EnableSpeedOverride(0, true);
  except

  end;

  Self.Change(); // change je dulezite volat kvuli menu
end;

procedure TBlkRT.StopRunTrain();
begin
  Self.m_rtState.stopStopped := false;
  Self.m_rtState.stopPassed := true;
  Self.m_rtState.stopSoundStep := 0;

  try
    Self.train.DisableSpeedOverride();
  except

  end;

  Self.Change(); // change je dulezite volat kvuli menu
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkRT.ShowPanelMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TAreaRights): string;
begin
  Result := inherited;

  var starI := Result.IndexOf('*');
  var debug: string := '';
  if (starI > -1) then
  begin
    debug := Copy(Result, starI-1, Length(Result)-starI+1);
    Result := Copy(Result, 0, starI-2);
  end;

  // zastavka
  if ((Self.inRailway > -1) and (Self.isStop)) then
  begin
    Result := Result + '-,';
    // pokud neni v zastavce zastavena souprava, lze zastavku vypinat a zapinat
    if (not Self.rtState.stopStopped) then
      Result := Result + ite(Self.rtState.stopEnabled, 'ZAST<,', 'ZAST>,');
  end;

  if (Self.bpError) then
    Result := Result + '!RBP,';

  Result := Result + debug;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkRT.PanelClick(SenderPnl: TIdContext; SenderOR: TObject; Button: TPanelButton; rights: TAreaRights;
  params: string = '');
begin
  case (Button) of
    F2:
      PanelServer.Menu(SenderPnl, Self, (SenderOR as TArea), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));

    ENTER:
      begin
        var handled := Self.MenuKCClick(SenderPnl, SenderOR);
        // no train moving here
        if (not handled) then
          PanelServer.Menu(SenderPnl, Self, (SenderOR as TArea), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));
      end;

    F1:
      begin
        var handled := Self.MenuVBClick(SenderPnl, SenderOR);
        if (not handled) then
          PanelServer.Menu(SenderPnl, Self, (SenderOR as TArea), Self.ShowPanelMenu(SenderPnl, SenderOR, rights))
      end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkRT.AddTrainL(index: Integer);
begin
  inherited;
  Self.AddTrain(index);
end;

procedure TBlkRT.AddTrainS(index: Integer);
begin
  inherited;
  Self.AddTrain(index);
end;

procedure TBlkRT.AddTrain(spr: Integer);
begin
  if ((Self.isStop) and (not Self.m_rtState.stopSlowReady)) then
    Self.m_rtState.stopSlowReady := true;

  // Zmena smeru soupravy muze nastat na zacatku i konci trati
  // tak, aby souprava byla vzdy rizeni spravnymi nasvestidly.
  // Souprava ve smeru A-->B vzdy jeden v lichem smeru

  if ((Self.railway <> nil) and (TBlkRailway(Self.railway).GetSettings().trackIds.Count > 0)) then
  begin
    if ((Self.id = TBlkRailway(Self.railway).GetSettings().trackIds[0])) then
    begin
      if (TBlkRailway(Self.railway).direction = TRailwayDirection.AtoB) then
      begin // vjizdim do trati
        if (Self.train.direction <> THVSite.odd) then
          Self.train.ChangeDirection();
      end else if (TBlkRailway(Self.railway).direction = TRailwayDirection.BtoA) then
      begin // vjizdim do posledniho useku ve smeru trati
        if (Self.train.direction <> TBlkSignal(TBlkRailway(Self.railway).signalA).direction) then
          Self.train.ChangeDirection();
      end;
    end;
    if ((Self.id = TBlkRailway(Self.railway).GetSettings().trackIds[TBlkRailway(Self.railway).GetSettings()
      .trackIds.Count - 1])) then
    begin
      if (TBlkRailway(Self.railway).direction = TRailwayDirection.BtoA) then
      begin // vjizdim do trati
        if ((Self.train.direction <> THVSite.even) and (TBlkRailway(Self.railway).GetSettings().trackIds.Count > 0))
        then
          Self.train.ChangeDirection();
      end else if (TBlkRailway(Self.railway).direction = TRailwayDirection.AtoB) then
      begin // vjizdim do posledniho useku ve smeru trati
        if (Self.train.direction <> TBlkSignal(TBlkRailway(Self.railway).signalB).direction) then
          Self.train.ChangeDirection();
      end;
    end;
  end;

  // kontrola zmeny OR trati, ve ktere jen jeden blok
  if (((Self.railway as TBlkRailway).direction >= TRailwayDirection.AtoB) and (Self.prevRT = nil) and
    (Self.nextRT = nil)) then
    TBlkRailway(Self.railway).TrainChangeOR(Self.train);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkRT.RemoveTrains();
begin
  for var train: Integer in Self.trains do
    Self.RemoveTrain(train);
end;

procedure TBlkRT.RemoveTrain(index: Integer);
var oldTrain: TTrain;
begin
  oldTrain := Self.train;

  inherited;

  if (Self.m_rtState.stopStopped) then
  begin
    // vlak, ktery oupsti TU a mel by stat v zastavce, je vracen do stavu, kdy se mu nastavuje rychlost
    // toto je pojistka, ke ktere by teoreticky nikdy nemelo dojit
    oldTrain.DisableSpeedOverride();
    Self.m_rtState.stopStopped := false;
  end;

  Self.m_rtState.stopPassed := false;
  Self.m_rtState.stopSlowReady := false;

  if (Self.stopL) then
  begin
    Self.m_rtSettings.stop.evL.stop.Unregister();
    if (Self.m_rtSettings.stop.evL.slow.enabled) then
      Self.m_rtSettings.stop.evL.slow.ev.Unregister();
  end;
  if (Self.stopS) then
  begin
    Self.m_rtSettings.stop.evS.stop.Unregister();
    if (Self.m_rtSettings.stop.evS.slow.enabled) then
      Self.m_rtSettings.stop.evS.slow.ev.Unregister();
  end;

  oldTrain.UpdateRailwaySpeed();

  // souprava uvolnena z useku, mozna bude nutne ji uvolnit z cele trati
  if (Self.railway <> nil) then
  begin
    var railway: TBlkRailway := TBlkRailway(Self.railway);

    // souprava vyjela z trate -> odstranit z trate
    if (not railway.IsTrainInAnyTU(oldTrain)) then
      railway.RemoveTrain(oldTrain);

    // zavolame uvolneni posledniho TU z jizdni cesty
    Self.ReleasedFromJC();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkRT.MenuZastClick(SenderPnl: TIdContext; SenderOR: TObject; new_state: Boolean);
begin
  if (not Self.rtState.stopStopped) then
    Self.m_rtState.stopEnabled := new_state;
end;

procedure TBlkRT.MenuJEDTrainClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  if (Self.rtState.stopStopped) then
    Self.StopRunTrain();
end;

procedure TBlkRT.MenuRBPClick(SenderPnl: TIdContext; SenderOR: TObject);
var csItems: TConfSeqItems;
begin
  csItems := TList<TConfSeqItem>.Create();
  try
    if (Self.IsTrain()) then
    begin
      csItems.Add(CSItem(Self, 'Smazání soupravy ' + Self.train.name + ' z úseku'));
      if ((Self.railway <> nil) and (not TBlkRailway(Self.railway).IsTrainInMoreTUs(Self.train))) then
        csItems.Add(CSItem(Self.railway, 'Smazání soupravy ' + Self.train.name + ' z tratě'));
      if (Blocks.GetBlkWithTrain(Self.train).Count = 1) then
        csItems.Add(CSItem(Self, 'Smazání soupravy ' + Self.train.name + ' z kolejiště'));
    end;

    PanelServer.ConfirmationSequence(SenderPnl, Self.PanelPotvrSekvRBP, SenderOR as TArea,
      'Zrušení poruchy blokové podmínky', GetObjsList(Self), csItems, True, False);
  finally
    csItems.Free();
  end;
end;

procedure TBlkRT.PanelPotvrSekvRBP(Sender: TIdContext; success: Boolean);
var old_train: Integer;
begin
  if (success) then
  begin
    old_train := Self.trainI;
    Self.bpInBlk := false;
    Self.bpError := false;
    if (Self.IsTrain()) then
    begin
      Self.RemoveTrains();
      var blks := Blocks.GetBlkWithTrain(TrainDb.trains[old_train]);
      try
        if (blks.Count = 0) then
          TrainDb.trains.Remove(old_train);
      finally
        blks.Free();
      end;

    end;

    if (Self.railway <> nil) then
      Self.railway.Change();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkRT.PanelMenuClick(SenderPnl: TIdContext; SenderOR: TObject; item: string; itemindex: Integer);
begin
  if ((item = 'JEĎ vlak') and (Self.m_rtState.stopStopped)) then
    Self.MenuJEDTrainClick(SenderPnl, SenderOR)
  else if (item = 'ZAST>') then
    Self.MenuZastClick(SenderPnl, SenderOR, true)
  else if (item = 'ZAST<') then
    Self.MenuZastClick(SenderPnl, SenderOR, false)
  else if (item = 'RBP') then
    Self.MenuRBPClick(SenderPnl, SenderOR)
  else
    inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkRT.GetRailway(): TBlk;
begin
  if (((Self.m_railway = nil) and (Self.rtState.inRailway <> -1)) or
    ((Self.m_railway <> nil) and (Self.m_railway.id <> Self.rtState.inRailway))) then
    Self.m_railway := Blocks.GetBlkRailwayByID(Self.rtState.inRailway);
  Result := Self.m_railway;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkRT.GetSignalCover(): TBlk;
var navPrevID: Integer;
begin
  if ((Self.railway = nil) or ((TBlkRailway(Self.railway).direction <> TRailwayDirection.AtoB) and
    (TBlkRailway(Self.railway).direction <> TRailwayDirection.BtoA))) then
    Exit(nil);

  case (TBlkRailway(Self.railway).direction) of
    TRailwayDirection.AtoB:
      navPrevID := Self.m_rtSettings.signalLid;
    TRailwayDirection.BtoA:
      navPrevID := Self.m_rtSettings.signalSid;
  else
    navPrevID := -1;
  end;

  if (((Self.m_signalCovering = nil) and (navPrevID <> -1)) or ((Self.m_signalCovering <> nil) and
    (Self.m_signalCovering.id <> navPrevID))) then
    Self.m_signalCovering := Blocks.GetBlkSignalByID(navPrevID);
  Result := Self.m_signalCovering;
end;

function TBlkRT.GetSignalCoverL(): TBlk;
begin
  Result := Blocks.GetBlkSignalByID(Self.m_rtSettings.signalLid);
end;

function TBlkRT.GetSignalCoverS(): TBlk;
begin
  Result := Blocks.GetBlkSignalByID(Self.m_rtSettings.signalSid);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkRT.GetRailwayReady(): Boolean;
begin
  Result := ((Self.railway <> nil) and ((TBlkRailway(Self.railway).direction = TRailwayDirection.AtoB) or
    (TBlkRailway(Self.railway).direction = TRailwayDirection.BtoA)));
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkRT.GetPrevRT(): TBlkRT;
begin
  if (not Self.railwayReady) then
    Exit(nil);

  case (TBlkRailway(Self.railway).direction) of
    TRailwayDirection.AtoB:
      Result := Self.lRT;
    TRailwayDirection.BtoA:
      Result := Self.sRT;
  else
    Result := nil;
  end;
end;

function TBlkRT.GetNextRT(): TBlkRT;
begin
  if (not Self.railwayReady) then
    Exit(nil);

  case (TBlkRailway(Self.railway).direction) of
    TRailwayDirection.AtoB:
      Result := Self.sRT;
    TRailwayDirection.BtoA:
      Result := Self.lRT;
  else
    Result := nil;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkRT.UpdateBP();
begin
  if ((not Self.railwayReady) or (not TBlkRailway(Self.railway).BP)) then
    Exit();

  if ((Self.prevRT = nil) and (Self.occupied = TTrackState.occupied)) then
  begin
    // nastala aktivace blokove podminky prvniho bloku trati
    Self.bpInBlk := true;
  end;

  // predavani soupravy z predchoziho TU do meho TU
  if ((Self.prevRT <> nil) and (Self.occupied = TTrackState.occupied) and (Self.prevRT.occupied = TTrackState.occupied)
    and ((Self.signalCover = nil) or (TBlkSignal(Self.signalCover).IsGoSignal()))) then
  begin
    // nastala aktivace blokove podminky
    Self.bpInBlk := true;

    if ((not Self.IsTrain()) and (Self.prevRT.IsTrain())) then
    begin
      if (Self.prevRT.Train.front = Self.prevRT) then
      begin
        Self.prevRT.Train.front := Self;
        Self.AddTrainL(Self.prevRT.trainI); // must be after setting front! must be before setting houkEvEnabled = true!
        Self.slowingReady := true;
        Self.houkEvEnabled := true;
        Self.speedUpdate := true;

        if (Self.nextRT = nil) then
        begin
          // souprava vstoupila do posledniho bloku trati
          // zmena stanic soupravy a hnacich vozidel v ni
          TBlkRailway(Self.railway).TrainChangeOR(Self.train);
        end;
      end else begin
        Self.AddTrainL(Self.prevRT.trainI);
        Self.slowingReady := false;
        Self.houkEvEnabled := false;
        Self.speedUpdate := false;
      end;
    end;
  end;

  // uvolnovani soupravy z TU (pokud je jiz predana do dalsiho TU)
  if ((Self.bpInBlk) and (Self.nextRT <> nil) and (Self.nextRT.Train = Self.train) and
    (Self.occupied = TTrackState.Free) and (Self.nextRT.occupied = TTrackState.occupied)) then
  begin
    Self.bpInBlk := false;
    Self.RemoveTrains();
    if (not TBlkRailway(Self.railway).occupied) then
      TBlkRailway(Self.railway).BP := false;
  end;

  // kontrola poruchy blokove podminky
  if (Self.bpInBlk) then
  begin
    if ((Self.occupied = TTrackState.Free) and (not Self.bpError) and
      ((Self.zaver = TZaver.no) or (Self.zaver = TZaver.ab))) then
      Self.bpError := true;
    if ((Self.occupied = TTrackState.occupied) and (Self.bpError)) then
      Self.bpError := false;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkRT.GetSectOccupy(): TTrackState;
var blk: TBlkRT;
  sectUseky: TList<TBlkRT>;
begin
  if (Self.railway = nil) then
    Exit(TTrackState.none);
  if (Self.occupied <= TTrackState.none) then
    Exit(TTrackState.occupied);

  // pozadavek na obsazenost sekce muze prijit i kdyz trat nema smer,
  // typicky kdyz se stavi JC do bezsouhlasove trati s automatickou
  // zmenou souhlasu
  // -> pro krajni useky trti vracime obsazenost prvni sekce

  case (TBlkRailway(Self.railway).direction) of
    TRailwayDirection.AtoB:
      sectUseky := Self.lsectTracks;
    TRailwayDirection.BtoA:
      sectUseky := Self.ssectTracks;
    TRailwayDirection.no:
      begin
        if (Self.sRT = nil) then
          sectUseky := Self.ssectTracks
        else
        begin
          if (Self.lRT = nil) then
            sectUseky := Self.lsectTracks
          else
            Exit(TTrackState.none);
        end;
      end;
  else
    Exit(TTrackState.none);
  end;

  for blk in sectUseky do
    if (blk.occupied <> TTrackState.Free) then
      Exit(blk.occupied);
  Exit(TTrackState.Free);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkRT.GetSectMaster(): TBlkRT;
begin
  if (Self.railway = nil) then
    Exit(nil);
  case (TBlkRailway(Self.railway).direction) of
    TRailwayDirection.AtoB:
      Result := Self.lsectMaster;
    TRailwayDirection.BtoA:
      Result := Self.ssectMaster;
  else
    Result := nil;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////
// vrati dalsi navestidlo v trati (pokud ma trat smer)
// pokud neni dalsi navestidlo autobloku, vrati hranicni navestidlo trati

function TBlkRT.GetNextSignal(): TBlk;
var blk: TBlkRT;
begin
  if (Self.railway = nil) or (TBlkRailway(Self.railway).direction = TRailwayDirection.no) then
    Exit(nil);

  blk := Self.nextRT;
  while ((blk <> nil) and (blk.sectMaster <> blk)) do
    blk := blk.nextRT;

  if (blk <> nil) then
    Exit(blk.signalCover)
  else
  begin
    case (TBlkRailway(Self.railway).direction) of
      TRailwayDirection.AtoB:
        Exit(TBlkRailway(Self.railway).signalB);
      TRailwayDirection.BtoA:
        Exit(TBlkRailway(Self.railway).signalA);
    end;
  end;

  Result := nil;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkRT.CreateNavRefs();
begin
  begin
    var signal: TBlkSignal := Blocks.GetBlkSignalByID(Self.m_rtSettings.signalLid);
    if ((signal <> nil) and (Self.lRT <> nil)) then
    begin
      signal.trackId := Self.lRT.id;
      signal.direction := THVSite.odd;
      signal.autoblok := true;
    end;
  end;

  begin
    var signal: TBlkSignal := Blocks.GetBlkSignalByID(Self.m_rtSettings.signalSid);
    if ((signal <> nil) and (Self.sRT <> nil)) then
    begin
      signal.trackId := Self.sRT.id;
      signal.direction := THVSite.even;
      signal.autoblok := true;
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkRT.RemoveTURefs();
begin
  Self.lRT := nil;
  Self.sRT := nil;
  Self.lsectMaster := nil;
  Self.lsectTracks.Clear();
  Self.ssectMaster := nil;
  Self.ssectTracks.Clear();
  Self.bpInBlk := false;
  Self.inRailway := -1;

  begin
    var signal := Blocks.GetBlkSignalByID(Self.m_rtSettings.signalLid);
    if (signal <> nil) then
      signal.trackId := -1;
  end;

  begin
    var signal := Blocks.GetBlkSignalByID(Self.m_rtSettings.signalSid);
    if (signal <> nil) then
      signal.trackId := -1;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////
// Tato metoda nastavuje kryci navestidlo sekce a zaroven kontroluje
// zruseni navesti do trati v pripade nahleho obsazeni prvni sekce trati.

procedure TBlkRT.UpdateSignals();
begin
  // kontrola zruseni navesti jizdni cesty pri obsazeni sekce trati:
  // tato metoda je volana vzdy pouze u sectMastera (tj. krajniho bloku sekce)
  if (Self.railway = nil) then
    Exit();

  // NASTAVOVANI NAVESTI AUTOBLOKU:

  // nejprve zhasneme navestidla v nespravnem smeru
  if ((TBlkRailway(Self.railway).direction = TRailwayDirection.AtoB) or
    (TBlkRailway(Self.railway).direction = TRailwayDirection.no)) then
  begin
    if (Self.m_rtSettings.signalSid > -1) then
    begin
      var signal: TBlkSignal := Blocks.GetBlkSignalByID(Self.m_rtSettings.signalSid);
      if (signal <> nil) then
        signal.signal := TBlkSignalCode(TBlkRailway(Self.railway).SignalCounterDirection());
    end;
  end;

  if ((TBlkRailway(Self.railway).direction = TRailwayDirection.BtoA) or
    (TBlkRailway(Self.railway).direction = TRailwayDirection.no)) then
  begin
    if (Self.m_rtSettings.signalLid > -1) then
    begin
      var signal: TBlkSignal := Blocks.GetBlkSignalByID(Self.m_rtSettings.signalLid);
      if (signal <> nil) then
        signal.signal := TBlkSignalCode(TBlkRailway(Self.railway).SignalCounterDirection());
    end;
  end;

  if (TBlkRailway(Self.railway).direction = TRailwayDirection.no) then
    Exit();

  // zrusit jizdni cestu muzeme pouze u sekce na kraji trati (v trati se rusi
  // navest autobloku)
  if ((Self.prevRT = nil) and (Self.sectOccupied = TTrackState.occupied) and (TBlkRailway(Self.railway).zaver)) then
  begin
    var jc := JCDb.FindActiveJCWithTrack(Self.id);
    if ((jc <> nil) and(not jc.waitForLastTrackOrRailwayOccupy) and (jc.state.destroyBlock < jc.data.tracks.Count - 1)) then
      JCDb.Cancel(Self);
  end;

  // nastavime kryci navestidlo
  if ((Self.signalCover <> nil) and (not TBlkSignal(Self.signalCover).ZAM) and
    (TBlkSignal(Self.signalCover).signal >= ncStuj)) then
  begin
    if (not Self.sectReady) then
    begin
      // sekce obsazena -> navestidlo na STUJ
      TBlkSignal(Self.signalCover).signal := ncStuj
    end else begin
      // sekce uvolnena -> hledame dalsi navestidlo
      case (TBlkRailway(Self.railway).signals) of
        TRailwaySignals.hradlo:
          TBlkSignal(Self.signalCover).signal := ncVolno;
        TRailwaySignals.autoblok:
          begin
            if ((Self.nextSignal = nil) or (not TBlkSignal(Self.nextSignal).IsGoSignal()) or
              (TBlkSignal(Self.nextSignal).IsOpakVystraha())) then
              TBlkSignal(Self.signalCover).signal := ncVystraha
            else if ((TBlkSignal(Self.nextSignal).FourtyKmph()) or (TBlkSignal(Self.nextSignal).signal = ncOpakOcek40))
            then
              TBlkSignal(Self.signalCover).signal := ncOcek40
            else
              TBlkSignal(Self.signalCover).signal := ncVolno;
          end;
      end;
    end;
  end;

end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkRT.ChangeFromRailway();
begin
  Self.UpdateSignals();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkRT.UpdateTrainSpeed();
begin
  if (Self.m_rtState.trainSpeedUpdateIter > 0) then
  begin
    Dec(Self.m_rtState.trainSpeedUpdateIter);
    if (Self.m_rtState.trainSpeedUpdateIter = 0) then
      if ((Self.IsTrain()) and (Self.slowingReady) and (Self.train.wantedSpeed > 0)) then
        Self.train.UpdateRailwaySpeed();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkRT.GetSpeedUpdate: Boolean;
begin
  Result := (Self.m_rtState.trainSpeedUpdateIter > 0);
end;

procedure TBlkRT.SetSpeedUpdate(state: Boolean);
begin
  if ((state) and (Self.m_rtState.trainSpeedUpdateIter = 0)) then
    Self.m_rtState.trainSpeedUpdateIter := 2;
  if ((not state) and (Self.m_rtState.trainSpeedUpdateIter > 0)) then
    Self.m_rtState.trainSpeedUpdateIter := 0;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkRT.ReleasedFromJC();
var railway: TBlkRailway;
begin
  Self.m_rtState.stopStopped := false;
  Self.m_rtState.stopPassed := false;

  if (Self.railway = nil) then
    Exit();
  railway := TBlkRailway(Self.railway);

  // zrusime potencialni poruchu blokove podminky a blokovou podminku
  Self.bpInBlk := false;
  Self.bpError := false;

  if (((railway.GetSettings().rType = TRailwayType.request)) and (not railway.zaver) and (not railway.occupied) and
    (not railway.RBPCan) and (railway.state.trains.Count = 0) and (not railway.emLock)) then
    railway.direction := TRailwayDirection.no;

  // pokud je trat uplne volna, zrusime blokovou podminku
  if (not railway.occupied) then
    railway.BP := false;

  railway.UpdateTrainPredict();
  railway.Change();
end;

/// /////////////////////////////////////////////////////////////////////////////
// komtrola volnosti sekce pro prijezd soupravy: musi byt splneno
// 1) zadny usek sekce neni obsazen
// 2) vsechny useky sekce jsou bez soupravy

function TBlkRT.GetSectReady(): Boolean;
var blk: TBlkRT;
  sectTracks: TList<TBlkRT>;
begin
  if ((Self.railway = nil) or (Self.occupied <= TTrackState.none)) then
    Exit(false);

  case (TBlkRailway(Self.railway).direction) of
    TRailwayDirection.AtoB:
      sectTracks := Self.lsectTracks;
    TRailwayDirection.BtoA:
      sectTracks := Self.ssectTracks;
    TRailwayDirection.no:
      begin
        if (Self.sRT = nil) then
          sectTracks := Self.ssectTracks
        else
        begin
          if (Self.lRT = nil) then
            sectTracks := Self.lsectTracks
          else
            Exit(false);
        end;
      end;
  else
    Exit(false);
  end;

  { zaver u prvniho bloku trati nekontrolujeme, protoze tuto metodu vyuziva JC
    pri staveni, ktera na prvni usek dava nouzovy zaver pri staveni }
  for blk in sectTracks do
    if ((blk.occupied <> TTrackState.Free) or (blk.IsTrain()) or (blk.bpError)) then
      Exit(false);
  Result := true;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkRT.GetReady(): Boolean;
begin
  Result := ((Self.occupied = TTrackState.Free) and (not Self.IsTrain()) and (not Self.bpError));
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkRT.SetBPError(state: Boolean);
begin
  if (Self.bpError <> state) then
  begin
    Self.m_rtState.bpError := state;
    Self.Change();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkRT.speed(train: TTrain): Cardinal;
begin
  if (Self.railway = nil) then
    Exit(0);

  var speed: Cardinal;
  var success: Boolean := false;

  if (TBlkRailway(Self.railway).direction = TRailwayDirection.AtoB) then
    success := TTrainSpeed.Pick(train, Self.m_rtSettings.speedsL, speed)
  else if (TBlkRailway(Self.railway).direction = TRailwayDirection.BtoA) then
  begin
    success := TTrainSpeed.Pick(train, Self.m_rtSettings.speedsS, speed);
    if (not success) then
      success := TTrainSpeed.Pick(train, Self.m_rtSettings.speedsL, speed);
  end;

  if (not success) then
    Exit(0);
  Result := speed;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkRT.mIsStop(): Boolean;
begin
  Result := (Self.m_rtSettings.stop <> nil);
end;

function TBlkRT.mIsStopL(): Boolean;
begin
  Result := Self.isStop and Assigned(Self.m_rtSettings.stop.evL);
end;

function TBlkRT.mIsStopS(): Boolean;
begin
  Result := Self.isStop and Assigned(Self.m_rtSettings.stop.evS);
end;

function TBlkRT.IsStopSlowedDown(): Boolean;
begin
  Result := (Self.isStop) and (Self.m_rtState.stopEnabled) and (not Self.m_rtState.stopPassed) and
    (not Self.m_rtState.stopSlowReady);
end;

/// /////////////////////////////////////////////////////////////////////////////
/// /////////////////////////////////////////////////////////////////////////////

constructor TBlkRTStop.Create();
begin
  inherited;

  Self.trainType := '';
  Self.evL := nil;
  Self.evS := nil;
end;

constructor TBlkRTStop.Create(ini_tech: TMemIniFile; const section: string);
begin
  Self.Create();
  Self.LoadFromFile(ini_tech, section);
end;

destructor TBlkRTStop.Destroy();
begin
  if (Assigned(Self.evL)) then
    Self.evL.Free();
  if (Assigned(Self.evS)) then
    Self.evS.Free();

  inherited;
end;

procedure TBlkRTStop.LoadFromFile(ini_tech: TMemIniFile; const section: string);
var str: string;
begin
  str := ini_tech.ReadString(section, 'zast_ev_lichy_zast', '');
  try
    if (str <> '') then
      Self.evL := TBlkRTStopEvents.Create(ini_tech, section, 'zast_ev_lichy')
    else if (Assigned(Self.evL)) then
      Self.evL.Free();
  except
    Self.evL := nil;
  end;

  try
    str := ini_tech.ReadString(section, 'zast_ev_sudy_zast', '');
    if (str <> '') then
      Self.evS := TBlkRTStopEvents.Create(ini_tech, section, 'zast_ev_sudy')
    else if (Assigned(Self.evS)) then
      Self.evS.Free();
  except
    Self.evS := nil;
  end;

  Self.maxLength := ini_tech.ReadInteger(section, 'zast_max_delka', 0);
  Self.delay := StrToTime(ini_tech.ReadString(section, 'zast_delay', '00:20'));
  Self.trainType := TBlkSignalTrainEvent.ParseTrainTypes(ini_tech.ReadString(section, 'zast_soupravy', ''));
end;

procedure TBlkRTStop.SaveToFile(ini_tech: TMemIniFile; const section: string);
begin
  ini_tech.WriteInteger(section, 'zast_max_delka', Self.maxLength);
  ini_tech.WriteString(section, 'zast_delay', TimeToStr(Self.delay));
  ini_tech.WriteString(section, 'zast_soupravy', 're:' + Self.trainType);
  if (Self.evL <> nil) then
    Self.evL.SaveToFile(ini_tech, section, 'zast_ev_lichy');
  if (Self.evS <> nil) then
    Self.evS.SaveToFile(ini_tech, section, 'zast_ev_sudy');
end;

/// /////////////////////////////////////////////////////////////////////////////

constructor TBlkRTStopEvents.Create();
begin
  inherited;

  Self.stop := nil;
  Self.slow.ev := nil;
end;

constructor TBlkRTStopEvents.Create(ini_tech: TMemIniFile; const section: string; const prefix: string);
begin
  Self.Create();
  Self.LoadFromFile(ini_tech, section, prefix);
end;

destructor TBlkRTStopEvents.Destroy();
begin
  if (Assigned(Self.stop)) then
    Self.stop.Free();
  if (Assigned(Self.slow.ev)) then
    Self.slow.ev.Free();

  inherited;
end;

procedure TBlkRTStopEvents.LoadFromFile(ini_tech: TMemIniFile; const section: string; const prefix: string);
var str: string;
begin
  Self.stop := TRREv.Create(false, ini_tech.ReadString(section, prefix + '_zast', ''));

  str := ini_tech.ReadString(section, prefix + '_zpom_ev', '');
  Self.slow.enabled := (str <> '');
  if (Self.slow.enabled) then
  begin
    Self.slow.ev := TRREv.Create(true, str);
    Self.slow.speed := ini_tech.ReadInteger(section, prefix + '_zpom_sp', 40);
  end;
end;

procedure TBlkRTStopEvents.SaveToFile(ini_tech: TMemIniFile; const section: string; const prefix: string);
begin
  ini_tech.WriteString(section, prefix + '_zast', Self.stop.GetDefStr());
  if (Self.slow.enabled) then
  begin
    ini_tech.WriteString(section, prefix + '_zpom_ev', Self.slow.ev.GetDefStr());
    ini_tech.WriteInteger(section, prefix + '_zpom_sp', Self.slow.speed);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

end.// unit
