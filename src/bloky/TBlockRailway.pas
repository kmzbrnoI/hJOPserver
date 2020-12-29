unit TBlockRailway;

{
 RAILWAY technological block definition (trať).

 Tento blok se na panelu nevyskytuje - slouzi pouze jako rodic dvou uvazek.
 U bloku trati je zajisteno, ze existuji a jsou typu TBlkLinker.
 Bloky, ktere tomuto nevyhovuji, jsou po startu odstraneny.
}

interface

uses IniFiles, TBlock, Menus, TOblsRizeni, SysUtils, Classes, JsonDataObjects,
     Generics.Collections, Train;

type
 TRailwayType = (permanent = 0, request = 2);
 TRailwayDirection = (disabled = -1, no = 0, AtoB = 1, BtoA = 2);
 TRailwaySignals = (autoblok = 0, hradlo = 1);

 TBlkRailwaySettings = record
  linkerA, linkerB: Integer;
  rType: TRailwayType;
  signals: TRailwaySignals;
  trackIds: TList<integer>;
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

     constructor Create(train: Integer); overload;
     constructor Create(train: Integer; time: TTime; predict: Boolean = false); overload;
     function IsTimeDefined(): Boolean;
     procedure UndefTime();
     property time: TTime read GetTime write SetTime;
     property train: TTrain read GeTTrain;

     function SerializeForPanel(trat: TBlk; trainPredict: Boolean = false): string;
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
   _def_railway_state: TBlkRailwayState = (
     zaver: false;
     direction: disabled;
     request: false;
     trainPredict: nil;
     BP: false;
   );

  private
   m_settings: TBlkRailwaySettings;
   m_state: TBlkRailwayState;
   file_direction: TRailwayDirection;
   m_linkerA, m_linkerB: TBlk;                                                  // tady si ukladame reference na skutecne bloky, ktere si vytvarime az pri prvnim pristupu k uvazce pres \uvazkaA a \uvazkaB
   m_signalA, m_signalB: TBlk;                                                  // analogicky funguji krajni navestidla trati, viz \navLichy a \navSudy
                                                                                // fNavLichy je navestidlo u stanice blize pocatku trati, fNavSudy navestidlo u stanice blize konce trati

    function GetLinkerA(): TBlk;
    function GetLinkerB(): TBlk;

    function GetOccupied(): Boolean;
    function GetDepartureForbidden(): Boolean;
    function GetRBP(): Boolean;
    function GetEmergencyLock(): Boolean;

    procedure SetDirection(smer: TRailwayDirection);
    procedure SetZaver(zaver: Boolean);
    procedure SetRequest(zadost: Boolean);
    procedure SetTrainPredict(train: TBlkRailwayTrain);

    procedure SetBP(state: Boolean);

    function GetSignalA(): TBlk;
    function GetSignalB(): TBlk;

    procedure CheckTUExist();                                                   // zkontroluje existenci vsech bloku, ktere maji v trati byt; nevalidni bloky z trati smaze a provede o tom zapis do LOGu
    procedure InitTUs();                                                        // inicializuje tratove useky - oznami jim, ze jsou v trati a provede mnoho dalsich veci, viz telo metody
    procedure ResetTUs();                                                       // resetuje stav tratoveho useku; tratovy usek zapomene, ze je v nejake trati a stane se neutralnim tratovym usekem, ktery nic nevi

    function GetReady(): Boolean;                                                // vrati, jestli jsou vsechny tratove useky pripraveny pro vjezd soupravy, pouziva se pri zjistovani toho, jestli je mozne obratit smer trati
    function GetTrainIndex(train: TTrain): Integer;
    function TrainTUsCount(train: TTrain): Integer;

    function GetLastTrack(): TBlk; overload;
    function GetLockout(): Boolean;

  public
    constructor Create(index: Integer);
    destructor Destroy(); override;

    procedure LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile); override;
    procedure SaveData(ini_tech: TMemIniFile; const section: string); override;
    procedure SaveStatus(ini_stat: TMemIniFile; const section: string); override;

    procedure Enable(); override;
    procedure Disable(); override;
    procedure Reset(); override;
    procedure AfterLoad(); override;

    procedure Change(now: Boolean = false); override;
    procedure ChangeFromLinker(Sender: TBlk);
    procedure ChangeTracks();

    //----- Railway specific functions -----

    function GetSettings(): TBlkRailwaySettings;
    procedure SetSettings(data: TBlkRailwaySettings);

    function IsFirstLinker(uv: TBlk): Boolean;
    procedure TrainChangeOR(train: TTrain); overload;
    procedure TrainChangeOR(train: TTrain; smer: TRailwayDirection); overload;

    procedure AddTrain(train: TTrain); overload;
    procedure AddTrain(train: TBlkRailwayTrain); overload;
    function GetTrainsList(separator: Char): string;
    procedure RemoveTrain(train: TTrain);

    function IsTrain(train: TTrain; predict: Boolean = true): Boolean;
    function IsTrainInAnyTU(train: TTrain): Boolean;
    function IsTrainInMoreTUs(train: TTrain): Boolean;

    procedure CallChangeToTU();
    procedure UpdateTrainPredict(call_prediction: Boolean = true);
    function SignalCounterDirection(): Integer;

    function SameUserBothLinkers(): Boolean;                                    // vraci true prave tehdy, kdyz obe uvazky kontrlu stejny uzivatel
                                                                                // kdyz je true, do trati neni potreba zadat

    function ChangesTrainDir(): Boolean;                                         // vraci true prave tehdy, kdyz se v trati meni smer soupravy
    function GetTrainTrack(train: TTrain): TBlk;
    function GetLastTrack(smer: TRailwayDirection): TBlk; overload;
    function HasAutoblokSignal(blk: TBlk): Boolean;

    procedure GetPtData(json: TJsonObject; includeState: Boolean); override;
    procedure GetPtState(json: TJsonObject); override;

    property linkerA: TBlk read GetLinkerA;                                      // blok uvazky blize zacatku trati
    property linkerB: TBlk read GetLinkerB;                                      // blok uvazky blize konci trati
    property RBPCan: Boolean read GetRBP;                                        // vraci, jestli v trati doslo k poruse uplne blokove podminky, resp. jesli je mozno ji zrusit

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

    // vrati hranicni navestidla
    property signalA: TBlk read GetSignalA;                                    // hranicni navestidlo trati blize zacatku trati
    property signalB: TBlk read GetSignalB;                                     // hranicni navestidlo trati blize konci trati

    property ready: Boolean read GetReady;                                       // jsou vsechny tratove useky "ready"? typicky se pouziva jako flag moznosti zmeny smeru trati
    property rType: TRailwayType read m_settings.rType;
    property signals: TRailwaySignals read m_settings.signals;

 end;

////////////////////////////////////////////////////////////////////////////////

implementation

uses GetSystems, TechnologieRCS, TBloky, TOblRizeni, TBlockSignal, Logging,
    TJCDatabase, fMain, TCPServerOR, TBlockTrack, TBlockLinker, TrainDb, THVDatabase,
    TBlockRailwayTrack, appEv, timeHelper, ownConvert, Graphics;

constructor TBlkRailway.Create(index: Integer);
begin
 inherited;

 Self.m_globSettings.typ := btRailway;
 Self.m_state := _def_railway_state;

 Self.m_linkerA := nil;
 Self.m_linkerB := nil;

 Self.m_signalA := nil;
 Self.m_signalB  := nil;

 Self.m_settings.trackIds := TList<Integer>.Create();
 Self.m_state.trains := TObjectList<TBlkRailwayTrain>.Create();
end;//ctor

destructor TBlkRailway.Destroy();
begin
 Self.m_state.trains.Free();
 Self.m_settings.trackIds.Free();
 inherited;
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure TBlkRailway.LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile);
var str: TStrings;
    i: Integer;
    data: TStrings;
    index: Integer;
begin
 inherited LoadData(ini_tech, section, ini_rel, ini_stat);

 Self.m_settings.linkerA  := ini_tech.ReadInteger(section, 'uvazkaA', -1);
 Self.m_settings.linkerB  := ini_tech.ReadInteger(section, 'uvazkaB', -1);
 i := ini_tech.ReadInteger(section, 'zabzar', 0);
 if (i = 1) then i := 2;
 Self.m_settings.rType := TRailwayType(i);
 Self.m_settings.signals := TRailwaySignals(ini_tech.ReadInteger(section, 'navestidla', 0));

 Self.file_direction := TRailwayDirection(ini_stat.ReadInteger(section, 'smer', 1));

 Self.m_state.BP := ini_stat.ReadBool(section, 'BP', false);

 data := TStringList.Create();
 ExtractStrings([',', ';'], [], PChar(ini_stat.ReadString(section, 'spr', '')), data);
 Self.m_state.trains.Clear();
 for i := 0 to data.Count-1 do
  begin
   index := Trains.GetTrainIndexByName(data[i]);
   if (index > -1) then Self.m_state.trains.Add(TBlkRailwayTrain.Create(index));
  end;
 data.Free();

 str := TStringList.Create();
 ExtractStrings([';', ','], [], PChar(ini_tech.ReadString(section, 'useky', '')), str);
 Self.m_settings.trackIds.Clear();
 for i := 0 to str.Count-1 do
  begin
   try
    Self.m_settings.trackIds.Add(StrToInt(str[i]));
   except

   end;
  end;//for i
 str.Free();
end;

procedure TBlkRailway.SaveData(ini_tech: TMemIniFile; const section: string);
var str: string;
    i: Integer;
begin
 inherited SaveData(ini_tech, section);

 ini_tech.WriteInteger(section, 'uvazkaA', Self.m_settings.linkerA);
 ini_tech.WriteInteger(section, 'uvazkaB', Self.m_settings.linkerB);
 ini_tech.WriteInteger(section, 'zabzar', Integer(Self.m_settings.rType));
 ini_tech.WriteInteger(section, 'navestidla', Integer(Self.m_settings.signals));

 str := '';
 for i := 0 to Self.m_settings.trackIds.Count-1 do
  str := str + IntToStr(Self.m_settings.trackIds[i]) + ',';
 ini_tech.WriteString(section, 'useky', str)
end;

procedure TBlkRailway.SaveStatus(ini_stat: TMemIniFile; const section: string);
var i: Integer;
    str: string;
begin
 ini_stat.WriteInteger(section, 'smer', Integer(Self.file_direction));

 if (Self.m_state.BP) then
   ini_stat.WriteBool(section, 'BP', Self.m_state.BP);

 str := '';
 for i := 0 to Self.m_state.trains.Count-1 do
   str := str + Self.m_state.trains[i].train.name + ';';

 if (str <> '') then
   ini_stat.WriteString(section, 'spr', str);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkRailway.Enable();
begin
 Self.m_state.direction := Self.file_direction;
 Self.Change();
end;

procedure TBlkRailway.Disable();
begin
 if (Self.direction <> TRailwayDirection.disabled) then
   Self.file_direction := Self.direction;
 Self.TrainPredict := nil;
 Self.m_state.direction := TRailwayDirection.disabled;
 Self.Change(true);
end;

procedure TBlkRailway.Reset();
begin
 Self.Zaver  := false;
 Self.request := false;
 Self.TrainPredict := nil;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkRailway.AfterLoad();
begin
 Self.CheckTUExist();
 Self.InitTUs();
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

////////////////////////////////////////////////////////////////////////////////

procedure TBlkRailway.ChangeTracks();
var i: Integer;
    Blk: TBlk;
begin
 for i := 0 to Self.m_settings.trackIds.Count-1 do
  begin
   Blky.GetBlkByID(Self.m_settings.trackIds[i], Blk);
   if ((Blk <> nil) and (Blk.typ = btRT)) then
    Blk.Change();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkRailway.GetOccupied(): Boolean;
var i: Integer;
    Blk: TBlk;
begin
 for i := 0 to Self.m_settings.trackIds.Count-1 do
  begin
   Blky.GetBlkByID(Self.m_settings.trackIds[i], Blk);
   if ((Blk = nil) or (Blk.typ <> btRT)) then continue;
   if ((Blk as TBlkRT).occupied = TTrackState.occupied) then
    Exit(true);
  end;

 Result := false;
end;

function TBlkRailway.GetDepartureForbidden(): Boolean;
begin
 if ((Self.linkerA = nil) or (Self.linkerB = nil)) then Exit(false);
 if (((Self.linkerA as TBlkLinker).departureForbidden) or ((Self.linkerB as TBlkLinker).departureForbidden)) then
  Result := true
 else
  Result := false;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkRailway.SetDirection(smer: TRailwayDirection);
begin
 Self.m_state.direction := smer;

 // zrusime blokovou podminku
 Self.BP := false;

 Self.Change();
 Self.CallChangeToTU();
end;

procedure TBlkRailway.SetZaver(Zaver: Boolean);
begin
 if (Self.m_state.zaver <> Zaver) then
  begin
   Self.m_state.zaver := zaver;
   Self.TrainPredict := nil;
   Self.Change();
  end else begin
   Self.TrainPredict := nil;
  end;
end;

procedure TBlkRailway.SetRequest(Zadost: Boolean);
var uvazka: TBlkLinker;
    oblr: TOR;
begin
 if (Self.request = Zadost) then Exit();

 // tady se resi prehravani zvuku
 try
   uvazka := nil;
   if ((Self.m_linkerA as TBlkLinker).request) then uvazka := (Self.m_linkerB as TBlkLinker)
   else if ((Self.m_linkerB as TBlkLinker).request) then uvazka := (Self.m_linkerA as TBlkLinker);

   if ((uvazka <> nil) and (Zadost <> Self.m_state.request)) then
    begin
     if (Zadost) then
      begin
       for oblr in uvazka.stations do
         oblr.ZadostBlkCnt := oblr.ZadostBlkCnt + 1;
      end else begin
       for oblr in uvazka.stations do
         oblr.ZadostBlkCnt := oblr.ZadostBlkCnt - 1;
      end;
    end;
 except
   on E: Exception do
     AppEvents.LogException(E, 'SetTratZadost');
 end;

 Self.m_state.request := zadost;
 Self.Change();
end;

procedure TBlkRailway.SetTrainPredict(train: TBlkRailwayTrain);
begin
 if (Self.m_state.TrainPredict = train) then Exit();

 if (Self.m_state.TrainPredict <> nil) then
   FreeAndNil(Self.m_state.TrainPredict);

 if (train <> nil) then
   Self.m_state.trainPredict := train;

 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

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

 if (not Assigned(data.trackIds)) then data.trackIds := TList<Integer>.Create();
 Self.CheckTUExist();
 Self.InitTUs();

 // zrusim uvazku, aby se prepocitala
 Self.m_linkerA := nil;
 Self.m_linkerB := nil;

 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkRailway.IsFirstLinker(uv: TBlk): Boolean;
begin
 if (uv = Self.linkerA) then
  Result := true
 else
  Result := false;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkRailway.GetLinkerA(): TBlk;
begin
 if (Self.m_linkerA = nil) then
  Blky.GetBlkByID(Self.m_settings.linkerA, Self.m_linkerA);
 Result := Self.m_linkerA;
end;

function TBlkRailway.GetLinkerB(): TBlk;
begin
 if (Self.m_linkerB = nil) then
  Blky.GetBlkByID(Self.m_settings.linkerB, Self.m_linkerB);
 Result := Self.m_linkerB;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkRailway.GetRBP(): Boolean;
var i: Integer;
    Blk: TBlk;
begin
 for i := 0 to Self.m_settings.trackIds.Count-1 do
  begin
   Blky.GetBlkByID(Self.m_settings.trackIds[i], Blk);
   if (TBlkRT(Blk).bpError) then Exit(true);
  end;//for i
 Exit(false);
end;

////////////////////////////////////////////////////////////////////////////////
// zavedeni / zruseni blokove podminky
// zavest blokovou podminky lze vzdy, zrusit ji lze jen tehdy, kdyz
//  na zadnem tratovem useku neni blokova podminka

procedure TBlkRailway.SetBP(state: Boolean);
var i: Integer;
    Blk: TBlk;
begin
 if (Self.BP = state) then Exit();

 if (state) then
  begin
   Self.m_state.BP := true;
  end else begin
   for i := 0 to Self.m_settings.trackIds.Count-1 do
    begin
     Blky.GetBlkByID(Self.m_settings.trackIds[i], Blk);
     if (TBlkRT(Blk).bpInBlk) then Exit();
    end;
   Self.m_state.BP := false;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkRailway.AddTrain(train: TTrain);
begin
 Self.AddTrain(TBlkRailwayTrain.Create(train.index, timeHelper.hJOPnow()));
end;

procedure TBlkRailway.AddTrain(train: TBlkRailwayTrain);
begin
 Self.m_state.trains.Add(train);
 if (train <> Self.trainPredict) then
   Self.trainPredict := nil // will also Free
 else
   Self.m_state.trainPredict := nil; // will not Free

 if (not train.IsTimeDefined()) then
   train.time := timeHelper.hJOPnow();

 writelog('Trať '+Self.m_globSettings.name+ ' : přidána souprava '+train.train.name, WR_SPRPREDAT);

 Self.Change();
end;

procedure TBlkRailway.RemoveTrain(train: TTrain);
var toChange: Boolean;
begin
 toChange := false;

 if ((Self.trainPredict <> nil) and (Self.trainPredict.train = train)) then
  begin
   Self.trainPredict := nil;
   toChange := true;
  end;

 if (Self.IsTrain(train)) then
  begin
   Self.m_state.trains.Delete(Self.GetTrainIndex(train));
   writelog('Trať '+Self.m_globSettings.name+ ' : smazána souprava '+train.name, WR_SPRPREDAT);
   toChange := true;
  end;

 if (toChange) then
   Self.Change(); 
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkRailway.GetTrainsList(separator: Char): string;
var train: TBlkRailwayTrain;
begin
 Result := '';

 for train in Self.m_state.trains do
   Result := Result + train.SerializeForPanel(Self) + separator;

 if (Self.trainPredict <> nil) then
   Result := Result + Self.trainPredict.SerializeForPanel(Self, true) + separator;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkRailway.TrainChangeOR(train: TTrain);
begin
 Self.TrainChangeOR(train, Self.direction);
end;

procedure TBlkRailway.TrainChangeOR(train: TTrain; smer: TRailwayDirection);
begin
 case (smer) of
   TRailwayDirection.AtoB: begin
      if ((Self.linkerB as TBlkLinker).stations.Count > 0) then
        train.station := (Self.linkerB as TBlkLinker).stations[0]
      else
        train.station := nil;
   end;//AtoB
   TRailwayDirection.BtoA: begin
      if ((Self.linkerA as TBlkLinker).stations.Count > 0) then
        train.station := (Self.linkerA as TBlkLinker).stations[0]
      else
        train.station := nil;
   end;//BtoA
 end;//case

 writelog('Trať '+Self.m_globSettings.name+ ' : souprava '+train.name+
          ' : stanice změněna na '+(train.station as TOR).Name, WR_SPRPREDAT);
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkRailway.GetEmergencyLock(): Boolean;
begin
 Result := (Self.linkerA as TBlkLinker).emLock or (Self.linkerB as TBlkLinker).emLock;
end;

////////////////////////////////////////////////////////////////////////////////

// vrati hranicni navestidlo trati na jejim zacatku
// hranicni navestidlo musi
//  - byt ve stejne OR, jako uvazka
//  - mit blok_pred_id hranicni blok trati
//  - nebyl navestidlo autobloku druheho useku trati
function TBlkRailway.GetSignalA(): TBlk;
var Blk: TBlk;
    BlkTU: TBlkRT;
begin
 if (Self.m_settings.trackIds.Count = 0) then Exit(nil);

 if ((Self.m_signalA = nil) or ((Self.m_signalA as TBlkSignal).trackId <> Self.m_settings.trackIds[0])) then
  begin
   if (Self.m_settings.trackIds.Count > 1) then
     Blky.GetBlkByID(Self.m_settings.trackIds[1], TBlk(BlkTU))
   else
     BlkTU := nil;

   for blk in Blky do
    begin
     if (Blk.typ <> btSignal) then continue;
     if ((TBlkSignal(Blk).trackId = Self.m_settings.trackIds[0]) and
         (Blk.stations[0] = Self.linkerA.stations[0]) and
         ((BlkTU = nil) or (Blk.id <> BlkTU.GetSettings.signalLid))) then
      begin
       Self.m_signalA := Blk;
       break;
      end;
    end;
  end;

 Result := Self.m_signalA;
end;

// vrati hranicni navestidlo trati na jejim konci
function TBlkRailway.GetSignalB(): TBlk;
var Blk: TBlk;
    BlkTU: TBlkRT;
begin
 if (Self.m_settings.trackIds.Count = 0) then Exit(nil);

 if ((Self.m_signalB = nil) or ((Self.m_signalB as TBlkSignal).trackId <> Self.m_settings.trackIds[Self.m_settings.trackIds.Count-1])) then
  begin
   if (Self.m_settings.trackIds.Count > 1) then
     Blky.GetBlkByID(Self.m_settings.trackIds[Self.m_settings.trackIds.Count-2], TBlk(BlkTU))
   else
     BlkTU := nil;

   for blk in Blky do
    begin
     if (Blk.typ <> btSignal) then continue;
     if ((TBlkSignal(Blk).trackId = Self.m_settings.trackIds[Self.m_settings.trackIds.Count-1]) and
         (Blk.stations[0] = Self.linkerB.stations[0]) and
         ((BlkTU = nil) or (Blk.id <> BlkTU.GetSettings.signalSid))) then
      begin
       Self.m_signalB := Blk;
       break;
      end;
    end;
  end;

 Result := Self.m_signalB;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkRailway.IsTrain(train: TTrain; predict: Boolean = true): Boolean;
begin
 Result := ((Self.GetTrainIndex(train) > -1) or
            ((predict) and (Self.trainPredict <> nil) and (Self.trainPredict.train = train)));
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkRailway.GetTrainIndex(train: TTrain): Integer;
var i: Integer;
begin
 for i := 0 to Self.m_state.trains.Count-1 do
   if (Self.m_state.trains[i].train = train) then
     Exit(i);
 Exit(-1);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkRailway.CheckTUExist();
var i: Integer;
    Blk: TBlk;
begin
 for i := Self.m_settings.trackIds.Count-1 downto 0 do
  begin
   Blky.GetBlkByID(Self.m_settings.trackIds[i], Blk);
   if ((Blk = nil) or (Blk.typ <> btRT)) then
    begin
     writelog('Trat '+Self.name+' obsahuje referenci na TU ID '+IntToStr(Self.m_settings.trackIds[i])+', tento blok ale bud neexistuje, nebo neni typu TU, odstranuji referenci', WR_ERROR);
     Self.m_settings.trackIds.Delete(i);
     continue;
    end;
   if (((Blk as TBlkRT).inRailway <> -1) and ((Blk as TBlkRT).inRailway <> Self.id)) then
    begin
     writelog('Trat '+Self.name+': TU ID '+IntToStr(Self.m_settings.trackIds[i])+' jiz referuje na trat ID '+IntToStr((Blk as TBlkRT).inRailway)+', odstranuji referenci', WR_ERROR);
     Self.m_settings.trackIds.Delete(i);
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkRailway.TrainTUsCount(train: TTrain): Integer;
var usek: Integer;
    Blk: TBlk;
begin
 Result := 0;
 for usek in Self.m_settings.trackIds do
  begin
   Blky.GetBlkByID(usek, Blk);
   if (TBlkRT(Blk).IsTrain(train)) then
     Inc(Result);
  end;
end;

function TBlkRailway.IsTrainInAnyTU(train: TTrain): Boolean;
begin
 Result := (Self.TrainTUsCount(train) > 0);
end;

function TBlkRailway.IsTrainInMoreTUs(train: TTrain): Boolean;
begin
 Result := (Self.TrainTUsCount(train) > 1);
end;

////////////////////////////////////////////////////////////////////////////////
// vytvoreni navaznosti mezi tratovymi useky, sekcemi tratovych useku a
// navestidly autobloku

procedure TBlkRailway.InitTUs();
var i: Integer;
    lTU, sTU: TBlkRT;
    tracks: TList<TBlkRT>;
    blk, sMaster: TBlkRT;
begin
 // 1) nejprve vytvorime navaznosti mezi tratovymi useky:
 //    Kazdemu TU rekneme, jaky TU je vedle neho v lichem smeru (bliz zacatku trati)
 //    a jaky TU je vedle enho v sudem smeru (bliz konci trati).
 //    Krajni TU maji referenci na dalsi TU "nil".

 if (Self.m_settings.trackIds.Count = 0) then Exit();

 lTU := nil;
 Blky.GetBlkByID(Self.m_settings.trackIds[0], TBlk(blk));
 if (Self.m_settings.trackIds.Count > 1) then
   Blky.GetBlkByID(Self.m_settings.trackIds[1], TBlk(sTU))
 else
   sTU := nil;

 for i := 0 to Self.m_settings.trackIds.Count-2 do
  begin
   Blk.lTU := lTU;
   Blk.sTU := sTU;
   lTU := Blk;
   Blk := sTU;
   if (i < Self.m_settings.trackIds.Count-2) then
     Blky.GetBlkByID(Self.m_settings.trackIds[i+2], TBlk(sTU));
  end;

 // posledni TU:
 Blk.lTU := lTU;
 Blk.sTU := nil;

 /////////////////////////////////////////////////////////////////////////////
 // 2) Kazdemu TU priradime jeho Section Master a Section Masteru priradime
 //    jeho useky.

 //  a) v lichem smeru: jdeme od zacatku trati ke konci
 Blky.GetBlkByID(Self.m_settings.trackIds[0], TBlk(sMaster));
 tracks := sMaster.lsectTracks;
 for i := 0 to Self.m_settings.trackIds.Count-1 do
  begin
   Blky.GetBlkByID(Self.m_settings.trackIds[i], TBlk(blk));

   // useku take priradime, ze je v nasi trati
   (Blk as TBlkRT).inRailway := Self.id;

   if (blk.GetSettings().signalLid <> -1) then
    begin
     sMaster := blk;
     tracks := sMaster.lsectTracks;
    end;
   blk.lsectMaster := sMaster;
   tracks.Add(blk);
  end;

 //  b) v sudem smeru: jdeme od konce trati k zacatku
 Blky.GetBlkByID(Self.m_settings.trackIds[Self.m_settings.trackIds.Count-1], TBlk(sMaster));
 tracks := sMaster.ssectTracks;
 for i := Self.m_settings.trackIds.Count-1 downto 0 do
  begin
   Blky.GetBlkByID(Self.m_settings.trackIds[i], TBlk(blk));
   if (blk.GetSettings().signalSid <> -1) then
    begin
     sMaster := blk;
     tracks := sMaster.ssectTracks;
    end;
   blk.ssectMaster := sMaster;
   tracks.Add(blk);
  end;

 /////////////////////////////////////////////////////////////////////////////
 // 3) inicializujeme navaznosti navestidel

 for i := 0 to Self.m_settings.trackIds.Count-1 do
  begin
   Blky.GetBlkByID(Self.m_settings.trackIds[i], TBlk(blk));
   blk.CreateNavRefs();
  end;

end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkRailway.ResetTUs();
var i: Integer;
    blk: TBlkRT;
begin
 for i := 0 to Self.m_settings.trackIds.Count-1 do
  begin
   Blky.GetBlkByID(Self.m_settings.trackIds[i], TBlk(blk));
   blk.RemoveTURefs();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkRailway.CallChangeToTU();
var i: Integer;
    blk: TBlkRT;
begin
 for i := 0 to Self.m_settings.trackIds.Count-1 do
  begin
   Blky.GetBlkByID(Self.m_settings.trackIds[i], TBlk(blk));
   blk.ChangeFromTrat();
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// aktualizace predpovidane soupravy na posledni usek trati
// volano pri uvolneni posledniho useku trati, nebo RBP

procedure TBlkRailway.UpdateTrainPredict(call_prediction: Boolean = true);
var Blk, last: TBlkRT;
    i: Integer;
begin
 if ((Self.direction <> TRailwayDirection.AtoB) and (Self.direction <> TRailwayDirection.BtoA)) then Exit();
 if (Self.m_settings.trackIds.Count = 0) then Exit();

 case (Self.direction) of
  TRailwayDirection.AtoB: begin
       Blky.GetBlkByID(Self.m_settings.trackIds[Self.m_settings.trackIds.Count-1], TBlk(last));
       last.trainPredict := nil;
       if (last.IsTrain()) then Exit();
       for i := Self.m_settings.trackIds.Count-2 downto 0 do
        begin
         Blky.GetBlkByID(Self.m_settings.trackIds[i], TBlk(Blk));
         if (Blk.IsTrain()) then
          begin
           last.trainPredict := Blk.train;
           break;
          end;
         if (Blk.trainPredict <> nil) then
          begin
           last.trainPredict := Blk.trainPredict;
           break;
          end;
         if ((Blk.signalCover <> nil) and (TBlkSignal(Blk.signalCover).signal = ncStuj)) then
          begin
           Blky.TrainPrediction(Self.signalB);
           Exit();
          end;
        end;

       if ((last.trainPredict = nil) and (Self.trainPredict <> nil)) then
         last.trainPredict := Self.trainPredict.train;
       if ((call_prediction) and (Self.signalB <> nil)) then
         Blky.trainPrediction(Self.signalB);
  end;

  TRailwayDirection.BtoA: begin
       Blky.GetBlkByID(Self.m_settings.trackIds[0], TBlk(last));
       last.trainPredict := nil;
       if (last.IsTrain()) then Exit();
       for i := 1 to Self.m_settings.trackIds.Count-1 do
        begin
         Blky.GetBlkByID(Self.m_settings.trackIds[i], TBlk(Blk));
         if (Blk.IsTrain()) then
          begin
           last.trainPredict := Blk.train;
           break;
          end;
         if (Blk.trainPredict <> nil) then
          begin
           last.trainPredict := Blk.trainPredict;
           break;
          end;
         if ((Blk.signalCover <> nil) and (TBlkSignal(Blk.signalCover).signal = ncStuj)) then
          begin
           Blky.TrainPrediction(Self.signalA);
           Exit();
          end;
        end;

       if ((last.trainPredict = nil) and (Self.trainPredict <> nil)) then
         last.trainPredict := Self.trainPredict.train;
       if ((call_prediction) and (Self.signalA <> nil)) then
         Blky.TrainPrediction(Self.signalA);
  end;
 end;//case
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkRailway.GetReady(): Boolean;
var i: Integer;
    Blk: TBlk;
begin
 for i := 0 to Self.m_settings.trackIds.Count-1 do
  begin
   Blky.GetBlkByID(Self.m_settings.trackIds[i], Blk);
   if ((Blk = nil) or (Blk.typ <> btRT)) then Exit(false);
   if (not TBlkRT(Blk).ready) then Exit(false);
  end;
 Result := true;
end;

////////////////////////////////////////////////////////////////////////////////
// Vraci true prave tehdy, pokud je trat na obou koncich rizena stejnym uzivatelem.
// Mazerne nenavazujeme rizeni obou koncu na konkretniho uzivatele, napriklad
// kvuli staveni ze zasobniku.

function TBlkRailway.SameUserBothLinkers(): Boolean;
var first, second: TORPanel;
begin
 if ((not Assigned(Self.linkerA)) or (not Assigned(Self.linkerB))) then Exit(false);
 if ((TBlkLinker(Self.linkerA).stations.Count <> 1) or (TBlkLinker(Self.linkerB).stations.Count <> 1)) then Exit(false);

 for first in TBlkLinker(Self.linkerA).stations[0].Connected do
   if (first.Rights >= TORControlRights.write) then
     for second in TBlkLinker(Self.linkerB).stations[0].Connected do
       if ((first.user = second.user) and (second.Rights >= TORControlRights.write)) then
         Exit(true);

 Result := false;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkRailway.ChangesTrainDir(): Boolean;
begin
 Result := (Assigned(Self.signalA)) and (Assigned(Self.signalB)) and
    (TBlkSignal(Self.signalA).direction = TBlkSignal(Self.signalB).direction);
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkRailway.GetTrainTrack(train: TTrain): TBlk;
var usekid: Integer;
    blk: TBlk;
begin
 for usekid in Self.m_settings.trackIds do
  begin
   Blky.GetBlkByID(usekid, blk);
   if ((blk <> nil) and (blk.typ = btRT) and (TBlkTrack(blk).train = train)) then
     Exit(blk);
  end;

 Result := nil;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkRailway.GetLastTrack(): TBlk;
begin
 Result := Self.GetLastTrack(Self.direction);
end;

function TBlkRailway.GetLastTrack(smer: TRailwayDirection): TBlk;
begin
 if (Self.m_settings.trackIds.Count < 1) then
   raise Exception.Create('Trať nemá žádný úsek!');

 if (smer = TRailwayDirection.AtoB) then
   Blky.GetBlkByID(Self.m_settings.trackIds[Self.m_settings.trackIds.Count-1], Result)
 else if (smer = TRailwayDirection.BtoA) then
   Blky.GetBlkByID(Self.m_settings.trackIds[0], Result)
 else
   raise Exception.Create('Trať nemá žádný směr!');
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkRailway.GetLockout(): Boolean;
var blkUsek: TBlkTrack;
    usek: Integer;
begin
 for usek in Self.m_settings.trackIds do
  begin
   Blky.GetBlkByID(usek, TBlk(blkUsek));
   if (blkUsek <> nil) then
     if (blkUsek.lockout <> '') then
       Exit(true);
  end;
 Result := false;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkRailway.SignalCounterDirection(): Integer;
begin
 case (Self.signals) of
  TRailwaySignals.autoblok: Result := Integer(ncZhasnuto);
  TRailwaySignals.hradlo: Result := Integer(ncStuj);
 else
  Result := Integer(ncZhasnuto);
 end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkRailway.HasAutoblokSignal(blk: TBlk): Boolean;
var usekid: Integer;
    track: TBlkRT;
begin
 for usekid in Self.m_settings.trackIds do
  begin
   Blky.GetBlkByID(usekid, TBlk(track));
   if (track.typ <> btRT) then continue;
   if ((blk = track.signalCoverL) or (blk = track.signalCoverS)) then
     Exit(true);
  end;
 Result := false;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkRailway.GetPtData(json: TJsonObject; includeState: Boolean);
var trackId: Integer;
begin
 inherited;

 json['linkerA'] := Self.m_settings.linkerA;
 json['linkerB'] := Self.m_settings.linkerB;
 json['zabzar'] := Integer(Self.m_settings.rType);
 json['signals'] := Integer(Self.m_settings.signals);
 for trackId in Self.m_settings.trackIds do
   json.A['tracks'].Add(trackId);

 if (includeState) then
   Self.GetPtState(json['blockState']);
end;

procedure TBlkRailway.GetPtState(json: TJsonObject);
var train: TBlkRailwayTrain;
begin
 json['lock'] := Self.m_state.zaver;
 json['direction'] := Integer(Self.m_state.direction);
 json['request'] := Self.m_state.request;

 for train in Self.m_state.trains do
   if (not train.predict) then
     json.A['trains'].Add(train.train.name);
 if (Self.m_state.trainPredict <> nil) then
   json['trainPredict'] := Self.m_state.trainPredict.train;
 json['BP'] := Self.m_state.BP;
end;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// TBlkTraTTrain

constructor TBlkRailwayTrain.Create(train: Integer);
begin
 inherited Create();
 Self.traini := train;
 Self.mTimeDefined := false;
 Self.predict := false;
end;

constructor TBlkRailwayTrain.Create(train: Integer; time: TTime; predict: Boolean = false);
begin
 inherited Create();
 Self.traini := train;
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

function TBlkRailwayTrain.SerializeForPanel(trat: TBlk; trainPredict: Boolean = false): string;
var addr, usek: Integer;
    porucha_bp: Boolean;
    blk: TBlk;
    stopsInHalt: Boolean;
begin
 // Pozor, souprava muze byt ve vice usecich a mit poruchu BP jen v jednom z nich
 porucha_bp := false;
 for usek in TBlkRailway(trat).GetSettings().trackIds do
  begin
   Blky.GetBlkByID(usek, blk);
   if ((blk <> nil) and (blk.typ = btRT)) then
     if (TBlkTrack(blk).train = Self.train) and (TBlkRT(blk).bpError) then
       porucha_bp := true;
  end;

 blk := Self.train.front as TBlk;
 stopsInHalt := ((blk <> nil) and (blk.typ = btRT) and (TBlkRT(blk).tuState.stopStopped));

 Result := Self.train.name + '|';
 if (trainPredict) then
   Result := Result + ownConvert.ColorToStr(clYellow) + '|'
 else if (porucha_bp) then
   Result := Result + ownConvert.ColorToStr(clAqua) + '|'
 else if ((Self.train.speed = 0) and (not stopsInHalt)) then
   Result := Result + ownConvert.ColorToStr(clRed) + '|'
 else
   Result := Result + ownConvert.ColorToStr(clWhite) + '|';

 if (Self.mTimeDefined) then
   Result := Result + FormatDateTime('nn', Self.mTime);
 Result := Result + '|';
 if (Self.predict) then
   Result := Result + ownConvert.ColorToStr(clYellow) + '|'
 else
   Result := Result + ownConvert.ColorToStr(clAqua) + '|';

 for addr in Self.train.HVs do
   Result := Result + HVDb[addr].name + '|';
end;

function TBlkRailwayTrain.GeTTrain(): TTrain;
begin
 if (Self.traini = -1) then
   Exit(nil);
 Result := Trains[Self.trainI];
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit

