unit Train;

{ This file defines "TTrain" class which represents a single train. }

interface

uses IniFiles, SysUtils, Classes, Forms, THnaciVozidlo, JsonDataObjects,
     Generics.Collections, predvidanyOdjezd, Block, Trakce, Math, IdContext,
     Logging, Area, ConfSeq, UPO;

const
  _MAX_TRAIN_HV = 4;
  _TRAVELED_REFRESH_PERIOD_MS = 980;

type
  ENotInRailway = class(Exception);
  EAlreadyOverriden = class(Exception);
  ENotOverriden = class(Exception);
  ENotStoppped = class(Exception);

  TTrainHVs = TList<Integer>; // seznam adres hnacich vozidel na souprave

  TTrainAcquire = record
    ok, err: TCb;
    toAcquire: TList<Integer>;
    nextAcquire: Integer;
  end;

  TVezmi = record
    ok, err: TCb;
    nextVezmi: Integer;
  end;

  TTrainData = record
    name: string;
    carsCount: Cardinal;
    note: string;
    length: Integer; // length of a train in centimeters
    typ: string; // MOs, Os, Mn, Pn, ...
    dir_L, dir_S: Boolean; // allowed directions
    HVs: TTrainHVs; // locomotives (engines)
    area: TObject; // Instance of TArea

    speed: Integer; // real speed passed to locomotives after all limitations
    wantedSpeed: Integer; // speed wanted by caller, before limitations
    maxSpeed: Cardinal; // max speed in km/h entered by user; maxSpeed = 0 <=> no limitation
    direction: THVSite;

    front: TObject; // most forward block train is/was on (always instance of TBlkUsek)
    areaFrom: TObject; // instance of TArea or Nil
    areaTo: TObject; // instance of TArea or Nil

    announcement: Boolean;
    announcementPlayed: Boolean;

    podj: TDictionary<Integer, TPOdj>;  // map track id: podj
  end;

  // Allow to override train speed
  // When override is enabled, setting of speed does NOT cause the actual train speed to be changed
  // just internal override.speed is changed.
  // This mechanism is suitable for train emergency stopping, stopping in stops etc.
  TTrainSpeedOverride = record
    isOverride: Boolean;
    allowRestore: Boolean;
    speed: Integer;
  end;

  TTrain = class
   private
    data: TTrainData;
    findex: Integer;
    filefront: Integer;
    fAcquiring: Boolean;
    _speedOverride: TTrainSpeedOverride;
    _emergencyStopped: Boolean;
    _traveled: Real; // never decreasing (for distance rrEvents); in meters
    _nextTraveledChange: TDateTime;

     procedure Init(index: Integer);
     procedure LoadFromFile(ini: TMemIniFile; const section: string);
     procedure LocoAcquiredOk(Sender: TObject; Data: Pointer);
     procedure LocoAcquiredErr(Sender: TObject; Data: Pointer);
     procedure AllLocoAcquiredOk(newLoks: TList<Integer>);

     procedure AcquireOk(Sender: TObject; Data: Pointer);
     procedure AcquireErr(Sender: TObject; Data: Pointer);

     procedure ReleaseAllLoko();

     procedure SetArea(area: TObject);

     procedure HVComErr(Sender: TObject; Data: Pointer);
     procedure SetSpeed(speed: Integer);
     procedure SetDirection(direction: THVSite);
     procedure SetFront(front: TObject);

     function IsStolen(): Boolean;
     function GetMaxSpeed(): Cardinal;
     function GetMaxSpeedStep(): Cardinal;
     function IsOnlyInRailway(): Boolean;

     procedure UpdateTrainFromJson(train: TJsonObject; ok: TCb; err: TCb);
     class procedure PtHVsListToDict(train: TJsonObject);

     procedure Log(msg: string; level: TLogLevel; source: TLogSource = lsAny);

   public

    changed: Boolean;

     constructor Create(ini: TMemIniFile; const section: string; index: Integer); overload;
     constructor Create(panelStr: TStrings; index: Integer; usek: TObject; area: TObject; ok: TCb; err: TCb); overload;
     constructor Create(train: TJsonObject; index: Integer; ok: TCb; err: TCb); overload;
     destructor Destroy(); override;

     procedure SaveToFile(ini: TMemIniFile; const section: string);

     function GetPanelString(): string;   // vraci string, kterym je definovana souprava, do panelu
     procedure UpdateTrainFromPanel(train: TStrings; usek: TObject; area: TObject; ok: TCb; err: TCb);
     procedure SetSpeedDirection(speed: Cardinal; dir: THVSite);
     procedure Acquire(ok: TCb; err: TCb);
     procedure UpdateFront();
     procedure ChangeDirection();
     procedure InterChangeArea(change_ev: Boolean = true);
     procedure LokDirChanged();
     procedure CheckAnnouncement(signal: TObject);

     procedure EnableSpeedOverride(newSpeed: Integer; allowRestore: Boolean);
     procedure DisableSpeedOverride();
     function IsSpeedOverride(): Boolean;

     procedure ToggleHouk(desc: string);
     procedure SetHoukState(desc: string; state: Boolean);

     procedure AddOrUpdatePOdj(usekid: Integer; var podj: TPOdj); overload;
     procedure AddOrUpdatePOdj(usek: TBlk; var podj: TPOdj); overload;
     function IsPOdj(usekid: Integer): Boolean; overload;
     function IsPOdj(usek: TBlk): Boolean; overload;
     function GetPOdj(usekid: Integer): TPOdj; overload;
     function GetPOdj(usek: TBlk): TPOdj; overload;
     procedure RemovePOdj(usekid: Integer); overload;
     procedure RemovePOdj(usek: TBlk); overload;
     procedure ClearPOdj();
     function IsAnyLokoInRegulator(): Boolean;
     procedure ForceRemoveAllRegulators();
     procedure EmergencyStop();
     procedure EmergencyStopRelease();
     function HasAnyHVNote(): Boolean;

     procedure UpdateRailwaySpeed();
     function GetRailwaySpeed(var speed: Cardinal): Boolean;
     function GetBlocks(): TList<TBlk>;

     function PredictedSignal(): TBlk;
     procedure OnPredictedSignalChange();
     procedure OnExpectedSpeedChange();

     procedure GetPtData(json: TJsonObject);
     procedure PutPtData(reqJson: TJsonObject; respJson: TJsonObject);

     function Menu(SenderPnl: TIdContext; SenderOR: TObject; SenderTrack: TBlk; SenderTrackI: Integer): string;
     function MenuRailwayPredicted(): string;
     function InfoWindowItems(): TList<TConfSeqItem>;
     function StrArrowDirection(): string;

     procedure CallChangeToTracks();
     function RucBarriers(): TList<TUPOItem>;
     procedure RucUPO(AContext: TIdContext; ref: TObject = nil; callbackOk: TNotifyEvent = nil; callbackEsc: TNotifyEvent = nil);
     function IsAnyHVRuc(): Boolean;

     procedure UpdateTraveled(msSinceLastUpdate: Cardinal);

     property index: Integer read findex;
     property sdata: TTrainData read data;

     property name: string read data.name;
     property station: TObject read data.area write SetArea;
     property speed: Integer read data.speed write SetSpeed;
     property wantedSpeed: Integer read data.wantedSpeed;
     property direction: THVSite read data.direction write SetDirection;
     property stolen: Boolean read IsStolen;
     property front: TObject read data.front write SetFront;
     property length: Integer read data.length;
     property typ: string read data.typ;

     property areaFrom: TObject read data.areaFrom;
     property areaTo: TObject read data.areaTo;

     property announcement: Boolean read data.announcement;
     property announcementPlayed: Boolean read data.announcementPlayed;

     property HVs: TTrainHVs read data.HVs;
     property maxSpeed: Cardinal read GetMaxSpeed; // warning: this could be speed with no speed step
     property maxSpeedStep: Cardinal read GetMaxSpeedStep;
     property acquiring: Boolean read fAcquiring;
     property emergencyStopped: Boolean read _emergencyStopped;
     property traveled: Real read _traveled;

     // uvolni stara hnaci vozidla ze soupravy (pri zmene HV na souprave)
     class procedure UvolV(old: TTrainHVs; new: TTrainHVs);

  end;

implementation

uses THVDatabase, ownStrUtils, TrainDb, BlockTrack, appEv,
      DataHV, AreaDb, TCPServerPanel, BlockDb, BlockSignal, blockRailway,
      fRegulator, fMain, BlockRailwayTrack, announcementHelper, announcement,
      TechnologieTrakce, ownConvert, TJCDatabase, TechnologieJC, IfThenElse,
      TCPAreasRef, JCBarriers, Config;

////////////////////////////////////////////////////////////////////////////////

constructor TTrain.Create(ini: TMemIniFile; const section: string; index: Integer);
begin
 inherited Create();
 Self.Init(index);
 Self.LoadFromFile(ini, section);
end;

constructor TTrain.Create(panelStr: TStrings; index: Integer; usek: TObject; area: TObject; ok: TCb; err: TCb);
begin
 inherited Create();
 Self.Init(index);
 Self.UpdateTrainFromPanel(panelStr, usek, area, ok, err);
end;

constructor TTrain.Create(train: TJsonObject; index: Integer; ok: TCb; err: TCb);
begin
 inherited Create();
 Self.Init(index);

 if (not train.Contains('station')) then
   raise Exception.Create('Vlak musí obsahovat pole "station"!');
 if (not train.Contains('front')) then
   raise Exception.Create('Vlak musí obsahovat pole "front"!');

 TTrain.PtHVsListToDict(train);
 Self.UpdateTrainFromJson(train, ok, err);
end;

procedure TTrain.Init(index: Integer);
begin
 Self._speedOverride.isOverride := false;
 Self.changed := false;
 Self.findex := index;
 Self.data.podj := TDictionary<Integer, TPOdj>.Create();
 Self.data.HVs := TList<Integer>.Create();
 Self.data.announcementPlayed := false;
 Self.fAcquiring := false;
 Self._emergencyStopped := false;
 Self._traveled := 0;
 Self._nextTraveledChange := 0;
end;

destructor TTrain.Destroy();
begin
 Self.ReleaseAllLoko();
 Self.ClearPOdj();
 Self.data.podj.Free();
 Self.data.HVs.Free();

 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrain.LoadFromFile(ini: TMemIniFile; const section: string);
begin
 Self.data.name := ini.ReadString(section, 'nazev', section);
 Self.data.carsCount := ini.ReadInteger(section, 'vozu', 0);
 Self.data.note := ini.ReadString(section, 'note', '');
 Self.data.dir_L := ini.ReadBool(section, 'L', false);
 Self.data.dir_S := ini.ReadBool(section, 'S', false);
 Self.data.length := ini.ReadInteger(section, 'delka', 0);
 Self.data.typ := ini.ReadString(section, 'typ', '');
 Self.filefront := ini.ReadInteger(section, 'front', -1);
 Self.data.direction := THVSite(ini.ReadInteger(section, 'smer', Integer(THVSite.odd)));
 Self.data.maxSpeed := ini.ReadInteger(section, 'maxRychlost', 0);

 Self.data.areaFrom := Areas.Get(ini.ReadString(section, 'z', ''));
 Self.data.areaTo := Areas.Get(ini.ReadString(section, 'do', ''));
 Self.data.area := Areas.Get(ini.ReadString(section, 'OR', ''));
 Self.data.announcement := ini.ReadBool(section, 'hlaseni', false);

 var strings: TStrings := TStringList.Create();
 ExtractStrings([';', ','], [], PChar(ini.ReadString(section, 'HV', '')), strings);

 while (strings.Count > _MAX_TRAIN_HV) do
   strings.Delete(_MAX_TRAIN_HV);

 // HV se nacitaji takto prapodivne pro osetreni pripadu, kdy u soupravy je uvedene HV, ktere neexistuje
 Self.data.HVs.Clear();
 try
   for var s in strings do
    begin
     var addr := StrToInt(s);
     if (Assigned(HVDb[addr])) then
      begin
       HVDb[addr].train := Self.index;
       Self.data.HVs.Add(addr);
      end;
    end;
 except

 end;

 strings.Free();
 Self.changed := true;
end;

procedure TTrain.SaveToFile(ini: TMemIniFile; const section: string);
begin
 ini.WriteString(section, 'nazev', Self.data.name);
 ini.WriteInteger(section, 'vozu', Self.data.carsCount);

 if (Self.data.note <> '') then
   ini.WriteString(section, 'poznamka', Self.data.note)
 else
   ini.DeleteKey(section, 'poznamka');

 ini.WriteInteger(section, 'delka', Self.data.length);
 ini.WriteString(section, 'typ', Self.data.typ);
 ini.WriteBool(section, 'L', Self.data.dir_L);
 ini.WriteBool(section, 'S', Self.data.dir_S);
 ini.WriteInteger(section, 'smer', Integer(Self.data.direction));
 if (Self.data.maxSpeed > 0) then
   ini.WriteInteger(section, 'maxRychlost', Self.data.maxSpeed);

 if (Self.data.areaFrom <> nil) then
   ini.WriteString(section, 'z', TArea(Self.data.areaFrom).id)
 else
   ini.DeleteKey(section, 'z');

 if (Self.data.areaTo <> nil) then
   ini.WriteString(section, 'do', TArea(Self.data.areaTo).id)
 else
   ini.DeleteKey(section, 'do');

 if (Self.data.front <> nil) then
   ini.WriteInteger(section, 'front', (Self.data.front as TBlk).id)
 else
   ini.WriteInteger(section, 'front', -1);

 if (Self.data.area <> nil) then
   ini.WriteString(section, 'OR', (Self.data.area as TArea).id)
 else
   ini.DeleteKey(section, 'OR');

 ini.WriteBool(section, 'hlaseni', Self.data.announcement);

 begin
   var str := ownConvert.SerializeIntList(Self.HVs);
   ini.WriteString(section, 'HV', str);
 end;
end;

////////////////////////////////////////////////////////////////////////////////

// vraci string, kterym je definovana souprava, do panelu
function TTrain.GetPanelString(): string;
begin
 Result := Self.data.name + ';' + IntToStr(Self.data.carsCount) + ';{' + Self.data.note + '};';

 if (Self.data.dir_L) then
   Result := Result + '1'
 else
   Result := Result + '0';

 if (Self.data.dir_S) then
   Result := Result + '1'
 else
   Result := Result + '0';

 Result := Result + ';' + IntToStr(Self.data.length) + ';' + Self.data.typ + ';{';

 for var addr in Self.HVs do
   Result := Result + '[{' + HVDb[addr].GetPanelLokString() + '}]';
 Result := Result + '};';

 if (Self.areaFrom <> nil) then
   Result := Result + TArea(Self.areaFrom).id;
 Result := Result + ';';

 if (Self.areaTo <> nil) then
   Result := Result + TArea(Self.areaTo).id;
 Result := Result + ';';

 if (Self.data.announcement) then
   Result := Result + '1;'
 else
   Result := Result + '0;';

 if (Self.data.maxSpeed <> 0) then
   Result := Result + IntToStr(Self.data.maxSpeed) + ';'
 else
   Result := Result + ';';
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrain.UpdateTrainFromPanel(train: TStrings; Usek: TObject; area: TObject; ok: TCb; err: TCb);
var json: TJsonObject;
    hvs, hv: TStrings;
begin
 json := TJsonObject.Create();
 try
   json['name'] := train[0];
   json['carsCount'] := train[1];
   json['note'] := train[2];
   json['dirL'] := train[3][1] = '1';
   json['dirS'] := train[3][2] = '1';
   json['length'] := train[4];
   json['type'] := train[5];
   json['station'] := TArea(area).id;
   json['front'] := TBlk(usek).id;

   if (train.Count > 7) then
     json['areaFrom'] := train[7];
   if (train.Count > 8) then
     json['areaTo'] := train[8];

   if (train.Count > 9) then
     json['announcement'] := (train[9] = '1')
   else
     json['announcement'] := TStationAnnouncement.AnnounceTrainType(json.S['type']);

   if ((train.Count > 10) and (train[10] <> '')) then
     json['maxSpeed'] := StrToInt(train[10])
   else
     json['maxSpeed'] := 0;

   hvs := TStringList.Create();
   hv := TStringList.Create();
   try
     ExtractStringsEx([']'], ['['], train[6], hvs);

     for var s in hvs do
      begin
       hv.Clear();
       ExtractStringsEx(['|'], [], s, hv);
       var straddr := hv[4];
       var hvobj := json['hvs'].O[straddr];
       hvobj['note'] := hv[3];
       hvobj['sta'] := StrToInt(hv[7]);
       hvobj['func'] := hv[8];
      end;
   finally
     hvs.Free();
     hv.Free();
   end;

   Self.UpdateTrainFromJson(json, ok, err);
 finally
   json.Free();
 end;
end;

procedure TTrain.UpdateTrainFromJson(train: TJsonObject; ok: TCb; err: TCb);
var acq: ^TTrainAcquire;
begin
 if (Self.acquiring) then
   raise Exception.Create('Přebírání lokomotiv soupravy již probíhá!');

 // zkontrolujeme, jestli nejaka souprava s timto cislem uz nahodou neexistuje
 for var i := 0 to _MAX_TRAIN-1 do
  begin
   if (Trains[i] = nil) then continue;

   if ((Trains[i].name = train['name']) and (Trains[i] <> Self)) then
    begin
     if (Trains[i].station <> nil) then
       raise Exception.Create('Souprava '+Trains[i].name+' již existuje v OŘ '+(Trains[i].station as TArea).name);
     raise Exception.Create('Souprava '+Trains[i].name+' již existuje');
    end;
  end;

 try
  StrToInt(train['name']);
 except
   on E: EConvertError do
     raise Exception.Create('Číslo soupravy není validní číslo!');
 end;

 Self.changed := true;
 Self.data.name := train['name'];

 if (train.Contains('carsCount')) then
   Self.data.carsCount := StrToInt(train['carsCount']);
 if (train.Contains('note')) then
   Self.data.note := train['note'];
 if (train.Contains('dirL')) then
   Self.data.dir_L := train.B['dirL'];
 if (train.Contains('dirS')) then
   Self.data.dir_S := train.B['dirS'];
 if (train.Contains('length')) then
   Self.data.length := train['length'];
 if (train.Contains('type')) then
   Self.data.typ := train['type'];
 if (train.Contains('station')) then
   Self.data.area := Areas.Get(train.S['station']);
 if (train.Contains('front')) then
   TBlk(Self.data.front) := Blocks.GetBlkByID(train['front']);

 if (train.Contains('areaFrom')) then
   Self.data.areaFrom := Areas.Get(train.S['areaFrom']);
 if (train.Contains('areaTo')) then
   Self.data.areaTo := Areas.Get(train.S['areaTo']);

 if (train.Contains('announcement')) then
   Self.data.announcement := train['announcement'];

 if (train.Contains('maxSpeed')) then
   Self.data.maxSpeed := StrToInt(train['maxSpeed']);

 if ((Self.wantedSpeed = 0) and (Self.data.dir_L xor Self.data.dir_S)) then
  begin
   // vypocet smeru ze sipky
   if (Self.data.dir_L) then
     Self.data.direction := THVSite.odd
   else
     Self.data.direction := THVSite.even;

   for var addr in Self.HVs do
     HVDb[addr].OnPredictedSignalChange();
  end;

 if (train.O['hvs'].Count > _MAX_TRAIN_HV) then
   raise Exception.Create('Překročen maximální počet hnacích vozidel na soupravě');

 var new := TList<Integer>.Create();
 try
   for var addrhv in train.O['hvs'] do
    begin
     var hv : TJsonObject := addrhv.Value;
     var addr := StrToInt(addrhv.Name);

     if (not Assigned(HVDb[addr])) then
       raise Exception.Create('Loko '+IntToStr(addr)+' neexistuje na serveru!');

     if ((HVDb[addr].train > -1) and (HVDb[addr].train <> Self.index)) then
       raise Exception.Create('Loko '+IntToStr(addr)+' již přiřazena soupravě '+Trains.GetTrainNameByIndex(HVDb[addr].train));

     if (new.Contains(addr)) then
       raise Exception.Create('Duplicitní loko!');

     if (hv.Contains('note')) then
       HVDb[addr].Data.note := hv['note'];
     if (hv.Contains('sta')) then
       HVDb[addr].state.siteA := THVSite(hv.I['sta']);

     HVDb[addr].train := Self.index;

     if (hv.Contains('func')) then
      begin
       var max_func := Min(System.Length(hv.S['func']), _HV_FUNC_MAX);
       for var i := 0 to max_func do
         HVDb[addr].state.functions[i] := (hv.S['func'][i+1] = '1');
      end;

     new.Add(addr);
    end;

   GetMem(acq, sizeof(TTrainAcquire));
   acq^.ok := ok;
   acq^.err := err;
   acq^.toAcquire := new;
   acq^.nextAcquire := 0;

   Self.fAcquiring := true;
   Self.LocoAcquiredOk(Self, acq);
 except
   new.Free();
 end;
end;

procedure TTrain.LocoAcquiredOk(Sender: TObject; Data: Pointer);
var acq: ^TTrainAcquire;
    addr: Integer;
begin
 acq := Data;

 if (not Self.acquiring) then
  begin
   Self.AllLocoAcquiredOk(acq^.toAcquire);
   if (Assigned(acq^.err.callback)) then
     acq^.err.callback(Self, acq^.err.data);
   FreeMem(acq);
   Exit();
  end;

 if (acq^.nextAcquire >= acq^.toAcquire.Count) then
  begin
   Self.AllLocoAcquiredOk(acq^.toAcquire);
   if (Assigned(acq^.ok.callback)) then
     acq^.ok.callback(Self, acq^.ok.data);
   FreeMem(acq);
   Exit();
  end;

 addr := acq^.toAcquire[acq^.nextAcquire];
 acq^.nextAcquire := acq^.nextAcquire + 1;

 if (not HVDb[addr].acquired) then
  begin
   HVDb[addr].TrakceAcquire(TTrakce.Callback(Self.LocoAcquiredOk, acq),
                            TTrakce.Callback(Self.LocoAcquiredErr, acq));
  end else begin
   HVDb[addr].StateFunctionsToSlotFunctions(TTrakce.Callback(Self.LocoAcquiredOk, acq),
                                            TTrakce.Callback(Self.LocoAcquiredErr, acq));
  end;
end;

procedure TTrain.LocoAcquiredErr(Sender: TObject; Data: Pointer);
var acq: ^TTrainAcquire;
begin
 acq := Data;
 Self.fAcquiring := false;
 if (Assigned(acq^.err.callback)) then
   acq^.err.callback(Self, acq^.err.data);
 acq^.toAcquire.Free();
 FreeMem(acq);
 Self.changed := true;
end;

procedure TTrain.AllLocoAcquiredOk(newLoks: TList<Integer>);
begin
 Self.fAcquiring := false;
 Self.UvolV(Self.HVs, newLoks);
 Self.data.HVs.Free();
 Self.data.HVs := newLoks;

 Self.SetSpeedDirection(Self.speed, Self.direction);
 Blocks.ChangeTrainToRailway(Self);

 if (Self.front <> nil) then
   TBlkTrack(Self.front).Change();

 for var blk in Blocks do
   if (((blk.typ = TBlkType.btTrack) or (blk.typ = TBlkType.btRT)) and
       ((TBlkTrack(blk).trains.Contains(Self.index)) or (TBlkTrack(blk).trainPredict = Self))) then
     blk.Change();

 for var signal in TBlkTrack(Self.front).signalJCRef do
   TBlkSignal(signal).UpdateTrainSpeed(true);

 Self.changed := true;
end;

////////////////////////////////////////////////////////////////////////////////

class procedure TTrain.UvolV(old: TTrainHVs; new: TTrainHVs);
var keep: TList<Integer>;
begin
 keep := TList<Integer>.Create();

 try
   for var new_addr in new do
     for var old_addr in old do
       if (new_addr = old_addr) then
          keep.Add(new_addr);

   for var old_addr in old do
    begin
     if (not keep.Contains(old_addr)) then
      begin
       // vozidlo, ktere neni v novem seznamu -> uvolnit
       HVDb[old_addr].train := -1;
      end;
    end;
 finally
   keep.Free();
 end;
end;

////////////////////////////////////////////////////////////////////////////////

// uvolnit vsechna loko
// pred uvolnenim loko take zastavime
procedure TTrain.ReleaseAllLoko();
begin
 if ((not Assigned(HVDb)) or (not Assigned(TrakceI))) then Exit();

 for var addr in Self.HVs do
  begin
   if (not Assigned(HVDb[addr])) then
     continue;

   HVDb[addr].train := -1;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrain.SetArea(area: TObject);
begin
 Self.data.area := area;
 for var addr in Self.HVs do
   HVDb[addr].MoveToArea(area as TArea);
 Self.Data.announcementPlayed := false;
 Self.changed := true;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrain.SetSpeedDirection(speed: Cardinal; dir: THVSite);
var dir_changed: Boolean;
begin
 if ((Self.front <> nil) and (TBlk(Self.front).typ = btRT) and (TBlkRT(Self.front).speedUpdate)) then
   TBlkRT(Self.front).speedUpdate := false;

 dir_changed := (Self.direction <> dir);
 Self.data.direction := dir;
 Self.data.wantedSpeed := speed;
 if (Self.emergencyStopped) then
   Self.data.speed := 0
 else if (Self.IsSpeedOverride()) then
   Self.data.speed := Self._speedOverride.speed
 else if (speed > Self.maxSpeed) then
   Self.data.speed := Self.maxSpeed
 else
   Self.data.speed := speed;

 if ((Self.front <> nil) and (TBlk(Self.front).typ = btRT) and (TBlkRT(Self.front).railway <> nil)) then
   TBlkRT(Self.front).railway.Change();

 for var addr in Self.HVs do
  begin
   HVDb[addr].OnExpectedSpeedChange();
   if (dir_changed) then
     HVDb[addr].OnPredictedSignalChange();

   if (HVDb[addr].ruc) then
    begin
     Self.Log('LOKO ' + IntToStr(addr) + ' v ručním regulátoru, nenastavuji rychlost', llInfo);
     continue;
    end;
   if (HVDb[addr].stolen) then
    begin
     Self.Log('LOKO ' + IntToStr(addr) + ' ukradena, nenastavuji rychlost', llInfo);
     continue;
    end;

   var direction := ownConvert.IntToBool(Integer(dir) xor Integer(HVDb[addr].state.siteA));

   try
     HVDb[addr].SetSpeedDir(Self.data.speed, direction,
                            TTrakce.Callback(), TTrakce.Callback(Self.HVComErr), Self);
   except
     on E: Exception do
       AppEvents.LogException(E, 'TTrain.SetSpeedDirection');
   end;
  end;

 if ((speed > 0) and (Assigned(Self.front)) and
     ((Self.front as TBlkTrack).IsTrainMoving()) and
      ((Self.front as TBlkTrack).trains[(Self.front as TBlkTrack).trainMoving] = Self.index)) then
  (Self.front as TBlkTrack).trainMoving := -1;

 Self.Log('rychlost want='+IntToStr(speed)+', real='+IntToStr(Self.speed)+', směr : '+IntToStr(Integer(dir)), llInfo);
 Self.changed := true;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrain.SetSpeed(speed: Integer);
begin
 Self.SetSpeedDirection(speed, Self.data.direction);
end;

procedure TTrain.SetDirection(direction: THVSite);
begin
 Self.SetSpeedDirection(Self.data.speed, direction);
end;

procedure TTrain.EmergencyStop();
begin
  Self._emergencyStopped := true;

  for var addr in Self.HVs do
    HVDb[addr].EmergencyStop(TTrakce.Callback(), TTrakce.Callback(Self.HVComErr), Self);

  Self.Log('Nouzové zastavení', llInfo);
  Self.SetSpeedDirection(Self.wantedSpeed, Self.direction);
  Self.CallChangeToTracks();
end;

procedure TTrain.EmergencyStopRelease();
begin
  if (not Self.emergencyStopped) then
    raise ENotOverriden.Create('Loco not emergency stopeed');

  Self._emergencyStopped := false;
  Self.SetSpeedDirection(Self.wantedSpeed, Self.direction);
  Self.CallChangeToTracks();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrain.HVComErr(Sender: TObject; Data: Pointer);
begin
 if (Self.data.area <> nil) then
   (Self.data.area as TArea).BlkWriteError(nil, 'Souprava '+Self.name+' nekomunikuje s centrálou', 'CENTRÁLA');
end;

////////////////////////////////////////////////////////////////////////////////

function TTrain.IsStolen(): Boolean;
begin
 for var addr in Self.HVs do
   if (HVDb[addr].stolen) then
     Exit(true);
 Result := false;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrain.Acquire(ok: TCb; err: TCb);
var vezmi: ^TVezmi;
begin
 GetMem(vezmi, sizeof(TVezmi));
 vezmi^.ok := ok;
 vezmi^.err := err;

 vezmi^.nextVezmi := 0;
 while ((vezmi^.nextVezmi < Self.HVs.Count) and (not HVDb[Self.HVs[vezmi^.nextVezmi]].stolen)) do
   Inc(vezmi^.nextVezmi);

 Self.AcquireOk(Self, vezmi);
end;

procedure TTrain.AcquireOk(Sender: TObject; Data: Pointer);
var vezmi: ^TVezmi;
    addr: Integer;
begin
 vezmi := Data;

 if (vezmi^.nextVezmi >= Self.HVs.Count) then
  begin
   Self.changed := true;
   Self.SetSpeedDirection(Self.speed, Self.direction);
   if (Assigned(vezmi^.ok.callback)) then
     vezmi^.ok.callback(Self, vezmi^.ok.data);
   FreeMem(vezmi);
   Exit();
  end;

 addr := Self.HVs[vezmi^.nextVezmi];

 vezmi^.nextVezmi := vezmi^.nextVezmi + 1;
 while ((vezmi^.nextVezmi < Self.HVs.Count) and (not HVDb[Self.HVs[vezmi^.nextVezmi]].stolen)) do
   Inc(vezmi^.nextVezmi);

 try
   HVDb[addr].TrakceAcquire(TTrakce.Callback(Self.AcquireOk, vezmi),
                            TTrakce.Callback(Self.AcquireErr, vezmi));
 except
   Self.AcquireErr(Sender, vezmi);
 end;
end;

procedure TTrain.AcquireErr(Sender: TObject; Data: Pointer);
var vezmi: ^TVezmi;
begin
 vezmi := Data;
 if (Assigned(vezmi^.err.callback)) then
   vezmi^.err.callback(Self, vezmi^.err.data);
 FreeMem(vezmi);
 Self.changed := true;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrain.SetFront(front: TObject);
begin
 if (Self.data.front = front) then Exit();

 if (Assigned(Self.data.front)) then
   (Self.data.front as TBlkTrack).slowingReady := false;
 Self.data.front := front;

 for var addr in Self.HVs do
   HVDb[addr].OnPredictedSignalChange();

 Self.changed := true;
end;

procedure TTrain.UpdateFront();
begin
 Self.front := Blocks.GetBlkTrackOrRTByID(Self.filefront);
end;

////////////////////////////////////////////////////////////////////////////////

// zmena smeru pri naslapu na smyckovy blok
procedure TTrain.ChangeDirection();
begin
 Self.Log('Změna směru', llInfo);

 // zmenit orintaci stanoviste A hnacich vozidel
 for var addr in Self.HVs do
  begin
   case (HVDb[addr].state.siteA) of
    THVSite.odd : HVDb[addr].state.siteA := THVSite.even;
    THVSite.even : HVDb[addr].state.siteA := THVSite.odd;
   end;
  end;

 // zmenit orientaci sipky soupravy
 var tmp := Self.data.dir_L;
 Self.data.dir_L := Self.data.dir_S;
 Self.data.dir_S := tmp;

 // zmenit smer suupravy - dulezite pro zastaveni pred navestidlem
 case (Self.data.direction) of
  THVSite.odd : Self.direction := THVSite.even;
  THVSite.even : Self.direction := THVSite.odd;
 end;

 if (Self.front <> nil) then
   (Self.front as TBlkTrack).Change();  // kvuli sipce
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrain.InterChangeArea(change_ev: Boolean = true);
var tmp: TObject;
begin
 tmp := Self.data.areaFrom;
 Self.data.areaFrom := Self.data.areaTo;
 Self.data.areaTo := tmp;

 Self.changed := true;
 if ((Self.front <> nil) and (change_ev)) then
   (Self.front as TBlkTrack).Change();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrain.EnableSpeedOverride(newSpeed: Integer; allowRestore: Boolean);
begin
  if (Self._speedOverride.isOverride) then
    raise EAlreadyOverriden.Create('Speed already overriden');

  Self._speedOverride.isOverride := true;
  Self._speedOverride.allowRestore := allowRestore;
  Self._speedOverride.speed := newSpeed;
  Self.SetSpeedDirection(Self.wantedSpeed, Self.direction);
end;

procedure TTrain.DisableSpeedOverride();
begin
  if (not Self._speedOverride.isOverride) then
    raise ENotOverriden.Create('Speed not overriden');

  Self._speedOverride.isOverride := false;
  Self.SetSpeedDirection(Self.wantedSpeed, Self.direction);
end;

function TTrain.IsSpeedOverride(): Boolean;
begin
  Result := Self._speedOverride.isOverride;
end;

////////////////////////////////////////////////////////////////////////////////
// V pripade, ze vsechna hnaci vozidla soupravy otocim do opacneho smeru,
// nez je smer soupravy, otoci se i smer soupravy. To umoznuje otoceni smeru
// soupravy z Rocomaus.
// Tato zmena je umoznena jen tehdy pokud nema sipka jednoznacne urceny smer
// a pokud souprava stoji.

procedure TTrain.LokDirChanged();
var dir: Boolean;
begin
 if ((Self.wantedSpeed <> 0) or (Self.data.dir_L xor Self.data.dir_S) or
     (Self.HVs.Count = 0) or ((Self.front <> nil) and (not TBlkTrack(Self.front).spnl.stationTrack))) then
   Exit();

 dir := HVDb[Self.HVs[0]].stACurrentDirection;

 if (dir = ownConvert.IntToBool(Integer(Self.direction))) then Exit();
 for var i := 1 to Self.HVs.Count-1 do
   if (dir <> HVDb[Self.HVs[i]].stACurrentDirection) then
     Exit();

 // vsechna hv nastavena do opacneho smeru -> zmenit smer soupravy
 Self.direction := THVSite(dir);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrain.ToggleHouk(desc: string);
begin
 Self.Log('Aktivuji houkání ' + desc, llInfo);

 for var addr in Self.HVs do
  begin
   var HV := HVDb[addr];
   if (HV.CanPlayHouk(desc)) then
     TrakceI.LokFuncToggle(Self, HV, HV.funcDict[desc]);
  end;
end;

procedure TTrain.SetHoukState(desc: string; state: Boolean);
begin
 if (state) then
   Self.Log('Aktivuji funkci ' + desc, llInfo)
 else
   Self.Log('Deaktivuji funkci ' + desc, llInfo);

 for var addr in Self.HVs do
  begin
   var HV := HVDb[addr];
   if (HV.CanPlayHouk(desc)) then
     HV.SetSingleFunc(HV.funcDict[desc], state, TTrakce.Callback(), TTrakce.Callback(), Self);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrain.CheckAnnouncement(signal: TObject);
var msignal: TBlkSignal;
    area: TArea;
    annPlay: TAnnToPlay;
    annTrain: TAnnTrain;
begin
 if ((not Self.announcement) or (Self.announcementPlayed) or (self.areaFrom = nil) or
     (self.areaTo = nil) or (Self.typ = '')) then Exit();

 msignal := TBlkSignal(signal);
 if (msignal.areas.Count < 1) then Exit();
 area := msignal.areas[0];

 if ((not Assigned(area.announcement)) or (not area.announcement.available)) then Exit();

 try
   annPlay := announcementHelper.CanPlayArrival(self, area);
 except
   on E: Exception do
     AppEvents.LogException(E, 'CanPlayPrijezdSH');
 end;

 annTrain.name := Self.name;
 annTrain.typ := Self.typ;
 annTrain.fromAreaId := TArea(Self.areaFrom).id;
 annTrain.toAreaId := TArea(Self.areaTo).id;
 annTrain.timeArrive := 0;
 annTrain.timeDepart := 0;

 if (annPlay.stationTrack <> nil) then
  begin
   annTrain.track := annPlay.stationTrack.spnl.trackName;

   if ((Self.IsPOdj(annPlay.stationTrack)) and (Self.GetPOdj(annPlay.stationTrack).abs_enabled)) then
     annTrain.timeDepart := Self.GetPOdj(annPlay.stationTrack).abs;
  end;

 try
   if ((annPlay.stationTrack <> nil) and ((annPlay.railway = nil) or (Self.IsPOdj(annPlay.stationTrack)))) then begin
     area.announcement.Arrival(annTrain);
     Self.data.announcementPlayed := true;
   end else if (annPlay.railway <> nil) then begin
     area.announcement.Transit(annTrain);
     Self.data.announcementPlayed := true;
   end;
 except
   on E: Exception do
     AppEvents.LogException(E, 'Prehravani hlaseni');
 end;
end;

////////////////////////////////////////////////////////////////////////////////
// Predvidane odjezdy:

procedure TTrain.AddOrUpdatePOdj(usekid: Integer; var podj: TPOdj);
begin
 if ((not podj.rel_enabled) and (not podj.abs_enabled)) then
  begin
   if (Self.data.podj.ContainsKey(usekid)) then
    begin
     Self.data.podj[usekid].Free();
     Self.data.podj.Remove(usekid);
    end;
   FreeAndNil(podj);
  end else begin
   if (Self.data.podj.ContainsKey(usekid)) then
     Self.data.podj[usekid].Free();
   Self.data.podj.AddOrSetValue(usekid, podj);
   podj := nil;
  end;
end;

function TTrain.IsPOdj(usekid: Integer): Boolean;
begin
 Result := Self.data.podj.ContainsKey(usekid);
end;

function TTrain.GetPOdj(usekid: Integer): TPOdj;
begin
 Result := Self.data.podj[usekid];
end;

procedure TTrain.RemovePOdj(usekid: Integer);
begin
 Self.data.podj[usekid].Free();
 Self.data.podj.Remove(usekid);
end;

procedure TTrain.AddOrUpdatePOdj(usek: TBlk; var podj: TPOdj);
begin
 Self.AddOrUpdatePOdj(usek.id, podj);
end;

function TTrain.IsPOdj(usek: TBlk): Boolean;
begin
 if (usek = nil) then Exit(false);
 Result := Self.IsPOdj(usek.id);
end;

function TTrain.GetPOdj(usek: TBlk): TPOdj;
begin
 Result := Self.GetPOdj(usek.id);
end;

procedure TTrain.RemovePOdj(usek: TBlk);
begin
 Self.RemovePOdj(usek.id);
end;

procedure TTrain.ClearPOdj();
var podj: TPOdj;
begin
 for podj in Self.data.podj.Values do
   podj.Free();

 Self.data.podj.Clear();
end;

////////////////////////////////////////////////////////////////////////////////

function TTrain.IsAnyLokoInRegulator(): Boolean;
begin
 for var hvaddr in Self.HVs do
   if (HVDb[hvaddr].state.regulators.Count > 0) then
     Exit(true);
 Result := false;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrain.ForceRemoveAllRegulators();
var hvaddr: Integer;
begin
 for hvaddr in Self.HVs do
   if (HVDb[hvaddr].state.regulators.Count > 0) then
     HVDb[hvaddr].ForceRemoveAllRegulators();
end;

////////////////////////////////////////////////////////////////////////////////

function TTrain.GetMaxSpeed(): Cardinal;
var minimum: Cardinal;
begin

 if (Self.HVs.Count = 0) then
  begin
   if (Self.data.maxSpeed > 0) then
     Result := Min(Self.data.maxSpeed, THnaciVozidlo._DEFAUT_MAX_SPEED)
   else
     Result := THnaciVozidlo._DEFAUT_MAX_SPEED;
  end else begin
   if (Self.data.maxSpeed > 0) then
     minimum := Min(Self.data.maxSpeed, HVDb[Self.HVs[0]].Data.maxSpeed)
   else
     minimum := HVDb[Self.HVs[0]].Data.maxSpeed;

   for var addr in Self.HVs do
     if (HVDb[addr].Data.maxSpeed < minimum) then
       minimum := HVDb[addr].Data.maxSpeed;

   Result := minimum;
  end;
end;

function TTrain.GetMaxSpeedStep(): Cardinal;
begin
 // vraci rychlost <= max rychlosti takovou, ze pro ni mame prirazeni stupne
 // tj. tuto rychlost lze skutene nastavit
 Result := TrakceI.NearestLowerSpeed(Self.maxSpeed);
end;

////////////////////////////////////////////////////////////////////////////////

function TTrain.PredictedSignal(): TBlk;
var frontblk: TBlkTrack;
    signal: TBlkSignal;
begin
 frontblk := Self.front as TBlkTrack;
 if (frontblk = nil) then
   Exit(nil);

 if (frontblk.typ = btRT) then
   Exit(TBlkRT(frontblk).nextSignal);

 signal := nil;
 case (Self.direction) of
   THVSite.odd: signal := frontblk.signalL as TBlkSignal;
   THVSite.even: signal := frontblk.signalS as TBlkSignal;
 end;

 if ((signal <> nil) and (signal.SymbolType = TBlkSignalSymbol.main)) then
   Exit(signal);

 var jc := JCDb.FindActiveJCWithTrack(frontblk.id);
 if (jc = nil) then
   Exit(nil);

 case (jc.data.nextSignalType) of
  TJCNextSignalType.no: Exit(nil);
  TJCNextSignalType.railway: begin
    var rt: TBlkRT := Blocks.GetBlkRTByID(jc.data.tracks[jc.data.tracks.Count-1]);
    if (rt <> nil) then
      Exit(rt.nextSignal)
    else
      Exit(nil);
  end;
  TJCNextSignalType.signal: begin
    Exit(Blocks.GetBlkSignalByID(jc.data.nextSignalId));
  end;
 end;

 Result := nil;
end;

procedure TTrain.OnPredictedSignalChange();
begin
 for var addr in Self.HVs do
   HVDb[addr].OnPredictedSignalChange();
end;

procedure TTrain.OnExpectedSpeedChange();
begin
 for var addr in Self.HVs do
   HVDb[addr].OnExpectedSpeedChange();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrain.GetPtData(json: TJsonObject);
var addr: Integer;
    blkId: Integer;
begin
 json['name'] := Self.data.name;
 json['carsCount'] := Self.data.carsCount;
 if (Self.data.note <> '') then
   json['note'] := Self.data.note;
 json['length'] := Self.data.length;
 json['type'] := Self.data.typ;
 json['dirS'] := Self.data.dir_S;
 json['dirL'] := Self.data.dir_L;

 json.A['hvs'];
 for addr in Self.data.HVs do
   json.A['hvs'].Add(addr);

 if (Self.data.area <> nil) then
   json['station'] := TArea(Self.data.area).id;
 json['speed'] := Self.data.speed;
 json['wantedSpeed'] := Self.data.wantedSpeed;
 if (Self.data.maxSpeed > 0) then
   json['maxSpeed'] := Self.data.maxSpeed;
 json['direction'] := Integer(Self.data.direction);
 if (Self.data.front <> nil) then
   json['front'] := TBlk(Self.data.front).id;
 if (Self.data.areaFrom <> nil) then
   json['areaFrom'] := TArea(Self.data.areaFrom).id;
 if (Self.data.areaTo <> nil) then
   json['areaTo'] := TArea(Self.data.areaTo).id;
 json['announcement'] := Self.data.announcement;
 json.F['traveled'] := Self.traveled;

 for blkId in Self.data.podj.Keys do
   Self.data.podj[blkId].GetPtData(json.O['podj'].O[IntToStr(blkId)]);
end;

procedure TTrain.PutPtData(reqJson: TJsonObject; respJson: TJsonObject);
begin
 if (not reqJson.Contains('name')) then
   reqJson['name'] := Self.name;
 if (not reqJson.Contains('hvs')) then
   for var hvaddr in Self.HVs do
     reqJson.A['hvs'].Add(hvaddr);

 TTrain.PtHVsListToDict(reqJson);
 Self.UpdateTrainFromJson(reqJson, TTrakce.Callback(), TTrakce.Callback());
 Self.GetPtData(respJson['train']);
end;

class procedure TTrain.PtHVsListToDict(train: TJsonObject);
var hvs: TList<Integer>;
    hvaddr: Integer;
begin
 hvs := TList<Integer>.Create();
 try
   for hvaddr in train.A['hvs'] do
     hvs.Add(hvaddr);
   train.Remove('hvs');
   for hvaddr in hvs do
     train.O['hvs'].O[IntToStr(hvaddr)];
 finally
   hvs.Free();
 end;
end;

////////////////////////////////////////////////////////////////////////////////

// Returns speed train should have based on its presence in railways.
// Returns true iff train speed should be controlled by railway.
function TTrain.GetRailwaySpeed(var speed: Cardinal): Boolean;
begin
 if (Self.front = nil) then
   Exit(false);

 var rtsSpeed: Integer := -1;
 var tracks := Blocks.GetBlkWithTrain(Self);
 var onlyInRailway: Boolean := true;
 var jc: TJC := nil;
 try
   for var track: TBlk in tracks do
   begin
     if (track.typ = btRT) then
     begin
       var rtSpeed := TBlkRT(track).Speed(Self);
       rtsSpeed := ite(rtsSpeed = -1, rtSpeed, Min(rtsSpeed, rtSpeed));
     end else begin
       // assert TBlk(track).typ = btTrack
       onlyInRailway := false;
       var _jc: TJC := JCDb.FindActiveJCWithTrack(track.id);
       if (jc = nil) then
         jc := _jc
       else if ((_jc <> nil) and (jc <> _jc)) then
         Exit(false); // train in different paths -> definitelly should not be controlled by railway
     end;
   end;
 finally
   tracks.Free();
 end;

 if ((rtsSpeed = -1) or (not onlyInRailway)) then
 begin
   // train not in railway -> it could be on path going to railway
   // or it could be on path entering area
   if (jc = nil) then
     Exit(false);
   if (jc.lastTrack.typ <> btRT) then
     Exit(false);

   // all tracks from this one to the end cannot contain turnouts
   begin
     var found: Boolean := false;
     for var i: Integer := 0 to jc.data.tracks.Count-1 do
     begin
       var trackId: Integer := jc.data.tracks[i];
       if ((not found) and (TBlk(Self.front).id = trackId)) then
         found := true;
       if ((found) and (i < jc.data.tracks.Count-1)) then
       begin
         // do not consider last track -> could be in railway track with turnout
         var turnouts: TList<TBlk> := Blocks.GetTurnoutsAtTrack(trackId);
         if (turnouts.Count > 0) then
           Exit(false);
         turnouts.Free();
       end;
     end;

     if (not found) then
       Exit(false);
   end;

   rtsSpeed := TBlkRT(jc.lastTrack).speed(Self);
 end;

 speed := rtsSpeed;
 Result := true;
end;

procedure TTrain.UpdateRailwaySpeed();
begin
 var speed: Cardinal;
 var success := Self.GetRailwaySpeed(speed);
 if ((not success) or (Self.wantedSpeed = 0) or (Self.front = nil)) then
   Exit();

 if ((TBlkTrack(Self.front).slowingReady) and
     ((TBlk(Self.front).typ <> btRT) or (not TBlkRT(Self.front).stopSlowedDown))) then
 begin
   // Not yet slowed -> increase & decrease speed
   if (Cardinal(Self.wantedSpeed) <> speed) then
     Self.speed := speed;
 end else begin
   // Already slowed down -> do not increase speed, just decrease
   if (Cardinal(Self.wantedSpeed) > speed) then
     Self.speed := speed;
 end;
end;

////////////////////////////////////////////////////////////////////////////////

function TTrain.GetBlocks(): TList<TBlk>;
begin
 Result := Blocks.GetBlkWithTrain(Self);
end;

////////////////////////////////////////////////////////////////////////////////

function TTrain.IsOnlyInRailway(): Boolean;
begin
  var blks: TList<TBlk> := Self.GetBlocks();
  try
    for var blk: TBlk in blks do
      if (blk.typ <> TBlkType.btRT) then
        Exit(false);
  finally
    blks.Free();
  end;

  Result := true;
end;

////////////////////////////////////////////////////////////////////////////////

function TTrain.HasAnyHVNote(): Boolean;
begin
  Result := false;
  for var addr in Self.HVs do
    if (HVDb[addr].data.note <> '') then
      Exit(true);
end;

////////////////////////////////////////////////////////////////////////////////

function TTrain.Menu(SenderPnl: TIdContext; SenderOR: TObject; SenderTrack: TBlk; SenderTrackI: Integer): string;
var shPlay: announcementHelper.TAnnToPlay;
    train_count: Integer;
    track: TBlkTrack;
begin
  track := TBlkTrack(SenderTrack);
  train_count := Blocks.GetBlkWithTrain(Self).Count;

  if (Self.speed > 0) then
    Result := Result + 'STOP vlak>,';
  if (Self.emergencyStopped) then
    Result := Result + 'STOP vlak<,';

  if (track.CanStandTrain()) then
    Result := Result + 'EDIT vlak,';
  Result := Result + 'INFO vlak,';
  if ((track.CanStandTrain()) or (train_count <= 1)) then
    Result := Result + '!ZRUŠ vlak,';
  if (train_count > 1) then
    Result := Result + '!UVOL vlak,';

  if (Self.HVs.Count > 0) then
  begin
    Result := Result + 'RUČ vlak,';
    if (TPanelConnData(SenderPnl.Data).maus) then
      Result := Result + 'MAUS vlak,';
  end;

  if (track.trainMoving = SenderTrackI) then
    Result := Result + 'PŘESUŇ vlak<,'
  else if ((not track.IsTrainMoving()) and ((Self.wantedSpeed = 0) or (Self.emergencyStopped))) then
    Result := Result + 'PŘESUŇ vlak>,';

  if (Self.stolen) then
    Result := Result + 'VEZMI vlak,'
  else
  begin
    if (Self.IsAnyLokoInRegulator()) then
      Result := Result + '!VEZMI vlak,';
  end;

  if ((Self._speedOverride.isOverride) and (Self._speedOverride.allowRestore) and (not Self.emergencyStopped)) then
    Result := Result + 'JEĎ vlak,';

  if (track.CanStandTrain()) then
    Result := Result + 'PODJ,';

  if ((Assigned(TArea(SenderOR).announcement)) and (TArea(SenderOR).announcement.available) and (Self.areaFrom <> nil)
    and (Self.areaTo <> nil) and (Self.typ <> '')) then
  begin
    if ((track.spnl.stationTrack) and (Self.announcement)) then
      Result := Result + 'HLÁŠENÍ odjezd,';

    try
      shPlay := announcementHelper.CanPlayArrival(Self, TArea(SenderOR));
    except
      on E: Exception do
        AppEvents.LogException(E, 'CanPlayPrijezdSH');
    end;

    if ((shPlay.stationTrack <> nil) and ((shPlay.railway = nil) or (Self.IsPOdj(shPlay.stationTrack)))) then
      Result := Result + 'HLÁŠENÍ příjezd,'
    else if (shPlay.railway <> nil) then
      Result := Result + 'HLÁŠENÍ průjezd,';
  end;
end;

function TTrain.MenuRailwayPredicted(): string;
begin
  Result := 'INFO vlak,';
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrain.Log(msg: string; level: TLogLevel; source: TLogSource);
begin
  Logging.log('Souprava ' + Self.name + ': ' + msg, level, source);
end;

////////////////////////////////////////////////////////////////////////////////

function TTrain.StrArrowDirection(): string;
begin
  Result := '';
  if (Self.data.dir_L) then
    Result := 'L';
  if (Self.data.dir_S) then
    Result := Result + 'S';
  if (Result = '') then
    Result := '-';
end;

////////////////////////////////////////////////////////////////////////////////

function TTrain.InfoWindowItems(): TList<TConfSeqItem>;
begin
  Result := TList<TConfSeqItem>.Create();
  try
    Result.Add(CSItem('Souprava '+Self.name));
    Result.Add(CSItem('Typ: '+Self.data.typ));
    Result.Add(CSItem('Délka: '+IntToStr(Self.data.length)+' cm'));
    Result.Add(CSItem('Počet vozů: '+IntToStr(Self.data.carsCount)));
    Result.Add(CSItem('Směr: '+Self.StrArrowDirection()));

    if (Self.data.areaFrom <> nil) then
      Result.Add(CSItem('Výchozí stanice: '+TArea(Self.data.areaFrom).name))
    else
      Result.Add(CSItem('Výchozí stanice: nevyplněno'));

    if (Self.data.areaTo <> nil) then
      Result.Add(CSItem('Cílová stanice: '+TArea(Self.data.areaTo).name))
    else
      Result.Add(CSItem('Cílová stanice: nevyplněno'));

    if (Self.data.note <> '') then
      Result.Add(CSItem('Poznámka: '+ Self.data.note));

    for var i: Integer := 0 to Self.HVs.Count-1 do
    begin
      if (HVDb[Self.HVs[i]] <> nil) then
      begin
        var hv := HVDb[Self.HVs[i]];

        var comment: string := '';
        if (hv.ruc) then
          comment := 'Ruční řízení jízdy i funkcí'
        else if (hv.state.regulators.Count > 0) then
          comment := 'Ruční ovládání funkcí, jízdu řídí hJOP'
        else
          comment := 'Jízdu i funkce řídí hJOP';

        Result.Add(CSItem('HV '+IntToStr(i+1)+': '+ hv.NiceName() + '  # ' + comment));

        for var regulator in hv.state.regulators do
        begin
          var user := TPanelConnData(regulator.conn.Data).regulator_user;
          if (hv.ruc) then
            Result.Add(CSItem('  Ruční řízení jízdy - '+user.fullName))
          else
            Result.Add(CSItem('  Ovládání funkcí - '+user.fullName));
        end;
      end;
    end;

  except
    Result.Free();
    raise;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrain.CallChangeToTracks();
begin
 var tracks := Blocks.GetBlkWithTrain(Self);
 try
   for var track in tracks do
     track.Change();
 finally
   tracks.Free();
 end;
end;

function TTrain.RucBarriers(): TList<TUPOItem>;
begin
  Result := TList<TUPOItem>.Create();
  for var addr: Integer in Self.HVs do
    if ((Assigned(HVDb[addr])) and (HVDb[addr].ruc)) then
      Result.Add(JCBarriers.JCBarrierToMessage(JCBarrier(barHVManual, nil, addr)));
end;

procedure TTrain.RucUPO(AContext: TIdContext; ref: TObject = nil; callbackOk: TNotifyEvent = nil; callbackEsc: TNotifyEvent = nil);
begin
  var UPO := Self.RucBarriers();
  try
    PanelServer.UPO(AContext, UPO, false, callbackOk, callbackEsc, ref);
  finally
    UPO.Free();
  end;
end;

function TTrain.IsAnyHVRuc(): Boolean;
begin
  for var addr: Integer in Self.HVs do
    if ((Assigned(HVDb[addr])) and (HVDb[addr].ruc)) then
      Exit(true);
  Result := false;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrain.UpdateTraveled(msSinceLastUpdate: Cardinal);
begin
  if (Self.speed = 0) then
    Exit();

  Self._traveled := Self._traveled + (Self.speed * Integer(msSinceLastUpdate) / (3.6 * GlobalConfig.scale * 1000));

  if (Now > Self._nextTraveledChange) then
  begin
    Self.changed := true;
    Self._nextTraveledChange := Now + EncodeTime(0, 0, _TRAVELED_REFRESH_PERIOD_MS div 1000, _TRAVELED_REFRESH_PERIOD_MS mod 1000);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit
