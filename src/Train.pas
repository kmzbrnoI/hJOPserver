unit Train;

{ This file defines "TTrain" class which represents a single train. }

interface

uses IniFiles, SysUtils, Classes, Forms, IBUtils, THnaciVozidlo, JsonDataObjects,
     Generics.Collections, predvidanyOdjezd, TBlok, Trakce;

const
  _MAX_TRAIN_HV = 4;

type
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
    station: TObject;

    speed: Integer; // real speed passed to locomotives after all limitations
    wantedSpeed: Integer; // speed wanted by caller, before limitations
    maxSpeed: Cardinal; // max speed in km/h entered by user; maxSpeed = 0 <=> no limitation
    direction: THVStanoviste;

    front: TObject; // most forward block train is/was on (always instance of TBlkUsek)
    stationFrom: TObject; // instance of TOR or Nil
    stationTo: TObject; // instance of TOR or Nil

    announcement: Boolean;
    announcementPlayed: Boolean;

    podj: TDictionary<Integer, TPOdj>;  // map track id: podj
  end;

  TTrain = class
   private
    data: TTrainData;
    findex: Integer;
    filefront: Integer;
    fAcquiring: Boolean;
    speedBuffer: PInteger; // pokud tento ukazatel neni nil, rychlost je nastavovana do promenne, na kterou ukazuje
                           // a ne primo souprave; to se hodi napriklad v zastavce v TU

     procedure Init(index: Integer);
     procedure LoadFromFile(ini: TMemIniFile; const section: string);
     procedure LocoAcquiredOk(Sender: TObject; Data: Pointer);
     procedure LocoAcquiredErr(Sender: TObject; Data: Pointer);
     procedure AllLocoAcquiredOk(newLoks: TList<Integer>);

     procedure AcquireOk(Sender: TObject; Data: Pointer);
     procedure AcquireErr(Sender: TObject; Data: Pointer);

     procedure ReleaseAllLoko();

     procedure SetOR(station: TObject);

     procedure HVComErr(Sender: TObject; Data: Pointer);
     procedure SetSpeed(speed: Integer);
     procedure SetDirection(direction: THVStanoviste);
     procedure SetFront(front: TObject);

     function IsStolen(): Boolean;
     function GetMaxSpeed(): Cardinal;
     function GetMaxSpeedStep(): Cardinal;

     procedure UpdateTrainFromJson(train: TJsonObject; ok: TCb; err: TCb);
     class procedure PtHVsListToDict(train: TJsonObject);

   public

    changed: Boolean;

     constructor Create(ini: TMemIniFile; const section: string; index: Integer); overload;
     constructor Create(panelStr: TStrings; index: Integer; usek: TObject; oblr: TObject; ok: TCb; err: TCb); overload;
     constructor Create(train: TJsonObject; index: Integer; ok: TCb; err: TCb); overload;
     destructor Destroy(); override;

     procedure SaveToFile(ini: TMemIniFile; const section: string);

     function GetPanelString(): string;   // vraci string, kterym je definovana souprava, do panelu
     procedure UpdateTrainFromPanel(train: TStrings; usek: TObject; oblr: TObject; ok: TCb; err: TCb);
     procedure SetSpeedDirection(speed: Cardinal; dir: THVStanoviste);
     procedure Acquire(ok: TCb; err: TCb);
     procedure UpdateFront();
     procedure ChangeDirection();
     procedure InterChangeStanice(change_ev: Boolean = true);
     procedure SetSpeedBuffer(speedBuffer: PInteger);
     procedure LokDirChanged();
     procedure CheckSH(signal: TObject);

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

     function PredictedSignal(): TBlk;
     procedure OnPredictedSignalChange();
     procedure OnExpectedSpeedChange();

     procedure GetPtData(json: TJsonObject);
     procedure PutPtData(reqJson: TJsonObject; respJson: TJsonObject);

     property index: Integer read findex;
     property sdata: TTrainData read data;

     property name: string read data.name;
     property station: TObject read data.station write SetOR;
     property speed: Integer read data.speed write SetSpeed;
     property wantedSpeed: Integer read data.wantedSpeed;
     property direction: THVStanoviste read data.direction write SetDirection;
     property stolen: Boolean read IsStolen;
     property front: TObject read data.front write SetFront;
     property length: Integer read data.length;
     property typ: string read data.typ;

     property stationFrom: TObject read data.stationFrom;
     property stationTo: TObject read data.stationTo;

     property announcement: Boolean read data.announcement;
     property announcementPlayed: Boolean read data.announcementPlayed;

     property HVs: TTrainHVs read data.HVs;
     property maxSpeed: Cardinal read GetMaxSpeed; // warning: this could be speed with no speed step
     property maxSpeedStep: Cardinal read GetMaxSpeedStep;
     property acquiring: Boolean read fAcquiring;

     // uvolni stara hnaci vozidla ze soupravy (pri zmene HV na souprave)
     class procedure UvolV(old: TTrainHVs; new: TTrainHVs);

  end;

implementation

uses THVDatabase, Logging, ownStrUtils, TrainDb, TBlokUsek, DataSpr, appEv,
      DataHV, TOblsRizeni, TOblRizeni, TCPServerOR, TBloky, TBlokNav,
      fRegulator, fMain, TBlokTratUsek, stanicniHlaseniHelper, stanicniHlaseni,
      TechnologieTrakce, ownConvert, TJCDatabase, TechnologieJC, IfThenElse;

////////////////////////////////////////////////////////////////////////////////

constructor TTrain.Create(ini: TMemIniFile; const section: string; index: Integer);
begin
 inherited Create();
 Self.Init(index);
 Self.LoadFromFile(ini, section);
end;

constructor TTrain.Create(panelStr: TStrings; index: Integer; usek: TObject; oblr: TObject; ok: TCb; err: TCb);
begin
 inherited Create();
 Self.Init(index);
 Self.UpdateTrainFromPanel(panelStr, usek, oblr, ok, err);
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
 Self.speedBuffer := nil;
 Self.changed := false;
 Self.findex := index;
 Self.data.podj := TDictionary<Integer, TPOdj>.Create();
 Self.data.HVs := TList<Integer>.Create();
 Self.data.announcementPlayed := false;
 Self.fAcquiring := false;
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
var addr: Integer;
    data: TStrings;
    s: string;
begin
 Self.data.name := ini.ReadString(section, 'nazev', section);
 Self.data.carsCount := ini.ReadInteger(section, 'vozu', 0);
 Self.data.note := ini.ReadString(section, 'note', '');
 Self.data.dir_L := ini.ReadBool(section, 'L', false);
 Self.data.dir_S := ini.ReadBool(section, 'S', false);
 Self.data.length := ini.ReadInteger(section, 'delka', 0);
 Self.data.typ := ini.ReadString(section, 'typ', '');
 Self.filefront := ini.ReadInteger(section, 'front', -1);
 Self.data.direction := THVStanoviste(ini.ReadInteger(section, 'smer', Integer(THVStanoviste.lichy)));
 Self.data.maxSpeed := ini.ReadInteger(section, 'maxRychlost', 0);

 Self.data.stationFrom := ORs.Get(ini.ReadString(section, 'z', ''));
 Self.data.stationTo := ORs.Get(ini.ReadString(section, 'do', ''));
 Self.data.station := ORs.Get(ini.ReadString(section, 'OR', ''));
 Self.data.announcement := ini.ReadBool(section, 'hlaseni', false);

 data := TStringList.Create();
 ExtractStrings([';', ','], [], PChar(ini.ReadString(section, 'HV', '')), data);

 while (data.Count > _MAX_TRAIN_HV) do
   data.Delete(_MAX_TRAIN_HV);

 // HV se nacitaji takto prapodivne pro osetreni pripadu, kdy u soupravy je uvedene HV, ktere neexistuje
 Self.data.HVs.Clear();
 try
   for s in data do
    begin
     addr := StrToInt(s);
     if (Assigned(HVDb[addr])) then
      begin
       HVDb[addr].train := Self.index;
       Self.data.HVs.Add(addr);
      end;
    end;
 except

 end;

 data.Free();
 Self.changed := true;
end;

procedure TTrain.SaveToFile(ini: TMemIniFile; const section: string);
var str: string;
    addr: Integer;
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

 if (Self.data.stationFrom <> nil) then
   ini.WriteString(section, 'z', TOR(Self.data.stationFrom).id)
 else
   ini.DeleteKey(section, 'z');

 if (Self.data.stationTo <> nil) then
   ini.WriteString(section, 'do', TOR(Self.data.stationTo).id)
 else
   ini.DeleteKey(section, 'do');

 if (Self.data.front <> nil) then
   ini.WriteInteger(section, 'front', (Self.data.front as TBlk).id)
 else
   ini.WriteInteger(section, 'front', -1);

 if (Self.data.station <> nil) then
   ini.WriteString(section, 'OR', (Self.data.station as TOR).id)
 else
   ini.DeleteKey(section, 'OR');

 ini.WriteBool(section, 'hlaseni', Self.data.announcement);

 str := '';
 for addr in Self.HVs do
  str := str + IntToStr(addr) + ';';
 ini.WriteString(section, 'HV', str);
end;

////////////////////////////////////////////////////////////////////////////////

// vraci string, kterym je definovana souprava, do panelu
function TTrain.GetPanelString(): string;
var addr: Integer;
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

 for addr in Self.HVs do
   Result := Result + '[{' + HVDb[addr].GetPanelLokString() + '}]';
 Result := Result + '};';

 if (Self.stationFrom <> nil) then
   Result := Result + TOR(Self.stationFrom).id;
 Result := Result + ';';

 if (Self.stationTo <> nil) then
   Result := Result + TOR(Self.stationTo).id;
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

procedure TTrain.UpdateTrainFromPanel(train: TStrings; Usek: TObject; OblR: TObject; ok: TCb; err: TCb);
var json: TJsonObject;
    hvs, hv: TStrings;
    s, straddr: string;
    hvobj: TJsonObject;
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
   json['station'] := TOR(OblR).id;
   json['front'] := TBlk(usek).id;

   if (train.Count > 7) then
     json['stationFrom'] := train[7];
   if (train.Count > 8) then
     json['stationTo'] := train[8];

   if (train.Count > 9) then
     json['announcement'] := (train[9] = '1')
   else
     json['announcement'] := TStanicniHlaseni.HlasitTrainTyp(json.S['type']);

   if ((train.Count > 10) and (train[10] <> '')) then
     json['maxSpeed'] := StrToInt(train[10])
   else
     json['maxSpeed'] := 0;

   hvs := TStringList.Create();
   hv := TStringList.Create();
   try
     ExtractStringsEx([']'], ['['], train[6], hvs);

     for s in hvs do
      begin
       hv.Clear();
       ExtractStringsEx(['|'], [], s, hv);
       straddr := hv[4];
       hvobj := json['hvs'].O[straddr];
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
var addrhv: TJsonNameValuePair;
    hv: TJsonObject;
    i, addr, max_func: Integer;
    new: TTrainHVs;
    acq: ^TTrainAcquire;
begin
 if (Self.acquiring) then
   raise Exception.Create('Přebírání lokomotiv soupravy již probíhá!');

 // zkontrolujeme, jestli nejaka souprava s timto cislem uz nahodou neexistuje
 for i := 0 to _MAX_TRAIN-1 do
  begin
   if (Trains[i] = nil) then continue;

   if ((Trains[i].name = train['name']) and (Trains[i] <> Self)) then
    begin
     if (Trains[i].station <> nil) then
       raise Exception.Create('Souprava '+Trains[i].name+' již existuje v OŘ '+(Trains[i].station as TOR).Name);
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
   Self.data.station := ORs.Get(train.S['station']);
 if (train.Contains('front')) then
   Blky.GetBlkByID(train['front'], TBlk(Self.data.front));

 if (train.Contains('stationFrom')) then
   Self.data.stationFrom := ORs.Get(train.S['stationFrom']);
 if (train.Contains('stationTo')) then
   Self.data.stationTo := ORs.Get(train.S['stationTo']);

 if (train.Contains('announcement')) then
   Self.data.announcement := train['announcement'];

 if (train.Contains('maxSpeed')) then
   Self.data.maxSpeed := StrToInt(train['maxSpeed']);

 if ((Self.wantedSpeed = 0) and (Self.data.dir_L xor Self.data.dir_S)) then
  begin
   // vypocet smeru ze sipky
   if (Self.data.dir_L) then
     Self.data.direction := THVStanoviste.lichy
   else
     Self.data.direction := THVStanoviste.sudy;

   for addr in Self.HVs do
     HVDb[addr].OnPredictedSignalChange();
  end;

 if (train.O['hvs'].Count > _MAX_TRAIN_HV) then
   raise Exception.Create('Překročen maximální počet hnacích vozidel na soupravě');

 new := TList<Integer>.Create();
 try
   for addrhv in train.O['hvs'] do
    begin
     hv := addrhv.Value;
     addr := StrToInt(addrhv.Name);

     if (not Assigned(HVDb[addr])) then
       raise Exception.Create('Loko '+IntToStr(addr)+' neexistuje na serveru!');

     if ((HVDb[addr].train > -1) and (HVDb[addr].train <> Self.index)) then
       raise Exception.Create('Loko '+IntToStr(addr)+' již přiřazena soupravě '+Trains.GetTrainNameByIndex(HVDb[addr].train));

     if (new.Contains(addr)) then
       raise Exception.Create('Duplicitní loko!');

     if (hv.Contains('note')) then
       HVDb[addr].Data.note := hv['note'];
     if (hv.Contains('sta')) then
       HVDb[addr].Stav.StanovisteA := THVStanoviste(hv.I['sta']);

     HVDb[addr].train := Self.index;

     if (hv.Contains('func')) then
      begin
       max_func := Min(System.Length(hv.S['func']), _HV_FUNC_MAX);
       for i := 0 to max_func do
         HVDb[addr].Stav.funkce[i] := (hv.S['func'][i+1] = '1');
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
   HVDb[addr].StavFunctionsToSlotFunctions(TTrakce.Callback(Self.LocoAcquiredOk, acq),
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
var signal: TBlk;
begin
 Self.fAcquiring := false;
 Self.UvolV(Self.HVs, newLoks);
 Self.data.HVs.Free();
 Self.data.HVs := newLoks;

 Self.SetSpeedDirection(Self.speed, Self.direction);
 Blky.ChangeTrainToTrat(Self);

 TBlkUsek(Self.front).Change();

 for signal in TBlkUsek(Self.front).signalJCRef do
   TBlkSignal(signal).UpdateRychlostTrain(true);

 Self.changed := true;
end;

////////////////////////////////////////////////////////////////////////////////

class procedure TTrain.UvolV(old: TTrainHVs; new: TTrainHVs);
var new_addr, old_addr: Integer;
    keep: TList<Integer>;
begin
 keep := TList<Integer>.Create();

 try
   for new_addr in new do
     for old_addr in old do
       if (new_addr = old_addr) then
          keep.Add(new_addr);

   for old_addr in old do
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
var addr: Integer;
begin
 if ((not Assigned(HVDb)) or (not Assigned(TrakceI))) then Exit();

 for addr in Self.HVs do
  begin
   if (not Assigned(HVDb[addr])) then
     continue;

   HVDb[addr].train := -1;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrain.SetOR(station: TObject);
var addr: Integer;
begin
 Self.data.station := station;
 for addr in Self.HVs do
   HVDb[addr].PredejStanici(station as TOR);
 Self.Data.announcementPlayed := false;
 Self.changed := true;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrain.SetSpeedDirection(speed: Cardinal; dir: THVStanoviste);
var addr: Integer;
    direction: Boolean;
    dir_changed: Boolean;
begin
 if ((TBlk(Self.front).typ = btTU) and (TBlkTU(Self.front).rychUpdate)) then
   TBlkTU(Self.front).rychUpdate := false;

 dir_changed := (Self.direction <> dir);
 Self.data.direction := dir;
 if (Self.speedBuffer = nil) then
  begin
   Self.data.wantedSpeed := speed;
   if (speed > Self.maxSpeed) then
     Self.data.speed := Self.maxSpeed
   else
     Self.data.speed := speed;
  end else begin
   Self.speedBuffer^ := speed;
   Exit();
  end;

 if ((TBlk(Self.front).typ = btTU) and (TBlkTU(Self.front).Trat <> nil)) then
   TBlkTU(Self.front).Trat.Change();

 for addr in Self.HVs do
  begin
   HVDb[addr].OnExpectedSpeedChange();
   if (dir_changed) then
     HVDb[addr].OnPredictedSignalChange();

   if (HVDb[addr].ruc) then
    begin
     writelog('LOKO ' + IntToStr(addr) + ' v ručním regulátoru, nenastavuji rychlost', WR_MESSAGE);
     continue;
    end;
   if (HVDb[addr].stolen) then
    begin
     writelog('LOKO ' + IntToStr(addr) + ' ukradena, nenastavuji rychlost', WR_MESSAGE);
     continue;
    end;

   direction := ownConvert.IntToBool(Integer(dir) xor Integer(HVDb[addr].stav.StanovisteA));

   try
     HVDb[addr].SetSpeedDir(Self.data.speed, direction,
                            TTrakce.Callback(), TTrakce.Callback(Self.HVComErr), Self);
   except
     on E: Exception do
       AppEvents.LogException(E, 'TTrain.SetSpeedDirection');
   end;
  end;

 if ((speed > 0) and (Assigned(Self.front)) and
     ((Self.front as TBlkUsek).IsVlakPresun()) and
      ((Self.front as TBlkUsek).trains[(Self.front as TBlkUsek).vlakPresun] = Self.index)) then
  (Self.front as TBlkUsek).VlakPresun := -1;

 writelog('Souprava ' + Self.name + ' : rychlost '+IntToStr(speed)+', směr : '+IntToStr(Integer(dir)), WR_MESSAGE);
 Self.changed := true;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrain.SetSpeed(speed: Integer);
begin
 Self.SetSpeedDirection(speed, Self.data.direction);
end;

procedure TTrain.SetDirection(direction: THVStanoviste);
begin
 Self.SetSpeedDirection(Self.data.speed, direction);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrain.HVComErr(Sender: TObject; Data: Pointer);
begin
 if (Self.data.station <> nil) then
   (Self.data.station as TOR).BlkWriteError(nil, 'Souprava '+Self.name+' nekomunikuje s centrálou', 'CENTRÁLA');
end;

////////////////////////////////////////////////////////////////////////////////

function TTrain.IsStolen(): Boolean;
var addr: Integer;
begin
 for addr in Self.HVs do
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
var addr: Integer;
begin
 if (Self.data.front = front) then Exit();

 if (Assigned(Self.data.front)) then
   (Self.data.front as TBlkUsek).zpomalovani_ready := false;
 Self.data.front := front;

 for addr in Self.HVs do
   HVDb[addr].OnPredictedSignalChange();

 Self.changed := true;
end;

procedure TTrain.UpdateFront();
var blk: TBlk;
begin
 Blky.GetBlkByID(Self.filefront, blk);
 Self.front := blk;
end;

////////////////////////////////////////////////////////////////////////////////

// zmena smeru pri naslapu na smyckovy blok
procedure TTrain.ChangeDirection();
var addr: Integer;
    tmp: Boolean;
begin
 writelog('Souprava '+ Self.name + ' : změna směru', WR_SPRPREDAT);

 // zmenit orintaci stanoviste A hnacich vozidel
 for addr in Self.HVs do
  begin
   case (HVDb[addr].Stav.StanovisteA) of
    THVStanoviste.lichy : HVDb[addr].Stav.StanovisteA := THVStanoviste.sudy;
    THVStanoviste.sudy  : HVDb[addr].Stav.StanovisteA := THVStanoviste.lichy;
   end;//case
  end;//for i

 // zmenit orientaci sipky soupravy
 tmp := Self.data.dir_L;
 Self.data.dir_L := Self.data.dir_S;
 Self.data.dir_S := tmp;

 // zmenit smer suupravy - dulezite pro zastaveni pred navestidlem
 case (Self.data.direction) of
  THVStanoviste.lichy : Self.direction := THVStanoviste.sudy;
  THVStanoviste.sudy  : Self.direction := THVStanoviste.lichy;
 end;//case

 if (Self.front <> nil) then
   (Self.front as TBlkUsek).Change();  // kvuli sipce
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrain.InterChangeStanice(change_ev: Boolean = true);
var tmp: TObject;
begin
 tmp := Self.data.stationFrom;
 Self.data.stationFrom := Self.data.stationTo;
 Self.data.stationTo := tmp;

 Self.changed := true;
 if ((Self.front <> nil) and (change_ev)) then
   (Self.front as TBlkUsek).Change();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrain.SetSpeedBuffer(speedBuffer: PInteger);
begin
 Self.speedBuffer := speedBuffer;
end;

////////////////////////////////////////////////////////////////////////////////
// V pripade, ze vsechna hnaci vozidla soupravy otocim do opacneho smeru,
// nez je smer soupravy, otoci se i smer soupravy. To umoznuje otoceni smeru
// soupravy z Rocomaus.
// Tato zmena je umoznena jen tehdy pokud nema sipka jednoznacne urceny smer
// a pokud souprava stoji.

procedure TTrain.LokDirChanged();
var i: Integer;
    dir: Boolean;
begin
 if ((Self.wantedSpeed <> 0) or (Self.data.dir_L xor Self.data.dir_S) or
     (Self.HVs.Count = 0) or ((Self.front <> nil) and (not TBlkUsek(Self.front).Stav.stanicni_kolej))) then
   Exit();

 dir := HVDb[Self.HVs[0]].stACurrentDirection;

 if (dir = ownConvert.IntToBool(Integer(Self.direction))) then Exit();
 for i := 1 to Self.HVs.Count-1 do
   if (dir <> HVDb[Self.HVs[i]].stACurrentDirection) then
     Exit();

 // vsechna hv nastavena do opacneho smeru -> zmenit smer soupravy
 Self.direction := THVStanoviste(dir);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrain.ToggleHouk(desc: string);
var addr: Integer;
    HV: THV;
begin
 writelog('Souprava ' + Self.name + ' : aktivuji houkání ' + desc, WR_MESSAGE);

 for addr in Self.HVs do
  begin
   HV := HVDb[addr];
   if (HV.CanPlayHouk(desc)) then
     TrakceI.LokFuncToggle(Self, HV, HV.funcDict[desc]);
  end;
end;

procedure TTrain.SetHoukState(desc: string; state: Boolean);
var addr: Integer;
    HV: THV;
begin
 if (state) then
   writelog('Souprava ' + Self.name + ' : aktivuji funkci ' + desc, WR_MESSAGE)
 else
   writelog('Souprava ' + Self.name + ' : deaktivuji funkci ' + desc, WR_MESSAGE);

 for addr in Self.HVs do
  begin
   HV := HVDb[addr];
   if (HV.CanPlayHouk(desc)) then
     HV.SetSingleFunc(HV.funcDict[desc], state, TTrakce.Callback(), TTrakce.Callback(), Self);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrain.CheckSH(signal: TObject);
var msignal: TBlkSignal;
    oblr: TOR;
    shPlay: TSHToPlay;
    shTrain: TSHTrain;
begin
 if ((not Self.announcement) or (Self.announcementPlayed) or (self.stationFrom = nil) or
     (self.stationTo = nil) or (Self.typ = '')) then Exit();

 msignal := TBlkSignal(signal);
 if (msignal.OblsRizeni.Count < 1) then Exit();
 oblr := msignal.OblsRizeni[0];

 if ((not Assigned(oblr.hlaseni)) or (not oblr.hlaseni.available)) then Exit();

 try
   shPlay := stanicniHlaseniHelper.CanPlayPrijezdSH(self, oblr);
 except
   on E: Exception do
     AppEvents.LogException(E, 'CanPlayPrijezdSH');
 end;

 shTrain.cislo := Self.name;
 shTrain.typ   := Self.typ;
 shTrain.fromORid := TOR(Self.stationFrom).id;
 shTrain.toORid := TOR(Self.stationTo).id;
 shTrain.timeArrive := 0;
 shTrain.timeDepart := 0;

 if (shPlay.stanicniKolej <> nil) then
  begin
   shTrain.kolej := shPlay.stanicniKolej.Stav.cislo_koleje;

   if ((Self.IsPOdj(shPlay.stanicniKolej)) and (Self.GetPOdj(shPlay.stanicniKolej).abs_enabled)) then
     shTrain.timeDepart := Self.GetPOdj(shPlay.stanicniKolej).abs;
  end;

 try
   if ((shPlay.stanicniKolej <> nil) and ((shPlay.trat = nil) or (Self.IsPOdj(shPlay.stanicniKolej)))) then begin
     oblr.hlaseni.Prijede(shTrain);
     Self.data.announcementPlayed := true;
   end else if (shPlay.trat <> nil) then begin
     oblr.hlaseni.Projede(shTrain);
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
var hvaddr: Integer;
begin
 for hvaddr in Self.HVs do
   if (HVDb[hvaddr].Stav.regulators.Count > 0) then
     Exit(true);
 Result := false;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrain.ForceRemoveAllRegulators();
var hvaddr: Integer;
begin
 for hvaddr in Self.HVs do
   if (HVDb[hvaddr].Stav.regulators.Count > 0) then
     HVDb[hvaddr].ForceRemoveAllRegulators();
end;

////////////////////////////////////////////////////////////////////////////////

function TTrain.GetMaxSpeed(): Cardinal;
var addr: Integer;
    minimum: Cardinal;
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

   for addr in Self.HVs do
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
var frontblk: TBlkUsek;
    signal: TBlkSignal;
    jc: TJC;
    tu: TBlkTU;
begin
 frontblk := Self.front as TBlkUsek;
 if (frontblk = nil) then
   Exit(nil);

 if (frontblk.typ = btTU) then
   Exit(TBlkTU(frontblk).nextNav);

 case (Self.direction) of
   THVStanoviste.lichy: signal := frontblk.navL as TBlkSignal;
   THVStanoviste.sudy: signal := frontblk.navS as TBlkSignal;
 end;

 if ((signal <> nil) and (signal.SymbolType = TBlkSignalSymbol.main)) then
   Exit(signal);

 jc := JCDb.FindPostavenaJCWithUsek(frontblk.id);
 if (jc = nil) then
   Exit(nil);

 case (jc.data.DalsiNavaznost) of
  TJCNextNavType.zadna: Exit(nil);
  TJCNextNavType.trat: begin
    Blky.GetBlkByID(jc.data.Useky[jc.data.Useky.Count-1], TBlk(frontblk));
    if (frontblk <> nil) and (frontblk.typ = btTU) then
     begin
      tu := TBlkTU(frontblk);
      Exit(tu.nextNav);
     end else
      Exit(nil);
  end;
  TJCNextNavType.blok: begin
    Blky.GetBlkByID(jc.data.DalsiNavestidlo, TBlk(signal));
    Exit(signal);
  end;
 end;

 Result := nil;
end;

procedure TTrain.OnPredictedSignalChange();
var addr: Integer;
begin
 for addr in Self.HVs do
   HVDb[addr].OnPredictedSignalChange();
end;

procedure TTrain.OnExpectedSpeedChange();
var addr: Integer;
begin
 for addr in Self.HVs do
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

 if (Self.data.station <> nil) then
   json['station'] := TOR(Self.data.station).id;
 json['speed'] := Self.data.speed;
 json['wantedSpeed'] := Self.data.wantedSpeed;
 if (Self.data.maxSpeed > 0) then
   json['maxSpeed'] := Self.data.maxSpeed;
 json['direction'] := Integer(Self.data.direction);
 if (Self.data.front <> nil) then
   json['front'] := TBlk(Self.data.front).id;
 if (Self.data.stationFrom <> nil) then
   json['stationFrom'] := TOR(Self.data.stationFrom).id;
 if (Self.data.stationTo <> nil) then
   json['stationTo'] := TOR(Self.data.stationTo).id;
 json['announcement'] := Self.data.announcement;

 for blkId in Self.data.podj.Keys do
   Self.data.podj[blkId].GetPtData(json.O['podj'].O[IntToStr(blkId)]);
end;

procedure TTrain.PutPtData(reqJson: TJsonObject; respJson: TJsonObject);
var hvaddr: Integer;
begin
 if (not reqJson.Contains('name')) then
   reqJson['name'] := Self.name;
 if (not reqJson.Contains('hvs')) then
   for hvaddr in Self.HVs do
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

end.//unit
