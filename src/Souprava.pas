unit Souprava;

// trida "TSouprava"

interface

uses IniFiles, SysUtils, Classes, Forms, IBUtils, THnaciVozidlo, JsonDataObjects,
     Generics.Collections, predvidanyOdjezd, TBlok, Trakce;

const
  _MAX_SPR_HV = 4;

type
  TSoupravaHVs = TList<Integer>; // seznam adres hnacich vozidel na souprave

  TSoupravaAcquire = record
    ok, err: TCb;
    toAcquire: TList<Integer>;
    nextAcquire: Integer;
  end;

  TVezmi = record
    ok, err: TCb;
    nextVezmi: Integer;
  end;

  TSoupravaData = record
    name: string;
    carsCount: Cardinal;
    note: string;
    length: Integer; // length of a train in centimeters
    typ: string; // MOs, Os, Mn, Pn, ...
    dir_L, dir_S: boolean; // allowed directions
    HVs: TSoupravaHVs; // locomotives (engines)
    station: TObject;

    speed: Integer; // real speed passed to locomotives after all limitations
    wantedSpeed: Integer; // speed wanted by caller, before limitations
    maxSpeed: Cardinal; // max speed in km/h entered by user; maxSpeed = 0 <=> no limitation
    direction: THVStanoviste;

    front: TObject; // most forward block train is/was on (always instance of TBlkUsek)
    stationFrom: TObject; // instance of TOR or Nil
    stationTo: TObject; // instance of TOR or Nil

    announcement: Boolean;
    announcementPlayed: boolean;

    podj: TDictionary<Integer, TPOdj>;  // map track id: podj
  end;

  TSouprava = class
   private
    data: TSoupravaData;
    findex: Integer;
    filefront: Integer;
    fAcquiring: Boolean;
    speedBuffer: PInteger; // pokud tento ukazatel neni nil, rychlost je nastavovana do promenne, na kterou ukazuje
                           // a ne primo souprave; to se hodi napriklad v zastavce v TU

     procedure Init(index:Integer);
     procedure LoadFromFile(ini:TMemIniFile; const section:string);
     procedure LocoAcquiredOk(Sender: TObject; Data: Pointer);
     procedure LocoAcquiredErr(Sender: TObject; Data: Pointer);
     procedure AllLocoAcquiredOk(newLoks: TList<Integer>);

     procedure VezmiVlakOk(Sender: TObject; Data: Pointer);
     procedure VezmiVlakErr(Sender: TObject; Data: Pointer);

     procedure ReleaseAllLoko();

     procedure SetOR(station: TObject);

     procedure HVComErr(Sender:TObject; Data:Pointer);
     procedure SetSpeed(speed:Integer);
     procedure SetSmer(smer:THVStanoviste);
     procedure SetFront(front:TObject);

     function IsStolen():boolean;
     function GetMaxSpeed():Cardinal;
     function GetMaxSpeedStep():Cardinal;

   public

    changed: Boolean;

     constructor Create(ini:TMemIniFile; const section:string; index:Integer); overload;
     constructor Create(panelStr:TStrings; Usek:TObject; index:Integer; OblR:TObject; ok: TCb; err: TCb); overload;
     destructor Destroy(); override;

     procedure SaveToFile(ini:TMemIniFile; const section:string);

     function GetPanelString():string;   // vraci string, kterym je definovana souprava, do panelu
     procedure UpdateSprFromPanel(spr:TStrings; Usek:TObject; OblR:TObject; ok:TCb; err:TCb);
     procedure SetRychlostSmer(speed:Cardinal; dir:THVStanoviste);
     procedure VezmiVlak(ok: TCb; err: TCb);
     procedure UpdateFront();
     procedure ChangeSmer();
     procedure InterChangeStanice(change_ev:Boolean = true);
     procedure SetSpeedBuffer(speedBuffer:PInteger);
     procedure LokDirChanged();
     procedure CheckSH(nav:TObject);

     procedure ToggleHouk(desc:string);
     procedure SetHoukState(desc:string; state:boolean);

     procedure AddOrUpdatePOdj(usekid:Integer; var podj:TPOdj); overload;
     procedure AddOrUpdatePOdj(usek:TBlk; var podj:TPOdj); overload;
     function IsPOdj(usekid:Integer):Boolean; overload;
     function IsPOdj(usek:TBlk):Boolean; overload;
     function GetPOdj(usekid:Integer):TPOdj; overload;
     function GetPOdj(usek:TBlk):TPOdj; overload;
     procedure RemovePOdj(usekid:Integer); overload;
     procedure RemovePOdj(usek:TBlk); overload;
     procedure ClearPOdj();
     function IsAnyLokoInRegulator():Boolean;
     procedure ForceRemoveAllRegulators();

     function PredictedSignal(): TBlk;
     procedure OnPredictedSignalChange();
     procedure OnExpectedSpeedChange();

     procedure GetPtData(json: TJsonObject);

     property index: Integer read findex;
     property sdata: TSoupravaData read data;

     property name: string read data.name;
     property station: TObject read data.station write SetOR;
     property speed: Integer read data.speed write SetSpeed;
     property wantedSpeed: Integer read data.wantedSpeed;
     property direction: THVStanoviste read data.direction write SetSmer;
     property stolen: boolean read IsStolen;
     property front: TObject read data.front write SetFront;
     property sprLength: Integer read data.length;
     property typ: string read data.typ;

     property stationFrom: TObject read data.stationFrom;
     property stationTo: TObject read data.stationTo;

     property announcement: boolean read data.announcement;
     property announcementPlayed: boolean read data.announcementPlayed;

     property HVs: TSoupravaHVs read data.HVs;
     property maxSpeed: Cardinal read GetMaxSpeed; // warning: this could be speed with no speed step
     property maxSpeedStep: Cardinal read GetMaxSpeedStep;
     property acquiring: Boolean read fAcquiring;

     // uvolni stara hnaci vozidla ze soupravy (pri zmene HV na souprave)
     class procedure UvolV(old:TSoupravaHVs; new:TSoupravaHVs);

  end;

implementation

uses THVDatabase, Logging, ownStrUtils, SprDb, TBlokUsek, DataSpr, appEv,
      DataHV, TOblsRizeni, TOblRizeni, TCPServerOR, TBloky, TBlokNav,
      fRegulator, fMain, TBlokTratUsek, stanicniHlaseniHelper, stanicniHlaseni,
      TechnologieTrakce, ownConvert, TJCDatabase, TechnologieJC;

////////////////////////////////////////////////////////////////////////////////

constructor TSouprava.Create(ini:TMemIniFile; const section:string; index:Integer);
begin
 inherited Create();
 Self.Init(index);
 Self.LoadFromFile(ini, section);
end;

constructor TSouprava.Create(panelStr:TStrings; Usek:TObject; index:Integer; OblR:TObject; ok: TCb; err: TCb);
begin
 inherited Create();
 Self.Init(index);
 Self.UpdateSprFromPanel(panelStr, Usek, OblR, ok, err);
end;

procedure TSouprava.Init(index:Integer);
begin
 Self.speedBuffer := nil;
 Self.changed := false;
 Self.findex := index;
 Self.data.podj := TDictionary<Integer, TPOdj>.Create();
 Self.data.HVs := TList<Integer>.Create();
 Self.data.announcementPlayed := false;
 Self.fAcquiring := false;
end;

destructor TSouprava.Destroy();
begin
 Self.ReleaseAllLoko();
 Self.ClearPOdj();
 Self.data.podj.Free();
 Self.data.HVs.Free();

 inherited;
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure TSouprava.LoadFromFile(ini:TMemIniFile; const section:string);
var addr:Integer;
    data:TStrings;
    s:string;
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

 while (data.Count > _MAX_SPR_HV) do
   data.Delete(_MAX_SPR_HV);

 // HV se nacitaji takto prapodivne pro osetreni pripadu, kdy u soupravy je uvedene HV, ktere neexistuje
 Self.data.HVs.Clear();
 try
   for s in data do
    begin
     addr := StrToInt(s);
     if (Assigned(HVDb[addr])) then
      begin
       HVDb[addr].souprava := Self.index;
       Self.data.HVs.Add(addr);
      end;
    end;
 except

 end;

 data.Free();
 Self.changed := true;
end;

procedure TSouprava.SaveToFile(ini:TMemIniFile; const section:string);
var str:string;
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
function TSouprava.GetPanelString():string;
var addr:Integer;
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

procedure TSouprava.UpdateSprFromPanel(spr:TStrings; Usek:TObject; OblR:TObject; ok:TCb; err:TCb);
var hvs,hv:TStrings;
    i, j, addr:Integer;
    new:TSoupravaHVs;
    max_func:Integer;
    s:string;
    acq: ^TSoupravaAcquire;
begin
 if (Self.acquiring) then
   raise Exception.Create('Přebírání lokomotiv soupravy již probíhá!');

 // zkontrolujeme, jestli nejaka souprava s timto cislem uz nahodou neexistuje
 for i := 0 to _MAX_SPR-1 do
  begin
   if (soupravy[i] = nil) then continue;

   if ((Soupravy[i].name = spr[0]) and (Soupravy[i] <> Self)) then
    begin
     if (Soupravy[i].station <> nil) then
       raise Exception.Create('Souprava '+Soupravy[i].name+' již existuje v OŘ '+(Soupravy[i].station as TOR).Name)
     else
       raise Exception.Create('Souprava '+Soupravy[i].name+' již existuje');

     Exit();
    end;
  end;

 try
  StrToInt(spr[0]);
 except
   on E:EConvertError do
     raise Exception.Create('Číslo soupravy není validní číslo!');
 end;

 Self.changed := true;

 Self.data.name := spr[0];
 Self.data.carsCount := StrToInt(spr[1]);
 Self.data.note := spr[2];
 Self.data.dir_L := (spr[3][1] = '1');
 Self.data.dir_S := (spr[3][2] = '1');

 Self.data.length := StrToInt(spr[4]);
 Self.data.typ := spr[5];

 Self.data.station := OblR;
 Self.data.front := Usek;

 if (spr.Count > 7) then
   Self.data.stationFrom := ORs.Get(spr[7]);

 if (spr.Count > 8) then
   Self.data.stationTo := ORs.Get(spr[8]);

 if (spr.Count > 9) then
   Self.data.announcement := (spr[9] = '1')
 else
   Self.data.announcement := TStanicniHlaseni.HlasitSprTyp(Self.typ);

 if ((spr.Count > 10) and (spr[10] <> '')) then
   Self.data.maxSpeed := StrToInt(spr[10])
 else
   Self.data.maxSpeed := 0;

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

 hvs := TStringList.Create();
 hv := TStringList.Create();
 new := TList<Integer>.Create();
 try
   ExtractStringsEx([']'], ['['], spr[6], hvs);

   if (hvs.Count > _MAX_SPR_HV) then
     raise Exception.Create('Překročen maximální počet hnacích vozidel na soupravě');

   for s in hvs do
    begin
     hv.Clear();
     ExtractStringsEx(['|'], [], s, hv);
     addr := StrToInt(hv[4]);

     if (not Assigned(HVDb[addr])) then
       raise Exception.Create('Loko '+IntToStr(addr)+' neexistuje na serveru!');

     if ((HVDb[addr].souprava > -1) and (HVDb[addr].souprava <> Self.index)) then
       raise Exception.Create('Loko '+IntToStr(addr)+' již přiřazena soupravě '+Soupravy.GetSprNameByIndex(HVDb[addr].souprava));

     if (new.Contains(addr)) then
       raise Exception.Create('Duplicitní loko!');

     HVDb[addr].Data.poznamka := hv[3];
     HVDb[addr].Stav.StanovisteA := THVStanoviste(StrToInt(hv[7]));
     HVDb[addr].souprava := Self.index;

     max_func := Min(Length(hv[8]), _HV_FUNC_MAX);
     for j := 0 to max_func do
       HVDb[addr].Stav.funkce[j] := (hv[8][j+1] = '1');

     new.Add(addr);
    end;

   GetMem(acq, sizeof(TSoupravaAcquire));
   acq^.ok := ok;
   acq^.err := err;
   acq^.toAcquire := new;
   acq^.nextAcquire := 0;

   Self.fAcquiring := true;
   Self.LocoAcquiredOk(Self, acq);
 except
   new.Free();
 end;

 hvs.Free();
 hv.Free();
end;

procedure TSouprava.LocoAcquiredOk(Sender: TObject; Data: Pointer);
var acq: ^TSoupravaAcquire;
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

procedure TSouprava.LocoAcquiredErr(Sender: TObject; Data: Pointer);
var acq: ^TSoupravaAcquire;
begin
 acq := Data;
 Self.fAcquiring := false;
 if (Assigned(acq^.err.callback)) then
   acq^.err.callback(Self, acq^.err.data);
 acq^.toAcquire.Free();
 FreeMem(acq);
 Self.changed := true;
end;

procedure TSouprava.AllLocoAcquiredOk(newLoks: TList<Integer>);
var nav:TBlk;
begin
 Self.fAcquiring := false;
 Self.UvolV(Self.HVs, newLoks);
 Self.data.HVs.Free();
 Self.data.HVs := newLoks;

 Self.SetRychlostSmer(Self.speed, Self.direction);
 Blky.ChangeSprToTrat(Self);

 TBlkUsek(Self.front).Change();

 for nav in TBlkUsek(Self.front).NavJCRef do
   TBlkNav(nav).UpdateRychlostSpr(true);

 Self.changed := true;
end;

////////////////////////////////////////////////////////////////////////////////

class procedure TSouprava.UvolV(old:TSoupravaHVs; new:TSoupravaHVs);
var new_addr, old_addr:Integer;
    keep:TList<Integer>;
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
       HVDb[old_addr].souprava := -1;
      end;
    end;
 finally
   keep.Free();
 end;
end;

////////////////////////////////////////////////////////////////////////////////

// uvolnit vsechna loko
// pred uvolnenim loko take zastavime
procedure TSouprava.ReleaseAllLoko();
var addr:Integer;
begin
 if ((not Assigned(HVDb)) or (not Assigned(TrakceI))) then Exit();

 for addr in Self.HVs do
  begin
   if (not Assigned(HVDb[addr])) then
     continue;

   HVDb[addr].souprava := -1;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TSouprava.SetOR(station: TObject);
var addr:Integer;
begin
 Self.data.station := station;
 for addr in Self.HVs do
   HVDb[addr].PredejStanici(station as TOR);
 Self.Data.announcementPlayed := false;
 Self.changed := true;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TSouprava.SetRychlostSmer(speed:Cardinal; dir:THVStanoviste);
var addr:Integer;
    direction:boolean;
begin
 if ((TBlk(Self.front).typ = btTU) and (TBlkTU(Self.front).rychUpdate)) then
   TBlkTU(Self.front).rychUpdate := false;

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
     on E:Exception do
       AppEvents.LogException(E, 'TSouprava.SetRychlostSmer');
   end;
  end;

 if ((speed > 0) and (Assigned(Self.front)) and
     ((Self.front as TBlkUsek).IsVlakPresun()) and
      ((Self.front as TBlkUsek).Soupravs[(Self.front as TBlkUsek).vlakPresun] = Self.index)) then
  (Self.front as TBlkUsek).VlakPresun := -1;

 writelog('Souprava ' + Self.name + ' : rychlost '+IntToStr(speed)+', směr : '+IntToStr(Integer(dir)), WR_MESSAGE);

 Self.changed := true;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TSouprava.SetSpeed(speed:Integer);
begin
 Self.SetRychlostSmer(speed, Self.data.direction);
end;

procedure TSouprava.SetSmer(smer:THVStanoviste);
var addr: Integer;
begin
 Self.SetRychlostSmer(Self.data.speed, smer);

 for addr in Self.HVs do
   HVDb[addr].OnPredictedSignalChange();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TSouprava.HVComErr(Sender:TObject; Data:Pointer);
begin
 if (Self.data.station <> nil) then
   (Self.data.station as TOR).BlkWriteError(nil, 'Souprava '+Self.name+' nekomunikuje s centrálou', 'CENTRÁLA');
end;

////////////////////////////////////////////////////////////////////////////////

function TSouprava.IsStolen(): Boolean;
var addr: Integer;
begin
 for addr in Self.HVs do
   if (HVDb[addr].stolen) then
     Exit(true);
 Result := false;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TSouprava.VezmiVlak(ok: TCb; err: TCb);
var vezmi: ^TVezmi;
begin
 GetMem(vezmi, sizeof(TVezmi));
 vezmi^.ok := ok;
 vezmi^.err := err;

 vezmi^.nextVezmi := 0;
 while ((vezmi^.nextVezmi < Self.HVs.Count) and (not HVDb[Self.HVs[vezmi^.nextVezmi]].stolen)) do
   Inc(vezmi^.nextVezmi);

 Self.VezmiVlakOk(Self, vezmi);
end;

procedure TSouprava.VezmiVlakOk(Sender: TObject; Data: Pointer);
var vezmi: ^TVezmi;
    addr: Integer;
begin
 vezmi := Data;

 if (vezmi^.nextVezmi >= Self.HVs.Count) then
  begin
   Self.changed := true;
   Self.SetRychlostSmer(Self.speed, Self.direction);
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
   HVDb[addr].TrakceAcquire(TTrakce.Callback(Self.VezmiVlakOk, vezmi),
                            TTrakce.Callback(Self.VezmiVlakErr, vezmi));
 except
   Self.VezmiVlakErr(Sender, vezmi);
 end;
end;

procedure TSouprava.VezmiVlakErr(Sender: TObject; Data: Pointer);
var vezmi: ^TVezmi;
begin
 vezmi := Data;
 if (Assigned(vezmi^.err.callback)) then
   vezmi^.err.callback(Self, vezmi^.err.data);
 FreeMem(vezmi);
 Self.changed := true;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TSouprava.SetFront(front:TObject);
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

procedure TSouprava.UpdateFront();
var blk:TBlk;
begin
 Blky.GetBlkByID(Self.filefront, blk);
 Self.front := blk;
end;

////////////////////////////////////////////////////////////////////////////////

// zmena smeru pri naslapu na smyckovy blok
procedure TSouprava.ChangeSmer();
var addr:Integer;
    tmp:boolean;
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

procedure TSouprava.InterChangeStanice(change_ev:Boolean = true);
var tmp:TObject;
begin
 tmp := Self.data.stationFrom;
 Self.data.stationFrom := Self.data.stationTo;
 Self.data.stationTo := tmp;

 Self.changed := true;
 if ((Self.front <> nil) and (change_ev)) then
   (Self.front as TBlkUsek).Change();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TSouprava.SetSpeedBuffer(speedBuffer:PInteger);
begin
 Self.speedBuffer := speedBuffer;
end;

////////////////////////////////////////////////////////////////////////////////
// V pripade, ze vsechna hnaci vozidla soupravy otocim do opacneho smeru,
// nez je smer soupravy, otoci se i smer soupravy. To umoznuje otoceni smeru
// soupravy z Rocomaus.
// Tato zmena je umoznena jen tehdy pokud nema sipka jednoznacne urceny smer
// a pokud souprava stoji.

procedure TSouprava.LokDirChanged();
var i:Integer;
    dir:Boolean;
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

procedure TSouprava.ToggleHouk(desc:string);
var addr:Integer;
    HV:THV;
begin
 writelog('Souprava ' + Self.name + ' : aktivuji houkání ' + desc, WR_MESSAGE);

 for addr in Self.HVs do
  begin
   HV := HVDb[addr];
   if (HV.CanPlayHouk(desc)) then
     TrakceI.LokFuncToggle(Self, HV, HV.funcDict[desc]);
  end;
end;

procedure TSouprava.SetHoukState(desc:string; state:boolean);
var addr:Integer;
    HV:THV;
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

procedure TSouprava.CheckSH(nav:TObject);
var mnav:TBlkNav;
    oblr:TOR;
    shPlay:TSHToPlay;
    shSpr:TSHSpr;
begin
 if ((not Self.announcement) or (Self.announcementPlayed) or (self.stationFrom = nil) or
     (self.stationTo = nil) or (Self.typ = '')) then Exit();

 mnav := TBlkNav(nav);
 if (mnav.OblsRizeni.Count < 1) then Exit();
 oblr := mnav.OblsRizeni[0];

 if ((not Assigned(oblr.hlaseni)) or (not oblr.hlaseni.available)) then Exit();

 try
   shPlay := stanicniHlaseniHelper.CanPlayPrijezdSH(self, oblr);
 except
   on E:Exception do
     AppEvents.LogException(E, 'CanPlayPrijezdSH');
 end;

 shSpr.cislo := Self.name;
 shSpr.typ   := Self.typ;
 shSpr.fromORid := TOR(Self.stationFrom).id;
 shSpr.toORid := TOR(Self.stationTo).id;
 shSpr.timeArrive := 0;
 shSpr.timeDepart := 0;

 if (shPlay.stanicniKolej <> nil) then
  begin
   shSpr.kolej := shPlay.stanicniKolej.Stav.cislo_koleje;

   if ((Self.IsPOdj(shPlay.stanicniKolej)) and (Self.GetPOdj(shPlay.stanicniKolej).abs_enabled)) then
     shSpr.timeDepart := Self.GetPOdj(shPlay.stanicniKolej).abs;
  end;

 try
   if ((shPlay.stanicniKolej <> nil) and ((shPlay.trat = nil) or (Self.IsPOdj(shPlay.stanicniKolej)))) then begin
     oblr.hlaseni.Prijede(shSpr);
     Self.data.announcementPlayed := true;
   end else if (shPlay.trat <> nil) then begin
     oblr.hlaseni.Projede(shSpr);
     Self.data.announcementPlayed := true;
   end;
 except
   on E:Exception do
     AppEvents.LogException(E, 'Prehravani hlaseni');
 end;
end;

////////////////////////////////////////////////////////////////////////////////
// Predvidane odjezdy:

procedure TSouprava.AddOrUpdatePOdj(usekid:Integer; var podj:TPOdj);
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

function TSouprava.IsPOdj(usekid:Integer):Boolean;
begin
 Result := Self.data.podj.ContainsKey(usekid);
end;

function TSouprava.GetPOdj(usekid:Integer):TPOdj;
begin
 Result := Self.data.podj[usekid];
end;

procedure TSouprava.RemovePOdj(usekid:Integer);
begin
 Self.data.podj[usekid].Free();
 Self.data.podj.Remove(usekid);
end;

procedure TSouprava.AddOrUpdatePOdj(usek:TBlk; var podj:TPOdj);
begin
 Self.AddOrUpdatePOdj(usek.id, podj);
end;

function TSouprava.IsPOdj(usek:TBlk):Boolean;
begin
 Result := Self.IsPOdj(usek.id);
end;

function TSouprava.GetPOdj(usek:TBlk):TPOdj;
begin
 Result := Self.GetPOdj(usek.id);
end;

procedure TSouprava.RemovePOdj(usek:TBlk);
begin
 Self.RemovePOdj(usek.id);
end;

procedure TSouprava.ClearPOdj();
var podj:TPOdj;
begin
 for podj in Self.data.podj.Values do
   podj.Free();

 Self.data.podj.Clear();
end;

////////////////////////////////////////////////////////////////////////////////

function TSouprava.IsAnyLokoInRegulator():Boolean;
var hvaddr:Integer;
begin
 for hvaddr in Self.HVs do
   if (HVDb[hvaddr].Stav.regulators.Count > 0) then
     Exit(true);
 Result := false;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TSouprava.ForceRemoveAllRegulators();
var hvaddr:Integer;
begin
 for hvaddr in Self.HVs do
   if (HVDb[hvaddr].Stav.regulators.Count > 0) then
     HVDb[hvaddr].ForceRemoveAllRegulators();
end;

////////////////////////////////////////////////////////////////////////////////

function TSouprava.GetMaxSpeed():Cardinal;
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
     minimum := Min(Self.data.maxSpeed, HVDb[Self.HVs[0]].Data.maxRychlost)
   else
     minimum := HVDb[Self.HVs[0]].Data.maxRychlost;

   for addr in Self.HVs do
     if (HVDb[addr].Data.maxRychlost < minimum) then
       minimum := HVDb[addr].Data.maxRychlost;

   Result := minimum;
  end;
end;

function TSouprava.GetMaxSpeedStep():Cardinal;
begin
 // vraci rychlost <= max rychlosti takovou, ze pro ni mame prirazeni stupne
 // tj. tuto rychlost lze skutene nastavit
 Result := TrakceI.NearestLowerSpeed(Self.maxSpeed);
end;

////////////////////////////////////////////////////////////////////////////////

function TSouprava.PredictedSignal(): TBlk;
var frontblk: TBlkUsek;
    nav: TBlkNav;
    jc: TJC;
    tu: TBlkTU;
begin
 frontblk := Self.front as TBlkUsek;
 if (frontblk = nil) then
   Exit(nil);

 if (frontblk.typ = btTU) then
   Exit(TBlkTU(frontblk).nextNav);

 case (Self.direction) of
   THVStanoviste.lichy: nav := frontblk.navL as TBlkNav;
   THVStanoviste.sudy: nav := frontblk.navS as TBlkNav;
 end;

 if ((nav <> nil) and (nav.SymbolType = TBlkNavSymbol.hlavni)) then
   Exit(nav);

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
    Blky.GetBlkByID(jc.data.DalsiNavestidlo, TBlk(nav));
    Exit(nav);
  end;
 end;

 Result := nil;
end;

procedure TSouprava.OnPredictedSignalChange();
var addr: Integer;
begin
 for addr in Self.HVs do
   HVDb[addr].OnPredictedSignalChange();
end;

procedure TSouprava.OnExpectedSpeedChange();
var addr: Integer;
begin
 for addr in Self.HVs do
   HVDb[addr].OnExpectedSpeedChange();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TSouprava.GetPtData(json: TJsonObject);
var addr: Integer;
begin
 json['name'] := Self.data.name;
 json['carsCount'] := Self.data.carsCount;
 if (Self.data.note <> '') then
   json['note'] := Self.data.note;
 json['length'] := Self.data.length;
 json['type'] := Self.data.typ;
 json['dirS'] := Self.data.dir_S;
 json['dirL'] := Self.data.dir_L;

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
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit
