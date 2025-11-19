unit TRVDatabase;

{
  Trida TRVDb je databaze hnacich vozidel.

  K principu indexovaci tabulky:
  kazde RV ulozene vpoli adres ma svuj index
  index se pocita pro existujici RV od nejnizsi adresy po nejvyssi
  slouzi ke snadmenu pristupu v tabulkach
  umoznuje jednoduse pripradit index (radek), pokud znamen adresu
}

interface

uses SysUtils, TRailVehicle, Classes, IdContext, IniFiles, Windows, ExtCtrls;

const
  _MAX_ADDR = 10000;
  _DEFAULT_OR = 0;
  _FILE_SUFFIX = '.2lok';
  _LOCO_UPDATE_TIME_MS = 1000;

type

  TRVArray = array [0 .. _MAX_ADDR - 1] of TRV;

  ENoLoco = class(Exception);
  ELocoOnTrain = class(Exception);
  ELocoPrevzato = class(Exception);
  EInvalidAddress = class(Exception);
  ELocoExists = class(Exception);

  TRVDb = class

  private
    // index odpovida adrese
    // jednu adresu muze mit pouze jedno vozidlo v Db
    mVehicles: TRVArray;

    fdefault_or: Integer;
    fLoksDir: string;
    eAcquiredOk: TNotifyEvent;
    eAcquiredErr: TNotifyEvent;
    eReleasedOk: TNotifyEvent;
    eLocoAcquired, eLocoReleased: TNotifyEvent;
    mAcquiring: Boolean;
    mReleasing: Boolean;
    tLocoUpdate: TTimer; // always running

    procedure Clear();
    procedure LoadFile(filename: string; stateini: TMemIniFile);

    function GetCnt(): Word;

    procedure CreateIndex();
    function GetItem(index: Integer): TRV;

    procedure AcquiredOk(Sender: TObject; Data: Pointer);
    procedure AcquiredErr(Sender: TObject; Data: Pointer);
    procedure ReleasedOk(Sender: TObject; Data: Pointer);

    procedure OnTLocoUpdate(Sender: TObject);

  public

    constructor Create();
    destructor Destroy(); override;

    procedure LoadFromDir(const dirname: string; const statefn: string);
    procedure SaveData(const dirname: string);
    procedure SaveState(const statefn: string);

    function Add(Data: TRVData; addr: Word; siteA: TRVSite; area: TObject): TRV; overload;
    function Add(panel_str: string; SenderOR: TObject): TRV; overload;
    procedure Remove(addr: Word);

    procedure RemoveRegulator(conn: TIDContext);

    procedure ClearAllStatistics();
    procedure ExportStatistics(filename: string);

    procedure UpdateTokenTimeout();
    function FilenameForLok(addr: Word): string; overload;
    function FilenameForLok(vehicle: TRV): string; overload;

    function AnyAcquiredRVHasActiveFunc(func: string): Boolean;
    function AllAcquiredRVsHaveActiveFunc(func: string): Boolean;
    function AnyRVToRestoreFunc(func: string): Boolean;

    procedure CSReset();
    procedure TrakceAcquireAllUsed(ok: TNotifyEvent = nil; err: TNotifyEvent = nil; locoAcquired: TNotifyEvent = nil);
    procedure TrakceReleaseAllUsed(ok: TNotifyEvent = nil; locoReleased: TNotifyEvent = nil);

    property cnt: Word read GetCnt; // vypocet tady tohoto trva celkem dlouho, pouzivat obezretne !
    property vehicles: TRVArray read mVehicles;
    property default_or: Integer read fdefault_or write fdefault_or;
    property loksDir: string read fLoksDir;
    property acquiring: Boolean read mAcquiring;
    property releasing: Boolean read mReleasing;

    property Items[index: Integer]: TRV read GetItem; default;

  end; // TRVDb

var
  RVDb: TRVDb;

implementation

uses fMain, DataRV, area, appEv, TrakceIFace, TrakceC, Logging;

/// /////////////////////////////////////////////////////////////////////////////

constructor TRVDb.Create();
begin
  inherited Create();

  Self.tLocoUpdate := TTimer.Create(nil);
  Self.tLocoUpdate.Interval := _LOCO_UPDATE_TIME_MS;
  Self.tLocoUpdate.OnTimer := Self.OnTLocoUpdate;
  Self.tLocoUpdate.Enabled := true;

  Self.fdefault_or := _DEFAULT_OR;
  Self.mAcquiring := false;
  Self.mReleasing := false;

  for var i := 0 to _MAX_ADDR - 1 do
    Self.mVehicles[i] := nil;
end;

destructor TRVDb.Destroy();
begin
  Self.tLocoUpdate.Free();
  Self.Clear();
  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRVDb.Clear();
begin
  for var i: Integer := 0 to _MAX_ADDR - 1 do
    if (Self.mVehicles[i] <> nil) then
      FreeAndNil(Self.mVehicles[i]);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRVDb.LoadFile(filename: string; stateini: TMemIniFile);
var ini: TMemIniFile;
    sections: TStrings;
begin
  ini := nil;
  sections := nil;

  try
    ini := TMemIniFile.Create(filename, TEncoding.UTF8);
    sections := TStringList.Create();
    ini.ReadSections(sections);

    // sem prijde kontrola verze souboru

    for var sect: string in sections do
    begin
      if (sect = 'global') then
        continue;

      // nacteni jedne loko
      var vehicle: TRV;
      try
        vehicle := TRV.Create(ini, stateini, sect);
      except
        on E: Exception do
          AppEvents.LogException(E, 'Chyba pri nacitani souboru loko : ' + filename + ', sekce ' + sect);
      end;

      if (vehicle = nil) then
        continue;

      if (Self.mVehicles[vehicle.addr] <> nil) then
      begin
        FreeAndNil(vehicle);
      end else begin
        Self.mVehicles[vehicle.addr] := vehicle;
      end;
    end; // for sect in sections
  except
    on E: Exception do
    begin
      AppEvents.LogException(E, 'Chyba pri nacitani souboru loko ' + filename);
      if (ini <> nil) then
        ini.Free();
      if (sections <> nil) then
        sections.Free();
    end;
  end;

  ini.Free();
  sections.Free();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRVDb.LoadFromDir(const dirname: string; const statefn: string);
var SR: TSearchRec;
  stateini: TMemIniFile;
begin
  Self.fLoksDir := dirname;
  stateini := TMemIniFile.Create(statefn);

  try
    Self.Clear();

    // prohledavani adresare a nacitani soubor *.2lok
    // najdeme prvni soubor
    if (FindFirst(dirname + '\*' + _FILE_SUFFIX, faAnyFile, SR) = 0) then
    begin
      if ((SR.Attr AND faDirectory) = 0) then
        Self.LoadFile(dirname + '\' + SR.Name, stateini);

      // hledame dalsi soubory
      while (FindNext(SR) = 0) do
        if ((SR.Attr AND faDirectory) = 0) then
          Self.LoadFile(dirname + '\' + SR.Name, stateini);

      SysUtils.FindClose(SR);
    end;

    Self.CreateIndex();
    RVTableData.LoadToTable();
  finally
    stateini.Free();
  end;
end;

procedure TRVDb.SaveData(const dirname: string);
begin
  Log('Ukládám vozidla...', llInfo, lsData);
  Self.fLoksDir := dirname;

  var count: Cardinal := 0;
  for var i: Integer := 0 to _MAX_ADDR - 1 do
  begin
    if (Self.mVehicles[i] <> nil) then
    begin
      try
        Self.mVehicles[i].SaveData(Self.FilenameForLok(Self.mVehicles[i]));
        Inc(count);
      except
        on E: Exception do
          AppEvents.LogException(E, 'TRVDb.SaveData ' + IntToStr(i));
      end;
    end; // if <> nil
  end; // for i
  Log('Uloženo vozidel: '+IntToStr(count), llInfo, lsData);
end;

procedure TRVDb.SaveState(const statefn: string);
var stateini: TMemIniFile;
begin
  stateini := TMemIniFile.Create(statefn);

  try
    for var i: Integer := 0 to _MAX_ADDR - 1 do
    begin
      if (Self.mVehicles[i] <> nil) then
      begin
        try
          Self.mVehicles[i].SaveState(stateini);
        except
          on E: Exception do
            AppEvents.LogException(E, 'TRVDb.SaveState ' + IntToStr(i));
        end;
      end; // if <> nil
    end; // for i
  finally
    stateini.UpdateFile();
    stateini.Free();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TRVDb.Add(Data: TRVData; addr: Word; siteA: TRVSite; area: TObject): TRV;
var state: TRVState;
begin
  if (addr > 9999) then
    raise EInvalidAddress.Create('Neplatná adresa lokomotivy ' + IntToStr(addr));
  if (Self.mVehicles[addr] <> nil) then
    raise ELocoExists.Create('Lokomotiva s adresou ' + IntToStr(addr) + ' již existuje');

  // pokud neexistuje, pridame ji

  state.siteA := siteA;
  state.traveled_forward := 0;
  state.traveled_backward := 0;
  state.area := (area as TArea);

  state.train := -1;
  state.manual := false;

  state.regulators := nil;
  state.tokens := nil;

  Self.mVehicles[addr] := TRV.Create(addr, Data, state);

  // ------- update indexu: ------

  // najdeme nejblizsi spodni index
  var index: Integer := -1;
  if (addr > 0) then
  begin
    for var i: Integer := addr - 1 downto 0 do
    begin
      if (Self.mVehicles[i] <> nil) then
      begin
        index := Self.mVehicles[i].index;
        break;
      end;
    end;
  end; // if addr > 0

  // nasemu RV priradime tento index + 1
  Self.mVehicles[addr].index := index + 1;

  // vsem RV nad nasim rv zvysime index o 1
  if (addr < _MAX_ADDR - 1) then
  begin
    for var i: Integer := addr + 1 to _MAX_ADDR - 1 do
      if (Self.mVehicles[i] <> nil) then
        Self.mVehicles[i].index := Self.mVehicles[i].index + 1;
  end;

  // aktualizujeme tabulky:
  RVTableData.AddRV(Self.mVehicles[addr].index, Self.mVehicles[addr]);

  Result := Self.mVehicles[addr];
end;

function TRVDb.Add(panel_str: string; SenderOR: TObject): TRV;
var vehicle: TRV;
begin
  try
    vehicle := TRV.Create(panel_str, (SenderOR as TArea));
  except
    on E: Exception do
    begin
      raise Exception.Create(E.Message);
      vehicle.Free();
      Exit();
    end; // on e: Exception
  end;

  if (Self[vehicle.addr] <> nil) then
  begin
    raise Exception.Create('RV ' + IntToStr(vehicle.addr) + ' již existuje');
    vehicle.Free();
    Exit();
  end;

  Self.mVehicles[vehicle.addr] := vehicle;

  // ------- update indexu: ------

  // najdeme nejblizsi spodni index
  var index: Integer := -1;
  if (vehicle.addr > 0) then
  begin
    for var i: Integer := vehicle.addr - 1 downto 0 do
    begin
      if (Self.mVehicles[i] <> nil) then
      begin
        index := Self.mVehicles[i].index;
        break;
      end;
    end;
  end; // if addr > 0

  // nasemu RV priradime tento index + 1
  vehicle.index := index + 1;

  // vsem RV nad nasim rv zvysime index o 1
  if (vehicle.addr < _MAX_ADDR - 1) then
  begin
    for var i: Integer := vehicle.addr + 1 to _MAX_ADDR - 1 do
      if (Self.mVehicles[i] <> nil) then
        Self.mVehicles[i].index := Self.mVehicles[i].index + 1;
  end;

  // aktualizujeme tabulky:
  RVTableData.AddRV(vehicle.index, vehicle);

  Result := vehicle;
end;

procedure TRVDb.Remove(addr: Word);
begin
  // rv neexistuje
  if (Self.mVehicles[addr] = nil) then
    raise ENoLoco.Create('Lokomotiva s touto adresou neexistuje!');
  if (Self.mVehicles[addr].state.train > -1) then
    raise ELocoOnTrain.Create('Lokomotiva je na vlaku!');
  if ((Self.mVehicles[addr].acquired) or (Self.mVehicles[addr].stolen)) then
    raise ELocoPrevzato.Create('Lokomotiva převzata do řízení počítače');

  var index: Integer := Self.mVehicles[addr].index;
  FreeAndNil(Self.mVehicles[addr]);

  // smazat soubor
  SysUtils.DeleteFile(Self.FilenameForLok(addr));

  // ------- update indexu: ------

  // vsechny indexy od addr zmensime o 1:
  for var i: Integer := addr to _MAX_ADDR - 1 do
    if (Self.mVehicles[i] <> nil) then
      Self.mVehicles[i].index := Self.mVehicles[i].index - 1;

  // aktualizujeme tabulky:
  RVTableData.RemoveRV(index);
end;

/// /////////////////////////////////////////////////////////////////////////////

// spocita pocet hnacich vozidel
function TRVDb.GetCnt(): Word;
begin
  Result := 0;
  for var i: Integer := 0 to _MAX_ADDR - 1 do
    if (Self.mVehicles[i] <> nil) then
      Result := Result + 1;
end;

/// /////////////////////////////////////////////////////////////////////////////

// vytvori index hnacich vozidel
// vola se jen pri nacteni souboru
// update indxu si zajistuji metody Add a remove trosku jinym algoritmem
// (neni zapotrebi kontrolovat cele pole)
procedure TRVDb.CreateIndex();
var index: Integer;
begin
  index := 0;
  for var i: Integer := 0 to _MAX_ADDR - 1 do
  begin
    if (Self.mVehicles[i] <> nil) then
    begin
      Self.mVehicles[i].index := index;
      index := index + 1;
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRVDb.RemoveRegulator(conn: TIDContext);
begin
  for var i: Integer := 0 to _MAX_ADDR - 1 do
    if (Assigned(Self.mVehicles[i])) then
      Self.mVehicles[i].RemoveRegulator(conn);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRVDb.UpdateTokenTimeout();
begin
  for var i: Integer := 0 to _MAX_ADDR - 1 do
    if (Assigned(Self.mVehicles[i])) then
      Self.mVehicles[i].UpdateTokenTimeout();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRVDb.ClearAllStatistics();
begin
  for var i: Integer := 0 to _MAX_ADDR - 1 do
    if (Assigned(Self.mVehicles[i])) then
      Self.mVehicles[i].ResetStats();
  RVTableData.reload := true;
  RVTableData.UpdateTable();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRVDb.ExportStatistics(filename: string);
begin
  var sw := TStreamWriter.Create(filename, False, TEncoding.UTF8);

  try
    sw.WriteLine('adresa,nazev,majitel,najeto_metru_vpred,najeto_metru_vzad');

    for var i: Integer := 0 to _MAX_ADDR - 1 do
      if (Assigned(Self.mVehicles[i])) then
        sw.WriteLine(Self.mVehicles[i].ExportStats());
  finally
    sw.Free();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TRVDb.FilenameForLok(addr: Word): string;
begin
  Result := Self.loksDir + '\L_' + IntToStr(addr) + _FILE_SUFFIX;
end;

function TRVDb.FilenameForLok(vehicle: TRV): string;
begin
  Result := Self.FilenameForLok(vehicle.addr);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TRVDb.GetItem(index: Integer): TRV;
begin
  Result := Self.mVehicles[index];
end;

procedure TRVDb.CSReset();
begin
  Self.mAcquiring := false;
  Self.mReleasing := false;
  for var addr: Word := 0 to _MAX_ADDR - 1 do
    if (RVDb[addr] <> nil) then
      RVDb[addr].CSReset();
end;

/// /////////////////////////////////////////////////////////////////////////////
// Trakce
/// /////////////////////////////////////////////////////////////////////////////

procedure TRVDb.TrakceAcquireAllUsed(ok: TNotifyEvent = nil; err: TNotifyEvent = nil; locoAcquired: TNotifyEvent = nil);
begin
  Self.mAcquiring := true;
  Self.mReleasing := false;
  Self.eAcquiredOk := ok;
  Self.eAcquiredErr := err;
  Self.eLocoAcquired := locoAcquired;

  Self.AcquiredOk(Self, Pointer(0));
end;

procedure TRVDb.AcquiredOk(Sender: TObject; Data: Pointer);
var addr: Word;
begin
  addr := Word(Data);

  if ((addr <> 0) and (Assigned(Self.eLocoAcquired))) then
    Self.eLocoAcquired(Self);

  if (not Self.mAcquiring) then
  begin
    Self.mAcquiring := false;
    if (Assigned(Self.eAcquiredErr)) then
      Self.eAcquiredErr(Self);
    Exit();
  end;

  while ((addr < _MAX_ADDR) and ((RVDb[addr] = nil) or (not RVDb[addr].ShouldAcquire()))) do
    Inc(addr);

  if (addr = _MAX_ADDR) then
  begin
    Self.mAcquiring := false;
    if (Assigned(Self.eAcquiredOk)) then
      Self.eAcquiredOk(Self);
    Exit();
  end;

  Data := Pointer(addr + 1);
  try
    RVDb[addr].TrakceAcquire(TTrakce.Callback(Self.AcquiredOk, Data), TTrakce.Callback(Self.AcquiredErr, Data));
  except
    Self.AcquiredErr(Self, Data);
  end;
end;

procedure TRVDb.AcquiredErr(Sender: TObject; Data: Pointer);
begin
  Self.mAcquiring := false;
  trakce.Log(llErrors, 'ERR: LOKO ' + IntToStr(Word(Data)) + ' se nepodařilo převzít');
  F_Main.LogBrief('LOKO: loko ' + IntToStr(Word(Data)) + ' se nepodařilo převzít', llError);
  if (Assigned(Self.eAcquiredErr)) then
    Self.eAcquiredErr(Self);
end;

procedure TRVDb.TrakceReleaseAllUsed(ok: TNotifyEvent = nil; locoReleased: TNotifyEvent = nil);
begin
  Self.mAcquiring := false;
  Self.mReleasing := true;
  Self.eReleasedOk := ok;
  Self.eLocoReleased := locoReleased;

  Self.ReleasedOk(Self, Pointer(0));
end;

procedure TRVDb.ReleasedOk(Sender: TObject; Data: Pointer);
var addr: Word;
begin
  addr := Word(Data);

  if ((addr <> 0) and (Assigned(Self.eLocoReleased))) then
    Self.eLocoReleased(Self);

  if (not Self.mReleasing) then
  begin
    Self.mReleasing := false;
    if (Assigned(Self.eReleasedOk)) then
      Self.eReleasedOk(Self);
    Exit();
  end;

  while ((addr < _MAX_ADDR) and ((RVDb[addr] = nil) or (not RVDb[addr].acquired))) do
    Inc(addr);

  if (addr = _MAX_ADDR) then
  begin
    Self.mReleasing := false;
    if (Assigned(Self.eReleasedOk)) then
      Self.eReleasedOk(Self);
    Exit();
  end;

  Data := Pointer(addr + 1);
  try
    RVDb[addr].TrakceRelease(TTrakce.Callback(Self.ReleasedOk, Data));
  except
    Self.ReleasedOk(Self, Data);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRVDb.OnTLocoUpdate(Sender: TObject);
var addr: Word;
begin
  for addr := 0 to _MAX_ADDR - 1 do
  begin
    if (Self.mVehicles[addr] = nil) then
      continue;

    try
      if ((Self.mVehicles[addr].stolen) and (not Self.mVehicles[addr].acquiring) and (not Self.mVehicles[addr].updating) and
        (Now > Self.mVehicles[addr].lastUpdated + EncodeTime(0, 0, _LOCO_UPDATE_TIME_MS div 1000,
        _LOCO_UPDATE_TIME_MS mod 1000))) then
        Self.mVehicles[addr].TrakceUpdateState(TTrakce.Callback(), TTrakce.Callback());

      Self.mVehicles[addr].UpdateTraveled(_LOCO_UPDATE_TIME_MS div 1000);
    except
      on E: Exception do
        AppEvents.LogException(E, 'RVDb.OnTLocoUpdate');
    end;
  end;

end;

/// /////////////////////////////////////////////////////////////////////////////

function TRVDb.AnyAcquiredRVHasActiveFunc(func: string): Boolean;
begin
  for var vehicle: TRV in Self.mVehicles do
  begin
    if ((vehicle <> nil) and (vehicle.acquired) and (vehicle.funcDict.ContainsKey(func)) and (vehicle.slotFunctions[vehicle.funcDict[func]]))
    then
      Exit(true);
  end;
  Result := false;
end;

function TRVDb.AllAcquiredRVsHaveActiveFunc(func: string): Boolean;
begin
  for var vehicle: TRV in Self.mVehicles do
  begin
    if ((vehicle <> nil) and (vehicle.acquired) and (vehicle.funcDict.ContainsKey(func)) and (not vehicle.slotFunctions[vehicle.funcDict[func]]))
    then
      Exit(false);
  end;
  Result := true;
end;

function TRVDb.AnyRVToRestoreFunc(func: string): Boolean;
begin
  for var vehicle: TRV in Self.mVehicles do
  begin
    if ((vehicle <> nil) and (vehicle.acquired) and (vehicle.funcDict.ContainsKey(func)) and (not vehicle.slotFunctions[vehicle.funcDict[func]])
      and (vehicle.stateFunctions[vehicle.funcDict[func]])) then
      Exit(true);
  end;
  Result := false;
end;

/// /////////////////////////////////////////////////////////////////////////////

initialization

RVDb := TRVDb.Create();

finalization

FreeAndNil(RVDb);

end.
