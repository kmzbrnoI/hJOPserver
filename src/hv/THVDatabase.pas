unit THVDatabase;

{
  Trida THVDb je databaze hnacich vozidel.

  K principu indexovaci tabulky:
  kazde HV ulozene vpoli adres ma svuj index
  index se pocita pro existujici HV od nejnizsi adresy po nejvyssi
  slouzi ke snadmenu pristupu v tabulkach
  umoznuje jednoduse pripradit index (radek), pokud znamen adresu
}

interface

uses SysUtils, THnaciVozidlo, Classes, IdContext, IniFiles, Windows, ExtCtrls;

const
  _MAX_ADDR = 10000;
  _DEFAULT_OR = 0;
  _FILE_SUFFIX = '.2lok';
  _LOCO_UPDATE_TIME_MS = 1000;

type

  THVArray = array [0 .. _MAX_ADDR - 1] of THV;

  ENoLoco = class(Exception);
  ELocoOnTrain = class(Exception);
  ELocoPrevzato = class(Exception);
  EInvalidAddress = class(Exception);
  ELocoExists = class(Exception);

  THVDb = class

  private
    // index odpovida adrese
    // jednu adresu muze mit pouze jedno vozidlo v Db
    HVs: THVArray;

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
    function GetItem(index: Integer): THV;

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

    function Add(Data: THVData; addr: Word; siteA: THVSite; area: TObject): THV; overload;
    function Add(panel_str: string; SenderOR: TObject): THV; overload;
    procedure Remove(addr: Word);

    procedure RemoveRegulator(conn: TIDContext);

    procedure ClearAllStatistics();
    procedure ExportStatistics(filename: string);

    procedure UpdateTokenTimeout();
    function FilenameForLok(addr: Word): string; overload;
    function FilenameForLok(hv: THV): string; overload;

    function AnyAcquiredHVHasActiveFunc(func: string): Boolean;
    function AllAcquiredHVsHaveActiveFunc(func: string): Boolean;
    function AnyHvToRestoreFunc(func: string): Boolean;

    procedure CSReset();
    procedure TrakceAcquireAllUsed(ok: TNotifyEvent = nil; err: TNotifyEvent = nil; locoAcquired: TNotifyEvent = nil);
    procedure TrakceReleaseAllUsed(ok: TNotifyEvent = nil; locoReleased: TNotifyEvent = nil);

    property cnt: Word read GetCnt; // vypocet tady tohoto trva celkem dlouho, pouzivat obezretne !
    property HVozidla: THVArray read HVs;
    property default_or: Integer read fdefault_or write fdefault_or;
    property loksDir: string read fLoksDir;
    property acquiring: Boolean read mAcquiring;
    property releasing: Boolean read mReleasing;

    property Items[index: Integer]: THV read GetItem; default;

  end; // THVDb

var
  HVDb: THVDb;

implementation

uses fMain, DataHV, area, appEv, TrakceIFace, TrakceC;

/// /////////////////////////////////////////////////////////////////////////////

constructor THVDb.Create();
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
    Self.HVs[i] := nil;
end;

destructor THVDb.Destroy();
begin
  Self.tLocoUpdate.Free();
  Self.Clear();
  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure THVDb.Clear();
begin
  for var i: Integer := 0 to _MAX_ADDR - 1 do
    if (Self.HVs[i] <> nil) then
      FreeAndNil(Self.HVs[i]);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure THVDb.LoadFile(filename: string; stateini: TMemIniFile);
var aHV: THV;
  ini: TMemIniFile;
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
      try
        aHV := THV.Create(ini, stateini, sect);
      except
        on E: Exception do
          AppEvents.LogException(E, 'Chyba pri nacitani souboru loko : ' + filename + ', sekce ' + sect);
      end;

      if (aHV = nil) then
        continue;

      if (Self.HVs[aHV.addr] <> nil) then
      begin
        FreeAndNil(aHV);
      end else begin
        Self.HVs[aHV.addr] := aHV;
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

procedure THVDb.LoadFromDir(const dirname: string; const statefn: string);
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
    HVTableData.LoadToTable();
  finally
    stateini.Free();
  end;
end;

procedure THVDb.SaveData(const dirname: string);
begin
  Self.fLoksDir := dirname;

  for var i: Integer := 0 to _MAX_ADDR - 1 do
  begin
    if (Self.HVs[i] <> nil) then
    begin
      try
        Self.HVs[i].SaveData(Self.FilenameForLok(Self.HVs[i]));
      except
        on E: Exception do
          AppEvents.LogException(E, 'THVDb.SaveData ' + IntToStr(i));
      end;
    end; // if <> nil
  end; // for i
end;

procedure THVDb.SaveState(const statefn: string);
var stateini: TMemIniFile;
begin
  stateini := TMemIniFile.Create(statefn);

  try
    for var i: Integer := 0 to _MAX_ADDR - 1 do
    begin
      if (Self.HVs[i] <> nil) then
      begin
        try
          Self.HVs[i].SaveState(stateini);
        except
          on E: Exception do
            AppEvents.LogException(E, 'THVDb.SaveState ' + IntToStr(i));
        end;
      end; // if <> nil
    end; // for i
  finally
    stateini.UpdateFile();
    stateini.Free();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function THVDb.Add(Data: THVData; addr: Word; siteA: THVSite; area: TObject): THV;
var state: THVState;
begin
  if (addr > 9999) then
    raise EInvalidAddress.Create('Neplatná adresa lokomotivy ' + IntToStr(addr));
  if (Self.HVs[addr] <> nil) then
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

  Self.HVs[addr] := THV.Create(addr, Data, state);

  // ------- update indexu: ------

  // najdeme nejblizsi spodni index
  var index: Integer := -1;
  if (addr > 0) then
  begin
    for var i: Integer := addr - 1 downto 0 do
    begin
      if (Self.HVs[i] <> nil) then
      begin
        index := Self.HVs[i].index;
        break;
      end;
    end;
  end; // if addr > 0

  // nasemu HV priradime tento index + 1
  Self.HVs[addr].index := index + 1;

  // vsem HV nad nasim hv zvysime index o 1
  if (addr < _MAX_ADDR - 1) then
  begin
    for var i: Integer := addr + 1 to _MAX_ADDR - 1 do
      if (Self.HVs[i] <> nil) then
        Self.HVs[i].index := Self.HVs[i].index + 1;
  end;

  // aktualizujeme tabulky:
  HVTableData.AddHV(Self.HVs[addr].index, Self.HVs[addr]);

  Result := Self.HVs[addr];
end;

function THVDb.Add(panel_str: string; SenderOR: TObject): THV;
var hv: THV;
begin
  try
    hv := THV.Create(panel_str, (SenderOR as TArea));
  except
    on E: Exception do
    begin
      raise Exception.Create(E.Message);
      hv.Free();
      Exit();
    end; // on e: Exception
  end;

  if (Self[hv.addr] <> nil) then
  begin
    raise Exception.Create('HV ' + IntToStr(hv.addr) + ' již existuje');
    hv.Free();
    Exit();
  end;

  Self.HVs[hv.addr] := hv;

  // ------- update indexu: ------

  // najdeme nejblizsi spodni index
  var index: Integer := -1;
  if (hv.addr > 0) then
  begin
    for var i: Integer := hv.addr - 1 downto 0 do
    begin
      if (Self.HVs[i] <> nil) then
      begin
        index := Self.HVs[i].index;
        break;
      end;
    end;
  end; // if addr > 0

  // nasemu HV priradime tento index + 1
  hv.index := index + 1;

  // vsem HV nad nasim hv zvysime index o 1
  if (hv.addr < _MAX_ADDR - 1) then
  begin
    for var i: Integer := hv.addr + 1 to _MAX_ADDR - 1 do
      if (Self.HVs[i] <> nil) then
        Self.HVs[i].index := Self.HVs[i].index + 1;
  end;

  // aktualizujeme tabulky:
  HVTableData.AddHV(hv.index, hv);

  Result := hv;
end;

procedure THVDb.Remove(addr: Word);
begin
  // hv neexistuje
  if (Self.HVs[addr] = nil) then
    raise ENoLoco.Create('Lokomotiva s touto adresou neexistuje!');
  if (Self.HVs[addr].state.train > -1) then
    raise ELocoOnTrain.Create('Lokomotiva je na soupravě!');
  if ((Self.HVs[addr].acquired) or (Self.HVs[addr].stolen)) then
    raise ELocoPrevzato.Create('Lokomotiva převzata do řízení počítače');

  var index: Integer := Self.HVs[addr].index;
  FreeAndNil(Self.HVs[addr]);

  // smazat soubor
  SysUtils.DeleteFile(Self.FilenameForLok(addr));

  // ------- update indexu: ------

  // vsechny indexy od addr zmensime o 1:
  for var i: Integer := addr to _MAX_ADDR - 1 do
    if (Self.HVs[i] <> nil) then
      Self.HVs[i].index := Self.HVs[i].index - 1;

  // aktualizujeme tabulky:
  HVTableData.RemoveHV(index);
end;

/// /////////////////////////////////////////////////////////////////////////////

// spocita pocet hnacich vozidel
function THVDb.GetCnt(): Word;
begin
  Result := 0;
  for var i: Integer := 0 to _MAX_ADDR - 1 do
    if (Self.HVs[i] <> nil) then
      Result := Result + 1;
end;

/// /////////////////////////////////////////////////////////////////////////////

// vytvori index hnacich vozidel
// vola se jen pri nacteni souboru
// update indxu si zajistuji metody Add a remove trosku jinym algoritmem
// (neni zapotrebi kontrolovat cele pole)
procedure THVDb.CreateIndex();
var index: Integer;
begin
  index := 0;
  for var i: Integer := 0 to _MAX_ADDR - 1 do
  begin
    if (Self.HVs[i] <> nil) then
    begin
      Self.HVs[i].index := index;
      index := index + 1;
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure THVDb.RemoveRegulator(conn: TIDContext);
begin
  for var i: Integer := 0 to _MAX_ADDR - 1 do
    if (Assigned(Self.HVs[i])) then
      Self.HVs[i].RemoveRegulator(conn);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure THVDb.UpdateTokenTimeout();
begin
  for var i: Integer := 0 to _MAX_ADDR - 1 do
    if (Assigned(Self.HVs[i])) then
      Self.HVs[i].UpdateTokenTimeout();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure THVDb.ClearAllStatistics();
begin
  for var i: Integer := 0 to _MAX_ADDR - 1 do
    if (Assigned(Self.HVs[i])) then
      Self.HVs[i].ResetStats();
  HVTableData.reload := true;
  HVTableData.UpdateTable();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure THVDb.ExportStatistics(filename: string);
begin
  var sw := TStreamWriter.Create(filename, False, TEncoding.UTF8);

  try
    sw.WriteLine('adresa,nazev,majitel,najeto_metru_vpred,najeto_metru_vzad');

    for var i: Integer := 0 to _MAX_ADDR - 1 do
      if (Assigned(Self.HVs[i])) then
        sw.WriteLine(Self.HVs[i].ExportStats());
  finally
    sw.Free();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function THVDb.FilenameForLok(addr: Word): string;
begin
  Result := Self.loksDir + '\L_' + IntToStr(addr) + _FILE_SUFFIX;
end;

function THVDb.FilenameForLok(hv: THV): string;
begin
  Result := Self.FilenameForLok(hv.addr);
end;

/// /////////////////////////////////////////////////////////////////////////////

function THVDb.GetItem(index: Integer): THV;
begin
  Result := Self.HVs[index];
end;

procedure THVDb.CSReset();
begin
  Self.mAcquiring := false;
  Self.mReleasing := false;
  for var addr: Word := 0 to _MAX_ADDR - 1 do
    if (HVDb[addr] <> nil) then
      HVDb[addr].CSReset();
end;

/// /////////////////////////////////////////////////////////////////////////////
// Trakce
/// /////////////////////////////////////////////////////////////////////////////

procedure THVDb.TrakceAcquireAllUsed(ok: TNotifyEvent = nil; err: TNotifyEvent = nil; locoAcquired: TNotifyEvent = nil);
begin
  Self.mAcquiring := true;
  Self.mReleasing := false;
  Self.eAcquiredOk := ok;
  Self.eAcquiredErr := err;
  Self.eLocoAcquired := locoAcquired;

  Self.AcquiredOk(Self, Pointer(0));
end;

procedure THVDb.AcquiredOk(Sender: TObject; Data: Pointer);
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

  while ((addr < _MAX_ADDR) and ((HVDb[addr] = nil) or (not HVDb[addr].ShouldAcquire()))) do
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
    HVDb[addr].TrakceAcquire(TTrakce.Callback(Self.AcquiredOk, Data), TTrakce.Callback(Self.AcquiredErr, Data));
  except
    Self.AcquiredErr(Self, Data);
  end;
end;

procedure THVDb.AcquiredErr(Sender: TObject; Data: Pointer);
begin
  Self.mAcquiring := false;
  trakce.Log(llErrors, 'ERR: LOKO ' + IntToStr(Word(Data)) + ' se nepodařilo převzít');
  F_Main.LogStatus('LOKO: loko ' + IntToStr(Word(Data)) + ' se nepodařilo převzít');
  if (Assigned(Self.eAcquiredErr)) then
    Self.eAcquiredErr(Self);
end;

procedure THVDb.TrakceReleaseAllUsed(ok: TNotifyEvent = nil; locoReleased: TNotifyEvent = nil);
begin
  Self.mAcquiring := false;
  Self.mReleasing := true;
  Self.eReleasedOk := ok;
  Self.eLocoReleased := locoReleased;

  Self.ReleasedOk(Self, Pointer(0));
end;

procedure THVDb.ReleasedOk(Sender: TObject; Data: Pointer);
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

  while ((addr < _MAX_ADDR) and ((HVDb[addr] = nil) or (not HVDb[addr].acquired))) do
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
    HVDb[addr].TrakceRelease(TTrakce.Callback(Self.ReleasedOk, Data));
  except
    Self.ReleasedOk(Self, Data);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure THVDb.OnTLocoUpdate(Sender: TObject);
var addr: Word;
begin
  for addr := 0 to _MAX_ADDR - 1 do
  begin
    if (Self.HVs[addr] = nil) then
      continue;

    try
      if ((Self.HVs[addr].stolen) and (not Self.HVs[addr].acquiring) and (not Self.HVs[addr].updating) and
        (Now > Self.HVs[addr].lastUpdated + EncodeTime(0, 0, _LOCO_UPDATE_TIME_MS div 1000,
        _LOCO_UPDATE_TIME_MS mod 1000))) then
        Self.HVs[addr].TrakceUpdateState(TTrakce.Callback(), TTrakce.Callback());

      Self.HVs[addr].UpdateTraveled(_LOCO_UPDATE_TIME_MS div 1000);
    except
      on E: Exception do
        AppEvents.LogException(E, 'HVDb.OnTLocoUpdate');
    end;
  end;

end;

/// /////////////////////////////////////////////////////////////////////////////

function THVDb.AnyAcquiredHVHasActiveFunc(func: string): Boolean;
begin
  for var hv: THV in Self.HVs do
  begin
    if ((hv <> nil) and (hv.acquired) and (hv.funcDict.ContainsKey(func)) and (hv.slotFunctions[hv.funcDict[func]]))
    then
      Exit(true);
  end;
  Result := false;
end;

function THVDb.AllAcquiredHVsHaveActiveFunc(func: string): Boolean;
begin
  for var hv: THV in Self.HVs do
  begin
    if ((hv <> nil) and (hv.acquired) and (hv.funcDict.ContainsKey(func)) and (not hv.slotFunctions[hv.funcDict[func]]))
    then
      Exit(false);
  end;
  Result := true;
end;

function THVDb.AnyHvToRestoreFunc(func: string): Boolean;
begin
  for var hv: THV in Self.HVs do
  begin
    if ((hv <> nil) and (hv.acquired) and (hv.funcDict.ContainsKey(func)) and (not hv.slotFunctions[hv.funcDict[func]])
      and (hv.stateFunctions[hv.funcDict[func]])) then
      Exit(true);
  end;
  Result := false;
end;

/// /////////////////////////////////////////////////////////////////////////////

initialization

HVDb := THVDb.Create();

finalization

FreeAndNil(HVDb);

end.
