﻿unit THVDatabase;

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

  THVArray = array [0.._MAX_ADDR-1] of THV;

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

     function Add(data: THVData; addr: Word; StanovisteA: THVStanoviste; area: TObject): THV; overload;
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

     property cnt: Word read GetCnt;              // vypocet tady tohoto trva celkem dlouho, pouzivat obezretne !
     property HVozidla: THVArray read HVs;
     property default_or: Integer read fdefault_or write fdefault_or;
     property loksDir: string read fLoksDir;
     property acquiring: Boolean read mAcquiring;
     property releasing: Boolean read mReleasing;

     property Items[index : integer] : THV read GetItem; default;

  end;//THVDb

var
  HVDb: THVDb;

implementation

uses fMain, DataHV, Area, appEv, Trakce, TechnologieTrakce;

////////////////////////////////////////////////////////////////////////////////

constructor THVDb.Create();
var i: Integer;
begin
 inherited Create();

 Self.tLocoUpdate := TTimer.Create(nil);
 Self.tLocoUpdate.Interval := _LOCO_UPDATE_TIME_MS;
 Self.tLocoUpdate.OnTimer := Self.OnTLocoUpdate;
 Self.tLocoUpdate.Enabled := true;

 Self.fdefault_or := _DEFAULT_OR;
 Self.mAcquiring := false;
 Self.mReleasing := false;

 // ukazatele nastavit na nil
 for i := 0 to _MAX_ADDR-1 do
   Self.HVs[i] := nil;
end;//ctor

destructor THVDb.Destroy();
begin
 Self.tLocoUpdate.Free();
 Self.Clear();
 inherited;
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure THVDb.Clear();
var i: Integer;
begin
 for i := 0 to _MAX_ADDR-1 do
   if (Self.HVs[i] <> nil) then
     FreeAndNil(Self.HVs[i]);
end;

////////////////////////////////////////////////////////////////////////////////

procedure THVDb.LoadFile(filename: string; stateini: TMemIniFile);
var aHV: THV;
    ini: TMemIniFile;
    sections: TStrings;
    sect: string;
begin
 ini      := nil;
 sections := nil;

 try
   ini := TMemIniFile.Create(filename, TEncoding.UTF8);
   sections := TStringList.Create();
   ini.ReadSections(sections);

   // sem prijde kontrola verze souboru

   for sect in sections do
    begin
     if (sect = 'global') then continue;

     // nacteni jedne loko
     try
       aHV := THV.Create(ini, stateini, sect);
     except
       on E: Exception do
         AppEvents.LogException(E, 'Chyba pri nacitani souboru loko : '+filename + ', sekce '+sect);
     end;

     if (aHV = nil) then continue;     

     if (Self.HVs[aHV.addr] <> nil) then
      begin
       FreeAndNil(aHv);
      end else begin
       Self.HVs[aHV.addr] := aHV;
      end;
    end;//for sect in sections
 except
  on E: Exception do
   begin
    AppEvents.LogException(E, 'Chyba pri nacitani souboru loko '+filename);
    if (ini <> nil) then ini.Free();
    if (sections <> nil) then sections.Free();
   end;
 end;

 ini.Free();
 sections.Free();
end;

////////////////////////////////////////////////////////////////////////////////

procedure THVDb.LoadFromDir(const dirname: string; const statefn: string);
var SR: TSearchRec;
    stateIni: TMemIniFile;
 begin
  Self.fLoksDir := dirname;
  stateIni := TMemIniFile.Create(statefn);

  try
    Self.Clear();

    // prohledavani adresare a nacitani soubor *.2lok
    // najdeme prvni soubor
    if (FindFirst(dirname+'\*'+_FILE_SUFFIX, faAnyFile, SR) = 0) then
     begin
      if ((SR.Attr AND faDirectory) = 0) then
        Self.LoadFile(dirname+'\'+SR.Name, stateIni);

      // hledame dalsi soubory
      while (FindNext(SR) = 0) do
        if ((SR.Attr AND faDirectory) = 0) then
          Self.LoadFile(dirname+'\'+SR.Name, stateIni);

      SysUtils.FindClose(SR);
     end;

   Self.CreateIndex();
   HVTableData.LoadToTable();
  finally
    stateIni.Free();
  end;
end;

procedure THVDb.SaveData(const dirname: string);
var i: Integer;
begin
 Self.fLoksDir := dirname;

 for i := 0 to _MAX_ADDR-1 do
  begin
   if (Self.HVs[i] <> nil) then
    begin
     try
       Self.HVs[i].SaveData(Self.FilenameForLok(Self.HVs[i]));
     except
       on E: Exception do
        AppEvents.LogException(E, 'THVDb.SaveData '+IntToStr(i));
     end;
    end;//if <> nil
  end;//for i
end;

procedure THVDb.SaveState(const statefn: string);
var i: Integer;
    stateIni: TMemIniFile;
begin
 stateIni := TMemIniFile.Create(statefn);

 try
   for i := 0 to _MAX_ADDR-1 do
    begin
     if (Self.HVs[i] <> nil) then
      begin
       try
         Self.HVs[i].SaveState(stateIni);
       except
         on E: Exception do
          AppEvents.LogException(E, 'THVDb.SaveState '+IntToStr(i));
       end;
      end;//if <> nil
    end;//for i
 finally
  stateIni.UpdateFile();
  stateIni.Free();
 end;
end;

////////////////////////////////////////////////////////////////////////////////

function THVDb.Add(data: THVData; addr: Word; StanovisteA: THVStanoviste; area: TObject): THV;
var i, index: Integer;
    stav: THVStav;
begin
 if (addr > 9999) then
   raise EInvalidAddress.Create('Neplatná adresa lokomotivy ' + IntToStr(addr));
 if (Self.HVs[addr] <> nil) then
   raise ELocoExists.Create('Lokomotiva s adresou ' + IntToStr(addr) + ' již existuje');

 // pokud neexistuje, pridame ji

 stav.StanovisteA := StanovisteA;
 stav.traveled_forward := 0;
 stav.traveled_backward := 0;
 stav.stanice := (area as TArea);

 stav.train := -1;
 stav.ruc := false;

 stav.regulators := nil;
 stav.tokens     := nil;

 Self.HVs[addr] := THV.Create(addr, data, stav);

 // ------- update indexu: ------

 // najdeme nejblizsi spodni index
 index := -1;
 if (addr > 0) then
  begin
   for i := addr-1 downto 0 do
    begin
     if (Self.HVs[i] <> nil) then
      begin
       index := Self.HVs[i].index;
       break;
      end;
    end;
  end;//if addr > 0

 // nasemu HV priradime tento index + 1
 Self.HVs[addr].Index := index+1;

 // vsem HV nad nasim hv zvysime index o 1
 if (addr < _MAX_ADDR-1) then
  begin
   for i := addr+1 to _MAX_ADDR-1 do
     if (Self.HVs[i] <> nil) then
        Self.HVs[i].index := Self.HVs[i].index + 1;
  end;

 // aktualizujeme tabulky:
 HVTableData.AddHV(Self.HVs[addr].index, Self.HVs[addr]);

 Result := Self.HVs[addr];
end;

function THVDb.Add(panel_str: string; SenderOR: TObject): THV;
var HV: THV;
    index, i: Integer;
begin
 try
   HV := THV.Create(panel_str, (SenderOR as TArea));
 except
   on e: Exception do
    begin
     raise Exception.Create(e.Message);
     HV.Free();
     Exit();
    end;//on e: Exception
 end;

 if (Self[HV.addr] <> nil) then
  begin
   raise Exception.Create('HV '+IntToStr(HV.addr)+' již existuje');
   HV.Free();
   Exit();
  end;

 Self.HVs[HV.addr] := HV;

 // ------- update indexu: ------

 // najdeme nejblizsi spodni index
 index := -1;
 if (HV.addr > 0) then
  begin
   for i := HV.addr-1 downto 0 do
    begin
     if (Self.HVs[i] <> nil) then
      begin
       index := Self.HVs[i].index;
       break;
      end;
    end;
  end;//if addr > 0

 // nasemu HV priradime tento index + 1
 HV.Index := index+1;

 // vsem HV nad nasim hv zvysime index o 1
 if (HV.addr < _MAX_ADDR-1) then
  begin
   for i := HV.addr+1 to _MAX_ADDR-1 do
    if (Self.HVs[i] <> nil) then
      Self.HVs[i].index := Self.HVs[i].index + 1;
  end;//if

 // aktualizujeme tabulky:
 HVTableData.AddHV(HV.index, HV);

 Result := HV;
end;

procedure THVDb.Remove(addr: Word);
var i, index: Integer;
begin
 // hv neexistuje
 if (Self.HVs[addr] = nil) then
   raise ENoLoco.Create('Lokomotiva s touto adresou neexistuje!');
 if (Self.HVs[addr].Stav.train > -1) then
   raise ELocoOnTrain.Create('Lokomotiva je na soupravě!');
 if ((Self.HVs[addr].acquired) or (Self.HVs[addr].stolen)) then
   raise ELocoPrevzato.Create('Lokomotiva převzata do řízení počítače');

 index := Self.HVs[addr].index;
 FreeAndNil(Self.HVs[addr]);

 // smazat soubor
 SysUtils.DeleteFile(Self.FilenameForLok(addr));

 // ------- update indexu: ------

 // vsechny indexy od addr zmensime o 1:
 for i := addr to _MAX_ADDR-1 do
   if (Self.HVs[i] <> nil) then
     Self.HVs[i].index := Self.HVs[i].index - 1;

 // aktualizujeme tabulky:
 HVTableData.RemoveHV(index);
end;

////////////////////////////////////////////////////////////////////////////////

// spocita pocet hnacich vozidel
function THVDb.GetCnt(): Word;
var i: Integer;
begin
 Result := 0;
 for i := 0 to _MAX_ADDR-1 do
  if (Self.HVs[i] <> nil) then
    Result := Result + 1;
end;

////////////////////////////////////////////////////////////////////////////////

// vytvori index hnacich vozidel
// vola se jen pri nacteni souboru
// update indxu si zajistuji metody Add a remove trosku jinym algoritmem
//    (neni zapotrebi kontrolovat cele pole)
procedure THVDb.CreateIndex();
var i, index: Integer;
begin
 index := 0;
 for i := 0 to _MAX_ADDR-1 do
  begin
   if (Self.HVs[i] <> nil) then
    begin
     Self.HVs[i].Index := index;
     index := index + 1;
    end;
  end;//for i
end;

////////////////////////////////////////////////////////////////////////////////

procedure THVDb.RemoveRegulator(conn: TIDContext);
var i: Integer;
begin
 for i := 0 to _MAX_ADDR-1 do
   if (Assigned(Self.HVs[i])) then
     Self.HVs[i].RemoveRegulator(conn);
end;

////////////////////////////////////////////////////////////////////////////////

procedure THVDb.UpdateTokenTimeout();
var i: Integer;
begin
 for i := 0 to _MAX_ADDR-1 do
   if (Assigned(self.HVs[i])) then
     Self.HVs[i].UpdateTokenTimeout();
end;

////////////////////////////////////////////////////////////////////////////////

procedure THVDb.ClearAllStatistics();
var i: Integer;
begin
 for i := 0 to _MAX_ADDR-1 do
   if (Assigned(self.HVs[i])) then
     Self.HVs[i].ResetStats();
 HVTableData.reload := true;
 HVTableData.UpdateTable();
end;

////////////////////////////////////////////////////////////////////////////////

procedure THVDb.ExportStatistics(filename: string);
var f: TextFile;
    i: Integer;
begin
  AssignFile(f, filename);
  Rewrite(f);

  try
    WriteLn(f, 'adresa;nazev;majitel;najeto_metru_vpred;majeto_bloku_vpred;najeto_metru_vzad;najeto_bloku_vzad');

    for i := 0 to _MAX_ADDR-1 do
      if (Assigned(self.HVs[i])) then
        WriteLn(f, Self.HVs[i].ExportStats());
  finally
    CloseFile(f);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function THVDb.FilenameForLok(addr: Word): string;
begin
 Result := Self.loksDir + '\L_' + IntToStr(addr) + _FILE_SUFFIX;
end;

function THVDb.FilenameForLok(hv: THV): string;
begin
 Result := Self.FilenameForLok(hv.addr);
end;

////////////////////////////////////////////////////////////////////////////////

function THVDb.GetItem(index: Integer): THV;
begin
 Result := Self.HVs[index];
end;

procedure THVDb.CSReset();
var addr: Word;
begin
 Self.mAcquiring := false;
 Self.mReleasing := false;
 for addr := 0 to _MAX_ADDR-1 do
   if (HVDb[addr] <> nil) then
     HVDb[addr].CSReset();
end;

////////////////////////////////////////////////////////////////////////////////
// Trakce
////////////////////////////////////////////////////////////////////////////////

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

 data := Pointer(addr+1);
 try
   HVDb[addr].TrakceAcquire(TTrakce.Callback(Self.AcquiredOk, data), TTrakce.Callback(Self.AcquiredErr, data));
 except
   Self.AcquiredErr(Self, data);
 end;
end;

procedure THVDb.AcquiredErr(Sender: TObject; Data: Pointer);
begin
 Self.mAcquiring := false;
 TrakceI.Log(llErrors, 'ERR: LOKO '+ IntToStr(Word(data)) + ' se nepodařilo převzít');
 F_Main.LogStatus('LOKO: loko '+ IntToStr(Word(data)) + ' se nepodařilo převzít');
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

 data := Pointer(addr+1);
 try
   HVDb[addr].TrakceRelease(TTrakce.Callback(Self.ReleasedOk, data));
 except
   Self.ReleasedOk(Self, data);
 end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure THVDb.OnTLocoUpdate(Sender: TObject);
var addr: Word;
begin
  for addr := 0 to _MAX_ADDR-1 do
   begin
    if (Self.HVs[addr] = nil) then continue;

    try
      if ((Self.HVs[addr].stolen) and (not Self.HVs[addr].acquiring) and (not Self.HVs[addr].updating) and
          (Now > Self.HVs[addr].lastUpdated+EncodeTime(0, 0, _LOCO_UPDATE_TIME_MS div 1000, _LOCO_UPDATE_TIME_MS mod 1000))) then
        Self.HVs[addr].TrakceUpdateState(TTrakce.Callback(), TTrakce.Callback());

      Self.HVs[addr].UpdateTraveled(_LOCO_UPDATE_TIME_MS div 1000);
    except
      on E: Exception do
        AppEvents.LogException(E, 'HVDb.OnTLocoUpdate');
    end;
   end;

end;

////////////////////////////////////////////////////////////////////////////////

function THVDb.AnyAcquiredHVHasActiveFunc(func: string): Boolean;
var hv: THV;
begin
 for hv in Self.HVs do
  begin
   if ((hv <> nil) and (hv.acquired) and (hv.funcDict.ContainsKey(func)) and
       (hv.slotFunkce[hv.funcDict[func]])) then
     Exit(true);
  end;
 Result := false;
end;

function THVDb.AllAcquiredHVsHaveActiveFunc(func: string): Boolean;
var hv: THV;
begin
 for hv in Self.HVs do
  begin
   if ((hv <> nil) and (hv.acquired) and (hv.funcDict.ContainsKey(func)) and
       (not hv.slotFunkce[hv.funcDict[func]])) then
     Exit(false);
  end;
 Result := true;
end;

function THVDb.AnyHvToRestoreFunc(func: string): Boolean;
var hv: THV;
begin
 for hv in Self.HVs do
  begin
   if ((hv <> nil) and (hv.acquired) and (hv.funcDict.ContainsKey(func)) and
       (not hv.slotFunkce[hv.funcDict[func]]) and (hv.stavFunkce[hv.funcDict[func]])) then
     Exit(true);
  end;
 Result := false;
end;

////////////////////////////////////////////////////////////////////////////////

initialization
 HVDb := THVDb.Create();

finalization
 FreeAndNil(HVDb);

end.//unit
