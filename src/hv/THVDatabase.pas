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

uses SysUtils, THnaciVozidlo, Classes, IdContext, IniFiles, Windows;

const
 _MAX_ADDR = 10000;
 _DEFAULT_OR = 0;
 _FILE_SUFFIX = '.2lok';

type

  THVArray = array [0.._MAX_ADDR-1] of THV;

  ENoLoco = class(Exception);
  ELocoOnSpr = class(Exception);
  ELocoPrevzato = class(Exception);
  EInvalidAddress = class(Exception);
  ELocoExists = class(Exception);

  THVDb = class

   private
    // index odpovida adrese
    // jednu adresu muze mit pouze jedno vozidlo v Db
    HVs:THVArray;

    fdefault_or:Integer;
    fLoksDir:string;

     procedure Clear();
     procedure LoadFile(filename:string; stateini:TMemIniFile);

     function GetCnt():Word;

     procedure CreateIndex();
     function GetItem(index:Integer):THV;

   public

     constructor Create();
     destructor Destroy(); override;

     procedure LoadFromDir(const dirname:string; const statefn:string);
     procedure SaveData(const dirname:string);
     procedure SaveState(const statefn:string);

     procedure Add(data:THVData; addr:Word; StanovisteA:THVStanoviste; OblR:TObject); overload;
     procedure Add(panel_str:string; SenderOR:TObject); overload;
     procedure Remove(addr:Word);

     procedure RemoveRegulator(conn:TIDContext);

     procedure ClearAllStatistics();
     procedure ExportStatistics(filename:string);

     procedure UpdateTokenTimeout();
     function FilenameForLok(addr:Word):string; overload;
     function FilenameForLok(hv:THV):string; overload;

     property cnt:Word read GetCnt;              // vypocet tady tohoto trva celkem dlouho, pouzivat obezretne !
     property HVozidla:THVArray read HVs;
     property default_or:Integer read fdefault_or write fdefault_or;
     property loksDir:string read fLoksDir;

     property Items[index : integer] : THV read GetItem; default;

  end;//THVDb

var
  HVDb:THVDb;

implementation

uses fSettings, fMain, DataHV, TOblRizeni, appEv;

////////////////////////////////////////////////////////////////////////////////

constructor THVDb.Create();
var i: Integer;
begin
 inherited Create();

 Self.fdefault_or := _DEFAULT_OR;

 // ukazatele nastavit na nil
 for i := 0 to _MAX_ADDR-1 do
   Self.HVs[i] := nil;
end;//ctor

destructor THVDb.Destroy();
begin
 Self.Clear();

 inherited Destroy();
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure THVDb.Clear();
var i:Integer;
begin
 for i := 0 to _MAX_ADDR-1 do
   if (Self.HVs[i] <> nil) then
     FreeAndNil(Self.HVs[i]);
end;

////////////////////////////////////////////////////////////////////////////////

procedure THVDb.LoadFile(filename:string; stateini:TMemIniFile);
var aHV:THV;
    ini:TMemIniFile;
    sections:TStrings;
    sect:string;
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
       on E:Exception do
         AppEvents.LogException(E, 'Chyba pri nacitani souboru loko : '+filename + ', sekce '+sect);
     end;

     if (aHV = nil) then continue;     

     if (Self.HVs[aHV.adresa] <> nil) then
      begin
       FreeAndNil(aHv);
      end else begin
       Self.HVs[aHV.adresa] := aHV;
      end;
    end;//for sect in sections
 except
  on E:Exception do
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

procedure THVDb.LoadFromDir(const dirname:string; const statefn:string);
var SR:TSearchRec;
    stateIni:TMemIniFile;
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

procedure THVDb.SaveData(const dirname:string);
var i:Integer;
begin
 Self.fLoksDir := dirname;

 for i := 0 to _MAX_ADDR-1 do
  begin
   if (Self.HVs[i] <> nil) then
    begin
     try
       Self.HVs[i].SaveData(Self.FilenameForLok(Self.HVs[i]));
     except
       on E:Exception do
        AppEvents.LogException(E, 'THVDb.SaveData '+IntToStr(i));
     end;
    end;//if <> nil
  end;//for i
end;

procedure THVDb.SaveState(const statefn:string);
var i:Integer;
    stateIni:TMemIniFile;
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
         on E:Exception do
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

procedure THVDb.Add(data:THVData; addr:Word; StanovisteA:THVStanoviste; OblR:TObject);
var i, index:Integer;
    stav:THVStav;
begin
 if (addr > 9999) then
   raise EInvalidAddress.Create('Neplatná adresa lokomotivy ' + IntToStr(addr));
 if (Self.HVs[addr] <> nil) then
   raise ELocoExists.Create('Lokomotiva s adresou ' + IntToStr(addr) + ' již existuje');

 // pokud neexistuje, pridame ji

 stav.StanovisteA := StanovisteA;
 stav.najeto_vpred.Metru := 0;
 stav.najeto_vpred.Bloku := 0;
 stav.najeto_vzad.Metru  := 0;
 stav.najeto_vzad.Bloku  := 0;
 stav.stanice            := (OblR as TOR);

 stav.souprava := -1;
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
 HVTableData.AddHV(Self.HVs[addr].index, Self.HVs[addr])
end;

procedure THVDb.Add(panel_str:string; SenderOR:TObject);
var HV:THV;
    index, i:Integer;
begin
 try
   HV := THV.Create(panel_str, (SenderOR as TOR));
 except
   on e:Exception do
    begin
     raise Exception.Create(e.Message);
     HV.Free();
     Exit();
    end;//on e:Exception
 end;

 if (Self.HVozidla[HV.adresa] <> nil) then
  begin
   raise Exception.Create('HV '+IntToStr(HV.adresa)+' již existuje');
   HV.Free();
   Exit();
  end;

 Self.HVs[HV.adresa] := HV;

 // ------- update indexu: ------

 // najdeme nejblizsi spodni index
 index := -1;
 if (HV.adresa > 0) then
  begin
   for i := HV.adresa-1 downto 0 do
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
 if (HV.adresa < _MAX_ADDR-1) then
  begin
   for i := HV.adresa+1 to _MAX_ADDR-1 do
    if (Self.HVs[i] <> nil) then
      Self.HVs[i].index := Self.HVs[i].index + 1;
  end;//if

 // aktualizujeme tabulky:
 HVTableData.AddHV(HV.index, HV);
end;

procedure THVDb.Remove(addr:Word);
var i, index:Integer;
begin
 // hv neexistuje
 if (Self.HVs[addr] = nil) then
   raise ENoLoco.Create('Lokomotiva s touto adresou neexistuje!');
 if (Self.HVs[addr].Stav.souprava > -1) then
   raise ELocoOnSpr.Create('Lokomotiva je na soupravì!');
 if ((Self.HVs[addr].Slot.prevzato) or (Self.HVs[addr].Slot.stolen)) then
   raise ELocoPrevzato.Create('Lokomotiva pøevzata do øízení poèítaèe');

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
function THVDb.GetCnt():Word;
var i:Integer;
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
var i, index:Integer;
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

procedure THVDb.RemoveRegulator(conn:TIDContext);
var i:Integer;
begin
 for i := 0 to _MAX_ADDR-1 do
   if (Assigned(Self.HVs[i])) then
     Self.HVs[i].RemoveRegulator(conn);
end;

////////////////////////////////////////////////////////////////////////////////

procedure THVDb.UpdateTokenTimeout();
var i:Integer;
begin
 for i := 0 to _MAX_ADDR-1 do
   if (Assigned(self.HVs[i])) then
     Self.HVs[i].UpdateTokenTimeout();
end;

////////////////////////////////////////////////////////////////////////////////

procedure THVDb.ClearAllStatistics();
var i:Integer;
begin
 for i := 0 to _MAX_ADDR-1 do
   if (Assigned(self.HVs[i])) then
     Self.HVs[i].RemoveStats();
 HVTableData.reload := true;
 HVTableData.UpdateTable();
end;

////////////////////////////////////////////////////////////////////////////////

procedure THVDb.ExportStatistics(filename:string);
var f:TextFile;
    i:Integer;
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

function THVDb.FilenameForLok(addr:Word):string;
begin
 Result := Self.loksDir + '\L_' + IntToStr(addr) + _FILE_SUFFIX;
end;

function THVDb.FilenameForLok(hv:THV):string;
begin
 Result := Self.FilenameForLok(hv.adresa);
end;

////////////////////////////////////////////////////////////////////////////////

function THVDb.GetItem(index:Integer):THV;
begin
 Result := Self.HVs[index];
end;

////////////////////////////////////////////////////////////////////////////////

initialization
 HVDb := THVDb.Create();

finalization
 FreeAndNil(HVDb);

end.//unit
