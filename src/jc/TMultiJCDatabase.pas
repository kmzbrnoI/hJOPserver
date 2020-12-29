unit TMultiJCDatabase;

////////////////////////////////////////////////////////////////////////////////
// TRIDA TMultiJCDb
//  databaze slozenych jizdnich cest
////////////////////////////////////////////////////////////////////////////////

interface

uses TechnologieMultiJC, TBlok, IniFiles, SysUtils, Windows, IdContext,
      Generics.Collections, Classes, Generics.Defaults, TBlokNav;

type
  MutiJCExistsException = class(Exception);

  TMultiJCDb = class
   private
    JCs: TObjectList<TMultiJC>; // seznam slozenych jizdnich cest serazeny podle jejich id vzestupne
    JCsStartSignal: TObjectDictionary<TBlkSignal, TList<TMultiJC>>;


    ffilename: string;

     function GetJCCnt(): Word;
     function GetItem(index: Integer): TMultiJC;
     procedure FillJCsStartSignal();

   public

     constructor Create();
     destructor Destroy(); override;

     procedure LoadData(const filename: string);
     procedure SaveData(const filename: string);

     procedure Update();
     function GetJCByID(id: Integer): TMultiJC;
     function GetJCIndexByID(id: Integer): Integer;

     function StavJC(StartBlk, EndBlk: TBlk; SenderPnl: TIdContext; SenderOR: TObject; abAfter: Boolean): Boolean;
     function FindMJC(startNav: TBlkSignal; vb: TList<TObject>; endBlk: TBlk): TMultiJC;
     function IsAnyMJCWithPrefix(startNav: TBlkSignal; vb: TList<TObject>): Boolean;

     function Add(data: TMultiJCProp): TMultiJC;
     procedure Remove(index: Integer);

     procedure IDChanged(previousIndex: Integer);
     procedure NavChanged(Sender: TObject; origNav: TBlk);

     property filename: string read ffilename;

     property Items[index : Integer] : TMultiJC read GetItem; default;
     property Count: Word read GetJCCnt;

  end;

var
  MultiJCDb: TMultiJcDb;


implementation

uses Logging, GetSystems, TBloky, TBlokUsek, TOblRizeni, TCPServerOR,
      DataMultiJC, Zasobnik, TOblsRizeni, TechnologieJC, TJCDatabase, appEv;

////////////////////////////////////////////////////////////////////////////////
// TRIDA TMultiJCDb
//  databaze slozenych jizdnich cest
////////////////////////////////////////////////////////////////////////////////


constructor TMultiJCDb.Create();
begin
 inherited Create();
 Self.JCs := TObjectList<TMultiJC>.Create(TMultiJC.IdComparer());
 Self.JCsStartSignal := TObjectDictionary<TBlkSignal, TList<TMultiJC>>.Create();
end;//ctor

destructor TMultiJCDb.Destroy();
begin
 Self.JCs.Free();
 Self.JCsStartSignal.Free();
 inherited Destroy();
end;//dtor

////////////////////////////////////////////////////////////////////////////////

// load data from ini file
procedure TMultiJCDb.LoadData(const filename: string);
var ini: TMemIniFile;
    i: Integer;
    mJC: TMultiJC;
    sections: TStrings;
begin
 writelog('Načítám složené JC - '+filename, WR_DATA);

 Self.ffilename := filename;

 try
   ini := TMemIniFile.Create(filename, TEncoding.UTF8);
 except
   on E: Exception do
    begin
     AppEvents.LogException(E, 'Nacitam slozene JC: nelze otevrit soubor s reliefy');
     Exit();
    end;
 end;

 Self.JCs.Clear();

 sections := TStringList.Create();
 ini.ReadSections(sections);
 for i := 0 to sections.Count-1 do
  begin
   mJC := TMultiJC.Create();
   try
     mJC.LoadData(ini, sections[i]);
   except
     on e: Exception do
      begin
       mJC.Free();
       AppEvents.LogException(E, 'Chyba při načítání složené JC '+mJC.Nazev);
       continue;
      end;
   end;

   // pridavame klice do seznamu v poradi jako v souboru
   Self.JCs.Add(mJC);
  end;//for i

 // a seznam seradime
 Self.JCs.Sort();

 // zkontrolujeme duplicity
 i := 0;
 while (i < Self.JCs.Count-1) do
  begin
   if (Self.JCs[i].id = Self.JCs[i+1].id) then
    begin
     writelog('WARNING: duplicita primárního klíče mJC ('+IntToStr(Self.JCs[i].id)+') - přeskakuji', WR_ERROR);
     Self.JCs.Delete(i+1);
    end;
   Inc(i);
  end;

 ini.Free();
 sections.Free();
 writelog('Načteno '+IntToStr(Self.JCs.Count)+' složených JC', WR_DATA);

 MultiJCTableData.LoadToTable();
 Self.FillJCsStartSignal();
end;

// save data to ini file:
procedure TMultiJCDb.SaveData(const filename: string);
var ini: TMemIniFile;
    i: Integer;
begin
 writelog('Ukládám složené JC - '+filename, WR_DATA);

 try
   DeleteFile(PChar(filename));
   ini := TMemIniFile.Create(filename, TEncoding.UTF8);
 except
   on E: Exception do
    begin
     AppEvents.LogException(E, 'Ukladam slozene JC: nelze otevrit soubor s reliefy');
     Exit();
    end;
 end;

 for i := 0 to Self.JCs.Count-1 do
   Self.JCs[i].SaveData(ini);

 ini.UpdateFile();
 ini.Free();

 writelog('Složené JC uloženy', WR_DATA);
end;

////////////////////////////////////////////////////////////////////////////////
// Vyhledavame slozenou JC binarnim vyhledavanim.

function TMultiJCDb.GetJCIndexByID(id: Integer): Integer;
var left, right, mid: Integer;
begin
 left := 0;
 right := Self.JCs.Count-1;

 while (left <= right) do
  begin
   mid := (left + right) div 2;
   if (Self.JCs[mid].id = id) then Exit(mid);

   if (Self.JCs[mid].id > id) then
     right := mid - 1
   else
     left := mid + 1;
  end;

 Result := -1;
end;

function TMultiJCDb.GetJCByID(id: Integer): TMultiJC;
var index: Integer;
begin
 index := Self.GetJCIndexByID(id);
 if (index < 0) then
   Result := nil
 else
   Result := Self.JCs[index];
end;

////////////////////////////////////////////////////////////////////////////////

procedure TMultiJCDb.Update();
var i: Integer;
begin
 for i := 0 to Self.JCs.Count-1 do
  begin
   try
     if (Self.JCs[i].stav.JCIndex > -1) then
       Self.JCs[i].UpdateStaveni();
   except
    on E: Exception do
     begin
      if (not log_err_flag) then
        AppEvents.LogException(E, 'JC '+Self.JCs[i].nazev + ' update error, rusim staveni');
      Self.JCs[i].RusStaveni();
     end;
   end;//except
  end;//for i }
end;

////////////////////////////////////////////////////////////////////////////////

function TMultiJCDb.GetJCCnt(): Word;
begin
 Result := Self.JCs.Count;
end;

////////////////////////////////////////////////////////////////////////////////

function TMultiJCDb.FindMJC(startNav: TBlkSignal; vb: TList<TObject>; endBlk: TBlk): TMultiJC;
var mjc: TMultiJC;
begin
 if (not Self.JCsStartSignal.ContainsKey(startNav)) then
   Exit(nil);

 for mjc in Self.JCsStartSignal[startNav] do
   if (mjc.Match(startNav, vb, endBlk)) then
     Exit(mjc);

 Result := nil;
end;

////////////////////////////////////////////////////////////////////////////////

function TMultiJCDb.StavJC(StartBlk, EndBlk: TBlk; SenderPnl: TIdContext; SenderOR: TObject; abAfter: Boolean): Boolean;
var mJC: TMultiJC;
    j: Integer;
begin
 mJC := Self.FindMJC(startBlk as TBlkSignal, (SenderOR as TOR).vb, endBlk);
 Result := (mJC <> nil);

 if (mJC <> nil) then
  begin
   if ((SenderOR as TOR).stack.volba = TORStackVolba.VZ) then
    begin
     // VZ -> pridame do zasobniku postupne vsechny jizdni cesty
     for j := 0 to mJC.data.JCs.Count-1 do
       (SenderOR as TOR).stack.AddJC(JCDb.GetJCByID(mJC.data.JCs[j]), SenderPnl, false, abAfter);

     (StartBlk as TBlkSignal).selected := TBlkSignalSelection.none;
     (EndBlk as TBlkUsek).KonecJC := TZaver.no;
     (SenderOR as TOR).ClearVb();
    end else begin
     mJC.StavJC(SenderPnl, SenderOR);
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TMultiJCDb.Add(data: TMultiJCProp): TMultiJC;
var mJC: TMultiJC;
    pos: Integer;
begin
 if (Self.GetJCByID(data.id) <> nil) then
   raise MutiJCExistsException.Create('Složená JC s ID '+IntToStr(data.id)+' již existuje');

 // linearne vyhledame pozici pro novou slozenou JC
 pos := 0;
 while ((pos < Self.JCs.Count) and (data.id > Self.JCs[pos].id)) do Inc(pos);

 mJC := TMultiJC.Create(data);
 Self.JCs.Insert(pos, mJC);

 Self.NavChanged(mJC, nil);
 MultiJCTableData.AddJC(pos);
 Result := mJC;
end;

procedure TMultiJCDb.Remove(index: Integer);
begin
 if ((index < Self.JCs.Count) and (not Self.JCs[index].staveni)) then
  begin
   if (Self.JCs[index].StartNav <> nil) then
     if (Self.JCsStartSignal.ContainsKey(Self.JCs[index].StartNav())) then
       Self.JCsStartSignal[Self.JCs[index].StartNav()].Remove(Self.JCs[index]);

   Self.JCs.Delete(index);
   MultiJCTableData.RemoveJC(index);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TMultiJCDb.GetItem(index: Integer): TMultiJC;
begin
 if ((index < 0) or (index >= Self.JCs.Count)) then
   Result := nil
 else
   Result := Self.JCs[index];
end;

////////////////////////////////////////////////////////////////////////////////

procedure TMultiJCDb.IDChanged(previousIndex: Integer);
var pos: Integer;
    tmp: TMultiJC;
begin
 tmp := Self.JCs[previousIndex];
 if (((previousIndex = 0) or (Self.JCs[previousIndex-1].id <= Self.JCs[previousIndex].id)) and
    ((previousIndex = Self.JCs.Count-1) or (Self.JCs[previousIndex].id <= Self.JCs[previousIndex+1].id))) then Exit();

 Self.JCs.OwnsObjects := false;
 Self.JCs.Delete(previousIndex);
 Self.JCs.OwnsObjects := true;
 MultiJCTableData.RemoveJC(previousIndex);

 // linearne vyhledame pozici pro novou slozenou JC
 pos := 0;
 while ((pos < Self.JCs.Count) and (tmp.id > Self.JCs[pos].id)) do Inc(pos);

 Self.JCs.Insert(pos, tmp);
 MultiJCTableData.AddJC(pos);
end;

////////////////////////////////////////////////////////////////////////////////

function TMultiJCDb.IsAnyMJCWithPrefix(startNav: TBlkSignal; vb: TList<TObject>): Boolean;
var mjc: TMultiJC;
    jc: TJC;
    j: Integer;
    error: Boolean;
begin
 // startNav musi mit navolenou volbu, aby tato funkce fungovala.
 if (not Self.JCsStartSignal.ContainsKey(startNav)) then
   Exit(False);

 for mjc in Self.JCsStartSignal[startNav] do
  begin
   if (mjc.data.JCs.Count < 2) then continue;

   jc := JCDb.GetJCByID(mjc.data.JCs[0]);
   if (JC = nil) then continue;
   if (Integer(startNav.selected) <> Integer(JC.typ)) then
     continue;

   if (JC.data.NavestidloBlok <> startNav.id) then
     continue;

   // kontrola variantnich bodu
   if (vb.Count > mjc.data.vb.Count) then continue;

   error := false;
   for j := 0 to vb.Count-1 do
     if (mjc.data.vb[j] <> (vb[j] as TBlk).id) then
       error := true;
   if (error) then continue;

   Exit(true);
  end;

 Result := false;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TMultiJCDb.FillJCsStartSignal();
var mJC: TMultiJC;
begin
 Self.JCsStartSignal.Clear();
 for mJC in Self.JCs do
  begin
   if (mJC.StartNav() <> nil) then
    begin
     if (not Self.JCsStartSignal.ContainsKey(mJC.StartNav())) then
       Self.JCsStartSignal.Add(mJC.StartNav(), TList<TMultiJC>.Create());
     Self.JCsStartSignal[mJC.StartNav()].Add(mJC);
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TMultiJCDb.NavChanged(Sender: TObject; origNav: TBlk);
var signal: TBlkSignal;
    mJC: TMultiJC;
begin
 signal := origNav as TBlkSignal;
 mJC := Sender as TMultiJC;

 if (origNav <> nil) then
  begin
   if (Self.JCsStartSignal.ContainsKey(signal)) then
     if (Self.JCsStartSignal[signal].Contains(mJC)) then
       Self.JCsStartSignal[signal].Remove(mJC);
  end;

 if (mJC.data.JCs.Count > 0) then
  begin
   if (mJC.StartNav() <> nil) then
    begin
     if (not Self.JCsStartSignal.ContainsKey(mJC.StartNav())) then
       Self.JCsStartSignal.Add(mJC.StartNav(), TList<TMultiJC>.Create());
     Self.JCsStartSignal[mJC.StartNav()].Add(mJC);
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

initialization
  MultiJCDb := TMultiJCDb.Create();

finalization
  FreeAndNil(MultiJCDb);

end.//unit
