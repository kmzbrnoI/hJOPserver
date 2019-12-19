unit TMultiJCDatabase;

////////////////////////////////////////////////////////////////////////////////
// TRIDA TMultiJCDb
//  databaze slozenych jizdnich cest
////////////////////////////////////////////////////////////////////////////////

interface

uses TechnologieMultiJC, TBlok, IniFiles, SysUtils, Windows, IdContext,
      Generics.Collections, Classes, Generics.Defaults;

type
  MutiJCExistsException = class(Exception);

  TMultiJCDb = class
   private
    JCs:TObjectList<TMultiJC>;                                                        // seznam slozenych jizdnich cest serazeny podle jejich id vzestupne

    ffilename:string;

     function GetJCCnt():Word;
     function GetItem(index:Integer):TMultiJC;

   public

     constructor Create();
     destructor Destroy(); override;

     procedure LoadData(const filename:string);
     procedure SaveData(const filename:string);

     procedure Update();
     function GetJCByID(id:Integer):TMultiJC;
     function GetJCIndexByID(id:Integer):Integer;

     function StavJC(StartBlk,EndBlk:TBlk; SenderPnl:TIdContext; SenderOR:TObject):boolean;   // vraci true, pokud nasel prislusnou cestu

     function Add(data:TMultiJCProp):TMultiJC;
     procedure Remove(index:Integer);
     procedure IDChanged(previousIndex:Integer);

     property filename:string read ffilename;

     property Items[index : Integer] : TMultiJC read GetItem; default;
     property Count:Word read GetJCCnt;

  end;

var
  MultiJCDb:TMultiJcDb;


implementation

uses Logging, GetSystems, TBloky, TBlokNav, TBlokUsek, TOblRizeni, TCPServerOR,
      DataMultiJC, Zasobnik, TOblsRizeni, TechnologieJC, TJCDatabase, appEv;

////////////////////////////////////////////////////////////////////////////////
// TRIDA TMultiJCDb
//  databaze slozenych jizdnich cest
////////////////////////////////////////////////////////////////////////////////


constructor TMultiJCDb.Create();
begin
 inherited Create();
 Self.JCs := TObjectList<TMultiJC>.Create(TMultiJC.IdComparer());
end;//ctor

destructor TMultiJCDb.Destroy();
begin
 Self.JCs.Free();
 inherited Destroy();
end;//dtor

////////////////////////////////////////////////////////////////////////////////

// load data from ini file
procedure TMultiJCDb.LoadData(const filename:string);
var ini:TMemIniFile;
    i:Integer;
    mJC:TMultiJC;
    sections:TStrings;
begin
 writelog('Načítám složené JC - '+filename, WR_DATA);

 Self.ffilename := filename;

 try
   ini := TMemIniFile.Create(filename, TEncoding.UTF8);
 except
   on E:Exception do
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
     on e:Exception do
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


 ini.Free;
 sections.Free();
 writelog('Načteno '+IntToStr(Self.JCs.Count)+' složených JC', WR_DATA);

 MultiJCTableData.LoadToTable();
end;

// save data to ini file:
procedure TMultiJCDb.SaveData(const filename:string);
var ini:TMemIniFile;
    i:Integer;
begin
 writelog('Ukládám složené JC - '+filename, WR_DATA);

 try
   DeleteFile(PChar(filename));
   ini := TMemIniFile.Create(filename, TEncoding.UTF8);
 except
   on E:Exception do
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

function TMultiJCDb.GetJCIndexByID(id:Integer):Integer;
var left, right, mid:Integer;
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

function TMultiJCDb.GetJCByID(id:Integer):TMultiJC;
var index:Integer;
begin
 index := Self.GetJCIndexByID(id);
 if (index < 0) then
   Result := nil
 else
   Result := Self.JCs[index];
end;

////////////////////////////////////////////////////////////////////////////////

procedure TMultiJCDb.Update();
var i:Integer;
begin
 if (not GetFunctions.GetSystemStart) then Exit;

 for i := 0 to Self.JCs.Count-1 do
  begin
   try
     if (Self.JCs[i].stav.JCIndex > -1) then
       Self.JCs[i].UpdateStaveni();
   except
    on E:Exception do
     begin
      if (not log_err_flag) then
        AppEvents.LogException(E, 'JC '+Self.JCs[i].nazev + ' update error, rusim staveni');
      Self.JCs[i].RusStaveni();
     end;
   end;//except
  end;//for i }
end;

////////////////////////////////////////////////////////////////////////////////

function TMultiJCDb.GetJCCnt():Word;
begin
 Result := Self.JCs.Count;
end;

////////////////////////////////////////////////////////////////////////////////

function TMultiJCDb.StavJC(StartBlk,EndBlk:TBlk; SenderPnl:TIdContext; SenderOR:TObject):boolean;
var i, j:Integer;
    Blk:TBlk;
    JC:TJC;
    flag:boolean;
begin
 for i := 0 to Self.JCs.Count-1 do
  begin
   if (Self.JCs[i].data.JCs.Count < 2) then continue;

   JC := JCDb.GetJCByID(Self.JCs[i].data.JCs[0]);
   if (JC = nil) then continue;

   Blky.GetBlkByID(JC.data.NavestidloBlok, Blk);
   if (Blk <> StartBlk) then continue;

   // posledni blok musi byt poseldni blok posledni jizdni cesty
   JC := JCDb.GetJCByID(Self.JCs[i].data.JCs[Self.JCs[i].data.JCs.Count-1]);
   Blky.GetBlkByID(JC.data.Useky[JC.data.Useky.Count-1], Blk);
   if (Blk <> EndBlk) then continue;

   // kontrola variantnich bodu
   flag := true;
   if (Self.JCs[i].data.vb.Count <> (SenderOR as TOR).vb.Count) then continue;
   for j := 0 to Self.JCs[i].data.vb.Count-1 do
     if (Self.JCs[i].data.vb[j] <> ((SenderOR as TOR).vb[j] as TBlk).id) then flag := false;
   if (not flag) then continue;

   JC := JCDb.GetJCByID(Self.JCs[i].data.JCs[0]);
   if ((Integer((StartBlk as TBlkNav).ZacatekVolba) = Integer(JC.data.TypCesty)) or
      (((StartBlk as TBlkNav).ZacatekVolba = TBlkNavVolba.NC) and (JC.data.TypCesty = TJCType.vlak))) then
    begin
     // nasli jsme jizdni cestu, kterou hledame

     // kontrola existence vsech jizdnich cest v dane slozene jizdni ceste
     for j := 0 to Self.JCs[i].data.JCs.Count-1 do
      if (JCDb.GetJCByID(Self.JCs[i].data.JCs[j]) = nil) then Exit(false);

     if ((SenderOR as TOR).stack.volba = TORStackVolba.VZ) then
      begin
       // VZ -> pridame do zasobniku postupne vsechny jizdni cesty
       for j := 0 to Self.JCs[i].data.JCs.Count-1 do
         (SenderOR as TOR).stack.AddJC(JCDb.GetJCByID(Self.JCs[i].data.JCs[j]), SenderPnl, false);

       (StartBlk as TBlkNav).ZacatekVolba := TBlkNavVOlba.none;
       (EndBlk as TBlkUsek).KonecJC        := TZaver.no;
       (SenderOR as TOR).ClearVb();
      end else begin
       Self.JCs[i].StavJC(SenderPnl, SenderOR);
      end;

     Exit(true);
    end;
  end;//for i

 Result := false;
end;

////////////////////////////////////////////////////////////////////////////////

function TMultiJCDb.Add(data:TMultiJCProp):TMultiJC;
var mJC:TMultiJC;
    pos:Integer;
begin
 if (Self.GetJCByID(data.id) <> nil) then
   raise MutiJCExistsException.Create('Složená JC s ID '+IntToStr(data.id)+' již existuje');

 // linearne vyhledame pozici pro novou slozenou JC
 pos := 0;
 while ((pos < Self.JCs.Count) and (data.id > Self.JCs[pos].id)) do Inc(pos);

 mJC := TMultiJC.Create(data);
 Self.JCs.Insert(pos, mJC);
 MultiJCTableData.AddJC(pos);
 Result := mJC;
end;

procedure TMultiJCDb.Remove(index:Integer);
begin
 if ((index < Self.JCs.Count) and (not Self.JCs[index].staveni)) then
  begin
   Self.JCs.Delete(index);
   MultiJCTableData.RemoveJC(index);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TMultiJCDb.GetItem(index:Integer):TMultiJC;
begin
 if ((index < 0) or (index >= Self.JCs.Count)) then
   Result := nil
 else
   Result := Self.JCs[index];
end;

////////////////////////////////////////////////////////////////////////////////

procedure TMultiJCDb.IDChanged(previousIndex:Integer);
var pos:Integer;
    tmp:TMultiJC;
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

initialization
  MultiJCDb := TMultiJCDb.Create();

finalization
  FreeAndNil(MultiJCDb);

end.//unit
