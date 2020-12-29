﻿unit TJCDatabase;

{
  TJCDb je databaze jizdnich cest.
}

interface

uses TechnologieJC, TBlock, IniFiles, SysUtils, Windows, IdContext,
      Generics.Collections, Classes, IBUtils, BlockDb, TBlockSignal;

type
  EJCIdAlreadyExists = class(Exception);

  TJCDb = class
   private
    JCs: TObjectList<TJC>;
    JCsStartSignal: TObjectDictionary<TBlkSignal, TList<TJC>>;

    ffilename: string;

     function GetCount(): Word;
     function GetItem(i: Integer): TJC;
     function FindPlaceForNewJC(id: Integer): Integer;
     procedure FillJCsStartSignal();

     procedure JCOnIDChanged(Sender: TObject);
     procedure JCOnNavChanged(Sender: TObject; origNav: TBlk);

   public

     constructor Create();
     destructor Destroy(); override;

     procedure LoadData(const filename: string);
     procedure SaveData(const filename: string);
     procedure UpdateJCIndexes();

     procedure Update();
     procedure StavJC(StartBlk, EndBlk: TBlk; SenderPnl: TIdContext; SenderOR: TObject; abAfter: Boolean);

     function AddJC(JCdata: TJCprop): TJC;
     procedure RemoveJC(index: Integer);

     function GetJCByIndex(index: Integer): TJC;
     function GetJCIndex(id: Integer): Integer;
     function GetJCByID(id: integer): TJC;

     // najde JC, ktera je postavena, ci prave stavena !
     function FindJC(NavestidloBlokID: Integer; Staveni: Boolean = false): TJC; overload;
     function FindOnlyStaveniJC(NavestidloBlokID: Integer): TJC;
     function IsJC(id: Integer; ignore_index: Integer = -1): Boolean;

     //pouzivano pri vypadku polohy vyhybky postavene jizdni cesty
     function FindPostavenaJCWithVyhybka(vyh_id: Integer): TList<TJC>;
     function FindPostavenaJCWithUsek(usek_id: Integer): TJC;
     function FindPostavenaJCWithPrj(blk_id: Integer): TList<TJC>;
     function FindPostavenaJCWithTrat(trat_id: Integer): TJC;
     function FindPostavenaJCWithZamek(zam_id: Integer): TList<TJC>;

     // jakmile dojde ke zmene navesti navestidla nav, muze dojit k ovlivneni nejakeho jineho navestidla
     // tato fce zajisti, ze k ovlivneni dojde
     procedure CheckNNavaznost(signal: TBlkSignal);

     procedure RusAllJC();
     procedure RusJC(Blk: TBlk);     // rusi cestu, ve ktere je zadany blok

     function IsAnyJC(signal: TBlkSignal): Boolean;
     function IsAnyVC(signal: TBlkSignal): Boolean;
     function IsAnyPC(signal: TBlkSignal): Boolean;

     function IsAnyJCAvailable(signal: TBlkSignal; typ: TJCType): Boolean;
     function IsAnyVCAvailable(signal: TBlkSignal): Boolean;
     function IsAnyPCAvailable(signal: TBlkSignal): Boolean;

     function FindJC(startNav: TBlkSignal; vb: TList<TObject>; EndBlk: TBlk): TJC; overload;
     function IsAnyJCWithPrefix(startNav: TBlkSignal; vb: TList<TObject>): Boolean;

     property Count: Word read GetCount;
     property filename: string read ffilename;

     function GetEnumerator(): TEnumerator<TJC>;
     property Items[index : integer] : TJC read GetItem; default;


  end;

var
  JCDb: TJcDb;


implementation

uses Logging, GetSystems, TBlockTrack, TOblRizeni, TCPServerOR, TBlockRailway,
      DataJC, Zasobnik, TOblsRizeni, TMultiJCDatabase, appEv, TBlockTurnout,
      TBlockRailwayTrack;

////////////////////////////////////////////////////////////////////////////////
// TRIDA TJCDb
//  databaze jizdnich cest
////////////////////////////////////////////////////////////////////////////////


constructor TJCDb.Create();
begin
 inherited;
 Self.JCs := TObjectList<TJC>.Create();
 Self.JCsStartSignal := TObjectDictionary<TBlkSignal, TList<TJC>>.Create();
end;//ctor

destructor TJCDb.Destroy();
begin
 Self.JCs.Free();
 Self.JCsStartSignal.Free();
 inherited;
end;//dtor

////////////////////////////////////////////////////////////////////////////////

// load data from ini file
procedure TJCDb.LoadData(const filename: string);
var ini: TMemIniFile;
    i: Integer;
    sections: TStrings;
    JC: TJC;
begin
 writelog('Načítám JC - '+filename, WR_DATA);

 Self.ffilename := filename;
 ini := TMemIniFile.Create(filename, TEncoding.UTF8);

 Self.JCs.Clear();
 sections := TStringList.Create();
 try
   ini.ReadSections(sections);

   for i := 0 to sections.Count-1 do
    begin
     JC := TJC.Create();
     try
       JC.index := i;
       JC.OnIdChanged := Self.JCOnIDChanged;
       JC.OnNavChanged := Self.JCOnNavChanged;
       JC.LoadData(ini, sections[i]);
       Self.JCs.Insert(Self.FindPlaceForNewJC(JC.id), JC);
     except
       on E: Exception do
        begin
         AppEvents.LogException(E, 'JC '+JC.name+' se nepodařilo načíst');
         JC.Free();
        end;
     end;
    end;//for i

   Self.UpdateJCIndexes();
 finally
   ini.Free;
   sections.Free();
 end;

 Self.FillJCsStartSignal();
 writelog('Načteno '+IntToStr(Self.JCs.Count)+' JC', WR_DATA);
end;

// save data to ini file:
procedure TJCDb.SaveData(const filename: string);
var ini: TMemIniFile;
    JC: TJC;
begin
 writelog('Ukládám JC - '+filename, WR_DATA);

 DeleteFile(PChar(filename));
 ini := TMemIniFile.Create(filename, TEncoding.UTF8);
 try
   for JC in Self.JCs do
     JC.SaveData(ini, IntToStr(JC.id));

   ini.UpdateFile();
 finally
   ini.Free();
 end;

 writelog('JC uloženy', WR_DATA);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TJCDb.Update();
var JC: TJC;
begin
 for JC in Self.JCs do
  begin
   try
     if (JC.stav.RozpadBlok > -5) then
       JC.UsekyRusJC();
     if (JC.stav.RozpadBlok = -6) then
       JC.UsekyRusNC();


     if ((JC.Staveni) or (JC.Krok = _JC_KROK_CEKANI_POSLEDNI_USEK)) then
      begin
       JC.UpdateStaveni();
       JC.UpdateTimeOut();
      end;
   except
    on E: Exception do
     begin
      if (not log_err_flag) then
       AppEvents.LogException(E, 'JC '+JC.name + ' update error');
      if (JC.staveni) then
        JC.CancelStaveni('Vyjímka', true)
      else
        JC.RusJCWithoutBlk();
     end;
   end;//except
  end;//for i
end;

////////////////////////////////////////////////////////////////////////////////

function TJCDb.GetJCByIndex(index: Integer): TJC;
begin
 if ((index < 0) or (index >= Self.JCs.Count)) then
  begin
   Result := nil;
   Exit;
  end;

 Result := Self.JCs[index];
end;

////////////////////////////////////////////////////////////////////////////////

function TJCDb.FindJC(startNav: TBlkSignal; vb: TList<TObject>; endBlk: TBlk): TJC;
var jc: TJC;
    blk: TBlk;
    j: Integer;
    match: Boolean;
begin
 if (not Self.JCsStartSignal.ContainsKey(startNav)) then
   Exit(nil);

 for jc in Self.JCsStartSignal[startNav] do
  begin
   if (JC.navestidlo <> startNav) then continue;

   Blocks.GetBlkByID(jc.data.Useky[jc.data.Useky.Count-1], blk);
   if (blk <> endBlk) then continue;

   if ((Integer(startNav.selected) = Integer(jc.typ)) or
      ((startNav.selected = TBlkSignalSelection.NC) and (jc.typ = TJCType.vlak)) or
      ((startNav.selected = TBlkSignalSelection.PP) and (jc.typ = TJCType.posun))) then
    begin
     // kontrola variantnich bodu:
     if (jc.data.vb.Count <> vb.Count) then continue;
     match := true;
     for j := 0 to jc.data.vb.Count-1 do
       if (jc.data.vb[j] <> (vb[j] as TBlk).id) then
         match := false;
     if (not match) then continue;

     Exit(jc);
    end;
  end;

 Result := nil;
end;

////////////////////////////////////////////////////////////////////////////////

//toto se vola zvnejsi, kdyz chceme postavit jakoukoliv JC
procedure TJCDb.StavJC(StartBlk, EndBlk: TBlk; SenderPnl: TIdContext; SenderOR: TObject; abAfter: Boolean);
var oblr: TOR;
    startNav: TBlkSignal;
    senderOblr: TOR;
    jc: TJC;
begin
 startNav := StartBlk as TBlkSignal;
 senderOblr := SenderOR as TOR;

 jc := Self.FindJC(StartNav, SenderOblr.vb, EndBlk);

 if (jc <> nil) then
  begin
   // v pripade nouzove cesty klik na DK opet prevest na klienta
   if (startNav.selected = TBlkSignalSelection.NC) then
     for oblr in startNav.stations do
       oblr.ORDKClickClient();

   if (SenderOblr.stack.volba = TORStackVolba.VZ) then
    begin
     SenderOblr.stack.AddJC(
      jc,
      SenderPnl,
      (startNav.selected = TBlkSignalSelection.NC) or (startNav.selected = TBlkSignalSelection.PP),
      abAfter
     );

     // zrusime zacatek, konec a variantni body
     startNav.selected := TBlkSignalSelection.none;
     (EndBlk as TBlkTrack).jcEnd := TZaver.no;
     SenderOblr.ClearVb();
    end else begin
     SenderOblr.vb.Clear(); // variantni body aktualne stavene JC jen smazeme z databaze (zrusime je na konci staveni JC)
     jc.StavJC(
       SenderPnl,
       SenderOR,
       nil,
       (startNav.selected = TBlkSignalSelection.NC) or (startNav.selected = TBlkSignalSelection.PP),
       false,
       abAfter
     );
    end;
  end else begin

   // kontrola staveni slozene jizdni cesty
   if ((startNav.selected = TBlkSignalSelection.VC) or (startNav.selected = TBlkSignalSelection.PC)) then
     if (MultiJCDb.StavJC(StartBlk, EndBlk, SenderPnl, SenderOR, abAfter)) then Exit();

   (EndBlk as TBlkTrack).jcEnd := TZaver.no;
   ORTCPServer.SendInfoMsg(SenderPnl, 'Cesta nenalezena v závěrové tabulce');
   writelog('Nelze postavit JC -  nenalezena v zaverove tabulce', WR_VC);
  end;
 end;

////////////////////////////////////////////////////////////////////////////////

function TJCDb.AddJC(JCdata: TJCprop): TJC;
var JC: TJC;
    index: Integer;
    i: Integer;
    signal: TBlkSignal;
begin
 // kontrola existence JC stejneho ID
 if (Self.IsJC(JCData.id)) then
   raise EJCIdAlreadyExists.Create('ID jízdní cesty '+IntToStr(JCData.id)+' již použito');

 index := Self.FindPlaceForNewJC(JCData.id);
 JC := TJC.Create(JCData);
 JC.index := index;
 JC.OnIdChanged := Self.JCOnIDChanged;
 JC.OnNavChanged := Self.JCOnNavChanged;
 Self.JCs.Insert(index, JC);

 // indexy prislusnych JC na konci seznamu posuneme o 1 nahoru
 for i := index+1 to Self.JCs.Count-1 do
   Self.JCs[i].index := Self.JCs[i].index + 1;

 signal := JC.navestidlo as TBlkSignal;
 if (not Self.JCsStartSignal.ContainsKey(signal)) then
   Self.JCsStartSignal.Add(signal, TList<TJC>.Create());
 Self.JCsStartSignal[signal].Add(JC);

 JCTableData.AddJC(index);
 Result := JC;
end;

procedure TJCDb.RemoveJC(index: Integer);
var i: Integer;
    OblR: TOR;
begin
 if (index < 0) then raise Exception.Create('Index podtekl seznam JC');
 if (index >= Self.JCs.Count) then raise Exception.Create('Index pretekl seznam JC');
 if (Self.JCs[index].postaveno or Self.JCs[index].staveni) then
   raise Exception.Create('JC postavena, nelze smazat');

 for OblR in ORs do
   if (OblR.stack.IsJCInStack(Self.JCs[index])) then
     raise Exception.Create('JC v zasobniku OR '+OblR.id);

 if (Self.JCsStartSignal.ContainsKey(Self.JCs[index].navestidlo as TBlkSignal)) then
   Self.JCsStartSignal[Self.JCs[index].navestidlo as TBlkSignal].Remove(Self.JCs[index]);

 Self.JCs.Delete(index);

 // aktulizujeme indexy JC (dekrementujeme)
 for i := index to Self.JCs.Count-1 do
   Self.JCs[i].index := Self.JCs[i].index - 1;

 JCTableData.RemoveJC(index);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TJCDb.RusAllJC();
var JC: TJC;
begin
 for JC in Self.JCs do
  begin
   if (JC.postaveno) then
     JC.RusJC()
   else if (JC.staveni) then
     JC.CancelStaveni('Nouzové rušení stavění JC', true);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TJCDb.FindJC(NavestidloBlokID: Integer; Staveni: Boolean = false): TJC;
var jc: TJC;
 begin
  for jc in Self.JCs do
    if (((jc.postaveno) or ((Staveni) and (jc.staveni))) and (jc.data.NavestidloBlok = NavestidloBlokID)) then
      Exit(jc);
  Result := nil;
 end;

function TJCDb.FindOnlyStaveniJC(NavestidloBlokID: Integer): TJC;
var jc: TJC;
begin
  for jc in Self.JCs do
    if ((jc.staveni) and (jc.data.NavestidloBlok = NavestidloBlokID)) then
      Exit(jc);
  Result := nil;
end;

////////////////////////////////////////////////////////////////////////////////

//vyuzivani pri vypadku polohy vyhybky ke zruseni jizdni cesty
// muze vracet vic jizdnich cest - jeden odvrat muze byt u vic aktualne postavenych JC
function TJCDB.FindPostavenaJCWithVyhybka(vyh_id: Integer): TList<TJC>;
var jc: TJC;
    vyhz: TJCVyhZaver;
    odvrz: TJCOdvratZaver;
    refz: TJCRefZaver;
    blk: TBlk;
    vyh: TBlkTurnout;
begin
 Result := TList<TJC>.Create();
 try
  Blocks.GetBlkByID(vyh_id, blk);
  vyh := TBlkTurnout(blk);

  for jc in Self.JCs do
   begin
    if (not jc.postaveno) then continue;

    // prime vyhybky
    for vyhz in jc.data.Vyhybky do
      if (vyhz.Blok = vyh_id) then
        Result.Add(jc);

    // odvraty
    for odvrz in jc.data.Odvraty do
      if (odvrz.Blok = vyh_id) then
        Result.Add(jc);

    // zamky
    if ((vyh <> nil) and (vyh.lock <> nil)) then
      for refz in jc.data.zamky do
        if (refz.Blok = vyh.lock.id) then
          Result.Add(jc);
   end;
 except
   Result.Free();
   raise;
 end;
end;

function TJCDB.FindPostavenaJCWithUsek(usek_id: Integer): TJC;
var jc: TJC;
    usekid: Integer;
    trat, tu: TBlk;
begin
  Result := nil;

  for jc in Self.JCs do
   begin
    if (not jc.postaveno) then continue;

    for usekid in jc.data.Useky do
      if (usekid = usek_id) then
        Exit(jc);

    if (jc.data.Trat > -1) then
     begin
      Blocks.GetBlkByID(jc.data.Trat, trat);
      if ((trat <> nil) and (trat.typ = btRailway)) then
       begin
        for usekid in TBlkRailway(trat).GetSettings().trackIds do
         begin
          Blocks.GetBlkByID(usekid, tu);
          if ((tu <> nil) and (tu.typ = btRT)) then
            if ((TBlkRT(tu).signalCover = nil) and (tu.id = usek_id)) then
              Exit(jc);
         end;
       end;
     end;

   end;
end;

function TJCDB.FindPostavenaJCWithTrat(trat_id: Integer): TJC;
var jc: TJC;
begin
 for jc in Self.JCs do
  begin
   if (not jc.postaveno) then continue;
   if (jc.data.Trat = trat_id) then Exit(jc);
  end;

 Result := nil;
end;

function TJCDB.FindPostavenaJCWithPrj(blk_id: Integer): TList<TJC>;
var prj: TJCPrjZaver;
    jc: TJC;
begin
 Result := TList<TJC>.Create();
 try
   for jc in Self.JCs do
    begin
     if (not jc.postaveno) then continue;
     for prj in jc.data.Prejezdy do
       if (prj.Prejezd = blk_id) then
         Result.Add(jc);
    end;
 except
   Result.Free();
   raise;
 end;
end;

function TJCDB.FindPostavenaJCWithZamek(zam_id: Integer): TList<TJC>;
var jc: TJC;
    zamZav: TJCRefZaver;
begin
 Result := TList<TJC>.Create();
 try
   for jc in Self.JCs do
    begin
     if (not jc.postaveno) then continue;
     for zamZav in jc.data.zamky do
       if (zamZav.Blok = zam_id) then
         Result.Add(jc);
    end;
 except
   Result.Free();
   raise;
 end;
end;

////////////////////////////////////////////////////////////////////////////////

// Jakmile dojde k nastaveni navestidla na ceste JC, tady se zkontroluje, zda-li
// se nahodou nema nejake navestidlo pred cestou JC rozsvitit jinak.
procedure TJCDb.CheckNNavaznost(signal: TBlkSignal);
var JC: TJC;
    prev_signal: TBlkSignal;
    navest: TBlkSignalCode;
begin
  for JC in Self.JCs do
   begin
    if ((JC.typ = TJCType.posun) or
        (JC.data.dalsiNavaznost <> TJCNextNavType.blok) or
        (JC.data.dalsiNavestidlo <> signal.id)) then continue;

    Blocks.GetBlkByID(JC.data.NavestidloBlok, TBlk(prev_signal));

    if (not prev_signal.IsGoSignal()) then continue;
    if (prev_signal.changing) then continue;

    if ((signal.IsGoSignal()) and (not signal.IsOpakVystraha())) then
     begin
      if (JC.data.odbocka) then
       begin
        if ((signal.FourtyKmph()) or (signal.signal = ncOpakOcek40)) then
          navest := nc40Ocek40
        else
          navest := ncVolno40;
       end else begin
        if ((signal.FourtyKmph()) or (signal.signal = ncOpakOcek40)) then
          navest := ncOcek40
        else
          navest := ncVolno;
       end;

     end else begin

      if (JC.data.odbocka) then
        navest := ncVystraha40
      else
        navest := ncVystraha;

     end;

    if ((JC.data.nzv) and (navest <> ncVolno)) then
      navest := TBlkSignal.AddOpak(navest);

    prev_signal.signal := navest;
   end;//for i
end;

////////////////////////////////////////////////////////////////////////////////

// rusi cestu, ve ktere je zadany blok
procedure TJCDb.RusJC(Blk: TBlk);
var tmpblk: TBlk;
    jc: TJC;
    oblr: TOR;
    jcs: TList<TJC>;
begin
 jcs := TList<TJC>.Create();
 try
   case (Blk.typ) of
    btTurnout: begin
      FreeAndNil(jcs);
      jcs := JCDb.FindPostavenaJCWithVyhybka(Blk.id);
    end;
    btCrossing: begin
      FreeAndNil(jcs);
      jcs := JCDb.FindPostavenaJCWithPrj(Blk.id);
    end;
    btTrack, btRT: begin
      jc := JCDb.FindPostavenaJCWithUsek(Blk.id);
      if (jc <> nil) then jcs.Add(jc);
    end;
    btSignal: begin
      jc := JCDb.FindJC(Blk.id);
      if (jc <> nil) then jcs.Add(jc);
    end;
    btRailway: begin
      jc := JCDb.FindPostavenaJCWithTrat(Blk.id);
      if (jc <> nil) then jcs.Add(jc);
    end;
    btLock: begin
      FreeAndNil(jcs);
      jcs := JCDb.FindPostavenaJCWithZamek(Blk.id);
    end;
   end;//case

   for jc in jcs do
    begin
     Blocks.GetBlkByID(jc.data.NavestidloBlok, tmpblk);
     if ((TBlkSignal(tmpblk).DNjc = jc) and
         ((TBlkSignal(tmpblk).IsGoSignal()) or (TBlkSignal(tmpblk).ZAM) or (jc.waitForLastUsekOrTratObsaz))) then
      begin
       jc.RusJCWithoutBlk();
       for oblr in (tmpBlk as TBlkSignal).stations do
         oblr.BlkWriteError(Self, 'Chyba povolovací návěsti '+tmpblk.name, 'TECHNOLOGIE');
      end;
    end;
  finally
   if (Assigned(jcs)) then
     jcs.Free();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TJCDb.GetCount(): Word;
begin
 Result := Self.JCs.Count;
end;

////////////////////////////////////////////////////////////////////////////////

// najde index pro novou jizdni cestu
function TJCDb.FindPlaceForNewJC(id: Integer): Integer;
var i: Integer;
begin
 i := Self.JCs.Count-1;
 while ((i >= 0) and (Self.JCs[i].id > id)) do
   i := i - 1;
 Result := i+1;
end;

////////////////////////////////////////////////////////////////////////////////

function TJCDb.IsJC(id: Integer; ignore_index: Integer = -1): Boolean;
var index: Integer;
begin
 index := Self.GetJCIndex(id);
 Result := ((index <> -1) and (index <> ignore_index));
end;

////////////////////////////////////////////////////////////////////////////////
// Hledame JC se zadanym ID v seznamu bloku pomoci binarniho vyhledavani.

function TJCDb.GetJCIndex(id: Integer): Integer;
var left, right, mid: Integer;
begin
 left  := 0;
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

////////////////////////////////////////////////////////////////////////////////

function TJCDb.GetJCByID(id: integer): TJC;
var index: Integer;
begin
 Result := nil;
 index := Self.GetJCIndex(id);
 if (index > -1) then Result := Self.JCs[index];
end;

////////////////////////////////////////////////////////////////////////////////

procedure TJCDb.JCOnIDChanged(Sender: TObject);
var new_index, min_index, i, index: Integer;
    tmp: TJC;
begin
 index := (Sender as TJC).index;
 Self.JCs.OwnsObjects := false;
 tmp := Self.JCs[index];
 Self.JCs.Delete(index);
 Self.JCs.OwnsObjects := true;
 new_index := FindPlaceForNewJC(tmp.id);

 // provedeme prehozeni bloku na jinou pozici
 Self.JCs.Insert(new_index, tmp);
 if (index = new_index) then Exit();

 // od nejmensiho prohazovaneho indexu aktualizujeme indexy
 // aktualizjeme dokud indexy nesedi
 min_index := Min(new_index, index);
 for i := min_index to Self.JCs.Count-1 do
   if (Self.JCs[i].index = i) then break
   else Self.JCs[i].index := i;

 JCTableData.MoveJC(index, new_index);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TJCDb.UpdateJCIndexes();
var i: Integer;
begin
 for i := 0 to Self.JCs.Count-1 do
   Self.JCs[i].index := i;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TJCDb.FillJCsStartSignal();
var JC: TJC;
    signal: TBlkSignal;
begin
 Self.JCsStartSignal.Clear();
 for JC in Self.JCs do
  begin
   if ((JC.navestidlo <> nil) and (JC.navestidlo.typ = btSignal)) then
    begin
     signal := JC.navestidlo as TBlkSignal;
     if (not Self.JCsStartSignal.ContainsKey(signal)) then
       Self.JCsStartSignal.Add(signal, TList<TJC>.Create());
     Self.JCsStartSignal[signal].Add(JC);
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TJCDb.JCOnNavChanged(Sender: TObject; origNav: TBlk);
var signal: TBlkSignal;
    jc: TJC;
begin
 signal := origNav as TBlkSignal;
 jc := Sender as TJC;

 if (origNav <> nil) then
  begin
   if (Self.JCsStartSignal.ContainsKey(signal)) then
     if (Self.JCsStartSignal[signal].Contains(jc)) then
       Self.JCsStartSignal[signal].Remove(jc);
  end;

 if (jc.navestidlo <> nil) then
  begin
   if (not Self.JCsStartSignal.ContainsKey(jc.navestidlo as TBlkSignal)) then
     Self.JCsStartSignal.Add(jc.navestidlo as TBlkSignal, TList<TJC>.Create());
   Self.JCsStartSignal[jc.navestidlo as TBlkSignal].Add(jc);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TJCDb.IsAnyJC(signal: TBlkSignal): Boolean;
begin
 Result := Self.JCsStartSignal.ContainsKey(signal) and (Self.JCsStartSignal[signal].Count > 0);
end;

function TJCDb.IsAnyVC(signal: TBlkSignal): Boolean;
var jc: TJC;
begin
 if (not Self.JCsStartSignal.ContainsKey(signal)) then
   Exit(false);
 for jc in Self.JCsStartSignal[signal] do
   if (jc.typ = TJCType.vlak) then
      Exit(true);
 Result := false;
end;

function TJCDb.IsAnyPC(signal: TBlkSignal): Boolean;
var jc: TJC;
begin
 if (not Self.JCsStartSignal.ContainsKey(signal)) then
   Exit(false);
 for jc in Self.JCsStartSignal[signal] do
   if (jc.typ = TJCType.posun) then
      Exit(true);
 Result := false;
end;

////////////////////////////////////////////////////////////////////////////////
// Zjistuje, jestli je mozno postvit z navestidla \nav v aktualni situaci alespon
// jednu cestu typu \typ.

function TJCDb.IsAnyJCAvailable(signal: TBlkSignal; typ: TJCType): Boolean;
var jc: TJC;
    blk: TBlk;
    usek: TBlkTrack;
begin
 if (not Self.JCsStartSignal.ContainsKey(signal)) then
   Exit(false);
 for jc in Self.JCsStartSignal[signal] do
  begin
   if ((jc.typ = typ) and (jc.data.Useky.Count > 0)) then
    begin
      Blocks.GetBlkByID(jc.data.Useky[0], blk);
      if ((blk <> nil) and ((blk.typ = btTrack) or (blk.typ = btRT))) then
       begin
        usek := blk as TBlkTrack;
        if ((usek.Zaver = TZaver.no) and (usek.occupied = TTrackState.free)) then
          Exit(true);
       end;
    end;
  end;
 Result := false;
end;

function TJCDb.IsAnyVCAvailable(signal: TBlkSignal): Boolean;
begin
 Result := Self.IsAnyJCAvailable(signal, TJCType.vlak);
end;

function TJCDb.IsAnyPCAvailable(signal: TBlkSignal): Boolean;
begin
 Result := Self.IsAnyJCAvailable(signal, TJCType.posun);
end;

////////////////////////////////////////////////////////////////////////////////

function TJCDb.GetItem(i: Integer): TJC;
begin
  Result := Self.JCs[i];
end;

function TJCDb.GetEnumerator(): TEnumerator<TJC>;
begin
 Result := Self.JCs.GetEnumerator();
end;

////////////////////////////////////////////////////////////////////////////////

function TJCDb.IsAnyJCWithPrefix(startNav: TBlkSignal; vb: TList<TObject>): Boolean;
var jc: TJC;
    j: Integer;
    error: Boolean;
begin
 // startNav musi mit navolenou volbu, aby tato funkce fungovala.

 if (not Self.JCsStartSignal.ContainsKey(startNav)) then
   Exit(false);

 for jc in Self.JCsStartSignal[startNav] do
  begin
   if (JC.navestidlo <> startNav) then continue;

   if ((Integer(startNav.selected) = Integer(jc.typ)) or
      ((startNav.selected = TBlkSignalSelection.NC) and (jc.typ = TJCType.vlak)) or
      ((startNav.selected = TBlkSignalSelection.PP) and (jc.typ = TJCType.posun))) then
    begin
     // kontrola variantnich bodu:
     if (vb.Count > jc.data.vb.Count) then continue;

     error := false;
     for j := 0 to vb.Count-1 do
       if (jc.data.vb[j] <> (vb[j] as TBlk).id) then
         error := true;
     if (error) then continue;        

     Exit(true);
    end;
  end;

 Result := false;
end;

////////////////////////////////////////////////////////////////////////////////

initialization
  JCDb := TJCDb.Create();

finalization
  FreeAndNil(JCDb);

end.//unit
