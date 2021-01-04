unit TJCDatabase;

{
  TJCDb je databaze jizdnich cest.
}

interface

uses TechnologieJC, Block, IniFiles, SysUtils, Windows, IdContext,
      Generics.Collections, Classes, IBUtils, BlockDb, BlockSignal;

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
     procedure UpdateIndexes();

     procedure Update();
     procedure ActivateJC(StartBlk, EndBlk: TBlk; SenderPnl: TIdContext; SenderOR: TObject; abAfter: Boolean);

     function Add(JCdata: TJCdata): TJC;
     procedure Remove(index: Integer);

     function GetJCByIndex(index: Integer): TJC;
     function GetJCIndex(id: Integer): Integer;
     function GetJCByID(id: integer): TJC;

     function FindJC(signalId: Integer; activatingToo: Boolean = false): TJC; overload;
     function FindJCActivating(signalId: Integer): TJC;
     function IsJC(id: Integer; ignore_index: Integer = -1): Boolean;

     function FindActiveJCWithTurnout(vyh_id: Integer): TList<TJC>;
     function FindActiveJCWithTrack(trackId: Integer): TJC;
     function FindActiveJCWithCrossing(blk_id: Integer): TList<TJC>;
     function FindActiveJCWithRailway(trat_id: Integer): TJC;
     function FindActiveJCWithLock(zam_id: Integer): TList<TJC>;

     // jakmile dojde ke zmene navesti navestidla nav, muze dojit k ovlivneni nejakeho jineho navestidla
     // tato fce zajisti, ze k ovlivneni dojde
     procedure UpdatePrevSignal(signal: TBlkSignal);

     procedure CancelAll();
     procedure Cancel(blk: TBlk);     // rusi cestu, ve ktere je zadany blok (jakehokoliv typu)

     function IsAnyJC(signal: TBlkSignal): Boolean;
     function IsAnyVC(signal: TBlkSignal): Boolean;
     function IsAnyPC(signal: TBlkSignal): Boolean;

     function IsAnyJCAvailable(signal: TBlkSignal; typ: TJCType): Boolean;
     function IsAnyVCAvailable(signal: TBlkSignal): Boolean;
     function IsAnyPCAvailable(signal: TBlkSignal): Boolean;

     function FindJC(startNav: TBlkSignal; vb: TList<TObject>; EndBlk: TBlk): TJC; overload;
     function IsAnyJCWithPrefix(startSignal: TBlkSignal; vb: TList<TObject>): Boolean;

     property Count: Word read GetCount;
     property filename: string read ffilename;

     function GetEnumerator(): TEnumerator<TJC>;
     property Items[index : integer] : TJC read GetItem; default;


  end;

var
  JCDb: TJcDb;


implementation

uses Logging, GetSystems, BlockTrack, Area, TCPServerPanel, BlockRailway,
      DataJC, AreaStack, AreaDb, TMultiJCDatabase, appEv, BlockTurnout,
      BlockRailwayTrack;

////////////////////////////////////////////////////////////////////////////////
// TRIDA TJCDb
//  databaze jizdnich cest
////////////////////////////////////////////////////////////////////////////////


constructor TJCDb.Create();
begin
 inherited;
 Self.JCs := TObjectList<TJC>.Create();
 Self.JCsStartSignal := TObjectDictionary<TBlkSignal, TList<TJC>>.Create();
end;

destructor TJCDb.Destroy();
begin
 Self.JCs.Free();
 Self.JCsStartSignal.Free();
 inherited;
end;

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
       JC.OnSignalChanged := Self.JCOnNavChanged;
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

   Self.UpdateIndexes();
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
     if (JC.state.destroyBlock > -5) then
       JC.DynamicCanceling();
     if (JC.state.destroyBlock = -6) then
       JC.DynamicCancelingNC();

     if ((JC.activating) or (JC.step = _JC_STEP_CEKANI_POSLEDNI_USEK)) then
      begin
       JC.UpdateActivating();
       JC.UpdateTimeOut();
      end;
   except
    on E: Exception do
     begin
      if (not log_err_flag) then
       AppEvents.LogException(E, 'JC '+JC.name + ' update error');
      if (JC.activating) then
        JC.CancelActivating('Vyjímka', true)
      else
        JC.CancelWithoutTrackRelease();
     end;
   end;//except
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TJCDb.GetJCByIndex(index: Integer): TJC;
begin
 if ((index < 0) or (index >= Self.JCs.Count)) then
  begin
   Result := nil;
   Exit();
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
   if (JC.signal <> startNav) then continue;

   Blocks.GetBlkByID(jc.data.tracks[jc.data.tracks.Count-1], blk);
   if (blk <> endBlk) then continue;

   if ((Integer(startNav.selected) = Integer(jc.typ)) or
      ((startNav.selected = TBlkSignalSelection.NC) and (jc.typ = TJCType.train)) or
      ((startNav.selected = TBlkSignalSelection.PP) and (jc.typ = TJCType.shunt))) then
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

procedure TJCDb.ActivateJC(StartBlk, EndBlk: TBlk; SenderPnl: TIdContext; SenderOR: TObject; abAfter: Boolean);
var area: TArea;
    startSignal: TBlkSignal;
    senderArea: TArea;
    jc: TJC;
begin
 startSignal := StartBlk as TBlkSignal;
 senderArea := SenderOR as TArea;

 jc := Self.FindJC(startSignal, senderArea.vb, EndBlk);

 if (jc <> nil) then
  begin
   // v pripade nouzove cesty klik na DK opet prevest na klienta
   if (startSignal.selected = TBlkSignalSelection.NC) then
     for area in startSignal.areas do
       area.ORDKClickClient();

   if (senderArea.stack.mode = TORStackMode.VZ) then
    begin
     senderArea.stack.AddJC(
      jc,
      SenderPnl,
      (startSignal.selected = TBlkSignalSelection.NC) or (startSignal.selected = TBlkSignalSelection.PP),
      abAfter
     );

     // zrusime zacatek, konec a variantni body
     startSignal.selected := TBlkSignalSelection.none;
     (EndBlk as TBlkTrack).jcEnd := TZaver.no;
     senderArea.ClearVb();
    end else begin
     senderArea.vb.Clear(); // variantni body aktualne stavene JC jen smazeme z databaze (zrusime je na konci staveni JC)
     jc.Activate(
       SenderPnl,
       SenderOR,
       nil,
       (startSignal.selected = TBlkSignalSelection.NC) or (startSignal.selected = TBlkSignalSelection.PP),
       false,
       abAfter
     );
    end;
  end else begin

   // kontrola staveni slozene jizdni cesty
   if ((startSignal.selected = TBlkSignalSelection.VC) or (startSignal.selected = TBlkSignalSelection.PC)) then
     if (MultiJCDb.Activate(StartBlk, EndBlk, SenderPnl, SenderOR, abAfter)) then Exit();

   (EndBlk as TBlkTrack).jcEnd := TZaver.no;
   PanelServer.SendInfoMsg(SenderPnl, 'Cesta nenalezena v závěrové tabulce');
   writelog('Nelze postavit JC -  nenalezena v zaverove tabulce', WR_VC);
  end;
 end;

////////////////////////////////////////////////////////////////////////////////

function TJCDb.Add(JCdata: TJCdata): TJC;
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
 JC.OnSignalChanged := Self.JCOnNavChanged;
 Self.JCs.Insert(index, JC);

 // indexy prislusnych JC na konci seznamu posuneme o 1 nahoru
 for i := index+1 to Self.JCs.Count-1 do
   Self.JCs[i].index := Self.JCs[i].index + 1;

 signal := JC.signal as TBlkSignal;
 if (not Self.JCsStartSignal.ContainsKey(signal)) then
   Self.JCsStartSignal.Add(signal, TList<TJC>.Create());
 Self.JCsStartSignal[signal].Add(JC);

 JCTableData.AddJC(index);
 Result := JC;
end;

procedure TJCDb.Remove(index: Integer);
var i: Integer;
    area: TArea;
begin
 if (index < 0) then raise Exception.Create('Index podtekl seznam JC');
 if (index >= Self.JCs.Count) then raise Exception.Create('Index pretekl seznam JC');
 if (Self.JCs[index].active or Self.JCs[index].activating) then
   raise Exception.Create('JC postavena, nelze smazat');

 for area in Areas do
   if (area.stack.IsJCInStack(Self.JCs[index])) then
     raise Exception.Create('JC v zasobniku OR '+area.id);

 if (Self.JCsStartSignal.ContainsKey(Self.JCs[index].signal as TBlkSignal)) then
   Self.JCsStartSignal[Self.JCs[index].signal as TBlkSignal].Remove(Self.JCs[index]);

 Self.JCs.Delete(index);

 // aktulizujeme indexy JC (dekrementujeme)
 for i := index to Self.JCs.Count-1 do
   Self.JCs[i].index := Self.JCs[i].index - 1;

 JCTableData.RemoveJC(index);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TJCDb.CancelAll();
var JC: TJC;
begin
 for JC in Self.JCs do
  begin
   if (JC.active) then
     JC.Cancel()
   else if (JC.activating) then
     JC.CancelActivating('Nouzové rušení stavění JC', true);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TJCDb.FindJC(signalId: Integer; activatingToo: Boolean = false): TJC;
var jc: TJC;
 begin
  for jc in Self.JCs do
    if (((jc.active) or ((activatingToo) and (jc.activating))) and (jc.data.signalId = signalId)) then
      Exit(jc);
  Result := nil;
 end;

function TJCDb.FindJCActivating(signalId: Integer): TJC;
var jc: TJC;
begin
  for jc in Self.JCs do
    if ((jc.activating) and (jc.data.signalId = signalId)) then
      Exit(jc);
  Result := nil;
end;

////////////////////////////////////////////////////////////////////////////////

//vyuzivani pri vypadku polohy vyhybky ke zruseni jizdni cesty
// muze vracet vic jizdnich cest - jeden odvrat muze byt u vic aktualne postavenych JC
function TJCDB.FindActiveJCWithTurnout(vyh_id: Integer): TList<TJC>;
var jc: TJC;
    turnoutZav: TJCTurnoutZav;
    refugeeZav: TJCRefugeeZav;
    refZav: TJCRefZav;
    blk: TBlk;
    turnout: TBlkTurnout;
begin
 Result := TList<TJC>.Create();
 try
  Blocks.GetBlkByID(vyh_id, blk);
  turnout := TBlkTurnout(blk);

  for jc in Self.JCs do
   begin
    if (not jc.active) then continue;

    // prime vyhybky
    for turnoutZav in jc.data.turnouts do
      if (turnoutZav.block = vyh_id) then
        Result.Add(jc);

    // odvraty
    for refugeeZav in jc.data.refuges do
      if (refugeeZav.block = vyh_id) then
        Result.Add(jc);

    // zamky
    if ((turnout <> nil) and (turnout.lock <> nil)) then
      for refZav in jc.data.locks do
        if (refZav.block = turnout.lock.id) then
          Result.Add(jc);
   end;
 except
   Result.Free();
   raise;
 end;
end;

function TJCDB.FindActiveJCWithTrack(trackId: Integer): TJC;
var jc: TJC;
    _trackId: Integer;
    railway, railwayTrack: TBlk;
begin
  Result := nil;

  for jc in Self.JCs do
   begin
    if (not jc.active) then continue;

    for _trackId in jc.data.tracks do
      if (_trackId = trackId) then
        Exit(jc);

    if (jc.data.railwayId > -1) then
     begin
      Blocks.GetBlkByID(jc.data.railwayId, railway);
      if ((railway <> nil) and (railway.typ = btRailway)) then
       begin
        for _trackId in TBlkRailway(railway).GetSettings().trackIds do
         begin
          Blocks.GetBlkByID(_trackId, railwayTrack);
          if ((railwayTrack <> nil) and (railwayTrack.typ = btRT)) then
            if ((TBlkRT(railwayTrack).signalCover = nil) and (railwayTrack.id = trackId)) then
              Exit(jc);
         end;
       end;
     end;

   end;
end;

function TJCDB.FindActiveJCWithRailway(trat_id: Integer): TJC;
var jc: TJC;
begin
 for jc in Self.JCs do
  begin
   if (not jc.active) then continue;
   if (jc.data.railwayId = trat_id) then Exit(jc);
  end;

 Result := nil;
end;

function TJCDB.FindActiveJCWithCrossing(blk_id: Integer): TList<TJC>;
var crossing: TJCCrossingZav;
    jc: TJC;
begin
 Result := TList<TJC>.Create();
 try
   for jc in Self.JCs do
    begin
     if (not jc.active) then continue;
     for crossing in jc.data.crossings do
       if (crossing.crossingId = blk_id) then
         Result.Add(jc);
    end;
 except
   Result.Free();
   raise;
 end;
end;

function TJCDB.FindActiveJCWithLock(zam_id: Integer): TList<TJC>;
var jc: TJC;
    lockZav: TJCRefZav;
begin
 Result := TList<TJC>.Create();
 try
   for jc in Self.JCs do
    begin
     if (not jc.active) then continue;
     for lockZav in jc.data.locks do
       if (lockZav.block = zam_id) then
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
procedure TJCDb.UpdatePrevSignal(signal: TBlkSignal);
var JC: TJC;
    prevSignal: TBlkSignal;
    code: TBlkSignalCode;
begin
  for JC in Self.JCs do
   begin
    if ((JC.typ = TJCType.shunt) or
        (JC.data.nextSignalType <> TJCNextSignalType.signal) or
        (JC.data.nextSignalId <> signal.id)) then continue;

    Blocks.GetBlkByID(JC.data.signalId, TBlk(prevSignal));

    if (not prevSignal.IsGoSignal()) then continue;
    if (prevSignal.changing) then continue;

    if ((signal.IsGoSignal()) and (not signal.IsOpakVystraha())) then
     begin
      if (JC.data.turn) then
       begin
        if ((signal.FourtyKmph()) or (signal.signal = ncOpakOcek40)) then
          code := nc40Ocek40
        else
          code := ncVolno40;
       end else begin
        if ((signal.FourtyKmph()) or (signal.signal = ncOpakOcek40)) then
          code := ncOcek40
        else
          code := ncVolno;
       end;

     end else begin

      if (JC.data.turn) then
        code := ncVystraha40
      else
        code := ncVystraha;

     end;

    if ((JC.data.nzv) and (code <> ncVolno)) then
      code := TBlkSignal.AddOpak(code);

    prevSignal.signal := code;
   end;
end;

////////////////////////////////////////////////////////////////////////////////

// rusi cestu, ve ktere je zadany blok
procedure TJCDb.Cancel(blk: TBlk);
var tmpblk: TBlk;
    jc: TJC;
    area: TArea;
    jcs: TList<TJC>;
begin
 jcs := TList<TJC>.Create();
 try
   case (blk.typ) of
    btTurnout: begin
      FreeAndNil(jcs);
      jcs := JCDb.FindActiveJCWithTurnout(blk.id);
    end;
    btCrossing: begin
      FreeAndNil(jcs);
      jcs := JCDb.FindActiveJCWithCrossing(blk.id);
    end;
    btTrack, btRT: begin
      jc := JCDb.FindActiveJCWithTrack(blk.id);
      if (jc <> nil) then jcs.Add(jc);
    end;
    btSignal: begin
      jc := JCDb.FindJC(blk.id);
      if (jc <> nil) then jcs.Add(jc);
    end;
    btRailway: begin
      jc := JCDb.FindActiveJCWithRailway(blk.id);
      if (jc <> nil) then jcs.Add(jc);
    end;
    btLock: begin
      FreeAndNil(jcs);
      jcs := JCDb.FindActiveJCWithLock(blk.id);
    end;
   end;//case

   for jc in jcs do
    begin
     Blocks.GetBlkByID(jc.data.signalId, tmpblk);
     if ((TBlkSignal(tmpblk).DNjc = jc) and
         ((TBlkSignal(tmpblk).IsGoSignal()) or (TBlkSignal(tmpblk).ZAM) or (jc.waitForLastTrackOrRailwayOccupy))) then
      begin
       jc.CancelWithoutTrackRelease();
       for area in (tmpBlk as TBlkSignal).areas do
         area.BlkWriteError(Self, 'Chyba povolovací návěsti '+tmpblk.name, 'TECHNOLOGIE');
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

procedure TJCDb.UpdateIndexes();
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
   if ((JC.signal <> nil) and (JC.signal.typ = btSignal)) then
    begin
     signal := JC.signal as TBlkSignal;
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

 if (jc.signal <> nil) then
  begin
   if (not Self.JCsStartSignal.ContainsKey(jc.signal as TBlkSignal)) then
     Self.JCsStartSignal.Add(jc.signal as TBlkSignal, TList<TJC>.Create());
   Self.JCsStartSignal[jc.signal as TBlkSignal].Add(jc);
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
   if (jc.typ = TJCType.train) then
      Exit(true);
 Result := false;
end;

function TJCDb.IsAnyPC(signal: TBlkSignal): Boolean;
var jc: TJC;
begin
 if (not Self.JCsStartSignal.ContainsKey(signal)) then
   Exit(false);
 for jc in Self.JCsStartSignal[signal] do
   if (jc.typ = TJCType.shunt) then
      Exit(true);
 Result := false;
end;

////////////////////////////////////////////////////////////////////////////////
// Zjistuje, jestli je mozno postvit z navestidla \nav v aktualni situaci alespon
// jednu cestu typu \typ.

function TJCDb.IsAnyJCAvailable(signal: TBlkSignal; typ: TJCType): Boolean;
var jc: TJC;
    blk: TBlk;
    track: TBlkTrack;
begin
 if (not Self.JCsStartSignal.ContainsKey(signal)) then
   Exit(false);
 for jc in Self.JCsStartSignal[signal] do
  begin
   if ((jc.typ = typ) and (jc.data.tracks.Count > 0)) then
    begin
      Blocks.GetBlkByID(jc.data.tracks[0], blk);
      if ((blk <> nil) and ((blk.typ = btTrack) or (blk.typ = btRT))) then
       begin
        track := blk as TBlkTrack;
        if ((track.Zaver = TZaver.no) and (track.occupied = TTrackState.free)) then
          Exit(true);
       end;
    end;
  end;
 Result := false;
end;

function TJCDb.IsAnyVCAvailable(signal: TBlkSignal): Boolean;
begin
 Result := Self.IsAnyJCAvailable(signal, TJCType.train);
end;

function TJCDb.IsAnyPCAvailable(signal: TBlkSignal): Boolean;
begin
 Result := Self.IsAnyJCAvailable(signal, TJCType.shunt);
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

function TJCDb.IsAnyJCWithPrefix(startSignal: TBlkSignal; vb: TList<TObject>): Boolean;
var jc: TJC;
    j: Integer;
    error: Boolean;
begin
 // startNav musi mit navolenou volbu, aby tato funkce fungovala.

 if (not Self.JCsStartSignal.ContainsKey(startSignal)) then
   Exit(false);

 for jc in Self.JCsStartSignal[startSignal] do
  begin
   if (JC.signal <> startSignal) then continue;

   if ((Integer(startSignal.selected) = Integer(jc.typ)) or
      ((startSignal.selected = TBlkSignalSelection.NC) and (jc.typ = TJCType.train)) or
      ((startSignal.selected = TBlkSignalSelection.PP) and (jc.typ = TJCType.shunt))) then
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
