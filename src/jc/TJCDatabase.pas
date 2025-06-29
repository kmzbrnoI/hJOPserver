unit TJCDatabase;

{
  TJCDb je databaze jizdnich cest.
}

interface

uses TechnologieJC, Block, IniFiles, SysUtils, Windows, IdContext, System.Math,
  Generics.Collections, Classes, BlockDb, BlockSignal;

type
  EJCIdAlreadyExists = class(Exception);
  EJCInvalidPath = class(Exception);
  TJCBlockFinder = function(jc: TJC; blockid: Integer): Boolean;

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

    function FindActiveJCs(finder: TJCContains; blockid: Integer): TList<TJC>;
    function FindActiveNCs(finder: TJCContains; blockid: Integer): TList<TJC>;

  public

    constructor Create();
    destructor Destroy(); override;

    procedure LoadData(const filename: string);
    procedure SaveData(const filename: string);
    procedure UpdateIndexes();

    procedure Update();
    procedure ActivateJC(blocks: TList<TBlk>; SenderPnl: TIdContext; SenderOR: TObject; abAfter: Boolean);

    function Add(JCdata: TJCdata): TJC;
    procedure Remove(index: Integer);

    function GetJCByIndex(index: Integer): TJC;
    function GetJCIndex(id: Integer): Integer;
    function GetJCByID(id: Integer): TJC;

    function FindJC(signalId: Integer; activatingToo: Boolean = false): TJC; overload;
    function FindJCActivating(signalId: Integer): TJC;
    function IsJC(id: Integer; ignore_index: Integer = -1): Boolean;

    function FindActiveJCsWithTurnout(turnout_id: Integer): TList<TJC>;
    function FindActiveNCsWithTurnout(turnout_id: Integer): TList<TJC>;
    function FindActiveJCsWithRailway(trat_id: Integer): TList<TJC>;
    function FindActiveNCsWithRailway(trat_id: Integer): TList<TJC>;
    function FindActiveJCsWithLock(zam_id: Integer): TList<TJC>;
    function FindActiveNCsWithLock(zam_id: Integer): TList<TJC>;
    function FindActiveJCWithTrack(track_id: Integer): TJC;
    function FindActiveJCsWithCrossing(blk_id: Integer): TList<TJC>;
    function FindActiveNCsWithPSt(pst_id: Integer): TList<TJC>;

    // jakmile dojde ke zmene navesti navestidla nav, muze dojit k ovlivneni nejakeho jineho navestidla
    // tato fce zajisti, ze k ovlivneni dojde
    procedure UpdatePrevSignal(signal: TBlkSignal);

    procedure CancelAll();
    procedure Cancel(blk: TBlk); // rusi cestu, ve ktere je zadany blok (jakehokoliv typu)

    function IsAnyJC(signal: TBlkSignal): Boolean;
    function IsAnyVC(signal: TBlkSignal): Boolean;
    function IsAnyPC(signal: TBlkSignal): Boolean;

    function IsAnyJCAvailable(signal: TBlkSignal; typ: TJCType): Boolean;
    function IsAnyVCAvailable(signal: TBlkSignal): Boolean;
    function IsAnyPCAvailable(signal: TBlkSignal): Boolean;

    function FindJC(blocks: TList<TBlk>): TJC; overload;
    function IsAnyJCWithPrefix(blocks: TList<TBlk>): Boolean;

    property Count: Word read GetCount;
    property filename: string read ffilename;

    function GetEnumerator(): TEnumerator<TJC>;
    property Items[index: Integer]: TJC read GetItem; default;

  end;

var
  JCDb: TJCDb;

implementation

uses Logging, GetSystems, BlockTrack, Area, TCPServerPanel, BlockRailway,
  DataJC, AreaStack, AreaDb, TMultiJCDatabase, appEv, BlockTurnout,
  BlockRailwayTrack, PanelConnData, BlockPst;

/// /////////////////////////////////////////////////////////////////////////////
// TRIDA TJCDb
// databaze jizdnich cest
/// /////////////////////////////////////////////////////////////////////////////

constructor TJCDb.Create();
begin
  inherited;
  Self.JCs := TObjectList<TJC>.Create();
  Self.JCsStartSignal := TObjectDictionary < TBlkSignal, TList < TJC >>.Create();
end;

destructor TJCDb.Destroy();
begin
  Self.JCs.Free();
  Self.JCsStartSignal.Free();
  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

// load data from ini file
procedure TJCDb.LoadData(const filename: string);
var ini: TMemIniFile;
  sections: TStrings;
begin
  Log('Načítám JC - ' + filename, llInfo, lsData);

  Self.ffilename := filename;
  ini := TMemIniFile.Create(filename, TEncoding.UTF8);

  Self.JCs.Clear();
  sections := TStringList.Create();
  try
    ini.ReadSections(sections);

    for var i: Integer := 0 to sections.Count - 1 do
    begin
      var JC: TJC := TJC.Create();
      try
        JC.index := i;
        JC.OnIdChanged := Self.JCOnIDChanged;
        JC.OnSignalChanged := Self.JCOnNavChanged;
        JC.LoadData(ini, sections[i]);
        Self.JCs.Insert(Self.FindPlaceForNewJC(JC.id), JC);
      except
        on E: Exception do
        begin
          AppEvents.LogException(E, 'JC ' + JC.name + ' se nepodařilo načíst');
          JC.Free();
        end;
      end;
    end; // for i

    Self.UpdateIndexes();
  finally
    ini.Free();
    sections.Free();
  end;

  Self.FillJCsStartSignal();
  Log('Načteno ' + IntToStr(Self.JCs.Count) + ' JC', llInfo, lsData);
end;

// save data to ini file:
procedure TJCDb.SaveData(const filename: string);
var ini: TMemIniFile;
begin
  Log('Ukládám JC - ' + filename, llInfo, lsData);

  DeleteFile(PChar(filename));
  ini := TMemIniFile.Create(filename, TEncoding.UTF8);
  try
    for var JC: TJC in Self.JCs do
      JC.SaveData(ini, IntToStr(JC.id));

    ini.UpdateFile();
  finally
    ini.Free();
  end;

  Log('JC uloženy', llInfo, lsData);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TJCDb.Update();
begin
  for var JC: TJC in Self.JCs do
    JC.Update();
end;

/// /////////////////////////////////////////////////////////////////////////////

function TJCDb.GetJCByIndex(index: Integer): TJC;
begin
  if ((index < 0) or (index >= Self.JCs.Count)) then
  begin
    Result := nil;
    Exit();
  end;

  Result := Self.JCs[index];
end;

/// /////////////////////////////////////////////////////////////////////////////

function TJCDb.FindJC(blocks: TList<TBlk>): TJC;
begin
  if (blocks.Count < 2) then // at least signal and last track must be in blocks
    Exit(nil);
  if ((blocks[0].typ <> btSignal) or ((blocks[blocks.Count-1].typ <> btTrack) and (blocks[blocks.Count-1].typ <> btRT))) then
    Exit(nil);

  var startSignal := TBlkSignal(blocks[0]);
  if (not Self.JCsStartSignal.ContainsKey(startSignal)) then
    Exit(nil);

  for var JC: TJC in Self.JCsStartSignal[startSignal] do
  begin
    if (JC.signal <> startSignal) then // just for sure
      continue;

    var blk := BlockDb.Blocks.GetBlkByID(JC.data.tracks[JC.data.tracks.Count-1]);
    if (blk <> blocks[blocks.Count-1]) then
      continue;

    if ((Integer(startSignal.selected) = Integer(JC.typ)) or ((startSignal.selected = TBlkSignalSelection.NC) and
        (JC.typ = TJCType.train)) or ((startSignal.selected = TBlkSignalSelection.PP) and (JC.typ = TJCType.shunt))) then
    begin
      // kontrola variantnich bodu:
      if (JC.data.vb.Count <> blocks.Count-2) then
        continue;
      var match: Boolean := true;
      for var j: Integer := 0 to JC.data.vb.Count-1 do
        if (JC.data.vb[j] <> blocks[j+1].id) then
          match := false;
      if (not match) then
        continue;

      Exit(JC);
    end;
  end;

  Result := nil;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TJCDb.ActivateJC(blocks: TList<TBlk>; SenderPnl: TIdContext; SenderOR: TObject; abAfter: Boolean);
begin
  var senderArea: TArea := TArea(SenderOR);

  if (blocks.Count < 2) then
    raise EJCInvalidPath.Create('Nedostatečný počet bloků JC');
  if ((blocks[0].typ <> btSignal) or ((blocks[blocks.Count-1].typ <> btTrack) and (blocks[blocks.Count-1].typ <> btRT))) then
    raise EJCInvalidPath.Create('Špatný typ bloků JC');

  var startSignal: TBlkSignal := TBlkSignal(blocks[0]);
  var lastBlock: TBlkTrack := TBlkTrack(blocks[blocks.Count-1]);
  var JC := Self.FindJC(blocks);

  if (JC <> nil) then
  begin
    // v pripade nouzove cesty klik na DK opet prevest na klienta
    if (startSignal.selected = TBlkSignalSelection.NC) then
      for var area: TArea in startSignal.areas do
        Area.ORDKClickClient();

    if (senderArea.stack.mode = TORStackMode.VZ) then
    begin
      senderArea.stack.AddJC(JC, SenderPnl, (startSignal.selected = TBlkSignalSelection.NC) or
        (startSignal.selected = TBlkSignalSelection.PP), abAfter);

      // zrusime zacatek, konec a variantni body
      TPanelConnData(SenderPnl.Data).ClearAndHidePathBlocks();
    end else begin
      // variantni body aktualne stavene JC jen smazeme z databaze (zrusime je na konci staveni JC)
      TPanelConnData(SenderPnl.Data).pathBlocks.Clear();
      JC.Activate(SenderPnl, SenderOR, nil, (startSignal.selected = TBlkSignalSelection.NC) or
        (startSignal.selected = TBlkSignalSelection.PP), false, abAfter);
    end;
  end else begin

    // kontrola staveni slozene jizdni cesty
    if ((startSignal.selected = TBlkSignalSelection.VC) or (startSignal.selected = TBlkSignalSelection.PC)) then
      if (MultiJCDb.Activate(blocks, SenderPnl, SenderOR, abAfter)) then
        Exit();

    lastBlock.jcEnd := TZaver.no;
    TPanelConnData(SenderPnl.Data).DeleteLastPathBlock();
    PanelServer.SendInfoMsg(SenderPnl, 'Cesta nenalezena v závěrové tabulce');
    Log('Nelze postavit JC -  nenalezena v zaverove tabulce', llInfo, lsJC);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TJCDb.Add(JCdata: TJCdata): TJC;
var JC: TJC;
  index: Integer;
  signal: TBlkSignal;
begin
  // kontrola existence JC stejneho ID
  if (Self.IsJC(JCdata.id)) then
    raise EJCIdAlreadyExists.Create('ID jízdní cesty ' + IntToStr(JCdata.id) + ' již použito');

  index := Self.FindPlaceForNewJC(JCdata.id);
  JC := TJC.Create(JCdata);
  JC.index := index;
  JC.OnIdChanged := Self.JCOnIDChanged;
  JC.OnSignalChanged := Self.JCOnNavChanged;
  Self.JCs.Insert(index, JC);

  // indexy prislusnych JC na konci seznamu posuneme o 1 nahoru
  for var i: Integer := index + 1 to Self.JCs.Count - 1 do
    Self.JCs[i].index := Self.JCs[i].index + 1;

  signal := JC.signal as TBlkSignal;
  if (not Self.JCsStartSignal.ContainsKey(signal)) then
    Self.JCsStartSignal.Add(signal, TList<TJC>.Create());
  Self.JCsStartSignal[signal].Add(JC);

  JCTableData.AddJC(index);
  Result := JC;
end;

procedure TJCDb.Remove(index: Integer);
begin
  if (index < 0) then
    raise Exception.Create('Index podtekl seznam JC');
  if (index >= Self.JCs.Count) then
    raise Exception.Create('Index pretekl seznam JC');
  if (Self.JCs[index].active or Self.JCs[index].activating) then
    raise Exception.Create('JC postavena, nelze smazat');

  for var area: TArea in areas do
    if (Area.stack.IsJCInStack(Self.JCs[index])) then
      raise Exception.Create('JC v zasobniku OR ' + Area.id);

  if (Self.JCsStartSignal.ContainsKey(Self.JCs[index].signal as TBlkSignal)) then
    Self.JCsStartSignal[Self.JCs[index].signal as TBlkSignal].Remove(Self.JCs[index]);

  Self.JCs.Delete(index);

  // aktulizujeme indexy JC (dekrementujeme)
  for var i: Integer := index to Self.JCs.Count - 1 do
    Self.JCs[i].index := Self.JCs[i].index - 1;

  JCTableData.RemoveJC(index);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TJCDb.CancelAll();
begin
  for var JC: TJC in Self.JCs do
  begin
    if (JC.active) then
      JC.Cancel()
    else if (JC.activating) then
      JC.CancelActivating('Nouzové rušení stavění JC');
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TJCDb.FindJC(signalId: Integer; activatingToo: Boolean = false): TJC;
begin
  for var JC: TJC in Self.JCs do
    if (((JC.active) or ((activatingToo) and (JC.activating))) and (JC.data.signalId = signalId)) then
      Exit(JC);
  Result := nil;
end;

function TJCDb.FindJCActivating(signalId: Integer): TJC;
begin
  for var JC: TJC in Self.JCs do
    if ((JC.activating) and (JC.data.signalId = signalId)) then
      Exit(JC);
  Result := nil;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TJCDb.FindActiveJCs(finder: TJCContains; blockid: Integer): TList<TJC>;
begin
  Result := TList<TJC>.Create();
  try
    for var jc: TJC in Self.JCs do
      if (jc.active) and (finder(jc, blockid)) then
        Result.Add(jc);
  except
    Result.Free();
    raise;
  end;
end;

function TJCDb.FindActiveNCs(finder: TJCContains; blockid: Integer): TList<TJC>;
begin
  Result := TList<TJC>.Create();
  try
    for var jc: TJC in Self.JCs do
      if (jc.ncActive) and (finder(jc, blockid)) then
        Result.Add(jc);
  except
    Result.Free();
    raise;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////


function TJCDb.FindActiveJCsWithTurnout(turnout_id: Integer): TList<TJC>;
begin
  Result := Self.FindActiveJCs(TechnologieJC.ContainsTurnout, turnout_id);
end;

function TJCDb.FindActiveNCsWithTurnout(turnout_id: Integer): TList<TJC>;
begin
  Result := Self.FindActiveNCs(TechnologieJC.ContainsTurnout, turnout_id);
end;

function TJCDb.FindActiveJCWithTrack(track_id: Integer): TJC;
begin
  var jcs: TList<TJC> := Self.FindActiveJCs(TechnologieJC.ContainsTrackInclRailway, track_id);
  try
    if (jcs.Count > 0) then
      Result := jcs[0]
    else
      Result := nil;
  finally
    jcs.Free();
  end;
end;

function TJCDb.FindActiveJCsWithRailway(trat_id: Integer): TList<TJC>;
begin
  Result := Self.FindActiveJCs(TechnologieJC.ContainsRailway, trat_id);
end;

function TJCDb.FindActiveNCsWithRailway(trat_id: Integer): TList<TJC>;
begin
  Result := Self.FindActiveNCs(TechnologieJC.ContainsRailway, trat_id);
end;

function TJCDb.FindActiveJCsWithCrossing(blk_id: Integer): TList<TJC>;
begin
  Result := Self.FindActiveJCs(TechnologieJC.ContainsCrossing, blk_id);
end;

function TJCDb.FindActiveJCsWithLock(zam_id: Integer): TList<TJC>;
begin
  Result := Self.FindActiveJCs(ContainsLock, zam_id);
end;

function TJCDb.FindActiveNCsWithLock(zam_id: Integer): TList<TJC>;
begin
  Result := Self.FindActiveNCs(ContainsLock, zam_id);
end;

function TJCDb.FindActiveNCsWithPSt(pst_id: Integer): TList<TJC>;
begin
  Result := TList<TJC>.Create();
  try
    var blk: TBlk := BlockDb.Blocks.GetBlkByID(pst_id);
    if ((blk = nil) or (blk.typ <> btPst)) then
      Exit();
    var pst: TBlkPst := TBlkPst(blk);
    for var trackid: Integer in pst.GetSettings().tracks do
    begin
      var jcs: TList<TJC> := Self.FindActiveNCs(TechnologieJC.ContainsTrackInclRailway, trackid);
      Result.AddRange(jcs);
      jcs.Free();
    end;
  except
    Result.Free();
    raise;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

// Jakmile dojde k nastaveni navestidla na ceste JC, tady se zkontroluje, zda-li
// se nahodou nema nejake navestidlo pred cestou JC rozsvitit jinak.
procedure TJCDb.UpdatePrevSignal(signal: TBlkSignal);
begin
  for var JC: TJC in Self.JCs do
  begin
    if ((JC.typ = TJCType.shunt) or (JC.data.nextSignalType <> TJCNextSignalType.signal) or
      (JC.data.nextSignalId <> signal.id) or ((not JC.active) and (not JC.activating))) then
      continue; // musime prenest navest i na predchozi JC, kde se momentalne stavi navestidlo na volnoznak

    var prevSignal := Blocks.GetBlkSignalByID(JC.data.signalId);

    if (not prevSignal.IsTargetGoSignal()) then
      continue;

    var code: TBlkSignalCode := ncStuj;
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

    Log('Předchozí návěstidlo '+prevSignal.name+': nastavuji návěst '+TBlkSignal.SignalToString(code)+' ...', llInfo);
    prevSignal.signal := code;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

// rusi cestu, ve ktere je zadany blok
procedure TJCDb.Cancel(blk: TBlk);
begin
  var JCs: TList<TJC> := nil;
  try
    case (blk.typ) of
      btTurnout:
          JCs := JCDb.FindActiveJCsWithTurnout(blk.id);
      btCrossing:
          JCs := JCDb.FindActiveJCsWithCrossing(blk.id);
      btTrack, btRT:
        begin
          JCs := TList<TJC>.Create();
          var JC := JCDb.FindActiveJCWithTrack(blk.id);
          if (JC <> nil) then
            JCs.Add(JC);
        end;
      btSignal:
        begin
          JCs := TList<TJC>.Create();
          var JC: TJC := JCDb.FindJC(blk.id);
          if (JC <> nil) then
            JCs.Add(JC);
        end;
      btRailway:
          JCs := JCDb.FindActiveJCsWithRailway(blk.id);
      btLock:
          JCs := JCDb.FindActiveJCsWithLock(blk.id);
    else
      JCs := TList<TJC>.Create();
    end; // case

    for var JC: TJC in JCs do
    begin
      var tmpSignal := Blocks.GetBlkSignalByID(JC.data.signalId);
      if ((tmpSignal <> nil) and (tmpSignal.DNjc = JC) and
          ((tmpSignal.IsGoSignal(TJCType.train) or (tmpSignal.IsGoSignal(TJCType.shunt))) or (tmpSignal.ZAM) or
          (JC.waitForLastTrackOrRailwayOccupy))) then
      begin
        JC.EmergencyCancelActivePath();
      end else begin
        JC.EmergencyStopTrainInVC();
      end;
    end;
  finally
    if (Assigned(JCs)) then
      JCs.Free();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TJCDb.GetCount(): Word;
begin
  Result := Self.JCs.Count;
end;

/// /////////////////////////////////////////////////////////////////////////////

// najde index pro novou jizdni cestu
function TJCDb.FindPlaceForNewJC(id: Integer): Integer;
var i: Integer;
begin
  i := Self.JCs.Count - 1;
  while ((i >= 0) and (Self.JCs[i].id > id)) do
    i := i - 1;
  Result := i + 1;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TJCDb.IsJC(id: Integer; ignore_index: Integer = -1): Boolean;
var index: Integer;
begin
  index := Self.GetJCIndex(id);
  Result := ((index <> -1) and (index <> ignore_index));
end;

/// /////////////////////////////////////////////////////////////////////////////
// Hledame JC se zadanym ID v seznamu bloku pomoci binarniho vyhledavani.

function TJCDb.GetJCIndex(id: Integer): Integer;
var left, right, mid: Integer;
begin
  left := 0;
  right := Self.JCs.Count - 1;

  while (left <= right) do
  begin
    mid := (left + right) div 2;
    if (Self.JCs[mid].id = id) then
      Exit(mid);

    if (Self.JCs[mid].id > id) then
      right := mid - 1
    else
      left := mid + 1;
  end;
  Result := -1;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TJCDb.GetJCByID(id: Integer): TJC;
var index: Integer;
begin
  Result := nil;
  index := Self.GetJCIndex(id);
  if (index > -1) then
    Result := Self.JCs[index];
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TJCDb.JCOnIDChanged(Sender: TObject);
var new_index, min_index, index: Integer;
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
  if (index = new_index) then
    Exit();

  // od nejmensiho prohazovaneho indexu aktualizujeme indexy
  // aktualizjeme dokud indexy nesedi
  min_index := Min(new_index, index);
  for var i: Integer := min_index to Self.JCs.Count - 1 do
    if (Self.JCs[i].index = i) then
      break
    else
      Self.JCs[i].index := i;

  JCTableData.MoveJC(index, new_index);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TJCDb.UpdateIndexes();
begin
  for var i: Integer := 0 to Self.JCs.Count - 1 do
    Self.JCs[i].index := i;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TJCDb.FillJCsStartSignal();
begin
  Self.JCsStartSignal.Clear();
  for var JC: TJC in Self.JCs do
  begin
    if ((JC.signal <> nil) and (JC.signal.typ = btSignal)) then
    begin
      var signal: TBlkSignal := JC.signal as TBlkSignal;
      if (not Self.JCsStartSignal.ContainsKey(signal)) then
        Self.JCsStartSignal.Add(signal, TList<TJC>.Create());
      Self.JCsStartSignal[signal].Add(JC);
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TJCDb.JCOnNavChanged(Sender: TObject; origNav: TBlk);
var signal: TBlkSignal;
  JC: TJC;
begin
  signal := origNav as TBlkSignal;
  JC := Sender as TJC;

  if (origNav <> nil) then
  begin
    if (Self.JCsStartSignal.ContainsKey(signal)) then
      if (Self.JCsStartSignal[signal].Contains(JC)) then
        Self.JCsStartSignal[signal].Remove(JC);
  end;

  if (JC.signal <> nil) then
  begin
    if (not Self.JCsStartSignal.ContainsKey(JC.signal as TBlkSignal)) then
      Self.JCsStartSignal.Add(JC.signal as TBlkSignal, TList<TJC>.Create());
    Self.JCsStartSignal[JC.signal as TBlkSignal].Add(JC);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TJCDb.IsAnyJC(signal: TBlkSignal): Boolean;
begin
  Result := Self.JCsStartSignal.ContainsKey(signal) and (Self.JCsStartSignal[signal].Count > 0);
end;

function TJCDb.IsAnyVC(signal: TBlkSignal): Boolean;
begin
  if (not Self.JCsStartSignal.ContainsKey(signal)) then
    Exit(false);
  for var JC: TJC in Self.JCsStartSignal[signal] do
    if (JC.typ = TJCType.train) then
      Exit(true);
  Result := false;
end;

function TJCDb.IsAnyPC(signal: TBlkSignal): Boolean;
begin
  if (not Self.JCsStartSignal.ContainsKey(signal)) then
    Exit(false);
  for var JC: TJC in Self.JCsStartSignal[signal] do
    if (JC.typ = TJCType.shunt) then
      Exit(true);
  Result := false;
end;

/// /////////////////////////////////////////////////////////////////////////////
// Zjistuje, jestli je mozno postvit z navestidla \nav v aktualni situaci alespon
// jednu cestu typu \typ.

function TJCDb.IsAnyJCAvailable(signal: TBlkSignal; typ: TJCType): Boolean;
begin
  if (not Self.JCsStartSignal.ContainsKey(signal)) then
    Exit(false);
  for var JC: TJC in Self.JCsStartSignal[signal] do
  begin
    if ((JC.typ = typ) and (JC.data.tracks.Count > 0)) then
    begin
      var track := Blocks.GetBlkTrackOrRTByID(JC.data.tracks[0]);
      if ((track <> nil) and (track.Zaver = TZaver.no) and (track.occupied = TTrackState.Free)) then
        Exit(true);
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

/// /////////////////////////////////////////////////////////////////////////////

function TJCDb.GetItem(i: Integer): TJC;
begin
  Result := Self.JCs[i];
end;

function TJCDb.GetEnumerator(): TEnumerator<TJC>;
begin
  Result := Self.JCs.GetEnumerator();
end;

/// /////////////////////////////////////////////////////////////////////////////

function TJCDb.IsAnyJCWithPrefix(blocks: TList<TBlk>): Boolean;
begin
  if ((blocks.Count < 1) or (blocks[0].typ <> btSignal)) then
    Exit(false);

  var startSignal: TBlkSignal := TBlkSignal(blocks[0]);
  if (not Self.JCsStartSignal.ContainsKey(startSignal)) then
    Exit(false);

  for var JC: TJC in Self.JCsStartSignal[startSignal] do
  begin
    if (JC.signal <> startSignal) then
      continue;

    if ((Integer(startSignal.selected) = Integer(JC.typ)) or ((startSignal.selected = TBlkSignalSelection.NC) and
      (JC.typ = TJCType.train)) or ((startSignal.selected = TBlkSignalSelection.PP) and (JC.typ = TJCType.shunt))) then
    begin
      // kontrola variantnich bodu:
      if (blocks.Count > JC.data.vb.Count+1) then
        continue;

      var error: Boolean := false;
      for var j: Integer := 1 to blocks.Count-1 do
        if (JC.data.vb[j-1] <> blocks[j].id) then
          error := true;
      if (error) then
        continue;

      Exit(true);
    end;
  end;

  Result := false;
end;

/// /////////////////////////////////////////////////////////////////////////////

initialization

JCDb := TJCDb.Create();

finalization

FreeAndNil(JCDb);

end.// unit
