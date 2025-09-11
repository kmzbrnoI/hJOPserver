unit BlockDb;

{
  Database of all technological blocks.

  - TBlock class holds database of all existing technological blocks.
  - TBlk is an abstract parent type of all block types; it holds common information
    (name etc.)
  - TBlocks in created as a shred variable.
  - OnChange event from block is passed to areas. Areas send this event to panels (clients).

  v4.3.7 update: all blocks are sorted according to id, search is O(log n) - binary search.
}

interface

uses IniFiles, Block, SysUtils, Windows, AreaDb, Area, StdCtrls,
  Generics.Collections, Classes, IdContext, RCSc, RCSsc,
  JsonDataObjects, Train, System.Math,
  BlockTrack, BlockTurnout, BlockIR, BlockLock, BlockRailway, BlockGroupSignal,
  BlockLinker, BlockAC, BlockRailwayTrack, BlockPst, BlockSignal, BlockSummary,
  BlockCrossing, BlockDisconnector;

CONST
  BLK_NOT_FOUND: Integer = -1;
  BLK_MULTIPLE: Integer = -2;

type
  EBlockNotFound = class(Exception);
  EMultipleBlocks = class(Exception);

  TBlocks = class(TObject)
  private
    data: TObjectList<TBlk>; // owns blocks
    nameToBlk: TDictionary<string, TBlk>; // does not own blocks

    ffstatus: string;
    ffile: string;
    fenabled: Boolean;

    class function NewBlk(typ: TBlkType; index: Integer): TBlk;
    procedure BlkChange(Sender: TObject);

    function GetCount(): Integer;

    function FindPlaceForNewBlk(id: Integer): Integer;
    procedure UpdateBlkIndexes(); // update indexes of all blocks
    function GetItem(i: Integer): TBlk;

  public
    constructor Create();
    destructor Destroy(); override;

    procedure LoadFromFile(const tech_filename, rel_filename, stat_filename: string);
    procedure SaveToFile(const tech_filename: string);
    procedure SaveStatToFile(const stat_filename: string);

    function Add(glob: TBlkSettings): TBlk;
    procedure Delete(index: Integer);

    function GetBlkByIndex(index: Integer): TBlk;
    function SetBlk(index: Integer; data: TBlk): Integer;

    procedure Enable();
    procedure Disable();
    procedure Reset();

    procedure Update();

    function GetBlkIndex(id: Integer): Integer;
    function GetBlkByID(id: Integer): TBlk;
    function GetBlkID(index: Integer): Integer; overload;
    function GetBlkID(name: string): Integer; overload;
    function GetBlkIDExc(name: string): Integer;
    function GetBlkName(id: Integer): string;
    function GetBlkByName(name: string): TBlk;
    function GetBlkIndexName(index: Integer): string;

    function GetBlkTrackOrRTByID(id: Integer): TBlkTrack;
    function GetBlkRTByID(id: Integer): TBlkRT;
    function GetBlkTurnoutByID(id: Integer): TBlkTurnout;
    function GetBlkSignalByID(id: Integer): TBlkSignal;
    function GetBlkCrossingByID(id: Integer): TBlkCrossing;
    function GetBlkRailwayByID(id: Integer): TBlkRailway;
    function GetBlkLinkerByID(id: Integer): TBlkLinker;
    function GetBlkLockByID(id: Integer): TBlkLock;
    function GetBlkIrByID(id: Integer): TBlkIR;
    function GetBlkDisconnectorByID(id: Integer): TBlkDisconnector;

    function GetBlkTrackTrainMoving(obl: string): TBlk;

    function PNSignals(Area: TArea): TBlksList;

    // send state of all blocks in area 'areaId' to 'conn'
    procedure GetAreaBlk(areaId: string; conn: TIdContext);

    // Check if block with id 'id' already exists.
    // Ignore block 'ignore_index'.
    function IsBlock(id: Integer; ignore_index: Integer = -1): Boolean; overload;
    function IsBlock(name: string; ignore_index: Integer = -1): Boolean; overload;

    procedure OnBoosterChange(booster: string);

    // state = true: apply NUZ for all nuz-selected blocks in 'areaId'
    // state = false: cancel NUZ for all nuz-selected blocks in 'areaId'
    procedure NUZ(areaId: string; state: Boolean = true);

    procedure FillCB(var cb: TComboBox; var items: TList<Integer>; const ignore: TList<Integer>;
      const areas: TList<TArea>; blkType: TBlkType; blkType2: TBlkType = btAny; blockId: Integer = -1); overload;

    procedure RemoveTrain(Train: TTrain);
    procedure TrainPrediction(signal: TBlkSignal);

    function GetBlkWithTrain(Train: TTrain): TBlksList;
    function GetTurnoutWithLock(zamekID: Integer): TBlksList;
    function GetTurnoutsAtTrack(trackId: Integer): TList<TBlk>;

    // call 'Change' on all tracks with train 'Train'
    procedure ChangeAllTracksWithTrain(Train: TTrain);

    // call 'Change' on all railways with train 'Train'
    procedure ChangeTrainToAllRailways(Train: TTrain);

    procedure BlkIDChanged(index: Integer);
    procedure BlkNameChanged(previous: string; index: Integer);
    procedure ClearPOdj();

    procedure GetPtData(json: TJsonObject; includeState: Boolean; Area: TArea = nil; typ: TBlkType = btAny);

    procedure NouzZaverZrusen(Sender: TBlk);
    procedure MoveTurnoutBasicPosition();

    function AnotherBlockUsesRCS(addr: TRCSsAddr; me: TBlk; typ: TRCSIOType): TBlk;

    procedure OnClientDisconnect(client: TIdContext);

    function GetEnumerator(): TEnumerator<TBlk>;
    property items[index: Integer]: TBlk read GetItem; default;
    property count: Integer read GetCount;

    property fstatus: string read ffstatus;
    property filename: string read ffile;
    property enabled: Boolean read fenabled;
  end;

var
  Blocks: TBlocks;

implementation

uses fMain, TJCDatabase, Logging, DataBloky, TrainDb, TechnologieJC,
  AreaStack, GetSystems, appEv, BlockIO, PTUtils, TechnologieAB,
  ACBlocks, TCPServerPanel;

/// /////////////////////////////////////////////////////////////////////////////

constructor TBlocks.Create();
begin
  inherited;
  Self.data := TObjectList<TBlk>.Create();
  Self.nameToBlk := TDictionary<string, TBlk>.Create();
  Self.fenabled := false;
end;

destructor TBlocks.Destroy();
begin
  FreeAndNil(Self.nameToBlk);
  FreeAndNil(Self.data);
  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

// This event is called on change of any block.
// This implementation solves all connections of blocks (e.g. track change causes on-track turnouts change etc.)
// Send event to areas.
procedure TBlocks.BlkChange(Sender: TObject);
begin
  if ((TBlk(Sender).typ = btTrack) or (TBlk(Sender).typ = btRT)) then
  begin
    // track change -> on-track turnouts change
    var turnouts: TList<TBlk> := Self.GetTurnoutsAtTrack(TBlk(Sender).id);
    try
      for var turnout: TBlk in turnouts do
        turnout.Change(true);
    finally
      turnouts.Free();
    end;
  end;

  // call 'BlkChange' to areas
  var areas: TList<TArea> := (Sender as TBlk).areas;
  if (areas.count > 0) then
    for var area: TArea in areas do
      Area.BlkChange(Sender);

  ACBlk.OnBlkChange(TBlk(Sender).id);
end;

/// /////////////////////////////////////////////////////////////////////////////

class function TBlocks.NewBlk(typ: TBlkType; index: Integer): TBlk;
begin
  case (typ) of
    btTurnout:
      Result := TBlkTurnout.Create(index);
    btTrack:
      Result := TBlkTrack.Create(index);
    btIR:
      Result := TBlkIR.Create(index);
    btSignal:
      Result := TBlkSignal.Create(index);
    btCrossing:
      Result := TBlkCrossing.Create(index);
    btRailway:
      Result := TBlkRailway.Create(index);
    btLinker:
      Result := TBlkLinker.Create(index);
    btLock:
      Result := TBlkLock.Create(index);
    btDisconnector:
      Result := TBlkDisconnector.Create(index);
    btRT:
      Result := TBlkRT.Create(index);
    btIO:
      Result := TBlkIO.Create(index);
    btSummary:
      Result := TBlkSummary.Create(index);
    btAC:
      Result := TBlkAC.Create(index);
    btGroupSignal:
      Result := TBlkGroupSignal.Create(index);
    btPst:
      Result := TBlkPst.Create(index);
  else
    Result := nil;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

// load all blocks from file
// Use default index '-1' when loading, index propely later ('UpdateBlkIndexes')
procedure TBlocks.LoadFromFile(const tech_filename, rel_filename, stat_filename: string);
var ini_tech, ini_rel, ini_stat: TMemIniFile;
  str: TStrings;
begin
  Log('Načítám bloky: ' + tech_filename + '; ' + rel_filename, llInfo, lsData);
  Self.ffile := tech_filename;
  Self.ffstatus := stat_filename;

  ini_tech := TMemIniFile.Create(tech_filename, TEncoding.UTF8);
  ini_rel := TMemIniFile.Create(rel_filename, TEncoding.UTF8);
  ini_stat := TMemIniFile.Create(stat_filename, TEncoding.UTF8);
  str := TStringList.Create();

  var blk: TBlk := nil;
  try
    Self.data.Clear();
    ini_tech.ReadSections(str);

    for var section: string in str do
    begin
      try
        var id: Integer := StrToIntDef(section, -1);
        if (id < 0) then
        begin
          Log('Nenačítám blok ' + section + ' - id není validní', llError, lsData);
          continue;
        end;

        if (Self.IsBlock(id)) then
        begin
          Log('Nenačítám blok ' + section + ' - blok s tímto id již existuje', llError, lsData);
          continue;
        end;

        var typeint: Integer := ini_tech.ReadInteger(section, 'typ', -1);
        blk := Self.NewBlk(TBlkType(typeint), -1);

        if (blk = nil) then
        begin
          Log('Nenačítám blok ' + section + ' - neznámý typ', llError, lsData);
        end else begin
          blk.LoadData(ini_tech, section, ini_rel, ini_stat);
          blk.OnChange := Self.BlkChange;

          if (blk.name = '') then
          begin
            Log('Nenačítám blok ' + blk.idName + ' - blok má prázdné jméno', llError, lsData);
          end else if (Self.GetBlkByName(blk.name) <> nil) then
          begin
            Log('Nenačítám blok ' + blk.idName + ' - blok tohoto jména již existuje', llError, lsData);
          end else begin
            Self.data.Insert(Self.FindPlaceForNewBlk(blk.id), blk);
            Self.nameToBlk.Add(blk.name, blk);
            blk := nil;
          end;
        end;
      except
        on E: Exception do
        begin
          if (Assigned(blk)) then
            blk.Free();
          AppEvents.LogException(E, 'Načítání bloku ' + section);
        end;
      end;
    end;

    Self.UpdateBlkIndexes();
  finally
    FreeAndNil(ini_tech);
    FreeAndNil(ini_rel);
    FreeAndNil(ini_stat);
    FreeAndNil(str);
  end;

  for blk in Self.data do
    blk.AfterLoad();

  Log('Načteno bloků: ' + IntToStr(Self.count), llInfo, lsData);
end;

procedure TBlocks.SaveToFile(const tech_filename: string);
var ini: TMemIniFile;
begin
  Log('Ukládám bloky...', llInfo, lsData);

  try
    DeleteFile(PChar(tech_filename)); // all data will be rewrited
    ini := TMemIniFile.Create(tech_filename, TEncoding.UTF8);
  except
    on E: Exception do
    begin
      AppEvents.LogException(E, 'Ukládám bloky: nelze zapsat výstupni soubor');
      Exit();
    end;
  end;

  for var blk: TBlk in Self.data do
    blk.SaveData(ini, IntToStr(Blk.id));

  ini.UpdateFile();
  FreeAndNil(ini);

  Log('Uloženo bloků: ' + IntToStr(Self.count), llInfo, lsData);

  Self.SaveStatToFile(Self.fstatus);
end;

procedure TBlocks.SaveStatToFile(const stat_filename: string);
var ini: TMemIniFile;
begin
  Log('Ukládám stavy bloků...', llInfo, lsData);

  try
    DeleteFile(PChar(stat_filename));
    ini := TMemIniFile.Create(stat_filename, TEncoding.UTF8);
  except
    on E: Exception do
    begin
      AppEvents.LogException(E, 'Ukládám stavy bloků: nelze zapsat výstupní soubor');
      Exit();
    end;
  end;

  for var blk: TBlk in Self.data do
  begin
    try
      Blk.SaveState(ini, IntToStr(Blk.id));
    except
      on E: Exception do
        AppEvents.LogException(E, 'Save blok ' + Blk.name);
    end;
  end;

  ini.UpdateFile();
  FreeAndNil(ini);

  Log('Uložen stav ' + IntToStr(Self.count) + ' bloků', llInfo, lsData);
end;

/// /////////////////////////////////////////////////////////////////////////////

// add 1 block
function TBlocks.Add(glob: TBlkSettings): TBlk;
var blk: TBlk;
  index: Integer;
begin
  if (glob.name = '') then
    raise EArgumentNilException.Create('Jméno bloku nemůže být prázdné!');
  if (Self.IsBlock(glob.id)) then
    raise EInvalidArgument.Create('Blok s ID '+IntToStr(glob.id)+' již existuje!');
  if (Self.GetBlkByName(glob.name) <> nil) then
    raise EInvalidArgument.Create('Blok s názvem "'+glob.name+'" již existuje!');

  index := Self.FindPlaceForNewBlk(glob.id);
  blk := Self.NewBlk(glob.typ, index);
  if (blk = nil) then
    Exit(nil);

  blk.SetGlobalSettings(glob);
  blk.OnChange := Self.BlkChange;

  Self.data.Insert(index, blk);
  Self.nameToBlk.Add(glob.name, blk);

  BlocksTablePainter.BlkAdd(index);

  // move indexes
  for var i: Integer := index + 1 to Self.data.count - 1 do
    Self.data[i].table_index := Self.data[i].table_index + 1;

  Result := blk;
end;

procedure TBlocks.Delete(index: Integer);
begin
  if (index < 0) then
    raise ERangeError.Create('Index podtekl seznam bloků');
  if (index >= Self.data.count) then
    raise ERangeError.Create('Index přetekl seznam bloků');

  var deleted: TBlk := Self.data[index];

  if ((deleted.typ = btRT) and (TBlkRT(deleted).inRailway > -1)) then
    raise Exception.Create('Tento blok je zaveden jako traťový úsek v trati ID ' + IntToStr((deleted as TBlkRT).inRailway));
  if ((deleted.typ = btSignal) and (TBlkSignal(deleted).groupMaster <> nil)) then
    raise Exception.Create('Toto návěstidlo je zavedeno ve skupinovém návěstidle ' + TBlkSignal(deleted).groupMaster.name);

  // Delete but do not destroy
  Self.data.OwnsObjects := False;
  Self.data.Delete(index);
  Self.data.OwnsObjects := True;

  if ((Assigned(Self.nameToBlk)) and (Self.nameToBlk.ContainsKey(deleted.name)) and (Self.nameToBlk[deleted.name] = deleted)) then
    Self.nameToBlk.Remove(deleted.name);

  // update indexes (decrement)
  for var i: Integer := index to Self.data.count - 1 do
    Self.data[i].table_index := Self.data[i].table_index - 1;

  // railway deletion -> linker deletion
  if (deleted.typ = btRailway) then
  begin
    var aindex: Integer := Self.GetBlkIndex((deleted as TBlkRailway).GetSettings().linkerA);
    if (aindex >= 0) then
      Self.Delete(aindex);

    var bindex: Integer := Self.GetBlkIndex((deleted as TBlkRailway).GetSettings().linkerB);
    if (bindex >= 0) then
      Self.Delete(bindex);
  end;
  if (deleted.typ = btLinker) then
  begin
    var i: Integer := Blocks.GetBlkIndex((deleted as TBlkLinker).GetSettings.parent);
    if (i >= 0) then
      Self.Delete(i);
  end;

  PanelServer.BlockRemoved(deleted);

  FreeAndNil(deleted);
  BlocksTablePainter.BlkRemove(index);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlocks.GetBlkByIndex(index: Integer): TBlk;
begin
  if ((index < 0) or (index >= Self.data.count)) then
    Exit(nil);
  Result := Self.data[index];
end;

function TBlocks.SetBlk(index: Integer; data: TBlk): Integer;
begin
  if ((index < 0) or (index >= Self.data.count)) then
    Exit(1);
  Self.data[index] := data;
  Result := 0;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlocks.Enable();
begin
  for var blk: TBlk in Self.data do
  begin
    try
      blk.Enable();
    except
      on E: Exception do
      begin
        E.Message := 'Blok ' + blk.idName + ': ' + E.Message;
        raise;
      end;
    end;
  end;

  Self.fenabled := true;
  BlocksTablePainter.reload := true;
  BlocksTablePainter.UpdateTable();
end;

procedure TBlocks.Disable();
begin
  for var blk: TBlk in Self.data do
    blk.Disable();
  Self.fenabled := false;
  BlocksTablePainter.reload := true;
  BlocksTablePainter.UpdateTable();
end;

procedure TBlocks.Reset();
var Blk: TBlk;
begin
  for Blk in Self.data do
    Blk.Reset();
  BlocksTablePainter.reload := true;
  BlocksTablePainter.UpdateTable();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlocks.Update();
begin
  if (not Self.enabled) then
    Exit();

  for var blk: TBlk in Self.data do
  begin
    try
      blk.Update();
    except
      on E: Exception do
      begin
        if (not log_last_error) then
          AppEvents.LogException(E, 'Blok ' + Blk.name + ' update error');
      end;
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlocks.GetBlkIndex(id: Integer): Integer;
var left, right, mid: Integer;
begin
  left := 0;
  right := Self.data.count - 1;

  while (left <= right) do
  begin
    mid := (left + right) div 2;
    if (Self.data[mid].id = id) then
      Exit(mid);

    if (Self.data[mid].id > id) then
      right := mid - 1
    else
      left := mid + 1;
  end;
  Result := -1;
end;

function TBlocks.GetBlkByID(id: Integer): TBlk;
begin
  Result := Self.GetBlkByIndex(Self.GetBlkIndex(id));
end;

function TBlocks.GetBlkTrackOrRTByID(id: Integer): TBlkTrack;
begin
  Result := nil;
  var blk: TBlk := Self.GetBlkByID(id);
  if ((blk <> nil) and ((blk.typ = btTrack) or (blk.typ = btRT))) then
    Result := TBlkTrack(blk);
end;

function TBlocks.GetBlkRTByID(id: Integer): TBlkRT;
begin
  Result := nil;
  var blk: TBlk := Self.GetBlkByID(id);
  if ((blk <> nil) and (blk.typ = btRT)) then
    Result := TBlkRT(blk);
end;

function TBlocks.GetBlkTurnoutByID(id: Integer): TBlkTurnout;
begin
  Result := nil;
  var blk: TBlk := Self.GetBlkByID(id);
  if ((blk <> nil) and (blk.typ = btTurnout)) then
    Result := TBlkTurnout(blk);
end;

function TBlocks.GetBlkSignalByID(id: Integer): TBlkSignal;
begin
  Result := nil;
  var blk: TBlk := Self.GetBlkByID(id);
  if ((blk <> nil) and (blk.typ = btSignal)) then
    Result := TBlkSignal(blk);
end;

function TBlocks.GetBlkCrossingByID(id: Integer): TBlkCrossing;
begin
  Result := nil;
  var blk: TBlk := Self.GetBlkByID(id);
  if ((blk <> nil) and (blk.typ = btCrossing)) then
    Result := TBlkCrossing(blk);
end;

function TBlocks.GetBlkRailwayByID(id: Integer): TBlkRailway;
begin
  Result := nil;
  var blk: TBlk := Self.GetBlkByID(id);
  if ((blk <> nil) and (blk.typ = btRailway)) then
    Result := TBlkRailway(blk);
end;

function TBlocks.GetBlkLinkerByID(id: Integer): TBlkLinker;
begin
  Result := nil;
  var blk: TBlk := Self.GetBlkByID(id);
  if ((blk <> nil) and (blk.typ = btLinker)) then
    Result := TBlkLinker(blk);
end;

function TBlocks.GetBlkLockByID(id: Integer): TBlkLock;
begin
  Result := nil;
  var blk: TBlk := Self.GetBlkByID(id);
  if ((blk <> nil) and (blk.typ = btLock)) then
    Result := TBlkLock(blk);
end;

function TBlocks.GetBlkIrByID(id: Integer): TBlkIR;
begin
  Result := nil;
  var blk: TBlk := Self.GetBlkByID(id);
  if ((blk <> nil) and (blk.typ = btIr)) then
    Result := TBlkIR(blk);
end;

function TBlocks.GetBlkDisconnectorByID(id: Integer): TBlkDisconnector;
begin
  Result := nil;
  var blk: TBlk := Self.GetBlkByID(id);
  if ((blk <> nil) and (blk.typ = btDisconnector)) then
    Result := TBlkDisconnector(blk);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlocks.GetAreaBlk(areaId: string; conn: TIdContext);
begin
  for var blk: TBlk in Self.data do
  begin
    var areas: TList<TArea> := Blk.areas;

    for var area: TArea in areas do
    begin
      if (area.id = areaId) then
      begin
        area.BlkChange(blk, conn);
        Break;
      end;
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlocks.IsBlock(id: Integer; ignore_index: Integer = -1): Boolean;
var index: Integer;
begin
  index := Self.GetBlkIndex(id);
  Result := ((index <> -1) and (index <> ignore_index));
end;

function TBlocks.IsBlock(name: string; ignore_index: Integer = -1): Boolean;
begin
  var blk: TBlk := Self.GetBlkByName(name);
  if (blk = nil) then
    Exit(False);
  var index: Integer := Self.GetBlkIndex(blk.id);
  Result := (index > -1) and (index <> ignore_index);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlocks.GetBlkID(index: Integer): Integer;
begin
  if (index < 0) or (index >= Self.data.count) then
    Exit(-1);
  Result := Self.data[index].id;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlocks.GetBlkID(name: string): Integer;
begin
  var blk: TBlk := Self.GetBlkByName(name);
  if (blk = nil) then
    Exit(BLK_NOT_FOUND);
  Result := blk.id;
end;

function TBlocks.GetBlkIDExc(name: string): Integer;
begin
  Result := Self.GetBlkID(name);
  if (Result = BLK_NOT_FOUND) then
    raise BlockDb.EBlockNotFound.Create('Block not found: '+name);
end;

function TBlocks.GetBlkByName(name: string): TBlk;
begin
  if (not Assigned(Self.nameToBlk)) then
    Exit(nil);
  if (not Self.nameToBlk.ContainsKey(name)) then
    Exit(nil);
  Result := Self.nameToBlk[name];
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlocks.GetBlkName(id: Integer): string;
var blk: TBlk;
begin
  blk := Self.GetBlkByID(id);
  if (not Assigned(blk)) then
    Exit('## Blok s timto ID neexistuje ##');
  Result := blk.name;
end;

function TBlocks.GetBlkIndexName(index: Integer): string;
begin
  if (index < 0) or (index >= Self.data.count) then
    Exit('## Blok s timto ID neexistuje ##');
  Result := Self.data[index].name;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlocks.GetBlkTrackTrainMoving(obl: string): TBlk;
begin
  for var blk: TBlk in Self.data do
  begin
    if ((Blk.typ <> btTrack) and (Blk.typ <> btRT)) then
      continue;

    var orindex: Integer := -1;
    for var j: Integer := 0 to (Blk as TBlkTrack).areas.count - 1 do
      if ((Blk as TBlkTrack).areas[j].id = obl) then
        orindex := j;

    if (orindex = -1) then
      continue;
    if ((Blk as TBlkTrack).IsTrainMoving()) then
      Exit(Blk);
  end;

  Result := nil;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlocks.OnBoosterChange(booster: string);
begin
  if (not Self.enabled) then
    Exit();

  for var blk: TBlk in Self.data do
    if ((Blk.typ = btTrack) or (Blk.typ = btRT)) then
      if ((booster = '') or (TBlkTrack(Blk).GetSettings().boosterId = booster)) then
        TBlkTrack(Blk).OnBoosterChange();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlocks.NUZ(areaId: string; state: Boolean = true);
begin
  for var blk: TBlk in Self.data do
  begin
    if (Blk.typ <> btTrack) then
      continue;
    var track: TBlkTrack := (Blk as TBlkTrack);
    if (not track.NUZ) then
      continue;

    for var area: TArea in track.areas do
    begin
      if (area.id = areaId) then
      begin
        if (state) then
        begin
          for var traini: Integer in track.trains do
            if (Self.GetBlkWithTrain(trains[traini]).count = 1) then
              trains.Remove(traini);

          if (ABlist.IsTrackInAnyABJC(track.id)) then
            track.Zaver := TZaver.ab
          else
            track.Zaver := TZaver.no;

          track.RemoveTrains();
        end
        else
          track.NUZ := false;
      end;
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlocks.FillCB(var cb: TComboBox; var items: TList<Integer>; const ignore: TList<Integer>;
  const areas: TList<TArea>; blkType: TBlkType; blkType2: TBlkType = btAny; blockId: Integer = -1);
begin
  if (Assigned(items)) then
    items.Clear();
  cb.Clear();

  for var blocki: Integer := 0 to Blocks.count - 1 do
  begin
    var blk: TBlk := Blocks[blocki];
    var glob: TBlkSettings := Blk.GetGlobalSettings();

    if ((glob.typ <> blkType) and (glob.typ <> blkType2)) then
      continue;

    if ((Assigned(ignore)) and (ignore.Contains(glob.id))) then
      continue;

    var assign: Boolean;
    if ((Assigned(areas)) and (areas.Count > 0)) then
    begin
      assign := false;

      if ((glob.typ = btRailway) or (glob.typ = btIR)) then
        assign := true
      else if (Blk.areas.Count = 0) then
        assign := true
      else begin
        for var area in areas do
        begin
          if (Blk.areas.Contains(area)) then
          begin
            assign := true;
            Break;
          end;
        end;
      end;
    end else begin
      assign := true;
    end;

    if (not assign) then
      continue;

    if (Assigned(items)) then
      items.Add(glob.id);

    cb.items.Add(glob.name);
    if (glob.id = blockId) then
      cb.ItemIndex := cb.Items.Count-1;
  end;

  cb.enabled := (cb.items.count > 0);
  if (cb.items.count = 0) then
    CB.items.Add('Bloky nenalezeny');
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlocks.RemoveTrain(Train: TTrain);
begin
  for var blk: TBlk in Self.data do
  begin
    if ((Blk.typ = btTrack) or (Blk.typ = btRT)) then
    begin
      if ((Blk as TBlkTrack).IsTrain(Train)) then
        (Blk as TBlkTrack).RemoveTrain(Train);

      if ((Blk as TBlkTrack).trainPredict = Train) then
        (Blk as TBlkTrack).trainPredict := nil;
    end;
    if (Blk.typ = btRailway) then
      (Blk as TBlkRailway).RemoveTrain(Train);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlocks.GetBlkWithTrain(Train: TTrain): TBlksList;
begin
  Result := TList<TBlk>.Create();
  try
    for var blk: TBlk in Self.data do
      if (((Blk.typ = btTrack) or (Blk.typ = btRT)) and ((Blk as TBlkTrack).IsTrain(Train))) then
        Result.Add(Blk);
  except
    Result.Free();
  end;
end;

function TBlocks.GetTurnoutsAtTrack(trackId: Integer): TList<TBlk>;
begin
  Result := TList<TBlk>.Create();
  try
    for var blk: TBlk in Self.data do
      if (blk.typ = btTurnout) then
        if ((blk as TBlkTurnout).trackID = trackId) then
          Result.Add(blk);
  except
    Result.Free();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlocks.TrainPrediction(signal: TBlkSignal);
var track, startTrack: TBlkTrack;
  railway: TBlkRailway;
  train: TTrain;
  JC: TJC;
begin
  if (signal = nil) then
    Exit();
  track := nil;

  try
    // get train on track before signal
    track := TBlkTrack(signal.track);
    startTrack := track;
    train := signal.GeTTrain(track);

    if (signal.IsGoSignal()) then
    begin
      if ((not track.IsTrain()) or (Train.direction <> signal.direction)) then
        train := track.trainPredict
    end
    else
      train := nil;
    JC := signal.DNjc;

    // predict while paths exist
    while ((JC <> nil) and (JC.typ = TJCType.Train) and (JC.state.destroyBlock <= 0)) do
    begin
      // signal is go?
      signal := Blocks.GetBlkSignalByID(JC.data.signalId);
      if ((signal = nil) or (not signal.IsGoSignal())) then
        train := nil;

      // get last track of the path
      track := Blocks.GetBlkTrackOrRTByID(JC.data.tracks[JC.data.tracks.count - 1]);

      if (track = startTrack) then
        Exit();

      if ((track.typ = btRT) and (TBlkRT(track).inRailway > -1)) then
      begin
        // last track in railway -> continue on the other side of railway
        railway := Blocks.GetBlkRailwayByID(TBlkRT(track).inRailway);
        if (train <> nil) then
        begin
          if ((railway.trainPredict = nil) or (railway.trainPredict.Train <> train)) then
            railway.trainPredict := TBlkRailwayTrain.Create(train.index);
        end else begin
          if (railway.trainPredict <> nil) then
            railway.trainPredict := nil;
        end;

        // railway contains other trains -> exit
        if (railway.state.trains.count > 0) then
          Exit();
        railway.UpdateTrainPredict(false);

        case (railway.direction) of
          TRailwayDirection.AtoB:
            track := Blocks.GetBlkTrackOrRTByID(railway.GetSettings().trackIds[railway.GetSettings().trackIds.count - 1]);
          TRailwayDirection.BtoA:
            track := Blocks.GetBlkTrackOrRTByID(railway.GetSettings().trackIds[0]);
        end;

        // train was not propagated to end of railway -> exit (maybe some signal in autoblock locked? etc.)
        if ((track.trainPredict <> train) or (track = startTrack)) then
          Exit();
      end;

      track.trainPredict := train;

      // is any next path active?
      if (track.signalJCRef.count = 0) then
        JC := nil
      else
        JC := TBlkSignal(track.signalJCRef[0]).DNjc;
    end; // while
  except
    on E: Exception do
    begin
      if (track <> nil) then
        AppEvents.LogException(E, 'Vyjímka při předpovídání soupravy - úsek ' + track.name)
      else
        AppEvents.LogException(E, 'Vyjímka při předpovídání soupravy');
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlocks.PNSignals(Area: TArea): TBlksList;
begin
  Result := TList<TBlk>.Create();
  try
    for var blk: TBlk in Self.data do
    begin
      if (Blk.typ <> btSignal) then
        continue;
      if ((Blk as TBlkSignal).signal <> ncPrivol) then
        continue;

      for var marea: TArea in (Blk as TBlkSignal).areas do
      begin
        if (marea = Area) then
        begin
          Result.Add(Blk);
          Break;
        end;
      end;
    end;
  except
    Result.Free();
    raise;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlocks.GetTurnoutWithLock(zamekID: Integer): TBlksList;
begin
  Result := TBlksList.Create();
  for var blk: TBlk in Self.data do
  begin
    if ((Blk.typ = btTurnout) and ((Blk as TBlkTurnout).GetSettings().lock = zamekID)) then
      Result.Add(Blk);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlocks.ChangeAllTracksWithTrain(Train: TTrain);
begin
  var blks := Self.GetBlkWithTrain(Train);
  try
    for var blk: TBlk in blks do
      blk.Change();
  finally
    blks.Free();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlocks.GetCount(): Integer;
begin
  Result := Self.data.count;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlocks.ChangeTrainToAllRailways(train: TTrain);
begin
  for var blk: TBlk in Self.data do
    if ((Blk.typ = btRailway) and (Blk as TBlkRailway).IsTrain(train, true)) then
      Blk.Change();
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlocks.FindPlaceForNewBlk(id: Integer): Integer;
var i: Integer;
begin
  i := Self.data.count - 1;
  while ((i >= 0) and (Self.data[i].id > id)) do
    i := i - 1;
  Result := i + 1;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlocks.UpdateBlkIndexes();
begin
  for var i: Integer := 0 to Self.data.count - 1 do
    Self.data[i].table_index := i;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlocks.BlkIDChanged(index: Integer);
var new_index, min_index: Integer;
  tmp: TBlk;
begin
  tmp := Self.data[index];

  Self.data.OwnsObjects := False;
  Self.data.Delete(index);
  Self.data.OwnsObjects := True;

  new_index := FindPlaceForNewBlk(tmp.id);

  Self.data.Insert(new_index, tmp);
  if (index = new_index) then
    Exit(); // position of block is not changed

  // update indexes from lowest-changed index
  // update while index mismatch
  min_index := Min(new_index, index);
  for var i: Integer := min_index to Self.data.count - 1 do
  begin
    if (Self.data[i].table_index = i) then
      Break
    else
      Self.data[i].table_index := i;
  end;

  BlocksTablePainter.BlkMove(index, new_index);
end;

procedure TBlocks.BlkNameChanged(previous: string; index: Integer);
begin
  var blk: TBlk := Self.data[index];
  if (previous = blk.name) then
    Exit();
  if ((Self.nameToBlk.ContainsKey(previous)) and (Self.nameToBlk[previous] = blk)) then
    Self.nameToBlk.Remove(previous);
  if (Self.GetBlkByName(blk.name) <> nil) then
    raise EMultipleBlocks.Create('Blok se jménem "'+blk.name+'" již existuje');
  Self.nameToBlk.AddOrSetValue(blk.name, blk);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlocks.GetPtData(json: TJsonObject; includeState: Boolean; Area: TArea = nil; typ: TBlkType = btAny);
begin
  for var blk: TBlk in Self.data do
  begin
    try
      if ((Area <> nil) and (not Blk.IsInArea(Area))) then
        continue;
      if ((typ <> btAny) and (Blk.typ <> typ)) then
        continue;

      Blk.GetPtData(json.A['blocks'].AddObject, includeState);
    except
      on E: Exception do
        PTUtils.PtErrorToJson(json.A['errors'].AddObject, '500', 'Chyba pri nacitani bloku ' + IntToStr(Blk.id) + ' : '
          + Blk.name, E.Message);
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlocks.NouzZaverZrusen(Sender: TBlk);
begin
  for var blk: TBlk in Self.data do
    if (Blk.typ = btSignal) then
      TBlkSignal(Blk).RemoveBlkFromRnz(Sender.id);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlocks.ClearPOdj();
begin
  for var blk: TBlk in Self.data do
    if ((Blk.typ = btTrack) or (Blk.typ = btRT)) then
      TBlkTrack(Blk).ClearPOdj();
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlocks.GetItem(i: Integer): TBlk;
begin
  Result := Self.data[i];
end;

function TBlocks.GetEnumerator(): TEnumerator<TBlk>;
begin
  Result := Self.data.GetEnumerator();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlocks.MoveTurnoutBasicPosition();
begin
  for var blk: TBlk in Self.data do
    if ((Blk.typ = btTurnout) and (TBlkTurnout(Blk).position <> TTurnoutPosition.plus) and
      (not TBlkTurnout(Blk).outputLocked) and (TBlkTurnout(Blk).occupied <> TTrackState.occupied) and
      ((TBlkTurnout(Blk).coupling = nil) or (TBlkTurnout(Blk).coupling.occupied <> TTrackState.occupied))) then
      TBlkTurnout(Blk).SetPosition(plus);
end;


/// /////////////////////////////////////////////////////////////////////////////

function TBlocks.AnotherBlockUsesRCS(addr: TRCSsAddr; me: TBlk; typ: TRCSIOType): TBlk;
begin
  for var blk: TBlk in Self.data do
    if (Blk <> me) and (Blk.UsesRCS(addr, typ)) then
      Exit(Blk);
  Result := nil;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlocks.OnClientDisconnect(client: TIdContext);
begin
  for var blk: TBlk in Self.data do
    if (Blk.typ = btAC) then
      TBlkAC(Blk).OnClientDisconnect(client);
end;

/// /////////////////////////////////////////////////////////////////////////////

initialization

Blocks := TBlocks.Create();

finalization

FreeAndNil(Blocks);

end.// unit
