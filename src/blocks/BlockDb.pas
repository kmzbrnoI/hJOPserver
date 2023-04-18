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
  Generics.Collections, Classes, IdContext, TechnologieRCS,
  JsonDataObjects, Train, System.Math,
  BlockTrack, BlockTurnout, BlockIR, BlockLock, BlockRailway, BlockGroupSignal,
  BlockLinker, BlockAC, BlockRailwayTrack, BlockPst, BlockSignal, BlockSummary,
  BlockCrossing, BlockDisconnector;

type
  TBlocks = class(TObject)
  private
    data: TList<TBlk>;

    ffstatus: string;
    ffile: string;
    fenabled: Boolean;

    procedure DestroyBlocks();
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
    function GetBlkByID(id: Integer): TBlk; overload;
    function GetBlkID(index: Integer): Integer;
    function GetBlkName(id: Integer): string;
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

    function GetNavPrivol(Area: TArea): TBlksList;

    // send state of all blocks in area 'areaId' to 'conn'
    procedure GetAreaBlk(areaId: string; conn: TIdContext);

    // Check if block with id 'id' already exists.
    // Ignore block 'ignore_index'.
    function IsBlock(id: Integer; ignore_index: Integer = -1): Boolean;

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
    procedure ChangeTrackWithTrain(Train: TTrain);

    // call 'Change' on all railways with train 'Train'
    procedure ChangeTrainToRailway(Train: TTrain);

    procedure BlkIDChanged(index: Integer);
    procedure ClearPOdj();

    class function SEInPortMaxValue(addr: Integer; currentValue: Integer): Integer;
    class function SEOutPortMaxValue(addr: Integer; currentValue: Integer): Integer;

    procedure GetPtData(json: TJsonObject; includeState: Boolean; Area: TArea = nil; typ: TBlkType = btAny);

    procedure NouzZaverZrusen(Sender: TBlk);
    procedure MoveTurnoutBasicPosition();

    function AnotherBlockUsesRCS(addr: TRCSAddr; me: TBlk; typ: TRCSIOType): TBlk;

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
  ACBlocks;

/// /////////////////////////////////////////////////////////////////////////////

constructor TBlocks.Create();
begin
  inherited;
  Self.data := TList<TBlk>.Create();
  Self.fenabled := false;
end;

destructor TBlocks.Destroy();
begin
  Self.DestroyBlocks();
  Self.data.Free();
  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlocks.DestroyBlocks();
begin
  for var i: Integer := Self.data.count - 1 downto 0 do
  begin
    if (Assigned(Self.data[i])) then
    begin
      var blk: TBlk := Self.data[i];
      Self.data.Delete(i);
      blk.Free();
    end;
  end;
  Self.data.Clear();
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

// load all blocks from file
// Use default index '-1' when loading, index propely later ('UpdateBlkIndexes')
procedure TBlocks.LoadFromFile(const tech_filename, rel_filename, stat_filename: string);
var ini_tech, ini_rel, ini_stat: TMemIniFile;
  Blk: TBlk;
  str: TStrings;
begin
  Log('Načítám bloky: ' + tech_filename + '; ' + rel_filename, llInfo, lsData);
  Self.ffile := tech_filename;
  Self.ffstatus := stat_filename;

  ini_tech := TMemIniFile.Create(tech_filename, TEncoding.UTF8);
  ini_rel := TMemIniFile.Create(rel_filename, TEncoding.UTF8);
  ini_stat := TMemIniFile.Create(stat_filename, TEncoding.UTF8);
  str := TStringList.Create();

  try
    Self.DestroyBlocks();
    ini_tech.ReadSections(str);

    Blk := nil;
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

        var typei: Integer := ini_tech.ReadInteger(section, 'typ', -1);

        case (typei) of
          Integer(btTurnout):
            Blk := TBlkTurnout.Create(-1);
          Integer(btTrack):
            Blk := TBlkTrack.Create(-1);
          Integer(btIR):
            Blk := TBlkIR.Create(-1);
          Integer(btSignal):
            Blk := TBlkSignal.Create(-1);
          Integer(btCrossing):
            Blk := TBlkCrossing.Create(-1);
          Integer(btRailway):
            Blk := TBlkRailway.Create(-1);
          Integer(btLinker):
            Blk := TBlkLinker.Create(-1);
          Integer(btLock):
            Blk := TBlkLock.Create(-1);
          Integer(btDisconnector):
            Blk := TBlkDisconnector.Create(-1);
          Integer(btRT):
            Blk := TBlkRT.Create(-1);
          Integer(btIO):
            Blk := TBlkIO.Create(-1);
          Integer(btSummary):
            Blk := TBlkSummary.Create(-1);
          Integer(btAC):
            Blk := TBlkAC.Create(-1);
          Integer(btGroupSignal):
            Blk := TBlkGroupSignal.Create(-1);
          Integer(btPst):
            Blk := TBlkPst.Create(-1);
        else
          Log('Nenačítám blok ' + section + ' - neznámý typ', llError, lsData);
          continue;
        end;

        Blk.LoadData(ini_tech, section, ini_rel, ini_stat);
        Blk.OnChange := Self.BlkChange;

        Self.data.Insert(Self.FindPlaceForNewBlk(Blk.id), Blk);
        Blk := nil;
      except
        on E: Exception do
        begin
          if (Assigned(Blk)) then
            Blk.Free();
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
var Blk: TBlk;
  index: Integer;
begin
  if (Self.IsBlock(glob.id)) then
    raise Exception.Create('Blok tohoto ID již existuje!');

  index := Self.FindPlaceForNewBlk(glob.id);

  case (glob.typ) of
    btTurnout:
      Blk := TBlkTurnout.Create(index);
    btTrack:
      Blk := TBlkTrack.Create(index);
    btIR:
      Blk := TBlkIR.Create(index);
    btSignal:
      Blk := TBlkSignal.Create(index);
    btCrossing:
      Blk := TBlkCrossing.Create(index);
    btRailway:
      Blk := TBlkRailway.Create(index);
    btLinker:
      Blk := TBlkLinker.Create(index);
    btLock:
      Blk := TBlkLock.Create(index);
    btDisconnector:
      Blk := TBlkDisconnector.Create(index);
    btRT:
      Blk := TBlkRT.Create(index);
    btIO:
      Blk := TBlkIO.Create(index);
    btSummary:
      Blk := TBlkSummary.Create(index);
    btAC:
      Blk := TBlkAC.Create(index);
    btGroupSignal:
      Blk := TBlkGroupSignal.Create(index);
    btPst:
      Blk := TBlkPst.Create(index);
  else
    Exit(nil);
  end;

  Blk.SetGlobalSettings(glob);
  Blk.OnChange := Self.BlkChange;
  Self.data.Insert(index, Blk);
  BlocksTablePainter.BlkAdd(index);
  Result := Blk;

  // move indexes
  for var i: Integer := index + 1 to Self.data.count - 1 do
    Self.data[i].table_index := Self.data[i].table_index + 1;
end;

procedure TBlocks.Delete(index: Integer);
begin
  if (index < 0) then
    raise Exception.Create('Index podtekl seznam bloků');
  if (index >= Self.data.count) then
    raise Exception.Create('Index přetekl seznam bloků');
  var tmp := Self.data[index];
  if ((tmp.typ = btRT) and (TBlkRT(tmp).inRailway > -1)) then
    raise Exception.Create('Tento blok je zaveden jako traťový úsek v trati ID ' + IntToStr((tmp as TBlkRT).inRailway));
  if ((tmp.typ = btSignal) and (TBlkSignal(tmp).groupMaster <> nil)) then
    raise Exception.Create('Toto návěstidlo je zavedeno ve skupinovém návěstidle ' + TBlkSignal(tmp).groupMaster.name);

  Self.data.Delete(index);

  // update indexes (decrement)
  for var i: Integer := index to Self.data.count - 1 do
    Self.data[i].table_index := Self.data[i].table_index - 1;

  // railway deletion -> linker deletion
  if (tmp.typ = btRailway) then
  begin
    Self.Delete(Blocks.GetBlkIndex((tmp as TBlkRailway).GetSettings().linkerA));
    Self.Delete(Blocks.GetBlkIndex((tmp as TBlkRailway).GetSettings().linkerB));
  end;
  if (tmp.typ = btLinker) then
  begin
    var blk := Blocks.GetBlkByID((tmp as TBlkLinker).GetSettings.parent);
    if (blk <> nil) then
      Self.Delete(Blocks.GetBlkIndex((tmp as TBlkLinker).GetSettings.parent));
  end;

  FreeAndNil(tmp);
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
    blk.Enable();
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

/// /////////////////////////////////////////////////////////////////////////////

function TBlocks.GetBlkID(index: Integer): Integer;
begin
  if (index < 0) or (index >= Self.data.count) then
    Exit(-1);
  Result := Self.data[index].id;
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
  for var blk: TBlk in Self.data do
    if (((Blk.typ = btTrack) or (Blk.typ = btRT)) and ((Blk as TBlkTrack).IsTrain(Train))) then
      Result.Add(Blk);
end;

function TBlocks.GetTurnoutsAtTrack(trackId: Integer): TList<TBlk>;
begin
  Result := TList<TBlk>.Create();
  for var blk: TBlk in Self.data do
    if (blk.typ = btTurnout) then
      if ((blk as TBlkTurnout).trackID = trackId) then
        Result.Add(blk);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlocks.TrainPrediction(signal: TBlkSignal);
var track, startTrack: TBlkTrack;
  railway: TBlkRailway;
  Train: TTrain;
  JC: TJC;
begin
  track := nil;

  try
    // get train on track before signal
    track := TBlkTrack(signal.track);
    startTrack := track;
    Train := signal.GeTTrain(track);

    if (signal.IsGoSignal()) then
    begin
      if ((not track.IsTrain()) or (Train.direction <> signal.direction)) then
        Train := track.trainPredict
    end
    else
      Train := nil;
    JC := signal.DNjc;

    // predict while paths exist
    while ((JC <> nil) and (JC.typ = TJCType.Train) and (JC.state.destroyBlock <= 0)) do
    begin
      // signal is go?
      signal := Blocks.GetBlkSignalByID(JC.data.signalId);
      if ((signal = nil) or (not signal.IsGoSignal())) then
        Train := nil;

      // get last track of the path
      track := Blocks.GetBlkTrackOrRTByID(JC.data.tracks[JC.data.tracks.count - 1]);

      if (track = startTrack) then
        Exit();

      if ((track.typ = btRT) and (TBlkRT(track).inRailway > -1)) then
      begin
        // last track in railway -> continue on the other side of railway
        railway := Blocks.GetBlkRailwayByID(TBlkRT(track).inRailway);
        if (Train <> nil) then
        begin
          if ((railway.trainPredict = nil) or (railway.trainPredict.Train <> Train)) then
            railway.trainPredict := TBlkRailwayTrain.Create(Train.index);
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

        // train was not propagated tot end of railway -> exit (maybe some signal in autoblock locked? etc.)
        if ((track.trainPredict <> Train) or (track = startTrack)) then
          Exit();
      end;

      track.trainPredict := Train;

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

function TBlocks.GetNavPrivol(Area: TArea): TBlksList;
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

procedure TBlocks.ChangeTrackWithTrain(Train: TTrain);
var Blks: TBlksList;
begin
  Blks := Self.GetBlkWithTrain(Train);
  for var i: Integer := 0 to Blks.count - 1 do
    (Blks[i] as TBlk).Change();
  Blks.Free();
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlocks.GetCount(): Integer;
begin
  Result := Self.data.count;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlocks.ChangeTrainToRailway(Train: TTrain);
begin
  for var blk: TBlk in Self.data do
    if ((Blk.typ = btRailway) and (Blk as TBlkRailway).IsTrain(Train, true)) then
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
var new_index, min_index, i: Integer;
  tmp: TBlk;
begin
  tmp := Self.data[index];
  Self.data.Delete(index);
  new_index := FindPlaceForNewBlk(tmp.id);

  Self.data.Insert(new_index, tmp);
  if (index = new_index) then
    Exit(); // position of block is not changed

  // update indexes from lowest-changed index
  // update while index mismatch
  min_index := Min(new_index, index);
  for i := min_index to Self.data.count - 1 do
    if (Self.data[i].table_index = i) then
      Break
    else
      Self.data[i].table_index := i;

  BlocksTablePainter.BlkMove(index, new_index);
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

class function TBlocks.SEInPortMaxValue(addr: Integer; currentValue: Integer): Integer;
var tmpMax: Integer;
begin
  tmpMax := Max(Integer(RCSi.GetModuleInputsCountSafe(addr)) - 1, 0);
  if (currentValue > tmpMax) then
    Result := 255 // max value defined in TechnologieRCS.TRCSAddr.port
  else
    Result := tmpMax;
end;

class function TBlocks.SEOutPortMaxValue(addr: Integer; currentValue: Integer): Integer;
var tmpMax: Integer;
begin
  tmpMax := Max(Integer(RCSi.GetModuleOutputsCountSafe(addr)) - 1, 0);
  if (currentValue > tmpMax) then
    Result := 255 // max value defined in TechnologieRCS.TRCSAddr.port
  else
    Result := tmpMax;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlocks.AnotherBlockUsesRCS(addr: TRCSAddr; me: TBlk; typ: TRCSIOType): TBlk;
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
