unit JCBarriers;

{
  Co je to BARIERA JIZDNI CESTY?
  > Bariera jizdni cesty je prekazka branici jejimu postaveni, ktere se lze
  > zbavit napr. jen pouhym potvrzenim (napr. jizdni cesta pres blok se stitkem),
  > potvrzenim pres potvrzovaci sekvenci, nebo se ji nelze zbavit vubec a jizdni
  > cestu jendnoduse nelze postavit.

  Technologie jizdnch cest rozeznava nekolik druhu berier:
  1) KRITICKE BARIERY
    jsou takove bariery, ktere dispecer nemuze odstranit (v ceste napriklad
    chybi tecnologicky blok).
    > Kriticka bariera se pozna tak, ze \CriticalBariera vrati true.
  2) STANDARDNI BARIERY
    jsou takove bariery, ktere se odstrani "samy" - napriklad usek pod
    zaverem, obsazney usek.
    > Standardni bariera typicky neni kriticka, ani neni varovna, tudiz
    > se pozna tak, ze nesplnuje podminky kriticke ani varovne bariery.
  3) VAROVNE BARIERY
    jsou takove bariery, ktere primo nebrani jizdni ceste ve staveni, ale je
    potreba si je uvedmit a potvrdit je (napr. na useku je stitek, ci vyluka).
    Tyto bariery je vzdy nutne potvrdit upozorneni v levem dolnim rohu panelu,
    nektere z nich mohou vyzadovat i potvrzeni potvrzovaci sekvenci.
    > Varovna bariera se pozna tak, ze \WarningBariera vrati true
    > Bariera nutna potvrzeni potvrzovaci sekvenci se pozna tak, ze
    >  \PotvrSekvBariera vrati true.
}

interface

uses Block, Generics.Collections, Area, JsonDataObjects, UPO, ConfSeq;

type
  TJCBarType = (
    barOk,
    barProcessing,
    barBlockDisabled,
    barBlockNotExists,
    barBlockWrongType,
    barPrivol,
    barBlockNote,
    barBlockLockout,

    barSignalNoTrack,
    barSignalActive,

    barTrackOccupied,
    barTrackZaver,
    barTrackTrain,
    barTrackAB,
    barTrackLastOccupied,
    barTrackPSt,

    barTurnoutNoPos,
    barTurnoutLocked,
    barTurnoutEmLock,
    barTurnoutWrongPos,
    barTurnoutPst,

    barCrosEmOpen,
    barCrosError,
    barCrosNotClosed,

    barRefugeeLocked,
    barRefugeeOccupied,
    barRefugeeNoPosition,
    barRefugeePst,

    barRailwayNotReady,
    barRailwayZAKVC,
    barRailwayZAKPC,
    barRailwayZaver,
    barRailwayOccupied,
    barRailwayRequesting,
    barRailwayWrongDir,
    barRailwayNoBp,
    barRailwayNoZAK,
    barRailwayNoTrainMove,
    barRailwayMoveEnd,

    barLockNotLocked,
    barLockEmLock,

    barDiscActive,

    barHVManual,
    barHVNotAllManual,
    barTrainWrongDir,
    barTrainNotFront,
    barControllerNotInBasicPos
  );


  TJCBarrier = record
    typ: TJCBarType;
    block: TBlk;
    param: Integer; // typically id of block when block not found (block = nil)
    class operator Equal(a, b: TJCBarrier): Boolean;
  end;

  TJCBarriers = class(TList<TJCBarrier>)
    function Add(const Value: TJCBarrier): Integer;
  end;

  function BarriersToConfSeq(barriers: TJCBarriers): TConfSeqItems;
  function CriticalBarrier(typ: TJCBarType): Boolean;
  function IsCSBarrier(typ: TJCBarType): Boolean;
  function BarrierGetCSNote(typ: TJCBarType): string;
  function JCBarrier(typ: TJCBarType; block: TBlk = nil; param: Integer = 0): TJCBarrier;
  function JCBarrierToMessage(barrier: TJCBarrier): TUPOItem;
  function JCWarningBarrier(typ: TJCBarType): Boolean;
  procedure BarrierToJson(const barrier: TJCBarrier; result: TJsonObject);
  function IsAnyCSBarrier(barriers: TJCBarriers): Boolean;

implementation

uses Graphics, Classes, SysUtils, BlockTurnout, BlockTrack, BlockPst, BlockLock,
  BlockCrossing, BlockLinker, BlockDisconnector, THVDatabase, TrainDb, colorHelper;

class operator TJCBarrier.Equal(a, b: TJCBarrier): Boolean;
begin
  Result := ((a.typ = b.typ) and (a.block = b.block) and (a.param = b.param));
end;

function TJCBarriers.Add(const Value: TJCBarrier): Integer;
begin
  if (not Self.Contains(Value)) then
    Result := inherited Add(Value)
  else
    Result := 1;
end;

function BarriersToConfSeq(barriers: TJCBarriers): TConfSeqItems;
begin
  Result := TList<TConfSeqItem>.Create();

  try
    for var barrier: TJCBarrier in barriers do
    begin
      case (barrier.typ) of
        barBlockDisabled:
          Result.Add(CSItem(barrier.block, 'Blok neaktivní'));

        barTrackOccupied:
          Result.Add(CSItem(barrier.block, 'Úsek obsazen'));
        barTrackTrain:
          Result.Add(CSItem(barrier.block, 'Úsek obsahuje soupravu'));

        barCrosEmOpen:
          Result.Add(CSItem(barrier.block, 'Nouzově otevřen'));
        barCrosError:
          Result.Add(CSItem(barrier.block, 'Porucha'));
        barCrosNotClosed:
          Result.Add(CSItem(barrier.block, 'Neuzavřen'));

        barTurnoutNoPos:
          Result.Add(CSItem(barrier.block, 'Není správná poloha'));
        barTurnoutEmLock:
          Result.Add(CSItem(barrier.block, 'Není zaveden nouzový závěr'));
        barTurnoutWrongPos:
          Result.Add(CSItem(barrier.block, 'Není správná poloha'));

        barRailwayZAKVC, barRailwayZAKPC:
          Result.Add(CSItem(barrier.block, 'Zákaz odjezdu'));
        barRailwayNoZAK:
          Result.Add(CSItem(barrier.block, 'Nezaveden zákaz odjezdu'));
        barRailwayZaver:
          Result.Add(CSItem(barrier.block, 'Závěr'));
        barRailwayNotReady:
          Result.Add(CSItem(barrier.block, 'Nepovoluje odjezd'));
        barRailwayRequesting:
          Result.Add(CSItem(barrier.block, 'Probíhá žádost'));
        barRailwayWrongDir:
          Result.Add(CSItem(barrier.block, 'Nesouhlas'));
        barRailwayNoBp:
          Result.Add(CSItem(barrier.block, 'Bloková podmínka nezavedena'));
        barRailwayNoTrainMove:
          Result.Add(CSItem(barrier.block, 'Nedojde k přenosu čísla vlaku'));
        barRailwayMoveEnd:
          Result.Add(CSItem(barrier.block, 'Vlak bude přenesen až na konec trati'));

        barLockNotLocked:
          Result.Add(CSItem(barrier.block, 'Neuzamčen'));
        barLockEmLock:
          Result.Add(CSItem(barrier.block, 'Není zaveden nouzový závěr'));

        barDiscActive:
          Result.Add(CSItem(barrier.block, 'Rozpojovač aktivní'));

        barControllerNotInBasicPos:
          Result.Add(CSItem(barrier.block, 'Volič není v základní poloze'));
      end;
    end;
  except
    Result.Free();
    raise;
  end;
end;

function CriticalBarrier(typ: TJCBarType): Boolean;
begin
  case (typ) of
    barProcessing, barBlockDisabled, barBlockNotExists, barBlockWrongType:
      result := true;
  else
    result := false;
  end;
end;

function IsCSBarrier(typ: TJCBarType): Boolean;
begin
  case (typ) of
    barBlockLockout, barRailwayZAKPC:
      result := true;
  else
    result := false;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function BarrierGetCSNote(typ: TJCBarType): string;
begin
  case (typ) of
    barBlockLockout:
      result := 'Výluka bloku';
    barRailwayZAKPC:
      result := 'Zákaz odjezdu na trať';
  else
    result := '';
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function JCBarrier(typ: TJCBarType; block: TBlk = nil; param: Integer = 0): TJCBarrier;
begin
  Result.typ := typ;
  Result.Block := Block;
  Result.param := param;
end;

/// /////////////////////////////////////////////////////////////////////////////

function JCBarrierToMessage(barrier: TJCBarrier): TUPOItem;
begin
  for var i := 0 to _UPO_LINES - 1 do
  begin
    Result[i].str := '';
    Result[i].fg := clNone;
    Result[i].bg := clNone;
  end;

  case (barrier.typ) of
    barBlockDisabled, barBlockWrongType, barSignalNoTrack, barSignalActive, barBlockNotExists, barTrackOccupied,
      barTrackZaver, barTrackAB, barTrackTrain, barTurnoutNoPos, barTurnoutLocked,
      barTurnoutEmLock, barCrosEmOpen, barCrosError, barRefugeeLocked,
      barRefugeeOccupied, barRefugeeNoPosition, barRailwayZaver, barRailwayNotReady, barRailwayRequesting,
      barRailwayWrongDir, barRailwayZAKVC, barLockNotLocked, barDiscActive, barTurnoutWrongPos,
      barTrackPSt, barTurnoutPst, barRefugeePst, barControllerNotInBasicPos:
      begin
        Result[0] := GetUPOLine('NEPŘÍPUSTNÉ', taCenter, TJopColor.red, TJopColor.white);
        if (Assigned(barrier.Block)) then
          Result[2] := GetUPOLine(barrier.Block.name)
        else
          Result[2] := GetUPOLine('ID ' + IntToStr(barrier.param));
      end;
  end;

  case (barrier.typ) of
    barOk:
      Result[0] := GetUPOLine('OK', taCenter, TJopColor.blue, TJopColor.grayDark);
    barProcessing:
      Result[0] := GetUPOLine('Již se staví', taCenter, TJopColor.blue, TJopColor.grayDark);

    barBlockDisabled:
      Result[1] := GetUPOLine('Blok neaktivní');
    barBlockNotExists:
      Result[1] := GetUPOLine('Blok neexistuje');
    barBlockWrongType:
      Result[1] := GetUPOLine('Blok není správného typu');

    barSignalNoTrack:
      Result[1] := GetUPOLine('Není úsek před návěstidlem');
    barSignalActive:
      Result[1] := GetUPOLine('Není základní návěst');

    barTrackOccupied:
      Result[1] := GetUPOLine('Úsek obsazen');
    barTrackZaver:
      Result[1] := GetUPOLine('Úsek zapevněn');
    barTrackTrain:
      Result[1] := GetUPOLine('Souprava');
    barTrackAB:
      Result[1] := GetUPOLine('Blokováno automatickou JC');

    barTurnoutNoPos:
      Result[1] := GetUPOLine('Není koncová poloha');
    barTurnoutLocked:
      Result[1] := GetUPOLine('Zamčena');
    barTurnoutEmLock:
      Result[1] := GetUPOLine('Nouzový závěr');
    barTurnoutWrongPos:
      Result[1] := GetUPOLine('Nesprávná poloha');

    barCrosEmOpen:
      Result[1] := GetUPOLine('Nouzově otevřen');
    barCrosError:
      Result[1] := GetUPOLine('Poruchový stav');

    barRefugeeLocked:
      Result[1] := GetUPOLine('Zamčena');
    barRefugeeOccupied:
      Result[1] := GetUPOLine('Obsazena');
    barRefugeeNoPosition:
      Result[1] := GetUPOLine('Není koncová poloha');

    barRailwayZaver:
      Result[1] := GetUPOLine('Závěr');
    barRailwayRequesting:
      Result[1] := GetUPOLine('Probíhá žádost');
    barRailwayWrongDir:
      Result[1] := GetUPOLine('Nesouhlas');
    barRailwayNotReady:
      Result[1] := GetUPOLine('Nepovoluje odjezd');
    barRailwayOccupied:
      begin
        Result[0] := GetUPOLine('NEBUDE POVOLUJÍCÍ NÁVĚST', taCenter, TJopColor.black, TJopColor.yellow);
        Result[1] := GetUPOLine('Trať obsazena');
        Result[2] := GetUPOLine(barrier.Block.name);
      end;
    barRailwayZAKVC:
      Result[1] := GetUPOLine('Zaveden zákaz odjezdu');

    barLockNotLocked:
      Result[1] := GetUPOLine('Neuzamčen');
    barLockEmLock:
      Result[1] := GetUPOLine('Není nouzový závěr');

    barDiscActive:
      Result[1] := GetUPOLine('Rozpojovač aktivní');

    barBlockLockout:
      begin
        Result[0] := GetUPOLine('VÝLUKA ' + barrier.Block.name, taCenter, TJopColor.black, TJopColor.brown);

        var lockout: string := '-';
        case (barrier.block.typ) of
          btTurnout: lockout := TBlkTurnout(barrier.block).lockout;
          btTrack, btRT: lockout := TBlkTrack(barrier.block).lockout;
          btCrossing: lockout := TBlkCrossing(barrier.block).lockout;
        end;

        var lines := GetLines(lockout, _UPO_LINE_LEN);
        try
          Result[1] := GetUPOLine(lines[0], taLeftJustify, TJopColor.yellow, TJopColor.grayDark);
          if (lines.Count > 1) then
            Result[2] := GetUPOLine(lines[1], taLeftJustify, TJopColor.yellow, TJopColor.grayDark);
        finally
          lines.Free();
        end;
      end;

    barBlockNote:
      begin
        var note: string := '-';
        case (barrier.block.typ) of
          btTurnout: note := TBlkTurnout(barrier.block).note;
          btTrack, btRT: note := TBlkTrack(barrier.block).note;
          btCrossing: note := TBlkCrossing(barrier.block).note;
          btLinker: note := TBlkLinker(barrier.block).note;
          btLock: note := TBlkLock(barrier.block).note;
          btDisconnector: note := TBlkDisconnector(barrier.block).note;
          btPst: note := TBlkPst(barrier.block).note;
        end;
        Result := NoteUPO(barrier.Block.name, note);
      end;

    barTrackLastOccupied:
      begin
        Result[0] := GetUPOLine('NEBUDE POVOLUJÍCÍ NÁVĚST', taCenter, TJopColor.black, TJopColor.yellow);
        Result[1] := GetUPOLine('Kolejový úsek obsazen');
        Result[2] := GetUPOLine(barrier.Block.name);
      end;

    barPrivol:
      begin
        Result := UPO.PNUPO(barrier.block.name);
      end;

    barHVManual:
      begin
        Result[0] := GetUPOLine('Hnací vozidlo v ručním řízení', taCenter, TJopColor.black, TJopColor.yellow);
        Result[1] := GetUPOLine(IntToStr(barrier.param) + ' : ' + HVDb[barrier.param].name);
        Result[2] := GetUPOLine('Řídí: '+HVDb[barrier.param].DriverFullNames());
      end;

    barHVNotAllManual:
      begin
        Result[0] := GetUPOLine('POZOR !', taCenter, TJopColor.black, TJopColor.yellow);
        Result[1] := GetUPOLine('Ne všechna HV v ručním řízení');
        Result[2] := GetUPOLine('');
      end;

    barRailwayZAKPC:
      begin
        Result[0] := GetUPOLine('ZAVEDEN ZÁKAZ ODJEZDU', taCenter, TJopColor.red, TJopColor.white);
        Result[1] := GetUPOLine(barrier.Block.name);
        Result[2] := GetUPOLine('');
      end;

    barTrainWrongDir:
      begin
        Result[0] := GetUPOLine('POZOR !', taCenter, TJopColor.black, TJopColor.yellow);
        Result[1] := GetUPOLine('Jízda proti směru soupravy');
        Result[2] := GetUPOLine('Souprava ' + trains[barrier.param].name);
      end;

    barTrainNotFront:
      begin
        Result[0] := GetUPOLine('POZOR !', taCenter, TJopColor.black, TJopColor.yellow);
        Result[1] := GetUPOLine('Čelo vlaku je na jiném úseku');
        Result[2] := GetUPOLine('Souprava ' + trains[barrier.param].name);
      end;

    barTrackPSt, barTurnoutPst, barRefugeePst:
      Result[1] := GetUPOLine('Prvek pod pom. stavědlem');

    barControllerNotInBasicPos:
      Result[1] := GetUPOLine('Volič není v základní poloze');

  else
    Result[0] := GetUPOLine('Neznámá bariéra', taCenter, TJopColor.red, TJopColor.white);
  end;
end;

function JCWarningBarrier(typ: TJCBarType): Boolean;
begin
  case (typ) of
    barBlockNote, barBlockLockout, barPrivol, barHVManual, barHVNotAllManual,
    barTrainWrongDir, barTrainNotFront, barTrackLastOccupied, barRailwayOccupied, barRailwayZAKPC:
      Result := true;
  else
    Result := false;
  end;
end;

procedure BarrierToJson(const barrier: TJCBarrier; Result: TJsonObject);
begin
  Result['typ'] := barrier.typ;
  if (barrier.Block <> nil) then
    Result['blok'] := barrier.Block.id;
  Result['param'] := barrier.param;
  if (CriticalBarrier(barrier.typ)) then
    Result['type'] := 'critical'
  else if (JCWarningBarrier(barrier.typ)) then
    Result['type'] := 'warning'
  else
    Result['type'] := 'standard';

  var upoItem := JCBarrierToMessage(barrier);
  Result['description'] := upoItem[0].str + ' ' + upoItem[1].str + ' ' + upoItem[2].str;
end;

function IsAnyCSBarrier(barriers: TJCBarriers): Boolean;
begin
  for var barrier: TJCBarrier in barriers do
    if (IsCSBarrier(barrier.typ)) then
      Exit(True);
  Result := False;
end;

end.
