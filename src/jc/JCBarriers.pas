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

uses Block, Generics.Collections, Area, JsonDataObjects, UPO;

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
  end;

  TJCBarriers = TList<TJCBarrier>;

  function BarriersToConfSeq(barriers: TJCBarriers): TConfSeqItems;
  function CriticalBarrier(typ: TJCBarType): Boolean;
  function IsCSBarrier(typ: TJCBarType): Boolean;
  function BarrierGetCSNote(typ: TJCBarType): string;
  function JCBarrier(typ: TJCBarType; block: TBlk = nil; param: Integer = 0): TJCBarrier;
  function JCBarrierToMessage(barrier: TJCBarrier): TUPOItem;
  function JCWarningBarrier(typ: TJCBarType): Boolean;
  procedure BarrierToJson(const barrier: TJCBarrier; result: TJsonObject);

implementation

uses Graphics, Classes, SysUtils, BlockTurnout, BlockTrack, BlockPst, BlockLock,
  BlockCrossing, BlockLinker, BlockDisconnector, THVDatabase, TrainDb;

function BarriersToConfSeq(barriers: TJCBarriers): TConfSeqItems;
begin
  result := TList<TConfSeqItem>.Create();

  for var i: Integer := 0 to barriers.Count - 1 do
  begin
    case (barriers[i].typ) of
      barBlockDisabled:
        result.Add(TArea.GetCSCondition(barriers[i].Block, 'Blok neaktivní'));

      barTrackOccupied:
        result.Add(TArea.GetCSCondition(barriers[i].Block, 'Úsek obsazen'));
      barTrackTrain:
        result.Add(TArea.GetCSCondition(barriers[i].Block, 'Úsek obsahuje soupravu'));

      barCrosEmOpen:
        result.Add(TArea.GetCSCondition(barriers[i].Block, 'Nouzově otevřen'));
      barCrosError:
        result.Add(TArea.GetCSCondition(barriers[i].Block, 'Porucha'));
      barCrosNotClosed:
        result.Add(TArea.GetCSCondition(barriers[i].Block, 'Neuzavřen'));

      barTurnoutNoPos:
        result.Add(TArea.GetCSCondition(barriers[i].Block, 'Není správná poloha'));
      barTurnoutEmLock:
        result.Add(TArea.GetCSCondition(barriers[i].Block, 'Není zaveden nouzový závěr'));
      barTurnoutWrongPos:
        result.Add(TArea.GetCSCondition(barriers[i].Block, 'Není správná poloha'));

      barRailwayZAKVC, barRailwayZAKPC:
        result.Add(TArea.GetCSCondition(barriers[i].Block, 'Zákaz odjezdu'));
      barRailwayNoZAK:
        result.Add(TArea.GetCSCondition(barriers[i].Block, 'Nezaveden zákaz odjezdu'));
      barRailwayZaver:
        result.Add(TArea.GetCSCondition(barriers[i].Block, 'Závěr'));
      barRailwayNotReady:
        result.Add(TArea.GetCSCondition(barriers[i].Block, 'Nepovoluje odjezd'));
      barRailwayRequesting:
        result.Add(TArea.GetCSCondition(barriers[i].Block, 'Probíhá žádost'));
      barRailwayWrongDir:
        result.Add(TArea.GetCSCondition(barriers[i].Block, 'Nesouhlas'));
      barRailwayNoBp:
        result.Add(TArea.GetCSCondition(barriers[i].Block, 'Bloková podmínka nezavedena'));
      barRailwayNoTrainMove:
        result.Add(TArea.GetCSCondition(barriers[i].Block, 'Nedojde k přenosu čísla vlaku'));
      barRailwayMoveEnd:
        result.Add(TArea.GetCSCondition(barriers[i].Block, 'Vlak bude přenesen až na konec trati'));

      barLockNotLocked:
        result.Add(TArea.GetCSCondition(barriers[i].Block, 'Neuzamčen'));
      barLockEmLock:
        result.Add(TArea.GetCSCondition(barriers[i].Block, 'Není zaveden nouzový závěr'));

      barDiscActive:
        result.Add(TArea.GetCSCondition(barriers[i].Block, 'Rozpojovač aktivní'));

      barControllerNotInBasicPos:
        result.Add(TArea.GetCSCondition(barriers[i].Block, 'Volič není v základní poloze'));
    end;
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
    result[i].str := '';
    result[i].fg := clNone;
    result[i].bg := clNone;
  end;

  case (barrier.typ) of
    barBlockDisabled, barBlockWrongType, barSignalNoTrack, barSignalActive, barBlockNotExists, barTrackOccupied,
      barTrackZaver, barTrackAB, barTrackTrain, barTurnoutNoPos, barTurnoutLocked,
      barTurnoutEmLock, barCrosEmOpen, barCrosError, barRefugeeLocked,
      barRefugeeOccupied, barRefugeeNoPosition, barRailwayZaver, barRailwayNotReady, barRailwayRequesting,
      barRailwayWrongDir, barRailwayZAKVC, barLockNotLocked, barDiscActive, barTurnoutWrongPos,
      barTrackPSt, barTurnoutPst, barRefugeePst, barControllerNotInBasicPos:
      begin
        result[0] := GetUPOLine('NEPŘÍPUSTNÉ', taCenter, clRed, clWhite);
        if (Assigned(barrier.Block)) then
          result[2] := GetUPOLine(barrier.Block.name)
        else
          result[2] := GetUPOLine('ID ' + IntToStr(barrier.param));
      end;
  end;

  case (barrier.typ) of
    barOk:
      result[0] := GetUPOLine('OK', taCenter, clBlue, $A0A0A0);
    barProcessing:
      result[0] := GetUPOLine('Již se staví', taCenter, clBlue, $A0A0A0);

    barBlockDisabled:
      result[1] := GetUPOLine('Blok neaktivní');
    barBlockNotExists:
      result[1] := GetUPOLine('Blok neexistuje');
    barBlockWrongType:
      result[1] := GetUPOLine('Blok není správného typu');

    barSignalNoTrack:
      result[1] := GetUPOLine('Není úsek před návěstidlem');
    barSignalActive:
      result[1] := GetUPOLine('Není základní návěst');

    barTrackOccupied:
      result[1] := GetUPOLine('Úsek obsazen');
    barTrackZaver:
      result[1] := GetUPOLine('Úsek zapevněn');
    barTrackTrain:
      result[1] := GetUPOLine('Souprava');
    barTrackAB:
      result[1] := GetUPOLine('Blokováno automatickou JC');

    barTurnoutNoPos:
      result[1] := GetUPOLine('Není koncová poloha');
    barTurnoutLocked:
      result[1] := GetUPOLine('Zamčena');
    barTurnoutEmLock:
      result[1] := GetUPOLine('Nouzový závěr');
    barTurnoutWrongPos:
      result[1] := GetUPOLine('Nesprávná poloha');

    barCrosEmOpen:
      result[1] := GetUPOLine('Nouzově otevřen');
    barCrosError:
      result[1] := GetUPOLine('Poruchový stav');

    barRefugeeLocked:
      result[1] := GetUPOLine('Zamčena');
    barRefugeeOccupied:
      result[1] := GetUPOLine('Obsazena');
    barRefugeeNoPosition:
      result[1] := GetUPOLine('Není koncová poloha');

    barRailwayZaver:
      result[1] := GetUPOLine('Závěr');
    barRailwayRequesting:
      result[1] := GetUPOLine('Probíhá žádost');
    barRailwayWrongDir:
      result[1] := GetUPOLine('Nesouhlas');
    barRailwayNotReady:
      result[1] := GetUPOLine('Nepovoluje odjezd');
    barRailwayOccupied:
      begin
        result[0] := GetUPOLine('NEBUDE POVOLUJÍCÍ NÁVĚST', taCenter, clBlack, clYellow);
        result[1] := GetUPOLine('Trať obsazena');
        result[2] := GetUPOLine(barrier.Block.name);
      end;
    barRailwayZAKVC:
      result[1] := GetUPOLine('Zaveden zákaz odjezdu');

    barLockNotLocked:
      result[1] := GetUPOLine('Neuzamčen');
    barLockEmLock:
      result[1] := GetUPOLine('Není nouzový závěr');

    barDiscActive:
      result[1] := GetUPOLine('Rozpojovač aktivní');

    barBlockLockout:
      begin
        result[0] := GetUPOLine('VÝLUKA ' + barrier.Block.name, taCenter, clBlack, clOlive);

        var lockout: string := '-';
        case (barrier.block.typ) of
          btTurnout: lockout := TBlkTurnout(barrier.block).lockout;
          btTrack, btRT: lockout := TBlkTrack(barrier.block).lockout;
          btCrossing: lockout := TBlkCrossing(barrier.block).lockout;
        end;

        var lines := GetLines(lockout, _UPO_LINE_LEN);
        try
          result[1] := GetUPOLine(lines[0], taLeftJustify, clYellow, $A0A0A0);
          if (lines.Count > 1) then
            result[2] := GetUPOLine(lines[1], taLeftJustify, clYellow, $A0A0A0);
        finally
          lines.Free();
        end;
      end;

    barBlockNote:
      begin
        result[0] := GetUPOLine('ŠTÍTEK ' + barrier.Block.name, taCenter, clBlack, clTeal);

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

        var lines := GetLines(note, _UPO_LINE_LEN);
        try
          result[1] := GetUPOLine(lines[0], taLeftJustify, clYellow, $A0A0A0);
          if (lines.Count > 1) then
            result[2] := GetUPOLine(lines[1], taLeftJustify, clYellow, $A0A0A0);
        finally
          lines.Free();
        end;
      end;

    barTrackLastOccupied:
      begin
        result[0] := GetUPOLine('NEBUDE POVOLUJÍCÍ NÁVĚST', taCenter, clBlack, clYellow);
        result[1] := GetUPOLine('Kolejový úsek obsazen');
        result[2] := GetUPOLine(barrier.Block.name);
      end;

    barPrivol:
      begin
        result[0] := GetUPOLine('POZOR !', taCenter, clBlack, clYellow);
        result[1] := GetUPOLine('Svítí přivolávací návěst');
        result[2] := GetUPOLine(barrier.Block.name);
      end;

    barHVManual:
      begin
        result[0] := GetUPOLine('POZOR !', taCenter, clBlack, clYellow);
        result[1] := GetUPOLine('Hnací vozidlo v ručním řízení');
        result[2] := GetUPOLine(IntToStr(barrier.param) + ' : ' + HVDb[barrier.param].name);
      end;

    barHVNotAllManual:
      begin
        result[0] := GetUPOLine('POZOR !', taCenter, clBlack, clYellow);
        result[1] := GetUPOLine('Ne všechna HV v ručním řízení');
        result[2] := GetUPOLine('');
      end;

    barRailwayZAKPC:
      begin
        result[0] := GetUPOLine('ZAVEDEN ZÁKAZ ODJEZDU', taCenter, clRed, clWhite);
        result[1] := GetUPOLine(barrier.Block.name);
        result[2] := GetUPOLine('');
      end;

    barTrainWrongDir:
      begin
        result[0] := GetUPOLine('POZOR !', taCenter, clBlack, clYellow);
        result[1] := GetUPOLine('Jízda proti směru soupravy');
        result[2] := GetUPOLine('Souprava ' + trains[barrier.param].name);
      end;

    barTrainNotFront:
      begin
        result[0] := GetUPOLine('POZOR !', taCenter, clBlack, clYellow);
        result[1] := GetUPOLine('Čelo vlaku je na jiném úseku');
        result[2] := GetUPOLine('Souprava ' + trains[barrier.param].name);
      end;

    barTrackPSt, barTurnoutPst, barRefugeePst:
      result[1] := GetUPOLine('Prvek pod pom. stavědlem');

    barControllerNotInBasicPos:
      result[1] := GetUPOLine('Volič není v základní poloze');

  else
    result[0] := GetUPOLine('Neznámá bariéra', taCenter, clRed, clWhite);
  end;
end;

function JCWarningBarrier(typ: TJCBarType): Boolean;
begin
  case (typ) of
    barBlockNote, barBlockLockout, barPrivol, barHVManual, barHVNotAllManual,
    barTrainWrongDir, barTrainNotFront, barTrackLastOccupied, barRailwayOccupied, barRailwayZAKPC:
      result := true;
  else
    result := false;
  end;
end;

procedure BarrierToJson(const barrier: TJCBarrier; result: TJsonObject);
var upoItem: TUPOItem;
begin
  result['typ'] := barrier.typ;
  if (barrier.Block <> nil) then
    result['blok'] := barrier.Block.id;
  result['param'] := barrier.param;
  if (CriticalBarrier(barrier.typ)) then
    result['type'] := 'critical'
  else if (JCWarningBarrier(barrier.typ)) then
    result['type'] := 'warning'
  else
    result['type'] := 'standard';

  upoItem := JCBarrierToMessage(barrier);
  result['description'] := upoItem[0].str + ' ' + upoItem[1].str + ' ' + upoItem[2].str;
end;

end.
