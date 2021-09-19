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

uses Block, Generics.Collections, Area, JsonDataObjects;

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

    barHVManual,
    barHVNotAllManual,

    barTrainWrongDir
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

implementation

uses UPO;

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

end.
